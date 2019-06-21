/*********************************************************************
**    NAME         :  reutr.c
**       CONTAINS:
**       ur_update_transf()
**       ur_update_transf1()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reutr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:41
*********************************************************************/

#include  "usysdef.h"
#include "umessages.h"
#include	"udebug.h"
#include	"rbase.h"
#include	"ribase.h"
#include	"rmtuple.h"
#include "riddle.h"
#include "mattrddl.h"
#include "mdcoord.h"
#include "r1emsgpk.h"

/*********************************************************************
** E_FUNCTION : status = ur_update_transf(&tran_pack)
**      Update transformation matrix (new or old) for existing key.
**      We have 4 cases:
**       (1) NO current transf key, tran_pack NOT EQUAL last_accessed:
**              create new transf tuple, use_count = 1.
**       (2) NO current transf, tran_pack EQUAL last_accessed:
**              use_count++, transf key is last_accessed tuple.
**       (3) HAVE current transf key, use_count=1:
**              update transf tuple with trans_packet.
**       (4) HAVE current transf key, use_count>1 (multi-use):
**              use_count-- for transf tuple, then create new tuple for
**              tran_pack, use=1.
**    PARAMETERS   
**       INPUT  : 
**				&tran_pack, address of transformation packet to 
**							update with
**				tran_pack.key, master key for transformation 
**							update
**				tran_pack.rel_num, must equal relation number of 
**							transformation relaion
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_transf(tran_pack)
struct UM_transf_rec  *tran_pack;	/* ptr to transformation data packet */
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_transf(key=0x%x)", tran_pack->key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_TRANS, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_transf1(tran_pack, &theMessage, 1));
}


/*********************************************************************
** E_FUNCTION : status = ur_update_transf1(&tran_pack, theMessage, messageCnt)
**      Update transformation matrix (new or old) for existing key.
**      We have 4 cases:
**       (1) NO current transf key, tran_pack NOT EQUAL last_accessed:
**              create new transf tuple, use_count = 1.
**       (2) NO current transf, tran_pack EQUAL last_accessed:
**              use_count++, transf key is last_accessed tuple.
**       (3) HAVE current transf key, use_count=1:
**              update transf tuple with trans_packet.
**       (4) HAVE current transf key, use_count>1 (multi-use):
**              use_count-- for transf tuple, then create new tuple for
**              tran_pack, use=1.
**    PARAMETERS   
**       INPUT  : 
**				&tran_pack, address of transformation packet to 
**							update with
**				tran_pack.key, master key for transformation 
**							update
**				tran_pack.rel_num, must equal relation number of 
**							transformation relaion
**				theMessage,	for scheduler
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_transf1(tran_pack, theMessage, messageCnt)
struct UM_transf_rec  *tran_pack;	/* ptr to transformation data packet */
UR_messagePacket	theMessage[];
int					messageCnt;
{
	struct UR_rcb_rec *rcb;				/* current trans. relation's rcb block */
	struct UM_transf_rec  transf;			/* generic transformation tuple */
	int					status;			/* return status */
	int					retcod;
	UU_REL_ID			rel_key;			/* general relation key */
	UR_REL_NUM			rel;			/* relation number */
	UR_TUPLE_INDX		tuple;		/* tuple index within a relation */
	UR_TUPLE_INDX		last_accessed;	/* tuple index last used */
	int					i;					/* loop index */
	UU_LOGICAL			create;			/* flag creation of a new transformation */
	int					comp_size;		/* number of bytes for transf comp */
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC, (us, "ur_update_transf1(key= 0x%x, rel= %d messageCnt=%d)",
					tran_pack->key, tran_pack->rel_num, messageCnt));

	/* get existing transformation's key */
	ur_k2rt(tran_pack->key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (ur_editablep(m_ptr))
	{
		rel_key = m_ptr->assocs[UR_TRANSF_INDX];
	}
	else
	{
		status = -1;	/* entity is not editable */
		goto theExit;
	}
	rel = tran_pack->rel_num;	/* get relation number */
	rcb = &UR_rcb[rel];					/* get trans. rcb addrs */
	last_accessed = rcb->last_accessed_index; /* save last used tuple index */
/*
.....vp 17-apr-97
.....simple for now! Use size of transformation matrix UM_trans, but if
.....anything is added to this structure to be valid for comparing we need
.....to add this change here or use dictionary for attributes length
	comp_size = rcb->tuple_size - (sizeof(int) * 3); 
*/
	comp_size = sizeof(UM_transf);     /* compare size of trans */

	/* only allow transformation to be associated if the relation is indeed */
	/* a tranformation relation */
	if(uu_tst_bit(&(UR_rcb[rel].rel_flags), UR_TRANSF_REL))
	{
		/* must create a new transformation tuple if no last used tuple */
		/* otherwise the default is reuse the last transformation */
		if (last_accessed==0)
		{
			create = UU_TRUE; 
		}
		else
		{
			create = UU_FALSE;
		}
		if (rel_key==0)
		{
			/* no transformation tuple this master*/
			uu_dprint(UU_RITRC,
				(us, "upd_transf no transformation key, last accessed tuple=%d",
				last_accessed));

			/* see if we can re-use the last trans. tuple */
			if(!create)                
			{
				/* get last used trans. tuple */
				status = ur_retrieve_tuple(rel, last_accessed, &transf);
				if(status!=0)
				{
					goto theExit;   /* cannot continue */
					/* could just create this tuple, but something hosed */
				}
				retcod = uu_comp_byte(transf.tfmat, tran_pack->tfmat, comp_size);
				if (retcod==0)
				{
					/* new transf same as last used */
					/* update the transf tuple use count */
					transf.use_count++;      /* one more mtuple using this one*/
					ur_update_tuple(rel, last_accessed, &transf);	

					/* update the transf key in the mtuple for this update */
					ur_rt2k(rel, last_accessed, &rel_key);
					ur_update_mtuple_attr(tran_pack->key, UR_TRANSF_INDX, rel_key);
					uu_dprint(UU_RITRC, (us, "transf new same as tuple 0x%x, new use=%d",
							rel_key, transf.use_count));
				}
				else                        /* new transf different from last */
				{
					uu_dprint(UU_RITRC, (us, "upd_transf new diff from last acc"));
					create = UU_TRUE;   /* we must create a transformation tuple*/
				}
			}
		} /* if no current trans for this mtuple */
		else /* mtuple has associated trans tuple already */
		{
			/* get the existing trans tuple */
			ur_k2rt(rel_key, &rel, &tuple); /*get indexes to retrieve*/
			status = ur_retrieve_tuple(rel, tuple, &transf);
			if(status!=0)
			{
				goto theExit;          /* cannot continue */
				/* could just create this tuple, but something hosed */
			}
			/* make sure we are not doing an update with what is there, */
			/* if so, do nothing */
			if(!uu_comp_byte(transf.tfmat, tran_pack->tfmat, comp_size)) goto theExit ;
			uu_dprint(UU_RITRC, (us, "current transf key =0x%x, use_count=%d",
							rel_key, transf.use_count));
			if (transf.use_count==1)
			{
				/* only 1 use (this mtuple). Just update tuple with new info */
				tran_pack->use_count = 1; /* set users area use count */
				tran_pack->rel_num = rel; /* and relation number */
				if(tuple != 1) /* don't allow update of the default */
				{
					ur_update_tuple( rel,   /* update tuple with new data */
						tuple, tran_pack);
					create = UU_FALSE;          /* do not create new tuple */
				}
				else
				{
					create = UU_TRUE ;	/* don't allow update of the default	*/
				}
			}
			else /* 2 or more uses of current trans dec. use count and create */
			{
				transf.use_count--;           /* decrease use count */
				ur_update_tuple(rel, tuple, &transf); /* and update tuple */
				if(!create)
				{
					/* get last used transformation tuple */
					status = ur_retrieve_tuple(rel, last_accessed, &transf);
					if(status!=0)
					{
						goto theExit;          /* cannot continue */
						/* could just create this tuple, but something hosed */
					}
					/* if not the same as the last accessed, create a new one, */
					/* otherwise re-use the last accessed */
					if(uu_comp_byte(transf.tfmat, tran_pack->tfmat, comp_size))
					{
						/* not same as last */
						create = UU_TRUE;           /* must create a new one */
					}
					else /* same as last accessed */
					{
						/* update the use count of the existing trans */
						transf.use_count++;
						ur_update_tuple(rel, last_accessed, &transf);	

						/* set the trans assoc key in the mtuple being updated */
						ur_rt2k(rel, last_accessed, &rel_key); /* get trans key */
						ur_update_mtuple_attr(tran_pack->key, UR_TRANSF_INDX, rel_key);
						uu_dprint(UU_RITRC,
							(us, "upd_transf new same as tuple 0x%x, new use=%d",
							rel_key, transf.use_count));
					}
				} /* !create	*/
			}
		} /* end master transf exists */
		if(create)
 		{
			/* Create new transformation tuple - set use count to 1 */
			rel = tran_pack->rel_num; /* get relation number */
			tran_pack->use_count = 1;
			status = ur_create_tuple(rel, &tuple, tran_pack);
			if (status!=0)
			{
				goto theExit;     /* cannot continue */
			}
			ur_rt2k(rel, tuple, &rel_key); /* get trans key */
			ur_update_mtuple_attr(tran_pack->key, UR_TRANSF_INDX, rel_key);
			uu_dprint(UU_RITRC, (us, "create transf tuple key =0x%x", rel_key));
		}
		/* get master rel & tuple, and a pointer to the master fixed data */
		ur_k2rt(tran_pack->key, &rel, &tuple);
		if(rel == UR_MTUPLE_REL)
		{
			status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

			/* now notify associates */
			if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
			{
				uri_notify(m_ptr, tran_pack->key, theMessage, messageCnt);
			}
		}
	}
	else
	{
		uu_dprint(UU_RTRC, (us, "illegal transformation rel num %d", rel));
		status = -1;
	}
	/*----------- function exit ----------------------------------------*/
theExit:                                /******* EXIT LABEL ******/
	uu_dprint(UU_RITRC, (us, "ur_update_transf exit status=%d", status));
	uu_dexit;
	return(status);
}
