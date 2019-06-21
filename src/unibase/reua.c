/*********************************************************************
**    NAME         :  reua.c
**       CONTAINS:
**       ur_update_attr()
**       ur_update_attr1()
**       ur_comp_attr()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reua.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:37
*********************************************************************/

#include  "usysdef.h"
#include "umessages.h"
#include	"udebug.h"
#include	"rbase.h"
#include	"ribase.h"
#include	"rmtuple.h"
#include "riddle.h"
#include "r1emsgpk.h"

/*********************************************************************
** E_FUNCTION : status = ur_update_attr(&attr_packet)
**      Update attribute (new or old) data for existing key.
**      We have 4 cases:
**       (1) NO current attr key, attr_packet NOT EQUAL last_accessed:
**              create new attr tuple, use_count = 1.
**       (2) NO current attr key, attr_packet EQUAL last_accessed:
**              use_count++, attr key is last_accessed tuple.
**       (3) HAVE current attr key, use_count=1:
**              update attr tuple with attr_packet.
**       (4) HAVE current attr key, use_count>1 (multi-use):
**              use_count-- for attr tuple, then create new tuple for
**              attr_packet, use=1.
**    PARAMETERS   
**       INPUT  : 
**				&attr_packet, address of attribute packet to update with
**				attr_packet.key_id, master key for attribute update
**				attr_packet.rel_num, must equal relation number of attr
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_attr(attr_packet)
struct	UR_attr  *attr_packet	;	/* pointer to attr data packet */
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_attr(key=0x%x)", attr_packet->key_id));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_ATTR, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_attr1(attr_packet, &theMessage, 1));
}

/*********************************************************************
** E_FUNCTION : ur_comp_attr(attr1,attr2)
**    RETURNS      :  0 if same, +/- 1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ur_comp_attr(attr1,attr2,size)
struct UR_attr *attr1,*attr2;
int size;
{
	int *l1,*l2;

	if (size != 52) 
		return (uu_comp_byte(&attr1->color,&attr2->color,size));

	l1 = (int *) attr1->data;
	l2 = (int *) attr2->data;

	if (attr1->color < attr2->color)
		return(-1);
	else if (attr1->color > attr2->color)
		return(1);
	if (attr1->layer < attr2->layer)
		return(-1);
	else if (attr1->layer > attr2->layer)
		return(1);
	if (attr1->pen < attr2->pen)
		return(-1);
	else if (attr1->pen > attr2->pen)
		return(1);
	else if (attr1->line_style < attr2->line_style)
		return(-1);
	else if (attr1->line_style > attr2->line_style)
		return(1);
	if (attr1->line_weight < attr2->line_weight)
		return(-1);
	else if (attr1->line_weight > attr2->line_weight)
		return(1);
	if (attr1->line_width < attr2->line_width)
		return(-1);
	else if (attr1->line_width > attr2->line_width)
		return(1);
	if (attr1->displayable < attr2->displayable)
		return(-1);
	else if (attr1->displayable > attr2->displayable)
		return(1);
	if (attr1->selectable < attr2->selectable)
		return(-1);
	else if (attr1->selectable > attr2->selectable)
		return(1);
	if (*l1 < *l2)
		return(-1);
	else if (*l1 > *l2)
		return(1);

	return(0);
}

/*********************************************************************
** E_FUNCTION : status = ur_update_attr1(&attr_packet, theMessage, messageCnt)
**      Update attribute (new or old) data for existing key.
**      We have 4 cases:
**       (1) NO current attr key, attr_packet NOT EQUAL last_accessed:
**              create new attr tuple, use_count = 1.
**       (2) NO current attr key, attr_packet EQUAL last_accessed:
**              use_count++, attr key is last_accessed tuple.
**       (3) HAVE current attr key, use_count=1:
**              update attr tuple with attr_packet.
**       (4) HAVE current attr key, use_count>1 (multi-use):
**              use_count-- for attr tuple, then create new tuple for
**              attr_packet, use=1.
**    PARAMETERS   
**       INPUT  : 
**				&attr_packet, address of attribute packet to update with
**				attr_packet.key_id, master key for attribute update
**				attr_packet.rel_num, must equal relation number of attr
**				theMessage,	for scheduler
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_attr1(attr_packet, theMessage, messageCnt)
struct	UR_attr  *attr_packet;	/* pointer to attr data packet */
UR_messagePacket	theMessage[];
int					messageCnt;
{
	struct UR_rcb_rec *rcb,*rcb_cur;   /* current relations rcb block */
	struct UR_attr  attr;           /* generic attribute tuple */
	int				status;
	int            retcod;
	UU_REL_ID      rel_key;
	UR_REL_NUM     rel;
	UR_TUPLE_INDX  tuple;
	UR_TUPLE_INDX  last_accessed,last_accessed_cur;
	UU_LOGICAL     create;
	int				comp_size;
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC,(us,"ur_update_attr1(key= 0x%x, rel= %d messageCnt=%d)",
					attr_packet->key_id,attr_packet->rel_num, messageCnt)) ;

	/* get existing attribute bundle's key */
	ur_k2rt(attr_packet->key_id, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (ur_editablep(m_ptr))
	{
		rel_key = m_ptr->assocs[UR_ATTR_INDX];
	}
	else
	{
		status = -1;	/* entity is not editable */
		goto theExit;
	}
	rel = attr_packet->rel_num; /* get relation number */
	rcb = &UR_rcb[rel];         /* get relation block addr */
	last_accessed = rcb->last_accessed_index; /*get last used index*/
	comp_size = rcb->tuple_size - (sizeof(int) * 3) ;

	/* only allow attributes to be associated if the relation is */
	/* indeed an attribute relation */
	if(uu_tst_bit(&(UR_rcb[rel].rel_flags),UR_ATTR_REL))
	{
		/* must create a new attribute bundle if no last used tuple */
		/* otherwise the default is reuse the last attributes */
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
			/* no attribute tuple this master*/
			uu_dprint(UU_RITRC,
				(us,"upd_attr no attr key, last accessed tuple=%d",
				last_accessed));

			/* see if we can re-use the last attribute tuple */
			if(!create)
			{
				/* get last used attr tuple */
				status = ur_retrieve_tuple(rel, last_accessed, &attr);
				if(status!=0)
				{
					goto theExit;   /* cannot continue */
					/* could just create this tuple, but something hosed */
				}
/*				retcod = uu_comp_byte(&attr.color,&attr_packet->color,comp_size); */
				retcod = ur_comp_attr(&attr,attr_packet,comp_size);
				if (retcod==0)
				{
					/* new attr same as last used */
					/* update the attribute tuple use count */
					attr.use_count++;        /* one more mtuple using this one*/
					ur_update_tuple(rel, last_accessed, &attr);	

					/* update the attr key in the mtuple for this update */
					ur_rt2k(rel,last_accessed,&rel_key);
					ur_update_mtuple_attr(attr_packet->key_id,UR_ATTR_INDX,rel_key);
					uu_dprint(UU_RITRC,(us,"attr new same as tuple 0x%x,new use=%d",
							rel_key,attr.use_count));
				}
				else                        /* new attr different from last */
				{
					uu_dprint(UU_RITRC,(us,"upd_attr new diff from last acc"));
					create = UU_TRUE;        /* so we must create a attr tuple*/
				}
			}
		} /* if no current attr for this mtuple */
		else /* mtuple has associated attr tuple already */
		{
			/* get the existing attr tuple */
			ur_k2rt(rel_key,&rel,&tuple); /*get indexes to retrieve*/
			status = ur_retrieve_tuple(rel, tuple, &attr);
			if (status!=0)
			{
				goto theExit;          /* cannot continue */
				/* could just create this tuple, but something hosed */
			}
			/* make sure we are not doing an update of what is already there */
			/* if so, do nothing */
/*			if(uu_comp_byte(&attr.color,&attr_packet->color,comp_size)) */
			if (ur_comp_attr(&attr,attr_packet,comp_size) == 0) 
				goto theExit;
			uu_dprint(UU_RITRC,(us,"current attr key =0x%x,use_count=%d",
						rel_key,attr.use_count));
			if(attr.use_count==1)
			{
				/* only 1 use (this mtuple). Just update tuple with new info */
				attr_packet->use_count = 1; /* set users area use count */
				attr_packet->rel_num = rel; /* and relation number */
				ur_update_tuple(rel, tuple, attr_packet);
				create = UU_FALSE;          /* do not create new tuple */
			}
			else /* 2 or more uses of current attr dec. use count and create */
			{
				attr.use_count--;           /* decrease use count */
				ur_update_tuple(rel, tuple, &attr);
				if(!create)
				{
					last_accessed_cur = last_accessed;
/*
..... test case where the 'if' below was needed had 'ubfn/aaa.u; put/sf58' with
..... sf58 being a planar trimmed surface with composite boundary made of lines
*/
					if (rel != attr_packet->rel_num)
					{
						rcb_cur = &UR_rcb[rel];
						last_accessed_cur = rcb_cur->last_accessed_index;
					}
					/* get last used attr tuple */
					status = ur_retrieve_tuple(rel,last_accessed_cur,&attr) ;
					if(status != 0)
					{
						goto theExit;	/* cannot continue */
						/* could just create this tuple, but something hosed */
					}
					/* if not the same as the last accessed, create a new one, */
					/* otherwise re_use the last accessed */
/*					if(uu_comp_byte(&attr.color,&attr_packet->color,comp_size)) */
					if (ur_comp_attr(&attr,attr_packet,comp_size) != 0) 
					{
						/* not same as last */
						create = UU_TRUE;           /* must create a new one */
					}
					else /* same as last accessed */
					{
						/* update the use count of the existing attr */
						attr.use_count++;
						ur_update_tuple(rel, last_accessed_cur, &attr);

						/* set the attr assoc key in the mtuple being updated */
						ur_rt2k(rel,last_accessed_cur,&rel_key) ;
						ur_update_mtuple_attr(attr_packet->key_id,UR_ATTR_INDX,
							rel_key);
						uu_dprint(UU_RITRC,
							(us,"upd_attr new same as tuple 0x%x,new use=%d",
							rel_key,attr.use_count));
					}
				} /* !create	*/
			}
		} /* end master attr exists */
		if(create)
 		{
			/* Create new attribute tuple - set use count to 1 */
			rel = attr_packet->rel_num; /* get relation number */
			attr_packet->use_count = 1;
			status = ur_create_tuple(rel, &tuple, attr_packet);
			if(status!=0)
			{
				goto theExit;     /* cannot continue */
			}
			ur_rt2k(rel, tuple, &rel_key); /* get attr key */
			ur_update_mtuple_attr(attr_packet->key_id,UR_ATTR_INDX,rel_key);
			uu_dprint(UU_RITRC,(us,"create attr tuple key =0x%x", rel_key));
		}

		/* get master rel & tuple, and a pointer to the master fixed data */
		ur_k2rt(attr_packet->key_id, &rel, &tuple);
		status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

		/* now notify associates */
		if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
		{
			uri_notify(m_ptr, attr_packet->key_id, theMessage, messageCnt);
		}
	}
	else
	{
		/* tried to use a non-attribute relation as an attribute bundle */
		uu_dprint(UU_RITRC,(us,"illegal attribute relation %d",rel)) ;
		status = -1 ;
	}
/*----------- function exit ----------------------------------------*/
theExit:                                /******* EXIT LABEL ******/
	uu_dprint(UU_RITRC,(us,"ur_update_attr exit status=%d",status));
	uu_dexit ;
	return(status) ;
}
