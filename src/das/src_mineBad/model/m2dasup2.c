
/*********************************************************************
**    NAME         : m2dasup2.c
**       CONTAINS:
**			int um_dl_pdas(type,subnum,prnum,data_ary,
**			int um_dl_pldas(type,subnum,prnum,data_ary,
**			int um_d_pickresolve(pick, level, pent)
**			int um_d_travresolve(pickpath, endpt1,endpt2);
**			UU_KEY_ID um_get_pickkey(pent, level)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2dasup2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:45
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "msol.h"
#include "mdpick.h"
#include "view.h"
#include "bsym.h"
#include "mdeval.h"
#include "nccs.h"

/*********************************************************************
**    E_FUNCTION     : int um_dl_pdas(type,subnum,prnum,data_ary,
**													data_size,numint,level)
**      This routine is a temporary implementation of a "limit the das"
**      system for picking. The calling routine will specify what type of
**      geometry entities are legal. This routine will check to see if the
**      picked data is of the appropriate type. Further, if the requested
**      data is a "component" of the actual picked data (e.g. edge/face
**      of body or component of composite curve), this routine will return
**      the  MTID of the component.
**    PARAMETERS   
**       INPUT  : 
**          type				type of DAS input
**				subnum			subsystem number
**				prnum				prompt number
**				data_ary			array to place MTID information
**				data_size		maximum number of MTIDs to return
**				level				1 => pick at segment level;
**									2 => pick at item level within segment
**       OUTPUT :  
**          numint			number of picked entities
**    RETURNS      :
**			UU_TRUE if no errors; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_dl_pdas(type,subnum,prnum,data_ary,data_size,numint,level)
	int type;
	int subnum;
	int prnum;
	UM_PICKENT *data_ary;
	int data_size;
	int *numint;
	int level;
	 
	{
	UD_PPICKREC pick;						/* data for entity picked */
	int numpick;							/* number of entities picked */
	int status;								/* status */
	UU_LOGICAL das_status;				/* status of das calls */

	uu_denter( UU_MTRC,(us,"um_dl_pdas(?..?)"));
	*numint = 0;
	status = -1;
	if (type != UD_DASPICK)
		{
		uu_uerror0(/*um_d_pdas: illegal type specification*/UM_MODEL,42);
		}
	else
		{
		numpick = 1;
		status = 0;
		while ((numpick != 0) && (*numint < data_size))
			{
			das_status = ud_ldas(type,subnum,prnum,&pick,1,&numpick,UD_NODEFAULT);
			if ((das_status == UU_TRUE) && (numpick != 0))
				{
				if (level > pick.depth)
					{
					uu_uerror0(/*um_d_pdas: illegal entity picked*/UM_MODEL,43);
					status = UU_FALSE;
					}
				else
					{
					status = um_d_pickresolve(&pick,level,&data_ary[*numint]);
					if (status == 0) (*numint)++;
					}
				}
			}
		}
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION : int um_dl_pldas(type,subnum,prnum,data_ary,
**												data_size,numint,level)
**      This routine is a temporary implementation of a "limit the das"
**      system for picking. The calling routine will specify what type of
**      geometry entities are legal. This routine will check to see if the
**      picked data is of the appropriate type. Further, if the requested
**      data is a "component" of the actual picked data (e.g. edge/face
**      of body or component of composite curve), this routine will return
**      the MTID of the component.
**    PARAMETERS   
**       INPUT  : 
**          type				type of DAS input
**				subnum			subsystem number
**				prnum				prompt number
**				data_ary			array to place PLOCREC information
**				data_size		maximum number of MTIDs to return
**				level				1 => pick at segment level;
**									2 => pick at item level within segment
**       OUTPUT :  
**          numint			number of picked entities
**    RETURNS      :
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_dl_pldas(type,subnum,prnum,data_ary,data_size,numint,level)
	int type;
	int subnum;
	int prnum;
	UM_PLOCREC data_ary[];
	int data_size;
	int *numint;
	int level;

	{
	UD_PLOCREC pick;								/* data for entity picked */
	int numpick;									/* number of entities picked */
	int status;										/* status */
	UU_LOGICAL das_status;							/* status of das calls */

	uu_denter(UU_MTRC,(us,"um_dl_pldas(%d,%d,%d,%x,%d,,%d)",
					type,subnum,prnum,data_ary,data_size,level));
	*numint = 0;
	if (type != UD_DASPCKLOC)
		{
		uu_uerror0(/*um_d_pldas: illegal type specification*/UM_MODEL,44);
		status = -1;
		}
	else
		{
		status = 0;
		numpick = 1;
		while ((numpick != 0) && (*numint < data_size))
			{
			das_status = ud_ldas(type,subnum,prnum,&pick,1,&numpick,UD_NODEFAULT);
			if (numpick != 0)
				{
				if (level > pick.ppath.depth)
					{
					uu_uerror0(/*um_d_pldas: illegal entity picked*/UM_MODEL,45);
					}
				else
					{
					status = um_d_pickresolve(&pick,level,&data_ary[*numint].pent);
					if (status == 0)
						{
						um_copyploc(&(pick.pndc), &(data_ary[*numint].ploc));
/*
.....
.....Added by Paul to copy the new part of UD_PLOCREC to UM_PLOCREC
.....07/15/92 
.....
*/
               strcpy(data_ary[*numint].ploc.label,pick.pndc.label);
/*****/
						(*numint)++;
						}
					}
				}
			}
		}
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_d_pickresolve(pick, level, pent)
**			Convert a DIGS pick path (PICK) (i.e. pairs of (display segment,
**			pickids)) into a modeling pick path (PENT) (i.e. a list of
**			UNIBASE keys of picked entities)
**    PARAMETERS   
**       INPUT  : 
**				pick					pick record buffer
**				level					1 => pick at segment level
**										2 => pick item within segment
**       OUTPUT :  
**          pent					pick path to picked entity
**    RETURNS      : none
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_d_pickresolve(pick, level, pent)
	UD_PPICKREC *pick;
	int level;
	UM_PICKENT *pent;
	{
	int rel_num;
	int dsegid;								/* display segment identifier */
	int pickid;								/* pick identifier within segment */
	uv_segbuff(buffer);					/* user defined segment data */
	int status;

	uu_denter(UU_MTRC,(us,"um_d_pickresolve(dsegid=%d,pickid=%d,level=%d)",
		pick->pickpath[0],pick->pickpath[1],level));
	status = 0;
	dsegid = pick->pickpath[0];
	pickid = pick->pickpath[1];
	gsegrud(dsegid,buffer); /* segment id to get segment user data */
	pent->num = 2; /* number of levels in pick path */
	pent->key[0] = uv_getkey(buffer); /* get first key in pick path */
	rel_num = uv_getrelnum(buffer);
	switch (rel_num)
		{
/*
		case UM_AGSHELL_REL:
         pent->key[1] = (UU_KEY_ID) -pickid;
			break;
*/
		case  UM_BODY_REL:
			{
			struct UM_body_rec e;
			e.key = pent->key[0];
			um_get_all_geom(&e, sizeof(e));					
			pickid = pickid - 2;
         if (pickid == -1)
            pent->key[1] = pent->key[0];
         else if ((pickid >= 0) && (pickid <= e.no_edge-1))
            pent->key[1] = (UU_KEY_ID) (0x80000000 | e.edge[pickid]);
			else
				{
				pent->key[1] = pent->key[0];
				if (level ==  2)
					{
					uu_uerror0(/*um_d_pickresolve: illegal pick id */ UM_MODEL,47);
					status = -1;
					}
				}
			}
			break;
		case UB_INSTANCE_REL:
			{
			struct UB_instance_rec e;
			e.key = pent->key[0]; 
			ub_retrieve_sym(&e, sizeof(struct UB_instance_rec));
			if (pickid <= e.no_geom)	/* geometry subentity was picked */
				pent->key[1] = (UU_KEY_ID) e.geom[pickid-1];
			else if (pickid <= e.no_geom+e.no_snap_nod)/* a snap node was picked */
				pent->key[1] = (UU_KEY_ID)e.snap_nod[pickid-(e.no_geom+1)].snap_key;
			else if (pickid <= e.no_geom+e.no_snap_nod+e.no_text_nod)/* a text node was picked */
				pent->key[1] = 
				(UU_KEY_ID) e.text_nod[pickid-(e.no_geom+e.no_snap_nod+1)].text_key;
			else
				pent->key[1] = (UU_KEY_ID)pickid;
			}
			break;
		default:
			pent->key[1] = (UU_KEY_ID) pickid;
			break;
		}
/*
	sprintf(UM_sbuf,"exit um_d_pickresolve: num=%d, pent[0]=%d, pent[1]=%d",
		pent->num, pent->key[0], pent->key[1]);
	um_pscroll(UM_sbuf);
*/
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : UU_KEY_ID um_get_pickkey(pent, level)
**			Given the pick path for a picked entity (PENT), return the
**			UNIBASE key of the entity at the specified LEVEL.
**    PARAMETERS   
**       INPUT  : 
**          pent						pick path of picked entity
**				level						level within pick path to extract
**											key
**       OUTPUT :  
**				none
**    RETURNS      : 
**				key of entity picked at the given level
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID
um_get_pickkey(pent, level)
	UM_PICKENT *pent;
	int level;

	{
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"um_get_pickkey(%x,%d)",pent, level));
	if ((level >= 0) && (level <= pent->num))
		{
		key = pent->key[level-1];
		}
	else
		{
		key = -1;
		um_pscroll("ERROR: um_get_pickkey");
		}
	uu_dexit;
	return (key);
	}
/*********************************************************************
**    E_FUNCTION     : int um_d_travresolve(pickpath,endpt1,endpt2)
**          Returns endpoints and key of entity.
**    PARAMETERS
**       INPUT  :
**              pickpath 
**       OUTPUT : 
**              endpt1
**              endpt2 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void um_d_travresolve(pickpath,pt1,pt2)
UM_coord *pt1,*pt2;
UD_PPICKREC *pickpath;

{
	int status;
/*
... aak 10-nov-1997: replaced
	struct UC_entitydatabag e;
*/
	struct NCL_fixed_databag e;
 	struct UM_evcrvout evcrv;
	UM_transf tfmat;
	UM_PICKENT pent;


	status = um_d_pickresolve(pickpath, 1, &pent);
	e.key = um_get_pickkey(&pent, 1);
	pickpath->pickpath[pickpath->depth-1] = (int)e.key;

/*
... aak 10-nov-1997: replaced
	status = uc_retrieve_data(&e, sizeof(e));
*/
	status = ncl_retrieve_data_fixed(&e);
	status = uc_retrieve_transf(e.key, tfmat);
	status = uc_init_evcrvout(&e, &evcrv);
	status = uc_evcrv(UM_POINT, (UU_REAL) 0.0, &e, tfmat, &evcrv);
	um_vctovc(evcrv.cp, pt1);
	status = uc_evcrv(UM_POINT, (UU_REAL) 1.0, &e, tfmat, &evcrv);
	um_vctovc(evcrv.cp, pt2);
}

