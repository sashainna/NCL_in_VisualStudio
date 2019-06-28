/*********************************************************************
**    NAME         :  nepick1.c
**       CONTAINS: User interface routines for highlighting pick elements.
**
**       ncl_get_seglabel
**       ncl_getdisp_shade
**       ncl_getpick_level
**       ncl_getpick_level0
**
**     MODULE NAME AND RELEASE LEVEL
**       nepick1.c , 25.5
**     DATE AND TIME OF LAST MODIFICATION
**       02/28/17 , 15:43:23
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdpick.h"
#include "modef.h"
#include "mcrv.h"

#include "mdeval.h"
#include "mgeom.h"

#include "mdgenent.h"
#include "ncl.h"
#include "vsegbf.h"
#include "mdrel.h"
#include "modef.h"
#include "msol.h"
#include "nccs.h"
#include "bsym.h"
#include "mattr.h"
#include "msrf.h"

/*********************************************************************
**    E_FUNCTION     :  ncl_get_seglabel(segno, pickid, surftext)
**       Get a segment label
**    PARAMETERS
**       INPUT  :
**          segno:   segment number
**			pickid: picking ID
**       OUTPUT :
**          surftext: label of segment       
**    RETURNS      : shade
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_seglabel(segno, pickid, surftext)
int segno, pickid;
char *surftext;
{
	long buffer[UG_UDATA_SIZE];
	struct UC_entitydatabag e;
	int level, sub, loop;

	gsegrud(segno,buffer);
	e.key = uv_getkey(buffer);
	ur_retrieve_data_fixed(&e);
	ncl_get_label(&e, surftext);
	level = ncl_getpick_level(segno, pickid, &sub, &loop);
	if (level==2)
	{
		if ((loop>=0)&&(sub>=0))
			sprintf(surftext, "%s_%d_%d", surftext, loop, sub);
		else if (loop>=0)
			sprintf(surftext, "%s_%d", surftext, loop);
		else if (sub>=0)
			sprintf(surftext, "%s_%d", surftext, sub);
	}
	return level;
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_getdisp_shade (eptr)
**       Get 'shaded' parameters for a surface.
**    PARAMETERS
**       INPUT  :
**          eptr       - ptr to surface
**       OUTPUT :
**          shade       
**    RETURNS      : shade
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_getdisp_shade (eptr)
struct NCL_fixed_databag *eptr;
{
	struct UC_attributedatabag attrptr;
	struct UM_surfattr_rec *sfattr;

	uc_retrieve_attr(eptr->key, &attrptr);
	sfattr = (struct UM_surfattr_rec *)&attrptr;
	switch (eptr->rel_num)
	{
		case UM_SOLID_REL:
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_NETSF_REL:
		case NCL_MESHSURF_REL:
		case NCL_TRIMSF_REL:
		case NCL_REVSURF_REL:
		{
			if (sfattr->rel_num != UM_SURFATTR_REL)
			{
				return (UM_srfattr.shaded);
			}
			else
			{
				return (sfattr->shaded);
			}
		}
		default:
		{
			return 0;
		}
	}
}
/*********************************************************************
**    E_FUNCTION     :  ncl_getpick_level(segno, pickid, sub, loop)
**       Get the pick level of a pickid of segment
**    PARAMETERS
**       INPUT  :
**          segno       - segment number
**			pickid:		pickid of the segment
**       OUTPUT :
**          sub:	sub index numbr of the surface
**			loop: loop number
**    RETURNS      : picking level
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_getpick_level(segno, pickid, sub, loop)
int segno, pickid, *sub, *loop;
{
	long buffer[UG_UDATA_SIZE];
	int key, rel_num;

	gsegrud(segno,buffer);
	key = uv_getkey(buffer);
	rel_num = uv_getrelnum(buffer);

	return ncl_getpick_level0(key, rel_num, pickid, sub, loop);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_getpick_level0(key1, rel_num, pickid, sub, loop)
**       Get the pick level of a pickid of segment
**    PARAMETERS
**       INPUT  :
**          key1, rel_num:       segment key and relation number
**			pickid:		pickid of the segment
**       OUTPUT :
**          sub:	sub index numbr of the surface       
**			loop: loop number
**    RETURNS      : picking level
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_getpick_level0(key1, rel_num, pickid, sub, loop)
int key1, rel_num, pickid, *sub, *loop;
{
	struct UC_entitydatabag e;
	int i, subid, cvid, cvkey, status, errfl;
	int key2, nkeys;
	struct NCL_trimsf_rec sf;
	struct NCL_fixed_databag cv;
	UU_LOGICAL Iscompcv,lcurve,lvalid, found, test;
	struct UM_compcrv_rec *ceptr;
	struct UM_solid_rec *septr;
	UU_KEY_ID key,*kptr;
	int lstix,ndisp,shaded,ecolor,nup,nvp,nu,nv;
	int level, sh_mode,sh_symbol,lnstyle0,color0,displstyp;

	*loop = -1;
	*sub = -1;
	switch (rel_num)
	{
		case  UM_BODY_REL:
		{
			struct UM_body_rec e1;
			e1.key = key1;
			um_get_all_geom(&e1, sizeof(e1));					
			pickid = pickid - 2;
			if (pickid == -1)
				key2 = key1;
			else if ((pickid >= 0) && (pickid <= e1.no_edge-1))
				key2 = (UU_KEY_ID) (0x80000000 | e1.edge[pickid]);
			else
			{
				key2 = key1;
			}
		}
		break;
		case UB_INSTANCE_REL:
		{
			struct UB_instance_rec e1;
			e1.key = key1; 
			ub_retrieve_sym(&e1, sizeof(struct UB_instance_rec));
			if (pickid <= e1.no_geom)
				key2 = (UU_KEY_ID) e1.geom[pickid-1];
			else if (pickid <= e1.no_geom+e1.no_snap_nod)
				key2 = (UU_KEY_ID)e1.snap_nod[pickid-(e1.no_geom+1)].snap_key;
			else if (pickid <= e1.no_geom+e1.no_snap_nod+e1.no_text_nod)
				key2 = 
				(UU_KEY_ID) e1.text_nod[pickid-(e1.no_geom+e1.no_snap_nod+1)].text_key;
			else
				key2 = (UU_KEY_ID)pickid;
		}
		break;
		default:
			key2 = (UU_KEY_ID) pickid;
			break;
	}
	if (key1!=key2)
	{
		subid = -1;
		sf.key = key1;
		status = ncl_retrieve_data_fixed(&sf);	
		sh_mode = ncl_shading_mode ();
		if (ncl_getdisp_shade(&sf)==0) sh_mode = 0;
/*
......ncl_get_cvkey will only get key for trim-surf
*/
		if (sf.rel_num!=NCL_TRIMSF_REL)
			goto check2;
		ncl_get_cvkey(&sf,key2,&cv.key,&errfl, sh_mode);
		if (errfl != 0)
		{
			test = (cv.rel_num == UM_COMPCRV_REL);
			goto check2;
		}
		else if (sf.rel_num==UM_SOLID_REL)
		{
			goto check2;
		}
		if (cv.key == sf.uv_key)
			subid = 0;
		else
		{
			for (i=0;i<sf.no_ibndykey && (subid==-1);i++)
			{
				if ( cv.key == sf.ibndykey[i])
					subid = (i+1)/2;
			}
			*loop = subid;
		}
		if (subid >= 0)
			Iscompcv = UU_TRUE;

		if (subid >= 0)
		{
			lvalid = UU_FALSE;
			status = ncl_retrieve_data_fixed(&cv);

			test = (cv.rel_num == UM_COMPCRV_REL);
			if (status == UU_SUCCESS)
			{
				lcurve = (uc_super_class(cv.rel_num) == UC_CURVE_CLASS);
				if (Iscompcv)
					lvalid = (lcurve);
				else
					lvalid = (lcurve && cv.rel_num != UM_COMPCRV_REL);
			}
		}
		if (!lvalid) 
		{
			subid = -1;
			*loop = -1;
		}
		if (subid >= 0 && Iscompcv)
		{
			if (sh_mode)
				subid = (key2-sf.key)%100;
			else
				subid = (key2)%100;
		}
		else
			subid = 0;
		
		if ((subid>0)||(*loop!=-1))
		{
			*sub = subid;
			return 2; 
		}
check2:;
		if (subid <= 0)
		{
			cv.key = key1;
			status = ncl_retrieve_data_fixed (&cv);
			if (cv.rel_num==UM_COMPCRV_REL)
			{
				ceptr = (struct UM_compcrv_rec *) &cv;
				found = UU_FALSE;
				for (i = 0; i < ceptr->no_cid && !found; i++)
					found = (key2 == ceptr->cid[i].crvid);
				if (found) 
				{
					*sub = i;
					return 2;
				}
			}
			else if (cv.rel_num==NCL_TRIMSF_REL)
			{
				ceptr = (struct UM_compcrv_rec *) &cv;
				for (i=0;i<sf.no_ibndykey && (subid==-1);i++)
				{
					if ( key2 == sf.ibndykey[i])
						subid = (i+1)/2;
				}
				if (subid>0)
				{
					*sub = i;
					return 2;
				}
			}
			else if (cv.rel_num==UM_SOLID_REL)
			{
				septr = (struct UM_solid_rec *)&cv;
				nkeys = 0;
				if (septr->type == UM_COMPOS_SOLID)
				{
					nkeys = septr->no_netkey;
					kptr = septr->netkey;
				}
				for (i=0;i<nkeys;i++)
				{
					key = kptr[i];
					sf.key = key;
					cv.key = key;
					status = ncl_retrieve_data_fixed(&cv);	
					status = ncl_retrieve_data_fixed(&sf);
					if (sf.key==key2)
					{
						*sub = i+1;
						return 2;
					}
					level = ncl_getpick_level0(key, sf.rel_num, key2, sub, loop);
					if (level==2)
					{
						*sub = i+1;
						return 2;
					}
					else
						continue;
				}
			}
		}
	}
	return 1;
}
