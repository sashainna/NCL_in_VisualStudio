/*********************************************************************
**    NAME         :  neuvcvass.c
**       CONTAINS: routine to support the uvcv associations
**       int ncl_update_sskey()
**       int ncl_delete_sskey()
**       int ncl_retrieve_sskeys(sfptr,lst,no_sskey,keys)
**       int ncl_cp_struct_uvcv_rbcv1 (eptr,rbcv)
**
**    COPYRIGHT 1997 (c) NCCS INC.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       neuvcvass.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:56
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mcrv.h"
#include "msrf.h"
#include "nclfc.h"
#include "nccs.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : ncl_update_sskey (sfptr,cvkey,actfl)
**       Create unibase entity UM_uvcvass_rec to store surface key and
**       keys of uv curves associated with this surface.  Only one
**       entity of this type will be created in unibase.
**    PARAMETERS   
**       INPUT  : 
**          cvkey   - key of initialized cv on surface entity.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_update_sskey (sfptr,cvkey,actfl)
struct NCL_fixed_databag *sfptr;
UU_KEY_ID cvkey;
int actfl;
{
	int status;
	int lst,no_sskey;
	int i, num, indx;
	UU_KEY_ID *keys,ckey;

	status = ncl_retrieve_sskeys (sfptr,&lst,&no_sskey,&keys);
	if (status != UU_SUCCESS) return (UU_FAILURE);
	if (lst == 0) return(UU_SUCCESS);

	status = UU_FAILURE;
/*
.....add new curve to list of curves on surface
*/
	if (actfl == 1)
	{
		indx = no_sskey + 1;	
		ckey = cvkey;
		status = ur_update_data_varlist(sfptr->key, lst, &ckey, indx, 1);
	}
/*
.....delete curve from list of curves on surface
.....since unibase tool deletes key when curve is removed
.....delete this atom from list for deleted curves so it does
.....not have 0 for key
*/
	else
	{
		num  = no_sskey;
		for (i=0; i<num; i++)
			if (keys[i] == cvkey || keys[i] == 0) break;

		if (i < num)
			status = ur_delete_varlist_atoms (sfptr->key,lst,i+1,1); 
	}		

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_delete_sskey (sfkey)
**       Create unibase entity UM_uvcvass_rec to store surface key and
**       keys of uv curves associated with this surface.  Only one
**       entity of this type will be created in unibase.
**    PARAMETERS   
**       INPUT  : 
**          cvkey   - key of initialized cv on surface entity.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_delete_sskey (sfkey,cvkey)
UU_KEY_ID sfkey,cvkey;
{
	int status;
	struct NCL_fixed_databag sfptr;

	sfptr.key = sfkey;
	status = ncl_retrieve_data_fixed (&sfptr);

	if (status == UU_SUCCESS)
		status = ncl_update_sskey (&sfptr,cvkey,0);

	return(status);
}

/*********************************************************************
**    FUNCTION     : ncl_retrieve_sskeys(sfptr,lst,no_sskey,keys)
**      Gets sskey data in a UNIBASE surface entity.
**    PARAMETERS
**       INPUT  :
**             sf       - a surface
**       OUTPUT :
**             lst        - list number
**             no_sskey    
**             keys       - pointer to the list
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_retrieve_sskeys (sf,lst,no_sskey,keys)
struct NCL_fixed_databag *sf;
int *lst,*no_sskey;
UU_KEY_ID **keys;
{
	int status = UU_SUCCESS;

	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		{
			struct UM_rbsplsrf_rec *sf1;

			sf1 = (struct UM_rbsplsrf_rec *) sf;
			*lst = 5;
			*no_sskey = sf1->no_sskey;
			*keys = sf1->sskey;
			break;
		}
		case NCL_SURF_REL:
		{
			struct NCL_surface_rec *sf1;
			sf1 = (struct NCL_surface_rec *) sf;
			*lst = 2;
			*no_sskey = sf1->no_sskey;
			*keys = sf1->sskey;
			break;
		}
		case NCL_NETSF_REL:
		{
			struct NCL_netsf_rec *sf1;
			sf1 = (struct NCL_netsf_rec *) sf;
			*lst = 2;
			*no_sskey = sf1->no_sskey;
			*keys = sf1->sskey;
			break;
		}
		case NCL_REVSURF_REL:
		{
			struct NCL_revsurf_rec *sf1;
			sf1 = (struct NCL_revsurf_rec *)sf;
			*lst = 1;
			*no_sskey = sf1->no_sskey;
			*keys = sf1->sskey;
			break;
		}
		default:
			*lst = 0;
			*no_sskey = 0;
/* 			status = UU_FAILURE; */
			break;
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_cp_struct_uvcv_rbcv1  (eptr,rbcv)
**       Convert a uv curve on surface into a rational bspline curve
**       The new structure uses same pointers to t, pt and wt arrays
**       as old (eptr) so it CAN NOT be used where these data is
**       afected (split, trim etc).
**    PARAMETERS
**       INPUT  :
**          eptr           uv on surface curve structure
**       OUTPUT :
**          rptr           rational bspline curve struct of uvonsf
**    RETURNS      :
**          0              no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int
ncl_cp_struct_uvcv_rbcv1 (eptr,rbcv)
struct UM_uvcvonsf_rec *eptr;
struct UM_rbsplcrv_rec *rbcv;
{
/*
.....not crucial numbers for evaluation
*/
	rbcv->subscr = eptr->subscr;
/*
... aak 03-nov-1997: use the "planar" field to memorize that this rbsplcrv came
... from a uvcvonsf curve
*/
	strncpy(rbcv->label,eptr->label, NCL_MAX_LABEL);
	rbcv->planar = eptr->planar + UM_UVCVONSF_REL;
	rbcv->open = eptr->open;
	rbcv->closdinu = eptr->closdinu;
	rbcv->no_displst = eptr->no_displst;
	rbcv->displst = eptr->displst;
/*
.....important data
*/
	rbcv->key = eptr->key;
	rbcv->rel_num = UM_RBSPLCRV_REL;
	rbcv->k = eptr->k;
	rbcv->n = eptr->n;
	rbcv->t0 = eptr->t0;
	rbcv->t1 = eptr->t1;
	rbcv->no_t = eptr->no_t;
	rbcv->t = eptr->t;
	rbcv->no_pt = eptr->no_pt;
	rbcv->pt = eptr->pt;
	rbcv->no_wt = eptr->no_wt;
	rbcv->wt = eptr->wt;

	return (UU_SUCCESS);
}

