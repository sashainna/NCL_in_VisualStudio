/*********************************************************************
**    NAME         :  necvsup.c
**       CONTAINS: functions supporting 'ranfile' replacement used
**           to create uv curves on surface.
**
**       int ncl_check_key (sfkey,cvkey)
**       nclf_get_bnkey
**       nclf_cp_struct_rbcv_uvcv
**       int ncl_put_uv (pts,npt)
**       int ncl_push_uv (pts)
**       int ncl_insert_uv (pts, npt,ind)
**       int ncl_put_asn (pts, num)
**       int ncl_push_asn (pts)
**       int ncl_get_asn (ist,asn,num)
**       int ncl_get_ent (indx,pts)
**       int ncl_weed_uv(npt)
**       int ncl_getn_uv (pts,npt)
**       void ncl_getpts_uv (npt,pts)
**       int ncl_revers_uv (pts,npt)
**       int ncl_free_uv ()
**       void ncl_nul_uv()
**       int ncl_replace_uv (indx,u,v)
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       necvsup.c , 25.2
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 17:13:52
*********************************************************************/
#include "ncldef.h"
#include "nccs.h"
#include "class.h"
#include "mattr.h"
#include "mdattr.h"
#include "mcrv.h"
#include "view.h"
#include "dasnog.h"
#include "dselmask.h"
#include "ginqatt.h"
#include "gobas.h"
#include "gtbl.h"
#include "mdpick.h"
#include "modef.h"
#include "ulist.h"
#include "mdcpln.h"
#include "zsysdep.h"
#include "class.h"
#include "nclfc.h"

UU_LIST NCL_uvintof;
int NCL_uvintof_init = 0;
extern int NCLX_internal_geom; 

/*********************************************************************
**    E_FUNCTION     : ncl_check_key (sfkey, cvkey)
**       Get surface key from CV on SF data structure. F77 callable.
**    PARAMETERS
**       INPUT  :
**          sfkey    - key of underlying surface used in CV on sf 
**          cvkey    - key of CV on sf entity
**       OUTPUT :
**          cvkey    - Updated key of CV on sf entity if input was
**                     a composite curve and the component curves are
**                     on different surfaces.
**    RETURNS      :
**       UU_SUCCESS if no error; else NCL error number
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_check_key (sfkey,cvkey)
int *sfkey, *cvkey;
{
	int i, num,rev,status, err = 470;
	UU_KEY_ID skey,bkey;
	struct NCL_fixed_databag cvsf;
	struct NCL_fixed_databag uvcvi;
	struct UM_uvcvonsf_rec *uvcv;
	struct UM_compcrv_rec  *compcrv;
	struct NCL_trimsf_rec trimsf;

	if (NCLX_internal_geom) return (UU_SUCCESS);

/*
.....Get base surface key
*/
	skey = *sfkey;
	bkey = 0;
	trimsf.key = *sfkey;
	if (ncl_retrieve_data_fixed (&trimsf) == UU_SUCCESS)
			if (ncl_itsa_trimsrf(&trimsf)) bkey = trimsf.bs_key;
/*
.....Get CVonSF
*/
	cvsf.key = *cvkey;
	if (ncl_retrieve_data_fixed (&cvsf) != UU_SUCCESS) return(err);
/*
......Single CVonSF
*/
	if (!ncl_itsa_compcrv(&cvsf))	
	{
		uvcv = (struct UM_uvcvonsf_rec *)&cvsf;
		if (skey != uvcv->bskey && bkey != uvcv->bskey)
			return(err);
		return (UU_SUCCESS);
	}
/*
.....Composite curve
.....Added support for CvonSf's to be on different surfaces
.....Bobby - 09/29/14
*/
	else
	{
		compcrv = (struct UM_compcrv_rec  *)&cvsf;
		status = ncl_compcrv_getnents(compcrv,&num);
/*
........First check if component curves
........Reference multiple surfaces
*/

		for(i=0; i<num && status == UU_SUCCESS; i++)
		{
			status = ncl_compcrv_getelm(compcrv,i,&uvcvi,&rev);
			if(status != UU_SUCCESS) return (err);
			uvcv = (struct UM_uvcvonsf_rec *)&uvcvi;
			if (skey == uvcv->bskey || bkey == uvcv->bskey)
			{
//				*cvkey = uvcv->key;
				return(UU_SUCCESS);
			}
		}
		return(err);
	}
}

/*********************************************************************
**    E_FUNCTION     : nclf_get_bnkey(sfkey,cvid,cvkey,nents)
**       Return a boundary key and the number of curves in the boundary
**       from a trimmed surface.
**    PARAMETERS
**       INPUT  :
**          sfkey    - Key of trimmed surface.
**          cvid     - Boundary curve to retrieve.  0 = Outer boundary.
**       OUTPUT :
**          cvkey    - Key of boundary curve.
**          nents    - Number of components in boundary curve.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_get_bnkey(sfkey,cvid,cvkey,nents)
UU_KEY_ID *sfkey,*cvkey;
int *cvid,*nents;
{
	int relnum;
	struct NCL_trimsf_rec ent;
	struct UM_compcrv_rec  *comp;
/*
.....Initialize routine
*/
	*cvkey = 0;
	*nents = 0;
/*
.....Get the trimmed surface
*/
	ent.key = *sfkey;
	ur_retrieve_data_relnum(ent.key,&relnum);
	if (relnum != NCL_TRIMSF_REL) return;
	if (ncl_retrieve_data(&ent) != UU_SUCCESS) return;
/*
.....Get the appropriate curve key
*/
	if (*cvid == 0)
		*cvkey = ent.uv_key;
	else if (*cvid <= ent.no_ibndykey)
		*cvkey = ent.ibndykey[2*(*cvid)-1];
	else
		return;
/*
.....Return the number of entities in the boundary curve
*/
	ent.key = *cvkey;
	if (ncl_retrieve_data_fixed(&ent) != UU_SUCCESS) return;
	if (ent.rel_num == UM_COMPCRV_REL)
	{
		comp = (struct UM_compcrv_rec  *)&ent;
		*nents = comp->no_cid;
	}
	else
		*nents = 1;
}

/*********************************************************************
**    E_FUNCTION     : nclf_cp_struct_rbcv_uvcv(cvkey,sfkey,key)
**       Copies a boundary curve B-spline structure into a CVonSF structure.
**    PARAMETERS
**       INPUT  :
**          cvkey    - Key of curve to convert.
**          sfkey    - Key of surface that CVonSF is located on.
**       OUTPUT :
**          key      - Key of created CVonSF.
**    RETURNS      : none
**    SIDE EFFECTS :
**          The created CVonSF is stored in the Unibase using a blank
**          label and should be deleted when the calling routine is
**          finished with it.
**    WARNINGS     : none
*********************************************************************/
void nclf_cp_struct_rbcv_uvcv(cvkey,sfkey,key)
UU_KEY_ID *sfkey,*cvkey,*key;
{
	struct UC_entitydatabag ent;
	struct UM_uvcvonsf_rec uvcv;
/*
.....Get curve
*/
	ent.key = *cvkey;
	uc_retrieve_data(&ent,sizeof(struct UC_entitydatabag));
/*
.....Convert line
*/
	if (ent.rel_num == UM_LINE_REL)
	{
		um_cp_struct_line_uvcv(&ent,sfkey,&uvcv);
		*key = uvcv.key;
	}
/*
.....Convert B-spline
*/
	else if (ent.rel_num == UM_RBSPLCRV_REL)
	{
		um_cp_struct_rbcv_uvcv(&ent,sfkey,&uvcv);
		*key = uvcv.key;
	}
/*
.....Unrecognized curve type
*/
	else
		*key = 0;
}

/**********************************************************************
**    E_FUNCTION     : ncl_put_uv (pts, npt)
**       Push points on common list to use by C functions to create
**       rbspline from set of points. Note: if list is not initialized
**       this will initialize list first. F77 callable function.
**    PARAMETERS
**       INPUT  :
**          npt     - number of points in buffer to store
**          pts     - buffer with points
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_put_uv (pts,npt)
UU_REAL *pts;
UM_int2 *npt;
{
	int num;
	if (NCL_uvintof_init == 0) 
	{
		uu_list_init (&NCL_uvintof,sizeof(UU_REAL),1200,1200);
		NCL_uvintof_init = 1;
	}
	num = *npt * 3;
	uu_list_push_multiple(&NCL_uvintof,num,pts);

	return(UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : ncl_push_uv (pts)
**       Push a points onto the list. F77 callable function.
**       Note: if list is not initialized this will initialize list first. 
**    PARAMETERS
**       INPUT  :
**          pts     - point
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_push_uv (pts)
UU_REAL *pts;
{
	if (NCL_uvintof_init == 0) 
	{
		uu_list_init (&NCL_uvintof,sizeof(UU_REAL),1200,1200);
		NCL_uvintof_init = 1;
	}
	uu_list_push_multiple(&NCL_uvintof,3,pts);

	return(UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : ncl_insert_uv (pts, npt,ind)
**       Insert points to common list to use by C functions to create
**       rbspline from set of points. Note: if list is not initialized
**       this will initialize list first. F77 callable function.
**    PARAMETERS
**       INPUT  :
**          pts     - buffer with points
**          npt     - number of points in buffer to insert
**          ind     - index at which to insert
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_insert_uv (pts, npt,ind)
UU_REAL *pts;
UM_int2 *npt;
UM_int2 *ind;
{
	int num, index;

	if (NCL_uvintof_init == 0) 
	{
		uu_list_init (&NCL_uvintof,sizeof(UU_REAL),1200,1200);
		NCL_uvintof_init = 1;
	}
	num = *npt * 3;
	index = 3*(*ind);
	uu_list_insert_multiple(&NCL_uvintof,index,num,pts);

	return(UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : ncl_put_asn (asn, num)
**       Put asn word on list to use by f77 functions to create curve.
**       Data is stored same as it was in ranfile and must be after 
**       geometry or alone.
**       F77 callable function.
**    PARAMETERS
**       INPUT  :
**          num     - number of words in buffer to store
**          asn     - buffer with asn words
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_put_asn (asn,num)
UU_REAL *asn;
int *num;
{
   if (NCL_uvintof_init == 0)
   {
      uu_list_init (&NCL_uvintof,sizeof(UU_REAL),100,100);
      NCL_uvintof_init = 1;
   }
   uu_list_push_multiple(&NCL_uvintof,*num,asn);

	return(UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : ncl_push_asn (asn)
**       Push an asn word onto list to use by f77 functions.
**    PARAMETERS
**       INPUT  :
**          asn     - asn word
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_push_asn (asn)
UU_REAL *asn;
{
   if (NCL_uvintof_init == 0)
   {
      uu_list_init (&NCL_uvintof,sizeof(UU_REAL),100,100);
      NCL_uvintof_init = 1;
   }
   uu_list_push(&NCL_uvintof,asn);

	return(UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : ncl_get_asn (ist,asn,num)
**       Retrieves points, vectors or pointvectors from list to use by
**       f77 functions to create curve from set of points.  Type of
**       data is returned same as it was in ranfile support.
**       F77 callable function.
**    PARAMETERS
**       INPUT  :
**          ist     - zero index
**          ntk     - number of asn words to get
**       OUTPUT :
**          asn     - buffer with asn words
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_get_asn (ist,indx,asn,num)
UU_REAL *asn;
UM_int2 *ist, *num, *indx;
{
	UU_REAL *rnum;
	UM_coord *ptr;
	int ntk, inx;

	if (NCL_uvintof_init == 0) 
		return (UU_FAILURE);
	else
	{
		ptr = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
		rnum = (UU_REAL *) ptr[*ist];
		inx = *indx - 1;
		ntk = *num;
		um_cparray (ntk,&rnum[inx],asn);
	}

	return (UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : ncl_get_asn_ptr (ptr,num)
**       Returns a pointer to the NCL_unintof list data and the number 
**       items in the list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          ptr     - Pointer of NCL_uvintof list data.
**          num     - Number of ASN's in list.
**    RETURNS      :
**       UU_FAILURE if error, else UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_get_asn_ptr (ptr,num)
UU_REAL **ptr;
int *num;
{
	int status;

	if (NCL_uvintof_init == 0) 
	{
		*ptr = UU_NULL;
		*num = 0;
		status = UU_FAILURE;
	}
	else
	{
		*ptr = (UU_REAL *) UU_LIST_ARRAY (&NCL_uvintof);
		*num = UU_LIST_LENGTH (&NCL_uvintof);
		status = UU_SUCCESS;

	}

	return (status);
}

/**********************************************************************
**    E_FUNCTION     : ncl_get_ent (indx,pts)
**       Retrieves points or vectors from list to use by
**       f77 functions to create curve from set of points. 
**    PARAMETERS
**       INPUT  :
**          indx    - index in the list
**          pts     - point
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_get_ent (indx,pts)
UU_REAL *pts;
UM_int2 *indx;
{
	UM_coord *ptr;
	int inx;

	if (NCL_uvintof_init == 0) 
		return (UU_FAILURE);
	else
	{
		ptr = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
		inx = *indx - 1;
		um_vctovc (ptr[inx],pts);
	}

	return (UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : ncl_weed_uv (tol,npt)
**       Weed out points in common list created for interpolation
**       rbspline from set of points. Note: if list is not initialized
**       this will quit with error. F77 callable function.
**    PARAMETERS
**       INPUT  :
**          tol     - tolerance
**       OUTPUT :
**          npt     - number of points in the revised list
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_weed_uv (tol,npt)
UU_REAL *tol;
UM_int2 *npt;
{
	UM_coord *ptr;
	int num,i,j,k,ntk;
	UU_REAL d;

	if (NCL_uvintof_init == 0) 
		return (UU_FAILURE);
	else
	{
		ptr = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
		ntk = num = UU_LIST_LENGTH(&NCL_uvintof)/3;
		for (i=0, j=1; j<num; j++)
		{
			d = um_dcccc (ptr[i+1],ptr[i]);
			i++;
			if (d <= *tol)
			{
				if (j == num-1) i--;
				for (k=i; k<ntk; k++) um_vctovc (ptr[k+1],ptr[k]);
				i--;
				ntk--;
			}
		}
		*npt = ntk;
		NCL_uvintof.cur_cnt = 3*ntk;
		return (UU_SUCCESS);
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_getn_uv (pts, npt)
**       Get number of points in common list created for interpolation
**       rbspline from set of points. Note: if list is not initialized
**       this will quit with error. F77 callable function.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          npt     - number of points in the list
**          pts     - the first point in the list
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_getn_uv (pts,npt)
UU_REAL *pts;
UM_int2 *npt;
{
	UM_coord *ptr;
	int num;

	if (NCL_uvintof_init == 0) 
		return (UU_FAILURE);
	else
	{
		ptr = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
		num = UU_LIST_LENGTH(&NCL_uvintof);
		um_vctovc (ptr,pts);
		*npt = num/3;
	}

	return (UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : void ncl_getpts_uv (npt,pts)
**       Get number of points and the points in the common list.
**       Note: if list is not initialized this will quit with error.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          npt     - number of points in the list
**          pts     - the first point in the list
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_getpts_uv (npt,pts)
UM_coord **pts;
int *npt;
{
	int num;

	if (NCL_uvintof_init == 0)
	{
		*npt = 0;
		*pts = UU_NULL;
	}
	else
	{
		*pts = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
		num = UU_LIST_LENGTH(&NCL_uvintof);
		*npt = num/3;
	}
}

/**********************************************************************
**    E_FUNCTION     : ncl_revers_uv (pts, npt)
**       Revers order of points in common list created for interpolation
**       rbspline from set of points. Note: if list is not initialized
**       this will quit with error. F77 callable function.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          npt     - number of points in the list
**          pts     - the first point in the list
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_revers_uv (pts,npt)
UU_REAL *pts;
UM_int2 *npt;
{
	UM_coord *ptr;
	int num;

	if (NCL_uvintof_init == 0) 
		return (UU_FAILURE);
	else
	{
		ptr = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
		num = UU_LIST_LENGTH(&NCL_uvintof)/3;
		ncl_revers1_list (num,0,ptr,1);
		um_vctovc (ptr,pts);
		*npt = num;
	}

	return (UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : ncl_free_uv()
**       Free list of points used for creation of UV curve on surface.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_free_uv ()
{
	if (NCL_uvintof_init == 1)
	{
		uu_list_free (&NCL_uvintof);
		NCL_uvintof_init = 0;
	}
	return (0);
}

/**********************************************************************
**    E_FUNCTION     : ncl_nul_uv()
**       Reset the list counter.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
void ncl_nul_uv()
{
	if (NCL_uvintof_init == 1) NCL_uvintof.cur_cnt = 0;
	return;
}

/**********************************************************************
**    E_FUNCTION     : ncl_replace_uv (indx,pts)
**       Replaces the indx'th coordinate in NCL_uvintof list with pts
**    PARAMETERS
**       INPUT  :
**          indx     - index at which to replace
**          pts      - coordinate 
**       OUTPUT :
**          changed NCL_uvintof
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int 
ncl_replace_uv (indx,pts)
UM_int2 *indx;
UU_REAL *pts;
{
	UM_coord *ptr;
	int inx;

	if (NCL_uvintof_init == 0) 
		return (UU_FAILURE);
	else
	{
		ptr = (UM_coord *) UU_LIST_ARRAY (&NCL_uvintof);
		inx = *indx - 1;
		um_vctovc (pts,ptr[inx]);
	}

	return (UU_SUCCESS);
}
