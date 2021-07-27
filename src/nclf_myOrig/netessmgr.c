/*********************************************************************
**    NAME         : netessmgr.c
**       Funtions to save base surface tessellation and surface boundary.
**
**			 nclc_tessmgr_init()
**			 nclc_tessmgr_create()
**			 nclc_tessmgr_free()
**			 nclc_tessmgr_check_init()
**			 nclc_tessmgr_check_sfkey()
**			 nclc_tessmgr_get_nbsf()
**			 nclc_tessmgr_get_tess_trianlst()
**			 nclc_tessmgr_get_tess_trian()
**			 nclc_tessmgr_get_srf_bndr()
**			 nclc_tessmgr_get_srf_polylst()
**           nclc_tessmgr_tessellate()
**			 nclc_tessmgr_srf_bndr()
**
**    COPYRIGHT 2009 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       netessmgr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:53
*********************************************************************/
#include "nccs.h"
#include "mgeom.h"
#include "uminmax.h"
#include "nclwaterln.h"

typedef struct
{
	int nsf;	    		/*number of surfaces saved*/
	UU_LIST sfkey;			/*surface key*/
	UU_LIST sftess;			/*base surface tess structure*/
	UU_LIST sfbndr;			/*boundary tolerance surface boundary data*/
	UU_LIST sfbndr0;		/*surface tolerance surface boundary data*/
} Ssftess_struc;

static Ssftess_struc Ssftess;

typedef struct
{
	UU_LIST *trianlist;			/*base surface triangle point list*/
	UU_LIST *uvtrilist;			/*base surface triangle uv-point list*/
	UU_LIST *polylist; 	    	/*trimmed surface triangle point list*/
} NCL_bsf_tess;

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_init()
**       Allocate memory and initialize lists used for 
**       saving sfkey, base surface tessellation and boundary.
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
void nclc_tessmgr_init()
{
	Ssftess.nsf= 0;
	uu_list_init(&Ssftess.sfkey, sizeof(UU_LIST), 200, 200);
	uu_list_init(&Ssftess.sftess, sizeof(UU_LIST), 200, 200);
	uu_list_init(&Ssftess.sfbndr, sizeof(UU_LIST), 200, 200);
	uu_list_init(&Ssftess.sfbndr0, sizeof(UU_LIST), 200, 200);
}

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_init()
**       Allocate memory and initialize lists used for 
**       creation of new sfkey, base surface tessellation and boundary.
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
void nclc_tessmgr_create()
{
	UU_LIST keylst;
	UU_LIST tesslst;
	UU_LIST bndrlst,bndrlst0;
	Ssftess.nsf++;

	uu_list_init(&keylst, sizeof(UU_KEY_ID), 1, 2);
	uu_list_push(&Ssftess.sfkey, &keylst);
	uu_list_init(&tesslst, sizeof(NCL_bsf_tess), 100, 200);
	uu_list_push(&Ssftess.sftess, &tesslst);
	uu_list_init(&bndrlst, sizeof(UM_srf_boundary), 10, 20);
	uu_list_push(&Ssftess.sfbndr, &bndrlst);
	uu_list_init(&bndrlst0, sizeof(UM_srf_boundary), 10, 20);
	uu_list_push(&Ssftess.sfbndr0, &bndrlst0);
}

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_free()
**       Free allocated memory used for saving and creation
**       of sfkey, base surface tessellation and boundary.
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
void nclc_tessmgr_free()
{
	int isf;
	UU_LIST *keylst;
	UU_LIST	*tesslst;
	UU_LIST	*bndrlst,*bndrlst0;
	NCL_bsf_tess *bsftess;
	UM_srf_boundary *sfbndr,*sfbndr0;
	UU_LIST *polylst;
	UM_srf_bound *poly;

	keylst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfkey);
	tesslst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sftess);
	bndrlst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfbndr);
	bndrlst0 = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfbndr0);

	for (isf = 0; isf < Ssftess.nsf; isf++)
	{
		uu_list_free (&keylst[isf]);
		bsftess = ( NCL_bsf_tess *) UU_LIST_ARRAY (&tesslst[isf]);
		UU_LIST_FREE (bsftess->trianlist);
		UU_LIST_FREE (bsftess->uvtrilist);
		if (UU_LIST_LENGTH(&bndrlst[isf])> 0)
		{
			sfbndr = ( UM_srf_boundary *) UU_LIST_ARRAY (&bndrlst[isf]);
			um_free_boundary(sfbndr);
		}
		if (UU_LIST_LENGTH(&bndrlst0[isf])> 0)
		{
			sfbndr0 = ( UM_srf_boundary *) UU_LIST_ARRAY (&bndrlst0[isf]);
			um_free_boundary(sfbndr0);
		}
	}

	Ssftess.nsf =0;
	uu_list_free (&Ssftess.sfkey);
	uu_list_free (&Ssftess.sftess);
	uu_list_free (&Ssftess.sfbndr);
	uu_list_free (&Ssftess.sfbndr0);
}

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_check_init()
**    Check if the Ssftess has benn initilized
**    PARAMETERS
**       INPUT  :
**			none
**       OUTPUT :
**			none
**    RETURNS      :
**       UU_SUCCESS if initilize, UU_FAILURE if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int nclc_tessmgr_check_init()
{
	if (Ssftess.nsf == 0)
		return UU_FAILURE;
	else
		return UU_SUCCESS;
}

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_check_sfkey(sfkey)
**    Check if the given surface key exist inside Ssftess structure
**    PARAMETERS
**       INPUT  :
**			skfey		- surface key
**       OUTPUT :
**			none
**    RETURNS      :
**       UU_SUCCESS if sfkey found, UU_FAILURE if sfkey not found
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int nclc_tessmgr_check_sfkey(sfkey)
UU_KEY_ID sfkey;
{
	int i,status;
	UU_KEY_ID *keyids;
	UU_LIST *keylst;

	status = UU_FAILURE;
	if (Ssftess.nsf == 0)
		return status;

/*
.....Check if the surface already tessellated or not
*/
	keylst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfkey);
	for (i = 0; i < Ssftess.nsf; i++)
	{
		keyids = (UU_KEY_ID*) UU_LIST_ARRAY(&keylst[i]);
		if (sfkey == keyids[0])
			return UU_SUCCESS;
	}

	return status;
}

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_get_nbsf()
**		Return the number of base surface data saved
**    PARAMETERS
**       INPUT  :
**			none
**       OUTPUT :
**			none
**    RETURNS      :
**      The number of base surface data saved
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int nclc_tessmgr_get_nbsf()
{
	return Ssftess.nsf;
}

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_get_tess_trianlst(sfkey,ptri,uvtri)
**       Get the surface base tessellation trangles and uv point from Ssftess
**		if the given surface key found match, otherwise create the base surface
**		tessellation and output the tessellation trangles and uv point 
**    PARAMETERS
**       INPUT  :
**			skfey		- surface key
**       OUTPUT :
**          ptri		- pointer to the point triangles list 
**          uvtri		- pointer to the uv-points triangles list
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int nclc_tessmgr_get_tess_trianlst(sfkey,trianlst,uvtrilst)
UU_KEY_ID sfkey;
UU_LIST **trianlst;
UU_LIST **uvtrilst;
{
	int i,status;
	UU_KEY_ID *keyids;
	UU_LIST *keylst;
	UU_LIST	*tesslst;
	NCL_bsf_tess *bsftess;

	status = UU_SUCCESS;

/*
.....Check if there are surface tessellation saved
*/
	if (Ssftess.nsf == 0)
	{
/*
.....Initialize list
*/
		Ssftess.nsf = 0;
		uu_list_init(&Ssftess.sfkey, sizeof(UU_LIST), 100, 100);
		uu_list_init(&Ssftess.sftess, sizeof(UU_LIST), 100, 100);
	}

/*
.....Check if the surface already tessellated or not
*/
	keylst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfkey);
	tesslst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sftess);
	for (i = 0; i < Ssftess.nsf; i++)
	{
		keyids = (UU_KEY_ID*) UU_LIST_ARRAY(&keylst[i]);
		if (sfkey == keyids[0])
		{
			bsftess = ( NCL_bsf_tess *) UU_LIST_ARRAY (&tesslst[i]);
			goto Done;
		}
	}
/*
.....Create tessellation, and save the tessellation to Ssftess
*/

Done:
	if (!bsftess || !bsftess->trianlist || !bsftess->uvtrilist)
		return UU_FAILURE;

	if(trianlst && bsftess->trianlist)
		*trianlst = bsftess->trianlist;
	if (uvtrilst && bsftess->uvtrilist)
		*uvtrilst = bsftess->uvtrilist;

	return status;
}

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_get_tess_trian(sfkey,ptri,uvtri)
**       Get the surface base tessellation trangles and uv point from Ssftess
**		if the given surface key found match, otherwise create the base surface
**		tessellation and output the tessellation trangles and uv point 
**    PARAMETERS
**       INPUT  :
**			skfey		- surface key
**       OUTPUT :
**          ptri		- pointer to the array of point triangles 
**          uvtri		- pointer to the array of uv-points triangles
**    RETURNS      :
**      The number of triangles in the base surface tessellation 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int nclc_tessmgr_get_tess_trian(sfkey,ptri,uvtri)
UU_KEY_ID sfkey;
UM_trian **ptri,**uvtri;
{
	int i,ntri,status;		
	UU_LIST *trianlst,*uvtrilst;

	ntri = 0;
	status =UU_FAILURE;

	status = nclc_tessmgr_get_tess_trianlst(sfkey,&trianlst,&uvtrilst);

	if (status == UU_SUCCESS)
	{
		*ptri = (UM_trian *)UU_LIST_ARRAY(trianlst);
		*uvtri = (UM_trian *)UU_LIST_ARRAY(uvtrilst);
	 	ntri = UU_LIST_LENGTH (trianlst);
	}

	return ntri;
}

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_get_srf_bndr(sfkey,bndr)
**       Get the surface boundary structure data
**    PARAMETERS
**       INPUT  :
**			skfey	- surface key
**       OUTPUT :
**          bndr	- surface boundary
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int nclc_tessmgr_get_srf_bndr(sfkey,bndr,bndr0)
UU_KEY_ID sfkey;
UM_srf_boundary **bndr;
UM_srf_boundary **bndr0;
{
	int i,status;
	UU_KEY_ID *keyids;
	UU_LIST *keylst;
	UU_LIST	*bndrlst;
	UU_LIST	*bndrlst0;
	UM_srf_boundary *sfbndr;

	status = UU_SUCCESS;

/*
.....Check if there are surface boundary saved
*/
	if (Ssftess.nsf == 0)
	{
/*
.....Initialize list
*/
		Ssftess.nsf = 0;
		uu_list_init(&Ssftess.sfkey, sizeof(UU_LIST), 100, 100);
		uu_list_init(&Ssftess.sfbndr, sizeof(UU_LIST), 100, 100);
		uu_list_init(&Ssftess.sfbndr0, sizeof(UU_LIST), 100, 100);
	}

/*
.....Check if this surface already have boundary or not
*/
	keylst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfkey);
	bndrlst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfbndr);
	bndrlst0 = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfbndr0);

	for (i = 0; i < Ssftess.nsf; i++)
	{
		keyids = (UU_KEY_ID*) UU_LIST_ARRAY(&keylst[i]);
		if (sfkey == keyids[0])
			goto Done;
	}

/*
.....Create srf boundary, and save the boundary to Ssftess
*/

Done:
	*bndr = ( UM_srf_boundary *) UU_LIST_ARRAY (&bndrlst[i]);
	*bndr0 = ( UM_srf_boundary *) UU_LIST_ARRAY (&bndrlst0[i]);

	return status;
}

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_get_srf_poly(sfkey,polylst)
**      Get the surface polygon polygon data from Ssftess
**		if the given surface key found match.
**    PARAMETERS
**       INPUT  :
**			skfey		- surface key
**       OUTPUT :
**          polylst		- pointer to the surface polygon list 
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int nclc_tessmgr_get_srf_polylst(sfkey,polylst)
UU_KEY_ID sfkey;
UU_LIST **polylst;
{
	int i,status;
	UU_KEY_ID *keyids;
	UU_LIST *keylst;
	UU_LIST	*tesslst;
	NCL_bsf_tess *bsftess;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	bsftess = UU_NULL;
/*
.....Check if there are surface tessellation saved
*/
	if (Ssftess.nsf == 0)
	{
/*
.....Initialize list
*/
		Ssftess.nsf = 0;
		uu_list_init(&Ssftess.sfkey, sizeof(UU_LIST), 100, 100);
		uu_list_init(&Ssftess.sftess, sizeof(UU_LIST), 100, 100);
	}

/*
.....Check if the this surface already tessellated or not
*/
	keylst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfkey);
	tesslst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sftess);
	for (i = 0; i < Ssftess.nsf; i++)
	{
		keyids = (UU_KEY_ID*) UU_LIST_ARRAY(&keylst[i]);
		if (sfkey == keyids[0])
		{
			bsftess = ( NCL_bsf_tess *) UU_LIST_ARRAY (&tesslst[i]);
			goto Done;
		}
	}
/*
.....Create tessellation, and save the tessellation to Ssftess
*/

Done:	
	if (!bsftess || !bsftess->polylist)
		return UU_FAILURE;

	if(polylst && bsftess->polylist)
		*polylst = bsftess->polylist;

	return status;
}

/**********************************************************************
**    E_FUNCTION     : nclc_tessmgr_get_srf_poly(sfkey,polylst)
**      Store the surface polygon polygon data to Ssftess
**    PARAMETERS
**       INPUT  :
**			skfey		- surface key
**          polylst		- pointer to the surface polygon list
**       OUTPUT :
**			none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int nclc_tessmgr_set_srf_polylst(sfkey,polylst)
UU_KEY_ID sfkey;
UU_LIST *polylst;
{
	int i,status,npts;
	UU_KEY_ID *keyids;
	UU_LIST *keylst;
	UU_LIST	*tesslst;
	NCL_bsf_tess *bsftess;

	status = UU_FAILURE;

/*
.....Check if there are surface tessellation saved
*/
	if (Ssftess.nsf == 0)
	{
/*
.....Initialize list
*/
		Ssftess.nsf = 0;
		uu_list_init(&Ssftess.sfkey, sizeof(UU_LIST), 100, 100);
		uu_list_init(&Ssftess.sftess, sizeof(UU_LIST), 100, 100);
	}

/*
.....Check if the this surface already tessellated or not
*/
	keylst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfkey);
	tesslst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sftess);
	for (i = 0; i < Ssftess.nsf; i++)
	{
		keyids = (UU_KEY_ID*) UU_LIST_ARRAY(&keylst[i]);
		if (sfkey == keyids[0])
		{
			bsftess = ( NCL_bsf_tess *) UU_LIST_ARRAY (&tesslst[i]);
			status = UU_SUCCESS;
			goto Done;
		}
	}

Done:	
/*
.....Store tessellation polygon data
*/
	if (status == UU_SUCCESS && polylst)
	{
		npts = polylst->cur_cnt;
		bsftess->polylist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		uu_list_init (bsftess->polylist,sizeof(UM_trian),npts,npts);
		uu_list_push_list (bsftess->polylist, polylst);
	}
	

	return status;
}

/*********************************************************************
**    E_FUNCTION : nclc_tessmgr_tessellate(isrf,srf,tfmat,bound,tol,tesstyp)
**       Tessellate base surface and save the NCL_bsf_tess data to Ssftess
**    PARAMETERS
**       INPUT  :
**			isrf		- surface key
**			srf			- pointer to the surface
**			tfmax		- surface matrix
**			bound		- surface boundary
**			tol			- tolerance
**			tesstyp		- tessellation type
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int nclc_tessmgr_tessellate(isrf,srf,tfmat,rot,irot,bound,tol,tesstyp)
struct NCL_fixed_databag *srf;
UM_transf tfmat,rot;
UM_srf_boundary *bound;
UU_REAL tol;
int isrf,irot;
UM_tess_settype tesstyp;
{
	int i,npts,status;
	struct NCL_fixed_databag sf;
	UM_tessellation tess;
	UM_trian *ptri;
	UU_LIST uvtes,trilst,uvtrilst,polylst;
	UU_LIST *tesslst;
	UU_LIST *keyids;
	UM_srf_boundary *bndr;
	UU_LIST *bndrlst;
	UU_REAL dtol;

	NCL_bsf_tess *bsf;
	bsf = (NCL_bsf_tess *) uu_malloc (sizeof (NCL_bsf_tess));
	bsf->trianlist = UU_NULL;
	bsf->uvtrilist = UU_NULL;
	bsf->polylist = UU_NULL;

	status = UU_SUCCESS;

	uu_list_init (&uvtes, sizeof(UM_coord), 100, 200);
	uu_list_init0 (&trilst);
	uu_list_init0 (&uvtrilst);
	uu_list_init0 (&polylst);
	um_init_tess(&tess);

/*
.....Calculate tessellation for base surface
*/
	if (srf->key == 19305)
	{
		i = 0;
	}
	status = ncl_tess_surfbase(srf,tfmat,bound,&tess,
					tol,tesstyp,0,0,0,0,&uvtes);

	if (status == UU_SUCCESS)
	{
		if (!trilst.data)
		{
			npts = tess.ntri;
			i = (npts > 200)? npts: 200;
			uu_list_init (&trilst,sizeof (UM_trian),i,i);
		}
		else
			UU_LIST_EMPTY (&trilst);
/*
.....Get the trianlist
*/
		status = ncl_get_tess_triangles (&tess,&trilst,2,0);
		npts = trilst.cur_cnt;

		if (status == UU_SUCCESS)
		{
			bsf->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
			uu_list_init (bsf->trianlist,sizeof (UM_trian),npts,npts);
			uu_list_push_list (bsf->trianlist, &trilst);
		}
/*
.....Get the uvtrilist
*/
		if (!uvtrilst.data)
		{
			i = (npts > 200)? npts: 200;
			uu_list_init (&uvtrilst,sizeof (UM_trian),i,i);
		}
		else
			UU_LIST_EMPTY (&uvtrilst);
		status = ncl_get_tess_uvtes (&tess,&uvtes,&uvtrilst);

		if (status == UU_SUCCESS)
		{
			bsf->uvtrilist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
			uu_list_init (bsf->uvtrilist,sizeof (UM_trian),npts,npts);
			uu_list_push_list (bsf->uvtrilist, &uvtrilst);
		}
/*
.....Store tessellation ploygon for watrev
*/
		if (ncl_is_watrev() && !polylst.data)
		{
			npts = tess.ntri;
			i = (npts > 200)? npts: 200;
			uu_list_init (&polylst,sizeof(UM_trian),i,i);
		}
		else
			UU_LIST_EMPTY (&polylst);

		if (ncl_is_watrev())
		{	
/*
.....Initialize surface boundary
*/	
			bndr = (UM_srf_boundary *) uu_malloc (sizeof(UM_srf_boundary));
			um_init_boundary (bndr);

			bndr->uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
			bndr->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
			uu_list_init (bndr->uvpts, sizeof(UM_coord), 200, 200);
			uu_list_init (bndr->cvpts, sizeof(UM_coord), 200, 200);

/*
.....Get the trimmed surface boundary with 0.0005 tolerance
*/
			dtol = 0.0005;
        	status = ncl_get_bndry (srf,tfmat,bndr,dtol,UU_FALSE);
/*
.....Store tight boundary data
*/
			if (status == UU_SUCCESS)
			{
				bndrlst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfbndr);
				uu_list_push(&bndrlst[isrf], bndr);
			}		
/*
.....Get tessellation polygon data
*/
			status = ncl_trim_tess_boundpoly(srf,tfmat,rot,irot,tol,
										bndr,&trilst,&uvtrilst,&polylst);
		    npts = polylst.cur_cnt;

/*
.....Store tessellation polygon data
*/
			if (status == UU_SUCCESS)
			{
				bsf->polylist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
				uu_list_init (bsf->polylist,sizeof(UM_trian),npts,npts);
				uu_list_push_list (bsf->polylist, &polylst);
			}
		}
	}

/*
.....Put base surface tessellation into lists
*/	
	if (bsf)
	{
/*
.....Surface key
*/
		keyids = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfkey);
		uu_list_push(&keyids[isrf], &srf->key);

/*
.....Base surface tessellation
*/
		tesslst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sftess);
		uu_list_push(&tesslst[isrf], bsf);
	}

	uu_list_free (&uvtes);	
	if (trilst.data) uu_list_free (&trilst);
	if (uvtrilst.data) uu_list_free (&uvtrilst);
	if (polylst.data) uu_list_free (&polylst);
	um_clean_tess (&tess);

	return status;
}

/**********************************************************************
**    E_FUNCTION : nclc_tessmgr_srf_bndr(isrf,srf,tfmat,toler, tol)
**       Calculate trimmed surface boundary and save the boundary to Ssftess
**    PARAMETERS
**       INPUT  :
**			isrf		- surface key
**			srf			- pointer to the surface
**			tfmax		- surface matrix
**			toler		- surface tolerance
**			tol			- boundary tolerance
**       OUTPUT :
**			none
**    RETURNS      :
**       UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int nclc_tessmgr_srf_bndr(isrf,srf,tfmat,toler,tol)
struct NCL_fixed_databag *srf;
UM_transf tfmat;
UU_REAL toler,tol;
int isrf;
{
	int status,n;
	UM_srf_boundary *bndr,*bndr0;
	UU_LIST *bndrlst,*bndrlst0;

/*
.....Initialize surface boundary
*/	
	bndr = (UM_srf_boundary *) uu_malloc (sizeof(UM_srf_boundary));
	um_init_boundary (bndr);
	bndr->uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	bndr->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (bndr->uvpts, sizeof(UM_coord), 200, 200);
	uu_list_init (bndr->cvpts, sizeof(UM_coord), 200, 200);

	bndr0 = (UM_srf_boundary *) uu_malloc (sizeof(UM_srf_boundary));
	um_init_boundary (bndr0);
	bndr0->uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	bndr0->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
	uu_list_init (bndr0->uvpts, sizeof(UM_coord), 200, 200);
	uu_list_init (bndr0->cvpts, sizeof(UM_coord), 200, 200);

	status = UU_SUCCESS;

/*
.....Get the trimmed surface boundary
*/
	status = ncl_get_bndry (srf,tfmat,bndr,tol,UU_FALSE);

	if (bndr->nb < 1)
		status = UU_FAILURE;
	
	if (status == UU_SUCCESS)
	{
		bndrlst = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfbndr);
		uu_list_push(&bndrlst[isrf], bndr);
	}		

	status = ncl_get_bndry (srf,tfmat,bndr0,toler,UU_TRUE);
	if (bndr0->nb < 1) 
		status = UU_FAILURE;

	if (status == UU_SUCCESS)
	{
		bndrlst0 = (UU_LIST *) UU_LIST_ARRAY (&Ssftess.sfbndr0);
		uu_list_push(&bndrlst0[isrf], bndr0);
	}

	return status;
}
