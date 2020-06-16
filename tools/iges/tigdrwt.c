/*********************************************************************
**    NAME         :  tigdrwt.c
**       CONTAINS:
**    		uig_transform_point(eptr,tranfmat)
**          uig_transform_array(array,tranfmat)
**    		uig_transform_line(eptr,tranfmat)
**    		uig_transform_circle(eptr,tranfmat)
**    		uig_transform_conic(eptr, basepoint, trform)
**    		uig_transform_rbspline(eptr,tranfmat)
**    		uig_transform_agcrv(eptr, tfmat)
**    		uig_transform_polyline(eptr, tfmat)
**    		uig_transform_polygon(eptr, trform)
**    		uig_transform_text(eptr,tranfmat)
**    		uig_transform_drafting (entity, tf)
**    		uig_transform_agsrf(eptr, tfmat)
**    		uig_transform_xh_rec(key, tf)
**    		uig_transform_xhatch (entity, tf)
**    		ua_retrieve_xhatch(e, size)
**    		ua_get_text(eptr, databagsize)
**          uig_transform_rbsrf(eptr,tranfmat)
**          uig_transform_revsf(eptr,tranfmat)
**          uig_transform_compcrv(eptr,tranfmat)
**          uig_transform(eptr,tranfmat,store)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigdrwt.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/17/15 , 17:58:16
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdrel.h"
#include "mdclass.h"
#include "mdeval.h"
#include "mcrv.h"
#include "msrf.h"
#include "umoveb.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mdgenent.h"
#include "mattr.h"
#include "msol.h"
#include "mdebug.h"
#include "r1emsgpk.h"
#include "adrf.h"
#include "adraft.h"
#include "atext.h"
#include "nccs.h"

#include "ag_incl.h"
#include "ag_global.h"
extern UU_REAL drw_scale, drw_org[3], drw_t[4][3];

/*********************************************************************
**    E_FUNCTION     : int uig_transform_point(eptr,tranfmat)
**      Transform a point by the given 4X3 matrix
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be transformed
**          tranfmat     the 4x3 transformation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_point(eptr,tranfmat)
	struct UM_point_rec *eptr;
   UM_transf    tranfmat;

	{                                     
   UM_coord    temppt;           /* Temporary point */

	uu_denter(UU_MTRC,(us,"um_tf1_tranfpt(key:%d,tfmat:%x)",
						eptr->key,tranfmat));
    
   um_cctmtf(eptr->pt, tranfmat, eptr->pt);

	uu_dexit;
	return (UU_SUCCESS);
	}

/* cpp: add a new function to transforn an array of doubles */
/*********************************************************************
**    E_FUNCTION     : int uig_transform_array(array,tranfmat)
**      Transform a array of doubles by the given 4X3 matrix
**    PARAMETERS   
**       INPUT  : 
**				array    array of doubles
**          tranfmat     the 4x3 transformation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_array(array,tranfmat)
UU_REAL     array[3];
UM_transf    tranfmat;
	{                                     
	UM_coord    temppt;           /* Temporary point */

	uu_denter(UU_MTRC,(us,"um_tf1_tranfpt(tfmat:%x)",
						tranfmat));

	um_cctmtf(array, tranfmat, array);

	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int uig_transform_line(eptr,tranfmat)
**			Transform a line by applying the specified 4X3 transformation
**    PARAMETERS   
**       INPUT  : 
**				eptr          pointer to the entity to be transformed
**          tranfmat      the 4x3 transformation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_line(eptr,tranfmat)
	struct UM_line_rec *eptr;
	UM_transf    tranfmat;

	{                                     

	uu_denter(UU_MTRC,(us,"um_tf2_tranfline(key:%d,tfmat:%x)",
					eptr->key, tranfmat));
    
   um_cctmtf(eptr->spt, tranfmat, eptr->spt);          
   um_cctmtf(eptr->ept, tranfmat, eptr->ept);          

	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int uig_transform_circle(eptr,tranfmat)
**			Transform a circle by the given 4X3 transformation 
**    PARAMETERS   
**       INPUT  : 
**			 eptr        	pointer to the entity to be transformed
**        tranfmat    	the 4x3 transformation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_circle(eptr,tranfmat)
	struct UM_circle_rec *eptr;
	UM_transf tranfmat;

	{                                     

	uu_denter(UU_MTRC,(us,"um_tf3_tranfcirc(key:%d,tfmat:%x)",
		eptr->key, tranfmat));
    
   um_cctmtf(eptr->center, tranfmat, eptr->center);
   um_vctmtf(eptr->nvec, tranfmat, eptr->nvec);
	um_unitvc(eptr->nvec,eptr->nvec);
   um_vctmtf(eptr->svec, tranfmat, eptr->svec);
	eptr->radius = eptr->radius * um_mag(eptr->svec);
	um_unitvc(eptr->svec,eptr->svec);

	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int uig_transform_conic(eptr,  trform)
**      Transform the specified conic by the given 4x3 matrix, 
**			conjugated by translation to basepoint.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be scaled
**          trform      the 4x3 scaling matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_conic(eptr, trform)

	struct UM_conic_rec *eptr;
   UM_transf    trform;
	{                                     
	int i;

	uu_denter(UU_MTRC,(us,"uio_transform_conic(key=%x)",
	eptr->key));

	/*
	uu_dprint(UU_MTRC,(us,"  transformation:"));
	for(i=0;i<4;i++)
		{
		uu_dprint(UU_MTRC,(us,"   %d  %g %g %g", i, trform[i][0], trform[i][1],
		trform[i][2]));
		}
	uu_dprint(UU_MTRC,(us,"  conic_transformation:"));
	for(i=0;i<4;i++)
		{
		uu_dprint(UU_MTRC,(us,"   %d  %g %g %g", i, eptr->tfmat[i][0], eptr->tfmat[i][1],
		eptr->tfmat[i][2]));
		}
	*/

   um_tftmtf(eptr->tfmat, trform, eptr->tfmat);

	/*
	uu_dprint(UU_MTRC,(us,"  conic_transformation_after:"));
	for(i=0;i<4;i++)
		{
		uu_dprint(UU_MTRC,(us,"   %d  %g %g %g", i, eptr->tfmat[i][0], eptr->tfmat[i][1],
		eptr->tfmat[i][2]));
		}
	*/

	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION		: uig_transform_rbspline(eptr,tranfmat,store)
**			Transform a rational bspline curve with a 
**			transformation matrix
**			PARAMETERS   
**				INPUT: 
**					eptr         pointer to a rational bspline entity to transform.
**					tranfmat     transformation matrix.
**					store        UU_TRUE = store updated curve in Unibase.
**				OUTPUT :  none.  
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
uig_transform_rbspline(eptr,tranfmat,store)
	struct UM_rbsplcrv_rec *eptr;
	UM_transf tranfmat;
	UU_LOGICAL store;
	{
	int i,j;						/*loop counters						  */

	uu_denter(UU_MTRC,(us,"uig_transform_rbspline(key:%d,tranfmat:%x)",
					eptr->key, tranfmat));

	for (i=0,j=0; i<eptr->no_pt; i++,j=j+3)	
		{
		um_cctmtf(&eptr->pt[j], tranfmat, &eptr->pt[j]);
		}
	if (store)
		ur_update_data_varlist(eptr->key, 2, eptr->pt, 1, eptr->no_pt);
	uu_dexit;
	return(UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION: int uig_transform_agcrv(eptr, tfmat)
**      Tranform entity (EPTR) by the specified transform (TFMAT)
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
**        tfmat	   	4 x 3 transformation matrix to transform entity by.
**       OUTPUT :  
**          eptr			pointer to the transformed entity record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_agcrv(eptr, tfmat)
   struct UM_agcrv_rec *eptr;
   UM_transf    tfmat;

	{
	int status = UU_SUCCESS;

	/* cpp: modify calls to use uig_transform_array instead of
	   uig_transform_point */

	int uig_transform_array();

	uu_denter(UU_MTRC, (us,"uig_transform_agcrv(key=%d,tfmat=%x)",
				eptr->key, tfmat));

	ag_tr_crv(eptr->crvaddr, uig_transform_array, tfmat, uig_transform_array);
	
	uu_dexitstatus("uig_transform_agcrv",status);
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : int uig_transform_polyline(eptr, tfmat)
**      Transform the specified polyline by the given 4x3 matrix. 
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be scaled
**          tfmat	      the 4x3 scaling matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_polyline(eptr, tfmat)
	struct UM_polyline_rec *eptr;
   UM_transf    tfmat;

	{                                     
	int	nvx;
	int	i,j;

	uu_denter(UU_MTRC,(us,"uig_transform_polyline(?,?,?)"));

	nvx = eptr->no_pt;
	for (i=0, j=0; i<nvx; i++, j=j+3)
		{
		um_cctmtf(&eptr->pt[j], tfmat, &eptr->pt[j]);
		}

	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int uig_transform_patern (eptr, tfmat)
**      Transform the specified patern by the given 4x3 matrix. 
**    PARAMETERS   
**       INPUT  : 
**          eptr          pointer to the entity to be scaled
**          tfmat         the 4x3 scaling matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_patern (eptr, tfmat)
struct NCL_patern_rec  *eptr;
UM_transf    tfmat;

{                                     
	int   nvx;
	int   i,itype;
	UU_REAL *p8;

	itype = eptr->pntype;
/*
.....vp 14-mar-97 set number of points/pv's according to 
.....patern type (no_patptn is not a number of points!)
	nvx   = eptr->no_patpnt;
*/
	nvx   = eptr->no_patpnt / 3;
	if (itype == 2) nvx = nvx / 2; 
	p8    = eptr->patpnt;
	for (i=0; i<nvx; i++)
	{
		um_cctmtf(p8, tfmat, p8);
		p8 += 3;
		if (itype == 2)
		{
			um_vctmtf(p8, tfmat, p8);
			p8 += 3;
		}
	}

	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int uig_transform_nclpv (eptr, tfmat)
**      Transform the specified nclpv by the given 4x3 matrix. 
**    PARAMETERS   
**       INPUT  : 
**          eptr          pointer to the entity to be scaled
**          tfmat         the 4x3 scaling matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_nclpv (eptr, tfmat)
   struct NCL_nclpv_rec  *eptr;
   UM_transf    tfmat;

   {                                     

   um_cctmtf(eptr->pt, tfmat, eptr->pt);
   um_vctmtf(eptr->ve, tfmat, eptr->ve);

   return (UU_SUCCESS);
   }
/*********************************************************************
**    E_FUNCTION     : int uig_transform_polygon(eptr, trform)
**      Transform the specified polygon by the given 4x3 matrix, 
**			conjugated by translation to basepoint.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be scaled
**          trform      the 4x3 scaling matrix
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_polygon(eptr, trform)

	struct UM_poly_rec *eptr;
   UM_transf    trform;
	{                                     
	int	nvx;
	int	i;

	uu_denter(UU_MTRC,(us,"uig_transform_polygon(key=%x)",
		eptr->key));

	nvx = eptr->numvtx;
	for ( i = 0; i < nvx; i++)
		{
		um_cctmtf(eptr->vertex[i], trform, eptr->vertex[i]);
		}

	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int uig_transform_text(eptr,tranfmat)
**      Transform the specified text by the given 4x3  matrix.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be transformed
**          tranfmat     the 4x3 transformation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_text(eptr,tranfmat)
struct     UA_txt_rec *eptr;
UU_REAL    tranfmat[4][3];

{                                     
	struct    UA_txtattr_rec ea;
	UM_vector xaxis, yaxis, zaxis;
	UU_REAL sx, sy;
	int	status = UU_SUCCESS;

	uu_denter( UU_MTRC,(us,"uig_transform_text(key=%d,tfmat=%x)",
		eptr->key,tranfmat));

	ea.key = eptr->key;
	ur_retrieve_attr(&ea);

	um_unitvc(ea.up, yaxis);
	um_unitvc(ea.plane, zaxis);
	um_cross(yaxis, zaxis, xaxis);

	um_vctmtf(ea.up, tranfmat, ea.up);
	um_vctmtf(ea.plane, tranfmat, ea.plane);
	um_cctmtf(eptr->position, tranfmat, eptr->position);

	um_vctmtf(xaxis, tranfmat, xaxis);
	sy = um_mag(ea.up);
	uu_dprint( UU_MTRC,(us,"sy=%g, height=%g", sy, ea.height));
	ea.height = ea.height * sy;
	uu_dprint( UU_MTRC,(us,"new height=%g", ea.height));
	ea.expn = um_mag(xaxis) / sy;

	um_unitvc(ea.up, ea.up);
	um_unitvc(ea.plane, ea.plane);

	uu_dexit;
	return (status);
}	

/*********************************************************************
**    E_FUNCTION :  uig_transform_drafting (entity, tf)
**       Transform a drafting entity
**    PARAMETERS   
**       INPUT  : 
**          entity			entity record
**          tf					transformation matrix
**       OUTPUT :  
**          entity			modified entity record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_transform_drafting(e, tf)
struct UA_generic_draft	*e;
UU_REAL						tf[4][3];

{
	int status, i, j;
	UU_KEY_ID key;
	UU_REAL scal,pt1[3],vtx[3],vty[3],vtz[3],pt3[3],zerov[3];
	struct   UA_txt_rec			note;			/* text record */

	uu_denter(UU_MTRC,(us,"uig_trans_draft(%x)",e));

	/*
	for(i=0;i<4;i++)
		uu_dprint(UU_MTRC,(us,"tf(%g %g %g )",tf[i][0], tf[i][1], tf[i][2]));
	*/

	/* check transformation */

	if(tf == UM_DEFAULT_TF) goto fexit;

	switch (e->rel_num)
	{
		case UA_HATCHING_REL:
			uu_denter2(UU_STRC,(us,"uc_transform called for xhatch"));
			uu_dexit;	
			break;	/* do nothing - handled in following case */

		case UA_LINEAR_DIMS_REL:
		/* compute scale factor */

			zerov[0] = zerov[1] = zerov[2] = 0.0;
			um_vctmtf(e->cpln.yaxis,tf,pt3);
			scal = um_mag(pt3);

			/* modify scalers */

			e->txt_gap = e->txt_gap * scal;
			e->grid_dist = e->grid_dist * scal;
			e->char_size = e->char_size * scal;
			e->gap_to_geo = e->gap_to_geo * scal;
			e->ext_past_line = e->ext_past_line * scal;
			e->stub_length = e->stub_length * scal;
			e->arrow_size = e->arrow_size * scal;
			e->xh_spacing = e->xh_spacing * scal;

			/* modify dim_org */

			um_cctmtf(e->dim_origin, tf, e->dim_origin);

			/* transform text plane */

			um_cctmtf(e->cpln.cpln_origin, tf, e->cpln.cpln_origin);
			um_vctmtf(e->cpln.xaxis, tf, vtx);
			um_vctmtf(e->cpln.yaxis, tf, vty);
			um_vctmtf(e->cpln.zaxis, tf, vtz);

			/* check vectors */

			if(um_cceqcc(vtz,zerov))
				{
				if((um_cceqcc(vtx,zerov)==UU_TRUE) ||
								(um_cceqcc(vty,zerov)==UU_TRUE)) goto lp1;
				um_cross(vtx,vty,vtz);
				}
			else
				{
				if(um_cceqcc(vty,zerov))
					{
					if(um_cceqcc(vtx,zerov)) goto lp1;
					um_cross(vtz,vtx,vty);
					}
				else
					{
					if(um_cceqcc(vtx,zerov))
						{
						um_cross(vty,vtz,vtx);
						}
					}
				}
			um_unitvc(vtx,vtx);
			um_unitvc(vty,vty);
			um_unitvc(vtz,vtz);
			um_vctovc(vtx,e->cpln.xaxis);
			um_vctovc(vty,e->cpln.yaxis);
			um_vctovc(vtz,e->cpln.zaxis);
	lp1:

			/* modify text blocks */

			for(i=0; i<e->txt_blk_use; i++)
				{
				um_vctmtf(e->txt_blk[i].origin, tf, e->txt_blk[i].origin);
				e->txt_blk[i].dx = e->txt_blk[i].dx * scal;
				e->txt_blk[i].dy = e->txt_blk[i].dy * scal;
				e->txt_blk[i].txt_size = e->txt_blk[i].txt_size * scal;
				}

			/* modify arc blocks */

			for(i=0; i<e->arc_blk_use; i++)
				{
				um_cctmtf(e->arc_blk[i].center_pt, tf, e->arc_blk[i].center_pt);
				e->arc_blk[i].radius = e->arc_blk[i].radius * scal;
				}

		 /* modify line blocks */

		 if (e->etype == UA_HATCHING_REL)
		 	{
			 key = e->asso_blk[e->asso_blk_use -1].key;
			 uig_transform_xh_rec(key, tf);
			 }

		for(i=0; i<e->line_blk_use; i++)
			{
			for(j=0; j<e->line_blk[i].num_pts; j++)
				{
				pt1[0] = e->line_blk[i].line_seg[j][0];
				pt1[1] = e->line_blk[i].line_seg[j][1];
				pt1[2] = e->line_blk[i].line_seg[j][2];
				um_cctmtf(pt1, tf, pt1);
				e->line_blk[i].line_seg[j][0] = pt1[0];
				e->line_blk[i].line_seg[j][1] = pt1[1];
				e->line_blk[i].line_seg[j][2] = pt1[2];
				}
			}

			/* modify arrow blocks */

			for(i=0; i<e->arrow_blk_use; i++)
				{
				um_cctmtf(e->arrow_blk[i].location, tf, e->arrow_blk[i].location);
				e->arrow_blk[i].size = e->arrow_blk[i].size * scal;
				}

			/* modify asso blocks */

			if(e->etype == UA_PL_DIM)
				{
				for(i=0; i<e->asso_blk_use; i++)
					{
					note.key = e->asso_blk[i].key;
					if(e->asso_blk[i].modifier == -99)
						e->asso_blk[i].location[0] = e->asso_blk[i].location[0]/scal;
					if(ua_get_text(&note, sizeof(struct UA_txt_rec)) == UU_SUCCESS)
						{
						uig_transform_text(&note, tf);
						}
					}
				}
			else
				{
				if(e->asso_blk_use > 0)
					{
					for(i=0; i<e->asso_blk_use; i++)
						{
						um_cctmtf(e->asso_blk[i].location, tf, e->asso_blk[i].location);
						}
					}
				}

			break;
		}	/* end of switch */

fexit:;
	uu_dexit;
	return(UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION: int uig_transform_agsrf(eptr, tfmat)
**      Tranform entity (EPTR) by the specified transform (TFMAT)
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
**        tfmat	   	4 x 3 transformation matrix to transform entity by.
**       OUTPUT :  
**          eptr			pointer to the transformed entity record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_agsrf(eptr, tfmat)
   struct UM_agsrf_rec *eptr;
   UM_transf    tfmat;

	{
	int status;
	int uig_transform_array();

	/* cpp: modify calls to use uig_transform_array instead of
	   uig_transform_point */

	uu_denter(UU_MTRC, (us,"uig_transform_agsrf(key=%d,tfmat=%x)",
				eptr->key, tfmat));

	status = UU_SUCCESS;

	ag_tr_srf(eptr->srfaddr, uig_transform_array, tfmat, uig_transform_array);

	uu_dexitstatus("uig_transform_agsrf",status);
	return(status);
	}

/*********************************************************************
**
**    E_FUNCTION  :	uig_transform_xh_rec(key, tf)
**
**			transform the coords in the hatchlin_rec referenced by key
**			by the matrix tf
**
**    PARAMETERS   
**
**       INPUT  :	key - key of hatchlin_rec to xform
**						tf - xform matrix
**
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

uig_transform_xh_rec(key, tf)
UU_KEY_ID	key;
UU_REAL	tf[4][3];
{
	struct UA_hatchlin_rec e;

	uu_denter(UU_STRC,(us, "uig_transform_xh_rec: key = %d", key));

	/* get hatchlin_rec for key */
	e.key = key;
	if (ua_retrieve_xhatch(&e, sizeof(struct UA_hatchlin_rec))
		!= UU_SUCCESS)
	{
		uu_denter2(UU_STRC,(us,"ua_transf_hatch_rec: uc_retrieve_data ERROR"));
		uu_dexit;
	}
	/* transform coords */
	uig_transform_xhatch(&e, tf);

	uu_free(e.wt);
	uu_dexit;
   return(UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION :  uig_transform_xhatch (entity, tf)
**       Transform a cross hatch entity
**    PARAMETERS   
**       INPUT  : 
**          entity			entity record
**          tf				transformation matrix
**       OUTPUT :  
**          entity			modified entity record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_transform_xhatch(e, tf)
struct UA_hatchlin_rec	*e;
UU_REAL						tf[4][3];
{
	UU_REAL	pt[3];
	UU_REAL  *p;

	uu_denter(UU_STRC,(us,"uig_transform_xhatch"));

	for (p=e->wt; p < (e->wt + e->no_wt); p+=3)
	{
		pt[0] = *p; 
		pt[1] = *(p + 1);
		pt[2] = *(p + 2);
		um_cctmtf(pt, tf, pt);
		*p = pt[0];
		*(p + 1) = pt[1];
		*(p + 2) = pt[2];
	}

	uu_dexit;
   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION :  ua_retrieve_xhatch(e, size)
**       retrieve a hatchline entity from Unibase
**    PARAMETERS   
**       INPUT  : 
**          	e - hatchline entity to be retrieved
**				size - size of entity
**       OUTPUT :  
**				none
**    RETURNS      : UU_SUCCESS if OK
**    SIDE EFFECTS : none
**    WARNINGS     : calling routine should call uu_free(e.pt)
**				before returning
*********************************************************************/
int ua_retrieve_xhatch(e, size)
struct UA_hatchlin_rec *e;
int size;
{
	int status;

	uu_denter(UU_STRC,(us,"entering ua_retrieve_xhatch(%x)",
		e));

	/* get fixed part of hatchlin_rec out of Unibase */
	if ((status = ur_retrieve_data_fixed(e)) != UU_SUCCESS)
	{
		uu_denter2(UU_STRC,(us,"ua_retrieve_xhatch: retrieve_data_fixed ERROR"));
		uu_dexit;
	}

	/* set up for coordinate array */
	e->wt = (UU_REAL*) uu_malloc(e->no_wt * sizeof(UU_REAL));

	/* fill it up */
	if (ur_retrieve_data_varlist(e->key, 1, e->wt, 1, e->no_wt) == UU_FAILURE)
	{
		status = UU_FAILURE;
		uu_denter2(UU_STRC,(us,"ua_retrieve_xhatch: retrieve_data_varlist ERROR"));
		uu_dexit;
	}
	uu_denter2(UU_STRC,(us,"ua_retrieve_xhatch: e.key=%d .rel_num=%d .no_wt=%d", e->key, e->rel_num, e->no_wt));
	uu_dexit;

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ua_get_text(eptr, databagsize)
**			Given an entity pointed to by EPTR->KEY this function
**			will retrieve all immediate information associated with the 
**			entity (i.e. fixed length entity records and all variable lists).
**    PARAMETERS   
**       INPUT  : 
**				eptr          pointer to an entity structure for returning
**								  the requested information.  Note that the
**                        "key" of the entity must be initialized. 
**				databagsize   size of the data bag pointed to by "eptr".
**       OUTPUT :  
**          eptr				all fixed and variable lists of the entity
**									are returned
**    RETURNS      :
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_get_text(eptr, databagsize)
struct UM_entitydatabag *eptr;
int databagsize;

{
	int isize;
	int status;

	uu_denter(UU_STRC,(us,"ua_get_text(key:%d, bagsize:%d)",
							eptr->key, databagsize));
	status = UU_SUCCESS; /* assume success */
	if ((int) eptr->key < 0)
		status = UU_FAILURE;
	else
	  {
		ur_retrieve_data_relnum(eptr->key, &eptr->rel_num);
		if (eptr->rel_num == UA_TEXT_REL)
			{
			 status = ur_retrieve_data(eptr, databagsize);
			 if (status != 0)
				{
				  status = UU_FAILURE;
				}
			  else		 /* got data OK	*/
				 {
				  isize = ((struct UA_txt_rec *)eptr)->no_tchar;
						/* end string */
				  ((struct UA_txt_rec *)eptr)->tchar[isize] = '\0';
				 }
			}
		else					/* invalid relation number		*/
			{
			 status = UU_FAILURE;
			}
		}
	if (status == UU_FAILURE)
		{
		uu_dprint(UU_MTRC,(us,"FAILURE: rel=%d, key=%d, databagsize=%d",
					eptr->rel_num, eptr->key, databagsize));
		}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int uig_transform_rbsrf(eptr,tranfmat)
**      Transform a rbsf by the given 4X3 matrix
**    PARAMETERS   
**       INPUT  : 
**          eptr         pointer to the entity to be transformed
**          tfmat        the 4x3 transformation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_rbsrf(eptr,tfmat)
   struct UM_rbsplsrf_rec *eptr;
   UM_transf    tfmat;
   {                                     
   int i;
   UU_REAL *p;

   uu_denter(UU_MTRC,(us,"uig_transform_rbsrf(key:%d,tfmat:%x)",
                  eptr->key,tfmat));
    
   p = eptr->pt;
   for (i=0; i<eptr->no_pt; i++)
     {
     um_cctmtf(p, tfmat, p);
     p += 3;
     }

   uu_dexit;
   return (UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : int uig_transform_revsf(eptr,tranfmat)
**      Transform a surface of revolution by the given 4X3 matrix
**    PARAMETERS   
**       INPUT  : 
**          eptr         pointer to the entity to be transformed
**          tfmat        the 4x3 transformation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_revsf(eptr,tfmat)
   struct NCL_revsurf_rec *eptr;
   UM_transf    tfmat;
   {                                     
   int i, status;
   struct NCL_fixed_databag crv;
	UU_LOGICAL store = UU_TRUE;

   uu_denter(UU_MTRC,(us,"uig_transform_revsf(key:%d,tfmat:%x)",
                  eptr->key,tfmat));
	
	if (eptr->cvkey <= 0) return (UU_FAILURE);

	crv.key = eptr->cvkey;
	status = ncl_retrieve_data_fixed (&crv);
	if (status == UU_SUCCESS) status = uig_transform (&crv, tfmat, store);

	if (status == UU_SUCCESS)
	{
		um_cctmtf(eptr->pta, tfmat, eptr->pta);
		um_vctmtf(eptr->vca, tfmat, eptr->vca);
	}

 	if (status == UU_SUCCESS && store) status = ur_update_data_fixed (eptr);   

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int uig_transform_compcrv(eptr,tranfmat)
**      Transform a composite curve by the given 4X3 matrix
**    PARAMETERS   
**       INPUT  : 
**          eptr         pointer to the entity to be transformed
**          tfmat        the 4x3 transformation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform_compcrv(eptr,tfmat)
   struct UM_compcrv_rec *eptr;
   UM_transf    tfmat;
   {                                     
   int i, status;
   struct NCL_fixed_databag crv;
   UU_LOGICAL store = UU_TRUE;

   uu_denter(UU_MTRC,(us,"uig_transform_compcrv(key:%d,tfmat:%x)",
                  eptr->key,tfmat));
    
   status=UU_SUCCESS;

   for (i=0; i<eptr->no_cid && status==UU_SUCCESS; i++)
     {
     crv.key = eptr->cid[i].crvid;
     status = ncl_retrieve_data_fixed (&crv);
     if (status == UU_SUCCESS) status = uig_transform (&crv, tfmat, store);
     }

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int uig_transform(eptr,tranfmat,store)
**      Transform a entity by the given 4X3 matrix
**    PARAMETERS   
**       INPUT  : 
**          eptr         pointer to the entity to be transformed
**          tfmat        the 4x3 transformation matrix
**          store        update unibase iff true.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transform(eptr,tfmat,store)
   struct NCL_fixed_databag *eptr;
   UM_transf    tfmat;
   UU_LOGICAL store;
   {                                     
   int status;

   uu_denter(UU_MTRC,(us,"uig_transform(key:%d,tfmat:%x)",
                  eptr->key,tfmat));
    
    switch (eptr->rel_num)
        {
        case UM_LINE_REL:
            status = uig_transform_line (eptr, tfmat);
            if (store) ur_update_data_fixed (eptr);
            break;
        case UM_CIRCLE_REL:
            status = uig_transform_circle (eptr, tfmat);
            if (store) ur_update_data_fixed (eptr);
            break;
        case UM_CONIC_REL:
            status = uig_transform_conic (eptr, tfmat);
            if (store) ur_update_data_fixed (eptr);
            break;
        case UM_COMPCRV_REL:
            status = uig_transform_compcrv (eptr, tfmat);
            break;
        case UM_RBSPLCRV_REL:
            status = uig_transform_rbspline (eptr, tfmat,store);
            break;
        case UM_RBSPLSRF_REL:
            status = ncl_transform_rbsf(eptr,tfmat,store);
            break;
        case NCL_REVSURF_REL:
            status = ncl_transform_revsf(eptr,tfmat,store);
            break;
        default:
            status = UU_FAILURE;
            break;
        }
    uu_dexit;
    return (status);
  }
