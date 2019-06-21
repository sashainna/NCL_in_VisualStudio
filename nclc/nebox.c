/*********************************************************************
**    NAME         :  nebox.c
**       CONTAINS:  Routines to calculate & intersect limit box around
**                  geometry.
**           ncl_geo_box
**           int cvbox (key, box, ierr)
**           int ncl_line_box (key, box)
**           int ncl_circ_box (key, box)
**           int ncl_ncrv_box (key, box)
**           int ncl_rbsp_box (key, box)
**           int ncl_ccrv_box (key, box)
**           int ncl_ecrv_box (key, box)
**           int cvxbox (box1, box2, irslt, boxo)
**           int ptinbx (pt, box, irslt)
**           int cilnth (nclkey, len, ierr)
**           int lnlnth (nclkey, len, ierr)
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nebox.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:24
*********************************************************************/

#include "usysdef.h"
#include "atext.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mdgenent.h"
#include "mgeom.h"
#include "msol.h"
#include "nccs.h"
#include "nclfc.h"
#include "uminmax.h"

/*********************************************************************
**    E_FUNCTION     : ncl_geo_box(inkey,box)
**       Call appropriate routine to calculate box around Unibase
**       geometry.  The box will be calculated in the current Working
**       Coordinate System.
**    PARAMETERS   
**       INPUT  : 
**          inkey    - Key of entity.
**       OUTPUT :  
**          box      - Calculated box.
**    RETURNS      : 
**         UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_geo_box(inkey,box)
UU_KEY_ID inkey;
UU_REAL *box;
{
	int rel_num,status,i,m,npt,nkeys,inc;
	UM_int2 ierr,idx,i3;
	UM_int4 key;
	UU_LOGICAL compfl;
	UU_KEY_ID nclkey;
	UU_REAL tsav,tol,tmpbox[6];
	UM_coord *pts;
	UM_transf tfmat;
	UM_3D_box sfbox;
	UU_LIST dlist;
	UM_tessellation tess;
	struct UM_srfdatabag ent;
	struct UM_solid_rec solid,csol;
	struct UM_point_rec pt;
	struct NCL_nclpv_rec pv;
	struct NCL_patern_rec pn;
	struct UC_entitydatabag e;
/*
.....Get the geometry type
*/
	i3 = 3;
	nclkey = inkey;
	status = ur_retrieve_data_relnum (nclkey,&rel_num);
	if (status != UU_SUCCESS) goto done;
/*
.....Composite solid
*/
	compfl = UU_FALSE;
	nkeys = 1;
	if (rel_num == UM_SOLID_REL)
	{
		csol.key = nclkey;
		status = ncl_retrieve_data(&csol,sizeof(struct UM_solid_rec));
		if (status != UU_SUCCESS) goto done;
		if (csol.type == UM_COMPOS_SOLID)
		{
			compfl = UU_TRUE;
			nkeys = csol.no_netkey;
		}
	}
/*
.....Loop through entities
*/
	for (inc=0;inc<nkeys;inc++)
	{
		if (compfl)
		{
			nclkey = csol.netkey[inc];
			status = ur_retrieve_data_relnum (nclkey,&rel_num);
			if (status != UU_SUCCESS) goto done;
		}
		switch (rel_num)
		{
/*
.....Points
*/
		case UM_POINT_REL:
			pt.key = nclkey;
			status = ncl_retrieve_data(&pt,sizeof(struct UM_point_rec));
			um_mcstoccs(0,pt.pt,pt.pt);
			conref(pt.pt,&i3);
			if (status == UU_SUCCESS) ncl_init_box(pt.pt,box);
			break;
/*
.....Point-Vectors
*/
		case NCL_POINTVEC_REL:
			pv.key = nclkey;
			status = ncl_retrieve_data(&pv,sizeof(struct NCL_nclpv_rec));
			um_mcstoccs(0,pt.pt,pt.pt);
			conref(pt.pt,&i3);
			if (status == UU_SUCCESS) ncl_init_box(pv.pt,box);
			break;
/*
.....Patterns
*/
		case NCL_PATERN_REL:
			pn.key = nclkey;
			status = ncl_retrieve_data(&pn,sizeof(struct NCL_patern_rec));
			if (status == UU_SUCCESS)
			{
				m = 3 * pn.pntype;
				npt = pn.no_patpnt;
				um_mcstoccs(0,pn.patpnt[0],pn.patpnt[0]);
				conref(pn.patpnt[0],&i3);
				ncl_init_box(&pn.patpnt[0],box);
				for (i=m;i<npt;i=i+m)
				{
					um_mcstoccs(0,pn.patpnt[i],pn.patpnt[i]);
					conref(pn.patpnt[i],&i3);
					ncl_update_box(&pn.patpnt[i],box);
				}
			}
			break;
/*
.....Curves
*/
		case UM_LINE_REL:
		case UM_CIRCLE_REL:
		case NCL_CURVE_REL:
		case UM_RBSPLCRV_REL:
		case UM_COMPCRV_REL:
		case UM_CONIC_REL:
		case UM_UVCVONSF_REL:
		case UM_POLYLINE_REL:
		case NCL_EVALCV_REL:
			e.key = nclkey;
			status = ncl_retrieve_data_fixed(&e);
			if (status == UU_SUCCESS) status = uc_retrieve_transf(e.key,tfmat);
			if (status != UU_SUCCESS) goto done;
			gettol(&tol);
			uu_list_init(&dlist,sizeof(UM_coord),100,100);
			if (e.rel_num == NCL_SHAPE_REL)
				npt = ncl_evolve_shape(&e,tfmat,tol,&dlist);
			else
				npt = ncl_evolve_all_curves(&e,tfmat,tol,&dlist,UU_NULL,UU_FALSE);
			if (npt <= 1) goto done;
			pts = (UM_coord *)UU_LIST_ARRAY(&dlist);
			um_mcstoccs(0,pts[0],pts[0]);
			conref(pts[0],&i3);
			ncl_init_box(pts[0],box);
			for (i=1;i<npt;i++)
			{
				um_mcstoccs(0,pts[i],pts[i]);
				conref(pts[i],&i3);
				ncl_update_box(pts[i],box);
			}
			uu_list_free(&dlist);
			break;
/*
.....Surfaces
*/
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_MESHSURF_REL:
		case NCL_QUILTSURF_REL:
		case NCL_NETSF_REL:
		case NCL_EVALSF_REL:
		case NCL_TRIMSF_REL:
		case NCL_REVSURF_REL:
			ent.key = nclkey;
			status = ncl_retrieve_data_fixed(&ent);
			if (status == UU_SUCCESS) status = ncl_sf_box(&ent,box);
			break;
/*
.....Solids
*/
		case UM_SOLID_REL:
			solid.key = nclkey;
			status = ncl_retrieve_data(&solid,sizeof(struct UM_solid_rec));
			if (status == UU_SUCCESS)
			{
				idx = 27; getsc(&idx,&tol);
				idx = 175; getsc(&idx,&tsav); setscv(&idx,&tol);
				ncl_solid_wcstomcs(solid.type,solid.sdata,UU_TRUE);
				status = ncl_solid_calc_lists(&solid,solid.sdata,solid.no_sdata,
					&dlist,&tess);
				if (status == UU_SUCCESS)
				{
					for (i=0;i<6;i++) box[i] = solid.box[i];
					uu_list_free(&dlist);
					um_free_tess(&tess);
				}
				idx = 27; setscv(&idx,&tsav);
			}
			break;
/*
.....Annotation
*/
		case UA_TEXT_REL:
			ent.key = nclkey;
			status = ncl_retrieve_data(&ent,sizeof(struct UA_txt_rec));
			if (status == UU_SUCCESS) status = uai_calc_text_bounds(&ent,box);
			break;
/*
.....Symbols
*/
		case UB_SYMBOL_REL:
		case UB_INSTANCE_REL:
			ent.key = nclkey;
			status = ncl_retrieve_data(&ent,sizeof(struct NCL_fixed_databag));
			if (status == UU_SUCCESS) status = ubi_calc_symbol_box(&ent,box);
			break;
/*
.....Unsupported geometry type
*/
		default:
			status = UU_FAILURE;
			break;
		}
/*
.....Update temporary box if composite solid
*/
		if (status == UU_SUCCESS && compfl)
		{
			if (inc == 0) ncl_init_box(box,tmpbox);
			else ncl_update_box(box,tmpbox);
			ncl_update_box(&box[3],tmpbox);
		}
	}
/*
.....End of routine
*/
done:
	if (status == UU_SUCCESS && compfl)
	{
		for (i=0;i<6;i++) box[i] = tmpbox[i];
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int cvbox (nclkey, box, ierr)
**       Call appropriate routine to calulate curve box.
**    PARAMETERS   
**       INPUT  : 
**          nclkey   - Key of curve.
**       OUTPUT :  
**          box      - Calculated box.
**          ierr     - 0 if no error else 1
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
cvbox (nclkey, box, ierr)
UM_int4 *nclkey;
UM_real8 *box;
UM_int2 *ierr;
   {
   
   int status, rel_num;
   UU_KEY_ID key;

   key = *nclkey;
   status = UU_FAILURE;
   if (ur_retrieve_data_relnum (key ,&rel_num) == 0)
     {
     switch (rel_num) 
       {
       case UM_LINE_REL:
         status = ncl_line_box (key, box);
         break;
       case UM_CIRCLE_REL:
         status = ncl_circ_box (key, box);
         break;
       case NCL_CURVE_REL:
         status = ncl_ncrv_box(key, box);
         break;
       case UM_RBSPLCRV_REL:
         status = ncl_rbsp_box(key, box);
         break;
       case UM_COMPCRV_REL:
         status = ncl_ccrv_box(key, box);
         break;
       default:
         status = ncl_ecrv_box(key, box);
         break;
       }
     }
   *ierr = 0;
   if (status == UU_FAILURE) *ierr = 1;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_line_box (key, box)
**       Calculate curve box for line.
**    PARAMETERS   
**       INPUT  : 
**          key      - Key of line.
**       OUTPUT :  
**          box      - Calculated box.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_line_box (key, box)
UU_KEY_ID key;
UM_real8 *box;
   {
   int i, status;
   struct UM_line_rec ln;
   UM_real8 buf[6];

   status = UU_SUCCESS;
   ln.key = key;
   if (ncl_retrieve_data_fixed ((struct NCL_fixed_databag *)&ln) != 0)
     {
     status = UU_FAILURE;
     }
   else
     {
     for (i=0;i<3;i++)
       {
       box[i] = ln.spt[i];
       if (ln.ept[i] < box[i]) box[i] = ln.ept[i];
       box[i+3] = ln.spt[i];
       if (ln.ept[i] > box[i+3]) box[i+3] = ln.ept[i];
       }
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_circ_box (key, box)
**       Calculate curve box for circ.
**    PARAMETERS   
**       INPUT  : 
**          key      - Key of circ.
**       OUTPUT :  
**          box      - Calculated box.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_circ_box (key, box)
UU_KEY_ID key;
UM_real8 *box;
   {
   int status;
   struct UM_circle_rec ci;
   UM_real8 buf[11];

   ci.key = key;
   if (ncl_retrieve_data_fixed ((struct NCL_fixed_databag *)&ci) != 0)
     {
     status = UU_FAILURE;
     }
   else
     {
     status = ncl_circ_to_nclcirc(&ci, buf);
     if (status == UU_SUCCESS)
       {
       cibox (buf, box);
       }
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_ncrv_box (key, box)
**       Calculate curve box for NCL curve. The NCL curve control
**       points are used, which the NCL curve always lies within.
**    PARAMETERS   
**       INPUT  : 
**          key      - Key of curve.
**       OUTPUT :  
**          box      - Calculated box.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ncrv_box (key, box)
UU_KEY_ID key;
UM_real8 *box;
   {
   int status, i, j;
   struct NCL_fixed_databag e1;
   struct NCL_curve_rec *ncrv;
   struct NCL_segment_rec *segp;
	struct UM_evcrvout evout;
   UU_REAL pt[3], rho;

   e1.key = key;
   if (ncl_retrieve_data_fixed (&e1) != 0)
     {
     status = UU_FAILURE;
     }
   else
     {
     status = UU_SUCCESS;
     ncrv = (struct NCL_curve_rec *)&e1;
     segp = ncrv->segment;
     ncl_init_box (segp->point, box);
     for (i=1;i<ncrv->no_segment;i++)
       {
       for (j=0;j<3;j++) pt[j] = segp->point[j]+segp->delta[j];
       ncl_update_box (pt, box);
       rho = segp->rho;
       segp++;
       for (j=0;j<3;j++) pt[j] = segp->point[j]-rho*segp->delta[j];
       ncl_update_box (pt, box);
       ncl_update_box (segp->point, box);
       }
/*
.....vp 29-apr-97 make sure that extension points are in box
*/
		uc_init_evcrvout (ncrv,&evout);
		if (ncrv->t0 != 0.0)
		{
			ncl_ev_curve(UM_FRSTDERIV,0.,ncrv,UM_DEFAULT_TF,&evout);
			ncl_update_box (evout.cp, box);
		}
		if (ncrv->t1 != ncrv->t_end)
		{
			ncl_ev_curve(UM_FRSTDERIV,1.,ncrv,UM_DEFAULT_TF,&evout);
			ncl_update_box (evout.cp, box);
		}
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_rbsp_box (key, box)
**       Calculate curve box for B-spline curve. The B-Spline control
**       points are used, which the B-spline always lies within.
**    PARAMETERS   
**       INPUT  : 
**          key      - Key of curve.
**       OUTPUT :  
**          box      - Calculated box.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_rbsp_box (key, box)
UU_KEY_ID key;
UM_real8 *box;
   {
   int status, i;
   struct UM_srfdatabag e1;
   struct UM_rbsplcrv_rec *rcrv;
	struct UM_evcrvout evout;
   UU_REAL *pt;

   e1.key = key;
   if (ncl_retrieve_data_fixed (&e1) != 0)
     {
     status = UU_FAILURE;
     }
   else
     {
     status = UU_SUCCESS;
     rcrv = (struct UM_rbsplcrv_rec *)&e1;
     pt = rcrv->pt;
     ncl_init_box (pt, box);
     for (i=1;i<rcrv->no_pt;i++)
       {
       pt += 3;
       ncl_update_box (pt, box);
       }
/*
.....vp 29-apr-97 make sure that extension points are in box
*/
		uc_init_evcrvout (rcrv,&evout);
		if (rcrv->t0 != rcrv->t[0])
		{
			um_ev7_rbsplcrv(UM_FRSTDERIV,0.,rcrv,UM_DEFAULT_TF,&evout);
			ncl_update_box (evout.cp, box);
		}
		if (rcrv->t1 != rcrv->t[rcrv->no_t-1])
		{
			um_ev7_rbsplcrv(UM_FRSTDERIV,1.,rcrv,UM_DEFAULT_TF,&evout);
			ncl_update_box (evout.cp, box);
		}
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_ccrv_box (key, box)
**       Calculate curve box for composite curve.
**    PARAMETERS   
**       INPUT  : 
**          key      - Key of curve.
**       OUTPUT :  
**          box      - Calculated box.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ccrv_box (key, box)
UU_KEY_ID key;
UM_real8 *box;
   {
   int status, i, rel_num;
   UU_KEY_ID lkey;
   UM_real8 tbox[6];
   struct NCL_fixed_databag e1;
   struct UM_compcrv_rec *ccrvp;
   struct UM_cid_rec *ccidp;

   e1.key = key;
   if (ncl_retrieve_data_fixed (&e1) != 0)
     status = UU_FAILURE;
   else
     {
     status = UU_SUCCESS;
     ccrvp = (struct UM_compcrv_rec *)&e1;
     ccidp = ccrvp->cid;
     for (i=0;i<ccrvp->no_cid && status==UU_SUCCESS;i++)
       {
       lkey = ccidp->crvid;
       if (ur_retrieve_data_relnum (lkey, &rel_num) != 0)
         status = UU_FAILURE;
       else
       switch (rel_num) 
         {
         case UM_LINE_REL:
           status = ncl_line_box (lkey, tbox);
           break;
         case UM_CIRCLE_REL:
           status = ncl_circ_box (lkey, tbox);
           break;
         case UM_RBSPLCRV_REL:
           status = ncl_rbsp_box (lkey, tbox);
           break;
         case NCL_CURVE_REL:
           status = ncl_ncrv_box(key, box);
           break;
         default:
           status = ncl_ecrv_box(lkey, tbox);
           break;
         }
       if (status == UU_SUCCESS)
         {
         if (i==0) 
           ncl_init_box (tbox, box);
         else
           ncl_update_box (tbox, box);
         ncl_update_box (&tbox[3], box);
         }
       ccidp++;
       }
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_ecrv_box (key, box)
**       Calculate curve box for evaluated curve.
**    PARAMETERS   
**       INPUT  : 
**          key      - Key of curve.
**       OUTPUT :  
**          box      - Calculated box.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ecrv_box (key, box)
UU_KEY_ID key;
UM_real8 *box;
   {
   int status, i;
   struct NCL_fixed_databag e1;
   struct UM_evcrvout evout;
   UM_transf tfmat;
   UM_param u;
   UU_REAL du;

   e1.key = key;
   if (ncl_retrieve_data_fixed (&e1) != 0)
     status = UU_FAILURE;
   else
     {
     status = um_get_transformation(e1.key, tfmat);
     if (status == UU_SUCCESS)
       {
       u = 0.0;
       status = uc_evcrv(UM_POINT, u, &e1, tfmat, &evout);
       ncl_init_box (evout.cp, box);
       du = .01;
       for (i=0, u=du;i<100 && status==UU_SUCCESS;i++, u+=du)
         {
         status = uc_evcrv(UM_POINT, u, &e1, tfmat, &evout);
         ncl_update_box (evout.cp, box);
         }
       }
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int cvxbox (box1, box2, irslt, boxo)
**       Intersect 2 curve limit boxes.
**    PARAMETERS   
**       INPUT  : 
**          box1     - Limit box of first curve.
**          box2     - Limit box of second curve.
**       OUTPUT :  
**          irslt    - 1 iff boxes intersect; else 0
**          boxo     - Intersection limit box.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void cvxbox (box1, box2, irslt, boxo)
UM_real8 box1[6], box2[6], boxo[6];
UM_int2 *irslt;
   {
   UM_real8 tmp;

   *irslt = 1;
/*
...   Store the highest "lower left" values in the output box
*/
   boxo[0] = box1[0];
   if (box2[0] > boxo[0]) boxo[0] = box2[0];
   boxo[1] = box1[1];
   if (box2[1] > boxo[1]) boxo[1] = box2[1];
   boxo[2] = box1[2];
   if (box2[2] > boxo[2]) boxo[2] = box2[2];
/*
...   Store the lowest "upper right" values in the output box
*/
   boxo[3] = box1[3];
   if (box2[3] < boxo[3]) boxo[3] = box2[3];
   boxo[4] = box1[4];
   if (box2[4] < boxo[4]) boxo[4] = box2[4];
   boxo[5] = box1[5];
   if (box2[5] < boxo[5]) boxo[5] = box2[5];
/*
...   If the lower coord is greater than the higher coord by more more than
...   a tolerance factor, the boxes do not intersect.
*/
   if (boxo[0] > boxo[3])
     {
     if (boxo[0]-boxo[3] > .1) *irslt = 0;
     tmp = boxo[0];
     boxo[0] = boxo[3];
     boxo[3] = tmp;
     }
   if (boxo[1] > boxo[4])
     {
     if (boxo[1]-boxo[4] > .1) *irslt = 0;
     tmp = boxo[1];
     boxo[1] = boxo[4];
     boxo[4] = tmp;
     }
   if (boxo[2] > boxo[5])
     {
     if (boxo[2]-boxo[5] > .1) *irslt = 0;
     tmp = boxo[2];
     boxo[2] = boxo[5];
     boxo[5] = tmp;
     }

   return;
   }
/*********************************************************************
**    E_FUNCTION     : int ptinbx (pt, box, irslt)
**       Determine if a point is inside a limit box.
**    PARAMETERS   
**       INPUT  : 
**          pt       - Point to test.
**          box      - Limit box of curve.
**       OUTPUT :  
**          irslt    - 1 iff point is inside box; else 0
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ptinbx (pt, box, irslt)
UM_real8 pt[3], box[6];
UM_int2 *irslt;
{
   UU_REAL tol;
/*
.....vp 23-apr-97 relax it
   UU_REAL tol = 1.0e-6;
*/
   getsct(&tol);
	tol = .5*tol;
   *irslt = 1;
   if (pt[0] < box[0]-tol || pt[0] > box[3]+tol ||
       pt[1] < box[1]-tol || pt[1] > box[4]+tol ||
       pt[2] < box[2]-tol || pt[2] > box[5]+tol) *irslt = 0;

   return;
}
/*********************************************************************
**    E_FUNCTION     : int cilnth (nclkey, len)
**       Calculate the arc length of a circle.
**    PARAMETERS   
**       INPUT  : 
**          nclkey   - Key of circle.
**       OUTPUT :  
**          len      - Length of circle.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
cilnth (nclkey, len, ierr)
UM_int4 *nclkey;
UM_real8 *len;
UM_int2 *ierr;
   {
   int status;
   struct UM_circle_rec c1;

   c1.key = *nclkey;
   if (ur_retrieve_data_fixed (&c1) != 0)
     {
     status = UU_FAILURE;
     *len = 0;
     *ierr = 1;
     }
   else
     {
     status = UU_SUCCESS;
     *ierr = 0;
     *len = c1.radius*c1.dang;
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int lnlnth (nclkey, len)
**       Calculate the length of a line.
**    PARAMETER
**       INPUT  :
**          nclkey   - Key of circle.
**       OUTPUT :
**          len      - Length of circle.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
lnlnth (nclkey, len, ierr)
UM_int4 *nclkey;
UM_real8 *len;
UM_int2 *ierr;
   {
   int status, i;
   UU_REAL d1;
   struct UM_line_rec l1;

   *len = 0;
   l1.key = *nclkey;
   if (ur_retrieve_data_fixed (&l1) != 0)
     {
     status = UU_FAILURE;
     *ierr = 1;
     }
   else
     {
     status = UU_SUCCESS;
     *ierr = 0;
     for (i=0;i<3;i++)
       {
       d1 = l1.ept[i]-l1.spt[i];
       *len += d1*d1;
       }
     *len = sqrt(*len);
     }

   return (status);
   }
/******************************************************************/
int
ncl_ncrv_len (key, len)
UU_KEY_ID *key;
UM_real8 *len;
{
	int status;
	struct NCL_curve_rec e1;

	status = UU_SUCCESS;
	e1.key = *key;
	if (ncl_retrieve_data_fixed (&e1) != 0)
	{
		status = UU_FAILURE;
	}
	else
	{
		*len = (e1.t1 - e1.t0) * e1.t_end;
	}
	return(status);
}
