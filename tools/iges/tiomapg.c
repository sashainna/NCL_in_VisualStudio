/*********************************************************************
**    NAME         :  tiomapg.c
**    CONTAINS:
**    	uio_point 	
**			uio_line 	
**			uio_plane 	
**			uio_arc 	 
**			uio_conic_arc
**			uio_get_coeff 		
**			uio_rbspl
**			uio_nclcrv
**			uio_nclpat
**			uio_agcrv_rbspl
**			uio_agsrf_rbspl
**			uio_comp_crv
**			uio_group
**			uio_check_view
**       uio_view_vlpln 
**    	uig_set_trans
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       tiomapg.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/17/15 , 17:57:12
*********************************************************************/

#include "umath.h"
#include		"mdeval.h"
#include		"udebug.h"
#include		"tiges.h"
#include		"tigdefs.h"
#include		"rbase.h"
#include		"class.h" 
#include		"mdrel.h" 
#include		"mdattr.h"
#include		"mattrddl.h"
#include		"mcrv.h"
#include		"mdcoord.h"
#include		"tioconv.h"
#include		"modef.h"
#include		"mxxx.h"
#include		"view.h"
#include		"nccs.h"

#include "ag_incl.h"

int UIO_sub_swt = 0;
int UIO_use_flg = 0;
UU_LOGICAL UIO_uvcurve = 0;
extern UU_LOGICAL UIO_drawing;
char	*uu_toolmalloc();
/*********************************************************************
**    I_FUNCTION :  uio_geometry(fd1,fd2,e,attr,dcount,pcount)
**      Output the entities for any curve entity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_geometry(fd1,fd2,e,attr,dcount,pcount)
int	fd1, fd2;
struct UC_entitydatabag	*e;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */

{
   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_geometry"));	

	switch (e->rel_num)
	{
		case UM_POINT_REL:			/* 1 point */
			uio_point(fd1,fd2,e,attr,dcount,pcount);
			break;
	 	case UM_LINE_REL	:			/* 2 line */
			uio_line(fd1,fd2,e,attr,dcount,pcount);
			break;
	 	case UM_CIRCLE_REL:			/* 3 circle */
			uio_arc(fd1,fd2,e,attr,dcount,pcount);
			break;
	 	case UM_CONIC_REL:			/* 4 conic curve*/
			uio_conic_arc(fd1,fd2,e,attr,dcount,pcount);
			break;
	 	case UM_COMPCRV_REL:			/* 5 composite curve*/
			uio_comp_crv(fd1,fd2,e,attr,dcount,pcount);
			break;
 		case UM_RBSPLCRV_REL:		/* 7 rational bspline curve */
			uio_rbspl(fd1,fd2,e,attr,dcount,pcount);
			break;
      case UM_RBSPLSRF_REL:      /* 11 rational bspline surface */
         uio_rbsrf(fd1,fd2,e,attr,dcount,pcount);
         break;
 		case NCL_CURVE_REL:		   /* 82 ncl Bezier curve */
			uio_nclcrv(fd1,fd2,e,attr,dcount,pcount);
			break;
 		case NCL_SURF_REL:		   /* 83 ncl Bezier surf */
			uio_nclsrf(fd1,fd2,e,attr,dcount,pcount);
			break;
      case NCL_MESHSURF_REL:     /* 85 NCL mesh surface */
         uio_mshsrf (fd1,fd2,e,attr,dcount,pcount);
         break;
 		case NCL_PATERN_REL:		   /* 92 ncl patern of points */
			uio_nclpat(fd1,fd2,e,attr,dcount,pcount);
			break;
		case UM_AGCRV_REL:
			uio_agcrv_rbspl(fd1,fd2,e,attr,dcount,pcount);
			break;
		case UM_POLY_REL:				/* 40 polyfill region	*/
			uio_map_polygon(fd1,fd2,e,attr,dcount,pcount);
			break;
	 	case UM_POLYLINE_REL:		/* 42 polyline */
			uio_polyline(fd1,fd2,e,attr,dcount,pcount);
			break;
	 	case NCL_REVSURF_REL:		/* 100 surf of revolution */
			uio_revsrf (fd1,fd2,e,attr,dcount,pcount);
			break;


		default:
			break;
	}	/* switch */
	uu_dexit;	
	return (0);
}	
/*********************************************************************
**    I_FUNCTION :  uio_point(fd1,fd2,e,attr,dcount,pcount)
**      Output the point information to
**			directory and parameter section record.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_point(fd1,fd2,e,attr,dcount,pcount)
int	fd1, fd2;
struct UM_point_rec	*e;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */
	
{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igespt_rec	ptrec;	/* iges point record */
	int prop[5];
	UU_REAL	v1[3];
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_point"));	

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GPOINT;
	dblk.sub_swt = UIO_sub_swt;
   dblk.use_flg = UIO_use_flg;
	strcpy(dblk.label,"POINT");    /*7.20.92vp*/
	dblk.subsno = e->subscr;       /*7.20.92vp*/

	ptrec.key = GPOINT;
	UIO_CC_INTTOEXT(e->pt,v1);		/* point coordinate */
	um_vctovc(v1,ptrec.pt);
								/* no display symbol specified */
	ptrec.fig_ptr = 0;
	uio_label (fd1,fd2,e->label,dcount,pcount); 
	ptrec.no_bptr = 0;
	dblk.par_ptr = *pcount;
	ptrec.no_prop = 1;
	prop[0] = *dcount - 2; 
/*	prop[0] = *dcount + 2; */ 
	ptrec.prop = &prop[0];
	uio_put_para(GPOINT,&ptrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
/*		uio_label (fd1,fd2,e->label,dcount,pcount); */
	uu_dexit;	
	return (0);
}
/*********************************************************************
**    I_FUNCTION :  uio_line(fd1,fd2,e,attr,dcount,pcount)
**      Output the line information to
**			directory and parameter section record.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_line(fd1,fd2,e,attr,dcount,pcount)
int	fd1, fd2;
struct UM_line_rec	*e;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */
	
{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igesline_rec	linerec;
	UU_REAL	v1[3];
	int  prop[5];
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_line"));	

	uio_init_dir(attr,&dblk);
	if (e->key == 0)
		blanked = UU_TRUE;
	else
	{
		uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
		ur_retrieve_blanked(e->key, &blanked);
	}
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GLINE;
	dblk.sub_swt = UIO_sub_swt;
   dblk.use_flg = UIO_use_flg;
	strcpy(dblk.label,"LINE");   /*7.20.92vp*/
	dblk.subsno = e->subscr;       /*7.20.92vp*/

	linerec.key = GLINE;			/* entity type */
	UIO_CC_INTTOEXT(e->spt,v1);
	um_vctovc(v1,linerec.spt);
	UIO_CC_INTTOEXT(e->ept,v1);
	um_vctovc(v1,linerec.ept);
	uio_label (fd1,fd2,e->label,dcount,pcount);
	dblk.par_ptr = *pcount;
	linerec.no_bptr = 0; 			/* no back pointers */
	linerec.no_prop = 1; 			/* properties */
	prop[0] = *dcount - 2;
	linerec.prop = &prop[0];
	uio_put_para(GLINE,&linerec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_dexit;	
	return (0);
}

/*********************************************************************
**    I_FUNCTION :  uio_plane(fd1,fd2,e,attr,dcount,pcount)
**      Output the plane information to
**			directory and parameter section record.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_plane(fd1,fd2,e,attr,dcount,pcount)
int	fd1, fd2;
struct NCL_nclpl_rec	*e;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */
	
{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igespln_rec	planerec;
	UU_REAL	v1[3],ds;
	int  prop[5];
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_plane"));	

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GPLANE;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
   dblk.use_flg = UIO_use_flg;
	dblk.form_no = 0;
	strcpy(dblk.label,"PLANE");
	dblk.subsno = e->subscr;       
	planerec.key = GPLANE;			/* entity type */
	UIO_CC_INTTOEXT(e->pt,planerec.disp_pt);
	v1[0] = e->nvec[0];
	v1[1] = e->nvec[1];
	v1[2] = e->nvec[2];
   ds    = e->pt[0]*v1[0] + e->pt[1]*v1[1] + e->pt[2]*v1[2];
	planerec.coef[0] = v1[0];
	planerec.coef[1] = v1[1];
	planerec.coef[2] = v1[2];
	UIO_LEN_INTTOEXT(ds,planerec.coef[3]);
   planerec.b_crv = 0;
/*
...assign just 0 to size_par to make sure that e.radius
...is not used since it was not set in older versions of IGES
...and can be bad number (fatal on alpha!)
	UIO_LEN_INTTOEXT(e->radius,v1[0]);
*/
	planerec.size_par = 0;
	uio_label (fd1,fd2,e->label,dcount,pcount);
	dblk.par_ptr = *pcount;
	planerec.no_bptr = 0;          /* no back pointers */
	planerec.no_prop = 1;          /* properties */
	prop[0] = *dcount - 2;
	planerec.prop = &prop[0];
	uio_put_para(GPLANE,&planerec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_dexit;	
	return (0);
}

/*********************************************************************
**    I_FUNCTION :  uio_arc()
**			Change the arc record into iges files standard then write
**			to the file.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_arc(fd1,fd2,e,attr,dcount,pcount)
int	fd1, fd2;
struct UM_circle_rec	*e;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;		/* parameter section sequence number */
	
{
	int	i;
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igesarc_rec	arcrec;
	UU_REAL	ctr[3], pts[3], pte[3], nvec[3];
	UU_REAL	ig_ctr[3], ig_pts[3], ig_pte[3];
	UU_REAL	yvec[3], tvec[3], stc_cc[3], ste_cc[3];
	UM_transf	transf, invtrf;
	UU_REAL	t[12];						/* transformation matrix */
	struct UM_evcrvout	evout;		/* curve evaluator record */
	int	mtrid;
	int  prop[5];
	UU_REAL	ang, del_ang;
	UU_LOGICAL	reverse;
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_arc"));	
	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GARC;
	strcpy(dblk.label,"ARC");
/*	strcpy(dblk.label,e->label);   7.20.92vp*/
	dblk.subsno = e->subscr;       /*7.20.92vp*/
	dblk.sub_swt = UIO_sub_swt;
   dblk.use_flg = UIO_use_flg;
/*
.....Flip the circle axis
.....if angle is negative
*/
	if (e->dang < 0.)
	{
		um_vctmsc(e->nvec,-1.,e->nvec);
		e->dang = -e->dang;
	}
	arcrec.key = GARC;
	um_ev3_circle(UM_POINT,(UU_REAL) 0.0,e,UM_DEFAULT_TF,&evout);
	UIO_CC_INTTOEXT(evout.cp,pts);
	um_ev3_circle(UM_POINT,(UU_REAL) 1.0,e,UM_DEFAULT_TF,&evout);
	UIO_CC_INTTOEXT(evout.cp,pte);
	UIO_CC_INTTOEXT(e->center,ctr);

			/* form the transformation matrix */
	um_cross(e->nvec,e->svec,yvec);
	for (i=0;i<3;i++)		tvec[i] = 0.0;
	um_vctovc(e->svec,transf[0]);
	um_vctovc(yvec,transf[1]);
	um_vctovc(e->nvec,transf[2]);
	um_vctovc(tvec,transf[3]);
	um_inverttf(transf,invtrf);

	um_cctmtf(pts,invtrf,ig_pts);
	um_cctmtf(pte,invtrf,ig_pte);
	um_cctmtf(ctr,invtrf,ig_ctr);

		/* check the arc direction on the definition plan, if not counterclockwise
			reverse the start and end point */
	um_vcmnvc(ig_pts,ig_ctr,stc_cc);
	um_vcmnvc(ig_pte,ig_ctr,ste_cc);
	nvec[0] = 0.0; 	nvec[1] = 0.0;		nvec[2] = 1.0;
	ang = um_angle2p(stc_cc,ste_cc,nvec);
	reverse = UU_FALSE;
	del_ang = fabs(e->dang);
	if ((ang-del_ang)>UM_FUZZ)
	{
		reverse = UU_TRUE;
	}
	if (reverse)
	{
		if(e->dang > 0.0)
		{
			for (i=0; i<3; i++)		/* flip the normal vector */
	  		 	transf[2][i] = transf[2][i] * -1.0;
			um_inverttf(transf,invtrf);
		}
		um_cctmtf(pte,invtrf,ig_pts);
		um_cctmtf(pts,invtrf,ig_pte);
		um_cctmtf(ctr,invtrf,ig_ctr);
	}
	for (i=0; i<2;	i++)
	{
		arcrec.spt[i] = ig_pts[i];
		arcrec.ept[i] = ig_pte[i];
		arcrec.cpt[i] = ig_ctr[i];
	}
	arcrec.zt = ig_ctr[2];
	arcrec.no_bptr = 0;
	if ((mtrid=uio_chk_matrix(transf,*dcount))<0)
	{
		uio_mtrtomtr(transf,t);		/* put two dimension matric to iges matrix */
		dblk.matrix_ptr = *dcount;

				/* set up the transformation matrix in the directory and parameter
					section */
		uio_tran(fd1,fd2,t,dcount,pcount);
	}
	else		/* same matrix exist before, put the pointer to it */
	{
		dblk.matrix_ptr = mtrid;
	}

	uio_label (fd1,fd2,e->label,dcount,pcount);
	dblk.par_ptr = *pcount;
	arcrec.no_bptr = 0;          /* no back pointers */
	arcrec.no_prop = 1;          /* properties */
	prop[0] = *dcount - 2;
	arcrec.prop = &prop[0];

	uio_put_para(GARC,&arcrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_dexit;	
	return (0);
}	

/*********************************************************************
**    I_FUNCTION :  uio_conic_arc()
**			Change the conic_arc record into iges files standard then write
**			to the file.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_conic_arc(fd1,fd2,e,attr,dcount,pcount)
int	fd1, fd2;
struct UM_conic_rec	*e;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;		/* parameter section sequence number */
	
{
	int	i;
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igescon_rec	conrec;
	int	mtrid;
	int  prop[5];
	UU_REAL	pts[3], pte[3];
	UU_REAL	t[12];
	struct UM_evcrvout	evout;		/* curve evaluator record */
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_conic_arc,a,b=%g,%g",
	e->invariants[0],e->invariants[1]));	

/*
	if (e->type != UM_ELLIPSE)
	{
  		uig_str_out("Parabola and Hyperbola are not implemented yet", UU_TRUE);
		return;
	}
*/

	uio_init_dir(attr,&dblk);
	dblk.rel_type = GCONIC;
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	strcpy(dblk.label,"CONICARC");
/*	strcpy(dblk.label,e->label);   7.20.92vp*/
	dblk.subsno = e->subscr;       /*7.20.92vp*/
	dblk.sub_swt = UIO_sub_swt;
   dblk.use_flg = UIO_use_flg;

	conrec.key = GCONIC;
	um_val4_conic(UM_POINT,e->t0,e,&evout);	/* find start point on the curve */
	UIO_CC_INTTOEXT(evout.cp,pts);
	um_val4_conic(UM_POINT,e->t1,e,&evout);	/* find end point on the curve */
	UIO_CC_INTTOEXT(evout.cp,pte);
	for (i=0;i<2;i++)
	{
		conrec.spt[i] = pts[i];
		conrec.ept[i] = pte[i];
	}
	conrec.zt = pts[2];
	uio_get_coeff(e,&conrec);			/* calculate the conic coefficients */
	UIO_CC_INTTOEXT(e->tfmat[3],e->tfmat[3]);
	if ((mtrid=uio_chk_matrix(e->tfmat,*dcount))<0)
	{
		uio_mtrtomtr(e->tfmat,t);	/* put two dimension matrix to iges matrix */
		dblk.matrix_ptr = *dcount;
		uio_tran(fd1,fd2,t,dcount,pcount);
	}
	else		/* same matrix exist before, put the pointer to it */
	{
		dblk.matrix_ptr = mtrid;
	}
	uio_label (fd1,fd2,e->label,dcount,pcount);
	dblk.par_ptr = *pcount;
	conrec.no_bptr = 0;          /* no back pointers */
	conrec.no_prop = 1;          /* properties */
	prop[0] = *dcount - 2;
	conrec.prop = &prop[0];

	uio_put_para(GCONIC,&conrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_dexit;	
	return (0);
}

/*********************************************************************
**    I_FUNCTION :  uio_get_coeff(e,conrec)
**       Given a conic entity, evaluate the six coefficients according
**			to the entity type and the invariants.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_get_coeff(e,conrec)			
struct UM_conic_rec	*e;
struct IG_igescon_rec	*conrec;
{

	UU_REAL	a, b;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	UIO_LEN_INTTOEXT(e->invariants[0],a);
	UIO_LEN_INTTOEXT(e->invariants[1],b);
	conrec->b = 0.0;
	conrec->d = 0.0;
	conrec->e = 0.0;
	switch(e->type)
	{
		case	UM_HYPERBOLA:	/* x**2/a**2 - y**2/b**2 = 1 	*/
			conrec->a = b*b;
			conrec->c = -a*a;
			conrec->f = -a*a*b*b;
			break;

		case	UM_PARABOLA:	/* x**2 = -4py			*/
/*       conrec->a = 1;
         conrec->c = 4*a;     
			conrec->f = 0;   901vp replaced by following  */
			conrec->a = 0;
         conrec->c = 1;       
         conrec->d = -1/a;       
			conrec->f = 0;
			break;

		case	UM_ELLIPSE:		/* x**2/a**2 + y**2/b**2 = 1		*/
			conrec->a = b*b;
			conrec->c = a*a;
			conrec->f = -a*a*b*b;
			break;

		default:
			;	
	}
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uio_rbspl(fd1,fd2,e,attr,dcount,pcount)
**				Map an UNICAD rational bspline to a IGES rational bspline.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_rbspl(fd1,fd2,e,attr,dcount,pcount)
int	fd1, fd2;
struct UM_rbsplcrv_rec	*e;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */
{

	int i, j, num;
	int  prop[5];
	UU_REAL pt_cc[3], pt[3], *p;
	struct IG_igesrspl_rec rsplrec;	/* IGES rational bspline parameter record */
	struct  dir_rec dblk;		/* directory record */
	UU_LOGICAL	blanked;
	int status;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/


	uu_denter(UU_MTRC,(us,"enter uio_rbspl"));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GRSPLINE;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
   dblk.use_flg = UIO_use_flg;
	strcpy(dblk.label,"RSPLINE");
/*	strcpy(dblk.label,e->label);   7.20.92vp*/
	dblk.subsno = e->subscr;       /*7.20.92vp*/
/*
.....FOr all extended curves evaluate the curve and interpolate the points to
.....form a new spline
*/
	if((e->t0<e->t[0])||(e->t1>e->t[e->no_t-1]))
	{
		status = uig_ncl_extended_rbsp(e,&rsplrec);
	}
	else
	{

		rsplrec.key = GRSPLINE;
		rsplrec.planar = e->planar;	
		rsplrec.open = (e->open)? 0 : 1;
		rsplrec.degree = e->k - 1;				/* degree of the bspline curve  */
		rsplrec.indx = e->no_pt - 1;		/* upper index of sum 			  */

		rsplrec.type = 0;				/* rational bspline  */
		rsplrec.period = 0;			/* non- periodic */
		rsplrec.no_rpara = 1;
		rsplrec.rpara = (struct IG_rpara_rec *)uu_toolmalloc(sizeof(struct IG_rpara_rec));
		rsplrec.rpara->t0 = e->t0;		/* starting value    */
		rsplrec.rpara->t1 = e->t1;		/* ending value      */
		rsplrec.rpara->norm[0] = 0.0;
		rsplrec.rpara->norm[1] = 0.0;
		rsplrec.rpara->norm[2] = 1.0;

		num = e->no_t;
		rsplrec.no_t = num;
		rsplrec.t = (UU_REAL *)uu_toolmalloc(num*sizeof(UU_REAL));
		for (i=0; i<num; i++)
		{
			rsplrec.t[i] = e->t[i];		/* knot values 		*/
		}

		num = e->no_pt;
		rsplrec.no_w = num;
		rsplrec.w = (UU_REAL *)uu_toolmalloc(num*sizeof(UU_REAL));
		if (e->no_wt > 0 && e->wt != UU_NULL)
		{
			for (i=0; i<num; i++) rsplrec.w[i] = e->wt[i];	/* weights */
		}
		else
		{
			for (i=0; i<num; i++) rsplrec.w[i] = 1.;
		}

		num = e->no_pt;
		rsplrec.no_pt3 = num;
		rsplrec.pt3 = (UU_REAL *)uu_toolmalloc(3*num*sizeof(UU_REAL));
		p = e->pt;
		for (i = 0, j = 0; i < num; i++, j = j+3)
		{
			pt_cc[0] = p[j];
			pt_cc[1] = p[j + 1];
			pt_cc[2] = p[j + 2];
			UIO_CC_INTTOEXT(pt_cc,pt);
			um_vctovc(pt, &rsplrec.pt3[j]);		/* control points	 */
		}
	}
	uio_label (fd1,fd2,e->label,dcount,pcount);
	dblk.par_ptr = *pcount;
	rsplrec.no_bptr = 0;          /* no back pointers */
	rsplrec.no_prop = 1;          /* properties */
	prop[0] = *dcount - 2;
	rsplrec.prop = &prop[0];

	uio_put_para(GRSPLINE,&rsplrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(rsplrec.t);
	uu_toolfree(rsplrec.w);
	uu_toolfree(rsplrec.pt3);
	uu_toolfree(rsplrec.rpara);  
	uu_dexit;
	return (0);
}	

/*********************************************************************
**    I_FUNCTION     :  uio_nclcrv (fd1,fd2,e,attr,dcount,pcount)
**				Map an NCL Bezier curve to a IGES rational bspline.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_nclcrv(fd1,fd2,e,attr,dcount,pcount)
int	fd1, fd2;
struct NCL_curve_rec	*e;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;  	/* parameter section sequence number */
{

	int i, j, num, k, m, ns, nt, n, lst;
	int  prop[5];
	UU_REAL pt_cc[3], pt[3], rho, dlt[3];
	struct IG_igesrspl_rec rsplrec;	/* IGES rational bspline parameter record */
	struct  dir_rec dblk;      		/* directory record */
	struct  NCL_segment_rec *segment;		/* NCL curve segment record */
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/


	uu_denter(UU_MTRC,(us,"enter uio_nclcrv"));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GRSPLINE;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
   dblk.use_flg = UIO_use_flg;
	strcpy(dblk.label,"RSPLINE");   
	dblk.subsno = e->subscr;       

	rsplrec.key = GRSPLINE;
	rsplrec.planar = 0;	
	rsplrec.open = e->closdinu;
	m     = 3;
	ns    = e->no_segment;
	k     = (ns - 1)*m;
	rsplrec.degree = m;			      	/* degree of the bspline curve  */
	rsplrec.indx = k;	               	/* upper index of sum 			  */

	rsplrec.type = 0;	         			/* rational bspline  */
	rsplrec.period = 0;		         	/* non- periodic */
	rsplrec.no_rpara = 1;

   segment = e->segment;

/* Calculate Bsplane points and store then in IGES record  */

	rsplrec.pt3 = (UU_REAL *)uu_toolmalloc(3*(k+1)*sizeof(UU_REAL));

   num   = e->no_param;
	j     = 0;
	for (i=0; i<num; i++)
	{
		if (i == 0)
		{
		   for (n=0; n<3; n++)
			{
				pt[n]  = segment->point[n];
		      dlt[n] = segment->delta[n];
         }
         UIO_CC_INTTOEXT(pt,pt);
         UIO_CC_INTTOEXT(dlt,dlt);
		}
      rho = segment->rho;  
      um_vctovc(pt, &rsplrec.pt3[j]);	   	/* I control point	 */
		pt[0] = pt[0] + dlt[0]; 
		pt[1] = pt[1] + dlt[1]; 
		pt[2] = pt[2] + dlt[2]; 
		j     = j + 3;
      um_vctovc(pt, &rsplrec.pt3[j]);	   	/* II control point	 */

/*  Get next segment point  */

      segment++;
		for (n=0; n<3; n++)
		{
			pt[n]  = segment->point[n];
			dlt[n] = segment->delta[n];
		}
      UIO_CC_INTTOEXT(pt,pt);
      UIO_CC_INTTOEXT(dlt,dlt);
		pt_cc[0] = pt[0] - rho*dlt[0]; 
		pt_cc[1] = pt[1] - rho*dlt[1]; 
		pt_cc[2] = pt[2] - rho*dlt[2]; 
		j     = j + 3;
      um_vctovc(pt_cc, &rsplrec.pt3[j]);		/* III control point	 */
		j     = j + 3;

/*  For last segment store 4-th point  */

      if (i == num-1)
		{
         um_vctovc(pt, &rsplrec.pt3[j]);	/* last control point	 */
		}
	}

/* Generate weights and knots for Bsplane record */

   nt     = ns * m + 2;
	rsplrec.t = (UU_REAL *)uu_toolmalloc(nt*sizeof(UU_REAL));
	rsplrec.no_t = nt;
	rsplrec.no_w = k + 1;
	rsplrec.no_pt3 = k + 1;

	rsplrec.w = (UU_REAL *)uu_toolmalloc((k+1)*sizeof(UU_REAL));
	for (i=0; i<=k; i++) rsplrec.w[i] = 1.0;

	for (j=0; j<4; j++) rsplrec.t[j] = 0.0;
   j     = 3;
	lst   = 3;
	for (i=0; i<num; i++)
	{
		if (i == num-1) lst = 4;
		for (n=0; n<lst; n++)
		{
			j   = j + 1;
	      rsplrec.t[j] = e->param[i];
      }
   }

	rsplrec.rpara = (struct IG_rpara_rec *)uu_toolmalloc(sizeof(struct IG_rpara_rec));
	rsplrec.rpara->t0 = e->t0;              		/* starting value    */
	rsplrec.rpara->t1 = e->t1;		/* ending value      */
	rsplrec.rpara->norm[0] = 0.0;
	rsplrec.rpara->norm[1] = 0.0;
	rsplrec.rpara->norm[2] = 1.0;

	uio_label (fd1,fd2,e->label,dcount,pcount);
	dblk.par_ptr = *pcount;
	rsplrec.no_bptr = 0;          /* no back pointers */
	rsplrec.no_prop = 1;          /* properties */
	prop[0] = *dcount - 2;
	rsplrec.prop = &prop[0];

	uio_put_para(GRSPLINE,&rsplrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);

	uu_toolfree(rsplrec.t);
	uu_toolfree(rsplrec.w);
	uu_toolfree(rsplrec.pt3);  
	uu_toolfree(rsplrec.rpara);  
	uu_dexit;
	return (0);
}	

/*********************************************************************
**    I_FUNCTION     :  uio_nclpat (fd1,fd2,e,attr,dcount,pcount)
**				Map an NCL patern to an IGES copious data (polyline).
**          vp 5-may-93 logic changed to convert NCL paterns type 1
**          & type 2 to polylines Form # 2 or 3.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_nclpat(fd1,fd2,e,attr,dcount,pcount)
int	fd1, fd2;
struct NCL_patern_rec	*e;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;  	/* parameter section sequence number */
{

	int i, j, num, pntyp;
	int  prop[5];
	UU_REAL ptp[3];
	struct IG_poly3d_rec polre3;     	/* IGES copious data record */
	struct IG_poly6d_rec polre6;     	/* IGES copious data record */
	struct IG_pt6_rec *ptr6;           	/* IGES point record */
	struct  dir_rec dblk;      	     	/* directory record */
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/


	uu_denter(UU_MTRC,(us,"enter uio_nclpat"));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GPOLY;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
   dblk.use_flg = UIO_use_flg;
	strcpy(dblk.label,"POLYLINE");   
	dblk.subsno = e->subscr;       

	pntyp  = e->pntype;
	num    = e->no_patpnt;

/*
...Patern of points convert to copious data type 2
*/
   if (pntyp == 1)
   { 
	   dblk.form_no = 2;
    	polre3.key = GPOLY;
   	polre3.type = 2;	

/* Get patern points and store then in IGES record  */

   	polre3.pt3 = (UU_REAL *)uu_toolmalloc(num*sizeof(UU_REAL));
      num   = num / 3;
   	polre3.no_pt3 = num;
   	j     = 0;
   	for (i=0; i<num; i++)
      {
   		ptp[0] = e->patpnt[j];
	   	ptp[1] = e->patpnt[j+1];
	   	ptp[2] = e->patpnt[j+2];
         UIO_CC_INTTOEXT(ptp,ptp);
         um_vctovc(ptp, &polre3.pt3[j]);	   	/* patern point	 */
		   j     = j + 3;
	   }

		uio_label (fd1,fd2,e->label,dcount,pcount);
		dblk.par_ptr = *pcount;
		polre3.no_bptr = 0;          /* no back pointers */
		polre3.no_prop = 1;          /* properties */
		prop[0] = *dcount - 2;
		polre3.prop = &prop[0];

   	uio_put_para(GPOLY3D,&polre3,fd2,pcount,*dcount);
   	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);

   	uu_toolfree(polre3.pt3);  
   }
   else
   {
	   dblk.form_no = 3;
    	polre6.key = GPOLY;
   	polre6.type = 3;	
      num   = num / 6;

	   polre6.pt6 = (struct IG_pt6_rec *)uu_toolmalloc
                                    (num*sizeof(struct IG_pt6_rec));
/*
...Get patern points and store then in IGES record
*/
   	polre6.no_pt6 = num;
   	j     = 0;
      ptr6 = polre6.pt6;
   	for (i=0; i<num; i++)
      {
     		ptp[0] = e->patpnt[j];
	   	ptp[1] = e->patpnt[j+1];
	   	ptp[2] = e->patpnt[j+2]; 
         UIO_CC_INTTOEXT(ptp,ptp);
         um_vctovc(ptp, ptr6->pt);	   	/* point of pointvector */
     		ptp[0] = e->patpnt[j+3];
	   	ptp[1] = e->patpnt[j+4];
	   	ptp[2] = e->patpnt[j+5]; 
         UIO_CC_INTTOEXT(ptp,ptp);
         um_vctovc(ptp, ptr6->vec);	  	/* vector of pointvector */
		   j     = j + 6;
         ptr6++;
	   }

      uio_label (fd1,fd2,e->label,dcount,pcount);
      dblk.par_ptr = *pcount;
      polre6.no_bptr = 0;          /* no back pointers */
      polre6.no_prop = 1;          /* properties */
      prop[0] = *dcount - 2;
      polre6.prop = &prop[0];

   	uio_put_para(GPOLY6D,&polre6,fd2,pcount,*dcount);
   	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);

   	uu_toolfree(polre6.pt6);  
   }

	uu_dexit;
	return (0);
}	

/*********************************************************************
**    I_FUNCTION     :  uio_agcrv_rbspl(fd1,fd2,agcrv,attr,dcount,pcount)
**				Map a UNICAD AG rational bspline to a IGES rational bspline.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_agcrv_rbspl(fd1,fd2,agcrv,attr,dcount,pcount)
int	fd1, fd2;
struct UM_agcrv_rec	*agcrv;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */

{
	int status;
	AG_CURVEP cv;
	AG_CURVEP agcv, ag_bld_crv();
	AG_SPLINEP bs, bs_prev, bs_next;
	struct UC_entitydatabag *unicrv;
	struct UM_agcrv_rec crv;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter uig_agcrv_rbspl"));

	status = UU_SUCCESS;

	unicrv = (struct UC_entitydatabag *)
					uu_toolmalloc(sizeof(struct UC_entitydatabag));

	cv = (AG_CURVEP) agcrv->crvaddr;

	agcv = ag_bld_crv(3);
	um_agcrv_setup_data(UM_AGCRV_REL, &crv, sizeof(crv));
	crv.crvaddr = (int) agcv;
	bs = cv->bs0;
	do
	{
		bs_prev = bs->prev;
		bs_next = bs->next;
		ag_crv_app_bs(agcv, bs);
		status = umi_agcv_to_unicrv(crv.crvaddr, unicrv);
		if (status == UU_FAILURE)
			status = um_agcrv_to_unirbsc(&crv, unicrv);
		unicrv->key = agcrv->key;
		if (status != UU_FAILURE)
			uio_geometry(fd1,fd2,unicrv,attr,dcount,pcount);
		agcv->bs0 = NULL;
		agcv->bs = NULL;
		agcv->nbs = 0;
		bs->prev = bs_prev;
		bs->next = bs_next;
		bs = bs->next;
	} while ((bs != NULL) && (bs != cv->bs0));

	agcv->bs0 = NULL;
	agcv->bs = NULL;
	ag_db_crv(&agcv);

done:;
	uu_toolfree(unicrv);
	uu_dexit;
	return (0);
}	

/*********************************************************************
**    I_FUNCTION :  uio_comp_crv(fd1,fd2,e,attr,dcount,pcount)
**      Output the entities for the composite curve entity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_comp_crv(fd1,fd2,e_comp,attr_comp,dcount,pcount)
int	fd1, fd2;
struct UM_compcrv_rec	*e_comp;
struct UR_attr 	*attr_comp;
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */

{
	struct  dir_rec dblk;				/* directory record */
	struct  IG_igescomp_rec	comprec;	/* iges group record */
	struct NCL_fixed_databag e;
	struct UR_attr 	attr;
	struct UM_rbsplcrv_rec	*rcrv;
	int prop[5];
	int i, size;
	int 					num_ents; 
   int savsub;
	UU_KEY_ID      	*ent_ptr;
	UU_LOGICAL	blanked;
	struct UC_entitydatabag *unicrv;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_comp_crv"));	
   savsub = UIO_sub_swt;
   UIO_sub_swt = 1;

	num_ents = e_comp->no_cid;
	ent_ptr = (UU_KEY_ID *) uu_toolmalloc((num_ents + 10)
																*sizeof(UU_KEY_ID));
	for(i=0;i<num_ents;i++)
	{
		e.key = e_comp->cid[i].crvid;
		ur_retrieve_data_relnum(e.key, &e.rel_num);
		rcrv = (struct UM_rbsplcrv_rec *) &e;
		if(uio_getdata(&e, &attr)) 
		{
			size = 0;
			if (e_comp->cid[i].reverse)
			{
				if (e.rel_num == NCL_CURVE_REL)
				{
					size = um_curve_size (&e);
					um_allocate_curve (&rcrv,size);
					um_c7_frmnclcrv (&e,rcrv);
					rcrv->key = e.key;
				}
				uc_reverse_curve(rcrv);
			}
			unicrv = (struct UC_entitydatabag *) rcrv;
			uio_geometry(fd1,fd2,unicrv,attr_comp,dcount,pcount);
			ent_ptr[i] = *dcount - 2;
			if (size > 0) uu_toolfree (rcrv);
		}
	}
   UIO_sub_swt = savsub;

	/* now output composite curve record */

	uio_init_dir(attr_comp,&dblk);
	uio_check_view(fd1, fd2, e_comp->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e_comp->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GCOMPOSITE;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
   dblk.use_flg = UIO_use_flg;
	dblk.form_no = 0;
	strcpy(dblk.label,"COMPOSIT");
	dblk.subsno = e_comp->subscr;       
	comprec.num = num_ents;
	comprec.no_cid = num_ents;
	comprec.cid = &ent_ptr[0];

	comprec.key = GCOMPOSITE;
								/* no back pointers */
	uio_label (fd1,fd2,e_comp->label,dcount,pcount);
	dblk.par_ptr = *pcount;
	comprec.no_bptr = 0;          /* no back pointers */
	comprec.no_prop = 1;          /* properties */
	prop[0] = *dcount - 2;
	comprec.prop = &prop[0];

	uio_put_para(GCOMPOSITE,&comprec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(ent_ptr);
	uu_dexit;	
	return (0);
}	

/*********************************************************************
**    I_FUNCTION :  uio_group(fd1,fd2,e,attr,dcount,pcount)
**      Output the entities for the group entity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_group(fd1,fd2,e_group,attr_group,dcount,pcount)
int	fd1, fd2;
struct UM_grouper_rec	*e_group;
struct UR_attr 	*attr_group;
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */
{

	struct  dir_rec dblk;				/* directory record */
	struct  IG_igesgrp_rec	grprec;	/* iges group record */
	struct UC_entitydatabag e;
	struct UR_attr 	attr;
	int 					i;
	int 					num_ents; 
	UU_KEY_ID      	*ent_ptr;
	UU_LOGICAL	blanked;
   int savsub;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_group"));	

	/* first output subordinate entities */

   savsub = UIO_sub_swt;
	UIO_sub_swt = 2;
	num_ents = 0;
	ent_ptr = (UU_KEY_ID *) uu_toolmalloc((e_group->no_member + 10)
																*sizeof(UU_KEY_ID));
	for(i=0;i<e_group->no_member;i++)
	{
		e.key = e_group->member[i];
		ur_retrieve_data_relnum(e.key, &e.rel_num);
		if(uio_getdata(&e, &attr))
			uio_geometry(fd1,fd2,&e,&attr,dcount,pcount);
	}
   UIO_sub_swt = savsub;

	/* now output group record */

	uio_init_dir(attr_group,&dblk);
	uio_check_view(fd1, fd2, e_group->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e_group->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GGROUP;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
	dblk.form_no = 7;
	strcpy(dblk.label,"GROUP");
	dblk.subsno = 0;       /*7.20.92vp*/
	grprec.num = num_ents;
	grprec.no_cid = num_ents;
	grprec.cid = &ent_ptr[0];

	grprec.key = GGROUP;
								/* no back pointers */
	grprec.no_bptr = 0;
								/* no properties */
	grprec.no_prop = 0;
	uio_put_para(GGROUP,&grprec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(ent_ptr);
	uu_dexit;	
	return (0);
}	

/*********************************************************************
**    I_FUNCTION :  uio_check_view(key, dblk)
**      Check if entity view dependent1
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_check_view(fd1, fd2, key, dblk, dcount, pcount)
int	fd1, fd2;
UU_KEY_ID key;
struct  dir_rec *dblk;				/* directory record */
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */
{
	struct IG_igesvie_rec iges_view;
	struct  dir_rec dblk_view;				/* directory record */
	UV_view view;
	UU_KEY_ID view_key;
	char vname[15];
	int i;
	UU_REAL vec[3], t[12], xmax, ymax, pln[3];

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_check_view dcnt %d pcnt %d",
								*dcount, *pcount));	

	/* check for a active view key */

	if(UIO_drawing) goto fexit;

	ur_retrieve_view_key(key, &view_key);
	if(view_key == 0)
	{
		uu_dexit;
		return (0);
	}
	else
	{
		/* check if view already output to IGES file */
		for(i=0;i<no_of_views;i++)
		{
			if(view_keys[i][0] == view_key)
			{
				dblk->view_ptr = view_keys[i][1];
				uu_dexit;
				return (0);
			}
		}
		view.key = view_key;
		view.rel_num = UV_VIEW_REL;
		ur_retrieve_data(&view, sizeof(view));

		/* create an IGES transformation */

		uio_tran_dir(&dblk_view);
		dblk_view.matrix_ptr = *dcount;

		um_vctovc(view.cur_ref_pt, vec);
		UIO_CC_INTTOEXT(vec,vec);
		uig_set_trans(4, vec, t);
		um_vctovc(view.cur_pln_norm, vec);
		UIO_CC_INTTOEXT(vec,vec);
		uig_set_trans(3, vec, t);
		um_vctovc(view.cur_up_vect, vec);
		UIO_CC_INTTOEXT(vec,vec);
		uig_set_trans(2, vec, t);
		um_cross(view.cur_up_vect, view.cur_pln_norm, vec);
		UIO_CC_INTTOEXT(vec,vec);
		uig_set_trans(1, vec, t);

		uio_tran(fd1,fd2,t,dcount,pcount);

		/* now complete view */

		view_keys[no_of_views][0] = view_key;

		dblk_view.rel_type = VIEW;
		dblk_view.sub_swt = 1;
		dblk_view.par_cnt = 1;
		strcpy (vname,view.name);
		vname[8] = '\0';
		strcpy(dblk_view.label,vname);
	   dblk_view.subsno = 0;       /*7.20.92vp*/

		iges_view.key = VIEW;
		iges_view.scale = 1.0;

/*  9.12.92vp changed to save aperture for back conv.

		iges_view.left = 0;
		iges_view.top = 0;
		iges_view.right = 0;
		iges_view.bottom = 0;
		iges_view.back = 0;
		iges_view.front = 0;  */

		xmax = view.cur_aperture * .5;
      ymax = .7 * xmax;

/* XVMIN plane */
      pln[0] = -t[0];
      pln[1] = -t[4];
      pln[2] = -t[8];
		iges_view.left = *dcount;
      uio_view_vlpln (fd1,fd2,pln,xmax,&dblk_view,dcount,pcount);

/* XVMAX plane */
      pln[0] = t[0];
      pln[1] = t[4];
      pln[2] = t[8];
		iges_view.right = *dcount;
      uio_view_vlpln (fd1,fd2,pln,xmax,&dblk_view,dcount,pcount);

/* YVMIN plane */
      pln[0] = -t[1];
      pln[1] = -t[5];
      pln[2] = -t[9];
		iges_view.bottom = *dcount;
      uio_view_vlpln (fd1,fd2,pln,ymax,&dblk_view,dcount,pcount);

/* YVMAX plane */
      pln[0] = t[1];
      pln[1] = t[5];
      pln[2] = t[9];
		iges_view.top = *dcount;
      uio_view_vlpln (fd1,fd2,pln,ymax,&dblk_view,dcount,pcount);
		
		view_keys[no_of_views][1] = *dcount;
		dblk->view_ptr = view_keys[no_of_views][1];
		no_of_views++;

		iges_view.view_id = no_of_views;
		dblk_view.par_ptr = *pcount;
		iges_view.back = 0;
		iges_view.front = 0; 
									/* no back pointers */
		iges_view.no_bptr = 0;
									/* no properties */
		iges_view.no_prop = 0;

		uio_put_para(VIEW,&iges_view,fd2,pcount,*dcount);
		uio_attr_to_dir(fd1,&dblk_view,dcount,*pcount);
	}
fexit:;
	uu_dexit;	
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_set_trans(type, vec, t)
**       Load the IGES transformation components specified by type.
**    PARAMETERS   
**       INPUT  : 
**          type                          compoent required
**                                           1 = x-axis
**                                           2 = y-axis
**                                           3 = z-axis
**                                           4 = translation vector
**          vec                           required component unit vector
**       OUTPUT :  
**          t                             IGES transformation matrix
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_set_trans(type, vec, t)
int      type;
UU_REAL  vec[3];
UU_REAL  t[12];

{
   int i,j;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   j = type - 1;
   i = 0;
   while(i < 3)
   {
		t[j] = vec[i];
      i++;
      j = j + 4;
   }
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uio_nclvec (fd1,fd2,e,attr,dcount,pcount)
**				Map an NCL vector to an IGES copious data (polyline).
**          vp 5-may-93 logic changed to convert NCL pointvector or
**          vector to the same IGES copious data Form 3 record.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_nclvec (fd1,fd2,e,attr,dcount,pcount)
int	fd1, fd2;
struct NCL_vector_rec	*e;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;  	/* parameter section sequence number */
{

	int prop[5];
	UU_REAL ptp[3];
	struct  NCL_nclpv_rec *ptvec;   		/* NCL pointvector record */
	struct  IG_poly6d_rec  polrec;     	/* IGES copious data record */
	struct  dir_rec dblk;      	     	/* directory record */
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/


	uu_denter(UU_MTRC,(us,"enter uio_nclvec"));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, e->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GPOLY;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
   dblk.use_flg = UIO_use_flg;
	dblk.form_no = 3;
	strcpy(dblk.label,"POLY6");   
	dblk.subsno = e->subscr;       

	polrec.key = GPOLY;
	polrec.type = 3;	
	polrec.no_pt6 = 1;

/* Get vector and store then in IGES record  */

	polrec.pt6 = (struct IG_pt6_rec *)uu_toolmalloc(sizeof(struct IG_pt6_rec));

/* vp 5-may-93 added NCL pointvector entity  */

   if (e->rel_num == NCL_POINTVEC_REL)
	{
		 ptvec  = (struct NCL_nclpv_rec *) e;
       UIO_CC_INTTOEXT(ptvec->pt,ptp);
       um_vctovc(ptp, polrec.pt6->pt);	   	/* point of pointvector */
       UIO_CC_INTTOEXT(ptvec->ve,ptp);
       um_vctovc(ptp, polrec.pt6->vec);	   /* vector */
	}

/*           NCL vector entity               */

	else
	{
		 polrec.pt6->pt[0] = 0.0;
		 polrec.pt6->pt[1] = 0.0;
		 polrec.pt6->pt[2] = 0.0;
       UIO_CC_INTTOEXT(e->vec,ptp);
       um_vctovc(ptp, polrec.pt6->vec);	   /* vector */
	}

	uio_label (fd1,fd2,e->label,dcount,pcount);
	dblk.par_ptr = *pcount;
	polrec.no_bptr = 0;          /* no back pointers */
	polrec.no_prop = 1;          /* properties */
	prop[0] = *dcount - 2;
	polrec.prop = &prop[0];

	uio_put_para(GPOLY6D,&polrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);

	uu_toolfree(polrec.pt6);  
	uu_dexit;
	return (0);
}	

/*********************************************************************
**    I_FUNCTION :  uio_view_vlpln(fd1,fd2,vpln,dis,labl,dcount,pcount)
**      Output the plane information to
**			directory and parameter section record.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_view_vlpln (fd1,fd2,vpln,dis,dblk,dcount,pcount)
int	fd1, fd2;
UU_REAL vpln[3],dis;
struct  dir_rec *dblk;				/* directory record */
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */
	
{
	struct  dir_rec dblk_vlpln;		/* plane directory record */
	struct  IG_igespln_rec	planerec;
	UU_REAL	pt[3];

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_view_vlpln"));	

	dblk_vlpln.matrix_ptr = 0;
	dblk_vlpln.level = 1;
	dblk_vlpln.blank = 1;
	dblk_vlpln.sub_swt = 1;
	dblk_vlpln.use_flg = 0;
	dblk_vlpln.hier = 1;
	dblk_vlpln.rel_type = GPLANE;
	dblk_vlpln.par_ptr = *pcount;
	dblk_vlpln.view_ptr = 0;       
	dblk_vlpln.line_font = 1;
	dblk_vlpln.line_wt = 2;
	dblk_vlpln.pen_no = 0;
	dblk_vlpln.par_cnt = 0;
	dblk_vlpln.form_no = 1;
	dblk_vlpln.label[0] = '\0';   
	dblk_vlpln.drec_num = 0;       
	dblk_vlpln.subsno = 0;       
	planerec.key = GPLANE;			/* entity type */
	pt[0] = vpln[0] * dis;
	pt[1] = vpln[1] * dis;
	pt[2] = vpln[2] * dis;
	UIO_CC_INTTOEXT(pt,planerec.disp_pt);
	planerec.coef[0] = vpln[0];
	planerec.coef[1] = vpln[1];
	planerec.coef[2] = vpln[2];
	UIO_LEN_INTTOEXT(dis,planerec.coef[3]);
   planerec.b_crv = 0;
	UIO_LEN_INTTOEXT(0.,planerec.size_par)
	planerec.no_bptr = 0; 			/* no back pointers */
	planerec.no_prop = 0; 			/* no properties */
	uio_put_para(GPLANE,&planerec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk_vlpln,dcount,*pcount);
	uu_dexit;	
	return (0);
}
/*********************************************************************
**    I_FUNCTION :  uio_polyline(fd1,fd2,poly,attr,dcount,pcount)
**       Map Unicad's polyline entity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uio_polyline(fd1,fd2,poly,attr,dcount,pcount)
int	fd1, fd2;
struct UM_polyline_rec   *poly;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;		/* parameter section sequence number */
{
/*
.....if unicad polyline is a uv curve on surface map it
.....to iges 2D polyline
*/
	if (UIO_uvcurve)
		uio_uv_poly2(fd1,fd2,poly,attr,dcount,pcount);
	else
		uio_map_poly3(fd1,fd2,poly,attr,dcount,pcount);
	return (0);
}

/*********************************************************************
**    I_FUNCTION :  uio_uv_poly2(fd1,fd2,poly,attr,dcount,pcount)
**       Map Unicad's polyline entity defining uv curve on surface.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uio_uv_poly2(fd1,fd2,poly,attr,dcount,pcount)
int	fd1, fd2;
struct UM_polyline_rec   *poly;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;		/* parameter section sequence number */
{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_poly2d_rec	polyrec;
	UU_REAL	*ptptr;
	int	i, j, n, num, prop[5];
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter uio_map_poly3"));

	uio_init_dir(attr,&dblk);
	ur_retrieve_blanked(poly->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GPOLY;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
	dblk.use_flg = 0;
	dblk.form_no = 11;
	strcpy(dblk.label,"POLY2");    
/*	strcpy(dblk.label,poly->label);    721vp*/
	dblk.subsno = poly->subscr;       /*721vp*/

	ptptr = &poly->pt[0];
	polyrec.key = GPOLY;
	polyrec.type = 1;
	polyrec.zt = poly->pt[2];
	num = poly->no_pt;
	polyrec.num = num;
	polyrec.no_pt2 = num;
	polyrec.pt2 = (UU_REAL *) uu_toolmalloc(2*num * sizeof(UU_REAL));
	for(i=0, j=0, n=0; i<num; i++, n++)
	{
		UIO_LEN_INTTOEXT(ptptr[n++],polyrec.pt2[j++]);		
		UIO_LEN_INTTOEXT(ptptr[n++],polyrec.pt2[j++]);		
	}
/*
.....put label in properties
*/
	uio_label (fd1,fd2,poly->label,dcount,pcount);
	polyrec.no_bptr = 0;
	dblk.par_ptr = *pcount;
	polyrec.no_prop = 1;
	prop[0] = *dcount - 2;
	polyrec.prop = &prop[0];

	uio_put_para(GPOLY,&polyrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(polyrec.pt2);
	uu_dexit;
	return (0);
}

/*********************************************************************
**    I_FUNCTION :  uio_cvonsf1(fd1,fd2,cvsf,attr,dcount,pcount)
**       Map NCL cvonsf entity defining uv curve on surface.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uio_cvonsf1(fd1,fd2,cvsf,attr,dcount,pcount)
int	fd1, fd2;
struct UM_uvcvonsf_rec   *cvsf;
struct UR_attr 	*attr;
int	*dcount;		/* directory section sequence number */
int	*pcount;		/* parameter section sequence number */
{
	int i, ns, savsub, savunit, savuse, mtrid;
	UU_KEY_ID bskey;
	struct UC_entitydatabag sf1;
	struct UM_rbsplcrv_rec cv1;
	struct UR_attr attr1;
	struct IG_igescvsf_rec igcvsf;
	surf_list *bslst;
	struct  dir_rec dblk;
	UU_LOGICAL  blanked;
	UM_transf tf;
	UU_REAL t1[12];
	int bsdir, cvdir, status, prop[5];
	struct UC_entitydatabag *unicrv;

	uu_denter(UU_MTRC,(us,"enter uio_cvonsf1"));

	bsdir = 0;
	savsub = UIO_sub_swt;
	savunit = uio_units;
	savuse = UIO_use_flg;
	bskey = cvsf->bskey;
/*
.....check if base surface is translated already
*/
	ns = UU_LIST_LENGTH(UIO_surf_list);
	bslst = (surf_list *) UU_LIST_ARRAY(UIO_surf_list);	
	i  = 0;
	while (i<ns && bskey != bslst->bskey)
	{
		bslst++; i++;
	}
	if (i<ns && ns>0)
	{
		bsdir = bslst->drec;
	}
	else
/*
.....not yet - do it now
*/
	{
		sf1.key = bskey;
		if (uio_getdata(&sf1, &attr1))
		{
			uio_geometry(fd1,fd2,&sf1,&attr1,dcount,pcount);
			bsdir = *dcount - 2;
		}
	}
/*
.....translate rb spline on surface
*/
	if (bsdir != 0) 
	{
		ncl_cp_struct_uvcv_rbcv1 (cvsf,&cv1);
		cv1.planar = cvsf->planar;
		UIO_sub_swt = 1;
		uio_units = 1.0;
		UIO_use_flg = 5;
		UIO_uvcurve = 1;
		unicrv = (struct UC_entitydatabag *) &cv1;
		status = uio_geometry (fd1,fd2,unicrv,attr,dcount,pcount);
		UIO_uvcurve = 0;
		cvdir = *dcount - 2;
	}
/*
.....create cv_on_sf iges data record
*/
	uio_units = savunit;
   UIO_sub_swt = savsub;
	UIO_use_flg = savuse;

   uio_init_dir(attr,&dblk);
   uio_check_view(fd1, fd2, cvsf->key, &dblk, dcount, pcount);
   ur_retrieve_blanked(cvsf->key, &blanked);
   dblk.blank = blanked? 1 : 0;
   dblk.rel_type = GCRVONSRF;
   dblk.sub_swt = UIO_sub_swt;
   strcpy(dblk.label,"EDGE");
   dblk.subsno = cvsf->subscr;
   dblk.matrix_ptr = 0;
   status = uc_retrieve_transf (cvsf->key, tf);
   if ((mtrid=uio_chk_matrix(tf,*dcount))<0)
   {
      uio_mtrtomtr(tf,t1);    /* Convert Unicad matrix to iges matrix */
      dblk.matrix_ptr = *dcount;
      uio_tran(fd1,fd2,t1,dcount,pcount);
   }
   else     /* Matrix exists in IGES file, point the pointer to it */
   {
      dblk.matrix_ptr = mtrid;
   }

   dblk.par_ptr = *pcount;
   dblk.form_no = 0;

   igcvsf.key = GCRVONSRF;
   igcvsf.crtn = 0;
   igcvsf.srf = bsdir;
   igcvsf.crv = 0;
   igcvsf.b_ptr = cvdir;
   igcvsf.pref = 1;
/*
.....put label in properties
*/
   uio_label (fd1,fd2,cvsf->label,dcount,pcount);
   igcvsf.no_bptr = 0;
   dblk.par_ptr = *pcount;
   igcvsf.no_prop = 1;
   prop[0] = *dcount - 2;
   igcvsf.prop = &prop[0];

   uio_put_para(GCRVONSRF,&igcvsf,fd2,pcount,*dcount);
   uio_attr_to_dir(fd1,&dblk,dcount,*pcount);

	uu_dexit;
	return (status);
}

/*********************************************************************
**    I_FUNCTION :  uio_colortable(fd1,fd2,dcount,pcount)
**      Output the color table information to
**			directory and parameter section record.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_colortable(fd1,fd2,dcount,pcount)
int	fd1, fd2;
int	*dcount;		/* directory section sequence number */
int	*pcount;	/* parameter section sequence number */
	
{
	struct  dir_rec dblk;		/* directory record */
	int  i, last;
	struct IG_igesclr_rec colorrec;

	last = -1;
	for (i=0; i<UIG_MAXCOLOR;i++)
	{
		if (uw_color_name[i][0]!='\0')
			last = i;
	}
	if (last==15) return;

	dblk.line_font = 1;
	dblk.level = 1;
	dblk.view_ptr = 0;
	dblk.matrix_ptr = 0;
	dblk.sub_swt = 0;
	dblk.use_flg = 0;
	dblk.hier = 0;
	dblk.line_wt = 0;
	dblk.pen_no = 0;
	dblk.par_cnt = 0;
	dblk.form_no = 0;
	dblk.blank = 0;
	dblk.rel_type = GCOLOR;
	dblk.sub_swt = UIO_sub_swt;
	dblk.use_flg = UIO_use_flg;
	dblk.subsno = 0;
	UIG_ncolor_iges = last;
	UIG_color_array = (struct UIG_color_struct *)
				uu_toolmalloc((UIG_ncolor_iges+1)*sizeof(struct UIG_color_struct));
	for (i=0; i<=last;i++)
	{
		colorrec.key = GCOLOR;
		colorrec.red = (double)uw_color_table[i][0]/2.55;
		colorrec.green = (double)uw_color_table[i][1]/2.55;
		colorrec.blue = (double)uw_color_table[i][2]/2.55;
		if (strlen(uw_color_name[i])<20)
			strcpy(colorrec.name, uw_color_name[i]);
		else
			sprintf(colorrec.name, "IGES_COLOR%d", i);

		dblk.par_ptr = *pcount;
		if (strlen(uw_color_name[i])<8)
			strcpy(dblk.label, uw_color_name[i]);	
		else
			sprintf(dblk.label, "COLOR%d", i);
		UIG_color_array[i].color = i;
		UIG_color_array[i].irec = *dcount;
		uio_put_para(GCOLOR,&colorrec,fd2, pcount,*dcount);
		uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	}
	return (0);
}
