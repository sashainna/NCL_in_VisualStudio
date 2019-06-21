/*********************************************************************
**    NAME         :  m3ecn1.c
**       CONTAINS:
**			int um_drw4_conic(eptr, tfmat, attrptr)
**			int um_p4_conic(ptr)
**			int um_cp4_copyconic(e1, e2, bagsize)
**			int um_tr4_tranconic(eptr,offset)
**			int um_tf4_tranfconic(eptr, basepoint, trform, store)
**			um_projcirc(circptr, tfmat, normal, point, ellptr, errptr);
**			um_cn4_endpoints(eptr, spt, ept, tfmat)
**			um_ilnconic(lpt, luvc, cptr, nint, ipt, mult)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3ecn1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:51
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"
#include "ulist.h"
#include "ncl.h"
#include "nclfc.h"

#define	UM_ECN1TRC	1

/*********************************************************************
**    E_FUNCTION :  int um_drw4_conic(eptr, tfmat, attrptr)
**       Draws conics, in fact any entity which can be drawn with
**			equal parameter increments, and which supports
**			a um_setxx call to provide appropriate number of polyline sides
**    PARAMETERS   
**       INPUT  : 
**				eptr    			pointer to conic record
**				tfmat				transformation matrix
**				attrptr			pointer to attribute record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : call to evaluator may be slow
*********************************************************************/
int
um_drw4_conic(eptr,tfmat,attrptr)
	struct  UM_conic_rec  *eptr;
	UM_transf tfmat;
	struct UM_attrdatabag *attrptr;

	{
	register UU_REAL	u;		/* curve segment parameter		*/
	register int i;			/* index						*/
	int   j, nu, nside;		/* the number of sides of the
										approximating polygon */
   UM_int2 idx, ival;
   UU_REAL *ptcv, tol;
	UM_param	delta_u;			/* parameter increment	*/
	Gwpoint3 *gpt;				/* points to sent to GKS */

	struct UM_evcrvout	evout;	
   UU_LIST cvpt;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_drw3_conic(key=%d,tfmat=%x,attrptr=%x)",
					eptr->key,tfmat,attrptr));

   idx = 136;
   getifl(&idx, &ival);
   nu  = ival;
   idx = 175;
   getsc (&idx,&tol);
   j   = (tol < .005)? 100: 50;
   if (nu > j) j = nu; 
   uu_list_init (&cvpt, sizeof(UM_coord), j, j);

	um_set_disp_attr(attrptr);

   if (nu > 0)
      nu = ncl_evolve1_curve (eptr,tfmat, nu, &cvpt);
   else
      nu = ncl_evolve_curve (eptr,tfmat,tol,&cvpt,UU_NULL,UU_NULL,0);

   ptcv  = (UU_REAL *) UU_LIST_ARRAY (&cvpt);

   for ( i = 0, j=0; i < nu; i++, j+=3 )
      glina3 (&ptcv[j],&ptcv[j+1],&ptcv[j+2]);

   gdraw();
/*
	/* determine delta_u */
/*um_set4_conic(eptr, tfmat, &nside);
/*delta_u = 1.0/nside;

	/* get array to pass to gpolyline	*/
/*gpt = (Gwpoint3 *) uu_malloc((nside +1) * sizeof(Gwpoint3));

/*u = 0;
/*for ( i = 0; i < nside; i++ )
/*	{
	/* get point and put it into polyline */
/*	uc_evcrv(UM_POINT, u, eptr, tfmat, &evout);

/*	gpt[i].x = evout.cp[0];
/*	gpt[i].y = evout.cp[1];
/*	gpt[i].z = evout.cp[2];
	/* um_vctovc(evout.cp, gpt[i]);	 */
/*	u += delta_u;
	/* Fix: error check on evaluator	*/

/*	}
	/* get last point */
/*uc_evcrv(UM_POINT, (UU_REAL) 1.0, eptr, tfmat, &evout);
/*	gpt[i].x = evout.cp[0];
/*	gpt[i].y = evout.cp[1];
/*	gpt[i].z = evout.cp[2];
	/* um_vctovc(evout.cp, gpt[i]);	 */

/*um_gpolyline3(attrptr, nside+1, gpt);

/*uu_free(gpt); 
*/
   uu_list_free (&cvpt); 

	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_p4_conic(ptr)
**       Print the data defining a conic (arc).
**    PARAMETERS   
**       INPUT  : 
**				ptr					pointer to conic record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_p4_conic(ptr)
	struct  UM_conic_rec  *ptr;

	{

	uu_denter( UU_MTRC,(us,"um_p3_conic(key=%x)",ptr->key));
	sprintf( UM_sbuf, "CONIC %d", ptr->key);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf, "label %7.7s", ptr->label);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PINT, "type", 1, &ptr->type);
	um_p_ary(UM_PFLOAT, "invariants", 2, ptr->invariants );
	um_p_ary(UM_PFLOAT, "x-axis", 3, ptr->tfmat[0]);
	um_p_ary(UM_PFLOAT, "y-axis", 3, ptr->tfmat[1]);
	um_p_ary(UM_PFLOAT, "normal", 3, ptr->tfmat[2]);
	um_p_ary(UM_PFLOAT, "center", 3, ptr->tfmat[3]);

	um_p_ary(UM_PFLOAT, "endpoint 0", 1, &ptr->t0);
	um_p_ary(UM_PFLOAT, "endpoint 1", 1, &ptr->t1);

	umi_print_transf(ptr->key);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION: int um_cp4_copyconic(e1, e2, bagsize)
**      Copy a conic
**    PARAMETERS   
**       INPUT  : 
**				e1			pointer to entity to be copied
**				bagsize	size of storage for entity.
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cp4_copyconic(e1, e2, bagsize)
	struct UM_conic_rec *e1;
  struct UM_conic_rec *e2;
  int bagsize;

	{
	struct UM_attrdata_rec attrbag;

	uu_denter(UU_MTRC,(us,"um_cp4_copyconic(key=%x,bagsize=%d)",e1->key,bagsize));
                                            
	ur_setup_data(e1->rel_num, e2, bagsize);
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e2->label, "");
	e2->subscr = 0;

		/* a structure copy would be nice */
	e2->type = e1->type;
	e2->invariants[0] = e1->invariants[0];
	e2->invariants[1] = e1->invariants[1];
	e2->t0 = e1->t0;
	e2->t1 = e1->t1;
	um_vctovc(e1->tfmat[0], e2->tfmat[0]);
	um_vctovc(e1->tfmat[1], e2->tfmat[1]);
	um_vctovc(e1->tfmat[2], e2->tfmat[2]);
	um_vctovc(e1->tfmat[3], e2->tfmat[3]);

	um_get_disp_attr(e1->key, &attrbag);
	um_create_geom(e2, UM_DEFAULT_TF, &attrbag);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_tr4_tranconic(eptr,offset)
**      Translate the specified conic by offset
**    PARAMETERS   
**       INPUT  : 
**				eptr		pointer to the  entity to be translated
**			   offset	vector by which to translate
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tr4_tranconic(eptr,offset)
	struct UM_conic_rec *eptr;
	UM_vector   offset;

	{
	uu_denter( UU_MTRC,(us,"um_tr4_tranconic(key=%x)",eptr->key));
    
   um_vcplvc(eptr->tfmat[3], offset, eptr->tfmat[3]);

	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_tf4_tranfconic(eptr, basepoint, trform, store)
**      Transform the specified conic by the given 4x3 matrix, 
**			conjugated by translation to basepoint.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be scaled
**          basepoint   point to be scaled about
**          trform      the 4x3 scaling matrix
**				store			if true, update_geom()
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tf4_tranfconic(eptr, trform, store)

	struct UM_conic_rec *eptr;
   UM_transf    trform;
	UU_LOGICAL	store;
	{                                     

	uu_denter(UU_MTRC,(us,"um_tf4_tranfconic(key=%x,store=%d)",
	eptr->key, store));

   um_tftmtf(eptr->tfmat, trform, eptr->tfmat);

	if (store)
		um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION :  um_projcirc(circptr, tfmat, normal, point, ellptr, errptr);
**       calculates ellipse by projecting circle onto plane
**    PARAMETERS   
**       INPUT  : 
**          circptr	a valid circle entity record
**				tfmat		transformation for circle to model space (that ellipse
**								is to live in)
**				normal	normal to projection plane	(not assumed unit length)
**				point		point on projection plane
**				ellptr	ellipse record (already ur_setup_data)
**       OUTPUT :  
**          ellptr	geometry filled in
**				*errptr	as follows:
**						0	-	no error
**						1	-	circle projects to zero-width ellipse
**						2	-	endpoint problems
**					other	-	as needed
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : the tranformation passed for the
**			circle must not make the radius of the circle invalid
*********************************************************************/
um_projcirc(circptr, tfmat, normal, point, ellptr, errptr)
	struct	UM_circle_rec	*circptr;
	UM_transf	tfmat;
	UM_vector	normal;
	UM_coord	point;
	struct	UM_conic_rec	*ellptr;
	int		*errptr;
	{
	UM_vector	pnormal;			/* modifiable copy of *normal					*/
	UM_vector	cnormal;			/* unit length copy of circle normal		*/
	UU_REAL	dihedral;			/* dot product of two plane unit normals	*/
	UM_vector	lift_vector;	/* unit vect which projects to minor axis	*/
	int		numisect;
	/* for endpoints	*/
	UM_coord	endpoints[2];
	struct	UM_evcrvout	evout;
	UM_coord	ccenter;


	uu_denter(UU_MTRC,(us,"um_projcirc(?,?,?,?,?)"));

	/* transform geometry of circle	*/
	um_vctmtf(circptr->nvec, tfmat, cnormal);
	um_cctmtf(circptr->center, tfmat, ccenter);
	/* note: no tranformation of radius	*/

		/* make sure we are working with unit vectors	*/
	um_unitvc(normal, pnormal);
	um_unitvc(cnormal, cnormal);
	dihedral = um_dot(cnormal, pnormal);
	if (UM_DZILCH(dihedral))
		{
		*errptr = 1;
		goto Done;
		}

	/*
	 * swap normal sign if necessary to project circle
	 * to counterclockwise segment
	 */
	if ((dihedral < 0 && circptr->dang > 0)||(dihedral > 0 && circptr->dang < 0))
		{
			/* flip plane normal	*/
		um_vctmsc(pnormal, (UU_REAL) -1.0, pnormal);
		dihedral *= -1.0;
		}

	/* 
	 * must handle parallel planes as special case
	 */
	if (UM_ZILCH(1.0 - dihedral) || UM_ZILCH(1.0 + dihedral))
		{
			/* arbitrary major-axis	*/
		um_perpvc(pnormal, ellptr->tfmat[0]);
		um_unitvc(ellptr->tfmat[0], ellptr->tfmat[0]);
			/* minor axis	*/
		um_cross(pnormal, ellptr->tfmat[0], ellptr->tfmat[1]);
			/* projection is congruent to circle	*/
		ellptr->invariants[0] = circptr->radius;
		ellptr->invariants[1] = ellptr->invariants[0];
		}
	else	/* general case	*/
		{
			/* major axis is intersection of planes	*/
		um_cross(pnormal, cnormal, ellptr->tfmat[0]);
		um_unitvc(ellptr->tfmat[0], ellptr->tfmat[0]);
		ellptr->invariants[0] = circptr->radius;

			/* minor axis is perpendicular to major	*/
		um_cross(pnormal, ellptr->tfmat[0], ellptr->tfmat[1]);

			/* projection scaling perpendicular to intersection of planes	*/
		um_cross(cnormal, ellptr->tfmat[0], lift_vector);	/* should be unit vc*/
		ellptr->invariants[1] =
				um_dot(lift_vector, ellptr->tfmat[1]) * circptr->radius;
		ellptr->invariants[1] = fabs(ellptr->invariants[1]);
		}

	/* project center of circle to plane of ellipse	*/
	um_ilnpln(ccenter,pnormal,point,pnormal,&numisect,ellptr->tfmat[3]);
	/* last piece of transformation is normal	*/
	um_vctovc(pnormal, ellptr->tfmat[2]);
	ellptr->type = UM_ELLIPSE;

	/**	--	ENDPOINTS	--	**/

	/* project endpoints of circle to plane	*/
	uc_init_evcrvout(circptr, &evout);

	uc_evcrv(UM_POINT, (UU_REAL) 0.0, circptr, tfmat, &evout);
	um_ilnpln(evout.cp,pnormal,point,pnormal,&numisect,endpoints[0]);

	uc_evcrv(UM_POINT, (UU_REAL) 1.0, circptr, tfmat, &evout);
	um_ilnpln(evout.cp,pnormal,point,pnormal,&numisect,endpoints[1]);

#if 	UM_ECN1TRC
	um_p_ary(UM_PFLOAT, "circle tfmat", 6, tfmat);
	um_p_ary(UM_PFLOAT, "endpoint 0 on circle", 3, endpoints[0]);
	um_p_ary(UM_PFLOAT, "endpoint 1 on circle", 3, endpoints[1]);
#endif

	/**	--	set endpoints	--	**/
	/* pass identity tranformation, which is the tranformation 
	 * for the new ellipse (its geometry is tranformed	
	 */
	if (um_cn4_endpoints(ellptr, endpoints[0], endpoints[1], UM_DEFAULT_TF))
		{
		ellptr->t0 = -2;
		ellptr->t1 =  2;
		*errptr = 2;
		}
	else
		{
		*errptr = 0;
		}

Done:
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION :  um_cn4_endpoints(eptr, spt, ept, tfmat)
**       sets endpoint fields for conic entity
**    PARAMETERS   
**       INPUT  : 
**          eptr		-	conic in question
**				spt, ept - 	endpoints in model space
**				tfmat		-	transformation from conic defn space to
**								space in which spt, ept are defined (model space)
**       OUTPUT :  
**          *eptr		-	endpoints are set
**    RETURNS      : 0 if reasonalbly close hits were made
**							1 if first endpoint is bad 
**							2 if second endpoint is bad 
**							3 if second endpoint is on wrong branch
**								of hyperbola
**    SIDE EFFECTS : may change internal conic transformation to 
**							swap hyperbola branches
**    WARNINGS     : if endpoints are close together (for case of ellipse)
**							will assume that the ellipse is complete (closed)
*********************************************************************/

um_cn4_endpoints(eptr, spt, ept, tfmat)
	struct	UM_conic_rec	*eptr;
	UM_coord	spt;
	UM_coord	ept;
	UM_transf	tfmat;
	{
	UU_REAL	fuzz;			/* endpoint imprecision				*/
	UU_REAL	t;				/*  parameter candidate value		*/
	UU_REAL	temp[3];
	int		retval;		/* return value						*/
	int		status;

	/* composition of tfmat with conic internal transformation		*/
	UM_transf	totaltfmat;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_cn4_endpoints()"));

	um_tftmtf(eptr->tfmat, tfmat, totaltfmat);

#if 	UM_ECN1TRC
	um_p_ary(UM_PFLOAT, "cn4_endpts: totaltfmat: ellipse to model space",
		12, totaltfmat);
#endif

	/** first endpoint	**/
	status = umi_cc4_cctot(spt, eptr, totaltfmat, &t, &fuzz);

#if 	UM_ECN1TRC
	sprintf(UM_sbuf, "um_cn4_endpoints: 1st umi_cc4_cctot returned %d.", status);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "tvalue %f, fuzz %f", t, fuzz);
	um_pscroll(UM_sbuf);
	um_pscroll(" ");
#endif

Startpoint:
	switch (status)
		{
	 case -2:	/* wrong hyperbola branch */
		um_vctovc(eptr->tfmat[1], temp);
		um_vctmsc(eptr->tfmat[0], (UU_REAL) -1.0, eptr->tfmat[1]);
		um_vctovc(temp, eptr->tfmat[0]);

		/** FIX: dubious flow of control **/
		goto Startpoint;

		break;

	 case 0:		/* answer close	*/
	 case 1:		/* answer not close	*/
		eptr->t0 = t;
		retval = 0;
		break;

	 case -1:	/* answer invalid	*/
	 case -3:
		retval = 1;
		goto Done;
		}

	/** second endpoint	**/
	status = umi_cc4_cctot(ept, eptr, totaltfmat, &t, &fuzz);

#if 	UM_ECN1TRC
	sprintf(UM_sbuf, "um_cn4_endpoints: umi_cc4_cctot returned %d.", status);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "tvalue %f, fuzz %f", t, fuzz);
	um_pscroll(UM_sbuf);
	um_pscroll(" ");
#endif

	switch (status)
		{
	 case 0:		/* answer close	*/
	 case 1:		/* answer not close	*/
		eptr->t1 = t;
		retval = 0;
		break;

	 case -2:	/* wrong hyperbola branch (fatal this time)*/
		retval = 3;
		break;
	 case -1:	/* answer invalid	*/
	 case -3:
		retval = 2;
		break;
		}


Done:
	
	/* special handling to detect closed ellipse	*/
	if (eptr->type == UM_ELLIPSE && (retval == 0 || retval == 1))
		{
		if ( fabs(eptr->t0 - eptr->t1) < UM_FUZZ)	/* same parameter	*/
			{
			eptr->t0 = -2;
			eptr->t1 =  2;
			}

		}

#if	UM_ECN1TRC
	um_p_ary(UM_PINT, "um_cn4_endpoint() returning:", 1, &retval);
#endif

	uu_dexit;
	return (retval);
	}
/*********************************************************************
**    E_FUNCTION :  um_ilnconic(lpt, luvc, cptr, nint, ipt, mult)
**       analytically intesects line with conic.  line is assumed
**			to be in plane of conic (silently projected)
**    PARAMETERS   
**       INPUT  : 
**          lpt	-	 point on line
**				luvc 	-	unit direction of line
**				cptr	-	pointer to conic entity rec.
**       OUTPUT :  
**          *nint	-	number of intersection points
**				ipt[2]-	intersection points
**				*mult	-	multiplicity of intersection (1 or 2)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/* y = slope*x + intercept	*/
#define	UM_LINEY(x)	(slope*(x) + intercept)

um_ilnconic(lpt, luvc, cptr, nint, ipt, mult)
	UM_coord		lpt;
	UM_vector	luvc;
	struct		UM_conic_rec	*cptr;
	int			*nint;
	UM_coord		ipt[2];
	int			*mult;
	{
	UU_REAL	a, b, c;		/* coefficients for quadratic equation to solve	*/
	UU_REAL	i0, i1;		/* conic invariants	*/
	UU_REAL	i0sq, i1sq;	/* their squares		*/
	UU_LOGICAL	multiple = UU_FALSE;
	UM_transf	localinv;
	UM_coord		lpt2;		/* local space lpt	*/
	UM_vector	luvc2;	/* local space luvc	*/
	int			i;

	uu_denter(UU_MTRC,(us,"um_ilnconic()"));

	/* get conic invariants	*/
	i0 = cptr->invariants[0];
	i1 = cptr->invariants[1];
	i0sq = i0*i0;
	i1sq = i1*i1;

#if UM_ECN1TRC
	sprintf(UM_sbuf, "um_ilnconic: invariants: %f, %f, type %d",
		i0, i1, cptr->type);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PFLOAT,"point on line", 3, lpt);
	um_p_ary(UM_PFLOAT,"direction", 3, luvc);
	um_p_ary(UM_PFLOAT, "internal tfmat: ", 6, cptr->tfmat);
#endif


	/* transform line by inverse of cptr->tfmat	*/
	um_inverttf(cptr->tfmat, localinv);
	um_vctmtf(luvc, localinv, luvc2);
	um_cctmtf(lpt, localinv, lpt2);

	/* vertical line test	*/
	if ( UM_ZILCH(luvc2[0]) )	/* y-component = 0	*/
		{
		UU_REAL	x0 = lpt2[0];

#if UM_ECN1TRC
		sprintf(UM_sbuf, "  vertical line: x0 = %f", x0 );
		um_pscroll(UM_sbuf);
#endif

		switch (cptr->type)		/* each will set *nint, ipt	*/
			{
		 case	UM_ELLIPSE:
			if (UM_ZILCH(x0 - i0) || UM_ZILCH(x0 + i0))	/* double intersect	*/
				{
				*nint = 1;
				multiple = UU_TRUE;
				ipt[0][0] = x0;
				ipt[0][1] = 0;
				ipt[0][2] = 0;
				}
			else if (fabs(x0) < i0)	/* two intersections	*/
				{
			UU_REAL	sqt;

				sqt = sqrt(1 - (x0*x0/(i0sq)));
				*nint = 2;
				ipt[0][0] = x0;
				ipt[0][1] = i1 * sqt;
				ipt[0][2] = 0;

				ipt[1][0] = x0;
				ipt[1][1] = -ipt[0][1];
				ipt[1][2] = 0;
				}
			else							/* no intersection	*/
				{
				*nint = 0;
				}
			break;

		 case	UM_HYPERBOLA:
			if (UM_ZILCH (x0 - i0))	/* double intersection	*/
				{
				*nint = 1;
				multiple = UU_TRUE;
				ipt[0][0] = x0;
				ipt[0][1] = 0;
				ipt[0][2] = 0;
				}
			else if (x0 > i0)			/* two intersections	*/
				{
				*nint = 2;
				ipt[0][0] = x0;
				ipt[0][1] = i1 * sqrt((x0*x0/(i0sq)) - 1);
				ipt[0][2] = 0;

				ipt[1][0] = x0;
				ipt[1][1] = -ipt[0][1];
				ipt[1][2] = 0;
				}
			else							/* no intersection	*/
				{
				*nint = 0;
				}
			break;
		 case	UM_PARABOLA:
			if (UM_ZILCH (x0))		/* double intersection	*/
				{
				*nint = 1;
				multiple = UU_TRUE;
				ipt[0][0] = x0;
				ipt[0][1] = 0;
				ipt[0][2] = 0;
				}
			else if ( x0 > 0 )		/* two intersections	*/
				{
				*nint = 2;
				ipt[0][0] = x0;
				ipt[0][1] = sqrt(x0/i0);
				ipt[0][2] = 0;

				ipt[1][0] = x0;
				ipt[1][1] = -ipt[0][1];
				ipt[1][2] = 0;
				}
			else							/* no intersections	*/
				{
				*nint = 0;
				}
			break;
		 default:
			 sprintf(UM_sbuf, "um_ilnconic: bad type %d.", cptr->type);
			 um_pscroll(UM_sbuf);
			}
		}	/* end: vertical line handling	*/
	else	/* non-vertical	*/
		{
		UU_REAL	slope;		/* line: y = slope*x + intercept	*/
		UU_REAL	intercept;

		int		degen;		/* output buffers for um_quadratslv	*/
		UU_REAL	xary[2];

#if UM_ECN1TRC
		um_pscroll("line not vertical");
#endif
		slope = luvc2[1]/luvc2[0];
		intercept = lpt2[1] - (lpt2[0]*slope);

		switch (cptr->type)
			{
			 case	UM_ELLIPSE:
				a = i1sq + i0sq*slope*slope;
				b = 2*i0sq*slope*intercept;
				c = i0sq*(intercept*intercept - i1sq);
				break;
	
			 case	UM_HYPERBOLA:
				a = i1sq - i0sq*slope*slope;
				b = -2*i0sq*slope*intercept;
				c = -i0sq*(i1sq + intercept*intercept);
				break;
	
			 case	UM_PARABOLA:
				a = i0*slope*slope;
				b = 2*i0*slope*intercept - 1;
				c = i0*intercept*intercept;
				break;
			 default:
				sprintf(UM_sbuf, "um_ilnconic: bad type %d.", cptr->type);
				um_pscroll(UM_sbuf);
			}

		/* now have coefficients a, b, c for quadratic equation	*/
		um_quadratslv(a, b, c, &degen, xary);

		/* for a hyperbola, throw away any point on negative branch */
		if ((cptr->type == UM_HYPERBOLA) && (degen == 0))
			{
			if (xary[0] < 0)
				{
				xary[0] = xary[1];
				degen = 1;
				}
			}

#if UM_ECN1TRC
		sprintf(UM_sbuf, "after um_quadratslv: degen = %d", degen);
		um_pscroll(UM_sbuf);
#endif

		switch (degen)
			{
			 case	0:		/* two roots	*/
				*nint = 2;
				ipt[0][0] =	xary[0];
				ipt[0][1] =	UM_LINEY(xary[0]);
				ipt[0][2] =	0;
	
				ipt[1][0] =	xary[1];
				ipt[1][1] =	UM_LINEY(xary[1]);
				ipt[1][2] =	0;
				break;
	
			 case	1:		/* degen: 1 root	*/
				*nint = 1;
				ipt[0][0] =	xary[0];
				ipt[0][1] =	UM_LINEY(xary[0]);
				ipt[0][2] =	0;
				break;
	
			 case	2:		/* double root	*/
				*nint = 1;
				multiple = UU_TRUE;
				ipt[0][0] =	xary[0];
				ipt[0][1] =	UM_LINEY(xary[0]);
				ipt[0][2] =	0;
	
				ipt[1][0] =	xary[1];
				ipt[1][1] =	ipt[0][1];
				ipt[1][2] =	0;
				break;
	
			 case	-1:	/*	no solution	*/
				 *nint = 0;
				break;
	
			 case	-2:	/* degenerate equation	*/
				*nint = 0;
				break;
	
			 default:
				 um_pscroll("um_ilnconic: unexpected return from um_quadratslv.");
			}
		}
	/* now have *nint, ipt[], and multiplicity set	*/

	/* transform ipt[], set *mult	*/
	for ( i = 0; i < *nint; ++i)
		{
		um_cctmtf(ipt[i], cptr->tfmat, ipt[i]);
		*mult = multiple? 2: 1;
		}

Done:
	uu_dexit;
	return;
	}
