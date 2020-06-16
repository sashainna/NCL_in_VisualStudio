/*********************************************************************
**    NAME         :  tigmodsp.c
**       CONTAINS:
**       int um_init_evcrvout(eptr, evoutptr)
**       um_evcrv(evflag,u,eptr,tfmat,evoutptr) 
**       int um_init_evsrfout(eptr, evoutptr)
**       uc_evsrf(evflag,u,eptr,tfmat,evoutptr) 
**       int uc_reverse_crv (eptr)
**       uc_super_class(rel_num)
**       int um_get_all_geom(eptr, databagsize)
**       int um_get_transformation(key, tfmat)
**       UU_LOGICAL um_dynamic_varlist(rel_num)
**       UU_LOGICAL um_isvalid_relation(rel_num)
**       int um_agcrv_to_unirbsc(agcrv, rbsc)
**       int um_agcrv_frmrbsplcrv(bptr,rptr)
**       int um_agcrv_frmline(lptr,rptr)
**       int um_agcrv_frmcirc(cptr,rptr)
**       int um_agcrv_frmconic(cptr, rptr)
**       int um_agcrv_frmcompcrv(compcrv, rbsc)
**       int um_agcrv_frmpolyline(pptr, rbsc)
**       int um_crv_to_agrbsc(unicrv, rbsc)
**       int um_cn_defn(eptr, tfmat, cn_defn)
**       int ncl_eval_surf (evflag, u, v, eptr, tfmat, evsrf)
**       int ncl_eval_rbsf (evflag, u, v, eptr, tfmat, evsrf)
**       int gtgeo(nclkey, buf)
**       int gtmpat(nclkey, ipatch, buf)
**       int ncl_get_meshheader(surf, buf)
**       int ncl_get_meshpatch(patch, meshpatch)
**       int evstup(nclkey)
**       int uevsrf(u,v,sv)
**       int uig_transf_entity (eptr, dblk)
**       int uig_transf_msf (msf, t)
**       int uig_transf_mpatch (patch, tfmat)
**       int uig_transf_rbsf (rsf, t)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigmodsp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:49
*********************************************************************/
#if UU_COMP == UU_WIN2K
#include <string.h>
#define index strchr
#endif

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdrel.h"
#include "mdclass.h"
#include "mdeval.h"
#include "mcrv.h"
#include "msrf.h"
#include "mfort.h"
#include "nclfc.h"
#include "ncl.h"
#include "umoveb.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mdgenent.h"
#include "mattr.h"
#include "msol.h"
#include "mdebug.h"
#include "modef.h"
#include "r1emsgpk.h"
#include "rbase.h"
#include "ribase.h"
#include "rmtuple.h"
#include "tiges.h"
#include "nccs.h"

#include "ag_incl.h"
#include "ag_global.h"

#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D) || (UU_COMP == UU_DECUNIX)
#ifndef UU_RS6000
#define mshevl mshevl_
#define nsfevl nsfevl_ 
#endif
#endif /* UU_COMP */

/*********************************************************************
**    E_FUNCTION: int um_init_evcrvout(eptr, evoutptr)
**			Initialize an evaluator record for curves. This function must
**			be called prior to the first call to a curve evaluator for the
**			specified entity.
**    PARAMETERS   
**       INPUT: 
**				eptr					the entity to have an evaluator record
**										set up for.
**       OUTPUT:  
**				evoutptr				pointer to the set up evaluator record. 
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_init_evcrvout(eptr, evoutptr)
	struct UM_crvdatabag *eptr;
	struct UM_evcrvout *evoutptr;

	{
	int status = UU_FAILURE;

	if (uc_super_class(eptr->rel_num) == UM_CURVE_CLASS)
		{
		evoutptr->firsteval = UU_TRUE;
		status = UU_SUCCESS;
		}

	return (status);
	}
/*********************************************************************
**    E_FUNCTION: int um_init_evsrfout(eptr, evoutptr)
**       Initialize an evaluator record for surfaces; Note this function
**       should be called before the surface evaluator is called.
**    PARAMETERS
**       INPUT:
**          eptr              the entity to have an evaluator record
**                            set up for.
**          evoutptr          pointer to the evaluator record to be set up.
**       OUTPUT:
**          evoutptr          pointer to the set up evaluator record.
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_init_evsrfout(eptr, evsrfptr)
struct UM_entitydatabag *eptr;
struct UM_evsrfout *evsrfptr;
{
	if (uc_super_class(eptr->rel_num) == UM_SURFACE_CLASS)
	{
		evsrfptr->firsteval = UU_TRUE;
		return (UU_SUCCESS);
	}
	else
		return (UU_FAILURE);
}

/*******************************************************************
**  E_FUNCTION: um_evcrv(evflag,u,eptr,tfmat,evoutptr) 
**		Given a curve entity (EPTR) with associated transformation
**		matrix (TFMAT which may be UM_DEFAULT_TF), calculate the
**		data requested by EVFLAG at the logical parameter value U
**		[0.0 <= U <= 1.0].
**    PARAMETERS   
**       INPUT  : 
**          evflag					defines the information to be
**											calculated for the curve
**												UM_POINT=>		point
**												UM_FRSTFERIV=> point, 1st deriv
**												UM_SECDETIV=>	point, 1st deriv, 2nd deriv
**												UM_CURVATURE=>	point, 1st deriv, 2nd deriv,
**																	curvature
**				u						parameter value to evaluate curve function
**				eptr					entity pointer to curve structure
**				tfmat					transformation matrix
**       OUTPUT :  
**          evoutptr				curve evaluator record to put results
**    RETURNS      : 
**			UM_VALID only; ultimately will return extended diagnostic
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_evcrv(evflag, u, eptr, tfmat, evoutptr)
	int evflag;
	UM_param u;
	struct UM_crvdatabag  *eptr;
	UM_transf tfmat;
	struct UM_evcrvout *evoutptr;

	{
	int order;
	int npts;
	UU_REAL *c;
	int status;

	status = UM_VALID;
	switch (eptr->rel_num)
		{
		case UM_LINE_REL:
			um_ev2_line(evflag, u, eptr, tfmat, evoutptr);
			break;
		case UM_CIRCLE_REL:
			um_ev3_circle(evflag, u, eptr, tfmat, evoutptr);
			break;
		case UM_CONIC_REL:
			um_ev4_conic(evflag, u, eptr, tfmat, evoutptr);
			break;
		case UM_COMPCRV_REL:
			um_ev5_compcrv(evflag, u, eptr, tfmat, evoutptr);
			break;
		case UM_RBSPLCRV_REL:
			um_ev7_rbsplcrv(evflag, u, eptr, tfmat, evoutptr);
/*			ncl_eval_rbsp (evflag, u, eptr, tfmat, evoutptr); */
			break;
		case UM_POLYLINE_REL:
			um_ev42_polyline(evflag, u, eptr, tfmat, evoutptr);
			break;
		case UM_UVCVONSF_REL:
			um_ev13_uvcvonsf(evflag, u, eptr, tfmat, evoutptr);
			break;
		default:
			status = UM_INVALID;
			break;
		}
	return (status);
	}
/*******************************************************************
**  E_FUNCTION: uc_reverse_crv (eptr)
**        Revese a curve.
**    PARAMETERS   
**       INPUT  : 
**            eptr             entity pointer to curve structure
**       OUTPUT :  
**            eptr             entity pointer to reversed curve.
**    RETURNS      : UU_SUCCESS iff no error; else UU_FAILURE; 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_reverse_crv (eptr)
    struct UM_crvdatabag  *eptr;
    {
    int status;

    switch (eptr->rel_num)
        {
        case UM_LINE_REL:
            status = um_reverse_line (eptr);
            break;
        case UM_CIRCLE_REL:
            status = um_reverse_circle (eptr);
            break;
        case UM_CONIC_REL:
            status = um_reverse_conic (eptr);
            break;
        case UM_COMPCRV_REL:
            status = um_reverse_compcrv (eptr);
            break;
        case UM_RBSPLCRV_REL:
            status = ncl_rbsp_reverse (eptr);
            break;
        default:
            status = UU_FAILURE;
            break;
        }
    return (status);
    }
/*********************************************************************
**    E_FUNCTION: uc_super_class(rel_num)
**
**			DESCRIPTION: Given the relation number of an entity, this
**				function returns the appropriate class of the entity.
**       
**			PARAMETERS   
**				INPUT: 
**          		rel_num         relation number of the entity whose
**										 class is to be returned.
**				OUTPUT :  none.
**          
**			RETURNS      : either UM_POINT_CLASS, UM_CURVE_CLASS, or 
**					UM_SURFACE_CLASS, UM_SOLID_CLASS, or UM_UNKNOWN_CLASS depending
**					whether the entity is a point, curve, surface, a solid, or
**					unknown.
**
**			SIDE EFFECTS : none
**
**			WARNINGS     : none
*********************************************************************/
int 
uc_super_class(rel_num)
	int rel_num;
		{
	int class;

	class = um_class(rel_num);
	return (class);
		}


static UU_LOGICAL trace = UU_TRUE;

/*********************************************************************
**    E_FUNCTION     : int um_get_all_geom(eptr, databagsize)
**			Given an entity pointed to by EPTR->KEY this function
**			will retrieve all immediate information associated with the 
**			entity (i.e. fixed length entity records and all variable lists).
**			and curves that define surfaces are retrieved.  However,
**			information about the geometric entities on a variable list
**			is not retrieved.
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
/*int
/*um_get_all_geom(eptr, databagsize)
/*	struct UM_entitydatabag *eptr;
/*	int databagsize;
/*
/*	{
/*	int isize;
/*	UU_LOGICAL	um_isvalid_relation();
/*	UU_LOGICAL	um_dynamic_varlist();
/*	int status;
/*
/*
/*	status = UU_SUCCESS; /* assume success */
/*
/*	if ((int) eptr->key < 0)
/*		status = um_ret_romgeom(eptr->key, &eptr->rel_num, eptr);
/*	else
/*		{
/*		ur_retrieve_data_relnum(eptr->key, &eptr->rel_num);
/*
/*		if (um_isvalid_relation(eptr->rel_num))	/* valid relation number	*/
/*			{
/*			if (um_dynamic_varlist(eptr->rel_num))
/*				status = ur_retrieve_app_data(eptr);
/*			else 
/*				status = ur_retrieve_data(eptr, databagsize);
/*			if (status != 0)
/*				{
/*				uu_uerror1(UM_MODEL, 139, eptr->rel_num);
/*				/* message is: um_get_all_geom: not enough space for
/*				 * retrieval of relation.
/*				 */			
/*				status = UU_FAILURE;
/*				}
/*			else		 /* got data OK	*/
/*				{
//****---	no UM_TEXT_REL in modeling
/*				if (eptr->rel_num == UM_TEXT_REL)
/*					{
/*					isize = ((struct UM_text_rec *)eptr)->no_tchar;
/*					((struct UM_text_rec *)eptr)->tchar[isize] = '\0';
/*					}
/*-/*--***/
/*				}
/*			}
/*		else					/* invalid relation number		*/
/*			{
/*			uu_uerror1(/*um_get_all_geometry: illegal relation %d*/
/*				UM_MODEL,49,eptr->rel_num);
/*			status = UU_FAILURE;
/*			}
/*		}
/*	return(status);
/*	}
//**********************************************************************
**    E_FUNCTION     : int um_get_transformation(key, tfmat)
**       Retrieve the transformation (TFMAT) for the entity with
**			the given KEY.
**    PARAMETERS   
**       INPUT  : 
**				key					key of entity to have its transformation
**										retrieved.
**       OUTPUT :  
**				tfmat					transformation matrix.
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*int
/*um_get_transformation(key, tfmat)
/*	int key;
/*	UM_transf tfmat;
/*	{
/*	int status;
/*	int urstatus;	/* return status from UNIBASE */
/*	struct UM_transf_rec transfpacket;
/*
/*	uu_denter(UU_MTRC,(us,"um_get_transformation(key:%d, tfmat:%x)",key, tfmat));
/*	status = UU_SUCCESS; /* assume success */
/*	if ((int) key < 0)
/*		{
/*		um_tftotf(UM_idmat, tfmat);
/*		}
/*	else
/*		{
/*		transfpacket.key = key;
/*		transfpacket.rel_num = UM_TRANSFORM_REL;
/*		urstatus = ur_retrieve_transf(&transfpacket);
/*		if (urstatus != 0)
/*			{
/*			uu_uerror1(UM_MODEL, 163, key);
/*			status = UU_FAILURE;
/*			}
/*		else
/*			um_tftotf(transfpacket.tfmat, tfmat);
/*		}
/*	uu_dexit;
/*	return(status);
/*	}
//**********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_dynamic_varlist(rel_num)
**       Determine if a modeling relation is to be handled by
**			dynamic storage allocation scheme.
**    PARAMETERS   
**       INPUT  : 
**          rel_num						relation number
**       OUTPUT :  
**          none
**    RETURNS      : 
**				UU_TRUE 				if relation is to be handled by dynamic 
**										storage allocation scheme
**				UU_FALSE				otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*UU_LOGICAL 
/*um_dynamic_varlist(rel_num)
/*	int rel_num;
/*
/*	{
/*	UU_LOGICAL status;
/*
/*	uu_denter(UU_MTRC,(us,"um_dynamic_varlist(%d)",rel_num));
/*	switch (rel_num)
/*		{
/*		case UM_DRAWING_REL:
/*		case UM_POLYLINE_REL:
/*			status = UU_TRUE;
/*			break;
/*		default:
/*			status = UU_FALSE;
/*			break;
/*		}
/*	uu_dexit;
/*	return(status);
/*	}

/*********************************************************************
**    E_FUNCTION : UU_LOGICAL um_isvalid_relation(rel_num)
**       Checks for valid modeling relation number.
**    PARAMETERS   
**       INPUT  : 
**          rel_num		 		relation number
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if in modeling's list of relation numbers,
**							UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*UU_LOGICAL
/*um_isvalid_relation(relation)
/*	int	relation;
/*
/*	{
/*	UU_LOGICAL	retval;
/*
/*	uu_denter(UU_MTRC,(us,"um_isvalid_relation()"));
/*
/*		switch (relation)
/*			{
/*			case UM_POINT_REL:
/*			case UM_LINE_REL:
/*			case UM_CIRCLE_REL:
/*			case UM_CONIC_REL:
/*			case UM_COMPCRV_REL:
/*			case UM_RBSPLCRV_REL:
/*			case UM_BODY_REL:
/*			case UM_POLY_REL:
/*			/*case UM_TEXT_REL: */
/*			case UM_POLYLINE_REL:
/*			case UM_COORDSYS_REL:
/*			case UM_LIGHT_REL:
/*			case UM_GROUP_REL:
/*			case UM_DRAWING_REL:
/*			case UM_LAYER_REL:
/*			case UV_VIEW_REL:
/*				retval = UU_TRUE;
/*				break;
/*			default:
/*				retval = UU_FALSE;
/*			}
/*		uu_dexit;
/*		return (retval);
/*	}
/*  */
/*********************************************************************
**    E_FUNCTION     : int um_agcrv_to_unirbsc(agcrv, rbsc)
**       Convert an AG non-uniform rational bspline to its 
**       equivalent UNICAD representation. The AG curve must
**			only have a single bspline segment.
**    PARAMETERS   
**       INPUT  : 
**          agcrv				AG rational bspline
**       OUTPUT :  
**          rbsc				equivalent UNICAD rational bspline
**    RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_to_unirbsc(agcrv, rbsc)
	struct UM_agcrv_rec *agcrv;
	struct UM_rbsplcrv_rec *rbsc;

	{
	int status;
	int i,j,k,n;
	AG_CURVEP crv;
	AG_SPLINEP bs;
	AG_CNODEP node;

	status = UU_FAILURE;

	crv = (AG_CURVEP) agcrv->crvaddr;
	if (crv->nbs != 1) goto done;

	bs = crv->bs0;

	ur_setup_data(UM_RBSPLCRV_REL, rbsc, sizeof(struct UM_rbsplcrv_rec));

	/*MILLS: initialize LABEL and SUBSCRIPT */
	strcpy(rbsc->label,"");
	rbsc->subscr = 0;

	rbsc->k = bs->m + 1;												/* order */
	rbsc->n = bs->n;													/* number of spans */
	rbsc->t0 = *(bs->node0->t);									/* start param */
	rbsc->t1 = *(bs->noden->t);									/* end param */

	rbsc->planar = UU_TRUE;											/* planar curve */
	rbsc->open = UU_TRUE;											/* open curve */

	n = 0;
	i = 0;
	j = 0;
	for (k=0; k<bs->m; k++, j++) rbsc->t[j] = rbsc->t0;	/* start knot mult */
	node = bs->node0;
	do
		{
		um_vctovc(node->Pw, &rbsc->pt[i]);						/* points */
		if (bs->rat)
			rbsc->wt[n] = node->Pw[3];								/* weights */
		else
			rbsc->wt[n] = 1.0;
		rbsc->t[j] = *(node->t);									/* internal knots */
		n++;
		i += 3;
		j++;
		node = node->next;
		}
	while (node != NULL);

	rbsc->t[j] = rbsc->t1;	/* end knot mult */
	j++;

	rbsc->no_t = j;
	rbsc->no_pt = n;
	rbsc->no_wt = n;

	status = UU_SUCCESS;

done:;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : 	int um_agcrv_frmrbsplcrv(bptr,rptr)
**       Convert a rational bspline into its equivalent AG rational bspline form.
**    PARAMETERS   
**       INPUT  : 
**				eptr    			rational bspline curve
**       OUTPUT :  
**				rptr				equivalent AG rational bspline
**    RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_frmrbsplcrv(eptr,rptr)
	struct  UM_rbsplcrv_rec  *eptr;
	struct  UM_agcrv_rec  *rptr;

	{
	int status = UU_FAILURE;
	int dim;
	int degree;
	int spans;
	int form;
	int rat;
	AG_CNODEP node;
	AG_SPLINEP bs, ag_bld_bs();
	AG_CURVEP bc, ag_bld_crv();
	int i, j, n;
	UU_REAL  *tnode;
	UU_LOGICAL um_cceqcc();
	struct UM_evcrvout evout;
	UM_coord spt;
	UM_coord ept;

	dim = 3;															/* dimension */
	degree = eptr->k - 1;										/* degree */
	if (degree > 3)
	  {
	  status = UU_FAILURE;
	  goto done;
	  }

	spans = eptr->n;												/* number of spans */
	rat = 1;															/* rational */

	status = uc_init_evcrvout(eptr, &evout);
	if (status != UU_SUCCESS) goto done;
	status = um_ev7_rbsplcrv(UM_POINT, (UU_REAL) 0.0, eptr, UM_idmat, &evout);
	if (status != UU_SUCCESS) goto done;
	um_vctovc(evout.cp, spt);
	status = um_ev7_rbsplcrv(UM_POINT, (UU_REAL) 1.0, eptr, UM_idmat, &evout);
	if (status != UU_SUCCESS) goto done;
	um_vctovc(evout.cp, ept);
	if (!um_cceqcc(spt, ept)) form = 0; else form = 1;	/* open/closed */

	/* allocate AG bspline structure */
	bs = ag_bld_bs(dim, NULL, NULL, AG_OTHER, degree, spans, rat, form);
	if (bs == NULL) goto done;

	/* allocate AG curve structure */
	bc = ag_bld_crv(dim);
	if (bc == NULL) goto done;

	/* initialize AG bspline structure */
	bc->nbs = 1;
	bc->bs0 = bs;
	bc->bs = bs;
	bc->form = form;

	/* initialize points, weights, and knots */
	for (n=0, i=0, j=bs->m; n<eptr->no_pt; n++, i+=3, j++)
		{
		if (n==0)
			ag_set_cnode_1(bs, eptr->t[j], &eptr->pt[i], eptr->wt[n]);
		else
			{
			if (fabs(eptr->t[j] - eptr->t[j-1]) < AG_tol_knot)
				tnode = NULL;
			else
				tnode = &eptr->t[j];
			ag_set_cnode_2(bs, tnode, &eptr->pt[i], eptr->wt[n]);
			}
		}

	/* set max/min boxes */
	ag_set_bsbox(bs);
	ag_set_cbox(bc);

	/* initialize UNIBASE entity */
	um_agcrv_setup_data(UM_AGCRV_REL, rptr, sizeof(struct UM_agcrv_rec));
	rptr->subscr = 0;
	rptr->key = -1;
	rptr->crvaddr = (int) bc;
	status = UU_SUCCESS;

done:;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_frmline(lptr,rptr)
**       Convert a line into its equivalent AG rational bspline form.
**    PARAMETERS   
**       INPUT  : 
**				lptr    			line entity
**       OUTPUT :  
**				rptr				equivalent AG rational bspline
**    RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_frmline(lptr,rptr)
	struct  UM_line_rec  *lptr;
	struct  UM_agcrv_rec  *rptr;

	{
	int status;
	AG_CURVEP cv;
	AG_CURVEP ag_crv_line_2pt();

	cv = ag_crv_line_2pt(lptr->spt, lptr->ept, 3);

	if (cv == NULL)
		status = UU_FAILURE;
	else
		{
		status = UU_SUCCESS;
		um_agcrv_setup_data(UM_AGCRV_REL, rptr, sizeof(struct UM_agcrv_rec));
		rptr->subscr = 0;
		rptr->crvaddr = (int) cv;
		}

done:;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_frmcirc(cptr,rptr)
**       Convert a circle into its equivalent AG rational bspline form.
**    PARAMETERS   
**       INPUT  : 
**				cptr    			circle entity
**       OUTPUT :  
**				rptr				equivalent AG rational bspline
**    RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_frmcirc(cptr,rptr)
	struct  UM_circle_rec  *cptr;
	struct  UM_agcrv_rec  *rptr;

	{
	int status;
	UM_coord center;
	UM_coord spoint;
	UM_vector svec;
	UM_vector normal;
	UM_length radius;
	UM_angle sang, eang;
	AG_CURVEP cv, ag_crv_carc_ang_3d();

	/* calculate circular arc parameters */
	um_vctovc(cptr->center, center);
	um_vctovc(cptr->nvec, normal);
	radius = cptr->radius;
	sang = 0.0;
	eang = cptr->dang;
	um_vctmsc(cptr->svec, radius, svec);
	um_vcplvc(center, svec, spoint);

	if (eang < 0.0)
		{
		um_vctmsc(normal, (UU_REAL) -1.0, normal);
		eang = -eang;
		}

	cv = ag_crv_carc_ang_3d(center, normal, radius, spoint, sang, eang);

	if (cv == NULL)
		{
		status = UU_FAILURE;
		}
	else
		{
		um_agcrv_setup_data(UM_AGCRV_REL, rptr, sizeof(struct UM_agcrv_rec));
		rptr->subscr = 0;
		rptr->crvaddr = (int) cv;
		status = UU_SUCCESS;
		}

done:;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION 	:  int um_agcrv_frmconic(cptr, rptr)
**       Convert a conic into its equivalent AG rational bspline form.
**    PARAMETERS   
**       INPUT  : 
**          cptr				conic entity
**       OUTPUT :  
**				rptr				equivalent AG rational bspline
**    RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_frmconic(cptr, rptr)
	struct	UM_conic_rec		*cptr;
	struct	UM_agcrv_rec	*rptr;

	{
	struct UM_rbsplcrv_rec unirbsc;
	int status;
	AG_CURVEP cv;
	AG_CURVEP ag_crv_ellp();
	AG_CURVEP ag_crv_parab();
	AG_CURVEP ag_crv_hyper();
	UM_cn_defn cn_defn;

	um_cn_defn(cptr, &cn_defn);	/* cpp */

	switch (cn_defn.type)
		{
		case UM_ELLIPSE:
			cv = ag_crv_ellp( cn_defn.center,
									cn_defn.major_axis,
									cn_defn.major_length,
									cn_defn.minor_axis,
									cn_defn.minor_length,
									cn_defn.spt,
									cn_defn.ept,
									3);
			break;
		case UM_PARABOLA:
			cv = ag_crv_parab(cn_defn.center,
									cn_defn.minor_axis,
									cn_defn.major_axis,
									cn_defn.major_length,
									cn_defn.spt,
									cn_defn.ept,
									3);
			break;
		case UM_HYPERBOLA:
			cv = ag_crv_hyper(cn_defn.center,
									cn_defn.minor_axis,
									cn_defn.minor_length,
									cn_defn.major_axis,
									cn_defn.major_length,
									cn_defn.spt,
									cn_defn.ept,
									3);
			break;
		default:
			cv = NULL;
			break;
		}

	if (cv == NULL)
		status = UU_FAILURE;
	else
		{
		um_agcrv_setup_data(UM_AGCRV_REL, rptr, sizeof(struct UM_agcrv_rec));
		rptr->subscr = 0;
		rptr->crvaddr = (int) cv;
		status = UU_SUCCESS;
		}

done:;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_frmcompcrv(compcrv, rbsc)
**       Convert a UNICAD composite curve to its equivalent AG non-uniform
**			rational bspline representation.
**    PARAMETERS   
**       INPUT  : 
**          compcrv				composite curve entity
**       OUTPUT :  
**				rbsc				equivalent AG rational bspline
**    RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_frmcompcrv(compcrv, agrbsc)
	struct UM_compcrv_rec *compcrv;
	struct UM_agcrv_rec *agrbsc;

	{
	int status;
	UM_coord spt;
	UM_coord ept;
	int form;
	struct UR_data 	*e;
	struct UR_attr 	attr;
	struct UM_agcrv_rec compag;	/* cpp */
	struct UM_agcrv_rec *compagrbsc;	/* cpp */
	struct UM_agcrv_rec *bsc;	/* cpp */
	AG_CURVEP crvp;
	int i, rel_num;	/* cpp */

	um_agcrv_setup_data(UM_AGCRV_REL, agrbsc, sizeof(struct UM_agcrv_rec));
	agrbsc->subscr = 0;

	e = (struct UR_data *) uu_toolmalloc(sizeof(struct UR_data));

	for (i=0; i<compcrv->no_cid; i++)
		{
		e->key_id = compcrv->cid[i].crvid;
		ur_retrieve_data_relnum(e->key_id, &rel_num);	/* cpp */
		e->rel_num = rel_num;		/* cpp */
		uio_getdata(e, &attr);
		if(rel_num == UM_AGCRV_REL)		/* cpp */
			{	
			/*	
			compagrbsc = (struct UM_agcrv_rec*) e;	
			*/	
			umi_agcrv_copy(e, &compag, sizeof(struct UM_agcrv_rec));	
			compagrbsc = &compag;	
			status = UU_SUCCESS;	
			}	
		else	{	
			status = um_crv_to_agrbsc(e, &compag);	
			compagrbsc = &compag;	
			}	
		crvp = (AG_CURVEP) compagrbsc->crvaddr;	/* cpp */
		if (compcrv->cid[i].reverse == UU_TRUE)
			{
			ag_crv_rev_dir(crvp);
			}
		if (i==0)
			{	/* cpp */
			agrbsc->crvaddr = compagrbsc->crvaddr;
			}	/* cpp */
		else
			{
			ag_crv_combine(agrbsc->crvaddr, compagrbsc->crvaddr);
			crvp = (AG_CURVEP) compagrbsc->crvaddr;	/* cpp */
			if (crvp->bs0 != NULL) crvp->bs0 = NULL;
			umi_agcrv_delete(compagrbsc);
			}
		}

	/* cpp */
/*	
	crvp = (AG_CURVEP) agrbsc->crvaddr;	
*/

	uu_toolfree(e);
	status = UU_SUCCESS;	/* cpp */
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_frmpolyline(pptr, rbsc)
**       Convert a UNICAD polyline curve to its equivalent AG non-uniform
**			rational bspline representation.
**    PARAMETERS   
**       INPUT  : 
**          pptr				polyline curve entity
**       OUTPUT :  
**				rbsc				equivalent AG rational bspline
**    RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_frmpolyline(pptr, agrbsc)
	struct UM_polyline_rec *pptr;
	struct UM_agcrv_rec *agrbsc;

	{
	int status = UU_SUCCESS;
	UM_coord spt;
	UM_coord ept;
	AG_CURVEP crvp, crvq;
	AG_CURVEP ag_bld_crv(), ag_crv_line_2pt();
	int i, j;

	um_agcrv_setup_data(UM_AGCRV_REL, agrbsc, sizeof(struct UM_agcrv_rec));
	agrbsc->subscr = 0;

	/* create individual bsplines for each polyline segment and chain 
		together */
	crvp = ag_bld_crv(3);
	for (i=0, j=0; i<(pptr->no_pt-1); i++, j=j+3)
		{
		crvq = ag_crv_line_2pt(&pptr->pt[j], &pptr->pt[j+3], 3);
		ag_crv_combine(crvp, crvq);
		ag_db_crv(&crvq);
		}

	/* hook up AG portion with UNICAD portion */
	agrbsc->crvaddr = (int) crvp;

	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_crv_to_agrbsc(unicrv, rbsc)
**       Convert a UNICAD curve to its equivalent AG non-uniform
**			rational bspline representation.
**    PARAMETERS   
**       INPUT  : 
**          unicrv				UNICAD curve
**       OUTPUT :  
**          rbsc					equivalent AG rational bspline
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_crv_to_agrbsc(e, agrbsc)
	struct UR_data *e;
	struct UM_agcrv_rec *agrbsc;

	{
	int status;

	switch (e->rel_num)
		{

	 	 case UM_LINE_REL	:			/* 2 line */
			status = um_agcrv_frmline(e, agrbsc);
			break;

	 	 case UM_CIRCLE_REL:			/* 3 circle */
			status = um_agcrv_frmcirc(e, agrbsc);
			break;

	 	 case UM_CONIC_REL:			/* 4 conic curve*/
			status = um_agcrv_frmconic(e, agrbsc);
			break;

	 	 case UM_COMPCRV_REL:		/*	5 composite curve */
			status = um_agcrv_frmcompcrv(e, agrbsc);
			break;

	 	 case UM_RBSPLCRV_REL:		/*	7 rational B-spline curve */
			status = um_agcrv_frmrbsplcrv(e, agrbsc);
			break;

		 case UM_AGCRV_REL:
			agrbsc = (struct UM_agcrv_rec *) e;
			break;

	 	 case UM_POLYLINE_REL:		/* 42 polyline */
			status = um_agcrv_frmpolyline(e, agrbsc);
			break;

	 	 default:
			status = UU_FAILURE;
		 	break;
		}

	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_cn_defn(eptr, tfmat, cn_defn)
**       Calculate the geometric definition and analytical definition
**			of a conic.
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to conic entity
**				tfmat					matrix to position conic in MCS
**       OUTPUT :  
**          cn_defn				geometrical and analytical definition
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cn_defn(eptr, cn_defn)
	struct UM_conic_rec *eptr;
	UM_cn_defn *cn_defn;

	{
	int status;
	struct UM_evcrvout evcrv;
	UM_transf mcs_tfmat;
	UU_REAL asq, bsq;
	int i;

	/* assume success */
	status = UU_SUCCESS;

	/* initialize type of conic */
	cn_defn->type = eptr->type;

	/* calculate start point and tangent */
	um_val4_conic(UM_POINT, eptr->t0, eptr, &evcrv);
	/*
	um_vctovc(evcrv.cp, cn_defn->spt);
	*/
	um_cctmtf(evcrv.cp, eptr->tfmat, cn_defn->spt);	/* cpp */

	/* calculate end point and tangent */
	um_val4_conic(UM_POINT, eptr->t1, eptr, &evcrv);
	/*
	um_vctovc(evcrv.cp, cn_defn->ept);
	*/
	um_cctmtf(evcrv.cp, eptr->tfmat, cn_defn->ept);	/* cpp */

	/* calculate conic plane definition */
	um_vctovc(eptr->tfmat[3], cn_defn->pln_point);
	um_vctovc(eptr->tfmat[2], cn_defn->pln_normal);

	/* calculate entity specific geometric data */
	switch (eptr->type)
		{
		case UM_PARABOLA:
		case UM_HYPERBOLA:
		case UM_ELLIPSE:
			um_cctmtf(UM_zerovec, eptr->tfmat, cn_defn->center);
			um_vctmtf(UM_xaxis, eptr->tfmat, cn_defn->major_axis);
			um_vctmtf(UM_yaxis, eptr->tfmat, cn_defn->minor_axis);
			cn_defn->major_length = eptr->invariants[0];
			cn_defn->minor_length = eptr->invariants[1];
			break;
		default:
			status = UU_FAILURE;
			break;
		}

	/* calculate the entity specific analytic data */
	for (i=0; i<6; i++) cn_defn->cn[i] = 0.0;
	switch (eptr->type)
		{
		case UM_PARABOLA:
			cn_defn->major_length = 1/(4*eptr->invariants[0]);
			cn_defn->cn[2] = eptr->invariants[0];
			cn_defn->cn[3] = -1.0;
			break;
		case UM_HYPERBOLA:
			asq = eptr->invariants[0] * eptr->invariants[0];
			bsq = eptr->invariants[1] * eptr->invariants[1];
			cn_defn->cn[0] = 1.0;
			cn_defn->cn[2] = -(asq/bsq);
			cn_defn->cn[5] = -(asq);
			break;
		case UM_ELLIPSE:
			asq = eptr->invariants[0] * eptr->invariants[0];
			bsq = eptr->invariants[1] * eptr->invariants[1];
			cn_defn->cn[0] = 1.0;
			cn_defn->cn[2] = asq/bsq;
			cn_defn->cn[5] = -(asq);
			break;
		default:
			status = UU_FAILURE;
			break;
		}
	
	return (status);
	}
/*********************************************************************
**    I_FUNCTION     : int uig_agcrv_evaluate(evflag, t, eptr, tfmat, evcrv)
**       Evaluate a rational bspline curve at a specified PHYSICAL
**			parameter T.
**    PARAMETERS   
**       INPUT  : 
**				evflag					flag specifying data to evaluate
**				t							physical parameter value
**				eptr						pointer to rational bspline curve
**				tfmat						transformation matrix
**       OUTPUT :  
**				evcrv						pointer to curve evaluator record
**    RETURNS      : 
**			UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_agcrv_evaluate(t, eptr, point)
	UU_REAL t;
	struct UM_agcrv_rec *eptr;
	UU_REAL point[3];

	{
	int status;
	int nd;
	AG_CPOINT cp[3];
	AG_CURVEP cv;
	AG_SPLINEP bs;
	UU_REAL t0, t1, t3;
	UU_REAL dcdu[3], d2cdu2[3];

	status = UU_SUCCESS;

	cv = (AG_CURVEP) eptr->crvaddr;

	/* initialize curve point data structure */
	ag_set_cp2(&cp[0], &cp[1], &cp[2], point, dcdu, d2cdu2);

	nd = 0;

	/* evaluate curve at the specified parameter */
	ag_eval_crv(t, nd, cv, &cp[0]);

	return (status);
	}
/*********************************************************************
**    I_FUNCTION     : int uc_evsrf(evflag, u, v, eptr, tfmat, evsrf)
**       Evaluate a surface.
**    PARAMETERS   
**       INPUT  : 
**            evflag               flag specifying data to evaluate
**            u                    U parameter.
**            v                    V parameter.
**            eptr                 pointer to a surface
**            tfmat                transformation matrix
**       OUTPUT :
**            evsrf                pointer to surface evaluator record
**    RETURNS      : 
**         UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_evsrf(evflag, u, v, eptr, tfmat, evsrf)
   int evflag;
   UM_param u, v;
   struct NCL_fixed_databag *eptr;
   UM_transf tfmat;
   struct UM_evsrfout *evsrf;

   {
   int status;

   status = UU_FAILURE;

	if (eptr->rel_num == NCL_MESHSURF_REL || eptr->rel_num == NCL_SURF_REL)
		status = ncl_eval_surf (evflag, u, v,eptr, tfmat, evsrf);
	else if (eptr->rel_num == NCL_REVSURF_REL)
		status = ncl_eval_revsf (evflag, u, v,eptr, tfmat, evsrf);
	else if (eptr->rel_num == UM_RBSPLSRF_REL)
		status = um_ev9_rbsplsrf(evflag,u,v,eptr,tfmat,evsrf);
/*
     status = ncl_eval_rbsf (evflag, u, v,eptr, tfmat, evsrf);
*/

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_eval_surf (evflag, u, v, eptr, tfmat, evsrf)
**       Evaluate an NCL or Mesh surface.
**    PARAMETERS   
**       INPUT  : 
**          evflag     - evaluation flag.
**          u          - U parameter.
**          v          - V parameter.
**          e          - pointer to surface.
**          tfmat      - transformation matrix.
**       OUTPUT :  
**          evsrf      - evaluation record.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_eval_surf (evflag, u, v, e, tfmat, evsrf)
   int evflag;
   UM_param u, v;
   struct NCL_surface_rec *e;
   UM_transf tfmat;
   struct UM_evsrfout *evsrf;

   {
   int i, status;
   UM_real8  sv[12];
   UM_real4 u4, v4;
   UM_int4 key[2];
   
   status = UU_SUCCESS;
   u4 = u; v4 = v;
   key[0] = e->key;

   if (e->rel_num == NCL_MESHSURF_REL)
       mshevl (key, &u4, &v4, sv);
	else if (e->rel_num == NCL_SURF_REL)
       nsfevl (key, &u4, &v4, sv);

   evsrf->sp[0] = sv[0];
   evsrf->sp[1] = sv[1];
   evsrf->sp[2] = sv[2];
   evsrf->dsdu[0] = sv[6];
   evsrf->dsdu[1] = sv[7];
   evsrf->dsdu[2] = sv[8];
   evsrf->dsdv[0] = sv[9];
   evsrf->dsdv[1] = sv[10];
   evsrf->dsdv[2] = sv[11];
   evsrf->snorm[0] = sv[3];
   evsrf->snorm[1] = sv[4];
   evsrf->snorm[2] = sv[5];

   return (status);
   }

#if 0
/*********************************************************************
**    E_FUNCTION     : int ncl_eval_rbsf (evflag, u, v, eptr, tfmat, evsrf)
**       Evaluate a rational B-Spline surface.
**    PARAMETERS   
**       INPUT  : 
**          evflag     - evaluation flag.
**          u          - U parameter.
**          v          - V parameter.
**          e          - pointer to surface.
**          tfmat      - transformation matrix.
**       OUTPUT :  
**          evsrf      - evaluation record.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_eval_rbsf (evflag, u, v, e, tfmat, evsrf)
   int evflag;
   UM_param u, v;
   struct UM_rbsplsrf_rec *e;
   UM_transf tfmat;
   struct UM_evsrfout *evsrf;

   {
   int i, status;
   UM_int2 m1, m2, nk1, nk2, nd;
   UM_real8 u8, v8, sv[9];
   
   status = UU_SUCCESS;
/*
   u8 = u; v8 = v;
   m1 = e->ku-1; m2 = e->kv-1; nk1 = e->nu+2*m1; nk2 = e->nv+2*m2;
   if (evflag != UM_POINT || e->offdist != 0.0) nd = 1; else nd = 0;
   evrbsf (&m1, &m2, &nk1, &nk2, e->tu, e->tv, e->wt, e->pt, &u8, &v8, &nd, sv);
*/
	status = um_ev9_rbsplsrf(evflag,u,v,e,tfmat,evsrf);
/*

   evsrf->sp[0] = sv[0];
   evsrf->sp[1] = sv[1];
   evsrf->sp[2] = sv[2];
*/
/*   if (tfmat != UM_DEFAULT_TF) um_cctmtf (evsrf->sp, tfmat, evsrf->sp); */

/*
   if (nd>0)
      {
      for (i=0; i<3; i++)
         {
         evsrf->dsdu[i] = sv[i+3];
         evsrf->dsdv[i] = sv[i+6];
         }
*/
/*      if (tfmat != UM_DEFAULT_TF) */
/*         { */
/*         um_vctmtf (evsrf->dsdu, tfmat, evsrf->dsdu); */
/*         um_vctmtf (evsrf->dsdv, tfmat, evsrf->dsdv); */
/*         } 
      if (evflag == UM_NORM || e->offdist != 0.0)
         {
         um_cross (evsrf->dsdu, evsrf->dsdv, evsrf->snorm);
         um_unitvc (evsrf->snorm, evsrf->snorm);
         }
      if (e->offdist != 0.0)
         {
         evsrf->sp[0] += evsrf->snorm[0] * e->offdist;
         evsrf->sp[1] += evsrf->snorm[1] * e->offdist;
         evsrf->sp[2] += evsrf->snorm[2] * e->offdist;
         }
      }
*/

   return (status);
   }
#endif

/*********************************************************************
**    E_FUNCTION     : int gtgeo(nclkey, buf)
**       Retrieve the NCL  geometric representation for any of the
**       legal NCL entities.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtgeo(nclkey, buf)
   UU_KEY_ID *nclkey;
   UM_real8 buf[];

   {
   int status;
   UU_KEY_ID key;
   int rel_num;
   struct NCL_fixed_databag e;
   UU_LOGICAL ncl_legal_relation();

   status = UU_SUCCESS;
   key = *nclkey;
   e.key = key;
   if (ur_retrieve_data_fixed(&e)!= 0)
         status = UU_FAILURE;
   else if (e.rel_num == NCL_MESHSURF_REL)
          ncl_get_meshheader(&e, buf);
        else if (e.rel_num == NCL_SURF_REL)
          ncl_get_srfheader(&e, buf);

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int gtmpat(nclkey, ipatch, buf)
**       Retrieve the data defining a patch (IPATCH) of an NCL mesh
**       surface entity.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**          ipatch            patch to retrieve
**       OUTPUT :
**          buf               buffer to place the data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
gtmpat(nclkey, ipatch, buf)
   UU_KEY_ID *nclkey;
   UM_int2  *ipatch;
   UM_real8 buf[];

   {
   int status;
   int patch;
   struct NCL_meshsf_rec surf;
   struct NCL_mpatch_rec patchdata;

   surf.key = *nclkey;
   patch = *ipatch;
   if (ur_retrieve_data_relnum(surf.key, &surf.rel_num) != 0)
      status = UU_FAILURE;
   else if (surf.rel_num != NCL_MESHSURF_REL)
      status = UU_FAILURE;
   else
      {
      if (ur_retrieve_data_fixed(&surf) != 0)
         status = UU_FAILURE;
      else if ((patch < 1) || (patch > surf.no_mpatch))
         status = UU_FAILURE;
      else
         {
         if (ur_retrieve_data_varlist(surf.key, 1, &patchdata, patch, 1)
              != 0) status = UU_FAILURE;
         else
            {
            ncl_get_meshpatch(&patchdata, buf);
            status = UU_SUCCESS;
            }
         }
      }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_get_meshheader(surf, buf)
**       Move the data for a mesh surface header into an NCLI
**       mesh surface header structure.
**    PARAMETERS   
**       INPUT  : 
**          surf              mesh surface record
**       OUTPUT :  
**          buf               NCLI representation of a mesh surface header
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_meshheader(surf, buf)
   struct NCL_meshsf_rec *surf;
   struct NCLI_meshhead_rec *buf;

   {

   buf->surftype = surf->surf_type;
   buf->numpatches = surf->no_mpatch;
   buf->m = surf->m;
   buf->n = surf->n;
   buf->offset = surf->offset;
   buf->offdist = surf->offdist;
	return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_meshpatch(patch, meshpatch)
**       Move a mesh surface patch into an NCLI internal structure.
**    PARAMETERS   
**       INPUT  : 
**          patch             surface patch
**       OUTPUT :  
**          sfrpatch          NCLI representation of a surface
**                            patch
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_meshpatch(patch, meshpatch)
   struct NCL_mpatch_rec *patch;
   struct NCLI_meshpatch_rec *meshpatch;

   {
   int i,j,k;

   for (i=0;i<3;i++) meshpatch->point[i] = patch->pt[i];
   for (j=0; j<15; j++) 
      {
      for (i=0; i<3; i++) meshpatch->delta[j][i] = patch->delta[j][i];
      }
	return(UU_SUCCESS);
   }
static struct NCL_fixed_databag evalent;
static struct UM_evcrvout evout;
static struct UM_evsrfout evsrf;
static UM_transf evtfmat;
static UM_transf *evtfptr;
/*********************************************************************
**    E_FUNCTION     : int evstup(nclkey)
**       Get the entity with key nclkey into global structure evalent and call
**       um_setup_evaluator to set up global evaluation record evout.
**       Call this routine before calling uevcrv() or uevsrf() with a
**       new entity.
**    PARAMETERS   
**       INPUT  : 
**          nclkey               UNIBASE key of entity to set up.
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
evstup (nclkey)
   UM_int4 *nclkey;
   {
   int iclass, status;

   evalent.key = *nclkey;
   status = ncl_retrieve_data_fixed (&evalent, sizeof(struct NCL_fixed_databag));
   status = um_get_transformation(evalent.key, evtfmat);
   evtfptr = UM_DEFAULT_TF;
   if (!um_is_idmat(evtfmat)) evtfptr = (UM_transf *) evtfmat;  
  
done:;
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : int uevsrf(u,v,sv)
**       Call uc_evsrf() to return the point and first derivative
**       (i.e. slope) at parameters u & v of unicad surface currently in
**       evalent.
**    PARAMETERS   
**       INPUT  : 
**          u                    u parameter of surface to evaluate
**          v                    v parameter of surface to evaluate
**       OUTPUT :  
**          sv[0:2]              point at u,v
**          sv[3:5]              first derivative in u
**          sv[6:8]              first derivative in v
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uevsrf (u,v,sv)
   UM_real8 *u;
   UM_real8 *v;
   UM_real8 sv[9];

   {
   int status, i;
   UM_param uu, vv;
   UM_int2 idx, mm;

   uu = *u;
   vv = *v;
   if (uu < 0.) uu = 0.;
   if (uu > 1.) uu = 1.;
   if (vv < 0.) vv = 0.;
   if (vv > 1.) vv = 1.;
   status = uc_evsrf(UM_FRSTDERIV, uu, vv, &evalent, evtfptr, 
                      &evsrf);
   for (i=0; i<3; i++)
      {
      sv[i] = evsrf.sp[i];
      sv[i+3] = evsrf.dsdu[i];
      sv[i+6] = evsrf.dsdv[i];
      }

   return(status);
   }
/*********************************************************************
**   E_FUNCTION  :  uig_transf_entity (eptr, dblk)
**      Transform an entity.
**    PARAMETERS   
**       INPUT  : 
**         eptr         - pointer to entity.
**         dblk         - directory record of entity.
**       OUTPUT :  
**         none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transf_entity (eptr, dblk)
struct NCL_fixed_databag *eptr;
struct dir_rec *dblk;
{
	int status;
	UU_REAL t[12];

	status = UU_SUCCESS;
	if (dblk->matrix_ptr)
	{
		uig_get_trans(dblk->matrix_ptr,t);
		switch (eptr->rel_num)
		{
		case NCL_MESHSURF_REL:
			status = uig_transf_msf (eptr, t);
			break;
		case UM_RBSPLSRF_REL:
			status = uig_transf_rbsf (eptr, t);
			break;
		default:
			status = UU_FAILURE;
			break;
		}
	}
	return (status);
}
/*********************************************************************
**   E_FUNCTION  :  uig_transf_msf (msf, t)
**      Transform a mesh surface.
**    PARAMETERS   
**       INPUT  : 
**         msf          - pointer to mesh surface
**         t            - transformation matrix
**       OUTPUT :  
**         none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transf_msf (msf, t)
struct NCL_meshsf_rec *msf;
UU_REAL t[12];
{
   int status;     /* status, -1 if error, 0 otherwise    */
   int ipat;
   struct NCL_mpatch_rec patch;

   status = UU_SUCCESS;
 
   for (ipat=1; ipat<=msf->no_mpatch && status == UU_SUCCESS; ipat++)
     {
     if (ur_retrieve_data_varlist (msf->key, 1, &patch, ipat, 1) != 0)
        status = UU_FAILURE;
     else status = uig_transf_mpatch (&patch, t);
     if (status == UU_SUCCESS)
       if (ur_update_data_varlist (msf->key, 1, &patch, ipat, 1) != 0)
         status = UU_FAILURE;
     }

   return(status);
}
/*********************************************************************
**    E_FUNCTION     : int uig_transf_mpatch (patch, tfmat)
**       Transform a mesh surface patch.
**    PARAMETERS
**       INPUT  :
**          patch      - pointer to mesh surface patch
**          t          - transformation matrix.
**       OUTPUT :
**          patch      - pointer to transformed surface patch.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int     
uig_transf_mpatch (patch, t)
   struct NCL_mpatch_rec *patch;
   UU_REAL t[12];
        
   {    
   int i, j, status;
        
   status = UU_SUCCESS;
        
   uig_tran_coor(patch->pt,t,patch->pt);
   for (i=0; i<15; i++)
     {
     uig_tran_vec(patch->delta[i],t,patch->delta[i]);
     }  
        
   return(status);
   }
/*********************************************************************
**   E_FUNCTION  :  uig_transf_rbsf (rsf, t)
**      Transform a rational B-spline surface.
**    PARAMETERS   
**       INPUT  : 
**         rsf          - pointer to rational B-spline surface
**         t            - transformation matrix
**       OUTPUT :  
**         none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_transf_rbsf (rsf, t)
struct UM_rbsplsrf_rec *rsf;
UU_REAL t[12];
{
   int status;
   int i, n;
   UM_coord *p;

   status = UU_SUCCESS;
   n = rsf->no_pt;
   p = (UM_coord *)rsf->pt;
   for (i=0; i<n; i++,p++) uig_tran_coor(p,t,p);
   status = ur_update_data_varlist (rsf->key, 3, rsf->pt, 1, n); 

   return(status);
}
/*********************************************************************
**    E_FUNCTION     : um_ev13_uvcvonsf(evflag, u, eptr, tfmat, evout)
**       Evaluate rational bspline curve on surface at u.
**    PARAMETERS
**       INPUT  :
**          evflag - evaluation flag
**          u      - parameter value
**          eptr   - pointer to rational bspline on surface
**          tfmat  - identity matrix
**       OUTPUT : 
**          evout  - curve evaluation structure
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_ev13_uvcvonsf (evflag, u, eptr, tfmat, evout)
int evflag;
UU_REAL u;
struct UM_uvcvonsf_rec *eptr;
UM_transf tfmat;
struct UM_evcrvout *evout;
{
	struct UM_rbsplcrv_rec rbcv, *rbptr;
	struct NCL_fixed_databag bsrf;
	struct UM_evcrvout uvcv;
	UU_REAL bplm[4];
	int status;

	bsrf.key = eptr->bskey;
	status = ncl_retrieve_data_fixed (&bsrf);
	if (status == UU_SUCCESS)
	{
		rbptr = &rbcv;
		ncl_cp_struct_uvcv_rbcv1(eptr,rbptr);
		bplm[0] = bplm[2] = 0;
		bplm[1] = bplm[3] = 1;
		status = um_ev7_crv_on_surf (evflag,&bsrf,rbptr,bplm,u,tfmat,evout,&uvcv);

	}
return(status);
}

/*********************************************************************
**    E_FUNCTION : ncl_transform_evcrvout(evflag, tfmat, cvoutp)
**       Apply the transformation matrix (TFMAT) to 3 of the defined
**       fields in the surface evaluator output record (SFOUTP).
**    PARAMETERS
**       INPUT  :
**          evflag      UM_POINT=>     point
**                      UM_NORM=>      point, normal
**                      UM_FRSTFERIV=> point, normal, 1st deriv
**          tfmat       transformation matrix
**       OUTPUT :
**          cvoutp      curve evaluator record pointer for results
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_transform_evcrvout(evflag, tfmat, cvoutp)
UM_transf tfmat;
struct UM_evcrvout *cvoutp;
int evflag;
{

	int status = UU_SUCCESS;
	UU_LOGICAL um_is_idmat();
	
	if (!um_is_idmat(tfmat))
	{
		um_cctmtf(cvoutp->cp, tfmat, cvoutp->cp);
		if (evflag >= UM_FRSTDERIV)
			if (evflag >= UM_FRSTDERIV)
			{
				um_vctmtf(cvoutp->dcdu, tfmat, cvoutp->dcdu);
				if (evflag >= UM_SECDERIV)
					um_vctmtf(cvoutp->d2cdu2, tfmat, cvoutp->d2cdu2);
			}
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION : ncl_transform_evsfout(evflag, tfmat, sfoutp)
**       Apply the transformation matrix (TFMAT) to 3 of the defined
**       fields in the surface evaluator output record (SFOUTP).
**    PARAMETERS  
**       INPUT  :
**          evflag      UM_POINT=>     point
**                      UM_NORM=>      point, normal
**                      UM_FRSTFERIV=> point, normal, 1st deriv
**          tfmat       transformation matrix
**       OUTPUT : 
**          sfoutp      surface evaluator record pointer for results
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_transform_evsfout(evflag, tfmat, sfoutp)
int evflag;
UM_transf tfmat;
struct UM_evsrfout *sfoutp;
{
	int status = UU_SUCCESS;
	UU_LOGICAL um_is_idmat();
	
	if (!um_is_idmat(tfmat))
	{
		um_cctmtf(sfoutp->sp, tfmat, sfoutp->sp);
		if (evflag >= UM_NORM)
		{
			um_vctmtf(sfoutp->snorm, tfmat, sfoutp->snorm);
			if (evflag >= UM_FRSTDERIV)
			{
				um_vctmtf(sfoutp->dsdu, tfmat, sfoutp->dsdu);
				um_vctmtf(sfoutp->dsdv, tfmat, sfoutp->dsdv);
			}
		}
	}
	return(status);
}

