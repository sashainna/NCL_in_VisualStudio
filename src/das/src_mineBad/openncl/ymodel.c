/*********************************************************************
**    NAME         :  ymodel.c
**       CONTAINS:
**			NclxMdlInquire(lab,rel,key,scalar)
**			NclxMdlSetupData(rec,relnum,label,sub)
**			int NclxMdlEvalCurve(cv,u,eval)
**			int NclxMdlEvalSurf(surf,u,v,eval,evflag)
**			NclxMdlInterpCurve(cvpt,n_in,knpt,cvpt,n_out)
**			NclxMdlGetCone(p,pt0,q)
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ymodel.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:00
*********************************************************************/
#include "usysdef.h"
#include "mdcoord.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mfort.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclx.h"
#include "nclxmdl.h"
/*
... jingrong 08/03/99 keep YCOMGBL in ymotmdl.c.
#define YCOMGBL
*/
#include "ycom.h"
/*#undef YCOMGBL*/

#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D)
#ifndef UU_RS6000
#define uevsff uevsff_
#define uevsft uevsft_
#endif
#endif

/*int NCLX_internal_geom = 0;...jingrong 08/03/99*/
extern int NCLX_internal_geom;
static struct UM_evsrfout evsrf;
static struct UM_evcrvout evcrv;

char *uu_malloc();

/*********************************************************************
**    E_FUNCTION     : NclxMdlInquire(lab,rel,key,scalar)
**       Inquires whether an entity exists in the Unibase and returns
**			the relation number if it does.
**    PARAMETERS
**       INPUT  :
**          lab               Label of entity inquiring about.
**       OUTPUT :
**          rel               Relation number of entity if it exists.
**				key               Unibase key of entity.
**				scalar            Value if entity is scalar.
**    RETURNS      :
**       NCLX_SUCCESS if entity exists, else NCLX_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlInquire(lab,rel,key,scalar)
char *lab;
NCLX_KEY_ID *key;
NCLX_mdl_type *rel;
double *scalar;
{
	char mlab[64];
	struct NCL_scalar_rec e;
	UM_f77_str f77_str;
	int nc,stat,relnum;
/*
.....Check for numeric data
*/
	stat = NCLX_SUCCESS;
	*rel = NCLX_MDL_UNDEF;
	*key = 0;
	if (ul_to_reals(scalar,&nc,1,lab) == UU_SUCCESS)
	{
		*rel = NCLX_MDL_REAL;
		return(stat);
	}
/*
.....Get the Unibase key for the input label
*/
	strcpy(mlab,lab);
	UM_init_f77_str(f77_str,mlab,64);
	getkey(UM_addr_of_f77_str(f77_str),key);
	if (*key == 0)
	{
		stat = NCLX_FAILURE;
	}
/*
.....Get the geometry type
*/
	else
	{
		stat = ur_retrieve_data_relnum(*key,&relnum);
		if (stat != 0) stat = NCLX_FAILURE;
		else
		{
			switch (relnum)
			{
			case UM_CIRCLE_REL:
			case NCL_CIRCLE_REL:
				*rel = NCLX_MDL_CIRCLE;
				break;
			case UM_CONIC_REL:
				*rel = NCLX_MDL_CONIC;
				break;
			case UM_COMPCRV_REL:
				*rel = NCLX_MDL_COMPOSITE;
				break;
			case NCL_CURVE_REL:
				*rel = NCLX_MDL_CURVE;
				break;
			case NCL_DATAST_REL:
				*rel = NCLX_MDL_DATA;
				break;
			case UM_LINE_REL:
			case NCL_LINE_REL:
				*rel = NCLX_MDL_LINE;
				break;
			case NCL_MATRIX_REL:
				*rel = NCLX_MDL_MATRIX;
				break;
			case NCL_PATERN_REL:
				*rel = NCLX_MDL_PATERN;
				break;
			case NCL_PLN_REL:
				*rel = NCLX_MDL_PLANE;
				break;
			case UM_POINT_REL:
			case NCL_POINT_REL:
				*rel = NCLX_MDL_POINT;
				break;
			case NCL_POINTVEC_REL:
				*rel = NCLX_MDL_PNTVEC;
				break;
			case UM_RBSPLCRV_REL:
			case NCL_EVALCV_REL:
			case UM_UVCVONSF_REL:
				*rel = NCLX_MDL_BSPLINE;
				break;
			case UM_RBSPLSRF_REL:
			case NCL_EVALSF_REL:
				*rel = NCLX_MDL_NSURF;
				break;
			case NCL_SCALAR_REL:
				*rel = NCLX_MDL_SCALAR;
				if (ur_retrieve_data(&e,sizeof(e)) != 0) stat = NCLX_FAILURE;
				else *scalar = e.scalar_value;
				break;
			case NCL_SURF_REL:
				*rel = NCLX_MDL_SURF;
				break;
			case NCL_TRIMSF_REL:
				*rel = NCLX_MDL_TRIMSF;
				break;
			case NCL_VECTOR_REL:
				*rel = NCLX_MDL_VECTOR;
				break;
			default:
				*rel = (NCLX_mdl_type) relnum;
				stat = NCLX_FAILURE;
			}
		}
	}
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlSetupData(rec,relnum,label,sub)
**       Initializes a geometric entity's fixed data structure and
**			the attribute record.
**    PARAMETERS
**       INPUT  :
**          rec               Entity record to initialize.
**          relnum            Relation number of entity.
**          label             Label of entity.
**          sub               Label subscript of entity.
**       OUTPUT :
**          rec               Entity record to initialize.
**    RETURNS      :
**       none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void NclxMdlSetupData(rec,relnum,label,sub)
NCLX_mdl_struct *rec;
NCLX_mdl_type relnum;
char *label;
int sub;
{
/*
.....Setup the default modelling structure
*/
	rec->key = 0;
	rec->relnum = relnum;
	strcpy(rec->label,label);
	rec->subscript = sub;
	rec->label_pos[0] = 0.;
	rec->label_pos[1] = 0.;
	rec->label_pos[2] = 0.;
/*
.....Setup the default attribute block
*/
	(*rec).attrib.color = NCLX_ORANGE;
	(*rec).attrib.layer = 1;
	(*rec).attrib.pen = 1;
	(*rec).attrib.line_style = NCLX_SOLID;
	(*rec).attrib.line_weight = NCLX_STANDARD;
	(*rec).attrib.visible = NCLX_FALSE;
	(*rec).attrib.label_on = NCLX_FALSE;
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     : int NclxMdlEvalCurve(cv,u,eval)
**       Curve evaluator routine.
**    PARAMETERS
**       INPUT  :
**          cv                Curve to evaluate.
**          u                 U parameter to evaluate at.
**       OUTPUT :
**          eval              Evaluator record.
**    RETURNS      :
**       none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMdlEvalCurve(cv,u,eval)
NCLX_mdl_curve *cv;
double u;
NCLX_mdl_curve_eval *eval;
{
	UM_param up;
	UM_transf tfmat;
	int stat,i,isav;
/*
.....Initialize the evaluator
.....If this is the first time for this curve
*/
/*	if ((*cv).header.key != eval->key ) um_init_evcrvout(surf,&evcrv);*/
/*
.....Let evaluator know that this is an
.....NCLX style curve
*/
	stat = UU_SUCCESS;
	isav = NCLX_internal_geom;
	NCLX_internal_geom = UU_TRUE;
/*
.....Get the transformation matrix
*/
	stat = um_get_transformation((*cv).header.key,tfmat);
/*
.....Evaluate the curve
*/
	up = u;
	if ((*cv).header.relnum == NCLX_MDL_CURVE)
		stat = ncl_ev_curve(UM_CURVATURE,up,cv,tfmat,&evcrv);
	else
		stat = um_ev7_rbsplcrv(UM_CURVATURE,up,cv,tfmat,&evcrv);
	if(stat!=UU_SUCCESS) goto done;
/*
.....Return the evaluation
*/
	NCLX_internal_geom = isav;
/*	eval->key = (*cv).header.key;*/
	for (i=0;i<3;i++)
	{
		eval->pt[i] = evcrv.cp[i];
		eval->udrv1[i] = evcrv.dcdu[i];
		eval->udrv2[i] = evcrv.d2cdu2[i];
	}
	eval->ucrv = evcrv.curv;
done:;
/*
.....Need to return a status.  JLS 11/8/99
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMdlEvalSurf(surf,u,v,eval,evflag)
**       Surface evaluator routine.
**    PARAMETERS
**       INPUT  :
**          surf              Surface to evaluate.
**          u                 U parameter to evaluate at.
**          v                 V parameter to evaluate at.
**          evflag            1 - evaluate point, normal, first derivatives
**                            2 - evaluate also curvatures
**       OUTPUT :
**          eval              Evaluator record.
**    RETURNS      :
**       NCLX_SUCCESS if OK, else NCLX_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
NclxMdlEvalSurf(surf,u,v,eval,evflag)
NCLX_mdl_surf *surf;
double u,v;
NCLX_mdl_surf_eval *eval;
int evflag;
{
	UM_param up,vp;
	UM_transf tfmat;
	int stat,i,isav,nclflag;
/*
.....Initialize the evaluator
.....If this is the first time for this surface
*/
   stat = NCLX_SUCCESS;
   
/*	if ((*surf).header.key != eval->key ) um_init_evsrfout(surf,&evsrf);*/
/*
.....Let evaluator know that this is an
.....NCLX style surface
*/
	isav = NCLX_internal_geom;
	NCLX_internal_geom = UU_TRUE;
/*
.....Get the transformation matrix
*/
	stat = um_get_transformation((*surf).header.key,tfmat);
   if (stat != UU_SUCCESS) goto done;
/*
.....Evaluate the surface
*/
	up = u; vp = v;
	nclflag = (evflag <= UM_FRSTDERIV)? UM_FRSTDERIV: UM_ALL;
	if ((*surf).header.relnum == NCLX_MDL_SURF)
		stat = ncl_eval_surf(nclflag,up,vp,surf,tfmat,&evsrf);
	else
		stat = um_ev9_rbsplsrf(nclflag,up,vp,surf,tfmat,&evsrf);
   if (stat != UU_SUCCESS) goto done;
/*
.....Return the evaluation
*/
	NCLX_internal_geom = isav;
	eval->key = (*surf).header.key;
	for (i=0;i<3;i++)
	{
		eval->pt[i] = evsrf.sp[i];
		eval->normal[i] = evsrf.snorm[i];
		eval->udrv1[i] = evsrf.dsdu[i];
		eval->vdrv1[i] = evsrf.dsdv[i];
	}
	if (evflag > UM_FRSTDERIV)
	{
		for (i=0;i<3;i++)
		{
			eval->udrv2[i] = evsrf.d2sdu2[i];
			eval->vdrv2[i] = evsrf.d2sdv2[i];
		}
		eval->ucrv = evsrf.ucurv;
		eval->vcrv = evsrf.vcurv;
	}

done:;
   return(stat);

}

int NclxMdlInterpCurve(curve,pts,n_in)
NCLX_mdl_curve *curve;
int n_in;
double *pts;
{
	int i,npt,jpt,status;
	struct NCL_crvgen_rec *cvgen;
	double *knpt,*cvpt;
/*
.....Allocate memory for cvgen structure
*/
	cvgen = (struct NCL_crvgen_rec *)
		uu_malloc(sizeof(struct NCL_crvgen_rec)*n_in);
/*
.....Create cvgen structure
*/
	for (i=0,jpt=0;i<n_in;i++,jpt=jpt+3)
	{
		cvgen[i].x = pts[jpt];
		cvgen[i].y = pts[jpt+1];
		cvgen[i].z = pts[jpt+2];
		cvgen[i].inv = 0;
	}
/*
.....Calculate knot & control points for curve
*/
	status = ncl_interp_rbsp1(n_in,cvgen,1,&npt,&knpt,&cvpt);
/*
.....Store curve structure
*/
	if (status == UU_SUCCESS)
	{
		status = NCLX_SUCCESS;
		curve->cvhead.degree = 4;
		curve->cvhead.degseg = npt - 3;
		curve->cvhead.t0 = 0.;
		curve->cvhead.t1 = knpt[npt+3];
		curve->cvhead.eval = (curve->cvhead.t1 - curve->cvhead.t0) / 2.;
		curve->cvhead.tlen = 0.;
		curve->evaluator = 0;
		curve->nparm = 0;
		curve->parms = 0;
		curve->ntparm = npt + 4;
		curve->tparms = knpt;
		curve->nwgt = 0;
		curve->wgt = 0;
		curve->npt = npt;
		curve->pt = cvpt;
		curve->nsegment = 0;
		curve->segment = 0;
	}
	else
		status = NCLX_FAILURE;
/*
.....Free allocated memory
*/
	uu_free(cvgen);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : void SW_NCL_cone (p,pt0,q)
**    PARAMETERS
**       INPUT  :
**              p[0-2]  apex point
**              p[3-5]  axis vector
**              p[6]    radius
**              p[7]    half-angle
**              pt0     point on top
**       OUTPUT :
**              q[0-8]  - NCL cone primitive parameters
**    RETURNS      :
**              
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void NclxMdlGetCone (p,pt0,q)
UU_REAL p[8],pt0[3],q[16];
{
	int i;
	UU_REAL H;
	UM_angle alpha;
	UM_vector vec;

	for (i = 0; i < 3; i++)
	{
		q[i] = p[i];
		q[i+3] = p[i+3];
		vec[i] = pt0[i] - p[i];
	}
	if (vec[0]*p[3] + vec[1]*p[4] + vec[2]*p[5] < 0.)
	{
		q[3] = -q[3]; q[4] = -q[4]; q[5] = -q[5];
	}
	alpha = q[6] = p[7]; /* half-angle */

	H = p[6]/tan(alpha); /* full height - distance from apex to bottom circle */
	q[8] = UM_MAG (vec); /* distance from apex to top circle */
	q[7] = H - q[8];     /* distance between top and bottom circles */
}
