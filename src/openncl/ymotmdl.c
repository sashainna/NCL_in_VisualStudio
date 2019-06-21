/*********************************************************************
**    NAME         :  ymotmdl.c
**       CONTAINS:
**
**				NclxMdlToNclType
**				NclxMdlToRelnum
**				uevcvt
**				uevsft
**				NclxMdlRetrieveGeo
**				NclxMdlFindPatern
**				NclxMdlPaternSearch
**				NclxMdlGetClosed
**				NclxMdlGetRuled
**				NclxMdlEvalSetup
**				NclxMdlEvalLine
**				NclxMdlEvalCircle
**				NclxMdlEvalComposite
**				nclx_smoot_comp
**				NclxMdlEvalPolyline
**				nclx_smoot_polyln
**				NclxMdlInitPolylineEval
**				NclxMdlFindGeo
**				NclxMdlFindCompositeGeo
**				NclxMdlFree
**				ncl_dscs_swap
**				ncl_dscs_restore
**				nclx_rmill_swap
**				nclx_eval_corner
**				smtset
**				smtrst
**
**    COPYRIGHT 1996 (c) Numerical Control Computer Sciences Inc.
**              All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ymotmdl.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:00
*********************************************************************/
#include "usysdef.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mfort.h"
#include "modef.h"
#include "nccs.h"
#include "ncl.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclfc.h"
#define YCOMGBL
#include "ycom.h"
#undef YCOMGBL

/*extern int NCLX_internal_geom;...jingrong 08/03/99*/
int NCLX_internal_geom = 0;
static NCLX_KEY_ID NCLX_polyln_key = 0;
static double NCLX_polyln_length = -1.;
static int NCLX_polyln_npts = 0;
static UU_REAL DU = 0.1;
static NCLX_KEY_ID NCLX_smoot_key = 0;

/*********************************************************************
**		FUNCTION     : int NclxMdlToNclType (rel)
**			This function converts an OpenNCL geometry type to an NCL
**			geometry type.
**		PARAMETERS
**		INPUT  :
**			rel          OpenNCL geometry type.
**		OUTPUT :
**			ierr         Return status, non-zero on error.
**		RETURNS      :
**			NCL geometry type.
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlToNclType (rel)
NCLX_mdl_type rel;
{
	int stat;
/*
.....Convert NCLX model type to NCL type
*/
	switch (rel)
	{
	case NCLX_MDL_CIRCLE:
		stat = NCLG_CIRCLE;
		break;
	case NCLX_MDL_CONIC:
	case NCLX_MDL_COMPOSITE:
	case NCLX_MDL_CURVE:
	case NCLX_MDL_BSPLINE:
		stat = NCLG_CURVE;
		break;
	case NCLX_MDL_LINE:
		stat = NCLG_LINE;
		break;
	case NCLX_MDL_MATRIX:
		stat = NCLG_MATRIX;
		break;
	case NCLX_MDL_PATERN:
		stat = NCLG_PATERN;
		break;
	case NCLX_MDL_PLANE:
		stat = NCLG_PLANE;
		break;
	case NCLX_MDL_POINT:
		stat = NCLG_POINT;
		break;
	case NCLX_MDL_PNTVEC:
		stat = NCLG_PNTVEC;
		break;
	case NCLX_MDL_NSURF:
	case NCLX_MDL_SURF:
	case NCLX_MDL_TRIMSF:
		stat = NCLG_SURF;
		break;
	case NCLX_MDL_NETSF:
		stat = NCLG_NETSF;
		break;
	case NCLX_MDL_SCALAR:
		stat = NCLG_SCALAR;
		break;
	case NCLX_MDL_SHAPE:
		stat = NCLG_SHAPE;
		break;
	case NCLX_MDL_VECTOR:
		stat = NCLG_VECTOR;
		break;
	case NCLX_MDL_POLYLINE:
		stat = NCLG_POLYLINE;
		break;
	default:
		stat = NCLG_UNKNOWN;
		break;
	}
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**		FUNCTION     : int NclxMdlToRelnum (rel)
**			This function converts an OpenNCL geometry type to an NCL
**			geometry type.
**		PARAMETERS
**		INPUT  :
**			rel          OpenNCL geometry type.
**		OUTPUT :
**			ierr         Return status, non-zero on error.
**		RETURNS      :
**			NCL geometry type.
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlToRelnum (rel)
NCLX_mdl_type rel;
{
	int stat;
/*
.....Convert NCLX model type to NCL type
*/
	switch (rel)
	{
	case NCLX_MDL_CIRCLE:
		stat = UM_CIRCLE_REL;
		break;
	case NCLX_MDL_CONIC:
		stat = UM_CONIC_REL;
		break;
	case NCLX_MDL_COMPOSITE:
		stat = UM_COMPCRV_REL;
		break;
	case NCLX_MDL_CURVE:
		stat = NCL_CURVE_REL;
		break;
	case NCLX_MDL_BSPLINE:
		stat = UM_RBSPLCRV_REL;
		break;
	case NCLX_MDL_LINE:
		stat = UM_LINE_REL;
		break;
	case NCLX_MDL_MATRIX:
		stat = NCL_MATRIX_REL;
		break;
	case NCLX_MDL_PATERN:
		stat = NCL_PATERN_REL;
		break;
	case NCLX_MDL_PLANE:
		stat = NCL_PLN_REL;
		break;
	case NCLX_MDL_POINT:
		stat = UM_POINT_REL;
		break;
	case NCLX_MDL_PNTVEC:
		stat = NCL_POINTVEC_REL;
		break;
	case NCLX_MDL_NSURF:
	case NCLX_MDL_SURF:
		stat = UM_RBSPLSRF_REL;
		break;
	case NCLX_MDL_TRIMSF:
		stat = NCL_TRIMSF_REL;
		break;
	case NCLX_MDL_NETSF:
		stat = NCL_NETSF_REL;
		break;
	case NCLX_MDL_SCALAR:
		stat = NCL_SCALAR_REL;
		break;
	case NCLX_MDL_SHAPE:
		stat = NCL_SHAPE_REL;
		break;
	case NCLX_MDL_VECTOR:
		stat = NCL_VECTOR_REL;
		break;
	case NCLX_MDL_POLYLINE:
		stat = UM_POLYLINE_REL;
		break;
	default:
		stat = UM_UNKNOWN;
		break;
	}
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**		FUNCTION     : uevcvt (u,isf,irs,p,v,ierr)
**			This is the buffer to the curve evaluator routine.
**		PARAMETERS
**		INPUT  :
**			u            U parameter of curve to evaluate.
**			isf          Which curve to evaluate.
**		OUTPUT :
**			irs          See uevcvv.
**			p            Evaluated point.
**			v            First derivative.
**			ierr         Return status, non-zero on error.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int uevcvt (u,isf,irs,p,v,ierr)
UM_int2 *ierr;
double *u,p[3],v[3];
UM_int2 *isf, *irs;
{
	int i, kerr = 0;
	int (*func)();
	int NclxMdlEvalLine(), NclxMdlEvalCircle(), NclxMdlEvalComposite();
	int NclxMdlEvalPolyline();
	UM_int2 idx,ival;
	NCLX_mdl_curve_eval eval;
	NCLX_mdl_curve *ncv;
	NCLX_mdl_composite *ccv;

	if (NCLX_internal_geom)
	{
		if (*isf == 1) ncv = (NCLX_mdl_curve *)UY_ps;
		else if (*isf == 2) ncv = (NCLX_mdl_curve *)UY_ds;
		else if (*isf == 3) ncv = (NCLX_mdl_curve *)UY_cs[UY_ics];
		else if (*isf == 4) ncv = (NCLX_mdl_curve *)UY_sf4;
		switch (ncv->header.relnum)
		{
		case NCLX_MDL_CURVE:
		case NCLX_MDL_BSPLINE:
			func = (int(*) ())ncv->evaluator;
			break;
		case NCLX_MDL_COMPOSITE:
			ccv = (NCLX_mdl_composite *)ncv;
			func = (int(*) ())ccv->evaluator;
			break;
		case NCLX_MDL_LINE:
			func = NclxMdlEvalLine;
			break;
		case NCLX_MDL_CIRCLE:
			func = NclxMdlEvalCircle;
			break;
		case NCLX_MDL_POLYLINE:
			func = NclxMdlEvalPolyline;
			break;
		}

		if (*u < 0.) *u = 0.;
		if (*u > 1.0) *u = 1.0;

		kerr = (*(func))(ncv,*u,&eval);
#ifdef TEMP_FIX
		kerr = 0; /* TEMP-FIX */
#endif
		if (kerr == 0)
		{
			for (i=0;i<3;i++)
			{
				p[i] = eval.pt[i];
				v[i] = eval.udrv1[i];
			}
		}
		else 
			kerr = 466;
	}
	else
	{
		uevcvv(u,isf,irs,p,v);
	}
	if (kerr == 0)
	{
		idx = 2; getifl(&idx,&ival); kerr = ival;
	}
	*ierr = kerr;
	NclxDbgPmdlEvalcv (*u,ncv,&eval);
	NclxDbgEvalRtn("Curve",kerr);
	return (0);
}

/*********************************************************************
**		FUNCTION     : uevsft (u,v,isf,sv,ierr)
**			This is the buffer to the surface evaluator routine.
**		PARAMETERS
**		INPUT  :
**			u            U parameter of surface to evaluate.
**			v            V parameter of surface to evaluate.
**			isf          Which surface to evaluate.
**		OUTPUT :
**			sv           Evaluated surface point and derivatives.
**			ierr         Return status, non-zero on error.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int uevsft (u,v,isf,sv,ierr)
UM_int2 *ierr;
double *u,*v,sv[9];
UM_int2 *isf;
{
	int i;
	int (*func)();
	int kerr=0;
	UM_int2 idx,ival;
	NCLX_mdl_surf_eval eval;
	NCLX_mdl_surf *nsf;
	NCLX_mdl_trimsf *tsf;

	if (NCLX_internal_geom)
	{
		if (*isf == 1) nsf = (NCLX_mdl_surf *)UY_ps;
		else if (*isf == 2) nsf = (NCLX_mdl_surf *)UY_ds;
		else if (*isf == 3) nsf = (NCLX_mdl_surf *)UY_cs[UY_ics];
		if (nsf->header.relnum == NCLX_MDL_TRIMSF)
		{
			tsf = (NCLX_mdl_trimsf *)nsf;
			nsf = tsf->surf;
		}
		func = (int(*) ())nsf->evaluator;
		if (*u < 0.) *u = 0.;
		if (*u > 1.0) *u = 1.0;
		if (*v < 0.) *v = 0.;
		if (*v > 1.0) *v = 1.0;
		kerr = (*(func))(nsf,*u,*v,&eval,1);
#ifdef TEMP_FIX
		kerr = 0; /* TEMP-FIX */
#endif
		if (kerr == 0)
		{
			for (i=0;i<3;i++)
			{
				sv[i] = eval.pt[i];
				sv[i+3] = eval.udrv1[i];
				sv[i+6] = eval.vdrv1[i];
			}
		}
		else kerr = 466;
	}
	else
	{
		uevsff(u,v,isf,sv);
	}
	if (kerr == 0)
	{
		idx = 2; getifl(&idx,&ival); kerr = ival;
	}
	*ierr = kerr;
	NclxDbgPmdlEvalsf (*u,*v,nsf,&eval);
	NclxDbgEvalRtn("Surface",kerr);
	return (0);
}

/*********************************************************************
**		E_FUNCTION     : int NclxMdlRetrieveGeo(key,dbuf)
**			Retrieves the geometry structure from the DS,PS, or CS.
**		PARAMETERS
**		INPUT  :
**			key          Key id of geo to retrieve.
**		OUTPUT :
**			dbuf         Geometry structure.
**		RETURNS      :
**			NCLX_FAILURE on error.
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlRetrieveGeo(key,dbuf)
NCLX_KEY_ID key;
double dbuf[];
{
	int stat,i;
	NCLX_mdl_struct *geo;
	NCLX_mdl_point *pt;
	NCLX_mdl_line *ln;
	NCLX_mdl_plane *pl;
	NCLX_mdl_pntvec *pv;
	NCLX_mdl_circle *ci;
	NCLX_mdl_netsf *nsf;
	struct NCLI_netsf_rec *nsfrec;
	UM_vector vtmp;
	UM_real8 hang, co, si;
/*
.....Search for geometry in PS,DS,CS
*/
	geo = 0;
	dbuf[0] = 0;
	stat = NclxMdlFindGeo (key,&geo);
	if (!geo || stat != NCLX_SUCCESS) goto failed;
/*
.....Transfer the geometry
*/
	switch (geo->relnum)
	{
	case NCLX_MDL_POINT:
		pt = (NCLX_mdl_point *)geo;
		um_vctovc (pt->pt,dbuf);
		break;
	case NCLX_MDL_LINE:
		ln = (NCLX_mdl_line *)geo;
		um_vctovc (ln->spt, dbuf);
		um_vcmnvc (ln->ept, ln->spt, &dbuf[3]);
		break;
	case NCLX_MDL_PLANE:
		pl = (NCLX_mdl_plane *)geo;
		um_vctovc (pl->vec,dbuf);
		dbuf[3] = um_dot (pl->pt, pl->vec);
		break;
	case NCLX_MDL_PNTVEC:
		pv = (NCLX_mdl_pntvec *)geo;
		um_vctovc (pv->pt,dbuf);
		um_vctovc (pv->vec,&dbuf[3]);
		break;
	case NCLX_MDL_CIRCLE:
		ci = (NCLX_mdl_circle *)geo;
		um_vctovc (ci->center, dbuf);
		um_vctovc (ci->nvec  , &dbuf[3]);
		if (ci->dang < 0.0) um_vctmsc (&dbuf[3], -1.0, &dbuf[3]);
		dbuf[6] = ci->radius;
		if (fabs(UM_TWOPI - fabs(ci->dang)) < UM_FUZZ)
		{
			dbuf[7] = dbuf[8] = dbuf[9] = dbuf[10] = 0.0;
		}
		else
		{
			um_cross (ci->nvec, ci->svec, vtmp);
			hang = ci->dang / 2.0;
			co = cos(hang);
			si = sin(hang);
			dbuf[7] = co*ci->svec[0]+si*vtmp[0];
			dbuf[8] = co*ci->svec[1]+si*vtmp[1];
			dbuf[9] = co*ci->svec[2]+si*vtmp[2];
			um_vctmsc (ci->svec, ci->radius, vtmp);
			um_vcplvc (ci->center, vtmp, vtmp);
			dbuf[10] = um_dot (vtmp, &dbuf[7]);
		}
		break;
	case NCLX_MDL_NETSF:
		nsf = (NCLX_mdl_netsf *)geo;
		nsfrec = (struct NCLI_netsf_rec *)dbuf;
		nsfrec->surftype = NCLI_NETSF;
		nsfrec->numsfs = nsf->nsf;
		for (i=0;i<nsf->nsf;i++)
		{
			geo = (NCLX_mdl_struct *)nsf->sfptr[i];
			nsfrec->sfkeys[i] = geo->key;
		}
		break;
	}
	goto done;
/*
.....End of routine
*/
failed:;
	stat = NCLX_FAILURE;
done:;
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : int NclxMdlFindPatern(key,dbuf)
**			Searches for a Patern in the DS, PS, and CS structures.
**		PARAMETERS
**		INPUT  :
**			key          Key id of geo to retrieve.
**		OUTPUT :
**			pat          Pointer to Pattern entity.
**		RETURNS      :
**			NCLX_FAILURE when 'key' pattern could not be found.
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlFindPatern(key,pat)
NCLX_KEY_ID key;
NCLX_mdl_patern **pat;
{
	int i,stat;
/*
.....Search for geometry in PS,DS,CS
*/
	stat = NclxMdlPaternSearch(UY_ps,key,pat);
	if (stat == NCLX_SUCCESS) return(stat);
	stat = NclxMdlPaternSearch(UY_ds,key,pat);
	if (stat == NCLX_SUCCESS) return(stat);
	for (i=0;i<UY_ncs;i++)
	{
		stat = NclxMdlPaternSearch(UY_cs[i],key,pat);
		if (stat == NCLX_SUCCESS) return(stat);
	}
	for (i=0;i<UY_nislands;i++)
	{
		stat = NclxMdlPaternSearch(UY_island[i],key,pat);
		if (stat == NCLX_SUCCESS) return(stat);
	}
	return (stat);
}

/*********************************************************************
**		E_FUNCTION     : int NclxMdlPaternSearch(ds,key,pat)
**			Locates a Patern in the as the structure or as a sub-type of
**			the input structure, for example, the bounding curve of trimmed
**			surface.
**		PARAMETERS
**		INPUT  :
**			ds           Data structure to search in for the pattern.
**			key          Key number of patern to search for.
**		OUTPUT :
**			pat          Pointer to Pattern entity.
**		RETURNS      :
**			NCLX_FAILURE when 'key' pattern could not be found. NCLX_SUCCESS
**			if the pattern was found.
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlPaternSearch(ds,key,pat)
NCLX_mdl_struct *ds;
NCLX_KEY_ID key;
NCLX_mdl_patern **pat;
{
	int i,stat;
	NCLX_mdl_trimsf *tsf;
	NCLX_mdl_netsf *nsf;
	NCLX_mdl_patern *tmp;
/*
.....Initialize routine
*/
	stat = NCLX_FAILURE;
/*
.....Is the entity the correct pattern?
*/
	if (ds->relnum == NCLX_MDL_PATERN && key == ds->key)
	{
		*pat = (NCLX_mdl_patern *)ds;
		return(NCLX_SUCCESS);
	}
/*
.....Check in a trimmed surface structure
*/
	if (ds->relnum == NCLX_MDL_TRIMSF)
	{
		tsf = (NCLX_mdl_trimsf *)ds;
		tmp = (NCLX_mdl_patern *)tsf->uv_cv;
		if (tmp->header.relnum == NCLX_MDL_PATERN &&
			key == tmp->header.key)
		{
			*pat = tmp;
			return(NCLX_SUCCESS);
		}
		tmp = (NCLX_mdl_patern *)tsf->xyz_cv;
		if (tmp->header.relnum == NCLX_MDL_PATERN &&
			key == tmp->header.key)
		{
			*pat = tmp;
			return(NCLX_SUCCESS);
		}
	}
/*
.....Check in a net surface structure
*/
	if (ds->relnum == NCLX_MDL_NETSF)
	{
		nsf = (NCLX_mdl_netsf *)ds;
		for (i=0;i<nsf->nsf;i++)
		{
			tsf = (NCLX_mdl_trimsf *)nsf->sfptr[i];
			tmp = (NCLX_mdl_patern *)tsf->uv_cv;
			if (tmp->header.relnum == NCLX_MDL_PATERN &&
				key == tmp->header.key)
			{
				*pat = tmp;
				return(NCLX_SUCCESS);
			}
			tmp = (NCLX_mdl_patern *)tsf->xyz_cv;
			if (tmp->header.relnum == NCLX_MDL_PATERN &&
				key == tmp->header.key)
			{
				*pat = tmp;
				return(NCLX_SUCCESS);
			}
		}
	}
	return (stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMdlGetClosed(key,flag,closed)
**			Retrieves the geometry closed flag for curves and surfaces.
**		PARAMETERS
**		INPUT  :
**			key          Key id of geo to retrieve.
**			flag         0 = Closed flag in U , 1 = V.
**		OUTPUT :
**			closed       1 = Geometry is closed.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlGetClosed(key,flag,closed)
NCLX_KEY_ID key;
UM_int2 flag,*closed;
{
	NCLX_mdl_data *geo;
	NCLX_mdl_surf *sfp;

	*closed = 0;
	if (key == UY_ps->key) geo = (NCLX_mdl_data *)UY_ps;
	else if (key == UY_ds->key) geo = (NCLX_mdl_data *)UY_ds;
	else if (key == UY_cs[UY_ics]->key) geo = (NCLX_mdl_data *)UY_cs[UY_ics];
	else goto done;
/*
.....Get closed flag
*/
	switch (geo->data.header.relnum)
		{
		case NCLX_MDL_CONIC:
		case NCLX_MDL_CURVE:
		case NCLX_MDL_BSPLINE:
			*closed = geo->data.cv.cvhead.closed;
			break;
		case NCLX_MDL_COMPOSITE:
			*closed = geo->data.cp.cvhead.closed;
			break;
		case NCLX_MDL_NSURF:
		case NCLX_MDL_SURF:
			if (flag == 0) *closed = (*geo).data.sf.sfhead.uclosed;
			else if (flag == 1) *closed = (*geo).data.sf.sfhead.vclosed;
			break;
		case NCLX_MDL_TRIMSF:
			sfp = geo->data.tf.surf;
			if (flag == 0) *closed = sfp->sfhead.uclosed;
			else if (flag == 1) *closed = sfp->sfhead.vclosed;
			break;
		default:
			break;
		}
done:;
	return (0);
}

/*********************************************************************
**		E_FUNCTION     : NclxMdlGetRuled(key,ruled)
**			Retrieves the geometry ruled flag for surfaces.
**		PARAMETERS
**		INPUT  :
**			key          Key id of geo to retrieve.
**		OUTPUT :
**			ruled        0 = Geometry is ruled in U, 1 = V.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlGetRuled(key,ruled)
NCLX_KEY_ID key;
UM_int2 *ruled;
{
	NCLX_mdl_surf *geo;

	*ruled = 0;
	if (key == UY_ps->key) geo = (NCLX_mdl_surf *)UY_ps;
	else if (key == UY_ds->key) geo = (NCLX_mdl_surf *)UY_ds;
	else if (key == UY_cs[UY_ics]->key) geo = (NCLX_mdl_surf *)UY_cs[UY_ics];
	else goto done;
/*
.....Get ruled flag
*/
	if (geo->header.relnum == NCLX_MDL_TRIMSF)
		geo = ((NCLX_mdl_trimsf *)geo)->surf;

	*ruled = (*geo).sfhead.urld;
done:;
	return (0);
}

/*********************************************************************
**		E_FUNCTION     : int NclxMdlEvalSetup (key,isrf)
**			Initializes surface for evaluating.
**		PARAMETERS
**		INPUT  :
**			key          Key id of geo to initialize.
**			isrf         Which surface to initialize, DS,PS,CS.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlEvalSetup (key,isrf)
NCLX_KEY_ID key;
UM_int2 isrf;
{
	int i;
	NCLX_mdl_struct *ptr;
/*
.....It is a NET drive surface
*/
	if (isrf == 2 && UY_nds > 1 && key != UY_ds->key)
	{
		for (i=0;i<UY_nds;i++)
		{
			ptr = (NCLX_mdl_struct *)UY_netds[i];
			if (key == ptr->key)
			{
				UY_ds = ptr;
				break;
			}
		}
	}
/*
...   If the cs key is not current, swap with hldgeo. Used for driving
...   guide curve.
*/
   if (isrf == 3)
   {
     if (key != UY_cs[UY_ics]->key)
     {
       for (i=0;i<UY_ncs;i++)
       {
         if (key == UY_cs[i]->key)
			{
			  UY_ics = i;
			  break;
			}
		 }
     }
   }

	return (NCLX_SUCCESS);
}

/*********************************************************************
**		E_FUNCTION     : int NclxMdlEvalLine (lnptr, u, eval)
**			Line evaluation routine.
**		PARAMETERS
**		INPUT  :
**			lnptr        Line structure.
**			u            U parameter to evaluate.
**		OUTPUT :
**			eval         Evaluation parameters.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlEvalLine (lnptr, u, eval)
double u;
NCLX_mdl_line *lnptr;
NCLX_mdl_curve_eval *eval;
{
	int i,status;

	status = NCLX_SUCCESS;
	um_vcmnvc (lnptr->ept, lnptr->spt, eval->udrv1);
	for (i=0;i<3;i++) eval->pt[i] = lnptr->spt[i] + u * eval->udrv1[i];
	for (i=0;i<3;i++) eval->udrv2[i] = 0.0;
	eval->ucrv = 0.0;
	return(status);
}

/*********************************************************************
**		E_FUNCTION     : int NclxMdlEvalCircle (ciptr, u, eval)
**			Circle evaluation routine.
**		PARAMETERS
**		INPUT  :
**			ciptr        Circle structure.
**			u            U parameter to evaluate.
**		OUTPUT :
**			eval         Evaluation parameters.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlEvalCircle (ciptr, u, eval)
double u;
NCLX_mdl_circle *ciptr;
NCLX_mdl_curve_eval *eval;
{
	UU_REAL rad, dang, ang, sine, cosine;
	UU_REAL *center, *svec, *nvec;
	UM_vector radvec, crosvec;

	rad = ciptr->radius;
	dang = ciptr->dang;
	center = ciptr->center;
	svec = ciptr->svec;
	nvec = ciptr->nvec;
	um_vctmsc(svec,rad,radvec);
	ang = dang * u;
	um_cross(nvec, radvec, crosvec);
	cosine = cos(ang);
	sine = sin(ang);
	eval->pt[0] = sine * crosvec[0] + cosine * radvec[0] + center[0];
	eval->pt[1] = sine * crosvec[1] + cosine * radvec[1] + center[1];
	eval->pt[2] = sine * crosvec[2] + cosine * radvec[2] + center[2];
	um_vcmnvc(eval->pt,center,radvec);

	eval->udrv1[0] =  dang * (nvec[1] * radvec[2] - nvec[2] * radvec[1]);
	eval->udrv1[1] =  dang * (nvec[2] * radvec[0] - nvec[0] * radvec[2]);
	eval->udrv1[2] =  dang * (nvec[0] * radvec[1] - nvec[1] * radvec[0]);

	dang = (-1.0) * dang * dang;
	um_vctmsc (radvec, dang, eval->udrv2);
	eval->ucrv = 1.0 / rad;
 
	return(NCLX_SUCCESS);
}

/*********************************************************************
**		E_FUNCTION     : int smtset (cvkey)
**			Set a curve key for the smooth corners evaluation.
**		PARAMETERS
**		INPUT  :
**			cvkey        curve key
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int smtset (cvkey)
UM_int4 *cvkey;
{
	NCLX_smoot_key = *cvkey;
	return (NCLX_SUCCESS);
}

/*********************************************************************
**		E_FUNCTION     : int smtrst ()
**			Unset a curve key for the smooth corners evaluation.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int smtrst ()
{
	NCLX_smoot_key = 0;
	return (NCLX_SUCCESS);
}

/*******************************************************************
**  E_FUNCTION: int nclx_eval_corner (corner,v1,v2,co,pt0,pt1,d,eval)
**    Move the evaluated point+tangent from a corner leg to a fillet arc.
**    PARAMETERS
**       INPUT  :
**          corner            flag: 1 iff the other leg is ahead,
**                                 -1 iff behind
**          v1                unit vector along current corner leg
**          v2                unit vector along the other corner leg
**          co                dot product (v1,v2)
**          pt0               corner point
**          pt1               fillet tangency point on current leg
**          d                 distance [pt0,pt1]
**          eval              curve evaluator record
**       OUTPUT :
**          eval              changed curve evaluator record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void nclx_eval_corner (corner,v1,v2,co,pt0,pt1,d,eval)
int corner;
UM_vector v1,v2;
UM_coord pt0,pt1;
UU_REAL co,d;
NCLX_mdl_curve_eval *eval;
{
	UU_REAL r,d1;
	UM_coord cen;
	UM_vector vn,vp;

	r = d*sqrt((1.+co)/(1.-co)); /* fillet radius */
/*
..... corner plane normal
*/
	if (corner == 1)
		um_cross (v1,v2,vn);
	else
		um_cross (v2,v1,vn);
	um_unitvc (vn,vn);

	um_cross (vn,v1,vp);
	um_translate_point (pt1,r,vp,cen); /* fillet center */
/*
..... new eval->pt is on the same radius as the original, but on the
..... fillet arc instead of the corner leg
*/
	um_vcmnvc (eval->pt,cen,vp);
	um_unitvc(vp,vp); /* vp goes from center to arc point */
	um_translate_point (cen,r,vp,eval->pt);
	um_cross (vp,vn,v1);
	d1 = UM_DOT(v1,eval->udrv1);
	um_vctmsc(v1,d1,eval->udrv1);

	return;
}

/*******************************************************************
**  E_FUNCTION: int nclx_smoot_comp (corner,du0,cvp,func,eval,rev,p2)
**    Fix a composite curve evaluator near a corner .
**    PARAMETERS
**       INPUT  :
**          corner            flag: 1 iff the other leg is ahead,
**                                 -1 iff behind
**          du0               parametric distance form the near endpoint
**          cvp               pointer to current component curve entity
**          func              current component evaluator
**          rev               current component reverse flag
**          p2                the other leg component pointer
**          eval              curve evaluator record
**       OUTPUT :
**          eval              changed curve evaluator record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int nclx_smoot_comp (corner,du0,cvp,func,eval,rev,p2)
int corner;
UU_REAL du0;
NCLX_mdl_curve *cvp;
int (*func)();
NCLX_mdl_curve_eval *eval;
UU_LOGICAL rev;
NCLX_mdl_cmp_entity *p2;
{
	UU_LOGICAL rev2;
	NCLX_mdl_curve *cvp2;
	NCLX_mdl_curve_eval ev;
	UU_REAL u,co,d,du;
	UM_coord pt0,pt1;
	UM_vector v1,v2;
	int NclxMdlEvalLine(), NclxMdlEvalCircle(), NclxMdlEvalComposite();
	int NclxMdlEvalPolyline();
	int (*func2)();
	int status = 0;

	rev2 = p2->reverse;
	cvp2 = p2->curve;
	switch (cvp2->header.relnum)
	{
		case NCLX_MDL_CURVE:
		case NCLX_MDL_BSPLINE:
			func2 = (int(*) ())cvp2->evaluator;
			break;
		case NCLX_MDL_LINE:
			func2 = NclxMdlEvalLine;
			break;
		case NCLX_MDL_CIRCLE:
			func2 = NclxMdlEvalCircle;
			break;
		case NCLX_MDL_POLYLINE:
			func2 = NclxMdlEvalPolyline;
			break;
	}

	u = (corner == 1)? 1: 0;
	if (rev) u = 1 - u;
	status = (*(func)) (cvp,u,&ev);
	if (status != 0) return (status);
	um_vctovc (ev.pt,pt0); /* corner point */
	um_unitvc (ev.udrv1,v1); /* our component tangent vector */
	if (rev) um_negvc(v1,v1);
	u = (corner == 1)? 0: 1;
	if (rev2) u = 1 - u;
	status = (*(func2)) (cvp2,u,&ev);
	if (status != 0) return (status);
	um_unitvc (ev.udrv1,v2); /* adjacent component tangent vector */
	if (rev2) um_negvc(v2,v2);
	co = UM_DOT (v1,v2);
	if (co > 0.9998477) return (status);

	if (co > 0.98480775)
		du = 0.05*DU;
	else if (co > 0.707106)
		du = 0.1*DU;
	else if (co > 0)
		du = 0.5*DU;
	else
		du = DU;
	if (du0 >= du) return (status);

	u = (corner == 1)? 1-du: du;
	if (rev) u = 1 - u;
	status = (*(func)) (cvp,u,&ev);
	if (status != 0) return (status);
	um_vctovc (ev.pt,pt1); /* fillet tangency point on our component */
	d = UM_DCCCC(pt0,pt1);

	nclx_eval_corner (corner,v1,v2,co,pt0,pt1,d,eval);

	return (0);
}

/*******************************************************************
**  E_FUNCTION: int NclxMdlEvalComposite (cvptr, u, eval)
**    Given a composite curve entity (CRVPTR) with associated transformation
**    matrix (TFMAT which may be UM_DEFAULT_TF), calculate the
**    data requested by EVFLAG at the logical parameter value U
**    [0.0 <= U <= 1.0] and return it in CRVOUT.
**    PARAMETERS
**       INPUT  :
**          cvptr             pointer to curve entity
**          u                 parameter value to evaluate curve function
**       OUTPUT :
**          eval              curve evaluator record to put results
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMdlEvalComposite (cvptr, u, eval)
double u;
NCLX_mdl_composite *cvptr;
NCLX_mdl_curve_eval *eval;
{
	int i;                     /* index */
	UU_REAL uu;                /* parameter on constituent curve */
	UU_REAL t0,t1;             /* start and end parameter range t0 <= u < t1 */
	UU_REAL dudw;
	UU_LOGICAL reverse;
	int status;
	NCLX_mdl_cmp_entity *p1;
	NCLX_mdl_curve *cvp;
	int (*func)();
	int NclxMdlEvalLine();
	int NclxMdlEvalCircle();
	int NclxMdlEvalPolyline();

	p1 = cvptr->cvid;
	for (i = 0; (i < cvptr->ncurve-1) && (u >= p1->endparam); i++, p1++);
	reverse = (p1->reverse);
	if (i==0) t0 = 0.0; else t0 = (p1-1)->endparam;
	t1 = p1->endparam;
	dudw = 1.0/(t1-t0);
	uu = (u - t0) * dudw;
	if (reverse) uu = 1 - uu;
	cvp = p1->curve;

	switch (cvp->header.relnum)
	{
	case NCLX_MDL_CURVE:
	case NCLX_MDL_BSPLINE:
		func = (int(*) ())cvp->evaluator;
		break;
	case NCLX_MDL_LINE:
		func = NclxMdlEvalLine;
		break;
	case NCLX_MDL_CIRCLE:
		func = NclxMdlEvalCircle;
		break;
	case NCLX_MDL_POLYLINE:
		func = NclxMdlEvalPolyline;
		break;
	}

	if (uu < 0.) uu = 0.;
	if (uu > 1.0) uu = 1.0;
	status = (*(func)) (cvp, uu, eval);
#ifdef TEMP_FIX
	status = 0; /* TEMP-FIX */
#endif
	if (status == 0 && cvptr->header.key == NCLX_smoot_key)
	{
		int corner = 0;

		if (i < cvptr->ncurve-1 && 
			((reverse && uu < DU) || (!reverse && uu > 1.-DU)))
			corner = 1;
		else if (i > 0 && 
			((!reverse && uu < DU) || (reverse && uu > 1.-DU)))
			corner = -1;
		if (corner != 0)
		{
			UU_REAL du;

			p1 = p1+corner;
			if ((corner == 1 && reverse) || (corner == -1 && !reverse))
				du = uu;
			else
				du = 1 - uu;
			status = nclx_smoot_comp (corner,du,cvp,func,eval,reverse,p1);
		}
	}
	if (status == 0)
	{
		if (reverse) dudw = -1.0 * dudw;
		um_vctmsc(eval->udrv1, dudw, eval->udrv1);
		um_vctmsc(eval->udrv2, dudw * dudw, eval->udrv2);
	}

	NclxDbgEvalRtn("Composite",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : void NclxMdlInitPolylineEval()
**      Initialize polyline data
**    PARAMETERS
**    INPUT  : none
**    OUTPUT : none
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void NclxMdlInitPolylineEval()
{
	NCLX_polyln_key = (NCLX_KEY_ID)0;
	NCLX_polyln_length = -1.;
	NCLX_polyln_npts = 0;

	return;
}

/*******************************************************************
**  E_FUNCTION: void nclx_smoot_polyln (corner,du0,p0,p1,magvec,p2,eval)
**    Fix a polyline curve evaluator near a corner .
**    PARAMETERS
**       INPUT  :
**          corner            flag: 1 iff the other leg is ahead,
**                                 -1 iff behind
**          du0               parametric distance form the near endpoint
**          (p0,p1)           current corner leg
**          magvec            distance [p0,p1]
**          p2                the third polyline point defining corner
**          eval              curve evaluator record
**       OUTPUT :
**          eval              changed curve evaluator record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void nclx_smoot_polyln (corner,du0,p0,p1,magvec,p2,eval)
int corner;
UM_coord p0,p1,p2;
UU_REAL magvec,du0;
NCLX_mdl_curve_eval *eval;
{
	UU_REAL co,d,du;
	UM_coord pt0,pt1;
	UM_vector v1,v2;


	um_vcmnvc (p1,p0,v1);
	um_vctmsc (v1,1./magvec,v1);
	if (corner == 1)
	{
		um_vcmnvc (p2,p1,v2);
		um_vctovc (p1,pt0);
	}
	else
	{
		um_vcmnvc (p0,p2,v2);
		um_vctovc (p0,pt0);
	}
	um_unitvc (v2,v2);
	co = UM_DOT (v1,v2);
	if (co > 0.9998477) return;

	if (co > 0.98480775)
		du = 0.05*DU;
	else if (co > 0.707106)
		du = 0.1*DU;
	else if (co > 0)
		du = 0.5*DU;
	else
		du = DU;
	if (du0 >= du) return;

	d = du*magvec;
	um_translate_point (pt0,-d*corner,v1,pt1); /* fillet tangency point */

	nclx_eval_corner (corner,v1,v2,co,pt0,pt1,d,eval);

	return;
}

/*********************************************************************
**    E_FUNCTION     : int NclxMdlEvalPolyline(polylnptr, u, eval)
**      Evaluate the given polyline at u parameter and put into eval the 
**      results.
**    PARAMETERS
**    INPUT  :
**       polylnptr    Pointer to polyline structure.
**       u            U-parameter to evaluate.
**    OUTPUT :
**       eval         Current UV settings.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMdlEvalPolyline(polylnptr, u, eval)
double u;
NCLX_mdl_polyline *polylnptr;
NCLX_mdl_curve_eval *eval;
{
	int i,j;
	UU_REAL magvec, lvec, ulen, partlen, len;
	UU_REAL *pt;
	int numpts;
	UM_vector vec;

	if ((*polylnptr).header.relnum != NCLX_MDL_POLYLINE || 
		NCLX_internal_geom != 1) return (UU_FAILURE);

	pt = polylnptr->pts;
	numpts = polylnptr->npts;
	if (numpts < 0) return (UU_FAILURE);

	if ((*polylnptr).header.key == NCLX_polyln_key && NCLX_polyln_length > 0)
	{
		len = NCLX_polyln_length;
		numpts = NCLX_polyln_npts;
	}
	else
	{
/*
.....Calculate total length of polyline
*/
		NCLX_polyln_npts = numpts;
		len = 0.0;
		for (i=1, j=0; i<numpts; i++, j+=3)
		{
			um_vcmnvc(&pt[j+3], &pt[j], vec);
			magvec = um_mag(vec);
			if (i == numpts - 1 && magvec <= 1.e-6)
				numpts--;
			else
			len += magvec;
		}
		if (len > 0.)
		{
			NCLX_polyln_key = (*polylnptr).header.key;
			NCLX_polyln_length = len;
			NCLX_polyln_npts = numpts;
		}
	}
	for (i=0;i<3;i++)
	{
		eval->udrv1[i] = 0.0; eval->udrv2[i] = 0.0;
	}
	eval->ucrv = 0.0;
/*
.....Find segment that parameter value lies in
*/
	ulen = u * len;
	partlen = 0.0;
	for (i=0, j=0; i<numpts; i++, j+=3)
	{
		um_vcmnvc(&pt[j+3], &pt[j], vec);
		magvec = um_mag(vec);
		partlen += magvec;
		if (partlen >= ulen && magvec > 1.e-6)
		{
			lvec = 1.0 - (partlen - ulen)/magvec;
			um_vctovc(vec, eval->udrv1);
			um_vctmsc(vec, lvec, vec);
			um_vcplvc(&pt[j], vec, eval->pt);
			if (NCLX_polyln_key == NCLX_smoot_key)
			{
				int corner = 0;

				if (i > 0 && lvec < DU)
				{
					corner = -1;
					nclx_smoot_polyln (corner,lvec,&pt[j],&pt[j+3],magvec,&pt[j-3],
											eval);
				}
				else if (i < numpts && lvec > 1.-DU)
				{
					corner = 1;
					lvec = 1. - lvec;
					nclx_smoot_polyln (corner,lvec,&pt[j],&pt[j+3],magvec,&pt[j+6],
											eval);
				}
			}
			return (UU_SUCCESS);
		}
	}

	return (UU_FAILURE);
}

/*********************************************************************
**		FUNCTION     : int NclxMdlFindGeo (key,geo)
**			Retrieves the pointer to geometry structure from the DS,PS, or CS.
**		PARAMETERS
**		INPUT  :
**			key  -    Key id of geo to retrieve.
**		OUTPUT :
**			geo  -    pointer to geometry structure.
**		RETURNS      :
**			NCLX_FAILURE on error.
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlFindGeo (key,geo)
NCLX_KEY_ID key;
NCLX_mdl_struct **geo;
{
	int i;
	NCLX_mdl_struct *g,*ptr;
/*
.....Search for geometry in PS,DS,CS
*/
	if (UY_ps && key == UY_ps->key) g = UY_ps;
	else if (UY_ds && key == UY_ds->key) g = UY_ds;
	else
	{
		g = 0;
		for (i=0;i<UY_ncs;i++)
			if (key == UY_cs[i]->key)
			{
				g = (NCLX_mdl_struct *) UY_cs[i];
				break;
			}
			else if (UY_nearpt[i] && key == UY_nearpt[i]->header.key)
			{
				g = (NCLX_mdl_struct *) UY_nearpt[i];
				break;
			}
/*
.....Search in DS Net surface
*/
		if (!g)
		{
			if (UY_ds->relnum == NCLX_MDL_NETSF)
			{
				for (i=0;i<UY_nds;i++)
				{
					ptr = (NCLX_mdl_struct *)UY_netds[i];
					if (key == ptr->key)
					{
						g = ptr;
						break;
					}
				}
			}
		}
/*
.....Search in Pocket Islands
*/
		if (!g)
		{
			for (i=0;i<UY_nislands;i++)
				if (key == UY_island[i]->key)
				{
					g = (NCLX_mdl_struct *) UY_island[i];
					break;
				}
		}
/*
.....Last chance, as part of a composite curve
*/
		if (!g)
		{
			if (UY_ps && UY_ps->relnum == NCLX_MDL_COMPOSITE)
				NclxMdlFindCompositeGeo(key,UY_ps,&g);
			if (!g)
			{
				if (UY_ds && UY_ds->relnum == NCLX_MDL_COMPOSITE)
					NclxMdlFindCompositeGeo(key,UY_ds,&g);
				if (!g)
				{
					for (i=0;i<UY_ncs;i++)
					{
						if (UY_cs[i]->relnum == NCLX_MDL_COMPOSITE)
							NclxMdlFindCompositeGeo(key,UY_cs[i],&g);
						if (g) break;
					}
					if (!g)
					{
						for (i=0;i<UY_nislands;i++)
						{
							if (UY_island[i]->relnum == NCLX_MDL_COMPOSITE)
								NclxMdlFindCompositeGeo(key,UY_island[i],&g);
							if (g) break;
						}
					}
				}
			}
		}
		if (!g)
		{
			if (UY_secps && key == UY_secps->key) g = UY_secps;
		}
/*
.....Error  -  Key not found
*/
		if (!g) return (NCLX_FAILURE);
	}

/*
.....Return matching geometry
*/
	*geo = g;
	return (NCLX_SUCCESS);
}

/*********************************************************************
**		FUNCTION     : int NclxMdlGenKey (key)
**			Finds an unused key
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			key  -    Key id of geo to retrieve.
**		RETURNS      :
**			NCLX_SUCCESS / NCLX_FAILURE
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlGenKey (key)
NCLX_KEY_ID *key;
{
	NCLX_KEY_ID i;
	int stat;
	NCLX_mdl_struct *geo;

	geo = 0;
	stat = NCLX_SUCCESS;
	for (i = 1001; i < 10000; i++)
	{
		stat = NclxMdlFindGeo (i,&geo);
		if (stat != NCLX_SUCCESS) break;
	}

	if (stat != NCLX_SUCCESS) *key = i;
	else *key = 0;

	return (-1-stat);
}

/*********************************************************************
**		FUNCTION     : int NclxMdlFindCompositeGeo (key,cv,geo)
**			Retrieves the pointer to geometry structure from a composite
**			curve using a key value.
**		PARAMETERS
**		INPUT  :
**			key  -    Key id of geo to retrieve.
**			cv   -    Composite curve structure to search.
**		OUTPUT :
**			geo  -    pointer to geometry structure.
**		RETURNS      :
**			NCLX_FAILURE on error.
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMdlFindCompositeGeo (key,cv,geo)
NCLX_KEY_ID key;
NCLX_mdl_struct **geo;
NCLX_mdl_composite *cv;
{
	int i;
	NCLX_mdl_struct *g,*dat;
	NCLX_mdl_cmp_entity *ent;
/*
.....Search for geometry in Composite Curve
*/
	g = 0;
	for (i=0;i<cv->ncurve;i++)
	{
		ent = (NCLX_mdl_cmp_entity *)&cv->cvid[i];
		dat = (NCLX_mdl_struct *)ent->curve;
		if (dat->key == key)
		{
			g = dat;
			break;
		}
	}
/*
.....Key not found
*/
	if (!g) return(NCLX_FAILURE);
/*
.....Return matching geometry
*/
	*geo = g;
	return(NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMdlFree(ptr)
**      Call uu_free to free memory.
**    PARAMETERS
**    INPUT  :
**       key  -    Memory to free.
**    OUTPUT :
**      None.
**    RETURNS      :
**      None.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMdlFree(ptr)
char *ptr;
{
   uu_free(ptr);
   return(NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_dscs_swap
**      Swap UY_ds and UY_cs.
**    PARAMETERS
**    INPUT  :
**	None
**    OUTPUT :
**      None.
**    RETURNS      :
**      None.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_dscs_swap()
{
	if (NCLX_internal_geom)
	{
		NCLX_mdl_struct *UYtmp;
		UYtmp = UY_ds;
		UY_ds = UY_cs[0];
		UY_cs[0] = UYtmp;
	}
   	return(NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_dscs_restore
**      Restore UY_ds and UY_cs.
**    PARAMETERS
**    INPUT  :
**	None
**    OUTPUT :
**      None.
**    RETURNS      :
**      None.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_dscs_restore()
{
	if (NCLX_internal_geom)
	{
		UY_ds = UY_hldds;
		UY_cs[0] = UY_hldcs;
	}
   	return(NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int nclx_rmill_swap(itsk)
**      Swap UY_ds and UY_cs while processing rmill.
**    PARAMETERS
**    INPUT  :
**              itsk -  swap UY_ds with UY_cs[itsk]
**    OUTPUT :
**      None.
**    RETURNS      :
**      None.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclx_rmill_swap(itsk)
UM_int2 *itsk;
{
	if (NCLX_internal_geom)
	{
		NCLX_mdl_struct *UYtmp;
		int ics = *itsk;

		if (ics == 1 || ics == 2)
		{
			UYtmp = UY_ds;
			UY_ds = UY_cs[ics];
			UY_cs[ics] = UYtmp;
		}
	}
	return (NCLX_SUCCESS);
}
