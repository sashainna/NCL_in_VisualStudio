/*********************************************************************
**    NAME         :  ymdlput.c
**       CONTAINS:
**
**				NclxMdlStoreGeom
**				NclxMdlStoreComposite
**				NclxMdlStoreCurve
**				NclxMdlStoreLine
**          NclxMdlStorePolyline
**          NclxMdlStoreCircle
**				NclxMdlStorePlane
**				NclxMdlStorePntvec
**				NclxMdlStorePoint
**				NclxMdlStoreSurf
**				NclxMdlStoreTrimSurf
**          uy_update_tfmat
**          uy_circ_3pts
**          uy_circ_2pts
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ymdlput.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:10:59
*********************************************************************/

#include "usysdef.h"
#include "mattrddl.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mfort.h"
#include "modef.h"
#include "mcrv.h"
#include "msrf.h"
#include "nccs.h"
#include "ncl.h"
#include "nclx.h"
#include "nclxmdl.h"

extern int NCL_ubcopy;
extern int NCLX_external_unibase;

static int uy_update_xform();
int uy_update_tfmat();

int s,ss,m,sm;
/*#define START_TIME gtimx(&ss,&sm);*/
/*#define END_TIME(tvar) gtimx(&s,&m); tvar = tvar + (s-ss)*1000 + (m-sm)*/

/*********************************************************************
**    E_FUNCTION     : NclxMdlStoreGeom(comp_rec,redef,ts)
**       Calls the various NclxMdlStore--- routines to store the proper
**       geometry type in the Unibase.
**    PARAMETERS
**       INPUT  :
**          geom              Geometry to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**				                  geometry, otherwise NCLX_FALSE.
**          ts                Array of 10 integers for tracking time.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else NCLX_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlStoreGeom(geom,redef,ts)
NCLX_mdl_data *geom;
int redef,ts[10];
{
	int status;
/*
.....Store correct geometry type
*/
	switch (geom->data.header.relnum)
	{
	case NCLX_MDL_COMPOSITE:
		status = NclxMdlStoreComposite(&geom->data.cp,redef,ts);
		break;
	case NCLX_MDL_CURVE:
	case NCLX_MDL_BSPLINE:
		status = NclxMdlStoreCurve(&geom->data.cv,redef,ts);
		break;
	case NCLX_MDL_LINE:
		status = NclxMdlStoreLine(&geom->data.ln,redef);
		break;
	case NCLX_MDL_POLYLINE:
		status = NclxMdlStorePolyline(&geom->data.py,redef,ts);
		break;
	case NCLX_MDL_CIRCLE:
		status = NclxMdlStoreCircle(&geom->data.ci,redef,ts);
		break;
	case NCLX_MDL_PLANE:
		status = NclxMdlStorePlane(&geom->data.pl,redef);
		break;
	case NCLX_MDL_PNTVEC:
		status = NclxMdlStorePntvec(&geom->data.pv,redef);
		break;
	case NCLX_MDL_POINT:
		status = NclxMdlStorePntvec(&geom->data.pt,redef);
		break;
	case NCLX_MDL_SURF:
	case NCLX_MDL_NSURF:
		status = NclxMdlStoreSurf(&geom->data.sf,redef);
		break;
	case NCLX_MDL_TRIMSF:
		status = NclxMdlStoreTrimSurf(&geom->data.tf,redef,ts);
		break;
	default:
		status = NCLX_FAILURE;
		break;
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlStoreComposite(comp_rec,redef)
**       Stores a Composite Curve record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          comp_rec          Composite Curve to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**				                  geometry, otherwise NCLX_FALSE.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlStoreComposite(comp_rec,redef,ts)
NCLX_mdl_composite *comp_rec;
int redef,ts[10];
{
	struct UM_compcrv_rec nb;
	struct UM_cid_rec *cid;
	int stat,i,icv;
	char lab[80];
	double val;
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
	NCLX_mdl_curve *curve;
/*
.....Initialize routine
*/
	if ((*comp_rec).header.label[0] == '\0') return (NCLX_FAILURE);
/*
.....Verify that this label is not already used
*/
	if ((*comp_rec).header.subscript != 0)
		sprintf(lab,"%s(%d)",(*comp_rec).header.label,
			(*comp_rec).header.subscript);
	else
		strcpy(lab,(*comp_rec).header.label);
	NclxMdlInquire(lab,&relnum,&key,&val);
	if (relnum != NCLX_MDL_UNDEF && redef == NCLX_FALSE)
		return (NCLX_FAILURE);
/*
.....Label is used, but redef is allowed
.....Delete existing geometry
*/
	if (relnum != NCLX_MDL_UNDEF) uc_delete(key);
/*
.....Store component curves
*/
	for (icv=0;icv<(*comp_rec).ncurve;icv++)
	{
		curve = (NCLX_mdl_curve *)(*comp_rec).cvid[icv].curve;
		curve->header.attrib.visible = UU_TRUE;
		curve->header.attrib.displayable = 1;
		strcpy((*curve).header.label,"@UN");
		(*curve).header.subscript = 0;
		if ((*curve).header.relnum == NCLX_MDL_BSPLINE)
		{
			NclxMdlStoreCurve(curve,NCLX_TRUE,ts);
		}
		else if (curve->header.relnum == NCLX_MDL_CIRCLE)
		{
			NclxMdlStoreCircle(curve,NCLX_TRUE);
		}
		else if (curve->header.relnum == NCLX_MDL_LINE)
		{
			NclxMdlStoreLine(curve,NCLX_TRUE);
		}
		else return(NCLX_FAILURE);
	}
/*
.....Create new geometry
*/
	ur_setup_data(UM_COMPCRV_REL,&nb,sizeof(struct UM_compcrv_rec));
	nb.key = 0;
	nb.rel_num =  UM_COMPCRV_REL;
	strcpy(nb.label,(*comp_rec).header.label);
	nb.subscr = (*comp_rec).header.subscript;
	for (i=0;i<3;i++) nb.labloc[i] = (*comp_rec).header.label_pos[i];
/*
.........Store fixed data
*/
	nb.closdinu = (*comp_rec).cvhead.closed;
	nb.arclen = (*comp_rec).cvhead.length;
	nb.planar = (*comp_rec).cvhead.planar;
	nb.open = UU_FALSE;
	nb.continuity = (*comp_rec).cvhead.continuity;
	nb.fcolor = (*comp_rec).cvhead.fcolor;
	nb.no_cid = 0;
	nb.no_displst = 0;
	nb.t0 = 0.; nb.t1 = 1.;
	nb.addflg = 0;
	NCL_ubcopy = 1;
	um_create_geom(&nb,UM_DEFAULT_TF, UM_CURRENT_ATTR);
	NCL_ubcopy = 0;
/*
........Store variable list data
*/
	cid = (struct UM_cid_rec *)uu_malloc(sizeof(struct UM_cid_rec)*comp_rec->ncurve);
	if (cid == UU_NULL) return(NCLX_FAILURE);
	for (i=0;i<(*comp_rec).ncurve;i++)
	{
		curve = (NCLX_mdl_curve *)(*comp_rec).cvid[i].curve;
		cid[i].crvid = (*curve).header.key;
		cid[i].reverse = (*comp_rec).cvid[i].reverse;
		cid[i].endparam = (*comp_rec).cvid[i].endparam;
	}
	stat = ur_update_data_varlist(nb.key,1,cid,1,comp_rec->ncurve);
	if (stat) return (NCLX_FAILURE);
	uu_free(cid);
/*
........Store attribute bundle &
........Display entity
*/
	(*comp_rec).header.key = nb.key;
	NclxMdlStoreAttr(comp_rec);
/*
.....End of routine
*/
	return (NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlStoreCurve(curve_rec,redef)
**       Stores a B-spline Curve record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          curve_rec         Curve to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**				                  geometry, otherwise NCLX_FALSE.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : NCL curves are not yet supported.
**    WARNINGS     : none
*********************************************************************/
NclxMdlStoreCurve(curve_rec,redef,ts)
NCLX_mdl_curve *curve_rec;
int redef,ts[10];
{
	struct UM_rbsplcrv_rec nb;
	int stat,i;
	char lab[80];
	double val;
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
/*
.....Initialize routine
*/
	if ((*curve_rec).header.label[0] == '\0') return (NCLX_FAILURE);
/*
.....Verify that this label is not already used
*/
ts[2] = ts[2] + 1;
/*START_TIME;*/
	if ((*curve_rec).header.subscript != 0)
		sprintf(lab,"%s(%d)",(*curve_rec).header.label,
			(*curve_rec).header.subscript);
	else
		strcpy(lab,(*curve_rec).header.label);
	NclxMdlInquire(lab,&relnum,&key,&val);
	if (relnum != NCLX_MDL_UNDEF && redef == NCLX_FALSE)
		return (NCLX_FAILURE);
/*
.....Label is used, but redef is allowed
.....Delete existing geometry
*/
	if (relnum != NCLX_MDL_UNDEF) uc_delete(key);
/*END_TIME(ts[0]);*/
/*
.....B-spline curve
*/
	if ((*curve_rec).header.relnum == NCLX_MDL_BSPLINE)
	{
/*
........Create new geometry
*/
/*START_TIME;*/
		ur_setup_data(UM_RBSPLCRV_REL,&nb,sizeof(struct UM_rbsplcrv_rec));
		nb.key = 0;
		nb.rel_num = UM_RBSPLCRV_REL;
		strcpy(nb.label,(*curve_rec).header.label);
		nb.subscr = (*curve_rec).header.subscript;
		for (i=0;i<3;i++) nb.labloc[i] = (*curve_rec).header.label_pos[i];
/*END_TIME(ts[4]);*/
/*
...........Store fixed data
*/
/*START_TIME;*/
		nb.planar = UU_FALSE;
		nb.open = UU_TRUE;
		nb.closdinu = (*curve_rec).cvhead.closed;
		nb.k = (*curve_rec).cvhead.degree;
		nb.n = (*curve_rec).cvhead.degseg;
		nb.t0 = (*curve_rec).cvhead.t0;
		nb.t1 = (*curve_rec).cvhead.t1;
		nb.no_t = 0;
		nb.no_pt = 0;
		nb.no_wt = 0;
		nb.no_displst = 0;
		NCL_ubcopy = 2;
		um_create_geom(&nb,UM_DEFAULT_TF, UM_CURRENT_ATTR);
		NCL_ubcopy = 0;
/*END_TIME(ts[5]);*/
/*
...........Store variable list data
*/
/*START_TIME;*/
		stat = ur_update_data_varlist(nb.key,1,curve_rec->tparms,1,
			curve_rec->ntparm);
		if (stat) return (NCLX_FAILURE);
		stat = ur_update_data_varlist(nb.key,2,curve_rec->pt,1,curve_rec->npt);
		if (stat) return (NCLX_FAILURE);
		if (curve_rec->nwgt != 0)
		{
			stat = ur_update_data_varlist(nb.key,3,curve_rec->wgt,1,
				curve_rec->nwgt);
			if (stat) return (NCLX_FAILURE);
		}
/*END_TIME(ts[6]);*/
/*
........Store attribute bundle &
........Display entity
*/
/*START_TIME;*/
		(*curve_rec).header.key = nb.key;
		NclxMdlStoreAttr(curve_rec);
/*END_TIME(ts[7]);*/
	}
	else return (NCLX_FAILURE);
/*
.....End of routine
*/
	return (NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlStoreLine(line_rec,redef)
**       Stores a Line record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          line_rec          Line to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**				                  geometry, otherwise NCLX_FALSE.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlStoreLine(line_rec,redef)
NCLX_mdl_line *line_rec;
int redef;
{
	struct UM_line_rec nl;
	int stat,i;
	double val;
	UM_transf tfmat;
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
/*
.....Initialize routine
*/
	stat = NCLX_SUCCESS;
	if ((*line_rec).header.label[0] == '\0')
	{
		stat = NCLX_FAILURE;
		goto done;
	}
/*
.....Check to see if this line already exists
*/
	if ((*line_rec).header.key != 0)
	{
		stat = ur_retrieve_data_relnum((*line_rec).header.key,&relnum);
/*
.....If it does not exist, or it is not a line, or
.....The label changed, then store as a new entity
*/
		if (stat != 0 || (relnum != UM_LINE_REL && relnum != NCL_LINE_REL))
			(*line_rec).header.key = 0;
		else
		{
			nl.key = (*line_rec).header.key;
			um_get_all_geom(&nl,sizeof(nl));
			if (strcmp(nl.label,(*line_rec).header.label) != 0 ||
				nl.subscr != (*line_rec).header.subscript)
					(*line_rec).header.key = 0;
		}
	}
/*
.....Store as an existing entity
*/
	if ((*line_rec).header.key != 0)
	{
		for (i=0;i<3;i++) 
		{
			nl.spt[i] = (*line_rec).spt[i];
			nl.ept[i] = (*line_rec).ept[i];
		}
		stat = um_get_transformation(nl.key,tfmat);
		if (stat != NCLX_SUCCESS) goto done;
		stat = um_update_geom(&nl,tfmat);
	}
		
/*
.....Store as a new entity
*/
	else
	{
/*
........Verify that this label is not already used
*/
		NclxMdlInquire((*line_rec).header.label,&relnum,&key,&val);
		if (relnum != NCLX_MDL_UNDEF && redef == NCLX_FALSE)
		{
			stat = NCLX_FAILURE;
			goto done;
		}
/*
........Label is used, but redef is allowed
........Delete existing geometry
*/
		if (relnum != NCLX_MDL_UNDEF) uc_delete(key);
/*
........Create new geometry
*/
		ur_setup_data(UM_LINE_REL,&nl,sizeof(struct UM_line_rec));
		nl.key = (*line_rec).header.key;
		nl.rel_num = UM_LINE_REL;
		strcpy(nl.label,(*line_rec).header.label);
		nl.subscr = (*line_rec).header.subscript;
		for (i=0;i<3;i++)
		{
			nl.labloc[i] = (*line_rec).header.label_pos[i];
			nl.spt[i] = (*line_rec).spt[i];
			nl.ept[i] = (*line_rec).ept[i];
		}
		NCL_ubcopy = 2;
		um_create_geom(&nl,UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uy_update_xform(nl.key);
		NCL_ubcopy = 0;
	}
/*
.....Store attribute bundle &
.....Display entity
*/
	if ((*line_rec).header.key == 0) (*line_rec).header.key = nl.key;
	NclxMdlStoreAttr(line_rec);
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlStorePolyline(curve_rec,redef)
**       Stores a Polyline Curve record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          curve_rec         Curve to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**				                  geometry, otherwise NCLX_FALSE.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : NCL curves are not yet supported.
**    WARNINGS     : none
*********************************************************************/
NclxMdlStorePolyline(curve_rec,redef,ts)
NCLX_mdl_polyline *curve_rec;
int redef,ts[10];
{
	struct UM_polyline_rec nb;
	int stat,i;
	char lab[80];
	double val;
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
/*
.....Initialize routine
*/
	if ((*curve_rec).header.label[0] == '\0') return (NCLX_FAILURE);
/*
.....Verify that this label is not already used
*/
ts[2] = ts[2] + 1;
/*START_TIME;*/
	if ((*curve_rec).header.subscript != 0)
		sprintf(lab,"%s(%d)",(*curve_rec).header.label,
			(*curve_rec).header.subscript);
	else
		strcpy(lab,(*curve_rec).header.label);
	NclxMdlInquire(lab,&relnum,&key,&val);
	if (relnum != NCLX_MDL_UNDEF && redef == NCLX_FALSE)
		return (NCLX_FAILURE);
/*
.....Label is used, but redef is allowed
.....Delete existing geometry
*/
	if (relnum != NCLX_MDL_UNDEF) uc_delete(key);
/*END_TIME(ts[0]);*/
/*
.....Create new geometry
*/
/*START_TIME;*/
	ur_setup_data(UM_POLYLINE_REL,&nb,sizeof(struct UM_rbsplcrv_rec));
	nb.key = 0;
	nb.rel_num = UM_POLYLINE_REL;
	strcpy(nb.label,(*curve_rec).header.label);
	nb.subscr = (*curve_rec).header.subscript;
	for (i=0;i<3;i++) nb.labloc[i] = (*curve_rec).header.label_pos[i];
/*END_TIME(ts[4]);*/
/*
........Store fixed data
*/
/*START_TIME;*/
	nb.no_displst = 0;
	NCL_ubcopy = 2;
	um_create_geom(&nb,UM_DEFAULT_TF, UM_CURRENT_ATTR);
	NCL_ubcopy = 0;
/*END_TIME(ts[5]);*/
/*
...........Store variable list data
*/
/*START_TIME;*/
	stat = ur_update_data_varlist(nb.key,1,curve_rec->pts,1,
		curve_rec->npts);
	if (stat) return (NCLX_FAILURE);
/*
........Store attribute bundle
........Display entity
*/
/*START_TIME;*/
	(*curve_rec).header.key = nb.key;
	NclxMdlStoreAttr(curve_rec);
/*END_TIME(ts[7]);*/
/*
.....End of routine
*/
	return (NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlStoreCircle(rec,redef)
**       Stores a Circle record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          rec               Circle to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**                            geometry, otherwise NCLX_FALSE.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlStoreCircle(rec,redef)
NCLX_mdl_circle *rec;
int redef;
{
	struct UM_circle_rec nci;
	int stat,i;
	double val;
	UM_transf tfmat;
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
/*
.....Initialize routine
*/
	stat = NCLX_SUCCESS;
	if (rec->header.label[0] == '\0')
	{
		stat = NCLX_FAILURE;
		goto done;
	}
/*
.....Check to see if this entity already exists
*/
	if (rec->header.key != 0)
	{
		stat = ur_retrieve_data_relnum(rec->header.key,&relnum);
/*
.....If it does not exist, or it is not a correct entity, or
.....The label changed, then store as a new entity
*/
		if (stat != 0 || relnum != UM_CIRCLE_REL)
			rec->header.key = 0;
		else
		{
			nci.key = rec->header.key;
			um_get_all_geom(&nci,sizeof(nci));
			if (strcmp(nci.label,rec->header.label) != 0 ||
				nci.subscr != rec->header.subscript)
					rec->header.key = 0;
		}
	}
/*
.....Store as an existing entity
*/
	if (rec->header.key != 0)
	{
		for (i=0;i<3;i++)
		{
			nci.center[i] = rec->center[i];
			nci.nvec[i]   = rec->nvec[i];
			nci.svec[i]   = rec->svec[i];
		}
		nci.radius = rec->radius;
		nci.dang   = rec->dang;
		stat = um_get_transformation(nci.key,tfmat);
		if (stat != NCLX_SUCCESS) goto done;
		stat = um_update_geom(&nci,tfmat);
	}
/*
.....Store as a new entity
*/
	else
	{
/*
........Verify that this label is not already used
*/
		NclxMdlInquire(rec->header.label,&relnum,&key,&val);
		if (relnum != NCLX_MDL_UNDEF && redef == NCLX_FALSE)
		{
			stat = NCLX_FAILURE;
			goto done;
		}
/*
........Label is used, but redef is allowed
........Delete existing geometry
*/
		if (relnum != NCLX_MDL_UNDEF) uc_delete(key);
/*
........Create new geometry
*/
		ur_setup_data(UM_CIRCLE_REL,&nci,sizeof(struct UM_circle_rec));
		nci.key = rec->header.key;
		nci.rel_num = UM_CIRCLE_REL;
		strcpy(nci.label,rec->header.label);
		nci.subscr = rec->header.subscript;
		for (i=0;i<3;i++)
		{
			nci.labloc[i] = rec->header.label_pos[i];
			nci.center[i] = rec->center[i];
			nci.nvec[i]   = rec->nvec[i];
			nci.svec[i]   = rec->svec[i];
		}
		nci.radius = rec->radius;
		nci.dang   = rec->dang;

		NCL_ubcopy = 2;
		um_create_geom(&nci,UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uy_update_xform(nci.key);
		NCL_ubcopy = 0;
	}
/*
.....Store attribute bundle &
.....Display entity
*/
	if (rec->header.key == 0) rec->header.key = nci.key;
	NclxMdlStoreAttr(rec);
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlStorePlane(rec,redef)
**       Stores a Plane record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          rec               Plane to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**				                  geometry, otherwise NCLX_FALSE.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlStorePlane(rec,redef)
NCLX_mdl_plane *rec;
int redef;
{
	struct NCL_nclpl_rec nl;
	int stat,i;
	double val;
	UM_transf tfmat;
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
/*
.....Initialize routine
*/
	stat = NCLX_SUCCESS;
	if ((*rec).header.label[0] == '\0')
	{
		stat = NCLX_FAILURE;
		goto done;
	}
/*
.....Check to see if this entity already exists
*/
	if ((*rec).header.key != 0)
	{
		stat = ur_retrieve_data_relnum((*rec).header.key,&relnum);
/*
.....If it does not exist, or it is not a correct entity, or
.....The label changed, then store as a new entity
*/
		if (stat != 0 || relnum != NCL_PLN_REL)
			(*rec).header.key = 0;
		else
		{
			nl.key = (*rec).header.key;
			um_get_all_geom(&nl,sizeof(nl));
			if (strcmp(nl.label,(*rec).header.label) != 0 ||
				nl.subscr != (*rec).header.subscript)
					(*rec).header.key = 0;
		}
	}
/*
.....Store as an existing entity
*/
	if ((*rec).header.key != 0)
	{
		for (i=0;i<3;i++)
		{
			nl.pt[i]   = rec->pt[i];
			nl.nvec[i] = rec->vec[i];
		}
		stat = um_get_transformation(nl.key,tfmat);
		if (stat != NCLX_SUCCESS) goto done;
		stat = um_update_geom(&nl,tfmat);
	}
/*
.....Store as a new entity
*/
	else
	{
/*
........Verify that this label is not already used
*/
		NclxMdlInquire((*rec).header.label,&relnum,&key,&val);
		if (relnum != NCLX_MDL_UNDEF && redef == NCLX_FALSE)
		{
			stat = NCLX_FAILURE;
			goto done;
		}
/*
........Label is used, but redef is allowed
........Delete existing geometry
*/
		if (relnum != NCLX_MDL_UNDEF) uc_delete(key);
/*
........Create new geometry
*/
		ur_setup_data(NCL_PLN_REL,&nl,sizeof(struct NCL_nclpl_rec));
		nl.key = (*rec).header.key;
		nl.rel_num = NCL_PLN_REL;
		strcpy(nl.label,(*rec).header.label);
		nl.subscr = (*rec).header.subscript;
		for (i=0;i<3;i++)
		{
			nl.labloc[i] = (*rec).header.label_pos[i];
			nl.pt[i]   = rec->pt[i];
			nl.nvec[i] = rec->vec[i];
		}
		NCL_ubcopy = 2;
		um_create_geom(&nl,UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uy_update_xform(nl.key);
		NCL_ubcopy = 0;
	}
/*
.....Store attribute bundle &
.....Display entity
*/
	if ((*rec).header.key == 0) (*rec).header.key = nl.key;
	NclxMdlStoreAttr(rec);
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlStorePntvec(rec,redef)
**       Stores a Point Vectore record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          rec               Point Vectore to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**				                  geometry, otherwise NCLX_FALSE.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlStorePntvec(rec,redef)
NCLX_mdl_pntvec *rec;
int redef;
{
	struct NCL_nclpv_rec nl;
	int stat,i;
	double val;
	UM_transf tfmat;
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
/*
.....Initialize routine
*/
	stat = NCLX_SUCCESS;
	if ((*rec).header.label[0] == '\0')
	{
		stat = NCLX_FAILURE;
		goto done;
	}
/*
.....Check to see if this entity already exists
*/
	if ((*rec).header.key != 0)
	{
		stat = ur_retrieve_data_relnum((*rec).header.key,&relnum);
/*
.....If it does not exist, or it is not a correct entity, or
.....The label changed, then store as a new entity
*/
		if (stat != 0 || relnum != NCL_POINTVEC_REL)
			(*rec).header.key = 0;
		else
		{
			nl.key = (*rec).header.key;
			um_get_all_geom(&nl,sizeof(nl));
			if (strcmp(nl.label,(*rec).header.label) != 0 ||
				nl.subscr != (*rec).header.subscript)
					(*rec).header.key = 0;
		}
	}
/*
.....Store as an existing entity
*/
	if ((*rec).header.key != 0)
	{
		for (i=0;i<3;i++) 
		{
			nl.pt[i] = (*rec).pt[i];
			nl.ve[i] = (*rec).vec[i];
		}
		stat = um_get_transformation(nl.key,tfmat);
		if (stat != NCLX_SUCCESS) goto done;
		stat = um_update_geom(&nl,tfmat);
	}
/*
.....Store as a new entity
*/
	else
	{
/*
........Verify that this label is not already used
*/
		NclxMdlInquire((*rec).header.label,&relnum,&key,&val);
		if (relnum != NCLX_MDL_UNDEF && redef == NCLX_FALSE)
		{
			stat = NCLX_FAILURE;
			goto done;
		}
/*
........Label is used, but redef is allowed
........Delete existing geometry
*/
		if (relnum != NCLX_MDL_UNDEF) uc_delete(key);
/*
........Create new geometry
*/
		ur_setup_data(NCL_POINTVEC_REL,&nl,sizeof(struct NCL_nclpv_rec));
		nl.key = (*rec).header.key;
		nl.rel_num = NCL_POINTVEC_REL;
		strcpy(nl.label,(*rec).header.label);
		nl.subscr = (*rec).header.subscript;
		for (i=0;i<3;i++)
		{
			nl.labloc[i] = (*rec).header.label_pos[i];
			nl.pt[i] = (*rec).pt[i];
			nl.ve[i] = (*rec).vec[i];
		}
		NCL_ubcopy = 2;
		um_create_geom(&nl,UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uy_update_xform(nl.key);
		NCL_ubcopy = 0;
	}
/*
.....Store attribute bundle &
.....Display entity
*/
	if ((*rec).header.key == 0) (*rec).header.key = nl.key;
	NclxMdlStoreAttr(rec);
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlStorePoint(rec,redef)
**       Stores a Point record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          rec               Point to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**				                  geometry, otherwise NCLX_FALSE.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlStorePoint(rec,redef)
NCLX_mdl_point *rec;
int redef;
{
	struct UM_point_rec nl;
	int stat,i;
	double val;
	UM_transf tfmat;
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
/*
.....Initialize routine
*/
	stat = NCLX_SUCCESS;
	if ((*rec).header.label[0] == '\0')
	{
		stat = NCLX_FAILURE;
		goto done;
	}
/*
.....Check to see if this entity already exists
*/
	if ((*rec).header.key != 0)
	{
		stat = ur_retrieve_data_relnum((*rec).header.key,&relnum);
/*
.....If it does not exist, or it is not a correct entity, or
.....The label changed, then store as a new entity
*/
		if (stat != 0 || relnum != UM_POINT_REL)
			(*rec).header.key = 0;
		else
		{
			nl.key = (*rec).header.key;
			um_get_all_geom(&nl,sizeof(nl));
			if (strcmp(nl.label,(*rec).header.label) != 0 ||
				nl.subscr != (*rec).header.subscript)
					(*rec).header.key = 0;
		}
	}
/*
.....Store as an existing entity
*/
	if ((*rec).header.key != 0)
	{
		for (i=0;i<3;i++) nl.pt[i] = (*rec).pt[i];
		stat = um_get_transformation(nl.key,tfmat);
		if (stat != NCLX_SUCCESS) goto done;
		stat = um_update_geom(&nl,tfmat);
	}
/*
.....Store as a new entity
*/
	else
	{
/*
........Verify that this label is not already used
*/
		NclxMdlInquire((*rec).header.label,&relnum,&key,&val);
		if (relnum != NCLX_MDL_UNDEF && redef == NCLX_FALSE)
		{
			stat = NCLX_FAILURE;
			goto done;
		}
/*
........Label is used, but redef is allowed
........Delete existing geometry
*/
		if (relnum != NCLX_MDL_UNDEF) uc_delete(key);
/*
........Create new geometry
*/
		ur_setup_data(UM_POINT_REL,&nl,sizeof(struct UM_point_rec));
		nl.key = (*rec).header.key;
		nl.rel_num = UM_POINT_REL;
		strcpy(nl.label,(*rec).header.label);
		nl.subscr = (*rec).header.subscript;
		for (i=0;i<3;i++)
		{
			nl.labloc[i] = (*rec).header.label_pos[i];
			nl.pt[i] = (*rec).pt[i];
		}
		NCL_ubcopy = 2;
		um_create_geom(&nl,UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uy_update_xform(nl.key);
		NCL_ubcopy = 0;
	}
/*
.....Store attribute bundle &
.....Display entity
*/
	if ((*rec).header.key == 0) (*rec).header.key = nl.key;
	NclxMdlStoreAttr(rec);
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlStoreSurf(surf_rec,redef)
**       Stores a Surface record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          surf_rec          Surface to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**				                  geometry, otherwise NCLX_FALSE.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlStoreSurf(surf_rec,redef)
NCLX_mdl_surf *surf_rec;
int redef;
{
	struct NCL_surface_rec ns;
	struct UM_rbsplsrf_rec nb;
	struct NCL_panel_rec panel;
	struct UM_surfattr_rec attr;
	int stat,i,j,npat,sf_key,sf_relnum;
	char lab[80];
	double val;
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
/*
.....Initialize routine
*/
	if ((*surf_rec).header.label[0] == '\0') return (NCLX_FAILURE);
/*
.....Verify that this label is not already used
*/
	if ((*surf_rec).header.subscript != 0)
		sprintf(lab,"%s(%d)",(*surf_rec).header.label,
			(*surf_rec).header.subscript);
	else
		strcpy(lab,(*surf_rec).header.label);
	NclxMdlInquire(lab,&relnum,&key,&val);
	if (relnum != NCLX_MDL_UNDEF && redef == NCLX_FALSE)
		return (NCLX_FAILURE);
/*
.....Label is used, but redef is allowed
.....Delete existing geometry
*/
	if (relnum != NCLX_MDL_UNDEF) uc_delete(key);
/*
.....NCL Surface
*/
	if ((*surf_rec).header.relnum == NCLX_MDL_SURF)
	{
/*
........Create new geometry
*/
		ur_setup_data(NCL_SURF_REL,&ns,sizeof(struct NCL_surface_rec));
		ns.key = sf_key = 0;
		ns.rel_num = sf_relnum = NCL_SURF_REL;
		strcpy(ns.label,(*surf_rec).header.label);
		ns.subscr = (*surf_rec).header.subscript;
		for (i=0;i<3;i++) ns.labloc[i] = (*surf_rec).header.label_pos[i];
/*
...........Store fixed data
*/
		ns.rldnu = -1;
		ns.swapuv = 0;
		ns.rev_normal = (*surf_rec).sfhead.reverse;
		ns.closdinu = (*surf_rec).sfhead.uclosed;
		ns.closdinv = (*surf_rec).sfhead.vclosed;
		ns.offset = (*surf_rec).sfhead.offset;
		ns.offdist = (*surf_rec).sfhead.offdist;
		ns.surf_type = (*surf_rec).sfhead.type;
		ns.no_panelkey = 0;
		NCL_ubcopy = 2;
		NclxMdlSetSurfAttr(surf_rec,&surf_rec->sfhead,&attr);
		um_create_geom(&ns,UM_DEFAULT_TF, &attr);
		uy_update_xform(ns.key);
		NCL_ubcopy = 0;
/*
...........Store panel data
*/
		for (i=0;i<surf_rec->npanel;i++)
		{
			npat = surf_rec->panel[i].npatch;
			panel.key = 0;
			panel.rel_num = NCL_PANEL_REL;
			panel.no_patch = 0;
			panel.type = surf_rec->panel[i].type;
			panel.no_param = surf_rec->panel[i].nparm;
			for (j=0;j<=surf_rec->panel[i].nparm;j++)
				panel.param[j] = surf_rec->panel[i].param[j];
			if (ur_create_data(&panel) != 0) return (NCLX_FAILURE);
/*
..............Store patch data
*/
			for (j=0;j<surf_rec->panel[i].npatch;j++)
			{
				stat = ur_update_data_varlist(panel.key, 1, &(surf_rec->panel[i].patch[j]),j+1,1);
				if (stat) return (NCLX_FAILURE);
			}
			stat = ur_update_data_varlist(ns.key,1,&panel.key,i+1,1);
			if (stat) return (NCLX_FAILURE);

		}
/*
........Store attribute bundle &
........Display entity
*/
/*		NclxMdlGetSurf(ns.key,&surf);*/
		(*surf_rec).header.key = ns.key;
/*		NclxMdlStoreSurfAttr(surf_rec);*/
	}
/*
.....NURB Surface
*/
	else if ((*surf_rec).header.relnum == NCLX_MDL_NSURF)
	{
/*
........Create new geometry
*/
		ur_setup_data(UM_RBSPLSRF_REL,&nb,sizeof(struct UM_rbsplsrf_rec));
		nb.key = sf_key = 0;
		nb.rel_num = sf_relnum = UM_RBSPLSRF_REL;
		strcpy(nb.label,(*surf_rec).header.label);
		nb.subscr = (*surf_rec).header.subscript;
		for (i=0;i<3;i++) nb.labloc[i] = (*surf_rec).header.label_pos[i];
/*
...........Store fixed data
*/
		nb.rldnu = -1;
		nb.swapuv = 0;
		nb.rev_normal = (*surf_rec).sfhead.reverse;
		nb.closdinu = (*surf_rec).sfhead.uclosed;
		nb.closdinv = (*surf_rec).sfhead.vclosed;
		nb.offdist = (*surf_rec).sfhead.offdist;
		nb.primitive = (*surf_rec).primitive;
		for (i=0;i<16;i++) nb.prim_param[i] = (*surf_rec).prim_param[i];
		nb.ku = (*surf_rec).sfhead.udegree;
		nb.kv = (*surf_rec).sfhead.vdegree;
		nb.nu = (*surf_rec).sfhead.udegseg;
		nb.nv = (*surf_rec).sfhead.vdegseg;
		nb.no_tu = 0;
		nb.no_tv = 0;
		nb.no_pt = 0;
		nb.no_wt = 0;
		nb.no_sskey = 0;
		nb.no_displst = 0;
		nb.no_tesslst = 0;
		nb.no_boxlst = 0;
		nb.no_xyzbylst = 0;
		NCL_ubcopy = 2;
		NclxMdlSetSurfAttr(surf_rec,&surf_rec->sfhead,&attr);
		um_create_geom(&nb,UM_DEFAULT_TF, &attr);
		uy_update_xform(nb.key);
		NCL_ubcopy = 0;
/*
...........Store variable list data
*/
		stat = ur_update_data_varlist(nb.key,1,surf_rec->tu,1,surf_rec->ntu);
		if (stat) return (NCLX_FAILURE);
		stat = ur_update_data_varlist(nb.key,2,surf_rec->tv,1,surf_rec->ntv);
		if (stat) return (NCLX_FAILURE);
		stat = ur_update_data_varlist(nb.key,3,surf_rec->pt,1,surf_rec->npt);
		if (stat) return (NCLX_FAILURE);
		if (surf_rec->nwgt != 0)
		{
			stat = ur_update_data_varlist(nb.key,4,surf_rec->wgt,1,surf_rec->nwgt);
			if (stat) return (NCLX_FAILURE);
		}
		if (surf_rec->no_boxlst != 0)
		{
			stat = ur_update_data_varlist(nb.key,8,surf_rec->boxlst,1,
				surf_rec->no_boxlst);
			if (stat) return (NCLX_FAILURE);
		}
		if (surf_rec->no_bndrylst != 0)
		{
			stat = ur_update_data_varlist(nb.key,9,surf_rec->bndrylst,1,
				surf_rec->no_bndrylst);
			if (stat) return (NCLX_FAILURE);
		}
/*
........Store attribute bundle &
........Display entity
*/
/*		NclxMdlGetSurf(nb.key,&surf);*/
		(*surf_rec).header.key = nb.key;
/*		NclxMdlStoreSurfAttr(surf_rec);*/
	}
	else return (NCLX_FAILURE);
/*
.....End of routine
*/
	return (NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlStoreTrimSurf(trimsf_rec,redef)
**       Stores a Trimmed Surface record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          trimsf_rec        Trimmed Surface to store in UNIBASE.
**          redef             NCLX_TRUE is ok to overwrite existing
**				                  geometry, otherwise NCLX_FALSE.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlStoreTrimSurf(trimsf_rec,redef,ts)
NCLX_mdl_trimsf *trimsf_rec;
int redef;
int ts[10];
{
	struct NCL_trimsf_rec nb;
	int stat,i,icv;
	char lab[80];
	double val;
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
	UU_KEY_ID *cvkeys;
	NCLX_mdl_surf *surf;
	NCLX_mdl_curve *curve;
	struct UM_surfattr_rec attr;
/*
.....Initialize routine
*/
	if ((*trimsf_rec).header.label[0] == '\0') return (NCLX_FAILURE);
/*
.....Verify that this label is not already used
*/
	if ((*trimsf_rec).header.subscript != 0)
		sprintf(lab,"%s(%d)",(*trimsf_rec).header.label,
			(*trimsf_rec).header.subscript);
	else
		strcpy(lab,(*trimsf_rec).header.label);
/*START_TIME;*/
	NclxMdlInquire(lab,&relnum,&key,&val);
/*END_TIME(ts[0]);*/
	if (relnum != NCLX_MDL_UNDEF && redef == NCLX_FALSE)
		return (NCLX_FAILURE);
/*
.....Label is used, but redef is allowed
.....Delete existing geometry
*/
	if (relnum != NCLX_MDL_UNDEF) uc_delete(key);
/*
.....Store base surface
*/
	surf = (NCLX_mdl_surf *)(*trimsf_rec).surf;
	strcpy(surf->header.label,"@UN");
	surf->header.subscript = 0;
	surf->header.attrib.visible = UU_TRUE;
	surf->header.attrib.displayable = 1;
/*START_TIME;*/
	NclxMdlStoreSurf(surf,NCLX_TRUE);
/*END_TIME(ts[1]);*/
/*
.....Store outer boundary UV-curve
*/
/*START_TIME;*/
	curve = (NCLX_mdl_curve *)(*trimsf_rec).uv_cv;
	strcpy(curve->header.label,"@UN");
	curve->header.subscript = 0;
	curve->header.attrib.visible = UU_TRUE;
	curve->header.attrib.displayable = 1;
	if (curve->header.relnum == NCLX_MDL_COMPOSITE)
		NclxMdlStoreComposite(curve,NCLX_TRUE,ts);
	else if (curve->header.relnum == NCLX_MDL_POLYLINE)
		NclxMdlStorePolyline(curve,NCLX_TRUE,ts);
	else
		NclxMdlStoreCurve(curve,NCLX_TRUE,ts);
/*END_TIME(ts[2]);*/
/*
.....Store inner trimming curves
*/
/*START_TIME;*/
	for (icv=1;icv<(*trimsf_rec).ncurve;icv=icv+2)
	{
		curve = (NCLX_mdl_curve *)(*trimsf_rec).inner[icv];
		strcpy(curve->header.label,"@UN");
		curve->header.subscript = 0;
		curve->header.attrib.visible = UU_TRUE;
		curve->header.attrib.displayable = 1;
		if (curve->header.relnum == NCLX_MDL_COMPOSITE)
			NclxMdlStoreComposite(curve,NCLX_TRUE,ts);
		else
			NclxMdlStoreCurve(curve,NCLX_TRUE);
	}
/*END_TIME(ts[3]);*/
/*
.....Create new geometry
*/
/*START_TIME;*/
	ur_setup_data(NCL_TRIMSF_REL,&nb,sizeof(struct NCL_trimsf_rec));
	nb.key = 0;
	nb.rel_num =  NCL_TRIMSF_REL;
	strcpy(nb.label,(*trimsf_rec).header.label);
	nb.subscr = (*trimsf_rec).header.subscript;
	for (i=0;i<3;i++) nb.labloc[i] = (*trimsf_rec).header.label_pos[i];
/*END_TIME(ts[4]);*/
/*
.........Store fixed data
*/
/*START_TIME;*/
	surf = (*trimsf_rec).surf;
	nb.closdinu = (*surf).sfhead.uclosed;
	nb.closdinv = (*surf).sfhead.vclosed;
	nb.offdist = 0.;
	curve = (*trimsf_rec).uv_cv;
	nb.uv_key = (*curve).header.key;
	curve = (*trimsf_rec).xyz_cv;
	if (curve == UU_NULL) nb.cv_key = 0;
	else nb.cv_key = (*curve).header.key;
	nb.bs_key = (*surf).header.key;
	nb.ub_min = nb.u_min = (*trimsf_rec).u_min;
	nb.ub_max = nb.u_max = (*trimsf_rec).u_max;
	nb.vb_min = nb.v_min = (*trimsf_rec).v_min;
	nb.vb_max = nb.v_max = (*trimsf_rec).v_max;
	nb.drive_type = (*trimsf_rec).trim_type;
	nb.no_ibndykey = 0;
	nb.no_displst = 0;
	nb.no_tesslst = 0;
	nb.no_boxlst = 0;
	nb.no_xyzbylst = 0;
	nb.no_uvbylst = 0;
	nb.no_uvboxlst = 0;
	NCL_ubcopy = 1;
	NclxMdlSetSurfAttr(trimsf_rec,&trimsf_rec->surf->sfhead,&attr);
	attr.displayable = 0;
	um_create_geom(&nb,UM_DEFAULT_TF, &attr);
	uy_update_xform(nb.key);
	NCL_ubcopy = 0;
/*END_TIME(ts[5]);*/
/*
........Store variable list data
*/
/*START_TIME;*/
	if (trimsf_rec->ncurve != 0)
	{
		cvkeys = (UU_KEY_ID *)uu_malloc(sizeof(UU_KEY_ID)*trimsf_rec->ncurve);
		if (cvkeys == UU_NULL) return(NCLX_FAILURE);
		for (i=1;i<(*trimsf_rec).ncurve;i=i+2)
		{
			curve = (NCLX_mdl_curve *)trimsf_rec->inner[i];
			cvkeys[i-1] = 0;
			cvkeys[i] = curve->header.key;
		}
		stat = ur_update_data_varlist(nb.key,1,cvkeys,1,trimsf_rec->ncurve);
		uu_free(cvkeys);
		if (stat) return (NCLX_FAILURE);
	}
	if (trimsf_rec->no_boxlst != 0)
	{
		stat = ur_update_data_varlist(nb.key,4,trimsf_rec->boxlst,1,
			trimsf_rec->no_boxlst);
		if (stat) return (NCLX_FAILURE);
	}
	if (trimsf_rec->no_bndrylst != 0)
	{
		stat = ur_update_data_varlist(nb.key,5,trimsf_rec->bndrylst,1,
			trimsf_rec->no_bndrylst);
		if (stat) return (NCLX_FAILURE);
	}
/*END_TIME(ts[6]);*/
/*
........Store attribute bundle &
........Display entity
*/
/*START_TIME;*/
	(*trimsf_rec).header.key = nb.key;
/*	NclxMdlStoreSurfAttr(trimsf_rec);*/
/*END_TIME(ts[7]);*/
/*
.....End of routine
*/
	return (NCLX_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : uy_update_xform(key)
**          Sets the default transformation matrix for the newly
**          created entity.
**    PARAMETERS
**       INPUT  :
**          key       Key of entity to update.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int uy_update_xform(key)
UU_KEY_ID key;
{
	int stat;
	struct UM_transf_rec tran;
/*
.....Update transformation
*/
	stat = UU_SUCCESS;
	return(stat);
/*
	if (NCLX_external_unibase)
	{
		tran.key = key;
		tran.rel_num = UM_TRANSFORM_REL;
		um_tftotf(UM_idmat,tran.tfmat);
		stat = ur_update_transf(&tran);
	}
	return(stat);
*/
}

/*********************************************************************
**    I_FUNCTION     : uy_update_tfmat(key,tfmat)
**          Sets the transformation matrix for an entity.
**    PARAMETERS
**       INPUT  :
**          key       Key of entity to update.
**          tfmatp    Transformation matrix.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : Transformation matrix updated.
**    WARNINGS     : none
*********************************************************************/
int uy_update_tfmat(key, tfmat)
UU_KEY_ID key;
UM_transf tfmat;
{
	int status = NCLX_SUCCESS;
	struct UM_transf_rec tran;
/*
.....Update transformation
*/
	if (NCLX_external_unibase)
	{
		tran.key = key;
		tran.rel_num = UM_TRANSFORM_REL;
		um_tftotf(tfmat,tran.tfmat);
		status = ur_update_transf(&tran);
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : uy_circ_3pts(pt1, pt2, pt3, cptr)
**         Calculates a circle through 3 points.
**    PARAMETERS
**       INPUT  :
**          pt1,pt2,pt3  Points.
**       OUTPUT :
**          cptr         Circle.
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uy_circ_3pts(pt1, pt2, pt3, cptr)
double *pt1,*pt2,*pt3;
NCLX_mdl_circle *cptr;
{
	int status = NCLX_SUCCESS;
	UM_vector a,b,c,ua,ub,uc,v1,v2,temp;
	UM_coord  pta,ptb,center;
	UU_REAL ma,mb,mv1,mv2,afactor,bfactor,cfactor,denom;

	um_vcmnvc(pt2,pt1,a);
	um_vcmnvc(pt3,pt1,b);
	um_vcmnvc(pt3,pt2,c);
	um_cross(a,b,v2);
	if (um_mag(v2) < 1.0e-6) return(NCLX_FAILURE);
	um_unitvc(a,ua);
	um_unitvc(b,ub);

	um_vcmnvc( a, b, v1 );
	ma = um_mag(a);
	mb = um_mag(b);
	mv1 = um_mag(v1);
	mv2 = um_mag(v2);
	cptr->radius = ma * mb * mv1 / ( 2.0 * mv2 );

	um_vcplvc(pt1,pt2,pta);
	um_vctmsc(pta,(UU_REAL) 0.5,pta);
	um_vcplvc(pt1,pt3,ptb);
	um_vctmsc(ptb,(UU_REAL) 0.5,ptb);

	um_unitvc(v2,uc);
	um_vctovc(uc,cptr->nvec);

	um_cross(ub,uc,temp);
	denom = um_dot(ua,temp);

	afactor = um_dot(ua,pta);
	um_vctmsc(temp,afactor,center);

	bfactor = um_dot(ub,ptb);
	um_cross(uc,ua,temp);
	um_vctmsc(temp,bfactor,temp);
	um_vcplvc(temp,center,center);

	cfactor = um_dot(uc,pt1);
	um_cross(ua,ub,temp);
	um_vctmsc(temp,cfactor,temp);
	um_vcplvc(temp,center,center);

	um_vctmsc(center,(1.0/denom),cptr->center);


	return(status);
}

/*********************************************************************
**    I_FUNCTION     : uy_circ_2pts (center,pt1,pt2,dir,cptr)
**         Calculates a circle using a center point and 2 points on the
**         circle.
**    PARAMETERS
**       INPUT  :
**          center       Circle center point.
**          pt1          Starting point on circle.
**          pt2          Ending point on circle.
**          dir          1 = CCLW, -1 = CLW.
**       OUTPUT :
**          cptr         Circle.
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uy_circ_2pts (center,pt1,pt2,dir,cptr)
double *center,*pt1,*pt2,dir;
NCLX_mdl_circle *cptr;
{
   int status;
   int vdir;
   UM_vector v0,v1;
   UM_angle ang2;

	status = NCLX_SUCCESS;
	um_vctovc(center, cptr->center);
	cptr->radius = um_dcccc(pt1, cptr->center);
	um_vcmnvc(pt1, cptr->center, v0);
	um_unitvc(v0, cptr->svec);
	um_vcmnvc(pt2, cptr->center, v1);
	um_unitvc(v1,v1);
	if (um_vcparall(cptr->svec, v1))
	{
		cptr->nvec[0] = cptr->nvec[1] = 0.; cptr->nvec[2] = dir;
	}
	else
	{
/*
		if (dir > 0.) um_cross(cptr->svec, v1, cptr->nvec);
		else um_cross(v1, cptr->svec, cptr->nvec);
*/
		um_cross(cptr->svec, v1, cptr->nvec);
		um_unitvc(cptr->nvec, cptr->nvec);
/*
		dir = (int)um_dot(UM_cpln.zaxis, cptr->nvec);
		if (dir < 0)
			um_vctmsc(cptr->nvec, (UU_REAL) -1.0, cptr->nvec);
*/
	}
   
	if (um_cceqcc(pt1,pt2)) ang2 = UM_TWOPI;
	else ang2 = um_angle2p(cptr->svec, v1, cptr->nvec);
	cptr->dang = ang2;
	return(status);
}
