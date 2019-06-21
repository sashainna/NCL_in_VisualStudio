/*********************************************************************
**    NAME         :  ymdlget.c
**       CONTAINS:
**
**				NclxMdlGetCircle
**				NclxMdlGetLine
**				NclxMdlGetNetsf
**				NclxMdlGetPatern
**				NclxMdlGetPlane
**				NclxMdlGetPntvec
**				NclxMdlGetPolyline
**				NclxMdlGetShape
**				NclxMdlGetSurf
**				NclxMdlGetTrimsf
**				NclxMdlGetCurve
**				NclxMdlGetComposite
**				NclxMdlGetGeom
**          NclxMdlGetList
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ymdlget.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:10:59
*********************************************************************/

#include "usysdef.h"
#include "modef.h"
#include "mdrel.h"
#include "mfort.h"
#include "mcrv.h"
#include "msrf.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "ycom.h"

extern NclxMdlEvalSurf();
extern NclxMdlEvalCurve();
extern NclxMdlEvalComposite();

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetCircle(key,cir_rec)
**       Gets a Circle record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          line_rec          Circle retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetCircle(key,cir_rec)
NCLX_KEY_ID key;
NCLX_mdl_circle *cir_rec;
{
	struct NCL_nclci_rec nl;
	int stat,relnum,i;
	UU_REAL pos[3];
/*
.....Verify this is actually a circle
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || (relnum != UM_CIRCLE_REL && relnum != NCL_CIRCLE_REL))
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Get an NCL circle
*/
	nl.key = key;
	um_get_all_geom(&nl,sizeof(nl));
	(*cir_rec).header.key = nl.key;
	(*cir_rec).header.relnum = NCLX_MDL_CIRCLE;
	strcpy((*cir_rec).header.label,nl.label);
	(*cir_rec).header.subscript = nl.subscr;
	(*cir_rec).radius = nl.radius;
	(*cir_rec).dang = nl.dang;
	for (i=0;i<3;i++) 
	{
		(*cir_rec).center[i] = nl.center[i];
		(*cir_rec).svec[i] = nl.svec[i];
		(*cir_rec).nvec[i] = nl.nvec[i];
		pos[i] = 0.;
	}
/*
.....Retrieve the attribute bundle
*/
	NclxMdlGetAttr(cir_rec);
/*
.....Retrieve the label location
*/
	ncl_retrieve_labloc(&nl,(*cir_rec).header.attrib.label_on,pos);
	(*cir_rec).header.label_pos[0] = pos[0];
	(*cir_rec).header.label_pos[1] = pos[1];
	(*cir_rec).header.label_pos[2] = pos[2];
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetLine(key,line_rec)
**       Gets a Line record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          line_rec          Line retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetLine(key,line_rec)
NCLX_KEY_ID key;
NCLX_mdl_line *line_rec;
{
	struct NCL_nclln_rec nl;
	int stat,relnum,i;
	UU_REAL pos[3];
/*
.....Verify this is actually a line
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || (relnum != UM_LINE_REL && relnum != NCL_LINE_REL))
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Get an NCL line
*/
	nl.key = key;
	um_get_all_geom(&nl,sizeof(nl));
	(*line_rec).header.key = nl.key;
	(*line_rec).header.relnum = NCLX_MDL_LINE;
	strcpy((*line_rec).header.label,nl.label);
	(*line_rec).header.subscript = nl.subscr;
	for (i=0;i<3;i++) 
	{
		(*line_rec).spt[i] = nl.spt[i];
		(*line_rec).ept[i] = nl.ept[i];
		pos[i] = 0.;
	}
/*
.....Retrieve the attribute bundle
*/
	NclxMdlGetAttr(line_rec);
/*
.....Retrieve the label location
*/
	ncl_retrieve_labloc(&nl,(*line_rec).header.attrib.label_on,pos);
	(*line_rec).header.label_pos[0] = pos[0];
	(*line_rec).header.label_pos[1] = pos[1];
	(*line_rec).header.label_pos[2] = pos[2];
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetNetsf(key,surf_rec)
**       Gets a Net Surface record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          surf_rec          Surface retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetNetsf(key,surf_rec)
NCLX_KEY_ID key;
NCLX_mdl_netsf *surf_rec;
{
	struct NCL_netsf_rec ns;
	int stat,relnum,i,isiz;
	UU_REAL pos[3];
	NCLX_mdl_ptr *dummy;
	UU_KEY_ID keys[200];
/*
.....Verify this is actually a surface
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || relnum != NCL_NETSF_REL)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Get the Surface fixed data
*/
	ns.key = key;
	if (ur_retrieve_data_fixed(&ns) != 0)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
	(*surf_rec).header.key = ns.key;
	(*surf_rec).header.relnum = NCLX_MDL_NETSF;
	strcpy((*surf_rec).header.label,ns.label);
	(*surf_rec).header.subscript = ns.subscr;
/*
.....Retrieve the attribute bundle
*/
	NclxMdlGetAttr(surf_rec);
/*
.....Retrieve the label location
*/
	ncl_retrieve_labloc(&ns,(*surf_rec).header.attrib.label_on,pos);
	(*surf_rec).header.label_pos[0] = pos[0];
	(*surf_rec).header.label_pos[1] = pos[1];
	(*surf_rec).header.label_pos[2] = pos[2];
/*
.....Retrieve all of the underlying surfaces
*/
	(*surf_rec).nsf = ns.no_netkey;
	isiz = sizeof(dummy) * surf_rec->nsf;
	(*surf_rec).sfptr = (NCLX_mdl_ptr *) uu_malloc(isiz);
	isiz = sizeof(NCLX_mdl_data);
	if (ur_retrieve_data_varlist(ns.key,1,keys,1,surf_rec->nsf) != 0)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
	for (i=0;i<surf_rec->nsf;i++)
	{
		(*surf_rec).sfptr[i] = (NCLX_mdl_ptr *)uu_malloc(isiz);
		if (NclxMdlGetGeom(keys[i],surf_rec->sfptr[i]) != NCLX_SUCCESS)
		{
			stat = NCLX_FAILURE;
			return(stat);
		}
	}
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetPatern(key,rec)
**       Gets a Patern record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          rec               Patern retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetPatern(key,rec)
NCLX_KEY_ID key;
NCLX_mdl_patern *rec;
{
	struct NCL_patern_rec nl;
	int stat,relnum,isiz,m;
	UU_REAL pos[3];
/*
.....Verify this is the correct relation
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || (relnum != NCL_PATERN_REL))
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Get the entity
*/
	nl.key = key;
	stat = ur_retrieve_data_fixed(&nl);
	if (stat != 0)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
	(*rec).header.key = nl.key;
	(*rec).header.relnum = NCLX_MDL_PATERN;
	strcpy((*rec).header.label,nl.label);
	(*rec).header.subscript = nl.subscr;
	if (nl.pntype == 1)
	{
		(*rec).pntype = NCLX_MDL_POINT;
		m = 3;
	}
	else
	{
		(*rec).pntype = NCLX_MDL_PNTVEC;
		m = 6;
	}
/*
.....Retrieve the patern data
*/
		rec->npts = nl.no_patpnt / m;
		if (rec->npts > 0)
		{
			isiz = sizeof(double) * nl.no_patpnt;
			rec->pts = (double *)malloc(isiz);
			ur_retrieve_data_varlist(nl.key,1,rec->pts,1,nl.no_patpnt);
		}
/*
.....Retrieve the attribute bundle
*/
	NclxMdlGetAttr(rec);
/*
.....Retrieve the label location
*/
	ncl_retrieve_labloc(&nl,(*rec).header.attrib.label_on,pos);
	(*rec).header.label_pos[0] = pos[0];
	(*rec).header.label_pos[1] = pos[1];
	(*rec).header.label_pos[2] = pos[2];
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetPlane(key,rec)
**       Gets a Plane record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          rec               Plane retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetPlane(key,rec)
NCLX_KEY_ID key;
NCLX_mdl_plane *rec;
{
	struct NCL_nclpl_rec nl;
	int stat,relnum,i;
	UU_REAL pos[3];
/*
.....Verify this is the correct relation
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || (relnum != NCL_PLN_REL))
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Get the entity
*/
	nl.key = key;
	stat = ur_retrieve_data_fixed(&nl);
	if (stat != 0)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
	(*rec).header.key = nl.key;
	(*rec).header.relnum = NCLX_MDL_PLANE;
	strcpy((*rec).header.label,nl.label);
	(*rec).header.subscript = nl.subscr;
/* 	ncl_plane_to_nclpln(&nl,rec->vec); */
	for (i=0;i<3;i++)
	{
		rec->pt[i]  = nl.pt[i];
		rec->vec[i] = nl.nvec[i];
		pos[i] = 0.;
	}
/*
.....Retrieve the attribute bundle
*/
	NclxMdlGetAttr(rec);
/*
.....Retrieve the label location
*/
	ncl_retrieve_labloc(&nl,(*rec).header.attrib.label_on,pos);
	(*rec).header.label_pos[0] = pos[0];
	(*rec).header.label_pos[1] = pos[1];
	(*rec).header.label_pos[2] = pos[2];
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetPntvec(key,rec)
**       Gets a Point Vector record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          rec               Point Vector retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetPntvec(key,rec)
NCLX_KEY_ID key;
NCLX_mdl_pntvec *rec;
{
	struct NCL_nclpv_rec nl;
	int stat,relnum,i;
	UU_REAL pos[3];
/*
.....Verify this is the correct relation
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || (relnum != NCL_POINTVEC_REL))
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Get the entity
*/
	nl.key = key;
	um_get_all_geom(&nl,sizeof(nl));
	(*rec).header.key = nl.key;
	(*rec).header.relnum = NCLX_MDL_PNTVEC;
	strcpy((*rec).header.label,nl.label);
	(*rec).header.subscript = nl.subscr;
	for (i=0;i<3;i++) 
	{
		(*rec).pt[i] = nl.pt[i];
		(*rec).vec[i] = nl.ve[i];
		pos[i] = 0.;
	}
/*
.....Retrieve the attribute bundle
*/
	NclxMdlGetAttr(rec);
/*
.....Retrieve the label location
*/
	ncl_retrieve_labloc(&nl,(*rec).header.attrib.label_on,pos);
	(*rec).header.label_pos[0] = pos[0];
	(*rec).header.label_pos[1] = pos[1];
	(*rec).header.label_pos[2] = pos[2];
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetPolyline(key,rec)
**       Gets a Polyline record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          rec               Plane retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetPolyline(key,rec)
NCLX_KEY_ID key;
NCLX_mdl_polyline *rec;
{
	struct UM_polyline_rec nl;
	int stat,relnum,isiz;
	UU_REAL pos[3];
/*
.....Verify this is the correct relation
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || (relnum != UM_POLYLINE_REL))
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Get the entity
*/
	nl.key = key;
	stat = ur_retrieve_data_fixed(&nl);
	if (stat != 0)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
	(*rec).header.key = nl.key;
	(*rec).header.relnum = NCLX_MDL_POLYLINE;
	strcpy((*rec).header.label,nl.label);
	(*rec).header.subscript = nl.subscr;
/*
.....Retrieve the polyline data
*/
		rec->npts = nl.no_pt;
		if (rec->npts > 0)
		{
			isiz = sizeof(double) * rec->npts * 3;
			rec->pts = (double *)malloc(isiz);
			ur_retrieve_data_varlist(nl.key,1,rec->pts,1,nl.no_pt*3);
		}
/*
.....Retrieve the attribute bundle
*/
	NclxMdlGetAttr(rec);
/*
.....Retrieve the label location
*/
	ncl_retrieve_labloc(&nl,(*rec).header.attrib.label_on,pos);
	(*rec).header.label_pos[0] = pos[0];
	(*rec).header.label_pos[1] = pos[1];
	(*rec).header.label_pos[2] = pos[2];
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetShape(key,rec)
**       Gets a Shape record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          rec               Patern retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetShape(key,rec)
NCLX_KEY_ID key;
NCLX_mdl_shape *rec;
{
	struct NCL_shape_rec nl;
	int stat,relnum,i,j,isiz,n,nw;
	UU_REAL pos[3],*dat;
	UM_int2 ityp,iwrd;
	UM_real8 rnum;
	union
	{
		UM_real8 asn;
		UM_real4 qsn[2];
		UM_int2 ksn[4];
		UM_int4 jsn[2];
	} val;
#define CLW 60
#define IN 652
#define LEFT 8
/*
.....Verify this is the correct relation
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || (relnum != NCL_SHAPE_REL))
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Get the entity
*/
	nl.key = key;
	stat = ur_retrieve_data_fixed(&nl);
	if (stat != 0)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
	(*rec).header.key = nl.key;
	(*rec).header.relnum = NCLX_MDL_SHAPE;
	strcpy((*rec).header.label,nl.label);
	(*rec).header.subscript = nl.subscr;
/*
.....Retrieve the shape data
*/
	if (nl.no_shapwd > 0)
	{
		isiz = sizeof(double) * nl.no_shapwd;
		dat = (double *)malloc(isiz);
		ur_retrieve_data_varlist(nl.key,1,dat,1,nl.no_shapwd);
	}
/*
.....Determine the number of actual entities
.....within the shape
*/
	rec->nents = 0;
	i = 7;
	do
	{
		rec->nents++;
		val.asn = dat[i];
		i = i + val.ksn[2] + 1;
		if (val.ksn[3] == 2000)
		{
			i++;
			rec->nents = rec->nents + val.ksn[2];
		}
	} while (i<nl.no_shapwd);
/*
.....Allocate the shape entity storage
*/
	if (rec->nents > 0)
	{
		isiz = sizeof(NCLX_mdl_shp_entity) * rec->nents;
		rec->shid = (NCLX_mdl_shp_entity *)malloc(isiz);
	}
/*
.....Convert the shape data
*/
	val.asn = dat[0];
	if (val.ksn[0] == IN) rec->side = NCLX_P_IN;
	else rec->side = NCLX_P_OUT;
	if (val.ksn[1] == LEFT) rec->dir = NCLX_P_LEFT;
	else rec->dir = NCLX_P_RIGHT;
	n = 7;
	for (i=0;i<rec->nents;i++)
	{
		val.asn = dat[n];
/*
........Lathe entity direction
*/
		if (val.ksn[3] == 19)
		{
			rec->shid[i].type = NCLX_S_DIRECTION;
			if (val.ksn[0] == CLW) rec->shid[i].dir = NCLX_P_CLW;
			else rec->shid[i].dir = NCLX_P_CCLW;
			n++;
		}
/*
........Post-processor command
*/
		else if (val.ksn[3] == 2000)
		{
			rec->shid[i].type = NCLX_S_VOCAB;
			rec->shid[i].vocab = val.ksn[0];
			nw = val.ksn[2];
			rec->shid[i].value = nw + 1;
			for (j=0;j<nw;j++)
			{
				n++; i++;
				clpwd(&dat[n],&ityp,&iwrd,&rnum);
				if (ityp == 0)
				{
					rec->shid[i].type = NCLX_S_VOCAB;
					rec->shid[i].vocab = iwrd;
				}
				else
				{
					rec->shid[i].type = NCLX_S_VALUE;
					rec->shid[i].value = rnum;
				}
			}
			n = n + 2;
		}
/*
........Geometry
*/
		else
		{
			rec->shid[i].type = NCLX_S_ENDPT;
			val.asn = dat[n]; n++;
/*
...........End point
*/
			rec->shid[i].pt[0] = dat[n]; n++;
			rec->shid[i].pt[1] = dat[n]; n++;
/*
...........Arc
*/
			
			if (val.ksn[3] == 7)
			{
				if (val.ksn[1] == -1) rec->shid[i].arc.dir = NCLX_P_CLW;
				else rec->shid[i].arc.dir = NCLX_P_CCLW;
				rec->shid[i].type = NCLX_S_ARC;
				rec->shid[i].arc.cen[0] = dat[n]; n++;
				rec->shid[i].arc.cen[1] = dat[n]; n++;
				rec->shid[i].arc.rad = dat[n]; n++;
				val.asn = dat[n]; n++;
				rec->shid[i].arc.sang = val.qsn[0];
				rec->shid[i].arc.eang = val.qsn[1];
			}
		}
	}
/*
.....Retrieve the attribute bundle
*/
	NclxMdlGetAttr(rec);
/*
.....Retrieve the label location
*/
	ncl_retrieve_labloc(&nl,(*rec).header.attrib.label_on,pos);
	(*rec).header.label_pos[0] = pos[0];
	(*rec).header.label_pos[1] = pos[1];
	(*rec).header.label_pos[2] = pos[2];
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetSurf(key,surf_rec)
**       Gets a Surface record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          surf_rec          Surface retrieved from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetSurf(key,surf_rec)
NCLX_KEY_ID key;
NCLX_mdl_surf *surf_rec;
{
	struct NCL_surface_rec ns;
	struct UM_rbsplsrf_rec nb;
	struct UM_surfattr_rec sfattr;
	int stat,relnum,i,j,isiz;
	UU_REAL pos[3];
/*
.....Verify this is actually a surface
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || (relnum != NCL_SURF_REL && relnum != UM_RBSPLSRF_REL))
	{
		stat = NCLX_FAILURE;
		return(stat);
	}

	(*surf_rec).sfhead.eval[0] = 0.5;
	(*surf_rec).sfhead.eval[1] = 0.5;
/*
.....NCL Surface
*/
	if (relnum == NCL_SURF_REL)
	{
/*
........Get the Surface fixed data
*/
		ns.key = key;
		if (ur_retrieve_data_fixed(&ns) != 0)
		{
			stat = NCLX_FAILURE;
			return(stat);
		}
		if (ur_retrieve_attr(key,&sfattr) != 0)
		{
			stat = NCLX_FAILURE;
			return(stat);
		}
		(*surf_rec).header.key = ns.key;
		(*surf_rec).header.relnum = NCLX_MDL_SURF;
		strcpy((*surf_rec).header.label,ns.label);
		(*surf_rec).header.subscript = ns.subscr;
		(*surf_rec).sfhead.material = sfattr.material;
		(*surf_rec).sfhead.upaths = sfattr.numupaths;
		(*surf_rec).sfhead.vpaths = sfattr.numvpaths;
		(*surf_rec).sfhead.upts = sfattr.ptsperucrv;
		(*surf_rec).sfhead.vpts = sfattr.ptspervcrv;
		(*surf_rec).sfhead.urld = 0;
		(*surf_rec).sfhead.vrld = 0;
		(*surf_rec).sfhead.reverse = ns.rev_normal;
		(*surf_rec).sfhead.uclosed = ns.closdinu;
		(*surf_rec).sfhead.vclosed = ns.closdinv;
		(*surf_rec).sfhead.offset = ns.offset;
		(*surf_rec).sfhead.offdist = ns.offdist;
		(*surf_rec).sfhead.type = ns.surf_type;
/*
........Retrieve the attribute bundle
*/
		NclxMdlGetAttr(surf_rec);
/*
........Retrieve the label location
*/
		ncl_retrieve_labloc(&ns,(*surf_rec).header.attrib.label_on,pos);
		(*surf_rec).header.label_pos[0] = pos[0];
		(*surf_rec).header.label_pos[1] = pos[1];
		(*surf_rec).header.label_pos[2] = pos[2];
/*
........Set the evaluator
*/
		(*surf_rec).evaluator = NclxMdlEvalSurf;
/*
........Retrieve the patches and panels
*/
		surf_rec->npanel = ns.no_panelkey;
		isiz = sizeof(NCLX_mdl_panel) * ns.no_panelkey;
		surf_rec->panel = (NCLX_mdl_panel *)uu_malloc(isiz);
		for (i=0;i<ns.no_panelkey;i++)
		{
			if (ur_retrieve_data_varlist(key,1,&(surf_rec->panel[i].key),i+1,1) != 0)
			{
				stat = NCLX_FAILURE;
				return(stat);
			}
			if (ur_retrieve_data_fixed(&(surf_rec->panel[i])) != 0)
			{
				stat = NCLX_FAILURE;
				return(stat);
			}
			else
			{
				isiz = sizeof(NCLX_mdl_patch) * surf_rec->panel[i].npatch;
				surf_rec->panel[i].patch = (NCLX_mdl_patch *)uu_malloc(isiz);
				for (j=0;j<surf_rec->panel[i].npatch;j++)
				{
					if (ur_retrieve_data_varlist(surf_rec->panel[i].key,1,
						&(surf_rec->panel[i].patch[j]),j+1,1) != 0)
					{
						stat = NCLX_FAILURE;
						return(stat);
					}
				}
			}
		}
		surf_rec->primitive = ns.primitive;
		if (ns.primitive > 1)
		{
			for (i = 0; i < 16; i++) surf_rec->prim_param[i] = ns.prim_param[i];
		}
	}
/*
.....NURB Surface
*/
	else if (relnum == UM_RBSPLSRF_REL)
	{
/*
........Get the Surface fixed data
*/
		nb.key = key;
		if (ur_retrieve_data_fixed(&nb) != 0)
		{
			stat = NCLX_FAILURE;
			return(stat);
		}
		if (ur_retrieve_attr(key,&sfattr) != 0)
		{
			stat = NCLX_FAILURE;
			return(stat);
		}
		(*surf_rec).header.key = nb.key;
		(*surf_rec).header.relnum = NCLX_MDL_NSURF;
		strcpy((*surf_rec).header.label,nb.label);
		(*surf_rec).header.subscript = nb.subscr;
		(*surf_rec).sfhead.material = sfattr.material;
		(*surf_rec).sfhead.upaths = sfattr.numupaths;
		(*surf_rec).sfhead.vpaths = sfattr.numvpaths;
		(*surf_rec).sfhead.upts = sfattr.ptsperucrv;
		(*surf_rec).sfhead.vpts = sfattr.ptspervcrv;
		(*surf_rec).sfhead.urld = 0;
		(*surf_rec).sfhead.vrld = 0;
		(*surf_rec).sfhead.reverse = nb.rev_normal;
		(*surf_rec).sfhead.uclosed = nb.closdinu;
		(*surf_rec).sfhead.vclosed = nb.closdinv;
		(*surf_rec).sfhead.offset = 0;
		(*surf_rec).sfhead.offdist = nb.offdist;
		(*surf_rec).sfhead.udegree = nb.ku;
		(*surf_rec).sfhead.vdegree = nb.kv;
		(*surf_rec).sfhead.udegseg = nb.nu;
		(*surf_rec).sfhead.vdegseg = nb.nv;
/*
........Retrieve the attribute bundle
*/
		NclxMdlGetAttr(surf_rec);
/*
........Retrieve the label location
*/
		ncl_retrieve_labloc(&nb,(*surf_rec).header.attrib.label_on,pos);
		(*surf_rec).header.label_pos[0] = pos[0];
		(*surf_rec).header.label_pos[1] = pos[1];
		(*surf_rec).header.label_pos[2] = pos[2];
/*
........Set the evaluator
*/
		(*surf_rec).evaluator = NclxMdlEvalSurf;

		surf_rec->primitive = nb.primitive;
		if (nb.primitive > 1)
		{
			for (i = 0; i < 16; i++) surf_rec->prim_param[i] = nb.prim_param[i];
		}
/*
........Retrieve the U t-parameters
*/
		surf_rec->ntu = nb.no_tu;
		if (surf_rec->ntu > 0)
		{
			isiz = sizeof(double) * surf_rec->ntu;
			surf_rec->tu = (double *)uu_malloc(isiz);
			ur_retrieve_data_varlist(nb.key,1,surf_rec->tu,1,surf_rec->ntu);
		}
/*
........Retrieve the V t-parameters
*/
		surf_rec->ntv = nb.no_tv;
		if (surf_rec->ntv > 0)
		{
			isiz = sizeof(double) * surf_rec->ntv;
			surf_rec->tv = (double *)uu_malloc(isiz);
			ur_retrieve_data_varlist(nb.key,2,surf_rec->tv,1,surf_rec->ntv);
		}
/*
........Retrieve the points
*/
		surf_rec->npt = nb.no_pt;
		if (surf_rec->npt > 0)
		{
			isiz = sizeof(double) * surf_rec->npt * 3;
			surf_rec->pt = (double *)uu_malloc(isiz);
			ur_retrieve_data_varlist(nb.key,3,surf_rec->pt,1,surf_rec->npt);
		}
/*
........Retrieve the weights
*/
		surf_rec->nwgt = nb.no_wt;
		if (surf_rec->nwgt > 0)
		{
			isiz = sizeof(double) * surf_rec->nwgt;
			surf_rec->wgt = (double *)uu_malloc(isiz);
			ur_retrieve_data_varlist(nb.key,4,surf_rec->wgt,1,surf_rec->wgt);
		}
	}

/*
..... Retrieve the bounding box and boundary strunctures
*/
	stat = NclxMdlGetList (NCLX_BOX_LIST,key,surf_rec);	
	if (stat != NCLX_SUCCESS) return (NCLX_FAILURE);
	stat = NclxMdlGetList (NCLX_BOUNDARY_LIST,key,surf_rec);	
/*
.....End of routine
*/
	return (stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetTrimsf(key,surf_rec)
**       Gets a Trimmed Surface record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          surf_rec          Trimmed Surface retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetTrimsf(key,surf_rec)
NCLX_KEY_ID key;
NCLX_mdl_trimsf *surf_rec;
{
	struct NCL_trimsf_rec ns;
	struct UM_surfattr_rec sfattr;
	int stat,relnum,i,isiz;
	UU_REAL pos[3];
	UU_KEY_ID *ibkey;
/*
.....Verify this is actually a trimmed surface
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || relnum != NCL_TRIMSF_REL)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Get the Surface fixed data
*/
	ns.key = key;
	if (ur_retrieve_data_fixed(&ns) != 0)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
	(*surf_rec).header.key = ns.key;
	(*surf_rec).header.relnum = NCLX_MDL_TRIMSF;
	strcpy((*surf_rec).header.label,ns.label);
	(*surf_rec).header.subscript = ns.subscr;
	(*surf_rec).offdist = ns.offdist;
	(*surf_rec).u_min = ns.u_min;
	(*surf_rec).v_min = ns.v_min;
	(*surf_rec).u_max = ns.u_max;
	(*surf_rec).v_max = ns.v_max;
/*   ------   FIX   ------ set drive_type to field from NCL trimmed surface   */
	(*surf_rec).trim_type = NCLX_FACE;
/*
.....Get the base surface
*/
	isiz = sizeof(NCLX_mdl_surf);
	surf_rec->surf = (NCLX_mdl_surf *)malloc(isiz);
	if (NclxMdlGetSurf(ns.bs_key,surf_rec->surf) != NCLX_SUCCESS)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
	if (ur_retrieve_attr(ns.bs_key,&sfattr) != 0)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Store the surface attributes
*/
	(*surf_rec).surf->sfhead.material = sfattr.material;
	(*surf_rec).surf->sfhead.upaths = sfattr.numupaths;
	(*surf_rec).surf->sfhead.vpaths = sfattr.numvpaths;
	(*surf_rec).surf->sfhead.upts = sfattr.ptsperucrv;
	(*surf_rec).surf->sfhead.vpts = sfattr.ptspervcrv;
	(*surf_rec).surf->sfhead.reverse = UU_FALSE;
	(*surf_rec).surf->sfhead.uclosed = ns.closdinu;
	(*surf_rec).surf->sfhead.vclosed = ns.closdinv;
/*
.....Retrieve the attribute bundle
*/
	NclxMdlGetAttr(surf_rec);
/*
.....Retrieve the label location
*/
	ncl_retrieve_labloc(&ns,(*surf_rec).header.attrib.label_on,pos);
	(*surf_rec).header.label_pos[0] = pos[0];
	(*surf_rec).header.label_pos[1] = pos[1];
	(*surf_rec).header.label_pos[2] = pos[2];
/*
.....Retrieve the outer boundary UV curve
*/
	isiz = sizeof(NCLX_mdl_data);
	surf_rec->uv_cv = (NCLX_mdl_data *)malloc(isiz);
	if (NclxMdlGetGeom(ns.uv_key,surf_rec->uv_cv) != NCLX_SUCCESS)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Retrieve the outer boundary XYZ curve
*/
	if (ns.cv_key != 0)
	{
		isiz = sizeof(NCLX_mdl_data);
		surf_rec->xyz_cv = (NCLX_mdl_data *)malloc(isiz);
		if (NclxMdlGetGeom(ns.cv_key,surf_rec->xyz_cv) != NCLX_SUCCESS)
		{
			stat = NCLX_FAILURE;
			return(stat);
		}
	}
	else surf_rec->xyz_cv = UU_NULL;
/*
.....Retrieve the inner boundary UV curves
*/
	surf_rec->ncurve = ns.no_ibndykey;
	if (surf_rec->ncurve > 0)
	{
/*
........Retrieve the curve keys
*/
		isiz = sizeof(UU_KEY_ID) * surf_rec->ncurve;
		ibkey = (UU_KEY_ID *)malloc(isiz);
		isiz = sizeof(surf_rec->inner) * surf_rec->ncurve;
		*(surf_rec->inner) = (UU_KEY_ID *)malloc(isiz);
		ur_retrieve_data_varlist(ns.key,1,ibkey,1,surf_rec->ncurve);
		isiz = sizeof(NCLX_mdl_data);
		for (i=0;i<surf_rec->ncurve;i++)
		{
			surf_rec->inner[i] = (NCLX_mdl_data *)malloc(isiz);
			if (NclxMdlGetGeom(ibkey[i],surf_rec->inner[i]) != NCLX_SUCCESS)
			{
				stat = NCLX_FAILURE;
				return(stat);
			}
		}
	}
/*
..... Retrieve the bounding box and boundary strunctures
*/
	stat = NclxMdlGetList (NCLX_BOX_LIST,key,surf_rec);	
	if (stat != NCLX_SUCCESS) return (NCLX_FAILURE);
	stat = NclxMdlGetList (NCLX_BOUNDARY_LIST,key,surf_rec);	
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetCurve(key,cv_rec)
**       Gets a Curve record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          cv_rec            Curve retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetCurve(key,cv_rec)
NCLX_KEY_ID key;
NCLX_mdl_curve *cv_rec;
{
	struct NCL_curve_rec nc;
	struct UM_rbsplcrv_rec nb;
	struct UM_uvcvonsf_rec nu;
	int stat,relnum,isiz;
	UU_REAL pos[3];
/*
.....Verify this is actually an NCL curve
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || (relnum != NCL_CURVE_REL && relnum != UM_RBSPLCRV_REL &&
		relnum != UM_UVCVONSF_REL))
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....NCL Curve
*/
	if (relnum == NCL_CURVE_REL)
	{
/*
........Get the Curve fixed data
*/
		nc.key = key;
		if (ur_retrieve_data_fixed(&nc) != 0)
		{
			stat = NCLX_FAILURE;
			return(stat);
		}
		(*cv_rec).header.key = nc.key;
		(*cv_rec).header.relnum = NCLX_MDL_CURVE;
		strcpy((*cv_rec).header.label,nc.label);
		(*cv_rec).header.subscript = nc.subscr;
		(*cv_rec).cvhead.closed = nc.closdinu;
		(*cv_rec).cvhead.t0 = nc.t0;
		(*cv_rec).cvhead.t1 = nc.t1;
		(*cv_rec).cvhead.tlen = nc.t_end;
/*
.....Retrieve the attribute bundle
*/
		NclxMdlGetAttr(cv_rec);
/*
.....Retrieve the label location
*/
		ncl_retrieve_labloc(&nc,(*cv_rec).header.attrib.label_on,pos);
		(*cv_rec).header.label_pos[0] = pos[0];
		(*cv_rec).header.label_pos[1] = pos[1];
		(*cv_rec).header.label_pos[2] = pos[2];
/*
.....Set the evaluator
*/
		(*cv_rec).evaluator = NclxMdlEvalCurve;
/*
.....Retrieve the parameters
*/
		cv_rec->nparm = nc.no_param;
		if (cv_rec->nparm > 0)
		{
			isiz = sizeof(float) * cv_rec->nparm;
			cv_rec->parms = (float *)malloc(isiz);
			ur_retrieve_data_varlist(nc.key,1,cv_rec->parms,1,cv_rec->nparm);
		}
/*
.....Retrieve the segments
*/
		cv_rec->nsegment = nc.no_segment;
		if (cv_rec->nsegment > 0)
		{
			isiz = sizeof(NCLX_mdl_curve_segment) * cv_rec->nsegment;
			cv_rec->segment = (NCLX_mdl_curve_segment *)malloc(isiz);
			ur_retrieve_data_varlist(nc.key,2,cv_rec->segment,1,cv_rec->nsegment);
		}
	}
/*
.....Bspline Curve
*/
	else if (relnum == UM_RBSPLCRV_REL)
	{
/*
........Get the Curve fixed data
*/
		nb.key = key;
		if (ur_retrieve_data_fixed(&nb) != 0)
		{
			stat = NCLX_FAILURE;
			return(stat);
		}
		(*cv_rec).header.key = nb.key;
		(*cv_rec).header.relnum = NCLX_MDL_BSPLINE;
		strcpy((*cv_rec).header.label,nb.label);
		(*cv_rec).header.subscript = nb.subscr;
		(*cv_rec).cvhead.closed = nb.closdinu;
		(*cv_rec).cvhead.t0 = nb.t0;
		(*cv_rec).cvhead.t1 = nb.t1;
		(*cv_rec).cvhead.degree = nb.k;
		(*cv_rec).cvhead.degseg = nb.n;
/*
.....Retrieve the attribute bundle
*/
		NclxMdlGetAttr(cv_rec);
	/*
.....Retrieve the label location
*/
		ncl_retrieve_labloc(&nb,(*cv_rec).header.attrib.label_on,pos);
		(*cv_rec).header.label_pos[0] = pos[0];
		(*cv_rec).header.label_pos[1] = pos[1];
		(*cv_rec).header.label_pos[2] = pos[2];
/*
.....Set the evaluator
*/
		(*cv_rec).evaluator = NclxMdlEvalCurve;
/*
.....Retrieve the t-parameters
*/
		cv_rec->ntparm = nb.no_t;
		if (cv_rec->ntparm > 0)
		{
			isiz = sizeof(double) * cv_rec->ntparm;
			cv_rec->tparms = (double *)malloc(isiz);
			ur_retrieve_data_varlist(nb.key,1,cv_rec->tparms,1,cv_rec->ntparm);
		}
/*
.....Retrieve the points
*/
		cv_rec->npt = nb.no_pt;
		if (cv_rec->npt > 0)
		{
			isiz = sizeof(double) * cv_rec->npt * 3;
			cv_rec->pt = (double *)malloc(isiz);
			ur_retrieve_data_varlist(nb.key,2,cv_rec->pt,1,cv_rec->npt);
		}
/*
.....Retrieve the weights
*/
		cv_rec->nwgt = nb.no_wt;
		if (cv_rec->nwgt > 0)
		{
			isiz = sizeof(double) * cv_rec->nwgt;
			cv_rec->wgt = (double *)malloc(isiz);
			ur_retrieve_data_varlist(nb.key,3,cv_rec->wgt,1,cv_rec->nwgt);
		}
	}
/*
.....UV Curve on Surface
*/
	else
	{
/*
........Get the Curve fixed data
*/
		nu.key = key;
		if (ur_retrieve_data_fixed(&nu) != 0)
		{
			stat = NCLX_FAILURE;
			return(stat);
		}
		(*cv_rec).header.key = nu.key;
		(*cv_rec).header.relnum = NCLX_MDL_BSPLINE;
		strcpy((*cv_rec).header.label,nu.label);
		(*cv_rec).header.subscript = nu.subscr;
		(*cv_rec).cvhead.closed = nu.closdinu;
		(*cv_rec).cvhead.t0 = nu.t0;
		(*cv_rec).cvhead.t1 = nu.t1;
		(*cv_rec).cvhead.degree = nu.k;
		(*cv_rec).cvhead.degseg = nu.n;
/*
.....Retrieve the attribute bundle
*/
		NclxMdlGetAttr(cv_rec);
	/*
.....Retrieve the label location
*/
		ncl_retrieve_labloc(&nu,(*cv_rec).header.attrib.label_on,pos);
		(*cv_rec).header.label_pos[0] = pos[0];
		(*cv_rec).header.label_pos[1] = pos[1];
		(*cv_rec).header.label_pos[2] = pos[2];
/*
.....Set the evaluator
*/
		(*cv_rec).evaluator = NclxMdlEvalCurve;
/*
.....Retrieve the t-parameters
*/
		cv_rec->ntparm = nu.no_t;
		if (cv_rec->ntparm > 0)
		{
			isiz = sizeof(double) * cv_rec->ntparm;
			cv_rec->tparms = (double *)malloc(isiz);
			ur_retrieve_data_varlist(nu.key,1,cv_rec->tparms,1,cv_rec->ntparm);
		}
/*
.....Retrieve the points
*/
		cv_rec->npt = nu.no_pt;
		if (cv_rec->npt > 0)
		{
			isiz = sizeof(double) * cv_rec->npt * 3;
			cv_rec->pt = (double *)malloc(isiz);
			ur_retrieve_data_varlist(nu.key,2,cv_rec->pt,1,cv_rec->npt);
		}
/*
.....Retrieve the weights
*/
		cv_rec->nwgt = nu.no_wt;
		if (cv_rec->nwgt > 0)
		{
			isiz = sizeof(double) * cv_rec->nwgt;
			cv_rec->wgt = (double *)malloc(isiz);
			ur_retrieve_data_varlist(nu.key,3,cv_rec->wgt,1,cv_rec->nwgt);
		}
	}
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlGetComposite(key,cv_rec)
**       Gets a Composite Curve record from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          cv_rec            Composite Curve retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMdlGetComposite(key,cv_rec)
NCLX_KEY_ID key;
NCLX_mdl_composite *cv_rec;
{
	struct UM_compcrv_rec nc;
	NCLX_mdl_line *ln;
	NCLX_mdl_circle *ci;
	NCLX_mdl_curve *cv;
	int stat,relnum,i,isiz,ncid;
	UU_REAL pos[3];
/*
.....Verify this is actually a Composite curve
*/
	stat = NCLX_SUCCESS;
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0 || relnum != UM_COMPCRV_REL)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
/*
.....Get the Curve fixed data
*/
	nc.key = key;
	if (ncl_retrieve_data_fixed(&nc) != 0)
	{
		stat = NCLX_FAILURE;
		return(stat);
	}
	(*cv_rec).header.key = nc.key;
	(*cv_rec).header.relnum = NCLX_MDL_COMPOSITE;
	strcpy((*cv_rec).header.label,nc.label);
	(*cv_rec).header.subscript = nc.subscr;
	(*cv_rec).cvhead.closed = nc.closdinu;
	(*cv_rec).cvhead.length = nc.arclen;
	(*cv_rec).cvhead.planar = nc.planar;
	(*cv_rec).cvhead.continuity = nc.continuity;
	(*cv_rec).cvhead.fcolor = nc.fcolor;
/*
.....Retrieve the attribute bundle
*/
	NclxMdlGetAttr(cv_rec);
/*
.....Retrieve the label location
*/
	ncl_retrieve_labloc(&nc,(*cv_rec).header.attrib.label_on,pos);
	(*cv_rec).header.label_pos[0] = pos[0];
	(*cv_rec).header.label_pos[1] = pos[1];
	(*cv_rec).header.label_pos[2] = pos[2];
/*
.....Set the evaluator
*/
	(*cv_rec).evaluator = NclxMdlEvalComposite;
/*
.....Allocate space for the component curves
*/
		ncid = nc.no_cid;
		cv_rec->cvid = 0;
		if (ncid > 0)
		{
			isiz = sizeof(NCLX_mdl_cmp_entity) * ncid;
			cv_rec->cvid = (NCLX_mdl_cmp_entity *)malloc(isiz);
		}
/*
.....Retrieve the component curves
*/
	cv_rec->ncurve = ncid;
	for (i=0;i<ncid;i++)
	{
/*
........Get curve type
*/
		stat = ur_retrieve_data_relnum(nc.cid[i].crvid,&relnum);
		(*cv_rec).cvid[i].reverse = nc.cid[i].reverse;
		(*cv_rec).cvid[i].endparam = nc.cid[i].endparam;
/*
...........LINE
*/
		if (relnum == UM_LINE_REL || relnum == NCL_LINE_REL)
		{
			ln = (NCLX_mdl_line *) malloc(sizeof(NCLX_mdl_line));
			NclxMdlGetGeom(nc.cid[i].crvid,ln);
			(*cv_rec).cvid[i].curve = ln;
		}
/*
...........CIRCLE
*/
		else if (relnum == UM_CIRCLE_REL || relnum == NCL_CIRCLE_REL)
		{
			ci = (NCLX_mdl_circle *) malloc(sizeof(NCLX_mdl_circle));
			NclxMdlGetGeom(nc.cid[i].crvid,ci);
			(*cv_rec).cvid[i].curve = ci;
		}
/*
...........CURVE
*/
		else if (relnum == UM_RBSPLCRV_REL || relnum == NCL_CURVE_REL ||
			relnum == UM_UVCVONSF_REL)
		{
			cv = (NCLX_mdl_curve *) malloc(sizeof(NCLX_mdl_curve));
			NclxMdlGetGeom(nc.cid[i].crvid,cv);
			(*cv_rec).cvid[i].curve = cv;
		}
/*
.......Unknown entity
*/
		else
		{
			stat = NCLX_FAILURE;
			return(stat);
		}
	}
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMdlGetGeom(key,mdl_rec)
**       Generic function to get a geometric entity from the Unibase.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          cv_rec            Geometric Entity retreived from UNIBASE.
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMdlGetGeom(key,mdl_rec)
NCLX_KEY_ID key;
NCLX_mdl_data *mdl_rec;
{
	int stat,relnum;
/*
.....Get relation number
*/
	stat = ur_retrieve_data_relnum(key,&relnum);
	if (stat != 0) stat = NCLX_FAILURE;
/*
.....Load geometry
*/
	else
	{
		switch (relnum)
		{
		case UM_CIRCLE_REL:
		case NCL_CIRCLE_REL:
			stat = NclxMdlGetCircle(key,(NCLX_mdl_circle *)mdl_rec);
			break;
		case UM_CONIC_REL:
			stat = NCLX_FAILURE;
			break;
		case UM_COMPCRV_REL:
			stat = NclxMdlGetComposite(key,(NCLX_mdl_composite *)mdl_rec);
			break;
		case NCL_CURVE_REL:
		case UM_RBSPLCRV_REL:
		case UM_UVCVONSF_REL:
			stat = NclxMdlGetCurve(key,(NCLX_mdl_curve *)mdl_rec);
			break;
		case NCL_DATAST_REL:
			stat = NCLX_FAILURE;
			break;
		case UM_LINE_REL:
		case NCL_LINE_REL:
			stat = NclxMdlGetLine(key,(NCLX_mdl_line *)mdl_rec);
			break;
		case NCL_MATRIX_REL:
			stat = NCLX_FAILURE;
			break;
		case NCL_NETSF_REL:
			stat = NclxMdlGetNetsf(key,(NCLX_mdl_netsf *)mdl_rec);
			break;
		case NCL_PATERN_REL:
			stat = NclxMdlGetPatern(key,(NCLX_mdl_patern *)mdl_rec);
			break;
		case NCL_PLN_REL:
			stat = NclxMdlGetPlane(key,(NCLX_mdl_plane *)mdl_rec);
			break;
		case UM_POINT_REL:
		case NCL_POINT_REL:
			stat = NCLX_FAILURE;
			break;
		case NCL_POINTVEC_REL:
			stat = NclxMdlGetPntvec(key,(NCLX_mdl_pntvec *)mdl_rec);
			break;
		case NCL_EVALCV_REL:
			stat = NCLX_FAILURE;
			break;
		case NCL_EVALSF_REL:
			stat = NCLX_FAILURE;
			break;
		case NCL_SCALAR_REL:
			stat = NCLX_FAILURE;
			break;
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
			stat = NclxMdlGetSurf(key,(NCLX_mdl_surf *)mdl_rec);
			break;
		case NCL_TRIMSF_REL:
			stat = NclxMdlGetTrimsf(key,(NCLX_mdl_trimsf *)mdl_rec);
			break;
		case NCL_VECTOR_REL:
			stat = NCLX_FAILURE;
			break;
		case NCL_SHAPE_REL:
			stat = NclxMdlGetShape(key,(NCLX_mdl_shape *)mdl_rec);
			break;
		default:
			stat = NCLX_FAILURE;
			break;
		}
	}
/*
.....End of routine
*/
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMdlGetList(type,key,surf_rec)
**       Get a list (BOX_LIST, BOUNDARY_LIST)  from the Unibase surface record.
**    PARAMETERS
**       INPUT  :
**          type - list type (BOX_LIST, BOUNDARY_LIST)
**          key               UNIBASE key of entity to get.
**       OUTPUT :
**          surf_rec is updated 
**    RETURNS      :
**       NCLX_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMdlGetList(type,key,surf_rec)
NCLX_mdl_srflist_type type;
NCLX_KEY_ID key;
NCLX_mdl_struct *surf_rec;
{
	int stat,relnum,no_boxlst,no_bndrylst,lst;
	struct NCL_surface_rec ns;
	struct UM_rbsplsrf_rec nb;
	struct NCL_trimsf_rec  nt;
	NCLX_mdl_surf *surf = 0;
	NCLX_mdl_trimsf *trimsf = 0;
/*
.....Verify this is actually a surface
*/
   stat = NCLX_SUCCESS;
   if (ur_retrieve_data_relnum(key,&relnum) != 0) return (NCLX_FAILURE);

	switch (relnum)
	{
		case NCL_SURF_REL:
			surf = (NCLX_mdl_surf *) surf_rec;
			ns.key = key;
			if (uc_retrieve_data(&ns) != 0) return (NCLX_FAILURE);
			no_boxlst = ns.no_boxlst;
			no_bndrylst = ns.no_xyzbylst;
			break;	
		case UM_RBSPLSRF_REL:
			surf = (NCLX_mdl_surf *) surf_rec;
			nb.key = key;
			if (uc_retrieve_data(&nb) != 0) return (NCLX_FAILURE);
			no_boxlst = nb.no_boxlst;
			no_bndrylst = nb.no_xyzbylst;
			break;	
		case NCL_TRIMSF_REL:
			trimsf = (NCLX_mdl_trimsf *) surf_rec;
			nt.key = key;
			if (uc_retrieve_data(&nt) != 0) return (NCLX_FAILURE);
			no_boxlst = nt.no_boxlst;
			no_bndrylst = nt.no_xyzbylst;
			break;	
		default:
	      return (NCLX_FAILURE);
			break;
	}

	switch (type)
	{
		case NCLX_BOX_LIST:
 			lst = ncl_surflist_number (BOX_LIST,relnum);
			if (surf)
			{
				surf->no_boxlst = 0;
				surf->boxlst = 0;
				if (no_boxlst > 0)
				{
					surf->no_boxlst = no_boxlst;
					surf->boxlst = (double *) uu_malloc(sizeof(double) * no_boxlst);
					stat = ur_retrieve_data_varlist(key,lst,surf->boxlst,1,no_boxlst);
				}
			}
			else if (trimsf)
			{
				trimsf->no_boxlst = 0;
				trimsf->boxlst = 0;
				if (no_boxlst > 0)
				{
					trimsf->no_boxlst = no_boxlst;
					trimsf->boxlst = (double *) uu_malloc(sizeof(double) * no_boxlst);
					stat = ur_retrieve_data_varlist(key,lst,trimsf->boxlst,1,no_boxlst);
				}
			}
			break;

		case NCLX_BOUNDARY_LIST:
			lst = ncl_surflist_number (WHOLE_BOUNDARY_LIST, relnum);
			if (surf)
			{
				surf->no_bndrylst = 0;
				surf->bndrylst = 0;
				if (no_bndrylst > 0)
				{
					surf->no_bndrylst = no_bndrylst;
					surf->bndrylst = (double *)uu_malloc(sizeof(double) * no_bndrylst);
					stat = ur_retrieve_data_varlist(key,lst,surf->bndrylst,1,no_bndrylst);
				}
			}
			else if (trimsf)
			{
				trimsf->no_bndrylst = 0;
				trimsf->bndrylst = 0;
				if (no_bndrylst > 0)
				{
					trimsf->no_bndrylst = no_bndrylst;
					trimsf->bndrylst = (double *)uu_malloc(sizeof(double) * no_bndrylst);
					stat = ur_retrieve_data_varlist(key,lst,trimsf->bndrylst,1,no_bndrylst);
				}
			}
			break;
		default:
			return (NCLX_FAILURE);
			break;
	}

	return (stat);
}
