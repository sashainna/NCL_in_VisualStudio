/*********************************************************************
**    NAME         :  tspmdlput.c
**       CONTAINS:
**					utp_store_circle
**					utp_store_composite
**					utp_store_conic
**					utp_store_curve
**					utp_store_line
**					utp_store_plane
**					utp_store_point
**					utp_store_surf
**					utp_store_trimsf
**					utp_store_solid
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tspmdlput.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:23
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "tstep.h"
#include "mcrv.h"
#include "mdattr.h"
#include "mdrel.h"
#include "msrf.h"
#include "msol.h"
#include "nccs.h"
#include "ncl.h"
#include "tzclass.h"
#include "udebug.h"
#include "tigglobal.h"

extern int NCL_ubcopy;
extern UU_LOGICAL UTP_debug_label;

static int Sci_sub=0;
static int Scv_sub=0;
static int Sln_sub=0;
static int Spt_sub=0;
static int Ssf_sub=0;

/*********************************************************************
**    E_FUNCTION     :  utp_store_circle(label,crec)
**				Stores a Circle record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          label    Label of circle to store.
**          crec     Circle record to store.
**       OUTPUT : none
**    RETURNS      :
**          Key of created circle.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_store_circle(crec,label)
struct UM_circle_rec *crec;
char *label;
{
/*
.....Create label
*/
	if (UTP_debug_label)
		sprintf(crec->label,"CI%d",++Sci_sub);
	else
		strcpy(crec->label,label);
	crec->subscr = 0;
	um_init_lablocal(crec);
/*
.....Store circle in Unibase
*/
	NCL_ubcopy = 2;
	crec->displst = UU_NULL;
	um_create_geom(crec,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	NCL_ubcopy = 0;
/*
.....Store attribute bundle
*/
	if (crec->key != 0) utp_store_attr(crec->key,UM_BLUE);
	return(crec->key);;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_composite(comp,label,cvid,ncvs)
**				Stores a Composite Curve record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          comp     Composite curve record to store.
**          label    Label of line to store.
**          cvid     List of curves that form the composite curve.
**          ncvs     Number of curves in 'cvid'.
**       OUTPUT : none
**    RETURNS      :
**          Key of created composite curve.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_store_composite(comp,label,cvid,ncvs)
struct UM_compcrv_rec *comp;
char *label;
struct UM_cid_rec *cvid;
int ncvs;
{
	int i,stat;
	struct UM_compcrv_rec cv;
/*
.....Initialize composite curve record
*/
	ur_setup_data(UM_COMPCRV_REL,&cv,sizeof(cv));
	cv.closdinu = comp->closdinu;
	cv.arclen = comp->arclen;
	cv.planar = comp->planar;
	cv.open = comp->open;
	cv.continuity = comp->continuity;
	cv.fcolor = comp->fcolor;
	cv.t0 = comp->t0;
	cv.t1 = comp->t1;
	cv.addflg = comp->addflg;
/*
.....Create label
*/
	if (UTP_debug_label)
		sprintf(cv.label,"CV%d",++Scv_sub);
	else
		strcpy(cv.label,"@UN");
	cv.subscr = 0;
	um_init_lablocal(&cv);
/*
.....Store composite curve in Unibase
*/
	NCL_ubcopy = 2;
	cv.displst = UU_NULL;
	um_create_geom(&cv,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	NCL_ubcopy = 0;
/*
.....Store variable list data
*/
	stat = ur_update_data_varlist(cv.key,1,cvid,1,ncvs);
	if (stat == UU_FAILURE) cv.key = 0;
/*
.....Store attribute bundle
*/
	if (cv.key != 0) utp_store_attr(cv.key,UM_YELLOW);
	return(cv.key);;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_conic(label,crec)
**				Stores a Conic record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          label    Label of conic to store.
**          crec     Conic record to store.
**       OUTPUT : none
**    RETURNS      :
**          Key of created conic.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_store_conic(crec,label)
struct UM_conic_rec *crec;
char *label;
{
/*
.....Create label
*/
	if (UTP_debug_label)
		sprintf(crec->label,"CO%d",++Sci_sub);
	else
		strcpy(crec->label,label);
	crec->subscr = 0;
	um_init_lablocal(crec);
/*
.....Store circle in Unibase
*/
	NCL_ubcopy = 2;
	crec->displst = UU_NULL;
	um_create_geom(crec,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	NCL_ubcopy = 0;
/*
.....Store attribute bundle
*/
	if (crec->key != 0) utp_store_attr(crec->key,UM_BLUE);
	return(crec->key);;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_curve(cv,label,flag)
**				Stores a Curve record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          cv       Curve record to store.
**          label    Label of entity to store.
**          flag     UU_TRUE = free variable lists after storing
**                   surface.  UU_FALSE = Don't free lists.
**       OUTPUT : none
**    RETURNS      :
**          Key of created surface.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_store_curve(cv,label,flag)
struct UM_rbsplcrv_rec *cv;
char *label;
UU_LOGICAL flag;
{
	int i,stat;
/*
.....Create label
*/
	if (UTP_debug_label)
		sprintf(cv->label,"CV%d",++Scv_sub);
	else
		strcpy(cv->label,label);
	cv->subscr = 0;
	um_init_lablocal(cv);
/*
.....Store trimmed surface in Unibase
*/
	NCL_ubcopy = 2;
	cv->displst = UU_NULL;
	um_create_geom(cv,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	NCL_ubcopy = 0;
/*
.....Store variable list data
*/
	if (cv->no_t != 0)
		stat = ur_update_data_varlist(cv->key,1,cv->t,1,cv->no_t);
	if (cv->no_t != 0 && stat == UU_SUCCESS)
		stat = ur_update_data_varlist(cv->key,2,cv->pt,1,cv->no_pt);
	if (cv->no_wt != 0 && stat == UU_SUCCESS)
		stat = ur_update_data_varlist(cv->key,3,cv->wt,1,cv->no_wt);
	if (stat == UU_FAILURE) cv->key = 0;
/*
.....Store attribute bundle
*/
	if (cv->key != 0) utp_store_attr(cv->key,UM_CYAN);
/*
.....Free variable lists
*/
	if (flag)
	{
		if (cv->no_t != 0) uu_free(cv->t);
		if (cv->no_pt != 0) uu_free(cv->pt);
		if (cv->no_wt != 0) uu_free(cv->wt);
	}
/*
.....End of routine
*/
	return(cv->key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_line(label,spt,ept)
**				Stores a Line record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          label    Label of entity to store.
**          spt      Start point of line.
**          ept      End point of line.
**       OUTPUT : none
**    RETURNS      :
**          Key of created line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_store_line(label,spt,ept)
char *label;
UM_coord spt,ept;
{
	struct UM_line_rec line;
/*
.....Initialize line record
*/
	ur_setup_data(UM_LINE_REL,&line,sizeof(line));
/*
.....Create label
*/
	if (UTP_debug_label)
		sprintf(line.label,"LN%d",++Sln_sub);
	else
		strcpy(line.label,label);
	line.subscr = 0;
	um_init_lablocal(&line);
/*
.....Store end points
*/
	um_vctovc(spt,line.spt);
	um_vctovc(ept,line.ept);
/*
.....Store line in Unibase
*/
	NCL_ubcopy = 2;
	line.displst = UU_NULL;
	um_create_geom(&line,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	NCL_ubcopy = 0;
/*
.....Store attribute bundle
*/
	if (line.key != 0) utp_store_attr(line.key,UM_GREEN);
	return(line.key);;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_plane(label,pln,ept)
**				Stores a Plane record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          label    Label of entity to store.
**          pln      NCL plane record to store.
**       OUTPUT : none
**    RETURNS      :
**          Key of created plane.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_store_plane(label,recno,pln)
char *label;
int recno;
struct NCL_nclpl_rec *pln;
{
/*
.....Create label
*/
	ur_setup_data(NCL_PLN_REL,pln,sizeof(struct NCL_nclpl_rec));
	utp_create_step_label(NCL_PLN_REL,label,recno,pln->label,&pln->subscr);
	um_init_lablocal(pln);
/*
.....Store plane in Unibase
*/
	NCL_ubcopy = 2;
	pln->displst = UU_NULL;
	um_create_geom(pln,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	NCL_ubcopy = 0;
/*
.....Store attribute bundle
*/
	if (pln->key != 0) utp_store_attr(pln->key,UM_GREEN);
	return(pln->key);;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_point(label,pt,marker)
**				Stores a Point record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          label    Label of entity to store.
**          pt       Coordinates of point.
**          marker   Marker type to store with point.
**       OUTPUT : none
**    RETURNS      :
**          Key of created point.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_store_point(label,pt,marker)
char *label;
UM_coord pt;
int marker;
{
	struct UM_point_rec e;
/*
.....Initialize point record
*/
	ur_setup_data(UM_POINT_REL,&e,sizeof(e));
	e.markertype = marker;
/*
.....Create label
*/
	if (UTP_debug_label)
		sprintf(e.label,"PT%d",++Spt_sub);
	else
		strcpy(e.label,label);
	e.subscr = 0;
	um_init_lablocal(&e);
/*
.....Store coordinates
*/
	um_vctovc(pt,e.pt);
/*
.....Store point in Unibase
*/
	NCL_ubcopy = 2;
	e.displst = UU_NULL;
	um_create_geom(&e,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	NCL_ubcopy = 0;
/*
.....Store attribute bundle
*/
	if (e.key != 0) utp_store_attr(e.key,UM_RED);
	return(e.key);;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_surf(sf,label,flag)
**				Stores a Surface record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          sf       Surface record to store.
**          label    Label of entity to store.
**          flag     UU_TRUE = free variable lists after storing
**                   surface.  UU_FALSE = Don't free lists.
**       OUTPUT : none
**    RETURNS      :
**          Key of created surface.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_store_surf(sf,label,flag)
struct UM_rbsplsrf_rec *sf;
char *label;
UU_LOGICAL flag;
{
	int i,stat,ptyp;
	UU_REAL pdata[16];
	struct UM_surfattr_rec attr;
/*
.....Create label
*/
	if (UTP_debug_label)
		sprintf(sf->label,"SF%d",++Ssf_sub);
	else
		strcpy(sf->label,label);
	sf->subscr = 0;
	um_init_lablocal(sf);
/*
.....Initialize attribute bundle
*/
	utp_init_surfattr(&attr,UM_MAGENTA,UIG_edge_color,UIG_srf_edge,
		shade_set,UU_TRUE);
/*
.....Store trimmed surface in Unibase
*/
	NCL_ubcopy = 1;
	um_create_geom(sf,UM_DEFAULT_TF,&attr);
	NCL_ubcopy = 0;
/*
.....Store variable list data
*/
	if (sf->no_tu != 0)
		stat = ur_update_data_varlist(sf->key,1,sf->tu,1,sf->no_tu);
	if (sf->no_tu != 0 && stat == UU_SUCCESS)
		stat = ur_update_data_varlist(sf->key,2,sf->tv,1,sf->no_tv);
	if (sf->no_tu != 0 && stat == UU_SUCCESS)
		stat = ur_update_data_varlist(sf->key,3,sf->pt,1,sf->no_pt);
	if (sf->no_wt != 0 && stat == UU_SUCCESS)
		stat = ur_update_data_varlist(sf->key,4,sf->wt,1,sf->no_wt);
	if (stat == UU_FAILURE) sf->key = 0;
/*
.....Store primitive data
*/
	if (sf->key != 0)
	{
		ncl_sf_prim_analyz(&sf->key,&ptyp,pdata);
/*
.....Store attribute bundle
*/
		utp_store_attr(sf->key,UM_YELLOW);
	}
/*
.....Free variable lists
*/
	if (flag)
	{
		if (sf->no_tu != 0) uu_free(sf->tu);
		if (sf->no_tv != 0) uu_free(sf->tv);
		if (sf->no_pt != 0) uu_free(sf->pt);
		if (sf->no_wt != 0) uu_free(sf->wt);
	}
/*
.....End of routine
*/
	return(sf->key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_trimsf(label,recno,bskey,uv,uvkeys,cvkeys,
**                                       ncvs,unklbl)
**				Stores a Trimmed Surface record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          label    Label of Face from STEP file.
**          recno    Record number of Advanced Face command.
**          bskey    Key of base surface.
**          uv       [0] = umin of surface, [1] = umax, [2] = vmin,
**                   [3] = vmax.
**          uvkeys   Key of UV boundary curves. [0] = Outer boundary.
**          cvkeys   Keys of XYZ boundary UV curves. [0] = Outer boundary.
**          ncvs     Number of boundary curves including the outer
**                   boundary.
**          unklbl   UU_TRUE = Store the trimmmed surfaces using the "@UN"
**                   label.
**       OUTPUT : none
**    RETURNS      :
**          Key of created Trimmed Surface.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_store_trimsf(label,recno,bskey,uv,uvkeys,cvkeys,ncvs,unklbl)
char *label;
int recno;
UU_KEY_ID bskey,*uvkeys,*cvkeys;
UU_REAL *uv;
int ncvs;
UU_LOGICAL unklbl;
{
	int i,stat,n,inc,color;
	UU_KEY_ID *keys;
	struct NCL_trimsf_rec tsf;
	struct UM_surfattr_rec attr;
/*
.....Initialize trimmed surface record
*/
	ur_setup_data(NCL_TRIMSF_REL,&tsf,sizeof(tsf));
	tsf.closdinu = tsf.closdinv = 0;
	tsf.offdist = 0.;
	tsf.u_min = uv[0]; tsf.u_max = uv[1];
	tsf.v_min = uv[2]; tsf.v_max = uv[3];
	tsf.ub_min = tsf.vb_min = 0.;
	tsf.ub_max = tsf.vb_max = 1.;
	tsf.drive_type = 0;
	tsf.bs_key = bskey;
	tsf.uv_key = uvkeys[0];
	tsf.cv_key = cvkeys[0];
/*
.....Create label
*/
	if (unklbl)
	{
		strcpy(tsf.label,"@UN");
		tsf.subscr = 0;
	}
	else
		utp_create_step_label(NCL_TRIMSF_REL,label,recno,tsf.label,&tsf.subscr);
	um_init_lablocal(&tsf);
/*
.....Initialize attribute bundle
*/
	if (unklbl) color = UM_LIGHTGREEN;
	else color = UM_MAGENTA;
	utp_init_surfattr(&attr,color,UIG_edge_color,UIG_srf_edge,
		shade_set,UU_TRUE);
/*
.....Store trimmed surface in Unibase
*/
	NCL_ubcopy = 1;
	um_create_geom(&tsf,UM_DEFAULT_TF,&attr);
	NCL_ubcopy = 0;
/*
.....Store variable list data
*/
	if (ncvs > 1)
	{
		n = (ncvs-1) * 2;
		keys = (UU_KEY_ID *)uu_malloc(n*sizeof(UU_KEY_ID));
		inc = 0;
		for (i=1;i<ncvs;i++)
		{
			keys[inc++] = cvkeys[i];
			keys[inc++] = uvkeys[i];
		}
		stat = ur_update_data_varlist(tsf.key,1,keys,1,n);
		if (stat == UU_FAILURE) tsf.key = 0;
		uu_free(keys);
	}
/*
.....Store attribute bundle
.......Need to create attribute record for base surface - ASF 7/2/13.
*/
	attr.key = bskey;
	ur_retrieve_attr(&attr);
	attr.displayable = UM_NEVERDISPLAYABLE;
	ur_update_attr(&attr);
/*
.....Creating a solid
.....Make trimmed surface not displayable
*/
	if (unklbl)
	{
		attr.key = tsf.key;
		ur_retrieve_attr(&attr);
		attr.displayable = UM_NEVERDISPLAYABLE;
		ur_update_attr(&attr);
	}
	return(tsf.key);;
}

/*********************************************************************
**    E_FUNCTION     :  utp_store_solid(label,recno,sfkeys,nkeys,closed)
**				Stores a Composite Solid record in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          label    Label of Closed shell from STEP file.
**          recno    Record number of Closed Shell command.
**          sfkeys   Keys trimmed surfaces comprising 
**          nkeys    Number of surface keys provided in 'sfkeys'.
**          closed   UU_TRUE = Solid is closed, UU_FALSE = Solid is open.
**       OUTPUT : none
**    RETURNS      :
**          Key of created Composite Solid.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_store_solid(label,recno,sfkeys,nkeys,closed)
char *label;
int recno;
UU_KEY_ID *sfkeys;
int nkeys;
UU_LOGICAL closed;
{
	int i,stat,n,inc;
	UU_KEY_ID *keys;
	struct UM_solid_rec solid;
	struct UM_surfattr_rec attr;
/*
.....Initialize composite solid record
*/
	ur_setup_data(UM_SOLID_REL,&solid,sizeof(solid));
	solid.type = UM_COMPOS_SOLID;
	solid.closed = closed;
	for (i=0;i<6;i++) solid.box[i] = 0.;
/*
.....Create label
*/
	utp_create_step_label(UM_SOLID_REL,label,recno,solid.label,&solid.subscr);
	um_init_lablocal(&solid);
/*
.....Initialize attribute bundle
*/
	utp_init_surfattr(&attr,UM_LIGHTGREEN,UIG_edge_color,UIG_srf_edge,
		shade_set,UU_TRUE);
/*
.....Store composite solid in Unibase
*/
	NCL_ubcopy = 1;
	um_create_geom(&solid,UM_DEFAULT_TF,&attr);
	NCL_ubcopy = 0;
/*
.....Store variable list data
*/
	stat = ur_update_data_varlist(solid.key,4,sfkeys,1,nkeys);
	if (stat == UU_FAILURE) solid.key = 0;
	return(solid.key);;
}
