/*********************************************************************
**    NAME         :  tsosrf.c
**       CONTAINS:
**			  utp_out_trimsf
**         utp_out_surf
**         utp_out_plane
**         utp_out_sphere
**         utp_out_cylinder
**         utp_out_cone
**         utp_out_torus
**         utp_out_revsf
**         utp_out_nsurf
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tsosrf.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			10/27/16 , 15:13:16
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "class.h"
#include "nccs.h"
#include "mattr.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mgeom.h"
#include "modef.h"
#include "msol.h"
#include "ncl.h"
#include "nconst.h"
#include "tstep.h"
#include "tiges.h"
#include "tioconv.h"
#include "nclver.h"
#include "xenv1.h"
#include "riddle.h"
#include <time.h>

/*
.....can not define Sdeflab as such, in this way, Sdeflab can not re-assigned
.....it will cause error later
*/
//static char Sbuf[81920],*Sdeflab={"NONE"}; 
static char Sbuf[81920];
static char Sdeflab[] = {"NONE"};

extern int output_normals;
extern UM_int2 NCL_ubas_unit;

/*********************************************************************
**    E_FUNCTION :  utp_out_trimsf(trimsf)
**			Output a Trim Surface entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          trimsf  = Trimmed Surface entity to output.
**       OUTPUT : none
**    RETURNS      : 
**          Record number of trimmed surface (ADVANCED_FACE) output to
**          STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_trimsf(trimsf)
struct NCL_trimsf_rec *trimsf;
{
	int i,recno,irec,jrec,status,*recs,nrec,inc,ncvs,revfl,reverse[2],ofl;
	UU_LOGICAL sense,adjfl;
	UU_KEY_ID *keys;
	char tbuf[20],buf[80];
	UM_coord spt,ept;
	UM_transf tfmat;
	struct UM_transf_rec tran;
	struct NCL_fixed_databag ent,srf;
/*
.....Initialize routine
*/
	recno = 0;
	keys = UU_NULL;
	ncvs = trimsf->no_ibndykey/2 + 2;
	recs = (int *)uu_malloc(sizeof(int)*ncvs);
	if (recs == UU_NULL) goto done;
	keys = (int *)uu_malloc(sizeof(UU_KEY_ID)*ncvs);
	if (recs == UU_NULL) goto done;
	for (i=0;i<ncvs;i++) recs[i] = 0;
if (trimsf->key == 542)
{
	recno = 0;
}
/*
.....Get transformation matrix
*/
	uc_retrieve_transf(trimsf->key,tfmat);
	if (!um_is_idmat(tfmat))
	{
		S_xform_trimsf(trimsf,tfmat);
		tran.key = trimsf->key;
		tran.rel_num = UM_TRANSFORM_REL;
		ur_retrieve_transf(&tran);
		um_tftotf(UM_idmat,tran.tfmat);
		ur_update_transf(&tran);
	}
/*
.....Create XYZ boundary curves if necessary
*/
	S_xyz_boundary(trimsf);
/*
.....Adjust boundary curves if necessary
*/
	adjfl = S_adjust_boundary(trimsf,keys,&ncvs,reverse);
/*
.....Output the boundary curves
*/
	nrec = 0;
	strcpy(buf,"FACE_OUTER_BOUND");
	for (i=0;i<ncvs;i++)
	{
		ent.key = keys[i];
		status = ncl_retrieve_data_fixed(&ent);
		if (status != UU_SUCCESS) goto done;
		if (i < 2) revfl = reverse[i];
		else revfl = UU_TRUE;
		irec = utp_out_curve(&ent,spt,ept,revfl,UU_TRUE);
/*
........Adjusted boundary curves are not composite curves
........Output EDGE_LOOP command in this case
*/
		if (adjfl && i < 2)
		{
			sprintf(Sbuf,"EDGE_LOOP('%s',(#%d));",Sdeflab,irec);
			irec = utp_out_record(Sbuf);
		}
/*
........Output FACE_BOUND record
*/
		if (irec != 0)
		{
			utp_get_logical_str(UU_TRUE,tbuf);
			sprintf(Sbuf,"%s('%s',#%d,%s);",buf,Sdeflab,irec,tbuf);
			recs[nrec++] = utp_out_record(Sbuf);
		}
/*
........Setup for next curve
*/
		if (i != 0 || !adjfl) strcpy(buf,"FACE_BOUND");
	}
/*
....Output the surface
*/
	srf.key = trimsf->bs_key;
	status = ncl_retrieve_data_fixed(&srf);
	if (status != UU_SUCCESS) goto done;
	jrec = utp_out_surf(&srf,&sense);
/*
.....Output ADVANCED_FACE record
*/
	if (nrec != 0 && jrec != 0)
	{
		strcpy(tbuf,".U.");
		if (output_normals == 1) utp_get_logical_str(sense,tbuf);
		sprintf(Sbuf,"ADVANCED_FACE('%s',(",Sdeflab);
		ofl = 0;
		utp_out_multiple_record(Sbuf,&ofl,&recno);
		for (i=0;i<nrec;i++)
		{
			if (recs[i] != 0)
			{
				if (i != 0) utp_out_multiple_record(",",&ofl,&recno);
				sprintf(buf,"#%d",recs[i]);
				utp_out_multiple_record(buf,&ofl,&recno);
			}
		}
		sprintf(buf,"),#%d,%s);",jrec,tbuf);
		ofl = 2;
		utp_out_multiple_record(buf,&ofl,&recno);
	}
/*
.....End of routine
*/
done:;
	if (recs != UU_NULL) uu_free(recs);
	if (keys != UU_NULL) uu_free(keys);
	utp_reset_identity_mx();
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_surf(surf,sense)
**			Output a Surface entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          surf     = Surface entity to output.
**       OUTPUT :
**          sense    = Same_sense flag to output with ADVANCED_FACE.
**    RETURNS      :
**          Record number of surface output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_surf(surf,sense)
struct NCL_fixed_databag *surf;
UU_LOGICAL *sense;
{
	int recno,iswap,sense1;
	UM_int2 primtyp;
	UU_LOGICAL revnorm;
	UU_REAL primdata[16];
	static UM_vector xaxis={1,0,0}, zaxis={0,0,1};
/*
.....Initialize routine
*/
	recno = 0;
/*
.....Get primitive type of surface
*/
	ncl_get_sf_primdat(&surf->key,&primtyp,primdata);
	switch (primtyp)
	{
	case NCLSF_PLANE:
		recno = utp_out_plane(&primdata[4],&primdata[0],Sdeflab,0);
		break;
	case NCLSF_SPHERE:
		recno = utp_out_sphere(&primdata[0],zaxis,xaxis,primdata[3],Sdeflab,0);
		break;
	case NCLSF_CYLINDER:
		recno = utp_out_cylinder(primdata,Sdeflab,0);
		break;
	case NCLSF_CONE:
		recno = utp_out_cone(primdata,Sdeflab,0);
		break;
	case NCLSF_REVOLV:
		recno = utp_out_revsf(surf,Sdeflab,0);
		break;
	case NCLSF_TORUS:
		recno = utp_out_torus(primdata,Sdeflab,0);
		break;
	default:
		switch (surf->rel_num)
		{
		case NCL_REVSURF_REL:
			recno = utp_out_revsf(surf,Sdeflab,0);
			break;
		case UM_RBSPLSRF_REL:
			recno = utp_out_nsurf(surf,Sdeflab,0);
			break;
		default:
			recno = 0;
		}
		break;
	}
/*
.....Set the same sense flag
*/
	if (recno != 0)
	{
		*sense = 1;
		sense1 = ncl_get_surface_sense(surf);
		if (sense1 == -1) *sense = 0;
	}
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_plane(pt,vec,label,sub)
**			Output a Plane entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          pt      = Point on plane.
**          nvec    = Normal vector.
**          label   = Label of plane.
**          sub     = Subscript of plane.
**       OUTPUT : none
**    RETURNS      :
**          Record number of plane output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_plane(pt,nvec,label,sub)
UM_coord pt;
UM_vector nvec;
char *label;
int sub;
{
	int irec,recno;
	char tbuf[NCL_MAX_LABEL_AND_SUBSCRIPT];
	UM_vector svec;
/*
.....Output plane canonical data
*/
	um_perpvc(nvec,svec);
	irec = utp_out_ptaxis(pt,nvec,svec);
/*
.....Output plane command
*/
	utp_format_label(label,sub,tbuf);
	sprintf(Sbuf,"PLANE('%s',#%d);",tbuf,irec);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_sphere(pt,svec,evec,rad,label,sub)
**			Output a Sphere entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          pt      = Center of sphere.
**          svec    = Vector to start of boundary curve.
**          evec    = Vector to end of boundary curve.
**          rad     = Radius of sphere.
**          label   = Label of sphere.
**          sub     = Subscript of sphere.
**       OUTPUT : none
**    RETURNS      :
**          Record number of sphere output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_sphere(pt,svec,evec,rad,label,sub)
UM_coord pt;
UM_vector svec,evec;
UU_REAL rad;
char *label;
int sub;
{
	int irec,recno;
	char tbuf[NCL_MAX_LABEL_AND_SUBSCRIPT],tbuf1[80];
	UU_REAL rnum;
/*
.....Output sphere canonical data
*/
	irec = utp_out_ptaxis(pt,svec,evec);
/*
.....Output sphere command
*/
	utp_format_label(label,sub,tbuf);
	UIO_LEN_INTTOEXT(rad,rnum);
	ul_format_numbers(&rnum,1,STEP_ACY,UU_TRUE,tbuf1);
	sprintf(Sbuf,"SPHERICAL_SURFACE('%s',#%d,%s);",tbuf,irec,tbuf1);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
	return(recno);
}
	
/*********************************************************************
**    E_FUNCTION :  utp_out_cylinder(cyl,label,sub)
**			Output a Cylinder entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          cyl     = Cylinder canonical data (x,y,z,i,j,k,r)
**          label   = Label of cylinder.
**          sub     = Subscript of cylinder.
**       OUTPUT : none
**    RETURNS      :
**          Record number of cylinder output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_cylinder(cyl,label,sub)
UU_REAL cyl[];
char *label;
int sub;
{
	int irec,recno;
	char tbuf[NCL_MAX_LABEL_AND_SUBSCRIPT],tbuf1[80];
	UU_REAL rnum;
	UM_vector svec;
/*
.....Output cylinder canonical data
*/
	um_perpvc(&cyl[3],svec);
	irec = utp_out_ptaxis(&cyl[0],&cyl[3],svec);
/*
.....Output cylinder command
*/
	utp_format_label(label,sub,tbuf);
	UIO_LEN_INTTOEXT(cyl[6],rnum);
	ul_format_numbers(&rnum,1,STEP_ACY,UU_TRUE,tbuf1);
	sprintf(Sbuf,"CYLINDRICAL_SURFACE('%s',#%d,%s);",tbuf,irec,tbuf1);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
	return(recno);
}
	
/*********************************************************************
**    E_FUNCTION :  utp_out_cone(cone,label,sub)
**			Output a Cone entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          cone    = Cone canonical data (x,y,z,i,j,k,angle,hgt,apxdis)
**          label   = Label of cone.
**          sub     = Subscript of cone.
**       OUTPUT : none
**    RETURNS      :
**          Record number of cone output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_cone(cone,label,sub)
UU_REAL cone[];
char *label;
int sub;
{
	int irec,recno;
	UU_REAL rad,rnum[2],hgt;
	char tbuf[NCL_MAX_LABEL_AND_SUBSCRIPT],tbuf1[80];
	UM_coord cpt;
	UM_vector svec;
/*
.....Calculate the bottom and top radii
*/
	hgt = cone[7] + cone[8];
	rad = hgt * tan(cone[6]);
/*
.....Output cone canonical data
*/
	um_translate_point(&cone[0],hgt,&cone[3],cpt);
	um_perpvc(&cone[3],svec);
	irec = utp_out_ptaxis(cpt,&cone[3],svec);
/*
.....Output cone command
*/
	utp_format_label(label,sub,tbuf);
	UIO_LEN_INTTOEXT(rad,rnum[0]);
	rnum[1] = cone[6];
	ul_format_numbers(rnum,2,STEP_ACY,UU_TRUE,tbuf1);
	sprintf(Sbuf,"CONICAL_SURFACE('%s',#%d,%s);",tbuf,irec,tbuf1);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
	return(recno);
}
	
/*********************************************************************
**    E_FUNCTION :  utp_out_torus(center,nvec,svec,rad1,rad2,label,sub)
**			Output a Torus entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          torus   = Torus canonical data (x,y,z,i,j,k,major,minor)
**          label   = Label of torus.
**          sub     = Subscript of torus.
**       OUTPUT : none
**    RETURNS      :
**          Record number of torus output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_torus(torus,label,sub)
UU_REAL *torus;
char *label;
int sub;
{
	int irec,recno;
	UU_REAL rnum[2];
	char tbuf[NCL_MAX_LABEL_AND_SUBSCRIPT],tbuf1[80];
	UM_vector svec;
/*
.....Output torus canonical data
*/
	um_perpvc(&torus[3],svec);
	irec = utp_out_ptaxis(&torus[0],&torus[3],svec);
/*
.....Output torus command
*/
	utp_format_label(label,sub,tbuf);
	UIO_LEN_INTTOEXT(torus[6],rnum[0]);
	UIO_LEN_INTTOEXT(torus[7],rnum[1]);
	ul_format_numbers(rnum,2,STEP_ACY,UU_TRUE,tbuf1);
	sprintf(Sbuf,"TOROIDAL_SURFACE('%s',#%d,%s);",tbuf,irec,tbuf1);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_revsf(surf,label,sub)
**			Output a Surface of Revolution entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          surf    = Surface of revolution.
**          label   = Label of surface.
**          sub     = Subscript of surface.
**       OUTPUT : none
**    RETURNS      :
**          Record number of surface output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_revsf(surf,label,sub)
struct NCL_revsurf_rec *surf;
char *label;
int sub;
{
	int i,irec,jrec,recno,nint,status;
	UM_int2 primtyp;
	UU_REAL primdata[16];
	char tbuf[NCL_MAX_LABEL_AND_SUBSCRIPT];
	UM_coord spt,ept;
	UM_vector svec;
	struct NCL_fixed_databag ent;
/*
.....Get surface primitive
*/
	status = ncl_sf_prim_analyz(&surf->key,&primtyp,primdata);
/*
.....Determine if it is a torus
*/
	if (primtyp == NCLSF_TORUS)
		recno = utp_out_torus(primdata,label,0);
/*
.....Output revolved curve
*/
	else
	{
		ent.key = surf->cvkey;
		status = ncl_retrieve_data_fixed(&ent);
		irec = utp_out_curve(&ent,spt,ept,UU_FALSE);
/*
.....Calculate starting vector
*/
		um_ilnpln(surf->pta,surf->vca,spt,surf->vca,&nint,ept);
		um_vcmnvc(spt,ept,svec); um_unitvc(svec);
/*
.....Output starting vector
*/
		jrec = utp_out_ptaxis(surf->pta,surf->vca,svec);
/*
.....Output surface command
*/
		utp_format_label(label,sub,tbuf);
		sprintf(Sbuf,"SURFACE_OF_REVOLUTION('%s',#%d,#%d);",tbuf,irec,jrec);
		recno = utp_out_record(Sbuf);
	}
/*
.....End of routine
*/
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_nsurf(surf,label,sub)
**			Output a B-spline surface entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          surf    = Nurbs surface.
**          label   = Label of surface.
**          sub     = Subscript of surface.
**       OUTPUT : none
**    RETURNS      :
**          Record number of surface output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_nsurf(surf,label,sub)
struct UM_rbsplsrf_rec *surf;
char *label;
int sub;
{
	int i,j,m,n,recno,irec,*recs,npt,degu,degv,nupts,nvpts,ipt,nwgt,nukts,nvkts;
	int ofl;
	UU_LOGICAL wgtfl;
	UU_REAL *knots,*wgts;
	char tbuf[NCL_MAX_LABEL_AND_SUBSCRIPT],ubuf[20],vbuf[20],fbuf[20];
	UM_coord *pts;
/*
.....Initialize routine
*/
	recno = 0;
	recs = UU_NULL;
	pts = UU_NULL;
	knots = UU_NULL;
	wgts = UU_NULL;
	degu = surf->ku - 1;
	degv = surf->kv - 1;
/*
.....Get Control Points
*/
	npt = surf->no_pt;
	pts = (UM_coord *)uu_malloc(sizeof(UM_coord)*npt);
	if (pts == UU_NULL) goto done;
	ur_retrieve_data_varlist(surf->key,3,pts,1,npt);

	n = surf->no_tu + surf->no_tv; if (surf->no_pt > n) n = surf->no_pt;
	recs = (int *)uu_malloc(sizeof(int)*n);
	if (recs == UU_NULL) goto done;
/*
.....Get weights
*/
	nwgt = surf->no_wt;
	wgtfl = UU_FALSE;
	if (nwgt > 0)
	{
		wgts = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*nwgt);
		if (wgts == UU_NULL) goto done;
		ur_retrieve_data_varlist(surf->key,4,wgts,1,nwgt);
		for  (i=0;i<nwgt;i++)
		{
			if (wgts[i] != 1.)
			{
				wgtfl = UU_TRUE;
				break;
			}
		}
	}
/*
.....Output Control points
.....Step file points are store V and then U
*/
	nupts = degu + surf->nu;
	nvpts = degv + surf->nv;
	m = 0;
	for (i=0;i<nupts;i++)
	{
		for (j=0;j<nvpts;j++)
		{
			ipt = i + j*nupts;
			recs[m++] = utp_out_point_ct(pts[ipt],Sdeflab,0);
		}
	}
/*
	for (i=0;i<npt;i++)
		recs[i] = utp_out_point_ct(pts[i],Sdeflab,0);
*/
/*
.....Setup bspline surface record
*/
	utp_format_label(label,sub,tbuf);
	if (wgtfl)
	{
		sprintf(Sbuf,"(BOUNDED_SURFACE() B_SPLINE_SURFACE(%d,%d,",degu,
			degv);
	}
	else
	{
		sprintf(Sbuf,"B_SPLINE_SURFACE_WITH_KNOTS('%s',%d,%d,",tbuf,degu,degv);
	}
	ofl = 0;
	utp_out_multiple_record(Sbuf,&ofl,&recno);
/*
........Append control point record numbers
*/
	m = 0;
	utp_out_multiple_record("(",&ofl,&recno);
	for (i=0;i<nupts;i++)
	{
		utp_out_multiple_record("(",&ofl,&recno);
		for (j=0;j<nvpts;j++)
		{
			if (j != 0) utp_out_multiple_record(",",&ofl,&recno);
			sprintf(tbuf,"#%d",recs[m++]);
			utp_out_multiple_record(tbuf,&ofl,&recno);
		}
		utp_out_multiple_record(")",&ofl,&recno);
		if (i != nupts-1) utp_out_multiple_record(",",&ofl,&recno);
	}
	utp_out_multiple_record(")",&ofl,&recno);
/*
........Append flags
*/
	utp_get_logical_str(surf->closdinu,ubuf);
	utp_get_logical_str(surf->closdinu,vbuf);
	utp_get_logical_str(UU_FALSE,fbuf);
	sprintf(tbuf,",.UNSPECIFIED.,%s,%s,%s",ubuf,vbuf,fbuf);
	utp_out_multiple_record(tbuf,&ofl,&recno);
/*
........Weights are specified
........Add surface command
*/
	if (wgtfl)
		utp_out_multiple_record(") B_SPLINE_SURFACE_WITH_KNOTS(",&ofl,&recno);
/*
........Add knot values
*/
	nukts = surf->no_tu - 2;
	nvkts = surf->no_tv - 2;
	n = surf->no_tu + surf->no_tv;
	knots = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*n);
	if (knots == UU_NULL) goto done;
	ur_retrieve_data_varlist(surf->key,1,knots,1,surf->no_tu);
	ur_retrieve_data_varlist(surf->key,2,&knots[nukts],1,surf->no_tv);
	npt = nukts + nvkts;
/*
...........Count multiplicities
*/
	n = 1;
	m = 0;
	for (i=1;i<npt;i++)
	{
		if (knots[i] == knots[i-1] && i != nukts)
			n++;
		else
		{
			recs[m++] = n;
			n = 1;
			if (i == nukts) ipt = m;
		}
	}
	recs[m++] = n;
/*
...........Append multiplicities
*/
	if (!wgtfl) utp_out_multiple_record(",",&ofl,&recno);
	utp_out_multiple_record("(",&ofl,&recno);
	for (i=0;i<m;i++)
	{
		if (i == ipt)
			utp_out_multiple_record("),(",&ofl,&recno);
		else if (i != 0)
			utp_out_multiple_record(",",&ofl,&recno);
		sprintf(tbuf,"%d",recs[i]);
		utp_out_multiple_record(tbuf,&ofl,&recno);
	}
	utp_out_multiple_record(")",&ofl,&recno);
/*
...........Append knot values
*/
	n = 0;
	utp_out_multiple_record(",(",&ofl,&recno);
	for (i=0;i<m;i++)
	{
		if (i == ipt)
			utp_out_multiple_record("),(",&ofl,&recno);
		else if (i != 0)
			utp_out_multiple_record(",",&ofl,&recno);
		ul_format_numbers(&knots[n],1,STEP_ACY,UU_TRUE,tbuf);
		utp_out_multiple_record(tbuf,&ofl,&recno);
		n += recs[i];
	}
	utp_out_multiple_record(")",&ofl,&recno);
/*
........Append final flag
*/
	utp_out_multiple_record(",.UNSPECIFIED)",&ofl,&recno);
/*
........Append weights
*/
	if (wgtfl)
	{
		utp_out_multiple_record(
			"GEOMETRIC_REPRESENTATION_ITEM() RATIONAL_B_SPLINE_SURFACE ((",
			&ofl,&recno);
		m = 0;
		for (i=0;i<nupts;i++)
		{
			utp_out_multiple_record("(",&ofl,&recno);
			for (j=0;j<nvpts;j++)
			{
				if (j != 0) utp_out_multiple_record(",",&ofl,&recno);
				ipt = i + j*nupts;
				ul_format_numbers(&wgts[ipt],1,STEP_ACY,UU_TRUE,tbuf);
				utp_out_multiple_record(tbuf,&ofl,&recno);
			}
			utp_out_multiple_record(")",&ofl,&recno);
			if (i != nupts-1) utp_out_multiple_record(",",&ofl,&recno);
		}
		utp_out_multiple_record(")) REPRESENTATION_ITEM('') SURFACE())",
			&ofl,&recno);
	}
/*
.....Output bspline surface record
*/
	ofl = 2;
	utp_out_multiple_record(";",&ofl,&recno);
/*
.....End of routine
*/
done:;
	if (pts != UU_NULL) uu_free(pts);
	if (knots != UU_NULL) uu_free(knots);
	if (recs != UU_NULL) uu_free(recs);
	if (wgts != UU_NULL) uu_free(wgts);
	return(recno);
}

/*********************************************************************
**    I_FUNCTION :  S_xyz_boundary(trimsf)
**			Creates 3-D curves from UV boundary curves in a trimmed surface
**       record if the XYZ curves do not already exist.
**    PARAMETERS   
**       INPUT  : 
**          trimsf  = Trimmed Surface entity.
**       OUTPUT :
**          keys    = Array of keys pointing to the boundary curves.
**          ncvs    = Number of keys in 'keys'.
**          revfl   = 0 = Curve is not reversed, 1 = curve is reversed.
**                    Only set for the outer boundary curve when it is
**                    adjusted.
**    RETURNS      :
**          UU_SUCCESS if all goes well, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_xyz_boundary(trimsf)
struct NCL_trimsf_rec *trimsf;
{
	int i,inc;
	UU_REAL tol;
	UM_transf tfmat;
/*
.....Initialize routine
*/
	tol = .001;
/*
....Check outer boundary curve
*/
	uc_retrieve_transf(trimsf->key,tfmat);
	if (trimsf->cv_key == 0)
	{
		uio_uvcrv_to_crv(trimsf,tfmat,0,tol,&trimsf->cv_key,0);
		ur_update_data_fixed(trimsf);
	}
/*
.....Check inner boundary curves
*/
	inc = 0;
	for (i=0;i<trimsf->no_ibndykey/2;i++)
	{
		if (trimsf->ibndykey[inc] == 0)
		{
			uio_uvcrv_to_crv(trimsf,tfmat,i+1,tol,&trimsf->ibndykey[inc],0);
		}
		inc += 2;
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION :  S_adjust_boundary(trimsf,keys,ncvs)
**			Makes adjustments to the boundary curve for closed surfaces
**       of revolution.
**    PARAMETERS   
**       INPUT  : 
**          trimsf  = Trimmed Surface entity.
**       OUTPUT :
**          keys    = Array of keys pointing to the boundary curves.
**          ncvs    = Number of keys in 'keys'.
**          revfl   = 0 = Curve is not reversed, 1 = curve is reversed.
**                    Only set for the outer boundary curve when it is
**                    adjusted.
**    RETURNS      :
**          UU_TRUE = Adjusted outer boundary curve for full 360 degree
**                    revolved surface.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_adjust_boundary(trimsf,keys,ncvs,revfl)
struct NCL_trimsf_rec *trimsf;
UU_KEY_ID *keys;
int *ncvs,revfl[2];
{
	int inc,status,i,ipt;
	UM_int2 primtyp;
	UU_LOGICAL ifl,flag;
	UU_REAL primdata[16];
	UM_coord spt,ept,cpt[2];
	UM_transf tfmat;
	struct NCL_fixed_databag ent,crv[2];
	struct UM_compcrv_rec *comp;
	struct UM_line_rec *ln1,*ln2;
/*
.....Initialize routine
*/
	keys[0] = trimsf->cv_key;
	revfl[0] = revfl[1] = 0;
	inc = 1;
	ifl = UU_FALSE;
/*
.....Determine if full surface of revolution
.....Designated by a composite curve consisting of the following
.....1. Closed curve.
.....2. Connector for CV1 & CV3.
.....3. Closed curve.
.....4. Connector for CV1 & CV3.
*/
	ncl_get_sf_primdat(&trimsf->bs_key,&primtyp,primdata);
/*
........Revolved surface
........check for closed surface and full boundary curve
*/
	if (primtyp == NCLSF_SPHERE || primtyp == NCLSF_CYLINDER ||
		primtyp == NCLSF_CONE || primtyp == NCLSF_REVOLV)
	{
		ent.key = trimsf->cv_key;
		status = uc_retrieve_data(&ent,sizeof(ent));
		if (status == UU_SUCCESS && ent.rel_num == UM_COMPCRV_REL)
		{
			comp = (struct UM_compcrv_rec *)&ent;
/*
........Should be 4 curves
*/
			if (comp->no_cid >= 4)
			{
/*
........1st and 3rd curves should be closed
*/
				flag = UU_FALSE;
				for (i=0;i<2;i++)
				{
					crv[0].key = comp->cid[i].crvid;
					ncl_retrieve_data_fixed(&crv[0]);
					uc_retrieve_transf(crv[0].key,tfmat);
					um_get_endpts(&crv[0],tfmat,spt,ept);
					if (um_cceqcc(spt,ept))
					{
						um_vctovc(spt,cpt[0]);
						crv[1].key = comp->cid[i+2].crvid;
						ncl_retrieve_data_fixed(&crv[1]);
						uc_retrieve_transf(crv[1].key,tfmat);
						um_get_endpts(&crv[1],tfmat,spt,ept);
						if (um_cceqcc(spt,ept))
						{
							flag = UU_TRUE;
							um_vctovc(spt,cpt[1]);
							ipt = 1 - i;
							break;
						}
					}
				}
/*
........The 2nd and 4th curves
........should be the same curve
*/
				if (flag)
				{
					crv[0].key = comp->cid[ipt].crvid;
					crv[1].key = comp->cid[ipt+2].crvid;
					ipt = 1 - ipt;
					for (i=0;i<2;i++)
					{
						ncl_retrieve_data_fixed(&crv[i]);
						uc_retrieve_transf(crv[i].key,tfmat);
						um_get_endpts(&crv[i],tfmat,spt,ept);
						if ((!um_cceqcc(spt,cpt[0]) && !um_cceqcc(spt,cpt[1])) ||
							(!um_cceqcc(ept,cpt[0]) && !um_cceqcc(ept,cpt[1])))
								flag = UU_FALSE;
					}
				}
/*
.........Closed revolved surface
.........Use only the top and bottom closed curves
*/
				if (flag)
				{
					inc  = 2;
					keys[0] = comp->cid[ipt].crvid;
					keys[1] = comp->cid[ipt+2].crvid;
					revfl[0] = comp->cid[ipt].reverse;
					revfl[1] = comp->cid[ipt+2].reverse;
					ifl = UU_TRUE;
				}
			}
		}
	}

/*
.....Store inner boundary curves
*/
	*ncvs = trimsf->no_ibndykey/2 + inc;
	for (i=0;i<trimsf->no_ibndykey;i=i+2) keys[inc++] = trimsf->ibndykey[i];
/*
.....End of routine
*/
	return(ifl);
}

/*********************************************************************
**    I_FUNCTION :  S_xform_trimsf(trimsf,tfmat)
**			Tranforms the trimmed surface base surface and boundary
**       curves based on the provided transformation matrix.
**    PARAMETERS   
**       INPUT  : 
**          trimsf  = Trimmed Surface entity.
**          tfmat   = Transformation matrix.
**       OUTPUT :
**          trimsf  = Transformed trimmed Surface entity.
**    RETURNS      :
**          UU_FAILURE on failure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_xform_trimsf(trimsf,tfmat)
struct NCL_trimsf_rec *trimsf;
UM_transf tfmat;
{
	int i,nkeys;
	UU_KEY_ID *keys;
	struct UC_entitydatabag e1;
/*
.....Transform the base surface
*/
	e1.key = trimsf->bs_key;
	uc_retrieve_data(&e1);
	uig_transform(&e1,tfmat,UU_TRUE);
/*
.....Transform the outer boundary curve
*/
	e1.key = trimsf->cv_key;
	if (e1.key != 0)
	{
		uc_retrieve_data(&e1);
		uig_transform(&e1,tfmat,UU_TRUE);
	}
/*
.....Transform the inner boundary curves
*/
	if (trimsf->no_ibndykey != 0)
	{
		nkeys = trimsf->no_ibndykey;
		keys = (UU_KEY_ID *)uu_malloc(sizeof(UU_KEY_ID)*nkeys);
		ur_retrieve_data_varlist(trimsf->key,1,keys,1,nkeys);
		for (i=0;i<nkeys;i++)
		{
			e1.key = keys[i];
			uc_retrieve_data(&e1);
			uig_transform(&e1,tfmat,UU_TRUE);
		}
	}
	return(UU_SUCCESS);
}
