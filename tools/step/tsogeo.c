/*********************************************************************
**    NAME         :  tsogeo.c
**       CONTAINS:
**			  utp_out_solid
**			  utp_out_surfaces
**			  utp_out_planes
**			  utp_out_wireframe
**         utp_out_point_vx
**         utp_out_point_ct
**         utp_out_ptaxis
**         utp_out_vector
**         utp_out_direction
**         utp_free_geo_lists
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tsogeo.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			10/27/16 , 15:04:56
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "class.h"
#include "nccs.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mgeom.h"
#include "modef.h"
#include "msol.h"
#include "ncl.h"
#include "nccs.h"
#include "nconst.h"
#include "tiges.h"
#include "tioconv.h"
#include "tstep.h"
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
static int S_output_shell();
static UU_LIST Sgeo_list[3];
static UU_LOGICAL Sgeo_init[3]={UU_FALSE,UU_FALSE,UU_FALSE};

static void S_list_push();

extern UM_int2 NCL_ubas_unit;

struct Sgeo_struc
{
	UM_coord pt;
	int recno;
};

/*********************************************************************
**    E_FUNCTION :  utp_out_solid(solid)
**			Output a Solid entity to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          solid   = Solid entity to output.
**       OUTPUT : none
**    RETURNS      :
**          Record number of solid (CLOSED_SHELL) output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_solid(solid)
struct UM_solid_rec *solid;
{
	int i,recno,nsol,status,*recs,nrec,irec,jrec,mxrec,unrec,brec;
	UU_KEY_ID *keys;
	char tbuf[20],label[NCL_MAX_LABEL_AND_SUBSCRIPT];
	struct NCL_fixed_databag ent;
	struct UC_attributedatabag attr;
/*
.....Initialize routine
*/
	keys = recs = UU_NULL;
	recno = 0;
	nrec = 0;
/*
.....The solid must be a Composite Solid
.....comprised of surfaces
*/
	status = ncl_retrieve_data_fixed(solid);
	if (status != UU_SUCCESS) goto done;
	if (solid->type != UM_COMPOS_SOLID) goto done;
/*
.....Get solid components
*/
	nsol = solid->no_netkey;
	keys = (UU_KEY_ID *)uu_malloc(sizeof(UU_KEY_ID)*nsol);
	if (keys == UU_NULL) goto done;
	recs = (int *)uu_malloc(sizeof(int)*nsol);
	if (recs == UU_NULL) goto done;
	ur_retrieve_data_varlist(solid->key,4,keys,1,nsol);
/*
.....Loop through the surfaces
*/
	for (i=0;i<nsol;i++)
	{
		ent.key = keys[i];
		status = ncl_retrieve_data_fixed(&ent);
		if (status != UU_SUCCESS) continue;
		if (ent.rel_num != NCL_TRIMSF_REL) continue;
/*
........Convert a Trimmed Surface
*/
		irec = utp_out_trimsf(&ent);
		if (irec != 0)
		{
			recs[nrec++] = irec;
/*
........Store attributes
*/
			attr.key = ent.key;
			status = ur_retrieve_attr(&attr);
			attr.rel_num = NCL_SURF_REL;
			if (status == UU_SUCCESS) utp_out_push_attr(irec,&attr);
		}
	}
/*
.....Output SHELL command
*/
	if (nrec != 0)
	{
		utp_format_label(solid->label,solid->subscr,label);
		if (solid->closed)
			recno = S_output_closed_shell(label,&attr,recs,nrec);
		else
			recno = S_output_open_shell(label,&attr,recs,nrec);
		utp_count_translated(UM_SOLID_REL,1,UU_TRUE);
		utp_out_attributes(UU_TRUE);
	}
/*
.....End of routine
*/
done:;
	if (keys != UU_NULL) uu_free(keys);
	if (recs != UU_NULL) uu_free(recs);
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_surfaces(relnum)
**			Output all "loose" surfaces to the STEP file.
**    PARAMETERS   
**       INPUT  :
**          relnum   = Relation number to output; NCL_TRIMSF_REL,
**                     NCL_NETSF_REL, NCL_RBSPLSRF_REL.
**       OUTPUT : none
**    RETURNS      :
**          Record number of geometry output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_surfaces(relnum)
int relnum;
{
	int i,recno,status,*recs,nrec,irec,brec,srec,mxrec,unrec,entnum,nkeys;
	int incsf,netnum,flag;
	UU_KEY_ID *keys;
	char tbuf[20];
	UU_LIST reclst;
	struct NCL_netsf_rec *nsf;
	struct NCL_fixed_databag ent,ent1;
	struct UC_attributedatabag attr;
/*
.....Initialize routine
*/
	nkeys = 1;
	uu_list_init(&reclst,sizeof(int),500,200);
/*
.....Initialize attribute records
*/
	utp_init_attr_recs();
/*
........Loop through NET surfaces
*/
	netnum = 0;
	for (;;)
	{
		if (relnum == NCL_NETSF_REL)
		{
			netnum++;
			ent.rel_num = relnum;
			status = ur_get_next_data_key(ent.rel_num,&netnum,&ent.key);
			if (status != UU_SUCCESS) break;
			status = ncl_retrieve_data_fixed(&ent);
			if (status != UU_SUCCESS) continue;
			if (ent.label[0] == '@' || ent.label[0] == '#') continue;
			nsf = (struct NCL_netsf_rec *)&ent;
			nkeys = nsf->no_netkey;
			if (nkeys <= 0) continue;
			keys = (UU_KEY_ID *)uu_malloc(sizeof(UU_KEY_ID)*nkeys);
			if (keys == UU_NULL) continue;
			ur_retrieve_data_varlist(ent.key,1,keys,1,nkeys);
		}
/*
.....Initialize STEP record stack
*/
		UU_LIST_EMPTY(&reclst);
		nrec = 0;
		recno = 0;
/*
.....Loop through all surfaces
*/
		for (incsf=0;incsf<nkeys;incsf++)
		{
			entnum = 0;
			do
			{
				if (netnum)
				{
					ent.key = keys[incsf];
					status = ncl_retrieve_data_fixed(&ent);
					if (status != UU_SUCCESS) continue;
				}
				else
				{
					entnum++;
					ent.rel_num = relnum;
					status = ur_get_next_data_key(ent.rel_num,&entnum,&ent.key);
					if (status != UU_SUCCESS) break;
					status = ncl_retrieve_data_fixed(&ent);
					if (status != UU_SUCCESS) continue;
					if (ent.label[0] == '@' || ent.label[0] == '#') continue;
				}
/*
........Convert a Base Surface
........to a trimmed surface
*/
				srec = 0;
				if (ent.rel_num == UM_RBSPLSRF_REL ||
					ent.rel_num == NCL_REVSURF_REL)
				{
					uu_move_byte(&ent,&ent1,sizeof(struct UM_rbsplsrf_rec));
					ncl_basesf_to_trimsf(&ent1,&ent);
				}
/*
........Convert a Trimmed Surface
*/
				srec = utp_out_trimsf(&ent);
				if (srec != 0)
				{
/*
........Ouput OPEN_SHELL command
*/
					sprintf(Sbuf,"OPEN_SHELL('%s',(#%d));",Sdeflab,srec);
					irec = utp_out_record(Sbuf);
/*
........Store attributes
*/
					if (irec != 0)
					{
						attr.key = ent.key;
						status = ur_retrieve_attr(&attr);
						attr.rel_num = NCL_SURF_REL;
						if (status == UU_SUCCESS) utp_out_push_attr(srec,&attr);
/*
........Store record number
*/
						uu_list_push(&reclst,&irec);
						nrec++;
					}
				}
				if (netnum) break;
			} while (status == UU_SUCCESS);
		}
/*
.....Output SHELL_BASED_SURFACE_MODEL command
*/
		if (nrec > 0)
		{
			utp_count_translated(NCL_TRIMSF_REL,nrec,UU_TRUE);
			recs = (int *)UU_LIST_ARRAY(&reclst);
			sprintf(Sbuf,"SHELL_BASED_SURFACE_MODEL('%s'",Sdeflab);
			flag = 0;
			utp_out_multiple_record(Sbuf,&flag,&srec);
			for (i=0;i<nrec;i++)
			{
				if (i==0)
					sprintf(tbuf,",(#%d",recs[i]);
				else
					sprintf(tbuf,",#%d",recs[i]);
				utp_out_multiple_record(tbuf,&flag,&srec);
			}
			flag = 2;
			utp_out_multiple_record("));",&flag,&srec);
/*
.....Output MANIFOLD_SURFACE_SHAPE_REPRESENTATION command
*/
			utp_get_identity_rec(&mxrec,&unrec);
			sprintf(Sbuf,"MANIFOLD_SURFACE_SHAPE_REPRESENTATION('%s',(#%d),#%d);",
				Sdeflab,srec,unrec);
			srec = utp_out_record(Sbuf);
/*
.....Output SHAPE_REPRESENTATION record
*/
			sprintf(Sbuf,"SHAPE_REPRESENTATION('%s',(#%d),#%d);",Sdeflab,mxrec,unrec);
			brec = utp_out_record(Sbuf);
/*
.....Output SHAPE_REPRESENTATION_RELATIONSHIP record
*/
			sprintf(Sbuf,"SHAPE_REPRESENTATION_RELATIONSHIP('%s',' ',#%d,#%d);",
				Sdeflab,brec,srec);
			recno = utp_out_record(Sbuf);
/*
.....Output attribute records
*/
			utp_out_push_attr(srec,&attr);
			utp_out_attributes(UU_FALSE);
		}
		if (netnum) uu_free(keys);
		else break;
	}
/*
.....End of routine
*/
	uu_list_free(&reclst);
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_planes()
**			Output all defined planes to the STEP file.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      :
**          Record number of geometry output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_planes()
{
	int i,recno,status,*recs,nrec,irec,brec,mxrec,unrec,entnum;
	char tbuf[20];
	UU_LIST reclst;
	struct NCL_nclpl_rec ent;
/*
.....Initialize routine
*/
	recno = 0;
	nrec = 0;
	uu_list_init(&reclst,sizeof(int),500,200);
/*
.....Loop through all planes
*/
	entnum = 0;
	do
	{
		entnum++;
		ent.rel_num = NCL_PLN_REL;
		status = ur_get_next_data_key(ent.rel_num,&entnum,&ent.key);
		if (status != UU_SUCCESS) break;
		status = ncl_retrieve_data_fixed(&ent);
		if (status != UU_SUCCESS) continue;
		if (ent.label[0] == '@' || ent.label[0] == '#') continue;
/*
........Convert a Plane
*/
		irec = utp_out_plane(ent.pt,ent.nvec,ent.label,ent.subscr);
		if (irec != 0)
		{
			uu_list_push(&reclst,&irec);
			nrec++;
		}
	} while (status == UU_SUCCESS);
/*
.....Output CONSTRUCTIVE_GEOMETRY_REPRESENTATION command
*/
	if (nrec > 0)
	{
		utp_count_translated(NCL_PLN_REL,nrec,UU_TRUE);
		recs = (int *)UU_LIST_ARRAY(&reclst);
		utp_get_identity_rec(&mxrec,&unrec);
		sprintf(Sbuf,"CONSTRUCTIVE_GEOMETRY_REPRESENTATION('%s'",Sdeflab);
		for (i=0;i<nrec;i++)
		{
			if (i==0)
				sprintf(tbuf,",(#%d",recs[i]);
			else
				sprintf(tbuf,",#%d",recs[i]);
			strcat(Sbuf,tbuf);
		}
		sprintf(tbuf,"),#%d);",unrec);
		strcat(Sbuf,tbuf);
		irec = utp_out_record(Sbuf);
/*
.....Output SHAPE_REPRESENTATION record
*/
		sprintf(Sbuf,"SHAPE_REPRESENTATION('%s',(#%d),#%d);",Sdeflab,mxrec,unrec);
		brec = utp_out_record(Sbuf);
/*
.....Output CONSTRUCTIVE_GEOMETRY_REPRESENTATION_RELATIONSHIP record
*/
		sprintf(Sbuf,
			"CONSTRUCTIVE_GEOMETRY_REPRESENTATION_RELATIONSHIP('%s',' ',#%d,#%d);",
			Sdeflab,brec,irec);
		recno = utp_out_record(Sbuf);
	}
/*
.....End of routine
*/
	uu_list_free(&reclst);
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_wireframe(cvfl,ptfl)
**			Output all curves and points to the STEP file.
**    PARAMETERS   
**       INPUT  :
**          cvfl   = UU_TRUE - output wireframe curves, lines, circles, etc.
**          ptfl   = UU_TRUE - output points.
**       OUTPUT : none
**    RETURNS      :
**          Record number of geometry output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_wireframe(cvfl,ptfl)
UU_LOGICAL cvfl,ptfl;
{
	int i,recno,status,*recs,nrec,irec,brec,mxrec,unrec,next_id,key,mtype;
	int flag,rel_num;
	char tbuf[20];
	UU_LIST reclst;
	struct UM_point_rec *pt;
	struct UM_circle_rec *ci;
	struct UM_rbsplcrv_rec *cv;
	struct UM_conic_rec *co;
	struct NCL_fixed_databag ent;
	struct UC_attributedatabag attr;
/*
.....Initialize routine
*/
	recno = 0;
	nrec = 0;
	uu_list_init(&reclst,sizeof(int),500,200);
/*
.....Loop through all geometry
*/
	next_id = 1;
	while (ur_get_next_key(&next_id,&key) > -1)
	{
		next_id++;
		ent.key = key;
		status = ncl_retrieve_data_fixed(&ent);
		if (ent.label[0] == '@' || ent.label[0] == '#' || ent.label[0] == '\0')
			continue;
		mtype = 0;
		switch (ent.rel_num)
		{
/*
........Convert a Point
*/
		case UM_POINT_REL:
			if (ptfl)
			{
				pt = (struct UM_point_rec *)&ent;
				irec = utp_out_point_ct(pt->pt,pt->label,pt->subscr);
				mtype = pt->markertype;
				rel_num = NCL_POINT_REL;
			}
			break;
/*
........Convert a Line
*/
		case UM_LINE_REL:
			if (cvfl)
			{
				irec = utp_out_trimcv(key);
				rel_num = NCL_CURVE_REL;
			}
			break;
/*
........Convert a Circle
*/
		case UM_CIRCLE_REL:
			if (cvfl)
			{
				ci = (struct UM_circle_rec *)&ent;
				if (fabs(ci->dang) == UM_TWOPI)
					irec = utp_out_circle(ci);
				else
					irec = utp_out_trimcv(key);
				rel_num = NCL_CURVE_REL;
			}
			break;
/*
........Convert a Curve
*/
		case UM_RBSPLCRV_REL:
			if (cvfl)
			{
				cv = (struct UM_rbsplcrv_rec *)&ent;
				if (cv->t0 == 0. && cv->t1 == 1.)
					irec = utp_out_bspline(cv,UU_FALSE);
				else
					irec = utp_out_trimcv(key);
				rel_num = NCL_CURVE_REL;
			}
			break;
/*
........Convert a Conic
*/
		case UM_CONIC_REL:
			if (cvfl)
			{
				co = (struct UM_conic_rec *)&ent;
				if (co->t0 == -2. && co->t1 == 2.)
					irec = utp_out_conic(co);
				else
					irec = utp_out_trimcv(key);
				rel_num = NCL_CURVE_REL;
			}
			break;
/*
........Convert a Composite Curve
*/
		case UM_COMPCRV_REL:
			if (cvfl)
			{
				irec = utp_out_compcrv(&ent,UU_FALSE);
				rel_num = NCL_CURVE_REL;
			}
			break;
/*
........Unrecognized entity
*/
		default:
			irec = 0;
			break;
		}
/*
........Push record onto stack
*/
		if (irec != 0)
		{
			uu_list_push(&reclst,&irec);
			nrec++;
			utp_count_translated(ent.rel_num,1,UU_TRUE);
			attr.key = ent.key;
			status = ur_retrieve_attr(&attr);
			attr.pen = mtype;
			attr.rel_num = rel_num;;
			if (status == UU_SUCCESS) utp_out_push_attr(irec,&attr);
		}
	}
/*
.....Output GEOMETRIC_SET command
*/
	if (nrec > 0)
	{
		recs = (int *)UU_LIST_ARRAY(&reclst);
		utp_get_identity_rec(&mxrec,&unrec);
		sprintf(Sbuf,"GEOMETRIC_SET('%s'",Sdeflab);
		flag = 0;
		utp_out_multiple_record(Sbuf,&flag,&irec);
		for (i=0;i<nrec;i++)
		{
			if (i==0)
				sprintf(tbuf,",(#%d",recs[i]);
			else
				sprintf(tbuf,",#%d",recs[i]);
			utp_out_multiple_record(tbuf,&flag,&irec);
		}
		flag = 2;
		utp_out_multiple_record("));",&flag,&irec);
		utp_out_push_attr(irec,&attr);
/*
.....Output GEOMETRICALLY_BOUNDED_SURFACE_SHAPE_REPRESENTATION record
*/
		sprintf(Sbuf,
			"GEOMETRICALLY_BOUNDED_SURFACE_SHAPE_REPRESENTATION('%s',(#%d),#%d);",
				Sdeflab,irec,unrec);
		irec = utp_out_record(Sbuf);
/*
.....Output SHAPE_REPRESENTATION record
*/
		sprintf(Sbuf,"SHAPE_REPRESENTATION('%s',(#%d),#%d);",Sdeflab,mxrec,unrec);
		brec = utp_out_record(Sbuf);
/*
.....Output SHAPE_REPRESENTATION_RELATIONSHIP record
*/
		sprintf(Sbuf, "SHAPE_REPRESENTATION_RELATIONSHIP('%s',' ',#%d,#%d);",
			Sdeflab,brec,irec);
		recno = utp_out_record(Sbuf);
/*
.....Output attribute records
*/
		utp_out_attributes(UU_FALSE);
	}
/*
.....End of routine
*/
	uu_list_free(&reclst);
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_point_vx(point,label,sub)
**			Output a Vertex point to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          point   = Point entity to output.
**          label   = Label of point.
**          sub     = Subscript of point.
**       OUTPUT : none
**    RETURNS      : 
**          Record number of point (VERTEX_POINT) output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_point_vx(point,label,sub)
UM_coord point;
char *label;
int sub;
{
	int recno,irec;
	char tbuf[NCL_MAX_LABEL_AND_SUBSCRIPT];
/*
.....Initialize routine
*/
	recno = 0;
/*
.....Output Cartesian point record
*/
	irec = utp_out_point_ct(point,Sdeflab,0);
	utp_format_label(label,sub,tbuf);
	sprintf(Sbuf,"VERTEX_POINT('%s',#%d);",tbuf,irec);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_point_ct(point,label,sub)
**			Output a Cartesian point to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          point   = Point entity to output.
**          label   = Label of point.
**          sub     = Subscript of point.
**       OUTPUT : none
**    RETURNS      : 
**          Record number of point (CARTESIAN_POINT) output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_point_ct(point,label,sub)
UM_coord point;
char *label;
int sub;
{
	int recno;
	char tbuf[NCL_MAX_LABEL_AND_SUBSCRIPT],lbuf[80];
	UM_coord pt;
/*
.....Initialize routine
*/
	recno = 0;
/*
.....Determine if point record already exists
*/
	recno = S_list_compare(0,point);
	if (recno != 0) goto done;
/*
.....Output point record
*/
	UIO_CC_INTTOEXT(point,pt);
	utp_format_label(label,sub,tbuf);
	ul_format_numbers(pt,3,STEP_ACY,UU_TRUE,lbuf);
	sprintf(Sbuf,"CARTESIAN_POINT('%s',(%s));",tbuf,lbuf);
	recno = utp_out_record(Sbuf);
/*
.....Save point record
*/
	S_list_push(0,recno,point);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_ptaxis(point,nvec,svec)
**			Output a Point-vector-vector type coordinate system to the
**       STEP file.
**    PARAMETERS   
**       INPUT  : 
**          point   = Point to output.
**          nvec    = Normal vector to output.
**          svec    = Starting vector to output.
**       OUTPUT : none
**    RETURNS      : 
**          Record number of system (AXIS2_PLACEMENT_3D) output to
**          STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_ptaxis(point,nvec,svec)
UM_coord point;
UM_vector nvec,svec;
{
	int recno,irec[3];
/*
.....Initialize routine
*/
	recno = 0;
/*
.....Output point-vector-vector records
*/
	irec[0] = utp_out_point_ct(point,Sdeflab,0);
	irec[1] = utp_out_direction(nvec,Sdeflab,0);
	irec[2] = utp_out_direction(svec,Sdeflab,0);
/*
.....Output AXIS2_PLACEMENT_3D command
*/
	sprintf(Sbuf,"AXIS2_PLACEMENT_3D('%s',#%d,#%d,#%d);",Sdeflab,irec[0],irec[1],
		irec[2]);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_vector(vector,label,sub)
**			Output a Vector to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          vector  = Vector entity to output.
**          label   = Label of vector.
**          sub     = Subscript of vector.
**       OUTPUT : none
**    RETURNS      : 
**          Record number of vector (VECTOR) output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_vector(vector,label,sub)
UM_vector vector;
char *label;
int sub;
{
	int recno,irec;
	char tbuf[80];
	UU_REAL d;
	UM_coord pt;
	UM_vector vec;
/*
.....Initialize routine
*/
	recno = 0;
/*
.....Calculate magnitude of vector
.....and direction
*/
	d = um_mag(vector);
	um_unitvc(vector,vec);
/*
.....Output direction record
*/
	irec = utp_out_direction(vec,Sdeflab,0);
/*
.....Determine if vector record already exists
*/
	pt[0] = d; pt[1] = irec; pt[0] = 0;
	recno = S_list_compare(1,pt);
	if (recno != 0) goto done;
/*
.....Output vector record
*/
	UIO_LEN_INTTOEXT(d,d);
	ul_format_numbers(&d,1,STEP_ACY,UU_TRUE,tbuf);
	sprintf(Sbuf,"VECTOR('%s',#%d,%s);",label,irec,tbuf);
	recno = utp_out_record(Sbuf);
/*
.....Save vector record
*/
	S_list_push(1,recno,pt);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_direction(vector,label,sub)
**			Output a direction to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          vector  = Direction vector to output.
**          label   = Label of vector.
**          sub     = Subscript of vector.
**       OUTPUT : none
**    RETURNS      : 
**          Record number of direction (DIRECTION) output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_direction(vector,label,sub)
UM_vector vector;
char *label;
int sub;
{
	int recno;
	char tbuf[80];
/*
.....Initialize routine
*/
	recno = 0;
/*
.....Determine if direction record already exists
*/
	recno = S_list_compare(2,vector);
	if (recno != 0) goto done;
/*
.....Output vector record
*/
	ul_format_numbers(vector,3,STEP_ACY,UU_TRUE,tbuf);
	sprintf(Sbuf,"DIRECTION('%s',(%s));",label,tbuf);
	recno = utp_out_record(Sbuf);
/*
.....Save direction record
*/
	S_list_push(2,recno,vector);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    E_FUNCTION :  utp_free_geo_lists()
**			Frees the memory allocated by geometry storage lists.
**    PARAMETERS   
**       INPUT  : 
**          list    = Which list to save the point to.
**          pt      = Point/vector to save.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_free_geo_lists(list,pt)
int list;
UM_coord pt;
{
	int i,recno;
	struct Sgeo_struc geo;
/*
.....Initialize list
*/
	for (i=0;i<3;i++)
	{
		if (Sgeo_init[i])
		{
			uu_list_free(&Sgeo_list[i]);
			Sgeo_init[i] = UU_FALSE;
		}
	}
}

/*********************************************************************
**    I_FUNCTION :  S_output_closed_shell(label,attr,recs,nrec)
**			Output a closed solid to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          label   = Label of output solid.
**          attr    = Attribute record to output with shell.
**          recs    = Record numbers of surfaces output.
**          nrec    = Number of entries in 'recs'.
**       OUTPUT : none
**    RETURNS      : none
**          Record number of solid (SHAPE_DEFINITION_REPRESENTATION)
**          output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_output_closed_shell(label,attr,recs,nrec)
char *label;
struct UC_attributedatabag *attr;
int *recs,nrec;
{
	int i,recno,irec,mxrec,unrec,brec,jrec;
	char tbuf[20];
/*
.....Output CLOSED_SHELL command
*/
	sprintf(Sbuf,"CLOSED_SHELL('%s'",Sdeflab);
	for (i=0;i<nrec;i++)
	{
		if (i==0)
			sprintf(tbuf,",(#%d",recs[i]);
		else
			sprintf(tbuf,",#%d",recs[i]);
		strcat(Sbuf,tbuf);
	}
	strcat(Sbuf,"));");
	irec = utp_out_record(Sbuf);
/*
.....Output MANIFOLD_SOLID_BREP command
*/
	sprintf(Sbuf,"MANIFOLD_SOLID_BREP('%s',#%d);",label,irec);
	irec = utp_out_record(Sbuf);
	utp_out_push_attr(irec,attr);
/*
.....Output ADVANCED_BREP_SHAPE_REPRESENTATION record
*/
	utp_get_identity_rec(&mxrec,&unrec);
	sprintf(Sbuf,"ADVANCED_BREP_SHAPE_REPRESENTATION('%s',(#%d,#%d),#%d);",
		label,irec,mxrec,unrec);
	brec = utp_out_record(Sbuf);
/*
.....Output DESIGN_CONTEXT record
*/
	if (UTP_step_214)
		strcpy(Sbuf,"APPLICATION_CONTEXT('automotive_design');");
	else
		strcpy(Sbuf,"APPLICATION_CONTEXT('configuration controlled 3d designs of mechanical parts and assemblies');");
	irec = utp_out_record(Sbuf);

	sprintf(Sbuf,"DESIGN_CONTEXT('detailed design',#%d,'design');",irec);
	jrec = utp_out_record(Sbuf);
/*
.....Output PRODUCT_DEFINTION_FORMAT--- record
*/
	sprintf(Sbuf,"MECHANICAL_CONTEXT('%s',#%d,'mechanical');",Sdeflab,irec);
	irec = utp_out_record(Sbuf);
		
	sprintf(Sbuf,"PRODUCT('%s','%s','',(#%d));",iges_igsfile,iges_igsfile,
		irec);
	irec = utp_out_record(Sbuf);
		
	sprintf(Sbuf,"PRODUCT_DEFINITION_FORMATION_WITH_SPECIFIED_SOURCE('ANY','',#%d,.NOT_KNOWN.);",irec);
	irec = utp_out_record(Sbuf);
/*
.....Output PRODUCT_DEFINITION_SHAPE record
*/
	sprintf(Sbuf,"PRODUCT_DEFINITION('UNKNOWN','',#%d,#%d);",irec,jrec);
	irec = utp_out_record(Sbuf);
		
	sprintf(Sbuf,"PRODUCT_DEFINITION_SHAPE('NONE','NONE',#%d);",irec);
	irec = utp_out_record(Sbuf);
/*
.....Output SHAPE_DEFINITION_REPRESENTATION record
*/
	sprintf(Sbuf,"SHAPE_DEFINITION_REPRESENTATION(#%d,#%d);",irec,brec);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    I_FUNCTION :  S_output_open_shell(label,attr,recs,nrec)
**			Output an open solid to the STEP file.
**    PARAMETERS   
**       INPUT  : 
**          label   = Label of output solid.
**          attr    = Attribute record to output with shell.
**          recs    = Record numbers of surfaces output.
**          nrec    = Number of entries in 'recs'.
**       OUTPUT : none
**    RETURNS      : none
**          Record number of solid (SHAPE_REPRESENTATION_RELATIONSHIP)
**          output to STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_output_open_shell(label,attr,recs,nrec)
char *label;
struct UC_attributedatabag *attr;
int *recs,nrec;
{
	int i,recno,irec,mxrec,unrec,brec;
	char tbuf[20];
/*
.....Output OPEN_SHELL command
*/
	sprintf(Sbuf,"OPEN_SHELL('%s'",label);
	for (i=0;i<nrec;i++)
	{
		if (i==0)
			sprintf(tbuf,",(#%d",recs[i]);
		else
			sprintf(tbuf,",#%d",recs[i]);
		strcat(Sbuf,tbuf);
	}
	strcat(Sbuf,"));");
	irec = utp_out_record(Sbuf);
/*
.....Output SHELL_BASED_SURFACE_MODEL command
*/
	sprintf(Sbuf,"SHELL_BASED_SURFACE_MODEL('%s',(#%d));",Sdeflab,irec);
	irec = utp_out_record(Sbuf);
	utp_out_push_attr(irec,attr);
/*
.....Output MANIFOLD_SURFACE_SHAPE_REPRESENTATION record
*/
	utp_get_identity_rec(&mxrec,&unrec);
	sprintf(Sbuf,"MANIFOLD_SURFACE_SHAPE_REPRESENTATION('%s',(#%d),#%d);",
		Sdeflab,irec,unrec);
	brec = utp_out_record(Sbuf);
/*
.....Output SHAPE_REPRESENTATION record
*/
	sprintf(Sbuf,"SHAPE_REPRESENTATION('%s',(#%d),#%d);",
		Sdeflab,mxrec,unrec);
	irec = utp_out_record(Sbuf);
/*
.....Output SHAPE_REPRESENTATION_RELATIONSHIP record
*/
	sprintf(Sbuf,"SHAPE_REPRESENTATION_RELATIONSHIP('%s',' ',#%d,#%d);",Sdeflab,
		irec,brec);
	recno = utp_out_record(Sbuf);
/*
.....End of routine
*/
done:;
	return(recno);
}

/*********************************************************************
**    I_FUNCTION :  S_list_compare(list,pt)
**			Compares an input point/vector to the list of defined entities
**       to see if there is a match.
**    PARAMETERS   
**       INPUT  : 
**          list    = Which list to search.
**          pt      = Point/vector to compare.
**       OUTPUT : none
**    RETURNS      :
**          Record number of existing point/vector or 0 if this point
**          has not been defined yet.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_list_compare(list,pt)
int list;
UM_coord pt;
{
	int i,recno;
	struct Sgeo_struc *geo;
/*
.....Initialize routine
*/
	recno = 0;
/*
........Point matching is too slow on large models
........Either needs to be optimized or just left out
........Test with QAR 101048
*/
	if (list == 0) goto done;
/*
.....Determine if point record already exists
*/
	if (Sgeo_init[list])
	{
		geo = (struct Sgeo_struc *)UU_LIST_ARRAY(&Sgeo_list[list]);
		for (i=0;i<UU_LIST_LENGTH(&Sgeo_list[list]);i++)
		{
			if (um_cceqcc_tol(pt,geo[i].pt,UM_DFUZZ))
			{
				recno = geo[i].recno;
				break;
			}
		}
	}
done:;
	return(recno);
}

/*********************************************************************
**    I_FUNCTION :  S_list_push(list,pt)
**			Pushes a point onto the saved geometry list.
**    PARAMETERS   
**       INPUT  : 
**          list    = Which list to save the point to.
**          pt      = Point/vector to save.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_list_push(list,recno,pt)
int list,recno;
UM_coord pt;
{
	int i;
	struct Sgeo_struc geo;
/*
.....Initialize list
*/
	if (!Sgeo_init[list])
	{
		uu_list_init(&Sgeo_list[list],sizeof(geo),5000,5000);
		Sgeo_init[list] = UU_TRUE;
	}
/*
.....Save point to list
*/
	geo.recno = recno;
	um_vctovc(pt,geo.pt);
	uu_list_push(&Sgeo_list[list],&geo);
}
