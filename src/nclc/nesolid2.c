/*********************************************************************
**    NAME         :  nesolid2.c
**       CONTAINS:
**             ncl_transform_solid()
**             ncl_solid_mcstowcs()
**             ncl_solid_wcstomcs()
**             ncl_copy_solid()
**             ncl_solid_d_endpts()
**             ncl_solid_to_coord()
**             ncl_solid_to_vector()
**             ncl_print_solid()
**             ncl_solid_query()
**             ncl_solid_proj_to_drawing()
**             ncl_solid_count_components()
**             ncl_solid_near_point()
**    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nesolid2.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       08/17/15 , 17:54:09
*********************************************************************/

#include "usysdef.h"
#include "lipv.h"
#include "umath.h"
#include "ulist.h"
#include "uhep.h"
#include "udebug.h"
#include "umoveb.h"
#include "class.h"
#include "mattr.h"
#include "mdcoord.h"
#include "mdebug.h"
#include "mdpick.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include	"mfort.h"
#include	"mdrel.h"
#include	"mdcpln.h"
#include	"mgeom.h"
#include	"nccs.h"
#include	"ncl.h"

static char *stype[]={"Box", "Cylinder", "Torus", "Sphere", "Cone",
	"Extruded", "Contour", "Revolved", "STL", "Composite"};

void ncl_solid_count_components();

/*********************************************************************
**    E_FUNCTION :  ncl_transform_solid(solid,tfmat,store)
**       Transforms a Visual Solid.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to transform.
**          tf       Transformation matrix.
**          store    UU_TRUE = transformed entity is to be stored in
**                   Unibase.
**       OUTPUT :  
**          solid    Transformed solid.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_transform_solid(solid,tf,store)
struct UM_solid_rec *solid;
UM_transf tf;
UU_LOGICAL store;
{
	int status;
	int i,np,nc;
	UU_REAL *p,*parms;
	UU_KEY_ID *keys;
	UM_coord *pts;
	UU_LIST dlist;
	UM_tessellation tess;
/*
.....Initialize routine
*/
	if (um_is_idmat(tf)) return(UU_SUCCESS);
	uu_list_init0(&dlist);
	tess.np = 0;
/*
.....Retrieve the solid parameters
*/
	if (solid->type == UM_COMPOS_SOLID)
	{
		np = solid->no_netkey;
		keys = (UU_KEY_ID *)uu_malloc(sizeof(UU_KEY_ID)*(np+1));
		if (keys == UU_NULL) goto failed;
		status = ur_retrieve_data_varlist(solid->key,4,&keys[1],1,np);
		keys[0] = np;
		parms = (UU_REAL *)keys;
	}
	else
	{
		np = solid->no_sdata;
		parms = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*np);
		if (parms == UU_NULL) goto failed;
		status = ur_retrieve_data_varlist(solid->key,1,parms,1,np);
	}
	if (status != UU_SUCCESS) goto failed;
/*
.....Transform the solid
*/
	status = ncl_xform_solid(solid->type,&parms,tf,store);
	if (status != UU_SUCCESS) goto failed;
/*
.....Update the variable lists
*/
	if (store)
	{
		status = ur_update_data_fixed(solid);
		if (status != UU_SUCCESS) goto failed;
/*
........Calculate the display lists
*/
		status = ncl_solid_calc_lists(solid,parms,np,&dlist,&tess);
		if (status != UU_SUCCESS) goto failed;
		p = (UU_REAL *)UU_LIST_ARRAY(&dlist);
		nc = UU_LIST_LENGTH(&dlist);
		status = ur_update_data_varlist(solid->key,2,p,1,nc);
		if (status != UU_SUCCESS) goto failed;
		status = ncl_store_surflist1(TESSELLATION_LIST,solid,&tess);
		if (status != UU_SUCCESS) goto failed;
		status = ur_update_data_varlist(solid->key,1,parms,1,np);
		if (status != UU_SUCCESS) goto failed;
	}
/*
.....Free stored measurement triangle list
*/
	ncl_free_single_trilist(solid->key);
	goto done;
/*
.....Could not transform solid
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (parms != UU_NULL) uu_free(parms);
	if (!UU_LIST_NULLPTR(&dlist)) uu_list_free(&dlist);
	if (tess.np != 0) um_free_tess(&tess);
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_solid_mcstowcs(type,parms,flag)
**       Transforms Visual Solid/NCLIPV stock data to the current MODSYS.
**    PARAMETERS   
**       INPUT  : 
**          type     Type of solid/stock to transforam.
**          parms    Solid/stock canonical data.
**          flag     UU_TRUE = Transform curve points.
**                   UU_FALSE = Curve points are already transformed.
**       OUTPUT :  
**          parms    Updated parameters.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_solid_mcstowcs(type,parms,flag)
int type;
UU_REAL *parms;
UU_LOGICAL flag;
{
	int i,np;
	UM_coord *pts;
	UM_vector *vcs;
/*
.....Initialize routine
*/
	switch (type)
	{
/*
.....Box solid
*/
	case UM_BOX_SOLID:
		ncl_mcstowcs(0,&parms[0],&parms[0]);
		ncl_mcstowcs(1,&parms[3],&parms[3]);
		ncl_mcstowcs(1,&parms[6],&parms[6]);
		ncl_mcstowcs(1,&parms[9],&parms[9]);
		np = 0;
		break;
/*
.....Extruded solid
*/
	case UM_EXTRUDED_SOLID:
		np = parms[3];
		pts = (UM_coord *)&parms[4];
		ncl_mcstowcs(1,&parms[0],&parms[0]);
		break;
/*
.....Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
	case UM_TORUS_SOLID:
		ncl_mcstowcs(0,&parms[0],&parms[0]);
		ncl_mcstowcs(1,&parms[3],&parms[3]);
		np = 0;
		break;
/*
.....Sphere solid
*/
	case UM_SPHERE_SOLID:
		ncl_mcstowcs(0,&parms[0],&parms[0]);
		np = 0;
		break;
/*
.....Revolved solid
*/
	case UM_REVOLVED_SOLID:
		np = parms[8];
		pts = (UM_coord *)&parms[9];
		ncl_mcstowcs(0,&parms[0],&parms[0]);
		ncl_mcstowcs(1,&parms[3],&parms[3]);
		if (flag)
		{
			vcs = (UM_vector *)&parms[9+np*3];
			for (i=0;i<np;i++) ncl_mcstowcs(0,vcs[i],vcs[i]);
		}
		break;
/*
.....Contour solid
*/
	case UM_CONTOUR_SOLID:
		np = parms[5];
		pts = (UM_coord *)&parms[6];
		ncl_mcstowcs(1,&parms[2],&parms[2]);
		break;
/*
.....Unknown stock
.....Do not transform
*/
	default:
		np = 0;
	}
/*
......Transform points
*/
	if (flag)
	{
		for (i=0;i<np;i++) ncl_mcstowcs(0,pts[i],pts[i]);
	}
}

/*********************************************************************
**    E_FUNCTION :  ncl_solid_wcstomcs(type,parms,flag)
**       Transforms Visual Solid/NCLIPV stock data from the current MODSYS.
**    PARAMETERS   
**       INPUT  : 
**          type     Type of solid/stock to transforam.
**          parms    Solid/stock canonical data.
**          flag     UU_TRUE = Transform curve points.
**                   UU_FALSE = Curve points are already transformed.
**       OUTPUT :  
**          parms    Updated parameters.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_solid_wcstomcs(type,parms,flag)
int type;
UU_REAL *parms;
UU_LOGICAL flag;
{
	int i,np;
	UM_coord *pts;
	UM_vector *vcs;
/*
.....Initialize routine
*/
	switch (type)
	{
/*
.....Box solid
*/
	case UM_BOX_SOLID:
		ncl_wcstomcs(0,&parms[0],&parms[0]);
		ncl_wcstomcs(1,&parms[3],&parms[3]);
		ncl_wcstomcs(1,&parms[6],&parms[6]);
		ncl_wcstomcs(1,&parms[9],&parms[9]);
		np = 0;
		break;
/*
.....Extruded solid
*/
	case UM_EXTRUDED_SOLID:
		np = parms[3];
		pts = (UM_coord *)&parms[4];
		ncl_wcstomcs(1,&parms[0],&parms[0]);
		break;
/*
.....Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
	case UM_TORUS_SOLID:
		ncl_wcstomcs(0,&parms[0],&parms[0]);
		ncl_wcstomcs(1,&parms[3],&parms[3]);
		np = 0;
		break;
/*
.....Sphere solid
*/
	case UM_SPHERE_SOLID:
		ncl_wcstomcs(0,&parms[0],&parms[0]);
		np = 0;
		break;
/*
.....Revolved solid
*/
	case UM_REVOLVED_SOLID:
		np = parms[8];
		pts = (UM_coord *)&parms[9];
		ncl_wcstomcs(0,&parms[0],&parms[0]);
		ncl_wcstomcs(1,&parms[3],&parms[3]);
		if (flag)
		{
			vcs = (UM_vector *)&parms[9+np*3];
			for (i=0;i<np;i++) ncl_wcstomcs(0,vcs[i],vcs[i]);
		}
		break;
/*
.....Contour solid
*/
	case UM_CONTOUR_SOLID:
		np = parms[5];
		pts = (UM_coord *)&parms[6];
		ncl_wcstomcs(1,&parms[2],&parms[2]);
		break;
/*
.....Unknown stock
.....Do not transform
*/
	default:
		np = 0;
	}
/*
......Transform points
*/
	if (flag)
	{
		for (i=0;i<np;i++) ncl_wcstomcs(0,pts[i],pts[i]);
	}
}

/*********************************************************************
**    E_FUNCTION :  ncl_copy_solid(e1,e2,esize)
**       Copies a Visual Solid.
**    PARAMETERS   
**       INPUT  : 
**          e1       Solid to copy.
**          esize    Size of solid record.
**       OUTPUT :  
**          e2       Copied solid.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_copy_solid(e1,e2,esize)
struct UM_solid_rec *e1,*e2;
int esize;
{
	int i,status,np[3];
	UM_int2 nclstatus;
	UU_REAL *parms,*p[3],opts[3];
	UU_KEY_ID *keys;
	struct UM_surfattr_rec attr;
/*
.....Initialize routine
*/
	parms = UU_NULL;
	keys = UU_NULL;
/*
.....Retrieve the specified solid
*/
	e2->key = e1->key;
	status = ncl_retrieve_data_fixed(e2);
	if (status != UU_SUCCESS) goto done;
	e2->key = 0;
	e2->no_sdata = 0;
	e2->no_displst = 0;
	e2->no_tesslst = 0;
	e2->sdata = UU_NULL;
	e2->displst = UU_NULL;
	e2->tesslst = UU_NULL;
/*
.....Label the new entity
*/
	status = ncl_label_wf(UM_SOLID_REL,e2->label,&e2->subscr,0,&nclstatus);
	if (status != UU_SUCCESS) goto done;
/*
.....Create the new solid
*/
	uc_create_data(e2,UM_DEFAULT_TF,&attr);
/*
.....Copy the variable lists
*/
	np[0] = e1->no_sdata;
	np[1] = e1->no_displst;
	np[2] = e1->no_tesslst;
	p[0] = e1->sdata;
	p[1] = e1->displst;
	p[2] = e1->tesslst;
	for (i=1;i<=3;i++)
	{
		status = ur_update_data_varlist(e2->key,i,p[i-1],1,np[i-1]);
		if (status != UU_SUCCESS) goto failed;
	}
/*
.....Copy components of a Composite Solid
*/
	if (e1->no_netkey != 0)
	{
		for (i=0;i<e1->no_netkey;i++)
			ncl_add_listkey1(0,e1->netkey[i]);
		opts[1] = 1; opts[2] = e1->no_netkey;
		status = ncl_solid_store_components(e2,opts);
		if (status != UU_SUCCESS) goto failed;
	}
/*
.....Copy the attributes
*/
	attr.key = e1->key;
	ur_retrieve_attr(&attr);
	attr.key = e2->key;
	ur_update_attr(&attr);
/*
.....Get the copied solid
*/
	status = ncl_retrieve_data(e2,sizeof(struct UM_solid_rec));
	if (status != UU_SUCCESS) goto failed;
/*
.....Store the new entity's label
*/
	status = ncl_store_wf1(e2->key);
	goto done;
/*
.....Failed to copy entity
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (parms != UU_NULL) uu_free(parms);
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_solid_d_endpts(level,path,loc,relnum,cpt,opt)
**       Calculates the end points of a picked Visual Solid.
**    PARAMETERS   
**       INPUT  : 
**          level    Level of entity picked.
**          path     Pick path.
**          loc      Pick location.
**       OUTPUT :  
**          relnum   Relation number of entity picked.
**          cpt      Closest end point to pick.
**          opt      Other end point.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_solid_d_endpts(level,path,loc,relnum,cpt,opt)
int level;
UD_PPICKREC *path;
UD_NDCLOCREC *loc;
int *relnum;
UM_coord cpt,opt;
{
	int status,np,i,closest,ilev;
	UU_REAL *ptr,d,dis;
	UM_coord pts[8],*ptx;
	UM_vector svec,nvec;
	UM_transf tf;
	UM_PICKENT pent;
	struct UM_solid_rec solid;
/*
.....Get the entity picked
*/
	ilev = 1;
	status = um_d_pickresolve(path,ilev,&pent);
	if (status != UU_SUCCESS) goto done;
	solid.key = um_get_pickkey(&pent,ilev);
	status = uc_retrieve_data(&solid,sizeof(solid));
	if (status != UU_SUCCESS) goto done;
/*
.....Store the points to use for nearest point
*/
	switch (solid.type)
	{
/*
........Box solid
*/
	case UM_BOX_SOLID:
		ptx = (UM_coord *)&solid.sdata[0];
		um_vctovc(ptx[0],pts[0]);
		um_vcplvc(pts[0],ptx[1],pts[1]);
		um_vcplvc(pts[1],ptx[2],pts[2]);
		um_vcmnvc(pts[2],ptx[1],pts[3]);
		um_vcplvc(pts[0],ptx[3],pts[4]);
		um_vcplvc(pts[4],ptx[1],pts[1]);
		um_vcplvc(pts[5],ptx[2],pts[2]);
		um_vcmnvc(pts[6],ptx[1],pts[3]);
		np = 8;
		break;
/*
........Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		um_vctovc(&ptr[0],pts[0]);
		um_translate_point(&ptr[0],1.,&ptr[3],pts[1]);
		np = 2;
		break;
/*
........Sphere solid
*/
	case UM_SPHERE_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		svec[0] = 0.; svec[1] = 0.; svec[2] = 1.;
		um_translate_point(&ptr[0],ptr[3],svec,pts[0]);
		um_translate_point(&ptr[0],-ptr[3],svec,pts[1]);
		np = 2;
		break;
/*
........Torus solid
*/
	case UM_TORUS_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		um_unitvc(&ptr[3],nvec);
		um_perpvc(nvec,svec);
		um_translate_point(&ptr[0],ptr[6]+ptr[7],svec,pts[0]);
		um_translate_point(&ptr[0],ptr[6]-ptr[7],svec,pts[1]);
		np = 2;
		break;
/*
........Extruded Solid
*/
	case UM_EXTRUDED_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		ptx = (UM_coord *)&solid.sdata[4];
		um_vctovc(ptx[0],pts[0]);
		um_vctovc(&ptr[0],nvec);
		um_translate_point(ptx[0],1.,nvec,pts[1]);
		np = 2;
		break;
/*
........Contour Solid
*/
	case UM_CONTOUR_SOLID:
		ptr = (UU_REAL *)&solid.sdata[2];
		ptx = (UM_coord *)&solid.sdata[6];
		um_vctovc(ptx[0],pts[0]);
		um_vctovc(&ptr[0],nvec);
		um_translate_point(ptx[0],1.,nvec,pts[1]);
		np = 2;
		break;
/*
........Revolved Solid
*/
	case UM_REVOLVED_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		ptx = (UM_coord *)&solid.sdata[9];
		np = ptr[8];
		um_vctovc(ptx[0],pts[0]);
		um_vctovc(ptx[np-1],pts[1]);
		np = 2;
		break;
/*
........Unsupported solid
*/
	default:
		status = UU_FAILURE;
		goto done;
	}
/*
.....Find closest point
*/
	closest = um_nearest_to_ploc(loc,np,pts);
	um_vctovc(pts[closest],cpt);
	if (np == 2) um_vctovc(pts[1-closest],opt);
	else
	{
		dis = 0.;
		for (i=0;i<np;i++)
		{
			d = um_dcccc(pts[i],pts[closest]);
			if (d > dis)
			{
				dis = d;
				um_vctovc(pts[i],opt);
			}
		}
	}
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_solid_to_coord(level,path,loc,cpt)
**       Determines a near point from a picked Visual Solid.
**    PARAMETERS   
**       INPUT  : 
**          level    Level of entity picked.
**          path     Pick path.
**          loc      Pick location.
**       OUTPUT :  
**          cpt      Closest end point to pick.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_solid_to_coord(level,path,loc,cpt)
int level;
UD_PPICKREC *path;
UD_NDCLOCREC *loc;
UM_coord cpt;
{
	int relnum,status;
	UM_coord opt;
/*
.....Get closest point to pick location
*/
	status = ncl_solid_d_endpts(level,path,loc,&relnum,cpt,opt);
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_solid_to_vector(level,path,loc,vec)
**       Determines a vector from a picked Visual Solid.
**    PARAMETERS   
**       INPUT  : 
**          level    Level of entity picked.
**          path     Pick path.
**          loc      Pick location.
**       OUTPUT :  
**          vec      Vector obtained from solid.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_solid_to_vector(level,path,loc,vec)
int level;
UD_PPICKREC *path;
UD_NDCLOCREC *loc;
UM_vector vec;
{
	int status,ilev;
	UM_PICKENT pent;
	struct UM_solid_rec solid;
/*
.....Get the entity picked
*/
	ilev = 1;
	status = um_d_pickresolve(path,ilev,&pent);
	if (status != UU_SUCCESS) goto done;
	solid.key = um_get_pickkey(&pent,ilev);
	status = uc_retrieve_data(&solid,sizeof(solid));
	if (status != UU_SUCCESS) goto done;
/*
.....Get the appropriate vector from the solid
*/
	switch (solid.type)
	{
/*
........Box solid
*/
	case UM_BOX_SOLID:
		um_vctovc(&solid.sdata[3],vec);
		break;
/*
........Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
		um_vctovc(&solid.sdata[3],vec);
		break;
/*
........Sphere solid
*/
	case UM_SPHERE_SOLID:
		vec[0] = 0.; vec[1] = 0.; vec[2] = 1.;
		break;
/*
........Torus solid
*/
	case UM_TORUS_SOLID:
		um_unitvc(&solid.sdata[3],vec);
		break;
/*
........Extruded Solid
*/
	case UM_EXTRUDED_SOLID:
		um_vctovc(&solid.sdata[0],vec);
		break;
/*
........Contour Solid
*/
	case UM_CONTOUR_SOLID:
		um_vctovc(&solid.sdata[2],vec);
		break;
/*
........Revolved Solid
*/
	case UM_REVOLVED_SOLID:
		um_vctovc(&solid.sdata[3],vec);
		break;
/*
.....Unrecognized solid
*/
	default:
		status = UU_FAILURE;
		goto done;
	}
/*
.....Unitize vector
*/
	um_unitvc(vec,vec);
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}
	
/*********************************************************************
**    E_FUNCTION :  ncl_solid_query(key,list)
**       Produces the query output for a Visual Solid.
**    PARAMETERS   
**       INPUT  : 
**          key      Key of solid.
**       OUTPUT :  
**          list     List that contains the query information.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_solid_query(key,list)
UU_KEY_ID key;
UU_LIST *list;
{
	int i,n,status,*pti,numsf,numso,rel;
	UU_REAL d1,d2,*ptr,um_mag();
	UM_coord *pts,ptx[2];
	struct UM_solid_rec solid;
	struct UM_surfattr_rec attr;
	char buf[256],*ptc;
	static char *sunits[2] = {"Inches", "Millimeters"};
	static char *sbin[2] = {"Ascii", "Binary"};
	static char *sclosed[2] = {"Open", "Closed"};
/*
.....Get the solid record
*/
	solid.key = key;
	status = uc_retrieve_data(&solid,sizeof(solid));
	if (status != UU_SUCCESS) goto done;
	attr.key = key;
	status = uc_retrieve_attr(key,&attr);
	if (status != UU_SUCCESS) goto done;
/*
.....Output standard entity data
*/
	sprintf(buf,"   solid type ................ %s",stype[solid.type-1]);
	uj_putstr(list,buf);
	sprintf(buf,"   %s",sclosed[solid.closed]);
	uj_putstr(list,buf);
	uj_putstr(list," ");
	if (solid.type != UM_STL_SOLID && solid.type != UM_COMPOS_SOLID)
	{
		sprintf(buf,
			"                     Model Space                           Working Plane");
		uj_putstr(list, buf);
		uj_putstr(list, " ");
	}
/*
.....Format the solid primitive output
*/
	switch (solid.type)
	{
/*
........Box solid
*/
	case UM_BOX_SOLID:
		pts = (UM_coord *)&solid.sdata[0];

		UM_cc_inttoext(pts[0],ptx[0]);
		um_mcstoccs(0,pts[0],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"lower left   <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		um_vcplvc(pts[0],pts[1],ptx[0]);
		um_vcplvc(ptx[0],pts[2],ptx[0]);
		um_vcplvc(ptx[0],pts[3],ptx[0]);
		um_mcstoccs(0,ptx[0],ptx[1]);
		UM_cc_inttoext(ptx[0],ptx[0]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"upper right  <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		UM_cc_inttoext(pts[1],ptx[0]);
		um_mcstoccs(1,pts[1],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"X-axis      <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		UM_cc_inttoext(pts[2],ptx[0]);
		um_mcstoccs(1,pts[2],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"Y-axis      <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		UM_cc_inttoext(pts[3],ptx[0]);
		um_mcstoccs(1,pts[3],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"Z-axis      <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);
		break;
/*
........Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		pts = (UM_coord *)&solid.sdata[0];

		UM_cc_inttoext(pts[0],ptx[0]);
		um_mcstoccs(0,pts[0],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"center     <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		d2 = um_mag(pts[1]);
		um_unitvc(pts[1],ptx[0]);
		um_mcstoccs(1,ptx[0],ptx[1]);
		sprintf(buf,
			"axis       <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);
		UM_len_inttoext(d2,d1)
		sprintf(buf,"height      %10.4f                          %10.4f",d1,d1);
		uj_putstr(list,buf);

		if (solid.type == UM_CONE_SOLID)
		{
			UM_len_inttoext(ptr[6],d1)
			sprintf(buf,"bottom rad  %10.4f                          %10.4f",
				d1,d1);
			uj_putstr(list,buf);
			UM_len_inttoext(ptr[7],d1)
			sprintf(buf,"top rad     %10.4f                          %10.4f",
				d1,d1);
			uj_putstr(list,buf);
		}
		else
		{
			UM_len_inttoext(ptr[6],d1)
			sprintf(buf,"radius      %10.4f                          %10.4f",
				d1,d1);
			uj_putstr(list,buf);
		}
		break;
/*
........Sphere
*/
	case UM_SPHERE_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		pts = (UM_coord *)&solid.sdata[0];

		UM_cc_inttoext(pts[0],ptx[0]);
		um_mcstoccs(0,pts[0],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"center     <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		UM_len_inttoext(ptr[3],d1)
		sprintf(buf,"radius      %10.4f                          %10.4f",d1,d1);
		uj_putstr(list,buf);
		break;
/*
........Torus
*/
	case UM_TORUS_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		pts = (UM_coord *)&solid.sdata[0];

		UM_cc_inttoext(pts[0],ptx[0]);
		um_mcstoccs(0,pts[0],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"center     <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		um_unitvc(pts[1],ptx[0]);
		um_mcstoccs(1,ptx[0],ptx[1]);
		sprintf(buf,
			"axis       <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		UM_len_inttoext(ptr[6],d1)
		sprintf(buf,"axial rad   %10.4f                          %10.4f",d1,d1);
		uj_putstr(list,buf);
		UM_len_inttoext(ptr[7],d1)
		sprintf(buf,"circle rad  %10.4f                          %10.4f",d1,d1);
		uj_putstr(list,buf);
		break;
/*
........Extruded
*/
	case UM_EXTRUDED_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		pts = (UM_coord *)&solid.sdata[4];

		um_unitvc(&ptr[0],ptx[0]);
		um_mcstoccs(1,ptx[0],ptx[1]);
		sprintf(buf,
			"direction  <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		n = ptr[3];
		sprintf(buf,"# of points %d",n);
		uj_putstr(list,buf);

		UM_cc_inttoext(pts[0],ptx[0]);
		um_mcstoccs(0,pts[0],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"cv point   <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);
		break;
/*
........Contour
*/
	case UM_CONTOUR_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		pts = (UM_coord *)&solid.sdata[6];

		UM_len_inttoext(ptr[0],d1)
		sprintf(buf,"expansion   %10.4f                          %10.4f",d1,d1);
		uj_putstr(list,buf);
		if (ptr[1] == 1.) sprintf(buf,"shape       box");
		else sprintf(buf,"shape       contour");

		um_unitvc(&ptr[2],ptx[0]);
		um_mcstoccs(1,ptx[0],ptx[1]);
		sprintf(buf,
			"direction  <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		n = ptr[5];
		sprintf(buf,"# of points %d",n);
		uj_putstr(list,buf);

		UM_cc_inttoext(pts[0],ptx[0]);
		um_mcstoccs(0,pts[0],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"cv point   <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);
		break;
/*
........Revolved
*/
	case UM_REVOLVED_SOLID:
		ptr = (UU_REAL *)&solid.sdata[0];
		pts = (UM_coord *)&solid.sdata[0];

		UM_cc_inttoext(pts[0],ptx[0]);
		um_mcstoccs(0,pts[0],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"center     <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		um_unitvc(pts[1],ptx[0]);
		um_mcstoccs(1,ptx[0],ptx[1]);
		sprintf(buf,
			"axis       <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);

		d1 = ptr[6];
		sprintf(buf,"start angle %10.4f                          %10.4f",d1,d1);
		uj_putstr(list,buf);
		d1 = ptr[7];
		sprintf(buf,"end angle   %10.4f                          %10.4f",d1,d1);
		uj_putstr(list,buf);

		n = ptr[8];
		sprintf(buf,"# of points %d",n);
		uj_putstr(list,buf);

		UM_cc_inttoext(pts[3],ptx[0]);
		um_mcstoccs(0,pts[3],ptx[1]);
		UM_cc_inttoext(ptx[1],ptx[1]);
		sprintf(buf,
			"cv point   <%10.4f,%10.4f,%10.4f>  <%10.4f,%10.4f,%10.4f>",
			ptx[0][0],ptx[0][1],ptx[0][2], ptx[1][0],ptx[1][1],ptx[1][2]);
		uj_putstr(list,buf);
		break;
/*
........STL
*/
	case UM_STL_SOLID:
		pti = (int *)&solid.sdata[0];
		ptc = (char *)&solid.sdata[14];

		sprintf(buf,"Units = %s",sunits[pti[0]]);
		uj_putstr(list,buf);
		sprintf(buf,"Format = %s",sbin[pti[1]]);
		uj_putstr(list,buf);
		sprintf(buf,"File = %s",ptc);
		uj_putstr(list,buf);
		break;
/*
........Composite
*/
	case UM_COMPOS_SOLID:
		ncl_solid_count_components(&solid,&numso,&numsf);
		if (numsf != 0)
		{
			sprintf(buf,"Number of surfaces = %d",numsf);
			uj_putstr(list,buf);
		}
		if (numso != 0)
		{
			sprintf(buf,"Number of solids = %d",numso);
			uj_putstr(list,buf);
		}
		break;
	}
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_print_solid(solid)
**       Prints out the canonical data for a Visual Solid.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid entity.
**       OUTPUT :  none
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_print_solid(solid)
struct UM_solid_rec *solid;
{
	int i,np,status;
	UU_REAL *ptr;
	UM_coord *pts;
/*
.....Output standard entity data
*/
	np = 0;
	sprintf(UM_sbuf,"   solid type: %s",stype[solid->type-1]);
	um_pscroll(UM_sbuf);
/*
.....Format the solid primitive output
*/
	switch (solid->type)
	{
/*
........Box solid
*/
	case UM_BOX_SOLID:
		pts = (UM_coord *)&solid->sdata[0];

		um_p_ary(UM_PFLOAT,"lower left",3,pts[0]);
		um_p_ary(UM_PFLOAT,"X-axis",3,pts[1]);
		um_p_ary(UM_PFLOAT,"Y-axis",3,pts[2]);
		um_p_ary(UM_PFLOAT,"Z-axis",3,pts[3]);
		break;
/*
........Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
		ptr = (UU_REAL *)&solid->sdata[0];
		pts = (UM_coord *)&solid->sdata[0];

		
		um_p_ary(UM_PFLOAT,"center",3,pts[0]);
		um_p_ary(UM_PFLOAT,"axis",3,pts[0]);

		if (solid->type == UM_CONE_SOLID)
		{
			um_p_ary(UM_PFLOAT,"bottom radius",1,&ptr[6]);
			um_p_ary(UM_PFLOAT,"top radius",1,&ptr[7]);
		}
		else
			um_p_ary(UM_PFLOAT,"radius",1,&ptr[6]);
		break;
/*
........Sphere
*/
	case UM_SPHERE_SOLID:
		ptr = (UU_REAL *)&solid->sdata[0];
		pts = (UM_coord *)&solid->sdata[0];

		um_p_ary(UM_PFLOAT,"center",3,pts[0]);
		um_p_ary(UM_PFLOAT,"radius",1,&ptr[6]);
		break;
/*
........Torus
*/
	case UM_TORUS_SOLID:
		ptr = (UU_REAL *)&solid->sdata[0];
		pts = (UM_coord *)&solid->sdata[0];

		um_p_ary(UM_PFLOAT,"center",3,pts[0]);
		um_p_ary(UM_PFLOAT,"axis",3,pts[1]);
		um_p_ary(UM_PFLOAT,"axis radius",1,&pts[6]);
		um_p_ary(UM_PFLOAT,"circular radius",1,&pts[7]);
		break;
/*
........Extruded
*/
	case UM_EXTRUDED_SOLID:
		ptr = (UU_REAL *)&solid->sdata[0];
		pts = (UM_coord *)&solid->sdata[4];
		np = ptr[3];

		um_p_ary(UM_PFLOAT,"direction",3,pts[0]);
		um_p_ary(UM_PINT,"npts",1,&np);

		pts = (UM_coord *)&solid->sdata[4];
		break;
/*
........Contour
*/
	case UM_CONTOUR_SOLID:
		ptr = (UU_REAL *)&solid->sdata[0];
		pts = (UM_coord *)&solid->sdata[6];
		np = ptr[5];

		um_p_ary(UM_PFLOAT,"expansion",1,&ptr[0]);

		if (ptr[1] == 1.) sprintf(UM_sbuf,"shape: box");
		else sprintf(UM_sbuf,"shape: contour");
		um_pscroll(UM_sbuf);

		um_p_ary(UM_PFLOAT,"direction",3,pts[0]);
		um_p_ary(UM_PINT,"npts",1,&np);

		pts = (UM_coord *)&solid->sdata[6];
		break;
/*
........Revolved
*/
	case UM_REVOLVED_SOLID:
		ptr = (UU_REAL *)&solid->sdata[0];
		pts = (UM_coord *)&solid->sdata[0];
		np = ptr[8];

		um_p_ary(UM_PFLOAT,"center",3,pts[0]);
		um_p_ary(UM_PFLOAT,"axis",3,pts[1]);
		um_p_ary(UM_PFLOAT,"start angle",1,&ptr[6]);
		um_p_ary(UM_PFLOAT,"end angle",1,&ptr[7]);
		um_p_ary(UM_PINT,"npts",1,&np);

		pts = (UM_coord *)&solid->sdata[9];
		break;
	}
/*
........Print out curve points
*/
	for (i=0;i<np;i++)
	{
		sprintf(UM_sbuf,"p%d",i);
		um_p_ary(UM_PFLOAT,UM_sbuf,3,pts[i]);
	}
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_solid_proj_to_drawing(solid,tf,attr,drwmat,
**                         vrefpt,vpnorm,keylist,render)
**       Create a list of keys (KEYLIST) of entities to be included
**       in a drawing for the given Visual Solid.
**       The transformation (DRWMAT) will position the viewplane
**       (VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**       scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**          solid    pointer to entity data (master tuple data only)
**          tfmat    matrix to position entity in MCS
**          attr     pointer to attribute bundle 
**          drwmat   transformation to convert MCS to DCS
**          vrefpt   reference point of viewing plane
**          vpnorm   normal of viewing plane
**          render   (ignored)
**       OUTPUT :  
**          keylist  keys of projected entities are pushed onto this list
**    RETURNS      : 
**       UU_SUCCESS if no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_solid_proj_to_drawing(solid,tf,attr,drwmat,vrefpt,vpnorm,keylist,render)
struct UM_solid_rec *solid;
UM_transf tf;
struct UM_surfattr_rec *attr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
UU_LIST *keylist;
UU_LOGICAL render;
{
	int status;
/*
.....Retrieve the display list
*/
	status = uc_retrieve_data(solid,sizeof(struct UM_solid_rec));
	if (status != UU_SUCCESS) goto done;
/*
.....Project display list onto drawing
*/
	status = ncl_displst_proj_to_drawing(solid,solid->displst,attr,drwmat,
		vrefpt,vpnorm,keylist);
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_solid_count_components(solid,nsol,nsrf)
**       Return the number of solids and surfaces within a composite
**       solid.
**    PARAMETERS   
**       INPUT  : 
**          solid    pointer to solid.
**       OUTPUT :  
**          numso    number of solid components in 'solid'.
**          numsf    number of surface components in 'solid'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_solid_count_components(solid,numso,numsf)
struct UM_solid_rec *solid;
int *numso,*numsf;
{
	int i,rel;
/*
.....Count the number of component surfaces and solids
*/
	*numsf = 0;
	*numso = 0;
	for (i=0;i<solid->no_netkey;i++)
	{
		um_retrieve_data_relnum(solid->netkey[i],&rel);
		if (rel == UM_SOLID_REL) *numso = *numso + 1;
		else *numsf = *numsf + 1;
	}
}
	
ncl_solid_near_point() {}
