/*********************************************************************
**    NAME         :  tspmsrf.c
**       CONTAINS:
**             utp_in_advanced_face
**             utp_in_brep
**					utp_in_manifold_surface
**             utp_in_open_shell
**             utp_in_oriented_shell
**             utp_in_shell_based
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tspmsrf.c , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**       04/05/18 , 15:06:31
*********************************************************************/

#include "class.h"
#include "nccs.h"
#include "tiges.h"
#include "tigdefs.h"
#include "tstep.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdeval.h"
#include "mdattr.h"
#include "mgeom.h"
#include "udebug.h"
#include "tigglobal.h"
#include "mlabddl.h"
#include "nconst.h"

#define DEBUG_LABEL 1
UU_LOGICAL UTP_debug_label=UU_FALSE;

static UU_LOGICAL Ssolid=UU_FALSE;
static UU_LOGICAL Sgroup=UU_FALSE;

static void S_get_cvpln();
static int S_get_cylcv();
static int S_get_torang();
static void S_calc_box();
static void S_fix_composite_circles();
static int S_find_connect_points();
static int S_single_boundary();
static int S_trim_store();
static UU_LOGICAL S_check_reverse();
static int S_check_match();
static int S_create_curves();
static UU_LOGICAL S_sf_is_planar();
static UU_LIST Slist;

extern int NCL_ubcopy;
extern UU_LOGICAL T_DEBUV;


/*********************************************************************
**    E_FUNCTION     :  utp_in_advanced_face(ptr,relnum)
**				ADVANCED_FACE surface handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_advanced_face(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,j,cvrel,sfrel,stat,ncvs,inc,ipt,outer,status,index,iflag,sense2;
	UU_REAL tol,uv[6],rad,rad1,cyl[9],sang,eang,cang;
	char label[20];
	UU_KEY_ID key,*bkey,skey,*uvkey,cvkey,*tkey;
	UM_coord cpt,ln[2],spt;
	UM_vector nvec,svec,csvec;
	struct NCL_fixed_databag cv,sf;
	struct NCL_nclpl_rec pl;
	struct UM_rbsplsrf_rec *bsf;
	UTPs_step_record *tptr,uptr;
	UU_LOGICAL sphfl,merged,flag,out_found,check,do_bspline,sense1;

	struct NCL_fixed_databag crv; 
	struct UM_compcrv_rec *ccrv;

	UU_REAL u;
	UM_transf tfmat;
	struct UM_evcrvout evout;
	char output[50];	// buffer to store for writing in Errors file. Sasha, April 2017
	

	UM_coord pz,tpz;
	UM_vector xaxis,yaxis;
	UU_REAL ang = 0.0,base_ang = 10.0/UM_RADIAN;
	char sbuf[80];
	struct UM_rotmatrix mx;
/*
.....Initialize routine
*/
	UIG_from_trimsrf = UU_TRUE;
	UIG_comp_tol = .005;
	bkey = uvkey = UU_NULL;
	strcpy(sf.label,"@UN");
	sf.subscr = 0;
	iflag = index = key = 0;
	skey = 0;
	tptr = ptr;
	out_found = merged = sphfl = UU_FALSE;
	*relnum = 0;
	utp_get_logicals(&ptr->parm[ptr->nparm-1],&sense1,1);
T_DEBUV = UU_FALSE;
if (ptr->recno == 472 || ptr->recno == 535 || ptr->recno == 286 || ptr->recno == 223)
{
	T_DEBUV = UU_FALSE;
	stat = UU_SUCCESS;
}
/*
.....Initialize the attribute bundle
*/
	utp_set_attr(ptr,UU_FALSE);
/*
.....Process the boundary curves
.......Modified FACE_OUTER_BOUND case.  There are some STEP files that
.......list multiple surfaces as outer boundaries - ASF 7/2/13.
*/
	inc = 1;
	ncvs = ptr->nparm - 3;
	bkey = (UU_KEY_ID *)uu_malloc((ncvs+1)*sizeof(UU_KEY_ID));
	tkey = (UU_KEY_ID *)uu_malloc((ncvs+1)*sizeof(UU_KEY_ID));
	if (bkey == UU_NULL) goto failed;
/*
.....Added loop to ensure all key values start as zero to prevent
.....memory errors in debug code (accessing invalid keys) ASF 10/15/13.
*/
	for (i=0;i<=ncvs;i++) bkey[i] = tkey[i] = 0;
	for (i=0;i<ncvs;i++)
	{
		if (ptr->parm[i+1].type != UTP_RECNO) goto synerr;
		tptr = utp_get_record(ptr->parm[i+1].ptype.recno);
		if (tptr == UU_NULL) goto failed;
		if (tptr->command == FACE_OUTER_BOUND)
		{
			if (bkey[0] > 0 || out_found)
			{
				if (!out_found)
				{
					for (j=ncvs;j>0;j--) bkey[j] = bkey[j-1];
					bkey[0] = 0;
					inc++;
					out_found = UU_TRUE;
				}
				ipt = inc++;
			}
			else
				ipt = 0;
		}
		else ipt = inc++;
		bkey[ipt] = utp_in_dispat_recno(ptr->parm[i+1].ptype.recno,&cvrel);
		if (bkey[ipt] == 0) goto failed;
		tkey[ipt] = bkey[ipt];
	}
/*
.....Process the surface
*/
	ipt = ptr->nparm - 2;
	if (ptr->parm[ipt].type == UTP_RECNO)
	{																								// Does not change anything
/*
........Get the surface itself
*/
		check = UU_FALSE;
		rad = rad1 = 0.;
		tptr = utp_get_record(ptr->parm[ipt].ptype.recno);
		if (tptr->command == CONICAL_SURFACE)
		{
			check = (ncvs == 2);
			stat = utp_map_cone(tptr,cyl);
			um_vctovc(&cyl[3],nvec); um_vctovc(&cyl[0],cpt);
			if (stat != UU_SUCCESS) goto done;
		}
		else if (tptr->command == CYLINDRICAL_SURFACE)
		{ 
			check = (ncvs == 2);
			stat = utp_map_cylinder(tptr,cyl);
			um_vctovc(&cyl[3],nvec); um_vctovc(&cyl[0],cpt);
			if (stat != UU_SUCCESS) goto done;
		}
		else if (tptr->command == SPHERICAL_SURFACE)
		{
			check = (ncvs == 2);
			sphfl = UU_TRUE;
			stat = utp_map_sphere(tptr,cpt,nvec,svec,&rad);
			if (stat != UU_SUCCESS) goto done;
		}
		else if (tptr->command == TOROIDAL_SURFACE)
		{
			check = (ncvs == 2);
			stat = utp_map_torus(tptr,cpt,nvec,svec,&rad,&rad1);
			if (stat != UU_SUCCESS) goto done;
		}
		else
			nvec[0] = nvec[1] = nvec[2] = 0.;
/*
.....memory error fixed
.....Yurong
*/
/*		if (bkey[0] == 0 || (ncvs == 2 && check) */
		if ((bkey[0] == 0 || (ncvs == 2 && check))&&(ncvs>0))
		{
			if (bkey[0] != 0)
			{
				bkey[ncvs] = bkey[0];
				bkey[0] = 0;
			}
			stat = S_calc_outer_boundary(bkey,&ncvs,nvec,cpt,
				tptr->command,rad,&merged,&index,iflag);
			if (stat != UU_SUCCESS) goto cverr;
		}
		uv[0] = uv[2] = 0.;
		uv[1] = uv[3] = 1.;
/*
........CONICAL_SURFACE
.....Added fix for merged boundaries that give invalid angels - ASF 1/27/14.
*/
		if (tptr->command == CONICAL_SURFACE)
		{
			stat = S_get_cylcv(bkey,&ncvs,cyl,ln,&sang,&eang,UU_FALSE);
			if (stat != UU_SUCCESS) goto cverr;
			if (eang - sang > 360.5 && merged)
			{
				iflag = 1; ncvs += 1;
				if (tkey[0] != 0)
				{
					tkey[ncvs] = tkey[0];
					tkey[0] = 0;
				}
				stat = S_calc_outer_boundary(tkey,&ncvs,nvec,cpt,
					tptr->command,rad,&merged,&index,iflag);
				if (stat != UU_SUCCESS) goto cverr;
				for (i=0;i<ncvs;i++) bkey[i] = tkey[i];
				stat = S_get_cylcv(bkey,&ncvs,cyl,ln,&sang,&eang,UU_FALSE);
				if (stat != UU_SUCCESS) goto cverr;
/*
.....if still wrong value, should give error
.....Yurong
*/
				if (eang - sang > 360.5)
					goto cverr;
			}
			else if (eang - sang > 360.5)
				goto cverr;
			stat = ncl_cylinder_to_sf(cyl,ln,sang,eang,&sf);
			if (stat != UU_SUCCESS) goto failed;
			skey = sf.key;
			sfrel = sf.rel_num;
		}
/*
........CYLINDRICAL_SURFACE
.....Added fix for merged boundaries that give invalid angels - ASF 1/27/14.
*/
		else if (tptr->command == CYLINDRICAL_SURFACE)
		{
			stat = S_get_cylcv(bkey,&ncvs,cyl,ln,&sang,&eang,UU_FALSE);
			if (stat != UU_SUCCESS) goto cverr;
			if (eang - sang > 360.5 && merged)
			{
				iflag = 1; ncvs += 1;
				if (tkey[0] != 0)
				{
					tkey[ncvs] = tkey[0];
					tkey[0] = 0;
				}
				stat = S_calc_outer_boundary(tkey,&ncvs,nvec,cpt,
					tptr->command,rad,&merged,&index,iflag);
				if (stat != UU_SUCCESS) 
					goto cverr;
				for (i=0;i<ncvs;i++) bkey[i] = tkey[i];
				stat = S_get_cylcv(bkey,&ncvs,cyl,ln,&sang,&eang,UU_FALSE);
				if (stat != UU_SUCCESS) 
					goto cverr;
/*
.....if still wrong value, should give error
*/
				if (eang - sang > 360.5)
					goto cverr;
			}
			else if (eang - sang > 360.5)
				goto cverr;
			stat = ncl_cylinder_to_sf(cyl,ln,sang,eang,&sf);
			if (stat != UU_SUCCESS) 
				goto failed; 
			skey = sf.key;
			sfrel = sf.rel_num;
		}
/*
........PLANE
*/
		else if (tptr->command == PLANE)
		{
			pl.key = 1;
			stat = utp_map_plane(tptr,&pl);
			if (stat != UU_SUCCESS) goto done;
			cv.key = bkey[0];
			stat = ncl_retrieve_data_fixed(&cv);
			stat = ncl_plane_to_sf(&pl,&cv,&sf);
			if (stat != UU_SUCCESS) goto failed;
			skey = sf.key;
			sfrel = sf.rel_num;
		}
/*
........SPHERICAL_SURFACE
*/
		else if (tptr->command == SPHERICAL_SURFACE)
		{
			S_get_cvpln(bkey,ncvs,cpt,nvec,svec,csvec,&sang,&eang,&cang,
				merged,index);
			stat = ncl_sphere_to_sf(cpt,nvec,svec,csvec,rad,sang,eang,
				cang,&sf);
			if (stat != UU_SUCCESS) goto failed;
			skey = sf.key;
			sfrel = sf.rel_num;
		}
/*
........SURFACE_OF_LINEAR_EXTRUSION
*/
		else if (tptr->command == SURFACE_OF_LINEAR_EXTRUSION)
		{
			stat = utp_map_extruded_surf(tptr,&sf,bkey[0]);
			if (stat != UU_SUCCESS) goto done;
			skey = sf.key;
			sfrel = sf.rel_num;
		}
/*
........TOROIDAL_SURFACE
..........Added case in boundary merge for torus.  When it is used, the given
..........parameters should be used without modification - ASF 7/2/13.
*/
		else if (tptr->command == TOROIDAL_SURFACE)
		{
			S_get_torang(bkey[0],cpt,nvec,svec,spt,&sang,&eang,UU_FALSE);
			flag = (merged && index == -1);
			stat = ncl_torus_to_sf(bkey[0],cpt,nvec,svec,rad,rad1,spt,sang,eang,
				flag,&sf);
			if (stat != UU_SUCCESS) goto failed;
			skey = sf.key;
			sfrel = sf.rel_num;
		}
/*
........SURFACE_OF_REVOLUTION
*/
		else if (tptr->command == SURFACE_OF_REVOLUTION)
		{
			stat = utp_map_revolved_surf(tptr,cpt,nvec,svec,&cvkey);
			if (stat != UU_SUCCESS) goto done;
			S_get_torang(bkey[0],cpt,nvec,svec,spt,&sang,&eang,UU_TRUE);
			stat = ncl_revolve_to_sf(cvkey,cpt,nvec,sang,eang,&sf);
			if (stat != UU_SUCCESS) goto failed;
			skey = sf.key;
			sfrel = sf.rel_num;
		}
/*
........B_SPLINE_SURFACE_WITH_KNOTS
*/
		else if (tptr->command == B_SPLINE_SURFACE_WITH_KNOTS)
		{
			stat = utp_map_bspline_surf(tptr,&sf);
			if (stat != UU_SUCCESS) goto failed;//done;
			do_bspline = UU_TRUE;
			bsf = (struct UM_rbsplsrf_rec *)&sf;
/*
.....Build planar surface if bspline surface is planar to
.....prevent problems with sspline using large base surface
.....relative to curve - ASF 10/16/13.
*/
			if (bsf->ku == 2 && bsf->kv == 2)
			{
				do_bspline = !S_sf_is_planar(bsf,&pl);
				if (!do_bspline)
				{
					cv.key = bkey[0];
					stat = ncl_retrieve_data_fixed(&cv);
					if (stat != UU_SUCCESS)
						goto failed;
					stat = ncl_plane_to_sf(&pl,&cv,&sf);
					if (stat != UU_SUCCESS)
						goto failed;
				}
			}
			skey = utp_store_surf(&sf,sf.label,UU_TRUE);
			if (do_bspline)
				uig_get_rbuv(&sf.key,&uv[0],&uv[2],&uv[1],&uv[3]);
			sfrel = sf.rel_num;
		}
/*
........BOUNDED_SURFACE
*/
		else if (tptr->command == BOUNDED_SURFACE)
		{
			if (tptr->parm[0].type != UTP_COMMAND) goto failed;
			uptr.command = tptr->parm[0].ptype.cmd;
			uptr.recno = tptr->recno;
			uptr.nparm = tptr->nparm - 1;
			uptr.parm = &tptr->parm[1];
/*
...........B-spline surface
*/
			if (uptr.command == B_SPLINE_SURFACE ||
				uptr.command == B_SPLINE_SURFACE_WITH_KNOTS)
			{
				status = utp_map_bspline_surf(&uptr,&sf);
				if (uptr.command == B_SPLINE_SURFACE_WITH_KNOTS)
				{
					do_bspline = UU_TRUE;
					bsf = (struct UM_rbsplsrf_rec *)&sf;
/*
.....Build planar surface if bspline surface is planar to
.....prevent problems with sspline using large base surface
.....relative to curve - ASF 10/16/13.
*/
					if (bsf->ku == 2 && bsf->kv == 2)
					{
						do_bspline = !S_sf_is_planar(bsf,&pl);
						if (!do_bspline)
						{
							cv.key = bkey[0];
							stat = ncl_retrieve_data_fixed(&cv);
							if (stat != UU_SUCCESS)
								goto failed;
							stat = ncl_plane_to_sf(&pl,&cv,&sf);
							if (stat != UU_SUCCESS)
								goto failed;
						}
					}
				}
			}
			else
				goto failed;
/*
...........Store surface
*/
			if (status == UU_SUCCESS)
			{
				skey = utp_store_surf(&sf,sf.label,UU_TRUE);
				if (do_bspline)
					uig_get_rbuv(&sf.key,&uv[0],&uv[2],&uv[1],&uv[3]);
				sfrel = sf.rel_num;
			}
		}
/*
........Quasi-Uniform surface
*/
		else if (tptr->command == QUASI_UNIFORM_SURFACE)
		{
			status = utp_map_bspline_surf(tptr,&sf);
			if (status != UU_SUCCESS) goto failed;
			skey = utp_store_surf(&sf,sf.label,UU_TRUE);
			sfrel = sf.rel_num;
		}
/*
........Evaluated surface
*/
		else
			skey = utp_in_dispat(tptr,&sfrel);
		if (skey == 0) goto failed;
/*
.....Make sure surface normal points to
.....outside of the solid
*/
		sf.key = skey;
		ncl_retrieve_data_fixed(&sf);
		sense2 = ncl_get_surface_sense(&sf);
		if (sense1 && sense2 == -1 || !sense1 && sense2 == 1)
			ncl_reverse_surface_sense(&sf);
/*
.....Create the UV boundary curve(s) if necessary
*/
		if (ncvs > 0)
		{
			uvkey = (UU_KEY_ID *)uu_malloc(ncvs*sizeof(UU_KEY_ID));
			if (uvkey == UU_NULL) goto failed;
			uv[4] = uv[5] = 0.;
			tol = .002;
			UIG_change_uv = UU_FALSE;
			for (i=0;i<ncvs;i++)
			{
				if (bkey[i] == 0)
					uvkey[i] = 0;
				else
				{					
					stat = uig_evcrv_to_uvcv(&bkey[i],&skey,&uvkey[i],uv,tol);
					if (stat != 0) goto cverr;
/*
.....When uv value wrong, should give error
.....Yurong
*/
/*					uig_check_uv(uv,uvkey[i]); */
					stat = uig_check_uv(uv,uvkey[i]);
					if (stat != 0) 
						goto cverr;
				}
			}
/*
.....Added check to make sure inner boundary curves are inside outer
.....boundary curves - ASF 1/27/14.
*/
			if (ncvs > 1)
			{
				stat = S_calc_outer_uvboundary(uvkey,ncvs);
				if (stat != 0) goto cverr;
			}
/*
.....Create the trimmed surface
*/
			key = utp_store_trimsf(ptr->parm[0].ptype.str,ptr->recno,skey,uv,
				uvkey,bkey,ncvs,Ssolid);
			if (key == 0) goto failed;
			if (entity_mask[0] == 1) S_create_curves(bkey,ncvs,ptr->recno);
		}
		else
		{
			key = skey;
			utp_count_translated(sfrel,1,UU_TRUE);
			utp_store_step_label(skey,ptr->parm[0].ptype.str,ptr->recno);
		}
/*
.....Delete duplicate geometry if flag is set.
*/
		sf.key = key;
		if (UIG_nodups && ncl_retrieve_data_fixed(&sf) == UU_SUCCESS &&
			uig_match_surface(&sf,0) == UU_SUCCESS)
		{
			ur_delete_all(sf.key);
			if (sf.rel_num == NCL_TRIMSF_REL)
				ur_delete_all(((struct NCL_trimsf_rec *)&sf)->bs_key);
			skey = key = -1;
			UIG_dupcount++;
		}
		else if (ncl_retrieve_data_fixed(&sf) == UU_SUCCESS)
			*relnum = sf.rel_num;
/*
.....Label the base surface &
.....boundary curve
*/
#ifdef DEBUG_LABEL
//		utp_store_debug_label(bkey[0],ptr,0,0);
//		utp_store_debug_label(skey,ptr,1,0);
//		utp_store_debug_label(key,ptr,2,0);
#endif
	}
	else
		goto synerr;
	goto done;
/*
.....Could not create surface
*/
failed:;
	key = 0;
	utp_surface_error(tptr);
	goto done;
/*
.....Could not create boundary curve
*/
cverr:;
	key = 0;
	utp_boundary_error(ptr);
	goto done;
/*
.....STEP file syntax error
*/
synerr:;
	key = 0;
	utp_syntax_error(ptr);
	goto done;
/*
....End of routine
*/
done:;
	UIG_from_trimsrf = UU_FALSE;
/*
.....Could not create trimmed surfce
.....Store base surface and boundary curve(s)
.....As standard geometry
.......Modified error output so curves can still be output even
.......if a base surface is not created - ASF 7/2/13.
*/
	if (key == 0)
	{
		utp_count_translated(NCL_TRIMSF_REL,1,UU_FALSE);
		if (skey != 0)
		{
			utp_count_translated(sfrel,1,UU_TRUE);
			utp_store_debug_label(skey,ptr,1,0);
		}
		else
			utp_count_translated(sfrel,1,UU_FALSE);
		for (i=0;i<ncvs;i++)
		{
			if (bkey[i] == 0) continue;
			utp_store_debug_label(bkey[i],ptr,0,i);
			ur_retrieve_data_relnum(bkey[i],&cvrel);
			utp_count_translated(cvrel,1,UU_TRUE);
		}
	}
	else if (key != skey && !Ssolid)
		utp_count_translated(NCL_TRIMSF_REL,1,UU_TRUE);
	if (bkey != UU_NULL) uu_free(bkey);
	if (uvkey != UU_NULL) uu_free(uvkey);
	if (key == -1)
	{
		key = 0;
		*relnum = 0;
	}
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_brep(ptr,relnum)
**				BREP_WITH_VOIDS handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_brep(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i;
	UU_KEY_ID key;
/*
.....Initialize the attribute bundle
*/
if (ptr->recno == 94145)
{
	i = 0;
}
	utp_set_attr(ptr,UU_TRUE);
/*
.....Get the pointer to the shell
*/
	if (ptr->parm[1].type == UTP_RECNO)
		for (i=1;i<ptr->nparm;i++)
			key = utp_in_dispat_recno(ptr->parm[i].ptype.recno,relnum);
/*
.....Could not reference shell
*/
	else
	{
		key = 0;
		utp_syntax_error(ptr);
	}
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_manifold_surface(ptr,relnum)
**				MANIFOLD_SURFACE_SHAPE_REPRESENTATION  &
**          ADVANCED_BREP_SHAPE_REPRESENTATION handling routine.
**          Typically a top level designator for surface definitions.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_manifold_surface(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int status,i;
	UU_KEY_ID key;
	UTPs_step_record *tptr;
/*
.....Get the active Units
*/
	if (ptr->parm[ptr->nparm-1].type == UTP_RECNO)
	{
		tptr = utp_get_record(ptr->parm[ptr->nparm-1].ptype.recno);
		if (tptr != UU_NULL)
		{
			status = utp_map_units(tptr);
			if (status != UU_SUCCESS)
			{
				key = 0;
				goto done;
			}
		}
	}
	else
		goto synerr;
/*
.....Initialize the attribute bundle
*/
	utp_set_attr(ptr,UU_TRUE);
/*
.....Get the pointer to the shell
*/
	for (i=1;i<ptr->nparm-1;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
			key = utp_in_dispat_recno(ptr->parm[i].ptype.recno,relnum);
/*
.....Could not reference shell
*/
		else
			goto synerr;
	}
	goto done;
/*
.....Syntax error
*/
synerr:;
	key = 0;
	utp_syntax_error(ptr);
/*
.....End of routine
*/
done:;
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_open_shell(ptr,relnum)
**				OPEN_SHELL surface handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_open_shell(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
int x;
	int i,nkeys;
	UU_LOGICAL closed;
	UU_KEY_ID key,*sfkeys;
/*
.....Initialize the attribute bundle
*/
	utp_set_attr(ptr,UU_TRUE);
	closed = ptr->command == CLOSED_SHELL;
/*
.....Check for solid creation
*/
	nkeys = 0;
/*
.....Added ability to create a solid from new command
.....ADVANCED_GROUP_ASSIGNMENT - ASF 11/12/13.
*/
	if (!Sgroup)
		Ssolid = (/*ptr->command == CLOSED_SHELL &&*/ entity_mask[1] == 1 &&
			ptr->nparm > 2);
	if (Ssolid && !Sgroup)
		uu_list_init(&Slist,sizeof(UU_KEY_ID),50,50);
/*
.....Get the pointer to the face
*/
	if (ptr->parm[1].type == UTP_RECNO)
	{
		for (i=1;i<ptr->nparm;i++)
		{
			key = utp_in_dispat_recno(ptr->parm[i].ptype.recno,relnum);
			if (key != 0 && Ssolid) uu_list_push(&Slist,&key);
		}
/*
........Store solid
*/
		if (Ssolid && !Sgroup)
		{
			sfkeys = (UU_KEY_ID *)UU_LIST_ARRAY(&Slist);
			nkeys = UU_LIST_LENGTH(&Slist);
			if (nkeys != 0)
			{
				key = utp_store_solid(ptr->parm[0].ptype.str,ptr->recno,sfkeys,
					nkeys,closed);
				if (key == 0)
				{
					utp_count_translated(UM_SOLID_REL,1,UU_FALSE);
					utp_solid_error(ptr);
					goto done;
				}
				else
					utp_count_translated(UM_SOLID_REL,1,UU_TRUE);
			}
		}
	}
/*
.....Could not reference shell
*/
	else
	{
		key = 0;
		utp_syntax_error(ptr);
	}
/*
.....End of routine
*/
done:;
	if (Ssolid && !Sgroup) uu_list_free(&Slist);
	if (!Sgroup) Ssolid = UU_FALSE;
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_oriented_shell(ptr,relnum)
**				ORIENTED_CLOSED_SHELL surface handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_oriented_shell(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	UU_KEY_ID key;
/*
.....Initialize the attribute bundle
*/
	utp_set_attr(ptr,UU_TRUE);
/*
.....Get the pointer to the shell
*/
	if (ptr->parm[2].type == UTP_RECNO)
		key = utp_in_dispat_recno(ptr->parm[2].ptype.recno,relnum);
/*
.....Could not reference shell
*/
	else
	{
		key = 0;
		utp_syntax_error(ptr);
	}
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_shell_based(ptr,relnum)
**				SHELL_BASED_SURFACE surface handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_shell_based(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	UU_KEY_ID key;
	UTPs_step_record *tptr;
	int i;
/*
.....Initialize the attribute bundle
*/
	utp_set_attr(ptr,UU_TRUE);
/*
.....Get the pointer to the shell
.......Modified so SHELL_BASED_SURFACE records that have more than
.......one surface record will process all surfaces - ASF 7/2/13.
*/
	for (i=1;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			key = utp_in_dispat_recno(ptr->parm[i].ptype.recno,relnum);
/*			if (key != 0) utp_count_translated(*relnum,1,UU_TRUE);*/
/*
.....Added ability to create a solid from new command
.....ADVANCED_GROUP_ASSIGNMENT - ASF 11/12/13.
*/
			if (key != 0 && Ssolid && Sgroup) uu_list_push(&Slist,&key);
		}
/*
.....Could not reference shell
*/
		else
		{
			key = 0;
			utp_syntax_error(ptr);
		}
	}
	return(key);
}

/*********************************************************************
**    E_FUNCTION     :  utp_in_group_def(ptr,relnum)
**				APPLIED_GROUP_ASSIGNMENT handling routine.
**    PARAMETERS   
**       INPUT  : 
**          ptr      Pointer to STEP record in global array.
**       OUTPUT : 
**          relnum   Relation number of created entity.
**    RETURNS      :
**          Key of entity created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_KEY_ID utp_in_group_def(ptr,relnum)
UTPs_step_record *ptr;
int *relnum;
{
	int i,nkeys;
	UU_LOGICAL closed;
	UU_KEY_ID key,*sfkeys;
/*
.....Initialize the attribute bundle
*/
	utp_set_attr(ptr,UU_TRUE);
/*
.....Check for solid creation
*/
	nkeys = 0;
	Ssolid = (entity_mask[1] == 1 && ptr->nparm > 2);
	Sgroup = UU_TRUE;
	closed = UU_FALSE;
	if (Ssolid) uu_list_init(&Slist,sizeof(UU_KEY_ID),50,50);
/*
.....Get the pointer to the face
*/
	if (ptr->parm[1].type == UTP_RECNO)
	{
		for (i=1;i<ptr->nparm;i++)
		{
			key = utp_in_dispat_recno(ptr->parm[i].ptype.recno,relnum);
			if (key != 0 && Ssolid) uu_list_push(&Slist,&key);
		}
/*
........Store solid
*/
		if (Ssolid)
		{
			sfkeys = (UU_KEY_ID *)UU_LIST_ARRAY(&Slist);
			nkeys = UU_LIST_LENGTH(&Slist);
			if (nkeys != 0)
			{
				key = utp_store_solid(ptr->parm[0].ptype.str,ptr->recno,sfkeys,
					nkeys,closed);
				if (key == 0)
				{
					utp_count_translated(UM_SOLID_REL,1,UU_FALSE);
					utp_solid_error(ptr);
					goto done;
				}
				else
					utp_count_translated(UM_SOLID_REL,1,UU_TRUE);
			}
		}
	}
/*
.....Could not reference shell
*/
	else
	{
		key = 0;
		utp_syntax_error(ptr);
	}
/*
.....End of routine
*/
done:;
	if (Ssolid) uu_list_free(&Slist);
	Sgroup = Ssolid = UU_FALSE;
	return(key);
}

/*********************************************************************
**    I_FUNCTION     :  S_sf_is_planar(sf,pl);
**       Determine whether bspline surface with knots is planar.  If
**       so, calculate and return the corresponding plane.
**    PARAMETERS   
**       INPUT  : 
**          sf    Surface to check
**       OUTPUT : 
**          pl    Plane matching surface
**    RETURNS      : UU_TRUE iff surface is planar
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_sf_is_planar(sf,pl)
struct UM_rbsplsrf_rec *sf;
struct NCL_nclpl_rec *pl;
{
	int i,ind1,ind2,npts,which;
	UU_REAL dis1,dis2,box[6],tol = 0.001;
	UM_coord *pts,pt1,pt2,tpt,npt;
	UM_vector zvec,xvec,dvec1,dvec2;
	UM_transf tf,tfi;
	char tbuf[80];

	box[0] = box[2] = box[4] = 1.e12;
	box[1] = box[3] = box[5] = -1.e12;
	pts = (UM_coord *)sf->pt;
	npts = sf->no_pt;
	if (npts < 3) return(UU_FALSE);
	which = ind1 = 0; ind2 = npts - 1;
	um_vctovc(pts[npts/2],pl->pt);
	zvec[0] = zvec[1] = 0.; zvec[2] = 1.;
/*
.....Find points to define plane.
*/
	while (ind1 < npts/2 || ind2 > 0)
	{
		dis1 = um_dcccc(pts[ind1],pl->pt); dis2 = um_dcccc(pts[ind2],pl->pt);
		um_vcmnvc(pts[ind1],pl->pt,dvec1); um_unitvc(dvec1,dvec1);
		um_vcmnvc(pts[ind2],pl->pt,dvec2); um_unitvc(dvec2,dvec2);
		if (dis1 > tol && dis2 > tol && fabs(um_dot(dvec1,dvec2)) < 1.-UM_FUZZ)
		{
			um_cross(dvec1,dvec2,pl->nvec); um_unitvc(pl->nvec,pl->nvec);
			break;
		}
		else
		{
			if (which == 0 && ind1 < npts/2) ind1++;
			else if (which == 1 && ind2 > 0) ind2--;
			which = 1 - which;
		}
	}
	um_perpvc (pl->nvec,xvec); um_unitvc(xvec,xvec);
	um_ptzx_tf(pl->pt,pl->nvec,xvec,tf); um_inverttf(tf,tfi);
/*
.....Determine if all points lie in the plane and find bounding box
.....to set plane point in middle of surface.
*/
	for (i=0;i<npts;i++)
	{
		um_cctmtf(pts[i],tfi,tpt);
		if (fabs(tpt[2]) > UM_FUZZ) return (UU_FALSE);
		if (box[0] > tpt[0]) box[0] = tpt[0];
		if (box[1] < tpt[0]) box[1] = tpt[0];
		if (box[2] > tpt[1]) box[2] = tpt[1];
		if (box[3] < tpt[1]) box[3] = tpt[1];
		if (box[4] > tpt[2]) box[4] = tpt[2];
		if (box[5] < tpt[2]) box[5] = tpt[2];
	}
	pl->pt[0] = (box[0]+box[1])/2.;
	pl->pt[1] = (box[2]+box[3])/2.;
	pl->pt[2] = (box[4]+box[5])/2.;
	um_cctmtf(pl->pt,tf,pl->pt);
	return(UU_TRUE);
}

/*********************************************************************
**    I_FUNCTION     :  S_get_cvpln(cvkey,refpt,nvec,svec);
**       Returns a starting and normal vector for the boundary curve.
**    PARAMETERS   
**       INPUT  : 
**          cvkey    Boundary curve key.
**          refpt    Reference point of surface, i.e. center of sphere.
**          merged   UU_TRUE if the boundary curve was merged to form
**                           a single boundary.
**                   UU_FALSE otherwise.
**          index    Index of first connecting component created to
**                   merge the boundary into one curve.
**       OUTPUT : 
**          nvec     Normal vector of plane.
**          svec     Start vector of plane.
**          sang     Starting angle for surface of revolution.
**          eang     Ending angle for surface of revolution.
**          cang     Angle for circle that defines surface.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_get_cvpln(cvkeys,ncvs,refpt,nvec,svec,csvec,sang,eang,
	cang,merged,index)
UU_KEY_ID cvkeys[];
int ncvs,index;
UM_coord refpt;
UM_vector nvec,csvec,svec;
UU_REAL *sang,*eang,*cang;
UU_LOGICAL merged;
{
	int i,tncvs;
	UU_REAL u,cyl[9],ang,h,r;
	UM_coord spt,ept,ptx,ln[2];
	UM_vector tvec;
	UM_transf tfmat;
	UM_plane pln;
	struct UM_compcrv_rec *comp;
	struct UM_crvdatabag crv;
	struct UM_circle_rec ci;
	struct UM_evcrvout evout;
	struct UM_rotmatrix mx;
	char sbuf[80];
	static int calls = 0;
	UU_KEY_ID key[1];
/*
....Get boundary curve
*/
	crv.key = cvkeys[0];
	ncl_retrieve_data_fixed(&crv);
	uc_retrieve_transf (crv.key,tfmat);
	uc_init_evcrvout (&crv,&evout);
/*
.....Get the start and ending points
*/
	u = 0.0;
	uc_evcrv(UM_POINT,u,&crv,tfmat,&evout);
	um_vctovc(evout.cp,spt);
/*
.....Calculate a plane from the end points and reference point
*/
	if (!merged)
	{
		u = 1.0;
		for (i=0;i<10;i++)
		{
			uc_evcrv(UM_POINT,u,&crv,tfmat,&evout);
			um_vctovc(evout.cp,ept);
			if (um_plane1(refpt,spt,ept,&pln))
			{
				um_vctovc(pln.n,nvec);
				um_vcmnvc(spt,refpt,svec); um_unitvc(svec,svec);
				break;
			}
			u = u - .1;
		}
	}
	else
	{
/*
.....Use the already defined circle that connects the two parts
.....of the boundary curve.
*/
		comp = (struct UM_compcrv_rec *)&crv;
		ci.key = comp->cid[index].crvid;
		ncl_retrieve_data_fixed(&ci);
		um_vctovc(nvec,svec);
	}
/*
.....Calculate starting and ending angles for revolved surface
*/
	um_vctovc(refpt,cyl);
	um_vctovc(svec,&cyl[3]);
	cyl[6] = um_dcccc(spt,refpt);
	S_get_cylcv(cvkeys,&ncvs,cyl,ln,sang,eang,UU_TRUE);
	um_vcmnvc(ln[0],refpt,tvec); um_unitvc(tvec,tvec);
/*
.....Determine the angle needed for circle that defines sphere
.....through rotation.
*/
	h = cyl[7];
	r = cyl[6];
	ang = asin((h-r)/r);
	if ((!merged && ang <= UM_HALFPI - (8.0/UM_RADIAN))||
		(merged && ang <= UM_HALFPI - (3.0/UM_RADIAN)))
	{
		if (!merged)
		{
/*
.....Rotate plane so the angles correspond to the correct normal
.....vector.
*/
			*cang = UM_HALFPI + ang; 
			ang = *eang - *sang;
			um_cross(svec,tvec,nvec);
			um_unitvc(nvec,nvec);
			if (ang <= 357.)
			{
				*sang -= 1.5;
				*eang += 1.5;
			}
			ang = -7.0/UM_RADIAN;
			*cang += (8.0/UM_RADIAN);
			um_vcplvc(refpt,svec,ptx);
			um_rotatept(ptx,nvec,refpt,ang,UU_TRUE,&mx);
			um_vcmnvc(ptx,refpt,svec);
			um_unitvc(svec,svec); um_vctovc(svec,csvec);
		}
		else
		{
			*sang = 0;
			*eang = 360;
			if (ang < 0.) ang *= -1.;
			*cang = ang + (3.0/UM_RADIAN);
			ang = -2.0/UM_RADIAN;
			um_vctovc(ci.svec,csvec);
			um_vctovc(ci.nvec,nvec);
			um_vcplvc(refpt,csvec,ptx);
			um_rotatept(ptx,nvec,refpt,ang,UU_TRUE,&mx);
			um_vcmnvc(ptx,refpt,csvec);
		}
	}
/*
.....Do not change the vectors for hemispheres since the circle and curve
.....cannot avoid intersecting.
*/
	else
	{
		*sang = 0.0;
		*eang = 360.0;
		*cang = UM_PI;
		um_vctovc(svec,csvec);
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    I_FUNCTION     :  S_get_cylcv(cvkey,ncvs,cyl,ln,sang,eang,sphfl)
**       Returns a line to revolve for a cylinder and the height of
**       the cylinder.  The starting and ending angles of the cylinder
**       will also be calculated based on the size of the boundary
**       curve.
**    PARAMETERS   
**       INPUT  : 
**          cvkey    Boundary curve keys.
**          ncvs     Number of boundary curves.  This variable
**                   is required in case the "outer" boundary
**                   is one end of the cylinder and the "inner"
**                   boundary is the other end.
**          cyl      Cylinder canonical data.
**          sphfl    UU_TRUE: Input was a sphere so only find angles.
**                   UU_FALSE: Input was not a sphere.
**       OUTPUT : 
**          ncvs     Could be set to 0 if boundary curves are
**                   removed to make an untrimmed surface.
**          cyl[7]   Cylinder height.
**          ln       Line on cylinder to revolve.
**          sang     Starting angle of cylinder.
**          eang     Ending angle.
**    RETURNS      : UU_FAILURE if could not calculate parameters.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_cylcv(cvkey,ncvs,cyl,ln,sang,eang,sphfl)
UU_KEY_ID *cvkey;
int *ncvs;
UU_REAL cyl[];
UM_coord ln[];
UU_REAL *sang,*eang;
UU_LOGICAL sphfl;
{
	int i,nint,npts,status,ipt,inc;
	UU_LOGICAL first,def,rev;
	UU_REAL dis,mindis,maxdis,tol,ang,um_angle2p_acc(),preang,minang,maxang/*, tangl*/;
	UU_REAL fudge,sgn;
	char sbuf[80];
	UM_coord spt,ept,cpt,*pts,ptx,mspt,mept;
	UM_vector svec,rvec,tvec,evec,prevec;
	UM_transf tfmat;
	UU_LIST ptlist;
	struct UM_crvdatabag crv;
	struct UM_rotmatrix mx;
	struct UM_compcrv_rec *comp;
/*
.....Initialize routine
*/
	first = UU_TRUE;
	def = UU_FALSE;
	*sang = 0.;
	*eang = 0.;
	preang = 0.;
	mindis = minang = 1.e12;
	maxdis = maxang = -1.e12;
	fudge = .001;
	if (cyl[8] > UM_PI/4.) fudge = .0001;
	if (cyl[8] > UM_PI/3.) fudge = .0;
	um_nullvc(svec);
	ipt = 0;
	cyl[7] = 0.;
	gettol(&tol);
	uu_list_init(&ptlist,sizeof(UM_coord),200,200);
/*
....Get boundary curve
*/
/*	while (ipt < 2 && ipt < *ncvs && cyl[7] <= .002) */
	while (ipt < 2 && ipt < *ncvs)	// remove check cyl height, no geom sense, causes subsequent errors, Sasha, June16, 2017
	{
		crv.key = cvkey[ipt];
		ncl_retrieve_data_fixed(&crv);
		uc_retrieve_transf (crv.key,tfmat);
/*
.....Evolve points on the curve
*/
		UU_LIST_EMPTY(&ptlist);
		npts = ncl_evolve_curve(&crv,tfmat,tol,&ptlist,UU_NULL,UU_NULL,0);
		if (npts < 3) goto failed;
		pts = (UM_coord *)UU_LIST_ARRAY(&ptlist);
/*
.....Determine direction to go around the cone
.....If the first line ends at the apex point
.....then reverse the direction
*/
		rev = UU_FALSE;
		if (cyl[8] != 0.)
		{
			um_nptpln(pts[1],&cyl[0],&cyl[3],ptx);
			dis = um_dcccc(ptx,&cyl[0]);
			if (dis <= UM_FUZZ) rev = UU_TRUE;
		}
/*
.....Get the start point of line
.....and starting and ending angles
*/
		for (i=0;i<npts;i++)
		{
			inc = i;
			if (rev) inc = npts - i - 1;
			um_nptpln(pts[inc],&cyl[0],&cyl[3],ptx);
/*
........Calculate starting point of line
*/
			if (!def)
			{
				dis = um_dcccc(ptx,&cyl[0]);
				if (dis > UM_FUZZ)
				{
					um_vcmnvc(ptx,&cyl[0],svec);
					um_unitvc(svec,svec);
					um_vctovc(svec,prevec);
					um_translate_point(&cyl[0],cyl[6],svec,spt);
					minang = maxang = 0.;
					def = UU_TRUE;
				}
			}
/*
........Calculate ending angle
*/
			else if (ipt == 0)
			{
				um_vcmnvc(ptx,&cyl[0],evec);
				if (um_mag(evec) > UM_FUZZ)
				{
					um_unitvc(evec,evec);
					ang = um_angle2p_acc(prevec,evec,&cyl[3]);
					if (ang > UM_PI) ang = ang - UM_TWOPI;
					if (ang == UM_PI && minang == maxang)
					{
						minang = maxang = preang = 0.;
						um_vctovc(evec,svec);
					}
					else
					{

						preang += ang;
						if (preang < minang) minang = preang;
						if (preang > maxang) maxang = preang;
					}
					um_vctovc(evec,prevec);
				}
				else
					fudge = 0.;
			}
/*
.....Calculate height of cylinder
*/
			um_vcmnvc(pts[inc],&cyl[0],rvec);
			dis = um_dot(rvec,&cyl[3]);
			if (first)
			{
				mindis = dis;
				maxdis = dis;
			}
			else
			{
				if (dis < mindis) mindis = dis;
				if (dis > maxdis) maxdis = dis;
			}
			first = UU_FALSE;
		}
		cyl[7] = fabs(cyl[7]);	// Sasha, June 15, 2017
/*
		cyl[7] = maxdis-mindis;
if (cyl[7] >= .002 && cyl[7] < .01)
*/
/*
{
		dis = dis;
}
*/
		ipt++;
	}
/*
.....2 Boundary curves were given
.....One on each side of the cylinder
*/
	if (ipt == 2 && *ncvs == 2) *ncvs = 0;
/*
.....Move center of cylinder and
.....start point of line to lower level of boundary curve
.......Not sure why sgn was used - ASF 7/2/13.
*/
//	sgn = mindis < 0 ? -1 : 1; mindis = mindis + fudge*sgn;
//	sgn = maxdis < 0 ? -1 : 1; maxdis = maxdis + fudge*sgn;
	mindis -= fudge;
	maxdis += fudge;
	*sang = minang * UM_RADIAN;
	*eang = maxang * UM_RADIAN;
//TEMP
/*      if (*sang < -0.001)
	{
		*sang = *sang + 360.;
		*eang = *eang + 360.;
	}
*/							// The only effect - all angles do not go to 720 around range
								// Comm out Sasha, June 28, 2017
/*
   if (*sang >= -.001 && *sang <= .001) *sang = 0.;
   if (*eang >= 359.999 && *eang <= 360.001) *eang = 360.;
   if (*sang < -0.001)
   {
      *sang = *sang + 360.;
      *eang = *eang + 360.;
   }
   if (*eang - *sang <= 358.)
   {
      *sang = *sang - 1; *eang = *eang + 1;
   }
*/
/*
sprintf(sbuf,"Sgn = %lf   Angles = %lf,%lf\n",sgn,*sang,*eang);
uig_list_out(sbuf,UU_FALSE);
*/
/*
.....Store line
*/
	um_vctovc(spt,ln[0]);
	if (!sphfl)
	{
/*
........For cylinder
*/
		if (cyl[8] == 0.)
		{
			um_translate_point(&cyl[0],mindis,&cyl[3],&cyl[0]);
			um_translate_point(spt,mindis,&cyl[3],ln[0]);
			um_translate_point(spt,maxdis,&cyl[3],ln[1]);
		}
/*
........For cone
*/
		else
		{
			um_cross(&cyl[3],svec,rvec);
			um_unitvc(rvec,rvec);
			um_nullvc(cpt);
			um_vctovc(&cyl[3],tvec);
			um_rotatept(tvec,rvec,cpt,cyl[8],UU_TRUE,&mx);
			um_translate_point(spt,mindis,&cyl[3],ept);
			um_ilnpln(spt,tvec,ept,&cyl[3],&nint,ln[0]);
			um_translate_point(spt,maxdis,&cyl[3],ept);
			um_ilnpln(spt,tvec,ept,&cyl[3],&nint,ln[1]);
		}
	}
	cyl[7] = fabs(cyl[7]);
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to generate curve
*/
failed:;
	status = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	uu_list_free(&ptlist);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_get_torang(cvkey,cpt,nvec,svec,spt,sang,eang,flag)
**       Calculates the initial vector, starting angle, and ending
**       angle of a torus or generic surface of revolution based on the
**       boundary curve of the surface.
**    PARAMETERS   
**       INPUT  : 
**          cvkey    Boundary curve key.
**          cpt      Torus center point.
**          nvec     Normal vector of torus.
**          svec     Starting vector of torus.
**          flag     UU_TRUE = Use input 'svec' as starting vector.
**                   UU_FALSE = Calculate starting vector from boundary curve.
**       OUTPUT : 
**          svec     Starting vector of torus.
**          spt      Starting point of minor circle.
**          sang     Starting angle of torus.
**          eang     Ending angle.
**    RETURNS      : UU_FAILURE if could not calculate parameters.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_torang(cvkey,cpt,nvec,svec,spt,sang,eang,flag)
UU_KEY_ID cvkey;
UM_coord cpt;
UM_vector nvec,svec;
UM_coord spt;
UU_REAL *sang,*eang;
UU_LOGICAL flag;
{
	int i,j,nc,npts,status,inc;
	UU_LOGICAL first;
	UU_REAL dis,tol,ang,um_angle2p_acc(),preang,minang,maxang,inang,mindis;
	char sbuf[80];
	UM_coord *pts,ptx;
	UM_vector evec,prevec,invec;
	UM_transf tfmat;
	UU_LIST ptlist;
	struct UM_crvdatabag crv;
/*
....Get boundary curve
*/
	crv.key = cvkey;
	ncl_retrieve_data_fixed(&crv);
	uc_retrieve_transf (crv.key,tfmat);
/*
.....Evolve points on the curve
*/
	gettol(&tol);
	uu_list_init(&ptlist,sizeof(UM_coord),200,200);
	npts = ncl_evolve_curve(&crv,tfmat,tol,&ptlist,UU_NULL,UU_NULL,0);
	if (npts < 3) goto failed;
/*
.....Initialize loop
*/
	first = UU_FALSE;
	*sang = 0.;
	*eang = 0.;
	preang = 0.;
	minang = 1.e12;
	maxang = -1.e12;
	um_vctovc(svec,prevec);
	um_vctovc(svec,invec);
	pts = (UM_coord *)UU_LIST_ARRAY(&ptlist);
/*
.....Reorder points based on
.....closest to axis of revolution
*/
	inc = 0;
	if (flag)
	{
		mindis = 10000.;
		for (i=0;i<npts;i++)
		{
			um_nptln(pts[i],cpt,nvec,ptx);
			dis = um_dcccc(pts[i],ptx);
			if (dis < mindis)
			{
				inc = i;
				mindis = dis;
			}
		}
	}
/*
.....Loop through points
.....to get starting and ending vectors
*/
	nc = npts;
	for (j=0;j<2;j++)
	{
		for (i=inc;i<nc;i++)
		{
			um_nptpln(pts[i],cpt,nvec,ptx);
/*
........Calculate starting point of line
*/
			if (!first)
			{
				dis = um_dcccc(ptx,cpt);
				if (dis > UM_FUZZ)
				{
					um_vctovc(pts[i],spt);
					um_vcmnvc(ptx,cpt,evec);
					um_unitvc(evec,evec);

/*
					inang = um_angle2p_acc(prevec,evec,nvec);
					if (inang > UM_PI) inang = inang - UM_TWOPI;
*/
					if (flag)
					{
						ang = um_angle2p_acc(prevec,evec,nvec);
						if (ang > UM_PI) ang = ang - UM_TWOPI;
						preang += ang;
						if (preang < minang) minang = preang;
						if (preang > maxang) maxang = preang;
					}
					else
					{
						minang = maxang = 0.;
						um_vctovc(evec,svec);
					}
					um_vctovc(evec,prevec);
					first = UU_TRUE;
				}
			}
/*
........Calculate ending angle
*/
			else
			{
				um_vcmnvc(ptx,cpt,evec);
				um_unitvc(evec,evec);
				ang = um_angle2p_acc(prevec,evec,nvec);
				if (ang > UM_PI) ang = ang - UM_TWOPI;
				preang += ang;
				if (preang < minang) minang = preang;
				if (preang > maxang) maxang = preang;
/*
				ang = um_angle2p_acc(svec,evec,nvec);
				if (ang > UM_PI) ang = ang - UM_TWOPI;
*/
				if (maxang-minang >= UM_TWOPI)
				{
					maxang = minang + UM_TWOPI;
					break;
				}
				um_vctovc(evec,prevec);
			}
		}
		if (inc == 0) break;
		inc = nc;
		nc = npts;
	}

/*
	if (flag)
	{
		minang = minang + inang;
		maxang = maxang + inang;
		um_vctovc(invec,svec);
	}
*/

	*sang = minang * UM_RADIAN;
	*eang = maxang * UM_RADIAN;
	if (*sang >= -.001 && *sang <= .001) *sang = 0.;
	if (*eang >= 359.999 && *eang <= 360.001) *eang = 360.;
	if (*sang < -0.001)
	{
		*sang = *sang + 360.;
		*eang = *eang + 360.;
	}
	if (*eang - *sang <= 358.)
	{
		*sang = *sang - 1; *eang = *eang + 1;
	}
/*
sprintf(sbuf,"Sgn = %lf   Angles = %lf,%lf\n",sgn,*sang,*eang);
uig_list_out(sbuf,UU_FALSE);
*/
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to generate curve
*/
failed:;
	status = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	uu_list_free(&ptlist);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_calc_outer_boundary(cvkey,ncvs,nvec,cpt,
**                         sftype,rad,merged,index,flag)
**       Determines which of the trimming curves is the actual
**       outer boundary curve, when all curves have been designated
**       as inner boundary curves.  May merge boundary curves if two
**       are outer boundary curves.
**    PARAMETERS   
**       INPUT  : 
**          cvkey    Boundary curve keys.
**          ncvs     Number of boundary curves.
**          nvec     Axisi of rotation for revolved surface.
**          cpt      Origin point for nvec.
**          sftype   Type of surface.
**          rad      Major radius of torus. (if surface is a torus)
**          flag     Type of connection search to use
**       OUTPUT : 
**          cvkey[0] Outer boundary curve.
**          ncvs     Updated number of boundary curves.
**          merged   UU_TRUE: Curves were merged.
**                   UU_FALSE: Curves not merged.
**          index    Index of first connecting geometry added to cv.
**    RETURNS      : UU_FAILURE if could not calculate outer boundary
**                   curve.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_calc_outer_boundary(cvkey,ncvs,nvec,cpt,sftype,rad,merged,
	index,flag)
UU_KEY_ID *cvkey;
UM_vector nvec;
int *ncvs,*index,flag;
UM_coord cpt;
UTP_command_type sftype;
UU_REAL rad;
UU_LOGICAL *merged;
{
	int i,inc1,inc2,status,n;
	UU_REAL box[6],mbox[6],tol;
	UU_LOGICAL contained,nullvc;
/*
.....Initialize routine
*/
	gettol(&tol);
	status = UU_FAILURE;
	contained = UU_TRUE;
	*merged = UU_FALSE;
	nullvc = UM_MAG(nvec) < UM_DFUZZ;
	mbox[0] = mbox[1] = mbox[2] = 100000.;
	mbox[3] = mbox[4] = mbox[5] = -100000.;
/*
.....Store initial bounding box
*/
	S_calc_box(cvkey[1],box,nvec);
	inc1 = 1;
	inc2 = 1;
	um_vctovc(&box[0],&mbox[0]);
	um_vctovc(&box[3],&mbox[3]);

	if (*ncvs>2)
	{
		inc1 = 1;
	}

/*
.....Calculate bounding boxes of all curves
*/
	for (i=2;i<=*ncvs;i++)
	{
		if (cvkey[i] != 0)
		{
			S_calc_box(cvkey[i],box,nvec);
			if (nullvc)
			{
				if (box[0]-tol <= mbox[0] &&  box[1]-tol <= mbox[1] &&
					box[2]-tol <= mbox[2] && box[3]+tol >= mbox[3] &&
					box[4]+tol >= mbox[4] && box[5]+tol >= mbox[5])
				{
					inc1 = i;
					inc2 = i;
					um_vctovc(&box[0],&mbox[0]);
					um_vctovc(&box[3],&mbox[3]);
				}
			}
			else if (sftype != PLANE)
			{
				if (box[2]+tol < mbox[2])
				{
					inc1 = i;
					um_vctovc(&box[0],&mbox[0]);
				}
			}
			if (sftype != PLANE)
			{
				if (box[5]-tol > mbox[5])
				{
					inc2 = i;
					um_vctovc(&box[3],&mbox[3]);
				}
			}
		}
	}
/*
.....Store new outer boundary curve
*/
	if (inc1 == inc2)
	{
		cvkey[0] = cvkey[inc1];
		for (i=inc1;i<*ncvs;i++) cvkey[i] = cvkey[i+1];
		if (*ncvs>1)
			*ncvs = *ncvs - 1;
		else
			*ncvs = 1;
		status = UU_SUCCESS;
	}
/*
.....Merge two boundary curves into one boundary curve.
*/
	else
	{
		status = S_single_boundary(cvkey,ncvs,nvec,cpt,sftype,rad,inc1,inc2,index,
			flag);
		if (status == UU_SUCCESS) *merged = UU_TRUE;
	}
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_calc_box(cvkey,box,nvec)
**       Calculates a bounding box around an XYZ boundary curve.
**.......Modified to translate curves to planes parallel to xy-plane
**.......ASF 7/2/13.
**    PARAMETERS   
**       INPUT  : 
**          cvkey    Key of curve to calculate box for.
**          nvec     Normal vector for revolved surface.(Pass in null
**                   vector to not use vector logic)
**       OUTPUT : 
**          box      Bounding box of curve.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_calc_box(cvkey,box,nvec)
UU_KEY_ID cvkey;
UU_REAL *box;
UM_vector nvec;
{
	int i,ntim;
	UU_REAL u,uinc;
	UM_transf tfmat,tf,tfi;
//	struct UM_crvdatabag crv;
	struct UM_rbsplcrv_rec crv;
	struct UM_evcrvout evout;
	UM_coord ppt;
	UM_vector xvec,yvec,zvec;
/*
.....If cvkey is not a valid key, should not continue
.....use the maxinum box
.....otherwise, it will give memory error later
.....Yurong
*/
	if (cvkey<=0)
	{
		box[0] = box[1] = box[2] = 100000.;
		box[3] = box[4] = box[5] = -100000.;
		return;
	}
/*
.....Set up transform.  The transform will move curves to plane
.....parallel to xy-plane.
*/
	um_unitvc(nvec,nvec);
	if (UM_MAG(nvec) > 0)
	{
		um_nullvc(ppt);
		um_perpvc (nvec,xvec); um_unitvc(xvec,xvec);
		um_ptzx_tf(ppt,nvec,xvec,tf); um_inverttf(tf,tfi);
	}
	else
		um_identtf(tfi);
/*
....Get boundary curve
*/
	crv.key = cvkey;
	ncl_retrieve_data_fixed(&crv);
	uc_retrieve_transf (crv.key,tfmat);
	uc_init_evcrvout (&crv,&evout);
/*
.....Initialize box
*/
	u = 0.0;
	uc_evcrv(UM_POINT,u,&crv,tfmat,&evout);
	um_cctmtf(evout.cp,tfi,ppt);
	um_vctovc(ppt,&box[0]); um_vctovc(ppt,&box[3]);
/*
.....Calculate a plane from the end points and reference point
*/
	ntim = 1000;
	uinc = 1. / (UU_REAL)ntim;
	ntim--;
	for (i=0;i<ntim;i++)
	{
		u = u + uinc;
		uc_evcrv(UM_POINT,u,&crv,tfmat,&evout);
		um_cctmtf(evout.cp,tfi,ppt);
		if (ppt[0] < box[0]) box[0] = ppt[0];
		if (ppt[1] < box[1]) box[1] = ppt[1];
		if (ppt[2] < box[2]) box[2] = ppt[2];
		if (ppt[0] > box[3]) box[3] = ppt[0];
		if (ppt[1] > box[4]) box[4] = ppt[1];
		if (ppt[2] > box[5]) box[5] = ppt[2];
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    I_FUNCTION     :  S_calc_uvbox(cvkey,box,nvec)
**       Calculates a bounding box around an XYZ boundary curve.
**.......Modified to translate curves to planes parallel to xy-plane
**.......ASF 7/2/13.
**    PARAMETERS   
**       INPUT  : 
**          cvkey    Key of curve to calculate box for.
**          nvec     Normal vector for revolved surface.(Pass in null
**                   vector to not use vector logic)
**       OUTPUT : 
**          box      Bounding box of curve.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_calc_uvbox(cvkey,box,nvec)
UU_KEY_ID cvkey;
UU_REAL *box;
UM_vector nvec;
{
	int i,ntim;
	UU_REAL u,uinc;
	UM_transf tfmat,tf,tfi;
	struct UM_rbsplcrv_rec crv;
	struct UM_evcrvout evout;
	UM_coord ppt;
	UM_vector xvec,yvec,zvec;

/*
.....Set up transform.  The transform will move curves to plane
.....parallel to xy-plane.
*/
	um_unitvc(nvec,nvec);
	if (UM_MAG(nvec) > 0)
	{
		um_nullvc(ppt);
		um_perpvc (nvec,xvec); um_unitvc(xvec,xvec);
		um_ptzx_tf(ppt,nvec,xvec,tf); um_inverttf(tf,tfi);
	}
	else
		um_identtf(tfi);
/*
.....Get boundary curve
.......Make sure the ends of the curve are valid.
*/
	crv.key = cvkey;
	ncl_retrieve_data_fixed(&crv);
	if (!(crv.t0 < 0. || crv.t0 > 0. || crv.t0 == 0.)) return UU_FAILURE;
	if (!(crv.t1 < 0. || crv.t1 > 0. || crv.t1 == 0.)) return UU_FAILURE;
	uc_retrieve_transf (crv.key,tfmat);
	uc_init_evcrvout (&crv,&evout);
/*
.....Initialize box
*/
	u = 0.0;
	uc_evcrv(UM_POINT,u,&crv,tfmat,&evout);
	if (evout.cp[0] < -0.001 || evout.cp[0] > 1.001 || 
		evout.cp[1] < -0.001 || evout.cp[1] > 1.001)	return UU_FAILURE;
	um_cctmtf(evout.cp,tfi,ppt);
	um_vctovc(ppt,&box[0]); um_vctovc(ppt,&box[3]);
/*
.....Calculate a plane from the end points and reference point
.......Make sure the points used are valid.
*/
	ntim = 1000;
	uinc = 1. / (UU_REAL)ntim;
	for (i=0;i<ntim;i++)
	{
		u = u + uinc;
		uc_evcrv(UM_POINT,u,&crv,tfmat,&evout);
		um_cctmtf(evout.cp,tfi,ppt);
		if (evout.cp[0] < -0.001 || evout.cp[0] > 1.001 || 
			evout.cp[1] < -0.001 || evout.cp[1] > 1.001)	return UU_FAILURE;
		if (ppt[0] < box[0]) box[0] = ppt[0];
		if (ppt[1] < box[1]) box[1] = ppt[1];
		if (ppt[2] < box[2]) box[2] = ppt[2];
		if (ppt[0] > box[3]) box[3] = ppt[0];
		if (ppt[1] > box[4]) box[4] = ppt[1];
		if (ppt[2] > box[5]) box[5] = ppt[2];
	}
/*
.....End of routine
*/
done:;
	return UU_SUCCESS;
}

/*********************************************************************
**    I_FUNCTION     :  S_calc_outer_uvboundary(cvkey,ncvs,nvec,cpt,
**                         sftype,rad,merged,index)
**       Determines which of the trimming curves is the actual
**       outer boundary curve, when all curves have been designated
**       as inner boundary curves.  May merge boundary curves if two
**       are outer boundary curves.
**    PARAMETERS   
**       INPUT  : 
**          cvkey    Boundary curve keys.
**          ncvs     Number of boundary curves.
**       OUTPUT : 
**          none
**    RETURNS      : UU_FAILURE if inner boundaries not contained
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_calc_outer_uvboundary(cvkey,ncvs)
UU_KEY_ID *cvkey;
int ncvs;
{
	int i,j,status;
	struct UM_compcrv_rec *ccv;
	struct UC_entitydatabag crv;
	UU_REAL box[6],tbox[6],mbox[6],tol;
	UM_vector nvec;
/*
.....Initialize routine
*/
	um_nullvc(nvec);
	status = UU_FAILURE;
	box[0] = box[1] = box[2] = mbox[0] = mbox[1] = mbox[2] = 100000.;
	box[3] = box[4] = box[5] = mbox[3] = mbox[4] = mbox[5] = -100000.;
/*
.....Calculate bounding boxes of all curves
*/
	for (i=0;i<ncvs;i++)
	{
		if (cvkey[i] != 0)
		{
			crv.key = cvkey[i];
			ncl_retrieve_data_fixed(&crv);
			if (crv.rel_num == UM_COMPCRV_REL)
			{
				ccv = (struct UM_compcrv_rec *)&crv;
				for (j=0;j<ccv->no_cid;j++)
				{
					status = S_calc_uvbox(ccv->cid[j].crvid,tbox,nvec);
					if (status != UU_SUCCESS) return status;
					if (tbox[0] < box[0]) box[0] = tbox[0];
					if (tbox[1] < box[1]) box[1] = tbox[1];
					if (tbox[2] < box[2]) box[2] = tbox[2];
					if (tbox[3] > box[3]) box[3] = tbox[3];
					if (tbox[4] > box[4]) box[4] = tbox[4];
					if (tbox[5] > box[5]) box[5] = tbox[5];
				}
			}
			else
				status = S_calc_uvbox(crv.key,box,nvec);
			if (i == 0 ||
				(box[0]-UM_FUZZ <= mbox[0] && box[1]-UM_FUZZ <= mbox[1] &&
				box[2]-UM_FUZZ <= mbox[2] && box[3]+UM_FUZZ >= mbox[3] &&
				box[4]+UM_FUZZ >= mbox[4] && box[5]+UM_FUZZ >= mbox[5]))
			{
				um_vctovc(&box[0],&mbox[0]);
				um_vctovc(&box[3],&mbox[3]);
			}
			else if (box[0]+UM_FUZZ >=  mbox[0] &&  box[1]+UM_FUZZ >= mbox[1] &&
				box[2]+UM_FUZZ >= mbox[2] && box[3]-UM_FUZZ <= mbox[3] &&
				box[4]-UM_FUZZ <= mbox[4] &&	box[5]-UM_FUZZ <= mbox[5])
			{
				continue;
			}
			else
				return UU_FAILURE;
		}
	}
	return UU_SUCCESS;
}

/*********************************************************************
**    I_FUNCTION     :  S_fix_composite_circles(cvkey,skey,tol)
**       Fixes any circles in a composite curve whose directions appear
**       to be backwards, causing the complement of the desired circle
**			to be created.
**    PARAMETERS   
**       INPUT  : 
**          cvkey    List of composite curves.
**          type     Type of surface being defined (to determine type of errors
**                   to look for).
**          tol      Tolerance to use for comparisons.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fix_composite_circles(cvkey,type,tol)
UU_KEY_ID cvkey;
UTP_command_type type;
UU_REAL tol;
{
	int i,j,k,relnum[2],inc,sflag,chg,iend,stat;
	UU_LOGICAL rev[2];
	UM_coord cpt;
	struct UM_crvdatabag cv1,cv2;
	struct UM_compcrv_rec *comp;
	struct UM_circle_rec c[2];
/*
.....Get composite curve
*/
	cv1.key = cvkey;
	ncl_retrieve_data_fixed(&cv1);
	if (cv1.rel_num != UM_COMPCRV_REL) return;
	comp = (struct UM_compcrv_rec *)&cv1;
/*
.....See if there are any 180 degree circles
*/
	sflag = 0;
	for (i=0;i<comp->no_cid;i++)
	{
		ur_retrieve_data_relnum(comp->cid[i].crvid,&relnum[0]);
		if (relnum[0] == UM_CIRCLE_REL)
		{
			c[0].key = comp->cid[i].crvid;
			ncl_retrieve_data_fixed(&c[0]);
			rev[0] = comp->cid[i].reverse;
/*
........Found 180 degree circle
*/
			if (fabs(c[0].dang-UM_PI) <= UM_FUZZ)
			{
/*
...........First pass check for consecutive circles/splines
...........Second pass check for alternating circles/splines
*/
				inc = 0;
				iend = 2;
				if ((type != CYLINDRICAL_SURFACE && type != CONICAL_SURFACE &&
					type != SPHERICAL_SURFACE && type != SURFACE_OF_REVOLUTION &&
					type != TOROIDAL_SURFACE) && comp->no_cid != 4) iend = 1;
				for (k=0;k<iend;k++)
				{
					chg = 1;
					inc++;
					for (j=i-inc;j<=i+inc;j=j+inc*2)
					{
						if (j < 0 || j >= comp->no_cid) continue;
						ur_retrieve_data_relnum(comp->cid[j].crvid,&relnum[1]);
/*
..............Found consecutive circle
*/
						if (relnum[1] == UM_CIRCLE_REL)
						{
							c[1].key = comp->cid[j].crvid;
							ncl_retrieve_data_fixed(&c[1]);
							rev[1] = comp->cid[j].reverse;
							if (j<i || fabs(c[1].dang-UM_PI) > UM_FUZZ) chg = 0;
							if (rev[0] == 1 && rev[1] == 1) chg = 0;
						}
/*
..............Found consecutive spline
..............Convert it into a circle for further testing
*/
						else if (relnum[1] == UM_RBSPLCRV_REL)
						{
							cv2.key = comp->cid[j].crvid;
							ncl_retrieve_data_fixed(&cv2);
							stat = S_create_cir(&cv2,&c[1]);
							if (stat != UU_SUCCESS) continue;
							rev[1] = comp->cid[j].reverse;
							chg = 0;
						}
						else
							continue;
/*
..............Found consecutive circle
*/
						um_nptpln(c[0].center,c[1].center,c[1].nvec,cpt);
						if (um_cceqcc_tol(c[1].center,cpt,tol) &&
							um_vcparall(c[0].nvec,c[1].nvec))
						{
							sflag = inc;
							break;
						}
					}
					if (sflag != 0) break;
				}
				if (sflag != 0) break;
			}
		}
	}
/*
.....Found circles to check
.....Adjust circle if necessary
*/
	if (sflag != 0)
	{
		if (rev[0] != rev[1]) sflag = 3 - sflag;
/*
........Circle vectors should be the same
*/
		if (sflag == 1 && !um_cceqcc(c[0].nvec,c[1].nvec))
		{
			um_vctovc(c[1-chg].nvec,c[chg].nvec);
			ur_update_data_fixed(c[chg]);
		}
/*
........Circles should be complementary
*/
		else if (sflag == 2 && um_cceqcc(c[0].nvec,c[1].nvec))
		{
			um_vctmsc(c[chg].nvec,-1.,c[chg].nvec);
			ur_update_data_fixed(c[chg]);
		}
	}
}

/*********************************************************************
**    I_FUNCTION     :  S_create_cir(cv,cir)
**       Builds a circle from a B-spline curve.  Used for testing 180
**       degree circle directions within a composite curve.
**    PARAMETERS   
**       INPUT  : 
**          cv       B-spline curve to convert to approximate circle.
**       OUTPUT :
**          cir      Calculated circle.
**    RETURNS      : UU_SUCCESS if circle was built.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_create_cir(cv,cir)
struct UM_rbsplcrv_rec *cv;
struct UM_circle_rec *cir;
{
	int status;
	UM_coord p1,p2,p3;
	UM_transf tfmat;
	struct UM_evcrvout evout;
/*
.....Initialize evaluator
*/
	uc_retrieve_transf(cv->key,tfmat);
	uc_init_evcrvout(cv,&evout);
/*
.....Get three points from curve to build circle from
*/
	uc_evcrv(UM_POINT,0.,cv,tfmat,&evout); um_vctovc(evout.cp,p1);
	uc_evcrv(UM_POINT,.5,cv,tfmat,&evout); um_vctovc(evout.cp,p2);
	uc_evcrv(UM_POINT,1.,cv,tfmat,&evout); um_vctovc(evout.cp,p3);
/*
.....Build circle
*/
	status = um_c3_3pt1(0,p1,p2,p3,cir);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_cctou(eptr)
**       Finds the u parameter for a point known to lie on the given
**       closed composite curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr     Composite curve.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS if a match is found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_cctou(eptr,tfmat,cc,u)
struct UM_compcrv_rec *eptr;
UM_transf tfmat;
UM_coord cc;
UM_param *u;
{
	int status;
	int i;
	struct UC_entitydatabag cons;
	UU_LOGICAL inside = UU_FALSE,found = UU_FALSE;
	UM_param cu, cu0, cu1;
	UM_length cdist,dmin;

	dmin = 1.e12;
	cu0 = 0.0;
	for (i=0; i<eptr->no_cid; i++)
	{
		cu1 = eptr->cid[i].endparam;
		cons.key = eptr->cid [i].crvid;
		status = uc_retrieve_data(&cons, sizeof(cons));
		if (status == UU_SUCCESS)
		{
			status = uc_cctou(&cons, tfmat, cc, &cu, &cdist);
			if (status == UU_SUCCESS)
			{
				if (cdist < dmin && cu > -UM_FUZZ && cu < 1.+UM_FUZZ)
				{
					dmin = cdist;
					if (cu > 0. && cu < 1.) inside = UU_TRUE;
					if (eptr->cid[i].reverse)
						*u = cu1 - ((cu1 - cu0) * cu);
					else
						*u = cu0 + ((cu1 - cu0) * cu);
					found = UU_TRUE;
				}
			}
			cu0 = cu1;
		}
	}

	if (found) status = UU_SUCCESS;
}

/*********************************************************************
**    I_FUNCTION     :  S_check_direction(crv,nvec)
**       Performs a simple check to see if the component curve is
**       too close to vertical to be a valid component to use for
**       connecting the two curves.  Note that this check may fail
**       when the boundary curve component has an irregular shape.
**       This test is intended to find the general direction of
**       basic curves used to trim a primitive type surface.
**    PARAMETERS   
**       INPUT  : 
**          crv      Curve.
**          nvec     Axis of rotation for surface curve is on.
**       OUTPUT :
**          none
**    RETURNS      : UU_TRUE component is far enough from vertical.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL S_check_direction(crv,nvec)
struct UM_crvdatabag *crv;
UM_vector nvec;
{
	int status;
	UM_coord p1,p2,p3;
	UM_transf tfmat;
	UM_vector dvec;
	UU_REAL dot;
//	UU_REAL maxdot = 0.996195; /* approx.  5 degrees */
	UU_REAL maxdot = 0.984808; /* approx. 10 degrees */
	struct UM_evcrvout evout;
/*
.....Initialize evaluator
*/
	uc_retrieve_transf(crv->key,tfmat);
	uc_init_evcrvout(crv,&evout);
/*
.....Get three points from curve to build direction vectors
*/
	uc_evcrv(UM_POINT,0.,crv,tfmat,&evout); um_vctovc(evout.cp,p1);
	uc_evcrv(UM_POINT,.5,crv,tfmat,&evout); um_vctovc(evout.cp,p2);
	uc_evcrv(UM_POINT,1.,crv,tfmat,&evout); um_vctovc(evout.cp,p3);
	um_vcmnvc(p2,p1,dvec); um_unitvc(dvec,dvec);
	dot = UM_DOT(dvec,nvec);
	if (fabs(dot) > maxdot) return UU_FALSE;
	um_vcmnvc(p3,p2,dvec); um_unitvc(dvec,dvec);
	dot = UM_DOT(dvec,nvec);
	if (fabs(dot) > maxdot) return UU_FALSE;
	return UU_TRUE;
}

/*********************************************************************
**    I_FUNCTION     :  S_find_connect_points(cvkey,pt1,pt2)
**       Finds the closet points on two curves that define the
**       boundary of a revolved surface.
**    PARAMETERS   
**       INPUT  : 
**          crv1     First curve.
**          crv2     Second curve.
**          nvec     Axis of rotation for surface curves are on.
**          cpt      Origin of nvec.
**          torfl    Surface is toroidal.
**          flag     Type of point finding to use
**       OUTPUT :
**          pt1      Point on CV1.
**          pt2      Point on CV2.
**          u1       u parameter for point on CV1.
**          u2       u parameter for point on CV2.
**          use_nvec Use nvec to define the plane for the connection.
**    RETURNS      : UU_SUCCESS if points are found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_find_connect_points(crv1,crv2,nvec,cpt,pt1,pt2,u1,u2,
	torfl,use_nvec,flag)
struct UM_crvdatabag *crv1,*crv2;
UM_vector nvec;
UM_coord cpt,pt1,pt2;
UU_REAL *u1,*u2;
UU_LOGICAL torfl,*use_nvec;
int flag;
{
	int i,j,n,n1,n2,ind,ind1,ind2,status,nint1,nint2,ptind;
	struct UM_crvdatabag tcv;
	struct UM_compcrv_rec *ccv,*ccv1,*ccv2;
	UM_transf tfmat1,tfmat2;
	UU_LIST ptlist1,ptlist2;
	UU_REAL u,tol,dis,dis2,mindis,len,maxlen;
	UM_coord *pts,*pts1,*pts2,pt,ipt1,ipt2,tpt1,tpt2;
	UM_vector pvec,plvec,lvec1,lvec2,xvec,dvec;
	UU_LOGICAL on_line1,on_line2,keep,use_pt,vecfl;
	struct UM_evcrvout evout;
	char sbuf[80];
	
	*use_nvec = UU_FALSE;
	status = UU_SUCCESS;
	gettol(&tol);
	ccv1 = (struct UM_compcrv_rec *)crv1; ccv2 = (struct UM_compcrv_rec *)crv2;
	uu_list_init0(&ptlist1);
	uu_list_init0(&ptlist2);
/*
.....Find the best component to use.
*/
	if (flag == 1 && (ccv1->no_cid > 1 || ccv2->no_cid > 1))
	{
/*
.....Find the longest component that is not in the same direction as nvec.
*/
		maxlen = -1.e10;
		for (i=0;i<2;i++)
		{
			if (i == 0) ccv = ccv1;
			else ccv = ccv2;
			if (ccv->no_cid <= 1) continue;
			for (j=0;j<ccv->no_cid;j++)
			{
				tcv.key = ccv->cid[j].crvid;
				ncl_retrieve_data_fixed(&tcv);
				len = um_getarclen(&tcv,UM_idmat);
				if (len > maxlen)
				{
					keep = S_check_direction(&tcv,nvec);
					if (keep)
					{
						ind1 = i;
						ind2 = j;
						maxlen = len;
					}
				}
			}
		}
		if (ind1 == 1)
		{
			ccv = ccv1;
			ccv1 = ccv2;
			ccv2 = ccv;
		}
/*
.....Use the midpoint of the longest segment found.
*/
		tcv.key = ccv1->cid[ind2].crvid; ncl_retrieve_data_fixed(&tcv);
		uc_retrieve_transf (tcv.key,tfmat1);
		uc_init_evcrvout(&tcv,&evout);
		uc_evcrv(UM_POINT,0.5,&tcv,tfmat1,&evout);
		use_pt = UU_TRUE;	n1 = 1; um_vctovc(evout.cp,pt1);
	}
/*
.....Find closest pair of points.
*/
	else
	{
		use_pt = UU_FALSE;
		uu_list_init(&ptlist1,sizeof(UM_coord),200,200);
		uc_retrieve_transf (ccv1->key,tfmat1);
		n1 = ncl_evolve_curve(ccv1,tfmat1,tol,&ptlist1,UU_NULL,UU_NULL,0);
		if (n1 < 2) goto failed;
		pts1 = (UM_coord *)UU_LIST_ARRAY(&ptlist1);
	}
	uu_list_init(&ptlist2,sizeof(UM_coord),200,200);
	uc_retrieve_transf (ccv2->key,tfmat2);
	n2 = ncl_evolve_curve(ccv2,tfmat2,tol,&ptlist2,UU_NULL,UU_NULL,0);
	if (n2 < 2) goto failed;
	pts2 = (UM_coord *)UU_LIST_ARRAY(&ptlist2);
/*
.....Find the closest pair of points
*/
	mindis = 1.e12;
	vecfl = UM_MAG(nvec) > UM_FUZZ;
	for (i=0;i<n1 && mindis>0.0;i++)
	{
		if (!use_pt) um_vctovc(pts1[i],pt1);
		for (j=0;j<n2 && mindis>0.0;j++)
		{
			if (vecfl)
			{
				um_nptpln(pts2[j],pt1,nvec,pt2);
				dis = um_dcccc(pt1,pt2);
			}
			else
				dis = um_dcccc(pt1,pts2[j]);
			if (dis < mindis)
			{
				mindis = dis;
				ind1 = i;
				ind2 = j;
				if (mindis < UM_DFUZZ) break;
			}
		}
		if (mindis < UM_DFUZZ) break;
	}
	if (!use_pt) um_vctovc(pts1[ind1],pt1);
	um_vctovc(pts2[ind2],pt2);
/*
.....If Torus check to see if both points line on
.....plane defined by cpt and nvec
.....If so, then set flag
*/
	if (torfl)
	{
		um_nptpln(pt1,cpt,nvec,tpt1); um_nptpln(pt2,cpt,nvec,tpt2);
		dis = um_dcccc(pt1,tpt1); dis2 = um_dcccc(pt2,tpt2);
		if (dis <= tol && dis2 <= tol) *use_nvec = UU_TRUE;
	}
/*
.....Refine second point based on normal vector if sftype is
.....cone or cylinder.
........The following code should no longer be needed
........As it is taken care of above by comparing planar points
........Bobby - 04/28/15
*/
/*
	if (UM_MAG(nvec) > UM_DFUZZ)
	{
*/
/*
.....Slice curve with plane perp to geometry plane through pt1 to
.....refine pt2 and ensure points line up with nvec - ASF 11/13/13.
*/
/*
		um_vcmnvc(pt1,cpt,xvec); um_unitvc(xvec,xvec);
		um_vcortho(nvec,xvec); um_unitvc(xvec,xvec);
		um_cross(xvec,nvec,plvec); um_unitvc(plvec,plvec);
		mindis = 1.e12;
		for (j=0;j<n2-1;j++)
		{
			um_vcmnvc(pts2[j+1],pts2[j],dvec); um_unitvc(dvec,dvec);
			um_ilnpln(pts2[j],dvec,pt1,plvec,&nint1,tpt1);
			if (nint1 > 0)
			{
				dis = um_dcccc(pt2,tpt1);
				if (dis < mindis)
				{
					mindis = dis;
					um_vctovc(tpt1,tpt2);
				}
			}
		}
		if (mindis < 1.e12) um_vctovc(tpt2,pt2);
		um_nptpln(pt1,cpt,nvec,tpt1); um_nptpln(pt2,cpt,nvec,tpt2);
		if (torfl)
		{
			dis = um_dcccc(pt1,tpt1); dis2 = um_dcccc(pt2,tpt2);
*/
/*
.....It is likely in this case that the two curves are on planes perpendicular
.....to the one defined by nvec and cpt.
*/
/*
			if (dis <= tol && dis2 <= tol)
			{
				*use_nvec = UU_TRUE;
				goto getu;
			}
		}
	}
*/
getu:
	S_cctou(ccv1,tfmat1,pt1,u1);
	S_cctou(ccv2,tfmat2,pt2,u2);
	if (*u1 < UM_FUZZ) *u1 = 0.;
	if (*u1+UM_FUZZ > 1.) *u1 = 1.;
	if (*u2 < UM_FUZZ) *u2 = 0.;
	if (*u2+UM_FUZZ > 1.) *u2 = 1.;
	goto done;
/*
.....Failed to generate curve points
*/
failed:;
	status = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	if (!use_pt) uu_list_free(&ptlist1);
	uu_list_free(&ptlist2);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_debug_compcrv(comp,ind,newfl)
**       Prints composite curve records to list file.
**    PARAMETERS   
**       INPUT  : 
**          comp     Pointer to composite curve record.
**          ind      Boundary curve index.
**          newfl    Newly defined curve flag.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS if a match is found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_debug_compcrv(comp,ind,newfl)
struct UM_compcrv_rec *comp;
int ind;
UU_LOGICAL newfl;
{
	int i;
	char sbuf[80];
	struct UM_crvdatabag tcv;

	if (newfl)
		sprintf(sbuf,"\nComponents of NEW curve with key = %d\n",comp->key);
	else
		sprintf(sbuf,"\nComponents of BNDR curve %d with key = %d\n",ind,comp->key);
	uig_list_out(sbuf,UU_FALSE);
	for (i=0;i<comp->no_cid;i++)
	{
		tcv.key = comp->cid[i].crvid;
		ncl_retrieve_data_fixed(&tcv);
		sprintf(sbuf,"\tComponent %d has key = %d, rel = %d and rev = %d\n",i+1,
			comp->cid[i].crvid,tcv.rel_num,comp->cid[i].reverse);
		uig_list_out(sbuf,UU_FALSE);
	}
}

/*********************************************************************
**    I_FUNCTION     :  S_single_boundary(cvkey,ncvs,nvec,cpt,sftype,
**                          rad,inc1,inc2,index,flag)
**       Create a single composite curve out of two boundary curves
**       on a surface of revolution.
**    PARAMETERS   
**       INPUT  : 
**          cvkey    Key list for curves to merge.
**          nvec     Rotation axis for surface the curves are on.
**          cpt      Origin of nvec.
**          sftype   Type of surface curve will be projected on.
**          rad      Major radius of torus (if surface is a torus).
**          inc1     Pointer into 'cvkey' of first curve to merge.
**          inc2     Pointer into 'cvkey' of second curve to merge.
**          flag     0 = Use closest points for finding connection
**                   1 = Use longest component
**       OUTPUT :
**          ncvs     Updated number of boundary curves.
**          index    Index of first entity created to connect the cvs.
**                   (Used when building revolved surface)
**    RETURNS      : UU_SUCCESS if single curve was built.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_single_boundary(cvkey,ncvs,nvec,cpt,sftype,rad,inc1,inc2,index,
	flag)
UU_KEY_ID cvkey[];
int *ncvs,inc1,inc2,*index,flag;
UM_vector nvec;
UM_coord cpt;
UTP_command_type sftype;
UU_REAL rad;
{
	int status,no_cid,i,n;
	UM_coord pt1,pt2,tcpt,tpt;
	UM_vector tnvec,tvec1,tvec2;
	UU_REAL u1,u2,dis;
	struct UM_compcrv_rec *comp,ccv;
	struct UM_crvdatabag crv1,crv2;
	struct UM_line_rec ln1,ln2;
	struct UM_circle_rec ci1,ci2;
	struct UC_attributedatabag attr;
	char label[NCL_MAX_LABEL+1];
	UU_LIST key_list,rev_list;
	UU_KEY_ID *keys,key;
	UU_LOGICAL sphfl,torfl,trev,*revs,use_nvec,init=UU_FALSE;

	strcpy(label,"@UN");
	attr.key = crv1.key = cvkey[inc1];
	ur_retrieve_attr(&attr);
	ncl_retrieve_data_fixed(&crv1);
	crv2.key = cvkey[inc2];
	ncl_retrieve_data_fixed(&crv2);
	um_vctovc(nvec,tnvec); um_unitvc(tnvec,tnvec);
	torfl = (sftype == TOROIDAL_SURFACE);
	sphfl = (sftype == SPHERICAL_SURFACE || torfl);
	status = S_find_connect_points(&crv1,&crv2,tnvec,cpt,pt1,pt2,&u1,&u2,
		torfl,&use_nvec,flag);
	if (status != UU_SUCCESS) goto failed;
	no_cid = 4; /* make room for two split components and two connections */
	comp = (struct UM_compcrv_rec *)&crv1;
	no_cid += comp->no_cid;
	comp = (struct UM_compcrv_rec *)&crv2;
	no_cid += comp->no_cid;
	init = UU_TRUE;
	uu_list_init(&key_list,sizeof(UU_KEY_ID),no_cid,10);
	uu_list_init(&rev_list,sizeof(UU_KEY_ID),no_cid,10);
	no_cid = 0;
/*
.....Trim component on first curve at closest point.
*/
//if (T_DEBUV) S_debug_compcrv(&crv1,1,UU_FALSE);
	status=S_trim_store(&crv1,u1,tnvec,&key_list,&rev_list,1,&no_cid);
	if (status != UU_SUCCESS) goto failed;
/*
.....Create lines/circles in opposite directions connecting closest points.
*/
	if (!sphfl)
	{
		ln1.key = utp_store_line(label,pt1,pt2);
		if (ln1.key == 0) goto failed;
		uu_list_push(&key_list,&(ln1.key));
		ln2.key = utp_store_line(label,pt2,pt1);
		if (ln2.key == 0) goto failed;
		no_cid++;
	}
	else
	{
		if (torfl && !use_nvec)
		{
			um_nptpln(pt1,cpt,tnvec,tpt);
			um_vcmnvc(tpt,cpt,tvec1); um_unitvc(tvec1,tvec1);
			um_translate_point(cpt,rad,tvec1,tcpt);
		}
		else
			um_vctovc(cpt,tcpt);
		ur_setup_data(UM_CIRCLE_REL,&ci1,sizeof(struct UM_circle_rec));
		ur_setup_data(UM_CIRCLE_REL,&ci2,sizeof(struct UM_circle_rec));
		um_vctovc(tcpt,ci1.center); um_vctovc(tcpt,ci2.center);
		um_vcmnvc(pt1,tcpt,tvec1); um_unitvc(tvec1,tvec1);
		um_vcmnvc(pt2,tcpt,tvec2); um_unitvc(tvec2,tvec2);
		um_vctovc(tvec1,ci1.svec); um_vctovc(tvec2,ci2.svec);
		um_cross(tvec1,tvec2,ci1.nvec); um_unitvc(ci1.nvec,ci1.nvec);
		um_cross(tvec2,tvec1,ci2.nvec); um_unitvc(ci2.nvec,ci2.nvec);
		ci1.radius = ci2.radius = um_dcccc(pt1,tcpt);
		ci1.dang = ci2.dang = um_angle2p(tvec1,tvec2,ci1.nvec);
		ci1.key = utp_store_circle(&ci1,label);
		if (ci1.key == 0) goto failed;
		uu_list_push(&key_list,&(ci1.key));
		ci2.key = utp_store_circle(&ci2,label);
		if (ci2.key == 0) goto failed;
		no_cid++;
	}
	trev = UU_FALSE;
	uu_list_push(&rev_list,&(trev));
	*index = no_cid-1;
	if (torfl && use_nvec) *index = -1;
/*
.....Trim component on second curve at closest point.
*/
//if (T_DEBUV) S_debug_compcrv(&crv2,2,UU_FALSE);
	status=S_trim_store(&crv2,u2,tnvec,&key_list,&rev_list,2,&no_cid);
	if (status != UU_SUCCESS) goto failed;
/*
.....Push final line or circle key onto list.
*/
	if (!sphfl) uu_list_push(&key_list,&(ln2.key));
	else uu_list_push(&key_list,&(ci2.key));
	trev = UU_FALSE;
	uu_list_push(&rev_list,&(trev));
	no_cid++;
/*
.....Create new composite curve with trimmed components and new lines.
*/
	keys = (int *)UU_LIST_ARRAY(&key_list);
	revs = (UU_LOGICAL *)UU_LIST_ARRAY(&rev_list);
	ccv.key = 0;
	ur_setup_data(UM_COMPCRV_REL, &ccv, sizeof(struct UM_compcrv_rec));
	ccv.subscr = 0;
	ccv.cid = (struct UM_cid_rec *)uu_malloc(sizeof(struct UM_cid_rec)*no_cid);
	for (i=0;i<no_cid;i++) 
	{
		ccv.cid[i].crvid = keys[i];
		if (revs[i] == 1) ccv.cid[i].crvid *= -1;
	}
	ccv.no_cid = no_cid;
	umi_fix_subcurve_fields(&ccv, UM_DEFAULT_TF);
	ccv.planar = UU_FALSE;
	ccv.continuity = 0;
	ccv.open = UU_FALSE;
	ccv.t0 = 0.;
	ccv.t1 = 1.;
	ccv.addflg = 0;
	strcpy(ccv.label,label);
	status = um_create_geom(&ccv,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	if (status != UU_SUCCESS) goto failed;
//if (T_DEBUV) S_debug_compcrv(&ccv,1,UU_TRUE);
	attr.key = ccv.key;
	attr.displayable = UM_NEVERDISPLAYABLE;
	ur_update_attr(&attr);
	ncl_retrieve_data_fixed(&ccv);
	cvkey[0] = ccv.key;
/*
.....Store inner boundary curves
*/
	n = 1;
	for (i=1;i<*ncvs+1;i++)
	{
		if (i != inc1 && i != inc2)
			cvkey[n++] = cvkey[i];
	}
	*ncvs = n;
	goto done;
failed:
	status = UU_FAILURE;
done:
	if (init)
	{
		uu_list_free(&key_list);
		uu_list_free(&rev_list);
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_trim_store(crv,u,nvec,key_list,rev_list,
**                          cvno,no_cid)
**       Trim a component of a curve and stores the crvids and reverse
**       flags for the modified curve.
**    PARAMETERS   
**       INPUT  : 
**         crv        Curve to split.
**         u          Parameter to split curve at.
**         nvec       Revolved surface normal vector.
**         cvno       Index for curve. Used to determine direction.
**       OUTPUT :
**         key_list   List of all keys for combined curve.
**         rev_list   Reversal flag for all curves in combined curve.
**         no_cid     Update number of components in combined curve.
**    RETURNS      : UU_SUCCESS if trim is successful.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_trim_store(crv,u,nvec,key_list,rev_list,cvno,no_cid)
struct UM_crvdatabag *crv;
UU_REAL u;
UU_LIST *key_list,*rev_list;
int cvno,*no_cid;
UM_vector nvec;
{
	int ind,i,j,k,status,size,added,half;
	UU_LOGICAL rev,reverse,trev,push,*revs;
	struct UM_crvdatabag tcv,*part1,*part2;
	struct UM_compcrv_rec *comp;
	struct UC_attributedatabag attr;
	UU_REAL uu0,uu1,delp,tu,uu[2],len1,len2,len3;
	char label[NCL_MAX_LABEL+1];
	char sbuf[80];
	UU_LIST tkeys,trevs;
	UU_KEY_ID *keys,tkey1,tkey2,tkey;
		
	added = status = 0;
	strcpy(label,"@UN");
	part1 = part2 = UU_NULL;
	trev = rev = UU_FALSE;
	push = UU_TRUE;
	if (u > 1.0) u -= 1.0;
	if (u < 0.0) u += 1.0;
	tu = u;
	uu_list_init(&tkeys,sizeof(UU_KEY_ID),20,20);
	uu_list_init(&trevs,sizeof(UU_KEY_ID),20,20);
	reverse = S_check_reverse(crv,nvec);
/*
.....Determine the component to trim.
*/
	if (crv->rel_num == UM_COMPCRV_REL)
	{
		comp = (struct UM_compcrv_rec *)crv;
/*
if (T_DEBUV)
{
	sprintf(sbuf,"\nNumber of components = %d\n",comp->no_cid);
	uig_list_out(sbuf,UU_FALSE);
	sprintf(sbuf,"\nOriginal Curve Data\n");
	uig_list_out(sbuf,UU_FALSE);
	for (i=0;i<comp->no_cid;i++)
	{
		if (comp->cid[i].reverse)
			sprintf(sbuf,"\tKey[%d] = %d - Reversed\n",i,comp->cid[i].crvid);
		else
			sprintf(sbuf,"\tKey[%d] = %d\n",i,comp->cid[i].crvid);
		uig_list_out(sbuf,UU_FALSE);
	}
}
*/
		ind = 0;
		while(ind<comp->no_cid && u>comp->cid[ind].endparam) ind++;
		tcv.key = comp->cid[ind].crvid;
		if (ind > 0) uu0 = comp->cid[ind-1].endparam;
		else uu0 = 0.0;
		uu1 = comp->cid[ind].endparam;
		tu = (u - uu0)/(uu1 - uu0);
		rev = comp->cid[ind].reverse;
		if (rev) tu = 1. - tu;
		if ((rev == UU_FALSE && fabs(1.-tu)<UM_FUZZ)||
			(rev == UU_TRUE && fabs(tu)<UM_FUZZ))
			push = UU_FALSE;
	}
	else 
	{
		tcv.key = crv->key;
		rev = UU_FALSE;
	}
	ncl_retrieve_data_fixed(&tcv);
/*
.....Split the component if necessary.
*/
	if (tu > UM_DFUZZ && tu < 1.0 - UM_DFUZZ)
	{
		size = um_curve_size (&tcv);
		attr.key = tcv.key;
		ur_retrieve_attr(&attr);
		status = um_allocate_curve (&part1,size);
		status += um_allocate_curve (&part2,size);
		if (status != UU_SUCCESS) goto failed;
		delp = (rev)? 1. : 0.;
		uu[0] = tu; uu[1] = -1.;
		status = um_splitcurve(&tcv, uu, &delp, part1, part2);
		if (status != UU_SUCCESS) goto failed;
		strcpy(part1->label,label); strcpy(part2->label,label);
		status = um_create_geom(part1,UM_DEFAULT_TF,&attr);
		status += um_create_geom(part2,UM_DEFAULT_TF,&attr);
		if (status != UU_SUCCESS) goto failed;
		attr.key = part1->key;
		attr.displayable = UM_NEVERDISPLAYABLE;
		ur_update_attr(&attr);
		attr.key = part2->key;
		attr.displayable = UM_NEVERDISPLAYABLE;
		ur_update_attr(&attr);
		len1 = um_getarclen(part1,UM_idmat);
		len2 = um_getarclen(part2,UM_idmat);
/*
if (T_DEBUV)
{
	sprintf(sbuf,"\tPART1 KEY = %d\n",part1->key);
	uig_list_out(sbuf,UU_FALSE);
	sprintf(sbuf,"\tPART1 KEY = %d\n",part2->key);
	uig_list_out(sbuf,UU_FALSE);
}
*/
		if (rev)
			uu_list_push(&tkeys,&(part1->key));
		else
			uu_list_push(&tkeys,&(part2->key));
		uu_list_push(&trevs,&(rev));
	}
/*
.....Trim not necessary since u is at end of component so push
.....the component onto the stack if u dictates it should be.
*/
	else if (push)
	{
		uu_list_push(&tkeys,&(tcv.key));
		uu_list_push(&trevs,&(rev));
		added++;
	}
/*
.....Store remaining components starting after split component.
*/
	if (crv->rel_num == UM_COMPCRV_REL && comp->no_cid > 1)
	{
		for (j=1;j<comp->no_cid;j++)
		{
			k = ind + j;
			if (k >= comp->no_cid) k -= comp->no_cid;
			trev = comp->cid[k].reverse;
			uu_list_push(&tkeys,&(comp->cid[k].crvid));
			uu_list_push(&trevs,&(trev));
			added++;
		}
	}
/*
.....Push the second portion of the split component.
*/
	if (tu > UM_DFUZZ && tu < 1.0 - UM_DFUZZ)
	{
		if (rev)
			uu_list_push(&tkeys,&(part2->key));
		else
			uu_list_push(&tkeys,&(part1->key));
		uu_list_push(&trevs,&(rev));
		added += 2;
	}
/*
.....Push the component if it was not split and the u parameter
.....dictates that it should be the last in the new component list.
*/
	else if (!push)
	{
		uu_list_push(&tkeys,&(tcv.key));
		uu_list_push(&trevs,&(rev));
		added++;
	}
/*
.....Push the modified lists onto the main list.
*/
	keys = (int *)UU_LIST_ARRAY(&tkeys);
	revs = (UU_LOGICAL *)UU_LIST_ARRAY(&trevs);
	k = added-1;
	if (cvno == 2) reverse = !reverse;
/*
if (T_DEBUV)
{
	sprintf(sbuf,"\nKey list after split and store:\n");
	uig_list_out(sbuf,UU_FALSE);
	for (i=0;i<added;i++)
	{
		if (revs[i])
			sprintf(sbuf,"\tKey[%d] = %d - Reversed\n",i,keys[i]);
		else
			sprintf(sbuf,"\tKey[%d] = %d\n",i,keys[i]);
		uig_list_out(sbuf,UU_FALSE);
	}
	if (reverse) sprintf(sbuf,"\nREVERSED\n");
	else sprintf(sbuf,"\nNOT REVERSED\n");
	uig_list_out(sbuf,UU_FALSE);

	sprintf(sbuf,"\nModified Curve Data\n");
	uig_list_out(sbuf,UU_FALSE);
}
*/
	for (j=0;j<added;j++)
	{
		if (reverse)
		{
			tcv.key = keys[k];
			ncl_retrieve_data_fixed(&tcv);
			len1 = um_getarclen(&tcv,UM_idmat);
			tkey = keys[k];
			trev = !revs[k];
			k--;
		}
		else
		{
			tcv.key = keys[j];
			ncl_retrieve_data_fixed(&tcv);
			len1 = um_getarclen(&tcv,UM_idmat);
			tkey = keys[j];
			trev = revs[j];
		}
/*
if (T_DEBUV)
{
	if (trev)
		sprintf(sbuf,"\tKey[%d] = %d - Reversed (%lf)\n",j,tkey,len1);
	else
		sprintf(sbuf,"\tKey[%d] = %d (%lf)\n",j,tkey,len1);
	uig_list_out(sbuf,UU_FALSE);
}
*/
		uu_list_push(rev_list,&(trev));
		uu_list_push(key_list,&(tkey));
	}
	*no_cid = *no_cid + added;
failed:
	if (part1 != UU_NULL) uu_toolfree ((struct UM_crvdatabag *)part1);
	if (part2 != UU_NULL) uu_toolfree ((struct UM_crvdatabag *)part2);
	uu_list_free(&tkeys);
	uu_list_free(&trevs);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  S_check_reverse(comp,nvec)
**       Checks to see if a composite curve should be reverseved when
**       combining two curves to make a single boundary curve on a
**       revolved surface.
**    PARAMETERS   
**       INPUT  : 
**         comp     Entity to find orientation of.
**         nvec     Axis of rotation for the surface the curve is on.
**       OUTPUT :
**         none
**    RETURNS      : UU_TRUE if curve is CLW with respect to nvec.
**                   UU_FASLE if curve is CCLW with respect to nvec.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_check_reverse(comp,nvec)
struct UM_crvdatabag *comp;
UM_vector nvec;
{
	int orient;
	UU_LOGICAL reverse;
/*
.....Get the orientation.
*/
	orient = um_polygon_orientation3D(comp,nvec);
	reverse = (orient > 0)? UU_FALSE : UU_TRUE;
	return(reverse);
}

/*********************************************************************
**    I_FUNCTION     :  S_check_match(eptr)
**       Checks for an exact match with an existing entity in the
**       unibase.
**    PARAMETERS   
**       INPUT  : 
**          eptr     Entity to search for a match of.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS if a match is found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_check_match(eptr)
struct NCL_fixed_databag *eptr;
{
	int status;
	switch(eptr->rel_num)
	{
		case UM_RBSPLCRV_REL:
			status = uig_match_rbsplcrv(eptr,0);
			break;
		case UM_COMPCRV_REL:
			status = uig_match_compcrv(eptr,0);
			break;
		case UM_CIRCLE_REL:
			status = uig_match_circle(eptr,0);
			break;
		case UM_LINE_REL:
			status = uig_match_line(eptr,0);
			break;
		default:
			status = UU_FAILURE;
			break;
	}
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  S_create_curves(cvkey,ncvs)
**       Builds and stores copies of trimmed surface boundary curves
**       and their component curves based on UIG_splitccrv flag.
**    PARAMETERS   
**       INPUT  : 
**          cvkeys   Boundary curve key list.
**          ncvs     Number of curve keys.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS if curves were built.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_create_curves(cvkeys,ncvs,recno)
UU_KEY_ID *cvkeys;
int ncvs,recno;
{
	int i,j,isize,status;
	struct UM_compcrv_rec ccv;
	struct UC_attributedatabag attr;
	struct NCL_fixed_databag comp,copy;
	struct UM_line_rec *ln;
	char sbuf[80];

	status = UU_SUCCESS;
	for (i=0;i<ncvs;i++)
	{
		ccv.key = cvkeys[i];
		ncl_retrieve_data_fixed(&ccv);
/*
.....Create boundary curves only if UIG_splitccrv = 0.
.....Create boundary curve components only if UIG_splitccrv = 1.
.....Create boundary curves and components if UIG_splitccrv = 2.
*/
		if (UIG_splitccrv > 0)
		{
			for (j=0;j<ccv.no_cid;j++)
			{
				attr.key = copy.key = comp.key = ccv.cid[j].crvid;
				ur_retrieve_attr(&attr);
				ncl_retrieve_data_fixed(&comp);
				if (UIG_nodups && S_check_match(&comp) == UU_SUCCESS)
				{
					UIG_dupcount++;
					continue;
				}
				ncl_retrieve_data_fixed(&copy);
/*
........Don't copy zero length lines
*/
				if (copy.rel_num == UM_LINE_REL)
				{
					ln = (struct UM_line_rec *)&copy;
					if (um_dcccc(ln->spt,ln->ept) < UM_FUZZ) continue;
				}
/*
........Copy entity
*/
				copy.key = 0;
				isize = sizeof(&comp);
				status = uc_copy(&comp,&copy,isize);
				if (status != UU_SUCCESS) goto done;
				attr.key = copy.key;
				attr.displayable = UM_DISPLAYABLE;
				ur_update_attr(&attr);
				utp_create_copy_label(copy.rel_num,copy.label,&(copy.subscr));
				ur_update_data_fixed(&copy);
				utp_count_translated(copy.rel_num,1,UU_TRUE);
			}
		}
		if (UIG_splitccrv == 0 || UIG_splitccrv == 2)
		{
			if (UIG_nodups && S_check_match(&ccv) == UU_SUCCESS)
			{
				UIG_dupcount++;
				continue;
			}
			attr.key = copy.key = ccv.key;
			ur_retrieve_attr(&attr);
			ncl_retrieve_data_fixed(&copy);
			copy.key = 0;
			isize = sizeof(&ccv);
			status = uc_copy(&ccv,&copy,isize);
			if (status != UU_SUCCESS) goto done;
			attr.key = copy.key;
			attr.displayable = UM_DISPLAYABLE;
			ur_update_attr(&attr);
			utp_create_copy_label(copy.rel_num,copy.label,&(copy.subscr));
			ur_update_data_fixed(&copy);
			utp_count_translated(copy.rel_num,1,UU_TRUE);
		}
	}
done:
	return(status);
}

