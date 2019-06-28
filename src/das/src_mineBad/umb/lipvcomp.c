/*********************************************************************
**    NAME         :  lipvcomp.c
**       CONTAINS:
**				ul_ipv_compare()
**				ul_ipv_compare_mod()
**				ul_ipv_compare_tol()
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvcomp.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       06/17/15 , 13:19:18
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "dselmask.h"
#include "lcom.h"
#include "lipv.h"
#include "lipvmach.h"
#include "mfort.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "mdpick.h"
#include "mdrel.h"
#include "mgeom.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclfile.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"

/*#define DEBUG_T 1*/

#define FTYP 0
#define FSEL 1
#define FHIL 2
#define FDES 3
#define FBRO 4
#define FSTL 5
#define FUNI 6
#define FMFL 7
#define FMNM 8
#define FMLD 9
#define FMMD 10
#define FVIS 11
#define FVIW 12
#define FMOD 13
#define FFRM 14
#define FTRA 15
#define FSTA 16
#define FFAC 17
#define FFND 18
#define FCOM 19
#define FLOG 20
#define FCLR 21
#define FPRG 22
#define FITR 23

#define FAUT 0
#define FTOL 1
#define FRES 2
#define FTOL1 3
#define FCOL1 4
#define FTOL2 5
#define FCOL2 6
#define FTOL3 7
#define FCOL3 8
#define FTOL4 9
#define FCOL4 10
#define FCOL5 11
#define FTOL5 12
#define FCOL6 13
#define FTOL6 14
#define FCOL7 15
#define FTOL7 16
#define FCOL8 17
#define FTOL8 18
#define FCOL9 19

#define FGRD 0
#define FSFT 1
#define FANG 2
#define FDEV 3
#define FVOL 5

#define FGOU 3

#define SURF_TOG 0
#define STL_TOG 1
#define SHAPE_TOG 2
#define INCH_TOG 0
#define MM_TOG 1

typedef struct
{
	UU_KEY_ID key;
	int relnum;
	char label[96];
	int color;
} S_geo;

typedef struct
{
	UU_REAL toler;
	UU_REAL ucut[2];
	UU_REAL ocut[2];
	LtFace face;
	UU_LOGICAL used;
	int surf;
	int color;
	char label[96];
} S_face;

typedef struct
{
	UU_REAL vol;
	LtDoubleBounds bounds;
	LtSessionPrim prim;
	int type;
	UU_LOGICAL analyzed;
} S_solid;

static int mainmarkval=0;
static int Sfrm,Sfrm1,Sfrm2;
static UU_LOGICAL Sfirst = UU_FALSE, Sinterrupt;
static UU_LOGICAL Sactive = UU_FALSE, Sactive1 = UU_FALSE, Sactive2 = UU_FALSE;
static UU_LOGICAL Sform_active = UU_FALSE;
static int Svis,Sprog;
static int Stype,Scolor,Scol[9],Sunits,Strans;
static UU_REAL Sauto,Stol[8];
static UX_pathname Sfile;
static LtPrim Starget;
static LtSessionPrim Sesstarget,Sessprim;
static UU_LIST Ssurf;
static LtSavedMWEnvironment Senv;
static UU_LIST Sface,Ssolid;
static int Snface=0,Snsolid=-1;
static UD_LIST Sresults;
static UU_LOGICAL Sgrid_flag,Sanalyze;
static int Sgrid;
static UU_REAL Sftol,Smaxang,Smaxdis,Sminvol;
static LtPrim Sprim=0;
static LtDoublePoint **Svert;
static UM_tessellation Stess;
static int *Svert_index=UU_NULL,Snvert;
static int Sndifs=0;
static LtSessionPrim Spdif[2];
static int Sfndsol;
static LtSessionPrim *Ssolptr=UU_NULL;
static int Snptr=0;
static int Smxfrm=0;
static UU_LOGICAL Smxflag=UU_FALSE, Smxchg=UU_FALSE;;
static UU_LOGICAL Sadded;
static char Smxname[NCL_MAX_LABEL_AND_SUBSCRIPT];
static UM_transf Smatrix={1,0,0,0, 0,1,0,0, 0,0,1,0};
static UD_LIST Smxlist;

static UD_FSTAT OnClear(),OnInterrupt();
static void S_add_list();
static void S_clear_list();
static void S_hilite_faces();
static LtStatus S_mesh_begin();
static LtStatus S_mesh_polygon();
static UU_REAL S_compare_point();
static UD_FSTAT ul_ipv_compare_tol();
static UD_FSTAT ul_ipv_compare_mod();
static UD_FSTAT ul_ipv_compare_find();
static UD_FSTAT OnSeg();
static int S_compare_part();
static int S_compare_solids();
static void S_place_solid();
static void S_remove_diff();
static void S_reset_comparison();
/*
......use as Compare choice input since for NCLIPV
......we only support STL file now
*/
static UD_LIST Slist;
static char *Scmptxt[] = {"Surfaces", "STL File", "Shape"};
extern char uw_color_name[64][96];
/*********************************************************************
**    S_FUNCTION     :  OnAnalyze(filedno, val, stat)
**       Performs a Report style analyzation on the faces within the
**       bounds of the current under/over cut.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnAnalyze(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnAuto(filedno, val, stat)
**       Set automatic tolerances.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnAuto(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_REAL atol;
	if (*fieldno!=FAUT) return(UD_FLDOK);
/*
.....Change the default tolerances
.....to increments based on the
.....automatic tolerance
*/
	UM_len_inttoext(Sauto,atol);
	Stol[0] = atol * 4 * -1.;
	ud_dispfrm_update_answer(Sfrm,FTOL1,(int *)&Stol[0]);
	Stol[1] = atol * 3 * -1.;
	ud_dispfrm_update_answer(Sfrm,FTOL2,(int *)&Stol[1]);
	Stol[2] = atol * 2 * -1.;
	ud_dispfrm_update_answer(Sfrm,FTOL3,(int *)&Stol[2]);
	Stol[3] = atol * -1.;
	ud_dispfrm_update_answer(Sfrm,FTOL4,(int *)&Stol[3]);
	Stol[4] = atol * 1;
	ud_dispfrm_update_answer(Sfrm,FTOL5,(int *)&Stol[4]);
	Stol[5] = atol * 2;
	ud_dispfrm_update_answer(Sfrm,FTOL6,(int *)&Stol[5]);
	Stol[6] = atol * 3;
	ud_dispfrm_update_answer(Sfrm,FTOL7,(int *)&Stol[6]);
	Stol[7] = atol * 4;
	ud_dispfrm_update_answer(Sfrm,FTOL8,(int *)&Stol[7]);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnBack(filedno, val, stat)
**       Routine to backspace the gouge finder.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnBack(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Backspace the gouge finder
*/
	if (Sfndsol > 1) Sfndsol = Sfndsol - 1;
	ud_dispfrm_update_answer(Sfrm2,FGOU,(int *)&Sfndsol);
	ud_update_form(Sfrm2);
	OnSeg(fieldno,val,stat);
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnBrowse(filedno, val, stat)
**       Routine to get the STL file name.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnBrowse(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int nc;
	UX_pathname file;
	UU_REAL tol;
	if (*fieldno!=FBRO) return UD_FLDOK;
/*
.....Load the STL file
*/
	if (Starget != 0) LiPrimitiveDestroy(Starget);
	if (Stess.np != 0) um_free_tess(&Stess);
	Starget = 0;
	file[0] = '\0';
	nc = 0;
	tol = .001;
	ul_ipv_import_stl(2,file,&nc,&Starget,Sunits,tol);
	if (strlen(file) != 0)
	{
		strcpy(Sfile,file);
		ud_update_answer(FSTL,(int *)Sfile);
	}
	Smxchg = Smxflag;
/*
.....Apply transformation matrix to STL file
*/
	if (Smxchg)
	{
		S_place_solid(Starget,Smatrix);
		Smxchg = UU_FALSE;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClear(filedno, val, stat)
**       Clears the results list.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClear(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ul_ipv_init_list(&Sresults,10000);
	strcpy(Sresults.answer," ");
	ud_update_answer(FSTA,(int *)&Sresults);
	ud_set_traverse_mask(FLOG,UU_FALSE);
	ud_set_traverse_mask(FCLR,UU_FALSE);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnInterrupt(fieldno, val, stat)
**       Clears the results list.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnInterrupt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	Sinterrupt = UU_TRUE;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose()
**       Method called when Colors form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose()
{
	Sactive = UU_FALSE;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose1()
**       Method called when Tolerances form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose1()
{
	Sactive1 = UU_FALSE;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose2()
**       Method called when Find form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose2()
{
	Sactive2 = UU_FALSE;
/*
.....Display main form
*/
	ud_form_vis();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnColor(fieldno, val, stat)
**			Color change callback.  Changes the color of all selected
**			surfaces.
**    PARAMETERS
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Current field value.
**			         stat    = Not used.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnColor(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,nc;
	struct NCL_fixed_databag e;
	S_geo *geo;
	UU_LIST *sfpt;
/*
.....Call the default method
.....This causes the answer field to be updated
*/
	ud_default_method(fieldno, val, stat);
	sfpt = &Ssurf;
/*
.....Change the color of all entities
.....in this list
*/
	nc = UU_LIST_LENGTH(sfpt);
	geo = (S_geo *) UU_LIST_ARRAY(sfpt);
	for (i=0;i<nc;i++)
	{
		e.key = geo[i].key;
		ncl_update_color(e.key,Scolor);
		uc_retrieve_data(&e, sizeof(e));
		uc_display(&e);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnCompare(fieldno, val, stat)
**       Performs the actual NCLIPV visual solid compare.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnCompare(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,ntr,status,nc,nucut,nocut,npts,perc,ifl,nstock[2];
	int usav,vsav,nsurf;
	UU_LOGICAL upd;
	char buf[UX_MAX_PATH_LEN],sbuf[80],ubuf[20];
	UU_REAL tsav,bsav,ucut[2],ocut[2],rval,rval1,tol;
	UU_REAL um_get_tess_toler(),ncl_get_boundary_toler();
	UU_LIST triangles,list;
	UM_int2 ifl35,ifl86;
	UM_coord trp[3],*opts,origpt;
	UM_vector axisvc;
	UM_trian *tri;
	UM_transf tfmat;
	UM_tess_settype ttyp;
	Gwpoint3 *pts;
	struct NCL_fixed_databag e;
	S_geo *geo;
	LW_stock_struc *sd,*sdtmp;
	LtMaterial mat[9];
	LtData stuff;
	LtPrim prim1;
	LtSessionPrim revprof;
	UM_tess_settype gtyp;
	S_face *tface;
	LtAttributeClass class;
	LtComparison scomp;
#ifdef DEBUG_T
	int stims,stimm,etims,etimm,ttim;
	char tbuf[80];
#endif
/*
.....Initialize routine
*/
	if (*fieldno!=FCOM) return(UD_FLDOK);
	um_linear_units_str(UM_cpln.length_unit,ubuf);
	Svert_index = UU_NULL;
	if (LW_mach_mode == LW_VISICUT && LW_mach_type == LW_LATHE)
		ul_ipv_lathe_stop();
	Sprog = 0;
	Sinterrupt = UU_FALSE;
	ud_update_answer(FPRG,&Sprog);
	ud_update_form(0);
/*
.....When going to a Report style comparison
.....from a Undercut/Overcut comparison
.....we need to start from scratch
*/
	if (Sndifs != 0 && Svis == 0)
	{
		S_reset_comparison();
	}
/*
.....Get rid of any previous difference solids
*/
	else
	{
		S_remove_diff();
		if (Snsolid != -1) uu_list_free(&Ssolid);
		Snsolid = -1;
	}
/*
.....Initialize target part for comparisons
........STL File
*/
	if (Stype == STL_TOG)
	{
		if (Starget == 0)
		{
			nc = 0;
			tol = .001;
			ul_ipv_import_stl(2,Sfile,&nc,&Starget,Sunits,tol);
			if (Starget == 0) goto notarget;
			Smxchg = Smxflag;
		}
/*
........Apply transformation matrix to STL file
*/
		if (Smxchg)
		{
			S_place_solid(Starget,Smatrix);
			Smxchg = UU_FALSE;
		}
	}
/*
........Surfaces
*/
	else if (Stype == SURF_TOG && Svis == 1)
	{
		if (Starget != 0) LiPrimitiveDestroy(Starget);
		Starget = LiPrimitiveCreateTriangleSolid(LI_MW_SOLID_TYPE_TARGET);
		if (Starget == 0) goto notarget;
	}
/*
........Shape
*/
	else if (Stype == SHAPE_TOG)
	{
		geo = (S_geo *) UU_LIST_ARRAY(&Ssurf);
		if (Starget != 0) LiPrimitiveDestroy(Starget);
		e.key = geo[0].key;
		status = ncl_retrieve_data_fixed(&e);
		if (status != UU_SUCCESS) goto nosurf;
		status = uc_retrieve_transf(geo[0].key,tfmat);
		if (status != UU_SUCCESS) goto nosurf;
		origpt[0] = 0.; origpt[1] = 0.; origpt[2] = 0.;
		axisvc[0] = 1.; axisvc[1] = 0.; axisvc[2] = 0.;
		uu_list_init(&list,sizeof(Gwpoint3),100,100);
		npts = ncl_evolve_shape(&e,tfmat,Sftol,&list);
		pts = (Gwpoint3 *)UU_LIST_ARRAY(&list);
		opts = (UM_coord *)uu_malloc(sizeof(UM_coord)*(npts+2));
		ul_ipv_gen_revsf(origpt,axisvc,pts,&npts,opts,UU_FALSE);
		revprof = LiProfileCreate(opts[0]);
		if (revprof == 0) goto notarget;
		for (i=1;i<npts;i++)
			LiProfileAddLine(revprof,opts[i],FALSE);
		Starget = LiPrimitiveCreateRotatedSolid(revprof,origpt,axisvc,Sftol,
			LI_MW_SOLID_TYPE_TARGET);
		if (Starget == 0) goto notarget;
		uu_free(opts);
		LiProfileDestroy(revprof);
		uu_list_free(&list);
	}
/*
.....Initialize face attributes and distances
.....By changing the color of all faces to
.....largest tolerance value color
*/
	if (Svis != 1)
	{
		if (Snface == 0)
			uu_list_init(&Sface,sizeof(S_face),1000,1000);
		S_hilite_faces();
	}
/*
.....Initialize under/over cut attributes
*/
	if (Svis > 1)
	{
		uu_list_init(&Ssolid,sizeof(S_solid),500,500);
		Snsolid = 0;
	}
/*
.....Get ready for surface tessellation
*/
	if (Stype == SURF_TOG)
	{
		geo = (S_geo *) UU_LIST_ARRAY(&Ssurf);
		ncl_get_tess_parms(&ttyp,&tsav,&usav,&vsav);
		bsav = ncl_get_boundary_toler();
		nsurf = UU_LIST_LENGTH(&Ssurf);
	}
	else
		nsurf = 1;

	if (Stype == SURF_TOG)
	{
/*
.....Create the tessellation
*/
		um_init_tess(&Stess);
		if (Svis == 1) uu_list_init(&triangles,sizeof(UM_trian),500,500);
		if (Sgrid_flag) gtyp = UM_TESS_GRID;
		else gtyp = UM_TESS_BOTH;
		ncl_set_tess_parms(gtyp,Sftol,Sgrid,Sgrid);
		ncl_set_boundary_toler(Sftol);
	}
/*
.....Loop through selected surfaces
*/
#ifdef DEBUG_T
	gtimx(&stims,&stimm);
#endif
	for (i=0;i<nsurf;i++)
	{
/*
.....Get selected surface
*/
		if (Stype == SURF_TOG)
		{
			e.key = geo[i].key;
			status = ncl_retrieve_data_fixed(&e);
			if (status != UU_SUCCESS) goto nosurf;
			status = uc_retrieve_transf(geo[i].key,tfmat);
			if (status != UU_SUCCESS) goto nosurf;
/*
........Reset the tessellation and triangles lists
*/
			um_clean_tess (&Stess);
			UU_LIST_EMPTY (&triangles);
			status = uc_srf_tessellate(&e,&Stess);
			if (status == UU_SUCCESS && Svis == 1)
			{
				status = ncl_get_tess_triangles(&Stess,&triangles,2,0);
			}
			if (status != UU_SUCCESS) goto notess;
/*
.....Add each triangle to the target part
*/
			if (Svis == 1)
			{
				ntr = UU_LIST_LENGTH(&triangles);
				tri = (UM_trian *)UU_LIST_ARRAY(&triangles);
				for (j=0;j<ntr;j++)
				{
					um_vctovc(tri[j].p1,&trp[0]);
					um_vctovc(tri[j].p2,&trp[1]);
					um_vctovc(tri[j].p3,&trp[2]);
					LiTriangleSolidAddFace(Starget,trp);
				}
			}
			ncl_get_label(&e,buf);
			nc = strlen(buf);
			ul_strip_blanks(buf,&nc);
		}
/*
.....Tessellate STL model
*/
		else
		{
			ul_short_filename(Sfile,buf,40);
			nc = strlen(buf);
			if (Svis == 0 && Stess.np == 0 || (Svis != 1 && Sanalyze))
			{
				LiPrimitiveGetSolidMesh(Starget,
					LI_MW_MESH_TRIANGLES|LI_MW_MESH_OUTPUT_PLANES,
						(LtFuncMWMeshBegin)S_mesh_begin,
						(LtFuncMWMeshAddPolygon)S_mesh_polygon,UU_NULL,UU_NULL);
			}
/*
.....Create solid differences
*/
			if (Svis > 1)
			{
				Sndifs = 0;
				Sesstarget=LiSessionAddPrim(LW_session[LW_mach_mode],Starget);
				Sadded = UU_TRUE;
/*
........Gouges
*/
				if (Svis == 2 || Svis == 4)
				{
					Spdif[Sndifs] =
						LiViSolidCreateDifference(Sesstarget,
							LW_stock_first[0]->stock);
					if (Spdif[Sndifs] != 0)
					{
						LiDataSetGenericPtr(&stuff,LW_material[Scol[0]]);
						LiSessionPrimSetVisualProperty(Spdif[Sndifs],
							LI_MW_VIS_PROP_MATERIAL,&stuff);
						Sndifs++;
					}
					else
					{
						sprintf(sbuf,"** Could not calculate Undercuts.");
						ul_ipv_put_list(&Sresults,sbuf);
						strcpy(Sresults.answer,sbuf);
						ud_update_answer(FSTA,(int *)&Sresults);
						if (Svis == 4) Svis = 3;
					}
				}
/*
........Overcuts
*/
				if (Svis == 3 || Svis == 4)
				{
					Spdif[Sndifs] =
						LiViSolidCreateDifference(LW_stock_first[0]->stock,
							Sesstarget);
					if (Spdif[Sndifs] != 0)
					{
						LiDataSetGenericPtr(&stuff,LW_material[Scol[8]]);
						LiSessionPrimSetVisualProperty(Spdif[Sndifs],
							LI_MW_VIS_PROP_MATERIAL,&stuff);
						Sndifs++;
					}
					else
					{
						sprintf(sbuf,"** Could not calculate Overcuts.");
						ul_ipv_put_list(&Sresults,sbuf);
						strcpy(Sresults.answer,sbuf);
						ud_update_answer(FSTA,(int *)&Sresults);
						if (Svis == 4) Svis = 2;
					}
				}
/*
........Add solids to session
*/
				for (j=0;j<Sndifs;j++)
				{
					LiDataSetBoolean(&stuff,TRUE);
					LiMWViewportSetSessPrimProperty(LW_viewport,Spdif[j],
						LI_VPSP_PROP_MW_VISIBLE,&stuff);
/*					LiMachiningSessionAddPrims(NULL,
						LiPrimitiveReferenceCreate(Spdif[j]));*/
				}
			}
		}
/*
.....Manual comparison
*/
		if (Svis == 0)
		{
/*
........Update Progress bar
*/
			if (nsurf > 10)
			{
				perc = (UU_REAL)(i+1) / (UU_REAL)nsurf * 100;
				if ((perc-Sprog) > 1 || perc == 100)
				{
					Sprog = perc;
					ud_update_answer(FPRG,&Sprog);
					ud_update_form(0);
				}
				upd = 0;
			}
			else upd = 1;
			status = S_compare_part(i,buf,&Stess,ucut,ocut,upd);
			if (Sinterrupt) break;
/*
.....Output comparison data
*/
			ud_set_traverse_mask(FLOG,UU_TRUE);
			ud_set_traverse_mask(FCLR,UU_TRUE);
			if (Sresults.num_item > 0) ul_ipv_put_list(&Sresults," ");
			if (status == UU_SUCCESS)
			{
				ud_set_traverse_mask(FFAC,UU_TRUE);
				sprintf(sbuf,"Compare %s at %d points.",buf,Stess.np);
				ul_ipv_put_list(&Sresults,sbuf);
				if (Stype == SURF_TOG)
				{
					if (Sgrid_flag)
						sprintf(sbuf,"Grid = %d",Sgrid);
					else
					{
						UM_len_inttoext(Sftol,rval);
						sprintf(sbuf,"Tolerance = %5.4f , Grid = %d",rval,Sgrid);
					}
					ul_ipv_put_list(&Sresults,sbuf);
				}
				UM_len_inttoext(Smaxdis,rval);
				sprintf(sbuf,"Maximum Deviations = %5.4f Degrees , %5.4f %s",
					Smaxang,rval,ubuf);
				ul_ipv_put_list(&Sresults,sbuf);
				if (ucut[0] != -10000)
				{
					UM_len_inttoext(ucut[0],rval);
					UM_len_inttoext(ucut[1],rval1);
					sprintf(sbuf,"Undercut = %5.4f thru %5.4f",rval,rval1);
					ul_ipv_put_list(&Sresults,sbuf);
				}
				UM_len_inttoext(ocut[0],rval);
				UM_len_inttoext(ocut[1],rval1);
				sprintf(sbuf,"Overcut = %5.4f thru %5.4f",rval,rval1);
				ul_ipv_put_list(&Sresults,sbuf);
			}
			else if (status == UU_INTERRUPT)
			{
				strcpy(sbuf,"...Comparison interrupted.");
				ul_ipv_put_list(&Sresults,sbuf);
			}
			else
			{
				sprintf(sbuf,"Could not compare %s at %d points.",buf,Stess.np);
				ul_ipv_put_list(&Sresults,sbuf);
			}
			strcpy(Sresults.answer,sbuf);
			ud_update_answer(FSTA,(int *)&Sresults);
		}
	}
#ifdef DEBUG_T
	gtimx(&etims,&etimm);
	ttim = (etims-stims)*1000 + (etimm-stimm);
	sprintf(tbuf,"Compare = %d",ttim);
	NclxDbgPstr(tbuf);
#endif
/*
.....Free tessellation
*/
	if (Stype == SURF_TOG)
	{
		um_free_tess(&Stess);
		if (Svis == 1) uu_list_free(&triangles);
		ncl_set_tess_parms(ttyp,tsav,usav,vsav);
		ncl_set_boundary_toler(bsav);
	}
/*
.....Display visual only comparison
*/
	if (Svis == 1)
	{
		ud_form_invis();
		ifl35 = 0; ckintr(&ifl86,&ifl35);
		if (Stype == SURF_TOG) LiTriangleSolidEnd(Starget,FALSE,Sftol);
/*
........Add the target solid to the machining session
*/
		Sesstarget = LiSessionAddPrim(LW_session[LW_mach_mode],Starget);
		Sadded = UU_TRUE;
/*
........Build the color array
*/
		for (i=0;i<9;i++)
			mat[i] = LW_material[Scol[i]];
/*
........Define the comparison with all stocks
*/
		sd = LW_stock_first[0];
/*
........Visicut
*/
		if (LW_mach_mode == LW_VISICUT)
		{
			ul_ipv_count_stocks(nstock,UU_FALSE);
			if (nstock[0] == 1)
			{
				scomp = LiComparisonCreate(8,Stol,mat);
				LiDataSetGenericPtr(&stuff,scomp);
				LiMWViewportSetSessPrimProperty(LW_viewport,sd->stock,
					LI_VPSP_PROP_MW_SOL_COMPARISON,&stuff);
				LiDataSetGenericPtr(&stuff,Sesstarget);
				LiMWViewportSetSessPrimProperty(LW_viewport,sd->stock,
					LI_VPSP_PROP_VI_SOL_CMPAR_SOLID,&stuff);
			}
			else
			{
				if (Sprim == 0)
				{
					for (i=1;i<LW_nstock[0];i++)
					{
						ifl = 0;
						do
						{
							ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
							if (ifl == -2) break;
							if (Sprim == 0)
								Sprim = LiPrimitiveSolidExtract(sdtmp->stock);
							else if (sdtmp->active && sdtmp->visible)
							{
								prim1 = LiPrimitiveSolidExtract(sdtmp->stock);
								if (prim1 != 0)
								{
									LiPrimitiveSolidUnion(Sprim,prim1);
									LiPrimitiveDestroy(prim1);
								}
							}
						} while (ifl != -1);
						sd = (LW_stock_struc *)uu_lsnext(sd);
					}
					Sessprim = LiSessionAddPrim(LW_session[LW_mach_mode],Sprim);
					LiDataSetBoolean(&stuff,FALSE);
					LiMWViewportSetSessPrimProperty(LW_viewport,Sessprim,
						LI_VPSP_PROP_MW_VISIBLE,&stuff);
					LiViSolidSetEnabled(Sessprim,FALSE);
				}
				scomp = LiComparisonCreate(8,Stol,mat);
				LiDataSetGenericPtr(&stuff,scomp);
				LiMWViewportSetSessPrimProperty(LW_viewport,Sessprim,
					LI_VPSP_PROP_MW_SOL_COMPARISON,&stuff);
				LiDataSetGenericPtr(&stuff,Sesstarget);
				LiMWViewportSetSessPrimProperty(LW_viewport,Sessprim,
					LI_VPSP_PROP_VI_SOL_CMPAR_SOLID,&stuff);
			}
/*
...........Display the comparison
*/
			LiDrawableRender(LW_drawable,LW_viewport,
				LI_RENDER_MODE_VI_RT_TARGET_DIFF);
		}
/*
........RapidCut
*/
		else
		{
			scomp = LiComparisonCreate(8,Stol,mat);
			LiDataSetBoolean(&stuff,UU_TRUE);
			LiSessionSetProperty(LW_session[LW_mach_mode],
				LI_SESS_PROP_RV_REN_TARGET_DIFF,&stuff);
			ul_ipv_flush();
			LiDataSetBoolean(&stuff,UU_FALSE);
			LiSessionSetProperty(LW_session[LW_mach_mode],
				LI_SESS_PROP_RV_REN_TARGET_DIFF,&stuff);
		}
/*
........Redisplay the form
*/
		ud_hakt(UX_UDOS,1);
		LiComparisonDestroy(scomp);
		ud_form_vis();
	}
/*
.....Under/Over Cut comparison
*/
	if (Svis > 1 && Sndifs > 0)
	{
		status = S_compare_solids(Svis,Spdif,&nucut,ucut,&nocut,ocut);
		if (Sresults.num_item > 0) ul_ipv_put_list(&Sresults," ");
		if (status == UU_SUCCESS)
		{
			ud_set_traverse_mask(FFAC,UU_TRUE);
			ud_set_traverse_mask(FLOG,UU_TRUE);
			ud_set_traverse_mask(FCLR,UU_TRUE);
			sprintf(sbuf,"Compare %s for Under/Over Cuts.",buf);
			ul_ipv_put_list(&Sresults,sbuf);
			UM_len_inttoext(Sminvol,rval);
			sprintf(sbuf,"Minimum Volume Considered= %5.4f cu %s",rval,ubuf);
			ul_ipv_put_list(&Sresults,sbuf);
			if (Svis == 2 || Svis == 4)
			{
				sprintf(sbuf,"%d Undercuts",nucut);
				ul_ipv_put_list(&Sresults,sbuf);
				if (nucut != 0)
				{
					UM_len_inttoext(ucut[0],rval);
					UM_len_inttoext(ucut[1],rval1);
					sprintf(sbuf,"Undercut = %5.4f thru %5.4f cu %s",rval,rval1,ubuf);
					ul_ipv_put_list(&Sresults,sbuf);
				}
			}
			if (Svis == 3 || Svis == 4)
			{
				sprintf(sbuf,"%d Overcuts",nocut);
				ul_ipv_put_list(&Sresults,sbuf);
				if (nocut != 0)
				{
					UM_len_inttoext(ocut[0],rval);
					UM_len_inttoext(ocut[1],rval1);
					sprintf(sbuf,"Overcut = %5.4f thru %5.4f cu %s",rval,rval1,ubuf);
					ul_ipv_put_list(&Sresults,sbuf);
				}
			}
			strcpy(Sresults.answer,sbuf);
			ud_update_answer(FSTA,(int *)&Sresults);
			if (nucut+nocut > 0) ud_set_traverse_mask(FFND,UU_TRUE);
			Sfndsol = 1;
		}
/*
........Set translucency of stocks
*/
		if (Strans != 100)
		{
			sd = LW_stock_first[0];
			LiDataSetNat32(&stuff,100-Strans);
			for (j=0;j<LW_nstock[0];j++)
			{
				ifl = 0;
				do
				{
					ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
					if (ifl == -2) break;
					LiSessionPrimSetVisualProperty(sdtmp->stock,
						LI_MW_VIS_PROP_TRANSPARENCY,&stuff);
				} while (ifl != -1);
				sd = (LW_stock_struc *)uu_lsnext(sd);
			}
		}
/*
.....Color faces
*/
		tface = (S_face *)UU_LIST_ARRAY(&Sface);
		class = LiViGetPredefinedAttributeClass(LI_VI_ATTRIB_FACE_MATERIAL);
		for (j=0;j<Snface;j++)
		{
			LiViEntitySetAttribute(tface[j].face,LI_ENTITY_TYPE_FACE,class,
				LW_material[tface[j].color]);
		}
		ul_ipv_flush();
	}
/*
.....Display the part comparison
.....which was performed manually
*/
	else if (Svis == 0)
	{
		if (Svert_index != UU_NULL)
		{
			uu_free(Svert_index);
			Svert_index = UU_NULL;
		}
		ul_ipv_flush();
	}
	goto done;
/*
.....Could not create tessellation
*/
notess:;
	ud_wrerr("Could not create surface tessellation.");
	goto done;
/*
.....Could not retrieve surfaces
*/
nosurf:;
	ud_wrerr("Could not obtain surface data.");
	goto done;
/*
.....Could not create target solid
*/
notarget:;
	ud_wrerr("Could not create target solid to compare.");
	goto done;
/*
.....End of routine
*/
done:;
	if (LW_mach_mode == LW_VISICUT && LW_mach_type == LW_LATHE)
		ul_ipv_lathe_start();
	um_reset_pocket_graphics(UM_IPV_WINDOW);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnDesel(fieldno, val, stat)
**			Deselects all surfaces from the list.
**    PARAMETERS
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Not used, form initiated through pushbutton.
**			         stat    = Not used.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnDesel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,nc;
	struct NCL_fixed_databag e;
	S_geo *geo;
	UU_LIST *sfpt;
	sfpt = &Ssurf;
/*
.....Remove all entities in this list
*/
	nc = UU_LIST_LENGTH(sfpt);
	geo = (S_geo *) UU_LIST_ARRAY(sfpt);
	for (i=0;i<nc;i++)
	{
		e.key = geo[i].key;
		ncl_update_color(e.key,geo[i].color);
		uc_retrieve_data(&e, sizeof(e));
		uc_display(&e);
	}
	uu_list_delete(sfpt,0,nc);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnFace(filedno, val, stat)
**       Displays the deviation for a specific face.  This routine is
**       is only valid after the part has been compared.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnFace(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,k,button,event,status,cutn;
	UU_REAL rval,rval1;
	char sbuf[80],ubuf[20],btyp[20];
	UU_LOGICAL found,idid;
	S_face *mface;
	LtDouble dis;
	LtPoint orig;
	LtVector norm;
	LtPickedEntityList entlist;
	LtPickedEntity entpick;
	LtEntity entity;
	LtEntityType type;
	LtSessionPrim solid;
	LW_rv_picked_entity rvent;
	UN_mot_attr *mattr;
	UN_mot_data *mdata;
	S_solid *sptr;
/*
.....Get the face to compare
*/
	for (k=0;k<1;k++)
	{
		idid = UU_FALSE;
		ud_form_invis();
		event = ul_ipv_pick_entity("Select face to compare",LI_ENTITY_TYPE_FACE,
			&entlist,&rvent,orig,norm,&button);
		ud_form_vis();
/*
.....Exit on middle or right mouse button
*/
		if (event != 3 || button != 1) break;
/*
.....No entities selected
*/
		if (entlist == 0)
		{
			ud_prmerr("No entities selected.");
			continue;
		}
/*
.....Get face id
*/
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick == 0)
		{
			ud_prmerr("No entities selected.");
			continue;
		}
		status = LiViPickedEntityExpand(entpick,&entity,&type,&dis);
		if (status != 0)
		{
			ud_prmerr("No entities selected.");
			continue;
		}
/*
.....Face comparison
........Find selected face
*/
		if (Svis == 0)
		{
			mface = (S_face *)UU_LIST_ARRAY(&Sface);
			for (i=0;i<Snface;i++)
			{
				if (entity == mface[i].face) break;
			}
/*
........Output Face comparisons
*/
			if (Sresults.num_item > 0) ul_ipv_put_list(&Sresults," ");
			if (i < Snface && mface[i].used)
			{
				if (mface[i].ucut[0] != -10000.)
				{
					UM_len_inttoext(mface[i].ucut[0],rval);
					UM_len_inttoext(mface[i].ucut[1],rval1);
					sprintf(sbuf,"Undercut = %5.4f thru %5.4f",rval,rval1);
					ul_ipv_put_list(&Sresults,sbuf);
				}
				if (mface[i].ocut[0] != 10000.)
				{
					UM_len_inttoext(mface[i].ocut[0],rval);
					UM_len_inttoext(mface[i].ocut[1],rval1);
					sprintf(sbuf,"Overcut = %5.4f thru %5.4f",rval,rval1);
					ul_ipv_put_list(&Sresults,sbuf);
				}
				idid = UU_TRUE;
			}
/*
........Face is not in comparison
*/
			else
			{
				sprintf(sbuf,"Face #%d is not in comparison.",i);
				ul_ipv_put_list(&Sresults,sbuf);
			}
		}
/*
.....Solid comparison
*/
		else
		{
			sptr = (S_solid *) UU_LIST_ARRAY(&Ssolid);
/*
........Get the solid picked
*/
			solid = LiViEntityGetSessionPrim(entity,LI_ENTITY_TYPE_FACE);
			for (i=0;i<Snptr;i++)
				if (solid == sptr[i].prim) break;
/*
........Output Solid comparisons
*/
			if (i < Snptr)
			{
				if (Sresults.num_item > 0) ul_ipv_put_list(&Sresults," ");
				if (sptr[i].type == 0) strcpy(btyp,"Undercut");
				else strcpy(btyp,"Overcut");
				UM_len_inttoext(sptr[i].vol,rval);
				um_linear_units_str(UM_cpln.length_unit,ubuf);
				sprintf(sbuf,"%s = %5.4f cu %s",btyp,rval,ubuf);
				ul_ipv_put_list(&Sresults,sbuf);
/*				idid = UU_TRUE;*/
			}
/*
........Solid is not in comparison
*/
			else
			{
				sprintf(sbuf,"Face is not in comparison.");
				ul_ipv_put_list(&Sresults,sbuf);
			}
		}
/*
.....Output face attributes
*/
		if (idid)
		{
			found = ul_ipv_find_mdata(entity,&cutn,&mattr,&mdata,UU_FALSE);
			if (found)
			{
				sprintf(sbuf,"ISN/%d",mdata->isn);
				ul_ipv_put_list(&Sresults,sbuf);
				if (mdata->seqno != 0)
				{
					sprintf(sbuf,"SEQNO/%d",mdata->seqno);
					ul_ipv_put_list(&Sresults,sbuf);
				}
				sprintf(sbuf,"CLREC/%d,%d",mdata->clrec[0],mdata->clrec[1]);
				ul_ipv_put_list(&Sresults,sbuf);
				sprintf(sbuf,"CUT/%d",cutn);
				ul_ipv_put_list(&Sresults,sbuf);
				ul_ipv_format_tool(sbuf,mattr->tlno);
				ul_ipv_put_list(&Sresults,sbuf);
			}
			else
			{
				if (Svis == 0)
					sprintf(sbuf,"No attributes stored with %s Face #%d",
						mface[i].label,i);
				else
					sprintf(sbuf,"No attributes stored with %s #%d",btyp,i);

				ul_ipv_put_list(&Sresults,sbuf);
			}
		}
/*
.....Update the list
*/
		strcpy(Sresults.answer,sbuf);
		ud_update_answer(FSTA,(int *)&Sresults);
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnFfwd(filedno, val, stat)
**       Routine to fast forward the gouge finder to the last gouge.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnFfwd(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Fast forward the gouge finder
*/
	Sfndsol = Snsolid;
	ud_dispfrm_update_answer(Sfrm2,FGOU,(int *)&Sfndsol);
	ud_update_form(Sfrm2);
	OnSeg(fieldno,val,stat);
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnFit(filedno, val, stat)
**       Routine to fit entire view in window (extrema zoom).
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnFit(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Fit view
*/
	ul_ipv_view_active();
	uz_extrema_zoom();
	ul_ipv_view_inactive();
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnFwd(filedno, val, stat)
**       Routine to advance the gouge finder.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnFwd(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Advance the gouge finder
*/
	if (Sfndsol < Snsolid) Sfndsol = Sfndsol + 1;
	ud_dispfrm_update_answer(Sfrm2,FGOU,(int *)&Sfndsol);
	ud_update_form(Sfrm2);
	OnSeg(fieldno,val,stat);
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnLog(filedno, val, stat)
**       Saves the results list to an external file.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnLog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,inum,status;
	char descrip[80],sbuf[80];
	char *p,*strrchr();
	UX_pathname fullname;
	char ext[UX_SUFFIX_LEN];
	FILE *fd;
/*
.....Get the log filename
*/
	if (Sresults.num_item == 0) goto done;
	sprintf(sbuf,"Part Comparison Log File");
	strcpy(ext,"*.log");
	strcpy(descrip,"Log Files (*.log)");
	fullname[0] = '\0';
	ud_get_filename(sbuf,sbuf,ext,fullname,&inum,descrip,0,UU_FALSE);
	if (inum == 0) goto done;
	p = strrchr(fullname,'.');
	if (p == UU_NULL) strcat(fullname,".log");
/*
.....Open the log file
*/
	status = ux_fopen0(fullname,"w",&fd);
	if (fd == UU_NULL || status != UU_SUCCESS)
	{
		sprintf(sbuf,"Failed to open log file '%s'.",fullname);
		ud_wrerr(sbuf);
		goto done;
	}
/*
.....Write out the Results Window
*/
	for (i=0;i<Sresults.num_item;i++)
	{
		sprintf(sbuf,"%s\n",Sresults.item[i]);
		status = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (status != UU_SUCCESS)
		{
			ud_wrerr("Error writing to log file '%s'.",fullname);
			goto done;
		}
	}
/*
.....Close the file
*/
	ux_fclose0(fd);
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnMeasure(filedno, val, stat)
**       Loads the NCLIPV Measurement form.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnMeasure(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ul_ipv_measure();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnModal(filedno, val, stat)
**       Method called when text field in Color Form is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnModal(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
/*
.....Auto Tolerance
*/
	case FTOL:
		UM_len_exttoint(*val->frmflt,Sauto);
		break;
/*
.....Tolerances
*/
	case FTOL1:
		UM_len_exttoint(*val->frmflt,Stol[0]);
		break;
	case FTOL2:
		UM_len_exttoint(*val->frmflt,Stol[1]);
		break;
	case FTOL3:
		UM_len_exttoint(*val->frmflt,Stol[2]);
		break;
	case FTOL4:
		UM_len_exttoint(*val->frmflt,Stol[3]);
		break;
	case FTOL5:
		UM_len_exttoint(*val->frmflt,Stol[4]);
		break;
	case FTOL6:
		UM_len_exttoint(*val->frmflt,Stol[5]);
		break;
	case FTOL7:
		UM_len_exttoint(*val->frmflt,Stol[6]);
		break;
	case FTOL8:
		UM_len_exttoint(*val->frmflt,Stol[7]);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnReset(filedno, val, stat)
**       Method called at when "Reset" button is pushed
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnReset(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (*fieldno!=FRES) return(UD_FLDOK);
/*
.....Change the default tolerances
.....to increments based on the
.....automatic tolerance
*/
	Scol[0] = LW_default_compare.color[0];
	ud_dispfrm_update_answer(Sfrm,FCOL1,&Scol[0]);
	Scol[1] = LW_default_compare.color[1];
	ud_dispfrm_update_answer(Sfrm,FCOL2,&Scol[1]);
	Scol[2] = LW_default_compare.color[2];
	ud_dispfrm_update_answer(Sfrm,FCOL3,&Scol[2]);
	Scol[3] = LW_default_compare.color[3];
	ud_dispfrm_update_answer(Sfrm,FCOL4,&Scol[3]);
	Scol[4] = LW_default_compare.color[4];
	ud_dispfrm_update_answer(Sfrm,FCOL5,&Scol[4]);
	Scol[5] = LW_default_compare.color[5];
	ud_dispfrm_update_answer(Sfrm,FCOL6,&Scol[5]);
	Scol[6] = LW_default_compare.color[6];
	ud_dispfrm_update_answer(Sfrm,FCOL7,&Scol[6]);
	Scol[7] = LW_default_compare.color[7];
	ud_dispfrm_update_answer(Sfrm,FCOL8,&Scol[7]);
	Scol[8] = LW_default_compare.color[8];
	ud_dispfrm_update_answer(Sfrm,FCOL9,&Scol[8]);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnRewind(filedno, val, stat)
**       Routine to rewind the gouge finder to the first gouge.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnRewind(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Rewind the gouge finder
*/
	Sfndsol = 1;
	ud_dispfrm_update_answer(Sfrm2,FGOU,(int *)&Sfndsol);
	ud_update_form(Sfrm2);
	OnSeg(fieldno,val,stat);
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSeg(filedno, val, stat)
**       This routine fits the NCLIPV view to the requested gouge.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSeg(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	S_solid *sptr;
/*
.....Make sure there are gouges
*/
	if (Snsolid == 0) goto done;
	if (Sfndsol < 1) Sfndsol = 1;
	if (Sfndsol > Snsolid) Sfndsol = Snsolid;
/*
.....Fit the view to the requested gouge
*/
	LW_diff_solid_view = UU_TRUE;
	sptr = (S_solid *) UU_LIST_ARRAY(&Ssolid);
	uu_move_byte(&sptr[Sfndsol-1].bounds,&LW_diff_bounds,sizeof(LtDoubleBounds));
	ul_ipv_view_active();
	uz_extrema_zoom();
	ul_ipv_view_inactive();
	LW_diff_solid_view = UU_FALSE;
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSelect(filedno, val, stat)
**       Routine to select the NCL surfaces.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSelect(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int color,numint,dum,csav,init;
	struct NCL_fixed_databag e;
	S_geo geo;
	UU_LOGICAL cmdreject,lcont;
	UM_PLOCREC pick;
/*
.....Take down form
*/
	if (*fieldno!=FSEL) return UD_FLDOK;
	ud_form_invis();
	color = Scolor;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0)
	{
		goto done;
	}
/*
.....Get the surfaces to compare
*/
	if (Stype == SURF_TOG)
	{
		ud_lgeo(UU_TRUE,UD_ncl_allsf);
		ud_ldas(UD_DASSELECT,UA_NCL,591,UU_NULL,1,&numint,UD_NODEFAULT);
	}
/*
.....Get the shape to compare
*/
	else if (Stype == SHAPE_TOG)
	{
		ud_lgeo(UU_TRUE,UD_ncl_sh);
		um_dl_pldas(UD_DASPCKLOC,UA_NCL,592,&pick,1,&numint,1);
	}
	else
		goto done;
	if (numint == 0) goto done;
/*
.....Loop through selections
*/
	init = UU_TRUE;
	lcont = UU_TRUE;
	do
	{
		if (Stype == SURF_TOG)
		{
			lcont = ud_gnxt(init,UU_NULL,&e.key,1);
			if (!lcont) break;
			init = UU_FALSE;
		}
		else
		{
			e.key = um_get_pickkey(&pick.pent,1);
			lcont = UU_FALSE;
		}
/*
.....Only 1 shape allowed per compare
*/
		if (Stype == SHAPE_TOG) S_clear_list(&Ssurf);
/*
.....Store this item in the list
*/
		uc_retrieve_data(&e,sizeof(e));
		geo.key = e.key;
		geo.relnum = e.rel_num;
		strcpy(geo.label,e.label);
		um_get_attrib(&e.key,&geo.color,&dum,&dum,&dum,&dum);
		csav = color;
		S_add_list(&Ssurf,&geo,&color);
/*
.....Update the entities color
*/
		if (color != -1)
		{
			ncl_update_color(e.key,color);
			uc_display(&e);
		}
		color = csav;
	} while (lcont);
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_form_vis();
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnText(filedno, val, stat)
**       Method called when text field is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnText(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
/*
.....STL file
*/
	case FSTL:
		strcpy(Sfile,val->frmstr);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnToler(filedno, val, stat)
**       Method called when text field in Tolerance Form is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnToler(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
/*
.....Surface Grid
*/
	case FGRD:
		Sgrid = *val->frmint;
		break;
/*
.....Surface Tolerance
*/
	case FSFT:
		UM_len_exttoint(*val->frmflt,Sftol);
		break;
/*
.....Angular deviation
*/
	case FANG:
		UM_len_exttoint(*val->frmflt,Smaxang);
		break;
/*
.....Linear Deviation
*/
	case FDEV:
		UM_len_exttoint(*val->frmflt,Smaxdis);
		break;
/*
.....Minimum Volume
*/
	case FVOL:
		UM_len_exttoint(*val->frmflt,Sminvol);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnType(filedno, val, stat)
**       Method called at when "Compare" toggle is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnType(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	if (*fieldno!=FTYP) return UD_FLDOK;
/*
.....Ignore first time through
*/
	if (Sfirst)
		Sfirst = UU_FALSE;
/*
.....Enable correct fields
.....based on toggle
*/
	else
	{
		if (LW_nclipv==LW_INPROCESS)
		{
			for (i=0;i<=3;i++)
			{
				if (strcmp(val->frmstr,Scmptxt[i]) == 0) break;
			}
			Stype = i;
		}
		else
			Stype = 1;
		if (Stype == SURF_TOG)
		{
			ud_set_traverse_mask(FSEL,UU_TRUE);
			ud_set_traverse_mask(FHIL,UU_TRUE);
			ud_set_traverse_mask(FDES,UU_TRUE);
			ud_set_traverse_mask(FBRO,UU_FALSE);
			ud_set_traverse_mask(FSTL,UU_FALSE);
			ud_set_traverse_mask(FUNI,UU_FALSE);
			ud_set_traverse_mask(FFND,UU_FALSE);
			ud_set_traverse_mask(FMFL,UU_FALSE);
			ud_set_traverse_mask(FMNM,UU_FALSE);
			ud_set_traverse_mask(FMLD,UU_FALSE);
			ud_set_traverse_mask(FMMD,UU_FALSE);
			if (Svis > 1)
			{
				Svis = 0;
				ud_update_answer(FVIS,&Svis);
			}
		}
		else if (Stype == STL_TOG)
		{
			ud_set_traverse_mask(FSEL,UU_FALSE);
			ud_set_traverse_mask(FHIL,UU_FALSE);
			ud_set_traverse_mask(FDES,UU_FALSE);
			ud_set_traverse_mask(FBRO,UU_TRUE);
			ud_set_traverse_mask(FSTL,UU_TRUE);
			ud_set_traverse_mask(FUNI,UU_TRUE);
			ud_set_traverse_mask(FMFL,UU_TRUE);
			if (Smxflag)
			{
				ud_set_traverse_mask(FMNM,UU_TRUE);
				ud_set_traverse_mask(FMLD,UU_TRUE);
				ud_set_traverse_mask(FMMD,UU_TRUE);
			}
		}
		if (Stype == SHAPE_TOG)
		{
			ud_set_traverse_mask(FSEL,UU_TRUE);
			ud_set_traverse_mask(FHIL,UU_TRUE);
			ud_set_traverse_mask(FDES,UU_FALSE);
			ud_set_traverse_mask(FBRO,UU_FALSE);
			ud_set_traverse_mask(FSTL,UU_FALSE);
			ud_set_traverse_mask(FUNI,UU_FALSE);
			ud_set_traverse_mask(FMFL,UU_FALSE);
			ud_set_traverse_mask(FMNM,UU_FALSE);
			ud_set_traverse_mask(FMLD,UU_FALSE);
			ud_set_traverse_mask(FMMD,UU_FALSE);
		}
		if (Svis == 1)
		{
			ud_set_traverse_mask(FMOD,UU_FALSE);
			ud_set_traverse_mask(FTRA,UU_FALSE);
			ud_set_traverse_mask(FFAC,UU_FALSE);
			OnClear(fieldno,val,stat);
		}
		else
		{
			ud_set_traverse_mask(FMOD,UU_TRUE);
			ud_set_traverse_mask(FTRA,UU_TRUE);
			if (Sresults.num_item > 0)
			{
				ud_set_traverse_mask(FLOG,UU_TRUE);
				ud_set_traverse_mask(FCLR,UU_TRUE);
			}
			else
			{
				ud_set_traverse_mask(FLOG,UU_FALSE);
				ud_set_traverse_mask(FCLR,UU_FALSE);
			}
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnView(filedno, val, stat)
**       Routine to enter dynamic viewing within the Comparison form.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnView(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Take down form
*/
	if (!Sactive2) ud_form_invis();
/*
.....Enter dynamic viewing
*/
	ul_ipv_view_active();
	uz_dyn_mouse();
	ul_ipv_view_inactive();
	um_reset_pocket_graphics(UM_IPV_WINDOW);
/*
.....End of routine
.....Redisplay form
*/
done:;
	if (!Sactive2) ud_form_vis();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnVis(fieldno, val, stat)
**       Enables/Disables fields based on the 'Visual only' toggle.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnVis(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Cannot do under/over cuts with surfaces
*/
	if (Stype == SURF_TOG && Svis > 1)
	{
		Svis = 0;
		ud_update_answer(FVIS,&Svis);
	}
/*
....Visual only is active
*/
	if (Svis == 1)
	{
		ud_set_traverse_mask(FMOD,UU_FALSE);
		ud_set_traverse_mask(FTRA,UU_FALSE);
		ud_set_traverse_mask(FSTA,UU_FALSE);
		ud_set_traverse_mask(FFAC,UU_FALSE);
		ud_set_traverse_mask(FFND,UU_FALSE);
		ud_set_traverse_mask(FLOG,UU_FALSE);
		ud_set_traverse_mask(FCLR,UU_FALSE);
		ud_set_traverse_mask(FPRG,UU_FALSE);
		ud_set_traverse_mask(FITR,UU_FALSE);
	}
/*
....Visual only is not active
*/
	else
	{
		ud_set_traverse_mask(FMOD,UU_TRUE);
		ud_set_traverse_mask(FTRA,UU_TRUE);
		ud_set_traverse_mask(FSTA,UU_TRUE);
		if (Sresults.num_item > 0)
		{
			ud_set_traverse_mask(FLOG,UU_TRUE);
			ud_set_traverse_mask(FCLR,UU_TRUE);
		}
		else
		{
			ud_set_traverse_mask(FLOG,UU_FALSE);
			ud_set_traverse_mask(FCLR,UU_FALSE);
		}
		if (Svis == 0)
		{
			ud_set_traverse_mask(FPRG,UU_TRUE);
			ud_set_traverse_mask(FITR,UU_TRUE);
		}
		else
		{
			ud_set_traverse_mask(FPRG,UU_FALSE);
			ud_set_traverse_mask(FITR,UU_FALSE);
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnMatrix(fieldno,val,stat)
**      Callback function to enable/disable transformation.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnMatrix(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Set traversal flags
*/
	Smxchg = UU_TRUE;
	ud_set_traverse_mask(FMNM,Smxflag);
	ud_set_traverse_mask(FMLD,Smxflag);
	ud_set_traverse_mask(FMMD,Smxflag);
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnMxname(fieldno,val,stat)
**      Callback function when new matrix name is selected.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnMxname(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Get and store the selected matrix name
*/
	strcpy(Smxname,val->frmstr);
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnLoadmx(fieldno,val,stat)
**      Callback function to load a predefined matrix.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnLoadmx(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,nc,status,inc;
	char label[NCL_MAX_LABEL+1],sbuf[NCL_MAX_LABEL+20];
	UM_f77_str ncllabel;
	UU_KEY_ID key;
	struct NCL_fixed_databag e;
	struct NCL_matrix_rec *mx;
/*
.....Verify that the matrix exists
*/
	strcpy(label,Smxname);
	nc = strlen(label);
	if (nc == 0) goto failed;
	for (i=0;i<nc;i++) label[i] = islower(label[i]) ? toupper(label[i]) :
		label[i];
	for (i=nc;i<NCL_MAX_LABEL;i++) label[i] = ' ';
	UM_init_f77_str(ncllabel,label,NCL_MAX_LABEL);
	getkey(UM_addr_of_f77_str(ncllabel),&key);
	if (key == 0) goto failed;
/*
.....Get the matrix data and
.....place it into the structure
*/
	e.key = key;
	status = ur_retrieve_data_fixed(&e);
	if (status != UU_SUCCESS) goto failed;
	if (e.rel_num != NCL_MATRIX_REL) goto notmx;
	mx = (struct NCL_matrix_rec *)&e;
	ncl_34mx_to_43mx(mx->mat,Smatrix);
/*
.....Update the form if displayed
*/
	if (Smxfrm != 0)
	{
		ud_dispfrm_update_answer(Smxfrm,0,(int *)Smxname);
		inc = 1;
		for (j=0;j<3;j++)
		{
			for (i=0;i<4;i++)
				ud_dispfrm_update_answer(Smxfrm,inc++,
					(int *)&Smatrix[i][j]);
		}
	}
	ud_update_form(Smxfrm);
/*
.....If the target is already defined
.....Then apply new matrix
*/
	if (Starget != 0) S_place_solid(Starget,Smatrix);
	goto done;
/*
.....Could not load matrix
*/
failed:;
	sprintf(sbuf,"Could not load Matrix '%s'",label);
	ud_wrerr(sbuf);
	goto done;
/*
.....Not a matrix
*/
notmx:;
	sprintf(sbuf,"'%s' is not a Matrix",label);
	ud_wrerr(sbuf);
	goto done;
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnMxclose(fieldno,val,stat)
**      Callback function when matrix form is closed.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnMxclose(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	Smxfrm = 0;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnModmx(fieldno,val,stat)
**      Callback function to display the active stock matrix form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnModmx(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int *ans[13],i,j,inc;
	static char called[] = {6,6,6,6,6,6,6,6,6,6,6,6};
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		OnMxclose};
/*
.....Setup the form field defaults
*/
	ans[0] = (int *)Smxname;
	inc = 1;
	for (j=0;j<3;j++)
	{
		for (i=0;i<4;i++) ans[inc++] = (int *)&Smatrix[i][j];
	}
/*
.....Display the form
*/
	Smxfrm = ud_form_display1("ipvstkmx.frm",ans,ans,methods,called,UU_NULL,
		UU_NULL);
	if (Smxfrm == -1) goto failed;
	goto done;
/*
.....Could not display form
*/
failed:;
	Smxfrm = 0;
	ud_wrerr("Could not create Matrix form.");
	goto done;
/*
.....End of routine
*/
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_add_list(sflst,geo,color)
**			Adds a new entity to the surface list.  If the entity
**			already exists, then it will remove it from the list instead.
**    PARAMETERS
**       INPUT  : sflist  = List of already selected entities.
**			         geo     = Entity to add/remove from list.
**			         color   = Entity display color when added to the list.
**			                   If the entity is deleted from the list, then
**			                   it is displayed using the 'geo.color' color.
**						          Returns -1 if the entity's color should not be
**						          updated.
**       OUTPUT :
**			         sflist  = Updated list.
**						color   = Updated entity color;
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_add_list(sflst,geo,color)
UU_LIST *sflst;
S_geo *geo;
int *color;
{
	int i,nc;
/*	char sbuf[100];*/
	UU_LOGICAL found;
	S_geo *list_geo;
/*
.....Point to the geometry list
*/
	list_geo = (S_geo *) UU_LIST_ARRAY(sflst);
/*
.....Search for the selected entity
*/
	found = UU_FALSE;
	nc = UU_LIST_LENGTH(sflst);
	for (i=0;i<nc;i++)
	{
/*
.....Entity found
.....Remove it from the list
*/
		if (geo->key == list_geo[i].key)
		{
			*color = list_geo[i].color;
			uu_list_delete(sflst,i,1);
/*			sprintf(sbuf,"Entity %s removed.",geo->label);
			ud_prmerr(sbuf);*/
			found = UU_TRUE;
			break;
		}
	}
/*
.....Entity not found
*/
	if (!found) uu_list_push(sflst,geo);
}

/*********************************************************************
**    I_FUNCTION     : S_clear_list(sflst)
**			Removes all entities from a surface list and restores their
**       colors.
**    PARAMETERS
**       INPUT  : sflist  = List of already selected entities.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_clear_list(sflst)
UU_LIST *sflst;
{
	int i,nc,color;
	S_geo *geo;
	struct NCL_fixed_databag e;
/*
.....Restore the entity colors
*/
	nc = UU_LIST_LENGTH(sflst);
	if (nc != 0)
	{
		geo = (S_geo *)UU_LIST_ARRAY(&Ssurf);
		for (i=0;i<nc;i++)
		{
			e.key = geo[i].key;
			color = geo[i].color;
			ncl_update_color(e.key,color);
			uc_retrieve_data(&e,sizeof(e));
			uc_display(&e);
		}
/*
.....Clear out the list
*/
		uu_list_delete(sflst,0,nc);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_analyze_solid(solid,solptr)
**       Performs a Report Style analyzation of the area enclosed
**       within the specified difference solid.
**    PARAMETERS
**       INPUT  :
**                solid   = Difference solid to analyze within.
**                solptr  = Pointer to actual difference solid.
**       OUTPUT :
**                none
**    RETURNS      :
**                UU_FAILURE if no faces are within the difference solid.
**                UU_SUCCESS normally.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int	S_analyze_solid(solid,solptr)
S_solid *solid;
LtSessionPrim *solptr;
{
	int i;
	S_face *tface;
	UU_REAL box[6],dis;
	UM_coord *pt;
	UM_vector *vc;
	LtAttributeClass class;
/*
.....Initialize routine
*/
	class = LiViGetPredefinedAttributeClass(LI_VI_ATTRIB_FACE_MATERIAL);
/*
.....Tessellate model if necessary
*/
	if (Stess.np == 0)
	{
		LiPrimitiveGetSolidMesh(Starget,
			LI_MW_MESH_TRIANGLES|LI_MW_MESH_OUTPUT_PLANES,
			(LtFuncMWMeshBegin)S_mesh_begin,
			(LtFuncMWMeshAddPolygon)S_mesh_polygon,UU_NULL,UU_NULL);
	}
/*
.....Set up face structures
*/
	if (Snface == 0)
	{
		uu_list_init(&Sface,sizeof(S_face),1000,1000);
		S_hilite_faces();
	}
	tface = (S_face *)UU_LIST_ARRAY(&Sface);
/*
.....Loop through surface points and
.....Check points within difference solid
.....to stock
*/
	box[0] = solid->bounds[LI_MINX];
	box[1] = solid->bounds[LI_MINY];
	box[2] = solid->bounds[LI_MINZ];
	box[3] = solid->bounds[LI_MAXX];
	box[4] = solid->bounds[LI_MAXY];
	box[5] = solid->bounds[LI_MAXZ];
/*printf("\nBox = %f,%f,%f,   %f,%f,%f\n",box[0],box[1],box[2],box[3],box[4],box[5]);*/
	pt = (UM_coord *)UU_LIST_ARRAY(&Stess.vertices);
	vc = (UM_vector *)UU_LIST_ARRAY(&Stess.normals);
	for (i=0;i<Stess.np;i++)
	{
/*
........Point is within box
........Get nearest face
*/
/*if (pt[i][0] >= box[0] && pt[i][0] <= box[3])
printf("pt = %f,%f,%f\n",pt[i][0],pt[i][1],pt[i][2]);*/
		if (pt[i][0] >= box[0] && pt[i][1] >= box[1] && pt[i][2] >= box[2] &&
			pt[i][0] <= box[3] && pt[i][1] <= box[4] && pt[i][2] <= box[5])
		{
			dis = S_compare_point(pt[i],vc[i],tface,0,Sfile);
		}
	}
/*
.....Color faces
*/
	for (i=0;i<Snface;i++)
	{
		LiViEntitySetAttribute(tface[i].face,LI_ENTITY_TYPE_FACE,class,
			LW_material[tface[i].color]);
	}
/*
.....End of routine
*/
	solid->analyzed = UU_TRUE;
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_compare_part(surf,label,tess,ucut,ocut,upd)
**			Compares the tessellated surfaces to the cut model and changes
**       the color of the faces depending on the tolerances.
**    PARAMETERS
**       INPUT  :
**                surf    = Specifies which surface in list is being tested.
**                label   = Label of surface.
**                tess    = Tessellation of selected surfaces.
**                upd     = UU_TRUE = update progress bar.
**       OUTPUT :
**                ucut[0] = Smallest undercut of surface.
**                ucut[1] = Largest undercut of surface.
**                ocut[0] = Smallest overcut of surface.
**                ocut[1] = Largest overcut of surface.
**    RETURNS      :
**                UU_FAILURE if no points in tessellation could check to
**                the surface.  UU_SUCCESS normally.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_compare_part(surf,label,tess,ucut,ocut,upd)
int surf;
char *label;
UM_tessellation *tess;
UU_REAL ucut[2],ocut[2];
UU_LOGICAL upd;
{
	int i,j,perc,ifl;
	UM_int2 ifl86,ifl35;
	UU_REAL dis;
	UM_coord *pt;
	UM_vector *vc;
	S_face *tface;
	LW_stock_struc *sd,*sdtmp;
	LtData transp;
	LtAttributeClass class;
/*
.....Initialize routine
*/
	ucut[0] = -10000.;
	ucut[1] = 10000.;
	ocut[0] = 10000.;
	ocut[1] = -10000.;
	ifl35 = 0;
	class = LiViGetPredefinedAttributeClass(LI_VI_ATTRIB_FACE_MATERIAL);
	pt = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
	vc = (UM_vector *)UU_LIST_ARRAY(&tess->normals);
	tface = (S_face *)UU_LIST_ARRAY(&Sface);
/*
.....Initialize progress bar
*/
	if (upd)
	{
		Sprog = perc = 0;
		ud_update_answer(FPRG,&Sprog);
		ud_update_form(0);
	}
/*
.....Loop through all vertices
*/
	for (i=0;i<tess->np;i++)
	{
		dis = S_compare_point(pt[i],vc[i],tface,surf,label);
		if (dis < 0.)
		{
			if (dis > ucut[0]) ucut[0] = dis;
			if (dis < ucut[1]) ucut[1] = dis;
		}
		else
		{
			if (dis < ocut[0]) ocut[0] = dis;
			if (dis > ocut[1]) ocut[1] = dis;
		}
/*
........Update Progress bar
*/
		if (upd)
		{
			perc = (UU_REAL)(i+1) / (UU_REAL)tess->np * 100;
			if ((perc-Sprog) > 1 || perc == 100)
			{
				Sprog = perc;
				ud_update_answer(FPRG,&Sprog);
				ud_update_form(0);
			}
		}
/*
........Check for user interrupt
*/
		ckintr(&ifl86,&ifl35);
		if (Sinterrupt) break;
	}
/*
.....Set translucency of stocks
*/
	if (Strans != 100)
	{
		sd = LW_stock_first[0];
		LiDataSetNat32(&transp,100-Strans);
		for (j=0;j<LW_nstock[0];j++)
		{
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
				if (ifl == -2) break;
				LiSessionPrimSetVisualProperty(sdtmp->stock,
					LI_MW_VIS_PROP_TRANSPARENCY,&transp);
			} while (ifl != -1);
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
	}
/*
.....Color faces
*/
	for (j=0;j<Snface;j++)
	{
		LiViEntitySetAttribute(tface[j].face,LI_ENTITY_TYPE_FACE,class,
			LW_material[tface[j].color]);
	}
/*
.....End of routine
*/
	if (ucut[0] == -10000. && ocut[0] == 10000.) return(UU_FAILURE);
	else return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_compare_point(pt,norm,tface,surf,label)
**			Compares a point with the cut model.  Modifies the face structure
**       of the cut model to reflect the distance of the point.
**    PARAMETERS
**       INPUT  :
**                pt      = Point to compare.
**                norm    = Normal vector of point.
**                tface   = Array of face structures on model.
**                surf    = Specifies which surface in list is being tested.
**                label   = Label of surface.
**       OUTPUT :
**                none
**    RETURNS      :
**                Distance of point to cut model.  A negative value is an
**                undercut, a positive value an overcut.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_REAL S_compare_point(pt,norm,tface,surf,label)
UM_coord pt;
UM_vector norm;
S_face *tface;
int surf;
char *label;
{
	int j,k,mat;
	UU_REAL dis,dis1,plane[4],tol,ang,retdis;
	UU_REAL fabs(),cos();
	UM_vector vc,vcn;
	LtPickedEntityList entlist;
	LtPickedEntity entpick;
	LtEntity entity,temp;
	LtEntityType type;
/*
.....Initialize routine
*/
	tol = .005;
	retdis = 0.;
/*
.....Project point onto cut model
.....Using normal vector
*/
	um_unitvc(norm,vc);
/*printf("pt = %g,%g,%g,  %g,%g,%g\n",pt[0],pt[1],pt[2],
vc[0],vc[1],vc[2]);*/
	um_vctovc(vc,vcn);
	dis = 10000.;
	entlist = LiViSolidPick(LW_viewport,pt,vc,LI_MW_COORD_SYSTEM_VIEW,
		LI_ENTITY_TYPE_FACE,tol,1);
	if (entlist != 0)
	{
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick != 0)
		{
			LiViPickedEntityExpand(entpick,&entity,&type,&dis);
			LiBrFaceGetPlaneEquation(entity,plane);
		}
		LiViPickedEntityListDestroy(entlist);
/*		printf("dis = %g\n",dis);*/
	}
/*
.....Using inverse vector
*/
	um_vctmsc(vc,-1.,vc);
	entlist = LiViSolidPick(LW_viewport,pt,vc,LI_MW_COORD_SYSTEM_VIEW,
		LI_ENTITY_TYPE_FACE,tol,1);
	dis1 = 10000.;
	if (entlist != 0)
	{
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick != 0)
		{
			LiViPickedEntityExpand(entpick,&temp,&type,&dis1);
			LiViPickedEntityListDestroy(entlist);
		}
	}
/*
.....Find closest distance
*/
/*		printf("dis1 = %g\n",dis1);*/
	if (dis1 < dis)
	{
		dis = dis1;
		um_vctovc(vc,vcn);
		LiBrFaceGetPlaneEquation(temp,plane);
		entity = temp;
	}
	if (dis > Smaxdis) goto done;
	ang = um_dot(vcn,plane);
	if (fabs(ang) < fabs(cos(Smaxang/UM_RADIAN)))
	{
/*			printf("Angle too large, ang = %g   dis = %g\n",ang,dis);*/
		goto done;
	}
/*
.....Store minimum and maximum distances
*/
	dis1 = dis;
	if (um_dot(vcn,plane) < 0.) dis = dis * -1.;
/*
.....Find selected face
*/
	for (j=0;j<Snface;j++)
	{
		if (entity == tface[j].face) break;
	}
/*
.....If distance is less than previous distance
.....and face belongs to another surface then
.....make this face belong to the current surface
.....
.....Used when multiple surfaces are compared, because
.....sometimes a face will be found by the wrong surface
*/
	if (j == Snface) goto done;
	if (tface[j].used && dis1 < tface[j].toler && surf != tface[j].surf)
	{
/*			printf("Face %d removed from SF%d\n",j,tface[j].surf);*/
		tface[j].used = UU_FALSE;
		tface[j].surf = surf;
		tface[j].toler = 0.;
	}
/*
.....Modify face structure if distance warrants it
*/
/*			printf("Face = %d    Dis = %g   Dis1 = %g\n",tface[j].face,dis,dis1);*/
	if (j < Snface && (!tface[j].used || dis1 > tface[j].toler) ||
		tface[j].toler == 0.)
	{
/*
....If distance is greater, but
....Face is owned by another surface,
....Then ignore this distance
*/
		if (!tface[j].used || surf == tface[j].surf)
		{
			tface[j].used = UU_TRUE;
			tface[j].surf = surf;
			strcpy(tface[j].label,label);
			tface[j].toler = dis1;
			mat = Scol[8];
			LiBrFaceGetPlaneEquation(tface[j].face,plane);
			for (k=0;k<8;k++)
			{
				if (dis < Stol[k])
				{
					mat = Scol[k];
					break;
				}
			}
/*					printf("Face Color = %d\n",mat);*/
			tface[j].color = mat;
			if (dis < 0.)
			{
				if (dis > tface[j].ucut[0]) tface[j].ucut[0] = dis;
				if (dis < tface[j].ucut[1]) tface[j].ucut[1] = dis;
			}
			else
			{
				if (dis < tface[j].ocut[0]) tface[j].ocut[0] = dis;
				if (dis > tface[j].ocut[1]) tface[j].ocut[1] = dis;
			}
			retdis = dis;
		}
		else
		{
/*					printf("Face %d not used in SF%d\n",j,surf);*/
		}
	}
/*
.....End of routine
*/
done:;
	return(retdis);
}

/*********************************************************************
**    I_FUNCTION     : S_compare_solids(type,solid,nucut,ucut,nocut,ocut)
**			Compares the difference solids to cut model and removes any
**       difference solids that are within tolerance.
**    PARAMETERS
**       INPUT  :
**                type    = 2 = Undercuts, 3 = Overcuts, 4 = Both.
**                solid   = Difference solid(s) to compare.
**       OUTPUT :
**                nucut   = Number of undercuts.
**                ucut[0] = Smallest undercut of surface.
**                ucut[1] = Largest undercut of surface.
**                nocut   = Number of overcuts.
**                ocut[0] = Smallest overcut of surface.
**                ocut[1] = Largest overcut of surface.
**    RETURNS      :
**                UU_FAILURE if no points in tessellation could check to
**                the surface.  UU_SUCCESS normally.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_compare_solids(type,solid,nucut,ucut,nocut,ocut)
int type;
LtSessionPrim solid[2];
int *nucut,*nocut;
UU_REAL ucut[2],ocut[2];
{
	int i,j,mode,nsol[2],status,ix;
	int *itype;
	UU_REAL lucut[2],locut[2];
	LtPrim prim;
	LtData data;
	char buf[20];
	S_solid sol;
/*
.....Initialize routine
*/
	*nucut = 0;
	*nocut = 0;
	lucut[0] = 100000.; lucut[1] = -100000.;
	locut[0] = 100000.; locut[1] = -100000.;
	ucut[0] = ucut[1] = ocut[0] = ocut[1] = 0.;
	status = UU_SUCCESS;
	itype = UU_NULL;
/*
.....Get number of solids in both Undercuts & Overcuts
*/
	Snptr = 0;
	for (i=0;i<2;i++)
	{
		if (i == 1 && type !=4) break;
		nsol[i] = LiViSolidGetNumParts(solid[i]);
		Snptr = Snptr + nsol[i];
	}
	if (Snptr == 0) goto done;
/*
.....Allocate memory for solids
*/
	Ssolptr = (LtSessionPrim *)LiAllocate(Snptr*sizeof(LtSessionPrim));
	if (Ssolptr == UU_NULL) goto failed;
	itype = (int *)LiAllocate(Snptr*sizeof(int));
	if (itype == UU_NULL) goto failed;
/*
.....Split the solids
*/
	ix = 0;
	for (i=0;i<2;i++)
	{
		if (i == 1 && type !=4) break;
		if (nsol[i] == 0) continue;
		mode = 0;
		if (type == 3 || i == 1) mode = 1;
		LiViSolidSplit(solid[i],&Ssolptr[ix]);
		for (j=0;j<nsol[i];j++) itype[ix+j] = mode;
		ix = ix + nsol[i];
	}
	if (type == 3)
	{
		nsol[1] = nsol[0];
		nsol[0] = 0;
	}
/*
.....Loop through solids
*/
/*printf("Number of solids = %d\n",Snptr);*/
	for (i=0;i<Snptr;i++)
	{
/*
.....Get volume of solid
*/
		sol.type = itype[i];
		sol.prim = Ssolptr[i];
		status = LiViSolidGetVolume(Ssolptr[i],&sol.vol);
/*
.....Remove any solids that are too small
*/
		if (sol.vol < Sminvol)
		{
/*
		 	LiSessionPrimGetProperty(Ssolptr[i],LI_SPRIM_PROP_PRIM,&data);
			prim = LiDataGetGenericPtr(&data);
			LiSessionRemovePrim(Ssolptr[i]);
			LiPrimitiveDestroy(prim);
*/
			Ssolptr[i] = 0;
			nsol[itype[i]]--;
		}
/*
.....Store min/max values
*/
		else
		{
			if (itype[i] == 0 && status == 0)
			{
				if (sol.vol < lucut[0]) lucut[0] = sol.vol;
				if (sol.vol > lucut[1]) lucut[1] = sol.vol;
			}
			else if (itype[i] == 1 && status == 0)
			{
				if (sol.vol < locut[0]) locut[0] = sol.vol;
				if (sol.vol > locut[1]) locut[1] = sol.vol;
			}
/*
.....Get the bounds of the solid
*/
			LiSessionPrimGetProperty(Ssolptr[i],LI_SOLID_PROP_MW_BOUNDS,&data);
			LiDataGetDoubleBounds(&data,sol.bounds);
/*
.....Analyze the part
.....in the area of this difference
*/
			sol.analyzed = UU_FALSE;
/*			if (Sanalyze)
			{
		 		LiSessionPrimGetProperty(Ssolptr[i],LI_SPRIM_PROP_PRIM,&data);
				prim = LiDataGetGenericPtr(&data);
				LiSessionRemovePrim(Ssolptr[i]);
				LiPrimitiveDestroy(prim);
				Ssolptr[i] = UU_NULL;
				status = S_analyze_solid(&sol,&Ssolptr[i]);
			}*/
/*
.....Tessellate each solid individually
*/
			if (Sanalyze)
			{
				prim = LiPrimitiveSolidExtract(Ssolptr[i]);
				if (prim == 0) continue;
				status = LiPrimitiveGetSolidMesh(prim,
					LI_MW_MESH_TRIANGLES|LI_MW_MESH_OUTPUT_PLANES,
					(LtFuncMWMeshBegin)S_mesh_begin,
					(LtFuncMWMeshAddPolygon)S_mesh_polygon,UU_NULL,UU_NULL);
				LiPrimitiveDestroy(prim);
				if (status == 0)
				{
/*
.....Compare this solid to the cut model
*/
					sprintf(buf,"Solid #%d\n",nsol[itype[i]]);
					buf[0] = '\0';
					status = S_compare_part(0,buf,&Stess,lucut,locut);
/*
.....Get rid of solids within tolerance
*/
/*printf("Solid #%d Undercut = %f,%f   Overcut = %f,%f\n",i,lucut[0],lucut[1],
locut[0],locut[1]);*/
					if ((mode == 0 && lucut[1] > Stol[3]) ||
						(mode == 1 && locut[1] < Stol[4]))
					{
/*printf("Solid #%d deleted\n",i);*/
/* HERE
		 				LiSessionPrimGetProperty(Ssolptr[i],LI_SPRIM_PROP_PRIM,
							&data);
						prim = LiDataGetGenericPtr(&data);
						LiSessionRemovePrim(Ssolptr[i]);
						LiPrimitiveDestroy(prim);
*/
						Ssolptr[i] = 0;
						nsol[itype[i]]--;
					}
/*
.....Free tessellation
*/
					um_free_tess(&Stess);
				}
			}
/* END HERE */
/*
.....Store this solid
*/
			if (Ssolptr[i] != 0)
			{
				uu_list_push(&Ssolid,&sol);
				Snsolid++;
			}
		}
	}
/*
.....Store number of under/over cuts
*/
	*nucut = nsol[0];
	if (lucut[0] != 100000.) ucut[0] = lucut[0];
	if (lucut[1] != -100000.) ucut[1] = lucut[1];
	*nocut = nsol[1];
	if (locut[0] != 100000.) ocut[0] = locut[0];
	if (locut[1] != -100000.) ocut[1] = locut[1];
	goto done;
/*
.....End of routine
*/
failed:;
	status = UU_FAILURE;
done:;
	if (itype != UU_NULL) LiDeallocate(itype);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_hilite_faces()
**			Initializes all face attributes and distances in preparation
**       for comparison.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_hilite_faces()
{
	int i,inc,ifl;
	LW_stock_struc *sd,*sdtmp;
	S_face tface,*mface;
	LtBody sbody;
	LtAttributeClass class;
	LtData stuff;
/*
.....Faces have already been retrieved
.....Just reset them now
*/
	if (Snface != 0)
	{
		mface = (S_face *)UU_LIST_ARRAY(&Sface);
		for (i=0;i<Snface;i++)
		{
			mface[i].used = UU_FALSE;
			mface[i].color = -1;
			mface[i].ucut[0] = -10000.;
			mface[i].ucut[1] = 10000.;
			mface[i].ocut[0] = 10000.;
			mface[i].ocut[1] = -10000.;
		}
	}
/*
.....Loop through all stocks
*/
	else
	{
		sd = LW_stock_first[0];
		for (i=0;i<LW_nstock[0];i++)
		{
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
				if (ifl == -2) break;
				sbody = LiViSolidGetBody(sdtmp->stock);
				tface.face = LiBrBodyGetFirstFace(sbody);
/*
.....Get all faces for current stock
.....And set their color to the maximum tolerance color
*/
				class = LiViGetPredefinedAttributeClass(LI_VI_ATTRIB_FACE_MATERIAL);
				tface.color = 0 /*Scol[8]*/;
				tface.used = UU_FALSE;
				tface.ucut[0] = -10000.;
				tface.ucut[1] = 10000.;
				tface.ocut[0] = 10000.;
				tface.ocut[1] = -10000.;
				while (tface.face != 0)
				{
					uu_list_push(&Sface,&tface);
					Snface++;
/*					LiViEntitySetAttribute(tface.face,LI_ENTITY_TYPE_FACE,class,
						LW_material[Scol[8]]);*/
					tface.face = LiBrFaceGetNextFace(tface.face);
				}
			} while (ifl != -1);
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
/*
.....Invisible all fixtures and tool
*/
		sd = LW_stock_first[1];
		LiDataSetBoolean(&stuff,FALSE);
		for (i=0;i<LW_nstock[1];i++)
		{
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
				if (ifl == -2) break;
				LiMWViewportSetSessPrimProperty(LW_viewport,sdtmp->stock,
					LI_VPSP_PROP_MW_VISIBLE,&stuff);
			} while (ifl != -1);
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
		for (inc=0;inc<LW_spindle_num;inc++)
		{
			if (LW_tool[inc] != 0)
				LiMWViewportSetSessPrimProperty(LW_viewport,LW_tool[inc],
					LI_VPSP_PROP_MW_VISIBLE,&stuff);
			if (LW_shank[inc] != 0)
				LiMWViewportSetSessPrimProperty(LW_viewport,LW_shank[inc],
					LI_VPSP_PROP_MW_VISIBLE,&stuff);
			for (i=0;i<LW_num_holder[inc];i++)
				if (LW_holder[inc][i] != 0)
					LiMWViewportSetSessPrimProperty(LW_viewport,LW_holder[inc][i],
						LI_VPSP_PROP_MW_VISIBLE,&stuff);
		}
	}
}

/*********************************************************************
**    S_FUNCTION     : S_mesh_begin(prim,nface,nloop,nindx,nvert,vert)
**			Routine called to intialize mesh output from STL model.
**    PARAMETERS
**       INPUT  :
**                prim    = STL model primitive.
**                nface   = Number of faces in STL model.
**                nloop   = Number of loops in STL model.
**                nindx   = Number of vertex indices in STL model.
**                nvert   = Number of vertices in STL model.
**                vert    = Pointer to array of vertices in STL model.
**       OUTPUT :
**                none.
**    RETURNS      :
**                UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static LtStatus S_mesh_begin(prim,nface,nloop,nindx,nvert,vert)
LtGenericPtr prim;
LtNat32 nface,nloop,nindx,nvert;
LtDoublePoint **vert;
{
	int i;
	Svert = vert;
/*printf("Nface = %d   Nloop = %d   Nindx = %d   Nvert = %d\n",nface,nloop,
nindx,nvert);*/
	um_init_tess(&Stess);
	uu_list_init(&Stess.vertices,sizeof(UM_coord),nvert,100);
	uu_list_init(&Stess.normals,sizeof(UM_coord),nvert,100);
	Svert_index = (int *)uu_malloc(nindx*sizeof(int));
	if (Svert_index != UU_NULL)
	{
		for (i=0;i<nindx;i++) Svert_index[i] = 0;
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    S_FUNCTION     : S_mesh_polygon(polyg)
**			Routine called to pass in a polygon from the STL model when
**       creating a mesh representation for comparison.
**    PARAMETERS
**       INPUT  :
**                polygon = Polygon from STL model.
**       OUTPUT :
**                none.
**                ucut[1] = Largest undercut of surface.
**                ocut[0] = Smallest overcut of surface.
**                ocut[1] = Largest overcut of surface.
**    RETURNS      :
**                UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static LtStatus S_mesh_polygon(polyg)
LtMeshPolygon *polyg;
{
	int i,j,inc,ip;
	UU_REAL x,y,z;
	UM_coord pt;
	UM_vector *vc;
	LtDoublePoint dpt;
/*
.....Store polygon data in tessellation list
*/
/*printf("\nnum_loops = %d\n",polyg->num_loops);
printf("Plane = %g,%g,%g\n",
polyg->plane_equation[0],polyg->plane_equation[1],polyg->plane_equation[2]);*/
	for (i=0;i<polyg->num_loops;i++)
	{
/*printf("vertices = %d\n",polyg->vertices_per_loop[i]);*/
		x = y = z = 0.;
		for (j=0;j<polyg->vertices_per_loop[i];j++)
		{
			inc = polyg->indices[j] * 3;
			LiMeshGetVertexPosition(dpt,Svert,Snvert,polyg->indices[j]);
			x = x + dpt[0]; y = y + dpt[1]; z = z + dpt[2];
/*
........Add new point
*/
			if (Svert_index != UU_NULL && Svert_index[inc] == 0)
			{
/*printf("inc = %d\n",inc);
printf("Vertex = %g,%g,%g\n",Svert[inc],Svert[inc+1],Svert[inc+2]);*/
				uu_list_push(&Stess.vertices,dpt);
				uu_list_push(&Stess.normals,polyg->plane_equation);
				Stess.np++;
				if (Svert_index != UU_NULL) Svert_index[inc] = Stess.np;
			}
/*
........This point was already added
........Calculate new normal
*/
			else
			{
				ip = Svert_index[inc] - 1;
				vc = (UM_vector *)UU_LIST_ARRAY(&Stess.normals);
/*printf("Start vector %d = %g,%g,%g\n",ip,vc[ip][0],vc[ip][1],vc[ip][2]);*/
				um_vcplvc(vc[ip],polyg->plane_equation,vc[ip]);
/*printf("Add vector %d = %g,%g,%g\n",ip,vc[ip][0],vc[ip][1],vc[ip][2]);*/
			}
		}
/*
........Add center of polygon
*/
		pt[0] = x / polyg->vertices_per_loop[i];
		pt[1] = y / polyg->vertices_per_loop[i];
		pt[2] = z / polyg->vertices_per_loop[i];
		uu_list_push(&Stess.vertices,pt);
		uu_list_push(&Stess.normals,polyg->plane_equation);
		Stess.np++;
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    S_FUNCTION     : S_place_solid(prim,tfmat);
**			Applies a transformation matrix to a solid primitive.
**    PARAMETERS
**       INPUT  :
**          prim    = Solid primitive to apply transformation to.
**          tfmat   = Transformation matrix.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_place_solid(prim,tfmat)
LtPrim prim;
UM_transf tfmat;
{
	LtMatrix mx1;
	LtTransform xf;
	LtPref pref;
/*
.....Convert Xform to MachineWorks Matrix
*/
	if (prim != 0)
	{
/*
.....Transform solid
*/
		ncl_43mx_to_44mx(tfmat,mx1);
		xf = LiTransformDefine(mx1);
		if (xf != 0)
		{
			if (Sadded)
			{
				LiSessionRemovePrim(
					LiPrimitiveGetSessionPrim(prim,LW_session[LW_mach_mode]));
			}
			LiPrimitiveSetSolidTransform(prim,xf);
			LiTransformDestroy(xf);
			if (Sadded) LiSessionAddPrim(LW_session[LW_mach_mode],prim);
		}
	}
}

/*********************************************************************
**    S_FUNCTION     : S_remove_diff()
**			Removes previously defined difference solids.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_remove_diff()
{
	int i;
	LtPrim prim;
	LtData stuff;
/*
.....Get rid of any previous difference solids
*/
	for (i=0;i<Sndifs;i++)
		LiViSolidUnsplit(Spdif[i]);
	if (Ssolptr != UU_NULL)
	{
/*
		for (i=0;i<Snptr;i++)
		{
			if (Ssolptr[i] != 0)
			{
				LiSessionPrimGetProperty(Ssolptr[i],LI_SPRIM_PROP_PRIM,&stuff);
				prim = LiDataGetGenericPtr(&stuff);
				LiSessionRemovePrim(Ssolptr[i]);
				LiPrimitiveDestroy(prim);
			}
		}
*/
		LiDeallocate(Ssolptr);
		Ssolptr = UU_NULL;
	}
/*
	for (i=0;i<Sndifs;i++)
	{
		LiSessionPrimGetProperty(Spdif[i],LI_SPRIM_PROP_PRIM,&stuff);
		prim = LiDataGetGenericPtr(&stuff);
		LiSessionRemovePrim(Spdif[i]);
		LiPrimitiveDestroy(prim);
	}
*/
	Sndifs = 0;
	if (Sadded)
	{
		LiSessionRemovePrim(Sesstarget);
		Sesstarget = 0;
		Sadded = UU_FALSE;
	}
}

/*********************************************************************
**    S_FUNCTION     : S_reset_comparison()
**			Resets the NCLIPV stocks and deletes all comparison memory.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_reset_comparison()
{
	int i,ifl;
	LW_stock_struc *sd,*sdtmp;
	LtData transp;
/*
.....Reset translucency of stocks
*/
	sd = LW_stock_first[0];
	for (i=0;i<LW_nstock[0];i++)
	{
		ifl = 0;
		do
		{
			ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
			if (ifl == -2) break;
			LiDataSetNat32(&transp,100-sdtmp->translucency);
			LiSessionPrimSetVisualProperty(sdtmp->stock,
				LI_MW_VIS_PROP_TRANSPARENCY,&transp);
		} while (ifl != -1);
		sd = (LW_stock_struc *)uu_lsnext(sd);
	}
/*
.....Get rid of any previous difference solids
*/
	S_remove_diff();
/*
.....Restore the saved session
*/
	um_delv_axis_ipv();
	LiMWEnvironmentRestore(Senv);
/*
.....Get rid of the target solid
*/
	if (Sprim != 0) LiPrimitiveDestroy(Sprim);
	Sprim = 0;
	if (Starget != 0) LiPrimitiveDestroy(Starget);
	Starget = 0;
	if (Snface != 0) uu_list_free(&Sface);
	Snface = 0;
	if (Snsolid != -1) uu_list_free(&Ssolid);
	Snsolid = -1;
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_compare()
**       Controlling routine for the NCLIPV visual compare form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ul_ipv_compare()
{
	int i,status,color,ifl;
	UU_LOGICAL imark;
	static int redisp_form = 0;
	S_geo *geo;
	struct NCL_fixed_databag e;
	LW_stock_struc *sd,*sdtmp;
	LtData transp;
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1,1, 1,1,1, 1,1,1,1, 1,1,1,1,1, 1,
		1,1,1,1,1,1,1};
	static UD_METHOD methods[] = {OnType,OnSelect,OnColor,OnDesel,
		OnBrowse,OnText,UU_NULL,
		OnMatrix,OnMxname,OnLoadmx,OnModmx,
		OnVis,OnView,ul_ipv_compare_tol,ul_ipv_compare_mod,UU_NULL,
		UU_NULL,
		OnFace,ul_ipv_compare_find,OnCompare,OnLog,OnClear,UU_NULL,OnInterrupt};
	static char called[] = {6,6,6,6, 6,6,6, 6,6,6,6, 6,6,6,6,6, 6, 6,6,6,6,6, 6,
		6};
	static char display[] = {1,1,1,1, 1,1,1, 1,1,1,1, 1,1,1,1,1, 1, 1,1,1,1,1,
		1,1};
	static int *ans[] = {(int *)&Slist,UU_NULL,(int *)&Scolor,UU_NULL,
		UU_NULL,(int *)Sfile,(int *)&Sunits,
		(int *)&Smxflag,(int *)&Smxlist,UU_NULL,UU_NULL,
		&Svis,UU_NULL,UU_NULL,UU_NULL,(int *)&Strans,
		(int *)&Sresults,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,&Sprog,UU_NULL};
/*
.....Initialize routine
*/
	if (Sform_active) return(UU_SUCCESS);
	Sactive = UU_FALSE;
	Sactive1 = UU_FALSE;
	Sactive2 = UU_FALSE;
	Starget = 0;
	Sprim = 0;
	Stess.np = 0;
	Smxchg = Smxflag;
	Sadded = UU_FALSE;
	Sprog = 0;
	imark = UU_FALSE;
	if (!LW_active)
	{
		ud_wrerr("NCLIPV must be active for part comparisons.");
		return(UU_SUCCESS);
	}
/*
.....Command Reject
*/
	imark = UU_TRUE;
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		redisp_form = UU_FALSE;
		goto done;
	}
/*
.....Save the current session
*/
	um_delv_axis_ipv();
	Senv = LiMWEnvironmentSave(LW_session[LW_mach_mode]);
/*
.....Initialize geometry list
*/
	uu_list_init(&Ssurf,sizeof(S_geo),50,50);
/*
.....Initialize fields
*/
	Stype = LW_compare.type;
	if (LW_nclipv==LW_STANDALONE)
		Stype = 1;
/*
.....Initialize compare list
*/
	Slist.item = (char **)uu_malloc(3*sizeof(char *));
	if (Slist.item == UU_NULL) goto nomem;
	Slist.answer = (char *)uu_malloc(sizeof(char)*80);
	if (Slist.answer == UU_NULL) goto nomem;
/*
.....Store source list toggles
*/
	if (LW_nclipv==LW_INPROCESS)
	{
		for (i=0;i<3;i++)
		{
			Slist.item[i] = (char *)uu_malloc(sizeof(char)*80);
			strcpy(Slist.item[i], Scmptxt[i]);
			if (i == Stype) strcpy(Slist.answer, Scmptxt[i]);
		}
		Slist.num_item = 3;
	}
	else
/*
......for NCLIPV application
*/
	{
		Slist.item[0] = (char *)uu_malloc(sizeof(char)*80);
		strcpy(Slist.item[0], Scmptxt[1]);
		strcpy(Slist.answer, Scmptxt[1]);
		Slist.num_item = 1;
	}
	Scolor = LW_compare.hilite;
	strcpy(Sfile,LW_compare.file);
	Sunits = LW_compare.units;
	Svis = LW_compare.vis;
	if (LW_mach_mode == LW_RAPIDCUT) Svis = 1;
	Strans = LW_compare.translucency;
	Sresults.item = (char **)uu_malloc(10000*sizeof(char*));
	Sresults.num_item = 0;
	Sresults.answer = (char *)uu_malloc(80*sizeof(char));
	strcpy(Sresults.answer," ");
	nclu_load_matrix_list(&Smxlist,Smxname);
/*
.....Colors form
*/
	UM_len_inttoext(LW_compare.auto_toler,Sauto);
	for (i=0;i<8;i++)
	{
		UM_len_inttoext(LW_compare.toler[i],Stol[i]);
		Scol[i] = LW_compare.color[i];
	}
	Scol[8] = LW_compare.color[8];
/*
.....Tolerances form
*/
	Sgrid_flag = LW_compare.grid_flag;
	Sgrid = LW_compare.grid;
	UM_len_inttoext(LW_compare.sftol,Sftol);
	Smaxang = LW_compare.maxang;
	UM_len_inttoext(LW_compare.maxdis,Smaxdis);
	UM_len_inttoext(LW_compare.minvol,Sminvol);
	Sanalyze = LW_compare.analyze;
/*
.....Set traverse masks
*/
	if (Stype == SURF_TOG)
	{
		traverse[FSEL] = UU_TRUE;
		traverse[FHIL] = UU_TRUE;
		traverse[FDES] = UU_TRUE;
		traverse[FBRO] = UU_FALSE;
		traverse[FSTL] = UU_FALSE;
		traverse[FUNI] = UU_FALSE;
	}
	else if (Stype == STL_TOG)
	{
		traverse[FSEL] = UU_FALSE;
		traverse[FHIL] = UU_FALSE;
		traverse[FDES] = UU_FALSE;
		traverse[FBRO] = UU_TRUE;
		traverse[FSTL] = UU_TRUE;
		traverse[FUNI] = UU_TRUE;
	}
	else
	{
		traverse[FSEL] = UU_TRUE;
		traverse[FHIL] = UU_TRUE;
		traverse[FDES] = UU_FALSE;
		traverse[FBRO] = UU_FALSE;
		traverse[FSTL] = UU_FALSE;
		traverse[FUNI] = UU_FALSE;
	}
	if (Svis == 1)
	{
		traverse[FMOD] = UU_FALSE;
		traverse[FTRA] = UU_FALSE;
	}
	else
	{
		traverse[FMOD] = UU_TRUE;
		traverse[FTRA] = UU_TRUE;
	}
	if (Stype == STL_TOG)
		traverse[FMFL] = UU_TRUE;
	else
		traverse[FMFL] = UU_FALSE;
	if (Smxflag && Stype == STL_TOG)
	{
		traverse[FMNM] = UU_TRUE;
		traverse[FMLD] = UU_TRUE;
		traverse[FMMD] = UU_TRUE;
	}
	else
	{
		traverse[FMNM] = UU_FALSE;
		traverse[FMLD] = UU_FALSE;
		traverse[FMMD] = UU_FALSE;
	}
	traverse[FVIS] = UU_TRUE;
	if (LW_mach_mode == LW_RAPIDCUT) traverse[FVIS] = UU_FALSE;
	traverse[FFAC] = UU_FALSE;
	traverse[FFND] = UU_FALSE;
	traverse[FLOG] = UU_FALSE;
	traverse[FCLR] = UU_FALSE;
	traverse[FPRG] = UU_FALSE;
	traverse[FITR] = UU_FALSE;
	if (Svis == 0)
	{
		traverse[FPRG] = UU_TRUE;
		traverse[FITR] = UU_TRUE;
	}
	if (LW_nclipv == LW_STANDALONE)
	{
		traverse[FMNM] = traverse[FMLD] = 0;
		display[FMNM] = display[FMLD] = 0;
	}
/*
.....Get the Form input
*/
form:;
	redisp_form = 1;
	Sform_active = UU_TRUE;
	status = ud_form1("ipvcomp.frm",ans,ans,methods,called,display,traverse);
	if (status==-1) goto done;
/*
.....Save the form
*/
	LW_compare.type = Stype;
	LW_compare.hilite = Scolor;
	strcpy(LW_compare.file,Sfile);
	LW_compare.units = Sunits;
	LW_compare.vis = Svis;
	LW_compare.translucency = Strans;
	strcpy(Smxname,Smxlist.answer);
/*
.....Colors form
*/
	UM_len_exttoint(Sauto,LW_compare.auto_toler);
	for (i=0;i<8;i++)
	{
		UM_len_exttoint(Stol[i],LW_compare.toler[i]);
		LW_compare.color[i] = Scol[i];
	}
	LW_compare.color[8] = Scol[8];
/*
.....Tolerances form
*/
	LW_compare.grid_flag = Sgrid_flag;
	LW_compare.grid = Sgrid;
	UM_len_exttoint(Sftol,LW_compare.sftol);
	LW_compare.maxang = Smaxang;
	UM_len_exttoint(Smaxdis,LW_compare.maxdis);
	UM_len_exttoint(Sminvol,LW_compare.minvol);
	LW_compare.analyze = Sanalyze;
/*
.....Save the modals file
*/
	S_save_modfile();
	goto done;
/*
.....Could not allocate memory
*/
nomem:
	ud_wrerr("Could not allocate memory for form.");
	goto done;
/*
.....End of Routine
*/
done:;
	redisp_form = 0;
	Sform_active = UU_FALSE;
	ud_free_flist(&Smxlist);
/*
.....Reset translucency of stocks
*/
	sd = LW_stock_first[0];
	for (i=0;i<LW_nstock[0];i++)
	{
		ifl = 0;
		do
		{
			ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
			LiDataSetNat32(&transp,100-sdtmp->translucency);
			LiSessionPrimSetVisualProperty(sdtmp->stock,
				LI_MW_VIS_PROP_TRANSPARENCY,&transp);
			if (ifl == -2) break;
		} while (ifl != -1);
		sd = (LW_stock_struc *)uu_lsnext(sd);
	}
/*
.....Restore IPV to pre-comparison state
*/
	S_reset_comparison();
/*
.....Redraw the IPV window
*/
	ul_ipv_flush();
	um_reset_pocket_graphics(UM_IPV_WINDOW);
/*	if (Sactive) ud_close_dispfrm(Sfrm);
	if (Sactive1) ud_close_dispfrm(Sfrm1);*/
/*
.....Restore the entity colors
*/
	geo = (S_geo *)UU_LIST_ARRAY(&Ssurf);
	for (i=0;i<UU_LIST_LENGTH(&Ssurf);i++)
	{
		e.key = geo[i].key;
		color = geo[i].color;
		ncl_update_color(e.key,color);
		uc_retrieve_data(&e,sizeof(e));
		uc_display(&e);
	}
	uu_list_free(&Ssurf);
	ud_free_flist(&Sresults);
	if (Stess.np != 0) um_free_tess(&Stess);
/*
.....Reset Reject OP
*/
	if (imark)
	{
		UD_UNMARK(mainmarkval);
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_compare_mod()
**       Controlling routine for the NCLIPV visual compare colors form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT ul_ipv_compare_mod(fieldno, val, stat)
{
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1, 1,1,1,1,1,1,1,1, 1, 1,1,1,1,1,1,1,1};
	static UD_METHOD methods[] = {OnAuto,OnModal,OnReset, OnModal,UU_NULL,
		OnModal,UU_NULL, OnModal,UU_NULL, OnModal,UU_NULL, UU_NULL,
		OnModal,UU_NULL, OnModal,UU_NULL, OnModal,UU_NULL, OnModal,UU_NULL,
		OnClose};
	static char called[] = {6,6,6, 6,6,6,6,6,6,6,6, 6, 6,6,6,6,6,6,6,6, 6};
	static char display[] = {1,1,1, 1,1, 1,1,1,1,1,1,1,1, 1,1, 1,1,1,1,1,1,1,1};
	static int *ans[] = {UU_NULL,(int *)&Sauto,UU_NULL,
		(int *)&Stol[0],(int *)&Scol[0],(int *)&Stol[1],(int *)&Scol[1],
		(int *)&Stol[2],(int *)&Scol[2],(int *)&Stol[3],(int *)&Scol[3],
		(int *)&Scol[4],(int *)&Stol[4],(int *)&Scol[5],
		(int *)&Stol[5],(int *)&Scol[6],(int *)&Stol[6],(int *)&Scol[7],
		(int *)&Stol[7],(int *)&Scol[8]};
/*
.....Make sure form is not already active
*/
	if (Sactive) goto done;
/*
.....Get the Form input
*/
	Sfrm = ud_form_display1("ipvcompmod.frm", ans, ans, methods, called, display,
		traverse);
	if (Sfrm != -1) Sactive = UU_TRUE;
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_compare_tol()
**       Controlling routine for the NCLIPV visual compare tolerances form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT ul_ipv_compare_tol(fieldno, val, stat)
{
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1, 1,1, 1,1,1};
	static UD_METHOD methods[] = {OnToler,OnToler, OnToler,OnToler,
		UU_NULL,OnToler, OnClose1};
	static char called[] = {6,6, 6,6, 6, 6,6};
	static char display[] = {1,1, 1,1, 1,1};
	static int *ans[] = {(int *)&Sgrid,(int *)&Sftol,(int *)&Smaxang,
		(int *)&Smaxdis,(int *)&Sgrid_flag,(int *)&Sminvol};
/*
.....Make sure form is not already active
*/
	if (Sactive1) goto done;
/*
.....Get the Form input
*/
	Sfrm1 = ud_form_display1("ipvcomptol.frm", ans, ans, methods, called,
		display, traverse);
	if (Sfrm1 != -1) Sactive1 = UU_TRUE;
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_compare_find()
**       Controlling routine for the NCLIPV Find Gouges form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT ul_ipv_compare_find(fieldno, val, stat)
{
/*
.....Set up form fields
*/
	static char traverse[]     = {1, 1,1, 1, 1,1, 1};
	static UD_METHOD methods[] = {OnFit,OnRewind,OnBack,OnSeg,OnFwd,
		OnFfwd,OnView,OnClose2};
	static char called[] = {6, 6,6, 6, 6,6, 6};
	static char display[] = {1, 1,1, 1, 1,1, 1};
	static int *ans[] = {UU_NULL,UU_NULL,UU_NULL,(int *)&Sfndsol,UU_NULL,
		UU_NULL,UU_NULL};
/*
.....Make sure form is not already active
*/
	if (Sactive2) goto done;
/*
.....Take down main form
*/
	ud_form_invis();
/*
.....Get the Form input
*/
	Sfrm2 = ud_form_display1("ipvcompfind.frm", ans, ans, methods, called,
		display, traverse);
	if (Sfrm2 != -1) Sactive2 = UU_TRUE;
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : S_save_modfile()
**       Save the NCLIPV Compare settings into modals file.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_modfile()
{
	int stat;
	char msg[80];
	FILE *fptr;
	UX_pathname fname;
	static char yesno[2][10] = {"*NO","*YES"};
	static char units[2][10] = {"*INCH","*MM"};
	static char srf[3][10] = {"*SURFACES","*STL_FILE","*SHAPE"};
	static char stype[5][10] = {"*REPORT","*VISUAL","*UNDERCUT","*OVERCUT",
		"*BOTH"};
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy(fname,"nclipv_compare.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store compare modals
*/
	ux_fputs0("#COMPARE#\n", fptr);

	sprintf(msg,"/COMPARE/ %s\n",srf[LW_compare.type]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/SEL_COLOR/ %s\n",uw_color_name[LW_compare.hilite]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/UNITS/ %s\n",units[LW_compare.units]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/TYPE/ %s\n",stype[LW_compare.vis]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/VOLUME/ %g\n",LW_compare.minvol);
	ux_fputs0(msg,fptr);

/*	sprintf(msg,"/ANALYZE/ %s\n",yesno[LW_compare.analyze]);
	ux_fputs0(msg, fptr);*/

	sprintf(msg,"/TRANSLUCENCY/ %d\n",LW_compare.translucency);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/GRID/ %d\n",LW_compare.grid);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/TOLERANCE/ %g\n",LW_compare.sftol);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/ANGULAR/ %g\n",LW_compare.maxang);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/LINEAR/ %g\n",LW_compare.maxdis);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/GRID_ONLY/ %s\n",yesno[LW_compare.grid_flag]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/NEG_TOLER/ %g,%g,%g,%g\n",LW_compare.toler[0],
		LW_compare.toler[1],LW_compare.toler[2],LW_compare.toler[3]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/AUTO_TOLER/ %g\n",LW_compare.auto_toler);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/POS_TOLER/ %g,%g,%g,%g\n",LW_compare.toler[4],
		LW_compare.toler[5],LW_compare.toler[6],LW_compare.toler[7]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/NEG_COLOR/ %s,%s,%s,%s\n",uw_color_name[LW_compare.color[0]],
		uw_color_name[LW_compare.color[1]],uw_color_name[LW_compare.color[2]],
		uw_color_name[LW_compare.color[3]]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/TOL_COLOR/ %s\n",uw_color_name[LW_compare.color[4]]);
	ux_fputs0(msg,fptr);

	sprintf(msg,"/POS_COLOR/ %s,%s,%s,%s\n",uw_color_name[LW_compare.color[5]],
		uw_color_name[LW_compare.color[6]],uw_color_name[LW_compare.color[7]],
		uw_color_name[LW_compare.color[8]]);
	ux_fputs0(msg,fptr);
/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}
