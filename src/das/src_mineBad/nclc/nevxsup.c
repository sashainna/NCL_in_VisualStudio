/*********************************************************************
**    NAME         :  nevxsup.c
**       CONTAINS:  Routines to interface to Varimetrix.
**           int vx_ncl()
**           int ncl_vx_find (label, isub, nclkey, nwds, ietype)
**           int ncl_vx_disp (eptr, tfmat, attptr)
**           int ncl_vx_disp_curve (key)
**           int ncl_vx_disp_surf (eptr)
**           int ncl_copy_vxline (e1, e2, bagsize)
**           int ncl_copy_vxcirc (e1, e2, bagsize)
**           int ncl_copy_vxsrf (e1, e2, bagsize)
**           int ncl_vx_getmodel ()
**           int ncl_vx_getdir (dir)
**           int ncl_vx_getppname (ppname)
**           int ncl_vx_set_attr (idx)
**           int ncl_vx_set_blank (idx, key)
**           int ncl_vx_create (idx, ietype, vxlab, e1)
**           int ncl_vx_compcrv (n1, vxcvl, fct, lab, ijoin, keycv)
**           int ncl_vx_create_curve (vxcrv, vxlab, crv)
**           int vxload()
**           int vxjump(iflg)
**           int vx_gettrm (nline, f77_prompt, f77_cin)
**           vx_error(kerr,cout)
**           vx_ersw3()
**           vx_getsrc(cin,kline)
**           vx_includ(cfil,kinc)
**           vx_putapt(cout,knc)
**           vx_putmsg(cout,knc,klin,kers)
**           vx_putw2(cout,knc)
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nevxsup.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:58
*********************************************************************/

#include "usysdef.h"

#define NCL_ICONS_FOR_INTER 1
#include "nclicons.h"
#undef NCL_ICONS_FOR_INTER

#if UU_COMP == UU_VAXVMS || UU_COMP == UU_WIN2K || UU_COMP == UU_WINNT
void vx_ncl() {}
void ncl_vx_find () {}
void ncl_vx_disp () {}
void ncl_vx_disp_curve () {}
void ncl_vx_disp_surf () {}
void ncl_copy_vxline () {}
void ncl_copy_vxcirc () {}
void ncl_copy_vxsrf () {}
void ncl_vx_getmodel () {}
void ncl_vx_getdir () {}
void ncl_vx_getppname () {}
void ncl_vx_set_attr () {}
void ncl_vx_set_blank () {}
void ncl_vx_create () {}
void ncl_vx_compcrv () {}
void ncl_vx_create_curve () {}
void vxload() {}
void vxjump(iflg) {}
void vx_gettrm () {}
void vx_error() {}
void vx_ersw3() {}
void vx_getsrc() {}
void vx_includ() {}
void vx_putapt() {}
void vx_putmsg() {}
void vx_putw2() {}
void ncl_vx_plotm() {}
void ncl_vx_get_units() {}
void ncl_vx_setunits() {}
void vx_mdl_inq_index() {}
void vx_mdl_inq_set() {}
#else

#include "driver.h"
#include "dmenucom.h"
#include "uhep.h"
#include "g.h"
#include "view.h"
#include "mfort.h"
#include "modef.h"
#include "mcrv.h"
#include "msrf.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdeval.h"
#include "mdrel.h"
#include "lcom.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclvx.h"

#include "X11/Xlib.h"


#include "wsxw.h"
#include "openvx_mdl.h"
#include "openvx_uif.h"

/*   ------   FIX   ------ Remove this define when _inq_ is fixed */
/* #define INQBUG */
/*   ------ END FIX ------ Remove this define when _inq_ is fixed */
static UX_pathname NCL_vx_dir;
static UX_pathname NCL_vx_ppname;

/*
.....map for line type
.....Yurong 4/21/99
*/
int NCL_vx_lntype_table[6] = { 0, 1, 2, 3, 4, 6 };
int *NCL_vx_color_table[16] = {
	&BLACK,
	&GREEN,
	&RED,
	&ORANGE,
	&YELLOW,
	&CYAN,
	&MAGENTA,
	&LTBLUE,
	&BLUE,
/*
.....should map to light blue
.....Yurong 4/21/99
	&MAGENTA,
*/
	&LTBLUE,
	&LTGREEN,
	&LTBLUE,
	&ORANGE,
	&PINK,
	&LTBLUE,
	&WHITE};

/*
/*int NCL_vx_color_table[16] = {
/*   UM_YELLOW,
/*   UM_GREEN,
/*   UM_RED,
/*   UM_YELLOW,
/*   UM_YELLOW,
/*   UM_CYAN,
/*   UM_MAGENTA,
/*   UM_YELLOW,
/*   UM_BLUE,
/*   UM_CYAN,
/*   UM_GREEN,
/*   UM_CYAN,
/*   UM_RED,
/*   UM_MAGENTA,
/*   UM_YELLOW,
/*   UM_WHITE};
*/

int Line_Num;

extern STORE_T root_store;

extern int NCL_vx_create;
jmp_buf NCL_ebuf1;
extern int NCL_vx_flag; /* 0=native NCL, 1=run native under VX, 2=run VX NCL */
static int NCL_vx_first = 1;
extern long NCL_i4d_gwid;

int NCL_exit_flag = 0;
extern int NCL_vx_createf;
/*
.....Added because of "undefined" w/VX V7.0
.....Bobby  -  10/14/97
*/
poll_data () {}

/*********************************************************************
**    E_FUNCTION     : int vx_ncl(itsk)
**       Start NCL under VX.
**    PARAMETERS 
**       INPUT  :
**          itsk       - 0 = run VX window, 1 = run old NCL.
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
vx_ncl(itsk)
int itsk;
	{
	int status, ix;
	UM_int2 idx, ival;
	UM_int4 cami, cadi, camo, lathe, tl3axis;
	STORE_T *store;

	status = UU_SUCCESS;

/*   ------   DEBUG   ------   */
	printf ("\nEntering vx_ncl...\n");
/*   ------   DEBUG   ------   */
	ncl_init_auth("NCLCAM,NCLCADD");

	if (itsk == 1)
	{
	  NCL_vx_flag = 1;
	  ix = setjmp(NCL_ebuf1);
	  if (ix == 0)
		 {
		 ncl_vx_getmodel();
		 if (!NCL_vx_first)
			{
			if (NCL_exit_flag == 0)
			  {
			  UD_markptr = 0;
			  UD_MARK(ix,UU_TRUE);
			  uw_mfsignon (1);
			  }
			else
			  {
			  UD_markptr = 0;
			  UD_MARK(ix,UU_TRUE);
			  uw_mfmenu_reset (UU_TRUE,UU_TRUE,UU_FALSE);
			  uw_mfmainloop();
			  }
			}
		 NCL_vx_first = 0;
		 unicad_();
		 }
	  else
		 {
		 while (uu_current_store != &root_store)
			{
			store = uu_get_current_store();
			uu_alloc_pop();
			uu_free_store(store);
			}
		 }
	}
	else
	{

	 /* Initialize some things from znu_init_runvars() */
	  ncl_init_auth("NCLCAM");
	  znu_init_units();
	  znu_init_machinetype();

	/* DEFAULTS TO RUNNING EQUIVALENT OF NCLCAM */
	  UU_application == UU_NCLCAM;

	/* Verify AUTHORIZATION for this terminal */
/*   cami = 0;
/*   cadi = 0;
/*   auth_vt(&camo,&lathe,&tl3axis);   
/*   if (camo == -1)
/*       {
/*       printf ("Error trying to authorize CAM terminal.\n");
/*       exit();
/*       }
/*
	  lathe = 1
	  tl3axis = 0;
.........RAH: set LATHE flag in FORTRAN. Fix MARK implemented in nclc/nauth.c.
.........I moved here because nclc/nauth.c must be application independant. 
*/
	  UL_lathe = lathe;
	  idx = 142;
	  ival = UL_lathe;
	  setifl (&idx, &ival);
/*
.........Set the 3AXIS mode flag in FORTRAN.
*/
	  UL_3axis = tl3axis;
	  idx = 304;
	  ival = UL_3axis;
	  setifl (&idx, &ival);
/*
.........Enable NCLVT
*/
	  idx=35;    /* set NCLVT flag  */
	  ival=2;
	  setifl(&idx,&ival);
/*
.........Enable NCLVX
*/
	  idx=322;    /* set NCLVX flag  */
	  ival=1;
	  setifl(&idx,&ival);

	  NCL_vx_flag = 2;

	  ncl_init_batch();  
		  
	  runvx();  
	}

	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : ncl_vx_setunits()
**       Set NCL units to VX units.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_setunits()
	{
	int status;
	UM_int2 idx, ival;
	mdl_default vx_def_lin_units = MDL_DEF_LIN_UNIT;
	mdl_lin_unit vx_units;

	status = vx_mdl_inq_default (vx_def_lin_units, &vx_units);
/*
...   VX bug - will sometimes return valid units with bad status. Second call ok.
*/
	if (status != 0) status = vx_mdl_inq_default (vx_def_lin_units, &vx_units);
	if (status == 0)
	  {
	  idx = 264;
	  getifl (&idx, &ival);
	  idx = 0;
	  if (vx_units == MDL_MM) idx = 1;
	  if (ival != idx)
		  {
		  if (idx == 1) millim(); else inches();
		  }
	  }

	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : ncl_vx_find (label, isub, nclkey, nwds, ietype)
**       Determine the type of a piece of VX geometry & save in name list.
**    PARAMETERS
**       INPUT  :
**          label    - Name of geometry
**          isub     - subscript
**       OUTPUT :
**          nclkey   - Key of geometry.
**          nwds     - Size of geometry.
**          ietype   - Type of geometry.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_find (label, isub, nclkey, nwds, ietype)
char *label;
UM_int4  isub, *nclkey;
UM_int2 *nwds, *ietype;
	{
	int status;
	UM_int2 ipg = 0, iel = 0, nocreat = 0;
	UU_KEY_ID key = 0;
	UM_f77_str_ptr f77label;

	status = ncl_vx_type (label, isub, nwds, ietype);

	if (status == UU_SUCCESS && *ietype > 1)
	  {
	  status = ncl_store_name(label, isub, ipg, iel, *nwds, *ietype,
									  nocreat, &key);
	  *nclkey = key;
	  }

	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : ncl_vx_type (label, isub, nwds, ietype)
**       Determine the type of a piece of VX geometry.
**    PARAMETERS 
**       INPUT  :
**          label    - Name of geometry
**          isub     - subscript
**       OUTPUT :
**          nwds     - Size of geometry.
**          ietype   - Type of geometry.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_type (label, isub, nwds, ietype)
char *label;
UM_int4 isub;
UM_int2 *nwds, *ietype;
	{
	int status, idx, nw, itype, vxtype;

	*ietype = 1;
	status = vx_mdl_inq_index (label, &idx);
/*   ------   FIX   ------ Remove this call when _inq_ is fixed */
#ifdef INQBUG
	if (status != 0) status = vx_mdl_inq_index (label, &idx);
#endif

	if (status == 0 && idx > 0)
	  {
	  status = vx_ncl_find (idx, &nw, &itype, &vxtype);

	  if (status == 0)
		 {
		 *nwds = nw;
		 *ietype = itype;
		 }
	  }

	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_getmodel ()
**       Save the VX model directory & filename.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS :
**         Sets NCL_vx_dir & NCL_vx_ppname.
**    WARNINGS     : none
*********************************************************************/
int ncl_vx_getmodel ()
{
	int err, i, n1;
	UX_pathname model;
	char *p1, *p2;
#ifndef UU_RS6000
	char *rindex();
#endif
	mdl_default option;

	NCL_vx_dir[0] = '\0';
	NCL_vx_ppname[0] = '\0';
	option = MDL_DEF_MODEL_NAME;
	err = vx_mdl_inq_default (option, model);
	if (err == 0)
	  {
/*
...   If no model name has been set in VX, trash is sometimes returned
...   Return with null directory & pp names.
*/
	  n1 = strlen(model);
	  if (n1>80) return;
	  for (i=0; i<n1; i++) if (model[i]<33 || model[i]>126) return;
	  p2 = model;
	  p1 = rindex(model,'/');
	  if (p1 != UU_NULL)
		 {
		 *p1 = '\0';
		 strcpy(NCL_vx_dir,model);
		 p2 = p1+1;
		 }
	  p1 = rindex(p2,'.');
	  if (p1 != UU_NULL) *p1 = '\0';
	  strcpy(NCL_vx_ppname,p2);
	  }

	return ;
}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_getdir (dir)
**       Return the current VX working directory.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          dir      - Pointer to directory.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_vx_getdir (dir)
char *dir;
{
	strcpy(dir,NCL_vx_dir);
	return ;
}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_getppname (ppname)
**       Return the current VX model name.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          ppname    - Pointer to model name.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_vx_getppname (ppname)
char *ppname;
{
	strcpy(ppname,NCL_vx_ppname);
	return ;
}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_set_attr (idx)
**       Set default attributes from vx entity.
**    PARAMETERS   
**       INPUT  : 
**          idx      - Key of Varimetrix entity.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : Sets default display attributes.
**    WARNINGS     : none
*********************************************************************/
ncl_vx_set_attr (idx)
int idx;
{
  int err;
  int color = 0;
  mdl_ety_at at;
  mdl_ety_type type;
	mdl_ln_type lntype;
	int ncl_lntype = 0;

  err = vx_mdl_inq_ety_at (idx, &at);
  if (err == 0)
	 um_set_active_layer (at.layer_id);
  err = vx_mdl_inq_type (idx, &type);
/*   ------   FIX   ------   */
/* If call to vx_mdl_inq_ety_at() above gets an error (e.g. if idx is a curve set)
/* call to vx_mdl_inq_type() get an error on first call & must be called twice */
  if (err != 0)
	 err = vx_mdl_inq_type (idx, &type);
/*   ------ END FIX ------   */
  if (err == 0)
	 {
	 switch (type)
		{
		case MDL_ETY_POINT:
		  color = at.data.pnt_at.color;
		  break;
		case MDL_ETY_LINE3:
		case MDL_ETY_CIR3:
		case MDL_ETY_CURV:
		case MDL_ETY_FCURV:
		case MDL_ETY_FNCURV:
/*
......added type MDL_ETY_ELL3
......Yurong 11/2/98
*/
		case MDL_ETY_ELL3:
		  color = at.data.crv_at.color;
/*
.....added for line type
.....Yurong 4/21/99
*/
			lntype = at.data.crv_at.ln_type;
			if ((lntype>6)||(lntype<0)) 
				ncl_lntype = 1;
			else
				ncl_lntype = NCL_vx_lntype_table[lntype]+1;
			ur_put_attrmdl_line_style(ncl_lntype);
/*   ------   FIX   ------   */
/*         ur_put_attrmdl_line_width (at.data.crv_at.ln_width/25.4); */
/*   ------ END FIX ------   */
		  break;
		case MDL_ETY_PLANE:
		case MDL_ETY_SURF:
		case MDL_ETY_SURFREV:
		case MDL_ETY_SPHERE:
		case MDL_ETY_CYLINDER:
		case MDL_ETY_CONE:
		case MDL_ETY_TORUS:
		case MDL_ETY_ELLIPSOID:
		case MDL_ETY_EXTRUSION:
		  color = at.data.srf_at.color-1;
/*
.....added for line type
.....Yurong 4/21/99
*/
			lntype = at.data.srf_at.ln_type;
			if ((lntype>6)||(lntype<0)) 
				ncl_lntype = 1;
			else
				ncl_lntype = NCL_vx_lntype_table[lntype]+1;
			ur_put_attrmdl_line_style(ncl_lntype);
		  break;
		default:
		  color = 0;
		  break;
		}
	 }
	if (color < 0 || color > 15) color = 0;
	color = *NCL_vx_color_table[color];
	ncl_set_color_attr(color);
	return;
}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_set_blank (idx, key)
**       Set the blanked status from vx entity.
**    PARAMETERS   
**       INPUT  : 
**          idx      - Key of Varimetrix entity.
**          key      - Key of NCL entity.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_vx_set_blank (idx, key)
int idx;
UU_KEY_ID key;
{
  int err;
  mdl_ety_vis vis;

	err = vx_mdl_inq_vis (idx, &vis);
	if (err == 0 && vis == MDL_INVISIBLE)
	  ncl_sea_ent_blank (0, key);

	return;
}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_create (idx, ietype, vxlab, e1)
**       Create NCL geometry out of VX geometry.
**    PARAMETERS
**       INPUT  :
**          idx        - VX key of entity.
**          ietype     - Type of entity.
**          vxlab      - Label of entity.
**       OUTPUT :
**          e1         - Structure containing entity.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_create (idx, ietype, vxlab, e1)
	int idx;
	UM_int2 ietype;
	char *vxlab;
	struct NCL_fixed_databag *e1;
	{
	int status = UU_FAILURE;
	int i, j, err, n1, ix, nloops;
	double *p2;
	UU_REAL fct = 1.0;
	UU_REAL pta[3];
	UU_REAL umin, umax, vmin, vmax;
	UM_int4 i4sub = 0;
	UU_KEY_ID keysf, keycv[2];
	mdl_data vx_data;
	mdl_ety_at at1;
	struct UM_point_rec *p1;
	struct UM_line_rec *l1;
	struct UM_circle_rec *c1;
	struct UM_rbsplcrv_rec *crv;
	struct UM_rbsplsrf_rec *srf;
	struct NCL_trimsf_rec *tsf;
	UM_f77_str f77lab;
	UM_f77_str f77unl;
	char unlab[NCL_MAX_LABEL+1];
	char label[NCL_MAX_LABEL+1];

	if (ietype == NCLI_POINT || ietype == NCLI_LINE || ietype == NCLI_CIRCLE)
	  {
	  err = vx_mdl_inq_prim (idx, &vx_data);
/*   ------   FIX   ------ Remove this call when _inq_ is fixed */
#ifdef INQBUG
	  if (err !=0) err = vx_mdl_inq_prim (idx, &vx_data);
#endif
	  if (err == 0)
		 {
		 switch (vx_data.type)
			{
			case MDL_DATA_3D_PNT:
			  p1 = (struct UM_point_rec *)e1;
			  p1->pt[0] = vx_data.data.pnt.x / 25.4;
			  p1->pt[1] = vx_data.data.pnt.y / 25.4;
			  p1->pt[2] = vx_data.data.pnt.z / 25.4;
			  p1->rel_num  = UM_POINT_REL;
			  status = UU_SUCCESS;
			  break;
			case MDL_DATA_LINE:
			  l1 = (struct UM_line_rec *)e1;
			  l1->spt[0] = vx_data.data.line.p1.x / 25.4;
			  l1->spt[1] = vx_data.data.line.p1.y / 25.4;
			  l1->spt[2] = vx_data.data.line.p1.z / 25.4;
			  l1->ept[0] = vx_data.data.line.p2.x / 25.4;
			  l1->ept[1] = vx_data.data.line.p2.y / 25.4;
			  l1->ept[2] = vx_data.data.line.p2.z / 25.4;
			  l1->rel_num = UM_LINE_REL;
			  status = UU_SUCCESS;
			  break;
			case MDL_DATA_CIRCLE:
			  c1 = (struct UM_circle_rec *)e1;
			  c1->center[0] = vx_data.data.cir.c_pnt.x / 25.4;
			  c1->center[1] = vx_data.data.cir.c_pnt.y / 25.4;
			  c1->center[2] = vx_data.data.cir.c_pnt.z / 25.4;
			  c1->nvec[0]   = vx_data.data.cir.norm.x;
			  c1->nvec[1]   = vx_data.data.cir.norm.y;
			  c1->nvec[2]   = vx_data.data.cir.norm.z;
			  c1->svec[0]   = vx_data.data.cir.s_pnt.x / 25.4 - c1->center[0];
			  c1->svec[1]   = vx_data.data.cir.s_pnt.y / 25.4 - c1->center[1];
			  c1->svec[2]   = vx_data.data.cir.s_pnt.z / 25.4 - c1->center[2];
			  c1->dang      = vx_data.data.cir.angle;
			  c1->radius    = sqrt(c1->svec[0]*c1->svec[0]+c1->svec[1]*c1->svec[1]
										+ c1->svec[2]*c1->svec[2]);
			  um_unitvc (c1->svec, c1->svec);
			  c1->rel_num = UM_CIRCLE_REL;
			  status = UU_SUCCESS;
			  break;
			default:
			  break;
			}
		 if (status == UU_SUCCESS)
			{
			UM_init_f77_str(f77lab, label, NCL_MAX_LABEL);
			for (i = 0; vxlab[i] != '\0' && i<NCL_MAX_LABEL; i++)
			  label[i] = vxlab[i];
			while (i<NCL_MAX_LABEL) label[i++] = ' ';
			label[NCL_MAX_LABEL] = '\0';
			vxnam (UM_addr_of_f77_str(f77lab), &i4sub);
			ur_setup_data (e1->rel_num, e1, sizeof(*e1));
			e1->key = 0;
			status = ncl_create_entity (e1, ietype);
			}
		 if (status == UU_SUCCESS)
			{
			status = ncl_retrieve_data_fixed(e1);
			}
		 }
	  }
	else if (ietype == NCLI_CURVE)
	  {
	  crv =  (struct UM_rbsplcrv_rec *)e1;
	  ur_setup_data (UM_RBSPLCRV_REL, crv, sizeof(*e1));
	  err = vx_mdl_inq_nrb (idx, MDL_TRIMMED, &vx_data);
	  if (err == 0)
		 {
		 fct = 1.0 / 25.4;
		 for (i = 0; vxlab[i] != '\0' && i<NCL_MAX_LABEL; i++)
			label[i] = vxlab[i];
		 while (i<NCL_MAX_LABEL) label[i++] = ' ';
		 label[NCL_MAX_LABEL] = '\0';
		 status = ncl_vx_create_curve (&vx_data.data.nrb_crv, fct, label, crv);
		 }
	  }
	else if (ietype == NCLI_SURF)
	  {
	  srf =  (struct UM_rbsplsrf_rec *)e1;
	  err = vx_mdl_inq_ety_at (idx, &at1);
		if (err == 0)
		{
			UM_srfattr.numupaths = at1.data.srf_at.num_v_iso;
			UM_srfattr.numvpaths = at1.data.srf_at.num_u_iso;
			if (UM_srfattr.numupaths == -1) UM_srfattr.numupaths = 3;
			if (UM_srfattr.numvpaths == -1) UM_srfattr.numvpaths = 3;
		}
		else
		{
			UM_srfattr.numupaths = 3;
			UM_srfattr.numvpaths = 3;
		}
	  srf->key = 0;
	  ncl_setup_rbsf (UM_RBSPLSRF_REL, srf, sizeof(*e1));
	  err = vx_mdl_inq_nrb (idx, MDL_TRIMMED, &vx_data);
	  if (err == 0)
		 {
		 srf->ku = vx_data.data.nrb_srf.u.degree+1;
		 srf->kv = vx_data.data.nrb_srf.v.degree+1;
		 srf->nu = vx_data.data.nrb_srf.u.num_kt -
					2*vx_data.data.nrb_srf.u.degree - 1;
		 srf->nv = vx_data.data.nrb_srf.v.num_kt -
					2*vx_data.data.nrb_srf.v.degree - 1;
		 UM_init_f77_str(f77lab, label, NCL_MAX_LABEL);
		 UM_init_f77_str(f77unl, unlab, NCL_MAX_LABEL);
		 nloops = vx_data.data.nrb_srf.num_loop;
		 if (nloops > 0)
			{
			strcpy (unlab, "@UN    ");
			vxnam (UM_addr_of_f77_str(f77unl), &i4sub);
			}
		 for (i = 0; vxlab[i] != '\0' && i<NCL_MAX_LABEL; i++)
			label[i] = vxlab[i];
		 while (i<NCL_MAX_LABEL) label[i++] = ' ';
		 label[NCL_MAX_LABEL] = '\0';
		 if (nloops == 0)
			vxnam (UM_addr_of_f77_str(f77lab), &i4sub);
		 e1->key = 0;
		 status = ncl_create_entity (e1, ietype);
		 if (status == UU_SUCCESS)
			{
			keysf = srf->key;
			n1 = vx_data.data.nrb_srf.u.num_kt;
			p2 = vx_data.data.nrb_srf.u.knots;
			umin = p2[0];
			umax = p2[n1-1];
			status = ur_update_data_varlist (keysf, 1, p2, 1, n1);
			}
/*
.....if failed, delete this entity
..... Yurong 11/2/98
*/
		else
		{
			uc_delete(keysf);
			return (status);
		}

		 if (status == UU_SUCCESS)
			{
			n1 = vx_data.data.nrb_srf.v.num_kt;
			p2 = vx_data.data.nrb_srf.v.knots;
			vmin = p2[0];
			vmax = p2[n1-1];
			status = ur_update_data_varlist (keysf, 2, p2, 1, n1);
			}
/*
.....if failed, delete this entity
..... Yurong 11/2/98
*/
		else
		{
			uc_delete(keysf);
			return (status);
		}
		 if (status == UU_SUCCESS)
			{
			n1 = vx_data.data.nrb_srf.cp.num_cp;
			ix = vx_data.data.nrb_srf.cp.dim;
			p2 = vx_data.data.nrb_srf.cp.list;
			pta[2] = 0.0;
			status = ur_update_data_varlist (keysf, 3, pta, n1, 1);
			for (i=1; i<=n1 && status == UU_SUCCESS; i++, p2+=ix)
			  {
			  for (j=0; j<ix-1; j++) pta[j] = p2[j] / p2[ix-1] / 25.4;
			  status = ur_update_data_varlist (keysf, 3, pta, i, 1);
			  }
			}
/*
.....if failed, delete this entity
..... Yurong 11/2/98
*/
		else
		{
			uc_delete(keysf);
			return (status);
		}
		 if (status == UU_SUCCESS)
			{
			p2 = vx_data.data.nrb_srf.cp.list+ix-1;
			status = ur_update_data_varlist (keysf, 4, p2, n1, 1);
			for (i=1; i<=n1 && status == UU_SUCCESS; i++, p2+=ix)
			  status = ur_update_data_varlist (keysf, 4, p2, i, 1);
			}
/*
.....if failed, delete this entity
..... Yurong 11/2/98
*/
		else
		{
			uc_delete(keysf);
			return (status);
		}
/*
.....if failed, delete this entity
..... Yurong 11/2/98
*/
		if (status != UU_SUCCESS)
		{
			uc_delete(keysf);
			return (status);
		}
		 if (nloops > 0 && status == UU_SUCCESS)
			{
			ur_update_displayable(keysf, UM_NEVERDISPLAYABLE);
			tsf = (struct NCL_trimsf_rec *)e1;
			tsf->key = 0;
			ur_setup_data (NCL_TRIMSF_REL, tsf, sizeof(*tsf));
			tsf->closdinu = 0;
			tsf->closdinv = 0;
			tsf->offdist = 0.0;
			tsf->cv_key = 0;
			tsf->bs_key = keysf;
			tsf->no_ibndykey = 0;
			tsf->ibndykey = 0;
			tsf->ub_min = 0.0;
			tsf->ub_max = 1.0;
			tsf->vb_min = 0.0;
			tsf->vb_max = 1.0;
			tsf->u_min = umin;
			tsf->u_max = umax;
			tsf->v_min = vmin;
			tsf->v_max = vmax;
			status=ncl_vx_compcrv (vx_data.data.nrb_srf.list_loop[0].num_cv,
				 vx_data.data.nrb_srf.list_loop[0].list_cv,fct,unlab,1,&tsf->uv_key);
			if (status == UU_SUCCESS)
			  {
			  ur_update_displayable(tsf->uv_key, UM_NEVERDISPLAYABLE);
			  vxnam (UM_addr_of_f77_str(f77lab), &i4sub);
			  status = ncl_create_entity (tsf, ietype);
			  }
/*
.....if failed, delete this entity
..... Yurong 11/2/98
*/
		else
		{
			uc_delete(keysf);
			return (status);
		}
/*
.....if failed, delete this entity
..... Yurong 11/2/98
*/
		if (status != UU_SUCCESS)
		{
			uc_delete(keysf);
			return (status);
		}
			keysf  = tsf->key;
			keycv[0] = 0;
			for (i=1; i<nloops && status == UU_SUCCESS; i++)
			  {
			  status=ncl_vx_compcrv (vx_data.data.nrb_srf.list_loop[i].num_cv,
				  vx_data.data.nrb_srf.list_loop[i].list_cv,fct,unlab,1,&keycv[1]);
			  if (status == UU_SUCCESS)
				 {
				 ur_update_displayable(keycv[1], UM_NEVERDISPLAYABLE);
				 status = ur_update_data_varlist (keysf, 1, keycv, i*2-1, 2);
				 }
/*
.....if failed, delete this entity
..... Yurong 11/2/98
*/
			else
			{
				uc_delete(keysf);
				return (status);
			}
			  }
			}
		 if (status == UU_SUCCESS)
			{
			status = ncl_retrieve_data_fixed(e1);
			}
/*
.....if failed, delete this entity
..... Yurong 11/2/98
*/
		else
		{
			uc_delete(keysf);
			return (status);
		}
		 }
	  }

	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_compcrv (n1, vxcvl, fct, lab, ijoin, keycv)
**       Create a composite curve from a list of vx curves.
**    PARAMETERS
**       INPUT  :
**          n1         - Number of curves.
**          vxcvl      - List of VX curves.
**          fct        - Factor to multiply points by
**          vxlab      - Label of curve.
**          ijoin      - =1, Force component curve to be joined.
**       OUTPUT :
**          keycv      - Key of created curve.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_compcrv (n1, vxcvl, fct, vxlab, ijoin, keycv)
int n1;
mdl_nrb_crv *vxcvl;
UU_REAL fct;
char *vxlab;
int ijoin;
UU_KEY_ID *keycv;
{
	int status = UU_SUCCESS;
	int i, j, lix, opflg;
	UM_int2 ietype = 8, iflg;
	UM_int4 i4sub = 0;
	UU_REAL d1, d2;
	UM_coord  spt, ept, fpt, lpt, pt0;
	struct NCL_fixed_databag e1;
	struct UM_compcrv_rec *cvp;
	struct UM_rbsplcrv_rec *crv;
	char label[NCL_MAX_LABEL+1];
	UU_KEY_ID *keyp = UU_NULL;
	struct UM_cid_rec *lcid = UU_NULL;
	UM_f77_str f77lab;
	char *uu_malloc();
	struct UM_evcrvout evout;
	UU_REAL tol;

	crv = (struct UM_rbsplcrv_rec *)&e1;
	*keycv = 0;
	strcpy (label, "@UN");
	for (i=0; i<NCL_MAX_LABEL; i++) label[i] = ' ';
	label[NCL_MAX_LABEL] = '\0';
	keyp = (UU_KEY_ID *)uu_malloc(n1*sizeof(UU_KEY_ID));
	lcid = (struct UM_cid_rec *)uu_malloc(n1*sizeof(struct UM_cid_rec));
	if (keyp == UU_NULL || lcid == UU_NULL) status = UU_FAILURE;
	for (i=0; i<n1 && status == UU_SUCCESS; i++)
	{
		status = ncl_vx_create_curve (&vxcvl[i], fct, label, &e1);
		if (status == UU_SUCCESS)
		{
			ur_update_displayable(e1.key, UM_NEVERDISPLAYABLE);
			keyp[i] = e1.key;
		}
	}
/*
...   Make sure the elements of the composite bounding curve join.
...   This is a bit crude. It blindly sets the closest end point of each B-spline
...   curve segment to the closest end of the segments found so far. It makes the
...   change "in-place" in memory rather than calling unibase routines.
	if (ijoin)
*/
	opflg = 1;
	for (i=0; i<n1 && status == UU_SUCCESS; i++)
	{
		crv->key = keyp[i];
		status = ncl_retrieve_data_fixed(crv);
		if (status == UU_SUCCESS)
			if (crv->rel_num != UM_RBSPLCRV_REL) status = UU_FAILURE;
		if (status == UU_SUCCESS)
		{
			lcid[i].crvid = crv->key;
			uc_init_evcrvout (crv,&evout);
			status = uc_evcrv (UM_POINT, (UU_REAL) 0.0, crv, UM_DEFAULT_TF, &evout);
			um_vctovc (evout.cp,spt);
			status = uc_evcrv (UM_POINT, (UU_REAL) 1.0, crv, UM_DEFAULT_TF, &evout);
			um_vctovc (evout.cp,ept);
			lix = (crv->no_pt-1)*3;
/*			um_vctovc(crv->pt, spt); */
/*			um_vctovc(&crv->pt[lix], ept); */
/*
...   For the first curve, save the first & last point
*/
			if (i==0)
			{
				um_vctovc(spt, fpt);
				um_vctovc(ept, lpt);
			}
/*
...   For subsequent curves, find out which end is closest to which end of the
...   curves found so far.
*/
			else
			{
				iflg = 0;
				d1 = um_dcccc (spt, lpt);
				d2 = um_dcccc (ept, lpt);
				if (d2 < d1)
				{
					d1 = d2;
					iflg = 1;
				}
/*
.....on second segment we can reverse direction on both
.....to match ends correctly
*/
				if (i == 1)
				{
					d2 = um_dcccc (spt, fpt);
					if (d2 < d1)
					{
						d1 = d2;
						iflg = 2;
					}
					d2 = um_dcccc (ept, fpt);
					if (d2 < d1)
					{
						d1 = d2;
						iflg = 3;
					}
				}
/*
...   If curves are not joined, give up.
*/
				if (!ijoin && d1 > UM_FUZZ*2.0)
				{
					for (j=0; j<n1; j++) uc_delete(keyp[j]);
					status = UU_FAILURE;
				}
/*
...   Set the appropiate end point of this curve to the appropiate end point
...   of the composite curve so far. If it is the last curve, make sure the loop
...   is closed
*/
				if (status == UU_SUCCESS)
				switch (iflg)
				{
					case 0:
						if (ijoin) um_vctovc (lpt, crv->pt);
						um_vctovc (ept, lpt);
						break;
					case 1:
						if (ijoin) um_vctovc (lpt, &crv->pt[lix]);
						um_vctovc (spt, lpt);
						lcid[i].crvid = 0;
						break;
					case 2:
						if (ijoin) um_vctovc (fpt, crv->pt);
						lcid[0].crvid = 0;
						um_vctovc (lpt,fpt);
						um_vctovc (ept, lpt);
						break;
					case 3:
						if (ijoin) um_vctovc (fpt, &crv->pt[lix]);
						lcid[0].crvid = 0;
						um_vctovc (lpt,fpt);
						um_vctovc (spt, lpt);
						lcid[i].crvid = 0;
						break;
				}
/*
.....for last segment check if end point meets first point
.....and close curve if it is loop
*/
				if (ijoin && i == n1-1)  
				{
/*
.....change check limit
.....Yurong 11/2/98
*/
/*					if (um_dcccc (lpt,fpt) < 2.0*UM_FUZZ) */
					gettol(&tol);
					if (um_dcccc (lpt,fpt) < 4.0*tol)
					{
						if (iflg == 0) um_vctovc (fpt,&crv->pt[lix]);
						else um_vctovc (fpt,crv->pt);
						opflg = 0;
					}  
/*					else status = UU_FAILURE;      */
					else
					{
						for (j=0; j<n1; j++) uc_delete(keyp[j]);
						status = UU_FAILURE;
					}
				}
			}
		}
	}
/*
.....create entity in unibase
*/
	if (status == UU_SUCCESS)
	{
		cvp = (struct UM_compcrv_rec *)&e1;
		ur_setup_data (UM_COMPCRV_REL, cvp, sizeof(*cvp));
		cvp->key = 0;
		UM_init_f77_str(f77lab, label, NCL_MAX_LABEL);
		for (i = 0; vxlab[i] != '\0' && i<NCL_MAX_LABEL; i++)
		 	label[i] = vxlab[i];
		while (i<NCL_MAX_LABEL) label[i++] = ' ';
		label[NCL_MAX_LABEL] = '\0';
		vxnam (UM_addr_of_f77_str(f77lab), &i4sub);
		iflg = 1;
		stdfwf(&iflg);
		status = ncl_create_entity (cvp,ietype);
		stdfwf(&iflg);
/*
.....update cid's for components
*/
		if (status == UU_SUCCESS)
		{
			int dim;
			UU_REAL space[2][3];
			status = ncl_vx_mergecv (n1, keyp, lcid, cvp);
			ncl_retrieve_data_fixed (cvp);
			uc_span_entity(cvp,&dim,space);
			if (dim == 2) cvp->planar = UU_TRUE;
			cvp->continuity = 0;
			cvp->closdinu = 0;
			cvp->open = opflg;
			cvp->fcolor = UM_BACKGROUND;
			
		}
	}
/*
.....update constants
*/
	if (status == UU_SUCCESS)
		status = ur_update_data_fixed(cvp);

	if (status == UU_SUCCESS)
	  {
	  *keycv = cvp->key;
	  }
/*
.....release memory
*/
	if (keyp != UU_NULL) uu_free(keyp);
	if (lcid != UU_NULL) uu_free(lcid);

	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_create_curve (vxcrv, fct, vxlab, crv)
**       Create a trim surface boundary curve.
**    PARAMETERS
**       INPUT  :
**          vxcrv      - VX trim curve.
**          vxlab      - Label of curve to create.
**       OUTPUT :
**          crv        - Created curve.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_create_curve (vxcrv, fct,vxlab, crv)
	mdl_nrb_crv *vxcrv;
	UU_REAL fct;
	char *vxlab;
	struct UM_rbsplcrv_rec *crv;
	{
	int status;
	UM_int4 i4sub = 0;
	UM_int2 ietype = 8;
	int i, j, ix, n1;
	double *p2;
	UU_REAL pta[3];
	UM_f77_str f77str;

	ur_setup_data (UM_RBSPLCRV_REL, crv, sizeof(*crv));
	crv->planar   = UU_FALSE;
	crv->open     = UU_TRUE;
	crv->closdinu = 0;
	crv->k        = vxcrv->t.degree+1;
	crv->n        = vxcrv->cp.num_cp - vxcrv->t.degree;
	crv->t0       = vxcrv->t.bnd.min;
	crv->t1       = vxcrv->t.bnd.max;
	UM_init_f77_str(f77str, vxlab, 6);
	vxnam (UM_addr_of_f77_str(f77str), &i4sub);
	crv->key = 0;
	status = ncl_create_entity (crv, ietype);
	if (status == UU_SUCCESS)
	  {
	  n1 = vxcrv->t.num_kt;
	  p2 = vxcrv->t.knots;
	  status = ur_update_data_varlist (crv->key, 1, p2, 1, n1);
	  }
	if (status == UU_SUCCESS)
	  {
	  n1 = vxcrv->cp.num_cp;
	  ix = vxcrv->cp.dim;
	  p2 = vxcrv->cp.list;
	  pta[2] = 0.0;
	  status = ur_update_data_varlist (crv->key, 2, pta, n1, 1);
	  for (i=1; i<=n1 && status == UU_SUCCESS; i++, p2+=ix)
		 {
		 for (j=0; j<ix-1; j++) pta[j] = fct * p2[j] / p2[ix-1];
		 status = ur_update_data_varlist (crv->key, 2, pta, i, 1);
		 }
	  }
	if (status == UU_SUCCESS)
	  {
	  p2 = vxcrv->cp.list+ix-1;
	  status = ur_update_data_varlist (crv->key, 3, p2, n1, 1);
	  for (i=1; i<=n1 && status == UU_SUCCESS; i++, p2+=ix)
		 status = ur_update_data_varlist (crv->key, 3, p2, i, 1);
	  }
	if (status == UU_SUCCESS)
	  {
	  status = ncl_retrieve_data_fixed(crv);
	  }

  return(status);
  }
/*********************************************************************
**    E_FUNCTION     : int vxload
**       Load & display Varimetrix geometry.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
vxload ()
	{
	int status, err, i, j, k, n1, nupsv, nvpsv, idx;
	UM_int2 nwds, ietype = 8;
	UM_int2 ipg = 0, iel = 0, nocreat = 1;
	UM_int4 isub=0, nclkey;
	UU_REAL fct = 1.0 / 25.4;
	char vxname[64];
	mdl_ety_type itype[6];
	mdl_list elist, elist2;
	mdl_data vx_data;
	mdl_nrb_crv *vxcvl, *cvp;
	UU_KEY_ID keycv;
	struct NCL_fixed_databag e1;
	struct UM_attrmdl_rec hldattr;
	struct NCL_nclattr_rec attr;
	int hldcolor;
	char *uu_malloc();

	status = ncl_vx_delall();

	err = 0;

	ur_get_attrmdl (&hldattr);
	hldcolor = ncl_get_attrmdl_color();
	
	nupsv = UM_srfattr.numupaths;
	nvpsv = UM_srfattr.numvpaths;
	UM_srfattr.numupaths = 3;
	UM_srfattr.numvpaths = 3;

	itype[0] = MDL_ETY_POINT;
	itype[1] = MDL_ETY_CURV;
	itype[2] = MDL_ETY_SURF;
	itype[3] = MDL_ETY_PLANE;
	itype[4] = MDL_ETY_VARIABLE;
	itype[5] = MDL_ETY_CURVSET;

/*
...   Convert curve sets (profiles) to composite curves.
*/
	elist.num = 0;
	elist.list = UU_NULL;
	err = vx_mdl_inq_ety_list (itype[5], &elist);
	NCL_vx_createf = 1;
	for (j=0; j<elist.num; j++)
	  {
	  idx = elist.list[j];
	  err = vx_mdl_inq_name (idx, vxname);
	  for (k=0;k<64 && vxname[k];k++)
		 if (islower(vxname[k])) vxname[k]=toupper(vxname[k]);
	  elist2.num = 0;
	  elist2.list = UU_NULL;
	  err = vx_mdl_inq_set (idx, &elist2);
	  n1 = elist2.num;
	  if (n1 > 0 && !err)
		 {
		 vxcvl = (mdl_nrb_crv *)uu_malloc(n1*sizeof(mdl_nrb_crv));
		 cvp = vxcvl;
		 if (vxcvl)
			{
			for (k=0; k<n1 && !err; k++, cvp++)
			  {
			  err = vx_mdl_inq_nrb (elist2.list[k], MDL_TRIMMED, &vx_data);
			  *cvp = vx_data.data.nrb_crv;
			  }
			if (!err) status = ncl_vx_compcrv (n1, vxcvl, fct, vxname, 0, &keycv);
			uu_free(vxcvl);
			if (status == UU_SUCCESS)
			  {
			  nwds = n1;
			  status = ncl_store_name(vxname, isub, ipg, iel, nwds, ietype,
									  nocreat, &keycv);
			  }
/*
...   Set comp curve display attributes to those of first profile element.
*/
			if (status == UU_SUCCESS)
			  {
			  ncl_vx_set_attr (elist2.list[0]);
			  status = ncl_init_attr_rec (keycv, &attr, NCLI_CURVE);
			  }
			if (status == UU_SUCCESS) ncl_vx_set_blank (elist2.list[0], keycv);

			if (status == UU_SUCCESS)
			  {
			  e1.key = keycv;
			  status = ncl_retrieve_data_fixed(&e1);
			  }

			if (status == UU_SUCCESS)
			  status = ur_update_view_key(e1.key, 0);

			if (status == UU_SUCCESS)
			  status = uv_disp_entity(&e1);

			}
		 if (!elist2.list) free (elist2.list);
		 }
	  }
	if (!elist.list) free (elist.list);
	NCL_vx_createf = 0;
/*
...   Convert the rest.
*/
	for (i=0; i<5; i++)
	  {
	  elist.num = 0;
	  elist.list = UU_NULL;
	  err = vx_mdl_inq_ety_list (itype[i], &elist);

#ifdef INQBUG
	  if (err != 0) {
		 if (elist.list != UU_NULL) free (elist.list);
		 elist.num = 0;
		 elist.list = UU_NULL;
		 err = vx_mdl_inq_ety_list (itype[i], &elist);
	  }
#endif
	  for (j=0; j<elist.num; j++)
		 {
		 err = vx_mdl_inq_name (elist.list[j], vxname);
		 if (err == 0)
			{
			for (k=0;k<64 && vxname[k];k++)
			  if (islower(vxname[k])) vxname[k]=toupper(vxname[k]);
			status = ncl_vx_find (vxname, isub, &nclkey, &nwds, &ietype);
			}
		 }
	  if (elist.list != UU_NULL) free (elist.list);
	  }

	ur_put_attrmdl (&hldattr);
	ncl_set_color_attr (hldcolor);
	UM_srfattr.numupaths = nupsv;
	UM_srfattr.numvpaths = nvpsv;

	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int vxjump (iflg)
**       Jump out of NCL into VX.
**    PARAMETERS
**       INPUT  :
**          iflg     - = 0, normal exit.
**                     = 1, restart.
**                     = 2, exit from signon.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
vxjump (iflg)
UM_int2 *iflg;
	{
	int status;

	if (*iflg != 2) NCL_exit_flag = *iflg;
	uw_mfmenu_reset(UU_FALSE,UU_FALSE,UU_FALSE);
	longjmp(NCL_ebuf1,1);

	}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_plotm (fpt, tpt)
**       Plot motion to VX graphics screen.
**    PARAMETERS
**       INPUT  :
**          fpt      - Start point of motion.
**          tpt      - End point of motion.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_plotm (fpt, tpt)
UM_real8 fpt[], tpt[];
	{
	int status;
	int i;
	double spt[3], ept[6];

	for (i=0; i<3; i++)
	  {
	  spt[i] = fpt[i];
	  ept[i] = tpt[i];
	  ept[i+3] = tpt[i+3];
	  }

	status = vx_ncl_plotm (spt, ept);

	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_get_units()
**       Set units from VX.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_get_units()
	{
	int status;
	mdl_default vx_def_lin_units = MDL_DEF_LIN_UNIT;
	mdl_lin_unit vx_units;
	UM_int2 idx, ival;

	status = vx_mdl_inq_default (vx_def_lin_units, &vx_units);
	if (status == 0)
		{
		idx = 264;
		getifl (&idx, &ival);
		idx = 0;
		if (vx_units == MDL_MM) idx = 1;
		if (ival != idx)
			{
			if (idx == 1) millim(); else inches();
			}
		}
	}
/*********************************************************************
**    E_FUNCTION     : int vx_gettrm (nline, f77_prompt, f77_cin)
**       Get a line of input from VX.
**    PARAMETERS
**       INPUT  :
**          nline       - NCL line number (for prompt)
**          f77_prompt  - Current statement at part program line nline.
**       OUTPUT :
**          f77_cin     - Line received from VX.
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
vx_gettrm(nline, f77_prompt, f77_cin)
UM_int4 *nline;
UM_f77_str_ptr f77_prompt, f77_cin;
{
	int i,nc;
	char prompt[20], edit_str[80], *pstr, *cstr;

	pstr = UM_cstr_of_f77_str(f77_prompt);
	cstr = UM_cstr_of_f77_str(f77_cin);

	sprintf(prompt, "NCL:%d>",*nline);
	for (nc=71; nc>=0 && pstr[nc] == ' '; nc--);
	nc++;
	for (i=0; i<nc; i++) edit_str[i] = pstr[i];
	edit_str[nc] = '\0';
	i = vx_ncl_prompt (prompt, edit_str, cstr);

	nc = strlen(cstr);
	for (i=nc-1;i<72;i++) cstr[i] = ' ';
}

/*********************************************************************
**    E_FUNCTION     : vx_error(kerr,cout)
**       Output error to VX.
**    PARAMETERS
**       INPUT  :
**          kerr        - error number.
**          cout        - error string.
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vx_error(kerr,cout)
UM_int2 *kerr;
UM_f77_str_ptr cout;
{
	int i;
	char *p;

	p = UM_cstr_of_f77_str(cout);
	for (i=79; i>=0; i--) if (p[i] != ' ') break;
	p[i+1] = '\n';
	p[i+2] = '\0';
	vx_ncl_putmsg(p);
	printf("Error = %d:  %s\n",*kerr,p);
}

/*********************************************************************
**    E_FUNCTION     : vx_ersw3 ()
**       VX erase window 3.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
vx_ersw3()
{
	printf("Erase Window 3\n");
}

/*********************************************************************
**    E_FUNCTION     : vx_getsrc (cin,kline)
**       VX get source.
**    PARAMETERS
**       INPUT  :
**          kline       - line number.
**       OUTPUT :
**          cin         - Line form source.
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
vx_getsrc(cin,kline)
UM_f77_str_ptr cin;
UM_int2 *kline;
{
	int i,nc;
	char prompt[80], *cstr;

/*  printf("%d: ",*kline); */
	cstr = UM_cstr_of_f77_str(cin);
/*   gets(cstr); */
	sprintf(prompt, "NCL:%d> ",*kline+1);
	i = vx_ncl_prompt (prompt, cstr);
	nc = strlen(cstr);
	for (i=nc;i<72;i++) cstr[i] = ' ';
}

/*********************************************************************
**    E_FUNCTION     : vx_includ (cfile, knc, kinc)
**       VX include.
**    PARAMETERS
**       INPUT  :
**          cfile       - File name.
**          knc         - Number of chars in 'cfile'.
**          kinc        - inc flag.
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
vx_includ(cfil,knc,kinc)
UM_f77_str_ptr cfil;
UM_int4 *knc;
UM_int2 *kinc;
{
	char *p;
	p = UM_cstr_of_f77_str(cfil);
	p[*knc] = '\0';
	printf("INCLUD/%s,%d\n",p,*kinc);
}

/*********************************************************************
**    E_FUNCTION     : vx_putapt (cout, knc)
**       VX putapt.
**    PARAMETERS
**       INPUT  :
**          cout        -
**          knc         -
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
vx_putapt(cout,knc)
UM_f77_str_ptr cout;
UM_int2 *knc;
{
	int i;
	char *p,mbuf[80];
	p = UM_cstr_of_f77_str(cout);
	for (i=0;i<*knc;i++) mbuf[i] = p[i];
	mbuf[*knc] = '\0';
	printf("AS = %s\n",mbuf);
}

/*********************************************************************
**    E_FUNCTION     : vx_putmsg (cout, knc, kline, kers)
**       VX putmsg.
**    PARAMETERS
**       INPUT  :
**          cout        -
**          knc         -
**          kline       -
**          kers        -
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
vx_putmsg(cout,knc,klin,kers)
UM_f77_str_ptr cout;
UM_int2 *knc,*klin,*kers;
{
	int i;
	char *p;

	printf("PM line = %d   erase = %d\n",*klin,*kers);

	if (*klin > 5)
	  {
	  p = UM_cstr_of_f77_str(cout);
	  for (i=79; i>=0; i--) if (p[i] != ' ') break;
	  p[i+1] = '\n';
	  p[i+2] = '\0';
	  vx_ncl_putmsg(p);
	  printf("%s\n",p);
	  }
}

/*********************************************************************
**    E_FUNCTION     : vx_putw2 (cout, knc)
**       VX putw2.
**    PARAMETERS
**       INPUT  :
**          cout        -
**          knc         -
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
vx_putw2(cout,knc)
UM_f77_str_ptr cout;
UM_int2 *knc;
{
	int i;
	char *p;
	p = UM_cstr_of_f77_str(cout);
	for (i=71; i>=0; i--) if (p[i] != ' ') break;
	p[i+1] = '\n';
	p[i+2] = '\0';
	vx_ncl_putw2(p);
	printf("W2 = %s\n",p);
}

int vx_ncl_prompt(prompt,edit_str,response)

char *prompt;
char *edit_str;
char *response;

{
	int err = 0;
	int cmd;

	err = vx_uif_tty_print(NCL_WIN,NCL_TTY,prompt);
	if (err) return(err);

/*   -----------------KLUDGE---------------------------   */
/*    temporary fix of timing problem displaying prompt   */
/*   -----------------KLUDGE---------------------------   */
		  sleep(1);
/*   -----------------KLUDGE---------------------------   */

	err = vx_uif_tty_input(NCL_WIN,NCL_TTY,edit_str);
	if (err) return(err);

	err = vx_wait_for_command(10010,10010,&cmd);
	if (err) return(err);

	if (strlen(Uif_data.str) < 1) return 1;

	strcpy(response,Uif_data.str);

	return(err);
}

int vx_ncl_putmsg(str)

char *str;

{
	int err = 0;

	err = vx_uif_tty_print(NCL_WIN,NCL_MSG,str);

	return(err);
}

int vx_ncl_putw2(str)

char *str;

{
	int err = 0;

	err = vx_uif_txt_edit_ins (NCL_WIN,1,-1, str);

	return(err);
}

int vx_command(command)

int command;

{

int 	err = 0;
static int running = 0;

switch(command) {

	case 100000: /* Display NCL Menu */

		/* display window */
		err = vx_uif_win_show(NCL_WIN,1);
		if (err) break;

		/* main NCL statement processor */
		if (!running)
			{
			running = 1;
			err = vx_ncl_exec(0);
			running = 0;
			}
	
		break;

	case 101000: /* RUn old NCL */

		/* main NCL statement processor */
		if (!running)
			{
			running = 1;
			err = vx_ncl(1);
			running = 0;
			}
	
		break;

	case 100001: /* New NCL program */

		err = vx_uif_tty_input(NCL_WIN,NCL_TTY,"*start");
		if (err) break;
		err = vx_uif_tty_trigger(NCL_WIN,NCL_TTY);
		if (err) break;

		/* re-initialize NCL */
		err = vx_ncl_init();
		if (err) break;

		break;

	case 100002: /* Load NCL Program */

		err = vx_uif_tty_input(NCL_WIN,NCL_TTY,"*loadpp/");
		if (err) break;

		break;

	case 100003: /* Save NCL Program */

		err = vx_uif_tty_input(NCL_WIN,NCL_TTY,"*savepp/");
		if (err) break;

		break;

	case 100004: /* Run NCL Program */

		err = vx_uif_tty_input(NCL_WIN,NCL_TTY,"*run");
		if (err) break;
		err = vx_uif_tty_trigger(NCL_WIN,NCL_TTY);
		if (err) break;

		break;

	case 100005: /* Regen NCL Display */

		err = vx_uif_tty_input(NCL_WIN,NCL_TTY,"*regen");
		if (err) break;
		err = vx_uif_tty_trigger(NCL_WIN,NCL_TTY);
		if (err) break;

		break;

	case 100006: /* Quit NCL */

		err = vx_uif_tty_input(NCL_WIN,NCL_TTY,"*quit");
		if (err) break;
		err = vx_uif_tty_trigger(NCL_WIN,NCL_TTY);
		if (err) break;
		break;


	case 100010:

		break;

	default:

		err = 1;
		break;
	}

	return(err);
}

int vx_ncl_find (idx, nwds, ietype, vxtype)

int idx;
int *nwds;
int *ietype;
int *vxtype;

{
	int err;
	mdl_ety_type type;

	err = vx_mdl_inq_type (idx, &type);
	if (err == 0)
	  {  
	  *vxtype = type;
	  switch (type)
		 {
		 case MDL_ETY_POINT:
			*ietype = 3;
			*nwds = 3;
			break;
		 case MDL_ETY_LINE3:
			*ietype = 5;
			*nwds = 6;
			break;
		 case MDL_ETY_CIR3:
			*ietype = 7;
			*nwds = 11;
			break;
		 case MDL_ETY_CURV:
		 case MDL_ETY_FCURV:
		 case MDL_ETY_FNCURV:
		 case MDL_ETY_ELL3:
		 case MDL_ETY_CONIC3:
			*ietype = 8;
			*nwds = 1;
			break;
		 case MDL_ETY_PLANE:
		 case MDL_ETY_SURF:
		 case MDL_ETY_SURFREV:
		 case MDL_ETY_SPHERE:
		 case MDL_ETY_CYLINDER:
		 case MDL_ETY_CONE:
		 case MDL_ETY_TORUS:
		 case MDL_ETY_ELLIPSOID:
		 case MDL_ETY_EXTRUSION:
			*ietype = 9;
			*nwds = 1;
			break;
		 default:
			err = 1;
			*ietype = 1;
			*nwds = 1;
		 }
	  }  

	return(err);
}

int vx_shutdown()
{
	int err = 1;

	/*
	**  Use this function for any kind of cleanup that you should do.
	*/

/*      err = vx_uif_win_destroy(10001); */

	return(err);

}

int vx_startup()
{
	/*
	**  Use this function for any kind of setup your program requires.
	*/

	int err = 1;

	/* create a window */
	err = vx_uif_win_create(TASK_NAME,NCL_WIN,500,600,1,3,3,210,100,0,0,2,1,2,1,
	"screen-bold-14","NCL/VX Version 1.0, Copyright 1994");
	if (err) return(err);
	
	/* add buttons */
	err = vx_uif_win_add_btns(NCL_WIN,1,1,3,1,6,1,0,0,0,0,0,25,120,3,0,0,
	20,21,22,23,26,24,0,"screen-bold-12");
	if (err) return(err);

	/* label buttons */
	err = vx_uif_btn_set_text(NCL_WIN,1,1,0,0,0,2,NCL_WIN,"New NCL");
	if (err) return(err);

	err = vx_uif_btn_set_text(NCL_WIN,1,2,0,0,0,2,10002,"Load NCL");
	if (err) return(err);

	err = vx_uif_btn_set_text(NCL_WIN,1,3,0,0,0,2,10003,"Save NCL");
	if (err) return(err);

	err = vx_uif_btn_set_text(NCL_WIN,1,4,0,0,0,2,10004,"Run NCL");
	if (err) return(err);

	err = vx_uif_btn_set_text(NCL_WIN,1,5,0,0,0,2,10005,"Regen NCL");
	if (err) return(err);

	err = vx_uif_btn_set_text(NCL_WIN,1,6,0,0,0,2,10006,"Quit NCL");
	if (err) return(err);

	/* add text edit panel */
	err = vx_uif_win_add_txt_edit(NCL_WIN,1,1,1,5,1,1,1,0,0,275,720,
	"screen-bold-12");
	if (err) return(err);

	/* add tty for input*/
	err = vx_uif_win_add_tty(NCL_WIN,NCL_TTY,10010,
	1,1,1,1,5,7,1,0,0,0,100,720,"screen-bold-12");
	if (err) return(err);

	/* add tty for messages*/
	err = vx_uif_win_add_tty(NCL_WIN,NCL_MSG,10011,
	1,1,1,1,5,6,NCL_TTY,0,0,0,100,720,"screen-bold-12");
	if (err) return(err);

	/* initialize ncl */
/*	err = vx_ncl_init(); */
/*	if (err) return(err); */

	return(0);
}

int vx_ncl_init()
{
	int err = 0;

	/* reset line number */
	Line_Num = 1;

	/* clear program window */
	err = vx_uif_txt_edit_clr(NCL_WIN,1,0,-1);
	if (err) return(err);

	/* clear input tty */
	err = vx_uif_tty_clr(NCL_WIN,NCL_TTY);
	if (err) return(err);

	/* clear message tty */
	err = vx_uif_tty_clr(NCL_WIN,NCL_MSG);
	if (err) return(err);

	return(0);
}

int vx_ncl_exec(itsk)

int itsk;

{
	int err = 0;

	err = vx_ncl_init();
	if (err) return(err);

	err = vx_ncl(itsk);
	if (err) return(err);

	return(err);

}

int vx_ncl_plotm (spt, ept)

double spt[], ept[];

{
	int err;
	int n1, n2;
	mdl_ln_bundle ln_att;
	mdl_pnt_bundle pt_att;
	mdl_line ln;
	mdl_pnt  pt;
	mdl_data vx_rslt;
	char set[12];

	strcpy(set, "NCL_MOTION");

	ln_att.ln_type = D_LIN_SOLID;
	ln_att.color   = 1;
	ln_att.width   = 1;

	ln.p1.x = spt[0];
	ln.p1.y = spt[1];
	ln.p1.z = spt[2];
	ln.p2.x = ept[0];
	ln.p2.y = ept[1];
	ln.p2.z = ept[2];

	pt_att.pnt_type = D_PNT_POINT;
	pt_att.color    = 2;
	pt_att.size     = 50;
	pt.x = spt[0];
	pt.y = spt[1];
	pt.z = spt[2];

	n1 = 1;
	n2 = 1;

/*   err = vx_mdl_display_pnt_line (set, &ln_att, &pt_att, n1, &ln, n2, &pt); */

	return(err);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_mergecv(itsk)
**       Set up composite curve components list.
**    PARAMETERS
**       INPUT  :
**          itsk       - 0 = run VX window, 1 = run old NCL.
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_mergecv(num,keys,lcid,crv)
int num;
UU_KEY_ID *keys;
struct UM_cid_rec *lcid;
struct UM_compcrv_rec  *crv;
{
	int status, i, j;
	UU_REAL length, um_getarclen();
	struct NCL_fixed_databag e;

	status = UU_SUCCESS;
	for(i=0; i<num && status==UU_SUCCESS; i++)
	{
/*
.....retrieve curve component
*/
		e.key = keys[i];
		status = ncl_retrieve_data_fixed (&e);
		if (status == UU_SUCCESS)
		{
/*
.....get arc length and set reverse flag if lcid[i].crvid
.....is set to 0 (in caller)
*/
			lcid[i].endparam = um_getarclen(&e, UU_NULL);
			crv->arclen += lcid[i].endparam;
			if (lcid[i].crvid == 0) lcid[i].reverse = UU_TRUE;
			else lcid[i].reverse = UU_FALSE;
			lcid[i].crvid = keys[i];
		}
	}
	if (status == UU_SUCCESS)
	{
		ur_update_data_fixed (crv);
		length = 0.0;
		for (i=0; i<num; i++)
		{
			length += lcid[i].endparam;
			lcid[i].endparam = length/crv->arclen;
		}
/*
.....update cid list in composite curve entity
*/
		status = ur_update_data_varlist (crv->key, 1, lcid, 1, num);
	}
	return (status);
}
#endif
