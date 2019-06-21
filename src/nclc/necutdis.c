/*********************************************************************
**    NAME         :  necutdis.c
**       CONTAINS:
**             ncl_cutter_post
**             ncl_cutter_post_drag
**             ncl_cutter_box_init
**             ncl_cutter_box_add
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       necutdis.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:27
*********************************************************************/
#include "usysdef.h"
#include <math.h>
#include "drubber.h"
#include "driver.h"
#include "nclfc.h"
#include "gtbl.h"
#include "gobas.h"
#include "view.h"
#include "ginqatt.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdattr.h"
#include "modef.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "zsysdep.h"
#include "mplot.h"
#include "mdraw.h"
#include "bsym.h"
#include "nclmplay.h"
#include "lcom.h"

#include "dasnog.h"
#include "ulist.h"
#include "mxxx.h"
#include "mdgenent.h"

UU_REAL Temp_vpn[3], Temp_vup[3];
UM_transf Temp_tfmat;
int Proj_motion = 0;
UU_REAL Temp_scale;

void ncl_cutter_box_add();

static void S_cutter_recalculate();
static void S_bounding_box();
static void S_symbol_bounds();
static void S_cutter_symbol();
static void S_calc_offset();

static int Scutter_nview = -1;

extern int DRAW_DISPLAY;
extern int UD_cmap_changed, NCL_swap_changed;
UN_cutdef_view_struc ncl_drawing_cutstct;
extern int NCL_move_save;
extern UN_motseg *NCL_save_mptr;

int NCL_animation = 0;

int NCL_erase_cut[UV_NVPORTS] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
int dyn_view = 0;

static int Svp,Smovfl,Serase,Sshaded[3];
static UU_LOGICAL Srecalc[3],Sredisp;

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_post(cpt,bptr,cutseg,movfl,erase)
**			Calculates and displays the cutter symbol at the current
**			location.
**    PARAMETERS   
**       INPUT  : 
**				cpt		= Point location to display cutter at.
**				bptr		= Pointer to blade position for this location.
** 	      cutseg   = Active cutter definition.
**				movfl    = 1 = Display moving cutter.
**				           2 = Display moving part.
**				erase    = 1 = Cutter is being erased.  Use segment erase
**				              color.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_post(cpt,bptr,cutseg,movfl,erase)
int erase,movfl,bptr;
UM_real8 cpt[];
UN_motseg_cutter *cutseg;
{
#define MXINC 100
#define MXGPT 500
	int i,j,ipt,erscut,cutfl,save_mask,mxj,segfl,*vnpt;
	UU_LOGICAL recalc[3],redisp;
	Gwpoint3 gpt[MXGPT],gnorm[MXGPT],*vgpt,*vgnorm;
	UM_vector vpn,vup;
	UU_REAL vscale,ofs[3],sofs[3],hofs[3];
	UU_REAL um_dot(),yrot,zrot;
	UM_transf tfr;
	Glntype lintyp,linestyle;
	Gscale linwid;
	int vmode,myshade,mytrans;
	UM_coord pt;
	int save_lucency;
	int save_linetype;
	Gfloat save_linewid, uw_getsave_linewid();
	UN_motseg *mpt;
	UN_motseg_blade blade;
	UN_mot_vpbox_struc box;
/*
.....Get current viewport extents
*/
	ncl_motvwp(&Svp,vpn,vup,&vscale,&vmode);
/*
.....Initialize routine
*/
	cutfl = cutseg->cutsym->geo->type;
	segfl = cutseg->cattr->segfl; if (dyn_view == 1) segfl = 2;
	Smovfl = movfl;
	Serase = erase;
	if (movfl == 1 && dyn_view != 1 &&
		(cutseg->cattr->shaded[0] == 1 || cutseg->cattr->shaded[1] == 1 ||
		cutseg->cattr->shaded[2] == 1 || segfl == 1))
			cutdef[Svp].seguse = UU_TRUE;
	else cutdef[Svp].seguse = UU_FALSE;
	ncl_motblade_get(&blade,bptr);
/*
.....Set view parameters when projecting to a drawing
*/
	if (DRAW_DISPLAY==1)
	{
		vpn[0] = Temp_vpn[0];
		vpn[1] = Temp_vpn[1];
		vpn[2] = Temp_vpn[2];
		vup[0] = Temp_vup[0];
		vup[1] = Temp_vup[1];
		vup[2] = Temp_vup[2];
		cutdef[Svp].mov = 0;
		if (Proj_motion==1) vscale = Temp_scale;
	}
	else if (cutdef[Svp].seguse)
	{
		vpn[0] = 0.; vpn[1] = 1.; vpn[2] = 0.;
	}
	um_unitvc(vpn,vpn);
/*
.....Cutter is not displayed and
.....previous cutter was moving
.....Erase previous cutter symbol
*/
	if (cutdef[Svp].mov == 1 && cutfl == 0)
	{
		ncl_erase_cutseg(Svp,&cutdef[Svp].view.box);
		goto done;
	}
/*
.....Determine if cutter can be shaded
*/
	Sshaded[0] = Sshaded[1] = Sshaded[2] = 0;
	if (cutseg->cattr->shaded[0] != 0 || cutseg->cattr->shaded[1] != 0 ||
		cutseg->cattr->shaded[2] != 0)
	{
		if (vmode == 2 || vmode == 3)
		{
			Sshaded[0] = cutseg->cattr->shaded[0];
			Sshaded[1] = cutseg->cattr->shaded[1];
			Sshaded[2] = cutseg->cattr->shaded[2];
		}
	}
/*
.....Determine if we have to recalculate
.....the cutter
*/
	S_cutter_recalculate(cpt,&blade,cutseg,Smovfl,recalc,&redisp);
/*
.....Save cutter definition
*/
	cutdef[Svp].cutfl[0] = cutseg->cutsym->geo->type;
	cutdef[Svp].cutfl[1] = cutseg->shank->geo->type;
	cutdef[Svp].cutfl[2] = cutseg->holder->geo->type;
	cutdef[Svp].segfl = segfl;
	cutdef[Svp].mov = Smovfl;
	cutdef[Svp].view.scale = vscale;
	for (i=0;i<3;i++)
	{
		cutdef[Svp].color[i] = cutseg->cattr->color[i];
		cutdef[Svp].shaded[i] = cutseg->cattr->shaded[i];
		cutdef[Svp].trans[i] = cutseg->cattr->trans[i];
	}
	strcpy(cutdef[Svp].symbol[0],cutseg->cutsym->geo->symbol);
	strcpy(cutdef[Svp].symbol[1],cutseg->shank->geo->symbol);
	strcpy(cutdef[Svp].symbol[2],cutseg->holder->geo->symbol);
/*
.....If previous cutter was moving then
.....Erase previous cutter symbol
*/
	erscut = 0;
	if (cutdef[Svp].mov == 1)
	{
		erscut = 1;
		if (NCL_erase_cut[Svp] == 0 && cutdef[Svp].seguse) erscut = 0;
/*
.....Dynamic viewing already cleared the viewport
.....prior to drawing anythin in OpenGL
*/
		if (dyn_view == 1) erscut = 0;
	}
/*
.....If we changed the color map
.....We need to recalculate and draw the cutter
.....Yurong  -  6/27/97
*/
	if (UD_cmap_changed==1 || NCL_swap_changed == 1)
	{
		erscut = 0;
		redisp = UU_TRUE;
		UD_cmap_changed = 0;
		NCL_swap_changed = 0;
	}
/*
........Set cutter display attributes
*/
#ifdef UU_OPENGL
	save_linewid = uw_getsave_linewid();
	save_linetype = uw_getsave_linetype();
#endif
	zbytecp(lintyp,*gqlinetype());
	linwid = gqlinewidth();

	linestyle.typeno = UM_SOLID_LINE;
	linestyle.npatn = 0;
	gslinetype(&linestyle);
	if (Serase == 1) gslinecolor(0);
	else gslinecolor(cutseg->cattr->color[0]);
	gslinewidth(1.0);
/*
.....Store current cutter definition
*/
	for (i=0;i<9;i++) cutdef[Svp].cutr[i] = cutseg->cutter->parms[i];
	for (i=0;i<4;i++)
	{
		cutdef[Svp].cutr[i+9] = cutseg->holder->parms[i];
		cutdef[Svp].cutr[i+13] = cutseg->shank->parms[i];
	}
/*
.....if Full segment and openGL moving cutter
.....don't recalculate cutter, only
.....use matrix rotation when display cutter
.....the tlax always use (1,0,0)
.....Yurong 6/14/99
.....If it is a full cutter use matrix rotation.
*/
	if (!cutdef[Svp].seguse)
	{
		for (i=0; i<3; i++) cutdef[Svp].tlax[i] = cpt[i+3];
		if (ncl_cutter_is_blade())
		{
			for (i=0; i<3; i++) cutdef[Svp].vfd[i]  = blade.tfwd[i];
		}
	}
	else
	{
		cutdef[Svp].tlax[0] = 1;
		cutdef[Svp].tlax[1] = 0;
		cutdef[Svp].tlax[2] = 0;
		if (ncl_cutter_is_blade())
		{
			pt[0] = pt[1] = pt[2] = 0.;
			ncl_cutsym_tf(pt,&cpt[3],tfr,&yrot,&zrot,1);
			um_vctmtf(blade.tfwd,tfr,cutdef[Svp].vfd);
		}
	}
	for (i=0; i<3; i++)
	{
		cutdef[Svp].view.vpn[i] = vpn[i];
		cutdef[Svp].view.vup[i] = vup[i];
	}
/*
.....Recalculate cutter shapes
*/
	ncl_draw_cutter_parts(cutseg,recalc,Svp,0);
/*
.....Calculate the display and bounding box
.....of the cutter, shank, and holder
*/
	if (redisp) S_bounding_box(cutseg,&blade,cpt,&box,sofs,hofs);
	else
	{
		um_vctovc(cutdef[Svp].view.box.ll,box.ll);
		um_vctovc(cutdef[Svp].view.box.ur,box.ur);
	}
/*
.....Open moving cutter segment
.....if new cutter has been calculated
.....and cutter is moving
*/
	if (Smovfl == 1 && redisp) ncl_open_cutseg(Svp,&cutdef[Svp].view.box);
/*
.....Display cutter symbol
*/
	ofs[0] = ofs[1] = ofs[2] = 0.;
	if (cutseg->cutsym->geo->type == 2 && cutseg->cutsym->geo->segno != 0)
		S_cutter_symbol(cpt,cutseg,cutseg->cutsym,&blade,redisp,ofs);
/*
.....Display shank symbol
*/
	if (cutseg->shank->geo->type == 2 && cutseg->shank->geo->segno != 0)
		S_cutter_symbol(cpt,cutseg,cutseg->shank,&blade,redisp,sofs);
/*
.....Display holder symbol
*/
	if (cutseg->holder->geo->type == 2 && cutseg->holder->geo->segno != 0)
		S_cutter_symbol(cpt,cutseg,cutseg->holder,&blade,redisp,hofs);
/*
.....Creating a drawing
*/
	if (Proj_motion) ncl_drawing_cutstct = cutdef[Svp].view;
/*
.....NCL calculated cutter display
*/
	else if (cutdef[Svp].view.ngeo != 0)
	{
		save_lucency = ug_get_lucency();
/*
........Change the cutter position
........to the current motion position
*/
		if (Smovfl == 0 || redisp)
		{
			if (cutseg->cattr->color[0] != 0)
			{
/*
...........Do the actual drawing of the cutter
...........If erasing the cutter, then
...........Draw the cutter using black color but don't use depth
*/
#ifdef UU_OPENGL
				if (Serase == 1)
				{
					save_mask = uw_glget_depthmask();
					uw_gldepth_mask(0);
				}
#endif
				ipt = 0;
				if (Serase == 1) gslinecolor(0);
				else gslinecolor(cutseg->cattr->color[0]);  
				myshade = cutseg->cattr->shaded[0];
				mytrans = cutseg->cattr->trans[0];
				vnpt = (int *)UU_LIST_ARRAY(&cutdef[Svp].view.npt);
				vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[Svp].view.gpt);
				vgnorm = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[Svp].view.gnorm);
				for (i=0; i<cutdef[Svp].view.ngeo; i++)
				{
					mxj = vnpt[i];
					if (mxj > MXGPT) mxj = MXGPT;
/*
........Set the cutter attributes
*/
					if (mxj < 0)
					{
						if (Serase == 0)
						{
							gslinecolor(-vnpt[i]); i++;
							myshade = -vnpt[i]; i++;
							mytrans = -vnpt[i];
							ug_set_lucency(mytrans);
						}
						else i = i + 2;
					}
/*
........Display the cutter segment
*/
					else
					{
						for (j=0; j<mxj; j++)
						{
							gnorm[j].x = vgnorm[ipt].x;
							gnorm[j].y = vgnorm[ipt].y;
							gnorm[j].z = vgnorm[ipt].z;
							if (Smovfl == 0)
							{
								gpt[j].x = cpt[0] + vgpt[ipt].x;
								gpt[j].y = cpt[1] + vgpt[ipt].y;
								gpt[j].z = cpt[2] + vgpt[ipt].z;
								ncl_cutter_box_add(&cutdef[Svp].view.box,&gpt);
							}
							else
							{
								gpt[j].x = vgpt[ipt].x;
								gpt[j].y = vgpt[ipt].y;
								gpt[j].z = vgpt[ipt].z;
								ncl_cutter_box_add(&cutdef[Svp].view.box,&vgpt[ipt]);
							}
							ipt++;
						}
						if (myshade)
							gshadearea(mxj,gpt,gnorm,1);
						else
							gpolyline3(mxj,gpt);
					}
				}
			}
		}
		ug_set_lucency(save_lucency);
	}
	if (Smovfl == 1 && redisp) ncl_close_cutseg(Svp);
	if (Serase == 1 && erscut != 1)
	{
#ifdef UU_OPENGL
		uw_gldepth_mask(save_mask);
#endif
	}
	if (erscut == 1) ncl_erase_cutseg(Svp,&box);

	if (NCL_move_save==1)
	{
		mpt = 0;
		if (NCL_save_mptr!=0)
		{
			mpt = (UN_motseg *)uu_lsprev(NCL_save_mptr);
			if (mpt!=0) 
			{
				mpt = (UN_motseg *)uu_lsprev(mpt);
			}
		}
	}
	else if (NCL_move_save != 0)
	{
		mpt = 0;
		if (NCL_save_mptr!=0)
		{
			mpt = (UN_motseg *)uu_lsnext(NCL_save_mptr);
		}
		NCL_move_save = 0;
	}
/*
.....Moving cutter
*/
	if (Smovfl == 1)
	{
		ncl_postn_cutseg(Svp,Svp,cpt,UU_TRUE);
	} 
done:;
/*
.....Reset line attributes
*/
#ifdef UU_OPENGL
	uw_setsave_linetype(save_linetype);
	uw_setsave_linewid (save_linewid);
#endif
	gslinetype(&lintyp);
	gslinewidth(linwid);
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_post_drag(cutseg)
**			Calculates and draws the cutter symbol for dragging.  This is
**			a pared down 'ncl_cutter_post' routine.
**    PARAMETERS   
**       INPUT  : 
** 	      cutseg   = Active cutter definition.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_post_drag(cutseg)
UN_motseg_cutter *cutseg;
{
	int i,j,ipt,mxj,*vnpt,nview;
	UU_LOGICAL recalc[3],redisp;
	Gwpoint3 gpt[MXGPT],*vgpt;
	UM_vector vpn,vup;
	UU_REAL vscale,ofs[3],sofs[3],hofs[3],cpt[6];
	Glntype lintyp,linestyle;
	Gscale linwid;
	int vmode;
	int save_linetype;
	Gfloat save_linewid, uw_getsave_linewid();
	UN_motseg_blade blade;
	UN_mot_vpbox_struc box;
	UN_cutdef_struc cutdef_save;
	UU_LIST list_npt,list_gpt,list_gnorm;
/*
.....Get current viewport extents
*/
	ncl_motvwp(&Svp,vpn,vup,&vscale,&vmode);
	um_unitvc(vpn,vpn);
/*
.....Save active cutter structure
*/
	cutdef_save = cutdef[Svp];
	nview = Scutter_nview;
	if (nview != -1)
	{
		uu_list_init(&list_npt,sizeof(int),100,50);
		uu_list_init(&list_gpt,sizeof(Gwpoint3),500,100);
		uu_list_init(&list_gnorm,sizeof(Gwpoint3),500,100);
		uu_list_push_list(&list_npt,&cutdef[0].view.npt);
		uu_list_push_list(&list_gpt,&cutdef[0].view.gpt);
		uu_list_push_list(&list_gnorm,&cutdef[0].view.gnorm);
	}
/*
.....Initialize routine
*/
	cpt[0] = cpt[1] = cpt[2] = 0.;
	um_vctovc(vpn,&cpt[3]);
	ncl_motblade_get(&blade,UN_MOTSEG_ACTIVE);
	Smovfl = Serase = Sshaded[0] = Sshaded[1] = Sshaded[2] = 0;
/*
.....Save cutter definition
*/
	cutdef[Svp].cutfl[0] = cutseg->cutsym->geo->type;
	cutdef[Svp].cutfl[1] = cutseg->shank->geo->type;
	cutdef[Svp].cutfl[2] = cutseg->holder->geo->type;
	cutdef[Svp].mov = 0;
	cutdef[Svp].segfl = UU_FALSE;
	cutdef[Svp].seguse = UU_FALSE;
	cutdef[Svp].view.scale = vscale;
	for (i=0;i<3;i++)
	{
		cutdef[Svp].color[i] = cutseg->cattr->color[i];
		cutdef[Svp].shaded[i] = 0;
	}
	strcpy(cutdef[Svp].symbol[0],cutseg->cutsym->geo->symbol);
	strcpy(cutdef[Svp].symbol[1],cutseg->shank->geo->symbol);
	strcpy(cutdef[Svp].symbol[2],cutseg->holder->geo->symbol);
	for (i=0;i<9;i++) cutdef[Svp].cutr[i] = cutseg->cutter->parms[i];
	for (i=0;i<4;i++)
	{
		cutdef[Svp].cutr[i+9] = cutseg->holder->parms[i];
		cutdef[Svp].cutr[i+13] = cutseg->shank->parms[i];
	}
	for (i=0; i<3; i++) cutdef[Svp].tlax[i] = cpt[i+3];
	if (ncl_cutter_is_blade())
	{
		for (i=0; i<3; i++) cutdef[Svp].vfd[i]  = blade.tfwd[i];
	}
	for (i=0; i<3; i++)
	{
		cutdef[Svp].view.vpn[i] = vpn[i];
		cutdef[Svp].view.vup[i] = vup[i];
	}
/*
.....Determine if we have to recalculate
.....the cutter
*/
	recalc[0] = recalc[1] = recalc[2] = UU_FALSE;
	for (i=0;i<3;i++) if (cutdef[Svp].cutfl[i] == 1) recalc[i] = UU_TRUE;
	redisp = UU_TRUE;
/*
........Set cutter display attributes
*/
#ifdef UU_OPENGL
	save_linewid = uw_getsave_linewid();
	save_linetype = uw_getsave_linetype();
#endif
	zbytecp(lintyp,*gqlinetype());
	linwid = gqlinewidth();

	linestyle.typeno = UM_SOLID_LINE;
	linestyle.npatn = 0;
	gslinetype(&linestyle);
	gslinecolor(cutseg->cattr->color[0]);
	gslinewidth(1.0);
/*
.....Recalculate cutter shapes
*/
	ncl_draw_cutter_parts(cutseg,recalc,Svp,0);
/*
.....Calculate the display and bounding box
.....of the cutter, shank, and holder
*/
	S_bounding_box(cutseg,&blade,cpt,&box,sofs,hofs);
/*
.....Display cutter symbol
*/
	ofs[0] = ofs[1] = ofs[2] = 0.;
	if (cutseg->cutsym->geo->type == 2 && cutseg->cutsym->geo->segno != 0)
		S_cutter_symbol(cpt,cutseg,cutseg->cutsym,&blade,redisp,ofs);
/*
.....Display shank symbol
*/
	if (cutseg->shank->geo->type == 2 && cutseg->shank->geo->segno != 0)
		S_cutter_symbol(cpt,cutseg,cutseg->shank,&blade,redisp,sofs);
/*
.....Display holder symbol
*/
	if (cutseg->holder->geo->type == 2 && cutseg->holder->geo->segno != 0)
		S_cutter_symbol(cpt,cutseg,cutseg->holder,&blade,redisp,hofs);
/*
.....NCL calculated cutter display
*/
	if (cutdef[Svp].view.ngeo != 0)
	{
/*
........Change the cutter position
........to the current motion position
*/
		if (cutseg->cattr->color[0] != 0)
		{
			ipt = 0;
			gslinecolor(cutseg->cattr->color[0]);  
			vnpt = (int *)UU_LIST_ARRAY(&cutdef[Svp].view.npt);
			vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[Svp].view.gpt);
			for (i=0; i<cutdef[Svp].view.ngeo; i++)
			{
				mxj = vnpt[i];
				if (mxj > MXGPT) mxj = MXGPT;
/*
........Display the cutter segment
*/
				if (mxj < 0)
				{
					gslinecolor(-vnpt[i]); i = i + 2;
				}
				else
				{
					for (j=0; j<mxj; j++)
					{
						gpt[j].x = cpt[0] + vgpt[ipt].x;
						gpt[j].y = cpt[1] + vgpt[ipt].y;
						gpt[j].z = cpt[2] + vgpt[ipt].z;
						ipt++;
					}
					gpolyline3(mxj,gpt);
				}
			}
		}
	}
done:;
/*
.....Reset line attributes
*/
#ifdef UU_OPENGL
	uw_setsave_linetype(save_linetype);
	uw_setsave_linewid (save_linewid);
#endif
	gslinetype(&lintyp);
	gslinewidth(linwid);
/*
.....Restore active cutter structure
*/
	cutdef[Svp] = cutdef_save;
	Scutter_nview = nview;
	if (nview != -1)
	{
		UU_LIST_EMPTY(&cutdef[0].view.npt);
		UU_LIST_EMPTY(&cutdef[0].view.gpt);
		UU_LIST_EMPTY(&cutdef[0].view.gnorm);
		uu_list_push_list(&cutdef[0].view.npt,&list_npt);
		uu_list_push_list(&cutdef[0].view.gpt,&list_gpt);
		uu_list_push_list(&cutdef[0].view.gnorm,&list_gnorm);
		uu_list_free(&list_npt);
		uu_list_free(&list_gpt);
		uu_list_free(&list_gnorm);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_box_init(box)
**			Initializes a bounding box.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          box      = Initialized bounding box.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_box_init(box)
UN_mot_vpbox_struc *box;
{
	box->ll[0] = 1000000; box->ur[0] = -1000000;
	box->ll[1] = 1000000; box->ur[1] = -1000000;
	box->ll[2] = 1000000; box->ur[2] = -1000000;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_box_add(box,pt)
**			Expands a bounding box by a single point.
**    PARAMETERS   
**       INPUT  : 
**          box      = Bounding box to expand.
**				pt       = Point used to expand bounding box.
**       OUTPUT :
**          box      = Updated bounding box.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_box_add(box,pt)
UM_coord pt;
UN_mot_vpbox_struc *box;
{
	if (pt[0] < box->ll[0]) box->ll[0] = pt[0];
	if (pt[1] < box->ll[1]) box->ll[1] = pt[1];
	if (pt[2] < box->ll[2]) box->ll[2] = pt[2];
	if (pt[0] > box->ur[0]) box->ur[0] = pt[0];
	if (pt[1] > box->ur[1]) box->ur[1] = pt[1];
	if (pt[2] > box->ur[2]) box->ur[2] = pt[2];
}

/*********************************************************************
**    I_FUNCTION     : S_cutter_recalculate(cpt,blade,cutseg,movfl,recalc,
**		                                      redisp)
**			Determines if the cutter has to be recalculated.
**    PARAMETERS   
**       INPUT  : 
**          cpt      = Cutter position and tool axis.
**          blade    = Blade direction structure.
** 	      cutseg   = Active cutter definition.
**				movfl    = 1 = Display moving cutter.
**				           2 = Display moving part.
**				recalc   = UU_TRUE = recalculate cutter shape.  (0) = Cutter,
**				           (1) = Shank, (2) = Holder.
** 	      redisp   = UU_TRUE = Redraw cutter.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_cutter_recalculate(cpt,blade,cutseg,movfl,recalc,redisp)
UU_REAL cpt[];
UN_motseg_blade *blade;
UN_motseg_cutter *cutseg;
int movfl;
UU_LOGICAL recalc[3],*redisp;
{
	int i,vp,vmode;
	UU_REAL vscale;
	UM_vector vpn,vup;
/*
.....Initialize routine
*/
	ncl_motvwp(&vp,vpn,vup,&vscale,&vmode);
	recalc[0] = recalc[1] = recalc[2] = UU_FALSE;
	*redisp = UU_FALSE;
/*
.....Determine if cutter needs to be recalculated
........Cutter parameters
*/
	if (cutseg->cutsym->geo->type == 1)
	{
		for (i=0;i<8;i++)
		{
			if (cutseg->cutter->parms[i] != cutdef[vp].cutr[i])
				recalc[0] = UU_TRUE;
		}
		if (vscale != cutdef[vp].view.scale) recalc[0] = UU_TRUE;
		if (cutdef[vp].cutfl[0] != 1) recalc[0] = UU_TRUE;
	}
/*
........Shank parameters
*/
	if (cutseg->shank->geo->type == 1)
	{
		for (i=0;i<4;i++)
		{
			if (cutseg->shank->parms[i] != cutdef[vp].cutr[i+13])
				recalc[1] = UU_TRUE;
		}
	}
/*
........Holder parameters
*/
	if (cutseg->holder->geo->type == 1)
	{
		for (i=0;i<4;i++)
		{
			if (cutseg->holder->parms[i] != cutdef[vp].cutr[i+9])
				recalc[2] = UU_TRUE;
		}
	}
/*
.....Determine if cutter needs to be redisplayed
*/
	*redisp =  recalc[0] || recalc[1] || recalc[2];
	if (!*redisp)
	{
/*
........Moving cutter flag
*/
		if (movfl != cutdef[vp].mov) *redisp = UU_TRUE;
/*
........Check Attributes
*/
		if (cutseg->cutsym->geo->type != cutdef[vp].cutfl[0] ||
			cutseg->cattr->color[0] != cutdef[vp].color[0] ||
			cutseg->cattr->segfl != cutdef[vp].segfl ||
			cutseg->cattr->mov != cutdef[vp].mov ||
			cutseg->cattr->shaded[0] != cutdef[vp].shaded[0] ||
			cutseg->cattr->trans[0] != cutdef[vp].trans[0]) *redisp = UU_TRUE;
		if (cutseg->shank->geo->type != cutdef[vp].cutfl[1] ||
			cutseg->cattr->color[1] != cutdef[vp].color[1] ||
			cutseg->cattr->shaded[1] != cutdef[vp].shaded[1] ||
			cutseg->cattr->trans[1] != cutdef[vp].trans[1]) *redisp = UU_TRUE;
		if (cutseg->holder->geo->type != cutdef[vp].cutfl[2] ||
			cutseg->cattr->color[2] != cutdef[vp].color[2] ||
			cutseg->cattr->shaded[2] != cutdef[vp].shaded[2] ||
			cutseg->cattr->trans[2] != cutdef[vp].trans[2]) *redisp = UU_TRUE;
/*
.......Symbols
*/
		if (strcmp(cutseg->cutsym->geo->symbol,cutdef[vp].symbol[0]) != 0 ||
			strcmp(cutseg->shank->geo->symbol,cutdef[vp].symbol[1]) != 0 ||
			strcmp(cutseg->holder->geo->symbol,cutdef[vp].symbol[2]) != 0)
				*redisp = UU_TRUE;
/*
........Static cutter
*/
		if (!cutdef[vp].seguse && !*redisp)
		{
			for (i=0; i<3; i++)
			{
				if (cpt[i+3] != cutdef[vp].tlax[i]) *redisp = UU_TRUE;
			}
			if (!um_vcparall(vpn,cutdef[vp].view.vpn)) *redisp = UU_TRUE;
		}
/*
........Blade cutter
*/
		if (!*redisp && ncl_cutter_is_blade())
		{
			for (i=0;i<3;i++)
			{
				if (blade->tfwd[i] != cutdef[vp].vfd[i]) *redisp = UU_TRUE;
			}
		}
/*
........Shank
*/
		if (!*redisp && cutseg->shank->geo->type != 0)
		{
			for (i=0;i<4;i++)
			{
				if (cutseg->shank->parms[i] != cutdef[vp].cutr[i+13])
					*redisp = UU_TRUE;
			}
		}
/*
........Holder
*/
		if (!*redisp && cutseg->holder->geo->type != 0)
		{
			for (i=0;i<4;i++)
			{
				if (cutseg->holder->parms[i] != cutdef[vp].cutr[i+9])
					*redisp = UU_TRUE;
			}
		}
	}
/*
.....If shank or holder rely on top diameter
.....then cutter has to be recalculated
*/
	if (recalc[1] && cutseg->shank->parms[0] == 0.)
	{
		if (cutseg->cutsym->geo->type == 1) recalc[0] = UU_TRUE;
	}
	if (recalc[2] && cutseg->holder->parms[0] == 0.)
	{
		if (cutseg->cutsym->geo->type == 1) recalc[0] = UU_TRUE;
		if (cutseg->shank->geo->type == 1) recalc[1] = UU_TRUE;
	}
/*
.....End of routine
.....Save flags for next view
*/
	Srecalc[0] = recalc[0]; Srecalc[1] = recalc[1]; Srecalc[2] = recalc[2];
	Sredisp = *redisp;
	return;
}

/*********************************************************************
**    I_FUNCTION     : S_bounding_box(cutter,movfl)
**			Calculates the 3-D based bounding box of the cutter, shank,
**       and holder.  Also calculates the actual display of the cutter
**       parts when they are specified using anything other than a
**       symbol.
**    PARAMETERS   
**       INPUT  : 
** 	      cutseg   = Active cutter definition.
**          cpt      = Cutter position and tool axis.
**          blade    = Blade direction structure.
**       OUTPUT :
**          box      = 3-D bounding box.
**          sofs     = Calculated shank offsets.
**          hofs     = Calculated holder offsets.
**    RETURNS      : UU_TRUE if cutter has to be recalculated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_bounding_box(cutseg,blade,cpt,box,sofs,hofs)
UN_motseg_cutter *cutseg;
UN_motseg_blade *blade;
UU_REAL cpt[];
UN_mot_vpbox_struc *box;
UU_REAL sofs[],hofs[];
{
	int i,j,ipt,mxj,npts,nface,ctype,*vnpt;
	UU_LOGICAL ifl[3];
	UU_REAL ofs[3];
	UM_coord tend;
	UM_coord *vgpt;
	UM_vector tax;
/*
.....Initialize cutter display storage
*/
	cutdef[Svp].view.ngeo = 0;
	if (Svp > Scutter_nview)
	{
		for (i=Scutter_nview+1;i<=Svp;i++)
		{
			uu_list_init(&cutdef[i].view.npt,sizeof(int),100,50);
			uu_list_init(&cutdef[i].view.gpt,sizeof(Gwpoint3),500,100);
			uu_list_init(&cutdef[i].view.gnorm,sizeof(Gwpoint3),500,100);
		}
		Scutter_nview = Svp;
	}
	else
	{
		UU_LIST_EMPTY(&cutdef[Svp].view.npt);
		UU_LIST_EMPTY(&cutdef[Svp].view.gpt);
		UU_LIST_EMPTY(&cutdef[Svp].view.gnorm);
	}
/*
.....Determine cutter type
*/
	ctype = 1;
	if (cutseg->cutter->parms[8] > 10) ctype = 2;
/*
.....Save current bounding box
*/
	um_vctovc(cutdef[Svp].view.box.ll,box->ll);
	um_vctovc(cutdef[Svp].view.box.ur,box->ur);
/*
.....Initialize bounding box
*/
	ncl_cutter_box_init(&cutdef[Svp].view.box);
/*
.....Initialize offsets
*/
	npts = 0;
	nface = 0;
	ofs[0] = ofs[1] = ofs[2] = 0.;
/*
.....Cutter parameters
*/
	if (cutseg->cutsym->geo->type != 2)
	{
		ncl_display_cutter(cutseg,cutseg->cutsym,Svp,cutseg->cattr->color[0],
			Sshaded[0],cutseg->cattr->trans[0],&npts,ofs,&nface,1);
	}
/*
.....Cutter Symbol
*/
	else if (cutseg->cutsym->geo->type == 2)
	{
		S_symbol_bounds(cutseg,cutseg->cutsym,blade,cpt,ofs);
	}
/*
.....Shank parameters
*/
	S_calc_offset(cutseg->shank,ctype,ofs);
	um_vctovc(ofs,sofs);
	if (cutseg->shank->geo->type == 1 || cutseg->shank->geo->type == 3 ||
		cutseg->shank->geo->type == 4 || cutseg->shank->geo->type == 5)
	{
		if (cutseg->shank->geo->type == 3 || cutseg->shank->geo->type == 4 ||
			cutseg->shank->geo->type == 5)
			sofs[2] = sofs[2] + cutseg->shank->parms[3];
		ncl_display_cutter(cutseg,cutseg->shank,Svp,cutseg->cattr->color[1],
			Sshaded[1],cutseg->cattr->trans[1],&npts,ofs,&nface,2);
	}
/*
.....Shank Symbol
*/
	else if (cutseg->shank->geo->type == 2)
	{
		S_symbol_bounds(cutseg,cutseg->shank,blade,cpt,ofs);
	}
/*
.....Holder parameters
*/
	if (cutseg->cutter->parms[8] > 10) ofs[2] = sofs[2];
	S_calc_offset(cutseg->holder,ctype,ofs);
	um_vctovc(ofs,hofs);
	if (cutseg->holder->geo->type == 1 || cutseg->holder->geo->type == 3 ||
		cutseg->holder->geo->type == 4 || cutseg->holder->geo->type == 5)
	{
		if (cutseg->holder->geo->type == 3 || cutseg->holder->geo->type == 4 ||
			cutseg->holder->geo->type == 5)
				hofs[2] = hofs[2] + cutseg->holder->parms[3];
		ncl_display_cutter(cutseg,cutseg->holder,Svp,cutseg->cattr->color[2],
			Sshaded[2],cutseg->cattr->trans[2],&npts,ofs,&nface,3);
	}
/*
.....Holder Symbol
*/
	else if (cutseg->holder->geo->type == 2)
	{
		S_symbol_bounds(cutseg,cutseg->holder,blade,cpt,ofs);
	}
/*
.....Display shaded cutter
*/
	if (cutdef[Svp].view.ngeo != 0)
	{
		ifl[0] = Sshaded[0];
		ifl[1] = Sshaded[1];
		ifl[2] = Sshaded[2];
		if (cutseg->shank->geo->type == 0) ifl[1] = ifl[2];
	 	if (cutdef[Svp].cutr[0] < 0) ifl[0] = UU_FALSE;
		if (cutdef[Svp].cutr[8] > 10)
		{
			ifl[0] = 0;
			if (cutseg->shank->geo->type != 5) ifl[1] = UU_FALSE;
			if (cutseg->holder->geo->type != 5) ifl[2] = UU_FALSE;
/*			um_vctovc(cutseg->shank->geo->axis,tax);*/
		}
		um_vctovc(cutdef[Svp].tlax,tax);
		if (ifl[0] || ifl[1] || ifl[2])
		{
			tend[0] = 0.; tend[1] = 0.; tend[2] = 0.;
			ncl_solid_cutter(tend,tax,nface,Svp,ifl);
		}
	}
/*
.....Calculate cutter bounding box
.....Around drawn portion of cutter
*/
	ipt = 0;
	vnpt = (int *)UU_LIST_ARRAY(&cutdef[Svp].view.npt);
	vgpt = (UM_coord *)UU_LIST_ARRAY(&cutdef[Svp].view.gpt);
	for (i=0; i<cutdef[Svp].view.ngeo; i++)
	{
		mxj = vnpt[i];
		if (mxj > MXGPT) mxj = MXGPT;
		for (j=0; j<mxj; j++)
		{
			ncl_cutter_box_add(&cutdef[Svp].view.box,vgpt[ipt]);
			ipt++;
		}
	}
	if (cutdef[Svp].view.box.ll[0] > cutdef[Svp].view.box.ur[0])
	{
		for (i=0;i<3;i++)
			cutdef[Svp].view.box.ll[i] = cutdef[Svp].view.box.ur[i] = 0.;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_symbol_bounds(cutseg,cutsym,blade,cpt,ofs)
**			Calculates the 3-D based bounding box of a cutter symbol.
**    PARAMETERS   
**       INPUT  : 
** 	      cutseg   = Active cutter definition.
** 	      cutsym   = Active cutter symbol structure.
**          blade    = Blade direction structure.
**          cpt      = Cutter position and tool axis.
**          ofs      = XYZ offsets for tool part.
**       OUTPUT :
**          ofs      = Updated XYZ offsets for next tool part.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_symbol_bounds(cutseg,cutsym,blade,cpt,ofs)
UN_motseg_cutter *cutseg;
UN_motseg_symbol *cutsym;
UN_motseg_blade *blade;
UU_REAL cpt[],ofs[];
{
	UU_REAL zpt[3],tvec[3],zhgt[3];
	UN_mot_vpbox_struc box;
/*
.....Calculate symbol position and orientation
........Moving shaded symbol
*/
	if (cutdef[Svp].seguse)
	{
		tvec[0] = 1;
		tvec[1] = 0;
		tvec[2] = 0;
		zpt[0] = ofs[2];
		zpt[1] = ofs[1];
		zpt[2] = -ofs[0];
	}
/*
........All other symbol displays
*/
	else
	{
		if (cutdef[Svp].cutr[8] > 10)
		{
			zpt[0] = ofs[0];
			zpt[1] = ofs[1];
			zpt[2] = ofs[2];
		}
		else
		{
			zpt[0] = ofs[2] * cpt[3];
			zpt[1] = ofs[2] * cpt[4];
			zpt[2] = ofs[2] * cpt[5];
		}
		um_vctovc(&cpt[3],tvec);
	}
/*
.....Calculate bounding box
*/
	ncl_move_cutsym(cutsym->geo->segno,zpt,tvec,blade,&box,zhgt,
		cutdef[Svp].seguse,-1);
	ncl_cutter_box_add(&cutdef[Svp].view.box,box.ll);
	ncl_cutter_box_add(&cutdef[Svp].view.box,box.ur);
	if (cutdef[Svp].cutr[8] > 10)
		ofs[1] = ofs[1] + zhgt[1];
	else
		ofs[2] = ofs[2] + zhgt[2];
}

/*********************************************************************
**    I_FUNCTION     : S_cutter_symbol(cpt,cutseg,cutsym,blade,redisp,ofs)
**			Displays the cutter symbol based on the current tool position
**			and cutter offsets.
**    PARAMETERS   
**       INPUT  : 
**          cpt      = Cutter position and tool axis.
** 	      cutseg   = Active cutter definition.
** 	      cutsym   = Active cutter symbol structure.
**          blade    = Blade direction structure.
**          redisp   = UU_TRUE = Tool needs to be redisplayed.
**          ofs      = XYZ offsets for tool part.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_cutter_symbol(cpt,cutseg,cutsym,blade,redisp,ofs)
UN_motseg_cutter *cutseg;
UN_motseg_symbol *cutsym;
UN_motseg_blade *blade;
UU_REAL cpt[],ofs[];
UU_LOGICAL redisp;
{
	UU_REAL x_vec[3],zpt[3],zhgt[3];
	UN_mot_vpbox_struc box;
/*
.....Display cutter as symbol
*/
	if (redisp)
	{
/*
........Moving cutter symbol
*/
		if (Smovfl == 1)
		{
/*
...........Cutter symbol needs to be recalculated
*/
			if (cutseg->cutter->parms[8] <= 10)
			{
				zpt[0] = ofs[0] * cpt[3];
				zpt[1] = ofs[1] * cpt[4];
				zpt[2] = ofs[2] * cpt[5];
			}
			else
			{
				zpt[0] = ofs[0];
				zpt[1] = ofs[1];
				zpt[2] = ofs[2];
			}
/*
.....If the conditions are correct for using matrix rotation
.....use the x_axis as the tool axis and zpt = <cutr[2]+cutr[9],0,0>
.....as the location point. Otherwise the symbol will not be in the
.....correct location. JLS 1/5/00
*/
			if (cutdef[Svp].seguse)
			{
				x_vec[0] = 1;
				x_vec[1] = 0;
				x_vec[2] = 0;
				zpt[0] = ofs[2];
				zpt[1] = ofs[1];
				zpt[2] = -ofs[0];
				ncl_move_cutsym(cutsym->geo->segno,zpt,x_vec,blade,&box,zhgt,
					cutdef[Svp].seguse,Serase);
			}
			else
			{
				ncl_move_cutsym(cutsym->geo->segno,zpt,&cpt[3],blade,&box,zhgt,
					cutdef[Svp].seguse,Serase);
			}
		}
/*
........Static cutter symbol display
*/
		else
		{
			if (cutseg->cutter->parms[8] <= 10)
			{
				zpt[0] = cpt[0] + ofs[2] * cpt[3];
				zpt[1] = cpt[1] + ofs[2] * cpt[4];
				zpt[2] = cpt[2] + ofs[2] * cpt[5];
			}
			else
			{
				zpt[0] = cpt[0] + ofs[0];
				zpt[1] = cpt[1] + ofs[1];
				zpt[2] = cpt[2] + ofs[2];
			}
			ncl_move_cutsym(cutsym->geo->segno,zpt,&cpt[3],blade,&box,zhgt,
				cutdef[Svp].seguse,Serase);
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_calc_offset(cutsym,type,ofs);
**			Adjusts the calculated tool offsets based on the tool part
**			and cutter type.  The values from the actual CUTTER/DISPLY
**			command are used to adjust the tool offsets.
**    PARAMETERS   
**       INPUT  : 
** 	      cutsym   = Active cutter symbol structure.
**          type     = 1 = Mill cutter, 2 = Lathe cutter.
**          ofs      = XYZ offsets for tool part.
**       OUTPUT :
**          ofs      = Updated offsets for tool part.
**    RETURNS      : UU_TRUE if cutter has to be recalculated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_calc_offset(cutsym,type,ofs)
UN_motseg_symbol *cutsym;
int type;
UU_REAL ofs[];
{
	UM_vector xaxis;
/*
.....Mill shank/holder
*/
	if (type == 1)
	{
/*
........Defined as parameters
*/
		if (cutsym->geo->type == 1)
			ofs[2] = ofs[2] + cutsym->parms[3];
/*
........Symbol or Geometry
*/
		else
			ofs[2] = ofs[2] + cutsym->parms[0];
	}
/*
.....Lathe shank/holder
*/
	else
	{
/*
........Defined as parameters
*/
		if (cutsym->geo->type == 1)
		{
			xaxis[0] = 1.; xaxis[1] = 0.; xaxis[2] = 0.;
			if (um_vcparall(cutsym->geo->axis,xaxis))
				ofs[0] = ofs[0] + cutsym->parms[3];
			else
				ofs[1] = ofs[1] + cutsym->parms[3];
		}
/*
........Symbol or Geometry
*/
		else
		{
			ofs[0] = ofs[0] + cutsym->parms[0];
			ofs[1] = ofs[1] + cutsym->parms[1];
		}
	}
}
