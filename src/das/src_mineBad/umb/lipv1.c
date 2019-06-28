/*********************************************************************
**    NAME         :  lipv1.c
**       CONTAINS:
**          ul_ipv_create_material()
**          ul_ipv_remove_chips()
**          ul_ipv_split_solids()
**          ul_ipv_unsplit_solids()
**          ul_ipv_section()
**          ul_ipv_pick_entity()
**          ul_ipv_pick_solid()
**          ul_ipv_pick_next()
**          ul_ipv_pick_cutn()
**          ul_ipv_locate()
**          ul_ipv_highlight_entity()
**          ul_ipv_polycurve()
**          ul_ipv_pick_point()
**          ul_ipv_polyline()
**          ul_ipv_text()
**          ul_ipv_deselect_tool()
**
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       lipv1.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       08/01/17 , 13:54:45
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
/*#include "mdclass.h"*/
/*#include "mdrel.h"*/
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdcpln.h"
#include	"mdpick.h"
#include	"misect.h"
#include	"mdeval.h"
#include "mpocket.h"
#include "view.h"
#include "mgeom.h"
#include "udforms.h"
#include "udfdata.h"

#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclfc.h"
#include "ddef.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gsegac.h"

#include "lipv.h"
#include "lipvmach.h"

void ul_ipv_highlight_entity();
void ul_ipv_unsplit_solids();
void ul_ipv_pick_next();

LtPrim LW_target;

static UM_vector Svec={0.,0.,1.};
static UM_coord Spt={0.,0.,0.};
static int Srev=0;
static UU_REAL Sdis=0.;

extern int UD_form_bypick;
extern int PKx,PKy;
static LtFont S_font = 0;
/*********************************************************************
**    E_FUNCTION     : ul_ipv_create_material()
**       Create material to be used by NCLIPV solids.
**    PARAMETERS   
**       INPUT  : 
**          colour  = Color of material.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
LtMaterial ul_ipv_create_material (colour)
LtColour colour;
{
	LtMaterial material;

	material = LiMaterialCreate(LI_MTL_TYPE_PLAIN,LI_MTL_SUBTYPE_DEFAULT);
/*	material = LiMaterialCreate(LI_MTL_TYPE_METAL,LI_MTL_SUBTYPE_METAL_BRASS);*/
	if (!material)
		return material;

	LiMaterialSetColour(material,colour);
	return material;
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_remove_chips(batch,pvs,npvs)
**       Remove unwanted solid pieces.
**    PARAMETERS   
**       INPUT  : 
**          batch  = UU_TRUE = running in batch mode, otherwise prompt
**                   for entities.
**          pvs    = Array of point-vectors to use in picking solids
**                   when in batch mode.
**          npvs   = Number of point-vectors in 'pvs'.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_remove_chips(batch,pvs,npvs)
UU_LOGICAL batch;
UU_REAL pvs[];
int npvs;
{
#define MAXPICK 50
	int i,j,nsol,*numsol,status,event,button,np;
	UU_LOGICAL irej;
	LtDouble dis1;
	LtPickedEntityList entlist;
	LtPickedEntity entpick;
	LtEntity entity1;
	LtEntityType type;
	LtSessionPrim *solptr;
	LtSessionPrim solid[MAXPICK];
	LtNat32 num;
	LtMaterial oldmat[MAXPICK];
	LtPoint orig;
	LtVector norm;
	LtBoolean edge;
	LtColour ecolor;
	LtPrim prim;
	LtData stuff;
	LW_rv_picked_entity rvent;
/*
.....Get number of solid parts in stock
*/
	if (LW_mach_mode == LW_RAPIDCUT) goto rverr;
	irej = UU_FALSE;
	status = ul_ipv_split_solids(&solptr,&num,&numsol,UU_FALSE);
	if (status == UU_FAILURE) goto nomem;
	if (num <= 1) goto nosolid;
/*
.....This call is temporary until
.....Support call 26028 (QAR 97343) is fixed
.....by MachineWorks
*/
	ipv_resize_window();
/*
.....Get solid pieces to keep
*/
	np = MAXPICK; if (batch) np = npvs;
	for (i=0;i<np;i++)
	{
		solid[i] = 0;
		if (batch)
		{
			entlist = 0;
			entlist = LiViSolidPick(LW_viewport,&pvs[i*6],&pvs[i*6+3],
				LI_MW_COORD_SYSTEM_VIEW,LI_ENTITY_TYPE_FACE,.001,2);
			event = 3; button = 1;
		}
		else
		{
			event = ul_ipv_pick_entity("Select solid to keep",LI_ENTITY_TYPE_FACE,
				&entlist,&rvent,orig,norm,&button);
		}
		if (event != 3 || button != 1)
		{
			if (event == 3 && button == 3) irej = UU_TRUE;
			break;
		}
/*
.....Determine if any entities picked
*/
		if (entlist == 0)
		{
			if (!batch)
			{
				ud_wrerr("No entities picked.",0);
				i--;
			}
		}
/*
.....Get entity picked
*/
		else
		{
			entpick = LiViPickedEntityListGetFirst(entlist);
			if (entpick == 0)
			{
				if (!batch)
				{
					ud_wrerr("No entities picked.",0);
					i--;
				}
			}
/*
.....Highlight entity
*/
			else
			{
				status = LiViPickedEntityExpand(entpick,&entity1,&type,&dis1);
				if (status != 0)
				{
					if (!batch)
					{
						ud_wrerr("No entities picked.",0);
						i--;
					}
				}
				else
				{
					solid[i] = LiViEntityGetSessionPrim(entity1,LI_ENTITY_TYPE_FACE);
					if (!batch)
					{
						oldmat[i] = 0;
						ul_ipv_highlight_entity(&oldmat[i],solid[i],
							LI_ENTITY_TYPE_SOLID,LW_material[13],&edge,ecolor);
						ul_ipv_flush();
						um_reset_pocket_graphics(UM_IPV_WINDOW);
					}
				}
			}
			LiViPickedEntityListDestroy(entlist);
		}
	}
	nsol = i;
/*
.....Unhighlight selected solids
*/
/*	for (i=0;i<nsol;i++)
		ul_ipv_highlight_entity(&oldm,solid[i],LI_ENTITY_TYPE_SOLID,oldmat[i]);*/
/*
.....Get rid of unwanted solids
*/
	if (!irej)
	{
		for (i=0;i<num;i++)
		{
			for (j=0;j<nsol;j++)
				if (solptr[i] == solid[j]) break;
			if (j==nsol)
			{
				LiSessionPrimGetProperty(solptr[i],LI_SPRIM_PROP_PRIM,&stuff);
				prim = (LtPrim)LiDataGetGenericPtr(&stuff);
				LiSessionRemovePrim(solptr[i]);
				LiPrimitiveDestroy(prim);
			}
		}
	}
/*
.....Join solids
*/
	ul_ipv_unsplit_solids(solptr,numsol,UU_FALSE);
	ul_ipv_flush();
	um_reset_pocket_graphics(UM_IPV_WINDOW);
	goto done;
/*
.....Could not allocate memory
*/
nomem:;
	if (!batch) ud_wrerr("Could not allocate memory.");
	goto done;
/*
.....No solids to split
*/
nosolid:;
	if (!batch) ud_wrerr("No solids to remove.");
	goto done;
/*
.....Not allowed in RapidCut
*/
rverr:;
	if (!batch) ud_wrerr("Cannot remove chips in a RapidCut Session.");
	goto done;
/*
.....End of routine
*/
done:;
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_split_solids(solptr,numsol,numary,fixt_flag)
**       Split defined solids into their individual pieces.
**    PARAMETERS   
**       INPUT  : 
**          fixt_flag = UU_TRUE if fixtures should be split also.
**                      Otherwise only stocks are split.
**       OUTPUT :  
**          solptr = Array of solids created.
**          numsol = Number of solids created.
**          numary = Array number of solids broken down by each stock
**                   and fixture.
**
**    RETURNS      : none
**    SIDE EFFECTS :
**          Call 'ul_ipv_unsplit_solids' to join the solids back together
**          and to free the 'solptr' and 'numary' arrays.
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_split_solids(solptr,numsol,numary,fixt_flag)
LtSessionPrim *solptr[];
int *numsol;
int *numary[];
UU_LOGICAL fixt_flag;
{
	int i,j,nt,stat,inc,nstock[2],ifl;
	LW_stock_struc *sp,*sdtmp;
	LtSessionPrim *solsp;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
	*numsol = 0;
	ul_ipv_count_stocks(nstock,UU_FALSE);
	i = nstock[0];
	nt = 1;
	if (fixt_flag)
	{
		i = i + nstock[1];
		nt = 2;
	}
	*numary = (int *)LiAllocate(i*sizeof(int));
	if (*numary == UU_NULL) goto failed;
/*
.....Get the number of parts in each solid
*/
	inc = 0;
	for (j=0;j<nt;j++)
	{
		sp = LW_stock_first[j];
		for (i=0;i<LW_nstock[j];i++)
		{
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sp,&sdtmp,&ifl,UU_FALSE);
				if (ifl == -2) break;
				(*numary)[inc] = LiViSolidGetNumParts(sdtmp->stock);
				*numsol = *numsol + (*numary)[inc];
			} while (ifl != -1);
			sp = (LW_stock_struc *)uu_lsnext(sp);
			inc++;
		}
	}
	if (*numsol <= 1) goto done;
/*
.....Split the solids
*/
	*solptr = (LtSessionPrim *)LiAllocate(*numsol*sizeof(LtSessionPrim));
	if (*solptr == UU_NULL) goto failed;
	solsp = *solptr;
	*numsol = 0;
	for (j=0;j<nt;j++)
	{
		sp = LW_stock_first[j];
		for (i=0;i<LW_nstock[j];i++)
		{
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sp,&sdtmp,&ifl,UU_FALSE);
				if (ifl == -2) break;
				LiViSolidSplit(sdtmp->stock,&solsp[*numsol]);
			} while (ifl != -1);
			sp = (LW_stock_struc *)uu_lsnext(sp);
			*numsol = *numsol + (*numary)[i];
		}
	}
	goto done;
/*
.....Could not allocate memory
*/
failed:;
	stat = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_unsplit_solids(solptr,numary,fixt_flag)
**       Join previously split solids into their original pieces.
**    PARAMETERS   
**       INPUT  : 
**          solptr = Array of solids created.
**          numary = Array number of solids broken down by each stock
**                   and fixture.
**          fixt_flag = UU_TRUE if fixtures should be split also.
**                      Otherwise only stocks are split.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**          Deallocates memory for 'solptr' and 'numary'.
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_unsplit_solids(solptr,numary,fixt_flag)
LtPrim *solptr;
int *numary;
UU_LOGICAL fixt_flag;
{
	int i,j,nt,ifl;
	LW_stock_struc *sp,*sdtmp;
/*
.....Deallocate memory
*/
	if (numary != UU_NULL) LiDeallocate(numary);
	if (solptr != UU_NULL) LiDeallocate(solptr);
/*
.....Join previously split solids
*/
	nt = 1;
	if (fixt_flag) nt = 2;
	for (j=0;j<nt;j++)
	{
		sp = LW_stock_first[j];
		for (i=0;i<LW_nstock[j];i++)
		{
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sp,&sdtmp,&ifl,UU_FALSE);
				if (ifl == -2) break;
				LiViSolidUnsplit(sdtmp->stock);
			} while (ifl != -1);
			sp = (LW_stock_struc *)uu_lsnext(sp);
		}
	}
}
/*********************************************************************
**    S_FUNCTION     : S_getvec_pt(entlist,rvent,orig,norm, vec, flag)
**       Determines if the selected face is a plane.  
**    PARAMETERS
**       INPUT  :
**          entlist  List of entities picked (Visicut).
**          rvent    Picked entity (RapidCut).
**          orig     World coordinate of user selection.
**          norm     Normal vector of selection.
**			flag:    0: from pick vector
**       OUTPUT :
**          vec: pick string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_getvec_pt(entlist, rvent, orig, norm, vec, pt)
LtPickedEntityList entlist;
LW_rv_picked_entity *rvent;
LtPoint orig;
LtVector norm;
UU_REAL *vec, *pt;
{
	int status;
	LtDouble dis1,plane[4];
	LtPickedEntity entpick;
	LtEntity Sentity[2];
	LtEntityType type;
	LtPickedSurface vsurf;
	if (LW_mach_mode == LW_VISICUT)
	{
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick == 0) goto failed;
		status = LiViPickedEntityExpand(entpick,&Sentity[0],&type,&dis1);
		if (status != 0) goto failed;
/*
......vector
*/
		status = LiViPickedEntityGetSurface(entpick,&vsurf);
		if (status == LI_STATUS_OK)
		{
			if (vsurf.type==LI_MW_SURFACE_PLANE)
			{
				plane[0] = vsurf.surface.plane.a;
				plane[1] = vsurf.surface.plane.b;
				plane[2] = vsurf.surface.plane.c;
				goto done;
			}
			else if ((vsurf.type==LI_MW_SURFACE_CONE)
				|| (vsurf.type==LI_MW_SURFACE_CYLINDER)
				|| (vsurf.type==LI_MW_SURFACE_SPHERE)
				|| (vsurf.type==LI_MW_SURFACE_SWEPT_TORUS)
				|| (vsurf.type==LI_MW_SURFACE_TORUS))
				goto failed;
		}
		LiBrFaceGetPlaneEquation(Sentity[0], plane);
	}
	else
	{
		um_vctovc(rvent->norm, plane);
		dis1 = rvent->dis;
	}
	goto done;
failed:;
	return -1;
done:;
	vec[0] = plane[0];
	vec[1] = plane[1];
	vec[2] = plane[2];
		
	pt[0] = orig[0] + norm[0]*dis1;
	pt[1] = orig[1] + norm[1]*dis1;
	pt[2] = orig[2] + norm[2]*dis1;
	return 0;
}

/*********************************************************************
**	 E_FUNCTION : OnSelipv()
**			This function Select a face. Callback from button "Plane
**				vector" and "plane origin"
**	 PARAMETERS	
**		 INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS:
*********************************************************************/
static UD_FSTAT OnSelipv(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char prompt[80];
	UU_LOGICAL markval;
	int button,event,status;
	LtPoint orig;
	LtVector norm;
	LtPickedEntityList entlist;
	LW_rv_picked_entity rvent;
	UU_REAL vec[3], pt[3];

	UD_MARK(markval,UU_FALSE);
	if (markval != 0)
	{
		*fieldno = -1;
		goto done;
	}
/*
.....Take down form
.....if Single Pick Mode
*/
	ud_dspfrm_invis(0);
/*
.....Let user select solid/face to analyze
*/
/*
	if (*fieldno==0)
	{
		strcpy(prompt,"Select Plane vector");
		flag = 0;
	}
	else
	{
		strcpy(prompt,"Select Plane origin");
		flag = 1;
	}
*/
	strcpy(prompt,"Select vector and origin");
	for(;;)
	{
/*
.....Get user selection
*/
		event = ul_ipv_pick_entity(prompt,
				LI_ENTITY_TYPE_FACE, &entlist, &rvent, orig, norm, &button);
/*
.....Exit on middle or right mouse button
*/
		if (event != 3 || button != 1)
		{
			ud_dspfrm_vis(0);
			break;
		}
/*
.....No entities selected
*/
		if (rvent.picked)
		{
			status = S_getvec_pt(entlist, &rvent, orig, norm, vec, pt);
			if (status!=0)
			{
				ud_wrerr("No entities selected.");
				continue;
			}
			else
				break;
		}
		else
		{
			ud_wrerr("No entities selected.");
			continue;
		}
	}
/*
.....reject, go directly to done
*/
	if (event == 3 && button == 3)
	{
		*fieldno = -1;
		goto done;
	}
/*
.....Get rid of the picking list
*/
	if (LW_mach_mode == LW_VISICUT)
		LiViPickedEntityListDestroy(entlist);
	ud_dspfrm_vis(0);
/*
.....update the new text string
*/
	Svec[0] = vec[0];
	Svec[1] = vec[1];
	Svec[2] = vec[2];
	Spt[0] = pt[0];
	Spt[1] = pt[1];
	Spt[2] = pt[2];
	ud_dispfrm_update_answer(0, 3, (int *)Spt);
	ud_dispfrm_update_answer(0, 1, (int *)Svec);
/*
.....Bring back the form
*/
done:;
	ud_dspfrm_vis(0);
	UD_UNMARK(markval);
	return(UD_FLDOK);
}
/*********************************************************************
**	 E_FUNCTION : OnSelncl()
**			This function Select a face. Callback from button "Plane
**				vector" and "plane origin"
**	 PARAMETERS	
**		 INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS:
*********************************************************************/
static UD_FSTAT OnSelncl(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status, ifl;
	UU_LOGICAL markval;
	UD_DASDATA dsda;
	char lstr[80];
	UU_REAL vec[3];

	UD_MARK(markval,UU_FALSE);
	if (markval != 0)
	{
		*fieldno = -1;
		goto done;
	}
/*
.....Take down form
*/
	ud_dspfrm_invis(0);
/*
.....set UD_form_bypick = 1
.....to let ud_gevt1 know we don't
.....need "BY TEXT" active
*/
	UD_form_bypick = 1;	
	while (1)
	{
		lstr[0] = '\0';
		if (*fieldno==0)
			status = ud_get_pickstr("Plane vector:", UD_DASVEC, lstr, 3, 80, 1);
		else
			status = ud_get_pickstr("Plane origin:", UD_DASCART, lstr, 3, 80, 1);
		if (UD_form_bypick==0)
			goto done;
		if (status==0)
		{
			break;
		}
	}
	ifl = 0;
	if (*fieldno==0)
		ifl = UD_UNITLESS;
	status = ud_dasin(lstr, &dsda, ifl);
/*
.....update the new text string
*/
	if (*fieldno==0)
	{
		um_ccstomcs(1, dsda.stval.stcord.coord, vec);
/*
......adjust the value
*/
		UM_cc_exttoint(vec, vec);
	}
	else
	{
		um_ccstomcs(0, dsda.stval.stcord.coord, vec);
	}
	if (*fieldno==0)
	{
		Svec[0] = vec[0];
		Svec[1] = vec[1];
		Svec[2] = vec[2];
		ud_dispfrm_update_answer(0, 1, (int *)Svec);
	}
	else
	{
		Spt[0] = vec[0];
		Spt[1] = vec[1];
		Spt[2] = vec[2];
		ud_dispfrm_update_answer(0, 3, (int *)Spt);
	}
/*
.....Bring back the form
*/
done:;
	ud_dspfrm_vis(0);
	UD_form_bypick = 0;
	UD_UNMARK(markval);
	return(UD_FLDOK);
}
/*********************************************************************
**	 E_FUNCTION : OnSel()
**			This function Select a face. Callback from button "Plane
**				vector" and "plane origin"
**	 PARAMETERS	
**		 INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS:
*********************************************************************/
static UD_FSTAT OnSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (LW_nclipv==LW_STANDALONE)
		return OnSelipv(fieldno, val, stat);
	else
		return OnSelncl(fieldno, val, stat);
}
/*********************************************************************
**	 E_FUNCTION : OnApply()
**			This function sections an NCLIPV model.
**	 PARAMETERS	
**		 INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS:
*********************************************************************/
static UD_FSTAT OnApply(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,ifl;
	UU_REAL dis;
	UM_vector vc1;
	UM_coord pt;
	LW_stock_struc *sd,*sdtmp;
	LtData stuff;
/*
.....Calculate section plane
*/
	um_unitvc(Svec,vc1);
	UM_len_exttoint(Sdis,dis);
	pt[0] = Spt[0] + dis*vc1[0];
	pt[1] = Spt[1] + dis*vc1[1];
	pt[2] = Spt[2] + dis*vc1[2];
	if (Srev == 1) um_vctmsc(vc1,-1.,vc1);
/*
.....Run plane through modsys
*/
	ncl_wcstomcs(0,pt,pt);
	ncl_wcstomcs(1,vc1,vc1);
/*
.....Visicut
*/
	if (LW_mach_mode == LW_VISICUT)
	{
		LiDataSetEnum(&stuff, LI_MW_COORD_SYSTEM_WORLD);
		LiSessionSetProperty(LW_session[LW_mach_mode],
			LI_SESS_PROP_VI_SECT_COORD_SYS,&stuff);
		for (j=0;j<2;j++)
		{
			sd = LW_stock_first[j];
			for (i=0;i<LW_nstock[j];i++)
			{
				ifl = 0;
				do
				{
					ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
					if (ifl == -2) break;
					LiViSolidSection(sdtmp->stock,pt,vc1,0,NULL,NULL);
				} while (ifl != -1);
				sd = (LW_stock_struc *)uu_lsnext(sd);
			}
		}
	}
/*
.....Rapidcut
*/
	else
	{
		LiRvStockSection(LW_session[LW_mach_mode],pt,vc1,NULL);
	}
/*
.....Flush the graphics
*/
	ul_ipv_flush();
	um_reset_pocket_graphics(UM_IPV_WINDOW);
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**	 E_FUNCTION : OnReset()
**			This function sections an NCLIPV model.
**	 PARAMETERS	
**		 INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS:
*********************************************************************/
static UD_FSTAT OnReset(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Reset section
*/
	if (LW_env != 0)
	{
		um_delv_axis_ipv();
		LiMWEnvironmentRestore(LW_env);
		ul_ipv_flush();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**	 E_FUNCTION : OnView()
**			This function enters dynamic viewing while in the sectioning
**       form.
**	 PARAMETERS	
**		 INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS:
*********************************************************************/
static UD_FSTAT OnView(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Take down form
*/
	ud_form_invis();
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
	ud_form_vis();
	return(UD_FLDOK);
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_section()
**			This function controls the IPV Section form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Initializes NCL502 variables.
**	 WARNINGS:
*********************************************************************/
void ul_ipv_section()
{
	int status;
	int *ans[9];	/* default answers/answers for form */
	UU_LOGICAL cmdreject,imark;
	static UD_METHOD methods[] = {OnSel, UU_NULL, OnSel, UU_NULL,
											UU_NULL, UU_NULL,OnApply,OnView,OnReset};
	static char called[]       = {6,6,6,6,6,6,6,6,6};
/*
.....Make sure NCLIPV is active
*/
	imark = UU_FALSE;
	if (!LW_active)
	{
		ud_wrerr("NCLIPV must be active for part sectioning.");
		goto done;
	}
/*
.....Save the current model
*/
	um_delv_axis_ipv();
	LW_env = LiMWEnvironmentSave(LW_session[LW_mach_mode]);
/*
.....Fields 0, 2 pushbuttons
*/
	ans[0] = ans[2] = UU_NULL;
/*
.....Field 1 Plane vector
*/
	UM_cc_exttoint(Svec,Svec);
	ans[1] = (int *)Svec;
/*
.....Field 3 is Origin
*/
	ans[3] = (int *)Spt;
/*
.....Field 2 is Distance along vector
*/
	ans[4] = (int *)&Sdis;
/*
.....Field 2 is Reverse vector
*/
	ans[5] = (int *)&Srev;
/*
.....Fields 6,7,8 are pushbuttons
*/
	ans[6] = ans[7] = ans[8] = UU_NULL;
/*
.....Get the Form input
*/
	imark = UU_TRUE;
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
		status = ud_form1("ipvsect.frm", ans, ans, methods, called, UU_NULL, UU_NULL);
		if (status==-1)
		{
			if(LW_env != 0)
			{
				um_delv_axis_ipv();
				LiMWEnvironmentRestore(LW_env);
			}
			goto done;
		}
	}	
	else
	{
		imark = UU_FALSE;
		if(LW_env != 0)
			LiMWEnvironmentRestore(LW_env);
		goto done;
	}
/*
.....Get rid of saved model
*/
	LiMWEnvironmentDestroy(LW_env);
	LW_env = 0;
done:;
	ul_ipv_flush();
	um_reset_pocket_graphics(UM_IPV_WINDOW);
	if (imark)
	{
		UD_UNMARK(cmdreject);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_pick_entity(prompt,type,entlist,rvent,orig,
**                                         norm,button)
**       Allows the user to select a face/edge/vertex from the NCLIPV
**       model.
**    PARAMETERS
**       INPUT  :
**          prompt     Prompt to display.
**          type       Type of entity to select.
**       OUTPUT :
**          entity     List of entities selected for Visicut Session.
**          rvent      Picked entity structure for RapidCut Session.
**          orig       Location of pick in world coordinates.
**          norm       View normal.
**          button     Mouse button which ended input.
**    RETURNS      : Event that ended picking.  Normally 3 (Mouse button).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_pick_entity(prompt,type,entlist,rvent,orig,norm,button)
char *prompt;
LtEntityType type;
LtPickedEntityList *entlist;
LtDoublePoint orig;
LtDoubleVector norm;
int *button;
LW_rv_picked_entity *rvent;
{
	int cursor,event,xy[2];
/*
.....Initialize routine
*/
	cursor = 0;
/*
.....Let user select solid/face to analyze
*/
	event = ug_pawaitloc(prompt,cursor,button,&xy[0],&xy[1]);
/*
.....Exit on middle or right mouse button
*/
	if (event == 3 && *button == 1)
	{
/*
.....Convert DC to WC
*/
		LiDrawableProjectImageToWorld(LW_drawable,LW_viewport,xy,orig,norm);
/*		printf("World = %f,%f,%f   %f,%f,%f\n",orig[0],orig[1],orig[2],norm[0],norm[1],norm[2]);*/
/*
.....Select the Solid
*/
		if (LW_mach_mode == LW_VISICUT)
		{
			*entlist = 0;
			*entlist = LiViSolidPick(LW_viewport,orig,norm,LI_MW_COORD_SYSTEM_VIEW,
				type,.001,2);
			if (*entlist == 0) rvent->picked = UU_FALSE;
			else rvent->picked = UU_TRUE;
		}
		else
		{
			ul_ipv_pick_next(orig,norm,rvent);
		}
	}
/*
.....End of routine
*/
	return(event);
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_pick_point(entlist,rvent,orig,norm,pt)
**       Converts a picked entity to a picked location on the entity.
**    PARAMETERS
**       INPUT  :
**          entlist    List of entities selected for Visicut Session.
**          rvent      Picked entity structure for RapidCut Session.
**          orig       Location of pick in world coordinates.
**          norm       View normal.
**       OUTPUT :
**          pt         Location on picked entity.
**    RETURNS      : UU_FAILURE if the pick location cannot be resolved.
**                   UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_pick_point(entlist,rvent,orig,norm,pt)
LtPickedEntityList *entlist;
LtPoint orig;
LtVector norm;
LW_rv_picked_entity *rvent;
LtPoint pt;
{
	int status;
	LtDouble dis1;
	LtPickedEntity entpick;
	LtEntityType type;
	LtEntity entity;
/*
.....No entities picket
*/
	status = UU_SUCCESS;
	if (!rvent->picked) goto failed;
/*
.....Calculate user selection
*/
	if (LW_mach_mode == LW_VISICUT)
	{
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick == 0) goto failed;
		status = LiViPickedEntityExpand(entpick,&entity,&type,&dis1);
		if (status != 0) goto failed;
	}
/*
.....Get RapidCut parameters
*/
	else
	{
		dis1 = rvent->dis;
	}
/*
.....Calculate the point
*/
	pt[0] = orig[0] + norm[0]*dis1;
	pt[1] = orig[1] + norm[1]*dis1;
	pt[2] = orig[2] + norm[2]*dis1;
/*
.....End of routine
*/
done:;
	return(status);
/*
.....Failed to resolve picked location
*/
failed:;
	status = UU_FAILURE;
	goto done;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_pick_solid(prompt,prim)
**       Allows the user to select a single solid from the NCLIPV model.
**    PARAMETERS
**       INPUT  :
**          prompt     Prompt to display.
**       OUTPUT :
**          prim       Solid primitive picked.
**    RETURNS      : UU_SUCCESS if a solid was picked, UU_FAILUR otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_pick_solid(prompt,prim)
char *prompt;
LtSessionPrim *prim;
{
	int status,stat,event,button;
	LtDouble dis1;
	LtPickedEntityList entlist;
	LtPickedEntity entpick;
	LtEntity entity1;
	LtEntityType type;
	LtPoint orig;
	LtVector norm;
	LW_rv_picked_entity rvent;
/*
.....Let user select solid
*/
	status = UU_SUCCESS;
	do
	{
		event = ul_ipv_pick_entity(prompt,LI_ENTITY_TYPE_FACE,
			&entlist,&rvent,orig,norm,&button);
		if (event == 3 && button == 1)
		{
/*
.....Determine if any entities picked
*/
			if (entlist == 0)
			{
				ud_wrerr("No entities picked.");
				continue;
			}
/*
.....Get entity picked
*/
			entpick = LiViPickedEntityListGetFirst(entlist);
			if (entpick == 0)
			{
				ud_wrerr("No entities picked.");
				continue;
			}
/*
.....Get solid primitive from entity
*/
			stat = LiViPickedEntityExpand(entpick,&entity1,&type,&dis1);
			if (stat != 0)
			{
				ud_wrerr("No entities picked.");
				continue;
			}
			*prim = LiViEntityGetSessionPrim(entity1,LI_ENTITY_TYPE_FACE);
			LiViPickedEntityListDestroy(entlist);
			break;
		}
/*
.....User terminated selection
*/
		else
		{
			status = UU_FAILURE;
		}
	} while (status == UU_SUCCESS);
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_pick_next(orig,norm,rvent)
**       Picks the solid using the user supplied Point and Vector.
**       Currently only valid for RapidCut.
**    PARAMETERS
**       INPUT  :
**          orig       Location of pick in world coordinates.
**          norm       View normal.
**       OUTPUT :
**          rvent      Picked entity structure for RapidCut Session.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_pick_next(orig,norm,rvent)
LtPoint orig;
LtVector norm;
LW_rv_picked_entity *rvent;
{
	int stat,status;
	LtData data;
/*
.....Pick RapidCut solid
*/
/*	LiRvPickExecute(orig,norm,&rvent->picked);*/
	rvent->epick = LiRvPickVisual(LW_viewport,orig,norm,.001,LI_MW_PICK_SOLIDS);
	if (rvent->epick != (LtRvPick)0)
	{
		rvent->picked = UU_TRUE;
		stat = LiRvPickGetResult(rvent->epick,LI_RV_PICK_DISTANCE,&data);
		if (stat != 0) goto failed;
		rvent->dis = LiDataGetDouble(&data);
		stat = LiRvPickGetResult(rvent->epick,LI_RV_PICK_CUT_NUMBER,&data);
		if (stat == 0) rvent->cutn = LiDataGetNat32(&data);
		else rvent->cutn = 0;
		LiDataSetDoublePtr(&data,rvent->norm);
		stat = LiRvPickGetResult(rvent->epick,LI_RV_PICK_NORMAL,&data);
		if (stat != 0) um_vctovc(norm,rvent->norm);
	}
	else
		goto failed;
	return;
/*
.....Could not resolve pick
*/
failed:;
	rvent->picked = UU_FALSE;
	rvent->dis = 0.;
	return;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_pick_cutn(prompt)
**       Selects a cut face and returns the cut number that generated
**       the face.
**    PARAMETERS
**       INPUT  :
**          prompt   = Prompt to display when picking.
**       OUTPUT :
**          cutn     = Cut number that generated the face.
**    RETURNS      :
**          0 if face successfully picked with valid cut number.
**          -1 if the face does not have an associated cut number.
**          -2 if the user did not pick a face.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_pick_cutn(prompt,cutn)
char *prompt;
LtNat32 cutn;
{
	int event,button,istat,type;
	UU_LOGICAL found,idid;
	UN_mot_attr *mattr;
	UN_mot_data *mdata;
	int status;
	LtDouble dis1;
	LtPoint orig;
	LtVector norm;
	LtPickedEntityList entlist;
	LtPickedEntity entpick;
	LtEntity entity;
	LW_rv_picked_entity rvent;
/*
.....Initialize routine
*/
	status = -2;
/*
.....Let the user pick the face
*/
	do
	{
		event = ul_ipv_pick_entity(prompt,LI_ENTITY_TYPE_FACE,&entlist,&rvent,
			orig,norm,&button);
/*
.....Exit on middle or right mouse button
*/
		if (event != 3 || button != 1) goto done;
/*
.....Calculate user selection
*/
		istat = -1;
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick != 0)
			istat = LiViPickedEntityExpand(entpick,&entity,&type,&dis1);
		if (istat != 0)
			ud_wrerr("A face was not picked.");
	} while (istat != 0);
/*
.....Get the cut number of the face
*/
	found = ul_ipv_find_mdata(entity,cutn,&mattr,&mdata,UU_FALSE);
	if (!found) goto no_cut;
	status = 0;
	goto done;
/*
.....The selected entity does not have a cut number
*/
no_cut:;
	status = -1;
	goto done;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_locate(prompt,cursor,orig,norm,button)
**       Allows the user to select a screen location in the NCLIPV window.
**    PARAMETERS
**       INPUT  :
**          prompt     Prompt to display.
**          cursor     2 = Standard locate cursor, 5 = Rubberband box.
**       OUTPUT :
**          orig       Location of pick in world coordinates.
**          norm       View normal.
**          button     Mouse button which ended input.
**    RETURNS      : Event that ended picking.  Normally 3 (Mouse button).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_locate(prompt,cursor,orig,norm,button)
char *prompt;
LtDoublePoint orig;
LtDoubleVector norm;
int *button;
int cursor;
{
	static int xy[2];
	int event;
/*
.....Write to NCLIPV window when rubber banding
*/
	if (cursor == 5) uw_glset_context(UM_IPV_WINDOW,UU_FALSE);
/*
.....Let user select location
*/
	event = ug_pawaitloc(prompt,cursor,button,&xy[0],&xy[1]);
	if (cursor == 5) uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
/*
.....Exit on middle or right mouse button
*/
	if (event == 3 && *button == 1)
	{
/*
.....Convert DC to WC
*/
		LiDrawableProjectImageToWorld(LW_drawable,LW_viewport,xy,orig,norm);
/*		printf("World = %f,%f,%f   %f,%f,%f\n",orig[0],orig[1],orig[2],norm[0],norm[1],norm[2]);*/
	}
/*
.....End of routine
*/
	return(event);
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_highlight_entity(attr,entity,type,color,edge,
**                                              ecolor)
**       Highlights an NCLIPV entity.
**    PARAMETERS
**       INPUT  :
**          entity     Entity to highlight.
**          type       Type of entity.  LI_ENTITY_TYPE_FACE,
**                     LI_ENTITY_TYPE_SOLID, or LI_ENTITY_TYPE_PATCH (surface).
**          color      Highlight Color.  -1 = Unhighlight solid.
**          edge       UU_TRUE if solid edges should be displayed when
**				           unhighlighting.
**          ecolor     Color of displayed edge when unhighlighting.
**       OUTPUT :
**          attr       Returns -1 if highlighting solid or current color of
**				           highlighted face.
**          edge       Returns UU_TRUE if solid edges are currently highlighted.
**          ecolor     Returns color of edges.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_highlight_entity(attr,entity,type,color,edge,ecolor)
LtMaterial *attr;
LtEntity entity;
LtEntityType type;
LtMaterial color;
LtBoolean *edge;
LtColour ecolor;
{
	int i;
	LtAttributeClass class;
	LtData data,cdata;
	LtNat32 nfaces;
	LtFace *faces;
/*
.....Highlighting only allowed in Visicut
*/
	if (LW_mach_mode != LW_VISICUT) return;
/*
.....Highlight solid
*/
	if (type == LI_ENTITY_TYPE_SOLID)
	{
		if ((int)color == -1)
		{
			LiDataSetBoolean( &data,*edge );
			LiDataSetColour(&cdata,ecolor);
			*attr = 0;
		}
		else
		{
			LiSessionPrimGetVisualProperty(entity,LI_MW_VIS_PROP_RENDER_EDGES,&data);
			*edge = LiDataGetBoolean(&data);
			LiSessionPrimGetVisualProperty(entity,LI_MW_VIS_PROP_EDGE_COLOUR,&data);
			LiDataGetColour(&data,ecolor);
			LiDataSetBoolean( &data, TRUE );
			LiDataSetColour(&cdata,LW_highlight_value);
			*attr = (LtMaterial)-1;
		}
		LiSessionPrimSetVisualProperty(entity, LI_MW_VIS_PROP_RENDER_EDGES,&data);
		LiSessionPrimSetVisualProperty(entity, LI_MW_VIS_PROP_EDGE_COLOUR,&cdata);
	}
/*
.....Highlight face
*/
	else if (type == LI_ENTITY_TYPE_FACE)
	{
		class = LiViGetPredefinedAttributeClass(LI_VI_ATTRIB_FACE_MATERIAL);
		*attr = LiViEntityGetAttribute(entity,class);
		LiViEntitySetAttribute(entity,type,class,color);
	}
/*
.....Highlight surface
*/
	else if (type == LI_ENTITY_TYPE_PATCH)
	{
		class = LiViGetPredefinedAttributeClass(LI_VI_ATTRIB_FACE_MATERIAL);
		*attr = LiViEntityGetAttribute(entity,class);
		LiViFaceGetSurfaceFaces(entity,&nfaces,&faces);
		for (i=0;i<nfaces;i++)
		{
			LiViEntitySetAttribute(faces[i],LI_ENTITY_TYPE_FACE,class,color);
		}
		LiDeallocate((LtGenericPtr)faces);
	}
	ul_ipv_flush();
	um_reset_pocket_graphics(UM_IPV_WINDOW);
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_polycurve(prim,sprim,pts,npts)
**       Creates a polycurve in NCLIPV.
**    PARAMETERS
**       INPUT  :
**          pts        Array of points used to define polycurve.
**          npts       Number of points in polycurve.
**       OUTPUT :
**          prim       Polycurve created.
**          sprim      Session Polycurve created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_polycurve(prim,sprim,pts,npts)
LtPrim *prim;
LtSessionPrim *sprim;
LtDoublePoint *pts;
int npts;
{
	void ul_ipv_polyline();
	ul_ipv_polyline(prim,sprim,pts,npts, 1, 2);
	return;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_polyline(prim,pts,npts, colr, ln)
**       Creates a polycurve in NCLIPV.
**    PARAMETERS
**       INPUT  :
**          pts        Array of points used to define polycurve.
**          npts       Number of points in polycurve.
**			colr:		color of the polyline
**			ln: line thickness
**       OUTPUT :
**          prim       Polycurve created.
**          sprim      Session Polycurve created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_polyline(prim,sprim,pts,npts, colr, ln)
LtPrim *prim;
LtSessionPrim *sprim;
LtDoublePoint *pts;
int npts, colr;
{
	int i;
	LtData stuff;
	LtLineStyle pstyle;
	LtColour col;
/*
.....Define polycurve
*/
	LiDataSetDoublePtr(&stuff,pts[0]);
	*prim = LiPrimitiveCreatePolycurve(LI_MW_POLYCURVE_COPY_GEOMETRY,&stuff);
/*
.....Set attributes
*/
	LiPrimitiveGetRenObjProperty(*prim,LI_ROBJ_PROP_MW_POLYC_DEF_STYLE,&stuff);
	pstyle = (LtLineStyle)LiDataGetGenericPtr(&stuff);
	ul_ipv_color_set(col,colr);
	LiDataSetColour(&stuff,col);
	LiLineStyleSetProperty(pstyle,LI_MW_LINE_COLOUR,&stuff);
	LiDataSetEnum(&stuff,LI_MW_PATTERN_SOLID);
	LiLineStyleSetProperty(pstyle,LI_MW_LINE_PATTERN,&stuff);
	LiDataSetBoolean(&stuff,TRUE);
	LiLineStyleSetProperty(pstyle,LI_MW_LINE_VISIBLE,&stuff);
	LiDataSetNat32(&stuff,ln);
	LiLineStyleSetProperty(pstyle,LI_MW_LINE_THICKNESS,&stuff);
/*
	LiDataSetEnum(&stuff, LI_MW_POSITION_MODE_3D_FRONT);
	LiMWViewportSetSessPrimProperty(LW_viewport, *prim,
		LI_VPSP_PROP_MW_ROBJ_POSN_MODE, &stuff);
*/
/*
.....Create polycurve segments
*/
	for (i=1;i<npts;i++)
	{
		LiDataSetDoublePtr(&stuff,pts[i]);
		LiPolycurveAddLine(*prim,&stuff);
	}
	LiPolycurveEnd(*prim);
	*sprim = LiSessionAddPrim(LW_session[LW_mach_mode],*prim);

	LiDataSetEnum(&stuff, LI_MW_POSITION_MODE_3D_FRONT);
	LiMWViewportSetSessPrimProperty(LW_viewport, *sprim,
		LI_VPSP_PROP_MW_ROBJ_POSN_MODE, &stuff);
}
/*********************************************************************
**    E_FUNCTION     :  ul_ipv_text(prim,sprim,text,pos,colr)
**       Create graphics text in NCLIPV.
**    PARAMETERS
**       INPUT  :
**			colr:		color of the text
**			text: text to be displayed
**			pos: text position
**			flag: using fix screen position
**       OUTPUT :
**          prim       Text created.
**          sprim      Session Text created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_text(prim, sprim, text, pos, colr, flag)
LtPrim *prim;
LtSessionPrim *sprim;
Gwpoint3 *pos;
char *text;
int colr, flag;
{
	LtColour col;
	LtMaterial material;
	LtData data;
	LtTextStyle textstyle, textstyle2;
#if UU_COMP==UU_WIN2K
	LOGFONT logfont;
#endif
	LtFloat posx, posy;
	int xy[2];
	LtDoublePoint wc;
	Gfloat ndc[2];
	LtDoublePoint pospt;
/*
.....Define text
*/
	*prim = LiPrimitiveCreateText((LtGenericPtr)text, LI_TEXT_ENCODE_ASCII);
	textstyle=LiTextStyleCreate();
	LiDataSetEnum(&data,LI_TEXT_TYPE_3D);
	LiTextStyleSetProperty(textstyle,LI_TEXT_STYLE_PROP_TYPE,&data);	
	LiDataSetGenericPtr(&data,(LtGenericPtr)textstyle);
	LiPrimitiveSetRenObjProperty(*prim,LI_ROBJ_PROP_MW_TEXT_STYLE,&data);

#if UU_COMP==UU_WIN2K
	logfont.lfHeight = 12;
	logfont.lfWidth = 5;
	logfont.lfEscapement = 0;
	logfont.lfOrientation = 0;
	logfont.lfItalic = FALSE;
	logfont.lfUnderline = FALSE;
	logfont.lfStrikeOut = FALSE;
	logfont.lfCharSet = ANSI_CHARSET;
	logfont.lfOutPrecision = OUT_DEFAULT_PRECIS;
	logfont.lfClipPrecision = OUT_DEFAULT_PRECIS;
	logfont.lfQuality = DEFAULT_QUALITY;
	logfont.lfPitchAndFamily = DEFAULT_PITCH;
	strcpy(logfont.lfFaceName, "Arial");
	if (S_font==0)
		S_font = LiFontCreateSystemFont((LtGenericPtr)&logfont);
#endif
	if (S_font!=NULL)
	{
		textstyle2=LiTextStyleCreate();
		LiDataSetGenericPtr(&data, (LtGenericPtr)S_font);
		LiTextStyleSetProperty(textstyle2,LI_TEXT_STYLE_PROP_FONT,&data);
		LiDataSetGenericPtr(&data,(LtGenericPtr)textstyle2);
		LiPrimitiveSetRenObjProperty(*prim,LI_ROBJ_PROP_MW_TEXT_STYLE,&data);
	}
	material = LiMaterialCreate(LI_MTL_TYPE_PLAIN, LI_MTL_SUBTYPE_DEFAULT);
	ul_ipv_color_set(col,colr);
	LiMaterialSetColour(material,col);
	LiPrimitiveSetMaterial(*prim, material);

	*sprim = LiSessionAddPrim(LW_session[LW_mach_mode],*prim);
 
	if (flag==1)
	{
		LiDataSetEnum(&data, LI_MW_POSITION_MODE_3D_FRONT);
		LiMWViewportSetSessPrimProperty(LW_viewport, *sprim,
			LI_VPSP_PROP_MW_ROBJ_POSN_MODE, &data);

		pospt[0] = pos->x;
		pospt[1] = pos->y;
		pospt[2] = pos->z;
		LiViRenderObjectSetPosition(*sprim, pospt);
	}
	else
	{
		wc[0] = pos->x;
		wc[1] = pos->y;
		wc[2] = pos->z;
		LiDrawableProjectWorldToImage(LW_drawable,LW_viewport, 
		  wc, xy);

		ndc[0] = ((UU_REAL)xy[0])/(UU_REAL)PKx;
		ndc[1] = ((UU_REAL)xy[1])/(UU_REAL)PKy;

		LiDataSetEnum(&data, LI_MW_POSITION_MODE_NORM_WINDOW);
		LiMWViewportSetSessPrimProperty(LW_viewport, *sprim,
			LI_VPSP_PROP_MW_ROBJ_POSN_MODE, &data);

		posx = (LtFloat)ndc[0];
		posy = (LtFloat)ndc[1];
		LiDataSetFloat(&data, posx);
		LiMWViewportSetSessPrimProperty(LW_viewport, *sprim,
			LI_VPSP_PROP_MW_ROBJ_POSN_X, &data);
		LiDataSetFloat(&data, posy);
		LiMWViewportSetSessPrimProperty(LW_viewport, *sprim,
			LI_VPSP_PROP_MW_ROBJ_POSN_Y, &data);
	}
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_deselect_tool()
**       Deselects and destroys the active tool.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_deselect_tool()
{
	int i,inc;
	LtData stuff;
	LtPrim prim;
	UN_cutter_list *cpt;
/*
.....Get rid of active tools
*/
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	for (inc=0;inc<LW_spindle_num;inc++)
	{
		if (LW_tool[inc] != (LtSessionPrim)0)
		{
/*
........Deselect tool
*/
			if (cpt[LW_act_tool[inc]].type == NCL_CUTTER_LATHE && !LW_mach_simul)
				LiViTurningToolSelect(LW_tool[inc],LW_lathe,NULL,FALSE);
			else
				LiViToolSelect(LW_tool[inc],FALSE);
/*
........Remove tool from machine
*/
			ul_ipv_tool_disassembly(inc);
/*
........Delete tool parts
*/
			if (LW_session[LW_mach_mode] != 0)
			{
				LiSessionPrimGetProperty(LW_tool[inc],LI_SPRIM_PROP_PRIM,&stuff);
				prim = (LtPrim)LiDataGetGenericPtr(&stuff);
				LiSessionRemovePrim(LW_tool[inc]);
				LiPrimitiveDestroy(prim);
				if (LW_shank[inc] != (LtSessionPrim)0)
				{
					LiSessionPrimGetProperty(LW_shank[inc],LI_SPRIM_PROP_PRIM,
						&stuff);
					prim = (LtPrim)LiDataGetGenericPtr(&stuff);
					LiSessionRemovePrim(LW_shank[inc]);
					LiPrimitiveDestroy(prim);
				}
				for (i=0;i<LW_num_holder[inc];i++)
				{
					if (LW_holder[inc][i] != (LtSessionPrim)0)
					{
						LiSessionPrimGetProperty(LW_holder[inc][i],LI_SPRIM_PROP_PRIM,
							&stuff);
						prim = (LtPrim)LiDataGetGenericPtr(&stuff);
						LiSessionRemovePrim(LW_holder[inc][i]);
						LiPrimitiveDestroy(prim);
					}
				}
			}
/*
........Mark tool parts as unused
*/
			LW_tool[inc] = (LtSessionPrim)0;
			LW_shank[inc] = (LtSessionPrim)0;
			for (i=0;i<LW_num_holder[inc];i++)
				LW_holder[inc][i] = (LtSessionPrim)0;
			LW_num_holder[inc] = 0;
		}
	}
}
