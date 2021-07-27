/*********************************************************************
**    NAME         :  nusf2.c
**       CONTAINS: User interface for REDEF/sf,PARAMS command.
**
**     nclu_sfparm_redef
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nusf2.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:15
*********************************************************************/

#include "usysdef.h"
#include "mgeom.h"
#include "wsgl.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "mdrel.h"
#include "mdpick.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclfc.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "mdeval.h"
#include "udforms.h"

#define FSEL 0
#define FSRF 1
#define FCOS 2
/*..... Surface parameters .....*/
#define FPU1 3
#define FPU2 4
#define FPLM 5
#define FTRM 6
/*..... Colors .....*/
#define FU0C 7
#define FU1C 8
#define FN0C 9
#define FN1C 10

#define AUTO 0
#define ALONGV 1
#define ALONGU 2

#define NUMSG 10
#define IU0 0
#define IN0 8

static int Sfrm0;
static int Scolor,Su0color,Su1color,Sn0color,Sn1color,Sswap,Srevu,Srevv,Srevn,
	Sparelm,Sdrive,Siu,Sin;

static UU_LOGICAL Shavuv,Shaven;
static Gwpoint3 Spt[10];
static Gwpoint3 Svc[10];

static UU_KEY_ID Sboxkey = NULLKEY;
static char Ssrf_lab[81];
static UM_sgeo Ssrf;
static UM_sgeo *Septr = UU_NULL;
static UU_LOGICAL Strimmed = UU_FALSE;
static struct NCL_fixed_databag Ssfdat;

static UU_LOGICAL Sactiv = UU_FALSE;

/*********************************************************************
**    S_FUNCTION     :  OnGeoSel(filedno, val, stat)
**       Routine to handle PS, DS thick parameters changes.
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
static UD_FSTAT OnGeoSel (fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int numint, pr;
	struct NCL_trimsf_rec *tsf;
	UM_PLOCREC pick;
	UU_LOGICAL cmdreject;
	UU_LOGICAL lprev = UU_FALSE;
	UU_KEY_ID lkey = NULLKEY;
	UM_int4 key4;
	UM_int2 irld;

/*
.....Take down form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;

	if (Septr == UU_NULL)
	{
		Septr = &Ssrf;
		nclu_init_sgeo (Septr);
	}
	else
		lprev = UU_TRUE;

	pr = 222;
	ud_lgeo(UU_TRUE,UD_surface);
/*	ud_lgeo(UU_TRUE,UD_ncl_netentity); */
	ua_dl_pldas(UD_DASPCKLOC,UA_NCL,pr,&pick,1,&numint,1);
	if (numint == 0) goto done;
	if (!Sactiv) goto done;

	lkey = um_get_pickkey(&(pick.pent),1);
	if (lprev && lkey == Septr->key) goto done;

	if (Shavuv || Shaven)
	{
		ud_delete_assist_segs();
		Shavuv = Shaven = UU_FALSE;
		Siu = Sin = -1;
	}

	Ssfdat.key = lkey;
	if (ncl_retrieve_data_fixed (&Ssfdat) != 0) goto done;
	if (lprev && Septr->key != Ssfdat.key) nclu_repaint (Septr,1,-1);

	Septr->key = Ssfdat.key;
	Septr->relnum = Ssfdat.rel_num;
	ncl_get_label(&Ssfdat,Septr->label);
	ncl_get_geo_color (Ssfdat.key,&Septr->color);
/*
.....Update the entities color
*/
	if (Scolor != -1)
	{
		ncl_update_geo_color(Ssfdat.key,Scolor,UU_TRUE);
		uc_display(&Ssfdat);
	}

	strcpy(Ssrf_lab,Septr->label);
	ud_update_answer(FSRF,(int *)Ssrf_lab);

	key4 = Ssfdat.key;
	gtrld1 (&key4, &irld);
	if (irld == 1)
		Sparelm = ALONGV;
	else if (irld == 2)
		Sparelm = ALONGU;
	else
		Sparelm = AUTO;
	ud_update_answer(FPLM,&Sparelm);

	Strimmed = (Ssfdat.rel_num == NCL_TRIMSF_REL);

	if (Strimmed)
	{
		tsf = (struct NCL_trimsf_rec *) &Ssfdat;
		Sdrive = tsf->drive_type;
	}
	else
	{
		Sdrive = 0;
	}
	ud_set_traverse_mask(FTRM,Strimmed);
	ud_update_answer(FTRM,&Sdrive);

/*
.....End of routine
.....Redisplay form
*/

done:
	ud_unlimit();
	ud_form_vis();
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  Srepaint()
**       Repaint existing assist vectors.
*********************************************************************/
static void Srepaint()
{
	int i,color;

	if (Shavuv)
	{
		for (i = 0; i < 8; i++)
		{
			color = 1;
			if (i == IU0) color = Su0color;
			if (i == Siu) color = Su1color;
			ud_assist_vector1(Spt[i],Svc[i],color);
		}
	}
	if (Shaven)
	{
		for (i = 8; i < 10; i++)
		{
			color = 1;
			if (i == IN0) color = Sn0color;
			if (i == Sin) color = Sn1color;
			ud_assist_vector1(Spt[i],Svc[i],color);
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : OnColor(fieldno, val, stat)
**			Color change callback.  Changes the color of all entities.
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
/*
.....Call the default method
.....This causes the answer field to be updated
*/
	ud_default_method(fieldno, val, stat);

	if (*fieldno == FCOS)
	{
		if (Septr == UU_NULL) return (UD_FLDOK);
/*
.....Change the color of all entities in this list
*/
		nclu_repaint (Septr,1,Scolor);
	}
	else if (*fieldno == FU0C || *fieldno == FU1C)
	{
		if (Shavuv)
		{
			ud_delete_assist_segs();
			Srepaint();
		}
	}
	else if (*fieldno == FN0C || *fieldno == FN1C)
	{
		if (Shaven)
		{
			ud_delete_assist_segs();
			Srepaint();
		}
	}

	return (UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  Ssave_ptvc (k,point,vector)
**       Save assist vector data.
*********************************************************************/
static void Ssave_ptvc (k,point,vector)
int k;
Gwpoint3 point,vector;
{
	Spt[k].x = point.x;
	Spt[k].y = point.y;
	Spt[k].z = point.z;
	Svc[k].x = vector.x;
	Svc[k].y = vector.y;
	Svc[k].z = vector.z;
}

/*********************************************************************
**    S_FUNCTION     :  OnPickVec(filedno, val, stat)
**       Routine to handle PS, DS thick parameters changes.
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
static UD_FSTAT OnPickVec (fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	static UM_transf tfmat;
	static UU_REAL ulim[2],vlim[2];

	UU_LOGICAL cmdreject;
	struct UM_evsrfout evsrf;
	int status,irtn,i,j,k;
	UM_real8 tol8;
	UU_REAL tol,u0,u1,v0,v1,u,v,pmi,pmj;
	UM_srf_boundary b;
	Gwpoint3 point,vector;
	UM_vector vec;

	if (Septr == UU_NULL) return (UD_FLDOK);
/*
.....Take down form
*/
	ud_form_invis();
	irtn = -1;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;

	if (Sboxkey != Ssfdat.key)
	{
		Sboxkey = Ssfdat.key;
		status = uc_retrieve_transf(Ssfdat.key, tfmat);
		if (status != UU_SUCCESS) goto done;

		u0 = v0 = 0;
		u1 = v1 = 1;
		if (Strimmed)
		{
			gettol (&tol8);
			tol = tol8;

			ncl_set_boundary_toler (tol);
			status = ncl_get_boundary (UV_BOX_LIST,&Ssfdat,&b);

			if (status == UU_SUCCESS)
			{
				u0 = b.ummx[0][0]; u1 = b.ummx[0][1];
				v0 = b.vmmx[0][0]; v1 = b.vmmx[0][1];
			}
			um_free_boundary (&b);
		}
		ulim[0] = u0; ulim[1] = u1;
		vlim[0] = v0; vlim[1] = v1;
	}

	if (*fieldno == FPU1)
	{
		if (!Shavuv)
		{
			Shavuv = UU_TRUE;
			k = 0;

			for (i = 0,pmi = 1; i < 2; i++, pmi = -pmi)
			{
				u = ulim[i];
				for (j = 0, pmj = 1; j < 2; j++, pmj = -pmj)
				{
					v = vlim[j];
					uc_evsrf(UM_FRSTDERIV,u,v,&Ssfdat,tfmat,&evsrf);
					point.x = evsrf.sp[0];
					point.y = evsrf.sp[1];
					point.z = evsrf.sp[2];
					um_unitvc(evsrf.dsdu,vec);
					vector.x = pmi*vec[0];
					vector.y = pmi*vec[1];
					vector.z = pmi*vec[2];
					if (i == 0 && j == 0)
					ud_assist_vector1(point,vector,Su0color);
					else
					ud_assist_vector1(point,vector,1);

					Ssave_ptvc (k,point,vector); k++;

					um_unitvc(evsrf.dsdv,vec);
					vector.x = pmj*vec[0];
					vector.y = pmj*vec[1];
					vector.z = pmj*vec[2];
					ud_assist_vector1(point,vector,1);

					Ssave_ptvc (k,point,vector); k++;
				}
			}
		}

		irtn = 11;
		irtn = ud_pick_assist_seg("Pick new U-direction and new UV-origin:");	
		if (irtn < 1 || irtn > 8) goto done;

		Siu = irtn - 1;

		if (irtn == 2 || irtn == 4 || irtn == 6 || irtn == 8)
			Sswap = 1;
		else
			Sswap = 0;

		if (irtn == 5 || irtn == 6 || irtn == 7 || irtn == 8)
			Srevu = 1;
		else
			Srevu = 0;

		if (irtn == 3 || irtn == 4 || irtn == 7 || irtn == 8)
			Srevv = 1;
		else
			Srevv = 0;
	}
	else if (*fieldno == FPU2)
	{
		if (!Shaven)
		{
			Shaven = UU_TRUE;
			k = 8;

			u = (ulim[0] + ulim[1])/2;
			v = (vlim[0] + vlim[1])/2;
			uc_evsrf(UM_NORM,u,v,&Ssfdat,tfmat,&evsrf);
			point.x = evsrf.sp[0];
			point.y = evsrf.sp[1];
			point.z = evsrf.sp[2];
			um_unitvc(evsrf.snorm,vec);
			vector.x = vec[0];
			vector.y = vec[1];
			vector.z = vec[2];
			ud_assist_vector1(point,vector,Sn0color);
			
			Ssave_ptvc (k,point,vector); k++;

			vector.x = -vec[0];
			vector.y = -vec[1];
			vector.z = -vec[2];
			ud_assist_vector1(point,vector,1);

			Ssave_ptvc (k,point,vector); k++;
		}

		irtn = 11;
		irtn = ud_pick_assist_seg("Pick new normal direction:");
		if (!Shavuv) irtn += 8;

		if (irtn < 9 || irtn > 10) goto done;

		Sin = irtn - 1;

		if (Sin == IN0)
			Srevn = 0;
		else
			Srevn = 1;
	}

done:
	Srepaint();
	ud_unlimit();
	ud_form_vis();
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_redef_cmd()
**			Builds and outputs the POCKET/ command.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS :
**			Saves and restores motion parameters when previewing the command.
**			The motion will remain displayed though.
**    WARNINGS     : none
*********************************************************************/
static void S_redef_cmd()
{
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL cmdreject;

	if (Septr == UU_NULL) return;

	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto fini;

	ncl_set_cmdmode(UU_TRUE);
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
	ncl_add_token(&cmdbuf, NCL_redef, NCL_nocomma);
	ncl_add_token(&cmdbuf, Ssrf_lab, NCL_comma);

	if (Sswap+Srevu+Srevv+Srevn > 0)
	{
		ncl_add_token(&cmdbuf, "PARAMS", NCL_comma);
		if (Sswap > 0)
		ncl_add_token(&cmdbuf, "SWAP", NCL_comma);
		if (Srevu+Srevv+Srevn > 0)
		{
			ncl_add_token(&cmdbuf, "REVERS", NCL_comma);
			if (Srevu > 0)
			ncl_add_token(&cmdbuf, "0", NCL_comma);
			if (Srevv > 0)
			ncl_add_token(&cmdbuf, "1", NCL_comma);
			if (Srevn > 0)
			ncl_add_token(&cmdbuf, NCL_normal, NCL_comma);
		}
	}
			
	ncl_add_token(&cmdbuf, NCL_parelm, NCL_comma);
	if (Sparelm == ALONGV)
		ncl_add_token(&cmdbuf, "0", NCL_comma);
	else if (Sparelm == ALONGU)
		ncl_add_token(&cmdbuf, "1", NCL_comma);
	else
		ncl_add_token(&cmdbuf, "-1", NCL_comma);

	if (Strimmed)
	{
		if (Sdrive == 1)
			ncl_add_token(&cmdbuf, NCL_face, NCL_nocomma);
		else
			ncl_add_token(&cmdbuf, NCL_base, NCL_nocomma);
	}

	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
	
fini:
	UD_UNMARK(cmdreject);
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_sfparm_redef()
**       Controlling routine for the pocket form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_sfparm_redef()
{
	UU_LOGICAL cmdreject;
/*
.....Set up form fields
*/
	static char traverse[] = 
		{1,1,1, 1,1,1,1, 1,1,1,1};

	static UD_METHOD methods[] = {
		OnGeoSel,UU_NULL,OnColor,
		OnPickVec,OnPickVec,UU_NULL,UU_NULL,
		OnColor,OnColor,OnColor,OnColor
		};

	static int *ans[] = {
		UU_NULL,(int *)Ssrf_lab,&Scolor,
		UU_NULL,UU_NULL,&Sparelm,&Sdrive,
		&Su0color,&Su1color,&Sn0color,&Sn1color
		};

/* The 'called' array indicates when my method is to be invoked.
    * Bitwise or of:
    *    1 - Invoked on field entry.
    *    2 - Invoked on field exit.
    *    4 - Invoked on field toggle.
*/
	static char called[] = {6,6,6, 6,6,6,6, 6,6,6,6};
/*
.....Initialize routine
*/
	if (Sactiv) goto done;
	Sactiv = UU_TRUE;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0) goto fini;
	
	Scolor = 8;
	Su0color = 9;
	Su1color = 10;
	Sn0color = 11;
	Sn1color = 12;
	Ssrf_lab[0] = '\0';
	Sswap = Srevu = Srevv = Srevn = Sparelm = Sdrive;

	Shavuv = Shaven = UU_FALSE;
	Siu = Sin = -1;
/*
.....Get the Form input
*/
	Sfrm0 = ud_form1("srfparam.frm",ans,ans,methods,called,UU_NULL,traverse);
/*
.....Erase last previewed motion
*/
	Sactiv = UU_FALSE;
	if (Sfrm0 == -1) goto done;
/*
.....Output the command
*/
	S_redef_cmd();

done:
	if (Septr != UU_NULL)
	{
		nclu_repaint (Septr,1,-1);
		Septr = UU_NULL;
	}
	Sboxkey = NULLKEY;
	ud_delete_assist_segs();

fini:
	UD_UNMARK(cmdreject);

	return;
}
