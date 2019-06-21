/*********************************************************************
**    NAME         :  m3ecomp1.c
**   NOTE - This file contains routines which are used by both NCL & IGES.
**       CONTAINS: composite curve support routines
**           umu_cv_offset_comp()
**
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ucomp1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:58
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "class.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"
#include "mfort.h"
#include "nccs.h"
#include "tzmdrel.h"
#include "misect.h"
#include "nconst.h"
#include "nclmplay.h"
#include "mdpick.h"
#include "mxxx.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclvx.h"
#include "nclupok.h"
#include "vsegbf.h"

#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "mgeom.h"

#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "mdgenent.h"
#include "vsegbf.h"

enum
{
	FLB1,
	FSEL,
	FALL,
	FSRT,
	FEND,
	FDR1,
	FDR2,
	FDIS,
	FVLB,
	FVSL,
	FCON,
	FEXS,
	FLB2,
	FVEW,
	FPVW,
	FAPP,
};

#define XLARGE 0
#define XSMALL 1
#define YLARGE 2
#define YSMALL 3
#define ZLARGE 4
#define ZSMALL 5
#define VECTOR 6

#define LINEAR 0
#define SMOOTH 1
#define CHAMFR 2

#define MXLAB NCL_MAX_LABEL_AND_SUBSCRIPT+1

static UU_LOGICAL Sapply,Scanon;
static UU_KEY_ID Skey = NULLKEY, Spreview_key = NULLKEY;
static int Sstart,Tstart,Send,Tend,Sdir,Tdir,Sall,Scon,Tcon,Sexist;
static int Scolor = -1,Soffind;
static char Slabel1[MXLAB],Slabel2[MXLAB],*Stlabel={"@Preview"},
				Tlabel1[MXLAB],Tlabel2[MXLAB],Sdist_str[65]="0.0",
				Tdist_str[65]="0.0",Svec_lab[MXLAB],Tvec_lab[MXLAB];

static UD_FSTAT OnAction(),OnSelect(),OnEdit(),OnOffset(),OnSelVec();
static void S_reset_values(),S_delete_preview(),S_process_vector_label();

/*********************************************************************
**   E_FUNCTION : void umu_cv_offset_comp()
**		 Interface for offsetting composite curve components
**   PARAMETERS   
**      INPUT  : none
**      OUTPUT : none
**   RETURNS      : none
**   SIDE EFFECTS : none
**   WARNINGS     : none
*********************************************************************/
void umu_cv_offset_comp()
{
	int nc,status;
	UU_LOGICAL cmdreject;
	NCL_cmdbuf cmdbuf1;
/*
.....Set up form fields
*/
	static int *ans[FAPP+1];
	static char traverse[] = {1,1, 1,1,1, 1,1,1, 1,1, 1, 1,1, 1,1,1};
	static char called[] =  {6,6, 6,6,6, 6,6,6, 6,6, 6, 6,6, 6,6,6};
	static char display[] = {1,1, 1,1,1, 1,1,1, 1,1, 1, 1,1, 1,1,1};
	static UD_METHOD methods[] = {OnEdit,OnSelect,OnAction,OnEdit,
		OnEdit,OnOffset,OnEdit,OnEdit,OnEdit,OnSelVec,OnEdit,OnAction,OnEdit,
		OnAction,OnAction,OnAction};
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0) goto done;
/*
.....Initialize parameters
*/
	S_reset_values();

	ans[FLB1] = (int *)&Slabel1;
	ans[FSEL] =	UU_NULL;
	ans[FALL] = &Sall;
	ans[FSRT] = &Sstart;
	ans[FEND] = &Send;
	ans[FDR1] = UU_NULL;
	ans[FDR2] = &Sdir;
	ans[FDIS] = (int *)&Sdist_str;
	ans[FVLB] = (int *)&Svec_lab;
	ans[FVSL] = UU_NULL;
	ans[FCON] = &Scon;
	ans[FEXS] = (int *)&Sexist;
	ans[FLB2] = (int *)&Slabel2;
	ans[FVEW] =	UU_NULL;
	ans[FPVW] =	UU_NULL;
	ans[FAPP] =	UU_NULL;
/*
.....Set traverse flags
*/
	if (Sall == 1)
	{
		traverse[FSRT] = 0;
		traverse[FEND] = 0;
	}
	else
	{
		traverse[FSRT] = 1;
		traverse[FEND] = 1;
	}
	if (Sdir < VECTOR)
	{
		traverse[FVLB] = 0;
		traverse[FVSL] = 0;
	}
	else
	{
		traverse[FVLB] = 1;
		traverse[FVSL] = 1;
	}
	traverse[FLB2] = !Sexist;
/*
.....Get the form input
*/
	status = ud_form1("offcomp.frm",ans,ans,methods,called,display,traverse);
	S_delete_preview();
	if (status == -1) goto done;
	if (!Sapply) S_build_cmd(UU_FALSE);
/*
.....Output CANON/OFF command
.....if CANON/ON output previously
*/
done:;
	if (Scanon)
	{
		ncl_init_cmdbuf(&cmdbuf1);
		ncl_add_token(&cmdbuf1,NCL_canon1,NCL_nocomma);
		ncl_add_token(&cmdbuf1,NCL_off,NCL_nocomma);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf1);
		ncl_call(&cmdbuf1);
	}
	UD_UNMARK(cmdreject);
}

/*********************************************************************
**    I_FUNCTION     : S_build_cmd(previw)
**    Build and output curve offset command(s).
**    PARAMETERS
**       INPUT  : 
**          previw - UU_TRUE  : output temporary command
**                 - UU_FALSE : output command
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_build_cmd(previw)
UU_LOGICAL previw;
{
	UM_int2 idx,ifl41;
	NCL_cmdbuf cmdbuf,cmdbuf1;
	UU_LOGICAL cmdreject;
	int i = 1,nc;
	char buf[MXLAB];
/*
.....Trap command reject
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto fini;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
	ifl41 = 1;
/*
.....Add label
*/
	if (previw)
	{
		sprintf(buf,"*%s=",Stlabel);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
	}
	else
	{
		if (Sexist)
		{
			idx = 41; getifl(&idx,&ifl41);
			if (ifl41 == 0)
			{
				ncl_init_cmdbuf(&cmdbuf1);
				ncl_add_token(&cmdbuf1,NCL_canon1,NCL_nocomma);
				ncl_add_token(&cmdbuf1,NCL_on,NCL_nocomma);
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf1);
				ncl_call(&cmdbuf1);
				Scanon = UU_TRUE;
			}
		}
		nc = strlen(Slabel2);
		ul_strip_blanks(Slabel2,&nc);
		if (nc > 0)
		{
			sprintf(buf,"%s=",Slabel2);
			ncl_add_token(&cmdbuf,buf,NCL_nocomma);
		}
	}
/*
.....Build command
*/
	ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_offset, NCL_comma);
/*
.....Curve
*/
	nc = strlen(Slabel1);
	ul_strip_blanks(Slabel1,&nc);
	if (nc <= 0)
	{
		ud_wrerr("No curve selected");
		goto fini;
	}
	sprintf(buf,"%s",Slabel1);
	ncl_add_token(&cmdbuf,buf,NCL_comma);
/*
.....Component range
*/
	if (Sall == 1)
		ncl_add_token(&cmdbuf, NCL_all, NCL_comma);
	else
	{
		sprintf(buf,"%d",Sstart);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
		sprintf(buf,"%d",Send);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
	}
/*
.....Direction modifier
*/
	if (Sdir == XLARGE)
		ncl_add_token(&cmdbuf,NCL_xlarge,NCL_comma);
	else if (Sdir == XSMALL)
		ncl_add_token(&cmdbuf,NCL_xsmall,NCL_comma);
	else if (Sdir == YLARGE)
		ncl_add_token(&cmdbuf,NCL_ylarge,NCL_comma);
	else if (Sdir == YSMALL)
		ncl_add_token(&cmdbuf,NCL_ysmall,NCL_comma);
	else if (Sdir == ZLARGE)
		ncl_add_token(&cmdbuf,NCL_zlarge,NCL_comma);
	else if (Sdir == ZSMALL)
		ncl_add_token(&cmdbuf,NCL_zsmall,NCL_comma);
/*
.....Added option to determine direction modifier based on
.....given vector - ASF 11/13/13.
*/
	else
	{
		S_process_vector_label(Svec_lab);
		ncl_add_token(&cmdbuf,Svec_lab,NCL_comma);
	}
/*
.....Add distance
*/
	ncl_add_token(&cmdbuf,Sdist_str,NCL_comma);
/*
.....Connection type
*/
	if (Scon == LINEAR)
		ncl_add_token(&cmdbuf,"LINEAR",NCL_nocomma);
	else if (Scon == SMOOTH)
		ncl_add_token(&cmdbuf,"SMOOTH",NCL_nocomma);
	else if (Scon == CHAMFR)
		ncl_add_token(&cmdbuf,"CHAMFR",NCL_nocomma);

	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);

fini:
	UD_UNMARK(cmdreject);

	return (UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION     : S_process_vector_label(label)
**       Check if vector is given only by components.  If so, convert
**       the given vector components to a nested vector command.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_process_vector_label(label)
char *label;
{
	int i,len;
	UU_LOGICAL fix = UU_FALSE;
	char tlabel[MXLAB];

	if (label[0] == '(') return;
	for (i=0;i<MXLAB && !fix;i++) if (label[i] == ',') fix = UU_TRUE;
	if (fix)
	{
		len = strlen(label);
		if (len >= MXLAB-5) label[MXLAB-5] = '\0';
		sprintf(tlabel,"(VE/%s)",label);
		strcpy(label,tlabel);
	}
}
/*********************************************************************
**    I_FUNCTION     : S_reset_values()
**       Resets static variables.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_reset_values()
{
	strcpy(Slabel1,"\0");
	strcpy(Slabel2,"\0");
	strcpy(Tlabel1,"\0");
	strcpy(Tlabel2,"\0");
	Sstart = Send = Tstart = Tend = 1;
	Skey = NULLKEY;
	Sall = 0;
	Sexist = 0;
	Scanon = UU_FALSE;
	Sapply = UU_FALSE;
}
/*********************************************************************
**    I_FUNCTION     : S_delete_preview()
**       Deletes the temporary geometry created by the Preview button.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_delete_preview()
{
	int nc,i;
	char label[MXLAB];
	UU_KEY_ID key;
	UM_f77_str f77_str;

	sprintf(label,"%s",Stlabel);
	ul_to_upper(&label);
	nc = strlen(label);
	UM_init_f77_str(f77_str,label,64);
	for (i=nc;i<64;i++) label[i] = ' ';
	getkey(UM_addr_of_f77_str(f77_str),&key);
/*
.....Delete preview geometry
*/
	if (key != 0)
		dlgeom(&key);
}
/*********************************************************************
**    E_FUNCTION   : UD_FSTAT OnSelect(fieldno, val, stat)
**			Select the curve for the form and fill values.
**    PARAMETERS
**       INPUT  : fieldno = Form field which initiated this call.
**                val     = Current field value.
**                stat    = Not used.
**       OUTPUT :
**                none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSelect(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
   int i,numint,irtn,status,temp,save_ptype,sold,eold,limit_key;
	struct NCL_fixed_databag crv;
	struct UM_compcrv_rec *eptr;
	struct UM_evcrvout evout;
	UD_PLOCREC pick;
	UM_PLOCREC pick1;
	UM_PICKENT pent;
   UU_LOGICAL cmdreject,found;
	UU_KEY_ID key,key1;
	UM_coord picked,npt,npt2;
	UM_vector nve,nve2,vec,assist,vc,ve0;
	Gwpoint3 point, vector;
	UM_transf tfmat;
	UM_vector vpnorm;
	UU_REAL adrv,bdrv,anorm,bnorm,LR, uu = 0.;
	uv_segbuff(udata);
	UU_REAL t0;

   if (*fieldno != FSEL) return (UD_FLDOK);
	S_delete_preview();
	limit_key = -1;
/*
.....Take down form
*/
   ud_form_invis();
/*
.....Trap Reject Op
*/
   UD_MARK (cmdreject, UU_TRUE);
   if (cmdreject != 0) 
	{
		if (limit_key!=-1)
			ud_limit_entity(UU_FALSE, limit_key);
		ud_setpick_type(save_ptype);
		goto done;
	}
/*
.....Set the appropriate selection mask
*/
redo1:
   ud_lgeo(UU_TRUE,UD_compcrv);
/*
.....Get the geometry selection
*/
	ua_dl_pldas(UD_DASPCKLOC,UA_NCL,713,&pick1,1,&numint,1);
	if (numint > 0)
	{
/*
.....Get curve data
*/
		crv.key = um_get_pickkey(&pick1.pent, 1);
		if (ncl_retrieve_data_fixed(&crv) != 0) goto done;
		eptr = (struct UM_compcrv_rec *) &crv;
		ncl_get_label(&crv,Slabel1);
		ul_to_upper(Slabel1); strcpy(Tlabel1,Slabel1);
		ud_update_answer(FLB1,&Slabel1);
		if (Sexist)
		{
			strcpy(Slabel2,Slabel1);
			ud_update_answer(FLB2,Slabel2);
		}
		Sapply = UU_FALSE;
		key1 = um_get_pickkey(&pick1.pent, 2);
		t0 = eptr->t0;
		for (Soffind=0;Soffind<eptr->no_cid;Soffind++)
			if (eptr->cid[Soffind].endparam > t0) break;
/*
.....Look for component
*/
		found = UU_FALSE;
		for (i = 0; i < eptr->no_cid && !found; i++)
			found = (key1 == eptr->cid[i].crvid);
		if (found) Sstart = Send = Tstart = Tend = i - Soffind;
/*		Sall = 0;
		ud_update_answer(FALL,&Sall);*/
		if (!Sall)
		{
			ud_update_answer(FSRT,&Sstart);
			ud_update_answer(FEND,&Send);
		}
/*		ud_set_traverse_mask(FSRT,UU_TRUE);
		ud_set_traverse_mask(FEND,UU_TRUE);*/
/*
......Autofill offset direction if possible
*/
		LR = 1.;
		um_ploctocc(&pick1.ploc,picked);
		uc_retrieve_transf(key1,tfmat);
		uc_init_evcrvout (&crv, &evout);
		if (Sstart+Soffind == 1)
			uu = 0.;
		else
			uu = eptr->cid[Sstart+Soffind-2].endparam;
		status = uc_evcrv(UM_FRSTDERIV,uu,&crv,tfmat,&evout);
		um_vctovc(evout.cp,npt); um_vctovc(evout.dcdu,nve);
		if (status != UU_SUCCESS) 
			ud_wrerr("Direction modifier cannot be determined automatically.");
		else
		{
			um_vpnorm(pick1.ploc.transform, vpnorm);
			ncl_proj_to_wp (picked, vpnorm, picked);
			ncl_proj_to_wp (evout.dcdu, vpnorm, ve0);
			ncl_proj_to_wp (npt, vpnorm, npt2);
			ncl_proj_to_wp (nve, vpnorm, nve2);

			adrv = nve2[0]; bdrv = nve2[1];
			if (adrv*adrv + bdrv*bdrv < UM_FUZZ)
				ud_wrerr("Direction modifier cannot be determined automatically.");
			else
			{
				anorm = - bdrv; bnorm = adrv;
				um_vcmnvc(picked, npt2, vc);
				if (anorm*vc[0] + bnorm*vc[1] < 0) 
					LR = -1.;

				adrv = ve0[0]; bdrv = ve0[1];
				if (adrv*adrv + bdrv*bdrv < UM_FUZZ)
					ud_wrerr("Direction modifier cannot be determined automatically.");
				else
				{
					vc[0] = - LR*bdrv; 
					vc[1] = LR*adrv;

					if (fabs(vc[0]) >= fabs(vc[1]))
					{
						if (vc[0] >= 0.0) Sdir = XLARGE;
						else Sdir = XSMALL;
					}
					else
					{
						if (vc[1] >= 0.0) Sdir = YLARGE;
						else Sdir = YSMALL;
					}
					ud_update_answer(FDR2,&Sdir);
				}
			}
		}
		if (Sall) goto done;
/*
.....Set up assist vectors for component order picking
*/
		if (Sstart+Soffind == 1)
			uu = eptr->cid[0].endparam/2.;
		else
			uu = (eptr->cid[Sstart+Soffind-2].endparam + 
				eptr->cid[Sstart+Soffind-1].endparam)/2.;
		status = uc_evcrv(UM_FRSTDERIV,uu,&crv,tfmat,&evout);
		um_vctovc(evout.cp,npt); um_vctovc(evout.dcdu,nve);
		point.x = npt[0];
		point.y = npt[1];
		point.z = npt[2];
		um_vctovc(nve,assist);
		um_unitvc(assist,assist);
		vector.x = assist[0];
		vector.y = assist[1];
		vector.z = assist[2];
		ud_assist_vector(point,vector);
		um_negvc(assist,assist);
		vector.x = assist[0];
		vector.y = assist[1];
		vector.z = assist[2];
		ud_assist_vector(point,vector);
/*
.....Prompt user to pick one of the assist vectors to determine direction.
*/
		ud_unlimit ();
		irtn = ud_pick_assist_seg("Pick the direction to chain components:");
		if (irtn == 0)
		{
			if (limit_key!=-1)
			{
				ud_limit_entity(UU_FALSE, limit_key);
				limit_key = -1;
			}
			goto redo1;
		}
		limit_key = crv.key;
		ud_limit_entity(UU_TRUE, limit_key);
redo2:
		ud_lgeo(UU_TRUE,UD_compcrv);
		ua_dl_pldas(UD_DASPCKLOC,UA_NCL,714,&pick1,1,&numint,1);
		if (numint > 0)
		{
			key = um_get_pickkey(&pick1.pent, 1);
			if (key == crv.key)
			{
				key1 = um_get_pickkey(&pick1.pent, 2);
				found = UU_FALSE;
				for (i = 0; i < eptr->no_cid && !found; i++)
					found = (key1 == eptr->cid[i].crvid);
				if (found) Send = Tend = i - Soffind;
				ud_update_answer(FEND,&Send);
			}
			else
			{
				ud_wrerr("Incorrect curve selected.");
				goto redo2;
			}
		}
/*
.....Switch order so output component numbers are in ascending order.
*/
		if (irtn != 1)
		{
			temp = Sstart;
			Sstart = Send; Tstart = Send;
			Send = temp; Tend = temp;
		}
		ud_update_answer(FSRT,&Sstart);
		ud_update_answer(FEND,&Send);
		Skey = crv.key;
		Tdir = Sdir;
	}
done:;
	if (limit_key!=-1)
	{
		ud_limit_entity(UU_FALSE, limit_key);
		limit_key = -1;
	}
   ud_unlimit();
   ud_form_vis();
   UD_UNMARK(cmdreject);

   return(UD_FLDOK);
}
/*********************************************************************
**    E_FUNCTION  : UD_FSTAT OnOffset(fieldno, val, stat)
**			Display assist vectors and get user offset direction choice.
**    PARAMETERS
**       INPUT  : fieldno = Form field which initiated this call.
**                val     = Current field value.
**                stat    = Not used.
**       OUTPUT :
**                none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnOffset(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int nc,i,numint,status,irtn,save_ptype;
	int keep1,keep2,keep3;
	char str[256];
	struct NCL_fixed_databag crv;
	struct UM_compcrv_rec *eptr;
	struct UM_evcrvout evout;
	UM_PLOCREC pick;
	UU_KEY_ID  key;
	UM_coord picked, npt;
	UM_vector assist,vc,nve,ve0;
	UM_vector vpnorm,xdir,ydir,zdir;
	UM_vector pv1,pv2,pv3,pv4,pv5,pv6;
	UM_transf tfmat, *tf;
	UU_REAL adrv,bdrv,anorm,bnorm,LR, uu=0.;
	Gwpoint3 point, vector;
	UU_LOGICAL cmdreject;
	char label[MXLAB];
	UM_f77_str f77_str;

   if (*fieldno != FDR1) return (UD_FLDOK);
	S_delete_preview();
/*
.....Take down form
*/
   ud_form_invis();
/*
.....Trap Reject Op
*/
   UD_MARK (cmdreject, UU_TRUE);
   if (cmdreject != 0) 
	{
		ud_setpick_type(save_ptype);
		goto done;
	}
/*
.....Retrieve curve information
*/
	if (Skey == NULLKEY)
	{
		sprintf(label,"%s",Slabel1);
		ul_to_upper(&label);
		nc = strlen(label);
		if (nc == 0)
		{
			ud_wrerr("Curve and starting index needed to determine offset direction.");
			goto done;
		}
		UM_init_f77_str(f77_str,label,64);
		for (i=nc;i<64;i++) label[i] = ' ';
		getkey(UM_addr_of_f77_str(f77_str),&key);
		if (key == 0)
		{
			ud_wrerr("Curve not found. Cannot determine offset direction.");
			goto done;
		}
		crv.key = key;
	}
	else
	{
		crv.key = Skey;
	}
	if (ncl_retrieve_data_fixed(&crv) != 0) goto done;
	eptr = (struct UM_compcrv_rec *) &crv;
	uc_retrieve_transf(crv.key,tfmat);
/*
.....Evaluate curve to set up data for calculating assist vectors
*/
	if (Sstart+Soffind == 1 || Sall == 1)
		uu = 0.;
	else
		uu = eptr->cid[Sstart+Soffind-2].endparam;
	uc_init_evcrvout (&crv, &evout);
	status = uc_evcrv(UM_FRSTDERIV, uu, &crv, tfmat, &evout);
	if (status != UU_SUCCESS)
	{
		ud_wrerr("Curve not found. Cannot determine offset direction.");
		goto done;
	}
	point.x = evout.cp[0];
	point.y = evout.cp[1];
	point.z = evout.cp[2];
	um_vctovc(evout.dcdu,nve);
	um_unitvc(nve,nve);
/*
.....Determine vectors perp to tangent
*/
	xdir[0] = 1.; xdir[1] = xdir[2] = 0.;
	ydir[0] = ydir[2] = 0.; ydir[1] = 1.;
	zdir[0] = zdir[1] = 0.; zdir[2] = 1.;
	um_triple_cross(nve,xdir,nve, pv1);
	um_triple_cross(nve,ydir,nve, pv2);
	um_triple_cross(nve,zdir,nve, pv3);
/*
.....Check to make sure vectors are well defined
*/
	keep1 = keep2 = keep3 = 0;
	if (UM_MAG(pv1) > 0.0157)
	{
		um_unitvc(pv1,pv1); um_negvc(pv1,pv4);
		if (!um_vcparall(pv1,nve)) keep1 = 1;
	}
	if (UM_MAG(pv2) > 0.0157)
	{
		um_unitvc(pv2,pv2); um_negvc(pv2,pv5);
		if (!um_vcparall(pv2,nve)) keep2 = 1;
	}
	if (UM_MAG(pv3) > 0.0157)
	{
		um_unitvc(pv3,pv3); um_negvc(pv3,pv6);
		if (!um_vcparall(pv3,nve)) keep3 = 1;
	}
	if (keep1 && keep2 && um_vcparall(pv1,pv2)) keep2 = 0;
	if (keep1 && keep3 && um_vcparall(pv1,pv3)) keep3 = 0;
	if (keep2 && keep3 && um_vcparall(pv2,pv3)) keep3 = 0;
	if (keep1 + keep2 + keep3 < 2)
	{
		ud_wrerr("Cannot determine offset automatically.");
		goto done;
	}
/*
.....Set up assist vectors
*/
	if (keep1 == 0) um_vctovc(pv2,assist);
	else um_vctovc(pv1,assist);
	vector.x = assist[0];
	vector.y = assist[1];
	vector.z = assist[2];
	ud_assist_vector(point,vector);
	um_negvc(assist,assist);
	vector.x = assist[0];
	vector.y = assist[1];
	vector.z = assist[2];
	ud_assist_vector(point,vector);
	if (keep1 == 0 || keep2 == 0)
		um_vctovc(pv3,assist);
	else um_vctovc(pv2,assist);
	um_unitvc(assist,assist);
	vector.x = assist[0];
	vector.y = assist[1];
	vector.z = assist[2];
	ud_assist_vector(point,vector);
	um_negvc(assist,assist);
	vector.x = assist[0];
	vector.y = assist[1];
	vector.z = assist[2];
	ud_assist_vector(point,vector);
/*
.....Prompt user to pick one of the assist vectors to determine direction.
*/
	save_ptype = ud_getpick_type();
	ud_unlimit ();
	irtn = ud_pick_assist_seg("Pick the direction to chain curve edges:");
	ud_setpick_type(save_ptype);
/*
.....Set offset direction
*/
	if (irtn == 1)
	{
		if (keep1 == 1) Sdir = XLARGE;
		else Sdir = YLARGE;
	}
	else if (irtn == 2)
	{
		if (keep1 == 1) Sdir = XSMALL;
		else Sdir = YSMALL;
	}
	else if (irtn == 3)
	{
		if (keep1 == 0 || keep2 == 0) Sdir = ZLARGE;
		else Sdir = YLARGE;
	}
	else if (irtn == 4)
	{
		if (keep1 == 0 || keep2 == 0) Sdir = ZSMALL;
		else Sdir = YSMALL;
	}
	Tdir = Sdir;
	Sapply = UU_FALSE;
done:
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION   : OnEdit(fieldno, val, stat)
**       Method called when an edit field traversed.  Set Sapply flag
**       if a change has been made to a field so the command is not
**       output on OK if Apply was pressed and no changes have been
**       made since then.
**    PARAMETERS
**       INPUT  : fieldno = Form field which initiated this call.
**                val     = Current field value.
**                stat    = Not used.
**       OUTPUT :
**                none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnEdit(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char tlab[MXLAB];
	UU_REAL tval;

	switch (*fieldno)
	{
/*
.....Curve Field
*/
		case FLB1:
			if (strcmp(Slabel1,Tlabel1) != 0)
			{
				Skey = NULLKEY;
				Sapply = UU_FALSE;
			}
			break;
/*
.....From field
*/
		case FSRT:
			if (Sstart != Tstart)
			{
				Tstart = Sstart;
				Sapply = UU_FALSE;
			}
			break;
/*
.....To field
*/
		case FEND:
			if (Send != Tend)
			{
				Tend = Send;
				Sapply = UU_FALSE;
			}
			break;
/*
.....Direction modifier menu
*/
		case FDR2:
			if (Sdir != Tdir)
			{
				Tdir = Sdir;
				Sapply = UU_FALSE;
			}
/*
.....Enable/Disable vector select fields.
*/
			if (Sdir == VECTOR)
			{
				ud_set_traverse_mask(FVLB,UU_TRUE);
				ud_set_traverse_mask(FVSL,UU_TRUE);
			}
			else
			{
				ud_set_traverse_mask(FVLB,UU_FALSE);
				ud_set_traverse_mask(FVSL,UU_FALSE);
			}
			break;
/*
.....Distance field
*/
		case FDIS:
			if (strcmp(Sdist_str,Tdist_str) != 0)
				Sapply = UU_FALSE;
			break;
/*
.....Connection type menu
*/
		case FCON:
			if (Scon != Tcon)
			{
				Tcon = Scon;
				Sapply = UU_FALSE;
			}
			break;
/*
.....New label field
*/
		case FLB2:
			if (strcmp(Slabel2,Tlabel2) != 0)
				Sapply = UU_FALSE;
			break;
/*
.....Vector label field
*/
		case FVLB:
			if (strcmp(Svec_lab,Tvec_lab) != 0)
				Sapply = UU_FALSE;
			break;
	}
	return (UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : OnAction(fieldno, val, stat)
**       Method called when an Action button is pressed.
**    PARAMETERS
**       INPUT  : fieldno = Form field which initiated this call.
**                val     = Current field value.
**                stat    = Not used.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnAction(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
/*
.....Enter viewing mode
*/
		case FVEW:
			ud_form_invis();
			uz_dyn_mouse();
			ud_form_vis();
			break;
/*
.....Preview command
*/
		case FPVW:
			S_delete_preview();
			S_build_cmd(UU_TRUE);
			break;
/*
.....Apply - Output command
.......Set Sapply to true so no extra command is output
.......on OK if no changes were made since the last Apply.
*/
		case FAPP:
			S_delete_preview();
			S_build_cmd(UU_FALSE);
			Sapply = UU_TRUE;
			break;
/*
.....All
*/
		case FALL:
		{
			if (Sall == 0)
			{
				ud_set_traverse_mask(FSRT, UU_TRUE);
				ud_set_traverse_mask(FEND, UU_TRUE);
			}
			else
			{
				ud_set_traverse_mask(FSRT, UU_FALSE);
				ud_set_traverse_mask(FEND, UU_FALSE);
			}
			Sapply = UU_FALSE;
			break;
		}
/*
.....Use Existing Label
*/
		case FEXS:
			if (Sexist)
				strcpy(Slabel2,Slabel1);
			else
				Slabel2[0] = '\0';
			ud_update_answer(FLB2,&Slabel2);
			ud_set_traverse_mask(FLB2,!Sexist);
			break;
	}
	return (UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : OnSelVec(fieldno, val, stat)
**       Method called when an Action button is pressed.
**    PARAMETERS
**       INPUT  : fieldno = Form field which initiated this call.
**                val     = Current field value.
**                stat    = Not used.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSelVec(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,dir,numint,pick_mask[UD_NMENTWD],*mask;
	struct NCL_fixed_databag vec;
	UM_PLOCREC pick;
	UU_LOGICAL cmdreject;

	S_delete_preview();
/*
.....Take down form
*/
   ud_form_invis();
/*
.....Trap Reject Op
*/
   UD_MARK (cmdreject, UU_TRUE);
   if (cmdreject != 0) goto done;
/*
.....Set the appropriate selection mask
*/
	for (i=0; i<UD_NMENTWD; i++)
	{
		pick_mask[i] = UD_ncl_ve[i] | UD_ncl_pv[i];
	}
	mask = (int *)pick_mask;
   ud_lgeo(UU_TRUE,mask);
/*
.....Get the vector
*/
	ua_dl_pldas(UD_DASPCKLOC,UA_NCL,709,&pick,1,&numint,1);
	if (numint > 0)
	{
		vec.key = um_get_pickkey(&pick.pent, 1);
		ncl_retrieve_data_fixed(&vec);
		ncl_get_label(&vec,Svec_lab);
		ul_to_upper(Svec_lab); strcpy(Tvec_lab,Svec_lab);
		ud_update_answer(FVLB,&Svec_lab);
		Tdir = Sdir;
		Sapply = UU_FALSE;
	}
done:
   ud_unlimit();
   ud_form_vis();
   UD_UNMARK(cmdreject);
	return (UD_FLDOK);
}
