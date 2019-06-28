/*********************************************************************
**    NAME         :  ntlattr.c
**       CONTAINS:
**                ncl_tool_calc()
**                ncl_tool_attr()
**                ncl_tool_attr1()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ntlattr.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 10:49:09
*********************************************************************/
#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "mdattr.h"
#include "mfort.h"
#include "nclstack.h"

double fabs();

UU_LOGICAL UN_motmodal_cmd=UU_TRUE;
int UN_motion_color=1, UN_rapid_color=3;
int UN_motion_line=UM_SOLID_LINE, UN_rapid_line=UM_DASHED_LINE;
int UN_motion_pen=1, UN_rapid_pen=1;

double UN_motion_width = 1.0;

extern char uw_color_name[64][96];

static int S_motion_modals();

/*********************************************************************
**    E_FUNCTION     : ncl_tool_attr(formfile)
**       Sets the motion display attributes using an interactive form.
**    PARAMETERS   
**       INPUT  : 
**          formfile          form file name
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_tool_attr(formfile)
char  *formfile;
{
	int color[3],mot_color,mot_line,rp_line,shaded[3],trans[3],itmp1,itmp2;
	int rp_color,rp_pen,mot_pen,pen[3],cmd,cutiter4,stk,trafl,status,cfl[10];
	int *ans[24], wid_chc;
	UM_int2 cutiter2;
	UM_int2 ifl,val;
	UM_int4 tracut;
	UU_LOGICAL iter,idid;
	char tstr[80];
	UN_motseg_cutattr cattr;
	NCL_cmdbuf cmdbuf;
	static char *stype[8] = {"SOLID","DASH","DOTTED","CENTER","PHANTM",
		"DASHLN","DASHDT","DASHSP"};
	static char *stoggle[2] = {"OFF","ON"};
/*
.....Get the default settings
*/
	ncl_cutter_get_defattr(&cattr);
	cutget_flag (cfl);
/*
........Motion attributes
*/
	mot_color = UN_motion_color;
	rp_color = UN_rapid_color;
	mot_pen = UN_motion_pen;
	rp_pen = UN_rapid_pen;
	gtrafl(&tracut);
	trafl = tracut;
	mot_line = UN_motion_line - 1;
	rp_line = UN_rapid_line - 1;
	if (UN_motion_width==1.0)
	{
		wid_chc = 0;
	}
	else if (UN_motion_width==2.0)
	{
		wid_chc = 1;
	}
	else if (UN_motion_width==3.0)
	{
		wid_chc = 2;
	}
	else if (UN_motion_width==4.0)
	{
		wid_chc = 3;
	}
	else
		wid_chc = 0;
//	sprintf (mot_wid, "%5.3f", UN_motion_width);
/*
........Cutter attributes
*/
	color[0] = cattr.color[0];
	pen[0] = cattr.pen[0];
	shaded[0] = cfl[3];
	trans[0] = cattr.trans[0];
	ifl = 129;
	getifl(&ifl,&cutiter2);
	cutiter4 = cutiter2;
/*
........Shank attributes
*/
	color[1] = cattr.color[1];
	pen[1] = cattr.pen[1];
	shaded[1] = cfl[6];
	trans[1] = cattr.trans[1];
/*
........Holder attributes
*/
	color[2] = cattr.color[2];
	pen[2] = cattr.pen[2];
	shaded[2] = cfl[7];
	trans[2] = cattr.trans[2];
/*
........Motion stack
*/
	stk = UN_mot_stack_size;
	ifl = 120; getlfl(&ifl,&val);
	iter = val;
	cmd = UN_motmodal_cmd;
/*
.....Setup the form answers
*/
	ans[0] = (int *)&mot_color;
	ans[1] = (int *)&mot_line;
	ans[2] = (int *)&mot_pen;
	ans[3] = (int *)&rp_color;
	ans[4] = (int *)&rp_line;
	ans[5] = (int *)&rp_pen;
	ans[6] = (int *)&trafl;
	ans[7] = (int *)&wid_chc;
	ans[8] = (int *)&color[0];
	ans[9] = (int *)&cutiter4;
	ans[10] = (int *)&pen[0];
	ans[11] = (int *)&shaded[0];
	ans[12] = (int *)&trans[0];
	ans[13] = (int *)&color[1];
	ans[14] = (int *)&pen[1];
	ans[15] = (int *)&shaded[1];
	ans[16] = (int *)&trans[1];
	ans[17] = (int *)&color[2];
	ans[18] = (int *)&pen[2];
	ans[19] = (int *)&shaded[2];
	ans[20] = (int *)&trans[2];
	ans[21] = (int *)&stk;
	ans[22] = (int *)&iter;
	ans[23] = (int *)&cmd;
/*
.....Get the form input
*/
	status = ud_form(formfile, ans, ans);
	if (status==-1) return -1;
	if ((color[0]==-1)||(color[1]==-1)||(color[2]==-1)
		|| (rp_color==-1) || (mot_color==-1))
	{
/*
......should not goes here. No default color selection for cutter
*/
		ud_winerror("No default color election for cutter.");
		return -1;
	}
/*
.....Output NCL commands
*/
	if (cmd)
	{
/*
........Motion & Cutter attributes
*/
		ncl_init_cmdbuf(&cmdbuf);
		sprintf(tstr,"%s%s",NCL_draft,NCL_show_cutter);
		ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
		idid = UU_FALSE;
		itmp1 = mot_color;
		itmp2 = color[0];
		if (itmp1 != UN_motion_color || itmp2 != cattr.color[0])
		{
			sprintf(tstr,",COLOR=%s,%s",uw_color_name[itmp1],uw_color_name[itmp2]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			idid = UU_TRUE;
		}
		if (cutiter4 != cutiter2)
		{
			sprintf(tstr,",STEP=%d",cutiter4);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			idid = UU_TRUE;
		}
		if (mot_line+1 != UN_motion_line)
		{
			sprintf(tstr,",LINTYP=%s",stype[mot_line]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			idid = UU_TRUE;
		}
		itmp1 = rp_color;
		itmp2 = (rp_line+1 != UN_rapid_line);
		if (rp_color != UN_rapid_color || itmp2)
		{
			ncl_add_token(&cmdbuf,",RAPID",NCL_nocomma);
			idid = UU_TRUE;
			if (rp_color != UN_rapid_color)
			{
				sprintf(tstr,",COLOR=%s",uw_color_name[rp_color]);
				ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			}
			if (itmp2)
			{
				sprintf(tstr,",LINTYP=%s",stype[rp_line]);
				ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			}
		}
		if (trans[0] != cattr.trans[0])
		{
			sprintf(tstr,",TRANS=%d",trans[0]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			idid = UU_TRUE;
		}
		if (trafl != tracut)
		{
			sprintf(tstr,",TRACUT=%s",stoggle[trafl]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			idid = UU_TRUE;
		}
		if (idid) ncl_add_cmdbuf(&cmdbuf);
		else ncl_init_cmdstr(&cmdbuf);
/*
........CUTTER/DISPLY
*/
		if (shaded[0] != cfl[3])
		{
			sprintf(tstr,"%s%s",NCL_cutter,NCL_cmd_disply);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			sprintf(tstr,",SHADE,%s,CUTTER",stoggle[shaded[0]]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
		}
/*
........Shank attributes
*/
		sprintf(tstr,"%s%s,%s",NCL_draft,NCL_show_cutter,"SHANK");
		ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
		idid = UU_FALSE;
		itmp1 = color[1];
		if (itmp1 != cattr.color[1])
		{
			sprintf(tstr,",COLOR=%s",uw_color_name[itmp1]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			idid = UU_TRUE;
		}
		if (trans[1] != cattr.trans[1])
		{
			sprintf(tstr,",TRANS=%d",trans[1]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			idid = UU_TRUE;
		}
		if (idid) ncl_add_cmdbuf(&cmdbuf);
		else ncl_init_cmdstr(&cmdbuf);
/*
........CUTTER/DISPLY,SHANK
*/
		if (shaded[1] != cfl[6])
		{
			sprintf(tstr,"%s%s",NCL_cutter,NCL_cmd_disply);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			sprintf(tstr,",SHADE,%s,SHANK",stoggle[shaded[1]]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
		}
/*
........HOLDER attributes
*/
		sprintf(tstr,"%s%s,%s",NCL_draft,NCL_show_cutter,"HOLDER");
		ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
		idid = UU_FALSE;
		itmp1 = color[2];
		if (itmp1 != cattr.color[2])
		{
			sprintf(tstr,",COLOR=%s",uw_color_name[itmp1]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			idid = UU_TRUE;
		}
		if (trans[2] != cattr.trans[2])
		{
			sprintf(tstr,",TRANS=%d",trans[2]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			idid = UU_TRUE;
		}
		if (idid) ncl_add_cmdbuf(&cmdbuf);
		else ncl_init_cmdstr(&cmdbuf);
/*
........CUTTER/DISPLY,HOLDER
*/
		if (shaded[2] != cfl[7])
		{
			sprintf(tstr,"%s%s",NCL_cutter,NCL_cmd_disply);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			sprintf(tstr,",SHADE,%s,HOLDER",stoggle[shaded[2]]);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
		}
/*
........Output command(s)
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}
/*
.....Set changed parameters
........Motion attributes
*/
	UN_motion_color = mot_color;
	UN_rapid_color = rp_color;
	UN_motion_pen = mot_pen;
	UN_rapid_pen = rp_pen;
	tracut = trafl;
	strafl(&tracut);
	UN_motion_line = mot_line + 1;
	UN_rapid_line = rp_line + 1;
	if (wid_chc==0)
		UN_motion_width = 1.0;
	else if (wid_chc==1)
		UN_motion_width = 2.0;
	else if (wid_chc==2)
		UN_motion_width = 3.0;
	else if (wid_chc==3)
		UN_motion_width = 4.0;
/*
........Cutter attributes
*/
	cattr.color[0] = color[0];
	cattr.pen[0] = pen[0];
	cfl[3] = shaded[0];
	cattr.trans[0] = trans[0];
	ifl = 129;
	cutiter2 = cutiter4;
	setifl(&ifl,&cutiter2);
/*
........Shank attributes
*/
	cattr.color[1] = color[1];
	cattr.pen[1] = pen[1];
	cfl[6] = shaded[1];
	cattr.trans[1] = trans[1];
/*
........Holder attributes
*/
	cattr.color[2] = color[2];
	cattr.pen[2] = pen[2];
	cfl[7] = shaded[2];
	cattr.trans[2] = trans[2];
/*
.....Set the default settings
*/
	ncl_cutter_set_attr(&cattr);
	cutset_flag(cfl);
/*
.....Motion stack
*/
	if (stk != UN_mot_stack_size)
	{
		UN_mot_stack_size = stk;
		ncl_mot_stack_init();
	}
	stk = UN_mot_stack_size;
	val = iter;
	ifl = 120; setlfl(&ifl,&val);
	UN_motmodal_cmd = cmd;
/*
.....Store modals file
*/
	S_motion_modals(&cattr,cfl,tracut,cutiter4,iter);
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_tool_attr1(cuttrans,cutiter,cut_color_ex,
**		                                mot_color_ex,mot_line_ex,rp_color_ex,
**		                                rp_line_ex,trafl,sh_color_ex,hd_color_ex)
**       Set tool attributes using NCL command parameters.  A value of -1
**			for any parameter will leave that attribute unchanged.
**    PARAMETERS   
**       INPUT  : 
**				cuttrans      = Cutter, Shank, and Holder transparencies.
**				cutiter       = Display cutter every 'cutiter' iteration.
**				cut_color_ex  = Cutter color.
**				mot_color_ex  = Motion color.
**				mot_line_ex   = Motion line style.
**				rp_color_ex   = Rapid color.
**				rp_line_ex    = Rapid line style.
**				trafl         = 1 = Apply TRACUT to motion display.
**				trafl         = 1 = Apply TRACUT to motion display.
**				sh_color_ex   = Shank color.
**				hd_color_ex   = Holder color.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_tool_attr1(cuttrans, cutiter,cut_color_ex,mot_color_ex,mot_line_ex,
	rp_color_ex,rp_line_ex,trafl,sh_color_ex,hd_color_ex)
int *cuttrans, *cutiter,*cut_color_ex,*mot_color_ex,*mot_line_ex,*rp_color_ex;
int *rp_line_ex,*trafl,*sh_color_ex,*hd_color_ex;
{
	UM_int2 ifl, cutiter2;
	UM_int4 tracut;
	UN_motseg_cutattr cattr;

	ifl = 129;
	getifl(&ifl,&cutiter2);
	gtrafl(&tracut);
/*
.....Set motion attributes
*/
	cattr.mov = cattr.segfl = -1;
	cattr.shaded[0] = cattr.shaded[1] = cattr.shaded[2] = -1;
	cattr.pen[0] = cattr.pen[1] = cattr.pen[2] = -1;
	cattr.color[0] = cattr.color[1] = cattr.color[2] = -1;
	cattr.trans[0] = cattr.trans[1] = cattr.trans[2] = -1;
	if(*cut_color_ex != -1) cattr.color[0] = *cut_color_ex;
	if(*mot_color_ex != -1) UN_motion_color = *mot_color_ex;
	if(*mot_line_ex  != -1) UN_motion_line  = *mot_line_ex + 1;
	if(*rp_color_ex  != -1) UN_rapid_color = *rp_color_ex;
	if(*rp_line_ex   != -1) UN_rapid_line   = *rp_line_ex + 1;
	if(*sh_color_ex  != -1) cattr.color[1] = *sh_color_ex;
	if(*hd_color_ex  != -1) cattr.color[2] = *hd_color_ex;
	if(cuttrans[0] != -1) cattr.trans[0] = cuttrans[0];
	if(cuttrans[1] != -1) cattr.trans[1] = cuttrans[1];
	if(cuttrans[2] != -1) cattr.trans[2] = cuttrans[2];
	ncl_cutter_set_attr(&cattr);

	/* update the number of iterations between cutter displays */

	if (*cutiter != -1 && *cutiter != cutiter2)
		{
        cutiter2 = *cutiter;
		upiter(&cutiter2);
		}
	if (*trafl != -1 && *trafl != tracut)
	{
		strafl(trafl);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_motion_modals(&cattr,cfl,tracut,cutiter,iter)
**       Save the NCLIPV playback properties into modals file.
**    PARAMETERS   
**       INPUT  : 
**          cattr   = Cutter attribute structure.
**          cfl     = Cutter shade flags.
**          tracut  = Tracut flag.
**          cutiter = Cutter Iteration count.
**          iter    = Display cutter at each iteration.
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_motion_modals(cattr,cfl,tracut,cutiter,iter)
UN_motseg_cutattr *cattr;
int cfl[],tracut,cutiter,iter;
{
	int stat;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;
	static char yesno[2][10] = {"*NO","*YES"};
	static char lstyle[8][64] = {"*SOLID","*SMALL_DASH","*DOTTED","*CENTER",
		"*PHANTOM","*DASHED","*DASH_DOT","*DASH_SPACE"};
	static char lmstyle[8][64] = {"*STANDARD","*MEDIUM","*HEAVY","*EXHEAVY"};
	static char ltra[2][64] = {"*IGNORE","*APPLY"};
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy (fname, "ncl_motion.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store motion modals
*/
	ux_fputs0("#MOTION#\n", fptr);
	sprintf(msg,"/CUTTER_COLOR/ *%s\n",uw_color_name[cattr->color[0]]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/CUTTER_PEN/ %d\n",cattr->pen[0]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/CUTTER_SHADED/ %s\n",yesno[cfl[3]]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/CUTTER_TRANS/ %d\n",cattr->trans[0]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SHANK_COLOR/ *%s\n",uw_color_name[cattr->color[1]]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SHANK_PEN/ %d\n",cattr->pen[1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SHANK_SHADED/ %s\n",yesno[cfl[6]]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/SHANK_TRANS/ %d\n",cattr->trans[1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/HOLDER_COLOR/ *%s\n",uw_color_name[cattr->color[2]]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/HOLDER_PEN/ %d\n",cattr->pen[2]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/HOLDER_SHADED/ %s\n",yesno[cfl[7]]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/HOLDER_TRANS/ %d\n",cattr->trans[2]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MOTION_COLOR/ *%s\n",uw_color_name[UN_motion_color]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MOTION_STYLE/ %s\n",lstyle[UN_motion_line-1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MOTION_PEN/ %d\n",UN_motion_pen);
	ux_fputs0(msg, fptr);
/*
	sprintf(msg,"/MOTION_WID/ %f\n",UN_motion_width);
*/
    if (UN_motion_width == 2) 
	    sprintf(msg,"/MOTION_WID/ %s\n",lmstyle[1]);
	else if (UN_motion_width == 3)		
	    sprintf(msg,"/MOTION_WID/ %s\n",lmstyle[2]);
	else if (UN_motion_width == 4)
	    sprintf(msg,"/MOTION_WID/ %s\n",lmstyle[3]);
	else
	    sprintf(msg,"/MOTION_WID/ %s\n",lmstyle[0]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/RAPID_COLOR/ *%s\n",uw_color_name[UN_rapid_color]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/RAPID_STYLE/ %s\n",lstyle[UN_rapid_line-1]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/RAPID_PEN/ %d\n",UN_rapid_pen);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/CUTTER_STEP/ %d\n",cutiter);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/TRACUT/ %s\n",ltra[tracut]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/CUTTER_ITERATE/ %s\n",yesno[iter]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/STACK_SIZE/ %d\n",UN_mot_stack_size);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/COMMANDS/ %s\n",yesno[UN_motmodal_cmd]);
	ux_fputs0(msg, fptr);
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

