/*********************************************************************
**      FILENAME: lmodals1.c
**      CONTAINS:
**            ul_modals_pfeeds
**            ul_modals_pinterp
**            ul_modals_macros
**            ul_modals_background
**    COPYRIGHT 2007 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       lmodals1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:19
*********************************************************************/

#include "lcom.h"
#include "usysdef.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "ustdio.h"
#include "mdattr.h"
#include "mdcpln.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "mattrddl.h"
#include "mdrel.h"
#include "nclmodals.h"
#include "view1.h"

static char lstyle1[9][64] = {"*DEFAULT","*SOLID","*DASH","*DOTS","*CENTER",
	"*PHANTOM","*DASHLN","*DASHDT","*DASHSP"};
static char scolor[2][96] = {"Default", "RGB"};
static char yesno[2][64] = {"*NO","*YES"};

extern char uw_color_name[64][96];
/*********************************************************************
**       I_FUNCTION : ul_modals_pfeeds(ctyp,cmsg)
**          This function sets the motion playback feed rate analyzation
**          speeds and colors.
**       PARAMETERS
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
ul_modals_pfeeds (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,inum;
	UU_REAL rval;
	int maxsub=20;
	static char csub[20][20] = {"FEED1", "FEED2", "FEED3", "FEED4", "FEED5",
		"FEED6", "FEED7", "FEED8", "FEED9", "FEED10", "FEED1_COLOR",
		"FEED2_COLOR", "FEED3_COLOR", "FEED4_COLOR", "FEED5_COLOR",
		"FEED6_COLOR", "FEED7_COLOR", "FEED8_COLOR","FEED9_COLOR",
		"FEED10_COLOR"};
/*
.....Get modal to define
*/
	ul_to_upper(ctyp);
	for (i=0;i<maxsub;i++)
	{
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (serror,"Not a valid PLAY_FEEDS modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Feed rates
*/
	case 0:
	case 1:
	case 2:
	case 3:
	case 4:
	case 5:
	case 6:
	case 7:
	case 8:
	case 9:
		if ((ul_to_reals(&rval,&inum,1,cmsg) != UU_SUCCESS) || inum != 1)
			goto bad_parm;
		UM_len_exttoint(rval,UN_anlz_feed[i]);
		break;
/*
......Feed colors
*/
	case 10:
	case 11:
	case 12:
	case 13:
	case 14:
	case 15:
	case 16:
	case 17:
	case 18:
	case 19:
		if (ul_modal_color(cmsg, &UN_anlz_fcolor[i-10], scolor, 1)
			!= UU_SUCCESS) goto bad_parm;		
			UN_anlz_fcolor[i-10] = UN_anlz_fcolor[i-10];
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for PLAY_FEEDS modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : ul_modals_pinterp(ctyp,cmsg)
**          This function sets the motion playback interpolation
**          analyzation colors and line styles.
**       PARAMETERS
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
ul_modals_pinterp (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status;
	int maxsub=12;
	static char csub[12][20] = {"GEO_COLOR", "GEO_LINE",
		"LINEAR_COLOR", "CIRCLE_COLOR", "RAPID_COLOR", "CYCLE_COLOR",
		"TLAXIS_COLOR",
		"LINEAR_LINE", "CIRCLE_LINE", "RAPID_LINE", "CYCLE_LINE",
		"TLAXIS_LINE"};
/*
.....Get modal to define
*/
	ul_to_upper(ctyp);
	for (i=0;i<maxsub;i++)
	{
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (serror,"Not a valid PLAY_INTERP modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Geometry color
*/
    case 0:
		if (ul_modal_color(cmsg, &UN_anlz_geo[0], scolor, 1)
			!= UU_SUCCESS) goto bad_parm;
		break;
/*
.....Geometry line style
*/
    case 1:
		if (ul_modal_toggle(cmsg,lstyle1,9,&UN_anlz_geo[1])
			!= UU_SUCCESS) goto bad_parm;

		break;
/*
......Interp colors
*/
	case 2:
	case 3:
	case 4:
	case 5:
	case 6:
		if (ul_modal_color(cmsg, &UN_anlz_icolor[i-2], scolor, 1)
			!= UU_SUCCESS) goto bad_parm;
		break;
/*
......Interp line styles
*/
	case 7:
	case 8:
	case 9:
	case 10:
	case 11:
		if (ul_modal_toggle(cmsg,lstyle1,9,&UN_anlz_istyle[i-7])
			!= UU_SUCCESS) goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for PLAY_INTERP modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}
/*********************************************************************
**       I_FUNCTION : ul_modals_macros(ctyp,cmsg)
**                      This function sets the Macro Form modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
ul_modals_macros(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status;
	int maxsub=3;
	static char csub[3][20] = {"MODAL","DEFAULT", "VALUES"};
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (serror,"Not a valid MACRO modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....MODAL
*/
	case 0:
		if (ul_modal_toggle(cmsg,yesno,2,&NCL_macro_modal) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....DEFAULT
*/
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&NCL_macro_outdefault) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....VALUES
*/
	case 2:
		if (ul_modal_toggle(cmsg,yesno,2,&NCL_macro_remval) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for MACRO modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**	 I_FUNCTION : ul_modals_background(ctyp,cmsg)
**			This function sets the View Background image modals.
**	 PARAMETERS	
**		 INPUT  :
**        ctyp = Modal begin defined.
**			 cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_modals_background (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status;
	int maxsub=18;
	static char *csub[] = {"SHADER","COLOR","RGB","TOP_COLOR","BOT_COLOR",
		"TOP_RGB","BOT_RGB","UL_COLOR","UR_COLOR","LL_COLOR","LR_COLOR",
		"UL_RGB","UR_RGB","LL_RGB","LR_RGB","IMAGE","ROTATE","STRETCH"};
	static char cshade[4][64] = {"*SOLID","*GRADUATE","*4-CORNER","*IMAGE"};
	static char crot[4][64] = {"*0","*90","*180","*270"};
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (serror,"Not a valid BACKGROUND modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Shader
*/
	case 0:
		if (ul_modal_toggle(cmsg,cshade,4,&UV_background.shader) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Solid Color
*/
	case 1:
		if (ul_modal_color(cmsg, &UV_background.bgcolor, scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UV_background.bgcolor<0)&&(UV_background.bgcolor!=-2))
			goto bad_parm;
		else if (UV_background.bgcolor<0)
			UV_background.bgcolor = -1;
		break;
/*
.....Solid RGB
*/
	case 2:
		if (ul_rgb_in(cmsg,UV_background.bgrgb) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....Graduated Top Color
*/
	case 3:
		if (ul_modal_color(cmsg, &UV_background.grad[0], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UV_background.grad[0]<0)&&(UV_background.grad[0]!=-2))
			goto bad_parm;
		else if (UV_background.grad[0]<0)
			UV_background.grad[0] = -1;
		break;
/*
.....Graduated Bottom Color
*/
	case 4:
		if (ul_modal_color(cmsg, &UV_background.grad[1], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UV_background.grad[1]<0)&&(UV_background.grad[1]!=-2))
			goto bad_parm;
		else if (UV_background.grad[1]<0)
			UV_background.grad[1] = -1;
		break;
/*
.....Graduated Top RGB
*/
	case 5:
		if (ul_rgb_in(cmsg,UV_background.grgb[0]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....Graduated Bottom RGB
*/
	case 6:
		if (ul_rgb_in(cmsg,UV_background.grgb[1]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....4-Corner Top-Left Color
*/
	case 7:
		if (ul_modal_color(cmsg, &UV_background.fcolor[0], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UV_background.fcolor[0]<0)&&(UV_background.fcolor[0]!=-2))
			goto bad_parm;
		else if (UV_background.fcolor[0]<0)
			UV_background.fcolor[0] = -1;
		break;
/*
.....4-Corner Top-Right Color
*/
	case 8:
		if (ul_modal_color(cmsg, &UV_background.fcolor[1], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UV_background.fcolor[1]<0)&&(UV_background.fcolor[1]!=-2))
			goto bad_parm;
		else if (UV_background.fcolor[1]<0)
			UV_background.fcolor[1] = -1;
		break;
/*
.....4-Corner Lower-Left Color
*/
	case 9:
		if (ul_modal_color(cmsg, &UV_background.fcolor[2], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UV_background.fcolor[2]<0)&&(UV_background.fcolor[2]!=-2))
			goto bad_parm;
		else if (UV_background.fcolor[2]<0)
			UV_background.fcolor[2] = -1;
		break;
/*
.....4-Corner Lower-Right Color
*/
	case 10:
		if (ul_modal_color(cmsg, &UV_background.fcolor[3], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((UV_background.fcolor[3]<0)&&(UV_background.fcolor[3]!=-2))
			goto bad_parm;
		else if (UV_background.fcolor[3]<0)
			UV_background.fcolor[3] = -1;
		break;
/*
.....4-Corner Top-Left RGB
*/
	case 11:
		if (ul_rgb_in(cmsg,UV_background.frgb[0]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....4-Corner Top-Right RGB
*/
	case 12:
		if (ul_rgb_in(cmsg,UV_background.frgb[1]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....4-Corner Lower-Left RGB
*/
	case 13:
		if (ul_rgb_in(cmsg,UV_background.frgb[2]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....4-Corner Lower-Right RGB
*/
	case 14:
		if (ul_rgb_in(cmsg,UV_background.frgb[3]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....Image file
*/
	case 15:
		strcpy(UV_background.bgfile,cmsg);
		break;
/*
.....Rotate
*/
	case 16:
		if (ul_modal_toggle(cmsg,crot,4,&UV_background.rotate) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Stretch
*/
	case 17:
		if (ul_modal_toggle(cmsg,yesno,2,&UV_background.stretch) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for BACKGROUND modal. /%s/ %s",ctyp,cmsg);
	ud_wrerr (serror);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}
