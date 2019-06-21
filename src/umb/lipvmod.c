/*********************************************************************
**   FILENAME: lipvmod.c
**   CONTAINS: ul_ipv_load_mod()
**             ul_ipv_mod_session()
**             ul_ipv_mod_tools()
**             ul_ipv_mod_stock()
**             ul_ipv_mod_colors()
**             ul_ipv_mod_toler()
**             ul_ipv_mod_compare()
**             ul_ipv_mod_diag()
**             ul_ipv_mod_universe()
**             ul_ipv_mod_display()
**             ul_ipv_mod_playback()
**             ul_ipv_mod_monitor()
**             ul_ipv_mod_background()
**             ul_ipv_mod_lights()
**     COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       lipvmod.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:14
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "lcom.h"
#include "mdcpln.h"
#include "mdrel.h"
#include "mfort.h"
#include "nclfc.h"
#include "nccs.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"
#if UU_COMP!=UU_WIN2K
/*#include "wsxw.h"*/
#else
#include "wsgl.h"
#endif

#include "lipv.h"
#include "lipvmplay.h"
#include "lipvstack.h"

extern int NAUTLTH;

void ul_ipv_set_colors();

static UU_REAL Scnv;

static char yesno[2][64] = {"*NO","*YES"};
static char scolor[64][96] = {"DEFAULT","RGB", "AUTO"};
static char units[2][64] = {"*INCH","*MM"};

#define S_len_exttoint(n1,n2)\
	{ n2 = n1 * Scnv; }
extern char uw_color_name[64][96];

/*********************************************************************
**	 E_FUNCTION : ul_ipv_load_mod()
**			This function loads the NCLIPV default modals
**			from a disk file.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_load_mod()
{
	char serror[80];
	int i,status,stat,opend;
	UX_pathname fname;
	FILE *fptr;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	opend = 0;
	Scnv = UM_cpln.length_to_cm;
/*
.....Check for modals file
*/
	fname[0] = '\0';
	stat = ul_open_mod_file (UU_NULL, UU_NULL, "UL_NCLIPV_MODALS", UU_NULL,
		fname, 2, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL))
	{
		sprintf (serror,"Cannot open NCLIPV Modals file %s.", fname);
		ud_wrerr (serror);
		goto failed;
	}
	opend = 1;
	status = ul_ipv_load_modfile(fptr);
	if (status == UU_SUCCESS) goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (opend == 1) ux_fclose0(fptr);
/*
........Set-up automatic motion colors
*/
	ul_ipv_set_colors();
	return(status);
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_load_modfile(fptr)
**			This function loads the NCLIPV default modals
**			from a disk file.
**	 PARAMETERS	
**		 INPUT  :  fptr   = File pointer of modals file to load.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_load_modfile(fptr)
FILE *fptr;
{
	char serror[80],buf[200],ctyp[40],cmsg[200];
	int status,stat,numint,ityp,i,isub,istat,first;
	int maxsub=19;
	static char *csub[]={"SESSION","TOOLS","STOCK","FIXTURE","COLORS",
		"TOLERANCES","COMPARE","DIAGNOSTICS","UNIVERSE","DISPLAY","PLAYBACK",
		"MONITOR","BACKGROUND","LIGHTS","UNDO","INCLUDE", "COLOR", "VIEW", "INTERFACE"};
/*
.....Assume success
*/
	status = UU_SUCCESS;
	isub = 0;
	first = 1;
/*
.....Read a record
*/
	do
	{
		stat = ul_fread (fptr,buf,sizeof(buf),&numint);
		if (stat == UX_EOF) goto done;
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			sprintf (serror,"Error reading from mod file E%d.",stat);
			ud_wrerr (serror);
			goto failed;
		}
/*
.....Check for record type
*/
		istat = ul_modal_check (buf,&ityp,ctyp,cmsg);
/*
.....Invalid syntax
*/
		if (istat != UU_SUCCESS)
		{
			sprintf (serror,"Modals file syntax error. %s",buf);
			ud_wrerr (serror);
		}
/*
.....Subsystem type
*/
		switch (ityp)
		{
		case 1:
			for (i=0;i<maxsub;i++)
			{
				ul_to_upper(ctyp);
				if (strcmp(ctyp,csub[i]) == 0) break;
			}
			if (i >= maxsub)
			{
				sprintf (serror,"Not a valid Modals parameter. %s",
					buf);
				ud_wrerr (serror);
				break;
			}
			isub = i + 1;
			break;
		case 2:
			switch (isub)
			{
			case 0:
				ul_ipv_mod_units(ctyp,cmsg);
				break;
			case 1:
				ul_ipv_mod_session(ctyp,cmsg);
				break;
			case 2:
				ul_ipv_mod_tools(ctyp,cmsg);
				break;
			case 3:
				ul_ipv_mod_stock(ctyp,cmsg,0);
				break;
			case 4:
				ul_ipv_mod_stock(ctyp,cmsg,1);
				break;
			case 5:
				ul_ipv_mod_colors(ctyp,cmsg);
				break;
			case 6:
				ul_ipv_mod_toler(ctyp,cmsg);
				break;
			case 7:
				ul_ipv_mod_compare(ctyp,cmsg);
				break;
			case 8:
				ul_ipv_mod_diag(ctyp,cmsg);
				break;
			case 9:
				break;
			case 10:
				ul_ipv_mod_display(ctyp,cmsg);
				break;
			case 11:
				ul_ipv_mod_playback(ctyp,cmsg);
				break;
			case 12:
				ul_ipv_mod_monitor(ctyp,cmsg);
				break;
			case 13:
				ul_ipv_mod_background(ctyp,cmsg,1);
				break;
			case 14:
				ul_ipv_mod_lights(ctyp,cmsg,1);
				break;
			case 15:
				ul_ipv_mod_undo(ctyp,cmsg,1);
				break;
			case 16:
				ul_modals_include(ctyp,cmsg,1);
				break;
/*
......color
*/
			case 17:
   				ul_ipvmod_custom_color(first,ctyp,cmsg);
				first = 0;
				break;
			case 18:
				ul_modals_view (ctyp,cmsg);
				break;
			case 19:
				ul_modals_interface (ctyp,cmsg);
				break;
			}
		}
	} while (stat == UU_SUCCESS || stat == UX_NO_SPACE);
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return (status);
}

/*********************************************************************
**	 I_FUNCTION : ul_ipv_mod_units(ctyp,cmsg)
**			This function sets the NCLIPV modal file units.
**	 PARAMETERS	
**		 INPUT  :  ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_units(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,inum;
	int maxsub=1;
	static char *csub[] = {"UNITS"};
	static char smode[2][64] = {"*INCH","*MM"};
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
		sprintf (serror,"Not a valid UNITS modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Units
*/
	case 0:
		if (ul_modal_toggle(cmsg,smode,2,&inum) != UU_SUCCESS)
			goto bad_parm;
		if (inum == 0) Scnv = 1.;
		else Scnv = 1. / 25.4;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for UNITS modal. /%s/ %s",ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_session(ctyp,cmsg)
**			This function sets the NCLIPV Session modals.
**	 PARAMETERS	
**		 INPUT  :  ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_session (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status,inum;
	UU_REAL rval[2];
	int maxsub=8;
	static char *csub[] = {"MODE","RAPIDCUT_GRID","AUTO_RESET", 
							"TITLE", "POSITION", "SIZE", "MACHINE","MONITOR"};
	static char smode[2][64] = {"*VISICUT","*RAPIDCUT"};
	static char smach[5][64] = {"*AUTO","*MILL","*LATHE","*MILLTURN",
		"*STRINGER"};
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
		sprintf (serror,"Not a valid SESSION modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Mode
*/
	case 0:
		if (ul_modal_toggle(cmsg,smode,2,&LW_mach_mode) != UU_SUCCESS)
			goto bad_parm;
		if (NAUTLTH == 0) LW_mach_mode = LW_MILL;
		break;
/*
.....Rapidcut Grid
*/
	case 1:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 300 || n > 1500) goto bad_parm;
		LW_rv_accy = n;
		break;
/*
.....Auto Reset
*/
	case 2:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_auto_reset) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Title
*/
    case 3:
		strcpy(LW_ipv_title, cmsg);
		break;
/*
.....Position
*/
	case 4:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
#if UU_COMP!=UU_WIN2K
/*		LW_ipv_pos[0] = rval[0] * uw_xw.dev_xmax;*/
/*		LW_ipv_pos[1] = rval[1] * uw_xw.dev_ymax;*/
#else
		LW_ipv_pos[0] = rval[0] * uw_gl.dev_xmax;
		LW_ipv_pos[1] = rval[1] * uw_gl.dev_ymax;
#endif
		break;
/*
......Size
*/
	case 5:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
#if UU_COMP!=UU_WIN2K
/*		LW_ipv_size[0] = rval[0] * uw_xw.dev_xmax;*/
/*		LW_ipv_size[1] = rval[1] * uw_xw.dev_ymax;*/
#else
		LW_ipv_size[0] = rval[0] * uw_gl.dev_xmax;
		LW_ipv_size[1] = rval[1] * uw_gl.dev_ymax;
#endif
		break;
/*
.....Machine
*/
	case 6:
		if (ul_modal_toggle(cmsg,smach,5,&LW_mach_type_flag) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Monitor Panel
*/
	case 7:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_monitor) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for SESSION modal. /%s/ %s",ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_tools(ctyp,cmsg)
**			This function sets the Tools file modals.
**	 PARAMETERS	
**		 INPUT  :  ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_tools(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status;
	UU_REAL r;
	int maxsub=11;
	static char *csub[] = {"TOLERANCE","MAXANG","TRANSLUCENCY","RAPID",
		"MIN_HEIGHT","MAX_HEIGHT","MIN_DIAMETER","FROM","SHANK","EDGE_DISPLAY",
		"EDGE_COLOR"};
	static char clash[2][64] = {"*CUTTER","*HOLDER"};
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
		sprintf (serror,"Not a valid TOOLS modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Cut Tolerance
*/
	case 0:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .001 || r > 2.5) goto bad_parm;
		S_len_exttoint(r,LW_default_tool.toler);
		break;
/*
.....Maximum angular change
*/
	case 1:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .001 || r > 360.) goto bad_parm;
		LW_default_tool.maxang = r;
		break;
/*
.....Cutter Translucency
*/
	case 2:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 100) goto bad_parm;
		LW_default_tool.translucency = n;
		break;
/*
.....Rapid rate
*/
	case 3:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < 0.) goto bad_parm;
		S_len_exttoint(r,LW_default_tool.rapid);
		break;
/*
.....Minimum tool height
*/
	case 4:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .0) goto bad_parm;
		S_len_exttoint(r,LW_tool_limit[0]);
		break;
/*
.....Maximum tool height
*/
	case 5:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .0) goto bad_parm;
		S_len_exttoint(r,LW_tool_limit[1]);
		break;
/*
.....Minimum tool diameter
*/
	case 6:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .0) goto bad_parm;
		S_len_exttoint(r,LW_tool_limit[2]);
		break;
/*
.....Treat next position as FROM
*/
	case 7:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_tool_from) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Treat SHANK clashes as CUTTER/HOLDER
*/
	case 8:
		if (ul_modal_toggle(cmsg,clash,2,&LW_default_tool.shank_clash) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Edge Display
*/
	case 9:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_default_tool.edge) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Edge Color
*/
	case 10:
		if (ul_modal_color(cmsg, &LW_default_tool.edge_color, scolor, 1)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_default_tool.edge_color<0)&&(LW_default_tool.edge_color!=-1))
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for TOOLS modal. /%s/ %s",ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_stock(ctyp,cmsg,which)
**			This function sets the NCLIPV Stock & Fixture modals.
**	 PARAMETERS	
**		 INPUT  :  ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**			which = 0 = Stock, 1 = Fixture.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_stock(ctyp,cmsg,which)
char *ctyp,*cmsg;
int which;
{
	char serror[200];
	int i,n,status;
	int maxsub=19;
	UU_REAL r;
	static char *csub[] = {"COLOR", "VISIBLE", "ACTIVE", "TRANSLUCENCY",
		"TOLERANCE", "STL_FORMAT", "EXPANSION", "UNITS", "ID", "STL_STOP",
		"STL_INACTIVE", "AXES", "AXES_COLOR", "EDGE_DISPLAY", "EDGE_COLOR",
		"IMPORTANT","STL_SKIP_ERROR","ATTR_RESET","SESS_DELETE"};
	static char *stype[] = {"STOCK","FIXTURE"};
	static char fmt[2][64] = {"*ASCII","*BINARY"};
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
		sprintf (serror,"Not a valid %s modal.  /%s/ %s",
			stype[which],ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Color
*/
	case 0:
		if (ul_modal_color(cmsg, &LW_stock_default[which].color, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (LW_stock_default[which].color<0)
			goto bad_parm;
		break;
/*
.....Visible
*/
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_stock_default[which].visible) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Active
*/
	case 2:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_stock_default[which].active) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Translucency
*/
	case 3:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 100) goto bad_parm;
		LW_stock_default[which].translucency = n;
		break;
/*
.....Tolerance
*/
	case 4:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .00001 || r > .5) goto bad_parm;
		S_len_exttoint(r,LW_stock_default[which].toler);
		break;
/*
.....STL file format
*/
	case 5:
		if (ul_modal_toggle(cmsg,fmt,2,&LW_stl_format) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Expansion
*/
	case 6:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .0 || r > 10000.) goto bad_parm;
		LW_box_expansion = r;
		break;
/*
.....Units
*/
	case 7:
		if (ul_modal_toggle(cmsg,units,2,&LW_stl_units) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....ID
*/
	case 8:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1) goto bad_parm;
		LW_stock_idn[which] = n;
		break;
/*
.....Stop
*/
	case 9:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_stl_flag[0]) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Inactive
*/
	case 10:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_stl_flag[1]) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Axes
*/
	case 11:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_stock_default[which].axes) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Axes Color
*/
	case 12:
		if (ul_modal_color(cmsg, &LW_stock_default[which].axes_color, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (LW_stock_default[which].axes_color<0)
			goto bad_parm;
		break;
/*
.....Edge Display
*/
	case 13:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_stock_default[which].edge) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Edge Color
*/
	case 14:
		if (ul_modal_color(cmsg, &LW_stock_default[which].edge_color, scolor, 1)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_stock_default[which].edge_color<0)&&(LW_stock_default[which].edge_color!=-1))
			goto bad_parm;
		break;
/*
.....Important
*/
	case 15:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_stock_default[which].important) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Skip Error
*/
	case 16:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_stl_flag[2]) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Reset Attributes
*/
	case 17:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_reset_attr[which]) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Delete on Reset Session
*/
	case 18:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_delete_stocks) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for %s modal. /%s/ %s",stype[which],ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_colors(ctyp,cmsg)
**			This function sets the NCLIPV Motion Color modals.
**	 PARAMETERS	
**		 INPUT  :  ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_colors(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status;
	int maxsub=11;
	static char *csub[] = {"CUT", "CUTTER", "HOLDER", "GOUGE_FIXT",
		"GOUGE_HOLD", "GOUGE_RAP", "HIGHLIGHT", "INITIAL",
		"USE_STOCK", "USE_FIXT", "SHANK"};
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
		sprintf (serror,"Not a valid COLORS modal.  /%s/ %s",ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Cut color
*/
	case 0:
		if (ul_modal_color(cmsg, &LW_default_tool.cut_color, scolor, 3)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_default_tool.cut_color<0)&&(LW_default_tool.cut_color!=-3))
			goto bad_parm;
		else if (LW_default_tool.cut_color<0)
			LW_default_tool.cut_color = -1;
		break;
/*
.....Cutter color
*/
	case 1:
		if (ul_modal_color(cmsg, &LW_default_tool.color, scolor, 3)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_default_tool.color<0)&&(LW_default_tool.color!=-3))
			goto bad_parm;
		else if (LW_default_tool.color<0)
			LW_default_tool.color = -1;
		break;
/*
.....Holder color
*/
	case 2:
		if (ul_modal_color(cmsg, &LW_default_tool.hold_color, scolor, 3)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_default_tool.hold_color<0)&&(LW_default_tool.hold_color!=-3))
			goto bad_parm;
		else if (LW_default_tool.hold_color<0)
			LW_default_tool.hold_color = -1;
		break;
/*
.....Fixture gouge color
*/
	case 3:
		if (ul_modal_color(cmsg, &LW_clash_material.fixture, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (LW_clash_material.fixture<0)
			goto bad_parm;
		break;
/*
.....Holder gouge color
*/
	case 4:
		if (ul_modal_color(cmsg, &LW_clash_material.holder, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (LW_clash_material.holder<0)
			goto bad_parm;
		break;
/*
.....Rapid gouge color
*/
	case 5:
		if (ul_modal_color(cmsg, &LW_clash_material.rapid, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (LW_clash_material.rapid<0)
			goto bad_parm;
		break;
/*
.....Highlight color
*/
	case 6:
		if (ul_modal_color(cmsg, &LW_highlight_color, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (LW_highlight_color<0)
			goto bad_parm;
		break;
/*
.....Initial color
*/
	case 7:
		if (ul_modal_color(cmsg, &LW_default_tool.initial, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (LW_default_tool.initial<0)
			goto bad_parm;
		break;
/*
.....Use stock colors
*/
	case 8:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_default_tool.use_stock[0]) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Use fixture colors
*/
	case 9:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_default_tool.use_stock[1]) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Shank color
*/
	case 10:
		if (ul_modal_color(cmsg, &LW_default_tool.shank_color, scolor, 3)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_default_tool.shank_color<0)&&(LW_default_tool.shank_color!=-3))
			goto bad_parm;
		else if (LW_default_tool.shank_color<0)
			LW_default_tool.shank_color = -1;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for COLORS modal. /%s/ %s",ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_toler(ctyp,cmsg)
**			This function sets the Tolerances file modals.
**	 PARAMETERS	
**		 INPUT  :  ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_toler(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status;
	UU_REAL r;
	int maxsub=2;
	static char *csub[] = {"GEOMETRY","STL"};
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
		sprintf (serror,"Not a valid TOLERANCES modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Geometry Tolerance
*/
	case 0:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .001 || r > 2.5) goto bad_parm;
		S_len_exttoint(r,LW_geom_toler);
		break;
/*
.....STL Tolerance
*/
	case 1:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .001 || r > 2.5) goto bad_parm;
		S_len_exttoint(r,LW_stl_toler);
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for TOLERANCES modal. /%s/ %s",ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_compare(ctyp,cmsg)
**			This function sets the NCLIPV Comparison modals.
**	 PARAMETERS	
**		 INPUT  :  ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_compare(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	char *p,*c,sbuf[80],*strchr();
	int i,n,status;
	int maxsub=18;
	UU_REAL r,ary[64];
	static char *csub[] = {"COMPARE", "SEL_COLOR", "UNITS", "TYPE",
		"TRANSLUCENCY", "GRID", "TOLERANCE", "ANGULAR", "LINEAR", "GRID_ONLY",
		"NEG_TOLER", "AUTO_TOLER", "POS_TOLER", "NEG_COLOR", "TOL_COLOR",
		"POS_COLOR","VOLUME","ANALYZE"};
	static char scomp[3][64] = {"*SURFACES","*STL_FILE","*SHAPE"};
	static char stype[5][64] = {"*REPORT","*VISUAL","*UNDERCUT","*OVERCUT",
		"*BOTH"};
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
		sprintf (serror,"Not a valid Comparison modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Comparison entity
*/
	case 0:
		if (ul_modal_toggle(cmsg,scomp,3,&LW_compare.type) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Selection color
*/
	case 1:
		if (ul_modal_color(cmsg, &LW_compare.hilite, scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (LW_compare.hilite<0)
			goto bad_parm;
		break;
/*
.....Units
*/
	case 2:
		if (ul_modal_toggle(cmsg,units,2,&LW_compare.units) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Comparison type
*/
	case 3:
		if (ul_modal_toggle(cmsg,stype,5,&LW_compare.vis) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Translucency
*/
	case 4:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 100) goto bad_parm;
		LW_compare.translucency = n;
		break;
/*
.....Surface Grid
*/
	case 5:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 100) goto bad_parm;
		LW_compare.grid = n;
		break;
/*
.....Tolerance
*/
	case 6:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .0001 || r > .1) goto bad_parm;
		S_len_exttoint(r,LW_compare.sftol);
		break;
/*
.....Angular deviation
*/
	case 7:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .1 || r > 90.) goto bad_parm;
		LW_compare.maxang = r;
		break;
/*
.....Linear deviation
*/
	case 8:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .001 || r > 10000.) goto bad_parm;
		S_len_exttoint(r,LW_compare.maxdis);
		break;
/*
.....Grid only
*/
	case 9:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_compare.grid_flag) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Negative Tolerances
*/
	case 10:
		n = strlen(cmsg);
		ul_strip_blanks(cmsg,&n);
		if (ul_to_reals(&ary,&n,4,cmsg) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 4) goto bad_parm;
		for (i=0;i<n;i++)
		{
			if (ary[i]< -10000. || ary[i] >= 0.) goto bad_parm;
			if (i != 0 && ary[i] < ary[i-1]) goto bad_parm;
			S_len_exttoint(ary[i],LW_compare.toler[i]);
		}
		break;
/*
.....Auto Tolerance
*/
	case 11:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .001 || r > 10000.) goto bad_parm;
		S_len_exttoint(r,LW_compare.auto_toler);
		break;
/*
.....Positive Tolerances
*/
	case 12:
		n = strlen(cmsg);
		ul_strip_blanks(cmsg,&n);
		if (ul_to_reals(&ary,&n,4,cmsg) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 4) goto bad_parm;
		for (i=0;i<n;i++)
		{
			if (ary[i] <= 0. || ary[i] > 10000.) goto bad_parm;
			if (i != 0 && ary[i] < ary[i-1]) goto bad_parm;
			S_len_exttoint(ary[i],LW_compare.toler[i+4]);
		}
		break;
/*
.....Negative colors
*/
	case 13:
		n = strlen(cmsg);
		ul_strip_blanks(cmsg,&n);
		c = cmsg;
		i = 0;
		do
		{
			p = strchr(c,',');
			if (p == UU_NULL) strcpy(sbuf,c);
			else
			{
				*p = '\0';
				strcpy(sbuf,c);
				*p = ',';
			}
			if (ul_modal_color(sbuf, &LW_compare.color[i], scolor, 0)
				!= UU_SUCCESS) goto bad_parm;
			if (LW_compare.color[i]<0)
				goto bad_parm;

			c = p+1;
			i++;
		} while (p != UU_NULL);
		break;
/*
.....In tolerance color
*/
	case 14:
		if (ul_modal_color(cmsg, &LW_compare.color[4], scolor, 0)
			!= UU_SUCCESS) goto bad_parm;
		if (LW_compare.color[4]<0)
			goto bad_parm;
		break;
/*
.....Positive colors
*/
	case 15:
		n = strlen(cmsg);
		ul_strip_blanks(cmsg,&n);
		c = cmsg;
		i = 0;
		do
		{
			p = strchr(c,',');
			if (p == UU_NULL) strcpy(sbuf,c);
			else
			{
				*p = '\0';
				strcpy(sbuf,c);
				*p = ',';
			}
			if (ul_modal_color(sbuf, &LW_compare.color[i+5], scolor, 0)
				!= UU_SUCCESS) goto bad_parm;
			if (LW_compare.color[i+5]<0)
				goto bad_parm;
			c = p+1;
			i++;
		} while (p != UU_NULL);
		break;
/*
.....Minimum volume for comparison
*/
	case 16:
		if (ul_to_reals(&r,&n,1,cmsg) != UU_SUCCESS) goto bad_parm;
		if (r < .0 || r > 10000.) goto bad_parm;
		S_len_exttoint(r,LW_compare.minvol);
		break;
/*
.....Auto Analyze
*/
	case 17:
/*		if (ul_modal_toggle(cmsg,yesno,2,&LW_compare.analyze) != UU_SUCCESS)
			goto bad_parm;*/
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for Compare modal. /%s/ %s",ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_diag(ctyp,cmsg)
**			This function sets the NCLIPV Diagnostic modals.
**	 PARAMETERS	
**		 INPUT  :  ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_diag(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	char *strchr();
	int i,status;
	int maxsub=35;
	static char *csub[] = {
		"TOOL_RAPID_STOP", "TOOL_FIXTURE_STOP", "TOOL_HEAD_STOP",
		"TOOL_AXIS_STOP",
		"HOLDER_STOCK_STOP", "HOLDER_FIXTURE_STOP", "HOLDER_HEAD_STOP",
		"HOLDER_AXIS_STOP",
		"HEAD_STOCK_STOP", "HEAD_FIXTURE_STOP", "HEAD_HEAD_STOP",
		"HEAD_AXIS_STOP",
		"AXIS_STOCK_STOP", "AXIS_FIXTURE_STOP", "AXIS_AXIS_STOP",
		"POST_ERROR_STOP",
		"TOOL_RAPID_LOG", "TOOL_FIXTURE_LOG", "TOOL_HEAD_LOG",
		"TOOL_AXIS_LOG",
		"HOLDER_STOCK_LOG", "HOLDER_FIXTURE_LOG", "HOLDER_HEAD_LOG",
		"HOLDER_AXIS_LOG",
		"HEAD_STOCK_LOG", "HEAD_FIXTURE_LOG", "HEAD_HEAD_LOG",
		"HEAD_AXIS_LOG",
		"AXIS_STOCK_LOG", "AXIS_FIXTURE_LOG", "AXIS_AXIS_LOG",
		"POST_ERROR_LOG",
		"FILE", "RESET", "CLASH_COLOR"};
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
		sprintf (serror,"Not a valid Diagnostics modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
/*
.....Stop on clash flags
*/
	if (i < 16)
	{
		if (ul_modal_toggle(cmsg,yesno,2,&LW_clash_stop[i]) != UU_SUCCESS)
			goto bad_parm;
		LW_default_stop[i] = LW_clash_stop[i];
	}
/*
.....Log clash flags
*/
	else if (i < 32)
	{
		if (ul_modal_toggle(cmsg,yesno,2,&LW_clash_log[i-16]) != UU_SUCCESS)
			goto bad_parm;
		LW_default_log[i] = LW_clash_log[i];
	}
/*
.....FILE
*/
	else if (i == 32)
		strcpy(LW_diag_file,cmsg);
/*
.....RESET
*/
	else if (i == 33)
	{
		if (ul_modal_toggle(cmsg,yesno,2,&LW_reset_log) != UU_SUCCESS)
			goto bad_parm;
	}
/*
.....CLASH_COLOR
*/
	else if (i == 34)
	{
		if (ul_modal_color(cmsg, &LW_clash_color, scolor, 1)
			!= UU_SUCCESS) goto bad_parm;
	}
/*
.....Unrecognized command
*/
	else
		goto bad_parm;
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for Diagnostics modal. /%s/ %s",ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_display(ctyp,cmsg)
**			This function sets the NCLIPV Display modals.
**	 PARAMETERS	
**		 INPUT  :  ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_display(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,n;
	int maxsub=8;
	static char *csub[] = {"AXES","SWAP_SCREEN","MINIMIZE","DISPLAY","BUFFER",
		"AUTO_HIDE","TRANSLUCENCY","EDGE_DISPLAY"};
	static char cmode[3][64] = {"*SHADED","*WIRE","*HIDDEN"};
	static char cbuffer[2][64] = {"*SWAP","*PIXEL"};
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
		sprintf (serror,"Not a valid Display modal.  /%s/ %s",ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Display Axes
*/
	case 0:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_display_prop.axes) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Swap Screen
*/
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_display_prop.swap) !=
			UU_SUCCESS)
			goto bad_parm;
/*
.....if it is standalone NCLIPV, LW_display_prop.swap is always false
*/
		if (LW_nclipv == LW_STANDALONE)
			LW_display_prop.swap =  UU_FALSE;
		break;
/*
.....Minimize Window
*/
	case 2:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_display_prop.minimize) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Display mode
*/
	case 3:
		if (ul_modal_toggle(cmsg,cmode,3,&LW_display_prop.mode) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Copy buffer
*/
	case 4:
		if (ul_modal_toggle(cmsg,cbuffer,3,&LW_display_prop.buf) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Auto-hide solids
*/
	case 5:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_display_prop.hide_auto) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Auto-hide Translucency
*/
	case 6:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 100) goto bad_parm;
		LW_display_prop.hide_lucency = n;
		break;
/*
.....Auto-hide edge display
*/
	case 7:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_display_prop.hide_edge) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for Display modal. /%s/ %s",ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_playback(ctyp,cmsg)
**			This function sets the NCLIPV Playback modals.
**	 PARAMETERS	
**		 INPUT  :
**       ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_playback(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,n,status;
	int maxsub=8;
	static char *csub[] = {"START","MODE","SPEED","STEPS","DYNAMIC","CYCLE",
		"ANALYZE","RAPID"};
	static char begcur[2][64] = {"*BEGIN","*CURRENT"};
	static char cmode[3][64] = {"*GO","*STEP","*TOOL"};
	static char cdyn[2][64] = {"*CUTTER","*PART"};
	static char cyc[4][64] = {"*OFF","*SIMPLE","*DETAIL","*LATHE"};
	static char analy[3][64] = {"*OFF","*FEED","*INTERP"};
	static char rapid[5][64] = {"*OFF","*XAXIS","*YAXIS","*ZAXIS","*TLAXIS"};
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
		sprintf (serror,"Not a valid Playback modal.  /%s/ %s",ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Start
*/
	case 0:
		if (ul_modal_toggle(cmsg,begcur,2,&LW_play_modal[0]) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Mode
*/
	case 1:
		if (ul_modal_toggle(cmsg,cmode,3,&LW_play_modal[2]) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Speed
*/
	case 2:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > 100) goto bad_parm;
		LW_play_modal[1] = n;
		break;
/*
.....Steps
*/
	case 3:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1) goto bad_parm;
		LW_play_modal[3] = n;
		break;
/*
.....Dynamic
*/
	case 4:
		if (ul_modal_toggle(cmsg,cdyn,2,&LW_play_modal[5]) !=
			UU_SUCCESS)
			goto bad_parm;
			LW_play_modal[5]++;
		break;
/*
.....Cycle Display
*/
	case 5:
		if (ul_modal_toggle(cmsg,cyc,4,&LW_play_modal[14]) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Analyzation
*/
	case 6:
		if (ul_modal_toggle(cmsg,analy,3,&LW_play_modal[15]) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Rapid
*/
	case 7:
		if (ul_modal_toggle(cmsg,rapid,5,&LW_play_modal[17]) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for Playback modal. /%s/ %s",ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_monitor(ctyp,cmsg)
**			This function sets the NCLIPV Monitor modals.
**	 PARAMETERS	
**		 INPUT  :
**       ctyp = Modal begin defined.
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_monitor(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status;
	int maxsub=N_IPVMON_FLD;
	static char *csub[] = {"ISN","MODE","MACHTYP","TOOL_END","TOOL_AXIS",
		"LINEAR","ROTARY","HEAD2","HEAD3","HEAD4","LOADTL","DIAMETER","RADIUS",
		"HEIGHT","CUTCOM","FEDRAT","MOVE_TIME","SPINDL","COOLNT","TOTAL_TIME",
		"PROGRESS","DOCK"};
	static char cdock[3][64] = {"*OFF","*LEFT","*RIGHT"};
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
		sprintf (serror,"Not a valid Display modal.  /%s/ %s",ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Dock
*/
	case IPVMON_DOCK:
		if (ul_modal_toggle(cmsg,cdock,3,&LW_monitor_field[IPVMON_DOCK]) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....All other fields
*/
	default:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_monitor_field[i]) !=
			UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for Monitor modal. /%s/ %s",ctyp,cmsg);
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
**	 I_FUNCTION : ul_ipv_mod_background(ctyp,cmsg)
**			This function sets the NCLIPV Background image modals.
**	 PARAMETERS	
**		 INPUT  :
**        ctyp = Modal begin defined.
**			 cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_background (ctyp,cmsg)
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
		if (ul_modal_toggle(cmsg,cshade,4,&LW_display_prop.shader) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Solid Color
*/
	case 1:
		if (ul_modal_color(cmsg, &LW_display_prop.bgcolor, scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_display_prop.bgcolor<0)&&(LW_display_prop.bgcolor!=-2))
			goto bad_parm;
		else if (LW_display_prop.bgcolor<0)
			LW_display_prop.bgcolor = -1;
		break;
/*
.....Solid RGB
*/
	case 2:
		if (ul_rgb_in(cmsg,LW_display_prop.bgrgb) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....Graduated Top Color
*/
	case 3:
		if (ul_modal_color(cmsg, &LW_display_prop.grad[0], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_display_prop.grad[0]<0)&&(LW_display_prop.grad[0]!=-2))
			goto bad_parm;
		else if (LW_display_prop.grad[0]<0)
			LW_display_prop.grad[0] = -1;
		break;
/*
.....Graduated Bottom Color
*/
	case 4:
		if (ul_modal_color(cmsg, &LW_display_prop.grad[1], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_display_prop.grad[1]<0)&&(LW_display_prop.grad[1]!=-2))
			goto bad_parm;
		else if (LW_display_prop.grad[1]<0)
			LW_display_prop.grad[1] = -1;
		break;
/*
.....Graduated Top RGB
*/
	case 5:
		if (ul_rgb_in(cmsg,LW_display_prop.grgb[0]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....Graduated Bottom RGB
*/
	case 6:
		if (ul_rgb_in(cmsg,LW_display_prop.grgb[1]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....4-Corner Top-Left Color
*/
	case 7:
		if (ul_modal_color(cmsg, &LW_display_prop.fcolor[0], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_display_prop.fcolor[0]<0)&&(LW_display_prop.fcolor[0]!=-2))
			goto bad_parm;
		else if (LW_display_prop.fcolor[0]<0)
			LW_display_prop.fcolor[0] = -1;
		break;
/*
.....4-Corner Top-Right Color
*/
	case 8:
		if (ul_modal_color(cmsg, &LW_display_prop.fcolor[1], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_display_prop.fcolor[1]<0)&&(LW_display_prop.fcolor[1]!=-2))
			goto bad_parm;
		else if (LW_display_prop.fcolor[1]<0)
			LW_display_prop.fcolor[1] = -1;
		break;
/*
.....4-Corner Lower-Left Color
*/
	case 9:
		if (ul_modal_color(cmsg, &LW_display_prop.fcolor[2], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_display_prop.fcolor[2]<0)&&(LW_display_prop.fcolor[2]!=-2))
			goto bad_parm;
		else if (LW_display_prop.fcolor[2]<0)
			LW_display_prop.fcolor[2] = -1;
		break;
/*
.....4-Corner Lower-Right Color
*/
	case 10:
		if (ul_modal_color(cmsg, &LW_display_prop.fcolor[3], scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_display_prop.fcolor[3]<0)&&(LW_display_prop.fcolor[3]!=-2))
			goto bad_parm;
		else if (LW_display_prop.fcolor[3]<0)
			LW_display_prop.fcolor[3] = -1;
		break;
/*
.....4-Corner Top-Left RGB
*/
	case 11:
		if (ul_rgb_in(cmsg,LW_display_prop.frgb[0]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....4-Corner Top-Right RGB
*/
	case 12:
		if (ul_rgb_in(cmsg,LW_display_prop.frgb[1]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....4-Corner Lower-Left RGB
*/
	case 13:
		if (ul_rgb_in(cmsg,LW_display_prop.frgb[2]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....4-Corner Lower-Right RGB
*/
	case 14:
		if (ul_rgb_in(cmsg,LW_display_prop.frgb[3]) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....Image file
*/
	case 15:
		strcpy(LW_display_prop.bgfile,cmsg);
		break;
/*
.....Rotate
*/
	case 16:
		if (ul_modal_toggle(cmsg,crot,4,&LW_display_prop.rotate) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Stretch
*/
	case 17:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_display_prop.stretch) != UU_SUCCESS)
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

/*********************************************************************
**	 I_FUNCTION : ul_ipv_mod_lights(ctyp,cmsg)
**			This function sets the NCLIPV Lights modals.
**	 PARAMETERS	
**		 INPUT  :
**        ctyp = Modal begin defined.
**			 cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_lights (ctyp,cmsg)
char *ctyp,*cmsg;
{
	static int ilight=0;
	char serror[200];
	int i,status,n;
	UU_REAL rval[3];
	int maxsub=8;
	static char *csub[] = {"LIGHT","ACTIVE","TYPE","COLOR","RGB",
		"INTENSITY","POSITION","DIRECTION"};
	static char ctype[4][64] = {"*AMBIENT","*DISTANT","*EYE","*POINT"};
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
		sprintf (serror,"Not a valid LIGHTS modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Light
*/
	case 0:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 1 || n > LW_MAX_LIGHT) goto bad_parm;
		ilight = n - 1;
		break;
/*
.....Active
*/
	case 1:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_lights[ilight].active) !=
			UU_SUCCESS) goto bad_parm;
		break;
/*
.....Type
*/
	case 2:
		if (ul_modal_toggle(cmsg,ctype,4,&LW_lights[ilight].type) !=
			UU_SUCCESS) goto bad_parm;
		break;
/*
.....Color
*/
	case 3:
		if (ul_modal_color(cmsg, &LW_lights[ilight].color, scolor, 2)
			!= UU_SUCCESS) goto bad_parm;
		if ((LW_lights[ilight].color<0)&&(LW_lights[ilight].color!=-2))
			goto bad_parm;
		else if (LW_lights[ilight].color<0)
			LW_lights[ilight].color = -1;
		break;
/*
.....RGB
*/
	case 4:
		if (ul_rgb_in(cmsg,LW_lights[ilight].rgb) != UU_SUCCESS) goto bad_parm;
		break;
/*
.....Intensity
*/
	case 5:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 100) goto bad_parm;
		LW_lights[ilight].intensity = n;
		break;
/*
.....Position
*/
	case 6:
		if ((ul_to_reals(rval,&n,3,cmsg) != UU_SUCCESS) || n != 3)
			goto bad_parm;
		S_len_exttoint(rval[0],LW_lights[ilight].position[0]);
		S_len_exttoint(rval[1],LW_lights[ilight].position[1]);
		S_len_exttoint(rval[2],LW_lights[ilight].position[2]);
		break;
/*
.....Direction
*/
	case 7:
		if ((ul_to_reals(rval,&n,3,cmsg) != UU_SUCCESS) || n != 3)
			goto bad_parm;
		um_vctovc(rval,LW_lights[ilight].direction);
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

/*********************************************************************
**	 I_FUNCTION : ul_ipv_mod_undo(ctyp,cmsg)
**			This function sets the NCLIPV Lights modals.
**	 PARAMETERS	
**		 INPUT  :
**        ctyp = Modal begin defined.
**			 cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
ul_ipv_mod_undo(ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,n;
	int maxsub=3;
	static char *csub[] = {"ACTIVE","SIZE","FIXTURE"};
/*
.....Get modal to define
*/
	status = UU_SUCCESS;
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (serror,"Not a valid UNDO modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Active
*/
	case 0:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_mot_stack_active) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....Size
*/
	case 1:
		if (ul_to_number(cmsg,&n) != UU_SUCCESS) goto bad_parm;
		if (n < 0 || n > 1000000) goto bad_parm;
		LW_mot_stack_size = n;
		break;
/*
.....Fixture
*/
	case 2:
		if (ul_modal_toggle(cmsg,yesno,2,&LW_mot_stack_fixture) != UU_SUCCESS)
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for UNDO modal. /%s/ %s",ctyp,cmsg);
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
**	 E_FUNCTION : ul_rgb_in(cmsg,gval)
**			Converts a text string to RGB values.
**	 PARAMETERS	
**		 INPUT  :
**        cmsg = Text string to convert.
**		 OUTPUT :  none.
**			 gval = RGB values between 0 and 1.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int ul_rgb_in(cmsg,gval)
char *cmsg;
UM_vector gval;
{
	int i,status,inum;
	UM_vector rval;
/*
.....Convert text string to RGB values
*/
	status = UU_SUCCESS;
	if ((ul_to_reals(rval,&inum,3,cmsg) != UU_SUCCESS) || inum != 3)
	{
		status = UU_FAILURE;
	}
	else
	{
		for (i=0;i<3;i++)
		{
			if (rval[i] < 0.) rval[i] = 0.;
			if (rval[i] > 1.) rval[i] = 1.;
		}
		um_vctovc(rval,gval);
	}
	return(UU_SUCCESS);
}
