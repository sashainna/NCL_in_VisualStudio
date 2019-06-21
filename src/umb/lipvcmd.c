/*********************************************************************
**    NAME         :  lipvcmd.c
**       CONTAINS:
**			ul_ipv_parse_pprint()
**       ul_ipv_parse_modals()
**       ul_ipv_parse_offset()
**       ul_ipv_parse_positn()
**       ul_ipv_parse_print_screen()
**       ul_ipv_parse_session()
**       ul_ipv_parse_spindle()
**       ul_ipv_parse_stock()
**       ul_ipv_parse_tool()
**       ul_ipv_parse_toolpn()
**       ul_ipv_parse_view()
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvcmd.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:11
*********************************************************************/

#include "usysdef.h"
#include "xenv1.h"
#include "lumb.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"
#include "lipvstack.h"
#include "mdcpln.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclfile.h"
#include "nclmplay.h"
#include "udfdata.h"
#include "view.h"
#include <ctype.h>
#include "lcom.h"

extern int NAUTLTH;

static int S_parse_idns();
static int S_parse_color();
static void S_default_filename();
static void S_error_msg();

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_pprint(isn,cstr,exefl,iclw,rclw,nclw)
**       Parses and executes a PPRINT IPV statement (command).
**    PARAMETERS   
**       INPUT  : 
**          isn       = Input ISN number.
**          cstr      = Text of PPRINT statement.
**          exefl     = UU_TRUE = processing NCLIPV playback file,
**                      process all commands.  UU_FALSE = Process only
**                      CUTTER, SHANK, and HOLDER commands.
**       OUTPUT :
**          iclw      = Cl record information if PPRINT IPV command.
**                      generates another command to be processed
**                      by the clfile reading routine (CUTTER, SHANK,etc.).
**          rclw      = Cl record real values if PPRINT IPV command
**                      generates another command.
**          nclw      = Number of values in 'rclw' if this routine
**                      returns a clfile record to be processed.
**                      Otherwise 0 if PPRINT IPV command is self contained.
**          mdid      = PPRINT IPV SPINDLE command processed.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_parse_pprint(isn,cstr,exefl,iclw,rclw,nclw,mdid)
int isn;
char *cstr;
UU_LOGICAL exefl;
int *iclw;
UU_REAL *rclw;
int *nclw;
int *mdid;
{
	int nc,ispin,status;
	UU_REAL rval;
	char buf[80],buf1[80],tstr[1024],lstr1[80],lstr2[80],errstr[80];
	char errlab[10];
/*
.....Search for IPV
*/
	*nclw = 0;
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
	buf[0] = '\0';
	errlab[0] = '\0';
	sscanf(tstr,"%s",buf);
	if (strcmp(buf,"IPV") == 0)
	{
		buf1[0] = '\0';
		nc = sscanf(tstr,"%s%s%d",buf,buf1,&ispin);
/*
.....PPRINT IPV CUTTER
*/
		if (strcmp(buf1,"CUTTER") == 0)
		{
			status = ul_ipv_parse_cutter(cstr,iclw,rclw,nclw,errstr);
			if (status != UU_SUCCESS) goto failed;
			goto done;
		}
/*
.....PPRINT IPV SHANK/HOLDER
*/
		else if (strcmp(buf1,"SHANK") == 0 || strcmp(buf1,"HOLDER") == 0)
		{
			status = ul_ipv_parse_holder(cstr,iclw,rclw,nclw,errstr);
			if (status != UU_SUCCESS) goto failed;
			goto done;
		}
/*
.....PPRINT IPV SPINDLE
*/
		else if (strcmp(buf1,"SPINDLE") == 0)
		{
			status = UU_SUCCESS;
			{
				if (exefl)
				{
					if (LW_mach_simul)
						status = ul_ipv_parse_spindle(cstr,errlab,errstr,mdid);
				}
				else
					*mdid = 1;
				if (status != UU_SUCCESS) goto failed;
			}
			goto done;
		}
/*
.....PPRINT IPV MODALS
*/
		else if (strcmp(buf1,"MODALS") == 0)
		{
			status = ul_ipv_parse_modals(cstr,exefl,errlab,errstr);
			if (status != UU_SUCCESS) goto failed;
			goto done;
		}
/*
.....PPRINT IPV TOOL
*/
		else if (strcmp(buf1,"TOOL") == 0)
		{
			status = ul_ipv_parse_tool(cstr,errlab,errstr);
			if (status != UU_SUCCESS) goto failed;
			goto done;
		}
/*
.....The rest of the commands are only
.....processed during NCLIPV playback
*/
		if (!exefl) goto done;
/*
.....PPRINT IPV DNTCUT
*/
		if (LW_mach_desc.type == -1 && nc == 2 && strcmp(buf1,"DNTCUT") == 0)
		{
			LW_dntcut = UU_TRUE;
		}
/*
.....PPRINT IPV VIEW
*/
		else if (strcmp(buf1,"VIEW") == 0)
		{
			status = ul_ipv_parse_view(cstr,errlab,errstr);
			if (status != UU_SUCCESS) goto failed;
		}
/*
.....PPRINT IPV PRINT_SCREEN
*/
		else if (strcmp(buf1,"PRINT_SCREEN") == 0)
		{
			status = ul_ipv_parse_print_screen(cstr,errlab,errstr);
			if (status != UU_SUCCESS) goto failed;
		}
/*
.....PPRINT IPV SESSION
*/
		else if (strcmp(buf1,"SESSION") == 0)
		{
			status = ul_ipv_parse_session(cstr,errlab,errstr);
			if (status != UU_SUCCESS) goto failed;
		}
/*
.....PPRINT IPV STOCK/FIXTUR
*/
		else if (strcmp(buf1,"STOCK") == 0 || strcmp(buf1,"FIXTUR") == 0)
		{
			status = ul_ipv_parse_stock(cstr,UU_TRUE,errlab,errstr);
			if (status != UU_SUCCESS) goto failed;
		}
/*
.....PPRINT IPV TOOLPN
*/
		else if (strcmp(buf1,"TOOLPN") == 0)
		{
			status = ul_ipv_parse_toolpn(cstr,UU_TRUE,errlab,errstr);
			if (status != UU_SUCCESS) goto failed;
		}
/*
.....PPRINT IPV POSITN
*/
		else if (strcmp(buf1,"POSITN") == 0)
		{
			status = ul_ipv_parse_positn(cstr,UU_FALSE,UU_TRUE,errlab,errstr);
			if (status != UU_SUCCESS) goto failed;
		}
/*
.....PPRINT IPV OFFSET
*/
		else if (strcmp(buf1,"OFFSET") == 0)
		{
			status = ul_ipv_parse_positn(cstr,UU_TRUE,UU_TRUE,errlab,errstr);
/*
........Not a direct offset type command
........parse for offset file instead
*/
			if (status != UU_SUCCESS)
			{
				status = ul_ipv_parse_offset(cstr,UU_TRUE,errlab,errstr);
				if (status != UU_SUCCESS) goto failed;
			}
		}
/*
......Unrecognized command
*/
		else
		{
			strcpy(errstr,buf1);
			goto failed;
		}
	}
	goto done;
/*
.....Error processing PPRINT IPV command
*/
failed:;
	if (exefl)
	{
		S_error_msg(errlab,errstr,lstr1);
		strcpy(lstr2,"PPRINT");
		strncat(lstr2,cstr,66);
		lstr2[72] = '\0';
		ul_ipv_diag_error(isn,lstr1,strlen(lstr1),lstr2,strlen(lstr2));
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_modals(cstr,exefl,errlab,errstr)
**       Parses and executes a PPRINT IPV MODALS statement (command).
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**          exefl     = UU_TRUE = processing NCLIPV playback file,
**                      process all commands.  UU_FALSE = Process only
**                      TOOL,SHANK commands.
**       OUTPUT :  
**          errlab    = Error message label.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_modals(cstr,exefl,errlab,errstr)
UU_LOGICAL exefl;
char *cstr,*errlab,*errstr;
{
	int nc,ncv,status,i,j,k,inum,ifxt,isav;
	UU_LOGICAL ifl,hide_flag,tool_flag;
	UU_REAL rvals[2];
	char tstr[1024],cbuf[24][20];
	char buf[100],cstk[100],ctyp[100];
	static char cols[3][96]={"DEFAULT","OFF","AUTO"};
	UN_cutter_list *cpt;
/*
.....Initialize routine
*/
	hide_flag = UU_FALSE;
	tool_flag = UU_FALSE;
/*
.....Determine if a tool is defined
*/
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	if (cpt != UU_NULL) tool_flag = UU_TRUE;
/*
.....Convert string to upper case
*/
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
	nc = ul_cut_string(tstr,1024);
	tstr[nc] = '\0';
/*
.....Parse string
*/
	nc = sscanf(tstr,"%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",buf,cstk,
		ctyp,cbuf[0],cbuf[1],cbuf[2],cbuf[3],cbuf[4],cbuf[5],cbuf[6],cbuf[7],
		cbuf[8],cbuf[9],cbuf[10],cbuf[11],cbuf[12],cbuf[13],cbuf[14],cbuf[15],
		cbuf[16],cbuf[17],cbuf[18],cbuf[19]);
	if (nc < 4)
	{
		strcpy(errlab,"TOOFEW");
		strcpy(errstr,ctyp);
		goto failed;
	}
/*
.....AUTO_HIDE
*/
	else if (strcmp(ctyp,"AUTO_HIDE") == 0)
	{
		if (!exefl) goto done;
		nc = nc - 3;
		for (i=0;i<nc;i++)
		{
			strcpy(errstr,cbuf[i]);
			if (strcmp(cbuf[i],"YES") == 0)
			{
				isav = LW_display_prop.hide_auto;
				LW_display_prop.hide_auto = inum = UU_TRUE;
			}
			else if (strcmp(cbuf[i],"NO") == 0)
			{
				isav = LW_display_prop.hide_auto;
				LW_display_prop.hide_auto = inum = UU_FALSE;
			}
			else if (strcmp(cbuf[i],"EDGES") == 0)
			{
				isav = LW_display_prop.hide_edge;
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"YES") == 0)
					LW_display_prop.hide_edge = inum = UU_TRUE;
				else if (strcmp(cbuf[i+1],"NO") == 0)
					LW_display_prop.hide_edge = inum = UU_FALSE;
				else
				{
					strcpy(errlab,"YESNO");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"TRANS") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = ul_to_number(cbuf[i+1],&inum);
				if (status != UU_SUCCESS || inum < 0 || inum > 100)
				{
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				isav = LW_display_prop.hide_lucency;
				LW_display_prop.hide_lucency = inum;
				i++;
			}
			else
			{
				strcpy(errlab,"INVMINOR");
				goto failed;
			}
			if (inum != isav) hide_flag = UU_TRUE;
		}
	}
/*
.....COLORS
*/
	else if (strcmp(ctyp,"COLORS") == 0)
	{
		if (!exefl) goto done;
		nc = nc - 3;
		for (i=0;i<nc;i++)
		{
			strcpy(errstr,cbuf[i]);
			if (strcmp(cbuf[i],"AUTO_COLOR") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols,0);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_default_tool.initial = inum;
				i++;
			}
			else if (strcmp(cbuf[i],"CUT") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols[2],1);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_default_tool.cut_color = inum;
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++) cpt[j].cut_color = inum;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"CUTTER") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols,1);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_default_tool.color = inum;
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++) cpt[j].color[0] = inum;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"FIXTUR_CUT") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols,0);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_clash_material.fixture = inum;
				i++;
			}
			else if (strcmp(cbuf[i],"HOLDER") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols,1);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_default_tool.hold_color = inum;
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++) cpt[j].color[2] = inum;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"HOLDER_CUT") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols,0);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_clash_material.holder = inum;
				i++;
			}
			else if (strcmp(cbuf[i],"RAPID_CUT") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols,0);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_clash_material.rapid = inum;
				i++;
			}
			else if (strcmp(cbuf[i],"SHANK") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols,1);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_default_tool.shank_color = inum;
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++) cpt[j].color[1] = inum;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"USE_FIXTUR") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"YES") == 0)
					LW_default_tool.use_stock[1] = UU_TRUE;
				else if (strcmp(cbuf[i+1],"NO") == 0)
					LW_default_tool.use_stock[1] = UU_FALSE;
				else
				{
					strcpy(errlab,"YESNO");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"USE_STOCK") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"YES") == 0)
					LW_default_tool.use_stock[0] = UU_TRUE;
				else if (strcmp(cbuf[i+1],"NO") == 0)
					LW_default_tool.use_stock[0] = UU_FALSE;
				else
				{
					strcpy(errlab,"YESNO");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else
			{
				strcpy(errlab,"INVMINOR");
				goto failed;
			}
		}
	}
/*
.....MACHINE
*/
	else if (strcmp(ctyp,"MACHINE") == 0)
	{
		if (!exefl) goto done;
		if (strcmp(cbuf[0],"MILL") == 0 || !NAUTLTH) LW_mach_type = LW_MILL;
		else if (strcmp(cbuf[0],"LATHE") == 0) LW_mach_type = LW_LATHE;
		else if (strcmp(cbuf[0],"MILLTURN") == 0) LW_mach_type = LW_MILLTURN;
		else if (strcmp(cbuf[0],"STRINGER") == 0) LW_mach_type = LW_STRINGER;
		else
		{
			strcpy(errlab,"MACHTYPE");
			strcpy(errstr,cbuf[0]);
			goto failed;
		}
		LW_mach_type_main = LW_mach_type;
		LW_mach_type_flag = LW_mach_type + 1;
	}
/*
.....STACK
*/
	else if (strcmp(ctyp,"STACK") == 0)
	{
		if (!exefl) goto done;
		nc = nc - 3;
		ifl = LW_mot_stack_active;
		for (i=0;i<nc;i++)
		{
			strcpy(errstr,cbuf[i]);
			if (strcmp(cbuf[i],"ON") == 0) LW_mot_stack_active = UU_TRUE;
			else if (strcmp(cbuf[i],"OFF") == 0) LW_mot_stack_active = UU_FALSE;
			else if (strcmp(cbuf[i],"FIXTUR") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"YES") == 0) LW_mot_stack_fixture = UU_TRUE;
				else if (strcmp(cbuf[i+1],"NO") == 0)
					LW_mot_stack_fixture = UU_FALSE;
				else
				{
					strcpy(errlab,"YESNO");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else
			{
				status = ul_to_number(cbuf[i],&inum);
				if (status != UU_SUCCESS || inum < 0)
				{
					strcpy(errlab,"POSVAL");
					goto failed;
				}
				LW_mot_stack_size = inum;
			}
		}
/*
........Reactivate stack
*/
		if (LW_mot_stack_active != ifl) ul_ipv_mot_stack_init();
	}
/*
.....STOCK
*/
	else if (strcmp(ctyp,"STOCK") == 0 || strcmp(ctyp,"FIXTUR") == 0)
	{
		if (!exefl) goto done;
		nc = nc - 3;
		ifxt = strcmp(ctyp,"FIXTUR") == 0;
		for (i=0;i<nc;i++)
		{
			strcpy(errstr,cbuf[i]);
			if (strcmp(cbuf[i],"COLOR") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols,1);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_stock_default[ifxt].color = inum;
				i++;
			}
			else if (strcmp(cbuf[i],"EDGES") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols,2);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				if (inum == -2) LW_stock_default[ifxt].edge = UU_FALSE;
				else
				{
					LW_stock_default[ifxt].edge = UU_TRUE;
					LW_stock_default[ifxt].edge_color = inum;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"IMPORTANT") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"YES") == 0)
					LW_stock_default[ifxt].important = UU_TRUE;
				else if (strcmp(cbuf[i+1],"NO") == 0)
					LW_stock_default[ifxt].important = UU_FALSE;
				else
				{
					strcpy(errlab,"YESNO");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"STL") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"ASCII") == 0) LW_stl_format = 0;
				else if (strcmp(cbuf[i+1],"BINARY") == 0) LW_stl_format = 1;
				else
				{
					strcpy(errlab,"ASCBIN");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"STL_DEACT") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"YES") == 0)
					LW_stl_flag[1] = UU_TRUE;
				else if (strcmp(cbuf[i+1],"NO") == 0)
					LW_stl_flag[1] = UU_FALSE;
				else
				{
					strcpy(errlab,"YESNO");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"STL_SKIP_ERROR") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"YES") == 0)
					LW_stl_flag[2] = UU_TRUE;
				else if (strcmp(cbuf[i+1],"NO") == 0)
					LW_stl_flag[2] = UU_FALSE;
				else
				{
					strcpy(errlab,"YESNO");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"STL_STOP") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"YES") == 0)
					LW_stl_flag[0] = UU_TRUE;
				else if (strcmp(cbuf[i+1],"NO") == 0)
					LW_stl_flag[0] = UU_FALSE;
				else
				{
					strcpy(errlab,"YESNO");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"TOLER") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = ul_to_reals(rvals,&ncv,1,cbuf[i+1]);
				if (status != UU_SUCCESS || rvals[0] < .00001 || 
					rvals[0] > .5)
				{
					if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
					else strcpy(errlab,"OUTRANGE");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				UM_len_exttoint(rvals[0],LW_stock_default[ifxt].toler);
				i++;
			}
			else if (strcmp(cbuf[i],"TRANS") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = ul_to_number(cbuf[i+1],&inum);
				if (status != UU_SUCCESS || inum < 0 || inum > 100)
				{
					if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
					else strcpy(errlab,"OUTRANGE");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_stock_default[ifxt].translucency = inum;
				i++;
			}
			else if (strcmp(cbuf[i],"VISIBLE") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"YES") == 0)
				{
					LW_stock_default[ifxt].visible = UU_TRUE;
					LW_stock_default[ifxt].active = UU_TRUE;
				}
				else if (strcmp(cbuf[i+1],"NO") == 0)
				{
					LW_stock_default[ifxt].visible = UU_FALSE;
					LW_stock_default[ifxt].active = UU_FALSE;
				}
				else
				{
					strcpy(errlab,"YESNO");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else
			{
				strcpy(errlab,"INVMINOR");
				goto failed;
			}
		}
	}
/*
.....TOOL
*/
	else if (strcmp(ctyp,"TOOL") == 0)
	{
		nc = nc - 3;
		for (i=0;i<nc;i++)
		{
			if (strcmp(cbuf[i],"FROM_NEXT") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"YES") == 0)
					LW_tool_from = UU_TRUE;
				else if (strcmp(cbuf[i+1],"NO") == 0)
					LW_tool_from = UU_FALSE;
				else
				{
					strcpy(errlab,"YESNO");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"EDGES") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					if (i == nc-1) goto failed;
				}
				status = S_parse_color(cbuf[i+1],&inum,cols,2);
				if (status == UU_FAILURE)
				{
					strcpy(errlab,"INVCOLOR");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				if (inum == -2) LW_default_tool.edge = UU_FALSE;
				else
				{
					LW_default_tool.edge = UU_TRUE;
					LW_default_tool.edge_color = inum;
				}
				if (tool_flag)
				{
					for (k=0;k<3;k++)
					{
						for (j=0;j<LW_ntool;j++)
						{
							if (inum == -2) cpt[j].edge[k] = UU_FALSE;
							else
							{
								cpt[j].edge[k] = UU_TRUE;
								cpt[j].edge_color[k] = inum;
							}
						}
					}
				}
				i++;
			}
			else if (strcmp(cbuf[i],"MAXANG") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = ul_to_reals(rvals,&ncv,1,cbuf[i+1]);
				if (status != UU_SUCCESS || rvals[0] < .001 || 
					rvals[0] > 360.)
				{
					if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
					else strcpy(errlab,"OUTRANGE");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_default_tool.maxang = rvals[0];
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++) cpt[j].maxang = rvals[0];
				}
				i++;
			}
			else if (strcmp(cbuf[i],"MAX_HEIGHT") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = ul_to_reals(rvals,&ncv,1,cbuf[i+1]);
				if (status != UU_SUCCESS || rvals[0] < .0)
				{
					if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
					else strcpy(errlab,"OUTRANGE");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				UM_len_exttoint(rvals[0],LW_tool_limit[1]);
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++)
					{
						if (LW_tool_limit[1] < cpt[j].cutter[2])
							cpt[j].cutter[2] = LW_tool_limit[1];
					}
				}
				i++;
			}
			else if (strcmp(cbuf[i],"MIN_DIAMETER") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = ul_to_reals(rvals,&ncv,1,cbuf[i+1]);
				if (status != UU_SUCCESS || rvals[0] < .0)
				{
					if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
					else strcpy(errlab,"OUTRANGE");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				UM_len_exttoint(rvals[0],LW_tool_limit[2]);
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++)
					{
						if (LW_tool_limit[2] > cpt[j].cutter[0])
							cpt[j].cutter[0] = LW_tool_limit[2];
					}
				}
				i++;
			}
			else if (strcmp(cbuf[i],"MIN_HEIGHT") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = ul_to_reals(rvals,&ncv,1,cbuf[i+1]);
				if (status != UU_SUCCESS || rvals[0] < .0)
				{
					if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
					else strcpy(errlab,"OUTRANGE");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				UM_len_exttoint(rvals[0],LW_tool_limit[0]);
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++)
					{
						if (LW_tool_limit[0] > cpt[j].cutter[2])
							cpt[j].cutter[2] = LW_tool_limit[0];
					}
				}
				i++;
			}
			else if (strcmp(cbuf[i],"RAPID") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = ul_to_reals(rvals,&ncv,1,cbuf[i+1]);
				if (status != UU_SUCCESS || rvals[0] < .0)
				{
					if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
					else strcpy(errlab,"OUTRANGE");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				UM_len_exttoint(rvals[0],LW_default_tool.rapid);
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++) cpt[j].rapid = LW_default_tool.rapid;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"SHANK") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				if (strcmp(cbuf[i+1],"CUTTER") == 0)
					LW_default_tool.shank_clash = 0;
				else if (strcmp(cbuf[i+1],"HOLDER") == 0)
					LW_default_tool.shank_clash = 1;
				else
				{
					strcpy(errlab,"CUTHOLD");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++)
						cpt[j].shank_clash = LW_default_tool.shank_clash;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"TOLER") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = ul_to_reals(rvals,&ncv,1,cbuf[i+1]);
				if (status != UU_SUCCESS || rvals[0] < .001 || 
					rvals[0] > 2.5)
				{
					if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
					else strcpy(errlab,"OUTRANGE");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				UM_len_exttoint(rvals[0],LW_default_tool.toler);
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++) cpt[j].toler = LW_default_tool.toler;
				}
				i++;
			}
			else if (strcmp(cbuf[i],"TRANS") == 0)
			{
				if (i == nc-1)
				{
					strcpy(errlab,"TOOFEW");
					goto failed;
				}
				status = ul_to_number(cbuf[i+1],&inum);
				if (status != UU_SUCCESS || inum < 0 || inum > 100)
				{
					if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
					else strcpy(errlab,"OUTRANGE");
					strcpy(errstr,cbuf[i+1]);
					goto failed;
				}
				LW_default_tool.translucency = inum;
				if (tool_flag)
				{
					for (j=0;j<LW_ntool;j++)
					{
						for (k=0;k<3;k++)
							cpt[j].trans[k] = LW_default_tool.translucency;
					}
				}
				i++;
			}
			else
				goto failed;
		}
	}
/*
.....Unrecognized command
*/
	else
	{
		strcpy(errlab,"INVMINOR");
		strcpy(errstr,ctyp);
		goto failed;
	}
/*
.....Done parsing command
........Auto_Hide properties changed
*/
	if (hide_flag)
		ul_ipv_display_obstruct();
	status = UU_SUCCESS;
	goto done;
/*
.....Invalid command
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_offset(cstr,exefl,errlab,errstr)
**       Parses and executes a PPRINT IPV OFFSET file statement.
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**          exefl     = UU_TRUE = Execute command,
**                      UU_FALSE = Test command for syntax only.
**       OUTPUT :  
**          errlab    = Error message label.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_offset(cstr,exefl,errlab,errstr)
char *cstr;
UU_LOGICAL exefl;
char *errlab,*errstr;
{
	int nc,status,inc;
	char *p;
	char tstr[1024];
/*
.....Break out Offsets filename
*/
	inc = 0;
	nc = strlen(cstr);
	do
	{
		ul_parse_string(cstr,nc,&inc,tstr);
	} while (ul_compare_upper(tstr,"OFFSET"));
	strcpy(tstr,&cstr[inc]);
	ul_cut_string(tstr,1024);
	ul_remove_quotes(tstr);
	nc = strlen(tstr);
/*
.....Load the Offsets file
*/
	status = UU_SUCCESS;
	if (exefl) status = ul_ipv_load_offsets(tstr);
	if (status != UU_SUCCESS)
	{
		strcpy(errlab,"LODOFFS");
		ul_short_filename(tstr,errstr,40);
	}
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_positn(cstr,ofs,exefl,errlab,errstr)
**       Parses and executes a PPRINT IPV POSITN/OFFSET statements.
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**          ofs       = UU_FALSE = process POSITN command.
**                      UU_TRUE  = process OFFSET command.
**          exefl     = UU_TRUE = Execute command,
**                      UU_FALSE = Test command for syntax only.
**       OUTPUT :  
**          errlab    = Error message label.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_positn(cstr,ofs,exefl,errlab,errstr)
char *cstr;
UU_LOGICAL ofs,exefl;
char *errlab,*errstr;
{
	int i,nc,status,naxes;
	char buf[100],label[LW_MAX_AXES][20],*p,*q;
	char tstr[1024];
	UU_REAL rvals[LW_MAX_AXES];
/*
.....Convert string to upper case
*/
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
/*
.....Position at POSITN parameters
*/
	buf[0] = '\0';
	if (ofs)
		p = strstr(tstr,"OFFSET");
	else
		p = strstr(tstr,"POSITN");
	if (p == UU_NULL) goto failed;
	p += 6;
/*
.....Loop through the parameters
*/
	naxes = 0;
	do
	{
		while (*p == ' ') p++;
/*
........Get the axis label
*/
		q = p;
		while (*q != ' ' && *q != '\0') *q++;
		if (*q == '\0') goto failed;
		nc = q - p;
		strncpy(label[naxes],p,nc); label[naxes][nc] = '\0';
		p = q++;
		while (*p == ' ') p++;
/*
........Get the position
*/
		q = p;
		while (*q != ' ' && *q != '\0') *q++;
		nc = q - p;
		strncpy(buf,p,nc); buf[nc] = '\0';
		p = q;
		while (*p == ' ') p++;
/*
.....Get axis position
*/
		status = ul_to_reals(&rvals[naxes],&nc,1,buf);
		if (nc != 1)
		{
			strcpy(errlab,"NUMEXP");
			strcpy(errstr,buf);
			goto failed;
		}
		naxes++;
	} while (*p != '\0');
/*
.....Process POSITN command
*/
	if (exefl)
	{
		for (i=0;i<naxes;i++) UM_len_exttoint(rvals[i],rvals[i]);
		status = ul_ipv_positn_axis(label,rvals,naxes,ofs);
		if (status != UU_SUCCESS)
		{
			strcpy(errlab,"NOTPOS");
			strcpy(errstr,label[0]);
		}
	}
	goto done;
/*
.....Invalid POSITN command
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_print_screen(cstr,errlab,errstr)
**       Parses and executes a PPRINT IPV PRINT_SCREEN statement (command).
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**       OUTPUT :  
**          errlab    = Error message label.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_print_screen(cstr,errlab,errstr)
char *cstr,*errlab,*errstr;
{
	int nc,status,i,ntyp=4,nsiz=16,ityp,isiz;
	UU_LOGICAL ifl;
	char tstr[1024];
	char buf[100],cstk[100],ctyp[100],csiz[100];
	UX_pathname cfil,fullname,dir;
	char *p;
	UU_REAL rvals[1000];
	static char ttyp[4][4]={"BMP","JPG","PS","GIF"};
	static char tsiz[16][5]={"AH","AV","B","C","D","E","F","A0","A1","A2","A3",
		"A4","USER1","USER2","USER3","USER4"};
/*
.....Convert string to upper case
*/
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
	nc = ul_cut_string(tstr,1024);
	tstr[nc] = '\0';
/*
.....Parse beginning of string
*/
	buf[0] = '\0';
	cfil[0] = '\0';
	nc = sscanf(tstr,"%s%s%s%s%s",buf,cstk,ctyp,csiz,cfil);
	strcpy(errstr,ctyp);
	if (nc < 4)
	{
		strcpy(errlab,"TOOFEW");
		goto failed;
	}
	if (nc > 4)
		nc = sscanf(cstr,"%s%s%s%s%s",buf,buf,buf,buf,cfil);
	ul_remove_quotes(cfil);
/*
.....Determine picture type
*/
	for (i=0;i<ntyp;i++) if (strcmp(ctyp,ttyp[i]) == 0) break;
	if (i == ntyp)
	{
		strcpy(errlab,"PICTYPE");
		goto failed;
	}
	ityp = i;
/*
.....Determine paper size
*/
	for (i=0;i<nsiz;i++) if (strcmp(csiz,tsiz[i]) == 0) break;
	if (i == nsiz)
	{
		strcpy(errlab,"PAPERSZ");
		goto failed;
	}
	isiz = i;
/*
.....Get filename
*/
	if (nc == 4)
	{
		S_default_filename(cfil,"");
		sprintf(buf,"_%d",LW_print_num++);
		strcat(cfil,buf);
	}
/*
.....Print the screen
*/
	ud_print_ipvscreen(cfil,0,ityp,isiz);
	status = UU_SUCCESS;
	goto done;
/*
.....Invalid command
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_session(cstr,errlab,errstr)
**       Parses and executes a PPRINT IPV SESSION statement (command).
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**       OUTPUT :  
**          errlab    = Error message label.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_session(cstr,errlab,errstr)
char *cstr,*errlab,*errstr;
{
	int ifxt,nc,idn,status,iftyp,i,kerr,ivals[1000];
	char tstr[1024];
	char buf[100],cstk[100],ctyp[100],sidn[100];
	char *p;
	UU_REAL rvals[1000];
/*
.....Convert string to upper case
*/
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
	nc = ul_cut_string(tstr,1024);
	tstr[nc] = '\0';
/*
.....Determine type of SESSION command
*/
	buf[0] = '\0';
	nc = sscanf(tstr,"%s%s%s",buf,cstk,ctyp);
	strcpy(errstr,cstk);
	if (nc != 3) goto failed;
	p = strstr(tstr,ctyp);
	p = p + strlen(ctyp);
	strcpy(errstr,ctyp);
/*
.....EXPORT
*/
	if (strcmp(ctyp,"EXPORT") == 0)
	{
		p = cstr + (int)(p-tstr);
		strcpy(tstr,p);
		nc = ul_cut_string(tstr,strlen(tstr)); tstr[nc] = '\0';
		ul_remove_quotes(tstr);
		nc = strlen(tstr);
		if (nc == 0)
		{
			S_default_filename(tstr,"");
			nc = strlen(tstr);
		}
		if (nc == 0)
		{
			strcpy(errlab,"FILEXP");
			goto failed;
		}
		ul_ipv_archive_session(tstr);
	}
/*
.....IMPORT
*/
	else if (strcmp(ctyp,"IMPORT") == 0)
	{
		p = cstr + (int)(p-tstr);
		strcpy(tstr,p);
		nc = ul_cut_string(tstr,strlen(tstr)); tstr[nc] = '\0';
		ul_remove_quotes(tstr);
		nc = strlen(tstr);
		if (nc == 0)
		{
			strcpy(errlab,"FILEXP");
			goto failed;
		}
		status = ul_ipv_load_session(tstr,UU_FALSE);
	}
/*
.....Unrecognized command
*/
	else
	{
		strcpy(errlab,"INVMINOR");
		goto failed;
	}
/*
.....Command parsed correctly
*/
	status = UU_SUCCESS;
	goto done;
/*
.....Invalid command
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_spindle(cstr,errlab,errstr,mdid)
**       Parses and executes a PPRINT IPV SPINDLE statement (command).
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**       OUTPUT :  
**          errlab    = Error message label.
**          errstr    = Part of command that caused error.
**          mdid      = PPRINT IPV SPINDLE command processed.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_spindle(cstr,errlab,errstr,mdid)
char *cstr;
char *errlab,*errstr;
int *mdid;
{
	int nc,status,i,j,inum,ivals[10];
	UU_LOGICAL ilod,idid;
	char buf[100],buf1[100],buf2[100],*p;
	char tstr[1024];
	UU_REAL rvals[10];
	LW_mach_model_struc *mpt;
/*
.....Convert string to upper case
*/
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
	nc = sscanf(tstr,"%s%s%s",buf,buf1,buf2);
/*
.....Position at SPINDLE parameters
........PPRINT IPV SPINDLE LOAD
*/
	if (strcmp(buf2,"LOAD") == 0)
	{
		ilod = UU_TRUE;
		p = strstr(tstr,"LOAD");
		if (p == UU_NULL) goto failed;
		p += 4;
	}
/*
........PPRINT IPV SPINDLE
*/
	else
	{
		ilod = UU_FALSE;
		p = strstr(tstr,"SPINDLE");
		if (p == UU_NULL) goto failed;
		p += 7;
	}
	while (*p == ' ') p++;
/*
.....Break out SPINDLE parameters
*/
	status = ul_to_reals(rvals,&nc,10,p);
	if (nc == 0)
	{
		strcpy(errlab,"NUMEXP");
		nc = sscanf(tstr,"%s%s%s",buf,buf1,errstr);
		goto failed;
	}
	for (i=0;i<nc;i++)
	{
		inum = rvals[i];
		if (inum < 0 || inum > 9)
		{
			strcpy(errlab,"OUTRANGE");
			sprintf(errstr,"%d",inum);
			goto failed;
		}
	}
/*
.....Define Spindles to load tools into
*/
	if (ilod)
	{
		for (i=0;i<nc;i++)
		{
			idid = UU_FALSE;
			inum = rvals[i];
			for (j=0;j<LW_spindle_num;j++)
			{
				if (inum == LW_spindle_ix[j])
				{
					idid = UU_TRUE;
					ivals[i] = j;
				}
			}
			if (!idid)
			{
				strcpy(errlab,"NOSPINDL");
				sprintf(errstr,"%d",inum);
				goto failed;
			}
		}
		for (i=0;i<nc;i++) LW_spindle_load[i] = ivals[i];
		LW_spindle_nload = nc;
	}
/*
.....Define active Spindles
........Restore previous spindle visibility &
........unload any currently loaded tools
*/
	else
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		for (i=0;i<LW_spindle_num;i++)
		{
			ul_ipv_deselect_tool();
			ul_ipv_spindle_vis(LW_spindle_ix[i],LW_spindle_vis[i]);
		}
/*
........Define active spindles
*/
		for (i=0;i<nc;i++)
		{
			LW_spindle_ix[i] = rvals[i];
			LW_spindle_vis[i] = mpt[LW_spindle[LW_spindle_ix[i]]].visible;
			LW_spindle_load[i] = i;
		}
		LW_spindle_num = LW_spindle_nload = nc;
/*
........Set new spindle visibility
*/
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		for (i=0;i<LW_spindle_num;i++)
			ul_ipv_spindle_vis(LW_spindle_ix[i],UU_TRUE);
	}
	status = UU_SUCCESS;
	*mdid = 1;
	goto done;
/*
.....Invalid SPINDLE command
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_stock(cstr,exefl,errlab,errstr)
**       Parses and executes a PPRINT IPV STOCK/FIXTUR statement (command).
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**          exefl     = UU_TRUE = Process STOCK command,
**                      UU_FALSE = Test command for syntax only.
**       OUTPUT :  
**          errlab    = Error message label.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_stock(cstr,exefl,errlab,errstr)
char *cstr;
UU_LOGICAL exefl;
char *errlab,*errstr;
{
	int ifxt,nc,idn,status,iftyp,i,kerr,ivals[1000],imx,ncopies;
	char tstr[1024];
	char buf[100],cstk[100],ctyp[100],sidn[100],cbuf[100];
	char *p,*q;
	UU_REAL rvals[1000],*rptr,tvals[12],co,si;
	static char cols[96]={"DEFAULT"};
/*
.....Convert string to upper case
*/
	kerr = 0;
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
	nc = ul_cut_string(tstr,1024);
	tstr[nc] = '\0';
/*
.....Determine type of STOCK command
*/
	buf[0] = '\0';
	nc = sscanf(tstr,"%s%s%s%s",buf,cstk,ctyp,&sidn);
	strcpy(errstr,cstk);
	if (nc < 3)
	{
		strcpy(errlab,"TOOFEW");
		goto failed;
	}
	p = strstr(tstr,ctyp);
	p = p + strlen(ctyp);
/*
.....Get STOCK ID number
*/
	if (nc == 3)
		idn = -1;
	else
	{
		q = strchr(sidn,',');
		if (q  != UU_NULL) *q = '\0';
		p = strstr(p,sidn);
		if (strcmp(ctyp,"REMOVE") != 0 && strcmp(ctyp,"RESET_CUTCOLOR") != 0)
		{
			p = p + strlen(sidn);
			if (*p == ',') p++;
			if ((strcmp(ctyp,"MOVE") == 0 || strcmp(ctyp,"TRANSL") == 0 ||
				strcmp(ctyp,"XYROT") == 0 || strcmp(ctyp,"YZROT") == 0 ||
				strcmp(ctyp,"ZXROT") == 0) && strcmp(sidn,"INCR") == 0) idn = 1;
			else
			{
				status = ul_to_number(sidn,&idn);
				if (status != UU_SUCCESS)
				{
					strcpy(errlab,"IDNEXP");
					strcpy(errstr,sidn);
					goto failed;
				}
			}
		}
	}
/*
.....Determine STOCK or FIXTUR
*/
	ifxt = strcmp(cstk,"FIXTUR") == 0;
/*
.....STOCK/BOX
*/
	strcpy(errstr,ctyp);
	if (strcmp(ctyp,"BOX") == 0)
	{
		status = ul_to_reals(rvals,&nc,6,p);
		if (status == UU_FAILURE || nc != 6)
		{
			strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%lf",rvals[nc-1]);
			goto failed;
		}
		iftyp = 1;
		UM_cc_exttoint(rvals,rvals);
		rptr = &rvals[3];
		UM_cc_exttoint(rptr,rptr);
		if (exefl) ulf_verify_box(&ifxt,&iftyp,rvals,&idn,&kerr);
	}
/*
.....STOCK/CLONE
*/
	else if (strcmp(ctyp,"CLONE") == 0)
	{
		status = ul_to_reals(rvals,&nc,2,p);
		if (nc != 2)
		{
			strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%lf",rvals[nc-1]);
			goto failed;
		}
		iftyp = rvals[0];
		ncopies = rvals[1];
/*
........Matrix specified in command
........parse it
*/
		imx = 0;
		if (status != UU_SUCCESS)
		{
			nc = strlen(p); ul_strip_blanks(p,&nc);
			nc = sscanf(p,"%lf,%lf,%s",&tvals[0],&tvals[1],cbuf);
			if (nc != 3)
			{
				strcpy(errlab,"FEWVALS");
				sprintf(errstr,"%lf",rvals[1]);
				goto failed;
			}
			strcpy(errstr,cbuf);
			if (strncmp(cbuf,"AT",2) == 0)
			{
				imx = 1;
				p = cbuf + 3;
			}
			else if (strncmp(cbuf,"TRANSL",6) == 0)
			{
				imx = 2;
				p = cbuf + 7;
			}
			else if (strncmp(cbuf,"XYROT",5) == 0)
			{
				imx = 3;
				p = cbuf + 6;
			}
			else if (strncmp(cbuf,"YZROT",5) == 0)
			{
				imx = 4;
				p = cbuf + 6;
			}
			else if (strncmp(cbuf,"ZXROT",5) == 0)
			{
				imx = 5;
				p = cbuf + 6;
			}
			else goto failed;
/*
...........Get matrix
*/
			status = ul_to_reals(rvals,&nc,12,p);
			if ((imx == 1 && nc != 12) || (imx == 2 && nc != 3) ||
				(imx > 2 && nc != 1))
			{
				strcpy(errlab,"NUMVALS");
				strcpy(errstr,p);
				goto failed;
			}
		}
/*
........Execute command
*/
		if (exefl) ulf_verify_copy(&ifxt,&iftyp,&idn,&ncopies,&imx,rvals,&kerr);
	}
/*
.....STOCK/COMPOS
*/
	else if (strcmp(ctyp,"COMPOS") == 0)
	{
		status = S_parse_idns(p,ivals,&nc,0,errstr);
		if (status != UU_SUCCESS)
		{
			strcpy(errlab,"IDNEXP");
			strcpy(errstr,p);
			goto failed;
		}
		if (exefl) ulf_verify_compos(&ifxt,&idn,ivals,&nc,&kerr);
	}
/*
.....STOCK/CONE
*/
	else if (strcmp(ctyp,"CONE") == 0)
	{
		status = ul_to_reals(rvals,&nc,9,p);
		if (status == UU_FAILURE || nc != 9)
		{
			if (status == UU_FAILURE) strcpy(errlab,"NUMEXP");
			else strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%lf",rvals[nc-1]);
			goto failed;
		}
		UM_cc_exttoint(rvals,rvals);
		rptr = &rvals[6];
		UM_cc_exttoint(rptr,rptr);
		if (exefl) ulf_verify_cone(&ifxt,rvals,&idn,&kerr);
	}
/*
.....STOCK/CYLNDR
*/
	else if (strcmp(ctyp,"CYLNDR") == 0)
	{
		status = ul_to_reals(rvals,&nc,8,p);
		if (status == UU_FAILURE || nc != 8)
		{
			if (status == UU_FAILURE) strcpy(errlab,"NUMEXP");
			else strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%f",rvals[nc-1]);
			goto failed;
		}
		UM_cc_exttoint(rvals,rvals);
		UM_len_exttoint(rvals[6],rvals[6]);
		UM_len_exttoint(rvals[7],rvals[7]);
		if (exefl) ulf_verify_cyl(&ifxt,rvals,&idn,&kerr);
	}
/*
.....STOCK/DECOMP
*/
	else if (strcmp(ctyp,"DECOMP") == 0)
	{
		status = S_parse_idns(p,ivals,&nc,0,errstr);
		if (status != UU_SUCCESS || nc == 0)
		{
			strcpy(errlab,"IDNEXP");
			strcpy(errstr,p);
			goto failed;
		}
		if (exefl) ulf_verify_decomp(&ifxt,&idn,ivals,&nc,&kerr);
	}
/*
.....STOCK/LOAD
*/
	else if (strcmp(ctyp,"LOAD") == 0)
	{
		p = cstr + (int)(p-tstr);
		strcpy(tstr,p);
		ul_remove_quotes(tstr);
		nc = strlen(tstr);
		if (exefl) ulf_verify_load(tstr,&nc,&idn,&kerr);
	}
/*
.....STOCK/MOVE
*/
	else if (strcmp(ctyp,"MOVE") == 0)
	{
		status = ul_to_reals(rvals,&nc,1000,p);
		if (nc < 13)
		{
			strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%lf",rvals[nc-1]);
			goto failed;
		}
		UM_len_exttoint(rvals[3],rvals[3]);
		UM_len_exttoint(rvals[7],rvals[7]);
		UM_len_exttoint(rvals[11],rvals[11]);
		status = S_parse_idns(p,ivals,&nc,12,errstr);
		if (status != UU_SUCCESS)
		{
			strcpy(errlab,"IDNEXP");
			strcpy(errstr,p);
			goto failed;
		}
		i = 0;
		if (exefl) ulf_verify_move(&ifxt,ivals,&nc,rvals,"",&i,&idn,&kerr);
	}
/*
.....STOCK/TRANSL
*/
	else if (strcmp(ctyp,"TRANSL") == 0)
	{
		status = ul_to_reals(tvals,&nc,1000,p);
		if (nc < 4)
		{
			strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%lf",tvals[nc-1]);
			goto failed;
		}
		for (i=0;i<12;i++) rvals[i] = 0.;
		rvals[0] = rvals[5] = rvals[10] = 1.;
		UM_len_exttoint(tvals[0],rvals[3]);
		UM_len_exttoint(tvals[1],rvals[7]);
		UM_len_exttoint(tvals[2],rvals[11]);
		status = S_parse_idns(p,ivals,&nc,3,errstr);
		if (status != UU_SUCCESS)
		{
			strcpy(errlab,"IDNEXP");
			strcpy(errstr,p);
			goto failed;
		}
		i = 0;
		if (exefl) ulf_verify_move(&ifxt,ivals,&nc,rvals,"",&i,&idn,&kerr);
	}
/*
.....STOCK/XYROT,YZROT,ZXROT
*/
	else if (strcmp(ctyp,"XYROT") == 0 || strcmp(ctyp,"YZROT") == 0 ||
		strcmp(ctyp,"ZXROT") == 0)
	{
		status = ul_to_reals(tvals,&nc,1000,p);
		if (nc < 2)
		{
			strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%lf",tvals[nc-1]);
			goto failed;
		}
			
		for (i=0;i<12;i++) rvals[i] = 0.;
		co = cos(tvals[0]/UM_RADIAN);
		si = sin(tvals[0]/UM_RADIAN);

		if (strcmp(ctyp,"XYROT") == 0)
		{
			rvals[0] = co; rvals[1] = -si;
			rvals[4] = si; rvals[5] = co;
			rvals[10] = 1.;
		}
		else if (strcmp(ctyp,"YZROT") == 0)
		{
			rvals[5] = co; rvals[6] = -si;
			rvals[9] = si; rvals[10] = co;
			rvals[0] = 1.;
		}
		else if (strcmp(ctyp,"ZXROT") == 0)
		{
			rvals[10] = co; rvals[8] = -si;
			rvals[2] = si; rvals[0] = co;
			rvals[5] = 1.;
		}

		status = S_parse_idns(p,ivals,&nc,1,errstr);
		if (status != UU_SUCCESS)
		{
			strcpy(errlab,"IDNEXP");
			strcpy(errstr,p);
			goto failed;
		}
		i = 0;
		if (exefl) ulf_verify_move(&ifxt,ivals,&nc,rvals,"",&i,&idn,&kerr);
	}
/*
.....STOCK/MODIFY
*/
	else if (strcmp(ctyp,"MODIFY") == 0)
	{
		q = strchr(p,',');
		if (q != UU_NULL)
		{
			*q = '\0';
			strcpy(tstr,p); nc = strlen(tstr); ul_strip_blanks(tstr,&nc);
			status = S_parse_color(tstr,&ivals[0],cols,1);
			if (status != UU_SUCCESS)
			{
				strcpy(errlab,"INVCOLOR");
				strcpy(errstr,tstr);
				goto failed;
			}
			p = q+1;
		}
		else
		{
			strcpy(errlab,"FEWVALS");
			strcpy(errstr,p);
			goto failed;
		}
		status = ul_to_reals(&rvals[1],&nc,1000,p);
		if (nc < 5) 
		{
			strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%lf",rvals[nc]);
			goto failed;
		}
		UM_len_exttoint(rvals[4],rvals[4]);
		for (i=1;i<4;i++) ivals[i] = rvals[i];
		status = S_parse_idns(p,&ivals[5],&nc,4,errstr);
		if (status != UU_SUCCESS)
		{
			strcpy(errlab,"IDNEXP");
			strcpy(errstr,p);
			goto failed;
		}
		if (exefl) ulf_verify_modify(&ifxt,&ivals[5],&nc,&ivals[0],&ivals[1],
			&rvals[4],&ivals[2],&ivals[3],&kerr);
	}
/*
.....STOCK/REMOVE
*/
	else if (strcmp(ctyp,"REMOVE") == 0)
	{
		status = S_parse_idns(p,ivals,&nc,0,errstr);
		if (status != UU_SUCCESS)
		{
			strcpy(errlab,"IDNEXP");
			strcpy(errstr,p);
			goto failed;
		}
		if (exefl) ulf_verify_remove(&ifxt,ivals,&nc,&kerr);
	}
/*
.....STOCK/REMOVE_CHIPS
*/
	else if (strcmp(ctyp,"REMOVE_CHIPS") == 0)
	{
		status = ul_to_reals(rvals,&nc,1000,p);
		if (status == UU_FAILURE || nc < 6)
		{
			if (status != UU_SUCCESS) strcpy(errlab,"NUMEXP");
			else strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%lf",rvals[nc-1]);
			goto failed;
		}
		for (i=0;i<nc;i=i+6)
		{
			rptr = &rvals[i];
			UM_cc_exttoint(rptr,rptr);
		}
		nc = nc / 6;
		if (exefl) ulf_verify_chips(rvals,&nc);
	}
/*
.....STOCK/RESET_CUTCOLOR
*/
	else if (strcmp(ctyp,"RESET_CUTCOLOR") == 0)
	{
		status = S_parse_idns(p,ivals,&nc,0,errstr);
		if (status != UU_SUCCESS)
		{
			strcpy(errlab,"IDNEXP");
			strcpy(errstr,p);
			goto failed;
		}
		if (exefl) ulf_verify_reset_cutcolor(&ifxt,ivals,&nc,&kerr);
	}
/*
.....STOCK/SAVE
*/
	else if (strcmp(ctyp,"SAVE") == 0)
	{
		p = cstr + (int)(p-tstr);
		strcpy(tstr,p);
		nc = ul_cut_string(tstr,strlen(tstr)); tstr[nc] = '\0';
		ul_remove_quotes(tstr);
		nc = strlen(tstr);
		if (nc == 0)
		{
			S_default_filename(tstr,"");
			nc = strlen(tstr);
		}
		if (exefl) ul_ipv_save_stl(ifxt,tstr,nc,idn);
	}
/*
.....STOCK/SPHERE
*/
	else if (strcmp(ctyp,"SPHERE") == 0)
	{
		status = ul_to_reals(rvals,&nc,4,p);
		if (status == UU_FAILURE || nc != 4)
		{
			if (status != UU_SUCCESS) strcpy(errlab,"NUMEXP");
			else strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%lf",rvals[nc-1]);
			goto failed;
		}
		UM_cc_exttoint(rvals,rvals);
		UM_len_exttoint(rvals[3],rvals[3]);
		if (exefl) ulf_verify_sphere(&ifxt,rvals,&idn,&kerr);
	}
/*
.....STOCK/STL
*/
	else if (strcmp(ctyp,"STL") == 0)
	{
		while (*p == ' ') p++;
		if (strncmp(p,"INCHES",6) == 0)
		{
			p = p + 6;
			i= 0;
		}
		else if (strncmp(p,"MM",2) == 0)
		{
			p = p + 2;
			i= 1;
		}
		else
		{
			strcpy(errlab,"INCHMM");
			strcpy(errstr,p);
			goto failed;
		}
		while (*p == ' ') p++;
     	p = cstr + (int)(p-tstr);
		strcpy(tstr,p);
		ul_remove_quotes(tstr);
		nc = strlen(tstr);
		if (exefl) ulf_verify_stl(&ifxt,tstr,&nc,&i,&idn,&kerr);
	}
/*
.....STOCK/TORUS
*/
	else if (strcmp(ctyp,"TORUS") == 0)
	{
		status = ul_to_reals(rvals,&nc,8,p);
		if (status == UU_FAILURE || nc != 8)
		{
			if (status == UU_FAILURE) strcpy(errlab,"NUMEXP");
			else strcpy(errlab,"FEWVALS");
			if (nc > 0) sprintf(errstr,"%lf",rvals[nc-1]);
			goto failed;
		}
		UM_cc_exttoint(rvals,rvals);
		UM_len_exttoint(rvals[6],rvals[6]);
		UM_len_exttoint(rvals[7],rvals[7]);
		if (exefl) ulf_verify_torus(&ifxt,rvals,&idn,&kerr);
	}
/*
.....Unrecognized command
*/
	else
	{
		strcpy(errlab,"INVMINOR");
		goto failed;
	}
/*
.....Command parsed correctly
*/
	if (kerr != 0) goto failed;
	status = UU_SUCCESS;
	goto done;
/*
.....Invalid STOCK command
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_tool(cstr,errlab,errstr)
**       Parses and executes a PPRINT IPV TOOL statement (command).
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**       OUTPUT :  
**          errlab    = Error message label.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_tool(cstr,errlab,errstr)
char *cstr,*errlab,*errstr;
{
	int nc,ncv,status,i,inum;
	UU_REAL rvals[2];
	char tstr[1024],cbuf[24][20];
	char buf[100],cstk[100];
	static char cols[3][96]={"DEFAULT","OFF","AUTO"};
	UN_cutter_list *cpt;
/*
.....Determine if a tool is defined
*/
	status = UU_SUCCESS;
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	if (cpt != UU_NULL && LW_act_tool[0] >= 0)
		cpt = &cpt[LW_act_tool[0]];
	else
		goto done;
/*
.....Convert string to upper case
*/
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
	nc = ul_cut_string(tstr,1024);
	tstr[nc] = '\0';
/*
.....Parse string
*/
	nc = sscanf(tstr,"%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",buf,cstk,
		cbuf[0],cbuf[1],cbuf[2],cbuf[3],cbuf[4],cbuf[5],cbuf[6],cbuf[7],
		cbuf[8],cbuf[9],cbuf[10],cbuf[11],cbuf[12],cbuf[13],cbuf[14],cbuf[15],
		cbuf[16],cbuf[17],cbuf[18],cbuf[19]);
	if (nc < 4)
	{
		strcpy(errlab,"TOOFEW");
		strcpy(errstr,cstk);
		goto failed;
	}
	nc = nc - 2;
/*
.....CUT_COLOR
*/
	for (i=0;i<nc;i++)
	{
		strcpy(errstr,cbuf[i]);
		if (strcmp(cbuf[i],"CUT_COLOR") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = S_parse_color(cbuf[i+1],&inum,&cols[2],1);
			if (status == UU_FAILURE)
			{
				strcpy(errlab,"INVCOLOR");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			cpt->cut_color = LW_cut_material = inum;
			i++;
		}
/*
.....CUTTER_COLOR
*/
		else if (strcmp(cbuf[i],"CUTTER_COLOR") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = S_parse_color(cbuf[i+1],&inum,cols,1);
			if (status == UU_FAILURE)
			{
				strcpy(errlab,"INVCOLOR");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			cpt->color[0] = LW_tool_material = inum;
			i++;
		}
/*
.....CUTTER_EDGES
*/
		else if (strcmp(cbuf[i],"CUTTER_EDGES") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = S_parse_color(cbuf[i+1],&inum,cols,2);
			if (status == UU_FAILURE)
			{
				strcpy(errlab,"INVCOLOR");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			if (inum == -2) cpt->edge[0] = UU_FALSE;
			else
			{
				cpt->edge[0] = UU_TRUE;
				cpt->edge_color[0] = inum;
			}
			i++;
		}
/*
.....CUTTER_TRANS
*/
		else if (strcmp(cbuf[i],"CUTTER_TRANS") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = ul_to_number(cbuf[i+1],&inum);
			if (status != UU_SUCCESS || inum < 0 || inum > 100)
			{
				if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
				else strcpy(errlab,"OUTRANGE");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			cpt->trans[0] = LW_translucency[1] = inum;
			i++;
		}
/*
.....HOLDER_COLOR
*/
		else if (strcmp(cbuf[i],"HOLDER_COLOR") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = S_parse_color(cbuf[i+1],&inum,cols,1);
			if (status == UU_FAILURE)
			{
				strcpy(errlab,"INVCOLOR");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			cpt->color[2] = inum;
			i++;
		}
/*
.....HOLDER_EDGES
*/
		else if (strcmp(cbuf[i],"HOLDER_EDGES") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = S_parse_color(cbuf[i+1],&inum,cols,2);
			if (status == UU_FAILURE)
			{
				strcpy(errlab,"INVCOLOR");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			if (inum == -2) cpt->edge[2] = UU_FALSE;
			else
			{
				cpt->edge[2] = UU_TRUE;
				cpt->edge_color[2] = inum;
			}
			i++;
		}
/*
.....HOLDER_TRANS
*/
		else if (strcmp(cbuf[i],"HOLDER_TRANS") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = ul_to_number(cbuf[i+1],&inum);
			if (status != UU_SUCCESS || inum < 0 || inum > 100)
			{
				if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
				else strcpy(errlab,"OUTRANGE");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			cpt->trans[2] = LW_translucency[2] = inum;
			i++;
		}
/*
.....MAXANG
*/
		else if (strcmp(cbuf[i],"MAXANG") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = ul_to_reals(rvals,&ncv,1,cbuf[i+1]);
			if (status != UU_SUCCESS || rvals[0] < .001 || 
				rvals[0] > 360.)
			{
				if (status != UU_SUCCESS) strcpy(errlab,"NUMEXP");
				else strcpy(errlab,"OUTRANGE");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			cpt->maxang = LW_maxang = rvals[0];
			i++;
		}
/*
.....RAPID
*/
		else if (strcmp(cbuf[i],"RAPID") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = ul_to_reals(rvals,&ncv,1,cbuf[i+1]);
			if (status != UU_SUCCESS || rvals[0] < .0)
			{
				if (status != UU_SUCCESS) strcpy(errlab,"NUMEXP");
				else strcpy(errlab,"POSVAL");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			UM_len_exttoint(rvals[0],cpt->rapid);
			i++;
		}
/*
.....SHANK
*/
		else if (strcmp(cbuf[i],"SHANK") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			if (strcmp(cbuf[i+1],"CUTTER") == 0)
				cpt->shank_clash = 0;
			else if (strcmp(cbuf[i+1],"HOLDER") == 0)
				cpt->shank_clash = 1;
			else
			{
				strcpy(errlab,"CUTHOLD");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			i++;
		}
/*
.....SHANK_COLOR
*/
		else if (strcmp(cbuf[i],"SHANK_COLOR") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = S_parse_color(cbuf[i+1],&inum,cols,1);
			if (status == UU_FAILURE)
			{
				strcpy(errlab,"INVCOLOR");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			cpt->color[1] = inum;
			i++;
		}
/*
.....SHANK_EDGES
*/
		else if (strcmp(cbuf[i],"SHANK_EDGES") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = S_parse_color(cbuf[i+1],&inum,cols,2);
			if (status == UU_FAILURE)
			{
				strcpy(errlab,"INVCOLOR");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			if (inum == -2) cpt->edge[1] = UU_FALSE;
			else
			{
				cpt->edge[2] = UU_TRUE;
				cpt->edge_color[1] = inum;
			}
			i++;
		}
/*
.....SHANK_TRANS
*/
		else if (strcmp(cbuf[i],"SHANK_TRANS") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = ul_to_number(cbuf[i+1],&inum);
			if (status != UU_SUCCESS || inum < 0 || inum > 100)
			{
				if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
				else strcpy(errlab,"OUTRANGE");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			cpt->trans[1] = LW_translucency[1] = inum;
			i++;
		}
/*
.....TOLER
*/
		else if (strcmp(cbuf[i],"TOLER") == 0)
		{
			if (i == nc-1)
			{
				strcpy(errlab,"TOOFEW");
				goto failed;
			}
			status = ul_to_reals(rvals,&ncv,1,cbuf[i+1]);
			if (status != UU_SUCCESS || rvals[0] < .001 || 
				rvals[0] > 2.5)
			{
				if (status != UU_SUCCESS) strcpy(errlab,"POSVAL");
				else strcpy(errlab,"OUTRANGE");
				strcpy(errstr,cbuf[i+1]);
				goto failed;
			}
			UM_len_exttoint(rvals[0],cpt->toler);
			LW_toler = cpt->toler;
			i++;
		}
/*
.....Unrecognized command
*/
		else
			goto failed;
	}
	goto done;
/*
.....Invalid command
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_toolpn(cstr,exefl,errlab,errstr)
**       Parses and executes a PPRINT IPV TOOLPN statement (command).
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**          exefl     = UU_TRUE = Process TOOLPN command,
**                      UU_FALSE = Test command for syntax only.
**       OUTPUT :  
**          errlab    = Error message label.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_toolpn(cstr,exefl,errlab,errstr)
char *cstr;
UU_LOGICAL exefl;
char *errlab,*errstr;
{
	int nc,status;
	UU_LOGICAL ifl;
	char buf[100],cstk[100],*p,*q;
	char tstr[1024];
	UU_REAL rvals[9];
/*
.....Convert string to upper case
*/
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
/*
.....Position at TOOLPN parameters
*/
	buf[0] = '\0';
	p = strstr(tstr,"TOOLPN");
	if (p == UU_NULL) goto failed;
	p += 6;
	while (*p == ' ') p++;
/*
.....Determine if there is a label
*/
	ifl = UU_TRUE;
	q = p;
	while (*q != ' ' && *q != ',') q++;
	if (*q == ' ')
	{
		nc = q - p;
		strncpy(buf,p,nc); buf[nc] = '\0';
		p = q++;
		while (*p == ' ') p++;
		nc = strlen(p);
		ul_strip_blanks(p,&nc);
		if (nc == 0) ifl = UU_FALSE;
	}
/*
.....Break out TOOLPN parameters
*/
	if (ifl)
	{
		status = ul_to_reals(rvals,&nc,9,p);
		if (nc != 9)
		{
			if (status != UU_SUCCESS) strcpy(errlab,"NUMEXP");
			else strcpy(errlab,"TOOFEW");
			if (nc > 0) sprintf(errstr,"%lf",rvals[nc-1]);
			goto failed;
		}
		UM_cc_exttoint(rvals,rvals);
	}
/*
.....Process TOOLPN command
*/
	if (exefl)
	{
		status = ul_ipv_tool_pin(buf,rvals,ifl);
		if (status != UU_SUCCESS) strcpy(errstr,buf);
	}
	goto done;
/*
.....Invalid STOCK command
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_view(cstr,errlab,errstr)
**       Parses and executes a PPRINT IPV VIEW statement (command).
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**       OUTPUT :  
**          errlab    = Error message label.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_view(cstr,errlab,errstr)
char *cstr;
char *errlab,*errstr;
{
	int nc,status;
	char tstr[1024];
	char buf[100],cstk[100],ctyp[100];
/*
.....Convert string to upper case
*/
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
	nc = ul_cut_string(tstr,1024);
	tstr[nc] = '\0';
/*
.....Parse beginning of string
*/
	buf[0] = '\0';
	nc = sscanf(tstr,"%s%s%s",buf,cstk,ctyp);
	strcpy(errstr,cstk);
	if (nc != 3)
	{
		strcpy(errlab,"TOOFEW");
		goto failed;
	}
/*
.....PPRINT IPV VIEW FIT
*/
	if (strcmp(ctyp,"FIT") == 0)
	{
		ul_ipv_view_active();
		uz_extrema_zoom();
	}
/*
.....Unrecognized command
*/
	else
	{
		strcpy(errlab,"INVMINOR");
		strcpy(errstr,ctyp);
		goto failed;
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Invalid command
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_parse_color(cstr,color,addtl_colors,ncolors)
**       Parses a color designator as either a label or number.
**    PARAMETERS   
**       INPUT  : 
**          cstr         = Color string to parse.
**          addtl_colors = Acceptable additional color names.
**          ncolors      = Number of additional colors.
**       OUTPUT :  
**          color        = Output color index value.
**    RETURNS      : UU_SUCCESS if a valid color, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_parse_color(cstr,color,addtl_colors,ncolors)
char *cstr;
int *color;
char addtl_colors[][96];
int ncolors;
{
	int status,ival;
/*
.....Check for integer color setting
*/
	status = ul_to_number(cstr,&ival);
	if (status == UU_SUCCESS)
	{
		if (ival > 63 || ival < -ncolors) goto failed;
		*color = ival;
	}
/*
.....Check for color label
*/
	else
	{
		status = ul_modal_color(cstr,color,addtl_colors,ncolors);
		if (status != UU_SUCCESS) goto failed;
	}
	status = UU_SUCCESS;
	goto done;
/*
.....End of routine
*/
failed:
	status = UU_FAILURE;
done:
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_parse_idns(cstr,idlist,nids,nignore,errstr)
**       Parses the ID-list portion of a STOCK/FIXTUR command.
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of ID-list.
**          nignore   = Number of parameters to ignore.
**       OUTPUT :  
**          errstr    = Part of command that caused error.
**          idlist    = ID numbers in specified in command.
**          nids      = Number of IDs in 'idlist' array.
**    RETURNS      : UU_SUCCESS if a valid ID-list, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_parse_idns(cstr,idlist,nids,nignore,errstr)
char *cstr;
int *idlist,*nids,nignore;
char *errstr;
{
	int status,ival,nc,i;
	UU_LOGICAL ithru;
	char lbuf[1024],cbuf[1024];
	char *p,*q;
/*
.....Ignore 'nignore' parameters
*/
	strcpy(cbuf,cstr);
	nc = strlen(cbuf); ul_strip_blanks(cbuf,&nc);
	p = cbuf;
	if (*p == ',') p++;
	for (i=0;i<nignore;i++)
	{
		p++; p = strchr(p,',');
	}
/*
.....Get rid of spaces and preceding comma
*/
	strcpy(lbuf,p);
	strcat(lbuf,",");
	q = lbuf;
	if (lbuf[0] == ',') q++;
/*
.....Parse string
*/
	ithru = UU_FALSE;
	*nids = 0;
	status = UU_SUCCESS;
	do
	{
		p = strchr(q,',');
		if (p == UU_NULL) break;
		*p = '\0';
		if (strcmp(q,"THRU") == 0)
		{
			if (*nids == 0)
			{
				status = UU_FAILURE;
				goto done;
			}
			ithru = UU_TRUE;
		}
		else
		{
			status = ul_to_number(q,&ival);
			if (status != UU_SUCCESS) break;
			if (ithru)
			{
				if (ival <= idlist[*nids-1])
				{
					status = UU_FAILURE;
					goto done;
				}
				ival *= -1.;
			}
			idlist[*nids] = ival; *nids = *nids + 1;
			ithru = UU_FALSE;
		}
		q = p + 1;
	} while (*q != '\0');
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_default_filename(cfile,cfext)
**       Builds a default filename based on the input playback file.
**    PARAMETERS   
**       INPUT  : 
**          cfext     = File extension to add to filename.
**       OUTPUT :  
**          cfile     = Generated filename.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_default_filename(cfile,cfext)
char *cfile,*cfext;
{
	int ifl;
	char *p;
	UX_pathname fullname,dir;
/*
.....Get the input filename
*/
	ifl = ncl_get_playfile(fullname);
/*
........Based on secondary clfile
*/
	if (ifl)
	{
		ul_break_fname(fullname,dir,cfile);
		p = (char *)rindex(cfile,'.');
		if (p != UU_NULL) *p = '\0';
	}
/*
........Based on part program file
*/
	else
		strcpy(cfile,UL_program);
/*
.....Add file extension
*/
	if (cfext[0] != '\0')
	{
		if (cfile[strlen(cfile)-1] != '.') strcat(cfile,".");
		strcat(cfile,cfext);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_error_msg(clab,cstr,cout)
**       Builds a default filename based on the input playback file.
**    PARAMETERS   
**       INPUT  : 
**          clab      = Label of error message to generate.
**          cstr      = String holding portion of command that generated
**                      the error.
**       OUTPUT :  
**          cout      = Output error message.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_error_msg(clab,cstr,cout)
char *clab,*cstr,*cout;
{
	int i;
	typedef struct
	{
		char *label;
		char *error;
	} errstruc;
#define MAXERR 20
	static errstruc errors[MAXERR] =
	{
		"ASCBIN",	"ASCII or BINARY expected",
		"CUTHOLD",	"CUTTER or HOLDER expected",
		"FEWVALS",	"Not enough values are specified.",
		"FILEXP",	"File name expected",
		"IDNEXP",	"Valid solid ID number expected",
		"INCHMM",	"INCHES or MM expected",
		"INVMINOR",	"Invalid minor word specified",
		"INVCOLOR",	"Invalid color specified",
		"LODOFFS",	"Could not load offsets file",
		"MACHTYPE",	"Valid machine type expected",
		"NOSPINDL",	"Spindle is not defined",
		"NOTPOS",	"Could not position machine axis",
		"NUMEXP",	"Numeric value expected",
		"NUMVALS",	"Incorrect number of values specified",
		"OUTRANGE",	"Input value out of range",
		"PAPERSZ",	"Invalid paper size specified",
		"PICTYPE",	"BMP, JPG, PS, or GIF expected",
		"POSVAL",	"Positive value expected",
		"TOOFEW",	"Not enough parameters specified",
		"YESNO",		"YES or NO expected"
	};
/*
.....Default to standard error message
*/
	sprintf(cout,"ERROR/Processing PPRINT IPV command near [%s]",cstr);
/*
.....Find proper error message
*/
	for (i=0;i<MAXERR;i++)
	{
		if (strcmp(clab,errors[i].label) == 0)
		{
			sprintf(cout,"ERROR/%s near [%s]",errors[i].error,cstr);
			break;
		}
	}
}
