#include "usysdef.h"
#if UU_COMP == UU_WIN2K 

/*********************************************************************
**  NAME:  zwntcalls.c
**
**
**    CONTAINS:
**
**			uz_wnt_callfunc(unsigned int)
**			uz_load_accel()
**			uz_parse_wntkey()
**			uw_ntstatus_calls
**			uz_ntparse_frmkey
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			zwntcalls.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			01/20/17 , 10:23:38
**    
*********************************************************************/
#include "windows.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "dmark.h"
#include "driver.h"
#include "dselect.h"
#include "bsym.h"
#include "ustdio.h"
#include "uhep.h"
#include "mdcpln.h"
#include "mattr.h"
#include "mdunits.h"
#include "nclicons.h"
#include "view.h"
#include "zkeysym.h"
#include "atext.h"
#include "wsntres.h"
#include "dmotif.h"
#include "lipv.h"

extern char UR_dpn[];

ACCEL *UZ_ncl_accel, UZ_form_hotkey[4];
int UZ_ncl_accelnum;

int NT_FuncEvent, NT_FuncEof;
extern ATXT_FRM UA_txtattr, UB_txtattr;
extern UX_libdata_bag UB_spl_libdata_rec;
extern UX_libdata_bag UB_libdata_rec;
extern int UV_current_sview;
extern int MSLite;

#define NALPHAKEYS 18
#define SELST ud_lpop()
#define SELEND ud_lpsh(UU_FALSE)

static char *alpha[NALPHAKEYS]={"0","1","2","3","4","5","6","7","8","9",
   ".",",","+","-","/","*","<-","ENTER"};

/*********************************************************************
**	 I_FUNCTION : uz_ntstatis_calls(int sub)
**			execute status calls according status sub number		
**
**	 PARAMETERS	
**		 INPUT  :
**			sub: status ID sub number
**		 OUTPUT :
**				None
**	 RETURNS: None
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
void uz_ntstatus_calls(sub)
int sub;
{
	int i,irtn, indx;
	int type;
	short app;
	UZ_keytable ktab;
	char func[80];
	func[0] = '\0';
/*
.....first search for the function name
*/
	i = 0;
	while (UU_TRUE)
	{
		if (strcmp(UZ_statfuncs[i].name,"~END~") == 0) break;
		if (strcmp(UZ_statfuncs[i].name,UZ_statfuncs[sub].name) == 0)
		{
			strcpy(func, UZ_statfuncs[i].func);
			break;
		}
		i++;
	}
	if (func[0]!='\0')
	{
		if (uz_which_keydef(func,&type,&sub,&app) == UU_SUCCESS)
		{
			ktab.type = type;
			ktab.sub = sub;
			ktab.flag = app;
			ktab.params = UU_NULL;
			irtn = uz_user_keydef(ktab,&indx,1);
		}
	}
}

/*********************************************************************
**	 I_FUNCTION : uz_putid_stattb(fid, sub)
**			Put the function ID in statusbar table		
**
**	 PARAMETERS	
**		 INPUT  :
**			fid: status button function ID
**			sub: index of the table
**		 OUTPUT :
**			none
**				
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
void uz_putid_stattb(fid, sub)
int fid, sub;
{

	int i;
	for (i=0; i<MAX_STAT_FUNC;i++)
	{
		if (UZ_status_table[sub][i]==fid)
			break;
		if (UZ_status_table[sub][i]==-1)
		{
			UZ_status_table[sub][i] = fid;
			break;
		}
	}
	if (i==MAX_STAT_FUNC)
	{
		ud_wrerr ("Too many functions tie to one status name.");
	}
}

/*********************************************************************
**	 I_FUNCTION : uz_ntcnvtfunc_to_num(func, parms, num)
**			find index number in a global function structure		
**	
**
**	 PARAMETERS	
**		 INPUT  :
**			name: status button name
**			
**		 OUTPUT :
**			num: index number of function structure array
**				
**	 RETURNS: -1: not found
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_ntstatname_to_num(name, func, descrip, num, subindx)	
int *num, *subindx;
char *name, *func, *descrip;
{
	char mydes[80];
	int i, sub, stat;
	UINT fid;

/*
..... check if it is a valid status function name
*/
	ul_to_upper(name);
	i = 0;
	sub = -1;
	while (UU_TRUE)
	{
		if (strcmp(UZ_statfuncs[i].name,"~END~") == 0) break;
		if (strcmp(UZ_statfuncs[i].name,name) == 0) 
		{
			sub = UZ_statfuncs[i].sub;
			break;
		}
		i++;
	}
	*subindx = sub;
	if (sub==-1)
		return -1;
	if (func[0]=='\0')
/*
.....use default function
*/
		strcpy(func, UZ_statfuncs[sub].func);

	if (descrip[0] == '\0') strcpy(mydes,UZ_statfuncs[sub].name);
	else strcpy(mydes,descrip);
	stat = uz_ntcnvtfunc_to_num(func, UU_NULL, mydes, num);
	if (stat==-1)
		return -1;
/*
.....put the new function ID into a status name ID table
.....which store deferent function ID into one status name
.....because one status name does not tie to one single function
.....(single ID)
*/
	fid = *num + WM_APP + UDM_MAX_MENU + UZ_MAX_KEYITEM;
	uz_putid_stattb(fid, sub);
	return 0;
}

/*********************************************************************
**	 I_FUNCTION : uz_ntcnvtnum_to_func(num, func, flag)
**			find function name in a global function structure		
**
**	 PARAMETERS	
**		 INPUT  :
**			num: index number of function structure array
**			flag: 1: for accelerator key
**				  2: for menu button
**		 OUTPUT :
**			func: function name
**			parms: parameter string
**	
**	 RETURNS: None
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_ntcnvtnum_to_func(num, func, parms, flag)
int num, flag;
char *func, *parms;
{
	if ((num>=UZ_MAX_MENUITEM)&&(flag==2))
		return -1;
	if ((num>=UZ_MAX_KEYITEM)&&(flag==1))
		return -1;
	if (num<0)
		return -1;
	if (flag==1)
	{
		strcpy(func, UZ_accelcalls[num].fname);
		if (UZ_accelcalls[num].fname[0]=='\0')
			return -1;
		if (UZ_accelcalls[num].params==NULL)
			parms[0] = '\0';
		else
			strcpy(parms, UZ_accelcalls[num].params);
	}
	else
	{
		strcpy(func, UZ_itemcalls[num].fname);
		if (UZ_itemcalls[num].params==NULL)
			parms[0] = '\0';
		else
			strcpy(parms, UZ_itemcalls[num].params);
	}
	return 0;
}
/*********************************************************************
**	 I_FUNCTION : uz_ntmenunum_to_num(menunum, bnum)
**			put the menu number into in a global function structure
**				and return the indx number		
**	
**	 PARAMETERS	
**		 INPUT  :
**			menunum: menu number
**			
**		 OUTPUT :
**			num: index number of function structure array
**				
**	 RETURNS: None
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_ntmenunum_to_num(menunum, bnum)
int *bnum, menunum;
{
	if (menunum>=UDM_MAX_MENU)
		return -1;
	*bnum = UZ_menu_count;
	UZ_menucalls[*bnum] = menunum;
	UZ_menu_count++;
	return 0;
}

/*********************************************************************
**	 I_FUNCTION : uz_ntcnvtnum_to_menu(num, menunum)
**			find menu index number in a global function structure		
**
**	 PARAMETERS	
**		 INPUT  :
**			num: index number of function structure array
**		 OUTPUT :
**			menunum: menu number
**	
**	 RETURNS: None
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_ntcnvtnum_to_menu(num, menunum)
int num, *menunum;
{
	if (num>=UDM_MAX_MENU)
		return -1;
	*menunum = UZ_menucalls[num];
	return 0;
}

/*********************************************************************
**	 I_FUNCTION : uz_ntcnvtfunc_to_num(func, parms, num)
**			find index number in a global function structure		
**	
**
**	 PARAMETERS	
**		 INPUT  :
**			func: function name
**			parms: parameter string
**			
**		 OUTPUT :
**			num: index number of function structure array
**				
**	 RETURNS: None
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_ntcnvtfunc_to_num(func, parms, descrip, num)
int *num;
char *func, *parms, *descrip;
{
	short app;
	int i, sub, type,nc;
	UU_LOGICAL match;
/*
.....first check if this function already have a coresponding
.....index number, if yes, just return the index
.....otherwise, if we repeat recreate "recent file" since
.....we need update the menu every time we open a file
.....then, this UZ_item_count will be easyly more than 
.....UZ_MAX_MENUITEM = 2000 and cause a memory error.
*/
	match = UU_FALSE;
	for (i=0; i<=UZ_item_count;i++)
	{
		if ((stricmp(UZ_itemcalls[i].fname, func)==0)
			&&(stricmp(UZ_itemcalls[i].descrip, descrip)==0))
		{
			if (UZ_itemcalls[i].params == UU_NULL)
			{
				if (parms==UU_NULL) match = UU_TRUE;
				else if (parms[0]=='\0') match = UU_TRUE;
			}
			else if (parms != UU_NULL)
			{
				if (stricmp(UZ_itemcalls[i].params,parms)==0) match = UU_TRUE;
			}
/*
......found the match
*/				
			if (match)
			{
				*num = i;
				return 0;
			}
		}
	}
/*
.....first check if it is a valid function name
*/
	if (uz_which_keydef(func,&type,&sub,&app) == UU_SUCCESS)
	{
		*num = UZ_item_count;
		strncpy(UZ_itemcalls[*num].fname, func, 29);
		UZ_itemcalls[*num].fname[29] = '\0';
/*		strncpy(UZ_itemcalls[*num].params, parms, 39);
		UZ_itemcalls[*num].params[39] = '\0'; */
/*
.....changed made here because we can't just assign UZ_itemcalls.params
.....because when we reload menu, we free UDM_menu[inc].menus[i].params but
.....no way to free UZ_itemcalls[i].params at that place. So we still malloc
.....UZ_itemcalls[i].params here and free later
*/
/*		UZ_itemcalls[*num].params = parms;  */
		if (parms==NULL)
			nc = 0;
		else
			nc = strlen(parms);
		if (nc==0)
			UZ_itemcalls[*num].params = NULL;
		else
		{
			UZ_itemcalls[*num].params = (char *)uu_malloc((nc+1)*sizeof(char));
			strcpy(UZ_itemcalls[*num].params, parms);
		}
		strncpy(UZ_itemcalls[*num].descrip, descrip, 39);
		UZ_itemcalls[*num].descrip[39] = '\0';
		UZ_item_count++;
		return 0;
	}
	return -1;
}

/*********************************************************************
**	 I_FUNCTION : uz_ntkeyfunc_to_num(func, parms, num)
**			find index number in a global accelerator function structure		
**	
**
**	 PARAMETERS	
**		 INPUT  :
**			func: function name
**			parms: parameter string
**
**		 OUTPUT :
**			num: index number of function structure array
**				
**	 RETURNS: None
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_ntkeyfunc_to_num(func, parms, num)
int *num;
char *func, *parms;
{
	short app;
	int i,sub, type,nc;
	char *p;
/*
.....first check if it is a valid function name
*/
	*num = UZ_accel_count;
	for (i=0; i<UZ_accel_count;i++)
	{
		if (stricmp(UZ_accelcalls[i].fname, func)==0)
		{
			if ((strlen(parms)==0)&&
				((UZ_accelcalls[i].params==UU_NULL)||(strlen(UZ_accelcalls[i].params)==0)))
			{
				*num = i;
				break;
			}
			if (stricmp(UZ_accelcalls[i].params, parms)==0)
			{
				*num = i;
				break;
			}
		}
	}
	if (uz_which_keydef(func,&type,&sub,&app) == UU_SUCCESS)
	{
/*
.....check if the function already saved
*/
/*		*num = UZ_accel_count; */
		strncpy(UZ_accelcalls[*num].fname, func, 29);
		UZ_accelcalls[*num].fname[29] = '\0';
/*		strncpy(UZ_accelcalls[*num].params, parms, 39);
		UZ_accelcalls[*num].params[39] = '\0'; */
		if (UZ_accelcalls[*num].params!=NULL)
			uu_free (UZ_accelcalls[*num].params);
		nc = strlen(parms);
		if (nc==0)
			UZ_accelcalls[*num].params = NULL;
		else
		{
			UZ_accelcalls[*num].params = (char *)uu_malloc((nc+1)*sizeof(char));
			strcpy(UZ_accelcalls[*num].params, parms);
		}
		UZ_accelcalls[*num].descrip[0] = '\0';
		if (i==UZ_accel_count)
			UZ_accel_count++;
		return 0;
	}
	else
	{
		p = strchr(func,'.');
		if (p==NULL) return -1;
		if (strcmp(p, ".menu")!=0) return -1;
/*		*num = UZ_accel_count; */
		strncpy(UZ_accelcalls[*num].fname, func, 29);
		UZ_accelcalls[*num].fname[29] = '\0';
/*		strncpy(UZ_accelcalls[*num].params, parms, 39);
		UZ_accelcalls[*num].params[39] = '\0'; */
		if (UZ_accelcalls[*num].params!=NULL)
			uu_free (UZ_accelcalls[*num].params);
		nc = strlen(parms);
		if (nc==0)
			UZ_accelcalls[*num].params = NULL;
		else
		{
			UZ_accelcalls[*num].params = (char *)uu_malloc((nc+1)*sizeof(char));
			strcpy(UZ_accelcalls[*num].params, parms);
		}
		UZ_accelcalls[*num].descrip[0] = '\0';
		if (i==UZ_accel_count)
			UZ_accel_count++;
		return 0;
	}
	return -1;
}

/*********************************************************************
**	 I_FUNCTION : uz_ntget_dspt(num, descript, flag)
**			get function description in a global function structure		
**
**	 PARAMETERS	
**		 INPUT  :
**			num: index number of function structure array
**			flag: 1: for accelerator key
**					2: for menu/status bar button
**		 OUTPUT :
**			descript: function description
**				
**	 RETURNS: None
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_ntget_dspt(num, descript, flag)
int num, flag;
char *descript;
{
	char func1[80], func2[80];
	int i, inum;
	descript[0] = '\0';
	if ((flag==1)||(flag==2))
	{
		if (flag==2)
		{
			if (num>=UZ_item_count)
				return -1;
			strcpy(descript, UZ_itemcalls[num].descrip);
		}
		else if (flag==1)
		{
			if (num>=UZ_accel_count)
				return -1;
			strcpy(descript, UZ_accelcalls[num].descrip);
		}
		if (descript[0]!='\0')
			return 0;
/*
.....if user does not defined in menu
.....use the default one
*/
		if (flag==2)
			strcpy(func1, UZ_itemcalls[num].fname);
		else if (flag==1)
			strcpy(func1, UZ_accelcalls[num].fname);
		i = 0;
		inum = -1;
		while (UU_TRUE)
		{
			if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
			strcpy (func2,UZ_keyfuncs[i].name);
			if (strcmp(func2,func1) == 0)
			{
				inum = i;
				break;
			}
			i++;
		}
		if (inum!=-1)
		{
			strcpy(descript, UZ_keyfuncs[i].descrip);
			return 0;
		}
	}
	return -1;
}

/*********************************************************************
**	 I_FUNCTION : uz_wnt_callfunc(int num)
**		This function accepts a key definition structure as input and
**		optionally executes its user defined function.
**	 PARAMETERS	
**		 INPUT  :
**			num: button number 
**		 OUTPUT :
**				None
**	 RETURNS: 0 = Normal function call.
**	          1 = DAS function call (index contains DAS function).
**	          2 = This key is not associated with a function.
**	          3 = Alpha key.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_wnt_callfunc(int num, int flag)
{
	char *index;
	char func[80], func1[80], buf[256], parms[500];
	UZ_keytable ktab;
	short app;
	int sub, type, stat, irtn;
	int size[2], pos[2];

	stat = uz_ntcnvtnum_to_func(num, func, parms, flag);

	strcpy(func1, func);
	if (uz_which_keydef(func,&type,&sub,&app) == UU_SUCCESS)
	{
		ktab.type = type;
		ktab.sub = sub;
		ktab.flag = app;
		ktab.params = parms;
		index = buf;
/*	
...added for record menu
*/
		ud_rpwrmenu(func, parms, func);

		irtn = uz_user_keydef(ktab,&index,1);
		if (irtn == 1)
		{
			if (UD_pickmode==1)
			{
				NT_FuncEvent = 10000 + sub;
				NT_FuncEof = 2;
/*
.....we must post this user event
*/
				uw_ntuser_event(NT_FuncEvent, NT_FuncEof);
			}
			else
			{
				ud_rpwrmenu(func1, parms, func1);
				uz_daskey1(sub, parms);
			}
		}
/*
........Propagate ALPHA key to graphics window
*/
		else if (irtn == 3)
		{
/*
......following function is not working
*/
/*			uw_ntsend_keymsg(WM_KEYDOWN, sub,1);
*/
			uw_ntinsert_cmd(sub);
		}
		if (irtn == 5)
		{
			if (index[0]!='\0')
				uw_ntinsert_cmdstr(index);
		}
		return 0;
	}
	else
/*
......menu
*/
	{
		ud_rpwrmenu(func, parms, func);
		pos[0] = -1;
		pos[1] = -1;
		size[0] = -1;
		size[1] = -1;
		if (udm_read_menu(func,pos,size, 1, 1, -1) != UU_SUCCESS)
		{
			sprintf(buf,"Could not load menu: %s",func);
			uw_nterror(buf);
		}
	}
	return  -1;
}
/*********************************************************************
**	 E_FUNCTION : uz_load_accel()
**		This function will translate all key defination
**		in 
**		and create global accelerator array
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_load_accel()
{
	char serror[80],buf[80];
	int i,status,stat,opend,numint,istat;
	BYTE  ktyp;
	WORD  kkey, kid;
	UX_pathname fullname;
	FILE *fptr;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	opend = 0;

	UZ_ncl_accelnum = 0;
	UZ_form_hotkey[0].cmd = -1;
	UZ_form_hotkey[1].cmd = -1;
	UZ_form_hotkey[2].cmd = -1;
	UZ_form_hotkey[3].cmd = -1;
/*
.....initial for 
*/
	for (i=0;i<UZ_MAX_KEYITEM;i++)
	{
		UZ_accelcalls[i].params = UU_NULL;
	}
	for (i=0;i<UZ_MAX_MENUITEM;i++)
	{
		UZ_itemcalls[i].params = UU_NULL;
	}
/*
.....Open key definition file
*/
	if (MSLite)
		stat = uz_open_keydef("UZ_MSL_KEYDEFS",fullname,&fptr,1);
	else if (LW_nclipv==LW_STANDALONE)
		stat = uz_open_keydef("UZ_IPV_KEYDEFS",fullname,&fptr,1);
	else
		stat = uz_open_keydef("UZ_CAM_KEYDEFS",fullname,&fptr,1);
	if (stat != UU_SUCCESS) goto failed;
	opend = 1;
/*
.....Read a record
*/
	do
	{
		stat = ul_fread(fptr,buf,sizeof(buf),&numint);
		if (stat == UX_EOF)
		{
			if (numint<=0) goto done;
			buf[numint+1] = '\0';
		}
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			sprintf (serror,"Error reading from %s E%d.",fullname,stat);
			ud_wrerr (serror);
			goto failed;
		}
		if (strlen(buf)==0) continue;
/*
.....Check for record type
*/
		istat = uz_parse_wntkey(buf,numint,&ktyp,&kkey,&kid);
		if (ktyp==0) continue;
/*
.....Invalid syntax
*/
		if (istat != UU_SUCCESS)
		{
			sprintf(serror,"Key definition file syntax error. %s",buf);
			ud_wrerr(serror);
		}
/*
.....Store key definition
*/
		else
		{
			UZ_ncl_accel[UZ_ncl_accelnum].fVirt = ktyp;
			UZ_ncl_accel[UZ_ncl_accelnum].key = kkey;
			UZ_ncl_accel[UZ_ncl_accelnum].cmd = kid;
			UZ_ncl_accelnum++;
		}
	}
	while (stat == UU_SUCCESS || stat == UX_NO_SPACE);
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (opend == 1) ux_fclose0(fptr);
	return(status);
}

/*********************************************************************
**	 I_FUNCTION : uz_parse_wntkey(char *buf, int numint, BYTE* ktyp,WORD* ksub,WORD* kid)
**		This function parses a key definitions record and returns the
**		key and its programmed value.
**	 PARAMETERS	
**		 INPUT  :
**			buf    = Input key definition record.
**			nc     = Number of chars in 'buf'.
**		 OUTPUT :
**			ktyp   = Specifies the accelerator flags.
**					This member can be a combination of the following values: 
**				FALT: The ALT key must be held down when the accelerator key is pressed. 
**				FCONTROL: The CTRL key must be held down when the accelerator key is pressed. 
**				FSHIFT: The SHIFT key must be held down when the accelerator key is pressed. 
**				FVIRTKEY: The key member specifies a virtual-key code. If this flag is not specified, key is assumed to specify an ASCII character code. 
**			ksub   = Specifies the accelerator key. 
**					This member can be either a virtual-key 
**					code or an ASCII character code
**			kid:  = Specifies the accelerator ID 
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_parse_wntkey(char *buf, int numint, BYTE* ktyp,WORD* ksub,WORD* kid)
{
	int i,status,nc,siz,ictrl,ishft, ialt,knum;
	char *p,*index(),str[80],tmp[80],tmp1[80],parms[40];
	int endkey;
	static char *keysym[] = { 
/*
.....normal characters
*/
	"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", 
	"n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
/*
......normal numbers
*/
	"0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
/*
.....some other keys
*/
	"-", "=", "[", "]", "\\", ";", "'", ",", ".", "/", 
/*
......special keys
*/
	"pause", "scroll_lock", /*"multi_key",*/ "home", "prior", "next", "end",
	/*"select",*/ "print", "execute", "insert", /*"undo", "find", */"cancel",
	"help", /*"break",*/ "num_lock",
/*
......key pad 
*/
	"kp_space", "kp_tab", "kp_enter", /*"kp_f1", "kp_f2", "kp_f3", "kp_f4",*/
	/*"kp_equal",*/ "kp_multiply", "kp_add", "kp_separator", "kp_subtract",
	"kp_decimal", "kp_divide", "kp_0", "kp_1", "kp_2", "kp_3", "kp_4",
	"kp_5", "kp_6", "kp_7", "kp_8", "kp_9",
/*
......function keys up to f12 for PC
*/
	"f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10",
	"f11", "f12", /*"f13", "f14", "f15", "f16", "f17", "f18", "f19", "f20",
	"f21", "f22", "f23", "f24", "f25", "f26", "f27", "f28", "f29", "f30",
	"f31", "f32", "f33", "f34", "f35", */
/*
.....arrow key
*/
	"/\\", "\\/",
	"~endkey"
};
/*
.....Assume success
*/
	status = UU_SUCCESS;
	*ktyp = 0;
/*
.....Purge record of spaces
*/
	strcpy(str,buf);
	nc = strlen(buf);
	ul_strip_blanks(str,&nc);
	if (nc == 0)
	{
		*ktyp = 0;
		goto done;
	}
/*
.....Check for control character (^)
*/
	ictrl = 0;
	if (str[0] == '^' && (str[1] != '=' || str[2] == '='))
	{
		ictrl = 1;
		strcpy(str,&str[1]);
	}
	ialt = 0;
	if (str[0] == '~')
	{
		ialt = 1;
		strcpy(str,&str[1]);
	}
/*
.....Break out key to define
*/
	p = index(str,'=');
	if (p == 0) goto failed;
	if (p == &str[0]) p = index(&str[1],'=');
	if (p <= &str[0]) goto failed;
	siz = p - &str[0];
	strncpy(tmp,str,siz);
	tmp[siz] = '\0';

	endkey = 0;
	ishft = 0;
	i = 0;
	while(1)
	{
		if (strcmp(keysym[i],"~endkey")==0)
		{
			endkey = 1;
			break;
		}
		if (strcmp(tmp, keysym[i]) == 0) break;
		strcpy(tmp1, keysym[i]);
		ul_to_upper(tmp1);
		if (strcmp(tmp,tmp1) == 0)
		{
			ishft = 1;
			break;
		}
		i++;
	}
	if (endkey==1)
		goto failed;
	if (i<=35)
/*
......normal letter and number
*/
	{
		*ksub = toupper(keysym[i][0]);
	}
	else if ((i>=36)&&(i<46))
		*ksub = keysym[i][0];
	else if (strcmp(tmp,  "pause")== 0)
		*ksub = VK_PAUSE;
	else if (strcmp(tmp,  "scroll_lock")== 0)
		*ksub = VK_SCROLL;
	else if (strcmp(tmp,  "home")== 0)
		*ksub = VK_HOME;
	else if (strcmp(tmp,  "prior")== 0)
		*ksub = VK_PRIOR;
	else if (strcmp(tmp,  "next")== 0)
		*ksub = VK_NEXT;
	else if (strcmp(tmp,  "end")== 0)
		*ksub = VK_END;
	else if (strcmp(tmp,  "print")== 0)
		*ksub = VK_PRINT;
	else if (strcmp(tmp,  "execute")== 0)
		*ksub = VK_EXECUTE;
	else if (strcmp(tmp,  "insert")== 0)
		*ksub = VK_INSERT;
	else if (strcmp(tmp,  "cancel")== 0)
		*ksub = VK_CANCEL;
	else if (strcmp(tmp,  "help")== 0)
		*ksub = VK_HELP;
	else if (strcmp(tmp,  "num_lock")== 0)
		*ksub = VK_NUMLOCK;
	else if (strcmp(tmp,  "kp_space")== 0)
		*ksub = VK_SPACE;
	else if (strcmp(tmp,  "kp_tab")== 0)
		*ksub = VK_TAB;
	else if (strcmp(tmp,  "kp_multiply")== 0)
		*ksub = VK_MULTIPLY;
	else if (strcmp(tmp,  "kp_add")== 0)
		*ksub = VK_ADD;
	else if (strcmp(tmp,  "kp_subtract")== 0)
		*ksub = VK_SUBTRACT;
	else if (strcmp(tmp,  "kp_separator")== 0)
		*ksub = VK_SEPARATOR;
	else if (strcmp(tmp,  "kp_decimal")== 0)
		*ksub = VK_DECIMAL;
	else if (strcmp(tmp,  "kp_divide")== 0)
		*ksub = VK_DIVIDE;
	else if (strcmp(tmp,  "kp_enter")== 0)
		*ksub = VK_RETURN;
	else if (strcmp(tmp,  "kp_0")== 0)
		*ksub = VK_NUMPAD0;
	else if (strcmp(tmp,  "kp_1")== 0)
		*ksub = VK_NUMPAD1;
	else if (strcmp(tmp,  "kp_2")== 0)
		*ksub = VK_NUMPAD2;
	else if (strcmp(tmp,  "kp_3")== 0)
		*ksub = VK_NUMPAD3;
	else if (strcmp(tmp,  "kp_4")== 0)
		*ksub = VK_NUMPAD4;
	else if (strcmp(tmp,  "kp_5")== 0)
		*ksub = VK_NUMPAD5;
	else if (strcmp(tmp,  "kp_6")== 0)
		*ksub = VK_NUMPAD6;
	else if (strcmp(tmp,  "kp_7")== 0)
		*ksub = VK_NUMPAD7;
	else if (strcmp(tmp,  "kp_8")== 0)
		*ksub = VK_NUMPAD8;
	else if (strcmp(tmp,  "kp_9")== 0)
		*ksub = VK_NUMPAD9;
	else if (stricmp(tmp,  "f1")== 0)
		*ksub = VK_F1;
	else if (stricmp(tmp,  "f2")== 0)
		*ksub = VK_F2;
	else if (stricmp(tmp,  "f3")== 0)
		*ksub = VK_F3;
	else if (stricmp(tmp,  "f4")== 0)
		*ksub = VK_F4;
	else if (stricmp(tmp,  "f5")== 0)
		*ksub = VK_F5;
	else if (stricmp(tmp,  "f6")== 0)
		*ksub = VK_F6;
	else if (stricmp(tmp,  "f7")== 0)
		*ksub = VK_F7;
	else if (stricmp(tmp,  "f8")== 0)
		*ksub = VK_F8;
	else if (stricmp(tmp,  "f9")== 0)
		*ksub = VK_F9;
	else if (stricmp(tmp,  "f10")== 0)
		*ksub = VK_F10;
	else if (stricmp(tmp,  "f11")== 0)
		*ksub = VK_F11;
	else if (stricmp(tmp,  "f12")== 0)
		*ksub = VK_F12;
	else if (strcmp(tmp,  "/\\")== 0)
		*ksub = VK_UP;
	else if (strcmp(tmp,  "\\/")== 0)
		*ksub = VK_DOWN;
	*ktyp = FNOINVERT;
	if (!((i>=36)&&(i<46)))
		*ktyp = FVIRTKEY;
	if (ictrl)
		*ktyp = *ktyp | FCONTROL;
	if (ialt)
		*ktyp = *ktyp | FALT;
	if (ishft)
		*ktyp = *ktyp | FSHIFT;
/*
.....check if the key defined
*/
	p++;
	strcpy(tmp,p);
/*
.....added parameter string
*/
	p = (char *)index(tmp,',');
	if (p != 0 && p != tmp)
	{
		*p = '\0';
		++p;
		while ((*p==' ') || (*p=='\t')) p++;
		strcpy(parms,p);
		ul_remove_quotes(parms);
	}
	else
		parms[0] = '\0';
/*
......convert function name string to function ID
*/
	if (uz_ntkeyfunc_to_num(tmp, parms, &knum)!=0)
		goto failed;
	*kid = WM_APP + UDM_MAX_MENU + knum;
	if (strcmp (tmp, "CAM_SCALARS")==0)
/*
.....save it in form hot key
*/
	{
		UZ_form_hotkey[0].fVirt = *ktyp ;
		UZ_form_hotkey[0].key = *ksub;
		UZ_form_hotkey[0].cmd = knum;
	}
	if (strcmp (tmp, "NCL_CALCULATOR")==0)
/*
.....save it in form hot key
*/
	{
		UZ_form_hotkey[1].fVirt = *ktyp ;
		UZ_form_hotkey[1].key = *ksub;
		UZ_form_hotkey[1].cmd = knum;
	}
	if (strcmp (tmp, "CAM_DATA_FORM")==0)
/*
.....save it in form hot key
*/
	{
		UZ_form_hotkey[2].fVirt = *ktyp ;
		UZ_form_hotkey[2].key = *ksub;
		UZ_form_hotkey[2].cmd = knum;
	}
	if (strcmp (tmp, "CAM_SYM_TABLE")==0)
/*
.....save it in form hot key
*/
	{
		UZ_form_hotkey[3].fVirt = *ktyp ;
		UZ_form_hotkey[3].key = *ksub;
		UZ_form_hotkey[3].cmd = knum;
	}
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**	 E_FUNCTION : uuz_load_accel2(keyfile)
**		This function will translate all key defination
**		in 
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_load_accel2(keyfile)
char *keyfile;
{
	char serror[80],buf[80];
	int status,stat,opend,numint,i,istat, replaced;
	BYTE  ktyp;
	WORD  kkey, kid;
	UX_pathname fullname;
	FILE *fptr;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	opend = 0;
/*
.....Open key definition file
*/
	stat = uz_open_keydef(keyfile,fullname,&fptr,1);
	if (stat != UU_SUCCESS) goto failed;
	opend = 1;
/*
.....Read a record
*/
	do
	{
		stat = ul_fread(fptr,buf,sizeof(buf),&numint);
		if (stat == UX_EOF)
		{
			if (numint<=0) goto done;
			buf[numint+1] = '\0';
		}
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			sprintf (serror,"Error reading from %s E%d.",fullname,stat);
			ud_wrerr (serror);
			goto failed;
		}
		if (strlen(buf)==0) continue;
/*
.....Check for record type
*/
		istat = uz_parse_wntkey(buf,numint,&ktyp,&kkey,&kid);
		if (ktyp==0) continue;
/*
.....Invalid syntax
*/
		if (istat != UU_SUCCESS)
		{
			sprintf(serror,"Key definition file syntax error. %s",buf);
			ud_wrerr(serror);
		}
/*
.....Store key definition
*/
		else
		{
/*
.....replace old definition if have it before
*/
			replaced = 0;
			for (i=0; i<UZ_ncl_accelnum; i++)
			{
				if ((UZ_ncl_accel[i].fVirt == ktyp) &&
					(UZ_ncl_accel[i].key == kkey))
				{
					UZ_ncl_accel[i].cmd = kid ;
					replaced = 1;
					break;
				}
			}
			if (replaced==0)
			{
				UZ_ncl_accel[UZ_ncl_accelnum].fVirt = ktyp;
				UZ_ncl_accel[UZ_ncl_accelnum].key = kkey;
				UZ_ncl_accel[UZ_ncl_accelnum].cmd = kid;
				UZ_ncl_accelnum++;
			}
		}
	}
	while (stat == UU_SUCCESS || stat == UX_NO_SPACE);
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (opend == 1) ux_fclose0(fptr);
#if UU_COMP==UU_WIN2K
	uw_ntupdate_accel();
#endif
	return(status);
}

/*********************************************************************
**	 I_FUNCTION : uz_ntparse_frmkey(char *buf, int numint, BYTE* ktyp,WORD* ksub)
**		This function parses a key definitions record and returns the
**		key and its programmed value.
**	 PARAMETERS	
**		 INPUT  :
**			buf    = Input key definition record.
**			nc     = Number of chars in 'buf'.
**		 OUTPUT :
**			ktyp   = Specifies the accelerator flags.
**					This member can be a combination of the following values: 
**				FALT: The ALT key must be held down when the accelerator key is pressed. 
**				FCONTROL: The CTRL key must be held down when the accelerator key is pressed. 
**				FSHIFT: The SHIFT key must be held down when the accelerator key is pressed. 
**				FVIRTKEY: The key member specifies a virtual-key code. If this flag is not specified, key is assumed to specify an ASCII character code. 
**			ksub   = Specifies the accelerator key. 
**					This member can be either a virtual-key 
**					code or an ASCII character code
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_ntparse_frmkey(char *buf, int numint, BYTE* ktyp,WORD* ksub)
{
	int i,status,nc,ictrl,ishft, ialt;
	char *index(),str[80],tmp1[80];
	int endkey;
	static char *keysym[] = { 
/*
.....normal characters
*/
	"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", 
	"n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
/*
......normal numbers
*/
	"0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
/*
.....some other keys
*/
	"-", "=", "[", "]", "\\", ";", "'", ",", ".", "/", 
/*
......special keys
*/
	"pause", "scroll_lock", /*"multi_key",*/ "home", "prior", "next", "end",
	/*"select",*/ "print", "execute", "insert", /*"undo", "find", */"cancel",
	"help", /*"break",*/ "num_lock",
/*
......key pad 
*/
	"kp_space", "kp_tab", "kp_enter", /*"kp_f1", "kp_f2", "kp_f3", "kp_f4",*/
	/*"kp_equal",*/ "kp_multiply", "kp_add", "kp_separator", "kp_subtract",
	"kp_decimal", "kp_divide", "kp_0", "kp_1", "kp_2", "kp_3", "kp_4",
	"kp_5", "kp_6", "kp_7", "kp_8", "kp_9",
/*
......function keys up to f12 for PC
*/
	"f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10",
	"f11", "f12", /*"f13", "f14", "f15", "f16", "f17", "f18", "f19", "f20",
	"f21", "f22", "f23", "f24", "f25", "f26", "f27", "f28", "f29", "f30",
	"f31", "f32", "f33", "f34", "f35", */
/*
.....arrow key
*/
	"/\\", "\\/",
	"~endkey"
};
/*
.....Assume success
*/
	status = UU_SUCCESS;
	*ktyp = 0;
/*
.....Purge record of spaces
*/
	strcpy(str,buf);
	nc = strlen(buf);
	ul_strip_blanks(str,&nc);
	if (nc == 0)
	{
		*ktyp = 0;
		goto done;
	}
/*
.....Check for control character (^)
*/
	ictrl = 0;
	if (str[0] == '^' && (str[1] != '=' || str[2] == '='))
	{
		ictrl = 1;
		strcpy(str,&str[1]);
	}
	ialt = 0;
	if (str[0] == '~')
	{
		ialt = 1;
		strcpy(str,&str[1]);
	}

	endkey = 0;
	ishft = 0;
	i = 0;
	while(1)
	{
		if (strcmp(keysym[i],"~endkey")==0)
		{
			endkey = 1;
			break;
		}
		if (strcmp(str, keysym[i]) == 0) break;
		strcpy(tmp1, keysym[i]);
		ul_to_upper(tmp1);
		if (strcmp(str,tmp1) == 0)
		{
			ishft = 1;
			break;
		}
		i++;
	}
	if (endkey==1)
		goto failed;
	if (i<=35)
/*
......normal letter and number
*/
	{
		*ksub = toupper(keysym[i][0]);
	}
	else if ((i>=36)&&(i<46))
		*ksub = keysym[i][0];
	else if (strcmp(str,  "pause")== 0)
		*ksub = VK_PAUSE;
	else if (strcmp(str,  "scroll_lock")== 0)
		*ksub = VK_SCROLL;
	else if (strcmp(str,  "home")== 0)
		*ksub = VK_HOME;
	else if (strcmp(str,  "prior")== 0)
		*ksub = VK_PRIOR;
	else if (strcmp(str,  "next")== 0)
		*ksub = VK_NEXT;
	else if (strcmp(str,  "end")== 0)
		*ksub = VK_END;
	else if (strcmp(str,  "print")== 0)
		*ksub = VK_PRINT;
	else if (strcmp(str,  "execute")== 0)
		*ksub = VK_EXECUTE;
	else if (strcmp(str,  "insert")== 0)
		*ksub = VK_INSERT;
	else if (strcmp(str,  "cancel")== 0)
		*ksub = VK_CANCEL;
	else if (strcmp(str,  "help")== 0)
		*ksub = VK_HELP;
	else if (strcmp(str,  "num_lock")== 0)
		*ksub = VK_NUMLOCK;
	else if (strcmp(str,  "kp_space")== 0)
		*ksub = VK_SPACE;
	else if (strcmp(str,  "kp_tab")== 0)
		*ksub = VK_TAB;
	else if (strcmp(str,  "kp_multiply")== 0)
		*ksub = VK_MULTIPLY;
	else if (strcmp(str,  "kp_add")== 0)
		*ksub = VK_ADD;
	else if (strcmp(str,  "kp_subtract")== 0)
		*ksub = VK_SUBTRACT;
	else if (strcmp(str,  "kp_separator")== 0)
		*ksub = VK_SEPARATOR;
	else if (strcmp(str,  "kp_decimal")== 0)
		*ksub = VK_DECIMAL;
	else if (strcmp(str,  "kp_divide")== 0)
		*ksub = VK_DIVIDE;
	else if (strcmp(str,  "kp_enter")== 0)
		*ksub = VK_RETURN;
	else if (strcmp(str,  "kp_0")== 0)
		*ksub = VK_NUMPAD0;
	else if (strcmp(str,  "kp_1")== 0)
		*ksub = VK_NUMPAD1;
	else if (strcmp(str,  "kp_2")== 0)
		*ksub = VK_NUMPAD2;
	else if (strcmp(str,  "kp_3")== 0)
		*ksub = VK_NUMPAD3;
	else if (strcmp(str,  "kp_4")== 0)
		*ksub = VK_NUMPAD4;
	else if (strcmp(str,  "kp_5")== 0)
		*ksub = VK_NUMPAD5;
	else if (strcmp(str,  "kp_6")== 0)
		*ksub = VK_NUMPAD6;
	else if (strcmp(str,  "kp_7")== 0)
		*ksub = VK_NUMPAD7;
	else if (strcmp(str,  "kp_8")== 0)
		*ksub = VK_NUMPAD8;
	else if (strcmp(str,  "kp_9")== 0)
		*ksub = VK_NUMPAD9;
	else if (stricmp(str,  "f1")== 0)
		*ksub = VK_F1;
	else if (stricmp(str,  "f2")== 0)
		*ksub = VK_F2;
	else if (stricmp(str,  "f3")== 0)
		*ksub = VK_F3;
	else if (stricmp(str,  "f4")== 0)
		*ksub = VK_F4;
	else if (stricmp(str,  "f5")== 0)
		*ksub = VK_F5;
	else if (stricmp(str,  "f6")== 0)
		*ksub = VK_F6;
	else if (stricmp(str,  "f7")== 0)
		*ksub = VK_F7;
	else if (stricmp(str,  "f8")== 0)
		*ksub = VK_F8;
	else if (stricmp(str,  "f9")== 0)
		*ksub = VK_F9;
	else if (stricmp(str,  "f10")== 0)
		*ksub = VK_F10;
	else if (stricmp(str,  "f11")== 0)
		*ksub = VK_F11;
	else if (stricmp(str,  "f12")== 0)
		*ksub = VK_F12;
	else if (strcmp(str,  "/\\")== 0)
		*ksub = VK_UP;
	else if (strcmp(str,  "\\/")== 0)
		*ksub = VK_DOWN;
	*ktyp = FNOINVERT;
	if (!((i>=36)&&(i<46)))
		*ktyp = FVIRTKEY;
	if (ictrl)
		*ktyp = *ktyp | FCONTROL;
	if (ialt)
		*ktyp = *ktyp | FALT;
	if (ishft)
		*ktyp = *ktyp | FSHIFT;
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}


#endif

