/*********************************************************************
**      FILENAME: zuserkeys.c
**      CONTAINS:
**           uz_load_keys()
**           uz_open_keydef()
**           uz_load_keydef_file()
**           uz_load_keydefs(keyfile,keytab,fkeytab)
**           uz_parse_keydef(buf,bnc,ktyp,ksub,dtyp,dsub);
**           uz_user_key(num,index,xflag)
**           uz_user_fkey(num,index,xflag)
**           uz_user_button(num,index,xflag)
**           uz_light_buttons()
**           uz_unlight_buttons()
**                       uz_load_keys2(keyfile);
**			uz_mfparse_frmkey
**			uz_key_pick_loc
**			uz_load_mousedef
**    MODULE NAME AND RELEASE LEVEL
**       zuserkeys.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       02/08/16 , 09:25:33
*********************************************************************/
#define ZKEYSYM
#include "usysdef.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "dmark.h"
#include "bsym.h"
#include "ustdio.h"
#include "mdcpln.h"
#include "mattr.h"
#include "mattr.h"
#include "nclicons.h"
#include "view.h"
#include "zkeysym.h"
#include "atext.h"
#include "spmouse.h"
#include "dmotif.h"
#include "lipv.h"

/*extern ATXT_FRM UA_txtattr, UB_txtattr;
extern UX_libdata_bag UB_libdata_rec;
extern int UV_current_sview;*/

extern char *uu_malloc();

#define NALPHAKEYS 18
static char *alpha[NALPHAKEYS]={"0","1","2","3","4","5","6","7","8","9",
	".",",","+","-","/","*","<-","ENTER"};
extern int Space_mouse_draw;
#if UU_COMP != UU_WIN2K
UZ_keytable UZ_form_hotkey[3];
int UZ_form_hotkey_inx[3];
#endif

static char S_svmouse_func[20][80] = {
	"", "", "","","","","","","","",
	"", "", "","","","","","","",""};
static UX_pathname Ssyspath={""};
extern char NCL_init_fstr[20];
extern int MSLite;
extern int NT_FuncEvent, NT_FuncEof;
extern int UW_dynamic_funkey;
int UZ_key_pickloc = 0;
extern int NCL_mouse_func;
extern int UZ_nclipv_view;
/*********************************************************************
**       E_FUNCTION : uz_load_keys()
**              This function is the high level routine for loading the
**              User defined key definition files.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void uz_load_keys()
{
/*
.....Load the NIS, CAM, & CAD
.....Key Definition files
*/
	if (MSLite)
		uz_load_keydefs("UZ_MSL_KEYDEFS", UZ_ipvkeytab, UZ_ipvfkeytab,
					UZ_ipvbkeytab);
	else if (LW_nclipv==LW_STANDALONE)
		uz_load_keydefs("UZ_IPV_KEYDEFS", UZ_ipvkeytab, UZ_ipvfkeytab,
					UZ_ipvbkeytab);
	else
		uz_load_keydefs("UZ_CAM_KEYDEFS",UZ_camkeytab,UZ_camfkeytab,UZ_cambkeytab);
/*
	uz_load_keydefs("UZ_NIS_KEYDEFS",UZ_niskeytab,UZ_nisfkeytab,UZ_nisbkeytab);
	uz_load_keydefs("UZ_CAD_KEYDEFS",UZ_cadkeytab,UZ_cadfkeytab,UZ_cadbkeytab);
*/
}

/*********************************************************************
**       I_FUNCTION : uz_parse_keydef(buf,bnc,ktyp,ksub,dtyp,dsub,name,parms,flag);
**              This function parses a key definitions record and returns the
**              key and its programmed value.
**       PARAMETERS     
**               INPUT  :
**                      buf    = Input key definition record.
**                      nc     = Number of chars in 'buf'.
**               OUTPUT :
**                      ktyp   = Type of key being defined.
**                               0 = Blank line, 1 = Normal key, 2 = Function key,
**                               3 = Button or Dial.
**                                      4 = menu name.
**                      ksub   = Value of key being defined.
**                      dtyp   = Application type of key definition (NIS, CAM, etc).
**                      dsub   = Key definition requested (KEY_PIKLOC, etc.).
**                      name: menu name or function name
**                      parms: parameter string for function 
**                      flag   = Valid application flag for key definition.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
/*
......added menu/function name param
......Yurong 4/4/00
*/
/*
static int uz_parse_keydef(buf,bnc,ktyp,ksub,dtyp,dsub,flag)
*/
/*
.....added parameter string
*/
static int uz_parse_keydef(buf,bnc,ktyp,ksub,dtyp,dsub,name,parms, flag)
char *buf;
int *ktyp,*ksub,*dtyp,*dsub,bnc;
char *name, *parms;
short *flag;
{
	int i,status,nc,siz,ictrl,ishft;
	char *p,*index(),*strchr(),str[80],tmp[80],tmp1[80];
/*
.....Assume success
*/
	status = UU_SUCCESS;
/*
.....Purge record of spaces
*/
	strcpy(str,buf);
	nc = bnc;
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
/*
.....Break out key to define
*/
	p = index(str,'=');
	if (p == 0) goto failed;
/*      if (p == &str[0]) p = index(&str[1],'=') + 1;*/
	if (p == &str[0]) p = index(&str[1],'=');
	if (p <= &str[0]) goto failed;
	siz = p - &str[0];
	strncpy(tmp,str,siz);
	tmp[siz] = '\0';
/*
........Determine key to define
...........Function keys
*/
	ishft = 0;
	for (i=0;i<NFKEYSYMS;i++)
	{
		if (strcmp(tmp,zfkeysym[i]) == 0) break;
		strcpy(tmp1,zfkeysym[i]);
		ul_to_upper(tmp1);
		if (strcmp(tmp,tmp1) == 0)
		{
			ishft = 1;
			break;
		}
	}
	if (i < NFKEYSYMS)
	{
		*ktyp = 2;
		*ksub = i;
		if (ictrl == 1) *ksub = *ksub + NFKEYSYMS * 2;
		else if (ishft == 1) *ksub = *ksub + NFKEYSYMS;
		goto get_func;
	}
/*
...........Buttons & Dials
*/
	strcpy(tmp1,tmp);
	ul_to_upper(tmp1);
	for (i=0;i<NBKEYSYMS;i++)
	{
		if (strcmp(tmp1,zbkeysym[i]) == 0) break;
	}
	if (i < NBKEYSYMS)
	{
		*ktyp = 3;
		*ksub = i;
		goto get_func;
	}
/*
...........Normal keys
*/
	nc = strlen(tmp);
	if (nc == 1)
	{
		*ktyp = 1;
		*ksub = tmp[0] - 1;
		if (ictrl == 1)
		{
			if (*ksub >= 96 && *ksub <= 121) *ksub = *ksub - 32;
			if (*ksub >= 64 && *ksub <= 89)
			{
				*ksub = *ksub - 64;
			}
			else 
			{
				*ksub = *ksub + NKEYSYMS;
			}
		}
	}
	else if (nc == 2)
	{
		if (strcmp(tmp,"/\\") == 0)
		{
			*ktyp = 1;
			*ksub = 127;
		}
		else if (strcmp(tmp,"\\/") == 0)
		{
			*ktyp = 1;
			*ksub = 128;
		}
		else
		{
			goto failed;
		}
	}
	else
	{
		goto failed;
	}
/*
.....Break out key definition
*/
get_func:;
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
	strcpy(name,tmp);
/*
......added menu name to be assigned
......Yurong 4/4/00
*/
/*
	if (uz_which_keydef(tmp,dtyp,dsub,flag) != UU_SUCCESS) goto failed;
*/
	if (uz_which_keydef(tmp,dtyp,dsub,flag) != UU_SUCCESS) 
	{
			p = strchr(tmp,'.');
			if (p==NULL) goto failed;
			if (strcmp(p, ".menu")!=0) goto failed;
			strcpy(name, tmp);
			*dtyp = MENUKEY;
			*ktyp = 4;
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
**       I_FUNCTION : uz_mfparse_frmkey(char *buf, int numint, int* ktyp,
ksub* ksub)
**              This function parses a key definitions record and returns the
**              key and its programmed value.
**       PARAMETERS     
**               INPUT  :
**                      buf    = Input key definition record.
**                      numint     = Number of chars in 'buf'.
**               OUTPUT :
**                      ktyp   = Type of key being defined.
**                               0 = Blank line, 1 = Normal key, 2 = Function key,
**                               3 = Button or Dial.
**                                      4 = menu name.
**                      ksub   = Value of key being defined.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int uz_mfparse_frmkey(buf, numint, ktyp, ksub)
char *buf;
int *ktyp,*ksub, numint;
{
	int i,status,nc,ictrl,ishft;
	char *index(),*strchr(),str[80],tmp[80],tmp1[80];
/*
.....Assume success
*/
	status = UU_SUCCESS;
/*
.....Purge record of spaces
*/
	strcpy(str,buf);
	nc = numint;
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
/*
........Determine key to define
...........Function keys
*/
	ishft = 0;
	for (i=0;i<NFKEYSYMS;i++)
	{
		if (strcmp(str,zfkeysym[i]) == 0) break;
		strcpy(tmp1,zfkeysym[i]);
		ul_to_upper(tmp1);
		if (strcmp(tmp,tmp1) == 0)
		{
			ishft = 1;
			break;
		}
	}
	if (i < NFKEYSYMS)
	{
		*ktyp = 2;
		*ksub = i;
		if (ictrl == 1) *ksub = *ksub + NFKEYSYMS * 2;
		else if (ishft == 1) *ksub = *ksub + NFKEYSYMS;
		goto done;
	}
/*
...........Normal keys
*/
	nc = strlen(tmp);
	if (nc == 1)
	{
		*ktyp = 1;
		*ksub = tmp[0] - 1;
		if (ictrl == 1)
		{
			if (*ksub >= 96 && *ksub <= 121) *ksub = *ksub - 32;
			if (*ksub >= 64 && *ksub <= 89)
			{
				*ksub = *ksub - 64;
			}
			else 
			{
				*ksub = *ksub + NKEYSYMS;
			}
		}
	}
	else if (nc == 2)
	{
		if (strcmp(tmp,"/\\") == 0)
		{
			*ktyp = 1;
			*ksub = 127;
		}
		else if (strcmp(tmp,"\\/") == 0)
		{
			*ktyp = 1;
			*ksub = 128;
		}
		else
		{
			goto failed;
		}
	}
	else
	{
		goto failed;
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
**  E_FUNCTION : uz_open_keydef(keyfile,fullname,fptr)
**       This function opens a key definition file and if it is the first
**       keydef file opened, it stores the path as the system path for
**       future openings of keydef files.
**  PARAMETERS     
**     INPUT  :
**       keyfile = Key definition file to open.  Can be an environmental
**                 variable.
**     OUTPUT :
**       fullname = Full name of file opened.
**       fptr     = File pointer to opened file.
**  RETURNS: UU_FAILURE if file could not be opened.
**  SIDE EFFECTS: none.
**  WARNINGS: none.
*********************************************************************/
int uz_open_keydef(keyfile,fullname,fptr, eflag)
char *keyfile,*fullname;
FILE **fptr;
int eflag;
{
	int status,stat;
	char *str,*ux_getenv(),sfname[42],serror[80];
	UU_LOGICAL found;
	char *pathlistptr = UU_NULL;
/*
.....Check for environmental variable
*/
	status = UU_FAILURE;

	str = ux_getenv(keyfile,UX_NPRTERRS);
	if (str != 0) 
	{
		ux_get_syspath(keyfile, &pathlistptr, fullname, 
			&found, UX_NPRTERRS|UX_NCHK|UX_NQUOTES);
		uu_lsdel(pathlistptr);
	}
	else strcpy(fullname,keyfile);
/*
.....See if file exists
*/
	stat = ul_open_mod_file("UU_USER_SETTINGS", "init", NCL_init_fstr, UU_NULL,
		fullname, 2, fptr);
	if ((stat != UU_SUCCESS)||(fptr==UU_NULL))
	{
		if (eflag)
		{
			ul_short_filename(fullname,sfname,40);
			sprintf (serror,"Cannot open Key definition file %s",sfname);
			ud_wrerr (serror);
		}
		goto done;
	}
	if (Ssyspath[0] == '\0') 
		ul_get_full_dir(NCL_init_fstr, Ssyspath);

	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**  E_FUNCTION : uz_load_keydef_file(keyfile)
**       This function loads a key definition file.  The filename can
**       either be passed in as 'keyfile' or this routine will prompt
**       for the key definition file if it is blank.
**  PARAMETERS     
**     INPUT  :
**       keyfile = Key definition file to load.
**     OUTPUT : none
**  RETURNS: UU_FAILURE if file could not be loaded.
**  SIDE EFFECTS: none.
**  WARNINGS: none.
*********************************************************************/
int uz_load_keydef_file(keyfile)
char *keyfile;
{
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	int status,nc,user_opened;
	char banner[30],fname[UX_MAX_PATH_LEN],descrip[UX_MAX_PATH_LEN];
	char ext[UX_SUFFIX_LEN];
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....No filename given
........Setup the file browser
*/
	user_opened = 0;
	if (keyfile[0] == '\0')
	{
		strcpy(banner,"Load Key Definition File");
		fname[0] = '\0'; nc = 0;
/*
		strcpy(ldir[1], dir2); */
#if UU_COMP == UU_WIN2K
		strcpy(ext,"*.wnt");
#else
		strcpy(ext,"*.sgi");
#endif
		sprintf(descrip,"Key Files (%s)",ext);
/*
........Get the key file to load
*/
		strcpy(paths, Ssyspath);
		strcat(paths, ";");
		strcat(paths, "%UU_USER_SETTINGS\\init");
		strcpy(path_des, "System;Local");
		ud_get_filename1(NULL, banner,ext, fname,&nc, descrip, 1, UU_FALSE, paths, path_des);
		if (strlen(fname) == 0) goto done;
		user_opened = 1;
	}
	else strcpy(fname,keyfile);
/*
.....Load the key definitions
*/
	if (user_opened)
		status = uz_load_keys2(fname, 1);
	else
		status = uz_load_keys2(fname, 0);
#if UU_COMP == UU_WIN2K
	if (status == UU_SUCCESS) uz_load_accel2(fname);
#endif
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**       E_FUNCTION : uz_load_keydefs(keyfile,keytab,fkeytab,bkeytab)
**              This function loads the User defined key definition files.
**       PARAMETERS     
**               INPUT  :
**                      keyfile = Environmental variable of keyfile to load.
**                      keytab  = Structure to hold normal key definitions.
**                      fkeytab = Structure to hold function key definitions.
**                      bkeytab = Structure to hold button and dials definitions.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uz_load_keydefs(keyfile,keytab,fkeytab,bkeytab)
char *keyfile;
UZ_keytable keytab[],fkeytab[],bkeytab[];
{
	char serror[80],buf[80];
	int status,stat,opend,numint,i,istat,nc;
	int ktyp,ksub,dtyp,dsub;
	short flag;
	UX_pathname fullname;
	FILE *fptr;
	char name[30], parms[500];
/*
.....Assume success
*/
	status = UU_SUCCESS;
	opend = 0;
/*
.....Blank out current Key definitions
*/
	for (i=0;i<NKEYSYMS*3;i++)
	{
		keytab[i].type = NOKEY;
		keytab[i].params = UU_NULL;
	}
	for (i=0;i<NFKEYSYMS*3;i++)
	{
		fkeytab[i].type = NOKEY;
		fkeytab[i].params = UU_NULL;
	}
/*
.....set default function for button and dail
*/
/*
.....BUTTON_1 NCL_PAN_TOGGLE
*/
	bkeytab[0].type = NCLKEY;
	bkeytab[0].sub = 144;
	bkeytab[0].flag = 0x106;
	bkeytab[0].params = UU_NULL;
	strcpy(bkeytab[0].name, "NCL_PAN_TOGGLE");                               
/*
.....BUTTON_2 NCL_ROTATE_TOOGLE
*/
	bkeytab[1].type = NCLKEY;
	bkeytab[1].sub = 145;
	bkeytab[1].flag = 0x106;
	bkeytab[1].params = UU_NULL;
	strcpy(bkeytab[1].name, "NCL_ROTATE_TOGGLE");                               
/*
.....BUTTON_3 NCL_DOMINATE_TOGGLE
*/
	bkeytab[2].type = NCLKEY;
	bkeytab[2].sub = 146;
	bkeytab[2].flag = 0x106;
	bkeytab[2].params = UU_NULL;
	strcpy(bkeytab[2].name, "NCL_DOMINATE_TOGGLE");                               
/*
.....BUTTON_4 NO default
*/

	bkeytab[3].type = NOKEY;
	bkeytab[3].params = UU_NULL;
/*
.....BUTTON_5 NCL_GAIN_DOWN
*/
	bkeytab[4].type = NCLKEY;
	bkeytab[4].sub = 147;
	bkeytab[4].flag = 0x106;
	bkeytab[4].params = UU_NULL;
	strcpy(bkeytab[4].name, "NCL_GAIN_DOWN");                               
/*
.....BUTTON_6 NCL_GAIN_UP
*/
	bkeytab[5].type = NCLKEY;
	bkeytab[5].sub = 148;
	bkeytab[5].flag = 0x106;
	bkeytab[5].params = UU_NULL;
	strcpy(bkeytab[5].name, "NCL_GAIN_UP");                               
/*
.....BUTTON_7 NCL_GAIN_DEFAULT
*/
	bkeytab[6].type = NISKEY;
	bkeytab[6].sub = 149;
	bkeytab[6].flag = 0x106;
	bkeytab[6].params = UU_NULL;
	strcpy(bkeytab[6].name, "NCL_GAIN_DEFAULT");                               
/*
.....BUTTON_8 NO default
*/

	bkeytab[7].type = NOKEY;
	bkeytab[7].params = UU_NULL;
/*
.....BUTTON_9 NO default
*/

	bkeytab[8].type = NOKEY;
	bkeytab[8].params = UU_NULL;
/*
.....BUTTON_10 NO default
*/

	bkeytab[9].type = NOKEY;
	bkeytab[9].params = UU_NULL;
/*
.....BUTTON_11 NO default
*/
	bkeytab[10].type = NOKEY;
	bkeytab[10].params = UU_NULL;

	for (i=11;i<32;i++)
	{
		bkeytab[i].type = NOKEY;
		bkeytab[i].params = UU_NULL;
	}
/*
.....dial_1_left NCL_XPAN_LEFT
*/
	bkeytab[32].type = NCLKEY;
	bkeytab[32].sub = 52;
	bkeytab[32].flag = (short)0x8116;
	bkeytab[32].params = UU_NULL;
	strcpy(bkeytab[32].name, "NCL_XPAN_LEFT");                               
/*
.....dial_1_right NCL_XPAN_RIGHT
*/
	bkeytab[33].type = NCLKEY;
	bkeytab[33].sub = 53;
	bkeytab[33].flag = (short)0x8116;
	bkeytab[33].params = UU_NULL;
	strcpy(bkeytab[33].name, "NCL_XPAN_RIGHT");                               

/*
.....dial_2_left NCL_YPAN_LEFT
*/
	bkeytab[34].type = NCLKEY;
	bkeytab[34].sub = 54;
	bkeytab[34].flag = (short)0x8116;
	bkeytab[34].params = UU_NULL;
	strcpy(bkeytab[34].name, "NCL_YPAN_LEFT");                               
/*
.....dial_2_right NCL_YPAN_RIGHT
*/
	bkeytab[35].type = NCLKEY;
	bkeytab[35].sub = 55;
	bkeytab[35].flag = (short)0x8116;
	bkeytab[35].params = UU_NULL;
	strcpy(bkeytab[35].name, "NCL_YPAN_RIGHT");                               
/*
.....dial_3_left NCL_ZOOM_DOWN
*/
	bkeytab[36].type = NCLKEY;
	bkeytab[36].sub = 62;
	bkeytab[36].flag = (short)0x8116;
	bkeytab[36].params = UU_NULL;
	strcpy(bkeytab[36].name, "NCL_ZOOM_DOWN");                               
/*
.....dial_3_right NCL_ZOOM_UP
*/
	bkeytab[37].type = NCLKEY;
	bkeytab[37].sub = 63;
	bkeytab[37].flag = (short)0x8116;
	bkeytab[37].params = UU_NULL;
	strcpy(bkeytab[37].name, "NCL_ZOOM_UP");                               
/*
.....dial_4_left NCL_XROT_LEFT
*/
	bkeytab[38].type = NCLKEY;
	bkeytab[38].sub = 56;
	bkeytab[38].flag = (short)0x8106;
	bkeytab[38].params = UU_NULL;
	strcpy(bkeytab[38].name, "NCL_XROT_LEFT");                               
/*
.....dial_4_right NCL_XROT_RIGHT
*/
	bkeytab[39].type = NCLKEY;
	bkeytab[39].sub = 57;
	bkeytab[39].flag = (short)0x8106;
	bkeytab[39].params = UU_NULL;
	strcpy(bkeytab[39].name, "NCL_XROT_RIGHT");                               

/*
.....dial_5_left NCL_YROT_LEFT
*/
	bkeytab[40].type = NCLKEY;
	bkeytab[40].sub = 58;
	bkeytab[40].flag = (short)0x8106;
	bkeytab[40].params = UU_NULL;
	strcpy(bkeytab[40].name, "NCL_YROT_LEFT");                               
/*
.....dial_5_right NCL_YROT_RIGHT
*/
	bkeytab[41].type = NCLKEY;
	bkeytab[41].sub = 59;
	bkeytab[41].flag = (short)0x8106;
	bkeytab[41].params = UU_NULL;
	strcpy(bkeytab[41].name, "NCL_YROT_RIGHT");                               
/*
.....dial_6_left NCL_ZROT_LEFT
*/
	bkeytab[42].type = NCLKEY;
	bkeytab[42].sub = 60;
	bkeytab[42].flag = (short)0x8106;
	bkeytab[42].params = UU_NULL;
	strcpy(bkeytab[42].name, "NCL_ZROT_LEFT");                               
/*
.....dial_6_right NCL_ZROT_RIGHT
*/
	bkeytab[43].type = NCLKEY;
	bkeytab[43].sub = 61;
	bkeytab[43].flag = (short)0x8106;
	bkeytab[43].params = UU_NULL;
	strcpy(bkeytab[43].name, "NCL_ZROT_RIGHT");                               

#if UU_COMP != UU_WIN2K
	UZ_form_hotkey[0].params = UU_NULL;
	UZ_form_hotkey[1].params = UU_NULL;
	UZ_form_hotkey[2].params = UU_NULL;
#endif
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
		if (stat == UX_EOF) goto done;
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			sprintf (serror,"Error reading from %s E%d.",fullname,stat);
			ud_wrerr (serror);
			goto failed;
		}
/*
.....Check for record type
*/
/*
.....added parm for name
.....Yurong 4/3/00
*/
/*
		istat = uz_parse_keydef(buf,numint,&ktyp,&ksub,&dtyp,&dsub,&flag);
*/
/*
.....added parameter string
.....Yurong 11/27/00
*/
		istat = uz_parse_keydef(buf,numint,&ktyp,&ksub,&dtyp,&dsub,name, parms, &flag);
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
........Normal keyboard key
*/
			if (ktyp == 1)
			{
				keytab[ksub].type = dtyp;
				keytab[ksub].sub = dsub;
				keytab[ksub].flag = flag;
				strcpy(keytab[ksub].name, name);   
				nc = strlen(parms);
				keytab[ksub].params = uu_malloc((nc+1)*sizeof(char));
				strcpy(keytab[ksub].params, parms);
			}
/*
........Function key
*/
			else if (ktyp == 2)
			{
				fkeytab[ksub].type = dtyp;
			
				fkeytab[ksub].sub = dsub;
				fkeytab[ksub].flag = flag;
				strcpy(fkeytab[ksub].name, name);                               
				nc = strlen(parms);
				fkeytab[ksub].params = uu_malloc((nc+1)*sizeof(char));
				strcpy(fkeytab[ksub].params, parms);
/*
......save this key if it is hot keys
*/
#if UU_COMP != UU_WIN2K
				if (strcmp (name, "CAM_SCALARS")==0)
				{
					UZ_form_hotkey[0].type = dtyp;
					UZ_form_hotkey[0].sub = dsub;
					UZ_form_hotkey[0].flag = flag;
					strcpy(UZ_form_hotkey[0].name, name);
					nc = strlen(parms);
					UZ_form_hotkey[0].params = uu_malloc((nc+1)*sizeof(char));
					strcpy(UZ_form_hotkey[0].params, parms);
					UZ_form_hotkey_inx[0] = ksub;
				}
				if (strcmp (name, "NCL_CALCULATOR")==0)
				{
					UZ_form_hotkey[1].type = dtyp;
					UZ_form_hotkey[1].sub = dsub;
					UZ_form_hotkey[1].flag = flag;
					strcpy(UZ_form_hotkey[1].name, name);
					nc = strlen(parms);
					UZ_form_hotkey[1].params = uu_malloc((nc+1)*sizeof(char));
					strcpy(UZ_form_hotkey[1].params, parms);
					UZ_form_hotkey_inx[1] = ksub;
				}
				if (strcmp (name, "CAM_DATA_FORM")==0)
				{
					UZ_form_hotkey[2].type = dtyp;
					UZ_form_hotkey[2].sub = dsub;
					UZ_form_hotkey[2].flag = flag;
					strcpy(UZ_form_hotkey[2].name, name);
					nc = strlen(parms);
					UZ_form_hotkey[2].params = uu_malloc((nc+1)*sizeof(char));
					strcpy(UZ_form_hotkey[2].params, parms);
					UZ_form_hotkey_inx[2] = ksub;
				}
#endif
			}
/*
........Button and Dials
*/
			else if (ktyp == 3)
			{
				bkeytab[ksub].type = dtyp;
				bkeytab[ksub].sub = dsub;
				bkeytab[ksub].flag = flag;
				nc = strlen(parms);
				bkeytab[ksub].params = uu_malloc((nc+1)*sizeof(char));
				strcpy(bkeytab[ksub].params, parms);
				strcpy(bkeytab[ksub].name, name);                               
			}
/*
......added menu name
*/
			else if (ktyp == 4)
			{
				keytab[ksub].type = dtyp;
				strcpy(keytab[ksub].name, name);                                
/*
.....always allowed
*/
				keytab[ksub].flag = (short)
					(NISFL|CAMFL|CADFL|SGNFL|DRWFL|SELFL|EXECFL);

				fkeytab[ksub].type = dtyp;
				strcpy(fkeytab[ksub].name, name);                               
/*
.....always allowed
*/
				fkeytab[ksub].flag = (short)
					(NISFL|CAMFL|CADFL|SGNFL|DRWFL|SELFL|EXECFL);

				bkeytab[ksub].type = dtyp;
				strcpy(bkeytab[ksub].name, name);                               
/*
.....always allowed
*/
				bkeytab[ksub].flag = (short)
					(NISFL|CAMFL|CADFL|SGNFL|DRWFL|SELFL|EXECFL);
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
	return(status);
}
/*********************************************************************
**       I_FUNCTION : uz_which_keydef(buf,dtyp,dsub,flag);
**              This function parses a key/menu definition.
**       PARAMETERS     
**               INPUT  :
**                      buf    = Key definition record string to be parsed.
**               OUTPUT :
**                      dtyp   = Application type of key definition (NIS, CAM, etc).
**                      dsub   = Key definition requested (KEY_PIKLOC, etc.).
**                      flag   = Valid application flag for key definition.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int uz_which_keydef(buf,dtyp,dsub,flag)
char *buf;
int *dtyp,*dsub;
short *flag;
{
	int i,status,inum;
	char tmp[80],tmp1[80];
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	strcpy(tmp,buf);
	ul_to_upper(tmp);
/*
........Find key definition
*/
	inum = 0;
	i = 0;
	while (UU_TRUE)
	{
		if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
		strcpy (tmp1,UZ_keyfuncs[i].name);
		if (strcmp(tmp,tmp1) == 0)
		{
			inum = i + 1;
			break;
		}
		i++;
	}
/*
........Set key definition
*/
	if (i < inum)
	{
		*dtyp = UZ_keyfuncs[i].type;
		*dsub = UZ_keyfuncs[i].sub;
		*flag = UZ_keyfuncs[i].app;
	}
/*
........Search for Alpha key
*/
	else
	{
		for (i=0;i<NALPHAKEYS;i++)
		{
			if (strcmp(tmp,alpha[i]) == 0) break;
		}
		if (i >= NALPHAKEYS) goto failed;
		*dtyp = ALPHAKEY;
		*dsub = i;
		*flag = NISFL | CAMFL | CADFL | SGNFL;
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
**       I_FUNCTION : uz_user_key(num,index,xflag)
**              This function accepts a normal keyboard key as input and
**              optionally executes it user defined function.
**       PARAMETERS     
**               INPUT  :
**                      num    = Input function key.
**                      xflag  = 1 = Call this function now.
**               OUTPUT :
**                      index  = Returns the DAS function call, if this key was
**                               programmed using a DAS function.
**       RETURNS: 0 = Normal function call.
**                1 = DAS function call (index contains DAS function).
**                2 = This key is not associated with a function.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uz_user_key(num,index,xflag)
int num,xflag;
char **index;
{
	int ipt,irtn;
	UZ_keytable ktab;
/*
.....Base key definition array to use
.....on current application
*/
	ipt = num - 1;
	if (UU_application == UU_NCLNIS || UU_application == UU_NCLSIGNON)
		ktab = UZ_niskeytab[ipt];
	else if (UU_application == UU_NCLCAM) ktab = UZ_camkeytab[ipt];
	else if (UU_application == UU_NCLCADD) ktab = UZ_cadkeytab[ipt];
	else if (UU_application == UU_NCLIPV) ktab = UZ_ipvkeytab[ipt];
/*
.....Get the key definition
*/
	irtn = uz_user_keydef(ktab,index,xflag);
	return(irtn);
}

/*********************************************************************
**       I_FUNCTION : uz_user_fkey(num,index,xflag)
**              This function accepts a function key as input and optionally
**              executes it user defined function.
**       PARAMETERS     
**               INPUT  :
**                      num    = Input function key.
**                      xflag  = 1 = Call this function now.
**               OUTPUT :
**                      index  = Returns the DAS function call, if this key was
**                               programmed using a DAS function.
**       RETURNS: 0 = Normal function call.
**                1 = DAS function call (index contains DAS function).
**                2 = This key is not associated with a function.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uz_user_fkey(num,index,xflag)
int num,xflag;
char **index;
{
	int ipt,irtn;
	UZ_keytable ktab;
/*
.....DAS key entered
*/
	if (num >= 10000)
	{
		ipt = num - 10000;
		irtn = uz_user_dkey(ipt,index,xflag);
	}
/*
.....Base key definition array to use
.....on current application
*/
	else
	{
		ipt = num - 1;
		if (UU_application == UU_NCLNIS || UU_application == UU_NCLSIGNON)
			ktab = UZ_nisfkeytab[ipt];
		else if (UU_application == UU_NCLCAM) ktab = UZ_camfkeytab[ipt];
		else if (UU_application == UU_NCLCADD) ktab = UZ_cadfkeytab[ipt];
		else if (UU_application == UU_NCLIPV) ktab = UZ_ipvfkeytab[ipt];
/*
.....Get the key definition
*/
		irtn = uz_user_keydef(ktab,index,xflag);
		if (irtn == 2) irtn = 0;
	}
	return(irtn);
}

/*********************************************************************
**       I_FUNCTION : uz_user_button(num,index,xflag)
**              This function accepts a button or dial as input and optionally
**              executes its user defined function.
**       PARAMETERS     
**               INPUT  :
**                      num    = Input button/dial.
**                      xflag  = 1 = Call this function now.
**               OUTPUT :
**                      index  = Returns the DAS function call, if this key was
**                               programmed using a DAS function.
**       RETURNS: 0 = Normal function call.
**                1 = DAS function call (index contains DAS function).
**                2 = This key is not associated with a function.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uz_user_button(num,index,xflag)
int num,xflag;
char **index;
{
	int ipt,irtn;
	UZ_keytable ktab;
/*
.....Base key definition array to use
.....on current application
*/
	ipt = num - 1;
	if (UU_application == UU_NCLNIS || UU_application == UU_NCLSIGNON)
		ktab = UZ_nisbkeytab[ipt];
	else if (UU_application == UU_NCLCAM) ktab = UZ_cambkeytab[ipt];
	else if (UU_application == UU_NCLCADD) ktab = UZ_cadbkeytab[ipt];
	else if (UU_application == UU_NCLIPV) ktab = UZ_ipvbkeytab[ipt];
/*
.....Get the key definition
*/
	irtn = uz_user_keydef(ktab,index,xflag);

	if (irtn == 2) irtn = 0;
	return(irtn);
}

/*********************************************************************
**       I_FUNCTION : uz_user_smouse(event, data, indx, xflag);
**              This function accepts SpaceMouse event as input and save the
**                              SpaceMouse data into global variables, then optionally
**              executes its user defined function.
**       PARAMETERS     
**               INPUT  :
**                      enum    = Input Spacemouse event
**                                              data    =       SpaceMouse data
**                      xflag  = 1 = Call this function now.
**               OUTPUT :
**                      index  = Returns the DAS function call, if this key was
**                               programmed using a DAS function.
**       RETURNS: 0 = Normal function call.
**                1 = DAS function call (index contains DAS function).
**                2 = This key is not associated with a function.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uz_user_smouse(event, data, index, xflag)
int event,xflag;
char **index;
int data[6];
{
	int i, irt,sav_ipv;

/*
......before doing space mouse function
......save the active view information first since the step view may change it
*/
	UM_pkwin_type sarea;
	sav_ipv = UZ_nclipv_view;	
/*
.....first, save SpaceMouse data into global variables
*/
	if ( (data[0]!=UV_SM_data[0]) || (data[1]!=UV_SM_data[1]) || (data[2]!=UV_SM_data[2]) ||
		(data[3]!=UV_SM_data[3]) || (data[4]!=UV_SM_data[4]) || (data[5]!=UV_SM_data[5]))
	{
		for (i=0; i<6; i++)
			UV_SM_data[i] = data[i];
	}

	UV_Cur_Dyn = 2;
	if (event<=32)
	{
		irt = uz_user_button(event,index,xflag);
		UV_Cur_Dyn = 0;
		return irt;
	}
/*
.....Motion
*/
	if ((event<33) || (event > 45))
	{
		UV_Cur_Dyn = 0;
		return -1;
	}

	Space_mouse_draw = 1;
	if (event==45)
	{
		for (i=33; i<33+11;i=i+2)
		{
			if ((i==33)&&(UV_SM_data[0]<0))
			{
				UV_actsm_dial = 1;
				uz_user_button(i,index,xflag);
				continue;
			}
			else if  ((i==33)&&(UV_SM_data[0]>0))
			{
				UV_actsm_dial = 1;
				uz_user_button(i+1,index,xflag);
				continue;
			}
			else if  ((i==35)&&(UV_SM_data[2]<0))
			{
				UV_actsm_dial = 3;
				uz_user_button(i,index,xflag);
				continue;
			}
			else if  ((i==35)&&(UV_SM_data[2]>0))
			{
				UV_actsm_dial = 3;
				uz_user_button(i+1,index,xflag);
				continue;
			}
			else if  ((i==37)&&(UV_SM_data[1]<0))
			{
				UV_actsm_dial = 2;
				uz_user_button(i,index,xflag);
				continue;
			}
			else if  ((i==37)&&(UV_SM_data[1]>0))
			{
				UV_actsm_dial = 2;
				uz_user_button(i+1,index,xflag);
				continue;
			}
			else if  ((i==39)&&(UV_SM_data[4]<0))
			{
				UV_actsm_dial = 5;
				uz_user_button(i,index,xflag);
				continue;
			}
			else if  ((i==39)&&(UV_SM_data[4]>0))
			{
				UV_actsm_dial = 5;
				uz_user_button(i+1,index,xflag);
				continue;
			}
			else if  ((i==41)&&(UV_SM_data[3]<0))
			{
				UV_actsm_dial = 4;
				uz_user_button(i,index,xflag);
				continue;
			}
			else if  ((i==41)&&(UV_SM_data[3]>0))
			{
				UV_actsm_dial = 4;
				uz_user_button(i+1,index,xflag);
				continue;
			}
			else if ((i==43)&&(UV_SM_data[5]<0))
			{
				UV_actsm_dial = 6;
				uz_user_button(i,index,xflag);
				continue;
			}
			else if ((i==43)&&(UV_SM_data[5]>0))
			{
				UV_actsm_dial = 6;
				uz_user_button(i+1,index,xflag);
				continue;
			}
		}
	}
	else
		uz_user_button(event,index,xflag);
	Space_mouse_draw = 0;
	uv_dynstep_disp();
/*
.....if form is active, the main window is not drawing
.....so we need updated here
*/
	if (uw_isform_active())
		ud_updatews(UG_SUPPRESS);
	
	UV_Cur_Dyn = 0;
	UV_actsm_dial = -1;
	UZ_nclipv_view = sav_ipv;	
	return 0;
}


/*********************************************************************
**       I_FUNCTION : uz_user_dkey(num,index,xflag)
**              This function accepts a DAS device function as input and simply
**              returns it.  Used when the DAS function is tied to a Menu.
**       PARAMETERS     
**               INPUT  :
**                      num    = DAS function index.
**                      xflag  = Ignored.
**               OUTPUT :
**                      index  = Returns the DAS function call.
**       RETURNS: 1 = DAS function call (index contains DAS function).
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uz_user_dkey(num,index,xflag)
int num,xflag;
char **index;
{
	*index = UZ_daskey[num];
	return(1);
}

/*********************************************************************
**       I_FUNCTION : uz_light_buttons()
**              This function turns on the lights on the button box which are
**              valid for this application (SIGNON, NIS, CAM, or CAD).  It
**              should be called whenever the main application changes in NCL.
**       PARAMETERS     
**               INPUT  :
**                      none.
**               OUTPUT :
**                      none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void uz_light_buttons()
{
#if UU_COMP == UU_IRIS4D
	int mask,i;
	if (!UZ_buttons) goto done;
	mask = 0;
	if (UU_application == UU_NCLCAM)
	{
		for (i=0;i<32;i++)
		{
			if (UZ_cambkeytab[i].type != NOKEY &&
				((UL_cad == 1 && UZ_cambkeytab[i].flag & CADFL) ||
				(UZ_cambkeytab[i].flag & CAMFL)))
				mask = mask | (1<<i);
		}
	}
	else if (UU_application == UU_NCLCADD)
	{
		for (i=0;i<32;i++)
		{
			if (UZ_cadbkeytab[i].type != NOKEY &&
				((UL_cam == 1 && UZ_cadbkeytab[i].flag & CAMFL) ||
				(UZ_cadbkeytab[i].flag & CADFL)))
				mask = mask | (1<<i);
		}
	}
	else if (UU_application == UU_NCLNIS)
	{
		for (i=0;i<32;i++)
		{
			if (UZ_nisbkeytab[i].type != NOKEY &&
				(UZ_nisbkeytab[i].flag & NISFL))
				mask = mask | (1<<i);
		}
	}
	else if (UU_application == UU_NCLSIGNON)
	{
		for (i=0;i<32;i++)
		{
			if (UZ_nisbkeytab[i].type != NOKEY &&
				(UZ_nisbkeytab[i].flag & SGNFL))
				mask = mask | (1<<i);
		}
	}
	else if (UU_application == UU_NCLIPV)
	{
		for (i=0;i<32;i++)
		{
			if (UZ_ipvbkeytab[i].type != NOKEY &&
				(UZ_ipvbkeytab[i].flag & IPVFL))
				mask = mask | (1<<i);
		}
	}
/*      setdblights(mask);*/
done:;
#endif
}

/*********************************************************************
**       I_FUNCTION : uz_unlight_buttons()
**              This function turns off all lights on the button box.  It
**              should be called upon exiting NCL.
**       PARAMETERS     
**               INPUT  :
**                      none.
**               OUTPUT :
**                      none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void uz_unlight_buttons()
{
/*      int mask;*/
#if UU_COMP == UU_IRIS4D
	if (UZ_buttons)
	{
/*              mask = 0;*/
/*              setdblights(mask);*/
	}
#endif
}

/*********************************************************************
**       E_FUNCTION : uz_load_keys2(keyfile, open_flag)
**              This function loads the User defined key definition files.
**                              
**       PARAMETERS     
**               INPUT  :
**                      keyfile = keyfile to load.
**						open_flag: 1: save into recent files, 0: not save
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uz_load_keys2(keyfile, open_flag)
char *keyfile;
int open_flag;
{
	char serror[80],buf[80];
	int status,stat,opend,numint,istat;
	int ktyp,ksub,dtyp,dsub,nc;
	short flag;
	UX_pathname fullname;
	FILE *fptr;
	char name[30], parms[40];
	static UX_pathname Skeyfile={""};
/*
.....Assume success
*/
	status = UU_SUCCESS;
	opend = 0;
/*
.....Open the key definition file
*/
	stat = uz_open_keydef(keyfile,fullname,&fptr,1);
	if (stat != UU_SUCCESS) goto failed;
	opend = 1;
	if (strcmp(fullname,Skeyfile) == 0) goto done;
	strcpy(Skeyfile,fullname);
/*
.....Read a record
*/
	do
	{
		stat = ul_fread(fptr,buf,sizeof(buf),&numint);
		if (stat == UX_EOF) goto done;
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			sprintf (serror,"Error reading from %s E%d.",fullname,stat);
			ud_wrerr (serror);
			goto failed;
		}
		istat = uz_parse_keydef(buf,numint,&ktyp,&ksub,&dtyp,&dsub,name,parms, &flag);
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
........Normal keyboard key
*/
			if (ktyp == 1)
			{
				UZ_camkeytab[ksub].type = dtyp;
				UZ_camkeytab[ksub].sub = dsub;
				UZ_camkeytab[ksub].flag = flag;
				strcpy(UZ_camkeytab[ksub].name, name);                                
				nc = strlen(parms);
				UZ_camkeytab[ksub].params = uu_malloc((nc+1)*sizeof(char));
				strcpy(UZ_camkeytab[ksub].params, parms);
			}
/*
........Function key
*/
			else if (ktyp == 2)
			{
				UZ_camfkeytab[ksub].type = dtyp;
				UZ_camfkeytab[ksub].sub = dsub;
				UZ_camfkeytab[ksub].flag = flag;
				strcpy(UZ_camfkeytab[ksub].name, name);                               
				nc = strlen(parms);
				UZ_camfkeytab[ksub].params = uu_malloc((nc+1)*sizeof(char));
				strcpy(UZ_camfkeytab[ksub].params, parms);
			}
/*
........Button and Dials
*/
			else if (ktyp == 3)
			{
				UZ_cambkeytab[ksub].type = dtyp;
				UZ_cambkeytab[ksub].sub = dsub;
				UZ_cambkeytab[ksub].flag = flag;
				nc = strlen(parms);
				UZ_cambkeytab[ksub].params = uu_malloc((nc+1)*sizeof(char));
				strcpy(UZ_cambkeytab[ksub].params, parms);
				strcpy(UZ_cambkeytab[ksub].name, name);                               
			}
/*
......added menu name
*/
			else if (ktyp == 4)
			{
				UZ_camkeytab[ksub].type = dtyp;
				strcpy(UZ_camkeytab[ksub].name, name);                                
				UZ_camkeytab[ksub].params = UU_NULL;
/*
.....always allowed
*/
				UZ_camkeytab[ksub].flag = (short) 
					(NISFL|CAMFL|CADFL|SGNFL|DRWFL|SELFL|EXECFL);

				UZ_camfkeytab[ksub].type = dtyp;
				strcpy(UZ_camfkeytab[ksub].name, name);                               
/*
.....always allowed
*/
				UZ_camfkeytab[ksub].flag = (short)
					(NISFL|CAMFL|CADFL|SGNFL|DRWFL|SELFL|EXECFL);
				UZ_camfkeytab[ksub].params = UU_NULL;

				UZ_cambkeytab[ksub].type = dtyp;
				strcpy(UZ_cambkeytab[ksub].name, name);                               
/*
.....always allowed
*/
				UZ_cambkeytab[ksub].flag = (short)
					(NISFL|CAMFL|CADFL|SGNFL|DRWFL|SELFL|EXECFL);
				UZ_cambkeytab[ksub].params = UU_NULL;
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
	if (opend == 1) 
	{
		ux_fclose0(fptr);
		if (open_flag)
			nclc_save_recent_file(fullname, 0);
	}
	return(status);
}

/*********************************************************************
**       E_FUNCTION : uz_getkey_funnam(irtn, k, name)
**              This function get key definition name of a key.
**                              
**       PARAMETERS     
**               INPUT  :
**                      irtn = 1: ctrl normal key
**								2: funtion key
**						k: key number
**               OUTPUT :  name: funtion name
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void uz_getkey_funnam(irtn, k, name)
int irtn, k;
char *name;
{
	int ipt;
	UZ_keytable ktab;

	ktab.name[0] = '\0';
	ipt = k - 1;
	if (irtn==1)
	{
/*
.....Base key definition array to use
.....on current application
*/
		if (UU_application == UU_NCLNIS || UU_application == UU_NCLSIGNON)
			ktab = UZ_niskeytab[ipt];
		else if (UU_application == UU_NCLCAM) ktab = UZ_camkeytab[ipt];
		else if (UU_application == UU_NCLCADD) ktab = UZ_cadkeytab[ipt];
		else if (UU_application == UU_NCLIPV) ktab = UZ_ipvkeytab[ipt];
	}
	else if (irtn==2)
	{
		if (UU_application == UU_NCLNIS || UU_application == UU_NCLSIGNON)
			ktab = UZ_nisfkeytab[ipt];
		else if (UU_application == UU_NCLCAM) ktab = UZ_camfkeytab[ipt];
		else if (UU_application == UU_NCLCADD) ktab = UZ_cadfkeytab[ipt];
		else if (UU_application == UU_NCLIPV) ktab = UZ_ipvfkeytab[ipt];
	}
	name[0] = '\0';
	if (ktab.name[0]!='\0')
		strcpy(name, ktab.name);
}
void uz_user_fkey_name(num,fname)
int num;
char *fname;
{
	int ipt;
	UZ_keytable ktab;
/*
.....Base key definition array to use
.....on current application
*/
		ipt = num - 1;
		if (UU_application == UU_NCLNIS || UU_application == UU_NCLSIGNON)
			ktab = UZ_nisfkeytab[ipt];
		else if (UU_application == UU_NCLCAM) ktab = UZ_camfkeytab[ipt];
		else if (UU_application == UU_NCLCADD) ktab = UZ_cadfkeytab[ipt];
		else if (UU_application == UU_NCLIPV) ktab = UZ_ipvfkeytab[ipt];
/*
.....Get the key definition
*/
		if (ktab.name!=NULL)
			strcpy(fname, ktab.name);
}
void uz_clear_parmspc()
{
	int i,j,inc;
	for (i=0;i<NKEYSYMS*3;i++)
	{
		if ((MSLite)||(LW_nclipv==LW_STANDALONE))
		{
			if (UZ_ipvkeytab[i].params!=UU_NULL)
				uu_free(UZ_ipvkeytab[i].params);
		}
		else
		{
			if (UU_application == UU_NCLNIS || UU_application == UU_NCLSIGNON)
			{
				if (UZ_niskeytab[i].params!=UU_NULL)
					uu_free(UZ_niskeytab[i].params);
			}
			else if (UU_application == UU_NCLCAM) 
			{
				if (UZ_camkeytab[i].params!=UU_NULL)
					uu_free(UZ_camkeytab[i].params);
			}
			else if (UU_application == UU_NCLCADD) 
			{
				if (UZ_cadkeytab[i].params!=UU_NULL)
					uu_free(UZ_cadkeytab[i].params);
			}
			else if (UU_application == UU_NCLIPV)
			{
				if (UZ_ipvkeytab[i].params!=UU_NULL)
					uu_free(UZ_ipvkeytab[i].params);
			}
		}
	}
	for (i=0;i<NFKEYSYMS*3;i++)
	{
		if ((MSLite)||(LW_nclipv==LW_STANDALONE))
		{
			if (UZ_ipvkeytab[i].params!=UU_NULL)
				uu_free(UZ_ipvfkeytab[i].params);
		}
		else
		{
			if (UU_application == UU_NCLNIS || UU_application == UU_NCLSIGNON)
			{
				if (UZ_niskeytab[i].params!=UU_NULL)
					uu_free(UZ_niskeytab[i].params);
			}
			else if (UU_application == UU_NCLCAM) 
			{
				if (UZ_camfkeytab[i].params!=UU_NULL)
					uu_free(UZ_camfkeytab[i].params);
			}
			else if (UU_application == UU_NCLCADD) 
			{
				if (UZ_cadfkeytab[i].params!=UU_NULL)
					uu_free(UZ_cadfkeytab[i].params);
			}
			else if (UU_application == UU_NCLIPV)
			{
				if (UZ_ipvfkeytab[i].params!=UU_NULL)
					uu_free(UZ_ipvfkeytab[i].params);
			}
		}
	}
	for (inc=0; inc<UDM_menu_count; inc++)
	{
		if (UDM_menu[inc].rows < 1 || UDM_menu[inc].cols < 1) continue;
		if (UDM_menu[inc].menus == NULL) continue;
		for (i=0; i<UDM_menu[inc].num; i++)
		{
			if (UDM_menu[inc].menus[i].params!=UU_NULL)
			{
				uu_free(UDM_menu[inc].menus[i].params);
				UDM_menu[inc].menus[i].params = UU_NULL;
			}
			for (j=0; j<UDM_menu[inc].menus[i].toggle_num; j++)
			{
				if (UDM_menu[inc].menus[i].toggle[j].params != UU_NULL)
				{
					uu_free(UDM_menu[inc].menus[i].toggle[j].params);
					UDM_menu[inc].menus[i].toggle[j].params = UU_NULL;
				}
			}
			if (UDM_menu[inc].menus[i].toggle!=UU_NULL)
			{
				free(UDM_menu[inc].menus[i].toggle);
				UDM_menu[inc].menus[i].toggle = UU_NULL;
			}
		}
		uu_free(UDM_menu[inc].menus);
	}
#if UU_COMP != UU_WIN2K
	if (UZ_form_hotkey[0].params!=UU_NULL)
	{
		uu_free (UZ_form_hotkey[0].params);
		UZ_form_hotkey[0].params = UU_NULL;
	}
	if (UZ_form_hotkey[1].params!=UU_NULL)
	{
		uu_free (UZ_form_hotkey[1].params);
		UZ_form_hotkey[1].params = UU_NULL;
	}
	if (UZ_form_hotkey[2].params!=UU_NULL)
	{
		uu_free (UZ_form_hotkey[2].params);
		UZ_form_hotkey[2].params = UU_NULL;
	}
#else
	for (i=0;i<UZ_accel_count;i++)
	{
		if (UZ_accelcalls[i].params!=NULL)
			uu_free (UZ_accelcalls[i].params);
	}
/*
.......UZ_itemcalls is freed already because it just
.......point to UDM_menu[inc].menus[i].params
*/
/*
.....changed made here because we can't just assign UZ_itemcalls.params
.....because when we reload menu, we free UDM_menu[inc].menus[i].params but
.....no way to free UZ_itemcalls[i].params at that place. So we still malloc
.....UZ_itemcalls[i].params and free it here
*/
	for (i=0;i<UZ_item_count;i++)
	{
		if (UZ_itemcalls[i].params!=NULL)
			uu_free (UZ_itemcalls[i].params);
	}
#endif
}
/*********************************************************************
**       E_FUNCTION : uz_rdbut_func(isub,ctyp,cmsg)
**              This function read a button function definition line.
**                              
**       PARAMETERS     
**               INPUT  :
**						isub: which button function
**                      ctyp = button function type string
**						cmsg: button function string
**               OUTPUT :  none
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
uz_rdbut_func(isub,ctyp,cmsg)
int isub;
char *cmsg, *ctyp;
{
	int i,len,status,inum;
	char buf[UX_MAX_PATH_LEN*2];
	int maxsub=5;
	static char csub[10][20] = {"LEFT","MIDDLE","RIGHT","WHEEL_DOWN","WHEEL_UP"};
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (buf, "Not a valid Button modal.  /%s/ %s\n",ctyp,cmsg);
		ud_printmsg (buf);
		return -1;
	}
	len = strlen(cmsg);
	while ((cmsg[len-1]=='\t')||(cmsg[len-1]=='\r')||(cmsg[len-1]==' '))
		len--;
	cmsg[len] = '\0';
	strcpy(UZ_mouse_function[isub*5+i], cmsg);
	return 0;
}

/*********************************************************************
**       E_FUNCTION : uz_load_mousedef(file, init)
**              load button function definition file.
**                              
**       PARAMETERS     
**               INPUT  : file: mouse file to be load if init=0
**							init: 1: initial mouse file
**									0: not init mouse file, use 'file' to load.
**               OUTPUT :  none
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int uz_load_mousedef(file, init)
char *file;
int init;
{
	char serror[UX_MAX_PATH_LEN*2], buf[UX_MAX_PATH_LEN*2],ctyp[UX_MAX_PATH_LEN*2],cmsg[UX_MAX_PATH_LEN*2];
	int stat,numint,ityp,i,isub,istat,kinc,status;
	int ktyp,ksub,dtyp,dsub,opend, len;
	short flag;
	UX_pathname fullname, dir, filename;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	FILE *fptr;
	char ext[UX_SUFFIX_LEN];
	char banner[30], descrip[UX_MAX_PATH_LEN];
	char name[30], parms[500];
	int maxsub=4;
	static char csub[4][12]={"CHOICE","LOCATE","PICK","TEXT"};
/*
.....Assume success
*/
	status = UU_SUCCESS;
	opend = 0;
/*
.....Open mouse definition file
*/
	if (init==0)
	{
		if ((file!=NULL)&&(file[0]!='\0'))
		{
			strcpy(filename, file);
		}
		else
		{
			strcpy(banner,"Load Mouse Definition File");
			filename[0] = '\0'; len = 0;
			strcpy(descrip, "Mouse Definition File (*.mo)");
			strcpy(ext,"*.mo");
			strcpy(paths, NCL_init_fstr);
			strcat(paths, ";");
			strcat(paths, "%UU_USER_SETTINGS\\init");
			strcpy(path_des, "System;Local");
			ud_get_filename1(NULL, banner,ext, filename,&len, descrip, 1, UU_FALSE, paths, path_des);
			if (strlen(filename) == 0) goto failed;
		}
		stat = ux_fopen0(filename, "r", &fptr);
	}
	else
	{
		stat = uz_open_keydef("UZ_MOUSE_BUTDEFS",fullname,&fptr,1);
	}
	if (stat != UU_SUCCESS) goto failed;
	opend = 1;
	for (i=0;i<20;i++)
	{
		strcpy(UZ_mouse_function[i], "KEY_NOOP");                               
	}
/*
.....set default function for mouse
*/
	strcpy(UZ_mouse_function[6], "KEY_DONE");                               
	strcpy(UZ_mouse_function[11], "KEY_DONE");                               
	strcpy(UZ_mouse_function[16], "KEY_DONE"); 

	strcpy(UZ_mouse_function[7], "KEY_REJECT");                               
	strcpy(UZ_mouse_function[12], "KEY_REJECT");                               
	strcpy(UZ_mouse_function[17], "KEY_REJECT");                               
/*
.....Read a record
*/
	do
	{
		stat = ul_fread(fptr,buf,sizeof(buf),&numint);
		if (stat == UX_EOF) goto done;
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			sprintf (serror,"Error reading from %s E%d.",fullname,stat);
			ud_wrerr (serror);
			goto failed;
		}
check:;
		istat = ul_modal_check (buf,&ityp, ctyp, cmsg);
		if (istat != UU_SUCCESS)
		{
			sprintf (serror, "button define file syntax error. %s\n",buf);
			ud_printmsg (serror);
		}
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
					sprintf (serror, "Not a valid button define file parameter. %s\n",buf);
					ud_printmsg (serror);
					break;
				}
				isub = i;
				break;
			case 2:
				uz_rdbut_func(isub,ctyp,cmsg);
				break;
		}
	}
	while (stat == UU_SUCCESS);
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
**       E_FUNCTION : uz_key_pick_loc
**              Enable/Disable standard mouse key PICK/LOC function.
**                              
**       PARAMETERS     
**               INPUT  : none
**               OUTPUT :  none
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void uz_key_pick_loc()
{
	char buf[40];
	UZ_key_pickloc = 1;
	if (UZ_key_pickloc)
	{
		sprintf(buf,"KEY Pick Locate mode ENABLED.");
	}
	ud_prmerr(buf);
}
int uz_ifmouse_key()
{
	return UZ_key_pickloc;
}
uz_save_mouse()
{
	int i;
	for (i=0;i<20;i++)
	{
		strcpy(S_svmouse_func[i], UZ_mouse_function[i]);                               
	}
}
uz_reset_mouse()
{
	int i;
	for (i=0;i<20;i++)
	{
		strcpy(UZ_mouse_function[i], S_svmouse_func[i]);                               
	}
}
uz_default_mouse()
{
	int i;
	for (i=0;i<20;i++)
	{
		strcpy(UZ_mouse_function[i], "KEY_NOOP");                               
	}
/*
.....set default function for mouse
*/
	strcpy(UZ_mouse_function[6], "KEY_DONE");                               
	strcpy(UZ_mouse_function[11], "KEY_DONE");                               
	strcpy(UZ_mouse_function[16], "KEY_DONE"); 

	strcpy(UZ_mouse_function[7], "KEY_REJECT");                               
	strcpy(UZ_mouse_function[12], "KEY_REJECT");                               
	strcpy(UZ_mouse_function[17], "KEY_REJECT");                               
}

/*********************************************************************
**       I_FUNCTION : uz_mouse_key(num,name,xflag)
**              This function accepts a mouse key as input and 
**				executes it user defined function during dynamic viewing.
**       PARAMETERS     
**               INPUT  :
**                      num    = Input mouse key.
**                      xflag: not used, always Call this function now.
**               OUTPUT :
**                      name  = not used.
**       RETURNS: Not used.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void uz_mouse_key(num,name,xflag)
int num,xflag;
char **name;
{
	char func_str[80], buf[256], parms[500], *index;
	UZ_keytable ktab;
	short app;
	int sub, type1, stat, irtn;
	int size[2], pos[2];

	UW_dynamic_funkey = -1;
	if (num==1)
/*
......mouse left
*/
		strcpy(func_str, UZ_mouse_function[0]);
	else if (num==2)
/*
......mouse middle
*/
		strcpy(func_str, UZ_mouse_function[1]);
	else if (num==3)
/*
......mouse right
*/
		strcpy(func_str, UZ_mouse_function[2]);
	else if (num==4)
/*
......wheel down
*/
		strcpy(func_str, UZ_mouse_function[3]);
	else if (num==5)
/*
......wheel up
*/
		strcpy(func_str, UZ_mouse_function[4]);
	if (func_str[0]=='\0')
		return;
	if (uz_which_keydef(func_str,&type1,&sub,&app) == UU_SUCCESS)
	{
/*
.....set mouse function on flag
*/
		NCL_mouse_func = 1;
		ktab.type = type1;
		ktab.sub = sub;
		ktab.flag = app;
		parms[0] = '\0';
		ktab.params = parms;
		index = buf;

		irtn = uz_user_keydef(ktab,&index,1);
	}
	else
/*
......menu, only display popup menu
*/
	{
		ud_rpwrmenu(func_str, parms, func_str);
		pos[0] = -1;
		pos[1] = -1;
		size[0] = -1;
		size[1] = -1;

		if (udm_read_menu(func_str,pos,size, 1, 1, UDM_MTYPE_POPUP) != UU_SUCCESS)
		{
			sprintf(buf,"Could not load menu: %s",func_str);
			uw_nterror(buf);
		}
	}
/*
.....reset UW_dynamic_funkey and jump of the function
*/
	ud_jump(-1, UU_FALSE);
}
