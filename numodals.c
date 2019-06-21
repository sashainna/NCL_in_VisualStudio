/*********************************************************************
**    NAME         :  numodals.c
**       CONTAINS: manipulation of NCL system modals
**       nclu_modals()
**       ncl_init_name_mod()
**       nclu_name_modals()
**       ncl_name_geom()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       numodals.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:09
*********************************************************************/
#include <ctype.h>
#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"
#include "lcom.h"
#include "lumb.h"
#include "nclcmd.h"
#include "uhep.h"
#include "nclinp.h"
#include "nclfc.h"
#include "mfort.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "mdrel.h"
#include "mlab.h"
#include "ncl.h"
#include "dmotif.h"
#include "xfsys1.h"
#include "xenv1.h"

static UU_LOGICAL  auto_name = UU_TRUE;
static UU_LOGICAL  redefine = UU_FALSE;

void ncl_handl_redef();

/*********************************************************************
**    E_FUNCTION     : nclu_modals()
**       Set NCL modals
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_modals()
{
	char tmp[80];
	int status;
	UM_int2 idx,ival;
	FILE *fptr;
	UX_pathname fname;

	static int stat, edit_mode, cmd_mod, ins_mod, line_len, com_col;
	static char traverse[] = {1,1,1,1,1,1};
	static int *ans[] = 
		{&line_len, &com_col, &edit_mode, &stat, &cmd_mod, &ins_mod};

	uu_denter(UU_MTRC,(us,"ncl_modals()"));

	/* set default answers .. */
	line_len = UL_line_len;
	com_col = UL_comment_column;
	edit_mode = NCL_edit_mode;
	cmd_mod = NCL_cmd_window_mode;
	ins_mod = NCL_com_mode;
#if UU_COMP!=UU_WIN2K
	stat = 0;
	traverse[3] = 0;
#else
	stat=UDM_run_layout.statwin_type;
	traverse[3] = 1;
#endif
	/** display form, get answers **/
	status = ud_form1("nmodals.frm", ans, ans, NULL, NULL, NULL,traverse);
	if (status == -1) return;
/*
.....Check answers
*/
	if (line_len < 10) line_len = 10;
	if (line_len > NCL_MAX_COMLINE) line_len = NCL_MAX_COMLINE;
	if (com_col != 0 && com_col <= line_len) com_col = line_len + 1;
	if (com_col < 0 || com_col > NCL_MAX_COMLINE) com_col = 0;
/*
.....Store answers
*/
#if UU_COMP==UU_WIN2K
	UDM_run_layout.statwin_type = stat;
#endif
	UL_line_len = line_len;
	UL_comment_column = com_col;
	NCL_edit_mode = edit_mode;
	NCL_cmd_window_mode = cmd_mod;
	NCL_com_mode = ins_mod;
/*
.....Open modals file
*/
	strcpy (fname, "ncl_cmdline.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) return;
/*
.....Store modals
*/
	ux_fputs0("#CMD_LINE#\n", fptr);

	sprintf(tmp,"/LENGTH/ %d\n",UL_line_len);
	ux_fputs0(tmp,fptr);

	sprintf(tmp,"/COMMENT/ %d\n",UL_comment_column);
	ux_fputs0(tmp,fptr);

	if (NCL_edit_mode)
		sprintf(tmp, "/EDIT/ *YES\n");
	else
		sprintf(tmp, "/EDIT/ *NO\n");
	ux_fputs0(tmp, fptr);

	if (NCL_cmd_window_mode)
		ux_fputs0("/WINDOW/ *YES\n", fptr);
	else
		ux_fputs0("/WINDOW/ *NO\n", fptr);

	if (NCL_com_mode==0)
		ux_fputs0("/INSERT/ *OFF\n", fptr);
	else if (NCL_com_mode==1)
		ux_fputs0("/INSERT/ *ON\n", fptr);
	else
		ux_fputs0("/INSERT/ *SAME\n", fptr);
	ux_fclose0 (fptr);
/*
.....Set the Fortran line length variables
*/
	idx = 106; ival = UL_line_len; setifl(&idx,&ival);
	idx = 387; ival = UL_comment_column; setifl(&idx,&ival);

	UL_line_len = line_len;
	UL_comment_column = com_col;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : ncl_init_name_mod()
**       Initialize geometry name modals.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_init_name_mod()
{
	NCL_auto_label = UU_TRUE; 
	auto_name = UU_TRUE;
	redefine = UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION     : nclu_name_modals()
**       Display CAM name modals form.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_name_modals()
{
#define NREL 14
	int *ans[NREL*2+2];
	char labs[NREL][21];
	int status,subs[NREL];
	int i, inc;
	UM_int2 ifl,ival;

	static int relnum[] = {NCL_POINT_REL, NCL_POINTVEC_REL, NCL_LINE_REL,
		NCL_VECTOR_REL, NCL_PLN_REL, NCL_CIRCLE_REL, NCL_CURVE_REL, NCL_SURF_REL,
		NCL_SHAPE_REL, NCL_MATRIX_REL, NCL_PATERN_REL, UA_TEXT_REL,
		UB_SYMBOL_REL, UM_SOLID_REL};

	uu_denter(UU_MTRC,(us,"ncl_name_modals()"));

	/* Set 'redefine' according to CA/ON or OFF */
	ifl = 41;
	getifl(&ifl,&ival);
	redefine = (ival? 1:0);

	/* Set AUTOLABEL */
	auto_name = NCL_auto_label;

/*
.....Get default prefixes &
.....Setup form
*/
	ans[0] = (int *)&auto_name;
	ans[1] = (int *)&redefine;
	inc = 2;
	for (i=0;i<NREL;i++)
	{
		um_get_rel_label(relnum[i],labs[i],&subs[i]);
		ans[inc] = (int *)&subs[i]; inc++;
		ans[inc] = (int *)labs[i]; inc++;
	}
/*
.....Display form
*/
	status = ud_form("namodal.frm", ans, ans);
	if (status == -1) return;
/*
.....Define global naming conventions
*/
	NCL_auto_label = auto_name;
	ncl_handl_redef();
	for (i=0;i<NREL;i++) ncl_name_geom(&subs[i],labs[i],relnum[i]);
}

/*********************************************************************
**    E_FUNCTION     : ncl_name_geom()
**       Update the name modal structure according to the options recieved
**       from the form.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_name_geom(geom_case,par,rel)
int rel;
UU_LOGICAL *geom_case;
char par[];
{
	int i, n,status;
	UU_LOGICAL no_alpha;
	UM_int2 type, idst;
	char lab[64];
	char str[64];
	UM_f77_str label;

	uu_denter(UU_MTRC,(us,"ncl_name_geom()"));

	status = UU_SUCCESS;
	n=strlen(par);
	/* if prefix option is picked for geometry check for two alpha character
	   base name and update the UM_labelmdl structure. */
	if (!(*geom_case))
	{
		NCL_auto_subscr = UU_FALSE;
		if (n <1 || n > 20) 
		{
			uu_uerror0(/* length sould be 2 characters */UA_NCL,10);
			ud_wrerr("length should not be more than 20 characters");
			goto done;
		}
		else
		{
			for (i=0; i<n; i++)
			{
				if (isalpha(par[i]) == 0 || par[i] == '\0')
				{
					uu_uerror0(/* must be alpha and no blank */UA_NCL,10);
					goto done;
				}
				else
				{
					if (islower(par[i]) != 0)
						par[i] =toupper(par[i]);
				}
			}
			um_update_rel_label(rel, par, 0);
		}
	}
	/* If subscript option is picked for geometry check for 6 character long
	   base name with at least one alphabet charater in it.  Update the
	   UM_labelmdl structure. */
	else
	{
		NCL_auto_subscr = UU_TRUE;
		if (n <1 || n > 20) 
		{
/*			uu_uerror0( length sould be 6 characters UA_NCL,9);*/
			ud_wrerr("length should not be more than 20 characters");
			*geom_case = UU_FALSE;
			status = UU_FAILURE;
			goto done;
		}
		no_alpha = UU_TRUE;
		for (i=0; i<n; i++)
		{
			if (isalpha(par[i]) != 0)
			{
				no_alpha = UU_FALSE;
				if (islower(par[i]) != 0)
					par[i] = toupper(par[i]);
			}
		}
		for (i=0; i<n; i++)
		{
			if (par[i] == '\0')
				no_alpha = UU_TRUE;
		}
		if (no_alpha)
		{
			uu_uerror0(/* must be alpha */UA_NCL,11);
			*geom_case = UU_FALSE;
			status = UU_FAILURE;
			goto done;
		}
		/* Check for valid 2 character vocabulary word. */
		ncl_get_type(rel, &idst);
		UM_init_f77_str (label, lab, 63);
		strcpy (str, par);
		for(i=n+1; i<63; i++) str[i] = ' ';
		str[63]='\0';
		sprintf(lab,"%-63s",str);
		chkvoc(UM_addr_of_f77_str(label), &idst, &type, &NCL_auto_subscr);
		if (type == 1)
		{
			uu_uerror0(/* Vocabulary word not allowed */UA_NCL,12);
			*geom_case = UU_FALSE;
			status = UU_FAILURE;
			goto done;
		}
		/* if base name for geometry has been modified update the parameter 
	   		table */
		um_update_rel_label(rel, par, 1);
	}
done:;
	return(status);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     : ncl_handl_redef()
**                Handle the redefine option
**                Insert *canon/on or *canon/off accordingly.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if the prefix is added to the label name server
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_handl_redef()
	{
	int status;
	UM_int2 ifl,ival, ivl1, ivl2;
	NCL_cmdbuf cmdbuf;

	uu_denter(UU_MTRC,(us,"ncl_handl_redef()"));

/* 
.....If REDEFINE is NO and CANON is ON, *insert a CA/OFF in the part program 
*/
	if (!redefine)
		{
		ifl = 41;
		getifl(&ifl,&ival);
		if (ival == 1)
			{
/*
.....If in CADD, set CANON OFF by directly setting the appropriate ifl flag.
.....Don't bother with building a *CA/OFF statement.
*/
			if (UU_application == UU_NCLCADD)
				{
				ifl = 41;
				ival = 0;
				setifl(&ifl, &ival);
/*
.....force redisplay of STATUS AREA after change.
*/
				uz_actredef();
				}
/*
.....If in CAM, set NCL to *INSERT by setting the appropriate ifl flag,
.....then build the CA/OFF statement and pass off to NCL.
.....Then, reset *INSERT to off by setting the ifl flag and
.....Force the re-display of the NCL status line by setting ifl(301)=0 
.....and calling statln().
*/
			else
				{
				ncl_init_cmdbuf(&cmdbuf);
				ivl1 = 2;
				ivl2 = 2;
				setins(&ivl1, &ivl2);
				status = ncl_add_token (&cmdbuf, NCL_ca, NCL_nocomma);
				status = ncl_add_token (&cmdbuf, NCL_indent_off, NCL_nocomma);
				if ((status == NCL_OKINPUT) || ( status == NCL_DONE))
					{
					ncl_add_cmdbuf (&cmdbuf);
					ncl_call (&cmdbuf);
					}
				ivl1 = 1;
				ivl2 = 0;
				setins(&ivl1, &ivl2);
/*
.....Force re-display of CAM status line
*/
				ifl = 301;
				ival = 0;
				setifl(&ifl,&ival);
				statln();
				}
			}
		}
/* 
.....If REDEFINE is YES and CANON is OFF, *INSERT a CA/ON in the part program 
*/
	else
		{
		ifl = 41;
		getifl(&ifl,&ival);
		if (ival == 0)
			{
/*
.....If in CADD, set CANON ON by directly setting the appropriate ifl flag.
.....Don't bother with building a *CA/ON statement.
*/
			if (UU_application == UU_NCLCADD)
				{
				ifl = 41;
				ival = 1;
				setifl(&ifl, &ival);
/*
.....force redisplay of STATUS AREA after change.
*/
				uz_actredef();
				}
/*
.....If in CAM, set NCL to *INSERT by setting the appropriate ifl flag,
.....then build the CA/ON statement and pass off to NCL.
.....Then, reset *INSERT to off by setting the ifl flag and
.....Force the re-display of the NCL status line by setting ifl(301)=0 
.....and calling statln().
*/
			else
				{
				ncl_init_cmdbuf(&cmdbuf);
				ivl1 = 2;
				ivl2 = 2;
				setins(&ivl1, &ivl2);
				status = ncl_add_token (&cmdbuf, NCL_ca, NCL_nocomma);
				status = ncl_add_token (&cmdbuf, NCL_on, NCL_nocomma);
				if ((status == NCL_OKINPUT) || ( status == NCL_DONE))
					{
					ncl_add_cmdbuf (&cmdbuf);
					ncl_call (&cmdbuf);
					}
				ivl1 = 1;
				ivl2 = 0;
				setins(&ivl1, &ivl2);
/*
.....Force re-display of CAM status line
*/
				ifl = 301;
				ival = 0;
				setifl(&ifl,&ival);
				statln();
				}
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : modtoc (subsc,ncltyp,geoname)
**                Interface for CAM 'defnam' command to call
**                CADD routine ncl_name_geom.
**    PARAMETERS   
**       INPUT  : 
**               subsc   - subscript flag: 0 - not subscripted,
**                         1 - subscripted name.
**               ncltyp  - geometry type (NCL fortran).
**               geoname - new default name for auto naming.
**       OUTPUT :  
**          none
**    RETURNS      : 
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void modtoc (subsc,ncltyp,geoname,ierr)
UM_int2 *subsc, *ncltyp, *ierr;
UM_f77_str_ptr geoname;
{
	int i,rel,isub;
   UM_int2 type;
   char *cstr, par[64];
/*
...process f77 string
*/
   cstr = UM_cstr_of_f77_str (geoname);
   i    = 63;
   strncpy (par,cstr,i);
   ul_strip_blanks(par,&i);
   par[i] = '\0';
/*
...convert NCL geo type to relation number
*/
   type   = *ncltyp;
   ncl_ncltyp_to_relnum(type,&rel);
/*
...set subscript flag & update default name
*/   
   isub = *subsc;
   i = ncl_name_geom (&isub,par,rel);
	*ierr = 0;
	if (i == UU_FAILURE) *ierr = 1;
}
