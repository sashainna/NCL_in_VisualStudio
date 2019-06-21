/*********************************************************************
**    NAME         : nedasif1.c
**       CONTAINS: routines to interface to DAS 
**			ncl_cctostr(option, msccc, str)
**			ncl_vctostr(mcsvc, str)
**			ncl_lentostr(len, str)
**			ncl_angtostr(ang, str)
**			ncl_picktostr(pick, str)
**			int ncl_get_str(str, prompt)
**			int ncl_get_num(num, prompt)
**			int ncl_get_dlabel(label, prompt, selmask)
**			int ncl_get_dlabel_rel (defmode, lab, prompt, selmask)
**			int ncl_get_dlabel1(lab, prompt, selmask)
**			int ncl_popup(menu, choice)
**			int ncl_get_coord(coord)
**			int ncl_get_z(zval) 
**			nclu_set_command_src()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nedasif1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:30
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "modef.h"
#include "mdebug.h"
#include "mdcoord.h"
#include "dinput.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclfc.h"
#include "lcom.h"
#include "udforms.h"
#include "udfdata.h"
#include "lumb.h"

extern UU_LOGICAL ncl_where;
extern UD_POPUPREC nmodfy[];
extern int NCL_accuracy;

/*
.....MAXPICK defines limit of entities to be picked via the SELECT subsystem.
.....Currently limited by max. num of curves from which to build a composite 
.....curve.
... aak 09-dec-1997: transferred NCL_MAXPICK to "nclcmd.h"
#define NCL_MAXPICK 100
*/
static int S_save_source_mod();
/*********************************************************************
**    E_FUNCTION     : ncl_cctostr(option, msccc, str)
**			Convert a MCS coordinate to an equivalent CCS representation
**			and then into a string "x,y[,z]".
**    PARAMETERS   
**       INPUT  : 
**				option				2 => x,y
**										3 => x,y,z
**          mcscc					cartesian coordinate
**       OUTPUT :  
**          str					string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cctostr(option, mcscc, str)
int option;
UM_coord mcscc;
char *str;
{
	UM_coord cc;

	um_mcstoccs(0, mcscc, cc);
	unitcv(cc);
	if (option == 2) 
	{
		ncl_sprintf(str,cc,2);
	}
	else if (option == 3) 
	{
		ncl_sprintf(str,cc,3);
	}
	else 
		strcpy(str,"\0");
}

/*********************************************************************
**    E_FUNCTION     : ncl_vctostr(mcsvc, str)
**			Convert a MCS vector into an equivalent CCS vector and
**			then into a string "i,j,k".
**    PARAMETERS   
**       INPUT  : 
**          mcsvc					vector
**       OUTPUT :  
**          str					string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_vctostr(mcsvc, str)
UM_vector mcsvc;
char *str;

{
	UM_vector vc;

	um_mcstoccs(1, mcsvc, vc);
	unitcv(vc);
	ncl_sprintf(str,vc,3);
}

/*********************************************************************
**    E_FUNCTION     : ncl_lentostr(len, str)
**       Convert a length to a string.
**    PARAMETERS   
**       INPUT  : 
**          len					length
**       OUTPUT :  
**          str					string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_lentostr(len, str)
UM_length len;
char *str;

{
	ncl_sprintf(str,&len,1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_angtostr(ang, str)
**       Convert an angle (in radians) to an angle in degrees and then
**			into a string.
**    PARAMETERS   
**       INPUT  : 
**          ang					angle
**       OUTPUT :  
**          str					string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_angtostr(ang, str)
UM_angle ang;
char *str;

{
	UU_REAL angdeg;

	angdeg = (360.0 / UM_TWOPI) * ang;
	ncl_sprintf(str,&angdeg,1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_picktostr(pick, str)
**       Convert a PICK to a string by getting the label of the
**			picked entity.
**    PARAMETERS   
**       INPUT  : 
**          pick					pick structure
**       OUTPUT :  
**          str					string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_picktostr(pick, str)
UM_PLOCREC *pick;
char *str;

{
	struct UC_entitydatabag e;

	uu_denter(UU_MTRC,(us,"ncl_picktostr(pick=%x,str=%x)", pick, str));

	e.key = um_get_pickkey(&(pick->pent), 1);
	if (e.key == 0) strcpy(str,pick->ploc.label);
	else
	{
		ur_retrieve_data_fixed(&e);
		ncl_get_label(&e, str);
	}

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_str(str, prompt)
**       Prompt the user to enter a text string. 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_str(str, prompt)
char str[];
int prompt;

{
	int status;
	int ret_status;
	int numint;
	UD_STRREC strrec;

	uu_denter(UU_MTRC,(us,"ncl_get_str(str=%x, prompt=%d)",
		*str, prompt));

	str[0] = '\0';
	strrec.instring = str;

	status = ud_ldas(UD_DASSTRINGDEF, UA_NCL, prompt, &strrec, 80,
				&numint, UD_NODEFAULT);

	ret_status = NCL_OKINPUT;
	if (numint == 0) ret_status = NCL_NOINPUT;

	uu_dexit;
	return (ret_status);
}


/*********************************************************************
**    E_FUNCTION     : int ncl_get_num(num, prompt, num_str)
**       Prompt the user to enter a numeric value string. 
**    PARAMETERS   
**       INPUT  : 
**          prompt	number of prompt message to dislay
**       OUTPUT :  
**          num		numeric value entered
**			num_str: string enterd  (allow scalar string)
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_num(num, prompt, num_str)
int *num;
int prompt;
char *num_str;
{
	int status;
	int ret_status;
	int numint;
	UD_SCA_IVAL num_in;
	uu_denter(UU_MTRC,(us,"ncl_get_num(num=%d, prompt=%d)",
		num, prompt));

	status = ud_ldas(UD_SCAINT, UA_NCL, prompt, &num_in, 1,
				&numint, UD_NODEFAULT);

	ret_status = NCL_OKINPUT;
	if (numint == 0) ret_status = NCL_NOINPUT;
	else
	{
		*num = num_in.value;
		strcpy(num_str, num_in.label);
	}
	uu_dexit;
	return (ret_status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_dlabel(defmode, lab, prompt, selmask)
**			Limit DAS to pick the specified entities (SELMASK), prompt
**			the user to enter a label field.
**    PARAMETERS   
**       INPUT  : 
**				defmode					UD_DASPCKLOC or UD_DASSTRING
**				prompt					prompt message number
**				selmask					DAS  pick select mask
**       OUTPUT :  
**          lab   					label of entity selected
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_dlabel(defmode, lab, prompt, selmask)
int defmode;
char *lab;
int prompt;
int selmask[];

{
	UM_PLOCREC pick;
	int inpmode;
	int ret_status;
	int numint;

	inpmode = defmode;
	do
	{
/*
.....Pick mode
*/
		if (inpmode == UD_DASPCKLOC)
		{
			ud_lgeo(UU_TRUE, selmask);
			pick.ploc.label[0] = '\0';
			ret_status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, prompt, &pick, 1,
								&numint, 1); 
			if (numint > 0)
			{
				strcpy(lab,pick.ploc.label);
				ret_status = NCL_OKINPUT;
			}
			else if (ret_status)
			{
/*
......if done is hit
*/
				ret_status = NCL_DONE;
			}
			else 
				ret_status = NCL_NOINPUT;
		}
/*
.....Text mode
*/
		if (inpmode == UD_DASSTRING)
		{
/*
.....why strcpy(lab,prompt); it cause error because the prompt is prompt number, not string
.....Yurong
*/
/*			strcpy(lab,prompt); */
			ret_status = ncl_get_str(lab, prompt);
		}
	}
	while (ret_status == NCL_ALTACTION); 
	return(ret_status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_dlabel_rel (lab,nlabel,prompt,
**                                            selmask,key,reltyp)
**			Limit DAS to pick the specified entities (SELMASK), prompt
**			the user to enter a label field.
**    PARAMETERS   
**       INPUT  : 
**          flag						UU_TRUE - Project near point onto entity.
**          prompt					prompt message number
**          selmask					DAS  pick select mask
**       OUTPUT :  
**          lab   					label of entity selected
**          nlabel					string defining a near point
**          key   					key of picked entity
**          reltyp					relation number of picked entity
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_dlabel_rel (lab,nlabel,flag,prompt,selmask,key,reltyp)
char *lab,*nlabel;
UU_LOGICAL flag;
int prompt;
int selmask[];
UU_KEY_ID *key;
int *reltyp;
{
	UM_PLOCREC pick;
	int ret_status;
	int numint;
	UU_REAL uv[2];
	UM_coord picked;
	UM_vector vpnorm;
	struct NCL_fixed_databag ent;


	ud_lgeo(UU_TRUE, selmask);
	ret_status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, prompt, &pick, 1,
		&numint, 1); 
	if (numint > 0)
	{
		strcpy(lab,pick.ploc.label);


		ret_status = NCL_OKINPUT;
		if (pick.pent.key[0] != 0 || pick.pent.key[1] != 0)
		{
			*key = um_get_pickkey(&pick.pent, 1);
			um_retrieve_data_relnum(*key, reltyp);
		}
		else
			*reltyp = 1;
/*
.....Project picked point to entity
*/
		um_ploctocc(&pick.ploc,picked);
		um_vpnorm(pick.ploc.transform, vpnorm);
		if (flag)
		{
			ent.key = *key;
			ncl_retrieve_data_fixed(&ent);
			ncl_pickptonent(&ent,picked,vpnorm,.005,picked,uv,vpnorm);
		}
/*
.....Project picked point to working plane
*/
		else
			ncl_proj_to_wp (picked, vpnorm, picked);
/*
.....Format selection point
*/
		strcpy(nlabel,"(PT/");
		ncl_cctostr (3,picked,&nlabel[4]);
		strcat(nlabel,")");
	}
	else if (ret_status==UU_TRUE)
		return NCL_DONE;
	else
		ret_status = NCL_NOINPUT;

	return(ret_status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_dlabel1(lab, prompt, selmask)
**			Limit DAS to pick the specified entities (SELMASK), prompt
**			the user to enter a label field.
**    PARAMETERS   
**       INPUT  : 
**          prompt					prompt message number
**          selmask					DAS  pick select mask
**       OUTPUT :  
**          lab   					label of entity selected
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done"
**			NCL_ALTACTION iff user hit "alt-act"
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_dlabel1(lab, prompt, selmask)
char *lab;
int prompt;
int selmask[];
{
	UM_PLOCREC pick;
	int ret_status;
	int numint;

	ud_lgeo(UU_TRUE, selmask);
	ret_status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, prompt, &pick, 1,
								&numint, 1); 
	if (numint > 0)
	{
		strcpy(lab,pick.ploc.label);
		ret_status = NCL_OKINPUT;
	}
	else if (ret_status == UD_DASALTACTION)
		ret_status = NCL_ALTACTION;
	else
		ret_status = NCL_NOINPUT;

	return (ret_status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_modifier(str, menu)
**       Prompt the user to pick a modifier.
**    PARAMETERS   
**       INPUT  : 
**          menu = Which popup menu to display.
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : 
**			Only supports the following popup menus.
**
**					NCL_XYZ_DIRECTION
**
**    WARNINGS     : none
*********************************************************************/
int ncl_get_modifier(str, menu)
char *str;
int menu;
{
	int status;
	int ret_status;
	int choice;
	int outchoice;

/*
.....Display popup menu
*/
	do
	{
		status = ud_ddas(UD_POPUP, &nmodfy[menu], &choice,
					1, &outchoice, UD_NODEFAULT);
	} while  (status != 1);
/*
.....Get menu selection
*/
	ret_status = NCL_OKINPUT;
	strcpy(str,"");
	switch (menu)
	{
	case NCL_XYZ_DIRECTION:
		switch (choice)
		{
		case 1:
			strcpy(str, NCL_posx);
			break;
		case 2:
			strcpy(str, NCL_negx);
			break;
		case 3:
			strcpy(str, NCL_posy);
			break;
		case 4:
			strcpy(str, NCL_negy);
			break;
		case 5:
			strcpy(str, NCL_posz);
			break;
		case 6:
			strcpy(str, NCL_negz);
			break;
		default:
			ret_status = NCL_NOINPUT;
			break;
		}
		break;
	case NCL_WHICH_LINE:
		switch (choice)
		{
		case 1:
			strcpy(str,NCL_xaxis);
			break;
		case 2:
			strcpy(str,NCL_yaxis);
			break;
		case 3:
			ncl_get_dlabel(UD_DASPCKLOC,str,46,UD_ncl_pvln);
			break;
		default:
			ret_status = NCL_NOINPUT;
			break;
		}
	}
	return(ret_status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_popup(menu, choice)
**       Put up popup MENU and return user response (CHOICE).
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_popup(menu, choice)
int menu;
int *choice;

{
	int status;
	int ret_status;
	int outchoice;

	uu_denter(UU_MTRC,(us,"ncl_popup(menu=%d, choice=%x)",
		menu, choice));

	status = ud_ddas(UD_POPUP, &nmodfy[menu], choice,
				1, &outchoice, UD_NODEFAULT);

	if (status == 1) 
		ret_status = NCL_OKINPUT;
	else
		ret_status = NCL_DONE;

	uu_dexit;
	return (ret_status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_get_select_labels(cmdbuf, numint)
**       Place list of label retrieved from select buffer into cmdbuf
**    PARAMETERS   
**       INPUT  : 
**          cmdbuf
**          numint - number of labels to retrieve
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_select_labels(cmdbuf,numint)
NCL_cmdbuf *cmdbuf;
int numint;
{
	struct UC_entitydatabag e1;
	UU_LOGICAL lstatus;
	char str[256];

	if (numint > 0)
	{
		lstatus = UU_TRUE;
/*
........While there are picked entities on the pick stack...
*/
		while(ud_gnxt(lstatus, UU_NULL, &e1.key, 1) == UU_TRUE)
		{                                              
			lstatus = UU_FALSE;
/*       
......get names of picked geometry 
*/
			if ((ur_retrieve_data_fixed(&e1) == UU_SUCCESS))
			{
				if (ncl_legal_relation(e1.rel_num))
			   {
				   ncl_get_label(&e1, str);
				   ncl_add_token(cmdbuf, str, NCL_comma);
			   }
			}
		}
/*
........Reached the end of the pick list, Delete the last ','
*/
		ncl_del_token(cmdbuf,"", UU_TRUE);
		return(NCL_OKINPUT);
	}

	return(NCL_NOINPUT);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_get_coord(coord)
**       Prompt the user to select coordinates. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          coord		x and y coordinates  
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_coord(coord)

UD_NDCLOCREC *coord;
{
	int status;
	int ret_status;
	int numint;

	uu_denter(UU_MTRC,(us,"ncl_get_coord()"));
/*
.....Prompt user for coordinates, it will get value and string input
*/
	status = ud_ldas(UD_SCACART, UM_MODEL, 35, coord, 1,
				&numint, UD_NODEFAULT);

	ret_status = NCL_OKINPUT;
/*
.....If user selected done, then set ret_status to NCL_NOINPUT
*/
	if (numint == 0) ret_status = NCL_NOINPUT;

	uu_dexit;
	return (ret_status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_z(zval)
**       Prompt the user to enter a Z value. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**         zval		numeric value entered
**			zstr:   string entered (we allow use input scalar,
**					so we will return value and string)
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_z(zval, zstr)
UU_REAL *zval;
char *zstr;
{
	int status;
	int ret_status;
	int numint;
	UD_SCA_VALUE val;

	uu_denter(UU_MTRC,(us,"ncl_get_z()"));

/*
.....Prompt user for Z value.
*/
	status = ud_ldas(UD_SCADISTANCE, UA_NCL, 3, &val, 1,
				&numint, UD_DEFAULT);

	ret_status = NCL_OKINPUT;

/*
.....If user hit done set ret_status to NCL_NOINPUT
*/
	if (numint == 0) ret_status = NCL_NOINPUT;
	else
	{
		*zval = val.value;
		strcpy(zstr, val.label);
	}
	uu_dexit;
	return (ret_status);
}

/*********************************************************************
**
**		I_FUNCTION         :  UD_FSTAT fmttog(fieldno, val, stat)
**
*********************************************************************/
static UD_FSTAT fmttog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	ud_default_method(fieldno, val, stat);
/*
.....Enable correct fields
.....based on toggle field value
*/
	switch(*fieldno)
	{
	case 0:
		if (val->frmint[0] == 0)
		{
			for (i=1;i<=8;i++) ud_set_traverse_mask(i,UU_FALSE);
		}
		else
		{
			for (i=1;i<=8;i++) ud_set_traverse_mask(i,UU_TRUE);
		}
		break;
	default:
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : nclu_set_command_src()
**             Set the format options for how command lines
**             are output.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_set_command_src()
{
	int major,label,vocab,fmt,status;
	int accuracy,align,indall,indsep;
	int  *ans[9];
	UM_int2 major_case,label_case,vocab_case;
	UM_int2 indx,indent_all,indent_sep,param_align,param_fmt;
	static char traverse[] = {1,1,1,1,1,1,1,1};
	static UD_METHOD methods[] = {fmttog,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL};
	static char called[] = {6,6,6,6,6,6,6,6};
/*
.....Get the current value for indent (all and sep),
.....vocab_case, label_case, and major_case.
*/
	indx = 230;
	getifl(&indx,&indent_all);
	indx = 231;
	getifl(&indx,&indent_sep);
	indx = 356;
	getifl(&indx,&vocab_case);
	indx = 357;
	getifl(&indx,&label_case);
	indx = 355;
	getifl(&indx,&major_case);
	indx = 358;
	getifl(&indx,&param_align);
	indx = 360;
	getifl(&indx,&param_fmt);
/*
.....Set form variables
*/
	indall = indent_all;
	indsep = indent_sep;
	major = major_case;
	label = label_case;
	vocab = vocab_case;
	accuracy = NCL_accuracy;
	align = param_align;
	fmt = param_fmt;

	ans[0] = (int *)&fmt;
	ans[1] = (int *)&major;
	ans[2] = (int *)&label;
	ans[3] = (int *)&vocab;
	ans[4] = (int *)&accuracy;
	ans[5] = (int *)&align;
	ans[6] = (int *)&indall;
	ans[7] = (int *)&indsep;
/*
.....Set the active fields
*/
	if (fmt == 1)
	{
		traverse[1] = 1;
		traverse[2] = 1;
		traverse[3] = 1;
		traverse[4] = 1;
		traverse[5] = 1;
		traverse[6] = 1;
		traverse[7] = 1;
	}
	else
	{
		traverse[1] = 0;
		traverse[2] = 0;
		traverse[3] = 0;
		traverse[4] = 0;
		traverse[5] = 0;
		traverse[6] = 0;
		traverse[7] = 0;
	}
/*
.....Call the form
*/
	status = ud_form1("srccmd.frm",ans,ans,methods,called,UU_NULL,traverse);
	if (status == -1) return(status);
/*
.....Set the variables returned from the form
*/
	indent_all = indall;
	indent_sep = indsep;
	major_case = major;
	label_case = label;
	vocab_case = vocab;
	NCL_accuracy = accuracy;
	param_align = align;
	param_fmt = fmt;
/*
.....Set ifl flags
*/
	indx = 230;
	setifl(&indx,&indent_all);
	indx = 231;
	setifl(&indx,&indent_sep);
	indx = 355;
	setifl(&indx,&major_case);
	indx = 356;
	setifl(&indx,&vocab_case);
	indx = 357;
	setifl(&indx,&label_case);
	indx = 358;
	setifl(&indx,&param_align);
	indx = 360;
	setifl(&indx,&param_fmt);
	
	UL_format_line = param_fmt;
	UL_major_case = major_case;
	UL_label_case = label_case;
	UL_vocab_case = vocab_case;
	UL_accuracy = NCL_accuracy;
	UL_alignment = param_align;
	UL_indent_all = indent_all;
	UL_indent_sep = indent_sep;
/*
......save the input into model file
*/
	S_save_source_mod();
	return(0);
}
/*********************************************************************
**    E_FUNCTION     : nlower(fstr,begin,end)
**          Converts a FORTRAN string to lower case
**    PARAMETERS
**       INPUT  :
**          begin         Location to begin changing to lower case
**          end           Location to end changing to lower case
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nlower(fstr,begin,end)
UM_f77_str_ptr fstr;
UM_int2 *begin;
UM_int2 *end;
{
	int i,first,last;
	char *cstr;

/*
.....Subtract one from begin and end to adjust to C array
*/
	first = *begin - 1;
	last = *end - 1;
/*
.....Convert FORTRAN string to C string
*/
	cstr = UM_cstr_of_f77_str(fstr);
/*
.....Convert to lower case
*/
	for(i=first;i<last;i++)
		if (isupper(cstr[i])) cstr[i] = tolower(cstr[i]);

	return;
}

/*********************************************************************
**    E_FUNCTION     : nupper(fstr,begin,end)
**           Converts a fortran string to upper case
**    PARAMETERS
**       INPUT  :
**          begin        The location to start changing to upper case
**          end          The location to stop changing to upper case
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nupper(fstr,begin,end)
UM_f77_str_ptr fstr;
UM_int2 *begin;
UM_int2 *end;
{
	int i;
	char *cstr;

/*
.....Convert FORTRAN string to c
*/
	cstr = UM_cstr_of_f77_str(fstr);
/*
.....Convert to upper case
*/
	for(i=*begin-1;i<*end;i++)
		if (islower(cstr[i])) cstr[i] = toupper(cstr[i]);

	return;
}

/*********************************************************************
**    E_FUNCTION     : S_save_source_mod()
**       Save the command line (#SOURCE#) settings into
**       modals file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_source_mod()
{
	int stat;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy(fname,"ncl_source.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,
		fname,3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
/*
.....Store playback modals
*/
	ux_fputs0("#SOURCE#\n", fptr);
	if (UL_format_line==1)
		ux_fputs0("/FORMAT/ *YES\n", fptr);
	else
		ux_fputs0("/FORMAT/ *NO\n", fptr);
	if (UL_major_case==0)
		ux_fputs0("/MAJOR_CASE/ *SAME\n", fptr);
	else if (UL_major_case==1)
		ux_fputs0("/MAJOR_CASE/ *UPPER\n", fptr);
	else
		ux_fputs0("/MAJOR_CASE/ *LOWER\n", fptr);
	if (UL_label_case==0)
		ux_fputs0("/LABEL_CASE/ *SAME\n", fptr);
	else if (UL_label_case==1)
		ux_fputs0("/LABEL_CASE/ *UPPER\n", fptr);
	else
		ux_fputs0("/LABEL_CASE/ *LOWER\n", fptr);
	if (UL_vocab_case==0)
		ux_fputs0("/VOCAB_CASE/ *SAME\n", fptr);
	else if (UL_vocab_case==1)
		ux_fputs0("/VOCAB_CASE/ *UPPER\n", fptr);
	else
		ux_fputs0("/VOCAB_CASE/ *LOWER\n", fptr);

	sprintf(msg,"/ACCURACY/ %d\n", UL_accuracy);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/ALIGNMENT/ %d\n", UL_alignment);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/INDENT_ALL/ %d\n", UL_indent_all);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/INDENT_SEP/ %d\n", UL_indent_sep);
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

