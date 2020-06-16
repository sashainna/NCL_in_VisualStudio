/********************************************************************
**    NAME         :  tspmodals.c
**
**       Functions to read in and set global information for NCLSTEP.
**       The information is read in from a modal file pointed to by
**       an environmental variable UL_STEP_MODALS.  It is read in when
**       NCLSTEP first starts.  Other functions process this information
**       and set global data structures such as naming conventions
**       (subscripts or postfixs and the base name -> this is used
**       during automatic name generation when going from .step to
**       unibase), geometry filtering (selectively translate geometry
**       types: for example, translate points but not vectors), and
**       shading.
**
**    CONTAINS:
**			utp_load_modals
**			utp_load_color_mode
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**		  tspmodals.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**		  10/27/16 , 15:21:14
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "mdattr.h"
#include "mdrel.h"
#include "mlab.h"
#include "umath.h"
#include "riddldef.h"
#include <stdio.h>
#include <string.h>
#include "xenv1.h"		/* used by "ux" calls: UX_PRTERRS value */
#include "xfsys1.h"
#include "udebug.h"
#include "nclver.h"
#include "nclfc.h"
#include "msrf.h"
#include "tigglobal.h"

extern int UIG_color_iges;
extern int UIG_use_sub;
extern int UIG_concat_sub;
extern UU_LOGICAL UTP_step_214;
extern int output_units;
extern int output_normals;
extern char NCL_init_fstr[20];

static char scolor[2][64] = {"Default", "Auto"};
static char yesno[2][64] = {"*NO","*YES"};

void S_set_lucency(char *param);
/*********************************************************************
**    E_FUNCTION     :  utp_load_modals()	
**       Read in NCLSTEP modal file and setup global information.
**       This modal file is pointed to by an environmental variable
**       UL_STEP_MODALS.  The file should be read only once, when NCLSTEP
**       first starts up.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**				none
**    RETURNS      :	UU_SUCCESS or UU_FAILURE or -2 (failed in function
**																and couldn't close file) 
**    SIDE EFFECTS :	Modifies global arrays geo_lab and lab_flag when
**								reading in file.
**							Modifies file when updating (in_out = 1) 
**    WARNINGS     : none
*********************************************************************/
int utp_load_modals()
{
	int i, j, status, type, iges_dispatch, open_file, numint;
	char str_in[81], modal[81], param[81];
	static char *mod_types[64] = {"GEOMETRY_NAME_MODALS","FILTER",
		"ATTRIBUTES","LABEL_MATCHING", "INCLUDE", "FILTER_OUT", "STEP_OUT"};
	FILE *iges_config_file;
	char *str,*ux_getenv();
	UU_LOGICAL found;
	UX_pathname fullname;
	char *pathlistptr = UU_NULL;
	
	iges_dispatch = 0;
	UTP_step_214 = UU_FALSE;
	output_units = -1;
	output_normals = 1;
/*
..... Open STEP modals file.
*/
	str = ux_getenv("UL_STEP_MODALS", UX_NPRTERRS);
	if (str != 0) 
	{
		ux_get_syspath("UL_STEP_MODALS", &pathlistptr, fullname, 
			&found, UX_NPRTERRS|UX_NCHK|UX_NQUOTES);
		uu_lsdel(pathlistptr);
/*
.....See if file exists
*/
		status = ul_open_mod_file("UU_USER_SETTINGS", "modals", NCL_init_fstr,
			UU_NULL, fullname, 2, &iges_config_file);
		if ((status != UU_SUCCESS)||(iges_config_file==UU_NULL))
		{
			status = UU_FAILURE;
		}
	}
	else
		status = UU_FAILURE;


	uig_change_labelmdl(UU_FALSE);	
	if (status != UU_SUCCESS)
		open_file = 0;
	else
		open_file = 1;
	
	if (open_file == 1)
	{      
		while (feof(iges_config_file) == 0)
		{
			status = ul_fread (iges_config_file, str_in, 80, &numint);
			if (status != UU_SUCCESS)
			{
/*
..... Did not read file.  If not at end of file, fail.
*/
				if (status == UX_EOF)
					break;
			}

			strcpy(modal,"");  strcpy(param,"");
			status = ul_modal_check(str_in, &type, modal, param);
			if (status != UU_SUCCESS) break;
/*
..... If comment, do nothing and go on to next line.
..... If section line, find out the section and set iges_dispatch.
..... Iges_dispatch remembers what section you are in while looping
..... through all the lines in that section.  It will allow the modal 
..... lines to be sent to the correct function to handle them.
..... If modal line, use iges_dispatch to determine which function
..... to use to process the line.
..... Use status = UU_FAILURE to break loop; also can use 
..... iges_dispatch = -1 to get out of loop.
*/	
			switch(type)
			{
				case 0:		/* Comment line */
					break;
				case 1:		/* Section line ->  #GEOMETRY_NAME_MODALS# */
					iges_dispatch = -1;
					status = UU_FAILURE;
/*	
..... status = UU_FAILURE breaks while loop
..... If match is found, reset status = UU_SUCCESS. 
*/
					for (i = 0; i < 7; i++)
					{
						if (strcmp(modal, mod_types[i]) == 0)
						{
							iges_dispatch = i+1;
							status = UU_SUCCESS;
							break;
						}
					}
					break;
				case 2:		/* Actual modal data line to process. */
					switch (iges_dispatch)
					{
						case 1:
							S_load_name_data(modal, param);
							break;
/*
.....#FILTER#
*/
						case 2:
							S_load_filter(modal, param);
							break;
/*
.....#ATTRIBUTES#
*/
						case 3:
							S_load_attributes(modal,param);
							break;
/*
.....#LABEL_MATCHING#
*/
						case 4:
							S_load_matching(modal,param);
							break;
/*
.....#INCLUDE#
*/
						case 5:
							if (strcmp(modal, "FILE") == 0)
   								utp_load_color_mode(param);
							break;
/*
.....#FILTER_OUT#
*/
						case 6:
							S_load_out_filter(modal, param);
							break;
/*
.....#STEP_OUT#
*/
						case 7:
							S_load_out_step(modal, param);
							break;
						default:           /* status = UU_FAILURE breaks while loop */
							status = UU_FAILURE;
					}
					break;
				default:				/* satatus = UU_FAILURE breaks while loop*/
					status = UU_FAILURE;
			}

			if (iges_dispatch == -1)
				status = UU_FAILURE;

		} /* End While loop  -> should have read entire file line by line */
	}
/*
..... If there was a problem before this, status = -1 = UU_FAILURE.
..... If not, then status = 0 = UU_SUCCESS.
..... Either way, if closing file successful, status stays the same;
..... however, if there is a problem, status-- (in other words,
..... status = status + UU_FAILURE = status - 1).
*/
	if (open_file == 1)
		status += ux_fclose0(iges_config_file);
	
	return UU_SUCCESS;
}

/*********************************************************************
**       I_FUNCTION : S_modals_custom_color(ctyp,cmsg,name)
**                      This function sets the custom color modals.
**       PARAMETERS     
**               INPUT  :
**                  ctyp = Modal begin defined.
**                  cmsg = Modal ctyp's parameter.
**               OUTPUT :
**                  name     = Name of color being defined.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_modals_custom_color (ctyp,cmsg,name)
char *ctyp,*cmsg,*name;
{
	char serror[200];
	int i,status,inum,col[3];
	UU_REAL rval[3];
	int maxsub=2;
	static char *csub[] = {"NAME", "RGB"};
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
		sprintf (serror,"Not a valid COLOR modal.  /%s/ %s",
			ctyp,cmsg);
		ud_wrerr (serror);
		goto failed;
	}
	switch(i)
	{
/*
.....Name
*/
	case 0:
		strcpy(name,cmsg);
		break;
/*
.....RGB values
*/
	case 1:
		if ((ul_to_reals(&rval,&inum,3,cmsg) != UU_SUCCESS) || inum != 3)
			goto bad_parm;
		col[0] = rval[0]; col[1] = rval[1]; col[2] = rval[2];
		utp_store_custom_color(name,col);
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (serror,"Invalid value for COLOR modal. /%s/ %s",ctyp,cmsg);
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
**       E_FUNCTION : S_load_clrfile(fptr)
**           This function loads the color file
**                      from a disk file.
**       PARAMETERS     
**               INPUT  :  fptr: color file to be load.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int S_load_clrfile(fptr)
FILE *fptr;
{
	char serror[80],buf[80],ctyp[40],cmsg[40],name[96];
	int status,stat,numint,ityp,i,isub,istat;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	name[0] = '\0';
/*
.....Read a record
*/
	do
	{
		stat = ul_fread (fptr,buf,sizeof(buf),&numint);
		if (stat == UX_EOF)
		{
			goto file_done;
		}
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			sprintf (serror,"Error reading from color file E%d.",stat);
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
			sprintf (serror,"Color file syntax error. %s",buf);
			ud_wrerr (serror);
		}
/*
.....Subsystem type
*/
		switch (ityp)
		{
		case 1:
			ul_to_upper(ctyp);
			if (strcmp(ctyp, "COLOR") != 0)
			{
				sprintf (serror, "Not a valid COLOR parameter. %s", buf);
				ud_wrerr (serror);
				break;
			}
   			break;
		case 2:
   			S_modals_custom_color(ctyp,cmsg,name);
   			break;
		}
	}
	while (stat == UU_SUCCESS || stat == UX_NO_SPACE);
file_done:;
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
**    I_FUNCTION     :  S_load_filter(modal, param)	
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      :	UU_SUCCESS or UU_FAILURE 
**    SIDE EFFECTS :	none	
**    WARNINGS     : none
*********************************************************************/
static int S_load_filter(modal, param)
char modal[81], param[81];
{
	int j,flag;
	static char *types[]={"CURVES","COMPONENTS","SOLIDS","WIREFRAME","PLANES",
		"POINTS"};
	static char opts[3][64]={"*NO","*YES","*BOTH"};
/*
.....Set the modals values
*/
	for (j=0;j<6;j++)
	{
		if (strcmp(modal,types[j]) == 0)
		{
			switch (j)
			{
/*
......CURVES
*/
			case 0:
				if (ul_modal_toggle(param,yesno,2,&flag) == UU_SUCCESS)
					entity_mask[0] = flag;
				break;
/*
......COMPONENTS
*/
			case 1:
				if (ul_modal_toggle(param,opts,3,&flag) == UU_SUCCESS)
					UIG_splitccrv = flag;
				break;
/*
......SOLIDS
*/
			case 2:
				if (ul_modal_toggle(param,opts,2,&flag) == UU_SUCCESS)
					entity_mask[1] = flag;
				break;
/*
......WIREFRAME
*/
			case 3:
				if (ul_modal_toggle(param,opts,2,&flag) == UU_SUCCESS)
					entity_mask[2] = flag;
				break;
/*
......PLANES
*/
			case 4:
				if (ul_modal_toggle(param,opts,2,&flag) == UU_SUCCESS)
					entity_mask[3] = flag;
				break;
/*
......POINTS
*/
			case 5:
				if (ul_modal_toggle(param,opts,2,&flag) == UU_SUCCESS)
					entity_mask[4] = flag;
				break;
			}
			break;
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  S_load_out_filter(modal, param)	
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      :	UU_SUCCESS or UU_FAILURE 
**    SIDE EFFECTS :	none	
**    WARNINGS     : none
*********************************************************************/
static int S_load_out_filter(modal, param)
char modal[81], param[81];
{
	int j,flag;
	static char *types[]={"SOLIDS","SURFACES","WIREFRAME","PLANES","POINTS",
		"NETSURFS","UNTRIMMED"};
/*
.....Set the modals values
*/
	for (j=0;j<7;j++)
	{
		if (strcmp(modal,types[j]) == 0)
		{
			if (ul_modal_toggle(param,yesno,2,&flag) == UU_SUCCESS)
				entity_out[j] = flag;
			break;
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  S_load_out_step(modal, param)	
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      :	UU_SUCCESS or UU_FAILURE 
**    SIDE EFFECTS :	none	
**    WARNINGS     : none
*********************************************************************/
static int S_load_out_step(modal, param)
char modal[81], param[81];
{
	int j,flag;
	static char *types[]={"FORMAT","UNITS","NORMALS"};
	static char fmts[2][64]={"*AP203","*AP214"};
	static char opts[3][64]={"*SAME","*INCH","*MM"};
/*
.....Set the modals values
*/
	for (j=0;j<3;j++)
	{
		if (strcmp(modal,types[j]) == 0)
		{
			switch (j)
			{
			case 0:
				if (ul_modal_toggle(param,fmts,2,&flag) == UU_SUCCESS)
					UTP_step_214 = flag;
				break;
			case 1:
				if (ul_modal_toggle(param,opts,3,&flag) == UU_SUCCESS)
					output_units = flag - 1;
				break;
			case 2:
				if (ul_modal_toggle(param,yesno,2,&flag) == UU_SUCCESS)
					output_normals = flag;
				break;
			}
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_init_label(modal,param)
**			Initiates the variable forreinitialization of labels 
**			according to the mod file.
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES.
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the reinitialization of labels variable
**						   UIG_reinit_lab	
**    WARNINGS     : none
**		AUTHOR		 : Himani	
*********************************************************************/
static int S_load_init_label(modal, param)
char modal[81], param[81];
{
	int i;
	if (ul_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	if(i == 0) UIG_reinit_lab = UU_FALSE;
   	else
      	UIG_reinit_lab = UU_TRUE;
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  S_load_attributes(modal,param)
**       Processes the #ATTRIBUTES# section of the modals file.
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of modal.
**          param  =  parameter belonging to modal.
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	none
**    WARNINGS     : none
*********************************************************************/
static int S_load_attributes(modal, param)
char modal[81], param[81];
{
	int j;
	static char *attr_types[] =
		{"SHADED","INIT_LABELS","DUPLICATES","COLORS","CONVERSION","SURF_CURVE",
		"LABELS","MAX_LABEL","SUBSCRIPTS","CONCATENATE", "EDGE_DISPLAY",
		"EDGE_COLOR", "FILE","ON_LAYERS","LAYER_START","TRANSLUCENCY"};
/*
.....Set the modal values
*/
	for(j=0;j<16;j++)
	{
		if (strcmp(modal, attr_types[j]) == 0)
		{
			switch (j)
			{
			case 0:
				S_load_shade_surf(modal,param);
				break;
			case 1:
				S_load_init_label(modal,param);
				break;
			case 2:
				S_load_no_dups(modal,param);
				break;
			case 3:
				S_load_color(modal,param);
				break;
			case 4:
				S_load_conv_opt(modal,param);
				break;
			case 5:
				S_load_srf_crv(modal,param);
				break;
			case 6:
				S_load_label_opt(modal,param);
				break;
			case 7:
				S_load_max_lab(modal,param);
				break;
			case 8:
				S_load_subscript(modal,param);
				break;
			case 9:
				S_load_concatenate(modal,param);
				break;
			case 10:
				S_load_edge_disp(modal,param);
				break;
			case 11:
				S_load_edge_color(modal,param);
				break;
			case 12:
				utp_load_color_mode(param);
				break;
			case 13:
			case 14:
				S_set_layer_opt(modal,param);
				break;
			case 15:
				S_set_lucency(param);
				break;
			}
			break;
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_color(modal,param)
**			Initializes the variable fo rhte color scheme according to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by IGES,
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the color seting variable
**							UIG_color_iges
**    WARNINGS     : none
**		AUTHOR		 : Himani	
*********************************************************************/
static int S_load_color(modal, param)
char modal[81], param[81];
{
	int i;
	static char color[2][64] = {"*NCL","*STEP"};

	if (ul_modal_toggle(param,color,2,&i) == UU_SUCCESS)
   	if(i == 0) UIG_color_iges = UU_FALSE;
   	else
      	UIG_color_iges = UU_TRUE;
	return (0);
}

/*********************************************************************
**		E_FUNCTION     :  utp_load_color_mode(file)
**			load color mod file
**
**    PARAMETERS   
**       INPUT  :
**          file  =  mode file contain color defines
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	none
**    WARNINGS     : none	
*********************************************************************/
int utp_load_color_mode(file)
char *file;
{
	UX_pathname modfile;
	char serror[200],sbuf[80];
	int status,stat,opend;
	FILE *fptr;

	opend = 0;
#if UU_COMP != UU_WIN2K
	status = UU_SUCCESS;
	goto done;
#endif

	if (file[0]=='\0')
		strcpy(modfile,"nclstep_color.mod");
	else
		strcpy(modfile, file);
/*
.....load file
*/
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", NCL_init_fstr, UU_NULL,
		modfile, 2, &fptr);
	if (stat!=UU_SUCCESS || fptr==UU_NULL)
	{
		ul_short_filename(modfile,sbuf,50);
		sprintf (serror,"Cannot open Modals file %s",modfile);
		ud_wrerr (serror);
		return UU_FAILURE;
	}
/*
.....Load the included modals file
*/
	opend = 1;
	status = S_load_clrfile(fptr);
	if (status == UU_SUCCESS)
		goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (opend == 1) ux_fclose0 (fptr);
	return(status);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_concatenate(modal,param)
**			Initializes the variable for the subscript to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES or NO
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the color seting variable
**							UIG_concat_sub
**    WARNINGS     : none	
*********************************************************************/
static int S_load_concatenate(modal,param)
char modal[81], param[81];
{
	int i;
	if (ul_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
		utp_set_label_method(-1,-1,i);
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_conv_opt(modal,param)
**			Initializes the variable for the conversion option according to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by ALL or LAYERS,
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the color seting variable
**							UIG_conv_opt
**    WARNINGS     : none	
*********************************************************************/
static int S_load_conv_opt(modal, param)
char modal[81], param[81];
{
	int i;
	static char alllayer[2][64] = {"*ALL","*COMPONENTS"};
	if (ul_modal_toggle(param,alllayer,2,&i) == UU_SUCCESS)
		UIG_conv_opt=i;
   	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_edge_color(modal,param)
**			Initializes the variable for the edge color to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by COLORs
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the color seting variable
**							UIG_edge_color
**    WARNINGS     : none	
*********************************************************************/
int S_load_edge_color(modal,param)
char modal[81], param[81];
{
	int index;
	if (ul_modal_color(param, &index, scolor, 2)
			!= UU_SUCCESS) return 0;
		
	if ((index<0)&&(index!=-1))
		return 0;
		
	if (index == -1)
		UIG_edge_color = UIG_MAXCOLOR;
	else
	{
		UIG_edge_color = index;
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_edge_disp(modal,param)
**			Initializes the variable for the edge display to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES or NO
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the color seting variable
**							UIG_srf_edge
**    WARNINGS     : none	
*********************************************************************/
int S_load_edge_disp(modal,param)
char modal[81], param[81];
{
	int i;
	if (ul_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	UIG_srf_edge =i ;
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_label_opt(modal,param)
**			Initializes the variable for the Entity label option according to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by IGES,LABEL,PROPERTY,UNIBASE
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the color seting variable
**							UIG_lab_opt
**    WARNINGS     : none	
*********************************************************************/
static int S_load_label_opt(modal, param)
char modal[81], param[81];
{
	int i;
	static char labopt[5][64] = {"*NCL","*COMPONENT","*FACE","*RECORD",
		"*UNIBASE"};

	if (ul_modal_toggle(param,labopt,5,&i) == UU_SUCCESS)
	utp_set_label_method(i,-1,-1);
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_level_no(modal,cmsg)
**			Sets the level name
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of label matching attribute
**          param  =  a string containing ,'*' followed by Level name
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies level for matching labels
**							UIG_matchlevel	
**    WARNINGS     : none
**		AUTHOR		 : Himani	
*********************************************************************/
static int S_load_level_no(modal, cmsg)
char modal[81], cmsg[81];
{
	char *p,*c,sbuf[80];
	int i,n,ind;
	static char level[5][64] = {"*EXACT","*LEVEL_1","*LEVEL_2","*LEVEL_3",
		"*LEVEL_4"};

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
			if (ul_modal_toggle(sbuf,level,5,&ind) == UU_SUCCESS)
				UIG_matchlevel = ind;	
			c = p+1;
			i++;
		} while (p != UU_NULL);
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  S_load_match_color(modal,param,j)
**				Sets the color o rthe layer
**
**    PARAMETERS
**       INPUT  :
**          modal  =  name of label matching attribute
**          param  =  a string containing ,'*' followed by a color
**				j		 =  level #
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies color for matching label
**                   UIG_match_color_array[]
**    WARNINGS     : none
**    AUTHOR       : Himani
*********************************************************************/
static int S_load_match_color(modal, param, j)
char modal[81], param[81];
int j;
{
	int index;
		
	if (ul_modal_color(param, &index, scolor, 2)
			!= UU_SUCCESS) return 0;
	
	UIG_match_color_array[j] = -1;
	if ((index<0)&&(index!=-2))
		return 0;
	else if (index<0)
		index = -1;

	UIG_match_color_array[j] = index;
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_match_layer(modal,param,j)
**			Sets the layer number
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of label matching attribute
**          param  =  a string containing a number 
**				j		 =	 layer #
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies layer for matching label
**							UIG_match_layer_array[]
**    WARNINGS     : none
**		AUTHOR		 : Himani	
*********************************************************************/
int S_load_match_layer(modal, param, j)
char modal[81], param[81];
int j;
{
	UIG_match_layer_array[j] = atoi(param);
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_match_tol(modal,cmsg)
**			Sets the tolerance for matching
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of label matching attribute
**          param  =  a string containing the mathcing tolerance real value ,
**					followed by the unit MM/IN
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the tolerance for matching labels
**							UIG_match_tol	
**    WARNINGS     : none
*********************************************************************/
static int S_load_match_tol(modal, cmsg)
char modal[81], cmsg[81];
{
	int  count =0,len,i;
	char *param, toler[8], in_mm[4];
/*
.....set default
*/
	UIG_units_factor = 1;
	UIG_match_tol = UIG_match_tol_disp =0.001;

	strcpy(toler, "");  strcpy(in_mm, "");
	len = strlen(cmsg);
	ul_strip_blanks(cmsg,&len);
	param = cmsg;
	len = strlen(param);
	count =0;
	toler[0]='\0';
	in_mm[0]='\0';
	for (i = 0; i < len; i++)
	{
		if (param[i] == '.')
		{
			if(count == 0) count = 1;
			else  
				goto Default;
		}
		else
		{
			if (isalpha(param[i]))
			{
				strncpy(toler,param,i);
				toler[i]='\0';
				sscanf((param+i), "%s", in_mm);
				if(strlen(in_mm) ==2)
				{
					if((in_mm[0] == 'm' || in_mm[0] == 'M') && 
						(in_mm[1] == 'm'||in_mm[1] == 'M'))
						UIG_units_factor =25.4;
					else if((in_mm[0] == 'i' || in_mm[0] == 'I') && 
								(in_mm[1] == 'n'||in_mm[1] == 'N'))
						UIG_units_factor =1;
					else 
						goto Default;
					break;
				}
				else 
					goto Default;
			}
			else if (isdigit(param[i]))
			{
				if(i == len-1)
				{
					strcpy(toler,param);
					UIG_units_factor =1;
				}
			}
			else 
				goto Default;
		}
	}
	sscanf(toler,"%lf", &UIG_match_tol_disp);
	UIG_match_tol = UIG_match_tol_disp / UIG_units_factor;
	if (UIG_match_tol < .0001)
		UIG_match_tol = .0001;
	goto Done;
Default:;
	UIG_units_factor = 1;
	UIG_match_tol = UIG_match_tol_disp =0.001;

Done:;
	if (UIG_units_factor == 1)
		UIG_match_tol = UIG_match_tol_disp;
	if (UIG_units_factor == 25.4)
		UIG_match_tol = UIG_match_tol_disp / UIG_units_factor;

	return UU_SUCCESS;
}

/*********************************************************************
**    I_FUNCTION     :  S_load_matching(modal,param)
**       Processes the #LABEL_MATCHING# section of the modals file.
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of modal.
**          param  =  parameter belonging to modal.
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	none
**    WARNINGS     : none
*********************************************************************/
static int S_load_matching(modal, param)
char modal[81], param[81];
{
	int j,status;
	static char *lab_match[] =
		{"LEVEL","TOLER","LAYER_EXACT","LAYER_1","LAYER_2","LAYER_3","LAYER_4",
		"LAYER_MATCH","LAYER_IMPORT","COLOR_EXACT","COLOR_1","COLOR_2",
		"COLOR_3","COLOR_4","COLOR_MATCH","COLOR_IMPORT","IMPORT_GEO",
		"START_UNMATCH","REGRESSIVE"};
/*
.....Set the modal values
*/
	status = UU_SUCCESS;
	for(j=0;j<19;j++)
		if(strcmp(modal, lab_match[j]) == 0)
		{
			switch(j)
			{
			case 0:
				S_load_level_no(modal,param);
				break;
			case 1:
				S_load_match_tol(modal,param);
				break;
			case 2:
			case 3:
			case 4:
			case 5:
			case 6:
			case 7:
			case 8:
				S_load_match_layer(modal,param,j-2);
				break;
			case 9:
			case 10:
			case 11:
			case 12:
			case 13:
			case 14:
			case 15:
				S_load_match_color(modal,param,j-9);
				break;										
			case 16:
				S_load_sec_label(modal,param);
				break;
			case 17:
				S_load_start_unmatch(modal,param);
				break;
			case 18:
				S_load_reg_match(modal,param);
				break;
			default:
				status = UU_FAILURE;
		}
		break;
	}
	return(status);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_max_lab(modal,param)
**			Initializes the variable for the max label size to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by OFF, or an integer from 1-6
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the color seting variable
**							UIG_max_lab
**    WARNINGS     : none	
*********************************************************************/
static int S_load_max_lab(modal, param)
char modal[81], param[81];
{
	UIG_max_lab=atoi(param);
	if(UIG_max_lab<0)UIG_max_lab =0;
	if(UIG_max_lab>6)UIG_max_lab =6;
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  S_load_name_data(modal,param)
**       Processes name modal configuration information from STEP 
**       modal file.  Two strings are brought in, read, and then
**       the global name modals structure UM_labelmdl is modified.
**       The modal part tell the type of geometry to modify (ie. POINTS,
**       etc...), while the param is a string that has two pieces
**       of information:  the first part is the default base name/
**       prefix to use when automatically generating labels, and the
**       second part is *YES or *NO.  If *YES, use subscripts.  For
**       example (PT(1), PT(2), PT(3), ...).  The PT is the base name 
**       while the (1), (2), (3), ... are the automatically generated
**       subscripts.  If the second part is *NO, use prefixes.  For
**       example, (PT1, PT2, PT3, ...).  The PT is the prefix while
**       the postfix numbers 1, 2, 3, ... are automatically generated.
**       
**       This is just to do automatic label generation when translating
**       from STEP to Unibase.
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of geometry type
**          param  =  a string containing abbr. to be used for geometry,
**                    a comma, and then a '*' followed by YES or NO.
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies global name modals data structure
**							UM_labelmdl 
**    WARNINGS     : none
**		AUTHOR		 :	Ed Ames  29 Jan 01
*********************************************************************/
static int S_load_name_data(modal, param)
char modal[81], param[81];
{
	int i;
	size_t comma_len, star_len;
	char abbr[8], yes_no[4];
	static char *geo_list[NREL] = {"POINTS", "PNTVECS", "LINES", "VECTORS",
		"PLANES","CIRCLES", "CURVES","SURFACES", "SHAPES","MATRICES", "PATTERNS",
		"SOLIDS"};

	strcpy(abbr, "");  strcpy(yes_no, "");	
	comma_len = strcspn(param, ",");
	star_len = strcspn(param, "*");
	strncpy(abbr, param, comma_len);
/*
.....strncpy will not have end marker
.....we have to mark ourselvies, otherwise
.....it will give a junk (mostcase it will be '\0'
.....but not all cases, especially when run release version)
.....Yurong
*/
	abbr[comma_len] = '\0';
	sscanf((param+star_len+1), "%s", yes_no);
/* 
..... Plus 1 to skip over the comma 
*/
	ul_to_upper(modal);
	for (i = 0; i < NREL; i++)
	{
		if (strcmp(modal, geo_list[i]) == 0)
			break;
	}
/*
.....Make sure the modal was found before assigning a label - ASF 1/14/14.
*/
	if (i >= NREL) return UU_SUCCESS;
	strcpy(geo_lab[i], abbr);
	
	ul_to_upper(yes_no);
	if (strcmp(yes_no, "YES") == 0)
		lab_flag[i] = 1;
	else
		lab_flag[i] = 0;

	return UU_SUCCESS;
}

/*********************************************************************
**		I_FUNCTION     :  S_load_no_dups(modal,param)
**			Initializes the variable for duplicates according to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES,
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies variables for duplicates
**							UIG_nodups
**    WARNINGS     : none
**		AUTHOR		 : Himani	
*********************************************************************/
static int S_load_no_dups(modal, param)
char modal[81], param[81];
{
	int i;
	if (ul_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	if(i == 1) UIG_nodups =UU_FALSE;
		else
   		UIG_nodups = UU_TRUE;
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  S_load_reg_match(modal,param)
**       Initializes the variable for regressive matching according to the 
**			mod file
**
**    PARAMETERS
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES,
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies 
**                   UIG_regressive
**    WARNINGS     : none
**********************************************************************/
static int S_load_reg_match(modal,param)
char modal[81], param[81];
{
   int index;
   if (ul_modal_toggle(param,yesno,2,&index) == UU_SUCCESS)
      if(index == 0) UIG_regressive = 0;
      else
         UIG_regressive = 1;
   return (0);
}

/*********************************************************************
**    I_FUNCTION     :  S_load_sec_label(modal,param)
**       Initializes the variable for creating unmacthed entities 
**			from the secondary unibase
**
**    PARAMETERS
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES,
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies 
**                   UIG_sec_label
**    WARNINGS     : none
**********************************************************************/
static int S_load_sec_label(modal, param)
char modal[81], param[81];
{
   int index;
   if (ul_modal_toggle(param,yesno,2,&index) == UU_SUCCESS)
      if(index == 0) UIG_unmatch_sec = 0;
      else
         UIG_unmatch_sec = 1;
   return (0);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_shade_surf(modal,param)
**			It initializes the value of the shading variable 
**			according to the ncliges.mod file. 
**		
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies surface shading global variable
**							shade_set
**    WARNINGS     : none
**		AUTHOR		 : Himani	
*********************************************************************/
int S_load_shade_surf(modal, param)
char modal[81], param[81];
{	
	int i;
	if (ul_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	if(i == 0) shade_set = UU_FALSE;
		else
			shade_set = UU_TRUE;
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_srf_crv(modal,param)
**			Initializes the variable for the surface curve according to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by CURVE,COMP or BOTH
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the color seting variable
**							UIG_splitccrv
**    WARNINGS     : none	
*********************************************************************/
static int S_load_srf_crv(modal, param)
char modal[81], param[81];
{
	int i;
	static char srfcrv[3][64] = {"*CURVE","*COMP","*BOTH"};

	if (ul_modal_toggle(param,srfcrv,3,&i) == UU_SUCCESS)
		UIG_splitccrv =i;
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  S_load_start_unmatch(modal,param)
**       Initializes the variable for starting the labels for
**			unmacthed entities.
**
**    PARAMETERS
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by NEXT or SECONDRY
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies 
**                   UIG_start_unmatch
**    WARNINGS     : none
**********************************************************************/
static int S_load_start_unmatch(modal, param)
char modal[81], param[81];
{
   int index;
	static char nextsec[2][64] = {"*NEXT","*SECONDRY"};
   if (ul_modal_toggle(param,nextsec,2,&index) == UU_SUCCESS)
      if(index == 0) UIG_start_unmatch = 0;
      else
         UIG_start_unmatch = 1;
   return (0);
}

/*********************************************************************
**		I_FUNCTION     :  S_load_subscript(modal,param)
**			Initializes the variable for the subscript to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES or NO
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies the color seting variable
**							UIG_use_sub
**    WARNINGS     : none	
*********************************************************************/
static int S_load_subscript(modal,param)
char modal[81], param[81];
{
	int i;
	if (ul_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	utp_set_label_method(-1,i,-1);
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  S_set_layer_opt(modal,param)
**			Initializes the value of the unique layer variable 
**			according to the nclstep.mod file. 
**		
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies surface shading global variable
**							shade_set
**    WARNINGS     : none
**		AUTHOR		 : Himani	
*********************************************************************/
int S_set_layer_opt(modal, param)
char modal[81], param[81];
{
	int j,flag;
	static char *types[]={"ON_LAYERS","LAYER_START"};
/*
.....Set the modals values
*/
	for (j=0;j<2;j++)
	{
		if (strcmp(modal,types[j]) == 0)
		{
			switch (j)
			{
/*
......CURVES
*/
			case 0:
				if (ul_modal_toggle(param,yesno,2,&flag) == UU_SUCCESS)
					utp_set_layer_flag(flag);
				break;
/*
......COMPONENTS
*/
			case 1:
				flag = atoi(param);
				utp_set_start_layer(flag);
				break;
			}
			break;
		}
	}
	return(UU_SUCCESS);
}
/*********************************************************************
**		I_FUNCTION     :  uig_load_lucency(param)
**			It initializes the value of the translucency variable 
**			according to the ncliges.mod file. 
**		
**    PARAMETERS   
**       INPUT  :
**          param  =  translucency value
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	None
**    WARNINGS     : none
**		
*********************************************************************/
void S_set_lucency(param)
char param[81];
{	
	int i;
	UIG_lucency = atoi(param);
}
