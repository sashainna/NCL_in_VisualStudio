/********************************************************************
**    NAME         :  tigmodals.c
**
**       Functions to read in and set global information for NCLIGES.
**       The information is read in from a modal file pointed to by
**       an environmental variable UL_IGES_MODALS.  It is read in when
**       NCLIGES first starts.  Other functions process this information
**       and set global data structures such as naming conventions
**       (subscripts or postfixs and the base name -> this is used
**       during automatic name generation when going from .igs to
**       unibase), geometry filtering (selectively translate geometry
**       types: for example, translate points but not vectors), and
**       shading.
**
**    CONTAINS:
**       uig_modal_config()
**       uig_load_name_data(modal, param)
**       uig_load_filter_flags(modal, param)
**			uig_load_shade_surf(modal, param)
**			uig_load_init_label(modal, param)
**			uig_load_no_dups(modal, param)
**			uig_load_color(modal, param)
**			uig_load_level_no(modal, cmsg)
**			uig_load_match_layer(modal, param, j)
**			uig_modal_toggle(cmsg,toggle,max,var)
**			uig_load_match_color(modal, param, j)
**			uig_strip_blanks (str,size)
**			uig_load_sec_label(modal,param)
**			uig_load_start_unmatch(modal,param)
**			uig_load_match_tol(modal,param)
**			uig_load_reg_match(modal,param)
**			uig_load_color_mode
**			tig_load_clrfile
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**		  tigmodals.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**		  10/27/16 , 14:53:45
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
#if (UU_COMP!=UU_WIN2K)
#include "tigmf.h"
#endif
#include "msrf.h"
#include "tigglobal.h"


/*
......Motif version defined those variables in igesmfmain.c
......and there is no C file only for Win2K, so no where define
......those variables, so define here
......Yurong
*/
extern int iges_st_type[IG_NUM+1];
int UIG_use_sub;
int UIG_concat_sub;
int UIG_decompose = 0;
extern int UIG_decompose_chc;

static char nextsec[2][64] = {"*NEXT","*SECONDRY"};
static char yesno[2][64] = {"*NO","*YES"};
static char color[2][64] = {"*NCL","*IGES"};

static char scolor[2][96] = {"Default", "Auto"};
static char level[5][64] = {"*EXACT","*LEVEL_1","*LEVEL_2","*LEVEL_3","*LEVEL_4"		};
static char alllayer[2][64] = {"*ALL","*LAYERS"};
static char srfcrv[3][64] = {"*CURVE","*COMP","*BOTH"};
static char labopt[4][64] = {"*IGES","*LABEL","*PROPERTY","*UNIBASE"};
extern char NCL_init_fstr[20];
static int S_cus_color = -1;
void uig_load_lucency(char*);
/*********************************************************************
**    I_FUNCTION     :  uig_modal_config()	
**       Read in NCLIGES modal file and setup global information such
**       as geometry names and subscript/postfix numbering flags (used
**       when NCLIGES automatically generates labels for geometry
**       translated from .igs to unibase), geometry filtering (only
**       translate certain types of geometry while ignoring others:
**       for example, translate points but not vectors), and shading
**       flags.
**       This modal file is pointed to by an environmental variable
**       UL_IGES_MODALS.  The file should be read only once, when NCLIGES
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
**		AUTHOR		 :	Ed Ames	3 Jan 01
*********************************************************************/
int uig_modal_config()
{
	int i, j, status, type, iges_dispatch, open_file, numint;
	char str_in[81], modal[81], param[81];
	static char *mod_types[] = {"GEOMETRY_NAME_MODALS","ENTITY_FILTER_MASK",
									"ATTRIBUTES","LABEL_MATCHING", "INCLUDE"};
	static char *attr_types[] = {"SHADED","INIT_LABELS","DUPLICATES","COLORS",
					"CONVERSION","SURF_CURVE","LABELS","MAX_LABEL","SUBSCRIPTS",
					"CONCATENATE", "EDGE_DISPLAY", "EDGE_COLOR", "FILE","DECOMPOSE",
					"TRANSLUCENCY", "DECO_DUP"};
	static char *lab_match[] = {"LEVEL","TOLER","LAYER_EXACT","LAYER_1","LAYER_2","LAYER_3","LAYER_4","LAYER_MATCH","LAYER_IMPORT","COLOR_EXACT","COLOR_1","COLOR_2","COLOR_3","COLOR_4","COLOR_MATCH","COLOR_IMPORT","IMPORT_GEO","START_UNMATCH","REGRESSIVE"};
	FILE *iges_config_file;
	char *str,*ux_getenv();
	UU_LOGICAL found;
	UX_pathname fullname;
	char *pathlistptr = UU_NULL;
	
	iges_dispatch = 0;
/*
..... Open IGES configuration file.
*/
/*	status = ul_open_mod_file("UL_IGES_MODALS", &iges_config_file);  */
	str = ux_getenv("UL_IGES_MODALS", UX_NPRTERRS);
	if (str != 0) 
	{
		ux_get_syspath("UL_IGES_MODALS", &pathlistptr, fullname, 
			&found, UX_NPRTERRS|UX_NCHK|UX_NQUOTES);
		uu_lsdel(pathlistptr);
/*
.....See if file exists
*/
		status = ul_open_mod_file("UU_USER_SETTINGS", "init", NCL_init_fstr, UU_NULL,
			fullname, 2, &iges_config_file);
		if ((status != UU_SUCCESS)||(iges_config_file==UU_NULL))
		{
/*			sprintf (serror,"Cannot open model definition file %s",sfname);
			ud_wrerr (serror); */
			status = UU_FAILURE;
		}
	}
	else
		status = UU_FAILURE;


	if (status != UU_SUCCESS)
	{
		open_file = 0;
		uig_change_labelmdl(UU_FALSE);	
	}
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
					for (i = 0; i < 5; i++)
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
							uig_load_name_data(modal, param);
							break;
						case 2:
							uig_load_filter_flags(modal, param);
							break;
/*
.....#ATTRIBUTES#
.......Added the option to decompose symbols - ASF 1/7/14.
*/
						case 3:
							for(j=0;j<16;j++)
							{
								if (strcmp(modal, attr_types[j]) == 0)
								{
									if(j == 0)
											uig_load_shade_surf(modal,param);
									if(j == 1)
   										uig_load_init_label(modal,param);
									if(j == 2)
   										uig_load_no_dups(modal,param);
									if(j ==3)
   										uig_load_color(modal,param);
									if(j ==4)
   										uig_load_conv_opt(modal,param);
									if(j ==5)
   										uig_load_srf_crv(modal,param);
									if(j ==6)
   										uig_load_label_opt(modal,param);
									if(j ==7)
   										uig_load_max_lab(modal,param);
									if(j ==8)
   										uig_load_subscript(modal,param);
									if(j ==9)
   										uig_load_con_subscrpt(modal,param);
									if(j ==10)
   										uig_load_edge_disp(modal,param);
									if(j ==11)
   										uig_load_edge_color(modal,param);
									if(j ==12)
   										uig_load_color_mode(param);
									if(j ==13)
   										uig_load_decompose(param);
									if(j ==14)
   										uig_load_lucency(param);
									if(j ==15)
   										uig_load_deco_dup(param);
									break;
								}
							}
							break; 
/*
.....#LABEL_MATCHING#
*/
						case 4:
							for(j=0;j<19;j++)
								if(strcmp(modal, lab_match[j]) == 0)
								{
									switch(j)
									{
										case 0:
											uig_load_level_no(modal,param);
											break;
										case 1:
											uig_load_match_tol(modal,param);
											break;
										case 2:
										case 3:
										case 4:
										case 5:
										case 6:
										case 7:
										case 8:
											uig_load_match_layer(modal,param,j-2);
											break;
										case 9:
										case 10:
										case 11:
										case 12:
										case 13:
										case 14:
										case 15:
											uig_load_match_color(modal,param,j-9);
											break;										
										case 16:
											uig_load_sec_label(modal,param);
											break;
										case 17:
											uig_load_start_unmatch(modal,param);
											break;
										case 18:
											uig_load_reg_match(modal,param);
											break;
										default:
											status = UU_FAILURE;
									}
									break;
								}
							break;
/*
.....#INCLUDE#
*/
						case 5:
							if (strcmp(modal, "FILE") == 0)
   								uig_load_color_mode(param);
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
**    I_FUNCTION     :  uig_load_name_data(modal,param)
**       Processes name modal configuration information from IGES 
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
**       from IGES to Unibase.
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
int uig_load_name_data(modal, param)
char modal[81], param[81];
{
	int i;
	size_t comma_len, star_len;
	char abbr[8], yes_no[4];
	static char *geo_list[11] = {"POINTS", "PNTVECS", "LINES", "VECTORS",
										"PLANES","CIRCLES", "CURVES","SURFACES",
										"SHAPES","MATRICES", "PATTERNS"};

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
	for (i = 0; i < 11; i++)
	{
		if (strcmp(modal, geo_list[i]) == 0)
			break;
	}
	strcpy(geo_lab[i], abbr);
	
	ul_to_upper(yes_no);
	if (strcmp(yes_no, "YES") == 0)
		lab_flag[i] = 1;
	else
		lab_flag[i] = 0;

	return UU_SUCCESS;
}

/*********************************************************************
**    I_FUNCTION     :  uig_load_filter_flags(modal, param)	
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      :	UU_SUCCESS or UU_FAILURE 
**    SIDE EFFECTS :	none	
**    WARNINGS     : none
**		AUTHOR		 :	Ed Ames  2 Feb 01
*********************************************************************/
int uig_load_filter_flags(modal, param)
char modal[81], param[81];
{
	int i, flag, rel_num;
	size_t star_len;
	char yes_no[4]; 

	rel_num	= atoi(modal);
	star_len = strcspn(param, "*");
	strcpy(yes_no,"");
	sscanf((param+star_len+1), "%s", yes_no);
/* 
..... Plus 1 to skip over the comma 
*/
	ul_to_upper(yes_no);
	if (strcmp(yes_no, "YES") == 0)
		flag = 1;
	else if (strcmp(yes_no, "NO") == 0)
		flag = 0;
	else
		flag = 1;
/*
..... Flag = 1 if you want to translate that type of entity.
..... Flag = 0 if you want to filter out those sort of entities.
*/
	for (i = 0; i < 37; i++)
	{
		if (rel_num == entity_ref[i])
		{
			entity_mask[i] = flag;
			break;
		}
	}	/* Got correct position in entity_mask and set flag */	

	return UU_SUCCESS;
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_shade_surf(modal,param)
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
int uig_load_shade_surf(modal, param)
char modal[81], param[81];
{	
	int i;
	if (uig_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	if(i == 0) shade_set = UU_FALSE;
		else
			shade_set = UU_TRUE;
	return (0);
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
void uig_load_lucency(param)
char param[81];
{	
	int i;
	UIG_lucency = atoi(param);
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_init_label(modal,param)
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
int uig_load_init_label(modal, param)
char modal[81], param[81];
{
	int i;
	if (uig_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	if(i == 0) UIG_reinit_lab = UU_FALSE;
   	else
      	UIG_reinit_lab = UU_TRUE;
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_no_dups(modal,param)
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
int uig_load_no_dups(modal, param)
char modal[81], param[81];
{
	int i;
	if (uig_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	if(i == 1) 
		UIG_nodups =UU_FALSE;
	else
   		UIG_nodups = UU_TRUE;
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_color(modal,param)
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
int uig_load_color(modal, param)
char modal[81], param[81];
{
	int i;
	if (uig_modal_toggle(param,color,2,&i) == UU_SUCCESS)
   	if(i == 0) UIG_color_iges = UU_FALSE;
   	else
      	UIG_color_iges = UU_TRUE;
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_conv_opt(modal,param)
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
int uig_load_conv_opt(modal, param)
char modal[81], param[81];
{
	int i;
	if (uig_modal_toggle(param,alllayer,2,&i) == UU_SUCCESS)
		UIG_conv_opt=i;
   	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_srf_crv(modal,param)
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
int uig_load_srf_crv(modal, param)
char modal[81], param[81];
{
	int i;
	if (uig_modal_toggle(param,srfcrv,3,&i) == UU_SUCCESS)
		UIG_splitccrv =i;
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_label_opt(modal,param)
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
int uig_load_label_opt(modal, param)
char modal[81], param[81];
{
	int i;
	if (uig_modal_toggle(param,labopt,4,&i) == UU_SUCCESS)
   	UIG_lab_opt =i ;
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_max_lab(modal,param)
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
int uig_load_max_lab(modal, param)
char modal[81], param[81];
{
	UIG_max_lab=atoi(param);
	if(UIG_max_lab<0)UIG_max_lab =0;
	if(UIG_max_lab>6)UIG_max_lab =6;
	return 0;
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_subscript(modal,param)
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
int uig_load_subscript(modal,param)
char modal[81], param[81];
{
	int i;
	if (uig_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	UIG_use_sub =i ;
	return (0);
}
/*********************************************************************
**		I_FUNCTION     :  uig_load_con_subscrpt(modal,param)
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
int uig_load_con_subscrpt(modal,param)
char modal[81], param[81];
{
	int i;
	if (uig_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	UIG_concat_sub =i ;
	return (0);
}
/*********************************************************************
**		I_FUNCTION     :  uig_load_edge_disp(modal,param)
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
int uig_load_edge_disp(modal,param)
char modal[81], param[81];
{
	int i;
	if (uig_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	UIG_srf_edge =i ;
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_edge_color(modal,param)
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
int uig_load_edge_color(modal,param)
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
}
/*********************************************************************
**		I_FUNCTION     :  uig_load_level_no(modal,cmsg)
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
int uig_load_level_no(modal, cmsg)
char modal[81], cmsg[81];
{
	char *p,*c,sbuf[80];
	int i,n,ind;
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
**		I_FUNCTION     :  uig_load_match_tol(modal,cmsg)
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
int uig_load_match_tol(modal, cmsg)
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
**		I_FUNCTION     :  uig_load_match_layer(modal,param,j)
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
int uig_load_match_layer(modal, param, j)
char modal[81], param[81];
int j;
{
	UIG_match_layer_array[j] = atoi(param);
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  uig_modal_toggle(cmsg,toggle,max,var)
**				This function sets a toggle form entry.
**    PARAMETERS   
**       INPUT  :
**          cmsg  =  Input toggle value.
**          toggle=  Acceptable toggel valeu.
**				max	=	# of acceptable toggles.
**       OUTPUT : 
**          var	=	index of toggle chosen.
**    RETURNS      :	UU_SUCCESS if no problems,UU_FAILURE otherwise. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_modal_toggle(cmsg,toggle,max,var)
char *cmsg,toggle[][64];
int max, *var;
{
	char buf[80];
	int j,status;
/*
.....Assume success
*/
	status = UU_SUCCESS;
/*
.....Convert input to uppercase
*/
	if (*cmsg == '\0') goto done;
	strcpy (buf,cmsg);
	ul_to_upper(buf);
/*
.....Check for recognized toggle value
*/
	for (j=0;j<max;j++)
	{
		ul_to_upper(buf);
		if (strcmp(buf,toggle[j]) == 0) break;
	}
	if (j >= max) goto failed;
	*var = j;
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
**    I_FUNCTION     :  uig_load_match_color(modal,param,j)
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
int uig_load_match_color(modal, param, j)
char modal[81], param[81];
int j;
{
	int index;
		
	if (ul_modal_color(param, &index, scolor, 2)
			!= UU_SUCCESS) return 0;
		
	if ((index<0)&&(index!=-2))
		return 0;
	else if (index<0)
		index = -1;

	UIG_match_color_array[j] = index;
	return (0);
}

/*********************************************************************
**	E_FUNCTION:	uig_strip_blanks (str,size)
**			This function removes any blanks from a text string
**    PARAMETERS   
**       INPUT  :	str = input text string
**       OUTPUT :  
**			str = returned string
**			size =  # of characters returned.
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
 
void uig_strip_blanks (str,size)
char *str;
int *size;
{
	int i,j;
/*
.....Remove all blanks from string
*/
	j = 0;
	for (i=0;i<*size;i++)
	{
		if (str[i] == '\0') break;
		if (str[i] > ' ')
		{
			str[j] = str[i];
			j++;
		}
	}
	*size = j;
	str[j] = '\0';
	return;
}

/*********************************************************************
**    I_FUNCTION     :  uig_load_decompose(param)
**       Initializes the variable for decomposing symbols during
**			translation from the secondary unibase
**
**    PARAMETERS
**       INPUT  :
**          param  =  a string containing ,'*' followed by YES/NO
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies 
**                   UIG_sec_label
**    WARNINGS     : none
**********************************************************************/
int uig_load_decompose(param)
char param[81];
{
   int index;
   if (ul_modal_toggle(param,yesno,2,&index) == UU_SUCCESS)
	{
      if(index == 0) 
		  UIG_decompose = 0;
      else 
		  UIG_decompose = 1;
	}
   return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_load_sec_label(modal,param)
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
int uig_load_sec_label(modal, param)
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
**    I_FUNCTION     :  uig_load_start_unmatch(modal,param)
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
int uig_load_start_unmatch(modal, param)
char modal[81], param[81];
{
   int index;
   if (ul_modal_toggle(param,nextsec,2,&index) == UU_SUCCESS)
      if(index == 0) UIG_start_unmatch = 0;
      else
         UIG_start_unmatch = 1;
   return (0);
}
/*********************************************************************
**    I_FUNCTION     :  uig_load_reg_match(modal,param)
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
int uig_load_reg_match(modal,param)
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
**       I_FUNCTION : S_modals_custom_color(ctyp,cmsg)
**                      This function sets the custom color modals.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                         cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int S_modals_custom_color (ctyp,cmsg)
char *ctyp,*cmsg;
{
	char serror[200];
	int i,status,inum;
	UU_REAL rval[3];
	int maxsub=2;
	static char *csub[] = {"NAME", "RGB"};
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
		S_cus_color++;
		strcpy(uw_color_name[S_cus_color+UIG_STDCOLOR], cmsg);
		break;
/*
.....RGB values
*/
	case 1:
		if ((ul_to_reals(&rval,&inum,3,cmsg) != UU_SUCCESS) || inum != 3)
			goto bad_parm;
		uw_color_table[S_cus_color+UIG_STDCOLOR][0] = (int)rval[0];
		uw_color_table[S_cus_color+UIG_STDCOLOR][1] = (int)rval[1];
		uw_color_table[S_cus_color+UIG_STDCOLOR][2] = (int)rval[2];
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
**		I_FUNCTION     :  uig_load_color_mode(file)
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
int uig_load_color_mode(file)
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
		strcpy(modfile,"ncliges_color.mod");
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
	status = tig_load_clrfile(fptr);
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
**       E_FUNCTION : tig_load_clrfile(fptr)
**           This function loads the color file
**                      from a disk file.
**       PARAMETERS     
**               INPUT  :  fptr: color file to be load.
**               OUTPUT :  none.
**       RETURNS: none
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int tig_load_clrfile(fptr)
FILE *fptr;
{
	char serror[80],buf[80],ctyp[40],cmsg[40];
	int status,stat,numint,ityp,i,isub,istat;
	int maxsub=31;
/*
.....Assume success
*/
	status = UU_SUCCESS;
	S_cus_color = -1;
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
   			S_modals_custom_color(ctyp,cmsg);
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
	if (S_cus_color!=-1)
		ncl_update_colors(0);
	return (status);
}

/*********************************************************************
**		I_FUNCTION     :  uig_load_deco_dup(param)
**			Initializes the variable for Decomposoe duplicates according to the mod file
**
**    PARAMETERS   
**       INPUT  :
**          param  =  a string containing '*' followed by YES,
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	modifies variables for 
**							UIG_decompose_chc
**    WARNINGS     : none
**		AUTHOR		 : Himani	
*********************************************************************/
int uig_load_deco_dup(param)
char param[81];
{
	int i;
	if (uig_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
   	if (i == 0) 
		UIG_decompose_chc = 0;
	else
   		UIG_decompose_chc = 1;
	return 0;
}
