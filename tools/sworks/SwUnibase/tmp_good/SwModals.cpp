/********************************************************************
**    NAME         :  SwModals.cpp
**
**       Functions to read in and set global information for NCL/Solid.
**       The information is read in from a modal file pointed to by
**       an environmental variable UL_SOLID_MODALS.  It is read in when
**       NCL/Soild first starts.
**
**    CONTAINS:
**       uk_modal_config()
**			label_options(modal, param)
**			load_bound_crvs(modal, param)
**       load_name_data(modal, param)
**			load_shade_surf(modal, param)
**			load_cv_option(modal,param)
**			load_level_no(modal, cmsg)
**			load_match_layer(modal, param, j)
**			load_match_color(modal, param, j)
**			load_sec_label(modal,param)
**			load_reg_match( modal,param)
**			load_start_unmatch(modal,param)
**		 	open_mod_file(env_dir,fname, fptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			SwModals.cpp , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**			09/11/13 , 13:07:16
*********************************************************************/
#include <SwStdafx.h>
#include "SwModals.h"
#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include "riddldef.h"
#include "nclxunib.h"

extern "C" NCLX_sw_options gl_sw_opt;
extern "C" int UIG_color_sec;
extern "C" int UIG_layer_sec;
extern "C" int UIG_unmatch_sec;
extern "C" int UIG_start_unmatch;
extern "C" int UIG_regressive;
static char lab_uni[2][64] = {"*PRIMARY","*SECONDRY"};
static char crv_opt[3][64] = {"*SEPARATE","*COMPOS", "*NODUPS"};
static char yesno[2][64] = {"*NO","*YES"};
static char ccolor[16][64] = {"*AUTO","*WHITE","*BLUE","*RED","*GREEN",
	"*MAGENTA","*YELLOW","*CYAN","*BROWN","*TAN","*LTBLUE","*SEAGREEN",
	"*ORANGE","*PINK","*PURPLE","*GREY"};
static char level[5][64] = {"*EXACT","*LEVEL_1","*LEVEL_2","*LEVEL_3","*LEVEL_4"		};
static char nextsec[2][64] = {"*NEXT","*SECONDRY"};
NCLX_sw_options gl_sw_opt;
static int first =0;

extern "C" int ul_fread (FILE *,char[],int,int *);
extern "C" int ul_modal_check (char *, int *, char[],char[]);
extern "C" int ux_fclose0(FILE *);
extern "C" int ul_modal_toggle(char *, char[][64],int, int *);
extern "C" void ul_to_upper(char *);
extern "C" void ul_strip_blanks(char *, int*);
extern "C" char *ux_getenv(char *, int);
extern "C" int ux_fopen0(char *,char *, FILE **);
extern "C" void ud_wrerr(char *);
extern "C" void getver(double *);

/*********************************************************************
**    I_FUNCTION     :  uk_modal_config()	
**       Read in NCL_SOLID modal file
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
extern "C" int uk_modal_config()
{
	char serror[UX_MAX_PATH_LEN+40], *str ;
   UX_pathname fullname;
	int i, j, status, type, og_dispatch, open_file, numint, ver_int;
	double ver;
	char str_in[81], modal[81], param[81],dir[81];
	static char *mod_types[] = {"LABELS","ATTRIBUTES",
										 "GEOMETRY_NAME_MODALS",
										 "LABEL_MATCHING",
										 "SKETCHES"};
	static char *attr_types[] = {"BOUND_CRVS","SHADED","CV_OPTION"};
	static char *lab_match[] = {"LEVEL","TOLER","LAYER_EXACT","LAYER_1","LAYER_2","LAYER_3","LAYER_4","LAYER_MATCH","LAYER_IMPORT","COLOR_EXACT","COLOR_1","COLOR_2","COLOR_3","COLOR_4","COLOR_MATCH","COLOR_IMPORT","IMPORT_GEO","START_UNMATCH","REGRESSIVE"};
	FILE *og_config_file;
	og_dispatch = 0;
/*
.....get form varables from the mod file only during initialization.
*/

	if(first)return UU_SUCCESS;
	first++;
/*
.....Initialize form fields
*/
	gl_sw_opt.toler =0.001;
	gl_sw_opt.label_opts=0;
	gl_sw_opt.import_curves=0;
	gl_sw_opt.shade_surfs=1;
	gl_sw_opt.import_sketch=0;
	gl_sw_opt.cvopt = 0;
	strcpy(gl_sw_opt.cvlab,"CV");
	gl_sw_opt.cvlab_type=0;
	strcpy(gl_sw_opt.sflab,"SF");
	gl_sw_opt.sflab_type=0;
	gl_sw_opt.exact_match =1;
	strcpy(gl_sw_opt.ptlab,"PT");
	gl_sw_opt.ptlab_type = 0;
	strcpy(gl_sw_opt.lnlab,"LN");
	gl_sw_opt.lnlab_type = 0;
	strcpy(gl_sw_opt.cilab,"CI");
	gl_sw_opt.cilab_type = 0;
	strcpy(gl_sw_opt.kvlab,"KV");
	gl_sw_opt.kvlab_type = 0;
	for(j=0;j<7;j++)
	{
		gl_sw_opt.layer[j] =j+1;
		gl_sw_opt.color[j] =0;
	}
/*
..... Open configuration file.
*/
	getver(&ver);
	ver_int = ver*10 + .5;
	sprintf(dir,"NCL%d_INIT_FILES",ver_int);
   str = ux_getenv (dir,UX_NPRTERRS);
   if (str == 0) return UU_FAILURE;
/*
.....Open the modals file
*/
   sprintf(fullname,"%s\\ncl_solid.mod",str);
	og_config_file = fopen(fullname,"r");
/*
.....Replacing ux_fopen0 with fopen to avoid memory problems caused by 
.....to uninitialized tables.
.....status = ux_fopen0 (fullname,"r",&og_config_file);
*/
   if (og_config_file == UU_NULL)
   {
      sprintf (serror,"Cannot open Modals file %s\\ncl_solid.mod",str);
      ud_wrerr (serror);
		return UU_FAILURE;
	}
	else
		open_file = 1;
	
	if (open_file == 1)
	{      
		while (feof(og_config_file) == 0)
		{
			status = ul_fread (og_config_file, str_in, 80, &numint);
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
..... If section line, find out the section and set og_dispatch.
..... og_dispatch remembers what section you are in while looping
..... through all the lines in that section.  It will allow the modal 
..... lines to be sent to the correct function to handle them.
..... If modal line, use og_dispatch to determine which function
..... to use to process the line.
..... Use status = UU_FAILURE to break loop; also can use 
..... og_dispatch = -1 to get out of loop.
*/	
			switch(type)
			{
				case 0:		/* Comment line */
					break;
				case 1:		/* Section line */
					og_dispatch = -1;
					status = UU_FAILURE;
/*	
..... status = UU_FAILURE breaks while loop
..... If match is found, reset status = UU_SUCCESS. 
*/
					for (i = 0; i < 5; i++)
					{
						if (strcmp(modal, mod_types[i]) == 0)
						{
							og_dispatch = i+1;
							status = UU_SUCCESS;
							break;
						}
					}
					break;
				case 2:		/* Actual modal data line to process. */
					switch (og_dispatch)
					{
/*
.....#LABELS#
*/
						case 1:
							mod_this->label_options(modal, param);
							break;
/*
.....#ATTRIBUTES#
*/
						case 2:
							for(j=0;j<3;j++)
							{
								if (strcmp(modal, attr_types[j]) == 0)
								{
									if(j == 0)
   										mod_this->load_bound_crvs(modal,param);
									if(j == 1)
											mod_this->load_shade_surf(modal,param);
									if(j == 2)
   										mod_this->load_cv_option(modal,param);
									break;
								}
							}
							break; 
/*
.....#GEOMETRY_NAME_MODALS#
*/
                 case 3:
                    mod_this->load_name_data(modal, param);
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
											mod_this->load_level_no(modal,param);
											break;
										case 1:
											gl_sw_opt.toler = atof(param);
											break;
										case 2:
										case 3:
										case 4:
										case 5:
										case 6:
										case 7:
										case 8:
											mod_this->load_match_layer(modal,param,j-2);
											break;
										case 9:
										case 10:
										case 11:
										case 12:
										case 13:
										case 14:
										case 15:
											mod_this->load_match_color(modal,param,j-9);
											break;										
										case 16:
											mod_this->load_sec_label(modal,param);
											break;
										case 17:
											mod_this->load_start_unmatch(modal,param);
											break;
										case 18:
											mod_this->load_reg_match(modal,param);
											break;
										default:
											status = UU_FAILURE;
									}
									break;
								}
							break;
/*
.....#SKETCHES#
*/
                 case 5:
                    mod_this->load_sketch_data(modal, param);
                    break;
						default:           /* status = UU_FAILURE breaks while loop */
							status = UU_FAILURE;
					}
					break;
				default:				/* satatus = UU_FAILURE breaks while loop*/
					status = UU_FAILURE;
			}

			if (og_dispatch == -1)
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
/*
.....Replacing ux_fclose0 with fclose to avoid memory problems caused by 
.....to uninitialized tables.
.....status += ux_fclose0(og_config_file);
*/
		fclose(og_config_file);
	return UU_SUCCESS;
}


/*********************************************************************
**    I_FUNCTION     :  label_options(modal,param)
**       It initializes the value of the label options variable
**       according to the ncl_solid.mod file.
**
**    PARAMETERS
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by PRIMARY or 
**							SECONDARY
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies label options in global variable
**                   gl_sw_opt
**    WARNINGS     : none
*********************************************************************/
int CMod::label_options(char modal[81],char param[81])
{
   int i;
   if (ul_modal_toggle(param,lab_uni,2,&i) == UU_SUCCESS)
			gl_sw_opt.label_opts = i;
   return (0);
}

/*********************************************************************
**    I_FUNCTION     :  load_bound_crvs(modal,param)
**       It initializes the value of the curves variable
**       according to the ncl_solid.mod file.
**
**    PARAMETERS
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES or NO
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies import_curves in global variable
**                   gl_sw_opt
**    WARNINGS     : none
*********************************************************************/
int CMod:: load_bound_crvs(char modal[81],char param[81])
{
   int i;
   if (ul_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
         gl_sw_opt.import_curves = i;
   return (0);
}
/*********************************************************************
**    I_FUNCTION     :  load_shade_surf(modal,param)
**       It initializes the value of the label options variable
**       according to the ncl_solid.mod file.
**
**    PARAMETERS
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES or NO
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies sahde_surfs in global variable
**                   gl_sw_opt
**    WARNINGS     : none
*********************************************************************/
int CMod::load_shade_surf(char modal[81],char param[81])
{
   int i;
   if (ul_modal_toggle(param,yesno,2,&i) == UU_SUCCESS)
         gl_sw_opt.shade_surfs = i;
   return (0);
}

/*********************************************************************
**    I_FUNCTION     :  load_cv_option(modal,param)
**       It initializes the value of the curve options variable
**       according to the ncl_solid.mod file.
**
**    PARAMETERS
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by SEPARATE or 
**							COMPOSITE or NODUPS.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies label options in global variable
**                   gl_sw_opt
**    WARNINGS     : none
*********************************************************************/
int CMod::load_cv_option(char modal[81],char param[81])
{
   int i;
   if (ul_modal_toggle(param,crv_opt,3,&i) == UU_SUCCESS)
         gl_sw_opt.cvopt = i;
   return (0);
}
/*********************************************************************
**    I_FUNCTION     :  load_name_data(modal,param)
**       Processes name modal configuration information from SOLID 
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
*********************************************************************/
int CMod::load_name_data(char modal[81],char param[81])
{
	int i;
	size_t comma_len, star_len;
	char abbr[8], yes_no[4];
	static char *geo_list[2] = 
								{"CURVES","SURFACES"};
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
	for (i = 0; i < 2; i++)
	{
		if (strcmp(modal, geo_list[i]) == 0)
			break;
	}
	if(!i)
	{
		strcpy(gl_sw_opt.cvlab, abbr);
		ul_to_upper(yes_no);
		if (strcmp(yes_no, "YES") == 0)
			gl_sw_opt.cvlab_type = 1;
		else
			gl_sw_opt.cvlab_type = 0;
	}
	else
	{
		strcpy(gl_sw_opt.sflab, abbr);
      ul_to_upper(yes_no);
      if (strcmp(yes_no, "YES") == 0)
			gl_sw_opt.sflab_type = 1;
      else
			gl_sw_opt.sflab_type = 0;
   }

	return UU_SUCCESS;
}

/*********************************************************************
**		I_FUNCTION     :  load_level_no(modal,cmsg)
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
**    WARNINGS     : none
*********************************************************************/
int CMod::load_level_no(char modal[81],char cmsg[81])
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
				gl_sw_opt.exact_match = ind;	
			c = p+1;
			i++;
		} while (p != UU_NULL);
	return (0);
}

/*********************************************************************
**		I_FUNCTION     :  load_match_layer(modal,param,j)
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
**    WARNINGS     : none
*********************************************************************/
int CMod::load_match_layer(char modal[81],char param[81], int j)
{
	if(j!=6)
		gl_sw_opt.layer[j] = atoi(param);
	else
		UIG_layer_sec = atoi(param);
		
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  load_match_color(modal,param,j)
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
**    WARNINGS     : none
*********************************************************************/
int CMod::load_match_color(char modal[81],char param[81], int j)
{
	int index;
   if (ul_modal_toggle(param,ccolor,16,&index) == UU_SUCCESS)	
 	{
		if(index < 0)
			index = 0;
		if( j!=6 )
			gl_sw_opt.color[j] = index;
		else 
			UIG_color_sec = index;
	}
  return (0);
}

/*********************************************************************
**    I_FUNCTION     :  load_sec_label(modal,param)
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
**    WARNINGS     : none
**********************************************************************/
int CMod::load_sec_label(char modal[81],char param[81])
{
   int index;
   if (ul_modal_toggle(param,yesno,2,&index) == UU_SUCCESS)
      if(index == 0) UIG_unmatch_sec = 0;
      else
         UIG_unmatch_sec = 1;
   return (0);
}
/*********************************************************************
**    I_FUNCTION     :  load_start_unmatch(modal,param)
**       Initializes the variable for starting the labels for
**			unmacthed entities.
**
**    PARAMETERS
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by NEXT,SECONDRY
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies 
**					UIG_start_unmatch
**    WARNINGS     : none
**********************************************************************/
int CMod::load_start_unmatch(char modal[81],char param[81])
{
   int index;
   if (ul_modal_toggle(param,nextsec,2,&index) == UU_SUCCESS)
      if(index == 0) UIG_start_unmatch = 0;
      else
         UIG_start_unmatch = 1;
   return (0);
}
/*********************************************************************
**    I_FUNCTION     :  load_reg_match(modal,param)
**       Initializes the variable for regressive matching according to the 
**			mod file
**    PARAMETERS
**       INPUT  :
**          modal  =  name of attribute
**          param  =  a string containing ,'*' followed by YES,NO
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : modifies 
**					 UIG_regressive
**    WARNINGS     : none
**********************************************************************/
int CMod::load_reg_match(char modal[81],char param[81])
{
   int index;
   if (ul_modal_toggle(param,yesno,2,&index) == UU_SUCCESS)
      if(index == 0) UIG_regressive = 0;
      else
         UIG_regressive = 1;
   return (0);
}

/*********************************************************************
**    I_FUNCTION     :  load_sketch_data(modal,param)
**       Processes sketch modal configuration information from SOLID 
**       modal file.
**
**    PARAMETERS   
**       INPUT  :
**          modal  =  name of geometry type
**          param  =  a string containing abbr. to be used for geometry,
**                    a comma, and then a '*' followed by YES or NO.
**       OUTPUT : 
**          none 
**    RETURNS      :	UU_SUCCESS 
**    SIDE EFFECTS :	none
**    WARNINGS     : none
*********************************************************************/
int CMod::load_sketch_data(char modal[81],char param[81])
{
	int i,type;
	size_t comma_len, star_len;
	char abbr[8], yes_no[4];
	static char *geo_list[5] = 
		{"EXPORT","POINTS","LINES","ARCS","CURVES"};
/*
.....Get modal to define
*/
	ul_to_upper(modal);
	for (i=0; i<5; i++)
	{
		if (strcmp(modal, geo_list[i]) == 0)
			break;
	}
/*
.....EXPORT
*/
	if (i == 0)
	{
		ul_to_upper(param);
		if (strcmp(param,"*YES") == 0) gl_sw_opt.import_sketch = 1;
		else if (strcmp(param,"*NO") == 0) gl_sw_opt.import_sketch = 0;
	}
/*
.....Label options
*/
	else
	{
		strcpy(abbr, "");  strcpy(yes_no, "");	
		comma_len = strcspn(param, ",");
		star_len = strcspn(param, "*");
		strncpy(abbr, param, comma_len);
		abbr[comma_len] = '\0';
		sscanf((param+star_len+1), "%s", yes_no);
		type = 0;
		if (strcmp(yes_no, "YES") == 0) type = 1;
/* 
..... Plus 1 to skip over the comma 
*/
		switch (i)
		{
		case 1:
			strcpy(gl_sw_opt.ptlab, abbr);
			gl_sw_opt.ptlab_type = type;
			break;
		case 2:
			strcpy(gl_sw_opt.lnlab, abbr);
			gl_sw_opt.lnlab_type = type;
			break;
		case 3:
			strcpy(gl_sw_opt.cilab, abbr);
			gl_sw_opt.cilab_type = type;
			break;
		case 4:
			strcpy(gl_sw_opt.kvlab, abbr);
			gl_sw_opt.kvlab_type = type;
			break;
		}
	}
	return UU_SUCCESS;
}

/*********************************************************************
**	 E_FUNCTION : open_mod_file(env_dir,fname, fptr)
**		Gets initialization/mod dir name from the environmental variable
**		env_dir and the file name form the fname.  Then open the file for 
**		reading and returns the file
**		stream/handle.
**	 PARAMETERS	
**		 INPUT  : env_dir	=	string containing the environmental variable
**									that tells the location of the initialization
**					 fname	=	string containign the filename. 
**		 OUTPUT :  fptr	=	pointer/handle to the initialization/mod
**									file.  The file is open for reading
**	 RETURNS:	UU_SUCCESS or UU_FAILURE 
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
/*int CMod::open_mod_file(char *env_dir,char *file, FILE **fptr)
{
	char serror[UX_MAX_PATH_LEN+40], *str ;
	int status;
	UX_pathname fullname;
/
.....Check for modals file
/
	str = ux_getenv (env_dir,UX_NPRTERRS);
	if (str == 0) return UU_FAILURE;
/
.....Open the modals file
/
	sprintf(fullname,"%s\\%s",str,file);
	status = ux_fopen0 (fullname,"r",fptr);
	if (status != UU_SUCCESS)
	{
		sprintf (serror,"Cannot open Modals file %s\\%s",str,file);
		ud_wrerr (serror);
		return UU_FAILURE;
	}
	
	return UU_SUCCESS;
}*/
