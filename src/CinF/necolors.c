/*********************************************************************
**    NAME         :  necolors.c
**       CONTAINS:  routines to manipulate Color Table.
**         ncl_init_color()
**         ncl_post_load_color()
**         ncl_update_colors()
**         ncl_getclr_inx()
**         ncl_store_clr()
**         ncl_obclr()
**         ncl_load_color()
**    COPYRIGHT 2011 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       necolors.c , 25.2
**     DATE AND TIME OF LAST  MODIFICATION
**       07/05/16 , 12:07:10
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "mpocket.h"
#include "nclfc.h"
#include "nclver.h"
#include "ncolorddl.h"

extern char uw_color_name[64][96];
extern int uw_color_table[64][3];
extern int UR_restore_clr;

/*********************************************************************
**    E_FUNCTION     : ncl_init_color()
**       Initialize the color tuple
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_init_color()
{
	struct NCL_color_rec colors;
	int	i, entnum,status;
	int color_table[16][3] =
	{
		0, 0, 0,				/* Black */
		255,  255, 255, 	/* White */
		0, 0, 255,			/* Dodger Blue */
		255,  0,   0, 		/* Red */
		0,    255, 0,		/* Green */
		255,  0,   255, 	/* Magenta */
		255,  255, 0, 		/* Yellow */
		0,    255, 255,	/* Cyan */
		184,  134, 11, 	/* Dark Goldenrod */
		210,  180, 140,	/* Tan */
		173,  216, 230,	/* Light Blue */
		84,   255, 159,	/* SeaGreen1 */
		255,  165, 0,		/* Orange */
		255,  195, 203,	/* Pink */
		221,  160, 221, 	/* Plum */
		192,  192, 192,	/* Gray */
	};
	char color_name[16][96] =
	{
		"BLACK",
		"WHITE",
		"BLUE",
		"RED",
		"GREEN",
		"MAGNTA",
		"YELLOW",
		"CYAN",
		"BROWN",
		"LTTAN",
		"LTBLUE",
		"SEAGRN",
		"ORANGE",
		"PINK",
		"PURPLE",
		"GREY"
	};

	colors.rel_num = NCL_COLOR_REL;
	status = 0;
	entnum = 0;
	while (status == 0)
	{
		entnum++;
		status = ur_get_next_tuple_index(colors.rel_num, &entnum);
		if (status == 0)
		{
/*
......aready initialize, just return
*/
			return;
		}
	}
	colors.key = 0;
	for (i=0; i<16; i++)
	{
		strcpy(colors.color_name[i], color_name[i]);
		colors.color_value[i][0] = color_table[i][0];
		colors.color_value[i][1] = color_table[i][1];
		colors.color_value[i][2] = color_table[i][2];
	}
	for (i=16; i<64; i++)
	{
		colors.color_name[i][0] = '\0';
		colors.color_value[i][0] = 192;
		colors.color_value[i][1] = 192;
		colors.color_value[i][2] = 192;
		uw_color_name[i][0] = '\0';
		uw_color_table[i][0] = colors.color_value[i][0];
		uw_color_table[i][1] = colors.color_value[i][1];
		uw_color_table[i][2] = colors.color_value[i][2];
	}
	ur_create_tuple(colors.rel_num, &entnum, &colors);
}

/*********************************************************************
**    E_FUNCTION     : ncl_post_load_color()
**       load color attributes into global color value
**
**    PARAMETERS   
**       INPUT  : 
**          loadoperation					UU_TRUE => load
**													UU_FALSE => merge
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_post_load_color(loadoperation)
UU_LOGICAL loadoperation;
{
	int i, status, entnum,len;
	struct NCL_color_rec colors;
	if (NCL_infile_version < 9.850)
	{
		ncl_init_color();
	}
/*
.....get the color data and assign to global color vaue
*/
	entnum = 1;
	colors.rel_num = NCL_COLOR_REL;
	status = ur_get_next_tuple_index(colors.rel_num, &entnum);
	if (status == 0)
	{
		ur_retrieve_tuple(colors.rel_num, entnum, &colors);
		for (i=0; i<64; i++)
		{
/*
......removed trailing spaces
*/
			len = strlen(colors.color_name[i]);
			while ((len>0) &&(colors.color_name[i][len-1]==' ')) len--;
			colors.color_name[i][len] = '\0';

			if (colors.color_name[i][0]!='\0')
			{
				strcpy(uw_color_name[i], colors.color_name[i]);
/*
......SAVE ALL IN UPPER CASE
*/
				ul_to_upper(uw_color_name[i]);
				if (strcmp(uw_color_name[i],"TAN")==0)
					strcpy(uw_color_name[i], "LTTAN");
			}
			else
				uw_color_name[i][0] = '\0';
			uw_color_table[i][0] = colors.color_value[i][0];
			uw_color_table[i][1] = colors.color_value[i][1];
			uw_color_table[i][2] = colors.color_value[i][2];
			UM_pkcolors[i].red = uw_color_table[i][0];
			UM_pkcolors[i].green = uw_color_table[i][1];
			UM_pkcolors[i].blue = uw_color_table[i][2];
		}
	}
	if(loadoperation && UR_restore_clr)
		ul_unibase_clr();
}
/*********************************************************************
**    E_FUNCTION     : ncl_update_colors(flag)
**       updated the color class data in the unibase
**       
**    PARAMETERS   
**       INPUT  : 
**          flag: 1: save updated color into the ncl_color.mod file
**					0: not save
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_update_colors(flag)
int flag;
{
	int i, status, entnum;
	struct NCL_color_rec colors;

	status = 0;
	entnum = 1;
	colors.rel_num = NCL_COLOR_REL;

	status = ur_get_next_tuple_index(colors.rel_num, &entnum);
	if (status == 0)
	{
		ur_delete_tuple_abs(NCL_COLOR_REL, entnum);
		for (i=0; i<64; i++)
		{
			if (uw_color_name[i][0]!='\0')
				strcpy(colors.color_name[i], uw_color_name[i]);
			else
				colors.color_name[i][0] = '\0';
			colors.color_value[i][0] = uw_color_table[i][0];
			colors.color_value[i][1] = uw_color_table[i][1];
			colors.color_value[i][2] = uw_color_table[i][2];
			UM_pkcolors[i].red = uw_color_table[i][0];
			UM_pkcolors[i].green = uw_color_table[i][1];
			UM_pkcolors[i].blue = uw_color_table[i][2];
		}
		entnum = 1;
		ur_create_tuple(colors.rel_num, &entnum, &colors);
	}
	if (flag)
		ncl_save_clrmod();
/*
......we need update NCLIPV color/material too
*/
	ul_ipv_update_colors();
}

/*********************************************************************
**    E_FUNCTION     : ncl_getclr_inx(clrstr, len, isub, clr)
**       get the color index of a color name
**       
**    PARAMETERS   
**       INPUT  : 
**          clrstr: color name string
**			len  length of color name string
**			isub subscript of the color
**       OUTPUT :  
**          clr: color index
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getclr_inx(clrstr, len, isub, clr)
UM_f77_str_ptr clrstr;
UM_int2 *len;
UM_int4 *clr, *isub;
{
	int i, clen;
	char *cstr, name[96];
	cstr = UM_cstr_of_f77_str(clrstr);
	if (*len>=95) clen = 95;
	else
		clen = *len;
	if (clen==0)
	{
		*clr = -1000;
		return;
	}
	strncpy(name, cstr, clen);
	name[clen] = '\0';
	ncl_format_label(name, *isub, name, 0);
	
	for (i=0; i<64; i++)
	{
		if (stricmp(name, uw_color_name[i])==0)
		{
			*clr = i;
			return;
		}
	}
	*clr = -1000;
}

/*********************************************************************
**    E_FUNCTION     : ncl_store_clr(clrstr, len, isub, red, green, blue, err)
**       store a color
**       
**    PARAMETERS   
**       INPUT  : 
**          clrstr: color name string
**			len  length of color name string
**			isub subscript of the color
**			red, green, blue: color value to be stored
**       OUTPUT :  
**          err: error number
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_store_clr(clrstr, len, isub, red, green, blue, err)
UM_f77_str_ptr clrstr;
UM_int4 *len, *isub, *red, *green, *blue,*err;
{
	int i,indx, clen;
	char *cstr, name[96];
	
	cstr = UM_cstr_of_f77_str(clrstr);
	if (*len>=95) clen = 95;
	else
		clen = *len;
	if (clen==0)
	{
		*err = 537;
		return;
	}
	strncpy(name, cstr, clen);
	name[clen] = '\0';
	ncl_format_label(name, *isub, name, 0);
/*
.....then find custom color index if the color already defined
*/
	indx = -1;
	for (i=0; i<64; i++)
	{
		if (stricmp(name, uw_color_name[i])==0)
		{
			indx = i;
			break;
		}
	}
/*
.....if not defined before, then get available custom color index
*/
	if (indx==-1)
	{
		for (i=0; i<64; i++)
		{
			if (uw_color_name[i][0]=='\0')
			{
				indx = i;
				break;
			}
		}
	}
	if (indx==-1)
	{
		*err = 538;
		return;
	}
	strcpy(uw_color_name[indx], name);
/*
......SAVE ALL IN UPPER CASE
*/
	ul_to_upper(uw_color_name[indx]);
	uw_color_table[indx][0] = *red;
	uw_color_table[indx][1] = *green;
	uw_color_table[indx][2] = *blue;
	ncl_update_colors(1);
	*err = 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_obclr(clrstr, len, isub, red, green, blue,err)
**       obtain a the color value from a color name
**       
**    PARAMETERS   
**       INPUT  : 
**          clrstr: color name string
**			len  length of color name string
**			isub subscript of the color
**       OUTPUT :  
**			red, green, blue: color value to be stored
**          err: error number
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_obclr(clrstr, len, isub, red, green, blue,err)
UM_f77_str_ptr clrstr;
UM_int4 *len,*isub;
UM_real8 *red, *green, *blue;
UM_int2 *err;
{
	int i,indx, clen;
	char *cstr, name[96];
	
	cstr = UM_cstr_of_f77_str(clrstr);
	if (*len>=95) clen = 95;
	else
		clen = *len;
	if (clen==0)
	{
		*err = 537;
		return;
	}
	strncpy(name, cstr, clen);
	name[clen] = '\0';
	ncl_format_label(name, *isub, name, 0);
/*
.....then find custom color index if the color already defined
*/
	indx = -1;
	for (i=0; i<64; i++)
	{
		if (stricmp(name, uw_color_name[i])==0)
		{
			indx = i;
			break;
		}
	}
	if (indx==-1)
	{
		*err = 539;
		return;
	}
	*red = uw_color_table[indx][0];
	*green = uw_color_table[indx][1];
	*blue = uw_color_table[indx][2];
	*err = 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_load_color(file, merge)
**       Load a color file
**       
**    PARAMETERS   
**       INPUT  : 
**          file: color file to be loaded
**			merge:  1: merge the color from color file into current colors
**					0: load colors and overwrite the current colors
**       OUTPUT :  -1: not loaded
**			none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_load_color(file, merge)
char *file;
int merge;
{
	UX_pathname filename,descrip,ext,serror;
	int len, saved,status;
	FILE *fptr;

	if ((file!=NULL)&&(file[0]!='\0'))
		strcpy(filename, file);
	else
	{
		filename[0] = '\0';
		strcpy(descrip, "Color Mode Files (*.mod)");
		strcpy(ext,"*.mod");
		ud_get_filename("Load Color File", "Color File", ext,
						filename, &len, descrip, 1, UU_FALSE) ;
	}
	if (filename[0]=='\0')
	{
		return -1;
	}
	status = ux_fopen0(filename, "r", &fptr);
	if (status!=UU_SUCCESS || fptr==UU_NULL)
	{
		sprintf (serror,"Cannot Open Color File %s.", filename);
		ud_wrerr (serror);
		return -1;
	}
	if (merge)
		ul_save_clrmod();
/*
......initial color setting
*/
	status = ul_load_clrfile(fptr);
	ux_fclose0 (fptr);
	if (merge)
	{
		saved = UR_restore_clr;
		UR_restore_clr = 2;
		ul_unibase_clr();
		UR_restore_clr = saved;
	}
	return 0;
}

