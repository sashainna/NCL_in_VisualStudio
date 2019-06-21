
/*********************************************************************
**    NAME         :  atxtfrm.c
**       CONTAINS:
**    		ua_get_txt_attr
**				ua_set_txt_attr
**				uaf_set_txt_attr
**				uaf_get_txt_defstyle
** 			ua_get_txt_fontnum 
** 			ua_get_txt_fontname 
** 			ua_get_txt_fontname_list
** 			uaf_check_font
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       atxtfrm.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:41
*********************************************************************/

#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfmracs.h"
#include "atext.h"
#include "adrfcom.h"
#include "modef.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "mfort.h"
#include "mpocket.h"
#include "nclfc.h"
#include "ulist.h"
#include "xenv1.h"

#define NO_DEFAULT 0
#define DEFAULT 1

#define FFNT 4

static UD_FSTAT OnFont();
static void S_close_window();
char **ua_get_txt_fontname_list();

static UU_REAL line_wt[4] = {1.0, 2.0, 3.0, 4.0};

/*********************************************************************
**    E_FUNCTION     : ua_get_txt_attr(attr, formfile)
**       Set text attributes using forms package.
**    PARAMETERS   
**       INPUT  : 
**          formfile				form file name
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ua_get_txt_attr(attr, formfile)
ATXT_FRM		*attr;
char *formfile;
	
{
	int status;
	UU_LOGICAL cmdreject;
	static int	toggle[8];						/* array for the toggle field */
	static UD_LIST fontname_list;
	static UU_REAL txt_hgt, txt_ang, txt_exp, txt_space,
						txt_sub_sup, txt_line_space;

	static char traverse[] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
	static char called[] = {6,6,6,6,6,6,6,6,6,6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

	static UD_METHOD methods[] =
		{UU_NULL,UU_NULL,UU_NULL,OnFont,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL};
	static int *ans[] = {
		(int *)&toggle[0],(int *)&txt_hgt,(int *)&txt_ang,UU_NULL,
		(int *)&fontname_list, 
		(int *)&toggle[1],(int *)&toggle[2],(int *)&toggle[3],
		(int *)&toggle[4],(int *)&toggle[5],(int *)&txt_exp,(int *)&txt_space,
		(int *)&toggle[6],(int *)&txt_sub_sup,(int *)&txt_line_space};

/*
.....Command Reject
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject) goto done;

/*	toggle[0] = attr->color - 1;*/
	toggle[0] = attr->color;
	UM_len_inttoext(attr->height,txt_hgt);
	txt_ang = attr->tangle;
/*
.....Use fontname list for field
*/
	fontname_list.item = ua_get_txt_fontname_list(&(fontname_list.num_item));
	fontname_list.answer = (char *)uu_malloc(80*sizeof(char));
	strcpy(fontname_list.answer,attr->fontname);

	toggle[1] = attr->prec;
	toggle[2] = attr->entity_site;
	toggle[3] = attr->path;
	toggle[4] = attr->align_hor;
	toggle[5] = attr->align_ver;
	txt_exp = attr->expn;
	UM_len_inttoext(attr->spacing,txt_space);
	toggle[6] = attr->txt_dens - 1;
	txt_sub_sup = attr->sub_sup;
	UM_len_inttoext(attr->line_spacing,txt_line_space);

	status = ud_form1(formfile, ans, ans,methods,called,display,traverse);
	if (status==-1) goto done;
	attr->color = toggle[0];
	attr->prec = toggle[1];
	attr->entity_site = toggle[2]; 
	attr->path = toggle[3]; 
	attr->align_hor = toggle[4]; 
	attr->align_ver = toggle[5]; 
	attr->txt_dens = toggle[6] + 1; 
	UM_len_exttoint(txt_hgt,attr->height);
	attr->tangle = txt_ang;
	strcpy(attr->fontname, fontname_list.answer);
	attr->expn = txt_exp;
	UM_len_exttoint(txt_space,attr->spacing);
	attr->sub_sup = txt_sub_sup;
	UM_len_exttoint(txt_line_space,attr->line_spacing);

done:;
	S_close_window();
	UD_UNMARK(cmdreject);
	return;
} 	/* ua_set_txt_attr */

/*********************************************************************
**    E_FUNCTION     : ua_set_txt_attr(formfile)
**       Set text attributes using forms package.
**    PARAMETERS   
**       INPUT  : 
**          formfile				form file name
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_set_txt_attr(formfile)
char	*formfile;
	{
	int status;
	UU_LOGICAL cmdreject;
	char **ua_get_txt_fontname_list();
	static UU_REAL  farr[8];
	static int txt_place, txt_site, txt_entry, app_text, txt_orient, txt_just,
					txt_dens, txt_color;
	static UD_LIST fontname_list;

	static char traverse[] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
	static char called[] = {6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

	static UD_METHOD methods[] =
		{UU_NULL,UU_NULL,UU_NULL,OnFont,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL};

	static int *ans[] = {&txt_color, (int *)&farr[0], (int *)&farr[1], UU_NULL,
								(int *)&fontname_list, &txt_place, &txt_site,&txt_entry,
								&app_text, &txt_orient, &txt_just, (int *)&farr[2], 
								(int *)&farr[3], (int *)&farr[4], &txt_dens, 
								(int *)&farr[5], (int *)&farr[6]};
/*
.....Command Reject
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject) goto done;

	txt_color = UA_char_color;
	farr[0] = UA_char_size;
	farr[1] = UA_text_ang;
/*
.....Use fontname list for field
*/
	fontname_list.item = ua_get_txt_fontname_list(&(fontname_list.num_item));
	fontname_list.answer = (char *)uu_malloc(80*sizeof(char));
	strcpy(fontname_list.answer,UA_txt_fontname);

	txt_place = UA_txt_place;
	txt_site = UA_entity_site;
	txt_entry = UA_txt_entry;
	app_text = UA_app_text;
	txt_orient = UA_txt_orient;
	txt_just = UA_txt_just;
	farr[2] = UA_grid_dist;
	farr[3] = UA_char_expansion;
	farr[4] = UA_char_space;
	txt_dens = UA_char_dens - 1.0;
	farr[5] = UA_sub_sup_ratio;
	farr[6] = UA_line_spacing;

	status = ud_form1(formfile, ans, ans,methods,called,display,traverse);
	if (status==-1) goto done;

	UA_txt_place = 	txt_place;
	UA_entity_site =	txt_site;
	UA_txt_entry =		txt_entry;
	UA_app_text =		app_text;
	UA_txt_orient =	txt_orient;
	UA_txt_just =		txt_just;
	UA_char_size = farr[0];
	UA_text_ang = farr[1];
	UA_grid_dist = farr[2];
	UA_char_expansion = farr[3];
	UA_char_space = farr[4];
	UA_char_dens = line_wt[txt_dens];
	UA_sub_sup_ratio = farr[5];
	UA_line_spacing = farr[6];
	UA_char_color =	txt_color;

	/* set font name and get font number from environment table 
	   if font name is valid */
	ua_set_txt_fontname(fontname_list.answer);

done:;
	S_close_window();
	ud_free_flist(&fontname_list);
	UD_UNMARK(cmdreject);
	return;

	}
/*********************************************************************
**    I_FUNCTION     : OnFont(fieldno, val, stat)
**			View font callback.  Displays the selected font's drawing in
**			a pocket window.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Current field value.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnFont(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fontn;
	char *p,fontname[30],title[40],buf[30];
	char *ux_getenv(),*strchr();
	UX_pathname dir,fname;
	UD_DDATA data;
/*
.....Call the default method
.....This causes the answer field to be updated
*/
	ud_default_method(fieldno, val, stat);
/*
.....Get the font name
*/
	data.frmstr = fontname;
	ud_get_field(FFNT,data,UU_FALSE);
/*
.....Get the actual font filename
*/
	fontn = ua_get_txt_fontnum(fontname);
	sprintf(buf,"FONT%d",fontn);
	p = ux_getenv(buf,UX_NPRTERRS);
	if (p == UU_NULL) goto failed;
	ul_break_fname(p,dir,fname);
	p = strchr(fname,'.');
	if (p != UU_NULL) *p = '\0';
/*
.....Close any open pocket window
.....and delete drawing
*/
	S_close_window();
/*
.....Open the pocket window with the font drawing
*/
	sprintf(title,"Font: %s",fname);
	strcpy(buf,"font");
	um_load_pocket_drawing(title,buf,fname,dir,1);
	goto done;
/*
.....Could not load font drawing
*/
failed:;
	return(UD_BADREQ);
/*
.....Let Form-DAS know that a pocket window is open
*/
done:;
	ud_setform_pocket(0,1);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_close_window()
**			Closes the pocket window and deletes the font drawing.
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_close_window()
{
	UU_KEY_ID key;
/*
.....Close the pocket window
*/
	um_close_pocket_window(UM_DRAWING_WINDOW);
	um_close_pocket_window(UM_GRAPHIC_WINDOW);
/*
.....Delete the font drawing
*/
	if (um_key_from_drawing_name("font",&key) == UU_SUCCESS)
		um_dl46_deldrawing(key);
}

/*********************************************************************
**    E_FUNCTION     : uaf_set_txt_attr(key,iattr,rattr,fontname,ierr)
**       Fortran callable routine to set default text attributes or
**       modify existing annotation's attributes.
**    PARAMETERS   
**       INPUT  : 
**          key      Key of annotation entity to modify.  0 = Set
**                   default text attributes.
**          iattr    (0) = Text Color.
**                   (1) = Text Pen.
**                   (2) = Text style (stroke, string).
**                   (3) = Text start site.
**                   (4) = Text direction.
**                   (5) = Horizontal alignment.
**                   (6) = Vertical alignment.
**                   (7) = Text Density.
**                   (8) = Text Layer.
**
**          rattr    (0) = Character height.
**                   (1) = Character expansion.
**                   (2) = Character rotation.
**                   (3) = Character spacing.
**                   (4) = Line spacing.
**                   (5) = Super/sub-script size.
**
**          fontname = Name of font.
**       OUTPUT :  
**          ierr     0 = Success, 1 = Error trying to change attribute,
**                   2 = Cannot change attribute for annotation type.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uaf_set_txt_attr(key,iattr,rattr,fontname,ierr)
UU_KEY_ID *key;
int iattr[];
UU_REAL rattr[];
char fontname[];
UM_int2 *ierr;
{
	int itype;
	struct UA_txt_rec note;
	struct UA_txtattr_rec txtattr;
	UM_transf rotmat;
/*
.....Initialize routine
*/
	*ierr = 0;
/*
.....Set the default text attributes
*/
	if (*key == 0)
	{
		if (iattr[0] != -1) UA_txtattr.color = iattr[0];
		if (iattr[2] != -1) UA_txtattr.prec = iattr[2];
		if (iattr[3] != -1) UA_txtattr.entity_site = iattr[3];
		if (iattr[4] != -1) UA_txtattr.path =	iattr[4];
		if (iattr[5] != -1) UA_txtattr.align_hor = iattr[5];
		if (iattr[6] != -1) UA_txtattr.align_ver = iattr[6];
		if (iattr[7] != -1) UA_txtattr.txt_dens = iattr[7] + 1;
		if (rattr[0] != -1) UM_len_exttoint(rattr[0],UA_txtattr.height);
		if (rattr[1] != -1) UA_txtattr.expn = rattr[1];
		if (rattr[2] != -1) UA_txtattr.tangle = rattr[2] / UM_RADIAN;
		if (rattr[3] != -1) UM_len_exttoint(rattr[3],UA_txtattr.spacing);
		if (rattr[4] != -1) UM_len_exttoint(rattr[4],UA_txtattr.line_spacing);
		if (rattr[5] != -1) UA_txtattr.sub_sup = rattr[5];
		if (fontname[0] != '\0') strcpy(UA_txtattr.fontname,fontname);
	}
/*
.....Modify an annotation's attributes
*/
	else
	{
		*ierr = 1;
/*
........Get the attributes
*/
		note.key = *key;
		if (ua_get_text1(&note,sizeof(struct UA_txt_rec)) != UU_SUCCESS)
			goto done;
		if (uc_retrieve_attr(note.key,&txtattr) != UU_SUCCESS) goto done;
/*
........Make sure attribute is changeable with annotation type
*/
		ua_get_note_type(&note,&itype);
		if (itype == 3 && (iattr[2] != -1 || iattr[3] != -1  || iattr[4] != -1 ||
			iattr[5] != -1 || iattr[6] != -1 || rattr[0] != -1 || rattr[1] != -1 ||
			rattr[2] != -1 || rattr[3] != -1 || rattr[5] != -1 ||
			fontname[0] != '\0'))
		{
			*ierr = 2;
			goto done;
		}
/*
........If along Arc, doe not allow angle change
*/
		if (itype == 2 && iattr[2] != -1)
		{			
			*ierr = 2;
			goto done;
		}
/*
........Update the attributes
*/
		if (iattr[0] != -1) txtattr.color = iattr[0];
		if (iattr[1] != -1) txtattr.pen = iattr[1];
		if (iattr[2] != -1) txtattr.prec = iattr[2];
/*
.....Need to update the origin so the chage can be seen.  This
.....matches the function when the angle changes - Andrew 4/3/13
*/
		if (iattr[3] != -1) 
		{
/*
.....Remove previous change to the origin
*/
			ua_txt_origin(&note,&txtattr,UU_TRUE);
			txtattr.entity_site = iattr[3];
/*
.....Calculate new origin
*/
			ua_txt_origin(&note,&txtattr,UU_FALSE);
		}
		if (iattr[4] != -1) txtattr.path =	iattr[4];
		if (iattr[5] != -1) txtattr.align_hor = iattr[5];
		if (iattr[6] != -1) txtattr.align_ver = iattr[6];
		if (iattr[7] != -1) txtattr.txt_dens = iattr[7] + 1;
		if (iattr[8] != -1) txtattr.layer = iattr[8];
		if (rattr[0] != -1) UM_len_exttoint(rattr[0],txtattr.height);
		if (rattr[1] != -1) txtattr.expn = rattr[1];
		if (rattr[2] != -1)
		{
/*
........Reset the origin before rotation
*/
			ua_txt_origin(&note,&txtattr,UU_TRUE);

			if (note.tangle != 0.)
			{
				um_rottf(txtattr.plane,-note.tangle,rotmat);
				um_cctmtf(txtattr.up,rotmat,txtattr.up);
			}
			note.tangle = rattr[2] / UM_RADIAN;
			if (note.tangle != 0.)
			{
				um_rottf(txtattr.plane,note.tangle,rotmat);
				um_cctmtf(txtattr.up,rotmat,txtattr.up);
			}
/*
........Set new origin after rotation
*/
			ua_txt_origin(&note,&txtattr,UU_FALSE);
		}
		if (rattr[3] != -1) UM_len_exttoint(rattr[3],txtattr.spacing);
		if (rattr[4] != -1) UM_len_exttoint(rattr[4],txtattr.line_spacing);
		if (rattr[5] != -1) txtattr.sub_sup = rattr[5];
		if (fontname[0] != '\0') txtattr.font = ua_get_txt_fontnum(fontname);
/*
........Store the updated attributes
*/
		if (rattr[2] != -1 || iattr[3] != -1) ur_update_data_fixed(&note,UM_DEFAULT_TF);
		ur_update_attr(&txtattr);
/*
........Display the updated annotation
*/
		uc_display(&note);
		*ierr = 0;
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : uaf_get_txt_defstyle(style)
**       Fortran callable routine to return the default annotation
**       style (STRING,STROKE).
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**          style    0 = String text, 2 = Stroked text.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uaf_get_txt_defstyle(style)
int *style;
{
/*
.....Return default style
*/
	*style = UA_txtattr.prec;
	return;
}

/*********************************************************************
**    E_FUNCTION :  ua_get_txt_fontnum(fontname)
**       Given a text font name, retun the corresponding font number
**			specified in init file.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_get_txt_fontnum(fontname)
char		*fontname;

{
	int		retcode;
	char		fontenv[101];
	char		fontnnn[101];
	int		fontnum;
 	char     *cptr;
 	char     *ux_getenv();


	uu_denter(UU_STRC,(us,"ua_get_txt_fontnum(fontname=%s)", fontname));

	fontnum = 0;
	strcpy(fontenv,"FONT_");
	strcat(fontenv,fontname);
	cptr = ux_getenv(fontenv, UX_PRTERRS);
/*	if (cptr == UU_NULL)*/					/* no such font in init file */  
/*		uu_uerror1(13,5,fontname);*/
/*	else*/
	if (cptr != UU_NULL)
	 {
		strcpy(fontnnn,cptr);
		retcode = sscanf(fontnnn,"FONT%d",&(fontnum));
		if ( retcode==1 ) 
		  {
			uu_dexit;
			return(fontnum);
		  }
	 }
	uu_dexit;
	return(fontnum);
} 	/* ua_get_txt_fontnum */

/*********************************************************************
**    E_FUNCTION :  ua_get_txt_fontname(fontnum,fontname)
**       Given a text font number, retun the corresponding font name
**			specified in init file.
**    PARAMETERS   
**       INPUT  : 
**          fontnum   Font number.
**       OUTPUT :  
**          fontname  Name of font assigned this font number.
**    RETURNS      : none
**    SIDE EFFECTS :
**			Returns the first fontname associated with this number.
**    WARNINGS     : none
*********************************************************************/
void ua_get_txt_fontname(fontnum,fontname)
int fontnum;
char *fontname;
{
	int nfont,i;
	UU_LIST varlist,envlist;
	char **v,**e,sbuf[80];
/*
.....Build a list of font names
*/
	nfont = ux_getenv_list("FONT_",&varlist,&envlist);
/*
.....Get the first fontname that matches
.....the request "FONT_number" string
*/
	v = (char **)UU_LIST_ARRAY(&varlist);
	e = (char **)UU_LIST_ARRAY(&envlist);
	sprintf(sbuf,"FONT%d",fontnum);
	for (i=0;i<nfont;i++)
	{
		if (strcmp(sbuf,e[i]) == 0) break;
	}
	if (i < nfont) strcpy(fontname,&v[i][5]);
	else strcpy(fontname,sbuf);
/*
.....Free the list memory
*/
	uu_list_free(&varlist);
	uu_list_free(&envlist);
}

/*********************************************************************
**    E_FUNCTION :  ua_get_txt_fontname_list(nfont)
**       Returns the list of defined font names in the environmental
**			table.  Typically used for form choicelist fields.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**          Number of font names in list.
**    RETURNS      : none
**    SIDE EFFECTS :
**			Returns the first fontname associated with this number.
**    WARNINGS     : none
*********************************************************************/
char **ua_get_txt_fontname_list(nfont)
int *nfont;
{
	int i;
	UU_LIST varlist,envlist;
	char **v,**fontname;
/*
.....Build a list of font names
*/
	*nfont = ux_getenv_list("FONT_",&varlist,&envlist);
/*
.....Return the list of font names
*/
	v = (char **)UU_LIST_ARRAY(&varlist);
	fontname = (char **)uu_malloc((*nfont)*sizeof(char *));
	for (i=0;i<(*nfont);i++)
	{
		fontname[i] = (char *)uu_malloc(sizeof(UA_txt_fontname));
		strcpy(fontname[i],&v[i][5]);
	}
/*
.....Free the list memory
*/
	uu_list_free(&varlist);
	uu_list_free(&envlist);
	return(fontname);
}

/*********************************************************************
**    E_FUNCTION :  uaf_check_font(fontname,ierr)
**       Determines if a font name has been assigned to a font number.
**    PARAMETERS   
**       INPUT  : 
**          fontname  Name of font to check.
**       OUTPUT :  
**          ierr      0 = Found font definition, 1 = Did not.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uaf_check_font(fontname,ierr)
char *fontname;
int *ierr;
{
	int fontn;
/*
.....Get the actual font filename
*/
	*ierr = 0;
	fontn = ua_get_txt_fontnum(fontname);
	if (fontn == 0) *ierr = 1;
	return;
}
