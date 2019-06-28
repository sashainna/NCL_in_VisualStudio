
/*********************************************************************
**    NAME         :  atxtinit.c
**       CONTAINS:
**							ua_init_txtrec(note,txtattr,txtinfo,reinit)
**							ua_init_text()
**							ua_save_text()
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       atxtinit.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:41
*********************************************************************/
#include		"usysdef.h"
#include 	"zsysdep.h"
#include		"udebug.h"
#include		"mdcoord.h"
#include		"mattrddl.h"
#include		"adrfcom.h"
#include		"mfort.h"
#include		"nclfc.h"
#define		ATEXT
#include		"atext.h"
#undef		ATEXT


/*********************************************************************
**    I_FUNCTION :  void ua_init_txtrec(note,txtattr,txtinfo,reinit)
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_init_txtrec(note,txtattr,txtinfo,reinit)
struct	UA_txt_rec			*note;
struct	UA_txtattr_rec		*txtattr;
ATXT_FRM	*txtinfo;
UU_LOGICAL	reinit;		/* flag to tell whether to init all the field or not*/
{
	UM_coord	cporig, cpxaxis;
	UU_REAL scale;
	UM_transf	rotmat;
	UM_int2 itype,ilbl;


	uu_denter(UU_STRC,(us,"ua_init_txtrec()"));
		/* This flag is set to avoid initializing the text position and text string */
	if (!reinit)
	{
		note->dx = note->dy = 0.0;
		note->position[0] = note->position[1] = note->position[2] = 0.0;
		note->no_tchar = 0;
		note->label[0] = '\0';
		note->subscr = 0;
		um_init_lablocal(note);
	}
	note->subtype = (int)main_txt1;
	note->tangle = (*txtinfo).tangle;
	note->arckey = 0;

	txtattr->color = (*txtinfo).color;
	txtattr->layer = ur_get_attrmdl_layer();
	txtattr->pen = ur_get_attrmdl_pen();
	txtattr->line_style = ur_get_attrmdl_line_style();
	txtattr->line_weight = ur_get_attrmdl_line_weight();
	txtattr->line_width = ur_get_attrmdl_line_width();
	txtattr->displayable = ur_get_attrmdl_displayable();
	txtattr->selectable = ur_get_attrmdl_selectable();
	itype = 30; lblchk(&itype,&ilbl);
	txtattr->label_on = ilbl;
	txtattr->font = ua_get_txt_fontnum((*txtinfo).fontname); 
	txtattr->prec = (*txtinfo).prec;;
	txtattr->expn = (*txtinfo).expn;
	um_get_drwscale(&scale);
	txtattr->height = (*txtinfo).height/scale;
	txtattr->spacing = (*txtinfo).spacing;		/* or UA_line_spacing? */
	txtattr->path = (*txtinfo).path;
	txtattr->align_hor = (*txtinfo).align_hor; 
	txtattr->align_ver = (*txtinfo).align_ver;;
	txtattr->txt_dens = (*txtinfo).txt_dens;
	txtattr->sub_sup = (*txtinfo).sub_sup;
	txtattr->line_spacing = (*txtinfo).line_spacing;
	txtattr->entity_site = (*txtinfo).entity_site;
	txtattr->slant = (*txtinfo).slant;
	if (txtattr->prec != UG_STROKE)
	{
		note->tangle = 0.;
	}
	um_getcpln(cporig,cpxaxis,txtattr->up,txtattr->plane);
	if (note->tangle != 0.0)		/* up must be rotated */
	  {
		um_rottf(txtattr->plane,(*txtinfo).tangle,rotmat);
		um_cctmtf(txtattr->up,rotmat,txtattr->up);
	  }
	uu_dexit;

}	

/*********************************************************************
**    I_FUNCTION :  ua_init_text()
**       Called by adrfinit.c to initialize the global text attribute
**			at the very begining of runing ddc.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_init_text()

	{
	uu_denter(UU_STRC,(us,"ua_init_text"));
	UA_TXT_ASSO = UU_FALSE;
	UA_txtattr.color = UA_char_color_note;
	UA_txtattr.prec = (int)UA_txt_precision_note;
	UA_txtattr.expn = UA_char_expansion_note;
	UA_txtattr.spacing = UA_char_space_note;
	UA_txtattr.height = UA_char_size_note;
	UA_txtattr.tangle = UA_text_ang_note;
	UA_txtattr.path = (int)UG_TP_RIGHT;
	UA_txtattr.align_hor = (int)UG_TH_NORMAL;
	UA_txtattr.align_ver = (int)UG_TV_NORMAL;
	UA_txtattr.txt_dens = UA_char_dens_note;
	UA_txtattr.slant = UA_char_slant_note;
	UA_txtattr.sub_sup = UA_sub_sup_ratio_note;
	UA_txtattr.line_spacing = UA_line_spacing_note;
	UA_txtattr.entity_site = UA_entity_site_note;
	strcpy(UA_txtattr.fontname,UA_txt_fontname_note);
	zbytecp(UB_txtattr,UA_txtattr);
	uu_dexit;
	}	

/*********************************************************************
**    I_FUNCTION :  ua_save_text()
**       Called by adefault.c to save the global text attribute
**			in the user defined DRAFTING STANDARDS file.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_save_text()
	{
	uu_denter(UU_STRC,(us,"ua_save_text"));
	UA_char_color_note = UA_txtattr.color;
	UA_txt_precision_note = (Gtxprec) UA_txtattr.prec;
	UA_char_expansion_note = UA_txtattr.expn;
	UA_char_space_note = UA_txtattr.spacing;
	UA_char_size_note = UA_txtattr.height;
	UA_text_ang_note = UA_txtattr.tangle;
	UA_char_dens_note = UA_txtattr.txt_dens;
	UA_char_slant_note = UA_txtattr.slant;
	UA_sub_sup_ratio_note = UA_txtattr.sub_sup;
	UA_line_spacing_note = UA_txtattr.line_spacing;
	UA_entity_site_note = UA_txtattr.entity_site;
	strcpy(UA_txt_fontname_note, UA_txtattr.fontname);
	uu_dexit;
	}	

/*********************************************************************
**    I_FUNCTION :  ua_init_txt_alone()
**       A stand alone initialization routine to initialize the text
**		attributes. User does not have to call ua_init_drafting to set
**		all UA_*_* variables first. 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_init_txt_alone()

	{
	uu_denter(UU_STRC,(us,"ua_init_txt_alone"));
	UA_TXT_ASSO = UU_FALSE;
	UA_txtattr.color = 6;
	UA_txtattr.prec = (int)UG_STROKE;
	UA_txtattr.expn = 1.000000e+000;
	UA_txtattr.spacing = 2.000000e-001;
	UA_txtattr.height = 3.0;
	UA_txtattr.tangle = 0.000000e+000;
	UA_txtattr.path = (int)UG_TP_RIGHT;
	UA_txtattr.align_hor = (int)UG_TH_NORMAL;
	UA_txtattr.align_ver = (int)UG_TV_NORMAL;
	UA_txtattr.txt_dens = 1.000000e+000;
	UA_txtattr.slant = 0.000000e+000;
	UA_txtattr.sub_sup = 7.500000e-001;
	UA_txtattr.line_spacing = 2.000000e-001;
	UA_txtattr.entity_site =  0;
	strcpy(UA_txtattr.fontname,"drafting");
	uu_dprint(UU_STRC,(us,"fontname=%s,%s",UA_txtattr.fontname,UA_txt_fontname));
	zbytecp(UB_txtattr,UA_txtattr);
	uu_dexit;
	}	
