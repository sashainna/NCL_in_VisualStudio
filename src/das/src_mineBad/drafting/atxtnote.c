
/*********************************************************************
**    NAME         :  atxtnote.c
**       CONTAINS:
**					ua_notes()
**					uaf_notes()
**					uaf_get_notes()
**					uaf_obtain_notes()
**					uaf_get_note_type()
**					ua_get_note_type()
**					ua_txt_on_arc()
**					uai_txtinfo_on_line(note,txtattr,location,tline,plocrec)
**					uai_txtinfo_on_arc(note,txtattr,location,arc,plocrec)
**					ua_get_note(prompt, maxsize, text, num_chars)
**					ua_create_note(note)
**					ua_txt_file(prompt, maxsize, text, num_chars)
**					ua_calc_text_attach()
**             ua_save_text_pos()
**             ua_restore_text_pos()
**             ua_reset_text_pos()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       atxtnote.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:41
*********************************************************************/

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "uhep.h"
#include "udebug.h"
#include "umoveb.h"
#include "dselmask.h"
#include "mdcoord.h"
#include "mcrv.h"
#include	"mfort.h"
#include	"mdrel.h"
#include	"mdcpln.h"
#include	"mdpick.h"
#include "dtypes.h"
#include	"dasnog.h"
#include "adraft.h"
#include "adrfcom.h"
#include "atext.h"
#include "nccs.h"
#include "mdattr.h"
#include "nclfc.h"

#define TEXTBUFSZ 1024

extern UU_LOGICAL UA_note_keyboard;
extern UU_KEY_ID	UA_drafting_view;

static UM_coord Slast_letter={0.,0.,0.},Slast_letter_save={0.,0.,0.};
static UM_coord Slast_line={0.,0.,0.},Slast_line_save={0.,0.,0.};

void ua_txt_file(),ua_calc_text_attach(),ua_get_note_type();
static void S_calc_text_projpos(),S_calc_text_nextpos(),S_calc_text_origin();

/*********************************************************************
**    I_FUNCTION :  ua_notes()
**       Let the user create a text(note) entity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_notes()

{
	struct   UA_txt_rec			note;			/* text record */
	struct	UA_txtattr_rec		txtattr;		/* text attribute */
	int		prev_key;
	UU_LOGICAL ilab;
	char		textstr[TEXTBUFSZ];
	int		flag;
	int		status;
	int		count;
	int		num_chars, num_line;
	UM_int2 istat;
	UU_LOGICAL	first;
	UU_LOGICAL	redo;
	UU_REAL	origin[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
/**	UU_REAL	location[3]; **/
    UD_NDCLOCREC location;

	uu_denter(UU_STRC,(us,"ua_notes(),UA_drafting_view=%d", UA_drafting_view));

	first = UU_TRUE;
	ua_note_msg(UA_note_keyboard);

	while (UU_TRUE)
	{
	count = 0;
	redo = UU_FALSE;
	flag = ud_ldas(UD_DASCART,UA_DRAFTING,51,&location,1,&count,UD_NODEFAULT);
	if ((count==0)&&(flag==1))
		break;

	switch( flag )
	{
	case UU_FALSE:
		uu_dexit;
		return;

	case UU_ALTACTION:			/* perform the alternative action */
		if ( first==UU_FALSE ) 
			{
			 redo = UU_TRUE;
			 status = uc_retrieve_data(&note,sizeof(note));
			 if( ( status==0 ) )
				{
				 flag = ud_ldas(UD_DASCART,UA_DRAFTING,50,&location,1,&count,UD_NODEFAULT);
				 if( ( flag==0 ) ) {
					uu_dexit;
					return;
					}
				}
			 else
				{
			 	 redo = UU_FALSE;
				 first = UU_TRUE;
				 continue;
				}
			}
		 else
			{
			 UA_note_keyboard = ( !UA_note_keyboard );
			 ua_note_msg(UA_note_keyboard);
			 continue;	
			}
		 break;

	case UU_TRUE:			/* create a text entity */
		ua_init_txtrec(&note,&txtattr,&UA_txtattr,UU_FALSE);
		textstr[0] = '\0';
			/* create new text entity	*/
		ur_setup_data(UA_TEXT_REL,&note,sizeof(struct UA_txt_rec));
		ur_setup_data(UA_TEXTATTR_REL,&txtattr,sizeof(struct UA_txtattr_rec));
		num_line = 0;				/* means no control of input text lines */
		if ( UA_note_keyboard )
			ua_get_texts(52,255,textstr,&num_chars,&num_line);
		else
			ua_txt_file(127,1024,textstr,&(num_chars));
	   break;
	}	/* switch */

		um_getcpln(origin,xaxis,yaxis,zaxis);
		um_nptpln(&location,origin,zaxis,note.position);
	
		if (note.label[0] == '\0')
		{
			ncl_label_wf(note.rel_num,note.label,&note.subscr,note.key,&istat);
			ilab = UU_TRUE;
		}
		if ( redo )
  		  {
			note.key = prev_key;
			status = um_update_geom(&note,UM_DEFAULT_TF);
			if ( status!=0 ) 
				status = uc_create_data(&note,UM_DEFAULT_TF,&txtattr);
  		  }
		else
			status = uc_create_data(&note,UM_DEFAULT_TF,&txtattr);
		status = ur_update_data_varlist(note.key,1,textstr,1,num_chars+1);
		status = ncl_retrieve_data_fixed (&note);
		ua_txt_origin(&note,&txtattr,UU_FALSE);	/* set text origin and dx, dy */
		status = ur_update_data_fixed(&note);

		if (ilab) ncl_store_wf1(note.key);

		if (status == UU_SUCCESS)
			if (UA_dims_disp == 0)
				status = ur_update_view_key(note.key, UA_drafting_view);
			else
				status = ur_update_view_key(note.key, 0);
		else
			status = -1;
	
		if (status == 0) 
		{
			uc_display(&note);
		}
		prev_key = note.key;
		first = UU_FALSE;
	}	/* while loop */

	uu_dexit;
	return;
}	/* ua_notes */

/*********************************************************************
**    E_FUNCTION :  uaf_notes(label,sub,textstr,atfl,pt,ptc,ptl,cvkey,projfl,
**                            sfkey,sfuv,key,ierr)
**       Create a note text (annotation).  Fortran callable routine.
**    PARAMETERS   
**       INPUT  : 
**          label    Entity label.
**          sub      Entity subscript.
**          texstr   Annotation text.
**          atfl     0 = Position text after last annotation letter,
**                   1 = Position text after last annotation line,
**                   2 = Position text at user supplied location.
**          pt       Location to position text.
**          projfl   Surface projection flags. (0) = -1 is no projection,
**                   (1) = Wrap style, (2) = Start point.
**          pjkey    (0) = Key of surface to project to, (1) = Key of
**                   projection vector, (2) = Key of attach point,
**                   (3) = Key of near point.
**          sfuv     Starting UV-parameter of surface.
**          cvkey    Key of curve to follow.  0 = No curve.
**       OUTPUT :  
**          ptc      Coordinates of next character position.
**          ptl      Coordinates of next line position.
**          key      Key of created annotation.
**          ierr     Returns non-zero if annotation could not be created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uaf_notes(label,sub,textstr,atfl,pt,ptc,ptl,cvkey,projfl,pjkey,sfuv,key,
	ierr)
char *label;
int *sub;
char textstr[];
int *atfl;
UU_REAL pt[],ptc[],ptl[],sfuv[];
UU_KEY_ID *key,*cvkey,pjkey[];
UM_int2 *ierr,projfl[];
{
	int i,status,nc,iproj,iwrap,iatach,relnum;
	UM_int2 i3,i4;
	UU_KEY_ID tkey;
	UU_LIST ptlist;
	UM_coord *pts,atpt;
	UM_covec *pto;
	UM_transf tfmat;
	struct UA_txt_rec note,tnote;
	struct UA_txtattr_rec txtattr;
	struct NCL_fixed_databag crv,crv1;
	char *tchar;
	int nchar;
	UM_int2 lfl_77;

/*
.....Initialize text record
*/
	*ierr = 0;
	i3 = 3; i4 = 4;
	ua_init_txtrec(&note,&txtattr,&UA_txtattr,UU_FALSE);
	strcpy(note.label,label);
	note.subscr = *sub;
/*
.....If Anote already exists
.....then use the existing attributes
*/
	status = ncl_vxchk(note.label,note.subscr,&tkey,&relnum);
	if (status == UU_SUCCESS && tkey != 0 && relnum == UA_TEXT_REL)
	{
		tnote.key = tkey;
		if (ua_get_text1(&tnote) == UU_SUCCESS)
		{
			if (uc_retrieve_attr(tkey,&txtattr) == UU_SUCCESS)
				note.tangle = tnote.tangle;
		}
	}
/*
.....Setup text record
*/
	ur_setup_data(UA_TEXT_REL,&note,sizeof(struct UA_txt_rec));
	ur_setup_data(UA_TEXTATTR_REL,&txtattr,sizeof(struct UA_txtattr_rec));

	note.no_displst = 0;
	note.displst = NULL;

	nc = strlen(textstr);
	tchar = (char *) uu_malloc(sizeof(char)*(nc+2));

	nchar = 0;
	for (i=0;i<nc;i++)
	{
		if (textstr[i] == '\\' && textstr[i+1] == 'n')
		{
			tchar[nchar] = '\n';
			i++;
		}
		else
			tchar[nchar] = textstr[i];
		nchar++;
	}
	tchar[nchar] = '\n';
	tchar[nchar+1] = '\0';
	nchar += 2;

	crv.key = *cvkey;
	if (crv.key != NULLKEY)
	{
		status = ncl_retrieve_data_fixed (&crv);
		if (status != UU_SUCCESS) goto done;

		if (crv.rel_num == UM_CIRCLE_REL)
		{
			lfl_77 = 1;
			stunlb (&lfl_77);
			crv1.key = 0;
			status = uc_copy (&crv, &crv1, sizeof(struct NCL_fixed_databag));
			stunlb (&lfl_77);

			if (status == UU_SUCCESS)
			{
				ur_update_displayable(crv1.key, UM_NEVERDISPLAYABLE);
				note.arckey = crv1.key;
			}
		}
		else
			note.arckey = crv.key;
	}
/*
.....Create the text entity
*/
	status = uc_create_data(&note,UM_DEFAULT_TF,&txtattr);
	if (status == UU_SUCCESS)
	{
		*key = note.key;
		status = ur_update_data_varlist(note.key,1,tchar,1,nchar);
	}
	if (status == UU_SUCCESS)
		status = ncl_retrieve_data_fixed (&note);
	if (status != UU_SUCCESS) goto done;
/*
.....Free the text memory
*/
	uu_free(tchar);
/*
.....Set text position
*/
	if (*atfl == 0)
		um_vctovc(Slast_letter,note.position);
	else if (*atfl == 1)
		um_vctovc(Slast_line,note.position);
	else
		um_vctovc(pt,note.position);
	S_calc_text_origin(&note,&txtattr);
/*
.....Update text record with new origin & attributes
*/
	status = ur_update_data_fixed(&note);
	if (status != UU_SUCCESS) goto done;
	status = ur_update_attr(&txtattr);
	if (status != UU_SUCCESS) goto done;
/*
.....Project the text
*/
	pto = UU_NULL;
	if (projfl[0] != -1)
	{
		uc_retrieve_transf(note.key,tfmat);
		ug_text_capture(UU_TRUE,&ptlist);
		ua_draw_text(&note,tfmat,&txtattr);
		pts = (UM_coord *)UU_LIST_ARRAY(&ptlist);
		nc = UU_LIST_LENGTH(&ptlist);
		iproj = projfl[0];
		iwrap = projfl[1];
		iatach = projfl[2];
		if (iatach >= 1 && iatach != 5)
		{
			S_calc_text_projpos(&note,&txtattr,iatach,atpt);
			iatach = 5;
		}
		else if (iatach == 5)
		{
			ncl_load_pt_vect(&pjkey[2],1,atpt);
		}
		pto = (UM_covec *)uu_malloc(sizeof(UM_covec)*nc);
		status = ncl_poly_project_sf(pts,pjkey[0],sfuv,iwrap,iatach,iproj,
			pjkey[1],atpt,pjkey[3],pto);
		if (status == 0)
		{
			note.no_displst = nc;
			note.displst = (UU_REAL *)pto;
			status = ur_update_data_varlist(note.key,2,pto,1,nc);
		}
		ug_text_capture(UU_FALSE,&ptlist);
	}
/*
.....Free the memory
*/
	if (pto != UU_NULL) uu_free(pto);
/*
.....Return the next character/line position
.....based on the initial text position site
*/
	if (status == 0)
	{
		S_calc_text_nextpos(&note,&txtattr,ptc,ptl);
		um_vctovc(ptc,Slast_letter);
		um_vctovc(ptl,Slast_line);
/*
		ncl_wcstomcs(0,ptc,ptc);
		ncl_wcstomcs(1,&ptc[3],&ptc[3]);
		ncl_wcstomcs(0,ptl,ptl);
		ncl_wcstomcs(1,&ptl[3],&ptl[3]);
		conref(ptc,&i3);
		conref(&ptc[3],&i4);
		conref(ptl,&i3);
		conref(&ptl[3],&i4);
		UM_cc_inttoext(ptc,ptc);
		UM_cc_inttoext(ptl,ptl);
*/
		conpt(ptc,&i3);
		conpt(&ptc[3],&i4);
		conpt(ptl,&i3);
		conpt(&ptl[3],&i4);
	}
	else
		*ierr = 1;
done:;
	if (status != UU_SUCCESS) *ierr = 1;
	return;
}

/*********************************************************************
**    E_FUNCTION :  uaf_get_notes(nclkey,textstr)
**       Returns the text of an annotation.  Fortran callable routine.
**    PARAMETERS   
**       INPUT  : 
**          nclkey   Key of annotation to retrieve.
**       OUTPUT :  
**          textstr  First line of annotation text string.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uaf_get_notes(key,textstr)
UU_KEY_ID *key;
char textstr[];
{
	struct UA_txt_rec note;
/*
.....Get text record
*/
	note.key = *key;
	if (ua_get_text1(&note) == UU_SUCCESS)
	{
/*
.....Return text string
*/
		strcpy(textstr,note.tchar);
	}
/*
.....Could not find annotation record
*/
	else
		textstr[0] = '\0';
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION :  uaf_obtain_notes(nclkey,nline,itype,spt,ept,lpt)
**       Returns the following parameters of an annotation.
**
**          1. Number of lines in annotation.
**          2. Type of annotation.
**          3. Starting position of annotation.
**          4. Next character position of annotation.
**          5. Next line position of annotation.
**    PARAMETERS   
**       INPUT  : 
**          key      Key of annotation to retrieve.
**       OUTPUT :  
**          nline    Number of lines.
**          itype    Type.
**          spt      Starting position.
**          ept      Next character position.
**          lpt      Next line position.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uaf_obtain_notes(key,nline,itype,spt,ept,lpt)
UU_KEY_ID *key;
int *nline,*itype;
UU_REAL spt[],ept[],lpt[];
{
	int i;
	UM_int2 i3;
	UU_REAL pt1[6],pt2[6];
	struct UA_txt_rec note;
	struct UA_txtattr_rec txtattr;
/*
.....Get text record
*/
	i3 = 3;
	note.key = *key;
	if (ua_get_text1 (&note) == UU_SUCCESS)
	{
/*
.....Determine number of lines in annotation
*/
		*nline = 1;
		for (i=0;i<note.no_tchar-1;i++)
		{
			if (note.tchar[i] == '\n') *nline = *nline + 1;
		}
/*
.....Determine type of annotation
*/
		ua_get_note_type(&note,itype);
/*
.....Get text attributes
*/
		uc_retrieve_attr(note.key,&txtattr);
/*
.....Calculate text positions
*/
		ua_calc_text_attach(&note,&txtattr,txtattr.entity_site,spt);
		S_calc_text_nextpos(&note,&txtattr,pt1,pt2);
/*
		ncl_wcstomcs(0,spt,spt);
		ncl_wcstomcs(0,pt1,ept);
		ncl_wcstomcs(0,pt2,lpt);
		conref(spt,&i3);
		conref(pt1,&i3);
		conref(pt2,&i3);
		UM_cc_inttoext(spt,spt);
		UM_cc_inttoext(pt1,pt1);
		UM_cc_inttoext(pt2,pt2);
*/
		um_vctovc(pt1,ept);
		um_vctovc(pt2,lpt);
		conpt(spt,&i3);
		conpt(ept,&i3);
		conpt(lpt,&i3);
	}
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION :  uaf_get_note_type(nclkey,itype)
**       Returns the type of an annotation.
**
**    PARAMETERS   
**       INPUT  : 
**          key      Key of annotation to retrieve.
**       OUTPUT :  
**          itype    1 = Standard annotation, 2 = Along arc, 3 = Projected.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uaf_get_note_type(key,itype)
UU_KEY_ID *key;
int *itype;
{
	struct UA_txt_rec note;
/*
.....Determine type of annotation
*/
	note.key = *key;
	if (ua_get_text1 (&note) == UU_SUCCESS)
		ua_get_note_type(&note,itype);
	else
		*itype = 1;
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION :  ua_get_note_type(note,itype)
**       Returns the type of an annotation.
**
**    PARAMETERS   
**       INPUT  : 
**          key      Key of annotation to retrieve.
**       OUTPUT :  
**          itype    1 = Standard annotation, 2 = Along arc, 3 = Projected.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_get_note_type(note,itype)
struct UA_txt_rec *note;
int *itype;
{
/*
.....Determine type of annotation
*/
	*itype = 1;
	if (note->arckey != 0) *itype = 2;
	if (note->no_displst != 0) *itype = 3;
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    I_FUNCTION :  ua_txt_on_arc()
**       Display the text on the user picked arc.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void	ua_txt_on_arc()

{
	struct   UA_txt_rec			note;			/* text record */
	struct	UA_txtattr_rec		txtattr;		/* text attribute */
	struct 	UM_line_rec			tline;
	struct 	UM_circle_rec 		arc;
	UU_KEY_ID	key;
	int			relnum;
	char		textstr[TEXTBUFSZ];
	UM_PLOCREC	plocrec;
	int		state;
	int		count;
	int		flag;
	int		num_line, num_chars;
	int 		numint;
    UD_NDCLOCREC location;

	UU_LOGICAL status;
	UU_LOGICAL uai_txtinfo_on_line();
	UU_LOGICAL uai_txtinfo_on_arc();

	uu_denter(UU_STRC,(us,"ua_txt_on_arc()"));

	ud_lgeo(UU_TRUE, UD_lncir);	/* restrict DAS to pick curves&line only */
	while (UU_TRUE)
	{
  	um_dl_pldas(UD_DASPCKLOC,/*pick a curve:*/UM_MODEL,299,&plocrec,1,&numint,2);
	if (numint == 0) 			goto done;
  	key = um_get_pickkey(&plocrec.pent, 2);
	ur_retrieve_data_relnum(key,&relnum);
	state = UU_FAILURE;
	if (relnum == UM_CIRCLE_REL)
	  {
		arc.key = key;
		state = uc_retrieve_data(&arc, sizeof(arc));
		num_line = 1; /* allow only one text line on the arc */
	  }
	else if (relnum == UM_LINE_REL)
			 {
			  tline.key = key;
			  state = uc_retrieve_data(&tline, sizeof(tline));
			  num_line = 0;	/* no control of input text line number */
			 }
	if (state == UU_FAILURE)
		goto done;

	count = 0;
	flag = ud_ldas(UD_DASCART,UA_DRAFTING,51,&location,1,&count,UD_NODEFAULT);
	if (((count==0)&&(flag==1)))
		goto done;

   ua_init_txtrec(&note,&txtattr,&UA_txtattr,UU_FALSE);
   textstr[0] = '\0';
				/* create new text entity	*/
   ur_setup_data(UA_TEXT_REL,&note,sizeof(struct UA_txt_rec));
   ur_setup_data(UA_TEXTATTR_REL,&txtattr,sizeof(struct UA_txtattr_rec));
   note.arckey = key;
   if ( UA_note_keyboard )
		ua_get_texts(52,255,textstr,&num_chars,&num_line);
   else
		ua_txt_file(127,1024,textstr,&(num_chars));
   if ( num_chars==0 ) 		goto done;

	uc_create_data(&note,UM_DEFAULT_TF,&txtattr);
	status = ur_update_data_varlist(note.key,1,textstr,1,num_chars+1);
	status = ncl_retrieve_data_fixed (&note);
	if (relnum == UM_LINE_REL)
		status = uai_txtinfo_on_line(&note,&txtattr,&location,&tline,&plocrec,
			UU_TRUE);
	else	/* must be a circle */
		status = uai_txtinfo_on_arc(&note,&txtattr,&location,&arc,&plocrec,
			UU_TRUE);
	if (status)
	  {
		status = ur_update_data_fixed(&note);
		uu_dprint(UU_STRC,(us,"after create text"));
		uc_display(&note);
	  }
}	/* while */
done:
	ud_lgeo(UU_FALSE, UD_lncir);
	uu_dexit;
	return;
}	/* ua_txt_on_arc */

/*********************************************************************
**    I_FUNCTION :  uai_txtinfo_on_line(note,txtattr,location,tline,plocrec)
**       Get text origin, up vector and normal vector, based on the line
**       and text attribute information.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL uai_txtinfo_on_line(note,txtattr,location,tline,plocrec,flag)
struct   UA_txt_rec			*note;			/* text record */
struct	UA_txtattr_rec		*txtattr;		/* text attribute */
struct 	UM_line_rec			*tline;
UU_REAL	location[3];
UM_PLOCREC	*plocrec;
UU_LOGICAL flag;		/* UU_TRUE = Interactive mode, 2 = Command mode */

{
	struct 	UM_line_rec			line;
	UM_coord		origin;
	UM_vector	xaxis, yaxis, zaxis;
	UM_vector	lnvec, vup;
	UU_REAL	dot;

			/* according to drafting's rule, text on the line should project
				to the construction plane */
	um_getcpln(origin,xaxis,yaxis,zaxis);
	um_proj2_line_to_plane(tline, location, zaxis, &line);
	um_vcmnvc(line.ept,line.spt,lnvec);
	um_cross(zaxis,lnvec,txtattr->up);
	um_vctovc(zaxis,txtattr->plane);
	um_unitvc(txtattr->up,txtattr->up);
	um_unitvc(txtattr->plane,txtattr->plane);
			/* If text up and view up are opposite use the reverse of text up.*/
	if (flag)
		um_vpup((*plocrec).ploc.transform,vup);
	else
		um_vctovc(yaxis,vup);
	if ((dot=um_dot(vup,txtattr->up))<0.0)
		um_vctmsc(txtattr->up,(UU_REAL) -1.0,txtattr->up);
	um_nptpln(location,line.spt,txtattr->plane,note->position);
	ua_txt_origin(note,txtattr,UU_FALSE);	/* set text origin and dx, dy */
	return(UU_TRUE);
}	/* uai_txtinfo_on_line */


/*********************************************************************
**    I_FUNCTION :  uai_txtinfo_on_arc(&note,&txtattr,location,&arc,&plocrec)
**       Get text origin, and normal vector, based on the arc and text 
**			attribute information. 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL uai_txtinfo_on_arc(note,txtattr,location,arc,plocrec,flag)
struct   UA_txt_rec			*note;			/* text record */
struct	UA_txtattr_rec		*txtattr;		/* text attribute */
struct 	UM_circle_rec 		*arc;
UM_PLOCREC	*plocrec;
UU_REAL	location[3];
UU_LOGICAL flag;		/* UU_TRUE = Interactive mode, 2 = Command mode */

{
	UU_REAL	vnormal[3];
	UU_REAL	dot;
	UU_LOGICAL status;
	UU_LOGICAL ua_arctxt_origin();
	UU_LOGICAL clockwise, ud_yesno();
	UM_coord		origin;
	UM_vector	xaxis, yaxis, zaxis;
/*
.....text on an arc will be on the arc's plane
*/
	if (flag)
		um_vpnorm((*plocrec).ploc.transform,vnormal);
	else
	{
		um_getcpln(origin,xaxis,yaxis,zaxis);
		um_vctovc(zaxis,vnormal);
	}
			/* If arc norm and view norm are opposite use the reverse of arc.nvec*/
	if ((dot=um_dot(vnormal,arc->nvec))<0.0)
		um_vctmsc(arc->nvec,(UU_REAL) -1.0,txtattr->plane);
	else
	 	um_vctovc(arc->nvec,txtattr->plane);
	um_nptpln(location,arc->center,vnormal,note->position);
	if (flag)
		clockwise = ud_yesno(0, "Text direction, Clockwise?", "Question?");
	else
		clockwise = UU_TRUE;
			/* find the text's real position */
	status = ua_arctxt_origin(note,txtattr,arc,clockwise);
	return(status);
}	/* uai_txtinfo_on_arc */


/*********************************************************************
**    I_FUNCTION :  ua_get_note(prompt, maxsize, text, num_chars, num_line)
**       Get user's input string for the note.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void	ua_get_note(prompt, maxsize, text, num_chars, num_lines)
int		prompt;
int		maxsize;
char		text[1];
int		*num_chars;
int		*num_lines;

{
	int		char_cnt;
	char		line_text[257];
	char		cr[2];
	int		status;
	int linenum;

	uu_denter(UU_STRC,(us,
		"ua_get_note(prompt=%d, maxsize=%d, text=%s, num_chars=%d)",
		prompt, maxsize, text, *num_chars));

	strcpy(cr,"\n");
	strcpy(text,"");
	*num_chars = 0;
	linenum = 0;
	do
	  {
		status = ud_string(13,prompt,line_text,maxsize,&(char_cnt),UU_FALSE);
		if (char_cnt <= 0) 	
			break;
		else
		  {
			*num_chars = *num_chars + char_cnt + 1;
			if (*num_chars >= TEXTBUFSZ)
			  break;
		  }
		strcat(text,line_text);
		strcat(text,cr);
		linenum++;
	  } while ((*num_lines==0)||(linenum<*num_lines));
	*num_lines = linenum;
	uu_dexit;
}	/* ua_get_note */



/*********************************************************************
**    I_FUNCTION :  ua_create_note(note)
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

void		ua_create_note(note)
struct UA_generic_draft	(*note);
{
/*	UU_REAL	corners[4][3];*/
/*	UU_REAL	offset_cc[3];*/

	uu_denter(UU_STRC,(us,"ua_create_note(note=%s)", "..."));

	/*ua_txt_offset(&((*note)),(*note).txt_blk_use,offset_cc); */
	/*ua_txt_origin(&((*note)),(*note).txt_blk_use,offset_cc,corners); */
	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : ua_txt_file(prompt, maxsize, text, num_chars)
**       Get text from a users supplied text file.
**    PARAMETERS   
**       INPUT  : 
**          prompt					number of prompt in prompt file
**				maxsize					maximum number of character in text
**       OUTPUT :  
**				text						text of note
**				num_chars				number of characters in text
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_txt_file(prompt, maxsize, text, num_chars)
int		prompt;
int		maxsize;
char		text[1];
int		(*num_chars);
{
	int		char_cnt;
	char		form[11];
	char		type[11];
	int		err;
	int		options;
	int		lu;
	char		interp[11];
	char		file_name[61];
	int		status;
	int		mode;

	uu_denter(UU_STRC,(us,
		"ua_txt_file(prompt=%d, maxsize=%d, text=%s, num_chars=%d)",
		prompt, maxsize, text, *num_chars));

	mode = 0;
	options = 1;
	status = ud_string(13,prompt,file_name,60,&(char_cnt),
	UU_FALSE);
	if( ( char_cnt>0 ) )
	{
		status = ux_access1(file_name,&(mode),options);
		if( ( ( status==0 )&&( mode==0 ) ) )
		{
			strcpy(type,"r+");
			strcpy(form,"STREAM");
			strcpy(interp,"BINARY");
			status = ux_open_to_data(file_name,type,form,interp,&(lu),
			options);
			if( ( status==0 ) )
			{
				char_cnt = maxsize;
				strcpy(text,"");
				err = ux_read(lu,text,1,&(char_cnt),options);
				strcpy(text,text);
				strcat(text,"");
				(*num_chars) = char_cnt;
				ux_close(lu,options);
			}
			else
			{
				(*num_chars) = 0;
				uu_uerror1(13,37,file_name);
			}
		}
		else
		{
			(*num_chars) = 0;
			uu_uerror1(13,37,file_name);
		}
	}
	else
	{
		(*num_chars) = 0;
	}
	uu_dexit;
} 		/* ua_txt_file */

/*********************************************************************
**    E_FUNCTION :  ua_calc_text_attach(note,txtattr,atfl,ptc)
**       Calculates the attach point for an annotation text string
**       based on the input site in the text box.
**    PARAMETERS   
**       INPUT  : 
**          note     Annotation record.
**          txtattr  Annotation attribute record.
**          isite    Attach point location within text box, 0:8.
**       OUTPUT :  
**          ptc      Attach position.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_calc_text_attach(note,txtattr,isite,ptc)
struct UA_txt_rec *note;
struct UA_txtattr_rec *txtattr;
int isite;
UU_REAL ptc[];
{
	int relnum;
	UU_LOGICAL cirfl;
	UU_REAL rad[3];
	UM_coord cpt,box[5];
	UM_vector nvec[3],vc1,vc2;
/*
.....Determine if text is on an arc
*/
	cirfl = UU_FALSE;
	if (note->arckey != 0)
	{
		if (ur_retrieve_data_relnum(note->arckey,&relnum) == UU_SUCCESS)
		{
			if (relnum == UM_CIRCLE_REL) cirfl = UU_TRUE;
		}
	}
/*
.....Return the next character/line position
.....based on the initial text position site
*/
	if (!cirfl)
	{
		uai_calc_text_box(note,UM_DEFAULT_TF,txtattr,box);
		switch (isite)
		{
		case UA_TOP_LEFT:
			um_vctovc(box[0],ptc);
			break;
		case UA_MIDDLE_LEFT:
			um_vcmnvc(box[0],box[3],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[3],vc2,ptc);
			break;
		case UA_BOTTOM_LEFT:
			um_vctovc(box[3],ptc);
			break;
		case UA_TOP_CENTER:
			um_vcmnvc(box[1],box[0],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[0],vc2,ptc);
			break;
		case UA_MIDDLE_CENTER:
			um_vcmnvc(box[1],box[3],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[3],vc2,ptc);
			break;
		case UA_BOTTOM_CENTER:
			um_vcmnvc(box[2],box[3],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[3],vc2,ptc);
			break;
		case UA_TOP_RIGHT:
			um_vcmnvc(box[1],box[0],vc1);
			um_vcplvc(box[0],vc1,ptc);
			break;
		case UA_MIDDLE_RIGHT:
			um_vcmnvc(box[1],box[2],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[2],vc2,ptc);
			break;
		case UA_BOTTOM_RIGHT:
			um_vcmnvc(box[2],box[3],vc1);
			um_vcplvc(box[3],vc1,ptc);
			break;
		}
	}
/*
.....Arc text
*/
	else
	{
		uai_calc_arctext_box(note,UM_DEFAULT_TF,txtattr,cpt,nvec,rad);
		switch (isite)
		{
		case UA_TOP_LEFT:
			um_translate_point(cpt,rad[0],nvec[0],ptc);
			break;
		case UA_MIDDLE_LEFT:
			um_translate_point(cpt,rad[1],nvec[0],ptc);
			break;
		case UA_BOTTOM_LEFT:
			um_translate_point(cpt,rad[2],nvec[0],ptc);
			break;
		case UA_TOP_CENTER:
			um_translate_point(cpt,rad[0],nvec[1],ptc);
			break;
		case UA_MIDDLE_CENTER:
			um_translate_point(cpt,rad[1],nvec[1],ptc);
			break;
		case UA_BOTTOM_CENTER:
			um_translate_point(cpt,rad[2],nvec[1],ptc);
			break;
		case UA_TOP_RIGHT:
			um_translate_point(cpt,rad[0],nvec[2],ptc);
			break;
		case UA_MIDDLE_RIGHT:
			um_translate_point(cpt,rad[1],nvec[2],ptc);
			break;
		case UA_BOTTOM_RIGHT:
			um_translate_point(cpt,rad[2],nvec[2],ptc);
			break;
		}
	}
}

/*********************************************************************
**    E_FUNCTION :  ua_save_text_pos()
**       Saves the character and text positions for annotation.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_save_text_pos()
{
	um_vctovc(Slast_letter,Slast_letter_save);
	um_vctovc(Slast_line,Slast_line_save);
}

/*********************************************************************
**    E_FUNCTION :  ua_restore_text_pos()
**       Restores ths saved the character and text positions for annotation.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_restore_text_pos()
{
	um_vctovc(Slast_letter_save,Slast_letter);
	um_vctovc(Slast_line_save,Slast_line);
}

/*********************************************************************
**    E_FUNCTION :  ua_reset_text_pos()
**       Resets ths saved the character and text positions for annotation
**       to 0,0,0.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_reset_text_pos()
{
	Slast_letter[0] = Slast_letter[1] = Slast_letter[2] = 0.;
	Slast_line[0] = Slast_line[1] = Slast_line[2] = 0.;
}

/*********************************************************************
**    I_FUNCTION :  S_calc_text_nextpos(note,txtattr,ptc,ptl)
**       Calculates the next character and line position for annotation
**       text string.
**    PARAMETERS   
**       INPUT  : 
**          note     Annotation record.
**          txtattr  Annotation attribute record.
**       OUTPUT :  
**          ptc      Next character position.
**          ptl      Next line position.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_calc_text_nextpos(note,txtattr,ptc,ptl)
struct UA_txt_rec *note;
struct UA_txtattr_rec *txtattr;
UU_REAL ptc[],ptl[];
{
	int relnum,site;
	UU_LOGICAL cirfl;
	UU_REAL rad[3],hgt,ang;
	UM_coord cpt,pt0,box[5];
	UM_vector nvec[3],vc1,vc2,tvec;
	UM_transf tf;
/*
.....Determine if text is on an arc
*/
	cirfl = UU_FALSE;
	if (note->arckey != 0)
	{
		if (ur_retrieve_data_relnum(note->arckey,&relnum) == UU_SUCCESS)
		{
			if (relnum == UM_CIRCLE_REL) cirfl = UU_TRUE;
		}
	}
/*
.....Return the next character/line position
.....based on the initial text position site
*/
	if (!cirfl)
	{
		uai_calc_text_box(note,UM_DEFAULT_TF,txtattr,box);
		site = txtattr->entity_site;
/*
		if (txtattr->path == 1)
		{
			if (site == UA_TOP_LEFT) site = UA_TOP_RIGHT;
			else if (site == UA_MIDDLE_LEFT) site = UA_MIDDLE_RIGHT;
			else if (site == UA_BOTTOM_LEFT) site = UA_BOTTOM_RIGHT;
		}
*/
		switch (site)
		{
		case UA_TOP_LEFT:
			um_vctovc(box[1],ptc);
			um_vctovc(box[3],ptl);
			break;
		case UA_MIDDLE_LEFT:
			um_vcmnvc(box[1],box[2],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[2],vc2,ptc);

			um_vcmnvc(box[3],box[0],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[3],vc2,ptl);
			break;
		case UA_BOTTOM_LEFT:
			um_vctovc(box[2],ptc);
	
			um_vcmnvc(box[3],box[0],vc1);
			um_vcplvc(box[3],vc1,ptl);
			break;
		case UA_TOP_CENTER:
			um_vcmnvc(box[1],box[0],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[1],vc2,ptc);

			um_vcmnvc(box[2],box[3],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[3],vc2,ptl);
			break;
		case UA_MIDDLE_CENTER:
			um_vcmnvc(box[1],box[3],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[2],vc2,ptc);

			um_vcmnvc(box[2],vc2,ptl);
			break;
		case UA_BOTTOM_CENTER:
			um_vcmnvc(box[2],box[3],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[2],vc2,ptc);

			um_vcplvc(box[3],vc2,vc1);
			um_vcmnvc(box[2],box[1],vc2);
			um_vcplvc(vc1,vc2,ptl);
			break;
		case UA_TOP_RIGHT:
			um_vcmnvc(box[1],box[0],vc1);
			um_vcplvc(box[1],vc1,ptc);

			um_vctovc(box[2],ptl);
			break;
		case UA_MIDDLE_RIGHT:
			um_vcmnvc(box[1],box[2],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[2],vc2,vc1);
			um_vcmnvc(vc1,box[3],vc2);
			um_vcplvc(box[2],vc2,ptc);

			um_vcmnvc(box[2],box[1],vc1);
			um_vctmsc(vc1,.5,vc2);
			um_vcplvc(box[2],vc2,ptl);
			break;
		case UA_BOTTOM_RIGHT:
			um_vcmnvc(box[2],box[3],vc1);
			um_vcplvc(box[2],vc1,ptc);

			um_vcmnvc(box[2],box[1],vc1);
			um_vcplvc(box[2],vc1,ptl);
			break;
		}
/*
.....Swap next character/line positions
.....For up/down text path
*/
		if (txtattr->path > 1)
		{
			um_vctovc(ptc,vc1);
			um_vctovc(ptl,ptc);
			um_vctovc(vc1,ptl);
		}
	}
/*
.....Arc text
*/
	else
	{
		uai_calc_arctext_box(note,UM_DEFAULT_TF,txtattr,cpt,nvec,rad);
		pt0[0] = pt0[1] = pt0[2] = 0.;
		ang = um_angle2p(nvec[0],nvec[2],txtattr->plane);
		um_rotlntf(pt0,txtattr->plane,ang,tf);
		hgt = rad[0] - rad[2];
		switch (txtattr->entity_site)
		{
		case UA_TOP_LEFT:
			um_translate_point(cpt,rad[0],nvec[2],ptc);
			um_translate_point(cpt,rad[2],nvec[0],ptl);
			break;
		case UA_MIDDLE_LEFT:
			um_translate_point(cpt,rad[1],nvec[2],ptc);
			um_translate_point(cpt,rad[1]-hgt,nvec[0],ptl);
			break;
		case UA_BOTTOM_LEFT:
			um_translate_point(cpt,rad[2],nvec[2],ptc);
			um_translate_point(cpt,rad[2]-hgt,nvec[0],ptl);
			break;
		case UA_TOP_CENTER:
			um_vctmtf(nvec[1],tf,tvec);
			um_translate_point(cpt,rad[0],tvec,ptc);
			um_translate_point(cpt,rad[2],nvec[1],ptl);
			break;
		case UA_MIDDLE_CENTER:
			um_vctmtf(nvec[1],tf,tvec);
			um_translate_point(cpt,rad[1],tvec,ptc);
			um_translate_point(cpt,rad[1]-hgt,nvec[1],ptl);
			break;
		case UA_BOTTOM_CENTER:
			um_vctmtf(nvec[1],tf,tvec);
			um_translate_point(cpt,rad[2],tvec,ptc);
			um_translate_point(cpt,rad[1]-hgt,nvec[1],ptl);
			break;
		case UA_TOP_RIGHT:
			um_vctmtf(nvec[2],tf,tvec);
			um_translate_point(cpt,rad[0],tvec,ptc);
			um_translate_point(cpt,rad[2],nvec[2],ptl);
			break;
		case UA_MIDDLE_RIGHT:
			um_vctmtf(nvec[2],tf,tvec);
			um_translate_point(cpt,rad[1],tvec,ptc);
			um_translate_point(cpt,rad[1]-hgt,nvec[2],ptl);
			break;
		case UA_BOTTOM_RIGHT:
			um_vctmtf(nvec[2],tf,tvec);
			um_translate_point(cpt,rad[2],tvec,ptc);
			um_translate_point(cpt,rad[1]-hgt,nvec[2],ptl);
			break;
		}
	}
/*
.....Store normal vectors
*/
	um_vctovc(txtattr->plane,&ptc[3]);
	um_vctovc(txtattr->plane,&ptl[3]);
}

/*********************************************************************
**    I_FUNCTION :  S_calc_text_projpos(note,txtattr,atfl,ptc)
**       Calculates the projection attach point for an annotation
**       text string.
**    PARAMETERS   
**       INPUT  : 
**          note     Annotation record.
**          txtattr  Annotation attribute record.
**          atfl     Attach point flag.
**       OUTPUT :  
**          ptc      Projection position.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_calc_text_projpos(note,txtattr,atfl,ptc)
struct UA_txt_rec *note;
struct UA_txtattr_rec *txtattr;
int atfl;
UU_REAL ptc[];
{
	int isite;
/*
.....Determine projection location based on attach flag
*/
	switch (atfl)
	{
/*
........START
........Use Projection Origin
*/
	case 1:
		isite = txtattr->entity_site;
		break;
/*
........MIDDLE
........Use center of text along origin height
*/
	case 2:
		switch (txtattr->entity_site)
		{
		case UA_TOP_LEFT:
		case UA_TOP_CENTER:
		case UA_TOP_RIGHT:
			isite = UA_TOP_CENTER;
			break;
		case UA_MIDDLE_LEFT:
		case UA_MIDDLE_CENTER:
		case UA_MIDDLE_RIGHT:
			isite = UA_MIDDLE_CENTER;
			break;
		case UA_BOTTOM_LEFT:
		case UA_BOTTOM_CENTER:
		case UA_BOTTOM_RIGHT:
			isite = UA_BOTTOM_CENTER;
			break;
		}
		break;
/*
........END
........Use end of text along origin height
*/
	case 3:
		switch (txtattr->entity_site)
		{
		case UA_TOP_LEFT:
		case UA_TOP_CENTER:
		case UA_TOP_RIGHT:
			isite = UA_TOP_RIGHT;
			break;
		case UA_MIDDLE_LEFT:
		case UA_MIDDLE_CENTER:
		case UA_MIDDLE_RIGHT:
			isite = UA_MIDDLE_RIGHT;
			break;
		case UA_BOTTOM_LEFT:
		case UA_BOTTOM_CENTER:
		case UA_BOTTOM_RIGHT:
			isite = UA_BOTTOM_RIGHT;
			break;
		}
		break;
/*
........CENTER
........Use center of text
*/
	case 4:
		isite = UA_MIDDLE_CENTER;
		break;
	}
/*
.....Calculate text attach point
*/
	ua_calc_text_attach(note,txtattr,isite,ptc);
}

/*********************************************************************
**    I_FUNCTION :  S_calc_text_origin(note,txtattr)
**       Adjusts the note's origin based on the attach position.
**    PARAMETERS   
**       INPUT  : 
**          note     Annotation record.
**          txtattr  Annotation attribute record.
**       OUTPUT :  
**          note.position      Actual attach position.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_calc_text_origin(note,txtattr)
struct UA_txt_rec *note;
struct UA_txtattr_rec *txtattr;
{
	int relnum,status;
	struct UM_circle_rec arc;
	struct UM_line_rec tline;
/*
.....Calculate origin for standard text
*/
	if (note->arckey == 0)
		ua_txt_origin(note,txtattr,UU_FALSE);
	else
	{
		ur_retrieve_data_relnum(note->arckey,&relnum);
/*
.....Calculate origin for text on an arc
*/
		if (relnum == UM_CIRCLE_REL)
		{
			arc.key = note->arckey;
			status = uc_retrieve_data(&arc,sizeof(arc));
			uai_txtinfo_on_arc(note,txtattr,note->position,&arc,
				(UM_PLOCREC *)UU_NULL,UU_FALSE);
		}
/*
.....Calculate origin for text on a line
*/
		else if (relnum == UM_LINE_REL)
		{
			tline.key = note->arckey;
			status = uc_retrieve_data(&tline,sizeof(tline));
			uai_txtinfo_on_line(note,txtattr,note->position,&tline,
				(UM_PLOCREC *)UU_NULL,UU_FALSE);
		}
	}
}
