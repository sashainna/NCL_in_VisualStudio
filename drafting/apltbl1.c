/*********************************************************************
**    NAME         : apltbl1.c
**       CONTAINS:
**    			ua_pl_draw
**    			ua_drw_pl_text
**    			ua_drw_pl_text_boxes
**    			ua_write_pl
**    			ua_read_pl
**    			ua_get_next_pl_string
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       apltbl1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:37
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]=	{
	"@(#) apltbl1.c 3.1 3/24/89 17:15:08 single"	};
#else
static char uu_sccsident[]=	{
	"@(#) apltbl1.c 3.1 3/24/89 17:15:08 double"	};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"
#include "uhep.h"
#include "class.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mdcoord.h"
#include "mdmatrix.h"
#include "mcrv.h"
#include "modef.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mdeval.h"
#include "mdattr.h"
#include "mattr.h"
#include "msol.h"
#include "mxxx.h"
#include "mdraw.h"
#include "mdebug.h"
#include "atext.h"
#include "go.h"
#include "gtbl.h"	
#include "ginqdsiz.h"
#include "ginqatt.h"
#include "ginqatt2.h"
#include "xfsys1.h"
#include <ctype.h>

/* PARTS LIST table sizes */
extern UU_REAL UA_line_hgt;
extern UU_REAL UA_pl_title;
extern UU_REAL UA_pl_sub_title;
extern UU_REAL UA_pl_char_hgt;
extern UU_REAL UA_pl_title_char_hgt;
extern UU_REAL UA_pl_subtitle_char_hgt;
extern UU_REAL UA_line_wdth[7];
extern int     UA_max_no_entries;

extern int UD_editable[UD_NMENTWD];

extern char	*uu_malloc();
/*********************************************************************
**    E_FUNCTION     :  ua_pl_draw(pl, tfmat)
**     Draw a PARTS LIST text lines
**    PARAMETERS   
**       INPUT  : 
**          pl									PARTS LIST structure	
**				tfmat								drawing transformation
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_pl_draw(pl, tfmat)

struct UA_generic_draft	*pl;
UM_transf 					tfmat;

	{
	UU_REAL 						scale, wdth[7];
	UM_coord 					location, x_off_set, y_off_set;
	int 							i, j, k;
	struct UA_txt_rec  		note;
	struct UA_txtattr_rec   note_attr;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_pl_draw "));

	/* scale table size values */
	for(i=0;i<pl->asso_blk_use;i++)
		{
		if(pl->asso_blk[i].modifier == -99)
			{
			scale = pl->asso_blk[i].location[0];
			break;
			}
		}

	for(i=0;i<7;i++)
		{
		wdth[i] = UA_line_wdth[i]/scale;
		}

	/* draw individual lines of text */
	for(i=0;i<pl->asso_blk_use;i++)
		{

		/* retrieve text entity */
		note.key = pl->asso_blk[i].key;
		if(ua_get_text(&note, sizeof(struct UA_txt_rec)) == UU_SUCCESS)
			{
			note_attr.key = note.key;
			if(ur_retrieve_attr(&note_attr) == UU_SUCCESS)
				{

				/* make each line of text separately selectable */
				gspickid(note.key);

				/* select appropriate drawing routine depending on the type of 
					text - table entry or table title */
				if(pl->asso_blk[i].modifier >= 0)
					{
					if(UA_text_box_ovrd == 0)
						ua_drw_pl_text(&note, tfmat, &note_attr, wdth);
					else
						ua_drw_pl_text_boxes(&note, tfmat, &note_attr, wdth);
					}
				else
					ua_draw_text(&note, tfmat, &note_attr);
				}
			}
		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  int ua_drw_pl_text( eptr, tfmat, attr, wdth)
**			Draw the PARTS LIST table entry text.
**    PARAMETERS   
**       INPUT  : 
**				eptr					pointer to text entity
**				tfmat					transformation matrix 
**				attr					pointer to attribute bundle
**				wdth					array of character off-set values
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_drw_pl_text(eptr, tfmat, attrptr, wdth)

	struct UA_txt_rec 		*eptr;
	UM_transf 					tfmat;
	struct UA_txtattr_rec 	*attrptr;
	UU_REAL 						wdth[7];

	{
	UM_coord 	char_position;
	UM_vector 	char_up_vector, char_pln_norm, xaxis, yaxis, zaxis;
	UU_REAL 		char_height, char_exp;
	UU_REAL  	scale;
	Gerror   	gstatus;
	Gtxfp    	fp, *pfp, save_fp;
	Gchrexp  	expn, save_exp;
	Gchrsp   	spacing, save_spacing;
	Gcolor   	color, save_color;
	Gchrht   	height, save_height;
	Gwpoint3 	up, save_up, plane, *save_plane, position;
	Gwpoint  	*pup;
	Gtxpath  	path, save_path;
	Gtxalign 	align, *palign, save_align;
	int			i, ind, column;
	char			*chptr, tstring[200];
	UM_coord 	tloc, sys_origin, sys_axis[3], dist;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_drw_pl_text(eptr->key:%d,tfmat:%x,attrptr:%x)",
						eptr->key, tfmat, attrptr));

	/* determine the character height, text plane normal, text up vector,
		and character postion */
	if (!um_is_idmat(tfmat))
		{
		um_unitvc(attrptr->up, yaxis);
		um_unitvc(attrptr->plane, zaxis);
		um_cross(yaxis, zaxis, xaxis);

		um_vctmtf(attrptr->up, tfmat, char_up_vector);
		um_vctmtf(attrptr->plane, tfmat, char_pln_norm);
		um_cctmtf(eptr->position, tfmat, char_position);

		um_vctmtf(xaxis, tfmat, xaxis);
		scale = um_mag(char_up_vector);
		char_height = attrptr->height * um_mag(char_up_vector);
		char_exp = um_mag(xaxis) / scale;
		}
	else
		{
		um_vctovc(attrptr->up, char_up_vector);
		um_vctovc(attrptr->plane, char_pln_norm);
		um_vctovc(eptr->position, char_position);
		char_height = attrptr->height;
		char_exp = attrptr->expn;
		}
	um_cross(char_up_vector, char_pln_norm, xaxis);
	um_unitvc(xaxis, xaxis);

/*-------- save the DIGS text attributes and set to new state --------------*/
	pfp = gqtextfp();          
	save_fp.font = pfp-> font;
	save_fp.prec = pfp-> prec;
	fp.font = attrptr->font;       
 	fp.prec = (Gtxprec)attrptr->prec;
 	gstextfp(&fp);

	save_color = gqtextcolor();	
	color = attrptr->color;
	gstextcolor(color);

	save_exp = gqcharexp();  
 	gscharexp(char_exp);

	save_spacing = gqcharspace(); 
 	spacing = attrptr->spacing;
	gscharspace(spacing);

	save_height = gqcharheight(); 
 	gscharheight(char_height);

	save_path = gqtextpath();
	path = (Gtxpath)attrptr->path;          
 	gstextpath(path);

	palign = gqtextalign();
	save_align.hor = palign->hor;
	save_align.ver = palign->ver;
	align.hor = (Gtxhor)attrptr->align_hor;  
	align.ver = (Gtxver)attrptr->align_ver;
	gstextalign(&align);

	save_plane = gqtxplane();
	um_vctovc(char_pln_norm, &plane);  
 	gstxplane(&plane);
	pup = gqcharup();          
	um_vctovc(pup, &save_up);
 	um_vctovc(char_up_vector, &up);  
 	gscharup3(&up);

	/* rotate construction upvec if have text angle */
	if (eptr->tangle != 0.0)
  	  {
		um_getcpln(sys_origin,sys_axis[0],sys_axis[1],sys_axis[2]);
		um_setcpln_yaxis(attrptr->up);	
  	  }

	/* draw the text */
	um_vctovc(char_position, &position);
	chptr = eptr->tchar;
	ind = 0;
	column = 0;
	while (ind < eptr->no_tchar)
  	  {
			/* strip out the ; between columns  */
		for (i=0; ((*chptr!=';')&&(ind<eptr->no_tchar)); ind++,i++)
	  	  {
			tstring[i] = *chptr;
			chptr++;
	  	  }
		ind++;
		chptr++;
		tstring[i] = '\0';
   	if(i > 0) gtext(&position, tstring);

		/* update new location for the text (depends on column) */
		if (ind < eptr->no_tchar)
	  	  {
		  um_vctmsc(xaxis, wdth[column], dist);
		  um_vcplvc(&position, dist, &position);
		  column++;
	     }
     }

/*-------- reset DIGS text  attributes to previous state -------------------*/
gstextfp(&save_fp);
gstextcolor(save_color);
gscharexp(save_exp);
gscharspace(save_spacing);
gscharheight(save_height);
gscharup(&save_up);
gstextpath(save_path);
gstextalign(&save_align);
gstxplane(save_plane);
if (eptr->tangle != 0.0)
	um_setcpln_yaxis(sys_axis[1]);

uu_dexit;
}	
/*********************************************************************
**    E_FUNCTION :  int ua_drw_pl_text_boxes( eptr, tfmat, attr, wdth)
**       Draw a box around the text primitive pointed to by eptr.
**    PARAMETERS   
**       INPUT  : 
**				eptr					pointer to text entity
**				tfmat					transformation matrix 
**				attrptr				pointer to attribute bundle
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_drw_pl_text_boxes(eptr, tfmat, attrptr, wdth)
	struct UA_txt_rec *eptr;          /* ptr to text data */
	UM_transf tfmat;
	struct UA_txtattr_rec *attrptr;
	UU_REAL 						wdth[7];

	{
	UM_coord char_position, position, dist;
	UM_vector char_up_vector, char_pln_norm, xaxis, yaxis, zaxis;
	UU_REAL char_dx, char_dy, scale, size, x_offset, y_offset, length;
	Glntype line_style;
	Gcolor   color, save_color;
	UM_coord del_x, del_y, corn[5], org_x, org_y;
	int status, i, ind, column, num;
	char *chptr, tstring[200];

	uu_denter(UU_STRC,(us,"ua_drw_pl_text_boxes"));

	if (!um_is_idmat(tfmat))
		{
		um_vctmtf(attrptr->up, tfmat, char_up_vector);
		um_vctmtf(attrptr->plane, tfmat, char_pln_norm);
		um_cctmtf(eptr->position, tfmat, char_position);
		scale = um_mag(char_up_vector);
		}
	else
		{
		um_vctovc(attrptr->up, char_up_vector);
		um_vctovc(attrptr->plane, char_pln_norm);
		um_vctovc(eptr->position, char_position);
		scale = 1.0;
		}
	um_cross(char_up_vector, char_pln_norm, xaxis);
	um_unitvc(char_up_vector, char_up_vector);
	um_unitvc(char_pln_norm, char_pln_norm);
	um_unitvc(xaxis, xaxis);
	size = attrptr->height*scale;
	char_dy = size;
	um_vctmsc(char_up_vector, char_dy, del_y);

	/* save and set text color */
	color = attrptr->color;
	gslinecolor(color);
	line_style.typeno = 0;
	line_style.npatn = 0;
	gslinetype(&line_style);
	gslinewidth((UU_REAL) 1.0);

	/* draw the text boxes */
	um_vctovc(char_position, corn[0]);
	um_vctovc(char_position, position);
	chptr = eptr->tchar;
	ind = 0;
	column = 0;
	while (ind < eptr->no_tchar)
  	  {
			/* strip out the ; between columns  */
		for (i=0; ((*chptr!=';')&&(ind<eptr->no_tchar)); ind++,i++)
	  	  {
			tstring[i] = *chptr;
			chptr++;
	  	  }
		ind++;
		chptr++;
		tstring[i] = '\0';
		num = strlen(tstring);
   	if(num > 0)
			{
			char_dx = num * size;
			um_vctmsc(xaxis, char_dx, del_x);

			x_offset = 0.0;
			y_offset = size;
		
			um_vctmsc(xaxis, x_offset, org_x);
			um_vcplvc(corn[0], org_x, corn[0]);
			um_vctmsc(char_up_vector, y_offset, org_y);
			um_vcplvc(corn[0], org_y, corn[0]);
		
			um_vcplvc(corn[0], del_x, corn[1]);
			um_vcmnvc(corn[1], del_y, corn[2]);
			um_vcmnvc(corn[2], del_x, corn[3]);
			um_vcplvc(corn[3], del_y, corn[4]);
		
			status = gpolyline3(5, corn);
		}

		/* update new location for the text (depends on column) */
		if (ind < eptr->no_tchar)
	  	  {
		  um_vctmsc(xaxis, wdth[column], dist);
		  um_vcplvc(position, dist, position);
		  um_vctovc(position, corn[0]);
		  column++;
	     }
     }

	uu_dexit;
	return (UU_SUCCESS);
}	

/*********************************************************************
**    E_FUNCTION     : ua_write_pl()
**       Write a ACSII file of the PARTS LIST table.
**    PARAMETERS   
**       INPUT  : 
**          none					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_write_pl()

	{
	struct UA_generic_draft		pl;
	struct UA_PLOCREC				plocrec;
	struct UA_PICKENT				pickent;
	UU_KEY_ID 						curr_key, key, save_key;
	int								i, j, k, count, d_status, num1, msg, num_entries,
										relation, num_chars, retstat, char_cnt, err,
										options, lu, status, mode, error, fenv;
	char								form[11], type[11], dataptr[11], interp[11],
										file_name[250], txtstr[255], cr[2];
	struct   UA_txt_rec			note;			/* text record */
	UU_LOGICAL 						init, ud_lyesno();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_write_pl()"));

	strcpy(cr, "\n");

	/* check the number of PARTS LIST tables in the data base */
	init = UU_TRUE;
	relation = 48;
	count = 0;

	/* loop over displayed drafting entities looking for PARTS LIST tables */
	while(uv_getalldispinrelation(init, relation, &key) == UU_SUCCESS)
		{
		pl.key = key;
		pl.rel_num = relation;
		status = uc_retrieve_data(&pl, sizeof(struct UA_generic_draft));
		if( status == UU_SUCCESS)
			{
			if(pl.etype == UA_PL_DIM)
				{
				count++;
				save_key = key;
				}
			}
		init = UU_FALSE;
		}
	if(count == 0)					/* no table in the data base - return */
		goto fexit;
	else if(count == 1)			/* single table - retrieve and continue */
		{
		pl.key = save_key;
		pl.rel_num = relation;
		status = uc_retrieve_data(&pl, sizeof(struct UA_generic_draft));
		}
	else					/* multiple tables in the data base - query user */
		{
	
		/* get user selection of the PARTS LIST table */
		ud_lgeo(UU_TRUE, UD_editable);
		d_status = ud_pick_loc(UA_DRAFTING, 149, &plocrec, 1, &num1, UU_FALSE);
		if(d_status == UA_REJECT || num1 < 1) goto fexit;
		d_status = um_d_pickresolve(&plocrec.ppick, 1, &pickent);
		curr_key = um_get_pickkey(&pickent, 1);
		pl.key = curr_key;
	
		/* check that the users actually selected a PARTS LIST table */
		d_status = um_retrieve_data_relnum(pl.key, &relation);
		if(  relation != UA_LINEAR_DIM )
			{
			uu_uerror0(UA_DRAFTING,44);
			goto fexit;
			}
		uc_retrieve_data(&pl, sizeof(struct UA_generic_draft	)) ;
		if(pl.etype != UA_PL_DIM)
			{
			uu_uerror0(UA_DRAFTING,44);
			goto fexit;
			}
		}

start:
	/* get  the report file name from the user */
	mode = UX_EXISTS;
	options = 1;
	status = ud_string(UA_DRAFTING,150,file_name,249,&char_cnt, UU_FALSE);
	if( ( char_cnt>0 ) )
		{

		/* append default label */
		strcat(file_name,".pl");

		/* check if file already exits */
		status = ux_access1(file_name,&mode,options);

		if(  status==UU_SUCCESS && mode==0  )
			{

			/* file already exits - check with user if we should overwrite */
			if( ud_lyesno(UA_DRAFTING, 156) )
				error = ux_delete(file_name,1);
			else
				goto fexit;
			}

		/* open the file for WRITE access */
		strcpy(type,"w+");
		strcpy(form,"STREAM");
		fenv = 0;
		mode = 0x1a4;
		strcpy(interp,"BINARY");
		strcpy(dataptr,"UX_NOHEADER");
		options = 1;
		error = ux_create_file(file_name,mode,fenv,form,interp,dataptr,
										&lu,options);

		/* retrieve table data and output in ASCII format */
		if(  error==UU_SUCCESS  )
			{

			/* first output number of entries */
			sprintf(txtstr,"%d\n", pl.asso_blk_use - 8);
			char_cnt = strlen(txtstr);
			err = ux_write(lu,txtstr,1,&char_cnt,options);

			/* next output each line of the table */
			for(i=0;i<pl.asso_blk_use;i++)
				{
				if(pl.asso_blk[i].modifier != -99)
					{
					note.key = pl.asso_blk[i].key;
					uc_retrieve_data(&note, sizeof(struct UA_txt_rec	)) ;
					strcpy(txtstr, note.tchar);
					strcat(txtstr, cr);
					char_cnt = strlen(txtstr);
					err = ux_write(lu,txtstr,1,&char_cnt,options);
					}
				}

			/* close the file */
			ux_close(lu,options);
			}
		}

fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_read_pl()
**       Read a ACSII file and create a PARTS LIST table.
**    PARAMETERS   
**       INPUT  : 
**          none					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_read_pl()

	{
	struct UA_generic_draft		*pl;
	struct   UA_txt_rec			*note;
	struct	UA_txtattr_rec		*txtattr;
	UU_KEY_ID 						curr_key, title_keys[10];
	int								i, j, k, count, d_status, num1, msg, num_entries,
										relation, num_chars, retstat, char_cnt, err, indx,
										find_no[1000], options, lu, status, mode, error,
										fenv, number, find_t, len;
	char								form[11], type[11], dataptr[11], interp[11],
										file_name[250], *textbuf, txtstr[255], cr[2],
										b_field[7][100];
	UM_coord							cpln_origin, xaxis, yaxis, zaxis, entity_origin,
										location, y_off_set, x_del, y_del; 
	UU_REAL 							scale, hgt, length, char_del;
	UU_TRUEDOUBLE        ff, uai_atof();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_read_pl()"));

	strcpy(cr, "\n");

start:

	/* allocate space for local variables */
	pl  = (struct UA_generic_draft *) uu_malloc(sizeof(struct UA_generic_draft));
	note  = (struct UA_txt_rec *) uu_malloc(sizeof(struct UA_txt_rec));
	txtattr = (struct UA_txtattr_rec *) uu_malloc(sizeof(struct UA_txtattr_rec));
	textbuf =  uu_malloc(20000*sizeof(char));

	/* get report file name from the user */
	mode = UX_EXISTS;
	options = 1;
	status = ud_string(UA_DRAFTING,150,file_name,249,&char_cnt, UU_FALSE);
	if( ( char_cnt>0 ) )
		{

		/* append standard label */
		strcat(file_name, ".pl");

		/* check status of the file */
		status = ux_access1(file_name,&mode,options);

		if(  status==UU_SUCCESS && mode==0  )
			{

			/* file ok; open for READ */
			strcpy(type,"r+");
			strcpy(form,"STREAM");
			strcpy(interp,"BINARY");
			options = 1;
			error = ux_open_to_data(file_name,type,form,interp,&lu, options);
			if(  error == UU_SUCCESS )
				{

				/* successfully opened the file - now read contents */
				number = 1;
				error = ux_read(lu,textbuf,20000,&number,options);
				number = strlen(textbuf);
				textbuf[number] = '\0';
				if(  error == UU_SUCCESS || error == UX_EOF  )
					{

					/* check number of table entries */
					j = 0;
					ua_get_next_pl_string(textbuf, &j, txtstr);
					sscanf(txtstr,"%d", &num_entries);
					if(num_entries > UA_max_no_entries )
						{
						uu_uerror0(UA_DRAFTING, 56);
						goto fexit;
						}

					/* check FIND NUMBER and QUANTITY fields */
					indx = 0;
					for(i=0;i<num_entries;i++)
						{
						ua_get_next_pl_string(textbuf, &j, txtstr);
						num_chars = strlen(txtstr);
						ua_strip_pl_entry(txtstr, &num_chars, b_field);
						len = strlen(b_field[0]);
						if(len > 3)
							{
							uu_uerror0(UA_DRAFTING,45);
							ux_close(lu,options);
							goto fexit;
							}
						for(k=0;k<len;k++)
							{
							if(isdigit(b_field[0][k]) == 0)
								{
								uu_uerror0(UA_DRAFTING,52);
								ux_close(lu,options);
								goto fexit;
								}
							}
	
						/* check quantity field */
						len = strlen(b_field[5]);
						if(len > 0)
							{
							if(len > 3)
								{
								uu_uerror0(UA_DRAFTING,57);
								ux_close(lu,options);
								goto fexit;
								}
							if(strcmp(b_field[5] ,"AR") != 0)
								{
								for(k=0;k<len;k++)
									{
									if(isdigit(b_field[5][k]) == 0)
										{
										uu_uerror0(UA_DRAFTING,58);
										ux_close(lu,options);
										goto fexit;
										}
									}
								}
							}

						/* check for duplicate find no's */
						ff = uai_atof(b_field[0]);
						if(i == 0)
							{
							find_no[0] = ff;
							indx = 1;
							}
						else
							{
							find_t = ff;
							for(k=0;k<indx;k++)
								{
								if(find_no[k] == find_t)
									{
									uu_uerror0(UA_DRAFTING,51);
									ux_close(lu,options);
									goto fexit;
									}
								}
							find_no[indx] = find_t;
							indx++;
							}
						}

					/* successfully read file - now create a PARTS LIST table */
					ua_init_entity(UA_PL_DIM,1,pl);
					pl->txt_just = UA_LEFT;
					pl->entity_site = UA_MIDDLE_CENTER;
					ua_getcpln(pl,cpln_origin,xaxis,yaxis,zaxis);
				
					/* get lower left corner from user */
					d_status = ud_world_coord(UA_DRAFTING, 142, location, 1,
														&num1, UU_FALSE);
					um_vctovc(location, pl->dim_origin);
					if( d_status==0 || d_status==2 ) goto fexit;
					if(  num1==0 )
						{
						d_status = 0;
						goto fexit;
						}

					/* get number of table entries */
					j = 0;
					ua_get_next_pl_string(textbuf, &j, txtstr);
					sscanf(txtstr,"%d", &num_entries);

					/* generate box and titles */
					um_get_drwscale(&scale);
					ua_pl_tbl_box(pl, num_entries, cpln_origin, xaxis,
										yaxis, zaxis,scale);
					ua_pl_titles(pl, cpln_origin, xaxis, yaxis, zaxis,
										title_keys,scale);

					/* initilize off-set data */
					hgt = UA_line_hgt /scale;
					char_del = 0.8*((hgt - UA_pl_char_hgt/scale)/2.0);
					um_vctmsc(xaxis, char_del, x_del);
					um_vctmsc(yaxis, char_del, y_del);
					char_del = (UA_pl_title + UA_pl_sub_title)/scale;

					/* loop on number of entries */
					for(i=0;i<num_entries;i++)
						{
						length = i*hgt + char_del;
						um_vctmsc(yaxis, length, y_off_set);
						um_vcplvc(pl->dim_origin, y_off_set, location);
						um_vcplvc(location, x_del, location);
						um_vcplvc(location, y_del, location);
						ua_get_next_pl_string(textbuf, &j, txtstr);
						num_chars = strlen(txtstr);
						
						/* create new text entity	*/
						ur_setup_data(UA_TEXT_REL,note,sizeof(struct UA_txt_rec));
						ur_setup_data(UA_TEXTATTR_REL,txtattr,
											sizeof(struct UA_txtattr_rec));
						ua_init_txtrec(note,txtattr,&UA_txtattr,UU_FALSE);
					
						note->no_tchar = num_chars;
						strcpy(note->tchar,txtstr);
						um_vctovc(location, note->position);
						if(num_chars == 1)
							{
							note->no_tchar++;
							strcat(note->tchar," ");
							}
						 
						txtattr->entity_site = UA_BOTTOM_LEFT;
						txtattr->height = UA_pl_char_hgt/scale;
						ua_txt_origin(note,txtattr,UU_FALSE);
					
						txtattr->displayable = UM_NEVERDISPLAYABLE;
						uc_create_data(note,UM_DEFAULT_TF,txtattr);
						pl->asso_blk[i].key = note->key;
						}

					pl->asso_blk_use = num_entries + 8;
					for(i=0;i<8;i++)
						{
						pl->asso_blk[num_entries+i].key = title_keys[i];
						pl->asso_blk[num_entries+i].modifier = -99;
						pl->asso_blk[num_entries+i].location[0] = scale;
						}
				
					ua_create_entity(pl, &curr_key);
					uc_display(pl);
					}
				ux_close(lu,options);
				}
			}
		else
			{
			uu_uerror1(UA_DRAFTING,37,file_name);
			}
		}

fexit:
	uu_free(pl);
	uu_free(note);
	uu_free(txtattr);
	uu_free(textbuf);
	uu_dexit;
	}
	 
/*********************************************************************
**    E_FUNCTION     :  ua_get_next_pl_string(buf, j, str)
**      Get next string from the PARTS LIST table input buffer.
**    PARAMETERS   
**       INPUT  : 
**          buf								text buffer
**				j									current index into the buffer
**       OUTPUT :  
**          str								new string
**				j									updated index
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_get_next_pl_string(buf, j, str)
	char *buf, str[250];
	int *j;
	{
	int i, char_loc;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_get_next_pl_string"));

	char_loc = *j;
	for(i=0;i<20000;i++)
		{
		if(buf[char_loc] == '\n' || buf[char_loc] == '\0')
			{
			char_loc++;
			str[i] = '\0';
			goto fexit;
			}
		else
			{
			str[i] = buf[char_loc];
			char_loc++;
			}
		}

fexit:;
	*j = char_loc;
	uu_dexit;
	}
