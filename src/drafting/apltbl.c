/*********************************************************************
**    NAME         : apltbl.c
**       CONTAINS:
**     	      ua_pl_tbl
**    			ua_pl_tbl_box
**					ua_pl_titles
**    			ua_pl_tbl_entry
**    			ua_get_pl_text
**    			ua_check_pl_quantity
**    			ua_strip_pl_entry
**    			ua_update_pl_count_field
**    			ua_ua_get_pl_text_form
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       apltbl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:37
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]=	{
	"@(#) apltbl.c 3.3 3/24/89 08:26:29 single"	};
#else
static char uu_sccsident[]=	{
	"@(#) apltbl.c 3.3 3/24/89 08:26:29 double"	};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "ddef.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfmracs.h"
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
UU_REAL UA_line_hgt = .35;
UU_REAL UA_pl_title = .4;
UU_REAL UA_pl_sub_title = .6;
UU_REAL UA_pl_char_hgt = .156;
UU_REAL UA_pl_title_char_hgt = .187;
UU_REAL UA_pl_subtitle_char_hgt = .125;
UU_REAL UA_line_wdth[7] = {.55, 2.75, .8, 3.35, 2.0, .65, .38};
int     UA_max_no_entries = 40;
UU_LOGICAL     UA_pl_table_entry_method = UU_TRUE;

extern int UD_editable[UD_NMENTWD];

/* local data */
static UU_REAL char_count[7] = {1.5, 5.0, 2.0, 5.0, 4.0, 1.5, 1.0};
static char    *form_msg = {"Current Table input mode is via a FORM"};
static char    *str_msg = {"Current Table input mode is via a STRING"};

/*********************************************************************
**    E_FUNCTION     : ua_pl_tbl()
**      User interaction routine for the creation of a PARTS LIST table
**    PARAMETERS   
**       INPUT  : 
**          none					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_pl_tbl()
	{
	struct UA_generic_draft		pl;
	UU_KEY_ID 						curr_key, title_keys[10];
	int								i, j, k, count, d_status, num1, msg, num_entries
										, find_no_index, find_no[500];
	UM_coord							cpln_origin, xaxis, yaxis, zaxis, entity_origin,
										location, y_off_set, x_del, y_del; 
	UU_REAL 							scale, hgt, length, char_del;
	UU_LOGICAL						first_time, redo, ok;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_pl_tbl()"));

	/* inform use of current format */
	if(UA_pl_table_entry_method)
		ud_prmerr(form_msg);
	else
		ud_prmerr(str_msg);

	/* initilize some local variables */
	first_time = UU_TRUE;
	count = 0;

table_org:;
	/* get lower left corner from the user */
	d_status = ud_world_coord(13, 142, location, 1, &num1, UU_FALSE);
	if( d_status == UU_TRUE && num1 == 0 ) goto fexit;

	/* switch on user response */
	switch(d_status)
		{
		case UU_FALSE:
			goto fexit;
			break;
		case UU_ALTACTION:
			UA_pl_table_entry_method = !UA_pl_table_entry_method;
			if(UA_pl_table_entry_method)
				ud_prmerr(form_msg);
			else
				ud_prmerr(str_msg);
			goto table_org;
			break;
		default:
			ua_init_entity(UA_PL_DIM, 1, &pl);
			pl.txt_just = UA_LEFT;
			pl.entity_site = UA_MIDDLE_CENTER;
			ua_getcpln(&pl, cpln_origin, xaxis, yaxis, zaxis);
			um_vctovc(location, pl.dim_origin);

			/* get the number of elements in the table */
			msg = 143;
			d_status = ud_integer(UA_DRAFTING,msg,&num_entries,1,&num1,UU_FALSE);
			if( ( d_status!=UA_OPCOMPLETE ) )
				goto fexit;
			if( num_entries<1 || num_entries > UA_max_no_entries )
				{
				uu_uerror0(UA_DRAFTING,56);
				goto fexit;
				}
		
			/* generate table outline and titles */
			um_get_drwscale(&scale);
			ua_pl_tbl_box(&pl, num_entries, cpln_origin, xaxis, yaxis, zaxis, scale);
			ua_pl_titles(&pl, cpln_origin, xaxis, yaxis, zaxis, title_keys, scale);
		
			/* loop on number of entries */
			hgt = UA_line_hgt /scale;
			char_del = 0.8*((hgt - UA_pl_char_hgt/scale)/2.0);
			um_vctmsc(xaxis, char_del, x_del);
			um_vctmsc(yaxis, char_del, y_del);
			char_del = (UA_pl_title + UA_pl_sub_title)/scale;
		
			find_no_index = 0;
			for(i=0;i<num_entries;i++)
				{
				 
				/* compute each entry off-set from the bottom of the table */
				length = i*hgt + char_del;
				um_vctmsc(yaxis, length, y_off_set);
				um_vcplvc(pl.dim_origin, y_off_set, location);
				um_vcplvc(location, x_del, location);
				um_vcplvc(location, y_del, location);
		
				/* get the text for the current entry and create a note entity */
				ua_pl_tbl_entry(&pl, i, location, scale, &find_no_index, find_no, &curr_key);
		
				/* add note key to list of entities associated with the table */
				if( curr_key != -1 )
					pl.asso_blk[i].key = curr_key;
				else
					{
					for(j=0;j<8;j++)
						uc_delete(title_keys[j]);
					for(j=0;j<i;j++)
						uc_delete(pl.asso_blk[j].key);
					goto fexit;
					}
				}
		
			/* add the table titles to the list of associated entities */
			pl.asso_blk_use = num_entries + 8;
			for(i=0;i<8;i++)
				{
				pl.asso_blk[num_entries+i].key = title_keys[i];
				pl.asso_blk[num_entries+i].modifier = -99;
				pl.asso_blk[num_entries+i].location[0] = scale;
				}
		
			/* create and display the table */
			ua_create_entity(&pl, &curr_key);
			uc_display(&pl);
		}


fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     :  ua_pl_tabl_box(pl, num_entries, origin, xaxis, yaxis
**														zaxis, scale)
**     Create lines for PARTS LIST table outline
**    PARAMETERS   
**       INPUT  : 
**          pl									PARTS LIST data structure	
**          num_entries						number of entries
**				origin,xaxis,yaxis,zaxis   plane on which the table is defined
**				scale 							current drawing scale
**       OUTPUT :  
**          pl									updated record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_pl_tbl_box(pl, num_entries, origin, xaxis, yaxis, zaxis, scale)

struct UA_generic_draft		*pl;
int								num_entries;
UM_coord 						origin, xaxis, yaxis, zaxis;
UU_REAL 							scale;

	{
	UU_REAL 		hgt, wdth[7], length, width, title_hgt, sub_title_hgt;
	UM_coord 	location, location1, x_off_set, y_off_set, del;
	int			i, j, k, m, n_pts;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_pl_tbl_box "));

	/* scale table size values with the drawing scale */
	/* this is required if the table is to project to a drawing correctly */
	hgt = UA_line_hgt /scale;
	title_hgt = UA_pl_title /scale;
	sub_title_hgt = UA_pl_sub_title /scale;
	width = 0.0;
	for(i=0;i<7;i++)
		{
		wdth[i] = UA_line_wdth[i]/scale;
		width = width + wdth[i];
		}

	/* compute total size of the table */
	length = num_entries * hgt + title_hgt + sub_title_hgt;

	/* compute off-set vectors for x and y */
	um_vctmsc(yaxis, length, y_off_set);
	um_vctmsc(xaxis, width, x_off_set);
	um_vctovc(pl->dim_origin, location);

	/* fill in the default line block parameters */
	m = 0; n_pts = 0;
	pl->line_blk[m].subtype = dim_line;
	pl->line_blk[m].line.line_font = UA_dim_line_font;
	pl->line_blk[m].line.line_density = UA_dim_line_dens;
	pl->line_blk[m].line.color = UA_dim_line_color;

	/* fill in the line segments for the table outline */
	um_vctovc(location, pl->line_blk[m].line_seg[0]);
	um_vcplvc(location, y_off_set, location);
	um_vctovc(location, pl->line_blk[m].line_seg[1]);
	um_vctovc(location, pl->line_blk[m].line_seg[2]);
	um_vcplvc(location, x_off_set, location);
	um_vctovc(location, pl->line_blk[m].line_seg[3]);
	um_vctovc(location, pl->line_blk[m].line_seg[4]);
	um_vcmnvc(location, y_off_set, location);
	um_vctovc(location, pl->line_blk[m].line_seg[5]);
	um_vctovc(location, pl->line_blk[m].line_seg[6]);
	um_vcmnvc(location, x_off_set, location);
	um_vctovc(location, pl->line_blk[m].line_seg[7]);

	/* fill in the lines for the column separation */
	length = num_entries*hgt + sub_title_hgt;
	um_vctmsc(yaxis, length, y_off_set);
	um_vctmsc(yaxis, (title_hgt), del);
	um_vcplvc(location, del, location);
	for(i=0,k=8;i<6;i++,k++,k++)
		{
		um_vctmsc(xaxis, wdth[i], x_off_set);
		um_vcplvc(location,x_off_set, location);
		um_vctovc(location, pl->line_blk[m].line_seg[k]);
		if(k == 10 || k == 14 || k == 18)
			um_vcmnvc(location, y_off_set, location);
		else
			um_vcplvc(location, y_off_set, location);
		um_vctovc(location, pl->line_blk[m].line_seg[k+1]);
		}

	/* fill in the line blocks for the table title */
	um_vctovc(pl->dim_origin, location);
	um_vctmsc(xaxis, width, x_off_set);
	um_vctmsc(yaxis, hgt, y_off_set);
	um_vctmsc(yaxis, title_hgt, del);
	um_vcplvc(location, del, location);
	um_vctovc(location, pl->line_blk[m].line_seg[20]);
	um_vcplvc(location, x_off_set, location1);
	um_vctovc(location1, pl->line_blk[m].line_seg[21]);

	/* fill int the line block for the column titles */
	um_vctmsc(yaxis, sub_title_hgt, del);
	um_vcplvc(location, del, location);
	um_vctovc(location, pl->line_blk[m].line_seg[22]);
	um_vcplvc(location, x_off_set, location1);
	um_vctovc(location1, pl->line_blk[m].line_seg[23]);
	n_pts = 24;
	for(i=0;i<(num_entries-1);i++)
		{
		if(n_pts > 46)
			{
			pl->line_blk[m].num_pts = n_pts;
			m++;
			n_pts = 0;
			pl->line_blk[m].subtype = dim_line;
			pl->line_blk[m].line.line_font = UA_dim_line_font;
			pl->line_blk[m].line.line_density = UA_dim_line_dens;
			pl->line_blk[m].line.color = UA_dim_line_color;
			}
		um_vcplvc(location, y_off_set, location);
		um_vctovc(location, pl->line_blk[m].line_seg[n_pts]);
		um_vcplvc(location, x_off_set, location1);
		um_vctovc(location1, pl->line_blk[m].line_seg[n_pts+1]);
		n_pts = n_pts + 2;
		}
	pl->line_blk_use = m + 1;
	pl->line_blk[m].num_pts = n_pts;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     :  ua_pl_titles(pl, origin, xaxis, yaxis, zaxis,
**																	title_keys, scale)
**     Create titles for PARTS LIST table 
**    PARAMETERS   
**       INPUT  : 
**          pl											PARTS LIST structure	
**				origin,xaxis,yaxis,zaxis			table plane
**				scale										current drawing scale
**       OUTPUT :  
**          title_keys								array of UNIBASE key_id's
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_pl_titles(pl, origin, xaxis, yaxis, zaxis, title_keys, scale)

struct UA_generic_draft		*pl;
UM_coord 						origin, xaxis, yaxis, zaxis;
UU_KEY_ID 						title_keys[10];
UU_REAL 							scale;

	{
	UU_REAL  					hgt, wdth[7], length, width, title_hgt,
									sub_title_hgt, title_char_hgt, sub_title_char_hgt,
									del_x, del_y, char_num;
	UM_coord 					location, location1, x_off_set, y_off_set, del;
	int							i, j, k;
	char							cr[2];
	struct	UA_txt_rec		note;			
	struct	UA_txtattr_rec	txtattr;	

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_titles "));

	/* scale table size values */
	strcpy(cr,"\n");
	title_hgt = UA_pl_title /scale;
	title_char_hgt = UA_pl_title_char_hgt /scale;
	sub_title_hgt = UA_pl_sub_title /scale;
	sub_title_char_hgt = UA_pl_subtitle_char_hgt /scale;
	width = 0.0;
	for(i=0;i<7;i++)
		{
		wdth[i] = UA_line_wdth[i]/scale;
		width = width + wdth[i];
		}
	
	/* create PARTS LIST title */
	del_y = 0.8*((title_hgt - title_char_hgt)/2.0);
	um_vctmsc(yaxis, del_y, y_off_set);
	um_vctmsc(xaxis, (width/2.0 - 5.0*title_char_hgt), x_off_set);
	um_vcplvc(pl->dim_origin, x_off_set, del);
	um_vcplvc(del, y_off_set, del);

	/* create new text entity	*/
	ur_setup_data(UA_TEXT_REL,&note,sizeof(struct UA_txt_rec));
	ur_setup_data(UA_TEXTATTR_REL,&txtattr,sizeof(struct UA_txtattr_rec));
	ua_init_txtrec(&note,&txtattr,&UA_txtattr,UU_FALSE);

	/* initilize text entity parameters */
	note.no_tchar = 10;
	strcpy(note.tchar,"PARTS LIST");
	um_vctovc(del, note.position);
	txtattr.entity_site = UA_BOTTOM_LEFT;
	txtattr.height = title_char_hgt;
	ua_txt_origin(&note,&txtattr,UU_FALSE);

	/* make text entity so that it will not be displayed except as part of
		the table */
	txtattr.displayable = UM_NEVERDISPLAYABLE;

	/* create text entity in UNIBASE */
	uc_create_data(&note,UM_DEFAULT_TF,&txtattr);
	title_keys[0] = note.key;

	/* now create  sub-titles */
	del_y = 0.8*((sub_title_hgt - 2.0*sub_title_char_hgt)/2.0);
	um_vctmsc(yaxis, (title_hgt + sub_title_hgt - del_y), y_off_set);

	/* determine character offset from the center of the column */
	for(i=1;i<=7;i++)
		{
		j = i-1;
		char_num = char_count[j];
		del_x =(wdth[j]/2.0 - char_num*sub_title_char_hgt); 

		/* compute off-set from the origin of the table */
		if(j > 0)
			for(k=0;k<j;k++) del_x = del_x + wdth[k];
		um_vctmsc(xaxis, del_x, x_off_set);
		um_vcplvc(pl->dim_origin, x_off_set, del);
		um_vcplvc(del, y_off_set, del);

		/* create a new text entity */
		ur_setup_data(UA_TEXT_REL,&note,sizeof(struct UA_txt_rec));
		ur_setup_data(UA_TEXTATTR_REL,&txtattr,sizeof(struct UA_txtattr_rec));
		ua_init_txtrec(&note,&txtattr,&UA_txtattr,UU_FALSE);
		um_vctovc(del, note.position);
		txtattr.entity_site = UA_TOP_LEFT;
		txtattr.height = sub_title_char_hgt;
		switch(i)
			{
			case 1:
				note.no_tchar = 7;
				strcpy(note.tchar, "FIND");
				strcat(note.tchar, cr);
				strcat(note.tchar, "NO");
				break;
			case 2:
				note.no_tchar = 21;
				strcpy(note.tchar, "PART NO. OR");
				strcat(note.tchar, cr);
				strcat(note.tchar, "IDENT NO.");
				break;
			case 3:
				note.no_tchar = 8;
				strcpy(note.tchar, "CAGE");
				strcat(note.tchar, cr);
				strcat(note.tchar, "NO.");
				break;
			case 4:
				note.no_tchar = 11;
				strcpy(note.tchar, "DESCRIPTION");
				break;
			case 5:
				note.no_tchar = 13;
				strcpy(note.tchar, "SPECIFICATION");
				break;
			case 6:
				note.no_tchar = 7;
				strcpy(note.tchar, "QTY");
				strcat(note.tchar, cr);
				strcat(note.tchar, "REQ");
				break;
			case 7:
				note.no_tchar = 2;
				strcpy(note.tchar, "PL");
				break;
			}
		ua_txt_origin(&note,&txtattr,UU_FALSE);
		txtattr.displayable = UM_NEVERDISPLAYABLE;

		/* create the UNIBASE record */
		uc_create_data(&note,UM_DEFAULT_TF,&txtattr);
		title_keys[i] = note.key;
		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  ua_pl_tbl_entry(pl, num, location, scale, index, find_no, key)
**       Create a PARTS LIST table entry line.
**    PARAMETERS   
**       INPUT  : 
**          pl											PARTS LIST structure
**				location									location of the entry in the table
**				scale										drawing scale
**       OUTPUT :  
**          key										UNIBASE key to the text
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_pl_tbl_entry(pl, num, location, scale, indx, find_no,  key)

	struct UA_generic_draft  	*pl;
	int 								num;
	UM_coord 						location;
	UU_REAL 							scale;
	int								*indx, find_no[];
	UU_KEY_ID 						*key;

	{
	struct   UA_txt_rec			note;			
	struct	UA_txtattr_rec		txtattr;	
	char								textstr[UA_TXT_BUFSZ];
	char                       field_str[7][100];
	int								i, flag, status, count, num_chars, num_line, len, id;
	UU_REAL							offset_cc[3];
	UU_LOGICAL form_status, UA_default, ua_get_pl_text_form(); 
	UU_TRUEDOUBLE              ff, uai_atof();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_pl_tbl_entry()"));
	

	/* get text for the entry from the user */
	count = num + 1;
	textstr[0] = '\0';
	num_chars = 0;
	UA_default = UU_FALSE;
repeat:;
	if(UA_pl_table_entry_method)	
		form_status = ua_get_pl_text_form("apltxt.frm", textstr, &num_chars,
													UA_default);
	else
		{
		ua_get_pl_text(170,255,textstr,&num_chars);
		if ( num_chars == 0 )
			form_status = UU_FALSE;
		else
			{
			form_status = UU_TRUE;
			}
		}
	if ( !form_status )
		{
		*key = -1;
		goto done;
		}
	else
	   {
		/* check on find number entry */
		ua_strip_pl_entry(textstr, &num_chars, field_str);
		len = strlen(field_str[0]);
		if(len > 3)
			{
			uu_uerror0(UA_DRAFTING,45);
			UA_default = UU_TRUE;
			goto repeat;
			}
		for(i=0;i<len;i++)
			{
			if(isdigit(field_str[0][i]) == 0)
				{
				uu_uerror0(UA_DRAFTING,52);
				UA_default = UU_TRUE;
				goto repeat;
				}
			}
		ff = uai_atof(field_str[0]);
		if(*indx == 0)
			{
			*indx = 1;
			find_no[0] = ff;
			}
		else
			{
			id = ff;
			for(i=0;i<*indx;i++)
				{
				if(id == find_no[i])
					{
					uu_uerror0(UA_DRAFTING,50);
					UA_default = UU_TRUE;
					goto repeat;
					}
				}
			find_no[*indx] = id;
			*indx = *indx + 1;
			}

		/* check quantity field if supplied */
		len = strlen(field_str[5]);
		if(len > 0)
			{
			if(len > 3)
				{
				uu_uerror0(UA_DRAFTING,57);
				*indx = *indx - 1;
				UA_default = UU_TRUE;
				goto repeat;
				}
			if(strcmp(field_str[5], "AR") != 0) 
				{
				for(i=0;i<len;i++)
					{
					if(isdigit(field_str[5][i]) == 0)
						{
						uu_uerror0(UA_DRAFTING,58);
						*indx = *indx - 1;
						UA_default = UU_TRUE;
						goto repeat;
						}
					}
				}
			}
		/* create new text entity	*/
		ur_setup_data(UA_TEXT_REL,&note,sizeof(struct UA_txt_rec));
		ur_setup_data(UA_TEXTATTR_REL,&txtattr,sizeof(struct UA_txtattr_rec));
		ua_init_txtrec(&note,&txtattr,&UA_txtattr,UU_FALSE);
		ua_check_pl_quantity(textstr, &num_chars);
		note.no_tchar = num_chars;
		strcpy(note.tchar,textstr);
		if(num_chars == 1) 
			{
			note.no_tchar++;
			strcat(note.tchar, " ");
			}
	   }

	/* set position and attributes of the text entity */
	um_vctovc(location, note.position);
	txtattr.entity_site = UA_BOTTOM_LEFT;
	txtattr.height = UA_pl_char_hgt/scale;
	ua_txt_origin(&note,&txtattr,UU_FALSE);
	txtattr.displayable = UM_NEVERDISPLAYABLE;

	/* create the UNIBASE record */
	uc_create_data(&note,UM_DEFAULT_TF,&txtattr);
	*key = note.key;

done:
	uu_dexit;
	return;
	}

/*********************************************************************
**    E_FUNCTION :  ua_get_pl_text(prompt, maxsize, text, num_chars)
**       Get user's input string for a PARTS LIST line.
**    PARAMETERS   
**       INPUT  : 
**          prompt									prompt number
**				maxsize									maximum number of characters
**
**       OUTPUT :  
**          text										text string
**				num_chars								number of characters in the string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_get_pl_text(msg, maxsize, text, num_chars)

	int		msg;
	char		text[1];
	int		*num_chars;

	{
	int		char_cnt, status;
	char		line_text[257];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,
		"ua_get_pl_text(maxsize=%d, text=%s, num_chars=%d)", maxsize,
					text, *num_chars));

	char_cnt = 0;
	status = ud_string(UA_DRAFTING,msg,line_text,maxsize, &char_cnt,UU_FALSE);
	strcpy(text,line_text);
	*num_chars = char_cnt;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  ua_check_pl_quantity(textstr, num_chars)
**       Check users input and auto-update quantity field if required.
**    PARAMETERS   
**       INPUT  : 
**          textstr									input text string
**				num_chars								number of characters input
**
**       OUTPUT :  
**          textstr									updated text string
**				num_chars								updated character count
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_check_pl_quantity(textstr, num_chars)

	char		textstr[];
	int		*num_chars;

	{
	char		field_str[7][100];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us, "ua_check_pl_quantity"));


	/* strip out strings */
	ua_strip_pl_entry(textstr, num_chars, field_str);

	/* check for no quantity value input by the user */
	if(strlen(field_str[5]) == 0)
		{
		ua_update_pl_count_field(field_str, textstr, num_chars);
		}

	uu_dexit;
	}
	 
/*********************************************************************
**    E_FUNCTION :  ua_strip_pl_entry(textstr, num_chars, field_str)
**       Strip out individual fields from the entry string
**    PARAMETERS   
**       INPUT  : 
**          textstr									input text string
**				num_chars								number of characters input
**
**       OUTPUT :  
**          field_str								array of strings
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_strip_pl_entry(textstr, num_chars, field_str)

	char		textstr[];
	int		*num_chars;
	char field_str[7][100];

	{
	int		i, field, status, count, number;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us, "ua_strip_pl_entry"));

	/* initilize  data */
	for(i=0;i<7;i++) field_str[i][0] = '\0';
	field = count = 0;

	/* strip out strings */
	while(count < *num_chars)
		{
		for(i=0;((textstr[count] != ';')&&(count<*num_chars));count++,i++)
			{
			field_str[field][i] = textstr[count];
			}
		field_str[field][i] = '\0';
		field++;
		count++;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  ua_update_pl_count_field(field_str, textstr, num_chars)
**       Update the count field and generate a new string.
**    PARAMETERS   
**       INPUT  : 
**          field_str								array of field strings
**
**       OUTPUT :  
**          textstr									updated text string
**				num_chars								updated character count
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_update_pl_count_field(field_str, textstr, num_chars)

	char     field_str[7][100];
	char		textstr[];
	int		*num_chars;

	{
	int		i, field, status, count, number;
	char		new_str[1024];
	UU_REAL  id_value[1];
	int		id_count[1];
	UU_TRUEDOUBLE ff, uai_atof();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us, "ua_update_pl_count_field"));

	ff = uai_atof(field_str[0]);
	id_value[0] = ff;
	number = 1;
	ua_balloon_count(id_value, number, id_count);
	if(id_count[0] > 0.1)
		{
		sprintf(new_str, "%d", id_count[0]);
		strcpy(field_str[5], new_str);
		}
	else if(id_count[0] < 0)
		strcpy(field_str[5], "AR");

	new_str[0] = '\0';
	for(i=0;i<7;i++)
		{
		strcat(new_str, field_str[i]);
		strcat(new_str, ";");
		}
	strcpy(textstr, new_str);
	i = strlen(textstr);
	textstr[i - 1] = '\0';
	*num_chars = i - 1;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_get_pl_text_form(formfile, str, num_chars, def_flg)
**       Get Parts List Table Data
**    PARAMETERS   
**       INPUT  : 
**          formfile				form file name
**          def_flg				UU_FALSE = no default in string
**                 				UU_TRUE  =  default in string
**       OUTPUT :  
**          str					string of characters entered by the user
**          num_chars			number of characters in the string
**    RETURNS      : 			UU_FALSE if bad input; UU_TRUE otherwise 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ua_get_pl_text_form(formfile, str, num_chars, def_flg)
	char				*formfile;
	char 			 	*str;
	int   			*num_chars;
	UU_LOGICAL 		def_flg;
	
	{
	char buff[120];
	int i;
	UU_LOGICAL status;

	static char tbl_field[7][100]; 
	static int *ans[] = {(int *)tbl_field[0], (int *)tbl_field[1],
								(int *)tbl_field[2], (int *)tbl_field[3],
								(int *)tbl_field[4], (int *)tbl_field[5],
								(int *)tbl_field[6]};

	uu_denter(UU_MTRC,(us,"ua_get_pl_txt_form()"));

	if(def_flg)
		{
		ua_strip_pl_entry(str, num_chars, tbl_field);
		}
	else
		{
		for(i=0;i<7;i++)
			tbl_field[i][0] = '\0';
		}

	status = ud_form(formfile, ans, ans);
	if (status==-1)
	   return UU_FALSE;

	if(strlen(tbl_field[0]) <= 0)
					status = UU_FALSE;
	else
		{
		str[0] = '\0';
		for(i=0;i<7;i++)
			{
			sprintf(buff,"i = %d field = %s", i, tbl_field[i]);
			um_pscroll(buff);
			strcat(str, tbl_field[i]);
			strcat(str, ";");
			}
		*num_chars = strlen(str);
		status = UU_TRUE;
		sprintf(buff,"num_chars %d str = %s", *num_chars, str);
		um_pscroll(buff);
		}
	
	uu_dexit;
	return(status);
	}
