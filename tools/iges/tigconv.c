/********************************************************************
**       NAME         :  tigconv.c
**       CONTAINS:
**             uig_in_convert
**             uig_get_para_rec
**             uig_nxt_field
**             uig_nxt_string
**             uig_join_cnt
**             uig_get_data
**             uig_get_trans
**             uig_update_trans
**             uig_check_range
**             uig_invert_trans
**             uig_check_for_drawing
**             uig_mfcheck_for_drawing
**             check_inx
**             uig_set_def (rel_type, d_bag)
**             uig_check_for_filter(dblk)
**				iges_batchrun
**				uig_save_custom_color()
**				uig_append_custom_color
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       tigconv.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       11/22/17 , 11:39:51
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "umath.h"
#include "riddle.h"
#include "riddldef.h"
#include <stdio.h>
#include "xenv1.h"		/* used by "ux" calls: UX_PRTERRS value */
#include "udebug.h"
#include "nclver.h"
#include "nclfc.h"
#include "nccs.h"
#if UU_COMP != UU_WIN2K
#include "tigmf.h"
#endif
#include "msrf.h"

int NCLX_external_unibase=0;

#define MAX_DRAWING 1000
extern int tig_unlabeled_ent[];
static int cnt_par =0;
extern char iges_fname[UX_MAX_PATH_LEN];

/******************************************************
/******************************************************
**                                                   **
** index array for the variable length lists         **
** specifies where in the data bag the lists are     **
** stored.                                           **
**                                                   **
******************************************************/

int join_indx[7] = {IDX1, IDX2, IDX3, IDX4, IDX5, IDX6, IDX7}; 

/******************************************************
**                                                   **
** layer number ranges                               **
**                                                   **
******************************************************/

/*
.....change variable name because other file will use it, and these
.....name are too common, extend range array.
.....Yurong 6/16/98
*/
/*int num_range, range[10][2]; */
int tig_num_range=0, tig_range[IG_LAYER][2];

int ent_cnt[IG_NUM+1], sub_ent_cnt[IG_NUM+1];
UU_LOGICAL bump_u = UU_FALSE;
UU_LOGICAL bump_v = UU_FALSE;
UU_REAL starting_ang_u = 0.0;
UU_REAL starting_ang_v = 0.0;
UU_REAL period_u = 0.0;
UU_REAL period_v = 0.0;
UU_LOGICAL uvcv_flag = UU_FALSE;
extern UU_REAL instance_tf[4][3];

int process_normal;	           	/*jkd23: structural entities or all */
int read_count;		              	/*jkd21: count of entities read */
int num_entities; 	            	/*jkd52: total number of entities */
int read_percent = 0;	         	/*jkd21: last percent value displayed */
int last_irec;		               	/*jkd50: max value of irec */
int is_drawing = 0;             /** cpp: logical flag for drawings */
int *ent_type = 0;              /*vp2.93: save types for each entity */
/* char *label_used;    */        /*vp2.93: list of labels in use, not implmt yet */
extern int MAX_PARA_REC;        /*MILLS: set size of parameter record
													  through global env.*/
extern int mapncl;
extern int drawing_id;
/*jkd5: count entities translated */

/*UU_KEY_ID tig_unlabeled_keys[50000]; */
/*
.....defined in tigglobal.h
UU_KEY_ID *tig_unlabeled_keys = NULL;
int tig_unlabeled = 0;
extern int tig_max_cvlab;
UU_LIST UIG_sflist_keys;
*/
extern int tig_max_ptlab;
extern int tig_max_lnlab;
extern int tig_max_cilab;
extern int tig_max_pllab;
extern int tig_max_cnlab;
extern int tig_max_pnlab;
extern int tig_max_pvlab;

/*duplicate symbol name counter*/
int uig_set_def();
void uig_update_trans();
void uig_chk_poly();
void uig_chk_group();
void uig_list_out();
UU_LOGICAL uig_check_range();
UU_LOGICAL uig_check_for_drawing();
UU_LOGICAL uig_mfcheck_for_drawing(); 
int uig_get_data();
int uig_nxt_field();
int uig_join_cnt();
int uig_get_join_rec();
/*
.....list of the 314 color entities found in the iges file.
*/

extern void uig_str_out();
extern int uig_get_global();
extern int uig_get_directory();
extern int uig_setup_save();
extern void uig_save();
extern void init_label();
extern void check_color();
extern void uig_in_dispat();
extern int uig_match_remaining();
extern int uig_secondary_unmatched();
extern int uig_label_remaining();
extern uig_prompt(); 
extern void um_tftmtf();
extern char* uu_toolmalloc();
extern char* uu_malloc();
extern void uu_toolfree();
extern int atoi();
extern void setifl();
extern void setscv();
extern int uig_rd_direct();
extern int uig_data_dict();
extern void uig_get_addr();
extern void uig_error();
extern void uig_move();
extern int uig_get_holerith();
extern UU_TRUEDOUBLE uig_atof();
extern int uig_nxtarg();

#if UU_COMP != UU_WIN2K
extern void uig_get_range();
extern void uig_get_label_type();
extern void uig_tty_mode();
extern int uig_get_curve_toler();
extern int uig_clear();
extern int uig_ans();
extern int uig_mfget_label_type();
extern void uig_mflist_ans();
#endif

static int uw_color_cust_table[UIG_MAXCOLOR-UIG_STDCOLOR][3];
static char uw_color_cust_name[UIG_MAXCOLOR-UIG_STDCOLOR][96];
/*********************************************************************
**    I_FUNCTION     :  uig_save_custom_color
**          Saved the current custom color into a static value
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**          none   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_save_custom_color()
{
	int i, cus_colors;
/*
......total allowed custom colors
*/
	cus_colors = UIG_MAXCOLOR - UIG_STDCOLOR;
	for (i=0; i<cus_colors; i++)
	{
		uw_color_cust_table[i][0] = uw_color_table[i+UIG_STDCOLOR][0];
		uw_color_cust_table[i][1] = uw_color_table[i+UIG_STDCOLOR][1];
		uw_color_cust_table[i][2] = uw_color_table[i+UIG_STDCOLOR][2];
		if (uw_color_name[i+UIG_STDCOLOR][0]!='\0')
			strcpy(uw_color_cust_name[i], uw_color_name[i+UIG_STDCOLOR]);
		else
			uw_color_cust_name[i][0] = '\0';
	}
}

/*********************************************************************
**    I_FUNCTION     :  uig_append_custom_color(int indx)
**          Append the current saved custom color from the modals file
**          into current color table.
**    PARAMETERS   
**       INPUT  : 
**          indx: 			Number of colors from imported from IGES
**                         file.  This value + UIG_STDCOLOR (16) contains
**                         the number of colors currently stored in
**                         the main color table (uw_color_table).
**       OUTPUT :    none
**    RETURNS      : none
**    SIDE EFFECTS :
**          Updates the global color table.
**    WARNINGS     : none
*********************************************************************/
void uig_append_custom_color(indx)
int indx;
{
	int i,j;
	double dx,dy,dz;	
	int maxcolors, numcolors;
/*
.....Initialize routine
.....maxcolors = Maximum number of custom colors allowed (48).
.....numcolors = Number of colors stored in color table.
*/
	maxcolors = UIG_MAXCOLOR - UIG_STDCOLOR;
	numcolors = UIG_STDCOLOR + indx;
/*
.....Main loop to go through modals file colors
*/
	for (i=0; numcolors<maxcolors && i<maxcolors; i++)
	{
/*
........At end of modals color table
*/
		if (uw_color_cust_name[i][0] == '\0') break;
/*
........Look for a matching color
........in the color table
*/
		for (j=0;j<numcolors;j++)
		{
			dx = 1.0*uw_color_cust_table[i][0] - 1.0*uw_color_table[j][0];
			dy = 1.0*uw_color_cust_table[i][1] - 1.0*uw_color_table[j][1];
			dz = 1.0*uw_color_cust_table[i][2] - 1.0*uw_color_table[j][2];
			if ((abs(dx)<=1.0)&&(abs(dy)<=1.0)&&(abs(dz)<=1.0))
				break;
		}
/*
......No matching entry
......Append modals color to color table
*/
		if (j == numcolors)
		{
			strcpy(uw_color_name[numcolors], uw_color_cust_name[i]);
			uw_color_table[numcolors][0] = uw_color_cust_table[i][0];
			uw_color_table[numcolors][1] = uw_color_cust_table[i][1];
			uw_color_table[numcolors][2] = uw_color_cust_table[i][2];
			numcolors++;
		}
	}
}

/*********************************************************************
**    I_FUNCTION     :  update_counts(key,irec)	
**          Update translated, untranslated counts. 
**    PARAMETERS   
**       INPUT  : 
**          key: 			if >0, translated; else not translated.
**			irec:			directory record number.
**       OUTPUT :  
**          output   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void update_counts(key, irec) 
int key, irec;
{
	int percent,i;
	char buf[80];

	i = irec/2;
	if (xlated_flag[i] == 0)
	{
		read_count++;
		xlated_flag[i] = 'c';   	/* counted but not translated */	
	}
	if (key != 0) {
		xlated_flag[i] = 'x';   	/* counted and translated */
		xlated_key[i] = key;
	}

 	percent = read_count *100 / num_entities;		/*jkd52*/
	if (percent % 2 == 0)	   	/*jkd21: print percent once ... */
													 /*jkd21: ... for each multiple of 10 */
	if (percent != read_percent && percent > 0 && percent < 100)
	{
/*
.....Used to display '%%', removed the extra '%' (remove two %% after
.....the %d to remove one '%')  JLS 10/1/99
*/
	   sprintf(buf, "  %d%% of file examined\n", percent);
	   uig_str_out(buf, UU_TRUE);
	   read_percent = percent;
		iges_disply_as_percent(percent);
	}	
}


/*********************************************************************
**    I_FUNCTION     :  uig_in_convert()
**          Convert IGES input file to neutral file format
**    PARAMETERS   
**       INPUT  : 
**          input    
**       OUTPUT :  
**          output   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_convert()
{
	struct global_rec gblk;          /* global record sturcture */
	struct dir_rec dblk;             /* directory record */
	struct IG_igesclr_rec *pblk;
	struct UR_unistat_rec unistat;
	int irec;                        /* directory record pointer */
	UX_pathname fnameu,fnames,fnamea;
	char *c;
	char  *ux_getenv();
	int set_color,i, n, status,lu, layer_trans, unlabeled = 0, start_id;
	int ent_not_xlated_cnt[IG_NUM+1], sub_ent_not_xlated_cnt[IG_NUM+1];
    int not_xlated_flag, sub_ent_flag, ent_not_xlated_flag;
	int sub_ent_not_xlated_flag,indx;
#if UU_COMP != UU_WIN2K
	int option;
#endif
	UM_int2 v264, i2;
	UU_KEY_ID key, *keyp;
	UU_LOGICAL	found, trans_drawing;
    static char ncl_type_names[IG_NUM+1][11] = {"circle    ","comp curve",
        "conic     ","polyline  ","pattern   ","point vect","plane     ",
        "line      ","rbspl crv ","mesh surf ","point     ","rbspl srf ",
        "srf of rev","rbspl srf ","transform ","rbspl crv ","rbspl srf ",
        "rbspl srf ","boundary  ","uv cvonsrf","trim srf  ","trim srf  ",
        "gen draft ","gen draft ","gen draft ","gen draft ","gen draft ",
        "gen draft ","gen draft ","gen symbol","group     ","          ",
        "          ","subfig    ","instance  ","view      ","drawing   ",
        "property  ","(unknown) ","mf solid  ","vertex lst","edge lst  ",
		  "face      ","loop      ","shell     ","color     "};

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	tig_unlabeled = 0;
	tig_max_ptlab = 0;
	tig_max_lnlab = 0;
	tig_max_cilab = 0;
	tig_max_pllab = 0;
	tig_max_cnlab = 0;
	tig_max_pnlab = 0;
	tig_max_pvlab = 0;
	tig_max_cvlab = 0;
	tig_max_sflab = 0;

	UIG_color_array = UU_NULL;

	for (i=0;i<9;i++) tig_unlabeled_ent[i] = 0;
	uig_get_global(&gblk);                    /* get global block */
/*
.....Initialize duplicate symbol name counter
*/
	dupsym =0;
/*
...vp 31-May-94
...Set units flag using IGES global section data,
...ifl(264) will output units field in unibase file header
*/
	i2 = 264;
	v264 = gblk.units - 1;
	setifl (&i2,&v264);
	i2 = 169;
	setscv (&i2, &NCL_version);
#if UU_COMP != UU_WIN2K
	if (UW_MOTIF!=1)
		uig_get_range(&tig_num_range, tig_range);         /* get users layer options */

	if(tig_num_range == -1) 
	{
		return(UU_SUCCESS);
	}

	if (UW_MOTIF!=1)
	{
		uig_get_label_type();                     /*jkd15: new labels */

	/* NCL: to properly handle writes to screen  on VAX */
		uig_tty_mode(SCROLL);
	}
	else
		uig_mfget_label_type();
#else
	iges_wntget_label_type();
#endif

	status = uig_setup_save(&lu, fnameu, fnames, fnamea); 

	if(status != 0) 
	{
#if UU_COMP != UU_WIN2K
		/* NCL: reset */
		if (UW_MOTIF!=1)
			uig_tty_mode(RESET);
#endif
		return(UU_SUCCESS);
	}
/*
.....there are other variables we may add to the input form for MOTIF
.....but since we never use mapncl=1 => NCLgeom, we ingore it now
.....Yurong
*/
/*
.....ignore it for WNT too
*/
#if UU_COMP != UU_WIN2K
	if (mapncl) uig_get_curve_toler(gblk.units); /* NCL: parametric spline to NCL curve*/
#endif
	uig_label_emptylists();
	switch (label_type)
		{
		case 1:
			uig_list_out("Labels generated.\n\n", UU_FALSE);
			break;
		case 2:
			uig_list_out("Labels generated using subscripts.\n\n", UU_FALSE);
			break;
		case 3:
			uig_list_out("Labels from file.\n\n", UU_FALSE);
			break;
		case 4:
			uig_list_out("Labels from file using subscripts.\n\n", UU_FALSE);
			break;
		case 5:
			uig_list_out("Labels from file using CV subscript.\n\n", UU_FALSE);
			break;
		case 6:
			uig_list_out("Labels from file using property.\n\n", UU_FALSE);
			break;
		case 7:
			uig_list_out("Labels from file using max 6 chars.\n\n", UU_FALSE);
			break;
		case 8:
			uig_list_out("Labels from existing unibase.\n\n", UU_FALSE);
			sprintf(p_buff," Labelling unibase: %s\n\n",UIG_label_unibase);
			uig_list_out(p_buff,UU_TRUE);
			break;
		case 9:
			uig_list_out("Labels from file label field and subscript field.\n\n",
			    UU_FALSE);
			break;
		case 10:
			uig_list_out("Labels from file using property without subscript.\n\n",
			    UU_FALSE);
			break;
		}
/*
.....If label_type is from property (6 or 10), need to make sure that there
.....are property entities to process.  If there aren't display a message 
.....and set label_type back to 1. JLS 1/28/99
*/
	if(label_type==6 || label_type==10)
	{
/*
.....Clear the array
*/
		for(irec=0;irec<IG_NUM;irec++) 
		{
			ent_cnt[irec] = 0;
			sub_ent_cnt[irec] = 0;
		}    
		for(irec=1;irec<(sect_ptr[2]+1);irec++,irec++)     
		{
/*
.....Get info on record
*/
			uig_get_directory(irec,&dblk);          
			if(irec > 0)
			{
/*
.....Look for the appropriate place to increment the count in the array
*/
				for(i=0;i<IG_NUM;i++)                  
				{
					if(dblk.rel_type == iges_type[i])
					{
/*
.....Found it now increment the count for that entity.
*/
						ent_cnt[i]++;
						break;
					}
				}
			}
		}
/*
.....If ent_cnt[37] is equal to zero there are no Properties to be processed
.....so print out message and set label_type back to 1.
*/
		if(ent_cnt[37]==0)
		{
			sprintf(p_buff,"\nLabel Property Entities do not exist in this file.\n");
			uig_list_out(p_buff,UU_TRUE);
			sprintf(p_buff,"IGES generated labels will be used.\n\n");
			uig_list_out(p_buff,UU_TRUE);
			label_type=1;
		}
	}
	for(i=0;i<=IG_NUM;i++)                     /* clear statistics arrays */
	{                                      /*jkd5: clear all counters  */
		ent_cnt[i] = 0;
		sub_ent_cnt[i] = 0;
		ent_not_xlated_cnt[i] = 0;
		sub_ent_not_xlated_cnt[i] = 0;
	}
	not_xlated_flag = 0;
	read_count = 0;

	number_of_masters = 0;
	UIG_unibase_entities = 0;
	UIG_dupcount = 0;

	/*  set up standard output mode on tty */
#if UU_COMP != UU_WIN2K
	if (UW_MOTIF!=1)
		uig_tty_mode(SCROLL);
#endif
	uig_str_out("\nConverting IGES input file to internal format\n",UU_TRUE);
	iges_open_process_win("Converting IGES to Unibase file");
	iges_disply_as_percent(1);
/***************************
**                        **
** primary directory loop **
**                        **
***************************/
	c = uu_toolmalloc(MAX_PARA_REC);
    if (c == UU_NULL)
    {
        sprintf(p_buff,"\nCan't allocate memory for parameter.\n");
        uig_list_out(p_buff,UU_TRUE);
        sprintf(p_buff,"Try setting MAX_PARA_REC to a smaller value.\n");
        uig_list_out(p_buff,UU_TRUE);
        sprintf(p_buff,"Stopping translation.\n");
        uig_list_out(p_buff,UU_TRUE);
        return(UU_SUCCESS);
    }
	/*jkd23: process_normal = 0: process all entities, independent of struct */ 
	/*jkd23: process_normal = 1: process structural entities (normal case) */ 
	process_normal = 0;		/*jkd23 */ 

	last_irec = sect_ptr[2]+1;   /*jkd50: do not exceed MAX_ENTITIES*/
	num_entities = last_irec/2;	/*jkd52 */

	if (ent_type != 0) 
	{
		uu_toolfree(ent_type);
		ent_type = 0;
	}
	ent_type = (int *)uu_toolmalloc(sizeof(int)*num_entities);
/*
...vp 4-may-93 free memory if alocated & alocate exactly 
...for this iges file (number of entities)
*/
	if (xlated_flag != NULL) 
	{
		uu_toolfree(xlated_flag);
		uu_toolfree(xlated_key);
		xlated_flag = NULL;
	}
	xlated_flag = uu_toolmalloc(num_entities);
	xlated_key = (UU_KEY_ID *) uu_toolmalloc(num_entities*sizeof(UU_KEY_ID));
	if (xlated_flag == NULL) status = UU_FAILURE;

	if (tig_unlabeled_keys)
	{
		uu_free(tig_unlabeled_keys);
		tig_unlabeled_keys = NULL;
	} 
	tig_unlabeled_keys = (UU_KEY_ID *)uu_malloc(num_entities*sizeof(UU_KEY_ID));
	if (!tig_unlabeled_keys) status = UU_FAILURE;

	for (i=0;i<num_entities;i++)     /* vp2.93 clear all flags */ 
	{
		xlated_key[i] = 0;
		xlated_flag[i] = 0;
		ent_type[i] = 0;
	}
	start_id = 1;
	/* cpp: check for a DRAWING */
#if UU_COMP != UU_WIN2K
	if (UW_MOTIF!=1) 
		trans_drawing = uig_check_for_drawing(last_irec, &start_id); 
	else
		trans_drawing = uig_mfcheck_for_drawing(last_irec, &start_id); 
#else
	trans_drawing = uig_mfcheck_for_drawing(last_irec, &start_id); 
#endif
	found = UU_FALSE;
/*
.....Chekcing for any 314 color entities.
.....Save the pointer thats points to them as well as the color.
*/
/*
.....create the default color table. If there is IGES color checked, read in GCOLOR read
.....to overwrite, otherwise, use default table
*/ 
	ncl_init_color();
	if(UIG_color_iges != 0)
	{
		status = uig_load_color_mode("");
/*
......we will add the iges color first, then current custom color later
......so we save it here and after we added iges color, append the current 
......custom color after the iges colors
*/
		uig_save_custom_color();			
		i = indx = 0;
		if ((last_irec-start_id) / 2>0)
		{
			UIG_ncolor_iges = (last_irec-start_id) / 2;
			if (UIG_color_array==NULL)
				UIG_color_array = (struct UIG_color_struct *)
					uu_toolmalloc(UIG_ncolor_iges*sizeof(struct UIG_color_struct));
			for(irec = start_id;irec< last_irec;irec++,irec++)
			{	
/*
..... note how uig_get_data is called by passing a 2 to all , 
......which signifies the check for the color entity.
*/
				pblk = (struct IG_igesclr_rec *)c;
				pblk->name[0] = '\0';
				status = uig_get_data(irec,2,&dblk,c);
				if(status == 0)
				{
					status = uig_in_color(c, &indx, &set_color);
					UIG_color_array[i].color = set_color;
					UIG_color_array[i].irec = irec;
					i++;
					if (i == UIG_ncolor_iges) break;
				}
			}
		}
		uig_append_custom_color(indx);			
		if (i>0)
		{
			UIG_ncolor_iges = i;
			ncl_update_colors(0);
		}
		else if (UIG_ncolor_iges != 0)
		{
			UIG_ncolor_iges = 0;
			uu_free(UIG_color_array);
			UIG_color_array = UU_NULL;
		}
	}
	for (irec = start_id;irec< last_irec;irec++,irec++)
	{
		if (xlated_flag[irec/2] == 'x')		/*jkd5a: already processed */
			status = -1;
		else
			status = uig_get_data(irec,1,&dblk,c); 
/*
..... Check to see if entity is filtered/masked out of translation.
..... First, must get the position for this entity in the entity_mask[].
..... This is based off the position in the global iges_type[].
..... If entity not found in iges_type, i = 36.  Translate them.
*/
		for (i = 0; i < 37; i++)
		{
			if (entity_ref[i] == dblk.rel_type)
				break;
		}
		if(((i == 4) && (entity_mask[36] ==1) && (dblk.form_no == 12)) ||
		   ((i == 5) && (entity_mask[36] ==1) && (dblk.form_no == 13)))
			entity_mask[i] = 1;

		if ((i != 37) && (i != 18)) /* Curves-on-surfaces handled in own funct */
		{
			if (entity_mask[i] == 0)
				status = -1;
		}
/*
..... If entity is filtered/masked out from being translated,
..... set status = -1.  This will skip the translation.
*/	
		if (status == 0)            /*jkd5: process this entity */
		{
			ent_type[irec/2] = IG_NUM;
			for (i = 0; i < IG_NUM; i++)
			{
				if (iges_type[i] == dblk.rel_type)
					ent_type[irec/2] = i;
			}
			init_label(dblk.rel_type, irec);	/*jkd15a: unique label names */
			check_color(&dblk);		/*jkd34: set default colors */
			current_dir_number = irec;
	   
			if ((ent_type[irec/2] == IG_NUM) && (dblk.rel_type != DRAW))   /* unknown type */
			{
				sprintf(p_buff, "(DREC = %d) Unknown entity type %d.\n", 
							irec, dblk.rel_type);
				uig_list_out(p_buff, UU_TRUE);
				key = 0;
				continue;
			}

			if(dblk.rel_type != GTRAN)
			{
				if(dblk.rel_type == DRAW)   /* cpp: found a DRAWING. Should we translate */
				{
					if(!trans_drawing)
						continue;
					found = UU_TRUE;
				}
				if( uig_check_range(&dblk) ||  dblk.rel_type == GGROUP)
				{
					uig_in_dispat(&gblk,&dblk,c,&key);   /* handle individual types */
					uig_check_current_matrix_list(&dblk);
				}
			}
			else
			{
				key = 1;         /*jkd5: mark this entity translated */ 
				uig_update_trans(irec,c);  /* update transformations */
			}

			update_counts((int)key, irec);	/*jkd5: update translated counts */

			if(found) break;		/* cpp: quit loop if just completed a DRAWING */
		}
	}
/*
.....THIS IS WHERE WE ARE GOING TO NEED TO PUT ROUTINES
.....THAT GO BACK AND SEARCH THE SECONDARY UNIBASE AND
.....TRY TO FIND MATCHES THAT ARE CLOSE.
*/
	uig_str_out(" 100% of file examined\n\n", UU_TRUE);
	iges_disply_as_percent(100);

	if (label_type == 8)
	{
		unlabeled = tig_unlabeled;
		if (unlabeled > 0 && UIG_matchlevel > 0)
		{
			uu_list_init(&UIG_sflist_keys,sizeof(UU_KEY_ID),unlabeled,unlabeled);
			uig_str_out("  Matching remaining unlabeled entities.\n",UU_TRUE);
			iges_set_process_lab("Performing Level 1 Matching", NULL);
			iges_set_procpos(0);
			uig_match_remaining(1, &unlabeled);
			if (unlabeled > 0 && UIG_matchlevel > 1)
			{
				uig_str_out("   Level 2 matching.\n",UU_TRUE);
				iges_set_process_lab("Performing Level 2 Matching", NULL);
				iges_set_procpos(0);
				uig_match_remaining(2, &unlabeled);
			}
			if (unlabeled > 0 && UIG_matchlevel > 2)
			{
				uig_str_out("   Level 3 matching.\n",UU_TRUE);
				iges_set_process_lab("Performing Level 3 Matching", NULL);
				iges_set_procpos(0);
				uig_match_remaining(3, &unlabeled);
			}
			if (unlabeled > 0 && UIG_matchlevel > 3)
			{
				uig_str_out("   Level 4 matching.\n",UU_TRUE);
				iges_set_process_lab("Performing Level 4 Matching", NULL);
				iges_set_procpos(0);
				uig_match_remaining(4, &unlabeled);
			}
			keyp = (UU_KEY_ID *)UU_LIST_ARRAY(&UIG_sflist_keys);
			if (keyp)
			{
				n = UU_LIST_LENGTH(&UIG_sflist_keys);
				for (i=0;i<n;i++)
				{
					ncl_lst_delete (BOX_LIST, &keyp[i]);
					ncl_lst_delete (WHOLE_BOUNDARY_LIST, &keyp[i]);
				}
				uu_list_free (&UIG_sflist_keys);
			}
		}
		if(UIG_unmatch_sec)
			uig_create_sec_unmatch();
		uig_secondary_unmatched();
		if (unlabeled >0)
			uig_label_remaining(&unlabeled);
	}

/*
.....Save Unibase Statistics record
*/
	uig_get_unistat(&gblk,&unistat);
	uig_put_unistat(&unistat);

/****************************
**                         **
**  now save file          **
**                         **
****************************/
	uig_str_out("\nWriting Unibase file to disk\n",UU_TRUE);	/*jkd18 */
	iges_set_process_lab("Writing Unibase file to disk", NULL);
	iges_disply_as_percent(50);
	uig_save(lu, fnameu, fnames, fnamea);

/****************************
**                         **
**  now print statistics   **
**                         **
****************************/
#if UU_COMP != UU_WIN2K
	if (UW_MOTIF!=1)
		uig_tty_mode(RESET);
#endif
	/*jkd5: check all entities; count if translated or not translated */
    sub_ent_flag = 0;
    ent_not_xlated_flag = 0;
    sub_ent_not_xlated_flag = 0;

	for(irec = 1;irec < last_irec;irec++,irec++)
	{
		uig_get_directory(irec, &dblk);
		if (xlated_flag[irec/2] == 'x')
		{
			if (dblk.sub_swt == 0)
				ent_cnt[ent_type[irec/2]]++;
			else
            {
				sub_ent_cnt[ent_type[irec/2]]++;
                sub_ent_flag = 1;
            }
		}
		else
		{
            if (dblk.sub_swt == 0)
            {
				ent_not_xlated_cnt[ent_type[irec/2]]++;
                ent_not_xlated_flag = 1;
            }
            else
			{
                sub_ent_not_xlated_cnt[ent_type[irec/2]]++;
                sub_ent_not_xlated_flag = 1;
            }
			not_xlated_flag = 1;
		}
	}

/*
.....We want to display which layers were translated.  If all layers were
.....translated, then tig_num_range is equal to 0. JLS 1/22/99
*/
	if(tig_num_range>0)
	{
		for (layer_trans=0;layer_trans<tig_num_range;layer_trans++)
		{
			sprintf(p_buff,"Layer %d translated.\n",tig_range[layer_trans][0]);
			uig_list_out(p_buff,UU_TRUE);
		}
	}
	else
	{
		sprintf(p_buff,"All layers were translated. \n");
		uig_list_out(p_buff,UU_TRUE);
	}

    uig_list_out("\n", UU_FALSE);
    sprintf(p_buff, "Surface Shading is ");
    uig_list_out(p_buff, UU_TRUE);
    if (shade_set)
        uig_list_out("on.\n",UU_TRUE);
    else
        uig_list_out("off.\n", UU_TRUE);

	if (UIG_lucency!=100)
	{
		sprintf(p_buff, "Surface Translucency is %d\n", UIG_lucency);
		uig_list_out(p_buff, UU_TRUE);
	}
    for(i=0;i<IG_NUM;i++)
    {
        if(entity_mask[i] == 0)
        {
            sprintf(p_buff,
                "%s type entities were filtered out of translation.\n",
                iges_name[i+1]);
            uig_list_out(p_buff, UU_TRUE);
        }
    }
    uig_list_out("\n", UU_TRUE);

	uig_list_out(" \n",UU_FALSE);
	uig_list_out("Entity Statistics:\n",UU_TRUE);
	uig_list_out("Independent Entities Translated:\n",UU_TRUE);
    uig_list_out("   IGES ENTITY TYPE        NCL ENTITY TYPE       COUNT\n",UU_TRUE);
	for(i=0;i<IG_NUM;i++)		
	{
		if(ent_cnt[i] != 0)
		{
            sprintf(p_buff,"     %s             %s            %d\n",
                iges_name[i], ncl_type_names[i], ent_cnt[i]);
			uig_list_out(p_buff,UU_TRUE);
		}
	}
    if (sub_ent_flag == 1)
    {
        uig_list_out("Dependent Entities Translated:\n",UU_TRUE);
        uig_list_out("   IGES ENTITY TYPE       COUNT\n",UU_TRUE);
        for(i=0;i<IG_NUM;i++)
        {
            if(sub_ent_cnt[i] != 0)
            {
                sprintf(p_buff,"    %s        %d\n",iges_name[i],sub_ent_cnt[i]);
                uig_list_out(p_buff,UU_TRUE);
            }
        }
    }

	/*jkd5: print summary of entities not translated; this accounts for all entities */ 
    if (not_xlated_flag == 1)
    {
        if (ent_not_xlated_flag == 1)
        {
            uig_list_out("\nIndependent Entities NOT Translated:\n",UU_TRUE);
            for(i=0;i<=IG_NUM;i++)
            {
                if(ent_not_xlated_cnt[i] != 0)
                {
                    sprintf(p_buff,"    %s        %d\n",
                        iges_name[i], ent_not_xlated_cnt[i]);
                    uig_list_out(p_buff,UU_TRUE);
                }
            }
        }
        if (sub_ent_not_xlated_flag == 1)
        {
            uig_list_out("Dependent Entities NOT Translated:\n",UU_TRUE);
            for(i=0;i<=IG_NUM;i++)
            {
                if(sub_ent_not_xlated_cnt[i] != 0)
                {
                    sprintf(p_buff,"    %s        %d\n",
                        iges_name[i], sub_ent_not_xlated_cnt[i]);
                    uig_list_out(p_buff,UU_TRUE);
                }
            }
        }
    }
	if (UIG_nodups)
	{
		if (UIG_dupcount == 0)
			strcpy(p_buff,"\nNo duplicates found\n");
		else
			sprintf(p_buff,"\nDuplicates not translated = %-d\n",UIG_dupcount);
		uig_list_out(p_buff,UU_TRUE);
	}
/*........temp.................*/
/*	uu_toolfree(c); */
	uig_list_out(" \n", UU_FALSE);
#if UU_COMP != UU_WIN2K
	if (UW_MOTIF!=1)
	{
		uig_prompt(2,&option); 
		uig_tty_mode(RESET);   
	}
#endif
/*
.....Set xlated_flag back to NULL.  Problems were occurring on SGI's if
.....the user continued to create unibases without first exiting IGES.
.....JLS 6/28/99
*/
	xlated_flag = NULL;
/*
.....Free colormap table
*/
	if (UIG_ncolor_iges != 0) uu_free(UIG_color_array);
	UIG_color_array = UU_NULL;
	UIG_ncolor_iges = 0;

	iges_close_process_win();
	return(UU_SUCCESS);
}

/*jkd27: avoid seg violation caused by overflowing array c */
/*********************************************************************
**    I_FUNCTION     :  int check_inx(inx, chars, irec)
**          Check that index for array c stays within bounds.
**    PARAMETERS   
**       INPUT  : 
**          inx                     current index of array c
**          chars                   number of chars to be moved to c
**          irec                    directory record index          
**       OUTPUT :  
**          none 
**    RETURNS      : 0:good 1:error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int check_inx(inx, chars, irec)
int inx, chars, irec;
{
	/*MILLS: converted to use environment variable set in uig_intro() */
	if (inx+chars >= MAX_PARA_REC)
	{
		sprintf(p_buff,"(PREC = %d) Parameter record too large: increase MAX_PARA_REC.\n",
			irec);
		uig_list_out(p_buff,UU_TRUE);
		return(1);
	}
	return(0);
}


/*********************************************************************
**    I_FUNCTION     :  int uig_get_para_rec(ent_type,drec_num,irec,c)
**          Get the designated parameter record
**    PARAMETERS   
**       INPUT  : 
**          ent_type                entity type
**          drec_num                pointer to directory record
**          irec                    pointer to parameter record
**       OUTPUT :  
**          c                       a filled out parameter record 
**    RETURNS      : 0:good 1:error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_get_para_rec(ent_type,drec_num,irec,c)
int ent_type;
int drec_num;
int irec;
char c[];
{
#define MAXJOIN 8
#define JOINSIZE (sizeof(char *)+sizeof(int)+sizeof(int))
	int prec;			/* local storage for the current parameter pointer */
	int status, i, j, k, l, j2, cnt1, cnt2, in, out, ierr, dummy;
	int fi, pointer_prob, param_type_prob, int_cnt, int_fields[10];
	int cur_ptr, ptr2, lst_cnt, next, ptr, num, start, join_cnt;
	int type = 3, bytes = 80, drec_back_ptr;
	char *loc_ptr;
	char inbuf[120], outbuf[120], *ptrbuf, temp_str[8];
	UU_LOGICAL f_key;
	UU_REAL ff;
	struct attr_def rtn1[20], rtn2[20];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	next = sizeof(long) + sizeof(int);        /* initialize local parameters */
	out = in = 0;
	int_cnt = join_cnt = lst_cnt = ptr = 0;
	ptr2 =0;
	prec = irec;
	status =0;
	f_key = UU_TRUE;
   
	uig_rd_direct(type,prec,bytes,inbuf); /* get parameter record */
/*
..... Parse the backpointer to the directory entry from the parameter line.
*/
    strncpy(temp_str, (inbuf+65), 7);
    temp_str[7] = '\0';
    drec_back_ptr = atoi(temp_str);

	/* find relation number */

	for(i=0;i<IG_NUM;i++)
	{
		if(ent_type == iges_type[i])
		{
			cnt1 = uig_data_dict(rtn1,20,ddl_nam[i],&dummy);
			if(cnt1 == 0)
			{
				sprintf(p_buff,"(DREC = %d) Unknown entity type %s.\n",drec_num,ddl_nam[i]);
				uig_list_out(p_buff,UU_TRUE);
				return(99);
			}
			for(j=0;j<cnt1;j++)              /* process fields in dictionary */
			{
				switch(rtn1[j].attr_type)     /* switch on type */
				{
					case FLOAT:
					case DOUBLE:
					case REAL:
					case INT:
						num = rtn1[j].num_rows * rtn1[j].num_cols;
						start = next;
						for(k=0;k<num;k++)
						{
							ierr = uig_nxt_field(3,&prec,&in,&out,inbuf,outbuf); /*jkd41*/
							if (check_inx(next, rtn1[j].attr_size, irec)) /*jkd8a*/
							return(1);   /* parameter block too large */
							switch(rtn1[j].attr_type)
							{
								case FLOAT:
								case DOUBLE:
								case REAL:
									ff = uig_atof(outbuf);

/* cpp: This is not needed if tiges.ddl file processed properly
	#if UU_COMP == UU_IRIS4D || UU_SUNTYPE == UU_SUN_SUN4
									while (next % 8 != 0) next++;
	#endif
*/
									uig_move(&ff,&c[next],rtn1[j].attr_size);
									break;
								case INT:
									fi = atoi(outbuf);
									uig_move(&fi,&c[next],rtn1[j].attr_size);
									int_fields[int_cnt] = fi;
									int_cnt++;
									break;
							}
							next = next + rtn1[j].attr_size;
							if(ierr != 0)
							{  /*jkd30: clear next 4 bytes in case it is a join */
/*                      fi = 0;                    init values in uig_set_def if nec */
/*                      uig_move(&fi,&c[next],4);  ijd 18-Jan-91  */
								return(0);
							}
						}
						next = start + rtn1[j].attr_off;
						break;
					case CHARACTER:
						num = rtn1[j].num_rows * rtn1[j].num_cols;
						cur_ptr = next;
						if (check_inx(next, num, irec))  /*jkd8a*/
						return(1);   /* parameter block too large */
						ierr = uig_get_holerith(3,&prec,&in,&out,inbuf,&ptrbuf);/*jkd41*/
						for(l=0;l<out;l++,next++)
							c[next] = ptrbuf[l];
						c[next] = '\0';
						if(ierr != 0) 
						{
							return(0);
						}
						next = cur_ptr + num;
						if (out > 0) uu_toolfree (ptrbuf);
						break;
					case KEY_ID:
						ierr = uig_nxt_field(3,&prec,&in,&out,inbuf,outbuf); /*jkd41*/
						fi = atoi(outbuf);
						if (f_key == UU_TRUE)
						{
/*
..... Check the integrity of the directory and parameter entries.
..... First check the backpointer in the parameter record (char 66 - 72) is
..... the same as the directory entry sequence number (DREC).
*/
							pointer_prob = 0;
							if (drec_back_ptr != drec_num)
								pointer_prob = 1;

							param_type_prob = 0;
							if (fi != ent_type)
							{
/*
..... Secondly, check to see if the parameter type matches the directory entity
..... type.  Be careful, since we have our own entity types for "groups" (404) and
..... for "copious data" (106).
*/
								param_type_prob = 1;

								if (( fi == GGROUP ) && \
									( (ent_type == VIEWVS) || (ent_type == PLNASSOC) ))
									param_type_prob = 0;

								if (( fi == GPOLY ) && \
									( (ent_type == GPOLY3D) || (ent_type == GPOLY6D) ))
									param_type_prob = 0;
							}
							if ((pointer_prob == 1) || (param_type_prob == 1))
							{
								if (drec_back_ptr != drec_num) /* pointer problem */
								{
	sprintf(p_buff,"(DREC = %d) Incorrect backpointer in parameter data.\n",drec_num);
								}
								else		/* parameter entity type problem */
								{
	sprintf(p_buff,"(DREC = %d) Entity type in parameter not same as directory.\n",irec);
								}
								uig_error (p_buff);
								return (1);
							}
							f_key = UU_FALSE;
						}
						else
						{
							if (check_inx(next, sizeof(long), irec))  /*jkd8a*/
								return(1);   /* parameter block too large */
							uig_move(&fi,&c[next],sizeof(long));
							next = next + rtn1[j].attr_off;
						}
						if(ierr != 0)
						{
							return(0);
						}
						break;
					
					/* special case of a join field - must process
						the fields in the join table
					*/
					case JOIN:
						lst_cnt = uig_join_cnt(ent_type,join_cnt,int_cnt,int_fields);

						if(lst_cnt == 0)
						{  
/*
.....jkd30: ok to have 0 for join 
.....vp 4-aug-97 but it is not ok to read next field and assume 
.....that it is the number of joins when it is next list number of joins.
.....This simple fix is used for now only in TrimSrf to read correctly
.....number of properties when list of inner boundary curves was empty.
.....See fix in uig_join_cnt where return value is set to -1 to prevent
.....get next field if inner boundary list (empty) was processed. 
*/
				/* cpp: following lines are required */
				/*  ierr = uig_nxt_field(3,&prec,&in,&out,inbuf,outbuf); */
				/*   lst_cnt = atoi(outbuf);   */
							ierr = uig_nxt_field(3,&prec,&in,&out,inbuf,outbuf);
							lst_cnt = atoi(outbuf);
						}
						else if (lst_cnt < 0)
						{ 
							lst_cnt = 0; 
						}
						if (check_inx(next, sizeof(int), irec))  /*jkd8a*/
						return(1);   /* parameter block too large */
/*
.....The elements of the join are going to be further down the list
.....So determine the ptr number.
*/
						if (ptr == 0) ptr = next + MAXJOIN*JOINSIZE;
						while (ptr % 8!= 0) ptr++;
/*
.....Get the address of ptr.
*/
						uig_get_addr(&c[ptr],&loc_ptr);
						if (check_inx(next, sizeof(char *), irec))  /*jkd8a*/
							return(1);   /* parameter block too large */
/*
.....Put the address of ptr into the array, this is going to be the start of the
.....join in the c array.
*/
						uig_move(&loc_ptr,&c[next],sizeof(char *));
						next = next + sizeof(char *);
/*
.....Put lst_cnt into c array, this is the number of joins
*/
						uig_move(&lst_cnt,&c[next],sizeof(int));
						next = next + sizeof(int)*2;

						join_cnt++;
						if(ierr != 0) 
						{
							return(0);
						}
						if(lst_cnt > 0)
						{
							cnt2 = uig_data_dict(rtn2,20,&rtn1[j].attr_name[0],&dummy);
							if(cnt2 == 0) 
							{
								return(1);
							}
/*
.....For a bounding curve which contains a join within a join, make sure
.....that the data for the second join starts after the data for the first
.....join. IJD 20-MAR-2001
*/
							if (ent_type == 141)
								ptr2 = ptr + lst_cnt*sizeof(struct IG_crvptr_rec);
							if (ent_type == 510 )
								ptr2 = ptr + lst_cnt*sizeof(UU_KEY_ID);
							if (ent_type == 508 )
  							 	ptr2 = ptr + lst_cnt*sizeof(struct IG_edge_rec);
						}
						while(lst_cnt > 0)
						{
							for (j2=1;j2<cnt2;j2++)
							{
/*
.....Removed old switch cases and replaced by calling uig_get_join_rec
.....This eliminates repetative code.  JLS 7/9/99
*/
								status = uig_get_join_rec(irec,&prec,ent_type,
															rtn2[j2],&in,&inbuf,c,&ptr,&ptr2);
/*
.....if status =1, then there was an error, exit as such.
.....if status = 9 then there wasn't an error, but reached
.....the end of parameter data, so exit.
*/
								if (status ==1) return(1);
								if (status ==9) return(0);
							}
/*
.....Not sure exactly why ptr needs to be incremented by 80 in
.....this case, but after discussing it with Bobby, we decided
.....that since it works, ok.  If it isn't incremented by 80
.....things become out of place in the c array.
.....It needs to be incremented by IG_CRVPTR_BUFSZ (=80) to account for
.....the total length of the structure so that indexing into the array
.....of these structures will work. IJD 20-MAR-2001
*/
							if (ent_type == 141)
							{
								ptr = ptr+ IG_CRVPTR_BUFSZ;
							}
						  	if (ent_type == 508)
							{
								ptr = ptr+ IG_EDGE_BUFSZ;
							}
							lst_cnt--;
						}
						break;
				}
			}
			break;
		}
	}
	return(0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_nxt_field(type,prec,in,out,inbuf,outbuf)
**          Get the next field in the parameter record
**    PARAMETERS   
**       INPUT  : 
**          input    
**       OUTPUT :  
**          option                  users selection   
**    RETURNS      : 0 good return ; 1 error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_nxt_field(type,prec,in,out,inbuf,outbuf)
int type;
int *prec;
int *in;
int *out;
char inbuf[];
char outbuf[];
{
	char overflow[80];
	int locstr,locout,locrec,save, status, bytes = 80;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	locstr = *in;           /* initilize local parameters */
	locout = *out;
	locrec = *prec;
	status = 0;

	status = uig_nxtarg(type,inbuf,locstr,&locstr,&locout,outbuf);

	if(locstr > 0)
	{
		if(locout > 0)
		{
			*in = locstr;
			*out = locout;
			return(status);
		}
		else
		{
			*in = locstr;
			*out = 1;
			outbuf[0] = '0';
			outbuf[1] = '\0';
			return(status);
		}
	}
	else
	{
		/* parameter field spans record - get next record and continue */

		locrec++;
		uig_rd_direct(type,locrec,bytes,inbuf);

		locstr = 0;
		save = locout;

		status = uig_nxtarg(type,inbuf,locstr,&locstr,&locout,overflow);

		overflow[locout] = '\0';
		strcpy(&outbuf[save],overflow);
		locout = locout + save;
		*in = locstr;
		*out = locout;
		*prec = locrec;
		return(status);
	}
}

/*********************************************************************
**    I_FUNCTION     :  uig_nxt_string(type,prec,in,out,inbuf,outbuf)
**          Get the next string field in the parameter record
**    PARAMETERS   
**       INPUT  : 
**          input    
**       OUTPUT :  
**          option                  users selection   
**    RETURNS      : 0 good return ; 1 error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_nxt_string(type,prec,in,out,inbuf,outbuf)
int type;
int *prec;
int *in;
int *out;
char inbuf[];
char outbuf[];
{
	char numstr[120]; /*jkd1: changed from 20 to 120 */
	int locstr,locout,locrec, i, j, k, fi, count, status, imax, bytes = 80;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	locstr = *in;           /* initilize local parameters */
	locout = 0;
	locrec = *prec;
	status = 0;
	if (type == 1) /*jkd41*/
		imax = 72;
	else
		imax =64;

loop:
	i = locstr;
	while(inbuf[i] == ' ' && i < imax) i++;

	if(i == imax)
	{
		locrec++;
		uig_rd_direct(type, locrec, bytes, inbuf);
		locstr = 0;
		goto loop;
	}

	k = 0;
scanner:
	if (inbuf[i] == ',' || inbuf[i] == ';')
	{
		j = i; /*jkd41*/
		goto exit;
	}
	for(j=i;j<imax;j++,k++)
	{
		outbuf[locout] = inbuf[j];
		locout++;
		if(inbuf[j] == 'H')
		{
			numstr[k] = '\0';
			fi = atoi(numstr);
			break;
		}
		else
		{
			numstr[k] = inbuf[j];
		}
	}
	if (k == 5)
	{
		sprintf(p_buff,"bad H-string\n");
		uig_list_out(p_buff,UU_TRUE);
		goto exit;
	}
	if(j == imax)
	{
		locrec++;
		uig_rd_direct(type, locrec, bytes, inbuf);
		i = 0;
		goto scanner;
	}

		count = 0;
		j++;
transfer:

	if (fi > 120)     /*jkd9: 120 is max size of outbuf */
	{
		sprintf(p_buff,"character string (%d) truncated to 120\n", fi);
		uig_list_out(p_buff,UU_TRUE);
		fi = 120;
	}
	while(count < fi && j < imax)
	{
		outbuf[locout] = inbuf[j];
		count++;
		j++;
		locout++;
	}

	if(j == imax)
	{
		locrec++;
		uig_rd_direct(type, locrec, bytes, inbuf);
		j = 0;
		goto transfer;
	}

 	outbuf[locout] = '\0';	/*jkd41*/

exit:
	*in = j + 1;
	*out = locout;
	*prec = locrec;
	if(inbuf[j] == ';')	/*cpp */
	status = 1;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  int uig_join_cnt(type,join_cnt,int_cnt,int_fields)
**          Determine the number of variables to read in special IGES
**          join fields.
**    PARAMETERS   
**       INPUT  : 
**          type                    relation type
**          join_cnt                current join field count
**          int_cnt                 number of integer fields read 
**          int_fields              integer field values
**       OUTPUT : none 
**    RETURNS      :lst_cnt         number of words in the join field to read
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_join_cnt(type,join_cnt,int_cnt,int_fields)
int type;
int join_cnt;
int int_cnt;
int int_fields[];
{
	int cnt;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	cnt = 0;
	switch(type)
	{
		case GSPLINE:
			if(join_cnt < 2)
			{
				cnt = int_fields[int_cnt  - 1] + 1;
			}
			return(cnt);
		case GRSPLINE:
			switch(join_cnt)
			{
				case 0:
					cnt = int_fields[0] + int_fields[1] + 2;
					break;
				case 1:
				case 2:
					cnt = int_fields[0] + 1;
					break;
				case 3:
					cnt = 1;
					break;
			}
			return(cnt);
		case GRSPLSRF:
			switch(join_cnt)
			{
				case 0:
					/*
						cnt = int_fields[0] + 2*(int_fields[2] - 1) + 1;
					*/
						cnt = int_fields[0] + int_fields[2]  + 2;
					break;
				case 1:
						cnt = int_fields[1] + int_fields[3] + 2;
					break;
				case 2:
					/*
					*** cpp: fix calculation of the number of weights
					cnt = (int_fields[0] + int_fields[2])*
														(int_fields[1] + int_fields[3]);
					*/
					cnt = (int_fields[0] + 1) * (int_fields[1] + 1);
					break;
				case 3:
					/*
					*** cpp: fix calculation of the number of control points
					cnt = (int_fields[0] + int_fields[2])*
														(int_fields[1] + int_fields[3]);
					*/
					cnt = (int_fields[0] + 1) * (int_fields[1] + 1);

					break;
				case 4:
					cnt = 1;
					break;
/*
.....Fix label from property for B-Spline surface, bptr and property
.....counts should be zero because count is in parameter record. IJD 28Apr98
*/
				case 5:
					cnt = 0;
					break;
				case 6:
					cnt = 0;
					break;
			}
			return(cnt);
		case GSPLSURF:     /*jkd14: p-spline surface   */
			switch(join_cnt)
			{
			case 0:
				cnt = int_fields[2] + 1;
				break;
			case 1:
				cnt = int_fields[3] + 1;
				break;
			case 2:
				cnt = (int_fields[2]+1) * (int_fields[3]+1) * 48;
				break;
			}
			return(cnt);
		case GTRIMSRF:     /*ijd : trimmed surface   */
			if (join_cnt == 0) 
			{
/*
.....vp 5-aug-97 if list is empty (int_fields[1]=0) prevent
.....reading next field as a length of inner boundary list, because
.....it will be back pointer list length and property list (next one)
.....will be shifted to its place. 
*/
				cnt = int_fields[1];
				if (cnt == 0) cnt = -1;
         }
			return(cnt);
		case GPOLY:
			if(join_cnt == 0)
			{
				if(int_fields[0] == 1)
				{
					cnt = int_fields[1];
				}
			}
			return(cnt);
		case GNOTE:
		case GGROUP:
		case GLEADER:
		case GCOMPOSITE: /*jkd36*/
			if(join_cnt == 0)
			{
				cnt = int_fields[0];
			}
			return(cnt);
		case SUB_FIG:
			if(join_cnt == 0)
			{
				cnt = int_fields[1];
			}
			return(cnt);
	  case VIEWVS:
			switch(join_cnt)
			{
			case 0:
				cnt = int_fields[0];
				break;
			case 1:
				cnt = int_fields[1];
				break;
			}
			return(cnt);
		case PLNASSOC:
			cnt = int_fields[1];
			return(cnt);
		case GLOOP:
			cnt = int_fields[0];
/*
.....if list is empty (int_fields[1]=0) prevent
.....reading next field as a par_curve
*/
			if(cnt ==0) cnt =-1;
			return(cnt);
		case GFACE:
			cnt = int_fields[0];
			return(cnt);
		case GEDGLST:
			cnt = int_fields[0];
			return(cnt);
		case GVERLST:
/*
.....each vertex list entity is made up of 3 real numbers
*/
			cnt = 3*int_fields[0];
			return(cnt);
	   case GSHELL:
			cnt = int_fields[0];
			return(cnt);
		default:
			return(0);
	}
}

/*********************************************************************
**    I_FUNCTION     :  int uig_get_data(drec,all,dblk,d_bag)
**          Get all data associated with IGES entity in dir record drec
**    PARAMETERS   
**       INPUT  : 
**          drec                    directory record number
**          all                     = 0 translate entity regardless of status
**                                  = 1 translate only if a primary entity
**												= 2 trnaslate only if color entity
**       OUTPUT :  
**          dblk                    directory structure
**          d_bag                   parameter data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_get_data(drec,all,dblk,d_bag)
int drec;
int all;
struct dir_rec *dblk;
char d_bag[];
{
	int status = -1;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uig_get_directory(drec,dblk);       /* get directory record */
	dblk->drec_num = drec;

	uig_set_def(dblk->rel_type, d_bag); /* set default values if nec - ijd */
	
/*
.....Extracting the parameter record for the 314 color entity
*/
		if(all == 2)
		{
			if(dblk->rel_type == GCOLOR)
				status = uig_get_para_rec(dblk->rel_type,drec,dblk->par_ptr,d_bag);
		}
		else
		{		
		
			if(dblk->rel_type == INST || dblk->rel_type == VIEW
									  || dblk->rel_type == GTRAN
									  || dblk->rel_type == DRAW)
			{
				status = uig_get_para_rec(dblk->rel_type,drec,dblk->par_ptr,d_bag);
			}
			else
			{
/*
.....Dependent enties are not translated here but under groups/instances/etc
*/
				if(dblk->sub_swt == 0 || all == 0)
				{
					if(dblk->rel_type == GLINE          || dblk->rel_type == GPOINT
				 		|| dblk->rel_type == GARC        || dblk->rel_type == GTRAN
				 		|| dblk->rel_type == GCOMPOSITE  || dblk->rel_type == GCONIC
				 		|| dblk->rel_type == GPOLY       || dblk->rel_type == GSPLINE
				 		|| dblk->rel_type == GLEADER     || dblk->rel_type == GRADIM
				 		|| dblk->rel_type == GDIADIM     || dblk->rel_type == GLINDIM
				 		|| dblk->rel_type == GANGDIM     || dblk->rel_type == GNOTE
				 		|| dblk->rel_type == GRSPLINE    || dblk->rel_type == GGROUP
				 		|| dblk->rel_type == GSPLSURF /*jkd14: p-spline surface */
				 		|| dblk->rel_type == GPLANE   /*jkd31: planes */
				 		|| dblk->rel_type == GRULEDSRF /*cpp */
				 		|| dblk->rel_type == GREVSRF /*cpp */
				 		|| dblk->rel_type == GTBCYSRF /*cpp */
				 		|| dblk->rel_type == SUB_FIG    || dblk->rel_type == GRSPLSRF
				 		|| dblk->rel_type == GCRVONSRF    || dblk->rel_type == GTRIMSRF
				 	|| dblk->rel_type == GOFFSTSRF
/*
.....Adding GBOUNDARY AND GBDSRF.  JLS 10-30-98
*/
             || dblk->rel_type == GBOUNDARY
             || dblk->rel_type == GBDSRF
/*
.....Adding GMSB.   06-16-05
*/
             || dblk->rel_type == GMSBO || dblk->rel_type == GLOOP
				 || dblk->rel_type == GEDGLST 
/*
.....Adding GVERLST
*/
				 || dblk->rel_type == GVERLST
				 || dblk->rel_type == GLABEL || dblk->rel_type == GGENSYM
				 || dblk->rel_type == GPLSF)
				{
 
					  /* special case of a copious entity - check dimensionality */
 
					 if(dblk->rel_type == GPOLY) uig_chk_poly(dblk);
					 if(dblk->rel_type == GGROUP) uig_chk_group(dblk);
						
					 status = uig_get_para_rec(dblk->rel_type,drec,dblk->par_ptr,d_bag);
				}
			}
			else
			{
				if((dblk->sub_swt == 1) &&(dblk->rel_type == GFACE )) 
				{
					status = uig_get_para_rec(dblk->rel_type,drec,dblk->par_ptr,d_bag);
				}
			}
		
	}}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_get_trans(ptr,t)
**          Get IGES transformation matrix in directory record ptr.
**    PARAMETERS   
**       INPUT  : 
**          ptr                     directory record
**       OUTPUT :  
**          t                       matrix
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_get_trans(ptr,t)
int ptr;
UU_REAL t[12];
{
	char c[150000];
	struct dir_rec dblk;
	int i,k,status;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	/* check if meaningfull pointer */

	if(ptr == 0)

	/* return identity matrix */

	{
		for(i=0;i<12;i++)
			t[i] = 0.0;
		t[0] = t[5] = t[10] = 1.0;
		goto xit;
	}

	/*  look in current transformation stack */

	if(t_num > 0)
	{
		for(i=0;i<t_num;i++)
		{
			if(ptr == t_ptr[i])
			{
				for(k=0;k<12;k++)
				{
					t[k] = tran[i][k];
				}
				goto xit;
			}
		}
	}

	/* not on current stack - go fetch data */

	status = uig_get_data(ptr,1,&dblk,c);
	if(status == 0)
	{
		uig_update_trans(ptr,c);
		for(k=0;k<12;k++)
		{
			t[k] = tran[t_num-1][k];
		}
	}

xit:;

	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  uig_update_trans(ptr,pblk)
**          Update transformation stack with latest matrix.
**    PARAMETERS   
**       INPUT  : 
**          ptr                     matrix directory pointer
**          pblk                    IGES matrix parameter structure
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_update_trans(ptr,pblk)
int ptr;
struct IG_igestran_rec *pblk;
{
	int i;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	if (t_num >= MAXTRAN) t_num = 0;
	for(i=0;i<12;i++)
	{
		tran[t_num][i] = pblk->trans[i];
	}
	t_ptr[t_num] = ptr;
	t_num++;
/*
.....This will update the property count if label_type
.....is 6 and there is a property entity associated with
.....the matrix.  JLS 1/27/99
*/
	if(pblk->no_prop>0 && (label_type==6 || label_type==10))
		update_counts(1,pblk->prop[0]);	
}

/*********************************************************************
**    I_FUNCTION     :  uig_chk_poly(dblk)
**          Check dimensionality of poly-line and change rel_type if 
**          necessary.
**    PARAMETERS   
**       INPUT  : 
**          dblk                    A poly-line directory record
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_chk_poly(dblk)
struct dir_rec *dblk;
{

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	if(dblk->form_no > 19) goto fexit;
	switch(dblk->form_no)
	{
		case 2:
		case 12:
			dblk->rel_type = GPOLY3D;
			break;
		case 3:
		case 13:
         dblk->rel_type = GPOLY6D;
         break;
	}

fexit:;
}

/*********************************************************************
**    I_FUNCTION     :  uig_chk_group(dblk)
**          Check type of group being translated.
**          necessary.
**    PARAMETERS   
**       INPUT  : 
**          dblk                    A group directory record
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_chk_group(dblk)
struct dir_rec *dblk;
{

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	switch(dblk->form_no)
	{
		case 3:
			dblk->rel_type = VIEWVS;
			break;
		case 16:
			dblk->rel_type = PLNASSOC;
			break;
	}
}

/*********************************************************************
**    I_FUNCTION     :  uig_check_range(dblk)
**          Check if entity on correct layer.
**    PARAMETERS   
**       INPUT  : 
**          dblk                    entity directory record
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if entity should be converted
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL uig_check_range(dblk)
struct dir_rec *dblk;
{
	int i;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	if(tig_num_range == 0)
	{
		return(UU_TRUE);
	}
	else
	{
		for(i=0;i<tig_num_range;i++)
		{
			if((dblk->level >= tig_range[i][0]) && (dblk->level <= tig_range[i][1]))
			{
				return(UU_TRUE);
			}
		}
		return(UU_FALSE);
	}
}

/*********************************************************************
**    I_FUNCTION     :  uig_invert_trans(tin,tout)
**          Invert an IGES transformation
**    PARAMETERS   
**       INPUT  : 
**          tin                     input transformation
**       OUTPUT :  
**          tout                    output transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_invert_trans(tin,tout)
UU_REAL tin[12], tout[12];
{
	int i;
	UU_REAL tf[4][3];
	UU_REAL tfr[4][3];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	for(i=0;i<3;i++)
	{
		tf[i][0] = tin[i];
		tf[i][1] = tin[i+4];
		tf[i][2] = tin[i+8];
	}
	tf[3][0] = tin[3];
	tf[3][1] = tin[7];
	tf[3][2] = tin[11];
	um_inverttf(tf, tfr);
	for(i=0;i<3;i++)
	{
		tout[i] = tfr[i][0];
		tout[i+4] = tfr[i][1];
		tout[i+8] = tfr[i][2];
	}
	tout[3] = tfr[3][0];
	tout[7] = tfr[3][1];
	tout[11] = tfr[3][2];

}

/*********************************************************************
**    I_FUNCTION     :  uig_mfcheck_for_drawing(last_rec, drawing_rec)
**          Check for a DRAWING entity in the IGES file
**    PARAMETERS   
**       INPUT  : 
**          last_rec	maximum directory record number
**       OUTPUT :  
**          drawing_rec drawing directory record if found 
**    RETURNS      : TRUE if DRAWING present and required by user
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_mfcheck_for_drawing(last_rec, drawing_rec)
int last_rec;
int *drawing_rec;
{
	struct dir_rec dblk;             /* directory record */
	int irec, i, drw_cnt, ans[MAX_DRAWING], drawing[MAX_DRAWING], nc, len;
	UU_LOGICAL found;
	char msg[400], tmp[80], **draw_list;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	found = UU_FALSE;
	drw_cnt = 0;
	i = 0;
	for(irec = 1;irec < last_irec;irec++,irec++)	
	{
		uig_get_directory(irec,&dblk);       /* get directory record */

	/*jkd5: determine type here to use for all entities, translated or not */  
		ent_type[irec/2] = IG_NUM;          /* unknown type */

		for(i=0;i<IG_NUM;i++)            /* determine type */
		{
			if(dblk.rel_type == iges_type[i])
			{
				ent_type[irec/2] = i;
				break;
			}
		}

		if(dblk.rel_type == DRAW && !found)
		{
			drw_cnt++;
			drawing[drw_cnt] = irec;
		}
	}
	if (drw_cnt>0)
	{
		draw_list = (char **) uu_malloc(drw_cnt*sizeof(char *));

		for (i=0; i<drw_cnt; i++)
		{
			sprintf(tmp, "DRAWING%d", (i+1));
			len = strlen(tmp)+1;
			draw_list[i] = (char *) uu_malloc(len*sizeof(char));
			strcpy(draw_list[i], tmp);
		}		
		sprintf(msg, "The IGES part file contains a DRAWING and MODEL\ndata. Currently DRAWINGS and MODEL geometry must\nbe translated into separate NCL Unibases.\n \nPick a drawing to translate the DRAWING.\nOr click cancel to not translate the DRAWING");
		nc = drw_cnt;
#if UU_COMP != UU_WIN2K
		uig_mflist_ans(NULL, "Translate Drawing?",  msg, draw_list, ans, &nc, 1);
#else
		iges_wntlist_ans(NULL, "Translate Drawing?",  msg, draw_list, ans, &nc, 1);
#endif
		if(nc!=0)
		{
			found = UU_TRUE;
			*drawing_rec = drawing[ans[0]];
			drawing_id = ans[0];
		}
	}
	return(found);
}

/*********************************************************************
**    I_FUNCTION     :  uig_check_for_drawing(last_rec, drawing_rec)
**          Check for a DRAWING entity in the IGES file
**    PARAMETERS   
**       INPUT  : 
**          last_rec	maximum directory record number
**       OUTPUT :  
**          drawing_rec drawing directory record if found 
**    RETURNS      : TRUE if DRAWING present and required by user
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if UU_COMP != UU_WIN2K
uig_check_for_drawing(last_rec, drawing_rec)
int last_rec;
int *drawing_rec;
{
	struct dir_rec dblk;             /* directory record */
	int irec, i, drw_cnt;
	UU_LOGICAL found;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	found = UU_FALSE;
	drw_cnt = 0;
	for(irec = 1;irec < last_irec;irec++,irec++)	
	{
		uig_get_directory(irec,&dblk);       /* get directory record */

	/*jkd5: determine type here to use for all entities, translated or not */  
		ent_type[irec/2] = IG_NUM;          /* unknown type */

		for(i=0;i<IG_NUM;i++)            /* determine type */
		{
			if(dblk.rel_type == iges_type[i])
			{
				ent_type[irec/2] = i;
				break;
			}
		}

		if(dblk.rel_type == DRAW && !found)
		{
			drw_cnt++;
			if (drw_cnt == 1)
			{
				uig_clear();
  	 			uig_str_out("The IGES part file contains a DRAWING and MODEL\n", UU_TRUE);
  	 			uig_str_out("data. Currently DRAWINGS and MODEL geometry must be  \n",UU_TRUE);
  	 			uig_str_out("translated into separate NCL Unibases.\n\n",UU_TRUE);
			}
/*
.....Check for maximum number of drawings
*/
			if (drw_cnt >= MAX_DRAWING)
			{
				if (drw_cnt == MAX_DRAWING)
				{
					sprintf(p_buff,"Maximum of %d Drawings allowed.",MAX_DRAWING);
					uig_list_out(p_buff,UU_TRUE);
				}
			}
			else
			{
				sprintf(p_buff, "Translate Drawing # %d? (y or n) ", drw_cnt); 
				uig_str_out(p_buff ,UU_TRUE); 
				if(uig_ans() == 0)		/* answer is yes */
				{
					found = UU_TRUE;
					*drawing_rec = irec;
				}
  	 			uig_str_out("\n",UU_TRUE);
			}
		}
	}
	uig_clear();
	return(found);
}
#endif

/*********************************************************************
**    I_FUNCTION     :  uig_set_def (rel_type, d_bag)
**          Set default values in parameter record structure.
**    PARAMETERS   
**       INPUT  : 
**          rel_type    IGES entity number.
**          d_bag       Parameter record structure.
**       OUTPUT :  
**          d_bag       Filled in with default values if any.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_set_def (rel_type, d_bag)
int rel_type;
char d_bag[];
{

	if (rel_type == VIEW)
	{
		struct IG_igesvie_rec *vw;
		vw = (struct IG_igesvie_rec *)d_bag;
		vw->scale = 1.0;
		vw->left = 0;
		vw->top = 0;
		vw->right = 0;
		vw->bottom = 0;
		vw->back = 0;
		vw->front = 0;
	}

	return(0);
}

/*********************************************************************
**    FUNCTION:     uig_get_join_rec
**
**    PURPOSE:      To place into the c array the elements of the
**                  join.
**    PARAMETERS
**       INPUT  :    irec:  current record number
**                   prec:  parameter line number
**                   ent_type:   type of entity
**                   rtn    contains dictionary definition of current join entity
**                   in     location along inbuf
**                   inbuf  parameter record
**                   c      character array being filled
**                   ptr    current location in c
**
**       OUTPUT :
**    RETURNS      : 0:good 1:error 9:still good, but ran out of parameters
**                                    so need to indicate to uig_get_para_rec
**                                    to exit with a status of 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_get_join_rec(irec,prec,ent_type,rtn,in,inbuf,c,ptr,ptrbig)
int irec;
int *prec;
int ent_type;
struct attr_def rtn;
int *in;
char inbuf[];
char c[];
int *ptr;
int *ptrbig;
{
#define MAXJOIN2 16
   int j2, k, l, lst_cnt, next, num, start, dummyptr, ptr2, cur_ptr;
   int out, fi, ierr, dummy, join_cnt, cnt2;
   char *loc_ptr;
   char  outbuf[120], *ptrbuf;
   UU_REAL ff;
   struct attr_def rtn2[20];

   out = 0;
   dummyptr =0;
	join_cnt = 0;
   next = *ptr;
   ptr2 = *ptrbig;

   switch(rtn.attr_type)
   {
      case FLOAT:
      case DOUBLE:
      case REAL:
      case INT:
         num = rtn.num_rows * rtn.num_cols;
         start = next;
         for(k=0;k<num;k++)
         {
/*
.....Put into outbuf the next element that is to go in c
.....If we are far enough down inbuf, (in is the location along inbuf)
.....prec will be incremented in uig_nxt_field adn the next line of
.....the parameter will be placed in inbuf.
*/
            ierr = uig_nxt_field(3,prec,in,&out,inbuf,outbuf);
            if (check_inx(next, rtn.attr_size, irec))
               return(1);
            switch(rtn.attr_type)
            {
               case FLOAT:
               case DOUBLE:
               case REAL:
                  ff = uig_atof(outbuf);
                  uig_move(&ff,&c[next],rtn.attr_size);
                  break;
               case INT:
                  fi = atoi(outbuf);
                  uig_move(&fi,&c[next],rtn.attr_size);
/*
.....saving the count for parametric curves for join
*/
				  if(ent_type == 508)
					cnt_par = fi;
                  break;
            }
            next = next + rtn.attr_size;
            if(ierr != 0)
               return(9);
         }
         next = start + rtn.attr_off;
         break;
      case CHARACTER:
         num = rtn.num_rows * rtn.num_cols;
         cur_ptr = next;
         if (check_inx(next, num, irec))
            return(1);
         ierr = uig_get_holerith(3,prec,in,&out,inbuf,&ptrbuf);
         for(l=0;l<out;l++,next++)
            c[next] = ptrbuf[l];
         c[next] = '\0';
         if(ierr != 0)
            return(9);
         next = cur_ptr + num;
         if (out > 0) uu_toolfree (ptrbuf);
         break;
      case KEY_ID:
         ierr = uig_nxt_field(3,prec,in,&out,inbuf,outbuf);
         fi = atoi(outbuf);
         if (check_inx(next, sizeof(long), irec))
            return(1);
         uig_move(&fi,&c[next],sizeof(long));
         next = next + rtn.attr_off;
         if(ierr != 0)
            return(9);
         break;
      case JOIN:
/*
.....Currently only a 141 entity will end up here so uig_join_cnt
.....would only return a 0, therefore, at this time it is not neccessary
.....to call it, but leaving it in for future use.
.....lst_cnt is the number of joins.
         lst_cnt = uig_join_cnt(ent_type,join_cnt,int_cnt,int_fields);
*/
			if(ent_type == 508)
			{
/*
.....if the preious field was 0(cnt_par=0) prevent
.....reading this field as parametric curve. 
*/
				if(cnt_par ==0) lst_cnt = -1;
				else lst_cnt = cnt_par;
				cnt_par = 0;
				ierr =0;
			}
			else
			{
         	ierr = uig_nxt_field(3,prec,in,&out,inbuf,outbuf);
         	lst_cnt = atoi(outbuf);
			}	
/*
.....Make sure there is room for lst_cnt
*/
         if (check_inx(next, sizeof(int), irec))
            return(1);
/*
.....The elements of the join are going to be further down the list
.....So determine the ptr number.
*/
         if (ptr2 == 0) ptr2 = next + JOINSIZE*MAXJOIN2;
         while (ptr2 % 16 != 0) ptr2++;
         uig_get_addr(&c[ptr2],&loc_ptr);
         if (check_inx(next, sizeof(char *), irec))
            return(1);
/*
.....Put a pointer to where in the array the join elements are into
.....the c array.
*/
         uig_move(&loc_ptr,&c[next],sizeof(char *));
         next = next + sizeof(char *);
/*
.....Put lst_cnt into c array, this is the number of join
*/
         uig_move(&lst_cnt,&c[next],sizeof(int));
         next = next + sizeof(int)*2;
         join_cnt++;
/*
.....If ierr is not equal to 0, there are no more parameters left, so exit.
*/
         if(ierr != 0)
            return(9);
         if(lst_cnt > 0)
         {
            cnt2 = uig_data_dict(rtn2,20,&rtn.attr_name[0],&dummy);
            if(cnt2 == 0)
               return(1);
         }
		 if (ent_type == 508 )
  			dummyptr = ptr2 + lst_cnt*sizeof(struct IG_par_sp_rec);
         while(lst_cnt > 0)
         {
            for(j2=1;j2<cnt2;j2++)
					uig_get_join_rec(irec,prec,ent_type,rtn2[j2],in,inbuf,c,&ptr2,&dummyptr);
			if (ent_type == 508)
				ptr2 = ptr2+ IG_EDGE_BUFSZ;
			lst_cnt--;
         }
         break;
   }

   *ptr = next;
   *ptrbig = ptr2;
   return(0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_check_for_filter(dblk)
**       Check to see if this entity type is filtered.  Filter
**       only primary entities.  If entity_mask[i] is set for
**       that type of entity, translate it; however, if
**       entity_mask[i] == 0, do not translate it (ie. filter it).
**       This only applies to independent entities; if an entity
**       is dependent, ignore the filter flag.
**    PARAMETERS
**       INPUT  :
**          dblk
**       OUTPUT :
**          none
**    RETURNS      : UU_TRUE or UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    AUTHOR       : Ed Ames  2 Apr 01
*********************************************************************/
int uig_check_for_filter(dblk)
struct dir_rec *dblk;
{
   int i, entity_type, sub_ent_switch, status;

   entity_type = dblk->rel_type;
   sub_ent_switch = dblk->sub_swt;

   for (i = 0; i < 43; i++)
   {
      if (entity_type == entity_ref[i])
         break;
   }

	status = UU_TRUE;
	if (entity_mask[i] == 0)   /* Check for filtering */
   {
      if (sub_ent_switch == 0)/* Check if subentity */
         status = UU_FALSE;   /* Do NOT translate */
      else
         status = UU_TRUE;    /* It's a subentity, therefore translate */
   }
   else
      status = UU_TRUE;       /* Not filtered, therefore translate */

	return (status);
}

/*********************************************************************
**    I_FUNCTION     :  iges_batchrun()
**       run batch of IGES.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void iges_batchrun()
{
	int status;
	if (iges_in==0)
		uio_main();
	else
	{
		status = uig_unb_opnfil(iges_igsfile);
		if ( status == 0)
			strcpy(iges_fname, iges_igsfile);
		else
			iges_fname[0] = '\0';
		if (status!=0) return;
		no_of_views = 0;
		current_dir_number = 0;
		number_of_masters = 0;
		sequence_no = 0;
		uig_in_convert();  
		t_num = 0;
	}
}
