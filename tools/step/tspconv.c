/********************************************************************
**       NAME         :  tspconv.c
**       CONTAINS:
**         utp_in_convert
**         utp_batchrun
**         utp_interrupt
**         utp_matchbar
**         utp_getcounts
**         utp_update_counts
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       tspconv.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 15:15:21
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "tigglobal.h"
#include "tstep.h"
#include "umath.h"
#include "mdrel.h"
#include "riddldef.h"
#include <stdio.h>
#include "xenv1.h"
#include "udebug.h"
#include "nclver.h"
#include "nclfc.h"
#include "nccs.h"
#include "msrf.h"
#include "ulist.h"
#include "nclxunib.h"
#include "riddle.h"

extern char iges_fname[UX_MAX_PATH_LEN];

extern int tig_max_ptlab;
extern int tig_max_lnlab;
extern int tig_max_cilab;
extern int tig_max_cvlab;
extern int tig_max_pllab;
extern int tig_max_cnlab;
extern int tig_max_pnlab;
extern int tig_max_pvlab;
extern int tig_max_sflab;

extern int iges_in;
extern int UIG_match_layer_array[];
extern UU_LOGICAL UIG_start_unmatch;
extern int UIG_reinit_lab;
extern int UIG_color_sec;
extern int UIG_layer_sec;

static int Scount[UM_MAX_RELATION];
static int Sread_percent;
static UU_LOGICAL Smodals_init=UU_FALSE;

static void S_update_counts();
int NclxMdlLoadSecondary();
void utp_getcounts(),utp_matchbar(),utp_interrupt();
static NCLX_label_struc lab[2];

/*********************************************************************
**    I_FUNCTION     :  utp_in_convert()
**          Convert STEP input file to neutral file format
**    PARAMETERS   
**       INPUT  : 
**          input    
**       OUTPUT :  
**          output   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
utp_in_convert()
{
	int num_entities,irec,i,status,lu,unlabeled=0,relnum,inc,method,subfl,nent;
	int concat,index,no_sf,no_cv;
	UX_pathname fnameu,fnames,fnamea;
	char  *ux_getenv(),label[20];
	UM_int2 v264, i2;
	UU_LOGICAL	found;
	UU_KEY_ID key;
	UTPs_step_record *ptr;
	struct UR_unistat_rec unistat;
/*
.....Load NCL custom colors
.....from Modals file
*/
/*
	if (!Smodals_init)
	{
		tsp_load_modals();
		Smodals_init = UU_TRUE;
	}
*/
/*
.....Initialize geometry labels
*/
	if (UIG_reinit_lab) utp_reset_subs();
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
	for (i=0;i<9;i++) tig_unlabeled_ent[i] = 0;
	uig_label_emptylists();
	utp_get_label_method(&method,&subfl,&concat);
	method = method*2 + subfl;
	switch (method)
	{
	case 0:
		uig_list_out("NCL style Labels.\n\n", UU_FALSE);
		break;
	case 1:
		uig_list_out("NCL style Labels with Subscripts.\n\n", UU_FALSE);
		break;
	case 2:
		if (concat) uig_list_out("Concatenated ");
		uig_list_out("Labels from STEP Components\n\n", UU_FALSE);
		break;
	case 3:
		if (concat) uig_list_out("Concatenated ");
		uig_list_out("Labels from STEP Components with Subscripts\n\n", UU_FALSE);
		break;
	case 4:
		uig_list_out("Labels from Trimmed Faces\n\n", UU_FALSE);
		break;
	case 5:
		uig_list_out("Labels from Trimmed Faces with Subscripts\n\n", UU_FALSE);
		break;
	case 6:
		uig_list_out("Labels from STEP Record Numbers\n\n", UU_FALSE);
		break;
	case 7:
		uig_list_out("Labels from STEP Record Numbers with Subscripts\n\n",
			UU_FALSE);
		break;
	case 8:
	case 9:
		uig_list_out("Labels from External Unibase\n\n", UU_FALSE);
		break;
	}
/*
.....Initialize duplicate symbol name counter
*/
	dupsym =0;
	UIG_dupcount = 0;
/*
.....Default to Inches for now
.....It looks like each entity can be stored
.....in its own units
*/
	i2 = 264;
	v264 = 0;
	setifl (&i2,&v264);
	i2 = 169;
	setscv (&i2, &NCL_version);
/*
.....Initialize the Unibase
*/
	status = uig_setup_save(&lu, fnameu, fnames, fnamea); 
	ul_default_ftype("u",fnameu);
	if (status != 0) return(UU_SUCCESS);
/*
.....Initialize colors
*/
	ncl_init_color();
/*
.....Initialize units
*/
	utp_init_units();
/*
.....Clear statistics array
*/
	UIG_dupcount = 0;
	Sread_percent = -1;
	utp_reset_translated();
/*
.....Write start of translation messages
*/
	iges_open_process_win("Converting STEP to Unibase file");
	iges_disply_as_percent(1);
/*
.....Allocate initial memory
*/
/*
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
*/
/*
.....Get a count of entities
*/
	num_entities = utp_count_entities();
/*
......Allocate memory for entities
*/
/*
	if (ent_type != 0) 
	{
		uu_toolfree(ent_type);
		ent_type = 0;
	}
	ent_type = (int *)uu_toolmalloc(sizeof(int)*num_entities);
*/
/*
.....Initialize flags
*/
	found = UU_FALSE;
/*
.....Initialize attribute settings
*/
	utp_init_attr_stack();
/*
.....Define all attributes & transformations
.....Attribute records are not guaranteed
.....to be defined prior to entity, but
.....we require the attributes defined first
*/
	ptr = (UTPs_step_record *)UU_LIST_ARRAY(&UTP_step_record);
	inc = 0;
	utp_set_attr_flag(UU_TRUE);
	for (irec=0; irec<UTP_step_numrec;irec++)
	{
		if (ptr[irec].used == 0 &&
			(ptr[irec].command == PRESENTATION_LAYER_ASSIGNMENT ||
			ptr[irec].command == CONTEXT_DEPENDENT_SHAPE_REPRESENTATION ||
			ptr[irec].command ==
				MECHANICAL_DESIGN_GEOMETRIC_PRESENTATION_REPRESENTATION))
		{
			utp_in_dispat(&ptr[irec],&relnum);
			ptr[irec].used = -1;
			inc++;
			S_update_counts(relnum,inc,num_entities,UU_TRUE);
		}
	}
/*
.....Align transformation storage
*/
	utp_align_transforms();
//	utp_print_xform_list(UU_FALSE);
/*
.....Loop through all records
*/
	utp_set_attr_flag(UU_FALSE);
	for (irec=0; irec<UTP_step_numrec;irec++)
	{
		utp_set_default_attr();
		if (ptr[irec].used == 0)
		{
			key = utp_in_dispat(&ptr[irec],&relnum);
			inc++;
			S_update_counts(relnum,inc,num_entities,UU_TRUE);
		}
	}
	uig_str_out(" 100% of file examined\n\n", UU_TRUE);
	iges_disply_as_percent(100);
/*
.....Save colors to Unibase
*/
	utp_merge_custom_color();
	ncl_update_colors(0);
/*
.....Match labels with secondary Unibase
*/
	if (method == 8 || method == 9)
	{
		NclxMdlLoadSecondary(UIG_label_unibase);
		lab[0].type = lab_flag[7];
		lab[1].type = lab_flag[6];
		strcpy(lab[0].prefix,geo_lab[7]);
		strcpy(lab[1].prefix,geo_lab[6]);
		lab[0].sub = lab[1].sub = 1;
		utp_getcounts(&no_sf,&no_cv);
		UIG_color_sec = UIG_match_color_array[6];
		UIG_layer_sec = UIG_match_layer_array[6];
		NclxMdlMatchLabels(no_sf,no_cv,0,UIG_match_tol,UIG_matchlevel,&utp_matchbar,
			lab,&utp_interrupt,UIG_match_layer_array,UIG_match_color_array,UU_NULL);
	}
/*
.....Create layers in Unibase
*/
	utp_store_layers();
/*
.....Update Unibase Statistics record
*/
	utp_get_unistat(&unistat);
	uig_put_unistat(&unistat);
/*
.....Save Unibase file
*/
	uig_str_out("\nWriting Unibase file to disk\n",UU_TRUE);	/*jkd18 */
	iges_set_process_lab("Writing Unibase file to disk", NULL);
	iges_disply_as_percent(50);
	i2 = 264;
	v264 = utp_get_units() - 1;
	setifl (&i2,&v264);
	uig_save(lu, fnameu, fnames, fnamea);

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
/*
.....Now print statistics
*/
	utp_print_statistics();
/*
.....Display duplicates summary
*/
	if (UIG_nodups)
	{
		if (UIG_dupcount == 0)
			strcpy(p_buff,"\nNo duplicates found\n");
		else
			sprintf(p_buff,"\nDuplicates not translated = %-d\n",UIG_dupcount);
		uig_list_out(p_buff,UU_TRUE);
	}
	uig_list_out(" \n", UU_FALSE);
/*
.....Reset used flags that were set to -1 in case the same step file
.....is used again in same session.
*/
	ptr = (UTPs_step_record *)UU_LIST_ARRAY(&UTP_step_record);
	for (irec=0; irec<UTP_step_numrec;irec++)
	{
		if (ptr[irec].used == -1 &&
			(ptr[irec].command == PRESENTATION_LAYER_ASSIGNMENT ||
			ptr[irec].command == CONTEXT_DEPENDENT_SHAPE_REPRESENTATION ||
			ptr[irec].command ==
				MECHANICAL_DESIGN_GEOMETRIC_PRESENTATION_REPRESENTATION))
		{
			ptr[irec].used = 0;
		}
	}
/*
.....Close Process Bar &
.....Redisplayed EXIT Button
*/
	utp_free_lists(UU_FALSE);
	iges_close_process_win();
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  utp_batchrun()
**       Run the STEP convertor in batch.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_batchrun()
{
	int status;
/*
.....Load modals file
*/
	utp_load_modals();
/*
.....Step out
*/
	if (iges_in==0)
		utp_out_main();
/*
.....Step in
*/
	else
	{
		status = utp_read_step_file(iges_igsfile);
		if ( status == 0)
			strcpy(iges_fname, iges_igsfile);
		else
			iges_fname[0] = '\0';
		if (status!=0) return;
		utp_in_convert();  
		utp_free_lists(UU_TRUE);
		t_num = 0;
	}
}

/*********************************************************************
**    I_FUNCTION     :  utp_interrupt(stopfl)	
**          Checks for interrupt. (Currently stubs for label matching)
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          stopfl     UU_TRUE: An interrupt was entered.
**                     UU_FALSE: otherwise.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_interrupt(stopfl)
UU_LOGICAL *stopfl;
{
	*stopfl = UU_FALSE;
}

/*********************************************************************
**    I_FUNCTION     :  utp_matchbar(ilev,nmatch)
**          Stubs routine for label matching.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_matchbar(ilev,nmatch)
int ilev,nmatch;
{}

/*********************************************************************
**    I_FUNCTION     :  utp_getcounts(no_sf,no_cv)
**          Retrieves the translated entity counts.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          no_sf    Number of translated surfaces.
**          no_cv    Number of translated curves.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_getcounts(no_sf,no_cv)
int *no_sf,*no_cv;
{
	*no_sf = Scount[NCL_TRIMSF_REL-1];
	*no_cv = Scount[UM_RBSPLCRV_REL-1] + Scount[UM_COMPCRV_REL-1] + 
		Scount[UM_LINE_REL-1] + Scount[UM_CIRCLE_REL-1];
}

/*********************************************************************
**    I_FUNCTION     :  S_update_counts(relnum,irec,numrec)	
**          Update translated entity counts.
**    PARAMETERS   
**       INPUT  : 
**          relnum   Relation number of entity translated if >0.
**          irec     Current entity being processed.
**          relnum   Number of entities to process.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_update_counts(relnum)
int relnum;
{
	S_update_counts(relnum,0,0,UU_FALSE);
}

/*********************************************************************
**    I_FUNCTION     :  S_update_counts(relnum,irec,numrec)	
**          Update translated entity counts.
**    PARAMETERS   
**       INPUT  : 
**          relnum   Relation number of entity translated if >0.
**          irec     Current entity being processed.
**          relnum   Number of entities to process.
**          flag     UU_TRUE: Update count and progress bar.
**                   UU_FALSE: Update count only.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_update_counts(relnum,irec,numrec,flag)
int relnum,irec,numrec;
{
	int percent,i;
	char buf[80];
/*
.....Update entity's count
*/
	if (relnum > 0 && relnum < UM_MAX_RELATION) Scount[relnum-1]++;

	if (flag)
	{
 		percent = irec*100 / numrec;
		if (percent % 2 == 0)
		{
			if (percent != Sread_percent && percent > 0 && percent < 100)
			{
	   		if (percent % 10 == 0)
				{
					sprintf(buf, "  %d%% of file examined\n", percent);
					uig_str_out(buf, UU_TRUE);
				}
	   		Sread_percent = percent;
				iges_disply_as_percent(percent);
			}	
		}
	}
}
