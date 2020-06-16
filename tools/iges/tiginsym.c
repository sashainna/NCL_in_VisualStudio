/*********************************************************************
**    NAME         :  tiginsym.c
**       CONTAINS:
**					uig_in_instance
**					uig_in_symnote
**					uig_in_master
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tiginsym.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 14:51:35
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "mcrv.h"
#include "mdattr.h"
#include "udebug.h"
#include "nccs.h"
#include "mdrel.h"
#include "mattr.h"

extern int MAX_PARA_REC ;
UU_LOGICAL instance_trans = UU_FALSE;
UU_REAL instance_tf[4][3];
struct KEY_LIST {
	int drec;
	int key;
	struct KEY_LIST *next;
	} ;
struct KEY_LIST *head_key_list=0, *last_key=0;
int UIG_decompose_chc = 0; /* No duplicated is disabled */
extern int UIG_decompose;

/*********************************************************************
**    I_FUNCTION     :  uig_in_instance(gblk,dblk,pblk,key)
**				Create a symbol from an iges sub-figure instance.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_instance(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record structure */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igessfi_rec *pblk;			/* iges instance record */
	UU_KEY_ID *key;
	{
	char p_buff[80];
	struct IG_igessfd_rec *sub_fig;			/* iges group record */
	struct dir_rec sub_dir;
	UU_KEY_ID e_key, lkey, *p, *inst_ptr_geom=UU_NULL, *inst_ptr_notes=UU_NULL;
	UU_KEY_ID master_key;
	int i,irec,status,num, num_inst_geom, num_inst_notes, svnodups;
	struct KEY_LIST *key_list;
	char *c = UU_NULL, *c1 = UU_NULL;
	char *uu_toolmalloc();
	struct dir_rec ldir;
	UU_REAL *t;
	UU_LOGICAL lsvlab;
	struct NCL_fixed_databag eptr;
	struct UM_transf_rec tran;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_instance"));

	/* first get sub-fig definition data */

	c = uu_toolmalloc(MAX_PARA_REC);
	c1 = uu_toolmalloc(MAX_PARA_REC);
	irec = pblk->sfd_ptr;
	if(number_of_masters == 0)
		{
		if (head_key_list)
		  {
		  key_list = head_key_list;
		  while (key_list)
			{
			last_key = key_list->next;
			uu_toolfree(key_list);
			key_list = last_key;
			}
		  head_key_list = 0;
		  }
		uig_in_master(irec, gblk, &master_key);
		}
	else
		{
		key_list = head_key_list;
		do {
			if (irec == key_list->drec)
				{
				master_key = key_list->key;
				goto cont;
				}
			key_list = key_list->next;
			} while (key_list);
		uig_in_master(irec, gblk, &master_key);
		}
cont:;
	status = uig_get_data(irec,0,&sub_dir,c1);
	check_color(&sub_dir);
	sub_fig = ( struct IG_igessfd_rec *) c1;
/*
....Load the transformation matrix form the instance and not the symbol
....uig_load_current_matrix(sub_dir.matrix_ptr, &t);
*/
	uig_load_current_matrix(dblk->matrix_ptr, &t);
	uig_igtf_to_tf(t, instance_tf);
	
	t[3] =  t[3] + pblk->xyz[0];
	t[7] =  t[7] + pblk->xyz[1];
	t[11] =  t[11] + pblk->xyz[2];

	t[0] = t[0]  * pblk->scale;
	t[5] = t[5] * pblk->scale;
	t[10] = t[10] * pblk->scale;
	instance_tf[0][0] = t[0];
	instance_tf[1][1] = t[5];
	instance_tf[2][2] = t[10];

	instance_tf[3][0] = t[3];
	instance_tf[3][1] = t[7];
	instance_tf[3][2] = t[11];
/*
.......Added option to decompose symbols during translation - ASF 1/7/14.
*/
	instance_trans = UU_TRUE;
	if (!UIG_decompose)
	{
		DDC_displayable_flag = UM_NEVERDISPLAYABLE;
		lsvlab = label_comp_element;
		label_comp_element = UU_TRUE;
	}
	else
	{
		svnodups = UIG_nodups;
		if (UIG_decompose_chc==0)
		{
			UIG_nodups = UU_FALSE;
		}
		um_vctmsc(instance_tf[3],unit_scale,instance_tf[3]);
	}
	/* now loop thru entities */

	p = sub_fig->cid;
	num = sub_fig->num;
	inst_ptr_geom = (UU_KEY_ID *)uu_toolmalloc(num*sizeof(UU_KEY_ID));
	inst_ptr_notes = (UU_KEY_ID *)uu_toolmalloc(num*sizeof(UU_KEY_ID));
	num_inst_geom = 0;
	num_inst_notes = 0;
	for(i=0;i<num;i++)
		{
		irec = p[i];
		status = uig_get_data(irec,0,&ldir,c);
		check_color(&ldir);
		ldir.view_ptr = dblk->view_ptr;
		check_color(&ldir);
		lkey = 0; /*jkd5: test lkey to see if translated */
		if(status == 0)
			{
			switch(ldir.rel_type)
				{
				case GLINE:
					uig_in_line(gblk,&ldir,c,&lkey);
					break;
				case GPOINT:
					uig_in_point(gblk,&ldir,c,&lkey);
					break;
				case GARC:
					uig_in_arc(gblk,&ldir,c,&lkey);
					break;
				case GCONIC:
					uig_in_conic(gblk,&ldir,c,&lkey);
					break;
				case GCOMPOSITE:
					uig_in_comp(gblk,&ldir,c,&lkey);
					break;
				case GGROUP:
					uig_in_group(gblk,&ldir,c,&lkey);
					break;
				case GPOLY:
					uig_in_poly2(gblk,&ldir,c,&lkey);
					break;
				case GPOLY3D:
					uig_in_poly3(gblk,&ldir,c,&lkey);
					break;
				case GPOLY6D:
					uig_in_poly6(gblk,&ldir,c,&lkey);
					break;
				case GSPLINE:
					uig_in_spline(gblk,&ldir,c,&lkey);
					break;
				case GRSPLINE:
					uig_in_rspline(gblk,&ldir,c,&lkey);
					break;
				case GNOTE:
/* 
.....vp 7-jan-97 replaced by gnote routine, now look for fsrs
					uig_in_symnote(gblk,&ldir,c,&lkey);
*/
					uig_in_gnote(gblk,&ldir,c,&lkey);
					break;
				case GRSPLSRF:
					uig_in_rsplsrf(gblk,&ldir,c,&lkey);
					break;
				case GRULEDSRF:
					uig_in_ruledsrf(gblk,&ldir,c,&lkey);
					break;
				case GREVSRF:
					uig_in_revsrf(gblk,&ldir,c,&lkey);
					break;
				case GTBCYSRF:
					uig_in_tbcysrf(gblk,&ldir,c,&lkey);
					break;
/*
.....Adding trim surfaces and bounded surface entities. JLS 1/13/99
*/
				case GTRIMSRF:
/*
.....for trimsrf: donot load the matrix form the instance , instead use 
.....this matrix for translating the trimsf.
*/
					uig_pop_current_matrix();
					uig_in_trimsrf(gblk,&ldir,c,&lkey);
					uig_load_current_matrix(dblk->matrix_ptr, &t);
					break;
				case GBDSRF:
					uig_in_bdsrf(gblk,&ldir,c,&lkey);
					break;
                case GOFFSTSRF:
                    uig_in_offstsrf(gblk,&ldir,c,&lkey);
                    break;
/***** currently no drafting in symbols *****/
/**
				case GANGDIM:
					uig_in_angdim(gblk,&ldir,c,&lkey);
					break;
				case GDIADIM:
					uig_in_diadim(gblk,&ldir,c,&lkey);
					break;
				case GLEADER:
					uig_in_leader(gblk,&ldir,c,&lkey);
					break;
				case GLINDIM:
					uig_in_lindim(gblk,&ldir,c,&lkey);
					break;
				case GRADIM:
					uig_in_radim(gblk,&ldir,c,&lkey);
					break;
**/
				default:
					break;
				}
			}
		if (lkey > 0)
			{
			if (ldir.rel_type == GNOTE)
				{
				inst_ptr_notes[num_inst_notes] = lkey;
				num_inst_notes++;
				}
			else
				{
				inst_ptr_geom[num_inst_geom] = lkey;
				num_inst_geom++;
				}
			}
		else
			{
			sprintf(p_buff,"(DREC = %d) Entity of subfigure not processed; rel_num=%d\n",
			  dblk->drec_num,ldir.rel_type);   /*jkd19 */
			uig_list_out(p_buff,UU_TRUE);
			}
		update_counts(lkey, irec); /*jkd5: update counts */
		}
		 
	/* now create instance entity */
/*
.......Do not create the actual instance if the decompose flag has been set.
.......Only keep the associated geometry - ASF 1/7/14.
*/
	instance_trans = UU_FALSE;
	if (!UIG_decompose)
	{
		DDC_displayable_flag = UM_DISPLAYABLE;
		label_comp_element = lsvlab;
		uig_map_instance( master_key, num_inst_geom, inst_ptr_geom,
			num_inst_notes, inst_ptr_notes, dblk, &e_key);
		*key = e_key;
	}
	else
	{
		UIG_nodups = svnodups;
		*key = 0;
	}
	if (c) uu_toolfree(c);
	if (c1) uu_toolfree(c1);
	if (inst_ptr_geom) uu_toolfree(inst_ptr_geom);
	if (inst_ptr_notes) uu_toolfree(inst_ptr_notes);
	uig_pop_current_matrix();

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    I_FUNCTION     :  uig_in_symnote(gblk,dblk,pblk,key)
**				Create a data base model note from an iges general note.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_in_symnote(gblk,dblk,pblk,key)
	struct global_rec *gblk;				/* global record sturcture */
	struct dir_rec *dblk;					/* directory record */
	struct IG_igesnote_rec *pblk;		/* iges gnote parameter record */
	UU_KEY_ID *key;
	{
	UU_REAL *t;
	int lkey;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_symnote"));

	*key = 0;

	uig_load_current_matrix(dblk->matrix_ptr, &t);

	/* now map to data base representation */

	uig_map_symnote(dblk,t,pblk,&lkey);
	uig_pop_current_matrix();
	*key = lkey;

	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION     :  uig_in_master(master_rec, gblk, key)
**				Create a symbol master from an iges sub-figure instance.
**    PARAMETERS   
**       INPUT  : 
**				master_rec						master directory record
**				gblk 							global block
**				irec 							sub_fig index 
**       OUTPUT :  
**				key							master key
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_in_master(master_rec, gblk, key)
	int master_rec;
	struct global_rec *gblk;				/* global record sturcture */
	UU_KEY_ID *key;
	{
	char p_buff[80];
	struct IG_igessfd_rec *sub_fig;			/* iges group record */
	struct dir_rec sub_dir;
	UU_KEY_ID e_key, lkey, *p;
	UU_KEY_ID *master_ptr_geom=UU_NULL, *master_ptr_notes=UU_NULL;
	int i,irec,status,num, num_master_geom, num_master_notes;
	struct KEY_LIST *key_ptr;
	char *c=UU_NULL,*c1=UU_NULL;
	char *uu_toolmalloc();
	struct dir_rec ldir;
	UU_REAL *t;
	UU_LOGICAL lsvlab;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_mster()"));

	/* first get sub-fig definition data */

	c1 = uu_toolmalloc(MAX_PARA_REC);
	c = uu_toolmalloc(MAX_PARA_REC);
	status = uig_get_data(master_rec,0,&sub_dir,c1);
	check_color(&sub_dir);
	sub_fig = ( struct IG_igessfd_rec *) c1;

	/* now loop thru entities */

	uig_load_current_matrix(sub_dir.matrix_ptr, &t);
	DDC_displayable_flag = UM_NEVERDISPLAYABLE;
	lsvlab = label_comp_element;
	label_comp_element = UU_TRUE;

	p = sub_fig->cid;
	num = sub_fig->num;
	if (num > 0)
	  {
	  master_ptr_geom = (UU_KEY_ID *)uu_toolmalloc(num*sizeof(UU_KEY_ID));
	  master_ptr_notes = (UU_KEY_ID *)uu_toolmalloc(num*sizeof(UU_KEY_ID));
	  }
	num_master_geom = 0;
	num_master_notes = 0;
	for(i=0;i<num;i++)
		{
		irec = p[i];
		status = uig_get_data(irec,0,&ldir,c);
		check_color(&ldir);
		lkey = 0;
		if(status == 0)
			{
			switch(ldir.rel_type)
				{
				case GLINE:
					uig_in_line(gblk,&ldir,c,&lkey);
					break;
				case GPOINT:
					uig_in_point(gblk,&ldir,c,&lkey);
					break;
				case GARC:
					uig_in_arc(gblk,&ldir,c,&lkey);
					break;
				case GCONIC:
					uig_in_conic(gblk,&ldir,c,&lkey);
					break;
				case GCOMPOSITE:
					uig_in_comp(gblk,&ldir,c,&lkey);
					break;
				case GGROUP:
					uig_in_group(gblk,&ldir,c,&lkey);
					break;
				case GPOLY:
					uig_in_poly2(gblk,&ldir,c,&lkey);
					break;
				case GPOLY3D:
					uig_in_poly3(gblk,&ldir,c,&lkey);
					break;
				case GPOLY6D:
					uig_in_poly6(gblk,&ldir,c,&lkey);
					break;
				case GSPLINE:
					uig_in_spline(gblk,&ldir,c,&lkey);
					break;
				case GRSPLINE:
					uig_in_rspline(gblk,&ldir,c,&lkey);
					break;
				case GNOTE:
/* 
.....vp 7-jan-97 replaced by gnote routine, now look for fsrs
					uig_in_symnote(gblk,&ldir,c,&lkey);
*/
					uig_in_gnote(gblk,&ldir,c,&lkey);
					break;
				case GRSPLSRF:
					uig_in_rsplsrf(gblk,&ldir,c,&lkey);
					break;
				case GRULEDSRF:
					uig_in_ruledsrf(gblk,&ldir,c,&lkey);
					break;
				case GREVSRF:
					uig_in_revsrf(gblk,&ldir,c,&lkey);
					break;
				case GTBCYSRF:
					uig_in_tbcysrf(gblk,&ldir,c,&lkey);
					break;
/*
.....Added GTRIMSRF and GBDSRF JLS 1/13/99
*/
				case GTRIMSRF:
					uig_in_trimsrf(gblk,&ldir,c,&lkey);
					break;
				case GBDSRF:
					uig_in_bdsrf(gblk,&ldir,c,&lkey);
					break;
                case GOFFSTSRF:
                    uig_in_offstsrf(gblk,&ldir,c,&lkey);
                    break;

/***** currently no drafting in symbols *****/
/**
				case GANGDIM:
					uig_in_angdim(gblk,&ldir,c,&lkey);
					break;
				case GDIADIM:
					uig_in_diadim(gblk,&ldir,c,&lkey);
					break;
				case GLEADER:
					uig_in_leader(gblk,&ldir,c,&lkey);
					break;
				case GLINDIM:
					uig_in_lindim(gblk,&ldir,c,&lkey);
					break;
				case GRADIM:
					uig_in_radim(gblk,&ldir,c,&lkey);
					break;
***** end of currently no drafting in symbols *****/
				default:
					break;
				}
			}
			if (lkey > 0)
				{
				if (ldir.rel_type == GNOTE)
					{
					master_ptr_notes[num_master_notes] = lkey;
					num_master_notes++;
					}
				else
					{
					master_ptr_geom[num_master_geom] = lkey;
					num_master_geom++;
					}
				}
			else
				{
				sprintf(p_buff,"(DREC = %d) Entity of subfigure not processed; rel_num=%d\n",
				  master_rec,ldir.rel_type);   /*jkd19 */
				uig_list_out(p_buff,UU_TRUE);
				}
			update_counts(lkey, irec); 
		}

	label_comp_element = lsvlab;

	/* now create master and instance entities */

	uig_map_master(sub_fig->name, num_master_geom, master_ptr_geom,
	num_master_notes, master_ptr_notes, &sub_dir, &e_key);
	key_ptr = (struct KEY_LIST *)uu_toolmalloc(sizeof(struct KEY_LIST));
	key_ptr->drec = master_rec;
	key_ptr->key  = e_key;
	key_ptr->next = 0;
	if (head_key_list == 0) head_key_list = key_ptr;
	else last_key->next = key_ptr;
	last_key = key_ptr;
	number_of_masters++;
	*key = e_key;
	update_counts(e_key, master_rec);
	if (c) uu_toolfree(c);
	if (c1) uu_toolfree(c1);
	if (master_ptr_geom) uu_toolfree(master_ptr_geom);
	if (master_ptr_notes) uu_toolfree(master_ptr_notes);
	uig_pop_current_matrix();

	uu_dexit;
	return(UU_SUCCESS);
	}
