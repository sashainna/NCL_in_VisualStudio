/*********************************************************************
**    NAME         :  tigglobal.h
**       CONTAINS:
**    global variables used in IGES and NclSolid
**     MODULE NAME AND RELEASE LEVEL 
**       tigglobal.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 11:48:07
*********************************************************************/

#ifndef TIGGLOBAL

#include "mdattr.h"
#include "ulist.h"
#include "xenv1.h"

#define MAX_VIEWS 100



#ifdef TIGMAIN
#define EXT
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

#define NREL 12
/*
.....Initialized variables
.....Must be declared as external in following section
*/
#ifdef TIGMAIN
int UIG_edge_color=-1, UIG_srf_edge=-1;
int UIG_from_sw = 0;
int UIG_unmatch_sec = 0;
int UIG_matchlevel = 4;
int UIG_start_unmatch = 0;
int UIG_regressive = 0;
int UIG_ncolor_iges = 0;
int UIG_unibase_entities = 0;
int tig_max_cvlab =0 ;
int tig_max_sflab = 0 ;
int iges_process = 0;
int iges_in = 0;
int Iges_batch = 0;
int dupsym = 0;
int DDC_displayable_flag = UM_DISPLAYABLE;
int entity_mask[37] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
int entity_out[37] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
int entity_ref[37] = {100, 102, 104, 106, 606, 906, 108, 110, 112, 114,
	116, 118, 120, 122, 126, 128, 140, 141, 142, 143, 144, 202, 206, 210, 212,
	214, 216, 222, 228, 402, 404, 406, 408, 410, 605, 602, 907};
int ipt=0, iln=0, ipl=0, ici=0, icn=0, icv=0, isf=0, isg=0, ipn=0, ipv=0;
int ixx=0;
int UIG_match_color_array[] = {-1,-1,-1,-1,-1,-1,-1};
int UIG_match_layer_array[] = {-1,-1,-1,-1,-1,-1,-1};
UU_LOGICAL UIG_nodups=UU_FALSE;
UU_LOGICAL UIG_reinit_lab=UU_TRUE;
UU_LOGICAL UIG_drawing_only = UU_FALSE;
UU_LOGICAL label_comp_element=UU_FALSE;
UU_LOGICAL shade_set=UU_TRUE;
UU_KEY_ID *tig_unlabeled_keys = UU_NULL;
UU_REAL UIG_match_tol=0., UIG_match_tol_disp=0.;
UU_REAL *UIG_regfactor = UU_NULL;
UU_REAL UIG_units_factor = 1.;
UU_KEY_ID *UIG_matchkeys = UU_NULL;
UU_KEY_ID *UIG_checkkeys = UU_NULL;
int UIG_lucency = 100;
/*
........External declarations
*/
#else
EXT int UIG_edge_color,UIG_srf_edge;
EXT int UIG_from_sw;
EXT int UIG_unmatch_sec;
EXT int UIG_matchlevel;
EXT int UIG_start_unmatch;
EXT int UIG_regressive;
EXT int UIG_ncolor_iges;
EXT int UIG_unibase_entities;
EXT int tig_max_cvlab;
EXT int tig_max_sflab;
EXT int iges_process;
EXT int iges_in;
EXT int Iges_batch;
EXT int dupsym;
EXT int DDC_displayable_flag;
EXT int entity_mask[],entity_ref[],entity_out[];
EXT int ipt,iln,ipl,ici,icn,icv,isf,isg,ipn,ipv,ixx;
EXT int UIG_match_color_array[];
EXT int UIG_match_layer_array[];
EXT UU_LOGICAL UIG_nodups;
EXT UU_LOGICAL UIG_reinit_lab;
EXT UU_LOGICAL UIG_drawing_only;
EXT UU_LOGICAL label_comp_element;
EXT UU_LOGICAL shade_set;
EXT UU_KEY_ID *tig_unlabeled_keys;
EXT UU_REAL UIG_match_tol, UIG_match_tol_disp;
EXT UU_REAL *UIG_regfactor;
EXT UU_REAL UIG_units_factor;
EXT UU_KEY_ID *UIG_matchkeys;
EXT UU_KEY_ID *UIG_checkkeys;
EXT int UIG_lucency;
#endif
/*
,.....Unitialized variables
*/
EXT int UIG_regcount;
EXT int UIG_splitccrv;
EXT int UIG_color_iges;
EXT int UIG_conv_opt;
EXT int UIG_lab_opt;
EXT int UIG_from_trimsrf;
EXT int UIG_max_lab;
EXT int UIG_change_uv;
EXT int UIG_dupcount;
EXT int tig_unlabeled;
EXT int tig_unlabeled_ent[9];
EXT int output_units;
EXT int output_normals;
EXT int sequence_no;
EXT int number_of_masters;
EXT int current_dir_number;
EXT int no_of_views;
EXT int lab_flag[NREL];
EXT int label_type;
EXT int view_keys[MAX_VIEWS+2][2];
EXT char p_buff[300];
EXT char geo_lab[11][7];
EXT UU_REAL UIG_comp_tol;
EXT UU_REAL uio_units;
EXT UX_pathname UIG_label_unibase;
EXT UX_pathname iges_outfile,iges_ptfile,iges_unifile,iges_igsfile;
EXT UU_LIST UIG_sflist_keys;
EXT UU_LIST *UIO_surf_list;

#define TIGGLOBAL
#undef EXT
#endif
