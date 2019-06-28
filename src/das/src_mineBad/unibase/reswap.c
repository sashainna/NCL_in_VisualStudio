/*********************************************************************
**    NAME         :  reswap.c
**       CONTAINS:
**       ur_active_unibase()
**       ur_getu_work()
**       ur_getu_second()
**       ur_saveu_active()
**       ur_restore_second_material()
**       memrel()
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**        reswap.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:11:37
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "rmtuple.h"
#include "riddle.h"
#include "ribase.h"
#include "rienvtab.h"
#include "rstack.h"
#include "xenv1.h"
#include "mattrddl.h"
#include "mdraw.h"
#include "mlabddl.h"

extern int UR_active;
struct UR_rcb_rec   URW_rcb[UR_NUM_REL], UR2_rcb[UR_NUM_REL];
extern UU_KEY_ID UR_last_mod_mkey;
extern UU_KEY_ID UR_low_key;
extern UU_REL_ID UR_last_mod_rkey;
extern UR_REL_NUM UR_transf_relnum;
extern UU_LOGICAL UR_dd_inited;
extern UU_LOGICAL UR_sav_all;
extern UU_LOGICAL UR_save_modals;
extern UU_LOGICAL UR_del_mark;
extern UU_LOGICAL UR_del_stack_enabled;
extern UU_LOGICAL UR_del_started;
extern UU_LOGICAL UR_changed;
extern UU_LOGICAL UR_load_env;

UU_KEY_ID URW_last_mod_mkey, UR2_last_mod_mkey; 
UU_KEY_ID URW_low_key, UR2_low_key; 
UU_REL_ID URW_last_mod_rkey, UR2_last_mod_rkey; 
UR_REL_NUM URW_transf_relnum, UR2_transf_relnum; 
UU_LOGICAL URW_dd_inited, UR2_dd_inited;  
UU_LOGICAL URW_sav_all, UR2_sav_all;   
UU_LOGICAL URW_save_modals, UR2_save_modals; 
UU_LOGICAL URW_del_mark, UR2_del_mark;  
UU_LOGICAL URW_del_stack_enabled, UR2_del_stack_enabled;
UU_LOGICAL URW_del_started, UR2_del_started; 
UU_LOGICAL URW_changed, UR2_changed; 
UU_LOGICAL URW_load_env, UR2_load_env; 

long   URW_default_transf, UR2_default_transf;
int   URW_chg_cnt,  UR2_chg_cnt; 

/*
...vp 2/16/98 environment table section setup data
......following structures are hardcoded and need be changed if
......ddl tool is changed and rienvtab.c contains different
......data than it has now.
*/

extern struct UR_env_table_rec UR_environ_table[];
struct UM_attrmdl_rec UMW_attrmdl, UM2_attrmdl;
struct UM_attrmdl_rec UMW_n_attrmdl, UM2_n_attrmdl;
struct UM_mtrlmdl_rec UMW_mtrlmdl, UM2_mtrlmdl;
struct UM_mtrlmdl_rec UMW_n_mtrlmdl, UM2_n_mtrlmdl;
struct UM_dispattr_rec UMW_dispattr, UM2_dispattr;
struct UM_dispattr_rec UMW_n_dispattr, UM2_n_dispattr;
struct UM_drwmdl_rec UMW_drwmdl, UM2_drwmdl;
struct UM_drwmdl_rec UMW_n_drwmdl, UM2_n_drwmdl;
struct UM_labelmdl_rec UMW_labelmdl, UM2_labelmdl;
struct UM_labelmdl_rec UMW_n_labelmdl, UM2_n_labelmdl;
struct UR_unimod_rec URW_unimod, UR2_unimod;
struct UR_unimod_rec URW_n_unimod, UR2_n_unimod;

struct UR_env_table_rec URW_environ_table[] = {
{"UM_attrmdl", (char *)&UMW_attrmdl,(char *)&UMW_n_attrmdl,sizeof(struct UM_attrmdl_rec)},
{"UM_mtrlmdl", (char *)&UMW_mtrlmdl,(char *)&UMW_n_mtrlmdl,sizeof(struct UM_mtrlmdl_rec)},
{"UM_dispattr", (char *)&UMW_dispattr,(char *)&UMW_n_dispattr,sizeof(struct UM_dispattr_rec)},
{"UM_drwmdl", (char *)&UMW_drwmdl,(char *)&UMW_n_drwmdl,sizeof(struct UM_drwmdl_rec)},
{"UM_labelmdl", (char *)&UMW_labelmdl,(char *)&UMW_n_labelmdl,sizeof(struct UM_labelmdl_rec)},
{"UR_unimod", (char *)&URW_unimod,(char *)&URW_n_unimod,sizeof(struct UR_unimod_rec)},
{"", 0, 0, 0}
};

struct UR_env_table_rec UR2_environ_table[] = {
{"UM_attrmdl", (char *)&UM2_attrmdl,(char *)&UM2_n_attrmdl,sizeof(struct UM_attrmdl_rec)},
{"UM_mtrlmdl", (char *)&UM2_mtrlmdl,(char *)&UM2_n_mtrlmdl,sizeof(struct UM_mtrlmdl_rec)},
{"UM_dispattr", (char *)&UM2_dispattr,(char *)&UM2_n_dispattr,sizeof(struct UM_dispattr_rec)},
{"UM_drwmdl", (char *)&UM2_drwmdl,(char *)&UM2_n_drwmdl,sizeof(struct UM_drwmdl_rec)},
{"UM_labelmdl", (char *)&UM2_labelmdl,(char *)&UM2_n_labelmdl,sizeof(struct UM_labelmdl_rec)},
{"UR_unimod", (char *)&UR2_unimod,(char *)&UR2_n_unimod,sizeof(struct UR_unimod_rec)},
{"", 0, 0, 0}
};

/*********************************************************************
**    E_FUNCTION     :  ur_active_unibase()
**       Returns the currently active Unibase designator (1 = Main,
**       2 = Secondary).
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      :  1 = Main Unibase is active, 2 = Secondary.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ur_active_unibase()
{
	return(UR_active);
}

/*********************************************************************
**    E_FUNCTION     :  ur_getu_work()
**       Switch to primary NCL unibase if active is secondary unibase,
**       saving all rcb structures & control vars. 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_getu_work()
{
   /* local  parameter declarations */
   int   i;                  /* an index                              */

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
   uu_denter(UU_RTRC,(us,"ur_getu_work with %d relations",UR_NUM_REL));
 if (UR_active == 1) goto done;
/* 
...Save active unibase as secondary
*/
 ur_saveu_active(2);

   /* set global unibase variables */
   UR_low_key = URW_low_key;
   UR_last_mod_mkey = URW_last_mod_mkey;      /* key of last modified master tuple*/
   UR_last_mod_rkey = URW_last_mod_rkey;       /* key of last modified tuple         */
   UR_transf_relnum = URW_transf_relnum;       /* set to something it can't be   */
   UR_default_transf   = URW_default_transf; /* no default matrix yet         */
   UR_dd_inited = URW_dd_inited;             /* no data dictionary yet */
   UR_sav_all = URW_sav_all;                   /* save all tuples               */
   UR_save_modals = URW_save_modals;         /* save modals in save file */
   UR_del_mark = URW_del_mark;                 /* set mark del stack            */
   UR_del_stack_enabled = URW_del_stack_enabled;   /* disable del stack               */
   UR_del_started = URW_del_started;

   /* set default save,load pathname to null */
/*   UR_dpn[0] = '\000';*/

/* 
...get all relations as saved 
*/
   for(i = 0; i <= UR_MAX_REL; i++) 
   {
      UR_rcb[i].status = URW_rcb[i].status;
      UR_rcb[i].rel_num = URW_rcb[i].rel_num;
      UR_rcb[i].n_ent = URW_rcb[i].n_ent;
      UR_rcb[i].init_ent = URW_rcb[i].init_ent;
      UR_rcb[i].n_varl = URW_rcb[i].n_varl;
      UR_rcb[i].rel_flags = URW_rcb[i].rel_flags;
      UR_rcb[i].last_accessed_index = URW_rcb[i].last_accessed_index;
      UR_rcb[i].last_active_index = URW_rcb[i].last_active_index;
      UR_rcb[i].active_tuple_cnt = URW_rcb[i].active_tuple_cnt;
      UR_rcb[i].tuple_size = URW_rcb[i].tuple_size;
      UR_rcb[i].bmap_size = URW_rcb[i].bmap_size;
      UR_rcb[i].ent_ptr = URW_rcb[i].ent_ptr;
      UR_rcb[i].bmap_ptr = URW_rcb[i].bmap_ptr;
   }
/*
...vp 2/16/98 get saved environment from URW
*/
   for (i=0; URW_environ_table[i].name[0] != '\0'; i++)
	{
      UR_environ_table[i].adrs = URW_environ_table[i].adrs;
      UR_environ_table[i].new_adrs = URW_environ_table[i].new_adrs;
      UR_environ_table[i].length = URW_environ_table[i].length;
	}

   UR_active = 1;

done:
   uu_dexit ;
   return(0) ;
}
/*********************************************************************
**    E_FUNCTION     :  ur_getu_second()
**       Switch to secondary unibase if active is primary NCL unibase,
**       saving all rcb structures & control vars. 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_getu_second()
{

   /* local  parameter declarations */
   int   i;                  /* an index                              */

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
   uu_denter(UU_RTRC,(us,"ur_getu_work with %d relations",UR_NUM_REL));
   if (UR_active == 2) goto done;
/* 
...Save active unibase as work unibase 
*/
   ur_saveu_active(1);

   /* set global unibase variables */
   UR_last_mod_mkey = UR2_last_mod_mkey;      /* key of last modified master tuple*/
   UR_low_key = UR2_low_key;      /* key of last modified master tuple*/
   UR_last_mod_rkey = UR2_last_mod_rkey;       /* key of last modified tuple         */
   UR_transf_relnum = UR2_transf_relnum;       /* set to something it can't be   */
   UR_default_transf   = UR2_default_transf; /* no default matrix yet         */
   UR_dd_inited = UR2_dd_inited;             /* no data dictionary yet */
   UR_sav_all = UR2_sav_all;                   /* save all tuples               */
   UR_save_modals = UR2_save_modals;         /* save modals in save file */
   UR_del_mark = UR2_del_mark;                 /* set mark del stack            */
   UR_del_stack_enabled = UR2_del_stack_enabled;   /* disable del stack               */
   UR_del_started = UR2_del_started;

   /* set default save,load pathname to null */
/*   UR_dpn[0] = '\000';*/

   /* get all relations as saved */
   for(i = 0; i <= UR_MAX_REL; i++) 
   {
      UR_rcb[i].status = UR2_rcb[i].status;
      UR_rcb[i].rel_num = UR2_rcb[i].rel_num;
      UR_rcb[i].n_ent = UR2_rcb[i].n_ent;
      UR_rcb[i].init_ent = UR2_rcb[i].init_ent;
      UR_rcb[i].n_varl = UR2_rcb[i].n_varl;
      UR_rcb[i].rel_flags = UR2_rcb[i].rel_flags;
      UR_rcb[i].last_accessed_index = UR2_rcb[i].last_accessed_index;
      UR_rcb[i].last_active_index = UR2_rcb[i].last_active_index;
      UR_rcb[i].active_tuple_cnt = UR2_rcb[i].active_tuple_cnt;
      UR_rcb[i].tuple_size = UR2_rcb[i].tuple_size;
      UR_rcb[i].bmap_size = UR2_rcb[i].bmap_size;
      UR_rcb[i].ent_ptr = UR2_rcb[i].ent_ptr;
      UR_rcb[i].bmap_ptr = UR2_rcb[i].bmap_ptr;
   }

/*
...vp 2/16/98 get saved environment from URW
......kludge! when ubfn opens unibase environment is stored in
.......new_adrs and there is no um_post_load operations done on
......loaded file it is not copied to .adrs so here we use data
......as it is after load.
*/
   for (i=0; URW_environ_table[i].name[0] != '\0'; i++)
	{
      UR_environ_table[i].new_adrs = UR2_environ_table[i].adrs;
      UR_environ_table[i].adrs = UR2_environ_table[i].new_adrs;
      UR_environ_table[i].length = UR2_environ_table[i].length;
	}

 UR_active = 2;

done:
   uu_dexit ;
   return(0) ;
}
/*********************************************************************
**    E_FUNCTION     :  ur_saveu_active(ibase)
**       Saves active unibase rcb structures & control data in the
**       corresponding global arrays/vars for future use. 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_saveu_active(ibase)
int ibase;
{
   /* local  parameter declarations */
   int   i;                  /* an index                          */

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
   uu_denter(UU_RTRC,(us,"ur_saveu_active unibase %d  ",ibase));

/* 
...Save active unibase as work unibase
*/
   if (ibase == 1) 
   {
/*
...... seve global unibase variables 
*/
      URW_last_mod_mkey = UR_last_mod_mkey;      /* key of last modified m tuple*/
      URW_low_key = UR_low_key;                   /*low key of loaded unibase */
      URW_last_mod_rkey = UR_last_mod_rkey;       /* key of last modified tuple*/
      URW_transf_relnum = UR_transf_relnum;       /* set to something it can't be   */
      URW_default_transf   = UR_default_transf; /* no default matrix yet         */
      URW_dd_inited = UR_dd_inited;             /* no data dictionary yet */
      URW_sav_all = UR_sav_all;                   /* save all tuples               */
      URW_save_modals = UR_save_modals;         /* save modals in save file */
      URW_del_mark = UR_del_mark;                 /* set mark del stack            */
      URW_del_stack_enabled = UR_del_stack_enabled;   /* disable del stack               */
      URW_del_started = UR_del_started;

   /* set default save,load pathname to null */
/*   UR_dpn[0] = '\000';*/

/*
...... store all relations as saved 
*/

      for(i = 0; i <= UR_MAX_REL; i++) 
      {
         URW_rcb[i].status = UR_rcb[i].status;
         URW_rcb[i].rel_num = UR_rcb[i].rel_num;
         URW_rcb[i].n_ent = UR_rcb[i].n_ent;
         URW_rcb[i].init_ent = UR_rcb[i].init_ent;
         URW_rcb[i].n_varl = UR_rcb[i].n_varl;
         URW_rcb[i].rel_flags = UR_rcb[i].rel_flags;
         URW_rcb[i].last_accessed_index = UR_rcb[i].last_accessed_index;
         URW_rcb[i].last_active_index = UR_rcb[i].last_active_index;
         URW_rcb[i].active_tuple_cnt = UR_rcb[i].active_tuple_cnt;
         URW_rcb[i].tuple_size = UR_rcb[i].tuple_size;
         URW_rcb[i].bmap_size = UR_rcb[i].bmap_size;
         URW_rcb[i].ent_ptr = UR_rcb[i].ent_ptr;
         URW_rcb[i].bmap_ptr = UR_rcb[i].bmap_ptr;
      }
/*
...vp 2/16/98 get saved environment from URW
*/
      for (i=0; UR_environ_table[i].name[0] != '\0'; i++)
	   {
         URW_environ_table[i].adrs = UR_environ_table[i].adrs;
         URW_environ_table[i].new_adrs = UR_environ_table[i].new_adrs;
         URW_environ_table[i].length = UR_environ_table[i].length;
	   }
   }
   else
/*
...Save active unibase as secondary unibase
*/
   {
/*
...... save global unibase variables 
*/
      UR2_last_mod_mkey = UR_last_mod_mkey;       /* key of last modified m tuple*/
      UR2_last_mod_rkey = UR_last_mod_rkey;       /* key of last modified tuple*/
      UR2_transf_relnum = UR_transf_relnum;       /* set to something it can't be   */
      UR2_default_transf   = UR_default_transf; /* no default matrix yet         */
      UR2_dd_inited = UR_dd_inited;             /* no data dictionary yet */
      UR2_sav_all = UR_sav_all;                   /* save all tuples               */
      UR2_save_modals = UR_save_modals;         /* save modals in save file */
      UR2_del_mark = UR_del_mark;                 /* set mark del stack            */
      UR2_del_stack_enabled = UR_del_stack_enabled;   /* disable del stack               */
      UR2_del_started = UR_del_started;

   /* set default save,load pathname to null */
/*   UR_dpn[0] = '\000';*/

/*
... store all relations as saved 
*/
      for(i = 0; i <= UR_MAX_REL; i++) 
      {
         UR2_rcb[i].status = UR_rcb[i].status;
         UR2_rcb[i].rel_num = UR_rcb[i].rel_num;
         UR2_rcb[i].n_ent = UR_rcb[i].n_ent;
         UR2_rcb[i].init_ent = UR_rcb[i].init_ent;
         UR2_rcb[i].n_varl = UR_rcb[i].n_varl;
         UR2_rcb[i].rel_flags = UR_rcb[i].rel_flags;
         UR2_rcb[i].last_accessed_index = UR_rcb[i].last_accessed_index;
         UR2_rcb[i].last_active_index = UR_rcb[i].last_active_index;
         UR2_rcb[i].active_tuple_cnt = UR_rcb[i].active_tuple_cnt;
         UR2_rcb[i].tuple_size = UR_rcb[i].tuple_size;
         UR2_rcb[i].bmap_size = UR_rcb[i].bmap_size;
         UR2_rcb[i].ent_ptr = UR_rcb[i].ent_ptr;
         UR2_rcb[i].bmap_ptr = UR_rcb[i].bmap_ptr;
      }
/*
...vp 2/16/98 get saved environment from URW
*/
      for (i=0; UR_environ_table[i].name[0] != '\0'; i++)
	   {
         UR2_environ_table[i].adrs = UR_environ_table[i].adrs;
         UR2_environ_table[i].new_adrs = UR_environ_table[i].new_adrs;
         UR2_environ_table[i].length = UR_environ_table[i].length;
	   }
   }
   UR_active = ibase;
   uu_dexit ;
   return(0) ;
}
/*********************************************************************
**    E_FUNCTION     :  ur_restore_second_material()
**      Restores the default material structures after loading an
**      older version Unibase, when the secondary (UBFN) Unibase is
**      active.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ur_restore_second_material()
{
	UR_environ_table[1].adrs = (char *)&UM2_mtrlmdl;
	UR_environ_table[1].new_adrs = (char *)&UM2_n_mtrlmdl;
	UR_environ_table[1].length = sizeof(struct UM_mtrlmdl_rec);
}

/*********************************************************************
**    E_FUNCTION     :  memrel()
**      deallocate memory used for secondary unibase. 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int memrel ()
{
   ur_flush_del_stack();
   ur_reset_unibase();
   uri_return_mem();   /* return allocated memory */
   uu_dexit ;
   return(0) ;
}
/*********************************************************************
**    E_FUNCTION     :  ur_restore_second_label()
**      Restores the default label structures after loading an
**      older version Unibase, when the secondary (UBFN) Unibase is
**      active.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ur_restore_second_label()
{
	UR_environ_table[4].adrs = (char *)&UM2_labelmdl;
	UR_environ_table[4].new_adrs = (char *)&UM2_n_labelmdl;
	UR_environ_table[4].length = sizeof(struct UM_labelmdl_rec);
}
