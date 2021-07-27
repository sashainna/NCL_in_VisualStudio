/*********************************************************************
**    NAME         :  neunibas.c
**       CONTAINS: interface routines to UNIBASE which follow the
**                   standard FORTRAN to C calling conventions.
**
**       int dlgeom
**       blkgeo
**       int saveu
**       int loadu
**       UU_LOGICAL ncl_unibase_used
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       neunibas.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:56
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "nclfc.h"

#include "rerrdef.h" 
#include "xenv1.h"
#include "xfsys1.h"		/* value of UX_NEXISTS */

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclver.h"
#include "riallmod.h"
#include "rienvtab.h"
#include "rver9400.h"
#include "rver9700.h"

extern UU_LOGICAL UM_modax_disp;

extern UX_pathname UR_exnam[2];
extern struct UR_env_table_rec	UR_environ_table[];
extern int UR_restore_mtrl;
extern int UR_restore_lights;
extern int UR_restore_clr;

/*********************************************************************
**    E_FUNCTION     : int dlgeom(nclkey)
**       Delete the entity specified by the UNIBASE key (NCLKEY) from
**       UNIBASE and from DIGS.
**    PARAMETERS
**       INPUT  :
**          nclkey            UNIBASE key of the entity
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int dlgeom(nclkey)
UM_int4 *nclkey;
{
	int status = UU_FAILURE, rel_num;
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"dlgeom(nclkey=%x)", *nclkey));

	key = *nclkey;

	if (ur_retrieve_data_relnum (key, &rel_num) == 0)
	{
		if (rel_num > 0 && uc_delete(key) == 0) status = UU_SUCCESS;
	}
/*
... aak 22-apr-1998: check if the deleted entity was the last
... evaluated NCL surface and reset the corresponding key if yes.
*/
	if (key == ncl_get_conv_sfseg_key () ) ncl_conv_sfseg_reset ();

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : blkgeo(nclkey)
**       calls ur_update_blanked routine for fortran calling routine
**    PARAMETERS
**       INPUT  :
**          nclkey                  key of geometry to set blanked attribute for
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
blkgeo(nclkey, iblank)
   UM_int4 *nclkey;
   UM_int2 *iblank;
   {
   UU_KEY_ID key;
   UU_LOGICAL blank;
   int status;

   uu_denter(UU_MTRC,(us,"blkgeo(nclkey=%x)", *nclkey));
   /* Extra switch for unblanking the geometry. */
	if (*iblank == 1)
		blank = UU_TRUE;
	else
		blank = UU_FALSE;

   key = *nclkey;
   status = ur_update_blanked (key, blank);

   uu_dexit;
   return(0);
   }

/*********************************************************************
**    E_FUNCTION     : int saveu(fname,nci,ierr)
**       save unibase
**    PARAMETERS
**       INPUT  :
**          fname -     file of unibase to save
**          nci   -     Number of chars in 'fname'.
**       OUTPUT :
**          ierr  -     0 iff no err
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int saveu(fname,nci,ierr)
UM_f77_str_ptr fname;
UM_int2 *ierr;
UM_int4 *nci;
{
   char *cstr;
   char cstr2[UX_MAX_PATH_LEN];
   int status;
   extern UU_LOGICAL UR_sav_all;
   extern UU_LOGICAL UR_changed;
   UU_LOGICAL save_ur_changed;
   UM_int2 ifl,val;
   char fnamea[UX_MAX_PATH_LEN];   /* appgeo filename to use   */

   uu_denter(UU_MTRC,(us,"saveu()"));

   cstr = UM_cstr_of_f77_str(fname);
	strncpy(cstr2,cstr,*nci);
   cstr2[*nci]='\0';
	UR_sav_all = UU_TRUE ;
   save_ur_changed = UR_changed;
   status = ur_sp02(cstr2);
   *ierr = status;

/*
build applied geometry filename from cstr2
*/
	ux_get_base_fname(cstr2, fnamea, UX_PRTERRS);
	status = ux_add_ftype("UR_AG_FILE",fnamea,UX_PRTERRS);
	switch(status)
		{
		case UU_SUCCESS:
		case UX_FIXED_EXT:
			break;
		case UX_FAILURE:
		case UX_BAD_ENV:
		case UX_BAD_SUBJECT:
		    *ierr = status;
			uu_dexit;
			return(0);
			break;
		default:
			break;
		}
/*
save the applied geometry portion of the database
	  um_save_appgeo(fnamea);
*/
	
   UR_changed = save_ur_changed;

   /* Update STATUS AREA */
   if (status == UU_SUCCESS)
      {
      /* update STATUS area with new program name */
      /* check to see if we are running batch */
      ifl=35;
      getifl(&ifl,&val);
      if (!val)
         uz_load_status();
      }

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int loadu(f77name,nci,batch,ierr)
**       load unibase
**	RAH: modified reporting of errors to use uu_errorXXX() calls only
**       if running NCLCADD, otherwise let CAM processor report the
**       errors.
**    PARAMETERS
**       INPUT  :
**          f77name -     file of unibase to load
**          nci     -     Number of characters in 'f77name'.
**          batch   -     1 = Running batch mode.
**       OUTPUT :
**          ierr  -     0 iff no error
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int loadu(f77name,nci,batch,ierr)
UM_f77_str_ptr f77name;
UM_int2 *ierr,*batch;
UM_int4 *nci;
{
   extern UU_LOGICAL  UR_load_env;   /* flag whether to read environment */
   extern char UR_dpn[]; /* default pathname for load */
   int i,status;
   char *fname;          /* base filename to use      */
   char fnameu[UX_MAX_PATH_LEN]; /* unibase filename to use */
   char fnames[UX_MAX_PATH_LEN]; /* solids filename to use */
   char fnamea[UX_MAX_PATH_LEN];    /* Applied Geometry filename to use */
   char dir[UX_MAX_PATH_LEN]; /* filearea part of sysdep */
   char file[UX_MAX_PATH_LEN]; /* file part of sysdep name */
   char *fext; 
   UX_pathname extnam[2];
   UX_pathname fomode;
#ifndef UU_RS6000
   char *rindex();
#endif
   UU_LOGICAL del_existing=UU_TRUE; /* true if we need to del geometry */
   int mode, umode;
   int apmode;
   int hstat;
   int hapg;
   unsigned long key_id; /* current entity key */
   int nxt_ent;   /* index of next master key */
   int get_nxt_status;  /* status of next get opertation */
   int dsegid;   /* display segment id of current entity */
   UM_int2 i2flg;
   UM_real8 mx[12];
   extern UU_LOGICAL NCL_merge_overwrite;

   uu_denter(UU_MTRC,(us,"loadu()"));
 
   fname = UM_cstr_of_f77_str(f77name);
   fnames[0] = '\0';
	fname[*nci]='\0';
	fext  = rindex (fname,'.');
	if (fext != UU_NULL) fext++;
#if UU_COMP == UU_VAXVMS
   if (fext != UU_NULL) ul_to_upper (fext);
#endif
/*
.....Save the current light and material settings before loading the unibase.
*/
	if(UR_restore_lights)
		ul_save_lgtmod();
	if(UR_restore_mtrl)
		ul_save_mtrlmod();
	if(UR_restore_clr)
		ul_save_clrmod();
/*
...get environment standard file extensions
*/
   status = ux_getsys_ext(2,UR_exnam,extnam);
   if (status == 1) goto done;

/*
.....Make sure that we have a file extension
.....Bobby  -  1/22/97
*/
	if (fext == UU_NULL)
	{
		strcat(fname,".");
		strcat(fname,UR_exnam[0]);
		fext = rindex(fname,'.') + 1;
	}

   strcpy (fomode,"UR_UNB_FILE");
   umode = 0;
   for (i=0; i<2; i++)
     {
      if (strcmp(fext,extnam[i]) == 0)
        {
         strcpy (fomode,UR_exnam[i]);
         umode = i;
         break;
        }
     }
/*
.....Use user supplied file extension
.....Bobby  -  3/9/97
*/
	if (umode == 0) strcpy(fomode,fext);

   status = ux_is_opsys_dep(fname, UX_PRTERRS); 
   if (status == UX_NOPSYS)
    {
     strcpy(dir, ""); /* in case UDOS doesn't fill this in */
     if (ux_decompose_path(fname, dir, file, UX_NQUOTES + UX_PRTERRS)
         == UX_FAILURE)
/* 
...because UDOS can't decompose a simple path 
*/
       {
        strcpy(file, fname);
        if (strcmp(dir,"") == 0) strcpy(dir, "^UR_PART_AREA"); 
       }
     ux_get_base_fname(file, fname, UX_PRTERRS);
    }
   else
    {
     strcpy(dir, "^UR_PART_AREA");
    }
   mode = 0;
   if ( (status = ux_file_inquire(UU_NULL, dir, fname, UU_NULL,
     fomode, &mode, &hstat, fnameu, UX_PRTERRS))
               != UU_SUCCESS)

    {
/* " cannot open file, try again "*/
/* report Unibase error via DAS only if running CADD, otherwise
      let CAM processor handle error */
     if ((*batch==0) && (UU_application == UU_NCLCADD))
     uu_uerror0(UU_UBASE,7);
     status=1;
     goto done;
    }
/* We do not use R solids in NCL - VP 6-apr-94 */
/*   solmode = 0;
   if ( (status = ux_file_inquire(UU_NULL, dir, fname, UU_NULL,
 "UR_SOL_FILE", &solmode, &hsol, fnames, UX_PRTERRS)) != UU_SUCCESS)
   {
 " cannot open file, try again "*/
/* if ((*batch==0) && (UU_application == UU_NCLCADD))
  uu_uerror0(UU_UBASE,7);
 status=1;
 goto done;
   }         */
/* if u-file doesn't exist or hasn't Unicad file header */
   if ( (mode == (mode|UX_NEXISTS)) || (hstat == UX_NFOUND) )
    {
/* " cannot open file, try again "*/
     if ((*batch==0) && (UU_application == UU_NCLCADD))
     uu_uerror0(UU_UBASE,7);
     status=1;
     goto done;
    }

   apmode = 0;
   if ( (status = ux_file_inquire(UU_NULL, dir, fname, UU_NULL,
      "UR_AG_FILE", &apmode, &hapg, fnamea, UX_PRTERRS)) != UU_SUCCESS)
    {
/* " cannot open file, try again "*/
     if ((*batch==0) && (UU_application == UU_NCLCADD))
     uu_uerror0(UU_UBASE,7);
     status=1;
     goto done;
    }

/* Unibase version checking: 8.001 is last supported, vp 11/4/94  */
   if (NCL_infile_version < 8.001)
    {
     if ((*batch==0) && (UU_application == UU_NCLCADD))
     uu_uerror0(UA_NCL,13); 
     status = 2;  
     goto done;
    }  

/* MILLS: add support for Unibase machine type checking */
   if (strcmp(NCL_machine, NCL_machine_type) != 0 && umode == 0)
    {
     if ((*batch==0) && (UU_application == UU_NCLCADD))
     uu_uerror0(UA_NCL,14); /* " wrong machine type "*/
     status = 3;  /* to be parsed in FORTRAN and gen approp. msg */
     goto done;
    }

   if (*batch == 0)
    {
		motdel();
     uv_set_defered_mode();  /* allow block erase if appropriate */
     uu_unireset();
     uv_set_immediate_mode();
    }
   else
    {
     um_feareset();   /* remove any features */

/* loop over all entities in the data base
   and delete from the gks segment file */

     nxt_ent = 1;
     get_nxt_status = 0;
     while(get_nxt_status == 0)
      {
/* get next entity */
       get_nxt_status = ur_get_next_key(&nxt_ent,&key_id);
       if (get_nxt_status == 0)
        {
/* get entities gks seg-id */
         ur_retrieve_disp_segid(key_id,&dsegid); 
         if(dsegid>=0) uv_delsegs(dsegid);  /* delete from gks */
        }
       nxt_ent++;
      }
/* uv_deactivsc(0); */ /* deactivate the curent screen */
/*     um_reset_appgeo();  */
     ur_reset_unibase();  /* reset unibase */
     um_rm31_romini();  /* reset romulus */
    }

   strcpy(UR_dpn,fname); /* set default pathname for next save */
/* do solids pre-processing */
/* del_existing is passed along to flag whether this is a load or merge */
/* ---  temp fix for segv. Whole routine neds to be updated per RULP.C --- */
/*   if (*batch == 0) status = ur_lp01(fnames, del_existing); */
   status = ur_lp01(fnames, fnamea, del_existing); 

/* do Unibase load */
   UR_load_env = UU_TRUE;
   status = ur_lp02(fnameu, del_existing);
   UR_load_env = UU_FALSE;
   if(status == URM_CANTOPEN_LD) goto done; /* couldn't open Unibase file */
   if(status != 0)
    {
     if ((*batch==0) && (UU_application == UU_NCLCADD))
        ur_report_error(status);
     goto done;
    }
/* do solids post-processing */
   NCL_merge_overwrite = UU_FALSE;
   if (*batch == 0)
    {
     status = ur_lp03(fnames, fnamea, del_existing);
     status = ur_lp04();   /* do display of new data after load */
    }
   else 
    {
/*
...vp 3/26/98 make sure that new modals are installed so saveu
...will save environment inhereted with unibase loaded,
...in intercative mode this is called in ur_lp03.
*/
		ur_install_new_modals();
		um_post_load_appgeo();
		ncl_post_load(del_existing); /* called from ur_lp03() in interactive */
		status = ncl_lp04();   /* set dsegid's to -1 for batch */
    }

done:
	*ierr = status;
/* Update STATUS AREA */
	if ((status == UU_SUCCESS) && (*batch==0))
	{
/* update STATUS area with new program name */
		mcsmx (&i2flg, mx);
		stmdmx (mx);
		um_drw_mod_axis (UM_modax_disp);
		uz_load_status();
		nclc_save_recent_file(fnameu, 1);
    }
/*
.....load the screen layout file again because the unibase will change it
.....the screen definintion in layout file will not overwrite the unibase (just loaded)
.....screen definition
*/
	uv_load_scr_layout(0);
   uu_dexit;
   return(status);
  }

/*********************************************************************
**    E_FUNCTION     :  ncl_unibase_used()
**      return true(active entries) if unibase has been used. Based on
**      ur_unibased_used except ignores UR-changed flag (set false when
**      unibase is saved).
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS : non-zero(UU_TRUE) if active unibase entries, 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/* UU_LOGICAL ncl_unibase_used()
 {
    int  i; *//* index to look from */
   /* UU_KEY_ID key_id;*/ /* the returned key_id */
/*   extern UU_LOGICAL UR_changed; *//* Unibase changed flag */

   /* i = 1 ;
    ur_get_next_key(&i,&key_id); *//* i = 0 if no active entries */
   /* if(i >= 1) *//* if unibase used */
	/* return(UU_TRUE);
    else
         return(UU_FALSE);
 } */
