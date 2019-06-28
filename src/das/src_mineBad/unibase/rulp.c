/*********************************************************************
**    NAME         :  rulp.c
**       CONTAINS:
**                      ur_load_part()
**                      ur_load_part_exec()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       rulp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:50
*********************************************************************/

#include "usysdef.h"
#include "dmark.h"
#include "dasnog.h"
#include        "udebug.h"
#include "rerrdef.h"
#include "xenv1.h"
#include "xfsys1.h"             /* value of UX_NEXISTS */
#include "uhep.h"
#include "umoveb.h"

#include "mxxx.h"
#include "nccs.h"
#include "nclver.h"
#include "riallmod.h"
#include "rienvtab.h"
#include "rver9400.h"
#include "rver9700.h"

#if UU_COMP == UU_WIN2K
#include <string.h>
#define rindex strrchr
#endif

extern struct UR_env_table_rec	UR_environ_table[];

UU_LOGICAL      ud_lyesno();
UU_LOGICAL      ud_yesno();
UU_LOGICAL      ur_unibase_used();
extern UU_LOGICAL NCL_merge_overwrite;
extern UX_pathname UR_exnam[2];
extern UU_LOGICAL UR_load_env;    /* flag whether to read environment */
extern  char UR_dpn[];       /* default pathname for load */
int UR_update_layer = 0;


/*********************************************************************
**    E_FUNCTION     :  status = ur_load_part() 
**  load a part into unibase which has previously been saved 
**    PARAMETERS   
**       INPUT  : 
**			pfile: part to be load/merge
**          merge   = 0-load, 1-merge, -1-ask
**       OUTPUT :  
**          none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ur_load_part(pfile, merge)
char *pfile;
int merge;
{
	int length;
	UX_pathname fname;
	UU_LOGICAL del_existing;
   char msg[100];
	UD_STRREC string_rec;
	UX_pathname ext,descrip;
	char *p, *ux_getenv(), ext1[UX_SUFFIX_LEN];
	char *uu_uprompt0();

	/* check for existing geometry */
	del_existing = UU_TRUE;
	if(merge)
	{
		del_existing = UU_FALSE;
	}
	if(ur_unibase_used() && merge == -1)
	{
		/* "enter Y to remove existing geometry, N otherwise "*/
/*
.....change to use Motif
......Yurong 9/11/98
*/
/*              del_existing = (ud_lyesno(UU_UBASE,3)); 
*/
		strcpy(msg, uu_uprompt0(UU_UBASE,3));
		del_existing = ud_yesno(0, msg, "Load Unibase");
	}
	/* get user's filename to load from */
	length = sizeof(fname);
	fname[0] = '\0';
	string_rec.instring = fname;    /* set string address */
	
	strcpy(ext,"*.");
	strcpy(descrip, "Unibase Files (");
	p = ux_getenv("UR_PART_SUFFIX");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
	}       
	else 
		strcat(ext,"u");

	strcat(descrip, ext);
	strcat(descrip, ")|Textual Unibase Files (*.");
			
	p = ux_getenv("UR_ASCII_PART");
	strcat(ext,"|*.");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
		strcat(descrip, ext1);
	}       
	else
	{
		strcat(ext,"ud");
		strcat(descrip, "ud");
	}
	strcat(descrip, ")");

	if (del_existing)
	{
/*
.....use Motif interface
......Yurong 9/11/98
*/
		/*" enter load filename -> "*/
/*              ud_ldas(UD_DASSTRINGDEF,UU_UBASE,4,&string_rec,length,&length,UD_DEFAULT);
*/
		if ((pfile==UU_NULL)||(pfile[0]=='\0'))
			ud_get_filename("Enter Load filename", "Enter Load Unibase", ext,
								fname, &length, descrip, 1,UU_FALSE);
		else
			strcpy(fname, pfile);
		length = strlen(fname);
		NCL_merge_overwrite = UU_FALSE;
	}
	else
	{
/*
		ud_ldas(UD_DASSTRINGDEF,UU_UBASE,12,&string_rec,length,&length,UD_DEFAULT);
*/
		/* MILLS- Option to automatic rename of geometries if there are previously
		   defined geometries with same label. */
		if ((pfile==UU_NULL)||(pfile[0]=='\0'))
			ud_get_filename("Enter Merge filename", "Enter Merge filename",
						ext, fname, &length, descrip, 1,UU_FALSE);
		else
			strcpy(fname, pfile);
		length = strlen(fname);
		if (length<=0) return(UU_SUCCESS);
		NCL_merge_overwrite = !(ud_yesno(0, 
			"Do you want merged entities to be auto renamed?", "Merge Unibase"));
	}
	if(length > 0)
	{
		ur_load_part_exec(fname,del_existing);
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_load_part_exec(fname,del_existing) 
**  Load a part into unibase which has previously been saved .
**    PARAMETERS   
**       INPUT  : 
**          fname   = Name of Unibase to load
**
**          del_existing   = UU_TRUE = Load the Unibase
**                           UU_FALSE = Merge with existing Unibase.
**       OUTPUT :  
**          none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ur_load_part_exec(fname,del_existing)
char *fname;
UU_LOGICAL del_existing;
{
	int status,mode,umode,hstat,i;
	char *fext;
	UX_pathname fnamex,fnameu,fnames,fnamea,fnamepu,fnamepa,fnameps,fnameRegurg;
	UX_pathname dir,filename,extnam[2];
/*
.....Determine type of Unibase to load
.....Binary or Ascii
*/
	fext  = (char *) rindex(fname,'.');
	status = ux_getsys_ext(2,UR_exnam,extnam);
	if (status == 1) 
	{
		status = UX_BAD_ENV; 
		return(status);
	}
	umode = 0;
	if (fext != UU_NULL)
	{
		for (i=0; i<2; i++)
		{
			if (strcmp(fext+1,extnam[i]) == 0)
			{
				umode = i;
				break;
			}
		}
	}
/*
.....if filename already have a path, we don't need
.....added it
.....Yurong 9/11/98
*/
	ul_break_fname(fname, dir, filename);
	if (dir[0]!='\0')
		status = ux_mk_chk_syspath(UU_NULL, NULL, fname, UU_NULL,
						UU_NULL, UU_NULL, fnamex, UX_PRTERRS);
	else
		status = ux_mk_chk_syspath(UU_NULL, "^UR_PART_AREA", fname, UU_NULL,
						UU_NULL, UU_NULL, fnamex, UX_PRTERRS);
	switch(status)
	{
	case UU_SUCCESS:
		break;
	case UX_FAILURE:
	case UX_BAD_SUBJECT:
	case UX_BAD_ENV:
	case UX_NO_ACCESS:
		uu_uerror0(UU_UBASE,7); /* " cannot open file, try again "*/
		goto l_p99;
	default:
			break;
	}
	strcpy(fnameu, fnamex);
	status = ux_add_ftype("UR_UNB_FILE",fnameu,UX_PRTERRS);
	switch(status)
	{
	case UU_SUCCESS:
	case UX_FIXED_EXT:
		break;
	case UX_FAILURE:
	case UX_BAD_ENV:
		uu_uerror0(UU_UBASE,7); /* " cannot open file, try again "*/
		goto l_p99;
	default:
		break;
	}
	strcpy(fnames, fnamex);
	status = ux_add_ftype("UR_SOL_FILE",fnames,UX_PRTERRS);
	switch(status)
	{
	case UU_SUCCESS:
	case UX_FIXED_EXT:
		break;
	case UX_FAILURE:
	case UX_BAD_ENV:
		uu_uerror0(UU_UBASE,7); /* " cannot open file, try again "*/
		goto l_p99;
	default:
		break;
	}
	strcpy(fnamea, fnamex);
	status = ux_add_ftype("UR_AG_FILE",fnamea,UX_PRTERRS);
	switch(status)
	{
	case UU_SUCCESS:
	case UX_FIXED_EXT:
		break;
	case UX_FAILURE:
	case UX_BAD_ENV:
		uu_uerror0(UU_UBASE,7); /* " cannot open file, try again "*/
		goto l_p99;
	default:
		break;
	}
	strcpy(fnamepu,fnamex);
	status = ux_add_ftype("UR_UNB_PFILE",fnamepu,UX_PRTERRS);
	switch(status)
	{
	case UU_SUCCESS:
	case UX_FIXED_EXT:
		break;
	case UX_FAILURE:
	case UX_BAD_ENV:
		uu_uerror0(UU_UBASE,7); /* " cannot open file, try again "*/
		goto l_p99;
	default:
		break;
	}
	strcpy(fnamepa,fnamex);
	status = ux_add_ftype("UR_AG_PFILE",fnamepa,UX_PRTERRS);
	switch(status)
	{
	case UU_SUCCESS:
	case UX_FIXED_EXT:
		break;
	case UX_FAILURE:
	case UX_BAD_ENV:
		uu_uerror0(UU_UBASE,7); /* " cannot open file, try again "*/
		goto l_p99;
	default:
		break;
	}
	strcpy(fnameps,fnamex);
	status = ux_add_ftype("UR_SOL_PFILE",fnameps,UX_PRTERRS);
	switch(status)
	{
	case UU_SUCCESS:
	case UX_FIXED_EXT:
		break;
	case UX_FAILURE:
	case UX_BAD_ENV:
		uu_uerror0(UU_UBASE,7); /* " cannot open file, try again "*/
		goto l_p99;
	default:
		break;
	}
/*
.....Make sure file exists
*/
	mode = 0;
	status = ux_file_inquire(UU_NULL, UU_NULL, fnameu, UU_NULL, UU_NULL,
									&mode, &hstat, fnameRegurg, UX_PRTERRS);
	switch(status)
	{
	case UU_SUCCESS:
		break;
	case UX_FAILURE:
	case UX_BAD_SUBJECT:
	case UX_BAD_ENV:
	case UX_NO_ACCESS:
		uu_uerror0(UU_UBASE,7); /* " cannot open file, try again "*/
		goto l_p99;
	default:
		break;
	}
	/* if u-file doesn't exist or hasn't Unicad file header */
	if ( (mode == (mode|UX_NEXISTS)) || (hstat == UX_NFOUND) )
	{
		mode = 0;
		status = ux_file_inquire(UU_NULL, UU_NULL, fnamepu, UU_NULL, UU_NULL,&mode, &hstat, fnameRegurg, UX_PRTERRS);
		if(mode != (mode|UX_NEXISTS))
		{
			if(del_existing)
			{
				uu_uerror0(UU_UBASE,26); /* " cannot load partial file, must use MERGE PARTS option" */
				goto l_p99;
			}
			else
			{
				uu_move_byte(fnamepu, fnameu, UX_MAX_PATH_LEN);
				uu_move_byte(fnamepa, fnamea, UX_MAX_PATH_LEN);
				uu_move_byte(fnameps, fnames, UX_MAX_PATH_LEN);
			}
		}
		else
		{
			uu_uerror0(UU_UBASE,7); /* " cannot open file, try again " */
			goto l_p99;
		}
	}
	/* MILLS: add support for Unibase version checking */
	if (NCL_infile_version < 8.001)
		{
		uu_uerror0(UA_NCL,13);  /* " incorrect version number "*/
		goto l_p99;
		}
	/* MILLS: add support for Unibase machine type checking */
	if (umode == 0 && strcmp(NCL_machine, NCL_machine_type) != 0)
		{
		uu_uerror0(UA_NCL,14);  /* " wrong machine type "*/
		goto l_p99;
		}

	/* if user asked to delete existing geometry, then do before load starts */
	if (del_existing)
	{
		uv_set_defered_mode();  /* allow block erase if appropriate */
		uu_unireset();
		uv_set_immediate_mode();
	}
	strcpy(UR_dpn,fname); /* set default pathname for next save */

	/* do special pre-processing */
	/* del_existing is passed along to flag whether this is a load or merge */
	status = ur_lp01(fnames, fnamea, del_existing);

/*
.....Set UR_load_env to 'del_existing' (merging)
.....instead of always true, otherwise the merged
.....Unibase environment will overwrite the active
.....Unibase environment, causing bad things to happen,
.....Such as the reassignment of the construction axis key
.....Bobby  -  4/29/05
*/
	UR_load_env = del_existing;
	/* MILLS- pass del_existing to ur_lp02b. */
	status = ur_lp02(fnameu, del_existing);
	UR_load_env = UU_FALSE;
	if(status == URM_CANTOPEN_LD) goto l_p999; /* couldn't open Unibase file */
	if(status != 0)
	{
		ur_report_error(status);
		goto l_p999;
	}
	/* do special post-processing */
	status =        ur_lp03(fnames, fnamea, del_existing);

	/* do display of new data after load */
	status = ur_lp04();
l_p999:
l_p99:
	return(status);
}
