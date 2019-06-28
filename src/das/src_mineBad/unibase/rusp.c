/*********************************************************************
**    NAME         :  rusp.c
**       CONTAINS:
**       ur_save_part()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       rusp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:50
*********************************************************************/

#include "usysdef.h"
#include "ustdio.h"
#include "dmark.h"
#include "dasnog.h"
#include "driver.h"
#include "udebug.h"
#include "rerrdef.h"
#include "ribase.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "uhep.h"
#include "mfort.h"
#include "nclfc.h"

UU_LOGICAL      ud_lyesno();

/*********************************************************************
**    E_FUNCTION     :  ur_save_part(exiting)
**       save a unibase database part for later load
**    PARAMETERS   
**       INPUT  : 
**                              exiting, true if exiting unicad
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added errflag to check if it is fit a final error
.....Yurong 10/8/98
*/
ur_save_part(exiting, errflag)
UU_LOGICAL      exiting;                /* true if exiting unicad */
UU_LOGICAL  errflag;
{
	extern  UU_LOGICAL      UR_sav_all;     /* boolean, UU_TRUE if save all         */
	extern  char    UR_dpn[]        ;       /* default pathname     */
	extern UU_LOGICAL ur_unibase_used();
	extern UU_LOGICAL       UR_changed;     /* Unibase change flag */
 extern char UBopen[];  /* second unibase file name */

	int             iostat                  ;       /* holds status of i/o calls                            */
	int             status                  ;       /* holds status of unibase calls                        */
	char            fname[UX_MAX_PATH_LEN]          ;       /* base filename to use */
	char            fnamex[UX_MAX_PATH_LEN];        /* unibase filename to use      */
	char            fnameu[UX_MAX_PATH_LEN] ;       /* unibase filename to use      */
	char            fnames[UX_MAX_PATH_LEN] ;       /* solid filename to use        */
	char            fnamea[UX_MAX_PATH_LEN] ;       /* appgeo filename to use       */
	char            fnameRegurg[UX_MAX_PATH_LEN];
	char            dir[UX_MAX_PATH_LEN];   /* filearea part of sysdep */
	char            filename[UX_MAX_PATH_LEN];      /* filearea part of sysdep */
	UX_pathname ext, descrip;
	char *p, *ux_getenv(), ext1[UX_SUFFIX_LEN];
	int             length                                          ;       /* returned lenof filename      */
	int             mode;
	int             solmode;
	int             hstat;
 UM_int2 ierr;
	UD_STRREC       string_rec;                             /* das input variable */
    UX_pathname  bname;

	uu_denter(UU_RTRC,(us,"ur_save_part")) ;
	status = 0 ;
	fname[0] = '\0';

	/* get user's filename */
	strcpy(fname,UR_dpn);   /* set default pathname */
	if (exiting)
		length = strlen(fname);
	else
	{
		length = sizeof(fname);
		string_rec.instring = fname;    /* set string address */
/*
.....use Motif interface
.....changed by Yurong 8/19/98
*/
/*
.....if final error, still use prompt
.....Yurong 10/8/98
*/
		/* prompt user "enter save filename ->" */
		if (errflag)
			ud_ldas(UD_DASSTRINGDEF,UU_UBASE,1,&string_rec,length,&length,
					UD_DEFAULT) ;
		else
		{
			strcpy(ext,"*.");
			strcpy(descrip, "Unibase File (");
			p = ux_getenv("UR_PART_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
				strcat(ext,ext1);
			}       
			else strcat(ext,"u");
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
			ud_get_filename("Enter save filename", "Save Unibase", ext,
								fname, &length,descrip, 0,UU_FALSE);
		}
	}
	if (length<=0) ud_jump(-1, UU_FALSE); 
	if(length > 0)
	{
/*
.....if filename already have a path, we don't need
.....added it
.....Yurong 8/19/98
*/
/*              strcpy(UR_dpn,fname);    set default pathname */

		/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
		/*  Note: The following code does something useful!  In    */
		/*        particular it turns a file name into ...         */
		/*                  a file name!                           */
		/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*              status = ux_mk_chk_syspath(UU_NULL, "^UR_PART_AREA", fname, UU_NULL,
											UU_NULL, UU_NULL, fnamex, UX_PRTERRS);
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
			uu_dprint(-1,(us,"bad status of %d from ux_mk_chk_syspath",status));
			uu_uerror0(UU_UBASE, 3);
/*
.....in case of error, just get out of this function
.....Yurong 8/19/98
*/
			return status;
/*                      goto getname;      */
		default:
			uu_dprint(-1,(us,"Illegal return(%d) from UDOS",status));
#if UU_DEBUG==1
			exit(-1);
#else
			break;
#endif
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
		case UX_BAD_SUBJECT:
			uu_dprint(-1,(us,"bad status %d from ux_add_ftype",status));
			uu_uerror0(UU_UBASE, 3);
/*
.....in case of error, just get out of this function
.....Yurong 8/19/98
*/
			return status;
/*                      goto getname;           */
		default:
			uu_dprint(-1,(us,"Illegal return(%d) from UDOS",status));
#if UU_DEBUG==1
			exit(-1);
#else
			break;
#endif
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
		case UX_BAD_SUBJECT:
			uu_dprint(-1,(us,"bad status %d from ux_add_ftype",status));
			uu_uerror0(UU_UBASE, 3);
/*
.....in case of error, just get out of this function
.....Yurong 8/19/98
*/
			return status;
/*                      goto getname;        */
		default:
			uu_dprint(-1,(us,"Illegal return(%d) from UDOS",status));
#if UU_DEBUG==1
			exit(-1);
#else
			break;
#endif
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
		case UX_BAD_SUBJECT:
			uu_dprint(-1,(us,"bad status %d from ux_add_ftype",status));
			uu_uerror0(UU_UBASE, 3);
/*
.....in case of error, just get out of this function
.....Yurong 8/19/98
*/
			return status;
/*                      goto getname;       */
		default:
			uu_dprint(-1,(us,"Illegal return(%d) from UDOS",status));
#if UU_DEBUG==1
			exit(-1);
#else
			break;
#endif
		}
	}
	else
	{
		goto theSav2;
	}

	/* check for existence of file, if it exists ask whether to remove it */
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
		uu_dprint(-1,(us,"bad status %d from ux_file_inquire",status));
		uu_uerror0(UU_UBASE, 3);
/*
.....in case of error, just get out of this function
.....Yurong 8/19/98
*/
		return status;
/*              goto getname;           */
	default:
		uu_dprint(-1,(us,"Illegal return(%d) from UDOS",status));
#if UU_DEBUG==1
		exit(-1);
#else
		break;
#endif
	}
	if ( !(mode & UX_NEXISTS))
	{
		/* prompt user "enter Y to delete file, N to exit" */
/*              if(ud_lyesno(UU_UBASE,2))           */
		if (ud_yesno(0, uu_uprompt0(UU_UBASE,2), "Unibase Exist"))
		{
			iostat = ux_delete(fnameu, UX_PRTERRS);

			/* if successfully deleted the Unibase portion, */
			/* inquire as to the existence of the Solids portion */
			/* and if it exists, delete it too */
			if (iostat == 0) 
			{
				solmode = 0;
				status = ux_file_inquire(UU_NULL, UU_NULL, fnames, UU_NULL, UU_NULL,
												&solmode, &hstat, fnameRegurg, UX_PRTERRS);
				switch(status)
				{
				case UU_SUCCESS:
					break;
				case UX_FAILURE:
				case UX_BAD_SUBJECT:
				case UX_BAD_ENV:
				case UX_NO_ACCESS:
					uu_dprint(-1,(us,"bad status %d from ux_file_inquire",status));
					uu_uerror0(UU_UBASE, 3);
/*
.....in case of error, just get out of this function
.....Yurong 8/19/98
*/
					return status;
/*                                      goto getname;             */
				default:
					uu_dprint(-1,(us,"Illegal return(%d) from UDOS",status));
#if UU_DEBUG==1
					exit(-1);
#else
					break;
#endif
				}
				if ( !(solmode & UX_NEXISTS))
					iostat = ux_delete(fnames, UX_PRTERRS);
			}
			/* if successfully deleted the Unibase portion, */
			/* inquire as to the existence of the Applied Geometry portion */
			/* and if it exists, delete it too */
			if (iostat == 0) 
			{
				solmode = 0;
				status = ux_file_inquire(UU_NULL, UU_NULL, fnamea, UU_NULL, UU_NULL,
												&solmode, &hstat, fnameRegurg, UX_PRTERRS);
				switch(status)
				{
				case UU_SUCCESS:
					break;
				case UX_FAILURE:
				case UX_BAD_SUBJECT:
				case UX_BAD_ENV:
				case UX_NO_ACCESS:
					uu_dprint(-1,(us,"bad status %d from ux_file_inquire",status));
					uu_uerror0(UU_UBASE, 3);
/*
.....in case of error, just get out of this function
.....Yurong 8/19/98
*/
					return status;
/*                                      goto getname;                 */
				default:
					uu_dprint(-1,(us,"Illegal return(%d) from UDOS",status));
#if UU_DEBUG==1
					exit(-1);
#else
					break;
#endif
				}
				if ( !(solmode & UX_NEXISTS))
					iostat = ux_delete(fnamea, UX_PRTERRS);
			}
			if (iostat != 0)
			{
				uu_uerror0(UU_UBASE,2);
/*
.....in case of error, just get out of this function
.....Yurong 8/19/98
*/
				return status;
/*                              goto getname;        */
			}
		}
		else
		{
/*
.....in case of error, just get out of this function
.....Yurong 8/19/98
*/
			return UU_FAILURE;
/*                      goto getname;              */
		}
	}

	/* determine if filename length is legal */
	status = ux_get_base_fname(fname,bname,(UX_NPRTERRS|UX_NCHK));
	if(status != UU_SUCCESS)
		{
		uu_uerror0(UX_UDOS,24);
/*
.....in case of error, just get out of this function
.....Yurong 8/19/98
*/
		return status;
/*              goto getname;              */
    }

	/* go do special pre-processing */
	status = ur_sp01(fnames, fnamea, UU_TRUE);

	/* go do Unibase save */
	UR_sav_all = UU_TRUE ;
	status = ur_sp02(fnameu) ;
	if(status != 0)
	{
		ur_report_error(status);
	}
	/* go do special post-processing */
	status = ur_sp03(fnames, fnamea);
/* NCL - */
/*      UR_changed = UU_FALSE;   flag database as clean */
/* end NCL */
/*
...Save Secondary unibase if open and changed
*/
theSav2:
 if (strcmp(UBopen,""))
  {
   status = ur_cl04(&ierr);
   if (status == 1) status = URM_CANTOPEN_SV;
   if (status == 2) status = URM_WRT_ERR;
  }
	uu_dexit;
	return(status);
}
