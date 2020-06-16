/*********************************************************************
**    NAME         :  tigsave.c
**       CONTAINS:
**					uig_setup_save
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tigsave.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:50
*********************************************************************/

#include <stdio.h>
#include "ribase.h"
/*
#include "tigspdef.h"
*/
#include "mdattr.h"
#include "mattr.h"
#include "ualloc.h"
#include "xenv1.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "udebug.h"
#if (UU_COMP!=UU_WIN2K)
#include "tigmf.h"
#endif

extern char ext_filename[];	
/*********************************************************************
**    I_FUNCTION     :  uig_setup_save(lu, fnamepu, fnameps, fnamepa)
**				Setup a UNIBASE file for neutral save format.
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				lu									file unit number
**				fnamepu							unibase partial file name
**				fnameps							romulus partial file name
**				fnamepa							appgeo partial file name
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_setup_save(lu, fnamepu, fnameps, fnamepa)
	int *lu;
	char *fnamepu[];
	char *fnameps[];
	char *fnamepa[];
	{
	UX_pathname fname, fname1, tmp_file, dir;
	int ans;
	int mode, solmode, hstat, solstat, agmode, agstat;
	int status;
	char p_buff[300];
	int i,j;

	/*----------------------------------------------------------------------
	** Start of executable code
	**--------------------------------------------------------------------*/
	/* initilize MPE subsystems */

	uio_init();

l_p01:
	uig_clear();			/* get file name */
#if (UU_COMP!=UU_WIN2K)
	if (UW_MOTIF!=1)
	{
		uig_str_out("Enter Unibase name: ",UU_TRUE);		/*jkd18 */
		uig_str_in(fname);
/*
.....yurong added to prevent empty file name
*/
		ul_break_fname(fname, dir,fname1);
		if (strlen(fname1)==0) return UU_FAILURE;
	}
	else
	{
		uig_mfget_unifil(fname);
		ul_break_fname(fname, dir,fname1);
		if (strlen(fname1)==0) return UU_FAILURE;
	}
#else
	iges_wntget_unifil(fname);
	ul_break_fname(fname, dir,fname1);
	if (strlen(fname1)==0) return UU_FAILURE;
#endif
	if(strlen(fname)< 1)					/*jkd43*/ 
		strcpy(fname, ext_filename);

/*
......We can not do this because, we may create a file
......in directory other than corrent directory
.....Yurong
*/
	/**NCL: modified to prevent possible filename ".u",
	   also ensures that unibases are created in the local
	   directory by default  **
	i = strcspn(fname, ".");
	strcpy(&fname[i], ".u");
	**/
/*	get_local_file(fname,fname,".u"); */
	tmp_file[0] = '\"';
	for (i=0; i<strlen(fname); i++)
		tmp_file[i+1] = fname[i];
	j = strcspn(fname1, ".");
	if (j==strlen(fname))
	{
/*
.....no suffix, add ".u"
*/
		tmp_file[i+1] = '.';
		tmp_file[i+2] = 'u';
		i = i+2;
	}
	tmp_file[i+1] = '\"';
	tmp_file[i+2] = '\0';
	strcpy(fname, tmp_file);

	ul_short_filename(fname,tmp_file,50);
	sprintf(p_buff, "Unibase Output file: %s\n", tmp_file);	/*jkd43*/
	uig_list_out(p_buff, UU_TRUE);				/*jkd43*/
	uig_list_out(" \n", UU_FALSE);

	/* generate full path name */

	mode = 0;
	status = ux_file_inquire(UU_NULL,"^UR_PART_AREA",fname,UU_NULL,
				"UR_UNB_FILE", &mode, &hstat, fnamepu, UX_PRTERRS);
	if(status != UU_SUCCESS)
	{
#if (UU_COMP!=UU_WIN2K)
		if (UW_MOTIF==1)
		{
			uig_mfmsg_box(NULL, "Error!", "file naming problem, retry!");
			return(status);
		}
		ul_short_filename(fname,tmp_file,50);
		sprintf(p_buff, "File naming problem. Can't open %s.\nPress return to continue", tmp_file);
		uig_str_out(p_buff,UU_TRUE);
		uig_str_in(fname);
		return(status);
#else
		iges_wnt_msg(NULL, "Error!", "file naming problem, retry!");
		return(status); 
#endif
	}

	solmode = 0;
	status = ux_file_inquire(UU_NULL,"^UR_PART_AREA",fname,UU_NULL,
				"UR_SOL_FILE", &solmode, &solstat, fnameps, UX_PRTERRS);
	if(status < 0)
		{
#if (UU_COMP!=UU_WIN2K)
		if (UW_MOTIF==1)
		{
			uig_mfmsg_box(NULL, "Error!", "file naming problem, retry!");
			return(status);
		}

		uig_str_out("  file naming problem; press return to continue",UU_TRUE);
		uig_str_in(fname);
	   return(status);
#else
		iges_wnt_msg(NULL, "Error!", "file naming problem, retry!");
		return(status); 
#endif
		}

	agmode = 0;
	status = ux_file_inquire(UU_NULL,"^UR_PART_AREA",fname,UU_NULL,
				"UR_AG_FILE", &agmode, &agstat, fnamepa, UX_PRTERRS);
	if(status < 0)
		{
#if (UU_COMP!=UU_WIN2K)
		if (UW_MOTIF==1)
		{
			uig_mfmsg_box(NULL, "Error!", "appgeo file naming problem, retry!");
			return(status);
		}

		uig_str_out("  appgeo file naming problem",UU_TRUE);
		uig_str_in(fname);
	   return(status);
#else
		iges_wnt_msg(NULL, "Error!", "appgeo file naming problem, retry!");
		return(status); 
#endif
		}
							/* open file */

l_p10:
	*lu = 0;
	if( !(mode & UX_NEXISTS))
	{
#if (UU_COMP!=UU_WIN2K)
		if (UW_MOTIF==1)
		{
			ul_short_filename(fnamepu,tmp_file,50);
			sprintf(p_buff, "  Unibase file %s exists; delete? ", tmp_file); /*jkd22*/
			ans = uig_mfyesno(NULL, "File Exist", p_buff);
		}
		else
		{
			ul_short_filename(fnamepu,tmp_file,50);
			sprintf(p_buff, "  Unibase file %s exists; delete? (y or n)", tmp_file); /*jkd22*/
			uig_str_out(p_buff,UU_TRUE); 	/*jkd22 */
			ans = uig_ans();
		}
#else
		ul_short_filename(fnamepu,tmp_file,50);
		sprintf(p_buff, "  Unibase file %s exists; delete? ", tmp_file);
		ans = iges_wntyesno(NULL, "File Exists", p_buff);
#endif

		if(ans == 0)
		{
			status = ux_delete(fnamepu, UX_PRTERRS);
			if(status != 0)
			{
#if (UU_COMP!=UU_WIN2K)
				if (UW_MOTIF==1)
				{
					uig_mfmsg_box(NULL, "Error!", "error in deleting file!");
					return(status);
				}
				ul_short_filename(fnamepu,tmp_file,50);
				sprintf (p_buff, " Error in deleting Unibase file %s", tmp_file);
				uig_str_out(p_buff,UU_TRUE);
				uig_str_in(fname);
				return(status);
#else
				iges_wnt_msg(NULL, "Error!", "error in deleting file!");
				return(status);
#endif
			}
			status = ux_delete(fnameps, UX_PRTERRS);
			status = ux_delete(fnamepa, UX_PRTERRS);
			return(0);
		}
		else
		{
#if (UU_COMP!=UU_WIN2K)
			if (UW_MOTIF==1)
			{
				status = -1;
				uu_dexit;
				return(-1);
			}
			uig_str_out("  Try again (y or n)? ",UU_TRUE);
			if (uig_ans() == 0)
			{
				goto l_p01;
			}
			else
			{
				status = -1;
				return(status);
			}
#else
			return(-1);
#endif
		}
	}
	else
	   return(0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_save(lu, fnamepu, fnameps, fnamepa)
**				Save the intermidiate file.
**    PARAMETERS   
**       INPUT  : 
**				lu							file unit number
**				fnamepu					unibase partial file name
**				fnameps					romulus partial file name
**				fnamepa					appgeo partial file name
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_save(lu, fnamepu, fnameps, fnamepa)
	int lu;
	char *fnamepu[];
	char *fnameps[];
	char *fnamepa[];
	{
	int status;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	status = ur_sp02(fnamepu);
	um_save_appgeo(fnamepa);

	return(0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_init_attr()
**				Initilize the default attribute bundle.
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_init_attr()
	{

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	ur_put_attrmdl_color(UM_YELLOW);
	ur_put_attrmdl_layer(1);
	ur_put_attrmdl_pen(1);
	ur_put_attrmdl_line_style((UU_REAL )1);
	ur_put_attrmdl_line_width((UU_REAL) 0);
	ur_put_attrmdl_displayable(UM_DISPLAYABLE);
	ur_put_attrmdl_selectable(UU_TRUE);
	ur_put_attrmdl_label_on(UU_FALSE);

	/*---------- curve display attributes ---------------------------*/
	UM_crvattr.relcirerr = .01; 		
	UM_crvattr.maxpts = 400;		

	/*---------- surface display attributes ---------------------------*/
	UM_srfattr.numupaths = 5;
	UM_srfattr.numvpaths = 5;	
	UM_srfattr.ptsperucrv = 0;	
	UM_srfattr.ptspervcrv = 0;	
	UM_srfattr.maxpaths = 200;	

	/*---------- solids display attributes ---------------------------*/
	UM_solattr.pitch = 3.;	

	return(0);
}
