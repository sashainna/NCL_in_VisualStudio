/********************************************************************r
**    NAME         :  nufile.c
**       CONTAINS:
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nufile.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:07
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "mfort.h"
#include "nclfc.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "lcom.h"
#include "lumb.h"
#include "xenv1.h"

extern char *nisstr;

/* static UU_LOGICAL first_time = UU_TRUE; */

/*********************************************************************
**    E_FUNCTION     : nclu_read_part_prog()
**       Prompt the user for a part program name and call NCL to
**         read in the part program.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**          UU_SUCCESS if the file was successfully loaded/saved otherwise
				UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_read_part_prog(filename, choice)
int choice;
char *filename;
{
	int numint, i, user_opened,istat;
	UX_pathname descrip,ext,buff,str;
	char sufx[UX_SUFFIX_LEN];
	UM_f77_str name;
	UM_int2 errflg, wflg;
	UM_int2 kfl;
	char *p, *index(), *ux_getenv();;


	uu_denter( UU_MTRC,(us,"nclu_read_part_prog()"));

	user_opened = 0;
	str[0] = '\0';
	UM_init_f77_str(name, str, UX_MAX_PATHLEN);
/*
.....Use default file name "and" Extension
.....Bobby  -  3/13/92
*/
	strcpy(str,UL_program);
	strcat(str,".");
	strcat(str,UL_program_suffix);
/*
.....use file browser
.....Yurong 9/15/98
*/
	ext[0] = '\0';
	p = ux_getenv("UL_PROGRAM_SUFFIX");
	if (p != 0) strcpy(sufx,p);
	else strcpy(sufx,"*");
	ud_format_file_filter(ext,sufx);
	sprintf(descrip, "Part Program Files (%s)",ext);

	if ((filename==NULL)||(filename[0]=='\0'))
	{
		if (choice == 1)
		{
			ud_get_filename("Enter Part Filename", "Enter Part Filename",
				  ext, str, &numint, descrip, 0, UU_FALSE);
		}
		else
		{
			ud_get_filename("Enter Part Filename", "Enter Part Filename",
				  ext, str, &numint, descrip, 1, UU_FALSE);
		}
		user_opened = 1;
	}
	else
	{
		strcpy(str, filename);
	}
	i = strlen(str);
	if (i<=0) return(UU_FAILURE);
	if(i > 0)
	{
		if (choice == 1)
		{
			wflg=0;
			istat = savepp(UM_addr_of_f77_str(name), &i, &wflg);
			if (istat != 0) return(UU_FAILURE);
		}
		else
		{
			wflg=0;
			loadpp(UM_addr_of_f77_str(name), &i, &errflg, &wflg);
			if (errflg != 0) 
			{
				ul_short_filename(str,buff,60);
				uu_uerror1(/* no file */UA_NCL, 5, buff);
				return(UU_FAILURE);
			}
			if (user_opened)
				nclc_save_recent_file(str, 0);
		}
		ptdfnm(UM_addr_of_f77_str(name),&i);
/*
.....store filename
*/
		ptppnm(UM_addr_of_f77_str(name),&i);
		ptdfnm(UM_addr_of_f77_str(name),&i);
		kfl = UL_create_cl;
		setclf(UM_addr_of_f77_str(name),&i,&kfl);
		kfl = UL_create_as;
		setas(UM_addr_of_f77_str(name),&i,&kfl);
	}

	/* if user entered a filename */
	if (i > 0)
	{
		/* update status area with new program name */
/*
.....use default, it will include ".pp" or ".u" type
.....Yurong changed 8/26/98
*/
/*              uz_actprogram(UL_program); */
		uz_actprogram("");
	}

   uu_dexit;
	return(UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : nclu_get_clfile()
**       Open a clfile.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_get_clfile()
   {
   int numint, i,j;
	UX_pathname descrip,ext,str;
   char *p, ext1[UX_SUFFIX_LEN], *ux_getenv();
   UM_f77_str name;
	UM_int2 ival;
	UM_int4 nco,ist[2],ien[2];

   uu_denter( UU_MTRC,(us,"nclu_get_clfile()"));

   str[0] = '\0';
   UM_init_f77_str(name, str, UX_MAX_PATH_LEN);
   gtrtnm(UM_addr_of_f77_str(name),&nco);
	name[nco] = '\0';

/*
.....use file browser
.....Yurong 9/15/98
*/
	strcpy(descrip, "Clfiles File (");
	strcpy(ext,"*.");
	p = ux_getenv("UL_CLFILE1_SUFFIX");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
	}       
	else strcat(ext,"cl");
	strcat(descrip, ext);
	strcat(descrip, ")|Secondary Clfiles (*.");
			
	p = ux_getenv("UL_CLFILE2_SUFFIX");
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
		strcat(ext,"cln");
		strcat(descrip, "cln");
	}
	strcat(descrip, ")");

	ud_get_filename("Enter Clfile to Save", "Enter Clfile to Save",
		  ext, str, &numint, descrip, 1, UU_FALSE);

   if(numint > 0)
	{
      i = strlen(str);
      for (j=i; j<UX_MAX_PATH_LEN; j++) str[j] = ' ';
/*		ival = 1;*/
/*		setclf(UM_addr_of_f77_str(name),&i,&ival);*/
		ist[0] = ist[1] = 0; ien[0] = ien[1] = 0;
		clsave(UM_addr_of_f77_str(name),ist,ien,&ival);
		if (ival != 0) ud_wrerr("Error trying to create an external clfile.");
	}
   uu_dexit;
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_get_asfile()
**       Open apt source file.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_get_asfile()
   {
   int numint, i,j;
   UX_pathname str,descrip;
   UM_f77_str name;
	UM_int2 ival,ifl;
	UM_int4 nco,ist[2],ien[2];

   uu_denter( UU_MTRC,(us,"nclu_get_asfile()"));

   str[0] = '\0';
   UM_init_f77_str(name, str, UX_MAX_PATH_LEN);
   gtrtnm(UM_addr_of_f77_str(name),&nco);
	name[nco] = '\0';

/*
.....use file browser
.....Yurong 9/15/98
*/
   strcpy(descrip, "APT Source Files (*.as)");
	ud_get_filename("Enter APT Source File to Save",
		"Enter APT Source File to Save",
		"*.as", str, &numint, descrip, 1, UU_FALSE);
   if(numint > 0)
	{
      i = strlen(str);
      for (j=i; j<UX_MAX_PATH_LEN; j++) str[j] = ' ';
/*		ival = 1;*/
/*      setas(UM_addr_of_f77_str(name),&i,&ival);*/
		ifl = 3;
		ival = 0;
		ist[0] = ist[1] = ien[0] = ien[1] = 0;
		aptsrc(&ifl,UM_addr_of_f77_str(name),&ival,ist,ien);
	}
   uu_dexit;
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_erase_clfile()
**       Erases (deletes) the entire internal clfile.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_erase_clfile()
{
/*
.....use Motif Question box
.....Yurong 9/15/98
*/
	if (ud_yesno(0, "Are you sure you want to erase the entire clfile?",
							"Erase Clfile"))
	{
		clinit();
	}
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_edit_clfile()
**       Spawns off the TED editor to edit the internal clfile.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_edit_clfile()
{
	int jerr,nc,i;
	UX_pathname udir,cfil;
	char cmd[UX_MAX_PATH_LEN+40];
	UM_int2 ifl,ival,iclf,ierr;
	UN_clstruc *ist,*ien;
	UM_f77_str fcfil;
/*
.....Verify TED utility exists
*/
	ul_verify_exe("post","pted",udir,&jerr);
	if (jerr != 0)
	{
		ud_wrerr("You must have the Pted utility to edit clfiles.");
		goto done;
	}
/*
.....Build temporary clfile name
.....from 'partpgm.ncl#' number
*/
	ifl = 297;
	getifl(&ifl,&ival);
	sprintf(cfil,"partpgm_%d.cl",ival);
/*
.....Save clfile to temp file
*/
	nc = strlen(cfil);
	for (i=nc;i<UX_MAX_PATH_LEN;i++) cfil[i] = ' ';
	UM_init_f77_str(fcfil,cfil,UX_MAX_PATH_LEN);
	ist = UU_NULL; ien = UU_NULL;
	clsave(UM_addr_of_f77_str(fcfil),&ist,&ien,&ierr);
	if (ierr != 0)
	{
		ud_wrerr("Error trying to create a temporary clfile.");
		goto done;
	}
/*
.....Spawn Pted to edit clfile
*/
	strncpy(cmd,cfil, nc);
	cmd[nc] = '\0';
	ul_run_process(udir,"pted",cmd);
/*
.....Load clfile back in
*/
	iclf = 1;
	ival = 1;
	clload(&iclf,UM_addr_of_f77_str(fcfil),&nc,&ival,&ierr);
	if (ierr == 0)
	{
		ncl_swap_clfiles();
	}
/*
.....Error loading clfile
*/
	else
	{
		ud_wrerr ("Error trying to reload clfile.  Clfile edits are lost.");
	}
done:;
	return(0);
}
