/*********************************************************************
**    NAME         :  tsomain.c
**       CONTAINS:
**     	 	utp_out_main()
**				utp_out_load_part()
**      		utp_out_write()
**      		utp_close_file()
**    COPYRIGHT 2014 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tsomain.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:22
*********************************************************************/

#include		"ustdio.h"
#include		"ulist.h"
#include		"usysdef.h"
#include		"udebug.h"
#include 	"ualloc.h"
#include		"xenv1.h"
#include		"xfsys1.h"
#include		"mdrel.h"
#include		"tiges.h"
#include		"tstep.h"
#include		"nclver.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <io.h>
#include <fcntl.h>
#define write _write
#define read _read
#define close _close
#define open _open
#define creat _creat
#define lseek _lseek


extern UU_STORE *uu_toolstore;

extern UM_int2 NCL_ubas_unit;
UU_LIST UIO_surfs;

void utp_close_file();
static int S_get_filid();

/*********************************************************************
**    I_FUNCTION :  utp_out_main()
**       Main routine for STEP/Out
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
utp_out_main()
{
	int	status;
	int	fd,fd1,fd2;
	UX_pathname us,f2name;	
	int	scount, gcount, dcount, pcount;	/* start,global,directory,parameter
															section sequence count */
	int count;
	UU_LOGICAL first = UU_TRUE;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

  	uio_init();
	status = -1;
	no_of_views = 0;
	current_dir_number = 0;
	number_of_masters = 0;
	sequence_no = 0;
/*
.....Load the Unibase file
*/
	if ((status=utp_out_load_part(f2name)) == 0)
	{
/*
.....Set units
*/
		if (output_units == -1) output_units = NCL_ubas_unit;
		if (output_units == -1) output_units = 0;
		uio_units = 1.;
		if (output_units == 1) uio_units = 1. / 25.4;
/*
.....Initialize surface list
*/
		if (UIO_surf_list == NULL) 
		{
			uu_list_init (&UIO_surfs, sizeof (surf_list), 100, 100);
			UIO_surf_list = &UIO_surfs;
		}
/*
.....Get STEP output file name
*/
		if (S_get_filid(iges_outfile,&fd,&fd1,&fd2))
		{
/*
.....Reset count of entities
*/
			utp_reset_translated();
/*
.....Open listing file
*/
			ul_remove_quotes(f2name);
			open_listing_file(f2name,"STEP");
/*
.....Create the HEADER section
*/
			utp_out_header(fd);		
/*
.....Put information on the screen for the user
*/
  			sprintf(us,"Converting Geometry from the UNIBASE file:  %s.\n",
				f2name);
  			uig_str_out(us ,UU_TRUE);
			iges_open_process_win("Writing data to the STEP file");
			iges_disply_as_percent(50);
			sprintf(us,"\nWriting data to the STEP file:  %s.\n", iges_outfile);
  			uig_str_out(us ,UU_TRUE);
/*
.....Create the DATA section
*/
			utp_out_data(fd);
			iges_close_process_win();
			status = 0;
/*
.....Free the lists
*/
			if (UIO_surf_list != NULL) 
			{
				uu_list_free (UIO_surf_list);
				UIO_surf_list = NULL; 
			}
			utp_free_lists(UU_FALSE);
			utp_free_geo_lists();
/*
.....Now print the statistics
*/
			utp_print_statistics();
/*
.....Close the STEP file
*/
			utp_close_file(fd);
		}
	}
	return(status);
}	

/*********************************************************************
**    E_FUNCTION     :  utp_out_load_part(f2name)
**  		Loads an external Unibase file for conversion to a STEP file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          f2name  = Name of Unibase file loaded.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_out_load_part(f2name)
UX_pathname f2name;	
{
	int status,iostat,i,len,mode;
	char *p;
	UX_pathname fname, tmp_file, fnameu;
/*
.....Initialize routine
*/
 NCL_ubas_unit = -1;
/*
.....Get user's filename to load from
*/
	if (Iges_batch)
		strcpy(fname, iges_ptfile);
	else
		iges_wntget_ptname(fname);
	tmp_file[0] = '\"';
	len = strlen(fname);
	for (i=0; i<len; i++)
		tmp_file[i+1] = fname[i];
	tmp_file[i+1] = '\"';
	tmp_file[i+2] = '\0';
	strcpy(fname, tmp_file);
	strcpy(f2name, fname);
/*
.....Get part area path name , and build full pathnames
........Get the name of Unibase portion of the file
*/

	mode = 0;
	ul_default_ftype("u",fname);
	status = ux_file_inquire(UU_NULL,UU_NULL,fname,UU_NULL,UU_NULL,
									&mode,&iostat,fnameu,UX_PRTERRS);
	if (mode == (mode|UX_NEXISTS))
	{
   	uig_error("Unibase file does not exist.");
		return(-1);
	}
	if (iostat == UX_NFOUND)
	{
   	uig_error("Could not open file");
		return(-1);
	}
/*
.....Load the external Unibase
*/
	uig_str_out("\nLoading part file .....\n",UU_TRUE);
	status = ur_reset_unibase();
	if (status == UU_SUCCESS)
		status = ur_lp02(fnameu,0);
	if (status == 0)
		ncl_post_load_color(0);
  	uig_str_out("Finished loading \n\n",UU_FALSE);
	strcpy(iges_unifile,fnameu);
	ul_break_fname(fnameu,tmp_file,iges_igsfile);
	ul_remove_quotes(iges_igsfile);
	p = strchr(iges_igsfile,'.');
	if (p != UU_NULL) *p = '\0';
	return(0);
}

/*********************************************************************
**    I_FUNCTION :  utp_out_write(fd,buf,nc)
**       Write to a file.
**    PARAMETERS   
**       INPUT  : 
**          fd       = File descriptor to write to.
**          buf      = Character string to write.
**          nc       = Number of chars in 'buf'.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
utp_out_write(fd,buf,nc)
int fd, nc;
char *buf;
{
  int	n;
/*
.....Write out record
*/
	n = write(fd,buf,nc);
	write(fd,"\n",1);
	return 0;
}

/*********************************************************************
**    E_FUNCTION :  utp_close_file(fd)
**       Closes the open STEP file.
**    PARAMETERS   
**       INPUT  : 
**          fd    = File descriptor for output STEP file.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_close_file(fd)
int fd;
{
	close(fd);
}

/*********************************************************************
**    I_FUNCTION :  S_get_filid(fname,fd,fd1,fd2)
**       Get the STEP output file name and open it for writing.
**			Open two temporary file to store the directory and parameter
**			section
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          fname = Name of output STEP file.
**          fd    = File descriptor for output IGES file.
**          fd1   = File descriptor for temporary IGES file.
**          fd2   = File descriptor for temporary IGES file.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_filid(fname,fd,fd1,fd2)
UX_pathname	fname;
int *fd, *fd1, *fd2;
{
	int status, ans;
	UX_pathname p_buff,obuf;
	UU_LOGICAL	done;
/*
.....Get the output STEP filename
*/
	done = UU_FALSE;
	while (!done)
	{
		iges_wntget_outname(fname);
		ul_default_ftype("stp",fname);
/*
.....File does not exist
*/
		if ((status=access(fname,0)) != 0)
		{
			*fd = open(fname, _O_CREAT|_O_RDWR, _S_IREAD|_S_IWRITE);
			done = UU_TRUE;
		}
/*
.....File already exists
.....See if the user wants to replace it
*/
		else
		{
			ul_short_filename(fname,obuf,40);
			sprintf(p_buff, "file %s exists. replace it?", fname);
			ans = iges_wntyesno(NULL, "File Exist", p_buff);
/*
........Replace the file
*/
			if(ans == 0)
			{
				if ((status=ux_delete(fname,UX_PRTERRS)) != 0)
				{
					iges_wnt_msg(NULL, "Error!", "error in deleting file!");
					return(UU_FALSE);			/* file has not been opened yet */
				}
			   else
				{
					*fd = open(fname, _O_CREAT|_O_RDWR, _S_IREAD|_S_IWRITE);
					done = UU_TRUE;
				}
			}
/*
........Don't replace the file
*/
			else
			{
				return(UU_FALSE);
			}
		 }
	  }	/* end of while */

/*
.....Open temporary files
*/
/*
	*fd1 = open("iges1.tmp", _O_CREAT|_O_RDWR, _S_IREAD|_S_IWRITE);
	*fd2 = open("iges2.tmp", _O_CREAT|_O_RDWR, _S_IREAD|_S_IWRITE);
*/
	return(UU_TRUE);
}
