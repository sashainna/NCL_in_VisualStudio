/*********************************************************************
**    NAME         :  tiomain.c
**       CONTAINS:
**     	 	uio_main()
**      		uio_init()
**      		uio_init2()
**				uio_load_part()
**      		uio_get_filid(iges_fd,iges_fd1,iges_fd2)
**      		uio_write(fd,buf,chsize)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tiomain.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:53
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
#include		"nclver.h"
#include		"ribase.h"
#if UU_COMP!=UU_VAXVMS
#include <sys/types.h>
#include <sys/stat.h>
#endif
#if UU_COMP != UU_WIN2K
#include		"tigmf.h"
#endif

#if UU_COMP == UU_WIN2K
#include <io.h>
#include <fcntl.h>
#define write _write
#define read _read
#define close _close
#define open _open
#define creat _creat
#define lseek _lseek
#endif


extern UU_STORE *uu_toolstore;
/* UU_STORE *uu_get_current_store();	*/
UU_LOGICAL first = UU_TRUE;

extern UM_int2 NCL_ubas_unit;
UU_LIST UIO_surfs;
/*********************************************************************
**    I_FUNCTION :  uio_main()
**       The main routine for the iges out
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_main()

	{
	int	status;
	int	iges_fd;			/* file id for the output igesfile */
	int	iges_fd1, iges_fd2;	/* temporary file id for directory and parameter 
											section  */
	UX_pathname us,fname,f2name;	
	int	scount, gcount, dcount, pcount;	/* start,global,directory,parameter
															section sequence count */
	int count;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

  	uio_init();
	status = -1;
	no_of_views = 0;
	current_dir_number = 0;
	number_of_masters = 0;
	sequence_no = 0;
#if UU_COMP != UU_WIN2K
	if (UW_MOTIF!=1)
		UIG_drawing_only = UU_FALSE;
#endif
	if ((status=uio_load_part(f2name)) == 0)
	{
/*
.....initialize surface list
*/
		if (UIO_surf_list == NULL) 
		{
			uu_list_init (&UIO_surfs, sizeof (surf_list), 100, 100);
			UIO_surf_list = &UIO_surfs;
		}

		/* check if any drawings in the database */

		count = ur_get_tuple_count(UM_DRAWING_REL);
#if UU_COMP != UU_WIN2K
		if(count > 0)
		{
			if (UW_MOTIF!=1)
			{
				uig_clear();
  		 		uig_str_out("Model Geometry/Drafting and Drawings cannot both\n",
																		UU_TRUE);
  		 		uig_str_out("be output in the same IGES part file. \n",UU_TRUE);
  		 		uig_str_out("Please specify which type is required.\n",UU_TRUE);
				uig_str_out("\nOutput Drawings only? (y or n) ",UU_TRUE); 
				if(uig_ans() == 0)		/* answer is yes */
				{
					UIG_drawing_only = UU_TRUE;
				}
				uig_clear();
			}
			
		}
#endif
		/* get IGES output file name */

		if (uio_get_filid(fname,&iges_fd,&iges_fd1,&iges_fd2))
		{
	 				/* put the start section information */
			uio_start_sec(iges_fd1,&scount);		
	   			/* put the global section information */
	   	uio_global_sec(iges_fd1,fname,&gcount);

		  /* put information on the screen for the user */

			uig_clear();
			if(!UIG_drawing_only) 
   			sprintf(us,"Converting Geometry/Drafting from the UNIBASE file:  %s.\n", f2name);
			else
   			sprintf(us,"Converting Drawings from the UNIBASE file: %s.\n", f2name);
   		uig_str_out(us ,UU_TRUE);
		iges_open_process_win("Writing data to the IGES file");
		iges_disply_as_percent(50);
			sprintf(us,"\nWriting data to the IGES file:  %s.\n", fname);
   		uig_str_out(us ,UU_TRUE);

	   			/* process the directory and parameter section */
	   	uio_dir_par(iges_fd1,iges_fd2,&dcount,&pcount);	
					/* process the terminate section */
			uio_term_sec(iges_fd,iges_fd1,iges_fd2,scount,gcount,dcount,pcount);	
		iges_close_process_win();
			status = 0;
#if UU_COMP==UU_VAXVMS
			delete("iges1.tmp");
			delete("iges2.tmp");
#else
			unlink("iges1.tmp");
			unlink("iges2.tmp");
#endif
			if (UIO_surf_list != NULL) 
			{
				uu_list_free (UIO_surf_list);
				UIO_surf_list = NULL; 
			}
		}
	}
	return(status);
}	

/*********************************************************************
**    I_FUNCTION :  uio_init()
**       the init routine for igestool 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_init()
{
   int status,i;
	static char *label[]={"PT","PV","LN","VE","PL","CI","CV","SF","SH","MX",
		"PN","SO"};
	/*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	if(first)
		{
		uu_toolmalloc_init();
		uu_toolstore = (UU_STORE *)uu_get_current_store();
		for (i=0;i<NREL;i++)
		{
			strcpy(geo_lab[i],label[i]);
			lab_flag[i] = 0;
		}
/*
.....ignore debug in WNT
*/
#if UU_COMP != UU_WIN2K
		uu_init_debug();				/* initilize appropriate Unicad systems */
#endif
		ux_init_table(UX_PRTERRS);
		}
	ur_init_unibase();
	if (UIG_reinit_lab)
	{
		um_init_labels();
		iln = ipt = ipl = ici = icn = icv = isf = isg = ipn = ixx = 0;
	}
	uc_uni_init_all();
	ncl_uni_init_all();
	ua_init_drafting();
	uig_init_attr();
	uio_init2();
	ag_init();
	ur_init_unibase_stat(UR_STAT_INIT);
/*
..... Read in config file, modify label modals when first start up.
*/
	if (first)
	{
		uig_init_color();
		status = uig_modal_config();
		first = UU_FALSE;
	}
	uig_change_labelmdl(UU_TRUE);
/*
.....Set units
*/
	uio_units = 1.;
	if (output_units == 1) uio_units = 1. / 25.4;
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION :  uio_init2()
**       the init routine for igestool 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_init2()

{
	int status;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
	status = 0;
	uio_trflst_init();
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     :  status = uio_load_part()
**  		load a part into unibase which has previously been saved 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uio_load_part(f2name)
	UX_pathname f2name;	

	{
	int		status				;	/* holds status of unibase calls		*/
	int		iostat				;	/* holds status of xio calls			*/
	int i;
	/* base filename to use	*/
	UX_pathname fname, tmp_file;
	/* unibase filename to use	*/
	UX_pathname fnameu;
	/* Romulussolids filename to use	*/
	/* Applied Geometry filename to use	*/
	UX_pathname fnamea;
	int	 	len,mode;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
 NCL_ubas_unit = -1;

	/* get user's filename to load from */
	if (Iges_batch)
		strcpy(fname, iges_ptfile);
	else
	{
#if UU_COMP != UU_WIN2K
	if (UW_MOTIF!=1)
	{
   	uig_str_out("Enter the part file name: ",UU_TRUE);
   	uig_str_in(fname);
	}
	else
		uig_get_ptname(fname);
#else
	iges_wntget_ptname(fname);
#endif
	}
/*
.....added quotes in filename because call ux_file_inquire
.....will asked quoted name
.....otherwise, it return "not exist" when with a path
.....Yurong
*/
	tmp_file[0] = '\"';
	len = strlen(fname);
	for (i=0; i<len; i++)
		tmp_file[i+1] = fname[i];
	tmp_file[i+1] = '\"';
	tmp_file[i+2] = '\0';
	strcpy(fname, tmp_file);

	strcpy(f2name, fname);
		/* get part area path name , and build full pathnames */

	/* get the name of Unibase portion of the file */
	mode = 0;
	status = ux_file_inquire(UU_NULL,"^UR_PART_AREA",fname,UU_NULL,"UR_UNB_FILE",
									&mode,&iostat,fnameu,UX_PRTERRS);
	if (mode == (mode|UX_NEXISTS))
	  {
   		uig_error("part file does not exist ");
#if UU_COMP != UU_WIN2K
		if (UW_MOTIF!=1)
			uig_prompt(2, &mode);
#endif
		return(-1);
	  }
	if(iostat == UX_NFOUND)
	  {
   		uig_error("part file is not xio compatible");
#if UU_COMP != UU_WIN2K
		if (UW_MOTIF!=1)
			uig_prompt(2, &mode);
#endif
		return(-1);
	  }

	/* get the name of Applied Geometry portion of the file */
   mode = 0;
   status = ux_file_inquire(UU_NULL,"^UR_PART_AREA",fname,UU_NULL,"UR_AG_FILE",
   								&mode,&iostat,fnamea,UX_PRTERRS);
/* if (mode == (mode|UX_NEXISTS)) */
/*   { */
/*   	uig_str_out("AG part file does not exist ",UU_TRUE); */
/* 	uig_prompt(2, &mode); */
/* 	uu_dexit;	 */
/* 	return(-1); */
/*   } */

	/* do load */
	uig_str_out("\nLoading part file .....\n",UU_TRUE);
	status = ur_reset_unibase();
	if (status == UU_SUCCESS)
		status = um_load_appgeo(fnamea, UU_TRUE);
	if (status == UU_SUCCESS)
		status = ur_lp02(fnameu,0);
	if (status == 0)
		ncl_post_load_color(0);
	if (status == 0)
		status = um_post_load_appgeo();
/* if(status != 0)
   {

    ur_report_error(status);
    uig_str_out("Error in loading part file\n",UU_TRUE);
    uig_prompt(2, &mode);
    uu_dexit;	
    return(-1);
    }   
*/
  	uig_str_out("FINISHED LOADING \n\n",UU_FALSE);
	return(0);
	}

/*********************************************************************
**    I_FUNCTION :  uio_get_filid(iges_fd,iges_fd1,iges_fd2)
**       Get the iges output file name and open it for writing.
**			Open two temporary file to store the directory and parameter
**			section
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_get_filid(fname,iges_fd,iges_fd1,iges_fd2)
	UX_pathname	fname;
	int	*iges_fd, *iges_fd1, *iges_fd2;
	
	{
	int		status, ans;
	UX_pathname p_buff,obuf;
	UU_LOGICAL	done;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
	done = UU_FALSE;
	while (!done)
	{
#if UU_COMP != UU_WIN2K
		if (UW_MOTIF!=1)
		{
   		uig_str_out("Enter the output iges file name: ",UU_TRUE);
   		uig_str_in(fname);
		}
		else
			uig_get_outname(fname,"igs");
#else
		iges_wntget_outname(fname);
#endif

	
		if ((status=access(fname,0)) != 0)		/* file does'nt exist */
		{
#if (UU_COMP!=UU_WIN2K)
			*iges_fd = creat(fname, 0644);		/* open for write */
#else
			*iges_fd = open(fname, _O_CREAT|_O_RDWR, _S_IREAD|_S_IWRITE);
#endif
			done = UU_TRUE;
		}
		else
		{
#if UU_COMP != UU_WIN2K
			if (UW_MOTIF==1)
			{
				ul_short_filename(fname,obuf,40);
				sprintf(p_buff, "file %s exists. replace it?", fname);
				ans = uig_mfyesno(NULL, "File Exist", p_buff);
			}
			else
			{
				uig_str_out("\nfile exists. replace it? (y or n) ",UU_TRUE); 
				ans = uig_ans();
			}
#else
			ul_short_filename(fname,obuf,40);
			sprintf(p_buff, "file %s exists. replace it?", fname);
			ans = iges_wntyesno(NULL, "File Exist", p_buff);
#endif
			if(ans == 0)		/* answer is yes */
			{
				if ((status=ux_delete(fname,UX_PRTERRS)) != 0)
				{
#if UU_COMP != UU_WIN2K
					if (UW_MOTIF==1)
					{
						uig_mfmsg_box(NULL, "Error!", "error in deleting file!");
						return(status);
					}
					uig_str_out("  error in deleting file",UU_TRUE);
#else
					iges_wnt_msg(NULL, "Error!", "error in deleting file!");
#endif
					return(UU_FALSE);			/* file has not been opened yet */
				}
			   else
				{
#if (UU_COMP!=UU_WIN2K)
					*iges_fd = creat(fname, 0644);		/* open for write */
#else
					*iges_fd = open(fname, _O_CREAT|_O_RDWR, _S_IREAD|_S_IWRITE);
#endif
					done = UU_TRUE;
				}
			}
			else							/* answer is no */
			{
#if UU_COMP != UU_WIN2K
				if (UW_MOTIF==1) return(UU_FALSE);
				uig_str_out("\ntry again(y or n)? ",UU_TRUE);
				if(uig_ans() != 0)		/* don't want to try again */
				{
					return(UU_FALSE);			/* file has not been opened yet */
				}
#endif
				return(UU_FALSE);
			}
		 }
	  }	/* end of while */

			/* try to open two temporary files */
#if (UU_COMP!=UU_WIN2K)
	*iges_fd1 = creat("iges1.tmp", 0664);
	*iges_fd2 = creat("iges2.tmp", 0664);
#else
	*iges_fd1 = open("iges1.tmp", _O_CREAT|_O_RDWR, _S_IREAD|_S_IWRITE);
	*iges_fd2 = open("iges2.tmp", _O_CREAT|_O_RDWR, _S_IREAD|_S_IWRITE);
#endif
	uig_clear();
	return(UU_TRUE);
}

/*********************************************************************
**    I_FUNCTION :  uio_write(fd,buf,chsize)
**       write to a file 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_write(fd,buf,chsize)
	int	fd, chsize;
	char *buf;

	{
  int	n;
  char *ux_getenv();
/*  
	char *p;
*/

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
	n = write(fd,buf,chsize);
	/*
	if ((p=ux_getenv("NEWlines")) != UU_NULL)
		write(fd,"\n",sizeof(char));
	*/
	return 0;
	}
