#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**    NAME         : vdamain.c
**       CONTAINS:
**             vda_main
**    COPYRIGHT 1989 (c) MILLS DATA SYSTEMS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       vdamain.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:55
*********************************************************************/

#include "tiges.h"
#include "vda.h"
#include "xenv1.h"
#include "usignal.h"
#include "ustdio.h"
#include <curses.h>

#if UU_COMP==UU_VAXVMS
#include iodef
#include descrip
int UIG_tty_channel;    /* set by SYS$ASSIGN */
#endif

extern int listfd;
extern int read_count;
extern int num_entities;
extern char *vda_command[];

int vda_fd;   /* vda file designator */
char ext_filename[UX_MAX_PATH_LEN];

/*********************************************************************
**    I_FUNCTION     :  vda_main
**       Main routine for the vda translator.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vda_main()
	{
	int status;                         /* status of VDA data file */
	int choice_num;                     /* option number selected by the user */
	UU_LOGICAL testit;
	UU_LOGICAL uig_ans();
	extern int NAUTVDA;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   /* GET AUTHORIZATION FOR VDA */
	ncl_init_auth("VDA");
	if (!NAUTVDA)
		exit();	/* window not yet initialized */

	uig_set();                       /* initialize some constants */
	uig_trap();                      /* set system traps */

l_p01:
	vda_intro(&status);              /* get initialize option from user */
	if(status < 0) 						/* exit system */
		{
		uig_clear();
		uig_tty_mode(RESET);
		uig_tty_mode(STANDARD);
		exit();
		}
	vda_create_directory();
	no_of_views = 0;
	current_dir_number = 0;
	number_of_masters = 0;
	sequence_no = 0;
	vda_in_convert();             /* convert ascii to neutral format */
	t_num = 0;
	uig_clear();  
	close(vda_fd);
   	 
#if UU_COMP==UU_VAXVMS

	/* delete temporary unblocked file */
	if (delete("vda.tmp"))
		uig_str_out("\nError! - can't delete vda.tmp", UU_TRUE);

#endif

	goto l_p01;
	}
#endif
