#define MODULEDBG 1
/*********************************************************************
**    NAME         :  resp02.c
**       CONTAINS:
**       ur_sp02()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       resp02.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:36
*********************************************************************/
#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define index strchr
#define rindex strrchr
#endif
#define	UR_BLKRCL	1024	/* block record length				*/
#include "udebug.h"
#include "usysdef.h"
#include "umoveb.h"
#include "uhep.h"
#include "rmtuple.h"
#include "ribase.h"
#include "ritrnerr.h"
#include "riddldef.h"
#include "rerrdef.h"
#include "xenv1.h"
#include "xfsys1.h"

extern UX_pathname UR_exnam[2];
extern UU_LOGICAL UR_save_modals;  /* boolean, UU_TRUE if save modals */
extern char UR_dpn[UX_MAX_PATH_LEN+1];

/*********************************************************************
**    E_FUNCTION     :  ur_sp02(fnameu)
**       save a unibase database part for later load
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_sp02(fnameu)
char		*fnameu;	/* unibase filename to use	*/
{
	int		status, iostat,stat2;				/* holds status of unibase calls */
	int		lu, i, mode;				           	/* xio logical device number */
   UX_pathname extnam[2],fomode,savnam;
   char fext[20], *last;
#ifndef UU_RS6000
   char *rindex();
#endif


	uu_denter(UU_RTRC,(us,"ur_sp02(name=%s)", fnameu));
	status = ur_chk_data_dict();
	if (status)
	{
		goto s_p99;		/* bailout - still no data dictionary */
	}
	if(*fnameu == '\0')
	{
		status = URM_CANTOPEN_SV;
		uu_dprint(-1,(us,"ERROR:unable to open save file.(no name)"));
		goto s_p99;			/* bailout - no part file name */
	}
	/* flush(clear) out any phantom tuples on the delete stack */
	ur_flush_del_stack();
 
   strcpy (fext,rindex(fnameu,'.')+1);
   i  = strlen(fext);
   if (i > 0)
     {
      if ((last = rindex(fext,UX_QUOTE_CHAR)) != UU_NULL) *last = '\0';
     }
   ux_getsys_ext (2,UR_exnam,extnam);
   mode = 0;
#if UU_COMP == UU_WIN2K
   ul_to_upper (fext);
   ul_to_upper (extnam[0]);
   ul_to_upper (extnam[1]);
#endif
   for (i=0; i<2; i++)
     {
      if (strcmp(fext,extnam[i]) == 0)
        {
         strcpy (fomode,UR_exnam[i]);
         mode = i;
         break;
        }
     }


	/* open blocked file with record length = UR_BLKRCL.  */
	do {
		status = (mode == 0)? 
			ux_create_file(fnameu, 0640, UU_NULL, "BLOCKED", "BINARY",
               "UX_NOEXTRA", &lu, UX_PRTERRS):
         ux_create_file(fnameu, 0600, UU_NULL, "STREAM", "ASCII",
               "UX_NOEXTRA", &lu, UX_PRTERRS);
/*
.....Need to check to see if the file was deleted, if not, exit.
.....An error is called both in ux_delete and below.
.....JLS 4/16/99
       if (status == UX_FOUND) ux_delete(fnameu, UX_PRTERRS);
*/
		if (status == UX_FOUND)
		{
			stat2= ux_delete(fnameu, UX_PRTERRS);
			if (stat2==UX_NO_ACCESS) status = stat2;
		}
	} while (status == UX_FOUND);

	if(status)
	{
		uu_dprint(-1,(us,"ERROR:unable to open save file(status %d).", status));
		status = URM_CANTOPEN_SV;
		goto s_p99;
	}
/*
...Store data in file
*/
	strcpy(savnam,UR_dpn); strcpy(UR_dpn,fnameu);
   status = (mode == 0)? ur_sv02 (lu): ur_txt_sv02 (lu,"part"); 
	strcpy(UR_dpn,savnam);
   if (status != 0) goto s_p99;
/*
... add the environment
*/
	if (UR_save_modals)
  		status = (mode == 0)? ur_environ_out(lu): ur_wrt_txt_env(lu);	

s_p90:
	iostat = ux_close(lu, UX_PRTERRS) ;
	if(iostat)
	{
		status = URM_WRT_ERR;
		ux_delete(fnameu, UX_PRTERRS);
	}
s_p99:
	uu_dexit ;
	return(status)	;
}

