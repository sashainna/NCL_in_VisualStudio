/*********************************************************************
**
**    NAME         :  udebug1.c.c
**
**    CONTAINS:
**    		uu_sys_err_recovery
**
**     MODULE NAME AND RELEASE LEVEL
**       udebug1.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/01/17 , 13:56:54
**
*********************************************************************/

#include "usysdef.h"
#include "ustdio.h"
#include "udebug.h"
#include "dasnog.h"

/*********************************************************************
**
**    E_FUNCTION     :  int uu_sys_err_recovery()
**       Default routine to recover when application runs out of dynamic memory
**
**    PARAMETERS   
**       INPUT  : 
**          subsys = subsystem number of error
**				errno1 = error number
**				actflag = if = 0 then return to caller, 
**								 = 1 then exit
**								 = -1 then jump to root
**
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uu_sys_err_recovery(actflag, subsys, errno1, errparm1, errparm2)
int actflag;
int subsys;
int errno1;
int errparm1, errparm2;
{
   int markval;

   uu_denter(UU_DTRC,(us,"enter uu_sys_err_reovery, sub=%d, err=%d, flag=%d",
               subsys, errno1, actflag));

/*   -- post error message -- */

   uu_uerror2(subsys, errno1, errparm1, errparm2);
   UD_MARK(markval, UU_FALSE);

/*   -- force operator to read help message -- */

   if(markval == 0)
      uj_help();

/* -- shut down R/P and jump to root -- */

   uu_dprint(UU_DTRC,(us,"leave uu_sys_err_recovery"));
   if(actflag == 0)
   {
/*
.....Calling 'uu_reset_mpe()' causes NCL to
.....go into a continuous loop.
.....Bobby  -  2/24/92
*/
/*      uu_reset_mpe();      /* return system to known state */
      UD_UNMARK(markval);
      uu_dexit;
      return;
   }
   else if(actflag == -1)
   {
/*
.....Calling 'uu_reset_mpe()' causes NCL to
.....go into a continuous loop.
.....Bobby  -  2/24/92
*/
/*      uu_reset_mpe();      /* return system to known state */
      UD_UNMARK(0);
      UD_enablejmp = UU_TRUE;
      ud_jump(UD_markptr, UU_TRUE);
   }
   else
   {
      exit(999);
   }
}
