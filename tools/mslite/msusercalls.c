/*********************************************************************
**	FILENAME: msusercalls.c
**	CONTAINS:
**		uz_user_keydef(ktab,index,xflag)
**    MODULE NAME AND RELEASE LEVEL
**       msusercalls.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:00
*********************************************************************/
#include "usysdef.h"
#include "lcom.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "dmark.h"
#include "driver.h"
#include "dselect.h"
#include "bsym.h"
#include "ustdio.h"
#include "uhep.h"
#include "mdcpln.h"
#include "mattr.h"
#include "mdunits.h"
#include "nclicons.h"
#include "view.h"
#include "zkeysym.h"
#include "atext.h"
#include "dpipe.h"

int UZ_nclipv_view;
extern ATXT_FRM UA_txtattr, UB_txtattr;
extern UX_libdata_bag UB_spl_libdata_rec;
extern UX_libdata_bag UB_libdata_rec;
extern int UV_current_sview;
int NCL_event_reject = 0;
extern int UW_dynamic_funkey;

#define NALPHAKEYS 18

static char *alpha[NALPHAKEYS]={"0","1","2","3","4","5","6","7","8","9",
   ".",",","+","-","/","*","<-","ENTER"};
/*********************************************************************
**	 I_FUNCTION : uz_user_keydef(ktab,index,xflag)
**		This function accepts a key definition structure as input and
**		optionally executes its user defined function.
**	 PARAMETERS	
**		 INPUT  :
**			ktab   = Input function key table.
**			xflag  = 1 = Call this function now.
**		 OUTPUT :
**			index  = Returns the DAS function call, if this key was
**			         programmed using a DAS function.
**     RETURNS: 0 = Normal function call.
**              1 = DAS function call (index contains DAS function).
**              2 = This key is not associated with a function.
**              3 = Alpha key.
**              5 = Normal function call but it will treat as text input
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_user_keydef(ktab,index,xflag)
int xflag;
char **index;
UZ_keytable ktab;
{
	int i;
	int i1,irtn;
	int pos[2];
	int size[2];
	char buf[80];
	char parms[40], text_str[256];

	if (ktab.params[0]=='\0')
		parms[0] = '\0';
	else
		strcpy(parms, ktab.params);
/*
.....No key definition
*/
	if (ktab.type == NOKEY)
	{
		return(2);
	}
/*
.....Function execution keys
*/
	irtn = 0;
	if (xflag || ktab.flag & EXECFL)
	{
		UD_MARK(i1,UU_FALSE);
		if (i1 == 0)
		{
			if (ktab.type == MSLKEY)
			{
				switch (ktab.sub)
				{
				case 0: msl_exit(); break;
				case 1: msl_view_from_axis(5); break;
				case 2: msl_view_from_axis(1); break;
				case 3: msl_view_from_axis(2); break;
				case 4: msl_view_from_axis(6); break;
				case 5: msl_view_from_axis(0); break;
				case 6: msl_view_from_axis(4); break;
				case 7: msl_view_from_axis(7); break;
				case 8: msl_reset_prev(); break;
				case 9: msl_reset_view(0); break;
				case 10: msl_load_machin(); break;
				case 11: msl_mach_attrfrm(); break;
				case 12: msl_vfit(); break;
				case 13: msl_dynamic(); break;
				case 14: ul_ipv_dyncenter(); break;
				case 15: msl_window_view(); break;
				case 16: msl_diag_form(); break;
				case 17: msl_simulate(); break;
				case 18: msl_about(); break;
				}
			}
/*
........Undefined keys
*/
			else
			{
				irtn = 0;
			}
/*
........End of Function execution keys
*/
		}
/*
.....Immediate execution flag
.....Return undefined key
*/
		if (ktab.flag & EXECFL) irtn = -1;
		UD_UNMARK(i1);
	}
	return(irtn);
}
