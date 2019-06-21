/*********************************************************************
**
**    NAME         :  f4trvf.c
**
**       CONTAINS:
**				UD_FSTAT ud_trvfrm(fs,fd)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d4trvf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:10
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "dmark.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfmracs.h"
#include "dinput.h"
#include "usysg.h"
#include "ginqatt.h"
#include "ginqatt2.h"
#include "gtblvar4.h"

#define STRDEV		ud_dtstr(UD_STRING, UD_formdev, 22); \
		gsstringmode(UD_ksws, 1, UG_REQUEST, UG_ECHO) ; \
		gsstringmode(UD_ksws, UD_formdev, UG_EVENT, UG_ECHO)
						
#define UNSTRDEV		ud_dtstr(UD_STRING, 1, 1); \
		gsstringmode(UD_ksws, UD_formdev, UG_REQUEST, UG_ECHO) ; \
		gsstringmode(UD_ksws, 1, UG_EVENT, UG_ECHO)
						
extern int UD_formdev;

/*******************************************************************
**
**		E_FUNCTION	:		UD_FSTAT ud_trvfrm(fs,fd)
**
**			traverse-form opens and activates a form on the
**			device supported by the linked-in device interface package,
**			presents all messages (titles, headers) and field prompts,
**			displays all defaults, and gets data from each field (default,
**			or operator entry), checking the data (ifnecessary) against
**			range specifications included in the form structure. If these
**			processes fail it returns with appropriate error statuses,
**			otherwise it returns with a filled in form data structure. Of
**			course, it deactivates and closes the form prior to returning.
**
**			PARAMETERS	:
**				INPUT		:
**					fs		: a pointer to the active form structure which defines
**								the form to be used.
**					fd		: a pointer to the form data structure where data will
**								be returned. This structure may contain defaults,
**								which will override any corresponding default in the
**								form structure.
**				OUTPUT	:
**					fd		: a pointer to the filled out data structure; same as
**								above.
**			RETURNS		:
**					The value of the function is of type UD_fstat:
**						UD_FRMOK - form traversed successfully, good data returned
**						UD_BADFILL - form data fill error, data no good
**						UD_BADPRST - form presentation error, data no good
**						UD_BADACT - form activate error, data no good
**			SIDE EFFECTS:
**					Changes the values of data fields in form data structure
**			WARNINGS		:
********************************************************************/

UD_FSTAT ud_trvfrm(fs, fd)		/* traverse-forms control module */
UD_FSTRUCT *fs;					/* pointer to the active form structure */
UD_FDATA *fd;						/* pointer to the active form data array */
{
	UD_FSTAT stat;					/* return status of call */
	int markval;
	UD_FSTAT ud_actfrm(), ud_prsnta(), ud_filall();
	int oldlncolor,oldtxcolor;

	uu_denter(UU_DTRC,(us,"entering ud_trvfrm:fstruct ptr=%x, fdata ptr=%x",
							fs, fd));

/* -- first init form struct access routines -- */

	ud_setfs(fs);
	
/* -- then activate the form -- */

	oldlncolor = gqlinecolor();
	oldtxcolor = gqtextcolor();
	gslinecolor(1);
	gstextcolor(1);

	if((stat=ud_actfrm()) == UD_ACTOK)
	{
		STRDEV;
		UD_MARK(markval, UU_FALSE);
		if(markval != 0)
		{
		   ud_dactfm(); 
		   gslinecolor(oldlncolor);
		   gstextcolor(oldtxcolor);
		   UNSTRDEV;
		   UD_UNMARK(markval);
		   stat = UD_BADFILL;
			goto done;
		}

/* 	-- ifnot error, present the form -- */

		if((stat=ud_prsnta()) == UD_PRSTOK)
		{

/*			-- ifnot error, fill the form -- */

			if((stat=ud_filall(fd)) == UD_FILLOK)
			{
				stat = UD_FRMOK;
				ud_dactfm();					/* close form */
				gslinecolor(oldlncolor);
				gstextcolor(oldtxcolor);
			}

/* 		-- bad fill error -- */

			else
			{
			   ud_dactfm();
			   gslinecolor(oldlncolor);
			   gstextcolor(oldtxcolor);
			}
			UNSTRDEV;
		   UD_UNMARK(markval);
			goto done;
		}
		else
		{

/* 		-- present error -- */

			ud_dactfm();
			gslinecolor(oldlncolor);
			gstextcolor(oldtxcolor);
			UNSTRDEV;
			UD_UNMARK(markval);
			goto done;
		}
	}

/* -- error activating form -- */

	else
	{
		ud_dactfm();
		gslinecolor(oldlncolor);
	   gstextcolor(oldtxcolor);
	}
done:;
	ud_pstat(stat, "ud_trvfrm");
   uu_dexit;
   return(stat);
}
