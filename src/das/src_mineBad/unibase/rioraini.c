
/*********************************************************************
**    NAME         :  ur_oracle_init
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rioraini.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:45
*********************************************************************/

#include "udebug.h"

#ifdef UR_ORACLE

#include "usysdef.h"
#include "rerrdef.h"

UU_LOGICAL	UR_ora_inited = UU_FALSE;	/* flag for ORACLE initialized */

#include <stdio.h>
/* start of ORACLE includes	*/
#include <std.h>
#include <osfdef.h>
#include <osddef.h>
#include <pgadef.h>
#ifndef STDDEF
#include <stddef.h>
#endif
#ifndef GENDEF
#include <gendef.h>
#endif
#ifndef IORDEF
#include <iordef.h>
/* end of ORACLE includes	*/
#endif

ur_oracle_init(dbname)
char        *dbname;          /* database name for oracle */
{
/* Do IOR clear/warm before every invocation */
	ctbdef	*ctbp;
	int		status;
	reg	text	*pfilep;
	word	list	=	0;
	word	reindx=	0;

	extern short   cur[5][32]; /* logon data area and 4 cursors */

	uu_denter(UU_RITRC,(us,"attempt to connect to Oracle"));
	status = 0;
	if (!UR_ora_inited)
	{
		pfilep = D_PFIL;
		ioreos(OSFISYS);
		ioreos(OSFIOR);
		iorclo();
 		uu_dprint(UU_RITRC,(us,"close done"));
 		for ( ctbp = iorctb;  ctbp->ctbpmp;	++ctbp )
 		{
			ctbp->ctbflg = 0;
 		}
 		uu_dprint(UU_RITRC,(us,"cleared tables"));
		iorprm(pfilep,list);
 		uu_dprint(UU_RITRC,(us,"prewarm done"));
		iorwrm(reindx);
		pgaflg = 0;
		uu_dprint(UU_RITRC,(us," ORACLE database warm started "));
		/* login as unicad/oracle */
		if(olon(cur[0],dbname,-1,-1,-1,0))
		{
			uu_dprint(-1,(us,
						"ERROR:ur_trans_out_oracle unable to log in to oracle"));
			status = URM_RDBMS_ERR;
			goto shutdown;
		} 
		uu_dprint(UU_RITRC,(us,"logged onto ORACLE database"));
	 
		/* open cursors */
		if(oopen(cur[1],cur[0],-1,-1,127,-1,-1)   ||
			oopen(cur[2],cur[0],-1,-1,127,-1,-1)   ||
			oopen(cur[3],cur[0],-1,-1,127,-1,-1)   ||
			oopen(cur[4],cur[0],-1,-1,127,-1,-1)
			)
		{
			uu_dprint(-1,(us,"ERROR:ur_trans_out_oracle error opening cursors"));
			status = URM_RDBMS_ERR;
			goto shutdown;
		} 
		uu_dprint(UU_RITRC,(us,"cursors 1-4 opened"));

		/* disable auto-commit */
		if(ocof(cur[0]))
		{
			uu_dprint(-1,(us,
			  	"ERROR:ur_trans_out_oracle unable to disable auto commit"));
			status = URM_RDBMS_ERR;
			goto shutdown;
		}
		UR_ora_inited = UU_TRUE;	/* set init done */
	}
shutdown:
	uu_dexit;
	return(status);
}

#endif
