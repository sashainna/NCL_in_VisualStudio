/*********************************************************************
**    NAME         : aattrib.c
**       CONTAINS:
**    		ua_set_txt_fontname(fontname)
**   	
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aattrib.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:30
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aattrib.c 3.1 2/2/88 14:36:29 single"};
#else
static char uu_sccsident[]={"@(#) aattrib.c 3.1 2/2/88 14:36:29 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"

/*********************************************************************
**    E_FUNCTION     : void		ua_set_txt_fontname(fontname)
**       Set font name and get font number from environment table
**       if font name is valid name.
**    PARAMETERS   
**       INPUT  : 
**          fontname -  Trial fontname.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_txt_fontname(fontname)
char		fontname[31];
	{
	char		fontenv[101];
	char		fontnnn[101];
	int		retcode;
	int		outrecs;
	int		fontnum;
	int		status;


	uu_denter(UU_STRC,(us,"ua_set_txt_fontname(fontname=%s)", fontname));
	strcpy(fontenv,"FONT_");
	strcat(fontenv,fontname);
	uu_sgetenv(fontenv,fontnnn);
	if( ( strcmp( fontnnn, "" ) == 0 ) )
		{
		uu_uerror1(UA_DRAFTING,5,fontname);
		}
	else
		{
		retcode = sscanf(fontnnn,"FONT%d",&(fontnum));
		if( ( retcode==1 ) )
			{
			UA_txt_fontnum = fontnum;
			strcpy(UA_txt_fontname,fontname);
			}
		}
	uu_dexit;
	}
