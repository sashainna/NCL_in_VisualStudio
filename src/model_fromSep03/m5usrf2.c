/*********************************************************************
**    NAME         :  m5usrf2.c
**       CONTAINS: interface routines for surface analysis
**			umu_agsrf_analysis()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m5usrf2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:07
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "dasnog.h"
#include "dselmask.h"
#include "dmark.h"
#include "class.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include "mdpick.h"
#include "mdebug.h"
#include "mdeval.h"
#include "mdcpln.h"

#include "ag_incl.h"

/*********************************************************************
**    E_FUNCTION     : umu_agsrf_analysis()
**			Prompt the user to pick surfaces for which the area, volumn, 
**			and centroid is to be calculated.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_agsrf_analysis()

	{
	int status;
	int numint;
	int markval;
	UU_LOGICAL initialize;
	struct UM_agsrf_rec srf;
	UM_coord centroid;
	UU_REAL area;
	UU_REAL vol;
	UU_REAL tol;

	uu_denter(UU_MTRC,(us,"umu_agsrf_analysis()"));

	ud_lgeo(UU_TRUE, UD_surface);

	ud_ldas(UD_DASSELECT, /* pick surfaces for analysis */
		UM_APPGEO, 78, UU_NULL, 1, &numint, UD_NODEFAULT);
	if (numint < 1) goto done;

	initialize = UU_TRUE;
	tol = .01;
	ux_window_open("UD");
	sprintf(UM_sbuf, "   area       volume                 centroid\n");
	ud_wrwin (UM_sbuf);
	ud_wrwin ("\n");
	UD_MARK(markval, UU_TRUE);
	if(markval == 0)
		{
		while (ud_gnxt(initialize, UU_NULL, &(srf.key), 1) == UU_TRUE)
			{
			initialize = UU_FALSE;
			status = uc_retrieve_data(&srf, sizeof(srf));
			if (status == UU_SUCCESS)
				{
				ag_cntd_area_srf(srf.srfaddr, tol, centroid, &area);
				ag_cntd_srf_vol(srf.srfaddr, tol, centroid, &vol);
				UM_len_inttoext(area, area);
				UM_len_inttoext(area, area);
				UM_len_inttoext(vol, vol);
				UM_len_inttoext(vol, vol);
				UM_len_inttoext(vol, vol);
				UM_cc_inttoext(centroid, centroid);
				sprintf(UM_sbuf, "%10.4f   %10.4f   <%10.4f,%10.4f,%10.4f>\n",
					area, vol, centroid[0], centroid[1], centroid[2]);
	      	ud_wrwin (UM_sbuf);
				}
			}
		ud_hakt(/* enter any key to terminate */ UD_DASHEP, 17);
		}
	UD_UNMARK(markval);
	ud_kiwin();

done:;
	uu_dexit;
	}

