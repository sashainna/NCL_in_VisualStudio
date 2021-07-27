/*********************************************************************
**    NAME         :  nupl.c
**       CONTAINS: User interface routines for plane creation.
**         nclu_pl_ijkd()
**         nclu_pl_pt_pt_pt()
**         nclu_pl_pv()
**         nclu_pl_pt_parlel_pl()
**         nclu_pl_parlel_pl()
**         nclu_pl_pt_perpto_ve()
**         nclu_pl_pt_pt_perpto_pl()
**         nclu_pl_pt_perpto_pl_pl()
**         nclu_pl_ln_perpto_pl()
**		     nclu_pl_fit()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nupl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:11
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "modef.h"
#include "mdclass.h"


#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION     : nclu_pl_ijkd()
**       description
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_ijkd()

{
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_pl_ijkd"));

   while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....Check to see if auto label is on and if not prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PLANE/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);
/*
.....Prompt user for vector.
*/
      status = ncl_add_vector(&cmdbuf, 130);

      if (status == NCL_ALTACTION)
         status = ncl_add_vector(&cmdbuf, 130);
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for distance.
*/
      status = ncl_add_length(&cmdbuf, 131);
/*
.....If user selected a distance prompt for a disply point
.....If no display point is selected, process command as is.
*/
		if(status==NCL_OKINPUT)
		{
      	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
			status = NCL_OKINPUT;
		}
/*
.....Process command.
*/
      if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(&cmdbuf);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
		}

	}
done:;
   uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_pt_pt_pt()
**       description
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_pt_pt_pt()

{
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_pl_pt_pt_pt"));

   while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
......If auto_label is off, prompt user for label.
*/

      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PLANE/ into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);
/*
.....Promt user for the first point or point-vector
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 132, UD_ncl_ptpv);
/*
.....If done was selected, exit.
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for second point or point-vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 133, UD_ncl_ptpv);
/*
.....If user selected second point prompt for third point or point-vector.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 134, UD_ncl_ptpv);
/*
.....If status is still okay, prompt for display point.
*/
		if(status==NCL_OKINPUT)
		{
      	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
/*
.....If done was selected, the default will be used so set status to NCL_OKINPUT.
*/
			status = NCL_OKINPUT;
		}
/*
.....Process command.
*/
      if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(&cmdbuf);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
		}
	}
done:;
   uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_pv()
**       description
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_pv()

{
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_pl_pv"));

   while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....Check to see if auto label is on and if not prompt for label.
*/

      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PLANE/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);
/*
.....Prompt user with  point-vector or line defining plane.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 241, UD_ncl_pvln);
/*
.....If done was selected, exit.
*/
		if(status!=NCL_OKINPUT) goto done;

/*
.....Prompt user for display point or point-vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
/*
.....Process command.
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
   uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_pt_parlel_pl()
**       description
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_pt_parlel_pl()

{
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_pl_pt_parlel_pl"));

   while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....Check to see if auto label is off, if so, prompt for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PLANE/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);
/*
.....Prompt for a point.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 135, UD_ncl_ptpv);
		if (status != NCL_OKINPUT) goto done;
/*
.....Put PARLEL, into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_parlel, NCL_comma);
/*
.....Prompt user to pick a plane.
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 136, UD_ncl_pl);
/*
.....Prompt user for display point.
*/
		if(status==NCL_OKINPUT)
		{
     		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
			status = NCL_OKINPUT;
		}
/*
.....Process command.
*/
      if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
		}
	}
done:;
   uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_parlel_pl()
**       description
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_parlel_pl()

{
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_pl_parlel_pl"));

   while (UU_TRUE)
   {
/*
.....Initialize command buffer.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off prompt for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PLANE/ into command.
*/
      status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);
/*
.....Put PARLEL into command..
*/
      status = ncl_add_token(&cmdbuf, NCL_parlel, NCL_comma);
/*
.....Prompt user for plane to be parallel to .
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 137, UD_ncl_pl);
/*
.....If done was selected, exit
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Add modifier.
*/
      status = ncl_add_modifier(&cmdbuf, NCL_XYZ_MODIFIER);
/*
.....Prompt for distance.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_length(&cmdbuf, 138);
/*
.....Prompt for display point.
*/
		if(status==NCL_OKINPUT)
		{
      	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
			status = NCL_OKINPUT;
		}
/*
.....Process command.
*/
      if ((status == NCL_OKINPUT))
      {
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
      }

   }
done:;
   uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_pt_perpto_ve()
**       description
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_pt_perpto_ve()

{
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_pl_pt_perpto_ve"));

   while (UU_TRUE)
   {
/*
.....Initialize command buffer.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....Check to see if auto_label in on, if not, prompt for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PLANE/ into command.
*/
      status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);

/*
.....Changed 139 to 135, displays same message. JLS 2/19/99
.....Prompt for a point.
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 135, UD_ncl_ptpv);
/*
.....If done was selected, exit.
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Put PERPTO into command.
*/
      status = ncl_add_token(&cmdbuf, NCL_perpto, NCL_comma);
/*
.....Prompt user for a vector or a point_vector.
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 140, UD_ncl_vepv);
/*
.....Prompt user for a display point.
*/
		if(status==NCL_OKINPUT)
		{
      	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
			status = NCL_OKINPUT;
		}
/*
.....Process command
*/
      if ((status == NCL_OKINPUT))
      {
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
      }
   }
done:;
   uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_pt_pt_perpto_pl()
**       description
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_pt_pt_perpto_pl()

{
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_pl_pt_pt_perpto_pl"));

   while (UU_TRUE)
   {
/*
.....Intialize command buffer.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....Check to see if auto label is on.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PLANE/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);
/*
.....Prompt for a point or point-vector.
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 141, UD_ncl_ptpv);
/*
.....If Done was selected, exit.
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Prompt for second point or point vector.
*/
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 142, UD_ncl_ptpv);
/*
.....Put PERPTO, into the command.
*/
      if (status == NCL_OKINPUT)
		{
         status = ncl_add_token(&cmdbuf, NCL_perpto, NCL_comma);
/*
.....Prompt user for plane.
*/
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 143, UD_ncl_pl);
/*
.....Prompt user for display point.
*/
			if(status==NCL_OKINPUT)
			{
				status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
				status = NCL_OKINPUT;
			}
		}
/*
.....Process command.
*/
      if ((status == NCL_OKINPUT))
      {
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
      }
   }
done:;
   uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_pt_perpto_pl_pl()
**       description
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_pt_perpto_pl_pl()

{
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_pl_pt_perpto_pl_pl"));

   while (UU_TRUE)
   {
/*
.....Intialize command buffer.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off, ask for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PLANE/ into command.
*/
      status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);
/*
.....Prompt user for point or point vector.
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 144, UD_ncl_ptpv);
/*
.....If done was selected, exit.
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Put PERPTO, into command.
*/
      status = ncl_add_token(&cmdbuf, NCL_perpto, NCL_comma);
/*
.....Prompt user for first plane.
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 145, UD_ncl_pl);

      if (status == NCL_OKINPUT)
		{
/*
.....Prompt user for second plane
*/
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 146, UD_ncl_pl);
/*
.....Prompt user for display point.
*/
			if(status==NCL_OKINPUT)
			{
  		    	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
				status = NCL_OKINPUT;
			}
		}
/*
.....Process command.
*/
      if ((status == NCL_OKINPUT))
      {
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
      }
   }
done:;
   uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_ln_perpto_pl()
**       description
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_ln_perpto_pl()

{
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_pl_ln_perpto_pl"));

   while (UU_TRUE)
   {
/*
.....Initialize command buffer.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off, prompt user for label.
*/

      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PLANE/ into command.
*/
      status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);
/*
.....Prompt user for line.
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 147, UD_ncl_pvln);
/*
.....If done was selected, exit.
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Put PERPTO into command.
*/
      status = ncl_add_token(&cmdbuf, NCL_perpto, NCL_comma);
/*
.....Prompt user for a plane.
*/
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 148, UD_ncl_pl);

/*
....If user did not select a plane, remove the PERPTO
....in the command and prompt of a display point.  Then
....process command.
*/
      if (status == NCL_NOINPUT)
      {
			status = ncl_del_token(&cmdbuf, NCL_perpto, NCL_comma);
  		  	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
	    	ncl_call(&cmdbuf);
		}
      else if (status == NCL_OKINPUT)
      {
/*
.....Prompt user for display point.
*/
  		  	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
			status = NCL_OKINPUT;
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
      }

  }
done:;
   uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_pt_ve_ve()
**       Interface for a plane through a point parallel to 2 vectors
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_pt_ve_ve()
{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_pl_pt_ve_ve"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
......If auto_label is off, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put PLANE/ into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);
/*
.....Promt user for the first point or point-vector
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 135, UD_ncl_ptpv);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Put PARLEL, into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_parlel, NCL_comma);
/*
.....Prompt user for first vector or point-vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 126, UD_ncl_vepv);
/*
.....Prompt for second vector or point-vector.
*/
		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 127, UD_ncl_vepv);
/*
.....If status is still okay, prompt for display point.
*/
		if (status == NCL_OKINPUT)
		{
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 139, UD_ncl_ptpv);
/*
..... If done was selected, the default will be used so set status to 
..... NCL_OKINPUT.
*/
			status = NCL_OKINPUT;
		}
/*
.....Process command.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(&cmdbuf);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
	uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_fit()
**       create an optimal plane from cv/sf
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pl_fit()
{
	NCL_cmdbuf cmdbuf;
	int numint,status;
	UM_PLOCREC pick;
	UU_KEY_ID um_get_pickkey();

	uu_denter(UU_MTRC,(us,"nclu_pl_fit"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on and if not prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
		if (status == NCL_DONE) goto done;
/*
.....Prompt user for cv/sf.
*/
		ud_lgeo(UU_TRUE,UD_cvsf);
		um_dl_pldas(UD_DASPCKLOC, /*pick cv/sf */ UA_NCL, 248,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;
/*
.....Put PLANE/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_pl, NCL_nocomma);
/*
.....Put FIT into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_fit, NCL_nocomma);
/*
.....Put picked entity into command.
*/
		ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);

		if ((status == NCL_NOINPUT)) goto done;
/*
.....Prompt user to enter a scalar variable.
*/
		status = ncl_add_str(&cmdbuf, 234, NCL_nocomma);
/*
.....Process command.
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
   uu_dexit;
	return 0;
}

