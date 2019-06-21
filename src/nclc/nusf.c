/*********************************************************************
**    NAME         :  nusf.c
**       CONTAINS: User interface routines for surface creation.
**
**       nclu_sf_ruled
**       nclu_sf_bnd_slp
**       nclu_sf_bnd_0
**       nclu_sf_offset
**       nclu_sf_fit_cv
**       nclu_sf_pl_pl_ra
**       nclu_sf_fillet
**       nclu_sf_mesh
**       nclu_sf_quilt
**       nclu_sf_net
**       nclu_nsf_multi_cv
**       nclu_nsf_revolv
**       nclu_sf_out
**       nclu_sf_trim
**       nclu_sf_redef
**       nclu_trim_remove
**       nclu_shade_surf
**       nclu_unshade_surf
**       nclu_analyze_surf
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nusf.c , 25.3
**    DATE AND TIME OF LAST MODIFICATION
**       02/08/17 , 11:41:36
*********************************************************************/

#include "mgeom.h"
#include "usysdef.h"
#include "wsgl.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdpick.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"

extern UD_POPUPREC nmodfy[];
extern UU_LOGICAL NCL_noname;
extern UU_KEY_ID NCL_nokey;
int nclu_temp_redef = 0;
UU_KEY_ID NCL_skey0 = NULLKEY;

/*********************************************************************
**    E_FUNCTION     : nclu_sf_ruled(ityp)
**       description
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric surface (SURF)
**                 2 - Create NURB surface (NSURF)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_sf_ruled(ityp)
int ityp;
{
   NCL_cmdbuf cmdbuf;
   int *mask;
   int status;
   int reltyp;				/* store relation type of first entity picked */

   uu_denter(UU_MTRC,(us,"nclu_sf_ruled"));

   if (ityp == 1) mask = (int *) UD_ncl_rldentity;
	else mask = (int *) UD_ncl_nsfcv;

   while (UU_TRUE)
   {
/*
.....Initialize command buffer and status.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off, prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put either SURF/ or NSURF/ into the command.
*/
		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_nsf, NCL_nocomma);
/*
.....Prompt user for first boundary curve.
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 164, mask, &reltyp);
/*
.....If user selected done, exit
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....If it was a point, prompt user for second curve, limit selection
.....to lines circles and curves, (i.e. user will be unable to pick
.....another point).
*/
		if ((reltyp == UM_POINT_REL) || (reltyp == NCL_POINT_REL))
		   status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 165, UD_ncl_curve);
		else
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 165, mask);
/*
.....If all is well, process the command, otherwise, return to 
.....first prompt.
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
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_bnd_slp(ityp)
**       description
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric surface (SURF)
**				   2 - Create NURB surface (NSURF)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_sf_bnd_slp(ityp)
int ityp;
{
   NCL_cmdbuf cmdbuf;
   int *mask, *slope_mask;
   char *sfwd;
   int status;
	int flag;

   uu_denter(UU_MTRC,(us,"nclu_sf_bnd_slp"));
/*
.....Set masks and definition word depending on ityp.
*/
   if (ityp == 1)
   {
     mask = (int *)UD_ncl_curve;
     slope_mask = (int *) UD_ncl_slope;
     sfwd = NCL_sf;
   }
   else
   {
     mask = (int *) UD_ncl_nsfcv;
     slope_mask = (int *) UD_ncl_nsfslope;
     sfwd = NCL_nsf;
   }

   while (UU_TRUE)
	{
/*
.....Initialize command buffer and status.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
		flag = 0;
/*
.....If auto label is off, prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put either SURF/ or NSURF/ into command 
*/
      ncl_add_token(&cmdbuf, sfwd, NCL_nocomma);
/*
.....Ask for boundaries and slope curves, if a boundary
.....is selected, but no slope curve, put 0 in for the 
.....slope curve.  If no boundary curves are selected, exit
.....If user selects done and one boundary curve has been
.....selected, go back to first prompt.  Otherwise when
.....user selects done during the prompt for a boundary
.....curve, process command.
*/
      while (UU_TRUE)
		{
			status = ncl_add_label1(&cmdbuf, 166, mask);
			if (status != NCL_OKINPUT)
			{
				if (flag<1) goto done;
				if (flag>1) status = NCL_OKINPUT;
				break;
			}
			flag++;
			status = ncl_add_label1(&cmdbuf, 167, slope_mask);
			if (status == NCL_DONE) 
				status=ncl_add_token(&cmdbuf, "0", NCL_comma);
		}
/*
.....Process command.
*/
		if (status == NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
   uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_bnd_0(ityp)
**       description
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric surface (SURF)
**				   2 - Create NURB surface (NSURF)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_sf_bnd_0(ityp)
int ityp;
{
   NCL_cmdbuf cmdbuf;
   int *mask;
   int status;
	int flag;

   uu_denter(UU_MTRC,(us,"nclu_sf_bnd_0"));
/*
.....It is a parameteric surface.
*/
   if (ityp == 1) mask =(int *)  UD_ncl_curve; 
/*
.....Or it is a NURB surface.
*/
   else mask =(int *)  UD_ncl_nsfcv;

	while (UU_TRUE)
	{
/*
....Initialize the command buffer and status.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
		flag = 0;
/*
.....If auto label is off, prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put either SURF/ or NSURF/ into the command.
*/
		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_nsf, NCL_nocomma);

      while (status == NCL_OKINPUT) 
		{
/*
.....Prompt user for a boundary.
*/
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 166, mask);
/*
.....If done was selected and this is the first time here, exit routine.
.....If done was selected and this is the second time here, return to first prompt
.....If done was selected and this is at least the third time here, process command.
*/
         if (status != NCL_OKINPUT)
			{
				if (flag == 0)
					goto done;
				else if (flag>1) 
					status = NCL_OKINPUT;
				break;
			}
			flag++;
/*
.....Put 0 in as slope curve.
*/
         status = ncl_add_token(&cmdbuf, "0", NCL_comma);
      }
/*
.....If everything is cool, process command.
*/
		if (status == NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
      	ncl_add_cmdbuf(&cmdbuf);
      	ncl_call(&cmdbuf);
		}		
	}
done:;
   uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_fit_cv(sfflg,ityp)
**       description
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric surface (SURF)
**                   2 - Create NURB surface (NSURF)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_sf_fit_cv(sfflg,ityp)
int  sfflg,ityp;

{
   NCL_cmdbuf cmdbuf;
   int *mask;
   int status;
	int flag;

   uu_denter(UU_MTRC,(us,"nclu_sf_fit_cv"));
/*
.....Set mask according to the type of surface being made.
*/
   mask =(int *)  UD_ncl_cv; /* if ityp==1 */
   if (ityp == 2 )
   {
      if (sfflg == 1) mask =(int *)  UD_ncl_nsfcv; 
      else mask =(int *)  UD_ncl_nsfcvthru;
   }

   while (UU_TRUE)
	{
/*
.....Initalize command buffer and status.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
		flag = 0;
/*
.....Check to see if auto label is off and if it is prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put either SURF/ or NSURF/ into command.
*/
		if (ityp == 1)
			status =ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
		else
			status =ncl_add_token(&cmdbuf, NCL_nsf, NCL_nocomma);
/*
.....Put FIT, into command.
*/
      status =ncl_add_token(&cmdbuf, NCL_fit, NCL_nocomma);
/*
.....If sfflg is equal to 1, prompt user for curves until done is hit
*/
      if (sfflg == 1)
		{
         while (status == NCL_OKINPUT) 
			{
            status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 168, mask);
/*
.....If done was selected and it is the first time here, exit
.....If done was hit and it is second time here, goto first prompt
.....Otherwise if done was hit, process command.
*/
            if (status != NCL_OKINPUT)
				{
					if (flag == 0)
						goto done;
					else if (flag>1)
						status = NCL_OKINPUT;
					break;
				}
				flag++;
			}
		}

       /*   added to create a menu for sf/fit,cv,thru,cv. kathy */

		if (sfflg == 2)
		{
/*
.....Prompt user for first curve.
*/
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 168, mask);
/*
.....If done was selected, exit.
*/
			if (status != NCL_OKINPUT) goto done;
/*
.....Put THRU, into the command.
*/
			status = ncl_add_token(&cmdbuf, NCL_thru, NCL_comma);
/*
.....Prompt user for second curve.
*/
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 168, mask);
		}
 
/*
.....If all is well, process command.
*/
		if (status == NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
      	ncl_add_cmdbuf(&cmdbuf);
      	ncl_call(&cmdbuf);
		}
	}
done:;
   uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_offset()
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
nclu_sf_offset()

{
   NCL_cmdbuf cmdbuf;
   int status;
	char str[256];
	char strmod[256];
	int choice, outchoice;

   uu_denter(UU_MTRC,(us,"nclu_sf_offset"));

   while (UU_TRUE)
	{
/*
.....Initialize command and status.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off, prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put SURF/OFFSET, into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
      status = ncl_add_token(&cmdbuf, NCL_offset, NCL_comma);
/*
.....Prompt user for label of surface to offset.
.....str is going to hold the label of the surface, it will
.....be placed in the command buffer here and then it will
.....be repeatedly placed in the command buffer until the user
.....chooses to select another surface.  JLS 6/9/99
*/
		status = ncl_get_dlabel(UD_DASPCKLOC, &str, 520, UD_ncl_netentity);
/*
.....If done was selected, exit.
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Put label into command buffer.
*/
		ncl_add_token(&cmdbuf,str,NCL_comma);
/*
.....Get modifier from user.
.....So that the user may select many different offset values with
.....the same surface and the same modifier, call ud_ddas to get the
.....modifier, this way the selected modifier maybe used again without
.....asking the user over and over again for it.   JLS 6/9/99

		status = ncl_add_modifier(&cmdbuf, NCL_XYZ_MODIFIER);
*/
getmodifier:;
		status = ud_ddas(UD_POPUP, &nmodfy[NCL_XYZ_VECTOR],
			 &choice,1, &outchoice, UD_NODEFAULT);
		switch (choice)
      {
			case 1:
				strcpy(strmod, NCL_xlarge);
				status = NCL_OKINPUT;
				break;
			case 2:
				strcpy(strmod, NCL_xsmall);
				status = NCL_OKINPUT;
				break;
			case 3:
				strcpy(strmod, NCL_ylarge);
				status = NCL_OKINPUT;
				break;
			case 4:
				strcpy(strmod, NCL_ysmall);
				status = NCL_OKINPUT;
				break;
			case 5:
				strcpy(strmod, NCL_zlarge);
				status = NCL_OKINPUT;
				break;
			case 6:
				strcpy(strmod, NCL_zsmall);
				status = NCL_OKINPUT;
				break;
			case 7:
				status = ncl_get_dlabel(UD_DASPCKLOC, &strmod, 218, UD_ncl_vepv);
				break;
			default:
				status = NCL_NOINPUT;
				strcpy(strmod,"");
				break;
		}
		if (status == NCL_OKINPUT)
			ncl_add_token(&cmdbuf,strmod,NCL_comma);
/*
.....Continue to ask for offset values until user selects done.
*/
		while(status == NCL_OKINPUT)
		{
        	status = ncl_add_str(&cmdbuf, 434, NCL_nocomma);
/*
.....Process command.
*/
      	if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
/*
.....Initialize command buffer and put SURF/OFFSET,sf,modifier, into command
*/
      		ncl_init_cmdbuf(&cmdbuf);
      		if (!NCL_auto_label)
        			status = ncl_add_name(&cmdbuf, 1);
				status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
      		status = ncl_add_token(&cmdbuf, NCL_offset, NCL_comma);
				ncl_add_token(&cmdbuf,str,NCL_comma);
				ncl_add_token(&cmdbuf,strmod,NCL_comma);
			}
			else
/*
.....User did not select an offset value, so initialize the buffer
.....and place SURF/OFFSET,sf into command and go back and get a different
.....modifier.
*/
			{
      		ncl_init_cmdbuf(&cmdbuf);
      		if (!NCL_auto_label)
        			status = ncl_add_name(&cmdbuf, 1);
				status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
      		status = ncl_add_token(&cmdbuf, NCL_offset, NCL_comma);
				ncl_add_token(&cmdbuf,str,NCL_comma);
				goto getmodifier;
			}
		}
	}
done:;
   uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_pl_pl_ra()
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
nclu_sf_pl_pl_ra()

{
   NCL_cmdbuf cmdbuf,save_cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_sf_pl_pl_ra"));

   while (UU_TRUE)
	{
/*
.....Initialize the command buffer and status.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put SURF/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
/*
.....Get modifier.
*/
		status = ncl_add_modifier(&cmdbuf, NCL_XYZ_MODIFIER);
/*
.....If done was selected, exit.
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for a plane.
*/
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 170, UD_ncl_pl);
/*
.....Get second modifier.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_modifier(&cmdbuf, NCL_XYZ_MODIFIER);
/*
.....Get second plane.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 171, UD_ncl_pl);
/*
.....Put RADIUS, into command.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_token(&cmdbuf, NCL_radius, NCL_comma);
/* 
.....Get Start and End radii
*/
      if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf, 524);

		if (status == NCL_OKINPUT)
		{
/* 
........Save "cmdbuf" before end_rad is given so the statement can be
........restored without an extra comma if "NCL_DONE" is the response
........to the prompt for the end_rad request.  
*/
			save_cmdbuf = cmdbuf;
			status = ncl_add_length(&cmdbuf, 525);

/*	
........Set status to NCL_OKINPUT if optional "end_rad" value not given.
*/

			if (status != NCL_OKINPUT)
			{
				cmdbuf = save_cmdbuf;
				status = NCL_OKINPUT;
			}
		}

/*
.....vp 12-apr-93 PV added
.....Prompt user for point or point-vector.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 543, UD_ncl_ptpv);
/*
.....Prompt user for point or point-vector.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 544, UD_ncl_ptpv);
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
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_fillet()
**       Interactive building of a CAM Fillet SURF statement.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_sf_fillet()

{
   NCL_cmdbuf cmdbuf;
   NCL_cmdbuf save_cmdbuf;
   int status, rad_type;

   uu_denter(UU_MTRC,(us,"nclu_sf_fillet"));

   while (UU_TRUE)
	{
/*
.....Initialize command buffer and status.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label if off, prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put SURF/FILLET into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_fillet, NCL_comma);
/*
.....Prompt user for first surface fillet is tangent to.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 521, UD_ncl_filletents);
/*
.....If done was selected, exit.
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Get the second surface fillet is tangent to.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 522, UD_ncl_filletents);

/*
....vp 12-apr-93 PV added
.....Prompt user for a near point or point-vector.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 523, UD_ncl_ptpv);

/*******************************************************************/
/* RADIUS INDICATOR TYPE DECISION - RADIUS INDICATOR TYPE DECISION */
/*******************************************************************/
/* 
.......Ask if user want to use radius values (== 1) or a radius curve (== 2) 
*/

		if (status == NCL_OKINPUT)
		{
			status = ncl_popup(NCL_RAD_IND, &rad_type);
/*
.....If rad_type is equal to zero then done had been selected
*/
			if (rad_type == 0) status = NCL_NOINPUT;
		}

		if ((status == NCL_OKINPUT))
		{
			if (rad_type == 1)
			{
/* 
.....User selected to use scalars as the radii, prompt user for radius.
*/
				status = ncl_add_length(&cmdbuf, 524);

				if (status == NCL_OKINPUT)
				{
/* 
.....Save "cmdbuf" before end_rad is given so the statement can be
.....restored without an extra comma if "NCL_DONE" is the response
.....to the prompt for the end_rad request.  
*/
					save_cmdbuf = cmdbuf;
					status = ncl_add_length(&cmdbuf, 525);

/*	
.....Set status to NCL_OKINPUT if optional "end_rad" value not given.
.....and restore cmdbuf.
*/
					if (status == NCL_NOINPUT || status == NCL_DONE)
					{
						cmdbuf = save_cmdbuf;
						status = NCL_OKINPUT;
					}
				}
			}
			else if (rad_type == 2)
			{
/*
.....User opted to use a curve as the radius.
*/
				status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 526, UD_ncl_cv);
			}
		}
/*
.....Prompt user for the optional limit curve.
*/
		if (status == NCL_OKINPUT)
		{
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 527, UD_ncl_cvpn);
/* 				
.....Set status to NCL_DONE if optional "limit_cv_or_pn" name not given. 
*/
		   status = NCL_OKINPUT;
		}
/*  
.....GENERATE STATEMENT AND SEND TO FORTRAN ROUTINES TO PROCESS  
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
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_mesh()
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
nclu_sf_mesh()

{

   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_sf_mesh"));

   while (UU_TRUE)
	{
/*
.....Initialize command buffer and status.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off, prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put SURF/MESH into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_mesh, NCL_comma);
/*
.....Prompt user for file containing mesh surface.
*/
		status = ncl_add_str(&cmdbuf, 204, NCL_comma);
/*
.....If done was selected, exit.
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user to pick a surface.
*/
		status = ncl_add_str(&cmdbuf, 205, NCL_comma);
/*
.....Prompt user for starting patch.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_str(&cmdbuf, 206, NCL_comma);
/*
.....Prompt user for ending patch.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_str(&cmdbuf, 207, NCL_nocomma);
/*
.....Process command.
*/
      if ((status == NCL_OKINPUT) ||(status == NCL_DONE))
		{
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
		}
	}
done:;
   uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_quilt()
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
nclu_sf_quilt()

{
   NCL_cmdbuf cmdbuf;
   int status;
	int flag;

   uu_denter(UU_MTRC,(us,"nclu_sf_quilt"));

   while (UU_TRUE)
	{
/*
.....Initialize command buffer, status and flag;
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
		flag = 0;
/*
.....If auto label if off, prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put SURF/QUILT, into command.
*/

		status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_quilt, NCL_comma);
/*
.....Prompt user for file containing quilt surface.
*/
		status = ncl_add_str(&cmdbuf, 208, NCL_comma);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for patches until user selects done;
*/
      while (status == NCL_OKINPUT)
		{
         if (status == NCL_OKINPUT)
            status = ncl_add_str(&cmdbuf, 209, NCL_comma);
		}

      if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
		}
	}
done:;
   uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_net(option)
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
nclu_sf_net(option)
int option;
{
   NCL_cmdbuf cmdbuf;
   int status;
	int numint;

   uu_denter(UU_MTRC,(us,"nclu_sf_net"));

   while (UU_TRUE)
   {
/*
.....Initialize command buffer and status.
*/
		ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put SURF/ into command.
*/
      status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
/*
.....If user doesn't want to use THRU, process here.
*/
		if (option ==0)
		{
/*
.....Limit geometry to surfaces allowed in a net surface
*/
			ud_lgeo(UU_TRUE, UD_ncl_netentity);
/*
.....Prompt user to pick the surfaces, the selection menu is brought up
.....here.
*/
			status = ud_ldas(UD_DASSELECT, UA_NCL, 477,UU_NULL, NCL_MAXPICK,
								&numint, UD_NODEFAULT);
/*
.....If nothting was picked, exit
*/
			if (numint <=0) goto done;
/*
.....Put the labels of the picked entities into the command.
.....make sure to set status to NCL_OKINPUT, since done had
.....to be hit to exit the SELECT menu, it is returned NCL_DONE
*/
			ncl_get_select_labels (&cmdbuf, numint);	
			status = NCL_OKINPUT;
		}
/*
.....User wants to use THRU
*/
		else
		{
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 475, UD_ncl_netentity);
			if (status != NCL_OKINPUT) goto done;
			status = ncl_add_token(&cmdbuf,NCL_thru, NCL_comma);
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 476, UD_ncl_netentity);
		}
		
/*
.....Process command.
*/
		if (status == NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
done:;
   uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_nsf_multi_cv()
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
nclu_nsf_multi_cv()
{

	NCL_cmdbuf cmdbuf;
	int status;

   uu_denter(UU_MTRC,(us,"nclu_nlsf_multi_cv"));

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
.....Put NSURF/EDGE, into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_nsf, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_edge, NCL_comma);
/*
.....Not sure why this was in the routine, it doesn't appear to have any effect
.....on the command if it is in there or not in there.
		status = ncl_add_token(&cmdbuf, l1, NCL_comma);
*/
/*
.....Prompt user for interpolation type.
*/
		status = ncl_add_str(&cmdbuf, 600, NCL_comma);
/*
.....If done was selected, exit.
*/
      if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for first boundary curve.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 601, UD_ncl_nsfcv);
/*
.....Prompt user for second thru fourth boundary curves.
*/
      if (status == NCL_OKINPUT) 
         status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 602, UD_ncl_nsfcv);
     	if (status == NCL_OKINPUT) 
        	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 603, UD_ncl_nsfcv);
    	if (status == NCL_OKINPUT) 
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 604, UD_ncl_nsfcv);
/*
.....Process command.
*/
      if (status == NCL_OKINPUT)
     	{
			ncl_set_cmdmode(UU_TRUE);
      	ncl_add_cmdbuf(&cmdbuf);
      	ncl_call(&cmdbuf);
     	}
	}
done:;
   uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_nsf_revolv(ityp)
**       description
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create NCL surface of revolution (SURF)
**                 2 - Create NURB surface (NSURF)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_nsf_revolv(ityp)
int ityp;
{
   NCL_cmdbuf cmdbuf;
   int status, reltyp;

   uu_denter(UU_MTRC,(us,"nclu_sf_revolv"));

   while (UU_TRUE)
	{
start:;
/*
.....Intialize command buffer and status.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put NSURF/REVOLV into command.
*/
		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_nsf, NCL_nocomma);
		if (status == UU_SUCCESS)
			status = ncl_add_token(&cmdbuf, NCL_revolv, NCL_comma);
		if (status != UU_SUCCESS) goto done;
/*
.....Prompt user for curve to revolve.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 605, UD_ncl_nsfcv);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for a point defining axis of revolution.
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 606, UD_ncl_ptpvln, &reltyp);
/*
.....If it was a point that was selected, prompt user for vector defining
.....axis of revolution.
*/
		if (status == NCL_OKINPUT) 
		{
			if (reltyp == UM_LINE_REL || reltyp == NCL_LINE_REL)
				reltyp = NCL_POINTVEC_REL;
			if (reltyp != NCL_POINTVEC_REL)
				status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 607, UD_ncl_vepv);
		}
/*
.....If at this point status is not equal to NCL_OKINPUT, return to first prompt
*/
		if (status != NCL_OKINPUT) goto start;
/*
.....Prompt user for stating angle.
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_str(&cmdbuf, 608, NCL_comma);
/*
.....Prompt user for ending angle
*/
      if (status == NCL_OKINPUT)
         status = ncl_add_str(&cmdbuf, 609, NCL_nocomma);
      else
         status = NCL_OKINPUT;
/*
.....Process command.
*/
      if (status == NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
		}
	}
done:;
   uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_out()
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
int nclu_sf_out()
{
   NCL_cmdbuf cmdbuf;
   int status;

   while (UU_TRUE)
   {
/*
.....Initialize command buffer and status.
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Put SURF/OUT into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_out, NCL_comma);
/*
.....Prompt user for a trimmed surface to extract surface from.
*/	
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 528, UD_ncl_trimsf);

      if (status == NCL_OKINPUT)
      {
			ncl_set_cmdmode(UU_TRUE);
         ncl_add_cmdbuf(&cmdbuf);
         ncl_call(&cmdbuf);
      }
		else 
			goto done;
   }
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_trim()
**       User interface for surface trimming.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_sf_trim()
{
	NCL_cmdbuf cmdbuf;
	int status, rel_num = 0, n, len;
	char buf[256];

	do
	{
/*
.....Initialize command buffer and status.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....If auto label is off prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put SURF/REDEF into command (replace slash following REDEF with comma).
*/
		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_redef, NCL_nocomma);
		if (status == NCL_OKINPUT)
		{
			n = strlen(cmdbuf.cur_str);
			if (n > 1) cmdbuf.cur_str[n-1] = '\0';
			status = ncl_add_token(&cmdbuf, ",", NCL_nocomma);
		}
/*
.....Prompt user for a surface to trim, if not creating trimmed sfs from
.....from planar curves.
*/
		if (status == NCL_OKINPUT)
		{
			len = strlen(cmdbuf.cur_str);
			status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 530,
					UD_ncl_sftrim, &rel_num);
			if (status == NCL_NOINPUT)
			{
/*
.....User hit done, set status for exit.
*/
				status = NCL_DONE;
			}
			else
			{
				if (rel_num==UM_RBSPLSRF_REL  || rel_num==NCL_SURF_REL
				 || rel_num==NCL_MESHSURF_REL || rel_num==NCL_PLN_REL
				 || rel_num==NCL_EVALSF_REL   || rel_num==NCL_TRIMSF_REL
				 || rel_num==NCL_REVSURF_REL)
				{
/*
.....User picked a surface, add OUT and prompt for outer boundary.
.....If outer boundary not picked, remove OUT and continue for inner
.....boundaries.
*/
					len = strlen(cmdbuf.cur_str);
					if (status == NCL_OKINPUT)
						status = ncl_add_token(&cmdbuf, NCL_out, NCL_comma);
					if (status == NCL_OKINPUT)
						status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 531, UD_ncl_cvtrim);
					if (status == NCL_NOINPUT)
					{
						cmdbuf.cur_str[len] = '\0';
						status = NCL_OKINPUT;
					}
				}
				else
				{
/*
.....User picked a curve, add OUT in front of it.
*/
					strcpy(buf,&cmdbuf.cur_str[len]);
					cmdbuf.cur_str[len] = '\0';
					status = ncl_add_token(&cmdbuf, NCL_out, NCL_comma);
					status = ncl_add_token(&cmdbuf, buf, NCL_comma);
				}
			}
		}
/*
.....Save length of current command and insert IN.
*/
		if (status == NCL_OKINPUT)
		{
			len = strlen(cmdbuf.cur_str);
			status = ncl_add_token(&cmdbuf, NCL_in, NCL_comma);
		}
/*
.....Prompt for optional inner trimming curves.
*/
		n = 0;
		while (status == NCL_OKINPUT)
		{
			n++;
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 532, UD_ncl_cvtrim);
		}
/*
.....If only 1 curve was specified, remove the trailing IN.
*/
		if (n > 0)
		{
			status = NCL_OKINPUT;
			if (n == 1) cmdbuf.cur_str[len] = '\0';
		}

		if (status == NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	} while (status != NCL_DONE);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_trim_remove()
**       User interface for the 'REDEF/sf,remove,1,0,2' command.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_trim_remove()
{
	NCL_cmdbuf cmdbuf;
	int numint, click, status,i,subid,sh_mode,errfl=0;
	char sflab[256],str[10];
	UM_PLOCREC pick;
	struct NCL_trimsf_rec sf;
	UU_KEY_ID ckey1,ckey2;

/*
.....Initialize command buffer and status.
*/
		ncl_init_cmdbuf(&cmdbuf);
		click = 0;
		status = NCL_OKINPUT;
/*
.....If auto label is off prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_redef, NCL_nocomma);
		if (status == NCL_NOINPUT) return (0);
		sf.key = NULLKEY;
/*
.....Limit geometry selection to trim-surfaces.
*/
		ud_lgeo(UU_TRUE, UD_ncl_trimsf);

		do
		{
/*
.....Prompt user for trim-surface.
*/
			ua_dl_pldas(UD_DASPCKLOC, UA_NCL, 533, &pick, 1,
						   &numint, 1);
			if (numint < 1) break;
			if (pick.pent.key[1] == pick.pent.key[0]) 
			{
				ud_wrerr("CVonSF picked is not a boundary CV.");
				continue;
			}
/*
..... different surface - start new command
*/
			if (sf.key != pick.pent.key[0])
			{
				if (click == 1)
				{
					ud_wrerr("Different surface picked.");
					continue;
				}
				else
				{
		 			ncl_picktostr(&pick, sflab);
					sf.key = pick.pent.key[0];
					status = ncl_retrieve_data_fixed(&sf);
					if (status != UU_SUCCESS) break;
				}
			}
/*
..... Get boundary number.
*/
			subid = -1;
			sh_mode = ncl_shading_mode ();
			if (ncl_getdisp_shade(&sf)==0) sh_mode = 0;
			ncl_get_cvkey(&sf,pick.pent.key[1],&ckey1,&errfl,sh_mode);
			if (errfl != 0)
			{
				ud_wrerr("CVonSF not found.");
				continue;
			}
			ckey2 = pick.pent.key[1];
			if (ckey1 == sf.uv_key || ckey2 == sf.uv_key)
				subid = 0;
			else
			{
				for (i=0;i<sf.no_ibndykey && (subid==-1);i++)
				{
					if (ckey1 == sf.ibndykey[i] || ckey2 == sf.ibndykey[i])
						subid = (i+2)/2;
				}
			}
								
			if (subid >= 0)
			{
				if (click == 0)
				{
					click = 1;
					status = ncl_add_token(&cmdbuf, sflab, NCL_comma);
					if (status == NCL_OKINPUT )
					status = ncl_add_token(&cmdbuf, "REMOVE", NCL_comma);
				}
				sprintf(str, "%d",subid); 
				if (status == NCL_OKINPUT )
				status = ncl_add_token(&cmdbuf, str, NCL_comma);
			}

		} while (status == NCL_OKINPUT);
/*
.....Process command.
*/
		if (status == NCL_OKINPUT && click == 1)
		{
			ncl_set_cmdmode(UU_TRUE);
  			ncl_add_cmdbuf(&cmdbuf);
  			ncl_call(&cmdbuf);
		}

	return (0);
}

/*********************************************************************
*********************************************************************/
static void S_add_label (list,sbuf)
UU_LIST *list;
char *sbuf;
{
	char *buf;

	buf = (char *) uu_malloc((strlen(sbuf)+1)*sizeof(char));
	strcpy (buf,sbuf);	
	uu_list_push (list,&buf);
}

/*********************************************************************
*********************************************************************/
static void S_pop_last (list)
UU_LIST *list;
{
	char **labs;
	int n;

	n = list->cur_cnt - 1;
	if (n < 0) return;

	labs = (char **) UU_LIST_ARRAY(list);
	uu_free (labs[n]);
	list->cur_cnt = n;
}

/*********************************************************************
**    E_FUNCTION     : nclu_show_redef()
**       Create and process the current '*REDEF/sf' command.
**    PARAMETERS   
**       INPUT  : 
**          sfflg = 0  - no base
**          sfflg = 1  - just base surface/plane label
**          sfflg = 2  - base surface/plane plus a near point
**          outflg = 0  - no outer bound
**          outflg = 1  - outer bound label
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void nclu_show_redef (sfflg,sflab,outflg,outlab,ptlab,inlst)
int sfflg,outflg;
UU_LIST *inlst;
char *sflab,*outlab,*ptlab;
{
	NCL_cmdbuf cmdbf0;
	int status,i,n;
	char **labs;

	if (NCL_nokey > NULLKEY)
	{
		uc_delete (NCL_nokey); NCL_nokey = NULLKEY;
	}

	ncl_init_cmdbuf(&cmdbf0);
	ncl_add_token(&cmdbf0, "*", NCL_nocomma);
	status = ncl_add_token(&cmdbf0, NCL_redef, NCL_nocomma);

	if (sfflg > 0)	status = ncl_add_token(&cmdbf0, sflab, NCL_comma);

	if (status != NCL_OKINPUT) return;

	if (outflg > 0)
	{
		status = ncl_add_token(&cmdbf0, NCL_out, NCL_comma);
		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbf0, outlab, NCL_comma);
		if (sfflg == 2 && status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbf0, ptlab, NCL_comma);
	}
	if (status != NCL_OKINPUT) return;
	n = inlst->cur_cnt;
	if (n > 0)
	{
		status = ncl_add_token(&cmdbf0, NCL_in, NCL_comma);
			
		labs = (char **) UU_LIST_ARRAY(inlst);
		for (i = 0; i < n && status == NCL_OKINPUT; i++)
			status = ncl_add_token(&cmdbf0, labs[i], NCL_comma);
	}
	if (status == NCL_OKINPUT)
	{
/*
.....set this flag to store temp surface key in NCL_nokey to be deleted later
*/
		nclu_temp_redef = 1;
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbf0);
		ncl_call(&cmdbf0);
		nclu_temp_redef = 0;
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_sf_redef()
**       User interface for the 'REDEF/sf1,out,sf2,in,cv3' surface trimming.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_sf_redef()
{
	NCL_cmdbuf cmdbuf;
	int status,i,n;
	int rel_num = 0;
	char sflab[256],outlab[256],ptlab[256];
	char str[256];
	int cmdreject; 			/* UD_MARK stack checker */

	char **labs;

	UU_LOGICAL onpl;
	UU_KEY_ID skey;
	int sfflg,outflg;
	UU_LIST inlst;

	uu_list_init (&inlst,sizeof(char *),10,10);

	UD_MARK (cmdreject, UU_FALSE);	/* If rejected in loop, come back here. */
	if (cmdreject != 0) goto done;

	do
	{
/*
.....Initialize command buffer and status.
*/
		ncl_init_cmdbuf(&cmdbuf);

		NCL_noname = UU_TRUE;
		status = NCL_OKINPUT;
/*
.....If auto label is off prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_redef, NCL_nocomma);
		if (status == NCL_NOINPUT) break;
/*
.....Prompt user for a surface to trim, if not creating trimmed sfs from
.....from planar curves.
*/
start:
		onpl = UU_FALSE;
		sfflg = outflg = 0;
		NCL_skey0 = skey = NULLKEY;
		status = ncl_get_dlabel_rel(str,ptlab,UU_TRUE,530,UD_ncl_sftrim,&skey,
			&rel_num);
		if (status == NCL_NOINPUT) break;

		if (rel_num == UM_RBSPLSRF_REL || rel_num == NCL_SURF_REL ||
			rel_num == NCL_MESHSURF_REL || rel_num == NCL_PLN_REL ||
			rel_num == NCL_EVALSF_REL || rel_num == NCL_TRIMSF_REL ||
			rel_num == NCL_REVSURF_REL)
		{
/*
.....User picked a surface, add OUT and prompt for outer boundary.
.....If outer boundary not picked, remove OUT and continue for inner
.....boundaries.
*/
			if (rel_num == NCL_PLN_REL)
			{
				sfflg = 1;
				onpl = UU_TRUE;
			}
			else
			{
				sfflg = 2;
				NCL_skey0 = skey;
			}
			strcpy (sflab,str);
					
			status = ncl_add_token(&cmdbuf, str, NCL_comma);
		}
		else
		{
			outflg = 1;
			strcpy (outlab,str);
			nclu_show_redef (sfflg,sflab,outflg,outlab,ptlab,&inlst);
		}

pickout:
		if (outflg == 0)
		{
			status = ncl_get_dlabel1 (outlab,531,UD_ncl_rmill_geo);
				
			if (status == NCL_OKINPUT)
			{
				outflg = 1;
				nclu_show_redef (sfflg,sflab,outflg,outlab,ptlab,&inlst);
			}
			else if (status == NCL_ALTACTION)
			{
				if (NCL_skey0 > NULLKEY)
				{
					ncl_sea_ent_blank (2,NCL_skey0);
				}
				goto start;
			}
		}

		if (status == NCL_DONE) break;
		if (onpl && outflg == 0)
		{
			ud_wrerr("Must have an outer boundary.");
			continue;
		}

		do
		{
			status = ncl_get_dlabel1 (&str,532,UD_ncl_rmill_geo);
				
			if (status == NCL_OKINPUT)
			{
				S_add_label (&inlst,str);
				nclu_show_redef (sfflg,sflab,outflg,outlab,ptlab,&inlst);
			}
			else if (status == NCL_ALTACTION)
			{
				n = inlst.cur_cnt;
				if ( n >= 1)
				{
					S_pop_last (&inlst);
					nclu_show_redef (sfflg,sflab,outflg,outlab,ptlab,&inlst);
					status = NCL_OKINPUT;
				}
				else
				{
					if (NCL_skey0 > NULLKEY)
					{
						ncl_sea_ent_blank (2,NCL_skey0);
					}

					if (NCL_nokey > NULLKEY )
					{
						uc_delete (NCL_nokey); NCL_nokey = NULLKEY;
					}
					if (sfflg > 0)
					{
						outflg = 0;	goto pickout;
					}
					else
						goto start;
				}
			}

		} while (status == NCL_OKINPUT);

		if (status == NCL_DONE) break;
		status = NCL_OKINPUT;
		NCL_noname = UU_FALSE;
		if (NCL_nokey > NULLKEY)
		{
			uc_delete (NCL_nokey); NCL_nokey = NULLKEY;
		}

		if (outflg >= 1)
		{
			status = ncl_add_token(&cmdbuf, NCL_out, NCL_comma);
			if (status == NCL_OKINPUT)
				status = ncl_add_token(&cmdbuf,outlab,NCL_comma);
			if (sfflg == 2 && status == NCL_OKINPUT)
				status = ncl_add_token(&cmdbuf, ptlab, NCL_comma);
		}
		
		n = inlst.cur_cnt;
		if (n > 0)
		{
			status = ncl_add_token(&cmdbuf, NCL_in, NCL_comma);
			labs = (char **) UU_LIST_ARRAY(&inlst);
			for (i = 0; i < n && status == NCL_OKINPUT; i++)
			{
				status = ncl_add_token(&cmdbuf, labs[i], NCL_comma);
				uu_free (labs[i]);
			}
			inlst.cur_cnt = 0;
		}

		if (status == NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	} while (status != NCL_DONE);

done:
	NCL_noname = UU_FALSE;
	if (NCL_skey0 > NULLKEY)
	{
		ncl_sea_ent_blank (2,NCL_skey0); NCL_skey0 = NULLKEY;
	}

	n = inlst.cur_cnt;
	if (n > 0)
	{
		labs = (char **) UU_LIST_ARRAY(&inlst);
		for (i = 0; i < n; i++)	uu_free (labs[i]);
	}

	uu_list_free (&inlst);
	if (NCL_nokey > NULLKEY)
	{
		uc_delete (NCL_nokey); NCL_nokey = NULLKEY;
	}
	UD_UNMARK (cmdreject);		/* Clean up stack and jump. */

	return(UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION     : nclu_unshade_surf ()
**       Calls routines which set up light/shading conditions and shade 
**       selected surfaces
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_unshade_surf ()
{
	UU_LIST keys;
	UU_KEY_ID key, *key0;
	int init,len,rel_num;

	ud_lgeo(UU_TRUE, UD_ncl_allsfsh);  
  	ud_ldas(UD_DASSELECT, VIEWERROR, 4, NULL, 1, &len, UD_NODEFAULT);

	uu_list_init (&keys, sizeof (UU_KEY_ID), 20, 20);
	init = UU_TRUE;

	while ( ud_gnxt (init, UU_NULL, &key, 1) == UU_TRUE)
	{
		init = UU_FALSE;
		ur_retrieve_data_relnum (key, &rel_num);
		if (!uu_tst_bit(UD_ncl_allsfsh, rel_num-1)) continue;
		uu_list_push (&keys,&key);
	}
	key0 = (UU_KEY_ID *) UU_LIST_ARRAY(&keys);
	ncl_unshade_surf (key0, UU_LIST_LENGTH (&keys));
	return(UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION     : nclu_shade_surf ()
**       Calls routines which set up light/shading conditions and shade 
**       selected surfaces
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_shade_surf ()
{
	UU_LIST keys;
	UU_KEY_ID key, *key0;
	int init,len,rel_num;
/* 
... Pick surfaces to render, limit das to only pick surfaces 
.....Changed to UD_ncl_allsfsh, this is all surfaces and shapes.
.....JLS 9/30/99
*/
	ud_lgeo(UU_TRUE, UD_ncl_allsfsh);  
   	ud_ldas(UD_DASSELECT, VIEWERROR, 4, NULL, 1, &len, UD_NODEFAULT);

	uu_list_init (&keys, sizeof (UU_KEY_ID), 20, 20);
	init = UU_TRUE;

	while ( ud_gnxt (init, UU_NULL, &key, 1) == UU_TRUE)
	{
		init = UU_FALSE;
/*
... filter out invalid geometry
*/
		ur_retrieve_data_relnum (key, &rel_num);
		if (!uu_tst_bit(UD_ncl_allsfsh, rel_num-1)) continue;
		uu_list_push (&keys,&key);
	}
	key0 = (UU_KEY_ID *) UU_LIST_ARRAY(&keys);
	ncl_shade_surf (key0, UU_LIST_LENGTH (&keys));

	uu_list_free (&keys);
	return (UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION     : nclu_analyze_surf ()
**       Calls routines analyze surface primitive type 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_analyze_surf (option)
int option;
{
	NCL_cmdbuf cmdbuf;
	int cmdreject; 			/* UD_MARK stack checker */
	int status;
	int numint;
	UU_LOGICAL found,stat,lstatus;
	char str[256];
	struct UC_entitydatabag e1;

	uu_denter(UU_MTRC,(us,"nclu_analyze_surf"));

	UD_MARK (cmdreject, UU_FALSE);	/* If rejected in loop, come back here. */
	if (cmdreject != 0) goto done;

	while (UU_TRUE)
	{
/*
.....Initialize command buffer and status.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Put SURF/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_analyz, NCL_nocomma);
/*
.....If user doesn't want to use THRU, process here.
*/
		if (option ==0)
		{
/*
.....Limit geometry to surfaces allowed in a net surface
*/
			ud_lgeo(UU_TRUE, UD_ncl_netentity);
/*
.....Prompt user to pick the surfaces, the selection menu is brought up
.....here.
*/
			status = ud_ldas(UD_DASSELECT, UA_NCL, 499,UU_NULL, 
				NCL_MAXPICK, &numint, UD_NODEFAULT);

/*
.....If nothting was picked, exit
*/
			if (numint <=0) goto done;
	
/*
.....Build the ANALYZE commands
.....Borrowed from the REMOVE command interface
.....and ncl_get_select_labels
*/
			stat = UU_TRUE;
			lstatus = UU_TRUE;
			while (lstatus)
			{
				ncl_init_cmdbuf(&cmdbuf);
				status = ncl_add_token(&cmdbuf, NCL_analyz, NCL_nocomma);
				found = UU_FALSE;
				while (lstatus && cmdbuf.num_cmd < 3)
				{
					lstatus = ud_gnxt(stat,UU_NULL,&e1.key,1);
					stat = UU_FALSE;
					if (lstatus)
					{
						if ((ur_retrieve_data_fixed(&e1) == UU_SUCCESS))
						{
							if (ncl_legal_relation(e1.rel_num))
							{
								found = UU_TRUE;
								ncl_get_label(&e1,str);
								ncl_add_token(&cmdbuf,str,NCL_comma);
							}
						}
					}
				}
				if (found)
				{
					ncl_del_token(&cmdbuf,"",UU_TRUE);
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
			status = NCL_OKINPUT;
		}
/*
.....User wants to use THRU
*/
		else
		{
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 500, UD_ncl_netentity);
			if (status != NCL_OKINPUT) goto done;
			status = ncl_add_token(&cmdbuf,NCL_thru, NCL_comma);
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 501, UD_ncl_netentity);
/*
.....Process command.
*/
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
			}
		}
	}
done:;
	ul_close_window ();
	UD_UNMARK (cmdreject);		/* Clean up stack and jump. */
	uu_dexit;
	return(UU_SUCCESS);
}

