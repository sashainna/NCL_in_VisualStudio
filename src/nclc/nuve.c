/*********************************************************************
**    NAME         :  nuve.c
**       CONTAINS: User interface routines for vector creation.
**			nclu_ve_ijk()
**			nclu_ve_fwd()
**			nclu_ve_tlaxis()
**			nclu_ve_pt_pt()
**			nclu_ve_perpto_pl()
**			nclu_ve_unit_ve()
**			nclu_ve_ve_ve()
**			nclu_ve_ve_cross_ve()
**			nclu_ve_ve_times()
**			nclu_ve_intof_pl_pl()
**			nclu_ve_pt_sf()
**       nclu_ve_tt_cv()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuve.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:17
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

#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION     : nclu_ve_ijk()
**       description:  This routine creates vectors from the
**                     i,j,k values the user inputs.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_ijk()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_ijk"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

/*
.....Check to see if auto name is on, and if not get the label.
*/
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
/*
.....This line puts the word VECTOR/ at the begining of the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);
/*
.....Get the vector values from the user.
*/
		status = ncl_add_vector(&cmdbuf, 119); 
/*
.....Process the command if input is okay.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
/*
.....Want to exit if done is selected JLS
*/

		if(status==NCL_NOINPUT) goto done;

	}

done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_fwd()
**       description:  Create a vector in the direction of the last
**                     cutter motion.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_fwd()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_fwd"));

/*
.....Initialize the command buffer.
*/
   ncl_init_cmdbuf(&cmdbuf);
   status = NCL_OKINPUT;
/*
.....Check to see if auto label in on and if not add get name.
*/
	if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);

/*
.....Put VECTOR/ at the begining of the command.
*/
	status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);
/*
.....Put FWD next in the command.
*/
	status = ncl_add_token(&cmdbuf, NCL_fwd, NCL_nocomma);
/*
.....Process the command.
*/
	if ((status == NCL_OKINPUT))
	{
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_tlaxis()
**       description:  Creates a vector in the direction of the
**                     tool axis.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_tlaxis()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_tlaxis"));
/*
.....Initialize the command buffer.
*/

	ncl_init_cmdbuf(&cmdbuf);
	status = NCL_OKINPUT;

/*
.....Check to see if auto label is on.
*/
	if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
/*
.....Put VECTOR/ at the begining of the command
*/
	status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);
/*
.....Add TLAXIS to the command line.
*/
   status = ncl_add_token(&cmdbuf, NCL_ve_tlaxis, NCL_nocomma);
	if (status !=NCL_OKINPUT) goto done;

/*
.....Process command.
*/

	if ((status == NCL_OKINPUT))
	{
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}

done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_pt_pt()
**       description:  Create a vector in the direction of one
**                     point to another point.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_pt_pt()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_pt_pt"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

/*
.....Check to see if auto label is on and if not get the label from
.....the user.
*/
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
/*
.....Put VECTOR/ in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);

/*
.....Get the first point.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 122, UD_ncl_ptpv);
/*
.....If the done button was hit, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
......Get the second point.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 123, UD_ncl_ptpv);
/*
.....If the two points were selected, process them, if done was selected 
.....instead of a second point, it will go back and ask for the first point
.....again. 
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
}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_perpto_pl()
**       description:  Creates a vector that is perpendicular 
**                     to a plane.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_perpto_pl()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_perpto_pl"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

/*
.....Check to see if auto_label is on
*/
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);

/*
.....Put VECTOR/ in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);

/*
.....Put PERPTO and a comma in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_perpto, NCL_comma);

/*
.....Get the plane to be perpendicular to.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 124, UD_ncl_pl);
/*
.....If the user selected done, exit.
*/
		if(status != NCL_OKINPUT) goto done;
/*
.....Bring up the pop-up menu to ask for the direction modifier.
*/
      status = ncl_add_modifier(&cmdbuf, NCL_XYZ_DIRECTION);

/*
.....Process the command.
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
}

/******************************************************************
**    E_FUNCTION     : nclu_ve_unit_ve()
**       description:  Create a unit vector out of either
**                     a vector or a point vector.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_unit_ve()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_unit_ve"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on and if not get the label from
.....the user.
*/

		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put VECTOR/ into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);
/*
....Put UNIT and a comma into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_unit, NCL_comma);
/*
.....Get the vector or pointvector to create the unit vector from.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 125, UD_ncl_vepv);
/*
.....If no vector was selected exit.
*/
		if (status != NCL_OKINPUT) goto done;

/*
.....Process the command.
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
}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_ve_ve()
**       description:  Create a vector by either adding or subtracting
**                     two vectors or two point-vectors.  The user may 
**                     select a point-vector and a vector, two vectors,
**                     or two point-vectors.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_ve_ve(add_sub)
/*
.....Originally nothing was passed in and the user would select to
.....add or subtract the vectors later through a pop-up menu, now
.....the user would have already selected either the PLUS button
.....or the MINUS button on the vector menu.  If 0 is passed in then
.....subtract and if 1 is passed in add.  JLS 2/10/99
*/
int add_sub;

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_ve_ve"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

/*
.....Check to see if auto label is on.
*/
		if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
/*
.....Put VECTOR/ in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);

/*
.....Get the first vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 126, UD_ncl_vepv);
/*
.....If done was hit, exit.
*/
		if (status !=NCL_OKINPUT) goto done;
/*
.....If add_sub is equal to 0 put MINUS and a comma in the command.
.....If add_sub is equal to 1 put PLUS and a comma in the command.
.....This portion changed to reflect the new menu selections. JLS 2/10/99
*/
		if(add_sub==0)
			status = ncl_add_token(&cmdbuf, NCL_minus, NCL_comma);
		else
			status = ncl_add_token(&cmdbuf, NCL_plus, NCL_comma);
/*
.....Get the second vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 127, UD_ncl_vepv);
/*
.....Process the command if the second vector was selected, if not go
.....back and ask for the first vector again.
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
}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_ve_cross_ve()
**       description:  Creates a vector that is the cross product of
**                     two vectors or two point-vectors.  The user
**                     may select any combination of vectors
**                     and point-vectors.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_ve_cross_ve()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_ve_cross_ve"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

/*
.....Check to see if auto label is on and if not get the label from the user.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

/*
.....Put VECTOR/ in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);

/*
.....Get the first vector from the user.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 126, UD_ncl_vepv);
/*
.....If user hit done, exit.
*/
		if (status != NCL_OKINPUT) goto done;

/*
.....add CROSS and a comma to the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_cross, NCL_comma);
/*
.....Get the second vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 127, UD_ncl_vepv);
/*
.....If the user selected a second vector, process the command, if not
.....go back and start the process over.
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
}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_ve_times()
**       description : Create a scaled vector from either a vector or
**                     a point-vector.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_ve_times()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_ve_times"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

/*
.....Check to see if auto label is on and if not prompt user for
.....label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

/*
.....Put VECTOR/ in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);
/*
.....Get the vector to be scaled.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 218, UD_ncl_vepv);
/*
.....If done was hit, exit.
*/
		if (status!=NCL_OKINPUT) goto done;

/*
....Put TIMES and a comma in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_times, NCL_comma);
/*
.....Get the value to scale by.
*/
      status = ncl_add_length(&cmdbuf, 201);
/*
.....If the user entered a scalar value, process the command, if not go
.....back and begin the process again.
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
}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_intof_pl_pl()
**       description : Create a unit vector from the intersection
**                     of two planes in the direction of the modifier.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_intof_pl_pl()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_intof_pl_pl"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

/*
.....Check to see if auto label is on and if not, prompt the user for
.....a label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put VECTOR/PARLEL,INTOF, in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_parlel, NCL_comma);
		status = ncl_add_token(&cmdbuf, NCL_intof, NCL_comma);
/*
.....Get the first plane from the user.
*/

		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 128, UD_ncl_pl);
/*
.....If done was hit, exit.
*/
		if(status!=NCL_OKINPUT) goto done;
/*
....Get the second plane from the user.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 129, UD_ncl_pl);

/*
.....If the second plane was selected bring up the pop_up
.....menu for the user to select the direction of the vector.
*/
		if (status==NCL_OKINPUT)
			status = ncl_add_modifier(&cmdbuf, NCL_XYZ_DIRECTION);

/*
.....If the user selected a modifier, process the command, if not
.....go back and prompt the user for the first plane.
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
}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_pt_sf()
**       description : Create a unit vector in the direction from a point
**                     or point-vector to a surface. 
**            
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_pt_sf()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_pt_sf"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on and if not prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put VECTOR/ in command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);
/*
.....Get the point or point-vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 120, UD_ncl_ptpv);
/*
.....If done was selected, exit.
*/
		if(status!=NCL_OKINPUT) goto done;
/*
.....Get the surface.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 121, UD_ncl_allsf);
/*
.....If the user selected a surface, process the command, if not start over.
*/
		if (status==NCL_OKINPUT)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	} 

done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_tt_cv()
**       Builds a statement to define a CAM VECTOR as tangent to a 
**       curve at a point on the curve.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_tt_cv()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_tt_cv"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on and if not prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put VECTOR/TANTO, in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_tanto, NCL_comma);
/*
.....Get the curve.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 550, UD_ncl_allcv);
/*
.....If done was selected, exit.
*/
		if (status!=NCL_OKINPUT) goto done;
/*
.....Get a point or point-vector near to where the the vector should
.....be tangent.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 551, UD_ncl_ptpv);
/*
.....If the user selected appropriately, process the command, if not
.....go back and prompt for a curve again.
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
}

/*********************************************************************
**    E_FUNCTION     : nclu_ve_pv()
**       description   Create a vector in the same direction and with
**                     the same magnitude as a point-vector.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_ve_pv()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_ve_pt_pt"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....Check to see if auto label is on and if not prompt user for label.
*/

		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put VECTOR/ in the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_ve, NCL_nocomma);
/*
.....Get the point-vector.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 240, UD_ncl_pv);
/*
.....If done was selected, exit, otherwise process the command.
*/
		if (status!=NCL_OKINPUT) goto done;

		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}

done:;
	uu_dexit;
}

