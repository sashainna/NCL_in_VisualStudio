
/*********************************************************************
**    NAME         :  numx.c
**       CONTAINS: User interface routines for matrix creation.
**			nclu_mx_pt_ve_ve()
**			nclu_mx_invers()
**			nclu_mx_mx_mx()
**			nclu_mx_mirror()
**			nclu_mx_scale()
**			nclu_mx_rotate()
**			nclu_mx_transl()
**			nclu_mx_params()
**			nclu_mx_coordsys()
**			nclu_load_matrix_list()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       numx.c , 25.1
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
#include "udforms.h"

#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION     : nclu_mx_pt_ve_ve()
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
nclu_mx_pt_ve_ve()

{
	NCL_cmdbuf cmdbuf;
	int status, reltyp;

	uu_denter(UU_MTRC,(us,"nclu_mx_pt_ve_ve"));

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
.....Put MATRIX/ into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
/*
.....Prompt user for point or point-vector defining the origin.
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 153,
            UD_ncl_ptpv, &reltyp);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....If it was a point that was selected, prompt user for a vector or 
.....point vector, this represents the positive x-axis.
.....If it was a point-vector that was choosen, then the point is the
.....origin of the secondary coordinate system and the vector portion
.....represents the positive x-axis.
*/
		if (reltyp != NCL_POINTVEC_REL) 
	     	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 154, UD_ncl_vepv);
/*
.....Get the second vector or point - vector, this will represent the
.....positive y-axis.
*/
		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 155, UD_ncl_vepv);
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
}

/*********************************************************************
**    E_FUNCTION     : nclu_mx_invers()
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
nclu_mx_invers()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_mx_invers"));

	while (UU_TRUE)
	{
/*
.....Initialize the command buffer.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....If auto label is off then prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put MATRIX/INVERS, into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_invers, NCL_comma);
/*
.....Prompt user for a label to invert.
*/
		status = ncl_add_str(&cmdbuf, 150, NCL_nocomma);
/*
.....Process command if a matrix was choosen, if not, exit.
*/

		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
		else
			goto done;
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_mx_mx_mx()
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
nclu_mx_mx_mx()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_mx_mx_mx"));

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
.....Put MATRIX/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
/*
.....Prompt user for the first matrix to multiply.
*/
		status = ncl_add_str(&cmdbuf, 151, NCL_comma);
/*
.....If done was selected, exit
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for second matrix to multiply.
*/
		status = ncl_add_str(&cmdbuf, 152, NCL_comma);
/*
.....If all is well, process command, if not go back to first prompt.
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
**    E_FUNCTION     : nclu_mx_mirror()
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
nclu_mx_mirror()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_mx_mirror"));

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
.....Put MATRIX/MIRROR into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_mirror1, NCL_comma);
/*
.....Prompt user for a plane to define mirror transformation.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 156, UD_ncl_pl);
/*
.....If user selected a plane, process command, if not exit.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
		else
			goto done;
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_mx_scale()
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
nclu_mx_scale()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_mx_scale"));

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
.....Put MATRIX/SCALE into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_scale1, NCL_comma);
/*
.....Prompt user for point or point-vector defining the origin.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 153, UD_ncl_ptpv);
/*
.....Prompt user for scale factor.
*/
		status = ncl_add_str(&cmdbuf, 157, NCL_nocomma);
/*
.....If scalar was entered, process command, else exit.
*/

		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
		else
			goto done;
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_mx_rotate()
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
nclu_mx_rotate()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_mx_rotate"));

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
.....Put MATRIX/ into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
/*
.....Prompt user to select rotation modifier
*/
		status = ncl_add_modifier(&cmdbuf, NCL_ROTATION_AXIS);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for angle of rotation.
*/
		status = ncl_add_angle(&cmdbuf, 158);
/*
.....If angle was entered, then process command, otherwise, return to
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
}

/*********************************************************************
**    E_FUNCTION     : nclu_mx_transl()
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
nclu_mx_transl()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_mx_transl"));

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
.....Put MATRIX/TRANSL, into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_transl, NCL_comma);
/*
.....Prompt user for X,Y,Z values to translate by.
*/
		status = ncl_add_vector(&cmdbuf, 159);
/*
.....If user selected a vector, process command, if not, exit
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
		else
			goto done;
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_mx_params()
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
nclu_mx_params()

{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_mx_params"));

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
.....Put MATRIX/ into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
/*
.....Prompt user for the 12 elements thst define the matrix.
*/
		status = ncl_add_str(&cmdbuf, 149, NCL_nocomma);
/*
.....if the elements were entered, process command, if not 
.....exit.
*/
		if ((status == NCL_OKINPUT))
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
		else
			goto done;
	}
done:;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_mx_coordsys()
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
nclu_mx_coordsys()

{
	NCL_cmdbuf cmdbuf;
	int status, reltyp;

	uu_denter(UU_MTRC,(us,"nclu_mx_coordsys"));

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
.....Put MATRIX/ into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
/*
.....Prompt user to Enter 1st input coord
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 183,
            UD_ncl_ptpv, &reltyp);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user to Enter 2st input coord
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 184,
            UD_ncl_ptpv, &reltyp);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user to Enter 3st input coord
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 185,
            UD_ncl_ptpv, &reltyp);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user to Enter 1st output coord
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 186,
            UD_ncl_ptpv, &reltyp);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user to Enter 2st output coord
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 187,
            UD_ncl_ptpv, &reltyp);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user to Enter 3st output coord
*/
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 188,
            UD_ncl_ptpv, &reltyp);
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
}

/*********************************************************************
**    E_FUNCTION     : nclu_load_matrix_list(list,default)
**       Loads a list of defined Macros into a UD_LIST for the form
**       CHOICE_LIST field.
**    PARAMETERS   
**       INPUT  : 
**          default   = Default selection from list.
**       OUTPUT :  
**          list      = Das list for form CHOICE_LIST field.
**    RETURNS      :
**          Number of items in list or -1 if there was a problem.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_load_matrix_list(dlist,sdef)
UD_LIST *dlist;
char *sdef;
{
	int nmx,i,status,inc;
	char *tptr,*tptrf;
/*
.....Initialize routine
*/
	nmx = 0;
	tptr = (char *)uu_lsnew();
	tptrf = tptr;;
	if (tptr == UU_NULL) goto failed;
	dlist->item = UU_NULL;
	dlist->answer = UU_NULL;
	dlist->num_item = 0;
/*
.....Get list of defined matrixes
*/
	status = ncl_entnam_list(10,tptr,&nmx);
/*
.....Copy list into DAS list
*/
	dlist->item = (char **)uu_malloc((nmx+1)*sizeof(char *));
	if (dlist->item == UU_NULL) goto failed;
	dlist->answer = (char *)uu_malloc(sizeof(char)*NCL_MAX_LABEL_AND_SUBSCRIPT);
	if (dlist->answer == UU_NULL) goto failed;
	strcpy(dlist->answer,sdef);
/*
.....Transfer matrix names to list
*/
	dlist->item[0] =
		(char *)uu_malloc(sizeof(char)*NCL_MAX_LABEL_AND_SUBSCRIPT);
	if (dlist->item[0] == UU_NULL) goto failed;
	strcpy(dlist->item[0]," ");
	inc = 0;
	for (i=0;i<nmx;i++)
	{
		tptr = (char *)uu_lsnext(tptr);
		if (tptr[0] != '@')
		{
			inc++;
			dlist->item[inc] =
				(char *)uu_malloc(sizeof(char)*NCL_MAX_LABEL_AND_SUBSCRIPT);
			if (dlist->item[inc] == UU_NULL) goto failed;
			strcpy(dlist->item[inc],tptr);
		}
	}
	dlist->num_item = inc + 1;
	goto done;
/*
.....Failed to load matrix list
*/
failed:;
	nmx = -1;
/*
.....End of routine
*/
done:;
	if (tptrf != UU_NULL) uu_lsdel(tptrf);
	return(nmx);
}
