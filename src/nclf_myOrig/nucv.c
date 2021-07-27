/*********************************************************************
**    NAME         :  nucv.c
**       CONTAINS: User interface routines for curve creation.
**
**			nclu_cv_pt_ve
**			nclu_cv_fit_pt_ve
**			nclu_cv_pt_thru_pt
**			nclu_cv_fit_pt_thru_pt
**			nclu_cv_pt
**			nclu_cv_offset
**			nclu_cv_offset1
**			nclu_cv_sf_edge
**			nclu_cv_io_sf_sf
**			nclu_cv_conic
**			nclu_cv_composite
**			nclu_cv_out
**       nclu_cv_chain_out
**			nclu_comp_cv_out
**			nclu_cv_revsf
**       nclu_compcv_contype
**
**     MODULE NAME AND RELEASE LEVEL
**       nucv.c , 25.5
**     DATE AND TIME OF LAST MODIFICATION
**       02/28/17 , 15:47:40
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdpick.h"
#include "modef.h"
#include "mcrv.h"
#include "mfort.h"

#include "mdeval.h"
#include "mgeom.h"

#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclfc.h"
#include "mdgenent.h"
#include "ncl.h"
#include "vsegbf.h"

#include "udforms.h"

#define SINGLE 1
#define ALL 2
#define THRU 3

void ncl_get_cvkey();

static int Scontype = 0;
static int Sclose = 1;

/*********************************************************************
**    E_FUNCTION     : nclu_cv_pt_ve(ityp)
**       Interface for creating a curve/spline that interpolates
**       some points and slopes
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric curve (CURVE)
**				   2 - Create B-spline curve (SPLINE)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_pt_ve(ityp)
int ityp;
{
	NCL_cmdbuf cmdbuf;
	int status,reltyp;
	int flag;

	uu_denter(UU_MTRC,(us,"nclu_cv_pt_ve"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer and status.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
		flag=0;
/*
.....If auto label is off, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....If it is a parametric curve, put CURVE/ into command.
*/
		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
/*
.....If it is a bspline, but SPLINE/ into command.
*/
		else
			status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
		

		while (status == NCL_OKINPUT) 
		{
/*
.....Prompt user for point or point-vector.
*/
 			status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 489,
            UD_ncl_ptpv, &reltyp);
/*
.....If the user has yet to select any points or point-vectors
.....exit the routine, if the user did select 1 point or point-vector
.....but that is it, return to first prompt.  If at least two
.....points or point-vectors have been selected, set status to
.....NCL_OKINPUT and break to go process command.
*/
			if (status != NCL_OKINPUT) 
			{
				if (flag==0)
					goto done;
				else
				{
					if (flag>1)
						status = NCL_OKINPUT;
					break;
				}
			}
			flag ++;
/*
.....If the it was a point selected, prompt user for a vector, a 
.....vector is not necessary so if user doesn't select a vector,
.....still set status to NCL_OKINPUT.
*/
			if (reltyp == NCL_POINT_REL || reltyp == UM_POINT_REL)
			{
			  	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 161, UD_ncl_vepv);   
				status = NCL_OKINPUT;
			}
				
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
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_fit_pt_ve(ityp)
**       Interface for creating a curve/spline that fits
**       some points and slopes
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric curve (CURVE)
**				   2 - Create B-spline curve (SPLINE)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_fit_pt_ve(ityp)
int ityp;

{
	NCL_cmdbuf cmdbuf;
	int status,reltyp;
	int flag;

	uu_denter(UU_MTRC,(us,"nclu_cv_fit_pt_ve"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer, status, and flag;
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
		flag = 0;
/*
.....If auto_label is off, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Depending on ityp, put either CURVE/ or SPLINE/ into command.
*/
		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
/*
.....Put FIT into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_fit, NCL_nocomma);

		while (status == NCL_OKINPUT) 
		{
/*
.....prompt user for point or point vector.
*/
 			status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 489,
            UD_ncl_ptpv, &reltyp);
/*
.....If done is selected and it is the first time here, exit, if it is the second
.....time here go back to the first prompt, and if done was selected and at least
.....two points or point-vectors have already been selected, process command.
*/
			if (status == NCL_NOINPUT) 
			{
				if (flag ==0) 
					goto done;
				else
				{
					if (flag>1)
						status = NCL_OKINPUT;
					break;
				}
			}
			flag++;
/*
.....Prompt for the optional vector if the previous pick was
.....a point.  This is optional, so if done was selected, still
.....keep status as NCL_OKINPUT.
*/

			if (reltyp == NCL_POINT_REL || reltyp == UM_POINT_REL)
			{
			  	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 161, UD_ncl_vepv);   
				status = NCL_OKINPUT;
			}
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
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_pt_thru_pt(ityp)
**       Interface for creating a curve/spline that interpolates
**       a sequence point through point
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric curve (CURVE)
**				   2 - Create B-spline curve (SPLINE)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_pt_thru_pt(ityp)
int ityp;
{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_cv_pt_thru_pt"));

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
.....If it is a parametric curve, put CURVE/ into the command, else 
.....it is a bspline curve, so put SPLINE/ into command.
*/
		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
/*
.....Prompt user for first point.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 489, UD_ncl_ptpv);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Put THRU into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_thru, NCL_comma);
/*
.....Prompt user for the second point.
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 489, UD_ncl_ptpv);
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
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_fit_pt_thru_pt(cvflg,ityp)
**       Interface for creating a curve/spline that fits
**       a sequence point through point
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric curve (CURVE)
**				   2 - Create B-spline curve (SPLINE)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_fit_pt_thru_pt(cvflg,ityp)
int cvflg,ityp;
{
	NCL_cmdbuf cmdbuf;
	int status;
	int flag;

	uu_denter(UU_MTRC,(us,"nclu_cv_pt_thru_pt"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer and status.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
		flag = 0;
/*
......If auto label is off, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....If it is a parametric curve put CURVE/FIT into command, otherwise,
.....put SPLINE/FIT into command.
*/
		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);

	  	status = ncl_add_token(&cmdbuf, NCL_fit, NCL_nocomma);
/*
.....Prompt user for a point
*/
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 489, UD_ncl_ptpv);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;

/*
.....If cvflg is equal to one, then this is a thru command.
.....so get the ending point of point - vector.
*/
		if (cvflg == 1)
		{
			status = ncl_add_token(&cmdbuf, NCL_thru, NCL_comma);
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 489, UD_ncl_ptpv);
		}
/*
.....The user wishes to continue selecting points or point -vectors to
.....create the curve, if at least two points or point - vectors have been
.....selected when done is hit, then process command.
*/ 
		if (cvflg == 2) 
		{
			while (status == NCL_OKINPUT)
			{
				status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 489, UD_ncl_ptpv);
				flag ++;
			}
			if (status != NCL_OKINPUT&&(flag>1))
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
			}
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
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_pt(ityp)
**       Interface for creating a curve/spline that interpolates
**       some points
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric curve (CURVE)
**				   2 - Create B-spline curve (SPLINE)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_pt(ityp)
int ityp;
{
	NCL_cmdbuf cmdbuf;
	int status;
	int flag;

	uu_denter(UU_MTRC,(us,"nclu_cv_pt"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer, status, and flag.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
		flag = 0;
/*
.....Check to see if auto_label is on, if not, prompt user for command
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put either CURVE/ or SPLINE/ into the command.
*/
		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
/*
.....Prompt user for points.
*/
		while (status == NCL_OKINPUT)
		{
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 489, UD_ncl_ptpv);
			if (status != NCL_OKINPUT) 
			{
/*
.....If done is selected the first time, exit.  If it is selected after
.....only one point has been chosen, return to first prompt, and if at
.....least two points have been selected, process command.
*/
				if (flag==0)
					goto done;
				else
				{
					if (flag>1)
						status = NCL_OKINPUT;
					break;
				}
			}
			flag++;
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
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_offset(ityp)
**       Interface for translating a curve/spline by a vector.
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric curve (CURVE)
**                 2 - Create B-spline curve (SPLINE)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_offset(ityp)
int ityp;

{
	NCL_cmdbuf cmdbuf;
	int status,save_loc;
	char str[256];
	int markval=0;

	uu_denter(UU_MTRC,(us,"nclu_cv_offset"));


	save_loc = UD_locint;
	UD_locint = UD_STRING;

	UD_MARK(markval,UU_FALSE);
	if (markval != 0)
	{
		UD_locint = save_loc;
		UD_UNMARK (markval);
		return (0);
	}
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
.....Put either CURVE/ or SPLINE/ into the command.
*/
		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
/*
.....Prompt user to select curve.
*/
		status = ncl_get_dlabel(UD_DASPCKLOC, &str, 579, UD_ncl_offcv);
		ncl_add_token(&cmdbuf,str,NCL_comma);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for offset vector.
*/
		while (status == NCL_OKINPUT)
		{
/*
.....Don't use vector because it does not allow (0,0,0)
*/
/*			status = ncl_add_vector(&cmdbuf, 48, 3); */
			status = ncl_add_coord(&cmdbuf, 48, 3); 
/*
.....If a vector was supplied, process command.
*/
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
     			ncl_add_cmdbuf(&cmdbuf);
     			ncl_call(&cmdbuf);
/*
.....Initialize the command buffer, prompt user for
.....a label if needed and put CV/cv, (or SP/cv,) into the buffer.
.....Then go up and ask for another offset vector.
*/
				ncl_init_cmdbuf(&cmdbuf);
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf, 1);
				if (ityp == 1)
					status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
				else
					status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
				ncl_add_token(&cmdbuf,str,NCL_comma);
			}
		}
	}
done:;
	UD_locint = save_loc;
	UD_UNMARK (markval);
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_offset1(ityp)
**       Interface for offsetting a curve/spline by a vector.
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric curve (CURVE)
**				       2 - Create B-spline curve (SPLINE)
**				       3 - Create S-spline curve (SSPLIN)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_offset1(ityp)
int ityp;
{
	NCL_cmdbuf cmdbuf;
	int numint, status;
	char str[256];
	UM_PLOCREC pick;
	UU_KEY_ID  key;
	UM_coord picked, npt;
	UM_vector vc,nve,ve0;
	UM_vector vpnorm;
	struct NCL_fixed_databag crv;
	UM_transf tfmat, *tf;
	struct UM_evcrvout evout;
	UU_REAL adrv,bdrv,anorm,bnorm,LR, uu = 0.;

	uu_denter(UU_MTRC,(us,"nclu_cv_offset1"));


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
		if (status == NCL_DONE) goto done;
/*
.....Put either CURVE/OFFSET or SPLINE/OFFSET into the command.
*/
		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
		else if (ityp == 2)
			status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_ssplin, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_offset, NCL_comma);
/*
.....Prompt user to select curve.
*/
		if (ityp == 3)
			ud_lgeo(UU_TRUE, UD_ncl_offss);
		else
			ud_lgeo(UU_TRUE, UD_ncl_offcv);
		ua_dl_pldas(UD_DASPCKLOC,UA_NCL,436,&pick,1,&numint,1);
		if (numint <= 0) goto done;
		status = ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
		if (status != NCL_OKINPUT) goto done;
		strcpy (str,pick.ploc.label);
		strcat (str,",");
/*
..... Add the direction modifier (XL,YS,...), as determined by the pick
*/
		LR = 1.;
		key = um_get_pickkey(&pick.pent, 1);
		crv.key = key;

		status = ncl_retrieve_data_fixed (&crv);
		if(status != UU_SUCCESS) continue;
		tf = &tfmat;
		status = ncl_trimsrf_get_tf (&crv,&tf);
		if (status != UU_SUCCESS) continue;
		uc_init_evcrvout (&crv, &evout);
		status = uc_evcrv(UM_FRSTDERIV, 0., &crv, tfmat, &evout);
		if (status != UU_SUCCESS) continue;
			
		um_ploctocc(&pick.ploc,picked);
		status = um_near_on_curve1(&crv,tfmat, &(pick.ploc), &uu,npt,nve);
		if(status != UU_SUCCESS) continue;
		um_vpnorm(pick.ploc.transform, vpnorm);
		ncl_proj_to_wp (picked, vpnorm, picked);
		ncl_proj_to_wp (evout.dcdu, vpnorm, ve0);
		ncl_proj_to_wp (npt, vpnorm, npt);
		ncl_proj_to_wp (nve, vpnorm, nve);

		adrv = nve[0]; bdrv = nve[1];
		if (adrv*adrv + bdrv*bdrv < UM_FUZZ)
		{
			ud_wrerr("Direction modifier cannot be determined.");
			continue;
		}
		anorm = - bdrv; bnorm = adrv;
		um_vcmnvc(picked, npt, vc);
		if (anorm*vc[0] + bnorm*vc[1] < 0) 
			LR = -1.;

		adrv = ve0[0]; bdrv = ve0[1];
		if (adrv*adrv + bdrv*bdrv < UM_FUZZ)
		{
			ud_wrerr("Direction modifier cannot be determined.");
			continue;
		}
		vc[0] = - LR*bdrv; 
		vc[1] = LR*adrv;

		if (fabs(vc[0]) >= fabs(vc[1]))
		{
			if (vc[0] >= 0.0)
			{
				status=ncl_add_token(&cmdbuf,NCL_xlarge,NCL_comma);
				strcat (str,NCL_xlarge);
			}
			else
			{
				status=ncl_add_token(&cmdbuf,NCL_xsmall,NCL_comma);
				strcat (str,NCL_xsmall);
			}
		}
		else
		{
			if (vc[1] >= 0.0)
			{
				status=ncl_add_token(&cmdbuf,NCL_ylarge,NCL_comma);
				strcat (str,NCL_ylarge);
			}
			else
			{
				status=ncl_add_token(&cmdbuf,NCL_ysmall,NCL_comma);
				strcat (str,NCL_ysmall);
			}
		}
		strcat (str,",");
		

		while(status == NCL_OKINPUT)
		{
			status = ncl_add_str(&cmdbuf, 434, NCL_nocomma);
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);

				ncl_init_cmdbuf(&cmdbuf);
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf, 1);
				if (ityp == 1)
					status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
				else if (ityp == 2)
					status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
				else if (ityp == 3)
					status = ncl_add_token(&cmdbuf, NCL_ssplin, NCL_nocomma);
				status = ncl_add_token(&cmdbuf, NCL_offset, NCL_comma);
				ncl_add_token(&cmdbuf,str,NCL_comma);
			}
			else
				break;
		}
	}
done:;
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_sf_edge()
**       Interface for creating a curve/sspline/bspline as a component
**       of a surface edge.
**    PARAMETERS   
**       INPUT  : 
**          type :  0 - curve
**                  1 - s-spline 
**                  2 - b-spline 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_sf_edge(type)
int type;
{
	NCL_cmdbuf cmdbuf;
	int status;
	int numint,lparam;
	int edge_number;
	char str[256],sbuf[20];

	uu_denter(UU_MTRC,(us,"nclu_cv_sf_edge"));

/*
.....Loop to define command
*/
	while (UU_TRUE)
	{
		status = NCL_OKINPUT;
/*
........Select surface.
*/
		status = ncl_get_dlabel(UD_DASPCKLOC, &str, 222, UD_ncl_uvsf);
		if (status != NCL_OKINPUT) break;
/*
........Get surface edge.
*/
		while (UU_TRUE)
		{
	  		status = ncl_popup(NCL_EDGE, &edge_number);
      	if (status != NCL_OKINPUT || edge_number == 0) break;
			while (UU_TRUE)
			{
/*
........Get Percent or Param qualifier
*/
				status = ncl_popup(NCL_PERCNT, &lparam);
				if (status != NCL_OKINPUT || lparam == 0) break;
/*
........Loop to get last parameter (length)
........and create command
*/
				while (status == NCL_OKINPUT)
				{
/*
...........Initialize command
*/
					ncl_init_cmdbuf(&cmdbuf);
/*
...........If auto label is off, prompt user for label.
*/
					if (!NCL_auto_label) status = ncl_add_name(&cmdbuf, 1);
					if (type == 0) 
						ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
					else if (type == 1)
						ncl_add_token(&cmdbuf, NCL_ssplin, NCL_nocomma);
					else 
						ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
/*
...........Put the surface into the command buffer.
*/
					ncl_add_token(&cmdbuf,str,NCL_comma);
/*
...........Put edge into command.
*/
					sprintf(sbuf,"%d",edge_number);
					status = ncl_add_token(&cmdbuf,sbuf,NCL_comma);
/*
...........Get location of curve, this is a number between 0 and 1
*/
					if (lparam == 2)
					{
						ncl_add_token(&cmdbuf,NCL_percnt,NCL_comma);
						status = ncl_add_length(&cmdbuf, 502);
					}
					else
						status = ncl_add_length(&cmdbuf, 488);
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
		}
	}
done:;
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_io_sf_sf()
**       Interface for creating a curve/sspline/bspline as a sf/sf or
**       sf/pl intersection
**    PARAMETERS   
**       INPUT  : 
**          type :  0 - curve
**                  1 - s-spline 
**                  2 - b-spline 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_io_sf_sf(type)
int type;
{
	NCL_cmdbuf cmdbuf;
	int status,choice,rel1,rel2;

	uu_denter(UU_MTRC,(us,"nclu_cv_io_sf_sf"));

	while (UU_TRUE)
	{
/*
.....Initialize command buffer and status.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....If auto_label is off, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Put either CURVE/ or SPLINE/ into command.
*/
		if (type == 0) 
			ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
		else if (type == 1)
			ncl_add_token(&cmdbuf, NCL_ssplin, NCL_nocomma);
		else 
			ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
/*
.....Put INTOF, into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_intof, NCL_comma);
/*
.....Prompt for either a surface or a plane.
*/
		status = ncl_add_label_rel1(&cmdbuf, 483, UD_ncl_allsfpl, &rel1);
/*
.....If done was selected, exit.
*/
		if (status != NCL_OKINPUT) goto done;
/*
.....Prompt user for second plane or surface.
*/
		if (rel1 == NCL_PLN_REL)
			status = ncl_add_label_rel1(&cmdbuf, 222, UD_ncl_allsf, &rel2);
		else
			status = ncl_add_label_rel1(&cmdbuf, 483, UD_ncl_allsfpl, &rel2);
/*
.....If a plane was picked, then prompt for
.....Single, All, Near point
*/
		if (rel1 == NCL_PLN_REL || rel2 == NCL_PLN_REL)
			status = ncl_popup(NCL_CV_INTOF,&choice);
		else
			choice = 3;
/*
.....Prompt user for a near point, or point-vector, command is still
.....valid even if none is selected, so process command.
*/
		if (status == NCL_OKINPUT)
		{
			if (choice == 2)
				ncl_add_token(&cmdbuf,NCL_all,NCL_nocomma);
			if (choice == 3)
				status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 484, UD_ncl_ptpv);
			ncl_set_cmdmode(UU_TRUE);
      	ncl_add_cmdbuf(&cmdbuf);
      	ncl_call(&cmdbuf);
		}
	}
done:;
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_conic()
**       Interface for creating a conic curve through
**       some points and/or pointvectors
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_conic()

{
	NCL_cmdbuf cmdbuf;
	int status, reltyp, msg;

	uu_denter(UU_MTRC,(us,"nclu_cv_conic"));

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
.....Put CURVE/CONIC, into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_conic, NCL_nocomma);
/*
...1st POINT or PNTVEC
*/
		msg = 580;
		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, 581,
           UD_ncl_ptpv, &reltyp);
/*
.....If user selected done, exit.
*/
		if (status != NCL_OKINPUT) goto done;

/*
.....If the first selection is a point-vector, then the second choice
.....must be a point, and only 2 or 3 more selections may be made so 
.....jump down to par3.
*/
		if (reltyp == NCL_POINTVEC_REL) 
		{
			msg = 583;
			goto par3;
		}
/*
.....2nd POINT or VECTOR, this is a slope point or vector, not a point for
.....the curve to go thru.
*/
	   status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, msg+2, UD_ncl_ptpve);                   
/*
.....3rd POINT (or second point if the first selection was a pointvector,or
.....the first selection was a point and the second a vector.
*/			                                                        
par3:
      
		if (status == NCL_OKINPUT)
			status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, msg+3, UD_ncl_ptpv);                   
/*
.....4th POINT or PNTVEC, it is more likely that it is the third selection.  If it is
.....the third selection and a point-vector is choosen, this will be the last entry.
*/
   	if (status == NCL_OKINPUT)
 	  		status = ncl_add_label_rel(UD_DASPCKLOC, &cmdbuf, msg+4, UD_ncl_ptpve, &reltyp);

/*
...5th POINT or VECTOR
*/
		if (status == NCL_OKINPUT)
		{
			if (reltyp == NCL_POINT_REL || reltyp == UM_POINT_REL)
		   	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, msg+5, UD_ncl_ptpve);
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
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_composite()
**       Interface for creating a composite curve from components
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_composite()
{
	NCL_cmdbuf cmdbuf;
	struct UM_crvdatabag e;
	UU_LOGICAL lstatus,closefl,closed;
	int status,numint,instat,itis_uvcv,choice,i,nc;
	char str[256];
	UU_LIST key_list;
	UU_KEY_ID *keys;

	uu_denter(UU_MTRC,(us,"nclu_cv_composite"));

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
			instat = ncl_add_name(&cmdbuf, 1);

		while (status == NCL_OKINPUT) 
		{
			do
			{
/*
.....Limit geometry to valid composite curve elements
*/
				ud_lgeo(UU_TRUE, UD_ncl_compcvelm);
/*
.....Prompt user with the selection menu.
*/
			   status = ud_ldas(UD_DASSELECT, UA_NCL, 580, UU_NULL, NCL_MAXPICK,
                        &numint, UD_NODEFAULT);
/*
.....If user hit done,exit
*/
				if (numint<=0) goto done;

				if (status == UD_DASALTACTION)
					 status = NCL_ALTACTION;
				else
					 status = NCL_NOINPUT;

				if (status == NCL_NOINPUT && numint > 0) 
				{
/*
... While there are picked entities on the pick stack...
*/
					lstatus = UU_TRUE;
/*
.....Loop thru, getting the key of the geometry and determine if it is
.....a valid entity for a compcrv.
*/
					uu_list_init(&key_list,sizeof(UU_KEY_ID),numint,1);
					while(ud_gnxt(lstatus, UU_NULL, &e.key, 1) == UU_TRUE)
					{
						if ((ncl_retrieve_data_fixed(&e) == UU_SUCCESS))
						{
							if (ncl_legal_relation(e.rel_num))
							{
/*
... aak 09-dec-1997: check rel_num of the first entity; if it's a CVonSf,
... write SSPLIN/COMPOS ; otherwise, write CURVE/COMPOS
*/
								if(lstatus) 
								{
									itis_uvcv = ncl_itsa_uvcv_onsf(&e);
									if (itis_uvcv) 
										instat = ncl_add_token(&cmdbuf, NCL_ssplin, 
											NCL_nocomma);
									else
										instat = ncl_add_token(&cmdbuf, NCL_cv, 
											NCL_nocomma);

									if (instat == NCL_OKINPUT) 
										instat = ncl_add_token(&cmdbuf, NCL_compos, 
											NCL_nocomma);
								}
								if(itis_uvcv != ncl_itsa_uvcv_onsf(&e))
								{
/*
...illegal  entity for composite curve
*/
									uu_uerror1(UM_MODEL,40, e.rel_num);
								}
/*
.....If everything is okay, get the label and add it to the command 
*/
								if(instat == NCL_OKINPUT)
									uu_list_push(&key_list,&e.key);
							}
						}
						lstatus = UU_FALSE;
					}
					status = NCL_DONE;
				}

			} while(status == NCL_ALTACTION);

			if(status == NCL_NOINPUT) break;
		} 
/*
.....Process command.
*/
		keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
		numint = key_list.cur_cnt;
		status = um_check_connected(numint,keys,&closed);
/*
.....Curve will be made from disconnected entities so bring up
.....the connection type form.
*/
		if (status == UU_FALSE)
		{
			choice = 0;
			status = nclu_compcv_contype(&choice,&closefl,closed);
			if (status == UU_FAILURE) 
			{
				ud_wrerr("Components not connected.");
				goto done;
			}
			if (choice == 0)
				ncl_add_token(&cmdbuf,NCL_linear,NCL_comma);
			if (choice == 1)
				ncl_add_token(&cmdbuf,NCL_smooth,NCL_comma);
			if (choice == 2)
				ncl_add_token(&cmdbuf,"CHAMFR",NCL_comma);
			if (closefl)
				ncl_add_token(&cmdbuf,NCL_close,NCL_comma);
			else
				ncl_add_token(&cmdbuf,NCL_open,NCL_comma);
		}
		for (i=0;i<numint;i++)
		{
			e.key = keys[i];
			ncl_retrieve_data_fixed(&e);
	      ncl_get_label(&e, str);
   	   ncl_add_token(&cmdbuf, str, NCL_comma);
		}
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
	uu_list_free(&key_list);
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_out ()
**       Extract trimmed surface curve.
**    PARAMETERS   
**       INPUT  : 
**          ityp = 1 - Create parametric curve (CURVE)
**				   2 - Create B-spline curve (SPLINE)
**				   3 - Create S-spline curve (SSPLIN)
**				   4 - Create an edge of S-spline curve (SSPLIN)
**				   5 - Create an edge of B-spline curve (SPLINE)
**				   6 - Create multiple edges of S-spline curve (SSPLIN)
**				   7 - Create multiple edges of B-spline curve (SPLINE)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_out(ityp)
int ityp;
{
	NCL_cmdbuf cmdbuf;
	int numint, inpmode, status,i,subid,sh_mode,errfl=0;
	char str[256];
	UM_PLOCREC pick;
	struct NCL_trimsf_rec sf;
	struct NCL_fixed_databag cv;
	UU_LOGICAL Iscompcv,lcurve,lvalid;

	if (ityp == 6 || ityp == 7)
		return nclu_cv_chain_out(ityp-5);

   do 
   {
/*
.....Initialize command buffer
*/
      ncl_init_cmdbuf(&cmdbuf);
      status = NCL_OKINPUT;
/*
.....If auto label is off, prompt user for label.
*/
      if (!NCL_auto_label)
         status = ncl_add_name(&cmdbuf, 1);
/*
.....Determine if it is a curve, bspline or sspline and put the 
.....appropriate word into the command.
*/

		if (ityp == 1)
			status = ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
		else if (ityp == 2 || ityp == 5)
			status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_ssplin, NCL_nocomma);
/*
.....Put OUT, into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_out, NCL_comma);

		inpmode = UD_DASPCKLOC;
		do
		{
			if (inpmode == UD_DASPCKLOC)
			{
/*
.....Limit geometry selection to trim-surfaces.
*/
     			ud_lgeo(UU_TRUE, UD_ncl_trimsf);
/*
.....Prompt user for trim-surface.
*/
     			status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, 529, &pick, 1,
                       &numint, 1);
    			if (status == UD_DASALTACTION)
     			{
        			inpmode = UD_DASSTRING;
        			status = NCL_ALTACTION;
        		}
/*
.....User selected a surface so add it to the command.
*/
     			else if (numint > 0)
  				{
					if (pick.pent.key[1]==pick.pent.key[0]) 
					{
						status = NCL_ALTACTION;
						inpmode = UD_DASPCKLOC;
						ud_wrerr("CVonSF picked is not a boundary CV.");
						goto NOINPUT;
					}
  					ncl_picktostr(&pick, str);
					status = ncl_add_token(&cmdbuf, str, NCL_comma);
					if (status == NCL_OKINPUT )
					{
						subid = -1;
						Iscompcv = UU_FALSE;
						sf.key = pick.pent.key[0];
						status = ncl_retrieve_data_fixed(&sf);

						if (status == UU_SUCCESS)
						{
/*
... If it's a composite cv key.
*/
							sh_mode = ncl_shading_mode ();
							if (ncl_getdisp_shade(&sf)==0) sh_mode = 0;
							ncl_get_cvkey(&sf,pick.pent.key[1],&cv.key,&errfl,sh_mode);
							if (errfl != 0)
							{
								ud_wrerr("CVonSF not found.");
								goto NOINPUT;
							}
							if (cv.key == sf.uv_key)
								subid = 0;
							else
							{
								for (i=0;i<sf.no_ibndykey && (subid==-1);i++)
								{
									if ( cv.key == sf.ibndykey[i])
										subid = (i+1)/2;
								}
							}
							if (subid >= 0)
								Iscompcv = UU_TRUE;
/*
... Verify it's a valid cv.
*/
							if (subid >= 0)
							{
								lvalid = UU_FALSE;

								status = ncl_retrieve_data_fixed(&cv);
								if (status == UU_SUCCESS)
								{
									lcurve = (uc_super_class(cv.rel_num) == UC_CURVE_CLASS);
									if (Iscompcv)
									{
/*
.....Removed check for closed curve.  The curve should already be closed if the
.....curve was used to trim the surface - ASF 2/11/14.
*/
										lvalid = (lcurve /*&& ncl_cv_isclosed (&cv,UM_FUZZ)*/);
									}
									else
									{
										lvalid = (lcurve && cv.rel_num != UM_COMPCRV_REL);
									}
								}
							}
							if (!lvalid) subid = -1;
						}
						sprintf(str, "%d", subid);
  	 					status = ncl_add_token(&cmdbuf, str, NCL_comma);
					}
					if (status == NCL_OKINPUT)
					{
						if (ityp == 3 || ityp == 2)
							status = ncl_add_token(&cmdbuf, "0", NCL_nocomma);
						else if (ityp == 4 || ityp == 5)
						{
							if (subid >= 0 && Iscompcv) 
							{
								sh_mode = ncl_shading_mode ();
								if (ncl_getdisp_shade(&sf)==0) sh_mode = 0;
								if (sh_mode)
									subid = (pick.pent.key[1]-sf.key)%100;
								else
									subid = (pick.pent.key[1])%100;
							}
							else subid = 0;
							sprintf(str, "%d",subid); 
     						status = ncl_add_token(&cmdbuf, str, NCL_nocomma);
						}
					}
					status = NCL_OKINPUT;
				}
     			else
					status = NCL_NOINPUT;
			}
NOINPUT:
 			if (inpmode == UD_DASSTRING)
			{
				status = ncl_add_str(&cmdbuf, 529,NCL_nocomma);
				if (status == NCL_ALTACTION) inpmode = UD_DASPCKLOC;
			}
		} while (status == NCL_ALTACTION);
/*
.....Process command.
*/
		if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
			ncl_set_cmdmode(UU_TRUE);
  			ncl_add_cmdbuf(&cmdbuf);
  			ncl_call(&cmdbuf);
		}
	} while (status == DE_TRUE);
done:;
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_chain_out (ityp)
**       Extract trimmed surface curve.
**    PARAMETERS   
**       INPUT  : 
**			ityp = 1 - Create multiple edges of S-spline curve (SSPLIN)
**				   2 - Create multiple edges of B-spline curve (SPLINE)
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cv_chain_out(ityp)
int ityp;
{
	NCL_cmdbuf cmdbuf;
	int numint, status,i,subid,ib,nb,errfl=0;
	char str[256];
	UM_PLOCREC pick1,pick2;
	struct NCL_trimsf_rec sf;
	struct NCL_fixed_databag cv,e;
	UU_LOGICAL Iscompcv,lcurve,lvalid;
	
	int irtn,nv,segno1,*mask, limit_key, sh_mode;
	UM_coord ptuv,pt,pt1,pt2,*pts,ept,spt,ptmin,eptmin,sptmin;
	UM_vector vec,assist;
	Gwpoint3 point, vector;
	UM_srf_boundary bound;
	UM_real8 tol8;
	UU_REAL tol;
	UU_REAL dis2,mindis;
	UU_LOGICAL cmdreject;	
	uv_segbuff(udata);


	gettol (&tol8);
	tol = tol8;
	limit_key = -1;

	UD_MARK(cmdreject, UU_FALSE);
	if (cmdreject != 0)
	{
		if (limit_key!=-1)
			ud_limit_entity(UU_FALSE, limit_key);
		ud_unlimit ();
		UD_UNMARK(cmdreject);
		return (0);
	}
	do 
    {
/*
.....Initialize command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
		e.key = NULLKEY;
    	subid = 0;
/*
.....If auto label is off, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
/*
.....Determine if it is a curve, bspline or sspline and put the 
.....appropriate word into the command.
*/
		if (ityp == 2)
			status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
		else
			status = ncl_add_token(&cmdbuf, NCL_ssplin, NCL_nocomma);
/*
.....Put OUT, into the command.
*/
		status = ncl_add_token(&cmdbuf, NCL_out, NCL_comma);

edge1:;
/*
.....Limit geometry selection to trim-surfaces.
*/
		ud_unlimit ();
/*
......only trim surface allowed, typo?
.....please double check it, if allow other surface
.....ncl_get_cvkey later will get memory error if sf
.....is not a trim-surf
.....Yurong
//		mask = (int *)UD_ncl_netentity;
*/
		mask = (int *)UD_ncl_trimsf;
		ud_lgeo(UU_TRUE, mask);
/*
.....Prompt user to select the first edge of trimmed-surface
*/
		ud_ldas(UD_DASPCKLOC, UM_MODEL, 150, &pick1, 1,
			&numint,1, UD_NODEFAULT);
/*
.....User selected a surface so add it to the command.
*/
		if (numint > 0)
		{
/*
.....Get point on the selected surface
*/
			ncl_get_pkpt(&pick1,&e,pt1,vec,ptuv);

			if (pick1.pent.key[1]==pick1.pent.key[0]) 
			{
				ud_wrerr("CVonSF picked is not a boundary CV.");
				goto edge1;
			}
			if (status == NCL_OKINPUT )
			{
				subid = -1;
				Iscompcv = UU_FALSE;
				sf.key = e.key;
				status = ncl_retrieve_data_fixed(&sf);
				ncl_get_label(&sf, str);
				status = ncl_add_token(&cmdbuf, str, NCL_comma);

				if (status == UU_SUCCESS)
				{
/*
..... If it's a composite cv key.
*/
					sh_mode = ncl_shading_mode ();
					if (ncl_getdisp_shade(&sf)==0) sh_mode = 0;
					ncl_get_cvkey(&sf,pick1.pent.key[1],&cv.key,&errfl,sh_mode);
					if (errfl != 0)
					{
						ud_wrerr("CVonSF not found.");
						goto edge1;
					}
/*					cv.key = pick1.pent.key[1]/100;*/
					if (cv.key == sf.uv_key)
						subid = 0;
					else
					{
						for (i=0;i<sf.no_ibndykey && (subid==-1);i++)
						{
							if ( cv.key == sf.ibndykey[i])
								subid = (i+1)/2;
						}
					}
					if (subid >= 0)
						Iscompcv = UU_TRUE;
/*
..... Verify it's a valid cv.
*/
					if (subid >= 0)
					{
						lvalid = UU_FALSE;
						status = ncl_retrieve_data_fixed(&cv);
						if (status == UU_SUCCESS)
						{
							lcurve = (uc_super_class(cv.rel_num) == UC_CURVE_CLASS);
/*
.....Removed check for closed curve.  The curve should already be closed if the
.....curve was used to trim the surface - ASF 2/11/14.
*/
							if (Iscompcv)
								lvalid = (lcurve /*&& ncl_cv_isclosed (&cv,UM_FUZZ)*/);
							else
								lvalid = (lcurve && cv.rel_num != UM_COMPCRV_REL);
						}
					}
					if (!lvalid) subid = -1;
				}
				sprintf(str, "%d", subid);
				status = ncl_add_token(&cmdbuf, str, NCL_comma);
			}
			if (status == NCL_OKINPUT)
			{
				if (subid >= 0 && Iscompcv)
				{
					sh_mode = ncl_shading_mode ();
					if (ncl_getdisp_shade(&sf)==0) sh_mode = 0;
					if (sh_mode)
						subid = (pick1.pent.key[1]-sf.key)%100;
					else
						subid = (pick1.pent.key[1])%100;
				}
				else
					subid = 0;
				sprintf(str, "%d",subid); 
				status = ncl_add_token(&cmdbuf, str, NCL_comma);
			}
			status = NCL_OKINPUT;
		}
		else
		{
			status = NCL_NOINPUT;
			if (limit_key!=-1)
				ud_limit_entity(UU_FALSE, limit_key);
			ud_unlimit ();
			break;
		}

		if (subid == 0)
			goto process;
/*
.....Project this point on the nearest sf bndry edge
.....display assist vectors in the two directions ablong this bndry edge
*/
		ncl_set_boundary_toler (tol);
		status = ncl_get_boundary (WHOLE_BOUNDARY_LIST,&e,&bound);	
		if (status == UU_SUCCESS)
		{
			nb = bound.nb;
			pts = (UM_coord *) UU_LIST_ARRAY (bound.cvpts);
			mindis = 10000.;
			for (ib = 0; ib < nb; ib++, pts += nv)
			{
				nv = bound.np[ib];
				ncl_near_bndry_edge(nv,pts,pt1,spt,ept,&segno1,pt);
				dis2 = UM_SQDIS(pt1,pt);
				if (dis2 < mindis)
				{
					mindis = dis2;
					um_vctovc (pt,ptmin);
					um_vctovc (spt,sptmin);
					um_vctovc (ept,eptmin);
				}
			}

			point.x = ptmin[0];
			point.y = ptmin[1];
			point.z = ptmin[2];
			um_vcmnvc(eptmin,ptmin,assist);
			um_unitvc(assist,assist);
			vector.x = assist[0];
			vector.y = assist[1];
			vector.z = assist[2];
			ud_assist_vector(point,vector);
			um_vcmnvc(sptmin,ptmin,assist);
			um_unitvc(assist,assist);
			vector.x = assist[0];
			vector.y = assist[1];
			vector.z = assist[2];
			ud_assist_vector(point,vector);
		}
/*
.......pick1.pent.key[0] is segment number, we need get key
*/
		gsegrud(pick1.pent.key[0], udata);
		limit_key = uv_getkey(udata);
		ud_limit_entity(UU_TRUE, limit_key);
/*
.....Prompt user to pick one of the assist vectors to determine direction.
*/
		ud_unlimit ();
		irtn = ud_pick_assist_seg("Pick the direction to chain surface edges:");	

		if (irtn == 0) 
		{
			if (limit_key!=-1)
			{
				ud_limit_entity(UU_FALSE, limit_key);
				limit_key = -1;
			}
			continue;
		}
		else if (irtn == 2)
			status = ncl_add_token(&cmdbuf, NCL_ccw, NCL_comma);
		else
			status = ncl_add_token(&cmdbuf, NCL_clw, NCL_comma);
edge2:;
/*
.....Prompt user to select the last edge of trimmed-surface
*/
		ud_lgeo(UU_TRUE, mask);
		ud_ldas(UD_DASPCKLOC, UM_MODEL, 152, &pick2, 1,
			&numint,1, UD_NODEFAULT);
		if (numint == 0) continue;

		if (pick2.pent.key[1]==pick2.pent.key[0]) 
		{
			ud_wrerr("CVonSF picked is not a boundary CV.");
			goto edge2;
		}

		if (pick1.pent.key[0] != pick2.pent.key[0]) 
		{
			ud_wrerr("The selected edges are on different surfaces");
			goto edge2;
		}
/*
.....Get the last edge of selected surface
*/
		if (status == UU_SUCCESS)
		{
			subid = -1;
			Iscompcv = UU_FALSE;
/*
..... If it's a composite cv key.
*/
			sh_mode = ncl_shading_mode ();
			if (ncl_getdisp_shade(&sf)==0) sh_mode = 0;
			ncl_get_cvkey(&sf,pick2.pent.key[1],&cv.key,&errfl,sh_mode);
			if (errfl != 0)
			{
				ud_wrerr("CVonSF not found.");
				goto edge2;
			}
/*			cv.key = pick2.pent.key[1]/100;*/
			if (cv.key == sf.uv_key)
				subid = 0;
			else
			{
				for (i=0;i<sf.no_ibndykey && (subid==-1);i++)
				{
					if ( cv.key == sf.ibndykey[i])
						subid = (i+1)/2;
				}
			}
			if (subid >= 0)
				Iscompcv = UU_TRUE;
/*
..... Verify it's a valid cv.
*/
			if (subid >= 0)
			{
				lvalid = UU_FALSE;
				status = ncl_retrieve_data_fixed(&cv);
				if (status == UU_SUCCESS)
				{
					lcurve = (uc_super_class(cv.rel_num) == UC_CURVE_CLASS);
/*
.....Removed check for closed curve.  The curve should already be closed if the
.....curve was used to trim the surface - ASF 2/11/14.
*/
					if (Iscompcv)
						lvalid = (lcurve /*&& ncl_cv_isclosed (&cv,UM_FUZZ)*/);
					else
						lvalid = (lcurve && cv.rel_num != UM_COMPCRV_REL);
				}
			}
			if (!lvalid) subid = -1;
		}

		if (status == NCL_OKINPUT)
		{
			if (subid >= 0 && Iscompcv)
			{
				sh_mode = ncl_shading_mode ();
				if (ncl_getdisp_shade(&sf)==0) sh_mode = 0;
				if (sh_mode)
					subid = (pick2.pent.key[1]-sf.key)%100;
				else
					subid = (pick2.pent.key[1])%100;
			}
			else 
				subid = 0;
			sprintf(str, "%d",subid); 
			status = ncl_add_token(&cmdbuf, str, NCL_nocomma);
		}
		status = NCL_OKINPUT;
		if (limit_key!=-1)
		{
			ud_limit_entity(UU_FALSE, limit_key);
			limit_key = -1;
		}
NOINPUT:
/*
.....Process command.
*/
process:;
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	} while (status == DE_TRUE);

done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_comp_cv_out ()
**       Interface for creating a curve/sspline/bspline as a 
**       projection of a curve/spline/circle/line onto a surface/plane
**    PARAMETERS   
**       INPUT  :  none 
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_comp_cv_out()
{
	NCL_cmdbuf cmdbuf;
	int status,lstatus,cur_len,len,sub,nc,tval,limit_key;
	int numint,i,i0,i1,choice,nkeys,ind=0;
	UU_KEY_ID key,key1,*keys;
	char str[80];
	struct NCL_fixed_databag crv;
	struct UM_compcrv_rec *eptr;
	UU_LOGICAL found,done,first,lstring,lpar;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT],lab[NCL_MAX_LABEL];
	char tstr[NCL_MAX_LABEL],slab[NCL_MAX_LABEL],*scalab;
	UM_f77_str f77lab;
	UM_int2 subsc;
	UU_LIST key_list;

	UM_PLOCREC pick;
	UU_LOGICAL cmdreject;	
	uv_segbuff(udata);

	uu_denter(UU_MTRC,(us,"nclu_cv_proj_sf"));

	choice = SINGLE;
	done = UU_FALSE;
	status = ncl_popup(NCL_COMPCV_OUT, &choice);

	limit_key = -1;
	UD_MARK(cmdreject, UU_FALSE);
	if (cmdreject != 0)
	{
		if (limit_key!=-1)
			ud_limit_entity(UU_FALSE, limit_key);
		ud_unlimit ();
		UD_UNMARK(cmdreject);
		return (0);
	}
	while (status == NCL_OKINPUT)
	{
		first = UU_TRUE;
		lstring = UU_FALSE;
/*
.....Prepick the composite curves if ALL
*/
		if (choice == ALL)
		{
			ud_lgeo(UU_TRUE, UD_compcrv);
			ud_ldas(UD_DASSELECT,UM_MODEL,148,UU_NULL,NCL_MAXPICK,&numint,
				UD_NODEFAULT);
			if (numint <= 0) break;
			uu_list_init(&key_list,sizeof(UU_KEY_ID),numint,1);
			lstatus = UU_TRUE;
			while (ud_gnxt(lstatus,UU_NULL,&key,1))
			{
				lstatus = UU_FALSE;
				uu_list_push(&key_list,&key);
			}
			keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
			nkeys = key_list.cur_cnt;
		}
		do
		{
/*
.....Initialize command buffer and status.
*/
			ncl_init_cmdbuf(&cmdbuf);
/*
.....If auto label is off, prompt user for label.
*/
			if (!NCL_auto_label)
				status = ncl_add_name(&cmdbuf, 1);

			ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
/*
.....Have user pick a composite curve if not ALL
*/
			if (choice != ALL)
			{
				ud_lgeo(UU_TRUE, UD_compcrv);
				if (choice == THRU)
					ua_dl_pldas(UD_DASPCKLOC,UA_NCL,577,&pick,1,&numint,1);
				else
					ua_dl_pldas(UD_DASPCKLOC,UM_MODEL,148,&pick,1,&numint,1);
				key = um_get_pickkey(&pick.pent, 1);
				if (numint <= 0) break;
				limit_key = key;
				ud_limit_entity(UU_TRUE, limit_key);
			}
			else
			{
				if (ind == nkeys) 
				{
					uu_list_free(&key_list);
					break;
				}
				key = keys[ind++];
			}
			i0 = i1 = 1;
			crv.key = key;

			status = ncl_retrieve_data_fixed (&crv);
			if (status != UU_SUCCESS) break;

			eptr = (struct UM_compcrv_rec *) &crv;
			if (choice != ALL)
			{

				key1 = um_get_pickkey(&pick.pent, 2);
				found = UU_FALSE;
				for (i = 0; i < eptr->no_cid && !found; i++)
					found = (key1 == eptr->cid[i].crvid);

				if (found) i0 = i1 = i;

			}
			ncl_add_token(&cmdbuf, NCL_out, NCL_comma);
/*
.....Format label used in command to include subscripts
*/
			strcpy(lab,eptr->label);
			len = strlen(lab);
			ul_strip_blanks(&lab,&len);
			ncl_format_label(&lab,eptr->subscr,&label,0);
			len = strlen(label);
			ul_strip_blanks(&label,&len);
			ncl_add_token(&cmdbuf,label,NCL_comma);

			if (choice == SINGLE)
			{
				sprintf(str, "%d",i0); 
     			status = ncl_add_token(&cmdbuf, str, NCL_nocomma);
			}
			else if (choice == ALL)
     			status = ncl_add_token(&cmdbuf, "ALL", NCL_nocomma);
			else /* choice == THRU */
			{
				if (found)
				{
					i1 = i0;
					ua_dl_pldas(UD_DASPCKLOC,UA_NCL,578,&pick,1,&numint,1);
					if (numint > 0)
					{
						key = um_get_pickkey(&pick.pent, 1);
						if (crv.key == key)
						{
							key1 = um_get_pickkey(&pick.pent, 2);
							found = UU_FALSE;
							for (i = 0; i < eptr->no_cid && !found; i++)
								found = (key1 == eptr->cid[i].crvid);
							if (found) i1 = i;
						}
					}
				}
				if (i0 < i1)
					sprintf(str,"%d,THRU,%d",i0,i1);
				else if (i0 > i1)
					sprintf(str,"%d,THRU,%d",i1,i0);
				else
					sprintf(str, "%d",i); 
     			status = ncl_add_token(&cmdbuf, str, NCL_nocomma);
			}
			if (status != UU_SUCCESS) break;
/*
.....Added number number of components variable to command syntax options
.....Andrew 11/9/12
*/
			if (choice != SINGLE && first)
			{
				ncl_get_str(&slab,673);
				nc = strlen(slab);
				ul_strip_blanks(slab,&nc);
				lpar = (slab[nc-1] == ')');
				if (choice == ALL) first = UU_FALSE;
				if (nc > 0)
				{
					UM_init_f77_str(f77lab,slab,NCL_MAX_LABEL);
					subsc = parslb(UM_addr_of_f77_str(f77lab),&tval);
					scalab = UM_cstr_of_f77_str(f77lab);
					nc = strlen(scalab);
					if (lpar) scalab[nc-1] = 0;
					sub = subsc;
					if (sub == 0 && nkeys > 1 && choice == ALL && tval == 1)
						sub++;
					lstring = UU_TRUE;
					if (sub > 0 || tval == 0)
					{
						if (lpar || (tval == 1 && choice == ALL && nkeys > 1))
							sprintf(tstr,"%s(%d)",scalab,sub);
						else
							sprintf(tstr,"%s%d",scalab,sub);
					}
					else
						sprintf(tstr,"%s",scalab);
					ncl_add_token(&cmdbuf,",NUM", NCL_comma);
					ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
				}
			}
			else if (choice == ALL && !first && lstring)
			{
				sub++;
				if (lpar || tval == 1)
					sprintf(tstr,"%s(%d)",scalab,sub);
				else
					sprintf(tstr,"%s%d",scalab,sub);
				ncl_add_token(&cmdbuf,",NUM", NCL_comma);
				ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			}
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			if (limit_key!=-1)
			{
				ud_limit_entity(UU_FALSE, limit_key);
				limit_key = -1;
			}
		} while (!done);
		if (choice == ALL && key_list.data != UU_NULL)
			uu_list_free(&key_list);
		break;
	}
	ud_unlimit();
	if (limit_key!=-1)
		ud_limit_entity(UU_FALSE, limit_key);
	UD_UNMARK(cmdreject);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_revsf(type)
**      Make a curve/sspline along the axis of a revsurf
**    PARAMETERS   
**       INPUT  : 
**          type	:	0 - spline/
*						1 - ssplin/
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_cv_revsf(type)
int type;
{
	int status;
	NCL_cmdbuf cmdbuf;
	char str[256];
	struct NCL_fixed_databag sf;
	struct NCL_trimsf_rec surf;
	UM_int2 primtyp;
	int rel_num = 0;
	char ptlab[256];
	UU_KEY_ID skey;
		
	while (UU_TRUE)
	{
prompt:
/*
.....Initialize commadn buffer and status.
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....If auto label is off, prompt user for label.
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
		switch(type)
		{
			case 0:
			ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
			break;
			case 1:
			ncl_add_token(&cmdbuf, NCL_ssplin, NCL_nocomma);
			break;
		}
		ncl_add_token(&cmdbuf, NCL_out, NCL_comma);
/*
.....Select surface.
*/
		status = ncl_get_dlabel_rel(str,ptlab,UU_FALSE,205,UD_ncl_netentity,&skey,
			&rel_num);
/*
.....If done is selected, exit
*/
		if (status != NCL_OKINPUT) goto done;
		
		sf.key = skey;
		ncl_retrieve_data_fixed (&sf);
		if (rel_num == NCL_TRIMSF_REL)
		{
			surf.key = sf.key;
			status = ncl_retrieve_data_fixed(&surf);
			sf.key = surf.bs_key;
			status = ncl_retrieve_data_fixed(&sf);
		 }

		 ncl_get_sf_primtyp(&sf.key,&primtyp);
		 if(primtyp != NCLSF_CYLINDER && primtyp !=NCLSF_CONE &&
			sf.rel_num != NCL_REVSURF_REL)
		 {
			 ud_wrerr("The entity is not a cone/cylinder or a surface of revolution. Retry. [More info in HELP.]");
			 goto prompt;
		 }
/*
......Put the surface into the command buffer.
*/
		ncl_add_token(&cmdbuf,str,NCL_comma);
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
done:
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     : ncl_get_cvkey(sf,pickid,cvkey,errfl,shade)
**      Get curve key from pick id for a trim surface using its
**      display list.  This routine relies on the display list format
**      "xxyy" where xx is the index in the curve table and yy is the
**      curve component number.
**    PARAMETERS   
**       INPUT  : 
**          sf	    - trim surface pick is on
**          pickid - pick id returned from picking routine
**          shade  - Surface shaded flag.
**       OUTPUT :  
**          cvkey  - key of curve picked
**          errfl  - 1 = error getting curve key.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_cvkey(sf,pickid,cvkey,errfl,shade)
struct NCL_trimsf_rec *sf;
int pickid,*errfl,shade;
UU_KEY_ID *cvkey;
{
	int cv;
	*errfl = 0;
	if (shade)
		pickid -= sf->key;
	cv = (pickid/100)-1;
	if (cv < 0 || 2*cv > sf->no_ibndykey)
	{
		*errfl = 1;
		return;
	}
	if (cv == 0)
		*cvkey = sf->uv_key;
	else
		*cvkey = sf->ibndykey[2*cv-1];
}

/*********************************************************************
**    S_FUNCTION     : nclu_compcv_contype(type,closefl)
**      Handles the connection type form for creating a composite
**      curve from diconnected entities.
**    PARAMETERS   
**       INPUT  : 
**          closed  - UU_TRUE : Curve will already be closed
**                    UU_FALSE: Let user decide if curve should be
**                              closed
**       OUTPUT :  
**          type    - connection type selected
**          closefl - UU_TRUE : Create a closed curve
**                    UU_FALSE: Do not close the curve
**    RETURNS      : UU_FAILURE iff CANCEL
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_compcv_contype(type,closefl,closed)
int *type;
UU_LOGICAL *closefl,closed;
{
	int status;

	static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL};
	static char called[] = {6,6,6,6,6,6,6,6};
	char traverse[] = {1,1,0,0,0,0,0,0};
	static char display[] = {1,1,1,1,1,1,1,1};
	static int *ans[] = {&Scontype,&Sclose,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL};

	if (closed) traverse[1] = 0;
	status = ud_form1("compcrv.frm",ans,ans,methods,called,display,
		traverse);
	if (status == -1) return(UU_FAILURE);
	*type = Scontype;
	*closefl = Sclose;
	return(UU_SUCCESS);
}


