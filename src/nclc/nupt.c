/*********************************************************************
**    NAME         :  nupt.c
**       CONTAINS: User interface routines for point creation
**			nclu_pt_xy()
**			nclu_pt_offset()
**			nclu_pt_intof()
**			nclu_pt_intof3()
**			nclu_pt_on_cv()
**			nclu_pt_dist_cv()
**			nclu_pt_on_sf()
**			nclu_pt_endpt()
**			nclu_pt_te()
**			nclu_pt_ci_atangl(eptr)
**			nclu_pt_pt_ci_angle()
**			nclu_pt_ce_ci()
**			nclu_pt_patern()
**			nclu_pt_proj_sf()
**			nclu_pt_fit()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nupt.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:13
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "mdclass.h"
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "mdpick.h"
#include "misect.h"
#include "mdeval.h"
#include "view.h"

#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclfc.h"

#include "go.h"				/* temporary */
#include "mdattr.h"			/* temporary */

#define  UM_maxrepetition 1024 

/*********************************************************************
**    E_FUNCTION     : nclu_pt_xy()
**       Create a single point.
**    PARAMETERS   
**       INPUT  : 
**          ifl	= 0 - XY Point ; 1 - XYZ Point.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_xy(ifl)
int ifl;
{
	struct UM_point_rec e;              /* point  entity structure */
	int numint,j,nxy,first;
	UD_NDCLOCREC tmp;
	char str[256];
	NCL_cmdbuf cmdbuf;
	int status,i,len;
	UD_SCA_VALUE zval;

	ur_setup_data(UM_POINT_REL, &e, sizeof(struct UM_point_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;
	nxy = 2;
	if (ifl == 1) nxy = 3;
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
		ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
		first = UU_TRUE;
/*
.....Get Z-level
*/
		if (ifl == 1)
		{
			status = ud_ldas(UD_SCADISTANCE, UA_NCL, 3, &zval, 1,
				&numint, UD_NODEFAULT);
			if (numint <= 0) goto done;
		}
/*
.....Get point coordinates
*/
		while (UU_TRUE)
		{
/*
.....If not autolabel, then get label
*/
			if (!first)
			{
					status = NCL_OKINPUT;
					if (!NCL_auto_label)
							status = ncl_add_name(&cmdbuf,1);
					if (status == NCL_DONE) goto done;
					ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
					first = UU_FALSE;
			}

/*
.....using UD_SCACART, it will save the input string
.....in the tmp.label and location in tmp.cord
*/
/*			ud_ldas(UD_DASCART, UM_MODEL, 35, &tmp, 
				 	1, &numint, UD_NODEFAULT);
*/
			ud_ldas(UD_SCACART,/*coordinate of point*/UM_MODEL, 35, &tmp, 
				 	1, &numint, UD_NODEFAULT);
			for(j=0; j<3; j++) e.pt[j] = tmp.cord[j];
			if (ifl == 1) e.pt[2] = zval.value;
			if (numint <= 0) goto repeat;
/*		um_create_pt1(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);*/
/*		uc_display(&e);*/
/*
.....Place point definition into source file
*/
			ncl_set_cmdmode(UU_TRUE);
/*
.....testing UD_SCACART, input string saved in the tmp.label
//			ncl_cctostr(nxy,e.pt,str);
*/
/*
.....remove preceeding spaces
*/
			i = 0; len = strlen(tmp.label);
			while (tmp.label[i]==' ') i++;
			if (tmp.label[i]!='\0')
			{
				strcpy(str, &(tmp.label[i]));
				if (ifl == 1)
				{
/*
.....add or replace the z value
*/
					ncl_replace_cordstr(str, zval.label, 3);
				}
			}
			else
			{
				ncl_cctostr(nxy, e.pt, str);
			}
			ncl_add_token(&cmdbuf,str,NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
/*
.....If not autolabel, then get label
*/
			status = NCL_OKINPUT;
			if (!NCL_auto_label)
				status = ncl_add_name(&cmdbuf,1);
			if (status == NCL_DONE) goto done;
			ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
		}
repeat:;
			if (ifl == 0) goto done;
	}
done:;
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_offset()
**       Create a sequence of delta points from a single point.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_offset()
	{
	struct UM_point_rec e;               /* point  entity structure */
/**	UM_coord basept; **/

	int i, len, numint;									/* number of interactions */
	char str[256],label[64];
	NCL_cmdbuf cmdbuf;
	int status, save_loc;
	UU_LOGICAL first;
	UD_NDCLOCREC tmp;
	int markval=0;

	uu_denter(UU_MTRC,(us,"umu_c1_deltapt()"));

	ur_setup_data(UM_POINT_REL, &e, sizeof(struct UM_point_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;
	save_loc = UD_locint;
	UD_locint = UD_STRING;
	UD_MARK(markval,UU_FALSE);
	if (markval != 0)
	{
		UD_locint = save_loc;
		UD_UNMARK (markval);
		return (UU_SUCCESS);
	}
	while (UU_TRUE)
		{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
		ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
		first = UU_TRUE;
/*
.....Get base point
*/
		status = ncl_get_dlabel(UD_DASPCKLOC,label,12,UD_ncl_ptpv);
		if (status == NCL_DONE || status == NCL_NOINPUT) goto done;
/*
.....Get offset vector
*/
		while (UU_TRUE)
		{
/*
.....If not autolabel, then get label
*/
			if (!first)
			{
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
				if (status == NCL_DONE) goto done;
				ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
				first = UU_FALSE;
			}
/*			ud_ldas(UD_SCAVEC, UM_MODEL, 249, &delta, 
				1, &numint, UD_NODEFAULT); */
			ud_ldas(UD_SCACART, UM_MODEL, 249, &tmp, 
				1, &numint, UD_NODEFAULT); 
			if (numint <= 0) goto repeat;
/*
.....Place point definition into source file
*/
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_token(&cmdbuf,label,NCL_comma);

			i = 0; len = strlen(tmp.label);
			while (tmp.label[i]==' ') i++;
			if (tmp.label[i]!='\0') 
				strcpy(str, &(tmp.label[i]));
			else
				ncl_vctostr(tmp.cord,str);

			ncl_add_token(&cmdbuf,str,NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
/*
.....If not autolabel, then get label
*/
			status = NCL_OKINPUT;
			if (!NCL_auto_label)
				status = ncl_add_name(&cmdbuf,1);
			if (status == NCL_DONE) goto done;
			ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
		}
repeat:;
		}
done:;
	uu_dexit;
	UD_locint = save_loc;
	UD_UNMARK (markval);
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_intof()
**       Create a point by the intersection of two planar curves.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_intof()

{
	int numint;							/* number of DAS items returned */
	struct UM_crvdatabag *e1;	/* first curve */
	struct UM_crvdatabag *e2;	/* second curve */
	struct NCL_fixed_databag s1, s2;
	UM_transf tfmat1;		/* transformation for first curve */
	UM_transf tfmat2;		/* transformation for second curve */
	int nint,ipt;			/* number of intersection points */
	int status, choice, intofs, ifirst;
	int relnum1, relnum2;
	UM_isect ibuff[UM_MAXISECT];		/* intersection points */
	UM_PLOCREC pick,pick2;				/* pick information */
	NCL_cmdbuf cmdbuf;
	char lmod[8],str[80],str1[80];
	int closest;
	UM_int2 primtyp;
	UU_LOGICAL first;
	UM_coord coord;
	struct UM_circle_rec *c1ptr, *c2ptr;
	struct UM_line_rec *l1ptr;
	UM_coord nrpt;

	uu_denter(UU_MTRC,(us,"umu_c1_ipcpc()"));

	e1 = (struct UM_crvdatabag *) &s1;
	e2 = (struct UM_crvdatabag *) &s2;

	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
		first = UU_TRUE;
		ifirst = 0;
/*
.....Get first intersection entity
*/
		ua_dl_pldas(UD_DASPCKLOC,/*pick first curve*/UM_MODEL,138,&pick,1,&numint,1);
		if (numint <= 0) goto done;
		e1->key = um_get_pickkey(&pick.pent, 1);
		ur_retrieve_data_relnum(e1->key, &(relnum1));
		if (relnum1 == UM_RBSPLSRF_REL || relnum1 == NCL_SURF_REL ||
			 relnum1 == NCL_TRIMSF_REL)
		{
			ncl_get_sf_primtyp(&e1->key, &primtyp);
			if (primtyp == 3) relnum1 = NCL_PLN_REL;
		}
		if (UM_CURVE_CLASS != uc_super_class(relnum1) &&
		    relnum1 != NCL_POINTVEC_REL && relnum1 != NCL_PLN_REL)
		{
			uu_uerror0(/*curve not picked*/UM_MODEL,142);
			goto repeat;
		}
		while (UU_TRUE)
		{
/*
.....If not autolabel, then get label
*/
         if (!first)
         {
            status = NCL_OKINPUT;
            if (!NCL_auto_label)
               status = ncl_add_name(&cmdbuf,1);
            if (status == NCL_DONE) goto done;
         }
ask_again:;
         first = UU_FALSE;
/*			um_dl_pdas(UD_DASPICK,UM_MODEL,139,&pick2,1,&numint,1);*/
			ua_dl_pldas(UD_DASPCKLOC,UM_MODEL,139,&pick2,1,&numint,1);
			if (numint <= 0) goto repeat;
			e2->key = um_get_pickkey(&pick2.pent, 1);
			ur_retrieve_data_relnum(e2->key, &(relnum2));
			if (relnum2 == UM_RBSPLSRF_REL || relnum2 == NCL_SURF_REL ||
				 relnum2 == NCL_TRIMSF_REL)
			{
				ncl_get_sf_primtyp(&e2->key, &primtyp);
				if (primtyp == 3) relnum2 = NCL_PLN_REL;
			}
/*
.....A circle and a plane cannot currently be intersected, so
.....display an error message.  JLS 6/30/99
*/
			if ((relnum1 == UM_CIRCLE_REL && relnum2 == NCL_PLN_REL) ||
			 	(relnum2 == UM_CIRCLE_REL && relnum1 == NCL_PLN_REL))
			{
				uu_uerror0(/*curve not picked**/UM_MODEL,327);
				goto ask_again;
			}

			if (UM_CURVE_CLASS != uc_super_class(relnum2) &&
			    relnum2 != NCL_POINTVEC_REL && relnum2 != NCL_PLN_REL)
			{
				uu_uerror0(/*curve not picked**/UM_MODEL,142);
			}
			else
			{
				if (e1->key == e2->key)
					uu_uerror0(/*same curve picked for both curves*/UM_MODEL,79);
				else
				{
/*
.....Place point definition into source file
.....Get curve data and xform matrices
*/
					status = ncl_retrieve_data_fixed(e1);
					if (status != UU_SUCCESS) goto repeat;
					status = uc_retrieve_transf(e1->key, tfmat1);
					if (status != UU_SUCCESS) goto repeat;
					status = ncl_retrieve_data_fixed(e2);
					if (status != UU_SUCCESS) goto repeat;
					status = uc_retrieve_transf(e2->key, tfmat2);
					if (status != UU_SUCCESS) goto repeat;
/*
.....Convert PointVectors to Lines
*/
					if (relnum1 == NCL_POINTVEC_REL)
					{
						um_pv_to_line(e1,e1,UU_TRUE);
						relnum1 = UM_LINE_REL;
					}
					if (relnum2 == NCL_POINTVEC_REL)
					{
						um_pv_to_line(e2,e2,UU_TRUE);
						relnum2 = UM_LINE_REL;
					}
/*
........INTOF,PL,PL,PL
*/
					if (relnum1==NCL_PLN_REL && relnum2==NCL_PLN_REL)
					{
						ncl_set_cmdmode(UU_TRUE);
						ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
						ncl_add_token(&cmdbuf,NCL_intof,NCL_comma);
						ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
						ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
						status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,20,UD_ncl_pl);
						if (status == NCL_OKINPUT)
						{
							ncl_add_cmdbuf(&cmdbuf);
							ncl_call(&cmdbuf);
						}
					}
/*
........INTOF,LN/PV,LN/PV/PL
*/
					else if ((relnum1==UM_LINE_REL &&
					         (relnum2==UM_LINE_REL ||
					          relnum2==NCL_PLN_REL)) ||
					         ((relnum1==UM_LINE_REL ||
					           relnum1==NCL_PLN_REL) &&
					           relnum2==UM_LINE_REL))
					{
						ncl_set_cmdmode(UU_TRUE);
						ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
						ncl_add_token(&cmdbuf,NCL_intof,NCL_comma);
						if (relnum1==NCL_PLN_REL)
						{
								ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
								ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
						}
						else
						{
								ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
								ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
						}
						ncl_add_cmdbuf(&cmdbuf);
						ncl_call(&cmdbuf);
					}
/*
........INTOF,LN/PV/CI,CI
*/
					else if (((relnum1==UM_LINE_REL ||
					           relnum1==UM_CIRCLE_REL) &&
					           relnum2==UM_CIRCLE_REL) ||
					         ((relnum2==UM_LINE_REL ||
					           relnum2==UM_CIRCLE_REL) &&
					           relnum1==UM_CIRCLE_REL))
					{
						if (relnum1==UM_CIRCLE_REL && relnum2==UM_CIRCLE_REL)
						{
							c1ptr = (struct UM_circle_rec *)e1;
							c2ptr = (struct UM_circle_rec *)e2;
							um_nptpln (c2ptr->center,c1ptr->center,c1ptr->nvec,c2ptr->center);
						}
						else
						{
							if (relnum1==UM_LINE_REL)
							{
								l1ptr = (struct UM_line_rec *)e1;
								c1ptr = (struct UM_circle_rec *)e2;
							}
							else
							{
								l1ptr = (struct UM_line_rec *)e2;
								c1ptr = (struct UM_circle_rec *)e1;
							}
							um_nptpln (l1ptr->spt,c1ptr->center,c1ptr->nvec,l1ptr->spt);
							um_nptpln (l1ptr->ept,c1ptr->center,c1ptr->nvec,l1ptr->ept);
						}
						status = uc_crv_intersect(e1,tfmat1,e2,tfmat2,
							&nint, UM_MAXISECT, ibuff);
						if (status != UU_SUCCESS || nint == 0)
							uu_uerror0(/*no intersection found*/UM_MODEL,80);
						else
						{
							closest = 0;
							if (nint > 1)
								closest = um_nearest_isect_to_ploc(&pick2.ploc,nint,ibuff);
							um_set_modsys_refsys();
							um_trim_modifier(ibuff,nint,closest,lmod);
							ncl_set_cmdmode(UU_TRUE);
							ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
							ncl_add_token(&cmdbuf,lmod,NCL_comma);
							ncl_add_token(&cmdbuf,NCL_intof,NCL_comma);
							if (relnum1 == UM_CIRCLE_REL)
							{
								ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
								ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
							}
							else
							{
								ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
								ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
							}
							ncl_add_cmdbuf(&cmdbuf);
							ncl_call(&cmdbuf);
						}
					}
/*
........INTOF,CV,CV,PT
*/
					else
					{
						ncl_set_cmdmode(UU_TRUE);
						ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
						ncl_add_token(&cmdbuf,NCL_intof,NCL_comma);
						if (relnum1==NCL_PLN_REL || relnum1==UM_CIRCLE_REL ||
							relnum2==UM_LINE_REL || relnum2==UM_CIRCLE_REL ||
							relnum2==NCL_PLN_REL)
						{
							ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
							ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
						}
						else
						{
							ncl_add_token(&cmdbuf,pick2.ploc.label,NCL_comma);
							ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
						}
/*
.....Get the intersection number of points
*/
						nint = 1;
						if (relnum1 != NCL_PLN_REL && relnum1 != UM_CIRCLE_REL)	
						{
							ipt = 2;

							if (relnum2 == NCL_PLN_REL) 
							{
								UU_REAL buff[6];		
								int plnr,ier;
								int cvflg = 1;
								plcvsf (&e1->key,&cvflg,buff,&plnr,&ier);
								if (ier != 1)
									um_pl_to_line(e2,buff,e2);
							}

							status = um_isect_curves(e1,tfmat1,e2,tfmat2,&ipt,
											nrpt,&nint, UM_MAXISECT, ibuff);
						}

						choice = 1;
						status = NCL_OKINPUT;
						if (nint > 1 && ifirst == 0)		
						{
							status = ncl_popup(NCL_CC_INTOF,&choice);
							ifirst++;
						}

						if (status == NCL_OKINPUT)
						{
							if (choice == 1)
							{
								um_ploctocc(&pick2.ploc,coord);
								ncl_cctostr(2,coord,str);
								sprintf(str1,"(PT/%s)",str);
								ncl_add_token(&cmdbuf,str1,NCL_nocomma);
							}
							else if (choice == 2)
							{
								ncl_add_token(&cmdbuf,"ALL",NCL_nocomma);
							}								
							ncl_add_cmdbuf(&cmdbuf);
							ncl_call(&cmdbuf);
						}
					}
				}
			}
		}
repeat:;
	}
done:;
	uu_dexit;
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_intof3()
**       Create a point at the intersection of two 3D curves.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_intof3()
{
	int numint;							/* number of DAS items returned */
	NCL_cmdbuf cmdbuf;

	struct UM_crvdatabag *e1;	/* first curve */
	struct UM_crvdatabag *e2;	/* second curve */
	struct NCL_fixed_databag s1, s2;
	UM_transf tfmat1;					/* transformation for first curve */
	UM_transf tfmat2;					/* transformation for second curve */

	int status,prompt,choice,nint,ierr,ifirst;
	int relnum1, relnum2;
	UM_coord nrpt;
	UM_isect ibuff[UM_MAXISECT];
	UU_LOGICAL first;
	char cv1_lab[80],cv2_lab[80],nrpt_lab[80];
 	static char intof3 [7] = "INTOF3";

	uu_denter(UU_MTRC,(us,"nclu_pt_intof3()"));

	e1 = (struct UM_crvdatabag *) &s1;
	e2 = (struct UM_crvdatabag *) &s2;

	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) return (0);
		first = UU_TRUE;
		ifirst = 0;
/*
.....Get first intersection entity
*/
		prompt = 138;
		numint = ncl_add_label_nrpt1(UM_MODEL,prompt,UD_ncl_offcv,cv1_lab,
			nrpt_lab,&relnum1,&e1->key);
		if (numint != NCL_OKINPUT) break;
		if (UM_CURVE_CLASS != uc_super_class(relnum1))
		{
			uu_uerror0(/*curve not picked*/UM_MODEL,142);
			continue;
		}
		while (UU_TRUE)
		{
/*
.....If not autolabel, then get label
*/
			if (!first)
			{
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
				if (status == NCL_DONE) return(0);
			}
			first = UU_FALSE;

			prompt = 139;
			numint = ncl_add_label_nrpt1(UM_MODEL,prompt,UD_ncl_offcv,cv2_lab,
				nrpt_lab,&relnum2,&e2->key);
			if (numint != NCL_OKINPUT) break;

			if (UM_CURVE_CLASS != uc_super_class(relnum2))
			{
				uu_uerror0(/*curve not picked**/UM_MODEL,142);
				continue;
			}
			if (strcmp(cv1_lab,cv2_lab) == 0)
			{
				uu_uerror0(/*same curve picked for both curves*/UM_MODEL,79);
				continue;
			}
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
			ncl_add_token(&cmdbuf,intof3,NCL_comma);

/*
..... no near point for 2 lines
*/
			if (relnum1 == UM_LINE_REL && relnum2 == UM_LINE_REL)
			{
				ncl_add_token(&cmdbuf,cv1_lab,NCL_comma);
				ncl_add_token(&cmdbuf,cv2_lab,NCL_nocomma);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
			}
/*
..... else add near point
*/
			else
			{
				ncl_add_token(&cmdbuf,cv1_lab,NCL_comma);
				ncl_add_token(&cmdbuf,cv2_lab,NCL_comma);

/*
.....Get curve data and xform matrices
*/
				status = ncl_retrieve_data_fixed(e1);
				status = uc_retrieve_transf(e1->key, tfmat1);
				status = ncl_retrieve_data_fixed(e2);
				status = uc_retrieve_transf(e2->key, tfmat2);

/*
.....Get the intersection number of points
*/
				ierr = 0;
				nint = 1;
				ncl_isect_3d(e1,tfmat1,e2,tfmat2,UU_FALSE,nrpt,&nint,ibuff,&ierr);

				choice = 1;
				status = NCL_OKINPUT;
				if (nint > 1 && ifirst == 0)	
				{
					status = ncl_popup(NCL_CC_INTOF,&choice);
					ifirst++;
				}

				if (status == NCL_OKINPUT)
				{
/*
..... second curve near point is used
*/
					if (choice == 1)
						ncl_add_token(&cmdbuf,nrpt_lab,NCL_nocomma);
					else if (choice == 2)
						ncl_add_token(&cmdbuf,"ALL",NCL_nocomma);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
	}
	ud_unlimit();
	uu_dexit;
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_on_cv()
**			Create point along a curve using percentage arc length as
**			a parameter value.
**    PARAMETERS   
**       INPUT  : 
**          lpv  - create a pointvector iff 1
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_on_cv(lpv)
int lpv;
	{
	int i,numint,lparam;
	int status;
	int bagsize;
	UM_PLOCREC pick;
	struct UC_entitydatabag *e;
	UU_LOGICAL first;
	NCL_cmdbuf cmdbuf;
	UD_SCA_VALUE u;
	int pick_mask[UD_NMENTWD], *mask;

	uu_denter(UU_MTRC,(us,"umu_c1_pt_on_crv_at_pal()"));

	bagsize = sizeof(struct UC_entitydatabag);
	e = (struct UC_entitydatabag *)  uu_malloc(bagsize);
/*
.....why remove the pick filter? (even though can't just
.....use UD_allcurvess;
.....added filter: all curves, lines and circles
.....Yurong 9/6/06
*/
/*	ud_lgeo(UU_TRUE, UD_allcurvess);*/
	for (i=0; i<UD_NMENTWD; i++)
	{
		pick_mask[i] = 0;
/*
.....UD_allcurvess include lines and circle
*/
		pick_mask[i] = pick_mask[i] | UD_ncl_allcv[i] | UD_ncl_ln[i]
							| UD_ncl_ci[i];
	}
	mask = (int *)pick_mask;
	ud_lgeo(UU_TRUE, mask);

	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
		first = UU_TRUE;
		lparam = 1;
/*
.....Get curve
*/
		um_dl_pldas(UD_DASPCKLOC, /*pick curve to evaluate */ UM_MODEL, 326,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;

		e->key = um_get_pickkey(&pick.pent, 1);
		ur_retrieve_data_relnum(e->key, &(e->rel_num));
		if (UM_CURVE_CLASS != uc_super_class(e->rel_num))
			uu_uerror0(/* curve not picked */UM_MODEL,142);
		else
		{
			while (UU_TRUE)
			{
/*
.....If not autolabel, then get label
*/
				if (!first)
				{
					status = NCL_OKINPUT;
					if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
					if (status == NCL_DONE) goto done;
				}
				else
				{
					if (e->rel_num == UM_CIRCLE_REL || e->rel_num == UM_LINE_REL)
						lparam = 1;
					else
					{
						numint = ncl_popup(NCL_PERCNT, &lparam);
						if (numint == NCL_DONE) break;
						if (numint == NCL_NOINPUT) lparam = 1;
					}
				}

				first = UU_FALSE;
				ud_ldas(UD_SCAUNITLESS, /* enter u arclength parameter on curve */
					UM_MODEL, 323, &u, 1, &numint, 2);
				if (numint <= 0) break;
/*
....Create command
*/
				ncl_set_cmdmode(UU_TRUE);
				if (lpv == 1)
					ncl_add_token(&cmdbuf,NCL_pv,NCL_nocomma);
				else
					ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
				ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
				ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
				if (lparam == 2)
					ncl_add_token(&cmdbuf,NCL_percnt,NCL_comma);
				ncl_add_token(&cmdbuf,u.label,NCL_nocomma);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);

			}
		}
	}
done:;
	uu_free(e);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_dist_cv()
**			Create point along a curve using a distance.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_dist_cv()

	{
	int i,numint;
	int status,ptstat;
	UM_PLOCREC pick;
	struct UC_entitydatabag e;
	UU_LOGICAL first;
	NCL_cmdbuf cmdbuf;
	char buf[80],label[80];
	int pick_mask[UD_NMENTWD], *mask;
/*
.....Limit pick to curves,line and circle
*/
	for (i=0; i<UD_NMENTWD; i++)
	{
		pick_mask[i] = 0;
		pick_mask[i] = pick_mask[i] | UD_ncl_allcv[i] | UD_ncl_ln[i]
							| UD_ncl_ci[i];
	}
	mask = (int *)pick_mask;

	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
		first = UU_TRUE;
/*
.....Get curve
*/
		ud_lgeo(UU_TRUE, mask);
		um_dl_pldas(UD_DASPCKLOC, /*pick curve to evaluate */ UM_MODEL, 326,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;

		e.key = um_get_pickkey(&pick.pent, 1);
		ur_retrieve_data_relnum(e.key, &(e.rel_num));
		if (UM_CURVE_CLASS != uc_super_class(e.rel_num) &&
			e.rel_num != UM_LINE_REL && e.rel_num != UM_CIRCLE_REL)
			uu_uerror0(/* curve not picked */UM_MODEL,142);
		else
		{
/*
........Get optional start point
*/
			ptstat = ncl_get_dlabel(UD_DASPCKLOC,label,225,UD_ncl_pt);
			while (UU_TRUE)
			{
/*
.....If not autolabel, then get label
*/
				if (!first)
				{
					status = NCL_OKINPUT;
					if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
					if (status == NCL_DONE) goto done;
				}
				first = UU_FALSE;
/*
.....Get distance along curve
..... EV 01/23/2001. the commented code divided distance by 25.4 for MM,
..... which was wrong - FSR 60379.
				ud_ldas(UD_DASDISTANCE, UA_NCL, 224, &u, 1, &numint, 2);
				if (numint <= 0) break;
*/
				status = ncl_get_str(buf,224);
				if (status != NCL_OKINPUT) break;
/*
....Create command
*/
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
				ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
				if (ptstat == NCL_OKINPUT)
				{
					ncl_add_token(&cmdbuf,buf,NCL_comma);
					ncl_add_token(&cmdbuf,label,NCL_nocomma);
				}
				else
				{
					ncl_add_token(&cmdbuf,buf,NCL_nocomma);
				}
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
			}
		}
	}
done:;
	uu_free(&e);
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_on_sf()
**			Create point along a surface using percentage arc length as
**			a parameter value.
**    PARAMETERS   
**       INPUT  : 
**          lpv  - create a pointvector iff 1
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_on_sf(lpv)
int lpv;
	{
	struct UC_entitydatabag *e;
	struct UM_evsrfout *evsrf;
	UM_PLOCREC pick;
	int numint,lparam;
	int status = UU_SUCCESS;
	int bagsize;
	UU_LOGICAL first;
	NCL_cmdbuf cmdbuf;
	char buf[80];
	UD_SCA_VALUE ua, va;

	uu_denter(UU_MTRC,(us,"umu_c1_pt_on_srf_at_pal()"));

	bagsize = sizeof(struct UC_entitydatabag);
	e = (struct UC_entitydatabag *)  uu_malloc(bagsize);
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));

	ud_lgeo(UU_TRUE, UD_surface);

/*	ur_setup_data(UM_POINT_REL, &p, sizeof(p));*/
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
/*	strcpy (p.label, "");*/
/*	p.subscr = 0;*/
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
		first = UU_TRUE;
		lparam = 1;

		um_dl_pldas(UD_DASPCKLOC, /* pick surface to evaluate */ UM_MODEL, 327,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;

		e->key = um_get_pickkey(&pick.pent, 1);
		uc_retrieve_data(e, bagsize);
	
		while (UU_TRUE)
		{
/*
.....If not autolabel, then get label
*/
			if (!first)
			{
				status = NCL_OKINPUT;
				if (!NCL_auto_label)
				status = ncl_add_name(&cmdbuf,1);
				if (status == NCL_DONE) goto done;
			}
			else
			{
				numint = ncl_popup(NCL_PERCNT, &lparam);
				if (numint == NCL_DONE) break;
				if (numint == NCL_NOINPUT) lparam = 1;
			}

			first = UU_FALSE;

			ud_ldas(UD_SCAUNITLESS, /* enter u arclength parameter on surface */
				UM_MODEL, 324, &ua, 1, &numint, 2);
			if (numint <= 0) goto repeat;
	
			ud_ldas(UD_SCAUNITLESS, /* enter v arclength parameter on surface */
				UM_MODEL, 325, &va, 1, &numint, 2);
			if (numint <= 0) goto repeat;
/*
....Create command
*/
				ncl_set_cmdmode(UU_TRUE);
				if (lpv == 1)
					ncl_add_token(&cmdbuf,NCL_pv,NCL_nocomma);
				else
					ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
				ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
				ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
				if (lparam == 2)
					ncl_add_token(&cmdbuf,NCL_percnt,NCL_comma);
				strcpy(buf, ua.label);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
				strcpy(buf, va.label);
				ncl_add_token(&cmdbuf,buf,NCL_nocomma);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
		}
repeat:;
	}
done:;
	uu_free(e);
	uu_free(evsrf);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_endpt()
**       Create a point as the end point of a line/pv.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_endpt()
{
	int numint,i;							/* number of DAS items returned */
	struct UM_entitydatabag e;
	int status;
	UM_isect ibuff[2];
	UM_PLOCREC pick;
	NCL_cmdbuf cmdbuf;
	char lmod[8];
	int closest;
	UM_transf tfmat;
	int pick_mask[UD_NMENTWD], *mask;

/*
.....Limit picking to Lines, Circles, PointVecs and Curves
*/
	for (i=0; i<UD_NMENTWD; i++)
	{
		pick_mask[i] = 0;
		pick_mask[i] = pick_mask[i] | UD_ncl_lncipv[i];
		pick_mask[i] = pick_mask[i] | UD_ncl_allcv[i];
	}
	mask = (int *)pick_mask;
	ud_lgeo(UU_TRUE, mask);
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
/*
.....Get entity
*/
		ua_dl_pldas(UD_DASPCKLOC,/*pick entity*/UA_NCL,14,&pick,1,&numint,1);
		if (numint <= 0) goto done;
/*
.....Get the 2 end points
*/
		e.key = um_get_pickkey(&pick.pent, 1);
		status = ncl_retrieve_data_fixed(&e);
		if (status != UU_SUCCESS) goto repeat;
		status = uc_retrieve_transf(e.key, tfmat);
		if (status != UU_SUCCESS) goto repeat;
		um_get_endpts(&e,tfmat,ibuff[0].pt,ibuff[1].pt);
/*
.....Calculate the modifier
*/
		closest = um_nearest_isect_to_ploc(&pick.ploc,2,ibuff);
		um_set_modsys_refsys();
		um_trim_modifier(ibuff,2,closest,lmod);
/*
........Output command
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
		ncl_add_token(&cmdbuf,lmod,NCL_comma);
		ncl_add_token(&cmdbuf,NCL_endpt,NCL_comma);
		ncl_add_token(&cmdbuf,pick.ploc.label,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
repeat:;
	}
done:;
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_te()
**       Create a point as the end point of the tool.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_te()
{
	NCL_cmdbuf cmdbuf;
	int status;
/*
.....Initialize the command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
	status = NCL_OKINPUT;
	if (!NCL_auto_label)
		status = ncl_add_name(&cmdbuf,1);
	if (status == NCL_DONE) goto done;
/*
........Output the command
*/
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
	ncl_add_token(&cmdbuf,NCL_te,NCL_nocomma);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
done:;
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_ci_atangl()
**       Create a point at an angle on a circle.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_ci_atangl()
{
	int numint;							/* number of DAS items returned */
	int status;
	UM_PLOCREC pick;
	NCL_cmdbuf cmdbuf;
	char str[80];
	UU_LOGICAL first;
	UU_REAL anyreal;
	UD_SCA_VALUE angle;

/*
.....Limit the geometry pick
*/
	ud_lgeo(UU_TRUE, UD_ncl_ci);
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
		first = UU_TRUE;
/*
.....Get circle
*/
		ua_dl_pldas(UD_DASPCKLOC,/*pick circle*/UA_NCL,4,&pick,1,&numint,1);
		if (numint <= 0) goto done;
		while (UU_TRUE)
		{
/*
.....If not autolabel, then get label
*/
         if (!first)
         {
            status = NCL_OKINPUT;
            if (!NCL_auto_label)
               status = ncl_add_name(&cmdbuf,1);
            if (status == NCL_DONE) goto done;
         }
         first = UU_FALSE;
/*
........Get angle
*/
			ud_ldas(UD_SCAANGLE,UA_NCL, 5, &angle, 1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			if (ul_to_reals(&anyreal,&numint,1,angle.label) != UU_SUCCESS)
				strcpy(str,angle.label);
			else
			{
				anyreal = angle.value*180.0/UM_PI;
				ncl_sprintf(str,&anyreal,1);
			}
/*
.....Place point definition into source file
*/
			ncl_set_cmdmode(UU_TRUE);
			ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
			ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
			ncl_add_token(&cmdbuf,NCL_atangl,NCL_comma);
			ncl_add_token(&cmdbuf,str,NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
repeat:;
	}
done:;
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_pt_ci_angle()
**       Create a point at an angle to a point on a circle.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_pt_ci_angle()
{
	int numint;							/* number of DAS items returned */
	int status;
	UM_PLOCREC pick;
	NCL_cmdbuf cmdbuf;
	char str[80],label[80];
	UU_LOGICAL first;
	UU_REAL anyreal;
	UD_SCA_VALUE angle;
/*
.....Limit the geometry pick
*/
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
		first = UU_TRUE;
/*
.....Get the circle
*/
		ud_lgeo(UU_TRUE, UD_ncl_ci);
		ua_dl_pldas(UD_DASPCKLOC,/*pick circle*/UA_NCL,4,&pick,1,&numint,1);
		if (numint <= 0) goto done;
		while (UU_TRUE)
		{
/*
.....If not autolabel, then get label
*/
         if (!first)
         {
            status = NCL_OKINPUT;
            if (!NCL_auto_label)
               status = ncl_add_name(&cmdbuf,1);
            if (status == NCL_DONE) goto done;
         }
/*
........Get the point
*/
			status = ncl_get_dlabel(UD_DASPCKLOC,label,9,UD_ncl_ptpv);
			if (status == NCL_DONE || status == NCL_NOINPUT) goto again;
/*
........Get angle
*/
			while (UU_TRUE)
			{
/*
.....If not autolabel, then get label
*/
				if (!first)
				{
					status = NCL_OKINPUT;
					if (!NCL_auto_label)
					status = ncl_add_name(&cmdbuf,1);
					if (status == NCL_DONE) goto done;
				}
				first = UU_FALSE;
				ud_ldas(UD_SCAANGLE,UA_NCL, 5, &angle, 1, &numint, UD_NODEFAULT);
				if (numint <= 0) goto repeat;
				if (ul_to_reals(&anyreal,&numint,1,angle.label) != UU_SUCCESS)
					strcpy(str,angle.label);
				else
				{
					anyreal = angle.value*180.0/UM_PI;
					ncl_sprintf(str,&anyreal,1);
				}
/*
.....Place point definition into source file
*/
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
				ncl_add_token(&cmdbuf,label,NCL_comma);
				ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
				ncl_add_token(&cmdbuf,str,NCL_nocomma);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
			}
repeat:;
		}
again:;
	}
done:;
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_ce_ci()
**       Create a point at the center of a circle.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_ce_ci()
{
	int numint;							/* number of DAS items returned */
	int status;
	UM_PLOCREC pick;
	NCL_cmdbuf cmdbuf;
/*
.....Limit the geometry pick
*/
	ud_lgeo(UU_TRUE, UD_ncl_ci);
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
/*
.....If not autolabel, then get label
*/
		status = NCL_OKINPUT;
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf,1);
		if (status == NCL_DONE) goto done;
/*
.....Get circle
*/
		ua_dl_pldas(UD_DASPCKLOC,/*pick circle*/UA_NCL,17,&pick,1,&numint,1);
		if (numint <= 0) goto done;
/*
.....Place point definition into source file
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
		ncl_add_token(&cmdbuf,NCL_center,NCL_comma);
		ncl_add_token(&cmdbuf,pick.ploc.label,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_patern()
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
int nclu_pt_patern()
{
	NCL_cmdbuf cmdbuf;
	int status,num,numint;
	char str[80];
	UM_PLOCREC pick;

	uu_denter(UU_MTRC,(us,"nclu_pt_patern"));
/*
....Limit geometry picks
*/
	ud_lgeo(UU_TRUE, UD_ncl_patern);
	while (UU_TRUE)
	{
/*
.....Initialize the NCL command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;
/*
.....If not autolabel, then get label
*/
		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);
		if (status == NCL_DONE) goto done;
/*
.....Get Patern
*/
		ua_dl_pldas(UD_DASPCKLOC,/*pick circle*/UA_NCL,450,&pick,1,&numint,1);
		if (numint <= 0) goto done;
		num = um_get_pickkey(&pick.pent,2);
		sprintf(str,"%d",num);
/*
.....Place point definition into source file
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
		ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
		ncl_add_token(&cmdbuf,str,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_proj_sf()
**       Projects a point onto a surface.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pt_proj_sf()
{
	int numint, prompt, projvec, relnum, i;
	NCL_cmdbuf cmdbuf;
	char pt_label[80], projvec_lab[80], surf_label[80], nrpt_lab[80];
 	static char projct_lab [7] = "PROJCT";
	UM_PLOCREC pick;
	int pick_mask[UD_NMENTWD], *mask;

/*
.....Limit picking to solids, surfaces and planes
*/
	for (i=0; i<UD_NMENTWD; i++)
	{
		pick_mask[i] = 0;
		pick_mask[i] = pick_mask[i] | UD_solid[i];
		pick_mask[i] = pick_mask[i] | UD_ncl_allsfpl[i];
	}
	mask = (int *)pick_mask;
	
	while (UU_TRUE)
	{
/*
..... Have user pick surface to project onto.  Get label of sf.
*/
		prompt = 711;		/* Pick a solid or surface or plane */	
		numint = ncl_add_label_nrpt(prompt, mask, surf_label,
			nrpt_lab, &relnum);
		if (numint != NCL_OKINPUT) break;
/*
..... Have user pick projection vector (optional).  Get label.
*/
		prompt = 61;		/* Enter vector to project along (optional). */
		ud_lgeo (UU_TRUE, UD_ncl_vepv);
		ua_dl_pldas(UD_DASPCKLOC, UM_APPGEO, 61, &pick, 1, &projvec, 1);
		if (projvec > 0)
			strcpy (projvec_lab, pick.ploc.label);
		while (UU_TRUE)
		{
/*
.....Initialize the NCL command buffer
*/
			ncl_init_cmdbuf(&cmdbuf);
/*
..... Have user pick a point/pointvector.  Get label. If none, stop loop.
*/
			prompt = 216;	/* Pick a point */
			numint = ncl_get_dlabel(UD_DASPCKLOC, pt_label, prompt, UD_ncl_ptpv);
			if (numint != NCL_OKINPUT) break;
/*
.....Place point definition into source file
.....Format is   pt/projct,point(,projection vector),surface(,nearpt)
.....The things in parentheses are optional.
*/
			ncl_set_cmdmode (UU_TRUE);
			ncl_add_token(&cmdbuf,NCL_pt,NCL_nocomma);
			ncl_add_token(&cmdbuf,projct_lab,NCL_comma);
			ncl_add_token(&cmdbuf, pt_label ,NCL_comma);
			if(projvec == 1)
				ncl_add_token (&cmdbuf, pick.ploc.label, NCL_comma);
			ncl_add_token(&cmdbuf, surf_label ,NCL_comma);
			if (relnum != NCL_PLN_REL)
				ncl_add_token(&cmdbuf, nrpt_lab, NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
	}
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : ncl_replace_cordstr(str, substr, n)
**       replace a cordinate sub-string within a cordinate string(x_str,y_str,z_str)
**    PARAMETERS   
**       INPUT  : 
**          str: a cordinate string("x_str,y_str,z_str")
**			substr: the substr will be insert in
**			n:		x_str: 1	y_str: 2	z_str: 3
**       OUTPUT :  
**          str: changed cordinate string
**    RETURNS      : -1: failed 
**					0: OK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_replace_cordstr(str, substr, n)
char *str, *substr;
int n;
{
	char *p, tempstr1[256], tempstr2[256], str1[80], str2[80], str3[80];
	strcpy(tempstr1, str);
	tempstr2[0] = '\0';

	str1[0] = str2[0] = str3[0] = '\0';
	p = (char*)strchr (tempstr1, ',');
	if (p==NULL) return -1;
	*p = 0;
	strcpy (str1, tempstr1);
	strcpy(tempstr2, p+1);

	p = (char*)strchr (tempstr2, ',');
	if (p==NULL)
	{
		strcpy(str2, tempstr2);
	}
	else
	{
		*p = 0;
		strcpy(str3, p+1);
		strcpy (str2, tempstr2);
	}
	if (n==1)
		sprintf (str, "%s, %s, %s", substr, str2, str3);
	else if (n==2)
		sprintf (str, "%s, %s, %s", str1, substr, str3);
	else if (n==3)
		sprintf (str, "%s, %s, %s", str1, str2, substr);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_fit()
**       create a point at the center of a  cv/sf
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pt_fit()
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
.....Put PT/ into command.
*/
		ncl_set_cmdmode(UU_TRUE);
		status = ncl_add_token(&cmdbuf, NCL_pt, NCL_nocomma);
/*
.....Put CENTER into command.
*/
		status = ncl_add_token(&cmdbuf, NCL_center, NCL_comma);
/*
.....Put picked entity into command.
*/
		ncl_add_token(&cmdbuf,pick.ploc.label,NCL_comma);
		if ((status == NCL_NOINPUT)) goto done;
/*
.....Process command.
*/
		ncl_set_cmdmode(&cmdbuf);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
done:;
   uu_dexit;
	return 0;
}
