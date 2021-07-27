/*********************************************************************
**    NAME         :  m3upt.c
**       CONTAINS: User interface routines for point creation
**			umu_c1_pt()
**			umu_c1_deltapt()
**			umu_c1_lpt()
**			umu_c1_cpt()
**			umu_c1_ipcpc()
**			umu_m1_modpt()
**			umu_c1_nearpt()
**			umu_c1_pt_on_crv_at_pal()
**			umu_pt_along_crv_at_u(eptr)
**			umu_c1_pt_on_srf_at_pal()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3upt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:59
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
#include	"mdpick.h"
#include	"misect.h"
#include	"mdeval.h"
#include "view.h"

#include "go.h"				/* temporary */
#include "mdattr.h"			/* temporary */

#define  UM_maxrepetition 1024 

/*********************************************************************
**    E_FUNCTION     : umu_c1_pt()
**       Create a single point.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c1_pt()
	{
	struct UM_point_rec e;              /* point  entity structure */
	int numint,j;									/* number of interactions */
    UD_NDCLOCREC tmp;

	uu_denter(UU_MTRC,(us,"umu_c1_pt()"));

	ur_setup_data(UM_POINT_REL, &e, sizeof(struct UM_point_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;
	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART,/*coordinate of point*/UM_MODEL, 35, &tmp, 
				 1, &numint, UD_NODEFAULT);
        for(j=0; j<3; j++) e.pt[j] = tmp.cord[j];

		if (numint <= 0) goto done;
		um_create_pt1(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uc_display(&e);
		}
done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_c1_deltapt()
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
umu_c1_deltapt()
	{
	struct UM_point_rec e;               /* point  entity structure */
/**	UM_coord basept; **/
    UD_NDCLOCREC basept;

	UM_vector delta;
	int numint;									/* number of interactions */

	uu_denter(UU_MTRC,(us,"umu_c1_deltapt()"));

	ur_setup_data(UM_POINT_REL, &e, sizeof(struct UM_point_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;
	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART,/*indicate base point */UM_MODEL, 248, &basept, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		while (UU_TRUE)
			{
			ud_ldas(UD_DASVEC,/*enter offset delta*/UM_MODEL, 249, delta, 
				1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			um_vcplvc(&basept, delta, e.pt);
			um_create_pt1(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&e);
			}
repeat:;
		}
done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_c1_lpt()
**       Create a linear pattern of points.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c1_lpt()

	{
	struct UM_point_rec *e;			/* point  entity structure */
	UM_vector delta;					/* delta x,y,z */
/**	UM_coord base_pt;					/* coordinates of base point */
    UD_NDCLOCREC base_pt;

	UM_coord pt;						/* coordinates of new point */
	int npts;							/* number of points in pattern */
	int i, j, savj;					/* indicies */
	int numint;							/* number of interactions */
	UM_vector zero;					/* zero vector */
	UU_LOGICAL ok;						/* npts in legal range */
	UU_LOGICAL ud_lyesno();
	char *msg1, *msg2, msg[10000];

	uu_denter(UU_MTRC,(us,"umu_c1_lpt()"));
	
	e = (struct UM_point_rec *)
				uu_malloc(UM_maxrepetition*sizeof(struct UM_point_rec));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART,/* Indicate base point */UM_MODEL,248,
			&base_pt,1,&numint,UD_NODEFAULT);
		if (numint <= 0) goto done;
		while (UU_TRUE)
			{
			ud_ldas(UD_DASVEC,/* Enter offset deltas */UM_MODEL,249,
				delta, 1,&numint,UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			ud_ldas(UD_DASINT,/*number of points*/UM_MODEL,38,&npts,1,
				&numint,	UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			for (j=0; j<3; j++) zero[j] = 0.0;
			if (um_cceqcc(delta,zero))
				uu_uerror0(/*can not have zero delta vector*/UM_MODEL,76);
			else
				{
				if (npts <= UM_maxrepetition) ok = UU_TRUE;
				else
					{
/*					uu_uerror2(UM_MODEL,120,npts,UM_maxrepetition); 
					/* message is: npts exceeds max repetition */
/*					ok = ud_lyesno(UM_MODEL,88); /* do you want to continue */
					msg1 = (char*)uu_uprompt2(UM_MODEL,120,npts,UM_maxrepetition);
					msg2 = (char*)uu_uprompt0(UM_MODEL,88);
					sprintf(msg, "%s\n%s",msg1, msg2);
					ok = ud_yesno(0, msg, "Question?");
					}
				if (ok)
					{
					j = 0;
					um_vctovc(&base_pt, pt);
					um_vcplvc(pt, delta, pt);
					while (j < npts)
						{
						i = 0;
						savj = j;
						/* do UNIBASE creates */
						while ((i< UM_maxrepetition) && (j < npts))
							{
							ur_setup_data(UM_POINT_REL, &(e[i]), 
												sizeof(struct UM_point_rec));
							/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
							strcpy (e[i].label, "");
							e[i].subscr = 0;
							um_vctovc(pt,(e[i].pt));
							um_vcplvc(pt,delta,pt);/* update delta for next call */
							um_create_pt1(&(e[i]), UM_DEFAULT_TF, UM_CURRENT_ATTR);
							j++;
							i++;
							}
						i = 0;
						j = savj;
						/* do displays */
						while ((i < UM_maxrepetition) && (j < npts))
							{
							uc_display(&(e[i])); 
							j++;
							i++;
							}
						}
					}/* do linear pattern */
				}/* delta ok */
			}
repeat:;
		}
done:;
	uu_free(e);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_c1_cpt()
**       Create a circular pattern of points about a circle defined
**       by the user.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c1_cpt()

	{
	struct UM_point_rec p;					/* point  entity structure */
/**	UM_coord center;							/* center point of pattern */
    UD_NDCLOCREC center;

	UU_REAL radius;							/* radius of circle */
/**	UM_coord pt;								/* coordinates of point */
    UD_NDCLOCREC pt;

	UM_coord npt;								/* projection of point onto plane */
	UU_REAL dang;								/* angle between points */
	UU_REAL ang;								/* angle */
	UM_vector xcomp, ycomp;					/* x and y component of point 
														relative to circle center */
	UU_REAL xlen, ylen;						/* length of x and y components */
	UM_vector xaxis;							/* "x" axis of circle */
	UM_vector yaxis;							/* "y" axis of circle */
	int numint;									/* number of das entries */
	int npts;									/* number of points in pattern */
	int j;										/* indicies */
	UU_LOGICAL ok;
	UU_LOGICAL ud_lyesno();
	UU_REAL um_dcccc();
	char *msg1, *msg2, msg[10000];

	uu_denter( UU_MTRC,(us,"umu_c1_cpt()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART,/*circle center*/UM_MODEL, 14, &center, 
			1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		while (UU_TRUE)
			{
			ud_ldas(UD_DASCART,/*starting point*/UM_MODEL, 36, &pt, 1, 
				&numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			ud_ldas(UD_DASANGLE, /*angle between points*/UM_MODEL, 41, &dang,
				1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			ud_ldas(UD_DASINT, /*number of points in pattern*/UM_MODEL, 38, 
				&npts, 1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			if (um_cceqcc(&center,&pt))
				uu_uerror0(/*center and starting point identical*/UM_MODEL,77);
			else
				{
				if (npts <  UM_maxrepetition) ok = UU_TRUE;
				else
					{
/*					uu_uerror2( UM_MODEL,120,npts,UM_maxrepetition);	/* npts exceeds max repetition */
/*					ok = ud_lyesno( UM_MODEL,88); /* answer yes if ok */
					msg1 = (char*)uu_uprompt2(UM_MODEL,120,npts,UM_maxrepetition);
					msg2 = (char*)uu_uprompt0(UM_MODEL,88);
					sprintf(msg, "%s\n%s",msg1, msg2);
					ok = ud_yesno(0, msg, "Question?");
					}
				if (ok)
					{
					um_nptpln(&pt,&center,UM_cpln.zaxis,npt);
					ang = 0.0;
					radius = um_dcccc(&center,npt);
					if ( radius < UM_FUZZ)
						uu_uerror0(/*radius of circular array of points too small*/UM_MODEL,78);
					else
						{
						um_vcmnvc(npt,&center,xaxis);
						um_unitvc(xaxis,xaxis);
						um_cross( UM_cpln.zaxis,xaxis,yaxis);
						um_unitvc(yaxis,yaxis);
						for (j = 0; j < npts; j++)
							{
							ur_setup_data(UM_POINT_REL, &p, sizeof(struct UM_point_rec));
							/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
							strcpy (p.label, "");
							p.subscr = 0;
							xlen = radius * cos(ang);
							um_vctmsc(xaxis, xlen, xcomp);
							ylen = radius * sin(ang);
							um_vctmsc(yaxis, ylen, ycomp);
							um_vcplvc(xcomp, ycomp, p.pt);
							um_vcplvc(&center, p.pt, p.pt);
							um_create_pt1(&p, UM_DEFAULT_TF, UM_CURRENT_ATTR);
							uc_display(&p);
							ang = ang + dang;
							}
						}
					}
				}
			}
repeat:;
		}
done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_c1_ipcpc()
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
umu_c1_ipcpc()

	{
	int numint;							/* number of DAS items returned */
	struct UM_crvdatabag *e1;	/* first curve */
	struct UM_crvdatabag *e2;	/* second curve */
	struct UM_point_rec e;			/* point  entity */
	UM_transf tfmat1;					/* transformation for first curve */
	UM_transf tfmat2;					/* transformation for second curve */
	int i, j;							/* indicies */
	int nint;							/* number of intersection points */
	int status;
	UM_isect ibuff[UM_MAXISECT];		/* intersection points */
	UM_PLOCREC pick;					/* pick information */
   UU_REAL Umin = -UM_FUZZ;
   UU_REAL Umax = 1. + UM_FUZZ;

	uu_denter(UU_MTRC,(us,"umu_c1_ipcpc()"));

	e1 = (struct UM_crvdatabag *) uu_malloc(sizeof(struct UM_crvdatabag));
	e2 = (struct UM_crvdatabag *) uu_malloc(sizeof(struct UM_crvdatabag));

	while (UU_TRUE)
		{
		um_dl_pdas(UD_DASPICK,/*pick first curve*/UM_MODEL,138,&pick,1,&numint,1);
		if (numint <= 0) goto done;
		e1->key = um_get_pickkey(&pick.pent, 1);
		ur_retrieve_data_relnum(e1->key, &(e1->rel_num));
		if (UM_CURVE_CLASS != uc_super_class(e1->rel_num))
			{
			uu_uerror0(/*curve not picked*/UM_MODEL,142);
			goto repeat;
			}
		while (UU_TRUE)
			{
			um_dl_pdas(UD_DASPICK,/*pick second curve*/UM_MODEL,139,&pick,1,&numint,1);
			if (numint <= 0) goto repeat;
			e2->key = um_get_pickkey(&pick.pent, 1);
			ur_retrieve_data_relnum(e2->key, &(e2->rel_num));
			if (UM_CURVE_CLASS != uc_super_class(e2->rel_num))
				{
				uu_uerror0(/*curve not picked**/UM_MODEL,142);
				}
			else
				{
				if (e1->key == e2->key)
					uu_uerror0(/*same curve picked for both curves*/UM_MODEL,79);
				else
					{
					status = ncl_retrieve_data_fixed(e1);
					if (status != UU_SUCCESS) goto repeat;
					status = uc_retrieve_transf(e1->key, tfmat1);
					if (status != UU_SUCCESS) goto repeat;
					status = ncl_retrieve_data_fixed(e2);
					if (status != UU_SUCCESS) goto repeat;
					status = uc_retrieve_transf(e2->key, tfmat2);
					if (status != UU_SUCCESS) goto repeat;
					status = uc_crv_intersect(e1, tfmat1, e2, tfmat2,
						&nint, UM_MAXISECT, ibuff);
					if ((status != UU_SUCCESS) || (nint == 0))
						uu_uerror0(/*no intersection found*/UM_MODEL,80);
					else
					  {
                  j = 0;
						for (i = 0; i < nint; i++)
						  {
                     if (ibuff[i].u0 > Umin && ibuff[i].u1 > Umin &&
                         ibuff[i].u0 < Umax && ibuff[i].u1 < Umax) 
                       {
						   	ur_setup_data(UM_POINT_REL, &e, sizeof(struct UM_point_rec));
							   /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
							   strcpy (e.label, "");
							   e.subscr = 0;
							   um_vctovc(ibuff[i].pt,e.pt);
							   um_create_pt1(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
							   uc_display(&e);
                        j++;
                       }
						  }
                  if (j == 0) uu_uerror0(/*no intersection found*/UM_MODEL,80);
					  }
					}
				}
			}
repeat:;
		}
done:;
	uu_free(e1);
	uu_free(e2);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_m1_modpt()
**      Prompt the user for the point to modify, then change coordinates
**		  and redisplay
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_m1_modpt()
	{
	int  numint,j;							/* number of entities picked */
	UU_KEY_ID key;
	int rel_num;
	struct UM_point_rec e1;
    UD_NDCLOCREC tmp;

	uu_denter(UU_MTRC,(us,"umu_m1_modpt()"));

	ur_setup_data(UM_POINT_REL, &e1, sizeof(struct UM_point_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e1.label, "");
	e1.subscr = 0;
	while (UU_TRUE)
		{
		um_dl_pdas(UD_DASPICK, /*Pick points to modify:*/UM_MODEL, 104, &e1.key, 
			 1,&numint,1);
		if (numint <= 0) goto done;
		ur_retrieve_data_relnum(e1.key, &rel_num);
		if (rel_num == UM_POINT_REL)
			{
			ud_ldas(UD_DASCART, /*New point coordinates: */UM_MODEL, 105, 
				&tmp, 1, &numint, UD_NODEFAULT);
            for(j=0; j<3; j++) e1.pt[j] = tmp.cord[j];

			if (numint <= 0) goto repeat;
			um_update_geom(&e1, UM_DEFAULT_TF);
			uc_display(&e1);
			}
		else
			uu_uerror0(UM_MODEL,130);/* point not picked, re-enter */
repeat:;
		}
done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_c1_nearpt()
**       Prompt the user to pick a curve and create a point at the
**			nearest point to the picked point.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c1_nearpt()

	{
	struct UM_point_rec p; 
	UU_KEY_ID key;
	UM_PLOCREC pick;
	int numint;
	int status;

	uu_denter(UU_MTRC,(us,"umu_c1_nearpt()"));

	ud_lgeo(UU_TRUE, UD_vircurves);

	ur_setup_data(UM_POINT_REL, &p, sizeof(p));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (p.label, "");
	p.subscr = 0;
	while (UU_TRUE)
		{
		um_dl_pldas(UD_DASPCKLOC, /*pick curve near new point */ UM_MODEL, 290,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;
		key = um_get_pickkey(&pick.pent, 2);
		status = uc_near_on_entity(key, &pick.ploc, p.pt);
		if (status == UU_FAILURE)
			uu_outputerr("unable to calculate nearest point on entity");
		else
			{
			um_create_pt1(&p, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&p);
			}
		}
done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_c1_pt_on_crv_at_pal()
**			Create point along a curve using percentage arc length as
**			a parameter value.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c1_pt_on_crv_at_pal()

	{
	int numint;
	int status;
	int bagsize;
	UM_PLOCREC pick;
	struct UC_entitydatabag *e;

	uu_denter(UU_MTRC,(us,"umu_c1_pt_on_crv_at_pal()"));

	bagsize = sizeof(struct UC_entitydatabag);
	e = (struct UC_entitydatabag *)  uu_malloc(bagsize);

	ud_lgeo(UU_TRUE, UD_allcurvess);

	while (UU_TRUE)
		{
		um_dl_pldas(UD_DASPCKLOC, /*pick curve to evaluate */ UM_MODEL, 326,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;

		e->key = um_get_pickkey(&pick.pent, 1);
		uc_retrieve_data(e, bagsize);

		status = uc_pt_on_crv_at_pal(e);

		}
done:;
	uu_free(e);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_pt_along_crv_at_u(eptr)
**			Create points along a curve at logical parameter (U) values. 
**			NOTE: for some curves (LINES, CIRCLES, CONICS), the logical
**			parameter U actually represents percentage arc length.
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to curve entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_pt_along_crv_at_u(eptr)
	struct UC_entitydatabag *eptr;

	{
	struct UM_point_rec p; 
	struct UM_evcrvout *evcrv;
	int numint;
	int status;
	UM_param u;

	uu_denter(UU_MTRC,(us,"umu_pt_along_crv_at_u(key=%x)",
		eptr->key));

	evcrv = (struct UM_evcrvout *) uu_malloc(sizeof(struct UM_evcrvout));

	ur_setup_data(UM_POINT_REL, &p, sizeof(p));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (p.label, "");
	p.subscr = 0;

	while (UU_TRUE)
		{
		ud_ldas(UD_DASUNITLESS, /* enter u arclength parameter on curve */
			UM_MODEL, 323, &u, 1, &numint, 2);
		if (numint <= 0) goto done;

		status = uc_evcrv(UM_POINT, u, eptr, UM_idmat, evcrv);
		if (status != UU_SUCCESS) goto done;
		um_vctovc(evcrv->cp, p.pt);

		um_create_pt1(&p, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uc_display(&p);
		}

done:;
	uu_free(evcrv);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_c1_pt_on_srf_at_pal()
**			Create point along a surface using percentage arc length as
**			a parameter value.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c1_pt_on_srf_at_pal()

	{
	struct UM_point_rec p; 
	struct UC_entitydatabag *e;
	struct UM_evsrfout *evsrf;
	UM_PLOCREC pick;
	int numint;
	int status = UU_SUCCESS;
	int bagsize;
	UM_param ua, va;
	UM_param u, v;

	uu_denter(UU_MTRC,(us,"umu_c1_pt_on_srf_at_pal()"));

	bagsize = sizeof(struct UC_entitydatabag);
	e = (struct UC_entitydatabag *)  uu_malloc(bagsize);
	evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));

	ud_lgeo(UU_TRUE, UD_surface);

	ur_setup_data(UM_POINT_REL, &p, sizeof(p));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (p.label, "");
	p.subscr = 0;
	while (UU_TRUE)
		{
		um_dl_pldas(UD_DASPCKLOC, /* pick surface to evaluate */ UM_MODEL, 327,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;

		e->key = um_get_pickkey(&pick.pent, 1);
		uc_retrieve_data(e, bagsize);
	
		while (UU_TRUE)
			{
			ud_ldas(UD_DASUNITLESS, /* enter u arclength parameter on surface */
				UM_MODEL, 324, &ua, 1, &numint, 2);
			if (numint <= 0) goto repeat;
	
			ud_ldas(UD_DASUNITLESS, /* enter v arclength parameter on surface */
				UM_MODEL, 325, &va, 1, &numint, 2);
			if (numint <= 0) goto repeat;
	
			/*status = uc_altouv(e, ua, va, &u, &v);*/
			u = ua; v = va;
			if (status == UU_SUCCESS)
				status = uc_evsrf(UM_POINT, u, v, e, UM_idmat, evsrf);
			if (status != UU_SUCCESS)
				uu_outputerr("unable to calculate point at specified parameter");
			else
				{
				um_vctovc(evsrf->sp, p.pt);
				um_create_pt1(&p, UM_DEFAULT_TF, UM_CURRENT_ATTR);
				uc_display(&p);
				}
			}
repeat:;
		}
done:;
	uu_free(e);
	uu_free(evsrf);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_c1_test(option)
**       Create a linear pattern of points for testing UNIBASE and
**			DIGS.
**    PARAMETERS   
**       INPUT  : 
**          option					0 => create 1024 points in UNIBASE
**          							1 => draw 1024 points in DIGS
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c1_test(option)
	int option;

	{
	struct UM_circle_rec e;			/* point  entity structure */
	UM_vector delta;					/* delta x,y,z */
	UM_coord pt;						/*  coordinates of point */
	int npts;							/* number of points in pattern */
	int j;								/* indicies */

	UM_transf tfmat;
	int dsegid;             		/* display segment identifier */
	uv_segbuff(buffer);				/* user defined display segment data */
	int gnseg();

	uu_denter(UU_MTRC,(us,"umu_c1_test(%d)",option));

	um_pscroll("umu_c1_test is not implemented");
	
	uu_dexit;
	}
