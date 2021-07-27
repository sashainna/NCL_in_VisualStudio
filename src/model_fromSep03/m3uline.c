/*********************************************************************
**    NAME         :  m3uline.c
**       CONTAINS: user interface routines to create a line
**			umu_c2_pp()
**			umu_c2_pt()
**			umu_c2_tt()
**			umu_c2_parto()
**			umu_c2_parpt()
**			umu_c2_angle()
**			umu_c2_ptveclen()
**			umu_c2_connected()
**			umu_m2_chamfer()
**			umu_m2_modline(option)
**			umu_c2_tancrv_at_pt()
**			umu_c2_normcrv_at_pt()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3uline.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:59
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "drubber.h"
#include "dselmask.h"
#include "mfort.h"
#include "mdclass.h"
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mdpick.h"
#include "mattr.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : umu_c2_pp()
**       Prompt the user for two points and create a line.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c2_pp()
	{
	struct UM_line_rec e;
	int status;
	int numint;
/**	UM_coord spt; **/
    UD_NDCLOCREC spt;

/**	UM_coord ept; **/
    UD_NDCLOCREC ept;

	UD_RUBBER rubber;			/* rubber band control block */
	 
	uu_denter(UU_MTRC,(us,"umu_c2_pp()"));

	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 0.0, &spt);

/*	-- see if rubber banding is active -- */

	RUBBER_LINE(&rubber);
	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*start point of new line*/UM_MODEL, 29, &spt, 
				 1, &numint, UD_DEFAULT);
		if (numint <= 0) goto done;

/*	-- see if time to turn on rubber. if input type is not locator,
			then don't. -- */

		RUBBER_ON(&rubber);

		ud_ldas(UD_DASCART, /*end point of line*/UM_MODEL, 30, &ept, 
			 1, &numint, UD_NODEFAULT);

/*	-- restore locator state if r.b. on and current state is locator -- */

		ud_endrub(&rubber);

		if (numint <= 0) goto repeat;
		status = um_c2_pp(&spt, &ept, &e);
		if (status == UU_SUCCESS)
			{
			e.key = 0;
			um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&e);
			}
		um_vctovc(&ept,&spt);
repeat:;
		}
done:
	RUBBER_OFF(&rubber);
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_c2_pt()
**       Create a line through a given point and tangent to a curve
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c2_pt()
	{
	struct UC_entitydatabag *c;/* curve entity data */
	struct UM_line_rec l;      /* line entity data */
	UM_PLOCREC pick;				/* pick record */
	int numint;						/* number of das entries returned */
/**	UM_coord pt; **/
    UD_NDCLOCREC pt;

	UM_transf tfmat;
	int status;

	uu_denter( UU_MTRC,(us,"umu_c2_pt()"));

	ud_lgeo(UU_TRUE, UD_vircurves);

	c = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*point on line*/UM_MODEL, 31, &pt, 
				 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
/*
...limit curves to circle for now. 11/29/94 vp.
*/
  ud_lgeo(UU_TRUE, UD_ncl_ci);
		um_dl_pldas(UD_DASPCKLOC, /*pick a curve*/UM_MODEL, 159, &pick, 
						1, &numint, 2);
		if (numint <= 0) goto repeat;

		c->key = um_get_pickkey(&pick.pent, 2);
		status = uc_retrieve_data(c, sizeof(struct UC_entitydatabag));
		if (status != UU_SUCCESS) goto repeat;
		status = uc_retrieve_transf(c->key, tfmat);
		if (status != UU_SUCCESS) goto repeat;
		status = uc_tan_line_thru_pt(c, tfmat, &pt, &(pick.ploc), &l);
		if (status != UU_SUCCESS) 
			{
			uu_uerror0(/* unable to calculate tangent line thru point */
			UM_MODEL, 280);
			}
		else
			{
			uc_create_data(&l, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&l);
			}
repeat:;
		}

done:;
	uu_free(c);
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_c2_tt()
**			Create a line tangent to two curves. The two curves must
**			lie in the same plane.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c2_tt()
	{
	UM_PLOCREC pick[2];				/* entity pick information */
	int numint;							/* number of DAS entries returned */
	struct UM_line_rec l;			/* line entity being constructed */
	struct UC_entitydatabag *c1;	/* first curve picked */
	UM_transf tfmat1;					/* transformation for first curve */
	struct UC_entitydatabag *c2;	/* second curve picked */
	UM_transf tfmat2;					/* transformation for second curve */
	int status;

	uu_denter( UU_MTRC,(us,"umu_c2_tt()"));

	ud_lgeo(UU_TRUE, UD_vircurves);

	c1 = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));
	c2 = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));

	while (UU_TRUE)
		{
/*
...limit curves to circle for now. 11/29/94 vp.
*/
  ud_lgeo(UU_TRUE, UD_ncl_ci);
		um_dl_pldas(UD_DASPCKLOC, /*pick first curve*/UM_MODEL, 160, &pick[0],
			1, &numint, 2);
		if (numint <= 0) goto done;
		c1->key = um_get_pickkey(&pick[0].pent, 2);

/*
...limit curves to circle for now. 11/29/94 vp.
*/
  ud_lgeo(UU_TRUE, UD_ncl_ci);
		um_dl_pldas (UD_DASPCKLOC, /*pick second circle*/UM_MODEL,188,
			&pick[1], 1, &numint, 2);
		if (numint <= 0) goto repeat;
		c2->key = um_get_pickkey(&pick[1].pent, 2);

		if (c1->key == c2->key)
	     uu_uerror0(/*second curve picked is same as first*/
						UM_MODEL,60);
		else
			{
			status = uc_retrieve_data(c1, sizeof(struct UC_entitydatabag));
			if (status != UU_SUCCESS) goto repeat;
			status = uc_retrieve_transf(c1->key, tfmat1);
			if (status != UU_SUCCESS) goto repeat;
			status = uc_retrieve_data(c2, sizeof(struct UC_entitydatabag));
			if (status != UU_SUCCESS) goto repeat;
			status = uc_retrieve_transf(c2->key, tfmat2);
			if (status != UU_SUCCESS) goto repeat;
			status = uc_tan_tan_line(c1, tfmat1, &pick[0].ploc,
				c2, tfmat2, &pick[1].ploc, &l);
			if (status != UU_SUCCESS)
				{
				uu_uerror0(/* unable to calculate line tangent to two curves */
				UM_MODEL, 281);
				}
			else
				{
				uc_create_data(&l, UM_DEFAULT_TF, UM_CURRENT_ATTR);
				uc_display(&l);
				}
			}
repeat:;
		}
done:;
	uu_free(c1);
	uu_free(c2);
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_c2_parto()
**			Create a line parallel to a given line and offset by a
**			specified distance.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c2_parto()
	{
	UM_PLOCREC pick;					/* pick information */
	int numint;							/* number of DAS entries returned */
	struct UM_line_rec e;			/* entity picked */
	struct UM_line_rec l;			/* new line entity */
	UM_length dist;					/* distance from picked line */
	int status;

	uu_denter( UU_MTRC,(us,"umu_c2_parto()"));

	while (UU_TRUE)
		{
		um_dl_pldas(UD_DASPCKLOC, /*pick a line*/UM_MODEL, 137, &pick, 1, &numint, 2);
		if (numint <= 0) goto done;
		e.key = um_get_pickkey(&pick.pent, 2);
		um_retrieve_data_relnum(e.key, &e.rel_num);
		if (e.rel_num  != UM_LINE_REL)
			uu_uerror0(/*you must pick a line*/UM_MODEL,67);
		else
			{
			um_get_all_geom(&e, sizeof(e));
			while (UU_TRUE)
				{
				ud_ldas(UD_DASDISTANCE, /*distance from picked line*/UM_MODEL, 
					11, &dist, 1, &numint, UD_NODEFAULT);
				if (numint <= 0) goto repeat;
				status = um_c2_parto(&e, &pick.ploc, dist, &l);
				if (status == UU_SUCCESS)
					{
					um_create_geom(&l, UM_DEFAULT_TF, UM_CURRENT_ATTR);
					ur_update_color(l.key, ur_get_attrmdl_color());
					uc_display(&l);
					}
				}
			}
repeat:;
		}
done:;
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_c2_parpt()
**			Prompt the user to pick a line and enter a point. A new line
**			is created, stored in UNIBASE, and displayed in DIGS. The
**			new line is parallel to the picked line, has the same length
**			and direction as the picked line, and starts at the supplied
**			point.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c2_parpt()
	{
	UM_PLOCREC pick;					/* pick information */
	int numint;							/* number of DAS entries returned */
	struct UM_line_rec e;			/* entity picked */
	struct UM_line_rec l;			/* new line entity */
   	UM_coord npt;						/* nearest point on original line to spt */
/*	spt						 start point new line */
	UD_NDCLOCREC spt;

	UM_vector line_vec;				/* vector along original line */
	UM_vector uline_vec;				/* unit vector along original line */
	UM_vector offset;					/* vector from original line to new line */
	int status;

	uu_denter( UU_MTRC,(us,"umu_c2_parpt()"));

	while (UU_TRUE)
		{
		um_dl_pldas(UD_DASPCKLOC, /*pick a line*/UM_MODEL, 137, &pick, 1, &numint, 2);
		if (numint <= 0) goto done;
		e.key = um_get_pickkey(&pick.pent, 2);
		um_retrieve_data_relnum(e.key, &e.rel_num);
		if (e.rel_num  != UM_LINE_REL)
			uu_uerror0(/*you must pick a line*/UM_MODEL,67);
		else
			{
			um_get_all_geom(&e, sizeof(e));
			um_vcmnvc(e.ept, e.spt, line_vec);
			um_unitvc(line_vec, uline_vec);
			while (UU_TRUE)
				{
				ud_ldas(UD_DASCART, /* point on line */UM_MODEL, 31,
					&spt, 1, &numint, UD_NODEFAULT);
				if (numint <= 0) goto repeat;
				um_nptln(&spt, e.spt, uline_vec, npt);
				um_vcmnvc(&spt, npt, offset);
				um_vcplvc(e.spt, offset, &spt);
				status = um_c2_ptvec(&spt, line_vec, &l);
				if (status == UU_SUCCESS)
					{
					um_create_geom(&l, UM_DEFAULT_TF, UM_CURRENT_ATTR);
					ur_update_color(l.key, ur_get_attrmdl_color());
					uc_display(&l);
					}
				}
			}
repeat:;
		}
done:;
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_c2_angle(perpline)
**       Create a line which makes the specified angle from
**			the picked line.
**    PARAMETERS   
**       INPUT  : 
**				perpline				UU_TRUE => angle is 90 degrees
**										UU_FALSE => prompt user for angle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c2_angle(perpline)
	UU_LOGICAL perpline;
	{
	UM_PLOCREC	pick;						/* pick location on picked entity */
	int			numint;					/* the of interactions input through das */
	struct		UM_line_rec lpk;		/* the line entity picked by the user */
	struct		UM_line_rec	la;		/* the new line entity created */
	UM_angle		angrad;					/* angle */
	UM_length	length;					/* length of new line */
	UM_coord		ppt;						/* projection of new point on line */
/*	pt						 point new line is to go through */
	UD_NDCLOCREC pt;

	UM_vector	line_vec;				/* vector along picked line */
	int			status;

	uu_denter(UU_MTRC,(us,"umu_c2_angle()"));

	while (UU_TRUE)
		{
		um_dl_pldas(UD_DASPCKLOC, /*pick line*/ UM_MODEL, 137, 
									&pick, 1, &numint , 2);
		lpk.key = um_get_pickkey(&pick.pent, 2);
		if (numint <= 0) goto done;
		um_retrieve_data_relnum(lpk.key, &lpk.rel_num);
		if(lpk.rel_num != UM_LINE_REL)
			{
			uu_uerror0(/*you must pick a line*/UM_MODEL,67);
			}
		else
			{
			um_get_all_geom(&lpk, sizeof(lpk));
			while (UU_TRUE) 
				{
				ud_ldas(UD_DASCART, /*point new line goes through*/UM_MODEL, 
						32, &pt, 1, &numint, UD_NODEFAULT);
				if (numint <= 0) goto repeat;
				if (perpline) 
					angrad = UM_HALFPI;
				else 
					ud_ldas(UD_DASANGLE, /*angle*/UM_MODEL, 13, &angrad, 
								1,&numint, UD_NODEFAULT);
				if(numint <= 0) goto repeat;
				if ( angrad < -UM_PI || angrad > UM_PI )
					{
					uu_uerror0(/*angle out of range*/UM_MODEL,69);
					goto repeat;
					}
				um_vcmnvc(lpk.ept, lpk.spt, line_vec);
				um_unitvc(line_vec, line_vec);
				um_nptln(&pt, lpk.spt, line_vec, ppt);
				if (!um_cceqcc(&pt, ppt)) length = 0;
				else
					{
					ud_ldas(UD_DASDISTANCE,/*length of new line*/UM_MODEL, 
						33, &length, 1, &numint, UD_NODEFAULT);
					}
				if( numint <= 0 ) goto repeat;
				status = um_c2_angle(&lpk, &pt, angrad, length, &pick.ploc, &la);
				if (status == UU_SUCCESS)
					{
					um_create_geom(&la, UM_DEFAULT_TF, UM_CURRENT_ATTR);
					uc_display(&la);
					}
				}
			}
repeat:;
		}
done:;
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_c2_ptveclen()
**       Prompt the user for a vector and then create a sequence of
**			lines parallel to this vector which start at a point supplied
**			by the users and have the (user) specified length.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c2_ptveclen()
	{
	struct UM_line_rec l;
/**	UM_coord spt; **/
    UD_NDCLOCREC spt;

	UM_vector vec;
	UM_vector line_vec;
	UM_length length;
	int numint;
	int status;

	uu_denter(UU_MTRC,(us,"umu_c2_ptveclen()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASVEC, /* Parallel to what vector? */UM_MODEL, 247,
			vec, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		um_unitvc(vec, vec);
		while (UU_TRUE)
			{
			ud_ldas(UD_DASCART, /* start point of new line */UM_MODEL, 29,
				&spt, 1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			ud_ldas(UD_DASDISTANCE, /* length of new line */ UM_MODEL, 33,
				&length, 1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			um_vctmsc(vec, length, line_vec);
			status = um_c2_ptvec(&spt, line_vec, &l);
			if (status == UU_SUCCESS)
				{
				um_create_geom(&l, UM_DEFAULT_TF, UM_CURRENT_ATTR);
				ur_update_color(l.key, ur_get_attrmdl_color());
				uc_display(&l);
				}
			}
repeat:;
		}
done:;
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_c2_connected()
**			Create lines by prompting the user for a sequence of points.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c2_connected()
	{
	struct UM_line_rec l;			/* line curve */
	UM_coord fstpt;					/* first point */
/*	spt						 start point of line */
/*	ept						 end point of line */
    UD_NDCLOCREC spt, ept;

	int numint;							/* number of DAS entries */
	int status;
	UD_RUBBER rubber;					/*  rubber band control block */

	uu_denter(UU_MTRC, (us, "umu_c2_connected"));
	numint = 1;
	RUBBER_LINE(&rubber);

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /*start point of new line*/UM_MODEL, 29, &spt, 
			 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto done;
		um_vctovc(&spt, fstpt);
		RUBBER_ON(&rubber);
		while (UU_TRUE)
			{
			ud_ldas(UD_DASCART, /*end point of line*/UM_MODEL, 30, &ept, 
				 1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto repeat;
			status = um_c2_pp(&spt, &ept, &l);
			if (status == UU_SUCCESS)
				{
				um_create_geom(&l, UM_DEFAULT_TF, UM_CURRENT_ATTR);
				uc_display(&l);
				}
			um_vctovc(&ept,&spt);
			}
repeat:
			ud_endrub(&rubber);
		}
done:
	RUBBER_OFF(&rubber);
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_m2_chamfer()
**       Chamfer by calculating the intersection of the two lines then
**	      calculate a point a specified distance back on the first line
**		   and a point on the second line at an angle from the first line.
**		   Create a new line between these new points and determine which
**		   endpoints on the first and second line to set to the new points.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_m2_chamfer()
	{
	UM_PLOCREC	pick;						/* the pick id and location */
	int			numint;					/* the number of interactions input
													through das */
	struct         UM_line_rec  e1;  /* the first line entity */ 
	struct         UM_line_rec  e2;  /* the second line entity */
	struct			UM_line_rec	lc;	/* the chamfer line entity */
	UD_NDCLOCREC	pk1;					/* pick location on the first line */
	UD_NDCLOCREC	pk2;					/* pick location on the second line */
	UM_vector		v1;					/* vector of the first line */
	UM_vector		v2;					/* vector of the second line */
	UM_vector		u1;					/* unit vector of the first line */
	UM_vector		u2;					/* unit vector of the second line */
	UM_vector		vc1,vc2;
	UM_coord			ipt;					/* the intersection between the
													first and second lines */
	UM_coord			tmp1;					/* temporary storage */
	UM_coord			tmp2;					/* tempoary storage */
	int				nint;					/* number of intersection points */
	UM_length		dist1;
	UM_length		dist2;
	UM_length		angc;
	UM_angle			ang1;
	UM_angle			ang2;
	UU_LOGICAL     um_ptbtwn();

	uu_denter(UU_MTRC,(us,"umu_m2_chamfer()"));

	ud_leditable(UU_TRUE);
	while (UU_TRUE)
		{
		um_dl_pldas(UD_DASPCKLOC, /*pick first line*/ UM_MODEL, 154, &pick, 1, &numint, 1);
		if (numint <= 0) goto done;
		e1.key = um_get_pickkey(&pick.pent, 1);
		ur_retrieve_data_relnum(e1.key, &e1.rel_num ); 
		um_copyploc(&pick.ploc, &pk1);
		if( e1.rel_num !=  UM_LINE_REL)
			{
			uu_uerror0( /*you must pick a line*/UM_MODEL,14);
			goto repeat;
			}
		um_dl_pldas( UD_DASPCKLOC, /*pick second line*/ UM_MODEL, 155, &pick, 1, &numint, 1);
		if (numint <= 0) goto repeat;
		e2.key = um_get_pickkey(&pick.pent, 1);
		ur_retrieve_data_relnum(e2.key, &e2.rel_num); 
		um_copyploc(&pick.ploc, &pk2);
		if( e2.rel_num !=  UM_LINE_REL)
			{
			uu_uerror0( /*you must pick a line*/UM_MODEL, 14);
			goto repeat;
			}
		ud_ldas(UD_DASDISTANCE, /*distance (along first line)*/UM_MODEL, 12, 
					&dist1, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		ud_ldas(UD_DASANGLE, /*angle of chamfer*/UM_MODEL, 13, &ang2, 1, 
					&numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;
		if( fabs(ang2) >= UM_PI )
			{
			uu_uerror0( /*incorrect angle*/UM_MODEL,15);
			goto repeat;
			}
	
	/* -- Calculate the intersection point -- */

		um_get_all_geom(&e1, sizeof(e1));
		um_get_all_geom(&e2, sizeof(e2));
		um_vcmnvc( e1.ept, e1.spt, v1 );
		um_unitvc( v1, u1 );
		um_vcmnvc( e2.ept, e2.spt, v2 );
		um_unitvc( v2, u2 );
		if(um_cceqcc(e1.spt,e2.spt) == UU_TRUE  ||
			um_cceqcc(e1.spt,e2.ept) == UU_TRUE) um_vctovc(e1.spt,ipt);
		else if(um_cceqcc(e1.ept,e2.spt) == UU_TRUE  ||
			um_cceqcc(e1.ept,e2.ept) == UU_TRUE) um_vctovc(e1.ept,ipt);
		else
			{
			um_ilnln(e1.spt, u1, e2.spt, u2, &nint, ipt );
			if( nint == 0 )
				{
				uu_uerror0( /*line 1 and line 2 do not intersect*/UM_MODEL,16);
				goto repeat;
				}
			}
	
	/* -- Calculate the point on the first line a distance back from the
			intersection and update the endpoints of the first line and
			create the starting point for the chamfer line.  -- */

		um_vctmsc( u1, dist1, v1 );
		if(um_cceqcc(e1.spt,ipt) == UU_TRUE)
			{
			um_vcplvc( ipt, v1, e1.spt );
			um_vctovc( e1.spt, lc.spt );
			if ( um_ptbtwn( e1.ept, e1.spt, ipt )==UU_FALSE )
				{
				uu_uerror0( /*distance too large*/UM_MODEL,17);
				goto repeat;
				}
			}
		else
			{
			uv_cctondc(e1.spt,tmp1,pk1.transform);
			uv_cctondc(ipt,tmp2,pk1.transform);
			if ( um_ptbtwn( tmp1, pk1.cord, tmp2 )==UU_TRUE )
				{
				um_vcmnvc(ipt, v1, e1.ept );
				um_vctovc( e1.ept, lc.spt );
				if ( um_ptbtwn( e1.spt, e1.ept, ipt )==UU_FALSE )
					{
					uu_uerror0( /*distance too large*/UM_MODEL,17);
					goto repeat;
					}
				}
			else
				{
				um_vcplvc( ipt, v1, e1.spt );
				um_vctovc( e1.spt, lc.spt );
				if ( um_ptbtwn( e1.ept, e1.spt, ipt )==UU_FALSE )
					{
					uu_uerror0( /*distance too large*/UM_MODEL,17);
					goto repeat;
					}
				}
			}
	
	/* -- Calculate the point on the second line at an angle from the first
			line and update the endpoints of the second line and create the
			ending point for the chamfer line.  -- */

		if(um_dcccc(ipt,e1.spt) < um_dcccc(ipt,e1.ept) )
			{
			um_vcmnvc(e1.ept,ipt,vc1);
			}
		else
			{
			um_vcmnvc(e1.spt,ipt,vc1);
			}
		if(um_dcccc(ipt,e2.spt) < um_dcccc(ipt,e2.ept) )
			{
			um_vcmnvc(e2.ept,ipt,vc2);
			}
		else
			{
			um_vcmnvc(e2.spt,ipt,vc2);
			}
		angc = um_angle(vc1,vc2);
		if(ang2 < 0.0 ) ang2 = UM_PI + ang2;
		if( ang2 + angc >= UM_PI )
			{
			uu_uerror0( /*angle too large*/UM_MODEL,18);
			goto repeat;
			}
		ang1 = UM_PI - ang2 - angc;
		if ( sin(ang1) < UM_FUZZ )
			{
			uu_uerror0(/*angle too large*/UM_MODEL,18);
			goto repeat;
			}
		dist2 = sin(ang2) * dist1 / sin(ang1);
		um_vctmsc( u2, dist2, v2 );
		if(um_cceqcc(e2.spt,ipt) == UU_TRUE)
			{
			um_vcplvc( ipt, v2, e2.spt );
			um_vctovc( e2.spt, lc.ept );
			if ( um_ptbtwn( e2.ept, e2.spt, ipt )==UU_FALSE )
				{
				uu_uerror0( /*angle too large*/UM_MODEL,18);
				goto repeat;
				}
		 	}
		else
			{
			uv_cctondc(e2.spt,tmp1,pk2.transform);
			uv_cctondc(ipt,tmp2,pk2.transform);
			if ( um_ptbtwn( tmp1, pk2.cord, tmp2 )==UU_TRUE )
				{
				um_vcmnvc( ipt, v2, e2.ept );
				um_vctovc( e2.ept, lc.ept );
				if ( um_ptbtwn( e2.spt, e2.ept, ipt )==UU_FALSE )
					{
					uu_uerror0( /*angle too large*/UM_MODEL,18);
					goto repeat;
					}
				}
			else
				{
				um_vcplvc( ipt, v2, e2.spt );
				um_vctovc( e2.spt, lc.ept );
				if ( um_ptbtwn( e2.ept, e2.spt, ipt )==UU_FALSE )
					{
					uu_uerror0( /*angle too large*/UM_MODEL,18);
					goto repeat;
					}
				}
			}

	/* -- Update the lines in unibase and redisplay them -- */

		ur_setup_data(UM_LINE_REL, &lc, sizeof(lc));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (lc.label, "");
		lc.subscr = 0;
		um_create_geom( &lc , UM_DEFAULT_TF, UM_CURRENT_ATTR); 
		uc_display( &lc );
		um_update_geom( &e1 , UM_DEFAULT_TF); 
		uc_display(&e1);
	
		um_update_geom( &e2 , UM_DEFAULT_TF);
		uc_display(&e2);

repeat:;
	}
done:
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_m2_modline(option)
**       Change the endpoint of a line.
**    PARAMETERS   
**       INPUT  : 
**          option				1 =>	move end point closest to 
**												picked location to any 3D
**												coordinate
**          						2 =>	move end point closest to 
**												picked location a given distance
**												along line
**          						3 =>	move end point closest to 
**												picked location to projection
**												of another point onto line
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_m2_modline(option)
	int option;

	{
	UM_PLOCREC	pick;						/* the pick id and location */
	int			numint;					/* the number of interactions input
													through das */
	struct		UM_line_rec	l1;		/* the first line entity */
	UM_coord		pt[2];					/* original end point locations */
/*	newpt					 new end point location */
/*	projpt				 point to project onto line */
    UD_NDCLOCREC newpt, projpt;

	UM_vector	lnvc;						/* vector along line */
	int			closest;					/* index of closest point to pick */
	UU_REAL		distance;
	int			status;
	char label[NCL_MAX_LABEL];		/*store label to reset */
	int subscr;			/*store subscr */
	UU_KEY_ID key;		/*store key */

	uu_denter( UU_MTRC,(us,"umu_m2_modline(option=%d)",option));

	/* limit pickable geometry */
	ud_lgeo(UU_TRUE, UD_line);
	ud_leditable(UU_TRUE);

	/* prompt the user to pick a line nearest to the end point to be moved */
	um_dl_pldas( UD_DASPCKLOC, /*Pick line near end to be moved:*/UM_MODEL, 161, 
			&pick, 1, &numint, 1);
	if (numint <= 0) goto done;

	/* retrieve line data and determine which end point was picked */
	l1.key = um_get_pickkey(&pick.pent, 1);
	um_get_all_geom(&l1, sizeof(l1));
	um_vctovc(l1.spt, pt[0]);
	um_vctovc(l1.ept, pt[1]);
	um_vcmnvc(l1.ept, l1.spt, lnvc);
	um_unitvc(lnvc, lnvc);
	closest = um_nearest_to_ploc(&pick.ploc, 2, pt);

	/* determine the new coordinates for the point to be moved */
	switch (option)
		{
		case 1:
			ud_ldas(UD_DASCART, /*New endpoint of line: */UM_MODEL, 106, &newpt, 
				1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto done;
			break;
		case 2:
			ud_ldas(UD_DASDISTANCE, /* distance to extend line */ UM_MODEL, 295,
				&distance, 1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto done;
			distance = fabs(distance);
			if (closest == 0)
				um_vctmsc(lnvc, -distance, lnvc);
			else
				um_vctmsc(lnvc, distance, lnvc);
			um_vcplvc(pt[closest], lnvc, &newpt);
			break;
		case 3:
			ud_ldas(UD_DASCART, /*Project point to define endpoint */UM_MODEL, 296,
				&projpt, 1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto done;
			um_nptln(&projpt, l1.spt, lnvc, &newpt);
			break;
		default:
			goto done;
		}

	/* save off label, key, subscr for later */
	strncpy(label,l1.label, NCL_MAX_LABEL);
	subscr = l1.subscr;
	key = l1.key;

	/* modify the chosen end point of the line */
	if (closest == 0)
		status = um_c2_pp(&newpt, pt[1], &l1);
	else
		status = um_c2_pp(pt[0], &newpt, &l1);
			
	/*reset label and subscr */
	strncpy(l1.label,label, NCL_MAX_LABEL);
	l1.subscr = subscr;
	l1.key = key;

	/* update UNIBASE and DIGS */
	if (status == 0)
		{
		um_update_geom(&l1, UM_DEFAULT_TF);
		uc_display(&l1);
		}

done:
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_c2_tancrv_at_pt()
**			Create a line tangent to a curve at a point on the curve.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c2_tancrv_at_pt()

	{
	struct UM_evcrvout *evcrv;
	struct UC_entitydatabag *crv;
	struct UM_line_rec l;
	UM_transf tfmat;
	UM_PLOCREC pick;
	int numint;
   	UM_coord ept;
    UD_NDCLOCREC spt;

	UM_vector uvc;
	UM_length length;
	UM_param u;
	UU_REAL error;
	int status;

	uu_denter( UU_MTRC,(us,"umu_c2_tancrv_at_pt()"));

	crv = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));
	evcrv = (struct UM_evcrvout *) uu_malloc(sizeof(struct UM_evcrvout));

	while (UU_TRUE)
		{
		ud_lgeo(UU_TRUE, UD_vircurves);

		um_dl_pldas(UD_DASPCKLOC, /*pick curve line is tangent to*/UM_MODEL, 291,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;

		ud_ldas(UD_DASCART, /*point on curve for tangent line*/UM_MODEL, 292,
				&spt, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;

		ud_ldas(UD_DASDISTANCE, /* length of new line */ UM_MODEL, 33,
			&length, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;

		crv->key = um_get_pickkey(&pick.pent, 2);

		status = uc_retrieve_data(crv, sizeof(struct UC_entitydatabag));
		if (status != UU_SUCCESS) goto repeat;

		status = uc_retrieve_transf(crv->key, tfmat);
		if (status != UU_SUCCESS) goto repeat;

		status = uc_cctou(crv, tfmat, &spt, &u, &error);
		if (status != UU_SUCCESS)
			uu_uerror0(/* point not on curve */UM_MODEL, 275);
		else
			{
			status = uc_init_evcrvout(crv, evcrv);
			if (status != UU_SUCCESS) goto repeat;

			status = uc_evcrv(UM_FRSTDERIV, u, crv, tfmat, evcrv);
			if (status != UU_SUCCESS) goto repeat;
	
			um_unitvc(evcrv->dcdu, uvc);
			um_vctmsc(uvc, length, uvc);
			um_vcplvc(evcrv->cp, uvc, ept);
	
			status = um_c2_pp(evcrv->cp, ept, &l);
			if (status != UU_SUCCESS) goto repeat;

			um_create_geom(&l, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&l);
			}
repeat:;
		}
done:;
	uu_free(crv);
	uu_free(evcrv);
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : umu_c2_normcrv_at_pt()
**			Create a line normal to a curve at a point on the curve.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c2_normcrv_at_pt()
	{
	struct UM_evcrvout *evcrv;
	struct UC_entitydatabag *crv;
	struct UM_line_rec l;
	struct UM_line_rec lpk;
	UM_transf tfmat;
	UM_PLOCREC pick;
	int numint;
	UM_coord  ept;
	UD_NDCLOCREC spt;

	UM_vector uvc;
	UM_length length;
	UM_param u;
	UU_REAL error;
	int status;

	uu_denter( UU_MTRC,(us,"umu_c2_normcrv_at_pt()"));

	ud_lgeo(UU_TRUE, UD_vircurves);

	crv = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));
	evcrv = (struct UM_evcrvout *) uu_malloc(sizeof(struct UM_evcrvout));

	while (UU_TRUE)
		{
		um_dl_pldas(UD_DASPCKLOC, /*pick curve line is normal to*/UM_MODEL, 293,
			&pick, 1, &numint, 2);
		if (numint <= 0) goto done;

		ud_ldas(UD_DASCART, /*point on curve for normal line*/UM_MODEL, 294,
				&spt, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;

		ud_ldas(UD_DASDISTANCE, /* length of new line */ UM_MODEL, 33,
			&length, 1, &numint, UD_NODEFAULT);
		if (numint <= 0) goto repeat;

		crv->key = um_get_pickkey(&pick.pent, 2);

		status = uc_retrieve_data(crv, sizeof(struct UC_entitydatabag));
		if (status != UU_SUCCESS) goto repeat;

		status = uc_retrieve_transf(crv->key, tfmat);
		if (status != UU_SUCCESS) goto repeat;

		status = uc_cctou(crv, tfmat, &spt, &u, &error);
		if (status != UU_SUCCESS)
			uu_uerror0(/* point not on curve */UM_MODEL, 275);
		else
			{
			status = uc_init_evcrvout(crv, evcrv);
			if (status != UU_SUCCESS) goto repeat;

			status = uc_evcrv(UM_FRSTDERIV, u, crv, tfmat, evcrv);
			if (status != UU_SUCCESS) goto repeat;
	
			um_unitvc(evcrv->dcdu, uvc);
			um_vcplvc(evcrv->cp, uvc, ept);
	
			status = um_c2_pp(evcrv->cp, ept, &lpk);
			if (status != UU_SUCCESS) goto repeat;

			status = um_c2_angle(&lpk,evcrv->cp,UM_HALFPI,length,&pick.ploc,&l);
			if (status != UU_SUCCESS) goto repeat;
			um_vcmnvc(l.ept, l.spt, uvc);
			um_unitvc(uvc, uvc);
			um_vctmsc(uvc, length, uvc);
			um_vcplvc(l.spt, uvc, l.ept);

			um_create_geom(&l, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			uc_display(&l);
			}
repeat:;
		}
done:;
	uu_free(crv);
	uu_free(evcrv);
	uu_dexit;
	return (0);
	}
