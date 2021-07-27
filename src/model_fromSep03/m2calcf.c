
/*********************************************************************
**    NAME         :  m2calcf.c
**       CONTAINS:
**				um_mindist
**				umi_line_mindist
**				umi_arc_mindist
**				um_perpdist
**				um_crvlen
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m2calcf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:44
*********************************************************************/

#include		"usysdef.h"
#include		"dselmask.h"
#include		"umath.h"
#include		"mdrel.h"
#include		"mdpick.h"
#include		"mdcoord.h"
#include		"mcrv.h"
#include		"mdcpln.h"
#include		"modef.h"
#include		"mdeval.h"
#include		"dtypes.h"
#include		"uhep.h"
#include		"udebug.h"

/*********************************************************************
**    I_FUNCTION :  um_mindist(type,val)
**       Find the minimum distance between two entities.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_mindist(type,val)
int	type;
UU_REAL	val[];
{
/**	UM_coord   pt; **/
    UD_NDCLOCREC pt;

	UM_PLOCREC pick;					/* pick information */
	int numint;							/* number of DAS entries returned */
	UU_KEY_ID	 key;
	int		 rel_num;
	int		 status;
	UU_LOGICAL	state;

   uu_denter(UU_MTRC,(us,"enter um_mindist"));

	numint = 1;
	pt.cord[0] = 0.0;
	pt.cord[1] = 0.0;
	pt.cord[2] = 0.0;
	state = UU_TRUE;
	status = ud_ldas(UD_DASCART,/*Enter a point:*/UM_MODEL, 297, &pt, 
				 1, &numint, UD_DEFAULT);
   uu_dprint(UU_MTRC,(us,"status1=%d,numint=%d",status,numint));
	if (numint<=0)
	  {
		state = UU_FALSE;
		goto done;
	  }

	ud_lgeo(UU_TRUE, UD_ptlncir);	/* restrict DAS to pick line/curves only */
	status = um_dl_pldas(UD_DASPCKLOC, /*pick a entity:*/UM_MODEL, 302, 
								&pick, 1, &numint, 2);
	uu_dprint(UU_MTRC,(us,"status2=%d,numint=%d",status,numint));
	if (numint <= 0) 
	{
	 state = UU_FALSE;
	 goto done;
	}
	key = um_get_pickkey(&pick.pent, 2);
	um_retrieve_data_relnum(key, &rel_num);
	switch (rel_num)
	{
	 case	UM_LINE_REL :
		 umi_line_mindist(&pt,key,rel_num,val);
		 break;

	 case	UM_CIRCLE_REL :
		 if (!umi_arc_mindist(&pt,key,rel_num,val))
			state = UU_FALSE;
		 break;

	 case UM_POINT_REL:
		 umi_point_dist(&pt,key,rel_num,val);
		 break;

	 default :
		 uu_uerror0(/*you must pick a line/curve*/UM_MODEL,277);
		 break;
	}
done:
	ud_lgeo(UU_FALSE, UD_ptlncir);	/* restrict DAS to pick line/curves only */
	uu_dexit;
	return(state);
}	/* um_mindist */


/*********************************************************************
**    I_FUNCTION :  umi_line_mindist(pt,key,rel_num,mindist)
**			Find the minimum distance from a point to a line.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

umi_line_mindist(pt,key,rel_num,mindist)
UM_coord		pt;
UU_KEY_ID 	key;
int			rel_num;
UU_REAL	   *mindist;

{
	struct UM_line_rec e;			/* entity picked */
	UM_vector vc;					
	UM_vector uvc;						/* unit vector */
	UM_vector ulvc;					/* unit vector on the line */
	UM_coord   nlpt;					/* nearest point */
	UU_REAL	 dist, dist1, dist2;
	UM_vector v1, v2, v3;					
	UU_REAL	ang1, ang2;
	
   uu_denter(UU_MTRC,(us,"enter umi_line_mindist"));
   e.key = key;
   e.rel_num= rel_num;
   um_get_all_geom(&e, sizeof(e));
   um_vcmnvc(e.ept,e.spt,vc);
   um_unitvc(vc,ulvc);
   um_nptln(pt,e.spt,ulvc,nlpt);		/* find the nearest point */
   um_vcmnvc(pt,e.spt,v1);
   um_vcmnvc(pt,e.ept,v2);
   um_vcmnvc(pt,nlpt,v3);
	ang1 = um_angle(v1,v2);
	ang2 = um_angle(v1,v3);
	if (ang1 > ang2)		/* the nearest point sits between the two endpoints */
   	dist = um_dcccc(pt,nlpt);
   else			/* nearest point deost not sit between two end points */
	  {
	   dist1 = um_dcccc(pt,e.spt);
	   dist2 = um_dcccc(pt,e.ept);
	   dist = (dist1 < dist2)? dist1 : dist2;
	  }
   UM_len_inttoext(dist,*mindist);

	uu_dexit;
}	/* umi_line_mindist */


/*********************************************************************
**    I_FUNCTION :  umi_arc_mindist(pt,key,rel_num)
**			Find the minimum distance from a point to a arc.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

umi_arc_mindist(pt,key,rel_num,mindist)
UM_coord		pt;
UU_KEY_ID 	key;
int			rel_num;
UU_REAL	   *mindist;

{
	struct UM_circle_rec	e;			/* circular entity */
	int		 nint;
	UM_coord	 pjt;
	UM_vector vc;					
	UM_vector uvc;						/* unit vector */
	UM_vector ulvc;					/* unit vector on the line */
	UM_coord  nlpt;					/* nearest point */
	UM_coord  spt, ept;	
	UU_REAL	 dist, dist1, dist2, angle;	
	struct UM_evcrvout	evout;		/* curve evaluator record */
	UU_LOGICAL	um_cceqcc();


   uu_denter(UU_MTRC,(us,"enter umi_arc_mindist"));

   e.key = key;
 	e.rel_num= rel_num;
 	um_get_all_geom(&e, sizeof(e));
		/* check whether the poit and the curve are on the same plane */
	um_ilnpln(pt,e.nvec,e.center,e.nvec,&nint,pjt);
	if (!um_cceqcc(pt,pjt))
	  {
		uu_uerror0(/*Point and the curve are not on the same plane*/UM_MODEL,278);
		uu_dexit;
		return(UU_FALSE);
	  }
	else
 	if (um_cceqcc(pt,e.center)==UU_TRUE)    /* point is on the center */
		dist = e.radius;
 	else
		{
 	 	 um_vcmnvc(pt,e.center,vc);
    	 um_unitvc(vc,uvc);
 	 	 um_vctmsc(uvc,e.radius,vc);
 	 	 um_vcplvc(e.center,vc,nlpt);		/* find the point on the curve */
						/* find the angle between start point and the new point*/
		 um_vcmnvc(nlpt,e.center,vc);
		 angle = um_angle2p(e.svec,vc,e.nvec);
   	 uu_dprint(UU_MTRC,(us,"angle=%g,dang=%g",angle,e.dang));
		 if (e.dang > angle)		/* new point sits between both end points */
 	 	 	dist = um_dcccc(pt,nlpt);
		 else	/* if new point does not sit between both end points then one of
					the end point is the nearest point */
			{
			 um_ev3_circle(UM_POINT,(UU_REAL) 0.0,&e,UM_DEFAULT_TF,&evout); /* start point */
			 um_vctovc(evout.cp,spt);
   		 uu_dprint(UU_MTRC,(us,"cp=%g,%g,%g;spt=%g,%g,%g",evout.cp[0],
			 evout.cp[1],evout.cp[2],spt[0],spt[1],spt[2]));
			 um_ev3_circle(UM_POINT,(UU_REAL) 1.0,&e,UM_DEFAULT_TF,&evout); /* end point */
			 um_vctovc(evout.cp,ept);
   		 uu_dprint(UU_MTRC,(us,"cp=%g,%g,%g;ept=%g,%g,%g",evout.cp[0],
			 evout.cp[1],evout.cp[2],ept[0],ept[1],ept[2]));
 	 	 	 dist1 = um_dcccc(pt,spt);
 	 	 	 dist2 = um_dcccc(pt,ept);
   		 uu_dprint(UU_MTRC,(us,"dist1=%g,dist2=%g",dist1,dist2));
			 dist = (dist1 < dist2)? dist1 : dist2;
			}
		}
 	UM_len_inttoext(dist,*mindist);
	uu_dexit;
	return(UU_TRUE);
}		/* umi_arc_mindist */



/*********************************************************************
**    I_FUNCTION :  umi_point_dist(pt,key,rel_num,mindist)
**			Find the distance from a point to a point.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

umi_point_dist(pt,key,rel_num,mindist)
UM_coord		pt;
UU_KEY_ID 	key;
int			rel_num;
UU_REAL	   *mindist;

{
	struct UM_point_rec e;			/* entity picked */
	UU_REAL	 dist;
	
   uu_denter(UU_MTRC,(us,"enter umi_point_dist"));
   e.key = key;
   e.rel_num= rel_num;
   um_get_all_geom(&e, sizeof(e));
	dist = um_dcccc(pt,e.pt);
   UM_len_inttoext(dist,*mindist);
	uu_dexit;
}	/* umi_line_mindist */



/*********************************************************************
**    I_FUNCTION :  um_perpdist(type,val)
**       Find the perpendicular distance between a point and a line, 
**			or a point and a curve.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_perpdist(type,val)
int	type;
UU_REAL	val[];

{
	UM_coord    pjt;
    UD_NDCLOCREC pt;

	UM_PLOCREC pick;					/* pick information */
	int numint;							/* number of DAS entries returned */
	int nint;
	struct UM_line_rec e;			/* entity picked */
	struct UM_circle_rec	l;			/* circular entity */
	UM_vector vc;					
	UM_vector uvc;						/* unit vector */
	UM_vector ulvc;					/* unit vector on the line */
	UM_coord   nlpt;					/* nearest point */
	UU_REAL	 dist, angle;	
	UU_KEY_ID	 key;
	int		 rel_num;
	UU_LOGICAL	state;

   uu_denter(UU_MTRC,(us,"enter um_perpdist"));

	numint = 1;
	pt.cord[0] = 0.0;
	pt.cord[1] = 0.0;
	pt.cord[2] = 0.0;
	state = UU_TRUE;
	ud_ldas(UD_DASCART,/*Enter a point:*/UM_MODEL, 297, &pt, 
				 1, &numint, UD_DEFAULT);
	if (numint <= 0) 
	  {
		state = UU_FALSE;
		goto done;
	  }

	ud_lgeo(UU_TRUE, UD_ptlncir);	/* restrict DAS to pick line/curves only */
   um_dl_pldas(UD_DASPCKLOC, /*pick a entity:*/UM_MODEL, 303, &pick, 1, 
					&numint, 2);
	if (numint <= 0) 
		{
		 state = UU_FALSE;
		 goto done;
	  	}
	key = um_get_pickkey(&pick.pent, 2);
	um_retrieve_data_relnum(key, &rel_num);
	switch (rel_num)
 	{
	 case	UM_LINE_REL :
		 e.key = key;
		 e.rel_num= rel_num;
		 um_get_all_geom(&e, sizeof(e));
		 um_vcmnvc(e.ept,e.spt,vc);
		 um_unitvc(vc,ulvc);
	 	 um_nptln(&pt,e.spt,ulvc,nlpt);
		 dist = um_dcccc(&pt,nlpt);
		 UM_len_inttoext(dist,val[0]);
		 break;

	 case	UM_CIRCLE_REL :
		 l.key = key;
		 l.rel_num= rel_num;
		 um_get_all_geom(&l, sizeof(l));
			/* check whether the poit and the curve are on the same plane */
		 um_ilnpln(&pt,l.nvec,l.center,l.nvec,&nint,pjt);
		 if (!um_cceqcc(&pt,pjt))
			{
			 state = UU_FALSE;
			 uu_uerror0(/*Point and the curve are not on the same plane*/UM_MODEL,
							278);
			 goto done;
			}
		 else
		  {
		   if (um_cceqcc(&pt,l.center)==UU_TRUE)    /* point is on the center */
			  dist = l.radius;
		   else
			  {
		 	   um_vcmnvc(&pt,l.center,vc);
		      um_unitvc(vc,uvc);
		 	   um_vctmsc(uvc,l.radius,vc);
		 	   um_vcplvc(l.center,vc,nlpt);	/* find the point on the curve */
		 	   dist = um_dcccc(&pt,nlpt);
			  }
		 	UM_len_inttoext(dist,val[0]);
		  }
		 break;

	 case UM_POINT_REL:
		 umi_point_dist(&pt,key,rel_num,val);
		 break;

	  default :
		 uu_uerror0(/*you must pick a line/curve*/UM_MODEL,277);
		 break;
	 }	/* switch */
done:
	ud_lgeo(UU_FALSE, UD_ptlncir);/* don't restrict DAS to pick line/curves only */
	uu_dexit;
	return(state);
}	/* um_perpdist */


/*********************************************************************
**    I_FUNCTION :  um_crvlen(type,val)
**       Find the curve's length.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_crvlen(type,val)
int	type;
UU_REAL	val[];

{
	UM_PLOCREC pick;					/* pick information */
	int numint;							/* number of DAS entries returned */
	struct UM_crvdatabag	e;			/* curve entity */
	UU_REAL	len;
	UM_transf tfmat;
	UU_LOGICAL	state;

   uu_denter(UU_MTRC,(us,"enter um_crvlen"));
	state = UU_TRUE;
	ud_lgeo(UU_TRUE, UD_allcurves);	/* restrict DAS to pick curves only */
  	um_dl_pldas(UD_DASPCKLOC, /*pick a curve:*/UM_MODEL, 299, &pick, 1, &numint, 2);
  	if (numint <= 0) 
		state = UU_FALSE;
	else
	  {
   	e.key = um_get_pickkey(&pick.pent, 2);
	  	um_get_all_geom(&e, sizeof(e));
		uc_retrieve_transf(e.key, tfmat);
	  	len = um_getarclen(&e,tfmat);
	  	UM_len_inttoext(len,val[0]);
	  }
	ud_lgeo(UU_FALSE, UD_allcurves);
	uu_dexit;
	return(state);
}	/* um_crvlen */


