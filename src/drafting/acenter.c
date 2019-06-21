/*********************************************************************
**    NAME         : acenter.c
**       CONTAINS:
**    			ua_centerline_arc 
**    			ua_init_bolt_arc_blk 
**    			ua_arc_pattern_fill 
**    			ua_init_center_line_blk 
**					ua_bolt_arc_blk
**					ua_fill_line_blk
**					ua_perp_line_blk
**					ua_para_line_blk
**					ua_full_bolt_create
**					ua_partial_bolt_create
**					ua_multi_arc
**					ua_multi_line_blk
**					ua_single_line_blk
**					ua_single_arc
**					ua_bolt_circle
**					ua_colinear_arc
**					ua_multi_point
**					ua_single_point
**					ua_colinear_point
**					ua_single_line
**					ua_center_regen
**					ua_center_line
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       acenter.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:31
*********************************************************************/

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "dasnog.h"
#include "adrfdefs.h"
#include "mdcoord.h"

extern int UD_draftable[UD_NMENTWD];
extern int UD_draft_line[UD_NMENTWD];
extern UU_REAL UA_gap, UA_dot, UA_dash_min, UA_dash_max;
UU_REAL scal_factor, dott, gap, dash, dash_min, dash_max, pattern1, pattern2;

void ua_para_line_blk(),ua_fill_line_blk(),ua_perp_line_blk();
void ua_arc_pattern_fill(),ua_single_line_blk();

/*********************************************************************
**    E_FUNCTION     : int		ua_single_line(ldim)
**       Function to create a simple center-line dimension.
**    PARAMETERS   
**       INPUT  : 
**          ldim                 drafting record
**       OUTPUT :  
**				ldim						undated drafting record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_single_line(ldim)
struct UA_generic_draft	(*ldim);
	{
	int		ablock, d_stat, status, count, dummy, msg;
	UM_coord	xaxis, yaxis, zaxis, location, end_pt, cpln_origin;

	uu_denter(UU_STRC,(us,"ua_single_line(ldim=%s)", "..."));

	ablock = 0;
	count = 0;
	status = 0;
	msg = 158;
	ua_getcpln(ldim,cpln_origin,xaxis,yaxis,zaxis);
	ud_lgeo(UU_TRUE,UD_draftable);

	while(count < 2)
		{
		/* get locations fron the user */
		if(count > 0 ) msg = 159;
		d_stat = ud_world_coord(13,msg,location,1,&dummy,UU_FALSE) ;
		if( d_stat == UA_REJECT || dummy == 0 )
			{
			status = 1;
			goto fexit;
			}
		else
			{
			if( ( dummy>0 ) )
				{
				ablock++;
				um_nptpln(location,cpln_origin,zaxis,end_pt);
				um_vctovc(end_pt,ldim->asso_blk[ablock-1].location);
				ldim->asso_blk[ablock-1].asso_type = 104;
				ldim->asso_blk[ablock-1].key = 0;
				count++;
				}
			}
		}

	/* got all user input - proceed */
	ldim->asso_blk_use = ablock;
	if( ( ablock==0 ) )
		{
		/* ERROR - no entities picked */
		status = 1;
		goto fexit;
		}

	status = ua_single_line_create(ldim);

fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_single_line_create(ldim)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_single_line_create(ldim)
struct UA_generic_draft	*ldim;
	{
	int		n, m, status;
	UM_coord	pt, tmp1, vec;
	UU_REAL distance, two_dott, um_mag(), count2, remainder, patt;

	uu_denter(UU_STRC,(us," ua_single_line_create("));

	n = 1;
	m = 0;
	status = 0;
	dash = dash_min;
	two_dott = 2.0*dott;
	patt = dash + two_dott + 2.0*gap;
	status = ua_init_center_line_blk(ldim, m);
	if(status != 0) goto fexit;
	um_vcmnvc(ldim->asso_blk[1].location, ldim->asso_blk[0].location, vec);
	distance = um_mag(vec);
	um_unitvc(vec, vec);
	um_vctovc(ldim->asso_blk[0].location, pt);
	remainder = distance - dash;
	count2 = 0.0;
	while(remainder > 0.0)
		{
		remainder = remainder - patt;
		count2 = count2 + 1.0;
		}
	count2 = count2 - 1.0;
	if(count2 <= 0.0)
		{
		status = 1;
		uu_uerror0(UA_DRAFTING, 41);
		goto fexit;
		}
	remainder = distance - dash - count2*patt;
	dash = dash + (remainder/(count2 + 1.0));
	if(dash > dash_max)
		{
		status = 1;
		uu_uerror0(UA_DRAFTING, 41);
		goto fexit;
		}

	while(distance > 0.0)
		{
		um_vctovc(pt,ldim->line_blk[m].line_seg[n-1]);
		n++;

		/* fill in dash */
		if(dash > distance)
			{
			dash = distance;
			um_vctmsc(vec, dash, tmp1);
			um_vcplvc(pt,tmp1,pt);
			um_vctovc(pt,ldim->line_blk[m].line_seg[n-1]);
			break;
			}
		else
			{
			um_vctmsc(vec, dash, tmp1);
			um_vcplvc(pt,tmp1,pt);
			um_vctovc(pt,ldim->line_blk[m].line_seg[n-1]);
			distance = distance - dash;
			if( ( n>=46 ) )
				{
				ldim->line_blk[m].num_pts = n;
				n = 0;
				m++;
				status = ua_init_center_line_blk(ldim,m);
				if(status != 0) goto fexit;
				}
			}

		/* skip over gap size */
		if(gap >= distance)
			break;
		else
			{
			n++;
			um_vctmsc(vec, gap, tmp1);
			um_vcplvc(pt, tmp1, pt);
			um_vctovc(pt,ldim->line_blk[m].line_seg[n-1]);
			distance = distance - gap;
			}

		/* fill in dot stroke */
		if(two_dott > distance)
			{
			n++;
			two_dott =  distance;
			um_vctmsc(vec, two_dott, tmp1);
			um_vcplvc(pt,tmp1,pt);
			um_vctovc(pt,ldim->line_blk[m].line_seg[n-1]);
			break;
			}
		else
			{
			n++;
			um_vctmsc(vec, two_dott, tmp1);
			um_vcplvc(pt,tmp1,pt);
			um_vctovc(pt,ldim->line_blk[m].line_seg[n-1]);
			distance = distance - two_dott;
			if( ( n>=46 ) )
				{
				ldim->line_blk[m].num_pts = n;
				n = 0;
				m++;
				ua_init_center_line_blk(ldim,m);
				}
			}

		/* skip over gap size */
		if(gap >= distance)
			break;
		else
			{
			n++;
			um_vctmsc(vec, gap, tmp1);
			um_vcplvc(pt, tmp1, pt);
			distance = distance - gap;
			}
		}
	ldim->line_blk[m].num_pts = n;
	ldim->line_blk_use = m + 1;

fexit:;
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_colinear_point(ldim)
**       Main function to create center-line dimension.
**    PARAMETERS   
**       INPUT  : 
**          ldim                 drafting record
**       OUTPUT :  
**				ldim						undated drafting record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_colinear_point(ldim)
struct UA_generic_draft	(*ldim);
	{
	int		ablock, d_stat, status, count, dummy;
	UM_coord	xaxis, yaxis, zaxis, location, end_pt, cpln_origin;

	uu_denter(UU_STRC,(us,"ua_colinear_point(ldim=%s)", "..."));

	ablock = 0;
	count = 0;
	status = 0;
	ua_getcpln(ldim,cpln_origin,xaxis,yaxis,zaxis);
	ud_lgeo(UU_TRUE,UD_draftable);

get:
	/* get locations fron the user */
	d_stat = ud_world_coord(13,130,location,1,&dummy,UU_FALSE) ;
	if( ( d_stat==UA_REJECT ) )
		{
		status = 1;
		goto fexit;
		}
	else
		{
		if( ( dummy>0 ) )
			{
			ablock = ( ablock+1 );
			um_nptpln(location,cpln_origin,zaxis,end_pt);
			um_vctovc(end_pt,ldim->asso_blk[ablock-1].location);
			ldim->asso_blk[ablock-1].asso_type = 104;
			ldim->asso_blk[ablock-1].key = 0;
			goto get;
			}
		}

	/* got all user input - proceed */
	ldim->asso_blk_use = ablock;
	if( ( ablock==0 ) )
		{
		/* ERROR - no entities picked */
		status = 1;
		goto fexit;
		}

	if( ( ablock==1 ) )
		status = ua_single_point(ldim);
	else
		status = ua_multi_point(ldim);

fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_multi_point(ldim)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_multi_point(ldim)
struct UA_generic_draft	(*ldim);
	{
	int		n_points, i, m, status, n_pts;
	UU_REAL	count1, count2, total_dist, remainder;
	UM_coord	perp_axis, lin_axis, cpln_origin, location_n, xaxis, yaxis,
				zaxis, location, tmp1;

	uu_denter(UU_STRC,(us,"ua_multi_point(ldim=%s)", "..."));

	dash = dash_min;
	pattern1 = ( ( dott+gap )+dash );
	pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
	status = 0;
	ua_getcpln(ldim,cpln_origin,xaxis,yaxis,zaxis);
	um_vctovc(ldim->asso_blk[0].location,location);
	n_points = ldim->asso_blk_use;
	um_vctovc(ldim->asso_blk[n_points-1].location,location_n);
	um_vcmnvc(location_n,location,tmp1);
	um_unitvc(tmp1,lin_axis);
	um_cross(lin_axis,zaxis,tmp1);
	um_unitvc(tmp1,perp_axis);
	total_dist = um_dcccc(location,location_n);
	dash = dash_max;
	pattern1 = ( ( dott+gap )+dash );
	pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
	remainder = ( total_dist-( 2.0*pattern1 ) );
	count2 = 0.0;
	if( ( remainder<0.0 ) )
		{
		dash = ( ( total_dist-( 2.0*dott ) )-( 2.0*gap ) );
		if( ( dash<dash_min ) )
			{
			uu_uerror0(UA_DRAFTING,41);
			status = 1;
			goto fexit;
			}
		}
	else
		{
		for(;;)
			{
			if( ! (( remainder>0.0 )) ) goto us_l175;
			remainder = ( remainder-pattern2 );
			count2 = ( count2+1.0 );
			}
us_l175: ;
		count2 = ( count2-1.0 );
		remainder = ( ( total_dist-( 2.0*pattern1 ) )- ( count2*pattern2 ) );
		if( ( remainder>( 2.0*( gap+dott ) ) ) )
			{
			remainder = ( remainder-( 2.0*( gap+dott ) ) );
			dash = ( dash+( remainder/( count2+2.0 ) ) );
			}
		else
			{
			remainder = ( ( 2.0*( gap+dott ) )-remainder );
			dash = ( dash-( remainder/( count2+2.0 ) ) );
			}
		if( ( dash<dash_min ) )
			{
			uu_uerror0(UA_DRAFTING,41);
			status = 1;
			goto fexit;
			}
		}
	count1 = 0.0;
	m = 0;
	n_pts = 0;
	ua_para_line_blk(ldim,location,lin_axis,dott,gap,dash, count1,count2,
							&m, &n_pts);
	for(i=1;i<=n_points;i++)
		{
		um_vctovc(ldim->asso_blk[i-1].location,location);
		ua_perp_line_blk(ldim,location,perp_axis,dott,gap,dash ,count1,&m,&n_pts);
		}
	ldim->line_blk_use = m+1;
fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_para_line_blk(ldim, center, axis, dott, gap, 
**										dash, count1, count2, m)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_para_line_blk(ldim, center, axis, dott, gap, dash, count1, count2, m, n_pts)
struct UA_generic_draft	(*ldim);
UM_coord	center, axis;
UU_REAL	dott, gap, dash, count1, count2;
int		*m, *n_pts;
	{
	UU_REAL	loop, dist1, dist2, dist_total;

	uu_denter(UU_STRC,(us,"ua_para_line_blk(ldim=%s, center=<%g,%g,%g>,\
		axis=<%g,%g,%g>, dott=%g, gap=%g, dash=%g, count1=%g, count2=%g, m=%d,n=%d)",
		"...", center[0],center[1],center[2], axis[0],axis[1],axis[2],
		dott, gap, dash, count1, count2, *m, *n_pts));

	loop = ( ( count2+3.0 )+( 2.0*count1 ) ) ;
	dist1 = ( ( dott+gap )+dash );
	dist2 = ( ( ( 2.0*dott )+( 2.0*gap ) )+ dash );
	dist_total = ( dist1+( count1*dist2 ) );
	ua_fill_line_blk(ldim,center,axis,dott,gap,dash, dist_total,loop,m,n_pts);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_multi_line_blk(ldim, center, xaxis, yaxis, dott
**								, gap, dash, count)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_multi_line_blk(ldim, center, xaxis, yaxis, dott , gap, dash, count)
struct UA_generic_draft	(*ldim);
UM_coord	center, xaxis, yaxis;
UU_REAL	dott, gap, dash, count;
	{
	int		m, n_pts;
	UU_REAL	loop, dist1, dist2, dist_total;

	uu_denter(UU_STRC,(us," ua_multi_line_blk(ldim=%s, center=<%g,%g,%g>,\
		xaxis=<%g,%g,%g>, yaxis=<%g,%g,%g>, dott=%g, gap=%g, dash=%g, count=%g)",
		"...", center[0],center[1],center[2], xaxis[0],xaxis[1],xaxis[2],
		yaxis[0],yaxis[1],yaxis[2], dott, gap, dash, count));

	m = 0;
	n_pts = 0;
	loop = ( (UU_REAL)ua_trunc(( ( 6.0+( 8.0* count ) )/4.0 )));
	dist1 = ( ( dott+gap )+dash );
	dist2 = ( ( ( 2.0*dott )+( 2.0*gap ) )+ dash );
	dist_total = ( dist1+( count*dist2 ) );
	uu_dprint(UU_STRC,(us,"    m=%d, n=%d", m, n_pts));
	ua_fill_line_blk(ldim,center,xaxis,dott,gap,dash,dist_total,loop,&m, &n_pts);
	uu_dprint(UU_STRC,(us,"    m=%d, n=%d", m, n_pts));
	ua_fill_line_blk(ldim,center,yaxis,dott,gap,dash,dist_total,loop,&m, &n_pts);
	uu_dprint(UU_STRC,(us,"    m=%d, n=%d", m, n_pts));
	ldim->line_blk_use = m+1;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_fill_line_blk(ldim, center, axis, dott, gap, 
**									dash, distance, count, m)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_fill_line_blk(ldim, center, axis, dott, gap, dash, distance, count, m, n_pts)
struct UA_generic_draft	(*ldim);
UM_coord	center, axis;
UU_REAL	dott, gap, dash, distance, count;
int		*m, *n_pts;
	{
	int		status;
	UU_REAL	loop;
	UM_coord	pt, tmp1;

	uu_denter(UU_STRC,(us," ua_fill_line_blk(ldim=%s, center=<%g,%g,%g>,\
		axis=<%g,%g,%g>, dott=%g, gap=%g, dash=%g, distance=%g, count=%g, m=%d ,n=%d)",
		"...", center[0],center[1],center[2], axis[0],axis[1],axis[2], dott, gap,
		dash, distance, count, *m, *n_pts));

	loop = count;
	if(*n_pts == 0) 
		{
		status = ua_init_center_line_blk(ldim, *m);
		if(status != 0) goto fexit;
		}
	else
		{
		if(*n_pts > 45)
			{
			*n_pts = 0;
			*m = *m + 1; 
			status = ua_init_center_line_blk(ldim,*m);
			if(status != 0) goto fexit;
			}
		}
	um_vctmsc(axis,distance,tmp1);
	um_vcmnvc(center,tmp1,pt);
	for(;;)
		{
		if( ! (( loop> 0.1 )) ) goto us_l180;
		um_vctovc(pt,ldim->line_blk[*m].line_seg[*n_pts]);
		*n_pts = *n_pts + 1;
		um_vctmsc(axis,dash,tmp1);
		um_vcplvc(pt,tmp1,pt);
		um_vctovc(pt,ldim->line_blk[*m].line_seg[*n_pts]);
		*n_pts = *n_pts + 1; 
		if( ( *n_pts>=45 ) )
			{
			ldim->line_blk[*m].num_pts = *n_pts;
			*n_pts = 0;
			*m = *m + 1; 
			status = ua_init_center_line_blk(ldim,*m);
			if(status != 0) goto fexit;
			}
		um_vctmsc(axis,gap,tmp1);
		um_vcplvc(pt,tmp1,pt);
		um_vctovc(pt,ldim->line_blk[*m].line_seg[*n_pts]);
		*n_pts = *n_pts + 1;
		um_vctmsc(axis,( 2.0*dott ),tmp1);
		um_vcplvc(pt,tmp1,pt);
		um_vctovc(pt,ldim->line_blk[*m].line_seg[*n_pts]);
		*n_pts = *n_pts + 1;
		um_vctmsc(axis,gap,tmp1);
		um_vcplvc(pt,tmp1,pt);
		loop = ( loop-1.0 );
		}
us_l180: ;
	um_vctovc(pt,ldim->line_blk[*m].line_seg[*n_pts]);
	*n_pts = *n_pts + 1;
	um_vctmsc(axis,dash,tmp1);
	um_vcplvc(pt,tmp1,pt);
	um_vctovc(pt,ldim->line_blk[*m].line_seg[*n_pts]);
	ldim->line_blk[*m].num_pts = *n_pts + 1;
	*n_pts = *n_pts + 1;
	uu_dprint(UU_STRC,(us,"   m= %d, n = %d", *m, *n_pts));
fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_colinear_arc(ldim)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_colinear_arc(ldim)
struct UA_generic_draft	(*ldim);
	{
	struct UA_PLOCREC	plocrec;
	int		relation, ablock, curr_key, ok, s_status, status, count;

	uu_denter(UU_STRC,(us," ua_colinear_arc(ldim=%s)", "..."));

	ud_lgeo(UU_TRUE,UD_draftable);
	ablock = 0;
	count = 0;
	status = 0;
get:
	ablock = ( ablock+1 );
	s_status = ua_select_ent_subf(132,&((*ldim)),ablock,&(plocrec));
	if( ( s_status==UA_TRUE ) )
		{
		curr_key = ldim->asso_blk[ablock-1].key;
		ok = uc_draft_type(curr_key,&(relation));
		if( ( relation!=UA_DRAFT_ARC ) )
			{
			status = 1;
			goto fexit;
			}
		goto get;
		}
	ablock = ( ablock-1 );
	ldim->asso_blk_use = ablock;
	if( ( ablock==0 ) )
		{
		status = 1;
		goto fexit;
		}
	if( ( ablock==1 ) )
		status = ua_single_arc(ldim);
	else
		status = ua_multi_arc(ldim);
fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_multi_arc(ldim)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_multi_arc(ldim)
struct UA_generic_draft	(*ldim);
	{
	int		curr_key, n_arcs, i, m, status, n_pts;
	UM_coord	c_spt, perp_axis, centern, lin_axis, cpln_origin, center, normal,
				c_ept, xaxis, yaxis, zaxis, tmp1;
	UU_REAL  radiusn, count1, count2, radius, remainder, dang, total_dist, dummy;
	UU_LOGICAL	first_time;

	uu_denter(UU_STRC,(us," ua_multi_arc(ldim=%s)", "..."));

	dash = dash_min;
	pattern1 = ( ( dott+gap )+dash );
	pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
	status = 0;
	ua_getcpln(&((*ldim)),cpln_origin,xaxis,yaxis,zaxis);
	curr_key = ldim->asso_blk[0].key;
	uc_draft_arc(curr_key,center,&(radius),&(dang),normal,c_spt,
							c_ept,&(dummy));
	n_arcs = ldim->asso_blk_use;
	curr_key = ldim->asso_blk[n_arcs-1].key;
	uc_draft_arc(curr_key,centern,&(radiusn),&(dang),normal,
						c_spt,c_ept,&(dummy));
	um_vcmnvc(centern,center,tmp1);
	um_unitvc(tmp1,lin_axis);
	um_cross(lin_axis,zaxis,tmp1);
	um_unitvc(tmp1,perp_axis);
	total_dist = um_dcccc(center,centern);
	dash = dash_max;
	first_time = UU_TRUE;
looper:
	pattern1 = ( ( dott+gap )+dash );
	pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
	remainder = ( total_dist-( 2.0*pattern1 ) );
	count2 = 0.0;
	count1 = 0.0;
	if( ( remainder<0.0 ) )
		{
		dash = ( ( total_dist-( 2.0*dott ) )-( 2.0*gap ) );
		if( ( dash<dash_min ) )
			{
			uu_uerror0(UA_DRAFTING,41);
			status = 1;
			goto fexit;
			}
		}
	else
		{
		for(;;)
			{
			if( ! (( remainder>0.0 )) ) goto us_l188;
			remainder = ( remainder-pattern2 );
			count2 = ( count2+1.0 );
			}
us_l188: ;
		count2 = ( count2-1.0 );
		remainder = (( total_dist-( 2.0*pattern1))-(count2*pattern2 ) );
		if( ( remainder>( 2.0*( gap+dott ) ) ) )
			{
			remainder = ( remainder-( 2.0*( gap+dott ) ) );
			dash = ( dash+( remainder/( count2+2.0 ) ) );
			}
		else
			{
			remainder = ( ( 2.0*( gap+dott ) )-remainder );
			dash = ( dash-( remainder/( count2+2.0 ) ) );
			}
		pattern1 = ( ( dott+gap )+dash );
		pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
		if( ( radius>pattern1 ) )
			{
			remainder = ( radius-pattern1 );
			if( ( remainder<pattern2 ) )
				{
				if( ( remainder<( 2.0*( gap+dott ) ) ) )
					{
					if( first_time )
						{
						dash = ( dash-( 4.0*( gap+dott ) ) );
						if( ( dash>dash_min ) )
							{
							first_time = UU_FALSE;
							goto looper;
							}
						else
							{
							uu_uerror0(UA_DRAFTING,41);
							status = 1;
							goto fexit;
							}
						}
					else
						{
						uu_uerror0(UA_DRAFTING,41);
						status = 1;
						goto fexit;
						}
					}
				else
					{
					count1 = 1.0;
					}
				}
			else
				{
				for(;;)
					{
					if( ! (( remainder>0.0 )) ) goto us_l189;
					remainder = ( remainder-pattern2 );
					count1 = ( count1+1.0 );
					}
us_l189: ;
				if( ( (UU_REAL) fabs(remainder)>dash ) )
					{
					if( first_time )
						{
						dash = ( dash-( 4.0*( gap+dott ) ) );
						if( ( dash>dash_min ) )
							{
							first_time = UU_FALSE;
							goto looper;
							}
						else
							{
							uu_uerror0(UA_DRAFTING,41);
							status = 1;
							goto fexit;
							}
						}
					else
						{
						uu_uerror0(UA_DRAFTING,41);
						status = 1;
						goto fexit;
						}
					}
				}
			}
		}
	m = 0;
	n_pts = 0;
	ua_para_line_blk(ldim,center,lin_axis,dott,gap,dash,count1,count2,&m,&n_pts);
	for(i=1;i<=n_arcs;i++)
		{
		curr_key = ldim->asso_blk[i-1].key;
		uc_draft_arc(curr_key,center,&(radius),&(dang),normal,c_spt,
								c_ept,&(dummy));
		ua_perp_line_blk(ldim,center,perp_axis,dott,gap,dash, count1,&m, &n_pts);
		um_vctovc(center, ldim->asso_blk[i-1].location);
		}
	ldim->line_blk_use =  m + 1;
fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_center_regen(ldim)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_center_regen(ldim)
struct UA_generic_draft	*ldim;
	{
	int		mode, ablock, status;
	UU_REAL scal_factor;

	uu_denter(UU_STRC,(us,"ua_center_regen(ldim=%s)", "..."));

	um_get_drwscale(&scal_factor);
	dott = ( ( UA_dot/scal_factor )*0.5 );
	gap = ( UA_gap/scal_factor );
	dash_min = ( UA_dash_min/scal_factor );
	dash_max = ( UA_dash_max/scal_factor );

	mode = ldim->subtype;
	ablock = ldim->asso_blk_use;
	status = 0;
	switch( mode )
		{
		case 1:
			status = ua_single_line_create(ldim);
			break;
		case 2:
			if(  ablock==1  )
				status = ua_single_point(ldim);
			else
				status = ua_multi_point(ldim);
			break;
		case 3:
			if(  ablock==1  )
				status = ua_single_arc(ldim);
			else
				status = ua_multi_arc(ldim);
			break;
		case 4:
			status = ua_full_bolt_create(ldim);
			break;
		case 5:
			status = ua_partial_bolt_create(ldim);
			break;
		}

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_perp_line_blk(ldim, center, axis, dott, gap, 
**										dash, count, m)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_perp_line_blk(ldim, center, axis, dott, gap, dash, count, m, n)
struct UA_generic_draft	(*ldim);
UM_coord	center, axis;
UU_REAL	dott, gap, dash, count;
int		*m, *n;
	{
	UU_REAL	loop, dist1, dist2, dist_total;

	uu_denter(UU_STRC,(us," ua_perp_line_blk(ldim=%s, center=<%g,%g,%g>,\
		axis=<%g,%g,%g>, dott=%g, gap=%g, dash=%g, count=%g, m=%d, n=%d)", "...",
		center[0],center[1],center[2], axis[0],axis[1],axis[2], dott, gap, dash,
		count, *m, *n));

	loop = ( ( 2.0*count )+1.0 );
	dist1 = ( ( dott+gap )+dash );
	dist2 = ( ( ( 2.0*dott )+( 2.0*gap ) )+ dash );
	dist_total = ( dist1+( count*dist2 ) );
	ua_fill_line_blk(ldim,center,axis,dott,gap,dash, dist_total,loop,m,n);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_init_bolt_arc_blk(ldim, n, radius, location)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_init_bolt_arc_blk(ldim, n, radius, location)
struct UA_generic_draft	(*ldim);
int		n;
UU_REAL	radius;
UM_coord	location;
	{

	uu_denter(UU_STRC,(us," ua_init_bolt_arc_blk(ldim=%s, n=%d, radius=%g,\
		location=<%g,%g,%g>)", "...",n,radius,location[0],location[1],
		location[2]));

	ldim->arc_blk[n-1].subtype = dim_arc;
	ldim->arc_blk[n-1].arc.line_font = UA_dim_line_font;
	ldim->arc_blk[n-1].arc.line_density = UA_dim_line_dens;
	ldim->arc_blk[n-1].arc.color = UA_dim_line_color;
	um_vctovc(location,ldim->arc_blk[n-1].center_pt);
	ldim->arc_blk[n-1].radius = radius;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_bolt_arc_blk(ldim, center, location, radius, 
**									s_ang, dott, gap, dash, count2, count1, type)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_bolt_arc_blk(ldim, center, location, radius, 
							s_ang, dott, gap, dash, count2, count1, type)
struct UA_generic_draft	(*ldim);
UM_coord	center, location;
UU_REAL	radius, s_ang, dott, gap, dash, count2, count1;
int		type;
	{
	int		arcpts, n;
	UU_REAL	loop, del_dott, arc_ang, del_gap, del_dash, two_pi;

	uu_denter(UU_STRC,(us," ua_bolt_arc_blk(ldim=%s, center=<%g,%g,%g>,\
		location=<%g,%g,%g>, radius=%g, s_ang=%g, dott=%g, gap=%g, dash=%g,\
		count2=%g, count1=%g, type=%d)", "...", center[0],center[1],center[2],
		location[0],location[1],location[2], radius, s_ang, dott, gap, dash,
		count2, count1, type));

	n = 1;
	ldim->arc_blk_use = n;
	ldim->arc_blk[n-1].num_pts = 0;
	arcpts = 1;
	two_pi = 6.283180e+000;
	ua_init_bolt_arc_blk(&((*ldim)),n,radius,location);
	del_dott = ( dott/radius );
	del_gap = ( gap/radius );
	del_dash = ( dash/radius );
	switch( type )
		{
		case 1:
			{
			loop = ( count2+2.0 );
			arc_ang = ( s_ang-del_dott );
			ua_arc_pattern_fill(ldim,&n,&arcpts,&loop,& arc_ang,(UU_REAL) ( 2.0*del_dott ),
										del_gap,del_dash,two_pi, radius,location);
			}
			break;
		case 2:
			{
			loop = ( ( count2+3.0 )+( 2.0*count1 ) ) ;
			arc_ang = ( ( ( ( s_ang-del_dott )-del_gap )-del_dash )-
						(count1*((( 2.0*del_dott )+( 2.0* del_gap ) )+del_dash )));
			ua_arc_pattern_fill(ldim,&n,&arcpts,&loop,&arc_ang,del_dash,del_gap,
										(UU_REAL) ( 2.0*del_dott ),two_pi, radius,location);
			ldim->arc_blk[n-1].angles[arcpts-1] = arc_ang;
			arcpts = ( arcpts+1 );

			arc_ang = ( arc_ang+del_dash ); 
			if( ( arc_ang>two_pi ) )
				{
				arc_ang = ( arc_ang-two_pi );
				}
			ldim->arc_blk[n-1].angles[arcpts-1] = arc_ang;
			arcpts = ( arcpts+1 );
			}
			break;
		}
	ldim->arc_blk[n-1].num_pts = ( arcpts-1 );
	ldim->arc_blk_use = n;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_centerline_line(key, plocrec, spt, ept)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_centerline_line(key, plocrec, spt, ept)
int		key;
struct UA_PLOCREC	(*plocrec);
UM_coord	spt, ept;
	{
	struct UA_generic_draft	e;
	int		j, i;
	UM_coord end_pts[50];

	uu_denter(UU_STRC,(us," ua_centerline_line(key=%d, plocrec=%s, spt=%s,\
		ept=%s)", key, "...", "...", "..."));

	e.key = key;
	if(uc_retrieve_data(&e, sizeof(struct UA_generic_draft)) == UU_SUCCESS)
		{
		ua_near_on_draft(key,plocrec,spt);
		if(e.etype == UA_CENTERLINE)
			{
			switch(e.subtype)
				{
				case 1:
				case 2:
					for(i=0;i<e.asso_blk_use;i++)
						{
						um_vctovc(e.asso_blk[i].location, end_pts[i]);
						}
					j = um_clospnt(e.asso_blk_use, end_pts, spt);
					um_vctovc(end_pts[j], ept);
					break;
				case 3:
				case 4:
					um_vctovc(e.arc_blk[0].center_pt,ept);
					break;
				}
			}
		else
			{
			um_vctovc(spt, ept);
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_bolt_circle(ldim, type)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_bolt_circle(ldim, type)
struct UA_generic_draft	(*ldim);
int		type;
	{
	struct UA_PLOCREC	plocrec;
	int		relation, ablock, curr_key, ok, s_status, d_stat, 
				status, count, dummy;
	UM_coord	xaxis, yaxis, zaxis, location, end_pt, origin;

	uu_denter(UU_STRC,(us," ua_bolt_circle(ldim=%s, type=%d)", "...", type));

	ud_lgeo(UU_TRUE,UD_draftable);
	ablock = 0;
	count = 0;
	status = 0;
	ua_getcpln(&((*ldim)),origin,xaxis,yaxis,zaxis);
	d_stat = ud_world_coord(13,131,location,1,&(dummy),UU_FALSE);
	if( ( d_stat==UA_REJECT ) )
		{
		status = 1;
		goto fexit;
		}
	else
		{
		if( ( dummy>0 ) )
			{
			ablock = ( ablock+1 );
			um_nptpln(location,origin,zaxis,end_pt);
			um_vctovc(end_pt,ldim->asso_blk[ablock-1].location);
			ldim->asso_blk[ablock-1].asso_type = 104;
			ldim->asso_blk[ablock-1].key = 0;
			goto get;
			}
		}
get:
	ablock = ( ablock+1 );
	s_status = ua_select_ent_subf(133,&((*ldim)),ablock,&(plocrec));
	if( ( s_status==UA_TRUE ) )
		{
		curr_key = ldim->asso_blk[ablock-1].key;
		ok = uc_draft_type(curr_key,&(relation));
		if( ( relation!=UA_DRAFT_ARC ) )
			{
			status = 1;
			goto fexit;
			}
		goto get;
		}
	ablock = ( ablock-1 );
	ldim->asso_blk_use = ablock;
	if( ( ablock<=1 ) )
		{
		status = 1;
		goto fexit;
		}
	switch( type )
		{
		case 1:
			{
			ua_full_bolt_create(&((*ldim)));
			}
			break;
		case 2:
			{
			ua_partial_bolt_create(&((*ldim)));
			}
			break;
		}
fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_full_bolt_create(ldim)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_full_bolt_create(ldim)
struct UA_generic_draft	(*ldim);
	{
	int		n_arcs, curr_key, i, m, status, n_pts;
	UU_REAL	arc_dist, count1, count2, bolt_radius, radius,
				remainder, dang, total_dist, s_ang, count,
				dummy, radius_2;
	UU_LOGICAL	first_time;
	UM_coord	c_spt, lin_axis, cpln_origin, center, normal, e_vec,
				center_2, c_ept, xaxis, yaxis, location, zaxis, s_vec, tmp1;

	uu_denter(UU_STRC,(us," ua_full_bolt_create(ldim=%s)", "..."));

	status = 0;
	ua_getcpln(&((*ldim)),cpln_origin,xaxis,yaxis,zaxis);
	um_vctovc(ldim->asso_blk[0].location,location);
	curr_key = ldim->asso_blk[1].key;
	ldim->asso_blk[0].key = curr_key;
	uc_draft_arc(curr_key,center,&radius,&dang,normal,c_spt,c_ept,&dummy);
	curr_key = ldim->asso_blk[2].key;
	uc_draft_arc(curr_key,center_2,&radius_2,&dang,normal,c_spt,c_ept,&dummy);
	bolt_radius = um_dcccc(center,location);
	total_dist = ( 6.283180e+000*bolt_radius );
uu_dprint(UU_MTRC,(us,"radius %g, total dist %g", bolt_radius, total_dist));
	um_vcmnvc(center,location,tmp1);
	um_unitvc(tmp1,s_vec);
	um_vcmnvc(center_2,location,tmp1);
	um_unitvc(tmp1,e_vec);
	s_ang = um_angle2p(s_vec,e_vec,zaxis);
	arc_dist = ( bolt_radius*s_ang );
uu_dprint(UU_MTRC,(us,"arcdist %g, angle %g", arc_dist, s_ang));
	dash = dash_max;
	first_time = UU_TRUE;
uu_dprint(UU_MTRC,(us,"dot %g, gap %g, dash %g", dott, gap, dash));
looper:
	pattern1 = ( ( dott+gap )+dash );
	pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
	count = 0.0;
	count2 = 0.0;
	count1 = 0.0;
	remainder = ( arc_dist-( 2.0*pattern1 ) );
uu_dprint(UU_MTRC,(us,"remainder 1 %g", remainder));
	if( ( remainder<0.0 ) )
		{
		dash = ( ( arc_dist-( 2.0*dott ) )-( 2.0 *gap ) );
		if( ( dash<dash_min ) )
			{
			uu_uerror0(13,41);
			status = 1;
			goto fexit;
			}
		if( first_time )
			{
			first_time = UU_FALSE;
			goto looper;
			}
		else
			{
			uu_uerror0(13,41);
			status = 1;
			goto fexit;
			}
		}
	else
		{
		for(;;)
			{
uu_dprint(UU_MTRC,(us,"remainder 2 %g", remainder));
			if( ! (( remainder>0.0 )) ) goto us_l195;
			remainder = ( remainder-pattern2 );
			count = ( count+1.0 );
			}
us_l195: ;
		count = ( count-1.0 );
		remainder = (( arc_dist-( 2.0*pattern1))-(count*pattern2 ) );
uu_dprint(UU_MTRC,(us,"remainder 3 %g", remainder));
		if( ( remainder>( 2.0*( gap+dott ) ) ) )
			{
			remainder = ( remainder-( 2.0*( gap+dott ) ) );
			dash = ( dash+( remainder/( count+2.0 ) ) );
			}
		else
			{
			remainder = ( ( 2.0*( gap+dott ) )-remainder );
			dash = ( dash-( remainder/( count+2.0 ) ) );
			}
		pattern1 = ( ( dott+gap )+dash );
		pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
		remainder = ( total_dist-( 2.0*pattern1 ) );
		if( ( remainder<0.0 ) )
			{
			dash = ( ( total_dist-( 2.0*dott ) )-( 2.0*gap ) );
			if( ( dash<dash_min ) )
				{
				uu_uerror0(13,41);
				status = 1;
				goto fexit;
				}
			else
				{
				}
			}
		else
			{
			for(;;)
				{
				if( ! (( remainder>0.0 )) ) goto us_l196;
				remainder = ( remainder-pattern2 );
				count2 = ( count2+1.0 );
				}
us_l196: ;
			count2 = ( count2-1.0 );
			remainder = ((total_dist-(2.0*pattern1))-(count2*pattern2 ));
			if( ( remainder>( 2.0*( gap+dott ) ) ) )
				{
				remainder = ( remainder-( 2.0*( gap+dott ) ) );
				dash = ( dash+( remainder/( count2+2.0 ) ) );
				}
			else
				{
				remainder = ( ( 2.0*( gap+dott ) )-remainder );
				dash = ( dash-( remainder/( count2+2.0 ) ) );
				}
			if( first_time )
				{
				first_time = UU_FALSE;
				goto looper;
				}
			pattern1 = ( ( dott+gap )+dash );
			pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
			if( ( radius>pattern1 ) )
				{
				remainder = ( radius-pattern1 );
uu_dprint(UU_MTRC,(us,"remainder 4 %g", remainder));
				if( ( remainder<pattern2 ) )
					{
					if( ( remainder<( 2.0*( gap+dott ) ) ) )
						count1 = 0.0;
					else
						count1 = 1.0;
					}
				else
					{
					for(;;)
						{
uu_dprint(UU_MTRC,(us,"remainder 5 %g", remainder));
						if( ! (( remainder>0.0 )) ) goto us_l197;
						remainder = ( remainder-pattern2 );
						count1 = ( count1+1.0 );
						}
us_l197: ;
					}
				}
			}
		}
	s_ang = um_angle2p(xaxis,s_vec,zaxis);
uu_dprint(UU_MTRC,(us,"cout2 %g, count1 %g, s_ang %g", count2, count1, s_ang));
	ua_bolt_arc_blk(ldim,center,location,bolt_radius,s_ang
										    ,dott,gap,dash,count2,count1,1);
	m = 0;
	n_pts = 0;
	n_arcs = ldim->asso_blk_use;
	for(i=2;i<=n_arcs;i++)
		{
		curr_key = ldim->asso_blk[i-1].key;
		uc_draft_arc(curr_key,center,&radius,&dang,normal,c_spt, c_ept,&dummy);
		um_vcmnvc(center,location,tmp1);
		um_unitvc(tmp1,lin_axis);
		um_vctovc(center, ldim->asso_blk[i-1].location);
		ua_perp_line_blk(ldim,center,lin_axis,dott,gap,dash, count1,&m,&n_pts);
		}
	ldim->line_blk_use = m+1;
fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_arc_pattern_fill(ldim, n, arcpts, loop, arc_ang
**									, a, b, c, two_pi, radius, location)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_arc_pattern_fill(ldim, n, arcpts, loop, arc_ang
									, a, b, c, two_pi, radius, location)
struct UA_generic_draft	(*ldim);
int		(*n);
int		(*arcpts);
UU_REAL	(*loop);
UU_REAL	(*arc_ang);
UU_REAL	a, b, c, two_pi, radius;
UM_coord	location;
	{

	uu_denter(UU_STRC,(us," ua_arc_pattern_fill(ldim=%s, n=%d, arcpts=%d,\
		loop=%s, arc_ang=%s, a=%g, b=%g, c=%g, two_pi=%g, radius=%g,\
		location=<%g,%g,%g>)", "...", *n, *arcpts, "...", "...", a, b, c, two_pi,
		radius, location[0],location[1],location[2]));

	for(;;)
		{
		if( ! (( (*loop)> 0.1 )) ) goto us_l202;
		ldim->arc_blk[(*n)-1].angles[(*arcpts)-1] = (*arc_ang);
		(*arcpts) = ( (*arcpts)+1 );
		(*arc_ang) = ( (*arc_ang)+a );
		if( ( (*arc_ang)>two_pi ) )
			{
			(*arc_ang) = ( (*arc_ang)-two_pi );
			}
		ldim->arc_blk[(*n)-1].angles[(*arcpts)-1] = (*arc_ang);
		if( ( (*arcpts)>=46 ) )
			{
			ldim->arc_blk[(*n)-1].num_pts = (*arcpts);
			(*arcpts) = 0;
			(*n) = ( (*n)+1 );
			ua_init_bolt_arc_blk(ldim,(*n),radius,location);
			}
		(*arcpts) = ( (*arcpts)+1 );
		(*arc_ang) = ( (*arc_ang)+b );
		if( ( (*arc_ang)>two_pi ) )
			{
			(*arc_ang) = ( (*arc_ang)-two_pi );
			}
		ldim->arc_blk[(*n)-1].angles[(*arcpts)-1] = (*arc_ang);
		(*arcpts) = ( (*arcpts)+1 );
		(*arc_ang) = ( (*arc_ang)+c );
		if( ( (*arc_ang)>two_pi ) )
			{
			(*arc_ang) = ( (*arc_ang)-two_pi );
			}
		ldim->arc_blk[(*n)-1].angles[(*arcpts)-1] = (*arc_ang);
		(*arcpts) = ( (*arcpts)+1 );
		(*arc_ang) = ( (*arc_ang)+b );
		if( ( (*arc_ang)>two_pi ) )
			{
			(*arc_ang) = ( (*arc_ang)-two_pi );
			}
		(*loop) = ( (*loop)-1.0 );
		}
us_l202: 
	;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_single_point(ldim)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_single_point(ldim)
struct UA_generic_draft	(*ldim);
	{
	int		status;
	UM_coord	cpln_origin, xaxis, yaxis, zaxis, location;

	uu_denter(UU_STRC,(us," ua_single_point(ldim=%s)", "..."));

	dash = dash_min;
	status = 0;
	ua_getcpln(&((*ldim)),cpln_origin,xaxis,yaxis,zaxis);
	um_vctovc(ldim->asso_blk[0].location,location);
	ua_single_line_blk(&((*ldim)),location,xaxis,yaxis,dott,gap, dash);
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_center_line()
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_center_line()
	{
	int		curr_key, d_stat, status, mode;
	struct UA_generic_draft	ldim;

	uu_denter(UU_STRC,(us," ua_center_line()"));

	/* initilize default pattern sizes */
	um_get_drwscale(&(scal_factor));
	dott = ( ( UA_dot/scal_factor )* 0.5 );
	gap = ( UA_gap/scal_factor );
	dash_min = ( UA_dash_min/scal_factor );
	dash_max = ( UA_dash_max/scal_factor );

main_loop:
	/* main loop - get users choice for type */
	d_stat = ua_popmenu(24,&(mode));

	if( ( d_stat==2 ) )
		{
		/* re-define pattern variables */
		ua_set_centerline_attr();
		dott = ( ( UA_dot/scal_factor )* 0.5 );
		gap = ( UA_gap/scal_factor );
		dash_min = ( UA_dash_min/scal_factor );
		dash_max = ( UA_dash_max/scal_factor );
		goto main_loop;
		}

	/* user rejected out of the menu */
	if( ( ( d_stat==UA_OPCOMPLETE )&&( mode==0 ) ) )
		{
		goto fexit;
		}

	/* switch on type of function selected */
	status = 0;
	switch( mode )
		{
		case 1:
			while (status == 0)
				{
				ua_init_entity(UA_CENTERLINE,mode,&(ldim));
				status = ua_single_line(&ldim);
				if( ( status==0 ) )
					{
					ua_create_entity(&ldim,&curr_key);
					uc_display(&(ldim));
					}
				}
			break;
		case 2:
			while (status == 0)
				{
				ua_init_entity(UA_CENTERLINE,mode,&ldim);
				status = ua_colinear_point(&ldim);
				if( ( status==0 ) )
					{
					ua_create_entity(&ldim,&curr_key);
					uc_display(&ldim);
					}
				}
			break;
		case 3:
			while (status == 0)
				{
				ua_init_entity(UA_CENTERLINE,mode,&ldim);
				status = ua_colinear_arc(&ldim);
				if( ( status==0 ) )
					{
					ua_create_entity(&ldim,&curr_key);
					uc_display(&ldim);
					}
				}
			break;
		case 4:
			ua_init_entity(UA_CENTERLINE,mode,&ldim);
			status = ua_bolt_circle(&ldim,1);
			if( ( status==0 ) )
				{
				ua_create_entity(&ldim,&curr_key);
				uc_display(&(ldim));
				}
			break;
		case 5:
			ua_init_entity(UA_CENTERLINE,mode,&ldim);
			status = ua_bolt_circle(&ldim,2);
			if( ( status==0 ) )
				{
				ua_create_entity(&ldim,&curr_key);
				uc_display(&(ldim));
				}
			break;
		}

	goto main_loop;
fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_init_center_line_blk(ldim, m)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_init_center_line_blk(ldim, m)
struct UA_generic_draft	(*ldim);
int		m;
	{
	int status;

	uu_denter(UU_STRC,(us," ua_init_center_line_blk(ldim=%s, m=%d)",
			"...", m));

	status = 0;
	if(m > 4) 
		{
		status = 1;
		goto fexit;
		}
	else
		{
		ldim->line_blk[m].subtype = dim_line;
		ldim->line_blk[m].line.line_font = 0;
		ldim->line_blk[m].line.line_density = UA_dim_line_dens;
		ldim->line_blk[m].line.color = UA_dim_line_color;
		}
fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_single_line_blk(ldim, center, xaxis, yaxis, 
**										dott, gap, dash)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_single_line_blk(ldim, center, xaxis, yaxis, dott, gap, dash)
struct UA_generic_draft	(*ldim);
UM_coord	center, xaxis, yaxis;
UU_REAL	dott, gap, dash;
	{
	UM_coord 	tmp1;

	uu_denter(UU_STRC,(us," ua_single_line_blk(ldim=%s, center=<%g,%g,%g>,\
		xaxis=<%g,%g,%g>, yaxis=<%g,%g,%g>, dott=%g, gap=%g, dash=%g)", "...",
		center[0],center[1],center[2], xaxis[0],xaxis[1],xaxis[2],
		yaxis[0],yaxis[1],yaxis[2], dott, gap, dash));

	ldim->line_blk_use = 1;
	ldim->line_blk[0].num_pts = 12;
	ldim->line_blk[0].subtype = dim_line;
	ldim->line_blk[0].line.line_font = 0;
	ldim->line_blk[0].line.line_density = UA_dim_line_dens;
	ldim->line_blk[0].line.color = UA_dim_line_color;
	um_vctmsc(xaxis,( ( dott+gap )+dash ),tmp1);
	um_vcmnvc(center,tmp1,ldim->line_blk[0].line_seg[0]);
	um_vctmsc(xaxis,( dott+gap ),tmp1);
	um_vcmnvc(center,tmp1,ldim->line_blk[0].line_seg[1]);
	um_vctmsc(xaxis,dott,tmp1);
	um_vcmnvc(center,tmp1,ldim->line_blk[0].line_seg[2]);
	um_vctmsc(xaxis,dott,tmp1);
	um_vcplvc(center,tmp1,ldim->line_blk[0].line_seg[3]);
	um_vctmsc(xaxis,( dott+gap ),tmp1);
	um_vcplvc(center,tmp1,ldim->line_blk[0].line_seg[4]);
	um_vctmsc(xaxis,( ( dott+gap )+dash ),tmp1);
	um_vcplvc(center,tmp1,ldim->line_blk[0].line_seg[5]);
	um_vctmsc(yaxis,( ( dott+gap )+dash ),tmp1);
	um_vcmnvc(center,tmp1,ldim->line_blk[0].line_seg[6]);
	um_vctmsc(yaxis,( dott+gap ),tmp1);
	um_vcmnvc(center,tmp1,ldim->line_blk[0].line_seg[7]);
	um_vctmsc(yaxis,dott,tmp1);
	um_vcmnvc(center,tmp1,ldim->line_blk[0].line_seg[8]);
	um_vctmsc(yaxis,dott,tmp1);
	um_vcplvc(center,tmp1,ldim->line_blk[0].line_seg[9]);
	um_vctmsc(yaxis,( dott+gap ),tmp1);
	um_vcplvc(center,tmp1,ldim->line_blk[0].line_seg[10]);
	um_vctmsc(yaxis,( ( dott+gap )+dash ),tmp1);
	um_vcplvc(center,tmp1,ldim->line_blk[0].line_seg[11]);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_partial_bolt_create(ldim)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_partial_bolt_create(ldim)
struct UA_generic_draft	(*ldim);
	{
	int		curr_key, n_arcs, i, m, status, n_pts;
	UU_REAL	arc_dist, count1, count2, bolt_radius, radius,
				remainder, dang, s_ang, dummy,
				radius_2;
	UM_coord	c_spt, lin_axis, cpln_origin, center, normal, e_vec, center_2,
				c_ept, xaxis, yaxis, location, zaxis, s_vec, tmp1;
	UU_LOGICAL	first_time;

	uu_denter(UU_STRC,(us," ua_partial_bolt_create(ldim=%s)", "..."));

	status = 0;
	ua_getcpln(&((*ldim)),cpln_origin,xaxis,yaxis,zaxis);
	n_arcs = ldim->asso_blk_use;
	um_vctovc(ldim->asso_blk[0].location,location);
	curr_key = ldim->asso_blk[1].key;
	ldim->asso_blk[0].key = curr_key;
	uc_draft_arc(curr_key,center,&(radius),&(dang),normal,c_spt,
	c_ept,&(dummy));
	curr_key = ldim->asso_blk[n_arcs-1].key;
	uc_draft_arc(curr_key,center_2,&(radius_2),&(dang),normal,
	c_spt,c_ept,&(dummy));
	bolt_radius = um_dcccc(center,location);
	um_vcmnvc(center,location,tmp1);
	um_unitvc(tmp1,s_vec);
	um_vcmnvc(center_2,location,tmp1);
	um_unitvc(tmp1,e_vec);
/*
.....the object is picked clockwise, so the anger shold be from end to start
*/
	s_ang = um_angle2p(e_vec,s_vec,zaxis);
/*	s_ang = um_angle2p(s_vec,e_vec,zaxis); */
	arc_dist = ( bolt_radius*s_ang );
	dash = dash_max;
	first_time = UU_TRUE;
looper:
	pattern1 = ( ( dott+gap )+dash );
	pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
	count2 = 0.0;
	count1 = 0.0;
	remainder = ( arc_dist-( 2.0*pattern1 ) );
	if( ( remainder<0.0 ) )
		{
		dash = ( ( arc_dist-( 2.0*dott ) )-( 2.0 *gap ) );
		if( ( dash<dash_min ) )
			{
			uu_uerror0(UA_DRAFTING,41);
			status = 1;
			goto fexit;
			}
		if( first_time )
			{
			first_time = UU_FALSE;
			goto looper;
			}
		else
			{
			uu_uerror0(13,41);
			status = 1;
			goto fexit;
			}
		}
	else
		{
		for(;;)
			{
			if( ! (( remainder>0.0 )) ) goto us_l217;
			remainder = ( remainder-pattern2 );
			count2 = ( count2+1.0 );
			}
us_l217: ;
		count2 = ( count2-1.0 );
		remainder = ( ( arc_dist-( 2.0*pattern1 ) )-( count2*pattern2 ) );
		if( ( remainder>( 2.0*( gap+dott ) ) ) )
			{
			remainder = ( remainder-( 2.0*( gap+dott ) ) );
			dash = ( dash+( remainder/( count2+2.0 ) ) );
			}
		else
			{
			remainder = ( ( 2.0*( gap+dott ) )-remainder );
			dash = ( dash-( remainder/( count2+2.0 ) ) );
			}
		pattern1 = ( ( dott+gap )+dash );
		pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
		if( ( radius>pattern1 ) )
			{
			remainder = ( radius-pattern1 );
			if( ( remainder<pattern2 ) )
				{
				if( ( remainder<( 2.0*( gap+dott ) ) ) )
					{
					if( first_time )
						{
						dash = ( dash-( 4.0*( gap+dott ) ) );
						if( ( dash>dash_min ) )
							{
							first_time = UU_FALSE;
							goto looper;
							}
						else
							{
							uu_uerror0(UA_DRAFTING,41);
							status = 1;
							goto fexit;
							}
						}
					else
						{
						uu_uerror0(UA_DRAFTING,41);
						status = 1;
						goto fexit;
						}
					}
				else
					{
					count1 = 1.0;
					}
				}
			else
				{
				for(;;)
					{
					if( ! (( remainder>0.0 )) ) goto us_l218;
					remainder = ( remainder-pattern2 );
					count1 = ( count1+1.0 );
					}
us_l218: ;
				if( ( (UU_REAL) fabs(remainder)>dash ) )
					{
					if( first_time )
						{
						dash = ( dash-( 4.0*( gap+dott ) ) );
						if( ( dash>dash_min ) )
							{
							first_time = UU_FALSE;
							goto looper;
							}
						else
							{
							uu_uerror0(UA_DRAFTING,41);
							status = 1;
							goto fexit;
							}
						}
					else
						{
						uu_uerror0(UA_DRAFTING,41);
						status = 1;
						goto fexit;
						}
					}
				}
			}
		}
/*
.....the object is picked clockwise, so the anger shold be from end vector
*/
/*	s_ang = um_angle2p(xaxis,e_vec,zaxis); */
	s_ang = um_angle2p(xaxis,e_vec,zaxis);
	ua_bolt_arc_blk(ldim,center,location,bolt_radius,s_ang
	   										 ,dott,gap,dash,count2,count1,2);
	m = 0;
	n_pts = 0;

	for(i=2;i<=n_arcs;i++)
		{
		curr_key = ldim->asso_blk[i-1].key;
		uc_draft_arc(curr_key,center,&(radius),&(dang),normal,c_spt,
							c_ept,&(dummy));
		um_vcmnvc(center,location,tmp1);
		um_unitvc(tmp1,lin_axis);
		um_vctovc(center, ldim->asso_blk[i-1].location);
		ua_perp_line_blk(ldim,center,lin_axis,dott,gap,dash, count1,&m,&n_pts);
		}
	ldim->line_blk_use = m+1;

fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_centerline_arc(key, center, radius, dang, 
**										normal, spt, ept, arc_len)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_centerline_arc(key, center, radius, dang, 
normal, spt, ept, arc_len)
int		key;
UU_REAL	*radius, *dang, *arc_len;
UM_coord	center, normal, spt, ept;
	{
	struct UA_generic_draft	e;
	int		j;
	UU_REAL	e_ang, s_ang;
	UM_coord	origin, 	e_vec, 	xaxis, 	yaxis, 	zaxis, 	s_vec,
				tmp1, tmp2, tmp3;

	uu_denter(UU_STRC,(us," ua_centerline_arc(key=%d, center=%s, radius=%s,\
		dang=%s, normal=%s, spt=%s, ept=%s, arc_len=%s)", key, "...", "...",
		"...", "...", "...", "...", "..."));

	e.key = key;
	j = uc_retrieve_data(&(e),sizeof(struct UA_generic_draft	));
	ua_getcpln(&(e),origin,xaxis,yaxis,zaxis);
	if( ( e.arc_blk_use>0 ) )
		{
		*radius = e.arc_blk[0].radius;
		um_vctovc(e.arc_blk[0].center_pt,center);
		um_vctovc(zaxis,normal);
		s_ang = e.arc_blk[0].angles[0];
		j = e.arc_blk[e.arc_blk_use-1].num_pts;
		e_ang = e.arc_blk[e.arc_blk_use-1].angles[j-1];
		um_vctmsc(xaxis,cos(s_ang),tmp1);
		um_vctmsc(yaxis,sin(s_ang),tmp2);
		um_vcplvc(tmp1,tmp2,tmp3);
		um_vctmsc(tmp3,(*radius),tmp1);
		um_vcplvc(center,tmp1,spt);
		um_vctmsc(xaxis,cos(e_ang),tmp1);
		um_vctmsc(yaxis,sin(e_ang),tmp2);
		um_vcplvc(tmp1,tmp2,tmp3);
		um_vctmsc(tmp3,(*radius),tmp1);
		um_vcplvc(center,tmp1,ept);
		um_vcmnvc(spt,center,tmp1);
		um_unitvc(tmp1,s_vec);
		um_vcmnvc(ept,center,tmp1);
		um_unitvc(tmp1,e_vec);
		(*dang) = um_angle2p(s_vec,e_vec,zaxis);
		(*arc_len) = ( (*dang)*(*radius) );
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_single_arc(ldim)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_single_arc(ldim)
struct UA_generic_draft	(*ldim);
	{
	int		curr_key, status;
	UU_REAL	past, radius, remainder, delta, dang, distance,
				count, dummy;
	UM_coord	c_spt, cpln_origin, center, normal, c_ept, xaxis, yaxis, zaxis;

	uu_denter(UU_STRC,(us," ua_single_arc(ldim=%s)", "..."));

	dash = dash_min;
	pattern1 = ( ( dott+gap )+dash );
	pattern2 = ( ( ( 2.0*dott )+( 2.0*gap ))+dash );
	status = 0;
	ua_getcpln(ldim,cpln_origin,xaxis,yaxis,zaxis);
	curr_key = ldim->asso_blk[0].key;
	uc_draft_arc(curr_key,center,&radius,&dang,normal,c_spt, c_ept,&dummy);
	um_vctovc(center, ldim->asso_blk[0].location);
	if( ( radius<pattern1 ) )
		{
		past = ( pattern1-radius );
		if( ( past<gap ) )
			{
			dash = ( dash+( gap-past ) );
			}
		ua_single_line_blk(ldim,center,xaxis,yaxis,dott,gap, dash);
		}
	else
		{
		dash = dash_max;
		pattern1 = ( ( gap+dott )+dash );
		if( ( radius<pattern1 ) )
			{
			past = ( pattern1-radius );
			if( ( past<gap ) )
				{
				dash = ( dash+( gap-past ) );
				}
			else
				{
				dash = ( dash-( past-gap ) );
				}
			ua_single_line_blk(ldim,center,xaxis,yaxis,dott,gap, dash);
			}
		else
			{
			pattern1 = ( ( dott+gap )+dash_min );
			dash = dash_min;
			remainder = ( radius-pattern1 );
			count = 0.0;
			for(;;)
				{
				if( ! (( remainder>0.0 )) ) goto us_l233;
				remainder = ( remainder-pattern2 );
				count = ( count+1.0 );
				}
us_l233: ;
			if( ( (UU_REAL) fabs(remainder)>( dash-gap ) ) )
				{
				count = ( count-1.0 );
				distance = ( ( radius-pattern1 )-( count*pattern2 ) );
				delta = ( ( distance+gap )/( count+1.0 ) );
				dash = ( dash+delta );
				ua_multi_line_blk(ldim,center,xaxis,yaxis,dott, gap,dash,count);
				}
			else
				{
				ua_multi_line_blk(ldim,center,xaxis,yaxis,dott,gap, dash,count);
				}
			}
		}
	uu_dexit;
	return(status);
	}
