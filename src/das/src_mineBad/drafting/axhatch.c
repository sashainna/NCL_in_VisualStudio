/*********************************************************************
**    NAME         : axhatch.c
**       CONTAINS:
**				ua_xhatch_exec()	--	(re)generates cross hatching
**				ua_xhatch_isect()	--	finds all xhatch intersections
**				ua_xhatch_dash()	--	adds dash pattern to intersection list
**				ua_xhatch_generate()	--	generates line segments from list
**				ua_xhatch_getcpln()	--	loads entity with xhatch plane
**				ua_xhatch_fixlist()	--	removes double intersections
**				ua_xh_tangent()		-- checks for "pseudo-tangents"
**				ua_xhatch()			--	user interface
**				ua_xhatch_regen()			--	regenerate cross-hatch entity
**    		ua_mod_xh_attr()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       axhatch.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:42
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) axhatch.c 3.9 6/7/88 08:18:00 single"};
#else
static char uu_sccsident[]={"@(#) axhatch.c 3.9 6/7/88 08:18:00 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"
#include "axhatch.h"
#include "adrf.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdattr.h"

static UU_LOGICAL	UA_first_call = UU_TRUE;
UU_LIST 	xh_table;						/* table of cross-hatch patter data	*/
static UU_LIST  UA_hatch_line_list; /* list of hatch line endpoints  */
extern int UD_draftable[UD_NMENTWD];	/* DAS select mask */
extern int UD_xhatchcurves[UD_NMENTWD];	/* DAS select mask */
UU_REAL 	UA_hatch_common_plane[2][3];	/* common plane for um_isect_sp */
UU_REAL    UA_xh_size;  /* size of xhatch region */
UA_xh_attr_rec	UA_xh_defaults;	/* xhatch default attributes */
UU_LOGICAL UA_xh_regeneration = UU_FALSE;
static int save_xh_int[4];
static UU_REAL  save_xh_real[2];
extern UU_KEY_ID UR_last_mod_mkey;

/*********************************************************************
**    E_FUNCTION     : us_init_axhatch()
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
us_init_axhatch()
	{
	UA_first_call = UU_TRUE;
	}

/*********************************************************************
**    E_FUNCTION     : enum UA_xhatch_error	ua_xhatch_getcpln(entity)
**       gets construction plane for cross-hatching from geometry of
**			entities whose keys are stored in the entity asso_blocks.
**			construction coord system is supposed to be "coherent with"
**			the model system.
**    PARAMETERS   
**       INPUT  : 
**          UA_generic_draft:	entity;	
**       OUTPUT :  
**          entity.cpln filled in
**    RETURNS      : UA_xhatch_error: in particular, illegal_boundary_type
**							if geometry in asso_blk is not valid type
**							also: region_not_planar may be common
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
enum UA_xhatch_error	ua_xhatch_getcpln(entity)
	struct UA_generic_draft	(*entity);
	{
	int		i, j, k, nint, key, ab_dim, netdim, jth_ab;
	UM_coord	box[2], cpln_origin, endpoint_1, endpoint_2,
				cc_box, temp_pt, xh_box[2], ab_space[2], cpln_xaxis,
				cpln_yaxis, cpln_zaxis, netspace[2];
	UU_LOGICAL ua_gnxt();
	enum UA_xhatch_error	error;
	
	uu_denter(UU_STRC,(us,"ua_xhatch_getcpln(entity=%s)", "..."));
	
	error = hatch_ok;
	netdim = -1;
		{
		int		us_t116;
		us_t116 = (*entity).asso_blk_use;
		jth_ab = 1;
		for(;;)
			{
			if( jth_ab > us_t116 ) 	break;
			ua_span_entity((*entity).asso_blk[jth_ab-1].key,&(ab_dim),
									ab_space);
			um_netspan(netdim,netspace,ab_dim,ab_space,&(netdim),
									netspace);
			if( ( netdim==3 ) )
				{
				error = region_not_planar;
				uu_uerror0(13,13);
				goto Done;
				}
			jth_ab++ ;
			}
		}
	if( ( netdim!=2 ) )
		{
		error = region_not_planar;
		uu_uerror0(13,13);
		goto Done;
		}
	for(i=0;i<3;i++)
		{
		cpln_origin[i] = 0.0;
		cpln_xaxis[i] =  0.0;
		cpln_yaxis[i] =  0.0;
		cpln_zaxis[i] =  0.0;
		}
	cpln_xaxis[0] = 1.000000e+000;
	cpln_yaxis[1] = 1.000000e+000;
	cpln_zaxis[2] = 1.000000e+000;

	um_ilnpln(cpln_origin,netspace[1],netspace[0],netspace[1],
							&(nint),(*entity).cpln.cpln_origin);
	if( ( um_dot(netspace[1],cpln_zaxis)<0.000000e+000 ) )
		{
			{
			UU_REAL	us_t117[3];
			um_unitvc(netspace[1],us_t117);
			um_vctmsc(us_t117,(UU_REAL) -1.000000e+000,(*entity).cpln.zaxis);
			}
		}
	else
		{
		um_unitvc(netspace[1],(*entity).cpln.zaxis);
		}
	if( um_vcparall(netspace[1],cpln_xaxis) )
		{
		um_nptpln(cpln_origin,netspace[0],netspace[1],endpoint_1);
		um_vcplvc(cpln_origin,cpln_yaxis,temp_pt);
		um_nptpln(temp_pt,netspace[0],netspace[1],endpoint_2);
			{
			UU_REAL	us_t118[3];
			um_vcmnvc(endpoint_2,endpoint_1,us_t118);
			um_unitvc(us_t118,(*entity).cpln.yaxis);
			}
		um_cross((*entity).cpln.yaxis,(*entity).cpln.zaxis,(*entity)
								.cpln.xaxis);
		}
	else
		{
		um_nptpln(cpln_origin,netspace[0],netspace[1],endpoint_1);
		um_vcplvc(cpln_origin,cpln_xaxis,temp_pt);
		um_nptpln(temp_pt,netspace[0],netspace[1],endpoint_2);
			{
			UU_REAL	us_t119[3];
			um_vcmnvc(endpoint_2,endpoint_1,us_t119);
			um_unitvc(us_t119,(*entity).cpln.xaxis);
			}
		um_cross((*entity).cpln.zaxis,(*entity).cpln.xaxis,(*entity)
						.cpln.yaxis);
		}
	um_vctovc((*entity).cpln.cpln_origin,UA_hatch_common_plane[1 -1]);
	um_vctovc((*entity).cpln.zaxis,UA_hatch_common_plane[1]);

	/* calculate and store pattern size if not regeneration */
	if(UA_xh_regeneration == UU_FALSE)
		{
		for(i=0;i<3;i++)
			{
			xh_box[0][i] = 1.000000e+015;
			xh_box[1][i] = -1.000000e+015;
			}
		ua_init_select();
		for(;;)
			{
			if( ! (ua_gnxt(&(key))) ) goto us_l120;
			if( ( ua_xh_entity_box(key,box)==0 ) )
				{
				um_nptpln(box[0],UA_hatch_common_plane[0],
				UA_hatch_common_plane[1],cc_box);
				if( ( cc_box[0]<xh_box[0][0] ) ) xh_box[0][0] = cc_box[0];
				if( ( cc_box[1]<xh_box[0][1] ) ) xh_box[0][1] = cc_box[1];
				if( ( cc_box[2]<xh_box[0][2] ) ) xh_box[0][2] = cc_box[2];
				um_nptpln(box[1],UA_hatch_common_plane[0],
												UA_hatch_common_plane[1],cc_box);
				if( ( cc_box[0]>xh_box[1][0] ) ) xh_box[1][0] = cc_box[0];
				if( ( cc_box[1]>xh_box[1][1] ) ) xh_box[1][1] = cc_box[1];
				if( ( cc_box[2]>xh_box[1][2] ) ) xh_box[1][2] = cc_box[2];
				}
			}
us_l120: ;
		UA_xh_size = um_dcccc(xh_box[0],xh_box[1]);
		entity->asso_blk[0].location[0] = UA_xh_size;
		}
	else
		{
		UA_xh_size = entity->asso_blk[0].location[0];
		if(UA_xh_size == 0.0) UA_xh_size = 100.0;
		}
Done:
	uu_dexit;
	return(error);
	}
		
/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL	ua_xh_tangent(ilnode1, ilnode2)
**       checks for "pseudo-tangents" - i.e. a double intersection
**			where the hatch line is tangent to a corner formed by
**			two boundary entities
**    PARAMETERS   
**       INPUT  : 
**          UA_ilnode:	ilnode1 - first intersection record
**			  UA_ilnode:	ilnode2 - second intersection record
**       OUTPUT :  
**				none
**    RETURNS      : LOGICAL - TRUE if pseudo-tangent exists
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL	ua_xh_tangent(ilnode1, ilnode2)
struct UA_ilnode	(*ilnode1);
struct UA_ilnode	(*ilnode2);
	{
	int		ret_val;
	UU_REAL	dot1;
	UU_LOGICAL	is_tangent;
	UM_coord	line_dir, cross1, cross2, tan1, tan2;
	
	uu_denter(UU_STRC,(us,"ua_xh_tangent(ilnode1=%s,ilnode2=%s)","...","..."));
	
	if( (! um_cceqcc( (*ilnode1).line_dir, (*ilnode2).line_dir) ) )
		{
		is_tangent = UU_FALSE;
		goto Done;
		}
	um_vctovc((*ilnode1).line_dir,line_dir);
	ret_val = ua_xh_evcrv(1,(*ilnode1).key,(*ilnode1).t1,tan1);
	if( ( ret_val!=0 ) )
		{
		}
	ret_val = ua_xh_evcrv(1,(*ilnode2).key,(*ilnode2).t1,tan2);
	if( ( ret_val!=0 ) )
		{
		}
	if( ( (UU_REAL) fabs((*ilnode1).t1)<5.000000e-001 ) )
		{
		um_cross(line_dir,tan1,cross1);
		}
	else
		{
		um_cross(tan1,line_dir,cross1);
		}
	if( ( (UU_REAL) fabs((*ilnode2).t1)<5.000000e-001 ) )
		{
		um_cross(line_dir,tan2,cross2);
		}
	else
		{
		um_cross(tan2,line_dir,cross2);
		}
	dot1 = um_dot(cross1,cross2);
	if( ( dot1>0.000000e+000 ) )
		{
		is_tangent = UU_TRUE;
		}
	else
		{
		is_tangent = UU_FALSE;
		}
Done:
	uu_dexit;
	return(is_tangent);
	}

/*********************************************************************
**    E_FUNCTION     : ua_xhatch_fixlist(ilist)
**       checks for double intersections and reduces them to one
**			or zero 
**    PARAMETERS   
**       INPUT  : 
**          ilist	--	intersection list
**       OUTPUT :  
**          ilist --	extra intersection entities removed
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_xhatch_fixlist(ilist)
UU_LIST	(*ilist);
	{
	int		ix;
	int		iy;
	UU_REAL	close_dist;
	UU_LOGICAL ua_xh_tangent();
	
	uu_denter(UU_STRC,(us,"ua_xhatch_fixlist(ilist=%d items)",
		UU_LIST_LENGTH(ilist)));
	
	ix = 1;
	for(;;)
		{
		if( ! (( ix<UU_LIST_LENGTH(ilist) )) ) goto us_l121;
		close_dist = ( ((struct UA_ilnode	(*)) ((ilist)->data))[( 
				ix+1 )-1].t0-((struct UA_ilnode	(*)) ((ilist)->data))[ix -1].t0 );
		if( ( ( ((struct UA_ilnode	(*)) ((ilist)->data))[ix-1].
					event==region_boundary )&&( ((struct UA_ilnode	(*)) ((
					ilist)->data))[( ix+1 )-1].event==region_boundary ) ) )
			{
			if( ( close_dist<UA_FUZZ ) )
				{
				uu_list_delete(ilist,(ix)-1,1);
				continue;
				}
			}
		else
			{
			if( ( close_dist<1.000000e-001 ) )
				{
				if( ( ua_xh_tangent(&(((struct UA_ilnode	(*)) ((ilist
							)->data))[ix-1]),&(((struct UA_ilnode	(*)) ((ilist
							)->data))[( ix+1 )-1]))==UU_TRUE ) )
					{
					uu_list_delete(ilist,(ix)-1,(( ix+1 )) - ((ix)-1));
					continue;
					}
				else
					{
					uu_list_delete(ilist,(ix)-1,1);
					continue;
					}
				}
			}
		ix = ( ix+1 );
		}
us_l121: ;
	ix = UU_LIST_LENGTH(ilist);
	iy = ( ( ix/2 )*2 );
	if( ( ( ix-iy )!=0 ) )
		{
		if( ( ((struct UA_ilnode	(*)) ((ilist)->data))[( ix-1 )-1]
						.event!=near_endpoint ) )
			{
			uu_list_delete(ilist,(ix)-1,1);
			}
		else
			{
			uu_list_delete(ilist,(( ix-1 ))-1,1);
			}
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : enum UA_xhatch_error	ua_xhatch_isect(entity, ilist,
**										xvers , spacing, dir, offset)
**       fills in hatch event ilist with bdy intersections and masks
**			this function is called for a single "line family", a set of
**			evenly spaced parallel (possibly dashed) lines
**    PARAMETERS   
**       INPUT  : 
**          UA_generic_draft:	entity;	asso_blk's used
**				VAR LIST OF UA_ilnode:		ilist;		allocated, but empty
**				COORD:			xvers;		UNIT vector perpendicular to pattern
**				COORD:			spacing;		spacing of line family (model units)
**				COORD:			dir;			direction of hatch lines;
**				REAL:				offset;		offset for line family
**
**       OUTPUT :  
**          					ilist;			filled in with intersection events
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
enum UA_xhatch_error	ua_xhatch_isect(entity, ilist, xvers
, spacing, dir, offset)
struct UA_generic_draft	(*entity);
UU_LIST	(*ilist);
UU_REAL	spacing, offset;
UM_coord	xvers, dir;
	{
	int		first_line_id, nint, line_id, key, ith_ab, ret_val, real_nint,
				jth_isect;
	UU_LOGICAL	increasing;
	struct UA_ilnode	ievent;
	enum UA_xhatch_error	error;
	struct UA_isect	ibuf[200];
	UM_coord	origin, point_on_bdy, line_basepoint, proj_point;
	
	uu_denter(UU_STRC,(us,
		"ua_xhatch_isect(entity=%s,ilist=%d items,xvers=<%g,%g,%g>, spacing=%g,\
		dir=<%g,%g,%g>, offset=%g)", "...", UU_LIST_LENGTH(ilist),
		xvers[0],xvers[1],xvers[2], spacing, dir[0],dir[1],dir[2], offset));
	
	um_vctovc((*entity).cpln.cpln_origin,origin);
	error = hatch_ok;
	ua_flushcache();
		{
		int		us_t124;
		us_t124 = (*entity).asso_blk_use;
		ith_ab = 1;
		for(;;)
			{
			if( ith_ab > us_t124 ) 	break;
			key = (*entity).asso_blk[ith_ab-1].key;
			switch( (*entity).asso_blk[ith_ab-1].asso_type )
				{
				case 6:
					{
					ret_val = ua_xh_evcrv(-1,key,(UU_REAL) 0.000000e+000,point_on_bdy);
					if( ( ret_val!=0 ) )
						{
						error = system_error;
						}
					}
					break;
				default:
					{
					uu_dexit;
					return(illegal_boundary_type);
					}
				}
			um_nptln(point_on_bdy,origin,xvers,proj_point);
			first_line_id = ( ua_floor(( ( um_dot(proj_point,xvers)/
									spacing )-offset ))+1 );
			increasing = UU_TRUE;
			line_id = first_line_id;
Start_walking:
			for(;;)
				{
					{
					UU_REAL	us_t126[3];
					um_vctmsc(xvers,( ( ( (UU_REAL)line_id )+offset )*spacing ),
									us_t126);
					um_vcplvc(origin,us_t126,line_basepoint);
					}
				ret_val = ua_isect_line(line_basepoint,dir,key,&(nint),200,
								ibuf);
				if( ( ret_val!=0 ) )
					{
					error = system_error;
					goto Done;
					}
				real_nint = 0;
					{
					int		us_t129;
					us_t129 = nint;
					jth_isect = 1;
					for(;;)
						{
						if( jth_isect > us_t129 ) 	break;
						switch( (*entity).asso_blk[ith_ab-1].asso_type )
							{
							case 8:
								{
								ievent.event = mask_encounter;
								}
								break;
							case 7:
							case 6:
								{
								if( ( ibuf[jth_isect-1].order!=0 ) )
									{
									goto us_l127;
									}
								else if( ( ( ibuf[jth_isect-1].t1<-1.000000e-003 )||( ibuf[
												jth_isect-1].t1>1.001000e+000 ) ) )
										{
										goto us_l127;
										}
								else if( ( (UU_REAL) fabs(ibuf[jth_isect-1].t1)<
													1.000000e-003 ) )
										{
										ievent.event = near_endpoint;
										}
								else if( ( (UU_REAL) fabs(( 1.000000e+000-ibuf[jth_isect-1].
												t1 ))<1.000000e-003 ) )
										{
										ievent.event = near_endpoint;
										}
								else
									{
									ievent.event = region_boundary;
									}
								}
								break;
						default:
								{
								error = illegal_asso_type;
								goto Done;
								}
						}
					ievent.t0 = ibuf[jth_isect-1].t0;
					ievent.t1 = ibuf[jth_isect-1].t1;
					ievent.line_id = line_id;
					um_vctovc(dir,ievent.line_dir);
					ievent.key = key;
					real_nint = ( real_nint+1 );
					uu_list_push(ilist,&(ievent));
us_l127:
					jth_isect++ ;
					}
				}
			if( ( real_nint==0 ) ) goto us_l125;
			if( increasing )
				{
				line_id = ( line_id+1 );
				}
			else
				{
				line_id = ( line_id-1 );
				}
			}
us_l125: ;
		if( increasing )
			{
			increasing = UU_FALSE;
			line_id = ( first_line_id-1 );
			goto Start_walking;
			}
		ith_ab++ ;
		}
		}
	uu_list_sort(ilist,ua_xhatch_compare);
	ua_flushcache();
Done:
	if( ( UU_LIST_LENGTH(ilist)==0 ) )
		{
		error = system_error;
		}
	uu_dexit;
	return(error);
	}
/*********************************************************************
**    E_FUNCTION     : enum UA_xhatch_error	ua_xhatch_generate(entity, ilist, 
**												basepoint, dirvec)
**       adds to line block from information inlist
**    PARAMETERS   
**       INPUT  : 
**          VAR UA_generic_draft:	entity;	line blocks filled in
**				LIST OF UA_ilnode:		ilist;	intersectionlist for a single line
**				COORD:			basepoint; line point t = 0;
**				COORD:			dirvec;	direction of line
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
enum UA_xhatch_error	ua_xhatch_generate(entity, ilist, 
basepoint, dirvec)
struct UA_generic_draft	(*entity);
UU_LIST	(*ilist);
UM_coord basepoint;
UM_coord	dirvec;
	{
	UU_LOGICAL	pen_down, dash_on, no_mask, region_interior;
	int		ix;
	UU_REAL	line_t;
	enum UA_xhatch_error	error;
	UM_coord	p1, p2;
	
	uu_denter(UU_STRC,(us,
		"ua_xhatch_generate(entity=%s, ilist=%d items, basepoint=<%g,%g,%g>,\
		dirvec=<%g,%g,%g>)", "...", UU_LIST_LENGTH(ilist),
		basepoint[0],basepoint[1],basepoint[2], dirvec[0],dirvec[1],dirvec[2]));
	
	pen_down = UU_FALSE;
	region_interior = UU_FALSE;
	dash_on = UU_FALSE;
	no_mask = UU_TRUE;
	line_t = 0.000000e+000;
	ua_print_list(ilist);
		{
		int		us_t132;
		us_t132 = UU_LIST_LENGTH(ilist);
		ix = 1;
		for(;;)
			{
			if( ix > us_t132 ) 	break;
			switch( ((struct UA_ilnode	(*)) ((ilist)->data))[ix-1].  event )
				{
				case near_endpoint:
				case island_boundary:
				case region_boundary:
					region_interior = ( !region_interior );
					break;
				case line_seg_start:
					dash_on = UU_TRUE;
					break;
				case line_seg_end:
					dash_on = UU_FALSE;
					break;
				case mask_encounter:
					no_mask = ( !no_mask );
					break;
				default:
					uu_dexit;
					return(illegal_event_type);
				}
			if( ( ( ( dash_on&&region_interior )&&no_mask )!=pen_down ))
				{
				if( pen_down )
					{
					if( ( (UU_REAL) fabs(( (UU_REAL) fabs(( line_t-((struct 
							UA_ilnode	(*)) ((ilist)->data))[ix-1].t0 ))-1.000000e+000
							 ))>1.000000e-003 ) )
						{
							{
							UU_REAL	us_t133[3];
							um_vctmsc(dirvec,line_t,us_t133);
							um_vcplvc(basepoint,us_t133,p1);
							}
							{
							UU_REAL	us_t134[3];
							um_vctmsc(dirvec,((struct UA_ilnode	(*)) ((ilist)->data))[
										ix-1].t0,us_t134);
							um_vcplvc(basepoint,us_t134,p2);
							}
						uu_list_push(&(UA_hatch_line_list),p1);
						uu_list_push(&(UA_hatch_line_list),p2);
						}
					}
				else
					{
					line_t = ((struct UA_ilnode	(*)) ((ilist)->data))[ix-1].t0 ;
					}
				pen_down = ( !pen_down );
				}
			ix++ ;
			}
		}
	uu_dexit;
	return(hatch_ok);
	}
/*********************************************************************
**    E_FUNCTION     : enum UA_xhatch_error	ua_xhatch_dash(dscale, ilist, 
**											dashes , basepoint)
**       adds dash pattern to intersection list for a single line
**    PARAMETERS   
**       INPUT  : 
**				REAL:	dscale		for cross-hatching, from entity.xh_spacing
**				VAR LIST OF UA_ilnode:		intersections must be filled out 
**										so that unecessary dash info need not be
**										added
**				LIST of REAL:		dashes; (from line family data)
**				REAL:					basepoint;	line parameter for start of pattern
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
enum UA_xhatch_error	ua_xhatch_dash(dscale, ilist, dashes
, basepoint)
UU_REAL	dscale;
UU_LIST	(*ilist);
UU_LIST	(*dashes);
UU_REAL	basepoint;
	{
	int		end, dx, dash_copy, start;
	struct UA_ilnode	devent;
	enum UA_xhatch_error	ret_val;
	UU_LIST	dlist;
	UU_REAL	dvscale, period, dash_t, dash;
	
	uu_denter(UU_STRC,(us,
"ua_xhatch_dash(dscale=%g, ilist=%d items, dashes=%d items, basepoint=%g)", dscale, UU_LIST_LENGTH(ilist), UU_LIST_LENGTH(dashes), basepoint));
	
	dlist.data = UU_NULL;
	period = 0.000000e+000;
	ret_val = hatch_ok;
	dvscale = (UU_REAL) fabs(dscale);
	if( ( UU_LIST_LENGTH(dashes)==0 ) )
		{
		devent.line_id = ((struct UA_ilnode	(*)) ((ilist)->data))[ 0].line_id;
		devent.t0 = ( ((struct UA_ilnode	(*)) ((ilist)->data))[1 -1].t0
							-1.000000e+000 );
		devent.event = line_seg_start;
		uu_list_insert(ilist,(1)-1,&(devent));
		devent.t0 = ( ((struct UA_ilnode	(*)) ((ilist)->data))[
		UU_LIST_LENGTH(ilist)-1].t0+1.000000e+000 );
		devent.event = line_seg_end;
		uu_list_push(ilist,&(devent));
		}
	else
		{
			{
			int		us_t137;
			us_t137 = UU_LIST_LENGTH(dashes);
			dx = 1;
			for(;;)
				{
				if( dx > us_t137 ) 	break;
				period = ( period+(UU_REAL) fabs(((UU_REAL	(*)) ((dashes
											)->data))[dx-1]) );
				dx++ ;
				}
			}
		period = ( period*dvscale );
		start = ua_floor(( ( ((struct UA_ilnode	(*)) ((ilist
									)->data))[0].t0-basepoint )/period ));
		end = ua_floor(( ( ((struct UA_ilnode	(*)) ((ilist
						)->data))[UU_LIST_LENGTH(ilist)-1].t0-basepoint )/period ));
		if( ( ( start-end )>0 ) )
			{
			ret_val = system_error;
			goto Done;
			}
		uu_list_init( &(dlist), sizeof(struct UA_ilnode	) , 0, 10 );
		devent.line_id = ((struct UA_ilnode	(*)) ((ilist)->data))[ 0].line_id;
			{
			int		us_t140;
			us_t140 = end;
			dash_copy = start;
			for(;;)
				{
				if( dash_copy > us_t140 ) 	break;
				dash_t = ( basepoint+( ( (UU_REAL)dash_copy )*period ) );
					{
					int		us_t143;
					us_t143 = UU_LIST_LENGTH(dashes);
					dx = 1;
					for(;;)
						{
						if( dx > us_t143 ) 	break;
						devent.t0 = dash_t;
						if( ( ((UU_REAL	(*)) ((dashes)->data))[dx-1]<0.000000e+000
										 ) )
							{
							devent.event = line_seg_end;
							}
						else
							{
							devent.event = line_seg_start;
							}
						uu_list_push(&(dlist),&(devent));
						dash_t = ( dash_t+( dvscale*(UU_REAL) fabs(((UU_REAL	(*)
										) ((dashes)->data))[dx-1]) ) );
						dx++ ;
						}
					}
				dash_copy++ ;
				}
			}
		devent.t0 = dash_t;
		if( ( ((UU_REAL	(*)) ((dashes)->data))[dx-1]<0.000000e+000) )
			{
			devent.event = line_seg_start;
			}
		else
			{
			devent.event = line_seg_end;
			}
		uu_list_push(&(dlist),&(devent));
		ua_xhatch_mergeilist(ilist,&(dlist),ilist);
		}
Done:
	uu_list_free( &dlist );
	uu_dexit;
	return(ret_val);
	}
/*********************************************************************
**    E_FUNCTION     : enum UA_xhatch_error	ua_xhatch_exec(entity)
**       (re) generates cross-hatching
**    PARAMETERS   
**       INPUT  : 
**          UA_generic_draft:	entity; filled in with boundary/islands,
**					as well as hatch type, angle, and spacing
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
enum UA_xhatch_error	ua_xhatch_exec(entity)
struct UA_generic_draft	(*entity);
	{
	UU_LIST	line_list;
	UU_LIST	lf_list;
	int		ith_event, patx, key, line_first_event, ith_pat, ix,
				current_line_id;
	UU_REAL	dash_begin, lf_angle, lf_offset, lf_spacing;
	UM_coord	line_origin, lf_xvers, lf_dir;
	enum UA_xhatch_error	error;
	
	uu_denter(UU_STRC,(us,"ua_xhatch_exec(entity=%s)", "..."));
	
	line_list.data = UU_NULL;
	lf_list.data = UU_NULL;
	uu_list_init( &(line_list), sizeof(struct UA_ilnode	) , 0, 10 );
	patx = 0;
		{
		int		us_t146;
		us_t146 = UU_LIST_LENGTH(&(xh_table));
		ith_pat = 1;
		for(;;)
			{
			if( ith_pat > us_t146 ) 	break;
			if( ( ((struct UA_xhpattern	(*)) ((&(xh_table))->data))[
							ith_pat-1].xh_id==(*entity).xh_pattern ) )
				{
				patx = ith_pat;
				goto us_l145;
				}
			ith_pat++ ;
			}
us_l145: ;
		}
	if( ( patx==0 ) )
		{
		error = invalid_pattern;
		goto Done;
		}
	error = ua_xhatch_getcpln(&((*entity)));
	if( ( error!=hatch_ok ) )
		{
		goto Done;
		}
			{
			int		us_t149;
			us_t149 = UU_LIST_LENGTH(&(((struct UA_xhpattern	(*)) ((&(
							xh_table))->data))[patx-1].xh_lf));
			ix = 1;
			for(;;)
				{
				if( ix > us_t149 ) 	break;
				lf_spacing = ( ((struct UA_lfdata	(*)) ((&(((struct 
						UA_xhpattern	(*)) ((&(xh_table))->data))[patx-1].xh_lf)
						)->data))[ix-1].lf_space*(*entity).xh_spacing );
				lf_angle = ( ((struct UA_lfdata	(*)) ((&(((struct 
						UA_xhpattern	(*)) ((&(xh_table))->data))[patx-1].xh_lf)
						)->data))[ix-1].lf_angle+(*entity).xh_angle );
				lf_offset = ((struct UA_lfdata	(*)) ((&(((struct 
						UA_xhpattern	(*)) ((&(xh_table))->data))[patx-1].xh_lf)
						)->data))[ix-1].lf_offset;
					{
					UU_REAL	us_t150[3];
					UU_REAL	us_t151[4][3];
					um_rottf((*entity).cpln.zaxis,lf_angle,us_t151);
					um_cctmtf((*entity).cpln.xaxis,us_t151,us_t150);
					um_unitvc(us_t150,lf_dir);
					}
					{
					UU_REAL	us_t152[3];
					um_cross((*entity).cpln.zaxis,lf_dir,us_t152);
					um_unitvc(us_t152,lf_xvers);
					}
				uu_list_init( &(lf_list), sizeof(struct UA_ilnode	) , 0, 10 );
				error = ua_xhatch_isect(&((*entity)),&(lf_list),lf_xvers,
						lf_spacing,lf_dir,lf_offset);
				if( ( error!=hatch_ok ) )
					{
					goto Done;
					}
				line_first_event = 1;
				current_line_id = ((struct UA_ilnode	(*)) ((&(lf_list)
						)->data))[0].line_id;
					{
					UU_REAL	us_t153[3];
					um_vctmsc(lf_xvers,( ( ( (UU_REAL)current_line_id )+
						lf_offset )*lf_spacing ),us_t153);
					um_vcplvc((*entity).cpln.cpln_origin,us_t153,line_origin);
					}
				ith_event = 1;
				for(;;)
					{
					if( ( ( ((struct UA_ilnode	(*)) ((&(lf_list))->data))[
							ith_event-1].line_id==current_line_id )&&( ith_event<=
							UU_LIST_LENGTH(&(lf_list)) ) ) )
						{
						ith_event = ( ith_event+1 );
						continue;
						}
					uu_list_init( &(line_list), sizeof(struct UA_ilnode	), 0, 10 );
					uu_list_push_multiple(&(line_list),(( ith_event-1 ) - 
								line_first_event + 1),&UU_LIST_ARRAY(&(lf_list))[(
								line_first_event-1)*((&(lf_list))->item_size)]);
					ua_print_list(&(line_list));
					if( ( UU_LIST_LENGTH(&(line_list))>1 ) )
						{
						ua_xhatch_fixlist(&(line_list));
						dash_begin = ( ( ( ( (UU_REAL)current_line_id )*((struct 
								UA_lfdata	(*)) ((&(((struct UA_xhpattern	(*)) ((&(
						xh_table))->data))[patx-1].xh_lf))->data))[ix-1].lf_shift )+
						lf_offset )*(*entity).xh_spacing );
						error = ua_xhatch_dash((*entity).xh_spacing,&(line_list),&(
							((struct UA_lfdata	(*)) ((&(((struct UA_xhpattern	(*)
							) ((&(xh_table))->data))[patx-1].xh_lf))->data))[ix-1].
							lf_dashes),dash_begin);
						if( ( error!=hatch_ok ) )
							{
							goto Done;
							}
						error = ua_xhatch_generate(&((*entity)),&(line_list),
									line_origin,lf_dir);
						if( ( error!=hatch_ok ) )
							{
							goto Done;
							}
						}
					current_line_id = ((struct UA_ilnode	(*)) ((&(lf_list)
							)->data))[ith_event-1].line_id;
					line_first_event = ith_event;
						{
						UU_REAL	us_t155[3];
						um_vctmsc(lf_xvers,( ( ( (UU_REAL)current_line_id )+
									lf_offset )*lf_spacing ),us_t155);
						um_vcplvc((*entity).cpln.cpln_origin,us_t155,line_origin);
						}
					ith_event = ( ith_event+1 );
					if( ( ith_event>UU_LIST_LENGTH(&(lf_list)) ) ) goto us_l154;
					}
us_l154: ;
				ix++ ;
				}
			}
		if( ( UU_LIST_LENGTH(&(UA_hatch_line_list))==0 ) )
			{
			error = nothing_generated;
			goto Done;
			}
Done:
		uu_list_free( &line_list );
		uu_list_free( &lf_list );
		uu_dexit;
	return(error);
	}
/*********************************************************************
**    E_FUNCTION     : ua_xhatch()
**       User level routine for cross_hatching
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_xhatch()
	{
	int		nint, type, island_count, key, ok, errint, pattern_id, i,
				status, dummy;
	struct UA_PLOCREC	plocrec;
	enum UA_xhatch_error	ret_val;
	struct UA_generic_draft	xh;
	UU_REAL	drwscale, hatch_angle, xh_scale;
	UU_LOGICAL ua_next_entity();
	
	uu_denter(UU_STRC,(us,"ua_xhatch()"));

	um_get_drwscale(&(drwscale));
	if( ( UA_first_call==UU_TRUE ) )
		{
		ua_xh_set_def();
		UA_first_call = UU_FALSE;
		}
	status = ua_xh_form("axhattr.frm");
	if (status==-1)
		return -1;
	ua_init_entity(55,UA_xh_defaults.material[0],&(xh));
	uu_list_init( &(UA_hatch_line_list), sizeof(UU_REAL	[3]) , 0, 10 );
	xh.xh_pattern = UA_xh_defaults.material[0];
	xh.xh_angle = UA_xh_defaults.xh_angle;
	if((fabs(xh.xh_angle) - UA_HALFPI) < UA_FUZZ)
									xh.xh_angle = xh.xh_angle - 0.001754;
	xh.xh_spacing = (UU_REAL) fabs(( UA_xh_defaults.xh_scale/ drwscale ));
	ud_lgeo(UU_TRUE,UD_xhatchcurves);
	ua_select(111,&(nint));
	if( ( nint==0 ) )
		{
		goto Done;
		}
	for(;;)
		{
		if( ! (ua_next_entity(&(key))) ) goto us_l156;
		type = ua_xhtype(key);
		if( ( type==-1 ) )
			{
			continue;
			}
		if( ( xh.asso_blk_use>50 ) )
			{
			errint = ua_xherrortoint(too_much_geometry);
			uu_uerror1(13,11,50);
			goto Done;
			}
		xh.asso_blk_use = ( xh.asso_blk_use+1 );
		xh.asso_blk[xh.asso_blk_use-1].key = key;
		xh.asso_blk[xh.asso_blk_use-1].asso_type = type;
		xh.asso_blk[xh.asso_blk_use-1].modifier = -1;
		}
us_l156: ;
	if( ( xh.asso_blk_use<1 ) )
		{
		errint = ua_xherrortoint(no_legal_geometry);
		uu_uerror0(13,15);
		goto Done;
		}
	ret_val = ua_xhatch_exec(&(xh));
	if( ( ret_val!=hatch_ok ) )
		{
		errint = ua_xherrortoint(ret_val);
		goto Done;
		}
	ua_create_hatch_rec(((UU_REAL	(*)[3]) ((&(
	UA_hatch_line_list))->data))[0],UU_LIST_LENGTH(&(
									UA_hatch_line_list)),&(key));
	uu_list_free(&(UA_hatch_line_list));
	xh.asso_blk_use = ( xh.asso_blk_use+1 );
	xh.asso_blk[xh.asso_blk_use-1].key = key;
	ua_create_entity(&(xh),&(key));
	uc_display(&(xh));
Done:
	uu_dexit;
	return;
	}
/*********************************************************************
**    E_FUNCTION     : ua_xhatch_regen(edrf)
**       Re-generate a cross-hatch entity
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_xhatch_regen(edrf)
	struct UA_generic_draft *edrf;
	{
	int		nint, type, island_count, key, ok, errint, pattern_id, i,
				status, dummy;
	enum UA_xhatch_error	ret_val;
	UU_REAL	drwscale, hatch_angle, xh_scale;
	UU_KEY_ID	hatch_key;
	struct UM_attrdata_rec  attr;
	
	uu_denter(UU_STRC,(us,"ua_xhatch_regen()"));

	/* initilize line list */
	uu_list_init( &(UA_hatch_line_list), sizeof(UU_REAL	[3]) , 0, 10 );
	UA_xh_regeneration = UU_TRUE;

	/* retrieve poly-line entity associated with cross-hatching */
	hatch_key = edrf->asso_blk[edrf->asso_blk_use -1].key;
	edrf->asso_blk_use = edrf->asso_blk_use - 1;
	uc_retrieve_attr(hatch_key, &attr);

	/* stuff entity attributes into default globals */
	ua_save_xh_def(save_xh_int, save_xh_real);
	ua_stuff_xh_def(edrf, &attr);

	/* regenerate crosshatch intersection pattern */
	ret_val = ua_xhatch_exec(edrf);

	if( ( ret_val!=hatch_ok ) )
		{
		errint = ua_xherrortoint(ret_val);
		status = 1;
		goto Done;
		}

	/* re-create poly-line entity to hold pattern strokes */
	ua_create_hatch_rec(((UU_REAL	(*)[3]) ((&( UA_hatch_line_list))->data))[0],
								UU_LIST_LENGTH(&( UA_hatch_line_list)),&(key));
	uu_list_free(&(UA_hatch_line_list));

	edrf->asso_blk_use =  edrf->asso_blk_use+1 ;
	edrf->asso_blk[edrf->asso_blk_use-1].key = key;
	ur_delete_all(hatch_key);
	status = 0;
Done:

	/* restore defaults and return */
	ua_restore_xh_def(save_xh_int, save_xh_real);
	uu_list_free(&(UA_hatch_line_list));
	UA_xh_regeneration = UU_FALSE;
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_mod_xh_attr(edrf)
**       Modify crosshatching attributes and regen entity
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_mod_xh_attr(edrf)
	struct UA_generic_draft *edrf;
	{
	int		nint, type, island_count, key, ok, errint, pattern_id, i,
				status, dummy;
	enum UA_xhatch_error	ret_val;
	UU_REAL	drwscale, hatch_angle, xh_scale;
	UU_KEY_ID	hatch_key;
	UU_LOGICAL changed, ua_check_xh_def();
	UU_LOGICAL ua_is_xh_regen_required();
	struct UM_attrdata_rec  attr;
	UU_KEY_ID ur_last_mod_key;
	
	uu_denter(UU_STRC,(us,"ua_mod_xh_attr()"));

	/* get poly-line entity assoc. with drafting */
	hatch_key = edrf->asso_blk[edrf->asso_blk_use -1].key;
	uc_retrieve_attr(hatch_key, &attr);

	/* get new attributes from the user */
	um_get_drwscale(&drwscale);
	ua_save_xh_def(save_xh_int, save_xh_real);
	ua_stuff_xh_def(edrf, &attr);
	status = ua_xh_form("axhattr.frm");
	if (status==-1)
		return -1;

	/* check if any attributes were changed by the user */
	if(!ua_check_xh_def(edrf, &attr)) goto Done;

	/* yes!! check if change requires regeneration? */
	if(!ua_is_xh_regen_required(edrf, &attr))
		{
		attr.color = UA_xh_defaults.color[0];  /* NO! just update attributes */
		attr.pen = UA_xh_defaults.pen;
		switch(UA_xh_defaults.linestyle[0])
			{
			case 0:
				attr.line_style  = 1;
				break;
			case 1:
				attr.line_style  = 6;
				break;
			case 2:
				attr.line_style  = 7;
				break;
			case 3:
				attr.line_style  = 5;
				break;
			}
		ur_update_attr(&attr);
	
		/* now re-display entity */
		uc_display(edrf);
		}
	else 				/* YES */
		{
		edrf->asso_blk_use = edrf->asso_blk_use - 1;
		edrf->xh_pattern = UA_xh_defaults.material[0];
		edrf->xh_angle = UA_xh_defaults.xh_angle;
		edrf->xh_spacing = (UU_REAL) fabs((UA_xh_defaults.xh_scale/drwscale));
		
		/* initilize line list */
		uu_list_init( &(UA_hatch_line_list), sizeof(UU_REAL	[3]) , 0, 10 );
	
		/* re-generate intersection pattern */
		UA_xh_regeneration = UU_TRUE;
		ret_val = ua_xhatch_exec(edrf);
	
		if( ( ret_val!=hatch_ok ) )
			{
			errint = ua_xherrortoint(ret_val);
			goto Done;
			}
	
		/* re-create poly-line entity for strokes */
		ur_last_mod_key = UR_last_mod_mkey;	/* store the last modified key */
		ua_create_hatch_rec(((UU_REAL	(*)[3])((&( UA_hatch_line_list))->data))[0],
									UU_LIST_LENGTH(&( UA_hatch_line_list)),&(key));
		uu_list_free(&(UA_hatch_line_list));
	
		edrf->asso_blk_use =  edrf->asso_blk_use+1 ;
		edrf->asso_blk[edrf->asso_blk_use-1].key = key;
	
		/* regeneration calculation complete - update UNIBASE */
		status = ua_update_entity(edrf->key,edrf);
		if(  status != 0  )
			{
			ua_create_entity(edrf,edrf->key);
			}
	
		UR_last_mod_mkey = ur_last_mod_key;	/* reset the last modified key */

		/* now re-display regenerated entity */
		uc_display(edrf);
		status = uc_delete(hatch_key);
		}

Done:
	UA_xh_regeneration = UU_FALSE;
	ua_restore_xh_def(save_xh_int, save_xh_real);
	uu_dexit;
	}
