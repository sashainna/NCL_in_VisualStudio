/*********************************************************************
**    NAME         : aleader.c
**       CONTAINS:
**					ua_leader_subf
**    			ua_crea_leader
**    			ua_crea_witness
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aleader.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:35
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aleader.c 3.2 5/18/88 09:08:10 single"};
#else
static char uu_sccsident[]={"@(#) aleader.c 3.2 5/18/88 09:08:10 double"};
#endif


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

/*********************************************************************
**    E_FUNCTION     : ua_crea_leader(e, s_loc, e_loc, dir)
**       Create a leader line block
**    PARAMETERS   
**       INPUT  : 
**				e								entity record
**				s_loc							start point
**				e_loc							end point
**				dir							direction flag
**       OUTPUT :  
**				e								updated entity record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_crea_leader(e, s_loc, e_loc, dir)
struct UA_generic_draft	*e;
UM_coord	s_loc, e_loc;
int		dir;
	{
	int		m, n;
	UM_coord	origin, pt, xaxis, yaxis, zaxis, tmp;

	uu_denter(UU_STRC,(us,"ua_crea_leader(e=%s, s_loc=<%g,%g,%g>,\
		e_loc=<%g,%g,%g>, dir=%d)", "...", s_loc[0],s_loc[1],s_loc[2],
		e_loc[0],e_loc[1],e_loc[2], dir));

	ua_getcpln(e,origin,xaxis,yaxis,zaxis);
	n = ( e->line_blk_use+1 );
	e->line_blk_use = n;
	e->line_blk[n-1].num_pts = 4;
	e->line_blk[n-1].subtype = dim_line;
	switch( dir )
		{
		case 4:
			um_vctmsc(xaxis,e->stub_length,pt);
			break;
		case 3:
			um_vctmsc(xaxis,( -e->stub_length ),pt);
			break;
		}
	um_vctovc(s_loc,e->line_blk[n-1].line_seg[0]);
	um_vcplvc(s_loc,pt,e->line_blk[n-1].line_seg[1]);
	um_vcplvc(s_loc,pt,e->line_blk[n-1].line_seg[2]);
	um_vctovc(e_loc,e->line_blk[n-1].line_seg[3]);
	m = ( e->arrow_blk_use+1 );
	e->arrow_blk_use = m;
	um_vctovc(e_loc,e->arrow_blk[m-1].location);
	um_vcmnvc(e->line_blk[n-1].line_seg[3],e->line_blk[n-1].line_seg[2],tmp);
	e->arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,tmp);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : int		ua_leader_subf(leader_type, entity, loc_option)
**       Support routine for leader line generation
**    PARAMETERS   
**       INPUT  : 
**				leader_type					switch for leader type
**				entity						generic drafting entity
**       OUTPUT : 
**				asso_blk					filled in association blk
**				loc_option				location option:
**												for fp_tol
**													1 = top
**													2 = bottom
**													3 = right
**													4 = left
**												for labels
**													1 = text horiz
**													2 = text aligned
**													3 = text over leader
**													4 = text over stub
**
**    RETURNS      : INTEGER		0	user rejected option
**											1  no leader required(for fp-tol)
**											2  user picked an entity(data in assoc blk)
**											3  user input screen pos(WC in assoc blk)
**											4  user selected AA (change entity origin)
**											5  user selected witness line option
**												WC in association block
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_leader_subf(leader_type, entity, loc_option)
int		leader_type;
struct UA_generic_draft	*entity;
int		*loc_option;
	{
	struct UA_PLOCREC	plocrec;
	int		ablock, d_stat, status, mode, dummy, r_stat;
	UM_coord	pos;

	uu_denter(UU_STRC,(us,
		"ua_leader_subf(leader_type=%d,entity=%s,loc_option=%d)",
		leader_type, "...", *loc_option));

	r_stat = 0;
	switch( leader_type )
		{
		case 1:
			{
			d_stat = ua_popmenu(2,&(mode));
			if( ( d_stat==0 ) )
				{
				r_stat = 0;
				goto fexit;
				}
			if( ( d_stat==2 ) )
				{
				r_stat = 4;
				goto fexit;
				}
			if( ( ( d_stat==1 )&&( mode==0 ) ) )
				{
				r_stat = 0;
				goto fexit;
				}
			switch( mode )
				{
				case 1:
					{
					r_stat = 1;
					goto fexit;
					}
				case 2:
					{
					(*loc_option) = 3;
					d_stat = ua_popmenu(4,&(mode));
					if( ( d_stat==0 ) )
						{
						r_stat = 0;
						goto fexit;
						}
					switch( mode )
						{
						case 1:
							{
							ablock = ( entity->asso_blk_use+1 );
							status = ua_select_ent_subf(100,&((*entity)),ablock,&(
							plocrec));
							if( ( status!=1 ) )
								{
								r_stat = 0;
								goto fexit;
								}
							else
								{
								uc_near_on_entity(entity->asso_blk[ablock-1].key,
								&(plocrec.ndcloc),entity->asso_blk[ablock-1].location);
								entity->asso_blk_use = ablock;
								r_stat = 2;
								goto fexit;
								}
							}
						case 2:
							{
							ablock = ( entity->asso_blk_use+1 );
							d_stat = ud_world_coord(UA_DRAFTING,42,pos,1,&(dummy),UU_FALSE);
							if( ( d_stat==0 ) )
								{
								r_stat = 0;
								goto fexit;
								}
							else
								{
								entity->asso_blk_use = ablock;
								um_vctovc(pos,entity->asso_blk[ablock-1].location);
								entity->asso_blk[ablock-1].key = 0;
								r_stat = 3;
								goto fexit;
								}
							}
						}
					}
					break;
				case 3:
					{
					d_stat = ua_popmenu(14,&((*loc_option)));
					if( ( d_stat==0 ) )
						{
						r_stat = 0;
						goto fexit;
						}
					else
						{
						ablock = ( entity->asso_blk_use+1 );
						d_stat = ud_world_coord(UA_DRAFTING,105,pos,1,&(dummy),UU_FALSE);
						if( ( d_stat==0 ) )
							{
							r_stat = 0;
							goto fexit;
							}
						else
							{
							entity->asso_blk_use = ablock;
							um_vctovc(pos,entity->asso_blk[ablock-1].location);
							entity->asso_blk[ablock-1].key = 0;
							r_stat = 5;
							goto fexit;
							}
						}
					}
				}
			}
			break;
		case 2:
			{
			d_stat = ua_popmenu(5,&((*loc_option)));
			if( ( ( d_stat==1 )&&( (*loc_option)==0 ) ) )
				{
				r_stat = 0;
				goto fexit;
				}
			else
				{
				r_stat = d_stat;
				goto fexit;
				}
			}
		case 3:
			{
			d_stat = ua_popmenu(4,&(mode));
			if( ( d_stat==2 ) )
				{
				r_stat = 2;
				goto fexit;
				}
			if( ( ( mode>2 )||( d_stat==0 ) ) )
				{
				r_stat = 0;
				goto fexit;
				}
			if( ( ( d_stat==1 )&&( mode==0 ) ) )
				{
				r_stat = 0;
				goto fexit;
				}
			switch( mode )
				{
				case 1:
					{
					ablock = ( entity->asso_blk_use+1 );
					status = ua_select_ent_subf(101,entity,ablock,&(plocrec));
					if( ( status!=1 ) )
						{
						r_stat = status;
						goto fexit;
						}
					else
						{
						uc_near_on_entity(entity->asso_blk[ablock-1].key,
						   &(plocrec.ndcloc),entity->asso_blk[ablock-1].location);
						entity->asso_blk_use = ablock;
						r_stat = status;
						goto fexit;
						}
					}
				case 2:
					{
					ablock = ( entity->asso_blk_use+1 );
					d_stat = ud_world_coord(UA_DRAFTING,42,pos,1,&(dummy),UU_FALSE);
					if(  d_stat != 1 || dummy == 0  )
						{
						r_stat = d_stat;
						goto fexit;
						}
					else
						{
						entity->asso_blk_use = ablock;
						um_vctovc(pos,entity->asso_blk[ablock-1].location);
						entity->asso_blk[ablock-1].key = 0;
						r_stat = d_stat;
						goto fexit;
						}
					}
				}
			}
			break;
		}
fexit:;
	uu_dexit;
	return(r_stat);
	}
/*********************************************************************
**    E_FUNCTION     : ua_crea_witness(e, s_loc, e_loc)
**       Create a witness line block
**    PARAMETERS   
**       INPUT  : 
**				e								entity record
**				s_loc							start point
**				e_loc							end point
**       OUTPUT :  
**				e								updated entity record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_crea_witness(e, s_loc, e_loc)
struct UA_generic_draft	(*e);
UM_coord	s_loc[3], e_loc[3];
	{
	int		n;
	UM_coord	save_origin, origin, xaxis, delt, yaxis, zaxis, tmp1, tmp2;
	UU_REAL y_proj, x_proj;

	uu_denter(UU_STRC,(us,
		"ua_crea_witness(e=%s,s_loc=<%g,%g,%g>,e_loc=<%g,%g,%g>)", "...",
		s_loc[0],s_loc[1],s_loc[2], e_loc[0],e_loc[1],e_loc[2]));

	ua_getcpln(e,origin,xaxis,yaxis,zaxis);
	n = ( e->line_blk_use+1 );
	e->line_blk_use = n;
	um_vctovc(e->dim_origin,save_origin);
	um_vcmnvc(e_loc,s_loc,delt);
	x_proj = um_dot(delt,xaxis);
	y_proj = um_dot(delt,yaxis);
	if( ( fabs(x_proj)<fabs(y_proj) ) )
		{
		um_vctmsc(xaxis,x_proj,tmp1);
		um_vcplvc(e->dim_origin,tmp1,e->dim_origin);
		}
	else
		{
		um_vctmsc(yaxis,y_proj,tmp1);
		um_vcplvc(e->dim_origin,tmp1,e->dim_origin);
		}
	e->line_blk[n-1].subtype = ext_line;
	e->line_blk[n-1].num_pts = 2;
	um_vcmnvc(e->dim_origin,save_origin,tmp1);
	um_vcplvc(s_loc,tmp1,e->line_blk[n-1].line_seg[0]);
	um_vctovc(e_loc,e->line_blk[n-1].line_seg[1]);
	uu_dexit;
	}
