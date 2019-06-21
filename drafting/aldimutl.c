
/*********************************************************************
**    NAME         : aldimutl.c
**       CONTAINS:
**    		ua_assoc_blk
**    		ua_near_on_draft
**    		ua_arrow_loc
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aldimutl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:35
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aldimutl.c 4.3 2/27/89 14:46:33 single"};
#else
static char uu_sccsident[]={"@(#) aldimutl.c 4.3 2/27/89 14:46:33 double"};
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

/*********************************************************************
**    E_FUNCTION     : int		ua_assoc_blk(origin, xaxis, yaxis, zaxis, entity, 
**												ablk1, ablk2, plocrec1, plocrec2)
**       Complete associativity blocks.
**    PARAMETERS   
**       INPUT  : 
**				origin					origin of dimension plane
**				xaxis						x axis of dimension plane
**				yaxis						y axis of dimension plane
**				zaxis						z axis of dimension plane
**				entity					entity record
**				ablk1						association block index
**				ablk2						association block index
**				ploc1						pick record
**				ploc2						pick record
**       OUTPUT :  
**				entity					completed association blocks
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_assoc_blk(origin, xaxis, yaxis, zaxis, entity, 
ablk1, ablk2, plocrec1, plocrec2)
UU_REAL	origin[3];
UU_REAL	xaxis[3];
UU_REAL	yaxis[3];
UU_REAL	zaxis[3];
struct UA_generic_draft	(*entity);
int		ablk1;
int		ablk2;
struct UA_PLOCREC	(*plocrec1);
struct UA_PLOCREC	(*plocrec2);
	{
	int		rel_num1;
	int		rel_num2;
	int		nint;
	int		closest;
	int		asso_type1;
	int		asso_type2;
	UU_REAL	near_pt[3];
	UU_REAL	radius[2];
	UU_REAL	normal[2][3];
	UU_REAL	pt[2][3];
	UU_REAL	unit_vec[3];
	UU_REAL	ipt[2][3];
	UU_REAL	c_spt[3];
	UU_REAL	far_pt[3];
	UU_REAL	sp_vec[3];
	UU_REAL	dang;
	UU_REAL	c_ept[3];
	UU_REAL	dummy;

	uu_denter(UU_STRC,(us,"SAL ua_assoc_blk(origin=<%g,%g,%g>, xaxis=<%g,%g,%g>,\
		yaxis=<%g,%g,%g>, zaxis=<%g,%g,%g>, entity=%s, ablk1=%d, ablk2=%d,\
		plocrec1=%s, plocrec2=%s)", origin[0],origin[1],origin[2],
		xaxis[0],xaxis[1],xaxis[2], yaxis[0],yaxis[1],yaxis[2],
		zaxis[0],zaxis[1],zaxis[2], "...", ablk1, ablk2, "...", "..."));

	asso_type1 = (*entity).asso_blk[ablk1-1].asso_type;
	asso_type2 = (*entity).asso_blk[ablk2-1].asso_type;
	switch( (*entity).subtype )
		{
		case UA_PERP_DIM:
			{
			if( ( (*entity).asso_blk[ablk2-1].modifier==0 ) )
				{
				uc_draft_endpts(2,&((*plocrec2).ppick),&((*plocrec2).ndcloc)
				    ,&(rel_num2),pt[1],far_pt);
				}
			else
				{
				if( ( asso_type2==UA_DRAFT_ARC ) )
					{
					uc_draft_arc((*entity).asso_blk[ablk2-1].key,pt[1],&(
					radius[1]),&(dang),normal[1],c_spt,c_ept,&(dummy));
					}
				else
					{
					uc_draft_conic((*entity).asso_blk[ablk2-1].key,pt[1],c_spt
					    ,c_ept);
					}
				}
			uc_draft_endpts(2,&((*plocrec1).ppick),&((*plocrec1).ndcloc)
			    ,&(rel_num1),near_pt,far_pt);
				{
				UU_REAL	us_t58[3];
				um_vcmnvc(far_pt,near_pt,us_t58);
				um_unitvc(us_t58,unit_vec);
				}
			um_nptln(pt[1],near_pt,unit_vec,pt[0]);
			if( ( (*entity).asso_blk[ablk2-1].modifier==1 ) )
				{
					{
					UU_REAL	us_t59[3];
					um_vcmnvc(pt[1],pt[0],us_t59);
					um_unitvc(us_t59,unit_vec);
					}
				if( ( asso_type2==2 ) )
					{
					um_ilncir(pt[1],unit_vec,pt[1],normal[1],radius[1], &(nint),ipt);
					if( ( nint==0 ) )
						{
						uu_dexit;
						return(1);
						}
					}
				else
					{
						{
						UU_REAL	us_t60[3];
						um_cross(zaxis,unit_vec,us_t60);
						um_unitvc(us_t60,sp_vec);
						}
					ua_tan_to_conic(sp_vec,(*entity).asso_blk[ablk2-1].key,
											&( nint),ipt);
					if( ( nint==0 ) )
						{
						uu_dexit;
						return(1);
						}
					}
				closest = um_nearest_to_ploc(&((*plocrec2).ndcloc),nint,ipt);
				um_vctovc(ipt[( closest+1 )-1],pt[1]);
				}
			}
			break;
		case UA_THICK_DIM:
			{
			uc_near_on_entity((*entity).asso_blk[0].key,
										&((*plocrec1).ndcloc),pt[0]);
			uc_draft_endpts(2,&((*plocrec2).ppick),&((*plocrec2).ndcloc)
			    ,&(rel_num2),near_pt,far_pt);
				{
				UU_REAL	us_t61[3];
				um_vcmnvc(far_pt,near_pt,us_t61);
				um_unitvc(us_t61,unit_vec);
				}
			um_nptln(pt[0],near_pt,unit_vec,pt[1]);
			}
			break;
		default:
			{
			if( ( (*entity).asso_blk[ablk1-1].modifier==0 ) )
				{
				uc_draft_endpts(2,&((*plocrec1).ppick),&((*plocrec1).ndcloc)
				    ,&(rel_num1),pt[0],far_pt);
				}
			else if( ( (*entity).asso_blk[ablk1-1].modifier==10 ) )
				{
				ua_near_on_draft((*entity).asso_blk[ablk1-1].key,&((*
				    plocrec1)),pt[0]);
				}
			else
				{
				if( ( asso_type1==2 ) )
					{
					uc_draft_arc((*entity).asso_blk[ablk1-1].key,pt[0],&(
					radius[0]),&(dang),normal[0],c_spt,c_ept,&(dummy));
					}
				else
					{
					uc_draft_conic((*entity).asso_blk[ablk1-1].key,pt[0],c_spt
					    ,c_ept);
					}
				}
			if( ( (*entity).asso_blk[ablk2-1].modifier==0 ) )
				{
				uc_draft_endpts(2,&((*plocrec2).ppick),&((*plocrec2).ndcloc)
				    ,&(rel_num2),pt[1],far_pt);
				}
			else if( ( (*entity).asso_blk[ablk2-1].modifier==10 ) )
				{
				ua_near_on_draft((*entity).asso_blk[ablk2-1].key,&((*
				    plocrec2)),pt[1]);
				}
			else
				{
				if( ( asso_type2==2 ) )
					{
					uc_draft_arc((*entity).asso_blk[ablk2-1].key,pt[1],&(
					radius[1]),&(dang),normal[1],c_spt,c_ept,&(dummy));
					}
				else
					{
					uc_draft_conic((*entity).asso_blk[ablk2-1].key,pt[1],c_spt
					    ,c_ept);
					}
				}
			if( ( ( (*entity).asso_blk[ablk1-1].modifier==1 )||( (*
			    entity).asso_blk[ablk2-1].modifier==1 ) ) )
				{
				if( ( (*entity).subtype==1 ) )
					{
					um_vctovc(xaxis,unit_vec);
					}
				else if( ( (*entity).subtype==2 ) )
					{
					um_vctovc(yaxis,unit_vec);
					}
				else
					{
						{
						UU_REAL	us_t62[3];
						um_vcmnvc(pt[1],pt[0],us_t62);
						um_unitvc(us_t62,unit_vec);
						}
					}
				if( ( (*entity).asso_blk[ablk1-1].modifier==1 ) )
					{
					if( ( asso_type1==2 ) )
						{
						um_ilncir(pt[0],unit_vec,pt[0],normal[0],radius[0],
						&(nint),ipt);
						if( ( nint==0 ) )
							{
							uu_dexit;
							return(1);
							}
						}
					else
						{
							{
							UU_REAL	us_t63[3];
							um_cross(zaxis,unit_vec,us_t63);
							um_unitvc(us_t63,sp_vec);
							}
						ua_tan_to_conic(sp_vec,(*entity).asso_blk[ablk1-1].key,&(
						nint),ipt);
						if( ( nint==0 ) )
							{
							uu_dexit;
							return(1);
							}
						}
					closest = um_nearest_to_ploc(&((*plocrec1).ndcloc),nint,ipt)
						;
					um_vctovc(ipt[( closest+1 )-1],pt[0]);
					}
				if( ( (*entity).asso_blk[ablk2-1].modifier==1 ) )
					{
					if( ( asso_type2==2 ) )
						{
						um_ilncir(pt[1],unit_vec,pt[1],normal[1],radius[1],
						&(nint),ipt);
						if( ( nint==0 ) )
							{
							uu_dexit;
							return(1);
							}
						}
					else
						{
							{
							UU_REAL	us_t64[3];
							um_cross(zaxis,unit_vec,us_t64);
							um_unitvc(us_t64,sp_vec);
							}
						ua_tan_to_conic(sp_vec,(*entity).asso_blk[ablk2-1].key,&(
						nint),ipt);
						if( ( nint==0 ) )
							{
							uu_dexit;
							return(1);
							}
						}
					closest = um_nearest_to_ploc(&((*plocrec2).ndcloc),nint,ipt)
						;
					um_vctovc(ipt[( closest+1 )-1],pt[1]);
					}
				}
			}
		}
	um_nptpln(pt[0],origin,zaxis,ipt[0]);
	um_nptpln(pt[1],origin,zaxis,ipt[1]);
	um_vctovc(ipt[0],(*entity).asso_blk[ablk1-1].location);
	um_vctovc(ipt[1],(*entity).asso_blk[ablk2-1].location);
	uu_dexit;
	return(0);
	}
/*********************************************************************
**    E_FUNCTION     : ua_near_on_draft(key, plocrec, pts)
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
ua_near_on_draft(key, plocrec, pts)
int		key;
struct UA_PLOCREC	(*plocrec);
UU_REAL	pts[3];
	{
	struct UA_generic_draft	e;
	int		i;
	int		j;
	int		k;
	UU_REAL	end_pts[50][3];
	UU_REAL	tmp_pts[10][3];

	uu_denter(UU_STRC,(us,"SAL ua_near_on_draft(key=%d, plocrec=%s, pts=%s)",
		key, "...", "..."));

	e.key = key;
	j = uc_retrieve_data(&(e),sizeof(struct UA_generic_draft	));
		{
		int		us_t67;
		us_t67 = e.line_blk_use;
		i = 1;
		for(;;)
			{
			if( i > us_t67 ) 	break;
				{
				int		us_t70;
				us_t70 = e.line_blk[i-1].num_pts;
				k = 1;
				for(;;)
					{
					if( k > us_t70 ) 	break;
					um_vctovc(e.line_blk[i-1].line_seg[k-1],end_pts[k-1]);
us_l68:
					k++ ;
					}
us_l69: ;
				}
			j = um_nearest_to_ploc(&((*plocrec).ndcloc),e.line_blk[i-1].
			    num_pts,end_pts);
			j = ( j+1 );
			um_vctovc(end_pts[j-1],tmp_pts[i-1]);
us_l65:
			i++ ;
			}
us_l66: ;
		}
	if( ( e.line_blk_use==1 ) )
		{
		um_vctovc(tmp_pts[0],pts);
		}
	else
		{
			{
			int		us_t73;
			us_t73 = e.line_blk_use;
			i = 1;
			for(;;)
				{
				if( i > us_t73 ) 	break;
				um_vctovc(tmp_pts[i-1],end_pts[i-1]);
us_l71:
				i++ ;
				}
us_l72: ;
			}
		j = um_nearest_to_ploc(&((*plocrec).ndcloc),e.line_blk_use,
		end_pts);
		um_vctovc(tmp_pts[( j+1 )-1],pts);
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_arrow_loc(key, plocrec, cp_org, xaxis, yaxis, 
**									zaxis, base_pt, base_vec)
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
ua_arrow_loc(key, plocrec, cp_org, xaxis, yaxis, 
	zaxis, base_pt, base_vec)
int		key;
struct UA_PLOCREC	(*plocrec);
UU_REAL	cp_org[3];
UU_REAL	xaxis[3];
UU_REAL	yaxis[3];
UU_REAL	zaxis[3];
UU_REAL	base_pt[3];
UU_REAL	base_vec[3];
	{
	struct UA_generic_draft	e;
	int		i;
	int		j;
	UU_REAL	sine;
	UU_REAL	cosine;
	UU_REAL	pts[2][3];

	uu_denter(UU_STRC,(us,"SAL ua_arrow_loc(key=%d,plocrec=%s,cp_org=<%g,%g,%g>,\
		xaxis=<%g,%g,%g>, yaxis=<%g,%g,%g>, zaxis=<%g,%g,%g>, base_pt=<%g,%g,%g>)",
		key, "...", cp_org[0],cp_org[1],cp_org[2],
		xaxis[0],xaxis[1],xaxis[2]	, yaxis[0],yaxis[1],yaxis[2],
		zaxis[0],zaxis[1],zaxis[2],base_pt[0],base_pt[1],base_pt[2]));

	e.key = key;
	j = uc_retrieve_data(&(e),sizeof(struct UA_generic_draft	));
	if( ( e.arrow_blk_use>1 ) )
		{
			{
			int		us_t76;
			us_t76 = e.arrow_blk_use;
			i = 1;
			for(;;)
				{
				if( i > us_t76 ) 	break;
				um_vctovc(e.arrow_blk[i-1].location,pts[i-1]);
us_l74:
				i++ ;
				}
us_l75: ;
			}
		j = um_nearest_to_ploc(&((*plocrec).ndcloc),e.arrow_blk_use, pts);
		j = ( j+1 );
		}
	else
		{
		j = 1;
		}
	um_vctovc(e.arrow_blk[j-1].location,base_pt);
	sine = sin(e.arrow_blk[j-1].aangle);
	cosine = cos(e.arrow_blk[j-1].aangle);
		{
		UU_REAL	us_t77[3];
		UU_REAL	us_t78[3];
		UU_REAL	us_t79[3];
		um_vctmsc(xaxis,cosine,us_t78);
		um_vctmsc(yaxis,sine,us_t79);
		um_vcplvc(us_t78,us_t79,us_t77);
		um_unitvc(us_t77,base_vec);
		}
	uu_dprint(UU_STRC,(us,"******** base_pt=<%g,%g,%g> base_vec=<%g,%g,%g>",	base_pt[0],base_pt[1],base_pt[2],base_vec[0],base_vec[1],base_vec[2]));
	uu_dprint(UU_STRC,(us,"******** angle=%g, j=%d",e.arrow_blk[j-1].aangle,j))
	uu_dexit;
	}
