/*********************************************************************
**    NAME         : abaseln.c
**       CONTAINS:
**      		 ua_cre_baseln
**     	    ua_baseln
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       abaseln.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:31
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]=	{
	"@(#) abaseln.c 4.2 8/11/89 02:35:22 single"	};
#else
static char uu_sccsident[]=	{
	"@(#) abaseln.c 4.2 8/11/89 02:35:22 double"	};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adrfdefs.h"
#include "adraft.h"
#include "dasnog.h"
#include "adrfcom.h"
#include "mdcoord.h"

#define UA_HORZ_BASELINE 6
#define UA_VERT_BASELINE 7
#define UA_PARA_BASELINE 8

extern int UD_draftable[UD_NMENTWD];
extern int UD_draft_line[UD_NMENTWD];

/*********************************************************************
**    E_FUNCTION     : void		ua_baseln(subtype)
**       User interaction routine for the creation of linear
**			baseln dimensions.
**    PARAMETERS   
**       INPUT  : 
**          dim_type					Dimension types code.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_baseln(subtype)
int		subtype;
	{
	struct UA_PLOCREC	plocrec1;
	struct UA_PLOCREC	plocrec2;
	struct UA_generic_draft	ldim;
	struct UA_PLOCREC	baseploc;
	int		base_key, previous_txt_blk_use, entity1, entity2, key, closest,
				origin_mode, drf_key, bitary[9], previous_key, base_modifier, j,
				locations, msgno, grid_save, count, dummy, base_asso_type, status;
	UU_REAL	dis1, dis2, ext_offset_dist;
	UM_coord	dim_origin, base_location, base_pt, base_org, base_cent_pt,
				base_ext_vec, cpln_origin, dvec, xaxis, base_vec, yaxis, zaxis,
				tmp1, tmp2, tmp3;
	UU_LOGICAL	base_flg, first_time, redo, ok;

	uu_denter(UU_STRC,(us,"SAL ua_baseln(subtype=%d)", subtype));

	first_time = UU_TRUE;
	count = 0;
	origin_mode = 1;
	ua_init_entity(UA_BASELN_DIM,subtype,&ldim);
	ext_offset_dist = ldim.gap_to_geo;
	ua_getcpln(&ldim,cpln_origin,xaxis,yaxis,zaxis);
start:
	ud_lgeo(UU_TRUE,UD_draftable);
	status = ua_select_ent_subf(128,&ldim,1,&plocrec1);
	if( ( status==UA_REJECT ) )
		{
		uu_dexit;
		return;
		}
	base_key = ldim.asso_blk[0].key;
	base_modifier = ldim.asso_blk[0].modifier;
	base_asso_type = ldim.asso_blk[0].asso_type;
	uu_move_byte( &plocrec1, &baseploc, sizeof( struct UA_PLOCREC	 ) );
	for(j=0;j<3;j++)
		{
		base_pt[j] = 0.0;
		base_vec[j] = 0.0;
		base_ext_vec[j] = 0.0;
		}
	base_vec[0] = 1.0;
	base_ext_vec[1] = 1.0;
	base_flg = UU_TRUE;
loop:
	if( ( count>0 ) )
		{
		ua_init_entity(UA_BASELN_DIM,subtype,&ldim);
		ldim.asso_blk_use = 1;
		ldim.asso_blk[0].key = base_key;
		ldim.asso_blk[0].modifier = base_modifier;
		ldim.asso_blk[0].asso_type = base_asso_type;
		uu_move_byte( &baseploc, &plocrec1, sizeof( struct UA_PLOCREC	 ) );
		origin_mode = 2;
		}
	ud_lgeo(UU_TRUE,UD_draftable);
	status = ua_select_ent_subf(29,&ldim,2,&plocrec2);
	switch( status )
		{
		case UA_REJECT:
			{
			uu_dexit;
			return;
			}
		case UA_ALT_ACTION:
			{
			if( ( count==1 ) )
				{
				redo = UU_TRUE;
				ldim.key = previous_key;
				status = uc_retrieve_data(&ldim,sizeof(struct UA_generic_draft));
				if( ( status==0 ) )
					{
					ldim.arc_blk_use = 0;
					ldim.line_blk_use = 0;
					ldim.arrow_blk_use = 0;
					ldim.txt_blk_use = previous_txt_blk_use;
					for(j=1;j<=UA_NUM_TXTBLKS;j++)
						{
						ldim.txt_blk[j-1].tangle = UA_text_ang;
						}
					origin_mode = 1;
					count = 0;
					base_flg = UU_TRUE;
					ext_offset_dist = ldim.gap_to_geo;
					goto origin;
					}
				else
					{
					redo = UU_FALSE;
					previous_key = 0;
					count = 0;
					origin_mode = 1;
					ua_init_entity(UA_LINEAR_DIM,subtype,&ldim);
					ua_getcpln(&ldim,cpln_origin,xaxis,yaxis,zaxis);
					ext_offset_dist = ldim.gap_to_geo;
					goto start;
					}
				}
			else
				{
				uu_dexit;
				return;
				}
			}
		case UA_OPCOMPLETE:
			{
			redo = UU_FALSE;
			}
			break;
		}
	ldim.asso_blk_use = 2;
	status = ua_assoc_blk(cpln_origin,xaxis,yaxis,zaxis,&ldim,1,2,&plocrec1,
								&plocrec2);
	if( ( status==1 ) )
		{
		uu_uerror0(UA_DRAFTING,17);
		goto start;
		}
	if( um_cceqcc(ldim.asso_blk[0].location,ldim.asso_blk[1].location) )
		{
		uu_uerror0(UA_DRAFTING,7);
		goto loop;
		}
text:
	ok = ua_text_subf(&(ldim));
	if( ( !ok ) )
		{
		goto text;
		}
	previous_txt_blk_use = ldim.txt_blk_use;
origin:
	if( redo )
		{
		switch( ldim.txt_place )
			{
			case UA_AUTOMATIC:
				msgno = 119;
				break;
			default:
				msgno = 118;
			}
		}
	else
		{
		switch( ldim.txt_place )
			{
			case UA_AUTOMATIC:
				msgno = 117;
				break;
			default:
				msgno = 30;
			}
		}
	if( ( ( ldim.txt_place==0 )&&( count>0 ) ) )
		{
		um_vctovc(base_org,ldim.dim_origin);
		}
	else
		{
org_rep1:
		status = ua_ent_origin_subf(msgno,&ldim);
		switch( status )
			{
			case UA_REJECT:
				goto start;
			case UA_ALT_ACTION:
				switch( ldim.txt_place )
					{
					case UA_AUTOMATIC:
						ldim.txt_place = UA_MANUAL;
						break;
					case UA_MANUAL:
						ldim.txt_place = UA_AUTOMATIC;
						break;
					}
				goto org_rep1;
			}
		}
	um_nptpln(ldim.dim_origin,cpln_origin,zaxis,dim_origin);
	um_vctovc(dim_origin,ldim.dim_origin);
	dummy = ( ldim.asso_blk_use+1 );
	ldim.asso_blk[dummy-1].key = 0;
	if( ( base_flg==UU_TRUE ) )
		{
		um_vcmnvc(ldim.asso_blk[1].location,ldim.asso_blk[0].
			    location,tmp1);
		um_unitvc(tmp1,ldim.asso_blk[dummy-1].location);
		}
	else
		{
		um_vctovc(base_vec,ldim.asso_blk[dummy-1].location);
		}
	dummy = ( dummy+1 );
	ldim.asso_blk[dummy-1].key = 0;
	if( ( base_flg==UU_TRUE ) )
		um_vctovc(ldim.asso_blk[0].location,ldim.asso_blk[dummy-1].location);
	else
		um_vctovc(base_pt,ldim.asso_blk[dummy-1].location);
	dummy = ( dummy+1 );
	ldim.asso_blk[dummy-1].key = 0;
	um_vctovc(base_ext_vec,ldim.asso_blk[dummy-1].location);
	ldim.asso_blk_use = dummy;
	status = ua_cre_baseln(cpln_origin,xaxis,yaxis,zaxis,&( base_flg)
								,base_pt,base_vec,count,&(ext_offset_dist),
								base_ext_vec,&(ldim));
	if( ( status!=1 ) )
		{
		if( ( status==2 ) )
			{
			uu_uerror0(UA_DRAFTING,26);
			}
		if( ( count==0 ) )
			{
			goto start;
			}
		else
			{
			goto loop;
			}
		}
	if( redo )
		{
		ldim.stack_grid = grid_save;
		count = 1;
		status = ua_update_entity(previous_key,&ldim);
		if( ( status!=0 ) )
			{
			ua_create_entity(&(ldim),&(key));
			}
		}
	else
		{
		ua_create_entity(&ldim,&key);
		count = ( count+1 );
		if( ( count==1 ) )
			{
			um_vctovc(ldim.dim_origin,base_org);
			}
		}
	uc_display(&(ldim));
	previous_key = ldim.key;
	goto loop;
	}
/*********************************************************************
**    E_FUNCTION     : int		ua_cre_baseln(cpln_origin, xaxis, yaxis, zaxis, 
**								base_flg, base_pt, base_vec, count, ext_offset_dist, 
**								ext_vect, ldim)
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
int		ua_cre_baseln(cpln_origin, xaxis, yaxis, zaxis, 
						base_flg, base_pt, base_vec, count, ext_offset_dist, 
						ext_vect, ldim)
struct UA_generic_draft	(*ldim);
int		count;
UU_REAL	(*ext_offset_dist);
UM_coord	ext_vect, cpln_origin, xaxis, yaxis, zaxis, base_pt, base_vec;
UU_LOGICAL	(*base_flg);
	{
	int		nint, use, num, rel_num, i, grid_save;
	UU_REAL	dir_test, off_set, temp_extent, dim_minext, dim_maxext, diff,
				box_minext, box_maxext, box_extent;
	UM_coord	extvect, spt, new_loc, int_point1, int_point2, ept_cp, dir_vec1,
				spt_cp, dir_vec2, dvec, int_pt, dimvect, move_vect, ept, box[4],
				dl_pt, tmp1, tmp2, tmp3;
	UU_LOGICAL	inside_cone;
	UU_LOGICAL	ua_text_fit();

	uu_denter(UU_STRC,(us,"SAL ua_cre_baseln(cpln_origin=<%g,%g,%g>,base_flg=%s,count=%d)",
		cpln_origin[0],cpln_origin[1],cpln_origin[2],
		*base_flg?"TRUE" : "FALSE",count));

	switch( (*ldim).subtype )
		{
		case UA_HORIZ_DIM:
			{
			um_vctovc(yaxis,extvect);
			um_vctovc(xaxis,dimvect);
			ldim->dim_value = ua_dim_value(horiz_dist,two_dim,dimvect,
								ldim->asso_blk[0].location,ldim->asso_blk[1].location);
			}
			break;
		case UA_VERT_DIM:
			{
			um_vctovc(xaxis,extvect);
			um_vctovc(yaxis,dimvect);
			ldim->dim_value = ua_dim_value(vert_dist,two_dim,dimvect,
								ldim->asso_blk[0].location,ldim->asso_blk[1].location);
			}
			break;
		case UA_PARAL_DIM:
			{
			if( ( (*base_flg)==UU_TRUE ) )
				{
				um_vcmnvc(ldim->asso_blk[1].location,ldim->asso_blk[0 ].location,
							tmp1);
				um_unitvc(tmp1,dimvect);
				}
			else
				{
				um_vctovc(base_vec,dimvect);
				}
			um_cross(dimvect,zaxis,tmp1);
			um_unitvc(tmp1,extvect);
			ldim->dim_value = ua_dim_value(shortest,two_dim,dimvect,
					ldim->asso_blk[0].location,ldim->asso_blk[1].location);
			}
			break;
		}
	if( ( ldim->dim_value<1.000000e-004 ) )
		{
		uu_uerror0(UA_DRAFTING,16);
		uu_dexit;
		return(0);
		}
	if( ( ldim->txt_entry==UA_SYS_TEXT ) )
		{
		ua_set_dim_text(ldim);
		}
	if( ( ldim->txt_orent!=UA_TXT_HORIZ ) )
		{
		switch( ldim->subtype )
			{
			case UA_HORIZ_DIM:
				inside_cone = UU_FALSE;
				break;
			case UA_VERT_DIM:
				inside_cone = UU_FALSE;
				break;
			default:
				{
				diff = ua_dir_angle(zaxis,yaxis,dimvect);
				if( ( diff>UA_PI ) )
					{
					diff = ( diff-UA_PI );
					}
				if( ( diff<5.230000e-001 ) )
					{
					inside_cone = UU_TRUE;
					}
				else
					{
					inside_cone = UU_FALSE;
					}
				}
			}
		um_vctovc(extvect,move_vect);
		switch( ldim->subtype )
			{
			case UA_HORIZ_DIM:
				dir_test = um_dot(extvect,yaxis);
				if( ( dir_test<0.0 ) )
					um_vctmsc(move_vect,(UU_REAL)-1.0,move_vect);
				break;
			case UA_VERT_DIM:
				um_vctmsc(move_vect,(UU_REAL)-1.0,move_vect);
				break;
			default:
				{
				um_ilnln(ldim->dim_origin,dimvect,ldim->asso_blk[0].location,
							extvect,&(num),int_pt);
				um_vcmnvc(int_pt,ldim->asso_blk[0].location,tmp1);
				um_unitvc(tmp1,move_vect);
				dir_test = um_dot(move_vect,yaxis);
				if( ( fabs(dir_test)<1.000000e-004 ) )
					{
					dir_test = um_dot(move_vect,xaxis);
					if( ( dir_test>0.0 ) )
						{
						um_vctmsc(move_vect,(UU_REAL)-1.0,move_vect);
						}
					}
				else
					{
					if( ( dir_test<0.000000e+000 ) )
						{
						um_vctmsc(move_vect,(UU_REAL)-1.0,move_vect);
						}
					}
				}
			}
		if( ( !inside_cone ) )
			{
			diff = ua_dir_angle(zaxis,yaxis,move_vect);
				{
				for(i=1;i<=ldim->txt_blk_use;i++)
					{
					ldim->txt_blk[i-1].tangle = ( ldim->txt_blk[i-1].tangle+diff );
					}
				}
			}
		}
	ua_box_site(ldim,box,dl_pt);
	ua_box_frame(ldim,box);
	off_set = 0.0;
	if( ( ( (*base_flg)==UU_FALSE )&&( ldim->txt_place==0 ) ))
		{
		off_set = ( ldim->grid_dist*( (UU_REAL)count ) );
		um_vctmsc(ext_vect,off_set,move_vect);
		um_vcplvc(ldim->dim_origin,move_vect,ldim->dim_origin);
		um_vcplvc(dl_pt,move_vect,dl_pt);
		for(i=1;i<=4;i++)
			{
			um_vcplvc(box[i-1],move_vect,box[i-1]);
			}
		}
	ua_box_move(dimvect,base_pt,base_vec,off_set,ldim,box, dl_pt);
	um_ilnln(ldim->asso_blk[0].location,extvect,dl_pt,dimvect,
					&(nint),int_point1);
	um_ilnln(ldim->asso_blk[1].location,extvect,dl_pt,dimvect,&(nint),
					int_point2);
	if( ( ldim->txt_orent>1 ) )
		{
		diff = um_dcccc(box[0],box[3]);
		diff = ( diff/2.0 );
		um_vcmnvc(int_point1,ldim->asso_blk[0].location,tmp1);
		um_unitvc(tmp1,dir_vec1);
		switch( ldim->txt_orent )
			{
			case UA_TXT_OVER:
				switch( ldim->subtype )
					{
					case UA_HORIZ_DIM:
						if( ( dir_test>0.0 ) )
							{
							um_vctmsc(dir_vec1,diff,tmp1);
							um_vcmnvc(dl_pt,tmp1,dl_pt);
							}
						else
							{
							um_vctmsc(dir_vec1,diff,tmp1);
							um_vcplvc(dl_pt,tmp1,dl_pt);
							}
						break;
					case UA_VERT_DIM:
						dir_test = um_dot(dir_vec1,xaxis);
						if( ( dir_test<0.0 ) )
							{
							um_vctmsc(dir_vec1,diff,tmp1);
							um_vcmnvc(dl_pt,tmp1,dl_pt);
							}
						else
							{
							um_vctmsc(dir_vec1,diff,tmp1);
							um_vcplvc(dl_pt,tmp1,dl_pt);
							}
						break;
					default:
						dir_test = um_dot(dir_vec1,yaxis);
						if( ( !inside_cone ) )
							{
							if( ( fabs(dir_test)<1.000000e-004 ) )
								{
								dir_test = um_dot(dir_vec1,xaxis);
								if( ( dir_test>0.0 ) )
									{
									um_vctmsc(dir_vec1,diff,tmp1);
									um_vcplvc(dl_pt,tmp1,dl_pt);
									}
								else
									{
									um_vctmsc(dir_vec1,diff,tmp1);
									um_vcmnvc(dl_pt,tmp1,dl_pt);
									}
								}
							else
								{
								if( ( dir_test<0.0 ) )
									{
									um_vctmsc(dir_vec1,diff,tmp1);
									um_vcplvc(dl_pt,tmp1,dl_pt);
									}
								else
									{
									um_vctmsc(dir_vec1,diff,tmp1);
									um_vcmnvc(dl_pt,tmp1,dl_pt);
									}
								}
							}
						else
							{
							if( ( fabs(dir_test)<1.000000e-004 ) )
								{
								dir_test = um_dot(dir_vec1,xaxis);
								if( ( dir_test>0.0 ) )
									{
									um_vctmsc(dir_vec1,ldim->txt_gap,tmp1);
									um_vcmnvc(box[0],tmp1,dl_pt);
									}
								else
									{
									um_vctmsc(dir_vec1,ldim->txt_gap,tmp1);
									um_vcplvc(box[0],tmp1,dl_pt);
									}
								}
							else
								{
								if( ( dir_test<0.0 ) )
									{
									um_vctmsc(dir_vec1,ldim->txt_gap,tmp1);
									um_vcplvc(box[0],tmp1,dl_pt);
									}
								else
									{
									um_vctmsc(dir_vec1,ldim->txt_gap,tmp1);
									um_vcmnvc(box[0],tmp1,dl_pt);
									}
								}
							}
						}
				break;
			case UA_TXT_UNDER:
				switch( ldim->subtype )
					{
					case UA_HORIZ_DIM:
						um_vctmsc(dir_vec1,diff,tmp1);
						um_vcmnvc(dl_pt,tmp1,dl_pt);
						break;
					case UA_VERT_DIM:
						dir_test = um_dot(dir_vec1,yaxis);
						if( ( dir_test<0.0 ) )
							{
							um_vctmsc(dir_vec1,diff,tmp1);
							um_vcplvc(dl_pt,tmp1,dl_pt);
							}
						else
							{
							um_vctmsc(dir_vec1,diff,tmp1);
							um_vcmnvc(dl_pt,tmp1,dl_pt);
							}
						break;
					default:
						dir_test = um_dot(dir_vec1,yaxis);
						if( ( !inside_cone ) )
							{
							if( ( fabs(dir_test)<1.000000e-004 ) )
								{
								dir_test = um_dot(dir_vec1,xaxis);
								if( ( dir_test>0.0 ) )
									{
									um_vctmsc(dir_vec1,diff,tmp1);
									um_vcmnvc(dl_pt,tmp1,dl_pt);
									}
								else
									{
									um_vctmsc(dir_vec1,diff,tmp1);
									um_vcplvc(dl_pt,tmp1,dl_pt);
									}
								}
							else
								{
								if( ( dir_test<0.0 ) )
									{
									um_vctmsc(dir_vec1,diff,tmp1);
									um_vcmnvc(dl_pt,tmp1,dl_pt);
									}
								else
									{
									um_vctmsc(dir_vec1,diff,tmp1);
									um_vcplvc(dl_pt,tmp1,dl_pt);
									}
								}
							}
						else
							{
							if( ( fabs(dir_test)<1.000000e-004 ) )
								{
								dir_test = um_dot(dir_vec1,xaxis);
								if( ( dir_test>0.0 ) )
									{
									um_vctmsc(dir_vec1,ldim->txt_gap,tmp1);
									um_vcplvc(box[2],tmp1,dl_pt);
									}
								else
									{
									um_vctmsc(dir_vec1,ldim->txt_gap,tmp1);
									um_vcmnvc(box[2],tmp1,dl_pt);
									}
								}
							else
								{
								if( ( dir_test<0.0 ) )
									{
									um_vctmsc(dir_vec1,ldim->txt_gap,tmp1);
									um_vcmnvc(box[2],tmp1,dl_pt);
									}
								else
									{
									um_vctmsc(dir_vec1,ldim->txt_gap,tmp1);
									um_vcplvc(box[2],tmp1,dl_pt);
									}
								}
							}
						}
				break;
			}
		um_ilnln(ldim->asso_blk[0].location,extvect,dl_pt,dimvect,&(nint),
						int_point1);
		um_ilnln(ldim->asso_blk[1].location,extvect,dl_pt,dimvect,&(nint),
						int_point2);
		}
	if( ( ua_text_fit(ldim,extvect,box,dl_pt,int_point1,
																int_point2)==UU_FALSE ) )
		{
		uu_dexit;
		return(2);
		}
	um_vcmnvc(int_point1,ldim->asso_blk[0].location,tmp1) ;
	um_unitvc(tmp1,dir_vec1);
	um_vcmnvc(int_point2,ldim->asso_blk[1].location,tmp1) ;
	um_unitvc(tmp1,dir_vec2);
	if( ( ldim->ext_line_sup!=UA_SUPPRESS_BOTH ) )
		{
		ldim->line_blk[0].num_pts = 0;
		ldim->line_blk[0].subtype = ext_line;
		ldim->line_blk[0].line.line_font = UA_ext_line_font;
		ldim->line_blk[0].line.line_density = UA_ext_line_dens;
		ldim->line_blk[0].line.color = UA_ext_line_color;
		ldim->line_blk_use = 1;
		if( ( ldim->ext_line_sup!=UA_SUPPRESS_FIRST ) )
			{
			um_vctmsc(dir_vec1,(*ext_offset_dist),tmp1);
			um_vcplvc(tmp1,ldim->asso_blk[0].location,ldim->line_blk[0].line_seg[0]);
			um_vctmsc(dir_vec1,ldim->ext_past_line,tmp1);
			um_vcplvc(tmp1,int_point1,ldim->line_blk[0].line_seg[1]);
			ldim->line_blk[0].num_pts = 2;
			(*ext_offset_dist) = um_dcccc(ldim->line_blk[0].line_seg[1],
										ldim->asso_blk[0].location);
			}
		if( ( ldim->ext_line_sup!=UA_SUPPRESS_SECOND ) )
			{
			ldim->line_blk[0].num_pts = ( ldim->line_blk[0].num_pts+2 );
			i = ldim->line_blk[0].num_pts;
			um_vctmsc(dir_vec2,ldim->gap_to_geo,tmp1);
			um_vcplvc(tmp1,ldim->asso_blk[1].location,ldim->
				    line_blk[0].line_seg[( i-1 )-1]);
			um_vctmsc(dir_vec2,ldim->ext_past_line,tmp1);
			um_vcplvc(tmp1,int_point2,ldim->line_blk[0].line_seg[i-1]);
			}
		}
	ldim->line_blk_use = ( ldim->line_blk_use+1 );
	i = ldim->line_blk_use;
	ldim->line_blk[i-1].subtype = dim_line;
	ldim->line_blk[i-1].line.line_font = UA_dim_line_font;
	ldim->line_blk[i-1].line.line_density = UA_dim_line_dens;
	ldim->line_blk[i-1].line.color = UA_dim_line_color;
	um_vctovc(int_point1,ldim->line_blk[i-1].line_seg[0]);
	um_vctovc(int_point2,ldim->line_blk[i-1].line_seg[1]);
	if( ( ldim->txt_orent<UA_TXT_OVER ) )
		{
		ldim->line_blk[i-1].num_pts = 4;
		um_vctovc(int_point2,ldim->line_blk[i-1].line_seg[2]);
		um_vctovc(int_point1,ldim->line_blk[i-1].line_seg[3]);
		}
	else
		{
		ldim->line_blk[i-1].num_pts = 2;
		}
	ldim->arrow_blk_use = 2;
	um_vctovc(int_point2,ldim->arrow_blk[0].location);
	um_vctovc(int_point1,ldim->arrow_blk[1].location);
	um_vcmnvc(ldim->line_blk[i-1].line_seg[1],ldim->
		    line_blk[i-1].line_seg[0],tmp1);
	ldim->arrow_blk[0].aangle = ua_dir_angle(zaxis,xaxis,tmp1);
	if( ( ldim->txt_orent<UA_TXT_OVER ) )
		{
		um_vcmnvc(ldim->line_blk[i-1].line_seg[3],ldim->
			    line_blk[i-1].line_seg[2],tmp1);
		ldim->arrow_blk[1].aangle = ua_dir_angle(zaxis,xaxis,tmp1);
		ua_trim_line(box,ldim->line_blk[i-1].line_seg[0],(*ldim)
		    .line_blk[i-1].line_seg[1],ldim->line_blk[1].line_seg[1]);
		ua_trim_line(box,ldim->line_blk[i-1].line_seg[2],(*ldim)
		    .line_blk[i-1].line_seg[3],ldim->line_blk[1].line_seg[3]);
		}
	else
		{
		um_vcmnvc(ldim->line_blk[i-1].line_seg[0],ldim->
			    line_blk[i-1].line_seg[1],tmp1);
		ldim->arrow_blk[1].aangle = ua_dir_angle(zaxis,xaxis,tmp1);
		}
	if( ldim->grid_dist == 0.0 )
		ldim->arrow_blk[1].arrow_type = UA_POINT;
	if( ( ldim->dim_type==UA_BASIC ) )
		{
		ldim->line_blk_use = ( ldim->line_blk_use+1 );
		i = ldim->line_blk_use;
		ldim->line_blk[i-1].subtype = misc_line;
		ldim->line_blk[i-1].line.line_font = UA_dim_line_font;
		ldim->line_blk[i-1].line.line_density = UA_dim_line_dens;
		ldim->line_blk[i-1].line.color = UA_dim_line_color;
		ldim->line_blk[i-1].num_pts = 8;
		um_vctovc(box[0],ldim->line_blk[i-1].line_seg[0]);
		um_vctovc(box[1],ldim->line_blk[i-1].line_seg[1]);
		um_vctovc(box[1],ldim->line_blk[i-1].line_seg[2]);
		um_vctovc(box[2],ldim->line_blk[i-1].line_seg[3]);
		um_vctovc(box[2],ldim->line_blk[i-1].line_seg[4]);
		um_vctovc(box[3],ldim->line_blk[i-1].line_seg[5]);
		um_vctovc(box[3],ldim->line_blk[i-1].line_seg[6]);
		um_vctovc(box[0],ldim->line_blk[i-1].line_seg[7]);
		}
	if( ( (*base_flg)==UU_TRUE ) )
		{
		um_vctovc(int_point1,base_pt);
		um_vcmnvc(int_point1,int_point2,tmp1);
		um_unitvc(tmp1,base_vec);
		(*base_flg) = UU_FALSE;
		um_vctovc(dir_vec1,ext_vect);
		um_vctovc(ext_vect,ldim->asso_blk[ldim->asso_blk_use-1].location);
		}
	uu_dexit;
	return(1);
	}
