/*********************************************************************
**    NAME         : alinear.c
**       CONTAINS:
**				ua_text_fit
**				ua_cre_lin_dim
**				ua_linear_dims
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       alinear.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:35
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) alinear.c 4.3 6/21/89 12:51:40 single"};
#else
static char uu_sccsident[]={"@(#) alinear.c 4.3 6/21/89 12:51:40 double"};
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

extern int UD_draftable[UD_NMENTWD];
extern int UD_draft_line[UD_NMENTWD];

/*********************************************************************
**    E_FUNCTION     : ua_linear_dims(subtype)
**       User interaction routine for the creation of linear dimensions.
**    PARAMETERS   
**       INPUT  : 
**          dim_type					Dimension types code.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_linear_dims(subtype)
int		subtype;
	{
	int		rel_num1, rel_num2, previous_txt_blk_use, entity1, entity2,
				key, closest, origin_mode, drf_key, bitary[9], previous_key,
				j, locations, status, msgno, save_grid, count, dummy, text_fit_flag, text_rot_flag;
	UU_REAL	off_set, dis1, dis2;
	UM_coord	base_pt, cpln_origin, near_pt, dim_origin, far_pt,
				unit_vec1, unit_vec2, dvec, xaxis, base_vec, yaxis, zaxis, tmp1,
				tmp2;
	UU_LOGICAL	ok, redo, first_time;
	struct UA_PLOCREC	plocrec1;
	struct UA_PLOCREC	plocrec2;
	struct UA_generic_draft	ldim;

	uu_denter(UU_STRC,(us,"SAL ua_linear_dims(subtype=%d)", subtype));

	first_time = UU_TRUE;
	origin_mode = 1;
	count = 0;
	text_rot_flag = 0;
	text_fit_flag = 0;
	ua_dump_set(0xffff);
start:
	ua_init_entity(48,subtype,&(ldim));
	if( ( first_time==UU_TRUE ) )
		save_grid = ldim.stack_grid;
	else
		ldim.stack_grid = save_grid;
	ud_lgeo(UU_TRUE,UD_draftable);
	if( ( ldim.subtype==4 ) )
		{
		ud_lgeo(UU_TRUE,UD_draft_line);
		ldim.stack_grid = 0;
		}
	if( ( ldim.subtype==5 ) ) ud_lgeo(UU_TRUE,UD_draft_line);
	status = ua_select_ent_subf(28,&(ldim),1,&(plocrec1));
	switch( status )
		{
		case 0:
			uu_dexit;
			return;
		case 2:
			if( ( first_time==UU_FALSE ) )
				{
				redo = UU_TRUE;
				ldim.key = previous_key;
				status = uc_retrieve_data(&(ldim),sizeof(struct UA_generic_draft));
				if( ( status==0 ) )
					{
					ldim.arc_blk_use = 0;
					ldim.line_blk_use = 0;
					ldim.arrow_blk_use = 0;
					ldim.asso_blk_use = 2;
					ldim.txt_blk_use = previous_txt_blk_use;
					for(j=1;j<=10;j++)
						{
						ldim.txt_blk[j-1].tangle = UA_text_ang;
						}
					ldim.stack_grid = 0;
					save_grid = origin_mode;
					origin_mode = 1;
					goto origin;
					}
				else
					{
					previous_key = 0;
					redo = UU_FALSE;
					first_time = UU_TRUE;
					goto start;
					}
				}
			else
				{
				status = ua_popmenu(1,&(origin_mode));
				if( ( status!=1 ) )
					{
					uu_dexit;
					return;
					}
				else
					{
					origin_mode = ( origin_mode+1 );
					goto start;
					}
				}
		case 1:
			redo = UU_FALSE;
			break;
		}
	ud_lgeo(UU_TRUE,UD_draftable);
	if( ( ldim.subtype==5 ) ) ud_lgeo(UU_TRUE,UD_draft_line);
	status = ua_select_ent_subf(29,&(ldim),2,&(plocrec2));
	if( ( status!=1 ) )
		{
		uu_dexit;
		return;
		}
	ldim.asso_blk_use = 2;
	ua_getcpln(&(ldim),cpln_origin,xaxis,yaxis,zaxis);
	status = ua_assoc_blk(cpln_origin,xaxis,yaxis,zaxis,&(ldim),1,2,
								&(plocrec1),&(plocrec2));
	if( ( status==1 ) )
		{
		uu_uerror0(13,17);
		goto start;
		}
	if( um_cceqcc(ldim.asso_blk[0].location,ldim.asso_blk[1].location) )
		{
		if( ( ldim.subtype==5 ) )
			uu_uerror0(13,20);
		else
			uu_uerror0(13,7);
		goto start;
		}
	if( ( ldim.subtype==5 ) )
		{
		uc_draft_endpts(2,&(plocrec1.ppick),&(plocrec1.ndcloc),&(rel_num1),
							near_pt,far_pt);
		um_vcmnvc(far_pt,near_pt,tmp1);
		um_unitvc(tmp1,unit_vec1);
		uc_draft_endpts(2,&(plocrec2.ppick),&(plocrec2.ndcloc),&(rel_num2),
								near_pt,far_pt);
		um_vcmnvc(far_pt,near_pt,tmp1);
		um_unitvc(tmp1,unit_vec2);
		if( ( !um_vcparall(unit_vec1,unit_vec2) ) )
			{
			uu_uerror0(13,19);
			goto start;
			}
		}
	if( ( ( origin_mode==3 )&&( first_time==UU_TRUE ) ) )
		{
		um_vctovc(ldim.asso_blk[0].location,base_pt);
		um_vcmnvc(ldim.asso_blk[1].location,ldim.asso_blk[0].location,tmp1);
		um_unitvc(tmp1,base_vec);
		}
origin:
	if( redo )
		{
		switch( ldim.txt_place )
			{
			case 0:
				msgno = 118;
				break;
			default:
				msgno = 118;
			}
		}
	else
		{
		switch( ldim.txt_place )
			{
			case 0:
				msgno = 30;
				break;
			default:
				msgno = 30;
			}
		}
/*
.....Origin_mode = 4
.....Align current dimension arrows
.....with an arrow from a previous dimension
*/
	if( ( origin_mode==4 ) )
		{
/*
........Get arrow from previous dimension
........to align to
*/
		ud_lgeo(UU_FALSE,UD_draftable);
		status = ua_select_ent_subf(-114,&(ldim),3,&(plocrec1));
		if( ( status!=1 ) ) goto start;
/*
........Determine which extension line to omit
*/
		drf_key = ldim.asso_blk[2].key;
		ua_arrow_loc(drf_key,&(plocrec1),cpln_origin,xaxis,yaxis,
							zaxis,base_pt,base_vec);
		dis1 = um_dcccc(base_pt,ldim.asso_blk[0].location);
		dis2 = um_dcccc(base_pt,ldim.asso_blk[1].location);
		if( ( dis1<dis2 ) )
			{
			ldim.ext_line_sup = 1;
			closest = 1;
			}
		else
			{
			ldim.ext_line_sup = 2;
			closest = 2;
			}

		ldim.stack_grid = 3;
		if( ( ldim.txt_place==1 ) )
			{
			status = ua_ent_origin_subf(30,&(ldim));
			if( ( status!=1 ) ) goto start;
			}
		else
			{
			um_vcmnvc(ldim.asso_blk[1].location,ldim.asso_blk[0].location,tmp1);
			um_vctmsc(tmp1,(UU_REAL)1.0 / (UU_REAL)2.0,tmp2);
			um_vcplvc(ldim.asso_blk[0].location,tmp2,ldim.dim_origin);
			um_vcmnvc(base_pt,ldim.asso_blk[closest-1].location,tmp1) ;
			um_unitvc(tmp1,dvec);
			um_vctmsc(dvec,ldim.grid_dist,tmp1);
			um_vcplvc(ldim.dim_origin,tmp1,ldim.dim_origin);
			}
		}
	else
		{
org_rep1:
		status = ua_ent_origin_subf(msgno,&(ldim));
		switch( status )
			{
			case 0:
				goto start;
			case 2:
				switch( ldim.txt_place )
					{
					case 0:
						ldim.txt_place = 1;
						break;
					case 1:
						ldim.txt_place = 0;
						break;
					}
				goto org_rep1;
			}
		switch( origin_mode )
			{
			case 1:
				ldim.stack_grid = 0;
				break;
			case 2:
				um_vctovc(ldim.asso_blk[0].location,base_pt);
				um_vcmnvc(ldim.asso_blk[1].location,ldim.asso_blk[0].location,tmp1);
				um_unitvc(tmp1,base_vec);
				ldim.stack_grid = 1;
				break;
			case 3:
				ldim.stack_grid = 2;
				break;
			}
		}
	if( ( redo==UU_FALSE ) )
		{
text:
		ok = ua_text_subf(&(ldim));
		if( ( !ok ) ) goto text;
		previous_txt_blk_use = ldim.txt_blk_use;
		}
restart:
	um_nptpln(ldim.dim_origin,cpln_origin,zaxis,dim_origin);
	um_vctovc(dim_origin,ldim.dim_origin);
	status = ua_cre_lin_dim(cpln_origin,xaxis,yaxis,zaxis,
					base_pt,base_vec,&(off_set),&(ldim),&(text_fit_flag),&(text_rot_flag));
	if( ( status==0 ) ) goto start;
	if( ( status==2 ) )
		{
		uu_uerror0(13,26);
		ldim.arc_blk_use = 0;
		ldim.line_blk_use = 0;
		ldim.arrow_blk_use = 0;
		ldim.asso_blk_use = 2;
		ldim.txt_blk_use = previous_txt_blk_use;
		for(j=1;j<=10;j++)
			{
			ldim.txt_blk[j-1].tangle = UA_text_ang;
			}
		ldim.txt_place = 1;
		ldim.arrow_place = 1;
		text_fit_flag = 1;
		msgno = 129;
		status = ua_ent_origin_subf(msgno,&(ldim));
		if( ( status!=1 ) ) goto start;
		goto restart;
		}
	if( redo )
		{
		origin_mode = save_grid;
		status = ua_update_entity(previous_key,&(ldim));
		if( ( status!=0 ) )
			ua_create_entity(&(ldim),&(key));
		}
	else
		ua_create_entity(&(ldim),&(key));
	uc_display(&(ldim));
	previous_key = ldim.key;
	first_time = UU_FALSE;
	count = ( count+1 );
	goto start;
	}
/*********************************************************************
**    E_FUNCTION     : ua_text_fit(ldim, extvect, box, dl_pt, int_pt1
**										, int_pt2)
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
UU_LOGICAL	ua_text_fit(ldim, extvect, box, dl_pt, int_pt1 , int_pt2)
struct UA_generic_draft	(*ldim);
UM_coord	extvect,	box[4],dl_pt,int_pt1,int_pt2;
	{
	int		num, i, j, k;
	UU_REAL	size;
	UM_coord	pt1, pt2, vec1, vec2, int_pt, spt1, spt2, tmp1, tmp2;
	UU_LOGICAL status;
	UU_LOGICAL um_ptinseg();

	uu_denter(UU_STRC,(us,"SAL ua_text_fit(ldim=%s, extvect=<%g,%g,%g>,\
		box=%s, dl_pt=<%g,%g,%g>, int_pt1=<%g,%g,%g>, int_pt2=<%g,%g,%g>)",
		"...", extvect[0],extvect[1],extvect[2], "...",
		dl_pt[0],dl_pt[1],dl_pt[2], int_pt1[0],int_pt1[1],int_pt1[2],
		int_pt2[0],int_pt2[1],int_pt2[2]));


	status = UU_TRUE;
	um_vcmnvc(dl_pt,int_pt1,tmp1);
	um_unitvc(tmp1,vec1);
	um_vcmnvc(dl_pt,int_pt2,tmp1);
	um_unitvc(tmp1,vec2);
	size = ldim->arrow_size;
/*
.....Make sure arrows are not tool large
.....to fit inside dimension lines
.....size    = arrow size
.....int_pt1 = Point on lower dimension line
.....int_pt2 = Point on upper dimension line
.....Bobby    10/12/93
*/
	if (size*2 > um_dcccc(int_pt1,int_pt2))
	{
		status = UU_FALSE;
		goto fexit;
	}

	um_vctmsc(vec1,size,tmp1);
	um_vcplvc(int_pt1,tmp1,spt1);
	um_vctmsc(vec2,size,tmp1);
	um_vcplvc(int_pt2,tmp1,spt2);
	for(i=1;i<=4;i++)
		{
		um_vctovc(box[i-1],pt1);
		if( ( i==4 ) )
			{
			um_vctovc(box[0],pt2);
			um_vcmnvc(box[3],box[0],tmp1);
			um_unitvc(tmp1,vec2);
			}
		else
			{
			um_vctovc(box[( i+1 )-1],pt2);
			um_vcmnvc(box[( i+1 )-1],box[i-1],tmp1);
			um_unitvc(tmp1,vec2);
			}
		um_ilnln(spt1,extvect,pt1,vec2,&(num),int_pt);
		if( ( num>0 ) )
			{
			if( um_ptinseg(pt1,int_pt,pt2) )
				{
				status = UU_FALSE;
				goto fexit;
				}
			}
		um_ilnln(spt2,extvect,pt1,vec2,&(num),int_pt);
		if( ( num>0 ) )
			{
			if( um_ptinseg(pt1,int_pt,pt2) )
				{
				status = UU_FALSE;
				goto fexit;
				}
			}
		}
fexit:
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : int		ua_cre_lin_dim(cpln_origin, xaxis, yaxis, zaxis, 
**												base_pt, base_vec, off_set, ldim, text_fit_flag,
**              text_rot_flag)
**       
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_cre_lin_dim(cpln_origin, xaxis, yaxis, zaxis, 
								base_pt, base_vec, off_set, ldim, text_fit_flag, text_rot_flag)
UM_coord	cpln_origin, xaxis, yaxis, zaxis, base_pt, base_vec;
UU_REAL	(*off_set);
struct UA_generic_draft	(*ldim);
int (*text_fit_flag), (*text_rot_flag);
	{
	int		use, num, rel_num, i, j, k, nint;
	UU_REAL	box_maxext, box_extent, box_minext, dim_minext, dim_maxext, diff,
				distance, dis1, temp_extent, del_num, test_dir, dir_test, dis[4];
	UM_coord	move_vect, dimvect, ept, box[4], extvect, spt, new_loc,
				int_point1, int_point2, vec1, vec2, ept_cp, int_pts[2], test_vector,
				dir_vec1, spt_cp, dir_vec2, dvec, int_pt, dl_pt, tmp1, tmp2;
	UU_LOGICAL	inside_cone = UU_TRUE;
	UU_LOGICAL	box_inside;
	UU_LOGICAL	ua_text_fit();

	uu_denter(UU_STRC,(us,"SAL ua_cre_lin_dim(cpln_origin=<%g,%g,%g>,\
		xaxis=<%g,%g,%g>, yaxis=<%g,%g,%g>, zaxis=<%g,%g,%g>,\
		base_pt=<%g,%g,%g>, base_vec=<%g,%g,%g>, off_set=%s,\
		ldim=%s)", cpln_origin[0],cpln_origin[1],cpln_origin[2],
		xaxis[0],xaxis[1],xaxis[2], yaxis[0],yaxis[1],yaxis[2],
		zaxis[0],zaxis[1],zaxis[2], base_pt[0],base_pt[1],base_pt[2],
		base_vec[0],base_vec[1],base_vec[2], "...", "..."));

	switch( ldim->subtype )
		{
		case 1:
			um_vctovc(yaxis,extvect);
			um_vctovc(xaxis,dimvect);
			ldim->dim_value = ua_dim_value(horiz_dist,two_dim,dimvect,
						ldim->asso_blk[0].location,ldim->asso_blk[1].location);
			break;
		case 2:
			um_vctovc(xaxis,extvect);
			um_vctovc(yaxis,dimvect);
			ldim->dim_value = ua_dim_value(vert_dist,two_dim,dimvect,
						ldim->asso_blk[0].location,ldim->asso_blk[1].location);
			break;
		case 4:
		case 3:
			um_vcmnvc(ldim->asso_blk[1].location,ldim->asso_blk[0].location,tmp1);
			um_unitvc(tmp1,dimvect);
			um_cross(dimvect,zaxis,tmp1);
			um_unitvc(tmp1,extvect);
			ldim->dim_value = ua_dim_value(shortest,two_dim,dimvect,
				ldim->asso_blk[0].location,ldim->asso_blk[1].location);
			break;
		case 5:
			um_vcmnvc(ldim->asso_blk[1].location,ldim->asso_blk[0].location,
							tmp1);
			um_unitvc(tmp1,dimvect);
			um_cross(dimvect,zaxis,tmp1);
			um_unitvc(tmp1,extvect);
			ldim->dim_value = ua_dim_value(shortest,two_dim,dimvect,
					ldim->asso_blk[0].location,ldim->asso_blk[1].location);
			ldim->ext_line_sup = 3;
			ldim->arrow_place = 1;
			break;
		}
	if( ( ldim->dim_value<1.000000e-004 ) )
		{
		uu_uerror0(13,16);
		uu_dexit;
		return(0);
		}
	if( ( ldim->txt_entry==0 ) )
		ua_set_dim_text(&((*ldim)));
	if( ( ldim->txt_orent!=0 ) )
		{
		switch( ldim->subtype )
			{
			case 1:
				inside_cone = UU_FALSE;
				break;
			case 2:
				inside_cone = UU_FALSE;
				break;
			default:
				diff = ua_dir_angle(zaxis,yaxis,dimvect);
				if( ( diff>3.141593e+000 ) )
					diff = ( diff-3.141593e+000 );
				if( ( diff<5.230000e-001 ) )
					inside_cone = UU_TRUE;
				else
					inside_cone = UU_FALSE;
			}
		um_vctovc(extvect,move_vect);
		switch( ldim->subtype )
			{
			case 1:
				dir_test = um_dot(extvect,yaxis);
				if( ( dir_test<0.0 ) )
					um_vctmsc(move_vect,(UU_REAL)-1.0,move_vect);
				break;
			case 2:
				um_vctmsc(move_vect,(UU_REAL)-1.0,move_vect);
				break;
			default:
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
					if( ( dir_test<0.0 ) )
						{
						um_vctmsc(move_vect,(UU_REAL)-1.0,move_vect);
						}
					}
				break;
			}
		}
	if( *text_rot_flag == 0 )
	{
		if( ( !inside_cone ) )
		{
			diff = ua_dir_angle(zaxis,yaxis,move_vect);
			for(i=1;i<=ldim->txt_blk_use;i++)
			{
				ldim->txt_blk[i-1].tangle = ( ldim->txt_blk[i-1].tangle+ diff );
			}
		}
	}
	ua_box_site(ldim,box,dl_pt);
	ua_box_frame(ldim,box);
	(*off_set) = 0.0;
	switch( ldim->stack_grid )
		{
		case 1:
			(*off_set) = ldim->grid_dist;
			break;
		case 2:
			ldim->stack_grid = 1;
			um_cross(base_vec,zaxis,tmp1);
			um_unitvc(tmp1,dvec);
			um_ilnln(base_pt,base_vec,dl_pt,dvec,&(num),int_pt);
			dis1 = um_dcccc(dl_pt,int_pt);
			for(i=1;i<=100;i++)
				{
				del_num = ( ( (UU_REAL)i )*ldim->grid_dist );
				diff = ( dis1-del_num );
				if( ( diff<0.0 ) )
					{
					num = ( i-1 );
					if( ( num==0 ) )
						{
						num = 1;
						goto us_l175;
						}
					del_num = ( ( (UU_REAL)num )*ldim->grid_dist );
					diff = ( dis1-del_num );
					if( ( diff>( 5.000000e-001*ldim->grid_dist ) ) )
						{
						num = ( num+1 );
						}
					goto us_l175;
					}
				}
us_l175: ;
			(*off_set) = ( ( (UU_REAL)num )*ldim->grid_dist );
			break;
		case 3:
			ldim->stack_grid = 1;
			(*off_set) = 0.0;
			break;
		}
	ua_box_move(dimvect,base_pt,base_vec,(*off_set),ldim, box,dl_pt);
	um_ilnln(ldim->asso_blk[0].location,extvect,dl_pt, dimvect,&(nint),
					int_point1);
	um_ilnln(ldim->asso_blk[1].location,extvect,dl_pt, dimvect,&(nint),
					int_point2);
	if( ( ( ldim->txt_orent>1 )&&( ldim->arrow_place==0 ) ))
		{
		um_vcmnvc(int_point1,ldim->asso_blk[0].location,tmp1);
		um_unitvc(tmp1,dir_vec1);
		diff = um_dcccc(box[0],box[3]);
		diff = ( diff/2.0 );
		switch( ldim->txt_orent )
			{
			case 2:
				switch( ldim->subtype )
					{
					case 1:
						dir_test = um_dot(dir_vec1,yaxis);
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
					case 2:
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
					case 5:
						um_vctovc(dl_pt,dl_pt);
						break;
					default:
						dir_test = um_dot(dir_vec1,yaxis);
						if( ( !inside_cone ) )
							{
							if( ( fabs(dir_test)<1.000000e-004 ) )
								{
								dir_test = um_dot(dir_vec1,xaxis);
								if( ( dir_test>0.0) )
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
			case 3:
				switch( ldim->subtype )
					{
					case 1:
						um_vctmsc(dir_vec1,diff,tmp1);
						um_vcmnvc(dl_pt,tmp1,dl_pt);
						break;
					case 2:
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
					case 5:
						um_vctovc(dl_pt,dl_pt);
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
		um_ilnln(ldim->asso_blk[0].location,extvect,dl_pt, dimvect,&(nint),
							int_point1);
		um_ilnln(ldim->asso_blk[1].location,extvect,dl_pt, dimvect,&(nint),
							int_point2);
		}
	um_vcmnvc(int_point1,ldim->asso_blk[0].location,tmp1) ;
	um_unitvc(tmp1,dir_vec1);
	um_vcmnvc(int_point2,ldim->asso_blk[1].location,tmp1) ;
	um_unitvc(tmp1,dir_vec2);
	um_vcmnvc(int_point1,dl_pt,tmp1);
	um_unitvc(tmp1,vec1);
	um_vcmnvc(int_point2,dl_pt,tmp1);
	um_unitvc(tmp1,vec2);
	if( ( um_dot(vec1,vec2)>-5.000000e-001 ) )
		box_inside = UU_FALSE;
	else
		box_inside = UU_TRUE;
	if( ( box_inside==UU_TRUE ) )
		{
		if(  !ua_text_fit(ldim,extvect,box,dl_pt,int_point1,int_point2) )
			{
			uu_dexit;
			return(2);
			}
		}
	if( ( ldim->ext_line_sup!=3 ) )
		{
		ldim->line_blk[0].num_pts = 0;
		ldim->line_blk[0].subtype = ext_line;
		ldim->line_blk[0].line.line_font = UA_ext_line_font;
		ldim->line_blk[0].line.line_density = UA_ext_line_dens;
		ldim->line_blk[0].line.color = UA_ext_line_color;
		ldim->line_blk_use = 1;
		if( ( ldim->ext_line_sup!=1 ) )
			{
			um_vctmsc(dir_vec1,ldim->gap_to_geo,tmp1);
			um_vcplvc(tmp1,ldim->asso_blk[0].location,ldim->
				    line_blk[0].line_seg[0]);
			um_vctmsc(dir_vec1,ldim->ext_past_line,tmp1);
			um_vcplvc(tmp1,int_point1,ldim->line_blk[0].line_seg[1]);
			ldim->line_blk[0].num_pts = 2;
			if( ( ldim->subtype==4 ) )
				{
				uc_draft_line(ldim->asso_blk[0].key,spt,ept);
				um_nptpln(spt,cpln_origin,zaxis,spt_cp);
				um_nptpln(ept,cpln_origin,zaxis,ept_cp);
				if( ( um_dcccc(int_point1,spt_cp)<um_dcccc(int_point1,ept_cp) ) )
					um_vctovc(spt_cp,new_loc);
				else
					um_vctovc(ept_cp,new_loc);
				if( um_ptinseg(spt_cp,int_point1,ept_cp) )
					{
					ldim->ext_line_sup = 1;
					ldim->line_blk[0].num_pts = 0;
					}
				um_vcmnvc(int_point1,new_loc,tmp1);
				um_unitvc(tmp1,dir_vec1);
				um_vctmsc(dir_vec1,ldim->gap_to_geo,tmp1);
				um_vcplvc(tmp1,new_loc,ldim->line_blk[0].line_seg[0]);
				um_vctmsc(dir_vec1,ldim->ext_past_line,tmp1);
				um_vcplvc(tmp1,int_point1,ldim->line_blk[0].line_seg[1]);
				}
			}
		if( ( ldim->ext_line_sup!=2 ) )
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
	if( ( ( ldim->txt_orent<2 )&&( box_inside==UU_TRUE ) ) )
		{
		ldim->line_blk[i-1].num_pts = 4;
		um_vctovc(int_point2,ldim->line_blk[i-1].line_seg[2]);
		um_vctovc(int_point1,ldim->line_blk[i-1].line_seg[3]);
		}
	else
		{
		if( ( box_inside==UU_FALSE ) )
			{
			um_vcmnvc(int_point1,int_point2,tmp1);
			um_unitvc(tmp1,vec1);
			ua_box_int(box,int_point1,vec1,int_pts);
			dis[0] = um_dcccc(int_point1,int_pts[0]);
			dis[1] = um_dcccc(int_point1,int_pts[1]);
			dis[2] = um_dcccc(int_point2,int_pts[0]);
			dis[3] = um_dcccc(int_point2,int_pts[1]);
			distance = dis[0];
			j = 1;
			k = 2;
			for(k=2;k<=4;k++)
				{
				if( ( distance>dis[k-1] ) )
					{
					distance = dis[k-1];
					j = k;
					}
				}
			ldim->line_blk[i-1].num_pts = 4;
			switch( j )
				{
				case 1:
					um_vctovc(int_pts[0],ldim->line_blk[i-1].line_seg[2]);
					um_vctovc(int_point1,ldim->line_blk[i-1].line_seg[3]);
					break;
				case 2:
					um_vctovc(int_pts[1],ldim->line_blk[i-1].line_seg[2]);
					um_vctovc(int_point1,ldim->line_blk[i-1].line_seg[3]);
					break;
				case 3:
					um_vctovc(int_point2,ldim->line_blk[i-1].line_seg[2]);
					um_vctovc(int_pts[0],ldim->line_blk[i-1].line_seg[3]);
					break;
				case 4:
					um_vctovc(int_point2,ldim->line_blk[i-1].line_seg[2]);
					um_vctovc(int_pts[1],ldim->line_blk[i-1].line_seg[3]);
					break;
				}
			}
		else
			{
			ldim->line_blk[i-1].num_pts = 2;
			}
		}
	ldim->arrow_blk_use = 2;
	um_vctovc(int_point2,ldim->arrow_blk[0].location);
	um_vctovc(int_point1,ldim->arrow_blk[1].location);
	if( ( ldim->arrow_place==0 ) )
		{
		um_vcmnvc(ldim->line_blk[i-1].line_seg[1],ldim->
			    line_blk[i-1].line_seg[0],tmp1);
		ldim->arrow_blk[0].aangle = ua_dir_angle(zaxis,xaxis, tmp1);
		um_vcmnvc(ldim->line_blk[i-1].line_seg[0],ldim->
			    line_blk[i-1].line_seg[1],tmp1);
			ldim->arrow_blk[1].aangle = ua_dir_angle(zaxis,xaxis, tmp1);
		if( ( ( ldim->txt_orent<2 )&&( box_inside==UU_TRUE ) ) )
			{
			ua_trim_line(box,ldim->line_blk[i-1].line_seg[0],(*ldim)
			    .line_blk[i-1].line_seg[1],ldim->line_blk[i-1].line_seg[1]);
			ua_trim_line(box,ldim->line_blk[i-1].line_seg[2],(*ldim)
			    .line_blk[i-1].line_seg[3],ldim->line_blk[i-1].line_seg[3]);
			}
		}
	else
		{
		um_vcmnvc(ldim->line_blk[i-1].line_seg[0],ldim->
			    line_blk[i-1].line_seg[1],tmp1);
		ldim->arrow_blk[0].aangle = ua_dir_angle(zaxis,xaxis, tmp1);
		um_vcmnvc(ldim->line_blk[i-1].line_seg[1],ldim->
			    line_blk[i-1].line_seg[0],tmp1);
		ldim->arrow_blk[1].aangle = ua_dir_angle(zaxis,xaxis, tmp1);
		ldim->line_blk[i-1].num_pts = 4;
		um_vcmnvc(ldim->line_blk[i-1].line_seg[0],ldim->
			    line_blk[i-1].line_seg[1],tmp1);
		um_unitvc(tmp1,dvec);
		if( ( box_inside==UU_TRUE ) )
			{
			dis1 = ( 3.0*ldim->arrow_size );
			}
		else
			{
			dis1 = distance;
			}
		um_vctovc(ldim->line_blk[i-1].line_seg[1],ldim->line_blk[i-1].line_seg[2]);
		um_vctmsc(dvec,dis1,tmp1);
		um_vcplvc(ldim->line_blk[i-1].line_seg[0],tmp1,(*ldim).line_blk[i-1].
						line_seg[1]);
		um_vctmsc(dvec,(UU_REAL)-1.0,tmp1);
		um_vctmsc(tmp1,dis1,tmp2);
		um_vcplvc(ldim->line_blk[i-1].line_seg[2],tmp2,ldim->line_blk[i-1].
						line_seg[3]);
		}
	if( ( ldim->dim_type==1 ) )
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
	if(( ldim->draft_stand == 1 ) && ((*text_fit_flag) == 1 ))
	{
		ldim->line_blk_use = ( ldim->line_blk_use+1 );
		i = ldim->line_blk_use;
		ldim->line_blk[i-1].subtype = dim_line;
		ldim->line_blk[i-1].line.line_font = UA_dim_line_font;
		ldim->line_blk[i-1].line.line_density = UA_dim_line_dens;
		ldim->line_blk[i-1].line.color = UA_dim_line_color;
		ldim->line_blk[i-1].num_pts = 2;
		um_vctovc(int_point1,ldim->line_blk[i-1].line_seg[0]);
		um_vctovc(int_point2,ldim->line_blk[i-1].line_seg[1]);
	}
	uu_dexit;
	return(1);
	}
