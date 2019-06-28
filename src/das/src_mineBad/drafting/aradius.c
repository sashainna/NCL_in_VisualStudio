/*********************************************************************
**    NAME         :aradius.c
**       CONTAINS:
**					ua_radius_cre
**					ua_radius_plus
**					ua_radius_regen
**					ua_radius
**					ua_rad_sym_adjust
**					ua_rad_txt
**					ua_rad_center
**					ua_rad_outside
**					ua_rad_large
**					ua_box_int
**					ua_rad_ext
**					ua_rad_txt_ang
**					ua_crea_long_leader
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aradius.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:38
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aradius.c 4.5 8/11/89 13:38:12 single"};
#else
static char uu_sccsident[]={"@(#) aradius.c 4.5 8/11/89 13:38:12 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"
#include "mdcoord.h"

extern int UD_draftable[UD_NMENTWD];

us_init_aradius()
{
}
/*********************************************************************
**    E_FUNCTION     : ua_box_int(corner, center, vec1, int_pts)
**       Intersect dimension line with text box.
**    PARAMETERS   
**       INPUT  : 
**				corner						text box
**				center						reference point to use in selecting points
**				vec1							unit vector along dimension line
**       OUTPUT :  
**				int_pts						correct intersection points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_box_int(corner, center, vec1, int_pts)
	UM_coord corner[4], center, vec1, int_pts[2];
	{
	UM_coord	temp, pt1, save_pt[4], pt[4], vec2, int_pt, tmp1, tmp2, tmp3;
	UU_REAL	dis1, dis2;
	int		num, i, j, n;

	uu_denter(UU_STRC,(us,"ua_box_int(center=<%g,%g,%g>,vec1=<%g,%g,%g>)",
		center[0],center[1],center[2],vec1[0],vec1[1],vec1[2], "..."));

	i = 1;
	j = 1;
	for(j=1;j<=4;j++)
		{
		um_vctovc(corner[j-1],pt1);
		if( ( j==4 ) )
			{
			um_vcmnvc(corner[3],corner[0],vec2);
			}
		else
			{
			um_vcmnvc(corner[j],corner[j-1],vec2);
			}
		um_unitvc(vec2,vec2);
		um_ilnln(center,vec1,pt1,vec2,&(num),int_pt);
		if( ( num==1 ) )
			{
			um_vctovc(int_pt,pt[i-1]);
			i = ( i+1 );
			}
		}
	num = ( i-1 );
	um_vcmnvc(corner[1],corner[0],tmp1);
	um_vctmsc(tmp1,(UU_REAL) 1.0 / 2.0,tmp2);
	um_vcplvc(corner[0],tmp2,tmp3);
	um_vcmnvc(corner[3],corner[0],tmp1);
	um_vctmsc(tmp1,(UU_REAL) 1.0 / 2.0,tmp2);
	um_vcplvc(tmp3,tmp2,temp);
	for(i=1;i<=2;i++)
		{
		j = ( j+1 );
		j = us_clospnt(num,pt,temp);
		um_vctovc(pt[j-1],save_pt[i-1]);
		pt[j-1][0] = 1.0;
		pt[j-1][1] = 0.0;
		pt[j-1][2] = 0.0;
		}
	dis1 = um_dcccc(center,save_pt[0]);
	dis2 = um_dcccc(center,save_pt[1]);
	if( ( dis1>dis2 ) )
		{
		um_vctovc(save_pt[0],temp);
		um_vctovc(save_pt[1],save_pt[0]);
		um_vctovc(temp,save_pt[1]);
		}
	um_vctovc(save_pt[0],int_pts[0]);
	um_vctovc(save_pt[1],int_pts[1]);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_merge_line_blk(rdim)
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
ua_merge_line_blk(rdim)
struct UA_generic_draft	(*rdim);
	{
	int		mpts, npts, i, j, m, n;

	uu_denter(UU_STRC,(us,"ua_merge_line_blk(rdim=%s)", "..."));

	n = (*rdim).line_blk_use;
	if( ( n>1 ) )
		{
		m = ( n-1 );
		npts = (*rdim).line_blk[n-1].num_pts;
		mpts = (*rdim).line_blk[m-1].num_pts;
		j = 1;
			{
			int		us_t172 = ( mpts+npts );
			for(i = (mpts+1);i <= us_t172;i++,j++)
				um_vctovc((*rdim).line_blk[n-1].line_seg[j-1],
				    (*rdim).line_blk[m-1].line_seg[i-1]);
			}
		(*rdim).line_blk_use = m;
		(*rdim).line_blk[m-1].num_pts = ( mpts+npts );
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_rad_txt(rdim, radius, corner, center)
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
ua_rad_txt(rdim, radius, corner, center)
struct UA_generic_draft	(*rdim);
UU_REAL	radius;
UM_coord	corner[4];
UM_coord	center;
	{
	char		temp[1025];
	char		rsym[13];
	UM_coord	new_pt, dl_pt, del_off, c_pt, lab_box[4], mdx, mdy,
						sdx, sdy, vec1, vec2;
	UU_REAL	mx_del, my_del, sx_del, sy_del;
	int		num, i, j;

	static char		crsym[1025] = "\\n";

	uu_denter(UU_STRC,(us,"ua_rad_txt(radius=%g)", radius));

	(*rdim).dim_value = radius;
	if( ( (*rdim).txt_entry==0 ) )
		{
		switch( (*rdim).rad_symb )
			{
			case 0:
				{
				goto offset;
				}
			case 1:
				{
				strcpy(rsym,"R");
				num = 1;
				}
				break;
			case 2:
				{
				strcpy(rsym,"RAD");
				num = 3;
				}
				break;
			case 3:
				{
				strcpy(rsym,UA_usr_rad_sym);
				num = strlen(rsym);
				}
				break;
			}
		(*rdim).txt_blk_use = ( (*rdim).txt_blk_use+1 );
		i = (*rdim).txt_blk_use;
		(*rdim).txt_blk[i-1].char_cnt = num;
		strcpy((*rdim).txt_blk[i-1].tstring,rsym);
		(*rdim).txt_blk[i-1].subtype = dia_rad_sym;
		ua_set_dim_text(&((*rdim)));
		}
offset:
	if ((*rdim).txt_orent > 0)
		ua_rad_txt_ang(&(*rdim),center);
	ua_box_site(&((*rdim)),corner,dl_pt);
	ua_box_frame(&((*rdim)),corner);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_radius_cre(type, mode, rdim, center, radius, 
**									dtheta, normal, c_spt, c_ept)
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
int	ua_radius_cre(type, mode, rdim, center, radius, 
		dtheta, normal, c_spt, c_ept)
int		type;
UU_LOGICAL	mode;
struct UA_generic_draft	(*rdim);
UU_REAL		radius, dtheta;
UM_coord	center, normal, c_spt, c_ept;
	{
	int		dir, ablock, i, j, d_stat, k, l, m, n, dummy;
	UM_coord pt1, origin, corner[4], flex_pt, end_pt, cir_pt, xaxis,
					yaxis, zaxis, location;
	UU_LOGICAL	drw_leader, rev_arr;

	uu_denter(UU_STRC,(us,"ua_radius_cre(type=%d,center=<%g,%g,%g>,radius=%g)",
		type,center[0],center[1],center[2], radius));

	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
	ua_rad_txt(&((*rdim)),radius,corner,center);
	drw_leader = UU_TRUE;
	rev_arr = UU_FALSE;
	switch( type )
		{
		case 4:
			{
			ua_rad_center(rdim,corner,center,normal,radius,pt1,
				cir_pt,&(dir),&(rev_arr),&(drw_leader));
			ua_radius_plus(rdim, center, xaxis, yaxis);
			}
			break;
		case 5:
			{
			ua_rad_outside(rdim,corner,center,normal,radius,c_spt,
				c_ept,dtheta,pt1,cir_pt,&(dir));
			ua_radius_plus(rdim, center, xaxis, yaxis);
			rev_arr = UU_TRUE;
			}
			break;
		case 6:
			{
			if( ( mode==UU_TRUE ) )
				{
				d_stat = ud_world_coord(13,35,location,1,&(dummy),UU_FALSE);
				if( ( ( d_stat==0 )||( dummy==0 ) ) )
					{
					uu_dexit;
					return(0);
					}
				um_nptpln(location,origin,zaxis,end_pt);
				d_stat = ud_world_coord(13,43,location,1,&(dummy),UU_FALSE);
				if( ( ( d_stat==0 )||( dummy==0 ) ) )
					{
					uu_dexit;
					return(0);
					}
				um_nptpln(location,origin,zaxis,flex_pt);
				ablock = ( (*rdim).asso_blk_use+1 );
				um_vctovc(end_pt,(*rdim).asso_blk[ablock-1].location);
				(*rdim).asso_blk[ablock-1].asso_type = 102;
				(*rdim).asso_blk[ablock-1].key = 0;
				ablock = ( ablock+1 );
				um_vctovc(flex_pt,(*rdim).asso_blk[ablock-1].location);
				(*rdim).asso_blk[ablock-1].asso_type = 103;
				(*rdim).asso_blk[ablock-1].key = 0;
				(*rdim).asso_blk_use = ablock;
				}
			else
				{
				um_vctovc((*rdim).asso_blk[1].location,end_pt);
				um_vctovc((*rdim).asso_blk[2].location,flex_pt);
				}
			ua_rad_large(&((*rdim)),corner,center,normal,radius,pt1,
				cir_pt,end_pt,flex_pt,&(rev_arr),&(drw_leader));
			ua_radius_plus(rdim, end_pt, xaxis, yaxis);
			}
			break;
		}
	if( ( (! um_cceqcc( c_spt, c_ept ) )&&( fabs(( fabs(dtheta)-
	    3.141593 ))>0.01 ) ) )
		{
		ua_rad_ext(&((*rdim)),cir_pt,center,normal,radius,dtheta,
		c_spt,c_ept);
		}
	if( drw_leader )
		{
		if ((*rdim).txt_orent == 0)
			ua_crea_leader(&(*rdim),pt1,cir_pt,dir);
		else
			ua_crea_long_leader(&(*rdim),corner,center,cir_pt);
		ua_merge_line_blk(&(*rdim));
		}
	if( rev_arr )
		{
		m = (*rdim).arrow_blk_use;
		(*rdim).arrow_blk[m-1].aangle = ( (*rdim).arrow_blk[m-1].
		    aangle+3.141593 );
		}
	uu_dexit;
	return(1);
	}
/*********************************************************************
**    E_FUNCTION     : int		ua_radius_plus(rdim, center, xaxis, yaxis )
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
int		ua_radius_plus(rdim, center, xaxis, yaxis )
struct UA_generic_draft	*rdim;
UM_coord	center, xaxis, yaxis;
	{
	int		n;
	UM_coord pt1, pt2, pt3;
	UU_REAL     plus_size;

	uu_denter(UU_STRC,(us,"ua_radius_plus"));

	plus_size = rdim->char_size/4.0;
	um_vctmsc(xaxis, plus_size, pt1);
	um_vcplvc(center, pt1, pt2);
	um_vcmnvc(center, pt1, pt3);
	n = rdim->line_blk_use+1;
	rdim->line_blk_use = n;
	rdim->line_blk[n-1].num_pts = 4;
	rdim->line_blk[n-1].subtype = dim_line;
	um_vctovc(pt2,rdim->line_blk[n-1].line_seg[0]);
	um_vctovc(pt3,rdim->line_blk[n-1].line_seg[1]);
	um_vctmsc(yaxis, plus_size, pt1);
	um_vcplvc(center, pt1, pt2);
	um_vcmnvc(center, pt1, pt3);
	um_vctovc(pt2,rdim->line_blk[n-1].line_seg[2]);
	um_vctovc(pt3,rdim->line_blk[n-1].line_seg[3]);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_rad_large(rdim, corner, center, normal, radius
**											, pt1, cir_pt, end_pt, flex_pt)
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
ua_rad_large(rdim, corner, center, normal, radius , pt1, cir_pt,
	end_pt, flex_pt, rev_arr, drw_leader)
struct UA_generic_draft	(*rdim);
UM_coord corner[4], center, normal, pt1, cir_pt, end_pt, flex_pt;
UU_REAL	radius;
UU_LOGICAL	(*rev_arr);
UU_LOGICAL	(*drw_leader);
	{
	int		num, i, j, d_stat, k, l, m, n, dummy;
	UM_coord	cir_int_pt[2], temp, pt2, corn_mod[4], off_s_pt[2], origin,
				cent_pt, pt[4], vec1, vec2, int_pts[2], ref_pt, int_pt, xaxis,
				yaxis, zaxis, vnorm, dl_pt;
	UU_REAL	txt_ang, dis1, dis2, sign;

	uu_denter(UU_STRC,(us,"ua_rad_large(center=<%g,%g,%g>, normal=<%g,%g,%g>)",
		center[0],center[1],center[2],normal[0],normal[1],normal[2]));

	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
		{
		UU_REAL	us_t173[3], us_t174[3], us_t175[3], us_t176[3], us_t177[3];
		um_vcmnvc(corner[3],corner[0],us_t175);
		um_vctmsc(us_t175,(UU_REAL) 1.0 / 2.000000e+000,us_t174);
		um_vcplvc(corner[0],us_t174,us_t173);
		um_vcmnvc(corner[1],corner[0],us_t177);
		um_vctmsc(us_t177,(UU_REAL) 1.0 / 2.000000e+000,us_t176);
		um_vcplvc(us_t173,us_t176,cent_pt);
		}
	if( ( (*rdim).txt_orent==0 ) )
		{
		um_vctovc(cent_pt,ref_pt);
		(*drw_leader) = UU_FALSE;
		}
	else
		{
		UU_REAL	us_t178[3], us_t179[3];
		UU_REAL hold_mag;
		UU_REAL	rad_line[3];
		UU_REAL rad_line_angle;
		um_vcmnvc(corner[1],corner[2],us_t179);
		hold_mag = um_mag(us_t179);
		for(i=0;i<3;i++)
			us_t179[i] = us_t179[i] / hold_mag;
		hold_mag = hold_mag + (*rdim).txt_gap;
		um_vctmsc(us_t179,hold_mag,us_t179);
		um_vcmnvc((*rdim).dim_origin,center,rad_line);
		rad_line_angle = um_angle(xaxis,rad_line);
		switch ((*rdim).txt_orent)
			{
			case 3:
				if ((rad_line_angle > 1.570796)
				    &&(rad_line_angle < 4.712389))
					um_vcplvc(corner[0],us_t179,ref_pt);
				else
					um_vcplvc(corner[1],us_t179,ref_pt);
				break;
			default:
				if ((rad_line_angle < 1.570796)
				    ||(rad_line_angle > 4.712389))
					um_vcplvc(corner[2],us_t179,ref_pt);
				else
					um_vcplvc(corner[3],us_t179,ref_pt);
					break;
			}
		}
	um_ilnpln(ref_pt,zaxis,center,normal,&(num),int_pt);
		{
		UU_REAL	us_t180[3];
		um_vcmnvc(center,int_pt,us_t180);
		um_unitvc(us_t180,vec1);
		}
	um_ilncir(center,vec1,center,normal,radius,&(num),pt);
	j = us_clospnt(2,pt,int_pt);
	um_vctovc(pt[j-1],cir_pt);
		{
		UU_REAL hold_vec[3];
		um_vcmnvc(cir_pt,int_pt,hold_vec);
		um_unitvc(hold_vec,vec2);
		}
	sign = um_dot(vec1,vec2);
	for(i=0;i<2;i++)
		{
		um_vctovc(pt[i],temp);
		um_ilnpln(temp,zaxis,origin,zaxis,&(num),int_pt);
		um_vctovc(int_pt,pt[i]);
		}
	um_vctovc(pt[j-1],cir_int_pt[0]);
	if( ( j==1 ) )
		um_vctovc(pt[1],cir_int_pt[1]);
	else
		um_vctovc(pt[0],cir_int_pt[1]);
	if( ( (*rdim).txt_orent>0 ) )
		{
		if(sign<0.0)
			(*rev_arr) = UU_TRUE;
		(*rdim).entity_site = 1;
			{
			UU_REAL	us_t183[3];
			um_cross(zaxis,vec1,us_t183);
			um_unitvc(us_t183,vnorm);
			}
		if( um_dot(vnorm,yaxis)<0.0 )
			um_vctmsc(vnorm,(UU_REAL)-1.0,vnorm);
		txt_ang = um_angle2p(yaxis,vnorm,zaxis);
		for(i=0;i<(*rdim).txt_blk_use;i++)
			(*rdim).txt_blk[i].tangle = txt_ang;
			{
			UU_REAL	us_t191[3];
			um_vcmnvc(cir_int_pt[0],cir_int_pt[1],us_t191);
			um_unitvc(us_t191,vec1);
			}
		um_cross(zaxis,vec1,vec2);
		um_ilnln(cir_int_pt[0],vec1,flex_pt,vec2,&(num),int_pt);
		dis1 = um_dcccc(flex_pt,end_pt);
			{
			UU_REAL	us_t192[3];
			um_vctmsc(vec1,dis1,us_t192);
			um_vcmnvc(flex_pt,us_t192,end_pt);
			}
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].num_pts = 6;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
		um_vctovc(end_pt,(*rdim).line_blk[n-1].line_seg[0]);
		um_vctovc(flex_pt,(*rdim).line_blk[n-1].line_seg[1]);
		um_vctovc(flex_pt,(*rdim).line_blk[n-1].line_seg[2]);
		um_vctovc(int_pt,(*rdim).line_blk[n-1].line_seg[3]);
		um_vctovc(int_pt,(*rdim).line_blk[n-1].line_seg[4]);
		um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[5]);
		}
	else
		{
		um_ilnpln(center,zaxis,origin,zaxis,&(num),cent_pt);
			{
			UU_REAL	us_t193[3];
			um_vcmnvc(cent_pt,cir_pt,us_t193);
			um_unitvc(us_t193,vec1);
			}
		ua_box_int(corner,cent_pt,vec1,int_pts);
			{
			UU_REAL	us_t194[3];
			um_vcmnvc(cir_int_pt[1],int_pts[0],us_t194);
			um_unitvc(us_t194,vec1);
			}
		um_cross(zaxis,vec1,vec2);
		um_ilnln(cent_pt,vec1,flex_pt,vec2,&(num),int_pt);
		if( ( num==0 ) )
			{
				{
				UU_REAL	us_t195[3];
				um_vcmnvc(cir_pt,int_pts[1],us_t195);
				um_vcmnvc(corner[0],us_t195,int_pt);
				}
			}
		dis1 = um_dcccc(flex_pt,end_pt);
			{
			UU_REAL	us_t196[3];
			um_vctmsc(vec1,dis1,us_t196);
			um_vcplvc(flex_pt,us_t196,end_pt);
			}
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].num_pts = 8;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
		um_vctovc(end_pt,(*rdim).line_blk[n-1].line_seg[0]);
		um_vctovc(flex_pt,(*rdim).line_blk[n-1].line_seg[1]);
		um_vctovc(flex_pt,(*rdim).line_blk[n-1].line_seg[2]);
		um_vctovc(int_pt,(*rdim).line_blk[n-1].line_seg[3]);
		um_vctovc(int_pt,(*rdim).line_blk[n-1].line_seg[4]);
			{
			UU_REAL	us_t197[3];
			um_vcmnvc(int_pts[0],cir_pt,us_t197);
			um_unitvc(us_t197,vec1);
			}
			{
			UU_REAL	us_t198[3];
			um_vcmnvc(int_pt,cir_pt,us_t198);
			um_unitvc(us_t198,vec2);
			}
		if( ( um_dot(vec1,vec2)<-5.000000e-001 ) )
			{
			dis1 = um_dcccc(int_pt,int_pts[0]);
			dis2 = um_dcccc(int_pt,int_pts[1]);
			if( ( dis1<dis2 ) )
				{
				um_vctovc(int_pts[0],(*rdim).line_blk[n-1].line_seg[5]);
				}
			else
				{
				um_vctovc(int_pts[1],(*rdim).line_blk[n-1].line_seg[5]);
				}
			(*rdim).line_blk[n-1].num_pts = 6;
			}
		else
			{
			um_vctovc(int_pts[0],(*rdim).line_blk[n-1].line_seg[5]);
			um_vctovc(int_pts[1],(*rdim).line_blk[n-1].line_seg[6]);
			um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[7]);
			}
		m = ( (*rdim).arrow_blk_use+1 );
		(*rdim).arrow_blk_use = m;
		um_vctovc(cir_pt,(*rdim).arrow_blk[m-1].location);
			{
			UU_REAL	us_t199[3];
			um_vcmnvc((*rdim).line_blk[n-1].line_seg[5],(*rdim).
			    line_blk[n-1].line_seg[4],us_t199);
			(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,us_t199);
			}
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_rad_sym_adjust(i, rdim, dl_pt, corner)
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
ua_rad_sym_adjust(i, rdim, dl_pt, corner)
int		i;
struct UA_generic_draft	(*rdim);
UM_coord	dl_pt, corner[4];
	{
	int		j;
	UM_coord	del_off, c_pt, lab_box[4], mdx, mdy, sdx, sdy, new_pt;

	uu_denter(UU_STRC,(us,"ua_rad_sym_adjust(i=%d)", i));

	ua_text_box(i,&((*rdim)),lab_box);
	um_vcmnvc(lab_box[1],lab_box[0],sdx);
	um_vcmnvc(lab_box[3],lab_box[0],sdy);
		{
		UU_REAL	us_t200[3], us_t201[3], us_t202[3];
		um_vctmsc(sdx,(UU_REAL) 1.0 / 2.000000e+000,us_t201);
		um_vcplvc(lab_box[0],us_t201,us_t200);
		um_vctmsc(sdy,(UU_REAL) 1.0 / 2.000000e+000,us_t202);
		um_vcplvc(us_t200,us_t202,c_pt);
		}
	um_vcmnvc(corner[1],corner[0],mdx);
	um_vcmnvc(corner[3],corner[0],mdy);
	switch( (*rdim).rad_place )
		{
		case 0:
			{
				{
				UU_REAL	us_t203[3], us_t204[3], us_t205[3];
				um_vctmsc(mdy,(UU_REAL) 5.000000e-001,us_t204);
				um_vcplvc(dl_pt,us_t204,us_t203);
				um_vctmsc(sdy,(UU_REAL) 6.000000e-001,us_t205);
				um_vcplvc(us_t203,us_t205,new_pt);
				}
			}
			break;
		case 1:
			{
				{
				UU_REAL	us_t206[3], us_t207[3], us_t208[3];
				um_vctmsc(mdy,(UU_REAL) 5.000000e-001,us_t207);
				um_vcmnvc(dl_pt,us_t207,us_t206);
				um_vctmsc(sdy,(UU_REAL) 6.000000e-001,us_t208);
				um_vcmnvc(us_t206,us_t208,new_pt);
				}
			}
			break;
		case 2:
			{
				{
				UU_REAL	us_t209[3], us_t210[3];
				um_vctmsc(mdx,(UU_REAL) 5.000000e-001,us_t210);
				um_vcmnvc(dl_pt,us_t210,us_t209);
				um_vcmnvc(us_t209,sdx,new_pt);
				}
			}
			break;
		case 3:
			{
				{
				UU_REAL	us_t211[3], us_t212[3], us_t213[3];
				um_vctmsc(mdx,(UU_REAL) 5.000000e-001,us_t212);
				um_vcplvc(dl_pt,us_t212,us_t211);
				um_vctmsc(sdx,(UU_REAL) 6.000000e-001,us_t213);
				um_vcplvc(us_t211,us_t213,new_pt);
				}
			}
			break;
		}
	um_vcmnvc(new_pt,c_pt,del_off);
	um_vcplvc((*rdim).txt_blk[i-1].origin,del_off,(*rdim).
	    txt_blk[i-1].origin);
	j = 1;
	for(;;)
		{
		if( j > 4 ) 	break;
		um_vcplvc(lab_box[j-1],del_off,lab_box[j-1]);
us_l214:
		j++ ;
		}
us_l215: 
	;
	switch( (*rdim).rad_place )
		{
		case 0:
			{
			corner[2][1] = lab_box[2][1];
			corner[3][1] = lab_box[3][1];
			}
			break;
		case 1:
			{
			corner[0][1] = lab_box[0][1];
			corner[1][1] = lab_box[1][1];
			}
			break;
		case 2:
			{
			corner[0][0] = lab_box[0][0];
			corner[3][0] = lab_box[3][0];
			}
			break;
		case 3:
			{
			corner[1][0] = lab_box[1][0];
			corner[2][0] = lab_box[2][0];
			}
			break;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_radius_regen(rdim)
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
ua_radius_regen(rdim)
struct UA_generic_draft	(*rdim);
	{
	int		relation, curr_key, type, status;
	UM_coord	c_spt, center, normal, c_ept;
	UU_REAL	dtheta, radius, dummy;
	UU_LOGICAL	mode;

	uu_denter(UU_STRC,(us,"ua_radius_regen(rdim=%s)", "..."));

	curr_key = (*rdim).asso_blk[0].key;
	status = um_retrieve_data_relnum(curr_key,&(relation));
	if( ( relation==48 ) )
		{
		ua_centerline_arc(curr_key,center,&(radius),&(dtheta),normal
		    ,c_spt,c_ept,&(dummy));
		}
	else
		{
		uc_draft_arc(curr_key,center,&(radius),&(dtheta),normal,
		c_spt,c_ept,&(dummy));
		}
	type = (*rdim).subtype;
	mode = UU_FALSE;
	status = ua_radius_cre(type,mode,&((*rdim)),center,radius,
	dtheta,normal,c_spt,c_ept);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_rad_outside(rdim, corner, center, normal, 
**				radius, spt, ept, dtheta, pt1, cir_pt, dir)
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
ua_rad_outside(rdim, corner, center, normal, 
		radius, spt, ept, dtheta, pt1, cir_pt, dir)
struct UA_generic_draft	(*rdim);
UM_coord	corner[4], center, normal, pt1, cir_pt, spt, ept;
UU_REAL	radius, dtheta;
int		*dir;
	{
	int		num;
	int		i,j,k,l,m,n;
	UM_coord	tst_pt[2],temp,pt2, off_s_pt[2], origin, cent_pt, pt[4],
			vec1, vec2, edge1_pt, edge2_pt, xaxis, yaxis, zaxis;
	UU_REAL	vdot, dis1, dis2;

	uu_denter(UU_STRC,(us,"ua_rad_outside(radius=%g,dtheta=%g,dir=%d)",
		radius,dtheta,*dir));

	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
	if ((*rdim).txt_orent == 0)
		{
		UU_REAL	us_t216[3], us_t217[3];
		um_vcmnvc(corner[3],corner[0],us_t217);
		um_vctmsc(us_t217,(UU_REAL) 1.0 / 2.0,us_t216);
		um_vcplvc(corner[0],us_t216,edge1_pt);
		um_vcmnvc(corner[1],corner[0],vec2);
		um_vcplvc(edge1_pt,vec2,edge2_pt);
		um_unitvc(vec2,vec2);
		for(i=1;i<=2;i++)
			{
			switch( i )
				{
				case 1:
					{
					UU_REAL	us_t220[3];
					um_vctmsc(vec2,(*rdim).stub_length,us_t220);
					um_vcmnvc(edge1_pt,us_t220,off_s_pt[i-1]);
					}
					break;
				case 2:
					{
					UU_REAL	us_t221[3];
					um_vctmsc(vec2,(*rdim).stub_length,us_t221);
					um_vcplvc(edge2_pt,us_t221,off_s_pt[i-1]);
					}
					break;
				}
				um_vctovc(off_s_pt[i-1],temp);
			um_ilnpln(temp,zaxis,center,normal,&(num),pt2);
			um_vctovc(pt2,tst_pt[i-1]);
				{
				UU_REAL	us_t222[3];
				um_vcmnvc(center,tst_pt[i-1],us_t222);
				um_unitvc(us_t222,vec1);
				}
			um_ilncir(center,vec1,center,normal,radius,&(num),pt);
			if ( num>1 )
				{
				j = us_clospnt(2,pt,tst_pt[i-1]);
				um_vctovc(pt[j-1],tst_pt[i-1]);
				}
			else
				um_vctovc(pt[0],tst_pt[i-1]);
			}
		for(i=1;i<=2;i++)
			{
			um_vctovc(tst_pt[i-1],temp);
			um_ilnpln(temp,zaxis,origin,zaxis,&(num),pt2);
			um_vctovc(pt2,tst_pt[i-1]);
			}
		dis1 = um_dcccc(off_s_pt[0],tst_pt[0]);
		dis2 = um_dcccc(off_s_pt[1],tst_pt[1]);
		if( ( dis1<dis2 ) )
			{
			um_vctovc(edge1_pt,pt1);
			um_vctovc(tst_pt[0],cir_pt);
			(*dir) = 3;
			}
		else
			{
			um_vctovc(edge2_pt,pt1);
			um_vctovc(tst_pt[1],cir_pt);
			(*dir) = 4;
			}
		if( ( (! um_cceqcc( spt, ept ) )&&( fabs(( fabs(dtheta)-
		    3.141593))>0.01 ) ) )
			{
			um_ilnpln(cir_pt,zaxis,center,normal,&(num),pt2);
			um_vcmnvc(spt,pt2,vec1);
			um_vcmnvc(ept,pt2,vec2);
			vdot = um_dot(vec1,vec2);
			if( ( ( ( vdot>0.0)&&( fabs(dtheta)<3.141593
			    ) )||( ( vdot<0.0)&&( fabs(dtheta)>3.141593) ) ) )
				{
				um_ilnpln(pt1,zaxis,center,normal,&(num),pt2);
					{
					UU_REAL	us_t225[3];
					um_vcmnvc(pt2,center,us_t225);
					um_unitvc(us_t225,vec1);
					}
				um_ilncir(center,vec1,center,normal,radius,&(num),pt);
				if( ( num>1 ) )
					{
					for(i=0;i<2;i++)
						um_ilnpln(pt[i],zaxis,origin,zaxis,&(num),tst_pt[i]);
					dis1 = um_dcccc(pt1,tst_pt[0]);
					dis2 = um_dcccc(pt1,tst_pt[1]);
					if( ( dis1<dis2 ) )
						um_vctovc(tst_pt[1],cir_pt);
					else
						um_vctovc(tst_pt[0],cir_pt);
					}
				}
			}
		}
	else
		{
		UU_REAL	us_t237[3];
		UU_REAL	us_t238[3];
		UU_REAL hold_mag;
		UU_REAL	rad_line[3];
		UU_REAL rad_line_angle;
		um_vcmnvc(corner[1],corner[2],us_t238);
		hold_mag = um_mag(us_t238);	
		for(i=0;i<3;i++)
			us_t237[i] = us_t238[i] / hold_mag;
		hold_mag = hold_mag + (*rdim).txt_gap;
		um_vctmsc(us_t237,hold_mag,us_t237);
		um_vcmnvc((*rdim).dim_origin,center,rad_line);
		rad_line_angle = um_angle(xaxis,rad_line);
		switch ((*rdim).txt_orent)
			{
			case 3:
				if ((rad_line_angle > 1.570796)
				    &&(rad_line_angle < 4.712389))
					um_vcplvc(corner[0],us_t237,vec1);
				else
					um_vcplvc(corner[1],us_t237,vec1);
				break;
			default:
				if ((rad_line_angle < 1.570796)
				    ||(rad_line_angle > 4.712389))
					um_vcplvc(corner[2],us_t237,vec1);
				else
					um_vcplvc(corner[3],us_t237,vec1);
				break;
			}
		um_vcmnvc(vec1,center,vec1);
		um_unitvc(vec1,vec1);
		um_ilncir(center,vec1,center,normal,radius,&(num),pt);
		if (num>1)
			{
			j = us_clospnt(2,pt,corner[0]);
			um_vctovc(pt[j-1],cir_pt);
			}
		else
			um_vctovc(pt[0],cir_pt);
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_rad_ext(rdim, cir_pt, center, normal, radius, 
**										dtheta, c_spt, c_ept)
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
ua_rad_ext(rdim, cir_pt, center, normal, radius, 
							dtheta, c_spt, c_ept)
struct UA_generic_draft	(*rdim);
UM_coord	cir_pt, 	center, 	normal, 	c_spt, 	c_ept;
UU_REAL	dtheta, radius;
	{
	int		i;
	int		num;
	int		j;
	int		k;
	int		l;
	int		m;
	int		n;
	UM_coord	cc_cir_pt, temp, cent_pt, origin, vec1, vec2, clo_pt, xaxis,
				yaxis, zaxis;
	UU_REAL	vdot, d1, d2, arc_len, del_tha1, del_tha2, e_ang, cc_radius, s_ang;

	uu_denter(UU_STRC,(us,"ua_rad_ext(radius=%g, dtheta=%g)",
		radius, dtheta));

	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
	um_ilnpln(cir_pt,zaxis,center,normal,&(num),cc_cir_pt);
	um_vcmnvc(c_spt,cc_cir_pt,vec1);
	um_vcmnvc(c_ept,cc_cir_pt,vec2);
	vdot = um_dot(vec1,vec2);
	if( ( ( ( vdot>0.000000e+000 )&&( fabs(dtheta)<3.141593e+000
	    ) )||( ( vdot<0.000000e+000 )&&( fabs(dtheta)>3.141593e+000
	    ) ) ) )
		{
		d1 = um_dcccc(cc_cir_pt,c_spt);
		d2 = um_dcccc(cc_cir_pt,c_ept);
		if( ( d1<d2 ) )
			{
			um_vctovc(c_spt,clo_pt);
			}
		else
			{
			um_vctovc(c_ept,clo_pt);
			}
		del_tha1 = ( (*rdim).gap_to_geo/radius );
		del_tha2 = ( (*rdim).ext_past_line/radius );
		um_ilnpln(center,zaxis,origin,zaxis,&(num),cent_pt);
		um_ilnpln(clo_pt,zaxis,origin,zaxis,&(num),temp);
			{
			UU_REAL	us_t228[3];
			um_vcmnvc(temp,cent_pt,us_t228);
			s_ang = ua_dir_angle(zaxis,xaxis,us_t228);
			}
			{
			UU_REAL	us_t229[3];
			um_vcmnvc(cir_pt,cent_pt,us_t229);
			e_ang = ua_dir_angle(zaxis,xaxis,us_t229);
			}
		if( ( ( s_ang>e_ang )||( ( s_ang<1.570796e+000 )&&( e_ang>
		    3.141593e+000 ) ) ) )
			{
			s_ang = ( s_ang-del_tha1 );
			e_ang = ( e_ang-del_tha2 );
			d1 = s_ang;
			s_ang = e_ang;
			e_ang = d1;
			}
		else
			{
			s_ang = ( s_ang+del_tha1 );
			e_ang = ( e_ang+del_tha2 );
			}
		m = ( (*rdim).arc_blk_use+1 );
		(*rdim).arc_blk_use = m;
		(*rdim).arc_blk[m-1].subtype = ext_arc;
		(*rdim).arc_blk[0].arc.line_font = UA_ext_line_font;
		(*rdim).arc_blk[0].arc.line_density = UA_ext_line_dens;
		(*rdim).arc_blk[0].arc.color = UA_ext_line_color;
		(*rdim).arc_blk[m-1].num_pts = 2;
		um_vctovc(cent_pt,(*rdim).arc_blk[m-1].center_pt);
		(*rdim).arc_blk[m-1].radius = um_dcccc(cir_pt,cent_pt);
		(*rdim).arc_blk[m-1].angles[0] = s_ang;
		(*rdim).arc_blk[m-1].angles[1] = e_ang;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_radius(type)
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
ua_radius(type)
int		type;
	{
	int	relation, dir, ablock, num_arcs, dim_type, num, curr_key, prev_key,
			subtype, ok, s_status, prev_txt_blk, i, j, d_stat, k, l, m, n;
	UM_coord	c_spt, 	pt1, 	cpln_origin, 	center, 	corner[4], 	normal,
						cir_pt, 	c_ept, 	xaxis, 	yaxis, 	zaxis;
	UU_REAL	dtheta, radius, dummy;
	UU_LOGICAL	drw_leader, rev_arr, status, first, mode, redo;
	struct UA_PLOCREC	plocrec;
	struct UA_generic_draft	rdim;

	uu_denter(UU_STRC,(us,"ua_radius(type=%d)", type));

	subtype = type;
	curr_key = -1;
	us_init_aradius();
	us_init_autility();
	first = UU_TRUE;
	ua_dump_set(0xffff);
main_loop:
	ua_init_entity(64,subtype,&(rdim));
	ua_getcpln(&(rdim),cpln_origin,xaxis,yaxis,zaxis);
	rdim.txt_just = 0;
	rdim.entity_site = 0;
	rev_arr = UU_FALSE;
	drw_leader = UU_TRUE;
	redo = UU_FALSE;
	ablock = ( rdim.asso_blk_use+1 );
get:
	ud_lgeo(UU_TRUE,UD_draftable);
	s_status = ua_select_ent_subf(34,&(rdim),ablock,&(plocrec));
	switch( s_status )
		{
		case 0:
			{
			uu_dexit;
			return;
			}
		case 2:
			{
			if( ( first==UU_FALSE ) )
				{
				redo = UU_TRUE;
				rdim.key = prev_key;
				s_status = uc_retrieve_data(&(rdim),sizeof(struct 
				    UA_generic_draft	));
				if( ( s_status==0 ) )
					{
					rdim.arc_blk_use = 0;
					rdim.line_blk_use = 0;
					rdim.arrow_blk_use = 0;
					rdim.asso_blk_use = 1;
					rdim.txt_blk_use = prev_txt_blk;
					j = 1;
					for(;;)
						{
						if( j > 10 ) 	break;
						rdim.txt_blk[j-1].tangle = UA_text_ang;
us_l230:
						j++ ;
						}
us_l231: 
					;
					curr_key = rdim.asso_blk[0].key;
					}
				else
					{
					redo = UU_FALSE;
					first = UU_TRUE;
					goto main_loop;
					}
				}
			else
				{
				uu_dexit;
				return;
				}
			}
			break;
		case 1:
			{
			curr_key = rdim.asso_blk[ablock-1].key;
			s_status = um_retrieve_data_relnum(curr_key,&(relation));
			if( ( relation==48 ) )
				{
				rdim.key = curr_key;
				uc_retrieve_data(&(rdim),sizeof(struct UA_generic_draft	))
					;
				dim_type = rdim.etype;
				num_arcs = rdim.arc_blk_use;
				ua_init_entity(61,subtype,&(rdim));
				rdim.txt_just = 0;
				rdim.entity_site = 0;
				if( ( dim_type==54 ) )
					{
					if( ( num_arcs==0 ) )
						{
						goto get;
						}
					}
				}
			else
				{
				ok = uc_draft_type(curr_key,&(relation));
				if( ( relation!=2 ) )
					{
					goto get;
					}
				}
			rdim.asso_blk_use = ablock;
			}
			break;
		}
	s_status = um_retrieve_data_relnum(curr_key,&(relation));
	if( ( relation==48 ) )
		{
		ua_centerline_arc(curr_key,center,&(radius),&(dtheta),normal
		    ,c_spt,c_ept,&(dummy));
		}
	else
		{
		uc_draft_arc(curr_key,center,&(radius),&(dtheta),normal,
		c_spt,c_ept,&(dummy));
		}
	if( ( fabs(um_dot(normal,zaxis))<1.000000e-004 ) )
		{
		uu_uerror0(13,17);
		goto get;
		}
	status = ua_dia_org(&(rdim),redo);
	/* to keep the appended text. kathy */
	prev_txt_blk = rdim.txt_blk_use;
	if( ( !status ) )
		{
		uu_dexit;
		return;
		}
	mode = UU_TRUE;
	um_vctovc(center, rdim.asso_blk[ablock-1].location);
	s_status = ua_radius_cre(type,mode,&(rdim),center,radius,
														dtheta,normal,c_spt,c_ept);
	if( ( s_status==0 ) )
		{
		goto main_loop;
		}
	if( ( redo==UU_TRUE ) )
		{
		s_status = ua_update_entity(prev_key,&(rdim));
		if( ( s_status!=0 ) )
			{
			ua_create_entity(&(rdim),&(curr_key));
			}
		}
	else
		{
		ua_create_entity(&(rdim),&(curr_key));
		}
	uc_display(&(rdim));
	prev_key = rdim.key;
	/*prev_txt_blk = rdim.txt_blk_use;*/
	first = UU_FALSE;
	goto main_loop;
	}
/*********************************************************************
**    E_FUNCTION     : ua_rad_center(rdim, corner, center, normal, radius,
**			pt1, cir_pt, dir, rev_arr, drw_leader)
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
ua_rad_center(rdim, corner, center, normal, radius,
	pt1, cir_pt, dir, rev_arr, drw_leader)
struct UA_generic_draft	(*rdim);
UU_REAL	corner[4][3];
UU_REAL	center[3];
UU_REAL	normal[3];
UU_REAL	radius;
UU_REAL	pt1[3];
UU_REAL	cir_pt[3];
int		(*dir);
UU_LOGICAL	(*rev_arr);
UU_LOGICAL	(*drw_leader);
	{
	UU_REAL	sign;
	UU_REAL	tst_pt[2][3];
	UU_REAL	temp[3];
	UU_REAL	pt2[3];
	UU_REAL	corn_mod[4][3];
	int		num;
	UU_REAL	off_s_pt[2][3];
	UU_REAL	origin[3];
	UU_REAL	cent_pt[3];
	UU_REAL	txt_ang;
	UU_REAL	pt[4][3];
	UU_REAL	vec1[3];
	UU_REAL	vec2[3];
	UU_REAL	dis1;
	UU_REAL	dis2;
	UU_REAL	int_pts[2][3];
	int		i;
	int		j;
	int		k;
	UU_REAL	ref_pt[3];
	int		l;
	int		m;
	int		n;
	UU_REAL	edge1_pt[3];
	UU_REAL	edge2_pt[3];
	UU_REAL	int_pt[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL	vnorm[3];
	UU_REAL	dl_pt[3];

	uu_denter(UU_STRC,(us,"ua_rad_center(center=<%g,%g,%g>,radius=%g)",
		center[0],center[1],center[2],radius));

	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
		{
		UU_REAL	us_t232[3];
		UU_REAL	us_t233[3];
		UU_REAL	us_t234[3];
		UU_REAL	us_t235[3];
		UU_REAL	us_t236[3];
		um_vcmnvc(corner[3],corner[0],us_t234);
		um_vctmsc(us_t234,(UU_REAL) 1.0 / 2.0,us_t233);
		um_vcplvc(corner[0],us_t233,us_t232);
		um_vcmnvc(corner[1],corner[0],us_t236);
		um_vctmsc(us_t236,(UU_REAL) 1.0 / 2.0,us_t235);
		um_vcplvc(us_t232,us_t235,cent_pt);
		}
	if( ( (*rdim).txt_orent==0 ) )
		um_vctovc(cent_pt,ref_pt);
	else
		{
		UU_REAL	us_t237[3];
		UU_REAL	us_t238[3];
		UU_REAL hold_mag;
		UU_REAL	rad_line[3];
		UU_REAL rad_line_angle;
		um_vcmnvc(corner[1],corner[2],us_t238);
		hold_mag = um_mag(us_t238);	
		for(i=0;i<3;i++)
			us_t237[i] = us_t238[i] / hold_mag;
		hold_mag = hold_mag + (*rdim).txt_gap;
		um_vctmsc(us_t237,hold_mag,us_t237);
		um_vcmnvc((*rdim).dim_origin,center,rad_line);
		rad_line_angle = um_angle(xaxis,rad_line);
		switch ((*rdim).txt_orent)
			{
			case 3:
				if ((rad_line_angle > 1.570796)
				    &&(rad_line_angle < 4.712389))
					um_vcplvc(corner[0],us_t237,ref_pt);
				else
					um_vcplvc(corner[1],us_t237,ref_pt);
				break;
			default:
				if ((rad_line_angle < 1.570796)
				    ||(rad_line_angle > 4.712389))
					um_vcplvc(corner[2],us_t237,ref_pt);
				else
					um_vcplvc(corner[3],us_t237,ref_pt);
				break;
			}
		}
	um_ilnpln(ref_pt,zaxis,center,normal,&(num),pt1);
		{
		UU_REAL	us_t239[3];
		um_vcmnvc(center,pt1,us_t239);
		um_unitvc(us_t239,vec1);
		}
	um_ilncir(center,vec1,center,normal,radius,&(num),pt);
	if( ( num>1 ) )
		{
		j = us_clospnt(2,pt,pt1);
		um_vctovc(pt[j-1],cir_pt);
		}
	else
		{
		um_vctovc(pt[0],cir_pt);
		}
		{
		UU_REAL	us_t240[3];
		um_vcmnvc(cir_pt,pt1,us_t240);
		um_unitvc(us_t240,vec2);
		}
	sign = um_dot(vec1,vec2);
	um_vctovc(cir_pt,temp);
	um_ilnpln(temp,zaxis,origin,zaxis,&(num),cir_pt);
	if( ( sign<0.0 ) )
		{
		if( ( (*rdim).txt_orent>0 ) )
			{
			(*rdim).entity_site = 1;
				{
				UU_REAL	us_t241[3];
				um_cross(zaxis,vec1,us_t241);
				um_unitvc(us_t241,vnorm);
				}
			if( ( um_dot(vnorm,yaxis)<0.0 ) )
				{
				um_vctmsc(vnorm,(UU_REAL)-1.0,vnorm);
				}
			txt_ang = um_angle2p(yaxis,vnorm,zaxis);
			for(i=1;i<=(*rdim).txt_blk_use;i++)
				(*rdim).txt_blk[i-1].tangle = txt_ang;
			n = ( (*rdim).line_blk_use+1 );
			(*rdim).line_blk_use = n;
			(*rdim).line_blk[n-1].num_pts = 2;
			(*rdim).line_blk[n-1].subtype = dim_line;
			(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
			(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
			(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
			um_vctovc(center,(*rdim).line_blk[n-1].line_seg[0]);
			um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[1]);
			}
		else
			{
			um_ilnpln(center,zaxis,origin,zaxis,&(num),cent_pt);
				{
				UU_REAL	us_t249[3];
				um_vcmnvc(cent_pt,cir_pt,us_t249);
				um_unitvc(us_t249,vec1);
				}
			ua_box_int(corner,cent_pt,vec1,int_pts);
			n = ( (*rdim).line_blk_use+1 );
			(*rdim).line_blk_use = n;
			(*rdim).line_blk[n-1].num_pts = 4;
			(*rdim).line_blk[n-1].subtype = dim_line;
			(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
			(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
			(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
				{
				UU_REAL	us_t250[3];
				um_vcmnvc(int_pts[0],cir_pt,us_t250);
				um_unitvc(us_t250,vec1);
				}
				{
				UU_REAL	us_t251[3];
				um_vcmnvc(center,cir_pt,us_t251);
				um_unitvc(us_t251,vec2);
				}
			if( ( um_dot(vec1,vec2)<-5.000000e-001 ) )
				{
				um_vctovc(center,(*rdim).line_blk[n-1].line_seg[0]);
				dis1 = um_dcccc(int_pt,int_pts[0]);
				dis2 = um_dcccc(int_pt,int_pts[1]);
				if( ( dis1<dis2 ) )
					um_vctovc(int_pts[0],(*rdim).line_blk[n-1].line_seg[1]);
				else
					um_vctovc(int_pts[1],(*rdim).line_blk[n-1].line_seg[1]);
				(*rdim).line_blk[n-1].num_pts = 2;
				}
			else
				{
				um_vctovc(center,(*rdim).line_blk[n-1].line_seg[0]);
				um_vctovc(int_pts[0],(*rdim).line_blk[n-1].line_seg[1]);
				um_vctovc(int_pts[1],(*rdim).line_blk[n-1].line_seg[2]);
				um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[3]);
				}
			}
		ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
		m = ( (*rdim).arrow_blk_use+1 );
		(*rdim).arrow_blk_use = m;
		um_vctovc(cir_pt,(*rdim).arrow_blk[m-1].location);
			{
			UU_REAL	us_t252[3];
			um_vcmnvc((*rdim).line_blk[n-1].line_seg[1],(*rdim).
			    line_blk[n-1].line_seg[0],us_t252);
			(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,
			us_t252);
			}
		(*drw_leader) = UU_FALSE;
		}
	else
		{
		if( ( (*rdim).txt_orent==0 ) )
			{
			UU_REAL	us_t253[3];
			UU_REAL	us_t254[3];
			um_vcmnvc(corner[3],corner[0],us_t254);
			um_vctmsc(us_t254,(UU_REAL) 1.0 / 2.0,us_t253);
			um_vcplvc(corner[0],us_t253,edge1_pt);
			um_vcmnvc(corner[1],corner[0],vec2);
			um_vcplvc(edge1_pt,vec2,edge2_pt);
			um_unitvc(vec2,vec2);
			for(i=1;i<=2;i++)
				{
				switch( i )
					{
					case 1: {
						UU_REAL	us_t257[3];
						um_vctmsc(vec2,(*rdim).stub_length,us_t257);
						um_vcmnvc(edge1_pt,us_t257,off_s_pt[i-1]);
						break;
						}
					case 2:	{
						UU_REAL	us_t258[3];
						um_vctmsc(vec2,(*rdim).stub_length,us_t258);
						um_vcplvc(edge2_pt,us_t258,off_s_pt[i-1]);
						break;
						}
					}
				um_vctovc(off_s_pt[i-1],temp);
				um_ilnpln(temp,zaxis,center,normal,&(num),pt2);
				um_vctovc(pt2,tst_pt[i-1]);
					{
					UU_REAL	us_t259[3];
					um_vcmnvc(center,tst_pt[i-1],us_t259);
					um_unitvc(us_t259,vec1);
					}
				um_ilncir(center,vec1,center,normal,radius,&(num),pt);
				if( ( num>1 ) )
					{
					j = us_clospnt(2,pt,tst_pt[i-1]);
					um_vctovc(pt[j-1],tst_pt[i-1]);
					}
				else
					um_vctovc(pt[0],tst_pt[i-1]);
				}
			for(i=0;i<2;i++)
				{
				um_vctovc(tst_pt[i],temp);
				um_ilnpln(temp,zaxis,origin,zaxis,&(num),pt2);
				um_vctovc(pt2,tst_pt[i]);
				}
			dis1 = um_dcccc(off_s_pt[0],tst_pt[0]);
			dis2 = um_dcccc(off_s_pt[1],tst_pt[1]);
			if( ( dis1<dis2 ) )
				{
				um_vctovc(edge1_pt,pt1);
				um_vctovc(tst_pt[0],cir_pt);
				(*dir) = 3;
				}
			else
				{
				um_vctovc(edge2_pt,pt1);
				um_vctovc(tst_pt[1],cir_pt);
				(*dir) = 4;
				}
			}
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].num_pts = 2;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
		um_vctovc(center,(*rdim).line_blk[n-1].line_seg[0]);
		um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[1]);
		(*rev_arr) = UU_TRUE;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_rad_txt_ang(rdim, center)
**      This function finds the text angle for radius dimension text
**	that is not horizontal so that it can be placed above or below
**	a dimension line that is not horizontal. 
**    PARAMETERS   
**       INPUT  : rdim		-generic drafitng entity 
**		  center	center of arc or circle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_rad_txt_ang(rdim,center)
struct UA_generic_draft	(*rdim);
UM_coord	center;
	{
	UM_coord	xaxis;
	UM_coord	zaxis;
	UM_coord	rad_line;
	UU_REAL		rad_line_angle;
	int		i;
	uu_denter(UU_STRC,(us,"ua_rad_txt_ang(center=<%g,%g,%g>)",
		center[0],center[1],center[2]));
	xaxis[0] = 1.0;
	xaxis[1] = 0.0;
	xaxis[2] = 0.0;
	zaxis[0] = 0.0;
	zaxis[1] = 0.0;
	zaxis[2] = 1.0;
	um_vcmnvc((*rdim).dim_origin,center,rad_line);
	rad_line_angle = ua_dir_angle(zaxis,xaxis,rad_line);
	switch ((*rdim).txt_orent)
		{
		case 3:
			if ((rad_line_angle > 1.570796)&&(rad_line_angle < 4.712389))
				{
				rad_line_angle += ((*rdim).txt_gap / um_mag(rad_line));
				rad_line_angle += 3.14159;
				}
			else
				rad_line_angle -= ((*rdim).txt_gap / um_mag(rad_line));
			break;
		default:
			if ((rad_line_angle > 1.570796)&&(rad_line_angle < 4.712389))
				{
				rad_line_angle += (((*rdim).txt_gap + (*rdim).char_size)
					/ um_mag(rad_line));
				rad_line_angle += 3.14159;
				}
			else
				rad_line_angle -= (((*rdim).txt_gap + (*rdim).char_size)
					/ um_mag(rad_line));
		}
	for (i=0; i<(*rdim).txt_blk_use; i++)
		(*rdim).txt_blk[i].tangle = rad_line_angle;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_crea_long_leader(rdim, corner, center, cir_pt)
**	This function draws the extension line for radius dimension
**	when the text is above or below the dimension line.
**    PARAMETERS   
**       INPUT  : rdim		-generic drafitng entity 
**		  corner	corners of box around the dimension text
**		  center	point at the center of the arc of the radius
**		  cir_pt	point on circle to start leader
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_crea_long_leader(rdim,corner,center,cir_pt)
struct UA_generic_draft	(*rdim);
UU_REAL	corner[4][3];
UU_REAL	center[3];
UU_REAL	cir_pt[3];

	{
	UU_REAL	us_t237[3];
	UU_REAL	us_t238[3];
	UU_REAL	rad_line[3];
	UU_REAL rad_line_angle;	
	UU_REAL hold_mag;
	UM_coord	ref_pt,tmp;
	int		i,n,m;
	UM_coord	xaxis,zaxis;
	uu_denter(UU_STRC,(us,"ua_crea_long_leader(cir_pt=<%g,%g,%g>)",
		cir_pt[0],cir_pt[1],cir_pt[2]));
	xaxis[0] = 1.0;
	xaxis[1] = 0.0;
	xaxis[2] = 0.0;
	zaxis[0] = 0.0;
	zaxis[1] = 0.0;
	zaxis[2] = 1.0;
	um_vcmnvc(corner[1],corner[2],us_t238);
	hold_mag = um_mag(us_t238);	
	for(i=0;i<3;i++)
		us_t237[i] = us_t238[i] / hold_mag;
	hold_mag = hold_mag + (*rdim).txt_gap;
	um_vctmsc(us_t237,hold_mag,us_t237);
	um_vcmnvc((*rdim).dim_origin,center,rad_line);
	rad_line_angle = um_angle(xaxis,rad_line);
	switch ((*rdim).txt_orent)
		{
		case 3:
			if ((rad_line_angle > 1.570796)
			    &&(rad_line_angle < 4.712389))
				um_vcplvc(corner[0],us_t237,ref_pt);
			else
				um_vcplvc(corner[1],us_t237,ref_pt);
			break;
		default:
			if ((rad_line_angle < 1.570796)
			    ||(rad_line_angle > 4.712389))
				um_vcplvc(corner[2],us_t237,ref_pt);
			else
				um_vcplvc(corner[3],us_t237,ref_pt);
			break;
		}
	n = ( (*rdim).line_blk_use+1 );
	(*rdim).line_blk_use = n;
	(*rdim).line_blk[n-1].num_pts = 2;
	(*rdim).line_blk[n-1].subtype = dim_line;
	(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
	(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
	(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
	um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[0]);
	um_vctovc(ref_pt,(*rdim).line_blk[n-1].line_seg[1]);
	m = (*rdim).arrow_blk_use + 1;
	(*rdim).arrow_blk_use = m;
	um_vctovc(cir_pt,(*rdim).arrow_blk[m-1].location);
	um_vcmnvc((*rdim).line_blk[n-1].line_seg[1],(*rdim).line_blk[n-1].line_seg[0],tmp);
	(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,tmp);
	uu_dexit;
	}
