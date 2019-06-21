/*********************************************************************
**    NAME         : adiam.c
**       CONTAINS:
**					ua_diam_cre
**					ua_diam_regen
**					ua_diam
**             ua_dia_txt
**             ua_dia_org
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       adiam.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:32
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) adiam.c 25.1 04/29/15 15:05:32 single"};
#else
static char uu_sccsident[]={"@(#) adiam.c 25.1 04/29/15 15:05:32 double"};
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
#include "dasnog.h"
#include "mdcoord.h"

extern int UD_draftable[UD_NMENTWD];
extern int UD_draft_line[UD_NMENTWD];
struct UA_generic_draft	test_dim;

us_init_adiam()
{
}
UU_LOGICAL	ua_dia_org();

/*********************************************************************
**    E_FUNCTION     : ua_diam(type)
**       Main function to handle diameter dimension.
**    PARAMETERS   
**       INPUT  : 
**				type						form of the dimension
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_diam(type)
int		type;
	{
	struct UA_PLOCREC	plocrec;
	struct UA_generic_draft	rdim;
	int		relation;
	int		dir;
	int		ablock;
	int		num_arcs;
	int		dim_type;
	int		num;
	int		curr_key;
	int		prev_key;
	int		subtype;
	int		ok;
	int		s_status;
	int		prev_txt_blk;
	int		i;
	int		j;
	int		d_stat;
	int		k;
	int		l;
	int		m;
	int		n;
	UU_REAL	radius;
	UU_REAL	corner[4][3];
	UU_REAL	normal[3];
	UU_REAL	c_spt[3];
	UU_REAL	pt1[3];
	UU_REAL	cpln_origin[3];
	UU_REAL	center[3];
	UU_REAL	cir_pt[3];
	UU_REAL	dang;
	UU_REAL	c_ept[3];
	UU_REAL	dummy;
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_LOGICAL	redo;
	UU_LOGICAL	status;
	UU_LOGICAL	drw_leader;
	UU_LOGICAL	rev_arr;
	UU_LOGICAL	first;

	uu_denter(UU_STRC,(us,"SAL ua_diam(type=%d)", type));

	ua_dump_set(0xffff);
	if( ( type==7 ) )
		{
		ua_dia_shaft();
		uu_dexit;
		return;
		}
	if( ( type==8 ) )
		{
		ua_dia_cyln();
		uu_dexit;
		return;
		}
/* type 9 is repetitive diameter dimensioning */
	if( ( type==9 ) )
		{
		ua_dia_rep_dim(type);
		uu_dexit;
		return;
		}
/* type 10 is repetitive radial dimensioning */
	if( ( type==10 ) )
		{
		ua_dia_rep_dim(type);
		uu_dexit;
		return;
		}
	subtype = type;
	curr_key = -1;
	us_init_adiam();
	us_init_autility();
	first = UU_TRUE;
main_loop:
	ua_init_entity(UA_DIA_IN_DIM,subtype,&(rdim));
	ua_getcpln(&(rdim),cpln_origin,xaxis,yaxis,zaxis);
	rdim.txt_just = UA_LEFT;
	rdim.entity_site = UA_TOP_LEFT;
	rev_arr = UU_FALSE;
	drw_leader = UU_TRUE;
	redo = UU_FALSE;
	ablock = ( rdim.asso_blk_use+1 );
get:
	ud_lgeo(UU_TRUE,UD_draftable);
	s_status = ua_select_ent_subf(34,&(rdim),ablock,&(plocrec));
	switch( s_status )
		{
		case UA_REJECT:
			{
			uu_dexit;
			return;
			}
		case UA_ALT_ACTION:
			{
			if( ( first==UU_FALSE ) )
				{
				redo = UU_TRUE;
				rdim.key = prev_key;
				s_status = uc_retrieve_data(&(rdim),
									sizeof(struct UA_generic_draft	));
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
						if( j > UA_NUM_TXTBLKS ) 	break;
						rdim.txt_blk[j-1].tangle = UA_text_ang;
us_l169:
						j++ ;
						}
us_l170: 
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
		case UA_OPCOMPLETE:
			{
			curr_key = rdim.asso_blk[ablock-1].key;
			s_status = um_retrieve_data_relnum(curr_key,&(relation));
			if( ( relation==UA_LINEAR_DIM ) )
				{
				rdim.key = curr_key;
				uc_retrieve_data(&(rdim),sizeof(struct UA_generic_draft	)) ;
				dim_type = rdim.etype;
				num_arcs = rdim.arc_blk_use;
				ua_init_entity(UA_DIA_IN_DIM,subtype,&(rdim));
				rdim.txt_just = UA_LEFT;
				rdim.entity_site = UA_TOP_LEFT;
				if( ( dim_type==UA_CENTERLINE ) )
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
				if( ( relation!=UA_DRAFT_ARC ) )
					{
					goto get;
					}
				}
			rdim.asso_blk_use = ablock;
			}
			break;
		}
	s_status = um_retrieve_data_relnum(curr_key,&(relation));
	if( ( relation==UA_LINEAR_DIM ) )
		{
		ua_centerline_arc(curr_key,center,&(radius),&(dang),normal,
									c_spt,c_ept,&(dummy));
		}
	else
		{
		uc_draft_arc(curr_key,center,&(radius),&(dang),normal,c_spt,
									c_ept,&(dummy));
		}
	if( ( fabs(um_dot(normal,zaxis))< UA_FUZZ) )
		{
		uu_uerror0(UA_DRAFTING,17);
		goto get;
		}
	um_vctovc(center, rdim.asso_blk[ablock-1].location);
	status = ua_dia_org(&(rdim),redo);
	/* added to keep the appended text. kathy */
	prev_txt_blk = rdim.txt_blk_use;
	if( ( !status ) )
		{
		uu_dexit;
		return;
		}
	s_status = ua_diam_cre(type,UU_TRUE,&(rdim),center,radius,
									dang,normal);
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
**    E_FUNCTION     : int		ua_diam_cre(type, mode, rdim, center, radius, 
**												dtheta, normal)
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
int		ua_diam_cre(type, mode, rdim, center, radius, 
							dtheta, normal)
int		type;
UU_LOGICAL	mode;
struct UA_generic_draft	(*rdim);
UU_REAL	center[3];
UU_REAL	radius;
UU_REAL	dtheta;
UU_REAL	normal[3];
	{
	int		dir;
	int		ablock;
	int		curr_key;
	int		i;
	int		j;
	int		d_stat;
	int		k;
	int		l;
	int		m;
	int		n;
	int		status;
	int		dummy;
	UU_REAL	cir_pt[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL	location[3];
	UU_REAL	flex_pt[3];
	UU_REAL	end_pt[3];
	UU_REAL	pt1[3];
	UU_REAL	origin[3];
	UU_REAL	corner[4][3];
	UU_LOGICAL	drw_leader;
	UU_LOGICAL	rev_arr;
	UU_LOGICAL	first;
	UU_LOGICAL	redo;

	uu_denter(UU_STRC,(us,"SAL ua_diam_cre(type=%d, mode=%s, rdim=%s,\
		center=<%g,%g,%g>, radius=%g, dtheta=%g, normal=<%g,%g,%g>)",
		type, mode?"TRUE":"FALSE", "...", center[0],center[1],center[2],
		radius, dtheta, normal[0],normal[1],normal[2]));

	ua_dia_txt(&((*rdim)),radius,corner,center);
	drw_leader = UU_TRUE;
	rev_arr = UU_FALSE;
	switch( type )
		{
		case 1:
			{
			ua_dia_inside(&((*rdim)),corner,center,normal,radius,pt1, cir_pt);
			drw_leader = UU_FALSE;
			}
			break;
		case 2:
			{
			if( (*rdim).draft_stand == 1 )
				{
				ua_dia_outiso(&((*rdim)),corner,center,normal,radius,pt1,cir_pt,&(dir));
				drw_leader = UU_FALSE;
				rev_arr = UU_FALSE;
				}
			else
				ua_dia_outside(&((*rdim)),corner,center,normal,radius,pt1,
								cir_pt,&(dir));
			}
			break;
		case 3:
			{
			if( ( mode==UU_TRUE ) )
				{
				ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
				d_stat = ud_world_coord(13,35,location,1,&(dummy),UU_FALSE);
				if( ( ( d_stat==0 )||( dummy==0 ) ) )
					{
					uu_dexit;
					return(0);
					}
				ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
				um_nptpln(location,origin,zaxis,end_pt);
				ablock = ( (*rdim).asso_blk_use+1 );
				um_vctovc(end_pt,(*rdim).asso_blk[ablock-1].location);
				(*rdim).asso_blk[ablock-1].asso_type = 104;
				(*rdim).asso_blk[ablock-1].key = 0;
				(*rdim).asso_blk_use = ablock;
				}
			else
				{
				um_vctovc((*rdim).asso_blk[1].location,end_pt);
				}
			ua_dia_oneside(&((*rdim)),corner,center,normal,radius,pt1,
			cir_pt,end_pt);
			drw_leader = UU_FALSE;
			}
			break;
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
**    E_FUNCTION     : UU_LOGICAL	ua_dia_org(rdim, redo)
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
UU_LOGICAL	ua_dia_org(rdim, redo)
struct UA_generic_draft	(*rdim);
UU_LOGICAL	redo;
	{
	UU_LOGICAL	ok;
	int		status;

	uu_denter(UU_STRC,(us,"SAL ua_dia_org(rdim=%s, redo=%s)", "...",
		redo?"TRUE":"FALSE"));

origin:
	if( ( redo==UU_FALSE ) )
		{
		status = ua_ent_origin_subf(30,&((*rdim)));
		}
	else
		{
		status = ua_ent_origin_subf(118,&((*rdim)));
		}
	if( ( status!=1 ) )
		{
		uu_dexit;
		return(UU_FALSE);
		}
	if( ( redo==UU_FALSE ) )
		{
		ok = ua_text_subf(&((*rdim)));
		if( ( !ok ) )
			{
			goto origin;
			}
		}
	uu_dexit;
	return(UU_TRUE);
	}

/*********************************************************************
**    E_FUNCTION     : ua_diam_regen(rdim)
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
ua_diam_regen(rdim)
struct UA_generic_draft	(*rdim);
	{
	int		curr_key;
	int		status;
	int		type;
	int		relation;
	UU_REAL	c_spt[3];
	UU_REAL	dtheta;
	UU_REAL	center[3];
	UU_REAL	radius;
	UU_REAL	normal[3];
	UU_REAL	c_ept[3];
	UU_REAL	dummy;
	UU_LOGICAL	mode;

	uu_denter(UU_STRC,(us,"SAL ua_diam_regen(rdim=%s)", "..."));

	curr_key = (*rdim).asso_blk[0].key;
	status = um_retrieve_data_relnum(curr_key,&(relation));
	if( ( relation==UA_LINEAR_DIM ) )
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
	if( type == 9 || type == 10 )
		status = ua_dia_cre_rep_dim(&(*rdim),type,center,radius,dtheta,normal);
	else
		status = ua_diam_cre(type,mode,&((*rdim)),center,radius,
								dtheta,normal);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_txt(rdim, radius, corner, center)
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
ua_dia_txt(rdim, radius, corner, center)
struct UA_generic_draft	(*rdim);
UU_REAL	radius;
UU_REAL	corner[4][3];
UU_REAL center[3];
	{
	char		temp[1025];
	char		rsym[13];
	int		num;
	int		i;
	int		j;
	UU_REAL	sx_del;
	UU_REAL	sy_del;
	UU_REAL	dl_pt[3];
	UU_REAL	mx_del;
	UU_REAL	vec1[3];
	UU_REAL	my_del;
	UU_REAL	vec2[3];

	static char		crsym[1025] = "\\n";

	uu_denter(UU_STRC,(us,"SAL ua_dia_txt(rdim=%s, radius=%g, corner=%s)",
			"...", radius, "..."));

	(*rdim).dim_value = ( 2.000000e+000*radius );
	if( ( (*rdim).txt_entry==UA_SYS_TEXT ) )
		{
		switch( (*rdim).diam_symbol )
			{
			case UA_NO_SYMBOL:
				{
				goto offset;
				}
			case UA_R_SYMBOL:
				{
				strcpy(rsym,"DIA");
				num = 3;
				}
				break;
			case UA_RAD_SYMBOL:
				{
				strcpy(rsym,"\\T");
				num = 1;
				}
				break;
			case UA_USER_SYMBOL:
				{
				strcpy(rsym,UA_usr_dia_sym);
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
	if( (*rdim).txt_orent > 0 )
		ua_dia_txt_ang(&(*rdim),center);
	ua_box_site(&((*rdim)),corner,dl_pt);
	ua_box_frame(&((*rdim)),corner);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_txt_ang(rdim, center)
**      This function finds the text angle for diameter dimension text
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
ua_dia_txt_ang(rdim,center)
struct UA_generic_draft	(*rdim);
UM_coord center;
	{
	UM_coord	xaxis;
	UM_coord	zaxis;
	UM_coord	rad_line;
	UU_REAL		rad_line_angle;
	int		i;
	uu_denter(UU_STRC,(us,"ua_dia_txt_ang(center=<%g,%g,%g>)",
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
