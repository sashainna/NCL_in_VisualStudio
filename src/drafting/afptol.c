
/*********************************************************************
**    NAME         : afptol.c
**       CONTAINS:
**					ua_fptol
**					ua_init_fpline
**					ua_fptol_char
**					ua_fptol_datum
**					ua_fptol_tol
**					ua_crea_fptol
**					ua_crea_box
**					ua_update_fptol
**					ua_fp_leader
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       afptol.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:34
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) afptol.c 3.3 4/11/88 13:05:56 single"};
#else
static char uu_sccsident[]={"@(#) afptol.c 3.3 4/11/88 13:05:56 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "mdcoord.h"

struct 	UA_fp_line {
			int	no_boxes;
			int 	char_indx;
			int 	datum_indx;
			int 	tol_indx;
			int 	box_type[20][2];
			int 	char_type[6];
			char	datum[6][21];
			char	tol[6][9];
	};

us_init_afptol()
{
}

/*********************************************************************
**    E_FUNCTION     : ua_crea_fptol(e, fpline, no_lines, s_loc, e_loc)
**       Create a form-positional tol entity.
**    PARAMETERS   
**       INPUT  : 
**				e								entity record
**				fpline						users input
**				no_lines						number of lines 
**				s_loc							option for start of leader
**				e_loc							option for end of leader
**       OUTPUT :  
**				e								updated entity record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_crea_fptol(e, fpline, no_lines, s_loc, e_loc)
struct UA_generic_draft	(*e);
struct UA_fp_line	fpline[5];
int		no_lines, s_loc, e_loc;
	{
	int		indx, type, no_end_pt, key, length, nt_blk, i, j, k, m, n;
	UM_coord	off_org, origin, corner[4], save_org, def_org, ref_pt, x_proj,
				xaxis, y_proj, yaxis, delt, zaxis, tmp1, tmp2, tmp3;
	UU_REAL  end_pt[20][2][3]; 
	UU_REAL	offset_dist, x_len, y_len, save_delx, delx, dely;

	static char		blk[1025] = " ";
	static char		def_tol[15][3] = 	{ "\\A","\\B","\\C","\\D",
		"\\E","\\F","\\H","\\I","\\J","\\K","\\L","\\M","\\N","\\O",
		"\\b"		};

	uu_denter(UU_STRC,(us,"SAL ua_crea_fptol(e=%s, fpline=%s, no_lines=%d,\
		s_loc=%d, e_loc=%d)", "...", "...", no_lines, s_loc, e_loc));

	for(i=0;i<3;i++) def_org[i] = 0.0;
	ua_getcpln(&((*e)),origin,xaxis,yaxis,zaxis);
	delx = e->txt_gap;
	dely = e->txt_gap;
	um_vctmsc(xaxis,delx,x_proj);
	um_vctmsc(yaxis,dely,y_proj);
	for(i=1;i<=no_lines;i++)
		{
		no_end_pt = fpline[i-1].no_boxes;
		for(j=0;j<3;j++) off_org[j] = 0.0;
			{
			for(j=1;j<=no_end_pt;j++)
				{
				type = fpline[i-1].box_type[j-1][0];
				indx = fpline[i-1].box_type[j-1][1];
				e->txt_blk_use = ( (*e).txt_blk_use+1 );
				nt_blk = e->txt_blk_use;
				for(k=0;k<3;k++) e->txt_blk[nt_blk-1].origin[k] = 0.0;
				strcpy(e->txt_blk[nt_blk-1].tstring,"");
				e->txt_blk[nt_blk-1].char_cnt = 0;
				switch( type )
					{
					case 1:
						strcpy(e->txt_blk[nt_blk-1].tstring,(*e).
									txt_blk[nt_blk-1] .tstring);
						strcat(e->txt_blk[nt_blk-1].tstring,def_tol[fpline[i-1].
						    char_type[indx-1]-1]);
						e->txt_blk[nt_blk-1].char_cnt = ( (*e).txt_blk[nt_blk-1].
						    char_cnt+2 );
						length = 1;
						break;
					case 2:
						length = strlen(fpline[i-1].datum[indx-1]);
						if( ( length==0 ) )
							{
							e->txt_blk_use = ( (*e).txt_blk_use-1 );
							no_end_pt = ( no_end_pt-1 );
							goto next_box;
							}
						strcpy(e->txt_blk[nt_blk-1].tstring,(*e).txt_blk
									[nt_blk-1].tstring);
						strcat(e->txt_blk[nt_blk-1].tstring,fpline[i-1].
									datum[indx-1]);
						e->txt_blk[nt_blk-1].char_cnt = ( (*e).txt_blk[nt_blk-1].
						    char_cnt+length );
						break;
					case 3:
						length = strlen(fpline[i-1].tol[indx-1]);
						if( ( length==0 ) )
							{
							e->txt_blk_use = ( (*e).txt_blk_use-1 );
							no_end_pt = ( no_end_pt-1 );
							goto next_box;
							}
						strcpy(e->txt_blk[nt_blk-1].tstring,(*e).txt_blk
									[nt_blk-1].tstring);
						strcat(e->txt_blk[nt_blk-1].tstring,fpline[i-1].
									tol[indx-1]);
						e->txt_blk[nt_blk-1].char_cnt = ( (*e).txt_blk[nt_blk-1].
						    char_cnt+length );
						break;
					}
				ua_text_box(nt_blk,&((*e)),corner);
				if( ( ( i==1 )&&( j==1 ) ) )
					offset_dist = (um_dcccc(corner[0],corner[3])+
										( 5.0e-001*dely ) );
				else
					{
					um_vcplvc(corner[0],def_org,corner[0]);
					um_vcplvc(corner[2],def_org,corner[2]);
					}
				um_vctovc(corner[0],end_pt[j-1][0]);
				um_vctovc(corner[2],end_pt[j-1][1]);
				um_vcmnvc(corner[2],corner[0],delt);
				if( ( j==1 ) )
					{
					um_vctmsc(x_proj,(UU_REAL) (1.0 / 2.0),tmp2);
					um_vctmsc(y_proj,(UU_REAL) (1.0 / 4.0),tmp3);
					um_vcplvc(tmp2,tmp3,tmp1);
					um_vcmnvc(end_pt[j-1][0],tmp1,end_pt[j-1][0]);
					um_vctmsc(x_proj,(UU_REAL) (1.0 / 2.0),tmp2);
					um_vctmsc(y_proj,(UU_REAL) (1.0 / 4.0),tmp3);
					um_vcplvc(tmp2,tmp3,tmp1);
					um_vcplvc(end_pt[j-1][1],tmp1,end_pt[j-1][1]);
					um_vcmnvc(end_pt[j-1][1],end_pt[j-1][0],off_org);
					if( ( i>1 ) ) um_vcplvc(e->txt_blk[nt_blk-1].origin,
							def_org,e->txt_blk[nt_blk-1].origin);
					}
				else
					{
					x_len = um_dot(off_org,xaxis);
					y_len = um_dot(off_org,yaxis);
					um_vcmnvc(end_pt[( j-1 )-1][0],e->dim_origin,save_org);
					um_vcplvc(e->txt_blk[nt_blk-1].origin,off_org,tmp1);
					um_vcplvc(tmp1,save_org,e->txt_blk[nt_blk-1].origin);
					um_vctmsc(x_proj,(UU_REAL) (1.0/2.0),tmp2);
					um_vctmsc(y_proj,(UU_REAL) (1.0/4.0),tmp3);
					um_vcmnvc(tmp2,tmp3,tmp1);
					um_vcplvc(e->txt_blk[nt_blk-1].origin,tmp1,
												e->txt_blk[nt_blk-1].origin);
					um_vctmsc(xaxis,x_len,tmp1);
					um_vcplvc(end_pt[( j-1 )-1][0],tmp1,end_pt[j-1][0]);
					um_vcplvc(end_pt[j-1][0],delt,tmp2);
					um_vcplvc(tmp2,x_proj,tmp1);
					um_vctmsc(y_proj,(UU_REAL) (1.0 / 2.0),tmp3);
					um_vcplvc(tmp1,tmp3,end_pt[j-1][1]);
					um_vcmnvc(end_pt[j-1][1],end_pt[j-1][0],off_org);
					}
next_box:;
				}
			}
		if( ( ( i==1 )&&( e_loc!=1 ) ) )
			{
			ua_fp_leader(&((*e)),s_loc,e_loc,no_end_pt,end_pt);
			}
		um_vctmsc(yaxis,offset_dist,tmp1);
		um_vcmnvc(def_org,tmp1,def_org);
		ua_crea_box(&((*e)),no_end_pt,end_pt);
		}
	ua_create_entity(&((*e)),&(key));
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_fptol_char(fpline)
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
int		ua_fptol_char(fpline)
struct UA_fp_line	(*fpline);
	{
	int		choice, i, d_stat, mode, dummy;

	uu_denter(UU_STRC,(us,"SAL ua_fptol_char(fpline=%s)", "..."));

	d_stat = ua_popmenu(7,&(mode));
	if( ( d_stat==0 ) )
		{
		uu_dexit;
		return(0);
		}
	if( ( mode==9 ) )
		{
		d_stat = ua_popmenu(8,&(mode));
		if( ( d_stat==0 ) )
			{
			uu_dexit;
			return(0);
			}
		else
			{
			choice = ( mode+8 );
			goto load;
			}
		}
	else
		{
		choice = mode;
		goto load;
		}
load:
	fpline->no_boxes = ( fpline->no_boxes+1 );
	fpline->char_indx = ( fpline->char_indx+1 );
	i = fpline->no_boxes;
	fpline->box_type[i-1][0] = 1;
	fpline->box_type[i-1][1] = fpline->char_indx;
	fpline->char_type[fpline->char_indx-1] = choice;
	uu_dexit;
	return(1);
	}

/*********************************************************************
**    E_FUNCTION     : ua_fp_leader(e, s_loc, e_loc, no_end_pt, end_pt)
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
ua_fp_leader(e, s_loc, e_loc, no_end_pt, end_pt)
struct UA_generic_draft	(*e);
int		s_loc, e_loc, no_end_pt;
UU_REAL	end_pt[20][2][3];
	{
	int		key, i, j, k, m, n, ss_loc;
	UU_REAL	y_len, dis1, dis2;
	UM_coord	e_pos, s_pos, save_org, origin, xaxis, yaxis, delt,
				tmp1, location, zaxis;

	uu_denter(UU_STRC,(us,"SAL ua_fp_leader(e=%s, s_loc=%d, e_loc=%d,\
		no_end_pt=%d, end_pt=%s)", "...", s_loc, e_loc, no_end_pt, "..."));

	ua_getcpln(&((*e)),origin,xaxis,yaxis,zaxis);
	if( ( e_loc!=5 ) )
		{
		k = (*e).asso_blk_use;
		um_vctovc((*e).asso_blk[k-1].location,e_pos);
		if( ( e_loc==3 ) ) (*e).asso_blk_use = ( (*e).asso_blk_use-1 );
		dis1 = um_dcccc(e_pos,end_pt[0][0]);
		dis2 = um_dcccc(e_pos,end_pt[no_end_pt-1][0]);
		if( ( dis1<dis2 ) )
			{
			ss_loc = 3;
			m = 1;
			n = 1;
			}
		else
			{
			ss_loc = 4;
			m = 2;
			n = no_end_pt;
			}
		um_vcmnvc(end_pt[n-1][1],end_pt[n-1][0],delt);
		y_len = ( um_dot(delt,yaxis)/2.000000e+000 );
		if( ( m==1 ) )
			{
			um_vctmsc(yaxis,y_len,tmp1);
			um_vcplvc(end_pt[n-1][m-1],tmp1,s_pos);
			}
		else
			{
			um_vctmsc(yaxis,y_len,tmp1);
			um_vcmnvc(end_pt[n-1][m-1],tmp1,s_pos);
			}
		ua_crea_leader(&((*e)),s_pos,e_pos,ss_loc);
		}
	else
		{
		k = (*e).asso_blk_use;
		um_vctovc((*e).asso_blk[k-1].location,e_pos);
		(*e).asso_blk_use = ( (*e).asso_blk_use-1 );
		n = no_end_pt;
		um_vcmnvc(end_pt[n-1][1],end_pt[n-1][0],delt);
		y_len = um_dot(delt,yaxis);
		um_vctovc((*e).dim_origin,save_org);
		switch( s_loc )
			{
			case 1:
				um_vctmsc(yaxis,y_len,tmp1);
				um_vcplvc(end_pt[0][0],tmp1,s_pos);
				break;
			case 2:
				um_vctovc(end_pt[0][0],s_pos);
				break;
			case 3:
				um_vctovc(end_pt[n-1][1],s_pos);
				break;
			case 4:
				um_vctmsc(yaxis,y_len,tmp1);
				um_vcmnvc(end_pt[n-1][1],tmp1,s_pos);
				break;
			}
		ua_crea_witness(&((*e)),s_pos,e_pos);
		um_vcmnvc((*e).dim_origin,save_org,delt);
		for(i=1;i<=no_end_pt;i++)
			{
			um_vcplvc(end_pt[i-1][0],delt,end_pt[i-1][0]);
			um_vcplvc(end_pt[i-1][1],delt,end_pt[i-1][1]);
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_fptol()
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
ua_fptol()
	{
	struct UA_fp_line	fpline[5];
	struct UA_generic_draft	fpdim;
	int	start_loc, size, choice, curr_key, line_no, box_choice, subtype,
			i, j, d_stat, k, l, m, dummy;
	UM_coord	curr_origin;

	uu_denter(UU_STRC,(us,"SAL ua_fptol()"));

	subtype = 0;
	curr_key = -1;
	us_init_afptol();
	us_init_autility();
	ua_dump_set(0xffff);

main_loop:
	ua_init_entity(UA_FANDP_TOL,subtype,&(fpdim));
	fpdim.txt_just = UA_LEFT;
	fpdim.entity_site = UA_TOP_LEFT;

	choice = ua_leader_subf(1,&(fpdim),&(start_loc));
	if( ( choice==0 ) )
		{
		uu_dexit;
		return;
		}
	if( ( choice==4 ) )
		{
		if( ( curr_key>0 ) )
			{
			fpdim.key = curr_key;
			size = 22500;
			d_stat = uc_retrieve_data(&(fpdim),size);
			if( ( d_stat==0 ) )
				{
				um_vctovc(fpdim.dim_origin,curr_origin);
				ua_ent_origin_subf(30,&(fpdim));
				ua_update_fptol(curr_origin,&(fpdim));
				d_stat = ua_update_entity(curr_key,&(fpdim));
				if( ( d_stat!=0 ) ) ua_create_entity(&(fpdim),&(curr_key));
				uc_display(&(fpdim));
				}
			}
		goto main_loop;
		}
	line_no = 1;
	ua_init_fpline(&(fpline[line_no-1]));
line_loop:
	if( ( fpline[line_no-1].no_boxes>19 ) )
		{
		uu_uerror0(UA_DRAFTING,29);
		goto done;
		}
	d_stat = ua_popmenu(6,&(box_choice));
	switch( box_choice )
		{
		case 1:
			if( ( fpline[line_no-1].char_indx>5 ) )
				{
				uu_uerror0(UA_DRAFTING,30);
				goto line_loop;
				}
			d_stat = ua_fptol_char(&(fpline[line_no-1]));
			if( ( d_stat==0 ) )
				{
				uu_dexit;
				return;
				}
			goto line_loop;
		case 3:
			if( ( fpline[line_no-1].datum_indx>5 ) )
				{
				uu_uerror0(13,32);
				goto line_loop;
				}
			d_stat = ua_fptol_datum(&(fpline[line_no-1]));
			if( ( d_stat==0 ) )
				{
				uu_dexit;
				return;
				}
			goto line_loop;
		case 2:
			if( ( fpline[line_no-1].tol_indx>5 ) )
				{
				uu_uerror0(UA_DRAFTING,31);
				goto line_loop;
				}
			d_stat = ua_fptol_tol(&(fpline[line_no-1]));
			if( ( d_stat==0 ) )
				{
				uu_dexit;
				return;
				}
			goto line_loop;
		case 4:
			if( ( fpline[line_no-1].no_boxes==0 ) ) goto done;
			else
				{
				if( ( line_no<5 ) )
					{
					line_no = ( line_no+1 );
					ua_init_fpline(&(fpline[line_no-1]));
					goto line_loop;
					}
				else
					{
					uu_uerror0(UA_DRAFTING,33);
					goto done;
					}
				}
		}
done:
	ua_ent_origin_subf(30,&(fpdim));
	ua_crea_fptol(&(fpdim),fpline,( line_no-1 ),start_loc,choice);
	uc_display(&(fpdim));
	curr_key = fpdim.key;
	goto main_loop;
	}

/*********************************************************************
**    E_FUNCTION     : ua_crea_box(e, n, end_pt)
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
ua_crea_box(e, n, end_pt)
struct UA_generic_draft	(*e);
int		n;
UU_REAL	end_pt[20][2][3];
	{
	int		i, j, k;
	UU_REAL	x_proj, y_proj;
	UM_coord	pt1, pt2, origin, xaxis, yaxis, delt, zaxis, tmp1;

	uu_denter(UU_STRC,(us,"ua_crea_box(e=%s, n=%d, end_pt=%s)",
		"...", n, "..."));

	ua_getcpln(&((*e)),origin,xaxis,yaxis,zaxis);
	um_vctovc(end_pt[0][0],pt1);
	k = ( (*e).line_blk_use+1 );
	j = 1;
	(*e).line_blk_use = k;
	(*e).line_blk[k-1].subtype = misc_line;
	um_vcmnvc(end_pt[n-1][1],end_pt[0][0],delt);
	x_proj = um_dot(delt,xaxis);
	y_proj = um_dot(delt,yaxis);
	for(i=1;i<=4;i++)
		{
		switch( i )
			{
			case 1:
				um_vctmsc(yaxis,y_proj,tmp1);
				um_vcplvc(pt1,tmp1,pt2);
				break;
			case 2:
				um_vctmsc(xaxis,x_proj,tmp1);
				um_vcplvc(pt1,tmp1,pt2);
				break;
			case 3:
				um_vctmsc(yaxis,y_proj,tmp1);
				um_vcmnvc(pt1,tmp1,pt2);
				break;
			case 4:
				um_vctmsc(xaxis,x_proj,tmp1);
				um_vcmnvc(pt1,tmp1,pt2);
				break;
			}
		um_vctovc(pt1,(*e).line_blk[k-1].line_seg[j-1]);
		um_vctovc(pt2,(*e).line_blk[k-1].line_seg[( j+1 )-1]);
		j = ( j+2 );
		um_vctovc(pt2,pt1);
		}
	for(i=1;i<=(n-1);i++)
		{
		um_vcmnvc(end_pt[i-1][1],end_pt[i-1][0],delt);
		y_proj = um_dot(delt,yaxis);
		um_vctovc(end_pt[i-1][1],pt1);
		um_vctmsc(yaxis,y_proj,tmp1);
		um_vcmnvc(pt1,tmp1,pt2);
		um_vctovc(pt1,(*e).line_blk[k-1].line_seg[j-1]);
		um_vctovc(pt2,(*e).line_blk[k-1].line_seg[( j+1 )-1]);
		j = ( j+2 );
		}
	(*e).line_blk[k-1].num_pts = ( j-1 );
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_fptol_tol(fpline)
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
int		ua_fptol_tol(fpline)
struct UA_fp_line	(*fpline);
	{
	char		tol_text[17];
	char		sym_text[9];
	int		choice, i, d_stat, mode, dummy;

	static char		def_dia[1][4] = 	{ "DIA"		};
	static char		def_tol[8][9] = 	{ ".0002",".0005",".001",
									".002",".005",".010",".025",".050"		};
	static char		def_sym[5][3] = 	{ "\\P","\\R","\\S","\\T", "  "		};

	uu_denter(UU_STRC,(us,"SAL ua_fptol_tol(fpline=%s)", "..."));

	strcpy(tol_text,"");

main_loop:
	d_stat = ua_popmenu(11,&(mode));
	if( ( d_stat==0 ) )
		{
		uu_dexit;
		return(0);
		}
	switch( mode )
		{
		case 1:
			d_stat = ua_popmenu(12,&(mode));
			if( ( mode==6 ) )
				{
				d_stat = ud_string(13,103,sym_text,4,&(dummy),UU_FALSE);
				if( ( d_stat==0 ) )
					{
					uu_dexit;
					return(0);
					}
				else
					{
					strcpy(tol_text,tol_text);
					strcat(tol_text,sym_text);
					goto main_loop;
					}
				}
			else
				{
				if( ( mode<5 ) )
					{
					strcpy(tol_text,tol_text);
					strcat(tol_text,def_sym[mode-1]);
					}
				else
					{
					strcpy(tol_text,tol_text);
					strcat(tol_text,def_dia[0]);
					}
				goto main_loop;
				}
		case 2:
			d_stat = ua_popmenu(13,&(mode));
			if( ( mode==9 ) )
				{
				d_stat = ud_string(13,102,sym_text,8,&(dummy),UU_FALSE);
				if( ( d_stat==0 ) )
					{
					uu_dexit;
					return(0);
					}
				else
					{
					strcpy(tol_text,tol_text);
					strcat(tol_text,sym_text);
					goto main_loop;
					}
				}
			else
				{
				strcpy(tol_text,tol_text);
				strcat(tol_text,def_tol[mode-1]);
				goto main_loop;
				}
		case 3:
			fpline->no_boxes = ( fpline->no_boxes+1 );
			fpline->tol_indx = ( fpline->tol_indx+1 );
			i = fpline->no_boxes;
			fpline->box_type[i-1][0] = 3;
			fpline->box_type[i-1][1] = fpline->tol_indx;
			strcpy(fpline->tol[fpline->tol_indx-1],tol_text);
			uu_dexit;
			return(1);
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_update_fptol(curr_org, e)
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
ua_update_fptol(curr_org, e)
UU_REAL	curr_org[3];
struct UA_generic_draft	(*e);
	{
	int		i, j, num;
	UU_REAL	x_proj, y_proj;
	UM_coord	origin, delta, pt, xaxis, yaxis, delt, zaxis, tmp1, tmp2, tmp3;

	uu_denter(UU_STRC,(us,"SAL ua_update_fptol(curr_org=<%g,%g,%g>, e=%s)",
		curr_org[0],curr_org[1],curr_org[2], "..."));

	ua_getcpln(e,origin,xaxis,yaxis,zaxis);
	um_vcmnvc(e->dim_origin,curr_org,delt);
	for(i=1;i<=(e->line_blk_use);i++)
		{
		switch( e->line_blk[i-1].subtype )
			{
			case misc_line:
				for(j=1;j<=(e->line_blk[i-1].num_pts);j++)
					{
					um_vcplvc(e->line_blk[i-1].line_seg[j-1],delt,e->
					    line_blk[i-1].line_seg[j-1]);
					}
				break;
			case dim_line:
				for(j=1;j<3;j++)
					{
					um_vcplvc(e->line_blk[i-1].line_seg[j-1],delt,e->
					    line_blk[i-1].line_seg[j-1]);
					}
				break;
			case ext_line:
				um_vcmnvc(e->line_blk[i-1].line_seg[0],curr_org,pt);
				um_vcplvc(pt,e->dim_origin,tmp1);
				um_vcmnvc(e->line_blk[i-1].line_seg[1],tmp1,delta);
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
				um_vcplvc(pt,e->dim_origin,e->line_blk[i-1].line_seg[0]);
				break;
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_init_fpline(fpline)
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
ua_init_fpline(fpline)
struct UA_fp_line	(*fpline);
	{

	uu_denter(UU_STRC,(us,"SAL ua_init_fpline(fpline=%s)", "..."));

	fpline->no_boxes = 0;
	fpline->char_indx = 0;
	fpline->datum_indx = 0;
	fpline->tol_indx = 0;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_fptol_datum(fpline)
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
int		ua_fptol_datum(fpline)
struct UA_fp_line	(*fpline);
	{
	char		sym_text[9], datum_text[17];
	int		choice, i, d_stat, mode, dummy;

	static char		def_sym[2][3] = 	{ "\\P","\\R"		}; 
	static char		minus[1025] = "-";

	uu_denter(UU_STRC,(us,"SAL ua_fptol_datum(fpline=%s)", "..."));

	strcpy(datum_text,"");
	d_stat = ua_popmenu(9,&(mode));
	if( ( d_stat==0 ) )
		{
		uu_dexit;
		return(0);
		}
	switch( mode )
		{
		case 1:
			d_stat = ud_string(13,104,sym_text,8,&(dummy),UU_FALSE);
			if( ( d_stat==0 ) )
				{
				uu_dexit;
				return(0);
				}
			else
				strcpy(datum_text,sym_text);
			break;
		case 2:
			for(i=1;i<=2;i++)
				{
				d_stat = ud_string(13,104,sym_text,8,&(dummy),UU_FALSE);
				if( ( d_stat==0 ) )
					{
					uu_dexit;
					return(0);
					}
				else
					{
					strcpy(datum_text,datum_text);
					strcat(datum_text,sym_text);
					}
				}
			break;
		case 3:
			d_stat = ud_string(13,104,sym_text,8,&(dummy),UU_FALSE);
			if( ( d_stat==0 ) )
				{
				uu_dexit;
				return(0);
				}
			else
				{
				strcpy(datum_text,"-");
				strcat(datum_text,sym_text);
				strcpy(datum_text,datum_text);
				strcat(datum_text,minus);
				goto load;
				}
		}
	d_stat = ua_popmenu(10,&(mode));
	if( ( d_stat==0 ) )
		{
		uu_dexit;
		return(0);
		}
	if( ( ( d_stat==1 )&&( mode==0 ) ) ) goto load;
	switch( mode )
		{
		case 1:
			goto load;
		case 4:
			d_stat = ud_string(13,103,sym_text,8,&(dummy),UU_FALSE);
			if( ( d_stat==0 ) )
				{
				uu_dexit;
				return(0);
				}
			else
				{
				strcpy(datum_text,datum_text);
				strcat(datum_text,sym_text);
				goto load;
				}
		default:
			strcpy(datum_text,datum_text);
			strcat(datum_text,def_sym[( mode-1 )-1]);
			goto load;
		}
load:
	fpline->no_boxes = ( fpline->no_boxes+1 );
	fpline->datum_indx = ( fpline->datum_indx+1 );
	i = fpline->no_boxes;
	fpline->box_type[i-1][0] = 2;
	fpline->box_type[i-1][1] = fpline->datum_indx;
	strcpy(fpline->datum[fpline->datum_indx-1],datum_text);
	uu_dexit;
	return(1);
	}
