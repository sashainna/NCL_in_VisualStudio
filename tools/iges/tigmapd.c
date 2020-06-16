/*********************************************************************
**    NAME         :  tigmapd.c
**       CONTAINS:
**					uig_map_lindim
**					uig_map_gnote
**					uig_map_diadim
**					uig_map_radim
**					uig_map_leader
**					uig_set_dpln
**					uig_map_angdim
**					uig_str_adjust
**             uig_map_label
**             uig_map_gsymbol
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigmapd.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:47
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "umath.h"
#include "adrfcom.h"
#include "tigtypes.h"
#include "udebug.h"
#include "atext.h"
#include "usysdef.h"
#include "uhep.h"
#include "mdrel.h"
#include "mdclass.h"
#include "mdeval.h"
#include "mcrv.h"
#include "msrf.h"
#include "umoveb.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mdgenent.h"
#include "mattr.h"
#include "msol.h"
#include "mdebug.h"
#include "r1emsgpk.h"
#include "adrf.h"
#include "adraft.h"

#define IG_PI (UU_REAL) 3.14159265359

UU_REAL x_axis[3] =  {1.,0.,0.};
UU_REAL z_axis[3] =  {0.,0.,1.};
extern UU_LOGICAL drw_flag;
extern int drw_ptr;
extern UU_REAL drw_t[4][3], drw_s[4][3], drw_v[4][3];

void uig_arrow();

/*********************************************************************
**    I_FUNCTION     :  uig_map_lindim(dblk,t,gnote,l1_form,l1,
**													l2_form,l2,w2,w2,key)
**				Map a IGES linear dim to a Unibase linear dim.
**    PARAMETERS   
**       INPUT  : 
**				dblk							directory block
**				t								IGES transformation matrix
**				gnote							 associated note
**				l1_form                  first leader form number
**				l1								 first leader 
**				l2_form                  second leader form number
**				l2								 second leader 
**				w1								 first witness line
**				w2								 second witness line
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_lindim(dblk,t,g,l1_form,l1,l2_form,l2,w1,w2,key)
	struct dir_rec *dblk;				/*  directory record */
	UU_REAL t[12];
	struct IG_igesnote_rec *g;
	int l1_form;
	struct IG_igeslead_rec *l1;
	int l2_form;
	struct IG_igeslead_rec *l2;
	struct IG_poly2d_rec *w1;
	struct IG_poly2d_rec *w2;
	UU_KEY_ID *key;
	{

	struct UA_generic_draft e;

	int i,j,k,start,arrow_type,mm,m_init,istat,nc;
	int num,count,l_cnt,l_loc,a_cnt,l_form;
	UU_TRUEDOUBLE ff;
	UU_LOGICAL w_first, zero_length;
	UU_REAL pt1[3],pt2[3],a_org[3];
	UU_REAL *pt;
	struct IG_gnote_rec *p;
	struct IG_igeslead_rec *l;
	struct IG_poly2d_rec *w;
	UU_REAL a_angle[2],vec[3], end_pt[2], arrow_pt[2], x_size;
	UU_REAL um_angle2p(), um_mag();
	UU_REAL fabs();
	UU_TRUEDOUBLE uig_atof();
	/* NCL: added new routine to parse for numeric strings */
	/* NCL: corrects problem with various forms of linear dim stmts */
	char tmpstr[20];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_lindim"));

	/* load record with defaults */

	ua_init_entity(UA_LINEAR_DIM,1,&e);
	e.rel_num = UA_LINEAR_DIMS_REL;

	/* load entity plane data */

	uig_set_dpln(t,&e);

	/*********************
	**                  **
	** load text blocks **
	**                  **
	*********************/

	num = g->no_gnote;
	p = g->gnote;
	if (num>10)  /*jkd2*/
		{
		sprintf(p_buff,
			"(DREC = %d) Number of notes in linear dimension = %d; set to 10.\n", 
			dblk->drec_num, num);
		uig_list_out(p_buff, UU_TRUE);
		num = 10;
		}
	for(i=0;i<num;i++)
		{
		e.txt_blk[i].subtype = main_txt1;
		nc = p->str_cnt;
                if (nc > 0)
                  {
		   e.txt_blk[i].char_cnt = p->str_cnt;
		   count = strlen(&p->str[0]);
		   start = count - p->str_cnt;
		/*
		uu_dprint(UU_MTRC,(us,"gnote: i = %d, count = %d, start = %d", i, count, start));
		uu_dprint(UU_MTRC,(us,"gnote: string = %s", &p->str[0]));
		*/
		   strcpy(&e.txt_blk[i].tstring[0],&p->str[start]);
		   uig_str_adjust(i, &e, p->fc);
                  }
                else
                  {
                   e.txt_blk[i].char_cnt = 1;
                   strcpy(&e.txt_blk[i].tstring[0]," ");
                  }
		if(i == 0)
			{
			/* NCL: call to extract only numeric part of dimension stmts */
			istat = uig_numeric_str(&p->str[start],tmpstr);
/*		ff = uig_atof(tmpstr);    */
			if (istat == UU_SUCCESS)
				ff = uig_atof(tmpstr);
			else
				ff = 1.0;
			ff = unit_scale * ff;
			e.dim_value = ff;
			if(unit_scale > 1.0)
				{
				e.units_sym = e.d_units_sym = UA_INCHES;
				}
			else
				{
				e.units_sym = e.d_units_sym = UA_CENTIMETERS;
				}
			uig_tran_coor(&p->xyzt[0],t,&e.dim_origin[0]);
			um_vctmsc(&e.dim_origin[0],unit_scale,&e.dim_origin[0]);
			e.txt_blk[i].origin[0] = e.txt_blk[i].origin[1] 
					= e.txt_blk[i].origin[2] = 0.0;
			}
		else
			{
			uig_tran_coor(&p->xyzt[0],t,&e.txt_blk[i].origin[0]);
			um_vctmsc(&e.txt_blk[i].origin[0],unit_scale,&e.txt_blk[i].origin[0]);
			um_vcmnvc(&e.txt_blk[i].origin[0],&e.dim_origin[0],
						&e.txt_blk[i].origin[0]);
			}
                if (nc > 0)
                  {
		   e.txt_blk[i].dx = unit_scale * p->wt;
		   e.txt_blk[i].dy = unit_scale * p->ht;
		   e.txt_blk[i].slant = p->sl;
		   e.txt_blk[i].tangle =  -p->ang; /* cpp: store correct angle */
		   e.txt_blk[i].txt_size = e.txt_blk[i].dy;

		/* cpp: add calculation for the character expansion factor */
		   x_size = 1.2*(e.txt_blk[i].dx/e.txt_blk[i].char_cnt);
		   e.txt_blk[i].char_expansion = x_size/e.txt_blk[i].txt_size;
                  }
		e.txt_blk_use++;
		if (e.txt_blk_use>10)  /*jkd2*/
			{
			sprintf(p_buff,
				"(DREC = %d) Number of text strings in linear dimension > 10, = %d\n", 
				dblk->drec_num, e.txt_blk_use);
			uig_list_out(p_buff,UU_TRUE);
			}
		p++;
		}
	
	/*************************
	**                      **
	** process leader lines **
	**                      **
	*************************/

	zero_length = UU_FALSE;
	for(i=0;i<2;i++)
		{
		switch(i)
			{
			case 0:
				l = l1;
				l_form = l1_form;
				l_cnt = e.line_blk_use;
				m_init = 0;
				e.line_blk_use++;
				if (e.line_blk_use>5)  /*jkd2*/
					{
					sprintf(p_buff,"(DREC = %d) Number of lines > 5, = %d.\n", 
						dblk->drec_num, e.line_blk_use);
					uig_list_out(p_buff,UU_TRUE);
					}
				e.line_blk[l_cnt].subtype = dim_line;
				e.line_blk[l_cnt].num_pts = 2 * l->num_seg;
				break;
			case 1:
				l = l2;
				l_form = l2_form;
				m_init = e.line_blk[l_cnt].num_pts;
		  		e.line_blk[l_cnt].num_pts = e.line_blk[l_cnt].num_pts + 
																			2 * l->num_seg;
				break;
			}
	
	/* first compute the angle of the first line segment(needed for arrow blk) */

		pt = l->pt2;
		if(i == 0 )
			{
			end_pt[0] = pt[0];
			end_pt[1] = pt[1];
			arrow_pt[0] = l->xyh[0];
			arrow_pt[1] = l->xyh[1];
			}
		for(j=0;j<2;j++) vec[j] = l->xyh[j] - pt[j];
		vec[2] = 0.0;

		/* cpp: take into account the special conditions
				in the CATIA file which I've never seen before
		*/

		if(fabs(vec[0]) < 1.0e-6 && fabs(vec[1]) <  1.0e-6) 
			{
			a_angle[i] = a_angle[i-1] + 3.14159;
			zero_length = UU_TRUE;
			}
		else
			{
			um_unitvc(vec,vec);
			a_angle[i] = um_angle2p(x_axis,vec,z_axis);
			}
		if(i == 1)
			{
			if(fabs(pt[0] - end_pt[0] ) < 1.0e-6 && fabs(pt[1] - end_pt[1]) < 1.0e-6)
				{
				a_angle[0] += 3.14159;
				e.arrow_blk[0].aangle += 3.14159;
				}
			}
	
	/* now load line block */

		for(j=0;j<2;j++) pt1[j] = l->xyh[j];
		pt1[2] = l->zt;
		uig_tran_coor(pt1,t,pt1);
		um_vctmsc(pt1,unit_scale,pt1);
		um_vctovc(pt1,a_org);
		for(j=0,k=0,mm=m_init;j<l->num_seg;j++,k++,k++,mm++,mm++)
			{
			if(i == 1 && zero_length)
				{
				pt2[0] = arrow_pt[0];
				pt2[1] = arrow_pt[1];
				pt2[2] = l->zt;
				}
			else
				{
				pt2[0] = pt[k];
				pt2[1] = pt[k+1];
				pt2[2] = l->zt;
				}
			uig_tran_coor(pt2,t,pt2);
			um_vctmsc(pt2,unit_scale,pt2);
			e.line_blk[l_cnt].line_seg[mm][0] = pt1[0];
			e.line_blk[l_cnt].line_seg[mm][1] = pt1[1];
			e.line_blk[l_cnt].line_seg[mm][2] = pt1[2];
			e.line_blk[l_cnt].line_seg[mm+1][0] = pt2[0];
			e.line_blk[l_cnt].line_seg[mm+1][1] = pt2[1];
			e.line_blk[l_cnt].line_seg[mm+1][2] = pt2[2];
			um_vctovc(pt2,pt1);
			}

	/**************************
	**                       **
	** load arrow-head block **
	**                       **
	**************************/

		a_cnt = e.arrow_blk_use;
		e.arrow_blk_use++;
		if (e.arrow_blk_use>10)  /*jkd2*/
			{
			sprintf(p_buff,"(DREC = %d) Number of arrows used > 10, = %d.\n", 
				dblk->drec_num, e.arrow_blk_use);
			uig_list_out(p_buff,UU_TRUE);
			}
		uig_arrow(l_form,&arrow_type);
		e.arrow_blk[a_cnt].arrow_type = arrow_type;
		um_vctovc(a_org,&e.arrow_blk[a_cnt].location[0]);
		e.arrow_blk[a_cnt].aangle = a_angle[i];
		e.arrow_blk[a_cnt].size = l->ah * unit_scale;

		}
	
	/**************************
	**                       **
	** process witness lines **
	**                       **
	**************************/

	w_first = UU_TRUE;
	for(i=0;i<2;i++)
		{
		switch(i)
			{
			case 0:
				w = w1;
				if(w != 0)
					{
					w_first = UU_FALSE;
					m_init = 0;
					l_cnt = e.line_blk_use;
					e.line_blk_use++;
					if (e.line_blk_use>5)  /*jkd2*/
						{
						sprintf(p_buff,"(DREC = %d) Number of lines used > 5, = %d.\n",
							dblk->drec_num, e.line_blk_use);	
						uig_list_out(p_buff,UU_TRUE);
						}
					e.line_blk[l_cnt].subtype = ext_line;
					e.line_blk[l_cnt].num_pts = w->num - 1;
					}
				break;
			case 1:
				w = w2;
				if(w != 0)
					{
					if(w_first)
						{
						w_first = UU_FALSE;
						m_init = 0;
						l_cnt = e.line_blk_use;
						e.line_blk_use++;
						if (e.line_blk_use>5) /*jkd*/
							{
							sprintf(p_buff,"(DREC = %d) Number of lines used > 5, = %d.\n",
								dblk->drec_num, e.line_blk_use);
							uig_list_out(p_buff,UU_TRUE);
							}
						e.line_blk[l_cnt].subtype = ext_line;
						e.line_blk[l_cnt].num_pts = w->num - 1;
						}
					else
						{
						m_init = e.line_blk[l_cnt].num_pts;
						e.line_blk[l_cnt].num_pts = e.line_blk[l_cnt].num_pts +
																w->num - 1;
						}
					}
				break;
			}
		if(w == 0) continue;
		pt = w->pt2;
		l_loc = 2;
		for(j=0,k=0,mm=m_init;j<(w->num - 2);j++,k++,k++,mm++,mm++)
			{
			pt1[0] = pt[l_loc];
			pt1[1] = pt[l_loc+1];
			pt2[0] = pt[l_loc+2];
			pt2[1] = pt[l_loc+3];
			pt1[2] = pt2[2] = w->zt;
			l_loc = l_loc + 4;
			uig_tran_coor(pt1,t,pt1);
			um_vctmsc(pt1,unit_scale,pt1);
			uig_tran_coor(pt2,t,pt2);
			um_vctmsc(pt2,unit_scale,pt2);
			e.line_blk[l_cnt].line_seg[mm][0] = pt1[0];
			e.line_blk[l_cnt].line_seg[mm][1] = pt1[1];
			e.line_blk[l_cnt].line_seg[mm][2] = pt1[2];
			e.line_blk[l_cnt].line_seg[mm+1][0] = pt2[0];
			e.line_blk[l_cnt].line_seg[mm+1][1] = pt2[1];
			e.line_blk[l_cnt].line_seg[mm+1][2] = pt2[2];
			}
		}

	/* check if creating a drawing entity */

	if(drw_flag)
		{
		/*
		uu_dprint(UU_MTRC,(us,"lindim org text plane x = %g %g %g\n", 
			e.cpln.xaxis[0], e.cpln.xaxis[1], e.cpln.xaxis[2]));
		uu_dprint(UU_MTRC,(us,"lindim org text plane y = %g %g %g\n", 
			e.cpln.yaxis[0], e.cpln.yaxis[1], e.cpln.yaxis[2]));
		uu_dprint(UU_MTRC,(us,"lindim org text plane z = %g %g %g\n", 
			e.cpln.zaxis[0], e.cpln.zaxis[1], e.cpln.zaxis[2]));
			*/
		uig_transform_drafting(&e, drw_v);
		uig_transform_drafting(&e, drw_s);
		uig_transform_drafting(&e, drw_t);
		/*
		uu_dprint(UU_MTRC,(us,"lindim transf text plane x = %g %g %g\n", 
			e.cpln.xaxis[0], e.cpln.xaxis[1], e.cpln.xaxis[2]));
		uu_dprint(UU_MTRC,(us,"lindim transf text plane y = %g %g %g\n", 
			e.cpln.yaxis[0], e.cpln.yaxis[1], e.cpln.yaxis[2]));
		uu_dprint(UU_MTRC,(us,"lindim transf text plane z = %g %g %g\n", 
			e.cpln.zaxis[0], e.cpln.zaxis[1], e.cpln.zaxis[2]));
			*/
		dblk->view_ptr = drw_ptr;
		}

	/* create Unibase record */

	uig_update_attr(dblk);
	uig_create_draft(&e,0,0,dblk->view_ptr);
	UIG_unibase_entities++;
	*key = e.key;
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    I_FUNCTION     :  uig_map_gnote(dblk,t,gnote,key)
**				Map a IGES general note to a Unibase note.
**    PARAMETERS   
**       INPUT  : 
**				dblk							directory block
**				t								IGES transformation matrix
**				gnote							note parameter block
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_gnote(dblk,t,g,key)
	struct dir_rec *dblk;				/*  directory record */
	UU_REAL t[12];
	struct IG_igesnote_rec *g;			/* parameter record */
	UU_KEY_ID *key;
	{

	struct UA_generic_draft e;

	int i,start,nc;
	int num,count;
	struct IG_gnote_rec *p;
	UU_REAL x_size;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_gnote"));

	/* load record with defaults */

	ua_init_entity(UA_NOTE_DIM,1,&e);
	e.rel_num = UA_LINEAR_DIMS_REL;

	/* load entity plane data */

	uig_set_dpln(t,&e);

	/*********************
	**                  **
	** load text blocks **
	**                  **
	*********************/

	num = g->no_gnote;
	p = g->gnote;
	if (num>10)  /*jkd2: cannot handle more than 10 */ 
		{
		sprintf(p_buff,
			"(DREC = %d) Number of notes = %d; set to 10.\n", dblk->drec_num, num);
		uig_list_out(p_buff,UU_TRUE);
		num = 10;
		}
	for(i=0;i<num;i++)
		{
		e.txt_blk[i].subtype = app_cre_txt;
		nc = p->str_cnt;
                if (nc > 0)
                   { 
		    e.txt_blk[i].char_cnt = nc;
		    count = strlen(&p->str[0]);
		    start = count - p->str_cnt;
		    strcpy(&e.txt_blk[i].tstring[0],&p->str[start]);
		    uig_str_adjust(i, &e, p->fc);
                   }
                else
                   {
		    e.txt_blk[i].char_cnt = 1;
                    strcpy(&e.txt_blk[i].tstring[0]," ");
                   }
		if(i == 0)
			{
			uig_tran_coor(&p->xyzt[0],t,&e.dim_origin[0]);
			um_vctmsc(&e.dim_origin[0],unit_scale,&e.dim_origin[0]);
			e.txt_blk[i].origin[0] = e.txt_blk[i].origin[1] 
					= e.txt_blk[i].origin[2] = 0.0;
			}
		else
			{
			uig_tran_coor(&p->xyzt[0],t,&e.txt_blk[i].origin[0]);
			um_vctmsc(&e.txt_blk[i].origin[0],unit_scale,&e.txt_blk[i].origin[0]);
			um_vcmnvc(&e.txt_blk[i].origin[0],&e.dim_origin[0],
						&e.txt_blk[i].origin[0]);
			}
		if (nc > 0)
                   {  
		    e.txt_blk[i].tangle =  -p->ang;    /* cpp: store correct text angle */
		    e.txt_blk[i].slant = p->sl;
		    e.txt_blk[i].dx = unit_scale * p->wt;
		    e.txt_blk[i].dy = unit_scale * p->ht;
                    e.txt_blk[i].txt_size = e.txt_blk[i].dy;

		/* cpp: add calculation of the character expansion factor */
		    x_size = 1.2*(e.txt_blk[i].dx/e.txt_blk[i].char_cnt);
		    e.txt_blk[i].char_expansion = x_size/e.txt_blk[i].txt_size;
                   }
		e.txt_blk_use++;
		if (e.txt_blk_use>10) /*jkd2: cannot handle more than 10 */
			{
			sprintf(p_buff,"(DREC = %d) Number of text strings > 10, = %d.\n", 
				dblk->drec_num, e.txt_blk_use);
			uig_list_out(p_buff,UU_TRUE);
			}
		p++;
		}

	/* check if creating a drawing entity */

	if(drw_flag)
		{
		uig_transform_drafting(&e, drw_v);
		uig_transform_drafting(&e, drw_s);
		uig_transform_drafting(&e, drw_t);
		dblk->view_ptr = drw_ptr;
		}

	/* create Unibase record */

	uig_update_attr(dblk);
	uig_create_draft(&e,0,0,dblk->view_ptr);
	UIG_unibase_entities++;
	*key = e.key;

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    I_FUNCTION     :  uig_map_diadim(dblk,t,gnote,l1_form,l1,
**													l2_form,l2,key)
**				Map a IGES diameter dim to a Unibase diameter dim.
**    PARAMETERS   
**       INPUT  : 
**				dblk							directory block
**				t								IGES transformation matrix
**				gnote							 associated note
**				l1_form                  first leader form number
**				l1								 first leader 
**				l2_form                  second leader form number
**				l2								 second leader 
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_diadim(dblk,t,g,l1_form,l1,l2_form,l2,key)
	struct dir_rec *dblk;				/*  directory record */
	UU_REAL t[12];
	struct IG_igesnote_rec *g;
	int l1_form;
	struct IG_igeslead_rec *l1;
	int l2_form;
	struct IG_igeslead_rec *l2;
	UU_KEY_ID *key;
	{

	struct UA_generic_draft e;

	int i,j,k,start,arrow_type,istat;
	int num,count,nc,l_cnt,a_cnt,l_form;
	UU_TRUEDOUBLE ff;
	UU_REAL pt1[3],pt2[3],a_org[3];
	UU_REAL *pt;
	struct IG_gnote_rec *p;
	struct IG_igeslead_rec *l;
	UU_REAL a_angle,vec[3];
	UU_REAL um_angle2p();
	UU_TRUEDOUBLE uig_atof();
	char tmpstr[256];	/*jkd53 */

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_diadim"));

	/* load record with defaults */

	ua_init_entity(UA_DIA_IN_DIM,1,&e);
	e.rel_num = UA_LINEAR_DIMS_REL;

	/* load entity plane data */

	uig_set_dpln(t,&e);

	/*********************
	**                  **
	** load text blocks **
	**                  **
	*********************/

	p = g->gnote;
	num = g->no_gnote;
	if (num>10)  /*jkd2: cannot handle more than 10 */ 
		{
		sprintf(p_buff,
			"(DREC = %d) Number of notes in diameter dimension = %d; set to 10.\n", 
			dblk->drec_num, num);
		uig_list_out(p_buff,UU_TRUE);
		num = 10;
		}
	for(i=0;i<num;i++)
		{
		e.txt_blk[i].subtype = main_txt1;
		nc = p->str_cnt;
                if (nc > 0)
                  {
		   e.txt_blk[i].char_cnt = p->str_cnt;
		   count = strlen(&p->str[0]);
		   start = count - p->str_cnt;
		   strcpy(&e.txt_blk[i].tstring[0],&p->str[start]);
		   uig_str_adjust(i,&e, p->fc);
                  }
                else
                  {
	   	   e.txt_blk[i].char_cnt = 1;
                   strcpy(&e.txt_blk[i].tstring[0]," ");
                  }
		if(i == 0)
			{
			istat = uig_numeric_str(&p->str[start],tmpstr);
			if (istat == UU_SUCCESS)
				ff = uig_atof(tmpstr);
			else
				ff = 1.0;
			ff = unit_scale * ff;
			e.dim_value = ff;
			uig_tran_coor(&p->xyzt[0],t,&e.dim_origin[0]);
			um_vctmsc(&e.dim_origin[0],unit_scale,&e.dim_origin[0]);
			e.txt_blk[i].origin[0] = e.txt_blk[i].origin[1] 
					= e.txt_blk[i].origin[2] = 0.0;
			}
		else
			{
			uig_tran_coor(&p->xyzt[0],t,&e.txt_blk[i].origin[0]);
			um_vctmsc(&e.txt_blk[i].origin[0],unit_scale,&e.txt_blk[i].origin[0]);
			um_vcmnvc(&e.txt_blk[i].origin[0],&e.dim_origin[0],
						&e.txt_blk[i].origin[0]);
			}
                if (nc > 0)
                  {
		   e.txt_blk[i].dx = unit_scale * p->wt;
		   e.txt_blk[i].dy = unit_scale * p->ht;
		   e.txt_blk[i].slant = p->sl;
		   e.txt_blk[i].tangle =  -p->ang;        /* cpp: store correct angle */
		   e.txt_blk[i].txt_size = e.txt_blk[i].dy;
                  }
		e.txt_blk_use++;
		if (e.txt_blk_use>10)  /*jkd2: cannot handle more than 10 */
			{
			sprintf(p_buff,
				"(DREC = %d) Number of text strings in diameter dimension > 10, = %d.\n", 
				dblk->drec_num, e.txt_blk_use);
			uig_list_out(p_buff,UU_TRUE);
			}
		p++;
		}
	
	/*************************
	**                      **
	** process leader lines **
	**                      **
	*************************/

	for(i=0;i<2;i++)
		{
		switch(i)
			{
			case 0:
				l = l1;
				l_form = l1_form;
				break;
			case 1:
				l = l2;
				l_form = l2_form;
				break;
			}

	/* check if leader line exits */
		if(l_form == -1) continue;
	
	/* first compute the angle of the first line segment(needed for arrow blk) */
		pt = l->pt2;
		for(j=0;j<2;j++) vec[j] = l->xyh[j] - pt[j];
		vec[2] = 0.0;
		um_unitvc(vec,vec);
		a_angle = um_angle2p(x_axis,vec,z_axis);
	
	/* now load line block */

		pt1[2] = l->zt;
		uig_tran_coor(pt1,t,pt1);
		um_vctmsc(pt1,unit_scale,pt1);
		um_vctovc(pt1,a_org);
		l_cnt = e.line_blk_use;
		e.line_blk_use++;
		if (e.line_blk_use>5) /*jkd2*/
			{
			sprintf(p_buff,
				"(DREC = %d) Number of lines use in diameter dimension > 5, = %d.\n", 
				dblk->drec_num, e.line_blk_use);
			uig_list_out(p_buff,UU_TRUE);
			}
		e.line_blk[l_cnt].subtype = dim_line;
		e.line_blk[l_cnt].num_pts = 2 * l->num_seg;
		for(j=0,k=0;j<l->num_seg;j++,k++,k++)
			{
			pt2[0] = pt[k];
			pt2[1] = pt[k+1];
			pt2[2] = l->zt;
			uig_tran_coor(pt2,t,pt2);
			um_vctmsc(pt2,unit_scale,pt2);
			e.line_blk[l_cnt].line_seg[k][0] = pt1[0];
			e.line_blk[l_cnt].line_seg[k][1] = pt1[1];
			e.line_blk[l_cnt].line_seg[k][2] = pt1[2];
			e.line_blk[l_cnt].line_seg[k+1][0] = pt2[0];
			e.line_blk[l_cnt].line_seg[k+1][1] = pt2[1];
			e.line_blk[l_cnt].line_seg[k+1][2] = pt2[2];
			um_vctovc(pt2,pt1);
			}

	/**************************
	**                       **
	** load arrow-head block **
	**                       **
	**************************/

		a_cnt = e.arrow_blk_use;
		e.arrow_blk_use++;
		if (e.arrow_blk_use>10) /*jkd2*/
			{
			sprintf(p_buff,
				"(DREC = %d) Number of arrows used > 10, = %d.\n", 
				dblk->drec_num, e.arrow_blk_use);
			uig_list_out(p_buff,UU_TRUE);
			}
		uig_arrow(l_form,&arrow_type);
		e.arrow_blk[a_cnt].arrow_type = arrow_type;
		um_vctovc(a_org,&e.arrow_blk[a_cnt].location[0]);
		e.arrow_blk[a_cnt].aangle = a_angle;
		e.arrow_blk[a_cnt].size = l->ah * unit_scale;
		}

	/* check if creating a drawing entity */

	if(drw_flag)
		{
		uig_transform_drafting(&e, drw_v);
		uig_transform_drafting(&e, drw_s);
		uig_transform_drafting(&e, drw_t);
		dblk->view_ptr = drw_ptr;
		}

	/* create Unibase record */

	uig_update_attr(dblk);
	uig_create_draft(&e,0,0,dblk->view_ptr);
	UIG_unibase_entities++;
	*key = e.key;

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    I_FUNCTION     :  uig_arrow(form,type)
**				Map arrow-head form number.
**    PARAMETERS   
**       INPUT  : 
**				form							IGES form number
**				type							UNICAD drafting form number
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_arrow(form,type)
	int form;
	int *type;
	{

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_arrow"));

	switch(form)
		{
		case 1:
			*type = UA_SINGLE_OPEN;
			break;
		case 2:
			*type = UA_SINGLE_CLOSED;
			break;
		case 3:
			/************************
			**
			**  temporary 
			**
			**   *type = UA_SINGLE_FILLED;
			**
			*************************/
			*type = UA_SINGLE_CLOSED;
			break;
		case 4:
			*type = UA_POINT;
			break;
		case 5:
			*type = UA_NODE;
			break;
		case 6:
			*type = UA_NODE;
			break;
		case 7:
			*type = UA_NODE;
			break;
		case 8:
			*type = UA_NODE;
			break;
		case 9:
			*type = UA_TILDE;
			break;
		case 10:
			*type = UA_TILDE;
			break;
		}

	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_map_radim(dblk,t,gnote,l_form,l,key)
**				Map a IGES radius dim to a Unibase radius dim.
**    PARAMETERS   
**       INPUT  : 
**				dblk							directory block
**				t								IGES transformation matrix
**				gnote							associated note
**				l_form                  leader form number
**				l								leader 
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_radim(dblk,t,g,l_form,l,key)
	struct dir_rec *dblk;				/*  directory record */
	UU_REAL t[12];
	struct IG_igesnote_rec *g;
	int l_form;
	struct IG_igeslead_rec *l;
	UU_KEY_ID *key;
	{

	struct UA_generic_draft e;

	int i,j,k,start,arrow_type, istat;
	int num,nc,count,l_cnt,a_cnt;
	UU_TRUEDOUBLE ff;
	UU_REAL pt1[3],pt2[3],a_org[3];
	UU_REAL *pt;
	struct IG_gnote_rec *p;
	UU_REAL a_angle,vec[3];
	UU_REAL um_angle2p();
	UU_TRUEDOUBLE uig_atof();
	char tmpstr[20];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_radim"));

	/* load record with defaults */

	ua_init_entity(UA_RAD_CEN_DIM,1,&e);
	e.rel_num = UA_LINEAR_DIMS_REL;

	/* load entity plane data */

	uig_set_dpln(t,&e);

	/*********************
	**                  **
	** load text blocks **
	**                  **
	*********************/

	num = g->no_gnote;
	p = g->gnote;
	if (num>10)  /*jkd2: cannot handle more than 10 */ 
		{
		sprintf(p_buff,
			"(DREC = %d) Number of notes in radial dimension = %d; set to 10.\n", 
			dblk->drec_num, num);
		uig_list_out(p_buff,UU_TRUE);
		num = 10;
		}
	for(i=0;i<num;i++)
		{
		e.txt_blk[i].subtype = main_txt1;
		nc = p->str_cnt;
                if (nc > 0)
                  {
		   e.txt_blk[i].char_cnt = p->str_cnt;
		   count = strlen(&p->str[0]);
		   start = count - p->str_cnt;
		   strcpy(&e.txt_blk[i].tstring[0],&p->str[start]);
		   uig_str_adjust(i,&e, p->fc);
		   istat = uig_numeric_str(&p->str[start],tmpstr);
		   if (istat == UU_SUCCESS)
		   	ff = uig_atof(tmpstr);
		   else
  		   	ff = 1.0;
		   ff = unit_scale * ff;
		   e.dim_value = ff;
		   }
                else
                  {
		   e.txt_blk[i].char_cnt = 1;
                   strcpy(&e.txt_blk[i].tstring[0]," ");
                  }
		if(i == 0)
			{
			uig_tran_coor(&p->xyzt[0],t,&e.dim_origin[0]);
			um_vctmsc(&e.dim_origin[0],unit_scale,&e.dim_origin[0]);
			e.txt_blk[i].origin[0] = e.txt_blk[i].origin[1] 
					= e.txt_blk[i].origin[2] = 0.0;
			}
		else
			{
			uig_tran_coor(&p->xyzt[0],t,&e.txt_blk[i].origin[0]);
			um_vctmsc(&e.txt_blk[i].origin[0],unit_scale,&e.txt_blk[i].origin[0]);
			um_vcmnvc(&e.txt_blk[i].origin[0],&e.dim_origin[0],
						&e.txt_blk[i].origin[0]);
			}
                if (nc > 0)
                  {
		   e.txt_blk[i].dx = unit_scale * p->wt;
		   e.txt_blk[i].dy = unit_scale * p->ht;
		   e.txt_blk[i].slant = p->sl;
		   e.txt_blk[i].tangle =  -p->ang;
		   e.txt_blk[i].txt_size = e.txt_blk[i].dy;
                  }
		e.txt_blk_use++;
		if (e.txt_blk_use>10) /*jkd2*/
			{
			sprintf(p_buff,
				"(DREC = %d) Number of text strings in radial dimension > 10, = %d.\n", 
				dblk->drec_num, e.txt_blk_use);
			uig_list_out(p_buff,UU_TRUE);
			}
		p++;
		}
	
	/*************************
	**                      **
	** process leader line  **
	**                      **
	*************************/

	/* first compute the angle of the first line segment(needed for arrow blk) */

	pt = l->pt2;
	for(j=0;j<2;j++) vec[j] = l->xyh[j] - pt[j];
	vec[2] = 0.0;
	um_unitvc(vec,vec);
	a_angle = um_angle2p(x_axis,vec,z_axis);
	
	/* now load line block */

	for(j=0;j<2;j++) pt1[j] = l->xyh[j];
	pt1[2] = l->zt;
	uig_tran_coor(pt1,t,pt1);
	um_vctmsc(pt1,unit_scale,pt1);
	um_vctovc(pt1,a_org);
	l_cnt = e.line_blk_use;
	e.line_blk_use++;
	if (e.line_blk_use>5) /*jkd2*/
		{
		sprintf(p_buff,
			"(DREC = %d) Number of lines use in radial dimension > 5, = %d.\n", 
			dblk->drec_num, e.line_blk_use);
		uig_list_out(p_buff,UU_TRUE);
		}
	e.line_blk[l_cnt].subtype = dim_line;
	e.line_blk[l_cnt].num_pts = 2 * l->num_seg;
	for(j=0,k=0;j<l->num_seg;j++,k++,k++)
		{
		pt2[0] = pt[k];
		pt2[1] = pt[k+1];
		pt2[2] = l->zt;
		uig_tran_coor(pt2,t,pt2);
		um_vctmsc(pt2,unit_scale,pt2);
		e.line_blk[l_cnt].line_seg[k][0] = pt1[0];
		e.line_blk[l_cnt].line_seg[k][1] = pt1[1];
		e.line_blk[l_cnt].line_seg[k][2] = pt1[2];
		e.line_blk[l_cnt].line_seg[k+1][0] = pt2[0];
		e.line_blk[l_cnt].line_seg[k+1][1] = pt2[1];
		e.line_blk[l_cnt].line_seg[k+1][2] = pt2[2];
		um_vctovc(pt2,pt1);
		}

	/**************************
	**                       **
	** load arrow-head block **
	**                       **
	**************************/

	a_cnt = e.arrow_blk_use;
	e.arrow_blk_use++;
	if (e.arrow_blk_use>10) /*jkd2*/
		{
		sprintf(p_buff,
			"(DREC = %d) Number of arrows used > 10, = %d.\n", 
			dblk->drec_num, e.arrow_blk_use);
		uig_list_out(p_buff,UU_TRUE);
		}
	uig_arrow(l_form,&arrow_type);
	e.arrow_blk[a_cnt].arrow_type = arrow_type;
	um_vctovc(a_org,&e.arrow_blk[a_cnt].location[0]);
	e.arrow_blk[a_cnt].aangle = a_angle;
	e.arrow_blk[a_cnt].size = l->ah * unit_scale;

	/* check if creating a drawing entity */

	if(drw_flag)
		{
		uig_transform_drafting(&e, drw_v);
		uig_transform_drafting(&e, drw_s);
		uig_transform_drafting(&e, drw_t);
		dblk->view_ptr = drw_ptr;
		}

	/* create Unibase record */

	uig_update_attr(dblk);
	uig_create_draft(&e,0,0,dblk->view_ptr);
	UIG_unibase_entities++;
	*key = e.key;
	return(UU_SUCCESS);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_map_leader(dblk,l,t,key)
**				Map a IGES Leader dim to a Unibase leader dim.
**    PARAMETERS   
**       INPUT  : 
**				dblk							directory block
**				l								leader 
**				t								IGES transformation matrix
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_leader(dblk,l,t,key)
	struct dir_rec *dblk;				/*  directory record */
	struct IG_igeslead_rec *l;
	UU_REAL t[12];
	UU_KEY_ID *key;
	{

	struct UA_generic_draft e;

	int j,k,arrow_type, l_form;
	int l_cnt,a_cnt;
	UU_REAL pt1[3],pt2[3],a_org[3];
	UU_REAL *pt;
	UU_REAL a_angle,vec[3];
	UU_REAL um_angle2p();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_radim"));

	/* load record with defaults */

	ua_init_entity(UA_LINEAR_DIM,1,&e);
	e.rel_num = UA_LINEAR_DIMS_REL;

	/* load entity plane data */

	uig_set_dpln(t,&e);

	/*************************
	**                      **
	** process leader line  **
	**                      **
	*************************/

	l_form = dblk->form_no;

	/* first compute the angle of the first line segment(needed for arrow blk) */
	pt = l->pt2;
	for(j=0;j<2;j++) vec[j] = l->xyh[j] - pt[j];
	vec[2] = 0.0;
	um_unitvc(vec,vec);
	a_angle = um_angle2p(x_axis,vec,z_axis);
	
	/* now load line block */
	for(j=0;j<2;j++) pt1[j] = l->xyh[j];
	pt1[2] = l->zt;
	uig_tran_coor(pt1,t,pt1);
	um_vctmsc(pt1,unit_scale,pt1);
	um_vctovc(pt1,a_org);
	l_cnt = e.line_blk_use;
	e.line_blk_use++;
	if (e.line_blk_use>5)  /*jkd2*/
		{
		sprintf(p_buff,
			"(DREC = %d) Number of lines use in leader > 5, = %d.\n", 
			dblk->drec_num, e.line_blk_use);
		uig_list_out(p_buff,UU_TRUE);
		}
	e.line_blk[l_cnt].subtype = dim_line;
	e.line_blk[l_cnt].num_pts = 2 * l->num_seg;
	for(j=0,k=0;j<l->num_seg;j++,k++,k++)
		{
		pt2[0] = pt[k];
		pt2[1] = pt[k+1];
		pt2[2] = l->zt;
		uig_tran_coor(pt2,t,pt2);
		um_vctmsc(pt2,unit_scale,pt2);
		e.line_blk[l_cnt].line_seg[k][0] = pt1[0];
		e.line_blk[l_cnt].line_seg[k][1] = pt1[1];
		e.line_blk[l_cnt].line_seg[k][2] = pt1[2];
		e.line_blk[l_cnt].line_seg[k+1][0] = pt2[0];
		e.line_blk[l_cnt].line_seg[k+1][1] = pt2[1];
		e.line_blk[l_cnt].line_seg[k+1][2] = pt2[2];
		um_vctovc(pt2,pt1);
		}

	/**************************
	**                       **
	** load arrow-head block **
	**                       **
	**************************/

	a_cnt = e.arrow_blk_use;
	e.arrow_blk_use++;
	if (e.arrow_blk_use>10)  /*jkd2*/
		{
		sprintf(p_buff,
			"(DREC = %d) Number of arrows used in leader > 10, = %d.\n", 
			dblk->drec_num, e.arrow_blk_use);
		uig_list_out(p_buff,UU_TRUE);
		}
	uig_arrow(l_form,&arrow_type);
	e.arrow_blk[a_cnt].arrow_type = arrow_type;
	um_vctovc(a_org,&e.arrow_blk[a_cnt].location[0]);
	e.arrow_blk[a_cnt].aangle = a_angle;
	e.arrow_blk[a_cnt].size = l->ah * unit_scale;

	/* check if creating a drawing entity */

	if(drw_flag)
		{
		uig_transform_drafting(&e, drw_v);
		uig_transform_drafting(&e, drw_s);
		uig_transform_drafting(&e, drw_t);
		dblk->view_ptr = drw_ptr;
		}

	/* create Unibase record */

	uig_update_attr(dblk);
	uig_create_draft(&e,0,0,dblk->view_ptr);
	UIG_unibase_entities++;
	*key = e.key;
	return(UU_SUCCESS);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_set_dpln(t,e)
**       Load IGES construction plane definition into the UNIBASE drafting
**       entity.
**    PARAMETERS   
**       INPUT  : 
**          t									IGES transformation matrix
**       OUTPUT :  
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_set_dpln(t,e)
	UU_REAL	t[12];
	struct	UA_generic_draft	(*e);
	{
	UU_REAL vec[3];
	int i,j;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_set_dpln"));

	for(i=1;i<5;i++)
		{
		uig_trans_comp(t,i,vec);
		switch(i)
			{
			case 1:
				for(j=0;j<3;j++)
					{
					(*e).cpln.xaxis[j] = vec[j];
					}
				break;
			case 2:
				for(j=0;j<3;j++)
					{
					(*e).cpln.yaxis[j] = vec[j];
					}
				break;
			case 3:
				for(j=0;j<3;j++)
					{
					(*e).cpln.zaxis[j] = vec[j];
					}
				break;
			case 4:
				for(j=0;j<3;j++)
					{
					(*e).cpln.cpln_origin[j] = vec[j];
					}
				break;
			}
		}
	return(UU_SUCCESS);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_map_angdim(dblk,t,gnote,l1_form,l1,
**													l2_form,l2,w2,w2,center,rad,key)
**				Map a IGES angular dim to a Unibase angular dim.
**    PARAMETERS   
**       INPUT  : 
**				dblk							directory block
**				t								IGES transformation matrix
**				gnote							 associated note
**				l1_form                  first leader form number
**				l1								 first leader 
**				l2_form                  second leader form number
**				l2								 second leader 
**				w1								 first witness line
**				w2								 second witness line
**				center						 center of the arc's
**				rad							 radius of the arc's
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_angdim(dblk,t,g,l1_form,l1,l2_form,l2,w1,w2,center,rad,key)
	struct dir_rec *dblk;				/*  directory record */
	UU_REAL t[12];
	struct IG_igesnote_rec *g;
	int l1_form;
	struct IG_igeslead_rec *l1;
	int l2_form;
	struct IG_igeslead_rec *l2;
	struct IG_poly2d_rec *w1;
	struct IG_poly2d_rec *w2;
	UU_REAL center[3], rad;
	UU_KEY_ID *key;
	{

	struct UA_generic_draft e;

	int i,j,k,start,arrow_type,mm,nc,m_init,istat;
	int num,count,l_cnt,l_loc,a_cnt,l_form,ar_cnt;
	UU_TRUEDOUBLE ff;
	UU_LOGICAL w_first;
	UU_REAL pt1[3],pt2[3],a_org[3];
	UU_REAL spt[3],ept[3],spt_cc[3],ept_cc[3],v1[3],v2[3],v3[3],v4[3];
	UU_REAL *pt;
	struct IG_gnote_rec *p;
	struct IG_igeslead_rec *l;
	struct IG_poly2d_rec *w;
	UU_REAL a_angle,vertex[3];
	UU_REAL radius,size,ang,sine,cosine,sign,ang1,ang2, x_size;
	UU_REAL um_angle2p();
#if UU_COMP!=UU_RIDGE
	UU_REAL sin(),cos();
#endif

	UU_LOGICAL uig_cceqcc();
	UU_TRUEDOUBLE uig_atof();
	char tmpstr[256];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_radim"));

	/* check data */

	if(rad < 0.0001 )
		{
		printf(" zero radius specified for angular dimension \n");
		uu_dexit;
		return(UU_FAILURE);
		}

	/* load record with defaults */

	ua_init_entity(UA_ANGULAR_DIM,1,&e);
	e.rel_num = UA_LINEAR_DIMS_REL;

	/* load entity plane data */

	uig_set_dpln(t,&e);
	/* phil's debugging?
	ua_dump_set(0xffff);
	*/

	/*********************
	**                  **
	** load text blocks **
	**                  **
	*********************/

	num = g->no_gnote;
	p = g->gnote;
	if (num>10)  /*jkd2: cannot handle more than 10 */ 
		{
		sprintf(p_buff,
			"(DREC = %d) Number of notes in angular dimension = %d; set to 10.\n", 
			dblk->drec_num, num);
		uig_list_out(p_buff,UU_TRUE);
		num = 10;
		}
	for(i=0;i<num;i++)
		{
		e.txt_blk[i].subtype = main_txt1;
		nc = p->str_cnt;
                if (nc > 0)
                  {
		   e.txt_blk[i].char_cnt = p->str_cnt;
		   count = strlen(&p->str[0]);
		   start = count - p->str_cnt;
		   strcpy(&e.txt_blk[i].tstring[0],&p->str[start]);
		   uig_str_adjust(i,&e, p->fc);
                  }
                else
                  {
		   e.txt_blk[i].char_cnt = 1;
                   strcpy(&e.txt_blk[i].tstring[0]," ");
                  }
		if(i == 0)
			{
			istat = uig_numeric_str(&p->str[start],tmpstr);
			if (istat == UU_SUCCESS)
				ff = uig_atof(tmpstr);
			else
				ff = 1.0;
			ff = unit_scale * ff;
			e.dim_value = ff;
			uig_tran_coor(&p->xyzt[0],t,&e.dim_origin[0]);
			um_vctmsc(&e.dim_origin[0],unit_scale,&e.dim_origin[0]);
			e.txt_blk[i].origin[0] = e.txt_blk[i].origin[1] 
					= e.txt_blk[i].origin[2] = 0.0;
			}
		else
			{
			uig_tran_coor(&p->xyzt[0],t,&e.txt_blk[i].origin[0]);
			um_vctmsc(&e.txt_blk[i].origin[0],unit_scale,&e.txt_blk[i].origin[0]);
			um_vcmnvc(&e.txt_blk[i].origin[0],&e.dim_origin[0],
						&e.txt_blk[i].origin[0]);
			}
                if (nc > 0)
                  {
		   e.txt_blk[i].dx = unit_scale * p->wt;
		   e.txt_blk[i].dy = unit_scale * p->ht;
		   e.txt_blk[i].slant = p->sl;
		   e.txt_blk[i].tangle =  -p->ang;
		   e.txt_blk[i].txt_size = e.txt_blk[i].dy;

		/* cpp: add calculation of the character expansion factor */
		   x_size = 1.2*(e.txt_blk[i].dx/e.txt_blk[i].char_cnt);
		   e.txt_blk[i].char_expansion = x_size/e.txt_blk[i].txt_size;
                  }
		e.txt_blk_use++;
		if (e.txt_blk_use>10) /*jkd2*/
			{
			sprintf(p_buff,
				"(DREC = %d) Number of text strings in angular dimension > 10, = %d.\n", 
				dblk->drec_num, e.txt_blk_use);
			uig_list_out(p_buff,UU_TRUE);
			}
		p++;
		}
	
	/*************************
	**                      **
	** process leader lines **
	**                      **
	*************************/

	if(l1 == 0) goto witness;
	center[2] = l1->zt;
	uig_tran_coor(center,t,pt1);
	um_vctmsc(pt1,unit_scale,vertex);
	radius = rad * unit_scale;

	for(i=0;i<2;i++)
		{
		switch(i)
			{
			case 0:
				l = l1;
				l_form = l1_form;
				sign = 1.0;
				ar_cnt = e.arc_blk_use;
				e.arc_blk_use++;
				m_init = 0;
				e.arc_blk[ar_cnt].num_pts = 0;
				um_vctovc(vertex,e.arc_blk[ar_cnt].center_pt);
				e.arc_blk[ar_cnt].radius = radius;
				break;
			case 1:
				if(l2 == 0) goto witness;
				l = l2;
				l_form = l2_form;
				sign = -1.0;
				m_init = e.arc_blk[ar_cnt].num_pts;
				break;
			}
	
	/* first compute the angle of the arrow-head  */

		pt = l->pt2;
		size = l->ah*unit_scale;
		ang = size/radius;
		sine = sin(ang);
		cosine = cos(ang);

		for(j=0;j<2;j++) spt_cc[j] = l->xyh[j];
		spt_cc[2] = l->zt;

		um_vcmnvc(spt_cc,center,v1);
		um_cross(z_axis,v1,v2);

		pt1[0] = sine*sign*v2[0] + cosine*v1[0] + center[0];
		pt1[1] = sine*sign*v2[1] + cosine*v1[1] + center[1];
		pt1[2] = sine*sign*v2[2] + cosine*v1[2] + center[2];

		um_vcmnvc(pt1,center,v4);
		if(sign > 0.0 )
			{
			um_cross(v1,z_axis,v3);
			um_cross(v4,z_axis,v1);
			}
		else
			{
			um_vctovc(v2, v3);
			um_cross(z_axis, v4, v1);
			}

		ang1 = um_angle2p(x_axis,v1,z_axis);
		ang2 = um_angle2p(x_axis,v3,z_axis);
		a_angle = (ang1 + ang2)/2.0;
		uig_tran_coor(spt_cc,t,a_org);
		um_vctmsc(a_org,unit_scale,a_org);
		um_vctovc(a_org,spt);

		/**************************
		**                       **
		** load arc	block		 **
		**                       **
		**************************/

		for(j=0,k=0,mm=m_init;j<l->num_seg;j++,k++,k++,mm++,mm++)
			{
			ept_cc[0] = pt[k];
			ept_cc[1] = pt[k+1];
			ept_cc[2] = l->zt;
			uig_tran_coor(ept_cc,t,ept);
			um_vctmsc(ept,unit_scale,ept);
			if(uig_cceqcc(spt,ept)) continue;
			if(j > 1)
				{
				/* create a line blk */

				l_cnt = e.line_blk_use;
				e.line_blk_use++;
				if (e.line_blk_use>5)  /*jkd2*/
					{
					sprintf(p_buff,
						"(DREC = %d) Number of lines use in angular dimension > 5, = %d.\n", 
						dblk->drec_num, e.line_blk_use);
					uig_list_out(p_buff,UU_TRUE);
					}
				e.line_blk[l_cnt].subtype = dim_line;
				e.line_blk[l_cnt].num_pts = 2;
				e.line_blk[l_cnt].line_seg[0][0] = spt[0];
				e.line_blk[l_cnt].line_seg[0][1] = spt[1];
				e.line_blk[l_cnt].line_seg[0][2] = spt[2];
				e.line_blk[l_cnt].line_seg[1][0] = ept[0];
				e.line_blk[l_cnt].line_seg[1][1] = ept[1];
				e.line_blk[l_cnt].line_seg[1][2] = ept[2];
				}
			else
				{
				um_vcmnvc(spt_cc,center,v1);
				um_vcmnvc(ept_cc,center,v2);
				ang1 = um_angle2p(x_axis,v1,z_axis);
				ang2 = um_angle2p(x_axis,v2,z_axis);
				switch(i)
					{
					case 0:
						switch(j)
							{
							case 0:
								e.arc_blk[ar_cnt].angles[mm] = ang1;
								e.arc_blk[ar_cnt].angles[mm+1] = ang2;
								break;
							case 1:
								e.arc_blk[ar_cnt].angles[mm] = ang2;
								e.arc_blk[ar_cnt].angles[mm+1] = ang1;
								break;
							}
						break;
					case 1:
						switch(j)
							{
							case 0:
								e.arc_blk[ar_cnt].angles[mm] = ang2;
								e.arc_blk[ar_cnt].angles[mm+1] = ang1;
								break;
							case 1:
								e.arc_blk[ar_cnt].angles[mm] = ang1;
								e.arc_blk[ar_cnt].angles[mm+1] = ang2;
								break;
							}
						break;
					}
				e.arc_blk[ar_cnt].num_pts = e.arc_blk[ar_cnt].num_pts + 2;
				}
			um_vctovc(ept,spt);
			um_vctovc(ept_cc,spt_cc);
			}
						
		/**************************
		**                       **
		** load arrow-head block **
		**                       **
		**************************/

		a_cnt = e.arrow_blk_use;
		e.arrow_blk_use++;
		if (e.arrow_blk_use>10)  /*jkd2*/
			{
			sprintf(p_buff,
				"(DREC = %d) Number of arrows used in angular dimension > 10, = %d.\n", 
				dblk->drec_num, e.arrow_blk_use);
			uig_list_out(p_buff,UU_TRUE);
			}
		uig_arrow(l_form,&arrow_type);
		e.arrow_blk[a_cnt].arrow_type = arrow_type;
		um_vctovc(a_org,&e.arrow_blk[a_cnt].location[0]);
		e.arrow_blk[a_cnt].aangle = a_angle;
		e.arrow_blk[a_cnt].size = size;

		}
	
	/**************************
	**                       **
	** process witness lines **
	**                       **
	**************************/

witness:
	w_first = UU_TRUE;
	for(i=0;i<2;i++)
		{
		switch(i)
			{
			case 0:
				w = w1;
				if(w != 0)
					{
					w_first = UU_FALSE;
					m_init = 0;
					l_cnt = e.line_blk_use;
					e.line_blk_use++;
					if (e.line_blk_use>5)  /*jkd2*/
						{
						sprintf(p_buff,
							"(DREC = %d) Number of lines use in angular dimension > 5, = %d.\n", 
							dblk->drec_num, e.line_blk_use);
						uig_list_out(p_buff,UU_TRUE);
						}
					e.line_blk[l_cnt].subtype = ext_line;
					e.line_blk[l_cnt].num_pts = w->num - 1;
					}
				break;
			case 1:
				w = w2;
				if(w != 0)
					{
					if(w_first)
						{
						w_first = UU_FALSE;
						m_init = 0;
						l_cnt = e.line_blk_use;
						e.line_blk_use++;
						if (e.line_blk_use>5)  /*jkd2*/
							{
							sprintf(p_buff,
								"(DREC = %d) Number of lines use in angular dimension > 5, = %d.\n", 
								dblk->drec_num, e.line_blk_use);
							uig_list_out(p_buff,UU_TRUE);
							}
						e.line_blk[l_cnt].subtype = ext_line;
						e.line_blk[l_cnt].num_pts = w->num - 1;
						}
					else
						{
						m_init = e.line_blk[l_cnt].num_pts;
						e.line_blk[l_cnt].num_pts = e.line_blk[l_cnt].num_pts +
																w->num - 1;
						}
					}
				break;
			}
		if(w == 0) continue;
		pt = w->pt2;
		l_loc = 2;
		for(j=0,k=0,mm=m_init;j<(w->num - 2);j++,k++,k++,mm++,mm++)
			{
			pt1[0] = pt[l_loc];
			pt1[1] = pt[l_loc+1];
			pt2[0] = pt[l_loc+2];
			pt2[1] = pt[l_loc+3];
			pt1[2] = pt2[2] = w->zt;
			l_loc = l_loc + 4;
			uig_tran_coor(pt1,t,pt1);
			um_vctmsc(pt1,unit_scale,pt1);
			uig_tran_coor(pt2,t,pt2);
			um_vctmsc(pt2,unit_scale,pt2);
			e.line_blk[l_cnt].line_seg[mm][0] = pt1[0];
			e.line_blk[l_cnt].line_seg[mm][1] = pt1[1];
			e.line_blk[l_cnt].line_seg[mm][2] = pt1[2];
			e.line_blk[l_cnt].line_seg[mm+1][0] = pt2[0];
			e.line_blk[l_cnt].line_seg[mm+1][1] = pt2[1];
			e.line_blk[l_cnt].line_seg[mm+1][2] = pt2[2];
			}
		}

	/* check if creating a drawing entity */

	if(drw_flag)
		{
		uig_transform_drafting(&e, drw_v);
		uig_transform_drafting(&e, drw_s);
		uig_transform_drafting(&e, drw_t);
		dblk->view_ptr = drw_ptr;
		}

	/* temporary fix for large angles */
	e.cpln.cpln_origin[0] = 0.0;
	e.cpln.cpln_origin[1] = 0.0;
	e.cpln.cpln_origin[2] = 0.0;

	/* create Unibase record */

	uig_update_attr(dblk);
	uig_create_draft(&e,0,0,dblk->view_ptr);
	UIG_unibase_entities++;
	*key = e.key;

	uu_dexit;
	return(UU_SUCCESS);
	}
/*********************************************************************
**    I_FUNCTION     :  uig_str_adjust(i,e, font)
**       Check text block for special characters
**    PARAMETERS   
**       INPUT  : 
**          i									Text block number
**          e									Drafting entity record
**          font								font number
**       OUTPUT :  
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_str_adjust(i,e, font)
	int	i;
	struct	UA_generic_draft	(*e);
	int font;
	{
	int k, j, num, count;
	char local[1000];

	static char plms[3] = {"\\+"};
	static char degree[3] = {"\\d"};
	static char dia[3] 	= {"\\T"};

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_str_adjust"));

	num = e->txt_blk[i].char_cnt;
	for(j=0;j<1000;j++) local[j] = '\0';
	k = 0;
	count = 0;
	for(j=0;j<num;j++)
		{
		if(e->txt_blk[i].tstring[j] == '#' && font == 1002)
			{
			strcat(local,plms);
			k = k + 2;
			count++;
			continue;
			}
		else if(e->txt_blk[i].tstring[j] ==  '$' && font == 1002)
			{
			strcat(local,degree);
			k = k + 2;
			count++;
			continue;
			}
		else if(e->txt_blk[i].tstring[j] == 'n')
			{
			if(font == 1001)
				{
				strcat(local,dia);
				k = k + 2;
				}
			else
				{
				local[k] = 'n';
				k++;
				}
			count++;
			continue;
			}
		else
			{
			local[k] = e->txt_blk[i].tstring[j];
			k++;
			count++;
			}
		}
	e->txt_blk[i].char_cnt = count;
	local[k] = '\0';
	strcpy(e->txt_blk[i].tstring,local);

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    I_FUNCTION     :  uig_map_label(dblk,t, note_t, leader_t, gnote, number,l1_form,ll, key)
**				Map a IGES label dim to a Unibase label dim.
**    PARAMETERS   
**       INPUT  : 
**				dblk			directory block
**				t				IGES transformation matrix
**				note_t			IGES transformation matrix
**				leader_t		IGES transformation matrix
**				gnote			 associated note
**				number			number of leaders
**				l1_form         leader form numbers
**				ll				leader data
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_label(dblk, t, note_t, leader_t, g, number, l1_form, ll, key)
	struct dir_rec *dblk;				/*  directory record */
	UU_REAL t[12], note_t[12], leader_t[12];
	struct IG_igesnote_rec *g;
	int number;
	int l1_form[10];
	char *ll[10];
	UU_KEY_ID *key;
	{

	struct UA_generic_draft e;

	int i,j,k,nc,start,arrow_type,mm,m_init;
	int num,count,l_cnt,a_cnt,l_form;
	UU_REAL pt1[3],pt2[3],a_org[3];
	UU_REAL *pt;
	struct IG_gnote_rec *p;
	struct IG_igeslead_rec *l;
	UU_REAL a_angle,vec[3], x_size;
	UU_REAL um_angle2p();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_label"));

	/* load record with defaults */

	ua_init_entity(UA_LABEL_DIM,1,&e);
	e.rel_num = UA_LINEAR_DIMS_REL;

	/* load entity plane data */

	uig_set_dpln(t,&e);

	/*********************
	**                  **
	** load text blocks **
	**                  **
	*********************/

	num = g->no_gnote;
	p = g->gnote;
	uu_dprint(UU_MTRC,(us,"uig_map_label: num = %d", num));
	for(i=0;i<num;i++)
		{
		e.txt_blk[i].subtype = main_txt1;
		nc = p->str_cnt;
                if (nc > 0)
                  {
 		   e.txt_blk[i].char_cnt = p->str_cnt;
		   count = strlen(&p->str[0]);
		   start = count - p->str_cnt;
		   strcpy(&e.txt_blk[i].tstring[0],&p->str[start]);
		   uu_dprint(UU_MTRC,(us,"uig_map_label: string = %s", e.txt_blk[i].tstring));
		   uig_str_adjust(i, &e, p->fc);
		   uu_dprint(UU_MTRC,(us,"uig_map_label: test ang = %g", p->ang));
                  }
                else
                  {
		   e.txt_blk[i].char_cnt = 1;
                   strcpy(&e.txt_blk[i].tstring[0]," ");
                  }
		if(i == 0)
			{
			e.dim_value = 0.0;
			if(unit_scale > 1.0)
				{
				e.units_sym = e.d_units_sym = UA_INCHES;
				}
			else
				{
				e.units_sym = e.d_units_sym = UA_CENTIMETERS;
				}
			uig_tran_coor(&p->xyzt[0],note_t,&e.dim_origin[0]);
			um_vctmsc(&e.dim_origin[0],unit_scale,&e.dim_origin[0]);
			e.txt_blk[i].origin[0] = e.txt_blk[i].origin[1] 
					= e.txt_blk[i].origin[2] = 0.0;
			}
		else
			{
			uig_tran_coor(&p->xyzt[0],note_t,&e.txt_blk[i].origin[0]);
			um_vctmsc(&e.txt_blk[i].origin[0],unit_scale,&e.txt_blk[i].origin[0]);
			um_vcmnvc(&e.txt_blk[i].origin[0],&e.dim_origin[0],
						&e.txt_blk[i].origin[0]);
			}
                if (nc > 0)
                  {
		   e.txt_blk[i].dx = unit_scale * p->wt;
		   e.txt_blk[i].dy = unit_scale * p->ht;
		   e.txt_blk[i].slant = p->sl;
		/* TEMPORARY CHANGE */
		/*
		e.txt_blk[i].tangle =  -p->ang;  
		*/
		   e.txt_blk[i].tangle =  p->ang;  
		   e.txt_blk[i].txt_size = e.txt_blk[i].dy;

		/* cpp: add calculation of the character expansion factor */
		   x_size = 1.2*(e.txt_blk[i].dx/e.txt_blk[i].char_cnt);
		   e.txt_blk[i].char_expansion = x_size/e.txt_blk[i].txt_size;
                  }
		e.txt_blk_use++;
		p++;
		}

	/*************************
	**                      **
	** process leader lines **
	**                      **
	*************************/

	uu_dprint(UU_MTRC,(us,"uig_map_label: number of leaders = %d", number));
	uu_dprint(UU_MTRC,(us,"leader_t %g %g %g %g", leader_t[0], leader_t[1],
	leader_t[3], leader_t[4]));
	for(i=0;i<number;i++)
		{
		uu_dprint(UU_MTRC,(us,"i = %d", i));
		l = (struct IG_igeslead_rec *) ll[i];
		l_form = l1_form[i];
		if(i == 0)
			{
			m_init = 0;
			l_cnt = e.line_blk_use;	/* MILLS:initialize l_cnt for SGI */
			e.line_blk_use++;
			e.line_blk[l_cnt].subtype = dim_line;
			e.line_blk[l_cnt].num_pts = 2 * l->num_seg;
			}
		else
			{
			m_init = e.line_blk[l_cnt].num_pts;
	  		e.line_blk[l_cnt].num_pts = e.line_blk[l_cnt].num_pts + 2 * l->num_seg;
			uu_dprint(UU_MTRC,(us,"m_init = %d n_segs = %d", m_init, 2*l->num_seg));
			}
	
	/* first compute the angle of the first line segment(needed for arrow blk) */

		pt = l->pt2;
		for(j=0;j<2;j++) vec[j] = l->xyh[j] - pt[j];
		vec[2] = 0.0;
		um_unitvc(vec,vec);
		uig_tran_vec(vec, leader_t, vec);
		a_angle = um_angle2p(e.cpln.xaxis,vec,e.cpln.zaxis);
	
	/* now load line block */

		for(j=0;j<2;j++) pt1[j] = l->xyh[j];
		pt1[2] = l->zt;
		uu_dprint(UU_MTRC,(us ,"p1(raw) = %g %g %g", pt1[0], pt1[1], pt1[2]));
		uig_tran_coor(pt1,leader_t,pt1);
		uu_dprint(UU_MTRC,(us ,"p1(mod trans) = %g %g %g", pt1[0], pt1[1], pt1[2]));
		um_vctmsc(pt1,unit_scale,pt1);
		um_vctovc(pt1,a_org);
		for(j=0,k=0,mm=m_init;j<l->num_seg;j++,k++,k++,mm++,mm++)
			{
			pt2[0] = pt[k];
			pt2[1] = pt[k+1];
			pt2[2] = l->zt;
		uu_dprint(UU_MTRC,(us ,"p2(raw) = %g %g %g", pt2[0], pt2[1], pt2[2]));
			uig_tran_coor(pt2,leader_t,pt2);
		uu_dprint(UU_MTRC,(us ,"p2(mod trans) = %g %g %g", pt2[0], pt2[1], pt2[2]));
			um_vctmsc(pt2,unit_scale,pt2);
			e.line_blk[l_cnt].line_seg[mm][0] = pt1[0];
			e.line_blk[l_cnt].line_seg[mm][1] = pt1[1];
			e.line_blk[l_cnt].line_seg[mm][2] = pt1[2];
			e.line_blk[l_cnt].line_seg[mm+1][0] = pt2[0];
			e.line_blk[l_cnt].line_seg[mm+1][1] = pt2[1];
			e.line_blk[l_cnt].line_seg[mm+1][2] = pt2[2];
			um_vctovc(pt2,pt1);
			}

	/**************************
	**                       **
	** load arrow-head block **
	**                       **
	**************************/

		a_cnt = e.arrow_blk_use;
		e.arrow_blk_use++;
		uig_arrow(l_form,&arrow_type);
		e.arrow_blk[a_cnt].arrow_type = arrow_type;
		um_vctovc(a_org,&e.arrow_blk[a_cnt].location[0]);
		e.arrow_blk[a_cnt].aangle = a_angle;
		e.arrow_blk[a_cnt].size = l->ah * unit_scale;

		}

	/* check if creating a drawing entity */

	if(drw_flag)
		{
		uig_transform_drafting(&e, drw_v);
		uig_transform_drafting(&e, drw_s);
		uu_dprint(UU_MTRC,(us,"line seg (after view) %g %g %g to %g %g %g",
		e.line_blk[0].line_seg[0][0],
		e.line_blk[0].line_seg[0][1],
		e.line_blk[0].line_seg[0][2],
		e.line_blk[0].line_seg[1][0],
		e.line_blk[0].line_seg[1][1],
		e.line_blk[0].line_seg[1][2]));
		uu_dprint(UU_MTRC,(us,"line seg (after view) %g %g %g to %g %g %g",
		e.line_blk[0].line_seg[2][0],
		e.line_blk[0].line_seg[2][1],
		e.line_blk[0].line_seg[2][2],
		e.line_blk[0].line_seg[3][0],
		e.line_blk[0].line_seg[3][1],
		e.line_blk[0].line_seg[3][2]));
		uig_transform_drafting(&e, drw_t);
		dblk->view_ptr = drw_ptr;
		}

	/* create Unibase record */

	uig_update_attr(dblk);
	uig_create_draft(&e,0,0,dblk->view_ptr);
	UIG_unibase_entities++;
	*key = e.key;
	uu_dprint(UU_MTRC,(us,"uig_map_label: returning "));

	uu_dexit;
	return(UU_SUCCESS);
	}
/*********************************************************************
**    I_FUNCTION     :  uig_map_gsymbol(dblk,t, note_t, leader_t, gnote,
**                                        number,l1_form,ll, key)
**	      Map a IGES general symbol to a Unibase balloon label.
**    PARAMETERS
**       INPUT  :
**          dblk         directory block
**          trn          IGES transformation matrix
**          note_t       Note transformation matrix
**          leader_t     Leader transformation matrices.
**          gnote        associated note
**          number       number of leaders
**          l1_form      leader form numbers
**          ll           leader data
**          ngeo         Number of balloon geometry entities.
**          pgeo         pointer to balloon geometry entities.
**          geo_t        pointer to balloon geometry transformation matices.
**       OUTPUT :
**          key          Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_map_gsymbol(dblk, trn, note_t, leader_t, g, number, l1_form, ll,
					 ngeo, pgeo, geo_t, key)
struct dir_rec *dblk;            /*  directory record */
UU_REAL trn[12], note_t[12], leader_t[10][12];
struct IG_igesnote_rec *g;
int number;
int l1_form[10];
char *ll[10];
int ngeo;        /* Number of balloon geometry entities */
char **pgeo;     /* pointer to balloon geo */
UU_REAL *geo_t;
UU_KEY_ID *key;
{

	struct UA_generic_draft e;
	struct UA_line_blk *lbptr;
	struct UA_arc_blk  *abptr;

	int i,j,k,nc,start,arrow_type,mm,m_init;
	int num,count,l_cnt,a_cnt,l_form;
	int iarc, first_line;
	UU_REAL pt1[3],pt2[3],a_org[3];
	UU_REAL *pt;
	struct IG_gnote_rec *p;
	struct IG_igeslead_rec *l;
	struct IG_igesarc_rec *cptr;
	struct IG_igesline_rec *lptr;
	UU_REAL a_angle,vec[3], *tran_ptr;
	UU_REAL um_angle2p();

	/* load record with defaults */

	ua_init_entity(UA_BALLOON_DIM,1,&e);
	e.rel_num = UA_LINEAR_DIMS_REL;

	/* load entity plane data */

	uig_set_dpln(trn,&e);

	/*********************
	**                  **
	** load text blocks **
	**                  **
	*********************/

	num = g->no_gnote;
	p = g->gnote;
	for(i=0;i<num;i++)
		{
		e.txt_blk[i].subtype = main_txt1;
		nc = p->str_cnt;
		if (nc > 0)
		{
			e.txt_blk[i].char_cnt = p->str_cnt;
		   count = strlen(&p->str[0]);
		   start = count - p->str_cnt;
		   strcpy(&e.txt_blk[i].tstring[0],&p->str[start]);
		   uig_str_adjust(i, &e, p->fc);
		}
		else
		{
			e.txt_blk[i].char_cnt = 1;
			strcpy(&e.txt_blk[i].tstring[0]," ");
		}
		if(i == 0)
			{
			e.dim_value = 0.0;
			if(unit_scale > 1.0)
				{
				e.units_sym = e.d_units_sym = UA_INCHES;
				}
			else
				{
				e.units_sym = e.d_units_sym = UA_CENTIMETERS;
				}
			uig_tran_coor(&p->xyzt[0],note_t,&e.dim_origin[0]);
			um_vctmsc(&e.dim_origin[0],unit_scale,&e.dim_origin[0]);
			e.txt_blk[i].origin[0] = e.txt_blk[i].origin[1] 
					= e.txt_blk[i].origin[2] = 0.0;
			}
		else
			{
			uig_tran_coor(&p->xyzt[0],note_t,&e.txt_blk[i].origin[0]);
			um_vctmsc(&e.txt_blk[i].origin[0],unit_scale,&e.txt_blk[i].origin[0]);
			um_vcmnvc(&e.txt_blk[i].origin[0],&e.dim_origin[0],
						&e.txt_blk[i].origin[0]);
			}
		if (nc > 0)
		{
		   e.txt_blk[i].dx = unit_scale * p->wt;
		   e.txt_blk[i].dy = unit_scale * p->ht;
		   e.txt_blk[i].slant = p->sl;
		   e.txt_blk[i].tangle =  p->ang;  
		   e.txt_blk[i].txt_size = e.txt_blk[i].dy;

		/* cpp: add calculation of the character expansion factor */
/* 		   x_size = 1.2*(e.txt_blk[i].dx/e.txt_blk[i].char_cnt); */
/* 		   e.txt_blk[i].char_expansion = x_size/e.txt_blk[i].txt_size; */
		   e.txt_blk[i].char_expansion = 1.0;
		}
		e.txt_blk_use++;
		p++;
		}

/*
.....Process symbol geometry
*/
	first_line = 1;
	tran_ptr = geo_t;
	for(i=0;i<ngeo;i++)
	{
		cptr = (struct IG_igesarc_rec  *)pgeo[i];
		lptr = (struct IG_igesline_rec *)pgeo[i];
		if (cptr->rel_num == GARC)
		{
			iarc = e.arc_blk_use;
			e.arc_blk_use++;
			abptr = &e.arc_blk[iarc];
			abptr->num_pts = 2;
			vec[0] = cptr->spt[0] - cptr->cpt[0];
			vec[1] = cptr->spt[1] - cptr->cpt[1];
			vec[2] = 0.0;
			uig_tran_vec(vec, tran_ptr, vec);
			abptr->radius = um_mag(vec)*unit_scale;
			abptr->center_pt[0] = cptr->cpt[0];
			abptr->center_pt[1] = cptr->cpt[1];
			abptr->center_pt[2] = cptr->zt;
			uig_tran_coor(abptr->center_pt, tran_ptr, abptr->center_pt);
			um_vctmsc (abptr->center_pt, unit_scale, abptr->center_pt);
			abptr->angles[0] = um_angle2p(e.cpln.xaxis,vec,e.cpln.zaxis);
			vec[0] = cptr->ept[0] - cptr->cpt[0];
			vec[1] = cptr->ept[1] - cptr->cpt[1];
			vec[2] = 0.0;
			uig_tran_vec(vec, tran_ptr, vec);
			abptr->angles[1] = um_angle2p(e.cpln.xaxis,vec,e.cpln.zaxis);
		}
		else if (lptr->rel_num == GLINE)
		{
			if (first_line)
			{
				m_init = 0;
				l_cnt = e.line_blk_use;
				e.line_blk_use++;
				lbptr = &e.line_blk[l_cnt];
				lbptr->subtype = ext_line;
				first_line = 0;
			}
			pt = lbptr->line_seg[m_init];
			uig_tran_coor(lptr->spt, tran_ptr, pt);
			um_vctmsc (pt,unit_scale,pt);
			m_init++;
			pt = lbptr->line_seg[m_init];
			uig_tran_coor(lptr->ept, tran_ptr, pt);
			um_vctmsc (pt,unit_scale,pt);
			m_init++;
			lbptr->num_pts = m_init;
		}
		tran_ptr += 12;
	}
	/*************************
	**                      **
	** process leader lines **
	**                      **
	*************************/

	for(i=0;i<number;i++)
		{
		l = (struct IG_igeslead_rec *) ll[i];
		l_form = l1_form[i];
		if(i == 0)
			{
			m_init = 0;
			l_cnt = e.line_blk_use;	/* MILLS:initialize l_cnt for SGI */
			e.line_blk_use++;
			e.line_blk[l_cnt].subtype = dim_line;
			e.line_blk[l_cnt].num_pts = 2 * l->num_seg;
			}
		else
			{
			m_init = e.line_blk[l_cnt].num_pts;
	  		e.line_blk[l_cnt].num_pts = e.line_blk[l_cnt].num_pts + 2 * l->num_seg;
			}
	
	/* first compute the angle of the first line segment(needed for arrow blk) */

		pt = l->pt2;
		for(j=0;j<2;j++) vec[j] = l->xyh[j] - pt[j];
		vec[2] = 0.0;
		um_unitvc(vec,vec);
		uig_tran_vec(vec, leader_t[i], vec);
		a_angle = um_angle2p(e.cpln.xaxis,vec,e.cpln.zaxis);
	
	/* now load line block */

		for(j=0;j<2;j++) pt1[j] = l->xyh[j];
		pt1[2] = l->zt;
		uig_tran_coor(pt1,leader_t[i],pt1);
		um_vctmsc(pt1,unit_scale,pt1);
		um_vctovc(pt1,a_org);
		for(j=0,k=0,mm=m_init;j<l->num_seg;j++,k++,k++,mm++,mm++)
			{
			pt2[0] = pt[k];
			pt2[1] = pt[k+1];
			pt2[2] = l->zt;
			uig_tran_coor(pt2,leader_t[i],pt2);
			um_vctmsc(pt2,unit_scale,pt2);
			e.line_blk[l_cnt].line_seg[mm][0] = pt1[0];
			e.line_blk[l_cnt].line_seg[mm][1] = pt1[1];
			e.line_blk[l_cnt].line_seg[mm][2] = pt1[2];
			e.line_blk[l_cnt].line_seg[mm+1][0] = pt2[0];
			e.line_blk[l_cnt].line_seg[mm+1][1] = pt2[1];
			e.line_blk[l_cnt].line_seg[mm+1][2] = pt2[2];
			um_vctovc(pt2,pt1);
			}

	/**************************
	**                       **
	** load arrow-head block **
	**                       **
	**************************/

		a_cnt = e.arrow_blk_use;
		e.arrow_blk_use++;
		uig_arrow(l_form,&arrow_type);
		e.arrow_blk[a_cnt].arrow_type = arrow_type;
		um_vctovc(a_org,&e.arrow_blk[a_cnt].location[0]);
		e.arrow_blk[a_cnt].aangle = a_angle;
		e.arrow_blk[a_cnt].size = l->ah * unit_scale;

		}

	/* check if creating a drawing entity */

	if(drw_flag)
		{
		uig_transform_drafting(&e, drw_v);
		uig_transform_drafting(&e, drw_s);
		uig_transform_drafting(&e, drw_t);
		dblk->view_ptr = drw_ptr;
		}

	/* create Unibase record */

	uig_update_attr(dblk);
	uig_create_draft(&e,0,0,dblk->view_ptr);
	UIG_unibase_entities++;
	*key = e.key;

	return (UU_SUCCESS);
	}
