/*********************************************************************
**    NAME         : alabel.c
**       CONTAINS:
**				ua_label_create
**				ua_label
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       alabel.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:35
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) alabel.c 4.2 7/24/89 16:05:17 single"};
#else
static char uu_sccsident[]={"@(#) alabel.c 4.2 7/24/89 16:05:17 double"};
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

extern int	UD_draftable[UD_NMENTWD];
static UM_coord xas = {1.0,0.0,0.0};
static UM_coord yas = {0.0,1.0,0.0};
static UM_coord zas = {0.0,0.0,1.0};

void ua_label_align(),ua_label_ovld();

/*********************************************************************
**    E_FUNCTION     : ua_label_horz(e, corn, torg, sorg)
**       Create label with horizontal text.
**    PARAMETERS   
**       INPUT  : 
**          e									entity record
**          corn								array of text box corner points
**       OUTPUT :  
**          e									updated entity record
**          torg								text origin
**          sorg								stub origin
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_label_horz(e, corn, torg, sorg)
struct UA_generic_draft	*e;
UM_coord	corn[4], torg, sorg;
	{
	int		itxt, idx1, idx2;
	UM_coord	txtorig, stuborig, tmp_vec1, tmp_vec2;
	UU_REAL one_half, um_dcccc();

	uu_denter(UU_STRC,(us, "ua_label_horz()"));

	itxt = 1;
	one_half = 1.0/2.0;
	switch( e->lead_orient )
		{
		case UA_LEFT:
			idx1 = 4;
			idx2 = 1;
			break;
		case UA_RIGHT:
			idx1 = 3;
			idx2 = 2;
			break;
		case UA_CENTER:
			idx1 = 4;
			idx2 = 1;
			break;
		}
	switch( e->leader_loc )
		{
		case UA_TOP:
			um_vctovc(corn[idx1-1],torg);
			break;
		case UA_MIDDLE:
			um_vctmsc(yas,( e->txt_blk[itxt-1].dy/2.000000e+000 ),tmp_vec2);
			um_vcmnvc(corn[idx1-1],tmp_vec2,torg);
			break;
		case UA_BOTTOM:
			um_vctovc(corn[idx2-1],torg);
			break;
		}
	switch( e->lead_orient )
		{
		case UA_LEFT:
			torg[0] = ( torg[0]-e->txt_gap );
			um_vctmsc(xas,e->stub_length,tmp_vec2);
			um_vcmnvc(torg,tmp_vec2,sorg);
			break;
		case UA_RIGHT:
			{
			torg[0] = ( torg[0]+e->txt_gap );
			um_vctmsc(xas,e->stub_length,tmp_vec2);
			um_vcplvc(torg,tmp_vec2,sorg);
			}
			break;
		case UA_CENTER:
			{
			um_vcmnvc(corn[1],corn[0],tmp_vec1);
			um_vctmsc(tmp_vec1,one_half,tmp_vec2);
			um_vcplvc(corn[0],tmp_vec2,torg);
			if( ( um_dcccc(e->asso_blk[0].location,corn[0])>
			    um_dcccc(e->asso_blk[0].location,corn[3]) ) )
				{
				torg[1] = ( corn[3][1]+( e->txt_blk[0].dy/2.000000e+000 ) );
				}
			else
				{
				torg[1] = ( corn[0][1]-( e->txt_blk[0].dy/2.000000e+000 ) );
				}
			um_vctovc(torg,sorg);
			}
			break;
		}
	um_cpltrans(torg,txtorig);
	um_cpltrans(sorg,stuborig);
	um_vctovc(txtorig,e->line_blk[0].line_seg[0]);
	um_vctovc(stuborig,e->line_blk[0].line_seg[1]);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_label_getnote(prompt, maxsize, text, num_chars)
**       Prompt the user TO enter the text lines of a label.
**    PARAMETERS   
**       INPUT  : 
**          prompt					number of prompt in prompt file
**				maxsize					maximum number of character in text
**       OUTPUT :  
**				text						text of note
**				num_chars				number of characters in text
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_label_getnote(prompt, maxsize, text, num_chars)
int		prompt;
int		maxsize;
char		text[1];
int		(*num_chars);
	{
	char		line_text[2048];
	char		cr[2];
	int		char_cnt;
	int		status;

	uu_denter(UU_STRC,(us,
		"ua_label_getnote(prompt=%d, maxsize=%d, text=%s, num_chars=%d)",
		prompt, maxsize, text, *num_chars));

	strcpy(cr,"\n");
	strcpy(text,"");
	(*num_chars) = 0;
	for(;;)
		{
		status = ud_string(13,prompt,line_text,maxsize,&(char_cnt), UU_FALSE);
		if( ! (( char_cnt>0 )) ) goto us_l166;
		strcpy(text,text);
		strcat(text,line_text);
		strcat(text,cr);
		(*num_chars) = ( ( (*num_chars)+char_cnt )+1 );
		}
us_l166: 
	;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_label_ovst(e, corn, torg, sorg)
**       Create label with text over stub.
**    PARAMETERS   
**       INPUT  : 
**          e									entity record
**          corn								array of text box corner points
**       OUTPUT :  
**          e									updated entity record
**          torg								text origin
**          sorg								stub origin
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_label_ovst(e, corn, torg, sorg)
struct UA_generic_draft	(*e);
UM_coord	corn[4], torg, sorg;
	{
	int		itxt, ia;
	UU_REAL	ang, dis;
	UM_coord	stuborig, txtorig, vec1, tmp1, tmp2;

	uu_denter(UU_STRC,(us,"ua_label_ovst( )" ));

	itxt = 1;
	ia = 1;
	switch( e->lead_orient )
		{
		case UA_LEFT:
			um_vctovc(corn[1],torg);
			um_vctovc(corn[0],sorg);
			break;
		case UA_RIGHT:
			um_vctovc(corn[0],torg);
			um_vctovc(corn[1],sorg);
			break;
		case UA_CENTER:
			um_vcmnvc(corn[1],corn[0],tmp1);
			um_vctmsc(tmp1,(UU_REAL) 1.0 / 2.000000e+000,tmp2);
			um_vcplvc(corn[0],tmp2,torg);
			um_vctovc(corn[0],sorg);
			break;
		}
	dis = e->txt_gap;
	um_vcmnvc(corn[0],corn[3],vec1);
	um_unitvc(vec1,vec1);
	um_vctmsc(vec1,dis,tmp1);
	um_vcplvc(torg,tmp1,torg);
	um_vctmsc(vec1,dis,tmp1);
	um_vcplvc(sorg,tmp1,sorg);
	um_cpltrans(torg,txtorig);
	um_cpltrans(sorg,stuborig);
	um_vctovc(txtorig,e->line_blk[0].line_seg[0]);
	um_vctovc(stuborig,e->line_blk[0].line_seg[1]);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_label_create(e)
**       Create a note entity, store in UNIBASE, and display in DIGS.
**    PARAMETERS   
**       INPUT  : 
**				e								drafting entity with label 
**												origin, note text and entity
**												(asso_blk) TO point TO.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_label_create(e)
struct UA_generic_draft	(*e);
	{
	int		ia, il, iarrow, keyid, type, itxt;
	UU_REAL	arrowang;
	UM_coord	arrowvec, stuborig, rot_yaxis, corners[4], stuborig_cc, dimorig_cc,
				loc_cc, sys_axis[3], txtorig, corners_cc[4], offset_cc, txtorig_cc,
				sys_origin, tmp1;

	uu_denter(UU_STRC,(us," ua_label_create(e=%s)", "..."));

	itxt = 1;
	e->txt_blk[itxt-1].tangle = 0.000000e+000;
	ua_text_offset(&((*e)),itxt,offset_cc);
	ua_text_origin(&((*e)),itxt,offset_cc,corners);
	um_mcstoccs(0,corners[0],corners_cc[0]);
	um_mcstoccs(0,corners[1],corners_cc[1]);
	um_mcstoccs(0,corners[2],corners_cc[2]);
	um_mcstoccs(0,corners[3],corners_cc[3]);
	type = e->asso_blk[0].modifier;
	switch( type )
		{
		case 1:
			ua_label_horz(&((*e)),corners_cc,txtorig_cc,stuborig_cc);
			il = 2;
			break;
		case 2:
			ua_label_align(&((*e)),corners_cc,txtorig_cc,stuborig_cc);
			il = 0;
			break;
		case 3:
			ua_label_ovld(&((*e)),corners_cc,txtorig_cc,stuborig_cc);
			il = 0;
			break;
		case 4:
			ua_label_ovst(&((*e)),corners_cc,txtorig_cc,stuborig_cc);
			il = 2;
			break;
		}
	um_cpltrans(stuborig_cc,stuborig);
	iarrow = e->arrow_blk_use;
	for(ia=1;ia<=e->asso_blk_use;ia++)
		{
		if(e->asso_blk[ia-1].asso_type >= 0)
			{
			il = ( il+1 );
			um_vctovc(stuborig,e->line_blk[0].line_seg[il-1]);
			il = ( il+1 );
			um_vctovc(e->asso_blk[ia-1].location,
										e->line_blk[0].line_seg[il-1]);
			iarrow = ( iarrow+1 );
			um_vctovc(e->asso_blk[ia-1].location,
										e->arrow_blk[iarrow-1].location);
			um_mcstoccs(0,e->asso_blk[ia-1].location,loc_cc);
			um_vcmnvc(loc_cc,stuborig_cc,arrowvec);
			e->arrow_blk[iarrow-1].aangle = um_angle(xas,arrowvec) ;
			if( ( arrowvec[1]<0.000000e+000 ) )
				{
				e->arrow_blk[iarrow-1].aangle = ( 0.000000e+000-e->
				    arrow_blk[iarrow-1].aangle );
				}
			}
		}
	e->line_blk[0].num_pts = il;
	e->line_blk_use = 1;
	e->arrow_blk_use = iarrow;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_label_align(e, corn, torg, sorg)
**       Create label with aligned text.
**    PARAMETERS   
**       INPUT  : 
**          e									entity record
**          corn								array of text box corner points
**       OUTPUT :  
**          e									updated entity record
**          torg								text origin
**          sorg								stub origin
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_label_align(e, corn, torg, sorg)
struct UA_generic_draft	(*e);
UM_coord	corn[4], torg, sorg;
	{
	int		site, i, idx1, idx2, side, itxt, ia;
	UU_REAL	ang, vdot, dis;
	UM_coord	corn_mod[4], vec, loc_cc, offset, vnorm, tmp1;

	uu_denter(UU_STRC,(us,"ua_label_align()"));

	itxt = 1;
	ia = 1;
	switch( e->lead_orient )
		{
		case UA_LEFT:
			idx1 = 4;
			idx2 = 1;
			side = 1;
			break;
		case UA_CENTER:
			idx1 = 4;
			idx2 = 1;
			side = 1;
			break;
		case UA_RIGHT:
			idx1 = 3;
			idx2 = 2;
			side = 2;
			break;
		}
	switch( e->leader_loc )
		{
		case UA_TOP:
			torg[0] = corn[idx1-1][0];
			torg[1] = corn[idx1-1][1];
			torg[2] = 0.000000e+000;
			if( ( side==1 ) )
				site = UA_TOP_LEFT;
			else
				site = UA_TOP_RIGHT;
			break;
		case UA_MIDDLE:
			torg[0] = corn[idx1-1][0];
			torg[1] = ( corn[idx1-1][1]-( e->txt_blk[itxt-1].dy/2.000000e+000 ) );
			torg[2] = 0.000000e+000;
			if( ( side==1 ) )
				site = UA_MIDDLE_LEFT;
			else
				site = UA_MIDDLE_RIGHT;
			break;
		case UA_BOTTOM:
			torg[0] = corn[idx2-1][0];
			torg[1] = corn[idx2-1][1];
			torg[2] = 0.000000e+000;
			if( ( side==1 ) )
				site = UA_BOTTOM_LEFT;
			else
				site = UA_BOTTOM_RIGHT;
			break;
		}
	um_mcstoccs(0,e->asso_blk[ia-1].location,loc_cc);
	um_vcmnvc(loc_cc,torg,vec);
	switch( e->lead_orient )
		{
		case UA_LEFT:
			um_cross(vec,zas,vnorm);
			break;
		case UA_CENTER:
			um_cross(vec,zas,vnorm);
			break;
		case UA_RIGHT:
			um_cross(zas,vec,vnorm);
			break;
		}
	vdot = um_dot(vnorm,yas);
	if( ( vdot<0.000000e+000 ) )
		{
		um_vctmsc(vnorm,(UU_REAL)-1.000000e+000,vnorm);
		switch( e->leader_loc )
			{
			case UA_TOP:
				if( ( side==2 ) )
					site = UA_TOP_LEFT;
				else
					site = UA_TOP_RIGHT;
				break;
			case UA_MIDDLE:
				if( ( side==2 ) )
					site = UA_MIDDLE_LEFT;
				else
					site = UA_MIDDLE_RIGHT;
				break;
			case UA_BOTTOM:
				if( ( side==2 ) )
					site = UA_BOTTOM_LEFT;
				else
					site = UA_BOTTOM_RIGHT;
				break;
			}
		}
	ang = um_angle2p(yas,vnorm,zas);
	e->txt_blk[itxt-1].tangle = ang;
	e->entity_site = site;
	um_cpltrans(torg,e->dim_origin);
	ua_text_offset(&((*e)),itxt,offset);
	ua_text_origin(&((*e)),itxt,offset,corn_mod);
	um_unitvc(vec,vec);
	um_vctmsc(vec,e->txt_gap,tmp1);
	um_vcplvc(torg,tmp1,sorg);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_label_setorient(val)
**       Set label text orientation global variable.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_label_setorient(val)
int		val;
	{

	uu_denter(UU_STRC,(us,"ua_label_setorient(val=%d)", val));

	switch( val )
		{
		case 0:
			UA_label_orient = LO_HORIZ;
			break;
		case 1:
			UA_label_orient = LO_HORIZX;
			break;
		case 2:
			UA_label_orient = LO_LEADER;
			break;
		case 3:
			UA_label_orient = LO_LEADERX;
			break;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_label_ovld(e, corn, torg, sorg)
**       Create label with text over the leader.
**    PARAMETERS   
**       INPUT  : 
**          e									entity record
**          corn								array of text box corner points
**       OUTPUT :  
**          e									updated entity record
**          torg								text origin
**          sorg								stub origin
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_label_ovld(e, corn, torg, sorg)
struct UA_generic_draft	(*e);
UM_coord	corn[4], torg, sorg;
	{
	int		site, stub_site, itxt, ia, num, i;
	UM_coord	mod_corn[4], vec,	temp,	vec_box, spt, vec_cc, loc_cc, offset,
				spt_box, stub_pt, org_pt, int_pt, loc1_cc, loc2_cc, vnorm, tmp1,
				tmp2, tmp3;
	UU_REAL	ang, vdot, dis, dis1;

	uu_denter(UU_STRC,(us,"ua_label_ovld()"));

	itxt = 1;
	ia = 1;
	switch( e->lead_orient )
		{
		case UA_LEFT:
			um_vctovc(corn[0],torg);
			um_vctovc(torg,org_pt);
			stub_site = 2;
			site = UA_BOTTOM_LEFT;
			break;
		case UA_CENTER:
			um_vctovc(corn[0],torg);
			um_vctovc(torg,org_pt);
			stub_site = 2;
			site = UA_BOTTOM_LEFT;
			break;
		case UA_RIGHT:
			um_vctovc(corn[0],torg);
			um_vctovc(torg,org_pt);
			stub_site = 1;
			site = UA_BOTTOM_LEFT;
			break;
		}
	um_mcstoccs(0,e->asso_blk[ia-1].location,loc_cc);
	um_vcmnvc(loc_cc,torg,vec);
	switch( e->lead_orient )
		{
		case UA_LEFT:
			{
			um_cross(vec,zas,vnorm);
			}
			break;
		case UA_CENTER:
			{
			um_cross(vec,zas,vnorm);
			}
			break;
		case UA_RIGHT:
			{
			um_cross(zas,vec,vnorm);
			}
			break;
		}
	vdot = um_dot(vnorm,yas);
	if( ( vdot<0.000000e+000 ) )
		{
		um_vctmsc(vnorm,(UU_REAL)-1.000000e+000,vnorm);
		switch( e->lead_orient )
			{
			case UA_RIGHT:
				stub_site = 2;
				break;
			case UA_LEFT:
				site = UA_BOTTOM_RIGHT;
				stub_site = 1;
				um_vctovc(corn[1],org_pt);
				break;
			case UA_CENTER:
				site = UA_BOTTOM_RIGHT;
				stub_site = 1;
				um_vctovc(corn[1],org_pt);
				break;
			}
		}
	ang = um_angle2p(yas,vnorm,zas);
	e->txt_blk[itxt-1].tangle = ang;
	e->entity_site = site;
	um_unitvc(vnorm,vnorm);
	if( ( vdot>0.000000e+000 ) )
		{
		um_vctmsc(vnorm,e->txt_gap,tmp1);
		um_vcplvc(torg,tmp1,temp);
		}
	else
		{
		if( ( site==1 ) )
			{
			um_vctmsc(vnorm,e->txt_gap,tmp1);
			um_vcplvc(torg,tmp1,temp);
			}
		else
			{
			um_unitvc(vec,vec);
			um_ilnln(torg,vec,org_pt,vnorm,&(num),int_pt);
			dis = um_dcccc(int_pt,org_pt);
			dis1 = um_dcccc(int_pt,corn[2]);
			if( ( dis>dis1 ) )
				{
				um_vctmsc(vnorm,( dis+e->txt_gap ),tmp1);
				um_vcplvc(org_pt,tmp1,temp);
				}
			else
				{
				um_vctmsc(vnorm,( dis-e->txt_gap ),tmp1);
				um_vcmnvc(org_pt,tmp1,temp);
				}
			}
		}
	um_cpltrans(temp,e->dim_origin);
	ua_text_offset(&((*e)),itxt,offset);
	ua_text_origin(&((*e)),itxt,offset,mod_corn);
	if( ( e->lead_orient==UA_CENTER ) )
		{
		um_mcstoccs(0,mod_corn[stub_site-1],loc_cc);
		um_vctmsc(vnorm,e->txt_gap,tmp1);
		um_vcmnvc(loc_cc,tmp1,sorg);
		um_mcstoccs(0,mod_corn[0],loc1_cc);
		um_mcstoccs(0,mod_corn[1],loc2_cc);
		if( ( stub_site==1 ) )
			{
			um_vcmnvc(loc1_cc,loc2_cc,tmp1);
			um_vctmsc(tmp1,(UU_REAL) 1.0 / 2.000000e+000,tmp2);
			um_vcmnvc(loc1_cc,tmp2,tmp3);
			um_vctmsc(vnorm,e->txt_gap,tmp1);
			um_vcmnvc(tmp3,tmp1,sorg);
			}
		else
			{
			um_vcmnvc(loc1_cc,loc2_cc,tmp1);
			um_vctmsc(tmp1,(UU_REAL) 1.0 / 2.000000e+000,tmp2);
			um_vcplvc(loc2_cc,tmp2,tmp3);
			um_vctmsc(vnorm,e->txt_gap,tmp1);
			um_vcmnvc(tmp3,tmp1,sorg);
			}
		}
	else
		{
		um_mcstoccs(0,mod_corn[stub_site-1],loc_cc);
		um_vctmsc(vnorm,e->txt_gap,tmp1);
		um_vcmnvc(loc_cc,tmp1,sorg);
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_label()
**			Get COORDinate or entity location, note text and note
**			origin and create the leader line and note.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_label()
	{
	struct UA_PICKENT	pent;
	struct UA_PLOCREC	plocrec;
	struct UA_generic_draft	e;
	int		i, numploc, num, prev_key, iassoc, relnum, keyid, status, flag,
				loc_option, count, dummy, prev_txt_blk_use;
	UM_coord	cpyaxis, npt, cpzaxis, cpxaxis, ewc, cporig, location;
	UU_REAL uu;
	UU_LOGICAL	redo, first;

	uu_denter(UU_STRC,(us,"ua_label()"));

	first = UU_TRUE;
	status = ua_leader_subf(2,&(e),&(loc_option));
	switch( status )
		{
		case UA_REJECT:
			uu_dexit;
			return;
		case UA_ALT_ACTION:
			uu_dexit;
			return;
		}
again:
	ua_init_entity(UA_LABEL_DIM,0,&(e));
	e.txt_entry = 1;
	ua_getcpln(&(e),cporig,cpxaxis,cpyaxis,cpzaxis);
	redo = UU_FALSE;
	e.txt_blk[0].tangle = 0.000000e+000;
	ud_lgeo(UU_TRUE,UD_draftable);
	status = ua_leader_subf(3,&(e),&(dummy));
	switch( status )
		{
		case UA_REJECT:
			uu_dexit;
			return;
		case UA_ALT_ACTION:
			if( ( first==UU_FALSE ) )
				{
				redo = UU_TRUE;
				status = uc_retrieve_data(&(e),sizeof(struct UA_generic_draft	));
				if( ( status==0 ) )
					{
					e.arc_blk_use = 0;
					e.line_blk_use = 0;
					e.arrow_blk_use = 0;
					e.txt_blk_use = prev_txt_blk_use;
					iassoc = e.asso_blk_use;
					}
				else
					{
					first = UU_TRUE;
					goto again;
					}
				}
			else
				{
				uu_dexit;
				return;
				}
			break;
		case UA_OPCOMPLETE:
			iassoc = e.asso_blk_use;
			for(;;)
				{
				if( ! (( status!=0 )) ) goto us_l182;
				um_vctovc(e.asso_blk[iassoc-1].location,ewc);
				um_nptpln(ewc,cporig,cpzaxis,npt);
				um_vctovc(npt,e.asso_blk[iassoc-1].location);
				e.asso_blk[iassoc-1].modifier = loc_option;
				ud_lgeo(UU_TRUE,UD_draftable);
				status = ua_leader_subf(3,&(e),&(dummy));
				iassoc = e.asso_blk_use;
				}
us_l182: ;
			if( ( iassoc==0 ) )
				{
				uu_dexit;
				return;
				}
		}
	e.asso_blk_use = iassoc;
	for(i=0;i<e.asso_blk_use;i++)
		{
		if(e.asso_blk[i].key != 0)
			{
			ua_para_on_curve(e.asso_blk[i].key, e.asso_blk[i].location, &uu);
			iassoc++;
			e.asso_blk[iassoc-1].location[0] = uu;
			e.asso_blk[iassoc-1].asso_type = -99;
			e.asso_blk[i].asso_type = iassoc - 1;
			}
		}
	e.asso_blk_use = iassoc;
	flag = ud_world_coord(UA_DRAFTING,54,location,1,&(count),UU_FALSE);
	if( ( count==0 ) ) goto again;
	um_nptpln(location,cporig,cpzaxis,npt);
	um_vctovc(npt,e.dim_origin);
	if( ( redo==UU_FALSE ) )
		{
		e.txt_blk_use = 1;
		e.txt_blk[0].subtype = main_txt1;
		strcpy(e.txt_blk[0].tstring,"");
		ua_label_getnote(53,1024,e.txt_blk[0].tstring,&(e.txt_blk[0].char_cnt));
		if( ( e.txt_blk[0].char_cnt==0 ) ) goto again;
		}
	ua_label_create(&(e));
	if( redo )
		{
		status = ua_update_entity(prev_key,&(e));
		if( ( status!=0 ) ) ua_create_entity(&(e),&(keyid));
		}
	else
		{
		ua_create_entity(&(e),&(keyid));
		}
	uc_display(&(e));
	prev_key = e.key;
	prev_txt_blk_use = e.txt_blk_use;
	first = UU_FALSE;
	goto again;
	}

/*********************************************************************
**    E_FUNCTION     : ua_label_regen( edrf)
**			Re-generate a label entity
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_label_regen( edrf)
	struct UA_generic_draft *edrf;
	{
	int i;
	UU_REAL uu;
	UM_coord location, pt, del_vec;
	UU_LOGICAL first;

	uu_denter(UU_STRC,(us,"ua_label_regen()"));

	first = UU_TRUE;
	for(i=0;i<edrf->asso_blk_use;i++)
		{
		if(edrf->asso_blk[i].key != 0 && edrf->asso_blk[i].asso_type > 0)
			{
			if(first)
				{
				if(edrf->asso_blk[edrf->asso_blk[i].asso_type].asso_type == -99)
					{
					um_vctovc(edrf->asso_blk[i].location, location);
					uu = edrf->asso_blk[edrf->asso_blk[i].asso_type].location[0];
					ua_pt_on_curve(edrf->asso_blk[i].key, uu, pt);
					um_vcmnvc(pt, location, del_vec);
					um_vcplvc(edrf->dim_origin, del_vec, edrf->dim_origin);
					um_vctovc(pt, edrf->asso_blk[i].location);
					first = UU_FALSE;
					}
				}
			else
				{
				if(edrf->asso_blk[edrf->asso_blk[i].asso_type].asso_type == -99)
					{
					uu = edrf->asso_blk[edrf->asso_blk[i].asso_type].location[0];
					ua_pt_on_curve(edrf->asso_blk[i].key, uu, pt);
					um_vctovc(pt, edrf->asso_blk[i].location);
					}
				}
			}
		}
	ua_label_create(edrf);
	uu_dexit;
	return(0);
	}

