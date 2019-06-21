/*********************************************************************
**    NAME         : aang.c
**       CONTAINS:
**    			ua_angular()
**    			ua_angular_setmsgno(e, redo)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aang.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:29
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aang.c 4.2 2/27/89 14:46:05 single"};
#else
static char uu_sccsident[]={"@(#) aang.c 4.2 2/27/89 14:46:05 double"};
#endif

static char *msg1 = {"angular type set to INTERIOR"};
static char *msg2 = {"angular type set to SUPPLEMENT"};
static char *msg3 = {"angular type set to EXTERIOR"};
static char *msg4 = {"angular type set to REPETITIVE"};

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "mdpick.h"
#include "mdcoord.h"
#include "adraft.h"
#include "adrf.h"
#include "adrfcom.h"
#include "adrfdefs.h"

#define UA_ANGULAR_ORIGMODE_OFF 1
#define UA_ANGULAR_ORIGMODE_AA 2

int UA_angular_subtype;
int UA_angular_origmode;
extern int UD_draftable[UD_NMENTWD];
extern int UD_draft_line[UD_NMENTWD];
struct UA_generic_draft	ad_test;

/*********************************************************************
**    E_FUNCTION     : ua_angular()
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
ua_angular()
	{
	struct UA_PLOCREC	plocrec;
	struct UA_generic_draft	ad;
	struct UA_PLOCREC	plocrec1;
	struct UA_PLOCREC	plocrec2;
	int		choice, num_arcs, dim_type, key, previous_txt_blk_use, curr_key,
				drf_key, relation1, relation2, subtype, s_status, previous_key,
				status, j, locations, msgno, dummy;
	UM_coord	base_pt, npt, cpln_origin, ewc, dim_origin, ept1, ept2, uvc1,
				uvc2, uvc3, dvec, npt1, npt2, int_pt, xaxis, spt1, base_vec,
				yaxis, spt2, zaxis;
	UU_REAL	off_set, ang12, proj, vdot, dis1, dis2;
	UU_LOGICAL	redo;
	UU_LOGICAL	first_time;
	UU_LOGICAL	ok;

	uu_denter(UU_STRC,(us,"SAL ua_angular()"));

	first_time = UU_TRUE;
	UA_angular_subtype = UA_ANGULAR_INTERIOR_DIM;
	UA_angular_origmode = UA_ANGULAR_ORIGMODE_OFF;
again:
	subtype = UA_angular_subtype;
	ua_init_entity(UA_ANGULAR_DIM,subtype,&(ad));
	ad.upper_tol = UA_ang_up_tol_val;
	ad.lower_tol = UA_ang_lo_tol_val;
	ua_getcpln(&(ad),cpln_origin,xaxis,yaxis,zaxis);
entity1:
	/* Indicate angle subtype to the user. */ 
	switch(UA_angular_subtype)
		{
		case UA_ANGULAR_INTERIOR_DIM:
			ud_prmerr(msg1);
			break;
		case UA_ANGULAR_SUPPLEMENT_DIM:
			ud_prmerr(msg2);
			break;
		case UA_ANGULAR_COMPLEMENT_DIM:
			ud_prmerr(msg3);
			break;
		case UA_ANGULAR_REP_INT_DIM:
		case UA_ANGULAR_REP_COM_DIM:
			ud_prmerr(msg4);
			break;
		default:
			break;
		}
	ud_lgeo(UU_TRUE,UD_draftable);
	status = ua_select_ent_subf(115,&(ad),1,&(plocrec1));
	switch( status )
		{
		case UA_REJECT:
			{
			uu_dexit;
			return;
			}
		case UA_ALT_ACTION:
			{
			if( ( first_time==UU_FALSE ) )
				{
				redo = UU_TRUE;
				ad.key = previous_key;
				status = uc_retrieve_data(&(ad),sizeof(struct UA_generic_draft	));
				if( ( status==0 ) )
					{
					ad.arc_blk_use = 0;
					ad.line_blk_use = 0;
					ad.arrow_blk_use = 0;
					ad.txt_orent = UA_txt_orient;
					ad.txt_blk_use = previous_txt_blk_use;
					j = 1;
					for(;;)
						{
						if( j > UA_NUM_TXTBLKS ) 	break;
						ad.txt_blk[j-1].tangle = UA_text_ang;
						j++ ;
						}
					goto origin;
					}
				else
					{
					redo = UU_FALSE;
					first_time = UU_TRUE;
					previous_key = 0;
					goto again;
					}
				}
			else
				{
				status = ua_popmenu(16,&(choice));
				if( ( status==UA_OPCOMPLETE ) )
					{
					switch( choice )
						{
						case 3:
						case 2:
						case 1:
							{
							UA_angular_subtype = choice;
							ad.subtype = UA_angular_subtype;
							}
							break;
						case 4:
							{
							UA_angular_origmode = UA_ANGULAR_ORIGMODE_AA;
							}
							break;
						case 5:
							/* Assume interior until selects lines. */
							UA_angular_subtype = UA_ANGULAR_REP_INT_DIM;
							break;
						default:
							{
							uu_uerror0(13,10);
							}
						}
					}
				goto entity1;
				}
			}
		case UA_OPCOMPLETE:
			{
			redo = UU_FALSE;
			}
			break;
		}
	ad.asso_blk_use = 1;
	curr_key = ad.asso_blk[ad.asso_blk_use-1].key;
	s_status = um_retrieve_data_relnum(curr_key,&(relation1));
	if( ( relation1==UA_LINEAR_DIM ) )
		{
		ad_test.key = curr_key;
		uc_retrieve_data(&(ad_test),sizeof(struct UA_generic_draft));
		dim_type = ad_test.etype;
		num_arcs = ad_test.arc_blk_use;
		if( ( dim_type==UA_CENTERLINE ) )
			{
			if( ( num_arcs==0 ) )
				{
				uu_uerror0(UA_DRAFTING,8);
				goto entity1;
				}
			}
		}
	else
		{
		status = uc_draft_type(curr_key,&(relation1));
		if( ( relation1!=UA_DRAFT_LINE ) )
			{
			uu_uerror0(UA_DRAFTING,8);
			goto entity1;
			}
		}
	if( ( relation1==UA_LINEAR_DIM ) )
		{
		ua_near_on_draft(ad.asso_blk[0].key,&(plocrec1),ewc);
		}
	else
		{
		uc_near_on_entity(ad.asso_blk[0].key,&(plocrec1.ndcloc),
		ewc);
		}
	um_nptpln(ewc,cpln_origin,zaxis,npt);
	um_vctovc(npt,ad.asso_blk[0].location);
entity2:
	/* check for repetitive angular type */
	if ((UA_angular_subtype == UA_ANGULAR_REP_INT_DIM) ||
			(UA_angular_subtype == UA_ANGULAR_REP_COM_DIM))
		{
		status = ua_cre_repang_dim(&(ad),&plocrec1,spt1,ept1,spt2,ept2);
		switch( status )
			{
			case UA_TRUE:
				goto origin;
			case UA_FALSE:
				goto entity1;
			default:
				uu_dexit;
				return;
			}
		}
	ud_lgeo(UU_TRUE,UD_draftable);
	status = ua_select_ent_subf(116,&(ad),2,&(plocrec2));
	if( ( status!=UA_OPCOMPLETE ) )
		{
		goto entity2;
		}
	ad.asso_blk_use = 2;
	curr_key = ad.asso_blk[ad.asso_blk_use-1].key;
	s_status = um_retrieve_data_relnum(curr_key,&(relation2));
	if( ( relation2==UA_LINEAR_DIM ) )
		{
		ad_test.key = curr_key;
		uc_retrieve_data(&(ad_test),sizeof(struct UA_generic_draft));
		dim_type = ad_test.etype;
		num_arcs = ad_test.arc_blk_use;
		if( ( dim_type==UA_CENTERLINE ) )
			{
			if( ( num_arcs==0 ) )
				{
				uu_uerror0(UA_DRAFTING,8);
				goto entity2;
				}
			}
		}
	else
		{
		status = uc_draft_type(curr_key,&(relation2));
		if( ( relation2!=UA_DRAFT_LINE ) )
			{
			uu_uerror0(UA_DRAFTING,8);
			goto entity2;
			}
		}
	if( ( relation2==UA_LINEAR_DIM ) )
		{
		ua_near_on_draft(ad.asso_blk[1].key,&(plocrec2),ewc);
		}
	else
		{
		uc_near_on_entity(ad.asso_blk[1].key,&(plocrec2.ndcloc), ewc);
		}
	um_nptpln(ewc,cpln_origin,zaxis,npt);
	um_vctovc(npt,ad.asso_blk[1].location);
	if( ( relation1==UA_LINEAR_DIM ) )
		{
		ua_centerline_line(ad.asso_blk[0].key,&(plocrec1),spt1, ept1);
		}
	else
		{
		uc_draft_line(ad.asso_blk[0].key,spt1,ept1);
		}
	um_nptpln(spt1,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,spt1);
	um_nptpln(ept1,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,ept1);
		{
		UU_REAL	us_t146[3];
		um_vcmnvc(ept1,spt1,us_t146);
		um_unitvc(us_t146,uvc1);
		}
	if( ( relation2==UA_LINEAR_DIM ) )
		{
		ua_centerline_line(ad.asso_blk[1].key,&(plocrec2),spt2, ept2);
		}
	else
		{
		uc_draft_line(ad.asso_blk[1].key,spt2,ept2);
		}
	um_nptpln(spt2,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,spt2);
	um_nptpln(ept2,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,ept2);
		{
		UU_REAL	us_t147[3];
		um_vcmnvc(ept2,spt2,us_t147);
		um_unitvc(us_t147,uvc2);
		}
	proj = um_dot(uvc1,uvc2);
	if( ( ( 1.000000e+000-fabs(proj) )<1.000000e-005 ) )
		{
		status = ua_popmenu(17,&(choice));
		if( ( status==UA_OPCOMPLETE ) )
			{
			if( ( choice==1 ) )
				{
				goto entity1;
				}
			if( ( choice==2 ) )
				{
				uu_dexit;
				return;
				}
			}
		goto entity1;
		}
origin:
	if( ( UA_angular_origmode==2 ) )
		{
		ud_lgeo(UU_FALSE,UD_draftable);
		status = ua_select_ent_subf(-114,&(ad),3,&(plocrec));
		if( ( status!=UA_OPCOMPLETE ) )
			{
			goto entity1;
			}
		ad.asso_blk_use = 3;
		ua_arrow_loc(ad.asso_blk[2].key,&(plocrec),cpln_origin,
		xaxis,yaxis,zaxis,ad.asso_blk[2].location,base_vec);
/*		ua_put_orig_in_angle(ad.dim_origin,ad.asso_blk[2].location,*/
		um_vctovc(ad.asso_blk[2].location,ad.dim_origin);
		if( ( redo||( ad.txt_place==UA_MANUAL ) ) )
			{
			status = UA_ALT_ACTION;
			for(;;)
				{
				if( ! (( status==UA_ALT_ACTION )) ) goto us_l148;
				msgno = ua_angular_setmsgno(&(ad),redo);
				status = ua_ent_origin_subf(msgno,&(ad));
				switch( status )
					{
					case UA_REJECT:
						{
						goto entity1;
						}
					case UA_ALT_ACTION:
						{
						switch( ad.txt_place )
							{
							case UA_AUTOMATIC:
								{
								ad.txt_place = 1;
								}
								break;
							case UA_MANUAL:
								{
								ad.txt_place = 0;
								}
								break;
							}
						}
						break;
					}
				}
us_l148: 
			;
			}
		}
	else
		{
		status = UA_ALT_ACTION;
		for(;;)
			{
			if( ! (( status==UA_ALT_ACTION )) ) goto us_l149;
			msgno = ua_angular_setmsgno(&(ad),redo);
			status = ua_ent_origin_subf(msgno,&(ad));
			switch( status )
				{
				case UA_REJECT:
					{
					goto entity1;
					}
				case UA_ALT_ACTION:
					{
					switch( ad.txt_place )
						{
						case UA_AUTOMATIC:
							{
							ad.txt_place = UA_MANUAL;
							}
							break;
						case UA_MANUAL:
							{
							ad.txt_place = UA_AUTOMATIC;
							}
							break;
						}
					}
					break;
				}
			}
us_l149: 
		;
		}
	ok = ua_text_subf(&(ad));
	if( ( !ok ) )
		{
		goto origin;
		}
	previous_txt_blk_use = ad.txt_blk_use;
	um_nptpln(ad.dim_origin,cpln_origin,zaxis,dim_origin);
	um_vctovc(dim_origin,ad.dim_origin);
	status = ua_angular_generate(&(ad),spt1,ept1,spt2,ept2);
	if (status == UU_FAILURE)
		{
		uu_dexit;
		return;
		}
	if( redo )
		{
		status = ua_update_entity(previous_key,&(ad));
		if( ( status!=0 ) )
			{
			ua_create_entity(&(ad),&(key));
			}
		}
	else
		{
		ua_create_entity(&(ad),&(key));
		}
	uc_display(&(ad));
	previous_key = ad.key;
	first_time = UU_FALSE;
	goto again;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_angular_setmsgno(e, redo)
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
int		ua_angular_setmsgno(e, redo)
	struct UA_generic_draft	(*e);
	UU_LOGICAL	redo;
	{
	int		msgno;

	uu_denter(UU_STRC,(us,"SAL ua_angular_setmsgno(e=%s, redo=%s)",
	"...", redo?"TRUE":"FALSE"));

	if( redo )
		{
		switch( (*e).txt_place )
			{
			case UA_AUTOMATIC:
				{
				msgno = 119;
				}
				break;
				default:
				{
				msgno = 118;
				}
			}
		}
	else
		{
		switch( (*e).txt_place )
			{
			case UA_AUTOMATIC:
				{
				msgno = 117;
				}
				break;
			default:
				{
				msgno = 30;
				}
			}
		}
	uu_dexit;
	return(msgno);
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_angular_regenerate(e)
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

ua_angular_regenerate(e)
	struct UA_generic_draft	*e;
	{
	int		status;
	UM_coord	cpln_origin, ept1, ept2, int_pt, xaxis, spt1, yaxis, spt2, zaxis;
	
	uu_denter(UU_STRC,(us,"ua_angular_regenerate"));

	ua_regen_endpts(&((*e)),0,spt1,ept1);
	ua_regen_endpts(&((*e)),1,spt2,ept2);
	ua_getcpln(&((*e)),cpln_origin,xaxis,yaxis,zaxis);
	um_nptpln(spt1,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,spt1);
	um_nptpln(ept1,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,ept1);
	um_nptpln(spt2,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,spt2);
	um_nptpln(ept2,cpln_origin,zaxis,int_pt);
	um_vctovc(int_pt,ept2);
	UA_angular_subtype = (*e).subtype;
	status = ua_angular_generate(&((*e)),spt1,ept1,spt2,ept2);

	uu_dexit;
	}


/*********************************************************************
**    E_FUNCTION     : ua_put_orig_in_angle()
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
/*ua_put_orig_in_angle(*/
