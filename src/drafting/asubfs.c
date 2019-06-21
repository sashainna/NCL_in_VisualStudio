/*********************************************************************
**    NAME         : asubfs.c
**       CONTAINS:
**				ua_select_ent_subf
**				ua_text_subf
**    		ua_user_text_dtol
**    		ua_user_text_mtol
**				ua_apptext_subf
**				ua_ent_origin_subf
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       asubfs.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:40
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) asubfs.c 25.1 04/29/15 15:05:40 single"};
#else
static char uu_sccsident[]={"@(#) asubfs.c 25.1 04/29/15 15:05:40 double"};
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
**    E_FUNCTION     : int		ua_select_ent_subf(msg_number, entity, 
**												blk_no, plocrec)
**       Subfunction to select entities for dimesions
**    PARAMETERS   
**       INPUT  : 
**				msg_number    - prompt message number
**				entity  			- entity record
**				blk_no  			- assoc blk no
**       OUTPUT :  
**				plocrec			- pick location record
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_select_ent_subf(msg_number, entity, blk_no, 
plocrec)
int		msg_number;
struct UA_generic_draft	(*entity);
int		blk_no;
struct UA_PLOCREC	(*plocrec);
	{
	int		relation;
	UU_REAL	pick_loc[3];
	struct UA_PICKENT	pent;
	int		asso_type;
	int		key;
	UU_REAL	far_ept[3];
	UU_REAL	ent_loc[3];
	int		rel_num;
	int		message_number;
	int		outchoice;
	int		locations;
	int		status;
	int		modifier;
	struct UA_generic_draft	edrf;
	int		dummy;

	uu_denter(UU_STRC,(us,"SAL ua_select_ent_subf(msg_number=%d, entity=%s,\
		blk_no=%d, plocrec=%s)", msg_number, "...", blk_no, "..."));

entity_get:
	modifier = 0;
	message_number = abs(msg_number);
	status = ud_pick_loc(13,message_number,&((*plocrec)),1,&(
	locations),UU_FALSE);
	if( ( status!=1 ) )
		{
		uu_dexit;
		return(status);
		}
	if( ( locations==0 ) )
		{
		uu_dexit;
		return(0);
		}
	status = um_d_pickresolve(&((*plocrec).ppick),2,&(pent));
	key = um_get_pickkey(&(pent),2);
	status = um_retrieve_data_relnum(key,&(relation));
	if( ( msg_number<0 ) )
		{
		if( ( relation==UA_LINEAR_DIM ) )
			{
			modifier = 10;
			asso_type = 5;
			}
		else
			{
			uu_uerror0(13,40);
			goto entity_get;
			}
		}
	else
		{
		if( ( relation==UA_LINEAR_DIM ) )
			{
			if( ( ( (*entity).etype==UA_LINEAR_DIM )&&( ( (*entity).subtype==4 )||
			    ( (*entity).subtype==5 ) ) ) )
				{
				uu_uerror0(13,40);
				goto entity_get;
				}
			else
				{
				edrf.key = key;
				uc_retrieve_data(&(edrf),sizeof(struct UA_generic_draft	))
					;
				if( ( edrf.etype==54 ) )
					{
					modifier = 10;
					asso_type = 5;
					}
				else
					{
					uu_uerror0(13,40);
					goto entity_get;
					}
				}
			}
		else
			{
			status = uc_draft_type(key,&(asso_type));
			if( ( ( (*entity).etype==UA_LINEAR_DIM  )||
					( (*entity).etype==UA_CHAIN_DIM   )||
					( (*entity).etype==UA_BASELN_DIM  )		) ) 
				{
				if( ( ( asso_type==2 )||( asso_type==3 ) ) )
					{
					modifier = 1;
					status = ua_popmenu(15,&(modifier));
					modifier = ( modifier-1 );
					if( ( status==0 ) )
						{
						uu_dexit;
						return(0);
						}
					}
				}
			}
		}
	(*entity).asso_blk[blk_no-1].key = key;
	(*entity).asso_blk[blk_no-1].modifier = modifier;
	(*entity).asso_blk[blk_no-1].asso_type = asso_type;
	uu_dexit;
	return(1);
	}
/*********************************************************************
**    E_FUNCTION     : int		ua_ent_origin_subf(msgno, entity)
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
int		ua_ent_origin_subf(msgno, entity)
int		msgno;
struct UA_generic_draft	(*entity);
	{
	int		choice;
	UU_REAL	origin[3];
	int		d_stat;
	int		location_mode;
	int		dummy;
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL	location[3];

	uu_denter(UU_STRC,(us,"SAL ua_ent_origin_subf(msgno=%d, entity=%s)",
			msgno, "..."));

	location_mode = 1;
orig_mode:
	switch( location_mode )
		{
		case 1:
			{
			d_stat = ud_world_coord(13,msgno,location,1,&(dummy),
			UU_FALSE);
			if( ( ( d_stat==0 )||( d_stat==2 ) ) )
				{
				goto funcexit;
				}
			if( ( dummy==0 ) )
				{
				d_stat = 0;
				goto funcexit;
				}
			ua_getcpln(&((*entity)),origin,xaxis,yaxis,zaxis);
			um_nptpln(location,origin,zaxis,(*entity).dim_origin);
			goto funcexit;
			}
		case 2:
			{
			}
			break;
		case 3:
			{
			}
			break;
		case 4:
			{
			}
			break;
		default:
			{
			d_stat = ua_popmenu(1,&(location_mode));
			if( ( ( d_stat==0 )||( d_stat==2 ) ) )
				{
				goto funcexit;
				}
			goto orig_mode;
			}
		}
funcexit:
	uu_dexit;
	return(d_stat);
	}
/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL	ua_text_subf(entity)
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
UU_LOGICAL	ua_text_subf(entity)
struct UA_generic_draft	(*entity);
	{
	int		choice;
	int		tblock;
	int		d_stat;
	int		dblk;
	int		mblk;
	int		dummy;

	uu_denter(UU_STRC,(us,"SAL ua_text_subf(entity=%s)", "..."));

	mblk = 0;
	dblk = 0;
	if( ( (*entity).txt_entry==1 ) )
		{
		if( ( (*entity).dual_format!=0 ) )
			{
			tblock = ( (*entity).txt_blk_use+1 );
			d_stat = ud_string(13,92,(*entity).txt_blk[tblock-1].tstring
			    ,1024,&((*entity).txt_blk[tblock-1].char_cnt),UU_FALSE);
			if( ( d_stat==1 ) )
				{
				(*entity).txt_blk_use = tblock;
				(*entity).txt_blk[tblock-1].subtype = main_txt1;
				mblk = tblock;
				}
			tblock = ( (*entity).txt_blk_use+1 );
			d_stat = ud_string(13,93,(*entity).txt_blk[tblock-1].tstring
			    ,1024,&((*entity).txt_blk[tblock-1].char_cnt),UU_FALSE);
			if( ( d_stat==1 ) )
				{
				(*entity).txt_blk_use = tblock;
				(*entity).txt_blk[tblock-1].subtype = main_txt2;
				dblk = tblock;
				}
			ua_user_text_mtol(&((*entity)),mblk);
			ua_user_text_dtol(&((*entity)),dblk);
			}
		else
			{
			tblock = ( (*entity).txt_blk_use+1 );
			d_stat = ud_string(13,45,(*entity).txt_blk[tblock-1].tstring
			    ,1024,&((*entity).txt_blk[tblock-1].char_cnt),UU_FALSE);
/* NCL - kathy */
			if ( (*entity).txt_blk[tblock-1].char_cnt == 0)
     				(*entity).txt_blk[tblock-1].char_cnt=1;

			if( ( d_stat==1 ) )
				{
				(*entity).txt_blk_use = tblock;
				(*entity).txt_blk[tblock-1].subtype = main_txt1;
				mblk = tblock;
				}
			ua_user_text_mtol(&((*entity)),mblk);
			}
		}
	ua_apptext_subf(&((*entity)));
	uu_dexit;
	return(UU_TRUE);
	}
/*********************************************************************
**    E_FUNCTION     : ua_user_text_dtol(e, dblk)
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
ua_user_text_dtol(e, dblk)
struct UA_generic_draft	(*e);
int		dblk;
	{
	char		fractext[51];
	char		dentext[51];
	int		format;
	char		toltext[51];
	char		numtext[51];
	int		i;
	char		lotext[51];
	char		uptext[51];
	char		dtext[51];
	int		start;

	uu_denter(UU_STRC,(us,"SAL ua_user_text_dtol(e=%s, dblk=%d)",
			"...", dblk));

	if( ( dblk==0 ) )
		{
		uu_dexit;
		return;
		}
	switch( (*e).tol_method )
		{
		case 0:
			{
			uu_dexit;
			return;
			}
		case 4:
		case 3:
		case 2:
		case 1:
			{
			uu_dexit;
			return;
			}
		case 5:
			{
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t135[1025];
					strcpy(toltext,"\\+");
					ua_value_to_string((*e).d_lin_up_tol,(*e).du_tol_pl,(*e).
					    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
					UA_d_dec_symbol,us_t135);
					strcat(toltext,us_t135);
					}
				}
			else
				{
				(*e).upper_tol = UA_ang_up_tol_val;
				(*e).lower_tol = UA_ang_lo_tol_val;
					{
					char		us_t136[1025];
					strcpy(toltext,"\\+");
					ua_dual_degree_tol_text(&((*e)),UA_ang_up_tol_val,us_t136);
					strcat(toltext,us_t136);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt2;
			}
			break;
		case 6:
			{
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t137[1025];
					char		us_t138[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).d_lin_up_tol,(*e).du_tol_pl,(*e).
					    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
					UA_d_dec_symbol,us_t137);
					strcat(toltext,us_t137);
					strcat(toltext,"\n-");
					ua_value_to_string((*e).d_lin_lo_tol,(*e).du_tol_pl,(*e).
					    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
					UA_d_dec_symbol,us_t138);
					strcat(toltext,us_t138);
					}
				}
			else
				{
				(*e).upper_tol = UA_ang_up_tol_val;
				(*e).lower_tol = UA_ang_lo_tol_val;
					{
					char		us_t139[1025];
					char		us_t140[1025];
					strcpy(toltext,"+");
					ua_dual_degree_tol_text(&((*e)),UA_ang_up_tol_val,us_t139);
					strcat(toltext,us_t139);
					strcat(toltext,"\n-");
					ua_degree_tol_text(&((*e)),UA_ang_lo_tol_val,us_t140);
					strcat(toltext,us_t140);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt2;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = ( (*e).
			    sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).
			    sub_sup_ratio;
			}
			break;
		case 7:
			{
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t141[1025];
					char		us_t142[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).d_lin_up_tol,(*e).du_tol_pl,(*e).
					    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
					UA_d_dec_symbol,us_t141);
					strcat(toltext,us_t141);
					strcat(toltext,"\n-");
					ua_value_to_string(0.000000e+000,(*e).du_tol_pl,(*e).
					    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
					UA_d_dec_symbol,us_t142);
					strcat(toltext,us_t142);
					}
				}
			else
				{
				(*e).upper_tol = UA_ang_up_tol_val;
				(*e).lower_tol = UA_ang_lo_tol_val;
					{
					char		us_t143[1025];
					char		us_t144[1025];
					strcpy(toltext,"+");
					ua_dual_degree_tol_text(&((*e)),UA_ang_up_tol_val,us_t143);
					strcat(toltext,us_t143);
					strcat(toltext,"\n-");
					ua_dual_degree_tol_text(&((*e)),0.000000e+000,us_t144);
					strcat(toltext,us_t144);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt2;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = ( (*e).
			    sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).
			    sub_sup_ratio;
			}
			break;
		case 8:
			{
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t145[1025];
					char		us_t146[1025];
					strcpy(toltext,"+");
					ua_value_to_string(0.000000e+000,(*e).du_tol_pl,(*e).
					    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
					UA_d_dec_symbol,us_t145);
					strcat(toltext,us_t145);
					strcat(toltext,"\n-");
					ua_value_to_string((*e).d_lin_lo_tol,(*e).du_tol_pl,(*e).
					    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
					UA_d_dec_symbol,us_t146);
					strcat(toltext,us_t146);
					}
				}
			else
				{
				(*e).upper_tol = UA_ang_up_tol_val;
				(*e).lower_tol = UA_ang_lo_tol_val;
					{
					char		us_t147[1025];
					char		us_t148[1025];
					strcpy(toltext,"\\+");
					ua_dual_degree_tol_text(&((*e)),0.000000e+000,us_t147);
					strcat(toltext,us_t147);
					strcat(toltext,"\n-");
					ua_dual_degree_tol_text(&((*e)),UA_ang_lo_tol_val,us_t148);
					strcat(toltext,us_t148);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt2;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = ( (*e).
			    sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).
			    sub_sup_ratio;
			}
			break;
		}
		{
		int		us_t151;
		us_t151 = (*e).txt_blk_use;
		i = 1;
		for(;;)
			{
			if( i > us_t151 ) 	break;
			(*e).txt_blk[i-1].txt_just = 2;
us_l149:
			i++ ;
			}
us_l150: 
		;
		}
	format = (*e).dual_format;
	if( ( ( ( ( format==5 )||( format==6 ) )||( format==7 ) )||
	    ( format==8 ) ) )
		{
		if( ( ( ( ( ( (*e).tol_method==2 )||( (*e).tol_method==4 ) )
		    ||( (*e).tol_method==7 ) )||( (*e).tol_method==8 ) )||( (*e)
		    .tol_method==6 ) ) )
			{
			uu_dexit;
			return;
			}
		else
			{
			strcpy(toltext,(*e).txt_blk[dblk-1].tstring);
			strcpy(dtext,"[");
			strcat(dtext,toltext);
			(*e).txt_blk[dblk-1].char_cnt = strlen(dtext);
			strcpy((*e).txt_blk[dblk-1].tstring,dtext);
			i = (*e).txt_blk_use;
			strcpy(toltext,(*e).txt_blk[i-1].tstring);
			strcpy(dtext,toltext);
			strcat(dtext,"]");
			(*e).txt_blk[i-1].char_cnt = strlen(dtext);
			strcpy((*e).txt_blk[i-1].tstring,dtext);
			}
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_user_text_mtol(e, mblk)
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
ua_user_text_mtol(e, mblk)
struct UA_generic_draft	(*e);
int		mblk;
	{
	char		fractext[51];
	char		dentext[51];
	char		toltext[51];
	char		numtext[51];
	int		i;
	char		lotext[51];
	char		uptext[51];
	char		dtext[51];
	int		start;

	uu_denter(UU_STRC,(us,"SAL ua_user_text_mtol(e=%s, mblk=%d)",
		"...", mblk));

	if( ( mblk==0 ) )
		{
		uu_dexit;
		return;
		}
	switch( (*e).tol_method )
		{
		case 0:
			{
			uu_dexit;
			return;
			}
		case 4:
		case 3:
		case 1:
		case 2:
			{
			uu_dexit;
			return;
			}
		case 5:
			{
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t152[1025];
					strcpy(toltext,"\\+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
					UA_dec_symbol,us_t152);
					strcat(toltext,us_t152);
					}
				}
			else
				{
				(*e).upper_tol = UA_ang_up_tol_val;
				(*e).lower_tol = UA_ang_lo_tol_val;
					{
					char		us_t153[1025];
					strcpy(toltext,"\\+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t153);
					strcat(toltext,us_t153);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			}
			break;
		case 6:
			{
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t154[1025];
					char		us_t155[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
					UA_dec_symbol,us_t154);
					strcat(toltext,us_t154);
					strcat(toltext,"\n-");
					ua_value_to_string((*e).lower_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
					UA_dec_symbol,us_t155);
					strcat(toltext,us_t155);
					}
				}
			else
				{
				(*e).upper_tol = UA_ang_up_tol_val;
				(*e).lower_tol = UA_ang_lo_tol_val;
					{
					char		us_t156[1025];
					char		us_t157[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t156);
					strcat(toltext,us_t156);
					strcat(toltext,"\n-");
					ua_degree_tol_text(&((*e)),(*e).lower_tol,us_t157);
					strcat(toltext,us_t157);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = ( (*e).
			    sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).
			    sub_sup_ratio;
			}
			break;
		case 7:
			{
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t158[1025];
					char		us_t159[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
					UA_dec_symbol,us_t158);
					strcat(toltext,us_t158);
					strcat(toltext,"\n-");
					ua_value_to_string(0.000000e+000,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
					UA_dec_symbol,us_t159);
					strcat(toltext,us_t159);
					}
				}
			else
				{
				(*e).upper_tol = UA_ang_up_tol_val;
				(*e).lower_tol = UA_ang_lo_tol_val;
					{
					char		us_t160[1025];
					char		us_t161[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t160);
					strcat(toltext,us_t160);
					strcat(toltext,"\n-");
					ua_degree_tol_text(&((*e)),0.000000e+000,us_t161);
					strcat(toltext,us_t161);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = ( (*e).
			    sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).
			    sub_sup_ratio;
			}
			break;
		case 8:
			{
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t162[1025];
					char		us_t163[1025];
					strcpy(toltext,"+");
					ua_value_to_string(0.000000e+000,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
					UA_dec_symbol,us_t162);
					strcat(toltext,us_t162);
					strcat(toltext,"\n-");
					ua_value_to_string((*e).lower_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
					UA_dec_symbol,us_t163);
					strcat(toltext,us_t163);
					}
				}
			else
				{
				(*e).upper_tol = UA_ang_up_tol_val;
				(*e).lower_tol = UA_ang_lo_tol_val;
					{
					char		us_t164[1025];
					char		us_t165[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),0.000000e+000,us_t164);
					strcat(toltext,us_t164);
					strcat(toltext,"\n-");
					ua_degree_tol_text(&((*e)),(*e).lower_tol,us_t165);
					strcat(toltext,us_t165);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = ( (*e).
			    sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).
			    sub_sup_ratio;
			}
			break;
		case 9:
			{
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t166[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
					UA_dec_symbol,us_t166);
					strcat(toltext,us_t166);
					}
				}
			else
				{
				(*e).upper_tol = UA_ang_up_tol_val;
				(*e).lower_tol = UA_ang_lo_tol_val;
					{
					char		us_t167[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t167);
					strcat(toltext,us_t167);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = (*e).char_size;
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).
			    sub_sup_ratio;
			}
			break;
		case 10:
			{
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t168[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).lower_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
					UA_dec_symbol,us_t168);
					strcat(toltext,us_t168);
					}
				}
			else
				{
				(*e).upper_tol = UA_ang_up_tol_val;
				(*e).lower_tol = UA_ang_lo_tol_val;
					{
					char		us_t169[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),(*e).lower_tol,us_t169);
					strcat(toltext,us_t169);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = (*e).char_size;
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).
			    sub_sup_ratio;
			}
			break;
		}
		{
		int		us_t172;
		us_t172 = (*e).txt_blk_use;
		i = 1;
		for(;;)
			{
			if( i > us_t172 ) 	break;
			(*e).txt_blk[i-1].txt_just = 2;
us_l170:
			i++ ;
			}
us_l171: 
		;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_apptext_subf(entity)
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
ua_apptext_subf(entity)
struct UA_generic_draft	(*entity);
	{
	int		choice;
	int		tblock;
	int		d_stat;
	int		num_lines;

	uu_denter(UU_STRC,(us,"SAL ua_apptext_subf(entity=%s)", "..."));

	if( ( (*entity).appn_text!=4 ) )
		{
LAB1:
		tblock = ( (*entity).txt_blk_use+1 );
		d_stat = 1;
		num_lines = 0;
		ua_get_note(48,1024,(*entity).txt_blk[tblock-1].tstring,
						&((*entity).txt_blk[tblock-1].char_cnt), &num_lines);
		if( ( (*entity).txt_blk[tblock-1].char_cnt>0 ) )
			{
			(*entity).txt_blk_use = tblock;
			(*entity).txt_blk[tblock-1].subtype = app_cre_txt;
			}
		if( ( d_stat==2 ) )
			{
			d_stat = ua_popmenu(96,&(choice));
			switch( choice )
				{
				case 1:
					{
					goto LAB1;
					}
				case 2:
					{
					}
					break;
				case 3:
					{
					}
					break;
				}
			}
		}
	uu_dexit;
	}
