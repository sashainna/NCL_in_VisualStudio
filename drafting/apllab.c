/*********************************************************************
**    NAME         : apllab.c
**       CONTAINS:
**          ua_balloon
**          ua_balloon_regen
**          ua_balloon_count
**          ua_edit_balloon
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       apllab.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:37
*********************************************************************/
#ifdef UU_SINGLE
   static char  uu_sccsident[] = {"@(#) apllab.c 3.4 3/24/89 08:26:24 single"};
#else
   static char  uu_sccsident[] = {"@(#) apllab.c 3.4 3/24/89 08:26:24 double"};
#endif

#include  "ustrings.h"
#include  "usysdef.h"
#include  "umath.h"
#include  "modef.h"
#include  "ulist.h"
#include  "udebug.h"
#include  "umoveb.h"
#include  "adraft.h"
#include  "adrfcom.h"
#include  "adrfdefs.h"
#include  "dasnog.h"
#include  "mdcoord.h"
#include  "class.h"
#include <ctype.h>

#define  UA_DONE          0
#define  UA_TEXT_HORIZ    1
#define  UA_USERDEF       1

extern int  UD_draftable[UD_NMENTWD];
extern int UD_editable[UD_NMENTWD];
extern UU_KEY_ID  UA_drafting_view;
extern int UA_balloon_type;
extern int UA_balloon_symbol_loc;


/*********************************************************************
**    E_FUNCTION     : int  ua_balloon ()
**       Get coordinate or entity location, note text and note origin,
**       and create the balloon with leader line and note.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_balloon ()
	{
   int 		d_stat, mode;
	UU_LOGICAL done;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon()"));

	done = UU_FALSE;
	while(!done)
		{
		/* get users choice for BALLOON type */
		d_stat = ua_popmenu(29, &mode);
	
		/* check for ERROR returns */
		if(  d_stat == 2  )
			{
			goto fexit;
			}
		if(  (mode > 4 || d_stat == 0) || (d_stat == 1 && mode == 0) )
			{
			goto fexit;
			}
	
		/* switch on users choice for type */
		switch( mode )
			{
			case 1:
				ua_balloon_simple(1);
				break;
			case 2:
				ua_balloon_simple(2);
				break;
			case 3:
				ua_balloon_X_multi();
				break;
			case 4:
				ua_balloon_PLCS_multi();
				break;
			}
		}

fexit:;
   uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_balloon_regen( edrf)
**			Re-generate a balloon entity
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_balloon_regen( edrf)
	struct UA_generic_draft *edrf;
	{
	int i;
	UU_REAL uu;
	UM_coord location, pt, del_vec;
	UU_LOGICAL first;

	uu_denter(UU_STRC,(us,"ua_balloon_regen()"));

	/* transform asso_blk coordinates if required */
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

	/* check for a multiplier and set parameters accordingly */
	UA_balloon_type = 1;
	UA_balloon_symbol_loc = 0;
	if(edrf->txt_blk_use == 2)
		{
		switch(edrf->txt_blk[1].subtype)
			{
			case app_cre_txt:
				UA_balloon_type = 4;
				edrf->txt_blk_use = 1;
				edrf->txt_blk[1].char_cnt = 0;
				edrf->txt_blk[1].tstring[0] = '\0';
				break;
			case main_txt2:
				UA_balloon_type = 2;
				if(edrf->txt_blk[1].tstring[edrf->txt_blk[1].char_cnt - 1] == 'x')
					{
					edrf->txt_blk[1].char_cnt--;
					edrf->txt_blk[1].tstring[edrf->txt_blk[1].char_cnt] = '\0';
					}
				break;
			case tol_txt1:
			case tol_txt2:
				UA_balloon_type = 3;
				if(edrf->txt_blk[1].tstring[edrf->txt_blk[1].char_cnt - 1] == 's')
					{
					edrf->txt_blk[1].char_cnt = edrf->txt_blk[1].char_cnt - 4;
					edrf->txt_blk[1].tstring[edrf->txt_blk[1].char_cnt] = '\0';
					}
				switch(edrf->txt_blk[1].subtype)
					{
					case tol_txt1:
						UA_balloon_symbol_loc = 1;
						break;
					case tol_txt2:
						UA_balloon_symbol_loc = 2;
						break;
					}
				break;
			}
		}
	ua_balloon_create(edrf);
	uu_dexit;
	return(0);
	}

/*********************************************************************
**    E_FUNCTION     : ua_balloon_count( id_value, count)
**			Count number of balloons with given ID numbers
**    PARAMETERS   
**       INPUT  : 
**          id_value						given ID number
**       OUTPUT :  
**          count							count of balloons with given ID number
**												includes a count of all leaders
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_balloon_count(id_value, number, count)
	UU_REAL id_value[];
	int     number, count[];
	{
	int        i, status, rel_num, multiplier;
	UU_LOGICAL init;
	UU_KEY_ID  key, view_key;
	struct UC_entitydatabag  eptr;
	struct UA_generic_draft *edrf;


	uu_denter(UU_STRC,(us,"ua_balloon_count()"));

	init = UU_TRUE;
	rel_num = UA_LINEAR_DIM;
	for(i=0;i<number;i++) count[i] = 0;
	while(uv_getalldispinrelation(init, rel_num, &key) == UU_SUCCESS)
		{
		eptr.key = key;
		eptr.rel_num = rel_num;
		if(ur_retrieve_view_key(key, &view_key) == UU_SUCCESS)
			{
			if(view_key == UA_drafting_view)
				{
				status = uc_retrieve_data(&eptr, sizeof(struct UC_entitydatabag));
				if(status == UU_SUCCESS)
					{
					edrf = (struct UA_generic_draft *) &eptr;
					if(edrf->etype == UA_BALLOON_DIM)
						{
						for(i=0;i<number;i++)
							{

							/* check for undefined ID or multiplier */
							if(edrf->dim_value == 0.0 || edrf->dim2_value == 0.0) 
															ua_balloon_update_ID_value(edrf);
							else
								{
								if(fabs(id_value[i] - edrf->dim_value) < 0.1)
									{
									multiplier = edrf->dim2_value;
									if(multiplier < 0)
										count[i] = multiplier;
									else
										count[i] = count[i] + edrf->arrow_blk_use	* multiplier;
									}
								}
							}
						}
					}
				}
			}
		init = UU_FALSE;
		}

	uu_dexit;
	return(0);
	}

/*********************************************************************
**    E_FUNCTION     : int  ua_edit_balloon ()
**       MAin user routine to support the BALLOON edit function.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_edit_balloon ()

	{
   struct UA_generic_draft	pl;       
	struct UA_PLOCREC			plocrec;
	struct UA_PICKENT			pickent;
	UU_KEY_ID 					curr_key, save_key, key, view_key;
	int							i, j, k, count, d_status, num1, msg, num_entries, len,
									relation, status, mode, mode1, d_stat, length, retstat,
									start, off_set, asso_indx[10];
   UM_coord  					stub_pt,pts[10],npt,loc,cporig,cpxaxis,cpyaxis,cpzaxis;
	UU_REAL 						uu;
	UU_LOGICAL  				init, done,first;           
	char							tmp_str[1024];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_edit_balloon()"));

	done = UU_FALSE;
	/* Loop editing balloon entities until user selects DONE. */
   while (! done)
		{
	
		/* get user selection of the BALLOON */
		ud_lgeo(UU_TRUE, UD_editable);
		d_status = ud_pick_loc(UA_DRAFTING, 164, &plocrec, 1, &num1, UU_FALSE);
		if(d_status == UA_REJECT || num1 < 1) goto fexit;

		d_status = um_d_pickresolve(&plocrec.ppick, 1, &pickent);
		curr_key = um_get_pickkey(&pickent, 1);
		pl.key = curr_key;
	
		/* check that the users actually selected a BALLOON */
		d_status = um_retrieve_data_relnum(pl.key, &relation);
		if(  relation != UA_LINEAR_DIM )
			{
			uu_uerror0(UA_DRAFTING,46);
			goto fexit;
			}
		uc_retrieve_data(&pl, sizeof(struct UA_generic_draft	)) ;
		if(pl.etype != UA_BALLOON_DIM)
			{
			uu_uerror0(UA_DRAFTING,46);
			goto fexit;
			}

		/* get users selection of the EDIT option */
		d_stat = ua_popmenu(28, &mode);
		if(  d_stat == 2 || (mode > 9 || d_stat == 0)
								|| (d_stat == 1 && mode == 0)  ) goto fexit;
	
		/* switch on EDIT option selected by the user */
		switch(mode)
			{
			case 1:			/* modify ID number */
				strcpy(tmp_str, pl.txt_blk[0].tstring);
repeat1:;
				status = ud_string_def(UA_DRAFTING, 125, tmp_str, 1024, 
								&length, &retstat);
				len = strlen(tmp_str);
				if( status != UA_OPCOMPLETE || len <= 0 ) goto fexit;
				if(len <= 3)
					{
					for(i=0;i<len;i++)
						{
						if(isdigit(tmp_str[i]) == 0)
							{
							uu_uerror0(UA_DRAFTING,52);
							goto repeat1;
							}
						}
					strcpy(pl.txt_blk[0].tstring, tmp_str);
					pl.txt_blk[0].char_cnt = len;
					ua_regen_drafting(&pl);
					}
				else
					uu_uerror0(13,45);
				break;
			case 2:
				if(pl.subtype == 1)
					{
					pl.dim2_value = -99.0;
					pl.subtype = 4;
					pl.txt_blk_use = 2;
					ua_initent_txt_blk(&pl, 2, 1);
					pl.txt_blk[1].subtype = app_cre_txt;
					pl.txt_blk[1].char_cnt = 0;
					pl.txt_blk[1].tstring[0] = '\0';
					ua_regen_drafting(&pl);
					}
				break;
			case 3:
				if(pl.subtype == 4)
					{
					pl.dim2_value = 1.1;
					pl.subtype = 1;
					pl.txt_blk_use = 1;
					ua_regen_drafting(&pl);
					}
				break;
			case 4:			/* change multiplier value */
				if(pl.txt_blk_use > 1 && pl.txt_blk[1].subtype != app_cre_txt)
					{
					strcpy(tmp_str, pl.txt_blk[1].tstring);
					switch(pl.txt_blk[1].subtype)
						{
						case main_txt2:
							pl.txt_blk[1].char_cnt--;
							break;
						case tol_txt1:
						case tol_txt2:
							pl.txt_blk[1].char_cnt = pl.txt_blk[1].char_cnt - 4;
						}
					tmp_str[pl.txt_blk[1].char_cnt] = '\0';
repeat2:;
					status = ud_string_def(UA_DRAFTING, 125, tmp_str, 1024, 
										&length, &retstat);
					if( status != UA_OPCOMPLETE || length <= 0 ) goto fexit;

					len = strlen(tmp_str);
					for(i=0;i<len;i++)
						{
						if(isdigit(tmp_str[i]) == 0)
							{
							uu_uerror0(UA_DRAFTING,53);
							goto repeat2;
							}
						}

					strcpy(pl.txt_blk[1].tstring, tmp_str);
					pl.txt_blk[1].char_cnt = len;
					ua_regen_drafting(&pl);
					}
				break;

			case 5:			/* Add X , or PLCS multiplier */
			case 6:
					if(pl.arrow_blk_use == 1)
						{
						/* initilize MULTIPLIER text record */
	               pl.txt_blk_use = 2;
						ua_initent_txt_blk(&pl, 2, 1);
repeat3:;
	           		/* Get multiplier value from the user */
		            status = ua_get_pl_text(168,1024,pl.txt_blk[1].tstring,
	                                           &pl.txt_blk[1].char_cnt);

						strcpy(tmp_str, pl.txt_blk[1].tstring);
						len = strlen(tmp_str);
						for(i=0;i<len;i++)
							{
							if(isdigit(tmp_str[i]) == 0)
								{
								uu_uerror0(UA_DRAFTING,53);
								goto repeat3;
								}
							}
	
						/* fix up text block according to type */
						if(mode == 5)
							{
							pl.subtype = 2;
							pl.txt_blk[1].subtype = main_txt2;
							}
						else
							{
							pl.subtype = 3;

							/* get users choice for PLCS symbol location type */
							d_stat = ua_popmenu(30, &mode1);
						
							/* check for ERROR returns */
							if(  d_stat == 2  ) goto fexit;
							if(  (mode1 > 2 || d_stat == 0) || (d_stat == 1 && mode1 == 0) ) goto fexit;
	
							/* set location mode for PLCS symbol selected by the user */
							if(mode1 == 1)
								pl.txt_blk[1].subtype = tol_txt1;
							else
								pl.txt_blk[1].subtype = tol_txt2;
							}
						ua_regen_drafting(&pl);
						}
					else
						uu_uerror0(13,48);   /* ERROR - user selected a BALLOON with multiple leaders */
				break;

			case 7:			/* delete multiplier */
				if(pl.txt_blk_use == 2 && pl.txt_blk[1].subtype != app_cre_txt)
					{
					pl.subtype = 1;
					pl.txt_blk_use--;
					pl.dim2_value = 1.1;
					ua_regen_drafting(&pl);
					}
				break;

			case 8:							/* add a leader */

				if(pl.txt_blk_use > 1 && pl.txt_blk[i].subtype != app_cre_txt)
					{
					uu_uerror0(13,49);   /* ERROR - user selected a BALLOON with a multiplier */
					}
				else
					{
					if(pl.arrow_blk_use < 9)
						{
						/* Get arrowhead endpoint. */
				      ud_lgeo(UU_TRUE, UD_draftable);
						status = ua_balloon_leader(&pl);
			         if(status == UA_OPCOMPLETE) 
							{
		               k = pl.asso_blk_use;
		               um_vctovc(pl.asso_blk[k-1].location, loc);
		      			ua_getcpln(&pl, cporig, cpxaxis, cpyaxis, cpzaxis);
		               um_nptpln(loc, cporig, cpzaxis, npt);
		               um_vctovc(npt,pl.asso_blk[k-1].location);
		               pl.asso_blk[k-1].asso_type = 1;
		               pl.asso_blk[k-1].modifier = UA_TEXT_HORIZ;
							if(pl.asso_blk[k-1].key != 0)
								{
								ua_para_on_curve(pl.asso_blk[k-1].key, pl.asso_blk[k-1].location, &uu);
								if(uu < 0.0)
									uu = 0.0;
								else if(uu > 1.0)
									uu = 1.0;
								k++;
								pl.asso_blk[k-1].location[0] = uu;
								pl.asso_blk[k-1].location[1] = 0.0;
								pl.asso_blk[k-1].location[2] = 0.0;
								pl.asso_blk[k-1].asso_type = -99;
		               	pl.asso_blk[k-1].modifier = UA_TEXT_HORIZ;
		               	pl.asso_blk[k-1].key = 0;
								pl.asso_blk[k-2].asso_type = k - 1;
		               	pl.asso_blk_use = k;
								}
							ua_regen_drafting(&pl);
		            	}
						}
					}
            break;

			case 9:			/* delete a leader */

				if(pl.arrow_blk_use < 2)
							uu_uerror0(UA_DRAFTING,47);
				else
					{
					/* get user selection of the ARROWHEAD */
					ud_lgeo(UU_TRUE, UD_editable);
					d_status = ud_pick_loc(UA_DRAFTING, 165, &plocrec, 1, &num1, UU_FALSE);
					if(d_status == UA_REJECT || num1 < 1) goto fexit;
			
					d_status = um_d_pickresolve(&plocrec.ppick, 1, &pickent);
					curr_key = um_get_pickkey(&pickent, 1);
				
					/* check that the users actually selected the correct BALLOON */
					if(  pl.key != curr_key )
									uu_uerror0(UA_DRAFTING,47);
					else
						{
						ua_sort_asso_blk(&pl);
						count = 0;
						for(i=0;i<pl.asso_blk_use;i++)
							{
							if(pl.asso_blk[i].asso_type != -99)
								{
								um_vctovc(pl.asso_blk[i].location, pts[count]);
								asso_indx[count] = i;
								count++;
								}
							}
						um_vctovc(pl.line_blk[0].line_seg[6], stub_pt);
						j = ua_closest_leader(&plocrec.ndcloc, pl.arrow_blk_use,
															pts, stub_pt);
						count = asso_indx[j];
						if(pl.asso_blk[count].key == 0 && pl.asso_blk[count].asso_type != -99)
							{
							start = count + 1;
							off_set = 1;
							}
						else
							{
							start = count + 2;
							off_set = 2;
							}

						while(start <= pl.asso_blk_use)
							{
							pl.asso_blk[count].asso_type = pl.asso_blk[start].asso_type;
							pl.asso_blk[count].modifier  = pl.asso_blk[start].modifier;
							pl.asso_blk[count].key       = pl.asso_blk[start].key;
							um_vctovc(pl.asso_blk[start].location, pl.asso_blk[count].location);
							if(pl.asso_blk[count].key != 0 &&
												pl.asso_blk[count].modifier != -99)	
								{
								pl.asso_blk[count].asso_type = pl.asso_blk[count].asso_type - off_set;
								}
							count++;
							start++;
							}
						pl.asso_blk_use = pl.asso_blk_use - off_set;
						ua_regen_drafting(&pl);
						}
					}
				break;
			}
		}

fexit:;
   uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int  ua_closest_leader ()
**       Find BALLOON leader closest to PLOC
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_closest_leader (plocrec, number, pts, stub_pt)
	struct UA_NDCLOCREC		*plocrec;
	int							number;
	UM_coord						pts[10], stub_pt;
	{
	int							i, jj;
	UU_REAL 						dist, dist_ln, um_dcccc();
	UM_coord						lpt, lvec, ploc_pt, proj_pt;
	UU_LOGICAL 					ua_between();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_closest_leader()"));

	um_ploctocc(plocrec, ploc_pt);

	jj = 0;
	dist = 100000.;
	for(i=0;i<number;i++)
		{
		um_vctovc(pts[i], lpt);
		um_vcmnvc(lpt, stub_pt, lvec);
		um_unitvc(lvec, lvec);
		um_nptln(ploc_pt, lpt, lvec, proj_pt);
		if(ua_between(lpt, proj_pt, stub_pt))
			{
			dist_ln = um_dcccc(ploc_pt, proj_pt);
			if(dist_ln < dist)
				{
				dist = dist_ln;
				jj = i;
				}
			}
		}
fexit:;
   uu_dexit;
	return(jj);
	}

/*********************************************************************
**    E_FUNCTION     : int  ua_sort_asso_blk ()
**       Sort BALLOON association blk's
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_sort_asso_blk (pl)
	struct UA_generic_draft		*pl;
	{
	int							i, count, indx, next;
	struct UA_asso_blk		asso_list[50];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_sort_asso_blk()"));

	for(i=0;i<pl->asso_blk_use;i++)
		{
		uu_dprint(UU_MTRC,(us,"i = %d, asso_type = %d, key = %d, loc[0] = %g",
		i,pl->asso_blk[i].asso_type, pl->asso_blk[i].key, pl->asso_blk[i].location[0]));
		}
	count = indx = 0;
	while(count < pl->asso_blk_use)
		{
		if(pl->asso_blk[count].key !=0 && pl->asso_blk[count].asso_type != -99)
			{
			next = pl->asso_blk[count].asso_type;
			pl->asso_blk[count].asso_type = indx + 1;
			asso_list[indx].asso_type = pl->asso_blk[count].asso_type;
			asso_list[indx].modifier  = pl->asso_blk[count].modifier;
			asso_list[indx].key       = pl->asso_blk[count].key;
			um_vctovc(pl->asso_blk[count].location, asso_list[indx].location);
			indx++;
			asso_list[indx].asso_type = pl->asso_blk[next].asso_type;
			asso_list[indx].modifier  = pl->asso_blk[next].modifier;
			asso_list[indx].key       = pl->asso_blk[next].key;
			um_vctovc(pl->asso_blk[next].location, asso_list[indx].location);
			indx++;
			count++;
			}
		else if(pl->asso_blk[count].key == 0 && pl->asso_blk[count].asso_type != -99)
			{
			asso_list[indx].asso_type = pl->asso_blk[count].asso_type;
			asso_list[indx].modifier  = pl->asso_blk[count].modifier;
			asso_list[indx].key       = pl->asso_blk[count].key;
			um_vctovc(pl->asso_blk[count].location, asso_list[indx].location);
			count++;
			indx++;
			}
		else
			count++;

		}

	uu_dprint(UU_MTRC,(us,"count = %d, indx = %d", count, indx));
	for(i=0;i<count;i++)
		{
		uu_dprint(UU_MTRC,(us,"i = %d, asso_type = %d, key = %d, loc[0] = %g",
		i,asso_list[i].asso_type, asso_list[i].key, asso_list[i].location[0]));
		}

	for(i=0;i<count;i++)
		{
		pl->asso_blk[i].asso_type = asso_list[i].asso_type;
		pl->asso_blk[i].modifier = asso_list[i].modifier;
		pl->asso_blk[i].key = asso_list[i].key;
		um_vctovc(asso_list[i].location, pl->asso_blk[i].location);
		}

fexit:;
   uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    I_FUNCTION     : UU_LOGICAL ua_between( pt1, ptb, pt2 )
**			Return UU_TRUE if point ptb is on line segment between point
**			pt1 and point pt2. 
**    PARAMETERS   
**       INPUT  : 
**				pt1  the first  endpoint
**				ptb  the point to test to be on the segment between pt1, pt2
**				pt2  the second endpoint 
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_TRUE  if ptb is between pt1 and pt2;  UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL ua_between( pt1, ptb, pt2 )

	UM_coord		pt1;
	UM_coord		ptb;
	UM_coord		pt2;

	{
	UM_vector	v2;			/* endpoint vector */
	UM_vector	u2;			/* unit vector of v2 */
	UM_length	m2;			/* magnitude of endpoint vector */
	UM_vector	vp;			/* projection point vector */
	UM_length	mp;			/* magnitude of projection point vector */
	UU_LOGICAL between;		/* TRUE if "ptb" is between "pt1" and "pt2" */

/*--------------------------------------------------------------------
**	Start of Executable Code
**--------------------------------------------------------------------
**
**	Test if point ptb is between point pt1 and pt2
*/
	uu_denter(UU_MTRC,(us,"ua_between(?,?,?)"));

	between = UU_FALSE;

	if (um_cceqcc(pt1, pt2))
		{
		if (um_cceqcc(pt1, ptb))
			between = UU_TRUE;
		else 
			between = UU_FALSE;
		goto Done;
		}

	um_vcmnvc( pt2, pt1, v2 );
	um_unitvc( v2, u2 );
	m2 = um_mag(v2);

	um_vcmnvc( ptb, pt1, vp );
	mp = um_mag(vp);

	if (um_dot(u2, vp) > 0 && mp <= m2 + UM_FUZZ)
		{
		between = UU_TRUE;
		goto Done;
		}
	else
		{

		between = UU_FALSE;
		goto Done;
		}

Done:
	uu_dexit;
	return (between);
	}
