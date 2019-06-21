/*********************************************************************
**    NAME         : apllab1.c
**       CONTAINS:
**          ua_balloon_simple
**          ua_balloon_X_multi
**    		ua_balloon_PLCS_multi
**          ua_balloon_create
**          ua_balloon_leader
**    		ua_balloon_asso_blk 
**    		ua_balloon_find_no_loc 
**    		ua_balloon_store
**    		ua_balloon_ALT_ACT 
**    		ua_balloon_init
**    		ua_balloon_update_ID_value
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       apllab1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:37
*********************************************************************/
#ifdef UU_SINGLE
   static char  uu_sccsident[] = {"@(#) apllab1.c 3.1 3/24/89 17:15:06 single"};
#else
   static char  uu_sccsident[] = {"@(#) apllab1.c 3.1 3/24/89 17:15:06 double"};
#endif

#include  "ustrings.h"
#include  "usysdef.h"
#include  "umath.h"
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
char	*uu_toolmalloc();
int UA_balloon_type;
int UA_balloon_symbol_loc;

static char *msg = {"Enter the find number."};
static char *msg1 = {"Enter the multiplier value."};


/*********************************************************************
**    E_FUNCTION     : int  ua_balloon_simple ( type )
**       Get coordinate or entity location, note text and note origin,
**       and create the balloon with leader line and note.
**    PARAMETERS
**       INPUT  :
**          type						balloon type ( 1 = simple, 2 = Reference)
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_balloon_simple (type)
	int type;

	{
   UM_coord  					cporig,cpxaxis,cpyaxis,cpzaxis,loc,npt,origin;
   int  							i,count,iassoc,status,d_stat,len;
   UU_LOGICAL  				done,first;           
	char 							tmp_str[1024];
   struct UA_generic_draft	e;       

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon_simple()"));

   done = UU_FALSE;
   first = UU_TRUE;
	if(type == 1)
		UA_balloon_type = 1;
	else
		UA_balloon_type = 4;

	/* Loop creating balloon entities until user selects DONE. */
   while (! done)
		{
		/* initialize BALLOON record */
		ua_balloon_init(&e, UA_balloon_type);
      ua_getcpln(&e, cporig, cpxaxis, cpyaxis, cpzaxis);

  		 /* Get first arrowhead endpoint. */
      ud_lgeo(UU_TRUE, UD_draftable);
      status = ua_balloon_leader(&e);

		/* check return status for user options */
      switch (status)
			{
         case UA_DONE:
            done = UU_TRUE;
            break;

         case UA_OPCOMPLETE: 
				
				/* standard return - get any additional arrowheads from user */
            while (status == UA_OPCOMPLETE)
					{
               iassoc = e.asso_blk_use;
					ua_balloon_asso_blk(&e, iassoc, cporig, cpzaxis);
               if (e.arrow_blk_use < 10)
						{
                  ud_lgeo(UU_TRUE, UD_draftable);
                  status = ua_balloon_leader(&e);
                	}
               else 
                  status = UA_DONE;
            	}

				/* get FIND NO. location from the user */
            count = ua_balloon_find_no_loc(&e, cporig, cpzaxis);

				/* check error status */
            if (count != 0)
					{

repeat:;
            	/* Get find number from the user */
               status = ua_get_pl_text(169,1024,e.txt_blk[0].tstring,
                                           &e.txt_blk[0].char_cnt);

					/* if we have good input from the user - store and display */
               if (e.txt_blk[0].char_cnt != 0 && e.txt_blk[0].char_cnt <= 3)
						{

						strcpy(tmp_str, e.txt_blk[0].tstring);
						len = strlen(tmp_str);
						for(i=0;i<len;i++)
							{
							if(isdigit(tmp_str[i]) == 0)
								{
								uu_uerror0(UA_DRAFTING,52);
								goto repeat;
								}
							}

						/* store and display current BALLOON */
						ua_balloon_store(&e);
                  first = UU_FALSE;
               	}
					else
						{
						uu_uerror0(13,45);
						}
            	}
            break;

         case UA_ALT_ACTION: 

				/* user selected ALT ACT */
            if (first == UU_TRUE) done = UU_TRUE;
            else 
					ua_balloon_ALT_ACT(&e, cporig, cpzaxis, &first);

            break;
			}
   	}

   uu_dexit;
   return(1);
	}


/*********************************************************************
**    E_FUNCTION     : int  ua_balloon_X_multi ()
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

int  ua_balloon_X_multi ()

	{
   UM_coord  					cporig,cpxaxis,cpyaxis,cpzaxis,loc,npt,origin;
   int  							i,count,iassoc,status, len;
	UU_REAL						uu;
   UU_LOGICAL  				done,first;           
	UU_KEY_ID					keyid;
	char							tmp_str[1024];
   struct UA_generic_draft	e;       


	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon_X_multi()"));

   done = UU_FALSE;
   first = UU_TRUE;
	UA_balloon_type = 2;

	/* Loop creating balloon entities until user selects DONE. */
   while (! done)
		{
		/* initialize BALLOON structure */
		ua_balloon_init(&e, 2);
      ua_getcpln(&e, cporig, cpxaxis, cpyaxis, cpzaxis);

  		 /* Get arrowhead endpoint. */
      ud_lgeo(UU_TRUE, UD_draftable);
      status = ua_balloon_leader(&e);

		/* check return status for user options */
      switch (status)
			{
         case UA_DONE:
            done = UU_TRUE;
            break;

         case UA_OPCOMPLETE: 
             iassoc = e.asso_blk_use;
				 ua_balloon_asso_blk(&e, iassoc, cporig, cpzaxis);
             status = UA_DONE;

				/* get FIND NO. location from the user */
            count = ua_balloon_find_no_loc(&e, cporig, cpzaxis);

				/* check error status */
            if (count != 0)
					{

repeat:;
            	/* Get find number from the user */
               status = ua_get_pl_text(169,1024,e.txt_blk[0].tstring,
                                           &e.txt_blk[0].char_cnt);

					/* check if we have good input from the user  */
               if (e.txt_blk[0].char_cnt != 0 && e.txt_blk[0].char_cnt <= 3)
						{

						strcpy(tmp_str, e.txt_blk[0].tstring);
						len = strlen(tmp_str);
						for(i=0;i<len;i++)
							{
							if(isdigit(tmp_str[i]) == 0)
								{
								uu_uerror0(UA_DRAFTING,52);
								goto repeat;
								}
							}


repeat1:;
            		/* Get multiplier value from the user */
	               status = ua_get_pl_text(168,1024,e.txt_blk[1].tstring,
                                           &e.txt_blk[1].char_cnt);

						strcpy(tmp_str, e.txt_blk[1].tstring);
						len = strlen(tmp_str);
						for(i=0;i<len;i++)
							{
							if(isdigit(tmp_str[i]) == 0)
								{
								uu_uerror0(UA_DRAFTING,53);
								goto repeat1;
								}
							}

						/* store and display current BALLOON entity */
						ua_balloon_store(&e);
                  first = UU_FALSE;
               	}
					else
						{
						uu_uerror0(13,45);
						}
            	}
            break;

         case UA_ALT_ACTION: 

				/* user selected ALT ACT */
            if (first == UU_TRUE) done = UU_TRUE;
            else 
					ua_balloon_ALT_ACT(&e, cporig, cpzaxis, &first);
            break;
			}
   	}

   uu_dexit;
   return(1);
	}


/*********************************************************************
**    E_FUNCTION     : int  ua_balloon_PLCS_multi ()
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

int  ua_balloon_PLCS_multi ()

	{
   UM_coord  					cporig,cpxaxis,cpyaxis,cpzaxis,loc,npt,origin;
   int  							i,count,iassoc,status, mode, d_stat, len;
	UU_REAL						uu;
   UU_LOGICAL  				done,first;           
	UU_KEY_ID					keyid;
	char							tmp_str[1024];
   struct UA_generic_draft	e;       

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon_PLCS_multi()"));

   done = UU_FALSE;
   first = UU_TRUE;
	UA_balloon_type = 3;

	/* get users choice for PLCS symbol location type */
	d_stat = ua_popmenu(30, &mode);

	/* check for ERROR returns */
	if(  d_stat == 2  )
		{
		goto fexit;
		}
	if(  (mode > 2 || d_stat == 0) || (d_stat == 1 && mode == 0) )
		{
		goto fexit;
		}
	UA_balloon_symbol_loc = mode;

	/* Loop creating balloon entities until user selects DONE. */
   while (! done)
		{
		/* initialize BALLOON structure */
		ua_balloon_init(&e, 3);
      ua_getcpln(&e, cporig, cpxaxis, cpyaxis, cpzaxis);

  		 /* Get arrowhead endpoint. */
      ud_lgeo(UU_TRUE, UD_draftable);
      status = ua_balloon_leader(&e);

		/* check return status for user options */
      switch (status)
			{
         case UA_DONE:
            done = UU_TRUE;
            break;

         case UA_OPCOMPLETE: 
             iassoc = e.asso_blk_use;
			  	 ua_balloon_asso_blk(&e, iassoc, cporig, cpzaxis);
             status = UA_DONE;

				/* get FIND NO. location from the user */
            count = ua_balloon_find_no_loc(&e, cporig, cpzaxis);

				/* check error status */
            if (count != 0)
					{

repeat:;
            	/* Get find number from the user */
               status = ua_get_pl_text(169,1024,e.txt_blk[0].tstring,
                                           &e.txt_blk[0].char_cnt);

					/* if we have good input from the user - store and display */
               if (e.txt_blk[0].char_cnt != 0 && e.txt_blk[0].char_cnt <= 3)
						{

						strcpy(tmp_str, e.txt_blk[0].tstring);
						len = strlen(tmp_str);
						for(i=0;i<len;i++)
							{
							if(isdigit(tmp_str[i]) == 0)
								{
								uu_uerror0(UA_DRAFTING,52);
								goto repeat;
								}
							}

repeat1:;
            		/* Get multiplier value from the user */
	               status = ua_get_pl_text(168,1024,e.txt_blk[1].tstring,
                                           &e.txt_blk[1].char_cnt);

						strcpy(tmp_str, e.txt_blk[1].tstring);
						len = strlen(tmp_str);
						for(i=0;i<len;i++)
							{
							if(isdigit(tmp_str[i]) == 0)
								{
								uu_uerror0(UA_DRAFTING,53);
								goto repeat1;
								}
							}

						/* store and display current BALLOON entity */
						ua_balloon_store(&e);
                  first = UU_FALSE;
               	}
					else
						{
						uu_uerror0(13,45);
						}
            	}
            break;

         case UA_ALT_ACTION: 

				/* user selected ALT ACT */
            if (first == UU_TRUE) done = UU_TRUE;
            else 
					ua_balloon_ALT_ACT(&e, cporig, cpzaxis, &first);
            break;
			}
   	}

fexit:;
   uu_dexit;
   return(1);
	}
	

/*********************************************************************
**    E_FUNCTION     : int  ua_balloon_create (e)
**       Create a balloon entity, store in UNIBASE, and display in DIGS.
**    PARAMETERS
**       INPUT  :
**          e                       balloon entity
**       OUTPUT :
**          e                       updated drafting entity with balloon
**                                  origin, note text and entity (asso_blk)
**                                  to point to.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_balloon_create (e)

struct UA_generic_draft  *e;   /* Pointer to balloon entity */

	{
   UM_coord  	arrowvec, spt, ept, xv, yv, del, cv, ctr_cc, loc_cc,
					stuborig, vector, cpln_origin, xaxis,
					yaxis, zaxis;
   int  			iarrow, iassoc, iline;
	UU_REAL     length, char_size, radius, corner[4][3];
	UU_REAL     zero = 0.0;
	char        buff[8];
	UU_TRUEDOUBLE ff, uai_atof();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon_create(e=%x)",e));

	/* get current entity text plane */
	ua_getcpln(e, cpln_origin, xaxis, yaxis, zaxis);

	/* define parameters for ID symbol outline */
	length = 1.5 * e->char_size;
	radius = 0.5*(e->char_size) + e->txt_gap;
	um_vctmsc(xaxis, 0.4*e->char_size, xv);

	/* fix up ID number text block data */
	e->txt_blk[0].dx = 3.0*e->char_size;
	e->txt_blk[0].dy = e->char_size;
	um_xyztovc(zero, zero, zero, e->txt_blk[0].origin);
	switch(e->txt_blk[0].char_cnt)
		{
		case 1:
			um_vctmsc(xv, (UU_REAL) 2.0, del);
			um_vcplvc(e->txt_blk[0].origin, del, e->txt_blk[0].origin);
			break;
		case 2:
			um_vcplvc(e->txt_blk[0].origin, xv, e->txt_blk[0].origin);
			break;
		}

	/* store ID value in value field for comparison with table values */
	ua_balloon_update_ID_value(e);

	/* store balloon lines in line block */
	iline = 0;
	e->line_blk_use = 1;
	um_vcplvc(e->dim_origin, xv, spt);
	um_vctovc(spt, e->line_blk[0].line_seg[iline]);
	iline++;
	um_vctmsc(xaxis, length, xv);
	um_vcplvc(spt, xv, e->line_blk[0].line_seg[iline]);
	iline++;
	um_vctmsc(yaxis, 2.0*radius, yv);
	um_vcplvc(spt, yv, spt);
	um_vctovc(spt, e->line_blk[0].line_seg[iline]);
	iline++;
	um_vcplvc(spt, xv, e->line_blk[0].line_seg[iline]);

	/* store balloon arcs in arc blocks */
	e->arc_blk_use = 2;
	e->arc_blk[0].num_pts = e->arc_blk[1].num_pts = 2;
	e->arc_blk[0].radius = e->arc_blk[1].radius = radius;
	um_vctmsc(yaxis, radius, yv);
	um_vcmnvc(spt, yv, spt);
	um_vctovc(spt, e->arc_blk[0].center_pt);
	um_vcplvc(spt, xv, e->arc_blk[1].center_pt);
	e->arc_blk[0].angles[0] = e->arc_blk[1].angles[1] = UA_HALFPI;
	e->arc_blk[0].angles[1] = e->arc_blk[1].angles[0] =  3.0*(UA_HALFPI);

	/* find text center point */
	um_vctmsc(xv, (UU_REAL) 0.5, xv);
	um_vcplvc(spt, xv, cv);

	/* test for location of stub */
	um_mcstoccs(0, cv, ctr_cc);
	um_mcstoccs(0, e->asso_blk[0].location, loc_cc);
   e->leader_loc = UA_MIDDLE;
   if (loc_cc[0] <= ctr_cc[0])
      e->lead_orient = UA_LEFT;
   else 
      e->lead_orient = UA_RIGHT;

	/* fix up text block data if required */
	if(e->txt_blk_use == 2 || UA_balloon_type == 4)
		{
		switch(UA_balloon_type)
			{
			case 4:    /* Reference Balloon */
				e->txt_blk_use = 2;
				e->txt_blk[1].subtype = app_cre_txt;
				strcpy(e->txt_blk[1].tstring, "REF");
				e->txt_blk[1].char_cnt = 3;
				ua_text_box(2, e, corner);
				switch(e->lead_orient)
					{
					case UA_LEFT:
						length = e->txt_gap;
						um_vctmsc(xaxis, (radius + length), vector);
						um_vcmnvc(e->arc_blk[1].center_pt, e->dim_origin, del);
						um_xyztovc(del[0], e->txt_blk[0].origin[1], zero, e->txt_blk[1].origin);
						um_vcplvc(e->txt_blk[1].origin, vector, e->txt_blk[1].origin);
						break;
					case UA_RIGHT:
						length = e->txt_gap;
						um_vctmsc(xaxis, radius, xv);
						um_vcmnvc(spt, xv, vector);
						um_vcmnvc(vector, e->dim_origin, del);
						um_xyztovc(del[0], e->txt_blk[0].origin[1], zero, e->txt_blk[1].origin);
						um_vctmsc(xaxis, (length + e->txt_blk[1].dx), vector);
						um_vcmnvc(e->txt_blk[1].origin, vector, e->txt_blk[1].origin);
						break;
					}
				break;
			case 2:    /* X multiplier */
				strcat(e->txt_blk[1].tstring, "x");
				e->txt_blk[1].char_cnt++;
				ua_text_box(2, e, corner);
				switch(e->lead_orient)
					{
					case UA_LEFT:
						length = e->txt_gap;
						um_vctmsc(xaxis, (radius + length), vector);
						um_vcmnvc(e->arc_blk[1].center_pt, e->dim_origin, del);
						um_xyztovc(del[0], e->txt_blk[0].origin[1], zero, e->txt_blk[1].origin);
						um_vcplvc(e->txt_blk[1].origin, vector, e->txt_blk[1].origin);
						break;
					case UA_RIGHT:
						length = e->txt_gap;
						um_vctmsc(xaxis, radius, xv);
						um_vcmnvc(spt, xv, vector);
						um_vcmnvc(vector, e->dim_origin, del);
						um_xyztovc(del[0], e->txt_blk[0].origin[1], zero, e->txt_blk[1].origin);
						um_vctmsc(xaxis, (length + e->txt_blk[1].dx), vector);
						um_vcmnvc(e->txt_blk[1].origin, vector, e->txt_blk[1].origin);
						break;
					}
				break;
			case 3:			/* PLCS multiplier */
				strcat(e->txt_blk[1].tstring, "plcs");
				e->txt_blk[1].char_cnt = e->txt_blk[1].char_cnt + 4;
				ua_text_box(2, e, corner);
				switch(UA_balloon_symbol_loc)
					{
					case 1:
						switch(e->lead_orient)
							{
							case UA_LEFT:
								length = e->txt_gap;
								um_vctmsc(xaxis, (radius + length), vector);
								um_vcmnvc(e->arc_blk[1].center_pt, e->dim_origin, del);
								um_xyztovc(del[0], e->txt_blk[0].origin[1], zero, e->txt_blk[1].origin);
								um_vcplvc(e->txt_blk[1].origin, vector, e->txt_blk[1].origin);
								break;
							case UA_RIGHT:
								length = e->txt_gap;
								um_vctmsc(xaxis, radius, xv);
								um_vcmnvc(spt, xv, vector);
								um_vcmnvc(vector, e->dim_origin, del);
								um_xyztovc(del[0], e->txt_blk[0].origin[1], zero, e->txt_blk[1].origin);
								um_vctmsc(xaxis, (length + e->txt_blk[1].dx), vector);
								um_vcmnvc(e->txt_blk[1].origin, vector, e->txt_blk[1].origin);
								break;
							}
						break;
					case 2:
						um_vctmsc(xaxis, 0.5*e->txt_blk[1].dx, vector);
						um_vctmsc(yaxis, 1.5*e->char_size, yv);
						um_vcmnvc(cv, e->dim_origin, del);
						um_xyztovc(del[0], e->txt_blk[0].origin[1], zero, e->txt_blk[1].origin);
						um_vcmnvc(e->txt_blk[1].origin, yv, e->txt_blk[1].origin);
						um_vcmnvc(e->txt_blk[1].origin, vector, e->txt_blk[1].origin);
					}
			}
		}

	/* calculate and store the stub vector */
	um_vctmsc(xaxis, radius, xv);
	switch(e->lead_orient)
		{
		case UA_LEFT:
			um_vcmnvc(spt, xv, spt);
			iline++;
			um_vctovc(spt, e->line_blk[0].line_seg[iline]);
			um_vctovc(spt, stuborig);
			um_vctmsc(xaxis, e->stub_length, xv);
			iline++;
			um_vcmnvc(e->line_blk[0].line_seg[iline-1], xv,
										e->line_blk[0].line_seg[iline]);
			um_vctovc(e->line_blk[0].line_seg[iline], stuborig);
			break;
		case UA_RIGHT:
			um_vcplvc(e->arc_blk[1].center_pt, xv, spt);
			iline++;
			um_vctovc(spt, e->line_blk[0].line_seg[iline]);
			um_vctmsc(xaxis, e->stub_length, xv);
			iline++;
			um_vcplvc(e->line_blk[0].line_seg[iline-1], xv,
										e->line_blk[0].line_seg[iline]);
			um_vctovc(e->line_blk[0].line_seg[iline], stuborig);
			break;
		}

	/* Define line and arrow blocks for leader line(s). */
	iarrow = 0;
   for (iassoc = 0; iassoc < e->asso_blk_use; iassoc++)
		{
		if(e->asso_blk[iassoc].asso_type != -99)
			{
 			/* store leader line  information */
 	      iline++;
 	      um_vctovc(stuborig,e->line_blk[0].line_seg[iline]);
 	      iline++;
 	      um_vctovc(e->asso_blk[iassoc].location,
 							e->line_blk[0].line_seg[iline]);
 	
 			/* store leader arrowhead information */
 	      um_vctovc(e->asso_blk[iassoc].location,e->arrow_blk[iarrow].location);
 	      um_vcmnvc(e->asso_blk[iassoc].location, stuborig, arrowvec);
 	      e->arrow_blk[iarrow].aangle = um_angle(xaxis, arrowvec);
 	      if (arrowvec[1] < 0.0)
 	         e->arrow_blk[iarrow].aangle = - (e->arrow_blk[iarrow].aangle);
 			iarrow++;
			}
   	}

   e->line_blk[0].num_pts = iline+1;
   e->arrow_blk_use = iarrow;

   uu_dexit;
   return(1);
	}


/*********************************************************************
**    E_FUNCTION     : int		ua_balloon_leader( pl)
**       Support routine for balloon leader line generation
**    PARAMETERS   
**       INPUT  : 
**				pl							balloon record
**       OUTPUT : 
**				pl							updated balloon record
**    RETURNS      : INTEGER		0	user rejected option
**											1  no leader required
**											2  user picked an entity(data in assoc blk)
**											3  user input screen pos(WC in assoc blk)
**											4  user selected AA (change entity origin)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_balloon_leader(pl)
struct UA_generic_draft	*pl;

	{
	struct UA_PLOCREC	plocrec;
	int					ablock, d_stat, status, mode, dummy, r_stat;
	UM_coord				pos;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uu_denter(UU_STRC,(us,"ua_balloon_leader"));

	r_stat = 0;

	/* get arrowhead location selection from the user */
	d_stat = ua_popmenu(26, &mode);

	/* check for ERROR returns */
	if(  d_stat == 2  )
		{
		r_stat = 2;
		goto fexit;
		}
	if(  (mode>2 || d_stat==0) || (d_stat == 1 && mode == 0) )
		{
		r_stat = 0;
		goto fexit;
		}

	/* switch on users choice for endpoint */
	switch( mode )
		{
		case 1:			/* endpoint on an entity */
			ablock = ( pl->asso_blk_use+1 );
			status = ua_select_ent_subf(160,pl,ablock,&(plocrec));
			if( status != 1 )
				{
				r_stat = status;
				goto fexit;
				}
			else
				{
				uc_near_on_entity(pl->asso_blk[ablock-1].key,
				   &(plocrec.ndcloc),pl->asso_blk[ablock-1].location);
				pl->asso_blk_use = ablock;
				r_stat = status;
				goto fexit;
				}
		case 2:			/* endpoint free */
			ablock = ( pl->asso_blk_use+1 );
			d_stat = ud_world_coord(UA_DRAFTING,161,pos,1,&(dummy),UU_FALSE);
			if(  d_stat != 1 || dummy == 0  )
				{
				r_stat = d_stat;
				goto fexit;
				}
			else
				{
				pl->asso_blk_use = ablock;
				um_vctovc(pos,pl->asso_blk[ablock-1].location);
				pl->asso_blk[ablock-1].key = 0;
				r_stat = d_stat;
				goto fexit;
				}
		}
fexit:;
	uu_dexit;
	return(r_stat);
	}
		
/*********************************************************************
**    E_FUNCTION     : int  ua_balloon_asso_blk (e, iblk, cporig, cpzaxis)
**       Update BALLOON e's association block i.
**    PARAMETERS
**       INPUT  :
**          e							balloon entity
**          iblk						association block number
**          cporig					construction plane origin
**          cpzaxis					construction plane z-axis
**       OUTPUT :
**          e							updated entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_balloon_asso_blk (e, iblk, cporig, cpzaxis)
	struct UA_generic_draft *e;
	int							iblk;
   UM_coord  					cporig,cpzaxis;

	{
	UU_REAL						uu;
   UM_coord  					npt,origin;
   int  							i,count,iassoc,status,d_stat;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon_asso_blk()"));

	count = iblk-1;
   um_nptpln(e->asso_blk[count].location, cporig, cpzaxis, npt);
   um_vctovc(npt,e->asso_blk[count].location);
   e->asso_blk[count].modifier = UA_TEXT_HORIZ;

	if(e->asso_blk[count].key != 0)
		{
		 
		/* get curve parameter closest to picked point */
		ua_para_on_curve(e->asso_blk[count].key, e->asso_blk[count].location, &uu);
		if(uu < 0.0)
			uu = 0.0;
		else if(uu > 1.0)
			uu = 1.0;

		/* store in assocation block record */
		count++;
		e->asso_blk[count].location[0] = uu;
		e->asso_blk[count].asso_type = -99;
		e->asso_blk[count-1].asso_type = count;
		e->asso_blk_use = count + 1;
		}

   uu_dexit;
   return(1);
	}

/*********************************************************************
**    E_FUNCTION     : int  ua_balloon_find_no_loc (e, cporig, cpzaxis)
**       Get find no. location from the user and update BALLOON origin field
**    PARAMETERS
**       INPUT  :
**          e							balloon entity
**          cporig					construction plane origin
**          cpzaxis					construction plane z-axis
**       OUTPUT :
**          e							updated entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_balloon_find_no_loc(e, cporig, cpzaxis)
	struct UA_generic_draft *e;
   UM_coord  					cporig,cpzaxis;

	{
   UM_coord  					npt,origin;
   int  							count, status;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon_find_no_loc()"));

   status = ud_world_coord(UA_DRAFTING,151,origin,1,&count,UU_FALSE);

	/* check error status */
   if (count != 0)
		{

		/* good input - store in BALLOON record */
      um_nptpln(origin,cporig,cpzaxis,npt);
      um_vctovc(npt,e->dim_origin);
		}

   uu_dexit;
   return(count);
	}
		
/*********************************************************************
**    E_FUNCTION     : int  ua_balloon_store (e)
**       Store and display the current BALLOON entity
**    PARAMETERS
**       INPUT  :
**          e							balloon entity
**       OUTPUT :
**          e							updated entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_balloon_store (e)
	struct UA_generic_draft *e;

	{
	UU_KEY_ID  keyid;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon_store()"));

   ua_balloon_create(e);
   ua_create_entity(e,&keyid);
   uc_display(e);

   uu_dexit;
   return(1);
	}

/*********************************************************************
**    E_FUNCTION     : int  ua_balloon_ALT_ACT (e, cporig, cpzaxis, first)
**       Handle ALT ACT reposition function.
**    PARAMETERS
**       INPUT  :
**          e							balloon entity
**          cporig					construction plane origin
**          cpzaxis					construction plane z-axis
**       OUTPUT :
**          e							updated entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_balloon_ALT_ACT (e, cporig, cpzaxis, first)
	struct UA_generic_draft *e;
   UM_coord  					cporig,cpzaxis;
	UU_LOGICAL              *first;

	{
   UM_coord  					loc,npt,origin;
   int  							count,status;
	UU_KEY_ID					keyid;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon_ALT_ACT()"));

    status = uc_retrieve_data(e,sizeof(struct UA_generic_draft));
    if (status != 0) *first = UU_TRUE;
	 else
		{

     	/* Get new origin for balloon entity. */
      status = ud_world_coord(UA_DRAFTING,151,origin,1,&count, UU_FALSE);

		/* now update UNIBASE record and re-display */
      if (count != 0) 
			{
         um_nptpln(origin,cporig,cpzaxis,npt);
         um_vctovc(npt,e->dim_origin);
         ua_balloon_create(e);  
         status = ua_update_entity(e->key,e);
         if (status != 0)
        		 ua_create_entity(e,&keyid);
          uc_display(e);
			*first = UU_FALSE;
			}
		}
   uu_dexit;
   return(1);
	}

/*********************************************************************
**    E_FUNCTION     : int  ua_balloon_init (e, type)
**       Initilize a BALLOON record
**    PARAMETERS
**       INPUT  :
**          e							balloon entity
**          type						tpye of balloon record to be initialized
**       OUTPUT :
**          e							updated entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_balloon_init (e, type)
	struct UA_generic_draft *e;
	int							type;
	{

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon_init()"));

   ua_init_entity(UA_BALLOON_DIM, 0, e);       /* Initialize structure. */

	/* set text subtype according to type*/
   e->txt_entry = UA_USERDEF;
	e->entity_site = UA_BOTTOM_LEFT;
	e->txt_just = UA_LEFT;
	e->subtype = type;
	switch(type )
		{
		case 1:
  		 	e->txt_blk_use = 1;
			break;
		case 2:
		   e->txt_blk_use = 2;
			e->txt_blk[1].subtype = main_txt2;
			break;
		case 3:
		   e->txt_blk_use = 2;
			switch(UA_balloon_symbol_loc)
				{
				case 1:
					e->txt_blk[1].subtype = tol_txt1;
					break;
				case 2:
					e->txt_blk[1].subtype = tol_txt2;
					break;
				}
			break;
		case 4:
			e->txt_blk_use = 1;
			e->dim2_value = -99;
			break;
		}
   uu_dexit;
   return(1);
	}

/*********************************************************************
**    E_FUNCTION     : int  ua_balloon_update_ID_value (e)
**       Store ID value and multiplier in dim_value and dim2_vlaue
**    PARAMETERS
**       INPUT  :
**          e							balloon entity
**       OUTPUT :
**          e							updated entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int  ua_balloon_update_ID_value (e)
	struct UA_generic_draft *e;
	{

	char        buff[100];
	UU_TRUEDOUBLE ff, uai_atof();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
   uu_denter(UU_STRC,(us,"ua_balloon_update_ID_value()"));

	strcpy(buff, e->txt_blk[0].tstring);
	buff[e->txt_blk[0].char_cnt] = '\0';
	ff = uai_atof(buff);
	e->dim_value = ff;

	/* test for multiplier */
	if(e->txt_blk_use == 2)
		{
		strcpy(buff, e->txt_blk[1].tstring);
		buff[e->txt_blk[1].char_cnt] = '\0';
		ff = uai_atof(buff);
		e->dim2_value = ff + 0.1;
		}
	else if(e->dim2_value == 0.0)
		{
		e->dim2_value = 1.1;
		}

   uu_dexit;
   return(1);
	}
