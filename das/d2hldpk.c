/*********************************************************************
**
**    NAME         :  d2hldpk.c
**
**       CONTAINS:
**  			ud_select
**  			ud_pick
**  			ud_pickdum
**  			ud_ploc
**  			ud_ploc1
**  			ud_pick1
**  			ud_init_select_buffer
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d2hldpk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:05
**
*********************************************************************/

#include "usysdef.h"
#include "nclfc.h"
#include "usysg.h"
#include "uhep.h"
#include "dmark.h"
#include "dinput.h"
#include "dasnog.h"
#include "diconm.h"
#include "dselect.h"
#include "udebug.h"
#include "xenv1.h"
#include "gmat4.h"
#include "ginqxf.h"
#include "gtblvar6.h"
#include "mdebug.h"
#include "nclicons.h"
#include "view.h"

/*
.....Added by Paul to implement the "text" and "location" input. 07/10/92
*/
#include <ctype.h>
#include "class.h" 
#include "uims.h"
#include "mdcoord.h"
#include "mfort.h"  
#include "mdpick.h"
#include "driver.h"

#define SELREC (*event).indata.pickdata

/*
.....For verify mode, reset to zero first time through picking routines
.....In case of previous reject op. - RAZ
*/
extern int NCL_nopick_cnt;
int UD_select_key = 0;
#define MAXVERCNT 256
extern UU_KEY_ID NCL_verify_list[256];

UD_DASTAT ud_cart1();
#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3
extern int NCL_mark_method;

/********************************************************************* 
**
**  I_FUNCTION			:  ud_select(prompt, pickbuf)
**     input multiple pick ids (pick some entities) high level DAS
**
**  PARAMETERS   
**      INPUT:  prompt = operator prompt string
**      output: pickbuf = address of the global pick buffer
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_select(prompt, pickbuf)
char *prompt; 					/*   operator prompt string  */
int *pickbuf;					/*   pick buffer address to return  */
{

	UD_DASTAT status;						/* status return cell */
	UD_DASTAT ud_single();
	int jmpflag;							/* mark return flag */
	void ud_init_select_buffer();

	uu_denter(UU_DTRC,(us,"entering ud_select(%s,%x)",prompt,pickbuf));

/* -- malloc space for select buffer if this is the first use of it */

	if(UD_Selinit)
		ud_init_select_buffer();

	if(UD_BUFTEST() == UU_FALSE)
	{

restart:
		UD_Select_ptr = 0;
		UD_Select_cnt = 0;
		UD_BUFBUSY();

/*	-- mark the current location so that we can unselect all if the
			operator enters a command reject -- */

		UD_MARK(jmpflag, UU_FALSE);

		if(jmpflag == 0)
		{
/*
			if(UU_application == UU_DDC)
				UD_ICMU_UP(9);
*/
			if ((UU_application == UU_NCLCAM) || (UU_application == UU_NCLCADD))
				{
				UD_ICMU_UP(SELECT_ICONS);
				SELECT_UP = SELECT_ICONS;
				}

			status = ud_single(prompt, UU_FALSE);

			if(status == DE_ALTACT1)

/*		-- Alternate action - invoke select subsystem -- */

				{
/*				iselect();*/
				status = DE_TRUE;
				}
			/* added for altcation. kathy */
			if (status == DE_ALTACT)
				{
				if (((UU_application == UU_NCLCAM) || (UU_application == UU_NCLCADD)) &&
					(SELECT_UP == SELECT_ICONS))
						{
						UD_ICMU_DN(SELECT_UP);
						SELECT_UP = 0;
						}
				UD_UNMARK(jmpflag);
				UD_BUFFREE();
				uu_dexit;
				return (status);
				}

/*	-- set all pickable again and pop mark from stack -- */

			ud_spstat(UU_TRUE, 0);
			ud_restore_pickarea();
			ud_delete_assist_segs();
/*
			if(UU_application == UU_DDC)
				UD_ICMU_DN(9);
*/
			if (((UU_application == UU_NCLCAM) || (UU_application == UU_NCLCADD)) &&
				(SELECT_UP == SELECT_ICONS))
					{
					UD_ICMU_DN(SELECT_UP);
					SELECT_UP = 0;
					}

			UD_UNMARK(jmpflag);

			if(status != DE_TRUE && status != DE_DONE)
			{

/*--		"invalid control request - reenter"		---*/

				uu_uerror0(UD_DASHEP, 12);
					goto restart;
			}
   	}
		else
		{

/*		-- if jump mark then reset allow multiple pick flag -- */

			UD_Sellim_mp = UU_TRUE;

/*		-- set everything pickable and unselected, flush select list,
				and set buffer free -- */

/*
			if(UU_application == UU_DDC)
				UD_ICMU_DN(9);
*/
			if (((UU_application == UU_NCLCAM) || (UU_application == UU_NCLCADD)) &&
				(SELECT_UP == SELECT_ICONS))
				{
				UD_ICMU_DN(SELECT_UP);
				SELECT_UP = 0;
				}
			ud_reject(0, 0);
			UD_BUFFREE();
			UD_UNMARK(jmpflag);

/*---	"ud_select returned from ud_jmpmark"		---*/

			uu_uerror0(UD_DASHEP, 31);
		}
	}
	else
	{

/*---	"select already in progress"		---*/

		uu_uerror0(UD_DASHEP, 77);

/*		-- get out of this semantic action -- */

		ud_jump(-1, UU_FALSE);
	}

	UD_BUFFREE();
	uu_dexit;
	return (status);
}

/********************************************************************* 
**
**  I_FUNCTION				:  ud_pick(prompt, ret_pck)
**     input a pick id (pick some entities) high level DAS
**
**  PARAMETERS   
**      INPUT:  prompt = operator prompt string
**      OUTPUT: ret_pck = record describing thing picked
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_pick(prompt, ret_pck)
char *prompt; 							/*   operator prompt string  */
UD_PPICKREC *ret_pck ;				/*   pick id to return  */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_pick1();			/* semantic processor */
	UD_DEVENT event;					/* input event buffer */
	char pbuf[100];					/* prompt buffer */
	UD_DASTAT ncl_verify_pick();

	uu_denter(UU_DTRC,(us,"entering ud_pick, add=%x, prompt=%s", 
							prompt, prompt));

/*	-- build the prompt message -- */

	sprintf(pbuf, "%s %s", prompt, UD_synsel);

	status = DE_AGAIN;
/*
.....Reset verify list if have any
*/
/*
.....Initialize verify counter
*/
	ud_reset_verify_list();
	while(status == DE_AGAIN)
	{
		ud_gevt(&event, UD_pckint, pbuf, 1, UD_pckdev, UD_pckech, NULL);
		status = ud_pick1(&event, ret_pck);
/*
.....restore picking area and ready to pick again or accept
*/
		ud_restore_pickarea();
/*
........VERIFY MODE:
*/
/*
.....this changed to use other key function to deal with it
*/
		if (NCL_mark_method==DYNAMIC_MARK)
		{
			if (UD_select_key)
			{
				ncl_verify_pick2(ret_pck);
				ud_post_msg(2);
				UD_select_key = 0;
				status = DE_AGAIN;
			}
		}
		else
		{
			if (status == DE_TRUE && (!UD_pckstr || event.evclass != UD_STRING))
				status = ncl_verify_pick(ret_pck);
		}
	}
	uu_dexit;

	UD_pckstr = UU_FALSE;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION		:  ud_pickdum(prompt, ret_pck)
**     input a pick id (pick some entities)
**
**  PARAMETERS   
**      INPUT:  prompt = operator prompt string
**      OUTPUT: ret_pck = record describing thing picked
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_pickdum(prompt, ret_pck)
char *prompt; 						/*   operator prompt string  */
UD_PPICKREC *ret_pck ;				/*   pick id to return  */
{
	UD_PLOCREC ploc;					/* save pick locate */
	int i;
	UD_DASTAT status;
	UD_DASTAT ud_ploc();

	status = ud_ploc(prompt, &ploc);
	if(status == DE_TRUE)
	{
		(*ret_pck).depth = ploc.ppath.depth;
		for(i=0; i<ploc.ppath.depth; i++)
			(*ret_pck).pickpath[i] = ploc.ppath.pickpath[i];
	}
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION			:  ud_ploc(prompt, ret_pck)
**      pick an entity and return the coordinate used to pick it 
**
**  PARAMETERS   
**      INPUT:  prompt = operator prompt string
**      OUTPUT: ret_pck = pick id picked, location, and choice
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_ploc(prompt, ret_pck)
char *prompt ; 					/*   operator prompt string  */
UD_PLOCREC *ret_pck ;				/*   pick id and location to return  */
{
	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_ploc1();			/* semantic processor */
	UD_DEVENT event;					/* input event buffer */
	char pbuf[100];					/* prompt message buffer */
	UD_DASTAT ncl_verify_pick();


	uu_denter(UU_DTRC,(us,"entering ud_ploc"));

/*	-- build the prompt message -- */

	sprintf(pbuf, "%s %s", prompt, UD_synplc);

	status = DE_AGAIN;
/*
.....Initialize verify counter
*/
/*
.....Reset verify list if have any
*/
	ud_reset_verify_list();
	while(status == DE_AGAIN)
	{
		ud_gevt(&event, UD_pckint, pbuf, 1, UD_pckdev, UD_pckech, NULL);
		status = ud_ploc1(&event, ret_pck);
/*
.....restore picking area and ready to pick again or accept
*/
		ud_restore_pickarea();
/*
........VERIFY MODE:
*/
/*
.....this changed to use other key function to deal with it
*/
		if (NCL_mark_method==DYNAMIC_MARK)
		{
			if (UD_select_key)
			{
				ncl_verify_pick2(ret_pck);
				ud_post_msg(2);
				UD_select_key = 0;
				status = DE_AGAIN;
			}
		}
		else
		{
			if (status == DE_TRUE && (!UD_pckstr || event.evclass != UD_STRING))
				status = ncl_verify_pick(ret_pck);
		}
	}

	uu_dexit;
	UD_pckstr = UU_FALSE;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION				:  ud_pick1(event, ret_pck)
**     input a pick id (pick some entities) high level DAS
**
**  PARAMETERS   
**      INPUT:  event = event structure
**      OUTPUT: ret_pck = record describing thing picked
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_pick1(event, ret_pck)
UD_DEVENT *event;						/* event structure */
UD_PPICKREC *ret_pck ;				/* pick id to return */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_auxm();
	UU_LOGICAL ud_limsel();
	int i,j;
/*
.....
..... Added by Paul for the new "text" and "location" input code. 08/05/92
.....
*/
	UU_KEY_ID key;
	int strln, xform, n;
	int assoc_buf;
	UV_vport vport;
	UV_view view;

    UM_f77_str f77_str;

	uu_denter(UU_DTRC,(us,"entering ud_pick1"));

	if((*event).evclass == UD_PICK)
	{

/*
.....see if the picking is limited by picking type
*/
		xform = (*event).indata.pickdata.transform;
		uv_getvpid(UV_act_screen[0].vports[xform-1],&vport);
		if (!ud_verifypick_type(&vport,UU_TRUE))
		{
			status = DE_AGAIN;
			return(status);
		}
		if (!ud_motion_pick_type())
		{
			n = SELREC.pickpath[SELREC.depth-2];
			if (((ud_getpick_type()!=UD_PICK_ASSIST) && (ud_isassist_seg(n)))
				|| ((ud_getpick_type()==UD_PICK_ASSIST) && (ud_isassist_seg(n)==0)))
			{
/*			-- restricted entity picked -- */
				uu_uerror0(UD_DASHEP, 76);
				status = DE_AGAIN;
				return(status);
			}
		}
/*		-- see if entity type is limited -- */

		if (!ud_motion_pick_type() &&
			ud_limsel(SELREC.pickpath[SELREC.depth-2], UU_FALSE, UU_NULL) == 
				UU_FALSE)

/*		-- invalid entity picked -- */

		{

/*			-- restricted entity picked -- */

			uu_uerror0(UD_DASHEP, 76);
			status = DE_AGAIN;
		}
		else

/*		-- valid entity picked -- */

		{

/*				-- move in pickpath and depth -- */

			(*ret_pck).depth = SELREC.depth;
			for(i=0; i<SELREC.depth && i<UD_MAXPICKDEPTH; i++)
				(*ret_pck).pickpath[i] = SELREC.pickpath[i];
/*
.....(*ret_pck).depth can not be bigger than UD_MAXPICKDEPTH+1
.....UD_MAXPICKDEPTH define as 5, why? the depth must be a even
.....number (1 for seg#, 1 for ID#)
.....Yurong 5/10/99
*/
			if ((*ret_pck).depth>UD_MAXPICKDEPTH+1)
				(*ret_pck).depth = UD_MAXPICKDEPTH+1;

/*			-- set up default interactions -- */

			UD_pckint = UD_PICK;

			status = DE_TRUE;
		}
	}
/*
.....
.....Added by Paul to get  the string input. 07/27/92
.....
*/
	else if((*event).evclass == UD_STRING)
	{
        strln = strlen((*event).indata.stringdata);
        for (j=0; j<strln; j++)
            (*event).indata.stringdata[j] =
            islower((*event).indata.stringdata[j]) ? toupper((*event).indata.stringdata[j])
            : (*event).indata.stringdata[j];

          UM_init_f77_str(f77_str,(*event).indata.stringdata,64);
			for (i=strln;i<64;i++) f77_str[i] = ' ';
          getkey(UM_addr_of_f77_str(f77_str), &key);

			if (key != 0) ur_retrieve_disp_segid(key, &assoc_buf);

        if(key == 0)
        {
            uu_uerror0(UD_DASHEP, 22);
            status = DE_AGAIN;
        }
        else if(ud_limsel(assoc_buf, UU_FALSE, UU_NULL) ==
                        UU_FALSE)	/** (ud_newfil(key) == UU_FALSE) **/
        {
            uu_uerror0(UD_DASHEP, 76);
            status = DE_AGAIN;
        }
        else
        {   
            UD_pckint = UD_PICK;
            status = DE_TRUE;
        	(*ret_pck).depth = 2;
        	(*ret_pck).pickpath[0] = assoc_buf;
        	(*ret_pck).pickpath[1] = key;
		}
	}
/*
.....
.....  The end of the additional code for the "string" input. Paul. 08/06/92
.....
*/

	else if((*event).evclass == UD_CHOICE && (*event).evdev == UD_AUXMENU)
		status = ud_auxm(event);
	else if ((*event).evclass == UD_PICKMENU)
	{
/*
......set the status to DE_ALTACT instead of DE_TRUE because this is not a normal
......input for pick location, set the return pick to empty
*/
		(*ret_pck).depth = 0;
		(*ret_pck).pickpath[0] = 0;
		(*ret_pck).pickpath[1] = 0;
		status = DE_ALTACT;
	}
	else 
	{

/*---		" invalid event"		---*/

/*		uu_uerror0(UD_DASHEP, 23);*/
		status = DE_AGAIN;
	}

	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION			:  ud_ploc1(event, ret_pck)
**      pick an entity and return the coordinate used to pick it 
**
**  PARAMETERS   
**      INPUT:  event = event structure
**      OUTPUT: ret_pck = pick id picked, location, and choice
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_ploc1(event, ret_pck)
UD_DEVENT *event;						/* event structure */
UD_PLOCREC *ret_pck ;				/*   pick id and location to return  */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_auxm();
	int i,j,strln;
	UU_REAL x, y, z, cord[3];		/* cells for coordinate system conversions */
	UU_LOGICAL ud_limsel();
    
	UU_KEY_ID key;
	char str_xy[81],str_tmp[81];
	UU_LOGICAL defflag;             /* default exists flag */
	UD_NDCLOCREC coord;                 /* default coordinate */
	UU_LOGICAL xflag;               /* NDC flag (UU_TRUE=W.C, UU_FALSE=NDC) */
 
	struct UC_entitydatabag e;
	int assoc_buf;

    int dsegid;
    uv_segbuff(buffer);

UM_f77_str f77_str;

	uu_denter(UU_DTRC,(us,"entering ud_ploc1"));

/*
.....
.....Use this case if we go to the "input by location" from "pick input"
.....UD_PICK --> UD_LOCATOR
.....Added by Paul. 07/22/92
.....
*/
	if((*event).evclass == UD_LOCATOR)
	{
/*
.....
.....Filter selected entity
.....
*/
		if(UD_LIMIT.lsel && !uu_tst_bit(UD_LIMIT.lselbf,0))
    	{
			uu_uerror0(UD_DASHEP, 70);
			status = DE_AGAIN;
		}
 		else
		{
/*
.....
..... Create the "Nested point" definition.
.....
*/
        	defflag = UU_FALSE;
        	xflag   = UU_TRUE; 
	        status = ud_cart1(event, &coord, defflag, &coord, xflag);
			ncl_cctostr(2,coord.cord,str_tmp);
            sprintf(str_xy,"(PT/%s)",str_tmp);

			UD_pckint = UD_PICK;
			strcpy((*ret_pck).pndc.label,str_xy);
/*
.....
.....This is just dummy operators to fill it up with some numbers. I hope we do not
.....use it anywere else. 
.....
*/
			(*ret_pck).ppath.depth = 1;
			(*ret_pck).ppath.pickpath[0] = 0;
			(*ret_pck).ppath.pickpath[1] = 0;
			(*ret_pck).pndc.cord[0] = 1;
			(*ret_pck).pndc.cord[1] = 1;
			(*ret_pck).pndc.cord[2] = 1;
			(*ret_pck).pndc.choice = 1;
			(*ret_pck).pndc.transform = 1;
			ug_mcopy((*ret_pck).pndc.wndc3_mat, gqnormmat((*ret_pck).pndc.transform));
			ug_invrt((*ret_pck).pndc.ndcw3_mat, (*ret_pck).pndc.wndc3_mat);
		}
	}
/*
.....
.....Use this case if we go to the "text input" from "pick input"
.....UD_PICK --> UD_STRING
.....Added by Paul. 07/08/92
.....
*/
    else if((*event).evclass == UD_STRING)
    {
		strln = strlen((*event).indata.stringdata);
/*
.....
..... If it is "Nested point" do not check anything, only copy the
..... string to out buffer.
.....
*/
		if((*event).indata.stringdata[0] == '(' && 
		   (*event).indata.stringdata[strln-1] == ')' && strln <= 80)
		{
		    UD_pckint = UD_PICK;
            strcpy((*ret_pck).pndc.label,(*event).indata.stringdata);
/*
.....
.....This is just dummy operators to fill it up with some numbers. I hope we do not
.....use it anywere else.
.....
*/
            (*ret_pck).ppath.depth = 1;
            (*ret_pck).ppath.pickpath[0] = 1;
            (*ret_pck).ppath.pickpath[1] = 1;
            (*ret_pck).pndc.cord[0] = 1;
            (*ret_pck).pndc.cord[1] = 1;
            (*ret_pck).pndc.cord[2] = 1;
            (*ret_pck).pndc.choice = 1;
            (*ret_pck).pndc.transform = 1;
            ug_mcopy((*ret_pck).pndc.wndc3_mat, gqnormmat((*ret_pck).pndc.transform));
            ug_invrt((*ret_pck).pndc.ndcw3_mat, (*ret_pck).pndc.wndc3_mat);
 
            status = DE_TRUE;
		}
		else
/*
.....
..... If it is not a "nested point"...
.....
*/
		{
	        strln = strlen((*event).indata.stringdata);
			for (j=0; j<strln; j++)
   				(*event).indata.stringdata[j] = 
				islower((*event).indata.stringdata[j]) ? toupper((*event).indata.stringdata[j]) 
				: (*event).indata.stringdata[j];


          UM_init_f77_str(f77_str,(*event).indata.stringdata,64);
			for (i=strln;i<64;i++) f77_str[i] = ' ';
          getkey(UM_addr_of_f77_str(f77_str), &key);
			(*event).indata.stringdata[strln] = '\0';

			if (key != 0) ur_retrieve_disp_segid(key, &assoc_buf);

    	if(key == 0 && !UD_pckstr)
    	{
			uu_uerror0(UD_DASHEP, 22);
			status = DE_AGAIN;
		}
        else if(key != 0 && ud_limsel(assoc_buf, UU_FALSE, UU_NULL) ==
                        UU_FALSE) 
        {
            uu_uerror0(UD_DASHEP, 76);
            status = DE_AGAIN;
        }
		else
		{
			UD_pckint = UD_PICK;
			strcpy((*ret_pck).pndc.label,(*event).indata.stringdata);
     		(*ret_pck).ppath.depth = 2;
     		(*ret_pck).ppath.pickpath[0] = assoc_buf;
     		(*ret_pck).ppath.pickpath[1] = key;
            (*ret_pck).pndc.cord[0] = 1;
            (*ret_pck).pndc.cord[1] = 1;
            (*ret_pck).pndc.cord[2] = 1;
            (*ret_pck).pndc.choice = 1;
            (*ret_pck).pndc.transform = 1;
            ug_mcopy((*ret_pck).pndc.wndc3_mat, gqnormmat((*ret_pck).pndc.transform));
            ug_invrt((*ret_pck).pndc.ndcw3_mat, (*ret_pck).pndc.wndc3_mat);
 
            status = DE_TRUE;
		}
		}
	}

/*
.....
..... The end of additional code for "text" and "location" input.
.....
*/


	else if((*event).evclass == UD_PICK)
	{
 
		if (!ud_motion_pick_type() &&
			ud_limsel(SELREC.pickpath[SELREC.depth-2], UU_FALSE, UU_NULL) ==
				UU_FALSE)
		{

/*			-- restricted entity picked -- */

			uu_uerror0(UD_DASHEP, 76);
			status = DE_AGAIN;
		}
		else
		{
			status = DE_TRUE;

/*				-- move in pickpath and depth -- */

			(*ret_pck).ppath.depth = SELREC.depth;
			for(i=0; i<SELREC.depth && i<UD_MAXPICKDEPTH; i++)
				(*ret_pck).ppath.pickpath[i] = SELREC.pickpath[i];

			(*ret_pck).pndc.cord[0] = SELREC.position[0];
			(*ret_pck).pndc.cord[1] = SELREC.position[1];
			givref3(SELREC.transform, cord);
			gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
			(*ret_pck).pndc.cord[2] = z;

			(*ret_pck).pndc.choice = SELREC.choice;
			(*ret_pck).pndc.transform = SELREC.transform;
			ug_mcopy((*ret_pck).pndc.wndc3_mat, gqnormmat((*ret_pck).pndc.transform));
			ug_invrt((*ret_pck).pndc.ndcw3_mat, (*ret_pck).pndc.wndc3_mat);
/*
.....
.....Added by Paul for copying LABEL to the UD_PLOCREC strucure. 07/15/92 
.....
*/
			dsegid = ret_pck->ppath.pickpath[0];
			if (ud_isassist_seg(dsegid)==0 && !ud_motion_pick_type())
			{
            gsegrud(dsegid,buffer);
            e.key = uv_getkey(buffer);
    			ur_retrieve_data_fixed(&e);
    			ncl_get_label(&e, (*ret_pck).pndc.label);
			}
/*******/
 
			uu_dprint(UU_DTRC,(us,"in ud_ploc1: transform=%d",
				(*ret_pck).pndc.transform));


/*			-- set up default interactions -- */

			UD_pckint = UD_PICK;
		}
	}
	else if((*event).evclass == UD_CHOICE && (*event).evdev == UD_AUXMENU)
	{
		status = ud_auxm(event);
	}
	else if ((*event).evclass == UD_PICKMENU)
	{
/*
......set the status to DE_ALTACT instead of DE_TRUE because this is not a normal
......input for pick location, set the return pick to empty
*/
		(*ret_pck).ppath.depth = 0;
		(*ret_pck).ppath.pickpath[0] = 0;
		(*ret_pck).ppath.pickpath[1] = 0;
		status = DE_ALTACT;
	}
	else 
	{

/* -- invalid input to pick and location -- */

/*		uu_uerror0(UD_DASHEP, 25);*/
		status = DE_AGAIN;
	}

	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  E_FUNCTION			:  ud_init_select_buffer
**      initiialize the global select buffer
**
**  PARAMETERS   
**      INPUT:  
**				none
**      OUTPUT: 
**				none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

void ud_init_select_buffer()
{
	char *ux_getenv();
	char *selbuf_str;						/* size of select buffer as string */
	int num_match = 0;					/* # of input items matched by sscanf */

	uu_denter(UU_DTRC,(us,"entering ud_init_select_buffer()"));

/* -- malloc space for select buffer if this is the first use of it */

	if(UD_Selinit)
	{
		if((selbuf_str = ux_getenv("UD_SELBUF_SIZ", UX_PRTERRS)) == NULL)
			UD_Selbuf_size = 10000;
		else
		{
			num_match = sscanf(selbuf_str,"%d",&UD_Selbuf_size);
			if(num_match == 0)
				UD_Selbuf_size = 10000;
		}

		uu_dprint(UU_DTRC,(us,"ud_select: num_match %d selbuf_size %d",
			num_match, UD_Selbuf_size));

		UD_Select_buf = (int*)uu_toolmalloc(UD_Selbuf_size * sizeof(int));
		UD_Selinit = UU_FALSE;
	}
	uu_dexit;
}

/********************************************************************* 
**
**  E_FUNCTION			:  ud_select_nextpk()
**      Select the next picking
**
**  PARAMETERS   
**      INPUT:  
**				none
**      OUTPUT: 
**				none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/
void ud_select_nextpk()
{
	if (NCL_mark_method==DYNAMIC_MARK)
	{
/*
.....If the cursor is not on an entity
.....when the KEY_FUNCTION key is pressed
.....then 'UD_select_key' is not reset
.....Don't see a reason for checking anyway
.....Bobby  -  12/4/09
*/
/*		if (UD_select_key==0) */
		{
			UD_select_key = 1;
			ud_post_msg(1);
		}
	}
}


