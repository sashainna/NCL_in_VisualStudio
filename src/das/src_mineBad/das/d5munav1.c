/*********************************************************************
**    NAME         :  d5munav1.c
**       CONTAINS:
**			ud_munavg2
**    	ud_icmustk_up
**    	ud_icmustk_dn
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       d5munav1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:12
*********************************************************************/

#include		"ustdio.h"
#include		"usysdef.h"
#include		"udebug.h"
#include		"gtbl.h"
#include		"gi1.h"
#include		"go3.h"
#include		"dinput.h"
#include		"dasg.h"
#include		"dasnog.h"
#include		"dmark.h"
#include		"dmenucom.h"
#include		"uhep.h"
#include		"diconm.h"
#include		"usysg.h"

#define EVIN event.indata
#define MUEX UDI_Muex_stk[UDI_Muex_ptr]

/*********************************************************************
**
**    E_FUNCTION     :  ud_munavg2 (mujtb,mustrtb,mustbsize,muftb,
**												mucases, mujsize,base,mubit, muflag)
**       menu navigator for icon menus where all are simultaneously active
**
**    PARAMETERS   
**       INPUT  : 
**          mujtb - menu's jump table
**          mustrtb - menu's string table
**          mustbsize - number of strings in the menu
**          muftb - array of menu name
**          mucases - action routines for the leaves
**          mujsize - number of menu
**          base - base number for the current menu
**          mubit - menu's reload flag
**       OUTPUT :  
**          base - base nember for the current menu
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_munavg2(mujtb, mustrtb, mustbsize, muftb, mucases, 
		  mujsize, base, mubit, muflag)
UDI_MUJTB mujtb[];		/* that holds state info for each node table */
UDI_MUSTR mustrtb[];		/* goto and call string table  */
int	mustbsize;			/* menu string table size */
char	*muftb[];			/* menu character string text array */
int	(*mucases)();		/* menu semantic action routines for leaves */
int	mujsize,				/* maximum menu number */
		*base,				/* base menu number */
		mubit,				/* current menu bit representation */
		muflag;				/* flag to decide which menu level the second reject 
									should go */

{
	UD_MENU_STACK_REC Menu_stk[UDI_MENU_DEPTH];	/* menu stack */
	int Msub_stk[UDI_SUB_DEPTH] ;		/* menu subroutine stack */
	int Mstk_ptr;				         /* Menu stack initial pointer */
	int Msub_ptr;							/* Menu subroutine stack initial pointer*/
	int i, i1, markval, save_mark_stack;
	int butnum;								/* current operator input value */
	int devnum;								/* current choice device number */
	int pre;									/* current menu number  */
	UD_DEVENT event;						/* DAS event buffer */
	UU_LOGICAL up_flag;					/*  menu up occurred flag */
	char buf[4];
	UU_LOGICAL locflag;					/* command reject, propogation flag */
	UU_LOGICAL semactflag;				/* semantic action executed flag */
	static char *nullstr = "";			/* null string */

	uu_denter(UU_GITRC,(us,"in ud_munavg1"));

/*	-- init -- */

	Mstk_ptr = -1;
	Msub_ptr = -1;
	up_flag = UU_FALSE;
	semactflag = UU_FALSE;
 
/*	-- propogate limit state if alternate action -- */

	if (muflag == UU_TRUE)  
  		locflag = UU_FALSE;
	else
  		locflag = UU_TRUE;

	ud_lpsh(locflag);

/*	-- increment number of times menu navigator has been called -- */

	UDI_muwhich++;

/*	-- flag that we are not ready to exit menu tree -- */

	UDI_Muex_ptr++;
	MUEX.loopflag = UU_FALSE;
	MUEX.nav_mode = 2;
	MUEX.menu_stk = Menu_stk;
	MUEX.menu_stk_ptr = &Mstk_ptr;
	MUEX.base = base;

/*	-- set up subsystem traceback name stack with root menu title -- */

	UD_sstrc[UD_sstrc_ptr] = muftb[mujtb[0].menuind];
	UD_sstrc_ptr++;
	if(UD_sstrc_ptr > UDI_MU_WARN+4)
	{
		uu_uerror0(UD_DASHEP, 68);
		UD_dastkfl = UU_FALSE;
		uu_dexit;
		ud_jump(UD_markptr, UU_TRUE);
	}
	else if(UD_sstrc_ptr > UDI_MU_WARN)
		uu_uerror0(UD_DASHEP, 87);

/*	-- push menu zero onto menu stack -- */

	pre = 0;
	udi_mupush (Menu_stk, &Mstk_ptr, 0);

/*		-- set environment stack pointer -- */

 	UD_MARK(i1, muflag);
 	uu_dprint(UU_DTRC,(us, "in ud_munavg, markval=%d, muflag=%d", i1, locflag));

/*	-- exit if jumping to root -- */

	if (i1 > 0)
		ud_muexit1();

/*	-- exit loop of command reject of alt act menu -- */

	if((i1<0) && (muflag == UU_FALSE))
		ud_muexit1();

/*	-- main loop for navigating menus -- */

	while (MUEX.loopflag == UU_FALSE)
	{

/*		-- process tutorial mode -- */

		if(semactflag == UU_FALSE)
		{
			UD_errorsys = 0;
			UD_promptsys = 0;
		}
		else
			semactflag = UU_FALSE;

		UD_tut_ptr = mujtb[pre].tutotb;
		if(UD_tut_mode == UD_TUT_ON)
			if(mujtb[pre].tutotb != NULL)
				if(up_flag == UU_FALSE)
					uj_tutorial(mujtb[pre].tutotb);

/*		-- record this menu -- */

		ud_rpwrcom(muftb[mujtb[pre].menuind]);

/*		-- get the next operator input -- */

		up_flag = UU_FALSE;
	  	devnum = *base+pre;
		UD_ICMU_UP(devnum);
     	ud_atgevt(&event, UD_CHOICE, &nullstr, 0, 1, 3, NULL);

/*		-- for the class of choice inputs that are not from the puck do -- */

		if ((event.evclass==UD_CHOICE) && (event.evdev!=3))
		{
/*			-- if operator input 'done' then simulate a menu up input -- */

			if ((event.evdev == UD_AUXMENU))
			{
				if(EVIN.choicedata == UD_CDONE)
      		{
        			event.evdev = 1;
        			EVIN.choicedata = '0';
      		}

/*				-- exit the sub system if indicated -- */

				if(EVIN.choicedata == UD_CEXITSUB)
      		{
					if(UDI_muwhich>1)
						ud_muexit1();
		 			pre = Menu_stk[Mstk_ptr].menuno;
					goto exitmain;
      		}
     		}

/*			-- if the operator struck a numeric key (choice pet = 3), 
				'command reject', or 'done' -- */

			if (event.evdev != UD_AUXMENU)
			{
/*				-- if char is numeric, convert to integer -- */

	 			butnum = ((EVIN.choicedata>='0')&&(EVIN.choicedata<='9'))?
		 			 EVIN.choicedata - '0' : EVIN.choicedata;
		
/*				-- menu up -- */

		 		if (butnum == 0)
				{
					up_flag = UU_TRUE;

/*					-- if at menu root and not at first menu, exit this
						subsystem -- */

					if((udi_mupop(Msub_stk,&Mstk_ptr,&Msub_ptr)==UU_FALSE) &&
							(UDI_muwhich>1))
					{
						ud_muexit1();
					}
			 		else
					{
/*						-- bring down icon menu, unhilite higher level -- */

			 			pre = Menu_stk[Mstk_ptr].menuno;
						Menu_stk[Mstk_ptr].choiceno = -1;
						UD_ICMU_DN(devnum);
						gschoicehilite(UD_ksws, *base+pre, -1, UG_NORMAL);
					}
				}
				else
				{

/*					-- perform implicit menu ups until we are at the right menu-- */

					if(event.evdev > UD_AUXMENU)
						while (event.evdev!=devnum && Mstk_ptr>0)
						{
/*						-- bring down icon menu, unhilite higher level -- */

							UD_ICMU_DN(devnum);
							udi_mupop(Msub_stk, &Mstk_ptr, &Msub_ptr);
			 				pre = Menu_stk[Mstk_ptr].menuno;
							devnum = *base + pre;
							gschoicehilite(UD_ksws, devnum, -1, UG_NORMAL);
						}

/*					-- if valid menu choice, traverse down -- */

		 			if (butnum < mujtb[pre].menuno)	
					{
				 		if (mujtb[pre].infor[butnum-1] >= 0)
						{

/*							-- if not at a leaf traverse down after hiliting menu -- */

							gschoicehilite(UD_ksws, *base+pre, butnum, UG_HIGHLIGHTED);
							Menu_stk[Mstk_ptr].choiceno = butnum;
					 	 	pre = mujtb[pre].infor[butnum-1];
					 	 	udi_mupush(Menu_stk, &Mstk_ptr, pre);
						}
						else
						{

/*							-- at a leaf node, execute semantic action -- */

				  	 		UD_MARK(markval, UU_TRUE);

							uu_dprint(UU_DTRC,(us, "in ud_munavg 1, markval=%d",
										markval));

					  		if(markval == 0)
                 		{
								Menu_stk[Mstk_ptr].choiceno = butnum;
								gschoicehilite(UD_ksws, *base+pre, butnum,
													UG_HIGHLIGHTED);

/*								-- set application stack -- */

								uu_appush_store();

/*								-- set limit state -- */

								ud_lpsh(locflag);

/*								-- set up for recording autotest mode so that
									menu leaf label is recorded -- */

								if(UD_autotest==UU_TRUE && 
											UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
									ud_record_auto(muftb[mujtb[pre].menuind],
														muftb[mujtb[pre].menuind+butnum]);

/*								-- execute semantic action -- */

								save_mark_stack = UD_markptr;
					     		(*mucases)(-mujtb[pre].infor[butnum-1]);
								ur_cond_auto_save();
								ncl_cond_auto_save();
                 		}

/*							-- test mark stack state -- */

							if(save_mark_stack != UD_markptr)
							{
						uu_dprint(UU_DTRC,(us, "MARK imbalance, save=%d, current=%d", 
										save_mark_stack, UD_markptr));
								uu_uerror0(UD_DASHEP, 97);
							}


/*							-- if jumping to root, don't bother unhiliting as
								ic_mustk_dn will do it -- */

							if(markval <= 0)
							{
								Menu_stk[Mstk_ptr].choiceno = -1;
								gschoicehilite(UD_ksws, *base+pre, -1, UG_NORMAL);
							}

/*							-- pop limit state, reset application store, and inhibit
								tutorials -- */

        			 		ud_lpop();
							uu_appop_store();
							up_flag = UU_TRUE;
							semactflag = UU_TRUE;
							UD_UNMARK(markval);
					  	}
					}
				}
			}
		 	else
			{

/*				-- " error - illegal input device " -- */
				uu_uerror0 (MENUERROR, 2);
			}
		}
		else if (event.evclass == UD_STRING)
		{
/*			-- " error - illegal information" --  */

			uu_uerror0 (MENUERROR, 1);
		}
exitmain:;
	}	/* while */

/*	-- pop limit state -- */

	ud_lpop();
	ud_icmustk_dn(UU_TRUE);
	UDI_Muex_ptr--;
	UD_UNMARK(i1);
	uu_dexit;

}	/* ud_munavg2 */

/*********************************************************************
**
**    I_FUNCTION     :  ud_icmustk_up()
**								function to bring up the icon menu stack
**       
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_icmustk_up()
{
	int i, devnum, butnum;

 	uu_denter(UU_DTRC,(us, "in ud_icmustk_up"));

	if(MUEX.nav_mode == 2)
	{
		if(UDI_Muex_ptr >= 0)
		{
 			uu_dprint(UU_DTRC,(us, "nav_mode=%d, ptr=%d", MUEX.nav_mode,
							(*MUEX.menu_stk_ptr)));

			for(i=0; i<=(*MUEX.menu_stk_ptr); i++)
			{
				devnum = MUEX.menu_stk[i].menuno + (*MUEX.base);
				butnum = MUEX.menu_stk[i].choiceno;
				if(butnum > 0)
					gschoicehilite(UD_ksws, devnum, butnum, UG_HIGHLIGHTED);

 				uu_dprint(UU_DTRC,(us, "ic_stack_up,dev=%d, chc=%d", devnum,
							MUEX.menu_stk[i].choiceno));
				UD_ICMU_UP(devnum);
			}
		}
	}
	uu_dexit;
}	/* ud_icmustk_up */

/*********************************************************************
**
**    I_FUNCTION     :  ud_icmustk_dn(hiflag)
**								function to bring down the icon menu stack
**       
**    PARAMETERS   
**       INPUT  : 
**          hiflag = if UU_TRUE, then unhilite choices
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_icmustk_dn(hiflag)
UU_LOGICAL hiflag;				/* unhilite icon flag */
{
	int i, devnum;

 	uu_denter(UU_DTRC,(us, "in ud_icmustk_dn, hiflag=%d", hiflag));

	if(UDI_Muex_ptr >= 0)
	{
		if(MUEX.nav_mode == 2)
		{
 			uu_dprint(UU_DTRC,(us, "nav_mode=%d, ptr=%d", MUEX.nav_mode,
								(*MUEX.menu_stk_ptr)));

			for(i=0; i<=(*MUEX.menu_stk_ptr); i++)
			{
				devnum = MUEX.menu_stk[i].menuno + (*MUEX.base);
				UD_ICMU_DN(devnum);
/*---------------
				if(hiflag == UU_TRUE)
---------------*/
					gschoicehilite(UD_ksws, devnum, -1, UG_NORMAL);
			}
		}
	}
 	uu_dexit;
}	/* ud_icmustk_dn */
