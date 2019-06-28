/*********************************************************************
**
**    NAME         :  d5munav.c
**       CONTAINS:
**			ud_munavg1
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d5munav.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:12
**
*********************************************************************/

#include		"ustdio.h"
#include		"usysdef.h"
#include		"dinput.h"
#include		"gtbl.h"
#include		"dmark.h"
#include		"dasnog.h"
#include		"dasg.h"
#include		"dmenucom.h"
#include		"uhep.h"
#include        "udebug.h"
#include        "mfort.h"
#include        "nclfc.h"

#define EVIN event.indata
#define MUEX UDI_Muex_stk[UDI_Muex_ptr]

/*********************************************************************
**
**    E_FUNCTION     :  ud_munavg1 (mujtb,mustrtb,mustbsize,muftb,
**												mucases, mujsize,base,mubit, muflag)
**       menu navigator for text menus
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

ud_munavg1(mujtb, mustrtb, mustbsize, muftb, mucases, 
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
	UD_MENU_STACK_REC Menu_stk[UDI_MENU_DEPTH] ;	/* menu stack */
	int Msub_stk[UDI_SUB_DEPTH] ;		/* menu subroutine stack */
	int Mstk_ptr;				         /* Menu stack initial pointer */
	int Msub_ptr;							/* Menu subroutine stack initial pointer*/
	int i, i1, markval, save_mark_stack;
	int butnum;								/* current operator input value */
	int devnum;								/* current choice device number */
	int pre;									/* current menu number  */
	UD_DEVENT event;						/* DAS event buffer */
	UU_LOGICAL up_flag;					/* menu up occurred flag */
	char buf[4];
	UU_LOGICAL locflag;					/* command reject, propogation flag */
	UU_LOGICAL semactflag;				/* semantic action executed flag */
	UD_RPSTATE saverp;					/* save R/P state flag */


/*
.....Added for NCL501+ mode only. Paul
.....03/05/92
*/
    UM_int2 ifl, mode, mode1;
    static UU_LOGICAL GOTOROOT, GOTOEXIT;

	uu_denter(UU_DTRC,(us, " entering ud_munav()"));

/*	-- init -- */

	Mstk_ptr = -1;
	Msub_ptr = -1;
	up_flag = UU_FALSE;
	semactflag = UU_FALSE;
/*
.....Added for NCL501+ mode
.....Paul. 03/06/92
*/
        if(UDI_muwhich == 0)
        {
        GOTOEXIT = UU_FALSE;
        GOTOROOT = UU_FALSE;
        }
 
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
	MUEX.nav_mode = 1;
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
	udi_mupush(Menu_stk, &Mstk_ptr, 0);

/*	-- set environment stack pointer -- */

	UD_MARK(i1, muflag);
 	uu_dprint(UU_DTRC,(us, "in ud_munavg, markval=%d, muflag=%d", i1, locflag));

/*	-- only exit loop if jump to root -- */

	if(i1 > 0)
		ud_muexit1();

/*	-- exit loop of command reject of alt act menu -- */

	if((i1<0) && (muflag==UU_FALSE))
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
		{
			semactflag = UU_FALSE;
		}

		UD_tut_ptr = mujtb[pre].tutotb;
		if(UD_tut_mode == UD_TUT_ON)
			if(mujtb[pre].tutotb != NULL)
				if(up_flag == UU_FALSE)
					uj_tutorial(mujtb[pre].tutotb);

/*		-- record this menu -- */

		ud_rpwrcom(muftb[mujtb[pre].menuind]);

/*		-- get the next operator input --*/

		up_flag = UU_FALSE;
	  	devnum = *base+pre;
     	ud_atgevt(&event, UD_CHOICE, &muftb[mujtb[pre].menuind],
					mujtb[pre].menuno-1, devnum, UD_chcech, NULL);

	 	uu_dprint(UU_DTRC,(us, "in ud_munavg, event.evclass=%d, event.evdev=%d",
					event.evclass, event.evdev));

/*		-- for the class of choice inputs that are not from the puck do -- */

		if ((event.evclass==UD_CHOICE) && (event.evdev!=3))
		{
/*			-- if operator input 'done' then simulate a menu up input -- */

			if ((event.evdev == UD_AUXMENU))
			{
/* 
.....Added for ncl501+ mode 
.....03/05/92  Paul. 
*/ 
/*             ---------------------- go to the root ---------------- */ 

                                ifl=35; 
                                getifl(&ifl,&mode); 
                                ifl = 350; 
                                getifl(&ifl,&mode1); 
                                if(mode == 0 && mode1 == 2) 
                                {
                                    if (EVIN.choicedata == UD_CRETROOT)
                                    {
                                    if(UDI_muwhich  > 1)
                                       {
                                       MUEX.loopflag = UU_TRUE;
                                       GOTOROOT = UU_TRUE;
                                       ud_muexit1();
                                       goto exitmain;
                                       }
                                    else
                                       {
                                       while (udi_mupop(Msub_stk,&Mstk_ptr,&Msub_ptr) == UU_TRUE) {}
                                       pre = Menu_stk[Mstk_ptr].menuno;
                                       goto exitmain;
                                       }
                                    }
                                }

				if(EVIN.choicedata == UD_CDONE)
      		{
        			event.evdev = 1;
					/* MILLS:DONE was '0' but is now numeric 0 (ctrl'0') *
        			EVIN.choicedata = '0';
					*/
        			EVIN.choicedata = 0;
      		}

/*				-- exit the sub system if indicated -- */

				if(EVIN.choicedata == UD_CEXITSUB)
      		{
/*
.....Added for NCL502+ mode
.....Paul. 03/05/92
.....Now in 501+ mode only we can leave menu subsystem, even if it is
.....the first subsystem in stack.
.....Old version was:if(UDI_muwhich>1)  ud_muexit1();
*/
                                 ifl=35;
                                 getifl(&ifl, &mode);
                                 ifl=350;
                                 getifl(&ifl, &mode1);
                                 if(mode == 0 && mode1 == 2)
                                    {
                                    GOTOEXIT = UU_TRUE;
                                    ud_muexit1();
                                    MUEX.loopflag = UU_TRUE;  
                                    }
                                 else
		                    if(UDI_muwhich>1)  ud_muexit1();
		 	         pre = Menu_stk[Mstk_ptr].menuno;
                                 goto exitmain;
      		}
     		}

/*			-- if the operator struck a numeric key (choice pet = 3), 
				'command reject', or 'done' -- */

			if (event.evdev != UD_AUXMENU)
			{
/*				-- if char is numeric(hex), convert to integer -- */

				/* MILLS: changes for enhanced numeric menu nav. */
	 			/* if((EVIN.choicedata>='0')&&(EVIN.choicedata<='9')) */
				/* map '0' to numeric 10 */
	 			if (EVIN.choicedata=='0')
					butnum = EVIN.choicedata - '0' +10 ;

				/* map '1' - '9' to numeric 1 - 9 */
	 			else if((EVIN.choicedata>='1')&&(EVIN.choicedata<='9'))
					butnum = EVIN.choicedata - '0' ;

				/* This maps (shift) '1' - '9' to 11-19 */
				else if(EVIN.choicedata=='!')
					butnum = 11;
				else if(EVIN.choicedata=='@')
					butnum = 12;
				else if(EVIN.choicedata=='#')
					butnum = 13;
				else if(EVIN.choicedata=='$')
					butnum = 14;
				else if(EVIN.choicedata=='%')
					butnum = 15;
				else if(EVIN.choicedata=='^')
					butnum = 16;
				else if(EVIN.choicedata=='&')
					butnum = 17;
				else if(EVIN.choicedata=='*')
					butnum = 18;
				else if(EVIN.choicedata=='(')
					butnum = 19;
				/* This maps (shift) '0' to DONE */
				else if(EVIN.choicedata==')')
					butnum = 0;		

				/* This maps 'a' - 'f' to 11-16 */
				/*
				else if((EVIN.choicedata>='a')&&(EVIN.choicedata<='f'))
					butnum = EVIN.choicedata - 'a' + 10;
				*/
				else 
					butnum = EVIN.choicedata;
		
/*				-- menu up -- */

		 		if (butnum == 0)
				{
					up_flag = UU_TRUE;

/*					-- if at menu root and not at first menu, exit this
						subsystem -- */

					if((udi_mupop(Msub_stk,&Mstk_ptr,&Msub_ptr)==UU_FALSE) &&
							(UDI_muwhich>1))
						ud_muexit1();
			 		else
			 			pre = Menu_stk[Mstk_ptr].menuno;
				}

/*				-- if valid menu choice, traverse down -- */

		 		else if (butnum < mujtb[pre].menuno)	
				{
			 		if (mujtb[pre].infor[butnum-1] >= 0)
					{
/*						-- if not at a leaf traverse down -- */

				 	 	pre = mujtb[pre].infor[butnum-1];
				 	 	udi_mupush(Menu_stk,&Mstk_ptr,pre);
					}
					else
					{

/*						-- at a leaf node, execute semantic action -- */

				  	 	UD_MARK(markval, UU_TRUE);

 						uu_dprint(UU_DTRC,(us, "in ud_munavg 1, markval=%d",
									markval));

				  		if (markval == 0)
                 	{
 
/*							-- set application stack and set limit state -- */

							uu_appush_store();
							ud_lpsh(locflag);

/*							-- execute sematic action, check auto save -- */

							save_mark_stack = UD_markptr;

/*							-- set up for recording autotest mode so that
								menu leaf label is recorded -- */

							if(UD_autotest==UU_TRUE && 
											UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
								ud_record_auto(muftb[mujtb[pre].menuind],
													muftb[mujtb[pre].menuind+butnum]);

/*							-- execute semantic action -- */

			     			(*mucases)(-mujtb[pre].infor[butnum-1]);
/*
.....Added for NCL501+ mode
.....03/06/92. Paul
*/
                                                 if(GOTOEXIT == UU_TRUE)
                                                   {
                                                    MUEX.loopflag = UU_TRUE;
                                                    if (UDI_muwhich = 1) UD_UNMARK(i1);                   
                                                    ud_muexit1();
                                                    goto exitmain;
                                                    }
                                                  if(GOTOROOT == UU_TRUE)
                                                    {
                                                    if(UDI_muwhich > 1)
                                                      {
                                                      MUEX.loopflag = UU_TRUE;
                                                      ud_muexit1();
                                                      goto exitmain;
                                                      }
                                                    else
                                                      {
                                                      GOTOROOT = UU_FALSE;
                                                      pre = 0;
                                                      }  
                                                    }

                                                        
							ur_cond_auto_save();
							ncl_cond_auto_save();
                 	}

/*						-- test mark stack state -- */

						if(save_mark_stack != UD_markptr)
						{
						uu_dprint(UU_DTRC,(us, "MARK imbalance, save=%d, current=%d", 
											save_mark_stack, UD_markptr));
							uu_uerror0(UD_DASHEP, 97);
						}

/*						-- pop limit state, reset application store, and inhibit
							tutorials -- */

        			 	ud_lpop();
						uu_appop_store();
						up_flag = UU_TRUE;
						semactflag = UU_TRUE;

						UD_UNMARK(markval);
				  	}
				}
				else					  /* input is a character  */
				{
					if (butnum == 'q')
					{
/*						-- return from previous 'call' -- */

						 udi_subpop(Msub_stk,&Mstk_ptr,&Msub_ptr);
						 pre = Menu_stk[Mstk_ptr].menuno;
					}
					else
					{
/*						-- check for either goto or call -- */

						buf[0] = butnum;
						buf[1] = '\0';
						if (udi_mumatch(mujtb,mustrtb,muftb,mustbsize, 
								Menu_stk, Msub_stk, &Mstk_ptr,&Msub_ptr, mucases,
								&pre,buf,mujsize,base,mubit,muflag) == UU_FALSE)
						{
/*							-- unrecognized label, " error - illegal information" */

							 uu_uerror0 (MENUERROR, 1);
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
/*						-- check for either goto or call -- */

			if (udi_mumatch(mujtb, mustrtb, muftb, mustbsize, Menu_stk, 
					Msub_stk,  &Mstk_ptr , &Msub_ptr, mucases,  &pre, 
					EVIN.stringdata, mujsize, base, mubit, muflag) == UU_FALSE)
			{
/*				-- " error - illegal information" --  */

				uu_uerror0 (MENUERROR, 1);
			}
		}
exitmain:;
	}	/* while */

/*	-- pop limit state -- */

	ud_lpop();
	UDI_Muex_ptr--;
	UD_UNMARK(i1);
	uu_dexit;

}


