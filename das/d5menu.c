/*********************************************************************
**
**    NAME         :  dmenus.c
**       CONTAINS:
**       	udi_muflush		
**				udi_mupop		
**				udi_mupush	
**				udi_subpop	
**				udi_subpush
**       	ud_muexit
**				udi_mufindsym
**				udi_mumatch
**    		ud_muexit()
**    		ud_muexit1()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d5menu.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:11
**
*********************************************************************/

#include "usysdef.h"
#include "dmenucom.h"
#include "dinput.h"
#include	"uhep.h"
#include	"dmark.h"
#include	"udebug.h"

#define MUEX UDI_Muex_stk[UDI_Muex_ptr]

/*********************************************************************
**
**    I_FUNCTION     :  udi_mupush(Menu_stk, Mstk_ptr, munum)
**       push the current menu choice on the menu stack	
**
**    PARAMETERS   
**       INPUT  : 
**          Menu_stk - menu stack
**				Mstk_ptr _ menu stack pointer
**				munum _ menu number to push
**       OUTPUT :  
**          Mstk_ptr _ menu stack pointer
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

udi_mupush(Menu_stk, Mstk_ptr, munum)
UD_MENU_STACK_REC Menu_stk[];
int *Mstk_ptr;
int munum;							/* menu number to push */
{

	(*Mstk_ptr)++ ;					/* point to next free stack location */
	if (*Mstk_ptr<UDI_MENU_DEPTH && *Mstk_ptr>=0)
	{
		Menu_stk[*Mstk_ptr].menuno = munum ;
		Menu_stk[*Mstk_ptr].choiceno = -1 ;
	}
	else
	{
		/*  derror("menu stack overflow");  */
		uu_uerror0 (MENUERROR, 4);
		*Mstk_ptr = 0;
	}
	return ;
}

/*********************************************************************
**
**    I_FUNCTION     :  udi_mupop(Msub_stk, Mstk_ptr, Msub_ptr)
**       to pop the top element on the menu stack	
**
**    PARAMETERS   
**       INPUT  : 
**          Msub_stk - substack contains index to the menu stack
**				Mstk_ptr - current index to menu stack
**			   Msub_ptr - current index to substack
**       OUTPUT :  
**          Mstk_ptr - current index to menu stack
**          Msub_ptr - current index to substack
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

udi_mupop(Msub_stk, Mstk_ptr, Msub_ptr)
int	Msub_stk[];
int	*Mstk_ptr, *Msub_ptr;
{

	(*Mstk_ptr)--;
	if (*Mstk_ptr < 0)
  	{
		*Mstk_ptr = 0;
		return (UU_FALSE);
  	}
	else if(*Msub_ptr >= 0)

/*	-- if the menu number at the top of the stack is <= to the menu number on
		the subroutine stack, then pop the subroutine stack also -- */

	if(Msub_stk[*Msub_ptr] >= *Mstk_ptr)
		(*Msub_ptr)--;

	return(UU_TRUE);
}

/*********************************************************************
**
**    I_FUNCTION     :  udi_muflush(Mstk_ptr, Msub_ptr)
**       flush menu stack	
**
**    PARAMETERS   
**       INPUT  : 
**          Mstk_ptr - current index to menu stack
**          Msub_ptr - current index to substack
**       OUTPUT :  
**          Mstk_ptr - current index to menu stack
**          Msub_ptr - current index to substack
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

udi_muflush(Mstk_ptr, Msub_ptr)
int	*Mstk_ptr, *Msub_ptr;
{
	*Mstk_ptr = -1;
	*Msub_ptr = -1;
}

/*********************************************************************
**
**    I_FUNCTION     :  udi_subpush(Msub_stk, Mstk_ptr, Msub_ptr)
**       push menu stack pointer value on subroutine stack
**
**    PARAMETERS   
**       INPUT  : 
**          Msub_stk - substack to contain the index to menu stack
**				Mstk_ptr - menu stack
**				Msub_ptr - substack index
**       OUTPUT :  
**          Msub_ptr - substack index
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

udi_subpush(Msub_stk, Mstk_ptr, Msub_ptr)
int	Msub_stk[];
int	Mstk_ptr, *Msub_ptr;
{

	(*Msub_ptr)++ ;
	if(*Msub_ptr < UDI_SUB_DEPTH)
		Msub_stk[*Msub_ptr] = Mstk_ptr ;
	else
	{
		/* derror("Menu subroutine stack overflow"); */
		uu_uerror0 (MENUERROR, 5);
		*Msub_ptr = -1;
	}
}

/*********************************************************************
**
**    I_FUNCTION     :  udi_subpop(Msub_stk, Mstk_ptr, Msub_ptr)
**       pop menu stack pointer to value on subroutine stack
**
**    PARAMETERS   
**       INPUT  : 
**          Msub_stk - substack contains index to menu stack
**				Mstk_ptr - index to menu stack
**				Msub_ptr - index to substack
**       OUTPUT :  
**          Mstk_ptr - index to menu stack
**          Msub_ptr - index to substack
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

udi_subpop(Msub_stk, Mstk_ptr, Msub_ptr)
int	Msub_stk[];
int	*Mstk_ptr, *Msub_ptr;
{

	if(*Msub_ptr >= 0)
	{
		*Mstk_ptr = Msub_stk[*Msub_ptr] ;
		*Msub_ptr--;
	}
}

/*********************************************************************
**    I_FUNCTION     :  udi_mumatch (mujtb,mustrtb,muftb,mustbsize,
**                      Menu_stk,Msub_stk, Mstk_ptr,Msub_ptr,mucases,pre,sym,
**								mujsize,base,mubit,muflag)
**
**       for an input character or string seach the string table and
**       perform the corresponing action if the string is found.
**
**    PARAMETERS   
**
**       INPUT  : 
**          sym - the input string or character
**          Menu_stk - stack contains the path of the menu traversed
**          Msub_stk - stack contains the marked menu
**          Mstk_ptr - index to Menu_stk
**          Msub_ptr - index to Msub_stk
**				muflag   - alternate action flag (if FALSE)
**
**       OUTPUT :  
**          pre - index of the string in the string table
**          Menu_stk - stack contains the path of the menu traversed
**          Msub_stk - stack contains the marked menu
**          Mstk_ptr - index to Menu_stk
**          Msub_ptr - index to Msub_stk
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

udi_mumatch (mujtb,mustrtb,muftb,mustbsize,Menu_stk,Msub_stk,
			Mstk_ptr,Msub_ptr,mucases,pre,sym,mujsize,base,mubit,muflag)
UDI_MUJTB mujtb[];					/* that holds state info for each node table */
int mustbsize;
UDI_MUSTR mustrtb[];
char	*muftb[];
int	Msub_stk[];
UD_MENU_STACK_REC	Menu_stk[];
int	*Mstk_ptr, *Msub_ptr;
int	(*mucases)();
int	*pre,mujsize,*base,mubit;
char	sym[];
int muflag;
{
	int	i, i1, id;
	char	buf[80];
	char	us[80];

	if (udi_mufindsym(mustrtb,mustbsize,sym,&id))
	{
		if (mustrtb[id].path[0] == UDI_MUCALL)		/* mark & push */
		{
			 udi_subpush(Msub_stk,*Mstk_ptr,Msub_ptr);
			 *pre = mustrtb[id].path[3];
			 udi_mupush(Menu_stk,Mstk_ptr,*pre);
			 if (mustrtb[id].path[1]==0)			/* a leaf node */
			 {
				 UD_MARK(i1, UU_TRUE);

	 			uu_dprint(UU_DTRC,(us, "in ud_munavg 2, markval=%d", i1));

				if (i1 == 0)
           	{
 
/*					--	propogate limit state if alternate action -- */

						if (muflag==UU_TRUE)  
  							ud_lpsh(UU_FALSE);
						else
  							ud_lpsh(UU_TRUE);

					   (*mucases)(-mujtb[*pre].infor[mustrtb[id].path[2]-1]);
						ur_cond_auto_save();
						ncl_cond_auto_save();
				}
				if (i1>0)
				{
					ud_muexit();
					UDI_Muex_ptr--;
				}

/*				 -- pop limit state -- */

        		ud_lpop();

				UD_UNMARK(i1);
			}
			return (UU_TRUE);
		}
		else if (mustrtb[id].path[0] == UDI_MUGOTO)	/* flush & push */
		{
			udi_muflush(Mstk_ptr,Msub_ptr);
			for (i=0; i<mustrtb[id].path[3]; i++)
				udi_mupush(Menu_stk,Mstk_ptr,mustrtb[id].path[i+4]);
			*pre = Menu_stk[*Mstk_ptr].menuno;
			if (mustrtb[id].path[1]==0)			/* a leaf node */
			{
			   UD_MARK(i1, UU_TRUE);

	 			uu_dprint(UU_DTRC,(us, "in ud_munavg 3, markval=%d", i1));

			   if (i1 == 0)
            {
 
/*				  --	propogate limit state if alternate action -- */

						if (muflag==UU_TRUE)  
  						ud_lpsh(UU_FALSE);
						else
  						ud_lpsh(UU_TRUE);

				   	(*mucases)(-mujtb[*pre].infor[mustrtb[id].path[2]-1]);
						ur_cond_auto_save();
						ncl_cond_auto_save();
            }
		 		if (i1>0)
				{
					ud_muexit();
					UDI_Muex_ptr--;
				}

/*				 -- pop limit state -- */

     			ud_lpop();

			   ud_jmpmark (i1);
			}
			return (UU_TRUE);
		}
	}
	else
		return (UU_FALSE);
}	/* udi_mumatch */

/*********************************************************************
**
**    I_FUNCTION     :  udi_mufindsym (mustrtb,mustbsize,sym,id)
**       search through the menu string table to find the index of the
**       input string
**
**    PARAMETERS   
**       INPUT  : 
**          mustrtb - the menu string table
**          mustbsize - number of GOTO and CALL strings
**          sym - the input string
**       OUTPUT :  
**          id - the index
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

udi_mufindsym (mustrtb,mustbsize,sym,id)
UDI_MUSTR	mustrtb[];
int	mustbsize;
char	sym[];
int	*id;

{
	int	i, found;

	found = UU_FALSE;
	for (i=0; (i<mustbsize)&&(found==UU_FALSE); i++)
	{
		if(strcmp(mustrtb[i].sym, sym) == 0)
		{
			 found = UU_TRUE;
			 *id = i;
		}
	}
	return (found);
}	/* udi_mufindsym */

/*********************************************************************
**
**    E_FUNCTION     :  ud_muexit()
**       set flag to exit the menu traversing
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

ud_muexit()
{
	if(!((UD_autotest==UU_TRUE) &&
		 (UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)))
	{
		MUEX.loopflag = UU_TRUE;
		if(UDI_muwhich > 0)
			UDI_muwhich--;
		if(UD_sstrc_ptr > 0)
			UD_sstrc_ptr--;
	}
}		/* ud_muexit */

/*********************************************************************
**
**    E_FUNCTION     :  ud_muexit1()
**       Set flag to exit the menu traversing regardless of auto-test
**			state.  This is used to exit the applications
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

ud_muexit1()
{
	MUEX.loopflag = UU_TRUE;
	if(UDI_muwhich >= 0)
		UDI_muwhich--;
	if(UD_sstrc_ptr > 0)
		UD_sstrc_ptr--;
}
