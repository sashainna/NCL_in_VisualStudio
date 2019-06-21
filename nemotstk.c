/*********************************************************************
**    NAME         :  nemotstk.c
**       CONTAINS:
**          ncl_mot_stack_init
**          ncl_mot_stack_beg
**          ncl_mot_stack_end
**          ncl_mot_stack_erase
**          ncl_mot_stack_delete
**          ncl_mot_stack_cldel
**          ncl_mot_stack_range
**          ncl_mot_stack_range_set
**          ncl_mot_stack_off
**          ncl_mot_stack_on
**          ncl_mot_stack_print
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nemotstk.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:38
*********************************************************************/
#include "usysdef.h"
#define NCLSTACK
#include "nclstack.h"
#undef NCLSTACK
#include "nclfc.h"

static UU_LOGICAL Sfirst=UU_TRUE;
static UU_LOGICAL Sdelete;
static UN_motseg *Smpt;

void ncl_mot_stack_delete();
void ncl_mot_stack_print();

#define PRINTIT UU_FALSE

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_init()
**       Initializes the motion display stack, which contains pointers
**       to the motion display and clfile position for separate motion
**       commands.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mot_stack_init()
{
/*
.....Been here before
.....Free up the previous stack
*/
	if (!Sfirst)
	{
		if (UN_mot_stack != UU_NULL) uu_free(UN_mot_stack);
		UN_mot_stack = UU_NULL;
	}
/*
.....Allocate the motion display stack
*/
	if (UN_mot_stack_size != 0)
	{
		UN_mot_stack = (UN_mot_stack_struc *)
			uu_malloc(sizeof(UN_mot_stack_struc)*UN_mot_stack_size);
		if (UN_mot_stack == UU_NULL) UN_mot_stack_size = 0;
		UN_mot_stack_state = UU_TRUE;
	}
	else
	{
		UN_mot_stack = UU_NULL;
		UN_mot_stack_state = UU_FALSE;
	}
	Sfirst = UU_FALSE;
/*
.....Initialize pointers
*/
	UN_mot_stack_ptr = 0;
	UN_mot_stack_num = 0;
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_beg(mpt,cpt,spt)
**       This function should be called when a new motion sequence
**       (generated by a single command) is about to be displayed.  It
**       stores the motion display list pointer and clfile pointer onto
**       the motion display stack.
**       commands.
**    PARAMETERS   
**       INPUT  : 
**          mpt   = Current motion display pointer.
**          cpt   = Current clfile pointer.
**          spt   = Current tool location (without TRACUT).
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mot_stack_beg(mpt,cpt,spt)
UN_motseg *mpt;
UN_clstruc *cpt;
UU_REAL spt[6];
{
	int i,nc,inc;
	UM_int2 ifl,val;
	UM_f77_str fsym[3];
	char sym[3][MAXSYMLEN+2];
	UN_mot_stack_struc *stkptr;
/*
.....Save the motion display and
.....clfile structure pointers
*/
	if (UN_mot_stack_state)
	{
		if (UN_mot_stack_ptr == UN_mot_stack_size) UN_mot_stack_ptr = 0;
		stkptr = &UN_mot_stack[UN_mot_stack_ptr];
		stkptr->mbegin = mpt;
		stkptr->cbegin = cpt;
/*
.....Save the motion attributes
........RAPID
*/
		rpchk(&stkptr->rapid);
/*
........MULTAX
*/
		ifl = 82;
		getifl(&ifl,&stkptr->multax);
/*
........CUTTER
*/
		UM_init_f77_str(fsym[0],sym[0],MAXSYMLEN);
		UM_init_f77_str(fsym[1],sym[1],MAXSYMLEN);
		UM_init_f77_str(fsym[2],sym[2],MAXSYMLEN);
		gdscut(stkptr->cutr,fsym[0],fsym[1],fsym[2],stkptr->symkey,stkptr->cfl);
		nc = MAXSYMLEN;
		ul_strip_blanks(sym[0],&nc);
		nc = MAXSYMLEN;
		ul_strip_blanks(sym[1],&nc);
		nc = MAXSYMLEN;
		ul_strip_blanks(sym[2],&nc);
		strcpy(stkptr->symbol[0],sym[0]);
		strcpy(stkptr->symbol[1],sym[1]);
		strcpy(stkptr->symbol[2],sym[2]);
/*
........TRACUT
*/
		ifl = 73;
		getifl(&ifl,&val);
		stkptr->trafl = val;
		gettra(stkptr->tracut,stkptr->invtra);
/*
........FROM
*/
		for (i=0;i<6;i++) stkptr->spt[i] =spt[i];
/*
.....Make sure previous stack position
.....ends at same location this one starts at
.....Just in case motion was displayed
.....outside of 'motbgn' and 'motend'
.....For example, Preview motion
*/
		if (UN_mot_stack_num > 0)
		{
			inc = UN_mot_stack_ptr - 1;
			if (inc == -1) inc = UN_mot_stack_size - 1;
			if (UN_mot_stack[inc].mend != UU_NULL) UN_mot_stack[inc].mend = mpt;
			if (UN_mot_stack[inc].cend != UU_NULL) UN_mot_stack[inc].cend = cpt;
		}
/*
.....Print the stack
*/
		ncl_mot_stack_print(UN_mot_stack_ptr,UN_mot_stack_num+1,"Begin");
	}
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_end(mpt,cpt)
**       This function should be called at the end of a new motion
**       sequence display.  It stores the motion display list pointer
**       and clfile pointer onto the motion display stack.
**       commands.
**    PARAMETERS   
**       INPUT  : 
**          mpt   = Current motion display pointer.
**          cpt   = Current clfile pointer.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mot_stack_end(mpt,cpt)
UN_motseg *mpt;
UN_clstruc *cpt;
{
/*
.....Save the motion display and
.....clfile structure pointers
*/
	if (UN_mot_stack_state)
	{
		UN_mot_stack[UN_mot_stack_ptr].mend = mpt;
		UN_mot_stack[UN_mot_stack_ptr].cend = cpt;
/*
.....Print the stack
*/
		ncl_mot_stack_print(UN_mot_stack_ptr,UN_mot_stack_num+1,"End");
/*
.....Adjust the stack pointers
*/
		UN_mot_stack_ptr++;
		if (UN_mot_stack_num < UN_mot_stack_size) UN_mot_stack_num++;
	}
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_erase(mpt)
**       Zeros out the motion display pointers in the requested motion
**       stack entity to the end of the stack.  If 'mpt' is equal to
**       UU_NULL, then all pointers on the stack will be zeroed out.
**       The clfile pointers will remain intact.
**    PARAMETERS   
**       INPUT  : 
**          mpt   = Pointer to motion display start that is being erased.
**                  When set to UU_NULL, then all display pointers on
**                  the stack will be zeroed out.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mot_stack_erase(mpt)
UN_motseg *mpt;
{
	int i,inc,num;
	UU_LOGICAL lfl;
/*
.....Make sure stack is active
*/
	if (UN_mot_stack_state)
	{
/*
.....Erase the requested motion display pointer
.....to the end of the stack
*/
		if (mpt != UU_NULL)
		{
			inc = UN_mot_stack_ptr - 1;
			if (inc == -1) inc = UN_mot_stack_size - 1;
/*
........Make sure this display pointer
........is stored in a stack entry
*/
			lfl = UU_FALSE;
			for (i=0;i<UN_mot_stack_num;i++)
				if (UN_mot_stack[i].mbegin == mpt) lfl = UU_TRUE;
/*			if (!lfl) UN_mot_stack[inc].mbegin = mpt;*/
/*
........Zero out the display pointers
*/
			if (lfl)
			{
				lfl = UU_FALSE;
				num = UN_mot_stack_num;
				for (i=0;i<num;i++)
				{
					if (mpt == UN_mot_stack[inc].mbegin)
					{
						lfl = UU_TRUE;
/*						if (Sdelete)
						{
							inc1 = inc - 1;
							if (inc1 < 0) inc1 = inc1 + num;
							if (UN_mot_stack[inc1].mend == mpt)
								UN_mot_stack[inc1].mend = Smpt;
						}*/
					}
					UN_mot_stack[inc].mbegin = UU_NULL;
					UN_mot_stack[inc].mend = UU_NULL;
					if (Sdelete)
					{
						UN_mot_stack_num--;
						UN_mot_stack_ptr--;
					}
					if (lfl) break;
					inc--;
					if (inc < 0) inc = inc + num;
				}
				if (UN_mot_stack_ptr < 0)
					UN_mot_stack_ptr = UN_mot_stack_ptr + UN_mot_stack_size;
			}
		}
/*
.....Erase all display pointers
*/
		else
		{
			for (i=0;i<UN_mot_stack_num;i++)
			{
					UN_mot_stack[i].mbegin = UU_NULL;
					UN_mot_stack[i].mend = UU_NULL;
			}
		}
/*
.....Print the stack
*/
		if (Sdelete)
		{
			ncl_mot_stack_print(UN_mot_stack_ptr-1,UN_mot_stack_num,"Delete");
		}
	}

/*
.....End of routine
*/
	ncl_mot_stack_delete(UU_FALSE,UU_NULL);
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_delete(flag,mpt)
**       Specifies whether the stack entries should be deleted or the
**       motion just erased on the next call to 'ncl_mot_stack_erase'.
**    PARAMETERS   
**       INPUT  : 
**          flag  = UU_TRUE = delete stack entry.
**                  UU_FALSE = erase motion only.
**          mpt   = Pointer to next slot in motion display array.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mot_stack_delete(flag,mpt)
UU_LOGICAL flag;
UN_motseg *mpt;
{
	Sdelete = flag;
	Smpt = mpt;
}

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_cldel(lpt,cpt)
**       This function is called when clfile records are being deleted.
**       It replaces the clfile location (lpt) within the stack with the
**       new postion (cpt).
**    PARAMETERS   
**       INPUT  : 
**          lpt   = Clfile record being deleted.
**          cpt   = New current clfile record.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mot_stack_cldel(lpt,cpt)
UN_clstruc *lpt,*cpt;
{
	int i;
/*
.....Replace clfile pointers
*/
		for (i=0;i<UN_mot_stack_num;i++)
		{
			if (lpt == UN_mot_stack[i].cbegin) UN_mot_stack[i].cbegin = cpt;
			if (lpt == UN_mot_stack[i].cend) UN_mot_stack[i].cend = cpt;
		}
}

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_range(ofset,rang)
**       Returns the motion display and clfile ranges along with the
**       motion attributes for the requested motion off of the motion
**       display stack.
**    PARAMETERS   
**       INPUT  : 
**          ofset = An offset from the last displayed motion to get
**                  the range for.  For example, 0 = Last displayed
**                  motion, 1 = Next previous displayed motion, etc.
**       OUTPUT : 
**          rang  = Range of this motion display.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_mot_stack_range(ofset,rang)
int ofset;
UN_mot_stack_struc *rang;
{
	int iret,inc,is1;
	UN_clstruc *crs;
/*
.....Make sure there are enough
.....displays on the stack
*/
	iret = UU_FAILURE;
	if (ofset < UN_mot_stack_num)
	{
		iret = UU_SUCCESS;
/*
.....Calculate pointer to motion display range
*/
		inc = UN_mot_stack_ptr - ofset - 1;
		if (inc < 0) inc = inc + UN_mot_stack_num;
/*
.....Setup range
*/
		crs = rang->crec; is1 = rang->cpt;
		uu_move_byte(&UN_mot_stack[inc],rang,sizeof(UN_mot_stack_struc));
		rang->crec = crs; rang->cpt = is1;
	}
/*
.....End of routine
*/
	return(iret);
}

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_range_set(ofset,rang)
**       Sets the motion display and clfile ranges for the requested
**       motion on the motion display stack.
**    PARAMETERS   
**       INPUT  : 
**          ofset = An offset from the last displayed motion to get
**                  the range for.  For example, 0 = Last displayed
**                  motion, 1 = Next previous displayed motion, etc.
**          rang  = Range of this motion display.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_mot_stack_range_set(ofset,rang)
int ofset;
UN_mot_stack_struc *rang;
{
	int iret,inc;
/*
.....Make sure there are enough
.....displays on the stack
*/
	iret = UU_FAILURE;
	if (ofset < UN_mot_stack_num)
	{
		iret = UU_SUCCESS;
/*
.....Calculate pointer to motion display range
*/
		inc = UN_mot_stack_ptr - ofset - 1;
		if (inc < 0) inc = inc + UN_mot_stack_num;
/*
.....Setup range
*/
		UN_mot_stack[inc].mbegin = rang->mbegin;
		UN_mot_stack[inc].mend = rang->mend;
		UN_mot_stack[inc].cbegin = rang->cbegin;
		UN_mot_stack[inc].cend = rang->cend;
	}
/*
.....End of routine
*/
	return(iret);
}

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_off()
**       Disables the motion display stack.  When disabled, no
**       modifications will be made to the stack.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mot_stack_off()
{
/*
.....Set the state of the Motion Display Stack
*/
	UN_mot_stack_state = UU_FALSE;
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_on()
**       Enables the motion display stack if it was enabled by the
**       user.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mot_stack_on()
{
/*
.....Set the state of the Motion Display Stack
*/
	if (UN_mot_stack_size != 0) UN_mot_stack_state = UU_TRUE;
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_mot_stack_print(iend,num_ent,stxt)
**       Prints out the contents of the motion dislay stack.
**    PARAMETERS   
**       INPUT  : 
**          iend      = Last entry to print.
**          nent      = Number of entries to print.
**          stxt      = Calling routine text.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mot_stack_print(iend,nent,stxt)
int iend,nent;
char *stxt;
{
	int inc,n,i;
/*
.....Print out header
*/
	if (PRINTIT)
	{
		printf("\n%s\n==========\n",stxt);
/*
.....Print out stack
*/
		inc = iend;
		n = nent; if (n > UN_mot_stack_size) n = UN_mot_stack_size;
		for (i=0;i<n;i++)
		{
			if (inc < 0) inc = inc + UN_mot_stack_size;
			printf("Range[%d] = %x,%x\n",inc,UN_mot_stack[inc].mbegin,
				UN_mot_stack[inc].mend);
			printf("Clfile[%d] = %x,%x\n",inc,UN_mot_stack[inc].cbegin,
				UN_mot_stack[inc].cend);
			inc--;
		}
	}
}
