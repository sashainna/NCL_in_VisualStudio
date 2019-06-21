/*********************************************************************
**    NAME         :  ubits
**       CONTAINS:
**				uu_set_bit
**				uu_clr_bit
**				uu_tst_bit
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ubits.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:52
*********************************************************************/

#include	"usysdef.h"

/*********************************************************************
**    I_FUNCTION     :  uu_set_bit(word_ptr,ibit)
**      set a bit in a bitmap array,
**		 bits are number right to left starting at bit 0,
**		 i.e. - word 0 on a 32 bit machine contains bits 31-0
**    PARAMETERS   
**       INPUT  : 
**			word_ptr,	pointer to an array of unsigned integers
**			ibit,			bit to set
**       OUTPUT :  
**          none
**    RETURNS      :  0 if bit not set, non-zero if set
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_set_bit(word_ptr,ibit)
unsigned	long	word_ptr[];				/* ptr to bitmap */
int ibit;									/* bit to set */
{
#ifdef UU_P2BITPW
	/* this version fast for power of 2 bits per word */
	word_ptr[(ibit & (~UU_BPWMASK)) >> UU_BPWSHFT] |= 1 << (ibit & UU_BPWMASK);
#else
	/* this version will work anywhere */
	int	i,j;		/* indexes */

	i = ibit  / UU_BITPW;	/* calculate position in the array	*/
	j = ibit - (i * UU_BITPW);
	word_ptr[i] |= (1 << j);
	return;
#endif
}

/*********************************************************************
**    I_FUNCTION     :  uu_clr_bit(word_ptr,ibit)
**      set a bit in a bitmap array,
**		 bits are number right to left starting at bit 0,
**		 i.e. - word 0 on a 32 bit machine contains bits 31-0
**    PARAMETERS   
**       INPUT  : 
**			word_ptr,	pointer to an array of unsigned integers
**			ibit,			bit to clear
**       OUTPUT :  
**          none
**    RETURNS      :  0 if bit not set, non-zero if set
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_clr_bit(word_ptr,ibit)
unsigned	long	word_ptr[];			/* ptr to bitmap */
int ibit;								/* bit to clear */
{
#ifdef UU_P2BITPW
	/* fast version if possible */
	word_ptr[(ibit & (~UU_BPWMASK)) >> UU_BPWSHFT] &= ~(1<<(ibit & UU_BPWMASK));
#else
	/* robust version if we have too */
	int	i,j;					/* indexs */

	i = ibit  / UU_BITPW;	/* calculate position in the array	*/
	j = ibit - (i * UU_BITPW);
	word_ptr[i] &= ~(1 << j);
	return;
#endif
}

/*********************************************************************
**    E_FUNCTION     :  uu_tst_bit(word_ptr,ibit)
**      test a bit in a bitmap array,
**		 bits are number right to left starting at bit 0,
**		 i.e. - word 0 on a 32 bit machine contains bits 31-0
**    PARAMETERS   
**       INPUT  : 
**			word_ptr,	pointer to an array of unsigned integers
**			ibit,			bit to test
**       OUTPUT :  
**          none
**    RETURNS      :  0 if bit not set, non-zero if set
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_tst_bit(word_ptr,ibit)
unsigned	long	word_ptr[];	/* ptr to bitmap								*/
int 				ibit		; /* bit to test									*/
{
#ifdef UU_P2BITPW
	/* fast version if possible */
	return((word_ptr[(ibit&(~UU_BPWMASK))>>UU_BPWSHFT] & (1<<(ibit&UU_BPWMASK)))!=0);
#else
	/* robust version if we have too */
	int	i,j				;/* indexs											*/

	i = ibit  / UU_BITPW;	/* calculate position in the array	*/
	j = ibit - (i * UU_BITPW);
	return((word_ptr[i] & (1 << j))!=0);
#endif
}

#ifdef TEST
main()
{
	long	words[4];
	int	i,j;
	unsigned long set, test, clear;

	for (i=0; i<40; i++)
	{
		uu_set_bit(words,i);
		set = words[i/UU_BITPW];
		test = uu_tst_bit(words,i);
		uu_clr_bit(words,i);
		clear = words[i/UU_BITPW];
		printf(" for i = %d set = 0x%x test = 0x%x clear = 0x%x\n",
					i,set,test,clear);
	}
	words[0] = -1;
	for (i=0;i<32;i++)
	{
		uu_clr_bit(words,i);
		clear = words[0];
		printf(" words[0] = 0x%x\n",words[0]);
	}
}
#endif
