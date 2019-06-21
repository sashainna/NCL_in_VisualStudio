/*********************************************************************
**    NAME         :  ufillb
**       CONTAINS:
**       uu_fill_bytes
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ufillb.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:53
*********************************************************************/

/*********************************************************************
**    E_FUNCTION     :  uu_fill_bytes(a_ptr,fill_char,nbytes)
**      fill a buffer with a character for n bytes 
**    PARAMETERS   
**       INPUT  : 
**				a_ptr, pointer to buffer to fill
**				f_char, the fill character
**				nbytes, number of bytes to fill
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_fill_bytes(a_ptr,f_char,nbytes)
char	*a_ptr	;		/* character pointer to buffer to fill	*/
char	f_char	;		/* the fill character						*/
int		nbytes	;		/* number of bytes to fill					*/

{
		register int	i			;	/* index								*/

		i = nbytes		;

		while(i > 0)
		{
			*a_ptr++ = f_char ;
			i-- ;
		}

		return	;
}
