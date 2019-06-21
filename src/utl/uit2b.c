/*********************************************************************
**    NAME         :  uit2b
**       CONTAINS:
**       uu_it2b
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uit2b.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:54
*********************************************************************/

#include "uctype.h"

/*********************************************************************
**    E_FUNCTION     :  type name (parameters)
**      convert integer text to binary
**    PARAMETERS   
**       INPUT  : 
**			txt_ptr - pointer to string
**			txt_len - length of string
**			ivalue - pointer to result
**			base - base of conversion
**       OUTPUT :  
**			ivalue - the computed value from txt_ptr
**    RETURNS      : the number of characters scanned
**    SIDE EFFECTS : none
**    WARNINGS     : 
**		Skips leading whitespace;
**		Allows sign;
**		If the base is 16 allows leading "0x" or "0X";
**		Allows numbers '0' to '9', letters 'a' to 'z' or 'A' to 'Z';
**		Allows trailing "l" or "L".
*********************************************************************/

int uu_it2b(txt_ptr, txt_len, ivalue, base)

register	char	*txt_ptr	;	/* pointer to text string for value	*/
int			txt_len			;	/* length of string pointed to		*/
int			*ivalue			;	/* pointer to returned value			*/
int			base				;	/* i.e. 8,10,16							*/

{
	register	int	count	;	/* characters count in buffer			*/
	register	int	i		; 	/* numeric value for current digit	*/
	register int	num	;	/* accumulated number					*/
	register	int	sign	;	/* sign flag, true if negative		*/

count = txt_len	;
/*
	strip off leading nulls & spaces
*/
	while (count > 0 && isspace(*txt_ptr)) 
	{
		txt_ptr++;
		count--;
	}

/*
	set sign , default to sign=FALSE indicates positive
*/
	sign = 0 ;
	if (count > 0) {
		if (*txt_ptr == '-') {
			sign++;
			txt_ptr++;
			count--;
		} else if (*txt_ptr == '+') {
			txt_ptr++;
			count--;
		}
	}

/*
	strip off leading '0x' or '0X' if hexadecimal
*/
	if (base == 16 && count >= 2 && *txt_ptr == '0' &&
	    (txt_ptr[1] == 'x' || txt_ptr[1] == 'X')) 
	{
		txt_ptr += 2;
		count -= 2;
	}

	num = 0 ;
	while (count > 0) {
		if (isdigit(*txt_ptr))
			i = *txt_ptr - '0';
		else if (islower(*txt_ptr))
			i = *txt_ptr - ('a' - 10);
		else if (isupper(*txt_ptr))
			i = *txt_ptr - ('A' - 10);
		else
		{
			break;
		}
			
		if (base > i)
		{
			num = (num * base) - i; /* accumulate as negative */
			txt_ptr++;
			count--;
		}
		else
			break ;
	}

	if (count > 0 && count != txt_len && (*txt_ptr == 'L' || *txt_ptr == 'l'))
		count--;
	*ivalue = (sign ? num : -num);
	return (txt_len - count);
}
