
/*********************************************************************
**    NAME         :  ub2it
**       CONTAINS:
**       uu_b2it
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ub2it.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:52
*********************************************************************/

/*********************************************************************
**    E_FUNCTION     :  length = uu_b2it(txt_ptr,ivalue,base)
**       convert binary to integer text string
**    PARAMETERS   
**       INPUT  : 
**			txt_ptr - pointer to where string is to be put
**			ivalue - the number to be converted
**			base - the conversion base
**       OUTPUT :  
**			a text string representing the binary value
**    RETURNS      : number of characters used to represent the number
**    SIDE EFFECTS : none
**	  WARNINGS     :  
**		Digits are 0 - 9 and a - z;
**		if (base > 0) the number is unsigned;
**		if (base < 0) the number is signed;
**		if (base == 0) use a base of -10.
*********************************************************************/

#define UU_BUFLEN	12

int uu_b2it(txt_ptr, ivalue, base)
register char	*txt_ptr	;	/* pointer to where the text string goes	*/
unsigned int		ivalue	;	/* value to convert								*/
register int		base		;	/* base to use in converting the number	*/
{
	char buf[UU_BUFLEN];
	register char *cp, *bp;

	bp = txt_ptr;

/*
	if negative, make positive and set sign in the string
*/
	if ((int)ivalue < 0 && base <= 0) 
	{
		ivalue = -(int)ivalue;
		*txt_ptr++ = '-';
	}

/*
	if no base specified, use 10
	if base negative, set positive
*/
	if (base == 0)
		base = 10;
	else if (base < 0)
		base = -base;

	cp = &buf[UU_BUFLEN - 1];
	*cp = '\0';
	do {
		register int d;

		d = ivalue % base;
		ivalue /= base;
		*--cp = d + (d < 10 ? '0' : 'a' - 10);
	} while (ivalue != 0);
	while (*cp)
		*txt_ptr++ = *cp++;
	return (txt_ptr - bp);
}

#ifdef TEST
main()
{
	char buf[16];
	int i;

	i = 0;
	buf[i = u_b2it( buf, i, -10 )] = '\0';
	printf( "test  1 expect 0 1 got %s %d\n", buf, i );
	i = -1;
	buf[i = u_b2it( buf, i, -10 )] = '\0';
	printf( "test  2 expect -1 2 got %s %d\n", buf, i );
	i = 1;
	buf[i = u_b2it( buf, i, -10 )] = '\0';
	printf( "test  3 expect 1 1 got %s %d\n", buf, i );
	i = -32768;
	buf[i = u_b2it( buf, i, -10 )] = '\0';
	printf( "test  4 expect -32768 6 got %s %d\n", buf, i );
	i = 32767;
	buf[i = u_b2it( buf, i, -10 )] = '\0';
	printf( "test  5 expect 32767 5 got %s %d\n", buf, i );
	i = 0;
	buf[i = u_b2it( buf, i, 10 )] = '\0';
	printf( "test  6 expect 0 1 got %s %d\n", buf, i );
	i = 65535;
	buf[i = u_b2it( buf, i, 10 )] = '\0';
	printf( "test  7 expect 65535 5 got %s %d\n", buf, i );
	i = 1;
	buf[i = u_b2it( buf, i, 10 )] = '\0';
	printf( "test  8 expect 1 1 got %s %d\n", buf, i );
	i = 0xffff;
	buf[i = u_b2it( buf, i, 16 )] = '\0';
	printf( "test  9 expect ffff 4 got %s %d\n", buf, i );
	i = 0xa9fd;
	buf[i = u_b2it( buf, i, 16 )] = '\0';
	printf( "test 10 expect a9fd 4 got %s %d\n", buf, i );
	i = -234;
	buf[i = u_b2it( buf, i, 0 )] = '\0';
	printf( "test 11 expect -234 4 got %s %d\n", buf, i );
}

#endif
