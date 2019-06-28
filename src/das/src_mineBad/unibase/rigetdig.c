#define MODULEDBG 0
/*********************************************************************
**    NAME         :  rigetdig.c
**       CONTAINS:
**       ur_get_digit()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rigetdig.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:43
*********************************************************************/

#define isdigit(c) (((c)>='0')&&((c)<='9'))

/*********************************************************************
**    I_FUNCTION     :  int ur_get_digit(fd, tokn_str)
**       finishes reading a numeric token from the file fd.
**    PARAMETERS   
**       INPUT  : 
**          fd		int	file descriptor(must be from ur_popen)
**       OUTPUT :  
**          tokn_str	char*	resultant numeric string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_digit(fd, tokn_str)
int	fd;
char	*tokn_str;
{
	char	c;
	int	rd_stat;

	while (((rd_stat = ur_nuread(fd, &c, 1)) > 0 ) && isdigit(c))
		*tokn_str++ = c;		/* add digits to the string */
	*tokn_str = '\0';			/* end the string */
	if (rd_stat > 0)
		ur_unread(fd, c);			/* put back the non-digit */
} /* get_digit */

