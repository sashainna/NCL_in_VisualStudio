#define MODULEDBG 0
/*********************************************************************
**    NAME         :  rigetaln.c
**       CONTAINS:
**       ur_get_alnum()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rigetaln.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:43
*********************************************************************/

#define isdigit(c) (((c)>='0')&&((c)<='9'))
#define isalpha(c) ((((c)>='a')&&((c)<='z'))||(((c)>='A')&&((c)<='Z')))


/*********************************************************************
**    I_FUNCTION     :  int ur_get_alnum(fd, tokn_str)
**       finish reading an alphanumeric token from the file fd
**    PARAMETERS   
**       INPUT  : 
**          fd		int	file descriptor(must be from ur_popen)
**       OUTPUT :  
**          tokn_str	char*	resulting alphanumeric string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_alnum(fd, tokn_str)
int	fd;
char	*tokn_str;
{
	char	c;
	int	rd_stat;

	while ((rd_stat = ur_nuread(fd, &c, 1)) > 0 )
	{
		if ( (!isdigit(c)) && (!isalpha(c)) && (c != '_') && (c != '.'))
			break;					/* end loop if not alphanumeric */
		*(tokn_str++) = c;		/* add alphanumerics to the string */
	}
	*tokn_str = '\0';			/* end the string */
	if (rd_stat > 0)
		ur_unread(fd, c);			/* put back the non-alphanumeric char */
} /* get_alnum */
