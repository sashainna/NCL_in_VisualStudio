#define MODULEDBG 0
/*********************************************************************
**    NAME         :  rigettok.c
**       CONTAINS:
**       ur_get_tokn()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rigettok.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:43
*********************************************************************/

#define TRUE 1
#define FALSE 0
#include "ritokdef.h"
int	UR_tline_num = 1;				/* the current line number */

/*********************************************************************
**    I_FUNCTION     :  int ur_get_tokn(fd, tokn_str)
**       gets the next token from the file fd
**    PARAMETERS   
**       INPUT  : 
**          fd		int	file descriptor(must be from ur_popen)
**       OUTPUT :  
**          tokn_str	char*	resulting token string
**    RETURNS      : token
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_tokn(fd, tokn_str)
int	fd;
char	*tokn_str;
	/* ur_get_tokn is a simple lexical analyzer for uddl.  It finds the next  */
	/*   lexical object in the input stream and returns a token describing */
	/*   it.  The string which constitutes the object is returned in the   */
	/*   argument tokn_str. */
{
	int	rd_stat;
	char	c, c2, *strcpy();
	int	comment;

	/* first skip leading white space */
	do
	{
		comment = FALSE;
		while ( ((rd_stat = ur_nuread(fd, &c, 1)) > 0) &&
					( (c==' ') || (c=='\t') || (c=='\n') || (c=='\r')))
		{
		 if (c == '\n')	UR_tline_num++;
		}
		if (rd_stat <= 0)
		{
			return(EOFTOK);
		}
	
		/* we got something - see if its a comment (treated as white space) */
		if (c == '/')
		{
			rd_stat = ur_nuread(fd, &c2, 1);
			if (rd_stat > 0)
				if (c2 == '*')
				{
					ur_eatcomnt(fd,&UR_tline_num);
					comment = TRUE;
				}
				else
					ur_unread(fd, c2);
			else
			{
				strcpy("/", tokn_str);
				return(SLASH);
			}
		}
	} while (comment);

	/* we've got the beginning of a real token */
	*tokn_str++ = c;		/* save in the string. */
	*tokn_str = '\0';
	if (((c >= 'A') && (c <= 'Z')) || ((c >= 'a') && (c <= 'z')) || (c == '_'))
	{
		ur_get_alnum(fd, tokn_str);
		return(ALPHNUM);
	}
	switch(c)				/* now decide what we have if not alphabetic */
	{
	case '(':
		rd_stat = OPENPAREN;
		break;
	case ')':
		rd_stat = CLOSPAREN;
		break;
	case '[':
		rd_stat = OPENBRAK;
		break;
	case ']':
		rd_stat = CLOSBRAK;
		break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		ur_get_digit(fd, tokn_str);
		rd_stat = NUMB;
		break;
	case ',':
		rd_stat = COMMA;
		break;
	case ';':
		rd_stat = SEMICOLON;
		break;
	case '*':
		rd_stat = ASTERISK;
		break;
	case '#':
		rd_stat = SHARP;
		break;
	default:
		rd_stat = UNKNOWN;
		break;
	}
	return(rd_stat);
}
