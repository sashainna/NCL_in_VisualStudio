#define MODULEDBG 0
/*********************************************************************
**    NAME         :  rieatcom.c
**       CONTAINS:
**       ur_eatcomnt()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rieatcom.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:42
*********************************************************************/

#define TRUE 1
#define FALSE 0
#include "ritokdef.h"

/*********************************************************************
**    I_FUNCTION     :  int ur_eatcomnt(fd,UR_tline_num)
**       consumes the remains of a comment from the file fd.
**    PARAMETERS   
**       INPUT  : 
**          fd		int	file descriptor returned by ur_popen().
**       OUTPUT :  
**				UR_tline_num	int	the current line number
**    RETURNS      : 0 if successful, else -2(unexpected end of input)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_eatcomnt(fd,UR_tline_num)
int	fd;
int	*UR_tline_num;

{
	int	comment;
	int	rd_stat;
	char	c, c2;

	comment = TRUE;
	do
	{
		while ( ((rd_stat = ur_nuread(fd, &c, 1)) > 0) && (c != '*'))
		{
			if (c=='\n')	(*UR_tline_num)++;
		}
		if (rd_stat <= 0)
		{
			return(-2);		/* This should be symbolic????? */
		}
	
		/* we got something - see if its end comment */
		rd_stat = ur_nuread(fd, &c2, 1);
		if (rd_stat > 0)
			if (c2 == '/')
			{
				comment = FALSE;
			}
			else
				ur_unread(fd, c2);
		else
		{
			return(-2);		/* This one too!????? */;
		}
	} while (comment);
	return(0);
}
