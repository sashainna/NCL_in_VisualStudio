#define MODULEDBG 0
/*********************************************************************
**    NAME         :  reunread.c
**       CONTAINS:
**       mapfd() ur_unread() ur_nuread() ur_popen() ur_pclos() rewind()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reunread.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:38
*********************************************************************/
#include "usysdef.h"
#if MODULEDBG != 0
#include "udebug.h"
#endif
#include "reddlerr.h"

#if (UU_COMP == UU_WIN2K)
#include <io.h>
#define write _write
#define read _read
#define close _close
#define open _open
#define creat _creat
#define lseek _lseek
#endif

int	UR_filmp[3] = {-1, -1, -1};	/* file descriptor to unred map */
int	UR_unred[3] = {-1, -1, -1};	/* place to push back a char per file */

/*********************************************************************
**    I_FUNCTION     :  int mapfd(fd)
**       maps a file descriptor to an internal index
**    PARAMETERS   
**       INPUT  : 
**          fd		int	file descriptor(must be from ur_popen)
**       OUTPUT :  
**          none
**    RETURNS      : internal index
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static mapfd(fd)
int	fd;
{
	int	index;

	for (index = 0; (index < 3) && (UR_filmp[index] != fd); index++)
		;
	if (index < 3)
		return(index);
	else
		return(UR_FNO);	/* not open for unread */
} /* mapfd */

/*********************************************************************
**    I_FUNCTION     :  int ur_unread(fd, c)
**       allows a single char to be "pushed back" onto the input stream
**    PARAMETERS   
**       INPUT  : 
**          fd		int	file descriptor(must be from ur_popen)
**          c		char	character to be pushed back
**       OUTPUT :  
**          none
**    RETURNS      : 1 if ok, error code(<0) otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_unread(fd, c)
/* routine pushes back a char onto the input stream */
int	fd;
char	c;
{
	int	ifd;	/* internal fd */
	
	ifd = mapfd(fd);	/* find our index */
	if (ifd < 0)
	{
		return(ifd);	/* return any error */
	}
	if (UR_unred[ifd] == -1)
		UR_unred[ifd] = c;
	else
	{
		return(UR_TMC);	/* Pushed back too many chars. */
	}
	return(1);
} /* ur_unread */

/*********************************************************************
**    I_FUNCTION     :  int ur_nuread(fd, pc, numchrs)
**       read characters including "push back" if any
**    PARAMETERS   
**       INPUT  : 
**          fd		int	file descriptor(must be from ur_popen)
**          numchrs	int	number of characters to read
**       OUTPUT :  
**          pc		char*	buffer for the characters read
**    RETURNS      : number of chars read, 0 for EOF, <0 for errors
**    SIDE EFFECTS : clears "pushed back" character if any
**    WARNINGS     : none
*********************************************************************/

ur_nuread(fd, pc, numchrs)
int	fd, numchrs;
char	*pc;
{
	int	ifd;	/* internal fd */
	
	ifd = mapfd(fd);	/* find our index */
	if (ifd < 0)
	{
		return(ifd);	/* return any error*/
	}
	if (UR_unred[ifd] != -1)
	{
		*pc++ = UR_unred[ifd];
		UR_unred[ifd] = -1;
		if (numchrs > 1)
		{
			return(read(fd, pc, numchrs-1));
		}
	}
	else
	{
		return(read(fd, pc, numchrs));
	}
	return(1);
} /* ur_nuread */

/*********************************************************************
**    E_FUNCTION     :  int ur_popen(name, mode)
**       open for "push back read"
**    PARAMETERS   
**       INPUT  : 
**          name	char*	file name string
**          mode	int	open mode - just like regular open
**       OUTPUT :  
**          none
**    RETURNS      : file descriptor, <0 for errors
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_popen(name, mode)
char	*name;
int	mode;
{
	int	ifd;	/* internal fd */

#if MODULEDBG != 0
	uu_denter(UU_RTRC,(us,"ur_popen"));
#endif
	ifd = mapfd(-1);	/* find unused */
	if (ifd < 0)
	{
#if MODULEDBG != 0
		uu_dexit;
#endif
		return(UR_TMF);	/* too many parse files */
	}
	UR_filmp[ifd] = open(name, mode);
#if MODULEDBG != 0
	uu_dexit;
#endif
	return(UR_filmp[ifd]);
}

/*********************************************************************
**    E_FUNCTION     :  int ur_pclos(fd)
**       close "push back" read files
**    PARAMETERS   
**       INPUT  : 
**          fd		int	file descriptor
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_pclos(fd)
int	fd;
{
	int	ifd;

#if MODULEDBG != 0
	uu_denter(UU_RTRC,(us,"ur_pclos"));
#endif
	ifd = mapfd(fd);
	if (ifd >= 0)
		UR_filmp[ifd] = -1;	/* mark closed */
	close(fd);
#if MODULEDBG != 0
	uu_dexit;
#endif
}

/*********************************************************************
**    I_FUNCTION     :  int ur_rewind(fd)
**       position "push back" read files at the begining
**    PARAMETERS   
**       INPUT  : 
**          fd		int	file descriptor
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : positions file
**    WARNINGS     : none
*********************************************************************/

ur_rewind(fd)
int	fd;
{
	int	ifd;
	long	lseek();

#if MODULEDBG != 0
	uu_denter(UU_RTRC,(us,"ur_rewind"));
#endif
	ifd = mapfd(fd);
	if (ifd >= 0)
		UR_unred[ifd] = -1;	/* no chars pushed back */
	lseek(fd, 0L, 0);
#if MODULEDBG != 0
	uu_dexit;
#endif
}

