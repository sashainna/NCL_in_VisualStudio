
/*********************************************************************
**    NAME         :  reparse.c
**       CONTAINS:
**       ur_parse
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reparse.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:33
*********************************************************************/

#include "riddldef.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION     :  int ur_parse(fd, defind, def_lim, rel_name, rel_type)
**			ur_parse finds the definition for a given relation name.
**    PARAMETERS   
**       INPUT  : 
**          fd			int	file descriptor returned by ur_popen().
**          def_lim	int	max. fields in the def. (elements in array)
**          rel_name	char*	name of the relation to be found.
**       OUTPUT :  
**          defind[]	struct contains def. one field per element.
**				rel_type	*int	type of relation parsed
**    RETURNS      : number of fields if found, 0 if not found, <0-error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_parse(fd, defind, def_lim, rel_name, rel_type)
char	*rel_name;
struct attr_def	defind[];
int	fd, def_lim;
int	*rel_type;
{
	char	fnd_name[80];
	int	found, qeof;

	uu_denter(UU_RTRC,(us,"ur_parse: finding %s", rel_name));
	found = 0;
	ur_rewind(fd);
	do
	{
		qeof = ur_parse1(fd, defind, def_lim, fnd_name, rel_type);
		if (qeof <= 0)
		{
			uu_dexit;
			return(qeof);	/* not found */
		}
		if (strcmp(rel_name, fnd_name) == 0)
		{
			found = 1;
		}
	} while (!found);
	uu_dexit;
	return (qeof);
}
