/*********************************************************************
**    NAME         :  tigdtdct.c
**       CONTAINS:
**       uig_data_dict()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigdtdct.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:46
*********************************************************************/

#include "usysdef.h"
#include "uminmax.h"
#include "ribase.h"
#include "rireldef.h"
#include "riddldef.h"
#include "udebug.h"
#include "ridctdef.h"
#include "umoveb.h"

/* arrays for the data dictionary */
#include "tigeldct.h"
#ifdef BUILD_32
#include "tigtrdct32.h"
#else
#include "tigtrdct.h"
#endif

/*********************************************************************
**    E_FUNCTION :  uig_data_dict(defind, def_lim, rel_name, rel_type)
**			look up a definition in the system data dictionary
**    PARAMETERS   
**       INPUT  : 
**          def_lim	int	max. fields in the def. (elements in array)
**          rel_name	char*	name of the relation to be found.
**       OUTPUT :  
**          defind[]	struct contains def. one field per element.
**				rel_type	int*	type of relation
**    RETURNS      : number of attributes, 0 not found, <0 error
**							(more fields than def_lim)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_data_dict(def, def_lim, rel_name, rel_type)
char					*rel_name;
struct attr_def	def[];
int					def_lim;
int					*rel_type;
{
	return(ur_dict_lookup(&UR_datadict, def, def_lim, rel_name, rel_type));
}
