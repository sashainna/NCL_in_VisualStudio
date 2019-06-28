/*********************************************************************
**
**    NAME         :  axhform.c
**
**    CONTAINS:		ua_xh_form()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       axhform.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:43
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) axhform.c 21.1 12/10/09 17:55:49 single"};
#else
static char uu_sccsident[]={"@(#) axhform.c 21.1 12/10/09 17:55:49 double"};
#endif

#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "axhatch.h"
#include "mdrel.h"
#include "mattr.h"

#define NO_DEFAULT 0
#define DEFAULT 1

/*********************************************************************
**
**    E_FUNCTION        :  ua_xh_form(name)
**
**		puts up xhatch attribute form and retrieves form data
**
**    PARAMETERS   
**
**       INPUT  :	char *name - name of form  
**
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : sets new values for xhatch attributes
**
**    WARNINGS     : none
**
*********************************************************************/
ua_xh_form(formfile)
char	*formfile;
{
	static int xh_material, xh_color, xh_linesty, xh_pen;
	static UU_REAL  xh_ang, xh_scal;

	static int *ans[] = {&xh_material, (int *)&xh_ang, (int *)&xh_scal,
								&xh_color, &xh_linesty, &xh_pen};
	int status;

	extern UA_xh_attr_rec UA_xh_defaults;

	uu_denter(UU_STRC,(us,"entering ua_xh_form"));

	xh_material = UA_xh_defaults.material[0];
	xh_ang = UA_xh_defaults.xh_angle;
	xh_scal = UA_xh_defaults.xh_scale;
	xh_color = UA_xh_defaults.color[0];
	xh_linesty = UA_xh_defaults.linestyle[0];
	xh_pen = UA_xh_defaults.pen;

	status = ud_form(formfile, ans, ans);
	if (status == -1) return (status);

	UA_xh_defaults.xh_angle = xh_ang;
	UA_xh_defaults.xh_scale = xh_scal;
	UA_xh_defaults.pen = xh_pen;
	UA_xh_defaults.material[0] = xh_material;
	UA_xh_defaults.color[0] = xh_color;
	UA_xh_defaults.linestyle[0] = xh_linesty;

	uu_dexit;
}
