/********************************************************************* 
**  NAME: m2einit.c
**
**		CONTAINS:
**				um_reninit()
**				um_is_light_init()
**
**  	COPYRIGHT  1984  UNICAD, Inc.
**  	MODULE NAME AND RELEASE LEVEL 
**       m2einit.c , 25.1
**  	DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:46
**
*********************************************************************/

#include "usysdef.h"
#include "mrender.h"
#include "udebug.h"
#include "uims.h"
#include "dinput.h"
#include "dascom.h"
#include "mattr.h"
#include "mconst.h"
#include "mdattr.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mrenddl.h"
#include "rver9400.h"

#define MAXLIGHT 15

int UM_light_keys[MAXLIGHT];
int UM_material_reset = 1;

int Slight_init = UU_FALSE;

static struct {
	UU_REAL kd;			
	UU_REAL ks;		
	UU_REAL spec_exp;	
} UM_mtrlmdl_default[NMATRLDEFAULTS] =
	{ 
			 1.0,  1.0,   128.0,		/* Constant */
		  	 0.75, 1.0,   0.0,			/* Matte */
		  	 0.75, 0.75,  10.0,			/* Phone */
		  	 0.75, 0.75,  115.0,		/* Plastic */
		  	 0.0,  1.0,   96.0,			/* Metal */
		  	 0.40, 0.40,  10.0,					/* Type 6 */
		  	 0.60, 0.00,  0.0,					/* Type 7 */
		  	 0.00, 0.00,  0.0,					/* Type 8 */
		  	 0.00, 0.00,  0.0,					/* Type 9 */
		  	 0.00, 0.00,  0.0,					/* Type 10 */
		  	 0.00, 0.00,  0.0,					/* Type 11 */
		  	 0.00, 0.00,  0.0,					/* Type 12 */
		  	 0.00, 0.00,  0.0,					/* Type 13 */
		  	 0.00, 1.00,  5.0,					/* Specular reflector */
		  	 1.00, 0.00,  0.0,					/* Diffuse reflector */
		  	 0.00, 0.00,  0.0					/* Ambient reflector */
	};

/*********************************************************************
**    I_FUNCTION     : um_reninit
**       Initializes rendering package
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
void um_reninit()
{
	int i;									/* Loop variable */
	extern int UD_ksws;					/* Open workstation */
	struct UM_attrdata_rec attr;		/* Default light attributes */
	struct UM_light_rec e;				/* Default light data */

	uu_denter(-1,(us,"um_reninit()"));

	/* Set up material table defaults */
	UM_mtrlmdl_old.index = 0;
	for( i=0; i<NMATRLDEFAULTS; i++ ) {
		UM_mtrlmdl_old.kd[i] = UM_mtrlmdl_default[i].kd;
		UM_mtrlmdl_old.ks[i] = UM_mtrlmdl_default[i].ks;
		UM_mtrlmdl_old.spec_exp[i] = UM_mtrlmdl_default[i].spec_exp;
	}
	UM_mtrlmdl.index = 0;
	for( i=0; i<NMATRLDEFAULTS; i++ ) {
		UM_mtrlmdl.ka[i] = 1.0;
		UM_mtrlmdl.kd[i] = UM_mtrlmdl_default[i].kd;
		UM_mtrlmdl.ks[i] = UM_mtrlmdl_default[i].ks;
		UM_mtrlmdl.spec_exp[i] = UM_mtrlmdl_default[i].spec_exp;
		if ((i==0)||(i==1)||(i==4))
		{
			UM_mtrlmdl.ks_r[i] = 0.0;
			UM_mtrlmdl.ks_g[i] = 0.0;
			UM_mtrlmdl.ks_b[i] = 0.0;
		}
		else
		{
			UM_mtrlmdl.ks_r[i] = 1.0;
			UM_mtrlmdl.ks_g[i] = 1.0;
			UM_mtrlmdl.ks_b[i] = 1.0;
		}
		sprintf(UM_mtrlmdl.name[i], "Type %d", i+1);
	}
	UM_material_reset = 1;
	/* Set up default light sources */

	/* Get current default attributes */
	um_current_default_attr(UM_LIGHT_REL, &attr);

	/* Set default light attributes */
	attr.line_style  = UM_SOLID_LINE;
	attr.line_weight = 0.0;
	attr.line_width  = 0.0;
	attr.color       = UM_WHITE;
	attr.selectable  = UU_TRUE;
	attr.displayable = UM_DISPLAYABLE;

	ur_setup_data(UM_LIGHT_REL, &e, sizeof(struct UM_light_rec));

	/* Set default light data */
	e.type = UM_EYE_LIGHT;
	e.space = UM_SCREEN_POS;
	e.exp = 0.0;
	e.ambient[0] = 0.2;
	e.ambient[1] = 0.2;
	e.ambient[2] = 0.2;
	e.ambient[3] = 1.0;
	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 0.0, e.position);
	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 1.0, e.direction);
	e.intens = 100;
	e.attenuation[0] = 0.0;
	e.attenuation[1] = 1.0;
	e.attenuation[2] = 0.0;
	e.cone_angle = 30.0;
/*
.....make the first light visible
*/
	/* Create MAXLIGHT light sources (all invisible) */
	for(i=0; i<MAXLIGHT; i++) {
		e.index = i;
		uc_create_data(&e, UM_DEFAULT_TF, &attr);
		if (i==0)
			ur_update_blanked(e.key, UU_FALSE);
		else
			ur_update_blanked(e.key, UU_TRUE);
		ur_update_data(&e);
		UM_light_keys[i] = e.key;
	}
	Slight_init = UU_TRUE;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     : um_is_light_init()
**       Returns the status of light initialization.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if lights have been initialized,
**                   UU_FALSE otherwise.
**		SIDE EFFECTS : If the secondary unibase is active, then this routine
**                   will always return false.
**    WARNINGS     : none
**
*************************************************************************/
UU_LOGICAL um_is_light_init()
{
/*
.....If the secondary unibase is active, we still need turn on/off light,
.....so don't always return False which is always keep the secondary unibase
.....light off and cause the extern unibase view display objets without light
.....
	if (ur_active_unibase() == 2) return(UU_FALSE);
	else return(Slight_init);
*/
	return(Slight_init);
}
