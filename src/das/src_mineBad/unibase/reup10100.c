/****************************************************************
**     NAME       : reup10100.c
**     CONTAINS   :
**        ur_update_10100()
**        ur_update_10100_unistat()
**
**-------------------------  IMPORTANT NOTE  ---------------------
**
**  Be sure to change all standard Unibase structure names to ...
**
**              'xxxver'
**
**  ... in THIS FILE when updating to the next version.  The newly
**  created file should still have the correct structure names
**  (NCL_surface_rec) just in case the structure changes during the
**  release cycle.
**
**  Where:  xxx = symbolic abbreviation for entity, for example,
**                'NCL_surface_rec' becomes 'surf'.
**
**          ver = Version number that THIS FILE was created to update
**                the Unibase TO.  For example '94'.
**
**  This would create the structure name '93'.  These new structure names
**  will be added to the NEW include file created for the next version's update
**  routine, for example 'rver9400.h'.  This new include file will of course
**  have to be added to THIS FILE also.
**
**  The reason for this is that all of the routines used to use the actual
**  structure name (ex. NCL_surface_rec) to update to, even though the
**  structure was changed from release to release.  Needless to say this
**  caused the structure to be updated incorrectly (especially since it would
**  not be updated multiple releases due to the old logic in
**  'ur_update_entity').
**
**-------------------------  IMPORTANT NOTE  ---------------------
**
**     MODULE NAME AND RELEASE LEVEL
**       reup10100.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:38
**
*****************************************************************/
#include "mlabddl.h"
#include "class.h"
#include "udebug.h"
#include "uhep.h"
#include "ribase.h"
#include "ritrnerr.h"
#include "rerrdef.h"
#include "mdrel.h"
#include "atext.h"
#include "bsym.h"

#include "msrf.h"
#include "nccs.h"
#include "msol.h"
#include "ncl.h"
#include "mdcoord.h"

#include "dtypes.h"
#include "nclver.h"

#include "ribase.h"
#include "mcrv.h"
#include "rver10100.h"
/*
#define V99MAIN
#include "rver9900.h"
#undef V99MAIN
*/

#define ATMAX 64

static int S_up_1010_compcrv_fxd();
static int S_up_1010_solid_fxd();

/***************************************************************************
**    E_FUNCTION     :  ur_update_10000 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 10.100). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_10100 (in_ptr,out_ptr)
struct UR_data *in_ptr, *out_ptr;
{
	int status;
	status = 0; 
	switch (in_ptr->rel_num)
	{
		case UM_COMPCRV_REL:
			status = S_up_1010_compcrv_fxd(in_ptr,out_ptr);
			break;
		case UM_SOLID_REL:
			status = S_up_1010_solid_fxd(in_ptr,out_ptr);
			break;
	}
	return(status);
}

/***************************************************************************
**    I_FUNCTION     :  S_up_1010_compcrv_fxd (e1,e2)
**    Update fixed data NCL compcrv entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL compcrv structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL compcrv structure.
**
****************************************************************************/
static int S_up_1010_compcrv_fxd (e1,e2)
struct UM_compcrv_rec100 *e1;
struct UM_compcrv_rec *e2;
{
/*
.....Copy old fields of curve
*/
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy (e2->label,e1->label);
	um_vctovc (e1->labloc,e2->labloc);
	um_vctovc (e1->ldrloc,e2->ldrloc);
	e2->subscr = e1->subscr;
	e2->closdinu = e1->closdinu;
	e2->arclen = e1->arclen;
	e2->planar = e1->planar;
	e2->open = e1->open;
	e2->continuity = e1->continuity;
	e2->fcolor = e1->fcolor;
	e2->no_cid = e1->no_cid;
	e2->no_displst = e1->no_displst;
/*
.....Set new fileds of curve
*/
	e2->t0 = 0.;
	e2->t1 = 1.;
	e2->addflg = 0;
	return(1);
}

/***************************************************************************
**    I_FUNCTION     :  S_up_1010_solid_fxd (e1,e2)
**    Update fixed data NCL solid entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL solid structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL solid structure.
**
****************************************************************************/
static int S_up_1010_solid_fxd (e1,e2)
struct UM_solid_rec100 *e1;
struct UM_solid_rec *e2;
{
/*
.....Copy old fields of solid
*/
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy (e2->label,e1->label);
	um_vctovc (e1->labloc,e2->labloc);
	um_vctovc (e1->ldrloc,e2->ldrloc);
	e2->subscr = e1->subscr;
	e2->type = e1->type;
	e2->no_sdata = e1->no_sdata;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
/*
.....Set new fileds of solid
*/
	e2->closed = 1;
	e2->no_netkey = 0;
	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_update_10100_unistat()
**       Update Unibase Statistics record (Unibase ver < 10.100). 
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
****************************************************************************/
void ur_update_10100_unistat()
{
/*
.....Store Unibase Statistics record
*/
	ur_init_unibase_stat(UR_STAT_UPDATE);
}

