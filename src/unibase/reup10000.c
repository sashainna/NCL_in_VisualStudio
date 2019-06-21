/****************************************************************
**     NAME       : reup10000.c
**     CONTAINS   :
**        ur_update_10000()
**        ur_update_10000_varl()
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
**       reup10000.c , 25.1
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
#include "ncl.h"
#include "mdcoord.h"

#include "dtypes.h"
#include "nclver.h"
/*
#define V99MAIN
#include "rver9900.h"
#undef V99MAIN
*/
#include "rver10100.h"

#define ATMAX 64

static int S_up_1000_varptr();

/***************************************************************************
**    E_FUNCTION     :  ur_update_10000 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 10.100). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_10000 (in_ptr,out_ptr)
struct UR_data *in_ptr, *out_ptr;
{
	int status;
	status = 0; 
	switch (in_ptr->rel_num)
	{
		default:
			status = S_up_1000_varptr(in_ptr,out_ptr);
			break;
	}
	return(status);
}

/***************************************************************************
**    E_FUNCTION     :  S_up_1000_varptr (e1,e2)
**    Update fixed data rbsplsrf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  rbsplsrf structure.
**       OUTPUT :
**          e2   - pointer to updated output  rbsplsrf structure.
**
****************************************************************************/
static int S_up_1000_varptr (e1,e2)
struct UR_data *e1,*e2;
{
	int i,nattr1,nattr2,rel_typ;
	char *ptr;
	struct UR_lpacket_99 *lptr1;
	struct UR_lpacket *lptr2;
	struct attr_def atdef1[ATMAX],atdef2[ATMAX];
/*
.....Get the structure layout for this Unibase entity
*/
	nattr1 = ur_data_dict_o(atdef1,ATMAX,UR_rcb[e1->rel_num].relname,
		&rel_typ,13,UU_TRUE);
	nattr2 = ur_data_dict_o(atdef2,ATMAX,UR_rcb[e1->rel_num].relname,
		&rel_typ,14,UU_FALSE);
/*	nattr2 = UR_rcb[e1->rel_num].rel_def->rellen;
	atdef2 = UR_rcb[e1->rel_num].relattr;*/
/*
.....Convert the Join structures
.....to the new format
.....
.....Old: (int nent; char *ptr)
.....New: (char *ptr; int nent; int padding)
*/
	uu_move_byte(e1,e2,UR_rcb[e1->rel_num].tuple_size);
	for (i=0;i<nattr1;i++)
	{
		if (atdef1[i].attr_type == JOIN || atdef1[i].attr_type == STRING)
		{
			
			ptr = (char *)e1 + atdef1[i].attr_here;
			lptr1 = (struct UR_lpacket_99 *)ptr;
			ptr = (char *)e2 + atdef2[i].attr_here;
			lptr2 = (struct UR_lpacket *)ptr;
			lptr2->atom_cnt = lptr1->atom_cnt;
		}
	}
	return(1);
}
/***************************************************************************
**    E_FUNCTION     :  ur_update_10000_varl (eptr,list,vli_ptr,vlo_ptr)
**    Update variable list data of entity (Unibase ver < 10.00). 
**
**       WARNING: The 'update_varl' routines are all called after all
**                versions of 'updated_fxd' routines, so the output
**                structures should be the same as the current version
**                of NCL.
**    PARAMETERS
**       INPUT  :
**          eptr     - Pointer to input structure (updated already).
**          list     - Var-list number to update.
**          vli_ptr  - Pointer to input variable list data (#list).
**       OUTPUT :
**          vli_ptr  - Pointer to updated list data.
****************************************************************************/
int ur_update_10000_varl (eptr,list,vli_ptr,vlo_ptr)
struct UR_data *eptr;
int list;
char *vli_ptr, **vlo_ptr;
{
	int status,ndisp;
	status = 0;

	switch (eptr->rel_num)
	{
	case NCL_TRIMSF_REL:
		if (list == 2) /* update display list to new format */
		{
			struct NCL_trimsf_rec100 *sf1;
			sf1 = (struct NCL_trimsf_rec100 *) eptr;
			ndisp =  sf1->no_displst;

			status = ur_up_1000_displst (ndisp,vli_ptr,vlo_ptr);
		}
		break;
	}

	uu_dexit;
	return(status);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_1000_displst (ndisp,dti,vlo)
**    Update display list in a trim surface entity (Unibase ver < 10.00). 
**    Note that this routine relies on the curves being stored in the order
**    of uv-curve followed by boundary curves in the display list.  If the
**    order for storage changes this routine will need to be updated to
**    match.
**    PARAMETERS
**       INPUT  :
**          ndisp   - number of stored UU_REAL numbers
**          dti     - pointer to input var-list.
**       OUTPUT :
**          vlo     - pointer to pointer of updated output var-list.
****************************************************************************/
int ur_up_1000_displst (ndisp,dti,vlo)
int ndisp;
UM_coord *dti;
char **vlo;
{
	int i,ninds,cur_ind,cur_key,new_ind,n;

	if (ndisp == 0) return(0);
/*
.....The new format for the display list is "xxyy" where xx = 100 times
.....the index of which key it is (i.e. = 0 uv_key and > 0 implies an
.....inner boundary key) and yy = the curve component number.
*/
	*vlo = (char *)dti;
	cur_ind = 1;
	ninds = dti[0][0];
	dti++;
/*
.....In old unibase files the keys stored in the display list have the
.....format 100*key + component number so we can tell when a new curve
.....is found by checking the prefix stored in cur_key.
*/
	cur_key = (dti[0][1])/100;

	for (i=0;i<ninds;i++)
	{
		n = dti[0][0];
		if (((int)dti[0][1])/100 != cur_key) 
		{
			cur_key = (dti[0][1])/100;
			cur_ind++;
		}
		new_ind = 100*cur_ind + ((int)dti[0][1])%100;
		dti[0][1] = (UU_REAL)new_ind;
		dti += n + 1;
	}
	return(1);
}
