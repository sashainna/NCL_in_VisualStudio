/****************************************************
**     NAME       : reupdate.c
**     CONTAINS   :
**        ur_update_entity (e1,e2)
**        ur_update_ent_varlist (e2,vl1,vl2)
**        ur_update_attribut (key,rel_num)
**        ur_get_input_asize (rel_num,list_num,atom_size)
**
**     MODULE NAME AND RELEASE LEVEL
**       reupdate.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:40
**
*****************************************************/

#include "mlabddl.h"
#include "class.h"
#include "udebug.h"
#include "uhep.h"
#include "ribase.h"
#include "ritrnerr.h"
#include "rerrdef.h"
#include "xenv1.h"
#include "nclfc.h"
#include "mfort.h"
#include "mdrel.h"
#include "nccs.h"
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"

extern int UR_verindex;
extern int UR_big_entry;

/***************************************************************************
**    E_FUNCTION     :  ur_update_entity (in_ptr,out_ptr)
**    Update fixed data of entity main routine (for all versions).
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_entity (in_ptr,out_ptr)
struct UR_data *in_ptr, **out_ptr;
{
	int status,inc;
	struct UR_data *inp,*outp,rdat;
	status = inc = 0; 
	inp = in_ptr;
	outp = *out_ptr;
	switch (UR_verindex)
	{
	case 1:   /* version < 8.103   */
		status = ur_update_8103 (inp,outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 2:   /* version < 8.105   */
		status = ur_update_8105 (inp,outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 3:   /* version < 8.201   */
		status = ur_update_8200 (inp,outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
/*
.....All NCL V8.251 updates are handled
.....in NCL V8.4 update routine
.....(only updates 'numptspercv' in surface data
*/
	case 4:   /* version < 8.251   */
/*
		status = ur_update_8250 (inp,outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,sizeof(struct UR_data));
			inp = &rdat;
		}
*/
	case 5:   /* version < 8.400   */
		status = ur_update_8400 (inp,outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 6:   /* version < 8.500   */
		status = ur_update_8500 (inp,outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 7:   /* version < 9.100   */
		status = ur_update_9100 (inp,outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 8:   /* version < 9.200   */
		status = ur_update_9200 (inp,outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 9:   /* version < 9.300   */
		status = ur_update_9300 (inp,outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 10:   /* version < 9.400   */
		status = ur_update_9400 (inp,outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 11:    /*version < 9.405 */
/*
	case 11:    version < 9.500 
		status = ur_update_9500 (inp, outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
*/
	case 12:    /*version < 9.500 */
		
	case 13:    /*version < 9.600 */
		status = ur_update_9600 (inp, outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 14:    /*version < 9.700 */
		status = ur_update_9700 (inp, outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 15:    /*version < 9.900 */
		status = ur_update_9900 (inp, outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 16:    /*version < 10.000 */
		status = ur_update_10000 (inp, outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	case 17:    /*version < 10.100 */
		status = ur_update_10100 (inp, outp);
		if (status == 1)
		{
			inc = 1;
			uu_move_byte(outp,&rdat,UR_big_entry);
			inp = &rdat;
		}
	default:;
	}

	if (inc == 0) *out_ptr = in_ptr;
     
	return(status);
}

/***************************************************************************
**    E_FUNCTION     :  ur_update_ent_varlist (eptr,list,vli_ptr,vlo_ptr)
**    Update var-list data of entity main routine (for all versions).
**    PARAMETERS
**       INPUT  :
**          eptr    - Pointer to input structure.
**          list    - Var-list number to update.
**          vli_ptr - Pointer to input var-list data.
**       OUTPUT :
**          vlo_ptr - Pointer to updated var-list data.
****************************************************************************/

int ur_update_ent_varlist (eptr,list,vli_ptr,vlo_ptr)
struct UR_data *eptr;
int list;
char *vli_ptr, **vlo_ptr; 
{
	int status=0;
	char *vlt;

	*vlo_ptr = vlt = vli_ptr;
	switch (UR_verindex)
	{
		case 1:   /* version < 8.103   */
		case 2:   /* version < 8.105   */
		case 3:   /* version < 8.201   */
			status += ur_update_8200_varl (eptr,list,vlt,vlo_ptr);
			if (vlt != *vlo_ptr)
			{
				uu_free(vlt);
				vlt = *vlo_ptr;
			}
		case 4:   /* version < 8.251   */
		case 5:   /* version < 8.400   */
		case 6:   /* version < 8.500   */
		case 7:   /* version < 9.100   */
		case 8:   /* version < 9.200   */
		case 9:   /* version < 9.300   */
		case 10:  /* version < 9.400   */
		case 11:  /* version < 9.405   */
		case 12:  /* version < 9.500   */
		case 13:  /* version < 9.600   */
		case 14:  /* version < 9.700   */
			status += ur_update_9700_varl (eptr,list,vlt,vlo_ptr);
			if (vlt != *vlo_ptr)
			{
				uu_free(vlt);
				vlt = *vlo_ptr;
			}
		case 15:  /* version < 9.900   */
		case 16:  /* version < 10.00   */
			status += ur_update_10000_varl (eptr,list,vlt,vlo_ptr);
			if (vlt != *vlo_ptr)
			{
				uu_free(vlt);
				vlt = *vlo_ptr;
			}
		default:;
	}

	uu_dexit;
	return(status);
 }

/***************************************************************************
**    E_FUNCTION     :  ur_update_attribut (key,rel_num)
**    Update attribute bundle (for all versions).
**    PARAMETERS
**       INPUT  :
**          key     - entity key.
**          rel_num - relation number of entity.
**       OUTPUT :
**          none
****************************************************************************/
int ur_update_attribut (key,rel_num)
UU_KEY_ID key;
int rel_num;
{
	int status;

	status  = 0; 
	switch (UR_verindex)
	{
		case 1:   /* version < 8.103   */
		case 2:   /* version < 8.105   */
		case 3:   /* version < 8.201   */
		case 4:   /* version < 8.251   */
		case 5:   /* version < 8.400   */
			if (status == 0) status = ur_update_8400_attr (key,rel_num);
		case 6:   /* version < 8.500   */
		case 7:   /* version < 9.100   */
		case 8:   /* version < 9.200   */
		case 9:   /* version < 9.300   */
		case 10:  /* version < 9.400   */
		case 11:  /*version < 9.405    */
		case 12:  /*version < 9.500    */
		case 13:  /*version < 9.600    */
			if (status == 0) status = ur_update_9600_attr (key,rel_num);
		case 14:  /* version < 9.700   */
			if (status == 0) status = ur_update_9700_attr (key,rel_num);

		default:;
	}

	uu_dexit;
	return(UU_SUCCESS);
}

/***************************************************************************
**    E_FUNCTION     :  ur_get_input_asize (rel_num,list_num,atom_size)
**    Get old var-list data atom size (main routine for all versions).
**    PARAMETERS
**       INPUT  :
**          rel_num   - Relation number.
**          list_num  - Var-list number which atom size is required.
**       OUTPUT :
**          atom_size - Var-list atom size.
****************************************************************************/

int ur_get_input_asize (rel_num,list_num,atom_size)
 UR_REL_NUM rel_num;
 int list_num;
 int *atom_size;

 {
  int status, size;
  size   = 0;
  switch (UR_verindex)
    {
     case 1:   /* version < 8.103   */
     case 2:   /* version < 8.105   */
     case 3:   /* version < 8.201   */
          status = ur_up_8200_atomsize (rel_num,list_num,&size);
          break;
     case 4:   /* version < 8.251   */
          break;
     case 5:   /* version < 8.400   */
          break;
     case 6:   /* version < 8.500   */
          break;
     default:;
    }
  *atom_size = size;
  uu_dexit;
  return(0);
 }
