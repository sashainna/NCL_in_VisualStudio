/*********************************************************************
**    NAME         :  rixmf
**       CONTAINS:
**			ur_set_mtuple_flag
**			ur_clr_mtuple_flag
**			ur_tst_mtuple_flag
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rixmf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:49
*********************************************************************/

#include	"usysdef.h"
#include	"ribase.h"
#include	"riddle.h"

/*********************************************************************
**    I_FUNCTION     :  ur_set_mtuple_flag(key_id,flag_num)
**      set a bit in the master tuple bit table ,
**		 bits are number right to left starting at bit 0,
**		 i.e. - word 0 on a 32 bit machine contains bits 31-0
**    PARAMETERS   
**       INPUT  : 
**			key_id,		key id of the master tuple to set flag in
**			flag_num,	number of the flag to set , 0-31
**       OUTPUT :  
**          none
**    RETURNS      :  0 if flag set, non-zero otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_set_mtuple_flag(key_id,flag_num)
/* argument declarations         */
UU_KEY_ID	key_id	;	/* key_id of master tuple to set flag in	*/
int 				flag_num ;	/* number of flag to set 0-31					*/
{
	/* local  parameter declarations */
	int		status							;
	struct	UR_MTID_rec	*mtuple_ptr	;	
	int		rel_num;
	int		tuple_indx;

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	status = 0 ;
	if(uu_tst_bit(UR_rcb[0].bmap_ptr,key_id-1))
	{
		ur_k2rt(key_id, &rel_num, &tuple_indx);
		ur_get_tuple_ptr(rel_num, tuple_indx, &mtuple_ptr) ;
		uu_set_bit(&mtuple_ptr->bit_tbl,flag_num) ;
	}
	else
	{
		status = -1 ;
	}
	return(status) ;
}

/*********************************************************************
**    I_FUNCTION     :  uu_clr_mtuple_flag(key_id,flag_num)
**      clear a bit in the master tuple
**		 bits are number right to left starting at bit 0,
**		 i.e. - the word on a 32 bit machine contains bits 31-0
**    PARAMETERS   
**       INPUT  : 
**			key_id,		key id of the master tuple to set flag in
**			flag_num,	number of the flag to set , 0-31
**       OUTPUT :  
**          none
**    RETURNS      :  0 if bit cleared, < 0 if inactive tuple
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_clr_mtuple_flag(key_id,flag_num)
/* argument declarations         */
UU_KEY_ID	key_id	;	/* key_id of master tuple to set flag in	*/
int 				flag_num ;	/* number of flag to set 0-31					*/
{
	/* local  parameter declarations */
	int		status							;
	struct	UR_MTID_rec	*mtuple_ptr	;	
	int		rel_num;
	int		tuple_indx;

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	status = 0 ;
	if(uu_tst_bit(UR_rcb[0].bmap_ptr,key_id-1))
	{
		ur_k2rt(key_id, &rel_num, &tuple_indx);
		ur_get_tuple_ptr(rel_num, tuple_indx, &mtuple_ptr) ;
		uu_clr_bit(&mtuple_ptr->bit_tbl,flag_num) ;
	}
	else
	{
		status = -1 ;
	}
	return(status) ;
}

/*********************************************************************
**    E_FUNCTION     :  ur_tst_mtuple_flag(key_id,flag_num)
**      test a flag in a master tuple 
**		 bits are number right to left starting at bit 0,
**		 i.e. - word 0 on a 32 bit machine contains bits 31-0
**    PARAMETERS   
**       INPUT  : 
**			key_id,		key id of the master tuple to test flag in
**			flag_num,	number of the flag to set , 0-31
**       OUTPUT :  
**          none
**    RETURNS      :  non_zero if bit set, zero otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ur_tst_mtuple_flag(key_id,flag_num)
/* argument declarations         */
UU_KEY_ID	key_id	;	/* key_id of master tuple to set flag in	*/
int 				flag_num ;	/* number of flag to set 0-31					*/
{
	/* local  parameter declarations */
	struct	UR_MTID_rec	*mtuple_ptr	;	
	int		rel_num;
	int		tuple_indx;

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	ur_k2rt(key_id, &rel_num, &tuple_indx);
	ur_get_tuple_ptr(rel_num, tuple_indx, &mtuple_ptr) ;
	return(uu_tst_bit(&mtuple_ptr->bit_tbl,flag_num)) ;
}
