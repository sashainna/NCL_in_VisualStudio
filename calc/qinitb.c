
/*********************************************************************
**    NAME         :  qinit.c
**       CONTAINS:
**    		uq_init_calc_rel
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qinitb.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:54
*********************************************************************/
#include		"usysdef.h"
#include		"udebug.h"
#include		"calcom.h"
#include		"mdrel.h"
#include		"mdcoord.h"

extern UQ_qstb	UQ_symtab[];
extern int		UQ_tbindex;
extern int		uq_calc2flag; 

extern UQ_query		UQ_querytb[];

/*********************************************************************
**    I_FUNCTION     :  uqi_tbreset ()
**       clear the attribute of the symbol table and set the symbol
**       table to empty
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_tbreset ()
{
	register  i, j;

	uu_denter(UU_DTRC,(us,"enter uqi_tbreset"));
	if (uq_calc2flag == UU_FALSE)
	{
	 for (i =0; i<UQ_tbindex; i++)
		{
		 UQ_symtab[i].symbol[0] = '\0';
		 switch (UQ_symtab[i].ttype)
		 {
		  case  UQ_SCALAR :
		  case  UQ_QUERYFUNC :
					UQ_symtab[i].val[0] = 0;
					break;

		  case  UM_CARTESIAN  :
		  case  UM_CYLINDRICAL:
		  case  UM_SPHERICAL  :
		  case  UM_VCARTESIAN  :
		  case  UM_VCYLINDRICAL:
		  case  UM_VSPHERICAL  :
						UQ_symtab[i].val[0] = 0;
						UQ_symtab[i].val[1] = 0;
						UQ_symtab[i].val[2] = 0;
						break;

		  case  UQ_FUNCTION  :
						uu_toolfree (UQ_symtab[i].func);
						UQ_symtab[i].func = NULL;
						break;

		  default  :
						break;
		 }	 /* switch  */
		 UQ_symtab[i].ttype = 0;
		 ur_delete_all(UQ_symtab[i].key);
	 	 UQ_symtab[i].key = 0;
		 UQ_symtab[i].no_func = 0;
		 UQ_symtab[i].func = NULL;
		}	/* for */
/*	 ud_wrwin("\n");*/
	 ul_win_out("\n",0);
	}
	UQ_tbindex = 0;
	j = 0;
	while (UQ_querytb[j].type != UQ_CNONE)
	{
	 UQ_symtab[UQ_tbindex].ttype = UQ_QUERYFUNC;
	 UQ_symtab[UQ_tbindex].val[0] = j;
	 strcpy(UQ_symtab[UQ_tbindex].symbol,UQ_querytb[j++].querynm);
	 UQ_symtab[UQ_tbindex].rel_num = UQ_CALC_REL;
	 UQ_symtab[UQ_tbindex].no_func = 0;
	 UQ_symtab[UQ_tbindex].func = NULL;
	 ur_create_data(&UQ_symtab[UQ_tbindex]);
	 UQ_tbindex++;
	 uu_dprint(UU_DTRC,(us,"tbindex=%d,j=%d",UQ_tbindex,j));
	}
	uu_dprint(UU_DTRC,(us,"tbindex=%d",UQ_tbindex));
	uu_dexit;
}	/* uqi_tbreset */

