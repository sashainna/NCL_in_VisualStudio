/*********************************************************************
**    NAME         :  classup.c
**       CONTAINS: class dispatchers
**    	UC_METHOD ucu_validate(table_desc, class_id, method_id)
**    	int ucu_init_root(table_desc)
**    	int ucu_init_class(classrec, table_desc)
**    	int ucu_init_class_rec(row, table_desc)
**    	UC_CANBE_REC_PTR um_init_canbe()
**   		int um_canbe_symbol(canberec)
**    	int um_canbe_group(canberec)
**    	int um_canbe_symgroup(canberec)
**			int uc_build_select_mask(offset, mask)
**			int uc_build1_select_mask(offset, mask)
**    	int uc_print_class_table(table_desc)
**			char *uc_method_lookup(entrypt, table_desc)
**    	int uc_init_method_trace(table_desc, method, name)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       classup.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:59
**
*********************************************************************/

#include "uhep.h"
#include "usysdef.h"
#include "zsysdep.h"
#include "class.h"
#include "canbe.h"
#include "ulist.h"
#include "dasnog.h"
#include "dselmask.h"
#include "mdcoord.h"
#include "mdattr.h"
#include "mdebug.h"
#include "mdpick.h"
#include "xenv1.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor /*, UC_scot_descriptor*/ ;

/*********************************************************************
**
**    E_FUNCTION:  UC_CLASS_REC *ucu_init_class_rec(row, table_desc)
**			This is the initialize function for a class record.
**			It implements inheretance
**
**    PARAMETERS   
**       INPUT: 
**				row = row number (class) to use
**				table_desc = table descriptor of table to use
**       OUTPUT:  
**				none
**
**    RETURNS: pointer to UC_CLASS_REC
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

UC_CLASS_REC *ucu_init_class_rec(row, table_desc)
int row;										/* row (class) to init */
UC_TABLE_DESCRIPTOR *table_desc;		/* descriptor of table to initialize */
{
	int i, superclass;
	static UC_CLASS_REC classrec;

	uu_denter(UU_MTRC, (us, "enter ucu_init_class_rec, row=%d", row));

	if((*(*table_desc).table_ptr[row]).name == row)
	{
		classrec.name = (*(*table_desc).table_ptr[row]).name;
		superclass = (*(*table_desc).table_ptr[row]).super_class;
		classrec.super_class = superclass;
		if(superclass != UC_ROOT_CLASS)
		{
			for(i=0; i<(*table_desc).num_methods; i++)
				classrec.methods[i]=(*(*table_desc).table_ptr[superclass]).methods[i];
		}
		else
		{
			for(i=0; i<(*table_desc).num_methods; i++)
				classrec.methods[i] = UC_UNDEFINED_METHOD;
		}
	}
	else
	{

/*		-- system error recovery, exit system -- */

		uu_sys_err_recovery(1, UU_CLASS, 1, row,
				(*(*table_desc).table_ptr[row]).name);
	}
	uu_dexit;
	return(&classrec);
}	

/*********************************************************************
**
**    E_FUNCTION:  int ucu_init_class(classrec, table_desc)
**			This is the initialize function for a class entry
**
**    PARAMETERS   
**       INPUT: 
**				classrec = filled row to use in initilization
**				table_desc = table descriptor of table to initialize
**       OUTPUT:  
**				none
**
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int ucu_init_class(classrec, table_desc)
UC_CLASS_REC *classrec;					/* filled in class record */
UC_TABLE_DESCRIPTOR *table_desc;		/* descriptor of table to initialize */
{
	int i, row, um_unknown_method();

	uu_denter(UU_MTRC, (us, "enter ucu_init_class, row=%d", (*classrec).name));

	row = (*classrec).name;
	(*(*table_desc).table_ptr[row]).initted = UU_TRUE;
	for(i=0; i<(*table_desc).num_methods; i++)
		(*(*table_desc).table_ptr[row]).methods[i] = (*classrec).methods[i];

	uu_dexit;
	return(UU_SUCCESS);
}	

/*********************************************************************
**
**    E_FUNCTION:  int ucu_init_root(table_desc)
**			This is the initialize function for the root class
**
**    PARAMETERS   
**       INPUT: 
**				table_desc = table descriptor of table to initialize
**       OUTPUT:  
**				none
**
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int ucu_init_root(table_desc)
UC_TABLE_DESCRIPTOR *table_desc;	/* descriptor of table to initialize */
{
	uu_denter(UU_MTRC, (us, "enter ucu_init_root"));

	(*(*table_desc).table_ptr[0]).initted = UU_TRUE;
	(*(*table_desc).table_ptr[0]).super_class = -1;

	uu_dexit;
	return(UU_SUCCESS);
}	

/*********************************************************************
**
**    I_FUNCTION:  UC_METHOD ucu_validate(table_desc, class_id, method_id)
**			This function validates a class dispatch request and
**			returns the function address
**
**    PARAMETERS   
**       INPUT: 
**				table_desc = class dispatch table descriptor
**				class_id = class identifier
**				method_id = method identifier
**       OUTPUT:  
**				none
**
**    RETURNS: function address if no problems encountered,
**					UU_NULL otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

UC_METHOD ucu_validate(table_desc, class_id, method_id)
UC_TABLE_DESCRIPTOR *table_desc;		 /* class dispatch table descriptor */
int class_id;								 /* class identifier */
int method_id;								 /* method identifier */
{

	UC_METHOD function;
	char *uc_method_lookup();

	uu_denter(UU_MITRC,(us, 
		"enter CLASS DISPATCHER, table=%x, class_id=%d, method=%d",
			table_desc, class_id, method_id));

	if((*table_desc).table_ptr[class_id] != UU_NULL)
	{
		uu_dprint(UU_MTRC,(us, "enter CLASS DISPATCHER, table=%s, class=%s",
			(*table_desc).table_name, (*(*table_desc).table_ptr[class_id]).label));

		uu_dprint(UU_MTRC,(us,"dispatch to %s = %s", 
			(*(*table_desc).method_ptr).meth_name[method_id],
			uc_method_lookup((*(*table_desc).table_ptr[class_id]).methods[method_id],
								table_desc)));

/*		-- validate class -- */

		if(class_id<0 || class_id>=(*table_desc).num_classes)
		{
			uu_dprint(UU_MTRC,(us,"ERROR ucu_validate = INVALID CLASS"));
			goto failed;
		}

		if((*(*table_desc).table_ptr[class_id]).initted == UU_FALSE)
		{
			uu_dprint(UU_MTRC,(us,"ERROR ucu_validate = UNINITIALIZED CLASS"));
			goto failed;
		}

/*		-- validate method -- */

		if(method_id<0 || method_id>=(*table_desc).num_methods)
		{
			uu_dprint(UU_MTRC,(us,"ERROR ucu_validate = INVALID METHOD"));
			goto failed;
		}

		function = (*(*table_desc).table_ptr[class_id]).methods[method_id];
		if(function == UC_UNDEFINED_METHOD)
		{
			uu_dprint(UU_MTRC,(us,"ERROR ucu_validate = UNDEFINED METHOD"));
			goto failed;
		}
	}
	else
	{
		uu_dprint(UU_MTRC,(us,"ERROR ucu_validate = UNIMPLEMENTED CLASS"));
		goto failed;
	}

/*		-- request is valid --*/

done:
	uu_dprint(UU_MITRC,(us,"ucu_validate method=%x", function));
	uu_dexit;
	return(function);

failed:
	function = UC_UNDEFINED_METHOD;
	goto done;
}

/*********************************************************************
**
**    E_FUNCTION:  int ucu_success()
**			This function returns success
**
**    PARAMETERS   
**       INPUT: 
**				none
**       OUTPUT:  
**				none
**
**    RETURNS: UU_SUCCESS always
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int ucu_success()
{
	return(UU_SUCCESS);
}

/*********************************************************************
**
**    E_FUNCTION:  int ucu_failure()
**			This function returns failure
**
**    PARAMETERS   
**       INPUT: 
**				none
**       OUTPUT:  
**				none
**
**    RETURNS: UU_FAILURE always
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int ucu_failure()
{
	return(UU_FAILURE);
}

/*********************************************************************
**
**    E_FUNCTION:  UC_CANBE_REC_PTR um_init_canbe()
**			This is the initialize function for the canbe query record
**
**    PARAMETERS   
**       INPUT: 
**				none
**       OUTPUT:  
**				none
**
**    RETURNS: pointer to canbe record
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

UC_CANBE_REC_PTR um_init_canbe()
{
	static UC_CANBE_REC loccanbe;
	int i;

	for(i=0; i<UC_CANBE_SIZE; i++)
		loccanbe[i] = UU_FALSE;
	return(loccanbe);
}

/*********************************************************************
**
**    E_FUNCTION:  int um_canbe_symbol(canberec)
**			This function says that the relation can be in a symbol
**
**    PARAMETERS   
**       INPUT: 
**				canberec = can be record
**       OUTPUT:  
**				none
**
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int um_canbe_symbol(canberec)
UC_CANBE_REC_PTR canberec;
{
	canberec[UC_CANBE_ASYMBOL] = UU_TRUE;			/* can be in a symbol */
	return(UU_SUCCESS);
}

/*********************************************************************
**
**    E_FUNCTION:  int um_canbe_group(canberec)
**			This function says that the relation can be in a group
**
**    PARAMETERS   
**       INPUT: 
**				canberec = can be record
**       OUTPUT:  
**				none
**
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int um_canbe_group(canberec)
UC_CANBE_REC_PTR canberec;
{
	canberec[UC_CANBE_AGROUP] = UU_TRUE;				/* can be in a group */
	return(UU_SUCCESS);
}

/*********************************************************************
**
**    E_FUNCTION:  int um_canbe_symgroup(canberec)
**			This function says that the relation can be in a symbol and a group
**
**    PARAMETERS   
**       INPUT: 
**				canberec = can be record
**       OUTPUT:  
**				none
**
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int um_canbe_symgroup(canberec)
UC_CANBE_REC_PTR canberec;
{
	uu_denter(UU_MTRC, (us, "enter um_canbe_symgroup, canberec=%x", canberec));
	canberec[UC_CANBE_ASYMBOL] = UU_TRUE;			/* can be in a symbol */
	canberec[UC_CANBE_AGROUP] = UU_TRUE;			/* can be in a group */
	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**
**    E_FUNCTION:  int uc_build_select_mask(offset, mask)
**			This function builds a select mask buffer by interrogating
**			each entity's can be method
**
**    PARAMETERS   
**       INPUT: 
**				offset = offset into canbe record
**       OUTPUT:  
**				mask = mask buffer
**
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int uc_build_select_mask(offset, mask)
int offset;					/* offset into canbe record */
int mask[];					/* select mask to return */
{
	int i, rel;
	UC_CANBE_REC_PTR canbeptr, uc_canbe();

	uu_denter(UU_MTRC, (us, "enter uc_build_select_mask, flag=%d", offset));
	for(i=0; i< UD_NMENTWD; i++)
		mask[i] = 0;

	for(rel=1; rel<UM_MAX_RELATION; rel++)
	{
		canbeptr = uc_canbe(rel);
		if(canbeptr[offset] == UU_TRUE)
			UD_set_selmask(rel, mask);
	}

	uu_dprint(UU_MTRC, (us, "leave uc_build_select_mask, mask=%x %x %x %x",
			mask[0], mask[1], mask[2], mask[3]));
	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**
**    E_FUNCTION:  int uc_build1_select_mask(offset, mask)
**			This function builds a select mask buffer by interrogating
**			each entity's method table.
**
**    PARAMETERS   
**       INPUT: 
**				method = method into class record
**       OUTPUT:  
**				mask = mask buffer
**
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int uc_build1_select_mask(method, mask)
int method;					/* method number */
int mask[];					/* select mask to return */
{
	int i, rel;

	uu_denter(UU_MTRC, (us, "enter uc_build1_select_mask, method=%d", method));

	for(i=0; i< UD_NMENTWD; i++)
		mask[i] = 0;

	for(rel=1; rel<UC_cot_descriptor.num_classes; rel++)
	{
		if((*UC_cot_descriptor.table_ptr[rel]).methods[method] != 
													UC_UNDEFINED_METHOD)
			UD_set_selmask(rel, mask);
	}

	uu_dprint(UU_MTRC, (us, "leave uc_build1_select_mask, mask=%x %x %x %x",
			mask[0], mask[1], mask[2], mask[3]));
	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**
**    E_FUNCTION:  int um_canbe_nothing(canberec)
**			This function says that the relation can be nothing
**
**    PARAMETERS   
**       INPUT: 
**				canberec = can be record
**       OUTPUT:  
**				none
**
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int um_canbe_nothing(canberec)
UC_CANBE_REC_PTR canberec;
{
	return(UU_SUCCESS);
}

/*********************************************************************
**
**    E_FUNCTION:  int um_canbe_compsymgroup(canberec)
**			This function says that the relation can be in a composite, a symbol,
**			and a group
**
**    PARAMETERS   
**       INPUT: 
**				canberec = can be record
**       OUTPUT:  
**				none
**
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int um_canbe_compsymgroup(canberec)
UC_CANBE_REC_PTR canberec;
{
	uu_denter(UU_MTRC, (us, "enter um_canbe_symgroup, canberec=%x", canberec));
	canberec[UC_CANBE_ASYMBOL] = UU_TRUE;			/* can be in a symbol */
	canberec[UC_CANBE_AGROUP] = UU_TRUE;			/* can be in a group */
	canberec[UC_CANBE_ACOMP] = UU_TRUE;				/* can be in a composite */
	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**
**    E_FUNCTION:  int uc_print_class_table(table_desc)
**			This function prints out the class tables in a legible form
**
**    PARAMETERS   
**       INPUT: 
**       OUTPUT:  
**				none
**
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int uc_print_class_table(table_desc)
UC_TABLE_DESCRIPTOR *table_desc;		/* descriptor of table to initialize */
{
	int i, j, k;
	char *nameptr, *uc_method_lookup();
	char namebuf[100];
	UC_CLASS_REC *class_ptr;

	uu_denter(UU_MTRC, (us, "enter uc_print_class_table()"));

	uu_dprint(UU_MTRC,(us," "));
	uu_dprint(UU_MTRC,(us," "));
	uu_dprint(UU_MTRC,(us," "));
	uu_dprint(UU_MTRC,(us,"           TABLE NAME = %s",
					(*table_desc).table_name));


	for(i=0; i<(*table_desc).num_classes; i++)
	{
		uu_dprint(UU_MTRC,(us," "));
		uu_dprint(UU_MTRC,(us," "));
		uu_dprint(UU_MTRC,(us," "));

/*		-- print out some class stuff -- */

		class_ptr = (*table_desc).table_ptr[i];

		if(class_ptr != UU_NULL)
		{
			if((*class_ptr).initted == UU_TRUE)
			{
				nameptr = uc_method_lookup((*class_ptr).initialize, table_desc);
				uu_dprint(UU_MTRC,(us,"%s class is %d, init is %s", 
							(*(*table_desc).table_ptr[i]).label, i, nameptr));

				uu_dprint(UU_MTRC,(us," "));

				namebuf[0] = '\0';
				nameptr = (*class_ptr).label;
				k = (*(*table_desc).table_ptr[i]).super_class;
				if(k > -1)
				{
					do
					{
						strcat(namebuf, ", ");
						strcat(namebuf, (*(*table_desc).table_ptr[k]).label);
						k = (*(*table_desc).table_ptr[k]).super_class;
					} while (k > -1);
				}

				uu_dprint(UU_MTRC,(us," super classes are %s%s", nameptr, namebuf));
				uu_dprint(UU_MTRC,(us," "));

				for(j=0; j<(*table_desc).num_methods; j++)
				{
					nameptr = uc_method_lookup((*class_ptr).methods[j], table_desc);
					uu_dprint(UU_MTRC,(us,"     %s = %s", 
											(*(*table_desc).method_ptr).meth_name[j], 
											nameptr));
				}
			}
			else
			{
				uu_dprint(UU_MTRC,(us,"          %d class is not initialized", i));
			}
		}
		else
		{
			uu_dprint(UU_MTRC,(us,"          %d class is not used", i));
		}
		uu_dprint(UU_MTRC,(us," "));
	}
	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**
**    I_FUNCTION:  char *uc_method_lookup(entrypt, table_desc)
**			This function prints out the class tables in a legible form
**
**    PARAMETERS   
**       INPUT: 
**				entrypt = address to look up
**				table_desc = table descriptor 
**       OUTPUT:  
**				none
**
**    RETURNS: pointer to procedure name
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

char *uc_method_lookup(entrypt, table_desc)
int (*entrypt)();								/* address to look up */
UC_TABLE_DESCRIPTOR *table_desc;			/* table to look at */
{
	static char addbuf[20];
	static char *undef_meth = " UC_UNDEFINED_METHOD";
	int i = 0;
	UC_METHOD_TABLE_DESCRIPTOR *method_desc;
	UC_METHOD_TABLE *method_ptr;

	if(entrypt == UC_UNDEFINED_METHOD)
		return(undef_meth);

	method_ptr = (*table_desc).method_ptr;
	method_desc = (*method_ptr).method_desc;
	if(method_desc != UU_NULL)
	{
		i = UZ_ADDRESS_HASH(entrypt, (*method_ptr).meth_ptr_num);
		while(method_desc[i].intry != UU_NULL)
		{
			if(entrypt == method_desc[i].intry)
				return(method_desc[i].name);
			else
			{
				i++;
				if(i >= (*method_ptr).meth_ptr_num)
					i = 0;
			}
		}
	}

	sprintf(addbuf, "x'%x'", entrypt);
	return(addbuf);
}

/*********************************************************************
**
**    E_FUNCTION:  int uc_init_method_trace(table_desc, entrypt, name)
**			This function inserts new entries in the method tables
**
**    PARAMETERS   
**       INPUT: 
**				table_desc = table descriptor record
**				entrypt = address to look up
**				name = name of entry point
**       OUTPUT:  
**				none
**
**    RETURNS: pointer to procedure name
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int uc_init_method_trace(table_desc, entrypt, name)
UC_TABLE_DESCRIPTOR *table_desc;		/* descriptor of table to initialize */
int (*entrypt)();
char *name;
{
	char *ptr, *ux_getenv();
	int i=0;
	int num_match;
	int size;
	int status = UU_SUCCESS;
	UC_METHOD_TABLE *method_ptr;

	uu_denter(UU_MTRC, (us, "enter uc_init_method_trace(), name=%s, add=%x",
			name, entrypt));

	method_ptr = (*table_desc).method_ptr;

/* -- malloc space for method entry buffer if this is the 
		first use of it  -- */

	if((*method_ptr).method_desc == UU_NULL)
	{
		size = (*method_ptr).meth_ptr_num;
		(*method_ptr).meth_ptr_left = size - 1;

		(*method_ptr).method_desc = 
				(UC_METHOD_TABLE_DESCRIPTOR *)
				uu_toolmalloc(size * sizeof(UC_METHOD_TABLE_DESCRIPTOR));

		for(i=0; i<(*method_ptr).meth_ptr_num; i++)
			(*method_ptr).method_desc[i].intry = UU_NULL;
	} 

	if((*method_ptr).meth_ptr_left > 0)
	{
		(*method_ptr).meth_ptr_left--;

		i = UZ_ADDRESS_HASH(entrypt, (*method_ptr).meth_ptr_num);
		while((*method_ptr).method_desc[i].intry != UU_NULL)
		{
			if((*method_ptr).method_desc[i].intry == entrypt)
				goto done;

			i++;
			if(i >= (*method_ptr).meth_ptr_num)
				i = 0;
		}

		(*method_ptr).method_desc[i].intry = entrypt;
		strncpy((*method_ptr).method_desc[i].name, name, UC_LABEL_SIZE);
		(*method_ptr).method_desc[i].name[UC_LABEL_SIZE] = '\0';
	}
	else
	{
		status = UU_FAILURE;
		uu_dprint(UU_MTRC,(us,"FAILURE uc_init_method_trace, out of space"));
	}

done:
	uu_dprint(UU_MTRC,(us,"leave uc_init_method_trace, status=%d", status));
	uu_dexit;
	return(status);
}
