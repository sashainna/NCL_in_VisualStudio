/*********************************************************************
**    NAME: bdebug.c
**       CONTAINS:
**    		int ub_print_symattr(attrptr)
**    		int ub_print_sym(symptr)
**    		int ubi_print_text_list
**				int ubi_print_snap_list
**    		int ubi_print_list
**    		ubi_pscroll(str)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       bdebug.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:03
*********************************************************************/
#include "usysdef.h"
#include "ustdio.h"
#include "udebug.h"
#include "usysg.h"
#include "mdrel.h"		/* for UB_SYMBOL_REL */
#include "bsym.h"

#define TRACE UU_FALSE	/* for debugging only */
/*********************************************************************
**    E_FUNCTION :  int ub_print_symattr(attrptr)
**			This function prints the symbol attribute bundle pointed to by
**			"attrptr".
**    PARAMETERS   
**       INPUT  : 
**			attrptr		pointer to the symbol attribute bundle to be printed.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_print_symattr(attrptr)
	struct UB_symattr_rec *attrptr;	
{
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ub_print_symattr(%x)",attrptr));

	ubi_pscroll("SYMBOL ATTRIBUTES");
	sprintf(UB_sbuf, "	key:%d, rel_num:%d",attrptr->key, attrptr->rel_num);
	ubi_pscroll(UB_sbuf);
	sprintf(UB_sbuf, "	color:%d, layer:%d, pen:%d", attrptr->color,
			attrptr->layer, attrptr->pen); 
	ubi_pscroll(UB_sbuf);
	sprintf(UB_sbuf, "	line style:%d, line width: %g, displayable: %d",
				attrptr->line_style, attrptr->line_width, attrptr->displayable);
	ubi_pscroll(UB_sbuf);
	sprintf(UB_sbuf, "	selectable:%d", attrptr->selectable);
	ubi_pscroll(UB_sbuf);

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION : int ub_print_sym(symptr)
**       Prints symbol record.
**    PARAMETERS   
**       INPUT  : 
**         symptr			Pointer to the symbol to be printed.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_print_sym(symptr)
	struct UB_symbol_rec *symptr;
{
	char *format;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ub_print_sym(symptr->key:%d)", symptr->key));
	if (symptr->rel_num == UB_SYMBOL_REL) /* print master symbol record */
	{
		sprintf(UB_sbuf,"SYMBOL MASTER:%d, NAME:%s, PATH:%s",
						symptr->key, symptr->label,symptr->path); 
		ubi_pscroll(UB_sbuf);
		sprintf(UB_sbuf,"	version:%d, no_masters:%d, no_inst:%d", 
					symptr->version,symptr->no_masters,symptr->no_inst);
		ubi_pscroll(UB_sbuf);
		sprintf(UB_sbuf,"	no_geom:%d, no_text_nod:%d, no_snap_nod:%d",
					symptr->no_geom,symptr->no_text_nod, symptr->no_snap_nod);
		ubi_pscroll(UB_sbuf);
		/* print geometry keys heading only */

		/* print geometry list */
		if (symptr->geom == UU_NULL)
			ubi_pscroll("		geometry list is null");
		else
		{
			sprintf(UB_sbuf,"		geometry keys:");
			ubi_pscroll(UB_sbuf);
			ubi_print_list(symptr->geom,symptr->no_geom,"%d",sizeof(UU_KEY_ID), 7);
		}

		/* print text node list */
		if (symptr->text_nod == UU_NULL)
			ubi_pscroll("		text node list is null");
		else
		{
			sprintf(UB_sbuf,"		text nodes:");
			ubi_pscroll(UB_sbuf);
			ubi_print_text_list(symptr->text_nod, symptr->no_text_nod, 1);
		}

		/* print snap node list */
		if (symptr->snap_nod == UU_NULL)
			ubi_pscroll("		snap node list is null");
		else
		{
			sprintf(UB_sbuf,"		snap nodes:");
			ubi_pscroll(UB_sbuf);
			ubi_print_snap_list(symptr->snap_nod, symptr->no_snap_nod, 7);
		}
	}
	else /* we have an instance record to print */
	{
		struct UB_instance_rec *instptr;
		struct UB_symbol_rec master;
		UU_KEY_ID *connListptr;
		int nbrConnectors;

		instptr = (struct UB_instance_rec *) symptr;
		if (ubi_get_master_key_for_inst(instptr, &(master.key)) != UU_SUCCESS)
					goto failed;
		if (ur_retrieve_data_fixed(&master) != UU_SUCCESS) goto failed;
		sprintf(UB_sbuf,
			"SYMBOL INSTANCE:%d, NAME:%s,no_geom:%d,no_text_nod:%d,no_snap_nod:%d",
			instptr->key, master.label, instptr->no_geom, instptr->no_text_nod,
			instptr->no_snap_nod);
		ubi_pscroll(UB_sbuf);
		/* print geometry keys heading only */
		sprintf(UB_sbuf,"		geometry keys:");
		ubi_pscroll(UB_sbuf);

		/* print geometry list */
		ubi_print_list
			(instptr->geom, instptr->no_geom, "%d", sizeof(UU_KEY_ID), 7);

		/* print text node list */
		sprintf(UB_sbuf,"		number text nodes: %d", instptr->no_text_nod);
		ubi_pscroll(UB_sbuf);
		ubi_print_text_list(instptr->text_nod, instptr->no_text_nod, 1);

		/* print snap node list */
		sprintf(UB_sbuf,"		number snap nodes: %d", instptr->no_snap_nod);
		ubi_pscroll(UB_sbuf);
		ubi_print_snap_list(instptr->snap_nod, instptr->no_snap_nod, 7);

		/* print associated connector keys heading only */
		sprintf(UB_sbuf,"		connector keys:");
		ubi_pscroll(UB_sbuf);

		/* print associated connector list */
		if (ur_get_app_spec_assoc_list(instptr->key,UB_CONECTOR_REL,
				UB_CON_INST_FLD, &nbrConnectors, &connListptr) != UU_SUCCESS)
					goto failed;
		ubi_print_list
			(connListptr, nbrConnectors, "%d", sizeof(UU_KEY_ID), 7);
	}
	goto done;
failed:;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ubi_print_text_list(list, nbronlist, nbrperline)
**       This functions prints an array of records.
**    PARAMETERS   
**       INPUT  : 
**          list			array of symbol text records to be printed.
**				nbronlist	number of elements in the array.
**				nbrperline	number of records per line.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_print_text_list(list, nbronlist, nbrperline)
	struct UB_text_nod_rec *list;
	int nbronlist;
	int nbrperline;
{
	int i, j, nxt;
	int status = UU_SUCCESS; /* assume success */
	uu_denter(UU_BTRC,(us,
		"ubi_print_text_list(list:%x,nbronlist:%d,nbrperline:%d)",
		list, nbronlist, nbrperline));

	nxt = 0;
	while (nxt < nbronlist)
	{
		for (i=0, j=nxt; (i<nbrperline && j<nbronlist); i++, j++)
		{
			sprintf(UB_sbuf, "			prompt:%d, visibility:%d, key:%d",
					list[j].prompt,list[j].visibility,list[j].text_key);
			ubi_pscroll(UB_sbuf);
		}
		nxt = nxt + nbrperline;
	}
	UB_IF_FAILURE_PRINT_IT
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ubi_print_snap_list(list, nbronlist, nbrperline)
**       This functions prints an array of records.
**    PARAMETERS   
**       INPUT  : 
**          list			array of symbol snap node records to be printed.
**				nbronlist	number of elements in the array.
**				nbrperline	number of records per line.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_print_snap_list(list, nbronlist, nbrperline)
	struct UB_snap_nod_rec *list;
	int nbronlist;
	int nbrperline;
{
	int i, j, nxt;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
		"ubi_print_snap_list(list:%x,nbronlist:%d,nbrperline:%d)",
		list, nbronlist, nbrperline));
	nxt = 0;
	while (nxt < nbronlist)
	{
		for (i=0, j=nxt; (i<nbrperline && j<nbronlist); i++, j++)
		{
			sprintf(UB_sbuf, "			key:%d, nbr:%d",
							list[j].snap_key,list[j].nbr);
			ubi_pscroll(UB_sbuf);
		}
		nxt = nxt + nbrperline;
	}
	UB_IF_FAILURE_PRINT_IT
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ubi_print_list
**								(list, nbronlist, format, eltsize, nbrperline)
**       This functions prints an array of records.
**    PARAMETERS   
**       INPUT  : 
**          list			array to be printed.
**				nbronlist	number of elements in the array.
**				format		C format for printing the array elements.
**								%d, %g, %f, %s, %c only.
**				eltsize		size of an element.
**				nbrperline	number of records per line.
**       OUTPUT :  none.
**    RETURNS: none
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_print_list(list, nbronlist, format, eltsize, nbrperline)
	char *list;
	int nbronlist;		/* number keys on list */
	char *format;		/* format of an element on the list */
	int eltsize;
	int nbrperline;	/* number keys wanted per line */
{
	char s1[50];
	int i, j, nxt;
	int status = UU_SUCCESS;	/* assume success */
	uu_denter(UU_BTRC,(us,"ubi_print_list(list:%x,%d,format:%s,eltsize:%d,%d)",
						list,nbronlist,format,eltsize,nbrperline));

	nxt = 0;
	while (nxt < nbronlist)
	{
		sprintf(UB_sbuf,"					\0");
		for (i=0, j=nxt*eltsize; 
				(i<nbrperline && j<nbronlist*eltsize); i++, j+=eltsize)
		{
			if (strcmp(format, "%d") == 0)
			{
				int *intptr;
				intptr = (int *)&(list[j]);
				sprintf(s1, "%d ", *intptr);
			}
			else if ((strcmp(format, "%g") == 0) || (strcmp(format, "%f") == 0))
			{
				UU_REAL *rptr;
				rptr = (UU_REAL *)&(list[j]);
				sprintf(s1, "%g ", *rptr);
			}
			else
				sprintf(s1, format, list[j]);
				
			um_concat(UB_sbuf, UB_sbuf, s1);
		}
		ubi_pscroll(UB_sbuf);
		nxt = nxt + nbrperline;
	}
	UB_IF_FAILURE_PRINT_IT
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION: ubi_pscroll(str)
**       Prints the string, "str", on the auxiliary terminal and to the 
**			both the trace file and the file, "psfile".
**    PARAMETERS   
**       INPUT: 
**          str            string to be printed.
**       OUTPUT:  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : prints to a terminal and to a file.
**    WARNINGS     : none
*********************************************************************/
ubi_pscroll(str)
	char *str;
{
	/*
     gmessage(D_ksws,str);
	*/ 
	uu_denter((UU_BTRC | UU_BITRC),(us,"%s",str));

	/* if there is something to print out to the "psfile", a file
	for modeling debugging, then pass it to modeling routine */

	if (UU_BTRC & UU_debmask)
	{
		um_pscroll1(UU_BTRC,str);
	}
	else
	{
		if (UU_BITRC & UU_debmask)
		{
			um_pscroll1(UU_BITRC,str);
		}
	}
	uu_dexit;
}
