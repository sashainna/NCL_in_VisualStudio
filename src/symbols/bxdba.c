/*********************************************************************
**    NAME:  bxdba.c
**       CONTAINS:
**				int ub_init_sym_data()
**				int ub_retrieve_sym(eptr, bagsize)
**				int ub_get_sym_attr(key, attrptr)
**				int ub_get_sym_transf(key, tfmat) 
**				int ub_create_symbol_tuple
**				int ub_create_instance_tuple
**				int ub_msubent_displability
**				int ub_isubent_displability
**				int ub_get_symmaster_by_name
**				int ub_update_sym_attr
**				int ub_set_sym_attr
**				int ubi_update_varlist
**				int ubi_update_app_varlist
**				int ub_retrieve_disp_segid
**				int ub_retrieve_varlist
**				int ubi_retrieve_inst_data_in_msym
**				int ubi_get_master_key_for_inst
**    		int ubi_update_inst_data_in_msym
**				int ubi_update_data_fixed
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bxdba.c , 25.4
**    DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 11:40:31
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "mdrel.h"		/* for relation numbers */
#include "mdattr.h"		/* for definition of colors and UM_DISPLAYABLE*/
#include "class.h"		/* for "UC_" data types */
#include "mattr.h"		/* for point attributes */
#include "mcrv.h"			/* for point definition */
#include "mdcpln.h"		/* for construction plane definition */
#include "mfort.h"
#include "nclfc.h"
#include "umessages.h"	/* for associativity messages */
#include "r1emacro.h"
#include "r1emsgpk.h"	/* for UR_messagePacket */
#include "xfsys1.h" 		/* for UX_libdata_bag */

#define UB_BPGM
#include "bsym.h"			/* declares symbol storage and definitions */
#undef UB_BPGM

#define TRACE UU_FALSE	/* for debugging only */
/*********************************************************************
**    E_FUNCTION :  int ub_init_sym_data()
**       Initialize symbol system data.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS: UU_SUCCESS if (apparently) successful, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_init_sym_data()
{
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us,"ub_init_sym_data()"));

	/*---------- symbol display attributes ---------------------------*/
	/* Note, the following is the initializations of a global structure defined
	 * in "bsym.h"; these initializations are used to set the values in the
	 * attribute bundle for symbols when UNIBASE is called. 
	 */
	/*
	UB_symattr_default_bundle.rel_num = UB_SYMATTR_REL;
	UB_symattr_default_bundle.color = UM_YELLOW;
	UB_symattr_default_bundle.layer = 1;
	UB_symattr_default_bundle.line_style = UM_SOLID_LINE;
	UB_symattr_default_bundle.line_width = 0.0;	/ not being used /
	UB_symattr_default_bundle.pen = 1;
	UB_symattr_default_bundle.displayable = UU_TRUE;
	UB_symattr_default_bundle.selectable = UU_TRUE;
	*/
	if (ub_set_sym_attr(-1, &UB_symattr_default_bundle) != UU_SUCCESS)
		goto failed;

	goto done;
failed: status = UB_FAILURE;	
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION : int ub_retrieve_sym(eptr, bagsize) 
**       This function retrieves symbol data from UNIBASE (either master
**			or instance).
**			Note, subentities are not retrieved here.
**    PARAMETERS   
**       INPUT  : 
**          eptr			pointer to the data bag to retrieve a symbol record.
**								Note, the key field must be filled in.
**				bagsize		size of the bag pointed to by "eptr".
**       OUTPUT :  
**          eptr			pointer to the filled in symbol record data bag.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_retrieve_sym(eptr, bagsize)
	struct UB_symbol_rec *eptr;
	int bagsize;
{
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ub_retrieve_sym(eptr->key:%d,bagsize:%d)", 
													eptr->key, bagsize));

	if (ur_retrieve_app_data(eptr) != 0)
	{
		/* error message is: Error on UNIBASE retrieve of symbol record with 
		 * key=%d (ub_retrieve_sym). */
		uu_uerror1(UB_SYMBOL, 26, eptr->key);
		goto failed; 
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :int ub_get_sym_attr(key, attrptr)
**			This function retrieves a symbol attribute record from UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          key			key of the symbol record for which the attribute record
**								is to be retrieve.
**				attrptr		pointer to the data bag to be filled.
**       OUTPUT :  
**          attrptr		pointer to the filled attribute data bag. 
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_get_sym_attr(key, attrptr)
	UU_KEY_ID key;
	struct UB_symattr_rec *attrptr;
{
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ub_get_sym_attr(key:%d, attrptr:%x)", key, attrptr));
	attrptr->key = key;
	if (ur_retrieve_attr(attrptr) != 0)
	{
		/* error message is: Error on UNIBASE retrieve of symbol attributes, 
		 * symbol key=%d (ub_get_sym_attr). 
		 */
		uu_uerror1(UB_SYMBOL, 27, key);
		goto failed;
	}
	goto success;
failed:status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
success:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION : int ub_get_sym_transf(key, tfmat) 
**       This function retrieves a symbol transformation from UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          key			key of the symbol entity to have its transformation
**								retrieved.
**				tfmat			storage for the transformation.
**       OUTPUT :  
**          tfmat			transformation for the symbol.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_get_sym_transf(key, tfmat)
	UU_KEY_ID key;
	UU_REAL tfmat[4][3];
{
	struct UM_transf_rec transfpacket;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ub_get_sym_transf(key:%d,tfmat:%x)", key, tfmat));
	transfpacket.key = key;
	if (ur_retrieve_transf(&transfpacket) != 0)
	{
		/* error message is: Error on UNIBASE retrieve of symbol transform, 
		 * symbol key=%d (ub_get_sym_transf). 
		 */
		uu_uerror1(UB_SYMBOL, 28, key);
		status = UB_FAILURE;
		goto failed;
	}
	um_tftotf(transfpacket.tfmat, tfmat);
	goto done;

failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int ub_create_symbol_tuple(eptr, tfmat, attrptr)
**       Creates a master symbol tuple in UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          eptr			Pointer to a symbol master with all
**								fields filled in.
**				tfmat			Transformation to associate with the symbol; note,
**								this can be defaulted by passing "UB_DEFAULT_TF"
**								as the value of this parameter.  
**				attrptr		Pointer to the attribute bundle to associate with
**								with this symbol; note, this can be defaulted by
**								passing "UB_CURRENT_ATTR" as the value of this
**								parameter.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_create_symbol_tuple(eptr, tfmat, attrptr)
	struct UB_symbol_rec *eptr;
	UU_REAL tfmat[4][3];
	struct UB_symattr_rec *attrptr;
{
	struct UM_transf_rec transpacket;
	UR_messagePacket msgPacket;
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ub_create_symbol_tuple(eptr:%x,tfmat:%x,attrptr:%x)",
												eptr,tfmat,attrptr));

	if (ur_create_data(eptr) != 0)
	{
		uu_uerror2(UB_SYMBOL, 13, eptr->rel_num,"ub_create_symbol_tuple");
		/* message is: Can not create UNIBASE entity tuple for relation %d
		 * (%s). */
		goto failed;
	}

	/* update attribute bundle */
	if (ub_update_sym_attr(eptr->key, attrptr) != UU_SUCCESS)
		goto failed;

	/* creation successful; set displayability */
	if (eptr->rel_num == UB_SYMBOL_REL) /* then making a master symbol */
	{
		if (ur1_initMsgPacket(&msgPacket, UU_NEVERDISPLAYABLE, 0) != UU_SUCCESS) 
			goto failed;

		if (ur_update_displayable1(eptr->key, UM_NEVERDISPLAYABLE, &msgPacket, 1)
			!= 0)
		{
			uu_uerror3(UB_SYMBOL, 47, eptr->key, eptr->rel_num, 
							"ub_create_symbol_tuple");
			/* error message:Can't set displayability for symbol: %d, relation: %d 
			 * (%s). */
			goto failed;
		}
		if (ub_msubent_displability(eptr) != UU_SUCCESS)
			goto failed;
	}
	if (tfmat != UB_DEFAULT_TF)
	{	/* transformation was given */
		transpacket.key = eptr->key;
		transpacket.rel_num = UM_TRANSFORM_REL;
		um_tftotf(tfmat, transpacket.tfmat);

		if (ur1_initMsgPacket(&msgPacket, UU_UPDATE_TRANSF,
			1 /* nbr data packets */) != UU_SUCCESS) goto failed;
		if (ur1_addData2MsgPacket(&msgPacket,1,tfmat,sizeof(tfmat)) 
			!= UU_SUCCESS) goto failed;

		if (ur_update_transf1(&transpacket, &msgPacket, 1) != 0)
		{
			uu_uerror1(UB_SYMBOL, 14, eptr->key);
			/* message is: Can not store transform for entity that has 
			 * key: %d */
			goto failed;
		}
	}
#if (TRACE)
	ubi_pscroll("from ub_create_symbol_tuple:");
	if (ub_print_sym(eptr) != UU_SUCCESS)
		goto failed;
#endif
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}      

/*********************************************************************
**    E_FUNCTION: int ub_create_instance_tuple(masterKey, eptr, tfmat, attrptr,
**										instdataptr)
**       Creates an instance symbol tuple in UNIBASE.
**    PARAMETERS   
**       INPUT: 
**				masterKey	Key of the master symbol whose instance we are creating.
**          eptr			Pointer to a symbol instance with all
**								fields filled in.
**				tfmat			Transformation to associate with the symbol; note,
**								this can be defaulted by passing "UB_DEFAULT_TF"
**								as the value of this parameter.  
**				attrptr		Pointer to the attribute bundle to associate with
**								with this symbol; note, this can be defaulted by
**								passing "UB_CURRENT_ATTR" as the value of this
**								parameter.
**				instdataptr Pointer to the instance data packet to be put on the
**								master's list. Must have transformation filled before 
**								calling this function.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_create_instance_tuple(masterKey, eptr, tfmat, attrptr, instdataptr)
	UU_KEY_ID masterKey;
	struct UB_instance_rec *eptr;
	UU_REAL tfmat[4][3];
	struct UB_symattr_rec *attrptr;
	struct UB_inst_rec *instdataptr;
{
	UR_messagePacket msgPacket;
	UU_LOGICAL ok;
	int status = UU_SUCCESS;	/* assume success */
	uu_denter(UU_BTRC,(us,
	"ub_create_instance_tuple(mastrKy:%d,eptr:%x,tf:%x,atrptr:%x,instdatptr:%x)",
				 eptr,tfmat,attrptr,instdataptr));

	if (ncl_create_data(eptr) != 0)
	{
		uu_uerror2(UB_SYMBOL, 13, eptr->rel_num, "ub_create_instance_tuple");
		/* message is: Can not create UNIBASE entity tuple for relation %d
		 * (%s). */
		goto failed;
	}

	if (ur1_initMsgPacket(&msgPacket, UU_DISPLAYABLE, 0) != UU_SUCCESS) 
			goto failed;

	UR1_UPDATE_ME(eptr,&msgPacket,1, UU_REC_CHANGE,
		ub_fixNewInstance(eptr,&msgPacket,masterKey,tfmat,attrptr,instdataptr),
		&ok, &status) 
	if (status != UU_SUCCESS)
		goto failed;

#if (TRACE)
	ubi_pscroll("from ub_create_instance_tuple:");
	if (ub_print_sym(eptr) != UU_SUCCESS)
		goto failed;
#endif
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}      
                                                                              
/*********************************************************************
**    E_FUNCTION : int ub_msubent_displability(eptr)
**			This function sets the displability of subentities of master
**			symbols. 
**    PARAMETERS   
**       INPUT  : 
**				eptr		Pointer to the master symbol to have the displayability 
**							of its subentities set.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_msubent_displability(eptr)
	struct UB_symbol_rec *eptr;
{
	UR_messagePacket msgPacket;
	int i, status = UU_SUCCESS;			
	uu_denter(UU_BTRC, (us,"ub_msubent_displayability(eptr->key:%d)",eptr->key));

	/* set displayability of subentity; all subentities are to be 
	 * nondisplayable */
	if (ur1_initMsgPacket(&msgPacket, UU_NEVERDISPLAYABLE, 0) != UU_SUCCESS) 
			goto failed;

	for (i=0; i<eptr->no_geom; i++)
		if (ur_update_displayable1(eptr->geom[i], UM_NEVERDISPLAYABLE, &msgPacket, 1) 
							!= 0) 
		{
			uu_uerror2(UB_SYMBOL, 63, eptr->geom[i], "ub_msubent_displability");
			/* error is: Can't set displayability for subentity with key:%d
			 * (%s). */
			goto failed;
		}
	/* set displayability of snap nodes */
	for (i=0; i<eptr->no_snap_nod; i++)
		if (ur_update_displayable1(eptr->snap_nod[i].snap_key,UM_NEVERDISPLAYABLE,
							&msgPacket,1) != 0) 
		{
			uu_uerror2(UB_SYMBOL, 63, eptr->snap_nod[i].snap_key, 
								"ub_msubent_displability");
			/* error is: Can't set displayability for subentity with key:%d
			 * (%s). */
			goto failed;
		}
	/* set displayability of text nodes */
	for (i=0; i<eptr->no_text_nod; i++)
		if (ur_update_displayable1(eptr->text_nod[i].text_key,UM_NEVERDISPLAYABLE,
								&msgPacket,1) != 0) 
		{
			uu_uerror2(UB_SYMBOL, 63, eptr->text_nod[i].text_key, 
				"ub_msubent_displability");
			/* error is: Can't set displayability for subentity with key:%d
			 * (%s). */
			goto failed;
		}

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION : int ub_isubent_displability(eptr)
**			This function sets the displability of subentities of symbol 
**			instances.
**    PARAMETERS   
**       INPUT  : 
**				eptr		Pointer to the instance to have the displayability of
**							of its subentities set.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_isubent_displability(eptr)
	struct UB_instance_rec *eptr;
{
	UR_messagePacket msgPacket;
	int i, status = UU_SUCCESS;		
	uu_denter(UU_BTRC, (us,"ub_isubent_displability(eptr->key:%d)",eptr->key));

	/* set displayability of subentity; all subentities are to be 
	 * nondisplayable */
	if (ur1_initMsgPacket(&msgPacket, UU_NEVERDISPLAYABLE, 0) != UU_SUCCESS) 
			goto failed;

	for (i=0; i<eptr->no_geom; i++)
		if (ur_update_displayable1(eptr->geom[i], UM_NEVERDISPLAYABLE, 
							&msgPacket, 1) != 0) 
		{
			uu_uerror2(UB_SYMBOL, 63, eptr->geom[i], "ub_isubent_displability");
			/* error is: Can't set displayability for subentity with key:%d
			 * (%s). */
			uu_dprint(UU_BTRC,(us,
			"ERROR: Can't set displayability for geom with key:%d (%s)",
					eptr->geom[i], "ub_isubent_displability"));
			goto failed;
		}
	/* set displayability of snap nodes */
	for (i=0; i<eptr->no_snap_nod; i++)
		if (ur_update_displayable(eptr->snap_nod[i].snap_key,UM_NEVERDISPLAYABLE) 					!= 0) 
		{
			uu_uerror2(UB_SYMBOL, 63, eptr->snap_nod[i].snap_key, 
					"ub_isubent_displability");
			/* error is: Can't set displayability for subentity with key:%d
			 * (%s). */
			uu_dprint(UU_BTRC,(us,
			"ERROR: Can't set displayability for snap node with key:%d (%s)",
					eptr->snap_nod[i].snap_key, "ub_isubent_displability"));
			goto failed;
		}
	/* set displayability of text nodes */
	for (i=0; i<eptr->no_text_nod; i++)
		if (ur_update_displayable(eptr->text_nod[i].text_key, UM_NEVERDISPLAYABLE)
					!= 0) 
		{
			uu_uerror2(UB_SYMBOL, 63, eptr->text_nod[i].text_key, 
				"ub_isubent_displability");
			/* error is: Can't set displayability for subentity with key:%d
			 * (%s). */
			uu_dprint(UU_BTRC,(us,
			"ERROR: Can't set displayability for text node with key:%d (%s)",
					eptr->text_nod[i].text_key, "ub_isubent_displability"));
			goto failed;
		}

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :  ub_get_symmaster_by_name(symptr, foundptr,fcase,flag)
**       This function retrieves the master symbol or instance whose name has
**       been specified in the "name" field of the master symbol record pointed 
**			to by "symptr".
**    PARAMETERS   
**       INPUT  : 
**         symptr    pointer to the master symbol with the name specified.
**         fcase     search is case sensitive when compare symbol name
**         flag      1 = Search symbol masters, 2 = Search symbol instances
**       OUTPUT :  
**         symptr		Pointer to the filled-in master symbol record, if
**							such a master symbol exists; the entire record is returned.
**			  foundptr	pointer to the value indicating whether the master
**							symbol was found or not; UU_TRUE iff it was found.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: Prints error messages. Note, not finding the master symbol
**						  is NOT an error.
**    WARNINGS: none
*********************************************************************/
int ub_get_symmaster_by_name(symptr, foundptr, fcase,flag)
struct UB_symbol_rec *symptr;
UU_LOGICAL *foundptr;
int fcase,flag;
{
	char name[UB_SYMBOL_NAME_LEN_PLUS];
	UU_LOGICAL stillooking;
	int i, nxtuple, nc;
	int status,isub,irel,libstat;
	char slib[256], fullname[256],direc[256];
	UM_f77_str flib;

	uu_denter(UU_BTRC, (us,"ub_get_symmaster_by_name(symptr->label:%s,?)", 
					symptr->label));
	status = UU_SUCCESS;	/* assume success */
	*foundptr = UU_FALSE;	/* haven't found master yet */
	
	UM_init_f77_str(flib,slib,256);
	gcutlb(UM_addr_of_f77_str(flib));
/*
.....Remove trailing spaces
*/
	nc = strlen (slib);
	if (nc>256)
		nc = 256;
	for (i=nc; i>0; i--)
	{
		if (slib[i-1]==' ')
			slib[i-1] = '\0';
		else
			break;
	}
	slib[255] = '\0';
	strcpy(name, symptr->label);	/* save symbol name */
	isub = symptr->subscr;
	nxtuple = 1;
	stillooking = UU_TRUE; /*TRUE iff still looking for the master requested*/
	irel = UB_SYMBOL_REL;
	if (flag == 2) irel = UB_INSTANCE_REL;
	while ((stillooking) 
		&& (ur_get_next_data_key(irel, &nxtuple, &(symptr->key)) == 0))
	{	/* got next master key see if its the right one */
		if (ur_retrieve_data_fixed(symptr) != UU_SUCCESS)
		{
			uu_uerror2(UB_SYMBOL, 24, symptr->key, "ub_get_symmaster_by_name");
			/* massage is: Can not retrieve master symbol data for key: %d
			 * (%s).
			 */
			goto failed;
		}
		/* master symbol record retrieved, see if its the right one */
/*
.....need compare the whole path
*/
		ul_remove_quotes(symptr->path);
		if (symptr->path[0]=='\0')
		{
/*
......there is no path
*/
			if (strcmp(name, symptr->label) == 0 && isub == symptr->subscr)
				stillooking = UU_FALSE;
			else if ((fcase==0) && (ul_compare_upper(name, symptr->label) == 0 &&
				isub == symptr->subscr))
				stillooking = UU_FALSE;
		}
		else
		{
			if (ubi_mk_libname("local",slib, direc, &libstat,UX_NPRTERRS) ==
						 UU_SUCCESS)
			{
				ul_build_full_fname(direc, name,"sy",fullname);
				if (stricmp(fullname, symptr->path)==0)
					stillooking = UU_FALSE;
				else
				{
					if (ubi_mk_libname("system",slib, direc, &libstat,UX_NPRTERRS) ==
								 UU_SUCCESS)
					{
						ul_build_full_fname(direc, name,"sy",fullname);
/*
.....no case
*/
						if (stricmp(fullname, symptr->path)==0)
							stillooking = UU_FALSE;
					}
				}
			}
		}
		nxtuple++;
	}/* end while stillooking */
	if (!stillooking) /* then master symbol is in UNIBASE */
	{
		*foundptr = UU_TRUE;
		if (ub_retrieve_sym(symptr, sizeof(struct UB_symbol_rec)) != UU_SUCCESS)
				goto failed;
	}
	else /* restore original name to "symptr->label" */
	{
		if (flag == 1) symptr->version = -1;
		strcpy(symptr->label, name);
		symptr->subscr = isub;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :  int ub_update_sym_attr(key, attrptr)
**		This function puts the following values into the symbol attribute
**		bundle pointed to by "attrptr": 
**			"key", UB_snap_nodes_visible, UB_text_node_visibility.
**		Then UNIBASE is called store the attribute bundle.
**    PARAMETERS   
**       INPUT  : 
**			key			UNIBASE key to be put in attribute bundle.
**			attrptr		pointer to the symbol attribute bundle to associate
**							with the instance, or, UB_CURRENT_ATTR if the current
**							symbol attribute bundle is to be used.
**       OUTPUT :  
**         attrptr		pointer to the filled-in attribute bundle.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_update_sym_attr(key, attrptr)
	UU_KEY_ID key;
	struct UB_symattr_rec *attrptr;	
{
	int status;

	uu_denter(UU_BTRC,(us,"ub_update_sym_attr(key:%d,%x)",key,attrptr));
	status = UU_SUCCESS;	/* assume success */

	if (attrptr != UB_CURRENT_ATTR)
	{
		attrptr->key = key;

		/* set snap and text node visibility */
		attrptr->see_snod = UB_snap_nodes_visible;
		attrptr->see_tnod = UB_text_node_visibility;

		if (ur_update_attr(attrptr) != 0)
		{
			uu_uerror1(UB_SYMBOL, 15, key);
			/* message is: Can not store attribute bundle for entity that
			 * has key: %d */
			goto failed;
		}
	}
	else /* store default attribute bundle */
	{
		/* set current default attributes */
		if (ub_set_sym_attr(key, &UB_symattr_default_bundle) != UU_SUCCESS)
			goto failed;

		if (ur_update_attr(&UB_symattr_default_bundle) != 0)
		{
			uu_uerror1(UB_SYMBOL, 15, key);
			/* message is: Can not store attribute bundle for entity that
			 * has key: %d. */
			goto failed;
		}
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :  int ub_set_sym_attr(key, attrptr)
**		This function sets the current display attributes for a symbol
**		attribute bundle to the current attributes; note, many of these are
**		modelling attributes.
**    PARAMETERS   
**       INPUT  : 
**				key		Key of entity associated with the attribute bundle.
**				attrptr	Pointer to the symbol attribute bundle to be set.
**       OUTPUT :  
**         attrptr		pointer to the filled-in attribute bundle.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_set_sym_attr(key, attrptr)
	UU_KEY_ID key;
	struct UB_symattr_rec *attrptr;	
{
	int status;
	UM_int2 itype,ilbl;

	uu_denter(UU_BTRC,(us,"ub_set_sym_attr(key:%d,%x)",key,attrptr));
	status = UU_SUCCESS;	/* assume success */
	attrptr->key = key;
	attrptr->rel_num = UB_SYMATTR_REL;
	attrptr->color = ur_get_attrmdl_color() /* UM_dispattr.consclr */;
	attrptr->layer = ur_get_attrmdl_layer();
	attrptr->pen = ur_get_attrmdl_pen();
	attrptr->line_style = ur_get_attrmdl_line_style() /*UM_dispattr.conslnstyl*/;
	attrptr->line_width = ur_get_attrmdl_line_width();
	attrptr->displayable = ur_get_attrmdl_displayable();
	attrptr->selectable = ur_get_attrmdl_selectable();
	itype = 32; lblchk(&itype,&ilbl);
	attrptr->label_on = ilbl;

	/* store visibility of snap and text nodes */
	attrptr->see_snod = UB_snap_nodes_visible;
	attrptr->see_tnod = UB_text_node_visibility;

#if (TRACE)
	if (ub_print_symattr(attrptr) != UU_SUCCESS) 
		goto failed;
#endif

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    I_FUNCTION: ubi_update_varlist(key, listnbr, bufptr, startatom, nbratoms)
**       This function updates a UNIBASE variable list for a symbol (instance
**			or master).
**    PARAMETERS   
**       INPUT: 
**          key			Key of the symbol to have a variable list updated.
**				listnbr		Number of the list to update.
**				bufptr		Pointer to the buffer containing the data with which to
**								update the variable list.
**				startatom	Number of atom with which to start the update (starts
**								with 1).
**				nbratoms		Number of atoms to update.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered; otherwise UB_FAILURE.
**    SIDE EFFECTS : Changes UNIBASE.
**    WARNINGS     : none
*********************************************************************/
int ubi_update_varlist(key, listnbr, bufptr, startatom, nbratoms)
	UU_KEY_ID key;
	int listnbr;
	char *bufptr;
	int startatom;
	int nbratoms;
{
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,
	"ubi_update_varlist(key:%d,listnbr:%d,bufptr:%x,start:%d,nbratoms:%d)",
		key, listnbr, bufptr, startatom, nbratoms));

	if (ur_update_data_varlist(key,listnbr,bufptr,startatom,nbratoms) < 0)
	{
		uu_uerror3(UB_SYMBOL, 82, listnbr, key, "ubi_update_varlist");
		/* error is:Error in updating UNIBASE list, %d, for key, %d, (%s). */
		goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    I_FUNCTION :int ubi_update_app_varlist(symptr, lst_num, bufptr, 
**								start_atom, nbr_atoms)
**		This function updates a variable list for a symbol entity (either
**		a master symbol or an instance).
**    PARAMETERS   
**       INPUT  : 
**          symptr		Pointer to the symbol entity, either a master or an
**								instance.
**				lst_num		Number of the list to update (first list is 1).
**				bufptr		Pointer to the buffer containing the data to update the
**								list.
**				start_atom	Index of the atom in the list to start writing into.
**								(first atom is 1).
**				nbr_atoms	Number of atoms to write.
**       OUTPUT :  none.
**    RETURNS      : UU_SUCCESS if no problems; otherwise, UB_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_update_app_varlist(symptr, lst_num, bufptr, start_atom, nbr_atoms)
	struct UB_symbol_rec *symptr;
	int lst_num;
	char *bufptr;
	int start_atom;
	int nbr_atoms;
{
	int stat;
	int status;			/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, 
	"ubi_update_app_varlist(sym>key:%d,lst:%d,%x,start_atom:%d,nbr_atoms:%d)",
			symptr->key,lst_num,bufptr,start_atom,nbr_atoms));
	status = UU_SUCCESS; /* assume success */
	if ((stat = ur_update_app_data_varlist(symptr,lst_num,bufptr,start_atom,
						nbr_atoms)) != nbr_atoms) 
	{
		uu_dprint(UU_BTRC,(us,"stat=%d", stat));
		uu_dprint(UU_BTRC,(us,"symptr->key:%d", symptr->key));
		uu_uerror3(UB_SYMBOL, 75, lst_num,symptr->key,"ubi_update_app_varlist");
		/* error is:Error in updating application list:%d for key:%d (%s).
		 */
		goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION : int ub_retrieve_disp_segid(key, dsegidptr) 
**			This function retrieves the display segment from UNIBASE and
**			prints an error message if retrieval is unsuccessful.
**    PARAMETERS   
**       INPUT  : 
**				key			Key of the symbol to be retrieved.
**       OUTPUT :  
**				dsegidptr	Pointer to the display segment id.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_retrieve_disp_segid(key, dsegidptr)
	UU_KEY_ID key;
	int *dsegidptr;
{
	int status;			/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ub_retrieve_disp_segid(key:%d, ?)", key));
	status = UU_SUCCESS; /* assume success */
	if (ur_retrieve_disp_segid(key,dsegidptr) != 0)
	{
		uu_uerror2(UB_SYMBOL, 38, key, "ub_retrieve_disp_segid");
		/* error message: unable to retrieve display segment id for key=%d, (%s) 
		 */
		goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION: int ub_retrieve_varlist(symptr, listnbr)
**			This functions retrieves the symbol variable list given by "listnbr".
**    PARAMETERS   
**       INPUT  : 
**				symptr	Pointer to the symbol fixed data with key filled in
**							that is to have a variable list retrieved.
**							Note, only the key need be filled-in.
**				listnbr	Number of the variable list to be retrieved.
**       OUTPUT :  
**				symptr	Pointer to the symbol entity with the variable list
**							requested fill-in.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_retrieve_varlist(symptr, listnbr)
	struct UB_symbol_rec *symptr; /* this need not be a master */
	int listnbr;
{
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us, "ub_retrieve_varlist(symptr>key:%d, listnbr:%d)",
						symptr->key, listnbr));

	if (ur_retrieve_app_data_varlist(symptr, listnbr) != UU_SUCCESS)
	{
		uu_uerror3(UB_SYMBOL, 79, listnbr, symptr->key, "ub_retrieve_varlist");
		/* Can't retrieve list, %d, for entity with key, %d,  (%s). */
		goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    I_FUNCTION: int ubi_retrieve_inst_data_in_msym(instptr, instdataptr,
**									masterkeyptr, indxptr)
**			This functions retrieves the instance data packet contained in the
**			master symbol from which the symbol instance pointed to by , "instptr",
**			was derived.
**    PARAMETERS   
**       INPUT: 
**				instptr			Pointer to the instance entity whose associated 
**									master has the instance data packet to be retrieved.
**       OUTPUT:  
**				instdataptr		Pointer to the retrieved data packet.
**				masterkeyptr	Pointer to the key of the associated master symbol.
**				indxptr			Pointer to the index into the list where the 
**									instance data packet resides; assuming the list is.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_retrieve_inst_data_in_msym(instptr, instdataptr, masterkeyptr, indxptr)
	struct UB_instance_rec *instptr;
	struct UB_inst_rec *instdataptr;
	UU_KEY_ID	*masterkeyptr;
	int	*indxptr;
{
	struct UB_symbol_rec master;
	int i;
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us, 
		"ubi_retrieve_inst_data_in_msym(instptr>key:%d,instdataptr:%x,?,?)",
						instptr->key, instdataptr));

	/* get master associated with instance */
	UB_SETUP_DATA(UB_SYMBOL_REL, &master, sizeof(struct UB_symbol_rec), status);
	if (ubi_get_master_key_for_inst(instptr,&(master.key)) != UU_SUCCESS)
			goto failed;
	/* master.key = instptr->master_key; */
	if (ub_retrieve_varlist(&master, UB_INST_LIST) != UU_SUCCESS)
			goto failed;

	/* find the instance data packet */
	for (i=master.no_inst-1; i>=0; i--) /* do the search backwards */
	{
		uu_dprint(UU_BTRC,
			(us,"master.inst[%d].inst_key:%d",i,master.inst[i].inst_key));
		if (instptr->key == master.inst[i].inst_key)
			break;
	}
	if (i < 0) /* instance not found */
	{
		uu_uerror2(UB_SYMBOL,80,instptr->key,"ubi_retrieve_inst_data_in_msym");
		/* error is:Can't find data packet for instance with key, %d, (%s). */
		goto failed;
	}
	/* otherwise, the instance was found; pass back the requested data */
	*indxptr = i;
	*masterkeyptr = master.key;
	uu_move_byte(&(master.inst[i]), instdataptr, sizeof(struct UB_inst_rec));
	ur_free_app_data(&master);		/* free varlist space */

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    I_FUNCTION: int ubi_get_master_key_for_inst(instanceptr, masterkeyptr) 
**       This function finds the key of the master associated with the instance
**			pointed to by "instanceptr".
**    PARAMETERS   
**       INPUT  : 
**          instanceptr		Pointer to the symbol instance for which the key of
**									the master symbol is desired.
**       OUTPUT :  
**          masterkeyptr	Pointer to the key of the master symbol desired.
**    RETURNS: UU_SUCCESS if no problems encountered; otherwise UB_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_get_master_key_for_inst(instanceptr, masterkeyptr) 
	struct UB_instance_rec *instanceptr;
	UU_KEY_ID *masterkeyptr;
{
	int indx, status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ubi_get_master_key_for_inst(instanceptr>key:%d,?)",
									instanceptr->key));
	indx = 1;
	if (ur_get_next_spec_assoc_key(instanceptr->key,&indx,UB_SYMBOL_REL,0,
			masterkeyptr) != 0) goto failed;
	/* *masterkeyptr = instanceptr->master_key; */
	
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    I_FUNCTION: int ubi_update_inst_data_in_msym(instptr, instdataptr, 
**											masterkey, indx)
**       This function updates (deletes) the instance data for the instance 
**			pointed to by "instptr", from its associated master symbol.
**			If a delete is being done, it is best to delete the instance first.
**    PARAMETERS   
**       INPUT  : 
**          instptr		Pointer to the instance to have its data packet 
**								deleted from the associated master; note, the instance
**								must exist in Unibase.
**				instdataptr	If UU_NULL then the instance data packet in the master
**								master symbol is deleted.  If not UU_NULL then this is
**								a pointer to an instance data packet to replace the 
**								the corresponding one in the associated master symbol.
**				masterkey	If not UB_UNKNOWN then this is the key of the master 
**								symbol associated with the instance.
**				indx			If the master symbol is given in "masterkey" then we
**								assume this has the correct index for locating the 
**								instance data packet in the list (assuming the list
**								is viewed as an array).
**       OUTPUT : none.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : Fixes master symbol records in UNIBASE.
**    WARNINGS     : none
*********************************************************************/
int ubi_update_inst_data_in_msym(instptr, instdataptr, masterkey, indx)
	struct UB_instance_rec *instptr;
	struct UB_inst_rec *instdataptr;
	UU_KEY_ID masterkey;
	int indx;
{
	struct UB_symbol_rec master;
	struct UB_inst_rec dummyinstdata;
	int status = UU_SUCCESS; 
	uu_denter(UU_BTRC,(us,
	"ubi_update_inst_data_in_msym(inst>ky:%d,instdataptr:%x,mastrky:%d,indx:%d)",
						instptr->key,instdataptr,masterkey,indx));
#if (TRACE)
	if (instdataptr == UU_NULL)
		uu_dprint(UU_BTRC,(us,"we are deleting instance data from master"));
	else uu_dprint(UU_BTRC,(us,"we are replacing instance data in master"));
#endif

	master.key = masterkey;
	if (master.key == UB_UNKNOWN) /* then we need to find it */
		if (ubi_retrieve_inst_data_in_msym(instptr,&dummyinstdata,&(master.key),
							&indx) != UU_SUCCESS) goto failed;

	if (instdataptr == UU_NULL) 
	{	/* then we delete this instance's data packet in the master */
		if (ur_delete_varlist_atoms(master.key,UB_INST_LIST,indx+1,1) != 0)
		{
			uu_uerror3(UB_SYMBOL, 81, UB_INST_LIST, master.key, 
							"ubi_update_inst_data_in_msym");
			/* error is: Error in deleting from list, %d, of master sym
			 * with key, %d, (%s). */
			goto failed;
		}
	}
	else /* there is an instance data packet to be updated */
	{
		if (ur_update_data_varlist(master.key,UB_INST_LIST,instdataptr,indx+1,1) 
					< 0)
		{
			uu_uerror3(UB_SYMBOL, 82, UB_INST_LIST, master.key, 
							"ubi_update_inst_data_in_msym"); 
			/* error is: Error in updating list, %d, of master sym with key, %d,
			 * (%s). */
			goto failed;
		}
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    I_FUNCTION: int ubi_update_data_fixed(symptr)
**       This function updates the fixed portion of a symbol (master or
**			instance) record in UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          symptr	Pointer to a symbol record (master or instance)
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_update_data_fixed(symptr)
	struct UB_symbol_rec *symptr;
{
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us, "ubi_update_data_fixed(symptr>key:%d)",symptr->key));
	
	if (ur_update_data_fixed(symptr) != 0)
	{
		uu_uerror2(UB_SYMBOL, 90, symptr->key, "ubi_update_data_fixed");	
		/* error is:Error in updating UNIBASE symbol record with key:%d,  (%s).*/
		goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION: int ub_fixNewInstance(eptr,msgPacketptr,masterKey,
**							tfmat,attrptr,instdataptr)
**			This function fixes the newly created symbol instance tuple;
**			i.e. sets its displayability, its links with it's master symbol,
**			the displayability of the instances subentities (to 
**			UM_NEVERDISPLAYABLE), and updates the instance's transformation
**			in Unibase.
**    PARAMETERS   
**       INPUT  : 
**
**       OUTPUT :  
**				output
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_fixNewInstance(eptr,msgPacketptr,masterKey,tfmat,attrptr,instdataptr)
	struct UB_instance_rec *eptr;
	UR_messagePacket *msgPacketptr;
	UU_KEY_ID masterKey;
	UU_REAL tfmat[4][3];
	struct UB_symattr_rec *attrptr;
	struct UB_inst_rec *instdataptr;
{
	struct UB_symbol_rec master;
	struct UM_transf_rec transpacket;
	UR_messagePacket msgPacket1;
	int status = UU_SUCCESS;	/* assume success */
	uu_denter(UU_BTRC, (us, 
	"ub_fixNewInstance(eptr>ky:%d,msg:%s,mKy:%d,tf,atptr,idatptr)",
		eptr->key,uu_msg(UR1_GET_MSG(msgPacketptr)),masterKey));

	/* update attribute bundle */
	if (ub_update_sym_attr(eptr->key, attrptr) != UU_SUCCESS)
		goto failed;

	if (ur_update_displayable1(eptr->key, UM_DISPLAYABLE, msgPacketptr, 1) != 0)
	{
		uu_uerror3(UB_SYMBOL, 47, eptr->key, eptr->rel_num, "ub_createInstance");
		/* error message:Can't set displayability for symbol: %d, relation: %d 
		 * (%s). */
		goto failed;
	}
	if (ub_isubent_displability(eptr) != UU_SUCCESS)
		goto failed;

	/*** add new instance data packet to its associated master ***/

	instdataptr->inst_key = eptr->key; /* put instance's new key in data packet*/
	UB_SETUP_DATA(UB_SYMBOL_REL, &master, sizeof(struct UB_symbol_rec), status);
	if (masterKey != 0)
	{
		master.key = masterKey;
		if (ub_retrieve_varlist(&master, UB_INST_LIST) != UU_SUCCESS)
			goto failed;  /* uses dynamic storage so we use the one below */

		if (ubi_update_varlist(master.key, UB_INST_LIST, (char *)instdataptr, 
			master.no_inst+1, 1) != UU_SUCCESS) goto failed;
		ur_free_app_data(&master);		/* free varlist space */
	}

	if (tfmat != UB_DEFAULT_TF)
	{	/* transformation was given */
		transpacket.key = eptr->key;
		transpacket.rel_num = UM_TRANSFORM_REL;
		um_tftotf(tfmat, transpacket.tfmat);

		if (ur1_initMsgPacket(&msgPacket1, UU_UPDATE_TRANSF, 1) != UU_SUCCESS)
			goto failed;
		if (ur1_addData2MsgPacket(&msgPacket1, 1, tfmat, sizeof(tfmat))
				!= UU_SUCCESS) goto failed;

		if (ur_update_transf1(&transpacket, &msgPacket1, 1) != 0)
		{
			uu_uerror1(UB_SYMBOL, 14, eptr->key);
			/* message is: Can not store transform for entity that has 
			 * key: %d */
			goto failed;
		}
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexitstatus("ub_fixNewInstance",status);
	return(status);
}

