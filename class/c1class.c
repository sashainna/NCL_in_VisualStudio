/*********************************************************************
**    NAME         :  c1class.c
**       CONTAINS: class dispatchers
**			int uc_retrieve_data(eptr, entsize)
**			int uc_retrieve_transf(key, tfmat)
**			int uc_retrieve_attr(key, attrptr)
**			int uc_create_data(eptr, tfmat, attrptr)
**			int uc_setup(rel_num, eptr, size)
**			int uc_display(eptr)
**			int uc_draw(eptr, tfmat, attr)
**			int uc_print(eptr)
**			int uc_copy(e1ptr, e2ptr, entsize)
**			int uc_ploc_to_coord(level,pickpath,pickloc,pt)
**			int uc_ploc_to_vector(level,pickpath,pickloc,vec)
**			int uc_near_on_entity(key, pickloc, pt)
**			int uc_feature(eptr, tfmat, feature, dploc)
**			int uc_proj_to_drawing(eptr, tfmat, attrptr,
**					tfmat, vrefpt, vpnorm, keylist, render_solids)
**			int uc_draft_type(rel_num)
**			int uc_draft_line(rel_num)
**			int uc_draft_arc(rel_num)
**			int uc_draft_conic(rel_num)
**			int uc_draft_endpts(rel_num)
**			int uc_deleteLast()
**			int uc_query(key, data, rows, maxCols, lines)
**			int uc_getKeyList(initialize, key, funct, added2listptr, listptr)
**			int uc_processKeyList(list, funct)
**			int uc_deleteExceptions(key, add2listptr)
**			int uc_canbe_undeleted(key, undeleteok);
**			int uc_canQuerySubentity(key, candoptr)
**			int uc_deleteIfNecessary(key)
**    	int uc_init_relations()
**			int uc_autoUpdate
**    	int uc_super_class(rel_num)
**			UC_CANBE_REC_PTR uc_canbe(relation)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       c1class.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:56
**
*********************************************************************/
#include "class.h"  /* includes needed usysdef.h and mdrel.h */
#include "canbe.h"
#include "mdpick.h"	/* includes needed dasnog.h */
#include "mdcoord.h" /* included by cstblist,mcrv,mdcpln,mdeval,misect,tzdcoord */
#include "mdattr.h"  /* UM_DISPLAYABLE ... bsym,dselect,tzbsym */
#include "r1esched.h"
#include "udebug.h"
#include "uhep.h"	/* define indexes for iunihep() stuff */
#include "ulist.h"	
#include "nccs.h"	

	extern UC_TABLE_DESCRIPTOR UC_root_descriptor;
	extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;
	extern UC_TABLE_DESCRIPTOR *UC_3d_descriptor[];	

/*********************************************************************
**    E_FUNCTION :  UC_CANBE_REC_PTR uc_canbe(relation)
**       This function returns the record of inquiry for this relation
**    PARAMETERS   
**       INPUT  : 
**          relation			relation number to query
**       OUTPUT :  
**				none
**    RETURNS      : ptr to record of what this relation can be
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UC_CANBE_REC_PTR uc_canbe(relation)
int relation;						/* relation number to query */
	{
	UC_METHOD function, ucu_validate();
	UC_CANBE_REC_PTR canberec, um_init_canbe();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC,(us,"uc_query_symbol(relation=%d)", relation));

	canberec = um_init_canbe();
	class = CONVERT_REL_TO_CLASS(relation);
	function = ucu_validate(&UC_cot_descriptor, class, UC_CANBE);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(canberec);
		}

	uu_dexitstatus("uc_canbe", status);
	return(canberec);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_retrieve_data(eptr, entsize)
**       This function retrieves entity data from UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          eptr				pointer to the storage in which to retrieve
**									entity data.
**				entsize			number of bytes of storage for entity retrieval.
**       OUTPUT :  
**          eptr				pointer to the entity data to be output.
**    RETURNS      : UU_SUCCESS ifno problems encountered. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_retrieve_data(eptr, entsize)
struct UC_entitydatabag *eptr;
int entsize;
	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;

	uu_denter(UU_MTRC,(us,"uc_retrieve_data((*eptr).key:%d,entsize:%d)",
						(*eptr).key, entsize));
	if(um_retrieve_data_relnum((*eptr).key, &(*eptr).rel_num) != UU_SUCCESS)
			goto done;
	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_RETRIEVE_DATA);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(eptr, entsize);
		}

done:;
	uu_dexitstatus("uc_retrieve_data", status);
	return(status);
	}
 
/*********************************************************************
**    E_FUNCTION :  int uc_retrieve_transf(key, tfmat)
**       This function retrieves entity data from UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**				key				key of the entity for which a transformation is
**									to be retrieved.
**       OUTPUT :  
**          tfmat				retrieved transformation.
**    RETURNS      : UU_SUCCESS ifno problems encountered. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_retrieve_transf(key, tfmat)
UU_KEY_ID key;
UU_REAL tfmat[4][3];
	{
	UC_METHOD function, ucu_validate();
	int rel_num;
	int status;
	int class;

	uu_denter(UU_MTRC,(us,"uc_retrieve_transf(key:%d,tfmat:%x)", key, tfmat));
	status = um_retrieve_data_relnum(key, &rel_num);
	if(status == 0)
		{
		class = CONVERT_REL_TO_CLASS(rel_num);
		function = ucu_validate(&UC_cot_descriptor, class, UC_RETRIEVE_TRANSF);
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
			status = (*function)(key, tfmat);
		}
	uu_dexitstatus("uc_retrieve_transf", status);
	return(status);
	}
 
/*********************************************************************
**    E_FUNCTION :  int uc_retrieve_attr(key, attrptr)
**       This function retrieves entity data from UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**				key				key of the entity for which a attribute bundle is
**									to be retrieved.
**       OUTPUT :  
**          attrptr			pointer to the retrieved attribute bundle.
**    RETURNS      : UU_SUCCESS ifno problems encountered. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_retrieve_attr(key, attrptr)
UU_KEY_ID key;
struct UC_attributedatabag *attrptr;
	{
	UC_METHOD function, ucu_validate();
	int rel_num;
	int status;
	int class;

	uu_denter(UU_MTRC,(us,"uc_retrieve_attr(key:%d,attrptr:%x)", key, attrptr));
	status = um_retrieve_data_relnum(key, &rel_num);
	class = CONVERT_REL_TO_CLASS(rel_num);
	if(status == 0)
		{
		function = ucu_validate(&UC_cot_descriptor, class, UC_RETRIEVE_ATTR);
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
			status = (*function)(key, attrptr);
		}
	uu_dexitstatus("uc_retrieve_attr", status);
	return(status);
	}
 
/*********************************************************************
**    E_FUNCTION :  int uc_create_data(eptr, tfmat, attrptr)
**       This function creates an entity (EPTR) in UNIBASE and 
**			associates the specified transformation (TFMAT) and 
**			attribute bundle (ATTRPTR) with it.
**    PARAMETERS   
**       INPUT  : 
**				eptr				pointer to entity data
**				tfmat				transformation matrix
**				attrptr			pointer to attribute bundle 
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS ifno problems encountered. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_create_data(eptr, tfmat, attrptr)
struct UC_entitydatabag *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;

	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;

	uu_denter(UU_MTRC,(us,"uc_create_data(eptr>relnum:%d, tfmat=%x, attrptr:%x)",
		(*eptr).rel_num, tfmat, attrptr));
	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_CREATE_DATA);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(eptr, tfmat, attrptr);
	uu_dexitstatus("uc_create_data", status);
	return(status);
	}
 
/*********************************************************************
**    E_FUNCTION :  int uc_setup(rel_num, eptr, size)
**       This function initializes an entitydatabag (EPTR) of a
**			specified SIZE and relation (REL_NUM).
**    PARAMETERS   
**       INPUT  : 
**				rel_num			relation number
**				eptr				pointer to entity data
**				int				databag size
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS if no problems encountered. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_setup_data(rel_num, eptr, size)
int rel_num;
struct UC_entitydatabag *eptr;
int size;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;

	uu_denter(UU_MTRC,(us,"uc_setup_data(relnum:%d, eptr=%x, size:%d)",
		rel_num, eptr, size));

	class = rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_SETUP_DATA);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(rel_num, eptr, size);

	uu_dexitstatus("uc_setup_data", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_display(eptr)
**       Depending upon the class of the given entity (EPTR), call
**			a routine to display the entity.
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to an entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_display(eptr)
struct UC_entitydatabag *eptr;
	{
	UC_METHOD function, ucu_validate();
	int status = UU_SUCCESS;
	int class;
	int displayable;

	uu_denter(UU_MTRC,(us,"uc_display(key=%d,rel_num=%d)",(*eptr).key,
		(*eptr).rel_num));
	ur_retrieve_displayable((*eptr).key, &displayable);
	if(displayable != UM_DISPLAYABLE) 
		{
		uu_dprint(UU_MTRC,(us,"entity not UM_DISPLAYABLE"));
		goto done;
		}
	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_DISPLAY);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(eptr);
done:
	uu_dexitstatus("uc_display", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_draw(eptr, tfmat, attr)
**       Depending upon the class of the given entity (EPTR), call
**			a routine to draw (i.e. stroke) the entity with the
**			specified transformation and attributes
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to an entity
**				tfmat					transformation matrix
**				attr					display attributes
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_draw(eptr, tfmat, attr)
struct UC_entitydatabag *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attr;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;

	uu_denter(UU_MTRC,(us,"uc_draw(key=%d,rel_num=%d,tfmat=%x,attr=%x)",
		(*eptr).key,(*eptr).rel_num,tfmat,attr));
	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_DRAW);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(eptr,tfmat,attr);

	/* NCL: added to display labels if label field is on - roberta */
	/* MILLS: add check to test for 'dummy' entities - eptr-> == 0 */
	if ((status == UU_SUCCESS) && (eptr->key != 0))
	   status = ncl_draw_unilabel(eptr, tfmat);

	uu_dexitstatus("uc_draw", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_print(eptr)
**			Dispatch to the appropriate routine to print the data
**			associated with the given record. 
**    PARAMETERS   
**       INPUT  : 
**				eptr		pointer to an entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_print(eptr)
struct UC_entitydatabag  *eptr;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;

	uu_denter(UU_MTRC,(us,"uc_print(key:%d, rel=%d)",
		(*eptr).key,(*eptr).rel_num));
	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_PRINT);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(eptr);
	uu_dexitstatus("uc_print", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_copy(e1ptr, e2ptr, entsize)
**      Copy an entity.
**    PARAMETERS   
**       INPUT  : 
**				e1ptr    pointer to entity to be copied
**				entsize	size of the data bags pointed to by e1 and e2.
**       OUTPUT :  
**				e2ptr    pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_copy(e1ptr, e2ptr, entsize)
struct UC_entitydatabag *e1ptr;
struct UC_entitydatabag *e2ptr;
int entsize;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;

	uu_denter(UU_MTRC, (us,"uc_copy(%8x,%8x,%d)",e1ptr,e2ptr,entsize));
	class = (*e1ptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_COPY);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(e1ptr, e2ptr, entsize);
	uu_dexitstatus("uc_copy", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION    : int uc_ploc_to_coord(level,pickpath,pickloc,pt)
**			Determine a cartesian coordinate (PT) given a picked location
**			on an entity (LEVEL, PICKPATH, PICKLOC).
**    PARAMETERS   
**       INPUT  : 
**				level					level of entity picked within pickpath
**				pickpath				DAS pick path record
**				pickloc				DAS pickloc record
**       OUTPUT :  
**				pt						cartesian coordinate (MCS) corresponding
**										to picked location on entity
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_ploc_to_coord(level, pickpath, pickloc, pt)
int level;
UD_PPICKREC *pickpath;
UD_NDCLOCREC *pickloc;
UM_coord pt;
	{
	UC_METHOD function, ucu_validate();
	UU_KEY_ID um_get_pickkey();
	UM_PICKENT pent;
	UU_KEY_ID key;
	int rel_num;
	int class;
	int status;

	uu_denter(UU_MTRC,(us,"uc_ploc_to_coord(level=%d,pickpath=%x,pickloc=%x)",
		level, pickpath, pickloc));

	status = um_d_pickresolve(pickpath, level, &pent);
	if(status == 0)
		{
		key = um_get_pickkey(&pent, 1);
		um_retrieve_data_relnum(key, &rel_num);
		class = CONVERT_REL_TO_CLASS(rel_num);
		function = ucu_validate(&UC_cot_descriptor, class, UC_PLOC_TO_COORD);	
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
			status = (*function)(level, pickpath, pickloc, pt);
		}
	uu_dexitstatus("uc_ploc_to_coord", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION    : int uc_ploc_to_vector(level,pickpath,pickloc,vec)
**			Determine a vector (VEC) given a picked location
**			on an entity (LEVEL, PICKPATH, PICKLOC).
**    PARAMETERS   
**       INPUT  : 
**				level					level of entity picked within pickpath
**				pickpath				DAS pick path
**				pickloc				DAS pickloc record
**       OUTPUT :  
**				vec					vector
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_ploc_to_vector(level, pickpath, pickloc, vec)
int level;
UD_PPICKREC *pickpath;
UD_NDCLOCREC *pickloc;
UM_vector vec;
	{
	UC_METHOD function, ucu_validate();
	UU_KEY_ID um_get_pickkey();
	UM_PICKENT pent;
	UU_KEY_ID key;
	int rel_num;
	int class;
	int status;

	uu_denter(UU_MTRC,(us,"uc_ploc_to_vector(level=%d,pickpath=%x,pickloc=%x)",
		level, pickpath, pickloc));

	status = um_d_pickresolve(pickpath, level, &pent);
	if(status == 0)
		{
		key = um_get_pickkey(&pent, 1);
		um_retrieve_data_relnum(key, &rel_num);
		class = CONVERT_REL_TO_CLASS(rel_num);
		function = ucu_validate(&UC_cot_descriptor, class, UC_PLOC_TO_VECTOR);
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
			status = (*function)(level, pickpath, pickloc, vec);
		}
	uu_dexitstatus("uc_ploc_to_vector", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION    : int uc_near_on_entity(key,pickloc,pt)
**			Retrieve the entity data and transformation using the
**			given UNIBASE KEY, and determine the nearest point (PT)
**			on the entity to the picked location (PICKLOC).
**    PARAMETERS   
**       INPUT  : 
**				key							entity key
**				pickloc						DAS pickloc record
**       OUTPUT :  
**				pt								closest point on the entity
**    RETURNS      : 
**			UU_SUCCESS  iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			The function called to calculate this point is called by
**				(*function) (eptr, tfmat, pickloc, pt)
*********************************************************************/

int uc_near_on_entity(key, pickloc, pt)
UU_KEY_ID key;
UD_NDCLOCREC *pickloc;
UM_coord pt;
	{
	UC_METHOD function, ucu_validate();
/*
... aak 10-nov-1997:
	struct UC_entitydatabag e;
*/
   struct NCL_fixed_databag e;
	UM_transf tfmat;
	int rel_num;
	int class;
	int status;

	uu_denter(UU_MTRC,(us,"uc_near_on_entity(key=%d,,pickloc=%x)",
		key, pickloc));

	um_retrieve_data_relnum(key, &rel_num);
	class = CONVERT_REL_TO_CLASS(rel_num);
	function = ucu_validate(&UC_cot_descriptor, class, UC_NEAR_ON_ENTITY);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		{
		e.key = key;
/* 
... aak 10-nov-1997: replaced
...   status = uc_retrieve_data(&e, sizeof(e));
*/
		status = ncl_retrieve_data_fixed(&e, sizeof(e));
		if(status != UU_SUCCESS) goto done;
		status = uc_retrieve_transf(key, tfmat);
		if(status != UU_SUCCESS) goto done;
		status = (*function)(&e, tfmat, pickloc, pt);
		}
done:;
	uu_dexitstatus("uc_near_on_entity", status) ;
	return(status);
	}

/**************************************************************************
**  E_FUNCTION:   int uc_feature(eptr, tfmat, feature, dploc)
**			Draw the specified feature order for the specified entity.
**  PARAMETERS   
**      INPUT  : 
**          eptr :			Pointer to the  entity
**				tfmat				Transformation matrix
**				feature			Order of feature to draw
**				dploc				picked locationon entity
**      OUTPUT :    
**				none
**  RETURNS      :  
**			0 iff no error;
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

int uc_feature(eptr, tfmat, feature, dploc)
struct UC_entitydatabag *eptr;
UM_transf tfmat;
int feature;
UD_PLOCREC *dploc;
	{                        
	UC_METHOD function, ucu_validate();
   int status;
   int class;

   uu_denter(UU_MTRC,(us,"uc_feature(key=%x,tfmat=%x,feature=%d)",(*eptr).key,
						tfmat, feature));
	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_FEATURE);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(eptr, tfmat, feature, dploc);
	uu_dexitstatus("uc_feature", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_proj_to_drawing(eptr, tfmat, attrptr,
**										tfmat, vrefpt, vpnorm,
**										keylist, render_solids)
**			Create a list of keys (KEYLIST) of entities to be included
**			in a drawing for the given entity (EPTR, TFMAT, ATTRPTR).
**			The transformation (DRWMAT) will position the viewplane
**			(VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**			scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**				eptr						pointer to entity data
**				tfmat						transformation to position entity
**											into MCS
**				attrptr					pointer to entity attribute bundle
**				drwmat					transformation to convert MCS to DCS
**				vrefpt					reference point of viewing plane
**				vpnorm					normal of viewing plane
**				render_solids			UU_TRUE => render solid entities
**       OUTPUT :  
**				keylist					list of keys which are to be included
**											in the drawing;  may be empty 
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_proj_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
							keylist, render_solids)
struct UC_entitydatabag *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
UU_LIST *keylist;
UU_LOGICAL render_solids;
	{                        
	UC_METHOD function, ucu_validate();
   int status;
   int class;

   uu_denter(UU_MTRC,(us,"uc_proj_to_drawing(key=%x)",(*eptr).key));
	class = (*eptr).rel_num;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_PROJ_TO_DRAWING);
	if(function == UC_UNDEFINED_METHOD)
		status = UU_FAILURE;
	else
		status = (*function)(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
									keylist, render_solids);
	uu_dexitstatus("uc_proj_to_drawing", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_draft_type(key, type)
**      Determine drafting type for entity with MTID key.
**    PARAMETERS   
**       INPUT  : 
**			 key	      	entity key
**       OUTPUT :  
**          type			drafting type
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_draft_type(key, type)
UU_KEY_ID key;
int *type;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;
	int rel_num;

	uu_denter(UU_MTRC, (us,"uc_draft_type(key:%d)",
		key));
	status = UU_SUCCESS;	/* assume success */
	status = um_retrieve_data_relnum(key, &rel_num);
	if(status == UU_SUCCESS)
		{
		class = CONVERT_REL_TO_CLASS(rel_num);
		function = ucu_validate(&UC_cot_descriptor, class, UC_DRAFT_TYPE);
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
			status = (*function)(rel_num, type);
		}
	uu_dexitstatus("uc_draft_type", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_draft_line(key, spt, ept)
**      Get data for a draftable line.
**    PARAMETERS   
**       INPUT  : 
**			 key	      	entity key
**       OUTPUT :  
**          spt			line start point 
**          ept			line end point 
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_draft_line(key, spt, ept)
UU_KEY_ID key;
UM_coord   spt, ept;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;
	int rel_num;

	uu_denter(UU_MTRC, (us,"uc_draft_line(key:%d)",
		key));
	status = UU_SUCCESS;	/* assume success */
	status = um_retrieve_data_relnum(key, &rel_num);
	if(status == UU_SUCCESS)
		{
		class = CONVERT_REL_TO_CLASS(rel_num);
		function = ucu_validate(&UC_cot_descriptor, class, UC_DRAFT_LINE);
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
			status = (*function)(key, spt, ept);
		}
	uu_dexitstatus("uc_draft_line", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_draft_arc(key, center, radius, dang, normal
**												, spt, ept, length)
**      Get data for a draftable arc.
**    PARAMETERS   
**       INPUT  : 
**			 key	      	entity key
**       OUTPUT :  
**          center		arc center point 
**          radius		arc radius
**          dang			arc angular extent
**          normal		arc normal
**          spt			arc start point 
**          ept			arc end point 
**          length		arc length
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_draft_arc(key, center, radius, dang, normal, spt, ept, length)
UU_KEY_ID key;
UM_coord   center;
UU_REAL    *radius, *dang, *length;
UM_vector  normal;
UM_coord   spt, ept;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;
	int rel_num;

	uu_denter(UU_MTRC, (us,"uc_draft_arc(key:%d)",
		key));
	status = UU_SUCCESS;	/* assume success */
	status = um_retrieve_data_relnum(key, &rel_num);
	if(status == UU_SUCCESS)
		{
		class = CONVERT_REL_TO_CLASS(rel_num);
		function = ucu_validate(&UC_cot_descriptor, class, UC_DRAFT_ARC);
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
			status = (*function)(key, center, radius,
																	dang, normal, spt, 
																	ept,length);
		}
	uu_dexitstatus("uc_draft_arc", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int uc_draft_conic(key, center, spt, ept)
**      Get data for a draftable conic.
**    PARAMETERS   
**       INPUT  : 
**			 key	      	entity key
**       OUTPUT :  
**          center		conic center point 
**          spt			conic start point 
**          ept			conic end point 
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_draft_conic(key, center, spt, ept)
UU_KEY_ID key;
UM_coord   center;
UM_coord   spt, ept;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;
	int rel_num;

	uu_denter(UU_MTRC, (us,"uc_draft_conic(key:%d)",
		key));
	status = UU_SUCCESS;	/* assume success */
	status = um_retrieve_data_relnum(key, &rel_num);
	if(status == UU_SUCCESS)
		{
		class = CONVERT_REL_TO_CLASS(rel_num);
		function = ucu_validate(&UC_cot_descriptor, class, UC_DRAFT_CONIC);
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
			status = (*function)(key, center, spt, ept);
		}
	uu_dexitstatus("uc_draft_conic", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: uc_draft_endpts(level,pickpath,pickloc,rel_num,cpt,opt)
**      Get the cartesian  coordinates of the end points of a
**      draftable curve. The closest point to the pick location is the
**	   	first point.
**    PARAMETERS   
**       INPUT  : 
**				levl							level of entity picked
**				pickpath						pick path 
**				pickloc                 pick location
**       OUTPUT :  
**				rel_num						relation number of entity picked
**				cpt							closest end point to pick
**				opt							other end point
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uc_draft_endpts(level, pickpath, pickloc, rel_num, cpt, opt)
int level;
UD_PPICKREC *pickpath;
UD_NDCLOCREC *pickloc;
int *rel_num;
UM_coord cpt;
UM_coord opt;
	{
	UC_METHOD function, ucu_validate();
	int status;
	int class;
	int relation;
	UM_PICKENT pent;
	UU_KEY_ID key;
	UU_KEY_ID um_get_pickkey();

	uu_denter(UU_MTRC, (us,"uc_draft_endpts()"));
	status = um_d_pickresolve(pickpath, level, &pent);
	if(status == 0)
		{
		key = um_get_pickkey(&pent, level);
		status = um_retrieve_data_relnum(key, &relation);
		if(status == UU_SUCCESS)
			{
			class = CONVERT_REL_TO_CLASS(relation);
			function = ucu_validate(&UC_cot_descriptor, class, UC_DRAFT_ENDPTS);
			if(function == UC_UNDEFINED_METHOD)
				status = UU_FAILURE;
			else
				status = (*function) (level, pickpath, pickloc, rel_num, cpt, opt);
			}
		}
	uu_dexitstatus("uc_draft_endpts", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: uc_deleteLast()
**       Delete the last entity(s) displayed.
**    PARAMETERS   
**       INPUT  :          none
**       OUTPUT :          none
**    RETURNS: UU_SUCCESS ifno problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_deleteLast()
	{
	int uc_deleteExceptions();
	UU_KEY_ID key;
	int initialize = UU_TRUE;
	char *list = UU_NULL;
	UU_LOGICAL cantUndel;
	int status = UU_SUCCESS;
	
	uu_denter(UU_MTRC,(us,"uc_deleteLast()"));
	if(ur_get_last_modified_mkey(&key) != UU_SUCCESS)
		goto failed;
	ur_enable_del_stack() ;	/* enable the delete stack	*/
	if(uc_getKeyList(initialize,key,uc_deleteExceptions,&cantUndel,
							&list) != UU_SUCCESS) goto failed;
	uu_dprint(UU_MTRC,(us,"from uc_deleteLast: list:%x, cantUndel:%d",
									list,cantUndel));
	initialize = UU_FALSE;
	if (!cantUndel && uc_delete(key) != UU_SUCCESS) goto failed;
	ur_disable_del_stack() ;	/* disable the delete stack	*/
	if(uc_processKeyList(list, uc_delete) != UU_SUCCESS) goto failed;

	goto done;

failed:
	status = UU_FAILURE;
done:
	uu_dexitstatus("uc_deleteLast", status);
	return(status);
	}

/*********************************************************************
**    I_FUNCTION: int uc_query(key, list)
**      Build a character array of the data from unibase
**      given an entity key, data array 
**
**      If this function cannot extract the contents for the
**      given key, it will return the following strings
**      in the first 3 rows:
**
**          Entity type: ???
**           key . . . . . . . . . . . . (input key)
**           rel_num . . . . . . . . . . (entity relation number)
**
**       The function returns a status of UU_FAILURE
**       so the application has the option of operating
**       on the contents of the data area or not.
**
**
**    PARAMETERS
**       INPUT  :
**          key               Unibase key.
**          list              2-D character array.
**										the string terminator, "\n".
**       OUTPUT :
**    RETURNS: UU_SUCCESS    if data has been extracted.
**           UU_FAILURE   	a class query method is not defined 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_query(key, list)
UU_KEY_ID key;
UU_LIST *list;
	{
	UC_METHOD function, ucu_validate();
	int rel_num;
	int status;
	int class;

	uu_denter(UU_MTRC,(us,"uc_query(key:%d,list:%x)",key, list));

	status = um_retrieve_data_relnum(key, &rel_num);
	if(status == 0)
		{
		class = CONVERT_REL_TO_CLASS(rel_num);
		function = ucu_validate(&UC_cot_descriptor, class, UC_QUERY);
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
			status = (*function)(key,list);
		}
	uu_dexitstatus("uc_query", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION : int uc_getKeyList(initialize, key, funct, added2listptr, 
**														list)
**		This function creates a list of keys of those enities that can
**		not currently  be undeleted. The "key" passed in is added to 
**		is the list, "list", ifit is the key of an entity that
**		can not be undeleted.          
**    PARAMETERS   
**       INPUT  : 
**				initialize		UU_TRUE iff a new list is to be created.
**				key				Key of the entity to be checked to see ifit
**									can be undeleted.
**				funct				Function used to determine what keys are to be 
**									added to "list". The call to "funct" is:
**									UU_LOGICAL add2list;
**										if(funct(key, &add2list) != UU_SUCCESS)
**												goto failed;
**				list				List of previous entity keys whose entities
**									can not be undeleted.
**       OUTPUT :  
**				added2listptr	Pointer whose value is UU_TRUE iff it can be
**									undeleted.
**				list				Same as input list except that the current
**									key may be on it.
**    RETURNS: UU_SUCCESS ifno problems encountered; UU_FAILURE
**					otherwise.
**    SIDE EFFECTS : Note, the list created here must be destoried
**						 with a call to "uu_lsdel"; "uc_processKeyList" will destory
**						 the list.
**    WARNINGS     : none
*********************************************************************/

int uc_getKeyList(initialize, key, funct, added2listptr, listptr)
UU_LOGICAL initialize;
UU_KEY_ID key;
int (*funct)();
UU_LOGICAL *added2listptr;
char **listptr;
	{
	char *uu_lsnew(), *uu_lsinsrt();
	char *nxtelt;
	UU_LOGICAL add2list;
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,
	"uc_getKeyList(initialz:%d,key:%d,funct,?,lst:%x)",initialize,key,*listptr));
	if(initialize)
		*listptr = uu_lsnew();
	if((*funct)(key, &add2list) != UU_SUCCESS)
			goto failed;
	if(add2list)
		{ 
		if(( nxtelt = uu_lsinsrt(*listptr, sizeof(UU_KEY_ID)) ) == UU_NULL)
				goto failed;
		*(UU_KEY_ID *)nxtelt = key;
		*added2listptr = UU_TRUE;
		}
	else *added2listptr = UU_FALSE;

	goto done;

failed:
	status = UU_FAILURE;
done:
	uu_dexitstatus("uc_getKeyList", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION:  int uc_processKeyList(list, funct)
**			This function applies the function, "funct", to each key found
**			on the list, "list", iff "list" is not UU_NULL.
**    PARAMETERS   
**       INPUT: 
**				list				List of keys to be processed.
**				funct				Function to use in processing the keys.
**									The call to "funct" is:
**									if(funct(key) != UU_SUCCESS)
**											goto failed;
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS ifno problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: Deletes the list.
**    WARNINGS: none
*********************************************************************/

int uc_processKeyList(list, funct)
char *list;
int  (*funct)();
	{
	char *uu_lsnext();
	char *nxtKeyptr;
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_processKeyList(list:%x,funct:%x)", list, funct));
	if(list != UU_NULL) /* list created; so process it */
		{
		nxtKeyptr = list;
		while ((nxtKeyptr = uu_lsnext(nxtKeyptr)) != UU_NULL)
			if (ur_test_tuple(*(UU_KEY_ID *)nxtKeyptr) == UU_SUCCESS)
			if((*funct)(*(UU_KEY_ID *)nxtKeyptr) != UU_SUCCESS)
				goto failed;
		uu_lsdel(list); /* delete list of keys */
		}
	else uu_dprint(UU_MTRC,(us,"from uc_processKeyList: list empty"));
	goto done;

failed:
	status = UU_FAILURE;
done:
	uu_dexitstatus("uc_processKeyList", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION : int uc_deleteExceptions(key, add2listptr)
**		This function determines whether the entity associated with the key,
**		"key", is to be added to the list of keys of entities that can not be
**		deleted explicitly, or, can't be undeleted undeleted.
**    PARAMETERS   
**       INPUT  : 
**			key				Key of the entity to be checked to see ifit
**								can be undeleted.
**       OUTPUT :  
**			add2listptr		Pointer whose value is UU_TRUE iff it can not be
**								undeleted.
**    RETURNS: UU_SUCCESS ifno problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_deleteExceptions(key, add2listptr)
UU_KEY_ID key;
UU_LOGICAL *add2listptr;

	{
	int status = UU_SUCCESS;
	UU_LOGICAL undeleteok;

	uu_denter(UU_MTRC,(us,"uc_deleteExceptions(key:%d,?)",key));

	status = uc_canbe_undeleted(key, &undeleteok);
	if (status != UU_SUCCESS) goto done;
	*add2listptr = !undeleteok;

done:
	uu_dexitstatus("uc_deleteExceptions", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_canbe_undeleted(key, undeleteok)
**			Determine if the entity (KEY) and be undeleted.
**    PARAMETERS   
**       INPUT  : 
**				key				key of entity
**       OUTPUT :  
**          undeleteok		UU_TRUE => can be undeleted
**									UU_FALSE => can not be undeleted
**    RETURNS      : 
**			UU_SUCCESS iff no problems encountered; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_canbe_undeleted(key, undeleteok)
UU_KEY_ID key;
UU_LOGICAL *undeleteok;

	{
	UC_METHOD function, ucu_validate();
	int status = UU_FAILURE;
	int class;
	int rel_num;

	uu_denter(UU_MTRC,(us,"uc_canbe_undeleted(key:%d)", key));
	if(um_retrieve_data_relnum(key, &rel_num) != UU_SUCCESS)
			goto done;
	class = CONVERT_REL_TO_CLASS(rel_num);
	function = ucu_validate(&UC_cot_descriptor, class, UC_CANBE_UNDELETED);
	if(function != UC_UNDEFINED_METHOD)
		{
		status = (*function)(key, undeleteok);
		}

done:;
	uu_dexitstatus("uc_canbe_undeleted", status);
	return(status);
	}

/*********************************************************************
**    I_FUNCTION :  int uc_canQuerySubentity(key, candoptr)
**			This function determines ifthe subentities of the entity whose key
**			is "key" can be queried.
**    PARAMETERS   
**       INPUT  : 
**          key			Key of the entity whose subentities maybe queried.
**       OUTPUT :  
**          candoptr		Pointer to UU_TRUE iff subentities can be queried.
**    RETURNS      : UU_SUCCESS ifno problems encountered; UU_FAILURE 
**							otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_canQuerySubentity(key, candoptr)
UU_KEY_ID key;
UU_LOGICAL *candoptr;
	{
	int rel_num, status = UU_SUCCESS;

	uu_denter(UU_MTRC, (us, "uc_canQuerySubentity(key,?)", key));
	if(um_retrieve_data_relnum(key, &rel_num) != UU_SUCCESS) goto failed;
	if(rel_num == UB_INSTANCE_REL) 
		{
		uu_uerror0(UB_SYMBOL, 102);
		/* error is: Subentities of symbol instances can't be queried. */
		*candoptr = UU_FALSE;
		}
	else *candoptr = UU_TRUE;

	goto done;

failed:
	status = UU_FAILURE;
done:
	uu_dexitstatus("uc_canQuerySubentity", status);
	return(status);
	}	

/*********************************************************************
**    E_FUNCTION:  int uc_deleteIfNecessary(key)
**			This function deletes only those entities that aren't deleted 
**			because of an association with another entity.
**    PARAMETERS   
**       INPUT: 
**				key		Key of the entity that is to be deleted ifthere is
**							not a reason to stop the deletion.
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS ifno problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/

int uc_deleteIfNecessary(key)
UU_KEY_ID key;
	{
	int rel_num, status = UU_SUCCESS;

	uu_denter(UU_MTRC, (us, "uc_deleteIfNecessary(key:%d)", key));
	if(um_retrieve_data_relnum(key, &rel_num) != 0) goto failed;
	if(rel_num != UB_CONECTOR_REL) 
		if(uc_delete(key) != UU_SUCCESS) goto failed;

	goto done;

failed:
	status = UU_FAILURE;
done:
	uu_dexitstatus("uc_deleteIfNecessary", status);
	return(status);
	}	

/*********************************************************************
**
**    E_FUNCTION:  int uc_super_class(rel_num)
**			This function returns the superclass of a relation
**
**    PARAMETERS   
**       INPUT: 
**				relnum = relation identifier
**       OUTPUT:  
**				none
**
**    RETURNS: superclass of the relation
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

int uc_super_class(relnum)
int relnum;
	{
	int class, super;

	uu_denter(UU_MTRC, (us, 
		"enter CLASS DISPATCHER, class = %d, method = uc_super_class", relnum));

	class = CONVERT_REL_TO_CLASS(relnum);
	super = (*UC_cot_descriptor.table_ptr[class]).super_class;
	uu_dprint(UU_MTRC, (us, "super_class = %d", super));

	uu_dexit;
	return(super);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_init_relations()
**	     Initialize ALL relations in unibase.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_relations()
	{
	int i, j, status;
	UC_METHOD initfunc;
	UC_TABLE_DESCRIPTOR *curtable;			/* current descriptor table pointer */

	uu_denter(UU_MTRC,(us,"uc_init_relations()"));

	uu_dprint(UU_MTRC,(us,"uc_init_relations: 3D=%x, add=%x, add1=%x",
		UC_3d_descriptor, UC_3d_descriptor[0], *UC_3d_descriptor));

/*	-- initialze the root class table -- */

	for(i=0; i<UC_root_descriptor.num_classes; i++)
		{

/*		-- for each non-null class (row), call the init function -- */

		if(UC_root_descriptor.table_ptr[i] != UU_NULL)
			{
			initfunc = (*UC_root_descriptor.table_ptr[i]).initialize;
			status = UU_SUCCESS;
			if(initfunc != UC_UNDEFINED_METHOD)
				status = (*initfunc)();
			if(status != UU_SUCCESS)
				goto failed;
			}
		}

/*	-- initialize the 3-D class table -- */

	for(j=0; j<UC_NUM_DOMAINS; j++)
		{
		curtable = UC_3d_descriptor[j];	

/*		-- for each defined domain -- */

		if(curtable != UU_NULL)
			{

/*			-- for all classes (rows) -- */

			uu_dprint(UU_MTRC,(us,"uc_init_relations, layer=%d, domain=%s",
					j, (*curtable).table_name));

			for(i=0; i<(*curtable).num_classes; i++)
				{

/*				-- for each non-null class (row), call the init function -- */

				if((*curtable).table_ptr[i] != UU_NULL)
					{
					initfunc = (*(*curtable).table_ptr[i]).initialize;
					status = UU_SUCCESS;
					if(initfunc != UC_UNDEFINED_METHOD)
						status = (*initfunc)();
					if(status != UU_SUCCESS)
						goto failed;
					}
				}
			}
		}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("uc_init_relations", status);
	return(status);
	}
	
/*********************************************************************
**    E_FUNCTION: int uc_autoUpdate(descrptr)
**	     	This function is the class entry point that Unibase calls to
**			automatically update associations.
**    PARAMETERS   
**       INPUT: 
**          descrptr		Unibase descriptor indicating the association
**								tuple to be updated and how it is to be updated.
**       OUTPUT: none. 
**    RETURNS: UU_SUCCESS ifno problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : All kinds of things, depending on what the application
**							update methods do.
**    WARNINGS     : none
*********************************************************************/
int uc_autoUpdate(descrptr)
	UR_assocUpdateDescriptor *descrptr;
	{                        
	UC_METHOD function, ucu_validate();
	int class;
	int status = UU_SUCCESS;
	uu_denter(UU_MTRC,(us,"uc_autoUpdate(descrptr>[assocKey:%d,1stmsg:%s])", 
					descrptr->assocKey,uu_msg(UR1_CURNT_MSG(descrptr))));

	if(um_retrieve_data_relnum(descrptr->assocKey, &class) != 0)
			goto failed;
	class = CONVERT_REL_TO_CLASS(class);
	function = ucu_validate(&UC_cot_descriptor, class, UC_ASSOC_UPDATE);
	if(function == UC_UNDEFINED_METHOD)
		goto failed;
	else
		if((*function)(descrptr) != UU_SUCCESS) goto failed;
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("uc_autoUpdate", status);
	return(status);
	}
