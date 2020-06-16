/*********************************************************************
**    NAME         :  nelabtbl.c
**       CONTAINS: routine to manipulate the labtbl entity
**       int ncl_init_labtbl()
**       int ncl_used_labtbl()
**       int ncl_delete_labtbl()
**       int ncl_add_labloc()
**       int ncl_add_ldrloc()
**       int ncl_retrieve_labloc()
**       int ncl_retrieve_ldrloc()
**       int ncl_delete_labloc()
**		 ncl_get_label_on()
**		 ncl_get_label_alter()
**		 ncl_get_label_ldr()
**		 ncl_set_label_on()
**		 ncl_set_label_alter()
**		 ncl_set_label_ldr()
**
**    COPYRIGHT 1989 (c) MILLS DATA SYSTEMS CO. INC.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nelabtbl.c , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 13:02:56
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdattr.h"
#include "nclfc.h"
#include "nccs.h"
#include "nclver.h"
#include "class.h"
#include "nclmodals.h"

extern UU_KEY_ID LABTBL_KEY;
/*********************************************************************
**    E_FUNCTION     : ncl_init_labtbl(key)
**       Create NCL entity NCL_labtbl_rel to store altered label loc.
**                  vp 2/10/98 stubbed (label table support removed!)
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_labtbl(key)
UU_KEY_ID key;
	{
	int status=UU_SUCCESS;
/*
	struct NCL_labtbl_rec labtbl;
	struct NCL_nclattr_rec attr;
	struct UM_transf_rec tran;
	int i;
	UU_KEY_ID keyid = 0, nxtid = 1;
*/
	/* don't know if I have a labtbl or not, search for it */
/*	if (key != NULL)
		{
		status = ur_get_next_data_key(NCL_LABTBL_REL, &nxtid, &keyid);
		if (status == 0)
			{
			LABTBL_KEY = keyid;
			return(status);
			}
		}
*/
	/* didn't find one, create one */
	/* initializing labtbl */
/*	ur_setup_data(NCL_LABTBL_REL, &labtbl, sizeof(struct NCL_labtbl_rec));
	labtbl.no_labloc = 0;
*/
	/* create UNIBASE entity */
/*	status = ur_create_data(&labtbl);
*/
	/* create UNIBASE attribute bundle and associate with entity */
	/* pass it some dummy type so we don't get an error */
/*	ncl_init_attr_rec(labtbl.key, &attr, 9);	
*/
	/* create UNIBASE identity transformation and associate with entity */
/*	tran.key = labtbl.key;
	tran.rel_num = UM_TRANSFORM_REL;
	um_tftotf(UM_idmat,tran.tfmat);
	if (status == 0)
		status = ur_update_transf(&tran);
*/
	/* set displayablity of entity */
/*	if (status == 0)
		status = ur_update_displayable(labtbl.key, UM_NEVERDISPLAYABLE);

	LABTBL_KEY = labtbl.key;
*/
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ncl_used_labtbl()
**       Returns UU_SUCCESS if used, UU_FAILURE otherwise
**                  vp 2/10/98 stubbed (label table support removed!)
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_used_labtbl()
	{
	int status;
	status = UU_FAILURE;

/*
	if (ur_retrieve_data_relnum(LABTBL_KEY, &rel_num) != 0)
		status = UU_FAILURE;
	else if (rel_num != NCL_LABTBL_REL)
		status = UU_FAILURE;
	else
		{
		labtbl.key = LABTBL_KEY;
		if (ur_retrieve_data_fixed(&labtbl) != 0)
			status = UU_FAILURE;
		else
			{
			if (labtbl.no_labloc == 0)
				return(status);
			else
			{
				for (i = 1; i <= labtbl.no_labloc; i++)
				{
					if (ur_retrieve_data_varlist(labtbl.key, 1, &lloc, i, 1) != 0) 
						status = UU_FAILURE;
					else if (lloc.index == -999)
						continue;
					else
						return(UU_SUCCESS);	
					}
				}
			}
		}
*/
	/* all altered labloc's deleted, unused */
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : ncl_delete_labtbl()
**       Delete the labtbl entry from unibase if unused.
**                  vp 2/10/98 stubbed (label table support removed!)
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_delete_labtbl()
	{
/*	ur_delete_all(LABTBL_KEY); */
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : ncl_add_labloc(eptr,index, altpos)
**       Create labloc entry in labtbl table.
**    PARAMETERS   
**       INPUT  : 
**          eptr  - entity pointer
**          altpos - new label location
**       OUTPUT :  
**          index - sets to be 2 (for compatibility with old labtbl) 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_labloc(eptr, index, altpos)
struct NCL_fixed_databag *eptr; 
int *index;
UM_coord altpos;
{
	int status;
	struct NCL_id_rec *hdr;

	hdr = (struct NCL_id_rec *) eptr;
/*
.....set label on and altered
*/
	ncl_set_label_on(index,1);
	ncl_set_label_alter(index,1);
/*
...vp 2/10/98 label table support removed
...copy label location into entity labloc
*/
	um_vctovc (altpos,hdr->labloc);
	status = ur_update_data_fixed(eptr);

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : ncl_add_ldrloc(eptr,index,ldrpt)
**       Create ldrloc entry in entity
**    PARAMETERS   
**       INPUT  : 
**          eptr  - entity pointer
**			ldrpt - start loaction for leader line on entity 
**       OUTPUT :  
**          index - sets to be 3
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_ldrloc(eptr, index,ldrpt)
struct NCL_fixed_databag *eptr; 
int *index;
UM_coord ldrpt;
{
	int status;
	struct NCL_id_rec *hdr;

	hdr = (struct NCL_id_rec *) eptr;
/*
.....set label on and altered
*/
	ncl_set_label_on(index,1);
	ncl_set_label_alter(index,1);
	/*
...copy leader line end location into entity ldrloc
*/
	um_vctovc (ldrpt,hdr->ldrloc);
	status = ur_update_data_fixed(eptr);

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : ncl_retrieve_labloc(index)
**       Retrieve a labloc entry in the labtbl table.
**    PARAMETERS   
**       INPUT  : 
**          eptr  - entity pointer
**          index - labloc flag (-2 or 2 to use labloc)
**       OUTPUT :  
**          pos - array to store retrived label location
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_retrieve_labloc(eptr,index,pos)
struct NCL_fixed_databag *eptr;
int index;
UM_coord pos;
{
	int status;
	struct NCL_id_rec *hdr;

	hdr = (struct NCL_id_rec *) eptr;
/*
.....Allow for turned off labels
.....Bobby  -  2/25/94
*/
/*
.....added new bitwise flag
.....if (index == 0 || index == 1)
*/
	if(!ncl_get_label_alter(index))
	{
		status = UU_FAILURE;
	}
	else 
/*
...vp 2/10/98 label table support removed
...get label location from entity labloc
*/
	{
		um_vctovc (hdr->labloc,pos);	
		status = UU_SUCCESS;
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_retrieve_ldrloc(index)
**       Retrieve a ldrloc entry 
**    PARAMETERS   
**       INPUT  : 
**          eptr  - entity pointer
**          index - label on flag 
**       OUTPUT :  
**          pos - array to store retrived label location
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_retrieve_ldrloc(eptr,index,pos)
struct NCL_fixed_databag *eptr;
int index;
UM_coord pos;
{
	int status;
	struct NCL_id_rec *hdr;

	hdr = (struct NCL_id_rec *) eptr;
/*
.....Allow for turned off labels
.....Bobby  -  2/25/94
*/
	if(!ncl_get_label_ldr(index))
	{
		status = UU_FAILURE;
	}
	else 
/*
...vp 2/10/98 label table support removed
...get label location from entity labloc
*/
	{
		um_vctovc (hdr->ldrloc,pos);	
		status = UU_SUCCESS;
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : ncl_delete_labloc(index)
**       Delete an entry in the labtbl table.
**    PARAMETERS   
**       INPUT  : 
**          index - 'segment' of labtbl table to delete
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int
ncl_delete_labloc(index)
int *index;
{
	int status=0;

	/* unreference labloc entry, set label ON */
	ncl_set_label_on(index,1);

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : ncl_fix_attr_label()
**       Fix attribute labe_on field in an older version (<9.0) of
**       unibase where label table enitity could exist and attributes
**       might be set to use it.  This fix sets label_on to 1 or 0
**       ignoring data in label table (which is not loaded anyway).
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int
ncl_fix_attr_label(key,rel)
int rel;
UU_KEY_ID key;
{
	int status, fix;
   struct NCL_nclattr_rec *nattr;
   struct UC_attributedatabag attr;

	nattr  = (struct  NCL_nclattr_rec *) &(attr);

	if (wf_geom_type(rel) == 0 || ncl_geom_type(rel) == 0)
	{
		fix = -1;
		status = uc_retrieve_attr (key,nattr);
		if (nattr->label_on > 1)
			fix = nattr->label_on = 1;
		else if (nattr->label_on < 0) 
			fix = nattr->label_on = 0;
		if (fix != -1) status =	ur_update_attr(&attr);
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_label_on(index)
**       Determines if label display is on
**    PARAMETERS   
**       INPUT  : 
**			index	:	label_on field 
**       OUTPUT :  
**          none
**    RETURNS      : 0: label off
**					 1: label on
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_label_on(index)
int	index;
{
	return(index & NCL_LABEL_BIT);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_label_alter(index)
**       Determines if label is altered
**    PARAMETERS   
**       INPUT  : 
**			index	:	label_on field 
**       OUTPUT :  
**          none
**    RETURNS      : 0: label at default location
**					 1: label at altered loaction
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_label_alter(index)
int	index;
{
	return(index & NCL_ALTER_BIT);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_label_ldr(index)
**       Determines if label has leader lines
**    PARAMETERS   
**       INPUT  : 
**			index	:	label_on field 
**       OUTPUT :  
**          none
**    RETURNS      : 0: label does not have leader lines
**					 1: label has leader lines
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_label_ldr(index)
int	index;
{
	return(index & NCL_LDR_BIT);
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_label_on(index,on)
**       set label display on/off
**    PARAMETERS   
**       INPUT  : 
**			index	:	label_on field 
**			on		:	on/off flag
**       OUTPUT :  
**          index	:	updated label_on field
**    RETURNS      updated label_on field
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_set_label_on(index,on)
int	*index;
int on;
{
	if(on)
		*index = *index | NCL_LABEL_BIT;
	else
		*index = *index & (~NCL_LABEL_BIT);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_label_alter(index,on)
**       set label alter on/off
**    PARAMETERS   
**       INPUT  : 
**			index	:	label_on field 
**			on		:	on/off flag
**       OUTPUT :  
**          index	:	updated label_on field
**    RETURNS      updated label_on field
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_set_label_alter(index,on)
int	*index;
int on;
{
	if(on)
		*index = *index | NCL_ALTER_BIT;
	else
		*index = *index & (~NCL_ALTER_BIT);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_label_ldr(index,on)
**       set label ldr on/off
**    PARAMETERS   
**       INPUT  : 
**			index	:	label_on field 
**			on		:	on/off flag
**       OUTPUT :  
**          index	:	updated label_on field
**    RETURNS      updated label_on field
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_set_label_ldr(index,on)
int	*index;
int on;
{
	if(on)
		*index = *index | NCL_LDR_BIT;
	else
		*index = *index & (~NCL_LDR_BIT);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_check_label_prefix(label,subfl)
**       Checks the label if the subscript flag is enabled and returns
**       an error if the label is an NCL vocabulary word.
**    PARAMETERS   
**       INPUT  : 
**				label   = Label to check.
**				subfl   = UU_TRUE = This is a subscripted label.
**				flag    = UU_TRUE = Modify the label to be a valid subscripted
**                    prefix.
**       OUTPUT :  
**          none
**    RETURNS   : UU_FAILURE if the label is not valid as a subscripted
**                prefix.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_check_label_prefix(label,subfl,flag)
char *label;
UU_LOGICAL subfl;
UU_LOGICAL flag;
{
#define NVOC 14
	int i;
	char tlab[64];
	static char prefix[NVOC][4]={"PT","PV","LN","VE","PL","CI","CV","SF","SH",
		"MX","PN","AN","SY","SO"};
/*
.....Return an error if a vocabulary word prefix
.....is used with a subscripted label
*/
	if (subfl)
	{
		strcpy(tlab,label);
		ul_to_upper(tlab);
		for (i=0;i<NVOC;i++)
		{
			if (strcmp(tlab,prefix[i]) == 0)
			{
				if (flag)
				{
					label[2] = label[1]; label[3] = '\0';
				}
				return(UU_FAILURE);
			}
		}
	}
	return(UU_SUCCESS);
}
