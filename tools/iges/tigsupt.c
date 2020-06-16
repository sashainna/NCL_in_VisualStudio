/*********************************************************************
**    NAME         :  tigsupt.c
**       CONTAINS:
**            int ncl_store_wf1 (key)
**            int ncl_label_wf (rel_num,label,isub,key,nstat)
**            int uc_create_data (eptr, tfmat, attrptr)
**            int uc_retrieve_data (eptr, isize)
**            int uc_setup_data (rel_num, eptr, isize)
**            int uc_delete (key)
**            int uc_retrieve_transf (key, tfmat)
**            int gettol (tol8)
**            int ncl_set_agcnv (lagcnv)
**            uig_igtf_to_tf(t, tfmat)
**            uig_tf_to_igtf(tfmat, t)
**            int uig_label_check(label, isub)
**            int uig_label_delete(label, isub)
**            void uig_label_emptylists()
**            uig_remove_dup()
**            uc_copy()
**            ncl_format_label()
**    COPYRIGHT 1992 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			tigsupt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:50
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "umath.h"
#include "mcrv.h"
#include "mdattr.h"
#include "tigsupp.h"
#include "mdrel.h"
#include "udebug.h"
#include "usysdef.h"
#include "mxxx.h"
#include "mdeval.h"
#include "mdclass.h"
#include "rbase.h"
#include "mfort.h"
#include "nclfc.h"
#include "nccs.h"

extern double ctol;
struct UIG_name_list_data
{
   char label[64];
   int isub;
};
/*********************************************************************
**    I_FUNCTION     :  ncl_store_wf1 (key)
**        Stub for Iges.
**    PARAMETERS   
**       INPUT  : 
**              key      - key of entity.
**       OUTPUT :  
**              none   
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_store_wf1 (key)
    UU_KEY_ID key;
    {
    int status = UU_SUCCESS;
    return(status);
    }
/*********************************************************************
**    I_FUNCTION     :  ncl_label_wf (rel_num,label,isub,key,nstat)
**        Stub for Iges.
**    PARAMETERS   
**       INPUT  : 
**              rel_num  - rel num of entity.
**              key      - key of entity.
**       OUTPUT :  
**              label    - label of entity
**              isub     - subscript of entity
**              nstat    - not used.
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_label_wf (rel_num,label,isub,key,nstat)
    int rel_num;
    char *label;
    int *isub;
    UU_KEY_ID key;
    UM_int2 nstat;
    {
    int status = UU_SUCCESS;

    uu_denter(UU_MTRC,(us,"ncl_label_wf()"));

    uu_dexit;

    return(status);
    }
/*********************************************************************
**    I_FUNCTION     :  uc_create_data (eptr, tfmat, attrptr)
**        Stub for Iges.
**    PARAMETERS   
**       INPUT  : 
**
**
**       OUTPUT :  
**              none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_create_data (eptr, tfmat, attrptr)
    struct UC_entitydatabag *eptr;
    UM_transf tfmat;
    struct UC_attributedatabag *attrptr;
    {
    int status = UU_SUCCESS;

    uu_denter(UU_MTRC,(us,"uc_create_data()"));

    status = uig_create_geom (eptr,tfmat,attrptr,0);
    uu_dexit;

    return(status);
    }
/*********************************************************************
**    I_FUNCTION     :  uc_retrieve_data (eptr, isize)
**        Stub for Iges.
**    PARAMETERS   
**       INPUT  : 
**
**
**       OUTPUT :  
**              none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_retrieve_data (eptr, isize)
    struct UC_entitydatabag *eptr;
    int isize;
    {
    int status;

    uu_denter(UU_MTRC,(us,"uc_retrieve_data()"));

    status = ncl_retrieve_data_fixed(eptr);

    uu_dexit;

    return(status);
    }
/*********************************************************************
**    I_FUNCTION     :  uc_setup_data (rel_num, eptr, isize)
**        Stub for Iges.
**    PARAMETERS   
**       INPUT  : 
**
**
**       OUTPUT :  
**              none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_setup_data (rel_num, eptr, isize)
    int rel_num;
    struct UC_entitydatabag *eptr;
    int isize;
    {
    int status = UU_SUCCESS;

    uu_denter(UU_MTRC,(us,"uc_setup_data()"));

    status = ur_setup_data (rel_num, eptr, isize);

    uu_dexit;

    return(status);
    }
/*********************************************************************
**    I_FUNCTION     :  uc_delete (key)
**        Stub for Iges.
**    PARAMETERS   
**       INPUT  : 
**
**
**       OUTPUT :  
**              none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_delete (key)
    UU_KEY_ID key;
    {
    int status;

    uu_denter(UU_MTRC,(us,"uc_delete()"));

    status = ur_delete_all (key);

    uu_dexit;

    return(status);
    }
/*********************************************************************
**    I_FUNCTION     :  uc_retrieve_transf (key, tfmat)
**        Stub for Iges.
**    PARAMETERS   
**       INPUT  : 
**
**
**       OUTPUT :  
**              none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_retrieve_transf (key, tfmat)
    UU_KEY_ID key;
    UU_REAL tfmat[4][3];
    {
    int  status = UU_SUCCESS;

    uu_denter(UU_MTRC,(us,"uc_retrieve_transf()"));

    status = uc_retrieve_mtuple_transf(key,tfmat);

    uu_dexit;

    return(status);
    }
/*********************************************************************
**    I_FUNCTION     :  gettol (tol8)
**        Stub for Iges.
**    PARAMETERS   
**       INPUT  : 
**
**
**       OUTPUT :  
**              none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gettol (tol8)
    UM_real8 *tol8;
    {
    int status = UU_SUCCESS;

    uu_denter(UU_MTRC,(us,"gettol()"));

    *tol8 = ctol;

    uu_dexit;

    return(status);
    }
/*********************************************************************
**    I_FUNCTION     :  ncl_set_agcnv (lagcnv)
**        Stub for Iges.
**    PARAMETERS   
**       INPUT  : 
**            none
**       OUTPUT :  
**            lagcnv     - UU_FALSE (Meaning do not convert AG entities).
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_set_agcnv (lagcnv)
    UU_LOGICAL *lagcnv;
    {
    int status = UU_SUCCESS;

    uu_denter(UU_MTRC,(us,"ncl_set_agcnv()"));

    *lagcnv = UU_FALSE;

    uu_dexit;

    return(status);
    }
/*********************************************************************
**    I_FUNCTION     :  ncl_igtf_to_tf(t,tfmat)
**        Convert an IGES Tranformation to an NCL (unibase) transformation.
**    PARAMETERS
**       INPUT  :
**            t          - IGES transformation.
**       OUTPUT :
**            tfmat      - unibase transformation.
**    RETURNS      :
**       UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_igtf_to_tf(t, tfmat)
UU_REAL t[12];
UM_transf tfmat;
{
	int i, j, k;

	k = 0;
	for (i=0;i<3;i++)
	{
		for (j=0;j<4;j++) tfmat[j][i] = t[k++];
	}

}
/*********************************************************************
**    I_FUNCTION     :  ncl_igtf_to_ncltf(t,tfmat)
**        Convert an NCL (unibase) tranformation to an IGES transformation.
**    PARAMETERS
**       INPUT  :
**            tfmat      - unibase transformation.
**       OUTPUT :
**            t          - IGES transformation.
**    RETURNS      :
**       UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_tf_to_igtf(tfmat, t)
UM_transf tfmat;
UU_REAL t[12];
{
	int i, j, k;

	k = 0;
	for (i=0;i<3;i++)
	{
		for (j=0;j<4;j++) t[k++] = tfmat[j][i];
	}

}

#define NCL_MAX_LABEL 64
#define UIG_NAME_HASH 37
static char *UIG_name_list[UIG_NAME_HASH] = {UU_NULL};
/*static struct UIG_name_list_data
{
	char label[8];
	int isub;
};*/

char *uu_lsnew(), *uu_lsinsrt(), *uu_lsnext();

/*********************************************************************
**    I_FUNCTION     : int uig_label_check(label,isub)
**        Check if a label is valid and is in the name lists. If it is
**        valid and not in the name lists, store it.
**    PARAMETERS
**       INPUT  :
**            label      - Label of entity.
**            isub       - Subscript of entity.
**       OUTPUT :
**            none
**    RETURNS      : 1 if label found,
**                   2 if label contains invalid characters,
**                   0 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_label_check(label, isub)
char *label;
int isub;
{
	int i, ix, n1;
	char llab[NCL_MAX_LABEL+1], *cptr;
	struct UIG_name_list_data *p1;

	ix = i = 0;
	cptr = label;
/*
.....Check for valid label and calculate hash index.
*/
	if (!isalnum(*cptr) && *cptr != '#') return(2);
	while (*cptr && *cptr != ' ')
	{
		ix += *cptr;
		llab[i++] = *cptr++;
		if (i>NCL_MAX_LABEL) return(2);
		if (*cptr && *cptr != ' ' && !isalnum(*cptr) && *cptr != '#'
		          && *cptr != '_' && *cptr != '.') return(2);
	}
	llab[i] = '\0';
	ix = (ix + isub) % UIG_NAME_HASH;
/*
.....look for label in name list
*/
	p1 = (struct UIG_name_list_data *)UIG_name_list[ix];
	if (p1) p1 = (struct UIG_name_list_data *) uu_lsnext(p1);
	while (p1)
	{
		if (!strcmp(llab, p1->label) && p1->isub == isub)
		{
			return(1);
		}
		p1 = (struct UIG_name_list_data *) uu_lsnext(p1);
	}
/*
.....Not in lists. Store it.
*/
	if (!UIG_name_list[ix])
	{
		UIG_name_list[ix] = uu_lsnew();
	}
	n1 = sizeof(struct UIG_name_list_data);
	p1 = (struct UIG_name_list_data *)UIG_name_list[ix];
	if (p1) p1 = (struct UIG_name_list_data *) uu_lsinsrt(p1,n1);
	if (p1)
	{
		strcpy(p1->label, llab);
		p1->isub = isub;
	}
	return(0);
}

/*********************************************************************
**    I_FUNCTION     : int uig_label_delete(label,isub)
**        Delete a label if it is in the name lists.
**    PARAMETERS
**       INPUT  :
**            label      - Label of entity.
**            isub       - Subscript of entity.
**       OUTPUT :
**            none
**    RETURNS      : 1 if label found,
**                   2 if label contains invalid characters,
**                   0 otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_label_delete(label, isub)
char *label;
int isub;
{
	int i, ix, n1;
	char llab[NCL_MAX_LABEL+1], *cptr;
	struct UIG_name_list_data *p1;

	ix = i = 0;
	cptr = label;
/*
.....Check for valid label and calculate hash index.
*/
	if (!isalnum(*cptr) && *cptr != '#') return(2);
	while (*cptr && *cptr != ' ')
	{
		ix += *cptr;
		llab[i++] = *cptr++;
		if (i>NCL_MAX_LABEL) return(2);
		if (*cptr && *cptr != ' ' && !isalnum(*cptr) && *cptr != '#'
		          && *cptr != '_' && *cptr != '.') return(2);
	}
	llab[i] = '\0';
	ix = (ix + isub) % UIG_NAME_HASH;
/*
.....look for label in name list
*/
	p1 = (struct UIG_name_list_data *)UIG_name_list[ix];
	if (p1) p1 = (struct UIG_name_list_data *) uu_lsnext(p1);
	while (p1)
	{
		if (!strcmp(llab, p1->label) && p1->isub == isub)
		{
			uu_lsdele(p1);
			return(1);
		}
		p1 = (struct UIG_name_list_data *) uu_lsnext(p1);
	}
/*
.....not in the list
*/
	return(0);
}

/*********************************************************************
**    I_FUNCTION     : int uig_label_emptylists()
**        Empty the name lists.
**    PARAMETERS
**       INPUT  :
**            none
**       OUTPUT :
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_label_emptylists()
{
	int i;

	for (i=0;i<UIG_NAME_HASH;i++)
	{
		if (UIG_name_list[i]) uu_lsempty(UIG_name_list[i]);
	}

	return;
}

/*********************************************************************
**    I_FUNCTION     : uig_remove_dup(eptr)
**        Deletes the entity from the Unibase and increases the duplicates
**        count.
**    PARAMETERS
**       INPUT  :
**            eptr   - Entity to delete.
**       OUTPUT :
**            key    - Reset to 0.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_remove_dup(eptr,key)
struct NCL_fixed_databag *eptr;
UU_KEY_ID *key;
{
	ur_delete_all(eptr->key);
	uig_label_delete(eptr->label,eptr->subscr);
	um_reset_active_label(eptr->rel_num);
	UIG_dupcount++;
	*key = 0;
}

/*********************************************************************
**    I_FUNCTION     :  uc_retrieve_attr(key, attrptr)
**        Stub for Iges.
**    PARAMETERS
**       INPUT  :
**
**
**       OUTPUT :
**              none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_retrieve_attr(key, attrptr)
UU_KEY_ID key;
struct UC_attributedatabag *attrptr;
{
   int status = UU_SUCCESS;
   status = uc_retrieve_mtuple_attr(key,attrptr);
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

int uc_copy(e1, e2, entsize)
struct UC_entitydatabag *e1;
struct UC_entitydatabag *e2;
int entsize;
{
	int status;

	switch (e1->rel_num)
	{
		case UM_LINE_REL:
			status = um_cp2_copyline(e1, e2, entsize);
			break;
		case UM_CIRCLE_REL:
			status = um_cp3_copycirc(e1, e2, entsize);
			break;
		case UM_CONIC_REL:
			status = um_cp4_copyconic(e1, e2, entsize);
			break;
		case UM_RBSPLCRV_REL:
			status = um_cp6_copybsplcrv(e1, e2, entsize);
			break;
		case UM_AGCRV_REL:
			status = um_agcrv_copy(e1, e2, entsize);
			break;
		case UM_COMPCRV_REL:
			status = um_cp5_copycompcrv(e1, e2, entsize);
			break;
		default:
			status = UU_FAILURE;
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_format_label(label,isub,str,pflg)
**       Formats the label and optional string into a text string
**       representation.
**    PARAMETERS
**       INPUT  :
**          label            Entity label
**          isub             Entity subscript
**          plfg             Parenthesis flag
**                           0: Use parenthesis around subscript
**                           1: Omit parenthesis around subscript
**       OUTPUT :
**          str              Text string representation 'lab(sub)'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_format_label(label,isub,str,pflg)
char *label,*str;
int isub,pflg;
{
	if (isub == 0)
		sprintf(str, "%s", label);
	else
	{
		if (pflg == 0) sprintf(str, "%s(%d)", label, isub);
		else sprintf(str, "%s%d", label, isub);
	}
}
