/*********************************************************************
**    NAME         :  m2elabel.c
**       CONTAINS: label manipulation routines
**			int um_init_labels()
**			int um_save_labels()
**			int um_reset_labels()
**			int um_save_active_label()
**			int um_reset_active_label()
**			umi_init_rel_label(rel_num, pf)
**			um_update_rel_label(rel_num, pf, isub)
**			int um_get_rel_label(rel_num,pf,isub)
**			int um_auto_label(rel_num, label)
**			int um_next_auto_label(rel_num, label)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2elabel.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:46
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
/* #include "dasnog.h" */
#include "mdclass.h"
#include "mdrel.h"
#include "mlab.h"
#include "nclmodals.h"
#include "rver9700.h"

char UM_overflow_pf[256];
extern int UR_active;
static struct UM_labelmdl_rec UM_labelmdl_save;
static int Sindex;

/*********************************************************************
**    E_FUNCTION     : int um_init_labels()
**       Initialize the automatic label server. A predefined label
**			"UN" will be defined and all relations will initially be
**			set to return this label if a request is made for a label 
**			and no other label prefix has been defined for the relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_init_labels()

	{
	int i;

	if (UR_active == 2) return;
	uu_denter(UU_MTRC, (us,"um_init_labels()"));


	UM_label_mode = UM_AUTO_LABEL;

/* MILLS - increase label prefixes from 10 to 20. (Also change mlabddl.h) */
	UM_labelmdl.max = 40; 
	UM_labelmdl.num = 1;
	UM_labelmdl.next[0] = 1;

/*
	UM_labelmdl_old.max = 20; 
	UM_labelmdl_old.num = 1;
	UM_labelmdl_old.next[0] = 1;
*/

/* MILLS -  Added 3 new field to UM_labelmdl_rec for Auto generated subscripted names.*/

	for (i=0; i<20; i++) 
	{
		UM_labelmdl.subscr[i] = 0;
		UM_labelmdl.issub[i] = 0;
		strcpy(UM_labelmdl.pf[i], "UN");
		strcpy(UM_labelmdl.pfs[i], "");
/*
		UM_labelmdl_old.subscr[i] = 0;
		UM_labelmdl_old.issub[i] = 0;
		strcpy(UM_labelmdl_old.pf[i], "UN");
		strcpy(UM_labelmdl_old.pfs[i], "");
*/
	}
	for (i=0; i<256; i++) UM_labelmdl.rel[i] = 0;
/*	for (i=0; i<256; i++) UM_labelmdl_old.rel[i] = 0;*/

	for (i=0; i<256; i++) UM_overflow_pf[i] = 'X';
	UM_overflow_pf[UM_POINT_REL]      = 'P';
	UM_overflow_pf[UM_LINE_REL]       = 'L';
	UM_overflow_pf[UM_CIRCLE_REL]     = 'A';
	UM_overflow_pf[UM_CONIC_REL]      = 'C';
	UM_overflow_pf[UM_COMPCRV_REL]    = 'C';
	UM_overflow_pf[UM_RBSPLCRV_REL]   = 'C';
	UM_overflow_pf[UM_RBSPLSRF_REL]   = 'S';
	UM_overflow_pf[UM_UVCVONSF_REL]   = 'C';
	UM_overflow_pf[NCL_VECTOR_REL]    = 'V';
	UM_overflow_pf[NCL_MATRIX_REL]    = 'M';
	UM_overflow_pf[NCL_CURVE_REL]     = 'C';
	UM_overflow_pf[NCL_SURF_REL]      = 'S';
	UM_overflow_pf[NCL_MESHSURF_REL]  = 'S';
	UM_overflow_pf[NCL_QUILTSURF_REL] = 'S';
	UM_overflow_pf[NCL_SHAPE_REL]     = 'B';
	UM_overflow_pf[NCL_POINT_REL]     = 'P';
	UM_overflow_pf[NCL_LINE_REL]      = 'L';
	UM_overflow_pf[NCL_CIRCLE_REL]    = 'A';
	UM_overflow_pf[NCL_PLN_REL]       = 'N';
	UM_overflow_pf[NCL_PATERN_REL]    = 'R';
	UM_overflow_pf[NCL_NETSF_REL]     = 'S';
	UM_overflow_pf[NCL_EVALCV_REL]    = 'C';
	UM_overflow_pf[NCL_EVALSF_REL]    = 'S';
	UM_overflow_pf[NCL_POINTVEC_REL]  = 'T';
	UM_overflow_pf[NCL_TRIMSF_REL]    = 'S';
	UM_overflow_pf[UA_TEXT_REL]       = 'X';
	UM_overflow_pf[UM_SOLID_REL]      = 'O';

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int um_save_labels()
**       Saves the label server structure in its current state.
**			'um_reset_labels' will restore this structure.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_save_labels()
{
	ur_get_labelmdl(&UM_labelmdl_save);
}

/*********************************************************************
**    E_FUNCTION     : int um_reset_labels()
**       Restores the label server structure to its initial state.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_reset_labels()
{
	ur_put_labelmdl(&UM_labelmdl_save);
}

/*********************************************************************
**    E_FUNCTION     : int um_save_active_label(rel_num)
**       Saves the current label count for the specified relation number.
**    PARAMETERS   
**       INPUT  : 
**          rel_num  = Relation number of label index to save.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_save_active_label(rel_num)
int rel_num;
{
	if (UM_labelmdl.issub[UM_labelmdl.rel[rel_num]] == 0)
		Sindex = UM_labelmdl.next[UM_labelmdl.rel[rel_num]];
	else
		Sindex = UM_labelmdl.subscr[UM_labelmdl.rel[rel_num]];
}

/*********************************************************************
**    E_FUNCTION     : int um_reset_active_label(rel_num)
**       Resets the previously saved label index for the specified
**       relation number.
**    PARAMETERS   
**       INPUT  : 
**          relnum  = Relation number of label index to restore.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_reset_active_label(rel_num)
int rel_num;
{
	if (UM_labelmdl.issub[UM_labelmdl.rel[rel_num]] == 0)
		UM_labelmdl.next[UM_labelmdl.rel[rel_num]] = Sindex;
	else
		UM_labelmdl.subscr[UM_labelmdl.rel[rel_num]] = Sindex;
}

/*********************************************************************
**    I_FUNCTION     : umi_init_rel_label(rel_num, pf)
**			Request that the automatic label server use the prefix (PF)
**			for the given relation (REL_NUM). If the prefix is identical
**			to one already in the label server, the two relations will
**			share names. If the new relation results in too many names
**			in the label server, the relation will continue to use "UN"
**			and a failure status will be returned.
**    PARAMETERS   
**       INPUT  : 
**          rel_num					relation number
**				pf							20 character prefix
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if the prefix is added to the label name server
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umi_init_rel_label(rel_num, pf, flg)
	int rel_num;
	char pf[];
	int flg; /*unused but passed in, why? */

	{
	int i;
	int status;

	if (UR_active == 2) return(UU_SUCCESS);

	uu_denter(UU_MTRC,(us,"umi_init_rel_label(rel_num=%d, pf=%s)",
		rel_num, pf));

	status = UU_SUCCESS;
	if ((0 < rel_num) && (rel_num < 256))
	{
		for (i=0; i< UM_labelmdl.num; i++)
		{
			if (strcmp(pf, UM_labelmdl.pf[i]) == 0)
				break;
		}
		if (i < UM_labelmdl.num)
		{/* new matches one in table */
			UM_labelmdl.rel[rel_num] = i;
		}
/*
		else if (UM_labelmdl.num == UM_labelmdl.max)
		{/* new one would overflow table; use undefined ("UN") label */
/*			status = UU_FAILURE;
			UM_labelmdl.rel[rel_num] = 0;
		}
*/
		else
			{/* new one is to be added to table */
/*			UM_labelmdl.rel[rel_num] = i;  */
			if (UM_labelmdl.rel[rel_num]!=0)
                i = UM_labelmdl.rel[rel_num];
			else
				UM_labelmdl.rel[rel_num] = i;
			strcpy(UM_labelmdl.pf[i], pf);
			UM_labelmdl.next[i] = 1;
			UM_labelmdl.subscr[i] = 0;  /*MILLS- new field in UM_labelmdl_rec structure */
			UM_labelmdl.num++;
			}
		}

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    I_FUNCTION     : um_update_rel_label(rel_num, pf, isub)
**			Request that the automatic label server use the prefix (PF)
**			for the given relation (REL_NUM). This routine merely changes
**			the prefix for the specified relation (and all related
**			relations (i.e. NCL_POINT_REL, UM_POINT_REL, etc.).  A current
**			label should already exist, but if one does not it creates one
**			(means that something is wrong).
**    PARAMETERS   
**       INPUT  : 
**          rel_num		relation number
**				pf				name prefix for relation
**				isub			subscript flag for relation
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS if the prefix is added to the label name server
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_update_rel_label(rel_num, pf, isub)
int rel_num;
char *pf;
int isub;
{
	int inc;
	int status;
/*
.....Don't update label when GETting from Unibase
*/
	if (UR_active == 2) return(UU_SUCCESS);
	status = UU_FAILURE;
/*
.....Check for valid relation number
*/
	if ((0 < rel_num) && (rel_num < 256))
	{
/*
.....Make sure that this relation does not point
.....to UN label
*/
		if (UM_labelmdl.rel[rel_num] == 0)
		{
/*
........Should never get here (logic problem)
........But if it does, then reinitialize label structure
*/
			um_reset_labels();
			if (UM_labelmdl.rel[rel_num] == 0 || isub == 0)
			{
				status = umi_init_rel_label(rel_num, pf, 0);
				if (status != UU_SUCCESS || UM_labelmdl.rel[rel_num] == 0)
					return(UU_FAILURE);
			}
		}
/*
.....Store new label prefix & subscript flag
*/
		inc = UM_labelmdl.rel[rel_num];
		if (isub == 0)
			strcpy(UM_labelmdl.pf[inc],pf);
		else
		{
			if (strcmp(UM_labelmdl.pfs[inc],pf) != 0) UM_labelmdl.subscr[inc] = 1;
			strcpy(UM_labelmdl.pfs[inc],pf);
		}
		UM_labelmdl.issub[UM_labelmdl.rel[rel_num]] = isub;
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : um_get_rel_label(rel_num, pf, isub)
**			Requests the automatic label prefix and subscript flag
**			for the given relation (REL_NUM).
**    PARAMETERS   
**       INPUT  : 
**          rel_num					relation number
**       OUTPUT :  
**				pf							Label prefix
**          isub						Returns 1 if subscripting is enabled
**											for this relation.
**    RETURNS      : 
**			UU_SUCCESS if this relation is assigned a label.
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_get_rel_label(rel_num,pf,isub)
int rel_num;
char pf[];
int *isub;
{
	int status,inc;
/*
.....Return relation label and subscript flag
*/
	status= UU_SUCCESS;
	inc = UM_labelmdl.rel[rel_num];
	if (0 < rel_num && rel_num < 256)
	{
		if (UM_labelmdl.rel[rel_num] != 0)
		{
			if (UM_labelmdl.issub[inc] == 0)
				strcpy(pf,UM_labelmdl.pf[inc]);
			else
				strcpy(pf,UM_labelmdl.pfs[inc]);
			*isub = UM_labelmdl.issub[inc];
		}
		else
			goto failed;
	}
	else
		goto failed;
	goto done;
/*
.....No name generated
*/
failed:;
		status = UU_FAILURE;
		strcpy(pf,"UN");
		*isub = 0;
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int um_auto_label(rel_num, label)
**       Automatically generate a LABEL for the specified relation
**			(REL_NUM).
**    PARAMETERS   
**       INPUT  : 
**          rel_num				relation for which to generate label
**       OUTPUT :  
**          label					next label for relation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_auto_label(rel_num, label)
	int rel_num;
	char *label;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_auto_label(rel_num=%d)", rel_num));

	/* MILLS- Added 3 new fields in UM_labelmdl_rec: issub, subscr, and pfs */

	status = um_next_auto_label(rel_num, label);
	if (status == UU_SUCCESS)
		if (UM_labelmdl.issub[UM_labelmdl.rel[rel_num]] == 0)
			UM_labelmdl.next[UM_labelmdl.rel[rel_num]]++;
		else
			UM_labelmdl.subscr[UM_labelmdl.rel[rel_num]]++;

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_auto_subscr(rel_num, sub)
**       Returns the subscript value for the specified relation
**			(REL_NUM).
**    PARAMETERS   
**       INPUT  : 
**          rel_num				relation for which to generate label
**       OUTPUT :  
**          sub  					next subscript for relation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_auto_subscr(rel_num, sub)
int rel_num;
int *sub;
{
	if (UM_labelmdl.issub[UM_labelmdl.rel[rel_num]] == 0)
		*sub = 0;
	else
		*sub = UM_labelmdl.subscr[UM_labelmdl.rel[rel_num]];
	return;
}

/*********************************************************************
**    E_FUNCTION     : int um_next_auto_label(rel_num, label)
**       Determine the label (LABEL) which would be the next one
**			automatically generated for the specified relation (REL_NUM).
**    PARAMETERS   
**       INPUT  : 
**          rel_num				relation for which to generate label
**       OUTPUT :  
**          label					next label for relation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_next_auto_label(rel_num, label)
	int rel_num;
	char *label;

	{
	int status;
	int i;
	char lab[80], emsg[256];

	uu_denter(UU_MTRC,(us,"um_next_auto_label(rel_num=%d)", rel_num));

	status = UU_FAILURE;
   if ((0 < rel_num) && (rel_num < 256))
      {
      i = UM_labelmdl.rel[rel_num];
      if (UM_labelmdl.issub[i] == 1)
         {
         sprintf(lab, "%s", UM_labelmdl.pfs[i]);
         }
      else
         {
			if  (UM_labelmdl.next[i] > 1000000 && strlen(UM_labelmdl.pf[i]) > 1)
            {
				lab[0] = UM_overflow_pf[rel_num];
				lab[1] = '\0';
				status = umi_init_rel_label(rel_num, lab, i);
				i = UM_labelmdl.rel[rel_num];
				sprintf(emsg, 
					"Automatically generated label number is too big.\r\n, we will use overflow prefix \'%s\'",
					lab);
 				ud_wrerr(emsg);
           }
/*			if  (UM_labelmdl.next[i] > 999999)
			{
				ud_wrerr("Automatically generated label number is too big.");
				return -1;
			} */
			sprintf(lab, "%s%d", UM_labelmdl.pf[i], UM_labelmdl.next[i]);
         }
      sprintf(label,"%-63s",lab);
      status = UU_SUCCESS;
      }

	uu_dexit;
	return (status);
	}

