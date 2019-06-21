/*********************************************************************
**    NAME:  c1uclass.c
**       CONTAINS:
**
**			int ucu_delete
**			int ucu_delete_all_disp
**			int ucu_translate
**			int ucu_rotate
**			int ucu_scale
**			int ucu_mirror
**			int ucu_copy_scale
**			int ucu_copy_translate
**			int ucu_copy_rotate
**			int ucu_copy_mirror
**			ucu_print
**			int ucu_drag_translate
**			int ucu_drag_copy_translate
**			int ucu_query
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       c1uclass.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:57
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "ulist.h"
#include "mdgenent.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mdcpln.h"	/* for UM_cpln, etc */
#include "modef.h"	/* for UM_PI, etc */
#include "mdpick.h"
#include "dmark.h"
#include "mdebug.h"
#include "mdunits.h"
#include "mdraw.h"
#include "dselmask.h"
#include "rbase.h"	/* for UR_REL_NAME */
#include <math.h>

UU_LOGICAL	ud_gnxt(), ud_lyesno();

static struct UC_entitydatabag e1;
static struct UC_entitydatabag e2;

char SSPLIN_CANNOT_BE_MOVED[] = 
	"Composite curve containig surface splines cannot be moved";

/*********************************************************************
**    E_FUNCTION     : ucu_delete()
**       Delete entities.
**    PARAMETERS   
**       INPUT  :          none
**       OUTPUT :          none
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ucu_delete()
	{
	int uc_delete();
	int uc_deleteExceptions();
	int uc_deleteIfNecessary();
	int numint;
	int key;
	int initialize1 = UU_TRUE;
	int initialize2 = UU_TRUE;
	char *list = UU_NULL;
	UU_LOGICAL cantUndel;
	int i, status = UU_SUCCESS;
	int rel_num;
	
	uu_denter(UU_MTRC,(us,"ucu_delete()"));

/*	-- Limit DAS to only editable entities -- */

	ud_leditable(UU_TRUE);
	ud_ldas(UD_DASSELECT, /*pick entities to delete*/UM_MODEL, 68, UU_NULL, 
			 1, &numint,UD_NODEFAULT);
	if (numint != 0)
		{
		ur_enable_del_stack() ;	/* enable the delete stack	*/
		while(ud_gnxt(initialize1, UU_NULL, &key, 1) == UU_TRUE)
			{
			initialize1 = UU_FALSE;

			if (ur_test_tuple(key)) continue;

			um_retrieve_data_relnum(key, &rel_num); /* accounts for ROM. entity */
			if (rel_num != UM_DRAWING_REL)
				{
				if (uc_getKeyList(initialize2,key,uc_deleteExceptions,&cantUndel,
										&list) != UU_SUCCESS) goto failed;
				uu_dprint(UU_MTRC,(us,"from ucu_delete: list:%x, cantUndel:%d",
												list,cantUndel));
				initialize2 = UU_FALSE;
				if (cantUndel) continue;
				if (uc_delete(key) != UU_SUCCESS)
					goto failed;
				}
			}
		ur_disable_del_stack() ;	/* disable the delete stack	*/
		if (uc_processKeyList(list, uc_delete) != UU_SUCCESS)
				goto failed;
		}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ucu_delete", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ucu_delete_all_disp()
**       Delete displayed entities.
**    PARAMETERS   
**       INPUT  :          none
**       OUTPUT :          none
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ucu_delete_all_disp()
	{
	int uc_delete();
	int uc_deleteExceptions();
	int uc_deleteIfNecessary();
	int numint; 
	UU_KEY_ID key; 
	char *list = UU_NULL;
	UU_LOGICAL initialize = UU_TRUE;
	UU_LOGICAL editable, blanked, cantUndel, firstim = UU_TRUE;
	int i, status = UU_SUCCESS;
	int rel_num;
	
	uu_denter(UU_MTRC,(us,"ucu_delete_all_disp()"));

	if (ud_lyesno(UM_MODEL, 304))
		{

		/* set DIGS into defered update mode */
		uv_set_defered_mode();
	
		ur_enable_del_stack(); /* enable delete stack */
		while (uv_getobjs(initialize, &key, UU_TRUE) == 0)
			{
			initialize = UU_FALSE;
			um_retrieve_data_relnum(key, &rel_num);
			ur_retrieve_blanked(key, &blanked);
			ur_retrieve_editability(key, &editable);
			if ((rel_num != UM_DRAWING_REL) && (blanked == UU_FALSE) &&
					(editable == UU_TRUE))
				{
				if (uc_getKeyList(firstim,key,uc_deleteExceptions,&cantUndel,&list) 
						!= UU_SUCCESS) goto failed;
				firstim = UU_FALSE;
				if (cantUndel) continue;
				if (uc_delete(key) != UU_SUCCESS)
					{
					status = UU_FAILURE;
					break;
					}
				}
			}
		ur_disable_del_stack()	;  /* disable the delete stack	*/
	
		/* delete the entities we can't undelete */
		if (uc_processKeyList(list, uc_delete) != UU_SUCCESS)
				goto failed;
	
		/* delete features */
		um_feareset();
	
		/* reset DIGS into immediate update mode */
		uv_set_immediate_mode();
		goto done;
		}
failed:;
done:;
	uu_dexitstatus("ucu_delete_all_disp", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int ucu_translate(option)
**			Prompt the user for the entities to translate and the translation 
**			offset, and call uc_translate for each entity.
**    PARAMETERS   
**       INPUT  : 
**				option					0 => between two points
**											1 => along a vector
**       OUTPUT :  
**          none
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ucu_translate(option)
	int option;
	{
	int  numpicks;             /* number of entities picked */
/** UM_coord from; **/					/* from point */
/**	UM_coord to;**/	                  	/* to point */
    UD_NDCLOCREC from, to;

	UM_vector offset;				/* vector to move along */
	int  numint;
	UU_LOGICAL initialize;		/*initialize get next entity */
	int status = UU_SUCCESS;
/*
** Get the entity information and the translation offset, and call 
** uc_translate for each one.
*/
	uu_denter(UU_MTRC,(us,"ucu_translate()"));

/*	-- Limit DAS to only editable entities -- */

	ud_leditable(UU_TRUE);
	ud_lgeo(UU_TRUE, UD_translate);

	ud_ldas(UD_DASSELECT, /*pick entities to translate:*/UM_MODEL, 71, 
				UU_NULL, 0, &numint,UD_NODEFAULT);
/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) status = ncl_no_ssplin_comp_selected (UU_NULL);
	if (status != UU_SUCCESS)
	{
		ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
		goto failed;
	}	

	numpicks = numint;
	if (option == 0)
		{
		if (numint > 0) 
			ud_ldas(UD_DASCART, /*from point: */UM_MODEL, 72, &from, 1, 
						&numint, UD_NODEFAULT);
		if (numint > 0) 
			ud_ldas(UD_DASCART, /*to point: */UM_MODEL, 73, &to, 1, 
						&numint, UD_NODEFAULT);
		if (numint > 0) um_vcmnvc(&to,&from,offset);
		}
	else
		ud_ldas(UD_DASVEC,UM_MODEL,49,offset,1,&numint,UD_NODEFAULT);

	if (numint > 0)
		{
		if (ur1_beginBatch() != UU_SUCCESS) goto failed;
		initialize = UU_TRUE;
		while(ud_gnxt(initialize, UU_NULL, &e1.key, 1) == UU_TRUE)
	   	{                                              
			initialize = UU_FALSE;
		   if (uc_retrieve_data(&e1, sizeof(e1)) == UU_SUCCESS) 
				{
				if (uc_translate(&e1, offset) == UU_SUCCESS)
					{
	 	   		if (uc_display(&e1) != UU_SUCCESS)
						{
						status = UU_FAILURE;
						}
					}
				else
					{
					status = UU_FAILURE;
					}
				}
			else
				{
				status = UU_FAILURE;
				}
  	   	}/* end while */
		if (ur1_endBatch() != UU_SUCCESS) goto failed;
		}
	goto done;
failed: status = UU_FAILURE;
done:;
	ud_lgeo(UU_FALSE, UD_translate);
	uu_dexitstatus("ucu_translate", status);
	return(status);
	}
/*********************************************************************
**    E_FUNCTION: int ucu_rotate(option)
**      Prompt the user for the entities to rotate and the rotation parameters,
**      and call um_rotate_geom for each entity.
**    PARAMETERS   
**       INPUT  : 
**				option						0 => rotate around vector
**												1 => align two vectors
**       OUTPUT :  
**          none
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ucu_rotate(option)
	int option;
	{
/**	UM_coord rotpt;	**/
    UD_NDCLOCREC rotpt;                         /* point defining axis of rotation */

	UM_vector rotvec;							/* direction vector of axis */
	UM_angle ang;								/* angle of rotation */
   UM_transf rotmat;							/* rotation matrix */
   int  numpicks;     	               /* number of entities picked */
	int  numint;
	UU_LOGICAL initialize;					/* initialize get next entity */
	UM_vector vec1,vec2;						/* vectors defining rotation */
	UU_REAL signtest;
	UM_vector rotpt_to_origin;
	UM_transf tempmat;
	int status,bskey;

/*------------------------------------------------------------------------
** Get the entity information and the rotation parameters, and call 
** uc_rotate for each one.
*/
	uu_denter(UU_MTRC,(us,"ucu_rotate(%d)",option));
	status = UU_SUCCESS;

/*	-- Limit DAS to only editable entities -- */

	ud_leditable(UU_TRUE);
	ud_lgeo(UU_TRUE, UD_rotate);
	ud_ldas(UD_DASSELECT, /*pick entities to rotate: */UM_MODEL, 74, UU_NULL, 
			 0, &numint,UD_NODEFAULT);
/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) status = ncl_no_ssplin_comp_selected (UU_NULL);
   if (status != UU_SUCCESS)
   {
      ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
      goto failed;
   }

	numpicks = numint;
	if (numint > 0) 
		{
		um_vctovc(UM_cpln.origin,&rotpt);
		ud_ldas(UD_DASCART, /*rotation axis point: */UM_MODEL, 75, &rotpt, 
				 1, &numint, UD_DEFAULT);
		}
	if (option == 0)
		{
		if (numint > 0) 
			{
			um_vctovc( UM_cpln.zaxis,rotvec);
			ud_ldas(UD_DASVEC, /*axis direction vector */UM_MODEL, 76, rotvec, 
					 1, &numint, UD_DEFAULT);
			} 
		if (numint > 0) 
			ud_ldas(UD_DASANGLE, /*angle of rotation*/UM_MODEL, 77, &ang, 
					 1, &numint, UD_NODEFAULT);
		}
	else
		{
		if (numint > 0)
			ud_ldas(UD_DASVEC, UM_MODEL, 113, vec1, 1, &numint, UD_NODEFAULT);
				/* message is: Enter a vector to be rotated */
		if (numint > 0)
			ud_ldas(UD_DASVEC,UM_MODEL, 114, vec2, 1, &numint, UD_NODEFAULT);
				/* message is: Enter direction of vector after rotation */
		if (numint > 0)
			{
			um_unitvc(vec1,vec1);
			um_unitvc(vec2,vec2);
			
			if (um_vcparall(vec1,vec2))
				{
				if (um_cceqcc(vec1,vec2)) /* the net effect is no rotation */
					ang = 0.0;
				else /* these two vectors must go in opposite directions */
					ang = UM_PI;  
				ud_ldas(UD_DASVEC,UM_MODEL, 115, rotvec, 1, &numint, UD_NODEFAULT);
					/* message is: Enter rotation axis */
				}
			else
				{
				um_cross(vec1,vec2,rotvec);
				ang = um_angle(vec1,vec2);
				}
			}
		}
	if (numint > 0)
		{
		um_rottf(rotvec, ang, rotmat);
		um_vctmsc(&rotpt, (UU_REAL) -1.0, rotpt_to_origin);
		um_disptf(rotpt_to_origin, tempmat);
		um_tftmtf(tempmat, rotmat, rotmat);
		um_disptf(&rotpt, tempmat);
		um_tftmtf(rotmat, tempmat, rotmat);

		if (ur1_beginBatch() != UU_SUCCESS) goto failed;
		initialize = UU_TRUE;
		while(ud_gnxt(initialize, UU_NULL, &e1.key, 1) == UU_TRUE)
			{
			initialize = UU_FALSE;
			if (uc_retrieve_data(&e1, sizeof(e1)) == UU_SUCCESS)
				{
				if (uc_rotate(&e1, &rotpt, rotvec, ang, rotmat) == UU_SUCCESS)
					{
					if (uc_display(&e1) != UU_SUCCESS)
						status = UU_FAILURE;
					}
				else
					status = UU_FAILURE;
				}
			else
				status = UU_FAILURE;
			}
		if (ur1_endBatch() != UU_SUCCESS) goto failed;
		}
	goto done;
failed: status = UU_FAILURE;
done:;
	ud_lgeo(UU_FALSE, UD_rotate);
	uu_dexitstatus("ucu_rotate", status);
	return(status);
	}

/**************************************************************************
**    E_FUNCTION: int ucu_scale()
**      Prompt the user for the entities to scale and the point to scale about
**      and call uc_scale  for each  entity.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS:  UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int ucu_scale()
	{
	int  numint;							/* number of entities picked */
	UU_KEY_ID key;							/* Local  entity template */
/**	UM_coord scalpt;**/
    UD_NDCLOCREC scalpt;                        /* point to scale about */

	UU_REAL scale[3];						/* scale ratio */
	UM_transf scalmat;					/* scaling matrice */
	UM_vector scalpt_to_origin;
	UM_transf tempmat;
	UU_LOGICAL initialize;				/* initialize get next  entity */
	int status = UU_SUCCESS;

/*------------------------------------------------------------------------
** Get the entity information and the scale plane, and call uc_scale
** for each one.
*/
	uu_denter(UU_MTRC,(us,"ucu_scale()"));

	/* have user pick entities to scale */

/*	-- Limit DAS to only editable entities -- */

	ud_leditable(UU_TRUE);
	ud_lgeo(UU_TRUE, UD_scale);
	ud_ldas(UD_DASSELECT, /*pick entities to scale:*/UM_MODEL, 101, UU_NULL, 
			 0, &numint,UD_NODEFAULT);
	if (numint <= 0) goto done;
/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) status = ncl_no_ssplin_comp_selected (UU_NULL);
   if (status != UU_SUCCESS)
   {
      ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
      goto failed;
   }


	/* Scale ratio: */
newscale:
	ud_ldas(UD_DASUNITLESS,UM_MODEL,103,&scale[0],1,&numint,UD_NODEFAULT);
	if (numint <= 0) goto done;
	if (scale[0] < UM_FUZZ)
		{
		uu_uerror0(/* scale factor must be positive */UM_MODEL, 227);
		goto newscale;
		}

	/* get point to scale about */
	ud_ldas(UD_DASCART, /*Point for scale origin: */UM_MODEL, 
				102, &scalpt, 1, &numint, UD_NODEFAULT);
	if (numint <= 0)  goto done;

	scale[1] = scale[0];
	scale[2] = scale[0];
	um_scaletf(scale, scalmat) ;
	um_vctmsc(&scalpt, (UU_REAL) -1.0, scalpt_to_origin);
	um_disptf(scalpt_to_origin, tempmat);
	um_tftmtf(tempmat, scalmat, scalmat);
	um_disptf(&scalpt, tempmat);
	um_tftmtf(scalmat, tempmat, scalmat);

	/* access the entities to be scaled */
	if (ur1_beginBatch() != UU_SUCCESS) goto failed;
	initialize = UU_TRUE;
	while(ud_gnxt(initialize, UU_NULL, &e1.key, 1) == UU_TRUE)
		{                                              
		initialize = UU_FALSE;
		if (uc_retrieve_data(&e1, sizeof(e1)) == UU_SUCCESS)
			{
			if (uc_scale(&e1, &scalpt, scalmat, scale[0]) == UU_SUCCESS)
				{
				if (uc_display(&e1) != UU_SUCCESS)
					status = UU_FAILURE;
				}
			else
				status = UU_FAILURE;
			}
		else
			status = UU_FAILURE;
		}
	if (ur1_endBatch() != UU_SUCCESS) goto failed;
	goto done;
failed: status = UU_FAILURE;
done:
	ud_lgeo(UU_FALSE, UD_scale);
	uu_dexitstatus("ucu_scale", status);
	return(status);
	}

/**************************************************************************
**  E_FUNCTION:  int ucu_mirror()
**      Prompt the user for the entities to mirror and the mirror plane,
**      and call uc_mirror  for each  entity.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS:  UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int ucu_mirror()
	{
	int  numint;							/* number of entities picked */
	int iflg;
/**	UM_coord mirrpt;**/
    UD_NDCLOCREC mirrpt;                        /* point on mirror plane */

	UM_vector mirrnorm;					/* mirror plane normal */
	UM_vector xaxis;						/* x-axis direction vector */
	UM_vector rotvec;						/* rotation axis vector */
	UM_angle rotangle;					/* angle of rotation */
	UM_transf mirrmat;					/* mirroring matrice */
	UM_transf rotmat;						/* rotate back matrice */
	UM_vector mirrpt_to_origin;
	UM_transf tempmat, mcsmat;					/* temporary matrix */
	UU_LOGICAL initialize;				/* initialize get next  entity */
	int status = UU_SUCCESS;

/*------------------------------------------------------------------------
** Get the entity information and the mirror plane, and call um_mirror_geom
** for each one.
*/
	uu_denter(UU_MTRC,(us,"ucu_mirror()"));

	/* have user pick entities to mirror */

/*	-- Limit DAS to only editable entities -- */

	ud_leditable(UU_TRUE);
	ud_lgeo(UU_TRUE, UD_mirror);
	ud_ldas(UD_DASSELECT, /*pick entities to mirror:*/UM_MODEL, 98, UU_NULL, 
						0, &numint,UD_NODEFAULT);
/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
	if(numint > 0) status = ncl_no_ssplin_comp_selected (UU_NULL);
	if (status != UU_SUCCESS)
	{
		ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
		goto failed;
	}

	if (numint > 0)
		{
		/* get plane normal */
		ud_ldas(UD_DASVEC,UM_MODEL,99,mirrnorm,1,&numint,UD_NODEFAULT);
		if (numint > 0)
			{
			ud_ldas(UD_DASCART, /*Point on mirror plane: */UM_MODEL, 100, &mirrpt, 
						1, &numint, UD_NODEFAULT);
			if (numint > 0) 
				{
				xaxis[0] = 1.0 ;
				xaxis[1] = 0.0 ;
				xaxis[2] = 0.0 ;
				ncl_wcstomcs(1, mirrnorm, mirrnorm);
				ncl_wcstomcs(0, &mirrpt, &mirrpt);
				um_unitvc(mirrnorm, mirrnorm);
				if (um_vcparall(mirrnorm, xaxis) != UU_TRUE)
					{
					um_cross(xaxis, mirrnorm, rotvec);
					rotangle = um_angle(mirrnorm, xaxis);
					}
				else
					{
					rotvec[0] = rotvec[1] = 0.0;
					rotvec[2] = 1.0;
					rotangle = 0.0 ;
					}
				/* normal of mirror plane is rotated to parallel to the x-axis,
				 *	the x-components of the mirror matix are negated, then the
				 *	normal to the plane is rotated back to its original
				 *	orientation by multiplying the two matrices.  The mirror
				 *	matrix resulting will be used to mirror relations */
				um_rottf(rotvec, rotangle, mirrmat) ;
				mirrmat[0][0] = -mirrmat[0][0];
				mirrmat[0][1] = -mirrmat[0][1];
				mirrmat[0][2] = -mirrmat[0][2];
				um_rottf(rotvec, -rotangle, rotmat) ;
				um_tftmtf(rotmat, mirrmat, mirrmat) ;
				um_vctmsc(&mirrpt, (UU_REAL) -1.0, mirrpt_to_origin);
				um_disptf(mirrpt_to_origin, tempmat);
				um_tftmtf(tempmat, mirrmat, mirrmat);
				um_disptf(&mirrpt, tempmat);
				um_tftmtf(mirrmat, tempmat, mirrmat);
				ncl_mcstf(&iflg, mcsmat);
				if (iflg == 1) um_inverttf(mcsmat, tempmat);
	
				/* access the entities to be mirrored */
				if (ur1_beginBatch() != UU_SUCCESS) goto failed;
				initialize = UU_TRUE;
				while(ud_gnxt(initialize, UU_NULL, &e1.key, 1) == UU_TRUE
						&& status == UU_SUCCESS)
					{                                              
					initialize = UU_FALSE;
					if (uc_retrieve_data(&e1, sizeof(e1)) != UU_SUCCESS)
						status = UU_FAILURE;
					if (status == UU_SUCCESS && iflg == 1)
						if (uc_transform (&e1, tempmat, UU_FALSE) != UU_SUCCESS)
							status = UU_FAILURE;
					if (status == UU_SUCCESS)
						if (uc_mirror(&e1, &mirrpt, mirrnorm, mirrmat) != UU_SUCCESS)
							status = UU_FAILURE;
					if (status == UU_SUCCESS && iflg == 1)
						if (uc_transform (&e1, mcsmat, UU_TRUE) != UU_SUCCESS)
							status = UU_FAILURE;
					if (status == UU_SUCCESS)
						if (uc_display(&e1) != UU_SUCCESS)
							status = UU_FAILURE;
					}/* end while */
				if (ur1_endBatch() != UU_SUCCESS) goto failed;
				}
			}
		}	
	goto done;
failed: status = UU_FAILURE;
done:;
	ud_lgeo(UU_FALSE, UD_mirror);
	uu_dexitstatus("ucu_mirror", status);
	return(status);
	}
 
/**************************************************************************
**  E_FUNCTION:  int ucu_copy_scale()
**      Prompt the user for the entities to scale and the point to scale about,
**      and call  uc_copy and  uc_scale  for each  entity.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS:  UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int ucu_copy_scale()
	{
	int  numint;							/* number of entities picked */
/**	UM_coord scalpt;**/
    UD_NDCLOCREC scalpt;                        /* point on scale plane */

	UU_REAL scale[3];						/* scale plane normal */
	UM_transf scalmat;					/* scaling matrice */
	UM_vector scalpt_to_origin;
	UM_transf tempmat;
	UU_LOGICAL initialize;			/* um_initialize get next  entity */
	struct UC_entitydatabag *currentptr, *newptr;
	int i;
	int copies;
	int status;

/*------------------------------------------------------------------------
** Get the entity information and the scale parameters, and call um_scalcurve
** and um_copy_geom for each one.
*/
	uu_denter(UU_MTRC,(us,"ucu_copy_scale()"));
	status = UU_SUCCESS;	/* assume success */

	/* have user pick entities to scale */
	ud_lgeo(UU_TRUE, UD_copyscale);
	ud_ldas(UD_DASSELECT, /*pick entities to scale:*/UM_MODEL, 101, UU_NULL, 
						0, &numint,UD_NODEFAULT);
	if (numint <= 0) goto done;
/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) status = ncl_no_ssplin_comp_selected (UU_NULL);
   if (status != UU_SUCCESS)
   {
      ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
      goto failed;
   }

	/* enter point to scale around */
	um_vctovc(UM_cpln.origin, &scalpt);
	ud_ldas(UD_DASCART, /*Point for scale origin: */UM_MODEL, 102, &scalpt, 
					1, &numint, UD_DEFAULT);
	if (numint <= 0) goto done;

	/* Scale factor: */
newscale:
	ud_ldas(UD_DASUNITLESS,UM_MODEL,103,&scale[0],1,&numint,UD_NODEFAULT);
	if (numint > 0)
		{
		if (scale[0] < UM_FUZZ) 
			{
			uu_uerror0(/* scale factor must be positive */ UM_MODEL, 227);
			goto newscale;
			}

		copies = 1;
		ud_ldas(UD_DASINT, UM_MODEL, 285, &copies, 1, &numint,
						UD_DEFAULT);
		/* prompt is: Enter number of scaled copies of each entity */
		if (copies > 1024)
			if (!ud_yesno(0, uu_uprompt1(UB_SYMBOL, 27, copies), "Question?"))
				/* prompt is: Number of copies to be created is %d, ok? */
				goto done;

		/* form scaling matrix */
		scale[1] = scale[0];
		scale[2] = scale[0];
		um_scaletf(scale, scalmat);
		um_vctmsc(&scalpt, (UU_REAL) -1.0, scalpt_to_origin);
		um_disptf(scalpt_to_origin, tempmat);
		um_tftmtf(tempmat, scalmat, scalmat);
		um_disptf(&scalpt, tempmat);
		um_tftmtf(scalmat, tempmat, scalmat);

		initialize = UU_TRUE;
		while(ud_gnxt(initialize, UU_NULL, &e1.key, 1) == UU_TRUE)
			{                                              
			initialize = UU_FALSE;
			currentptr = &e1;			
			newptr = &e2;
			for (i=0; i<copies; i++) /* copy entity number of times requested */
				{
				if (uc_retrieve_data(currentptr, sizeof(e1))
					!= UU_SUCCESS) goto failed;
			
				if (uc_copy(currentptr, newptr, sizeof(e1))
					!= UU_SUCCESS) goto failed;

				if (uc_scale(newptr, &scalpt, scalmat, scale[0]) != UU_SUCCESS)
					goto failed;
					
				if (uc_display(newptr) != UU_SUCCESS)
					goto failed;

				currentptr = newptr;
				newptr = (currentptr == &e1) ? &e2 : &e1;
				}
			}
		}
failed:;
done:
	ud_lgeo(UU_FALSE,UD_copyscale);
	uu_dexitstatus("ucu_copy_scale", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int ucu_copy_translate(option)
**      Prompt the user for the entities to copy and the translation offset,
**      and call  uc_copy and  uc_translate for each entity.
**    PARAMETERS   
**       INPUT  : 
**				option					0 => between two points
**											1 => along a vector
**       OUTPUT :  
**          none
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ucu_copy_translate(option)
	int option;
	{
	int  numpicks;                      /* number of entities picked */
/**	UM_coord from;**/							/* from point */
/**	UM_coord to;**/								/* to point */
    UD_NDCLOCREC from, to;

	UM_vector offset;							/* direction to move along */
	int copies;
	int  numint;
	int status;

/*------------------------------------------------------------------------
** Get the  entity information and the translation offset, and call um_copy_geom
** and  um_translate_geom  for each one.
*/
	uu_denter(UU_MTRC,(us,"ucu_copy_translate(option:%d)", option));
	status = UU_SUCCESS;	/* assume success */
	copies = 1;

	ud_lgeo(UU_TRUE, UD_copytranslate);
	ud_ldas(UD_DASSELECT, /*pick entities to copy/translate:*/UM_MODEL, 78, 
				UU_NULL, 0, &numint,UD_NODEFAULT);

/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) status = ncl_no_ssplin_comp_selected (UU_NULL);
   if (status != UU_SUCCESS)
   {
      ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
      goto failed;
   }

	numpicks = numint;

	if (numint > 0)
	if (option == 0)
	  	{
		ud_ldas(UD_DASINT, UM_MODEL, 261, &copies, 1, &numint, UD_DEFAULT);
		/* prompt is: Enter number of translation copies of each entity */

		if (copies > 1024)
			if (!ud_yesno(uu_uprompt1(UB_SYMBOL, 27, copies), "Question?"))
				/* prompt is: Number of copies to be created is %d, ok? */
				goto done;
/*
.....
.....Commented by Paul to fix FSR 51187. 09/25/92
.....
        if (numint > 0)
*/
			ud_ldas(UD_DASCART, /*from point: */UM_MODEL, 72, &from, 1, 
						&numint, UD_NODEFAULT);
		if (numint > 0) 
		while (UU_TRUE)
		  	{
			ud_ldas(UD_DASCART, /*to point: */UM_MODEL, 73, &to, 1, 
				 &numint, UD_NODEFAULT);
			 if (numint <= 0) 	goto done;
			 else
					{
					um_vcmnvc(&to,&from,offset);
					if (uci_copy_trans(offset,copies)==UU_FAILURE)
						goto failed;
					}
			 
		   	}
	  	}
	else
	  	{
		ud_ldas(UD_DASVEC,UM_MODEL,49,offset,1,&numint,UD_NODEFAULT);
		ud_ldas(UD_DASINT, UM_MODEL, 261, &copies, 1, &numint, UD_DEFAULT);
		/* prompt is: Enter number of translation copies of each entity */

		if (copies > 1024)
			if (!ud_yesno(0, uu_uprompt1(UB_SYMBOL, 27, copies), "Question?"))
				/* prompt is: Number of copies to be created is %d, ok? */
				goto done;
	   if (uci_copy_trans(offset,copies)==UU_FAILURE)
			 goto failed;
	  	}
	goto done;

failed: status = UU_FAILURE;
done:;
	ud_lgeo(UU_FALSE, UD_copytranslate);
	uu_dexitstatus("ucu_copy_translate", status);
	return(status);
	}


/*********************************************************************
**    I_FUNCTION: int uci_copy_tran(offset,copies)
**      call  uc_copy and  uc_translate for each entity to copy translate. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uci_copy_trans(offset,copies)
UM_vector offset;							/* direction to move along */
int copies;

	{	
	struct UC_entitydatabag *currentptr, *newptr;
	int i;
	UU_LOGICAL initialize;					/* initialize get next entity */
	int status;
	UM_vector newoffset;
	UU_REAL scalar;

	uu_denter(UU_MTRC,(us,"enter uci_copy_trans"));
	if (ur1_beginBatch() != UU_SUCCESS) goto failed;
	initialize = UU_TRUE;
	status = UU_SUCCESS;	/* assume success */
	while(ud_gnxt(initialize, UU_NULL, &e1.key, 1) == UU_TRUE)
	{                                              
		initialize = UU_FALSE;
		currentptr = &e1;			
		newptr = &e2;
		for (i=0; i<copies; i++) /* copy entity number of times requested */
		{
/*
.....The vector offset needs to be scaled accordingly for when user
.....has choosen to make multiple copies.
*/
      	scalar=i+1.0;
        	um_vctmsc(offset,scalar,newoffset);
			if (uc_retrieve_data(currentptr, sizeof(e1)) == UU_SUCCESS)
			{
				if (uc_copy(currentptr, newptr, sizeof(e1)) == UU_SUCCESS)
				{
					if (uc_translate(newptr, newoffset) == UU_SUCCESS)
					{
						if (uc_display(newptr) != UU_SUCCESS)
						{
							status = UU_FAILURE;
						}
					}
					else status = UU_FAILURE;
				}
				else status = UU_FAILURE;
			}
			else status = UU_FAILURE;
		}
		currentptr = newptr;
		newptr = (currentptr == &e1) ? &e2 : &e1;
	}
	if (ur1_endBatch() != UU_SUCCESS) goto failed;
	goto done;

failed: status = UU_FAILURE;
done:   
	uu_dexit;
   return(status);
	}

/*********************************************************************
**    E_FUNCTION: int ucu_copy_rotate() 
**      Prompt the user for the entities to copy and the rotation parameters,
**      and call  uc_copy and  uc_rotate for each entity.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ucu_copy_rotate()
	{
/**	UM_coord rotpt;**/
    UD_NDCLOCREC rotpt;                         /* point to rotate around */

	UM_vector rotvec;							/* direction vector of axis */
	UM_angle angle;							/* angle of rotation */
  UM_transf rotmat;      	      	   /* rotation matrix */
  int  numpicks;                      /* number of entities picked */
	int  numint;
	UM_vector rotpt_to_origin;
	UM_transf tempmat;
	int copies;
	int i;
	struct UC_entitydatabag *currentptr, *newptr;
	UU_LOGICAL initialize;					/* initialize get next entity */
	int status;

/*------------------------------------------------------------------------
** Get the entity information and the rotation parameters, and call  crcurve
** for each one.
*/
	uu_denter(UU_MTRC,(us,"ucu_copy_rotate()"));
	status = UU_SUCCESS;	/* assume success */
	ud_lgeo(UU_TRUE, UD_copyrotate);
	ud_ldas(UD_DASSELECT, /*pick entities to copy/rotate:*/UM_MODEL, 79, 
				UU_NULL, 0, &numint,UD_NODEFAULT);

/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) status = ncl_no_ssplin_comp_selected (UU_NULL);
   if (status != UU_SUCCESS)
   {
      ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
      goto failed;
   }

	numpicks = numint;
	if (numint > 0) 
		{
		um_vctovc(UM_cpln.origin, &rotpt);
		ud_ldas(UD_DASCART, /*rotation axis point: */UM_MODEL, 75, &rotpt, 
				 1, &numint, UD_DEFAULT);
		}
	if (numint > 0) 
		{
		um_vctovc(UM_cpln.zaxis, rotvec);
		ud_ldas(UD_DASVEC, /*rotation direction vector: */UM_MODEL, 80, rotvec, 
				 1, &numint, UD_DEFAULT);
		}
	if (numint > 0) 
	   ud_ldas(UD_DASANGLE, /*angle of rotation*/UM_MODEL, 48, &angle, 
				 1, &numint, UD_NODEFAULT);
	if (numint > 0)
		{
		copies = 1;
		ud_ldas(UD_DASINT, UM_MODEL, 262, &copies, 1, &numint,
						UD_DEFAULT);
		/* prompt is: Enter number of rotation copies of each entity */

		if (copies > 1024)
			if (!ud_yesno(0, uu_uprompt1(UB_SYMBOL, 27, copies), "Question?"))
				/* prompt is: Number of copies to be created is %d, ok? */
				goto done;

		um_rottf(rotvec, angle, rotmat);
		um_vctmsc(&rotpt, (UU_REAL) -1.0, rotpt_to_origin);
		um_disptf(rotpt_to_origin, tempmat);
		um_tftmtf(tempmat, rotmat, rotmat);
		um_disptf(&rotpt, tempmat);
		um_tftmtf(rotmat, tempmat, rotmat);
		initialize = UU_TRUE;
		while(ud_gnxt(initialize, UU_NULL, &e1.key, 1) == UU_TRUE)
			{                                              
			initialize = UU_FALSE;
			currentptr = &e1;
			newptr = &e2;
			for (i=0; i<copies; i++)
				{
				if (uc_retrieve_data(currentptr, sizeof(e1))
					!= UU_SUCCESS) goto failed;
			
				if (uc_copy(currentptr, newptr, sizeof(e1)) 
					!= UU_SUCCESS) goto failed;
				
				if (uc_rotate(newptr, &rotpt, rotvec, angle, rotmat) 
					!= UU_SUCCESS) goto failed;
					
				if (uc_display(newptr) != UU_SUCCESS)
					goto failed;

				currentptr = newptr;
				newptr = (currentptr == &e1) ? &e2 : &e1;
				}
			}
		}

	goto done;
failed: status = UU_FAILURE;
done:;
	ud_lgeo(UU_FALSE, UD_copyrotate);
	uu_dexitstatus("ucu_copy_rotate", status);
	return(status);
	}

/**************************************************************************
**  E_FUNCTION:  int ucu_copy_mirror()
**      Prompt the user for the entities to copy and the mirror plane,
**      and call  uc_copy  and  uc_mirror  for each  entity.
**  COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int ucu_copy_mirror()
	{
	int  numint;							/* number of entities picked */
	int  iflg;
/**	UM_coord mirrpt;	**/
    UD_NDCLOCREC mirrpt;                        /* point on mirror plane */

	UM_vector mirrnorm;					/* mirror plane normal */
	UM_vector xaxis;						/* x-axis direction vector */
	UM_vector rotvec;						/* rotation axis vector */
	UM_angle rotangle;					/* angle of rotation */
	UM_transf mirrmat;					/* mirroring matrice */
	UM_transf rotmat;						/* rotate back matrice */
	UM_vector mirrpt_to_origin;
	UM_transf tempmat, mcsmat;
	UU_LOGICAL initialize;			/* initialize get next  entity */
	int status;
/*------------------------------------------------------------------------
** Get the entity information and the mirror plane, and call um_mirror_geom
** for each one.
*/
	uu_denter(UU_MTRC,(us,"ucu_copy_mirror()"));
	status = UU_SUCCESS;	/* assume success */
	/* have user pick entities to mirror */
	ud_lgeo(UU_TRUE, UD_copymirror);
	ud_ldas(UD_DASSELECT, /*pick entities to mirror:*/UM_MODEL, 98, UU_NULL, 
						0, &numint,UD_NODEFAULT);
/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) status = ncl_no_ssplin_comp_selected (UU_NULL);
   if (status != UU_SUCCESS)
   {
      ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
      goto failed;
   }

	if (numint > 0)
		{

		/* Mirror plane normal: */
		ud_ldas(UD_DASVEC,UM_MODEL,99,mirrnorm,1,&numint,UD_NODEFAULT);
	
		if (numint > 0)
			{
			ud_ldas(UD_DASCART, /*Point on mirror plane: */UM_MODEL, 
						100, &mirrpt, 1, &numint, UD_NODEFAULT);
			if (numint > 0) 
				{
				ncl_wcstomcs(1, mirrnorm, mirrnorm);
				ncl_wcstomcs(0, &mirrpt, &mirrpt);
				xaxis[0] = 1.0;
				xaxis[1] = xaxis[2] = 0.0;
				um_unitvc(mirrnorm, mirrnorm);
				if (um_vcparall(mirrnorm, xaxis) != UU_TRUE)
					{
					um_cross(xaxis, mirrnorm, rotvec);
					rotangle = um_angle(mirrnorm, xaxis);
					}
				else
					{
					rotvec[0] = rotvec[1] = 0.0;
					rotvec[2] = 1.0;
					rotangle = 0.0 ;
					}
	
				/* normal of mirror plane is rotated to parallel to the x-axis,
					the x-components of the mirror matix are negated, then the
					normal to the plane is rotated back to its original
					orientation by multiplying the two matrices.  The mirror
					matrix resulting will be used to mirror relations */
				um_rottf(rotvec, rotangle, mirrmat) ;
				mirrmat[0][0] = -mirrmat[0][0];
				mirrmat[0][1] = -mirrmat[0][1];
				mirrmat[0][2] = -mirrmat[0][2];
				um_rottf(rotvec, -rotangle, rotmat) ;
				um_tftmtf(rotmat, mirrmat, mirrmat) ;
				um_vctmsc(&mirrpt, (UU_REAL) -1.0, mirrpt_to_origin);
				um_disptf(mirrpt_to_origin, tempmat);
				um_tftmtf(tempmat, mirrmat, mirrmat);
				um_disptf(&mirrpt, tempmat);
				um_tftmtf(mirrmat, tempmat, mirrmat);
				ncl_mcstf(&iflg, mcsmat);
				if (iflg == 1) um_inverttf(mcsmat, tempmat);
	
				/* access the entities to be mirrored */
				initialize = UU_TRUE;
				while(ud_gnxt(initialize, UU_NULL, &e1.key, 1) == UU_TRUE
						&& status == UU_SUCCESS)
					{                                              
					initialize = UU_FALSE;
					if (uc_retrieve_data(&e1, sizeof(e1)) != UU_SUCCESS)
						status = UU_FAILURE;
					if (status == UU_SUCCESS)
						if (uc_copy(&e1, &e2, sizeof(e2)) != UU_SUCCESS)
							status = UU_FAILURE;
					if (status == UU_SUCCESS && iflg == 1)
						if (uc_transform (&e2, tempmat, UU_FALSE) != UU_SUCCESS)
							status = UU_FAILURE;
					if (status == UU_SUCCESS)
						if (uc_mirror(&e2, &mirrpt, mirrnorm, mirrmat) != UU_SUCCESS)
							status = UU_FAILURE;
					if (status == UU_SUCCESS && iflg == 1)
						if (uc_transform (&e2, mcsmat, UU_TRUE) != UU_SUCCESS)
							status = UU_FAILURE;
					if (status == UU_SUCCESS)
						if (uc_display(&e2) != UU_SUCCESS)
							status = UU_FAILURE;
					}
				}
			}
		}

failed:;
	ud_lgeo(UU_FALSE, UD_copymirror);
	uu_dexitstatus("ucu_copy_mirror", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ucu_print(option)
**       Print an entity either by having the user pick a displayed
**			entity or by entering an MTID of an entity.
**    PARAMETERS   
**       INPUT  : 
**          option				1 => pick from display
**										2 => enter MTID
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ucu_print(option)
	int option;

	{
	UM_PICKENT pent;
	int numint;

	uu_denter(UU_MTRC,(us, "ucu_print(option=%d)",option));

	while (UU_TRUE)
		{
		if (option  ==  1)
			{
			um_dl_pdas(UD_DASPICK, /*pick entity*/UM_MODEL, 153, &pent, 
							1, &numint, 1);
			if (numint > 0) e1.key = um_get_pickkey(&pent, 1);
			}
		else if (option  ==  2)
			{
			ud_ldas(UD_DASINT, /*enter MTID*/UM_MODEL, 69, &e1.key, 
						1, &numint,UD_NODEFAULT);
			}
		else goto done;
		if (numint <= 0) goto done;
		if (uc_retrieve_data(&e1, sizeof(e1)) != UU_SUCCESS)
			goto failed;
		if (uc_print(&e1) != UU_SUCCESS) goto failed;
		goto repeat;
failed:
repeat:;
			}
done:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION: int ucu_drag_translate()
**			Prompt the user for the entities to translate and the translation 
**			offset, and drag each entity
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ucu_drag_translate()
	{
	int  numpicks;             /* number of entities picked */
  	UM_coord temp;					/* temporary point */
  	UM_coord from;					/* from point */
/*	UM_coord to;*/					/* to point */
    UD_NDCLOCREC to;

	UM_vector offset;				/* vector to move along */
	int  numint;
	int stat, status = UU_SUCCESS;
	UD_RUBBER drag;				/* dragging control block */
	UD_PLOCREC stuff;				/* thing to drag */
	UM_PICKENT modstuff;			/* pickresolve record */
	int um_d_pickresolve();
	int markval;					/* mark return cell */
	int bskey;

/*
** Get the entity information and the translation offset, and call 
** uc_translate for each one.
*/
	uu_denter(UU_MTRC,(us,"ucu_drag_translate()"));

/*	-- get the entity to drag and start point -- */


/*	-- Limit DAS to only editable entities -- */

	ud_leditable(UU_TRUE);
	ud_lgeo(UU_TRUE, UD_translate);
	ud_ldas(UD_DASPCKLOC, /*pick entities to xlate*/UM_MODEL, 
						71, &stuff, 1, &numint, UD_NODEFAULT);
	if(numint > 0)
	{
/*
..... aak 15-apr-1998: check if it's composite curve made of CVonSF's
..... and give error if it is.
*/
		status = ncl_no_ssplin_comp_selected (&stuff);
		if (status != UU_SUCCESS)
		{
			ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
			goto failure;
		}

/*
.....get the key id form pickpath
*/
		status = um_d_pickresolve(&stuff,1,&modstuff);
		e1.key = um_get_pickkey(&modstuff, 1);
   	status = uc_retrieve_data(&e1, sizeof(e1));
		if (status != UU_SUCCESS) goto failure;
/*
.....convert "from" point to point on construction plane
*/
		uv_ndctocc(stuff.pndc.cord, temp, stuff.pndc.transform);
		status = uv_projcpln(temp, from, stuff.pndc.transform);
		if (status != UU_SUCCESS) goto failure;

/*
.....start the drag and input "to" point
*/
		DRAG_GRAPHICS(&drag, stuff.ppath.pickpath[0], from,
						stuff.pndc.transform);
		DRAG_ON(&drag);

		ud_ldas(UD_DASCART, /*to point: */UM_MODEL, 73, &to, 1, 
				&numint, UD_NODEFAULT);
		DRAG_OFF(&drag);

		if (numint > 0)
		{
			um_vcmnvc(&to , from, offset);

			if (uc_translate(&e1, offset) == UU_SUCCESS)
	   		if (uc_display(&e1) != UU_SUCCESS) status = UU_FAILURE;
			else
						status = UU_FAILURE;
		}
	}
failure:

	ud_lgeo(UU_FALSE, UD_translate);
	uu_dexitstatus("ucu_drag_translate", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int ucu_drag_copy_translate()
**      Prompt the user for the entities to copy and drag the entity,
**      and call  uc_copy and  uc_translate for each entity.
**    PARAMETERS   
**       INPUT  : 
**				none
**
**       OUTPUT :  
**          none
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ucu_drag_copy_translate()
	{
   int  numpicks;                      /* number of entities picked */
	UM_coord temp;								/* temporary point */
	UM_coord from;								/* from point */
/*	UM_coord to;*/								/* to point */
    UD_NDCLOCREC to;

	UM_vector offset;							/* direction to move along */
	int  numint;
	int stat, status;
	UD_RUBBER drag;				/* dragging control block */
	UD_PLOCREC stuff;				/* thing to drag */
	UM_PICKENT modstuff;			/* pickresolve record */
	int um_d_pickresolve();

/*------------------------------------------------------------------------
** Get the  entity information and the translation offset, and call um_copy_geom
** and  um_translate_geom  for each one.
*/
	uu_denter(UU_MTRC,(us,"ucu_drag_copy_translate()"));
	status = UU_SUCCESS;	/* assume success */

/*	-- get the entity to drag and start point -- */

	ud_lgeo(UU_TRUE, UD_copytranslate);
	ud_ldas(UD_DASPCKLOC, /*pick entities to xlate*/UM_MODEL, 
						71, &stuff, 1, &numint, UD_NODEFAULT);
/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) status = ncl_no_ssplin_comp_selected (&stuff);
   if (status != UU_SUCCESS)
   {
      ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
      goto failure;
   }

	if(numint > 0)
		{
/*		-- convert "from" point to w.c. -- */

		uv_ndctocc(stuff.pndc.cord, temp, stuff.pndc.transform);
		status = uv_projcpln(temp, from, stuff.pndc.transform);
		if (status != UU_SUCCESS) goto failure;

/*		-- start the drag and input "to" point -- */

		DRAG_GRAPHICS(&drag, stuff.ppath.pickpath[0], from,
						stuff.pndc.transform);
		while(numint > 0)
			{
			DRAG_ON(&drag);

			ud_ldas(UD_DASCART, /*to point: */UM_MODEL, 73, &to, 1, 
						&numint, UD_NODEFAULT);
			if (numint > 0)
				{
				um_vcmnvc(&to , from, offset);

/*			-- get the key id form pickpath -- */

				status = um_d_pickresolve(&stuff,1,&modstuff);
				e1.key = um_get_pickkey(&modstuff, 1);
				if (uc_retrieve_data(&e1, sizeof(e1)) == UU_SUCCESS)
					{
  	     		 	if (uc_copy(&e1, &e2, sizeof(e2)) == UU_SUCCESS)
						{
  	      			if (uc_translate(&e2, offset) == UU_SUCCESS)
							{
							if (uc_display(&e2) != UU_SUCCESS)
								{
								status = UU_FAILURE;
								}
							}
						else
							{
							status = UU_FAILURE;
							}
						}
					else
						{
						status = UU_FAILURE;
						}
					}
				else
					{
					status = UU_FAILURE;
					}
				}
			}
		DRAG_OFF(&drag);
		}

failure:
	ud_lgeo(UU_FALSE, UD_copytranslate);
	uu_dexitstatus("ucu_drag_copy_translate", status);
	return(status);
	}

/********************************************************************* 
**    I_FUNCTION :  ucu_query(flag)
**			Interactively select and display the UniBase data
**			for a piece of geometry.
**
**    PARAMETERS   
**       INPUT  : flag;
**       OUTPUT : none 
**
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ucu_query(flag)
int flag;
{
	int nsels;
   UU_KEY_ID key;
	UM_PLOCREC pick;
	int rows = 50;
	int status;
	UU_LIST	list;
/*
.....Pick geometry for query data display
*/
	um_dl_pldas(UD_DASPCKLOC, UJ_SUPPORT, 16, &pick, 1, &nsels, 1);
	if (nsels <= 0) goto done;
/*
.....Get the key from the pick record
*/
	key = um_get_pickkey(&pick.pent, 1);
/*
.....Display data based on data type
*/
	uj_init_query(&list, rows);
	status = UU_SUCCESS;
/*
.....Get additional information
.....if Entity Data
*/
	if (flag == 1) status = uc_query(key, &list);
	if (status == UU_SUCCESS) uju_qry_display(key,&list,flag);
	uj_free_list(&list);
done:;
	return(status);
}
