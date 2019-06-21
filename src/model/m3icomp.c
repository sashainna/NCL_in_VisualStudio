
/*********************************************************************
**    NAME         :  m3icomp.c
**       CONTAINS:
**			umi_fix_subcurve_fields
**			umi_appcompcrv
**			um_flip 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3icomp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:56
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "nccs.h"

/*********************************************************************
**    I_FUNCTION :  umi_fix_subcurve_fields(comptr, tfmat)
**      		Fix the subcurve ids, set the subcurve directions with respect
**				to the composite curve, set the "endparam" field for each subcurve,
**				and set the arclen of the composite.
**	
**    PARAMETERS   
**       INPUT  : 
**          comptr		pointer to the composite curve
**				tfmat			transformation for composite curve.
**       OUTPUT :  
**          comptr		pointer to the updated composite curve data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umi_fix_subcurve_fields(comptr, tfmat)
	struct UM_compcrv_rec *comptr;
	UM_transf tfmat;

	{
	UU_REAL length;
	int i;
/* 
... storage for constituent curves in * the compcrv 
*/
/*
... aak 10-nov-1997: replaced
	struct UC_entitydatabag cons;	
*/
   struct NCL_fixed_databag cons;
	UM_transf constfmat;			/* transformation for constituent */
	UM_transf inv_tfmat;			/* inverse transformation for composite */

	uu_denter(UU_MTRC,(us,"umi_fix_subcurve_fields(key:%d,tfmat:%x)",
					comptr->key,tfmat));

	/* put the length of the subcurve of the compcrv being constructed
 	 * into the "endparam" fields for later use. 
 	 */
	comptr->arclen = 0;
	for (i=0; i<comptr->no_cid; i++)
		{
		cons.key = abs( (int)comptr->cid[i].crvid );
/*
... aak 07-nov-1997: replaced
		uc_retrieve_data(&cons, sizeof(cons));
*/
		ncl_retrieve_data_fixed(&cons);
		uc_retrieve_transf(cons.key, constfmat);

		/* transform constituent entity geometry into the coordinate
		 * space of the (non-transformed) composite curve
		 */
		um_inverttf(tfmat, inv_tfmat);
		um_tftmtf(constfmat, inv_tfmat, constfmat);
		comptr->cid[i].endparam = um_getarclen(&cons, constfmat);

		comptr->arclen = comptr->arclen + comptr->cid[i].endparam;
		}

	length = 0.0;
	for (i=0; i<comptr->no_cid; i++) /* fix key's and "endparam" fields */
		{
		/* the following if-then-else causes the sense of the subcurve with
		 * respect to the composite curve to be stored.
		 */
		if (( (int)comptr->cid[i].crvid) < 0)
			{
			comptr->cid[i].reverse = UU_TRUE;
			comptr->cid[i].crvid = -comptr->cid[i].crvid;
			}
		else
			comptr->cid[i].reverse = UU_FALSE;	

		/* calculate the composite curve parameter value of the end points
		 * of the subcurves.
		 */
		length = length + comptr->cid[i].endparam;
		comptr->cid[i].endparam = length / comptr->arclen;
		}
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION : umi_appcompcrv
**						(compptr, comptfmat, consptr, constfmat, fpt, lpt)
**       Append a constituent curve to an existing composite curve.
**       Update the composite curve n-value and cids.
**    PARAMETERS   
**       INPUT  : 
**				compptr		pointer to the composite curve.
**				comptfmat	transformation for composite curve.
**				consptr   	pointer to the constituent to be appended.
**				constfmat 	transformation for constituent.
**       OUTPUT :  
**				compptr   pointer to the existing composite curve.   
**          fpt:      First point of the composite.
**          lpt:      Current end point of the composite.
**    RETURNS      : 
**			UU_FALSE if constituent and composite not connected; else UU_TRUE.
**    SIDE EFFECTS : Note, "constfmat" is changed to constfmat*(comptfmat)**-1
**    WARNINGS     : none
**			compptr->no_cid must be the current number of
**       constituents actually in the composite.  Must be set to
**       zero before the initial call with an empty composite.
*********************************************************************/
umi_appcompcrv(compptr, comptfmat, consptr, constfmat, fpt, lpt)
    struct UM_compcrv_rec *compptr;
	 UM_transf comptfmat;
    struct UM_crvdatabag *consptr;
	 UM_transf constfmat;
    UM_coord  fpt, lpt;

	{
    UM_coord  spt, ept;				/* Start and end points of the constituent */
    int j;								/* index */
	 UM_transf inv_comptfmat;		/* inverse transformation to "comptfmat" */
	 UM_transf subcrv_tfmat;		/* transformation associated with the 
												subcurve of a constituent composite 
												curve */
	 UU_REAL toler = 1.e-3;       /* tolerance for appending a curve to a composed curve */
/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
**
**		First: flip curves if backwards and set  fpt  and  lpt.
**		Then:  install cons key(s) into comp cid, and update comp n.
*/
	 uu_denter(UU_MTRC, (us, 
		"umi_appcompcrv(compkey:%d,comptfmat:%x,conskey:%d,constfmat:%x,%x,%x)",
			compptr->key,comptfmat,consptr->key,constfmat,fpt,lpt));


	 um_get_endpts(consptr,constfmat,spt,ept);
	 if (consptr->rel_num == UM_COMPCRV_REL)
	 	{
		struct UM_compcrv_rec *ptr;
		ptr = (struct UM_compcrv_rec *) consptr;
		/* for each subcurve of the constituent curve change its key to negative
		 * if its reversed in the constituent
		 */
		for (j=0; j<ptr->no_cid; j++)
			if (ptr->cid[j].reverse)
				ptr->cid[j].crvid = -ptr->cid[j].crvid;
	 	}

/*
..... aak, 9-8-97:
..... now uses bigger tolerance to decide whether a curve can be appended to a composite crv.
..... replaced calls to um_cceqcc() by um_cceqcc_tol(.,., toler);
..... in um_cceqcc() the tolerance is UM_FUZZ;
*/

   if (compptr->no_cid == 0)               /* first constituent */
   	{
        um_vctovc(spt, fpt);
        um_vctovc(ept, lpt);
   	}
   else     /* subsequent constituents */
   	{
		if (um_cceqcc_tol(spt, lpt,toler))       /* both curves OK */
			{                                              
         um_vctovc(ept, lpt);
			}
     else if (um_cceqcc_tol(ept, lpt,toler)) /* appended curve backwards */
     	{
         um_flip(consptr);
			um_vctovc(spt, lpt);
     	}
     else if (um_cceqcc_tol(spt, fpt, toler))  /* composite backwards */
     	{
         um_flip(compptr);
			um_vctovc(lpt, fpt);
         um_vctovc(ept, lpt);
     	}
     else if (um_cceqcc_tol(ept, fpt, toler))  /* both curves backwards */
     	{
         um_flip(compptr);
			um_flip(consptr);
			um_vctovc(lpt, fpt);
         um_vctovc(spt, lpt);
     	}
     else                        /* curves not connected */
			{
			uu_dexit;
         return(UU_FALSE);
			}
   	}
	 /* Install cons key(s), update comp n, and fix up transformations. */

	 /* get transformation that puts the constituent curve in the 
	  * (non-transformed) composite curve space.
	  */
	 um_inverttf(comptfmat, inv_comptfmat);
	 um_tftmtf(constfmat, inv_comptfmat, constfmat);

    if (consptr->rel_num ==  UM_COMPCRV_REL)
    	{    	/* This is the branch that prevents hierarchies  */
			struct UM_compcrv_rec *ptr;
			ptr = (struct UM_compcrv_rec *) consptr;

			/* for each subcurve of the constituent composite curve, get the right
			 * orientation within the new composite curve and change the 
			 * transformation associated with each subcurve so that it will 
			 * be positioned properly with the composite curve's transformation.
			 */
			for (j = 0; j < ptr->no_cid; j++) 
				{
					compptr->cid[compptr->no_cid + j].crvid = ptr->cid[j].crvid;
					/* update transformation for subcurve */
					/* Note, this guarantees that when the transformation for the
					 * composite is applied to this entity, it will position
					 * the entity correctly.
					 */
					uc_retrieve_transf(abs(ptr->cid[j].crvid), subcrv_tfmat);
					/* subcrv_tfmat puts the subcurve in model space;
					 * (subcrv_tfmat)*(constfmat) puts the subcurve in the
					 * composite curve space.
					 */
					um_tftmtf(subcrv_tfmat, constfmat, subcrv_tfmat);
					um_update_transformation(abs(ptr->cid[j].crvid), subcrv_tfmat);
				}
        compptr->no_cid += ptr->no_cid;
   	}
   else    /* single curve */
   	{
      compptr->cid[compptr->no_cid].crvid = consptr->key;
      compptr->no_cid += 1;
	    consptr->key = abs(consptr->key);    /* In case it was flipped */
      /* update transformation for constituent curve */
		 um_update_transformation(consptr->key, constfmat);
		 
   	}
   uu_dexit;
	 return(UU_TRUE);
	}

/*********************************************************************
**    I_FUNCTION     : um_flip(eptr)
**			Reverse a single or composite curve--change sign of key (single) or
**			switch cid[].crvid entries so that first is last and last is first;
**			Also switch sense of cid[].reverse (composite).
**    PARAMETERS   
**       INPUT  : 
**				eptr   pointer to the entity to be flipped. 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			When a single curve is flipped, its key is set negative.
**			As soon as the calling procedure uses the negative value,
**			it must reset it to  abs(key).
*********************************************************************/
um_flip(eptr)
	struct UM_crvdatabag *eptr;
	{
	UU_KEY_ID tempid;		/* temporary key - for switching */
	int	n;					/* temporary - for convenience */
	int	i;					/* index */

	uu_denter(UU_MTRC,(us,"um_flip(%d)",eptr->key));
	if (eptr->rel_num ==  UM_COMPCRV_REL)
		{
		struct UM_compcrv_rec *ptr;
		ptr = (struct UM_compcrv_rec *) eptr;

		n = ptr->no_cid;
		for (i = 0; i < (n+1)/2; i++)
			{
			/* switch subcurve id's */
			tempid = ptr->cid[i].crvid;
			ptr->cid[i].crvid = -ptr->cid[n-i-1].crvid;
			ptr->cid[n-i-1].crvid = -tempid;
			}
		}
	else		/* single curve */
		{
		eptr->key = -eptr->key;
		}
	uu_dexit;
	}
