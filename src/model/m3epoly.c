
/*********************************************************************
**    NAME         :  m3epoly.c
**       CONTAINS: support routines for polygon fill entity
**			int um_drw40_poly(eptr, tfmat, attrptr)
**			int um_p40_poly(ptr)
**			int um_cp40_copypoly(e1, e2, bagsize)
**			int um_tr40_tranpoly(eptr,offset)
**			int um_tf40_tranfpoly(eptr, basepoint, trform, store)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3epoly.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:54
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdebug.h"




/*********************************************************************
**    E_FUNCTION :  int um_drw40_poly(eptr, tfmat, attrptr)
**			draw polyfill region, using current fill color
**    PARAMETERS   
**       INPUT  : 
**				eptr    			pointer to poly record
**				tfmat				transformation matrix
**				attrptr			pointer to attribute record
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_drw40_poly(eptr,tfmat,attrptr)
	struct  UM_poly_rec  *eptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;
	{
	register int i;			/* index						*/
	Gwpoint3 *gpt;				/* points to sent to GKS */
	int		nvx;				/* holder for eptr->numvtx	*/
	UU_REAL	tmpt[3];

	uu_denter(UU_MTRC,(us,"um_drw40_poly(numpoints:%d,tfmat:%x,attrptr:%x)",
					eptr->numvtx,tfmat,attrptr));


	/* get array to pass to gpolyfill	*/
	nvx = eptr->numvtx;
	gpt = (Gwpoint3 *) uu_malloc( (nvx + 1) * sizeof(Gwpoint3));

	for ( i = 0; i < nvx; i++ )
		{

		um_cctmtf(eptr->vertex[i], tfmat, tmpt);
		gpt[i].x = tmpt[0];
		gpt[i].y = tmpt[1];
		gpt[i].z = tmpt[2];
		}

	/* complete polygon	*/
	um_cctmtf(eptr->vertex[0], tfmat, tmpt);
	gpt[nvx].x = tmpt[0];
	gpt[nvx].y = tmpt[1];
	gpt[nvx].z = tmpt[2];

	if (eptr->fcolor != -1)
		{
		gsfillcolor(eptr->fcolor);
		gfillarea3(nvx, gpt);
		}

	/* draw boundary, too */
	um_set_disp_attr(attrptr);
	gpolyline3(nvx + 1, gpt);

	uu_free(gpt);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_p40_poly(ptr)
**       Print the data defining a polyfill region
**    PARAMETERS   
**       INPUT  : 
**				ptr					pointer to polyfill region
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_p40_poly(ptr)
	struct  UM_poly_rec  *ptr;
	{
	int		i;
	int		nvx;

	uu_denter( UU_MTRC,(us,"um_p40_poly(key=%x)",ptr->key));

	nvx = ptr->numvtx;
	sprintf( UM_sbuf, "POLYFILL %d", ptr->key);
	um_pscroll(UM_sbuf);

	um_p_ary(UM_PINT, "color", 1, &(ptr->fcolor));
	um_p_ary(UM_PINT, "number of vertices", 1, &nvx);
	for (i = 0; i < nvx; i++)
		{
		um_p_ary(UM_PFLOAT, "vertex", 3, ptr->vertex[i]);
		}
	umi_print_transf(ptr->key);

	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION: int um_cp40_copypoly(e1, e2, bagsize)
**      Copy a polyfill region	
**    PARAMETERS   
**       INPUT  : 
**				e1			pointer to entity to be copied
**				bagsize	size of storage for entity.
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cp40_copypoly(e1, e2, bagsize)
	struct UM_poly_rec *e1;        /* The entity to translate */
  struct UM_poly_rec *e2;        /* New entity */
  int bagsize;
	{
	struct UM_attrdata_rec attrbag;
	int	nvx;
	int	i;

	uu_denter(UU_MTRC,(us,"um_cp40_copypoly(key=%x, bagsize=%d)",e1->key, bagsize));
                                            
	ur_setup_data(e1->rel_num, e2, bagsize);
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e2->label, "");
	e2->subscr = 0;

	e2->fcolor = e1->fcolor;
	nvx = e1->numvtx;
	e2->numvtx = nvx;
	for (i = 0; i < nvx; i++)
		{
		um_vctovc(e1->vertex[i], e2->vertex[i]);
		}

	um_get_disp_attr(e1->key, &attrbag);
	um_create_geom(e2, UM_DEFAULT_TF, &attrbag);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_tr40_tranpoly(eptr,offset)
**      Translate the specified polfill region by offset
**    PARAMETERS   
**       INPUT  : 
**				eptr		pointer to the  entity to be translated
**			   offset	vector by which to translate
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tr40_tranpoly(eptr,offset)
	struct UM_poly_rec *eptr;
	UM_vector   offset;

	{
	int	nvx;
	int	i;

	uu_denter( UU_MTRC,(us,"um_tr40_tranpoly(key=%x)", eptr->key));
    
	nvx = eptr->numvtx;
	for ( i = 0; i < nvx; i++)
		{
		um_vcplvc(eptr->vertex[i], offset, eptr->vertex[i]);
		}

	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_tf40_tranfpoly(eptr, basepoint, trform, store)
**      Transform the specified polygon by the given 4x3 matrix, 
**			conjugated by translation to basepoint.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be scaled
**          basepoint   point to be scaled about
**          trform      the 4x3 scaling matrix
**				store			if true, update_geom()
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tf40_tranfpoly(eptr, trform, store)

	struct UM_poly_rec *eptr;
   UM_transf    trform;
	UU_LOGICAL	store;
	{                                     
	int	nvx;
	int	i;

	uu_denter(UU_MTRC,(us,"um_tf40_tranfpoly(key=%x, store=%d)",
		eptr->key, store));

	nvx = eptr->numvtx;
	for ( i = 0; i < nvx; i++)
		{
		um_cctmtf(eptr->vertex[i], trform, eptr->vertex[i]);
		}

	if (store)
		um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
