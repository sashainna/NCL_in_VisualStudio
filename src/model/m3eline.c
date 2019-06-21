/*********************************************************************
**    NAME         :  m3eline.c
**       CONTAINS:
**			int um_drw2_line(ptr,tfmat,attrptr)
**			int um_p2_line(ptr)
**			int um_tr2_tranline(eptr,offset)
**			int um_rt2_rotline(eptr,rotpt,rotmat)
**			int um_sc2_scalline(eptr,scalpt,scalmat)
**			int um_tf2_tranfline(eptr,tranfmat,store)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3eline.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:54
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdattr.h"
#include "mcrv.h"
#include "mdeval.h"
#include "modef.h"
#include "mplot.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     : int um_drw2_line(ptr,tfmat,attrptr)
**       Display a line in the current segment.
**    PARAMETERS   
**       INPUT  : 
**				ptr     			pointer to line record
**				tfmat				transformation matrix
**				attrptr			pointer to attribute record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_drw2_line(ptr,tfmat,attrptr)
struct  UM_line_rec  *ptr;
UM_transf tfmat;
struct UM_attrdata_rec *attrptr;

{
	UM_coord spt;
	UM_coord ept;
	Gwpoint3 gpt[2];			/* points to send to GKS */

	uu_denter(UU_MTRC,(us,"um_drw2_line(key:%d,tfmat:%x,attrptr:%x)",
					ptr->key,tfmat,attrptr));

/*
.....Calling um_dash2_line sets the dashes too small for
.....the plotter large dash linetype.  This overides the
.....line style pattern that is set in the plotting routines.
.....JLS 8/25/99
	if ((UM_plotting) && (attrptr->line_style == UM_DASHED_LINE))
		um_dash2_line(ptr, tfmat, attrptr);
	else
*/
	um_set_disp_attr(attrptr);
	
	um_cctmtf(ptr->spt, tfmat, spt);
	um_cctmtf(ptr->ept, tfmat, ept);
	
	gpt[0].x = spt[0];
	gpt[0].y = spt[1];
	gpt[0].z = spt[2];
	gpt[1].x = ept[0];
	gpt[1].y = ept[1];
	gpt[1].z = ept[2];
	um_gpolyline3(attrptr, 2, gpt);

	uu_dexit;
	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int um_p2_line(ptr)
**       Print the contents of a line record.
**    PARAMETERS   
**       INPUT  : 
**				ptr			pointer to fixed data of line record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_p2_line(ptr)
	struct  UM_line_rec  *ptr;

	{

	uu_denter(UU_MTRC,(us,"um_p2_line(%8x)",ptr));
	sprintf(UM_sbuf, "LINE %d", ptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "label %7.7s", ptr->label);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PFLOAT, "spt", 3, ptr->spt); 
	um_p_ary(UM_PFLOAT, "ept", 3, ptr->ept); 
	umi_print_transf(ptr->key);

	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_tr2_tranline(eptr,offset)
**      Translate the specified line from the "from" point to the "to" point.
**    PARAMETERS   
**       INPUT  : 
**				eptr		pointer to the  entity to be translated
**			   offset	vector by which to translate
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tr2_tranline(eptr,offset)
	struct UM_line_rec *eptr;
	UM_vector   offset;

	{
	uu_denter( UU_MTRC,(us,"um_tr2_tranline(?,?)"));
    
   um_vcplvc(eptr->spt, offset, eptr->spt);
   um_vcplvc(eptr->ept, offset, eptr->ept);

	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_rt2_rotline(eptr,rotpt,rotmat)
**      Rotate the specified line by the given 4x3 matrix.
**    PARAMETERS   
**       INPUT  : 
**				eptr        pointer to the entity to be rotated
**          rotpt       point on rotation axis
**          rotmat      the 4x3 rotation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_rt2_rotline(eptr,rotpt,rotmat)
	struct UM_line_rec *eptr;
	UM_coord    rotpt;
   UM_transf    rotmat;
	{                                     

   UM_coord    temppt;           /* Temporary point */

	uu_denter( UU_MTRC,(us,"um_rt2_rotline(?,?,?)"));
    
   um_vcmnvc(eptr->spt, rotpt, temppt);          
   um_cctmtf(temppt, rotmat, temppt);
   um_vcplvc(temppt, rotpt, eptr->spt);

   um_vcmnvc(eptr->ept, rotpt, temppt);          
   um_cctmtf(temppt, rotmat, temppt);
   um_vcplvc(temppt, rotpt, eptr->ept);

	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_sc2_scalline(eptr,scalpt,scalmat)
**      Scale the specified line by the given 4x3 matrix.
**    PARAMETERS   
**       INPUT  : 
**				eptr          pointer to the entity to be scaled
**          scalpt       point to be scaled about
**          scalmat      the 4x3 scaling matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_sc2_scalline(eptr,scalpt,scalmat)

	struct UM_line_rec *eptr;
	UM_coord    scalpt;
	UM_transf    scalmat;

	{                                     
   UM_coord    temppt;           /* Temporary point */

	uu_denter( UU_MTRC,(us,"um_sc2_scalline(?,?,?)"));
    
   um_vcmnvc(eptr->spt, scalpt, temppt);          
   um_cctmtf(temppt, scalmat, temppt);
   um_vcplvc(temppt, scalpt, eptr->spt);

   um_vcmnvc(eptr->ept, scalpt, temppt);          
   um_cctmtf(temppt, scalmat, temppt);
   um_vcplvc(temppt, scalpt, eptr->ept);

	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_tf2_tranfline(eptr,tranfmat,store)
**			Transform a line by applying the specified 4X3 transformation
**			and update UNIBASE iff store == UU_TRUE.
**    PARAMETERS   
**       INPUT  : 
**				eptr          pointer to the entity to be transformed
**          tranfmat      the 4x3 transformation matrix
**				store			  TRUE iff UNIBASE is to be updated here.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tf2_tranfline(eptr,tranfmat,store)
	struct UM_line_rec *eptr;
	UM_transf    tranfmat;
	UU_LOGICAL store;

	{                                     

	uu_denter(UU_MTRC,(us,"um_tf2_tranfline(key:%d,tfmat:%x,store:%d)",
					eptr->key, tranfmat,store));
    
   um_cctmtf(eptr->spt, tranfmat, eptr->spt);          
   um_cctmtf(eptr->ept, tranfmat, eptr->ept);          

	if (store)
		um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
