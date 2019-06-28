/*********************************************************************
**    NAME:  nuclone.c.
**       CONTAINS: NCL clone operations.
**
**       int ncli_clone
**       int nclu_clone_translate
**       int nclu_clone_rotate
**       int nclu_clone_scale
**       int nclu_clone_mirror
**       int nclu_clone_matrix
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuclone.c , 25.1 
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:04 
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "modef.h"
#include "mdpick.h"
#include "mdebug.h"

#include "nccs.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "nclcmd.h"
#include "nclmodals.h"
#include "nclfc.h"

#define TRACE UU_TRUE /* for debugging only */

static char at_mx1[] = "@MX1";
extern char SSPLIN_CANNOT_BE_MOVED[];

static struct UC_entitydatabag e1;

/*********************************************************************
**    E_FUNCTION: ncli_clone(cmdbuf, mx_name)
**       For each entity in the current select buffer, call NCL to
**       clone_move the entity by the specified matrix. (mx_name)
**    PARAMETERS   
**       INPUT  : 
**          cmdbuf         buffer to put clone statements in   
**          mx_name        name of matrix to be used by clone statements
**       OUTPUT :  
**          cmdbuf         buffer clone statements are loaded in
**    RETURNS: none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncli_clone(cmdbuf, mx_name)
   NCL_cmdbuf *cmdbuf;
   char *mx_name;

   {
   UU_LOGICAL initialize;
   UU_LOGICAL ncl_legal_relation();
   char str[256];
   int status;
/*
.....A flag to insure that the user isn't trying to clone
.....matrices.
*/
	int matrixflag =0;

   uu_denter(UU_MTRC,(us,"ncli_clone()"));

   initialize = UU_TRUE;
   while(ud_gnxt(initialize, UU_NULL, &e1.key, 1) == UU_TRUE)
      {                                              
      initialize = UU_FALSE;
      if (uc_retrieve_data(&e1, sizeof(e1)) == UU_SUCCESS) 
         {
         if (ncl_legal_relation(e1.rel_num))
            {
				if (e1.rel_num!=81)
					{
					matrixflag=1;
            	ncl_init_cmdbuf(cmdbuf);
            	status = NCL_OKINPUT;

            	if (!NCL_auto_label)
               	status = ncl_add_name(cmdbuf, 1);

            	if (status == NCL_OKINPUT)
               	status = ncl_add_token(cmdbuf, NCL_clone, NCL_nocomma);
        
            	if (status == NCL_OKINPUT)
               	{
               	ncl_get_label(&e1, str);
               	status = ncl_add_token(cmdbuf, str, NCL_comma);
               	}

            	if (status == NCL_OKINPUT)
               	status = ncl_add_token(cmdbuf, mx_name, NCL_nocomma);
        
            	if ((status == NCL_OKINPUT) || (status == NCL_DONE))
               	{
               	ncl_add_cmdbuf(cmdbuf);
               	ncl_call(cmdbuf);
               	}
					}
            }
         }
      }
/*
.....If matrixflag is still 0 then the user was trying to clone
.....a matrix and an error will be displayed.
*/
	if (matrixflag==0)
		uu_uerror0(UA_DRAFTING, 10);
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION: nclu_clone_translate(option)
**       Prompt the user to pick NCL geometric entities to translate
**       along a vector or between two points.
**    PARAMETERS   
**       INPUT  : 
**          option               0 => between two points
**                               1 => along a vector
**       OUTPUT :  
**          none
**    RETURNS: none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_clone_translate(option)
   int option;

   {
   NCL_cmdbuf cmdbuf;

/**UM_coord from;
   UM_coord to; **/
   UD_NDCLOCREC from, to;

   UM_vector offset;
   UM_transf tfmat;
   int numint,status;

   uu_denter(UU_MTRC,(us,"nclu_clone_translate()"));

   if (option == 0)
      {
      ud_ldas(UD_DASCART, /*from point: */UM_MODEL, 72, &from, 1, 
               &numint, UD_NODEFAULT);
      if (numint <= 0)  goto done;

      ud_ldas(UD_DASCART, /*to point: */UM_MODEL, 73, &to, 1, 
               &numint, UD_NODEFAULT);
      if (numint <= 0)  goto done;

      ncl_wcstomcs (0, &from, &from);
      ncl_wcstomcs (0, &to, &to);
      um_vcmnvc(&to,&from,offset);
      }
   else
      {
      ud_ldas(UD_DASVEC,UM_MODEL,49,offset,1,&numint,UD_NODEFAULT);
      ncl_wcstomcs (1, offset, offset);
      if (numint <= 0)  goto done;
      }
   unitcv(offset);
   um_disptf(offset, tfmat);

   ncl_init_cmdbuf(&cmdbuf);
   ncl_add_temp_matrix(&cmdbuf, tfmat);

/*    have user pick entities to translate */
/*    created a new mask for move and clone. kathy */

   ud_lgeo(UU_TRUE, UD_ncl_mvclngeom);
   ud_ldas(UD_DASSELECT, /*pick entities to copy/translate:*/UM_MODEL, 78, 
            UU_NULL, 0, &numint,UD_NODEFAULT);
   if (numint <= 0) goto done;
/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) status = ncl_no_ssplin_comp_selected (UU_NULL);
   if (status != UU_SUCCESS)
   {
      ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
      goto done;
   }

   ncli_clone(&cmdbuf, at_mx1);

done:
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION: nclu_clone_rotate(option)
**       Prompt the user for the NCL geometric entities to rotate.
**    PARAMETERS   
**       INPUT  : 
**          option                  0 => rotate around vector
**                                  1 => align two vectors
**       OUTPUT :  
**          none
**    RETURNS: none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_clone_rotate(option)
   int option;

   {
   NCL_cmdbuf cmdbuf;

/**UM_coord rotpt;**/ 
   UD_NDCLOCREC rotpt;                  /* point defining axis of rotation */
 
   UM_vector rotvec;                   /* direction vector of axis */
   UM_angle ang;                       /* angle of rotation */
   UM_transf rotmat;                   /* rotation matrix */
   int  numint,status;
   UM_vector vec1,vec2;                /* vectors defining rotation */
   UU_REAL signtest;
   UM_vector rotpt_to_origin;
   UM_transf tempmat;

   uu_denter(UU_MTRC,(us,"nclu_clone_rotate(%d)",option));

   um_vctovc(UM_cpln.origin,&rotpt);
   ud_ldas(UD_DASCART, /*rotation axis point: */UM_MODEL, 75, &rotpt, 
          1, &numint, UD_DEFAULT);
   if (numint <= 0) goto done;
   ncl_wcstomcs (0, &rotpt, &rotpt);

   if (option == 0)
      {
      um_vctovc( UM_cpln.zaxis,rotvec);
      ud_ldas(UD_DASVEC, /*axis direction vector */UM_MODEL, 76, rotvec, 
             1, &numint, UD_DEFAULT);
      if (numint <= 0)  goto done;
      ncl_wcstomcs (1, rotvec, rotvec);

      ud_ldas(UD_DASANGLE, /*angle of rotation*/UM_MODEL, 77, &ang, 
             1, &numint, UD_NODEFAULT);
      if (numint <= 0)  goto done;
      }
   else
      {
      ud_ldas(UD_DASVEC, UM_MODEL, 113, vec1, 1, &numint, UD_NODEFAULT);
            /* message is: Enter a vector to be rotated */
      if (numint <= 0)  goto done;
      ncl_wcstomcs (1, vec1, vec1);

      ud_ldas(UD_DASVEC,UM_MODEL, 114, vec2, 1, &numint, UD_NODEFAULT);
         /* message is: Enter direction of vector after rotation */
      if (numint <= 0)  goto done;
      ncl_wcstomcs (1, vec2, vec2);

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
         ncl_wcstomcs (1, rotvec, rotvec);
         }
      else
         {
         um_cross(vec1,vec2,rotvec);
         ang = um_angle(vec1,vec2);
         }
      }

   um_rottf(rotvec, ang, rotmat);
   um_vctmsc(&rotpt, -1.0, rotpt_to_origin);
   unitcv(rotpt_to_origin);
   um_disptf(rotpt_to_origin, tempmat);
   um_tftmtf(tempmat, rotmat, rotmat);
   unitcv(&rotpt);
   um_disptf(&rotpt, tempmat);
   um_tftmtf(rotmat, tempmat, rotmat);

   ncl_init_cmdbuf(&cmdbuf);
   ncl_add_temp_matrix(&cmdbuf, rotmat);

/*    have user pick entities to rotate */
/*    created a new mask for move and clone. kathy */

   ud_lgeo(UU_TRUE, UD_ncl_mvclngeom);
   ud_ldas(UD_DASSELECT, /*pick entities to copy/rotate: */UM_MODEL, 79,
         UU_NULL, 0, &numint,UD_NODEFAULT);
   if (numint <= 0) goto done;
/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) status = ncl_no_ssplin_comp_selected (UU_NULL);
   if (status != UU_SUCCESS)
   {
      ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
      goto done;
   }

   ncli_clone(&cmdbuf, at_mx1);

done:
   uu_dexit;
   }

/**************************************************************************
**    E_FUNCTION: nclu_clone_scale()
**       Prompt the user for the NCL geometric entities to scale.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS:  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
nclu_clone_scale()

   {
   NCL_cmdbuf cmdbuf;
   int  numint,status;                     /* number of entities picked */
   UU_KEY_ID key;                   /* Local  entity template */

/**UM_coord scalpt;**/
   UD_NDCLOCREC scalpt;                 /* point to scale about */

   UU_REAL scale[3];                /* scale ratio */
   UM_transf scalmat;               /* scaling matrice */
   UM_vector scalpt_to_origin;
   UM_transf tempmat;

   uu_denter(UU_MTRC,(us,"nclu_scale()"));

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
   ncl_wcstomcs (0, &scalpt, &scalpt);

   scale[1] = scale[0];
   scale[2] = scale[0];
   um_scaletf(scale, scalmat) ;
   um_vctmsc(&scalpt, -1.0, scalpt_to_origin);
   unitcv(scalpt_to_origin);
   um_disptf(scalpt_to_origin, tempmat);
   um_tftmtf(tempmat, scalmat, scalmat);
   unitcv(&scalpt);
   um_disptf(&scalpt, tempmat);
   um_tftmtf(scalmat, tempmat, scalmat);

   ncl_init_cmdbuf(&cmdbuf);
   ncl_add_temp_matrix(&cmdbuf, scalmat);

/*    have user pick entities to scale */
/*    created a new mask for move and clone. kathy */

   ud_lgeo(UU_TRUE, UD_ncl_mvclngeom);
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
      goto done;
   }

   ncli_clone(&cmdbuf, at_mx1);

done:
   uu_dexit;
   }
 
/**************************************************************************
**  E_FUNCTION:  nclu_clone_mirror()
**       Prompt the user for the NCL geometric entities to mirror.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS:  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
nclu_clone_mirror()

   {
   NCL_cmdbuf cmdbuf;
   int  numint,status;              /* number of entities picked */

/**UM_coord mirrpt; **/
   UD_NDCLOCREC mirrpt;             /* point on mirror plane */

   UM_vector mirrnorm;              /* mirror plane normal */
   UM_vector xaxis;                 /* x-axis direction vector */
   UM_vector rotvec;                /* rotation axis vector */
   UM_angle rotangle;               /* angle of rotation */
   UM_transf mirrmat;               /* mirroring matrice */
   UM_transf rotmat;                /* rotate back matrice */
   UM_vector mirrpt_to_origin;
   UM_transf tempmat;               /* temporary matrix */

   uu_denter(UU_MTRC,(us,"nclu_clone_mirror()"));

   /* get plane normal */
   ud_ldas(UD_DASVEC,UM_MODEL,99,mirrnorm,1,&numint,UD_NODEFAULT);
   if (numint <= 0) goto done;

   ud_ldas(UD_DASCART, /*Point on mirror plane: */UM_MODEL, 100, &mirrpt, 
            1, &numint, UD_NODEFAULT);
   if (numint <= 0) goto done;

   ncl_wcstomcs(1, mirrnorm, mirrnorm);
   ncl_wcstomcs(0, &mirrpt, &mirrpt);
   um_unitvc(mirrnorm, mirrnorm);
   xaxis[0] = 1.0;
   xaxis[1] = xaxis[2] = 0.0;
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
    * the x-components of the mirror matix are negated, then the
    * normal to the plane is rotated back to its original
    * orientation by multiplying the two matrices.  The mirror
    * matrix resulting will be used to mirror relations */
   um_rottf(rotvec, rotangle, mirrmat) ;
   mirrmat[0][0] = -mirrmat[0][0];
   mirrmat[0][1] = -mirrmat[0][1];
   mirrmat[0][2] = -mirrmat[0][2];
   um_rottf(rotvec, -rotangle, rotmat) ;
   um_tftmtf(rotmat, mirrmat, mirrmat) ;
   um_vctmsc(&mirrpt, -1.0, mirrpt_to_origin);
   unitcv(mirrpt_to_origin);
   um_disptf(mirrpt_to_origin, tempmat);
   um_tftmtf(tempmat, mirrmat, mirrmat);
   unitcv(&mirrpt);
   um_disptf(&mirrpt, tempmat);
   um_tftmtf(mirrmat, tempmat, mirrmat);

   ncl_init_cmdbuf(&cmdbuf);
   ncl_add_temp_matrix(&cmdbuf, mirrmat);

/*    have user pick entities to mirror */
/*    created a new mask for move and clone. kathy */

   ud_lgeo(UU_TRUE, UD_ncl_mvclngeom);
   ud_ldas(UD_DASSELECT, /*pick entities to mirror:*/UM_MODEL, 98, UU_NULL, 
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
      goto done;
   }

   ncli_clone(&cmdbuf, at_mx1);
   
done:
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION: nclu_clone_matrix()
**       Prompt the user to pick NCL geometric entities to translate
**       via a named matrix   
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS: none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_clone_matrix()

   {
   NCL_cmdbuf cmdbuf;
   UM_transf tfmat;
   int  numint, status,stat;
   char mx_name[16];

   uu_denter(UU_MTRC,(us,"nclu_clone_matrix()"));

/*    get name of matrix to transform by */
   status = ncl_get_str(mx_name, 416);

/*    have user pick entities to translate */
/*    created a new mask for move and clone. kathy */

   ud_lgeo(UU_TRUE, UD_ncl_mvclngeom);
   ud_ldas(UD_DASSELECT, /*pick entities to copy/translate:*/UM_MODEL, 78, 
            UU_NULL, 0, &numint,UD_NODEFAULT);
   if (numint <= 0) goto done;
/*
..... aak 15-apr-1998: check if among the selected entities there is
..... composite curve made of CVonSF's and give error if so.
*/
   if(numint > 0) stat = ncl_no_ssplin_comp_selected (UU_NULL);
   if (stat != UU_SUCCESS)
   {
      ud_wrerr(SSPLIN_CANNOT_BE_MOVED);
      goto done;
   }

   if (status == NCL_OKINPUT) ncli_clone(&cmdbuf, mx_name);

done:
   uu_dexit;
   }
