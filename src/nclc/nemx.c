/*********************************************************************
**    NAME         :  nemx.c
**       CONTAINS: routines to handle NCL matrices
**       int ncl_matrix_to_nclmatrix(e, ncl)
**       int ncl_nclmatrix_to_matrix(ncl, e)
**       ncl_p_nclmatrix(ncl)
**       ncl_p87_matrix(eptr)
**       ncl_add_temp_matrix(cmdbuf, tfmat)
**       mx_color_reset (type)
**       mxcolc (nclkey,type,flag)
**       upmatr (key,axis)
**       gtmxds (buf,nclkey)
**       cctmtf (cci,tf,cco)
**       ncl_invert_matrix(mxin,mxout)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nemx.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:39
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "class.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdattr.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mdcoord.h"

#include "ncl.h"
#include "nclfc.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclvx.h"
#include "lcom.h"

static char MX_TRANS[3][NCL_MAX_LABEL];
static int MX_SUBSCR[3];
static int MXCOL[3] = {UM_CYAN, UM_DARKGREEN, UM_DARKRED};
static int MX_PREV_COL[3] = {UM_CYAN, UM_CYAN, UM_CYAN};

/*********************************************************************
**    E_FUNCTION     : int ncl_matrix_to_nclmatrix(e, ncl)
**       Convert a UNIBASEe matrix to an NCLI  matrix.
**    PARAMETERS   
**       INPUT  : 
**          e                 UNIBASE matrix entity
**       OUTPUT :  
**          ncl               buffer to place NCL matrix
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_matrix_to_nclmatrix(e, ncl)
   struct NCL_matrix_rec *e;
   struct NCLI_matrix_rec *ncl;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_matrix_to_nclmatrix(key=%x, ncl=%x)",
      e->key, ncl));

   status = UU_SUCCESS;
   ncl_uureal_to_real8(12, e->mat, ncl->mat);
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_nclmatrix_to_matrix(ncl, e)
**       Convert an NCL matrix to a UNICAD matrix.
**    PARAMETERS   
**       INPUT  : 
**          ncl                  buffer holding NCL matrix
**       OUTPUT :  
**          e                    UNICAD matrix entity
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_nclmatrix_to_matrix(ncl, e)
   struct NCLI_matrix_rec *ncl;
   struct NCL_matrix_rec *e;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_nclmatrix_to_matrix(ncl=%x, e=%x)",
      ncl, e));
   status = UU_SUCCESS;
   e->rel_num = NCL_MATRIX_REL;
   ncl_real8_to_uureal(12, ncl->mat, e->mat);
   e->dalen = ncl->dalen;
   ncl_real8_to_uureal(3, ncl->dbox, e->dbox);
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p_nclmatrix(ncl)
**       Print an NCLI matrix.
**    PARAMETERS   
**       INPUT  : 
**          ncl               buffer holding NCLI matrix.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_p_nclmatrix(ncl)
   struct NCLI_matrix_rec *ncl;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_p_nclmatrix(ncl=%x)",ncl));

   sprintf(UM_sbuf,"NCLMATRIX:");
   um_pscroll(UM_sbuf);
   for (i=0; i<3; i++)
      {
      sprintf(UM_sbuf,"  mat=(%g,%g,%g,%g)", ncl->mat[i][0],
         ncl->mat[i][1], ncl->mat[i][2], ncl->mat[i][3]);
      um_pscroll(UM_sbuf);
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p87_matrix(eptr)
**       Print an UNIBASE NCL matrix.
**    PARAMETERS   
**       INPUT  : 
**          eptr              entity holding NCL matrix.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_p87_matrix(eptr)
   struct NCL_matrix_rec *eptr;

   {
   int i;

   uu_denter(UU_MTRC,(us,"ncl_p87_matrix(key=%x)",eptr->key));

   sprintf(UM_sbuf,"MATRIX: %x", eptr->key);
   um_pscroll(UM_sbuf);

   sprintf(UM_sbuf, "label %s",eptr->label);
   um_pscroll(UM_sbuf);

   for (i=0; i<3; i++)
      {
      sprintf(UM_sbuf,"  mat=(%g,%g,%g,%g)", eptr->mat[i][0],
         eptr->mat[i][1], eptr->mat[i][2], eptr->mat[i][3]);
      um_pscroll(UM_sbuf);
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_add_temp_matrix(tfmat)
**       Add a temporary matrix definition to the command buffer.
**       It will always be called @MX1.
**    PARAMETERS   
**       INPUT  : 
**          tfmat                UNICAD transformation matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_temp_matrix(cmdbuf, tfmat)
   NCL_cmdbuf *cmdbuf;
   UM_transf tfmat;

   {
   int status;
   char str[256];

   uu_denter(UU_MTRC,(us,"ncl_add_temp_matrix(tfmat=%x)",
      tfmat));

   status = NCL_OKINPUT;

   sprintf(str, "@MX1=MX/%f,%f,%f,%f", tfmat[0][0], tfmat[1][0], tfmat[2][0],
      tfmat[3][0]);
   ncl_add_token(cmdbuf, str, NCL_comma);

   sprintf(str, "        %f,%f,%f,%f", tfmat[0][1], tfmat[1][1], tfmat[2][1],
      tfmat[3][1]);
   ncl_add_token(cmdbuf, str, NCL_comma);

   sprintf(str, "        %f,%f,%f,%f", tfmat[0][2], tfmat[1][2], tfmat[2][2],
      tfmat[3][2]);
   ncl_add_token(cmdbuf, str, NCL_nocomma);
   ncl_add_cmdbuf(cmdbuf);
   ncl_call(cmdbuf);

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : mxcolc (key,type,flag)
**          Set/reset color of matrix display when matrix is used      
**          as REFSYS, TRACUT or MODSYS. Fortran callable.
**    PARAMETERS
**       INPUT  :
**          key      Unibase key to matrix (flag = 1).
**          type     Matrix type (1 - REFSYS, 2 - TRACUT, 3 - MODSYS) 
**          flag     1 - to set attribute, 0 - reset to default.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
 mxcolc (key,type,flag)
  UU_KEY_ID *key;
  UM_int2 *type, *flag;
  {
   struct NCL_matrix_rec mx;
   struct UC_attributedatabag attr;
   int mt, color, status = UU_FAILURE;
/*
...reset color in any previous active MX
*/   
   color  = UM_DARKBLUE;
   mt     = *type;
   status = mx_color_reset(mt);
   if (*flag == 1) 
     {
      mx.key = *key;
      status = ur_retrieve_data_fixed(&mx);
/*
...select color of display
*/
      if (status == UU_SUCCESS)
        {
         strncpy (MX_TRANS[mt-1],mx.label,NCL_MAX_LABEL);
         color = MXCOL[mt-1];
        }
/*
...set color in matrix attribute bundle
*/
      if (*type != 0) MX_SUBSCR[mt-1] = mx.subscr;
      if (status == UU_SUCCESS) status = uc_retrieve_attr (mx.key,&attr);
      if (status == UU_SUCCESS)
        {
/*
.....save current matrix color
*/
         if (flag) 	MX_PREV_COL[mt-1] = attr.color;
         attr.color = color;
         status = ur_update_attr(&attr);
         uc_display (&mx);
        }
     } 
   uu_dexit;
  }
/*********************************************************************
**    E_FUNCTION     : mx_color_reset (type)
**          Reset matrix display color to previous or default after       
**          it was set due to using the matrix for any NCL translation.
**    PARAMETERS
**       INPUT  :
**          type     Matrix type (1 - REFSYS, 2 - TRACUT, 3 - MODSYS) 
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
mx_color_reset (type)
int type;
{
	struct UC_attributedatabag attr;
	struct NCL_fixed_databag e;
	int i, n1, color, rscol, status = UU_FAILURE;
	UM_int2 ifnd, ifl;
	UM_int4 nclsub;
	UU_KEY_ID nkey;

   UM_f77_str ncllabel;
   union { 
      double junk;
      char label[NCL_MAX_LABEL+1];
      } ncl;
/*
...get matrix name used for selected function 
*/   
   i     = type - 1;
   strncpy (ncl.label,MX_TRANS[i],NCL_MAX_LABEL);
   MX_TRANS[i][0] = '\0';
   color = MXCOL[i];
/*
.....set to previous matrix color 
*/
   rscol = MX_PREV_COL[type-1];
   n1 = strlen (ncl.label); if (n1 > NCL_MAX_LABEL) n1 = NCL_MAX_LABEL;
   ncl.label[n1] = '\0';
   UM_init_f77_str(ncllabel, ncl.label, NCL_MAX_LABEL);
   nclsub = MX_SUBSCR[i];
   MX_SUBSCR[i] = 0;
/*
...check if specified entity still exists in Unibase
...and is marked for this function, than reset its color
*/
   if (n1 > 0)
     {
      chklab (UM_addr_of_f77_str(ncllabel), &nkey, &nclsub, &ifnd, &ifl);
      if (ifnd == 1)
        {
         e.key = nkey;
         status = ur_retrieve_data_fixed(&e);
         if (status == UU_SUCCESS && e.rel_num == NCL_MATRIX_REL)
             status = uc_retrieve_attr (e.key,&attr);
         if (status == UU_SUCCESS && color == attr.color)
           {
            for (i=0; i<3; i++)
               if (strcmp(ncl.label,MX_TRANS[i]) == 0) rscol = MXCOL[i];
            attr.color = rscol;
            status = ur_update_attr(&attr);
            uc_display (&e);
           }
        }
     }
   uu_dexit;
   return (status);
  }
/*********************************************************************
**    E_FUNCTION     : upmatr (key,axis)
**          Update matrix display parameters.      
**    PARAMETERS
**       INPUT  :
**          key      Unibase key to matrix.
**          axis     Matrix display parameters. 
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
 upmatr (key,axis)
  UU_KEY_ID *key;
  UU_REAL axis[4];
  {
   struct NCL_matrix_rec mx;
   int status = UU_FAILURE;
/*
...Get MX data and
...replace display parameters in MX structure
*/   
   mx.key = *key;
   status = ur_retrieve_data_fixed(&mx);
   if (status == UU_SUCCESS)
     {
      mx.dalen = axis[0];
      mx.dbox[0] = axis[1];
      mx.dbox[1] = axis[2];
      mx.dbox[2] = axis[3];
      status = ur_update_data_fixed(&mx);
     } 
   uu_dexit;
   return(status);
  }
/*********************************************************************
**    E_FUNCTION     : int gtmxds (buf)
**       Convert a UNIBASEe matrix to an NCLI  matrix.
**    PARAMETERS   
**       INPUT  : 
**          nclkey            UNIBASE key of the entity
**       OUTPUT :  
**          buf               buffer to place matrix display prms.
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
 gtmxds (buf,nclkey)
  struct NCLI_mxdisp_rec *buf;
  UM_int4 *nclkey;
   {
   int i, status;
   struct NCL_matrix_rec mx;

   uu_denter(UU_MTRC,(us,"gtmxds(buf=%x, key=%d)",buf,nclkey));

/*
   idx = 264;
   getifl (&idx, &ival);
   unit = (ival == 0) ? 1.0: 25.4;
*/
   mx.key = *nclkey;
   status = ur_retrieve_data_fixed(&mx);

   status = UU_SUCCESS;
   buf->dalen = mx.dalen;
   for (i=0; i<3; i++) buf->dbox[i] = mx.dbox[i];
   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     :  um_cctmtf( cci, tf, cco )
**       Multiply a cartesian coordinate by a transformation matrix
**          and return the result cco = cci * tf. The output coordinate
**          may be identical to the input.
**    PARAMETERS
**       INPUT  :
**              cci     coordinate
**              tf          transformation  -- if UM_DEFAULT_TF: identity
**       OUTPUT :
**          cco     coordinate * transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void cctmtf( cci, tf, cco )
UU_REAL cci[],tf[],cco[];
{
	um_cctmtf(cci,tf,cco);
}
