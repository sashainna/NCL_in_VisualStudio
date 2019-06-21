/*********************************************************************
**    NAME         :  nemodsys.c
**       CONTAINS:  routines to handle changing modeling coord sys.
**    COPYRIGHT 1992 (c) NCCS Inc.  All Rights Reserved.
**         int stmdmx(mx)
**     MODULE NAME AND RELEASE LEVEL 
**       nemodsys.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:37
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "modef.h"
#include "mdcpln.h"
#include "mdebug.h"

#include "nccs.h"
#include "ncl.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "nclcmd.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     :stmdmx (mx)
**         Set the current modeling system matrix.
**    PARAMETERS   
**       INPUT  : 
**           mx      - matrix from which to set modeling system.
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void stmdmx (mx)
   UM_real8 mx[12];

   {
   int i, j;
   UM_coord origin;
   UM_vector xaxis;
   UM_vector yaxis;
   UM_vector zaxis;

   uu_denter(UU_MTRC,(us,"stmdmx()"));

   for (i=0;i<3;i++)
     {
     j = i*4;
     xaxis[i] = mx[j];
     yaxis[i] = mx[j+1];
     zaxis[i] = mx[j+2];
     origin[i] = mx[j+3];
     }

   um_set_mod_sys (origin, xaxis, yaxis, zaxis);

   uu_dexit;
   return;
   }
/*********************************************************************
**    E_FUNCTION     : ncl_wcstomcs(option, wcs, mcs)
**         Convert a vector or cartesian  coordinate specified in the
**         current construction  coordinate system to a vector or
**         cartestian  coordinate specified relative to the modeling
**         coordinate system.
**    PARAMETERS   
**       INPUT  : 
**            option      0 => cartesian  coordinate;
**                        1 => vector
**            wcs         input (construction  coordinate system)
**       OUTPUT :  
**            mcs         output (modeling coordinate system)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_wcstomcs(option, wcs, mcs)
   int option;
   UM_coord  wcs;
   UM_coord  mcs;

   {
   UM_real8 buf[3];
   UM_int2 i2opt;

   buf[0] = wcs[0];
   buf[1] = wcs[1];
   buf[2] = wcs[2];
   i2opt = option;
   wcsmcs (&i2opt, buf);
   mcs[0] = buf[0];
   mcs[1] = buf[1];
   mcs[2] = buf[2];
   }
/*********************************************************************
**    E_FUNCTION     : ncl_mcstf(iflg, mcstf)
**         Return the modeling coordinate system transformation matrix.
**    PARAMETERS   
**       INPUT  : 
**            none
**       OUTPUT :  
**            iflg       - = 1 iff modeling coord sys is in effect.
**            mcstf      - Modelling coord sys trasform.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mcstf(iflg, mcstf)
   int *iflg;
   UM_transf mcstf;

   {
   int i, j;
   UM_real8 mx[12];
   UM_int2 i2flg;

   mcsmx (&i2flg, mx);
   *iflg = i2flg;
   if (i2flg == 1)
     {
     for (i=0;i<3;i++)
       {
       j = i*4;
       mcstf[0][i] = mx[j];
       mcstf[1][i] = mx[j+1];
       mcstf[2][i] = mx[j+2];
       mcstf[3][i] = mx[j+3];
       }
     }

   }
/*********************************************************************
**    E_FUNCTION     : ncl_set_modaxis(iflg)
**         Set the model coord sys.
**    PARAMETERS   
**       INPUT  : 
**            iflg       - = 0 reset, = 1 set.
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_modaxis(iflg)
   int iflg;

   {
   int i, j;
   UM_real8 mx[12];
   UM_int2 i2flg;

   i2flg = iflg;
   if (i2flg == 1)
     {
     for (i=0;i<3;i++)
       {
       j = i*4;
       mx[j] = UM_xaxis[i];
       mx[j+1] = UM_yaxis[i];
       mx[j+2] = UM_zaxis[i];
       mx[j+3] = UM_origin[i];
       }
     }
   stmdax (&i2flg, mx);

   }
/*********************************************************************
**    E_FUNCTION     : ncl_set_wpaxis(iflg, wptf)
**         Set the working coord sys.
**    PARAMETERS   
**       INPUT  : 
**            iflg       - = 0 reset, = 1 set to mx.
**            wptf      - Working coord sys transform.
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_wpaxis(iflg, wptf)
   int iflg;
   UM_transf wptf;

   {
   int i, j, k;
   UM_real8 mx[12];
   UM_int2 i2flg;

   i2flg = iflg;
   if (i2flg == 1)
     {
     for (i=0;i<3;i++)
       {
       j = i*4;
       for (k=0;k<4;k++) mx[j+k] = wptf[k][i];
       }
     }
   stwpax (&i2flg, mx);

   }
/*********************************************************************
**    E_FUNCTION     : ncl_insert_modsys(option)
**         Insert a MODSYS command in the part program.
**    PARAMETERS   
**       INPUT  : 
**            option    - 0 = insert modsys/nm, 1 = insert modsys/mx
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_insert_modsys(option, axis_label)
int option;
char *axis_label;
   {
   int i, status;
   NCL_cmdbuf cmdbuf;
   UM_vector origin;
   UM_int2 ivl1,ivl2;
   char buf[80];

   ivl1 = ivl2 = 2;
   setins(&ivl1,&ivl2);

   status = NCL_OKINPUT;
   ncl_init_cmdbuf(&cmdbuf);
   ncl_add_token(&cmdbuf, NCL_modsys, NCL_nocomma);

	if (axis_label!=NULL)
	{
		i = strlen(axis_label);
		while ((i>0) && (axis_label[i-1]==' ')) i--;
		axis_label[i] = '\0';
	}
	switch (option)
	{
	case 0:
		status = ncl_add_str(&cmdbuf, 181, NCL_nocomma);
		break;
	case 1:
		ncl_add_token(&cmdbuf, "(", NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
		if ((axis_label==NULL) || (axis_label[0]=='\0'))
		{
			ncl_add_token(&cmdbuf, "(PT/", NCL_nocomma);
			um_vctovc (UM_origin, origin);
			unitcv(origin);
			ncl_fmtreal (3, origin, buf);
			ncl_add_token(&cmdbuf, buf, NCL_nocomma);
			ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
		}
		else
		{
			ncl_add_token(&cmdbuf, axis_label, NCL_comma);
		}
		ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
		ncl_fmtreal (3, UM_xaxis, buf);
		ncl_add_token(&cmdbuf, buf, NCL_nocomma);
		ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);

		ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
		ncl_fmtreal (3, UM_yaxis, buf);
		ncl_add_token(&cmdbuf, buf, NCL_nocomma);
		ncl_add_token(&cmdbuf, ") )", NCL_nocomma);
		break;
	case 2:
		ncl_add_token(&cmdbuf, "(", NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
		ncl_add_token(&cmdbuf, "(PT/", NCL_nocomma);
		um_vctovc (UM_origin, origin);
		unitcv(origin);
		ncl_fmtreal (3, origin, buf);
		ncl_add_token(&cmdbuf, buf, NCL_nocomma);
		ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);

		if ((axis_label==NULL) || (axis_label[0]=='\0'))
		{
			ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
			ncl_fmtreal (3, UM_xaxis, buf);
			ncl_add_token(&cmdbuf, buf, NCL_nocomma);
			ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
		}
		else
		{
			ncl_add_token(&cmdbuf, axis_label, NCL_comma);
		}
		ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
		ncl_fmtreal (3, UM_yaxis, buf);
		ncl_add_token(&cmdbuf, buf, NCL_nocomma);
		ncl_add_token(&cmdbuf, ") )", NCL_nocomma);
		break;
	case 3:
		ncl_add_token(&cmdbuf, "(", NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
		ncl_add_token(&cmdbuf, "(PT/", NCL_nocomma);
		um_vctovc (UM_origin, origin);
		unitcv(origin);
		ncl_fmtreal (3, origin, buf);
		ncl_add_token(&cmdbuf, buf, NCL_nocomma);
		ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);

		ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
		ncl_fmtreal (3, UM_xaxis, buf);
		ncl_add_token(&cmdbuf, buf, NCL_nocomma);
		ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);

		if ((axis_label==NULL) || (axis_label[0]=='\0'))
		{
			ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
			ncl_fmtreal (3, UM_yaxis, buf);
			ncl_add_token(&cmdbuf, buf, NCL_nocomma);
			ncl_add_token(&cmdbuf, ") )", NCL_nocomma);
		}
		else
		{
			ncl_add_token(&cmdbuf, axis_label, NCL_nocomma);
			ncl_add_token(&cmdbuf, ")", NCL_nocomma);
		}
		break;
	case 4:
		ncl_add_token(&cmdbuf, "(", NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_mx, NCL_nocomma);
		ncl_add_token(&cmdbuf, "(PT/", NCL_nocomma);
		um_vctovc (UM_origin, origin);
		unitcv(origin);
		ncl_fmtreal (3, origin, buf);
		ncl_add_token(&cmdbuf, buf, NCL_nocomma);
		ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);

		ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
		ncl_fmtreal (3, UM_xaxis, buf);
		ncl_add_token(&cmdbuf, buf, NCL_nocomma);
		ncl_add_token(&cmdbuf, "),$", NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);

		ncl_add_token(&cmdbuf, "(VE/", NCL_nocomma);
		ncl_fmtreal (3, UM_yaxis, buf);
		ncl_add_token(&cmdbuf, buf, NCL_nocomma);
		ncl_add_token(&cmdbuf, ") )", NCL_nocomma);
		break;
	default:
		status = NCL_NOINPUT;
		break;
	}

   if (status == NCL_OKINPUT)
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }
   ivl1 = 1; ivl2 = 0;
   setins(&ivl1,&ivl2);

   }
/*********************************************************************
**    E_FUNCTION     : ncl_setmsys_xaxis (xaxis)
**         Set the working coord sys.
**    PARAMETERS   
**       INPUT  : 
**            xaxis     - xaxis.
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_setmsys_xaxis (xaxis)
   UM_vector xaxis;

   {
   int status;
   UM_angle ang;
   UU_REAL signtest;
   UM_vector temp;
   UM_transf matrix;

   um_unitvc(xaxis,xaxis);
   if (um_cceqcc(xaxis,UM_zerovec))
      {
      uu_uerror0(/*vector is essentially a zero vector*/UM_MODEL,81);
      status = -1;
      }
   else
      {
      status = 0;
      if (um_vcparall(xaxis,UM_xaxis))
         {
         ang = 180.0;
         um_rottf( UM_yaxis,ang,matrix);
         }
      else
         {
         um_cross(UM_xaxis,xaxis,temp);
         ang = um_angle(UM_xaxis,xaxis);
         signtest = um_dot(UM_xaxis,temp);
         if (signtest < 0.0) ang = -ang;
         um_rottf(temp,ang,matrix);
         }
      um_cctmtf(UM_yaxis,matrix,UM_yaxis);
      um_vctovc(xaxis,UM_xaxis);

      um_cross(UM_xaxis,  UM_yaxis, temp);
      um_cross(UM_yaxis, temp,  UM_xaxis);
      um_unitvc(UM_xaxis,  UM_xaxis);
      um_unitvc(UM_yaxis,  UM_yaxis);
      um_cross(UM_xaxis,  UM_yaxis, UM_zaxis);
      }

   uu_dexit;
   return(status);

   }
/*********************************************************************
**    E_FUNCTION     : ncl_setmsys_yaxis (yaxis)
**         Set the working coord sys.
**    PARAMETERS   
**       INPUT  : 
**            yaxis     - yaxis.
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_setmsys_yaxis (yaxis)
   UM_vector yaxis;

   {
   int status;
   UM_angle ang;
   UU_REAL signtest;
   UM_vector temp;
   UM_transf matrix;

   um_unitvc(yaxis,yaxis);
   if (um_cceqcc(yaxis,UM_zerovec))
      {
      uu_uerror0(/*vector is essentially a zero vector*/UM_MODEL,81);
      status = -1;
      }
   else
      {
      status = 0;
      if (um_vcparall(yaxis,UM_yaxis))
         {
         signtest = um_dot (yaxis, UM_yaxis);
         if (signtest < 0.0) ang = UM_PI; else ang = 0.0;
         um_rottf (UM_yaxis,ang,matrix);
         }
      else
         {
         um_cross(UM_yaxis,yaxis,temp);
         ang = um_angle(yaxis,UM_yaxis);
         signtest = um_dot(UM_zaxis,temp);
         if (signtest < 0.0) ang = -ang;
         um_rottf(temp,ang,matrix);
         }
      um_cctmtf(UM_zaxis,matrix,UM_zaxis);
      um_vctovc(yaxis,UM_yaxis);

      um_cross(UM_yaxis,  UM_zaxis, temp);
      um_cross(UM_zaxis, temp,  UM_yaxis);
      um_unitvc(UM_yaxis,  UM_yaxis);
      um_unitvc(UM_zaxis,  UM_zaxis);
      um_cross(UM_yaxis,  UM_zaxis, UM_xaxis);
      }

   uu_dexit;
   return(status);

   }
/*********************************************************************
**    E_FUNCTION     : ncl_setmsys_zaxis (zaxis)
**         Set the working coord sys.
**    PARAMETERS   
**       INPUT  : 
**            zaxis     - zaxis.
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_setmsys_zaxis (zaxis)
   UM_vector zaxis;

   {
   int status;
   UM_angle ang;
   UU_REAL signtest;
   UM_vector temp;
   UM_transf matrix;

   um_unitvc(zaxis,zaxis);
   if (um_cceqcc(zaxis,UM_zerovec))
      {
      uu_uerror0(/*vector is essentially a zero vector*/UM_MODEL,81);
      status = -1;
      }
   else
      {
      status = 0;
      if (um_vcparall(zaxis,UM_zaxis))
         {
         ang = 180.0;
         um_rottf( UM_yaxis,ang,matrix);
         }
      else
         {
         um_cross(UM_zaxis,zaxis,temp);
         ang = um_angle(UM_zaxis,zaxis);
         signtest = um_dot(UM_zaxis,temp);
         if (signtest < 0.0) ang = -ang;
         um_rottf(temp,ang,matrix);
         }
      um_cctmtf(UM_yaxis,matrix,UM_yaxis);
      um_vctovc(zaxis,UM_zaxis);

      um_cross(UM_yaxis,  UM_zaxis, temp);
      um_cross(UM_zaxis, temp,  UM_yaxis);
      um_unitvc(UM_zaxis,  UM_zaxis);
      um_unitvc(UM_yaxis,  UM_yaxis);
      um_cross(UM_yaxis,  UM_zaxis, UM_xaxis);
      }

   uu_dexit;
   return(status);

   }
