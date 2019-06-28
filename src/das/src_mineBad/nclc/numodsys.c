/*********************************************************************
**    NAME         :  numodsys.c
**       CONTAINS:  routines to handle changing modeling coord sys.
**    COPYRIGHT 1992 (c) NCCS Inc.  All Rights Reserved.
**         int nclu_modsys()
**     MODULE NAME AND RELEASE LEVEL 
**       numodsys.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:10
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "dasnog.h"
#include "view.h"
#include "mdcoord.h"
#include "mdunits.h"
#include "mpopmenu.h"
#include "uhep.h"

#include "nkeywd.h"
#include "nclinp.h"
#include "nclcmd.h"

/*********************************************************************
**    E_FUNCTION     nclu_modsys()
**         Set the current modeling system matrix.
**    PARAMETERS   
**       INPUT  : 
**            option    - 0 = MODSYS/mx
**                        1 = MODSYS/NOMORE
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_modsys(option)
int option;
   {
   int status;
   NCL_cmdbuf cmdbuf;

   uu_denter(UU_MTRC,(us,"nclu_modsys()"));

   ncl_init_cmdbuf(&cmdbuf);
   ncl_add_token(&cmdbuf, NCL_modsys, NCL_nocomma);

   switch (option)
      {
      case 0:
         status = ncl_add_str(&cmdbuf, 182, NCL_nocomma);
         break;
      case 1:
         status = ncl_add_token(&cmdbuf, NCL_nomore, NCL_nocomma);
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

   uu_dexit;
   return;
   }
/*********************************************************************
**    E_FUNCTION     nclu_chg_modaxis(itsk)
**         Set the current modeling system matrix.
**    PARAMETERS   
**       INPUT  : 
**            itsk   - 0 = align mod axis with view
**                     1 = set origin
**                     2 = snap axis to vector
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_chg_modaxis(itsk)
int itsk;
{
	int numint, option, iop;
	UM_vector   new_axis;         /* new axis vector */
	UD_DASCORD new_axis2;
	UD_NDCLOCREC origin;
	UM_vector   normal;
	UM_vector   upvec;
	UM_vector   temp;
	char axis_label[80];
	UV_vport    vport;

	if (UM_2d3d_mode == UM_3D)
	{
		new_axis[0] = new_axis[1] = new_axis[2] = 0.0;
		axis_label[0] = '\0';
		switch (itsk)
		{
		case 0:        /* align to view port */
			uvu_pickvp (&vport);
			uv_getvinfo(vport.key, &origin, normal, upvec);
			if (!um_vcparall(normal, upvec))
			{
				um_vctovc (&origin, UM_origin);
				um_unitvc (normal, UM_zaxis);
				um_vcplvc (&origin, upvec, temp);
				um_nptpln(temp, &origin, normal, UM_yaxis);
				um_vcmnvc(UM_yaxis, UM_origin, UM_yaxis);
				um_unitvc(UM_yaxis, UM_yaxis);
				um_cross(UM_yaxis, UM_zaxis, UM_xaxis);
			}
			iop = 4;
			break;
		case 1:       /* Set modeling origin */
			origin.label[0] = '\0';
			ud_ldas(UD_SCACART2, /*construction plane origin*/UM_MODEL, 341,
				&origin, 1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto done;
			um_vctovc (&origin, UM_origin);
			strcpy(axis_label, origin.label);
			iop = 1;
			break;
		case 2:        /* Snap modeling axis to vector */
			new_axis2.cord[0] = new_axis2.cord[1] = new_axis2.cord[2] = 0.;
			um_popupmenu(UM_ALIGN_ALONG_VEC, &option);
			switch(option)
			{
			case 1:
				new_axis2.cord[0] = 1.0;
				new_axis2.label[0] = '\0';
				ud_ldas(UD_SCAVEC2, /*construction plane x axis*/UM_MODEL, 342,
					&new_axis2, 1, &numint, UD_DEFAULT);
				if (numint <= 0) goto done;
				ncl_setmsys_xaxis (new_axis2.cord);
				strcpy(axis_label, new_axis2.label);
				iop = 2;
				break;
			case 2:
				new_axis2.cord[1] = 1.0;
				new_axis2.label[0] = '\0';
				ud_ldas(UD_SCAVEC2, /*construction plane y axis*/UM_MODEL, 343,
					&new_axis2, 1, &numint, UD_DEFAULT);
				if (numint <= 0) goto done;
				ncl_setmsys_yaxis (new_axis2.cord);
				strcpy(axis_label, new_axis2.label);
				iop = 3;
				break;
			case 3:
				new_axis[2] = 1.0;
				ud_ldas(UD_DASVEC, /*construction plane z axis*/UM_MODEL, 344,
					new_axis, 1, &numint, UD_DEFAULT);
				if (numint <= 0) goto done;
				ncl_setmsys_zaxis (new_axis);
				iop = 4;
				break;
			}
			break;
		}
		if (UU_application == UU_NCLCAM)
			ncl_insert_modsys(iop,axis_label);
		else
			ncl_set_modaxis(1);
	}
	else
		uu_uerror1 (UU_SIGNON, 6, "Change Model Axis");
done:
	return;
}

/*********************************************************************
**    E_FUNCTION     nclu_snap_modaxis()
**         Set the current modeling system matrix.
**    PARAMETERS   
**       INPUT  : 
**            none
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_snap_modaxis()
   {
   int numint, option, status;
   UD_PLOCREC pick;
   UM_vector origin;
   UM_vector new_axis;

   uu_denter(UU_MTRC,(us,"nclu_snap_modaxis()"));

   if (UM_2d3d_mode == UM_3D)
     {
     um_popupmenu(UM_SNAP_TO_LINE, &option);

     ud_ldas(UD_DASPCKLOC,/*pick a line*/UM_MODEL, 137, &pick, 1, &numint);
     if (numint <= 0) goto done;

     /* get the new origin */
     status = uc_ploc_to_coord(2, &pick.ppath, &pick.pndc, origin);
     if (status != UU_SUCCESS) goto done;

     /* get the new axis vector */
     status = uc_ploc_to_vector(2, &pick.ppath, &pick.pndc, new_axis);
     if (status != UU_SUCCESS) goto done;

     /* set the new origin and axis vector */
     um_vctovc (origin, UM_origin);

     /* set the new axis */
     switch (option)
       {
       case 1: /* set the new x direction */
         ncl_setmsys_xaxis (new_axis);
         break;
       case 2: /* set the new y direction */
         ncl_setmsys_yaxis (new_axis);
         break;
       case 3: /* set the new z xaxis */
         ncl_setmsys_zaxis (new_axis);
         break;
       }

     if (UU_application == UU_NCLCAM)
       ncl_insert_modsys(4,0);
     else
       ncl_set_modaxis(1);
     }
   else
     uu_uerror1 (UU_SIGNON, 6, "Change Model Axis");

done:
   uu_dexit;
   return;
   }
/*********************************************************************
**    E_FUNCTION     nclu_rot_modaxis()
**         Set the current modeling system matrix.
**    PARAMETERS   
**       INPUT  : 
**            option    - 0 = MODSYS/mx
**                        1 = MODSYS/NOMORE
**       OUTPUT :  
**            none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_rot_modaxis()
   {
   int numint, option;
   UM_vector xaxis;
   UM_vector yaxis;
   UM_vector zaxis;
   UM_vector temp;
   UM_transf rotmat;
   UU_REAL ang;

   uu_denter(UU_MTRC,(us,"nclu_rot_modaxis()"));

   xaxis[0] = yaxis[1] = zaxis[2] = 1.0;
   xaxis[1] = xaxis[2] = 0.0;
   yaxis[0] = yaxis[2] = 0.0;
   zaxis[0] = zaxis[1] = 0.0;

   if (UM_2d3d_mode == UM_3D)
     {
     um_popupmenu(UM_ROTATE_ABOUT_AXIS, &option);

     ud_ldas(UD_DASANGLE, /*angle of rotation*/UM_MODEL, 77, &ang,
                1, &numint, UD_NODEFAULT);
     if (numint > 0)
       {
       switch (option)
         {
         case 1:
            /* get the rotation matrix */
            um_rottf(xaxis, ang, rotmat);

            /* rotate the x and y axis */
            um_cctmtf(zaxis, rotmat, zaxis);
            um_cross(zaxis, xaxis, temp);
            um_cross(xaxis, temp, zaxis);
            um_unitvc(xaxis, UM_xaxis);
            um_unitvc(zaxis, UM_zaxis);
            um_cross(UM_zaxis, UM_xaxis, UM_yaxis);

            break;

         case 2:
            /* get the rotation matrix */
            um_rottf(yaxis, ang, rotmat);

            /* rotate the x and z axis */
            um_cctmtf(xaxis, rotmat, xaxis);
            um_cross(xaxis, yaxis, temp);
            um_cross(yaxis, temp, xaxis);
            um_unitvc(xaxis, UM_xaxis);
            um_unitvc(yaxis, UM_yaxis);
            um_cross(UM_xaxis, UM_yaxis, UM_zaxis);

            break;

         case 3:
            /* get the rotation matrix */
            um_rottf(zaxis, ang, rotmat);

            /* rotate the x and y axis */
            um_cctmtf(yaxis, rotmat, yaxis);
            um_cross(yaxis, zaxis, temp);
            um_cross(zaxis, temp, yaxis);
            um_unitvc(zaxis, UM_zaxis);
            um_unitvc(yaxis, UM_yaxis);
            um_cross(UM_yaxis, UM_zaxis, UM_xaxis);

            break;
          }
        }

     if (UU_application == UU_NCLCAM)
       ncl_insert_modsys(4,0);
     else
       ncl_set_modaxis(1);
     }
   else
     uu_uerror1 (UU_SIGNON, 6, "Change Model Axis");

done:
   uu_dexit;
   return;
   }
