
/*********************************************************************
**    NAME         :  m8irsol7.c
**       CONTAINS:
**			umomom(r_volume, r_cofg, r_p_axes, r_p_moments, 
**					r_p_tensor, r_g_tensor, r_about, r_labout,
**					r_vdir, r_lvdir)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m8irsol7.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:11
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "ustdio.h"
#include "uhep.h"
#include "xfsys2.h"
#include "dasnog.h"
#include "go.h"
#include "dmark.h"
#include "mromcom.h"
#include "mfcifdef.h"
#include "mdcpln.h"


/****************************************************************************
**    I_FUNCTION      : umomom(r_volume, r_cofg, r_p_axes, r_p_moments, 
**									r_p_tensor, r_g_tensor, r_about, r_labout,
**									r_vdir, r_lvdir)
**       output the moment information from ROMULUS to a
**       window on the screen
**    PARAMETERS   
**       INPUT  : 
**          R_VOLUME    : volume
**          R_COFG      : center of gravity
**          R_P_AXES    : principal axes
**          R_P_MOMENTS : principal moments
**          R_P_TENSOR  : inertial tensor calculated with the principal axes
**          R_G_TENSOR  : inertial tensor calculated with user given axes
**          R_ABOUT     : If LABOUT true; gives origin and first given
**                      coord. axis. If LABOUT false; gives point on the
**                      rotation axes.
**          R_LABOUT    :
**          R_VDIR      : If LVDIR true; gives second given coord. axis
**          R_LVDIR     :
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umomom(r_volume, r_cofg, r_p_axes, r_p_moments, r_p_tensor, r_g_tensor,
		r_about, r_labout, r_vdir, r_lvdir)
	float *r_volume;
	float  r_cofg [3];
	float  r_p_axes [3][3];
	float  r_p_moments [3];
	float  r_p_tensor [3][3];
	float  r_g_tensor [3][3];
	float  r_about [6];
	int  *r_labout;
	float  r_vdir [3];
	int  *r_lvdir;

	{

   char msg [150];                    /* message return pointer */
   int markval;    		              /* mark return integer */
	UU_REAL volume;
	UU_REAL cofg [3];
	UU_REAL p_axes [3][3];
	UU_REAL p_moments [3];
	UU_REAL p_tensor [3][3];
	UU_REAL g_tensor [3][3];
	UU_REAL about1[3];
	UU_REAL about2[3];
	UU_REAL vdir [3];
	UU_REAL inttoext, inttoext3, inttoext5;
	int i, j;

/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
**/

/* -- open a window -- */

   uu_denter( UU_MTRC,(us,"umomom(?,?,?,?,?,?,%d,?,%d)",*r_labout,*r_lvdir));

	ux_window_open("UD");

   UD_MARK(markval, UU_TRUE);
   if(markval == 0)
     	{
		/* convert common data to current output units */
		UM_len_inttoext((UU_REAL) 1.0, inttoext);
		inttoext3 = inttoext * inttoext * inttoext;
		inttoext5 = inttoext3 * inttoext * inttoext;

		volume = *r_volume * inttoext3;

		for (i=0; i<3; i++)
			cofg [i] = r_cofg[i] * inttoext;

		/* print information to window */
      if (*r_labout == 1 && *r_lvdir == 0)
        	{
			/* convert remaining data to current output units */
			g_tensor [0][0] = r_g_tensor[0][0] * inttoext5;
			for (i=0; i<3; i++)
				about1 [i] = r_about[i] * inttoext;
			for (i=0; i<3; i++)
				about2 [i] = r_about[i+3];
	
			/* print to window */
         sprintf (msg, "\tMOMENT OF INERTIA\n\n");
         ud_wrwin (msg);
         sprintf (msg, "\tVolume\t\t\t: %e\n",volume);
         ud_wrwin (msg);
         sprintf (msg, "\tCenter of Gravity\t: %.2f, %.2f, %.2f\n\n",
                  cofg[0],cofg[1],cofg[2]);
         ud_wrwin (msg);
         sprintf (msg, "\tMoment of Inertia\t: %e\n",g_tensor[0][0]);
         ud_wrwin (msg);
         sprintf (msg, "\tRadius of Gyration\t: %.4f\n\n",
                  sqrt(g_tensor[0][0] / volume));
         ud_wrwin (msg);
				sprintf (msg, "\tSPECIFIED AXIS\t (normalized)\n");
         ud_wrwin (msg);
         sprintf (msg, "\tOrigin of axis\t\t: %.2f,%.2f,%.2f\n"
						  ,about1[0],about1[1],about1[2]);
         ud_wrwin (msg);
         sprintf (msg, "\tDirection of axis\t: %.2f, %.2f, %.2f\n",
                  about2[0],about2[1],about2[2]);
         ud_wrwin (msg);
         }

      if (*r_labout == 0 && *r_lvdir == 0)
         {
			/* convert all remaining data to current output units */
			for (i=0; i<3; i++) 
				{
				for (j=0; j<3; j++)
					p_axes [i][j] = r_p_axes[i][j];
				}
		
			for (i=0; i<3; i++)
				p_moments [i] = r_p_moments[i] * inttoext5;
	
			/* output to window */
         sprintf (msg, "\tINERTIAL TENSOR: PRINCIPAL AXES\n\n");
         ud_wrwin(msg);
         sprintf (msg, "\tVolume\t\t\t: %e\n",volume);
         ud_wrwin(msg);
         sprintf (msg, "\tCenter of Gravity\t: %.2f, %.2f, %.2f\n\n",
                  cofg[0],cofg[1],cofg[2]);
         ud_wrwin(msg);
         sprintf (msg, "\tInertial Tensor    Ixx\t: %e\n",p_moments[0]);
				ud_wrwin (msg);
				sprintf (msg, "\t                   Iyy\t: %e\n",p_moments[1]);
				ud_wrwin (msg);
				sprintf (msg, "\t                   Izz\t: %e\n\n",p_moments[2]);
				ud_wrwin (msg);
         sprintf (msg, "\tRadius of Gyration Kxx\t: %.4f\n",
							sqrt(p_moments[0] / volume));
         ud_wrwin (msg);
				sprintf (msg, "\t                   Kyy\t: %.4f\n",
							sqrt(p_moments[1] / volume));
         ud_wrwin (msg);
				sprintf (msg, "\t                   Kzz\t: %.4f\n\n",
							sqrt(p_moments[2] / volume));
         ud_wrwin (msg);
         sprintf (msg, "\tPrincipal  Axes    X  \t: %.4f, %.4f, %.4f\n",
							p_axes[0][0],p_axes[0][1],p_axes[0][2]);
         ud_wrwin (msg);
				sprintf (msg, "\t                   Y  \t: %.4f, %.4f, %.4f\n",
							p_axes[1][0],p_axes[1][1],p_axes[1][2]);
         ud_wrwin (msg);
				sprintf (msg, "\t                   Z  \t: %.4f, %.4f, %.4f\n",
							p_axes[2][0],p_axes[2][1],p_axes[2][2]);
         ud_wrwin (msg);
         }

      if (*r_labout == 1 && *r_lvdir == 1)
         {
			/* convert all remaining data to current output units */
			for (i=0; i<3; i++)
				{
				for (j=0; j<3; j++)
					g_tensor [i][j] = r_g_tensor[i][j] * inttoext5;
				}
		
			for (i=0; i<3; i++)
				about1 [i] = r_about[i] * inttoext;

			for (i=0; i<3; i++)
				about2 [i] = r_about[i+3];
	
			for (i=0; i<3; i++)
				vdir [i] = r_vdir[i];
	
         sprintf (msg,"\tINERTIAL TENSOR : SPECIFIED AXES\n\n");
         ud_wrwin (msg);
         sprintf (msg, "\tVolume\t\t\t: %e\n",volume);
         ud_wrwin(msg);
         sprintf (msg, "\tCenter of Gravity\t: %.2f, %.2f, %.2f\n\n",
                  cofg[0],cofg[1],cofg[2]);
         ud_wrwin(msg);
         sprintf (msg, "\tInertial Tensor    Ixx\t: %e\n",
							g_tensor[0][0]);
         ud_wrwin(msg);
				sprintf (msg, "\t                   Iyy\t: %e\n",
							g_tensor[1][1]);
         ud_wrwin(msg);
				sprintf (msg, "\t                   Izz\t: %e\n\n",
							g_tensor[2][2]);
         ud_wrwin (msg);
         sprintf (msg, "\tInertial Tensor    Ixy\t: %e\n",
							g_tensor[0][1]);
         ud_wrwin (msg);
				sprintf (msg, "\t                   Ixz\t: %e\n",
							g_tensor[0][2]);
         ud_wrwin (msg);
				sprintf (msg, "\t                   Iyz\t: %e\n\n",
							g_tensor[1][2]);
         ud_wrwin (msg);
         sprintf (msg, "\tRadius of Gyration Kxx\t: %.4f\n",
							sqrt (g_tensor[0][0] / volume));
         ud_wrwin (msg);
				sprintf (msg, "\t                   Kyy\t: %.4f\n",
							sqrt (g_tensor[1][1] / volume));
         ud_wrwin (msg);
				sprintf (msg, "\t                   Kzz\t: %.4f\n\n",
							sqrt (g_tensor[2][2] / volume));
         ud_wrwin (msg);
         sprintf (msg, "\tSPECIFIED AXES (normalized) \n");
         ud_wrwin (msg);
				sprintf (msg, "\tOrigin of axes\t\t: %.4f, %.4f, %.4f\n",
							about1[0], about1[1], about1[2]);
         ud_wrwin (msg);
				sprintf (msg, "\tDirection of U axis\t: %.4f, %.4f, %.4f\n",
							about2[0], about2[1], about2[2]);
         ud_wrwin (msg);
				sprintf (msg, "\tDirection of V axis\t: %.4f, %.4f, %.4f\n",
							vdir[0], vdir[1], vdir[2]);
         ud_wrwin (msg);
        }

/*    -- enter any key to terminate -- */

      ud_hakt(UD_DASHEP, 17);
     	}
   UD_UNMARK(markval);
   ud_kiwin();
   uu_dexit;
	}
