
/*********************************************************************
**    NAME         :  m2uanaly.c
**       CONTAINS: user interface to 2D analysis routines
**				umu_2d_analysis()
**				umu_2da_modals()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m2uanaly.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:48
*********************************************************************/
#include "usysdef.h"
#include "ustdio.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dselmask.h"
#include "dmark.h"
#include "mdcoord.h"
#include "mdebug.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "xfsys2.h"
#include "mdrel.h"
#include "m2dattr.h"

/*********************************************************************
**    E_FUNCTION     : umu_2d_analysis()
**       Calculate the 2-d properties of areas bounded by polygons
**       and composite curves.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_2d_analysis()

	{
	UU_REAL area,
	        perimeter,
           Ixo,Iyo,xbar,ybar,zbar,
           Ix,
			  Iy,
			  kx,
			  ky,
			  kz,
			  J,
			  sum_area,
			  sum_perimeter,
			  sum_Ixo,
			  sum_Iyo,
			  sum_Adx,
			  sum_Ady,
			  sum_Adx2,
			  sum_Ady2;
	struct UM_poly_rec pgon;
   struct UM_compcrv_rec ccrv;
	UM_PICKENT pent;
	int numint;
	int markval;
	int npos;
	char ext_units[5];
	UM_vector xaxis,yaxis;
	UU_KEY_ID key;
	int rel_num;
	int error;
	int i;
	UM_vector plane_normal,
				 null;
	UM_coord	plane_origin,
				centroid;

	char fmt_bufr[132];
	char quote='\34';

	uu_denter(UU_MTRC,(us,"umu_2d_analysis()"));

	/* set 2D analysis attributes if not initialized */
	if (UM_initialize_2da_attr == UU_TRUE)
		{
		UM_2dattr.pts_per_span	= 100;
		UM_2dattr.width			=  16;
		UM_2dattr.prec				=   3;
		UM_2dattr.type				=   0;
		UM_2dattr.disp_flag		= UU_TRUE;
		strcpy(UM_2dattr.format, "%16.3f");

		UM_initialize_2da_attr	= UU_FALSE;
		}

	while (UU_TRUE)
		{
		um_getcpln(plane_origin,xaxis,yaxis,plane_normal);

		/* limit geometry to entities for which 2D analysis is supported */
		ud_lgeo(UU_TRUE, UD_2D_analysis);

		/* calculate positive areas and perimeters */
	   sum_area = 0.0;
		sum_perimeter = 0.0;
		npos = 0;
		sum_Ixo = 0.0;
		sum_Iyo = 0.0;
		sum_Adx = 0.0;
		sum_Ady = 0.0;
		sum_Adx2 = 0.0;
		sum_Ady2 = 0.0;
			 
		while (UU_TRUE)
			{
			um_dl_pdas(UD_DASPICK, /*pick positive area entity*/UM_MODEL, 287,
				&pent, 1, &numint, 1);
			if (numint <= 0 ) break;
			key = um_get_pickkey(&pent, 1);
			ur_retrieve_data_relnum(key, &rel_num);

			if (rel_num == UM_POLY_REL)
			  {
           um_polygon_props(key,plane_normal,plane_origin,xaxis,&area,
									 &perimeter,&Ixo,&Iyo,&xbar,&ybar,&error);

			  if (error == 0) /* planar curve */
				  {
	    	     /* summation of props */	
			     sum_area += area;
			     sum_perimeter += perimeter;
			     sum_Ixo += Ixo;
			     sum_Iyo += Iyo;
			     sum_Adx += (area * xbar);
			     sum_Ady += (area * ybar);
			     sum_Adx2 += (area * xbar * xbar);
			     sum_Ady2 += (area * ybar * ybar);
			     npos++;
				  }
			  else
				  {
				  if (error == 1)
       	        uu_uerror0(/*ERROR - polygon not planar*/UM_MODEL,284);
				  if (error == 2)
       	        uu_uerror0(/*ERROR - polygon is not in defined plane*/
										UM_MODEL,285);
				  }
			  }

			if (rel_num == UM_COMPCRV_REL)
			  {
           um_ccrv_props(key,plane_normal,plane_origin,xaxis,&area,
									 &perimeter,&Ixo,&Iyo,&xbar,&ybar,&error);
			  if (error == 0) /* planar curve */
				  {
	    	     /* summation of props */	
			     sum_area += area;
			     sum_perimeter += perimeter;
			     sum_Ixo += Ixo;
			     sum_Iyo += Iyo;
			     sum_Adx += (area * xbar);
			     sum_Ady += (area * ybar);
			     sum_Adx2 += (area * xbar * xbar);
			     sum_Ady2 += (area * ybar * ybar);
			     npos++;
				  }
			  else
				  {
				  if (error == 1)
       	        uu_uerror0(/*ERROR - unsupported curve in ccrv"*/UM_MODEL,289);
				  if (error == 2)
       	        uu_uerror0(/*ERROR - ccrv not planar*/UM_MODEL,286);
				  if (error == 3)
       	        uu_uerror0(/*ERROR - ccrv is not in defined plane*/
										UM_MODEL,287);
				  if (error == 4)
       	        uu_uerror0(/*ERROR - ccrv is not closed*/UM_MODEL,288);
				  }
			  }
			}
	   if (npos == 0) goto done;
			
	   /* calculate negative areas and perimeters */
		while (UU_TRUE)
			{
			um_dl_pdas(UD_DASPICK, /*pick positive area entity*/UM_MODEL, 288,
				&pent, 1, &numint, 1);
			if (numint <= 0 ) break;
			key = um_get_pickkey(&pent, 1);
			ur_retrieve_data_relnum(key, &rel_num);

			if (rel_num == UM_POLY_REL)
			  {
           um_polygon_props(key,plane_normal,plane_origin,xaxis,&area,
									 &perimeter,&Ixo,&Iyo,&xbar,&ybar,&error);

			  if (error == 0) /* planar curve */
				  {
	    	     /* summation of props */	
				  area = (0 - area);
			     sum_area += area;
			     sum_perimeter += perimeter;
			     sum_Ixo = sum_Ixo - Ixo;
			     sum_Iyo = sum_Iyo - Iyo;
			     sum_Adx += (area * xbar);
			     sum_Ady += (area * ybar);
			     sum_Adx2 += (area * xbar * xbar);
			     sum_Ady2 += (area * ybar * ybar);
				  }
			  else
				  {
				  if (error == 1)
       	        uu_uerror0(/*ERROR - polygon not planar*/UM_MODEL,284);
				  if (error == 2)
       	        uu_uerror0(/*ERROR - polygon is not in defined plane*/
										UM_MODEL,285);
				  }
			  }

			if (rel_num == UM_COMPCRV_REL)
			  {
           um_ccrv_props(key,plane_normal,plane_origin,xaxis,&area,
									 &perimeter,&Ixo,&Iyo,&xbar,&ybar,&error);
			  if (error == 0) /* planar curve */
				  {
	    	     /* summation of props */	
				  area = (0 - area);
			     sum_area += area;
			     sum_perimeter += perimeter;
			     sum_Ixo = sum_Ixo - Ixo;
			     sum_Iyo = sum_Iyo - Iyo;
			     sum_Adx += (area * xbar);
			     sum_Ady += (area * ybar);
			     sum_Adx2 += (area * xbar * xbar);
			     sum_Ady2 += (area * ybar * ybar);
				  }
			  else
				  {
				  if (error == 1)
       	        uu_uerror0(/*ERROR - unsupported curve in ccrv"*/UM_MODEL,289);
				  if (error == 2)
       	        uu_uerror0(/*ERROR - ccrv not planar*/UM_MODEL,286);
				  if (error == 3)
       	        uu_uerror0(/*ERROR - ccrv is not in defined plane*/
										UM_MODEL,287);
				  if (error == 4)
       	        uu_uerror0(/*ERROR - ccrv is not closed*/UM_MODEL,288);
				  }
			  }
			}
      /* convert props to correct units */
		UM_len_inttoext(sum_area, sum_area);
		UM_len_inttoext(sum_area, sum_area);

		UM_len_inttoext(sum_perimeter, sum_perimeter);

		UM_len_inttoext(xbar, xbar);

		UM_len_inttoext(ybar, ybar);

		UM_len_inttoext(plane_origin[0], plane_origin[0]);
		UM_len_inttoext(plane_origin[1], plane_origin[1]);
		UM_len_inttoext(plane_origin[2], plane_origin[2]);

		UM_len_inttoext(sum_Ixo, sum_Ixo);
		UM_len_inttoext(sum_Ixo, sum_Ixo);
		UM_len_inttoext(sum_Ixo, sum_Ixo);
		UM_len_inttoext(sum_Ixo, sum_Ixo);

		UM_len_inttoext(sum_Iyo, sum_Iyo);
		UM_len_inttoext(sum_Iyo, sum_Iyo);
		UM_len_inttoext(sum_Iyo, sum_Iyo);
		UM_len_inttoext(sum_Iyo, sum_Iyo);

		UM_len_inttoext(sum_Adx, sum_Adx);
		UM_len_inttoext(sum_Adx, sum_Adx);
		UM_len_inttoext(sum_Adx, sum_Adx);

		UM_len_inttoext(sum_Ady, sum_Ady);
		UM_len_inttoext(sum_Ady, sum_Ady);
		UM_len_inttoext(sum_Ady, sum_Ady);

		UM_len_inttoext(sum_Adx2, sum_Adx2);
		UM_len_inttoext(sum_Adx2, sum_Adx2);
		UM_len_inttoext(sum_Adx2, sum_Adx2);
		UM_len_inttoext(sum_Adx2, sum_Adx2);

		UM_len_inttoext(sum_Ady2, sum_Ady2);
		UM_len_inttoext(sum_Ady2, sum_Ady2);
		UM_len_inttoext(sum_Ady2, sum_Ady2);
		UM_len_inttoext(sum_Ady2, sum_Ady2);

		/* -- open a DAS type window -- */
		ux_window_open("UD");

		UD_MARK(markval, UU_TRUE);
		if(markval == 0)
			{
			/* determine external units name */
			strcpy(ext_units, UM_linear_units_name[UM_cpln.length_unit]);

			/* calculate and display area */
			strcpy(fmt_bufr, "Area      = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s^2\n");
			sprintf(UM_sbuf, fmt_bufr, sum_area, ext_units);
      	ud_wrwin (UM_sbuf);

			/* calculate and display perimeter */
			strcpy(fmt_bufr, "Perimeter = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s\n");
			sprintf(UM_sbuf, fmt_bufr, sum_perimeter, ext_units);
      	ud_wrwin (UM_sbuf);

			/* section for props about locally defined coor. system */
			sprintf(UM_sbuf, "\nCentroid About Modeling Axis\n");
      	ud_wrwin (UM_sbuf);

			/* calculate and display perimeter */
			xbar = (sum_Adx/sum_area);
			ybar = (sum_Ady/sum_area);
		   UM_len_exttoint(xbar,xbar);
		   UM_len_exttoint(ybar,ybar);
			centroid[0] = xbar;
			centroid[1] = ybar;
			centroid[2] = 0.;
			um_ccstomcs(0,centroid,centroid);
		   UM_len_inttoext(centroid[0],centroid[0]);
		   UM_len_inttoext(centroid[1],centroid[1]);
		   UM_len_inttoext(centroid[2],centroid[2]);
		   UM_len_inttoext(xbar,xbar);
		   UM_len_inttoext(ybar,ybar);

			strcpy(fmt_bufr, "xbar      = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s\n");
			sprintf(UM_sbuf, fmt_bufr, centroid[0], ext_units);
      	ud_wrwin (UM_sbuf);

			strcpy(fmt_bufr, "ybar      = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s\n");
			sprintf(UM_sbuf, fmt_bufr, centroid[1], ext_units);
      	ud_wrwin (UM_sbuf);

			strcpy(fmt_bufr, "zbar      = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s\n");
			sprintf(UM_sbuf, fmt_bufr, centroid[2], ext_units);
      	ud_wrwin (UM_sbuf);

			/* section for props about locally defined coor. system */
			sprintf(UM_sbuf, "\nCentroid About Working Plane Axis\n");
      	ud_wrwin (UM_sbuf);

			/* display xbar */
			strcpy(fmt_bufr, "xbar      = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s\n");
			sprintf(UM_sbuf, fmt_bufr, xbar, ext_units);
      	ud_wrwin (UM_sbuf);

			/* display ybar */
			strcpy(fmt_bufr, "ybar      = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s\n");
			sprintf(UM_sbuf, fmt_bufr, ybar, ext_units);
      	ud_wrwin (UM_sbuf);

			/* display zbar */
			zbar = 0.;
			strcpy(fmt_bufr, "zbar      = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s\n");
			sprintf(UM_sbuf, fmt_bufr, zbar, ext_units);
      	ud_wrwin (UM_sbuf);
			
			/* display xaxis vector */
         um_unitvc(xaxis, xaxis);
	      sprintf(UM_sbuf,"x axis    = <%f, %f, %f>\n",
				xaxis[0],xaxis[1],xaxis[2]);
      	ud_wrwin (UM_sbuf);
			
			/* display yaxis vector */
         um_unitvc(yaxis, yaxis);
	      sprintf(UM_sbuf,"y axis    = <%f, %f, %f>\n",
				yaxis[0],yaxis[1],yaxis[2]);
      	ud_wrwin (UM_sbuf);

			/* section for props about centroid */
			sprintf(UM_sbuf, "\nProperties About Centroid\n");
      	ud_wrwin (UM_sbuf);

         /* display the moment of inertia about the x axis */
         Ix = sum_Ixo + sum_Ady2;
         Ixo = Ix - (sum_area * ybar * ybar);
			strcpy(fmt_bufr, "Ixo       = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s^4\n");
			sprintf(UM_sbuf, fmt_bufr, Ixo, ext_units);
      	ud_wrwin (UM_sbuf);

         /* display the moment of inertia about the x axis */
         Iy = sum_Iyo + sum_Adx2;
         Iyo = Iy - (sum_area * xbar * xbar);
			strcpy(fmt_bufr, "Iyo       = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s^4\n");
			sprintf(UM_sbuf, fmt_bufr, Iyo, ext_units);
      	ud_wrwin (UM_sbuf);

         /* display the polar moment of inertia */
			J = Ixo + Iyo;
			strcpy(fmt_bufr, "Jo        = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s^4\n\n");
			sprintf(UM_sbuf, fmt_bufr, J, ext_units);
      	ud_wrwin (UM_sbuf);

         /* display the radius of gyration about the local x axis */
			if (Ixo/sum_area < 0)
				sprintf(UM_sbuf, "kxo       = Can't Compute!\n");
			else
				{
				kx = sqrt(Ixo/sum_area);
				strcpy(fmt_bufr, "kxo       = ");
				strcat(fmt_bufr, UM_2dattr.format);
				strcat(fmt_bufr, " %s\n");
				sprintf(UM_sbuf, fmt_bufr, kx, ext_units);
				}
      	ud_wrwin (UM_sbuf);

         /* display the radius of gyration about the local y axis */
			if (Iyo/sum_area < 0)
				sprintf(UM_sbuf, "kyo       = Can't Compute!\n");
			else
				{
				ky = sqrt(Iyo/sum_area);
				strcpy(fmt_bufr, "kyo       = ");
				strcat(fmt_bufr, UM_2dattr.format);
				strcat(fmt_bufr, " %s\n");
				sprintf(UM_sbuf, fmt_bufr, ky, ext_units);
				}
      	ud_wrwin (UM_sbuf);

         /* display the radius of gyration about the local y axis */
			if (J/sum_area < 0)
				sprintf(UM_sbuf, "kzo       = Can't Compute!\n");
			else
				{
				kz = sqrt(J/sum_area);
				strcpy(fmt_bufr, "kzo       = ");
				strcat(fmt_bufr, UM_2dattr.format);
				strcat(fmt_bufr, " %s\n");
				sprintf(UM_sbuf, fmt_bufr, kz, ext_units);
				}
      	ud_wrwin (UM_sbuf);

			/* section for props about working plane axis */
			sprintf(UM_sbuf, "\nProperties About Working Plane Axis\n");
      	ud_wrwin (UM_sbuf);

			/* display first moment about the x axis */
			strcpy(fmt_bufr, "Qx        = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s^3\n");
			sprintf(UM_sbuf, fmt_bufr, sum_Ady, ext_units);
      	ud_wrwin (UM_sbuf);
			
			/* display first moment about the y axis */
			strcpy(fmt_bufr, "Qy        = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s^3\n\n");
			sprintf(UM_sbuf, fmt_bufr, sum_Adx, ext_units);
      	ud_wrwin (UM_sbuf);
			
         /* display the moment of inertia about the x axis */
			strcpy(fmt_bufr, "Ix        = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s^4\n");
			sprintf(UM_sbuf, fmt_bufr, Ix, ext_units);
      	ud_wrwin (UM_sbuf);

         /* display the moment of inertia about the x axis */
			strcpy(fmt_bufr, "Iy        = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s^4\n");
			sprintf(UM_sbuf, fmt_bufr, Iy, ext_units);
      	ud_wrwin (UM_sbuf);

         /* display the polar moment of inertia */
			J = Ix + Iy;
			strcpy(fmt_bufr, "J         = ");
			strcat(fmt_bufr, UM_2dattr.format);
			strcat(fmt_bufr, " %s^4\n\n");
			sprintf(UM_sbuf, fmt_bufr, J, ext_units);
      	ud_wrwin (UM_sbuf);

         /* display the radius of gyration about the local x axis */
			if (Ix/sum_area < 0)
				sprintf(UM_sbuf, "kx        = Can't Compute!\n");
			else
				{
				kx = sqrt(Ix/sum_area);
				strcpy(fmt_bufr, "kx        = ");
				strcat(fmt_bufr, UM_2dattr.format);
				strcat(fmt_bufr, " %s\n");
				sprintf(UM_sbuf, fmt_bufr, kx, ext_units);
				}
      	ud_wrwin (UM_sbuf);

         /* display the radius of gyration about the local y axis */
			if (Iy/sum_area < 0)
				sprintf(UM_sbuf, "ky        = Can't Compute!\n");
			else
				{
				ky = sqrt(Iy/sum_area);
				strcpy(fmt_bufr, "ky        = ");
				strcat(fmt_bufr, UM_2dattr.format);
				strcat(fmt_bufr, " %s\n");
				sprintf(UM_sbuf, fmt_bufr, ky, ext_units);
				}
      	ud_wrwin (UM_sbuf);

         /* display the radius of gyration about the local y axis */
			if (J/sum_area < 0)
				sprintf(UM_sbuf, "kz        = Can't Compute!\n");
			else
				{
				kz = sqrt(J/sum_area);
				strcpy(fmt_bufr, "kz        = ");
				strcat(fmt_bufr, UM_2dattr.format);
				strcat(fmt_bufr, " %s\n");
				sprintf(UM_sbuf, fmt_bufr, kz, ext_units);
				}
      	ud_wrwin (UM_sbuf);

			ud_hakt(/* enter any key to terminate */ UD_DASHEP, 17);
			}
		UD_UNMARK(markval);
		ud_kiwin();
		if (markval != 0) break;
		}

done:;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : umu_2da_modals()
**       Function to control the "attributes" associated with the
**       2D property analysis functions.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_2da_modals()
	{
	int status;
	char  bufr[32];
	char  type;

	static int	m2da_pts, 	m2da_width, m2da_prec,
					m2da_type, m2da_disp;
	static int *ans[] = {&m2da_pts,  &m2da_width, &m2da_prec,
								&m2da_type, &m2da_disp};
	uu_denter(UU_MTRC, (us, "umu_2da_modals()"));

	if (UM_initialize_2da_attr == UU_TRUE)
		{
		UM_2dattr.pts_per_span	= 100;
		UM_2dattr.width			=  16;
		UM_2dattr.prec				=   3;
		UM_2dattr.type				=   0;
		UM_2dattr.disp_flag		= UU_TRUE;

		UM_initialize_2da_attr	= UU_FALSE;
		}
		
	m2da_pts			= UM_2dattr.pts_per_span;
	m2da_width		= UM_2dattr.width;
	m2da_prec		= UM_2dattr.prec;
	m2da_type		= UM_2dattr.type;
	m2da_disp		= (UM_2dattr.disp_flag == UU_TRUE) ? 0 : 1;

	status = ud_form("analysis.frm", ans, ans);
	if (status==-1)
	   return -1;
	UM_2dattr.pts_per_span	= m2da_pts;
	UM_2dattr.width			= m2da_width;
	UM_2dattr.prec				= m2da_prec;
	UM_2dattr.type				= m2da_type;
	UM_2dattr.disp_flag		= (m2da_disp == 0) ? UU_TRUE : UU_FALSE;

	type = (m2da_type==0)? 'f' : 'e';

	/* check to ensure that precision is at least 2 less than width */
	if (m2da_prec > (m2da_width -2))
		{
		uu_outputerr("Warning: precision value too large for field -- set to 3");
		m2da_prec = 3;
		}

	sprintf(UM_2dattr.format, "%c%d%c%d%c",
		'%', m2da_width, '.', m2da_prec, type);

	uu_dexit;
	}
