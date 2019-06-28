/*********************************************************************
**    NAME         :  m3egrid.c
**       CONTAINS: construction plane grid manipulation
**			umu_gridfrm()
**			um_draw_grid()
**			umi_drawgrid()
**			umi_snapping(pt,dx,dy)
**			um_inactgrid()
**			um_init_grid()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m3egrid.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:53
*********************************************************************/
#include	"ustdio.h"
#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include	"mdcoord.h"
#include	"mdcpln.h"
#include	"mdebug.h"
#include	"view.h"
#include	"gerrorst.h"

#define	DEFAULT	1
#define	NODEFAULT	0
#define	UM_MAXGRID	100
static UU_LOGICAL	UM_initialize_grid = UU_TRUE;

void umi_drawgrid(),umi_snapping(),um_inactgrid();

/*********************************************************************
**    U_FUNCTION :  umu_gridfrm()
**       Put up the grid form.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_gridfrm()

	{
	UV_vport  vport;							/* vport to display grid */
	UV_view	 view;							/* view associated with viewport */
	UU_REAL	min;
	int	i, state;
	UU_REAL	width, height,curwidth,curheight;
	int	xratio, yratio, wid, ht, ddx, ddy;
	char	unitstr[12];
	static int	dcont[2];					/* yes/no to display grid */
	static int	dcolor[15];					/* new grid color */
	static UU_REAL	 sx, sy;					/* new grid spacing */
	static UU_REAL	 dx, dy;					/* new grid snapping */
	static UU_REAL	minratio = 20.0;		/* ratio to set the minimun grid space */
	static char	aperture[40];
	static int *ans[] = {(int *) &sx,
								(int *) &sy,
								(int *) &dx,
								(int *) &dy,
								(int *) dcolor,
								(int *) dcont,
								(int *) aperture
								};

	uu_denter(UU_MTRC,(us, "umu_gridfrm, UM_initialize_grid=%d",
		UM_initialize_grid));

	if (uvu_pickvp(&vport) != UU_SUCCESS) goto done;

	uv_getvid(vport.cur_view, &view);

	if (um_vcperp(view.cur_pln_norm, UM_cpln.zaxis))
		uu_uerror0(/* Construction plane is perpendicular to the view */
			UM_MODEL,231);
	else
		{
		/* calculate the current aperature in x and y */
		curwidth = view.cur_aperture;
		curheight = (vport.urb[1]-vport.llf[1])/(vport.urb[0]-vport.llf[0])*curwidth;
		UM_len_inttoext(curwidth,wid);
		width = wid;
		UM_len_inttoext(curheight,ht);
		height = ht;
		um_linear_units_str(UM_cpln.length_unit, unitstr);
		sprintf(aperture, "%.1f %s x %.1f %s", width,unitstr,height,unitstr);

		do
			{
			if (UM_initialize_grid)
				{
				/* The grid space is calculated between internal and external units 
					to guarantee to display a whole number on the first form */
				sx = curwidth / minratio;
				UM_len_inttoext(sx,sx);
				ddx = sx + 0.99;				/* round the number */
				sx = ddx;
				UM_len_exttoint(sx,sx);
				sy = curheight / minratio;
				UM_len_inttoext(sy,sy);
				ddy = sy + 0.99;				/* round the number */
				sy = ddy;
				UM_len_exttoint(sy,sy);
				min = (sx < sy)? sx : sy;
				dy = sy = dx = sx = min;
#if (UU_COMP == UU_IRIS4D) || (UU_COMP == UU_SUN) || (UU_COMP == UU_WIN2K)
	/*NCL: change default grid color to white for SGI and SUN */		
				dcolor[0] = 1;
#else
				dcolor[0] = 4;
#endif
			  }
			if(ud_form("gridform.frm", ans, ans) == -1) goto done;

			UM_cpln.grid.vpid = vport.key;
			UM_cpln.grid.viewid = vport.cur_view;
			UM_cpln.grid.sx = sx;
			UM_cpln.grid.sy = sy;
			UM_cpln.grid.dx = dx;
			UM_cpln.grid.dy = dy;
			UM_cpln.grid.color = dcolor[0];
			UM_cpln.grid.disp = (dcont[0]==0)? UU_TRUE : UU_FALSE;
			UM_cpln.grid.snap = UU_TRUE;
			state = um_draw_grid(UU_TRUE);
			UM_initialize_grid = UU_FALSE;
			} while (state==UU_FALSE);
		}
done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_inactgrid()
**       User interface routine to inactivate grid.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_inactgrid()

	{
	uu_denter(UU_MTRC,(us,"umu_inactgrid()"));
	um_inactgrid(UU_TRUE);
	UM_cpln.grid.disp = UU_FALSE;
	uu_dexit;
	}


/*********************************************************************
**    E_FUNCTION :  um_draw_grid(new)
**       Draw the grid.
**    PARAMETERS   
**       INPUT  : 
**          new					UU_TRUE  => recalculate grid
**										UU_FALSE => use old values for grid
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_draw_grid(new)
	UU_LOGICAL new;

	{
	UV_vport vport;
	UV_view view;
	int i;
	int nint;
	int xnum, ynum;
	UU_REAL aspect_ratio;
	UM_coord cur_xaxis;
	UM_length xlength, ylength;
	UM_vector tempx, tempy;
	UM_coord window[4];
	UM_coord cpln[4];
	UM_coord temp;
	UM_coord cpln_ccllf, cpln_ccurb;

	uu_denter(UU_MTRC,(us,"um_draw_grid()"));
	
	/* delete old grid if it exists */
	if (UM_cpln.grid.dsegid != -1)
		{
		gdeleteseg(UM_cpln.grid.dsegid); 
		UM_cpln.grid.dsegid = -1;
		}

	uv_getvpid(UM_cpln.grid.vpid, &vport);
	uv_getvid(vport.cur_view, &view);

	if (new)
		{
		if (um_vcperp(UM_cpln.zaxis, view.cur_pln_norm)) goto done;
	
		uv_vp_aspect_ratio(&vport, &aspect_ratio);
		um_cross(view.cur_up_vect, view.cur_pln_norm, cur_xaxis);
		um_unitvc(cur_xaxis, cur_xaxis);
		xlength = view.cur_aperture / 2.0;
		ylength = xlength * aspect_ratio;
		um_vctmsc(cur_xaxis, xlength, tempx);
		um_vctmsc(view.cur_up_vect, ylength, tempy);
		um_vcmnvc(view.cur_ref_pt, tempy, window[0]);
		um_vcmnvc(window[0], tempx, window[0]);
		um_vctmsc(tempx, (UU_REAL) 2.0, tempx);
		um_vctmsc(tempy, (UU_REAL) 2.0, tempy);
		um_vcplvc(window[0], tempx, window[1]);
		um_vcplvc(window[1], tempy, window[2]);
		um_vcmnvc(window[2], tempx, window[3]);
	
		for (i=0; i<4; i++)
			{
			um_ilnpln(window[i],view.cur_pln_norm,UM_cpln.origin,UM_cpln.zaxis,
				&nint,temp);
			um_mcstoccs(0, temp, cpln[i]);
			}
	
		um_vctovc(cpln[0], cpln_ccllf);
		um_vctovc(cpln[0], cpln_ccurb);
		for (i=1; i<4; i++)
			{
			if (cpln[i][0] < cpln_ccllf[0]) cpln_ccllf[0] = cpln[i][0];
			if (cpln[i][0] > cpln_ccurb[0]) cpln_ccurb[0] = cpln[i][0];
			if (cpln[i][1] < cpln_ccllf[1]) cpln_ccllf[1] = cpln[i][1];
			if (cpln[i][1] > cpln_ccurb[1]) cpln_ccurb[1] = cpln[i][1];
			}
		cpln_ccllf[2] = cpln_ccurb[2] = cpln[0][2];
	
		umi_snapping(UU_TRUE,cpln_ccllf);
		umi_snapping(UU_FALSE,cpln_ccurb);
		
		xnum = (cpln_ccurb[0] - cpln_ccllf[0]) / UM_cpln.grid.sx + 0.5;
		ynum = (cpln_ccurb[1] - cpln_ccllf[1]) / UM_cpln.grid.sy + 0.5;
		if ((xnum > UM_MAXGRID) || (ynum > UM_MAXGRID))
		  {
			uu_uerror0(/*Grid space is too small. Please redefine it.*/
			UM_MODEL,272);
			uu_dexit;
			return(UU_FALSE);
		  }

		um_vctovc(cpln_ccllf, UM_cpln.grid.cpln_ccllf);
		UM_cpln.grid.xnum = xnum;
		UM_cpln.grid.ynum = ynum;
		}

	/* create the DIGS segment */
	if (UM_cpln.grid.disp)
		{
		UM_cpln.grid.dsegid = gnseg();
		if( gcreateseg(UM_cpln.grid.dsegid) != NCL_NO_ERROR ) 
			uu_sys_err_recovery(/* Out of digs segments */ -1, UM_MODEL, 273, 0, 0);
		gsnormtran(vport.xform);
		umi_drawgrid();
		gssegvis(UM_cpln.grid.dsegid, UG_VISIBLE);
		gcloseseg();
		gssegdet(UM_cpln.grid.dsegid, UG_UNDETECTABLE);
		}

done:
	uu_dexit;
	return(UU_TRUE);
	}

/*********************************************************************
**    E_FUNCTION :  umi_drawgrid()
**       draw the grid on the screen
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umi_drawgrid()

	{
	UM_coord cpln_pt;
	UM_coord	tmcs;
	UU_REAL  sx, sy;
	int	i, j;
	int xnum, ynum;
	int npts, ind;
	Gwpoint3 *mcs;
	Gwpoint3 mcssave[1000];

	uu_denter(UU_MTRC,(us,"umi_drawgrid()"));
	gsmarkcolor(UM_cpln.grid.color);
	gsmarktype(1);
	xnum = UM_cpln.grid.xnum;
	ynum = UM_cpln.grid.ynum;
	sx = UM_cpln.grid.sx;
	sy = UM_cpln.grid.sy;
	um_vctovc(UM_cpln.grid.cpln_ccllf, cpln_pt);
	npts = (xnum+1) * (ynum+1);
	/* mcs = (Gwpoint3 *) uu_malloc(npts*sizeof(Gwpoint3)); */
	ind = 0;
	mcs = mcssave;
	for (i=0; i<=ynum; i++)
		{
		for (j=0; j<=xnum; j++)
			{
			um_ccstomcs(0, cpln_pt, tmcs);
			(*mcs).x = tmcs[0];
			(*mcs).y = tmcs[1];
			(*mcs).z = tmcs[2];
			ind++;
			if (ind < 1000)
				mcs++;
			else
			  {
				mcs = mcssave;
				gpolymarker3(1000,mcs);
				ind = 0;
			  }
			cpln_pt[0] = cpln_pt[0] + sx;
			}
	 	cpln_pt[0] = UM_cpln.grid.cpln_ccllf[0];
		cpln_pt[1] = cpln_pt[1] + sy;
	   }
/*
	for (i=npts, j=0; i>0; i=i-1000, j=j+1000)
		{
		if (i > 1000)
			gpolymarker3(1000,mcssave[j]);
		else
			gpolymarker3(i,mcssave[j]);
		}
*/
	if (ind > 0)
		gpolymarker3(ind,mcssave);
	/* uu_free(mcssave);	*/
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION :  umi_snapping(pt,dx,dy)
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void umi_snapping(isllf,pt)
	UU_LOGICAL	isllf;
	UM_coord	pt;

	{
	int	scalx, scaly;
	UU_REAL	sx, sy;

	sx = UM_cpln.grid.sx;
	sy = UM_cpln.grid.sy;

	if (isllf)
	  {
		if (pt[0] > 0)
			scalx = (pt[0] + sx) / sx;
		else
			scalx = pt[0] / sx;
		if (pt[1] > 0)
			scaly = (pt[1] + sy) / sy;
		else
			scaly = pt[1] / sy;
	  }
	else
	  {
		if (pt[0] < 0)
			scalx = (pt[0] - sx) / sx;
		else
			scalx = pt[0] / sx;
		if (pt[1] < 0)
			scaly = (pt[1] - sy) / sy;
		else
			scaly = pt[1] / sy;
	  }
	pt[0] = scalx * sx;
	pt[1] = scaly * sy;
	}

/*********************************************************************
**    I_FUNCTION :  um_inactgrid(resetviewid)
**       Inactivate the construction plane grid.
**    PARAMETERS   
**       INPUT  : 
**          resetviewid				UU_TRUE => set viewid to -1
**											UU_FALSE => leave viewid alone
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_inactgrid(resetviewid)
	UU_LOGICAL resetviewid;

	{
	UV_vport  vport;

	uu_denter(UU_MTRC,(us,"um_inactgrid()"));
	if (UM_cpln.grid.dsegid != -1)
		gdeleteseg(UM_cpln.grid.dsegid);
	UM_cpln.grid.dsegid = -1;
	UM_cpln.grid.vpid = -1;
	if (resetviewid) UM_cpln.grid.viewid = -1;
	/*MILLS: so grid will be redisplayed appropriately */
	/* UM_cpln.grid.disp = UU_FALSE;  */
	UM_cpln.grid.snap = UU_FALSE;
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION :  um_actgrid(vpid, viewid)
**       Activate the construction plane grid.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_actgrid(vpid, viewid)
	UU_KEY_ID vpid;
	UU_KEY_ID viewid;

	{
	UV_vport  vport;

	uu_denter(UU_MTRC,(us,"um_actgrid(vpkey=%d,viewkey=%d)",vpid,viewid));
	UM_cpln.grid.vpid = vpid;
	UM_cpln.grid.viewid = viewid;
	UM_cpln.grid.snap = UU_TRUE;
	um_draw_grid(UU_FALSE);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     :
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_print_grid()

	{
	um_p_ary(UM_PINT, "vpid", 1, &UM_cpln.grid.vpid);
	um_p_ary(UM_PINT, "dsegid", 1, &UM_cpln.grid.dsegid);
	um_p_ary(UM_PINT, "disp", 1, &UM_cpln.grid.disp);
	um_p_ary(UM_PINT, "snap", 1, &UM_cpln.grid.snap);
	um_p_ary(UM_PFLOAT, "sx", 1, &UM_cpln.grid.sx);
	um_p_ary(UM_PFLOAT, "sy", 1, &UM_cpln.grid.sy);
	um_p_ary(UM_PFLOAT, "dx", 1, &UM_cpln.grid.dx);
	um_p_ary(UM_PFLOAT, "dy", 1, &UM_cpln.grid.dy);
	}

/*********************************************************************
**    E_FUNCTION     : um_init_grid()
**       Initialize grid.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_init_grid()

   {
   UM_initialize_grid = UU_TRUE;
   }
