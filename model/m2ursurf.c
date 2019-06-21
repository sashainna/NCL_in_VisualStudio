/*********************************************************************
**    NAME         :  m2ursurf.c
**       CONTAINS:
**       render_surf()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m2ursurf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:50
*********************************************************************/

/* This set of includes is marginal on the sun */
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "ginqxf.h"
#include "gtblvar4.h"
#include "gtblst.h"
#include "gdidd.h"
#include "mnns.h"
#include "view.h"
#include "dselmask.h"
#include "dasnog.h"
#include "dinput.h"
#include "class.h"
#include "uims.h"
#include "xenv1.h"
#include "dselect.h"

extern int *UD_ksws;					/* Current workstation id */

UU_LOGICAL	ud_gnxt();

/* External variables for tesselation code */
int um_renmode=0;						/* 0=Hardware rendering, 1=Software */
int um_lightmodel=4;					/* <=0	Not used.
											 *	1		NONE. Faceted shading.
											 *	2		COLOUR. Gourad shading.
											 *	3		DOT PRODUCT. Not implemented.
											 *	4		NORMAL. Phong shading.
											 *	>=5	Not used.
											 */

static	int segno;					/* Segment number of surf rendering */
static Gwrect3 window;				/* Current window */

/* int UMI_lu; */
/* UM_SORTA UMI_sorttype;				/* flag determining sorting algorithm */
/* UU_REAL UMI_lightpos[3];			/* light source position					*/
/* UM_LIGHT UMI_light;					/* Light positioning value 		*/
/* UM_POLYOUT UMI_polyout;				/* Output value (to screen, file 	*/
/* Surfatts	*umi_slist_head;			/* head of surface list 					*/
/* Polygon	*umi_list1_head,			/* head of polygon list 1					*/
/* 			*umi_list2_head;			/* head of polygon list 2					*/

/*	Moved outside function for Silicon Graphics C compiler. */
static struct
	UC_entitydatabag 	surface;			/* General surface variables 		*/

/******************************************************************************
**    E_FUNCTION     :  um_render_surf()
**       Lets user pick surfaces to render, tesselates and renders.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
******************************************************************************/
void um_render_surf()
{

/*		--- local variables ---    */

	int i;
	int len;
	UU_LOGICAL initialize;				/* flag for ud_gnxt */
	Gcobundl *cb;							/* color bundle */
	Gcobundl *gqcolorrep();				/* Gks inquiry function */
	UU_KEY_ID key;							/* Entity id  for surface	 */
	int rel_num;		/*RAH: rel_num of selected geo */
	int colorindex;						/* vlt index */
	UV_vport vvport[6];					/* Contains viewport information */
	int nviews, sav_mode, save_ptype;		
	int mat;									/* Material table index */
	UU_REAL kd, ks, spec_exp;			/* Material attributes */
	UU_REAL ka, kt;						/* More material attributes */
	char *ux_getenv();
	Gcolr color;							/* A Color and its model space */
	extern Gcobundl UD_colortable[];	/* Das color definitions */

	uu_denter(UU_MTRC,(us,"um_rensurf()"));

	/* MILLS: a) force initialization of rendering modals and b) display error
			  when attempting HARDWARE rendering on systems without */
	if (um_renmode == 0)
		/* RENDERING MODALS NOT INITIALIZED */
		ud_prmerr("Surface rendering modals not initialized.  Navigate RENDERING MODALS.");

#if (UU_COMP == UU_SUN) || (UU_COMP == UU_VAXVMS)
	if (um_renmode == 2)
		/* HARDWARE RENDERING NOT AVAILABLE ON THIS TERMINAL - yet */
		ud_prmerr("HARDWARE surface rendering not available on this terminal.  Using SOFTWARE.");
#endif
	/* MILLS: EOC */

	/* Get the current viewport */
	uvu_pickvp(&vvport[0]);
	nviews = 1;

/*
.....we only render surf in normal view
.....if the picking type is not UD_PICK_NORMAL
.....we save it and reset after we done
*/
	if (!ud_verifypick_type(&vvport[0],UU_TRUE)) return;
	sav_mode = 0;
	if ((ud_getpick_type()==UD_PICK_SECONDARY)
			|| (ud_getpick_type()==UD_PICK_INVISIBLE))
	{
		save_ptype = ud_getpick_type();
		ud_setpick_type(UD_PICK_NORMAL);
		sav_mode = 1;
	}
	/*	Pick surfaces to render, limit das to only pick surfaces */ 
	ud_lgeo(UU_TRUE, UD_render);
	/*RAH: use of SELECT subsystem bypasses filter set above ... */
	ud_ldas(UD_DASSELECT, VIEWERROR, 4, NULL, 1, &len, UD_NODEFAULT); 

	/* For each surface, set up color, add to list, and tesselate */
	initialize = UU_TRUE;
	surface.key = 0;
	while(ud_gnxt(initialize, UU_NULL, &key, 1) == UU_TRUE)
	{
		initialize = UU_FALSE;
		/* RAH: filter out invalid geometry */
		ur_retrieve_data_relnum (key, &rel_num);
		if (!uu_tst_bit(UD_render, rel_num-1)) continue;

		if( surface.key == 0 ) um_pre_render( &vvport[0]);

		/* Get all the geometry for this surface */
		surface.key = key;
		uc_retrieve_data(&surface, sizeof(struct UC_entitydatabag));

		/* Set the color for this surface */
		ur_retrieve_color(key,&colorindex);
		cb = &UD_colortable[colorindex];
		color.red   = cb->red;
		color.green = cb->green;
		color.blue  = cb->blue;
		color.model = 1;				/* RGB color model */
		gsinteriorcolor( &color );	/* Set the direct color */
		gsfillcolor(colorindex);	/* Set the fill color index */

		/* Set digs surface properties */
		um_retrieve_material( &surface, &mat, &kd, &ks, &spec_exp );
		kt = 0.0;			/* No transparency */
		ka = 1.0;			/* Reflects all ambient light */
		uu_dprint(UU_MTRC,(us,"kd %f",kd));
		uu_dprint(UU_MTRC,(us,"ks %f",ks));
		uu_dprint(UU_MTRC,(us,"sex %f",spec_exp));
		color.red   = 1.0;		/* White specular highlights */
		color.green = 1.0;
		color.blue  = 1.0;
		color.model = 1;			/* Rgb color space */
		gssurfaceprop( ka, kd, ks, &color, spec_exp, kt );

		/*	Go tesselate this surface */
		for(i=0; i<nviews; i++) {
			gsnormtran(vvport[i].xform);
			uu_dprint(UU_GITRC,(us,"curvwindex=%d", ug_gksstli.curvwindex));
			uc_srf_tessellate( &surface );
		}

	}

	/* If anything picked for rendering, do cleanup here */
	/*RAH added check of surface key just in case nothing was selected. */
	if( !initialize  && (surface.key > 0)) {
		um_post_render( &vvport[0] );
	}
	if (sav_mode)
		ud_setpick_type(save_ptype);
	uu_dexit;
}


/******************************************************************************
**    S_FUNCTION     :  static um_pre_render()
**       Initializes DIGS rendering.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
******************************************************************************/
static um_pre_render(vvport)
UV_vport *vvport;
{
	UG_vtran3 gvtran3;					/* viewing definitons */
	Gcobundl *gqcolorrep();				/* Gks inquiry function */
	UV_view view;							/* Current view information */
	Gwrect3 box;							/* Bounding box. */
	Gnrect3 vp;								/* Digs viewport */
	Gfloat zwidth;							/* Width viewport */
	extern Gcobundl UD_colortable[];	/* Das color definitions */
	UU_LOGICAL ok;

	uu_denter(UU_MTRC,(us,"um_pre_render()"));

	/* Temporarily set the window in z to match extreams of data.
	 * This minimized the depth buffer aliasing on z-buffer hardware. 
	 */
	ok = gextrema(vvport->xform, &box);
	if( !ok ) {
		uu_dprint(-1,(us,"ERROR. gextrema failed"));
	}

	/* Get the view corresponding to viewport */
	uv_getvid(vvport->cur_view, &view);

	uv_extrema_project(&box.llf,&box.urb,&view);
	uu_move_byte( gqwindow3( vvport->xform ), &window, sizeof( Gwrect3 ));

	box.llf.x = window.llf.x;							/* Use original xy window */
	box.llf.y = window.llf.y;
	box.urb.x = window.urb.x;
	box.urb.y = window.urb.y;
	box.llf.z += ((box.llf.z-box.urb.z) * 0.1 + 1.0);	/* Goose the z-planes */
	box.urb.z -= ((box.llf.z-box.urb.z) * 0.1 + 1.0);
	gswindow3( vvport->xform, &box );

	/* Digs requires that the x/z aspect ratio of the viewport match the
	 * aspect ratio of the window.  If they don't match, normals transformed
	 * into ndc space change direction, this destroys the rendering.  We
	 * temporarily set the viewport to the correct aspect ratio here.
	 */
	uu_move_byte(gqvport3(vvport[0].xform), &vp, sizeof(Gnrect3));
	zwidth = (box.llf.z-box.urb.z)*(vp.urb.x-vp.llf.x)/(box.urb.x-box.llf.x);
	vp.llf.z = zwidth / 2.0;
	vp.urb.z = -vp.llf.z;
	gsview3( vvport[0].xform, &vp );

	/* Get the current viewing information */
	ug_iview( &gvtran3 );

	/* Blank the existing geometry */
	uv_blankvp(vvport, UG_INVISIBLE);

	/* Set the correct rendering mode 0=no rendering, 2=hardware, 
	 * 3=software, 4=software with dither.
	 */
	gshlhsrmode(UD_ksws, um_renmode);

	/* Create the render segment...many devices can't support a
	 * surface rendering inside a segment.  This is mostly for
	 * the Graphicon, which requires that renderings be inside
	 * a segment 
	 */
	segno = gnseg();
	gcreateseg(segno);

	gsinteriorshade( um_lightmodel );	/* Set Phong interior shading method */
	gsinteriorlight( 4 );					/* Use all terms of lighting equation */

	uu_dexit;
}


/******************************************************************************
**    S_FUNCTION     :  static um_post_render()
**       Finishes a DIGS rendering.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
******************************************************************************/
static um_post_render(vvport)
UV_vport *vvport;
{

	uu_denter(UU_MTRC,(us,"um_post_render()"));

	/* Close the digs segment */
	gcloseseg();

	/* Request terminal input before removing image */
	ud_hakt(VIEWERROR, 5);

	gdeleteseg(segno);			/* Delete the render segment */
	gsinteriorshade( 0 );		/* Set no interior shading method */
	gsinteriorlight( 0 );		/* Set no interior lighting method */
	gshlhsrmode(UD_ksws, 0);	/* Set no hidden surface removal */
	gswindow3( vvport->xform, &window );	/* Restore the window */
	uv_blankvp(vvport, UG_VISIBLE);	/* Return to wireframe mode */

	uu_dexit;

}
