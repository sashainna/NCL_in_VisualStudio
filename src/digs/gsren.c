/********************************************************************* 
**  NAME:  gsren.c
**
**  Contains:	
**
**  Gerror gssurfaceprop( ka, kd, ks, color, sex, kt ) -- 
**  Gerror ug_dsfillrep(ws,index,rep) --  Set fill area rep.
**  Gerror gsinteriorcolor( colr );	Set the direct color
**  Gerror gsinteriorshade( method );
**  Gerror gsinteriorlight( method );
**
**  COPYRIGHT  1987  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gsren.c , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:25
**
*********************************************************************/

#include <stdio.h>
#include "umath.h"
#include "udebug.h"
#include "umoveb.h"
#include "go.h"
#include "gdidd.h"
#include "gerror.h"
#include "gtbl.h"
#include "gtblvar8.h"
#include "gerrorst.h"
#include "grender.h"
#include "xenv1.h"
#include "umoveb.h"
/********************************************************************* 
**  E_FUNCTION:  Gerror gshlhsrmode(ws, mode) 
**		Set workstation hidden line/hidden surface removal mode.
**  PARAMETERS   
**      INPUT:  	
**			Gws *ws
**			Gint mode 			0 = No hidden line/hidden surface removal.
**									1 = Hidden line removal (not implemented).
**									2 = Hardware hidden surface removal.
**									3 = Software hidden surface removal.
**									4 = Software hidden surface removal, with dither.
**
**		  OUTPUT		none
**
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gshlhsrmode(ws, mode)
/*$ INPUT */
Gws *ws;
Gint mode;
{
	Gerror irtn=NCL_NO_ERROR;

	uu_denter(UU_GTRC,(us,"gshlhsrmode(%d, %d)",*ws, mode));

	if( mode < 0 || mode > 4 ) {
		uu_dprint(-1,(us,"ERROR gshlhsrmode: %d out of range", mode));
	}
	else {
		ug_gksstli.wsopen[*ws].outptr->rhlhsrmode = mode;	/* Requested mode */
		ug_gksstli.wsopen[*ws].outptr->hlhsrupdate = 1;		/* Update pending */
	}

	/* Inform workstation of the change */
/*	(*(ug_gksstli.wsopen[*ws].connid)[UG_DHLHSRMODE])(ws, mode); */
/*	ug_dshlhsrmode(ws, mode); */

	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gssurfaceprop( ka, kd, ks, color, sex, kt ) -- 
**			Set reflectivity coefficents.
**  PARAMETERS   
**						Gfloat ka		Ambient reflection coefficient.
**      			 	Gfloat kd		Diffuse reflction coefficent.
**      			 	Gfloat ks		Specular reflction coefficient.
**						Gcolr scolor	Specular highlight color.
**      			 	Gfloat sex		Specular exponent.
**						Gfloat kt		Transparency coefficient.
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gssurfaceprop( ka, kd, ks, scolor, sex, kt )
/*$ INPUT */
Gfloat ka, kd, ks, sex, kt;
Gcolr *scolor;
{
	Gerror irtn;

	uu_denter(UU_GTRC,(us,"gssurfaceprop(%f %f %f color(%f %f %f %d) %f, %f)",
		ka,kd,ks, scolor->red,scolor->green,scolor->blue,scolor->model, sex, kt));

	/* DEBUG...need to add this command to segment data */
	ug_gksstli.rendats.ffambrfl = ka;
	ug_gksstli.rendats.ffdifrfl = kd;
	ug_gksstli.rendats.ffspcrfl = ks;
	ug_gksstli.rendats.ffspcexp = sex;
	ug_gksstli.rendats.fftrnsp = kt;
	ug_gksstli.rendats.ffspccolr.red = scolor->red;
	ug_gksstli.rendats.ffspccolr.green = scolor->green;
	ug_gksstli.rendats.ffspccolr.blue = scolor->blue;
	ug_gksstli.rendats.ffspccolr.model = scolor->model;

	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}
/********************************************************************* 
**  E_FUNCTION:  Gerror gsinteriorcolor( colr );	Set the direct color
**						-- Set direct color of fill area interiors.
**  PARAMETERS   
**      INPUT:  	Gcolr colr			Color of interior and color model.
**		  OUTPUT:	none
**
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  The specified direct interior color is set.
**  WARNINGS     :  none
*********************************************************************/
Gerror gsinteriorcolor( colr )	/* Set the direct color */
/*$ INPUT */
Gcolr *colr;
{
	Gerror irtn=NCL_NO_ERROR;						/* Return value */

	uu_denter(UU_GTRC,(us,"gsinteriorcolor( color=%f %f %f, model=%d )",
		colr->red, colr->green, colr->blue, colr->model));

#ifdef UU_CHECK
	if( (colr->red<0.)  || ( colr->red>1.0) ||
		 (colr->green<0.)|| ( colr->green>1.0) ||
		 (colr->blue<0.) || ( colr->blue>1.0)) {
		ug_errorhand(ECOLRNGE,"gsinteriorcolor",&colr); irtn=ECOLRNGE;
	}
	if (irtn!=NCL_NO_ERROR) {
		uu_dprint(-1,(us,"ERROR gsinteriorcolor. no color set"));
		uu_dexit;
		return(irtn);
	}
#endif

	/* Save this color in the phigs state list */
	ug_gksstli.rendats.ffcolr.red   = colr->red;
	ug_gksstli.rendats.ffcolr.green = colr->green;
	ug_gksstli.rendats.ffcolr.blue  = colr->blue;
	ug_gksstli.rendats.ffcolr.model = colr->model;

	uu_dexit;
	return(irtn);
}


/********************************************************************* 
**  E_FUNCTION:  Gerror gsinteriorshade( method );
**						-- Set interior shading method.
**  PARAMETERS   
**      INPUT:  	Gint  method		<=0	Implementation dependent.
**												1		NONE. Faceted shading.
**												2		COLOUR. Gourad shading.
**												3		DOT PRODUCT. Not implemented.
**												4		NORMAL. Phong shading.
**												>=5	Reserved for future standardization.
**		  OUTPUT:	none
**
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  The specified direct interior color is set.
**  WARNINGS     :  This is a huge KLUDGE for workstations which
**						  use the software rendering simulations.  These
**						  workstations take gsinteriorshade(0) as the clue
**						  to render the primitives which have been traversed
**						  since a gsinteriorshade( >0 ) call.  Each primitive
**						  should really be rendered as it is traversed.
*********************************************************************/
Gerror gsinteriorshade( method )	/* Set the interior shading method */
/*$ INPUT */
Gint method;
{
	Gerror irtn=NCL_NO_ERROR;						/* Return value */
	struct { int op; int id; int method; } prms;

	uu_denter(UU_GTRC,(us,"gsinteriorshade( %d )", method ));

	/* DEBUG... Needs to be added to segment data! */
	/* Save interior shading method */
	ug_gksstli.rendats.intrshademethod = method;

	/* Call workstation */
	prms.op=UG_DINTRSHADE;
	prms.method = method;
	ug_wkout(&prms,2);

	uu_dexit;
	return(irtn);
}
/********************************************************************* 
**  E_FUNCTION:  Gerror gsinteriorlight( method );
**						-- Set interior lighting method.
**  PARAMETERS   
**      INPUT:  	Gint  method		<=0	Implementation dependent.
**												1		No lighting calculations performed.
**														Use per vertex colour or PHIGS index.
**												2		Ambient term of the lighting eqn.
**												3		Ambient and Diffuse terms of the
**														lighting equation.
**												4		Ambient, Diffuse, and Specular terms
**														of the lighting equation.
**												>=5	Reserved for future standardization.
**		  OUTPUT:	none
**
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  The specified direct interior color is set.
**  WARNINGS     :  none
*********************************************************************/
Gerror gsinteriorlight( method )	/* Set the interior lighting method */
/*$ INPUT */
Gint method;
{
	Gerror irtn=NCL_NO_ERROR;						/* Return value */

	uu_denter(UU_GTRC,(us,"gsinteriorlight( %d )", method ));

	/* Save interior lighting method */
	ug_gksstli.rendats.intrlightmethod = method;

	/* DEBUG... Needs to be added to segment data! */

	uu_dexit;
	return(irtn);
}

