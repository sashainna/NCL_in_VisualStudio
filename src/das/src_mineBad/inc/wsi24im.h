/********************************************************************* 
**  NAME:  wsi24im.h 
**
**      Unicad immediate mode macros.  Used in segmment traversing
**			when subroutines calls are too expensive to use.  Only
**			wsi24seg.c and wsi24pk.c use this file
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       wsi24im.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:10
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#include "wsi24.h"

#define uw_iriscolor( index )	\
	if( uw_iris.cur_surf == GRAPHIC ) {	\
		im_color( index );	\
	} \
	else {	\
		im_color((index+1) << uw_iris.ngraphplanes); \
	}


#define uw_imidentx() { \
	static Matrix m = {	0.0019531250, 0.0, 0.0, 0.0, \
								0.0, 0.0026041666, 0.0, 0.0, \
								0.0, 0.0, -0.001953125, 0.0, \
								-0.9990234375, -0.9986979166, -0.9990234375, 1.0 }; \
	\
	uw_irispushx();	\
	viewport(0,1023,0,767);	\
	im_loadmatrix(m);	\
}


#define uw_impushx() \
	im_pushmatrix(); pushviewport()

#define uw_impopx() \
	im_popmatrix(); popviewport()

#define uw_irismarker(x, y, z, mtyp) \
{ \
 \
	/* Matrix created by ortho2(-0.5, 1023.5, -0.5, 767.5) */ \
	static Matrix m = {	0.0019531250, 0.0, 0.0, 0.0, \
								0.0, 0.0026041666, 0.0, 0.0, \
								0.0, 0.0, -1.0, 0.0, \
								-0.9990234375, -0.9986979166, 0.0, 1.0 }; \
 \
	/* Definition of circle marker */ \
	static float circ[][2] = {  \
				2.500000, -0.669873, 1.830127, -1.830127, \
				0.669873, -2.500000, -0.669873, -2.500000, \
				-1.830128, -1.830128, -2.500000, -0.669872, \
				-2.500000, 0.669873, -1.830127, 1.830128, \
				-0.669872, 2.499999, 0.669874, 2.500001, \
				1.830128, 1.830126, 2.500000, 0.669872 }; \
 \
 \
	uw_iriscolor(ug_gksstli.curprats.mkbundl.color); \
	im_setlinestyle(1); \
	im_linewidth(1); \
 \
	switch( mtyp ) { \
 \
		case 1:  /* dot */ \
 \
					im_pnt(x, y, z); \
					break; \
 \
		case 2:	/* plus */ \
 \
					im_move(x, y, z); /* 3-D move to marker position */ \
 \
					/* Relative draw in raster coordinates */	 \
					im_pushmatrix(); \
					im_loadmatrix(m); \
					im_rmv2i(-5,0);	im_rdr2i(10,0); \
					im_rmv2i(-5,-5);	im_rdr2i(0,10); \
					im_popmatrix(); \
					break; \
 \
		case 3:	/* star */ \
 \
					im_move(x, y, z); /* 3-D move to marker position */ \
 \
					/* Relative draw in raster coordinates */	 \
					im_pushmatrix(); \
					im_loadmatrix(m); \
					im_rmv2i(-5,0);	im_rdr2i(10,0); \
					im_rmv2i(-5,-5);	im_rdr2i(0,10); \
					im_rmv2i(-3,-2);	im_rdr2i(6,-6); \
					im_rmv2i(-6,0);	im_rdr2i(6,6); \
					im_popmatrix(); \
					break; \
 \
		case 4:	/* circle */ \
 \
					im_move(x, y, z); /* 3-D move to marker position */ \
 \
					/* Relative draw in raster coordinates */	 \
					im_pushmatrix(); \
					im_loadmatrix(m); \
 \
					/* Move to beginning */ \
					im_rmv2(0.0,5.0); \
 \
					/* Draw the circle */ \
					for(i=0; i<12; i++) \
						im_rdr2(circ[i][0], circ[i][1]); \
					 \
					im_popmatrix(); \
					break; \
 \
		case 5:	/* cross	*/ \
 \
					im_move(x, y, z); /* 3-D move to marker position */ \
 \
					/* Relative draw in raster coordinates */	 \
					im_pushmatrix(); \
					im_loadmatrix(m); \
					im_rmv2i(-10,-10);	im_rdr2i(20,20); \
					im_rmv2i(-20,0); 		im_rdr2i(20,-20); \
					im_popmatrix(); \
					break; \
 \
		case 7:  /* diamond */ \
 \
					im_move(x, y, z); /* 3-D move to marker position */ \
 \
					/* Relative draw in raster coordinates */	 \
					im_pushmatrix(); \
					im_loadmatrix(m); \
					im_rmv2i(0,5);	 \
					im_rdr2i(5,-5); im_rdr2i(-5,-5);	 \
					im_rdr2i(-5,5); im_rdr2i(5,5); \
					im_popmatrix(); \
					break; \
 \
 \
		case 8:  /* square */ \
 \
					im_move(x, y, z); /* 3-D move to marker position */ \
 \
					/* Relative draw in raster coordinates */	 \
					im_pushmatrix(); \
					im_loadmatrix(m); \
					im_rmv2i(5,5);	 \
					im_rdr2i(0,-10);	im_rdr2i(-10,0);	 \
					im_rdr2i(0,10);	im_rdr2i(10,0); \
					im_popmatrix(); \
					break; \
 \
		default: im_pnt(x, y, z);					/* dot */ \
					break; \
	} \
 \
	im_setlinestyle(ug_gksstli.curprats.lnbundl.type.typeno); \
	im_linewidth(ug_gksstli.curprats.lnbundl.width); \
 \
}

	/* copy structure by hand for double/single precision */
/*#define ug_linetype(typ) zbytecp(ug_gksstli.curprats.lnbundl.type,*(typ))*/
#define ug_linetype(typ) {						\
 	ug_gksstli.curprats.lnbundl.type.typeno = (*typ).typeno;	\
 	ug_gksstli.curprats.lnbundl.type.npatn = (*typ).npatn;		\
 	ug_gksstli.curprats.lnbundl.type.patnlen = (*typ).patnlen;	\
 	strcpy(ug_gksstli.curprats.lnbundl.type.typepatn,(*typ).typepatn);	\
 }

#define ug_linewidth(wd) ug_gksstli.curprats.lnbundl.width = (wd)
#define ug_textfp(fopr) ug_gksstli.curprats.txbundl.fp.font = (fopr)->font; \
							 	ug_gksstli.curprats.txbundl.fp.prec = (fopr)->prec
#define ug_charexp(e) ug_gksstli.curprats.txbundl.expn = (e)

/* shift these into c code - too many defines :
#define ug_txplane(tpvc) uw_ItoGpoint3((tpvc),ug_gksstli.curprats.txpvec)
#define ug_charup3(upvec) uw_ItoGpoint3((upvec),ug_gksstli.curprats.txuv)
/* */
#define ug_charup(up) { \
	extern Gfloat ug_chhtsclflag[]; \
	for (i=0; i<UG_MAXNTRAN; i++) ug_chhtsclflag[i]=0; \
	ug_gksstli.curprats.txuv.x = (*up).x; \
	ug_gksstli.curprats.txuv.y = (*up).y; \
	ug_gksstli.curprats.txuv.z = 0.0; \
}
#define ug_textpath(path) ug_gksstli.curprats.txpath = (path)
#define ug_charspace(spacing) ug_gksstli.curprats.txbundl.space = (spacing);
#define ug_textalign(align) ug_gksstli.curprats.txalign.hor = (align)->hor; \
									ug_gksstli.curprats.txalign.ver = (align)->ver
#define ug_marktype(tp) ug_gksstli.curprats.mkbundl.type = (tp)
#define ug_spickid(id) ug_gksstli.curprats.pickid = (id)
#define ug_linecolor(co) ug_gksstli.curprats.lnbundl.color = (co)
#define ug_markcolor(co) ug_gksstli.curprats.mkbundl.color = (co)
#define ug_textcolor(co) ug_gksstli.curprats.txbundl.color = (co)
#define ug_fillcolor(co) ug_gksstli.curprats.flbundl.color = (co)
#define ug_sedgeflag(f) ug_gksstli.curprats.flbundl.edgeflag = (f)
