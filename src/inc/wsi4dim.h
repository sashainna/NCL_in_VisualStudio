/********************************************************************* 
**  NAME:  wsi4dim.h 
**
**      Unicad immediate mode macros.  Used in segment traversing
**			when subroutines calls are too expensive to use.  Only
**			wsi4dseg.c and wsi4dpk.c use this file
**
**  COPYRIGHT  1988  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL 
**       wsi4dim.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:11
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#include "wsi4d.h"

#define uw_imidentx() { \
	static Matrix m = {	0.0015625, 0.0, 0.0, 0.0, \
								0.0, 0.00199203, 0.0, 0.0, \
								0.0, 0.0, -0.0015625, 0.0, \
								-0.999219, -0.999004, -0.999219, 1.0 }; \
	\
	uw_4dpushx();	\
	viewport(0,1279,0,1023-20);	\
	loadmatrix(m);	\
}

/*
.....Don't use this any more
.....Bobby  -  9/25/91
*/
/*#define uw_4dmarker(x, y, z, mtyp) \
/*{ 													\
/* 													\
	/* Matrix created by ortho2(-0.5, 1279.5, -0.5, 1023.5-20)  \*/
/*	static Matrix m = {	0.0015625, 0.0, 0.0, 0.0, \
/*								0.0, 0.00199203, 0.0, 0.0, \
/*								0.0, 0.0, -1.0, 0.0, \
/*								-0.999219, -0.999004, 0.0, 1.0 }; \
/*\
/*	/* Definition of circle marker  \*/
/*	static float circ[][2] = {  \
/*				2.500000, -0.669873, 1.830127, -1.830127, \
/*				0.669873, -2.500000, -0.669873, -2.500000, \
/*				-1.830128, -1.830128, -2.500000, -0.669872, \
/*				-2.500000, 0.669873, -1.830127, 1.830128, \
/*				-0.669872, 2.499999, 0.669874, 2.500001, \
/*				1.830128, 1.830126, 2.500000, 0.669872 }; \
/* \
/* \
/*	uw_4dcolor(ug_gksstli.curprats.mkbundl.color); \
/*	setlinestyle(1); \
/*	linewidth(1); \
/* \
/*	switch( mtyp ) { \
/* \
/*		case 1:  /* dot  \*/
/* \
/*					pnt(x, y, z); \
/*					break; \
/* \
/*		case 2:	/* plus  \*/
/* \
/*					move(x, y, z); /* 3-D move to marker position  \*/
/* \
/*					/* Relative draw in raster coordinates 	 \*/
/*					pushmatrix(); \
/*					loadmatrix(m); \
/*					rmv2i(-5,0);	rdr2i(10,0); \
/*					rmv2i(-5,-5);	rdr2i(0,10); \
/*					popmatrix(); \
/*					break; \
/* \
/*		case 3:	/* star  \*/
/* \
/*					move(x, y, z); /* 3-D move to marker position  \*/
/* \
/*					/* Relative draw in raster coordinates 	 \*/
/*					pushmatrix(); \
/*					loadmatrix(m); \
/*					rmv2i(-5,0);	rdr2i(10,0); \
/*					rmv2i(-5,-5);	rdr2i(0,10); \
/*					rmv2i(-3,-2);	rdr2i(6,-6); \
/*					rmv2i(-6,0);	rdr2i(6,6); \
/*					popmatrix(); \
/*					break; \
/* \
/*		case 4:	/* circle  \*/
/* \
/*					move(x, y, z); /* 3-D move to marker position  \*/
/* \
/*					/* Relative draw in raster coordinates 	 \*/
/*					pushmatrix(); \
/*					loadmatrix(m); \
/* \
/*					/* Move to beginning  \*/
/*					rmv2(0.0,5.0); \
/* \
/*					/* Draw the circle  \*/
/*					for(i=0; i<12; i++) \
/*						rdr2(circ[i][0], circ[i][1]); \
/*					 \
/*					popmatrix(); \
/*					break; \
/* \
/*		case 5:	/* cross	 \*/
/* \
/*					move(x, y, z); /* 3-D move to marker position  \*/
/* \
/*					/* Relative draw in raster coordinates 	 \*/
/*					pushmatrix(); \
/*					loadmatrix(m); \
/*					rmv2i(-10,-10);	rdr2i(20,20); \
/*					rmv2i(-20,0); 		rdr2i(20,-20); \
/*					popmatrix(); \
/*					break; \
/* \
/*		case 7:  /* diamond  \*/
/* \
/*					move(x, y, z); /* 3-D move to marker position  \*/
/* \
/*					/* Relative draw in raster coordinates 	 \*/
/*					pushmatrix(); \
/*					loadmatrix(m); \
/*					rmv2i(0,5);	 \
/*					rdr2i(5,-5); rdr2i(-5,-5);	 \
/*					rdr2i(-5,5); rdr2i(5,5); \
/*					popmatrix(); \
/*					break; \
/* \
/* \
/*		case 8:  /* square  \*/
/* \
/*					move(x, y, z); /* 3-D move to marker position  \*/
/* \
/*					/* Relative draw in raster coordinates 	 \*/
/*					pushmatrix(); \
/*					loadmatrix(m); \
/*					rmv2i(5,5);	 \
/*					rdr2i(0,-10);	rdr2i(-10,0);	 \
/*					rdr2i(0,10);	rdr2i(10,0); \
/*					popmatrix(); \
/*					break; \
/* \
/*		default: pnt(x, y, z);					/* dot  \*/
/*					break; \
/*	} \
/* \
/*	setlinestyle(ug_gksstli.curprats.lnbundl.type.typeno); \
/*	linewidth((short)ug_gksstli.curprats.lnbundl.width); \
/* \
/*}
*/
		/* if segment data is not same precision as build, this copy must
			be done by hand as in wsi24im.h */
#define ug_linetype(typ) zbytecp(ug_gksstli.curprats.lnbundl.type,*(typ))
#define ug_linewidth(wd) ug_gksstli.curprats.lnbundl.width = (wd)
#define ug_textfp(fopr) ug_gksstli.curprats.txbundl.fp.font = (fopr)->font; \
							 	ug_gksstli.curprats.txbundl.fp.prec = (fopr)->prec
#define ug_charexp(e) ug_gksstli.curprats.txbundl.expn = (e)
#define ug_txplane(tpvc) zbytecp(ug_gksstli.curprats.txpvec, *(tpvc))
#define ug_charup3(upvec) zbytecp( ug_gksstli.curprats.txuv, *(upvec))
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
