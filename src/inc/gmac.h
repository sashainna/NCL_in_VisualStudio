
/********************************************************************* 
**  NAME:	gmac.h
**		
**		CONTAINS:
**					GQRAS_CHARWIDTH
**					GQRAW_CHARHEIGHT
**					UG_DATEXT_CALL
**					UG_D1CHAR_CALL
**					UG_DANSIRGT_CALL
**					UG_DANSILFT_CALL
**
**
**  	COPYRIGHT  1984  UNICAD, Inc.
**  	MODULE NAME AND RELEASE LEVEL 
**       gmac.h , 25.1
**  	DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:20
**
*********************************************************************/

#ifndef GMACH


#define GQRAS_CHARWIDTH(ws) \
	( (ug_gksstli.wsopen[(ws)].wdtptr->dspsize.raster.x) / \
	(ug_gksstli.wsopen[(ws)].wdtptr->colmax) )

#define GQRAS_CHARHEIGHT(ws) \
	( (ug_gksstli.wsopen[(ws)].wdtptr->dspsize.raster.y) / \
	(ug_gksstli.wsopen[(ws)].wdtptr->rowmax) )

#define UG_DATEXT_CALL(ws) 	(*(ug_gksstli.wsopen[(ws)].connid)[UG_DATEXT])
#define UG_D1CHAR_CALL(ws)		(*(ug_gksstli.wsopen[(ws)].connid)[UG_D1CHAR])
#define UG_DANSIRGT_CALL(ws)  (*(ug_gksstli.wsopen[(ws)].connid)[UG_DANSIRGT])
#define UG_DANSILFT_CALL(ws) 	(*(ug_gksstli.wsopen[(ws)].connid)[UG_DANSILFT])

#define GMACH
#endif
