/********************************************************************* 
**  NAME:   gksomisc.h
**
**		Output Type definitions for GKS. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gomisc.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:22
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
/* From ANSI X3H34/83-12R1, the GKS C-language binding */

#ifndef GKSOMISCH		
#include "gobas.h"
#include "go1.h"
#include "go2.h"
/**********		MISCELLANEOUS TYPES	**********/

/* Gmodtran -- composition type of a MODeling TRANsform */

	typedef enum { 
						UG_PRECONCATENATE, 
						UG_POSTCONCATENATE, 	
						UG_MODREPLACE 
		  } Gmodtran ;

/* Gcolor  -  COLOR index */

	typedef Gint	Gcolor;


/* Ginten  -  color INTENsity */

	typedef Gfloat	Ginten;


/* Gasfs  -  Aspect Source FlagS */

	typedef struct	{
		Gasf		ln_type;		/* line type */
		Gasf		ln_width;	/* linewidth scale factor */
		Gasf		ln_color;	/* polyline color */
		Gasf		mk_type;		/* marker type */
		Gasf		mk_size;		/* marker size scale factor */
		Gasf		mk_color;	/* polymarker color */
		Gasf		tx_fp;		/* text font and precision */
		Gasf		tx_exp;		/* text character expansion factor */
		Gasf		tx_space;	/* text character spacing */
		Gasf		tx_color;	/* text color */
		Gasf		fl_inter;	/* fill area interior style */
		Gasf		fl_style;	/* fill area style index */
		Gasf		fl_color;	/* fill area color */
	} Gasfs;


/* Gdefer  -  DEFERral state */

	typedef struct	{
		Gdefmode	defmode;		/* deferral mode */
		Girgmode	irgmode;		/* implicit regeneration mode */
	} Gdefer;


/* Gdspsize  -  DiSPlay SIZE */

	typedef struct	{
		Gdevunits	units;	/* device coordinate units */
		Gdpoint	device;		/* device coordinate unit size */
		Gipoint	raster;		/* raster unit size */
	} Gdspsize;


/* Gerror  -  ERROR code */

	typedef Gint	Gerror;


/* Ggdp - GDP. PHIGS extension. Not in GKS  */

	typedef struct {			/* not used now */
		Gint n_attrsets;		/* number of sets of attributes used */
		Gint *attrsets;		/* pointer to list of sets of attributes */
	} Ggdp;

/* Ggksmit  -  GKS Metafile ITem */

	typedef struct	{
		Gint		type;			/* item type */
		Gint		length;		/* item length */
	} Ggksmit;


/* Gindex  -  bundle INDEX */

	typedef Gint	Gindex;


/* Gbii  -  Bundle Index Information */

	typedef struct	{
		Gint		number;		/* number of bundles */
		Gbis		defined;		/* defined bundles */
	} Gbii;
#define GKSOMISCH
#endif
