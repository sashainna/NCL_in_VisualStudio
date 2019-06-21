/*********************************************************************
**
**    NAME         :  dselect.h
**
**       CONTAINS:
**       	common for select subsystem
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       dselect.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:14
**
*********************************************************************/

#ifndef DSELECTH

#include "usysdef.h"
#include "mdattr.h"
#include "mdrel.h"
#include "ribase.h"

/*	-- Define Section -- */
#define UD_MAX_MARKERTYPES 11
#define UD_NLAYER_WORDS (UM_MAX_LAYERS/32) + 1
#define UD_NENTITY_WORDS (UR_MAX_REL/32) + 1
#define UD_NPEN_WORDS (UM_MAX_PENS/32) + 1
#define UD_NCOLOR_WORDS (UM_MAX_COLORS/32) + 1
#define UD_NLINESTYLE_WORDS (UM_MAX_LINESTYLES/32) + 1
#define UD_NMARKERTYPE_WORDS (UD_MAX_MARKERTYPES/32) + 1

#define UD_INCLUDE 0
#define UD_EXCLUDE 1
#define UD_INACTIVE 2

#define UD_SELECTXIN  0
#define UD_SELECTXOUT 1
#define UD_SELECTIN   2
#define UD_SELECTOUT  3

#define UD_PICK_NORMAL 0
#define UD_PICK_INVISIBLE 1
#define UD_PICK_SECONDARY 2
#define UD_PICK_ASSIST 3
#define UD_PICK_MOTION 4
#define UD_PICK_MOTSEG 5

#define	UD_NUMMSEL 11

/*	-- Macro Section -- */

#define UD_BUFBUSY()		UD_Selbusy = UU_TRUE; 		/* set buffer busy */
#define UD_BUFFREE()		UD_Selbusy = UU_FALSE; 		/* set buffer free */
#define UD_BUFTEST()		UD_Selbusy 						/* test buffer busy */

/*	-- Typedef Section -- */

/*
.....Chain Modal structure
*/
   typedef struct
   {
		UU_LOGICAL conditional;
		UU_LOGICAL planar;
		UU_REAL toler;
		char plane[15];
		UU_LOGICAL lines;
		UU_LOGICAL circles;
		UU_LOGICAL splines;
		UU_LOGICAL composites;
		UU_REAL surftoler;
		int vectors;
		int direction;
		int mult_surf;
   } UD_chain_mod_struc ;


	typedef struct
	{
		/* state of individual attribute filters */

		int layer_state;
		int color_state;
		int linestyle_state;
		int entity_state;
		int pen_state;
		int cont_state;
		int marker_state;

		/* attribute filters - bit arrays */
		long f_layer[UD_NLAYER_WORDS];
		long f_color[UD_NCOLOR_WORDS];
		long f_linestyle[UD_NLINESTYLE_WORDS];
		long f_entity[UD_NENTITY_WORDS];
		long f_pen[UD_NPEN_WORDS];
		long f_marker[UD_NMARKERTYPE_WORDS];
	} UD_FILTREC;

	typedef struct
	{ /* raw data on filter states */
		int layer_state;
		int color_state;
		int linestyle_state;
		int entity_state;
		int pen_state;
		int cont_state;
		int marker_state;
		/* raw data on filter values */ 
		int layer_min; 
		int layer_max; 
		int colornum; 
		int linestyle;
		int entnum;
		int pen_min;
		int pen_max;
		int marker_type;
	} UD_RAWFILTREC;

	typedef struct ud_lay_filt_rec
	{ /* layer filter rec for layerfilter.frm */
		int state[8];
		int min[8];
		int max[8];
		UU_LOGICAL cont;
/*	UU_LOGICAL clear; ---- allow the user to clear the form(s) */
		struct ud_lay_filt_rec *link;
	} UD_layer_filter;

/* -- momentary select record -- */

	typedef struct
	{
		int relnum;					/* relation number */
		char selectchar;			/* character to momentary select */
	} UD_SELECTREC;

	typedef struct
	{
		UU_REAL rplane[6][4];
		int side[6];
		int cross[6];
	} UD_SELECTCLIP;

#ifdef DPGM
#define EXT 
#else
#define EXT extern
#endif

	
	EXT int *UD_Select_buf;					/* selected entity buffer */
	EXT int UD_Selbuf_size;					/* size of UD_Select_buf */
	EXT int UD_Select_ptr;					/* select buffer pointer */
	EXT int UD_Select_cnt;					/* number of items selected */
	EXT int UD_Selrej_ptr;					/* reject location pointer */
	EXT int UD_Selrej_cnt;					/* reject select count */
	EXT UD_chain_mod_struc UD_chain_mod;
#ifdef DPGM
	UD_FILTREC UD_filter = {/* select filter definition record */
		UD_INACTIVE,
		UD_INACTIVE, 
		UD_INACTIVE, 
		UD_INACTIVE, 
		UD_INACTIVE,
		0,
		UD_INACTIVE};
		UU_LOGICAL UD_Sellim_mp = UU_TRUE;  /*if TRUE do not allow multiple picks */
		UU_LOGICAL UD_Selbusy = UU_FALSE;  /*if TRUE global select buffer busy */
		UU_LOGICAL UD_Selinit = UU_TRUE;  /*if TRUE first use of select buffer */
#else
	EXT UD_FILTREC UD_filter;				/* select filter definition record */
	EXT UU_LOGICAL UD_Sellim_mp;			/* if TRUE do not allow multiple picks */
	EXT UU_LOGICAL UD_Selbusy;				/* if TRUE global select buffer busy */
	EXT UU_LOGICAL UD_Selinit;				/* if TRUE first use of select buffer */
#endif

/*	-- key definition for momentary select keys -- */

#ifdef DPGM
	char UD_selvector = 'v';					/* vectors are a special case */
	int UD_num_momselect = UD_NUMMSEL;		/* Number of momentary selects */
	UD_SELECTREC UD_momselect[UD_NUMMSEL] = {
			{UM_POINT_REL, 'p'},				/* point */
			{UM_CIRCLE_REL, 'a'},				/* circles and arcs */
			{UM_LINE_REL, 'l'},					/* lines */
			{UM_CONIC_REL,		'n'},			/* conic */
			{UM_COMPCRV_REL,	'c'},			/* composite */
			{UM_AGCRV_REL,		'b'},			/* ag bspline */
			{UM_AGSRF_REL,		's'},			/* ag surface */
			{UM_AGSHELL_REL,	'h'},			/* ag shell */
			{UM_BODY_REL,		'r'},			/* romulus solid */
			{UA_TEXT_REL,		't'},			/* text */
			{UM_FEAT_REL,		'f'}			/* feature */
		};
	UD_SELECTREC *UD_momselect_ptr[] = {UD_momselect};	/* pointer to array */
	int UD_selclip_npl = 0;
	UD_SELECTCLIP UD_selclip;
#else
	EXT char UD_selvector;
	EXT int UD_num_momselect;					/* Number of momentary selects */
	EXT UD_SELECTREC UD_momselect[UD_NUMMSEL];
	EXT UD_SELECTREC *UD_momselect_ptr[];
	EXT UD_SELECTCLIP UD_selclip;
	EXT int UD_selclip_npl;
#endif
#undef EXT

#define DSELECTH
#endif


