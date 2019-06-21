/*********************************************************************
**    NAME         :  gtblvar3.h
**       CONTAINS:
**       All GKS external data tables and compile time constants.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtblvar3.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:24
*********************************************************************/
#ifndef GTBLVAR3H

#include "gtblvar2.h"
#include "gtbldesc.h"

#ifdef GMAIN
UG_gksdesctbl ug_gksdesctbl={ 		/* GKS description table */
				1,						/* number of avail workstation types */
				ug_wsptr,				/* pointer to array of ws pointers */
				UG_MAXOPWS,			/* maximum no. open workstations */
				/* following are default primitive attributes */
				1,						/* polyline index */
				{{1,1,.01,"\0"},		/* linetype */
				1.0,1},				/* lw scale, cindex */
				3,1,1.0,1,			/* marker index, type, size factor, cindex */
				1,1,UG_STRING,1.0,0.0,1,	/* text index, bundle */
				.01,0.,1.,0.,UG_TP_RIGHT,	/* char ht, up vec, path */
				UG_TH_LEFT,UG_TV_BASE,	/* text alignment */
				0.,0.,1.,			/* char plane vector */
				1,							/* fill area index */
				UG_HOLLOW,1,1,1.0,1.0,1.0,UG_ON,  /* fill area bundle */
				UG_PHONG,0.0,0.0,0.0,	/* fill area bundle */
				1.,1.,0.,0.,		/* pattern size, reference point */
				0.,1.,0.,0.,1.,0.,	/* pattern plane vector, up vector */
				1,						/* pick id */
				UG_INDIVIDUAL,			/* linetype asf */
				UG_INDIVIDUAL,			/* linewidth scale factor asf*/
				UG_INDIVIDUAL,			/* polyline color asf */
				UG_INDIVIDUAL,			/* marker type asf */
				UG_INDIVIDUAL,			/* marker size scale factor asf */
				UG_INDIVIDUAL,			/* polymarker color asf */
				UG_INDIVIDUAL,			/* text font & precision asf */
				UG_INDIVIDUAL,			/* text char expansion factor asf */
				UG_INDIVIDUAL,			/* text character spacing asf */
				UG_INDIVIDUAL,			/* text color asf */
				UG_INDIVIDUAL,			/* fill area interior style asf */
				UG_INDIVIDUAL,			/* fill area style index asf */
				UG_INDIVIDUAL,			/* fill area color asf */
				1.,0.,0.,0.,		/* default global modeling xform (ident) */
				0.,1.,0.,0.,
				0.,0.,1.,0.,
				0.,0.,0.,1.,
				1.,0.,0.,0.,		/* default local modeling xform (ident) */
				0.,1.,0.,0.,
				0.,0.,1.,0.,
				0.,0.,0.,1.,
				1,						/* default view index */
				1,						/* default no. requestes ws LDC space */
				ug_wsldc};				/* pointer to list of requested LDC space */
#endif

#ifndef GMAIN
extern UG_gksdesctbl ug_gksdesctbl;
#endif

#define GTBLVAR3H
#endif
