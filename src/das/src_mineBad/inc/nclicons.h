/*********************************************************************
**    NAME         :  nclicons.h
**       CONTAINS:
**       definitions for NCL icons arrays
**       and macros for navigating subsystems.
**
**     MODULE NAME AND RELEASE LEVEL 
**       nclicons.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:35
*********************************************************************/
/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                             !!!!!!
!!!!!!   NOTE:                                                     !!!!!!
!!!!!!      Recompile routine UMAINC.C to cause any changes        !!!!!!
!!!!!!      made in this include file to be reflected in the       !!!!!!
!!!!!!      executable.                                            !!!!!!
!!!!!!                                                             !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/
#ifndef NCLICOM

#include "diconm.h"
#include "lcom.h"


#ifdef NCLIPGM
#define EXT
#else
#define EXT extern
#endif
EXT int ICON_UP,	/* added. kathy */
	ALTACTION_UP,	/* NCL - rah 8.0 layout changes, implmnt ALTACTION like DDC */
	SELECT_UP;

#define SUBSYS(x1) 										\
			{ 											\
			extern int x1();  							\
			ud_subsystem(x1, NULL, NULL, UU_FALSE);		\
			}


/* NCL502 ICON ARRAYS */
#ifdef NCL_ICONS_FOR_INTER

  int UMB_ICONS       = 6;      /* NIS icon array */
  int UMBS_ICONS      = 7;      /* NIS SIDE icon array */
  int CAM_ICONS       = 8;      /* NCLCAM icons*/
  int CAMS_ICONS      = 9;      /* CADD/CAM SIDE icon array */
  int CADD_ICONS     = 10;      /* NCLCADD icons   */
  int CADDS_ICONS    = 11;      /* CADD/CAM SIDE icon array */
  int SELECT_ICONS   = 12;      /* SELECT icon */
  int ALTACT_ICONS   = 13;      /* DRAFT ALT ACTION icon */
  int MOTION_ICONS   = 14;      /* NCLCAM motion icon array */
  int MODEL_ICONS    = 15;      /* NCLCAM modeling icon array */

#else

  extern int UMB_ICONS;       /* NIS icon array */ 
  extern int UMBS_ICONS;      /* NIS SIDE icon array */ 
  extern int CAM_ICONS;       /* NCLCAM icons*/ 
  extern int CAMS_ICONS;      /* CADD/CAM SIDE icon array */ 
  extern int CADD_ICONS;      /* NCLCADD icons   */ 
  extern int CADDS_ICONS;     /* CADD/CAM SIDE icon array */ 
  extern int SELECT_ICONS;    /* SELECT icon */ 
  extern int ALTACT_ICONS;    /* DRAFT ALT ACTION icon */ 
  extern int MOTION_ICONS;    /* NCLCAM motion icon array */ 
  extern int MODEL_ICONS;     /* NCLCAM modeling icon array */

#endif


/* NIS MENU ICONS */
#define FILEMGT  7
#define SYSTEM   8
#define BATCH    9
#define POST    10
#define PLOT    11
#define UTILITY 12
#define TAPE    13
#define LOGO    17

#define UZ_DOWNCAM() \
		{if (ICON_UP) { \
			UD_ICMU_DN(ICON_UP); \
			ICON_UP = 0; \
			} \
		if (SELECT_UP) { \
			UD_ICMU_DN(SELECT_UP); \
			SELECT_UP = 0; \
			} \
		if (ALTACTION_UP) { \
			UD_ICMU_DN(ALTACTION_UP); \
			ALTACTION_UP = 0; \
			} \
		UD_ICMU_DN(CAM_ICONS); \
		UD_ICMU_DN(CAMS_ICONS);}
#define UZ_DOWNCADD() \
		{if (SELECT_UP) { \
			UD_ICMU_DN(SELECT_UP); \
			SELECT_UP = 0; \
			} \
		if (ALTACTION_UP) { \
			UD_ICMU_DN(ALTACTION_UP); \
			ALTACTION_UP = 0; \
			} \
		UD_ICMU_DN(CADD_ICONS); \
		UD_ICMU_DN(CADDS_ICONS);} 
#define UZ_DOWNNIS() \
		{UD_ICMU_DN(UMB_ICONS); \
		UD_ICMU_DN(UMBS_ICONS);}

#define UZ_UPCAM() \
		{UD_ICMU_UP(CAM_ICONS); \
		UD_ICMU_UP(CAMS_ICONS);}
#define UZ_UPCADD() \
		{UD_ICMU_UP(CADD_ICONS); \
		UD_ICMU_UP(CADDS_ICONS);} 
#define UZ_UPNIS() \
		{UD_ICMU_UP(UMB_ICONS); \
		UD_ICMU_UP(UMBS_ICONS);} 

#undef EXT
#define NCLICOM

#endif
