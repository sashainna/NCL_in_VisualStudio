
/*********************************************************************
**    NAME         :  mromcom.h
**       CONTAINS: definitions of romulus data
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       mromcom.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:32
*********************************************************************/
#ifndef UM_MROMCOM

#include "usysdef.h"

#ifdef  UM_MPGM
#define EXT
#else
#define EXT extern
#endif

/****************************************************************************
**                UNICAD record definitions of ROMULUS entities                 **
****************************************************************************/

struct  UM_rom_line
	{
	float pt[3];								/* point on line */
	float vec[3];								/* unit vector along line 
														in forward direction */
	};

struct  UM_rom_circle
	{
	float center[3];							/* center of circle */
	float normal[3];							/* unit normal vector */
	float radius;								/* radius */
	};

struct  UM_rom_ellipse
	{
	float center[3];							/* center of ellipse */
	float normal[3];							/* unit normal vector */
	float major_len;							/* length of major axis */
	float major_axis[3];						/* unit major axis vector */
	float minor_len;							/* length of minor axis */
	};

struct  UM_rom_limb
	{
	int surf;									/* surface of which this is silhouette */
	float eye_dir[3];							/* unit vector from origin to eye */
	float eye_dist;							/* distance from origin to eye */
	float help_pt[3];							/* help point */
	float term_pt[4][3];						/* terminator points */
	};

struct  UM_rom_int
	{
	int surf1;									/* first surface identifier for curve */
	int surf2;									/* second surface identifier for curve */
	float help_pt[3];							/* help point */
	float term_pt[4][3];						/* terminator points */
	};

struct  UM_rom_pln
	{
	float point[3];								/* point */
	float normal[3];								/* normal */
	};

struct  UM_rom_cyl
	{
	float point[3];								/* point on axis */
	float axis[3];									/* unit vector along axis */
	float radius;									/* radius */
	};

struct  UM_rom_con
	{
	float point[3];								/* point on axis */
	float axis[3];									/* unit vector along axis */
	float radius;									/* radius at point */
	float sinang;									/* sine of half angle of divergence */
	float cosang;									/* cosine of half angle of divergence */
	};

struct  UM_rom_sph
	{
	float center[3];								/* center */
	float radius;									/* radius */
	};

struct  UM_rom_tor
	{
	float center[3];								/* center */
	float normal[3];								/* normal */
	float major_radius;							/* major radius */
	float minor_radius;							/* minor radius */
	};

struct  UM_rom_entity
	{
	union {
		struct  UM_rom_line rline;				/* line */
		struct  UM_rom_circle rcircle;		/* circle */
		struct  UM_rom_ellipse rellipse;		/* ellipse */
		struct  UM_rom_limb rlimb;				/* limb curve */
		struct  UM_rom_int rintcrv;			/* intersection curve */
		struct  UM_rom_pln rpln;				/* plane */
		struct  UM_rom_cyl rcyl;				/* cylinder */
		struct  UM_rom_con rcon;				/* cone */
		struct  UM_rom_sph rsph;				/* sphere */
		struct  UM_rom_tor rtor;				/* tours */
		} g;
	};

#define  UM_MAX_CMD 100
EXT int		UM_NUM_OF_SOLIDS;

EXT struct{
	int interactive;								/* input mode for ROMULUS
																UU_TRUE  => interactive mode
																UU_FALSE => read from buffer */
	int cur_cmd;									/* last command read from buffer
																-1 => initial value */
	int num_cmd;									/* number of commands in buffer */
	int max_cmd;									/* maximum number of commands */
	char cmd[ UM_MAX_CMD][120];				/* command strings for ROMULUS */
	}  UM_rombuf;

#define UM_MAX_HIDDEN_XFORMS 6

EXT struct{
	int key[UM_MAX_HIDDEN_XFORMS];				/* key of associated view */
	int xform[UM_MAX_HIDDEN_XFORMS];				/* DIGS transform number  */
	int seg[UM_MAX_HIDDEN_XFORMS];				/* DIGS segement number containing 
															the hidden lines */
	}UM_hidden;
 
#undef EXT

#define UM_MROMCOM
#endif
