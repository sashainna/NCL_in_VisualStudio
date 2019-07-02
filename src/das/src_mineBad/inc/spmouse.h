/*********************************************************************
**    NAME         : spmouse.h
**       CONTAINS:
**    COPYRIGHT 2002 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       spmouse.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:52
*********************************************************************/

#ifndef SPMOUSE_H

#define NCL_ScaleRotation		1024.0

#ifdef SPMOUSE_MAIN
int UV_SM_data[6] = {0,0,0,0,0,0};
int UV_SM_sdata[6] = {0,0,0,0,0,0};
int UV_SM_pan = 1;
int UV_SM_rotate = 1;
int UV_SM_dom = 0;
float UV_SM_sensi = 1.0;
int UV_SM_NCL = 1;
/*
.....default to Keyboard
.....1: mouse, 2 SpaceMouse
*/
int UV_Cur_Dyn = 0;
int UV_actsm_dial = -1;

float UV_SM_pangain = 0.0005;
float UV_MS_pangain = 0.01;
float UV_KB_pangain = 0.01;

float UV_SM_rotgain = 1.0/360;
float UV_MS_rotgain = 3.0;
float UV_KB_rotgain = 3.0;

float UV_SM_zoomgain = 0.0005;
float UV_MS_zoomgain = .030;
float UV_KB_zoomgain = .030;

#else
#ifdef EXT
#undef EXT
#endif
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
EXT int UV_SM_data[6], UV_SM_sdata[6];
EXT int UV_SM_pan;
EXT int UV_SM_rotate;
EXT int UV_SM_dom;
EXT float UV_SM_sensi;
EXT int UV_Cur_Dyn;
EXT int UV_actsm_dial;
EXT int UV_SM_NCL;
EXT float UV_SM_pangain;
EXT float UV_MS_pangain;
EXT float UV_KB_pangain;

EXT float UV_SM_rotgain;
EXT float UV_MS_rotgain;
EXT float UV_KB_rotgain;

EXT float UV_SM_zoomgain;
EXT float UV_MS_zoomgain;
EXT float UV_KB_zoomgain;
#endif

#endif
