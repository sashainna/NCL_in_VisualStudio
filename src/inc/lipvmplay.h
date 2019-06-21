/*********************************************************************
**    NAME         :  lipvmplay.h
**
**       CONTAINS:
**				NCLIPV Playback definitions
**
**    COPYRIGHT 2005 (c) NCCS, Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       lipvmplay.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:27
**
*********************************************************************/

#ifndef LIPVMPLAY
#define LIPVMPLAY

#undef EXT
#ifdef LPGM
#define EXT
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

#define N_IPVMON_FLD 22
#define IPVMON_ISN 0
#define IPVMON_MODE 1
#define IPVMON_MACHIN 2
#define IPVMON_TEND 3
#define IPVMON_TLAXIS 4
#define IPVMON_LINAXS 5
#define IPVMON_ROTAXS 6
#define IPVMON_HEAD2 7
#define IPVMON_HEAD3 8
#define IPVMON_HEAD4 9
#define IPVMON_LOADTL 10
#define IPVMON_DIA 11
#define IPVMON_RAD 12
#define IPVMON_HGT 13
#define IPVMON_CUTCOM 14
#define IPVMON_FEDRAT 15
#define IPVMON_MOVTIM 16
#define IPVMON_SPINDL 17
#define IPVMON_COOLNT 18
#define IPVMON_MCHTIM 19
#define IPVMON_PROGRESS 20
#define IPVMON_DOCK 21

EXT int LW_monitor_field[N_IPVMON_FLD];
EXT int LW_play_modal[20],LW_progress_count,LW_progress_total;
EXT UU_LOGICAL LW_monitor;

#endif
