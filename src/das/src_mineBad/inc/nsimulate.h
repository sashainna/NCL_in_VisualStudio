/*********************************************************************
**    NAME         :  nsimulate.h
**     MODULE NAME AND RELEASE LEVEL
**       nsimulate.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:40
*********************************************************************/
#ifndef NSIMULATE
#define NSIMULATE
#include "ulist.h"
#include "xenv1.h"
/*
.....Simulation HEADER structure
*/
typedef struct
{
	char post[20];
	char prev[12];
	int mach;
	char date[12];
	char time[10];
	UX_pathname clfile;
	char cldate[12];
	char cltime[10];
	int units;
} UN_sim_header;
/*
.....Simulation MACHINE structure
*/
typedef struct
{
	int type;
	char linaxs[6][12];
	int numlin[3];
	int lincalc[3];
	UU_REAL lindis[3];
	int numrot;
	char rotaxs[4][12];
	int rottype[4];
	UU_REAL rotvec[4][3];
	UU_REAL rotorg[4][3];
} UN_sim_machine;
/*
.....Simulation MOTION structure
*/
typedef struct
{
	int isn;
	UU_REAL seq;
	int skip;
	int type;
	int head;
	UU_REAL tend[6];
	UU_REAL vfwd[3];
	UU_REAL axis[13];
	UU_REAL feed;
	UU_REAL time;
} UN_sim_motion;
/*
.....Simulation STATUS structure
*/
typedef struct
{
	int isn;
	UU_REAL seq;
	int stop;
	int ncut;
	UU_REAL cutter[7];
	int ncutd;
	UU_REAL cutterd[7];
	UU_REAL rpm;
	int coolant;
	int cutcom;
	UU_REAL tlno;
	UU_REAL tlength;
	int cutfl;
	char cutsym[24];
	UU_REAL symofs[4];
	int nshank;
	UU_REAL shank[5];
   int mode;
	int offsets[4];
} UN_sim_status;
/*
.....Simulation CIRCLE structure
*/
typedef struct
{
	int isn;
	UU_REAL seq;
	UU_REAL parms[7];
} UN_sim_circle;
/*
.....Simulation CUTTER structure
*/
typedef struct
{
	int isn;
	UU_REAL seq;
	int ncut;
	UU_REAL cutter[7];
	int ncutd;
	UU_REAL cutterd[7];
	int cutfl[3];
	char cutsym[3][80];
	UU_REAL symofs[3][4];
   int shkfl;
} UN_sim_cutter;
/*
.....Simulation PROFIL structure
*/
typedef struct
{
	int isn;
	UU_REAL seq;
	char symbol[80];
	int subt;
	int npts;
	int type;
	int mxcl;
	UU_REAL *pts;
} UN_sim_profil;
/*
.....Simulation TEXT structure
*/
typedef struct
{
	int isn;
	UU_REAL seq;
	char str[80];
} UN_sim_text;
/*
.....Simulation STOCK structures
*/
typedef struct
{
	int id;
	int subt;
	UU_REAL pt1[3];
	UU_REAL pt2[3];
} UN_sim_box;
typedef struct
{
	int id;
	int subt;
	UU_REAL params[9];
} UN_sim_params;
typedef struct
{
	int id;
	int subt;
	UU_REAL cir[7];
	UU_REAL hgt;
} UN_sim_cyl;
typedef struct
{
	int id;
	UX_pathname fnam;
} UN_sim_load;
typedef struct
{
	int id;
   int units;
	UX_pathname fnam;
} UN_sim_stl;
typedef struct
{
	int id;
   int stock;
   int ncopy;
} UN_sim_clone;
typedef struct
{
	UU_REAL mx[12];
	char mnam[6];
	int msub;
} UN_sim_move;
typedef struct
{
	int col;
	int vis;
	UU_REAL tol;
	int trans;
	int act;
} UN_sim_modify;
typedef struct
{
	int type;
	union
	{
		UN_sim_box    box;
		UN_sim_cyl    cyl;
		UN_sim_params pstk;
		UN_sim_load   load;
		UN_sim_stl    stl;
		UN_sim_clone  clone;
		UN_sim_move   move;
		UN_sim_modify modify;
	}
	data;
	UU_LIST idlist;
	UU_LIST ptlist;
} UN_sim_stock;
/*
.....Simulation record types
*/
#define SIM_HEADER 1
#define SIM_MACHINE 2
#define SIM_PARTNO 3
#define SIM_ERROR 4
#define SIM_PPRINT 5
#define SIM_STATUS 6
#define SIM_MOTION 7
#define SIM_FINI 8
#define SIM_STOCK 9
#define SIM_FIXTUR 10
#define SIM_TOOLPN 11
#define SIM_ATTRIB 12
#define SIM_CUTTER 13
#define SIM_PROFIL 14
#define SIM_CIRCLE 15
#define SIM_ISN 16
#define SIM_HEDMOT 17
/*
.....Simulation STOCK/FIXTURE subtypes
*/
#define SIM_STOCK_BOX 340
#define SIM_STOCK_CONE 632
#define SIM_STOCK_CYL 620
#define SIM_STOCK_CLONE 576
#define SIM_STOCK_MOVE 577
#define SIM_STOCK_REMOVE 843
#define SIM_STOCK_CHIPS 331
#define SIM_STOCK_MODIFY 732
#define SIM_STOCK_LOAD 1075
#define SIM_STOCK_SPHERE 631
#define SIM_STOCK_STL 330
#define SIM_STOCK_TORUS 627
#endif
