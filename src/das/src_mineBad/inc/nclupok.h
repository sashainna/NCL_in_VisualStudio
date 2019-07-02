/*********************************************************************
**    NAME         :  nclupok.h
**       CONTAINS: Pocket Modal definitions
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclupok.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       02/01/16 , 10:18:36
*********************************************************************/
#include "udforms.h"
#include "nclfc.h"
#include "dselmask.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "uhep.h"

#define STRL 64

typedef struct
{
	int entrtype;
	int tanto;
	int nrampstype;
	int clfile;
	int warn;
	int nlevstype;
	int clpltype;
	int endretract;
	int levretract;
	int secretract;
	int perexit;
	int islexit;
	int lacefl;
	int scrdir;
	int pocdir;
	int lcdir;
	int lcfin;
	int spiral;
	int arctrans;
	int autocorner;
	int arcrad;
	int appslowang;
	int cutcom;
	int cutcomdir;
	int cutcompln;
	int genfrtyp;
	int posfrtyp;
	int retfrtyp;
	int entfrtyp;
	int trnfrtyp;
	int finfrtyp;
	int frsfrtyp;
	char gen_fr[STRL];
	char pos_fr[STRL];
	char ret_fr[STRL];
	char ent_fr[STRL];
	char trn_fr[STRL];
	char fin_fr[STRL];
	char frs_fr[STRL];
	char arc_rad[STRL];
	char slow_ang[STRL];
	char cclrlv[STRL];
	char cut_com[80];
	char num_ramps[STRL];
	char ramp_dist[STRL];
	char num_levs[STRL];
	char ret_dist[STRL];
	char hret_dis[STRL];
	char vret_dis[STRL];
	char max_step[STRL];
	char scallop[STRL];
	char min_step[STRL];
} NCLU_pokmod;

UD_FSTAT nclu_pokmod();
