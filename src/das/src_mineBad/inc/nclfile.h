/*********************************************************************
**    NAME         :  nclfile.h
**     MODULE NAME AND RELEASE LEVEL 
**       nclfile.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:35
*********************************************************************/

#ifndef NCLF
#ifdef EXT
#undef EXT
#endif
#ifdef NCLFILE
#define EXT
#else
#define EXT extern
#endif
#define MAXCLF 3
/*
.....Clfile header record structure
*/
#ifndef CLSTRUC
typedef struct
{
	int isn[2];
	int clrec;
	int type;
	int subt;
	int mxcl;
} UN_clstruc;
#define CLSTRUC
#endif
/*
.....Clfile global variables
*/
	EXT UN_clstruc *UN_clpt[MAXCLF],*UN_clfirst[MAXCLF];
	EXT int UN_clnum[MAXCLF],UN_clrec;
/*
.....SEQUNC labels
*/
#define MAXCLSEQ 100
	EXT int UN_clseq[MAXCLF];
	EXT UN_clstruc *UN_clseq_rec[MAXCLF][MAXCLSEQ];
	EXT UN_clstruc *UN_clseq_end[MAXCLF][MAXCLSEQ];
	EXT int UN_clseq_cur[MAXCLF];
	EXT char UN_clseq_label[MAXCLF][MAXCLSEQ][22];
#undef EXT
#define NCLF
#endif
