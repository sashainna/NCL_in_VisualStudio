/*********************************************************************
**    NAME         :  toolibdata.h
**
**   CONTAINS:
**     Tool Library data base record definitions.
**
**    COPYRIGHT 2007 (c) NCCS Inc.  All Rights Reserved.
**      MODULE NAME AND RELEASE LEVEL
**       toolibdata.h , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       12/01/15 , 10:25:32
*********************************************************************/
#if !defined(TOOLDATA_INCLUDE)
#define TOOLDATA_INCLUDE

#include "usysdef.h"
#include "umoveb.h"

#define NJOINS 3
#define TOOLREC_SIZE32 736
#define HEADREC_SIZE32_102 680
#define HEADREC_SIZE32 928
/*
......used to read/save toolib data struction
*/
struct TOOL_data
{
	char data[10000];
};

struct TL_loadtl_rec
{
	int	parm;
	char	value[22];
};

struct TL_toolhead_rec95
{
	char	head[30];
	char	create_date[12];
	char	create_time[12];
	char	mod_date[12];
	char	mod_time[12];
	char	name[256];
	char	description[42];
	char	symlib[12];
	char	proflib[256];
	int	units;
	int	no_tools;
	int	low_tool;
	int	high_tool;
	UU_REAL	version;
};

struct TL_toolhead_rec102
{
	char	head[30];
	char	create_date[12];
	char	create_time[12];
	char	mod_date[12];
	char	mod_time[12];
	char	name[256];
	char	description[42];
	char	symlib[12];
	char	proflib[256];
	int	units;
	int	no_tools;
	double	low_tool;
	double	high_tool;
	UU_REAL	version;
};


struct TL_toolhead_rec
{
	char	head[30];
	char	create_date[12];
	char	create_time[12];
	char	mod_date[12];
	char	mod_time[12];
	char	name[256];
	char	description[42];
	char	symlib[256];
	char	proflib[256];
	int	units;
	int	no_tools;
	double	low_tool;
	double	high_tool;
	UU_REAL	version;
	int utype_no;
	char **utype;
};

struct TL_tooldata_rec
{
	char	description[42];
	char	symbol[81];
	char	symshk[81];
	char	symhld[81];
	char	drawing[64];
	char	major[8];
	double	toolno;
	int	ncvals;
	int	ndvals;
	int	ctype;
/*
......0: STATIC: using default
......1: ON
......2: OFF
*/
	int	shade;
	int	sshade;
	int	hshade;
/*
......0: STATIC: using default
......1: Partial
......2: All
*/
	int	segments;
/*
......0: STATIC: using default
......1: ON
......2: OFF
*/
	int	move;
	int	cparms[6];
	int	dparms[6];
	int	yparms[4];
	int	sparms[4];
	int	hparms[4];
	UU_LOGICAL	fpseudo;
	UU_LOGICAL	fsymbol;
	UU_LOGICAL	fshank;
	UU_LOGICAL	fholder;
	UU_LOGICAL	floadtl;
	UU_REAL	cutter[6];
	UU_REAL	pseudo[6];
	UU_REAL	catt[4];
	UU_REAL	satt[4];
	UU_REAL	hatt[4];
	int	no_loadtl;
	struct TL_loadtl_rec	*loadtl;
	int	no_plabel;
	char	*plabel;
	int	no_command;
	char	*command;
};
struct TL_tooldata_rec95
{
	char	description[42];
	char	symbol[22];
	char	symshk[22];
	char	symhld[22];
	char	drawing[64];
	char	major[8];
	int	toolno;
	int	ncvals;
	int	ndvals;
	int	ctype;
/*
......0: STATIC: using default
......1: ON
......2: OFF
*/
	int	shade;
	int	sshade;
	int	hshade;
/*
......0: STATIC: using default
......1: Partial
......2: All
*/
	int	segments;
/*
......0: STATIC: using default
......1: ON
......2: OFF
*/
	int	move;
	int	cparms[6];
	int	dparms[6];
	int	yparms[4];
	int	sparms[4];
	int	hparms[4];
	UU_LOGICAL	fpseudo;
	UU_LOGICAL	fsymbol;
	UU_LOGICAL	fshank;
	UU_LOGICAL	fholder;
	UU_LOGICAL	floadtl;
	UU_REAL	cutter[6];
	UU_REAL	pseudo[6];
	UU_REAL	catt[4];
	UU_REAL	satt[4];
	UU_REAL	hatt[4];
	int	no_loadtl;
	struct TL_loadtl_rec	*loadtl;
	int	no_plabel;
	char	*plabel;
	int	no_command;
	char	*command;
};
#endif // !defined(TOOLDATA_INCLUDE)
