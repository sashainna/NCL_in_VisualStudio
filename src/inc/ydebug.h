/*********************************************************************
**    NAME         :  ydebug.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ydebug.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:24
*********************************************************************/
#ifndef NCLXDEBDEF

static char *godir[] =
{
	"NCLX_GOFWD", "NCLX_GOLFT", "NCLX_GORGT", "NCLX_GOBCK", 
	"NCLX_GOUP", "NCLX_GODWN"
};

static char *csatt[] =
{
	"NCLX_TO", "NCLX_PAST", "NCLX_ON", "NCLX_TANTO", "NCLX_PSTAN",
	"NCLX_AUTO"
};

static char *dsatt[] =
{
	"NCLX_TLRGT", "NCLX_TLLFT", "NCLX_TLON"
};

static char *toolmod[] =
{
	"TLAXIS_SAME",
	"TLAXIS_FIXED",
	"TLAXIS_NORMAL",
	"TLAXIS_ATANGL",
	"TLAXIS_TANTO",
	"TLAXIS_FAN",
	"TLAXIS_COMBIN",
	"TLAXIS_POINT",
	"TLAXIS_CURVE"
	"TLAXIS_UNKNOWN"
};

static char *mdltype[] =
{
	"NCLX_MDL_UNDEF",
	"NCLX_MDL_REAL",
	"NCLX_MDL_CIRCLE",
	"NCLX_MDL_CONIC",
	"NCLX_MDL_COMPOSITE",
	"NCLX_MDL_CURVE",
	"NCLX_MDL_DATA",
	"NCLX_MDL_LINE",
	"NCLX_MDL_MATRIX",
	"NCLX_MDL_PATERN",
	"NCLX_MDL_PLANE",
	"NCLX_MDL_POINT",
	"NCLX_MDL_PNTVEC",
	"NCLX_MDL_BSPLINE",
	"NCLX_MDL_NSURF",
	"NCLX_MDL_SCALAR",
	"NCLX_MDL_SURF",
	"NCLX_MDL_TRIMSF",
	"NCLX_MDL_VECTOR"
};

static char *shptyp[] =
{ 
	"NCLX_S_ENDPT", "NCLX_S_ARC", "NCLX_S_DIRECTION", "NCLX_S_VOCAB",
	"NCLX_S_VALUE"
};

static char *pptype[] =
{ 
	"NCLX_P_VOCAB", "NCLX_P_VALUE"
};

static char *tlcond[] =
{ 
	"NCLX_P_IN", "NCLX_P_OUT", "NCLX_P_ON"
};

static char *tldir[] =
{ 
	"NCLX_P_LEFT", "NCLX_P_RIGHT"
};

static char *arcdir[] =
{ 
	"NCLX_P_CLW", "NCLX_P_CCLW"
};

static char *rettyp[] =
{ 
	"NCLX_P_XAXIS", "NCLX_P_YAXIS", "NCLX_P_OFF", "NCLX_P_POINT", "NCLX_P_START"
};


static char *trimtype[] =
{ 
	"NCLX_BASE", "NCLX_FACE" 
};

static char *vocab[] =
{  
	"NCLX_ON", "NCLX_OFF", "NCLX_AUTO", "NCLX_TRUE",  "NCLX_FALSE" ,
	"NCLX_SCRUB", "NCLX_LACE", "NCLX_FIXED", "NCLX_SCALLOP", "NCLX_CLR_PLANE",
	"NCLX_DISTANCE"
};

static char *gtsubtp[] =
{  
	"NCLX_UNDEF", "NCLX_POSITION", "NCLX_DEPARTURE", "NCLX_FROM", "NCLX_APPROACH",  
	"NCLX_CUT", "NCLX_CONTINUATION", "NCLX_RETRACT",  "NCLX_STEPOVER",  "NCLX_ENTRY" 
};

static char *mrtnpl[] =
{"NCLX_CLR_PLANE", "NCLX_CLR_DISTANCE", "NCLX_CLR_INCR"
};

static char *apentry[] =
{ "NCLX_RAMP", "NCLX_PLUNGE", "NCLX_HELIX", "NCLX_OMIT"
};

static char *awarn[] =
{ "NCLX_WARN", "NCLX_NOWARN", "NCLX_AVOID"
};

static char *apdir[] =
{ "NCLX_CLW", "NCLX_CCLW"
};

static char *apcorner[] =
{ "NCLX_ARC", "NCLX_SHARP"
};

static char *aptlcond[] =
{ "NCLX_IN", "NCLX_OUT", "NCLX_ON"
};

static char *sftyp[] = 
{ "ps", "ds", "cs" };

static char *cuttprm[] = 
{
	"diameter", "radius", "height", "side_angle", 
	"zheight", "flat_angle", "prmtr(7)", "prmtr(8)", "prmtr(9)" 
};

#define NCLXDEBDEF
#endif
