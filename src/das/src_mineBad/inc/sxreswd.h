/*********************************************************************
**    NAME         : sxreswd.h 
**       Extra reserved word definitions
**			(to get around YACC max. terminals limitation)
**		CONTAINS:
**			defines
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sxreswd.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:53
*********************************************************************/

#ifndef SXRESWDH


/* Unary operators (all seen by YACC as XUNARY) */
#define XNOT			130
#define XDISP			131
#define XNODISP		132
#define XSCALE			133
#define XMAG			134
#define XUNIT			135

/* Ending words (all seen by YACC as XEND) */
#define XENDPROC		140
#define XENDIF			141
#define XENDDO			142
#define XENDFOR		143
#define XENDSEL		144
#define XENDREC		145
#define XENDVAR		146
#define XENDPACKED	147
#define XENDCLASS		148
#define XENDENUM		149

/* Simple data types (all seen by YACC as XSIMPLE) */
#define XINTEGER		160
#define XLOGICAL		161
#define XREAL			162
#define XCOORD			163
#define XBIT			164
#define XCHARACTER	165
#define XOBJECT_KEY	166
#define XJOIN			167


#define SXRESWDH
#endif
