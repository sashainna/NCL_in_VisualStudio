/*********************************************************************
**    NAME         : sinput.h 
**       Source input handling def's
**		CONTAINS:
**			us_input_init()
**			stat = us_input_open( path )
**			stat = us_include( path )
**			us_input_add_search( path )
**			us_input_close()
**			line = us_input_line( line, linemax )
**			c = us_input()
**			us_output( c )
**			us_unput( c )
**			eof = yywrap()
**			us_input_lineno
**			us_ident_lineno
**			us_input_path
**			us_modlist
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sinput.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:51
*********************************************************************/

#ifndef SINPUTH

#include "ustdio.h"


extern void		us_input_init();
extern int		us_input_open();
extern int		us_include();
extern void		us_input_add_search();
extern void		us_input_close();
extern char *	us_input_line();
extern char		us_input();
extern void		us_output();
extern void		us_unput();
extern int		yywrap();

/* structure for module/include list */
typedef struct t_modinc
{
	short					modflag;		/* TRUE if module, FALSE if include */
	struct t_modinc	*next;		/* pointer to next node */
	char					*name;		/* pointer to include name */
} n_modinc;

#ifndef USINPUTC			/* if we are not compiling sinput.c */
	extern int			us_input_lineno;
	extern int			us_ident_lineno;
	extern char			us_input_path[];
	extern FILE			*us_stream;
	extern int			us_isatty;
	extern n_modinc	*us_modlist;
#endif

#ifdef input
#undef input
#define input()		us_input()
#endif

#ifdef output
#undef output
#define output(c)		us_output(c)
#endif

#ifdef unput
#undef unput
#define unput(c)		us_unput(c)
#endif

#define SINPUTH
#endif
