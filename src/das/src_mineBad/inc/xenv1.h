/************************************************************
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       xenv1.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:23
************************************************************/
#ifndef XENV1H
#ifdef UX_E1PGM
#define EXT
#else
#define EXT extern
#endif

#define UX_SUFFIX_LEN 260 /* length of path suffix buffers */
#define UX_MAX_PATH_LEN 1024
#define UX_MAX_FILE_LEN 520
#define UX_FAILURE -1
#ifndef UX_EOF
#define UX_EOF -23
#endif
#define UX_NOPSYS -1 /* temporarily same as a failure for testing only */
/*
.....changed for WNT
.....Yurong 1/13/00
*/
/* #ifndef WNT */
#if UU_COMP != UU_WIN2K
#define UX_SEP_CHAR '\\'			/* symbol table alternative values seperator */
#define UX_PATH_SEP '/'				/* the Unicad seperator */
#else
#define UX_PATH_SEP '\\'
#define UX_SEP_CHAR '/'	
#endif

#define UX_QUOTE_CHAR '"'
#define UX_QUOTE_STR "\""
#define UX_EOLN '\0'

	/* The following definitions are used as values for the
	 * parameters */
#define UX_PRTERRS   1 
#define UX_NPRTERRS  2 
#define UX_CHK       4 
#define UX_NCHK      8 
#define UX_QUOTES    16
#define UX_NQUOTES   32

#define UX_TO_UPPER(name)									\
		{ char *ptr; ptr = name; while (*ptr != '\0'){*ptr=toupper(*ptr);ptr++;}}

/* The following definition is the data structure to hold
 * path names */
typedef char UX_pathname[UX_MAX_PATH_LEN];

typedef char UX_ftype[UX_SUFFIX_LEN];

typedef struct
	{
	UX_pathname sname;
	UX_pathname symstr;
	} UX_TABLEREC;

	/* The following flags are used to indicate what kinds of
	 * expansions to do when attempting to get system dependent
	 * path names from UNICAD path names */
#define UX_FIRSTONLY "FIRST"
			/* get only the first system dependent path name only 
			 * that corresponds to a UNICAD path name. */
#define UX_ALL "ALL" /* get all system dependent path names
			 * that correspond to a UNICAD path name. */

	/* The following are all the return codes for uni_env except
	 * UU_SUCCESS and UX_FAILURE. */

#define UX_NOFAREA    16 /* these are bitwise ORed */
#define UX_NOFILE      8
#define UX_NOSUFFIX    4 
#define UX_BAD_SUBJECT 2

#define UX_MODIFIED    -3 /* these are returned individually */
#define UX_CANTFIX     -5
#define UX_BAD_TARGET  -6
#define UX_BAD_OPSYS   -7
#define UX_NFOUND      -9  
#define UX_BAD_ENV    -10 
#define UX_FIXED_EXT		-11
#define UX_BAD_TYPE		-12

#if UU_COMP == UU_VAXVMS
#define index strchr
#define rindex strrchr
#endif 
	/* The following definitions print error messages, if the error
	 * subsystem is desired. */
#ifndef UU_NOERROR

#define UX_ERROR0(nbr, printit)						\
	if (printit)											\
		uu_uerror0(UX_UDOS, nbr)                     

#define UX_ERROR1(nbr, arg1, printit)				\
	if (printit)											\
		uu_uerror1(UX_UDOS, nbr, arg1)

#define UX_ERROR2(nbr, arg1, arg2, printit)		\
	if (printit)											\
		uu_uerror2(UX_UDOS, nbr, arg1, arg2)

#define UX_ERROR3(nbr, arg1, arg2, arg3, printit)	\
	if (printit)												\
		uu_uerror3(UX_UDOS, nbr, arg1, arg2, arg3)

#else
	
#define UX_ERROR0(nbr, printit)

#define UX_ERROR1(nbr, arg1, printit)

#define UX_ERROR2(nbr, arg1, arg2, printit)

#define UX_ERROR3(nbr, arg1, arg2, arg3, printit)

#endif

char *ux_getenv();
#ifdef __cplusplus 
	extern "C" int ux_chk_path_syntax (UX_pathname, int);
#endif

	/* The following macro just makes life easier, */
#define UX_CHK_PATHERRS(pathname, printit, options)  \
	/* make default to print errors */                                \
	printit = ((options | UX_NPRTERRS) != options);           \
	/* make default not to check pathname syntax */                   \
	if (options != (options | UX_NCHK))                        \
		if (ux_chk_path_syntax(pathname, options) != UU_SUCCESS)  \
		{                                                              \
			status = UX_BAD_SUBJECT;                                    \
			goto done;                                                  \
		}

#undef EXT
#define XENV1H
#endif
