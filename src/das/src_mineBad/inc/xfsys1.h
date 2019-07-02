/********************************************************
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       xfsys1.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:23
*********************************************************/
#ifndef XFSYS1H
#include "xenv1.h"

#ifdef EXT
#undef EXT
#endif
#ifdef UX_F1PGM
#define EXT
#else 
#define EXT extern
#endif

#define UX_BAD_DES   -20		/* "status" return codes for routines */
#define UX_NO_ACCESS -21
#define UX_NO_SPACE  -22
#ifndef UX_EOF
#define UX_EOF       -23
#endif
#define UX_BAD_FORMAT -24
#define UX_BAD_WRITE -25
#define UX_BAD_INTERP -26
#define UX_BAD_FILE  -27
#define UX_FOUND     -28
#define UX_BAD_SUBMODE -29
#define UX_BAD_TARMODE -30
#define UX_BAD_MODE  -31
#define UX_DIR       -32
#define UX_EOL       -33
#define UX_NO_ENTRY  -34
#define UX_NFAREA		-35		/* status value returned by ux_is_farea */

	/* The following are either input or output file mode indications */
#define UX_NEXISTS 4096 /* file(area) doesn't exist / can't be found (output) */
#define	UX_EXISTS    0 /* does the designated file already exist */
#define UX_EXECUTE   1 /* can the file be executed */
#define UX_WRITE     2 /* can the file be written to */
#define UX_READ      4 /* can the file be read */
#define UX_APPEND    8 /* can the file be appended to */
#define	UX_DELETE   16  /* can the file be deleted */
#define	UX_OPEN     32  /* is the file open. */
#define UX_CREATE   64 /* can the file designated by "pathname" be created */
#define UX_R       128 /* file open for reading */
#define UX_W       512 /* file open for writing */
#define UX_A      1024 /* file open for appending */
#define UX_FAREA  2048 /* check for file area */
#define UX_LOCKED 8192

#define UX_EWRDEX  UX_EXISTS+UX_READ+UX_APPEND+UX_WRITE+UX_EXECUTE+UX_DELETE
#define UX_EWRDOEX UX_EWRDEX+UX_OPEN
#define UX_EWRDOCEX UX_EWRDOEX+UX_CREATE
#define UX_EWRDOCEXF UX_EWRDOCEX+UX_FAREA

#define UX_EXPAND_CHAR '^'			/* used by ux_mk_chk_syspath routine */

	/* The following data type specifies contents of the UNICAD file header */
typedef struct {
	char head[30]; /* "UNICAD FILE HEADER DATA\n" */
	char nbrbytes[6]; /* number of bytes remaining header */
	char data[30]; /* date file is created */
	char version[20]; /* product and/or version of software which 
							  created the file */
	char precision[6]; /* number of bytes per UU_REAL */
	char hformat[12]; /* currently either BLOCK (blocked sequencial file) or, 
							 STREAM (stream file) */
	char hinterp[12]; /* currently either ASCII (an ascii file), or, TEXT
							 (non-ascii text files), or, BINARY (binary files) */
	char machine[20];/* machine, model, and/or operating system used in
							 creating the file */
	char *extra;   	/*   any extra information in the header */
	} UX_fheader;

#define UX_HDR_LEN 128		/* current total no. of bytes of FIXED portion
								of a Unicad file header, has same value as the total
								of char arrays less the string terminators (136 - 8) */

typedef struct {
	UU_LOGICAL default_firstim;
	char default_area[UX_MAX_PATH_LEN];
	char default_lib[UX_MAX_PATH_LEN];
	char loc_farea[UX_MAX_PATH_LEN];
	char sys_farea[UX_MAX_PATH_LEN];
	char pstring[UX_MAX_PATH_LEN];
	char fenv[UX_MAX_PATH_LEN];
	char farea_suffix[UX_MAX_PATH_LEN];
	char ftype[UX_MAX_PATH_LEN];
	} UX_libdata_bag;

#undef EXT
#define XFSYS1H
#endif
