
/******************************************************************
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       xfsys2.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:23
******************************************************************/
#ifndef UX_XFSYS2
#ifdef UX_F2PGM
#define EXT 
#else
#define EXT extern
#endif

EXT int UX_windrows;
EXT int UX_windwidth;

#undef EXT

/* The following data structure is the file descriptor used to list files in
 * "uxu_list_archive" */
typedef struct {
			char *filearea;
			char *libname;
			int farea_flag;
			char *filetype;
			} UX_descriptor;

/* The following typedef is used for listing file types in "SUPPORT" 
 * functions */
typedef struct {
	int *part;
	char *partpath;
	int *rp;
	char *rppath;
	int *symarea;
	char *symareapath;
	int *symlib;
	char *symlibpath;
	int *drawing;
	char *drawingpath;
	int *psetup;
	char *psetuppath;
	int *pentbl;
	char *pentblpath;
	} UX_file_list_rec;

#define UX_XFSYS2
#endif
