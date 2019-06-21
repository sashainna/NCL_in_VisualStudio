/************************************************************************
c
c   FILE NAME: Toollibcc.h
c
c	 CONTAINS: 
c		Define C or Fortran functions used by C++ function
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       toolibcc.h , 25.3
c    DATE AND TIME OF LAST  MODIFICATION
c       12/01/15 , 10:25:04
c
c**********************************************************************
*/
extern "C" int inittool(int*, int*);
extern "C" int tool_mf_filename(CWnd* parent, char *title, char *ext, LPCTSTR filter, char* fnam, int *nc);
extern "C" int tool_mfopt_filename(CWnd* parent, char *title, LPCTSTR filter, char *dlocal, 
			char *dsys, char* fnam, int *nc, int flag);
extern "C" int toolf_savlib(int*, char *, char*, int*);
extern "C" void toolf_lodlib(int *i2, char filename[256], char msg[200], int*kerr);
extern "C" void wnt_modlis(char msg[80], int*);
extern "C" void getf_list_rcd(int* type, char lst_rcd[200][80], int*);
extern "C" void mf_crelib(char*, char*, char*);
extern "C" void toolf_savrec(char prmstr[100][80], int *imod, int *add, int *err);
extern "C" void toolf_recchg(char prmstr[100][80], int *imod, int *chg);
extern "C" void tool_exit(int*);
extern "C" void toolf_delrec(int*, int*);
extern "C" void getf_stat_str(char stat_str[6][200], int stat_num[6], int *err);
extern "C" void toolf_addbat(char* filename, int *err);
extern "C" void toolf_createbat(char* filename);
extern "C" void toolf_list(char*filename, int* flag);
extern "C" void toolf_findrec(int*pos, char*msg, int*nc, int*err);
extern "C" void toolf_getdata(int* pos, char prmstr[100][80]);
extern "C" int tool_get_fname(char* fullname,char *fname);

extern "C" void toolf_savpcrec(int *type, char prmstr[512], int *len, int *add, int*cont, int *iniflg, int *err);
extern "C" void toolf_getpcrec(int *type, char prmsstr[512], int *len, int*cont, int *iniflg, int *err);
extern "C" int tool_get_dir(char *dir,char *fullname); 
extern "C" void toolf_initlib();
extern "C" char *tool_getenv(char *);
extern "C" void tool_break_fname(char *fullname,char *dir,char *fname,char *ext);
extern "C" void toolf_findtool(char *find_str, int *snc, int *pos, char *cmsg, int *nc, int *kerr);
extern "C" char *tool_short_filename(char *, char *, int);
