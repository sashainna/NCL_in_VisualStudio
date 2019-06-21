/*********************************************************************
**  NAME:  PtdFunc.h  
**  Description:
**				Function declared for Pted called from C++ function.
**    CONTAINS:
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdFunc.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:30
*********************************************************************/
#ifndef PTDFUNCH
#define PTDFUNCH

#define		LINE_MAX_CHARS				256

#ifdef __cplusplus
extern "C"
{
#endif
	extern int Ptd_Appinit();
	extern int Ptd_init_mpost();
	extern int Ptd_edit_input();
	extern int Ptd_edit_output();
	extern int Ptd_saveMDF(char *);
	extern void Ptd_setmdf();
	extern void Ptd_cpmch_out();
	extern void Ptd_cpout_mch();
	extern void Ptd_cpin_mch();
	extern void  Ptd_cpmch_in();
	extern int Ptd_Convert(char*, int, int, char**, int);
	extern int Ptd_Format(char*, char**, int);
	extern int Ptd_Unformat(char*, char**, int);
	extern int Ptd_Unpack(char*, char**);
	extern int Ptd_Reverse(char*, int, char**);
	extern int Ptd_BadCTL(char*, char**);
	extern int Ptd_BadAPT(char**,int, int, int*);
	extern int Ptd_BadCL(char**,int, int, int*);
	extern int  Ptd_Edit_input();
	extern int  Ptd_Edit_output();
	extern int  Ptd_LoadInput(char*, char*, char*);
	extern int  Ptd_LoadOutput(char*, char*, char*);
	extern int  Ptd_LoadMDF(char*, char*, char*);
	extern int  Ptd_GetFindStr(char*, char**, int, int*);
//	extern int  Ptd_Resequence(char*,int, int, int, int, char**, int *);
	extern int  Ptd_Resequence(int *, int, int,int, int, int, int, char**, int *);
	extern int  Ptd_strcat(char**, char*);
	extern int  Ptd_GetLine(char*, int, char*);
	extern int  Ptd_setreg(char*);
	extern void  Ptd_cpmch_backup();
	extern void  Ptd_cpback_mch();
	extern void  Ptd_NCtoAPT(char*, char**, int *);
	extern void  Ptd_nctosimfile(char*, char*, char**, int*);
	extern int  Ptdc_BadCL(char*, char**);
	extern int  Ptdc_BadCTL(char*, char**);
	extern int  Ptdc_BadAPT(char*, char**);
	extern int  Ptd_initreglist();
	extern int  Ptd_freereglist();
	extern int  Ptd_set_tllen(int tlno[], double tllen[], int tnum, int ton);
	extern int  Ptd_set_units(int kun);
	extern int  Ptd_Add(char *cin, char **adds, int num, char**cout, char *msg);
	extern int  Ptd_Mirror(char *cin, char **adds, int num, char**cout, char *msg);
	extern int  Ptd_Multiply(char *cin, char **adds, int num, char**cout, char *msg);
	extern int	Ptd_Rotate(char *cin, char **adds, int num, char**cout, char *msg);
	extern int	Ptd_Scale(char *cin, char **adds, int num, char**cout, char *msg);
	extern int	Ptd_Trans(char *cin, char **adds, int num, char**cout, char *msg);
	extern int	Ptd_GetAdds(char*in, char**out, int bcase, double fval);
	extern int  Ptd_BadCommand(char*inputdata, char**outdata, int fsave);
	extern void Ptd_flist_init(int, int);
	extern void Ptd_flist_push_multiple(int, char*);
	extern void Ptd_flist_push_eol();
	extern void Ptd_flist_finish(char**);
	extern void ptd_ctocd(char*, int*,int*,double*,int*);
	extern void ptd_stoval(int*,double*,int*);
	extern void ptd_init();
	extern void ptd_savmch(char*,int*);
	extern void ptd_edit_mdf(int*,int*);
	extern void outcptm();
	extern void mcptout();
	extern void mcptback();
	extern void backcptm();
	extern void ptd_inpack(int*);
	extern void ptd_inendchar(char*);
	extern void ptd_load_mdf(char*,int*,char*,char*,int*);
	extern void ptd_defmtcod(int*,double*,char*,int*,int*,int*);
	extern void ptd_fmticod(int*,double*,char*,int*);
	extern void ptd_tapblk(char*,char*,int*);
	extern void ptd_gtregv(int*,double*,char*,int*,int*,int*,int*);
	extern void ptd_badcla(int*,int*,int*);
	extern void ptd_badapt(int*,int*,int*);
	extern void ptdf_badctl(char*,int*,int*);
	extern void ptd_cparse(char*,int*,int*,int*);
	extern void ptd_rmopt(char*,int*);
	extern void ptd_getseq(int*);
	extern void ptdf_loddef();
	extern void ptd_aptsrc(char*,int*);
	extern void ptd_simhdr(int*, char*, char*, int*);
	extern void ptd_nctosim(char*, int*, int*);
	extern void Ptd_docutter(char*, int*, char*);
	extern void ptd_toldo(char*, int*, int*);
	extern void ptdf_tolfil(char*, int*, int*, char*);
	extern void ptd_tolinit();
	extern void ptd_initregtab();
	extern void set_tllen(int*,double*,int*,int*);
	extern void Ptd_cllist_init();
	extern void Ptd_cllist_push_multiple(int, char*);
	extern void Ptd_cllist_push_eol();
	extern void Ptd_cllist_finish(char**);
	extern void ptdf_stoval(int*, double*, int*);
	extern void ptdf_ledout(int*, int*, char*, int*);
	extern int  Ptd_Get_NxtBk(char*, char*, int, int*);
#ifdef IBM
	extern int strcasecmp(char*, char*);
	extern int strncasecmp(char*, char*, int);
#endif
#ifdef __cplusplus
}
#endif

#endif
