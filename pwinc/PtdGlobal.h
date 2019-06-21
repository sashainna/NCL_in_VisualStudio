/*********************************************************************
**  NAME:  PtdGlobal.h
**  Description:
**				Globle functions used for all classes
**    CONTAINS:
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdGlobal.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:30
*********************************************************************/

#ifndef PTDGLOBALH
#define PTDGLOBALH

#include "pwenv.h"

#ifdef __cplusplus
extern "C"
{
#endif

extern void StrtoUpper(char *, char *);
extern int Ptd_Getline_from_address(char *, int *, char*, int*);
extern int Ptd_Getline_from_string(char *, int *, char*, int*);
extern int Ptd_Get_Select_text(char *, int, int, char *);
extern int Ptd_findaddr(char *, char *, int, int, int, int, int *, int*, double *);
extern int Ptd_findtext(char*, char*, int, int, int, int, int*);
extern int Ptd_SameAdds(char*, char*, int);
extern int Ptd_open_flist(char*, char[5][UX_MAX_PATH], char*);
extern int Ptd_save_flist(char*, char[5][UX_MAX_PATH], char*);
extern int Ptd_clload(char *filename, char **outdata);
extern int Ptd_clsave(char *filename, char *savedata, int* err);
extern int Ptd_TextReplace(char **textstr, int spos, int epos, char *rstr);

#ifdef WNT

void Run_Batch(char *filename, int *classpt, int class_type);
void Run_command(char* command, int *classpt, int class_type);;
int Get_Range_from_String (char *rangestr, PtedRangeStruct*  sRange);
int ProgramSaveSelectAs(char *fname, char *TextData, int ftype, PtedRangeStruct sRange, 
						int *classpt, int class_type);
void ProgramLoadSelect(char* FileName, 
			PtedRangeStruct sRange, char *seldata, int *m_ftype, int *classpt, int class_type);
void Substitute(char *infstr, char *inrstr, int vflag, 
 			PtedRangeStruct range, int *classpt, int class_type);
int LoadProgramData(char *filename, int *classpt, int class_type);
void LoadCutterFile(char *filename, int *classpt, int class_type);
int ProgramSaveAs( char *fileName, char *filedata, int ftype, int *classpt, int class_type);

#else
void Run_Batch(char *filename, int *classpt, int class_type);
void Run_command(char* command, int *classpt, int class_type);;
int Get_Range_from_String (char *rangestr, RangeDlgStruct*  sRange);
int ProgramSaveSelectAs(char *fname, char *TextData, int ftype,
                  RangeDlgStruct sRange, int *classpt, int class_type);
void ProgramLoadSelect(char* FileName, RangeDlgStruct sRange, 
			char *seldata, int *m_ftype, int *classpt, int class_type);
void Substitute(char *infstr, char *inrstr, int vflag,
         RangeDlgStruct range, int *classpt, int class_type);
int LoadProgramData(char *filename, int *classpt, int class_type);
void LoadCutterFile(char *filename, int *classpt, int class_type);
int ProgramSaveAs( char *fileName, char *filedata, int ftype, int *classpt,
int class_type);

#endif
#ifndef WNT
#ifndef VAXVMS
#ifndef HP
#ifndef IBM
#define csystem csystem_
#endif
#endif
#endif
#endif
extern void csystem(char *cmdln, int *nc, int *result);
extern void Ptd_rmchar(char *, char);
extern void Ptd_rm_prespc(char *string);

#ifdef __cplusplus
}
#endif

#endif
