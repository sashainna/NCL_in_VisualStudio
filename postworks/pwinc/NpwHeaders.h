/* 
 * Description:	Functions and struct declarations for 
 *				PostWorks C wrappers and structs called
 *				from C++ framework. 
 * 
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        NpwHeaders.h , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        12/09/13 , 11:11:35
c
 */
#ifndef PWHEADER_H
#define PWHEADER_H

#define MAXFMT 92

typedef enum
{
	NpwEPushB=1, 
	NpwEChoiceB, 
	NpwELabelB, 
	NpwETextB, 
	NpwEDynChoiceB
}NpwEButType;

typedef enum
{
	NpwEButton=1, 
	NpwEPrompt, 
	NpwEForm 
}NpwEWinType;

typedef struct npwDynWinStruct{
	
	NpwEWinType winType; /* 1 = Buttons, 2 = Prompt, 3 = Form */
	char winTitle[80];	/* Window Title Bar */
	char winId[10];	/* Window ID (1.2, etc.) */
	int numButtons; /* Number of buttons in menu */
	char Buttonname[60][80]; /* Names of buttons */
	int ButtonId[60];	/* Button ID (1,2,3,etc.) */
	int Buttype[60]; /* 1 = Push button, 2 = Choice, 3 = Label, 4 = Text input */
	char Butdefault[60][80]; /* Default strings for prompts */
	int Butnumchoice[60];	/* Number of selections in choice button */
	int Butdefchoice[60];	/* Default selection for choice button */
/*
.....buffer too small
.....Yurong 
*/
/*	char Butchoicestring[60][10][10];	 Choice button selection strings */
	char Butchoicestring[60][20][20];	/* Choice button selection strings */
}	NpwDynWinStruct;

typedef struct npwDynFormStruct
{
	int numRecords;	/* Number of records in form */
	char recordString[MAXFMT][132];	/* Text of each record */
	int numFields;	/* Number of fields per record */
	int totalFields;	/* Total number of fields in record */
	int fieldStart[20];	/* Starting location of each field */
	int fieldEnd[20];	/* Ending location of each field */
	int fieldType[20];	/* Type of field (same as Buttype) */
	char fieldLabel[20][20];	/* Label of each field */
	int numChoiceFields;	/* Number of choice fields */
	int numChoice[20];	/* Number of choices in choice fields */
	int defaultChoice[MAXFMT][20];	/* Default answer for choices */
/*
.....buffer too small
.....Yurong 4/10/02
*/
/*	char choiceString[20][10][10];	 Choice string */
	char choiceString[60][20][20];	/* Choice string */
} NpwDynFormStruct;

typedef struct npwReturnStruct{
	char winId[10];
	int numButtons; /* Number of buttons in menu */
	int ButtonId[MAXFMT];
	char Butanswer[MAXFMT][132];
}	NpwReturnStruct;

/*
.....On HP, when compiling mainc.c, the function declare can not
.....include parameters, hit errors, for other machine, it is OK 
.....to declare this way.
.....changed by Yurong  2/26/98
*/
#ifdef __cplusplus
extern "C"
{
	extern int NpwInitApp(char *, char *, int *, char **,char *);
	extern int NpwNTInitApp(char *, char *, char *, char *);
	extern int NpwCloseApp();
	extern int NpwGetWinLayout(int *level,int *stage,
		NpwDynWinStruct *wintype,NpwDynFormStruct *frmtyp, char *msg);
	extern int NpwPutAnswer(NpwReturnStruct *ans, int *,char *msg);
#ifdef WNT
	extern int NpwPutForm(NpwReturnStruct *ans,char **sbuf,int *,char
		*msg);
#else
	extern int NpwPutForm(NpwReturnStruct *ans,char sbuf[MAXFMT][132], int *,char
		*msg);
#endif
	extern int NpwLoadMachine(char *fil, char *msg1, char *msg2);
	extern int NpwSaveMachine(int *kask, char *msg1, char *msg2);
	extern int NpwWasLoaded(char *fname);
	extern int NpwCreateDocument(char *fil, char *msg1, char *msg2);
	extern int NpwGetHelp(char *, char *, int *, int *, char *, int *);
	extern int NpwGetAllHelp(NpwDynWinStruct *winStruct, char *helptext, int flag);
	extern int NpwCreateMachine(int *, char *, char *);
	extern int NpwViewMachine(char *, char *);
	extern int NpwSetDefault(char *);

}
#else
	extern int NpwInitApp();
/*
	HP doesn't like full function prototypes.
	extern int NpwNTInitApp(char *, char *, char *, char *);
*/
	extern int NpwNTInitApp();
	extern int NpwCloseApp();
	extern int NpwGetWinLayout();
	extern int NpwPutAnswer();
	extern int NpwPutForm();
	extern int NpwLoadMachine();
	extern int NpwSaveMachine();
	extern int NpwCreateDocument();
	extern int NpwGetHelp();
	extern int NpwGetAllHelp();
	extern int NpwCreateMachine();
	extern int NpwViewMachine();
#endif

#endif
