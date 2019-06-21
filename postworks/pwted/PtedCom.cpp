/********************************************************************
*  NAME:  PtedCom.cpp
**  Description:
**			all common functions for classes
**				
**    CONTAINS:
**			extern "C" void Run_Batch(char *filename, int *classpt, int class_type)
**			extern "C" void Run_command(char* command, int *classpt, int class_type);
**			extern "C" int Get_Range_from_String (char *rangestr, PtedRangeStruct*  sRange)
**			extern "C" Substitute(char *infstr, char *inrstr, int vflag, 
**						PtedRangeStruct range, int *classpt, int class_type)
**			extern "C" int LoadProgramData(char *filename, int *classpt, int class_type)
**			extern "C" void LoadCutterFile(char *filename, int *classpt, int class_type)
**
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c        PtedCom.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:26
**********************************************************************/
#include "pwstdafx.h"
#include "PWBrowser.h"
#include "Pted.h"
#include "PtedRangeBox.h"
#include "PtedBatch.h"
#include "PtdGlobal.h"
#include "PtdFunc.h"
#include "PtedWindow.h"
#include "PtedTextView.h"

#include <conio.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>

extern "C" int Pw_dispmsg(char *msgline, int flag);
extern "C" void Run_command(char* command, int *classpt, int class_type);
extern "C" int Get_Range_from_String (char *rangestr, PtedRangeStruct*  sRange);
extern "C" void Pted_Display_Dlg_percent(int num);

/***********************************************************************
c
c   FUNCTION:  Pted_Disp_Msg(char *msg, int flag, int *classpt, int class_type)
c
c              Run command file. This function could called from PtedWindow class
c				or PtedBatch class
c
c   INPUT:  msg: message string to display
c			flag: 1: error messge
c				  2: warning message
c				  3. info message
c			classpt: pointer of class
c			class_type: 0: batch
c						1: window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" int Pted_Disp_Msg(char *msg, int flag, int *classpt, int class_type)
{
	if ((class_type==1) && (classpt!=NULL))
	{
		((CPtedWindow *)classpt)->Disp_Msg(msg, flag);
	}
	else if ((class_type==2) && (classpt!=NULL))
	{
		((CPtedTextView *)classpt)->Disp_Msg(msg, flag);
	}
	else
	{
		Pw_dispmsg(msg, 0);
	}
	return 0;
}
 
/***********************************************************************
c
c   FUNCTION:  Pted_SetFtype(int ftype, int *classpt, int class_type)
c
c              Set file type
c
c   INPUT:  ftype: file type to be set
c			classpt: pointer of class
c			class_type: 0: batch
c						1: window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" void Pted_SetFtype(int ftype, int *classpt, int class_type)
{
	if (class_type==0)
		((PtedBatch* )classpt)->SetFtype(ftype);
	else
		((CPtedWindow *)classpt)->SetFtype(ftype);
}

/***********************************************************************
c
c   FUNCTION:  Run_Batch(char *filename, int *classpt, int class_type)
c
c              Run command file. This function could called from PtedWindow class
c				or PtedBatch class
c
c   INPUT:  filename: commmand file name
c			classpt: pointer of class
c			class_type: 0: batch
c						1: window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" void Run_Batch(char *filename, int *classpt, int class_type)
{
	FILE *fp;
	char *tok, command[500], msg[UX_MAX_PATH];
	if ((!strlen(filename)) || ((fp = fopen(filename, "r") )== NULL))
	{
		sprintf(msg,"Unable to open command file: %s",filename);
		Pted_Disp_Msg(msg, 1, classpt, class_type);
		return;
	}
	while (fgets(command, 500, fp))
	{
		tok = strtok (command, "\r\n");
		if (tok!=NULL)
			strcpy(command, tok);
		if (class_type==0)
			Pted_Display_Dlg_percent(1);
		Run_command(command, classpt, class_type);
		if (class_type==0)
			if (((PtedBatch *)classpt)->m_exit)
				break;
	}
}
/***********************************************************************
c
c   FUNCTION:  Run_command(char* command, int *classpt, int class_type)
c
c              Run a command.
c
c   INPUT:  command: commmand name
c			classpt: pointer of class
c			class_type: 0: batch
c						1: window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" void Run_command(char* command, int *classpt, int class_type)
{
	char msg[UX_MAX_PATH],msg1[80],msg2[80],FileName[UX_MAX_PATH];
	char *indx, *indx1, *tok;
	char tmpstr1[500], fstr[500], rstr[500];
	char **adds, *cout = NULL;
	PtedRangeStruct sRange;
	char tmpcmd[500];
	int stat, vflag, flag;
	int i, tlno[120], ton, tseq;
	int ftype;
	double tllen[120];

	FileName[0] = '\0';

	if (strlen(command)==0)
	{
		return;
	}
/*
.....set wait cursor
*/
	CWaitCursor wait;
	char hldcmd[500];
	strcpy(hldcmd,command);
/*
.....remove space before command
.....and change all command to upper case
.....but not include the parameters followed after ":"
*/
	Ptd_rm_prespc(command);
/*
.....use another string because we don't want change
.....the original command
*/
	strcpy(tmpcmd, command);
	char *endcmd = strchr(tmpcmd, ':');
	if (endcmd!=NULL)
	{
		*endcmd = '\0';
/*
.....we need check if this is range specific
.....if it is, it will have '\\' before ':'
*/
		endcmd = strchr(tmpcmd, '\\');
		if (endcmd!=NULL)
			*endcmd = '\0';
	}
	else
	{
/*
.....if there is no ':', we need check for '\\'
*/
		endcmd = strchr(tmpcmd, '\\');
		if (endcmd!=NULL)
			*endcmd = '\0';
	}
	StrtoUpper(tmpcmd, tmpcmd);

	if (strncmp(tmpcmd, "MAC", 3)==0 || strncmp(tmpcmd, "MAO", 3)==0 ||
		 strncmp(tmpcmd, "MAI", 3)==0)
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			if (class_type==0)
				Pw_dispmsg("The MAx command must have a filename\r\n",0);
			else
			{
				stat = browsefile("MDF Files (*.MDF)\0*.MDF\0\0", FileName, TRUE,
					0, TRUE);
				if (stat==0)
				{
					goto done;
				}
			}
		}
		else
		{
			strcpy(FileName, indx+1);
		}
		if (strncmp(tmpcmd, "MAC", 3)==0)
			stat = Ptd_LoadMDF(FileName,msg1,msg2);
		else if (strncmp(tmpcmd, "MAO", 3)==0)
			stat = Ptd_LoadOutput(FileName,msg1,msg2);
		else
			stat = Ptd_LoadInput(FileName,msg1,msg2);
		if (stat!=0)
		{
			Pted_Disp_Msg(msg1, 1, classpt, class_type);
		}
		goto done;
	}
	if (strncmp(tmpcmd, "@", 1)==0)
	{
		if (strlen(command) == 1)
		{
			if (class_type==0)
				Pw_dispmsg("The @ command must have a filename\r\n",0);
			else
			{
				stat = ("Command Files (*.com)\0*.com\0\0", FileName, TRUE, 0,
					FALSE);
				if (stat==0)
				{
					goto done;
				}
			}
		}
		else if (command[1] != ':')
		{
			strcpy (msg,"Missing colon before filename");
			Pted_Disp_Msg(msg, 1, classpt, class_type);	
			goto done;
		}
		else
		{
			strcpy(FileName,  &command[2]);
		}
		Run_Batch(FileName, classpt, class_type);
		goto done;
	}
	if (strncmp(tmpcmd, "TLE", 3)==0)
	{
/*
.....TN POSMAP[3601]  120 long
.....TLNO POSMAP[3841] 
*/
		ton = 0;
		tseq = 0;
		tok = NULL;
		indx = strchr(command, ':');
		if (indx) tok = strtok(indx+1, " ,\t\n");
		if (tok==NULL)
		{
			sprintf(msg, "Invalid Command %s", command);
			Pted_Disp_Msg(msg, 0, classpt, class_type);
			goto done;
		}
		if (_stricmp(tok, "ON")==0)
		{
			ton = 1;
			tok = strtok(NULL, " ,\t\n");
			if (tok==NULL)
			{
				sprintf("Invalid Command %s", command);
				Pted_Disp_Msg(msg, 0, classpt, class_type);
				goto done;
			}
			if (_stricmp(tok, "SEQ")==0)
			{
				tseq = 1;
				tok = strtok(NULL, " ,\t\n");
			}
		}
		else if (_stricmp(tok, "SEQ")==0)
		{
			tseq = 1;
			tok = strtok(NULL, " ,\t\n");
		}
/*
.....get tool number and tool length
*/
		if (tseq==1)
		{
			i = 0;
			while(tok!=NULL)
			{
				tlno[i] = i+1;
				tllen[i] = atof(tok);
				if (tllen[i]<=0)
				{
					sprintf(msg,"Invalid tool length %d", tllen[i]);
					Pted_Disp_Msg(msg, 1, classpt, class_type);
					goto done;
				}
				i++;
				tok = strtok(NULL, " ,\t\n");
			}
		}
		else
		{
			i = 0;
			while(tok!=NULL)
			{
				tlno[i] = atoi(tok);
				if (tlno[i] <= 0)
				{
					sprintf(msg,"Invalid tool number %d", tlno[i]);
					Pted_Disp_Msg(msg, 1, classpt, class_type);
					goto done;
				}
				tok = strtok(NULL, " ,\t\n");
				if (tok!=NULL)
					tllen[i] = atof(tok);
				if ((tllen[i]<=0)||(tok==NULL))
				{
					sprintf(msg,"Invalid tool length %d\r\n", tllen[i]);
					Pted_Disp_Msg(msg, 1, classpt, class_type);
					goto done;
				}
				i++;
				tok = strtok(NULL, " ,\t\n");
			}
		}
		Ptd_set_tllen(tlno, tllen, i, ton);
		goto done;
	}
/*
.....UNITS:INCH,MM
*/
	if (strncmp(tmpcmd, "UNI", 3)==0)
	{
		indx = strchr(command, ':');
		if (indx) tok = strtok(indx+1, " ,\t\n");
		if (tok==NULL)
		{
			sprintf(msg, "Invalid Command %s", command);
			Pted_Disp_Msg(msg, 0, classpt, class_type);
			goto done;
		}
		if (_stricmp(tok, "INCH")==0)
			i = 1;
		else if (_stricmp(tok, "MM")==0)
			i = 2;
		else
		{
			sprintf("Invalid Command %s", command);
			Pted_Disp_Msg(msg, 0, classpt, class_type);
			goto done;
		}
		Ptd_set_units(i);
		goto done;
	}
/*
.....Transformation commands
*/
	if ((strncmp(tmpcmd, "ADD",3)==0)||
		(strncmp(tmpcmd, "MIR",3)==0)||
		(strncmp(tmpcmd, "MULT",4)==0)||
		(strncmp(tmpcmd, "ROTAT",5)==0)||
		(strncmp(tmpcmd, "SCA",3)==0)||
		(strncmp(tmpcmd, "TR",2)==0))
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			sprintf("Invalid Command %s", command);
			Pted_Disp_Msg(msg, 1, classpt, class_type);
			goto done;
		}
		strcpy(tmpstr1, indx+1);
		indx1 = strrchr(tmpstr1, '\\');
		if (indx1==NULL)
		{
			stat = 1;
			sRange.begin = 1;
			sRange.end = 1;
		}
		else
		{
			stat = Get_Range_from_String (indx1+1, &sRange);
			*indx1 = '\0';
		}
		if (stat==-1)
		{
			Pted_Disp_Msg("Wrong Range Specifier!", 1, classpt, class_type);
			goto done;
		}
		adds = (char**)malloc(50*sizeof(char*));
		for (i=0; i<50; i++)
			adds[i] = (char*)malloc(50*sizeof(char));
/*
.....get every parameter from tmpstr1
*/
		tok = strtok(tmpstr1, " ,\t\n");
		i = 0;
		while (tok!=NULL)
		{
			strcpy(adds[i], tok);
			tok = strtok(NULL, " ,\t\n");
			i++;
		}
		if (i!=0)
		{
			if (class_type==1)
			{
				if (strncmp(tmpcmd, "ADD",3)==0)
					((CPtedWindow *)classpt)->ConvertMathRange(adds, i, sRange, 1);
				else if (strncmp(tmpcmd, "MIR",3)==0)
					((CPtedWindow *)classpt)->ConvertMathRange(adds, i, sRange, 2);
				else if (strncmp(tmpcmd, "MULT",4)==0)
					((CPtedWindow *)classpt)->ConvertMathRange(adds, i, sRange, 3);
				else if (strncmp(tmpcmd, "ROTAT",5)==0)
					((CPtedWindow *)classpt)->ConvertMathRange(adds, i, sRange, 4);
				else if (strncmp(tmpcmd, "SCA",3)==0)
					((CPtedWindow *)classpt)->ConvertMathRange(adds, i, sRange, 5);
				else if (strncmp(tmpcmd, "TR",2)==0)
					((CPtedWindow *)classpt)->ConvertMathRange(adds, i, sRange, 6);
			}
			else
			{
				if (strncmp(tmpcmd, "ADD",3)==0)
					((PtedBatch *)classpt)->ConvertMathRange(adds, i, sRange, 1);
				else if (strncmp(tmpcmd, "MIR",3)==0)
					((PtedBatch *)classpt)->ConvertMathRange(adds, i, sRange, 2);
				else if (strncmp(tmpcmd, "MULT",4)==0)
					((PtedBatch *)classpt)->ConvertMathRange(adds, i, sRange, 3);
				else if (strncmp(tmpcmd, "ROTAT",5)==0)
					((PtedBatch *)classpt)->ConvertMathRange(adds, i, sRange, 4);
				else if (strncmp(tmpcmd, "SCA",3)==0)
					((PtedBatch *)classpt)->ConvertMathRange(adds, i, sRange, 5);
				else if (strncmp(tmpcmd, "TR",2)==0)
					((PtedBatch *)classpt)->ConvertMathRange(adds, i, sRange, 6);
			}
		}
		for (i=0; i<50; i++)
			free(adds[i]);
		free(adds);
		goto done;
	}
		

	if (strncmp(tmpcmd, "CL", 2)==0)
	{
		Pted_SetFtype(3, classpt, class_type);
		goto done;
	}
	
	if (strncmp(tmpcmd, "TAP",3)==0)
	{
		Pted_SetFtype(2, classpt, class_type);
		goto done;
	}
	if (strncmp(tmpcmd, "SIM",3)==0)
	{
		indx = strchr(command, ':');
		if (indx != NULL) strcpy(FileName, indx+1);
		else FileName[0] = '\0';
		if (class_type==0)
		{
			((PtedBatch *)classpt)->OnConvertNctosim(FileName);
		}
		else
		{
			((CPtedWindow *)classpt)->OnConvertNctosim();
		}
		goto done;
	}
	if (strncmp(tmpcmd, "CUT",3)==0)
	{
		indx = strchr(command, ':');
		if (class_type==1)
		{
			if (indx==NULL)
				((CPtedWindow *)classpt)->LoadCutterFile(((CPtedWindow *)classpt)->m_file);
			else
				((CPtedWindow *)classpt)->LoadCutterFile(indx+1);
		}
		else
		{
			if (indx==NULL)
				((PtedBatch *)classpt)->LoadCutterFile(((PtedBatch *)classpt)->m_filen);
			else
				((PtedBatch *)classpt)->LoadCutterFile(indx+1);
		}
		goto done;
	}
	if (strncmp(tmpcmd, "SET",3)==0)
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			goto error;
		}
		else
		{
			if (Ptd_setreg(indx+1) != 0)
			{
				sprintf(msg, "Invalid Register Definition %s", command);
				Pted_Disp_Msg(msg, 1, classpt, class_type);			
			}
		}
		goto done;
	}
		
	if (strncmp(tmpcmd, "LOAD",4)==0)
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			goto error;
		}
		else
		{
			strcpy(FileName, indx+1);
			if (class_type==1)
				((CPtedWindow *)classpt)->LoadProgram(FileName, 1);
			else
				((PtedBatch *)classpt)->LoadProgram(FileName);
		}
		goto done;
	}			

	if (strncmp(tmpcmd, "EX", 2)==0)
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			if (class_type==1)
			{
				((CPtedWindow *)classpt)->DlgQuit();
				goto done;
			}
			else
/*
.....save the changed file into load file
.....and exit
*/
			{
/*
.....do not save the file, just exit
*/
//				strcpy(FileName, ((PtedBatch *)classpt)->m_filen);
				((PtedBatch *)classpt)->m_exit = 1;
				goto done;
			}
		}
		else
		{
			strcpy(FileName, indx+1);
		}
		if (class_type==1)
		{
			((CPtedWindow *)classpt)->ProgramSaveAs(FileName);
			((CPtedWindow *)classpt)->DlgQuit();
		}
		else
		{
			((PtedBatch *)classpt)->ProgramSaveAs(FileName);
			((PtedBatch *)classpt)->m_exit = 1;
		}
		goto done;
	}
	if (strncmp(tmpcmd, "WR", 2)==0)
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			if (class_type==0)
			{
				sprintf(msg,"Invalid Command %s\r\n", command);
				Pw_dispmsg(msg,0);
				goto done;
			}
			else
			{
				stat = browsefile("CL Text Files (*.cla)\0*.cla\0NC Data Files (*.pu*)\0*.pu*\0CL Binary Files (*.cl)\0*.cl\0APT Source Files (*.as)\0*.as\0All Files (*.*)\0*.*\0\0",
					FileName,0,0,FALSE);
				if (stat==0)
				{
					goto error;
				}
				indx = strchr(command, '\\');
				if (indx==NULL)
				{
/*
.....save the whole file
*/
					((CPtedWindow *)classpt)->ProgramSaveAs(FileName);
					goto done;
				}
				else
				{
					stat = Get_Range_from_String (indx+1, &sRange);
					if (stat==-1) 
					{
						goto error;
					}
				}
			}
		}
		else
		{
/*
......allow long filename with space and directory '\' if the filename
......with double quotes
*/
			strcpy(tmpstr1, indx+1);
			if (tmpstr1[0]=='\"')
			{
				indx = strchr(&(tmpstr1[1]), '\"');
				tok = strtok(&(tmpstr1[1]), "\"");
				if ((tok==NULL) || ( indx==NULL))
				{
					sprintf(msg, "Invalid Command %s", command);
					Pted_Disp_Msg(msg, 1, classpt, class_type);			
					goto done;
				}
				strcpy(FileName, tok);
				tok = strtok(NULL, "\\");
				if (tok!=NULL)
				{
					stat = Get_Range_from_String (tok, &sRange);
					if (stat==-1)
					{
						goto error;
					}
				}
			}
			else
			{
				tok = strtok(tmpstr1, " \r\n");
				if (tok==NULL)
				{
					sprintf(msg, "Invalid Command %s", command);
					Pted_Disp_Msg(msg, 1, classpt, class_type);			
					goto done;
				}
				else
				{
					if (tok[0]!='\\')
					{
						strcpy(tmpstr1, tok);
						tok = strtok(tmpstr1, " \t\n\\");
						if (tok!=NULL)
						{
							strcpy(FileName, tok);
							tok = strtok(NULL, " \t\n");
							if (tok!=NULL)
								stat = Get_Range_from_String (tok, &sRange);
							else
							{
								sRange.begin = 1;
								sRange.end = 1;
								stat = 1;
							}
						}
						else
						{
							sRange.begin = 1;
							sRange.end = 1;
							stat = 1;
						}
						if (stat==-1)
							goto error;
					}
					else
					{
						goto error;
					}
				}
			}
		}
		if (class_type==0)
			((PtedBatch *)classpt)->ProgramSaveSelectAs(FileName, sRange);
		else
			((CPtedWindow *)classpt)->ProgramSaveSelectAs(FileName, sRange);
		goto done;
	}
	if (strncmp(tmpcmd, "UNF", 3)==0)
	{
		indx = strchr(command, '\\');
		if (indx==NULL)
		{
/*
.....Unformat the whole file
*/
			sRange.begin = 1;
			sRange.end = 1;
		}
		else
		{
			stat = Get_Range_from_String (indx+1, &sRange);
			if (stat==-1)
			{
				goto error;
			}
		}
		if (class_type==0)
			((PtedBatch *)classpt)->UnformatRange(&sRange);
		else
			((CPtedWindow *)classpt)->UnformatRange(sRange);
		goto done;
	}
	if (strncmp(tmpcmd, "FORM", 4)==0)
	{
		indx = strchr(command, '\\');
		if (indx==NULL)
		{
/*
.....Format the whole file
*/
			sRange.begin = 1;
			sRange.end = 1;
		}
		else
		{
			stat = Get_Range_from_String (indx+1, &sRange);
			if (stat==-1)
			{
				goto error;
			}
		}
		if (class_type==0)
			((PtedBatch *)classpt)->FormatRange(&sRange);
		else
			((CPtedWindow *)classpt)->FormatRange(sRange);
		goto done;
	}
	if (strncmp(tmpcmd, "GET", 3)==0)
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			sprintf(msg,"Invalid Command %s", command);
			Pted_Disp_Msg(msg, 1, classpt, class_type);
			return;
		}
		else
		{
/*
......allow long filename with space and directory '\' if the filename
......with double quotes
*/
			strcpy(tmpstr1, indx+1);
			if (tmpstr1[0]=='\"')
			{
				indx = strchr(&(tmpstr1[1]), '\"');
				tok = strtok(&(tmpstr1[1]), "\"");
				if ((tok==NULL) || ( indx==NULL))
				{
					sprintf(msg, "Invalid Command %s", command);
					Pted_Disp_Msg(msg, 1, classpt, class_type);			
					goto done;
				}
				strcpy(FileName, tok);
				tok = strtok(NULL, "\\");
				if (tok!=NULL)
				{
					stat = Get_Range_from_String (tok, &sRange);
					if (stat==-1)
					{
						goto error;
					}
				}
			}
			else
			{
				tok = strtok(tmpstr1, " \r\n");
				if (tok==NULL)
				{
					sprintf(msg, "Invalid Command %s", command);
					Pted_Disp_Msg(msg, 1, classpt, class_type);			
					goto done;
				}
				else
				{
					if (tok[0]!='\\')
					{
						strcpy(tmpstr1, tok);
						tok = strtok(tmpstr1, " \t\n\\");
						if (tok!=NULL)
						{
							strcpy(FileName, tok);
							tok = strtok(NULL, " \t\n");
							if (tok!=NULL)
								stat = Get_Range_from_String (tok, &sRange);
							else
							{
								sRange.begin = 1;
								sRange.end = 1;
								stat = 1;
							}
						}
						else
						{
							sRange.begin = 1;
							sRange.end = 1;
							stat = 1;
						}
						if (stat==-1)
							goto error;
					}
					else
					{
						goto error;
					}
				}
			}
		}
		if (class_type==0)
			((PtedBatch *)classpt)->ProgramLoadSelect(FileName, sRange);
		else
			((CPtedWindow *)classpt)->ProgramLoadSelect(FileName, sRange);
		goto done;
	}
	
	if (strncmp(tmpcmd, "INC", 3)==0)
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			sprintf(msg,"Invalid Command %s", command);
			Pted_Disp_Msg(msg, 1, classpt, class_type);
			goto error;
		}
		else
		{
/*
......allow long filename with space and directory '\' if the filename
......with double quotes
*/
			strcpy(tmpstr1, indx+1);
			if (tmpstr1[0]=='\"')
			{
				indx = strchr(&(tmpstr1[1]), '\"');
				tok = strtok(&(tmpstr1[1]), "\"");
				if ((tok==NULL) || ( indx==NULL))
				{
					sprintf(msg, "Invalid Command %s", command);
					Pted_Disp_Msg(msg, 1, classpt, class_type);			
					goto done;
				}
				strcpy(FileName, tok);
				tok = strtok(NULL, "\\");
				if (tok!=NULL)
				{
					stat = Get_Range_from_String (tok, &sRange);
					if (stat==-1)
					{
						goto error;
					}
				}
			}
			else
			{
				tok = strtok(tmpstr1, " \r\n");
				if (tok==NULL)
				{
					sprintf(msg, "Invalid Command %s", command);
					Pted_Disp_Msg(msg, 1, classpt, class_type);			
					goto done;
				}
				else
				{
					if (tok[0]!='\\')
					{
						strcpy(tmpstr1, tok);
						tok = strtok(tmpstr1, " \t\n\\");
						if (tok!=NULL)
						{
							strcpy(FileName, tok);
							tok = strtok(NULL, " \t\n");
							if (tok!=NULL)
								stat = Get_Range_from_String (tok, &sRange);
							else
							{
								sRange.begin = 1;
								sRange.end = 1;
								stat = 1;
							}
						}
						else
						{
							sRange.begin = 1;
							sRange.end = 1;
							stat = 1;
						}
						if (stat==-1)
							goto error;
					}
					else
					{
						goto error;
					}
				}
			}
		}
		if (class_type==0)
			((PtedBatch *)classpt)->ProgramIncludeSelect(FileName, sRange);
		else
			((CPtedWindow *)classpt)->ProgramIncludeSelect(FileName, sRange);
		goto done;
	}
	if (strncmp(tmpcmd, "CON", 3)==0)
	{
		indx = strchr(command, '\\');
		if (indx==NULL)
		{
/*
.....Convert the whole file
*/
			sRange.begin = 1;
			sRange.end = 1;
		}
		else
		{
			stat = Get_Range_from_String (indx+1, &sRange);
			if (stat==-1)
			{
				goto error;
			}
		}
		if (class_type==0)
			((PtedBatch *)classpt)->ConvertRange(&sRange);
		else
			((CPtedWindow *)classpt)->ConvertRange(sRange);
		goto done;
	}
	if (strncmp(tmpcmd, "SOURCE", 3)==0)
	{
		indx = strchr(command, '\\');
		if (indx==NULL)
		{
/*
.....Convert the whole file
*/
			if (class_type==1)
				((CPtedWindow *)classpt)->OnConvertNctoapt();
			else
				((PtedBatch *)classpt)->OnConvertNctoapt();		
			goto done;
		}
		else
		{
			stat = Get_Range_from_String (indx+1, &sRange);
			if (stat==-1)
			{
				return;
			}
			sprintf(msg, "Range not implemented %s", command);
			Pted_Disp_Msg(msg, 1, classpt, class_type);
			return;
		}
	}
	if (strncmp(tmpcmd, "REV", 3)==0)
	{
		indx = strchr(command, '\\');
		if (indx==NULL)
		{
			sRange.begin = 1;
			sRange.end = 1;
		}
		else
		{
			stat = Get_Range_from_String (indx+1, &sRange);
			if (stat==-1)
			{
				goto error;
			}
		}
		if (class_type==0)
			((PtedBatch *)classpt)->ReverseRange(&sRange);
		else
			((CPtedWindow *)classpt)->ReverseRange(sRange);
		goto done;
	}
	if (strncmp(tmpcmd, "RES", 3)==0)
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			sprintf(msg,"Invalid Command %s", command);
			Pted_Disp_Msg(msg, 1, classpt, class_type);
			return;
		}
		strcpy(tmpstr1, indx+1);
		indx = strchr(tmpstr1, '\\');
		stat = 1;
		if (indx!=NULL)
		{
			stat = Get_Range_from_String (indx+1, &sRange);
			*indx = '\0';
			if (stat==-1)
			{
				return;
			}
		}
		else
		{
			sRange.begin = 1;
			sRange.end = 1;
		}
		tok = strtok(tmpstr1, " ,\t\n");
		int bseq = 1;
		int seqinc = 1;
		int seqn = 1;
		int nonly = 0;
		if (tok!=NULL)
		{
			bseq = atoi(tok);
			tok = strtok(NULL, " ,\t\n");
			if (tok!=NULL)
			{
				seqinc = atoi(tok);
				tok = strtok(NULL, " ,\t\n");
				if (tok!=NULL)
				{
					seqn = atoi(tok);
					tok = strtok(NULL, " ,\t\n");
					if (tok!=NULL)
					{
						nonly = atoi(tok);
					}
				}
			}
		}	
		if (class_type==0)
			((PtedBatch *)classpt)->ReseqRange(&sRange, bseq, seqinc, seqn, nonly);
		else
			((CPtedWindow *)classpt)->ReseqRange(sRange, bseq, seqinc, seqn, nonly);
		goto done;
	}
	if (strncmp(tmpcmd, "SUB", 3)==0)
	{
		stat = 1;
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			sprintf(msg,"Invalid Command %s", command);
			Pted_Disp_Msg(msg, 1, classpt, class_type);
			goto error;
		}
		strcpy(tmpstr1, indx+1);

		tok = strtok(tmpstr1, " ,\t\n");
		if (tok==NULL)
		{
			sprintf(msg,"Invalid Command %s", command);
			Pted_Disp_Msg(msg, 1, classpt, class_type);
			goto error;
		}
		else
		{
			if (strncmp(tok, "VER", 3)==0)
			{
				vflag = 1;
				tok = strtok(NULL, " ,\t\n");
				if (tok==NULL)
				{
					sprintf(msg,"Invalid Command %s", command);
					Pted_Disp_Msg(msg, 1, classpt, class_type);
					goto error;
				}
			}
			strcpy(fstr, tok);
			tok = strtok(NULL, " ,\t\n");
			if (tok==NULL)
			{
				rstr[0] = '\0';
				sRange.begin = 1;
				sRange.end = 1;
			}
			else
			{
				if (tok[0]!='\\')
				{
					strcpy(tmpstr1, tok);
					tok = strtok(tmpstr1, " ,\t\n\\");
					if (tok!=NULL)
					{
						strcpy(rstr, tok);
						tok = strtok(NULL, " \t\n");
						if (tok!=NULL)
							stat = Get_Range_from_String (tok, &sRange);
						else
						{
							sRange.begin = 1;
							sRange.end = 1;
						}
					}
					else
					{
						rstr[0] = '\0';
						sRange.begin = 1;
						sRange.end = 1;
					}
				}
				else
				{
					rstr[0] = '\0';
					stat = Get_Range_from_String (tok+1, &sRange);
				}
			}
		}
		if (stat!=-1)
		{
			Substitute(fstr, rstr, vflag, sRange, classpt, class_type);
		}
		goto done;
	}
	if (strncmp(tmpcmd, "COLOR", 5)==0)
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			sprintf(msg,"Invalid Command %s", command);
			Pted_Disp_Msg(msg, 1, classpt, class_type);
			goto error;
		}
		strcpy(tmpstr1, indx+1);
		if (_stricmp(tmpstr1, "ON")==0)
		{
			flag = 1;
		}
		else
		{
			flag = 0;
		}
		if (class_type==0)
			((PtedBatch *)classpt)->SetWindowSyntaxClr(flag);
		else
			((CPtedWindow *)classpt)->SetWindowSyntaxClr(flag);
		goto done;
	}
	if (strncmp(tmpcmd, "FILE", 4)==0)
	{
		indx = strchr(command, ':');
		if (indx==NULL)
		{
			sprintf(msg,"Invalid Command %s", command);
			Pted_Disp_Msg(msg, 1, classpt, class_type);
			goto error;
		}
		strcpy(tmpstr1, indx+1);
		if (_stricmp(tmpstr1, "TEXT")==0)
		{
			ftype = 1;
		}
		else if (_stricmp(tmpstr1, "MCD")==0)
		{
			ftype = 2;
		}
		else if (_stricmp(tmpstr1, "CL")==0)
		{
			ftype = 3;
		}
		else if (_stricmp(tmpstr1, "APT")==0)
		{
			ftype = 4;
		}
		else if (_stricmp(tmpstr1, "SIM")==0)
		{
			ftype = 5;
		}
		else if (_stricmp(tmpstr1, "CUT")==0)
		{
			ftype = 6;
		}
		Pted_SetFtype(ftype, classpt, class_type);
		goto done;
	}
error:;
/*
.....invalid command
*/
	if (strncmp(tmpcmd, "QUIET", 5)==0)
		return;
	else
	{
		sprintf(msg,"Invalid Command %s", command);
		Pted_Disp_Msg(msg, 0, classpt, class_type);
	}
	return;
done:;	
	if (class_type==1)
	{
		sprintf(msg, "%% %s", hldcmd);
		Pted_Disp_Msg(msg, 0, classpt, class_type);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  Get_Range_from_String (char *rangestr, PtedRangeStruct*  sRange)
c
c   FUNCTION:  This function get a range struction from range string
c				
c
c   INPUT:  rangestr: string to get range from
c			
c   OUTPUT: sRange: range structure
c
c***********************************************************************
*/
extern "C" int Get_Range_from_String (char *rangestr, PtedRangeStruct*  sRange)
{
	char *indx;
	char tmprange[400];
	char bstr[200], estr[200];
	strcpy(tmprange, rangestr); 
	indx = strchr(rangestr, ':');
	if (indx==NULL)
/*
.....standalone range specify
*/
	{
		StrtoUpper(rangestr, rangestr);
		if (strcmp(rangestr, "%WH")==0)
		{
			sRange->begin = 1;
		}
		else if (strcmp(rangestr, "%SE")==0)
		{
			sRange->begin = 2;
		}
		else if (atoi(rangestr)>0)
		{
			sRange->begin = 3;
			sRange->end = atoi(rangestr);
		}
		else
		{
			Pw_dispmsg("Wrong Range Specifier!\r\n", 0);
			return -1;
		}
	}
	else
	{
		strcpy(estr, indx+1);
		*indx = '\0';
		strcpy(bstr, rangestr);
		if (strcmp(bstr, ".")==0)
			sRange->begin = 3;
		else if (strcmp(bstr, "%BE")==0)
			sRange->begin = 4;
		else if (bstr[0]=='\'')
		{
			sRange->begin = 6;
			indx = strtok(&(bstr[1]), "\'");
			if (indx!=NULL)
				strcpy(sRange->bstring, indx);
			else
			{
				Pw_dispmsg("Wrong Range Specifier!\r\n", 0);
				return -1;
			}
		}
		else if (bstr[0]=='\"')
		{
			sRange->begin = 6;
			indx = strtok(&(bstr[1]), "\"");
			if (indx!=NULL)
				strcpy(sRange->bstring, indx);
			else
			{
				Pw_dispmsg("Wrong Range Specifier!\r\n", 0);
				return -1;
			}
		}
		else 
		{
			sRange->begin = 5;
			strcpy(sRange->baddress, bstr);
		}
		
		if (strcmp(estr, "%EN")==0)
			sRange->end = 2;
		else if (atoi(estr)>0)
		{
			sRange->end = 1;
			strcpy(sRange->enumber, estr);
		}
		else if (estr[0]=='\'')
		{
			sRange->end = 4;
			indx = strtok(&(estr[1]), "\'");
			if (indx!=NULL)
				strcpy(sRange->estring, indx);
			else
			{
				Pw_dispmsg("Wrong Range Specifier!\r\n", 0);
				return -1;
			}
		}
		else if (bstr[0]=='\"')
		{
			sRange->end = 4;
			indx = strtok(&(estr[1]), "\"");
			if (indx!=NULL)
				strcpy(sRange->estring, indx);
			else
			{
				Pw_dispmsg("Wrong Range Specifier!\r\n", 0);
				return -1;
			}
		}
		else 
		{
			sRange->end = 3;
			strcpy(sRange->eaddress, estr);
		}
	}
	return 1;
}


extern "C" void LoadCutterFile(char *filename, int *classpt, int class_type)
{
	FILE *fp = NULL;
	char errMsg[UX_MAX_PATH];
	char *tmpstr = NULL;
	int kerr;

	Pted_SetFtype(6, classpt, class_type);
/*
.....open file
*/
	if (class_type==0)
	{
		;
	}
	else
	{
		((CPtedTextView*)classpt)->LoadFromFile(filename, 6);
	}
	Ptd_docutter(tmpstr, &kerr, errMsg);
	if(kerr)
	{
		Pted_Disp_Msg(errMsg, 1, classpt, class_type);
		return;
	}
}
/***********************************************************************
c
c   SUBROUTINE:  Substitute(char *fstr, char *rstr, int vflag, 
c						PtedRangeStruct range)
c				
c   FUNCTION:  Substitute all string fstr with rstr in specified range
c				it always case sensitive
c
c   INPUT:  
c			fstr: find string
c			rstr: replace string
c			vflag: 1: display a verify window
c			range: range to do substitute
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
extern "C" void Substitute(char *infstr, char *inrstr, int vflag, 
		PtedRangeStruct range, int *classpt, int class_type)
{
	int flen, fletter;
	int rlen;
	flen = strlen(infstr);
	rlen = strlen(inrstr);
	char *fstr, *rstr;
	fstr = (char*)malloc((flen+1)*sizeof(char));
	rstr = (char*)malloc((rlen+1)*sizeof(char));
/*
.....default to replace all with register match
*/
	fletter = 1;
	if (infstr[0]=='\'')
	{
		if (infstr[flen-1]!='\'')
		{
			Pted_Disp_Msg("Wrong find string specifier!", 1, classpt, class_type);
			goto done;
		}
		strcpy(fstr, &(infstr[1]));
		fstr[flen-2] = '\0';
		fletter = 0;
	}
	else
		strcpy(fstr, infstr);
	if (inrstr[0]=='\'')
	{
		if (inrstr[rlen-1]!='\'')
		{
			Pted_Disp_Msg("Wrong find string specifier!", 1, classpt, class_type);
			goto done;;
		}
		strcpy(rstr, &(inrstr[1]));
		rstr[rlen-2] = '\0';
	}
	else
		strcpy(rstr, inrstr);
	if (class_type==1)
	{
		((CPtedWindow *)classpt)->OnReplaceAll(fstr, rstr, 1, fletter, &range, vflag);
		((CPtedWindow *)classpt)->Update_undo_redomenu(1,0);
	}
	else
		((PtedBatch *)classpt)->OnReplaceText(fstr, rstr, 1, 1, fletter, &range, 1, vflag);
done:
	free(fstr);
	free(rstr);
}
