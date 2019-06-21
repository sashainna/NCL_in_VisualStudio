/*********************************************************************
**  NAME:  PtdGlobal.c
**  Description:
**        functions can used for both WNT and UNIX
**			for PTED Application
**    CONTAINS:
**			StrtoUpper(char *in, char*out)
**			csystem(char *cmdln, int *nc, int *result)
**			Ptd_SameAdds(char *inadds, char *cmstr, int bcase)
**			Ptd_open_flist(char* list_fname, char file_list[5][UX_MAX_PATH], char*errMsg)
**			Ptd_save_flist(char* list_fname, char file_list[5][UX_MAX_PATH], char*errMsg)
**			Ptd_rmchar(char *string, char c)
**			Ptd_rm_prespc(char *string)
**			Ptd_TextReplace(char **textstr, int spos, int epos, char *rstr)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdGlobal.c , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:59:19
*********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#ifndef WNT
#ifndef VAXVMS
#ifndef HP
#ifndef IBM
#define ptd_gtregv			ptd_gtregv_
#define ptd_defmtcod	      ptd_defmtcod_
#define csystem				csystem_
#endif
#endif
#endif
#endif

#include "pwenv.h"
#include "PtdFunc.h"

#ifdef WNT 
#include <Windows.h>
#include <process.h>

#endif
char Pted_localdir[UX_MAX_PATH];

/*********************************************************************
**    E_FUNCTION     : StrtoUpper(char *in, char*out)
**       Change string to upper case
**    PARAMETERS
**       INPUT  :  in: input string 
**       OUTPUT :  out: output string which upper cased
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void StrtoUpper(char *in, char*out)
{
	int len, i;
	len = strlen(in);
	for (i=0; i<len; i++)
	{
		out[i] = toupper(in[i]);
	}
	out[i] = '\0';
}

/*********************************************************************
**    E_FUNCTION     : csystem(char *cmdln, int *nc, int *result)
**       Execute command "cmdln"
**    PARAMETERS
**       INPUT  :  cmdln: command line to be executed
**							nc:  number of command line characters
**       OUTPUT :  result: -1: failed
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void csystem(char *cmdln, int *nc, int *result)
{
	char *args[256], *str;
	int len, i;
	char cmdline[UX_MAX_PATH];
	strncpy(cmdline, cmdln, *nc);
	cmdline[*nc] = '\0';
#ifdef WNT
	str = strtok(cmdline, " \t\n");
	i = 0;
	while(i<UX_MAX_PATH && str)
	{
		len = strlen(str);
		args[i] = (char*)malloc((len+1)*sizeof(char));
		strcpy(args[i], str);
		i++;
		str = strtok(NULL, " \t\n");
	}
	args[i] = NULL;
	*result = _spawnvp(_P_WAIT, args[0], args);
#else
	*result = system (cmdline);
#endif
}
/*********************************************************************
**    E_FUNCTION     : Ptd_SameAdds(char *inadds, char *cmstr, int bcase)
**    		Compare if inadds and cmstr are same letter address   
**				for example inadds = "A.0", cmstr = "A0.0", it will return 1
**				because they have same register and same value.
**    PARAMETERS
**       INPUT  :  inadds: Letter address to compare
**							cmstr: Letter address to compare
**							bcase: 1: case sensitive
**       OUTPUT :  None
**    RETURNS      : 1: Same
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int Ptd_SameAdds(char *inadds, char *cmstr, int bcase)
{
	double gval1, gval2;
	int kerr, regonly;
	int kreg1, kreg2;
	int nc, type;
	char *adds = (char*)malloc(200*sizeof(char));
/*
.....the adds may be a text presentation of Rega&Val
*/
	Ptd_GetFindStr(inadds, &adds, bcase, &type);
	if (type==1) return 0;
	regonly = 0;
	nc = strlen(adds);
	ptd_defmtcod(&kreg1, &gval1, adds, &nc, &bcase, &kerr);
	if (kerr==-1) return 0;
	if (kerr==2)  regonly = 1;
	nc = strlen(cmstr);
	ptd_defmtcod(&kreg2, &gval2, cmstr, &nc, &bcase, &kerr);
	if (kerr==-1) return 0;
	if ((kreg1==kreg2)&&((gval1==gval2)||(regonly)))
		return 1;
	return 0;
}


/*********************************************************************
**    E_FUNCTION     : Ptd_open_flist(char* list_fname, 
**								char file_list[5][256], char*errMsg)
**    	Open a file list file and put the file list into file_list array	
**				
**    PARAMETERS
**       INPUT  :  list_fname: list file to be opened
**							
**       OUTPUT :  file_list: list file array
**						errMsg: error message
**    RETURNS      : -1: failed
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int Ptd_open_flist(char* list_fname, char file_list[5][UX_MAX_PATH], char*errMsg)
{
	char line[UX_MAX_PATH];
	FILE *fp;
	int i, len;
/*
.....initial file list
*/
	for (i=0; i<5; i++)
		file_list[i][0] = '\0';

/*
.....open file
*/
	if ((!strlen(list_fname)) || ((fp = fopen(list_fname, "r") )== NULL))
	{
		sprintf(errMsg,"Error: Unable to open previous files data file:\n %s\nNo previous file listed",
							list_fname);
		return -1;
	}
	i = 0;
	while ((fgets( line, UX_MAX_PATH, fp ) != NULL)&&(i<5))
	{
		strcpy(file_list[i], line);
		len = strlen(line);
		if (len > 1 && file_list[i][len-2]=='\r')
			file_list[i][len-2] = '\0';
		else if (len > 0 && file_list[i][len-1]=='\n')
			file_list[i][len-1] = '\0';
		i++;
	}
	fclose(fp);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : Ptd_save_flist(char* list_fname, 
**								char file_list[5][256], char*errMsg)
**    	save a file list array into a file_list file
**				
**    PARAMETERS
**       INPUT  :  file_list: list file array
**							
**       OUTPUT :  list_fname: list file to be saved
**						errMsg: error message
**    RETURNS      : -1: failed
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int Ptd_save_flist(char* list_fname, char file_list[5][UX_MAX_PATH], char*errMsg)
{
	char savstring[UX_MAX_PATH];
	FILE *fp;
	int i;
/*
.....open file
*/
	if ((!strlen(list_fname)) || ((fp = fopen(list_fname, "w") )== NULL))
	{
		sprintf(errMsg,"Error: Unable to open previous files data file:\n %s to write",
							list_fname);
		return -1;
	}
	i = 0;
	while (i<5)
	{
		sprintf(savstring, "%s\n", file_list[i]);
		fputs(savstring, fp );
		i++;
	}
	fclose(fp);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : Ptd_rmchar(char *string, char c)
**    	Remove char c from string 
**				
**    PARAMETERS
**       INPUT  : string: string to be changed
**						c: character to be removed	
**       OUTPUT :  string
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
void Ptd_rmchar(char *string, char c)
{
	int i, j;
	char *tempstr;
	int len = strlen(string);
	tempstr = (char*)malloc((len+1)*sizeof(char));
	for (i=0, j=0; i<len; i++)
	{
		if (string[i]!=c)
			tempstr[j++] = string[i];
	}
	tempstr[j] = '\0';
	strcpy(string, tempstr);
	free(tempstr);
}
			
/*********************************************************************
**    E_FUNCTION     : Ptd_rm_prespc(char *string)
**    	Remove spaces before any other characters from string 
**				
**    PARAMETERS
**       INPUT  : string: string to be changed
**       OUTPUT :  string
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
Ptd_rm_prespc(char *string)
{
	int i;
	char *tempstr;
	int len = strlen(string);
	if (len==0)
		return 0;

	tempstr = (char*)malloc((len+1)*sizeof(char));
	for (i=0; i<len; i++)
	{
		if (string[i]!=' ')
		{
			strcpy(tempstr, &(string[i]));
			break;
		}
	}
	strcpy(string, tempstr);
	free(tempstr);
	return(0);
}
	
/*********************************************************************
**    E_FUNCTION     : Ptd_FindStringInTextLine(char* pszFindWhere, 
**						char* pszFindWhat, int bWholeWord)
**       Search string "pszFindWhat" from "pszFindWhere" 
**				
**    PARAMETERS
**       INPUT  :  pszFindWhere: input string  to search from
**				pszFindWhat: String to search
**				bWholeWord: if match whole word
**       OUTPUT :  None
**    RETURNS      : find position (-1: not found)
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int Ptd_FindStringInTextLine(char* pszFindWhere, char* pszFindWhat, int bWholeWord)
{
	char *pszPos;
	int nCur = 0;
	int nLength = lstrlen(pszFindWhat);
	for (;;)
	{
		pszPos = strstr(pszFindWhere, pszFindWhat);
		if (pszPos == NULL)
			return -1;
		if (! bWholeWord)
			return nCur + (pszPos - pszFindWhere);
		if (pszPos > pszFindWhere && (isalnum(pszPos[-1]) || pszPos[-1] == '_'))
		{
			nCur += (pszPos - pszFindWhere);
			pszFindWhere = pszPos + 1;
			continue;
		}
		if (isalnum(pszPos[nLength]) || pszPos[nLength] == '_')
		{
			nCur += (pszPos - pszFindWhere + 1);
			pszFindWhere = pszPos + 1;
			continue;
		}
		return nCur + (pszPos - pszFindWhere);
	}
	return -1;
}

/*********************************************************************
**    E_FUNCTION     : Ptd_FindAddrInTextLine(char* pszFindWhere, 
**						char* pszFindAddr, int* flen, double *fval)
**       Search letter address "adds" from "pszFindWhere" 
**				
**    PARAMETERS
**       INPUT  :  pszFindWhere: input string  to search from
**							pszFindAddr: Letter address to search
**       OUTPUT :  
**				flen: find letter address length
**				fval: find letter address value
**				fpos: find position
**    RETURNS      : find position (0: not found)
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int Ptd_FindAddrInTextLine(char* pszFindWhere, char* pszFindAddr, int* flen, double *fval,int *fpos)
{
	int bcase, kerr,regonly;
	char bkstr[500];
	int line_num, i, len, end, tlen, stat;
	double gval1, gval2;
	int lstart;
	int kreg1, kreg2, start, next, possav, pos1;
	int nc = strlen(pszFindAddr);
	regonly = 0;
	lstart = 1;
	tlen = strlen(pszFindWhere);

	*flen = 0;
	if (nc>499) nc=499;
	for (i=0;i<nc;i++) bkstr[i] = toupper(pszFindAddr[i]);
	bkstr[nc] = '\0';
/*
.....Call ptd_defmtcod() to handle the case of a register specified with
.....no value, then call ptd_gtregv() to handle registers with a value.
.....This should be done a better way.
*/
	bcase = 0;
	ptd_defmtcod(&kreg1, &gval1, bkstr, &nc, &bcase, &kerr);
	if (kerr == 2)
		regonly = 1;
	else
	{
		ptd_gtregv (&kreg1,&gval1, bkstr, &nc, &lstart, &end, &bcase);
		if (kreg1==-1000) return 0;
	}
	*fpos = -1;
	i = 0;
	line_num = 0;
		
	start = 0;
	lstart = 1;
beginbk:
	if (start>=tlen) 
	{
		if (*fpos==-1)
			return 0;
		else
			return 1;
	}
	len = tlen;
	pos1 = start;
	stat = Ptd_Get_NxtBk(pszFindWhere, bkstr, start, &next);
	start = next;
	if (strlen(bkstr)==0)
		goto beginbk;
	possav = next-1;
	len = strlen(bkstr);
	
beginreg:
	ptd_gtregv (&kreg2,&gval2, bkstr, &len, &lstart, &end, &bcase);
	if (kreg2!=-1000) 	
/*
.....found a letter address, compare to "address"
*/
	{
		pos1 = pos1 + end;
		if ((kreg1==kreg2)&&((gval1==gval2)||(regonly)))
		{
			*fpos = pos1 - end + lstart - 1;
			*flen = end - lstart+1;
			*fval = gval2;
			goto done;
		}
		if (end<len)
		{
			while (bkstr[end]==' ')
			{
				if (end<len)
				{
					end++;
					pos1++;
				}
			}
		}
		strcpy(bkstr, &(bkstr[end]));
		len = strlen(bkstr);
		if (len==0) goto beginbk;
		goto beginreg;
	}
	else
	{
		strcpy(bkstr, &(bkstr[1]));
		len--;
		pos1++;
		if (len<=0) goto beginbk;
		goto beginreg;
	}
done:
	return 1;
}
/*********************************************************************
**    E_FUNCTION     : Ptd_FindAddrInTextLine2(char* pszFindWhere,
**						int kreg, double gval, int* flen, double *fval)
**       Search letter address "adds" from "pszFindWhere" 
**				
**    PARAMETERS
**       INPUT  :  pszFindWhere: input string  to search from
**				kreg: Letter address register number
**				gval: letter adrress 
**       OUTPUT :  
**				flen: find letter address length
**				fval: find letter address value
**				fpos: find position
**    RETURNS      : find position (0: not found)
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int Ptd_FindAddrInTextLine2(char* pszFindWhere, int kreg, double gval, int regonly, int* flen, double *fval,int *fpos)
{
	int bcase;
	char bkstr[500];
	int line_num, i, len, end, tlen, stat;
	double gval2;
	int lstart;
	int kreg2, start, next, possav, pos1;
	lstart = 1;
	tlen = strlen(pszFindWhere);

	*flen = 0;
	*fpos = -1;
	i = 0;
	line_num = 0;
		
	start = 0;
	lstart = 1;
beginbk:
	if (start>=tlen) 
	{
		if (*fpos==-1)
			return 0;
		else
			return 1;
	}
	len = tlen;
	pos1 = start;
	stat = Ptd_Get_NxtBk(pszFindWhere, bkstr, start, &next);
	start = next;
	if (strlen(bkstr)==0)
		goto beginbk;
	possav = next-1;
	len = strlen(bkstr);
beginreg:
	ptd_gtregv (&kreg2,&gval2, bkstr, &len, &lstart, &end, &bcase);
	if (kreg2!=-1000) 	
/*
.....found a letter address, compare to "address"
*/
	{
		pos1 = pos1 + end;
		if ((kreg==kreg2)&&((gval==gval2)||(regonly)))
		{
			*fpos = pos1 - end + lstart - 1;
			*flen = end - lstart+1;
			*fval = gval2;
			goto done;
		}
		if (end<len)
		{
			while (bkstr[end]==' ')
			{
				if (end<len)
				{
					end++;
					pos1++;
				}
			}
		}
		strcpy(bkstr, &(bkstr[end]));
		len = strlen(bkstr);
		if (len==0) goto beginbk;
		goto beginreg;
	}
	else
	{
		strcpy(bkstr, &(bkstr[1]));
		len--;
		pos1++;
		if (len<=0) goto beginbk;
		goto beginreg;
	}
done:
	return 1;
}
