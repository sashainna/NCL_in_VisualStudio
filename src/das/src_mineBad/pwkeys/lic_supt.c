/***********************************************************************
**
**  FILE NAME: lic_supt.c
**  CONTAINS:
**			to_upper(str)
**			get_licbuf(batfile, flen, buf, bufnum, err)
**			get_batbuf(batfile, flen, buf, bufnum, err)
**			addlicence(record)
**			openlicfile(file)
**			closelicfile()
**			cydelfile(fnam1, nc1, fnam2, nc2, err)
**			
**    COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
**          All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       lic_supt.c , 23.1
**    DATE AND TIME OF LAST  MODIFICATION
**       05/22/12 , 11:15:11 
**
***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef WNT
#include <windows.h> 
#else
#include "licfc.h"
#endif

static FILE * license_fstream;

/*********************************************************************
**	E_FUNCTION:	to_upper(str)
**			This function converts an Ascii string to all
**			upper case letters.
**    PARAMETERS   
**       INPUT  :	str = input text string
**       OUTPUT :  	str = returned string
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
 
void to_upper(str)
	char *str;
{
	char *p;
	int i;
/*
.....Convert string to uppercase
*/
	p = str;
	for (i=0;i<strlen(str);i++)
	{
		if (isalpha(*p) && islower(*p)) *p = toupper(*p);
		p++;
	}
	return;
}
/***********************************************************************
c
c   SUBROUTINE: get_licbuf(batfile, flen, buf, bufnum, err)	
c
c   FUNCTION:  Get the license record buffer from a license file
c
c   INPUT:  batfile : license file
c			flen: length of batfile
c			
c			
c   OUTPUT: buf: buffer to store license records
c			bufnum: number of license record 'buf' have
c			err: 0: no errors
c
c***********************************************************************
*/
void get_licbuf(batfile, flen, buf, bufnum, err)
char *batfile;
int *flen;
char buf[10000][10][132];
int *bufnum;
int *err;
{
	FILE * fstream;
	char line[132];
	char *tok, *tok1;
	char msg[256];
	int krec,stx;
	batfile[*flen] = '\0';
	if ((fstream = fopen(batfile, "r"))==NULL)
	{
		*err = 1;
		sprintf (msg, "Can't open license file %s", batfile);
#ifdef WNT
		Pw_dispmsg(msg, 0);
#else
		lic_mfmsg_box(NULL, "Error!", msg);
#endif
		return;
	}
/*
.....reading batch file and store in the buf
*/
	krec = 0;
getline:
	if (fgets(line, 132, fstream)==NULL)
	{
		if (krec==0)
		{
			*err = 1;
#ifdef WNT
			Pw_dispmsg("No complete record or format can't read!", 0);
#else
			lic_mfmsg_box(NULL, "Error!",
						 "No complete record or format can't read!");
#endif
			fclose (fstream);
			return;
		}
		fclose (fstream);
		*bufnum = krec; 
		*err = 0;
		return;
	}
	tok = NULL;
	to_upper(line);
	tok = strtok(line, "|\x02\x0C\r\n");
	if (tok==NULL) goto getline;
	if (strncmp(tok, "COMPANY:",8)==0)
	{
		stx = 8;
		while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
/*		if (tok[stx]=='\0')
		{
/*
.....get company info from following line
*/
/*			while (fgets(line, 132, fstream)!=NULL)
			{
				to_upper(line);
				tok = strtok(line, "|\x02\x0C\r\n");
				if (tok==NULL) continue;
				stx = 0;
				while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
				if (tok[stx]=='\0') continue;
				break;
			}
		}
*/
		if (tok != NULL)
		{
/*
.....using strncpy because we don't want put '\0' 
.....fortran code init all buf to spaces, we want keep it
*/
			if (strlen(tok)-stx>0)
				strncpy(buf[krec][0], &(tok[stx]), strlen(tok)-stx);
		}
		goto getline;
	}
	else if (strncmp(tok, "HARDWARE:", 9)==0)
	{
		stx = 9;
		while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
/*		if (tok[stx]=='\0')
		{
/*
.....get Hardware info from following line
*/
/*			while (fgets(line, 132, fstream)!=NULL)
			{
				to_upper(line);
				tok = strtok(line, "|\x02\x0C\r\n");
				if (tok==NULL) continue;
				stx = 0;
				while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
				if (tok[stx]=='\0') continue;
				break;
			}
		}
*/
		if (tok != NULL)
		{
/*
.....using strncpy because we don't want put '\0' 
.....fortran code init all buf to spaces, we want keep it
*/
			if (strlen(tok)-stx>0)
				strncpy(buf[krec][1], &(tok[stx]), strlen(tok)-stx);
		}
		goto getline;
	}
	else if (strncmp(tok, "SOFTWARE:",9)==0)
	{
		stx = 9;
		while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
/*		if (tok[stx]=='\0')
		{
/*
.....get Hardware info from following line
*/
/*			while (fgets(line, 132, fstream)!=NULL)
			{
				to_upper(line);
				tok = strtok(line, "|\x02\x0C\r\n");
				if (tok==NULL) continue;
				stx = 0;
				while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
				if (tok[stx]=='\0') continue;
				break;
			}
		}
*/
		if (tok != NULL)
		{
/*
.....using strncpy because we don't want put '\0' 
.....fortran code init all buf to spaces, we want keep it
*/
			if (strlen(tok)-stx>0)
				strncpy(buf[krec][2], &(tok[stx]), strlen(tok)-stx);
		}
		goto getline;
	}
	else if (strncmp(tok, "OPTIONS:",8)==0)
	{
		stx = 8;
		while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
/*		if (tok[stx]=='\0')
		{
/*
.....get Hardware info from following line
*/
/*			while (fgets(line, 132, fstream)!=NULL)
			{
				to_upper(line);
				tok = strtok(line, "|\x02\x0C\r\n");
				if (tok==NULL) continue;
				stx = 0;
				while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
				if (tok[stx]=='\0') continue;
				break;
			}
		}
*/
		if (tok != NULL)
		{
/*
.....using strncpy because we don't want put '\0' 
.....fortran code init all buf to spaces, we want keep it
*/
			if (strlen(tok)-stx>0)
				strncpy(buf[krec][3], &(tok[stx]), strlen(tok)-stx);
		}
		goto getline;
	}
	else if (strncmp(tok, "NUM OF USERS:",13)==0)
	{
		stx = 13;
		while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
/*		if (tok[stx]=='\0')
		{
/*
.....get Hardware info from following line
*/
/*			while (fgets(line, 132, fstream)!=NULL)
			{
				to_upper(line);
				tok = strtok(line, "|\x02\x0C\r\n");
				if (tok==NULL) continue;
				stx = 0;
				while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
				if (tok[stx]=='\0') continue;
				break;
			}
		}
*/
		if (tok != NULL)
		{
/*
.....using strncpy because we don't want put '\0' 
.....fortran code init all buf to spaces, we want keep it
*/
			if (strlen(tok)-stx>0)
				strncpy(buf[krec][4], &(tok[stx]), strlen(tok)-stx);
		}
		goto getline;
	}
	else if (strncmp(tok, "LICENSE TERM:",13)==0)
	{
		stx = 13;
		while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
/*		if (tok[stx]=='\0')
		{
/*
.....get Hardware info from following line
*/
/*			while (fgets(line, 132, fstream)!=NULL)
			{
				to_upper(line);
				tok = strtok(line, "|\x02\x0C\r\n");
				if (tok==NULL) continue;
				stx = 0;
				while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
				if (tok[stx]=='\0') continue;
				break;
			}
		}
*/
		if (tok != NULL)
		{
/*
.....using strncpy because we don't want put '\0' 
.....fortran code init all buf to spaces, we want keep it
*/
			if (strlen(tok)-stx>0)
				strncpy(buf[krec][5], &(tok[stx]), strlen(tok)-stx);
		}
		goto getline;
	}
	else if (strncmp(tok, "VERSION:",8)==0)
	{
		stx = 8;
		while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
/*		if (tok[stx]=='\0')
		{
/*
.....get Hardware info from following line
*/
/*			while (fgets(line, 132, fstream)!=NULL)
			{
				to_upper(line);
				tok = strtok(line, "|\x02\x0C\r\n");
				if (tok==NULL) continue;
				stx = 0;
				while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
				if (tok[stx]=='\0') continue;
				break;
			}
		}
*/
		if (tok != NULL)
		{
/*
.....using strncpy because we don't want put '\0' 
.....fortran code init all buf to spaces, we want keep it
*/
			if (strlen(tok)-stx>0)
				strncpy(buf[krec][6], &(tok[stx]), strlen(tok)-stx);
		}
		goto getline;
	}
	else if (strncmp(tok, "SYSTEM ID:",10)==0)
	{
		stx = 10;
		while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
/*		if (tok[stx]=='\0')
		{
/*
.....get Hardware info from following line
*/
/*			while (fgets(line, 132, fstream)!=NULL)
			{
				to_upper(line);
				tok = strtok(line, "|\x02\x0C\r\n");
				if (tok==NULL) continue;
				stx = 0;
				while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
				if (tok[stx]=='\0') continue;
				break;
			}
		}
*/
		if (tok != NULL)
		{
/*
.....using strncpy because we don't want put '\0' 
.....fortran code init all buf to spaces, we want keep it
*/
			if (strlen(tok)-stx>0)
				strncpy(buf[krec][7], &(tok[stx]), strlen(tok)-stx);
		}
		goto getline;
	}
	else if (strncmp(tok, "PASSWORD:",9)==0)
	{
		stx = 9;
		while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
/*		if (tok[stx]=='\0')
		{
/*
.....get Hardware info from following line
*/
/*			while (fgets(line, 132, fstream)!=NULL)
			{
				to_upper(line);
				tok = strtok(line, "|\x02\x0C\r\n");
				if (tok==NULL) continue;
				stx = 0;
				while ((tok[stx]==' ')||(tok[stx]=='\t')) stx++;
				if (tok[stx]=='\0') continue;
				break;
			}
		}
*/
		if (tok != NULL)
		{
/*
.....using strncpy because we don't want put '\0' 
.....fortran code init all buf to spaces, we want keep it
*/
			if (strlen(tok)-stx>0)
				strncpy(buf[krec][8], &(tok[stx]), strlen(tok)-stx);
		}
		krec++;
		goto getline;
	}
	fclose (fstream);
	*bufnum = krec; 
	*err = 0;
}

/***********************************************************************
c
c   SUBROUTINE: closelicfile()
c
c   FUNCTION:  Close license file stream
c
c   INPUT:  None
c			
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void closelicfile()
{
	fclose (license_fstream);
}

/***********************************************************************
c
c   SUBROUTINE: openlicfile(file)
c
c   FUNCTION:  Open license file stream
c
c   INPUT:  file: license file to open
c						
c   OUTPUT: None
c	RETURN: 0: success
c
c***********************************************************************
*/
int openlicfile(file)
char *file;
{
	char msg[256];
	if ((license_fstream = fopen(file, "w"))==NULL)
	{
		sprintf (msg, "Can't open file %s to write!", file);
#ifdef WNT
		Pw_dispmsg(msg, 0);
#else
		lic_mfmsg_box(NULL, "Error!", msg);
#endif
		return -1;
	}
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE: addlicence(record)
c
c   FUNCTION:  Added a license record into license file
c
c   INPUT:  record: license record to add
c						
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void addlicence(record)
char record[10][132];
{
	fprintf(license_fstream, "Company: %s\n", record[0]);
	fprintf(license_fstream, "Hardware: %s\n", record[1]);
	fprintf(license_fstream, "Software: %s\n", record[2]);
	fprintf(license_fstream, "Options: %s\n", record[3]);
	fprintf(license_fstream, "Num of Users: %s\n", record[4]);
	fprintf(license_fstream, "License Term: %s\n", record[5]);
	fprintf(license_fstream, "Version: %s\n", record[6]);
	fprintf(license_fstream, "System ID: %s\n", record[7]);
	fprintf(license_fstream, "Password: %s\n", record[8]);
	fprintf(license_fstream, "\n");
}

/***********************************************************************
c
c   SUBROUTINE: get_batbuf(batfile, flen, buf, bufnum, err)	
c
c   FUNCTION:  Get the batch record buffer from a batch file
c
c   INPUT:  batfile : batch file
c			flen: length of batfile
c			
c			
c   OUTPUT: buf: buffer to store license records
c			bufnum: number of license record 'buf' have
c			err: 0: no errors
c
c***********************************************************************
*/
void get_batbuf(batfile, flen, buf, bufnum, err)
char *batfile;
int *flen;
char buf[10000][10][132];
int *bufnum;
int *err;
{
	FILE * fstream;
	char line[132];
	char *tok, *tok1;
	char msg[256], uptok[256];
	int i, krec;
	batfile[*flen] = '\0';
	if ((fstream = fopen(batfile, "r"))==NULL)
	{
		*err = 1;
		sprintf (msg, "Can't open batch file %s", batfile);
#ifdef WNT
		Pw_dispmsg(msg, 0);
#else
		lic_mfmsg_box(NULL, "Error!", msg);
#endif
		return;
	}
/*
.....reading batch file and store in the buf
*/
	i = 0;
	krec = 0;
getline:
	if (fgets(line, 132, fstream)==NULL)
	{
		if (krec==0)
		{
			*err = 1;
#ifdef WNT
			Pw_dispmsg("No complete record or format can't read!", 0);
#else
			lic_mfmsg_box(NULL, "Error!",
						 "No complete record or format can't read!");
#endif
			fclose (fstream);
			return;
		}
		fclose (fstream);
		*bufnum = krec; 
		*err = 0;
		return;
	}
	tok = strtok(line, "|\x02\x0C\r\n");
	if (tok!=NULL) 		
	{
/*
.....using strncpy because we don't want put '\0' 
.....fortran code init all buf to spaces, we want keep it
*/
		strcpy(uptok, tok);
		to_upper(uptok);
		strncpy(buf[krec][i], uptok, strlen(uptok));
	}
	i++;
	if (i>9) 
	{
		i = 0;
		krec++;
	}
	goto getline;
}

#ifdef WNT
/***********************************************************************
c
c   SUBROUTINE: cydelfile(fnam1, fnam2,err)
c
c   FUNCTION:  copy fnam2 to fnam1 and remove fnam2
c
c   INPUT:  fnam1: file copy to
c			fnam2: file copy from and removed
c			
c			
c   OUTPUT: err: 
c
c***********************************************************************
*/
void cydelfile(fnam1, nc1, fnam2, nc2, err)
char fnam1[80], fnam2[80];
int *err, *nc1, *nc2;
{
	char fname1[80], fname2[80];
	int stat;
	*err = 0;
	strncpy(fname1, fnam1,*nc1);
	fname1[*nc1] = '\0';
	strncpy(fname2, fnam2,*nc2);
	fname2[*nc2] = '\0';
	stat = CopyFile(fname2, fname1, FALSE);
	if (stat==0) *err = -1;
	stat = DeleteFile(fname2);
	if (stat==0) *err = -1;
}
#endif
