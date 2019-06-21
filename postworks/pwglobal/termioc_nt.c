/***********************************************************************
c
c   FILE NAME: termioc_nt.c
C                 This file only for WinNT
c   CONTAINS:
c              displn(msg, len)
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        termioc_nt.c , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:19 
c
***********************************************************************/
#ifdef WNT

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
/***********************************************************************
c
c   SUBROUTINE:  displn(msg, len)
c
c   FUNCTION:  This routines display a line in screen.
c
c   INPUT:  msg: message displayed in a line.
c
c   OUTPUT: none
c
***********************************************************************/
displn(msg, len)
char* msg;
int *len;
{
	char msgstr[200];
	
	strncpy(msgstr, msg, *len);
	msgstr[*len] = '\0';
	printf("%s", msgstr);
	return 1;
}

void getchrc(ptchr)
int *ptchr;
{
	*ptchr = getchar();
}

void getlnc(buf, nc)
char *buf;
int *nc;
{
	gets( buf );
	*nc = strlen(buf);
/*
.....this function is used by fortran, so use
.....space instead of '\0' because later we use 
.....Fortran routine to calculate strlen, it will
.....not recognize end marker
*/
	buf[*nc] = ' ';
}


#endif
