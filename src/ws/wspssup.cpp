/************************************************************************
c
c   FILE NAME: wspssup.cpp
c
c	 CONTAINS: 
c		PostScript Print support file 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c         wspssup.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c         04/29/15 , 15:12:31
c
c**********************************************************************
*/
#include "stdafx.h"
#include "xenv1.h"
#include "plot.h"
#include "wsps.h"

#define MAX_PSBUF	1000
extern "C" Gsps uw_ps;
CDC *PS_pDC;
extern "C" int uu_ttput(int fd, char *buf, int len);
/***********************************************************************
c
c   FUNCTION: utp_ttputps(int fd, char *buf, int len)
c
c         output a plotter line
c
c   INPUT:  fd: file index
c			buf: buffer to be output
c			len: length of the buffer
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" int utp_ttputps(int fd, char *buf, int len)
{
	if (fd>=0)
	{
		return uu_ttput(fd, buf, len);
	}

	char tempBuf[MAX_PSBUF];
	strncpy(tempBuf, buf, len);
	tempBuf[len] = '\0';
	int gPrCode = POSTSCRIPT_PASSTHROUGH;
	char szBuf[MAX_PSBUF+sizeof(short)];
/*
......leave 2 spaces for buffer length
*/
	wsprintf(szBuf, "  %s", tempBuf);
	*((short*)szBuf) = strlen(szBuf)-2;
	int stat = PS_pDC->Escape(POSTSCRIPT_PASSTHROUGH, strlen(szBuf)-2, szBuf,NULL);
	if (stat<=0)
	{
		MessageBox(NULL, "PostScript Wrong command!", "Error", MB_OK);
		return 0;
	}
	return 1;
}
