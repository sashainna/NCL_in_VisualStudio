/*********************************************************************
**    NAME         :PWBrowser.h
**		   File browser routines.
** 
**    CONTAINS     : 
**  
**    COPYRIGHT 2009 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			PWBrowser.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**			09/11/13 , 12:58:27
*********************************************************************/
#ifndef PWBROWSE_H
#define PWBROWSE_H


#define EXT extern

EXT int browsefile(char *Filter, char *FileName, int flag=TRUE, int definx=0,
	int descfl=FALSE);

#endif
