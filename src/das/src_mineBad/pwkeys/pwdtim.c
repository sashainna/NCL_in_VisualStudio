/*
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pwdtim.c , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:12
c
*/
#ifdef IBM
#include <time.h>
#include <sys/time.h>

idate(imon,iday,iyear)
int *imon,*iday,*iyear;
{
	time_t tp;
	struct tm *tmv;
/*
.....Get the current Unix time
*/
	ftime(&tp);
/*
.....Convert to local date and time
*/
	tmv = localtime(&tp);
/*
.....Return date
*/
	*imon = tmv->tm_mon + 1;
	*iday = tmv->tm_mday;
	*iyear = tmv->tm_year;
}

itime(ihr,imin,isec)
int *ihr,*imin,*isec;
{
	time_t tp;
	struct tm *tmv;
/*
.....Get the current Unix time
*/
	ftime(&tp);
/*
.....Convert to local date and time
*/
	tmv = localtime(&tp);
/*
.....Return date
*/
	*ihr = tmv->tm_hour;
	*isec = tmv->tm_min;
	*imin = tmv->tm_sec;
}
#endif

//#define __cplusplus
//
//#ifdef __cplusplus
//extern "C" int getenvc(char* key, char *value, int* len);
//#endif

#ifdef WNT
#include <stdlib.h>
#include <string.h>

//extern "C" int getenvc(char* key, char *value, int* len);

/***********************************************************************
c
c   SUBROUTINE:  getenvc(char* key, char *value, int len)
c
c   FUNCTION:  This function get the enviornment value
c
c   INPUT:  key    environment key
c
c   OUTPUT: value: environment value
c			len  : environment value's length
c
c***********************************************************************
*/
int getenvc(char* key, char *value, 
	unsigned short int* len)
{
	int nc;
	char key1[1024], *value1;
	/*strncpy(key1, key, *len);*/
	strncpy_s(key1, 1024, key, *len);
	key1[*len] = '\0'; 
	//key1[12] = '\0'; 
	value1 = getenv (key1);
	//value1 = _dupenv_s (key1);
	if (value1!=NULL)
	{
		if (value1[0] == '\"') strcpy(value,&value1[1]);
		else strcpy(value, value1);
		nc = strlen(value);
		if (value[nc-1] == '\"')
		{
			value[nc-1] = '\0';
			nc = nc - 1;
		}
		return(nc);
	}
	else
	{
		value[0]='\0';
		return 0;
	}
}
#endif
