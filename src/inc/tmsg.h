/*********************************************************************
**    NAME         :  	tmsg.h
**		CONTAINS:	declaration area for prompt, error, and help subsytem
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tmsg.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:55
*********************************************************************/
#ifndef TMSGH

#define	UTI_SUBSYSTEM	200
#define	UTI_ERROR			201
#define  UTI_PROMPT		202
#define	UTI_NUMBER		203
#define	UTI_NUMID			204
#define	UTI_MSGIDENT		206
#define	UTI_SEVERITY		207
#define	UTI_SYSTEM		208
#define	UTI_INFORMATION	209
#define	UTI_CHOICE		210

#define	UTI_MSGTXSIZE	80

typedef	struct
	{
	 int	erbase,		/* base record of the primary error message */
			erleng,		/* number of record of the primary error message */
			pmtbase,		/* base record of the primary prompt message */
			pmtleng,	  /* number of record of the primary prompt message */
			chobase,		/* base record of the choice message */
			choleng;		/* number of choice message */
	} UTI_HEADER;

typedef  struct
	{
	 int  ernum,		/* error # */
			severity	;
	 /* char msg[UTI_MSGTXSIZE+1];		/* message text        */
	 char *msg;		   /* message text        */
	 int  hmsgnum;		/* help mesage record  */
	} UTI_ERMSG;




#define TMSGH
#endif
