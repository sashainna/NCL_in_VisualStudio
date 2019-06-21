
/*********************************************************************
**    NAME         :  tsemcmp.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tsemcmp.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:58
*********************************************************************/

#ifndef TSEMCMPH


/* file semcmp.c -- generates a semmch.c and semmc1.c program */
#include "ustdio.h"
#include "tlangtool.h"

static FILE *fd,*fd2;
static int num=0;
extern char semroot[];




/*********************************************************************
**    I_FUNCTION :  uti_semcmp()
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/


uti_semcmp(in,kf,kl,ip,is,len,irc)
int  kf,kl,ip[],is,*len,*irc;
ITSTYPE	in[];

{
	int i;
	char	semnm[40];

	/* header for semantic machine */
	static char header[11][70]=
	{
		"#include	\"tlangtool.h\" \n",
		"static semmch(iprog,istrt,len,in,ikf,ikl,",
		"			kf,kl,itrace,its,ltr,irc)",
		"int iprog[],istrt,len,in[],ikf,ikl;",
		"int kf,kl,itrace,ltr,*irc;",
		"ITSTYPE	 its[];",
		"{",
		"extern int itktbl[7][4];",
		"int igo;",
		"*irc=0; igo=iprog[istrt];",
		"switch(igo) {"
	};

	/* header for secondary semantic machine */
	static char headr2[15][70]=
	{
		"semmc1(iprog,istrt,len,in,ikf,ikl,",
		"			kf,kl,itrace,its,ltr,irc)",
		"int iprog[],istrt,len,in[],ikf,ikl;",
		"int kf,kl,itrace,ltr,*irc);",
		"ITSTYPE	 its[];",
		"{",
		"extern int semcom[3][6];",
		"extern int var[46];",
		"int igo;",
		"*irc=0; igo=iprog[istrt-1];",
		"if (igo<=0) {",
		"	pshsem(istrt,istrt+len-1);",
		"	return;",
		"}",
		"switch(igo) {"
		};


	if (num==0)				/* first time semcmp called */
	{
		/* output headers on file semmch.c  and semmc1.c */
		sprintf(semnm, "%ssem.h", semroot);
		fd=fopen(semnm,"w");
		/* fd2=fopen("semmc1.c","w"); */
		for (i=0; i<11; i++) fprintf(fd,"%s\n",&header[i][0]);
		/* for (i=0; i<15; i++) fprintf(fd2,"%s\n",&headr2[i][0]); */
	}
	num=num+1;
	*len=1;
	ip[is]=num;
	*irc=1;
	fprintf(fd,"case %d: ",num);
	/* fprintf(fd2,"case %d: ",num); */
	/* copy semantic program  onto output files */
	for (i= kf; i<= kl; i++) {
		/* if (in[i]==186) {			/* new line char */
		/*	fprintf(fd,"\n"); /* fprintf(fd2,"\n"); */
		/* } */
		/* else {					/* output this char to files */
			fprintf(fd,"%c",in[i].lint);
			/* fprintf(fd2,"%c",in[i]); */
		/* } */
	}
	/* write last line of this case */
	fprintf(fd,"\nbreak;\n");
	/* fprintf(fd2,"\nbreak;\n"); */
}



/*********************************************************************
**    I_FUNCTION :  semmch()
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/


semmch(iprog,istrt,len,in,ikf,ikl,kf,kl,itrace,its,ltr,irc)
int iprog[],istrt,len,in[],ikf,ikl,kf,kl,itrace;
int ltr,*irc;
ITSTYPE	its[];

{
	printf("semmch. shouldn't be called.\n");
	*irc=0;
}




/*********************************************************************
**    I_FUNCTION :  uti_semfin()
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/


uti_semfin()
{
	if (num>0) {
		fprintf(fd,"}\n}\n");
		close(fd);
	}
}



#define TSEMCMPH
#endif
