/***********************************************************************
**
**  FILE NAME:  d1cmdsem.h
**
**    MODULE NAME AND RELEASE LEVEL
**       d1cmdsem.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:12
***********************************************************************/

#include	"tlangtool.h" 

static semmch(iprog,istrt,len,in,ikf,ikl,
			kf,kl,itrace,its,ltr,irc)
int iprog[],istrt,len,in[],ikf,ikl;
int kf,kl,itrace,ltr,*irc;
ITSTYPE	 its[];
{
extern int itktbl[7][4];
int igo;
*irc=0; igo=iprog[istrt];
switch(igo) {
case 1: uu_unidone(); exit();
break;
case 2: umu_c2_pp();
break;
case 3: printf("hello to you too.\n");
break;
}
}
