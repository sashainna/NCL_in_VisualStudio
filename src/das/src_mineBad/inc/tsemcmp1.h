
/*********************************************************************
**    NAME         :  tsemcmp1.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tsemcmp1.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:58
*********************************************************************/

#ifndef TSEMCMP1H


/* semcmp1.h -- semantic compiler and machine for LANGPAK */

#include		"tlangtool.h"


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


uti_semcmp(in,kf,kl,iprog,istrt,len,irc)
int  kf,kl,iprog[],istrt,*len,*irc;
ITSTYPE	in[];

{
/*     in(kf) thru in(kl) contain semantic input string.
      copy it to iprog(istrt). set len to its length.
      return irc=1 (successful translation) */
		int i;
		*len=kl-kf+1;
		for(i=1;i<= *len; i++) {
 	   	iprog[istrt-1+i]=in[kf-1+i].lint;
		}
		*irc=1;
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
int iprog[],istrt,len,in[],ikf,ikl,kf,kl,itrace,ltr,*irc;
ITSTYPE	its[];

{
/*     semantic machine to just print out semantic specification
c     contained in iprog(istrt) thru iprog(istrt+len-1)
c     return irc=0 (success) */
		uti_twrite(iprog,istrt,len);
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
}


/**  dumysemch.c - dummy semmch routine used to satisfy langsub.c */

/**   for calculator      **/
calcsemmch(iprog,istrt,len,in,ikf,ikl,
			kf,kl,itrace,its,ltr,irc)
int iprog[],istrt,len,in[],ikf,ikl;
int kf,kl,itrace,ltr,*irc;
ITSTYPE	its[];

{ }

/**   for das      **/
dassemmch(iprog,istrt,len,in,ikf,ikl,
			kf,kl,itrace,its,ltr,irc)
int iprog[],istrt,len,in[],ikf,ikl;
int kf,kl,itrace,ltr,*irc;
ITSTYPE	its[];

{ }


#define TSEMCMP1H
#endif
