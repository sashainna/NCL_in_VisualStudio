/***********************************************************************
**
**  FILE NAME:  dassem.h  
**
**    MODULE NAME AND RELEASE LEVEL
**       dassem.h , 25.1
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
case 1: udi_dgnum();
break;
case 2: udi_dgcord(3);
break;
case 3: udi_dgcord(1);
break;
case 4: udi_ckincfm(its,ltr);
break;
case 5: udi_dgincremnt();
break;
case 6: udi_dits1push(its,ltr,1);
break;
case 7: udi_setpflag();
break;
case 8: udi_chkpostf();
break;
case 9: udi_dgcord(2);
break;
case 10: udi_ckcylindr();
break;
case 11: udi_ckspherical();
break;
case 12: udi_chkz();
break;
case 13: udi_ckcart();
break;
case 14: udi_dstnum();
break;
case 15: udi_dits1push(its,ltr,2);
break;
case 16: udi_dits2push(its,ltr);
break;
case 17: udi_dunitrack();
break;
case 18: udi_dits1push(its,ltr,2); udi_scinum();
break;
case 19: udi_dits3push(its,ltr);
break;
}
}
