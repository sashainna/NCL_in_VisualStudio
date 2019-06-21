/***********************************************************************
**
**  FILE NAME:  icalcsem.h
**
**    MODULE NAME AND RELEASE LEVEL
**       icalcsem.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:25
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
case 1: uqi_setend();
break;
case 2: uqi_tbreset();
break;
case 3: uqi_chgnotation (1, its, ltr);
break;
case 4: uqi_chgnotation (2, its, ltr);
break;
case 5: uqi_angtype (1);
break;
case 6: uqi_angtype (2);
break;
case 7: uqi_stblist();
break;
case 8: uqi_eval(3);
break;
case 9: uqi_eval(4);
break;
case 10: uqi_cfaddsym(in,kf,kl);
break;
case 11: uqi_cfuncsym(1,in,kf);
break;
case 12: uqi_cfuncsym(2,in,kf);
break;
case 13: uqi_coeval();
break;
case 14: uqi_veval(1,its,ltr);
break;
case 15: uqi_veval(2,its,ltr);
break;
case 16: uqi_veval(3,its,ltr);
break;
case 17: uqi_caddsym(in,kf);
break;
case 18: uqi_eval(2);
break;
case 19: uqi_eval(2);
break;
case 20: uqi_eval(2);
break;
case 21: uqi_eval(1);
break;
case 22: uqi_its2push(its,ltr);
break;
case 23: uqi_its2push(its,ltr);
break;
case 24: uqi_its2push(its,ltr);
break;
case 25: uqi_its2push(its,ltr);
break;
case 26: uqi_its1push(its,ltr);
break;
case 27: uqi_varval(1,its,ltr);
break;
case 28: uqi_evala1();
break;
case 29: uqi_evala2();
break;
case 30: uqi_cevalfunc(3,its,ltr);
break;
case 31: uqi_its2push(its,ltr);
break;
case 32: uqi_its2push(its,ltr);
break;
case 33: uqi_its2push(its,ltr);
break;
case 34: uqi_its2push(its,ltr);
break;
case 35: uqi_its2push(its,ltr);
break;
case 36: uqi_its2push(its,ltr);
break;
case 37: uqi_its2push(its,ltr);
break;
case 38: uqi_its2push(its,ltr);
break;
case 39: uqi_cordpush(2);
break;
case 40: uqi_cordpush(3);
break;
case 41: uqi_cordpush(1);
break;
case 42: uqi_cordpush(5);
break;
case 43: uqi_cordpush(6);
break;
case 44: uqi_cordpush(4);
break;
case 45: uqi_evala3(1,its,ltr);
break;
case 46: uqi_evala3(2,its,ltr);
break;
case 47: uqi_evala3(3,its,ltr);
break;
case 48: uqi_cevalfunc(1,its,ltr);
break;
case 49: uqi_cevalfunc(2,its,ltr);
break;
case 50: uqi_cevalfunc(2,its,ltr);
break;
}
}
