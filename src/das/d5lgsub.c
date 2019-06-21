
/*********************************************************************
**    NAME         :  tlangsub.c
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d5lgsub.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:11
**********************************************************************/

/*     file langsub.c -- subroutines for langpak */
/*		modified to use ascii char set on 11/14/84 */

#include "ustdio.h"
#include "uctype.h"
#include "umath.h"
#include "usysdef.h"
#include	"tlangtool.h"
#define MAX 1024

/* declare "common" variables */
int ix;
int irepfg,ialtfg,iandfg,isynpt,isynln;
int lstsuc,lastlp,isnvec[2001],lx[40001];
int itktbl[7][4]={0,0,0,0,0,0,0,
						0,0,0,0,0,0,0,
						0,0,0,0,0,0,0,
						0,0,0,0,0,0,0};
int nomore=1;				/* default is no incremental input */



/*********************************************************************
**    I_FUNCTION :  uti_lxgen
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

		/* create an empty lexicon of size lxsiz in lx */
		uti_lxgen(lx,lxsiz)
      int lx[],		/* array to hold empty lexicon */
			lxsiz;		/* size of lx */
		{
		int i;
      lx[1]=1;
      lx[2]= lxsiz;
      lx[3]=111;
      lx[4]=1;
      for (i=5;i<=110; i++) {
      	lx[i]=0;
		}
		}



/*********************************************************************
**    I_FUNCTION :  uti_lexanl
**			 If LEXANL is successful in matching the desired terminal word set
**			member, IRC is set to 1, IKF is set to the first symbol beyond the
**			matched terminal word set member, the translation element (if any)
**			is placed into ITS starting at ITS[LTR+1] and LTR is incremented
**			by the length of the translation element. IRC is set to 0 if LEXANL
**			is uncuccessful.
**				In addition, if LEXANL is successful, it modifies the entries
**			in the array ITKTBL. LEXANL sets ITKTBL[IOP1-10,1] to the first
**			symbol position and ITKTBL[IOP1-10][2] to the last symbol position
**			in IN of the matched terminal word set member. ITKTBL[IOP1-10][3] is
**			set to the index into ITS of the beginning of the translation element
**			of the matched terminal word set member. If the matched terminal word
**			set member is either a literal or a V with a null translation,
**			ITKTBL[IOP1-10][3] is set to zero. ITKTBL[6][1] is set to the same
**			values as ITKTBL[IP1-10][I], I=1,2,3. ITKTBL is not altered if 
**			meta-language statements are being parsed. 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
			
		uti_lexanl(in,ikf,ikl,lx,iop1,iop2,its,ltr,irc,vmatch)
		int in[],		/* contains symbol sequence */
			*ikf,			/* in[ikf]=first symbol of the unmatched portion of IN */
			*ikl,			/* in[ikl]= the last unmatched symbol */
			lx[],			/* contains lexicon */
			iop1,iop2,	/* specify what terminal word member is to be matched */
			*irc;			/* return code. 1=success */
		ITSTYPE its[];	/* its contains the current translation sequence of
								length ltr */
		int	*ltr;
		int (*vmatch)();

		{
      /* extern int itktbl[7][4]; */
		UU_REAL  xval;
		int i,j,ij;
		int ivalue,kf,kl,ntc,ix1,ix2,ix3,lltr,ivrc,numbs,ifnb;
		int nattr,ix,itkf,len,inrc,ival;
		int lenn;
		int idx;
		char tmp[MAX];
#ifdef CHECK
	if ((*ikf<1)||(*ikf>10000)||(*ikl<1)||(*ikl>10000)
			||(*ltr<1)||(*ltr>10000)) {
			printf("uti_lexanl ERROR. ikf=%d, ikl=%d, ltr=%d\n",
			*ikf,*ikl,*ltr);
			*irc=0; return;
	}
#endif
#ifdef debug
	/*	printf("uti_lexanl(in,%d,%d,lx,%d,%d,its,%d,%d)\n",
			*ikf,*ikl,iop1,iop2,*ltr,*irc);	*/
		for(idx=0;idx<(*ikl-*ikf+1);idx++)
		{
			tmp[idx]=(char)in[*ikf+idx];
		}
		tmp[idx]='\0';
		printf("uti_lexanl:in=%s\n",tmp);
		printf("uti_lexanl:ikf=%d,ikl=%d\n",*ikf,*ikl);
		printf("iop1=%d,iop2=%d\n",iop1,iop2);
#endif
		/* xval=(UU_REAL  *)(&ivalue);		/* equivalence (xval,ivalue) */
		/****** following lines added for incremental reading of input *****/
		if ((*ikf> *ikl)&&(nomore==0)) {
			uti_tread(in,*ikl+1,&lenn);
			if (in[*ikl+1]=='$') nomore=1;	/* beginnign $ ends further input*/
			else *ikl= *ikl+lenn;
		}
		/******* end of stuff added for incremental reading of input *****/
      if( *ikf> *ikl) goto L21;
      kf= *ikf;
      kl= *ikl;
      ntc= iop1-10;
L1:   if(in[kf]!=' ') { 			/* goto(3,3,4,2,16,22),ntc; */
			switch(ntc) {
			case 1: case 2: goto L3;
			case 3: goto L4;
			case 4: goto L2;
			case 5: goto L16;
			case 6: goto L22;
		}
		}
      kf=kf+1;
      if((kl-kf)<0) goto L21;
		goto L1;
L2:     i=kf;
      lltr= *ltr;
      (*vmatch)(in,&kf,&kl,its,ltr,&ivrc);
      if(ivrc==0) *ltr=lltr;
      if(ivrc==0) goto L21;
      ix1=i;
      ix2=kf-1;
      ix3=lltr+1;
      if(lltr== *ltr) ix3=0;
      goto L20;
L3:     i=kf;
      uti_number(in,&kf,kl,ntc,&ival,&xval,&inrc);
      if(inrc==0) goto L21;
      ix1=i;
      ix2=kf-1;
      *ltr= *ltr+1;
      ix3= *ltr;
      its[*ltr].lint=ntc;
      *ltr= *ltr+1;
      if(ntc==1) its[*ltr].lint = ival;
		if(ntc==2) its[*ltr].lreal = xval; /* real number */
      goto L20;
L4:   ifnb=kf;
      kf=kf+1;
      i=iop2%1000;				/* i= iop2-(iop2/1000)*1000 */
      numbs= iop2/1000;
      if(i!=0) goto L6;
      kf=kl+1;
      goto L12;
L5:   if(i==0) goto L21;
L6:   j=i%10;						/* j=i-(i/10)*10 */
      i=i/10;
		switch (j) { 				/* goto(7,9,10,21,21,30), j; */
		case 1: goto L7;
		case 2: goto L9;
		case 3: goto L10;
		case 4: case 5: goto L21;
		case 6: goto L30;
		}
L7:   if(kf>kl) goto L11;
      ix=uti_lxold(lx,in,kf,kl,numbs);
		if (ix!=0) goto L12;				/* if(ix) 12,8,12; */
L8:   kf=kf+1;
      goto L7;
L9:   if(kf>kl) goto L11;
      itkf=kf;
      lltr= *ltr;
      (*vmatch)(in,&itkf,&kl,its,&lltr,&ivrc);
      if(ivrc==1) goto L12;
      kf=kf+1;
      goto L9;
L10:    if(kf>kl) goto L11;
      itkf=kf;
      uti_number(in,&itkf,kl,2,&ival,&xval,&inrc);
      if(inrc==1) goto L12;
      kf=kf+1;
      goto L10;
L11:    kf=ifnb+1;
      goto L5;
L12:    j=kf-1;
L13:    if(in[j]!=' ') goto L14;
      j=j-1;
      goto L13;
L14:    *ltr= *ltr+1;
      ix1=ifnb;
      ix2=j;
      ix3= *ltr;
      its[*ltr].lint=3;
      *ltr= *ltr+1;
      len=j-ifnb+1;
      its[*ltr].lint=len;
      ifnb=ifnb-1;
      for(i=1;i<=len;i++) {
      	ifnb=ifnb+1;
      	*ltr= *ltr+1;
    		its[*ltr].lint=in[ifnb];
		}
      goto L20;
L16:    len=lx[iop2];
L17:    if((kl-kf+1)<len) goto L21;
      ix= iop2;
      ij=kf-1;
      for(i=1;i<=len;i++) {		/* do 18 */
      ix=ix+1;
      ij=ij+1;
      if(lx[ix]!=in[ij]) goto L21;
		}
		ix1=kf;
      kf=kf+len;
      ix2=kf-1;
      nattr=lx[ix+1];
      ix3=0;
      if(nattr==0) goto L20;
      *ltr= *ltr+1;
      ix3= *ltr;
      its[*ltr].lint=5;
      nattr=nattr+1;
      for(i=1;i<=nattr;i++) {			/* do 19 */
      	*ltr= *ltr+1;
      	ix=ix+1;
	    	its[*ltr].lint=lx[ix];
		}
L20:  *ikf=kf;
      *irc=1;
      if(lx[1]!=1) goto rtn;
      itktbl[ntc][1]=ix1;
      itktbl[ntc][2]=ix2;
      itktbl[ntc][3]=ix3;
      itktbl[6][1]=ix1;
      itktbl[6][2]=ix2;
      itktbl[6][3]=ix3;
      goto rtn;
L21:    *irc=0;
      goto rtn;
L22:  if(kf== *ikf)goto L21;
      ix1=0;
      ix2=0;
      ix3=0;
      goto L20;
L30:    if(kf>kl)goto L11;
      if(in[kf]==' ')goto L12;
      kf=kf+1;
      goto L30;
rtn:
#ifdef debug
	printf("uti_lexanl returns irc=%d, ikf=%d, ikl=%d, ltr=%d\n",
			*irc,*ikf,*ikl,*ltr);
#endif
		return;
}
     



/*********************************************************************
**    I_FUNCTION :  uti_lxnew()
**	   if ITYP==1 the symbol sequence in IN is entered into the lexicon LX as
**		a language specification type and IAPTR is set to the index in LX where
**		the entry begins. If ITYP==2 the symbol sequence in IN 
**		is entered into the lexicon as a string break literal whose string 
**		number is ISNO. 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uti_lxnew(lx,its,kf,kl,ityp,isno,iaptr)
int lx[],		/* contains a lexicon */
    kf,kl,		/* in[kf]-in[kl] contain symbol sequence */
ityp,				/* 1 or 2 */
isno,*iaptr;
ITSTYPE  its[]; /* its[kf]-its[kl] contain symbol sequence */

{
		int itmp[MAX],i,l,keyp,iasp,nkf;
		char tmp[MAX];
		int ix;

#ifdef debug
		printf("uti_lxnew:kf=%d; kl=%d; ityp=%d; isno=%d\n",kf,kl,ityp,isno);
		for(ix=0;ix<(kl-kf+1);ix++)
			tmp[ix]=(char)its[kf+ix].lint;
		tmp[(kl-kf+1)]='\0';
		printf("uti_lxnew:its=%s\n",tmp);
#endif
		l=kl-kf+1;
      keyp=kf-ityp+2;
      tmp[0]=its[keyp].lint;
		cin_intern(itmp,1,tmp,0);
      keyp=itmp[0]-53*(itmp[0]/53+1-ityp)+5;
#ifdef debug
		printf("uti_lxnew: itmp=%d; keyp=%d; lx[keyp]=%d\n",itmp[0],keyp,lx[keyp]);
#endif
      iasp=lx[3];
      if (lx[keyp]>0) goto L2;		/* if(lx[keyp]) 1,1,2 */
L1:   lx[keyp]=iasp;
      lx[iasp]=0;
      goto L3;
L2:   lx[iasp]=lx[keyp];
      lx[keyp]=iasp;
L3:   switch (ityp) {					/* goto(4,7),ityp */
		case 1: goto L4;
		case 2: goto L7;
		}
L4:   iasp=iasp+1;
      lx[iasp]=l;
      *iaptr=iasp;
      nkf=kf-1;
      for(i=1;i<=l;i++) {				/* do 5 */
      iasp=iasp+1;
      	nkf=nkf+1;
     		lx[iasp]=its[nkf].lint;
		}
		if (ityp==2) goto L8;			/* goto(6,8),ityp; */
L6:   lx[3]=iasp+3;
      lx[iasp+1]=0;
      lx[iasp+2]=0;
      return;
L7:   iasp=iasp+1;
      lx[iasp]=isno;
      goto L4;
L8:   lx[3]=iasp+1;
}



/*********************************************************************
**    I_FUNCTION :  uti_lxold()
**   if ityp==1 the lexicon in LX is examined to see if it contains an
**	entry for the language specification type whose name is contained
**	in IN. If so, LXOLD returns as its value an index into LX of the 
**	second element of the entry. If not, the value returned is zero. 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uti_lxold(lx,in,kf,kl,ityp)
int lx[],		/* contains lexicon */
in[],kf,kl,	/* in[kf]-in[kl] contains symbol sequence */
ityp;
{
	int i,j,l,keyp,key,nkf,ireturn;
		char tmp[MAX];
		int itmp[MAX];
#ifdef debug
		printf("uti_lxold:kf=%d; kl=%d; ityp=%d\n",kf,kl,ityp);
		for(ix=0;ix<(kl-kf+1);ix++)
			tmp[ix]=(char)in[kf+ix];
		tmp[(kl-kf+1)]='\0';
		printf("uti_lxold:in=%s\n",tmp);
#endif
	l= kl-kf+1;
      keyp=kf;
      if(ityp==1) keyp=keyp+1;
      tmp[0]=in[keyp];
		cin_intern(itmp,1,tmp,0);
#ifdef debug
		printf("uti_lxold:keychar=%d\n",itmp[0]);
#endif
      keyp=itmp[0]-53*(itmp[0]/53)+5;
      if(ityp!=1) keyp=keyp+53;
#ifdef debug
		printf("uti_lxold:lexicon ptr=%d\n",keyp);
#endif
      ireturn=0;
      key=lx[keyp];
      if(ityp!=1) goto L5;
L1:     if(key==0) return(ireturn);
      j=key+1;
      if(lx[j]!=l) goto L4;
L2:     nkf=kf-1;
      for(i=1;i<=l;i++) {				/* do 3 */
      	j=j+1;
      	nkf=nkf+1;
      	if(lx[j]!=in[nkf]) goto L4;
		}
		ireturn=key+1;
      if(ityp!=1) ireturn=1;
      return(ireturn);
L4:     key=lx[key];
      if(ityp==1) goto L1;
L5:     if(key==0) return(ireturn);
      j=key+1;
      if(lx[j]!=ityp) goto L4;
      j=j+1;
      if(lx[j]>kl-kf+1) goto L4;
      l=lx[j];
      goto L2;
} 



/*********************************************************************
**    I_FUNCTION :  uti_metas()
**   METAS executes the IPROG'th meta-semantic procedure setting IRC to
**	  one if it is successful and zero otherwise. Returning a zero causes
**		translation of the current language specification type definition
**		to be aborted. 
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

uti_metas(iprog,in,ikf,ikl,kf,its,ltr,irc,semcmp)
int iprog,			/* integer between 1 and 14 */
in[],ikf,ikl,		/* in[ikf]-in[ikl] contain a meta-language statement */
kf,					/* in[kf] is 1st unmatched symbol of meta-lang stmt */
*irc;					/* return code. 1=success. */
ITSTYPE its[];		/* ITS contains translation sequence of length LTR */
int	*ltr;
int (*semcmp)();

{
      /*extern int irepfg,ialtfg,iandfg,isynpt,isynln; */
      /*extern int lstsuc,lastlp,isnvec[2001],lx[40001]; */
		static int two=2, one=1;
		int isno,iop1,itmp,iop2,i,len,lflag,k,isbrk,idum,lname;
		int isrc,ix,jj,nup,low,kk,j;
		int iix, jk, intits[80];

#ifdef debug
	printf("uti_metas(%d,in,%d,%d,%d,its,%d,%d)\n",
			iprog,ikf,ikl,kf,*ltr,*irc);
#endif
      *irc=1;
      iop2=0;
		switch(iprog) { 	/*goto(1,44,28,44,2,10,11,12,22,16,26,17,3,24),iprog; */
		case 1: goto L1;
		case 2: goto L44;
		case 3: goto L28;
		case 4: goto L44;
		case 5: goto L2;
		case 6: goto L10;
		case 7: goto L11;
		case 8: goto L12;
		case 9: goto L22;
		case 10: goto L16;
		case 11: goto L26;
		case 12: goto L17;
		case 13: goto L3;
		case 14: goto L24;
		}
L1:   isynln=0;
      irepfg=0;
      lastlp=0;
      ialtfg=0;
      iandfg=2;
      goto L31;
L2:   len=its[2].lint+2;
      its[2].lint='<';
      its[len+1].lint='>';
		for (jk=2; jk<len+2; jk++)
			intits[jk] = its[jk].lint;
      isynpt=uti_lxold(lx,intits,2,len+1,1);
      itmp=isynpt;
      if(isynpt==0) uti_lxnew(lx,its,2,len+1,1,isno,&itmp);
      isynpt=itmp+len+1;
      goto L31;
L3:   iop1=10+its[3].lint;
      if((its[3].lint!=3)||(*ltr==3)) goto L27;
      lflag=0;
      isno=0;
      k=4;
      i=0;
L4:   isbrk=its[k].lint;
      if(isbrk==3) isbrk=1;
      if(isbrk!=1) isbrk=its[k+2].lint;
      if((isbrk==1)&&(lflag==1)) goto L5;
      i=i+1;
      if(isbrk!=1) goto L8;
      lx[4]=lx[4]+1;
      isno=lx[4];
L5:   uti_lxnew(lx,its,k+2,its[k+1].lint+k+1,2,isno,&idum);
      k=k+2+its[k+1].lint;
      if(lflag==1) goto L7;
      lflag=1;
L6:   iop2=iop2+(pow((UU_REAL) 10.,i-1.)*isbrk);
L7:   if((*ltr-k)<0) goto L9; else goto L4;  /* if(ltr-k) 9,4,4; */
L8:   k=k+3;
      goto L6;
L9:   iop2=1000*isno+iop2;
      goto L27;
L10:  lname=its[2].lint+2;
      its[2].lint='<';
      its[lname+1].lint='>';
		for (jk=2; jk<lname+2; jk++)
			intits[jk] = its[jk].lint;
      iop2=uti_lxold(lx,intits,2,lname+1,1);
      if(iop2==0) uti_lxnew(lx,its,2,lname+1,1,isno,&iop2);
      iop1=3;
      goto L27;
L11:  
(*semcmp)(its,3,its[2].lint+2,isnvec,isynln+5,&iop2,&isrc);
      if(isrc==0) *irc=0;
      iop1=7;
      if (isrc==0) goto L31;		/*if(isrc) 27,31,27; */
		goto L27;
L12:  lname=its[2].lint;
      jj=lx[3];
      iop2=jj;
      lx[jj]=lname;
      k=2;
      for(i=1;i<=lname;i++) {			/* do 13 */
      	jj=jj+1;
      	k=k+1;
L13:		lx[jj]=its[k].lint;
		}
      jj=jj+1;
      ix=jj;
      k=4+lname;
      if(k> *ltr) goto L15;
      for(i=k;i<= *ltr;i=i+2) {			/* do 14 */
      	jj=jj+1;
L14:		lx[jj]=its[i].lint;
		}
L15:  lx[3]=jj+1;
      lx[ix]=jj-ix;
      iop1=15;
      goto L27;
L16:  if(irepfg!=0) goto L32;
      irepfg=isynln+2;
      lastlp=irepfg+2;
      iop1=5;
      goto L27;
L17:  if(irepfg==0) goto L36;
      if(irepfg>=isynln-2) goto L40;
      if(ialtfg==lstsuc+1) goto L38;
      if(iandfg==lstsuc-1) goto L42;
      nup=1;
      low=1;
      if(*ltr==0) goto L18;
      low=its[2].lint;
      nup=its[4].lint;
      if((low<0)||(nup<=0)||(nup<low)) goto L34;
L18:  isnvec[irepfg]=nup;
      iop1=6;
      iop2=low;
      if(iandfg==2) goto L19;
      isnvec[isynln+1]=8;
      isnvec[isynln+2]=0;
      lstsuc=isynln+3;
      isnvec[lstsuc]=1;
      isynln=isynln+4;
      isnvec[isynln]=0;
      iandfg=2;
L19:  ialtfg=0;
      kk=irepfg-1;
L20:  kk=kk+4;
      if(kk>isynln) goto L21;
      if(isnvec[kk+3]==0) isnvec[kk+3]=isynln+1;
      if(isnvec[kk]==7) kk=kk+isnvec[kk+1];
      goto L20;
L21:  isnvec[lstsuc]=1;
      isnvec[lastlp]=isynln+5;
      irepfg=0;
      goto L27;
L22:  if(irepfg==0) goto L36;
      if(ialtfg==0) ialtfg=irepfg+2;
      if(ialtfg==lstsuc+1) goto L38;
      if(iandfg==lstsuc-1) goto L42;
      if(iandfg==2) goto L23;
      isnvec[isynln+1]=8;
      isnvec[isynln+2]=0;
      lstsuc=isynln+3;
      isnvec[lstsuc]=isynln+5;
      isynln=isynln+4;
      isnvec[isynln]=0;
      iandfg=2;
L23:    isnvec[ialtfg+4]=isynln+1;
      isnvec[lstsuc]=1;
      ialtfg=isynln;
      goto L31;
L24:    if(irepfg==0) goto L36;
      if((irepfg==lstsuc-1)||(iandfg==lstsuc-1)) goto L42;
      if(ialtfg==lstsuc+1) goto L38;
      iop1=4;
      if(iandfg==2) goto L25;
      isnvec[isynln+1]=8;
      isnvec[isynln+2]=0;
      lstsuc=isynln+3;
      isnvec[lstsuc]=isynln+5;
      isynln=isynln+4;
      isnvec[isynln]=0;
L25:    iandfg=isynln+2;
      goto L27;
L26:    k=its[3].lint;
      if(k>=4) k=k-10;
      iop1=7+k;
L27:    isnvec[isynln+1]=iop1;
      isnvec[isynln+2]=iop2;
      lstsuc=isynln+3;
      isnvec[lstsuc]=isynln+5;
      isynln=isynln+4;
      isnvec[isynln]=0;
      if(iop1!=7) goto L31;
      isnvec[isynln-1]=isnvec[isynln-1]+iop2;
      isynln=isynln+iop2;
      goto L31;
L28:    if(isynln==0) goto L49;
      if(irepfg>0) goto L32;
      if(irc==0) goto L31;
      isnvec[lstsuc]=1;
      if(isnvec[lstsuc-2]==6) isnvec[lastlp]=1;
      lx[isynpt]=lx[3];
      j=lx[3];
      lx[j]=isynln;
      for(i=1;i<=isynln;i++) {			/* do 29 */
      	j=j+1;
L29:		lx[j]=isnvec[i];
		}
      j=j+1;
      lx[isynpt+1]=j;
      lx[j]=ikl-ikf+1;
      for(i= ikf;i<= ikl;i++) {				/* do 30 */
      	j=j+1;
L30:		lx[j]=in[i];
		}
      lx[3]=j+1;
L31:  *ltr=0;
      goto rtn;
L32:  ud_printmsg(" ) missing\n");
		goto L51;
L34:  ud_printmsg("repeating specification limits illegally constructed\n");
      goto L51;
L36:  ud_printmsg(" ( missing\n");
      goto L51;
L38:  ud_printmsg("alternation illegally constructed\n");
      goto L51;
L40:  ud_printmsg("  null repeating specification\n");
      goto L51;
L42:  ud_printmsg(" conjunction illegally constructed\n");
      goto L51;
L44:  ud_printmsg(" the symbol sequence beginning with\n");
      for(i=kf;i<= ikl;i++) {			/* do 46 */
      	if(in[i]!=' ') goto L47;
		}
L47:  kf=i;
      ix= ikl-kf+1;
      if(ix>10) ix=10;
      uti_twrite(in,kf,ix);
L48:  ud_printmsg(" is not a legal element\n");
      goto L51;
L49:  ud_printmsg("null l.s.t.d.\n");
L51:  *irc=0;
      goto L31;
rtn:	
#ifdef debug
	printf("uti_metas returned irc=%d, ltr=%d\n",*irc,*ltr);
#endif
	return;
}



/*********************************************************************
**    I_FUNCTION :  uti_number()
**   if ITYP==1 NUMBER attempts to match an integer starting at IN[KF]
**		and ending at or before IN[KL]. If successful, IRC is set to 1,
**		IVAL is set to the value of the matched number, and KF is set to
**		indicate the first symbol past the integer. Otherwise IRC is set to 0.
**		If ITYP==2 an analogous operation is performed to match a real number
**		whose value is placed in XVAL 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/


uti_number(in,kf,kl,ityp,ival,xval,irc)
int in[],*kf,kl,		/* in[kf]-in[kl] contains symbol sequence */
ityp,					/* flag */
*ival,					/* returned value of integer */
*irc;						/* return code. 1=success */
UU_REAL  *xval;			/* returned value of real number */
	{
	int isign,ixval,igotn,igotp,igots,ikf,ikl,i,ic;
	UU_REAL  frac,p10,xic,rxval;

   *irc=1;
   isign=1;
   ixval=0;
   igotn=0;
   igotp=0;
   frac=0.0;
   igots=0;
   p10=1.0;
   ikf= *kf-1;
L1:ikf=ikf+1;
   if(ikf>kl) goto L5;
   ic=in[ikf]-'0';
   if((ic<0)||(ic>9)) goto L3;
   igotn=1;
   if(igotp==1) goto L2;
   ixval=ixval*10+ic;
   goto L1;
L2:xic=ic;
   p10=.1*p10;
   frac=frac+p10*xic;
   goto L1;
L3:if((in[ikf]=='+')||(in[ikf]=='-')) goto L4;
   if(in[ikf]!='.') goto L5;
   if((ityp==1)||(igotp==1)) goto L5;
   igotp=1;
   goto L1;
L4:if(igotn==1) goto L6;
   if((igots==1)||(igotp==1)) goto L5;
   igots=1;
   if(in[ikf]=='+') goto L1;
   isign= -1;
   goto L1;
L5:if(igotn!=0) goto L6;
   *irc=0;
   return;
L6: *kf=ikf;
   if(ityp==2) goto L7;
   *ival=isign*ixval;
   return;
L7:   xic=(UU_REAL)isign;
	 *xval = (UU_REAL)ixval;
   *xval=xic*(*xval+frac);
   return;
} 



/*********************************************************************
**   I_FUNCTION :  uti_parser()
**   PARSER attempts to parse the symbol sequence in IN according to the
**		language specification type contained in ISYN. If ITRACE is zero,
**		the parse is not traced. If ITRACE is 1, the complete parse is traced.
**		when the parse is resumed.
**		Upon a successful parse, the translation sequence is contained
**		in ITS and LTR is set to its length. 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uti_parser(isyn,lname,in,ikf,ikl,lx,istk,itrace,irkf,its,ltr,irc,semmch,vmatch,semcmp)
int isyn[],lname,
in[],ikf,ikl,		/* in[ikf]-in[ikl] contain symbol sequence to parse */
lx[],						/* contains a lexicon */
istk[],					/* space for a recursion stack */
*itrace,					/* 0=no trace, 1=trace, >1= see page 127 */
*irkf,
*ltr,						/* length of ITS */
*irc;						/* return code. 0=successful, 1=parse failed. else
								see page 127 of LANG-PAK book */
ITSTYPE its[];					/* will contain translation sequence */
int  (*semmch)();
int  (*vmatch)();
int  (*semcmp)();

{
#ifdef debug 
		static char opstring[16][11]={		/* names of parse op codes */
						"succeed","gsucceed","apply lst","and","(","dec stk",
						"semantic","eos","alternate","failure","match word",
						"match word","match word","match word","match word",
						"match word"};
		static char tmp[MAX];
#endif
		/* extern int itktbl[7][4]; */
		static int one=1;
		int j,i,iudfs,kf,kl,istptr,lrc,idef,ibreg,loc,iop1;
		int icount,ialt,iflag,mrc,isrc;
		int	ix;

		mrc=0;
#ifdef debug
	/*printf("uti_parser(isyn,%d,in,%d,%d,lx,istk,%d,%d,its,%d,%d)\n",
			lname,ikf,ikl,*itrace,*irkf,*ltr,*irc);*/
		for(ix=0;ix<4;ix++)
			tmp[ix]=(char)isyn[kf+ix];
		tmp[(kl-kf+1)]='\0';
		printf("uti_parser:isyn=%s\n",tmp);
		for(ix=0;ix<(ikl-ikf+1);ix++)
			tmp[ix]=(char)in[ikf+ix];
		tmp[(ikl-ikf)+1]='\0';
		printf("uti_parser:in=%s\n",tmp);
		printf("uti_parser:lname=%d; ikf=%d; ikl=%d; itrace=%d\n",
					lname,ikf,ikl,*itrace);
#endif

      *irc=0;
      if((*itrace==0)||(*itrace==1)) goto L1;
      istptr=istk[1];
      kf=istk[istptr-3];
      kl=istk[istptr-2];
      *ltr=istk[istptr-1];
      iudfs= *itrace;
      if(iudfs<0) iudfs= -iudfs;
      goto L2;
		/* do initialization for parse */
L1:   kf= ikf;
      kl= ikl;
      *ltr=0;
      istptr=1;
		/* lookup l.s.t. name in lexicon */
      iudfs=uti_lxold(lx,isyn,1,lname,1);
      if(iudfs<=0)goto L26;
      idef=iudfs+lx[iudfs]+1;
      if(lx[idef]==0) goto L26;		/* branch if l.s.t. undefined */
		/* pick up location of parse machine instruction list
			entry in l.s.t. entry */
L2:   ibreg=iudfs+lx[iudfs]+1;
		/* output l.s.t. name if tracing */
      if(*itrace==1) uti_twrite(lx,iudfs+1,lx[iudfs]);
		/* reset trace flag if resumption */
      if(iudfs== *itrace) *itrace=1;
      if(iudfs== -(*itrace)) *itrace=0;
		/* set base register for parse machine instruction list */
      ibreg=lx[ibreg]+1;			
      if(ibreg==1) goto L27;			/* branch if undefined */
      loc=ibreg;							
L3:   iop1=lx[loc]; 						/* set to 1st word of op-code */
		/* switch on 1st word of op-code */
#ifdef debug
	printf("parse IOP1: %d=%s\n",iop1,&opstring[iop1-1][0]);
#endif
		switch (iop1) { /*goto(5,25,13,15,17,12,19,21,9,23,4,4,4,4,4,4),iop1; */
		case 1: goto L5;
		case 2: goto L25;
		case 3: goto L13;
		case 4: goto L15;
		case 5: goto L17;
		case 6: goto L12;
		case 7: goto L19;
		case 8: goto L21;
		case 9: goto L9;
		case 10: goto L23;
		case 11: case 12: case 13: case 14: case 15: case 16: goto L4;
		}

		/* IOP1=11,12,13,14,15,16 */
		/* Attempt to match a terminal word set member */
L4:   uti_lexanl(in,&kf,&kl,lx,iop1,lx[loc+1]+0,its,ltr,&lrc,vmatch);
		ikl=kl;			/* update ikl (locally) in case of incremental input */
		if(lrc==0) goto L9;				/* branch if match fails */
      if(*itrace!=1) goto L5;
      i=itktbl[6][1];
      j=itktbl[6][2]-i+1;
      uti_twrite(in,i,j);			/* output matched terminal word set member*/
      i=itktbl[6][3];
      j= *ltr-i+1;
      if(i==0) j=0;
		/* output translation of terminal word set member*/
      uti_trace(its,i,j);		

		/* IOP1=1 SUCCEED. */
		/* branch if successor field does not contain success */
L5:   if(lx[loc+2]!=1) goto L16;
		/* branch if not at top of recursion stack and successor field
			is not success */
L6:   if((istptr!=1)&&(istk[istptr-4]!= -1)) goto L8;
      if(*itrace==1) uti_twrite(lx,iudfs+1,lx[iudfs]);
      if(*itrace==1) printf(" success\n");
      if(istptr==1) goto L25;
L8:   istptr=istptr-6;
      loc=istk[istptr+1];
      kl=istk[istptr+4];
      iudfs=istk[istptr+6];
      ibreg=iudfs+lx[iudfs]+1;
      ibreg=lx[ibreg]+1;
      icount=istk[istptr+2];
      if(icount==-1) goto L5;
      icount=icount+1;
      if(icount<lx[loc+1]) goto L18;
		/* set to alternate field */
L9:   ialt=lx[loc+3];
      if(ialt>1) loc=ialt+ibreg-1;
      if ((ialt-1)>0) goto L3;		/* if(ialt-1) 10,6,3 */
		if ((ialt-1)==0) goto L6;
L10:  if(*itrace==1) uti_twrite(lx,iudfs+1,lx[iudfs]);
      if(*itrace==1) printf(" failure\n");
      if(istptr==1) goto L24;
L12:  istptr=istptr-6;
      icount=istk[istptr+2];
      if(icount==-1) loc=istk[istptr+1];
      kf=istk[istptr+3];
      kl=istk[istptr+4];
      *ltr=istk[istptr+5];
      iudfs=istk[istptr+6];
      ibreg=iudfs+lx[iudfs]+1;
      ibreg=lx[ibreg]+1;
      if(icount== -1)goto L9;
      if ((icount-lx[loc+1])<0) goto L9;	 /* if(icount-lx[loc+1]) 9,5,5; */
		goto L5;

		/* IOP1=3. Apply l.s.t. */
L13:  icount= -1;
      iflag=1;
      goto L22;
L14:  iudfs=lx[loc+1];
      goto L2;

		/* IOP1=4. AND */
L15:  kl=kf-1;
      kf=istk[istptr-3];
L16:  loc=lx[loc+2]+ibreg-1;		/* set to location of next instruction */
      goto L3;

		/* IOP1=5. ( */
L17:  icount=0;
L18:  iflag=2;
      goto L22;

		/* IOP1=7. Semantic specification */
L19:  if(lx[1]==1) goto L20;
      uti_metas(lx[loc+4]+0,in,ikf,ikl,kf,its,ltr,&mrc,semcmp);
      if (mrc==0) goto L23;		/*if(mrc) 5,23,5 */
		goto L5;
L20:  (*semmch)(lx,loc+4,lx[loc+1]+0,in,ikf,ikl,kf,kl,*itrace,its,*ltr,&isrc); 
     /***** next 3 stmts added 8/81 by g. hamlin ***/
      if (isrc!=3) goto L205;
      kf=kl+1;
      goto L25;
L205: if ((isrc-1)<0) goto L5;			/*if(isrc-1)5,9,23 */
		if ((isrc-1)==0) goto L9; goto L23;

		/* IOP1=8. EOS */
L21:  if ((kl-kf)<0) goto L5; goto L9;		/*if(kl-kf) 5,9,9 */
L22:  istk[istptr+1]=loc;
      istk[istptr+2]=icount;
      istk[istptr+3]=kf;
      istk[istptr+4]=kl;
      istk[istptr+5]=(*ltr);
      istptr=istptr+6;
      istk[istptr]=iudfs;
      switch (iflag) {					/*goto(14,16),iflag */
		case 1:goto L14; 
		case 2:goto L16;		
		}
		/* IOP1=10. FAILURE */
L23:  *irc=2;
      goto L25;
L24:  *irc=1;								/* syntactic failure */
		
		/* IOP1=2. GSUCCEED */
L25:  *irkf=kf;
      return;
L26:  *irc=3;					/* l.s.t. in isyn is undefined */
      goto L25;
L27:  *irc=iudfs;				/* tried to apply an undefined l.s.t. */
      if(itrace==0) *irc= -(*irc);
      istk[1]=istptr;		/* remember position in recursion stack */
      goto L25;
}



/*********************************************************************
**    I_FUNCTION :  uti_trace()
**	 Trace outputs the translation sequence contained in 
**		ITS[KF] thru ITS[LTR] 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uti_trace(its,kf,ltr)
int kf,ltr;		/* its[kf]-its[kr] contain translation sequence */
ITSTYPE  its[];

{
    int ival,num,i,j,ix,jj,kk;
		UU_REAL  x;
		int	jk, intits[80];
		char msg[256];

		/* x=(UU_REAL  *)(&ival);			/* equivalence (ival,x) */
      if(ltr!=0) goto L2;
L1:   ud_printmsg(" null translation\n");
      goto L18;
L2:   ud_printmsg(" translation\n");
      i=kf;
L4:   j=its[i].lint;				/* branch depending on translantion element */
      switch (j) {				/* goto(12,8,10,5,14),j */
		case 1: goto L12;			/* integer */
		case 2: goto L8;			/* real number */
		case 3: goto L10;			/* string */
		case 4: goto L5;			/* variable matched */
		case 5: goto L14;			/* user defined string */
		}
L5:   num=its[i+1].lint;
      i=i+2;
      j=i+num-1;
L6:   ud_printmsg(" v\n");
L7:   for(ix=i; ix<=j; ix++) printf(" %10d\n",its[ix].lint);
      i=j+1;
      goto L17;
L8:   x = its[i+1].lreal;
L9:   sprintf(msg, " n= %g\n",x);
	  ud_printmsg(msg);
      i=i+2;
      goto L17;
L10:  jj=its[i+1].lint;
L11:  sprintf(msg, " stg %10d\n",jj);
	  ud_printmsg(msg);
      i=i+2;
		for (jk=i; jk<=jj; jk++)
			intits[jk] = its[jk].lint;
      uti_twrite(intits,i,jj);
      i=i+jj;
      goto L17;
L12:  sprintf(msg, " i= %11d\n",its[i+1].lint);
	  ud_printmsg(msg);
      i=i+2;
      goto L17;
L14:  jj=its[i+1].lint;
      i=i+2;
      jj=i+jj-1;
L15:  ud_printmsg(" lit\n");
L16:  for(kk=i; kk<=jj; kk++) printf(" %12d\n",its[kk].lint);
      i=jj+1;
L17:  if(i<ltr) goto L4;
L18:  
return;
}

/* 	---------- conversion routine ------------------- */

/*  character conversion tables */
/*  ascii code (dc) to lang-pak internal symbol code */
int dctoic[96]={
/*  all ctrl chars to blanks					*/
	64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64, 
	64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64, 
/*32  sp  !  "   #   $   %  &   ' 			*/
      64,90,127,123,91,108,80,125,
/*40   (  )  *  +  ,   -  .  / 				*/
     	77,93,92,78,107,96,75,97,
/*48   0   1   2   3   4   5   6   7 		*/
     	240,241,242,243,244,245,246,247,
/*56   8   9   :   ;  <  =   >   ?   		*/
     	248,249,122,94,76,126,110,111,
/*64   at  a   b   c   d   e   f   g		*/
     	124,193,194,195,196,197,198,199,
/*72   h   i   j   k   l   m   n   o  		*/
     	200,201,209,210,211,212,213,214,
/*80   p   q   r   s   t   u   v   w 		*/
     	215,216,217,226,227,228,229,230,
/*88   x   y   z   [   \   ]   ^   _		*/
     	231,232,233,173,186,189,170,109};

/* lang-pak internal symbol codes to ascii code	*/
int ictodc[250]={
/*    ctrl chars 1-32 to '?'. null will have problem. */
     63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,
     63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,
/*    for all those ascii chars not in langpak set,	*/
/*    set up identity translation in first 64 elts of ictodc	*/
/*       # $ %    '     *     ,      /						*/
     	32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
     	48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,
/*    translate langpak internal codes to ascii codes	*/
/* 64 sp                               .  <   (  +     &	*/
     	32,32,32,32,32,32,32,32,32,32,32,46,60,40,43,32,38,
		32,32,32,32,32,32,32,32,32,
/* 90 exc $  *  )  ;     -  /									*/
     	33,36,42,41,59,32,45,47,
/*        							 	  ,  %   >  ?    		*/
     	32,32,32,32,32,32,32,32,32,44,37,32,62,63,
		32,32,32,32,32,32,32,32,32,32,
/*122 col # at 													*/
		58,35,64,
/*125 ' =  "      ^      [										*/
     	39,61,34,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
		32,32,32,32,32,32,32,32,32,
		94,32,32,91,32,32,32,32,32,32,32,32,32,32,32,32,
/*186  \       ]            a  b  c  d   e   f   g   h   i		*/
     	92,32,32,93,32,32,32,97,98,99,100,101,102,103,104,105,
/* 						      j 	 k   l   m   n   o   p   q   r	*/
     32,32,32,32,32,32,32,106,107,108,109,110,111,112,113,114,
	  32,32,32,32,32,32,32,32,
/*226  s   t   u   v   w   x   y   z 						*/
     	115,116,117,118,119,120,121,122,32,32,32,32,32,32,
/*240  0  1  2  3  4  5  6  7  8  9							*/
		48,49,50,51,52,53,54,55,56,57};



/*********************************************************************
**    I_FUNCTION :  cin_intern()
**		  change the character string into the langpak internal form
**    PARAMETERS   
**       INPUT  : 
**				len : langth of the input string
**				ibuf: array contains the input string
**				kf:	begin index to "in" array (ie- where to start o/p)
**       OUTPUT :  
**				in : array contains the string in langpak internal
**					  form
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

cin_intern(in, len, ibuf, kf)
int  in[], len, kf;
char  ibuf[];

{
	int  i, j;

	for (i=0; i<len; i++)
	{
		if (islower(ibuf[i]))
			ibuf[i] = toupper(ibuf[i]);	/* translate to upper case */
			j=ibuf[i]&127;
			in[kf+i]=dctoic[j];	/* translate to internal code*/
	}
}	/* cin_intern */
