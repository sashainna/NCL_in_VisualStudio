#include "usysdef.h"
#if UU_COMP == UU_CIM
/************************************************************************
**	MODULE: uctofor.c
**
**       linchk() ** commented out **
**       uplabl() ** commented out **
**       ptlbl()  ** commented out **
**       lnlbl()  ** commented out **
**       cilbl()  ** commented out **
**
**	Wrappers for Fortran routines that are called by C.
**    COPYRIGHT 1988 (c) Mills Data Systems Co.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
c**       uctofor.c , 25.1
c**    DATE AND TIME OF LAST MODIFICATION
c**       04/29/15 , 15:11:52
**********************************************************************/
#ifdef UU_DEBUGON
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) uctofor.c 19.1 10/5/6 10:54:07 single"};
#else
static char uu_sccsident[]={"@(#) uctofor.c 19.1 10/5/6 10:54:07 double"};
#endif
#endif

#include "udebug.h"

savepp(name,whr)
char *name;
short *whr;
{
	extern	SAVEPP();

	uu_denter(UU_MTRC,(us,"WRAPPER savepp(name,whr,%s,%d)",name,*whr));
	callFortran(SAVEPP,3,name,80,whr);
	uu_dexit;
	return;
}

nclfin()
{
	uu_denter(UU_DTRC,(us,"WRAPPER nclfin()"));
	NCLFIN();
	uu_dexit;
	return;
}

setifl(idx,ival)
short *idx,*ival;
{
	extern	SETIFL();

	uu_denter(UU_MTRC,(us,"WRAPPER setifl(idx=%d,ival=%d)",*idx,*ival));
	callFortran(SETIFL,2,idx,ival);
	uu_dexit;
	return;
}

setsc(ival)
short *ival;
{
	extern	SETSC();

	uu_denter(UU_MTRC,(us,"WRAPPER setsc(ival=%d)",*ival));
	callFortran(SETSC,1,ival);
	uu_dexit;
	return;
}

int getpp()
{
	int	rtn;

	uu_denter(UU_MTRC,(us,"WRAPPER getpp()"));
	rtn = GETPP();
	uu_dexit;
	return(rtn);
}

ptppnm(name)
char	*name;
{
	extern	PTPPNM();

	uu_denter(UU_MTRC,(us,"WRAPPER ptppnm(%s)",name));
	callFortran(PTPPNM,2,name,80);
	uu_dexit;
	return;
}

reslab()
{
	uu_denter(UU_MTRC,(us,"WRAPPER reslab()"));
	RESLAB();
	uu_dexit;
	return;
}

setlbt()
{
	uu_denter(UU_MTRC,(us,"WRAPPER setlbt()"));
	SETLBT();
	uu_dexit;
	return;
}

savlab()
{
	uu_denter(UU_MTRC,(us,"WRAPPER savlab()"));
	SAVLAB();
	uu_dexit;
	return;
}

setlbf()
{
	uu_denter(UU_MTRC,(us,"WRAPPER setlbf()"));
	SETLBF();
	uu_dexit;
	return;
}

getas(name)
char	*name;
{
	extern	GETAS();

	uu_denter(UU_MTRC,(us,"WRAPPER getas(%s)",name));
	callFortran(GETAS,2,name,80);
	uu_dexit;
	return;
}

gpgmnm(name)
char	*name;
{
	extern	GPGMNM();

	uu_denter(UU_MTRC,(us,"WRAPPER gpgmnm(%s)",name));
	callFortran(GPGMNM,2,name,80);
	uu_dexit;
	return;
}

getclf(name)
char	*name;
{
	extern	GETCLF();

	uu_denter(UU_MTRC,(us,"WRAPPER getclf(%s)",name));
	callFortran(GETCLF,2,name,80);
	uu_dexit;
	return;
}

gtrtnm(name)
char	*name;
{
	extern	GTRTNM();

	uu_denter(UU_MTRC,(us,"WRAPPER gtrtnm(%s)",name));
	callFortran(GTRTNM,2,name,80);
	uu_dexit;
	return;
}

loadpp(name,errflg,whr)
char	*name;
short	*errflg, *whr;
{
	extern	LOADPP();

	uu_denter(UU_MTRC,(us,"WRAPPER loadpp(%s,%d,%d)",name,*errflg,*whr));
	callFortran(LOADPP,4,name,80,errflg,whr);
	uu_dexit;
	return;
}

ptdfnm(name)
char	*name;
{
	extern	PTDFNM();

	uu_denter(UU_MTRC,(us,"WRAPPER ptdfnm(%s)",name));
	callFortran(PTDFNM,2,name,80);
	uu_dexit;
	return;
}

tlpara(maxang,maxdp,mxloop,mindp,warn,numpts,thps,thds,thcs,toler,gougck,iter)
double	*maxang, *maxdp, *toler, *thps, *thds, *thcs;
float   *mindp;
short	*numpts, *gougck, *iter,*warn, *mxloop;
{
	extern	TLPARA();
	
	uu_denter(UU_MTRC,(us,"WRAPPER tlpara(%f,%f,%d,%f,%d,%d,%f,%f,%f,%f,%d,%d)"
		,*maxang,*maxdp,*mxloop,*mindp,*warn,*numpts,*thps,*thds,*thcs,
		*toler,*gougck,*iter));
	callFortran(TLPARA,12,maxang,maxdp,mxloop,mindp,warn,numpts,thps,thds,
				thcs,toler,gougck,iter);
	uu_dexit;
	return;
}

upiter(iter)
short *iter;
{
	extern	UPITER();

	uu_denter(UU_MTRC,(us,"WRAPPER upiter(%d)",*iter));
	callFortran(UPITER,1,iter);
	return;

}

curnam(name,isubsr)
char	*name;
int	*isubsr;
{
	extern	CURNAM();

	uu_denter(UU_MTRC,(us,"WRAPPER curnam(name=%x,isubsr=%d)",name,*isubsr));
	callFortran(CURNAM,3,name,64,isubsr);
	uu_dexit;
}

getifl(idx,ival)
short	*idx, *ival;
{
	extern	GETIFL();

	uu_denter(UU_MTRC,(us,"WRAPPER getifl(%d,%d)",*idx,*ival));
	callFortran(GETIFL,2,idx,ival);
	uu_dexit;
	return;
}

ptpsrc(cline,cnchar,cstr)
short	*cline, *cnchar;
char	*cstr;
{
	extern	PTPSRC();

	uu_denter(UU_MTRC,(us,"WRAPPER ptpsrc(%d,%d,%s)",*cline,*cnchar,cstr));
	callFortran(PTPSRC,4,cline,cnchar,cstr,80);
	uu_dexit;
	return;
}

dnarrw()
{
	uu_denter(UU_MTRC,(us,"WRAPPER dnarrw()"));
	DNARRW();
	uu_dexit;
	return;
}

getnln(ival)
short	*ival;
{
	extern	GETNLN();

	uu_denter(UU_MTRC,(us,"WRAPPER getnln(%d)",*ival));
	callFortran(GETNLN,1,ival);
	uu_dexit;
	return;
}

uparrw()
{
	uu_denter(UU_MTRC,(us,"WRAPPER uparrw()"));
	UPARRW();
	uu_dexit;
	return;
}

gtpsrc(cline,cnchar,cstr)
short	*cline, *cnchar;
char	*cstr;
{
	extern	GTPSRC();

	uu_denter(UU_MTRC,(us,"WRAPPER gtpsrc(%d,%d,%d)",*cline,*cnchar,cstr));
	callFortran(GTPSRC,4,cline,cnchar,cstr,80);
	uu_dprint(UU_MTRC,(us,"WRAPPER after call to gtpsrc = %d",cstr));
	uu_dexit;
	return;
}

ranini()
{
	uu_denter(UU_MTRC,(us,"WRAPPER ranini()"));
	RANINI();
	uu_dexit;
	return;
}

ranstr(name,isub,ietype,nclkey,is)
char	*name;
short	*ietype, *is;
int	*isub, *nclkey;
{
	extern	RANSTR();

	uu_denter(UU_MTRC,(us,"WRAPPER ranstr(%s,%d,%d,%d,%d)",name,*isub,*ietype,*nclkey,*is));
	callFortran(RANSTR,6,name,64,isub,ietype,nclkey,is);
	uu_dexit;
	return;
}

randel(name,isub,is)
char	*name;
short	*is;
int *isub;
{
	extern	RANDEL();

	uu_denter(UU_MTRC,(us,"WRAPPER randel(%s,%d,%d)",name,*isub,*is));
	callFortran(RANDEL,4,name,64,isub,is);
	uu_dexit;
	return;
}

sfeval(nclkey,max,npts,buf,iret)
int	*nclkey;
short	*max, *npts, *iret;
double	*buf;
{
	extern	SFEVAL();

	uu_denter(UU_MTRC,(us,"WRAPPER sfeval(%d,%d,%d,%x,%d)",*nclkey,*max,*npts,buf,*iret));
	callFortran(SFEVAL,5,nclkey,max,npts,buf,iret);
	uu_dexit;
	return;
}

cveval(nclkey,max,npts,buf)
int	*nclkey;
short	*max, *npts;
double	*buf;
{
	extern	CVEVAL();

	uu_denter(UU_MTRC,(us,"WRAPPER cveval(%d,%d,%d,%x)",*nclkey,*max,*npts,buf));
	callFortran(CVEVAL,4,nclkey,max,npts,buf);
	uu_dexit;
	return;
}

nclini()
{
	uu_denter(UU_MTRC,(us,"WRAPPER nclini()"));
	NCLINI();
	uu_dexit;
	return;
}

unitcv(dbuf)
double	*dbuf;
{
	extern	UNITCV();

	uu_denter(UU_MTRC,(us,"WRAPPER unitcv(%x)",dbuf));
	callFortran(UNITCV,1,dbuf);
	uu_dexit;
	return;
}

nclsys(ierr)
short	*ierr;
{
	extern	NCLSYS();

	uu_denter(UU_MTRC,(us,"WRAPPER nclsys(%d)",*ierr));
	callFortran(NCLSYS,1,ierr);
	uu_dprint(UU_MTRC,(us,"WRAPPER after nclsys"));
	uu_dexit;
	return;
}

drwent(nclkey,dtype)
int	*nclkey;
short	*dtype;
{
	extern	DRWENT();

	uu_denter(UU_MTRC,(us,"WRAPPER drwent(%d,%d)",*nclkey,*dtype));
	callFortran(DRWENT,2,nclkey,dtype);
	uu_dexit;
	return;
}

rsttrm()
{
	uu_denter(UU_MTRC,(us,"WRAPPER rsttrm()"));
	RSTTRM();
	uu_dexit;
	return;
}

cvepts(nclkey,spt,ept)
int	*nclkey;
double	*spt, *ept;
{
	extern	CVEPTS();

	uu_denter(UU_MTRC,(us,"WRAPPER cvepts(%d,%x,%x)",nclkey,spt,ept));
	callFortran(CVEPTS,3,nclkey,spt,ept);
	uu_dexit;
	return;
}

cvlbl(ival)
short	*ival;
	{
	extern	CVLBL();

	uu_denter(UU_MTRC,(us,"WRAPPER cvlbl(%d)",*ival));
	callFortran(CVLBL,1,ival);
	uu_dprint(UU_MTRC,(us,"WRAPPER after cvlbl"));
	uu_dexit;
	return;
	}

randlk(nclkey, ncltype, is)
int *nclkey; 
short *is, *ncltype;
	{
	extern	RANDLK();

	uu_denter(UU_MTRC,(us,"WRAPPER randlk(%d,%d,%d)",*nclkey, *ncltype, *is));
	callFortran(RANDLK,3,nclkey,ncltype,is);
	uu_dprint(UU_MTRC,(us,"WRAPPER after randlk"));
	uu_dexit;
	return;
	}

strwf1(nclkey)
int *nclkey;
	{
	extern	STRWF1();

	uu_denter(UU_MTRC,(us,"WRAPPER strwf1(%d)",*nclkey));
	callFortran(STRWF1,1,nclkey);
	uu_dprint(UU_MTRC,(us,"WRAPPER after strwf1"));
	uu_dexit;
	return;
	}

strwf2(nclkey, ietype, nam)
int *nclkey;
short *ietype;
char *nam;
	{
	extern	STRWF2();

	uu_denter(UU_MTRC,(us,"WRAPPER strwf2(%d,%d,%s)",*nclkey,
		*ietype, nam));
	callFortran(STRWF2,4,nclkey,ietype,nam,64);
	uu_dprint(UU_MTRC,(us,"WRAPPER after strwf2"));
	uu_dexit;
	return;
	}

labwf(ietype, label, nclkey, where, istatus)
short *ietype;
char *label;
int  *nclkey;
int  *where;
short *istatus;
	{
	extern	LABWF();

	uu_denter(UU_MTRC,(us,"WRAPPER labwf(%d,%s,%d,%d,%d)",*ietype,label,*nclkey,*where,*istatus));
	callFortran(LABWF,6,ietype,label,64,nclkey,where,istatus);
	uu_dprint(UU_MTRC,(us,"WRAPPER after labwf"));
	uu_dexit;
	return;
	}

lblchk(itype, lblst)
short *itype, *lblst;
	{
	extern	LBLCHK();

	uu_denter(UU_MTRC,(us,"WRAPPER lblchk(%d,%d)",*itype,*lblst));
	callFortran(LBLCHK,2,itype,lblst);
	uu_dprint(UU_MTRC,(us,"WRAPPER after lblchk"));
	uu_dexit;
	return;
	}

pleval(nclkey,max,npts,buf,n)
int	*nclkey;
short	*max, *npts, *n;
double	*buf;
{
	extern	PLEVAL();

	uu_denter(UU_MTRC,(us,"WRAPPER pleval(%d,%d,%d,%d,%x)",*nclkey,*max,*npts,*n,buf));
	callFortran(PLEVAL,5,nclkey,max,npts,buf,n);
	uu_dexit;
	return;
}

rstint()
{
	uu_denter(UU_MTRC,(us,"WRAPPER rstint()"));
	RSTINT();
	uu_dexit;
	return;
}

rpchk(rpfl)
short	*rpfl;
	{
	extern	RPCHK();

	uu_denter(UU_MTRC,(us,"WRAPPER rpchk(%d)",*rpfl));
	callFortran(RPCHK,1,rpfl);
	uu_dprint(UU_MTRC,(us,"WRAPPER after rpchk"));
	uu_dexit;
	return;
	}

setstp(ival)
short	*ival;
	{
	extern	SETSTP();

	uu_denter(UU_MTRC,(us,"WRAPPER setstp(%d)",*ival));
	callFortran(SETSTP,1,ival);
	uu_dprint(UU_MTRC,(us,"WRAPPER after setstp"));
	uu_dexit;
	return;
	}


setins(ivl1,ivl2)
short *ivl1,*ivl2;
{
	extern	SETINS();

	uu_denter(UU_MTRC,(us,"WRAPPER setins(ivl1=%d,ivl2=%d)",*ivl1,*ivl2));
	callFortran(SETINS,2,ivl1,ivl2);
	uu_dexit;
	return;
}


gtview(rsorig,rszvec,rsyvec,option,status)
short	*option,*status;
double	*rsorig, *rszvec, *rsyvec;
{
	extern	GTVIEW();

	uu_denter(UU_MTRC,(us,"WRAPPER gtview(%x,%x,%x,%d)",rsorig,rszvec,rsyvec,option));
	callFortran(GTVIEW,5,rsorig,rszvec,rsyvec,option,status);
	uu_dexit;
	return;
}

gtmx(cbuf,rsorig,rszvec,rsyvec,ierr)
UU_LOGICAL	*ierr;
double	*rsorig, *rszvec, *rsyvec;
{
	extern	GTMX();

	uu_denter(UU_MTRC,(us,"WRAPPER gtmx(%x,%x,%x,%d)",rsorig,rszvec,rsyvec,ierr));
	callFortran(GTMX,6,cbuf,16,rsorig,rszvec,rsyvec,ierr);
	uu_dexit;
	return;
}

labcam(ietype, label, isub, nclkey, istatus)
short *ietype;
char *label;
int *isub;
int  *nclkey;
short *istatus;
	{
	extern	LABCAM();

	uu_denter(UU_MTRC,(us,"WRAPPER labcam(%d,%s,%d,%d,%d)",*ietype,label,*isub,*nclkey,*istatus));
	callFortran(LABCAM,6,ietype,label,64,isub,nclkey,istatus);
	uu_dprint(UU_MTRC,(us,"WRAPPER after labcam"));
	uu_dexit;
	return;
	}

chklab(label,nclkey, isub, ifound, ifl41)
char *label;
int  *nclkey;
short *ifound;
short *ifl41;
int *isub;
	{
	extern	CHKLAB();

	uu_denter(UU_MTRC,(us,"WRAPPER chklab(%s,%d,%d,%d)",label,*nclkey,*ifound,*ifl41));
	callFortran(CHKLAB,6,label,64,nclkey,isub,ifound,ifl41);
	uu_dprint(UU_MTRC,(us,"WRAPPER after chklab"));
	uu_dexit;
	return;
	}

uerror(label, isub)
char *label;
int *isub;
	{
	extern	UERROR();

	uu_denter(UU_MTRC,(us,"WRAPPER uerror(%s,%d)",label,*isub));
	callFortran(UERROR,3,label,64,isub);
	uu_dprint(UU_MTRC,(us,"WRAPPER after uerror"));
	uu_dexit;
	return;
	}

chkvoc(name,ncltyp, gtyp, lsub)
char *name;
short  *ncltyp;
short *gtyp;
int *lsub;
	{
	extern	CHKVOC();

	uu_denter(UU_MTRC,(us,"WRAPPER chkvoc(%s,%d,%d,%d)",name,*ncltyp,*gtyp,*lsub));
	callFortran(CHKVOC,5,name,64,ncltyp,gtyp,lsub);
	uu_dprint(UU_MTRC,(us,"WRAPPER after chkvoc"));
	uu_dexit;
	return;
	}

sflbl(ival)
short	*ival;
	{
	extern	SFLBL();

	uu_denter(UU_MTRC,(us,"WRAPPER sflbl(%d)",*ival));
	callFortran(SFLBL,1,ival);
	uu_dprint(UU_MTRC,(us,"WRAPPER after sflbl"));
	uu_dexit;
	return;
	}

pokpar(fnramp,fentry,fnwrn,frtrct,fcltyp,fpdir,fspirl,fslift,fcornr,
       frmpds,fnumlv,fretds,fmaxst,fminst,fgenfr,fposfr,
       fretfr,fentfr,ftrnfr,ffinfr,fclrlv,cclrlv)

short  *fnramp, *fentry, *fnwrn, *frtrct, *fcltyp, *fpdir;
short  *fspirl, *fslift, *fcornr;
float  *frmpds, *fnumlv, *fretds, *fmaxst, *fminst;
float  *fgenfr, *fposfr, *fretfr, *fentfr, *ftrnfr, *ffinfr;
double *fclrlv;
char   *cclrlv;
	{
	extern	POKPAR();

	uu_denter(UU_MTRC,(us,"WRAPPER pokpar(%d,%d,%d,%d,%d,%d,%d,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%s)",fnramp,*fentry,*fcltyp,*fpdir,
           *fspirl,*fslift,*fcornr,*frmpds,*fnumlv,*fretds,*fmaxst,*fminst,
           *fgenfr,*fposfr,*fretfr,*fentfr,*ftrnfr,*ffinfr,*fclrlv,*cclrlv));

	callFortran(POKPAR,21,fnramp,fentry,fnwrn,frtrct,fcltyp,fpdir,fspirl,fslift,
           fcornr, frmpds,fnumlv,fretds,fmaxst,fminst,fgenfr,fposfr,
           fretfr,fentfr,ftrnfr,ffinfr,fclrlv,cclrlv,16);

	uu_dprint(UU_MTRC,(us,"WRAPPER after pokpar"));
	uu_dexit;
	return;
	}

ppini()
{
	uu_denter(UU_DTRC,(us,"WRAPPER ppini()"));
	PPINI();
	uu_dexit;
	return;
}

statln()
{
	uu_denter(UU_DTRC,(us,"WRAPPER STATLN()"));
	STATLN();
	uu_dexit;
	return;
}

ncvevl(nclkey, u, pt, ve)
double *u;
int	*nclkey;
double pt[3], ve[3];
{
	extern	NCVEVL();

	uu_denter(UU_MTRC,(us,"WRAPPER ncvevl(nclkey=%d,u=%g)",*nclkey,*u));
	callFortran(NCVEVL,4,nclkey, u, pt, ve);
	uu_dexit;
	return;
}
#ifdef OLDNCL
uplabl(label1, label2)
char *label1;
char *label2;
	{
	extern	UPLABL();

	uu_denter(UU_MTRC,(us,"WRAPPER uplabl(%s,%s)",label1,label2));
	callFortran(UPLABL,4,label1,8,label2,8);
	uu_dprint(UU_MTRC,(us,"WRAPPER after uplabl"));
	uu_dexit;
	return;
	}


ptlbl(ival)
short	*ival;
	{
	extern	PTLBL();

	uu_denter(UU_MTRC,(us,"WRAPPER ptlbl(%d)",*ival));
	callFortran(PTLBL,1,ival);
	uu_dprint(UU_MTRC,(us,"WRAPPER after ptlbl"));
	uu_dexit;
	return;
	}

lnlbl(ival)
short	*ival;
	{
	extern	LNLBL();

	uu_denter(UU_MTRC,(us,"WRAPPER lnlbl(%d)",*ival));
	callFortran(LNLBL,1,ival);
	uu_dprint(UU_MTRC,(us,"WRAPPER after lnlbl"));
	uu_dexit;
	return;
	}

cilbl(ival)
short	*ival;
	{
	extern	CILBL();

	uu_denter(UU_MTRC,(us,"WRAPPER cilbl(%d)",*ival));
	callFortran(CILBL,1,ival);
	uu_dprint(UU_MTRC,(us,"WRAPPER after cilbl"));
	uu_dexit;
	return;
	}

linchk(linfl, chkfl, istfl)
short *linfl,*chkfl,*istfl;
{
	extern	LINCHK();

	uu_denter(UU_MTRC,(us,"WRAPPER linchk(linfl=%d,chkfl=%d,istfl=%d)",*linfl,*chkfl,*istfl));
	callFortran(LINCHK,3,linfl,chkfl,istfl);
	uu_dexit;
	return;
}

#endif /* OLDNCL */
#endif
