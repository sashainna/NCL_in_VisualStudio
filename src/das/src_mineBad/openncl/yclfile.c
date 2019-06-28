/*********************************************************************
**    NAME         :  yclfile.c
**       CONTAINS:
**          int NclxMotClRead(clrec)
**          int NclxMotClPrev(clrec)
**          int NclxMotClWrite(clrec)
**          int NclxMotClPrint(clrec)
**          int NclxMotClExtents(clrec,pos)
**          int NclxMotClReset()
**          int NclxMotFillet(fillet)
**    COPYRIGHT 2000 (c) Numerical Control Computer Sciences Inc.
**              All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       yclfile.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:10:58
*********************************************************************/
#include "usysdef.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mfort.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclfile.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "ycom.h"
#include <setjmp.h>

static int printcl=0;
char *uu_lsnext(), *uu_lsprev(), *uu_lsinsrt();

/*********************************************************************
**    E_FUNCTION     : int NclxMotClRead(clrec)
**       This function reads a clfile record.
**    PARAMETERS
**    INPUT  :
**       clrec        Clfile record to read.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotClRead(clrec)
NCLX_mot_clrec *clrec;
{
	int i;
	double *pd;
	UN_clstruc *ps,*pc;
/*
*/
	if (printcl == 0) NclxDbgEnter ("NclxMotClRead(clrec)");
	NclxDbgPhdclrec (0,"clrec_header",clrec);
/*
.....Point to clfile record
*/
	ps = (UN_clstruc *)clrec->current;
	if (ps == 0 || ps == UN_clfirst[0])
		ps = (UN_clstruc *)uu_lsnext(UN_clfirst[0]);
	if (ps == 0) goto failed;
	while (ps && !ps->type)
	{
		ps = (UN_clstruc *)uu_lsnext(ps);
		if (ps) ps = (UN_clstruc *)uu_lsnext(ps);
	}
	if (ps == 0) goto failed;
/*
.....Retrieve cl record header
*/
	clrec->isn[0] = ps->isn[0];
	clrec->isn[1] = ps->isn[1];
	clrec->clrecno = ps->clrec;
	clrec->type = ps->type;
	clrec->subtype = ps->subt;
	clrec->mxcl = ps->mxcl;
/*
.....Point to cl data
*/
	pd = (double *)uu_lsnext(ps);
	if (pd == 0) goto failed;
	pc = (UN_clstruc *)pd;
/*
.....Retrieve cl data
*/
	for (i=0;i<ps->mxcl;i++)
	{
		clrec->cldata[i] = *pd++;
	}
/*
.....Move pointer to next cl record
*/
	ps = (UN_clstruc *)uu_lsnext(pc);
	if (ps == UN_clpt[0] || clrec->end == (char *)ps) ps = (UN_clstruc *)0;
	clrec->current = (char *)ps;
	goto done;
/*
.....Failure
.....Return EOF
*/
failed:;
   clrec->current = UU_NULL;
done:;
	NclxDbgPhdclrec (1,"clrec_header",clrec);
	if (printcl == 0) NclxDbgExit ("NclxMotClRead",0);
   return 0;
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotClPrev(clrec)
**       This function reads the previous clfile record.
**    PARAMETERS
**    INPUT  :
**       clrec        Clfile record to read.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotClPrev(clrec)
NCLX_mot_clrec *clrec;
{
	int i;
	double *pd;
	UN_clstruc *ps,*pc;
/*
.....Debug output
*/
	NclxDbgEnter ("NclxMotClPrev(clrec)");
	NclxDbgPhdclrec (0,"clrec_header",clrec);
/*
.....Point to clfile record
*/
	ps = (UN_clstruc *)clrec->current;
	if (ps == 0) ps = UN_clpt[0];
	if (ps == (UN_clstruc *)clrec->start) goto failed;
	while (ps && !ps->type)
	{
		ps = (UN_clstruc *)uu_lsprev(ps);
		if (ps) ps = (UN_clstruc *)uu_lsprev(ps);
	}
	if (ps == 0) goto failed;
/*
.....Retrieve cl record header
*/
	clrec->isn[0] = ps->isn[0];
	clrec->isn[1] = ps->isn[1];
	clrec->clrecno = ps->clrec;
	clrec->type = ps->type;
	clrec->subtype = ps->subt;
	clrec->mxcl = ps->mxcl;
/*
.....Point to cl data
*/
	pd = (double *)uu_lsnext(ps);
	if (pd == 0) goto failed;
	pc = (UN_clstruc *)pd;
/*
.....Retrieve cl data
*/
	for (i=0;i<ps->mxcl;i++)
	{
		clrec->cldata[i] = *pd++;
	}
/*
.....Move pointer to this cl record
*/
	if (ps == (UN_clstruc *)clrec->start) ps = (UN_clstruc *)0;
	clrec->current = (char *)ps;
	goto done;
/*
.....Failure
.....Return EOF
*/
failed:;
   clrec->current = UU_NULL;
done:;
	NclxDbgPhdclrec (1,"clrec_header",clrec);
	NclxDbgExit ("NclxMotClRead",0);
   return 0;
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotClWrite(clrec)
**       This function writes a clfile record.
**    PARAMETERS
**    INPUT  :
**       clrec        Clfile record to read.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotClWrite(clrec)
NCLX_mot_clrec *clrec;
{
	int i,iend,inc;
	double *pd;
	UN_clstruc *ps,*pc;
/*
.....Debug output
*/
	NclxDbgEnter ("NclxMotClWrite(clrec)");
	NclxDbgPhdclrec (0,"clrec_header",clrec);
/*
.....Determine if at EOF
*/
	if (clrec->current == (char *)UN_clpt[0])
	{
		iend = 1;
		ps = UN_clpt[0];
	}
	else
	{
		iend = 0;
		ps = (UN_clstruc *)clrec->current;
		if (ps == 0) ps = UN_clfirst[0];
		if (ps != UN_clfirst[0])
			ps = (UN_clstruc *)uu_lsnext(ps);
		ps = (UN_clstruc *)uu_lsinsrt(ps,sizeof(UN_clstruc));
		if (ps == 0) goto failed;
	}
/*
.....Set up integer portion of clrec
*/
	ps->isn[0] = clrec->isn[0];
	ps->isn[1] = clrec->isn[1];
	ps->clrec = clrec->clrecno;
	ps->type = clrec->type;
	ps->subt = clrec->subtype;
	ps->mxcl = clrec->mxcl;
/*
.....Store cl data
*/
	inc = ps->mxcl; if (inc == 0) inc = 1;
	pd = (double *)uu_lsinsrt(ps,sizeof(double)*inc);
	if (pd == 0) goto failed;
	pc = (UN_clstruc *)pd;
	if (clrec->mxcl > 0)
	{
		for (i=0;i<clrec->mxcl;i++)
		{
			*pd++ = clrec->cldata[i];
		}
	}
/*
.....Move pointer to next cl record
*/
	if (iend == 1)
	{
		UN_clpt[0] = (UN_clstruc *)uu_lsinsrt(pc,sizeof(UN_clstruc));
		ps = UN_clpt[0];
	}
	clrec->current = (char *)ps;
	goto done;
/*
.....Failure
.....Return EOF
*/
failed:;
   clrec->current = UU_NULL;
done:;
	NclxDbgPhdclrec (1,"clrec_header",clrec);
	NclxDbgExit ("NclxMotClWrite",0);
   return 0;
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotClPrint(clrec)
**       This function prints out the requested clfile range.
**    PARAMETERS
**    INPUT  :
**       clrec        Clfile range to print.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMotClPrint(clrec)
NCLX_mot_clrec *clrec;
{
   int inc,i;
	char sbuf[200];
/*
.....Print clfile
*/
	printcl = 1;
   NclxMotClRewind((*clrec));
   do
   {
      NclxMotClRead(clrec);
      if (clrec->type == 5000 || clrec->type == 5200 || clrec->type == 5210 ||
			clrec->type == 5220)
      {
        inc = 6;
        if (clrec->type >= 5200) inc = 21;
        if (clrec->type == 5210 || clrec->type == 5000)
			{
				sprintf(sbuf,"$$........");
				NclxDbgPstr(sbuf);
			}
        for (i=0;i<clrec->mxcl;i+=inc)
        {
            sprintf(sbuf,"%d %d(%d)/%f,%f,%f,   %f,%f,%f",clrec->isn[0],
					clrec->type,clrec->subtype,clrec->cldata[i],clrec->cldata[i+1],
            clrec->cldata[i+2],clrec->cldata[i+3],clrec->cldata[i+4],
            clrec->cldata[i+5]);
				NclxDbgPstr(sbuf);
        }
      }
      else if (clrec->type == 2000)
      {
        if (clrec->subtype == 1009)
        {
          sprintf (sbuf,"%d FEDRAT/%f",clrec->isn[0],clrec->cldata[0]);
				NclxDbgPstr(sbuf);
        }
        else if (clrec->subtype == 5)
        {
          sprintf (sbuf,"%d RAPID",clrec->isn[0]);
				NclxDbgPstr(sbuf);
        }
      }
      else if (clrec->type == 3000)
		{
			sprintf(sbuf,"$$........");
				NclxDbgPstr(sbuf);
         sprintf(sbuf,"CIRCLE/%f,%f,%f,%f,%f,%f,%f",clrec->cldata[5],
				clrec->cldata[6],clrec->cldata[7],clrec->cldata[8],
				clrec->cldata[9],clrec->cldata[10],clrec->cldata[11]);
				NclxDbgPstr(sbuf);
		}
   } while (clrec->current != UU_NULL);
	printcl = 0;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotClExtents(clrec,pos)
**       This function returns the EOF clfile record number.
**    PARAMETERS
**    INPUT  :
**       none
**    OUTPUT :
**       clrec        EOF clfile record (in clrec.current).
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotClExtents(clrec,pos)
NCLX_mot_clrec *clrec;
NCLX_mot_clrec_pos pos;
{
/*
.....Debug output
*/
	NclxDbgEnter ("NclxMotClExtents(clrec)");
/*
.....Point to clfile record
*/
/*	clrec->start = (char *)uu_lsnext(UN_clfirst[0]);*/
	clrec->start = (char *)UN_clfirst[0];
	clrec->end = (char *)UN_clpt[0];
	if (pos == NCLX_BOF) clrec->current = clrec->start;
	else clrec->current = clrec->end;
/*
.....End of routine
*/
done:;
	NclxDbgPhdclrec (1,"clrec_header",clrec);
	NclxDbgExit ("NclxMotClExtents",0);
   return 0;
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotClReset()
**       Performs a full reset on the clfile (closes it and reopens it)
**    PARAMETERS
**    INPUT  :
**       none
**    OUTPUT :
**       none
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotClReset()
{
	UM_int2 i2=0;
	UN_clstruc *clptr;
/*
.....Debug output
*/
	NclxDbgEnter ("NclxMotClReset()");
/*
.....Initialize the clfile
*/
	clopen(&i2,&clptr);
	UY_clstart = 0;
/*
.....End of routine
*/
done:;
	NclxDbgExit ("NclxMotClReset",0);
	return(NCLX_SUCCESS);
}


/*********************************************************************
**    E_FUNCTION     : int NclxMotFillet (fillet)
**       This function creates fillets between motions.
**			surface.
**    PARAMETERS
**    INPUT  :
**       fillet       Contains the fillet parameters and clfile range
**			             to process.
**    OUTPUT :
**       none
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : 
**			Modifies the clfile.
**    WARNINGS     : none
*********************************************************************/
NclxMotFillet(fillet)
NCLX_mot_fillet *fillet;
{
	int stat,icnt;
	NCLX_mot_clrec start;
	char *first, *second, *old, *irec;
	UM_int2 istat,idx;
	UM_real8 sc123;
	NCLX_mot_fillet filoff;
/*
....Debug output
*/
	NclxDbgEnter ("NclxMotFillet(fillet)");
	NclxDbgPfillet (0,"fillet",fillet);
/*
.....Setup fillet attributes
*/
	stat = 0;
	icnt = 0;
	idx = 123;
	getsc(&idx,&sc123);
	NclxMotSetFillet(fillet);
/*
.....Position to beginning of cl record range
*/
	NclxMotClRewind(fillet->clrange);
	NclxMotClRead(&(fillet->clrange));
	start = fillet->clrange;
/*
.....Move start back to the selected beginning record.
*/
	NclxMotClRewind(start);
/*
.....Find record coming from
*/
	do
	{
		irec = start.current;
		NclxMotClRead(&start);
/*	printf("Old irec = %d   type = %d   subtype = %d\n",
		irec,start.type,start.subtype);
	printf("Pt = %f,%f,%f\n",start.cldata[0],start.cldata[1],start.cldata[2]);*/
	} while (start.type != 5000 && start.type != 5200);
	old = irec;
/*
.....Find corner point
*/
	do
	{
		irec = start.current;
		NclxMotClRead(&start);
/*	printf("First irec = %d   type = %d   subtype = %d\n",
		irec,start.type,start.subtype);
	printf("Pt = %f,%f,%f\n",start.cldata[0],start.cldata[1],start.cldata[2]);*/
	} while ((start.type != 5000 && start.type != 5200) || start.subtype == 6);
	first = irec;
/*
.....Fillet all corners in clfile range
*/
	second = start.end;
/*   printf("old = %d   first = %d   second = %d\n",old,first,second);*/
	pfillet(&first,&second,&old,&istat);
	stat = istat;
/*
.....Cancel fillet routine
*/
	idx = 123;
	setscv(&idx,&sc123);
	filoff.rad = 0.;
	filoff.tol = fillet->tol;
	filoff.same = fillet->same;
	filoff.maxang = fillet->maxang;
	filoff.combine = fillet->combine;
	filoff.fedctl = fillet->fedctl;
	filoff.fedrt = fillet->fedrt;
	filoff.fmax = fillet->fmax;
	filoff.direction = fillet->direction;
	filoff.cdia = fillet->cdia;
	NclxMotSetFillet(&filoff);
/*
.....End of routine
*/
done:;
	NclxDbgExit ("NclxMotFillet",stat);
	return(stat);
}
