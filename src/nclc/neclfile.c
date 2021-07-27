/*********************************************************************
**    NAME         :  neclfile.c
**       CONTAINS:
**			clopen(clfile,clpt)
**			clclos(clfile)
**			clgetp(clfile,clpt)
**			ncl_swap_clfiles()
**			clstor(clfile,icpt,iclw,rdata)
**			clread(clfile,icpt,iclw,rdata)
**			clprev(clfile,icpt,iclw,rdata)
**			cldel(clfile,icpt,iept)
**       clstor_insert()
**       inirvl
**       finrvl
**       pshrvl
**       poprvl
**       ncl_set_temp_clfile
**       ncl_reset_temp_clfile
**       ncl_pause_temp_clfile
**       ncl_resume_temp_clfile
**       ncl_revers_clfile
**       isn2i4
**       isn4i2
**    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       neclfile.c , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**       10/27/16 , 13:01:14
*********************************************************************/

#define NCLFILE
#include "usysdef.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclfile.h"
#include "ulist.h"
#include "xenv1.h"

typedef struct
{
	UN_clstruc *begin;
	UN_clstruc *end;
} REV_list_struc;

#define NREVLISTS 10
static UU_LIST rev_list[NREVLISTS];
static int rev_nl[NREVLISTS];
static UU_LOGICAL Stemp_flag = UU_FALSE, Stemp_save = UU_FALSE, Slast_saved = UU_FALSE;
/*
.....Pointers used when the temporary cl file is saved and added to the 
.....primary cl file
*/
static UN_clstruc *Sread = 0, *Sstor = 0;

char *uu_lsnew(), *uu_lsinsrt(), *uu_lsnext(), *uu_lsprev(), *uu_lsdele();
void clclos(),S_debug_clfile();

static int S_clptr();

static int lines = 0;

/*********************************************************************
**    E_FUNCTION     : clopen(clfile,clpt)
**       FORTRAN callable routine to initialize an internal clfile.
**    PARAMETERS   
**       INPUT  : 
**          clfile  = 1 = Open primary clfile.  2 = Secondary.
**       OUTPUT :  
**          clpt    = Pointer to next record in internal clfile.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void clopen(clfile,clpt)
UN_clstruc **clpt;
UM_int2 *clfile;
{
	int ipt;
/*
.....Create a clfile list to store
.....all clfile records
*/
	ipt = *clfile;
	clclos(clfile);
	UN_clpt[ipt] = (UN_clstruc *)uu_lsnew();
/*
.....Could not allocate list
*/
	if (UN_clpt[ipt] == 0)
	{
		ud_wrerr("Could not allocate memory for internal clfile.");
		goto done;
	}
/*
.....Initialize clfile record number
*/
	UN_clnum[ipt] = 0;
	UN_clfirst[ipt] = UN_clpt[ipt];
	UN_clpt[ipt] = (UN_clstruc *)uu_lsinsrt((char *)UN_clpt[ipt],
		sizeof(UN_clstruc));
done:;
    *clpt = UN_clpt[ipt];
	return;
}

/*********************************************************************
**    E_FUNCTION     : clclose(clfile)
**       FORTRAN callable routine to close and delete an internal
**       clfile.
**    PARAMETERS   
**       INPUT  : 
**          clfile  = 1 = Close primary clfile.  2 = Secondary.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void clclos(clfile)
UM_int2 *clfile;
{
	int ipt;
/*
.....Delete the clfile list
*/
	ipt = *clfile;
	if (UN_clfirst[ipt] != 0) uu_lsdel(UN_clfirst[ipt]);
	UN_clpt[ipt] = 0;
	UN_clfirst[ipt] = 0;
	UN_clnum[ipt] = 0;
	UN_clseq[ipt] = -1;
}

/*********************************************************************
**    E_FUNCTION     : clgetp(clfile,clpt)
**       FORTRAN callable routine that returns the pointer to the
**               next record in an internal clfile.
**    PARAMETERS   
**       INPUT  : 
**          clfile  = 1 = Primary clfile.  2 = Secondary.
**       OUTPUT :  
**          clpt    = Pointer to next record in internal clfile.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void clgetp(clfile,clpt)
UN_clstruc **clpt;
UM_int2 *clfile;
{
	int ipt;
/*
.....Return the clfile pointer
*/
	ipt = S_clptr(clfile);
   *clpt = UN_clpt[ipt];
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_swap_clfiles()
**       Swaps the primary and secondary internal clfiles.  Usually
**       used after coming back from an interactive clfile edit.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_swap_clfiles()
{
	int clnum,clseq,clseq_cur,i;
	char clseq_label[22];
	UN_clstruc *clpt,*clfirst,*clseq_rec,*clseq_end;

	clpt = UN_clpt[0];
	clfirst = UN_clfirst[0];
	clnum = UN_clnum[0];
	clseq = UN_clseq[0];
	clseq_cur = UN_clseq_cur[0];

	UN_clpt[0] = UN_clpt[1];
	UN_clfirst[0] = UN_clfirst[1];
	UN_clnum[0] = UN_clnum[1];
	UN_clseq[0] = UN_clseq[1];
	UN_clseq_cur[0] = UN_clseq_cur[1];

	UN_clpt[1] = clpt;
	UN_clfirst[1] = clfirst;
	UN_clnum[1] = clnum;
	UN_clseq[1] = clseq;
	UN_clseq_cur[1] = clseq_cur;

	for (i=0;i<MAXCLSEQ;i++)
	{
		clseq_rec = UN_clseq_rec[0][i];
		UN_clseq_rec[0][i] = UN_clseq_rec[1][i];
		UN_clseq_rec[1][i] = clseq_rec;

		clseq_end = UN_clseq_end[0][i];
		UN_clseq_end[0][i] = UN_clseq_end[1][i];
		UN_clseq_end[1][i] = clseq_end;

		strncpy(clseq_label,UN_clseq_label[0][i],20);
		strncpy(UN_clseq_label[0][i],UN_clseq_label[1][i],20);
		strncpy(UN_clseq_label[1][i],clseq_label,20);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : clstor(clfile,icpt,iclw,rdata)
**       FORTRAN callable routine to store data in an internal clfile.
**    PARAMETERS   
**       INPUT  : 
**          clfile  = 1 = Primary clfile.  2 = Secondary.
**			icpt    = Pointer inside clfile to store record.
**			iclw    = Clfile record integer data.
**			rdata   = Clfile record real data.
**       OUTPUT :  
**          icpt    = Pointer to next available record in internal
**			          clfile.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void clstor(clfile,icpt,iclw,rdata)
UN_clstruc **icpt;
UM_int2 *clfile;
UM_int4 iclw[];
UM_real8 rdata[];
{
	int i,inc,ipt;
	double *pd;
/*
.....Clfile is not open
*/
	ipt = S_clptr(clfile);
	if (UN_clpt[ipt] == 0) goto done;
/*
.....Increment cl record counter
*/
	UN_clnum[ipt]++;
/*
.....Save cl record header
*/
	if (UN_clpt[ipt] == 0)
	{
		ud_wrerr("Could not allocate memory for cl header record.");
		goto done;
	}
	UN_clpt[ipt]->isn[0] = iclw[0];
	UN_clpt[ipt]->isn[1] = iclw[1];
	UN_clpt[ipt]->clrec = UN_clnum[ipt];
	UN_clpt[ipt]->type = iclw[2];
	UN_clpt[ipt]->subt = iclw[3];
	UN_clpt[ipt]->mxcl = iclw[4];
/*
.....Save cl data
*/
	inc = iclw[4]; if (inc == 0) inc = 1;
	pd = (double *)uu_lsinsrt((char *)UN_clpt[ipt],sizeof(double)*inc);
	UN_clpt[ipt] = (UN_clstruc *)pd;
	if (UN_clpt[ipt] == 0)
	{
		ud_wrerr("Could not allocate memory for cl data record.");
		goto done;
	}
	if (iclw[4] != 0)
	{
		for (i=0;i<iclw[4];i++)
		{
#if UU_COMP == UU_VAXVMS
			ird = (UM_int2 *)&rdata[i];
			ipd = (UM_int2 *)pd;
			for (j=0;j<4;j++) ipd[j] = ird[j];
			pd++;
#else
			*pd++ = rdata[i];
#endif
		}
	}
/*
.....Allocate next clfile record
*/
	UN_clpt[ipt] = (UN_clstruc *)uu_lsinsrt((char *)UN_clpt[ipt],
		sizeof(UN_clstruc));
	*icpt = UN_clpt[ipt];
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : clread(clfile,icpt,iclw,rdata)
**       FORTRAN callable routine to read a record from an internal
**       clfile.
**    PARAMETERS   
**       INPUT  : 
**          clfile  = 1 = Primary clfile.  2 = Secondary.
**			icpt    = Pointer inside clfile of record to read.
**       OUTPUT :  
**          icpt    = Pointer to next record in internal clfile.
**                    Returns 0 if this is the last record.
**          iclw    = Clfile record integer data.
**          rdata   = Clfile record real data.
**          kerr    = 0 iff no error, 1 otherwise.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void clread(clfile,icpt,iclw,rdata,kerr)
UN_clstruc **icpt;
UM_int2 *clfile,*kerr;
UM_int4 iclw[];
UM_real8 rdata[];
{
	int i,ipt;
	UN_clstruc *ps;
	double *pd;
/*
.....Retrieve cl record header
*/
	*kerr = 0;
	ipt = S_clptr(clfile);
/*	i = *icpt; */
/*	if (i == -1) goto failed; */
/*	ps = (UN_clstruc *)i; */
	ps = *icpt;
	if (ps == 0 && UN_clfirst[ipt] != 0)
		ps = (UN_clstruc *)uu_lsnext(UN_clfirst[ipt]);
	if (ps == 0) goto failed;
	iclw[0] = ps->isn[0];
	iclw[1] = ps->isn[1];
	iclw[2] = ps->type;
	iclw[3] = ps->subt;
	iclw[4] = ps->mxcl;
	iclw[5] = ps->clrec;
/*
.....Kludge
.....Let C routines see long clfile record
.....Eventually should store long ints in output clfile
*/
	UN_clrec = ps->clrec;
/*
.....Retrieve cl data
*/
	pd = (double *)uu_lsnext(ps);
	if (pd == 0) goto failed;
	ps = (UN_clstruc *)pd;
	for (i=0;i<iclw[4];i++)
	{
#if UU_COMP == UU_VAXVMS
		ird = (UM_int2 *)&rdata[i];
		ipd = (UM_int2 *)pd;
		for (j=0;j<4;j++) ird[j] = ipd[j];
		pd++;
#else
		rdata[i] = *pd++;
#endif
	}
/*
.....Move pointer to next cl record
*/
	ps = (UN_clstruc *)uu_lsnext(ps);
	if (ps == UN_clpt[ipt]) ps = (UN_clstruc *)0;
	*icpt = ps;
	goto done;
/*
.....Failure
.....Return EOF
*/
failed:;
	*icpt = (UN_clstruc *)0;
	*kerr = 1;
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : clprev(clfile,icpt,iclw,rdata)
**       FORTRAN callable routine to read previous record from an
**       internal clfile.
**    PARAMETERS   
**       INPUT  : 
**          clfile  = 1 = Primary clfile.  2 = Secondary.
**			icpt    = Pointer inside clfile.  The record read will be
**			          the one before the record pointed to by 'icpt'.
**       OUTPUT :  
**          icpt    = Returns the record number of the actual record
**                    read.
**			iclw    = Clfile record integer data.
**			rdata   = Clfile record real data.
**       kerr    = 0 iff no error, 1 otherwise.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void clprev(clfile,icpt,iclw,rdata,kerr)
UM_int2 *clfile, *kerr;
UM_int4 iclw[];
UN_clstruc **icpt;
UM_real8 rdata[];
{
	int i,ipt;
	UN_clstruc *ps;
	double *pd;
/*
.....Move pointer to previous cl record
*/
	*kerr = 0;
	ipt = S_clptr(clfile);
/*	i = *icpt; */
/* ps = (UN_clstruc *)i; */
	ps = *icpt;
	if (ps == 0) ps = UN_clpt[ipt];
	if (ps == UN_clfirst[ipt]) goto failed;
	pd = (double *)uu_lsprev(ps);
	if (pd == (double *)UN_clfirst[ipt] || pd == 0) goto failed;
	ps = (UN_clstruc *)uu_lsprev(pd);
	if (ps == 0) goto failed;
/*
.....Retrieve cl record header
*/
	iclw[0] = ps->isn[0];
	iclw[1] = ps->isn[1];
	iclw[2] = ps->type;
	iclw[3] = ps->subt;
	iclw[4] = ps->mxcl;
	iclw[5] = ps->clrec;
/*
.....Kludge
.....Let C routines see long clfile record
.....Eventually should store long ints in output clfile
*/
	UN_clrec = ps->clrec;
/*
.....Retrieve cl data
*/
	for (i=0;i<iclw[4];i++)
	{
		rdata[i] = *pd++;
	}
/*
.....Move pointer to this cl record
*/
	*icpt = ps;
	goto done;
/*
.....Failure
.....Return EOF
*/
failed:;
	*icpt = (UN_clstruc *)0;
	*kerr = 1;
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : cldel(clfile,icpt,iept)
**       FORTRAN callable routine to delete a range of records from an
**       internal clfile.
**    PARAMETERS   
**       INPUT  : 
**          clfile  = 1 = Primary clfile.  2 = Secondary.
**			icpt    = Beginning record of range to delete.
**			iept    = Ending record of range to delete.
**       OUTPUT :  
**          inxt    = Next available cl record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void cldel(clfile,icpt,iept,inxt)
UM_int2 *clfile;
UN_clstruc **icpt,**iept,**inxt;
{
	int ipt;
	UN_clstruc *ps,*pd,*pe;
/*
.....Retrieve cl record header
*/
	ipt = S_clptr(clfile);
	pd = *icpt;
	pe = *iept;
	if (pe == 0) pe = UN_clpt[ipt];
	ps = pd;
	if (ps)
	{
		while (ps != pe)
		{
			pd = (UN_clstruc *)uu_lsnext(ps);
			if (ps == UN_clpt[ipt]) goto done;
			ps->type = 0;
			ncl_mot_stack_cldel(ps,UN_clpt[ipt]);
			if (pd == 0) goto done;
			ps = (UN_clstruc *)uu_lsnext(pd);
		}
	}
done:;
	*inxt = UN_clpt[ipt];
	return;
}
/*********************************************************************
**
**      FUNCTION:  clstor_insert
**
**      PURPOSE:   Changes cl records and inserts the new fillet
**                 records
**                 ICPT is the initial record of the new fillet, it
**                 will be altered to contain the starting point of 
**                 the fillet.
**                 ICPT2 is the last record of the new fillet. 
**                 ICLW1 and RDATA1 is the initial record with the 
**                 changed point(s).  
**                 ICLW2,ICLW5,RDATA2,and RDATA5 are used for feedrates
**                 they may be empty.
**                 ICLW3 and RDATA3 is used for a circular record if 
**                 one exists.
**                 ICLW4 and RDATA4 has the fillet information.  If we
**                 are here, there should most certainly be information
**                 in these two arrays. 
**                 ICLW6 and RDATA6 is the data for the last record, it
**                 also has been altered, so changes will need to be made.
**                 Added 4/14/99 JLS
**
*********************************************************************/
void clstor_insert(clfile,icpt,iclw1,rdata1,iclw2,rdata2,iclw3,rdata3,
					iclw4,rdata4,iclw5,rdata5,iclw6,rdata6,icpt2)
UN_clstruc **icpt,**icpt2;
UM_int2 *clfile;
UM_int4 iclw1[],iclw2[],iclw3[],iclw4[],iclw5[],iclw6[];
UM_real8 rdata1[],rdata2[],rdata3[],rdata4[],rdata5[],rdata6[];
{
	int i,k,inc,ipt;
	double *pd,*pc;
	UN_clstruc *ps,*ps2,*psc;
	int start_rec;
	int numwds,mxcl,continuation;
	UU_LOGICAL lend;
/*
.....Delete all 5xxx Records
.....Between start and end records
.....Also get rid of any 3000 Records
.....whose motion record is being destroyed
*/
	ps = *icpt;
	ps = (UN_clstruc *)uu_lsnext(ps);
	ps = (UN_clstruc *)uu_lsnext(ps);
	ps2 = *icpt2;
	if (ps != ps2)
	{
		do
		{
			if (ps->type >= 5000 && ps->type < 6000)
			{
				ps = (UN_clstruc *)uu_lsdele(ps);
				ps = (UN_clstruc *)uu_lsdele(ps);
			}
			else
			{
				ps = (UN_clstruc *)uu_lsnext(ps);
				ps = (UN_clstruc *)uu_lsnext(ps);
			}
		} while (ps != ps2);
	}
/*
.....Get the information about the first record and set start_rec
.....this record's record number.
*/
	ps = *icpt;
	start_rec = ps->clrec;
/*
.....Get the information about the last record.
*/
	ps2 = *icpt2;
	ipt = S_clptr(clfile);
/*
.....If the CLfile is not open, exit.
*/
	if (UN_clpt[ipt] == 0) goto done;

	numwds = iclw1[4];
	mxcl = ps->mxcl;
	if (mxcl > numwds) mxcl = numwds;
	numwds -= mxcl;
/*
.....Change the header record to the new information.  Most likely
.....the only thing that will be different is mxcl.
*/
	ps->isn[0] = iclw1[0];
	ps->isn[1] = iclw1[1];
	ps->clrec = UN_clnum[ipt];
	ps->type = iclw1[2];
	ps->subt = iclw1[3];
	ps->mxcl = mxcl;
/*
.....Replace the first record's cldata
*/
	pd = (double *)uu_lsnext(ps);
	if (pd == 0)
	{
		ud_wrerr("Could not allocate memory for cl data record.");
		goto done;
	}
	pc = pd;
	if (iclw1[4] != 0)
	{
		for (i=0;i<mxcl;i++)
		{
#if UU_COMP == UU_VAXVMS
			ird = (UM_int2 *)&rdata1[i];
			ipd = (UM_int2 *)pd;
			for (j=0;j<4;j++) ipd[j] = ird[j];
			pd++;
#else
			*pd++ = rdata1[i];
#endif
		}
	}
/*
.....If new record is bigger than old, create continuation records containing
.....remaining data.
*/
	k = i;
	while (numwds > 0)
	{
		mxcl = numwds;
		if (mxcl > 126) mxcl = 126;
		ps = (UN_clstruc *)uu_lsinsrt((char *)pc,sizeof(UN_clstruc));
		*icpt = ps;
		ps->isn[0] = iclw1[0];
		ps->isn[1] = iclw1[1];
		ps->clrec = UN_clnum[ipt];
		ps->type = iclw1[2];
		ps->subt = 6;
		ps->mxcl = mxcl;
/*
.....Save cl data
*/
		inc = mxcl; if (inc == 0) inc = 1;
		pd = (double *)uu_lsinsrt((char *)ps,sizeof(double)*inc);
		pc = pd;
		if (pd == 0)
		{
			ud_wrerr("Could not allocate memory for cl data record.");
			goto done;
		}
		for (i=0;i<mxcl;i++)
		{
#if UU_COMP == UU_VAXVMS
			ird = (UM_int2 *)&rdata1[k];
			ipd = (UM_int2 *)pd;
			for (j=0;j<4;j++) ipd[j] = ird[j];
			pd++;
#else
			*pd++ = rdata1[k];
#endif
			k++;
		}
		numwds = numwds - mxcl;
		UN_clnum[ipt] ++;
	}
/*
.....Insert the feed rate record if it exists.
*/
	if (iclw2[2] !=0)
	{
		ps = (UN_clstruc *)uu_lsinsrt((char *)pc,sizeof(UN_clstruc));
		ps->isn[0] = iclw2[0];
		ps->isn[1] = iclw2[1];
		ps->clrec = UN_clnum[ipt];
		ps->type = iclw2[2];
		ps->subt = iclw2[3];
		ps->mxcl = iclw2[4];
/*
.....Save cl data
*/
		inc = iclw2[4]; if (inc == 0) inc = 1;
		pd = (double *)uu_lsinsrt((char *)ps,sizeof(double)*inc);
		pc = pd;
		if (pd == 0)
		{
			ud_wrerr("Could not allocate memory for cl data record.");
			goto done;
		}
		if (iclw2[4] != 0)
		{
			for (i=0;i<iclw2[4];i++)
			{
#if UU_COMP == UU_VAXVMS
				ird = (UM_int2 *)&rdata2[i];
				ipd = (UM_int2 *)pd;
				for (j=0;j<4;j++) ipd[j] = ird[j];
				pd++;
#else
				*pd++ = rdata2[i];
#endif
			}
		}
		UN_clnum[ipt] ++;
	}
/*
.....If a circular record exists, insert it.
*/
	if (iclw3[2]!=0)
	{
		ps = (UN_clstruc *)uu_lsinsrt((char *)pc,sizeof(UN_clstruc));
		ps->isn[0] = iclw3[0];
		ps->isn[1] = iclw3[1];
		ps->clrec = UN_clnum[ipt];
		ps->type = iclw3[2];
		ps->subt = iclw3[3];
		ps->mxcl = iclw3[4];
/*
.....Save cl data
*/
		inc = iclw3[4]; if (inc == 0) inc = 1;
		pd = (double *)uu_lsinsrt((char *)ps,sizeof(double)*inc);
		pc = pd;
		if (pd == 0)
		{
			ud_wrerr("Could not allocate memory for cl data record.");
			goto done;
		}
		if (iclw3[4] != 0)
		{
			for (i=0;i<iclw3[4];i++)
			{
#if UU_COMP == UU_VAXVMS
				ird = (UM_int2 *)&rdata3[i];
				ipd = (UM_int2 *)pd;
				for (j=0;j<4;j++) ipd[j] = ird[j];
				pd++;
#else
				*pd++ = rdata3[i];
#endif
			}
		}
		UN_clnum[ipt] ++;
	}
/*
.....Insert the fillet points.
.....It is possible that there are more than 21 points, if so,
.....we need to create continuation records. Set continuation
.....to 5 for the first record.  If we circle through again it
.....will be set to 6 to indicate that the record is a continuation
.....record.
*/
	continuation = 5;
/*
.....Determine the number of points in the fillet.
*/
	numwds = iclw4[4];
/*
.....Set i = 0 here because we might need to go through the 
.....for loop two or three times and we don't want it to start
.....at zero each time, just the first.
*/
	i = 0;
	while (numwds > 0)
	{
		mxcl = numwds;
/*
.....If there are more than 21 points, then only do the first 21 this
.....time through.
*/
		if (mxcl > 126) mxcl = 126;
		ps = (UN_clstruc *)uu_lsinsrt((char *)pc,sizeof(UN_clstruc));
		*icpt = ps;
		ps->isn[0] = iclw4[0];
		ps->isn[1] = iclw4[1];
		ps->clrec = UN_clnum[ipt];
		ps->type = iclw4[2];
		ps->subt = continuation;
		ps->mxcl = mxcl;
/*
.....Save cl data
*/
		inc = mxcl; if (inc == 0) inc = 1;
		pd = (double *)uu_lsinsrt((char *)ps,sizeof(double)*inc);
		pc = pd;
		if (pd == 0)
		{
			ud_wrerr("Could not allocate memory for cl data record.");
			goto done;
		}
		mxcl = i + mxcl;
		if (mxcl != 0)
		{
			for (i=i;i<mxcl;i++)
			{
#if UU_COMP == UU_VAXVMS
				ird = (UM_int2 *)&rdata4[i];
				ipd = (UM_int2 *)pd;
				for (j=0;j<4;j++) ipd[j] = ird[j];
				pd++;
#else
				*pd++ = rdata4[i];
#endif
			}
		}
/*
.....Set continuation to 6 for the return through the loop.  
.....Decrease numwds by 126, if numwds is still greater than 
.....0, we will go back through the loop.
.....And last but not least increment UN_clnum[ipt].
*/
		continuation = 6;
		numwds = numwds -126;
		UN_clnum[ipt] ++;
	}
/*
.....Insert the feed rate record if it exists.   
*/
	if (iclw5[2]!=0)
	{
		ps = (UN_clstruc *)uu_lsinsrt((char *)pc,sizeof(UN_clstruc));
		ps->isn[0] = iclw5[0];
		ps->isn[1] = iclw5[1];
		ps->clrec = UN_clnum[ipt];
		ps->type = iclw5[2];
		ps->subt = iclw5[3];
		ps->mxcl = iclw5[4];
/*
.....Save cl data
*/
		inc = iclw5[4]; if (inc == 0) inc = 1;
		pd = (double *)uu_lsinsrt((char *)ps,sizeof(double)*inc);
		pc = pd;
		if (pd == 0)
		{
			ud_wrerr("Could not allocate memory for cl data record.");
			goto done;
		}
		if (iclw5[4] != 0)
		{
			for (i=0;i<iclw5[4];i++)
			{
#if UU_COMP == UU_VAXVMS
				ird = (UM_int2 *)&rdata5[i];
				ipd = (UM_int2 *)pd;
				for (j=0;j<4;j++) ipd[j] = ird[j];
				pd++;
#else
				*pd++ = rdata5[i];
#endif
			}
		}
		UN_clnum[ipt] ++;
	}
/*
.....We are now at the location of the last record, put the new 
.....information into the header file.  clrec will remain the same.
*/
	ps2->isn[0] = iclw6[0];
	ps2->isn[1] = iclw6[1];
	ps2->type = iclw6[2];
	ps2->subt = iclw6[3];
	ps2->mxcl = iclw6[4];
/*
.....Insert the new cldata.
*/
	inc = iclw6[4]; if (inc == 0) inc = 1;
	pd = (double *)uu_lsnext(ps2);
	pc = pd;
	if (pd == 0)
	{
		ud_wrerr("Could not allocate memory for cl data record.");
		goto done;
	}
	if (iclw6[4] != 0)
	{
		for (i=0;i<iclw6[4];i++)
		{
#if UU_COMP == UU_VAXVMS
			ird = (UM_int2 *)&rdata6[i];
			ipd = (UM_int2 *)pd;
			for (j=0;j<4;j++) ipd[j] = ird[j];
			pd++;
#else
			*pd++ = rdata6[i];
#endif
		}
	}
/*
.....Delete all unnecessary type 3000 Records
.....Usually due to a circular record
.....being dropped during ARCSLP/FILLET,COMBIN
*/
	ps = *icpt;
	ps = (UN_clstruc *)uu_lsnext(ps);
	ps = (UN_clstruc *)uu_lsnext(ps);
	ps2 = *icpt2;
	psc = (UN_clstruc *)UU_NULL;
	lend = UU_FALSE;
	if (ps != ps2)
	{
		do
		{
			if (ps == ps2) lend = UU_TRUE;
			if (ps->type >= 5000 && ps->type < 6000)
			{
				if (psc != (UN_clstruc *)UU_NULL && ps->isn[0] != psc->isn[0])
				{
					psc = (UN_clstruc *)uu_lsdele(psc);
					psc = (UN_clstruc *)uu_lsdele(psc);
				}
				ps = (UN_clstruc *)uu_lsnext(ps);
				ps = (UN_clstruc *)uu_lsnext(ps);
				psc = (UN_clstruc *)UU_NULL;
			}
			else
			{
				if (psc != (UN_clstruc *)UU_NULL)
				{
					psc = (UN_clstruc *)uu_lsdele(psc);
					psc = (UN_clstruc *)uu_lsdele(psc);
				}
				if (ps->type == 3000) psc = ps;
				else psc = (UN_clstruc *)UU_NULL;
				ps = (UN_clstruc *)uu_lsnext(ps);
				ps = (UN_clstruc *)uu_lsnext(ps);
			}
		} while (!lend);
	}
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : inirvl
**      Initializes the clfile reversal lists.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void inirvl()
{
	int i;
/*
....Initialize clfile reversal lists
*/
	for (i=0;i<NREVLISTS;i++)
	{
		rev_nl[i] = 0;
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : finrvl
**      Destroys the clfile reversal lists.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void finrvl()
{
	int i;
/*
....Destroy clfile reversal lists
*/
	for (i=0;i<NREVLISTS;i++)
	{
		if (rev_nl[i] != 0) uu_list_free(&rev_list[i]);
		rev_nl[i] = 0;
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : pshrvl (icpt,istk,nstk,which)
**      Pushes a cl record pointer onto a stack during reversal of the
**      clfile.  There can be up to 10 stacks active (circular, fedrat,
**      cutcom, etc.).
**    PARAMETERS   
**       INPUT  : 
**          icpt    = Cl record number to push onto stack.
**          istk    = Which stack to update (1-10).
**          which   = 1 = New entry on stack.  2 = Update final cl
**                    record number for previous entry on stack.
**       OUTPUT :  
**          nstk    = Incremented when 'nstk = 1'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void pshrvl (icpt,istk,nstk,which)
UN_clstruc **icpt;
UM_int4 *istk,*nstk,*which;
{
	int ipt;
	REV_list_struc rev;
/*
.....Initialize routine
*/
	ipt = *istk - 1;
/*
.....Initialize stack
*/
	if (rev_nl[ipt] == 0)
	{
		uu_list_init(&rev_list[ipt],sizeof(REV_list_struc),50,50);
	}
/*
.....Create new entry on stack
*/
	if (*which == 1)
	{
		rev.begin = *icpt;
		rev.end = (UN_clstruc *)UU_NULL;
		uu_list_push(&rev_list[ipt],&rev);
		rev_nl[ipt]++;
		*nstk = rev_nl[ipt];
	}
/*
.....Update last entry on stack
*/
	else
	{
		uu_list_pop(&rev_list[ipt],&rev);
		rev.end = *icpt;
		uu_list_push(&rev_list[ipt],&rev);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : poprvl (istk,nstk,ibeg,iend)
**      Pops a cl record pointer from a stack during reversal of the
**      clfile.  There can be up to 10 stacks active (circular, fedrat,
**      cutcom, etc.).
**    PARAMETERS   
**       INPUT  : 
**          istk    = Which stack to retrieve from (1-10).
**       OUTPUT :  
**          nstk    = Decremented by 1.
**          ibeg    = Starting cl record number.
**          iend    = Ending cl record number.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void poprvl (istk,nstk,ibeg,iend)
UM_int4 *istk,*nstk;
UN_clstruc **ibeg,**iend;
{
	int ipt;
	REV_list_struc rev;
/*
.....Initialize routine
*/
	ipt = *istk - 1;
/*
.....No entries on stack
*/
	if (rev_nl[ipt] == 0)
	{
		*ibeg = UU_NULL;
		*iend = UU_NULL;
	}
/*
.....Pop entry from stack
*/
	else
	{
		uu_list_pop(&rev_list[ipt],&rev);
		*ibeg = rev.begin;
		*iend = rev.end;
		rev_nl[ipt]--;
		*nstk = rev_nl[ipt];
/*
.....End of stack
.....Delete it
*/
		if (rev_nl[ipt] == 0) uu_list_free(&rev_list[ipt]);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_temp_clfile (icpt,ocpt,ncpt)
**      Initializes (opens) the temporary working clfile and copies
**      the clfile records from the main clfile, starting with the
**      requested clfile record, into it.
**    PARAMETERS   
**       INPUT  : 
**          icpt    = Pointer to first record inside main clfile to 
**                    start copying from.
**       OUTPUT :  
**          ocpt    = Pointer to first record inside temporary clfile.
**          ncpt    = Pointer to next available record inside temporary
**                    clfile.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_temp_clfile(icpt,ocpt,ncpt)
UN_clstruc **icpt,**ocpt,**ncpt;
{
	UM_int2 iclf0,iclf2,jerr;
	UM_int4 iclw[6];
	UM_real8 rclw[420];
	UN_clstruc *irec;
/*
.....Initialize routine
*/
	iclf0 = 0;
	iclf2 = 2;
/*
.....Open temporary clfile
*/
	clopen(&iclf2,ocpt);
	*ncpt = *ocpt;
/*
.....Copy clfile
*/
	irec = *icpt;
/*
.....Store the location of the first entry in the temp cl file and
.....the first entry copied to the temp cl file
.......These locations are used when the two lists are made into one
*/
	Sread = *icpt;
	Sstor = *ocpt;
/*	ncpt = ocpt;*/
/*
.....find the last motion goto point (if have any) 
.....to save as first motion point of temp file per Ken request
.....when reset and link to the main CL file
.....remember don't save this record again
*/
	Slast_saved = 0;
	irec = UN_clpt[iclf0];
	if (irec==UU_NULL)
		goto done;
	while (irec!=UU_NULL)
	{
		irec = (UN_clstruc *)uu_lsprev(irec);
		if (irec==UU_NULL)
			goto done;
		irec = (UN_clstruc *)uu_lsprev(irec);
		if ((irec->type==5000)||(irec->type==5200))
		{
			clread(&iclf0,&irec,iclw,rclw,&jerr);
			if (jerr != 0)
				goto done;
			iclw[3] = 3;
			clstor(&iclf2,ncpt,iclw,rclw);
			Slast_saved = 1;
			break;
		}
	}
done:;
/*
.....Set temporary clfile flag
*/
	Stemp_flag = UU_TRUE;
/*
......if there is no motion point define so far from main clfile
......set the start point as current cutter position
*/
/*
......do not add the start point if there is no motion
......
	if (Slast_saved==0)
	{
		wrttpos_cl();
		Slast_saved = 1;
	}
*/
}

/*********************************************************************
**    E_FUNCTION     : ncl_reset_temp_clfile ()
**      Closes the temporary working clfile and resets the flag.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_reset_temp_clfile(flag)
UU_LOGICAL *flag;
{
	UN_clstruc *ocpt;
	UM_int2 iclf0,iclf2;
/*
.....Initialize routine
*/
	iclf2 = 2;
/*
.....Reset temporary clfile flag
*/
	Stemp_flag = UU_FALSE;
/*
.....Close temporary clfile
*/
	if (!*flag) clclos(&iclf2);
/*
.....Append temp cl file data on end of primary cl file
*/
	else
	{
		iclf0 = 0;
		ocpt = Sread;
/*
.....Move Sread to the previous entry in the list so we can append
.....the temp clfile to this location after we delete what follows it
*/
		Sread = (UN_clstruc *)uu_lsprev(Sread);
/*
.....Delete all entries from the primary cl file that were copied over
.....to the temporary cl file
*/
		do
		{
/*
.....don't delete the first one
*/
			if (UN_clfirst[0]!=ocpt)
				ocpt = (UN_clstruc *)uu_lsdele(ocpt);
			else
				break;
		} while (ocpt != UU_NULL);
/*
.....Update pointer to last entry
*/
		UN_clpt[0] = UN_clpt[2];
/*
.....Append the temp cl file data and free the header memory
*/
		if (Sread==UU_NULL)
		{
/*
.....the primary cl file list is empty, use the first entity
*/
			Sread = UN_clfirst[0];
		}
/*
.....if we saved the last motion point from the main clfile,
.....delete it from temp cl file before add this temp cl record to'
.....main cl file record
*/
		if ((Sstor!=UU_NULL)&&(Slast_saved))
		{
			Sstor = (UN_clstruc *)uu_lsdele(Sstor);
			if (Sstor!=UU_NULL)
				Sstor = (UN_clstruc *)uu_lsdele(Sstor);
		}
		uu_lsapndl(Sstor,Sread,UN_clpt[2],UN_clfirst[0]);
/*
.....Set the header to null so the file is seen as closed
.......The header memory is freed in the uu_laspnd routine
*/
		UN_clfirst[2] = 0;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_pause_temp_clfile ()
**      Temporarily disables the temporary clfile used for Previewing motion.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pause_temp_clfile()
{
/*
.....Reset temporary clfile flag
*/
	Stemp_save = Stemp_flag;
	Stemp_flag = UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION     : ncl_resume_temp_clfile ()
**      Reactivates a paused temporary clfile.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_resume_temp_clfile()
{
/*
.....Reset temporary clfile flag
*/
	Stemp_flag = Stemp_save;
}

/*********************************************************************
**    E_FUNCTION     : ncl_revers_clfile(clfile,ibpt,iept)
**       FORTRAN callable routine to reverse the order of the specified
**       clfile records.
**    PARAMETERS   
**       INPUT  : 
**          clfile  = 1 = Primary clfile.  2 = Secondary.
**          ibpt    = Starting clfile record to reverse.
**          iept    = Ending clfile record to reverse.
**       OUTPUT :  
**          ibpt    = New starting record for this range.
**          iept    = New ending record for this range.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     :
**          This routine is not currently used and does not handle
**          important things, such as feed rates, 52xx records,
**          multiple 1000 records, etc.
*********************************************************************/
void ncl_revers_clfile(clfile,ibpt,iept)
UM_int2 *clfile;
UN_clstruc **ibpt,**iept;
{
	int ipt;
	double *pds,*pde;
	UN_clstruc *ps,*pe,*pss,*pse;
/*
.....Initialize routine
*/
	ipt = S_clptr(clfile);
	ps = *ibpt;
	pe = *iept;
/*
.....Start pointer should be at current record
.....End pointer should be at last clfile record
*/
	if (ps == UN_clfirst[ipt]) ps = (UN_clstruc *)uu_lsnext(ps);
	if (pe == UN_clpt[ipt])
	{
		pde = (double *)uu_lsprev(pe);
		pe = (UN_clstruc *)uu_lsprev(pde);
	}
	pss = ps; pse = pe;
/*
.....Loop to swap records
*/
	while (ps != pe)
	{
		if (ps->type == 1000)
		{
			pds = (double *)uu_lsnext(ps);
			ps = (UN_clstruc *)uu_lsnext(pds);
		}
		pds = (double *)uu_lsnext(ps);
		pde = (double *)uu_lsnext(pe);
		uu_lsswap(ps,pe); uu_lsswap(pds,pde);
		pe = ps;
		ps = (UN_clstruc *)uu_lsnext(pde);
		if (ps == pe) break;
		pde = (double *)uu_lsprev(pe);
		pe = (UN_clstruc *)uu_lsprev(pde);
		if (ps == pe) break;
		if (ps == UN_clpt[ipt] || pe == UN_clfirst[ipt]) break;
	}
/*
.....Move pointer to this cl record
*/
	*ibpt = pse;
	*iept = pss;
	return;
}

/*********************************************************************
**    E_FUNCTION     : isn2i4(isn2,isn4)
**      Converts an Integer*2 to an Integer*4.  Negative I*2 numbers
**      will be converted to their appropriate positive I*4 numbers.
**      This routine is typically used to convert an ISN or CLREC value
**      from an external clfile to an I*4 value.
**    PARAMETERS   
**       INPUT  :
**          isn2    = Integer*2 value.
**       OUTPUT :
**          isn4    = Integer*4 value.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void isn2i4(isn2,isn4)
UM_int2 *isn2;
UM_int4 *isn4;
{
	union
	{
		UM_int2 ival[2];
		UM_int4 jval;
	} itmp;
/*
.....Convert number
*/
#if UU_COMP == UU_WIN2K
	itmp.ival[0] = *isn2;
	itmp.ival[1] = 0;
#else
	itmp.ival[0] = 0;
	itmp.ival[1] = *isn2;
#endif
	*isn4 = itmp.jval;
	return;
}

/*********************************************************************
**    E_FUNCTION     : isn4i2(isn4,isn2)
**      Converts an Integer*4 to an Integer*2.  Numbers larger than
**      65535 will be truncated to be <= 65535 and then any number
**      larger than 32767 will be converted to a negative (unsigned)
**      value.  This routine is typically used to convert an ISN or CLREC
**      value from an internal clfile to an I*2 value valid for output
**      to an external clfile.
**    PARAMETERS   
**       INPUT  :
**          isn4    = Integer*4 value.
**       OUTPUT :
**          isn2    = Integer*2 value.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void isn4i2(isn4,isn2)
UM_int4 *isn4;
UM_int2 *isn2;
{
	union
	{
		UM_int2 ival[2];
		UM_int4 jval;
	} itmp;
/*
.....Shorten to I*2 range
*/
	itmp.jval = *isn4;
	itmp.jval = itmp.jval - ((itmp.jval-1)/65535*65535);
/*
.....Convert number
*/
#if UU_COMP == UU_WIN2K
	*isn2 = itmp.ival[0];
#else
	*isn2 = itmp.ival[1];
#endif
	return;
}

/*********************************************************************
**    I_FUNCTION     : S_debug_clfile (name,type)
**      Creates an aptsrc file for the given cl file record type using
**      the name provided.  This routine allows the tracking of changes
**      made to the cl file.
**    PARAMETERS   
**       INPUT  : 
**          name  = aptsrc file name to use.
**          type  = type of cl record to read from
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_debug_clfile(str,type)
char str[];
int type;
{
	int i,j;
   UM_f77_str name;
   UM_int2 ival,ifl;
   UM_int4 ist[2],ien[2];
	UU_LOGICAL Ttemp_flag;
/*
.....Set cl file type so the correct flags are used when reading file
*/
	Ttemp_flag = Stemp_flag;
	if (type == 2) Stemp_flag = UU_TRUE;
	else Stemp_flag = UU_FALSE;
/*
.....Set aptsrc file name
*/
	UM_init_f77_str(name, str, UX_MAX_PATH_LEN);
   i = strlen(str);
   for (j=i; j<UX_MAX_PATH_LEN; j++) str[j] = ' ';
   ifl = 3;
   ival = type;
   ist[0] = ist[1] = ien[0] = ien[1] = 0;
/*
.....Read cl file data and store in aptsrc file
*/
   aptsrc(&ifl,UM_addr_of_f77_str(name),&ival,ist,ien);

	Stemp_flag = Ttemp_flag;
}

/*********************************************************************
**    I_FUNCTION     : S_clptr (clfile)
**      Determines if the temporary clfile is being used and returns
**      the actual clfile pointer to use.  If the temporary is acvtive and
**      the main clfile is being accessed, then the temporary clfile
**      pointer is returned, otherwise the input clfile pointer is
**      returned.
**    PARAMETERS   
**       INPUT  : 
**          clfile  = Clfile pointer requested to be used.
**       OUTPUT :  
**          nstk    = Decremented by 1.
**          ibeg    = Starting cl record number.
**          iend    = Ending cl record number.
**    RETURNS      : The clfile pointer to use.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_clptr(clfile)
UM_int2 *clfile;
{
	int clp;
	if (Stemp_flag && *clfile == 0) clp = 2;
	else clp = *clfile;
	return(clp);
}
