/*********************************************************************
**    NAME         :  neifelse.c
**       CONTAINS:  Routines to support if-then-else statements.
**
**     void ncl_ifstor (nline)
**     void ncl_ifpush (nlif,nline,endflg,kerr)
**     void ncl_ifgtln(ifline,nline,nlif,kerr)
**     void ncl_ifgtend(ifline,nline,kerr)
**     void ncl_fixif(numlns, linen)
**     void ncl_ifinit()
**     void ncl_ifunmatched(lstart,lend,lnum)
**     void ncl_iflabchk(lnum,kerr)
**     void ncl_ifreset(lstart,lend,ifindx)
**     ncl_ifget_arrays()
**     ncl_ifset_arrays()
**
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neifelse.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:35
*********************************************************************/

#include "ncldef.h"
#include "nclfc.h"

static UU_LIST NCL_ifthenelse_list;
static UU_LIST NCL_endif_list;
static int NCL_ifinit = 0;

/*********************************************************************
**    FUNCTION     : void ncl_ifstor
**      Store ncl if-then-else data in static list.
**    PARAMETERS
**       INPUT  :
**          nline  - Part program line number.
**       OUTPUT :
**          none
**    RETURNS      :
**         none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ifstor (nline)
UM_int4 *nline;
{
	int j, k, n, *jp;
	UU_LIST lst, *lstp;

	if (NCL_ifinit == 0)
	{
		NCL_ifinit = 1;
		uu_list_init(&NCL_ifthenelse_list,sizeof(UU_LIST),100,100);
		uu_list_init(&NCL_endif_list,sizeof(int),100,100);
	}

	n = UU_LIST_LENGTH(&NCL_ifthenelse_list);
	lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
	for (j=0;j<n;j++,lstp++)
	{
		jp = (int *)UU_LIST_ARRAY(lstp);
		if (*jp == *nline) break;
	}
	if (j<n)
	{
		UU_LIST_EMPTY(lstp);
		jp = (int *)UU_LIST_ARRAY(&NCL_endif_list);
		jp[j] = 0;
	}
	else
	{
		uu_list_init(&lst,sizeof(int),10,10);
		uu_list_push(&NCL_ifthenelse_list,&lst);
		lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list) + n;
		k = 0;
		uu_list_push(&NCL_endif_list,&k);
	}
	uu_list_push(lstp,nline);

	return;
}

/*********************************************************************
**    FUNCTION     : void ncl_ifpush
**      Push elseif, else or endif line number onto if list.
**    PARAMETERS
**       INPUT  :
**          nlif    - Part program line number of corresponding
**                   if statement
**          nline   - Part program line number of this statement.
**          endflg  - 1 if endif, else 0
**       OUTPUT :
**          kerr    - 1 if error; else 0
**    RETURNS      :
**         none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ifpush (nlif,nline,endflg,kerr)
UM_int4 *nlif, *nline, *endflg, *kerr;
{
	int j, n, *jp;
	UU_LIST *lstp;

	*kerr = 0;
	if (NCL_ifinit == 0)
	{
		*kerr = 1;
		return;
	}

	n = UU_LIST_LENGTH(&NCL_ifthenelse_list);
	lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
	for (j=0;j<n;j++,lstp++)
	{
		jp = (int *)UU_LIST_ARRAY(lstp);
		if (*jp == *nlif) break;
	}
	if (j<n)
	{
		uu_list_push(lstp,nline);
		if (*endflg)
		{
			jp = (int *)UU_LIST_ARRAY(&NCL_endif_list);
			jp[j] = *nline;
		}
	}
	else
	{
		*kerr = 1;
	}

	return;
}

/*********************************************************************
**    FUNCTION     : void ncl_ifgtln
**      Return the part program line number of the next elseif, else
**      or endif of an if statement.
**    PARAMETERS
**       INPUT  :
**          ifline - Line number of the if statement.
**          nline  - Line number of the elseif or else statement.
**       OUTPUT :
**          nlif   - Line number of the next elseif, else or endif
**                   statement.
**          kerr   - 1 if error; else 0
**    RETURNS      :
**         none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ifgtln(ifline,nline,nlif,kerr)
UM_int4 *ifline, *nline, *nlif, *kerr;
{
	int j, k, n1, n2, *jp;
	UU_LIST *lstp;

	*kerr = 0;
	if (NCL_ifinit == 0)
	{
		*kerr = 1;
		return;
	}

	n1 = UU_LIST_LENGTH(&NCL_ifthenelse_list);
	lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
	for (j=0;j<n1;j++,lstp++)
	{
		jp = (int *)UU_LIST_ARRAY(lstp);
		if (*jp == *ifline) break;
	}
	if (j<n1)
	{
		n2 = UU_LIST_LENGTH(lstp);
		for (k=0;k<n2-1;k++,jp++)
			if (*jp==*nline)
			{
				*nlif = *(jp+1);
				break;
			}
		if (k >= n2-1) 
		{
			jp = (int *)UU_LIST_ARRAY(&NCL_endif_list);
			*nlif = jp[j];
			if (*nlif == 0) *kerr = 1;
		}
	}
	else
	{
		*kerr = 1;
	}

	return;
}
/*********************************************************************
**    FUNCTION     : void ncl_ifgtend
**      Return the part program line number of the endif of an if 
**      statement.
**    PARAMETERS
**       INPUT  :
**          ifline - Line number of the if statement.
**       OUTPUT :
**          nline  - Line number of the endif statement.
**          kerr   - 1 if error; else 0
**    RETURNS      :
**         none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ifgtend(ifline,nline,kerr)
UM_int4 *ifline, *nline, *kerr;
{
	int j, n, *jp;
	UU_LIST *lstp;

	*kerr = 0;
	if (NCL_ifinit == 0)
	{
		*kerr = 1;
		return;
	}

	n = UU_LIST_LENGTH(&NCL_ifthenelse_list);
	lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
	for (j=0;j<n;j++,lstp++)
	{
		jp = (int *)UU_LIST_ARRAY(lstp);
		if (*jp == *ifline) break;
	}
	if (j<n)
	{
		jp = (int *)UU_LIST_ARRAY(&NCL_endif_list);
		*nline = jp[j];
	}
	else
	{
		*kerr = 1;
	}

	return;
}
/*********************************************************************
**    FUNCTION     : void ncl_fixif
**      Fix the if-then-else list for lines inserted into or deleted from
**      the part program file.
**    PARAMETERS
**       INPUT  :
**          numlns - Number of lines inserted or deleted.
**          linen  - Line number where insertion or deletion occurred.
**       OUTPUT :
**          none.
**    RETURNS      :
**         none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fixif(numlns, linen)
UM_int4 *numlns, *linen;
{
	int j, k, n1, n2, *jp;
	UU_LIST *lstp;

	if (NCL_ifinit == 0)
	{
		return;
	}

	n1 = UU_LIST_LENGTH(&NCL_ifthenelse_list);
	lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
	for (j=0;j<n1;j++,lstp++)
	{
		jp = (int *)UU_LIST_ARRAY(lstp);
		n2 = UU_LIST_LENGTH(lstp);
		for (k=0;k<n2;k++,jp++)
		{
			if (*jp >= *linen) *jp += *numlns;
		}
	}
	jp = (int *)UU_LIST_ARRAY(&NCL_endif_list);
	n2 = UU_LIST_LENGTH(&NCL_endif_list);
	for (k=0;k<n2;k++,jp++)
		if (*jp >= *linen) *jp += *numlns;

	return;
}
/*********************************************************************
**    FUNCTION     : void ncl_ifinit
**      Initialize if-then-else.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      :
**         none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ifinit()
{
	int j, n1;
	UU_LIST *lstp;

	if (NCL_ifinit == 0)
	{
		return;
	}

	n1 = UU_LIST_LENGTH(&NCL_ifthenelse_list);
	lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
	for (j=0;j<n1;j++,lstp++)
	{
		uu_list_free(lstp);
	}
	uu_list_free(&NCL_ifthenelse_list);
	uu_list_free(&NCL_endif_list);
	NCL_ifinit = 0;

	return;
}
/*********************************************************************
**    FUNCTION     : void ncl_ifunmatched(lstart,lend,lnum)
**      Determine if there are any if-thens without matching endif.
**    PARAMETERS
**       INPUT  :
**          lstart - Start of line number range to test.
**          lend   - End of line number range to test.
**       OUTPUT :
**          lnum   - Line number of unmatched if or 0
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ifunmatched(lstart,lend,lnum)
UM_int4 *lstart, *lend, *lnum;
{
	int j, n1, *jp, *kp,blin,elin;
	UU_LIST *lstp;

	*lnum = 0;
	if (NCL_ifinit == 0)
	{
		return;
	}

	n1 = UU_LIST_LENGTH(&NCL_ifthenelse_list);
	lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
	kp = (int *)UU_LIST_ARRAY(&NCL_endif_list);
	for (j=0;j<n1;j++,lstp++)
	{
		jp = (int *)UU_LIST_ARRAY(lstp);
		nclf_src_rec_to_line(jp,&blin);
		nclf_src_rec_to_line(&kp[j],&elin);
		if (blin >= *lstart && blin <= *lend && elin == 0)
		{
			*lnum = blin;
			break;
		}
	}

	return;
}
/*********************************************************************
**    FUNCTION     : void ncl_iflabchk(nline,lnum,kerr)
**      Check if a line number is in an if-then-else range
**    PARAMETERS
**       INPUT  :
**          nline  - Current line number.
**          lnum   - Destination line number.
**       OUTPUT :
**          ifindx - Index of currently executing if statement.
**          kerr   - 1 if line number is in if-then-else range, else 0
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_iflabchk(nline,lnum,ifindx,kerr)
UM_int4 *nline, *lnum, *ifindx;
UM_int2 *kerr;
{
	int j, k, n1, n2, *jp, *kp, blin, elin;
	UU_LIST *lstp;

	*kerr = 0;
	if (NCL_ifinit == 0 || *ifindx == 0) return;

	n1 = UU_LIST_LENGTH(&NCL_ifthenelse_list);
	lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
	kp = (int *)UU_LIST_ARRAY(&NCL_endif_list);
	for (j=0;j<n1 && *kerr==0;j++,lstp++)
	{
		jp = (int *)UU_LIST_ARRAY(lstp);
		nclf_src_rec_to_line(jp,&blin);
		nclf_src_rec_to_line(&kp[j],&elin);
		if (*lnum >= blin && *lnum <= elin)
		{
			*kerr = 1;
			n2 = UU_LIST_LENGTH(lstp);
			for (k=0;k<n2-1;k++)
			{
				nclf_src_rec_to_line(&jp[k],&blin);
				nclf_src_rec_to_line(&jp[k+1],&elin);
				if (*nline > blin && *nline <= elin &&
				    *lnum  > blin && *lnum  <= elin)
				{
					*kerr = 0;
					break;
				}
			}
		}
	}
/*
.....Reduce the current if block count by the number of 
.....if blocks exited.
*/
	if (*kerr == 0)
	{
		lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
		for (j=0;j<n1 && *ifindx>0;j++,lstp++,kp++)
		{
			jp = (int *)UU_LIST_ARRAY(lstp);
			nclf_src_rec_to_line(jp,&blin);
			nclf_src_rec_to_line(kp,&elin);
			if (*nline > blin && *nline <= elin &&
			   (*lnum  < blin || *lnum  >  elin))
			{
				(*ifindx)--;
			}
		}
	}

	return;
}
/*********************************************************************
**    FUNCTION     : void ncl_ifreset(lstart,lend,ifindx)
**      Reset all if statements in a line number range.
**    PARAMETERS
**       INPUT  :
**          lstart - Start of line number range to test.
**          lend   - End of line number range to test.
**          ifindx - Index of currently executing if statement.
**       OUTPUT :
**          ifindx - Index of currently executing if statement updated.
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ifreset(lstart,lend,ifindx)
UM_int4 *lstart, *lend, *ifindx;
{
	int j, n1, *jp, *kp, blin, elin, ilin1, ilin2;
	UU_LIST *lstp;

	if (NCL_ifinit == 0 || *ifindx == 0) return;
	nclf_src_rec_to_line(lstart,&blin);
	nclf_src_rec_to_line(lend,&elin);

	n1 = UU_LIST_LENGTH(&NCL_ifthenelse_list);
	lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
	kp = (int *)UU_LIST_ARRAY(&NCL_endif_list);
	for (j=0;j<n1 && *ifindx>0;j++,lstp++,kp++)
	{
		jp = (int *)UU_LIST_ARRAY(lstp);
		nclf_src_rec_to_line(jp,&ilin1);
		nclf_src_rec_to_line(kp,&ilin2);
		if (blin <= ilin1 && blin <= ilin2 &&
		    elin >  ilin1 && elin >  ilin2)
		{
			(*ifindx)--;
		}
	}

	return;
}

/*********************************************************************
**    FUNCTION     : void ncl_ifget_arrays(which,lstp,ary,nent)
**      Returns the number of values and the actual values in the IF
**      and ENDIF arrays.
**    PARAMETERS
**       INPUT  :
**          which   = 0 = Return IF list, 1 = IF array, 2 = ENDIF values.
**          lstp    = IF list to return values from when 'which' = 1.
**       OUTPUT :
**          lstp    = IF list when 'which' = 0.
**          ary     = Requested array values when 'which' is not 0.
**          nent    = Number of values/lists returned in array.
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ifget_arrays(which,lstp,ary,nent)
int which,**ary,*nent;
UU_LIST **lstp;
{
/*
.....Determine which array to return values for
*/
	if (which == 0)
	{
		*nent = UU_LIST_LENGTH(&NCL_ifthenelse_list);
		*lstp = (UU_LIST *)UU_LIST_ARRAY(&NCL_ifthenelse_list);
	}
	else if (which == 1)
	{
		*nent = UU_LIST_LENGTH(*lstp);
		*ary = (int *)UU_LIST_ARRAY(*lstp);
	}
	else
	{
		*nent = UU_LIST_LENGTH(&NCL_endif_list);
		*ary = (int *)UU_LIST_ARRAY(&NCL_endif_list);
	}
	return;
}

/*********************************************************************
**    FUNCTION     : void ncl_ifset_arrays(which,ary,nent)
**      Returns the number of values and the actual values in the IF
**      and ENDIF arrays.
**    PARAMETERS
**       INPUT  :
**          which   = 1 = IF values, 2 = ENDIF values.
**          ary     = Array values.
**          nent    = Number of values/lists in array.
**       OUTPUT : none
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ifset_arrays(which,ary,nent)
int which,*ary,nent;
{
	UU_LIST lst;
/*
.....Initialize storage
*/
	if (NCL_ifinit == 0)
	{
		NCL_ifinit = 1;
		uu_list_init(&NCL_ifthenelse_list,sizeof(UU_LIST),100,100);
		uu_list_init(&NCL_endif_list,sizeof(int),100,100);
	}
/*
.....Store IF values
*/
	if (which == 1)
	{
		uu_list_init(&lst,sizeof(int),nent,10);
		uu_list_push_multiple(&lst,nent,ary);
		uu_list_push(&NCL_ifthenelse_list,&lst);
	}
/*
.....Store ENDIF values
*/
	else
	{
		uu_list_push_multiple(&NCL_endif_list,nent,ary);
	}
	return;
}
