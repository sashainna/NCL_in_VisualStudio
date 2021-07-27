/*********************************************************************
**    NAME         :  nesrcctl.c
**       CONTAINS:
**					ncl_srcctl_init()
**					ncl_srcctl_free()
**					ncl_srcctl_put()
**					ncl_srcctl_get()
**					ncl_srcctl_get_count()
**					ncl_srcctl_scan_init()
**					ncl_srcctl_scan()
**    COPYRIGHT 2009 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nesrcctl.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:52
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include	"lcom.h"
#include	"mfort.h"
#include	"nclfc.h"
#include	"uhash.h"

static int Smemsiz=0;
static int *Sline_mem=UU_NULL;
static UU_LOGICAL Sinit=UU_FALSE;
static UU_HASH_TBL Shash;

void ncl_srcctl_init();
void ncl_srcctl_free();

char *uu_hash_add();
char *uu_hash_get();

/*********************************************************************
**    E_FUNCTION :  ncl_srcctl_init()
**       Initializes the Source Control routines.  Currently this
**       consists of a table that keeps track of the part program line
**       numbers that generated the geometry and motion.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_srcctl_init()
{
/*
.....Free previous hash table
*/
	if (Sinit) ncl_srcctl_free();
/*
.....Initialize the source control storage
*/
	uu_hash_init(Shash,512);
	Sinit = UU_TRUE;
/*
.....End of routine
*/
done:
	return;
}

/*********************************************************************
**    E_FUNCTION :  ncl_srcctl_free()
**       Frees the Source Control storage.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_srcctl_free()
{
/*
.....Free the local storage
*/
	if (Sinit)
	{
		if (Smemsiz !=0)
		{
			uu_free(Sline_mem);
			Smemsiz = 0;
			Sline_mem = UU_NULL;
		}
/*
.....Delete the hash table
*/
		uu_hash_del(Shash);
	}
	Sinit = UU_FALSE;
/*
.....End of routine
*/
done:
	return;
}

/*********************************************************************
**    E_FUNCTION :  ncl_srcctl_put(key)
**       Stores the key along with the current line number stack in
**       the source control table.  The line number stack is created
**       using the currently active line number.
**    PARAMETERS   
**       INPUT  : 
**          key      Key of entity to store.
**       OUTPUT :  none
**    RETURNS      : UU_FAILURE if could not store line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_srcctl_put(key)
UU_KEY_ID key;
{
	int status,i,mxc,inc,*hptr;
	UM_int4 nline,type,indx;
	union {UU_REAL val; UM_int4 ival[2];} doindx;
/*
.....Initialize routine
*/
	status = 0;
/*
.....Get number of commands on stack
*/
	getxln_count(&mxc);
	if (mxc == 0) goto done;
/*
.....Initialize storage
*/
	if (!Sinit) ncl_srcctl_init();
/*
.....Allocate memory for command stack
*/
	if (mxc > Smemsiz)
	{
		if (Smemsiz != 0) uu_free(Sline_mem);
		Smemsiz = mxc < 10 ? 10 : mxc;
		Sline_mem = (int *)uu_malloc((sizeof(UU_REAL)+sizeof(int))*Smemsiz);
		if (Sline_mem == UU_NULL)
		{
			Smemsiz = 0;
			goto failed;
		}
	}
/*
.....Loop to get all commands on stack &
.....Store in source control table
*/
	indx = inc = 0;
	for (i=0;i<mxc;i++)
	{
		getxln (&nline,&doindx.val,&type,&indx);
		if (indx == -1) break;
		if (type == 2)
		{
			Sline_mem[inc++] = -nline;
			Sline_mem[inc++] = doindx.ival[0];
			Sline_mem[inc++] = doindx.ival[1];
		}
		else
			Sline_mem[inc++] = nline;
	}
/*
.....Store data in hash table
*/
	hptr = (int *)uu_hash_add(&Shash,key,(inc+2)*(sizeof(int)));
	if (hptr == UU_NULL) goto failed;
	hptr[0] = key;
	hptr[1] = inc;
	for (i=0;i<inc;i++) hptr[i+2] = Sline_mem[i];
	goto done;
/*
.....Failure storing source line
*/
failed:
	status = -1;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_srcctl_store(key,nent,data)
**       Stores the key along with the current line number stack in
**       the source control table.
**    PARAMETERS   
**       INPUT  : 
**          key      Key of entity to store.
**          nent     Number of entries in the line number stack.
**          data     The line number stack.
**       OUTPUT :  none
**    RETURNS      : UU_FAILURE if could not store line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_srcctl_store(key,nent,data)
UU_KEY_ID key;
int nent,*data;
{
	int status,i,*hptr;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Initialize storage
*/
	if (!Sinit) ncl_srcctl_init();
/*
.....Store data in hash table
*/
	hptr = (int *)uu_hash_add(&Shash,key,(nent+2)*(sizeof(int)));
	if (hptr == UU_NULL) goto failed;
	hptr[0] = key;
	hptr[1] = nent;
	for (i=0;i<nent;i++) hptr[i+2] = data[i];
	goto done;
/*
.....Failure storing source line
*/
failed:
	status = -1;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_srcctl_get(key,nline,ktype,dval,cstr,nc,indx)
**       Returns the line number along text of the source line that
**       generated the input geometry key.
**
**       This routine should be called repeatedly to return all source
**       lines in the calling stack.  Calling it once with 'indx'
**       set to 0 will return the current line number.
**    PARAMETERS   
**       INPUT  : 
**          key      Key of created entity.
**          indx     Should be set to 0 on initial call and will be
**                   adjusted from then on out.
**       OUTPUT :
**          nline    Line number of source line.
**          ktype    0 = Current line, 1 = Macro Call, 2 = DO Loop.
**          dval     DO loop index value when 'ktype' = 2.
**          cstr     Text of source line.
**          nc       Number of characters in 'cstr'.
**          indx     Updated to point to the next source line associated
**                   with the input geometry key.  Returns -1 if there
**                   are no more associated lines.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_srcctl_get(key,nline,ktype,dval,cstr,nc,indx)
UU_KEY_ID key;
int *nline,*ktype;
UU_REAL *dval;
char *cstr;
int *nc,*indx;
{
	int i,inc,*hptr,nent,ilin;
	union {UU_REAL val; UM_int4 ival[2];} doindx;
/*
.....Find source lines associated with geometry key
*/
	hptr = (int *)uu_hash_get(&Shash,key);
	if (hptr == UU_NULL) goto failed;
	if (*indx >= hptr[1]) goto failed;
/*
.....Find the requested entry
*/

	nent = hptr[1] + 2;
	inc = 2;
	for (i=0;i<=*indx;i++)
	{
		if (hptr[inc] < 0)
		{
			ilin = -hptr[inc];
			*ktype = 2;
			doindx.ival[0] = hptr[inc+1];
			doindx.ival[1] = hptr[inc+2];
			*dval = doindx.val;
			inc += 3;
		}
		else
		{
			ilin = hptr[inc];
			*ktype = 1;
			if (i == 0) *ktype = 0;
			inc++;
		}
		if (inc >= nent && i != *indx) goto failed;
	}
/*
.....Get actual line number & source statement
*/
	*indx += 1;
	ncl_getsrc_rec(ilin,nline,cstr,nc);
	goto done;
/*
.....Failure storing source line
*/
failed:
	*indx = -1;
/*
.....End of routine
*/
done:
	return;
}

/*********************************************************************
**    E_FUNCTION :  ncl_srcctl_get_count(key,nent)
**       Returns the number of lines in the call stack for the specified
**       key.
**
**    PARAMETERS   
**       INPUT  : 
**          key      Key of created entity.
**       OUTPUT :
**          nent     Number of lines in call stack.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_srcctl_get_count(key,nent)
UU_KEY_ID key;
int *nent;
{
	int *hptr;
/*
.....Find source lines associated with geometry key
*/
	*nent = 0;
	hptr = (int *)uu_hash_get(&Shash,key);
/*
.....Return the number of lines on call stack
*/

	if (hptr != UU_NULL) *nent = hptr[1];
}

/*********************************************************************
**    E_FUNCTION :  ncl_srcctl_scan_init()
**       Initializes scanning capabilities of the call stack hash table.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_FAILURE if the call stack table does not exist.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_srcctl_scan_init()
{
/*
.....Call stack does not exist
*/
	if (!Sinit) return(UU_FAILURE);
/*
.....Initialize scan
*/
	uu_hash_initscan(&Shash);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION :  ncl_srcctl_scan()
**       Scans through the call stack table, returning the next record
**       in the hash table.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      :
**          Pointer to next call stack data, or UU_NULL if
**          at the end of the table.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int *ncl_srcctl_scan()
{
/*
.....Return the next entry in the call stack hash table
*/
	return((int *)uu_hash_scan(&Shash));
}
