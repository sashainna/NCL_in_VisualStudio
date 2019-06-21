/*********************************************************************
**    NAME         :  netextvar.c
**       CONTAINS: routines to handle NCL textvar
**                 ncl_get_textvar(e, ncl)
**                 ncl_put_textvar(e, ncl)
**    COPYRIGHT 1988 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       netextvar.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:53
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mdcoord.h"

#include "ncl.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_gttext(nclkey, f77textp, len)
**       Retrieve a text variable from the unibase.
**    PARAMETERS
**       INPUT  :
**          nclkey   - Key of UNIBASE textvar entity
**          len      - Maximum number of characters to return.
**       OUTPUT :
**          f77textp - Pointer to hold NCL textvar data.
**          len      - Number of characters returned.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_gttext(nclkey, f77textp, len)
UM_int4 *nclkey;
UM_f77_str_ptr f77textp;
UM_int2 *len;
	{
	int j,k,status = UU_SUCCESS;
   struct NCL_textvar_rec txtrec;
	char *tptr;

	j = *len;
	*len = 0;
	txtrec.key = *nclkey;
	status = ur_retrieve_data_relnum(txtrec.key, &txtrec.rel_num);
	if (txtrec.rel_num != NCL_TEXTVAR_REL) status = UU_FAILURE;
	if (status == UU_SUCCESS)
	{
		status = ncl_retrieve_data_fixed(&txtrec);
	}
	if (status == UU_SUCCESS)
	{
		tptr = UM_cstr_of_f77_str(f77textp);
		k = strlen(txtrec.text);
		if (k>j) k = j;
		strncpy(tptr,txtrec.text,k);
		*len = k;
	}

	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_pttext(f77textp, len, keyold, nclkey)
**       Store text in a UNICAD textvar.
**    PARAMETERS 
**       INPUT  :
**          f77textp - Pointer to text to store.
**          len      - Length of text to store.
**          keyold   - Key of textvar to upadte or 0 to create new one.
**       OUTPUT :
**          nclkey   - Key of textvar entity created or updated.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_pttext(f77textp, len, keyold, nclkey)
UM_f77_str_ptr f77textp;
UM_int2 *len;
UM_int4 *keyold, *nclkey;
	{
	int jlen = *len, status = UU_SUCCESS;
   struct NCL_textvar_rec txtrec;
	char *tptr;

	*nclkey        = 0;
	txtrec.key     = *keyold;
	txtrec.rel_num = NCL_TEXTVAR_REL;
	if (*keyold)
		status = ncl_retrieve_data_fixed(&txtrec);
	if (status == UU_SUCCESS)
	{
		tptr = UM_cstr_of_f77_str(f77textp);
		if (jlen>256) jlen = 256;
		strncpy(txtrec.text,tptr,jlen);
		txtrec.text[jlen] = '\0';
		status = ncl_create_entity(&txtrec, NCL_TEXTVAR_REL);
		if (status == UU_SUCCESS) *nclkey = txtrec.key;
	}

	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_lntext(nclkey, len)
**       Retrieve the number of character in a text variable from the 
**       unibase.
**    PARAMETERS
**       INPUT  :
**          nclkey   - Key of UNIBASE textvar entity.
**       OUTPUT :
**          len      - Number of characters in textvar.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_lntext(nclkey, len)
UM_int4 *nclkey;
UM_int2 *len;
	{
	int status = UU_SUCCESS;
   struct NCL_textvar_rec txtrec;

	*len = 0;
	txtrec.key = *nclkey;
	status = ur_retrieve_data_relnum(txtrec.key, &txtrec.rel_num);
	if (txtrec.rel_num != NCL_TEXTVAR_REL) status = UU_FAILURE;
	if (status == UU_SUCCESS)
	{
		status = ncl_retrieve_data_fixed(&txtrec);
	}
	if (status == UU_SUCCESS)
	{
		*len = strlen(txtrec.text);
	}

	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_fmtold(fmt, rval, str, len)
**       Format a real*8 variable using a caller supplied format.
**    PARAMETERS 
**       INPUT  :
**          fmt      - Format to use in the form w.d
**          rval     - Variable to format.
**       OUTPUT :
**          str      - Formatted variable.
**          len      - Number of characters in str.
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_fmtold(f77fmtp, lenf, rval, f77textp, len)
UM_f77_str_ptr f77fmtp, f77textp;
UM_real8 *rval;
UM_int2 *lenf, *len;
	{
	int status = UU_SUCCESS, k;
	char *sptr, *fptr, fstr[80];

	sptr = UM_cstr_of_f77_str(f77textp);
	fptr = UM_cstr_of_f77_str(f77fmtp);
	k = *lenf;
	if (k > 72) k = 72;

	fstr[0] = '%';
	strncpy(&fstr[1],fptr,k);
	fstr[k+1] = 'f';
	fstr[k+2] = '\0';

	*len = sprintf(sptr, fstr, *rval);

	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_fmtstr (f77fmtp, n, iflg, rval, f77strp,
**                                     f77textp, len, ierr)
**       Format real*8 and string variables using a caller supplied format.
**    PARAMETERS 
**       INPUT  :
**          f77fmtp  - C style format containing %f and %s formats and text.
**          n        - Number of variables to format.
**          iflg     - Array containing -1 for each real*8 variable and
**                     index into f77strp array for each string variable
**          rval     - Array of real*8 variables to format.
**          f77strp  - Strings to format.
**       OUTPUT :
**          f77textp - Formatted data.
**          len      - Length of formatted data.
**          ierr     - 0 if no error, else 1
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_fmtstr (f77fmtp, n, iflg, rval, f77strp, f77textp, len, ierr)
UM_f77_str_ptr f77fmtp, f77strp, f77textp;
UM_real8  *rval;
UM_int2 *n, *iflg, *len, *ierr;
{
	int status = UU_SUCCESS, j, k, i1, i2, k1, k2;
	char lstr[256], lfmt[80], *sptr, *fptr, *tptr, *fp1, *fp2, *tp1;

	fptr = UM_cstr_of_f77_str(f77fmtp);
	sptr = UM_cstr_of_f77_str(f77strp);
	tptr = UM_cstr_of_f77_str(f77textp);

	fp1 = fp2 = fptr;
	tp1 = tptr;
	*len = *ierr = 0;
	j = i1 = i2 = k1 = 0;
	while (*fp2)
	{
		if (*fp2 == '%')
		{
			if (fp2 > fp1)
			{
				k = fp2-fp1;
				*len += k;
				if (*len > 255) goto err;
				strncpy(tp1,fp1,k);
				tp1 += k;
				fp1 = fp2;
			}
			fp2++;
			if (*(fp2) == '%')
			{
				*tp1 = '%';
				tp1++;
				*len += 1;
			}
			else
			{
				while (*fp2=='+' || *fp2=='-' || *fp2=='.' || isdigit(*fp2)) fp2++;
				if (*fp2=='F') *fp2 = 'f';
				if (*fp2=='S') *fp2 = 's';
				if (!(*fp2=='f' && iflg[j] == -1 || *fp2=='s' && iflg[j]!=-1))
					goto err;
				k = fp2-fp1+1;
				if (k>79) goto err;
				strncpy(lfmt,fp1,k);
				lfmt[k] = '\0';
				if (iflg[j] == -1)
				{
					k = sprintf(lstr,lfmt,rval[j]);
				}
				else
				{
					k = sprintf(lstr,lfmt,&sptr[iflg[j]]);
				}
				*len += k;
				if (*len > 255) goto err;
				strncpy(tp1,lstr,k);
				tp1 += k;
				j++;
				if (j>*n) goto err;
			}
			fp1 = fp2+1;
		}
		fp2++;
	}
	if (fp2 > fp1)
	{
		k = fp2-fp1;
		*len += k;
		if (*len > 255) goto err;
		strncpy(tp1,fp1,k);
	}
	if (j!=*n) goto err;

	return (status);
err:;
	 *ierr = 1;
	 return(UU_FAILURE);
}
