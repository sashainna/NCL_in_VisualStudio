
/*********************************************************************
**    NAME         :   tlangio2.c
**       CONTAINS:
**       	ut_cfnmread		ut_cin_intern	ut_dfnmread		ut_lxread		
**				UT_lxrite
**				ut_twrite
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       qlangio2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:55
*********************************************************************/

#include "uctype.h"
#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dmark.h"
#include "dinput.h"
#include "mdcpln.h"
#include "calcom.h"
#include "cdef.h"
#include "xenv1.h"		/* used by "ux" calls: UX_PRTERRS value */

/*********************************************************************
**    I_FUNCTION     :  int *ut_lxread(ln,len,lflag)	
**       toolmalloc storage and read lexicon file into it.
**    PARAMETERS   
**       INPUT  : 
**          ln[] - lexicon file name in internal code 
**          len - length of filename 
**       OUTPUT :  
**          UT_lx[] - array to contain the lexicon 
**          lflag	- returned 1 if no errors. else 0 
**    RETURNS      : pointer to array containg lexicon file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int *ut_lxread(ln,len,lflag)		
int ln[];						/* lexicon file name in internal code */
int len;							/* length of filename */
int *lflag;						/* returned 1 if no errors. else 0 */
{
	int *lexicon;						/* array to contain the lexicon */
	int i,fd,j;
	UX_pathname lnname; 	/* replaces char lnname[UU_MAXPATHLEN] */
	char us[80];
	/* translate lexicon name to ascii */
	for (i=1; i<= len; i++)
	{
		/* lnname[i-1]=UT_ictodc[ln[i]]; */
		lnname[i-1]=ln[i];
	}
	lnname[len]='\0';		/* null string terminator */
	uu_denter2(UU_DTRC,(us,"ut_lxread(%s)",lnname));
	lexicon=NULL;
#if UU_COMP==UU_WIN2K
/*
......must be open as binary mode
*/
	fd=open(lnname,0x8000);
#else
	fd=open(lnname,0);
#endif
	if (fd<0)
	{
		/* printf("ut_lxread cant open lexicon file %s\n",lnname); */
		uu_uerror1 (UQ_CALCERROR, 51, lnname);
		*lflag=0; goto rtn;
	}
	i=read(fd,&j,sizeof(int));	/* read length of file */
	if (i!=sizeof(int))
	{
		/* printf("ut_lxread(%s,len,lflag) cant read file length. i=%d\n",lnname,i);*/
		uu_uerror1 (UQ_CALCERROR, 52, lnname);
		*lflag=0; close(fd); goto rtn;
	}
	lexicon=(int *)uu_toolmalloc((j+100)*sizeof(int));
	i=read(fd,&lexicon[1],j*sizeof(int));	/* read lexicon */
	if (i!=j*sizeof(int))
	{
		/* printf("ut_lxread(%s) cant read lexicon data. length=%d\n",lnname,j); */
		uu_toolfree(lexicon); lexicon=NULL;	
		uu_uerror1 (UQ_CALCERROR, 53, lnname);
		*lflag=0; close(fd); goto rtn;
	}
	close(fd);
	/*
	printf("read lexicon file %s of length %d words.\n",lnname,j);
	*/
	*lflag=1;
rtn: uu_dprint(UU_DTRC,(us,"%x=ut_lxread(%s) *lflag=%d len=%d",
	lexicon,lnname,*lflag,j));
	uu_dexit;
	return(lexicon);
}

/*********************************************************************
**    I_FUNCTION     :  ut_cfnmread(in,kf,len)		
**       convert filename with ascii codes to langpak internal codes 
**    PARAMETERS   
**       INPUT  : 
**          kf - index of where to start placing data
**       OUTPUT :  
**          in[] - array to receive data 
**          len - on return, length of data read in 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ut_cfnmread(in,kf,len)		
int in[];				/* array to receive data */
int kf;					/* index of where to start placing data*/
int *len;				/* on return, length of data read in */
{
	static  UX_pathname ibuf;		/* replaces char  ibuf[UU_MAXPATHLEN] */
	int  i, j;
	char	*charptr;
	UX_pathname	pathname;
	UU_LOGICAL	found;

	charptr = UU_NULL;
	if (ux_get_syspath("CALCGRAM",&charptr,pathname,&found,UX_NPRTERRS|UX_NQUOTES)!=UU_SUCCESS)
	/* chptr = ux_getenv("CALCGRAM",UX_PRTERRS);	 */
		{
		/* gmessage("Can not open file 'calc.grm'"); */
		 uu_uerror0 (UQ_CALCERROR, 49);
		 uu_lsdel(charptr);
		 return(UU_FALSE);
		}
	else
		{
		 strcpy(ibuf,pathname);
		 *len=strlen(ibuf);
		 for (i=0; i<*len; i++)
			in[kf+i] = ibuf[i];
		 /* ut_cin_intern( in,*len,ibuf,kf); */
		 uu_lsdel(charptr);
		 return(UU_TRUE);
		}
}		/* ut_cfnmread */

/*********************************************************************
**    I_FUNCTION     :  ut_dfnmread(in,kf,len,fnamevar)		
**       convert filename with ascii codes to langpak internal codes 
**    PARAMETERS   
**       INPUT  : 
**          kf - index of where to start placing data
**				char *fnamevar; -- shell variable containing .grm filename.
**       OUTPUT :  
**          in[] - array to receive data 
**          len - on return, length of data read in 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ut_dfnmread(in,kf,len,fnamevar)		
int in[];				/* array to receive data */
int kf;					/* index of where to start placing data*/
int *len;				/* on return, length of data read in */
char *fnamevar;		/* name of shell variable containing .grm filename */
{
	static UX_pathname ibuf;		/* replaces  char  ibuf[UU_MAXPATHLEN] */
	int  i, j;
	char	*charptr;
	UX_pathname	pathname;
	UU_LOGICAL	found;

	uu_denter(UU_DTRC,(us,"ut_dfnmread(%s)",fnamevar));

	charptr = UU_NULL;
	if (ux_get_syspath(fnamevar,&charptr,pathname,&found,UX_NPRTERRS|UX_NQUOTES)==UU_SUCCESS)
	  {
		strcpy(ibuf, pathname);
		*len=strlen(ibuf);
 		for (i=0; i<*len; i++)
			in[kf+i] = ibuf[i];
	  }
	uu_lsdel(charptr);
	/* ut_cin_intern( in,*len,ibuf,kf);	*/
	uu_dexit;
}		/* ut_dfnmread */

/*********************************************************************
**    I_FUNCTION     :  ut_cin_intern (in, len, ibuf, kf)
**       change the character string into the lowercase integer array
**    PARAMETERS   
**       INPUT  : 
**          len : langth of the input string
**          ibuf: array contains the input string
**       OUTPUT :  
**          in : array contains the string in langpak internal from
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ut_cin_intern (in, len, ibuf, kf)
int  in[], len, kf;
char  ibuf[];
{
	int  i, j;

	for (i=0; i<len; i++)
	{
		if (isupper(ibuf[i]))
			ibuf[i] = tolower(ibuf[i]);	/* translate to lower case */
		in[kf+i] = ibuf[i];
			/* j=ibuf[i]&127;
			in[kf+i]=UT_dctoic[j];	*//* translate to internal code*/
	}
}	/* ut_cin_intern */
