 /************************************************************************
c
c   FILE NAME: toolsupt.c
c
c	 CONTAINS: functions that NCL already define, it could include the lib
c				when we link the toolib with NCL
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        toolsupt.c , 25.5
c     DATE AND TIME OF LAST  MODIFICATION
c        10/27/16 , 14:41:22
c
c**********************************************************************
*/

#include "usysdef.h"
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "xfsys1.h"
#include "xenv1.h"
#include "ulist.h"
#include "udforms.h"

#define UU_DEBUGMAIN 1
#include "ustdio.h"
#include "usignal.h"
#include "usysdef.h"
#include "udebug.h"
#include "nclver.h"

#define UX_F0PGM
#include "xfsys0.h"
#include "xfsys1.h"
#include "unserve.h"
#undef UX_F0PGM
#define TRACE UU_TRUE

#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define index strchr
#define rindex strrchr
#endif
/*not need here
#ifndef UU_RS6000
char *rindex(),*index(),*strchr();
#endif
*/
double NCL_version;
////
UU_LOGICAL NCLX_external_unibase=UU_FALSE;
/*********************************************************************
**	E_FUNCTION:	ul_fread (fptr,buf,size,numint)
**			This function reads a logical record from a disk
**			file.  The file must already be opened using the
**			"ux_fopen0" routine.
**
**       INPUT  :	fptr = file descriptor of input file.
**			size = size of output buffer.
**
**			numchars = size of string
**       OUTPUT :  
**			buf = address of buffer to receive data.
**			numint = actual number of bytes read
**
**    RETURNS      :	UU_SUCCESS if successful, UX_EOF if end of file,
**			UX_NSPACE if buffer is not large enough to hold data,
**			otherwise UU_FAILURE
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none.
**    NOTES	: This routine was borrowed from "ux_init_table"
*********************************************************************/
ul_fread (fptr,buf,size,numint)
	char buf[256];
	int *numint,size,fptr;
{
	int i,c,status,stat;
	char cc;

	status = UU_SUCCESS;
/*
.....Read a character from the input file stream
*/
	i = -1;
	for (;;)
	{
		if ((stat=ux_fgetc0(fptr,&cc))==-1) goto failed;
		c = cc;
/*
.....Check for EOF
*/
/*
.....we still need pass the last line
.....for we only changed for WIN2K
*/
#if UU_COMP!=UU_WIN2K
		if (stat == UX_EOF)
		{
			status = UX_EOF;
			goto done;
		}
#else
		if ((i<=0)&&(stat == UX_EOF))
		{
			status = UX_EOF;
			goto done;
		}
		else if ((stat == UX_EOF)&&(i>0))
		{
			i++;
			buf[i] = '\0';
			status = UU_SUCCESS;
			goto done;
		}
#endif
/*
.....Make sure there is enough room in buffer
*/
		else if (++i >= size)
		{
			status = UX_NO_SPACE;
			i = size;
/*
.....WinNT
*/
			while (c != '\n' && c != '\r' && stat != UX_EOF)
			{
				if ((stat=ux_fgetc0(fptr,&cc))==-1)
					goto failed;
				c = cc;
			}
			goto done;
		}
/*
.....Check for end of line
*/
		else if (c == '\n')
		{
			buf[i] = '\0';
			goto done;
		}
		if (c != '\r') buf[i] = c;
		else i--;
	}
failed:;
	status = UU_FAILURE;
done:;
	*numint = i;
	return(status);
}	

/*********************************************************************
**	E_FUNCTION:	ul_to_upper(str)
**			This function converts an Ascii string to all
**			upper case letters.
**    PARAMETERS   
**       INPUT  :	str = input text string
**       OUTPUT :  	str = returned string
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void ul_to_upper(str)
char *str;
{
	char *p;
	int i;
/*
.....Convert string to uppercase
*/
	p = str;
	for (i=0;i<strlen(str);i++)
	{
		if (isalpha(*p) && islower(*p)) *p = toupper(*p);
		p++;
	}
	return;
}

/*********************************************************************
**	E_FUNCTION:	ul_strip_blanks (str,size)
**			This function removes any blanks from a text string
**    PARAMETERS   
**       INPUT  :	str = input text string
**       OUTPUT :  
**			str = returned string
**			size =  # of characters returned.
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
 
void ul_strip_blanks (str,size)
char *str;
int *size;
{
	int i,j;
/*
.....Remove all blanks from string
*/
	j = 0;
	for (i=0;i<*size;i++)
	{
		if (str[i] == '\0') break;
		if (str[i] > ' ')
		{
			str[j] = str[i];
			j++;
		}
	}
	*size = j;
	str[j] = '\0';
	return;
}
 

/*********************************************************************
**  E_FUNCTION: ul_remove_quotes (buf)
**          This function removes quotes from a string.
**    PARAMETERS
**       INPUT  :   buf = input/output string.
**		 OUTPUT :	buf = output string.
**
**    RETURNS      :    None.
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none.
*********************************************************************/
void ul_remove_quotes(buf)
char *buf;
{
#define UX_QUOTE_CHAR '"'
	char *fq;
/*
.....Remove bounding quote marks
.....from string
*/
    fq = index(buf,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
		strcpy(buf,(fq+1));
		fq = rindex(buf,UX_QUOTE_CHAR);
		if (fq != UU_NULL) *fq = '\0';
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : ncl_sprintf(strng,elm,num)
**             Formats a string with a real number and eliminates
**             trailing zeroes.
**    PARAMETERS
**       INPUT  :
**          strng      A character string
**          elm        An array of reals to be formatted
**          num        The number of elements in elm
**          
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_sprintf(strng,elm,num)
char strng[];
UU_REAL elm[];
int num;
{

	int len,i,j,status;
	char buf[80],buf2[80];

	status = UU_FAILURE;
	strng[0]='\0';
	
	for (j=0;j<num;j++)
	{
		sprintf(buf2,"%f",elm[j]);
		len = strlen(buf2);
		i =0;
		while(i<len && status == UU_FAILURE)
		{
			if (buf2[i] == '.')
				status = UU_SUCCESS;
			i++;
		}
		if (status == UU_SUCCESS)
		{
/*
.....Starting at the end of the string, if we find a zero
.....null it out, otherwise we are done.
*/
			i = len - 1;
			while(i>0 && status == UU_SUCCESS)
			{
/*
.....Leave the 0 if it is right next to the decimal point;
....."1.0" looks a lot better than "1." or just "1".
*/
				if (buf2[i] == '0' && buf2[i-1]!='.')
					buf2[i] = '\0';
				else
					status = UU_FAILURE;
				i--;
			}
		}
		strcat(strng,buf2);
		if (j!=(num-1))
			strcat(strng,",");
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_mach_parse_rec(lbuf,nc,parms,nparm,istext)
**			This function parses a Machine Simulation record from any of
**       the data files.
**	 PARAMETERS	
**		 INPUT  :
**         lbuf    = Machine Simulation file record to parse.
**         nc      = Number of chars in 'lbuf'.
**         ptext   = 0 - No parameters contain a text string.
**                   Non-zero - Parameter that is purely a text string.
**                   This will be the last parameter parsed for this record.
**		 OUTPUT :
**         parms   = Array of text parameters in record.
**         nparm   = Number of text parameters returned in 'parms'.
**	 RETURNS:
**         UU_SUCCESS when parsing is successful, UU_FAILURE otherwise.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
int ul_ipv_mach_parse_rec(lbuf,nc,parms,nparm,ptext)
char *lbuf;
int nc;
char parms[50][80];
int *nparm;
int ptext;
{
	int i,ix,ipt,status,inc;
	UU_LOGICAL coment;
	char *p;
	static char *delim={"/ 	,()="};
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	i = ix = ipt = 0;
	coment = UU_FALSE;
/*
.....Find parameters
*/
	do
	{
/*
........Get rid of delimiters
*/
		for (i=i;i<nc;i++)
		{
			p = strchr(delim,lbuf[i]);
			if (p == UU_NULL) break;
/*
...........Comment
*/
			if (lbuf[i] == '/' && lbuf[i+1] == '/')
			{
				coment = UU_TRUE;
				break;
			}
		}
		if (coment || i >= nc) break;
		if (ix >= 50)
		{
			status = UU_FAILURE;
			break;
		}
/*
........Text record
*/
		if (ptext != 0 && ix == ptext)
		{
			strcpy(parms[ix],&lbuf[i]);
			inc = strlen(parms[ix]) - 1;
			if (inc >= 0 && parms[ix][inc] == ')') parms[ix][inc] = '\0';
			ix++;
			break;
		}
/*
........Break out parameter
*/
		for (i=i;i<nc;i++)
		{
			p = strchr(delim,lbuf[i]);
			if (p != UU_NULL) break;
			parms[ix][ipt] = lbuf[i];
			ipt++;
		}
		parms[ix][ipt] = '\0';
		ix++;
		ipt = 0;
	} while (i < nc);
	*nparm = ix;
/*
.....End of routine
*/
	return(status);
}
typedef UU_REAL	UM_vector[3];
/*********************************************************************
**    E_FUNCTION     : um_vctovc(vci,vco)
**      Copy one vector to another.
**    PARAMETERS   
**       INPUT  : 
**				vci        vector in
**       OUTPUT :  
**				vco        vector out
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_vctovc(vci,vco)
	UM_vector vci;
	UM_vector vco;

	{
	int i;				/* index */

	for (i = 0; i < 3; i++) vco[i]  =  vci[i];
	return (0);
	}

/*********************************************************************
**  E_FUNCTION: ul_to_reals(ary,inum,maxnum,str)
**          This function converts an Ascii string into an array of
**          real numbers.  Each number in the string should be
**			separated by commas. Checking is done to verify that
**          the string contains only numeric data.
**
**			Example: -1.5,3.456,1.2
.....it can have space too, Yurong changed, like
.....		Example: -1.5, 3.456, 1.2
**
**    PARAMETERS
**       INPUT  :   maxnum = Maximum number of reals to convert.
**					str = input text string
**       OUTPUT :   ary = Real array to receive converted numbers.
**					inum = Number of real numbers converted.
**    RETURNS      : UU_SUCCESS if no problems, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/

ul_to_reals(ary,inum,maxnum,str)
UU_REAL ary[];
int *inum,maxnum;
char str[];
{
	UU_REAL atof();
	int status,i,idot,nc;
	char *p,*pe,*es;
	char lbuf[80];
/*
.....Assume success
*/
	status = UU_SUCCESS;
	*inum = 0;
/*
.....Zero length string
*/
	if (strlen(str) == 0) goto done;
/*
.....Get next number from string
*/
	p = &str[0];
	es = p + strlen(str);
	do
	{
/*
.....remove heading space and '\t'
*/
		while ((*p==' ') || (*p=='\t')) 
		{
			p++;
		}
		pe = index(p,',');
		if (pe == 0) pe = es;
		if (pe > p)
		{
			nc = (int)pe - (int)p;
			strncpy (lbuf,p,nc);
			lbuf[nc] = '\0';
/*
.....remove trailing space and '\t'
*/
			for (i=strlen(lbuf); i>=0; i--)
			{
				if ((lbuf[i-1]==' ')||(lbuf[i-1]=='\t'))
					lbuf[i-1] = '\0';
				else
					break;
			}
/*
.....Check for valid number
*/
			idot = 0;
			for (i=0;i<nc;i++)
			{
				
				if (lbuf[i] == '.')
				{
					if (idot == 1) goto failed;
					idot = 1;
				}
				else if (lbuf[i] == '-' || lbuf[i] == '+')
				{
					if (i != 0) goto failed;
				}
				else
				{
					if (isdigit(lbuf[i]) == 0) goto failed;
				}
			}
/*
.....String contains only numeric data
.....Convert it to a number
*/
			ary[*inum] = atof(lbuf);
		}
/*
.....Point to next number in string
*/
		*inum = *inum + 1;
		if (*inum > maxnum) goto failed;
		p = pe + 1;
	} while (p < es);
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}
/*********************************************************************
**    E_FUNCTION : ud_free_tlist(formlist) 
**    PARAMETERS   
**             UD_TLIST *formlist: structure for forms that need be
**                                be released
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_free_tlist(formlist)
UD_TLIST *formlist;
{
	int i,j;

	if (formlist->col_label!=NULL)
	{
		for (i=0; i<formlist->num_col; i++)
		{
			if (formlist->col_label[i]!=NULL)
				uu_free (formlist->col_label[i]);
			formlist->col_label[i] = NULL;
		}
	}
	if ((formlist->num_col)&&(formlist->col_label!=NULL))
	{
		uu_free (formlist->col_label);
		formlist->col_label = NULL;
	}
	formlist->num_col = 0;
	for (i=0; i<formlist->num_item; i++)
	{
		for (j=0; j<formlist->data[i].itemnum; j++)
		{
			if (formlist->data[i].data_items[j]!=NULL)
				uu_free(formlist->data[i].data_items[j]);
			formlist->data[i].data_items[j] = NULL;
		}
		if (formlist->data[i].itemnum>0)
		{
			if (formlist->data[i].data_items!=NULL)
				uu_free(formlist->data[i].data_items);
			formlist->data[i].data_items = NULL;
		}
	}
	if ((formlist->num_item)&&(formlist->data!=NULL))
		uu_free(formlist->data);
	formlist->data = NULL;
/*
.....Reset structure
*/
	formlist->num_item = 0;
	return (0);
}

/*********************************************************************
**    E_FUNCTION : ud_tlist_copy(list1, list2)
**				copy the UD_TLIST strcuture
**    PARAMETERS   
**       INPUT  :  list1: list to be copied
**					
**       OUTPUT :  list2: list copied
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_tlist_copy(list1, list2)
UD_TLIST *list1, *list2;
{
	int i, j, len;

	list2->num_item = list1->num_item;
	list2->num_col = list1->num_col;
	list2->answer = list1->answer;
	if (list2->num_col>0)
		list2->col_label = (char**) uu_malloc(list2->num_col*sizeof (char*));
	for (i=0; i<list2->num_col;i++)
	{
		len = strlen (list1->col_label[i]);
		list2->col_label[i] = (char*) uu_malloc((len+1)*sizeof(char));
		strcpy(list2->col_label[i], list1->col_label[i]);
	}
	if (list2->num_item>0)
		list2->data = (UD_ITEMDATA *) uu_malloc(list2->num_item*sizeof(UD_ITEMDATA));
	for (i=0; i<list2->num_item;i++)
	{
		(list2->data[i]).itemnum = (list1->data[i]).itemnum;
		(list2->data[i]).data_items = 
					(char **) uu_malloc(list2->num_col*sizeof(char*));
		for (j=0; j<(list2->data[i]).itemnum; j++)
		{
			len = strlen(list1->data[i].data_items[j]);
			list2->data[i].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(list2->data[i].data_items[j], list1->data[i].data_items[j]);
		}
	}
}
/*********************************************************************
**    E_FUNCTION : ud_tlist_copy_idata(data1, data2)
**				copy the UD_TLIST item UD_ITEMDATA strcuture
**    PARAMETERS   
**       INPUT  :  data1: data to be copied
**					
**       OUTPUT :  data2: data copied
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_tlist_copy_idata(data1, data2)
UD_ITEMDATA *data1, *data2;
{
	int j, len;

	data2->itemnum = data1->itemnum;
	data2->frmid = data1->frmid;
	data2->fldno = data1->fldno;
	data2->flag = data1->flag;
	if ((data1->data_items!=NULL)&&(data1->itemnum>0))
		data2->data_items = (char**)uu_malloc(data1->itemnum*sizeof(char*));
	for (j=0; j<data1->itemnum; j++)
	{
		len = strlen(data1->data_items[j]);
		data2->data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
		strcpy(data2->data_items[j], data1->data_items[j]);
	}
}
/*********************************************************************
**    E_FUNCTION : ud_tlist_free_idata(data)
**				free UD_ITEMDATA strcuture
**    PARAMETERS   
**       INPUT  :  data: data to be freed
**					
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_tlist_free_idata(data)
UD_ITEMDATA *data;
{
	int j;

	if (data->data_items==NULL)
		return;
	for (j=0; j<data->itemnum; j++)
	{
		if (data->data_items[j]!=NULL)
		{
			uu_free(data->data_items[j]);
			data->data_items[j] = NULL;
		}
	}
	if ((data->itemnum>0)&&(data->data_items!=NULL))
		uu_free(data->data_items);
	data->data_items = NULL;
}

int ud_issame_idata(UD_ITEMDATA *data1, UD_ITEMDATA *data2)
{
	int j, len;

	if (data2->itemnum != data1->itemnum)
		return 0;
/*
.....for toolib, these value is ignored
//	if (data2->frmid != data1->frmid)
//		return 0;
//	if (data2->fldno != data1->fldno)
//		return 0;
//	if (data2->flag != data1->flag)
//		return 0;
*/
	if (data1->itemnum==0)
		return 0;
	for (j=0; j<data1->itemnum; j++)
	{
		if (stricmp(data2->data_items[j], data1->data_items[j])!=0)
			return 0;
	}
	return 1;
}
/*********************************************************************
**    E_FUNCTION     : S_match_after_star (pattern, text)
**        check if a text is match with wildcard string
**    PARAMETERS
**       INPUT  :
**          text     - text string to be checked
**          pattern     - wildcard string to be compared
**       OUTPUT : none
**    RETURNS      : 
**          match     - 1: matched
**						0: not matched
**						-1: abort
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_match_after_star (pattern, text)
char *pattern, *text;
{
	int match = 0;
	char nextp;

	while (*pattern=='*')
	{
		pattern++;
	}
	if (*pattern=='\0')
		return 1;

	nextp = *pattern;
	do
	{
		if (nextp == *text)
			match = S_ncl_match(pattern, text);
		if (*text==0)
			match = -1;
		text++;
	} while ( match != 1 && match != -1 );
	if (match==-1) match = 0;
	return match;
}
/*********************************************************************
**    E_FUNCTION     : S_ncl_match (pattern, text)
**        check if a text is match with wildcard string
**    PARAMETERS
**       INPUT  :
**          text     - text string to be checked
**          pattern     - wildcard string to be compared
**       OUTPUT : none
**    RETURNS      : 
**          match     - 1: matched
**						0: not matched
**						-1: abort
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_ncl_match (pattern, text)
char *pattern, *text;
{
	for ( ; *pattern; pattern++, text++)
	{
		if (text[0]=='\0')
		{
			if (*pattern== '*'&& *++pattern == '\0')
				return 1;  /* matched */
			else
				return -1;
		}
		switch (*pattern)
		{
			case '*': 
				return S_match_after_star (pattern, text);
			default:
				if (*pattern != *text)
					return 0;
		}
    }
/* 
.....if end of text not reached then the pattern fails 
*/
	if (*text)
       return 0;
	else  
		return 1;
}


/*********************************************************************
**    E_FUNCTION     : chkwstr(token, nc1, wstr, nc2, match)
**        check if a token is match with wildcard string
**    PARAMETERS
**       INPUT  :
**          token - token string to be checked
**			   nc1:  - length of the token
**          wstr  - wildcard string to be compared
**	         nc2:  - length of the wildcard string
**       OUTPUT :
**          match - 1: matched
**						  0: not matched
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void chkwstr(token, nc1, wstr, nc2, match)
char *token, *wstr;
int *nc1, *nc2, *match;
{
	char string1[65], string2[65];
	strncpy(string1, token, *nc1);
	string1[*nc1]='\0';
	strncpy(string2, wstr, *nc2);
	string2[*nc2]='\0';
	*match = 0;
	if ((*nc1==0) || (*nc2==0)) 
		return;
	if (strcmp(string2, "*")==0)
	{
		*match = 1;
		return;
	}
	*match = S_ncl_match(string2, string1);
	if (*match==-1)
		*match = 0;
}

/*********************************************************************
**    E_FUNCTION         :  ncl_filter_str2(label, filter)
**       This function doing the same as ncl_filter_str but the filter is without "*"
**		and the 'filter' can be anywhere in the string
**
**    PARAMETERS   
**       INPUT  : label: string to be checked
**					filter: filter string
**       OUTPUT : 
**    RETURNS      : 0: not match, 1: match
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_filter_str2(label, filter)
char *label, *filter;
{
	char tmpstr[1024], label_str[1024], filter_str[1024];
	int i, nc1, nc2, match;
/*
.....should not change the input string
*/
	strcpy(label_str, label);
	strcpy(filter_str, filter);
	nc1 = strlen (label_str);
	while ((nc1>1)&&(label_str[nc1-1]==' ')) nc1--;
	label_str[nc1] = '\0';
/*
.....remove '*' in the front and end, then add in 
.....(to make sure the phase can be anywhere in the string)
*/
	nc2 = strlen (filter_str);
	i=0;
	while ((nc2>1)&&(filter_str[nc2-1]==' ')) nc2--;
	if (nc2==0)
/*
.....all empty for filter, always return matched
*/
		return 1;

	while ((nc2>1)&&(filter_str[nc2-1]=='*')) nc2--;
	filter_str[nc2] = '\0';
	
	if (nc2>0)
	{
		strcpy(tmpstr, "*");
		strcat(tmpstr, filter_str);
		strcat(tmpstr, "*");
		strcpy(filter_str, tmpstr);
		nc2 = strlen (filter_str);
		chkwstr(label_str, &nc1, filter_str, &nc2, &match);
		return match;
	}
/*
......if nc2 = 0, if mean only have "*", so always match
*/
	return 1;
}

void ud_tlist_delete(UD_TLIST *list, int pos)
{
	int i, j, len;
	UD_ITEMDATA *data;
	if (list->num_item==0)
	{
		return;
	}
	list->num_item--;
	data = (UD_ITEMDATA *) uu_malloc(list->num_item*sizeof(UD_ITEMDATA));

	for (i=0; i<pos;i++)
	{
		data[i].itemnum = (list->data[i]).itemnum;
		data[i].data_items = 
					(char **) uu_malloc(list->num_col*sizeof(char*));
		for (j=0; j<(list->data[i]).itemnum; j++)
		{
			len = strlen(list->data[i].data_items[j]);
			data[i].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(data[i].data_items[j], list->data[i].data_items[j]);
			uu_free(list->data[i].data_items[j]);
			list->data[i].data_items[j] = NULL;
		}
	}
	for (j=0; j<(list->data[pos]).itemnum; j++)
	{
		uu_free(list->data[pos].data_items[j]);
		list->data[pos].data_items[j] = NULL;
	}
	for (i=pos; i<list->num_item;i++)
	{
		data[i].itemnum = (list->data[i+1]).itemnum;
		data[i].data_items = 
					(char **) uu_malloc(list->num_col*sizeof(char*));
		for (j=0; j<(list->data[i+1]).itemnum; j++)
		{
			len = strlen(list->data[i+1].data_items[j]);
			data[i].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(data[i].data_items[j], list->data[i+1].data_items[j]);
			uu_free(list->data[i+1].data_items[j]);
			list->data[i+1].data_items[j] = NULL;
		}
	}
	uu_free(list->data);
	list->data = data;
}

/*********************************************************************
**    E_FUNCTION : ud_tlist_insert(tlist, pos, data)
**				insert a data into the UD_TLIST strcuture
**    PARAMETERS   
**       INPUT  :  list1: list to be copied
**					
**       OUTPUT :  list2: list copied
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_tlist_insert(list, pos, indata)
UD_TLIST *list;
int pos;
UD_ITEMDATA *indata;
{
	int i, j, len;
	UD_ITEMDATA *data;
	list->num_item++;
	data = (UD_ITEMDATA *) uu_malloc(list->num_item*sizeof(UD_ITEMDATA));
	for (i=0; i<pos;i++)
	{
		data[i].itemnum = (list->data[i]).itemnum;
		data[i].data_items = 
					(char **) uu_malloc(list->num_col*sizeof(char*));
		for (j=0; j<(list->data[i]).itemnum; j++)
		{
			len = strlen(list->data[i].data_items[j]);
			data[i].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(data[i].data_items[j], list->data[i].data_items[j]);
			uu_free(list->data[i].data_items[j]);
			list->data[i].data_items[j] = NULL;
		}
	}
	for (i=list->num_item-1; i>pos;i--)
	{
		data[i].itemnum = (list->data[i-1]).itemnum;
		data[i].data_items = 
					(char **) uu_malloc(list->num_col*sizeof(char*));
		for (j=0; j<(list->data[i-1]).itemnum; j++)
		{
			len = strlen(list->data[i-1].data_items[j]);
			data[i].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(data[i].data_items[j], list->data[i-1].data_items[j]);
			uu_free(list->data[i-1].data_items[j]);
			list->data[i-1].data_items[j] = NULL;
		}
	}
	for (j=0; j<list->num_col; j++)
	{
		len = strlen(indata->data_items[j]);
		data[pos].data_items[j] = (char*)uu_malloc((len+1)*sizeof(char));
		strcpy(data[pos].data_items[j], indata->data_items[j]);
	}
	uu_free(list->data);
	list->data = data;
}
int ul_compare_upper(str1,str2)
char *str1,*str2;
{
	UX_pathname tmp1,tmp2;
/*
.....Convert strings to uppercase and compare
*/
	strcpy(tmp1,str1); strcpy(tmp2,str2);
	ul_to_upper(tmp1); ul_to_upper(tmp2);
	return(strcmp(tmp1,tmp2));
}

/*
......we don't care about points in tooolib, so just return 0
......then it will not add into list
......This is a junk routine (I don't use NCL function because
......it involve too many functions)
*/
typedef UU_REAL	UM_coord[3];
int ul_ipv_circle_pts(cpt,spt,ept,dir,tlist,npt)
UM_coord cpt,spt,ept;
int dir;
UU_LIST *tlist;
int *npt;
{
	uu_list_init(tlist,sizeof(UM_coord),50,20);
	return 0;
}
uu_exit() {}
uu_enter() {}
uu_trcprint() {}
uu_sys_err_recovery() {}
uu_qsort() {}
ncl_getcut_profnam() {}
setins() {}
ncl_getcut_profil() {}
getins() {}

int tool_getwd (cdir)
char *cdir;
{
	return 0;
}

ud_printmsg(){}
uu_uerror0(){}
uu_uerror1(){}
uu_uerror2(){}
ur_skip_ent(){}
void getifl(short*idx,short*ival)
{
}
