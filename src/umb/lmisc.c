/*********************************************************************
**	FILENAME: lmisc.c
**	CONTAINS:
**				ul_fread
**				ul_fread1
**				ul_is_binary
**				ul_remove_quotes
**				ul_add_quotes
**				ul_string_def
**				ul_strip_blanks
**				ul_to_number
**				ul_to_reals
**				ul_to_upper
**				ul_to_lower
**				ul_compare_upper
**				ul_cut_string
**				ul_parse_string
**				ul_date_and_time
**				ul_format_date
**				ul_format_time
**				ul_format_data11
**				ul_format_numbers
**     MODULE NAME AND RELEASE LEVEL 
**       lmisc.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       10/27/16 , 14:06:30
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "driver.h"
#include "gtblws.h"
#include "udebug.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "ctype.h"
#include "nclfc.h"

#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define index strchr
#define rindex strrchr
#endif

#if UU_COMP == UU_IRIS || UU_COMP == UU_IRIS4D
#ifdef UU_RS6000
#include <sys/time.h>
#endif
#include <time.h>
#else
#if (UU_COMP != UU_WIN2K)
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

void ul_format_date();

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
char *buf;
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
		if ((stat=ux_fgetc0(fptr,&cc))==UX_FAILURE) goto failed;
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
				if ((stat=ux_fgetc0(fptr,&cc))==UX_FAILURE)
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
**	E_FUNCTION:	ul_fread1 (fptr,buf,size_ptr,numint)
**			This function reads a logical record from a disk
**			file.  The file must already be opened using the
**			"ux_fopen0" routine.  If there is not enough memory to store
**       the entire line, then more memory will be allocated.  The
**       original memory needs to be allocated by the calling routine
**       and freed by the calling routine.
**
**       INPUT  :
**				fptr = file descriptor of input file.
**				size_ptr = size of output buffer.
**
**       OUTPUT :  
**				bptr   = address of buffer to receive data.
**				numint = actual number of bytes read
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
int ul_fread1(fptr,bptr,size_ptr,numint)
char **bptr;
int *numint,*size_ptr,fptr;
{
	int i,c,status,stat,size;
	char cc,*buf,*cptr;

	status = UU_SUCCESS;
	size = *size_ptr;
	buf = *bptr;
/*
.....Read a character from the input file stream
*/
	i = -1;
	for (;;)
	{
		if ((stat=ux_fgetc0(fptr,&cc))==UX_FAILURE) goto failed;
		c = cc;
/*
.....Check for EOF
*/
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
/*
.....Make sure there is enough room in buffer
*/
		else if (++i >= size)
		{
/*
........Increase the size of the buffer
*/
			cptr = (char *)uu_malloc(sizeof(char)*size*2);
			strncpy(cptr,buf,size);
			uu_free(buf);
			*bptr = buf = cptr;
			size *= 2;
			*size_ptr = size;
		}
/*
.....Check for end of line
*/
		if (c == '\n')
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
**	E_FUNCTION:	ul_is_binary (sfil)
**			This function determines if a file is binary or ASCII.
**
**       INPUT  :
**          sfil   = Name of file to check.
**       OUTPUT : none
**
**    RETURNS      :	1 if file is binary,  0 if ASCII, -1 if not found.
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none.
*********************************************************************/
int ul_is_binary(sfil)
char *sfil;
{
	int i,j,stat,mode,fstat,inum,status;
	UU_LOGICAL opend;
	UX_pathname lbuf;
	FILE *fd;
/*
.....Initialize routine
*/
	status = -1;
	opend = UU_FALSE;
/*
.....See if file exists
*/
	mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
	stat = ux_file_inquire(UU_NULL,UU_NULL,sfil,UU_NULL,UU_NULL,
		&mode,&fstat,lbuf,UX_NPRTERRS);
	if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS)) goto done;
/*
.....Open file for reading
*/
	stat = ux_fopen0(sfil,"rb",&fd);
	if (stat != UU_SUCCESS || fd == UU_NULL) goto done;
	opend = UU_TRUE;
/*
.....Determine if Binary or Ascii file
*/
	for (i=0;i<2;i++)
	{
		stat = ul_fread(fd,lbuf,sizeof(lbuf),&inum);
		if (stat != UU_SUCCESS && stat != UX_NO_SPACE) goto done;
		status = 0;
		for (j=0;j<inum;j++)
		{
			if (lbuf[j] < 32 || lbuf[j] > 126)
			{
				status = 1;
				break;
			}
		}
	}
/*
.....End of routine
........Close the file
*/
done:;
	if (opend) ux_fclose0(fd);
	return(status);
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
	char *fq,*index(),*rindex();
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
**  E_FUNCTION: ul_add_quotes (ibuf,obuf)
**          This function encloses a string in quotes.
**    PARAMETERS
**     INPUT  :   ibuf = Input string.
**		 OUTPUT :	obuf = Quoted string.
**
**    RETURNS      :    None.
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none.
*********************************************************************/
void ul_add_quotes(ibuf,obuf)
char *ibuf,*obuf;
{
	char tbuf[UX_MAX_PATH_LEN+2];
	int nc;
/*
.....Add quotes to string
*/
	strcpy(tbuf,ibuf);
	if (tbuf[0] == UX_QUOTE_CHAR)
		obuf[0] = '\0';
	else
	{
		obuf[0] = UX_QUOTE_CHAR;
		obuf[1] = '\0';
	}
	strcat(obuf,tbuf);
	nc = strlen(obuf);
	if (obuf[nc-1] != UX_QUOTE_CHAR)
	{
		obuf[nc] = UX_QUOTE_CHAR;
		obuf[nc+1] = '\0';
	}
	return;
}

/*********************************************************************
**	E_FUNCTION:	ul_string_def (prompt,string,numchars,outchars,
**					termstat)
**			This function prompts the user for an input string
**			using a default string.
**    PARAMETERS   
**       INPUT  :	prompt = address of ascii prompt string.
**			string = default string
**			numchars = size of string
**       OUTPUT :  
**			string = returned string
**			outchars = # of characters returned.
**			termstat = termination status.
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none.
**    NOTES	: This routine was borrowed from "ud_string_def"
*********************************************************************/

ul_string_def(prompt, string, numchars, outchars, termstat)
	char *prompt,*string;
	int numchars, *outchars;
	UD_DASTAT *termstat;
{
	int status;
	UD_STRREC strrec;		/* string control block */
/*
.....Store default string in String Control Block
*/
	strrec.instring = string;
/*
.....Get input from user
*/
	status = ud_ddas(UD_DASSTRINGDEF, prompt, &strrec, numchars,
			outchars, UD_DEFAULT);
	*termstat = strrec.termcon;
	return(status);
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
** E_FUNCTION: ul_to_number(str,num)
**     This function converts an Ascii string to an
**     integer number.  Checking is done to verify that
**     the string contains only numeric data.
**.....it can have heading and trailing space too, Yurong changed
**    PARAMETERS   
**       INPUT  :    str = input text string
**       OUTPUT :    num = output number
**    RETURNS      : UU_SUCCESS if no problems, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
 
ul_to_number(str,num)
	char str[];
	int *num;
{
	int status;
	char *sptr=str;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	if (!*sptr) goto failed;
/*
.....Get past leading white space
*/
	while (*sptr && (*sptr==' ' || *sptr=='\t')) sptr++;
	if (!*sptr) goto failed;
/*
.....Leading plus or minus sign
*/
	if (*sptr=='+' || *sptr=='-') sptr++;
	if (!*sptr) goto failed;
/*
.....check for digits
*/
	while (*sptr && *sptr!=' ' && *sptr!='\t')
	{
		if (*sptr == '.')
		{
			*sptr = ' ';
			break;
		}
		if (!isdigit(*sptr)) goto failed;
		sptr++;
	}
/*
.....Check for nothing other than trailing white space
*/
	while (*sptr)
	{
		if (*sptr!=' ' && *sptr!='\t') goto failed;
		sptr++;
	}
/*
.....String contains only numeric data
.....Convert it to a number
*/
	*num = atol(str);
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
**  E_FUNCTION: ul_to_reals(ary,inum,maxnum,str)
**          This function converts an Ascii string into an array of
**          real numbers.  Each number in the string should be
**          separated by commas. Checking is done to verify that
**          the string contains only numeric data.
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
	char *p,*pe,*es,*index();
	char lbuf[80];
/*
.....Assume success
*/
	status = UU_SUCCESS;
	*inum = 0;
/*
.....Zero length string
*/
	if (strlen(str) == 0) goto failed;
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
			nc = i;
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
			*inum = *inum + 1;
			if (*inum > maxnum) goto failed;
/*
.....Point to next number in string
*/
		}
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
**	E_FUNCTION:	ul_to_lower(str)
**			This function converts an Ascii string to all
**			upper case letters.
**    PARAMETERS   
**       INPUT  :	str = input text string
**       OUTPUT :  	str = returned string
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void ul_to_lower(str)
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
		if (isalpha(*p) && isupper(*p)) *p = tolower(*p);
		p++;
	}
	return;
}

/*********************************************************************
**	E_FUNCTION:	ul_compare_upper(str1,str2)
**			This function compares two strings ignoring the case.
**    PARAMETERS   
**       INPUT  :
**				str1    = First string to compare.
**				str2    = Second string to compare.
**       OUTPUT :
**    RETURNS      :
**				0 if strings compare, pointer to character that does not
**				compare otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
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

/*********************************************************************
**	E_FUNCTION:	ul_cut_string(str,num)
**			This function find length of the string excluding non
**       printable characters and trailing spaces.
**    PARAMETERS   
**       INPUT  :	str = input text string
**                num = maximum length of string
**       OUTPUT :  	str = returned string
**    RETURNS      : Number of characters in output string.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
int ul_cut_string (str,num)
char *str;
int num;
{
	int i, nc = 0;
/*
.....ignore all unprintable characters
.....and trailing spaces in the string
*/
/*
.....get the lenght of th estring if it is less than num
*/
   while(nc<num && str[nc]!='\0'&& (unsigned)(str[nc] + 1) <= 256) nc++;
	i = 0;
	while (i<nc && isprint(str[i]) != 0) i++;
	nc = i;
	while (nc>0 && isspace(str[nc-1]) != 0) nc--;
	return (nc);
}

/*********************************************************************
**	E_FUNCTION:	ul_parse_string(cinp,nc,inc,cout)
**			This function returns the next substring in a string separated
**       by commas, spaces, or tabs.
**    PARAMETERS   
**       INPUT  :	cinp   = Input text string
**                nc     = Number of characters in 'cinp'
**                inc    = Starting position within 'cinp' to start
**                         parsing from.  Should be set to 0 at first
**                         call and modified by this routine thereafter.
**       OUTPUT : cout   = Substring of 'cinp'.  Will be blank if there
**                         are no substrings left.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void ul_parse_string (cinp,nc,inc,cout)
char *cinp;
int nc,*inc;
char *cout;
{
	int i;
/*
.....No substrings left
*/
	if (*inc >= nc) cout[0] = '\0';
/*
.....Find end of next substring
*/
	else
	{
		strcpy(cout,&cinp[*inc]);
		for (i=0;i<nc-(*inc)+1;i++)
		{
			if (cout[i] == ',' || cout[i] == ' ' || cout[i] == '\t' ||
				cout[i] == '\0')
			{
				cout[i] = '\0';
				*inc = *inc + i + 1;
				break;
			}
		}
	}
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**	E_FUNCTION:	ul_date_and_time(str)
**	This function formats the current date and time into a string.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : str  = Contains current date and time in the format
**                       'dd-mmm-yyyy hh:mm:ss'
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void ul_date_and_time(str)
char *str;
{
	int year;
	struct tm *time_st;
#ifdef UU_RS6000
	struct timeval timval;
#else
#if UU_COMP ==  UU_WIN2K
	time_t timval;
#else
	long timval;
#endif
#endif
/*
.....Get the current date and time
*/
#ifndef UU_RS6000
	time(&timval);
#else
	ftime(&timval);
#endif
	time_st = localtime(&timval);
	year = time_st->tm_year;
	if (year < 70) year = year + 2000;
	else if (year > 99) year = year + 1900;
	ul_format_date(time_st->tm_mon+1,time_st->tm_mday,year,
		time_st->tm_hour,time_st->tm_min,time_st->tm_sec,str);
	return;
}

/*********************************************************************
**	E_FUNCTION:	ul_format_date(mm,dd,yy,hh,mm,ss,str)
**	This function formats a date and time into a string.
**    PARAMETERS   
**       INPUT  :
**          mm     = Month.
**          dd     = Day.
**          yy     = Year.
**          h      = Hours.
**          m      = Minutes.
**          s      = Seconds.
**       OUTPUT :
**          str    = Contains current date and time in the format
**                   'dd-mmm-yyyy hh:mm:ss'
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void ul_format_date(mm,dd,yy,h,m,s,str)
int mm,dd,yy,h,m,s;
char *str;
{
	static char *mon[]={"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG",
		"SEP","OCT","NOV","DEC"};
/*
.....Make sure all numbers are in range
*/
	if (mm < 1 || mm > 12) mm = 1;
	if (dd < 1 || dd > 31) dd = 1;
	if (yy < 0 || yy > 9999) yy = 1900;
	if (h < 0 | h > 24) h = 0;
	if (m < 0 | m > 60) m = 0;
	if (s < 0 | s > 60) s = 0;
/*
.....Format the date and time
*/
	sprintf(str,"%02d-%s-%04d %02d:%02d:%02d",dd,mon[mm-1],yy,h,m,s);
	return;
}

/*********************************************************************
**	E_FUNCTION:	ul_format_time(tim,str)
**	This function formats the specified time (in minutes).
**    PARAMETERS   
**       INPUT  : tim  = Contains the time to be formatted in minutes.
**       OUTPUT : str  = Contains the time in the format 'hh:mm:ss'.
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void ul_format_time(tim,str)
UU_REAL tim;
char *str;
{
	UU_REAL hrs,mins,secs;
	int ihrs,imins,isecs;
/*
.....Get the time in Hours, Minutes, Seconds
*/
	hrs = tim / 60.;
	ihrs = (int)hrs;
	mins = tim - ihrs*60;

	imins = (int)mins;
	mins = mins - imins;
	
	secs = mins * 60.;
	isecs = (int)secs;
	sprintf(str,"%02d:%02d:%02d",ihrs,imins,isecs);
}
/*********************************************************************
**	E_FUNCTION:	ul_format_data11(num, numstr)
**	This function formats the a REAL number to a character string of 11.
**		the format rule is made for print file
**    PARAMETERS   
**       INPUT  : num  = REAL number to be formated.
**       OUTPUT : numstr  = formated string.
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
ul_format_data11(num, numstr)
UU_REAL *num;
char *numstr;
{
	UU_REAL tmpnum = *num;
	char tmpstr[80];
	if (((tmpnum>0)&&(tmpnum<0.0001))||((tmpnum<0)&&(tmpnum>-0.0001)))
		tmpnum = 0.0;
/*
.....number is less than 7 digits or 6 digits if negtive
*/
	if (((tmpnum>=0)&&(tmpnum<10000000))||((tmpnum<=0)&&(tmpnum>-1000000)))
		sprintf(tmpstr, "%11.4f", tmpnum);
/*
.....number is 8 digits or 7 digits if negtive
*/
	if (((tmpnum>=10000000)&&(tmpnum<=100000000))
			||((tmpnum>=-10000000)&&(tmpnum<-1000000)))
		sprintf(tmpstr, "%11.3f", tmpnum);
/*
.....number is 9 digits or 8 digits if negtive
*/
	if (((tmpnum>=100000000)&&(tmpnum<=1000000000))
			||((tmpnum>=-100000000)&&(tmpnum<-10000000)))
		sprintf(tmpstr, "%11.2f", tmpnum);
/*
.....others: number is more 9 digits or more than 8 digits if negtive
*/
	if (((tmpnum>=100000000)&&(tmpnum>=1000000000))
			||((tmpnum>=-100000000)&&(tmpnum<-10000000)))
	{
		sprintf(tmpstr, "%11f", tmpnum);
		tmpstr[10] = '*';
	}
	strncpy(numstr, tmpstr, 11);
}

/*********************************************************************
**	E_FUNCTION:	ul_format_numbers(vals,nvals,ndigs,flag,numstr)
**	This function formats a REAL number array to a comma delimited
** character string.
**    PARAMETERS   
**       INPUT  :
**                vals    = Array of REAL numbers to be formatted.
**                nvals   = Number of reals in 'vals'.
**                ndigs   = Number of digits to keep to the right of
**                          the decimal point.
**                flag    = UU_TRUE = Remove trailing zeroes.
**       OUTPUT :
**                numstr  = Formatted string.
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void ul_format_numbers(vals,nvals,ndigs,flag,numstr)
UU_REAL *vals;
int nvals, ndigs;
char *numstr;
UU_LOGICAL flag;
{
	int i,j;
	char fmt[20],sbuf[80];
/*
.....Format number
*/
	numstr[0] = '\0';
//	sprintf(sbuf,".%d",ndigs);
//	sprintf(fmt,"%%%sf",sbuf);
	sprintf(fmt,"%%.%df",ndigs);
	for (i=0;i<nvals;i++)
	{
		sprintf(sbuf,fmt,vals[i]);
		if (flag)
		{
			for (j=strlen(sbuf)-1;j>=0;j--)
			{
				if (sbuf[j] != '0')
				{
					sbuf[j+1] = '\0';
					break;
				}
			}
		}
		strcat(numstr,sbuf);
		if (i != nvals-1) strcat(numstr,",");
	}
}
