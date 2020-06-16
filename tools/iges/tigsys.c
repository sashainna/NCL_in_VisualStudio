#include "usysdef.h"
/*********************************************************************
**    NAME         :  igsys.c
**       CONTAINS:
**             uig_is_listing_on
**             uig_clean
**             uig_open
**             uig_rd_direct
**             uig_nxtarg
**             uig_move
**             uig_str_in
**             uig_line_in
**             open_listing_file
**             uig_list_out
**             uig_str_out
**             uig_ans
**             uig_atof
**             uig_get_file
**             uig_tran_vec
**             uig_tran_coor
**             uig_get_addr
**             uig_tty_mode
**             uig_trans_comp
**             uig_cceqcc
**             uig_time
**             uig_unblock
**             uig_numeric_str
**             uu_sys_err_recovery
**             uig_unb_opnfil
**             uig_readrec
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tigsys.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:51
*********************************************************************/
#include <errno.h>
#define EPGM
#include "derror.h"
#undef EPGM
#include "tiges.h"
#include "xenv1.h"
#include "umath.h"
#include <stdio.h>
#include <ctype.h>
#include	"udebug.h"
#include "nclver.h"
#if (UU_COMP!=UU_WIN2K)
#include "tigmf.h"
#include <curses.h>
#endif
#if (UU_COMP==UU_WIN2K)
#include <io.h>
#include <fcntl.h>
#define write _write
#define read _read
#define close _close
#define open _open
#define creat _creat
#define lseek _lseek
#endif

#if UU_COMP==UU_PYRAMID || UU_COMP==UU_VAXULTRIX
#include	</sys/h/time.h>
#else
#include	<time.h>
#endif

#if UU_COMP==UU_VAXVMS
#include file
#include iodef
#include stdio
extern int UIG_tty_channel;
#endif

#if UU_OPSYS==UU_SYS5 || UU_OPSYS==UU_SYS53 || UU_COMP==UU_IRIS4D
#include <fcntl.h>
#endif
#if UU_COMP==UU_APOLLO
#include <fcntl.h>
#endif

#if UU_COMP!=UU_RIDGE
#if UU_COMP!=UU_APOLLO
#if UU_COMP!=UU_VAXVMS
#if UU_COMP!=UU_IRIS
#if UU_COMP!=UU_IRIS4D
#if (UU_COMP!=UU_WIN2K)
#include <sys/file.h>
#endif
#endif
#endif
#endif
#endif
#endif

#if UU_COMP!=UU_VAXVMS
#include <sys/types.h>
#include <sys/stat.h>
#endif
static char save_str[400];

/********************************************************************
** global tty statis flag
*********************************************************************/

static UU_LOGICAL tty_status;
int listfd;				/*jkd32: listing file */
char iges_fname[UX_MAX_PATH_LEN] = "";
char iges_tmpfile[UX_MAX_PATH_LEN] = "";
char ext_filename[UX_MAX_PATH_LEN];

void uig_list_out();
void uig_str_out();
void uig_tty_mode();
void uig_unblock();

/*********************************************************************
**    E_FUNCTION     :  uig_is_listing_on()
**       Returns UU_TRUE if a listing file is being created.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : UU_TRUE if a listing file is being created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL uig_is_listing_on()
{
	if (listfd != 0) return(UU_TRUE);
	else return(UU_FALSE);
}

/*********************************************************************
**    I_FUNCTION     :  uig_clear()
**       Clear the alpha screen.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_clear()
   {
/*
.....if run MFC on PC
.....we don't need do any thing
.....Yurong
*/
#if (UU_COMP!=UU_WIN2K)
		if (UW_MOTIF==1) return 0;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   /* VT100 clear screen commands 

   uig_str_out("\033[2J");
   uig_str_out("\033[H");
    
    */
    clear();	/*jkd35*/
    refresh();
#endif
	return 0;
   }
/*********************************************************************
**    I_FUNCTION     :  int uig_open(filename, flag)
**       Open the IGES date file for read.
**    PARAMETERS   
**       INPUT  : 
**          filename                    name of the IGES data file 
**          flag                        access permission flag
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_open(iges_file, flag) 
 
char iges_file[];
int flag;
{ 
	int mode,oflag,status;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	mode = 0;
	switch(flag)
	{
#if (UU_COMP!=UU_WIN2K)
	case 0:
		oflag = O_RDONLY;									/* open for read */
		break;
	case 1:
		oflag = O_WRONLY;									/* open for write */
		break;
	case 2:
  		oflag = O_RDWR;                        /* open for read & write */
		break;
#else
	case 0:
		oflag = _O_RDONLY;									/* open for read */
		break;
	case 1:
		oflag = _O_WRONLY;									/* open for write */
		break;
	case 2:
  		oflag = _O_RDWR;                        /* open for read & write */
		break;
#endif
	}
	status = open(iges_file,oflag,mode);    /* user std open for now */
	return(status);                         /* return file status */
 }                                                                 
/*********************************************************************
**    I_FUNCTION     :  int uig_rd_direct(type,irec,nbytes,buff)
**       Read a record from the IGES disk file.
**    PARAMETERS   
**       INPUT  : 
**          type                  record type 
**          irec                  record number 
**          nbytes                number of bytes to read 
**       OUTPUT :  
**          buff                  ascii characters in the record 
**    RETURNS      : the number of bytes read
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_rd_direct(type,irec,nbytes,buff)
   int type;                           /* record type */
   int irec;                           /* record number */
   int nbytes;                         /* number of bytes to read */
   char buff[];                        /* output character buffer */
   {
   int offset;                         /* byte offset in the file */
   int stat;                           /* file status */
   int bytes = 80;                     /* number of bytes per record */
   int k;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   switch(type)                        /* switch on record type */
      {
      case 0:                          /* start block */

         offset = irec-1;
         break;

      case 1:                          /* global block */

         offset = sect_ptr[0] + irec -1;
         break;

      case 2:                          /* directory block */

         if (irec > sect_ptr[2])
            {
            printf("invalid directory record index");
            offset = 0;
			return(0);	/*MILLS: no divide by zero! */
            /* offset = irec / offset; */ /* div by zero */
            };
         offset = (sect_ptr[0] + sect_ptr[1] + irec-1) * bytes;
         if(dbyte == 0 || offset < dbyte || (offset+nbytes) > (dbyte+2047))
            {
            dbyte = offset;                     /* read new buffer */
            stat = lseek(iges_fd,offset,0);
            stat = read(iges_fd,dir_buff,2048);
            for(k=0;k<2048;k++)
               {
               dir_buff[k] &= 0x7f;
               }
            for(stat=0;stat<nbytes;stat++)
               buff[stat] = dir_buff[stat];
            return(stat++);
            }
         else                             /* supply data from incore buffer */
            {
            for(k=offset,stat=0;stat<nbytes;k++,stat++)
               buff[stat] = dir_buff[k-dbyte];
            return(stat++);
            }

      case 3:                                      /* parameter block */

         if (irec > sect_ptr[3])
            {
            printf("invalid parameter record index\n");
            offset = 0;
			return(0);	/*MILLS: no divide by zero! */
			/* offset = irec / offset; */ /* div by zero */
            };
         offset = (sect_ptr[0] + sect_ptr[1] + sect_ptr[2] + irec-1) * bytes;
         if(pbyte == 0 || offset < pbyte || (offset+nbytes) > (pbyte+2047))
            {
            pbyte = offset;                        /* read new buffer */
            stat = lseek(iges_fd,offset,0);
            stat = read(iges_fd,par_buff,2048);
            for(k=0;k<2048;k++)
               {
               par_buff[k] &= 0x7f;
               }
            for(stat=0;stat<nbytes;stat++)
               buff[stat] = par_buff[stat];
            return(stat++);
            }
         else                                /* supply data for incore buffer */
            {
            for(k=offset,stat=0;stat<nbytes;k++,stat++)
               buff[stat] = par_buff[k-pbyte];
            return(stat++);
            }

      case 4:                                /* terminal block */

         offset = sect_ptr[0] + sect_ptr[1] + sect_ptr[2] + 
                  sect_ptr[3] + irec -1;
         break;
      }
   offset = offset * bytes;
   stat = lseek(iges_fd,offset,0);        /* move file pointer */
   stat = read(iges_fd,buff,nbytes);
   for(k=0;k<nbytes;k++)
      {
      buff[k] &= 0x7f;
      }
   return(stat);
   }

/*********************************************************************
**    I_FUNCTION     :  int uig_nxtarg(type,inbuf,start,next,outnum,outbuf))
**      Find the next parameter in either a global or parameter record. Scan
**      the record starting at start looking for a delimitor character or the
**      end of the current record. Return the characters found, the count of
**      of characters, and an index to the next good character in inbuff.
**    PARAMETERS   
**       INPUT  : 
**          type                 record type 
**          inbuf                inbuf character string
**          start                index of first character in inbuff to start
**                                  parsing
**       OUTPUT :  
**          next                 index of next character beyond delimitor
**          outnum               number of characters transferred to outbuf
**          outbuf               output buffer
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_nxtarg(type,inbuf,start,next,outnum,outbuf)
   int type;                           /* record type */ 
   char inbuf[];                       /* input buffer */
   int start;                          /* index to starting character */
   int *next;                          /* index to next good character */
   int *outnum;                        /* count of characters in outbuf */
   char outbuf[];                      /* output buffer */
   {
   int i,j,imax,count;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   if(type == 1)                       /* set maximum record size */
      {
      imax = 72;                       /* global record */
      }
   else
      {
      imax = 64;                       /* parameter record */
      }

   *outnum = count = 0;
   i = start;
   while(inbuf[i] == ' ' && i < imax) i++;  /* skip leading blanks */

   if(i == imax)                       /* check if at end of record */
      {
      *next = -imax;
      *outnum = count;
      return(0);
      }
                                       /* start search loop */
   for(j=i;j<imax;j++)
      {
      if(inbuf[j] == delim || inbuf[j] == term)
         {
         *next = j+1;                  /* found delimitor - return */
         *outnum = count;
         outbuf[count] = '\0';
         if(inbuf[j] == term)
            {
            return(1);
            }
         else
            {
            return(0);
            }
         }
      else
         {
         outbuf[count] = inbuf[j];  /* good char transfer to output */
         count++;
         }
      }
   *next = -imax;                      /* ran out end of record */
   *outnum = count;
   return(0);
   }

/*********************************************************************
**    I_FUNCTION     :  uig_move(c1,c2,n)
**       Move a real or integer as a byte stream into a character array
**    PARAMETERS   
**       INPUT  : 
**          c1                      real or integer number
**          n                       number of bytes to move
**       OUTPUT :  
**          c2                      character array to store bytes
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_move(c1,c2,n)

   char c1[];                 /* real or integer */
   char c2[];                 /* target array */
   int n;                     /* sizeof(int or float) */
   {
   int i;
   for(i=0;i<n;i++)
      c2[i] = c1[i];
   }

/*********************************************************************
**    I_FUNCTION     :  uig_str_in(string)
**          Input a string
**    PARAMETERS   
**       INPUT  : 
**          input    
**       OUTPUT :  
**          string                  character string input by user   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if (UU_COMP!=UU_WIN2K)
void uig_str_in(string)
   char string[];
   {
   int i, ichar;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   /* NCL: null terminate string in case we don't get one */
   string[0] = '\0';

/*jkd35: handle delete and backspace on input */
#if UU_COMP!=UU_VAXVMS
   if(tty_status == UU_FALSE)
      {
      noecho();
      i = 0;
      ichar = getch();
      while (ichar != '\n')
         {
         if (ichar==8 || ichar==127)
            {
            if (i>0)
               {
               addch(8);	/* delch() doesn't seem to work...      */
               addch(' '); /* ... but BS, blank, BS does the same. */
               addch(8);
               i--;
               }
            }
         else
            {
            string[i++] = ichar;
            addch(ichar);
            };
         refresh();
         ichar = getch();
         }
      string[i] = '\0';
	  /**
      putchar('\n');
	  **/
      addch('\n');
      }
   else
#endif
      {
	  /* NCL: must use gets() since scanf() reads until error or EOF */
      gets(string);
      }
   }
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_line_in(string)
**          Input a line before the newline
**    PARAMETERS   
**       INPUT  : 
**          input    
**       OUTPUT :  
**          string                  character string input by user   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if (UU_COMP!=UU_WIN2K)
uig_line_in(string)
   char string[];
   {
   int  num, i;
	char	ch;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"**uig_line_in, tty_status=%d",tty_status));
	i = 0;
   if(tty_status == UU_FALSE)
      {
      echo();
#if UU_COMP==UU_VAXVMS
		gets(string);
#else
		ch = getch();
		while (ch != '\n')
		  {
			if ((ch==8)&&(i>0))	/* back space */
				i--;
			else
				string[i++] = ch;
			ch = getch();
		  }
		string[i] = '\0';
#endif
      noecho();
      }
   else
      {
      scanf("%c",&ch);
		while (ch != '\n')
		  {
			if ((ch==8)&&(i>0))	/* back space */
				i--;
			else
			   string[i++] = ch;
			scanf("%c",&ch);
		  }
		string[i] = '\0';
      }
   num = strlen(string);
	uu_dprint(UU_MTRC,(us,"string=%s,num=%d",string,num));
	uu_dexit;
	return(num);
   }
#endif
/*jkd32: listing file */
/*********************************************************************
**    I_FUNCTION     :  open_listing_file(fname)
**          Initialize output listing file.
**    PARAMETERS   
**       INPUT  : 
**          fname  					iges input file name
**          pgm    					Either IGES or STEP.
**       OUTPUT :  
**          none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void open_listing_file(fname,pgm)
char fname[];
char pgm[];
{
	int i;
	char inputname[62];
	UX_pathname listname,filename,dir;
/*
.....Create listing file name
*/
	ul_break_fname(fname, dir, filename);
	i = strcspn(filename, ".");
	if (i != 0) filename[i] = '\0';
	ul_build_full_fname(dir,filename,"lst",listname);
/*
.....Create listing file
*/
//	listfd = open(listname, _O_CREAT|_O_RDWR, _S_IREAD|_S_IWRITE);
	listfd = open(listname, _O_CREAT|_O_TRUNC|_O_WRONLY, _S_IWRITE);
	ul_short_filename(listname,inputname,60);
	if (listfd == -1)
	{
		printf("Error open listing file %s\n", inputname);
	}
	sprintf(p_buff, "Listing file: %s\n", inputname);
	uig_str_out(p_buff, UU_TRUE);
	if (strcmp(pgm,"STEP") == 0)
		uig_list_out("STEP Conversion Program\n", UU_FALSE);
	else
		uig_list_out("I G E S   C o n v e r s i o n   P r o g r a m\n", UU_FALSE);
	sprintf(p_buff, "Version %.3f\n", NCL_version);	 /*jkd43*/
	uig_list_out(p_buff, UU_FALSE);									 /*jkd43*/
	uig_list_out(" \n", UU_FALSE);
	ul_short_filename(fname,inputname,60);
	sprintf(p_buff, "Input File: %s\n", inputname);
	uig_list_out(p_buff, UU_FALSE);
	ul_date_and_time(inputname);
	sprintf(p_buff, "Conversion: %s\n", inputname);
	uig_list_out(p_buff, UU_FALSE);
}

/*jkd32: listing file */
/*********************************************************************
**    I_FUNCTION     :  uig_list_out(line,display)
**          Write line to listing file.
**    PARAMETERS   
**       INPUT  : 
**          line						string to write to file 
**          display						display on screen if true 
**       OUTPUT :  
**          none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_list_out(line,display)	
char line[];
UU_LOGICAL display;
{
   int i,n;
	int ll;
   char buff[400];  
	if (display)
/*		if (UW_MOTIF!=1) */
			uig_str_out(line,UU_TRUE);

	/* cpp: add code to account for the new fields */

   if (listfd > 0)
	{
		strcpy(buff, line);
		n = strlen(line);
		if (buff[n-1] == '\n')
		{
			if(ll = strlen(save_str) > 0)
			{
				n--;
				strcat(save_str, buff);
				ll = strlen(save_str);
				write(listfd, save_str, ll);
				save_str[0] = '\0';
			}
			else
			{
				write(listfd, buff, n);
/*
				if (n > 0)
				{
					if (n < 80)
					{
						for (i=n;i<80;i++) buff[i] = ' ';
					}
					buff[80] = '\n';
					write(listfd, buff, 81);
					save_str[0] = '\0';
				}
*/
			}
		}
		else
		{
			strcpy(save_str, buff);
		}
	}
}

/*********************************************************************
**    I_FUNCTION     :  uig_str_out(string,display)
**          Output a string
**    PARAMETERS   
**       INPUT  : 
**          string                  string to output
**          display                 logical to set whether message is 
**                                  displayed or just added to screen def.
**       OUTPUT :  
**          option                  users selection   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_str_out(string,display)
char string[];
UU_LOGICAL display;
{
	if (Iges_batch)
		return;
   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
#if (UU_COMP!=UU_WIN2K)
	if (UW_MOTIF==1) 
	{
		uig_mfstr_out(string,display);
		return;
	}
   if(tty_status == UU_FALSE)
      {
      printw(string);
      if(display == UU_TRUE) refresh();
      }
   else
      {
      printf(string);
      }
#else
	iges_wntstr_out(string,display);
#endif
   }


/*********************************************************************
**    I_FUNCTION     :  int uig_ans()
**          Get a yes/no response from the user.
**    PARAMETERS   
**       INPUT  : 
**          input    
**       OUTPUT :  
**          option                  users selection   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if (UU_COMP!=UU_WIN2K)
int uig_ans()
   {
   char c[5];
   int iret = -1;

#if UU_COMP==UU_VAXVMS
   short stat;

   /* termination mask for SYS$QIOW - all chars are terminators */
   unsigned long term_mask[2] = {0,0};

   /* i/o status buffer */
   short iosb[4];
#endif

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
   while (iret == -1)
   {
#if UU_COMP==UU_VAXVMS

   /* c[0] must be initialized becase SYS$QIOW reads just 8 bits
   ** into this 32 bit "buffer"
   */
   c[0] = 0;

   /* don't use getch() - it requires a <CR> */
   SYS$QIOW(0, UIG_tty_channel, (IO$_READVBLK | IO$M_NOECHO | 
      IO$M_NOFILTR), iosb, 0, 0, c, 1, 0, term_mask, 0, 0);
   stat = iosb[0];
   if (stat != SS$_NORMAL)
      printf("uig_ans: SYS$QIOW bad status %d", stat);
#else
   c[0] = getch();
#endif
/*   uig_clear(); */
   if(c[0] == 'y' || c[0] == 'Y')
      {
       iret = 0;
      }
   else if(c[0] == 'n' || c[0] == 'N')
      {
       iret = 1;
      }
   else
     {
     uig_str_out ("\nY or N answer required.", UU_TRUE);
     }
   }
   return(iret);
   }
#endif
/*********************************************************************
**    I_FUNCTION     :  UU_TRUEDOUBLE uig_atof(buff)
**          Convert ASCII string to a double precision number.
**    PARAMETERS   
**       INPUT  : 
**          buff                    character string     
**       OUTPUT :  
**          output
**    RETURNS      : double precision number
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_TRUEDOUBLE uig_atof(buff)
   char buff[];
   {
   int dloc,eloc,i,len;
   UU_TRUEDOUBLE dd,expn,power,tt;
   UU_TRUEDOUBLE ten = 10.0;
   UU_TRUEDOUBLE one = 1.0;
   UU_TRUEDOUBLE zero = 0.0;
   UU_TRUEDOUBLE one_ten = 0.1;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   /* NCL: added to check for a NULL string  - pulled 0.0 out of my hat */
   if (buff == NULL)
		return (0.0);

   len = strlen(buff);
   dloc = eloc = -1;

   /* locate decimal point and exponent if any */

   for(i=0;i<len;i++)
      {
      if(buff[i] == '.')
         {
         dloc = i;
         }
	  /*jkd59: allow 'e' as well as 'E' and 'D' */
      else if(buff[i] == 'E' || buff[i] == 'D' || 
		buff[i] == 'e' || buff[i] == 'd')
         {
         eloc = i;
         }
      }
   
   /* get power of ten */

   expn = zero;
   power = one;
   if(eloc >  0)
      {
      for(i=(len-1);i>eloc;i--)
         {
         if(buff[i] == '-')
            {
            expn = -expn;
            }
         else if(buff[i] != '+' && buff[i] != ' ')
            {
            dd = buff[i] - '0';
            expn = expn + dd * power;
            power = power * ten;
            }
         }
      tt = ten;
      dd = expn;
      expn = one;
      if(dd < zero)
         {
         dd = -dd;
         tt = one_ten;
         }
      if(fabs(dd-zero) > one_ten)
         {
         for(power=dd;power>zero;power=(power-one))
               expn = expn * tt;
         }
      len = eloc;
      }
   else
      {
      expn = one;
      eloc = len;
      }

   /* compute fractional part */

   tt = zero;
   power = one_ten;
	if(dloc != -1)
		{
   	if(len > dloc)
			{
			for(i=(dloc+1);i<eloc;i++)
				{
				if(buff[i] != ' ')
					{
					dd = buff[i] - '0';
					tt = tt + dd * power;
					power = power * one_ten;
					}
				}
			}
		}
	else
		{
		dloc = len;
		}
		

   /* compute integral part */

   power = one;
   if(dloc > 0)
      {
      for(i=(dloc-1);i>-1;i--)
         {
         if(buff[i] == '-')
            {
            tt = -tt;
            }
         else
            {
            if(buff[i] != ' ' && buff[i] != '+')
               {
               dd = buff[i] - '0';
               tt = tt + dd * power;
               power = power * ten;
               }
            }
         }
      }

   /* now apply exponent */

   tt = tt * expn;
   return(tt);

   }

/*********************************************************************
**    I_FUNCTION     :  int uig_get_file(prompt, flag)
**          Get a file for the application
**    PARAMETERS   
**       INPUT  : 
**          prompt                  prompt string     
**          flag                    access permission flag
**       OUTPUT :  
**          output
**    RETURNS      : file pointer
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if (UU_COMP!=UU_WIN2K)
int
uig_get_file(prompt, flag)
char prompt[];
int flag;
	{
	UX_pathname filename;
	int fd;
	char *tname;

/*------------------------------------------------------------------------
** Start of executable code
**----------------------------------------------------------------------*/

	uig_str_out(prompt,UU_TRUE);

	filename[0] = '\0';

	uig_str_in(filename);

	if(strlen(filename) < 1)
		{
		fd = -1;
		return(fd);
		}
	strcpy(ext_filename, filename);		/*jkd52: listing file */

	/*NCL: to handle writes to screen on vax correctly */
	uig_tty_mode(SCROLL);

	/* create an unblocked version of the iges input file called iges.tmp */
/*	uig_unblock(filename,"iges.tmp"); */
	tname = tempnam("c:\\tmp", "iges");
	strcpy(iges_tmpfile, tname);
	uig_unblock(filename, iges_tmpfile);

	if (access(filename,0) != 0)		/* file does'nt exist */
		{
		fd = -1;
		}
	else
		{
		fd = uig_open(filename, flag);
		}

	/*NCL: to handle writes to screen on vax correctly */
	uig_tty_mode(RESET);

	return(fd);
	}
#endif

/*********************************************************************
**    I_FUNCTION     :  uig_tran_vec(v1,t,v2)
**          Transform a vector using the IGES matrix t.
**    PARAMETERS   
**       INPUT  : 
**          v1                      input vector
**          t                       IGES transformation matirx
**       OUTPUT :  
**          v2                      ouput vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_tran_vec(v1,t,v2)
   UU_REAL v1[3];
   UU_REAL t[12];
   UU_REAL v2[3];
   {
   UU_REAL temp[3];
   int i,j,k;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   k = 0;
   for(i=0;i<3;i++)
      {
      temp[i] = 0.;
      for(j=0;j<3;j++)
         {
         temp[i] = temp[i] + t[j+k] * v1[j];
         }
      k = k + 4;
      }
   for(i=0;i<3;i++)
      v2[i] = temp[i];
   }


/*********************************************************************
**    I_FUNCTION     :  uig_tran_coor(pt1,t,pt2)
**          Transform a coordinate using the IGES matrix t.
**    PARAMETERS   
**       INPUT  : 
**          pt1                     input coordinate
**          t                       IGES transformation matirx
**       OUTPUT :  
**          pt2                     ouput coordinate
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_tran_coor(pt1,t,pt2)
   UU_REAL pt1[3];
   UU_REAL t[12];
   UU_REAL pt2[3];
   {
   UU_REAL temp[3];
   int i,j,k;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   k = 0;
   for(i=0;i<3;i++)
      {
      temp[i] = 0.;
      for(j=0;j<3;j++)
         {
         temp[i] = temp[i] + t[j+k] * pt1[j];
         }
      k = k + 4;
      }
   k = 3;
   for(i=0;i<3;i++)
      {
      pt2[i] = temp[i] + t[i+k];
      k = k + 3;
      }
   }

/*********************************************************************
**    I_FUNCTION     :  uig_get_addr(word,out)
**          Store address of arg word in integer out.
**    PARAMETERS   
**       INPUT  : 
**          word                    address of word.
**       OUTPUT :  
**          out                     integer containing address of word
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_get_addr(word,out)
   char *word;
   char **out;
   {

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   *out = word;
   }

/*********************************************************************
**    I_FUNCTION     :  uig_tty_mode(flag)
**          Set up tty for standard output.
**    PARAMETERS   
**       INPUT  : 
**          flag                    =STANDARD set up standard mode
**                                  =RESET end standard mode
**                                  =SETUP setup special terminal mode
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if (UU_COMP!=UU_WIN2K)
void uig_tty_mode(flag)
   int flag;
   {

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   switch(flag)
      {
      case INIT:
         initscr();
         crmode();
         erase();
         noecho();
         clearok(stdscr,FALSE);
         scrollok(stdscr,TRUE);
         move(1,1);
         refresh();
         tty_status = UU_FALSE;
         break;
      case SCROLL:
		 /** NCL: removed special case for scroll since window will scroll
				  as it is. - rah 
			NOTE: Vax needs this special case.
		 **/
#if UU_COMP == UU_VAXVMS
         standout();
         tty_status = UU_TRUE;
#else
         tty_status = UU_FALSE;
#endif
         break;
      case RESET:
         standend();
         endwin();
         initscr();
         crmode();
         erase();
         noecho();
         clearok(stdscr,FALSE);
         scrollok(stdscr,TRUE);
         move(1,1);
         refresh();
         tty_status = UU_FALSE;
         break;
      case STANDARD:
         uig_clear();
         move(0,0);
         refresh();
         endwin();
         tty_status = UU_TRUE;
         break;
      }
   }
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_trans_comp(t,type,vec)
**       Get the IGES transformation components specified by type.
**    PARAMETERS   
**       INPUT  : 
**          t                             IGES transformation matrix
**          type                          compoent required
**                                           1 = x-axis
**                                           2 = y-axis
**                                           3 = z-axis
**                                           4 = translation vector
**       OUTPUT :  
**          vec                           required component unit vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_trans_comp(t,type,vec)
   UU_REAL  t[12];
   int      type;
   UU_REAL  vec[3];

   {
   int i,j;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   j = type - 1;
   i = 0;
   while(i < 3)
      {
      vec[i] = t[j];
      i++;
      j = j + 4;
      }
   }
/*********************************************************************
**    I_FUNCTION     :  uig_cceqcc(cc1,cc2)
**       Compare two IGES coordinates for equality
**    PARAMETERS   
**       INPUT  : 
**          cc1,cc2                       IGES coordinates
**       OUTPUT :  
**          none
**    RETURNS      : TRUE if equal
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL uig_cceqcc(cc1,cc2)
   UU_REAL  cc1[3],cc2[3];

   {
   int i;
   UU_REAL dist;
   UU_REAL sqrt();

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   dist = 0.0;
   for(i=0;i<3;i++)
      {
      dist = dist + ((cc1[i] - cc2[i]) * (cc1[i] - cc2[i]));
      }
   dist = sqrt(dist);
   if(dist < 1.0e-4)
      {
      return(UU_TRUE);
      }  
   else
      {
      return(UU_FALSE);
      }
   }


/*********************************************************************
**    I_FUNCTION :  uig_time(sdatime)
**       Get system date and time string of the form:
**       "YYMMDD.HHNNSS"
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          char sdatime[29]   ascii date time format
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_time(sdatime)
char	*sdatime;

{
char *ctime();
/*
.....use "time_t" not "long" because this value could be different depend on version
.....in the new version of  time_t is equivalent to __time64_t. (64bits), but old vrsion
.....is 32bits
.....so just use time_t and not 'long' value for clock
*/
/*long clock;*/
time_t clock;
struct tm *ptm;
struct tm *localtime();
int	i;
int year;

	clock = time((time_t *) 0);
	ptm = localtime(&clock);
/*
.....Added support for Y2K
.....Bobby  -  2/7/2000
*/
	year = ptm->tm_year;
	if (year < 70) year = year + 2000;
	else if (year > 99) year = year + 1900;
	sprintf(sdatime,"%2d%2d%2d.%2d%2d%2d",year,ptm->tm_mon+1,
		ptm->tm_mday,ptm->tm_hour,ptm->tm_min,ptm->tm_sec);
	for (i=0; i<strlen(sdatime); i++)			/* change all the blanks to zero */
	  if (sdatime[i] == ' ')
		sdatime[i] = '0';

}

/*********************************************************************
**    I_FUNCTION     :  uig_unblock(file,tfile)
**       UNIVERSAL unblock file
**    PARAMETERS   
**       INPUT  : 
**          char *file                       blocked file name
**          char *tfile                      unblocked file name
**       OUTPUT :  
**          char *file             unblocked file name
**    RETURNS      : none
**    SIDE EFFECTS : if error occurs, *file is set to ""
**    WARNINGS     : none
*********************************************************************/

void uig_unblock(file,tfile)
char *file, *tfile;
	{
	char buff[1000];
	UX_pathname lst_file;
	UX_pathname tdir, tnam, tmpname;
	int nbytes;
	int infile, tmpfile;
/*
.....we need put this file in the seem directory as iges file
.....if it does not have its own path
*/
	ul_break_fname(tfile, tdir, tnam);
	if (tdir[0]=='\0')
	{
		ul_break_fname(file, tdir, tnam);
		if (tdir[0]!='\0')
		{
			ul_build_full_fname(tdir, tfile,"", tmpname);
		}
	}
	else
		strcpy(tmpname, tfile);
#if (UU_COMP!=UU_WIN2K)
	infile = open(file,O_RDONLY);
	tmpfile = creat(tmpname,0664);
#else
	infile = open(file, _O_RDONLY);
	tmpfile = open(tmpname, _O_CREAT|_O_RDWR, _S_IREAD|_S_IWRITE);
#endif
	if (tmpfile < 0 || infile < 0)
	{
		strcpy(file,"");
		if (infile>0)
			close (infile);
		if (tmpfile>0)
			close (tmpfile);
		return;
	}
/*
.....Put the name of the file into iges_fname.  JLS 7/26/99
*/
	strcpy(iges_fname,file);
/*
.....iges list file should put in the same directory as output unibase
.....directory if MOTIF
.....Yurong  2/25/98
*/
#if (UU_COMP!=UU_WIN2K)
	if (UW_MOTIF==1)
		uig_mfget_unifil(lst_file);
	else
		strcpy(lst_file, file);
#else
	iges_wntget_unifil(lst_file);
#endif
	if (strcmp(tfile,iges_tmpfile) == 0)
		open_listing_file(lst_file,"IGES");			/*jkd32 */

	uig_str_out("\nConverting to unblocked format \n", UU_TRUE);

	while ((nbytes = uig_readrec(infile,buff)) > 0)
	{
		if (write(tmpfile,buff,80) != 80)
		{
			strcpy(file,"");
			return;
		}
	}
	/* got EOF */

	close(infile);
	close(tmpfile);

	strcpy(file,tmpname);
	return;
}

/*********************************************************************
**    I_FUNCTION :  uig_numeric_str(str)
**       return the numeric portion of 'str'.  Checks for scientific too.
**    PARAMETERS   
**       INPUT  : 
**			str gnote string
**       OUTPUT :  
**    RETURNS      : char numstr[20]   numeric string
**                   NULL if error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
uig_numeric_str(str,strout)
char *str,strout[];
	{
	int count = 0, period_cntr = 0,
			i = 0;
	int status,	len;
	char *ptr;

   status = UU_SUCCESS;
	len = strlen(str);
	ptr = str;

	/* find first numeric character */
	while ((!isdigit(*ptr) && (*ptr != '+') && (*ptr != '-') && (*ptr != '.')) 
			&& ((len - count) > 0)) 
		{
		ptr++;
		count++;
		}
	/* local equals only the numeric string */
	if (count < len)
		{
		/**
		strout[i++] = *ptr++;
		**/

		while ((len - count) > 0)
			{
			if (*ptr == '.')
				period_cntr++;

			/* allow only one 'decimal' */
			if (period_cntr > 1)
				{
				strout[i] = '\0';
				break;
				}

			if (isdigit(*ptr) || (*ptr == '.'))
				{
				strout[i++] = *ptr++;
				count++;
				}
			/* check for scientific notation */
			else if ((*ptr == 'E') || (*ptr == 'D'))
				{
				strout[i++] = *ptr++;
				count++;

				/* next char must be a '-' or '+' */
				if (((len - count) > 0) && 
					((*ptr == '-') || (*ptr == '+') || (isdigit(*ptr))))
					{
					strout[i++] = *ptr++;
					count++;

					/* next characters must be numerics ... */
					if (!((len - count) > 0) && (isdigit(*ptr)))
					/*'E' of 'D' isn't scientific/out of room, reset, exit*/
						{
						i-=2;
						strout[i] = '\0';
						}
					else
						{
						/* disallow more periods */
						if (!period_cntr)
							period_cntr++;
						strout[i++] = *ptr++;
						count++;
						}
					}
				else
					{
					i--;
					strout[i] = '\0';
					break;
					}
				} /* end of scientific notation */
			else
				{
				strout[i] = '\0';
				break;
				}
			} /* while there is some string left to parse */

		strout[i] = '\0';
		return (status);
		} /* end of numeric values */
	else
		return UU_FAILURE;
	}

/*********************************************************************
**
**    E_FUNCTION     :  int uu_sys_err_recovery()
**       Default routine to recover when application runs out of dynamic memory
**
**    PARAMETERS   
**       INPUT  : 
**          subsys = subsystem number of error
**				errno1 = error number
**				actflag = if = 0 then return to caller, 
**								 = 1 then exit
**								 = -1 then jump to root
**				p1 = parameter 1
**				p2 = parameter 2
**
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
/*
.....I found nowhere called this function
*/
#if (UU_COMP!=UU_WIN2K)
void uu_sys_err_recovery(actflag, subsys, errno1, p1, p2)
int actflag;
int subsys;
int errno1;
int p1, p2;
	{
	char buf[80];

	tig_retrieve_errmsg0(subsys, errno1, buf);
	printf("UU_SYS_ERR_RECOVERY: %s\n", buf);
	if(actflag != 0)
		exit(999);
	}
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_unb_opnfil(filename)
**       UNIVERSAL unblock filea and open file
**    PARAMETERS   
**       INPUT  : 
**          char *filename                       blocked file name
**       OUTPUT :  
**    RETURNS      : status: 0 success
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
uig_unb_opnfil(filename)
char *filename;
{
	char p_buff[81];
	char *tname, fnam[42];
#if UU_COMP == UU_WIN2K
	UX_pathname localdir,envstr;
#endif

	ul_short_filename(filename,fnam,40);
#if UU_COMP == UU_WIN2K
	getcwd(localdir, UX_MAX_PATH_LEN);
	sprintf (envstr, "TMP=%s", localdir);
	_putenv(envstr);
	sprintf (envstr, "TEMP=%s", localdir);
	_putenv(envstr);
	sprintf (envstr, "TMPDIR=%s", localdir);
	_putenv(envstr);
	tname = tempnam(localdir, "iges");
#else
	tname = tempnam("c:\\tmp", "iges");
#endif
			
	if (iges_fd!=0)
	{
		_close(iges_fd);	
		iges_fd = 0;
		ux_delete0(iges_tmpfile);
		iges_tmpfile[0] = '\0';
	}
	strcpy(iges_tmpfile, tname);
	uig_unblock(filename, iges_tmpfile);
	if (access(filename,0) != 0)
		iges_fd = -1;
	else
		iges_fd = uig_open(filename, 0);
	if( iges_fd < 0 )
	{
		sprintf (p_buff, "\nError in opening file %s.\n", fnam);
		uig_error(p_buff);
		if (iges_tmpfile[0] != '\0')
			ux_delete0(iges_tmpfile);
		iges_tmpfile[0] = '\0';
		iges_fd = 0;
		return -1;
	}
	sect_ptr[0] = sect_ptr[1] = sect_ptr[2] = sect_ptr[3] = 0;
	uig_terminal_rec();
	if(sect_ptr[2] <= 0)
	{
		sprintf (p_buff, "\nError in reading file %s.\n", fnam);
		uig_error (p_buff);
		sprintf (p_buff, "Terminal sect says that size of directory sect <= 0.\n");
		uig_error (p_buff);
		return -1;
	}
	if(sect_ptr[3] < ((sect_ptr[2]/2)-1))
	{
		uig_error("\nError in Terminal block\n");
		uig_str_out("Size of Parameter section too small.\n");
		uig_str_out("Smallest possible size = (size of Directory section)/2 = (%d)/2\n", sect_ptr[2]);
		uig_str_out("Size of Parameter section = %d.\n", sect_ptr[3]);
		return -1;
	}
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  uig_readrec(infile)
**       Read the next record from the IGES file. Record can have
**       0, 1 or 2 end of line characters, so read 82 bytes and
**       reposition the file pointer to the start of the next record
**       if there are less than 2 end of line characters.
**    PARAMETERS
**       INPUT  :
**          int  infile    - blocked file.
**       OUTPUT :
**          char *buff     - record read.
**    RETURNS      : Number of bytes read.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_readrec(infile,buff)
int  infile;
char *buff;
{
	int nbytes, n;

	nbytes = read(infile,buff,82);
	if (nbytes>80)
	{
		n = 0;
		if (nbytes>81 && buff[81]>31) n--;
		if (buff[80]>31) n--;
		if (n)
		{
			lseek(infile,n,1);
		}
		nbytes = 80;
	}
	return(nbytes);
}
