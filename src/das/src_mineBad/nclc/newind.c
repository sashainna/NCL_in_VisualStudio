/*********************************************************************
**    NAME         :  newind.c
**       CONTAINS:
**         getlin(bufsiz, buf)
**         ptmsgu(buf, buflen)
**         opnwin
**         winopn(iopen)
**         cmdmod(yesno)
**         clswin
**         nclpmt(message, answer)
**         ncl_windout
**         nclwsz(lines)
**         ncl_wind_init
**         ncl_cmd_mode()
**         ncl_err_mode(cmdbuf,nline)
**         ncl_do_cmd(cmdbuf,nline)
**         ncl_do_err(cmdbuf,nline)
**         ncl_write_cmd_buf(cmdbuf)
**         ncl_write_pp(str)
**         edtlin(line_no, num_chars, str)
**         nclctm()
**         nclrtm()
**         nclf_flush_buffer()
**         ncl_set_statact()
**			ncl_cmd_key(cmdbuf)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       newind.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:01
*********************************************************************/
#include <errno.h>
#include <ctype.h>
#include "usysdef.h"
#include "lcom.h"
#include "udebug.h"
#include "uhep.h"
#include "usysg.h"
#include "ustdio.h"
#include "uims.h"
#include "dinput.h"
#include "dasnog.h"
#include "dasg.h"
#include "dmark.h"
#include "driver.h"
#include "dwindow.h"
#include "go.h"
#include "gtblopst.h"
#include "gtblws.h"
#include "gtblst.h"
#include "gtblvar4.h"
#include "ginqdsiz.h"
#include "mfort.h"
#include "mdebug.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"

/* #define BUFSIZE   200 */
#define    eind      event.indata

static int NCL_digs_seg = -1;
static UU_LOGICAL wind_active = UU_FALSE;
static UU_LOGICAL wind_cmd_mode = UU_FALSE;
static int markval;

void ncl_windout();
void ncl_wind_init();
void ncl_write_cmd_buf();
void ncl_write_pp();

int NCL_cmdmod = 0;
NCL_cmdbuf Sncl_cmdbuf;
/*********************************************************************
**    E_FUNCTION     : getlin(bufsiz, buf)
**         Prompt the user to enter an NCL command. If the buffer (BUF)
**         is not empty (i.e. BUFSIZ > 0), it will be used for a default
**         response. Otherwise, there will be no default response. The
**         string entered by the user is blank filled (to a maximum of UL_line_len
**         characters) and returned.
**    PARAMETERS
**       INPUT  :
**          bufsiz               0 => no default string
**            buf                  default string
**       OUTPUT :
**          bufsiz               actual length of input string
**            buf                  input string (blank filled to
**                                 UL_line_len characters)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void getlin(bufsiz, buf)
UM_int2 *bufsiz;
char *buf;
{
   int numint,nc;
   int retstat;
   int user_default;
   char inbuf[NCL_MAX_COMLINE];
   char *cstr;
   int i,j;
   int num_cmd, cur_cmd;

   uu_denter(UU_MTRC,(us,"getlin(bufsiz=%d, buf=%x)", *bufsiz, buf));

   cstr = buf;
   num_cmd = ncl_get_numcmds();
   if(num_cmd > 0)
      {
      cur_cmd = ncl_get_curnum();
      if(cur_cmd != num_cmd)
         {
         ncl_get_curcmd(inbuf);
         numint = strlen(inbuf);
/*         if(numint <= 0) goto repeater; */
         um_pscroll("getlin:");
         um_pscroll(inbuf);
         goto scopy;
         }
      else
         {
         ncl_reset_cmdbuf();
         strcpy(inbuf,"*return");
         um_pscroll("getlin:");
         um_pscroll(inbuf);
         goto scopy;
         }
      }

   /* setup default response  */
   if  (*bufsiz <= 0) user_default = UD_NODEFAULT;
   else user_default = UD_DEFAULT;
   if( *bufsiz > 0)
      {
		nc = *bufsiz;
		strncpy(inbuf,cstr,nc);
      inbuf[nc] = '\0';
      um_pscroll(inbuf);
      }

   /* let DAS  prompt user for input (with/without default response) */
   if  ( user_default == UD_NODEFAULT)
      {
      ud_ldas(UD_DASSTRING, /* Enter NCL command */ UA_NCL, 401,
            inbuf, NCL_MAX_COMLINE, &numint, user_default);
      }
   else
      {   
      ud_string_def(UA_NCL, 425, inbuf, NCL_MAX_COMLINE, &numint, &retstat);
      }
   if (numint <= 0) strcpy(inbuf, "*return");

scopy:
   /* copy input string and blank fill to UL_line_len characters */
   for (i=0; ((inbuf[i] != '\0') && (i < NCL_MAX_COMLINE)); i++) cstr[i] = inbuf[i];
   *bufsiz = i;

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ptmsgu(buf, buflen)
**       Write a message (BUF of length BUFLEN).
**    PARAMETERS
**       INPUT  :
**          buf                     character buffer (no terminator)
**          buflen                  length of buffer
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ptmsgu(buf, buflen)
   UM_f77_str_ptr buf;
   UM_int2 *buflen;

   {
   int i,nchar;
   char msg[256];
   char *cstr;

   uu_denter(UU_MTRC,(us,"ptmsgu(buf=%x, buflen=%d)",
      buf, *buflen));

   if( wind_active == UU_TRUE)
      {
      cstr = UM_cstr_of_f77_str(buf);
      nchar = *buflen;
      if (nchar > 0)
         {
         if (nchar > NCL_MAX_COMLINE) nchar=NCL_MAX_COMLINE;
         for (i=(nchar-1); i>0; i--)
            {
            if(cstr[i] != ' ') break;
            }
         nchar = i+1;
         for (i=0; i<nchar; i++) msg[i] = cstr[i];
         msg[nchar] = '\0';
         ncl_windout(msg, 0);
         }
      }
   uu_dexit;
   }

/**************************************************************************
**  E_FUNCTION:  opnwin
**      FORTRAN callable routine to open the DIGS scrolling window.
**  PARAMETERS   
**      INPUT :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void opnwin()
	{
	int seg_typ, stat;

/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
*/
	uu_denter( UU_MTRC,(us,"opnwin"));

/*	UD_MARK(markval, UU_FALSE); */
	markval=0;
	if(markval == 0)
	{
		if(wind_active == UU_FALSE)
		{
			gqopenstr(&seg_typ, &NCL_digs_seg);
			if(NCL_digs_seg > 0)
			{
				gcloseseg();
			}
			wind_active = UU_TRUE;
			ncl_wind_init();
		}
	}
	else
	{
		if(wind_active == UU_TRUE && wind_cmd_mode == UU_FALSE)
		{
			stat = ul_close_window();
			if (stat==1)
			{
				wind_active = UU_FALSE;
				if(NCL_digs_seg > 0)
				{
					gopenseg(NCL_digs_seg);
					NCL_digs_seg = -1;
				}
			}
/*		UD_UNMARK(markval); */
		}
	}
	uu_dexit;
}

/**************************************************************************
**  E_FUNCTION:  winopn(iopen)
**      FORTRAN callable routine to report whether command window
**              is open.
**  PARAMETERS
**      INPUT :
**          none
**      OUTPUT :
**          iopen  - 0 if window is closed, 1 if window is open.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void winopn(iopen)
UM_int2 *iopen;
   {

   uu_denter( UU_MTRC,(us,"winopn"));

   *iopen=0;
   if(wind_active == UU_TRUE) *iopen=1;

   uu_dexit;
   }
/**************************************************************************
**  E_FUNCTION:  cmdmod(yesno)
**      FORTRAN callable routine to report whether in command mode or not
**  PARAMETERS
**      INPUT :
**          none
**      OUTPUT :
**          yesno  - 0 if in command mode, 1 if not in command mode
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void cmdmod(yesno)
UM_int2 *yesno;
   {

   uu_denter( UU_MTRC,(us,"cmdmod"));

   *yesno=1;
   if(wind_cmd_mode == UU_TRUE) *yesno=0;

   uu_dexit;
   }
/**************************************************************************
**  E_FUNCTION:  clswin
**      FORTRAN callable routine to close the DIGS scrolling window.
**  PARAMETERS
**      INPUT :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void clswin()
{
	int stat;
/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
*/
   uu_denter( UU_MTRC,(us,"clswin"));

	if(wind_active == UU_TRUE && wind_cmd_mode == UU_FALSE)
	{
		stat = ul_close_window();
		if (stat==1)
		{
			wind_active = UU_FALSE;
			if(NCL_digs_seg > 0)
			{
				gopenseg(NCL_digs_seg);
				NCL_digs_seg = -1;
			}
		}
	}
/*   UD_UNMARK(markval); */
   uu_dexit;
}
/*********************************************************************
**    E_FUNCTION     : nclpmt(message, answer)
**         A FORTRAN callable routine to obtain a DAS string response.
**    PARAMETERS
**       INPUT  :
**          message               prompt string
**       OUTPUT :
**          answer               return string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclpmt(message, ncp, answer, nco)
UM_f77_str_ptr message;
UM_f77_str_ptr answer;
int *ncp,*nco;
{
	int numint, length;
	UD_STRREC strrec;
	char inbuf[NCL_MAX_COMLINE];
	char msgbuf[80];
	char *astr;
	char *mstr;
	int i,j;

	mstr = UM_cstr_of_f77_str(message);
	astr = UM_cstr_of_f77_str(answer);
	strncpy(msgbuf,mstr,*ncp);
	msgbuf[*ncp] = '\0';

	inbuf[0] = '\0';
	strrec.instring = inbuf;
	ud_ddas(UD_DASSTRINGDEF, msgbuf, &strrec,
		NCL_MAX_COMLINE, &numint, UD_DEFAULT);

/*
.....Copy input string and blank fill to UL_line_len characters
*/
	*nco = strlen(inbuf);
	if (*nco > NCL_MAX_COMLINE) *nco = NCL_MAX_COMLINE;
	strncpy(astr,inbuf,*nco);
	astr[*nco] = '\0';
}

/**************************************************************************
**  E_FUNCTION:  ncl_windout
**      Write a string from NCL
**  PARAMETERS
**      INPUT  :  buff :   string to be written out
**      OUTPUT :  start :  if == 0 reset line counter
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void ncl_windout(buff, start)
   char *buff;          /*  string to be output  */
   int  start;          /*  if == 0 a new command reset  */
   {
#define	field	7		/*	size of tab field	*/
#define tab		9		/*	tab character	*/
   char buff3[250];		/*	buff with tabs converted to spaces	*/
   int	length;			/*	length of string buff2	*/
   int	i, j, k;		/*	indices used in tabs conversion	*/
   int	r;				/*	remainder in modulus function in tabs conversion	*/
   int	nspaces;		/*  # of spaces replacing tab in tabs conversion	*/

/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
** Write the string to the current output device
**
*/
   uu_denter( UU_MTRC,(us,"ncl_windout()"));

/*
..... Convert tabs to appropriate # of spaces before output to window
..... Sharon - 20JUN91
*/
	length = strlen(buff);
	j = 0;
	for (i=0; i<length; i++)
	{
		if (buff[i] != tab)            /* check for tabs */
		{
			buff3[j] = buff[i];        /* not a tab */
			j++;
		}
		else
		{
			r = j - field*(j/field);	/* remainder = mod(j,field)	*/
			nspaces = field - r;		/* field - remainder	*/
			for (k=j; nspaces-->0; k++)
			{
				buff3[k] = ' ';         /* store spaces in place of tab */
			}
			j = j + (field-r);
		}
	}
	buff3[j] = '\0';
/*
.....Write out line
*/
   ul_win_out(buff3,start);
   uu_dexit;
   }

/**************************************************************************
**  E_FUNCTION:  nclwsz(nline)
**      Return number of lines in the current NCL text window
**  PARAMETERS
**      INPUT  :  none
**      OUTPUT :  nline - Integer 2 - Number of lines in the current NCL text
**                                    window.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void nclwsz(nline)
   UM_int2 *nline;
   {
/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
*/
	if(wind_active == UU_TRUE)
	{
/*
.....Just return the 'glines' variable
*/
		*nline = glines;
	}
}

/**************************************************************************
**  E_FUNCTION:  ncl_wind_init
**      Initialize NCL text window
**  PARAMETERS
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void ncl_wind_init()
   {
   UM_int2 numlin;                  /* number of lines in text window */
	int wargs[3];

/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
** Open window, initialize file store pointers call routine to read and
**   process user input
**
*/
   uu_denter( UU_MTRC,(us,"ncl_wind_init()"));

   nclwsz(&numlin);
	wargs[1] = 1;
   ul_open_window(glines,80,wargs);
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_cmd_mode()
**       Enter NCL command mode.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cmd_mode()
   {
   int done;
   UM_int4 nline;

   done = 0;

   if(NCL_cmd_window_mode)
   {
      wind_active = UU_TRUE;
      wind_cmd_mode = UU_TRUE;
      ncl_wind_init();
   }
/*
.....Added use of nline (current NCL part program line in CAM processor.
.....This helps the error routine ncl_do_err() better known when it really
.....has an error.  nline is the line number of the part program statement
.....in cmdbuf->cmd[].
*/
   while (!done)
      {
      done = ncl_do_cmd(&Sncl_cmdbuf,&nline);
      if (!done) done = ncl_do_err(&Sncl_cmdbuf, nline);
      }
   }

/*********************************************************************
**    E_FUNCTION     : ncl_err_mode(cmdbuf)
**       Allow user to correct a statement in error and go into command mode.
**    PARAMETERS
**       INPUT  :
**          cmdbuf   -  cmd buf containing the statement in error
**          nline    -  line number of statement with error in cmdbuf.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_err_mode(cmdbuf,nline)
NCL_cmdbuf *cmdbuf;
UM_int4 *nline;

   {
   int done;

   done = 0;

   if(NCL_cmd_window_mode)
   {
      wind_active = UU_TRUE;
      wind_cmd_mode = UU_TRUE;
      ncl_wind_init();
   }

/*
.....Added use of nline (current NCL part program line in CAM processor.
.....This helps the error routine ncl_do_err() better known when it really
.....has an error.  nline is the line number of the part program statement
.....in cmdbuf->cmd[].
*/
   while (!done)
      {
      done = ncl_do_err(cmdbuf,*nline);
      if (!done) done = ncl_do_cmd(cmdbuf,nline);
      }
   }

/*********************************************************************
**    E_FUNCTION     : ncl_do_cmd(cmdbuf,nline)
**       Enter NCL command mode.
**    PARAMETERS
**       INPUT  :
**          cmdbuf  -  cmd buf used to build statements in.
**       OUTPUT :
**          nline   - line number of current command buffer
**    RETURNS      : 0 if error occurred
**                   1 if user ended with done or reject op
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_do_cmd(cmdbuf,nline)
NCL_cmdbuf *cmdbuf;
UM_int4 *nline;
{
	int noerr, status, numint, i, stat, sav_cmdmod;
	UD_DASTAT retstat;
	UU_LOGICAL NCL_cmd_window_mode_save;
	int NCL_com_mode_save;
	UM_int2 nchar;
	UM_int4 line_no;
	UM_f77_str line;
	char  inbuf[NCL_MAX_COMLINE], prompt[80];
	UD_STRREC strrec;

	uu_denter( UU_MTRC,(us,"ncl_do_cmd()"));

	noerr = 1;
	NCL_cmd_window_mode_save = NCL_cmd_window_mode;
	NCL_com_mode_save = NCL_com_mode;
	sav_cmdmod = NCL_cmdmod;
	UD_MARK(markval, UU_FALSE);
	if(markval == 0)
	{
		UM_init_f77_str(line, inbuf, NCL_MAX_COMLINE);

		ncl_init_cmdbuf(cmdbuf);
		line_no = 0;
start:;
      /* get current source line */
#if UU_COMP == UU_CIM
		gtpsrc(&line_no, &nchar, inbuf);
#else
		gtpsrc(&line_no, &nchar, UM_addr_of_f77_str(line));
#endif

/*
.....Initialize nline to current line number
*/
		*nline = line_no;
		inbuf[nchar] = '\0';

		if (nchar==0)
			nchar = 0;
loop:;
/* get input from user */
		sprintf(prompt,"edit line %d: ", line_no);
		strrec.instring = inbuf;
		NCL_cmdmod = 1;
		ud_ddas(UD_DASSTRINGDEF, prompt, &strrec,
			NCL_MAX_COMLINE, &numint, UD_DEFAULT);
		NCL_cmdmod = sav_cmdmod;
		inbuf[numint]='\0';
		retstat = strrec.termcon;
/*
..... Up arrow
*/
		if(retstat == DE_UPFIELD)
		{
			inbuf[numint] = '\0';
/*
.....If there was something on the line, save it in the command buffer.
*/
			if (numint > 0)
			{
				strcpy(cmdbuf->cmd[cmdbuf->cur_cmd], inbuf);
				if (cmdbuf->num_cmd <= cmdbuf->cur_cmd) cmdbuf->num_cmd++;
			}
/*
.....If we are at the first line of the command buffer, write the command
.....buffer to the pp file, call uparrw to set nline correctly and go to
.....get the next line.
*/
			if (cmdbuf->cur_cmd == 0)
			{
				if (cmdbuf->num_cmd > 0)
				{
					ncl_write_cmd_buf(cmdbuf);
					cmdbuf->num_cmd = 0;
				}
				uparrw();
				line_no = 0;
				goto start;
			}
/*
.....We are not at the beginning of the command buffer, move to previous line.
*/
			cmdbuf->cur_cmd--;
			strcpy(inbuf, cmdbuf->cmd[cmdbuf->cur_cmd]);
			line_no--;
			goto loop;
		}
/*
..... Down arrow
*/
		if(retstat == DE_DNFIELD)
		{
			inbuf[numint] = '\0';
/*
.....If there was something on the line, save it in the command buffer.
*/
			if (numint > 0)
			{
				strcpy(cmdbuf->cmd[cmdbuf->cur_cmd], inbuf);
				if (cmdbuf->num_cmd <= cmdbuf->cur_cmd) cmdbuf->num_cmd++;
			}
/*
.....If the command buffer is empty, call dnarrw to set nline correctly
.....and go to get the next line.
*/
			if (cmdbuf->num_cmd == 0)
			{
				dnarrw();
				line_no = 0;
				goto start;
			}
/*
.....If we are at the last line of the command buffer, write the command
.....buffer to the pp file, call dnarrw to set nline correctly and go to
.....get the next line.
*/
			if (cmdbuf->cur_cmd == cmdbuf->num_cmd-1)
			{
				cmdbuf->cur_cmd = 0;
				ncl_write_cmd_buf(cmdbuf);
				for (i=0; i<cmdbuf->num_cmd; i++) dnarrw();
				cmdbuf->num_cmd = 0;
				line_no = 0;
				goto start;
			}
/*
.....We are not at the end of the command buffer, move to next line.
*/
			cmdbuf->cur_cmd++;
			strcpy(inbuf, cmdbuf->cmd[cmdbuf->cur_cmd]);
			line_no++;
			goto loop;
		}
/*
.....The user entered data
*/
		if(numint > 0 && retstat != DE_DONE)
		{
			strcpy(cmdbuf->cur_str, inbuf);
/*
.....Check for continuation line
*/
			if (ncl_check_cont(inbuf, numint))
			{
				strcpy(cmdbuf->cmd[cmdbuf->cur_cmd], inbuf);
				if (cmdbuf->cur_cmd < cmdbuf->max_cmd)
				{
					if (cmdbuf->num_cmd <= cmdbuf->cur_cmd) cmdbuf->num_cmd++;
					cmdbuf->cur_cmd++;
				}
				line_no++;
				goto start;
			}
			if (cmdbuf->cur_cmd >= cmdbuf->num_cmd) ncl_add_cmdbuf(cmdbuf);
			status = NCL_OKINPUT;
		}
/*
.....The user entered done or no data
*/
		else
		{
			status = NCL_NOINPUT;
		}

      /* pass line to NCL for processing */

		if(status == NCL_OKINPUT)
		{
			if (ncl_cmd_call(cmdbuf)==1) noerr = 0;
			if(wind_active && !NCL_cmd_window_mode) 
            {
				NCL_cmd_window_mode = UU_TRUE;
            }
			if (noerr) 
            {
				ncl_init_cmdbuf(cmdbuf);
				line_no = 0;
				goto start;
			}
		}
		else
		{
			if (wind_active) 
			{
                stat = ul_close_window();
				if (stat==1)
					wind_active = UU_FALSE;
      		}
			wind_cmd_mode = UU_FALSE;
		}
	}
	else
	{
		if(wind_active == UU_TRUE)
		{
			stat = ul_close_window();
			if (stat==1)
				wind_active = UU_FALSE;
		}
		wind_cmd_mode = UU_FALSE;
	}
	NCL_cmd_window_mode = NCL_cmd_window_mode_save;
	NCL_com_mode = NCL_com_mode_save;
	NCL_cmdmod = sav_cmdmod;
	UD_UNMARK(markval);
	uu_dexit;
	return(noerr);
}

/*********************************************************************
**    E_FUNCTION     : ncl_do_err(sav_cmd_buf,nline)
**       Allow user to correct an error in the cmdbuf
**    PARAMETERS
**       INPUT  :
**          sav_cmd_buf  -  cmdbuf containing statement in error.
**          nline        -  CAM line number of statement in sav_cmd_buf
**       OUTPUT :
**          none
**    RETURNS      : 0  - error was successfully corrected.
**                   1  - user hit reject op
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_do_err(sav_cur_buf,nline)
NCL_cmdbuf *sav_cur_buf;
UM_int4 nline;					/* Command line of sav_cur_buf->cmd[] */

{
   int done, numint, i, j, stat, sav_cmdmod;
   UD_DASTAT retstat;
   UU_LOGICAL  NCL_cmd_window_mode_save, cmdflg, clear_error=UU_TRUE;
   UM_int4 line_no;
   UM_int2 nchar, idx, imp,l2;
   UM_f77_str line, line_sav;
   char  inbuf[NCL_MAX_COMLINE], prompt[80];
   UD_STRREC strrec;

   uu_denter( UU_MTRC,(us,"ncl_do_err()"));

	done = 0;
/*
.....do not used this when multi-window is opened
*/
	if (uw_cmd_extend())
		return done;
	NCL_cmd_window_mode_save = NCL_cmd_window_mode;
	sav_cmdmod = NCL_cmdmod;
	UD_MARK(markval, UU_FALSE);
	if(markval == 0)
	{
		UM_init_f77_str(line, inbuf, NCL_MAX_COMLINE);
		idx=123;
		getifl(&idx,&l2);
		line_no = l2;
/*
...
...Added by Paul to check if we are in implied motion. We need this
...to fix the problem with multiple command lines ($) when they are
...not shown on the screen in case of error. See few lines bellow.
...05/06/93
...
*/
		idx=215;
		getifl(&idx,&imp);
/*...*/
		getnln(&line_sav);
		sav_cur_buf->cur_cmd = 0;
		line_no = (int)line_sav;
		cmdflg = sav_cur_buf->num_cmd > 0;

		while (cmdflg)
		{
/*
.........If line number of buffer passed in is different than
.........the line number current in the CAM processor, get out.
.........We do not belong here in this case.
*/
/*
...
...05/06/93
...Additionaly changed by Paul. original version was:
...if (line_no != nline)
...
*/
			if (line_no != nline && imp != 0)
			{
				goto exit;
			}
			else
			{
				strcpy(inbuf, sav_cur_buf->cmd[sav_cur_buf->cur_cmd]);
				sprintf(prompt,"edit line %d: ", line_no);
			}
			strrec.instring = inbuf;
			NCL_cmdmod = 1;
			ud_ddas(UD_DASSTRINGDEF, prompt, &strrec,
				NCL_MAX_COMLINE, &numint, UD_DEFAULT);
			NCL_cmdmod = sav_cmdmod;
			if (clear_error)
			{
				ud_prmerr("");
				clear_error = UU_FALSE;
			}
			retstat = strrec.termcon;
			if(retstat == DE_UPFIELD)
			{
				strcpy(sav_cur_buf->cmd[sav_cur_buf->cur_cmd], inbuf);
				if (sav_cur_buf->cur_cmd == 0)
				{
					ncl_write_cmd_buf(sav_cur_buf);
					uparrw();
					cmdflg = UU_FALSE;
				}
				else
				{
					sav_cur_buf->cur_cmd--;
/*
.....We must still decrement the line count
.....even if we are still in the stored command
.....buffer
.....Bobby  -  9/15/93
*/
					line_no--;
				}
			}
			else if(retstat == DE_DNFIELD)
			{
				strcpy(sav_cur_buf->cmd[sav_cur_buf->cur_cmd], inbuf);
				if (sav_cur_buf->cur_cmd == sav_cur_buf->num_cmd-1)
				{
					sav_cur_buf->cur_cmd = 0;
					ncl_write_cmd_buf(sav_cur_buf);
					for (i=0; (i<sav_cur_buf->num_cmd); i++) dnarrw();
					cmdflg = UU_FALSE;
				}
				else
				{
					sav_cur_buf->cur_cmd++;
					line_no++;
				}
            }
			else if (retstat == DE_DONE)
			{
				if(wind_active == UU_TRUE)
				{
					stat = ul_close_window();
					if (stat==1)
					{
						wind_active = UU_FALSE;
					}
					wind_cmd_mode = UU_FALSE;
				}
				done = 1;
				cmdflg = UU_FALSE;
			}
			else if(numint > 0)
			{
                /*  check for * or **.     */
				j=0;
				for (i=0; (i<nchar && inbuf[i] == ' '); i++) ;
				if (inbuf[i] == '*' )
				{
					for (j=i+1; (j<nchar && inbuf[j] == ' '); j++) ;
				}             
				if (inbuf[i] != '*' || inbuf[j] == '*')
				{
					strcpy(sav_cur_buf->cmd[sav_cur_buf->cur_cmd], inbuf);
/*
................If no error in ncl_cmd_call() set cmdflg to FALSE.
................This will cause control to return from this subroutine -
................error was resolved.
*/
					if (!ncl_check_cont(inbuf, numint))
					{
						sav_cur_buf->cur_cmd = 0;
						if (ncl_cmd_call(sav_cur_buf)!=1)
							cmdflg = UU_FALSE;
						else
						{
							line_no = (int)line_sav;
							sav_cur_buf->cur_cmd = 0;
							clear_error = UU_TRUE;
						}
					}
					else
					{
						line_no++;
						sav_cur_buf->cur_cmd++;
						if (sav_cur_buf->cur_cmd >= sav_cur_buf->num_cmd)
						{
							gtpsrc(&line_no, &nchar, UM_addr_of_f77_str(line));
							inbuf[nchar] = '\0';
							strcpy(sav_cur_buf->cmd[sav_cur_buf->cur_cmd], inbuf);
							sav_cur_buf->num_cmd = sav_cur_buf->cur_cmd + 1;
						}
					}
				}
				else
				{
					ncl_init_cmdbuf(sav_cur_buf);
					strcpy(sav_cur_buf->cur_str,inbuf);
					ncl_add_cmdbuf(sav_cur_buf);
/*
................If no error in ncl_cmd_call() set cmdflg to FALSE.
................This will cause control to return from this subroutine -
................error was resolved.
*/
					if (ncl_cmd_call(sav_cur_buf)!=1) cmdflg = UU_FALSE;
				}
			}
		}
	}
	else
	{
		if(wind_active == UU_TRUE)
		{
			stat = ul_close_window();
			if (stat==1)
			{
				 wind_active = UU_FALSE;
			}
			wind_cmd_mode = UU_FALSE;
			done = 1;
		}
	}
exit:;
	NCL_cmd_window_mode = NCL_cmd_window_mode_save;
	NCL_cmdmod = sav_cmdmod;
	UD_UNMARK(markval);
	uu_dexit;
	return(done);
}

/*********************************************************************
**    E_FUNCTION     : ncl_write_cmd_buf (cmdbuf)
**         Write all lines of a cmdbuf to the part program.
**    PARAMETERS
**       INPUT  :
**          cmdbuf   - cmdbuf to write.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_write_cmd_buf(cmdbuf)
   NCL_cmdbuf *cmdbuf;
   {
   int i,j,k;
   char inbuf[NCL_MAX_COMLINE];
   UM_f77_str line;
   UM_int2 nchar;
   UM_int4 line_no;

   uu_denter(UU_MTRC,(us,"ncl_write_cmd_buf(line_no=%d)",(line_no)));

   UM_init_f77_str(line,inbuf,NCL_MAX_COMLINE);

   getnln(&line_no);  /* get nline - current line number in part program */

   for (k=cmdbuf->cur_cmd; k<cmdbuf->num_cmd; k++)
      {
      strcpy(inbuf, cmdbuf->cmd[k]);
      nchar=strlen(inbuf);
         /*  check for * or **. Do nothing if *.    */
      j=0;
      for (i=0; (i<nchar && inbuf[i] == ' '); i++) ;
      if (inbuf[i] == '*' )
         {
         for (j=i+1; (j<nchar && inbuf[j] == ' '); j++) ;
         }             
      if (inbuf[i] != '*' || inbuf[j] == '*' || k>0)
         {
         if (i<nchar)
            {    /* if line is non-blank, blank fill and put in source file  */
            for (j=nchar; j<NCL_MAX_COMLINE; j++) inbuf[j] = ' ';
            ptpsrc(&line_no, &nchar, UM_addr_of_f77_str(line));
            }
         line_no++;
         }
      }

   uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : ncl_write_pp(str)
**         Write a line to the part program at the current value of 
**         nline. Do nothing if the line begins with a single star.
**    PARAMETERS
**       INPUT  :
**          str     - string to write.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_write_pp(str)
   char str[NCL_MAX_COMLINE];
   {
   int i,j;
   char inbuf[NCL_MAX_COMLINE];
   UM_f77_str line;
   UM_int2 nchar;
   UM_int4 line_no;

   uu_denter(UU_MTRC,(us,"ncl_write_pp"));

   UM_init_f77_str(line,inbuf,NCL_MAX_COMLINE);

   strcpy(inbuf, str);
   nchar=strlen(inbuf);
            /*  check for * or **. Do nothing if *.    */
   j=0;
   for (i=0; (i<nchar && inbuf[i] == ' '); i++) ;
   if (inbuf[i] == '*' )
      {
      for (j=i+1; (j<nchar && inbuf[j] == ' '); j++) ;
      }             
   if (inbuf[i] != '*' || inbuf[j] == '*')
      {
/*
.....If line is non-blank put in source file
*/
      if (i<nchar)
      {
         line_no=0;    /* 0 means put at current line number */
         ptpsrc(&line_no, &nchar, UM_addr_of_f77_str(line));
         }
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : edtlin(line_no)
**         Allow user to edit a source lines.
**    PARAMETERS
**       INPUT  :
**          line_no               starting line number in source file.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void edtlin(line_no)
   UM_int4 *line_no;

   {
   UD_DASTAT retstat;
   char inbuf[NCL_MAX_COMLINE], prompt[80];
   int numint,j;
   UD_STRREC strrec;
   UM_f77_str line;
   UM_int2 nchar;

   uu_denter(UU_MTRC,(us,"edtlin(line_no=%d)",*line_no));

   UM_init_f77_str(line,inbuf,NCL_MAX_COMLINE);

#if UU_COMP == UU_CIM
   gtpsrc(&line_no, &nchar, inbuf);
#else
   gtpsrc(line_no, &nchar, UM_addr_of_f77_str(line));
#endif
   inbuf[nchar] = '\0';
   sprintf(prompt,"edit line %d: ", *line_no);
   strrec.instring =inbuf;
      ud_ddas(UD_DASSTRINGDEF, prompt, &strrec,
          NCL_MAX_COMLINE, &numint, UD_DEFAULT);
   retstat = strrec.termcon;
   if (numint > 0 && retstat != DE_DONE)
      {
/*
.....Put in source file
*/
      nchar=numint;
      ptpsrc(line_no, &nchar, UM_addr_of_f77_str(line));
      }

   uu_dexit;
   }
/**************************************************************************
**  E_FUNCTION:  nclctm()
**      
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
nclctm()
   {
   UU_LOGICAL status;
/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
*/
   uu_denter( UU_MTRC,(us,"ncl_clear_for_os()"));
   status = UU_TRUE;
   uu_dexit;
   return(status);
   }
/**************************************************************************
**  E_FUNCTION:  nclrtm()
**      
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
nclrtm()
   {
   UU_LOGICAL status;
/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
*/
   uu_denter( UU_MTRC,(us,"ncl_clear_for_os()"));
   status = UU_TRUE;
   uu_dexit;
   return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_check_cont(inbuf)
**         Check if input line is a continuation line
**    PARAMETERS
**       INPUT  :
**          inptr   - Pointer to input line
**       OUTPUT :
**          none
**    RETURNS      : UU_TRUE if continuation line, else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_check_cont(inptr,numchr)
char *inptr;
int numchr;
{
	int j;
	UU_LOGICAL log_flg, qflg, cont;
	char tmpstr[8], *inp;
/*
.....Check for continuation line
*/
	log_flg = 0;
	qflg = 0;
	cont = UU_FALSE;
	for (j=0,inp=inptr;j<numchr;j++,inp++)
	{
		if (*inp == '\'') log_flg = 1 - log_flg;
		if (*inp == '"') qflg = 1 - qflg;
		if (log_flg == 0 && qflg==0)
		{
			if (*inp == '%') break;
			if (*inp == '$')
			{
				if (*(inp+1) != '$') cont = UU_TRUE;
				break;
			}
		}
	}
	if (cont)
	{
		for (j=0,inp=inptr;j<6 && isalpha(*inp);j++, inp++)
		{
			tmpstr[j] = toupper(*inp);
		}
		tmpstr[j] = '\0';
		if (strcmp(tmpstr,"PARTNO")==0 ||
		    strcmp(tmpstr,"INSERT")==0 ||
		    strcmp(tmpstr,"PPRINT")==0 ||
		    strcmp(tmpstr,"REMARK")==0 ||
		    strcmp(tmpstr,"LOADU")==0 ||
		    strcmp(tmpstr,"UBFN")==0 ||
		    strcmp(tmpstr,"LETTER")==0) 
		{
			cont = UU_FALSE;
		}
	}
	return (cont);
}

/*********************************************************************
**    E_FUNCTION     : nclf_flush_buffer()
**         Flushes the output buffer.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_flush_buffer()
{
	ud_updatews(UG_SUPPRESS);
}
/*********************************************************************
**    E_FUNCTION     : ncl_set_statact(flag)
**         set the status window active flag
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_statact(flag)
int flag;
{
	wind_active = flag;
} 
/*********************************************************************
**    E_FUNCTION     :  ncl_cmd_key(cmdbuf)
**         execute the command cmdbuf
**    PARAMETERS
**       INPUT  :
**          cmdbuf: command to be executed
**       OUTPUT :
**          none
**    RETURNS      : 1: no error, run success, 0: error, -1: continue line
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cmd_key(cmdbuf)
char *cmdbuf;
{
	int noerr, status, numint, i, stat, sav_cmdmod;
	UD_DASTAT retstat;
	UU_LOGICAL NCL_cmd_window_mode_save;
	int NCL_com_mode_save;
	UM_int2 nchar;
	UM_int4 line_no;

	noerr = 1;
	NCL_cmd_window_mode_save = NCL_cmd_window_mode;
	NCL_com_mode_save = NCL_com_mode;

	sav_cmdmod = NCL_cmdmod;
	NCL_cmdmod = 0;
	numint = strlen(cmdbuf);
/*
.....remove end '\r\n' and trailing spaces
*/
	if (numint>0)
	{
		for (i=numint-1;i>=0;i--)
		{
			if ((cmdbuf[i]=='\r')||(cmdbuf[i]=='\n')||(cmdbuf[i]=='\t')||(cmdbuf[i]==' '))
				continue;
			break;
		}
		cmdbuf[i+1]='\0';
	}
	numint = strlen(cmdbuf);
	if (numint==0)
		goto done;
	UD_MARK(markval, UU_FALSE);
	if(markval == 0)
	{
/*
.....The user entered data
*/
		strcpy(Sncl_cmdbuf.cur_str, cmdbuf);
/*
.....Check for continuation line
*/
		if (ncl_check_cont(cmdbuf, numint))
		{
			strcpy(Sncl_cmdbuf.cmd[Sncl_cmdbuf.cur_cmd], cmdbuf);
			if (Sncl_cmdbuf.cur_cmd < Sncl_cmdbuf.max_cmd)
			{
				if (Sncl_cmdbuf.num_cmd <= Sncl_cmdbuf.cur_cmd) Sncl_cmdbuf.num_cmd++;
				Sncl_cmdbuf.cur_cmd++;
			}
			noerr = -1;
			goto done;
		}
		if (Sncl_cmdbuf.cur_cmd >= Sncl_cmdbuf.num_cmd) ncl_add_cmdbuf(&Sncl_cmdbuf);
		if (ncl_cmd_call(&Sncl_cmdbuf)==1) noerr = 0;
		if(wind_active && !NCL_cmd_window_mode) 
		{
			NCL_cmd_window_mode = UU_TRUE;
		}
		ncl_init_cmdbuf(&Sncl_cmdbuf);
	}
done:;
	NCL_cmd_window_mode = NCL_cmd_window_mode_save;
	NCL_com_mode = NCL_com_mode_save;
	NCL_cmdmod = sav_cmdmod;
	UD_UNMARK(markval);
	return noerr;
}

void ncl_reset_cmdline()
{
	Sncl_cmdbuf.cur_cmd = 0;
	Sncl_cmdbuf.num_cmd = 0;
}
