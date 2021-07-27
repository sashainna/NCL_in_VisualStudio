/*********************************************************************
**    NAME         :  necmd.c
**       CONTAINS:
**       swinpt()
**       ncl_init_cmdbuf(cmdbuf)
**       ncl_init_cmdstr(cmdbuf)
**       ncl_add_cmdbuf(cmdbuf)
**       ncl_call(cmdbuf)
**       ncl_cmd_call(cmdbuf)
**       ncl_set_cur_cmdbuf(cmdbuf)
**       ncl_get_cur_cmdbuf(cmdbuf)
**       inibuf()
**       ncl_add_token(token, cmdbuf, terminator)
**       ncl_toggle_label()
**       ncl_edit_line(str)
**       ncl_edit_option()
**       ncl_get_edit_option()
**       ncl_get_cmdstr(cmdbuf, prompt_number)
**       ncl_get_numcmds()
**       ncl_get_curnum()
**       ncl_get_curcmd(str)
**       ncl_get_cmd(num, str)
**       ncl_reset_cmdbuf()
**       edtcbf(cont_line) ** COMMENTED OUT **
**       ncl_init(cmdbuf)
**       puterr(cout)
**			ncl_add_tlabel(cmdbuf,label,sub)
**			ncl_put_in_src(cmdbuf)
**       savecmd()
**       resetcmd()
**       ncl_call_input()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       necmd.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:27
*********************************************************************/
#include <errno.h>
#include <ctype.h>
#include "usysdef.h"
#include "udebug.h"
#include "usysg.h"
#include "ustdio.h"
#include "uhep.h"
#include "dinput.h"
#include "dasnog.h"
#include "dasg.h"
#include "dmark.h"
#include "mfort.h"
#include "mdebug.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclmodals.h"
#include "nclinp.h"
#include "lcom.h"

/*  this include is for inibuf to allow it to send an extra command to the
    tek 4107 terminal to set gamode to replace.
*/
/* #include "ws410x.h" */

int ncl_call();
void ncl_set_cmdmode();

static NCL_cmdbuf *cur_cmd_buf;
/* static UU_LOGICAL initial = UU_TRUE; */
static char l_cmd_str[NCL_MAX_COMLINE];
static UU_LOGICAL S_buf_max = UU_FALSE; 
static NCL_cmdbuf*  save_cmd_buf;
static int cmd_buf_saved = 0;
extern int NCL_cmdmod;
/*********************************************************************
**    E_FUNCTION     : swinpt()
**        Temporarilly switch input mode to terminal.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          none
**            
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void swinpt()

   {
   uu_denter(UU_MTRC,(us,"swinpt"));
   uu_dexit;
   }


/*********************************************************************
**    E_FUNCTION     : ncl_init_cmdbuf(cmdbuf)
**       Initilize NCL command buffer.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf               command buffer
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_init_cmdbuf(cmdbuf)
   NCL_cmdbuf *cmdbuf;

   {
   uu_denter(UU_MTRC,(us,"ncl_init_cmdbuf()"));
   cmdbuf->active = UU_FALSE;
   cmdbuf->cur_cmd = 0;
/*
.....Increased number of lines in cmdbuf to 25
*/
   cmdbuf->max_cmd = 25;
   cmdbuf->num_cmd = 0;
   cmdbuf->cur_str[0] = '\0';
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_cmdstr(cmdbuf)
**       Initilize NCL command string.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf               command buffer
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_init_cmdstr(cmdbuf)
   NCL_cmdbuf *cmdbuf;

   {
   uu_denter(UU_MTRC,(us,"ncl_init_cmdstr()"));
   cmdbuf->cur_str[0] = '\0';
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_add_cmdbuf(cmdbuf)
**       Add a command to the NCL command buffer.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf               command buffer
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_add_cmdbuf(cmdbuf)
   NCL_cmdbuf   *cmdbuf;

   {
   int len, i, ii;

   uu_denter(UU_MTRC,(us,"ncl_add_cmdbuf()"));

   len = strlen(cmdbuf->cur_str);
   ii = 0;
   if(len < 2) goto next;
   for(i=(len-1);i>0;i--)
      {
      if(cmdbuf->cur_str[i] != ' ')
         {
         cmdbuf->cur_str[i+1] = '\0';
         ii = i;
         goto next;
         }
      }
next:;
/*   if(len > 0 ) */
      {
      while(( cmdbuf->cur_str[ii] == ',' ) && ii > 0)
         {
         cmdbuf->cur_str[ii] = '\0';
         ii = ii - 1;
         }
      strcpy(cmdbuf->cmd[cmdbuf->num_cmd],cmdbuf->cur_str);
      cmdbuf->cur_str[0] = '\0';
      cmdbuf->num_cmd++;
      cmdbuf->active = UU_TRUE;
      if(cmdbuf->num_cmd > cmdbuf->max_cmd)
         {
/*
.....replace the ncl_call by display of an error
....."NESTED LINE TOO LONG; PLEASE BREAK IT UP "
         ncl_call(cmdbuf);
.....Set the flag S_buf_max so ncl_call will not be called for this command.
*/
				uu_uerror0 (UA_NCL, 19);
         	ncl_init_cmdbuf(cmdbuf);
				S_buf_max = UU_TRUE;
         }
      }
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_call(cmdbuf)
**       Call NCL with the current command buffer.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf                command buffer to process
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_call(cmdbuf)
   NCL_cmdbuf  *cmdbuf;

   {
	char str[NCL_MAX_COMLINE];
	UM_int2 ifl37, ifl25, ifl37_save, ifl25_save, ifl37_val, ifl25_val;  
	UM_int2 err;
	UM_int4 nline;
	int i, len,status;

	uu_denter(UU_MTRC,(us,"ncl_call()"));

	cur_cmd_buf = cmdbuf;
/*
....Dont call ncl_call if the limit for the no of lines in cmdbuf has reached
*/ 
	if(cmdbuf->num_cmd == 0 || S_buf_max) goto fini;
	for(i=0;i<cmdbuf->num_cmd;i++)
	{
		strcpy(str, cmdbuf->cmd[i]);
		ncl_edit_line(str);
		len = strlen(str);
		if(len > 0 ) strcpy(cmdbuf->cmd[i], str);
		else cmdbuf->cmd[i][0] = '\0';
	}

	err = 0;
/*
.....Added use of nline (current NCL part program line in CAM processor.
.....This helps the error routine ncl_do_err() better known when it really
.....has an error.  nline is the line number of the part program statement
.....in cmdbuf->cmd[].
*/
   getnln(&nline);
#if UU_COMP == UU_WIN2K
	ud_prmerr(" ");
#endif
/*
......ncl_get_cmdmode() return true if is interface generated command
*/
	if (ncl_get_cmdmode() == UU_TRUE)
	{
/*
.....sets the command insert mode but save it first
*/
		ifl37 = 37;
		ifl25 = 25;
		getifl(&ifl37, &ifl37_save);
		getifl(&ifl25, &ifl25_save);
		if (NCL_com_mode==0)
/* 
......INSERT OFF for interface generated command (overwrite)
*/
		{
			ifl37_val = 1;
			ifl25_val = 0;
			setins(&ifl37_val, &ifl25_val);
		}
		else if (NCL_com_mode==1)
/* 
......INSERT ON for interface generated command (Insert)
*/
		{
			ifl37_val = 2;
			ifl25_val = 2;
			setins(&ifl37_val, &ifl25_val);
		}
	}
	nclsys(&err);
	if (ncl_get_cmdmode() == UU_FALSE)
	{
/*
.....do not used this when multi-window is opened
*/
/*
.....we will use it even when multi-window is opened
*/
/*		if (uw_cmd_extend())
			goto fini;
*/
		if (err == 1) ncl_err_mode(cmdbuf, &nline); /* err from other than *run */
		else if (err == 2) ncl_cmd_mode();  /* error from *run icon */
	}
	else
	{
		ncl_set_cmdmode(UU_FALSE);
		setins(&ifl37_save, &ifl25_save);
	}
fini:;
/*
.....Reset this flag
*/
	status = err;
	S_buf_max = UU_FALSE;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cmd_call(cmdbuf)
**       Call NCL with the current command buffer.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf                command buffer to process
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_cmd_call(cmdbuf)
   NCL_cmdbuf  *cmdbuf;

   {
   UM_int2 err;

   uu_denter(UU_MTRC,(us,"ncl_cmd_call()"));
/*
.....when we call the command, reset command mode to 0 in order to accept
.....the single text input, after that reset command mode
*/
   err = 0;
   cmdbuf->cur_cmd = 0;
   cur_cmd_buf = cmdbuf;
   if(cmdbuf->num_cmd == 0) goto fini;

#if UU_COMP == UU_WIN2K
	ud_prmerr(" ");
#endif
   nclsys(&err);

fini:;
   uu_dexit;
   return(err);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_set_cur_cmdbuf(cmdbuf)
**       Set the current command buffer to CMDBUF.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf                command buffer to process
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_cur_cmdbuf(cmdbuf)
   NCL_cmdbuf  *cmdbuf;

   {
   uu_denter(UU_MTRC,(us,"ncl_set_cur_cmdbuf(cmdbuf=%x)", cmdbuf));

   cur_cmd_buf = cmdbuf;

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_cur_cmdbuf(cmdbuf)
**       Set CMDBUF to the current command buffer.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf                command buffer to process
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_cur_cmdbuf(cmdbuf)
   NCL_cmdbuf  *cmdbuf;

   {

   uu_denter(UU_MTRC,(us,"ncl_set_cur_cmdbuf(cmdbuf=%x)", cmdbuf));

   cmdbuf = cur_cmd_buf;

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : inibuf()
**       Initilize NCL command buffer.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void inibuf()

   {
/*   extern uw_41xxdat uw_41xx;	 common data for all parts of 41xx driver */

   uu_denter(UU_MTRC,(us,"inibuf()"));

/*  this code is to allow it to send an extra command to the
    tek 4107 terminal to set gamode to replace.
*/
	/* make space characters delete previous char in kbd input */
/*	uu_ttput(uw_41xx.ttfd,"\033MG",3);  set graphics area writing mode */
/*	uw_ws410xinteg(0);		    to REPLACE */
   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : ncl_add_token(cmdbuf, token, terminator)
**       Add a token to the current string and write to command buffer
**       if character cout exceeded.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf         current command buffer
**          token          token to be added to the string
**          terminator     UU_TRUE   add a ","
**          terminator     UU_FALSE  no not add the terminator
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_token(cmdbuf, token, terminator)
   NCL_cmdbuf   *cmdbuf;
   char *token;
   UU_LOGICAL terminator;

   {
   char blank;
   int i, ii, str_len, token_len;

   uu_denter(UU_MTRC,(us,"ncl_add_token()"));

   strcpy(l_cmd_str, token);
   blank = ' ';
   token_len = strlen(token);
   ii = 0;
	for(i=(token_len-1);i>0;i--)
	{
/*
.....use 'l_cmd_str' instead of 'token' because
.....'token' may pass as const value and can't assaign
.....any value (such as  ncl_add_token(&cmdbuf, "(PT/", NCL_nocomma);)
.....hit fatal error if not change here for WIN2K
*/
/*      if(token[i] != blank)
         {
         token[i+1] = '\0';
*/
		if(l_cmd_str[i] != blank)
		{
			l_cmd_str[i+1] = '\0';
			ii = i;
			goto next;
		}
	}
next:;
/*   token_len = strlen(token); */
	token_len = strlen(l_cmd_str); 
	str_len = strlen(cmdbuf->cur_str);
/*
.....don't use NCL_MAX_COMLINE
.....but use actual coomand line length allowed (UL_line_len)
*/
/*	if((str_len + token_len) > NCL_MAX_COMLINE - 12)  */
	if(str_len != 0 && (str_len + token_len) >= UL_line_len-1)
	{
		strcat(cmdbuf->cur_str, "$");
		ncl_add_cmdbuf(cmdbuf);
		cmdbuf->cur_str[0] = '\0';
	}
	strcat(cmdbuf->cur_str, l_cmd_str);
	if(terminator)
	{
		if(token[ii] != ',') strcat(cmdbuf->cur_str,",");
	}
   uu_dexit;
   return(0);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_toggle_label()
**       Toggle NCL automatic label option
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_toggle_label()
   {

   uu_denter(UU_MTRC,(us,"ncl_toggle_label()"));

   NCL_auto_label = !NCL_auto_label;
	if (NCL_auto_label) ud_prmerr("Auto naming ENABLED.");
	else ud_prmerr("Auto naming DISABLED.");

   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : ncl_edit_line(str)
**       Edit an NCL command line
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_edit_line(str)
   char str[NCL_MAX_COMLINE];

   {
   char inbuf[NCL_MAX_COMLINE];
   char buf[NCL_MAX_COMLINE+40];
   int count, i;
   int retstat;
   int numint;
   int status;

   uu_denter(UU_MTRC,(us,"ncl_edit_line()"));

   status = 0;
   if(NCL_edit_mode == UU_FALSE) goto exit;

   count = strlen(str);
   /* setup default response  */
   if  (count == 0)
      {
      return (0);
      }
   else
      {
      for (i=0; i<count; i++) inbuf[i] = str[i];
      inbuf[count] = '\0';
      }

   /* let DAS  prompt user for any modifications */

   status = ud_string_def(UA_NCL, 425, inbuf, NCL_MAX_COMLINE,&numint,&retstat);
   sprintf(buf,"edit numit = %d string = %s", numint, inbuf);
   um_pscroll(buf);

   if(numint == 0)
      {
      str[0] = '\0';
      }
   else
      {
      for (i=0; i<numint; i++) str[i] = inbuf[i];
      str[numint] = '\0';
      }
exit:;
   uu_dexit;
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_edit_option()
**       Set NCL edit option
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_edit_option()
   {

   uu_denter(UU_MTRC,(us,"ncl_edit_option()"));

   NCL_edit_mode = !NCL_edit_mode;

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_edit_option()
**       Set NCL edit option
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
ncl_get_edit_option()
   {

   uu_denter(UU_MTRC,(us,"ncl_get_edit_option()"));

   uu_dexit;
   return(NCL_edit_mode);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_get_cmdstr(cmdbuf, prompt_number)
**       Get a string from the user and put into the command buffer.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf                        current command buffer
**          prompt_number                 prompt number
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
ncl_get_cmdstr(cmdbuf, prompt_number)
   NCL_cmdbuf  *cmdbuf;
   int prompt_number;
   {
   int numint;                /* number of das entries returned */
   UU_LOGICAL ret;
   char str[NCL_MAX_COMLINE];

   uu_denter( UU_MTRC,(us,"ncl_get_cmdstr()"));
   ncl_init_cmdbuf(cmdbuf);

   /* let DAS  prompt user for input */

   ud_ldas(UD_DASSTRING,  UA_NCL, 427,
         str, NCL_MAX_COMLINE, &numint, UD_NODEFAULT);

   if(numint > 0)
      {
      ncl_add_token(cmdbuf, str, UU_FALSE);
      ncl_add_cmdbuf(cmdbuf);
      um_pscroll(str);
      ret = UU_TRUE;
      }
   else
      {
      ret = UU_FALSE;
      }
   uu_dexit;
   return(ret);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_get_numcmds()
**       Get number of commands in current buffer
**    PARAMETERS   
**       INPUT  :
**          cmdbuf         current command buffer
**       OUTPUT :
**          none
**    RETURNS      : number of commands in current buffer
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_numcmds()
   {

   uu_denter( UU_MTRC,(us,"ncl_get_numcmds()"));

   uu_dexit;

   return(cur_cmd_buf->num_cmd);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_get_curnum()
**       Get the index of the current command line in current buffer
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : number of commands in current buffer
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_curnum()
   {

   uu_denter( UU_MTRC,(us,"ncl_get_curnum()"));

   uu_dexit;

   return(cur_cmd_buf->cur_cmd);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_get_curcmd( str)
**       Get current command from command buffer
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          str            current command line
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_curcmd(str)
   char  str[NCL_MAX_COMLINE];
   {

   uu_denter( UU_MTRC,(us,"ncl_get_curcmd()"));

   strcpy(str, cur_cmd_buf->cmd[cur_cmd_buf->cur_cmd]);
   cur_cmd_buf->cur_cmd++;

   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : ncl_get_cmd(num, str)
**       Get a line from the current buffer
**    PARAMETERS   
**       INPUT  :
**          num            number of command line to return
**       OUTPUT :
**          str            command line
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_cmd(num, str)
   int num;
   char  str[NCL_MAX_COMLINE];
   {

   uu_denter( UU_MTRC,(us,"ncl_get_cmd()"));

   strcpy(str, cur_cmd_buf->cmd[num]);

   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : ncl_reset_cmdbuf()
**       Reset current command buffer
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : number of commands in current buffer
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_reset_cmdbuf()
   {

   uu_denter( UU_MTRC,(us,"ncl_rest_cmdbuf()"));

   cur_cmd_buf->cur_cmd = 0;
   cur_cmd_buf->num_cmd = 0;
   cur_cmd_buf->cur_str[0] = '\0';
   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : edtcbf(cont_line)
**         Allow user to edit lines in the current command buffer
**         between (cur_cmd - cont_line) and num_cmd.
**    PARAMETERS
**       INPUT  :
**          cont_line               line number of source line
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     :
**        contents of current command buffer will be altered
*********************************************************************/
#ifdef OLDNCL
edtcbf(cont_line)
   UM_int2 *cont_line;

   {
   int line_num;
   int start_line;
   int end_line;
   UD_DASTAT retstat;
   int numint;
   char line[NCL_MAX_COMLINE];

   uu_denter(UU_MTRC,(us,"edtcbf(cont_line=%d)", *cont_line));

   uu_dprint(UU_MTRC,(us,"..addr=%x, cur=%d, num=%d", cur_cmd_buf,
      cur_cmd_buf->cur_cmd, cur_cmd_buf->num_cmd));

   line_num = *cont_line;
   line_num = cur_cmd_buf->cur_cmd - line_num;
   if (line_num < 0) line_num = 0;
   start_line = line_num;
   end_line = cur_cmd_buf->num_cmd - 1;

   uu_dprint(UU_MTRC,(us,"..start=%d,end=%d",start_line, end_line));   
   do
      {
      /* get current source line */
      strcpy(line, cur_cmd_buf->cmd[line_num]);

      /* let user modify line */
      ud_string_def(UA_NCL, 425, line, NCL_MAX_COMLINE, &numint, &retstat);
   uu_dprint(UU_MTRC,(us,"ud_string_def, stat=%d, line=%s", retstat, line));
      strcpy(cur_cmd_buf->cmd[line_num], line);

      if(retstat == DE_UPFIELD)
         {
         line_num--;
         if (line_num < start_line) line_num = start_line;
         }
      else /*if(retstat == DE_DNFIELD)*/
         {
         line_num++;
         if (line_num > end_line) line_num = start_line;
         }
      }
   while (retstat != DE_DONE);
   cur_cmd_buf->cur_cmd = start_line;

   uu_dexit;
   }
#endif /*OLDNCL*/
/*********************************************************************
**    E_FUNCTION     : ncl_init()
**       Dummy call to NCL to make first real call faster.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_init(cmdbuf)
   NCL_cmdbuf *cmdbuf;
   {
   UM_int2 one = 0;

   uu_denter( UU_MTRC,(us,"ncl_init()"));

   ncl_init_cmdbuf(cmdbuf);
   cmdbuf->cur_cmd = 1;
   cmdbuf->num_cmd = 1;
   ncl_set_cur_cmdbuf(cmdbuf);
   nclsys(&one);

   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : ncl_del_token(cmdbuf, token, terminator)
**       Delete a token from the current string
**    PARAMETERS   
**       INPUT  :
**          cmdbuf         current command buffer
**          token          token to be deleted from the string
**          terminator     UU_TRUE   delete a ","
**          terminator     UU_FALSE  no not delete the terminator
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_del_token(cmdbuf, token, terminator)
NCL_cmdbuf   *cmdbuf;
char *token;
UU_LOGICAL terminator;
	{
	char blank, ltoken[80];
	int i, ii, str_len, token_len;
	/**
	char *token;
	**/

	uu_denter(UU_MTRC,(us,"ncl_del_token()"));

	/** 1). get length of string
		2). get length of token
		3). if token length exceeds string length report error
		3a). if terminator, remove it
		4). for length of token, decrease length of string
		5). if terminator, remove it
	**/
	/**
	token = l_cmd_str;
	**/
	blank = ' ';
	ii = 0;
	token_len = strlen(token);

	/** strip trailing blanks from token **/
/*
.....use 'ltoken' instead of 'token' because
.....'token' may pass as const value and can't assaign
.....any value (such as  ncl_del_token(&cmdbuf,"", UU_TRUE);)
*/
	strcpy(ltoken, token);
	for (i = (token_len-1); i > 0; i--)
		{
		if (ltoken[i] != blank)
			{
			ltoken[i+1] = '\0';
			ii = i;
			goto next;
			}
		}
next:;
	token_len = strlen(ltoken);
	str_len = strlen(cmdbuf->cur_str);

	/** if cmdbuf string has trailing , remove it **/
/*
.....Sometimes there is more than one ',', remove those too.
.....JLS 2/22/99
	if (cmdbuf->cur_str[str_len - 1] == ',')
		str_len -= 1;
*/
	while (cmdbuf->cur_str[str_len - 1] == ',')
		str_len -= 1;

	if(str_len < token_len)
		{
		/** cmd spanning multiply lines ? **/
		/** for now return equiv of rej op **/
		return (1);
		}

	/** decrease cur_string **/
	cmdbuf->cur_str[str_len - token_len] = '\0';

	if(terminator)
		{
		str_len = strlen(cmdbuf->cur_str);
		if(token[ii] != ',') 
			cmdbuf->cur_str[str_len] = '\0';
		}
	uu_dexit;
	return(0);
	}
/*********************************************************************
**    E_FUNCTION     : puterr(cout,knc)
**       Fortran callable routine to output error message dialog.
**    PARAMETERS   
**       INPUT  :
**          cout           Error message to output.
**          knc            Number of chars in 'cout'.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void puterr(cout,knc)
char *cout;
UM_int4 *knc;
{
	cout[*knc] = '\0';
	ud_wrerr(cout);
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_cmdmode(val)
**       Let's the fortran routines and 'ncl_call' that this is a
**			interface generated command.  Errors are handled differently
**			when the command is generated by the interface.
**    PARAMETERS   
**       INPUT  :
**          val            0 = Reset command mode, 1 = Set, -1 = Returns
**                         command mode
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_cmdmode(val)
int val;
{
	UM_int2 ifl,ival;
	ifl = 343; ival = val;
	setifl(&ifl,&ival);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_cmdmode()
**       Returns the command mode setting for interface routines.
**    PARAMETERS   
**       INPUT  :
**				none
**       OUTPUT :
**          none
**    RETURNS      : UU_TRUE if in interface generated command
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_cmdmode()
{
	UM_int2 ifl,ival;
	ifl = 343;
	getifl(&ifl,&ival);
	if (ival == 1) return(UU_TRUE);
	else return(UU_FALSE);
}
/*********************************************************************
**    E_FUNCTION     : ncl_add_tlabel(cmdbuf,label,sub)
**       Places the assignment label, subscript, and '=' within the
**			current command buffer.
**			processing it.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf         current command buffer
**				label				label of entity
**				sub				label's subscript
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_add_tlabel(cmdbuf,label,sub,ent)
NCL_cmdbuf *cmdbuf;
char *label,*ent;
int sub;
{
	char buf[20];
	ncl_add_token(cmdbuf,label,NCL_nocomma);
	if (sub > 0)
	{
		sprintf(buf,"(%d)",sub);
		ncl_add_token(cmdbuf,buf,NCL_nocomma);
	}
	ncl_add_token(cmdbuf,"=",NCL_nocomma);
	ncl_add_token(cmdbuf,ent,NCL_nocomma);
}

/*********************************************************************
**    E_FUNCTION     : ncl_put_in_src(cmdbuf)
**       Places the command buffer into the part program file without
**			processing it.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf         current command buffer
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_put_in_src(cmdbuf)
NCL_cmdbuf cmdbuf;
{
int i,n;
UM_f77_str fcmd;
/*
.....Place in w2 (Fortran) array
.....for placement into 'pp' file
*/
	for (i=0; i<cmdbuf.num_cmd; i++)
	{
		n = strlen(cmdbuf.cmd[i]);
		UM_init_f77_str(fcmd,cmdbuf.cmd[i],NCL_MAX_COMLINE);
		putinw(UM_addr_of_f77_str(fcmd),&n,&i);
	}
	putinf(&cmdbuf.num_cmd);
	return;
}
/*********************************************************************
**    E_FUNCTION     : savecmd
**       save the current command struct.
**			
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void savecmd()
{
	if ((cur_cmd_buf!=NULL)&&(cur_cmd_buf->num_cmd!=0))
	{
		save_cmd_buf = cur_cmd_buf;
		cmd_buf_saved = 1;
	}
}
/*********************************************************************
**    E_FUNCTION     : resetcmd
**       reset the current command struct.
**			
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void resetcmd()
{
	if (cmd_buf_saved)
	{
		cur_cmd_buf = save_cmd_buf;
		cmd_buf_saved = 0;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_call_input(cmdbuf,flg)
**       Places the command buffer into the part program file without
**			processing it.  Mimics *INPUT.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf  -  current command buffer
**          flag    -  execute command flag
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_call_input(cmdbuf,flg)
NCL_cmdbuf *cmdbuf;
UU_LOGICAL flg;
{
	UM_int2 ifl37,ifl150,tfl37,tfl150;
/*
.....Turn on input mode so the command is not executed
*/
	if (flg)
	{
		getinp(&ifl37,&ifl150);
		tfl37 = 4; tfl150 = 1;
		setinp(&tfl37,&tfl150);
	}
	ncl_call(cmdbuf);
	if (flg) setinp(&ifl37,&ifl150);
	return;
}
