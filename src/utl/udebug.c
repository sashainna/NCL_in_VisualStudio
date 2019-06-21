/*********************************************************************
**
**    NAME         :  udebug.c -- UNICAD debugging tools
**       uu_enter() -- called on entry to a subroutine.
**			uu_exit() -- called on exit from a subroutine.
**			uu_trap() -- set up exception handling.
**    	uu_prtstk() -- print procedure stack.
**    	uu_trcstk() -- print procedure stack on trc file.
**			uu_debug_off() -- turn trace off
**			uu_trcfile() -- return file pointer of open trc file.
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       udebug.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:52
**
*********************************************************************/

#define UU_DEBUGMAIN 1
#ifdef UU_IRIX64
#include <sys/sysmips.h>
#endif
#include "ustdio.h"
#include "usignal.h"
#include "usysdef.h"
#include "udebug.h"
#include "lcom.h"

/********************************************************************* 
**  E_FUNCTION: UU_ENTER, UU_EXIT, UU_TRAP -- exception handling routines
**
** description: 
**	Call UU_TRAP() at the beginning of your main program. This will
**	initialize the exception handling system. Should an exception occur,
**	the process will be terminated gracefully (files closed).
**	    If you wish to know where you were when the exception occurred, 
**	call UU_ENTER(s) whenever you enter an important
**	subroutine. S is a character string (such as the name
**	of the routine and perhaps its arguments). Whenever you exit the subroutine
**	call UU_EXIT(). You must pair up calls to UU_ENTER and UU_EXIT correctly.
**	If there is an exception, such as illegal address, illegal instruction,
**	floating point exception, bus error, operator hits ctrl c, etc.,
**	the current stack of strings passed to UU_ENTER will be printed, and the
**	process exited gracefully.
**
**  PARAMETERS   
**      input:  UENTER(s) s==pointer to character string.
**      output: none
**  RETURNS      :  severity of error.
**  SIDE EFFECTS :  Prints stack of strings on an exception.
**  WARNINGS     :  The maximum depth of UENTER calls is 25. Exceeding
**							this causes termination of the process with
**							a message. The maximum length of the character string
**							parameter to UENTER is 99. Exceeding this limit
**							causes the string to be truncated to 80 characters.
*********************************************************************/

static int (*emergencyclose)() = NULL;		/* Emergency close function */
static int trc=2;					/* Means trc file isn't used */
static int trap=1;				/* Means uu_trap() hasn't been called */
static FILE *fd;					/* Pointer to trc file */
static char *buff;				/* (circular) buffer pointer */
static int bufsize=0;			/* size of buffer */
static int bufpos;				/* where next string goes in buff */

uu_enter(s)							/* Pushes string s on stack */
char s[];
{
	int len;
	char msg[256];

	/* Initialize trapping */
	if( trap==1 ) {
		uu_trap();
	}

	/* if buffer not active, Print the string on trace file 
	(check for mask has been made by uu_denter macro). Else put in buff */
	if (bufsize>0) uu_tobuff(s);
	else uu_trcprint(s);

	UU_stklen++;
	if (UU_stklen>=UU_STKDEPTH) {
		sprintf(msg, "\n uu_enter stack overflow. length=%d\n",UU_stklen);
		ud_printmsg(msg);
		if( emergencyclose  != NULL ) (*emergencyclose)();
#if UU_COMP==UU_VAXVMS
		ul_reset_control();
		unauth (&UL_cam,&UL_cad);	/* unauthorize terminal */
#endif
		exit(12);
	}
	len=strlen(s);
	if (len>148)  len=148;					/* use at most 148 chars of s */
	strncpy(&ustk[UU_stklen-1][0],s,len);
	ustk[UU_stklen-1][len] = '\0';
	addstk[UU_stklen-1] = s;
}

/*********************************************************************
**    E_FUNCTION     :  uu_exit(s)
**			This function is called by the "uu_dexit" debugging macro when
**			the debug stack has been found to be corrupted.  This is not
**			a fatal error.  The function prints the stack, but lets 
**			execution continue.
**    PARAMETERS   
**       INPUT  : 
**          char *s		The automatic variable us from the function
**								where the debug stack was found to be corrupt.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Prints current (incorrect) debug stack in the
**							trc file.
**    WARNINGS     : none
*********************************************************************/
uu_exit(s)
char *s;
{
	char *p,*getenv(), lmsg[1000], msg[256];
	FILE *uu_opentrc();

	/* Initialize trapping */
	if( trap==1 ) {
		uu_trap();
	}

	/* Open trc file */
	if( trc==1 ) {
		uu_opentrc();
	}

	if (trc == 0)
	{
		if(UU_stklen <= 0 ) {
			sprintf(msg, "\nuu_exit called with UU_stklen=%d\n",UU_stklen);
			ud_printmsg(msg);
			fprintf(fd,"\nuu_exit called with UU_stklen=%d\n",UU_stklen);
		}

		else if(addstk[UU_stklen] != s) {
			lmsg[0] = '\0';
			sprintf(msg, "\ndenter/dexit mismatch detected\n");
			strcat(lmsg, msg);
			sprintf(msg, "UU_stklen = %d\n",UU_stklen);
			strcat(lmsg, msg);
			sprintf(msg, "add = %x, us = %x\n",addstk[UU_stklen],s);
			strcat(lmsg, msg);
			sprintf(msg, "us = %s\n",s);
			strcat(lmsg, msg);
			if (UU_stklen>0)
			{
				sprintf(msg, "stktop = %s\n",&ustk[UU_stklen-1][0]);
				strcat(lmsg, msg);
			}
			ud_printmsg(lmsg);
	
			fprintf(fd,"\ndenter/dexit mismatch detected\n");
			fprintf(fd,"UU_stklen = %d\n",UU_stklen);
			fprintf(fd,"add = %x, us = %x\n",addstk[UU_stklen],s);
			fprintf(fd,"us = %s\n",s);
			if (UU_stklen>0)
				fprintf(fd,"stktop = %s\n",&ustk[UU_stklen-1][0]);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  int uu_trcprint() -- print string to trc file
**    PARAMETERS   
**       INPUT  :  us		Character string to print
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_trcprint(s)
char s[];
{
	int i;
	char *p;
	char *getenv();
	FILE *uu_opentrc();

	if( trc==1 ) { 				/* Open trc file, if necessary */
		uu_opentrc();
	}

	if (trc == 0)
	{
		/* Print the string (check for mask has been made by uu_dprint macro */
		fprintf(fd,"%2d",UU_stklen);	
		for(i=0; i<UU_stklen; ++i) putc(' ', fd);
		fprintf(fd,"%s\n",s);
		fflush(fd);
	}
}

/******************* uu_tobuff(s) **************************/
uu_tobuff(s) 						/* put string s in buff */
char *s;
{
	int len;
	if (bufsize>0) {
		for (len=0; (s[len]!='\0')&&(len<400); len++) {
			buff[bufpos++]=s[len];
			if (bufpos>=bufsize) bufpos=0;
		}
		buff[bufpos++]='\n'; 			/* add newline */
		if (bufpos>=bufsize) bufpos=0;
	}
}

/*********************************************************************
**    E_FUNCTION :  uu_trcbuf() -- print buff to trace file.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_trcbuf()
{
	int endpos,indx;
	char *p;
	char *getenv();
	FILE *uu_opentrc();

	if( trc==1 ) { 				/* Open trc file, if necessary */
		uu_opentrc();
	}

	if (bufsize>0 && trc == 0) {
		fprintf(fd,"%d character buffer:\n",bufsize);
		endpos=bufpos-1; if (endpos<0) endpos=bufsize-1;
		for (indx=bufpos; indx!=endpos; indx=(indx+1)%bufsize) {
			if (buff[indx]) putc(buff[indx],fd);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  int uu_prtstk() -- print procedure stack.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_prtstk()
{
	int i;
	char msg[256];
	sprintf(msg, " The procedure stack is length %d\n",UU_stklen);
	ud_printmsg(msg);
	for (i=0; i<UU_stklen; i++) {
		sprintf(msg, "%s\n",&ustk[i][0]);	
		ud_printmsg(msg);
	}
}

/*********************************************************************
**    E_FUNCTION     :  int uu_trcstk() -- print procedure stack in trc file.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_trcstk()
{
	int i;
	char *p;
	char *getenv();
	FILE *uu_opentrc();

	if( trc==1 ) { 				/* Open trc file, if necessary */
		uu_opentrc();
	}

	if (trc == 0)
	{
		printf(" The procedure stack is length %d\n",UU_stklen);
		for (i=0; i<UU_stklen; i++) {
			fprintf(fd,"%s\n",&ustk[i][0]);	
		}
		fflush(fd);
	}
}

/* print stack and buffer. Abort the task */
int uu_abort()
{
	int status;
/*
....don't display error box
....Yurong 10/8/98
*/
/*
	ud_wrerr ("NCL has received a fatal error.  Enter REJECT OP to try to continue.");
*/
	ud_prmerr ("NCL has received a fatal error.  Enter REJECT OP to try to continue.");
/*
.....tell save function this is called from error
.....Yurong 10/8/98
*/
	ur_save_part(0, 1);
#if UU_COMP==UU_VAXVMS
	ul_reset_control();
	unauth (&UL_cam,&UL_cad);	/* unauthorize terminal */
#endif
	uu_prtstk(); 				/* print stack on terminal */
	uu_trcbuf(); 				/* print buffer (if active) on trace file */
	uu_trcstk(); 				/* print stack on trace file */
	if( emergencyclose  != NULL ) (*emergencyclose)();
	exit(12);
}

static void uinst()
{
	ud_printmsg ("illegal instruction. exiting process\n");
	signal(SIGILL,uinst);		/* Illegal instruction */
	uu_abort();
}

static void ufpe()
{
	ud_printmsg ("floating point exception\n");
	signal(SIGFPE,ufpe);		/* floating point exception */
	/* uu_prtstk(); ud_jump(-1,UU_TRUE); */
	uu_abort();
}
#if UU_COMP!=UU_WIN2K
static void ube()
{
	ud_printmsg ("bus error. exiting process\n");
	signal(SIGBUS,ube);		/* bus error */
	uu_abort();
}
#endif
static void usegvio()
{
	ud_printmsg ("segmentation violation. exiting process\n");
	signal(SIGSEGV,usegvio);		/* segmentation violation */
	uu_abort();
}
#if UU_COMP!=UU_WIN2K
static void ubasys()
{
	ud_printmsg ("bad arg to system call. exiting process\n");
	signal(SIGSYS,ubasys);		/* bad arg to system call */
	uu_abort();
}
#endif
static void quint()

{
	char c[20];
	int i;
	static int ifirst=1;
	static FILE *fd;
	if (ifirst==1) {
#if UU_COMP==UU_VAXVMS
		fd=fopen("tt","r");
#else		
		fd=fopen("/dev/tty","r");
#endif
		if (fd==NULL) {

 ud_printmsg ("quint can't open /dev/tty. bye\n");

#if UU_COMP==UU_VAXVMS
		ul_reset_control();
		unauth (&UL_cam,&UL_cad);	/* unauthorize terminal */
#endif
			exit(4);
		}
		ifirst=0;
	}
	ud_printmsg ("kbd interrupt.\n");
	uu_prtstk(); 
	while (1) {					/* do forever */
		printf("\ne(xit), n(o trace), t(race on) b(uffer) f(ile) r(eturn): ");
		fflush(stdout);
		fgets(c,19,fd);
		if (sscanf(c,"%d",&i)) {		/* got a number */
			UU_debmask=i;
			printf("trace now %d\n",UU_debmask);
		}
		else switch (c[0]) {
		case 'e': 
			if( emergencyclose  != NULL ) (*emergencyclose)();
#if UU_COMP==UU_VAXVMS
			ul_reset_control();
			unauth (&UL_cam,&UL_cad);	/* unauthorize terminal */
#endif
	      exit(12); 
			break;

  case 'r': signal(SIGINT,quint); return;

		case 't': 		/* turn on a trace */
			printf("enter trace letter to turn on(G,D,M,X,R,S,K,I,N,B,A): ");
			fflush(stdout);
			fgets(c,19,fd);
			switch (c[0]) {
				case 'G': UU_debmask=UU_debmask|UU_GTRC; break;
				case 'D': UU_debmask=UU_debmask|UU_DTRC; break;
				case 'M': UU_debmask=UU_debmask|UU_MTRC; break;
				case 'X': UU_debmask=UU_debmask|UU_XTRC; break;
				case 'R': UU_debmask=UU_debmask|UU_RTRC; break;
				case 'S': UU_debmask=UU_debmask|UU_STRC; break;
				case 'K': UU_debmask=UU_debmask|UU_KTRC; break;
				case 'I': UU_debmask=UU_debmask|UU_ITRC; break;
				case 'N': UU_debmask=UU_debmask|UU_NTRC; break;
				case 'B': UU_debmask=UU_debmask|UU_BTRC|UU_BITRC; break;
				case 'A': UU_debmask= -1; break;					/* all on */
				default: printf(" cant turn on %c\n",c);
							fflush(stdout);
			}
			break;				/* end of case t */
		case 'n':				/* turn off a trace */
			printf("enter trace letter to turn off(G,D,M,X,R,S,K,I,N,B,A): ");
			fflush(stdout);
			fgets(c,19,fd);
			switch (c[0]) {
				case 'G': UU_debmask=UU_debmask&(~UU_GTRC); break;
				case 'D': UU_debmask=UU_debmask&(~UU_DTRC); break;
				case 'M': UU_debmask=UU_debmask&(~UU_MTRC); break;
				case 'X': UU_debmask=UU_debmask&(~UU_XTRC); break;
				case 'R': UU_debmask=UU_debmask&(~UU_RTRC); break;
				case 'S': UU_debmask=UU_debmask&(~UU_STRC); break;
				case 'K': UU_debmask=UU_debmask&(~UU_KTRC); break;
				case 'I': UU_debmask=UU_debmask&(~UU_ITRC); break;
				case 'N': UU_debmask=UU_debmask&(~UU_NTRC); break;
				case 'B': UU_debmask=UU_debmask&(~(UU_BTRC|UU_BITRC)); break;
				case 'A': UU_debmask= 0; break;					/* all off */
				default: printf(" cant turn off %d\n",c);
						fflush(stdout);
			}
			break;				/* end of case n */
		case 'b':				/* turn on buffer */
			if (bufsize>0) {
				printf("%d char buffer already active\n",bufsize);
			}
			else {
				int i;
				printf("enter number of characters in buffer: ");
				fflush(stdout);
				fgets(c,19,fd); sscanf(c,"%d",&bufsize);
				printf("buffer now set to %d chars\n",bufsize);
				buff= (char *)uu_toolmalloc(bufsize);
				printf("%x=malloc(%d)\n",buff,bufsize);
				fflush(stdout);
				for (i=0; i<bufsize; i++) buff[i]=0;
				bufpos=0;				/* where to put new string */
			}
			break;				/* end of case b */
		case 'f':				/* turn on file */
			if (bufsize) uu_toolfree(buff);
			bufsize=0;
			break;				/* end of case file */
		}							/* end of main switch */
	}							/* end of while */
}

/*********************************************************************
**    E_FUNCTION     :  uu_trap() -- set up interrupt handling routines.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_trap()
{
	char *p, *getenv();

	/* Remember we've been here */
	trap=0;
/*
.....Disables bus errors due to data alignment problems
.....when running on SGI IRIX 6.5 systems with code
.....generated using -mips2 or above flag
.....Bobby  -  1/28/01
*/
#ifdef UU_IRIX64
	sysmips(MIPS_FIXADE,1,0,0);
#endif

	/* Get the no-trap shell variable */
	p = getenv("UU_notrap");

#if UU_COMP==UU_MASSCOMP
	signal(SIGINT,SIG_IGN);
#else

	/* Don't trap if the notrap environment variable is true */
	if ( (p != NULL) && (strcmp(p,"TRUE")==0) ) return; 

	/* Set the interrupts */
	signal(SIGILL,uinst);		/* Illegal instruction */
	signal(SIGFPE,ufpe);			/* Floating point exception */
	signal(SIGSEGV,usegvio);	/* Segmentation violation */
#if UU_COMP != UU_WIN2K
	signal(SIGSYS,ubasys);		/* Bad arg to system call */
	signal(SIGBUS,ube);			/* Bus error */
#endif
	/*	signal(SIGINT,uint);			/* Keyboard interrupt */
#endif
}

/*********************************************************************
**    E_FUNCTION     :  uu_emergencyclose() 
**		Set up emergency close function called just before an exit.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_emergencyclose(closefunc)
int (*closefunc)();
{
	uu_denter(UU_UITRC,(us,"uu_emergencyclose(%x)", closefunc));

	emergencyclose = closefunc;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  uu_debug_off() 
**			turn tracing off
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_debug_off()
{
	UU_debmask = 0;
}


/*********************************************************************
**    E_FUNCTION     :  uu_opentrc() 
**			Return file pointer of open trc file.  If file not yet opened,
**			open it here.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : FILE pointer of trc file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
FILE *uu_opentrc()
{
char *p, *getenv();

	if( trc==1 ) {									/* Need to open trc file */
		p = getenv("trc");
		if (p==NULL) fd=fopen("trc","w");
		else fd=fopen(p,"w");
		trc = 0;
	}

	return(fd);
}

