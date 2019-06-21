 /*********************************************************************
**    NAME         :  gttio.c -- serial line i/o routines 
**		CONTAINS:
**			NOTE:!!! Most of these functions have 2 versions, one
**			for UNIX and one for VMS - make sure that changes are
**			made in both versions
**
**
**		  fd=uu_ttopen(filename,mode) -- open file/device for i/o.
**		  uu_ttclos(fd,mode) -- close the open file of mode.
**      uu_ttsing(fd) -- put line in single char at a time mode. No echo, no
**						  c/r required.
**		  uu_ttnorm(fd) -- put line in normal mode, echo, c/r required, line
**						  editing enabled.
**		  int uu_ttmode(fd) -- return what input mode tt line is in.
**			uu_ttdatabits(fd,n) -- n is either 7 or 8.
**		  uu_ttput(fd,buf,len) -- write contents of buf to output buffer.
**		  uu_ttflout(fd) -- flush the output buffer, if any.
**		  ch=uu_ttget(fd) -- blocking read for next input char.
**		  len=uu_ttread(fd,buf,maxlen) -- blocking read for at most maxlen chars. 
**		  uu_ttunget(fd,ch) -- put a char back into input buffer.
**		  len=uu_ttline(fd,inbuf,maxlen)	-- blocking read for next line of input.
**		  ch=uu_tttest(fd) -- non-blocking read of next character into ch.
**							If nothing available, returns -1.
**		  uu_ttflin(fd) -- flush input buffer, if any.
**		  uu_ttstartsave(fd,func,flag) -- Start calling func from uu_ttflout().
**				Func is called with two arguments, the first is a pointer 
**				to the character output buffer and the second is the length of 
**				the output buffer. If flag=1, also send contents of output 
**				buffer down serial line.
**		  uu_ttstopsave(fd) -- Stop action of uu_ttstartsave.
**        uu_newline_flush()-- Flush the output buffer when new line of PP file
**                             is displaied in NCL501+ mode. Added by Paul. 02/12/92
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**        gttio.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:05:26
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "zsysdep.h"

#if (UU_COMP != UU_WIN2K)
#if (UU_COMP == UU_WINNT)
#include <termios.h>
#else

#if (UU_OPSYS==UU_SYS5)|| (UU_OPSYS==UU_SYS53) || (UU_COMP==UU_IRIS4D)
#include <termio.h>
#endif

#if (UU_OPSYS==UU_XENIX)
#include <termio.h>
#endif

#if (UU_OPSYS==UU_B42) && (UU_COMP!=UU_IRIS4D)
#include <sgtty.h>
#endif

#if UU_OPSYS==UU_ULTRIX
#include <sgtty.h>
#endif

#if UU_COMP==UU_APOLLO
#include <sgtty.h>
#include "/sys/ins/base.ins.c"
#include "/sys/ins/sio.ins.c"
#include "/sys/ins/streams.ins.c"
#include "/sys/ins/error.ins.c"
#endif
#endif	/* WINNT */
#endif /*WIN2K*/

#define MAXFD 5
static int fdlen=0;							/* current length of arrays */
static int fdin[MAXFD], fdout[MAXFD];	/* unix file descriptors */
static int inopen[MAXFD]={0,0,0,0,0}; 	/* 1 if a file is open */
static int outopen[MAXFD]={0,0,0,0,0};	/* 1 if a file is open */
static int inmode[MAXFD]={0,0,0,0,0};	/* 0=normal, 1=single char input mode */
static int ndatabits[MAXFD]={7,7,7,7,7};

#define BUFLEN 1000
static char outbuf[MAXFD][BUFLEN];		/* output buffers */
static int outbuflen[MAXFD]={0,0,0,0,0};	/* length of outbuf */
static char inbuf[MAXFD][BUFLEN];		/* stack buffer for uu_ttunget */
static int inbuflen[MAXFD]={0,0,0,0,0};/* length of inbuf */
static int ttwriteflag=1,ttsaveflag=0;
static int (*savefunc)();

/*
....Added for NCL501+. Paul
....02/03/92
*/
char TEKMODE = '0';
char logical_mode = '0';
char TEKMODE1 = 29;
char logical_mode1 = 29;
int CHANEL;
#include "stdio.h"
#include <signal.h>
#include "mfort.h" 
#include "nclfc.h" 
/*
......added for paral port
......Yurong 2/16/98
*/
/*#include <sys/plp.h> */


#ifdef UU_UNIX   /* VMS routines are separate and at bottom of file */
#if (UU_COMP != UU_WIN2K)
#if (UU_COMP==UU_WINNT)
static struct termios oldline[MAXFD],newline[MAXFD];	/* stty parms */
#else

#if (UU_OPSYS==UU_SYS5)|| (UU_OPSYS==UU_SYS53) || (UU_COMP==UU_IRIS4D)
static struct termio oldline[MAXFD],newline[MAXFD];	/* stty parms */
#endif

#if (UU_OPSYS==UU_XENIX)
static struct termio oldline[MAXFD],newline[MAXFD];	/* stty parms */
#endif

#if (UU_OPSYS==UU_B42) && (UU_COMP!=UU_IRIS4D)
static struct sgttyb oldline[MAXFD],newline[MAXFD]; /* stty parms */
#endif

#if UU_OPSYS==UU_ULTRIX
static struct sgttyb oldline[MAXFD],newline[MAXFD]; /* stty parms */
#endif

#if UU_COMP==UU_APOLLO
static short shfdin[MAXFD];				/* short of fdin */
static status_$t status[MAXFD];
#endif
#endif /*WINNT*/
#endif /*WIN2K*/

/*
......added for see if plotter is serial or parallel
......UU_PORT = 0  serial
......UU_PORT = 1 parallel
......Yurong
*/
static int pport = 0;
/*********************************************************************
**    E_FUNCTION :  int uu_ttport()
**		check to see if port is parallel or serial
**    PARAMETERS   
**       INPUT  :	None
**       OUTPUT : None
**    RETURNS : 1: parallel
**						2: serial	 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_ttport()
{
	return pport;
}
/*********************************************************************
**    E_FUNCTION :  int uu_ttopen(fname,mode)
**		open file (device) for i/o and return index to device
**    PARAMETERS   
**       INPUT  :	   char *fname;	name of device 
**						int inmode;		0=input, 1=output, 2=both
**						                3 = both, but don't open parallel
**						                    if cannot open. Just return
**										    failure (0).
**       OUTPUT :  	none	 
**    RETURNS      : index for device
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ttopen(fname,inmode)
char *fname;
int inmode;
{
	int i,irtn, ret,mode;

	uu_denter(UU_UITRC,(us,"uu_ttopen(%s %d)", fname, mode));
	if ((fdlen<0)||(fdlen>=MAXFD)) {
		uu_dprint(UU_UITRC,(us,"uu_ttopen ERROR - exiting - fdlen=%d",fdlen));
		uu_dexit;
		exit(UU_ABORT);
	}
	outbuflen[fdlen]=0;
	irtn=fdlen;
	i=0;
	mode = inmode;
	switch (mode) {
	case 0: fdin[fdlen]=open(fname,0);
				inopen[fdlen]=1;
			   break;
	case 1: fdout[fdlen]=open(fname,1);
				outopen[fdlen]=1;
				break;
	case 2:
	case 3:
			fdin[fdlen]=open(fname,2);
			fdout[fdlen]=fdin[fdlen];
			mode = 2;

#if UU_COMP==UU_APOLLO
			  shfdin[fdlen]=fdin[fdlen];
#endif
				inopen[fdlen]=1; outopen[fdlen]=1;
				break;
	}
#if (UU_COMP != UU_WIN2K)
	if ((mode==0)||(mode==2)) {

#if (UU_COMP == UU_WINNT)
		i=ioctl(fdin[fdlen],TCGETS,&oldline[fdlen]);
		if ( i == -1)
			goto Paral;
		zbytecp(newline[fdlen],oldline[fdlen]);	/* structure assignment */

		/* disable echo, etc */
		newline[fdlen].c_lflag &= ~(ISIG|ICANON|ECHO|ECHOE);	
		newline[fdlen].c_oflag &= ~OPOST;	/* disable output post-processing */
		newline[fdlen].c_iflag |= IXOFF | IXON;	/* turn on xon/xoff protocol. */
		/* newline[fdlen].c_iflag &= ~(ISTRIP|INLCR|ICRNL|IXON); */
		newline[fdlen].c_iflag &= ~(ISTRIP|INLCR|ICRNL);
		newline[fdlen].c_cc[VMIN]=1;			/* 1 char buffer */
#else

#if (UU_OPSYS==UU_SYS5)|| (UU_OPSYS==UU_SYS53) || (UU_COMP==UU_IRIS4D)
		i=ioctl(fdin[fdlen],TCGETA,&oldline[fdlen]);
/*
......check if ioctl call failed, if it is
......it may be parallel printer 
......Yurong 2/16/98
*/
		if ( i == -1)
			goto Paral;
		zbytecp(newline[fdlen],oldline[fdlen]);	/* structure assignment */

		/* disable echo, etc */
		newline[fdlen].c_lflag &= ~(ISIG|ICANON|ECHO|ECHOE);	
		newline[fdlen].c_oflag &= ~OPOST;	/* disable output post-processing */
		newline[fdlen].c_iflag |= IXOFF | IXON;	/* turn on xon/xoff protocol. */
		/* newline[fdlen].c_iflag &= ~(ISTRIP|INLCR|ICRNL|IXON); */
		newline[fdlen].c_iflag &= ~(ISTRIP|INLCR|ICRNL);
		newline[fdlen].c_cc[VMIN]=1;			/* 1 char buffer */
#endif
#if UU_OPSYS==UU_XENIX
		i=ioctl(fdin[fdlen],TCGETA,&oldline[fdlen]);
/*
......check if ioctl call failed, if it is
......it may be parallel printer 
......Yurong 2/16/98
*/
		if ( i == -1)
			goto Paral;
		zbytecp(newline[fdlen],oldline[fdlen]);	/* structure assignment */

		/* disable echo, etc */
		newline[fdlen].c_lflag &= ~(ISIG|ICANON|ECHO|ECHOE);	
		newline[fdlen].c_oflag &= ~OPOST;	/* disable output post-processing */
		newline[fdlen].c_iflag |= IXOFF;		/* disable input processing. */
		newline[fdlen].c_iflag &= ~(ISTRIP|INLCR|ICRNL|IXON);
		newline[fdlen].c_cc[VMIN]=1;			/* 1 char buffer */
#endif

#if (UU_OPSYS==UU_B42) && (UU_COMP!=UU_IRIS4D)
		i=ioctl(fdin[fdlen],TIOCGETP,&oldline[fdlen]);
/*
......check if ioctl call failed, if it is
......it may be parallel printer 
......Yurong 2/16/98
*/
		if ( i == -1)
			goto Paral;
		zbytecp(newline[fdlen],oldline[fdlen]);		/* structure assignment */
		newline[fdlen].sg_flags=EVENP+ODDP+CBREAK+TANDEM;
#endif
#if UU_OPSYS==UU_ULTRIX
		i=ioctl(fdin[fdlen],TIOCGETP,&oldline[fdlen]);
/*
......check if ioctl call failed, if it is
......it may be parallel printer 
......Yurong 2/16/98
*/
		if ( i == -1)
			goto Paral;
		zbytecp(newline[fdlen],oldline[fdlen]);		/* structure assignment */
		newline[fdlen].sg_flags=EVENP+ODDP+CBREAK+TANDEM;
#endif
#endif	/* WINNT */
/*
.....remove following code because if it is
.....a parallel port, it result i = -1 but not a error
.....Yurong 2/16/98
*/
/*		if (i!=0) {
			uu_dprint(UU_UITRC,(us,"uu_ttopen ERROR - exiting - ioctl returns %d",i));
			uu_dexit;
			exit(UU_ABORT);
		}
*/
/*
......added for paral port
......Yurong 2/16/98
*/
	fdlen++;
	return(irtn);
Paral:
		if (inmode == 3)
		{
			uu_ttclos(fdlen,2);
			irtn = irtn - 100;	
		}
		else
			pport = 1;
	}
#endif /*WIN2K*/
	fdlen++;
	uu_dexit;

	return(irtn);
}
	

/*********************************************************************
**    E_FUNCTION :  	uu_ttclos(fd,mode)
**							close device indexed by fd
**    PARAMETERS   
**       INPUT  :		int fd;		index of device
**							mode;			0=input, 1=output, 2=both
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_ttclos(fd,mode)				/* close device */
int mode;							/* 0=input, 1=output, 2=both */
int fd;
{
	uu_denter(UU_UITRC,(us,"uu_ttclos(%d %d)", fd, mode));

	/* flush the output buffer */
	uu_ttflout(fd);
	switch (mode) {
	case 0:
		if (inopen[fd]==1) {close(fdin[fd]); inopen[fd]=0;}
		break;
	case 1: 
		if (outopen[fd]==1) {close(fdout[fd]); outopen[fd]=0;}
		break;
	case 2:
		if ((inopen[fd]==1)&&(outopen[fd]==1)) {
			close(fdout[fd]); outopen[fd]=0; inopen[fd]=0;
		}
		break;
	}
	if (fdlen > 0) fdlen--;
	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION :		uu_ttsing(fd)
**								set single character mode (no <CR> needed to
**								get character)
**    PARAMETERS   
**       INPUT  :		int fd;	index of device 
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ttsing(fd)
int fd;
{
#if (UU_COMP != UU_WIN2K)
	int i;
	char us[150];

/*
......if pport = 1, parallel
......bypass this function
......Yurong 2/13/98
*/
	if (pport == 1) 
		return;
  	i=0;
#if (UU_COMP==UU_WINNT)
	i=ioctl(fdin[fd],TCSETSF,&newline[fd]);	/* set new line parms */
#else

#if (UU_OPSYS==UU_SYS5)|| (UU_OPSYS==UU_SYS53) || (UU_COMP==UU_IRIS4D)
	i=ioctl(fdin[fd],TCSETAF,&newline[fd]);	/* set new line parms */
#endif
#if UU_OPSYS==UU_XENIX
	i=ioctl(fdin[fd],TCSETAF,&newline[fd]);	/* set new line parms */
#endif
#if (UU_OPSYS==UU_B42) && (UU_COMP!=UU_IRIS4D)
	i=ioctl(fdin[fd],TIOCSETP,&newline[fd]);	/* set new line parms */
#endif
#if UU_OPSYS==UU_ULTRIX
	i=ioctl(fdin[fd],TIOCSETP,&newline[fd]);	/* set new line parms */
#endif
#if UU_COMP==UU_APOLLO
	shfdin[fd]=fdin[fd];
    sio_$control (shfdin[fd], sio_$raw, -1, status[fd]);
      uu_err_check(fd);
    sio_$control (shfdin[fd], sio_$no_echo, -1, status[fd]);
      uu_err_check(fd);
    sio_$control (shfdin[fd], sio_$no_NL, -1, status[fd]);
      uu_err_check(fd);
#endif
#endif	/* WINNT */
	if (i!=0) {
		uu_dprint(UU_UITRC,(us,"uu_ttsing ERROR - exiting - ioctl returns %d",i));
		uu_dexit;
		exit(UU_ABORT);
	}
	inmode[fd]=1;
#endif
}

#if UU_COMP==UU_APOLLO
static uu_err_check(fd)
int fd;
{
	char us[150];

      if (status[fd].s.fail != 0)
       {
        	uu_dprint(UU_UITRC,(us,"uu_err_check ERROR - exiting"));
         error_$print(status[fd]);
         exit(UU_ABORT);
       }
}
#endif


/*********************************************************************
**    E_FUNCTION :  	uu_ttnorm(fd)
**							set normal mode (need <CR> to get character)
**    PARAMETERS   
**       INPUT  :		int fd;		index of device 
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_ttnorm(fd)
int fd;
{
#if (UU_COMP != UU_WIN2K)
	int i;
	char us[150];

  	i=0;
#if (UU_COMP == UU_WINNT)
	i=ioctl(fdin[fd],TCSETSF,&oldline[fd]);
#else

#if (UU_OPSYS==UU_SYS5)|| (UU_OPSYS==UU_SYS53) || (UU_COMP==UU_IRIS4D)
	i=ioctl(fdin[fd],TCSETAF,&oldline[fd]);
#endif
#if UU_OPSYS==UU_XENIX
	i=ioctl(fdin[fd],TCSETAF,&oldline[fd]);
#endif
#if (UU_OPSYS==UU_B42) && (UU_COMP!=UU_IRIS4D)
	i=ioctl(fdin[fd],TIOCSETP,&oldline[fd]);
#endif
#if UU_OPSYS==UU_ULTRIX
	i=ioctl(fdin[fd],TIOCSETP,&oldline[fd]);
#endif
#if UU_COMP==UU_APOLLO
    sio_$control (shfdin[fd], sio_$raw, 0, status[fd]);
      uu_err_check(fd);
    sio_$control (shfdin[fd], sio_$no_echo, 0, status[fd]);
      uu_err_check(fd);
    sio_$control (shfdin[fd], sio_$no_NL, 0, status[fd]);
      uu_err_check(fd);
#endif
#endif /* WINNT */
	if (i!=0) {
		uu_dprint(UU_UITRC,(us,"uu_ttnorm ERROR - exiting - ioctl returns %d ",i));
		uu_dexit;
		exit(UU_ABORT);
	}
	inmode[fd]=0;
#endif /*WIN2K*/
}

/*********************************************************************
**    E_FUNCTION :  int uu_ttdatabits(fd,n)
**       				set flag for number of databits.
**                   Currently used by Masscomp driver only -- 8
**    PARAMETERS   
**       INPUT  : 	int fd;		index of device
**                   int n;      number of databits
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ttdatabits(fd,n)
int fd,n;
{
	ndatabits[fd]=n;
}

/*********************************************************************
**    E_FUNCTION :  int uu_ttmode(fd)
**       				return input mode of line.
**    PARAMETERS   
**       INPUT  : 	int fd;		index of device
**       OUTPUT :  
**    RETURNS      : 1 if line is in single mode, 0 if normal mode.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ttmode(fd)
int fd;
{
	return(inmode[fd]);
}

/*********************************************************************
**    E_FUNCTION:		uu_ttput(fd,buf,len)
**							write buf to output buffer, flush output buffer
**							if full
**    PARAMETERS   
**       INPUT  :		int fd;		index of device
**							char buf;	string to write
**							int len;		length of buf
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*uu_ttput(fd,buf,len)
*
.....Changed to work correctly in NCL501+ mode on the VAX/VMS platform
.....Paul - 10/30/92
.....Old version was:
*
*int fd;
*char buf[]; 
*int len;
*
*{
*	int i;
*     
*	for (i=0; i<len; i++) 
*        {
*            outbuf[fd][outbuflen[fd]]=buf[i];
*            outbuflen[fd]++;
*        /* if outbuf is full, flush it */    
/*            if (outbuflen[fd] >= BUFLEN - 1) uu_ttflout(fd);
*        }
*}
*/
int uu_ttput(fd,buf,len)
int fd;
char buf[];
int len;

{
    int i,j;
    static  int tmp = 0;

    if(len == 1 && ( buf[0] == 28 || buf[0] == 29 || buf[0] == 31))
    {
    logical_mode1 = buf[0];
    }

    if(strcmp(buf,"\033%!") == 0)
    {
    tmp = 1;
    goto a;
    }

    if(tmp==1 && len==1 && (buf[0]=='0' || buf[0]=='1'))
    {
    logical_mode = buf[0];
    tmp = 0;
    }

    if (tmp == 1) tmp = 0;

a:;

   for (i=0; i<len; i++)
   {
      outbuf[fd][outbuflen[fd]]=buf[i];
      outbuflen[fd]++;

      if (outbuflen[fd]>=BUFLEN-1)
           uu_ttflout(fd);
   }
}


/*********************************************************************
**    E_FUNCTION :  	uu_ttflout(fd)
**							flush the output buffer (send its contents down
**							the serial line )
**    PARAMETERS   
**       INPUT  :		int fd;		index of device 
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_ttflout(fd)					/* flush the output buffer */
int fd;
{	
	int i;
	char us[150];

	if (outbuflen[fd]>0) {
		if(ttsaveflag) 
			{
			(*savefunc)(&outbuf[fd][0],outbuflen[fd]);
			}
		if(ttwriteflag)
     		{
			i=write(fdout[fd],&outbuf[fd][0],outbuflen[fd]);
			if (i!=outbuflen[fd]) {
				uu_dprint(UU_UITRC,(us,"uu_ttflout ERROR  i=%d outbuflen[fd]=%d",
					i,outbuflen[fd]));
				} 
			}
	}
	outbuflen[fd]=0;
}


/*********************************************************************
**    E_FUNCTION :  	char uu_ttget(fd)
**							blocking read of one character
**    PARAMETERS   
**       INPUT  :		int fd;		index of device 
**       OUTPUT :   	none
**    RETURNS      : character read
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char uu_ttget(fd)						/* blocking read */
int fd;
{
	char c;
	int i, j;
	char us[150];

	if (outbuflen[fd]>0) uu_ttflout(fd);	/* flush output buffer if needed */
	if (inbuflen[fd]>0) {

		c=inbuf[fd][inbuflen[fd]];
		inbuflen[fd]=inbuflen[fd]-1;
	}
	else {


		i=read(fdin[fd],&c,1);
#if UU_COMP == UU_RIDGE
		if( i!=1 ) {					/* Try some more, Control-C hoses read!!! */
			for(j=0; j<50; j++) {
				i=read(fdin[fd],&c,1);
				if( i==1 ) break;
			}
		}
#endif
		if (i!=1) {
			uu_dprint(UU_UITRC,(us,"uu_ttget ERROR - exiting - i=%d",i));
			exit(UU_ABORT);
		}
	}
	if (ndatabits[fd]==7) c=c&127;						/* turn off parity bit */
	return(c);
}


/*********************************************************************
**    E_FUNCTION :  int uu_ttread(fd,buf,maxlen)
**						read at most maxlen chars into buf - stop at
**						newline or c/r
**    PARAMETERS   
**       INPUT  :		int fd;		index of device
**							char buf;	empty buffer
**							int maxlen;	# of chars to read
**       OUTPUT :   	char buf;	filled buffer
**    RETURNS      : number of chars read
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ttread(fd,buf,maxlen) /* read at most maxlen chars, 
										until newline or c/r */
int fd;
char buf[];
int maxlen;
{
	int len,i;

	len=0;

	/* first get chars from input buffer */
	while((len<maxlen)&&(inbuflen[fd]>0)) {
		buf[len]=uu_ttget(fd);
		if ((buf[len]=='\n')||(buf[len]=='\015')) return(len+1);
		len++;
	}
	/* direct read to finish filling buf */
	i=read(fdin[fd],&buf[len],maxlen-len);
	return(i+len);
}
			

/*********************************************************************
**    E_FUNCTION :   uu_ttunget(fd,ch)
**					puts char back into input buffer
**    PARAMETERS   
**       INPUT  : 	int fd;	index of device
**							char ch;	char to put back
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_ttunget(fd,ch)				/* put ch back into input buffer */
int fd;
char ch;
{
	inbuflen[fd]++;
	inbuf[fd][inbuflen[fd]]=ch;
}


/*********************************************************************
**    E_FUNCTION :  int uu_ttline(fd,inbuf,maxlen)
**						get next line
**    PARAMETERS   
**       INPUT  : 	int fd;			index of device
**							char inbuf;		empty buffer
**							int maxlen;		max # of chars to read
**       OUTPUT :   	char inbuf;		filled buffer
**    RETURNS      : number of chars read
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ttline(fd,inbuf,maxlen)		/* get next line in inbuf, return length*/
/* line assumed to start with first non-control char, end with
	first ctrl char after non-ctrl char. Thus null line not possible. 
	Inbuf must be 1 greater than maxlen since ttline will pad inbuf with
	a null terminator */
int fd;
char inbuf[];
int maxlen;
{
	int len,i;
	char us[150];

	while ((inbuf[0]=uu_ttget(fd))<' ');	/* ignore leading ctrl chars */
	len=1;								/* length of line so far */
	if (maxlen>1) {
		i=uu_ttread(&inbuf[1],maxlen-1);
		if (i<=0) {
			uu_dprint(UU_UITRC,(us,"uu_ttline ERROR - exiting - i=%d",i));
			exit(UU_ABORT);
		}
		for (len=1; len<=i; len++) {
			if (inbuf[len]<' ') break;
		}
		len=len+1;
	}
	inbuf[len]=0;					/* null terminator */
	return(len);
}


/*********************************************************************
**    E_FUNCTION :  	int uu_tttest(fd)
**					non-blocking read of one char
**    PARAMETERS   
**       INPUT  :		int fd;		index of device 
**       OUTPUT :   	none
**    RETURNS      : char read or -1 if none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_tttest(fd)
int fd;
{
	int len,i;

#if (UU_OPSYS==UU_B42 || UU_COMP==UU_MASSCOMP) && UU_COMP!=UU_IRIS4D
	i=ioctl(fdin[fd],FIONREAD,&len);
#endif
#if UU_COMP==UU_APOLLO
	/* here put whatever system 5 needs */
	len=0;
#endif
#if UU_OPSYS==UU_SYS5 && UU_COMP!=UU_MASSCOMP
	/* here put whatever system 5 needs */
	len=0;
#endif
#if UU_OPSYS==UU_XENIX
	/* here put whatever system 5 needs */
	len=0;
#endif
	if (len>0) {
		i=uu_ttget(fd);
		return(i);
	}
	else return(-1);
}


/*********************************************************************
**    E_FUNCTION :  	uu_ttflin(fd)
**							flush input buffer
**    PARAMETERS   
**       INPUT  :		int fd;		index of device 
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ttflin(fd)							/* flush input buffer */
int fd;
{
#if (UU_COMP != UU_WIN2K)
	long len;
	char junk[100];
	int blen;
  	int rlen;
  	int rptr;
	int i,sing;
/*
.....By pass this function if parallel
.....Yurong 2/13/98
*/
	if (pport==1) 
		return;
	uu_denter(UU_UITRC,(us,"ttflin(%d)",fd));
	uu_ttflout(fd);							/* first flush output, if any */
  	blen=100;
	inbuflen[fd]=0;
	sing=inmode[fd];						/* remember input mode */
	if (inmode[fd]==0) uu_ttsing(fd);
#if UU_OPSYS==UU_B42 && UU_COMP!=UU_IRIS4D
	i=ioctl(fdin[fd],FIONREAD,&len);
	i=read(fdin[fd],junk,len);
#endif
#if UU_OPSYS==UU_ULTRIX
	i=ioctl(fdin[fd],FIONREAD,&len);
	i=read(fdin[fd],junk,len);
#endif
#if (UU_COMP == UU_WINNT)
	i=ioctl(fdin[fd],TCSETSF,&newline[fd]);		/* assume in single char mode */
#else

#if (UU_OPSYS==UU_SYS5)|| (UU_OPSYS==UU_SYS53) || (UU_COMP==UU_IRIS4D)
	i=ioctl(fdin[fd],TCSETAF,&newline[fd]);		/* assume in single char mode */
#endif
#if UU_OPSYS==UU_XENIX
	i=ioctl(fdin[fd],TCSETAF,&newline[fd]);		/* assume in single char mode */
#endif
#if UU_COMP==UU_APOLLO
	shfdin[fd]=fdin[fd];
  	stream_$get_conditional (shfdin[fd], &junk, &blen, &rptr, &rlen, status[fd]);
  	uu_err_check(fd);
#endif
#endif /* WINNT */
	if (sing==0) uu_ttnorm(fd);					/* reset input mode */
	uu_dexit;
#endif
}
#endif								/* end of ifdef UU_UNIX */

/*#if UU_OPSYS==UU_VMS || UU_OPSYS == UU_ALPHAVMS*/
#if UU_COMP == UU_VAXVMS
#include iodef
#include ssdef

/* #include descrip . It is not necessary to include it now. It seems to be
                      already included in "mfort.h"     */
#include psldef
static short wiost[4];

/*********************************************************************
**    E_FUNCTION :  int uu_ttopen(fname,mode)
**		open file (device) for i/o and return index to device
**    PARAMETERS   
**       INPUT  :	   char *fname;	name of device 
**							int mode;		0=input, 1=output, 2=both
**       OUTPUT :  	none	 
**    RETURNS      : index for device
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ttopen(fname,mode)              /* open channel to device */
char *fname;                           /* file name */
int mode;                              /* 0=input, 1=output, 2=both*/
{
	$DESCRIPTOR (device, fname);
   int stat,irtn;

	uu_denter(UU_UITRC,(us,"uu_ttopen(%s %d)", fname, mode));

   if ((fdlen<0)||(fdlen>=MAXFD)) {
      uu_dprint(UU_UITRC,(us,"uu_ttopen ERROR - exiting - fdlen=%d",
			fdlen));
		uu_dexit;
      exit(UU_ABORT);
   }
	device.dsc$w_length = strlen(fname);	/* Set correct length in device */
   outbuflen[fdlen]=0;
   irtn=fdlen;
   switch (mode) {
   case 0: 
	   stat=SYS$ASSIGN(&device,&fdin[fdlen],PSL$C_USER,0);
      inopen[fdlen]=1;
      break;
   case 1: 
	   stat=SYS$ASSIGN(&device,&fdout[fdlen],PSL$C_USER,0); 
		outopen[fdlen]=1; 
		break; 
	case 2: 
	   stat=SYS$ASSIGN(&device,&fdin[fdlen],PSL$C_USER,0);
/*
....Added for NCL501+ mode.
....Paul 02/27/92
*/
      CHANEL = fdin[0];

      fdout[fdlen]=fdin[fdlen];
      inopen[fdlen]=1; 
		outopen[fdlen]=1;
      break;
	default:
		uu_dprint(UU_UITRC,(us,"uu_ttopen ERROR - exiting - mode=%d", mode));
		uu_dexit;
		exit(UU_ABORT);
   }
	if (stat != SS$_NORMAL)
	{
		uu_dexit;
		LIB$STOP(stat);
	}

   fdlen++;
	stat=sys$setef(9);
	wiost[0] = 1;
	uu_dexit;
   return(irtn);
}

   
/*********************************************************************
**    E_FUNCTION :  	uu_ttclos(fd,mode)
**							close device indexed by fd
**    PARAMETERS   
**       INPUT  :		int fd;		index of device
**							mode;			0=input, 1=output, 2=both
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_ttclos(fd,mode)			/* close device, release channel */
int mode;                     /* 0=input, 1=output, 2=both */
int fd;
{
	int stat;

	uu_denter(UU_UITRC,(us,"uu_ttclose(%d %d)", mode, fd));

	/* flush output buffer */
   uu_ttflout(fd);
   switch (mode) {
   case 0:
      if (inopen[fd]==1)
			inopen[fd]=0;
      break;
   case 1: 
      if (outopen[fd]==1)
			outopen[fd]=0;
      break;
   case 2:
      if ((inopen[fd]==1)&&(outopen[fd]==1)) 
		{
         outopen[fd]=0; 
			inopen[fd]=0;

			/* release channel */
			stat=SYS$DASSGN(fd);
			if (stat!=SS$_NORMAL)
			{
				uu_dexit;
				LIB$STOP(stat);
			}
      }
      break;
   }
	fdlen--;
	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION :		uu_ttsing(fd)
**								set single character mode (no <CR> needed to
**								get character)
**    PARAMETERS   
**       INPUT  :		int fd;	index of device 
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_ttsing(fd)
int fd;
{

	/* there is a different read operation for single and
		normal reads, see uu_ttget() */
   inmode[fd]=1;
}


/*********************************************************************
**    E_FUNCTION :  	uu_ttnorm(fd)
**							set normal mode (need <CR> to get character)
**    PARAMETERS   
**       INPUT  :		int fd;		index of device 
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_ttnorm(fd)                      /* set i/o back */
int fd;
{

	/* there is a different read operation for single and
		normal reads, see uu_ttget() */
   inmode[fd]=0;
}


/*********************************************************************
**    E_FUNCTION :  int uu_ttmode(fd)
**       				return input mode of line.
**    PARAMETERS   
**       INPUT  : 	int fd;		index of device
**       OUTPUT :  
**    RETURNS      : 1 if line is in single mode, 0 if normal mode.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ttmode(fd)
int fd;
{
	return(inmode[fd]);
}


/*********************************************************************
**    E_FUNCTION:		uu_ttput(fd,buf,len)
**							write buf to output buffer, flush output buffer
**							if full
**    PARAMETERS   
**       INPUT  :		int fd;		index of device
**							char buf;	string to write
**							int len;		length of buf
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....Changed to work correctly in NCL501+ mode on the VAX/VMS platform
.....Paul - 03/18/92
.....Old version was:
*
*int uu_ttput(fd,buf,len)          
*char buf[]; int len;
*int fd;
*{
*   int i;
*
*   uu_denter(UU_UITRC,(us,"ttput fd=%d len=%d",fd,len));
*
*   for (i=0; i<len; i++)
*	{
*      outbuf[fd][outbuflen[fd]]=buf[i];
*      outbuflen[fd]++;
*
*      if (outbuflen[fd]>=BUFLEN-1)
*			uu_ttflout(fd);
*   }
*   uu_dexit;
*}
*/
int uu_ttput(fd,buf,len)
int fd;
char buf[];
int len;

{
    int i,j;
    static  int tmp = 0;
 
    if(len == 1 && ( buf[0] == 28 || buf[0] == 29 || buf[0] == 31))
    {
    logical_mode1 = buf[0];
    }
 
    if(strcmp(buf,"\033%!") == 0)
    {
    tmp = 1;
    goto a;
    }
 
    if(tmp==1 && len==1 && (buf[0]=='0' || buf[0]=='1')) 
    {
    logical_mode = buf[0];
    tmp = 0;
    }
    
    if (tmp == 1) tmp = 0;
 
    a:;
 
/******************************************************************/

   for (i=0; i<len; i++)
   {
      outbuf[fd][outbuflen[fd]]=buf[i];
      outbuflen[fd]++;

      if (outbuflen[fd]>=BUFLEN-1)
           uu_ttflout(fd);
   }
}



/*********************************************************************
**    E_FUNCTION :  	uu_ttflout(fd)
**							flush the output buffer (send its contents down
**							the serial line)
**    PARAMETERS   
**       INPUT  :		int fd;		index of device 
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_ttflout(fd)               /* flush the output buffer */
int fd;
{  
   int stat;
	short iosb[4];		/* i/o status buffer */
	short count;		/* number of bytes written */
	char us[150];

   if (outbuflen[fd]>0)
	{
		if(ttsaveflag) 
		{
			(*savefunc)(&outbuf[fd][0],outbuflen[fd]);
		}
		if(ttwriteflag)
		{
			/* send contents of outbuf down serial line */
			do
			{
/* NCL -
.....Go do other things while VAX
.....is performing write
.....Bob Jr.  7/7/88
*/
			stat= sys$synch(9,wiost);
				stat=SYS$QIO(9,fdout[fd],IO$_WRITELBLK|IO$M_NOFORMAT,wiost,0,0,
					   &outbuf[fd][0],outbuflen[fd],0,0,0,0);
			}
			while (stat == SS$_CONTROLC || stat == SS$_CONTROLY
				 || wiost[0] == SS$_CONTROLC || wiost[0] == SS$_CONTROLY);

/*			count=iosb[1]; */
			if (stat != SS$_NORMAL)
			/* check for error conditions */
				LIB$STOP(stat);

	    }
     }
   outbuflen[fd]=0;
}


/*********************************************************************
**    E_FUNCTION :  	char uu_ttget(fd)
**							blocking read of one character
**    PARAMETERS   
**       INPUT  :		int fd;		index of device 
**       OUTPUT :   	none
**    RETURNS      : character read
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

char uu_ttget(fd)                /* blocking read  of one character */
int fd;
{
   char c;
   int i,stat;
	short count;				/* number of bytes read */
	short iosb[4];				/* i/o status buffer */
	char buff[BUFLEN];	

	/* termination mask for single char mode reads */
	static int term_mask[2] = {0,0};

	char us[150];


	/* flush output buffer if needed */
   if (outbuflen[fd]>0)
		uu_ttflout(fd);

	/* if inbuf is not empty, get character from inbuf */
   if (inbuflen[fd]>0)
	{
      c=inbuf[fd][--inbuflen[fd]];
   }

	/* else if normal mode and inbuf is empty */
   else if (inmode[fd]==0 && inbuflen[fd]==0)
	{
      do {
/* NCL -
.....Wait for terminal write
.....Bob Jr.  7/7/88
*/
	stat= sys$synch(9,wiost);
/*
.....Do a READ PASS ALL to get all control characters
.....Bobby  -  11/3/92
*/
      stat=SYS$QIOW (0, fdin[fd], IO$_TTYREADALL|IO$M_NOECHO, iosb, 0, 0,
/*      stat=SYS$QIOW (0, fdin[fd], IO$_READVBLK, iosb, 0, 0,*/
                 buff, BUFLEN-1, 0, 0, 0, 0) ;
      }
      while (stat == SS$_CONTROLC || stat == SS$_CONTROLY
             || iosb[0] == SS$_CONTROLC || iosb[0] == SS$_CONTROLY);

		/* check for error conditions */
		if (stat != SS$_NORMAL)
			LIB$STOP(stat);
		else if (iosb[0] != SS$_NORMAL)
			LIB$STOP(iosb[0]);

      inbuflen[fd] = iosb[1];
      inbuflen[fd]++ ;

      for (i=0; i<inbuflen[fd]; i++)
			inbuf[fd][i]=buff[inbuflen[fd]-1-i] ;

      c=inbuf[fd][--inbuflen[fd]];
   }
	/* else if single mode and inbuf is empty */
	else if (inmode[fd]==1 && inbuflen[fd]==0)
	{
      do {
/* NCL -
.....Wait for terminal write
.....Bob Jr.  7/7/88
*/
	stat= sys$synch(9,wiost);
      stat=SYS$QIOW(0,fdin[fd],(IO$_READVBLK|IO$M_NOECHO|IO$M_NOFILTR),iosb,0,0,
               &c, 1, 0, term_mask, 0, 0 ) ;
      }
      while (stat == SS$_CONTROLC || stat == SS$_CONTROLY
             || iosb[0] == SS$_CONTROLC || iosb[0] == SS$_CONTROLY);

		/* check for error conditions */
		if (stat != SS$_NORMAL)
			LIB$STOP(stat);
		else if (iosb[0] != SS$_NORMAL)
			LIB$STOP(iosb[0]);
   }
	else
	{
		uu_dprint(UU_UITRC,(us,"uu_ttget ERROR - inmode[fd]=%d inbuflen[fd]=%d",
			inmode[fd], inbuflen[fd]));
	}

   return(c);
}


/*********************************************************************
**    E_FUNCTION :  int uu_ttread(fd,buf,maxlen)
**						read at most maxlen chars into buf - stop at
**						newline or c/r
**    PARAMETERS   
**       INPUT  :		int fd;		index of device
**							char buf;	empty buffer
**							int maxlen;	# of chars to read
**       OUTPUT :   	char buf;	filled buffer
**    RETURNS      : number of chars read
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_ttread(fd,buf,maxlen)           /* read at most maxlen chars, 
  			                                 until newline or c/r */
char buf[];
int maxlen;
int fd;

{
   int len = 0 ;

   while( (len<maxlen) && ((buf[len++]=uu_ttget(fd) ) != 0x0a))
		;
   return(len);

}

         
/*********************************************************************
**    E_FUNCTION :   uu_ttunget(fd,ch)
**					puts char back into input buffer
**    PARAMETERS   
**       INPUT  : 	int fd;	index of device
**							char ch;	char to put back
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_ttunget(fd,ch)            /* put ch back into input buffer */
char ch;
int fd;
{
   inbuf[fd][inbuflen[fd]]=ch;
   inbuflen[fd]++;
}


/*********************************************************************
**    E_FUNCTION :  int uu_ttline(fd,inbuf,maxlen)
**						get next line
**    PARAMETERS   
**       INPUT  : 	int fd;			index of device
**							char inbuf;		empty buffer
**							int maxlen;		max # of chars to read
**       OUTPUT :   	char inbuf;		filled buffer
**    RETURNS      : number of chars read
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_ttline(fd,inbuf,maxlen)      /* get next line  in inbuf, return length*/
/* line assumed to start with first non-control char, end with
   first ctrl char after non-ctrl char. Thus null line not possible. 
   Inbuf must be 1 greater than maxlen since ttline will pad inbuf with
   a null terminator */
char inbuf[];
int maxlen;
int fd;
{
   int len,i;
	char us[150];

   while ((inbuf[0]=uu_ttget(fd))<' ');   /* ignore leading ctrl chars */
   len=1;                        /* length of line so far */
   if (maxlen>1) {
      i=uu_ttread(&inbuf[1],maxlen-1);
      if (i<=0) {
         uu_dprint(UU_UITRC,(us,"uu_ttline ERROR - exiting - i=%d",i));
         exit(UU_ABORT);
      }
      for (len=1; len<=i; len++) {
         if (inbuf[len]<' ') break;
      }
      len=len+1;
   }
   inbuf[len]=0;              /* null terminator */
   return(len);
}


/*********************************************************************
**    E_FUNCTION :  	int uu_tttest(fd)
**					non-blocking read of one char
**    PARAMETERS   
**       INPUT  :		int fd;		index of device 
**       OUTPUT :   	none
**    RETURNS      : char read or -1 if none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_tttest(fd)
int fd;
{
   int len,iret;
   int modesave ;

   modesave = -1 ;

   if ( (len=inbuflen[fd]) == 0 ) {
      modesave = inmode[fd] ;
      uu_ttsing(fd) ;
      len = uu_ttbuflen(fd) ;
   }

   if (len>0) 
      iret=uu_ttget(fd);
   else iret = -1 ;

   if ( modesave==0 ) uu_ttnorm(fd) ;
   return(iret);
}

/******** internal func uu_ttbuflen() called by uu_tttest() - VMS only ********/

static int uu_ttbuflen(fd)	/* return length of system type-ahead buffer */
int fd;
{
   int len,stat;
   int typahead[2] ;
	short iosb[4];

   do {
/* NCL -
.....Wait for terminal write
.....Bob Jr.  7/7/88
*/
	stat= sys$synch(9,wiost);
   stat=SYS$QIOW ( 0, fdin[fd], (IO$_SENSEMODE|IO$M_TYPEAHDCNT), iosb, 0, 0,
      typahead, 8, 0, 0, 0, 0 ) ;
      }
      while (stat == SS$_CONTROLC || stat == SS$_CONTROLY);

	if (stat != SS$_NORMAL)
		LIB$STOP(stat);
   len = typahead[0] & 0x0000ffff ;
   return(len);
}



/*********************************************************************
**    E_FUNCTION :  	uu_ttflin(fd)
**							flush input buffer
**    PARAMETERS   
**       INPUT  :		int fd;		index of device 
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_ttflin(fd)                   /* flush input buffer, if any */
int fd;
{
   inbuflen[fd] = 0 ;
   while ( uu_tttest(fd) != -1 ) ;
}

#endif						/* end of ifdef UU_VMS */


/*********************************************************************
**    E_FUNCTION :  uu_ttstartsave(fd,func,flag)
**				begin calling func() from uu_ttflout() with contents of outbuf - 
**				if flag=1, also send outbuf contents down serial line
**    PARAMETERS   
**       INPUT  :		int fd;			index of device
**							int (*func)();	function to be called with outbuf
**							int flag;		if 1, also send outbuf down serial line
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_ttstartsave(fd,func,flag)	
int fd,(*func)(),flag;
{	
	uu_ttflout(fd);
	ttsaveflag=1;
	ttwriteflag=flag;
	savefunc=func;
}


/*********************************************************************
**    E_FUNCTION : 	uu_ttstopsave(fd)
**						stop call savefunc() from uu_ttflout() with contents
**						of outbuf - go back to sending outbuf down serial line
**    PARAMETERS   
**       INPUT  : 	int fd;		index of device
**       OUTPUT :   	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_ttstopsave(fd)
int fd;
{
	uu_ttflout(fd);
	ttsaveflag=0;
	ttwriteflag=1;
}


/*********************************************************************
**    E_FUNCTION : 	uu_ttfd(fd)
**						Return file descriptor (or channel in VMS).  Used
**						in VMS to set asyncronous traps for a particular line.
**    PARAMETERS   
**       INPUT  : 	int fd;		index of device
**       OUTPUT :   	none
**    RETURNS      : File descriptor or channel number.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_ttfd(fd)
int fd;
{
	uu_denter(UU_GITRC,(us,"uu_ttfd(%d)=%d", fd, fdin[fd]));
	uu_dexit;
	return( fdin[fd] );
}
/*********************************************************************
**    E_FUNCTION :  uu_newline_flush()
**                         Flush the output buffer when new line of PP file
**                         is displaied in NCL501+ mode. Added by Paul. 02/12/92
**    PARAMETERS
**       INPUT  :   none
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_newline_flush(fd)
int fd;

{
UM_int2 ifl, mode, mode1;
   ifl=35;
   getifl(&ifl,&mode);
   ifl=350;
   getifl(&ifl,&mode1);
   if(mode == 2 && mode1 == 1)
   {
        outbuf[fd][outbuflen[fd]]='\033';
        outbuflen[fd]++;
        outbuf[fd][outbuflen[fd]]='%';
        outbuflen[fd]++;
        outbuf[fd][outbuflen[fd]]='!';
        outbuflen[fd]++;
        outbuf[fd][outbuflen[fd]]='1';
        outbuflen[fd]++;

        uu_ttflout(fd);

        outbuf[fd][outbuflen[fd]]='\033';
        outbuflen[fd]++;
        outbuf[fd][outbuflen[fd]]='%';
        outbuflen[fd]++;
        outbuf[fd][outbuflen[fd]]='!';
        outbuflen[fd]++;
        outbuf[fd][outbuflen[fd]]=logical_mode;
        outbuflen[fd]++;
        if (logical_mode == '0')
            {
            outbuf[fd][outbuflen[fd]]=logical_mode1;
            outbuflen[fd]++;
            }
   }
}

