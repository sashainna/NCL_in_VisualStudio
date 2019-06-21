/***********************************************************************
c
c   FILE NAME:  ltermio.c
c   CONTAINS:
c               clreol  clreos  clrscr  crslft  crsrgt  dmpbuf  getc1
c               hilit   plot    rstcur  savcur  scroll  trmatt  trmerr
c               trmnl   trmrst  wflush
c
** 
**    MODULE NAME AND RELEASE LEVEL 
**       ltermio.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:36
***********************************************************************/

#include "usysdef.h"
#include "mfort.h"
#include <signal.h>
#if UU_COMP!=UU_WIN2K
#include <curses.h>
#endif

#include "gtbl.h"
/*#include "ws410x.h"
/*#include "ws411x.h"
/*#include "ws.h"
/*extern uw_41xxdat uw_41xx;     /* common data for all parts of 41xx driver */
extern UL_cam, UL_cad;
extern UU_LOGICAL UR_changed;  /* Used in trmrst. Logic copied from */
                               /* ul_sysexit "umb/lsignon.c". Paul.11/01/92 */

#if UU_COMP != UU_VAXVMS
#include <string.h>
#endif
#if UU_COMP == UU_VAXVMS 
#include <iodef.h> 
#endif 

#include "nclfc.h"
#if UU_COMP!=UU_WIN2K
static WINDOW *cwin[2];
#endif
static int lasty,lastx,dwin[2],lstwin,ipmax;
static int CURSES1=0,TTY1=0,OPEN1=0;

#if UU_COMP == UU_VAXVMS 
static int istat,ichan,iorlb,iostat[2]; 
#endif 

/**********************************
c...Added check for NCL-VT mode
c...Paul  -  11/01/91
**********************************/
static char map [24][80];

void trmast()
{
#if UU_COMP != UU_WIN2K
	int *status;

        rsttrm();
        signal(SIGINT,trmast);
        if (CURSES1 == 0)  printf ("%c[1;24r",27);   
#endif
		return;
}
/***********************************************************************
c
c   SUBROUTINE:  clreol
c
c   FUNCTION:  This routine erases from the current cursor position to
c              the end of the line.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
***********************************************************************/

clreol()
{
#if UU_COMP != UU_WIN2K
	if (CURSES1 == 1)
	{
	        wclrtoeol(cwin[0]);
       		dwin[0] = 1;
	}
	else
	{
	    printf ("%c[K",27);
	}
#endif
        return;
}

/***********************************************************************
c
c   SUBROUTINE:  clreos
c
c   FUNCTION:  This routine erases from the current cursor position to
c              the end of the screen.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
***********************************************************************/

clreos()
{
#if UU_COMP != UU_WIN2K
        if (CURSES1 == 1)
        {
		wclrtobot(cwin[0]);
		dwin[0] = 1;
	}
	else
	{
	        printf ("%c[J",27);
	}
#endif
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  clrscr
c
c   FUNCTION:  This routine erases the entire screen.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
***********************************************************************/
 
clrscr()
{
#if UU_COMP != UU_WIN2K
	if (CURSES1 == 1)
	{
		wclear(cwin[0]);
		dwin[0] = 1;
	}
	else
	{
		 printf ("%c[2J",27);
	}
#endif
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  crslft (knt)
c
c   FUNCTION:  This routine moves the cursor to the left 'knt' spaces.
c
c   INPUT:  knt     I*4  D1  -  Number of positions to move the cursor.
c
c   OUTPUT: none.
c
***********************************************************************/

crslft(knt)
int *knt;
{
#if UU_COMP != UU_WIN2K
	int x,y,i;

        if (CURSES1 == 1)
        {
/*
...Get the current cursor position
*/
		if (*knt <=0) return;
		getyx(cwin[0],y,x);
/*
...Move the cursor
*/
		x = x - *knt;
		if (x < 0) x = 0;
		wmove(cwin[0],y,x);
		dwin[0] = 1;
	}

/*
...Move the cursor
...Without using curses
*/
	else
	{
		 for (i=0;i<*knt;i++) printf ("%c[D",27);
	}
#endif
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  crsrgt (knt)
c
c   FUNCTION:  This routine moves the cursor to the right 'knt' spaces.
c
c   INPUT:  knt     I*4  D1  -  Number of positions to move the cursor.
c
c   OUTPUT: none.
c
***********************************************************************/
 
crsrgt(knt)
int *knt;
{
#if UU_COMP != UU_WIN2K
	int x,y,i;

	if (CURSES1 == 1)
	{
/* 
...Get the current cursor position 
*/
		if (*knt <=0) return;
        	getyx(cwin[0],y,x);
/* 
...Move the cursor
*/
        	x = x + *knt; 
      		if (x > 80) x = 80; 
        	wmove(cwin[0],y,x); 
        	dwin[0] = 1;
	}
/* 
...Move the cursor
...Without using curses
*/ 
        else
        { 
                 for (i=0;i<*knt;i++) printf ("%c[C",27);
        }
#endif
        return;
}

/***********************************************************************
c
c   SUBROUTINE:  dmpbuf (cbuf,knc)
c
c   FUNCTION:  This routine writes a character string to the screen.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to write to the screen.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: none.
c
***********************************************************************/
 
dmpbuf(cbuf,knc)

#if UU_COMP == UU_VAXVMS 
  UM_f77_str_ptr cbuf;
#else
  char *cbuf; 
#endif

int *knc;
{
#if UU_COMP != UU_WIN2K
	int nc;
	char obuf[256];   
#if UU_COMP == UU_VAXVMS 
        char *tmpbuf;
#endif
/*
...Set up dummy buffer
*/
	if (*knc <= 0) goto endit;
	nc = *knc;
#if UU_COMP == UU_VAXVMS       
        tmpbuf = UM_cstr_of_f77_str(cbuf);
        strncpy (obuf,tmpbuf,nc);
#else
        strncpy (obuf,cbuf,nc);
#endif
	obuf[nc] = '\0';
/*
...Dump output buffer
*/
        if (CURSES1 == 1)
        {
		wprintw (cwin[0],"%s",obuf);
		dwin[0] = 1;
	}
	else
	{
		printf ("%s",obuf);
	}
endit:;
#endif
	return;
}


/***********************************************************************
c
c   SUBROUTINE:  dmpbuf_w2 (cbuf,knc)
c
c   FUNCTION:  This routine writes a character string to the screen.
c
c   INPUT:  cbuf    C*n  D1  -  Character string to write to the screen.
c
c           knc     I*4  D1  -  Number of characters in 'cbuf'.
c
c   OUTPUT: none.
c
***********************************************************************/
 
dmpbuf_w2(cbuf,knc)

#if UU_COMP == UU_VAXVMS
  UM_f77_str_ptr cbuf;
#else
  char *cbuf;
#endif
 
int *knc;
{
#if UU_COMP != UU_WIN2K
        int nc;
        char obuf[256];   
#if UU_COMP != UU_VAXVMS
        int lin,col;   
#endif
#if UU_COMP == UU_VAXVMS
        char *tmpbuf;
#endif
/*
...Set up dummy buffer
*/
        if (*knc <= 0) goto endit;
        nc = *knc;
#if UU_COMP == UU_VAXVMS
        tmpbuf = UM_cstr_of_f77_str(cbuf);
        strncpy (obuf,tmpbuf,nc);
#else
        strncpy (obuf,cbuf,nc);
#endif
        obuf[nc] = '\0';
/*
...Dump output buffer
*/
#ifndef UU_RS6000
if (CURSES1 == 1)
        {
#if UU_COMP != UU_VAXVMS
                   lin = 13;
                   col = 1;
                   plot(&lin,&col);
                   clreol();
                wrefresh(cwin[0]);
#endif
                wprintw (cwin[1],"%s",obuf);

                wrefresh(cwin[1]);
                dwin[1] = 1;
        }
        else
        {
                printf ("%s",obuf);
                printf ("%c[1;24r",27);  
                OPEN1 = 0; 
        }
#endif
#ifdef UU_RS6000
     if (CURSES1 == 1)
     {
      wprintw (cwin[0],"%s",obuf);
      dwin[0] = 1;
      scrollok(cwin[0],FALSE);
    }
    else
    {
        printf ("%s",obuf);
    }

#endif
endit:;
        return;
#endif
}
 

/***********************************************************************
c
c   SUBROUTINE:  getc1 (kch)
c
c   FUNCTION:  This routine reads a character from the keyboard.
c
c   INPUT:  none.
c
c   OUTPUT: kch     I*4  D1  -  Numeric value of input character.
c
***********************************************************************/
 
getc1(kch)
int *kch;
{
#if UU_COMP != UU_WIN2K
int stat;
        if (CURSES1 == 1)
        {

   	        if (dwin[0] == 1) wrefresh(cwin[0]);
		dwin[0] = 0;
#if UU_COMP != UU_VAXVMS 
		*kch = getch();
#endif
#if UU_COMP == UU_VAXVMS 
        stat = sys$qiow(0,ichan,iorlb,&iostat[0],0,0,kch,1,0,0,0,0);
#endif
 	}
	else
	{
		
#if UU_COMP != UU_VAXVMS 
		*kch = getchar();
#endif
#if UU_COMP == UU_VAXVMS 
        stat = sys$qiow(0,ichan,iorlb,&iostat[0],0,0,kch,1,0,0,0,0);
#endif
	}
#endif
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  hilit (kfl)
c
c   FUNCTION:  This routine turns the screen hiliting mode on and off.
c
c   INPUT:  kfl     I*4  D1  -  0 = Turn hiliting mode off.  1 = Turn
c                               hiliting mode off.
c
c   OUTPUT: none.
c
***********************************************************************/
 
hilit(kfl)
int *kfl;
{
#if UU_COMP != UU_WIN2K
/*
...Turn hiliting off
*/
	if (*kfl == 0)
	{
        	if (CURSES1 == 1)
        	{
			wstandend(cwin[0]);
		}
		else
		{
		        printf ("%c[0m",27);
		}
	}
/*
...Turn hiliting on
*/
	else
	{
        	if (CURSES1 == 1)
		{
			wstandout(cwin[0]);
		}
		else
		{
		        printf ("%c[7m",27);
		}
	}
#endif
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  plot (klin,kcol)
c
c   FUNCTION:  This routine positions the cursor at the specified line
c              and column.
c
c   INPUT:  klin    I*4  D1  -  Line number to position cursor.
c
c           kcol    I*4  D1  -  Column number to position cursor.
c
c   OUTPUT: none.
c
***********************************************************************/
 

plot(klin,kcol)
int *klin,*kcol;

{
#if UU_COMP != UU_WIN2K
        int ilin,icol;
/*
...Window system is based on
...x=0, y=0 instead of 1,1
*/
	if (CURSES1 == 1)
	{
        	ilin = *klin - 1;
			if (ilin > 23) ilin = 23;
        	icol = *kcol - 1;
		wmove (cwin[0],ilin,icol);  
 		dwin[0] = 1;
	}
        else
        {
                 printf ("%c[%02d;%02dH",27,*klin,*kcol);
        }
#endif  
        return;
}

/***********************************************************************
c
c   SUBROUTINE:  plot_w2 (klin,kcol)
c
c   FUNCTION:  This routine positions the cursor at the specified line
c              and column.
c
c   INPUT:  klin    I*4  D1  -  Line number to position cursor.
c
c           kcol    I*4  D1  -  Column number to position cursor.
c
c   OUTPUT: none.
c
***********************************************************************/
 
plot_w2(klin,kcol)
int *klin,*kcol;
{
#if UU_COMP != UU_WIN2K

        int ilin,icol;
/*
...Window system is based on
...x=0, y=0 instead of 1,1
*/ 
        if (CURSES1 == 1)
#ifndef UU_RS6000
        {
                ilin = *klin - 1;
                        if (ilin > 23) ilin = 23;
                icol = *kcol - 1;
                wmove (cwin[1],ilin,icol);
                dwin[1] = 1;
        } 
        else
        {
                 printf ("%c[%02d;%02dH",27,*klin+5,*kcol); 

        }
#endif
#ifdef UU_RS6000 
        {
                ilin = *klin - 1 + 5;
                if (ilin > 23) ilin = 23;
                icol = *kcol - 1;
                wmove (cwin[0],ilin,icol);
                dwin[0] = 1;
        } 
        else
        {   
                 printf ("%c[%02d;%02dH",27,*klin+5,*kcol);
        }
#endif 
#endif
        return; 
} 
 

/***********************************************************************
c
c   SUBROUTINE:  rstcur
c
c   FUNCTION:  This routine restores the cursor position to the last
c              position saved by 'savcur'.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
***********************************************************************/
 
rstcur()
{
#if UU_COMP != UU_WIN2K
        if (CURSES1 == 1) 
        {
		wmove(cwin[0],lasty,lastx);
		dwin[0] = 1;
	}
#endif
		return;
}

/***********************************************************************
c
c   SUBROUTINE:  savcur
c
c   FUNCTION:  This routine saves the current cursor position.  Call
c              'rstcur' to restore this position.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
***********************************************************************/
 

savcur()
{
#if UU_COMP != UU_WIN2K
        if (CURSES1 == 1) 
        {
		getyx(cwin[0],lasty,lastx);
	}
#endif
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  scroll1 (kbeg,kend,klin,kdir)
c
c   FUNCTION:  This routine scrolls a portion of the screen.
c
c   INPUT:  kbeg    I*4  D1  -  Beginning line number of portion of
c                               screen to scroll.
c
c           kend    I*4  D1  -  Ending line number of portion of screen
c                               to scroll.
c
c           klin    I*4  D1  -  Number of lines to scroll.
c
c           kdir    I*4  D1  -  1 = Scroll the screen up.  2 = scroll
c                               down.
c
c   OUTPUT: none.
c
***********************************************************************/
/*
...
...This routine was changed by Paul to work correctly on the VAX
...12/18/91
...
*/

scroll1 (kbeg,kend,klin,kdir)
int *kbeg,*kend,*klin,*kdir;
{
#if UU_COMP != UU_WIN2K
        int ibeg,isiz,i;
/*
...Define scrolling region
*/
        if (CURSES1 == 1) 
        {

#ifndef UU_RS6000
          if (OPEN1 == 0)
            {            
	         ibeg = *kbeg - 1;
             isiz = *kend - ibeg;
             cwin[1] = subwin(cwin[0],isiz,80,ibeg,0);
             scrollok(cwin[1],TRUE);
                OPEN1 = 1;
            }

/*
...Scroll up
*/
        	if (*kdir == 1)
        	{
			for (i=0;i<*klin;i++)
                	{
				scroll(cwin[1]);
                wrefresh(cwin[1]);
                       	}
        	}
/*
...Scroll down
*/
        	else
        	{
			for (i=0;i<*klin;i++)
                	{
				winsertln(cwin[1]);
                	}
        	}
/*        	delwin(cwin[1]);    */
/*		touchwin(cwin[0]);  */
/*		dwin[0] = 1;        */
        }
        else
        {
        if (OPEN1 == 0)
                {
/*               printf ("%c[?4i",27); !!VT100 emulator TE100TOOL doesn't 
                                         work correctly with it   */
                 printf ("%c[%d;%dr",27,*kbeg,*kend);  
                 printf ("%c[%d;%df\n",27,*kend,80);
                 OPEN1 = 1;
                }
#endif
#ifdef UU_RS6000
        ibeg = *kbeg - 1;
        isiz = *kend - 1;
        wsetscrreg(cwin[0],ibeg,isiz);
        scrollok(cwin[0],TRUE);
        idlok(cwin[0],FALSE); 
/*
...Scroll up
*/
        if (*kdir == 1)
        {
            for (i=0;i<*klin;i++)
            {
                scroll(cwin[0]);
            }
        }
/*
...Scroll down
*/
        else
        {
            wmove(cwin[0],isiz,0);
            wclrtobot(cwin[0]);
            wmove(cwin[0],ibeg,0);
            for (i=0;i<*klin;i++)
            {
                winsertln(cwin[0]);
            }
        }

#endif
	}
#endif
   return;
}

/***********************************************************************
c
c   SUBROUTINE:  trmatt
c
c   FUNCTION:  This routine opens the terminal for reading & writing.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
***********************************************************************/
 
trmatt(kfl)
int *kfl;

{
#if UU_COMP != UU_WIN2K
int *status, i;

#if UU_COMP == UU_VAXVMS
   UM_int2 ifl, mode, mode1;
   extern CHANEL;             /* from digs/gttio.c  */
   ifl=35;
   getifl(&ifl,&mode);
   ifl=350;
   getifl(&ifl,&mode1);
   if(mode == 2 && mode1 == 0)   /* NCLVT mode   */
   {
   $DESCRIPTOR(sname,"SYS$INPUT");   
   istat = sys$assign(&sname,&ichan,3,0);
   iorlb = IO$_TTYREADALL | IO$M_NOECHO;
   }
/*   if(mode == 2 && mode1 > 0)   /* NCL501+ mode */
/*   {
/*   iorlb = IO$_TTYREADALL | IO$M_NOECHO;
/*   ichan = CHANEL;
/*   uu_ttput(uw_41xx.ttfd,"\033%!0",4);
/*   uu_ttput(uw_41xx.ttfd,"\033LL",3); uw_ws410xinteg(24);
/*   uu_ttput(uw_41xx.ttfd,"\033KW0",4);
/*   uu_ttput(uw_41xx.ttfd,"\033%!1",4);
/*   uu_newline_flush(uw_41xx.ttfd);
/*
/*   }*/
#endif

#if UU_COMP != UU_VAXVMS
   UM_int2 ifl, mode, mode1;
   ifl=35;
   getifl(&ifl,&mode);
   ifl=350;
   getifl(&ifl,&mode1);
/*   if(mode == 2 && mode1 == 1)  /* NCL501+ mode */
/*   {
/*   uu_ttput(uw_41xx.ttfd,"\033%!0",4);
/*   uu_ttput(uw_41xx.ttfd,"\033LL",3); uw_ws410xinteg(24);
/*   uu_ttput(uw_41xx.ttfd,"\033KW0",4);
/*   uu_ttput(uw_41xx.ttfd,"\033%!1",4);
/*   uu_newline_flush(uw_41xx.ttfd);
/*   }*/
#endif
   
    CURSES1 = *kfl;

/*
...Trap keyboard interrupts
*/
 
#if UU_COMP == UU_VAXVMS       
        if (mode == 2 && mode1 == 0)  status = (int *)signal(SIGINT,trmast);   
#else
        status = (int *)signal(SIGINT,trmast);
#endif

        status = (int *)signal(SIGQUIT,SIG_IGN);     
#if UU_COMP != UU_VAXVMS
/*        status = (int *)signal(SIGTSTP,SIG_IGN);*/
#endif

    if (CURSES1 == 1) 
    {
    
   
/*
...Initialize curses
*/
		initscr();
 		noecho();
		nonl();
#if UU_COMP != UU_VAXVMS 
        noraw();
  		cbreak();  
#endif

/*
...Define window
*/
		cwin[0] = stdscr;
		cwin[1] = 0;
        	scrollok(cwin[0],TRUE);
		clearok(cwin[0],FALSE);
#if UU_COMP == UU_IRIS4D
#ifndef UU_RS6000
  idlok(cwin[0],TRUE); 
#endif
#endif
	}
/*
...Intitialize terminal input mode
*/
	else
	{

#if UU_COMP != UU_VAXVMS
	TTY1 = isatty(1); 
		if (TTY1 != 0) system ("stty raw  -echo"); 
#endif
	}
#endif
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  trmnl (knt)
c
c   FUNCTION:  This routine moves the cursor to the beginning of the
c              next line.
c
c   INPUT:  knt     I*4  D1  -  Number of lines to move the cursor down.
c
c   OUTPUT: none.
c
***********************************************************************/
 
trmnl(knt)
int *knt;
{
#if UU_COMP != UU_WIN2K
	int i;
        if (CURSES1 == 1) 
        {
#ifdef UU_RS6000 
        for (i=0;i<*knt;i++) wprintw(cwin[0],"\n");
#else    
        for (i=0;i<*knt;i++) wprintw(cwin[0],"\n\r");
#endif
		wflush();
	}
	else
	{
#ifdef UU_RS6000
        for (i=0;i<*knt;i++) printf ("\n");
#else 
		if (TTY1 == 0)
		{
			for (i=0;i<*knt;i++) printf ("\n");
		}
		else
		{
			for (i=0;i<*knt;i++) printf ("\n\r");
		}
#endif
	}
#endif
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  trmrst
c
c   FUNCTION:  This routine resets the terminal to it's original state.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
***********************************************************************/
 
trmrst()
{
UM_int2 ifl, mode, mode1;
#if UU_COMP != UU_WIN2K
	if (CURSES1 == 1) 
    {
/*
....Added later for NCL501+ mode
....Paul. 10/30/92
*/
        ifl = 35;
        getifl(&ifl,&mode);
        ifl = 350;
        getifl(&ifl,&mode1);
        if(mode == 2 && mode1 == 1)
        {
        unauth (&UL_cam, &UL_cad);
/*
.....This is almost "ul_sysexit call (umb/lsignon.c) exept call
.....of exit() routine. In this version we have to close "curses"
.....before. Paul. 11/01/92
*/
        nclfin();
#if UU_COMP == UU_VAXVMS
        ul_reset_control();
#endif
        UR_changed = UU_FALSE;
        uu_app_done();
        uu_mpe_done();
        }

        if ( OPEN1 == 1) delwin(cwin[1]);  
        delwin(cwin[0]);  
        endwin();

        exit();

	}
	else
	{
#if UU_COMP != UU_VAXVMS
	/* system ("stty -raw -nl echo"); */
#endif 
	}
/*******	CURSES = 0; ********/
/*
....Added for NCL501+ mode
....Paul. 03/03/92
*/
        ifl = 35;
        getifl(&ifl,&mode);
        ifl = 350;
        getifl(&ifl,&mode1);
        if(mode == 2 && mode1 == 1)
        {
        unauth (&UL_cam, &UL_cad);
        ul_sysexit();
        }
#endif
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  wflush
c
c   FUNCTION:  This routine flushes the terminal's output.
c
c   INPUT:  none.
c
c   OUTPUT: none.
c
***********************************************************************/
 
wflush()
{
#if UU_COMP != UU_WIN2K
        if (CURSES1 == 1) 
        {
#if UU_COMP == UU_VAXVMS 
                inch();
#endif
		wrefresh(cwin[0]);
	    }
	return;
#endif
}

/***********************************************************************
c
c   SUBROUTINE:  savescr
c
c   FUNCTION:  This routine saves screen before *EDT and *SYSTEM command
c              in the VT mode only.
c   INPUT:  none.
c
c   OUTPUT: none.
c
***********************************************************************/

savescr()
{
int y,x;
#if UU_COMP != UU_WIN2K
for (y=0; y <=23; y++)
{
  for (x=0; x <= 79; x++)
  {
    wmove(cwin[0],y,x);
    map[y][x] = inch();
  }
}
#endif
return;
}

/***********************************************************************
c
c   SUBROUTINE:  restscr
c
c   FUNCTION:  This routine restores screen after *EDT and *system com.
c              in the VT mode only.
c   INPUT:  none.
c
c   OUTPUT: none.
c
***********************************************************************/

restscr()
{
int y,x;
#if UU_COMP != UU_WIN2K
for (y=0; y <=23; y++)
{
  for (x=0; x <= 79; x++)
  {
    wmove(cwin[0],y,x);
    winsch(cwin[0],map[y][x]);
  } 
} 
#endif
return; 
}

