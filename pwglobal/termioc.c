
#if !defined WNT
/***********************************************************************
c
c   FILE NAME:  termioc.c
c   CONTAINS:
c               clreol  clreos  clrscr  crslft  crsrgt  dmpbuf  gtch
c               hilit   plott   rstcur  savcur  scrol  trmatt  trmerr
c               trmnl   trmrst  wflush  opnlogfil clslogfil
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        termioc.c , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:19
c
***********************************************************************/

#include <curses.h>
#include <string.h>
static WINDOW *cwin[2];
static int lasty,lastx,dwin[2],lstwin,ipmax;
static int CURSES=0,TTY=0;

#if (!defined IBM && !defined HP)
#define clreol clreol_
#define clreos clreos_
#define clrscr clrscr_
#define crslft crslft_
#define crsrgt crsrgt_
#define dmpbuf dmpbuf_
#define gtch gtch_
#define hilit hilit_
#define plott plott_
#define rstcur rstcur_
#define savcur savcur_
#define scrol scrol_
#define trmatt trmatt_
#define trmerr trmerr_
#define trmnl trmnl_
#define trmrst trmrst_
#define wflush wflush_
#define opnlogfil opnlogfil_
#define clslogfil clslogfil_
#endif

FILE *PW_logfile = NULL;
/***********************************************************************
c
c   SUBROUTINE:  opnlogfil (logfil, nc)
c
c   FUNCTION:  This routine open a log file
c
c   INPUT:  logfil: log file name
c				nc: length of log file name
c
c   OUTPUT: none.
c
***********************************************************************/
opnlogfil (logfil, nc)
char *logfil;
int *nc;
{
	logfil[*nc] = '\0';
	PW_logfile = fopen(logfil, "a+");
}
/***********************************************************************
c
c   SUBROUTINE:  clslogfil
c
c   FUNCTION:  This routine close a log file
c
c   INPUT:  none
c
c   OUTPUT: none.
c
***********************************************************************/
clslogfil ()
{
	if (PW_logfile != NULL)
		fclose(PW_logfile);
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
	if (CURSES == 1)
	{
	        wclrtoeol(cwin[0]);
       		dwin[0] = 1;
	}
	else
	{
		if (TTY !=0) printf ("%c[K",27);
	}
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
        if (CURSES == 1)
        {
		wclrtobot(cwin[0]);
		dwin[0] = 1;
	}
	else
	{
		if (TTY !=0) printf ("%c[J",27);
	}
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
	if (CURSES == 1)
	{
		wclear(cwin[0]);
		dwin[0] = 1;
	}
	else
	{
		if (TTY !=0) printf ("%c[2J",27);
	}
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
	int x,y,i;

        if (CURSES == 1)
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
		if (TTY !=0) for (i=0;i<*knt;i++) printf ("%c[D",27);
	}
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
	int x,y,i;

	if (CURSES == 1)
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
                if (TTY !=0) for (i=0;i<*knt;i++) printf ("%c[C",27);
        }
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
char *cbuf;
int *knc;
{
	int nc;
	char obuf[256];
/*
...Set up dummy buffer
*/
      	nc = *knc;
       if (nc > 0) strncpy (obuf,cbuf,nc);
      	obuf[nc] = '\0';
/*
...Dump output buffer
*/
	if (CURSES == 1)
	{
		wprintw (cwin[0],"%s",obuf);
		dwin[0] = 1;
	}
	else if (PW_logfile==NULL)
	{
		printf ("%s",obuf);
	}
	else
		fprintf(PW_logfile, "%s",obuf);

	return;
}

/***********************************************************************
c
c   SUBROUTINE:  gtch (kch)
c
c   FUNCTION:  This routine reads a character from the keyboard.
c
c   INPUT:  none.
c
c   OUTPUT: kch     I*4  D1  -  Numeric value of input character.
c
***********************************************************************/
 
gtch(kch)
int *kch;
{
        if (CURSES == 1)
        {
		if (dwin[0] == 1) wrefresh(cwin[0]);
		dwin[0] = 0;
		*kch = getch();
	}
	else
	{
		if (TTY !=0)
		{
			*kch = getchar();
		}
		else
		{
			*kch = 3;
		}
	}
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
/*
...Turn hiliting off
*/
	if (*kfl == 0)
	{
        	if (CURSES == 1)
        	{
			wstandend(cwin[0]);
		}
		else
		{
			if (TTY !=0) printf ("%c[0m",27);
		}
	}
/*
...Turn hiliting on
*/
	else
	{
        	if (CURSES == 1)
		{
			wstandout(cwin[0]);
		}
		else
		{
			if (TTY !=0) printf ("%c[7m",27);
		}
	}
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  plott (klin,kcol)
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
 

plott(klin,kcol)
int *klin,*kcol;
{
        int ilin,icol;
/*
...Window system is based on
...x=0, y=0 instead of 1,1
*/
	if (CURSES == 1)
	{
        	ilin = *klin - 1;
        	icol = *kcol - 1;
		wmove (cwin[0],ilin,icol);
		dwin[0] = 1;
	}
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
        if (CURSES == 1) 
        {
		wmove(cwin[0],lasty,lastx);
		dwin[0] = 1;
	}
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
        if (CURSES == 1) 
        {
		getyx(cwin[0],lasty,lastx);
	}
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  scrol (kbeg,kend,klin,kdir)
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

scrol(kbeg,kend,klin,kdir)
int *kbeg,*kend,*klin,*kdir;
{
	int ibeg,isiz,i;
/*
...Define scrolling region
*/
	if (CURSES == 1) 
	{
#if defined IBM || defined SGI
		ibeg = *kbeg - 1;
		isiz = *kend - 1;
		wsetscrreg(cwin[0],ibeg,isiz);
		scrollok(cwin[0],TRUE);
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
#else
		ibeg = *kbeg - 1;
		isiz = *kend - ibeg;
		cwin[1] = subwin(cwin[0],isiz,80,ibeg,0);
		scrollok(cwin[1],TRUE);
/*
...Scroll up
*/
        	if (*kdir == 1)
        	{
			for (i=0;i<*klin;i++)
                	{
				scroll(cwin[1]);
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
        	delwin(cwin[1]);
			touchwin(cwin[0]);
#endif
		dwin[0] = 1;
	}
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
	void *junk;
	CURSES = *kfl;
        if (CURSES == 1) 
        {
/*
...Initialize curses
*/
		initscr();
/*
...Set terminal attributes
*/
		noecho();
		nonl();
		raw();
/*
...Define window
*/
		cwin[0] = stdscr;
		cwin[1] = 0;
        	scrollok(cwin[0],TRUE);
		clearok(cwin[0],FALSE);
	}
/*
...Intitialize terminal input mode
*/
	else
	{
		TTY = isatty(0);
		if (TTY != 0) system ("stty raw -echo");
	}
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
	int i;
        if (CURSES == 1) 
        {
#ifdef IBM
		for (i=0;i<*knt;i++) wprintw(cwin[0],"\n");
#else
		for (i=0;i<*knt;i++) wprintw(cwin[0],"\n\r");
#endif
		wflush();
	}
	else if (PW_logfile==NULL)
	{
#ifdef IBM
		for (i=0;i<*knt;i++) printf ("\n\r");
#else
		if (TTY == 0)
		{
			for (i=0;i<*knt;i++) printf ("\n");
		}
		else
		{
			for (i=0;i<*knt;i++) printf ("\n\r");
		}
#endif
	}
	else
	{
#ifdef IBM
		for (i=0;i<*knt;i++) fprintf (PW_logfile, "\n\r");
#else
		for (i=0;i<*knt;i++) fprintf (PW_logfile, "\n");
#endif
	}
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
	if (CURSES == 1) 
        {
		endwin();
		system ("stty -raw -nl echo icrnl onlcr");
	}
	else
	{
		if (TTY !=0) system ("stty -raw -nl echo icrnl onlcr");
	}
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
        if (CURSES == 1) 
        {
		wrefresh(cwin[0]);
	}
	return;
}
#endif
