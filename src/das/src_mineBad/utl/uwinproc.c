/*********************************************************************
**    NAME         :  uwinproc.c
**       CONTAINS:
**       uu_winproc(type,async,rect,title,file) -- start a process in a window.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uwinproc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:56
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "g.h"
#include "ginqdsiz.h"
#include "gdidd.h"
#if (UU_COMP==UU_APOLLO) 
/*#include "/sys/ins/base.ins.c"
#include "/sys/ins/pgm.ins.c"
#include "/sys/ins/error.ins.c"*/
#endif

/*********************************************************************
**    E_FUNCTION :  uu_winproc(ws,type,parallel,rect,title,file) -start process 
**							in a workstation window.
**    PARAMETERS   
**       INPUT  : 	Gws *ws; -- workstation to bring window up on.
**							int type; -- 0=text window, 1=graphics window.
**							int parallel; -- 0= current process is suspended until new 
**												process ends.
**											  1=new process runs in parallel with current 
**												process.
**							Gnrect *rect; -- NDC rectangle of the window.
**							char *title; -- title for the new window.
**							char *file; -- name of the executable file for the new
**												process.
**       OUTPUT :  
**    RETURNS      :  0 if all went ok, 1 if input arg errors,i
**							 2 if system error, 3 if this computer doesn't support it.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_winproc(ws,type,parallel,rect,title,file)
Gws *ws;					/* workstation to bring window up on */
int type;				/* 0=text window, 1=graphics window */
int parallel;			/* 0=runs serially. 1=file runs in parallel. */
Gdrect *rect;			/* where the new window goes, in device coords */
char *title;			/* title for the new window */
char *file;				/* name of the executable file for the new process */
{
#if (UU_COMP==UU_SUN)
#define RASMAX 1023
#define RASYMAX 767
#endif
#if (UU_COMP==UU_IRIS)
#define RASMAX 1023
#define RASYMAX 767
#endif
#if (UU_COMP==UU_HPUX)
#define RASMAX 1023
#define RASYMAX 767
#endif
#if (UU_COMP==UU_IRIS4D)
#define RASMAX 1023
#define RASYMAX 767
#endif
#if (UU_COMP==UU_APOLLO)
#define RASMAX 1023
#define RASYMAX 767
#endif
#ifndef RASMAX
#define RASMAX 1023
#define RASYMAX 767
#endif
	int irtn;
	Gipoint rasll,rasur;
	int dx,dy,top,left;

	uu_denter(UU_UITRC,(us,"uu_winproc(%d,%d,%g %g %g %g,%s,%s)",
		type,parallel,(*rect).ll.x,(*rect).ll.y,(*rect).ur.x,(*rect).ur.y,
		title, file));
	irtn=3;
	/* calculate size and top left of window, assuming 0,0 is top left */
	rasll.x=(*rect).ll.x*RASMAX;
	rasll.y=(*rect).ll.y*RASMAX;
	rasur.x=(*rect).ur.x*RASMAX;
	rasur.y=(*rect).ur.y*RASMAX;
	dx=rasur.x-rasll.x+1;
	dy=rasur.y-rasll.y+1;
	left=rasll.x;
	top=RASYMAX-rasur.y;
#if (UU_COMP==UU_SUN)
	{
		static char sunnames[2][10]={"shelltool","gfxtool"};
		static char sunparallel[2][2]={" ","&"};
		char str[200];
		sprintf(str,"%s -Ws %d %d -Wp %d %d -Wl \"%s\" %s %s",
			&sunnames[type][0],dx,dy,left,top,title,file,
			&sunparallel[parallel][0]);
		uu_dprint(UU_UITRC,(us,"uw_winproc. sun system(%s)",str));
		system(str);
		irtn=0;
	}
#endif
#if (UU_COMP==UU_IRIS) || (UU_COMP==UU_IRIS4D) || (UU_COMP == UU_HPUX)
	if ((type!=0)||(parallel!=0)) {
		uu_dprint(-1,(us,
		"uwinproc error. iris can't do text or parallel windows"));
		irtn=1;
	}
	else {
		system(file);
		irtn=0;
	}
#endif
#if (UU_COMP==UU_APOLLO) 
	{
		/*pgm_$proc handle;
		/*status_$t status;
		/*short n;
		/*static pgm_$arg str;
		/*static name_$pname_t comxdmc={'/','c','o','m','/','x','d','m','c','\0'};
		/*static pgm_$arg xdmc={4,'x','d','m','c','\0'};
		/*static pgm_$argv pargv={(int *)&xdmc,(int *)&str};
	
		/* declare and load the standard streams */
		/*static pgm_$connv connv={stream_$stdin, stream_$stdout,
								/*stream_$errin, stream_$errout};
		/*sprintf(str.chars,"cp %s",file);
		/*str.len=strlen(str.chars);
		/*uu_dprint(UU_UITRC,(us,"uwinproc apollo invoke(%s)",str.chars));
		/*n=strlen(str.chars);
		/*pgm_$invoke(comxdmc,				/* pgm to be invoked */
					/*n,							/* name length */
					/*(short)2,pargv,		/* argc, arg-vector */
					/*connv,					/* std. streams */
					/*pgm_$wait,
					/*handle,
					/*status);
		/*if (status.all!=status_$ok) {
			/*uu_dprint(-1,(us,"uu_winproc apollo invoked pgm ended with error %d",
					/*status.all));
		/*} */
		char str[200];
		sprintf(str,"/com/xdmc (%d,%d)dr; (%d,%d)cp %s",left,top,
			left+dx,top+dy,file);
		uu_dprint(UU_UITRC,(us,"uwinproc apollo str=%s",str));
		system(str);
		irtn=0;
	}
#endif
rtn:	uu_dprint(UU_UITRC,(us,"uu_winproc returns %d.",irtn));
	uu_dexit;
	return(irtn);
}
