/*********************************************************************
**    NAME         :  qlang.c
**       CONTAINS:
**       	uq_calc
**				uqi_funcparse
**				uqi_lxconread
**				uqi_setend
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qlang.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:54
*********************************************************************/

#include 	"usysdef.h"
#include 	"ustdio.h"
#include		"uims.h"
#include		"dasnog.h"
#include		"dwindow.h"
#include		"uhep.h"
#include		"udebug.h"
#include		"calcom.h"
#include		"cdef.h"
#include    "gtbl.h"
#include    "g.h"
#include		"mdcoord.h"
#include		"tlangtool.h"
#include		"mdrel.h"
#include		"qlang.h"
#include  	"icalcsem.h"
#include		"gcolors.h"         /* color definitions - ud_crwin */
#include		"xenv1.h"

char	*uu_toolmalloc();
int	uqi_vmatch();
int *ut_lxread();
UD_WINDOW_REC UQ_wcb;	/* window control block */

/*********************************************************************
**    I_FUNCTION     :  uqi_setend ()
**       set the endflag to quit the uq_calculator
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_setend ()
{
	if (uq_calc2flag == UU_FALSE)
			UQ_cendflag[UQ_cendindex] = 1;
}		/* uqi_setend */


/*********************************************************************
**    E_FUNCTION     :  uq_calc()
**       main subroutine of the uq_calculator. call the langpak and the
**       input routine to start parsing the input line until user
**       input a command of 'QUIT' to quit the uq_calculator.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uq_calc()
{
	static int one=1;		/* static ok since never change value */
	int markval;
	int stack[1000];		/* stack area for uti_parser to use */
	ITSTYPE its[1000];	/* uti_parser returns translation sequence here*/
	int ltr;					/* length of returned translation sequence */
	int irc;					/* return code from ut_lxread or uti_parser */
	int in[200];			/* user input buffer */
	int len;					/* length of fname or input buffer */
	int irkf;				/* in[irkf-1] contains index of last
									matched character position of parse */
	UD_AREA *areapt;		/* pointer to an array of areas */
	Gdrect winbnd;			/* window bounds */
	char	us[132];
	int bckgrnd;			/* bckgrnd color of ansi window */
	int args[2];

 	if (UQI_clxconflag == UU_FALSE)
	if	(!(uqi_lxconread()))
			return;
	if (UQ_cendindex < 4)
	{

/*	-- create and bump windows -- */

	if(UD_duimsdeflt.screen[UD_curlayout.curr_screen].noareas[UD_HLP] != 0)
	  areapt= &(UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_HLP][0]);
	else
	 areapt= &(UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_GRAF][0]);
	ud_devrect(&((*areapt).posn), &winbnd);
	bckgrnd = dqwinback();
/*	WINDOW_INIT(&UQ_wcb, &winbnd, UG_C_WHITE, bckgrnd);
/*	WINDOW_ON(&UQ_wcb, &UD_winrow, &UD_wincol);*/
	args[1] = 1;
	UD_winrow = 20; UD_wincol = 80;
	ul_open_window(UD_winrow,UD_wincol,args);
	UQ_cendflag[++UQ_cendindex] = 0;
/*
.....Let the user know how to get out of here
.....Bobby  -  3/13/92
*/
/*	WINDOW_WRITE(&UQ_wcb, "Enter 'q' or 'quit' to exit calculator.\n\n");*/
	ul_win_out("Enter 'q' or 'quit' to exit calculator.\n",0);

/* 	-- keep parsing as long as successful -- */

	while (UQ_cendflag[UQ_cendindex]==0) 
  {
/* 		-- read a symbol sequence from the user to be parsed -- */

	if (ut_ctread(in,1,&len,&UQ_cendindex)==UU_TRUE)/* read the user's input line successfully */
	  {
		irc=0;
		UQI_cer1 = UU_FALSE;   
		UQI_cer2 = UU_FALSE;
		UQI_cfuncflag = UU_FALSE;
		UQ_cfnindex = 0;
		UQI_cstkpt = -1;
		UQI_cinptr = 1;			/* pointer to next symbol in the "in"  */
		UQI_cfpar = -1;

 /* 		-- call the langpak uti_parser routine to parse input -- */
		uti_parser(UQ_symbol,uq_symlength,in,one,len,UQ_lexicon,stack,
				&UQ_trace,&irkf,its,&ltr,&irc,semmch,uqi_vmatch,NULL);

		/* irc=0 if parse successful, else see page 128 of langpak book*/
		uu_denter2(UU_DTRC,(us,"after parsing, irc=%d", irc));
		uu_dexit;
		if (UQI_cdebug)
		   printf("uti_parser returned irc=%d\n",irc);
		if (irc == 1)
		  {
			if (UQI_cer1)
				/* gmessage (D_ksws, " error - undefined name "); */
				uu_uerror0 (UQ_CALCERROR, 1);
			else
			   /* gmessage (D_ksws, " error - syntax error!! "); */
				uu_uerror0 (UQ_CALCERROR, 2);
			uqi_resetstack ();
/*			WINDOW_WRITE(&UQ_wcb, "\n");*/
			ul_win_out(" ",0);
           }	
        }
   }	/* while */
	UQ_cendindex--;

/*	-- kill the window and adjust to next location -- */

/*	WINDOW_OFF(&UQ_wcb);*/
	ul_close_window();
	}	/* if    */
	else
		/* gmessage (D_ksws, " error - too many calc calls, action denied"); */
		uu_uerror0 (UQ_CALCERROR, 3);

}		/* uq_calc */

/*********************************************************************
**    I_FUNCTION     :  uqi_lxconread ()
**       read the lexicon file name and the lexicon file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_lxconread ()
{
	char c[40];
	static int one=1;		/* static ok since never change value */
	int fname[UX_MAX_PATH_LEN];			/* lexicon file name */
	int irc;					/* return code from ut_lxread */
	int len;					/* length of fname */

 	/* printf("enter lexicon file name: ");      */
	/* uti_tread(fname,one,&len);  */   	   /* read filename into fname */
	if (ut_cfnmread(fname,one,&len))   		/* read filename into fname */
	  {
/* 	-- allocate & read the lexicon file -- */

		UQ_lexicon=ut_lxread(fname,len,&irc);	
		if (irc!=1)
		  {
			/* gmessage (D_ksws, "error reading lexicon file "); */
			/* uu_uerror0 (UQ_CALCERROR, 5); */
			/* uti_twrite(fname,&one,&len); */
			/* exit(UU_ABORT); */
			uu_toolfree(UQ_lexicon);
			return(UU_FALSE);
		  }
		else
			UQI_clxconflag = UU_TRUE;

	/*
		printf(" tracing enabled(y/n)? ");
		gets(c);
		if (c[0]=='y') UQ_trace=1; else UQ_trace=0;

		printf(" UQI_cdebug enabled(y/n)? ");
		gets(c);
		if (c[0]=='y') UQI_cdebug=1; else UQI_cdebug=0;
	
		printf(" UQI_sdebug enabled(y/n)? ");
		gets(c);
		if (c[0]=='y') UQI_sdebug=1; else UQI_sdebug=0;

	*/
		UQI_cdebug = 0;
		UQI_sdebug = 0;
		UQ_trace = 0;

		UQ_symbol[1]='<'; 		/* "<" */
		UQ_symbol[2]='s';		/* "s" */
		UQ_symbol[3]='>';		/* ">" */
		uq_symlength=3;
		return(UU_TRUE);
	 }
  else
	 return(UU_FALSE);
}		/* uqi_lxconread */

/*********************************************************************
**    I_FUNCTION     :  uqi_funcparse()
**       call the uti_parser to parse the function definition
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uqi_funcparse()
{
	static int one=1;		/* static ok since never change value */
	int stack[1000];		/* stack area for uti_parser to use */
	ITSTYPE its[1000];	/* uti_parser returns translation sequence here*/
	int ltr;					/* length of returned translation sequence */
	int irc;					/* return code from uti_parser */
	int in[200];			/* user input buffer */
	int len;					/* length of input buffer */
	int irkf;				/* in[irkf-1] contains index of last
									matched character position of parse */

	irc=0;
	UQ_cfnindex++;
	UQI_cfuncflag = UU_TRUE;
		/* read a symbol sequence from the ymbol table to be parsed */
	uqi_cfread (in,1,&len);
	UQI_cinptr = 1;			/* pointer to next symbol in the "in"  */

			 /* call the langpak uti_parser routine to parse input */
	uti_parser(UQ_symbol,uq_symlength,in,one,len,UQ_lexicon,stack,&UQ_trace,
			&irkf,its,&ltr,&irc,semmch,uqi_vmatch,NULL);

	if (--UQ_cfnindex == 0)
   	UQI_cfuncflag = UU_FALSE;
		/* irc=0 if parse successful, else see page 128 of langpak book*/
	if (UQI_cdebug)
	   printf("uti_parser returned irc=%d\n",irc);
	if (irc == 1)
	  {
		if (UQI_cer1)
			/* gmessage (D_ksws, " error - undefined name "); */
			uu_uerror0 (UQ_CALCERROR, 1);
		else
		   /* gmessage (D_ksws, " error - syntax error!! "); */
		   uu_uerror0 (UQ_CALCERROR, 2);
     }
}		/* uqi_funcparse */

