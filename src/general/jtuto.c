/*********************************************************************
**    NAME         :  jtuto.c
**       CONTAINS:
**				uj_tutorial
**				uji_titlepage
**				uji_itempag
**				uj_tut_on
**				uj_tut_off
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       jtuto.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       jtuto.c , 25.1
*********************************************************************/

#include "usysdef.h"
#include	"ustdio.h"
#include "uims.h"
#include "dwindow.h"
#include "dasg.h"
#include "dasnog.h"
#include "dmark.h"
#include "dinput.h"
#include "dtuto.h"
#include "dmark.h"
#include "dmenucom.h"
#include "xenv1.h"
#include "gcolors.h"
#include "uhep.h"
#include "udebug.h"

static int wincreated;						/* window created flag */
static int	offset;

/*********************************************************************
**
**    E_FUNCTION :  uj_tutorial(tutotb)
**					main routine for tutorials
**       
**    PARAMETERS   
**       INPUT  : 
**				 tutfnm = tutorial file name
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uj_tutorial (tutfnm)
char	*tutfnm; 				/* tutorial file name */
{
	UD_TUTO	*tutotb;	
	int	fd1, fd2;
	FILE  *fdpt1;
	UX_pathname	pathnm; 	/* replaces char	pathnm[UU_MAXPATHLEN] */
	int	chnu, status;
	int	markval;
	int	ch;
	int 	mode;
	char	buf[120];
	char sympath[200];

	uu_denter(UU_DTRC,(us,"enter uj_tutorial, tutfnm=%s", tutfnm));

	wincreated = 0;
	mode = 0;

/*	-- check for existence of .hlp file -- */

	sprintf(sympath,"UJ_TUTO/%s",tutfnm);
	if(ux_search_for_path(sympath, pathnm, UX_PRTERRS|UX_NCHK|UX_QUOTES) 
						!= UU_SUCCESS)
	{
		uu_uerror1(UJ_SUPPORT,7,pathnm);
		uu_dexit;
		return;
	}


	if((status = ux_open_to_data(pathnm,"r","STREAM","TEXT",&fd1,UX_PRTERRS))
		!= UU_SUCCESS)
	{
		uu_dprint(UU_DTRC,(us,"in uj_tutorial fopen error, status=%d, pathnm=%s",
						status, pathnm));
		uu_uerror1(UJ_SUPPORT,7,pathnm);
		uu_dexit;
		return;
	}

   ux_get_os_filedesc(fd1,&fdpt1,UX_PRTERRS);
	uji_getfls(fdpt1,tutfnm,&tutotb);
	while (1)
   {
		UD_MARK(markval,UU_FALSE);
		if(markval== 0)
		{
			ch= uji_titlepage(fd1, fdpt1, tutotb->pagenu, tutotb->itemnu, 
										tutotb->row,tutotb->col);
			if(ch == UD_CDONE) 	
			{
				status= ux_close(fd1,UX_PRTERRS);
				UD_UNMARK(markval);
				uji_free(tutotb);
				uu_dexit;
				return;
			}
			else	 
			{
				ux_file_rewind(fd1,UX_PRTERRS);
				ux_fseek0(fdpt1,offset,0);
				if((ch >= 'a')&&(ch <= 'c'))
					chnu= ch - 'a';
				else
					chnu= ch - '1';
			}
			ch= uji_itempage(tutotb->items[chnu].filnm, 
								tutotb->items[chnu].pages,tutotb->row);
			uu_dprint(UU_DTRC,(us,"afteritempage, ch=%d, UD_CDONE=%d",
									ch,UD_CDONE));
			if(ch == UD_CDONE)
			{
				UD_UNMARK(markval);
				uji_free(tutotb);
				uu_dexit;
				return;
	 		}
	 	}
	 	else
	 	{
			if(wincreated!= 0)
				ul_close_window();
/*				ud_kiwin();*/
			UD_UNMARK(markval);
			uji_free(tutotb);
			uu_dexit;
			return;
	 	}
		UD_UNMARK(markval);
   }
}	/* uj_tutorial */

/*********************************************************************
**    I_FUNCTION :  uji_titlepage(fd1, fdpt1, pnu, itemnu, row, col)
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uji_titlepage(fd1,fdpt1, pnu, itemnu,row,col)
int	pnu, itemnu;
int	fd1;
FILE	*fdpt1;
int	row, col;
{
	int	i, j, pnum, chnu, done;
	char	message[82], buf[8];
	UD_DEVENT event;								/* event buffer */
	UD_AREA *areapt;								/* pointer to an array of areas */
	static UD_EVENTDEF inparm = {1, ""};	/* string input parameters */
	char ichar;
	int	drow, dcol;
	int bckgrnd;									/* bckgrnd color of ansi window */
	UD_WINDOW_REC wcb;							/* window control block */
	int args[2];

	uu_denter(UU_DTRC, (us, "uji_titlepage, itemnu=%d, row=%d, col=%d", 
									itemnu, row, col));
	
	pnum = 1;
	drow = row;
	dcol = col;
	if(! wincreated)
	{
/*		bckgrnd = dqwinback();
/*		ud_initwin_rec(&wcb, UD_HELPWIN, UG_C_WHITE, bckgrnd);
/*		ud_crwin(&wcb, &drow, &dcol);*/
		args[1] = 1;
		ul_open_window(drow,dcol,args);

		wincreated = 1;
		uu_dprint(UU_DTRC,(us, "after ud_crwin, row=%d, column=%d", drow, dcol));
	}

	while (1)
   {
/*		ud_eraswin();*/
		for (i=0; i<row ; i++)
  	   {
			ux_fgets0(message,82,fdpt1);
			uu_dprint(UU_DTRC,(us,"**%s**",message));
/*			ud_wrwin(message);*/
			ul_win_out(message,0);
     	}
		done = UU_FALSE;
		do
		{
			ud_gevt(&event, UD_STRING, "", 1, 1, 1, &inparm);
			uu_dprint(UU_DTRC,(us, "after ud_gevt,choicedata=%d",
				event.indata.choicedata));

			if((event.evclass==UD_CHOICE)&&(event.evdev==UD_AUXMENU))
		  	{
				if(event.indata.choicedata == UD_CDONE)
			  	{
/*					ud_kiwin();*/
					ul_close_window();
					uu_dexit;
					return(event.indata.choicedata);
				}
				else
					uu_uerror0(UJ_SUPPORT,8);
			}
			else if(event.evclass == UD_STRING)
			{
		  		switch (event.indata.stringdata[0])
			   {
				 	case	'\0':				/* return */
						pnum = 1;
						ux_file_rewind(fd1,UX_PRTERRS);
						ux_fseek0(fdpt1,offset,0);
						done = UU_TRUE;
						break;
			
				 	case '\040':				/* space  */
						if(pnum == pnu)
						{
							pnum = 1;
							ux_file_rewind(fd1,UX_PRTERRS);
							ux_fseek0(fdpt1,offset,0);
						}
						else
						  pnum++;
						done = UU_TRUE;
						break;
		
					case '1': case '2': case '3': case '4': case '5':
					case '6': case '7': case '8': case '9':
					case 'a': case 'b': case 'c':
						ichar = event.indata.stringdata[0];
						if((ichar >= 'a')&& (ichar <= 'c'))
							chnu = ichar - 'a';
						else
							chnu = ichar - '0';
						if(chnu <= itemnu)
						{
							if(pnum == pnu)	/* last page */
							{
								/* ud_kiwin(); */
								uu_dexit;
								return(ichar);
							}
							else
								/* printf("illegal input\n"); */
								uu_uerror0(UJ_SUPPORT,8);
						}
						else
							/* printf("item number out of range\n");	*/
							uu_uerror0(UJ_SUPPORT,9);
						break;
		
					default:
						/* printf("illegal input\n"); */
						uu_uerror0(UJ_SUPPORT,8);
						break;
				}	/* switch */
			}
		}
		while (!done);
   }	/* while */
}	/* uji_titlepage */

/*********************************************************************
**    I_FUNCTION :  uji_itempage(fname, pnu, row)
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uji_itempage(fname, pnu, row)
char	*fname;						/* file name */
int	pnu;
int	row;
{
	FILE	*fdpt2;
	int	fd2, status;
	int	i, j, pnum, done;
	int	mode;
	char	message[82], buf[8];
	UD_DEVENT event;		/* event buffer */
	char	pathnm[33];
	static UD_EVENTDEF inparm = {1, ""};	/* string input parameters */
	
	uu_denter(UU_DTRC,(us,"uji_itempage"));

	pnum = 1;
	mode = 0;
	ux_mk_chk_syspath(UU_NULL,"^UJ_TUTO", fname, UU_NULL, UU_NULL, &mode,
		pathnm, UX_PRTERRS);
	if((status=ux_open_to_data(pathnm,"r","STREAM","TEXT",&fd2,UX_PRTERRS)) 
		!= UU_SUCCESS)
	{
		/* printf("Can't open file %s", fname); */
		uu_uerror1(UJ_SUPPORT,7,fname);
		uu_dexit;
		return('\015');
	}
	else
	{
    	ux_get_os_filedesc(fd2,&fdpt2,UX_PRTERRS);
	 	while (1)
	 	{
/*			ud_eraswin();*/
			for (i=0; i<row ; i++)
		  	{
				ux_fgets0(message, 82,fdpt2);
/*				ud_wrwin(message);*/
				ul_win_out(message,0);
	     	}
			done = UU_FALSE;
			do
			{
				ud_gevt(&event, UD_STRING, "", 1, 1, 1, &inparm);
				if((event.evclass==UD_CHOICE)&&(event.evdev==UD_AUXMENU))
				{
					if(event.indata.choicedata == UD_CDONE)
					{
/*						ud_kiwin();*/
						ul_close_window();
						ux_close(fd2, UX_PRTERRS);
						uu_dexit;
						uu_dprint(UU_DTRC,(us,"choicedata=%d",
									event.indata.choicedata));
						return(event.indata.choicedata);
					}
					else
						uu_uerror0(UJ_SUPPORT,8);
				}
				else if(event.evclass == UD_STRING)
				{
				  	switch (event.indata.stringdata[0])
					{
					 	case	'\0':				/* return */
							/* ud_kiwin(); */
							ux_close(fd2, UX_PRTERRS);
							uu_dexit;
							return('\015');
							break;
				
					 	case '\040':				/* space  */
							if(pnum == pnu)
							{
								ux_close(fd2, UX_PRTERRS);
								uu_dexit;
								return('\015');
							}
							else
							  pnum++;
		
							done = UU_TRUE;
							break;
			
						default:
							/* printf("illegal input\n"); */
							uu_uerror0(UJ_SUPPORT,8);
							break;
					}	/* switch */
				}
			} while (!done);
   	}	/* while */
	}		/* else  */
}	/* uji_itempage */

/*********************************************************************
**    I_FUNCTION :  uji_getfls(fd,tutfnm,tutotb)
**       Given the tutorial topic file name, put all the associated
**			information such as number of pages, item file name, etc. into
**			a table;
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uji_getfls(fd,tutfnm,tutotb)
FILE	*fd;
char	*tutfnm;
UD_TUTO	**tutotb;
{
	register	 i, k;
	char	buf[120];
	UD_TOPICS	*ptr;

	uu_denter(UU_DTRC,(us,"enter uji_getfls"));

	*tutotb = (UD_TUTO *) uu_toolmalloc (sizeof(UD_TUTO));
	ux_fgets0(buf,120,fd);
	sscanf(buf,"%d %d %d %d", &((*tutotb)->row),&((*tutotb)->col),
									  &((*tutotb)->pagenu),&((*tutotb)->itemnu));
	k = (*tutotb)->itemnu;
	if (k==0)
		(*tutotb)->items = UU_NULL;
	else
	  {
		ptr = (*tutotb)->items = (UD_TOPICS *) uu_toolmalloc(k*sizeof(UD_TOPICS));
		for (i=0; i<k; i++)
   	{
			ux_fgets0(buf,120,fd);
			sscanf(buf,"%d %s",&(ptr->pages),ptr->filnm);
			uu_dprint(UU_DTRC,(us,"pages=%d,filnm=%s",ptr->pages,ptr->filnm));
			ptr++;
		}
	  }
	ux_ftell0(fd,&offset);
	uu_dprint(UU_DTRC,(us,"row=%d,col=%d,pagenu=%d,itemnu=%d",
	(*tutotb)->row,(*tutotb)->col,(*tutotb)->pagenu,(*tutotb)->itemnu));

	uu_dexit;
}	/* uji_getfls */


/*********************************************************************
**    I_FUNCTION :  uji_free(tutotb)
**       free the tutorial table
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uji_free(tutotb)
UD_TUTO	*tutotb;
{
   if (tutotb->items != UU_NULL)
		uu_free(tutotb->items);
	uu_free(tutotb);
}	/* uji_free */

/*********************************************************************
**
**    E_FUNCTION     :  int uj_tut_on()
**       Tutorial on for DDC and DD/1
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uj_tut_on()
{
	uu_denter(UU_DTRC,(us,"in uj_tut_on"));
	UD_tut_mode = UD_TUT_ON;
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION     :  int uj_tut_off()
**       Tutorial off for DDC and DD/1
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uj_tut_off()
{
	uu_denter(UU_DTRC,(us,"in uj_tut_off"));
	UD_tut_mode = UD_TUT_OFF;
	uu_dexit;
}
