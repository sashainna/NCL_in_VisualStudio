/*********************************************************************
**    NAME         :  ginqinp.h
**       CONTAINS:
**       gqninp(ws) -- inquire no. logical input devices.
**			gqdefloc(ws,devno) -- inquire default loc device data.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ginqinp.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:19
*********************************************************************/

#ifndef GINQINPH
/*********************************************************************
	E_FUNCTION: Gdefloc *gqdefloc(ws,devno) -- inquire default loc device data.
*********************************************************************/
#define gqdefloc(ws,devno) (&(*(*ug_gksstli.wsopen[*ws].wdtptr).inwdtpt).defloc[devno-1])

/*********************************************************************
	E_FUNCTION: Gdefstroke *gqdefstroke(ws,devno) -- inquire default stroke 
																	device data.
*********************************************************************/
#define gqdefstroke(ws,devno) (&(*(*ug_gksstli.wsopen[*ws].wdtptr).inwdtpt).defstk[devno-1])

/*********************************************************************
	E_FUNCTION: Gdefval *gqdefval(ws,devno) -- inquire default val device data.
*********************************************************************/
#define gqdefval(ws,devno) (&(*(*ug_gksstli.wsopen[*ws].wdtptr).inwdtpt).defval[devno-1])

/*********************************************************************
	E_FUNCTION: Gdefchoice *gqdefchoice(ws,devno) -- inquire default choice 
																	device data.
*********************************************************************/
#define gqdefchoice(ws,devno) (&(*(*ug_gksstli.wsopen[*ws].wdtptr).inwdtpt).defcho[devno-1])

/*********************************************************************
	E_FUNCTION: Gdefpick *gqdefpick(ws,devno) -- inquire default pick 
																	device data.
*********************************************************************/
#define gqdefpick(ws,devno) (&(*(*ug_gksstli.wsopen[*ws].wdtptr).inwdtpt).defpick[devno-1])

/*********************************************************************
	E_FUNCTION: Gdefstring *gqdefstring(ws,devno) -- inquire default string 
																	device data.
*********************************************************************/
#define gqdefstring(ws,devno) (&(*(*ug_gksstli.wsopen[*ws].wdtptr).inwdtpt).defstr[devno-1])
#define GINQINPH
#endif
