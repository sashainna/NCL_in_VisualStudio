
/********************************************************************* 
**  NAME:  gansi.c
**
**	 							ANSI terminal functions.
**
**	 CONTAINS:
**			gansion(area,dev,lines,cols)		gansioff(ws,dev) 
**			gansivis(ws,dev)						gansinvis(ws,dev)
**			gansiup(ws,dev)						gansidwn(ws,dev)
**			gansilft(ws,dev)						gansirgt(ws,dev)
**			gansiec2e(ws,dev) 					gansieb2c(ws,dev) 
**			gansiel(ws,dev) 						gansiec2es(ws,dev) 
**			gansiebs2c(ws,dev) 					gansiescr(ws,dev) 
**			gansispos(ws,dev,pos)				gansiqpos(ws,dev,pos)			
**
**  COPYRIGHT  1984  UNICAD, Inc.
** 
**    MODULE NAME AND RELEASE LEVEL
**       gansi.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:16
**
*********************************************************************/

#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "ginq.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "udebug.h"
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gansi.c 2.5 3/21/86 08:50:43 single"};
#else
static char uu_sccsident[]={"@(#) gansi.c 2.5 3/21/86 08:50:43 double"};
#endif

/*********************************************************************
**    I_FUNCTION     :  gansion(ws,area,dev,lines,cols)			
**				Define ansi terminal dev.
**				For possible future use.
**    PARAMETERS   
**       INPUT  :    ws					Workstation terminal dev is on.
**							Gdrect *area; -- echo area.
**							dev				Ansi terminal number.
**							int lines,cols; -- size of ansi terminal window.
**       OUTPUT :  none 
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
/* gansion(ws,area,dev,lines,cols)
/* Gws *ws;
/* Gdrect *area;
/* int dev;
/* int lines,cols;
/* {
/* uu_denter(UU_GTRC,(us,"gansion(%d, %g %g %g %g, dev=%d, lines,cols=%d %d)", 
		/* *ws, (*area).ll.x,(*area).ll.y,(*area).ur.x,(*area).ur.y,dev,
		/* lines,cols));
	/* here must add dev to the list of ansi windows maintained in 
		ug_dinitstr in wsdev.c */
	/* (*(ug_gksstli.wsopen[*ws].connid)[UG_DANSION])(*ws,area,dev,lines,cols);
/* 	uu_dexit;
/* } */

/*********************************************************************
**    I_FUNCTION     :  gansioff(ws,dev)			
**				Delete ansi terminal dev.
**				For possible future use.
**    PARAMETERS   
**       INPUT  :    ws					Workstation terminal dev is on.
**							dev				Ansi terminal number.
**       OUTPUT :  none 
**    RETURNS      : none
**    SIDE EFFECTS : Ansi terminal area dev becomes visible if it isn't.
**    WARNINGS     : none
*********************************************************************/
/* gansioff(ws,dev)			
/* Gws *ws;
/* int dev;
/* {
	/* uu_denter(UU_GTRC,(us,"gansioff(*ws=%d, dev=%d)", *ws, dev));
	/* here must remove dev from the list of ansi windows maintained in 
		ug_dinitstr in wsdev.c */
	/* (*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIOFF])(*ws,dev);
	/* uu_dexit;
/* } */

/* ANSI Visibilities */

/*********************************************************************
**    E_FUNCTION     :  gansivis(ws,dev)			
**       
**				Make ansi terminal dev visible.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation terminal dev is on.
**							dev				Ansi terminal number.
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : Ansi terminal area dev becomes visible if it isn't.
**    WARNINGS     : none
*********************************************************************/
gansivis(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansivis(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIVIS])(ws,dev);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gansinvis(ws,dev)			
**       
**				Make ansi terminal dev invisible.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation terminal dev is on.
**							dev				Ansi terminal number.
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : Ansi terminal area dev becomes invisible.
**    WARNINGS     : none
*********************************************************************/
gansinvis(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansinvis(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSINVIS])(ws,dev);

	uu_dexit;
}

/* ANSI Cursor movements */

/*********************************************************************
**    E_FUNCTION     :  gansiup(ws,dev)			
**       
**			Move cursor up one row in ansi area dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansiup(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansiup(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIUP])(ws,dev);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gansidwn(ws,dev)			
**       
**			Move cursor down one row in ansi area dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansidwn(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansidwn(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIDWN])(ws,dev);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gansilft(ws,dev)			
**       
**			Move cursor left one column in ansi area dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansilft(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansilft(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSILFT])(ws,dev);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gansirgt(ws,dev)			
**       
**			Move cursor right one column in ansi area dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansirgt(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansirgt(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIRGT])(ws,dev);

	uu_dexit;
}

/* ANSI Terminal erasing functions */

/*********************************************************************
**    E_FUNCTION     :  gansiec2e(ws,dev)
**       
**			Erase from Cursor to End of line in ansi terminal dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansiec2e(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansiec2e(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIEC2E])(ws,dev);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gansieb2c(ws,dev)
**
** 	Erase from Beginning of line to Cursor in ansi device dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansieb2c(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansieb2c(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIEB2C])(ws,dev);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gansiel(ws,dev)
**
**    Erase Line cursor is on in ansi device dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansiel(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansiel(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIEL])(ws,dev);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gansiec2es(ws,dev)
**
**	 	Erase from Cursor to End of Screen in ansi device dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansiec2es(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansiec2es(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIEC2ES])(ws,dev);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gansiebs2c(ws,dev)
**
**		Erase from Beginning of Screen to Cursor in ansi device dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansiebs2c(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansiebs2c(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIEBS2C])(ws,dev);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gansiescr(ws,dev)
**
**		Erase ansi screen for ansi terminal dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansiescr(ws,dev)			
/*$ INPUT */
Gws *ws;
int dev;
{
	uu_denter(UU_GTRC,(us,"gansiescr(*ws=%d, dev=%d)", ws, dev));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIESCR])(ws,dev);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  gansispos(ws,dev,pos)			
**
**	 	Set current position (row, column) for ansi device dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**							pos				New position of cursor.
**       OUTPUT :  none 
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansispos(ws,dev,pos)			
/*$ INPUT */
Gws *ws;
int dev;
int pos[2];
{
	uu_denter(UU_GTRC,(us,"gansispos(%d, dev=%d pos=%d %d)", 
			*ws,dev,pos[0],pos[1]));

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSISPOS])(ws,dev,pos);

	uu_dexit;
}

/* ANSI Terminal Inquiry Functions */

/*********************************************************************
**    E_FUNCTION     :  gansiqpos(ws,dev,pos)			
**
**		Return current position (row, column) in pos for ansi device dev.
**
**    PARAMETERS   
**       INPUT  :  
**							ws					Workstation ansi area dev is on.
**							dev				Number of ansi terminal
**       OUTPUT :
**  						pos				Curent postion of cursor.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gansiqpos(ws,dev,pos)			
/*$ INPUT */
Gws *ws;
int dev;
/*$ OUTPUT */
int pos[2];
{
	char us[100];

	(*(ug_gksstli.wsopen[*ws].connid)[UG_DANSIQPOS])(ws,dev,pos);
	uu_denter2(UU_GTRC,(us,"gansiqpos %d dev=%d,pos=%d %d)",
			*ws, dev,pos[0],pos[1]));
	uu_dexit;
}
