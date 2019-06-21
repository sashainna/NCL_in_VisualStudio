/*********************************************************************
**    NAME         :  gkserror.c -- DIGS error functions.
**       CONTAINS:
**		gemergencyclosegks() --  emergency close.
**		Gerror ug_errorhand(err_no,funcname,args)
**		Gerror ug_chkerrfile(funcname) -- make sure error msg file is available.
**		Gerror wschk(ws) -- check ws is active.
**		Gerror ug_chkwsac(funcname)--check for active ws.
**		int ug_chkad(addr,funcname) -- check for legal addr.
**		Gerror ug_chkrect(rect,funcname) -- check 2D rectangle
**		Gerror ug_chkrect3(rect,funcname) -- check 3D rectangle.
**		Gerror ug_chkpoints3(n,points,funcname) -- check 3D points.
**		Gerror ug_chkpoints(n,points,funcname) -- check 2D points.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gerror.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:19
*********************************************************************/
#include <signal.h>
#include "umath.h"
#include "zsysdep.h"
#include <stdio.h>
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "ginq.h"
#include "gerror.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "uerror.h"
#include "udebug.h"
#include "xenv1.h"
#include "xfsys1.h"

char *ux_getenv();

/********************************************************************* 
**  E_FUNCTION:  gemergencyclosegks() --  emergency close.
**	 	Any open segment is closed. 
**		All workstations are updated. All active workstations are	
**		deactivated. All open workstations are closed. GKS is closed.
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
gemergencyclosegks()			/* emergency close gks */
	/* any open segment is closed. 
		All workstations are updated. All active workstations are	
		deactivated. All open workstations are closed. GKS is closed */
{
	int prms[5],reply[5];
	int i;

	uu_denter(-1,(us,"gemergencyclosegks()"));
	/* now only close workstations */
	prms[0]=UG_DCLOSEWS;
	for (i=0; i<ug_gksdesctbl.maxopws; i++) {    /* for each workstation*/
		if (ug_gksstli.wsopen[i].connid!=NULL) {
			prms[1]=ug_gksstli.wsopen[i].id;
			/* call workstation entry point thru table */
			(*(ug_gksstli.wsopen[i].connid)[prms[0]])(prms,reply);
   	}
   }
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  Gerror ug_errorhand(err_no,funcname,args)
**			DIGS error handler.
**		Invoked by GKS library functions which need to report an error.
**		This function calls the error logging function with the
**		error code, and handles the error.
**    PARAMETERS   
**       INPUT  : 
**				Gerror err_no	 the error code -- see gkserror.h
**				Gchar *funcname The GKS function causing the error
**				Gint *args		 pointer to argument list
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror ug_errorhand(err_no,funcname,args)		/* error handler */
	/* Invoked by GKS library functions which need to report an error.
		This function calls the error logging function with the
		error code, and handles the error */
Gerror err_no;					/* the error code -- see gkserror.h */
Gchar *funcname;				/* The GKS function causing the error */
Gint *args;						/* pointer to argument list */
{
	Gerror irtn;
	irtn=uu_erlog(GKS,err_no,funcname,args);	/* log the error */
	/* should handle the error here */
	if (irtn>=12) {			/* A fatal error, print trace stack and exit. */
#if UU_DEBUG==1
		uu_prtstk();
#endif
		exit(irtn);
	}
	return(err_no);
}

/*********************************************************************
**    I_FUNCTION     :  Gerror ug_chkerrfile(funcname)
**       Make sure gks error msg file is available.
**    PARAMETERS   
**       INPUT  :  char *funcname -- name of calling routine.
**       OUTPUT :  
**    RETURNS      : NCL_NO_ERROR if all went OK, else EIO_READ.
**    SIDE EFFECTS : Prints an error msg if couldn't open error file.
**    WARNINGS     : none
*********************************************************************/
Gerror ug_chkerrfile(funcname)		/* check gkserrors file is available */
char *funcname;
{
	FILE *fd;
	char *p;
	char us[120];
	int mode=UX_READ;
	char path[120];

	ux_mk_chk_syspath(NULL, NULL, "^gkserrors", NULL, NULL, &mode,
		path, UX_PRTERRS);

	if( mode & UX_READ ) 
		return(NCL_NO_ERROR);
	else {
		uu_erprt(GKS,0,12,EIO_READ,funcname,
			"gkserrors file can't be opened",NULL);
		return(EIO_READ);
	}

}

/*********************************************************************
**    I_FUNCTION     :  Gerror wschk(ws) -- check ws is active.
**    PARAMETERS   
**       INPUT  : 
**          ws			workstation id
**       OUTPUT :  
**          output
**    RETURNS      : error codes as follows:
**								EWSIDINV - invalid workstation id
**								ENOTWSAC - DIGS not in proper state
**								EWSNOTOP - workstation not open
**								EWSNTACT - workstation not active
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror ug_wschk(ws,funcname)
Gws ws;
char *funcname;
{
	Gerror irtn;
	irtn=NCL_NO_ERROR;
	if ((ws<0)||(ws>4))
	{
		ug_errorhand(EWSIDINV,funcname,ws);
		irtn=EWSIDINV;
	}
	if (ug_gksos.sysstate!=UG_WSAC)
	{
		ug_errorhand(ENOTWSAC,funcname,NULL); irtn=ENOTWSAC;
	}
	else if (ug_gksstli.wsopen[ws].connid==NULL)
	{															/*workstation is closed */
		ug_errorhand(EWSNOTOP,funcname,NULL); irtn=EWSNOTOP;
	}
	else if (ug_gksstli.wsopen[ws].state!=UG_ACTIVE)
	{															 /*workstation not active */
		ug_errorhand(EWSNTACT,funcname,NULL); irtn=EWSNTACT;
	}
															/* should check for MI or INPUT */
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION   :  Gerror ug_chkwsac(funcname)--check for active ws.
**      Check to see at least 1 workstation is active.
**    PARAMETERS   
**       INPUT  :  char *funcname -- name of function calling ug_chkwsac.
**       OUTPUT :  none.
**    RETURNS      : NCL_NO_ERROR if a workstation is active. Else returns
							ENOTWSOP.
**    SIDE EFFECTS : Issues ENOTWSOP error msg if no workstation is active.
**    WARNINGS     : none
*********************************************************************/
#ifdef UU_CHECK
Gerror ug_chkwsac(funcname)			/* check for an active workstation */
char *funcname;
{
	Gerror irtn;
	if ((ug_gksos.sysstate!=UG_WSAC)&&(ug_gksos.sysstate!=UG_SGOP)) {
		ug_errorhand(ENOTWSOP,funcname,NULL); irtn=ENOTWSOP;
	}
	else irtn=NCL_NO_ERROR;
	return(irtn);
}
#endif

/*********************************************************************
**    I_FUNCTION     :  int ug_chkad(addr,funcname) -- check for legal addr.
**       Check to see that addr points to within the data section of
**			this process.
**    PARAMETERS   
**       INPUT  :  char *addr -- the address to be checked.
**						 char *funcname -- name of function calling ug_chkad.
**       OUTPUT :  none.
**    RETURNS      : Gerror if addr is legal. Else EBADAD.
**    SIDE EFFECTS : Prints an error msg and exits if addr not legal.
**    WARNINGS     : none
*********************************************************************/
#ifdef UU_CHECK
Gerror ug_chkad(addr,funcname)
char *addr;								/* address to be checked */
char *funcname;						/* name of function calling ug_chkad.*/
{
	char us[120];
	if (uu_adok(addr)==1) return(NCL_NO_ERROR);
	uu_denter2(UU_GITRC,(us,"ug_chkad bad address=%x from %s",addr,funcname));
	ug_errorhand(EBADAD,funcname,addr);
	/* EBADAD is usually a fatal error, so errorhand never returns here */
	uu_dexit;
	return(EBADAD);
}
#endif
/*********************************************************************
**    I_FUNCTION   :  Gerror ug_chkrect(rect,funcname) 
**			Make sure rect is a valid rectangle.
**    PARAMETERS   
**       INPUT  :  Gwrect *rect -- rectangle to be checked.
**						 char *funcname -- name of GKS function calling ug_chkwrect.
**       OUTPUT :   none
**    RETURNS      : NCL_NO_ERROR if rect is OK, else EBADRCTD.
**    SIDE EFFECTS : issues GKS error message EBADRCTD.
**    WARNINGS     : none
*********************************************************************/
Gerror ug_chkrect(rect,funcname)		/* check rectangle, 2D */
Gwrect *rect;				/* rectangle to be checked */
char *funcname;			/* name of GKS function calling ug_chkwrect */
{
	Gerror irtn;
	Gfloat dx,dy;
	irtn=NCL_NO_ERROR;
	dx=(*rect).ur.x-(*rect).ll.x;
	dy=(*rect).ur.y-(*rect).ll.y;
	if ((dx<UG_EPS)||(dy<UG_EPS)||
		(fabs((*rect).ll.x)>UG_MAXFLOAT)||
		(fabs((*rect).ll.y)>UG_MAXFLOAT)||
		(fabs((*rect).ur.x)>UG_MAXFLOAT)||
		(fabs((*rect).ur.y)>UG_MAXFLOAT)) {
			ug_errorhand(EBADRCTD,funcname); irtn=EBADRCTD;
	}
	return(irtn);
}
/*********************************************************************
**    I_FUNCTION   :  Gerror ug_chkrect3(rect,funcname) 
**			Make sure rect is a valid rectangle.
**    PARAMETERS   
**       INPUT  :  Gwrect *rect -- rectangle to be checked.
**						 char *funcname -- name of GKS function calling ug_chkwrect.
**       OUTPUT :   none
**    RETURNS      : NCL_NO_ERROR if rect is OK, else EBADRCTD.
**    SIDE EFFECTS : issues GKS error message EBADRCTD.
**    WARNINGS     : none
*********************************************************************/
Gerror ug_chkrect3(rect,funcname)		/* check rectangle, 3D */
Gwrect3 *rect;				/* rectangle to be checked */
char *funcname;			/* name of GKS function calling ug_chkwrect */
{
	Gerror irtn;
	Gfloat dx,dy,dz;
	irtn=NCL_NO_ERROR;
	dx=(*rect).urb.x-(*rect).llf.x;
	dy=(*rect).urb.y-(*rect).llf.y;
	dz=(*rect).urb.z-(*rect).llf.z;
	if ((dx<UG_EPS)||(dy<UG_EPS)||(dz<UG_EPS)||
		(fabs((*rect).llf.x)>UG_MAXFLOAT)||
		(fabs((*rect).llf.y)>UG_MAXFLOAT)||
		(fabs((*rect).llf.z)>UG_MAXFLOAT)||
		(fabs((*rect).urb.x)>UG_MAXFLOAT)||
		(fabs((*rect).urb.y)>UG_MAXFLOAT)||
		(fabs((*rect).urb.z)>UG_MAXFLOAT)) {
			ug_errorhand(EBADRCTD,funcname); irtn=EBADRCTD;
	}
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION   :  Gerror ug_chkpoints3(n,points,funcname) 
**			Make sure 3D points are within range.
**    PARAMETERS   
**       INPUT  :  Gwpoint3 points[] -- array of points to be checked.
**						 int n -- number of points.
**						 char *funcname -- name of GKS function calling ug_chkwpoints.
**       OUTPUT :   none
**    RETURNS      : NCL_NO_ERROR if points are OK, else EBADPNT.
**    SIDE EFFECTS : issues GKS error message EBADPNT.
**    WARNINGS     : none
*********************************************************************/
Gerror ug_chkpoints3(n,points,funcname)		/* check 3D  points */
int n;						/* number of points */
Gwpoint3 points[];		/* points to be checked */
char *funcname;			/* name of GKS function calling ug_chkwpoints */
{
	int i;
	Gerror irtn;
	irtn=NCL_NO_ERROR;
	for (i=0; i<n; i++) {
		if ((fabs(points[i].x)>UG_MAXFLOAT)||
			(fabs(points[i].y)>UG_MAXFLOAT)||
			(fabs(points[i].z)>UG_MAXFLOAT)) {
				ug_errorhand(EBADPNT,funcname); irtn=EBADPNT;
				break;
		}
	}
	return(irtn);
}
/*********************************************************************
**    I_FUNCTION   :  Gerror ug_chkpoints(n,points,funcname) 
**			Make sure 2D points are within range.
**    PARAMETERS   
**       INPUT  :  Gwpoint points[] -- array of points to be checked.
**						 int n -- number of points.
**						 char *funcname -- name of GKS function calling ug_chkwpoints.
**       OUTPUT :   none
**    RETURNS      : NCL_NO_ERROR if points are OK, else EBADPNT.
**    SIDE EFFECTS : issues GKS error message EBADPNT.
**    WARNINGS     : none
*********************************************************************/
Gerror ug_chkpoints(n,points,funcname)		/* check 2D  points */
int n;						/* number of points */
Gwpoint points[];			/* points to be checked */
char *funcname;			/* name of GKS function calling ug_chkpoints */
{
	int i;
	Gerror irtn;
	irtn=NCL_NO_ERROR;
	for (i=0; i<n; i++) {
		if ((fabs(points[i].x)>UG_MAXFLOAT)||
			(fabs(points[i].y)>UG_MAXFLOAT)) {
				ug_errorhand(EBADPNT,funcname); irtn=EBADPNT;
				break;
		}
	}
	return(irtn);
}
