/*********************************************************************
**
**    NAME         :  zstatus
**
**       CONTAINS:
**    		uz_status_init
**    		uz_status
**    		uz_load_status
**				uz_actpart
**				uz_actscale
**				uz_actlayer
**				uz_actunits
**				uz_actgrid
**          uz_actcapture
**				uz_actrp
**    		uz_clear_status
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zstat.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:38
*********************************************************************/
#include "usysdef.h"
#if UU_COMP!=UU_WIN2K
#include <unistd.h>
#include "xfsys1.h"
#include "usysg.h"
#include "driver.h"
#include "gi.h"
#include "gsegdet.h"
#include "mdattr.h"
#include "mattr.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mdraw.h"
#include "dsubcom.h"
#include "udebug.h"
#include "dcapture.h"
#include "uims.h"
#include "lcom.h"
#include "mfort.h"
#include "nclfc.h"
#include "mxxx.h"

typedef struct
{
	int field;					/* field number */
	Gnpoint3 loc;				/* lower left corner of message */
} UZ_STATREC;

/* x,y,z locations stored as delta values from llx, lly of status area */
/* see layout file to location of status area */
static UZ_STATREC zstat_off[9] =
{{ 3, {.008, 0.033, 0.}},
 { 4, {.008, 0.019, 0.}},
 { 5, {.008, 0.005, 0.}},
 { 0, {.0  , 0.0,   0.}},
 { 6, {.095, 0.033, 0.}},
 { 7, {.095, 0.019, 0.}},
 { 8, {.208, 0.033, 0.}},
 { 9, {.208, 0.019, 0.}},
 {10, {.208, 0.005, 0.}}};

static UZ_STATREC zsystem;
static UZ_STATREC zredef;
static UZ_STATREC zdscale;
static UZ_STATREC zunits;
static UZ_STATREC zlayer;
static UZ_STATREC zdname;
static UZ_STATREC zpname;
static UZ_STATREC zfname;

UX_pathname partdir;
extern char UR_dpn[];
static int UPDATE = 1;

/* STATUS AREA BOUNDS */
static Gdrect swin;

void uz_actsystem(),uz_actprogram(),uz_actpart(),uz_actscal(),uz_actlayer();
void uz_actunits(),uz_actredef(),uz_actscale();

/*********************************************************************
**
** status area layout:
** 1:NIS/CAM/CAD      4:LAYER   7:DIRECTORY
** 2:REDEF            5:UNITS   8:PROGRAM
** 3:DSCALE           6:N/A    9:UNIBASE
**
*********************************************************************/
/*********************************************************************
**
**    E_FUNCTION :  uz_status_init()
**			Get size of status area from layout
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/
void uz_status_init()
	{
	UD_AREA *win;			/* pointer to an array of areas */
	int i;
	UZ_STATREC *zstatptr[9];

	/* Init status fields into pointer */
	zstatptr[0] = &zsystem;
	zstatptr[1] = &zredef;
	zstatptr[2] = &zdscale;
	zstatptr[3] = UU_NULL;
	zstatptr[4] = &zunits;
	zstatptr[5] = &zlayer;
	zstatptr[6] = &zdname;
	zstatptr[7] = &zpname;
	zstatptr[8] = &zfname;

	/* Get status area corrdinates  */
	win= &UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_GRAF][1];
	swin.ll.x = (*win).posn.ll.x; 
	swin.ll.y = (*win).posn.ll.y;
	swin.ur.x = (*win).posn.ur.x;
	swin.ur.y = (*win).posn.ur.y;

	/* Calculate location for fields */
	for (i = 0; i < 9; i++)
		{
		if (zstatptr[i] != UU_NULL)
		{
			zstatptr[i]->field = zstat_off[i].field;
/*
.....Protect against illegal FP instruction
*/
			zstatptr[i]->loc.x = 0;
			zstatptr[i]->loc.y = 0;
		}
	}

	return;
}
/*********************************************************************
**
**    E_FUNCTION :  uz_status()
**			redraw status area for DD/1
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/
void uz_status()
	{
	uu_denter(UU_DTRC,(us,"uz_status()"));

#if UU_COMP == UU_CIM
	uz_clear_status();
#endif
	/* flag other routines to not worry about updating affected fields */
	UPDATE=0;
/*	-- write out status information -- */
	/*ORDER IMPORTANT IF SCREEN IS SMALL ... */
	uz_actscale();
	uz_actredef();
	uz_actsystem(); 
	uz_actlayer(ur_get_attrmdl_layer());
	uz_actunits(UM_cpln.length_unit);
	uz_actpart(UR_dpn);
	uz_actprogram("");

	UPDATE = 1;

	uu_dexit;
	}

/*********************************************************************
**
**    E_FUNCTION :  uz_actsystem()
**			Update current system
**
**    PARAMETERS   
**       INPUT  :	 none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uz_actsystem()
	{
	char buffer[40];
	struct UM_drawing_rec drawing;

#if UU_COMP == UU_CIM
	if (UPDATE)
		{
		uz_status();
		return;
		}
#endif
	ud_killstat(zsystem.field);

/*	-- write out status information -- */
/*
.....Allow for DRAWING application
.....added by Yurong
.....8/20/97
*/
	if (UM_2d3d_mode == UM_2D)
	{	
		drawing.key = ur_get_drwmdl_curdrw();
		if (drawing.key == 0)
			sprintf(buffer,"DRAWING ");
		else
		{
			um_get_all_geom(&drawing, sizeof(drawing));
			sprintf(buffer,"DRAWING = %s", drawing.name);
		}
	}
/*
.....NCLCAM application
*/
	else
	{
		sprintf(buffer,"NCLCAM  ");
	}
	ud_wrstat(zsystem.field, &zsystem.loc, buffer);
	}
/*********************************************************************
**
**    E_FUNCTION :  uz_load_status()
**			redraw status area for DD/1 after a part load
**
**    PARAMETERS   
**       INPUT  :	 none
**
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

void uz_load_status()
{

	extern char UR_dpn[];

	uu_denter(UU_DTRC,(us,"uz_load_status()"));

/*	-- write out status information -- */
	if (UPDATE)
		uz_status();
	else
		uz_actpart(UR_dpn);

	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION         :  uz_actdir(dname)
**       write active directory
**    PARAMETERS   
**       INPUT  : 
**          dname = directory name area
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_actdir(dname)
char *dname;					/* directory file name */
	{
	char buffer[UX_MAX_PATH_LEN];

#if UU_COMP == UU_CIM
	if (UPDATE)
		{
		uz_status();
		return;
		}
#endif
/*		-- bring dowm old message -- */
	ud_killstat(zdname.field);

/*		-- format new message and store away -- */
	/* sprintf(buffer,"DIR=%s", dname); */
	if (dname[0]=='\0')
	{
		sprintf(buffer,"DIR=%s", partdir);
		chdir(partdir);
	}
	else
	{
		sprintf(buffer,"DIR=%s", dname);
		chdir(dname);
	}
	ud_wrstat(zdname.field, &zdname.loc, buffer);
}
/*********************************************************************
**    E_FUNCTION         :  uz_actprogram(pname)
**       write active part program file name
**    PARAMETERS   
**       INPUT  : 
**          pname = part program name
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_actprogram(pname)
char *pname;					/* part program file name */
	{
	char buffer[UX_MAX_PATH_LEN];
	char dir[UX_MAX_PATH_LEN], fname[UX_MAX_PATH_LEN];

#if UU_COMP == UU_CIM
	if (UPDATE)
		{
		uz_status();
		return;
		}
#endif
/*		-- bring dowm old message -- */
	ud_killstat(zpname.field);

/*		-- format new message and store away -- */
	if (*pname != NULL)
	{
		ul_break_fname(pname, dir, fname);
		sprintf(buffer,"PROGRAM=%s", fname);
	}
	else
	{
		ul_break_fname(UL_program, dir, fname);
		sprintf(buffer,"PROGRAM=%s.%s", fname,UL_program_suffix);
	}
	ud_wrstat(zpname.field, &zpname.loc, buffer);

	if (UPDATE)
		uz_actdir(dir);
}
/*********************************************************************
**    E_FUNCTION         :  uz_actpart(fname)
**       write active file name to status area
**    PARAMETERS   
**       INPUT  : 
**          fname = file name area
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_actpart(fname)
char *fname;					/* part file name */
	{
	char buffer[UX_MAX_PATH_LEN];
	char dir[UX_MAX_PATH_LEN], name[UX_MAX_PATH_LEN];

/*
.....Added for NCL501+ mode.
.....Paul. 04/01/92
*/
        UM_int2 mode, mode1;
        mode = 350;
        getifl(&mode, &mode1);
        if (mode1 == 1 || mode1 == 2) return;


#if UU_COMP == UU_CIM
	if (UPDATE)
		{
		uz_status();
		return;
		}
#endif
/*		-- bring dowm old message -- */
	ud_killstat(zfname.field);

/*		-- format new message and store away -- */
/*
.....break filename down to directory and name
*/
	ul_break_fname(fname, dir, name);

	sprintf(buffer,"UNIBASE=%s", name);
	ud_wrstat(zfname.field, &zfname.loc, buffer);

	if (UPDATE)
		uz_actprogram("");
	uz_actdir(dir);
}

/*********************************************************************
**
**    E_FUNCTION         :  uz_actscale()
**
**       write active drawing scale to status area
**
**    PARAMETERS   
**
**       INPUT  : 
**
**          none
**
**       OUTPUT :  
**
**          none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

void uz_actscale()
{
	char dscale[100];
	char buffer[80];
 
/* 
.....Added for NCL501+ mode. 
.....Paul. 04/01/92 
*/ 
        UM_int2 mode, mode1; 
        mode = 350; 
        getifl(&mode, &mode1); 
        if (mode1 == 1 || mode1 == 2) return; 
 

#if UU_COMP == UU_CIM
	if (UPDATE)
		{
		uz_status();
		return;
		}
#endif
		ud_killstat(zdscale.field);

/*		-- format new message and store away -- */

		ur_get_drwmdl_unitsstr(dscale);
		sprintf(buffer,"D.S.=%s", dscale);
		ud_wrstat(zdscale.field, &zdscale.loc, buffer);

	if (UPDATE)
		{
		uz_actredef();
		uz_actlayer(ur_get_attrmdl_layer());
		uz_actpart(UR_dpn);
		}
}

/*********************************************************************
**
**    E_FUNCTION         :  uz_actlayer(layer)
**
**       write active file name to status area
**
**    PARAMETERS   
**
**       INPUT  : 
**
**          layer = new current layer
**
**       OUTPUT :  
**
**          none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

void uz_actlayer(layer)
int layer;						/* new layer number */
{
	char buffer[40];

 
/* 
.....Added for NCL501+ mode. 
.....Paul. 04/01/92 
*/ 
        UM_int2 mode, mode1; 
        mode = 350; 
        getifl(&mode, &mode1); 
        if (mode1 == 1 || mode1 == 2) return; 
 

#if UU_COMP == UU_CIM
	if (UPDATE)
		{
		uz_status();
		return;
		}
#endif

		ud_killstat(zlayer.field);

/*		-- format new message and store away -- */

		sprintf(buffer,"Layer = %d", layer);
		ud_wrstat(zlayer.field, &zlayer.loc, buffer);
	
	if (UPDATE)
		uz_actunits(UM_cpln.length_unit);
}

/*********************************************************************
**
**    E_FUNCTION         :  uz_actunits(units)
**
**       write active input units to status area
**
**    PARAMETERS   
**
**       INPUT  : 
**
**          units = input units
**
**       OUTPUT :  
**
**          none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

void uz_actunits(units)
int units;					/* input units */
{
	char buffer[40];

 
/* 
.....Added for NCL501+ mode. 
.....Paul. 04/01/92 
*/ 
        UM_int2 mode, mode1; 
        mode = 350; 
        getifl(&mode, &mode1); 
        if (mode1 == 1 || mode1 == 2) return; 
 

#if UU_COMP == UU_CIM
	if (UPDATE)
		{
		uz_status();
		return;
		}
#endif

		ud_killstat(zunits.field);

/*		-- format new message and store away -- */
/*
.....use full name
*/	
/*		sprintf(buffer,"Units = %s", UM_linear_units_name[units]); */
		if (units==0)
			strcpy(buffer, "Units = Inches");
		else if (units==3)
			strcpy(buffer, "Units = Millimeters");
		ud_wrstat(zunits.field, &zunits.loc, buffer);
}

/*********************************************************************
**
**    E_FUNCTION         :  upstat()
**       Update REDEF status (update entire status so area does not
**       become corrupted on smaller screens.)
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void upstat()
	{
	uz_status();
	return;
	}
/*********************************************************************
**
**    E_FUNCTION         :  uz_actredef()
**       write redef status to status area
**    PARAMETERS   
**       INPUT  : 
**          redef = "on" or "off"
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_actredef()
	{
	char buffer[40];
	UM_int2 ivl;
	UM_int2 ifl=41;

#if UU_COMP == UU_CIM
	if (UPDATE)
		{
		uz_status();
		return;
		}
#endif
	ud_killstat(zredef.field);

/*	-- write out status information -- */
	switch (UU_application)
		{
		case UU_NCLNIS:
		case UU_NCLCAM:
		case UU_NCLCADD:
			getifl(&ifl, &ivl);
			if (ivl == 0)
				sprintf(buffer,"REDEF OFF");
			else
				sprintf(buffer,"REDEF ON ");
			break;
		}
/*		-- format new message and store away -- */
	ud_wrstat(zredef.field, &zredef.loc, buffer); 

	if (UPDATE)
		uz_actsystem();
	}

/*********************************************************************
**
**    E_FUNCTION         :  uz_actgrid(grid)
**       write gird status to status area
**    PARAMETERS   
**       INPUT  : 
**          grid = "on" or "off"
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* Not used  */
void uz_actgrid(grid)
char *grid;					/* part file name  */
	{
/*	ud_killstat(zgrid.field); */
/*		-- format new message and store away -- */
/*	ud_wrstat(zgrid.field, &zgrid.loc, grid);  */
	}

/*********************************************************************
**
**    E_FUNCTION :  uz_clear_status()
**			clear status area for DD/1
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/
void uz_clear_status()
{
	ud_killstat(zdscale.field);
	ud_killstat(zredef.field);
	ud_killstat(zsystem.field);
	ud_killstat(zlayer.field);
	ud_killstat(zunits.field);
	ud_killstat(zfname.field);
	ud_killstat(zpname.field);
	ud_killstat(zdname.field);
}
/*********************************************************************
**
**    E_FUNCTION         :  uz_actcapture(capture)
**       write capture staus to status area
**    PARAMETERS   
**       INPUT  : 
**          capture = "on" or "off"
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void uz_actcapture(capture)
char *capture;
{
/*		ud_killstat(zcapture.field);
		ud_wrstat(zcapture.field, &zcapture.loc, capture); */
}
/*********************************************************************
**    E_FUNCTION         :  uz_actrp(rp)
**       write R/P state to status area
**    PARAMETERS   
**       INPUT  : 
**          rp = "record on", "record off", or "playback"
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_actrp(rp)
char *rp;					/* r/p state message */
{
/*		ud_killstat(zrp.field);
		ud_wrstat(zrp.field, &zrp.loc, rp); */
}

/*********************************************************************
**    E_FUNCTION         :  wrtstat(name, nc1, msg, nc2)
**       write a Status message into a status button
**		not implement for UNIX yet
**    PARAMETERS   
**       INPUT  : 
**          name = status button name
**			msg:   message to be write
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void wrtstat(name, nc1, msg, nc2)
UM_f77_str_ptr name, msg;
UM_int4 *nc1,*nc2;
{
}
/*********************************************************************
**    E_FUNCTION         :  uz_actsequnc(label)
**       write Sequnc label
**    PARAMETERS   
**       INPUT  : 
**          label = Sequnc label
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uz_actsequnc(label)
char *label;
{
	UM_int2 iclf;
	char lcom[80];
	int nc1, nc2;

	if (label[0]=='\0')
	{
		iclf = 0;
		ncl_clseq_label(&iclf,lcom,0);
		if (lcom[0] == '\0')
		{
			nc1 = 6;
			nc2 = 3;
			wrtstat("SEQUNC", &nc1, "OFF", &nc2);
			return;
		}
		else
		{
			nc1 = 6;
			nc2 = strlen(lcom);
			wrtstat("SEQUNC", &nc1, lcom, &nc2);
		}
	}
	else
	{
		nc1 = 6;
		nc2 = strlen(label);
		wrtstat("SEQUNC", &nc1, label, &nc2);
	}
}
#endif
