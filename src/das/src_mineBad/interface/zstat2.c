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
**				uz_actcapture
**				uz_actsequnc(label)
**				uz_actrp
**    			uz_clear_status
**				wrtstat(name, msg)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**			zstat2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:38
*********************************************************************/
#include "usysdef.h"
#include "nclfc.h"
#if UU_COMP==UU_WIN2K
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
#include "mxxx.h"
#include "nclinp.h"
#include "zkeysym.h"


UX_pathname partdir;
extern char UR_dpn[];
static int UPDATE = 1;


void uz_actlayer(), uz_actunits(), uz_actscale(),uz_acttime(), uz_actpart(), 
	uz_actprogram(), uz_actsystem(), uz_actsequnc(), uz_actredef(), fdisp_stat();

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
**			initial status area from layout
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

}

/*********************************************************************
**
**    E_FUNCTION :  uz_status_inittable()
**			initial status table
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
void uz_status_inittable()
{
	int i, j; 
	for (i=0;i<UZ_MAX_STATNAME;i++)
	{
		for (j=0; j<MAX_STAT_FUNC; j++)
			UZ_status_table[i][j] = -1;
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
	UPDATE=0;
/*	-- write out status information -- */
	uz_actscale();
	uz_actredef();
	uz_actsystem(); 
	uz_actlayer(ur_get_attrmdl_layer());
	uz_actunits(UM_cpln.length_unit);
	uz_actpart(UR_dpn);
	uz_actprogram(" ");
	uz_actsequnc(" ");
	uz_acttime();
	fdisp_stat();
	UPDATE = 1;
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

	if (label[0]=='\0')
	{
		iclf = 0;
		ncl_clseq_label(&iclf,lcom,0);
		if (lcom[0] == '\0')
		{
			ud_wrstat("SEQUNC", "OFF");
			return;
		}
		else
			ud_wrstat("SEQUNC", lcom);
	}
	else
		ud_wrstat("SEQUNC", label);
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
/*	-- write out status information -- */
/*
.....Allow for DRAWING application
.....added by Yurong
.....8/20/97
*/
	if (UM_2d3d_mode == UM_2D)
	{	
		drawing.key = ur_get_drwmdl_curdrw();
		if (drawing.key == UU_NULL)
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
	ud_wrstat("NCLMODE", buffer);
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
char *dname;					
{
	char *chrs;
	if (dname[0]=='\0')
	{
/*
.....Get currect directory
*/
		chrs = getcwd(partdir,UX_MAX_PATH_LEN);
		ud_wrstat("DIRECTORY", partdir);
		_chdir(partdir);
	}
	else
	{
		ud_wrstat("DIRECTORY", dname);
		_chdir(dname);
	}
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
char *pname;				
{
	UX_pathname buffer, dir, fname;

	if (*pname != NULL)
		strcpy(buffer, pname); 
	else
		sprintf(buffer,"%s.%s", UL_program,UL_program_suffix);

/*
.....break filename down to directory and name
*/
	ul_break_fname(buffer, dir, fname);
	ud_wrstat("PROGRAM", fname);

	if (UPDATE)      
		uz_actdir(dir);
	ud_update_win_title();
done:;
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
	UX_pathname dir;
	char name[UX_MAX_FILE_LEN];
	UM_int2 mode, mode1;
	mode = 350;
	getifl(&mode, &mode1);
	if (mode1 == 1 || mode1 == 2) return;
/*
.....break filename down to directory and name
*/
	ul_break_fname(fname, dir, name);

	ud_wrstat("UNIBASE", name);

	if (UPDATE)
		uz_actprogram("");
	uz_actdir(dir);
	ud_update_win_title();
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
 
	UM_int2 mode, mode1; 
	mode = 350; 
	getifl(&mode, &mode1); 
	if (mode1 == 1 || mode1 == 2) return; 
 
	ur_get_drwmdl_unitsstr(dscale);
	ud_wrstat("DRAFT_SCALE", dscale);

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
	UM_int2 mode, mode1; 
	mode = 350; 
	getifl(&mode, &mode1); 
	if (mode1 == 1 || mode1 == 2) return; 
 
	sprintf(buffer,"%d", layer);
	ud_wrstat("LAYER", buffer);
	
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
	UM_int2 mode, mode1; 
	mode = 350; 
	getifl(&mode, &mode1); 
	if (mode1 == 1 || mode1 == 2) return; 
	if (units==0)
		strcpy(buffer, "Inches");
	else if (units==3)
		strcpy(buffer, "Millimeters");
	ud_wrstat("UNITS", buffer);
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

	switch (UU_application)
	{
		case UU_NCLNIS:
		case UU_NCLCAM:
		case UU_NCLCADD:
			getifl(&ifl, &ivl);
			if (ivl == 0)
				sprintf(buffer,"OFF");
			else
				sprintf(buffer,"ON ");
			break;
	}
	ud_wrstat("REDEF", buffer); 

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
	UM_int2 ifl, ifl35=35;
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;

	ud_wrstat("DRAFT_SCALE", " ");
	ud_wrstat("REDEF", " ");
	ud_wrstat("NCLMODE", " ");
	ud_wrstat("LAYER", " ");
	ud_wrstat("UNITS", " ");
	ud_wrstat("UNIBASE", " ");
	ud_wrstat("PROGRAM", " ");
	ud_wrstat("DIRECTORY", " ");

	ud_wrstat("INPUT_MODE", " ");
	ud_wrstat("PROCESS_MODE", " ");
	ud_wrstat("REFSYS", " ");
	ud_wrstat("MODSYS", " ");
	ud_wrstat("TRACUT", " ");
	ud_wrstat("FEDRAT", " ");
	ud_wrstat("TOLER", " ");
	ud_wrstat("THICK", " ");
	ud_wrstat("TOOL_ENDPT", " ");
	ud_wrstat("TLAXIS", " ");
	ud_wrstat("TLAXIS_MODE", " ");
	ud_wrstat("SEQUNC", " ");
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
}


/*********************************************************************
**    E_FUNCTION         :  wrtstat(name, nc1, msg, nc2)
**       write a Status message into a status button
**    PARAMETERS   
**       INPUT  : 
**          name = status button name
**			nc1:   length of status button name
**			msg:   message to be write
**			nc2:   length of message
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void wrtstat(name, nc1, msg, nc2)
UM_f77_str_ptr name, msg;
int *nc1, *nc2;
{
	char *cmsg, token[UX_MAX_PATH_LEN];
	char *cnam, namstr[40];
	int len;
	cnam = UM_cstr_of_f77_str(name);
	cmsg = UM_cstr_of_f77_str(msg);
/*
.....the message will be put into a button,
.....so max allow 80
*/
	len = *nc2;
	if (len>=80) len = 79;
	strncpy(token, cmsg, len);
	token[len] = '\0';
/*
.....max allow name 40 chars
*/
	len = *nc1;
	if (len>=40) len = 39;
	strncpy(namstr, cnam, len);
	namstr[len] = '\0';
	ud_wrstat(namstr, token); 
}

/*********************************************************************
**
**    E_FUNCTION         :  uz_acttime()
**
**       write the the elapsed time to status area
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

void uz_acttime()
{
	char etime[100];
	int nc;
 
	UM_int2 mode, mode1; 
	mode = 350; 
	getifl(&mode, &mode1); 
	if (mode1 == 1 || mode1 == 2) return; 
 
	nclf_get_etime(etime, &nc);
	etime[nc] = '\0';
	ud_wrstat("TIME", etime);
}

#else
void uz_acttime() {}
#endif
