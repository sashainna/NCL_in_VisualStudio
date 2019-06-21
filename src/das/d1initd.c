/*********************************************************************
**
**    NAME         :  d1initd.c
**
**       CONTAINS:
**  			ud_idas
**  			ud_igks
**  			ud_ogks
**			
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d1initd.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:03
*********************************************************************/

#define EPGM
#include "derror.h"
#undef EPGM

#include "usysdef.h"
#include "uhep.h"
#include "uio.h"
#include "g.h"
#define DPGM 1
#include "dinput.h"
#include "dasnog.h"
#include "dasg.h"
#include "driver.h"
#include "usysg.h"
#include "dselect.h"
#include "dselmask.h"
#include "dsubcom.h"
#include "ddef.h"
#include "dmark.h"
#include "dsubcom.h"
#include "dcapture.h"
#include "dmotif.h"
#undef DPGM
#include "modef.h"
#include "unserve.h"
#include "mdattr.h"
#include "udebug.h"
#include "uims.h"
#include "xenv1.h"
int UD_gotuimsfil=0;							/* 0=uims file not read yet */
#define DEF
#include "jplot.h"
#undef DEF

/*
    EXPLANATION OF PROCESSING THE COLOR FIELD IN THE MODAL ATTRIBUTES FORM


The "SET ATTRIBS" field in the MODEL ENVIRON icon menu is processed under the 
control of the ZNCLENV.MU file.  This file generates a ZNCLENV.C module.  This
module processes the form by calling:
    umu_get_def_attr ([model]m2uattr.c)

    This calls:
        umu_attr_form ([model]m2uattrf.c)

        To handle the color field of the form, this calls:
            ncl_get_color ([nclc]necattr.c)

            With an argument of subroutine:
                ncl_get_attrmdl_color ([nclc)neattr.c)

                This returns the value of the variable:
                    NCL_model_color
                    Its value is a number indicating the index entry number in 
                    the UD_colortable array found in the d1initd.c ([das]) 
                    module.  This is the table of RGB values used to define the 
                    color mix of each color on the terminal.

            This index number is then used by ncl_get_color to return a value
            that is the number of which of the 8 toggle entries to use in the 
            chgattr.frm form's color field.

        After the form is put up and values entered, umu_attr_form then calls:
            ncl_set_color ([nclc]necattr.c)
            which uses the form's color field toggle number to index into a
            table and return the associated index entry number in the 
            UD_colortable array. (d1init.c)

    umu_get_def_attr then calls:
        ncl_set_color_attr ([nclc]neattr.c)

        This calls:
            ur_put_attrmdl.color

            Which is defined in mattrddl.h as:
                UM_attrmdl.color = where
            so this sets the variable UM_attrmdl.color to the UD_colortable
            array index number.

        It also sets:
            NCL_model_color to the UD_colortable array index number.
yurong note: we are not using UD_colortable anymore, we used uw_color_table defined in wsgl.h
when color_index = -1, it mean default. before default color mean NCL_model_color = 0, but
NCL_model_color = 0 mean black color, default color = -1

There are four (4) tables used by the color setting routines.  They are:

==============================================================================

The CHGATTR.FRM form which has an 8 entry table for the colors:

    0 - white
    1 - yellow
    2 - blue
    3 - red
    4 - green
    5 - magenta
    6 - cyan
    7 - default

==============================================================================

The UD_colortable in D1INITD.C which has a 16 entry table that defines the
RGB numbers each terminal/workstation uses to generate the colors:

    0 - background     
    1 - white          
    2 - dark blue
    3 - red 
    4 - green 
    5 - magenta
    6 - yellow         
    7 - cyan           ____________________________________
    8 - magenta            These entries are not used
    9 - red                            |
   10 - green                          |
   11 - blue                           |
   12 - orange                         |
   13 - pink                           |
   14 - green (light)                  |
   15 - blue (light)                   \/

==============================================================================

The NCL_GET_COLOR routine has an entry table which relates the UD_colortable
array value to the CHGATTR.FRM table value:

  UD_colortable  CHGATTR.FRM
      value          value

 (background)  0 -  7  (default)
      (white)  1 -  0  (white)
       (blue)  2 -  2  (default)
        (red)  3 -  3  (red)
      (green)  4 -  4  (green)
    (magenta)  5 -  5  (magenta)
     (yellow)  6 -  1  (yellow)
       (cyan)  7 -  6  (cyan)

==============================================================================

The NCL_SET_COLOR routine has an 8 entry table which relates the CHGATTR.FRM 
table value to the UD_colortable array value:

    CHGATTR.FRM  UD_colortable  
        value        value

     (white)  0 -  1  (white)
    (yellow)  1 -  6  (yellow)
      (blue)  2 -  2  (blue)
       (red)  3 -  3  (red)
     (green)  4 -  4  (green)
   (magenta)  5 -  5  (magenta)
      (cyan)  6 -  7  (cyan)
   (default)  7 -  0  (background)

==============================================================================
*/

#if UU_COMP == UU_VAXVMS
   /* static */ Gcobundl UD_colortable[16]={	/* ALPHA */
/* 0 */      0.,0.,0.,         /* UM_BACKGROUND */
/* 1 */      1.,1.,1.,         /* white           chgattr.frm color field # 0 */ 
/* 2 */      .1,.6,1.,         /* blue            chgattr.frm color field # 2 */ 
/* 3 */      1.,0.,0.,         /* red             chgattr.frm color field # 3 */
/* 4 */      0.,.8, 0.,        /* green           chgattr.frm color field # 4 */
/* 5 */      1.,0.,1.,         /* magenta         chgattr.frm color field # 5 */
/* 6 */      1.,1.,0.,         /* yellow          chgattr.frm color field # 1 */
/* 7 */      0.,1.,1.,         /* cyan            chgattr.frm color field # 6 */
             1.,0.,1.,         /* UM_MAGENTA      VMS doesn't use this */
             1.,0.,0.,         /* UM_RED          VMS doesn't use this */    
             0.,1.,0.,         /* UM_GREEN        VMS doesn't use this */
             0.,0.,1.,         /* UM_BLUE         VMS doesn't use this */
             1.,.51,0.,        /* UM_ORANGE       VMS doesn't use this */
             1.,.667,.667,     /* UM_PINK         VMS doesn't use this */
             0.,.667,0.,       /* UM_LIGHTGREEN   VMS doesn't use this */   
             0., 0.,.667       /* UM_LIGHTBLUE    VMS doesn't use this */
	};
#else
/* original unicad 2.0 colors (roughly) */
/*   static Gcobundl colortable[16]={ */
   Gcobundl UD_colortable[16]={
/* 0 */      .0,.0,.0,			/* background, set to black */
/* 1 */      1.,1.,1.,			/* white           chgattr.frm color field # 0 */
/* 2 */      0.,.6,1.,	   	/* blue            chgattr.frm color field # 2 */
/* 3 */      1.,0.,0.,			/* red             chgattr.frm color field # 3 */
/* 4 */      0.,1., 0.,		   /* green           chgattr.frm color field # 4 */
/* 5 */      1.,0.,1.,       	/* magenta         chgattr.frm color field # 5 */
/* 6 */      1.,1.,0.,			/* yellow          chgattr.frm color field # 1 */
/* 7 */      0.,1.,1.,			/* cyan            chgattr.frm color field # 6 */
             1.,0.,1.,			/* magenta         UNIX doesn't use this */
             1.,0.,0.,			/* red             UNIX doesn't use this */
             0.,1.,0.,			/* green           UNIX doesn't use this */
             0.,0.,1.,			/* blue            UNIX doesn't use this */
             1.,.51,0.,		   /* orange          UNIX doesn't use this */
             1.,.667,.667,		/* pink            UNIX doesn't use this */
             0.,.667,0.,		/* lt green        UNIX doesn't use this */
             0., 0.,.667	   /* lt blue         UNIX doesn't use this */
   };
#endif /* UU_VAXVMS */
/* 
   Gcobundl UD_colortable[16]={  Unicad 2.5 colors  
 		.392,.392,.431,	 UM_BACKGROUND 
 		1.,1.,1.,			 UM_WHITE  
 		0.,0.,0.,			 UM_BLACK  
 		.755,0.,0.,			 UM_DARKRED 
 		0.,.588, 0.,		 UM_DARKGREEN 
 		0.,0.,.588,			 UM_DARKBLUE 
 		1.,1.,0.,			 UM_YELLOW 
 		0.,1.,1.,			 UM_CYAN 
 		1.,0.,1.,			 UM_MAGENTA 
 		1.,0.,0.,			 UM_RED  	
 		0.,1.,0.,			 UM_GREEN 
 		0.,0.,1.,			 UM_BLUE 
 		1.,.51,0.,			 UM_ORANGE 
 		1.,.667,.667,		 UM_PINK 
 		.5,1.,.5,		 UM_LIGHTGREEN 	
 		0.5,0.5,1.		 UM_LIGHTBLUE 
 	}; 
*/

void ud_idas();
void ud_igks();
void ud_ogks();
	
/********************************************************************* 
**
**  I_FUNCTION		:  ud_idas() -- initialize the DAS
**      initialize the DAS
**
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

void ud_idas()
{
	char *uu_uprompt0();
	char *p, *ux_getenv();

	uu_denter(UU_DTRC,(us,"ud_idas()"));

/*	-- set syntactic prompts -- */

	strcpy(UD_synwc, uu_uprompt0(UD_DASHEP, 32));
	strcpy(UD_synsel, uu_uprompt0(UD_DASHEP, 33));
	strcpy(UD_syndis, uu_uprompt0(UD_DASHEP, 34));
	strcpy(UD_synint, uu_uprompt0(UD_DASHEP, 35));
	strcpy(UD_synvec, uu_uprompt0(UD_DASHEP, 36));
	strcpy(UD_synplc, uu_uprompt0(UD_DASHEP, 37));
	strcpy(UD_synstr, uu_uprompt0(UD_DASHEP, 38));
	strcpy(UD_synchc, uu_uprompt0(UD_DASHEP, 39));
	strcpy(UD_synndc, uu_uprompt0(UD_DASHEP, 40));
	strcpy(UD_synang, uu_uprompt0(UD_DASHEP, 41));
	strcpy(UD_synul, uu_uprompt0(UD_DASHEP, 42));
	strcpy(UD_synstk, uu_uprompt0(UD_DASHEP, 43));

/*	-- set lexical prompts -- */

	strcpy(UD_chcpmt, uu_uprompt0(UD_DASHEP, 11));
	strcpy(UD_valpmt, uu_uprompt0(UD_DASHEP, 12));
	strcpy(UD_locpmt, uu_uprompt0(UD_DASHEP, 13));
	strcpy(UD_pckpmt, uu_uprompt0(UD_DASHEP, 14));
	strcpy(UD_strpmt, uu_uprompt0(UD_DASHEP, 15));
	strcpy(UD_stkpmt, uu_uprompt0(UD_DASHEP, 44));

	
/* -- read in default UIMS from a layout file, gen segments for layout */
/*
......only read once, for native WinNT,
......we call ud_getuims before when we create
......frame, but call unicad_ (which will call
......here too) when we create view. we need
......read layout only one
......Yurong 7/10/00
*/
	if (UD_gotuimsfil!=1)
		ud_getuims();
	UD_gotuimsfil=1;

/*	-- set up the default user interface -- */
	ud_setuims(&UD_duimsdeflt,&UD_curlayout,0);		/* first of all areas */
	ud_igks(UU_TRUE);			/* initialize GKS for UG_EVENT mode */
	udi_mu_reset();			/* reset menu system */
	gsnormtran(0);				/* must do this becuase um_reset sets normtran to 1*/

/*	-- set up for auto-testing mode -- */

	p = ux_getenv("UU_AUTOTEST", UX_NPRTERRS);

	if(p == NULL)
		UD_autotest = UU_FALSE;
	else
		UD_autotest = UU_TRUE;

	uu_dexit;
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_igks(modeflag)
**       initialize gks for event and request mode.
**
**			The following devices for each input type will be initialized to event 
**			mode at this time.  All other deveices, such as tablet menus, must be
**			initialized by the appropriate subsystem when the tablet menu
**			is attached.
**
**			In either request or event mode, the choice devices one and two
**			will be initialized to run in event mode.  The other input devices
**			will only be initialized when the system is running in event
**			mode.
**
**				Input		Device	Phyical			Prompt and
**							Number	Device			Echotype
**				------------------------------------------
**				CHOICE		1		Typewriter			None
**										Keyboard
**								2		Other function 	None
**										Keys
**								3		Text Menus			None
**										Kbd input only
**								5		Icon Menus			None
**								>10	Fast Menus			None
**								21		Text Menu			None
**										Fctn Key input only
**								22		Text Menu			None
**										Mouse or Kbd input (default)
**								23		Text Menu			None
**										22 + help messages
**								24		Text Menu			None
**										22 + backlighting
**								25		Text Menu			None
**										Mouse input only
**
**				UD_LOCATOR	1		Graphic Terminal	Crosshair
**				VALUATOR		1		Keyboard				Text echo
**				PICK			1		Graphic Terminal	Pick Cursor
**				STRING		1		Keyboard				Text Echo
**
**  PARAMETERS   
**      INPUT  : 
**          modeflag = UU_TRUE for event mode, UU_FALSE for request mode
**      OUTPUT :  
**				none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

void ud_igks(modeflag)
UU_LOGICAL modeflag;					/* enter into request or event mode flag */
{

/*	-- locator initialize stuff -- */

	Glocrec locrec;

/*	-- valuator initialize stuff -- */

	static Gvalrec valrec = {UM_MINREAL, UM_MAXREAL, " "};	/* high, low value range, prompt */

/*	-- choice initialize stuff -- */

	Gchoicerec choicerec;			/* high, low value range */

/*	-- pick initialize stuff -- */

	Gpicks initpick;
	Gpickrec pickrec;

/*	-- string initialize stuff -- */

	static Gstringrec stringrec = {80, 0, ""};		/* no initial string */
	Gdspsize *dispsize;			/* display size */
	Gdrect halfrect;				/* pos after ws xform */

/*	-- Initialize GKS -- */

	uu_denter(UU_DTRC,(us,"entering ud_igks"));

/*	-- set CHOICE device 1, 2, 3, and 4 into event mode -- */
/*	-- use calls to gqdefchoice to find number of choices -- */

/* set UD_chcwin to whole screen */
/*
	if (UD_motif == 0)
	{
		scrn=UD_curlayout.curr_screen;
		areapt= &(UD_duimsdeflt.screen[scrn].areas[UD_LPRMT])
									[UD_curlayout.curr_lex_prompt_area];
		ud_halfrect(&pos,&(*areapt).posn);
		ud_devrect(&pos,&halfrect);
	}
*/
	dispsize=gqdisplaysize(UD_ksws);
	UD_chcwin.ll.x=0.; UD_chcwin.ll.y=0.;
	UD_chcwin.ur.x=(*dispsize).device.x; 
	UD_chcwin.ur.y=(*dispsize).device.y;

	choicerec.strings = &UD_chcpmtptr;
	
	choicerec.number = 128;
	gschoicemode(UD_ksws, 1, UG_REQUEST, UG_NOECHO);
	ginitchoice(UD_ksws, 1, 0, 1, &halfrect, &choicerec);
	gschoicemode(UD_ksws, 1, UG_EVENT, UG_NOECHO);

	choicerec.number = 15;
	gschoicemode(UD_ksws, 2, UG_REQUEST, UG_NOECHO);
	ginitchoice(UD_ksws, 2, 0, 1/*21*/, &halfrect, &choicerec);
	gschoicemode(UD_ksws, 2, UG_EVENT, UG_NOECHO);

	choicerec.number = 4;
	gschoicemode(UD_ksws, 3, UG_REQUEST, UG_NOECHO);
	ginitchoice(UD_ksws, 3, 0, 1/*21*/, &halfrect, &choicerec);
	gschoicemode(UD_ksws, 3, UG_EVENT, UG_NOECHO);

	choicerec.number = 484;
	gschoicemode(UD_ksws, 4, UG_REQUEST, UG_NOECHO);
	ginitchoice(UD_ksws, 4, 0, 1/*22*/, &halfrect, &choicerec);
	gschoicemode(UD_ksws, 4, UG_EVENT, UG_NOECHO);

/*	-- if running in event mode set Locator, Valuator, Pick, and
		String devices in event mode -- */

	UD_Eventm = modeflag;
	if(modeflag == UU_TRUE)
	{
		uu_dprint(UU_DTRC,(us,"ud_igks. curr_lex_prompt_area=%d",
			UD_curlayout.curr_lex_prompt_area));
		locrec.prompt = UD_locpmt;				/* locator data record */
		gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO);

		uu_dprint(UU_DTRC,(us,"ud_igks. initloc(%d,area=%g %g %g %g)",
			UD_locdev, (*areapt).posn.ll.x,(*areapt).posn.ll.y,
			(*areapt).posn.ur.x,(*areapt).posn.ur.y));
		ginitloc(UD_ksws, UD_locdev, &UD_nxhair, UD_locech, &halfrect, &locrec);
		gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO);

		valrec.prompt = UD_valpmt;
		gsvalmode(UD_ksws, 1, UG_REQUEST, UG_ECHO);
		ginitval(UD_ksws, UD_valdev, (UU_REAL) 0., UD_valech,&halfrect,&valrec);
		gsvalmode(UD_ksws, 1, UG_EVENT, UG_ECHO);

		pickrec.prompt = UD_pckpmt;
		initpick.status = UG_NOPICK;
		initpick.depth = 0;
		initpick.pickpath = NULL;
		gspickmode(UD_ksws, 1, UG_REQUEST, UG_ECHO);
		ginitpick(UD_ksws, UD_pckdev, &initpick, UD_pckech,&halfrect,&pickrec);
		gspickmode(UD_ksws, 1, UG_EVENT, UG_ECHO);

		stringrec.prompt = UD_strpmt;
		gsstringmode(UD_ksws, 1, UG_REQUEST, UG_ECHO);
		ginitstring(UD_ksws, UD_strdev, "", UD_strech, &halfrect, &stringrec);
		gsstringmode(UD_ksws, 1, UG_EVENT, UG_ECHO);

		gsstringmode(UD_ksws, 2, UG_REQUEST, UG_ECHO);
		ginitstring(UD_ksws, 2, "", UD_strech, &halfrect, &stringrec);
	}
	uu_dexit;
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_ogks()
**      open gks for the specified workstation(s)
**
**  PARAMETERS   
**      INPUT  : 	none
**      OUTPUT :  none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

void ud_ogks()
{
	Dphdev pun;
	char *p,*ux_getenv();
	char msg[256];
	int i;
	UU_LOGICAL found;
	int ncolors;
	int gemergencyclosegks();
	static FILE *gkstrc;
	FILE *uu_opentrc();
	char lhost[80];

/*	-- initialize GKS -- */

	uu_denter(UU_DTRC,(us,"entering ud_odas"));

	UD_wsptr = 0;								/* next free workstation pointer */

/*	-- get file pointer of trc file, if not yet opened, this call opens it. -- */
	gkstrc = uu_opentrc();

/*	-- open digs, use mpe's trc file as digs error file -- */
	i = gopengks(64,gkstrc);
	if( i != NCL_NO_ERROR ) exit(999);

/* set up emergency close function */
	uu_emergencyclose( gemergencyclosegks );

/* initialize name servers for icon menus */
	uu_nserv_init(6,UD_start_menu_num-1,UU_CHOICE_NM);

/*  initialize workstation  */

	p=ux_getenv("ws", UX_NPRTERRS);
	strcpy(lhost,p);
	ul_to_upper(lhost);

	found = UU_FALSE;
	for(i=0; i<UD_gkstlen; i++)
	{
		if (strcmp(p, UD_gkstable[i].name)==0)
		{
			(*UD_gkstable[i].ddentry)();
			found = UU_TRUE;
		}
	}  
		
	if(found == UU_FALSE)
	{
		sprintf(msg, " Invalid workstation shell variable = %s\n", p);
		ud_printmsg(msg);
		exit(12);
	}
		
/*	-- set UD_CONS to workstation just activated -- */

	pun.type = 1;
	pun.ws = UD_ksws;
	ud_dsphun(UD_CONS, pun);
}
