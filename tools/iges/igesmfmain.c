#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**    NAME         :  igesmfmain.c
**       CONTAINS:
**             iges_mfmain
**					iges_mfapp_quitCB
**					static void iges_DeselAllCB
**					static void iges_SelAllCB
**					static void iges_savoptCB
**					static void iges_mfrunCB
**					static void iges_mfform_quitCB
**					static void iges_ckCB
**					static void iges_mbrowseCB
**					static void iges_FileSCB
**					static void iges_chcCB
**					static void iges_optionUCB
**					Widget iges_mfcreate_action
**					uig_mfget_range(num,range)
**					uig_get_label_type
**					uig_mfget_unifil(filename)
**					uig_get_ptname(filename)
**					uig_get_outname(filename)
**					iges_enable_out()
**					iges_enable_in()
**             iges_file(widget, client_data, call_data)
**             iges_help(widget, client_data, call_data)
**             uig_name_modal(OptionFrm, client_data, call_data)
**             uig_attributes(OptionFrm, client_data, call_data)
**             uig_change_prefix(option_but, client_data, call_data)
**             uig_destroy_names(widget, client_data, call_data)
**             uig_name_configCB(widget, client_data, call_data)
**             uig_translate_filter_form(OptionFrm, client_data, call_data)
**             set_entity(widget, client_data, call_data)
**             filter_entities(widget, client_data, call_data)
**             all_on(widget, client_data, call_data)
**             all_off(widget, client_data, call_data)
**             sfcvCB(widget, client_data, call_data)
**			   uig_startunmatchCB(widget, client_data, call_data)
**					static int exchange(a, b)
**					uig_bubble_sort(array, size)
**    COPYRIGHT 1989 (c) MILLS DATA SYSTEMS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**			igesmfmain.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:43
*********************************************************************/
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeBG.h>
#include <Xm/DialogS.h>
#include <Xm/MainW.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/MwmUtil.h>
#include <Xm/Protocols.h>
#include <Xm/SashP.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Frame.h>
#include <X11/cursorfont.h>

#include "mdrel.h"
#include "tiges.h"
#include "usignal.h"
#include "ustdio.h"
#include "ulist.h"
#include "xenv1.h"
#define MFIGPRM
#include "tigmf.h"
#undef MFIGPRM
#include "nclver.h"
#include "xfsys1.h"
#include "tigglobal.h"

#if UU_COMP==UU_VAXVMS
#include iodef
#include descrip
int UIG_tty_channel;    /* set by SYS$ASSIGN */
#endif


extern int NAUTIGES, NAUT502;
extern int tig_num_range, tig_range[IG_LAYER][2];
extern UX_pathname iges_fname, iges_tmpfile;
static int UIG_start_unmatch_cba[2] = {0,1};
static int UIG_start_unmatch_hld;
static int UIG_matchlev_cba[5] = {0,1,2,3,4};
static int UIG_matchlevel_hld = 0;
extern int UIG_use_sub;
extern int UIG_concat_sub;

int output_units;

int MAX_PARA_REC=300000;
int UG_def_line_wt = 0;
Cursor normal_cursor, wait_cursor;

static char input_ptname[UX_MAX_PATH_LEN] = "";
static Widget CrtU_form, cformMenu[6], range_form,label_opt[18];
static Widget Main_Window, menuBar, menu1, menu2, cform_item[11];
static Widget cform_choice[17], form_in, form_out;
static Widget toggle1, name_modal_button;
static Widget UIG_toggle_use_subscript, UIG_toggle_concat_subscript;
static Widget UIG_toggle_reinit_lab;
static Widget UIG_toggle_nodups;
static Widget UIG_toggle_unmatch_sec;
static Widget UIG_toggle_reg_match;
static Widget UIG_toggle_color_iges;
static Widget UIG_match_attributes;
static Widget UIG_toggle_sfedge;
static int formEof;
static int Nps=0, ONps=0;
static int Byrange=0;
static int sav_label_type = 1;
static int iges_in = 1;
static int iges_out = 0;
static int All_layer[IG_LAYER];
/*static int All_num;*/

typedef struct
{
   int type,fld,chc;
} Pstruct;
/*
.....Increasing arrays to 24 from 20. JLS 1/19/99
*/
Pstruct *pStruct[25];
Pstruct *OpStruct[25];

/*
..... Name Modal data structures
..... These are global declarations since they will need to
..... be accessed in multiple files (ex. tiomain, tigconv, here, etc.)
*/
Widget lab_flag_widget[11], geo_lab_widget[11];
/*
.....These variable are now defined only in tigmodals.c for
.....all platforms instead of two different definitions in two 
.....files.
.....Himani
*/
int temp_lab_flag[11];
extern int iln, ipt, ipl, ici, icn, icv, isf, isg, ipn, ixx;

/*
..... Filter Entity data structures
*/
static int UIG_splitccrv_wrk;
Widget entity_widget[37];
Widget sfcvopts_widget;
/*
.....These variable are now defined only in tigmodals.c for
.....all platforms instead of two different definitions in two
.....files.
.....Himani
*/
static void iges_mfform_quitCB(), iges_mfapp_quitCB(),
			iges_savoptCB(), iges_mfrunCB();
	
static ActionAreaItem actionList1[] =
{
	{"RUN", iges_mfrunCB, (XtPointer)1},
	{"EXIT", iges_mfapp_quitCB, (XtPointer)2}
};
static ActionAreaItem actionList2[] =
{
	{"OK", iges_savoptCB, (XtPointer)1},
	{"CANCEL", iges_mfform_quitCB, (XtPointer)2}
};

static Widget m_process_bar;
static Widget m_process_label1, m_process_label2;
static GC m_process_gc;
static int m_percent = 0;
void iges_mf_xflush_win();
/*********************************************************************
**    I_FUNCTION     :iges_mfapp_quitCB
**       Callback for "Exit" button on the main window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_mfapp_quitCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int i;
	char tmp[52], msg[UX_MAX_PATH_LEN*2];
	for (i=0;i<Nps;i++) uu_free(pStruct[i]);
	if (iges_fname[0]!='\0')
	{
		close(iges_fd);
		if (ux_delete0(iges_tmpfile))
		{
			ul_short_filename(iges_fname,tmp,50);
			sprintf (msg, "can't delete %s corresponding %s", iges_tmpfile,
					tmp);
			printf(msg);
		}
	}
	exit();
}

/*******************************************************************
**   I_FUNCTION : iges_file(widget, client_data, call_data)
**              Callback function "File" menu
**
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = menu item picked
**          call_data   = Motif callback structure.
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
static void iges_file(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	int pick_no = (int) client_data;
   if (pick_no == 0)
	{
		int i;
   	for (i=0;i<Nps;i++) uu_free(pStruct[i]);
   	exit();
	}
}

/*******************************************************************
**   I_FUNCTION : iges_help(widget, client_data, call_data)
**              Callback function "Help" menu
**
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = menu item picked
**          call_data   = Motif callback structure.
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
static void iges_help(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	char *p, buf[256],*ux_getenv();
   int pick_no = (int) client_data;
   if (pick_no == 0)
   {
   	int len;
   	p = ux_getenv("NCL_IGES_HELP");
   	if (p != 0)
   	{
      	len = strlen(p);
      	if (len==0)
      	{
         	sprintf(buf,"cmd.exe /K %s", p);
         	system(buf);
         	return;
      	}
      	system(p);
   	}

   }
   else if (pick_no == 1)
   {
      char msg[500], msg1[256], msg2[256];
      iges_getabout_str(msg1, msg2);
      sprintf(msg, "\n%s\n%s\n", msg1, msg2);
      uig_mfmsg_box(NULL, "About NCL/IGES",msg, 0);
	}
}

/*********************************************************************
**    I_FUNCTION     :iges_DeselAllCB(widget, client_data, call_data)
**       Callback for "Clear All" button on the options window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_DeselAllCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	Widget *list = (Widget *) client_data;
	XmListDeselectAllItems(*list);
}

/*********************************************************************
**    I_FUNCTION     :iges_SelAllCB(widget, client_data, call_data)
**       Callback for "Select All" button on the options window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_SelAllCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int i, num;

	Widget list = (Widget ) client_data;
	XtVaGetValues((Widget)list, XmNitemCount, &num, NULL); 
	for (i=1; i<num+1; i++)
		XmListSelectPos(list, i, False);
}

/*********************************************************************
**    I_FUNCTION     :iges_savoptCB(widget, client_data, call_data)
**       Callback for "OK" button on the options window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_savoptCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int len,count=0,i,status,index;
	char *text,toler[10],in_mm[10];

	uig_mfget_range(&tig_num_range, tig_range);
	UIG_use_sub = XmToggleButtonGetState(UIG_toggle_use_subscript); 
	UIG_concat_sub = XmToggleButtonGetState(UIG_toggle_concat_subscript); 
	label_type = sav_label_type;

	if (sav_label_type == 2)
	{
		label_type = 3;
		if (UIG_use_sub==0 && UIG_concat_sub==0)
		{
			if (UIG_max_lab==6) label_type = 7;
		}
		else if (UIG_use_sub==1 && UIG_concat_sub==0)
		{
			label_type = 4;
		}
		else if (UIG_concat_sub==1)
		{
			if (UIG_max_lab==1)
				label_type = 5;
			else
				label_type = 9;
		}
	}
	else if (sav_label_type == 3)
	{
		label_type = 6;
		if (UIG_use_sub==0) label_type = 10;
	}
	else if (sav_label_type == 4)
	{
/*
.....If label type is 4, set to 8 and get secondary unibase
*/
		label_type = 8;
		text = XmTextGetString(label_opt[1]);
		status = uig_open_secondary(text,&index);
		if (status == UU_SUCCESS)
		{
			ur_saveu_active(2);
			if (index <= 9)
				uig_update_surface_prim();
			ur_saveu_active(1);
			text = XmTextGetString(label_opt[5]);
			len = strlen(text);
/*
.....Parse the string from the Matching tolerance field to get the 
..... tolerance value and the units
*/
			count =0;
			toler[0]='\0';
			in_mm[0]='\0';
			for (i = 0; i < len; i++)
			{
				if (text[i] == '.')
				{
					if(count == 0) count = 1;
					else  
					{
						uig_mfmsg_box(NULL, "ERROR", 
						"Text input not in format required by tolerance.");
						goto use_mod;
					}
				}
				else
				{
					if (isalpha(text[i]))
			   		{
						strncpy(toler,text,i);
						toler[i]='\0';
						sscanf((text+i), "%s", in_mm);
						if(strlen(in_mm) ==2)
						{
							if((in_mm[0] == 'm' || in_mm[0] == 'M') && 
								(in_mm[1] == 'm'||in_mm[1] == 'M'))
								UIG_units_factor =25.4;
							else
								if((in_mm[0] == 'i' || in_mm[0] == 'I') && 
								(in_mm[1] == 'n'||in_mm[1] == 'N'))
									UIG_units_factor =1;
							else 
							{
								uig_mfmsg_box(NULL, "ERROR",
								"Text input not in format required by tolerance.");
								goto use_mod;
							}
							break;
						}
						else 
						{
							uig_mfmsg_box(NULL, "ERROR",
							"Text input not in format required by tolerance.");
							goto use_mod;
						}
			   		}
					else if (isdigit(text[i]))
					{
						if(i == len-1)
							strcpy(toler,text);
					}
					else 
					{
						uig_mfmsg_box(NULL, "ERROR",
						"Text input not in format required by tolerance.");
						goto use_mod;
					}
				}							
			}				
			sscanf(toler,"%lf", &UIG_match_tol_disp);
			UIG_match_tol = UIG_match_tol_disp / UIG_units_factor;
			if (UIG_match_tol < .0001)
				UIG_match_tol = .0001;
use_mod:;
			if (UIG_units_factor == 1)
				UIG_match_tol = UIG_match_tol_disp;
			if (UIG_units_factor == 25.4)
				UIG_match_tol = UIG_match_tol_disp / UIG_units_factor;

			UIG_matchlevel = UIG_matchlevel_hld;
			UIG_start_unmatch  = UIG_start_unmatch_hld;
         if (XmToggleButtonGetState(UIG_toggle_unmatch_sec))
            UIG_unmatch_sec = UU_TRUE;
         else
            UIG_unmatch_sec = UU_FALSE;
		 if (XmToggleButtonGetState(UIG_toggle_reg_match))
			UIG_regressive = UU_TRUE;
		 else
			UIG_regressive = UU_FALSE;
		}
		else
			sav_label_type = label_type =1;
	}
/*
.....Label reinitialization.
*/
	if (XmToggleButtonGetState(UIG_toggle_reinit_lab)) 
		UIG_reinit_lab = UU_TRUE;
	else
		UIG_reinit_lab = UU_FALSE;
/*
.....No duplicate geometry
*/
	if (XmToggleButtonGetState(UIG_toggle_nodups)) 
		UIG_nodups = UU_TRUE;
	else
		UIG_nodups = UU_FALSE;
/*
.....Iges Color entity
*/
   if (XmToggleButtonGetState(UIG_toggle_color_iges))
      UIG_color_iges = 1;
   else
      UIG_color_iges = 0;

/*
.....Added for importing surfaces as shaded or unshaded. JLS 7/29/99
*/
	if (XmToggleButtonGetState(toggle1)) 
		shade_set = UU_TRUE;
	else
		shade_set = UU_FALSE;
/*
.....Display surface edges
*/
	if (XmToggleButtonGetState(UIG_toggle_sfedge)) 
		UIG_srf_edge = UU_TRUE;
	else
		UIG_srf_edge = UU_FALSE;
/*
.....close form
*/
	for (i=0;i<ONps;i++) uu_free(OpStruct[i]);
	formEof = True;
	XtUnmanageChild((Widget)client_data); 
	XtDestroyWidget((Widget)client_data); 
	XmUpdateDisplay((Widget)client_data);
}

/*********************************************************************
**    I_FUNCTION     : iges_mfrunCB(widget, client_data, call_data)
**       Callback for "RUN" button on the main window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_mfrunCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int status;
	char *text;
	UX_pathname filename,fname,dir,tmp;
	char msg[200];
	iges_in = XmToggleButtonGetState(mform_item[2]);
	iges_out = XmToggleButtonGetState(mform_item[12]);

/*
..... Make sure that the subscripts are reset to 0 for each
..... translation. These are global variables found in tigmapg.c.
..... These are the things that are actually used for subscripting
..... not the UM_labelmdl.subscr[].
*/
	iln = ipt = ipl = ici = icn = icv = isf = isg = ipn = ixx = 0;	
	
	dbyte = pbyte = 0;
	if (iges_in==1)
	{
		text = XmTextGetString(mform_item[4]);
		if(text[0]!='\0')
			strcpy(filename, text);
		XtFree(text);
/*
.....check if user entered filename
*/

		ul_break_fname(filename, dir, fname);
		if (strlen(fname)==0)
		{
			uig_mfmsg_box(NULL, "ERROR", "No input IGES data file specified!");
			return;
		}
		status = 0;
/*
.....added ext '.igs'
.....Nope, doesn't always have to be .igs  JLS 7/23/99
.....The file name should be what the user entered.

		i = strcspn (fname, ".");
		strcpy(&fname[i], ".igs");
*/
		strcpy(fname , filename);
/*
.....The above change makes the following lines of code unnecessary
.....JLS 8/5/99
#if UU_COMP == UU_VAXVMS
		strcat (dir, fname);
#else
		if (strlen(dir)!=0)
			strcat (dir,"/");
		strcat (dir, fname);
#endif
		strcpy(fname, dir);
*/
		if (strcmp(filename, iges_fname)!=0)
		{
			if (iges_fname[0]!='\0')
			{
				close(iges_fd);
/*
				if (ux_delete0("iges.tmp"))
				{
					ul_short_filename(iges_fname,tmp,40);
					sprintf (msg, "can't delete iges.tmp corresponding %s", tmp);
					uig_mfmsg_box(NULL, "Error!", msg);
				}
*/
				if (ux_delete0(iges_tmpfile))
				{
					ul_short_filename(iges_fname,tmp,50);
					sprintf (msg, "can't delete %s corresponding %s", iges_tmpfile, tmp);
					uig_mfmsg_box(NULL, "Error!", msg);
				}
			}
			strcpy(tmp, filename);
			status = uig_unb_opnfil(filename);
			if ( status == 0)
				strcpy(iges_fname, tmp);
			else
				iges_fname[0] = '\0';
		}
		if (status!=0) return;
		no_of_views = 0;
		current_dir_number = 0;
		number_of_masters = 0;
		sequence_no = 0;
		uig_in_convert();  
		t_num = 0;
	}
	else if (iges_out==1)
	{
		uio_main();
	}
	else
	{
		uig_mfmsg_box(NULL, "No Input", "You need either pick IGES IN or IGES OUT");
	}
}

/*********************************************************************
**    I_FUNCTION     :iges_mfform_quitCB(widget, client_data, call_data)
**       Callback for "Cancel" button on the options window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_mfform_quitCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int i;
/*
.....close form
*/
	for (i=0;i<ONps;i++) uu_free(OpStruct[i]);
	formEof = True;
	XtUnmanageChild((Widget)client_data); 
	XtDestroyWidget((Widget)client_data);
	XmUpdateDisplay((Widget)client_data);
}

/*********************************************************************
**    I_FUNCTION     : iges_ckCB(widget, client_data, call_data)
**       Callback for radio box on the main window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_ckCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int opt = (int)client_data;
	if (opt==0)
	{
		iges_in = XmToggleButtonGetState(mform_item[2]);
		if (iges_in==1)
		{
			XmToggleButtonSetState(mform_item[12], 0, False);
		}
		else
			XmToggleButtonSetState(mform_item[12], 1, False);
		iges_out = XmToggleButtonGetState(mform_item[12]);
	}
	else
	{
		iges_out = XmToggleButtonGetState(mform_item[12]);
		if (iges_out==1)
			XmToggleButtonSetState(mform_item[2], 0, False);
		else
			XmToggleButtonSetState(mform_item[2], 1, False);
		iges_in = XmToggleButtonGetState(mform_item[2]);
	}
	if (iges_in==1)
	{
		iges_enable_in();
	}
	else
	{
		iges_enable_out();
	}
}

/***********************************************************************
c
c   FUNCTION:  drawCB(widget, client_data, call_data)
c              Callback function expose event of processbar.
c					This function draw a process control
c
c   INPUT:  
c			t	
c
c   OUTPUT :  
c	 RETURN:    None
c
c***********************************************************************/
void drawCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;
{
	GC gc_ptr;
	Display *display;
	Dimension width, height, cx, cy;
	Position x, y;
	Colormap cmap;
	XmString lstr;
	char perc_str[10];
	XColor current_color;
	int i, x1, x2, y1, y2, box, boxwid, stat, hgt, wid;

	if (m_process_gc==NULL)
		return;
	display = XtDisplay(m_process_bar);

	gc_ptr = m_process_gc;
	XtVaGetValues(widget, XmNwidth, &width,
				XmNheight, &height, NULL);

	XtVaGetValues(widget, XmNcolormap, &cmap, NULL);
	x = 0; y = height*0.15;
	cx = width; cy = height*0.7;
/*
......don't update the back ground if not needed
*/
/*	if (m_update_back) */
	{
		current_color.red = 255*100;
		current_color.green = 255*100;
		current_color.blue = 255*100;
		stat = XAllocColor(display, cmap, &current_color);

		XSetLineAttributes(display,
								gc_ptr,
								2, LineSolid,
								CapProjecting, JoinBevel);
		XSetForeground(display,
							gc_ptr,
							current_color.pixel);
/*
......leave some spaces for height
*/
		XFillRectangle(display, XtWindow(widget), gc_ptr, 0, height*0.15,
									width, height*0.7);
		x1 = x; y1= y;
		x2 = x+cx ; y2 = y;
		XDrawLine(display, XtWindow(widget), gc_ptr, x1,y1,x2,y2);
		x1 = x; y1= y+cy;
		x2 = x; y2 = y;
		XDrawLine(display, XtWindow(widget), gc_ptr, x1,y1,x2,y2);
	
		current_color.red = 255*200;
		current_color.green = 255*200;
		current_color.blue = 255*200;
		XAllocColor(display, cmap, &current_color);
		XSetForeground(display, 
						gc_ptr,
						current_color.pixel);
		x1 = x+cx; y1 = y;
		y2= y + cy; x2 = x+cx;
		XDrawLine(display, XtWindow(widget), gc_ptr, x1,y1,x2,y2);
		x1 = x2; y1 = y2;
		y2= y+ cy; x2 = x;
		XDrawLine(display, XtWindow(widget), gc_ptr, x1,y1,x2,y2);
/*		m_update_back = 0; */
	}
	current_color.red = 0;
	current_color.green = 0;
	current_color.blue = 255*255;
	XAllocColor(display, cmap, &current_color);
	XSetForeground(display, 
						gc_ptr,
						current_color.pixel);
	box = (m_percent/5)+0.5;
	if (box<1) box = 1;
	boxwid = cx * 0.05 + 1;
	for (i=0; i<box; i++)
	{
		x1 = x - 1 + i*boxwid;
		y1 = y + 1;
		wid = boxwid;
		hgt = cy - 2;
		XFillRectangle(display, XtWindow(widget),
						gc_ptr,
						x1,y1,wid,hgt);
	}
	XFlush(display);
/*
......update the label
*/
	if (m_percent<0)
		strcpy(perc_str, "Processing ...");
	else
		sprintf(perc_str, "%d%% Completed ...", m_percent);
	lstr = XmStringCreateSimple(perc_str);
	XtVaSetValues(m_process_label2,
								XmNlabelString, lstr, NULL);
	XmStringFree(lstr);
	iges_mf_xflush_win();
}

/*********************************************************************
**    I_FUNCTION     :  iges_mbrowseCB(widget, client_data, call_data)
**       Callback for browse button on the main window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_mbrowseCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	UX_pathname filename;
	int len;
	static int flag1=0, flag2=0, flag3=0, flag4=0;
	int i = (int)client_data;
	filename[0] = '\0';
	if (i==5)
	{
		if (flag1==0)
		{
			flag1 = 1;
			uig_mf_filename(widget, "Input IGES Data File", "*.igs", filename, &len);
			flag1 = 0;
		}
	}
	else if (i==10)
	{
		if (flag4==0)
		{
			flag4 = 1;
			uig_mf_filename(widget, "Output Unibase File", "*.u", filename, &len);
			flag4 = 0;
		}
	}
	else if (i==15)
	{
		if (flag2==0)
		{
			flag2 = 1;
			uig_mf_filename(widget, "Input Unibase File", "*.u", filename, &len);
			flag2 = 0;
		}
	}
	else
	{
		if (flag3 == 0)
		{
			flag3 = 1;
			uig_mf_filename(widget, "Output IGES Data File", "*.igs", filename, &len);
			flag3 = 0;
		}
	}
	if (len!=0)
	{
		XmTextSetString(mform_item[i-1], filename);
		XmTextSetInsertionPosition(mform_item[i-1], len);
		XFlush(XtDisplay(TOOL_Parent));
	}
}

/*********************************************************************
**    I_FUNCTION     :  iges_labelCB(widget, client_data, call_data)
**       Callback for browse button in the option window for
**       using a unibase for labeling purposes.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_labelCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	UX_pathname filename;
	int len;

	filename[0] = '\0';
	uig_mf_filename(NULL, "Label Unibase File", "*.u", filename, &len);
	if (len!=0)
	{
/*
		if (status == UU_SUCCESS)
			ur_saveu_active(2);
*/
		XmTextSetString(label_opt[1], filename);
		XmTextSetInsertionPosition(label_opt[1], len);
		XFlush(XtDisplay(TOOL_Parent));
	}
}
/*********************************************************************
**    I_FUNCTION     :  iges_FileSCB(widget, client_data, call_data)
**       Callback for "File Summary" button on the main window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_FileSCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	UX_pathname filename,fname,dir,tmp;
	char *text, msg[200];
	int status;
	Widget text_widget = (Widget)client_data;
	if(text_widget!=(Widget)NULL)
	text = XmTextGetString(text_widget);
	if(text[0]!='\0')
		strcpy(filename, text);
	else
		filename[0] = '\0';
	XtFree(text);
/*
.....check if user entered filename
*/
	ul_break_fname(filename, dir, fname);
	if (strlen(fname)==0)
	{
		uig_mfmsg_box(NULL, "ERROR", "No input IGES data file specified!");
		return;
	}
/*
.....added ext '.igs'
	i = strcspn (fname, ".");
	strcpy(&fname[i], ".igs");
*/
	strcpy(fname,filename);
/*
.....The previous change makes these lines no longer needed. JLS 8/5/99
	if (strlen(dir)!=0)
#if UU_COMP == UU_VAXVMS
	strcat (dir, fname);
#else
	if (strlen(dir)!=0)
		strcat (dir,"/");
	strcat (dir, fname);
#endif
	strcpy(filename, dir);
*/
	status = 0;
	if (strcmp(filename, iges_fname)!=0)
	{
		if (iges_fname[0]!='\0')
		{
			close(iges_fd);
/*			if (ux_delete0("iges.tmp"))
			{
				ul_short_filename(iges_fname,tmp,40);
				sprintf (msg, "can't delete iges.tmp corresponding %s", tmp);
				uig_mfmsg_box(NULL, "Error!", msg);
			}
*/
			if (ux_delete0(iges_tmpfile))
			{
				ul_short_filename(iges_fname,tmp,50);
				sprintf (msg, "can't delete %s corresponding %s", iges_tmpfile, tmp);
				uig_mfmsg_box(NULL, "Error!", msg);
			}
		}
		strcpy(tmp, filename);
		dbyte = pbyte = 0;
		status = uig_unb_opnfil(filename);
		if ( status == 0)
			strcpy(iges_fname, tmp);
		else
			iges_fname[0] = '\0';
	}
/*
.....Create window to display file summary
*/
	if ( status == 0)
		uig_summary(); 
}

/*********************************************************************
**    I_FUNCTION     :  iges_chcCB(widget, client_data, call_data)
**       Callback for choice button on the main window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_chcCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int state;
	Pstruct *ps=(Pstruct *)client_data;
/*
.....call from main form
*/
	if (ps->type == 0)
	{
		if (ps->chc==0)
			UIG_drawing_only = UU_TRUE;
		else
			UIG_drawing_only = UU_FALSE;
	}
	else
/*
.....call from Create Unibase Form
*/
	{
		if (ps->fld==1)
/*
.....Conversion Option
*/
		{
			if (ps->chc==0)
			{
				XtSetSensitive(range_form, False);
				UIG_conv_opt =0;
				Byrange = 0;
				return;
			}
			else
			{
				XtSetSensitive(range_form, True);
				UIG_conv_opt =1;
				Byrange = 1;
			}
		}
		else
/*
......Label Option
*/
		{
			sav_label_type = ps->chc+1;
			UIG_lab_opt=ps->chc;
/*
.....If label option is equal to 4 activate the
.....text field and browse button for unibase labels
*/
			state = False;
			if (sav_label_type == 4) state = True;
			XtSetSensitive(label_opt[0],state);
			XtSetSensitive(label_opt[1],state);
			XtSetSensitive(label_opt[2],state);
			
			XtSetSensitive(UIG_match_attributes,state);
			XtSetSensitive(cform_item[7],state);
			XtSetSensitive(cform_item[8],state);
			XtSetSensitive(label_opt[14],state);
			XtSetSensitive(label_opt[15],state);
			/*XtSetSensitive(label_opt[13],state);*/
			if(UIG_nodups)state = True;
			XtSetSensitive(label_opt[4],state);
			XtSetSensitive(label_opt[5],state);
			state = False;
			if (sav_label_type == 2 || sav_label_type == 3) state = True;
			XtSetSensitive(label_opt[8],state);
			XtSetSensitive(label_opt[9],state);
			XtSetSensitive(cform_item[6],state);
		}
	}
	return;
}
/*********************************************************************
**    I_FUNCTION     :  uig_startunmatchCB(widget, client_data, call_data)
**       Callback for start unmatch button in the options window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void uig_startunmatchCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int *iptr=(int *)client_data;

	UIG_start_unmatch_hld = *iptr;

	return;
}
/*********************************************************************
**    I_FUNCTION     :  uig_matchlevCB(widget, client_data, call_data)
**       Callback for maxlab button in the options window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void uig_matchlevCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int *iptr=(int *)client_data;

	UIG_matchlevel_hld = *iptr;

	return;
}
/*********************************************************************
**    I_FUNCTION     :  uig_sfedgeCB(widget, client_data, call_data)
**       Callback for Display Surface Edges checkbox.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void uig_sfedgeCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int state;
	XmToggleButtonCallbackStruct *toggle_data = 
			(XmToggleButtonCallbackStruct *) call_data;
	state = toggle_data->set;
	if (state)
		UIG_srf_edge = UU_TRUE;
	else
		UIG_srf_edge = UU_FALSE;
	XtSetSensitive(cform_item[10],state);
	return;
}

/*********************************************************************
**    I_FUNCTION     :  uig_edgecolorCB(widget, client_data, call_data)
**       Callback for maxlab button in the options window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void uig_edgecolorCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int *iptr=(int *)client_data;

	UIG_edge_color = *iptr - 1;
	if (UIG_edge_color == -1)
		UIG_edge_color = 64;
	return;
}

/*********************************************************************
**    I_FUNCTION     :  iges_maxlabCB(widget, client_data, call_data)
**       Callback for maxlab button in the options window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_maxlabCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	Pstruct *ps=(Pstruct *)client_data;

	UIG_max_lab = ps->chc;

	return;
}
/*********************************************************************
**    I_FUNCTION     :  iges_optionUCB
**       Callback for "options" button on the main window. it will
**			display an option window.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_optionUCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	struct global_rec gblk;
	XmString lstr, xmstring;
	struct dir_rec dblk;
	UX_pathname fname,dir,filename,tmp;
	char *cc, *text;
	XEvent x_event;
	char layer_str[IG_LAYER][5], msg[200];
	char unit[3],*p,*ux_getenv();
	Widget pane, winid, iges_mfcreate_action(),range_frame;
	Widget board, frame, filter_entities_but;
	int n,i,j,k,rows,cols,status,state,ifl,flag, color;
	Arg args[20];
	extern void uig_name_modal(), uig_translate_filter_form();
	void uig_attributes();
	extern void no_dups();

	UIG_matchlevel_hld = UIG_matchlevel;
	UIG_start_unmatch_hld = UIG_start_unmatch;
/*
......create another form
*/
	n = 0;
	ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
				MWM_DECOR_MINIMIZE;
	XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
	ifl = MWM_FUNC_ALL | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE |
				MWM_FUNC_CLOSE;
	XtSetArg(args[n],XmNmwmFunctions,ifl); n++;

	CrtU_form = XtCreatePopupShell("iges_option",xmDialogShellWidgetClass,
						TOOL_Parent, args,n);
	if (CrtU_form == NULL) return;
	XtVaSetValues(CrtU_form, XmNtitle, "IGES IN OPTIONS", NULL);
/*
.....Create a paned window to hold the form &
.....Action Area
*/
	pane = XtVaCreateWidget("option_form",xmPanedWindowWidgetClass, CrtU_form,
				XmNsashWidth,1,
				XmNsashHeight,1,
				NULL);
/*
.....Create a Form widget 
*/
	n = 0;
	rows = 16;
	cols = 26;
	XtSetArg(args[n],XmNfractionBase,rows*cols); n++;
	winid = XtCreateWidget("options", xmFormWidgetClass,pane,args,n);
	if (winid == NULL) return;
/*
.....first row
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	
	cformMenu[0] = (Widget)XmCreatePulldownMenu(winid, "Conversion", NULL,0);
/*
...........Create an Option button
*/
	lstr = XmStringCreateSimple("Conversion Option:");
	XtSetArg(args[n], XmNsubMenuId, cformMenu[0]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	cform_item[0] = (Widget)XmCreateOptionMenu(winid,
		"Conversion Options:", args, n);
	XmStringFree(lstr);
	cform_choice[0] = XtVaCreateManagedWidget("All Entities",
							xmPushButtonGadgetClass,  cformMenu[0], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 1;
	OpStruct[ONps]->chc = 0;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[0], XmNactivateCallback,
			(XtCallbackProc)iges_chcCB, (XtPointer)OpStruct[ONps++]); 
	cform_choice[1] = XtVaCreateManagedWidget("By Layers",
							xmPushButtonGadgetClass,  cformMenu[0], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 1;
	OpStruct[ONps]->chc = 1;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[1],  XmNactivateCallback,
			(XtCallbackProc)iges_chcCB, (XtPointer)OpStruct[ONps++]); 
/*
...........Set the default choice
*/

	XtVaSetValues(cformMenu[0] , XmNmenuHistory,
					cform_choice[UIG_conv_opt], NULL);

	XtManageChild(cform_item[0]);

/*
..... Filter entities button	
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 21*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 25*rows); n++;

   filter_entities_but = XmCreatePushButton(winid,"Filter Entities", args, n);
   XtAddCallback(filter_entities_but, XmNactivateCallback, \
         uig_translate_filter_form,NULL);
   XtManageChild(filter_entities_but);

/*
.....second row
*/
/*
.....create a frame for layer range
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 6*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20*rows); n++;

	range_frame = XtCreateManagedWidget("layer", xmFrameWidgetClass, winid,
								args,n);
	n = 0;
	range_form = XtCreateManagedWidget("range_form", xmFormWidgetClass,
							range_frame, args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 20); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0); n++;
	cform_item[1] = XtCreateManagedWidget("Layer Range:          ", xmLabelWidgetClass,
					range_form, args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 20); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 90); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, cform_item[1]); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 80); n++;
	XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
	XtSetArg(args[n], XmNselectionPolicy, XmMULTIPLE_SELECT); n++;

	
	cform_item[2] = (Widget)XmCreateScrolledList(range_form, "list",args, n);
/*
.....get layer range and add in list
*/
	filename[0] = '\0';
	text = XmTextGetString(mform_item[4]);
	if(text[0]!='\0')
		strcpy(filename, text);
	XtFree(text);
	flag = 0;
/*
.....check if user entered filename
*/
	ul_break_fname(filename, dir, fname);
	if (strlen(fname)==0)
	{
		uig_mfmsg_box(NULL, "WARNING", "No input IGES data file specified! The option dialog may beempty");
	}
	status = 0;
/*
	i = strcspn (fname, ".");
	strcpy(&fname[i], ".igs");
*/
	strcpy(fname,filename);
/*
.....The previous change makes these lines no longer needed. JLS 8/5/99
#if UU_COMP == UU_VAXVMS
	strcat (dir, fname);
#else
	if (strlen(dir)!=0)
		strcat (dir,"/");
	strcat (dir, fname);
#endif
	strcpy(filename, dir);
*/
	if (strcmp(filename, iges_fname)!=0)
	{
		flag = 1;
		if (iges_fname[0]!='\0')
		{
			close(iges_fd);
/*			if (ux_delete0("iges.tmp"))
			{
				ul_short_filename(iges_fname,tmp,40);
				sprintf (msg, "can't delete iges.tmp corresponding %s", tmp);
				uig_mfmsg_box(NULL, "Error!", msg);
			}
*/
			if (ux_delete0(iges_tmpfile))
			{
				ul_short_filename(iges_fname,tmp,50);
				sprintf (msg, "can't delete %s corresponding %s", iges_tmpfile, tmp);
				uig_mfmsg_box(NULL, "Error!", msg);
			}
		}
		strcpy(tmp, filename);
		dbyte = pbyte = 0;
		status = uig_unb_opnfil(filename);
		if ( status == 0)
			strcpy(iges_fname, tmp);
		else
			iges_fname[0] = '\0';
	}
		
	if (iges_fname[0]!='\0')
	{
/*
...... Display the tolerance and the units
*/
		uig_get_global(&gblk);
		UIG_units_factor = 1.;
		if (gblk.units == 2 ) 
		{
			if(UIG_units_factor==1)
				UIG_match_tol_disp =UIG_match_tol * 25.4;
			UIG_units_factor = 25.4;
			strcpy(unit, "mm");
		}
		else if (gblk.units == 1) 
		{
			if(UIG_units_factor==25.4)
				UIG_match_tol_disp =UIG_match_tol ;
			UIG_units_factor = 1;
			strcpy(unit, "in");
		}
		if (UIG_match_tol < .0001)
		{
			if ((p=ux_getenv("UIG_MATCH_TOL")))
				UIG_match_tol = atof(p);
			else
				UIG_match_tol = .001;
		}
		UIG_match_tol_disp = UIG_match_tol * UIG_units_factor;
		uig_terminal_rec();
		cc = (char*) uu_toolmalloc(MAX_PARA_REC);
		j = 0;
		dbyte = pbyte = 0;
		for(i=1; i<sect_ptr[2]+1; i++, i++)
		{
			status = uig_get_data(i,1,&dblk,cc);
			for (k=0; k<j; k++)
			{
				if (dblk.level==All_layer[k]) 
				{
					break;
				}
			}
			if (k>=j && j<IG_LAYER)
			{
				All_layer[j] = dblk.level;
				j++;
			}
		}
		uu_toolfree(cc);
/*
.....sort layer number
*/
/*
.....just use bubble sort
*/
		uig_bubble_sort(All_layer, j);
		for (i=0; i<j; i++)
		{
			sprintf(layer_str[i], "%d", All_layer[i]);
			xmstring = XmStringCreateSimple(layer_str[i]);
			XmListAddItem(cform_item[2], xmstring, 0);
			XmStringFree(xmstring);
		}
	}
	else
	{
/*
.....Display default tolerance form the mod file if user does not enter filename
*/
			if(UIG_units_factor==1)
				strcpy(unit, "in");
			else if(UIG_units_factor==2)
				strcpy(unit, "mm");
		
			xmstring = XmStringCreateSimple(" ");
			XmListAddItem(cform_item[2], xmstring, 0);
			XmStringFree(xmstring);
	}
	XtManageChild(cform_item[2]);
/*
.....set default selection
*/
	if ((flag!=1)&&(tig_num_range!=0))
	{
		for (i=0; i<tig_num_range; i++)
		{
			sprintf(msg, "%d", tig_range[i][0]);
			xmstring = XmStringCreateSimple(msg);
			XmListSelectItem(cform_item[2], xmstring, False);
			XmStringFree(xmstring);
		}
	}
	else
		XmListSelectPos(cform_item[2], 1, False);
/*
.....added "Clear List" and "Select All" button besides the list
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 30); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 50); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0); n++;
/*	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition,20 ); n++;*/
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNrightWidget,cform_item[2] ); n++;
	cform_item[4] = XmCreatePushButton(range_form, "Select All", args, n);
	XtAddCallback(cform_item[4], XmNactivateCallback, 
					(XtCallbackProc)iges_SelAllCB, (XtPointer)cform_item[2]);
	XtManageChild(cform_item[4]);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 55); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 75); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0); n++;
/*	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20); n++;*/
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNrightWidget,cform_item[2] ); n++;
	cform_item[5] = XmCreatePushButton(range_form, "Clear All", args, n);
	XtAddCallback(cform_item[5], XmNactivateCallback, 
					(XtCallbackProc)iges_DeselAllCB, (XtPointer)cform_item[2]);
	XtManageChild(cform_item[5]);
	if (UIG_conv_opt)
		XtSetSensitive(range_form, True);
	else
		XtSetSensitive(range_form, False);
	
/*
.....third row
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 6*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 7*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20*rows); n++;
	
	cformMenu[1] = (Widget)XmCreatePulldownMenu(winid, "Labels", NULL,0);
/*
...........Create an Option button
*/
	lstr = XmStringCreateSimple("Entity Label Options:");
	XtSetArg(args[n], XmNsubMenuId, cformMenu[1]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	cform_item[3] = (Widget)XmCreateOptionMenu(winid,
		"Entity Label Option", args, n);
	XmStringFree(lstr);

	cform_choice[0] = XtVaCreateManagedWidget("Generate by IGES",
							xmPushButtonGadgetClass,  cformMenu[1], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 0;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[0], XmNactivateCallback,
			(XtCallbackProc)iges_chcCB, (XtPointer)OpStruct[ONps++]); 
	cform_choice[1] = XtVaCreateManagedWidget("From IGES Label Field",
							xmPushButtonGadgetClass,  cformMenu[1], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 1;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[1], XmNactivateCallback,
			(XtCallbackProc)iges_chcCB, (XtPointer)OpStruct[ONps++]); 
	cform_choice[2] = XtVaCreateManagedWidget(
							"From IGES Property",
							xmPushButtonGadgetClass,  cformMenu[1], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 2;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[2], XmNactivateCallback,
			(XtCallbackProc)iges_chcCB, (XtPointer)OpStruct[ONps++]); 
	cform_choice[3] = XtVaCreateManagedWidget(
							"From Existing Unibase",
							xmPushButtonGadgetClass,  cformMenu[1], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 3;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[3], XmNactivateCallback,
			(XtCallbackProc)iges_chcCB, (XtPointer)OpStruct[ONps++]); 
/*
...........Set the default choice
*/
	XtVaSetValues(cformMenu[1] , XmNmenuHistory,
			cform_choice[UIG_lab_opt], NULL);
	XtManageChild(cform_item[3]);
/*
..... Name Modals Configuration Button  -> Ed Ames Jan 01
*/
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 6*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 7*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 21*rows); n++;
   XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNrightPosition, 25*rows); n++;

   name_modal_button = XmCreatePushButton(winid,"Name Modals", args, n);
   XtAddCallback(name_modal_button, XmNactivateCallback, \
         uig_name_modal,NULL);
   XtManageChild(name_modal_button);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 7*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 8*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 7*rows); n++;
	
	cformMenu[2] = (Widget)XmCreatePulldownMenu(winid, "Labels", NULL,0);
/*
.....Max label size.
*/
	lstr = XmStringCreateSimple("Max Label Size:");
	XtSetArg(args[n], XmNsubMenuId, cformMenu[2]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
  	if (UIG_lab_opt !=1 && UIG_lab_opt !=2)
  		XtSetArg(args[n],XmNsensitive, False); 
	else
  		XtSetArg(args[n],XmNsensitive, True); 
	n++;
	cform_item[6] = (Widget)XmCreateOptionMenu(winid,
		"Max Label Size", args, n);
	XmStringFree(lstr);

	cform_choice[0] = XtVaCreateManagedWidget("Off",
							xmPushButtonGadgetClass,  cformMenu[2], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 0;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[0], XmNactivateCallback,
			(XtCallbackProc)iges_maxlabCB, (XtPointer)OpStruct[ONps++]); 
	cform_choice[1] = XtVaCreateManagedWidget("1",
							xmPushButtonGadgetClass,  cformMenu[2], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 1;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[1], XmNactivateCallback,
			(XtCallbackProc)iges_maxlabCB, (XtPointer)OpStruct[ONps++]); 
	cform_choice[2] = XtVaCreateManagedWidget("2",
							xmPushButtonGadgetClass,  cformMenu[2], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 2;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[2], XmNactivateCallback,
			(XtCallbackProc)iges_maxlabCB, (XtPointer)OpStruct[ONps++]); 
	cform_choice[3] = XtVaCreateManagedWidget("3",
							xmPushButtonGadgetClass,  cformMenu[2], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 3;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[3], XmNactivateCallback,
			(XtCallbackProc)iges_maxlabCB, (XtPointer)OpStruct[ONps++]); 
	cform_choice[4] = XtVaCreateManagedWidget("4",
							xmPushButtonGadgetClass,  cformMenu[2], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 4;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[4], XmNactivateCallback,
			(XtCallbackProc)iges_maxlabCB, (XtPointer)OpStruct[ONps++]); 
	cform_choice[5] = XtVaCreateManagedWidget("5",
							xmPushButtonGadgetClass,  cformMenu[2], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 5;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[5], XmNactivateCallback,
			(XtCallbackProc)iges_maxlabCB, (XtPointer)OpStruct[ONps++]);
	cform_choice[6] = XtVaCreateManagedWidget("6",
							xmPushButtonGadgetClass,  cformMenu[2], NULL);
	OpStruct[ONps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	OpStruct[ONps]->fld = 3;
	OpStruct[ONps]->chc = 6;
	OpStruct[ONps]->type = 1;
	XtAddCallback(cform_choice[6], XmNactivateCallback,
			(XtCallbackProc)iges_maxlabCB, (XtPointer)OpStruct[ONps++]);
/*
...........Set the default choice
*/
	XtVaSetValues(cformMenu[2] , XmNmenuHistory,
			cform_choice[UIG_max_lab], NULL);
	XtManageChild(cform_item[6]);

/*
.....Use Subscript check box
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 7*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 8*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 7*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 13*rows); n++;

	if (UIG_lab_opt !=1 && UIG_lab_opt !=2)
  		XtSetArg(args[n],XmNsensitive, False); 
	else
  		XtSetArg(args[n],XmNsensitive, True); 
	n++;

	label_opt[8] = XtCreateManagedWidget("frame", xmFrameWidgetClass, winid,
								args,n);
   frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,label_opt[8],
      XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
   board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
      XmNorientation,XmVERTICAL,NULL);
   UIG_toggle_use_subscript = XtVaCreateManagedWidget("Use Subscripts", 
			xmToggleButtonWidgetClass, board,NULL,0);
   if(UIG_use_sub)state = True;
   else state = False;
   XmToggleButtonSetState(UIG_toggle_use_subscript,state,False);
   XtManageChild(board);
   XtManageChild(frame);
/*
.....Concatenate Subscript check box
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 7*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 8*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 13*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20*rows); n++;

  	if (UIG_lab_opt !=1 && UIG_lab_opt !=2)
  		XtSetArg(args[n],XmNsensitive, False); 
	else
  		XtSetArg(args[n],XmNsensitive, True); 
	n++;

	label_opt[9] = XtCreateManagedWidget("frame", xmFrameWidgetClass, winid,
								args,n);
   frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,label_opt[9],
      XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
   board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
      XmNorientation,XmVERTICAL,NULL);
   UIG_toggle_concat_subscript = XtVaCreateManagedWidget("Concatenate Subscripts", 
			xmToggleButtonWidgetClass, board,NULL,0);
   if(UIG_concat_sub)state = True;
   else state = False;
   XmToggleButtonSetState(UIG_toggle_concat_subscript,state,False);
   XtManageChild(board);
   XtManageChild(frame);

/*
......Label for text field for unibase label option JANET
*/
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 8*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 9*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
   XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNrightPosition, 8*rows); n++;
   XtSetArg(args[n],XmNalignment, XmALIGNMENT_BEGINNING); n++;
/*
.....This dimms the label "Unibase for Labels:" if the label
.....type is not equal to eight.
*/
	if (UIG_lab_opt !=3)
  		XtSetArg(args[n],XmNsensitive, False); 
	else
  		XtSetArg(args[n],XmNsensitive, True); 
	n++;

   label_opt[0] = XtCreateManagedWidget("Unibase for Labels:",
               xmLabelWidgetClass,winid,args,n);

/*
......Text field for unibase label option JANET
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 8*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 9*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 9*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20*rows); n++;
	if(UIG_lab_opt !=3)
		XtSetArg(args[n],XmNsensitive, False); n++;

	label_opt[1] = XtCreateManagedWidget("Text", xmTextWidgetClass, winid,
								args,n);
	XmTextSetString(label_opt[1],"");
/*
......Browse Button
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 8*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 9*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 21*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 25*rows); n++;
	if (UIG_lab_opt != 3)
		XtSetArg(args[n],XmNsensitive, False); n++;

   label_opt[2]= XmCreatePushButton(winid, "Browse", args, n);
   XtAddCallback(label_opt[2], XmNactivateCallback,
               (XtCallbackProc)iges_labelCB, (XtPointer)5);
   XtManageChild(label_opt[2]);
/*
.....Match level.
*/
	cformMenu[3] = (Widget)XmCreatePulldownMenu(winid, "Match_level", NULL,0);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 9*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 10*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 7*rows); n++;

	lstr = XmStringCreateSimple("Match Level: ");
	XtSetArg(args[n], XmNsubMenuId, cformMenu[3]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	if (UIG_lab_opt !=3)
  		XtSetArg(args[n],XmNsensitive, False); 
	else
  		XtSetArg(args[n],XmNsensitive, True); 
	n++;
	cform_item[7] = (Widget)XmCreateOptionMenu(winid,
		"Match Level: ", args, n);
	XmStringFree(lstr);

	cform_choice[0] = XtVaCreateManagedWidget("Exact",
							xmPushButtonGadgetClass,  cformMenu[3], NULL);
	XtAddCallback(cform_choice[0], XmNactivateCallback,
			(XtCallbackProc)uig_matchlevCB, (XtPointer)&UIG_matchlev_cba[0]); 
	cform_choice[1] = XtVaCreateManagedWidget("1",
							xmPushButtonGadgetClass,  cformMenu[3], NULL);
	XtAddCallback(cform_choice[1], XmNactivateCallback,
			(XtCallbackProc)uig_matchlevCB, (XtPointer)&UIG_matchlev_cba[1]); 
	cform_choice[2] = XtVaCreateManagedWidget("2",
							xmPushButtonGadgetClass,  cformMenu[3], NULL);
	XtAddCallback(cform_choice[2], XmNactivateCallback,
			(XtCallbackProc)uig_matchlevCB, (XtPointer)&UIG_matchlev_cba[2]); 
	cform_choice[3] = XtVaCreateManagedWidget("3",
							xmPushButtonGadgetClass,  cformMenu[3], NULL);
	XtAddCallback(cform_choice[3], XmNactivateCallback,
			(XtCallbackProc)uig_matchlevCB, (XtPointer)&UIG_matchlev_cba[3]); 
	cform_choice[4] = XtVaCreateManagedWidget("4",
							xmPushButtonGadgetClass,  cformMenu[3], NULL);
	XtAddCallback(cform_choice[4], XmNactivateCallback,
			(XtCallbackProc)uig_matchlevCB, (XtPointer)&UIG_matchlev_cba[4]); 
/*
...........Set the default choice
*/
	XtVaSetValues(cformMenu[3] , XmNmenuHistory,
			cform_choice[UIG_matchlevel], NULL);
	XtManageChild(cform_item[7]);
/*
.....Matching tolerance label.
*/
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 9*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 10*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 8*rows); n++;
   XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNrightPosition, 15*rows); n++;
   XtSetArg(args[n],XmNalignment, XmALIGNMENT_BEGINNING); n++;

	if (UIG_lab_opt !=3 && !UIG_nodups)
  		XtSetArg(args[n],XmNsensitive, False); 
	else
  		XtSetArg(args[n],XmNsensitive, True); 
	n++;

   label_opt[4] = XtCreateManagedWidget("Matching Tolerance:",
               xmLabelWidgetClass,winid,args,n);
/*
.....Match tolerance field
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 9*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 10*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 15*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20*rows); n++;

	if (UIG_lab_opt !=3 && !UIG_nodups)
  		XtSetArg(args[n],XmNsensitive, False); 
	else
  		XtSetArg(args[n],XmNsensitive, True); 
	n++;

	label_opt[5] = XtCreateManagedWidget("Text", xmTextWidgetClass, winid,
								args,n);
	sprintf(tmp,"%6.4lf%s",UIG_match_tol_disp,unit);
	XmTextSetString(label_opt[5],tmp);
/*
......Attributes Button
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 9*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 10*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 21*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 25*rows); n++;
	if (UIG_lab_opt != 3)
		XtSetArg(args[n],XmNsensitive, False); n++;

   UIG_match_attributes = XmCreatePushButton(winid, "Attributes", args, n);
   XtAddCallback(UIG_match_attributes, XmNactivateCallback,
               uig_attributes, NULL);
   XtManageChild(UIG_match_attributes);
/*
.....Start Unmatched Entities.
*/
	cformMenu[4] = (Widget)XmCreatePulldownMenu(winid,"Start_Unmatch", NULL,0);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 12*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 13*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 15*rows); n++;

	lstr = XmStringCreateSimple("Start Unmatched Entites from: ");
	XtSetArg(args[n], XmNsubMenuId, cformMenu[4]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	if (label_type !=8)
  		XtSetArg(args[n],XmNsensitive, False); 
	else
  		XtSetArg(args[n],XmNsensitive, True); 
	n++;
	cform_item[8] = (Widget)XmCreateOptionMenu(winid,
		"Start Unmatched Entites from: ", args, n);
	XmStringFree(lstr);

	cform_choice[0] = XtVaCreateManagedWidget("Next",
							xmPushButtonGadgetClass,  cformMenu[4], NULL);
	XtAddCallback(cform_choice[0], XmNactivateCallback,
			(XtCallbackProc)uig_startunmatchCB, (XtPointer)&UIG_start_unmatch_cba[0]); 
	cform_choice[1] = XtVaCreateManagedWidget("Secondary",
							xmPushButtonGadgetClass,  cformMenu[4], NULL);
	XtAddCallback(cform_choice[1], XmNactivateCallback,
			(XtCallbackProc)uig_startunmatchCB, (XtPointer)&UIG_start_unmatch_cba[1]); 
/*
...........Set the default choice
*/
	XtVaSetValues(cformMenu[4] , XmNmenuHistory,
			cform_choice[UIG_start_unmatch], NULL);
	XtManageChild(cform_item[8]);
/*
.....Create unmatched entities checkbox
*/
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 10*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 11*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
   XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNrightPosition, 20*rows); n++;
	if (UIG_lab_opt != 3)
      XtSetArg(args[n],XmNsensitive, False); 
	else
      XtSetArg(args[n],XmNsensitive, True);
	n++;

   label_opt[14] = XtCreateManagedWidget("frame", xmFrameWidgetClass, winid,
                        args,n);
   frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,label_opt[14],
      XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
   board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
      XmNorientation,XmVERTICAL,NULL);
   UIG_toggle_unmatch_sec =
   XtVaCreateManagedWidget(
   "Create Unmatched entities from secondary unibase",
   xmToggleButtonWidgetClass, board,NULL,0);
   state = True ; if (UIG_unmatch_sec == UU_FALSE) state = False;
   XmToggleButtonSetState(UIG_toggle_unmatch_sec,state,False);
   XtManageChild(board);
   XtManageChild(frame);

/*
.....Regressive Matching
*/
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 11*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 12*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
   XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNrightPosition, 20*rows); n++;
	if (label_type != 8)
      XtSetArg(args[n],XmNsensitive, False); 
	else
      XtSetArg(args[n],XmNsensitive, True);
	n++;

   label_opt[15] = XtCreateManagedWidget("frame", xmFrameWidgetClass, winid,
                        args,n);
   frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,label_opt[15],
      XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
   board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
      XmNorientation,XmVERTICAL,NULL);
   UIG_toggle_reg_match =
   XtVaCreateManagedWidget(
   "Regressive Matching",
   xmToggleButtonWidgetClass, board,NULL,0);
   state = True ; if (UIG_regressive == UU_FALSE) state = False;
   XmToggleButtonSetState(UIG_toggle_reg_match,state,False);
   XtManageChild(board);
   XtManageChild(frame);
/*
.....Reinitialize Label Counters check box.
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 13*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 14*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 10*rows); n++;

	label_opt[7] = XtCreateManagedWidget("frame", xmFrameWidgetClass, winid,
								args,n);
   frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,label_opt[7],
      XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
   board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
      XmNorientation,XmVERTICAL,NULL);
   UIG_toggle_reinit_lab = XtVaCreateManagedWidget("Reinitialize Label Counters", 
			xmToggleButtonWidgetClass, board,NULL,0);
   state = True ; if (UIG_reinit_lab == UU_FALSE) state = False;
   XmToggleButtonSetState(UIG_toggle_reinit_lab,state,False);
   XtManageChild(board);
   XtManageChild(frame);
/*
.....Iges color entity check box
*/
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 13*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 14*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 12*rows); n++;
   XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNrightPosition, 20*rows); n++;

   label_opt[12] = XtCreateManagedWidget("frame", xmFrameWidgetClass, winid,
                        args,n);
   frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,label_opt[12],
      XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
   board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
      XmNorientation,XmVERTICAL,NULL);
   UIG_toggle_color_iges = XtVaCreateManagedWidget("Use Iges Color entity",
         xmToggleButtonWidgetClass, board,NULL,0);
   state = True ; if (UIG_color_iges == UU_FALSE) state = False;
   XmToggleButtonSetState(UIG_toggle_color_iges,state,False);
   XtManageChild(board);
   XtManageChild(frame);
/*
.....No Duplicate Geometry check box.
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 14*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 15*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 12*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20*rows); n++;

	label_opt[10] = XtCreateManagedWidget("frame", xmFrameWidgetClass, winid,
								args,n);
   frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,label_opt[10],
      XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
   board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
      XmNorientation,XmVERTICAL,NULL);
/*
.....control area
.....this toggle box activates the tolerance field.
*/
   UIG_toggle_nodups = XtVaCreateManagedWidget("No Duplicate Geometry", 
			xmToggleButtonWidgetClass, board,NULL);

   XtAddCallback (UIG_toggle_nodups, XmNvalueChangedCallback, 
					no_dups,NULL);
   state = True ; if (UIG_nodups == UU_FALSE) state = False;
   XmToggleButtonSetState(UIG_toggle_nodups,state,False);
   XtManageChild(board);
   XtManageChild(frame);
/*
.....Adding a check box for importing
.....surfaces as shaded or unshaded, the default is
.....shaded.  JLS  7/29/99
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 14*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 15*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 10*rows); n++;

	label_opt[6] = XtCreateManagedWidget("frame", xmFrameWidgetClass, winid,
								args,n);
   frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,label_opt[6],
      XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
   board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
      XmNorientation,XmVERTICAL,NULL);
   toggle1 = XtVaCreateManagedWidget("Import Surfaces as Shaded", 
			xmToggleButtonWidgetClass, board,NULL,0);
   state = True ; if (shade_set == UU_FALSE) state = False;
   XmToggleButtonSetState(toggle1,state,False);
   XtManageChild(board);
   XtManageChild(frame);
/*
.....Display Surface Edges
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 15*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 16*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 10*rows); n++;

	label_opt[16] = XtCreateManagedWidget("frame", xmFrameWidgetClass, winid,
								args,n);
   frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,label_opt[16],
      XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
   board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
      XmNorientation,XmVERTICAL,NULL);
   UIG_toggle_sfedge = XtVaCreateManagedWidget("Display Surface Edges", 
			xmToggleButtonWidgetClass, board,NULL);
	XtAddCallback(UIG_toggle_sfedge, XmNvalueChangedCallback,
			(XtCallbackProc)uig_sfedgeCB, NULL); 
   state = True ; if (UIG_srf_edge == UU_FALSE) state = False;
   XmToggleButtonSetState(UIG_toggle_sfedge,state,False);
   XtManageChild(board);
   XtManageChild(frame);
/*
.....Surface Edge Color
*/
/*
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 15*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 16*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 12*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20*rows); n++;

	label_opt[17] = XtCreateManagedWidget("frame", xmFrameWidgetClass, winid,
								args,n);
   frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,label_opt[17],
      XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
   board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
      XmNorientation,XmVERTICAL,NULL);
*/
/*
........Create the pulldown menu
*/
	cformMenu[5] = (Widget)XmCreatePulldownMenu(winid, "Edge_color",
		NULL,0);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 15*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 16*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 12*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20*rows); n++;

	lstr = XmStringCreateSimple("Surface Edge Color: ");
	XtSetArg(args[n], XmNsubMenuId, cformMenu[5]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	if (UIG_srf_edge)
		XtSetArg(args[n],XmNsensitive,True);
	else
		XtSetArg(args[n],XmNsensitive,False);
	n++;
	cform_item[10] = (Widget)XmCreateOptionMenu(winid,
		"Surface Edge Color: ", args, n);
	XmStringFree(lstr);
	cform_choice[0] = XtVaCreateManagedWidget("Default",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 0;
	XtAddCallback(cform_choice[0], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[1] = XtVaCreateManagedWidget("Black",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 1;
	XtAddCallback(cform_choice[1], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[2] = XtVaCreateManagedWidget("White",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 2;
	XtAddCallback(cform_choice[2], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[3] = XtVaCreateManagedWidget("Blue",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 3;
	XtAddCallback(cform_choice[3], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[4] = XtVaCreateManagedWidget("Red",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 4;
	XtAddCallback(cform_choice[4], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[5] = XtVaCreateManagedWidget("Green",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 5;
	XtAddCallback(cform_choice[5], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[6] = XtVaCreateManagedWidget("Magenta",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 6;
	XtAddCallback(cform_choice[6], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[7] = XtVaCreateManagedWidget("Yellow",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 7;
	XtAddCallback(cform_choice[7], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[8] = XtVaCreateManagedWidget("Cyan",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 8;
	XtAddCallback(cform_choice[8], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[9] = XtVaCreateManagedWidget("Brown",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 9;
	XtAddCallback(cform_choice[9], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[10] = XtVaCreateManagedWidget("Tan",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 10;
	XtAddCallback(cform_choice[10], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[11] = XtVaCreateManagedWidget("Lt Blue",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 11;
	XtAddCallback(cform_choice[11], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[12] = XtVaCreateManagedWidget("Sea Green",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 12;
	XtAddCallback(cform_choice[12], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[13] = XtVaCreateManagedWidget("Orange",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 13;
	XtAddCallback(cform_choice[13], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[14] = XtVaCreateManagedWidget("Pink",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 14;
	XtAddCallback(cform_choice[14], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[15] = XtVaCreateManagedWidget("Purple",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 15;
	XtAddCallback(cform_choice[15], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
	cform_choice[16] = XtVaCreateManagedWidget("Grey",
							xmPushButtonGadgetClass,  cformMenu[5], NULL);
	color = 16;
	XtAddCallback(cform_choice[16], XmNactivateCallback,
			(XtCallbackProc)uig_edgecolorCB, (XtPointer)&color); 
/*
...........Set the default choice
*/
	if (UIG_edge_color>=0 && UIG_edge_color<64)
		i = UIG_edge_color + 1;
	else
		i = 0;
	XtVaSetValues(cformMenu[5] , XmNmenuHistory,
			cform_choice[i], NULL);
	XtManageChild(cform_item[10]);
/*
   XtManageChild(board);
   XtManageChild(frame);
*/
/*
.....Create the Action Area
*/
	n = XtNumber(actionList2);
	for (i=0;i<n;i++) actionList2[i].data = (XtPointer)CrtU_form	;
	iges_mfcreate_action(pane,actionList2,n);
/*
.....Manage the Form
*/
	XtManageChild(winid);
	XtManageChild(pane);
	XtManageChild(CrtU_form);
	while (!formEof)
	{
		XDefineCursor(XtDisplay(TOOL_Parent), XtWindow(TOOL_Parent), normal_cursor);
		XFlush(XtDisplay(TOOL_Parent));
		XtAppNextEvent(TOOL_App, &x_event);
		XtDispatchEvent(&x_event);
	}
}

/*********************************************************************
**    I_FUNCTION     :  iges_chcIN(widget, client_data, call_data)
**       Callback for choice button asking for units, either INCHES
**       or MM.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void iges_chcIN(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	Pstruct *ps=(Pstruct *)client_data;
/*
.....call from main form
*/
	if (ps->type == 0)
	{
		if (ps->chc==0)
			output_units = 1;
		else
			output_units = 0;
	}
}
/*********************************************************************
**    I_FUNCTION     :  iges_mfmain
**       Main routine for the UNICAD IGES translator.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
iges_mfmain()
{
	Arg args[20];
   XmString file,help, str1, key1,str2, key2;
	XmString lstr;
	XEvent event;
	int rows1;
	char *p,*ux_getenv();
	char pbuf[80];
	Widget frame_in, frame_out, pane, iges_mfcreate_action();
	int n,i,rows,cols;
	char perc_str[10];
/*
.....Initialize MOTIF
*/
	i = 0;
	n = 0;
	XtSetArg(args[n],XmNmappedWhenManaged,False); n++;
	XtSetArg(args[n],XmNwidth, 750); n++;
	XtSetArg(args[n],XmNheight, 600); n++;
/*
.....Display the version number in the title bar of the form. JLS 10/15/99
	XtSetArg(args[n],XmNtitle, "NCL/IGES Conversion Program "); n++;
*/
	sprintf(pbuf, "NCL/IGES V%5.3f\n",NCL_version);
	XtSetArg(args[n],XmNtitle, pbuf); n++;
	TOOL_Parent = XtAppInitialize (&TOOL_App, "IGES", NULL,0,&i,NULL,NULL,args,n);
   n=0;
   XtSetArg(args[n],XmNdeleteResponse, XmDO_NOTHING); n++;
   Main_Window = XtCreateManagedWidget("MAIN", xmMainWindowWidgetClass,
                  TOOL_Parent, args,n);

   file = XmStringCreateSimple("File");
   help = XmStringCreateSimple("Help");

   menuBar = XmVaCreateSimpleMenuBar(Main_Window, "menubar",
               XmVaCASCADEBUTTON, file,'F' ,
               XmVaCASCADEBUTTON, help, 'H',
               NULL);
   XmStringFree(file);
   XmStringFree(help);
   str1 = XmStringCreateSimple("Exit");
   key1 = XmStringCreateSimple("Alt + x");
   menu1 = XmVaCreateSimplePulldownMenu(menuBar, "file_menu", 0, iges_file,
            XmVaPUSHBUTTON, str1, 'x', "Alt<Key>x", key1,
            NULL);

   XmStringFree(str1);

   str1 = XmStringCreateSimple("Help");
   str2 = XmStringCreateSimple("About Ncl/Iges..."); 
   key1 = XmStringCreateSimple("Alt + h");
   key2 = XmStringCreateSimple("Alt + b");
   menu2 = XmVaCreateSimplePulldownMenu(menuBar, "help_menu", 1, iges_help,
            XmVaPUSHBUTTON, str1, 'H', "Alt<Key>h", key1,
            XmVaPUSHBUTTON, str2, 'B', "Alt<Key>b", key2,
            NULL);

   XmStringFree(str1);
   XmStringFree(str2);
   XmStringFree(key1);
   XmStringFree(key2);
   XtManageChild(menuBar);

	pane = XtVaCreateWidget("iges_main",xmPanedWindowWidgetClass, Main_Window,
				XmNsashWidth,1,
				XmNsashHeight,1,
				NULL);
/*
.....Create a Form widget
*/
	n = 0;
	rows = 15;
	cols = 75;
	XtSetArg(args[n],XmNfractionBase,rows*cols); n++;
	Main_form = XtCreateWidget("main_form", xmFormWidgetClass,pane,args,n);
	if (Main_form == NULL) return 0;

	n= 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	 XtSetArg(args[n],XmNbottomPosition, 4*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;

	frame_in = XtCreateManagedWidget("iges_in", xmFrameWidgetClass,Main_form,args,n);
	n = 0;
	rows1 = 4;
	XtSetArg(args[n],XmNfractionBase,rows1*cols); n++;
	form_in =  XtCreateManagedWidget("form_in", xmFormWidgetClass,frame_in,
						args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 4*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 8*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	frame_out = XtCreateManagedWidget("iges_out", xmFrameWidgetClass,Main_form,args,n);

	n = 0;
	XtSetArg(args[n],XmNfractionBase,rows1*cols); n++;
	form_out =  XtCreateManagedWidget("form_out", xmFormWidgetClass,frame_out,
						args,n);
/*
...Create first row, it is for "IGES IN"
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows); n++;
	
	mform_item[1] = XtCreateManagedWidget("IGES IN",
					xmLabelWidgetClass,form_in,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[1]); n++;
	XtSetArg(args[n],XmNindicatorType, XmONE_OF_MANY); n++;
	mform_item[2] = XtCreateWidget("", xmToggleButtonWidgetClass, form_in,
							args, n);
	XmToggleButtonSetState(mform_item[2], 1, False);
/*
...........Add the callback routine
*/
	XtAddCallback(mform_item[2], XmNvalueChangedCallback,
			(XtCallbackProc)iges_ckCB, (XtPointer) 0);
	XtManageChild(mform_item[2]);
/*    
...Create second row, it is for "IGES IN"
*/ 
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 30*rows1); n++;
	XtSetArg(args[n],XmNalignment, XmALIGNMENT_BEGINNING); n++;

	mform_item[3] = XtCreateManagedWidget("Input IGES Data File Name:",
					xmLabelWidgetClass,form_in,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[3]); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 30*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 62*rows1); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;

	mform_item[4] = XtCreateManagedWidget("File_Text",
			xmTextWidgetClass,form_in,args,n);
	XmTextSetString(mform_item[4], "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[4]); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 62*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
/*
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 26*rows1); n++;
*/
	mform_item[5] = XmCreatePushButton(form_in, "Browse", args, n);
	XtAddCallback(mform_item[5], XmNactivateCallback, 
					(XtCallbackProc)iges_mbrowseCB, (XtPointer)5);
	XtManageChild(mform_item[5]);
/*    
...Create forth row, it is for "IGES IN"
*/ 
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 3*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 4*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20*rows1); n++;
	mform_item[6] = XmCreatePushButton(form_in, "File Summary", args, n);
	XtAddCallback(mform_item[6], XmNactivateCallback, 
					(XtCallbackProc)iges_FileSCB, (XtPointer)mform_item[4]);
	XtManageChild(mform_item[6]);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 3*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 4*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 21*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 39*rows1); n++;
	mform_item[7] = XmCreatePushButton(form_in, "Options", args, n);
	XtAddCallback(mform_item[7], XmNactivateCallback, 
					(XtCallbackProc)iges_optionUCB, (XtPointer)NULL);
	XtManageChild(mform_item[7]);

/*    
...Create third row, it is for "IGES IN"
*/ 
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 30*rows1); n++;
	XtSetArg(args[n],XmNalignment, XmALIGNMENT_BEGINNING); n++;

	mform_item[8] = XtCreateManagedWidget("Output Unibase File Name:",
					xmLabelWidgetClass,form_in,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[8]); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 30*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 62*rows1); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;

	mform_item[9] = XtCreateManagedWidget("File_Text",
			xmTextWidgetClass,form_in,args,n);
	XmTextSetString(mform_item[9], "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[9]); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 62*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	mform_item[10] = XmCreatePushButton(form_in, "Browse", args, n);
	XtAddCallback(mform_item[10], XmNactivateCallback, 
					(XtCallbackProc)iges_mbrowseCB, (XtPointer)10);
	XtManageChild(mform_item[10]);
/*
.....First row, for "IGES OUT"
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows1); n++;
	mform_item[11] = XtCreateManagedWidget("IGES OUT",
					xmLabelWidgetClass,form_out,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[11]); n++;
	XtSetArg(args[n],XmNindicatorType, XmONE_OF_MANY); n++;
	mform_item[12] = XtCreateWidget("", xmToggleButtonWidgetClass, form_out,
							args, n);
	XmToggleButtonSetState(mform_item[12], 0, False);
/*
...........Add the callback routine
*/
	XtAddCallback(mform_item[12], XmNvalueChangedCallback,
			(XtCallbackProc)iges_ckCB, (XtPointer) 1);
	XtManageChild(mform_item[12]);
/*    
...Create second row, it is for "IGES OUT"
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 30*rows1); n++;
	XtSetArg(args[n],XmNalignment, XmALIGNMENT_BEGINNING); n++;
	mform_item[13] = XtCreateManagedWidget("Input Part File Name:",
					xmLabelWidgetClass,form_out,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 30*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 62*rows1); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[13]); n++;
*/
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;

	mform_item[14] = XtCreateManagedWidget("File_Text",
			xmTextWidgetClass,form_out,args,n);
	XmTextSetString(mform_item[14], "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 62*rows1); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[14]); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
*/
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	mform_item[15] = XmCreatePushButton(form_out, "Browse", args, n);
	XtAddCallback(mform_item[15], XmNactivateCallback, 
					(XtCallbackProc)iges_mbrowseCB, (XtPointer)15);
	XtManageChild(mform_item[15]);

/*
......forth row, it is for "IGES OUT"
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 3*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 4*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2*rows1); n++;
	mform_item[16] = (Widget)XmCreatePulldownMenu(form_out, "Drawing",NULL,0);
/*
...........Create an Option button
*/

	lstr = XmStringCreateSimple("Output Drawings Only:");
	XtSetArg(args[n], XmNsubMenuId, mform_item[16]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	mform_item[17] = (Widget)XmCreateOptionMenu(form_out,
						"", args, n);
	XmStringFree(lstr);
/*
...........Create the options
*/
	mform_choice[0] = XtVaCreateManagedWidget("Yes", xmPushButtonGadgetClass, 
							mform_item[16], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 17;
	pStruct[Nps]->chc = 0;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[0], XmNactivateCallback,
				(XtCallbackProc)iges_chcCB, (XtPointer)pStruct[Nps++]);
	mform_choice[1] = XtVaCreateManagedWidget("No", xmPushButtonGadgetClass, 
							mform_item[16], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 17;
	pStruct[Nps]->chc = 1;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[1], XmNactivateCallback,
				(XtCallbackProc)iges_chcCB, (XtPointer)pStruct[Nps++]);
/*
...........Set the default choice
*/
	XtVaSetValues(mform_item[16], XmNmenuHistory,
			mform_choice[1], NULL);
	XtManageChild(mform_item[17]);

/*
.....Adding a choice button to choose whether the output files
.....should be in inches or mm. Inches will be the default
.....JLS 1/19/99
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 3*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 4*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 42*rows1); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[16]); n++;
*/
	mform_item[22] = (Widget)XmCreatePulldownMenu(form_out, "Units",NULL,0);
/*
.....Create an Another Option button
*/
	lstr = XmStringCreateSimple("OUTPUT UNITS :");
	XtSetArg(args[n], XmNsubMenuId, mform_item[22]); n++;
	XtSetArg(args[n], XmNlabelString, lstr); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	mform_item[23] = (Widget)XmCreateOptionMenu(form_out,
						"", args, n);
	XmStringFree(lstr);
/*
.....Create the options, MM or INCHES.
*/
	mform_choice[0] = XtVaCreateManagedWidget("MM", xmPushButtonGadgetClass, 
							mform_item[22], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 23;
	pStruct[Nps]->chc = 0;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[0], XmNactivateCallback,
				(XtCallbackProc)iges_chcIN, (XtPointer)pStruct[Nps++]);
	mform_choice[1] = XtVaCreateManagedWidget("INCHES", xmPushButtonGadgetClass, 
							mform_item[22], NULL);
/*
...........Add the callback routine
*/
	pStruct[Nps] = (Pstruct *)uu_malloc(sizeof(Pstruct));
	pStruct[Nps]->fld = 23;
	pStruct[Nps]->chc = 1;
	pStruct[Nps]->type = 0;
	XtAddCallback(mform_choice[1], XmNactivateCallback,
				(XtCallbackProc)iges_chcIN, (XtPointer)pStruct[Nps++]);
/*
...........Set the default choice
*/
	XtVaSetValues(mform_item[22], XmNmenuHistory,
			mform_choice[1], NULL);
	XtManageChild(mform_item[23]);

/*
.....third row, it is for "IGES OUT"
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 30*rows1); n++;
	XtSetArg(args[n],XmNalignment, XmALIGNMENT_BEGINNING); n++;
	mform_item[18] = XtCreateManagedWidget("Output IGES Data File Name:",
					xmLabelWidgetClass,form_out,args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[18]); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 30*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 62*rows1); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;
	mform_item[19] = XtCreateManagedWidget("File_Text",
			xmTextWidgetClass,form_out,args,n);
	XmTextSetString(mform_item[19], "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, mform_item[19]); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 62*rows1); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;

	mform_item[20] = XmCreatePushButton(form_out, "Browse", args, n);
	XtAddCallback(mform_item[20], XmNactivateCallback, 
					(XtCallbackProc)iges_mbrowseCB, (XtPointer)20);
	XtManageChild(mform_item[20]);
/*
....."status" label
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 8*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 9*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 0*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 10*rows); n++;
	mform_item[21] = XtCreateWidget("Status:",
					xmLabelWidgetClass,Main_form,args,n);
	XtManageChild(mform_item[21]);
/*
......add the processbar/prompt/label in the line of the "status" label. 
......In PC, the processbar will show under "status text" but here it 
......hard to change the size and update processbar like PC code does. 
......So we will replace the "status" label with the processbar 
......we just show/hide the processbar/status label without have to i
......change the window size
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 8*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 9*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 20*rows); n++;
	XtSetArg(args[n],XmNalignment, XmALIGNMENT_BEGINNING); n++;

	m_process_label1 = XtCreateWidget("Status:",
						xmLabelWidgetClass, Main_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 8*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 9*cols); n++;

	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 21*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 50*rows); n++;
	m_process_bar = (Widget) XmCreateDrawingArea(
						Main_form, "processbar", args,n);
	m_percent = 0;
	XtAddCallback(m_process_bar, XmNexposeCallback,
					(XtCallbackProc)drawCB,
					NULL);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 8*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 9*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 51*rows); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 74*rows); n++;
	XtSetArg(args[n],XmNalignment, XmALIGNMENT_BEGINNING); n++;

	sprintf(perc_str, "%d%%", 1);
	m_process_label2 = XtCreateWidget(perc_str,
						xmLabelWidgetClass, Main_form,args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 9*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNscrollVertical, True); n++;
	XtSetArg(args[n], XmNeditable, False); n++;
	XtSetArg(args[n],XmNeditMode, XmMULTI_LINE_EDIT); n++;
	tool_stat_win = XmCreateScrolledText(Main_form,
							"status_text" ,args,n);
	XmTextSetString(tool_stat_win, "");
	XtManageChild(tool_stat_win);
/*
.....Create the Action Area
*/
	n = XtNumber(actionList1);
	for (i=0;i<n;i++) actionList1[i].data = (XtPointer)TOOL_Parent	;
	iges_mfcreate_action(pane,actionList1,n);
	
	XtManageChild(Main_form);
	XtManageChild(pane);
	XtRealizeWidget(TOOL_Parent);
	XtMapWidget(TOOL_Parent);
	iges_enable_in();
	m_process_gc = XCreateGC(XtDisplay(m_process_bar), 
							XtWindow(m_process_bar),
							0L, (XGCValues*) NULL);
	normal_cursor = XCreateFontCursor(XtDisplay(TOOL_Parent),
									XC_top_left_arrow);
	wait_cursor = XCreateFontCursor(XtDisplay(TOOL_Parent), XC_watch);
	for(;;)
	{
		XDefineCursor(XtDisplay(TOOL_Parent), XtWindow(TOOL_Parent), normal_cursor);
		XFlush(XtDisplay(TOOL_Parent));
		XtAppNextEvent(TOOL_App,&event);
		XDefineCursor(XtDisplay(TOOL_Parent), XtWindow(TOOL_Parent), wait_cursor);
		XFlush(XtDisplay(TOOL_Parent));
		XtDispatchEvent(&event);
	} 
/*
	if (iges_fname[0]!='\0')
	{
		close(iges_fd);
		if (ux_delete0("iges.tmp"))
			printf("Error! - can't delete iges.tmp");
	}
*/
failed:
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  iges_mfcreate_action(parent,actions,num_actions)
**       Creates the action area for forms, which includes PICK, LOCATE,
**			ACCEPT, and CANCEL.  PICK and LOCATE are only enabled during
**			text field input.
**    PARAMETERS   
**       INPUT  : 
**          parent      = Form widget to create Action Area for.
**				actions     = List of Action Area buttons and procedures.
**				num_actions = Number of 'actions' for this area.
**       OUTPUT :  
**          output
**    RETURNS      : Action Area Widget.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Widget iges_mfcreate_action(parent,actions,num_actions)
Widget parent;
ActionAreaItem *actions;
int num_actions;
{
	Widget actionBut[10];
#define TIGHTNESS 40
	Widget action_area;
	int i;
	Dimension h,height;
/*
.....Create Action Area Form
*/
	action_area = XtVaCreateWidget("action_area",
		xmFormWidgetClass,parent,
		XmNfractionBase,num_actions*TIGHTNESS-1,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
/*
.....Create each of the buttons
*/
	for (i=0; i<num_actions; i++)
	{
		actionBut[i] = XtVaCreateManagedWidget(actions[i].label,
			xmPushButtonWidgetClass, action_area,
			XmNleftAttachment, i? XmATTACH_POSITION : XmATTACH_FORM,
			XmNleftPosition, TIGHTNESS*i,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNrightAttachment, i!=num_actions ? XmATTACH_POSITION : XmATTACH_FORM,
			XmNrightPosition, TIGHTNESS * i + (TIGHTNESS - 1),
			XmNshowAsDefault, i==0,
			XmNdefaultButtonShadowThickness, 1,
			NULL);
/*
.....Add the button callback
*/
		if (actions[i].callback)
			XtAddCallback(actionBut[i],XmNactivateCallback,
				actions[i].callback,actions[i].data);
/*
.....Set the default button and disable
.....resize of this pane
*/
		if (i == 0)
		{
			XtVaGetValues(action_area, XmNmarginHeight, &h, NULL);
			XtVaGetValues(actionBut[i], XmNheight, &height, NULL);
			height += 2 * h;
			XtVaSetValues(action_area,
				XmNdefaultButton, actionBut[i],
				XmNpaneMaximum, height,
				XmNpaneMinimum, height,
				NULL);
		}
	}
/*
.....Manage the Action Area
*/
	XtManageChild(action_area);
	return(action_area);
}

/*********************************************************************
**    I_FUNCTION     :  uig_mfget_range(num,range)
**          Get layer number ranges from user.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          num                     Number of ranges defined
**          range                   ranges
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_mfget_range(num, range)
int *num;
int range[10][2];
{
	int sel_num, *ans;
	int i = 0;
	if (Byrange==0)
		*num = 0;	
	else
	{
		XmListGetSelectedPos(cform_item[2], &ans, &sel_num);
		if (sel_num==0) *num = 0;
		else
		{
			for (i=0; i<sel_num; i++)
			{
				range[i][0] = range[i][1] = All_layer[ans[i]-1];
			}
			*num = sel_num;
		}
		if (sel_num!=0)
			XtFree((char*)ans);
	}
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  uig_get_label_type
**          put label type to list file. we have already get label
**				type when pick up choice button.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_mfget_label_type()
{
	switch (label_type)
	{
		case 1:
			uig_list_out("Labels generated by IGES.\n\n", UU_FALSE);
			break;
		case 2:
			uig_list_out("Labels generated by IGES using subscripts.\n\n", UU_FALSE);
			break;
		case 3:
			uig_list_out("Labels from IGES file.\n\n", UU_FALSE);
			break;
		case 4:
			uig_list_out("Labels from IGES file using subscripts.\n\n", UU_FALSE);
			break;
		case 5:
			uig_list_out("Labels from file using CV style subscripts.\n\n", UU_FALSE);
			break;
		case 6:
			uig_list_out("Labels from file using property entity.\n\n", UU_FALSE);
			break;
		case 7:
			uig_list_out("Labels from IGES using max 6 characters. \n\n", UU_FALSE);
			break;
		case 8:
			uig_list_out("Labels from existing unibase. \n\n", UU_FALSE);
			break;
	}
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  uig_mfget_unifil(filename)
**          get user output unibase file name
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          filename
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_mfget_unifil(filename)
char *filename;
{
	char* text;
	UX_pathname dir, fname, tempname, tempname1;
	int i;
	if (Iges_batch)
	{
		strcpy(filename, iges_unifile);
		return (0);
	}
	text = XmTextGetString(mform_item[9]);
	if(text[0]!='\0')
		strcpy(filename, text);
	else
		filename[0] = '\0';
	XtFree(text);
/*
.....check if user entered filename
*/
	ul_break_fname(filename, dir, fname);
	if (strlen(fname)==0)
/*
.....use default name
*/
	{
		strcpy(filename, iges_fname);
		ul_break_fname(filename, dir, fname);
	}
/*
.....if no directory input, use input iges file's directory
*/
	else if (strlen(dir)==0)
	{
		strcpy(tempname, iges_fname);
		ul_break_fname(tempname, dir, tempname1);
	}
/*
.....added ext '.u' if not have
*/
	i = strcspn (fname, ".");
	strcpy(&fname[i], ".u");
/*
.....then cat with directory
*/
#if UU_COMP == UU_VAXVMS
		strcat (dir, fname);
#else
	if (strlen(dir)!=0)
		strcat (dir,"/");
	strcat (dir, fname);
#endif
	strcpy(filename, dir);
	return 0;
}


/*********************************************************************
**    I_FUNCTION     :  uig_get_ptname(filename)
**          get user input part file name
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          filename
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_get_ptname(filename)
char *filename;
{
	char* text;
	UX_pathname dir, fname;
	int i;
	if (Iges_batch)
	{
		strcpy(filename, iges_ptfile);
		return (0);
	}
	text = XmTextGetString(mform_item[14]);
	if(text[0]!='\0')
		strcpy(filename, text);
	else
		filename[0] = '\0';
	XtFree(text);
/*
.....check if user entered filename
*/
	ul_break_fname(filename, dir, fname);
	if (strlen(fname)==0)
	{
		uig_mfmsg_box(NULL, "ERROR", "No input unibase file specified!");
		strcpy(input_ptname, filename);
		return 0;
	}
/*
.....added ext '.u'
*/
	i = strcspn (fname, ".");
	strcpy(&fname[i], ".u");
/*
.....then cat with directory
*/
#if UU_COMP == UU_VAXVMS
		strcat (dir, fname);
#else
	if (strlen(dir)!=0)
		strcat (dir,"/");
	strcat (dir, fname);
#endif
	strcpy(filename, dir);
	strcpy(input_ptname, filename);
	return 0;
}


/*********************************************************************
**    I_FUNCTION     :  uig_get_outname(filename)
**          get user output IGES name
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          filename
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_get_outname(filename)
char *filename;
{
	char* text;
	UX_pathname fname, dir, tempname, tempname1;
	int i;
	if (Iges_batch)
	{
		strcpy(filename, iges_outfile);
		return (0);
	}
	text = XmTextGetString(mform_item[19]);
	if(text[0]!='\0')
		strcpy(filename, text);
	else
		filename[0] = '\0';
	XtFree(text);
/*
.....check if user entered filename
*/
	ul_break_fname(filename, dir, fname);
	if (strlen(fname)==0)
/*
.....use default name
*/
	{
		strcpy(filename, input_ptname);
		ul_break_fname(filename, dir, fname);
	}
/*
.....if no directory input, use input unibase file's directory
*/
	else if (strlen(dir)==0)
	{
		strcpy(tempname, input_ptname);
		ul_break_fname(tempname, dir, tempname1);
	}
/*
.....added ext '.igs' if not have
*/
	i = strcspn (fname, ".");
	strcpy(&fname[i], ".igs");
/*
.....then cat with directory
*/
#if UU_COMP == UU_VAXVMS
		strcat (dir, fname);
#else
	if (strlen(dir)!=0)
		strcat (dir,"/");
	strcat (dir, fname);
#endif
	strcpy(filename, dir);
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  iges_enable_out()
**          Enable "IGES OUT" fields, and Disable "IGES IN" fields
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
iges_enable_out()
{
	int i;
	for (i=3; i<11; i++)
		XtSetSensitive(mform_item[i], False);
/*
....Need to increase to 24 from 21. JLS 1/19/99
*/
	for (i=13; i<24; i++)
		XtSetSensitive(mform_item[i], True);
	return 0;

}

/*********************************************************************
**    I_FUNCTION     :  iges_enable_in()
**          Enable "IGES IN" fields, and Disable "IGES OUT" fields
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
iges_enable_in()
{
	int i;
	for (i=3; i<11; i++)
		XtSetSensitive(mform_item[i], True);
/*
.....Increase 21 to 24.  JLS 1/19/99
*/
	for (i=13; i<21; i++)
		XtSetSensitive(mform_item[i], False);
	for (i=22; i<24; i++)
		XtSetSensitive(mform_item[i], False);
	return 0;
}

/*********************************************************************
**    I_FUNCTION     : uig_name_modal(OptionFrm, client_data, call_data)
**			Make a Motif form that allows the user to change the automatic
**			naming modals when converting from an IGES file to a Unibase
**			file.  The defaults are set in an INIT file.  The geometry 
**			abbreviations (ex. pt for points) can be set (between 3 and 7
**			characters), and numbering convention can be switched between
**			subscripts (ex. pt(1), pt(2), ...) and prefixs (ex. pt1, pt2,...).
**			If the user hits the ACCEPT button, update the INIT file and
**			the naming modal data structure UM_labelmdl.
**    PARAMETERS
**       INPUT  :
**				OptionFrm	=	ignored
**				client_data	=	ignored
**				call_data	=	ignored
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		AUTHOR		 :	Ed Ames  10 Jan 01
*********************************************************************/
void uig_name_modal(OptionFrm, client_data, call_data)
Widget OptionFrm;
XtPointer client_data;
XtPointer call_data;
{
	int i, num_geo;
	char temp[3];
	static char *names [11] = {"POINTS", "PNTVECS", "LINES", "VECTORS", 
      "PLANES", "CIRCLES", "CURVES", "SURFACES", "SHAPES", "MATRICES",
		"PATTERNS"}; 
	Widget name_modal_dialog, pane, control_form, action_form;
	Widget accept_but, cancel_but;
	XmString lab_str, prefix_lab, subscr_lab;
	extern void uig_destroy_names(), uig_name_configCB(), uig_change_prefix();
	
	num_geo = 11;
/*
..... Start of Name Modals form.
*/	
	name_modal_dialog = XtVaCreatePopupShell("Geometry Naming Modals", 
            xmDialogShellWidgetClass, XtParent(OptionFrm),
            XmNdeleteResponse, XmDESTROY, 
            NULL);

	pane = XtVaCreateWidget ("pane", xmPanedWindowWidgetClass, 
				name_modal_dialog, XmNsashWidth, 1, XmNsashHeight, 1, NULL);
/*
..... Control Area: LABEL: OPTION_MENU(Prefix or Subscript) TEXTBOX(Default)
*/
	control_form = XtVaCreateWidget ("form1", xmFormWidgetClass, pane, 
				XmNfractionBase, num_geo,
				NULL);

	prefix_lab = XmStringCreateSimple("prefix");
	subscr_lab = XmStringCreateSimple("subscript");
	for (i = 0; i < num_geo; i++)
	{
		lab_str = XmStringCreateSimple(names[i]);
		XtVaCreateManagedWidget("  ", 
				xmLabelWidgetClass, 	control_form,
				XmNleftAttachment,   XmATTACH_POSITION,
				XmNleftPosition,     1,
            XmNrightAttachment,  XmATTACH_POSITION,
            XmNrightPosition,    3, 
				XmNtopAttachment,    XmATTACH_POSITION,
				XmNtopPosition,      i,
				XmNbottomAttachment, XmATTACH_POSITION,
				XmNbottomPosition,   i+1,
				XmNlabelString, lab_str, 
				NULL);
		XmStringFree(lab_str);
/*
..... Option menu for switching between prefix and subscript.
..... Set the initial state from global array lab_flag[].  Need to be
..... able to tell the state of all the option menus when user is finished,
..... so I made a callback function uig_change_prefix().  There, read the
..... name of calling option menu.
..... Name of calling option menu set by the sprintf(temp, ...) thing.
..... IT IS NEEDED TO DISTINGUISH BETWEEN ALL THE DIFFERENT MENUS!
.....
..... XmVaCreateSimpleOptionMenu(parent, name, option_label, opt_mnemonic,
.....    button_set, callback, ..., NULL)
.....
..... In this case, option_label = opt_mnemonic = NULL
..... and the button is set by lab_flag[i].  You specify what the buttons
..... are by variable length arguments (different arguments for different
..... types of buttons). 
..... XmVaPUSHBUTTON, label, mnemonic, accelerator, accel_text 
*/
		sprintf (temp, "%d", i);
		lab_flag_widget[i] = XmVaCreateSimpleOptionMenu(control_form,
				temp, NULL, NULL, lab_flag[i], uig_change_prefix, 
				XmVaPUSHBUTTON, prefix_lab, 'P', NULL, NULL,
				XmVaPUSHBUTTON, subscr_lab, 'S', NULL, NULL,
				XmNleftAttachment,	XmATTACH_POSITION,
            XmNleftPosition,     3,
            XmNrightAttachment,  XmATTACH_POSITION,
            XmNrightPosition,    7,
            XmNtopAttachment,    XmATTACH_POSITION,
            XmNtopPosition,      i,
            XmNbottomAttachment, XmATTACH_POSITION,
            XmNbottomPosition,   i+1,
				NULL);
		XtManageChild(lab_flag_widget[i]);
		temp_lab_flag[i] = lab_flag[i];

		geo_lab_widget[i] = XtVaCreateManagedWidget ("", 
				xmTextWidgetClass, 	control_form,
				XmNleftAttachment,   XmATTACH_POSITION,
				XmNleftPosition,     7,
				XmNtopAttachment,    XmATTACH_POSITION,
				XmNtopPosition,      i,
				XmNbottomAttachment, XmATTACH_POSITION,
				XmNbottomPosition,   i+1,
				XmNvalue, geo_lab[i], 
				NULL);
	}
	XmStringFree(prefix_lab); XmStringFree(subscr_lab); 
	XtManageChild(control_form);
/*
..... Action area:  Contains ACCEPT and CANCEL buttons.
*/
	action_form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane, 
				XmNfractionBase, 5, NULL);
	accept_but = XtVaCreateManagedWidget ("ACCEPT", 
				xmPushButtonGadgetClass, action_form,
				XmNtopAttachment,    XmATTACH_FORM, 
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment,   XmATTACH_POSITION,
				XmNleftPosition,     1,
				XmNrightAttachment,  XmATTACH_POSITION,
				XmNrightPosition,    2,
				XmNshowAsDefault,    True,
				NULL);
	XtAddCallback (accept_but, XmNactivateCallback, 
					uig_name_configCB,name_modal_dialog);

	cancel_but = XtVaCreateManagedWidget ("CANCEL", 
				xmPushButtonGadgetClass, action_form,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_POSITION,
            XmNleftPosition,     3,
            XmNrightAttachment,  XmATTACH_POSITION,
            XmNrightPosition,    4,
            NULL);
	XtAddCallback (cancel_but, XmNactivateCallback, 
					uig_destroy_names, name_modal_dialog);
	
	XtManageChild (action_form);
	XtManageChild (pane);
	XtPopup (name_modal_dialog, XtGrabNone);
/*
..... End of Name Modals form.
*/            
}

/*********************************************************************
**    I_FUNCTION     :  uig_change_prefix(widget, client_data, call_data)
**
**       This function doesn't do anything for now.  Everything after
**       this is legacy stuff.  IGNORE IT! 
**
**			Callback function for the option menus in the uig_name_modal
**			form.  Read the menu selection and find out which menu
**			(which geometry type) was changed.  Update the global array
**			lab_flag[].  The widget that is returned is the button on
**			the option menu (option menus can be made of a series of push-
**			buttons called button_1, button_2, button_3, ...).  Need to 
**			get the parent (the option menu widget itself); they are
**			named with a number that represents the geometry type.  Use
**			this info and the state (client_data) to update the array. 
**    PARAMETERS
**       INPUT  :
**				option_but	=	one of the pushbutton widgets that make
**									make up the option menus in uig_name_modal form.
**				client_data	=	pointer to integer that tells which selection
**									in the option menu was selected.
**				call_data	=	ignored
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		AUTHOR		 :	Ed Ames  10 Jan 01
*********************************************************************/
void uig_change_prefix(option_but, client_data, call_data)
Widget option_but;
XtPointer client_data, call_data;
{
	Widget geo_type;
	char *text;
	int i, status = (int) client_data;
/*
..... status now holds the selection in the option menu that was selected.
*/
	

/*
..... Need to find out which option menu made the call.  The different
..... menus were named with a number (0 - 10 for the 11 different geometry
..... types).  Get the parent of the option_but -> the option menu widget.
..... From this, you can get the name.  Use the name to find out which 
..... element of the lab_flag[] array to change.
*/
	geo_type = XtParent(option_but);
	text = XtName (geo_type);
	
	i = atoi(text);

/*  
.....  Only set when user hits ACCEPT button.
*/
	temp_lab_flag[i] = status;
}


/*********************************************************************
**    I_FUNCTION     :  uig_destroy_names(widget, client_data, call_data)
**			Callback function for the CANCEL button in the uig_name_modal
**			Motif form.  Destroy the form without changing anything.
**    PARAMETERS
**       INPUT  :
**				widget		=	iges_name_model form to be destroyed	
**				client_data	=	ignored
**				call_data	=	ignored	
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		AUTHOR		 : Ed Ames  10 Jan 01
*********************************************************************/
void uig_destroy_names(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;
{
	Widget shell = (Widget) client_data;

	XtDestroyWidget (shell);
}

/*********************************************************************
**    I_FUNCTION     :  uig_name_configCB(widget, client_data, call_data)
**			Callback function for the ACCEPT button on the uig_name_modal
**			Motif form.  Read in info from global array of textbox widgets
**			that are displayed in the form.  This is to find out what
**			abbreviations the user wants for the geometry (ex. pt for points).
**			The abbreviations must be between 2 and 6 characters.  Pass 
**			the global array lab_flag[] and the geometry labels to a
**			function to update the INIT file containing the defaults.
**			Next, modify the global data structure UM_labelmdl.
**			Finally, destroy the form. 
**    PARAMETERS
**       INPUT  :
**				widget		=	ignored	
**				client_data	=	name_modal_form -> so it can be destroyed
**									at the end.
**				call_data	=	ignored
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : 
**			Changes the global default naming modal convention by 
**			changing UM_labelmdl 
**    WARNINGS     : none
**		AUTHOR		 : Ed Ames  10 Jan 01
*********************************************************************/
void uig_name_configCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;
{
	int i, n, len, do_error_dialog;
	static int relnum[] = {UM_POINT_REL, NCL_POINTVEC_REL, UM_LINE_REL,
      NCL_VECTOR_REL, NCL_PLN_REL, UM_CIRCLE_REL, UM_RBSPLCRV_REL, 
		UM_RBSPLSRF_REL, NCL_SHAPE_REL, NCL_MATRIX_REL, NCL_PATERN_REL};
	char *text, err_str[48];
	XmString err_message;
	Arg args[5];
	Widget err_dialog, name_modals_dialog = (Widget) client_data;
	
/*
..... Get the abbreviations that the user wants to use by getting the 
..... text from the global array of textbox widgets that are used in the
..... uig_name_modal form.  Check to make sure that they are between 2
..... and 6 characters long.
*/
	for (i = 0; i < 11; i++)
	{
/*
		j = 0;
		XtSetArg(args[j], XmNbuttonSet, &lab_flag[i]); j++;
		XtGetValues( lab_flag_widget[i], args, j);
*/	
		lab_flag[i] = temp_lab_flag[i];

		text = XmTextGetString(geo_lab_widget[i]);
		len = strlen(text);
		do_error_dialog = 0;
		if ((lab_flag[i] == 0) && (len != 2))
		{
			strcpy (err_str, "Prefixes must be 2 characters!");
			err_message = XmStringCreateSimple(err_str);
			do_error_dialog = 1;
		}
		if ((lab_flag[i] == 1) && (len > 6))
		{
			strcpy (err_str, "Labels for subscripting must be <= 6 characters!");
			err_message = XmStringCreateSimple(err_str);
			do_error_dialog = 1;
		}
		if (do_error_dialog == 1)
		{
			n = 0;	
			XtSetArg (args[n], XmNmessageString, err_message); n++;
			err_dialog = (Widget)XmCreateErrorDialog(name_modals_dialog, 
								"Abbreviations Too Long ", args, n);
			XmStringFree(err_message);
			XtManageChild(err_dialog);
			XtPopup (XtParent(err_dialog), XtGrabNone);
			
			return;
		}
		
		ul_to_upper(text);
		strcpy (geo_lab[i], text);
		XtFree(text);

		um_update_rel_label (relnum[i], geo_lab[i], lab_flag[i]);
	}

	XtDestroyWidget (name_modals_dialog);
}

/*********************************************************************
**    I_FUNCTION     : uig_translate_filter_form(OptionFrm, client_data,\
**                                              call_data)
**
**       Makes form to handle selective filtering of geometry types from
**       IGES to unibase.  The initial values are read in from a
**       global array of flags, entity_mask.  This array is set from
**       the NCLIGES modal file (pointed to by the environmental variable
**       UL_IGES_MODALS).  If the mask is set to 1 (entity_mask[i] == 1),
**       set the togglebutton as checked; this means that NCLIGES should
**       translate that type of entity.  If the mask is equal to 0, do
**       not translate and the togglebutton should be unchecked.
**
**       Each of the togglebuttons has the number in which it was created
**       in the name.  This allows for easy processing. 
**
**    PARAMETERS
**       INPUT  :
**				OptionFrm	=	ignored
**				client_data	=	ignored
**				call_data	=	ignored
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		AUTHOR		 :	Ed Ames   Jan 01
*********************************************************************/
void uig_translate_filter_form(OptionFrm, client_data, call_data)
Widget OptionFrm;
XtPointer client_data;
XtPointer call_data;
{
	int i;
	static char *names [37] = {
            "CIRCULAR ARC(100)","COMPOSITE CURVE(102)","CONIC(104)",
            "POLY2D(106/1,11)","POINT PATTERN(106/2)","PT VEC PATTERN(106/3)",
            "PLANE(108)","LINE(110)",
            "PARAM SPLINE CV(112)","PARAM SPLINE SURF(114)","POINT(116)",
            "RULED SURF(118)","SURF OF REV(120)","TABULATED CYL(122)",
            "NURB SPLINE CV(126)","NURB SURF(128)",
            "OFFSET SURF(140)","BOUNDED CRV(141)","SURF CURVE(142)",
            "BOUNDED SURF(143)","TRIMMED SURF(144)","ANGULAR DIM(202)",
            "DIAMETER DIM(206)","LABEL(210)","NOTE(212)","ARROW(214)",
            "LINEAR DIM(216)","RADIUS DIM(222)","SYMBOL(228)",
            "GROUP(402)","DRAWING(404)","NAME(406)","SUBFIG INSTANCE(408)",
            "VIEW(410)","VERTEX(502)","ASSOCIATION(602)","POLY CV(106/12,13)"};
	Widget entity_filter_dialog, pane, control_form, action_form;
	Widget all_on_but, all_off_but, accept_but, cancel_but;
	Widget sfcvframe_widget, sfcvlab_widget;
	Widget sfcvopts_form;
	extern void uig_destroy_names(), all_on(), all_off(), filter_entities();
	extern void set_entity(), sfcvCB();
	XmString button1, button2, button3;

	UIG_splitccrv_wrk = UIG_splitccrv;

	entity_filter_dialog = XtVaCreatePopupShell("Entity Translation", 
            xmDialogShellWidgetClass, XtParent(OptionFrm),
            XmNdeleteResponse, XmDESTROY, 
            NULL);

	pane = XtVaCreateWidget ("pane", xmPanedWindowWidgetClass, 
				entity_filter_dialog, XmNsashWidth, 1, XmNsashHeight, 1, NULL);
/*
..... Control Area: 
*/
	control_form = XtVaCreateWidget ("control_form", 
					xmRowColumnWidgetClass, pane, 
					XmNpacking, XmPACK_COLUMN,
					XmNnumColumns, 3,
					NULL);
	for (i = 0; i < XtNumber(names); i++)
	{
		entity_widget[i] = XtVaCreateManagedWidget(names[i], 
					xmToggleButtonWidgetClass, control_form,
					NULL);
      if (entity_mask[i] == 1)
         XmToggleButtonSetState(entity_widget[i], True, False);
      else
         XmToggleButtonSetState(entity_widget[i], False, False);
		XtAddCallback (entity_widget[i], XmNvalueChangedCallback, 
					set_entity, (XtPointer) i);
	}
	XtManageChild(control_form);
/*
.....Create surface curve translation options.
*/
	sfcvframe_widget = XtVaCreateManagedWidget("SurfCurveOptions",
		xmFrameWidgetClass, pane,
		NULL);
#if UU_COMP != UU_HPUX
	sfcvlab_widget = XtVaCreateManagedWidget("Surface Curve Translation",
		xmLabelWidgetClass, sfcvframe_widget,
		XmNchildType,XmFRAME_TITLE_CHILD,
		XmNalignment,XmALIGNMENT_BEGINNING,
		NULL);
#endif
	button1 = XmStringCreateSimple("Curve Only");
	button2 = XmStringCreateSimple("Components Only");
	button3 = XmStringCreateSimple("Curve and Components   ");
	sfcvopts_form = XtVaCreateManagedWidget("sfcvopts_form", xmFormWidgetClass,
							sfcvframe_widget, NULL);
	sfcvopts_widget = XmVaCreateSimpleRadioBox (sfcvopts_form, 
				"sfcv Radio Buttons",
				UIG_splitccrv,
				sfcvCB,
				XmVaRADIOBUTTON, button1, NULL, NULL, NULL,
				XmVaRADIOBUTTON, button2, NULL, NULL, NULL,
				XmVaRADIOBUTTON, button3, NULL, NULL, NULL,
				XmNnumColumns, (short)3,
				NULL);
	XmStringFree(button1);
	XmStringFree(button2);
	XmStringFree(button3);
	XtManageChild(sfcvopts_widget);
/*
..... Action area:  Contains ALL ON, ALL OFF, ACCEPT, and CANCEL buttons.
*/
	action_form = XtVaCreateWidget ("action_form", xmFormWidgetClass, pane, 
				XmNfractionBase, 5, NULL);
	all_on_but = XtVaCreateManagedWidget ("All On", 
				xmPushButtonGadgetClass, action_form,
				XmNtopAttachment,    XmATTACH_FORM, 
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment,   XmATTACH_POSITION,
				XmNleftPosition,     0,
				XmNrightAttachment,  XmATTACH_POSITION,
				XmNrightPosition,    1,
				XmNshowAsDefault,    True,
				NULL);
	XtAddCallback (all_on_but, XmNactivateCallback, 
					all_on,entity_filter_dialog);
	all_off_but = XtVaCreateManagedWidget ("All Off", 
				xmPushButtonGadgetClass, action_form,
				XmNtopAttachment,    XmATTACH_FORM, 
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment,   XmATTACH_POSITION,
				XmNleftPosition,     1,
				XmNrightAttachment,  XmATTACH_POSITION,
				XmNrightPosition,    2,
				XmNshowAsDefault,    True,
				NULL);
	XtAddCallback (all_off_but, XmNactivateCallback, 
					all_off,entity_filter_dialog);

	accept_but = XtVaCreateManagedWidget ("ACCEPT", 
				xmPushButtonGadgetClass, action_form,
				XmNtopAttachment,    XmATTACH_FORM, 
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment,   XmATTACH_POSITION,
				XmNleftPosition,     3,
				XmNrightAttachment,  XmATTACH_POSITION,
				XmNrightPosition,    4,
				XmNshowAsDefault,    True,
				NULL);
	XtAddCallback (accept_but, XmNactivateCallback, 
					filter_entities,entity_filter_dialog);
	cancel_but = XtVaCreateManagedWidget ("CANCEL", 
				xmPushButtonGadgetClass, action_form,
            XmNtopAttachment,    XmATTACH_FORM,
            XmNbottomAttachment, XmATTACH_FORM,
            XmNleftAttachment,   XmATTACH_POSITION,
            XmNleftPosition,     4,
            XmNrightAttachment,  XmATTACH_POSITION,
            XmNrightPosition,    5,
				XmNshowAsDefault,    True,
            NULL);
	XtAddCallback (cancel_but, XmNactivateCallback, 
					uig_destroy_names, entity_filter_dialog);
	
	XtManageChild (action_form);
	XtManageChild (pane);
	XtPopup (entity_filter_dialog, XtGrabNone);
/*
..... End of Name Modals form.
*/            
}

/*********************************************************************
**    I_FUNCTION     :  set_entity(widget, client_data, call_data)
**
**       Ignore this for now.  It is just here to give Motif a 
**       callback function.
**
**    PARAMETERS
**       INPUT  :
**				widget		=	ignored	
**				client_data	=	
**				call_data	=	
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		AUTHOR		 : Ed Ames  
*********************************************************************/
void set_entity(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;
{
	int state;
	int i = (int) client_data;
	XmToggleButtonCallbackStruct *toggle_data = 
			(XmToggleButtonCallbackStruct *) call_data;

	if (i==18)
	{
		state = toggle_data->set;
		XtSetSensitive(sfcvopts_widget, state);
	}
}

/*********************************************************************
**    I_FUNCTION     :  filter_entities(widget, client_data, call_data)
**
**       The callback function for the ACCEPT button on the filter
**       entities dialog.  This dialog is called from the Options
**       dialog which is in turn called from the main form.
**
**       Read the state of each of the togglebutton widgets and
**       update the corresponding entry in the global data structure
**       entity_mask[].  Each one of these represent a type of entity
**       that could be filtered out of the translation from iges to
**       unibase by NCLIGES.  If the togglebutton is checked, translate
**       that type of entity; however, if it is not checked, filter
**       the entity out of all translations.  For example, points are
**       checked, but trimmed surfaces is not checked.  Therefore,
**       translate all points but ignore any trimmed surfaces.
**
**    PARAMETERS
**       INPUT  :
**				widget		=	
**				client_data	=	ignored
**				call_data	=	ignored	
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : modifies global data structure entity_mask[] 
**    WARNINGS     : none
**		AUTHOR		 : Ed Ames  
*********************************************************************/
void filter_entities(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;
{
	Widget shell = (Widget) client_data;
	int i;

	for (i = 0; i < XtNumber(entity_widget); i++)
		entity_mask[i] = XmToggleButtonGetState(entity_widget[i]);

	UIG_splitccrv = UIG_splitccrv_wrk;

	XtDestroyWidget (shell);
}

/*********************************************************************
**    I_FUNCTION     :  all_on(widget, client_data, call_data)
**
**       Callback function for the all_on button on the entity 
**       filtering dialog.  This form is called from the Options
**       dialog which is in turn called from the main NCLIGES form.
**
**       Check all the togglebuttons on the form. 
**
**    PARAMETERS
**       INPUT  :
**				widget		=	
**				client_data	=	ignored
**				call_data	=	ignored	
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		AUTHOR		 : Ed Ames  
*********************************************************************/
void all_on(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;
{
	int i;

	for (i = 0; i < XtNumber(entity_widget); i++)
		XmToggleButtonSetState(entity_widget[i], True, False);

	XtSetSensitive(sfcvopts_widget, True);
}
/*********************************************************************
**    I_FUNCTION     :  all_off(widget, client_data, call_data)
**
**       Callback function for the all_off button on the entity 
**       filtering dialog.  This form is called from the Options
**       dialog which is in turn called from the main NCLIGES form.
**
**       Uncheck all the togglebuttons on the form. 
**
**    PARAMETERS
**       INPUT  :
**				widget		=	
**				client_data	=	ignored
**				call_data	=	ignored	
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		AUTHOR		 : Ed Ames  
*********************************************************************/
void all_off(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;
{
	int i;

	for (i = 0; i < XtNumber(entity_widget); i++)
		XmToggleButtonSetState(entity_widget[i], False, False);

	XtSetSensitive(sfcvopts_widget, False);
}
/*********************************************************************
**    I_FUNCTION     :  no_dups(widget, client_data, call_data)
**
**       Callback function for the no_dups toggle button on the options
**		 dialog.  This form is called from the main NCLIGES form.
**
**       Activate the match tolerance field
**
**    PARAMETERS
**       INPUT  :
**				widget		=	
**				client_data	=	ignored
**				call_data	=	ignored	
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		AUTHOR		 : Ed Ames  
*********************************************************************/
void no_dups(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;
{

	int state;
	XmToggleButtonCallbackStruct *toggle_data = 
			(XmToggleButtonCallbackStruct *) call_data;
	state = toggle_data->set;
	if (state)UIG_nodups = UU_TRUE;
	else UIG_nodups = UU_FALSE;
	if (UIG_lab_opt !=3 )
	{
		XtSetSensitive(label_opt[4],state);
		XtSetSensitive(label_opt[5],state);
	}

  		 

}
/*********************************************************************
**    I_FUNCTION     :  sfcvCB(widget, client_data, call_data)
**
**       Callback function for the surface curve options radio buttons
**       on the entity filtering dialog.
**
**    PARAMETERS
**       INPUT  :
**				widget		=	
**				client_data	=	ignored
**				call_data	=	ignored	
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void sfcvCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;
{
	int i = (int) client_data;
	XmToggleButtonCallbackStruct *toggle_data = 
			(XmToggleButtonCallbackStruct *) call_data;
	if (toggle_data->set) UIG_splitccrv_wrk = i;
}

/*********************************************************************
**    I_FUNCTION     :  iges_close_process_win()
**         Hide the processbar window
**    PARAMETERS
**       INPUT  :
**				None
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
iges_close_process_win()
{
	XtUnmanageChild(m_process_label1); 
	XtUnmanageChild(m_process_bar); 
	XtUnmanageChild(m_process_label2); 
	XtManageChild(mform_item[21]);
}
/*********************************************************************
**    E_FUNCTION : iges_mf_xflush_win()
**
**    DESCRIPTION:
**        execute any impeding expose events on the input queue.
**
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void iges_mf_xflush_win()
{
	int i;
	XEvent x_event;
/*
.....Flush all expose and events
*/
/*
   i = XPending(XtDisplay(TOOL_Parent));
	if (i > 0)
	{
		while(XCheckTypedEvent(XtDisplay(TOOL_Parent), Expose,&x_event))
		{
			XtDispatchEvent(&x_event);
		}
	}
*/
	while (XPending(XtDisplay(TOOL_Parent)) != 0 )
	{
		XNextEvent(XtDisplay(TOOL_Parent), &x_event);
		switch(x_event.type)
		{
			case Expose:
				XtDispatchEvent(&x_event);
		}
	}
	return;
}

/*********************************************************************
**    I_FUNCTION     :  iges_open_process_win()
**         show the processbar window
**    PARAMETERS
**       INPUT  :
**				None
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
iges_open_process_win()
{
	XtUnmanageChild(mform_item[21]); 
	XtManageChild(m_process_label1);
	XtManageChild(m_process_bar);
	XtManageChild(m_process_label2);
	iges_mf_xflush_win();
}
/*********************************************************************
**    I_FUNCTION     :  iges_set_procpos()
**         set the position of the processbar
**    PARAMETERS
**       INPUT  :
**				None
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
iges_set_procpos(pos)
int pos;
{
	m_percent = pos;
}
/*********************************************************************
**    I_FUNCTION     :  iges_set_process_lab(label1, label2)
**         set the label of the processbar
**    PARAMETERS
**       INPUT  : char *label1: label before processor
**						char *label2: label after processor	
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
iges_set_process_lab(label1, label2)
char *label1, *label2;
{
	XmString lstr1, lstr2;
	lstr1 = XmStringCreateSimple(label1);
	XtVaSetValues(m_process_label1,
								XmNlabelString, lstr1, NULL);
	XmStringFree(lstr1);
	lstr2 = XmStringCreateSimple(label2);
	XtVaSetValues(m_process_label2,
								XmNlabelString, lstr2, NULL);
	XmStringFree(lstr2);
	iges_mf_xflush_win();
}
/***********************************************************************
c
c   SUBROUTINE:  iges_disply_as_percent
c
c   FUNCTION:  This function display process as percent num
c
c   INPUT:  num: percentage to display
c
c   OUTPUT: none
c
c***********************************************************************
*/
void iges_disply_as_percent(num)
int num;
{
	static int save_per = -1;
	if (num-m_percent>2)
	{
		m_percent = num;
		drawCB(m_process_bar, NULL, NULL);
	}
}

#endif

/*********************************************************************
**    I_FUNCTION     :  exchange(a, b)
*          Exchange value a and b
**    PARAMETERS
**       INPUT  :
**          a, b
**       OUTPUT :
**          a, b
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int exchange(a, b)
int* a, *b;
{
	int tmp;
	tmp = *a;
	*a = *b;
	*b = tmp;
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  uig_bubble_sort(array, size)
**         Bubble sort to sort an integer array in ascending order.
**    PARAMETERS
**       INPUT  :
**          array
**				size
**       OUTPUT :
**          array
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_bubble_sort(array, size)
int *array;
int size;
{
	int i, j, done=0;
/*
.....just use bubble sort
*/
	for (j=0; j<size-1 && !done; j++)
	{
		done = 1;
		for (i=0; i<size-1; i++)
		{
			if (array[i]>array[i+1])
			{
				exchange(&(array[i]), &(array[i+1]));
				done = 0;
			}
		}
	}
	return 0;
}
