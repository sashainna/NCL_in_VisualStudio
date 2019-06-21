#include "usysdef.h"
#if UU_COMP != UU_WIN2K

/*********************************************************************
**  NAME:  wsmfform.c
**
**		CONTAINS:
**			uw_mfform(fstruct,fdata)
**			uw_mfform_adjust(fstruct)
**...change this function name to ud_formstr_format
**...and put in d4meth.c file
**...removed			uw_mfform_format(typ,data,prec,len,buf)
**			uw_mfform_ckdata(fno,buf)
**			uw_mfcreate_action(parent,actions,num_actions)
**			uw_mfform_redisplay()
**			uw_mfform_close(frmid)
**			uw_mfform_focusCB(widget, client_data, call_data)
**			uw_mfform_activateCB(widget, client_data, call_data)
**			uw_mfform_pickCB(widget, client_data, call_data)
**			uw_mfform_okCB(widget, client_data, call_data)
**			uw_mfform_cancelCB(widget, client_data, call_data)
... added for playback
... Yurong
**			uw_mfform_playb()
**			setState(toggle,value,state)
**			getState(toggle,value,state)
**			uw_mfset_frmpocket()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsmfform.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:10
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:Xlib.h>
#include <decw$include:cursorfont.h>
#include <decw$include:keysym.h>
#include <decw$include:Xm.h>
#include <decw$include:PushB.h>
#include <decw$include:PushBG.h>
#include <decw$include:CascadeBG.h>
#include <decw$include:DialogS.h>
#include <decw$include:PanedW.h>
#include <decw$include:Form.h>
#include <decw$include:Text.h>
#include <decw$include:ToggleB.h>
#include <decw$include:MwmUtil.h>
#include <decw$include:Protocols.h>
#include <decw$include:SashP.h>
#include <decw$include:Label.h>
#include <decw$include:Frame.h>
#include <decw$include:LabelG.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeBG.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/MwmUtil.h>
#include <Xm/Protocols.h>
#include <Xm/SashP.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#endif

#include "gtbl.h"
#include "ws.h"
#include "wsxw.h"
#include "wsmf.h"
#include "dasnog.h"
#include "dinput.h"
#include "dmotif.h"
#include "ddef.h"
#include "dtypes.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfmracs.h"
#include "lcom.h"
#include "xfsys1.h"
#include "xenv1.h"
#include <math.h>
#include "zkeysym.h"
#include "mdcpln.h"

#define MAX_DISPLAY_FRM 50
#define MAX_CHOICE	50
extern int XBorder[2],YBorder[2];
/*
.....Global variable definitions
*/
extern int NCL_nopick_cnt;
/*
.....added by Yurong
.....8/27/97
*/
extern UWS_MF uw_mf;
extern UWS_MFLAYOUT uw_mflayout;
extern int UD_form_bypick;
extern UD_METHOD UD_initfrm_intry;  

extern UD_FSTRUCT *UD_dispfrm[50];
extern UD_FDATA *UD_dispfdata[50];
int formActive=False,formField;
int formOn=False;
static Widget actionBut[10];
void uw_mfadj_frmpos2();
/*
.....added initialize method
.....Yurong 5/3/00
*/

Widget formInpWin=NULL;
static int formActFrm = -1;
static int save_formActFrm = -1;
static int save_formField = -1;
/*
.....current_frmid is for modal form (request and wait for input)
.....display form start from 1
*/
static int current_frmid = -1;
static int active_frmid = -1;
static Widget UW_display_frm[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };

static int m_frmid[MAX_DISPLAY_FRM];
static int m_parentID[MAX_DISPLAY_FRM];
static int m_childnum[MAX_DISPLAY_FRM];
static int m_childfrm[MAX_DISPLAY_FRM][60];

static Widget *m_formFrame[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget *m_formForm[MAX_DISPLAY_FRM] = 
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget *m_frametitle[MAX_DISPLAY_FRM] = 
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget *m_formEntry[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget  **m_formChoice[MAX_DISPLAY_FRM] = 
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget *m_formPrompt[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget *m_FormDisp[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget *m_list[MAX_DISPLAY_FRM] = 
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget *m_textPrompt[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget *m_formMenu[MAX_DISPLAY_FRM] = 
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget *m_draw_area[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };

static Widget m_winid[MAX_DISPLAY_FRM],
					m_pane[MAX_DISPLAY_FRM];

static GC *m_Draw_gc[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static int *m_Draw_Width[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static int *m_WC_Width[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static int *m_Draw_Height[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static int *m_WC_Height[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static int **m_draw_array[MAX_DISPLAY_FRM];

static Widget *m_process_bar[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static Widget *m_process_label[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static GC *m_process_gc[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static int *m_percent[MAX_DISPLAY_FRM] =
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
static int UW_first_resize[MAX_DISPLAY_FRM] = 
	{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

static UD_LIST Sflist[200];
static int Sflist_num = 0;
static int m_formEof[MAX_DISPLAY_FRM], m_Nps[MAX_DISPLAY_FRM];
static UD_FSTRUCT m_fStruct[MAX_DISPLAY_FRM];
static UD_FDATA m_fData[MAX_DISPLAY_FRM];
static char UW_frmfont_name[256];
static int fontget_first = 1;

void uw_mfform_pickCB(),uw_mfform_okCB(), uw_mfform_helpCB();
void uw_mfform_cancelCB(),uw_mfform_activateCB(),uw_mfform_focusCB(),
		uw_mfform_editdoneCB();
void uw_mflistCB();
void uw_mflistCB2();
void uw_mfform_close();
void uw_mffrm_setlist();
void uw_mfget_frmfield();
void uw_mftxt_next();
void uw_mfclose_dispfrm();
void uw_mffree_formdata();
void uw_mffrm_setlist1();
void uw_mffrm_setlist2();
void uw_mfadj_frmfld2();
void uw_mfadj_frmfld3();

static void ButtonEventHandler();
static void uw_mfform_textretCB();
static void drawCB();
static void drawCB2();
static UU_LOGICAL Sfdisplay=UU_FALSE;

int UW_Form_pocket = 0, UW_Form_help = 0;

typedef struct
{
	int frmid, type,fld,chc;
} Pstruct;

static Pstruct current_field;
Pstruct **m_pStruct[MAX_DISPLAY_FRM] = 
	{ NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
	NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };

typedef struct
{
	char *label;
	void (*callback)();
	XtPointer data;
} ActionAreaItem;

static ActionAreaItem actionList[] =
{
		{"ACCEPT",uw_mfform_okCB, NULL},
		{"CANCEL",uw_mfform_cancelCB, NULL},
		{"HELP",uw_mfform_helpCB, NULL}
};

static ActionAreaItem dactionList[] =
{
		{"CLOSE",uw_mfform_cancelCB, NULL},
		{"HELP",uw_mfform_helpCB, NULL}
};

static void setState();
static void getState();
static int active_number;
/*
.....Arrow length and angle
*/
int Arrow_len = 10;
double Arrow_angle = M_PI/8;
/*
.....text position is little off with PC's data
*/
int Text_offy = 8;
float uw_form_scalex;
float uw_form_scaley;
extern UZ_keytable UZ_form_hotkey[3];
extern int UZ_form_hotkey_inx[3];

typedef struct
{
	int type;
	int event;
	int frmid;
	int fldno;
} UW_frm_shortcut;
static UW_frm_shortcut UW_form_acclkey[200];
static int UW_form_acclnum = 0;
static void execute_frmcmd();


void uw_get_rgb (color_str,current_color) 
char *color_str;
XColor *current_color;
{
	if (stricmp (color_str, "WHITE")==0)
	{
		current_color->red = 255*255;
		current_color->green = 255*255;
		current_color->blue = 255*255;
	}
	else if (stricmp (color_str, "YELLOW")==0)
	{
		current_color->red = 255*255;
		current_color->green = 255*255;
		current_color->blue = 0;
	}
	else if (stricmp (color_str, "BLUE")==0)
	{
		current_color->red = 0;
		current_color->green = 0;
		current_color->blue = 255*255;
	}
	else if (stricmp (color_str, "RED")==0)
	{
		current_color->red = 255*255;
		current_color->green = 0;
		current_color->blue = 0;
	}
	else if (stricmp (color_str, "GREEN")==0)
	{
		current_color->red = 0;
		current_color->green = 255*255;
		current_color->blue = 0;
	}
	else if (stricmp (color_str, "MAGENTA")==0)
	{
		current_color->red = 255*255;
		current_color->green = 0;
		current_color->blue = 255*255;
	}
	else if (stricmp (color_str, "CYAN")==0)
	{
		current_color->red = 0;
		current_color->green = 255*255;
		current_color->blue = 255*255;
	}
	else if (stricmp (color_str, "BLACK")==0)
	{
		current_color->red = 0;
		current_color->green = 0;
		current_color->blue = 0;
	}
	else if (stricmp (color_str, "BROWN")==0)
	{
		current_color->red = 184*255;
		current_color->green = 134*255;
		current_color->blue = 11*255;
	}
	else if (stricmp (color_str, "TAN")==0)
	{
		current_color->red = 210*255;
		current_color->green = 180*255;
		current_color->blue = 140*255;
	}
	else if (stricmp (color_str, "LTBLUE")==0)
	{
		current_color->red = 173*255;
		current_color->green = 216*255;
		current_color->blue = 230*255;
	}
	else if (stricmp (color_str, "SEAGREEN")==0)
	{
		current_color->red = 84*255;
		current_color->green = 255*255;
		current_color->blue = 159*255;
	}
	else if (stricmp (color_str, "ORANGE")==0)
	{
		current_color->red = 255*255;
		current_color->green = 165*255;
		current_color->blue = 0;
	}
	else if (stricmp (color_str, "PINK")==0)
	{
		current_color->red = 255*255;
		current_color->green = 195*255;
		current_color->blue = 203*255;
	}
	else if (stricmp (color_str, "PURPLE")==0)
	{
		current_color->red = 221*255;
		current_color->green = 160*255;
		current_color->blue = 221*255;
	}
	else if (stricmp (color_str, "GREY")==0)
	{
		current_color->red = 192*255;
		current_color->green = 192*255;
		current_color->blue = 192*255;
	}
}

int uw_mfaccel_key(x_event)
XEvent *x_event;
{
	int i, event;
	int irtn,rebound,ibuf;
	XKeyPressedEvent *kevent;
	char buffer[100], buf[256];
	KeySym keysym;
	char *index;
	
	index = buf;
	if (formActFrm==-1)
		return 0;
	if (x_event->type!=KeyPress)
		return 0;
	kevent = (XKeyPressedEvent *)x_event;
	rebound = XLookupString(kevent, buffer, sizeof(buffer),
			&keysym, NULL);
	ibuf = keysym;
	irtn = 0;
	event = -1;
/*
.........Pause/Scroll Lock
*/
	if (keysym == XK_Pause || keysym == XK_Scroll_Lock)
	{
		irtn = 2;
		event = keysym - XK_Pause + 1;
	}
	else if (keysym == XK_Multi_key)
	{
		irtn = 2;
		event = keysym - XK_Multi_key + 3;
	}
/*
.........Cursor function keys
*/
	else if (keysym == XK_Home)
	{
		irtn = 2;
		event = keysym - XK_Home + 4;
	}
	else if (keysym >= XK_Prior && keysym <= XK_End)
	{
		irtn = 2;
		event = keysym - XK_Prior + 5;
	}
/*
.........Miscellaneous function keys
*/
	else if (keysym >= XK_Select && keysym <= XK_Insert)
	{
		irtn = 2;
		event = keysym - XK_Select + 8;
	}
	else if (keysym == XK_Undo)
	{
		irtn = 2;
		event = keysym - XK_Undo + 12;
	}
	else if (keysym >= XK_Find && keysym <= XK_Break)
	{
		irtn = 2;
		event = keysym - XK_Find + 13;
	}
	else if (keysym == XK_Num_Lock)
	{
		irtn = 2;
		event = keysym - XK_Num_Lock + 17;
	}
/*
.........Keypad function keys
*/
	else if (keysym == XK_KP_Space)
	{
		irtn = 2;
		event = keysym - XK_KP_Space + 18;
	}
	else if (keysym == XK_KP_Tab)
	{
		irtn = 2;
		event = keysym - XK_KP_Tab + 19;
	}
	else if (keysym == XK_KP_Enter)
	{
		irtn = 2;
		event = keysym - XK_KP_Enter + 20;
	}
	else if (keysym >= XK_KP_F1 && keysym <= XK_KP_F4)
	{
		irtn = 2;
		event = keysym - XK_KP_F1 + 21;
	}
	else if (keysym == XK_KP_Equal)
	{
		irtn = 2;
		event = keysym - XK_KP_Equal + 25;
	}
	else if (keysym >= XK_KP_Multiply && keysym <= XK_KP_9)
	{
		irtn = 2;
		event = keysym - XK_KP_Multiply + 26;
	}
/*
.........Standard function keys (F1-F35)
*/
	else if (keysym >= XK_F1 && keysym <= XK_F35)
	{
		irtn = 2;
		event = keysym - XK_F1 + 42;
	}
/*
.......Controlled Function key
*/
	if (irtn == 2)
	{
		if (kevent->state & ControlMask)
			event = event + NFKEYSYMS * 2;
/*
.......Shifted Function key
*/
		else if (kevent->state & ShiftMask)
			event = event + NFKEYSYMS;
	}
	if (event==UZ_form_hotkey_inx[0]+1)
	{
		uz_user_keydef(UZ_form_hotkey[0], &index,1);
		return 1;
	}
	if (event==UZ_form_hotkey_inx[1]+1)
	{
		uz_user_keydef(UZ_form_hotkey[1], &index,1);
		return 1;
	}
	if (event==UZ_form_hotkey_inx[2]+1)
	{
		uz_user_keydef(UZ_form_hotkey[2], &index,1);
		return 1;
	}
	for (i=0; i<UW_form_acclnum; i++)
	{
		if ((event==UW_form_acclkey[i].event+1)&&
					(UW_form_acclkey[i].frmid==formActFrm))
		{
			if ((formField>0) && 
					((m_fStruct[formActFrm].input_flds[formField].toggle==1)
					|| (m_fStruct[formActFrm].input_flds[formField].toggle==11)))
				return 0;
			else
				execute_frmcmd(UW_form_acclkey[i].frmid, UW_form_acclkey[i].fldno);
			return 1;
		}
	}
	return 0;
}
/**********************************************************************
**    E_FUNCTION :  int uw_mfclose_crrform(flag)
**       Close current Motif Style Form and save the formdata
**       it set long jump flag depond on input flag
**       This function can let other functions close the form
**       other than push cancel/accept buttons. Usually for close
**       a main form and open a subform (because only one form can exist
**       at a time). The flag allows canceling subform without longjump
**		This routine is only for "INPUT type form"
**    PARAMETERS
**       INPUT  :
**				flag: not used, for WinNT only
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS on success.
**    SIDE EFFECTS : Contains own Main App Loop.
**    WARNINGS     : none
*********************************************************************/
uw_mfclose_crrform(flag)
int flag;
{
	int stat;
	if (formOn==True)
/*
......accept form value and close
......Yurong 8/28/97
*/
	{
		stat = uw_mfaccept_formdata(0);
		if (stat)
		{
			uw_mfform_close(0);
		}
	}
	return stat;
}

/**********************************************************************
**    E_FUNCTION :  int uw_mfform_display(fstruct,fdata)
**       Displays a Motif Style Form and not waits for user input.
**				we call it "Display type form"
**    PARAMETERS
**       INPUT  :
**          fstruct	Form structure.
**				fdata		Default form data.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS on success.
**    SIDE EFFECTS : Contains own Main App Loop.
**    WARNINGS     : none
*********************************************************************/
int uw_mfform_display(fstruct,fdata)
UD_FSTRUCT *fstruct;
UD_FDATA *fdata;
{
	return uw_mfform1(fstruct,fdata,1);
}

/**********************************************************************
**    E_FUNCTION :  int uw_mfform(fstruct,fdata)
**       Displays a Motif Style Form and waits for user input.
**				we call it "INPUT type form"
**    PARAMETERS
**       INPUT  :
**          fstruct	Form structure.
**				fdata		Default form data.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS on success.
**    SIDE EFFECTS : Contains own Main App Loop.
**    WARNINGS     : none
*********************************************************************/
int uw_mfform(fstruct,fdata)
UD_FSTRUCT *fstruct;
UD_FDATA *fdata;
{
	return uw_mfform1(fstruct,fdata,0);
}

/**********************************************************************
**    E_FUNCTION :  int uw_mfform1(fstruct,fdata, dflag)
**       Displays a Motif Style Form.
**    PARAMETERS
**       INPUT  :
**          fstruct	Form structure.
**				fdata		Default form data.
**				dflag: 1: display type form, not wait for input
**						 0: "Input" type form, wait for input
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS on success.
**    SIDE EFFECTS : Contains own Main App Loop if dflag=0.
**    WARNINGS     : none
*********************************************************************/
int uw_mfform1(fstruct,fdata, dflag)
UD_FSTRUCT *fstruct;
UD_FDATA *fdata;
int dflag;
{
	int stat, tmpwid, tmphgt, nfram, butlen;
	int temppos, fldno;
	int n,i,j,status,ifl,num_children;
	Arg args[20];
	Widget action_area,*children,uw_mfcreate_action(), pkwin, uw_mfget_pkwin();
	char buf[120];
	XEvent x_event;
	int status1;
	int posx, posy, pposx, pposy, rposx, rposy, hgt, wid;
	Atom watom;
	int disp_vis[200], input_vis[200];
	void uw_mfform_resizeCB();

	fontget_first = 1;
	for (i=0; i<200; i++)
	{
		disp_vis[i] = 0;
		input_vis[i] = 0;
		Sflist[i].num_item = 0;
		Sflist[i].item = NULL;
		Sflist[i].answer = NULL;
	}
	if (dflag==0)
	{
/*
....Form is already active
*/
		if (formOn) goto done;
		current_frmid = 0;
	}
	else
	{
/*
......search for available form
*/
	   for (i=1; i<50; i++)
   	{
      	if (UW_display_frm[i]==NULL)
      	   break;
   	}
   	current_frmid = i;
   	if (current_frmid>=50)
      	return -1;
	}
	m_frmid[current_frmid] = current_frmid;
	for (i=0; i<60; i++)
		m_childfrm[current_frmid][i] = -1;
	m_childnum [current_frmid] = 0;
	UW_form_acclnum = 0;

	current_field.frmid = current_frmid;
	current_field.type = -1;
	current_field.fld = -1;
	current_field.chc = -1;
/*
...record form id
... yurong added
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
	{
		if (dflag==0)
			ud_rpwrform( "NAME", (*fstruct).ud_frmdf.ud_fid, "0");
		else
			ud_rpwrform( "NAME", (*fstruct).ud_frmdf.ud_fid, "1");
	}
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	m_formEof[current_frmid] = False;
/*
.....Playback is active
.....Restore recorded form fields
.....Yurong  -  2/7/97
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)
	{
		status1 = uw_mfform_playb(current_frmid, fstruct,fdata);
		if (status1==0)
		{
			m_formEof[current_frmid] = True;
			if (dflag==0)
			{
				formActive = False;
				formOn = False;
/*
......we don't need display form now but form structure
......is copied from fstruct & fdata (which normally freed 
......in 'Close' routine.
......free form structure & data space
*/
				ud_delform(&(m_fStruct[current_frmid]));
				uu_free(m_fData[current_frmid].ud_data);
			}
		}
		else
			goto form;
	}
	else
	{
	form:
/*
......set up form structure and data
*/
		m_formFrame[current_frmid] =
						(Widget*)uu_malloc((fstruct->n_frame_fields)*sizeof(Widget));
		m_formForm[current_frmid] =
						(Widget*)uu_malloc((fstruct->n_frame_fields)*sizeof(Widget));
		m_frametitle[current_frmid] =
						(Widget*)uu_malloc((fstruct->n_frame_fields)*sizeof(Widget));
		m_formEntry[current_frmid] = 
						(Widget*)uu_malloc((fstruct->n_input_fields)*sizeof(Widget));
		m_FormDisp[current_frmid] = 
					(Widget*)uu_malloc((fstruct->n_display_fields)*sizeof(Widget));
		m_formChoice[current_frmid] = 
					(Widget**)uu_malloc((fstruct->n_input_fields)*sizeof(Widget*));
		for (i=0; i<fstruct->n_input_fields; i++)
		{
			m_formChoice[current_frmid][i] = 
						(Widget*)uu_malloc(MAX_CHOICE*sizeof(Widget));
			for (j=0; j<MAX_CHOICE; j++)
				m_formChoice[current_frmid][i][j] = NULL;
		}
		m_formPrompt[current_frmid] =
						(Widget*)uu_malloc((fstruct->n_input_fields)*sizeof(Widget));
		m_list[current_frmid] =
						(Widget*)uu_malloc((fstruct->n_input_fields)*sizeof(Widget));
		m_textPrompt[current_frmid] =
						(Widget*)uu_malloc((fstruct->n_input_fields)*sizeof(Widget));
		m_formMenu[current_frmid] =
						(Widget*)uu_malloc((fstruct->n_input_fields)*sizeof(Widget));
		m_draw_area[current_frmid] =
						(Widget*)uu_malloc((fstruct->n_input_fields)*sizeof(Widget));
		m_Draw_gc[current_frmid] =
						(GC*)uu_malloc((fstruct->n_input_fields)*sizeof(GC));
		m_Draw_Width[current_frmid] =
						(int *)uu_malloc((fstruct->n_input_fields)*sizeof(int));
		m_WC_Width[current_frmid] =
						(int *)uu_malloc((fstruct->n_input_fields)*sizeof(int));
		m_Draw_Height[current_frmid] =
						(int *)uu_malloc((fstruct->n_input_fields)*sizeof(int));
		m_WC_Height[current_frmid] =
						(int *)uu_malloc((fstruct->n_input_fields)*sizeof(int));
		m_pStruct[current_frmid] = 
						(Pstruct **)uu_malloc (400*sizeof(Pstruct*));
		m_draw_array[current_frmid] = 
						(int **)uu_malloc((fstruct->n_input_fields)*sizeof(int*));

		m_process_bar[current_frmid] =
						(Widget*)uu_malloc((fstruct->n_input_fields)*sizeof(Widget));
		m_process_label[current_frmid] =
						(Widget*)uu_malloc((fstruct->n_input_fields)*sizeof(Widget));
		m_process_gc[current_frmid] =
						(GC*)uu_malloc((fstruct->n_input_fields)*sizeof(GC));
		m_percent[current_frmid] = 
						(int *)uu_malloc((fstruct->n_input_fields)*sizeof(int));

		if (dflag==0)
		{
			UW_Form_pocket = 0;
			UW_Form_help = 0;
		}
		for (i=0; i<fstruct->n_frame_fields; i++)
		{
			m_formFrame[current_frmid][i] = NULL;
			m_formForm[current_frmid][i] = NULL;
			m_frametitle[current_frmid][i] = NULL;
		}
		for (i=0; i<fstruct->n_display_fields; i++)
		{
			m_FormDisp[current_frmid][i] = NULL;
		}
		for (i=0; i<fstruct->n_input_fields; i++)
		{
			m_formEntry[current_frmid][i] = NULL;
			m_formPrompt[current_frmid][i] = NULL;
			m_list[current_frmid][i] = NULL;
			m_formMenu[current_frmid][i] = NULL;
			m_textPrompt[current_frmid][i] = NULL;
			m_draw_area[current_frmid][i] = NULL;
			m_process_label[current_frmid][i] = NULL;
			m_process_bar[current_frmid][i] = NULL;
			m_Draw_gc[current_frmid][i] = NULL;
		}
		m_Nps[current_frmid] = 0;
/*
.....copy structure to local data member
*/
/*
.....We use uu_move_byte to copy structure (include the pointer), so we don't
.....call ud_delform to free the pointer space after this routine now, but we
.....need free there space after we close the form
*/
	   uu_move_byte((char*)fstruct, (char*)&(m_fStruct[current_frmid]), 
							sizeof(UD_FSTRUCT));
		uu_move_byte((char*)fdata, (char*)&(m_fData[current_frmid]), 
							sizeof(UD_FDATA));
		UD_dispfrm[current_frmid] = &(m_fStruct[current_frmid]);
		UD_dispfdata[current_frmid] = &(m_fData[current_frmid]);
/*
......this is just for safe reason, we no longer use UD_frm
......and UD_fdata for form (WIN2K and MOTIF version)
*/
		if (dflag==0)
		{
			ud_setfs(&(m_fStruct[current_frmid]));
			ud_setfdata(&(m_fData[current_frmid]));
		}
/*
.....Forms do not have a Menu button
*/
		n = 0;
/*
		ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
			MWM_DECOR_MINIMIZE;
		XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
		ifl = MWM_FUNC_ALL | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE |
			MWM_FUNC_CLOSE;
		XtSetArg(args[n],XmNmwmFunctions,ifl); n++;
*/
		ifl = MWM_DECOR_MENU | MWM_DECOR_TITLE | MWM_DECOR_RESIZEH;
		XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
		ifl = MWM_FUNC_CLOSE | MWM_FUNC_RESIZE | MWM_FUNC_MOVE;
		XtSetArg(args[n],XmNmwmFunctions,ifl); n++;
/*
......set width and height, in pixel (not rows and cols)
......Yurong 10/11/00
*/
		wid = (fstruct->ud_frmdf).ud_frc.ud_c;
		hgt = (fstruct->ud_frmdf).ud_frc.ud_r;
/*
......always put form in the center of the screen
*/
/****
		posx = (uw_xw.dev_xmax - uw_form_scalex * wid)/2;
		posy = (uw_xw.dev_ymax - uw_form_scaley * hgt)/2;
		XtSetArg(args[n],XmNx, posx); n++;
		XtSetArg(args[n],XmNy, posy); n++;
***/
      tmpwid = uw_form_scalex * wid;
      tmphgt = uw_form_scaley * hgt;
		uw_mfadj_frmpos(&posx, &posy, &tmpwid, &tmphgt, current_frmid);
		posx += XBorder[1];
		posy += YBorder[1];
		
		XtSetArg(args[n],XmNx, posx); n++;
		XtSetArg(args[n],XmNy, posy); n++;
		XtSetArg(args[n],XmNwidth, tmpwid); n++;
		XtSetArg(args[n],XmNheight, tmphgt); n++;
		hgt = hgt - 50/uw_form_scaley;
/*
.....Create Window for Form
*/
		pkwin = (Widget) uw_mfget_pkwin();
		if (pkwin==NULL)
		{
			UW_display_frm[current_frmid] = 
								XtCreatePopupShell("Form",xmDialogShellWidgetClass,
								uw_mf.graphic_app,args,n);
		}
		else
		{
			UW_display_frm[current_frmid] = 
								XtCreatePopupShell("Form",xmDialogShellWidgetClass,
								pkwin,args,n);
		}
		if (UW_display_frm[current_frmid] == NULL) goto failed;
/*
.....Trap the CLOSE button
*/
		watom = XmInternAtom(uw_xw.disp,"WM_DELETE_WINDOW",True);
		XmAddWMProtocolCallback(UW_display_frm[current_frmid], watom,
					uw_mfform_cancelCB, (XtPointer)current_frmid);
		XtAddEventHandler(UW_display_frm[current_frmid], StructureNotifyMask,
				False, uw_mfform_resizeCB, (XtPointer)current_frmid);
/*
......set title
*/
		XtVaSetValues(UW_display_frm[current_frmid],XmNtitle,
				fstruct->ud_frmdf.ud_fid, NULL);
/*
.....Create a paned window to hold the form &
.....Action Area
*/
		m_pane[current_frmid] = XtVaCreateWidget("pane",xmPanedWindowWidgetClass,
										UW_display_frm[current_frmid],
										XmNsashWidth,1,
										XmNsashHeight,1,
										NULL);
/*
.....Create a Form widget
*/
		n = 0;
		sprintf(buf,"%s",fstruct->ud_frmdf.ud_fid);

		tmpwid = uw_form_scalex * wid;
		tmphgt = uw_form_scaley * hgt;

		XtSetArg(args[n],XmNwidth, tmpwid); n++;
		XtSetArg(args[n],XmNheight, tmphgt); n++;

		m_winid[current_frmid] = XtCreateWidget(buf,
			xmFormWidgetClass,m_pane[current_frmid],args,n);
		if (m_winid[current_frmid] == NULL) goto failed;
/*
.....get default form font used
*/
/*
		uw_mfget_winfont(m_winid[current_frmid], UW_frmfont_name);
*/
/*
......Put up Frame first
*/
		for (nfram=0;nfram<fstruct->n_frame_fields;nfram++)
		{
			n = 0;
			XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
			temppos = (fstruct->frame_flds[nfram].y * 100)/hgt;
			XtSetArg(args[n],XmNtopPosition,temppos); n++;
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
			temppos = (fstruct->frame_flds[nfram].x * 100)/wid;
			XtSetArg(args[n],XmNleftPosition,temppos); n++;

			XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
			temppos = ((fstruct->frame_flds[nfram].x + 
								fstruct->frame_flds[nfram].cx)
								* 100)/wid;
			XtSetArg(args[n],XmNrightPosition,temppos); n++;

			XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
			temppos = ((fstruct->frame_flds[nfram].y + 
								fstruct->frame_flds[nfram].cy)
								* 100)/hgt;
			XtSetArg(args[n],XmNbottomPosition,temppos); n++;
			sprintf(buf,"%s",fstruct->frame_flds[nfram].title);
			m_formFrame[current_frmid][nfram] = XtCreateManagedWidget(buf,
											xmFrameWidgetClass, m_winid[current_frmid],
											args,n);
			n = 0;
			XtSetArg(args[n], XmNchildType, XmFRAME_TITLE_CHILD); n++;
			XtSetArg(args[n], XmNchildVerticalAlignment, XmALIGNMENT_CENTER);
			n++;
			m_frametitle[current_frmid][nfram] = XtCreateManagedWidget(buf,
											xmLabelGadgetClass,
											m_formFrame[current_frmid][nfram], args,n);

			n = 0;
			m_formForm[current_frmid][nfram] = XtCreateManagedWidget(buf,
											xmFormWidgetClass,
											m_formFrame[current_frmid][nfram], args,n);
/*
.....Put up Display and input entries into frame
*/
			for (i=0;i<fstruct->n_display_fields;i++)
			{
/*
.....check if this item is inside frame
*/
				if (uw_ifpos_inarea(fstruct->display_flds[i].pos.ud_c, 
										fstruct->display_flds[i].pos.ud_r, 
										fstruct->frame_flds[nfram].x,
										fstruct->frame_flds[nfram].y,
										fstruct->frame_flds[nfram].cx,
										fstruct->frame_flds[nfram].cy))
				{
					posx = ((fstruct->display_flds[i].pos.ud_c -
									fstruct->frame_flds[nfram].x) *100 ) /
									fstruct->frame_flds[nfram].cx;
					posy = ((fstruct->display_flds[i].pos.ud_r -
									fstruct->frame_flds[nfram].y) *100 ) /
									fstruct->frame_flds[nfram].cy;
					stat = uw_putdisp_entry(m_formForm[current_frmid][nfram],
							current_frmid, i, fstruct, fdata, posx, posy);
					if (stat == UU_FAILURE) goto failed;
					disp_vis[i] = 1;
				}
			}
			for (i=0;i<fstruct->n_input_fields;i++)
			{
/*
.....check if this item is inside frame
*/
				if (uw_ifpos_inarea(fstruct->input_flds[i].ud_prmloc.ud_c, 
										fstruct->input_flds[i].ud_prmloc.ud_r, 
										fstruct->frame_flds[nfram].x,
										fstruct->frame_flds[nfram].y,
										fstruct->frame_flds[nfram].cx,
										fstruct->frame_flds[nfram].cy))
				{
					pposx = ((fstruct->input_flds[i].ud_prmloc.ud_c -
									fstruct->frame_flds[nfram].x) *100 ) /
									fstruct->frame_flds[nfram].cx;
					pposy = ((fstruct->input_flds[i].ud_prmloc.ud_r -
									fstruct->frame_flds[nfram].y) *100 ) /
									fstruct->frame_flds[nfram].cy;
/*
.....move little bit in order to line of with othe input field for choicebox
.....if posx is the same, the choicebox normally will have different position
*/
					if (fstruct->input_flds[i].toggle == 2)
					{
						posx = ((fstruct->input_flds[i].ud_prmloc.x -
									fstruct->frame_flds[nfram].x - 5) *100 ) /
									fstruct->frame_flds[nfram].cx;
					}
					else
					{
						posx = ((fstruct->input_flds[i].ud_prmloc.x -
									fstruct->frame_flds[nfram].x) *100 ) /
									fstruct->frame_flds[nfram].cx;
					}	
					posy = ((fstruct->input_flds[i].ud_prmloc.y -
									fstruct->frame_flds[nfram].y) *100 ) /
									fstruct->frame_flds[nfram].cy;
					rposx = ((fstruct->input_flds[i].ud_prmloc.ud_c 
								+ fstruct->input_flds[i].ud_fldloc.ud_c
								- fstruct->frame_flds[nfram].x) *100 ) /
									fstruct->frame_flds[nfram].cx;
					rposy = ((fstruct->input_flds[i].ud_prmloc.ud_r 
								+ fstruct->input_flds[i].ud_fldloc.ud_r
								- fstruct->frame_flds[nfram].y) *100 ) /
									fstruct->frame_flds[nfram].cy;
					stat = uw_putinput_entry(m_formForm[current_frmid][nfram], 
							current_frmid, i, fstruct, fdata, pposx, pposy,
							posx, posy, rposx, rposy,
							fstruct->frame_flds[nfram].cx,
							fstruct->frame_flds[nfram].cy);
					if (stat == UU_FAILURE) goto failed;
					input_vis[i] = 1;
				}
			}
			uw_mfadj_frmfld3(fstruct, current_frmid, nfram);
		}	
/*
.....Put up Display entries that not in the frame
*/
		for (i=0;i<fstruct->n_display_fields;i++)
		{
/*
.....check if this item is displayed
*/
			if (disp_vis[i]==0) 
			{
				posx = (fstruct->display_flds[i].pos.ud_c*100 ) /wid;
				posy = (fstruct->display_flds[i].pos.ud_r*100 ) /hgt; 
				stat = uw_putdisp_entry(m_winid[current_frmid],
							current_frmid, i, fstruct, fdata, posx, posy);
				if (stat == UU_FAILURE) goto failed;
			}
		}
		for (i=0;i<fstruct->n_input_fields;i++)
		{
/*
.....check if this item is displayed
*/
			if (input_vis[i]==0) 
			{
				pposx = (fstruct->input_flds[i].ud_prmloc.ud_c*100 ) /wid;
				pposy = (fstruct->input_flds[i].ud_prmloc.ud_r*100 ) /hgt; 
				if (fstruct->input_flds[i].ud_prmloc.x!=-1)
				{
/*
.....move little bit in order to line of with othe input field for choicebox
.....if posx is the same, the choicebox normally will have different position
*/
					if (fstruct->input_flds[i].toggle == 2)
					{
						posx = ((fstruct->input_flds[i].ud_prmloc.x-5)*100 ) /wid;
					}
					else if (fstruct->input_flds[i].toggle == 1)
					{
						if ((fstruct->input_flds[i].ud_input==FORM_PICK)
								|| (fstruct->input_flds[i].ud_input==FORM_LOCATE))
						{
							butlen = fstruct->input_flds[i].ud_prmloc.x
												- fstruct->input_flds[i].ud_prmloc.ud_c;
							posx = ((fstruct->input_flds[i].ud_prmloc.ud_c + butlen)
										* 100) /wid;
						}
						else
							posx = (fstruct->input_flds[i].ud_prmloc.x*100 ) /wid;
					}
					else
						posx = (fstruct->input_flds[i].ud_prmloc.x*100 ) /wid;
				}
				else if ((fstruct->input_flds[i].toggle == 1)
							&& ( (fstruct->input_flds[i].ud_input==FORM_PICK)
							|| (fstruct->input_flds[i].ud_input==FORM_LOCATE)))
				{
						butlen = strlen(fstruct->input_flds[i].prompt)*4;
						posx = ((fstruct->input_flds[i].ud_prmloc.ud_c + butlen)
									* 100) /wid;
				}
				else
					posx = -1;
				if (fstruct->input_flds[i].ud_prmloc.y==-1)
					posy = -1;
				else
					posy = (fstruct->input_flds[i].ud_prmloc.y*100 ) /hgt; 

				rposx = ((fstruct->input_flds[i].ud_prmloc.ud_c 
							+ fstruct->input_flds[i].ud_fldloc.ud_c)*100 ) /wid;
				rposy = ((fstruct->input_flds[i].ud_prmloc.ud_r 
								+ fstruct->input_flds[i].ud_fldloc.ud_r)*100 ) /hgt;
				stat = uw_putinput_entry(m_winid[current_frmid], 
							current_frmid, i, fstruct, fdata, pposx, pposy,
							posx, posy, rposx, rposy,
							wid, hgt);
				if (stat == UU_FAILURE) goto failed;
			}
		}	
/*
.....Create the Action Area
*/
		if (dflag==0)
		{
			n = XtNumber(actionList);
			for (i=0;i<n;i++) actionList[i].data = 
								(XtPointer)current_frmid;
			if (m_fStruct[current_frmid].helpflag==0)
				action_area = 
					uw_mfcreate_action(m_pane[current_frmid],actionList,n-1);
			else
				action_area = 
					uw_mfcreate_action(m_pane[current_frmid],actionList,n);
		}
		else
		{
			dactionList[0].data = (XtPointer)current_frmid;
			if (m_fStruct[current_frmid].helpflag==0)
				uw_mfcreate_action(m_pane[current_frmid], dactionList, 1);
			else
			{
				dactionList[1].data = (XtPointer)current_frmid;
				uw_mfcreate_action(m_pane[current_frmid], dactionList, 2);
			}
		}

		formInpWin = NULL;
/*
.....Manage the Form
*/
		XtManageChild(m_winid[current_frmid]);
		XtManageChild(m_pane[current_frmid]);
		XtManageChild(UW_display_frm[current_frmid]);
		XtAddEventHandler(m_winid[current_frmid], ButtonReleaseMask, False,
                  (XtEventHandler) ButtonEventHandler,
                  (XtPointer)current_frmid);
		if (UW_auto_cursor)
			XWarpPointer(uw_xw.disp,None,XtWindow(UW_display_frm[current_frmid]),
				0,0,0,0,XBorder[1]+5,YBorder[1]+5);
		if (UD_initfrm_intry!=NULL)
			(*UD_initfrm_intry)(&fldno, 
				&(m_fData[current_frmid].ud_data[0].ud_delem),UD_TFWD);
/*
.....Disable Sash Traversal
*/
		XtVaGetValues(m_pane[current_frmid],XmNchildren, &children,
				XmNnumChildren, &num_children,
				NULL);
		while (num_children-- > 0)
			if (XmIsSash(children[num_children]))
					XtVaSetValues(children[num_children],
						XmNtraversalOn,False,
						NULL);
/*
.....Wait for the User to
.....accept/reject form
*/
		if (dflag==0)
		{
			formActive = True;
			formOn = True;
/*
.....always set UD_form_bypick = 0, only
.....when form in pick mode, UD_form_bypick = 1;
*/
			UD_form_bypick = 0;
		}
		for (i=0; i<fstruct->n_input_fields; i++)
		{
			if (fstruct->input_flds[i].toggle == 7)
			{
				m_Draw_gc[current_frmid][i] = 
							XCreateGC(XtDisplay(m_draw_area[current_frmid][i]), 
							XtWindow(m_draw_area[current_frmid][i]),
							0L, (XGCValues*) NULL);
			}
			else if (fstruct->input_flds[i].toggle == 9)
			{
				m_process_gc[current_frmid][i] = 
							XCreateGC(XtDisplay(m_process_bar[current_frmid][i]), 
							XtWindow(m_process_bar[current_frmid][i]),
							0L, (XGCValues*) NULL);
			}
		}
	}
/*
.....We should have wait cursor
.....when form displayed
.....changed by Yurong 9/12/97
*/
	if (current_frmid==0)
		m_parentID[current_frmid] = -1;
	else if (active_frmid!=-1)
	{
		m_parentID[current_frmid] = active_frmid;
		n = m_childnum [active_frmid] ;
		m_childnum [active_frmid] ++;
		m_childfrm[active_frmid][n] = current_frmid;
	}	
	else
		m_parentID[current_frmid] = -1;
		
	active_frmid = current_frmid;
	formActFrm = current_frmid;

	if (dflag==0)
	{
		uw_mfsetcursor(21);
		while (!m_formEof[0])
		{
			XtAppNextEvent(uw_mf.application,&x_event);
/*
......if it is a special key, we need handle it here as accelerator key
*/
			stat = uw_mfaccel_key(&x_event);
			if (stat==0)
				XtDispatchEvent(&x_event);
		}
	}
	goto done;
/*
.....Error trying to create form
*/
failed:;
	sprintf(buf,"Could not create Form: %s\n",fstruct->ud_frmdf.ud_fid);
	ud_wrerr(buf);
	status = UU_FAILURE;
/*
.....destroy created Widget 
*/
	if (UW_display_frm[current_frmid] != NULL)
	{
   	XtUnmanageChild(UW_display_frm[current_frmid]);
   	XtDestroyWidget(UW_display_frm[current_frmid]);
   	XmUpdateDisplay(UW_display_frm[current_frmid]);
   	UW_display_frm[current_frmid] = NULL;
		UW_first_resize[current_frmid] = 0;
	}
/*
.....Free allocated form data
*/
	uw_mffree_formdata(current_frmid);
/*
.....End of routine
*/
done:;
	if (dflag==0)
		return(status);
	else
	{
		if (status != UU_FAILURE)
			return current_frmid; 
		else
			return UU_FAILURE;
	}
}


/**********************************************************************
**    E_FUNCTION :  uw_putdisp_entry(parent, current_frmid, i, fstruct,
fdata, posx, posy)
**      Displays a "display" item on the form.
**    PARAMETERS
**       INPUT  :
**          parent: parent widget
**				current_frmid: current form ID
**				i: "display" item number
**				fstruct: form structure 
**				fdata : form data
**				posx, posy: position of "display" item
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_putdisp_entry(parent, current_frmid, i, fstruct, fdata, posx, posy)
Widget parent;
int current_frmid, i;
UD_FSTRUCT *fstruct;
UD_FDATA *fdata;
int posx, posy;
{
	Arg args[20];
	int n, status;
	status = UU_SUCCESS;
/*
.....Put up Display only entries
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,posy); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,posx); n++;
	m_FormDisp[current_frmid][i] = 
			XtCreateWidget(fstruct->display_flds[i].message,
			xmLabelWidgetClass,parent,args,n);
/*
........Manage the form display entry
........if visible
*/
	if (fstruct->ud_display_mask[i] == 1)
	{
		if (m_FormDisp[current_frmid][i] != NULL) 
			XtManageChild(m_FormDisp[current_frmid][i]);
	}
	uw_mfadj_frmfld2(fstruct, current_frmid, i);
	return status;
}


/**********************************************************************
**    E_FUNCTION :  issame(fontstrct1, fontstrct2)
**      compare 2 font structure to see if it have same attributes
**    PARAMETERS
**       INPUT  :
**				fontstrct1 : font structure to be compared
**				fontstrct2: font structure to be compared
**       OUTPUT :
**          none
**    RETURNS      : 0: not the same
**							1: the same
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int issame(fontstrct1, fontstrct2)
XFontStruct *fontstrct2, *fontstrct1;
{
	int i;
	if (fontstrct1->direction!=fontstrct2->direction)
		return 0;
	if (fontstrct1->min_char_or_byte2!=fontstrct2->min_char_or_byte2)
		return 0;
	if (fontstrct1->max_char_or_byte2!=fontstrct2->max_char_or_byte2)
		return 0;
	if (fontstrct1->min_byte1!=fontstrct2->min_byte1)
		return 0;
	if (fontstrct1->max_byte1!=fontstrct2->max_byte1)
		return 0;
	if (fontstrct1->all_chars_exist!=fontstrct2->all_chars_exist)
		return 0;
	if (fontstrct1->default_char!=fontstrct2->default_char)
		return 0;
	if (fontstrct1->n_properties!=fontstrct2->n_properties)
		return 0;
	if (fontstrct1->ascent!=fontstrct2->ascent)
		return 0;
	if (fontstrct1->descent!=fontstrct2->descent)
		return 0;
	if (fontstrct1->min_bounds.lbearing!=fontstrct2->min_bounds.lbearing)
		return 0;
	if (fontstrct1->min_bounds.rbearing!=fontstrct2->min_bounds.rbearing)
		return 0;
	if (fontstrct1->min_bounds.width!=fontstrct2->min_bounds.width)
		return 0;
	if (fontstrct1->min_bounds.ascent!=fontstrct2->min_bounds.ascent)
		return 0;
	if (fontstrct1->min_bounds.descent!=fontstrct2->min_bounds.descent)
		return 0;
	if (fontstrct1->min_bounds.attributes!=fontstrct2->min_bounds.attributes)
		return 0;
	if (fontstrct1->max_bounds.lbearing!=fontstrct2->max_bounds.lbearing)
		return 0;
	if (fontstrct1->max_bounds.rbearing!=fontstrct2->max_bounds.rbearing)
		return 0;
	if (fontstrct1->max_bounds.width!=fontstrct2->max_bounds.width)
		return 0;
	if (fontstrct1->max_bounds.ascent!=fontstrct2->max_bounds.ascent)
		return 0;
	if (fontstrct1->max_bounds.descent!=fontstrct2->max_bounds.descent)
		return 0;
	if (fontstrct1->max_bounds.attributes!=fontstrct2->max_bounds.attributes)
		return 0;
	for (i=0; i<fontstrct1->n_properties; i++)
	{
		if (fontstrct1->properties[i].name!=fontstrct2->properties[i].name)
			return 0;
		if (fontstrct1->properties[i].card32!=fontstrct2->properties[i].card32)
			return 0;
	}
	return 1;
}
/**********************************************************************
**    E_FUNCTION :  uw_mfget_winfont(window, fontname)
**      Get window default font name string
**    PARAMETERS
**       INPUT  :
**				window: window to get the font
**       OUTPUT :
**          fontname: font name
**    RETURNS      : 0: failed
**							1: Get it
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfget_winfont(window, fontname)
Widget window;
char *fontname;
{
#if UU_COMP != UU_HPUX
	Bool stat;
	XmFontList  oldlist;
	XmFontListEntry font_entry;
	XmFontContext font_context;
	XFontStruct *fontstrct;
	XtPointer fontPtr;
	XmFontType ftyp;
	Display *display;
	unsigned long value;
	if (window==NULL)
	{
		strcpy(fontname, "-*-courier-bold-r-normal-*-14-*-*-*-*-*-iso8859-1");
		return 1;
	}
	XtVaGetValues(window, XmNfontList, &oldlist, NULL);
	XmFontListInitFontContext(&font_context, oldlist);
	font_entry = XmFontListNextEntry(font_context);
	fontPtr = XmFontListEntryGetFont(font_entry, &ftyp);
	if (ftyp==XmFONT_IS_FONT)
		fontstrct = (XFontStruct*) fontPtr;
	else
	{
		strcpy(fontname, "-*-courier-bold-r-normal-*-14-*-*-*-*-*-iso8859-1");
		return 1;
	}
	display = XtDisplay(window);
	stat = XGetFontProperty(fontstrct, XA_FONT, &value);
	if (stat)
		strcpy(fontname, XGetAtomName(display, (Atom)value));
	else
		strcpy(fontname, "-*-courier-bold-r-normal-*-14-*-*-*-*-*-iso8859-1");
	XmFontListFreeFontContext(font_context);
done:;
#endif
	return 1;
}

/***********************************************************************
c
c   SUBROUTINE:  uw_mfadj_frmfld3(fstruct, current_frmid, fldno)
c
c   FUNCTION:  This function adjust COLOR, FONT, JUSTIFY data
c				of form frame title
c
c   INPUT:  fstruct: form strcut
c				current_frmid: form id
c				fldno: form field number
c
c   OUTPUT: none
c
c***********************************************************************
*/
void uw_mfadj_frmfld3(fstruct, current_frmid, fldno)
int current_frmid, fldno;
UD_FSTRUCT *fstruct;
{ 
	unsigned long new_bg, fg, bg, new_fg;
	XColor current_color;
	Colormap cmap;
	Display *display;
	char font_name[256];
	int font_size;
	XmFontList frm_fontlist;
	XFontStruct *fontstrct;
	int  i,j,k, len,m,n, stat;
	char tempstr[256], first_str[256];

	if (strcmp (fstruct->frame_flds[fldno].fcolor, "DEFAULT")!=0)
	{
		XtVaGetValues(m_frametitle[current_frmid][fldno], XmNcolormap, &cmap,
							XmNforeground, &fg, NULL);
		uw_get_rgb (fstruct->frame_flds[fldno].fcolor, &current_color);
		display = XtDisplay(m_frametitle[current_frmid][fldno]);
		stat = XAllocColor(display, cmap, &current_color);
		if (stat == 0) new_fg = fg;
		else new_fg = current_color.pixel;
		XtVaSetValues(m_frametitle[current_frmid][fldno], 
				XmNforeground, new_fg, NULL);
	} 
	if (strcmp (fstruct->frame_flds[fldno].bcolor, "DEFAULT")!=0)
	{
		XtVaGetValues(m_frametitle[current_frmid][fldno], XmNcolormap, &cmap,
							XmNbackground, &bg, NULL);
		uw_get_rgb (fstruct->frame_flds[fldno].bcolor, &current_color);
		display = XtDisplay(m_frametitle[current_frmid][fldno]);
		stat = XAllocColor(display, cmap, &current_color);
		if (stat == 0) new_bg = bg;
		else new_bg = current_color.pixel;
		XtVaSetValues(m_frametitle[current_frmid][fldno], 
				XmNbackground, new_bg, NULL);
	} 
	if (fontget_first==1)
	{
		uw_mfget_winfont(m_frametitle[current_frmid][fldno], UW_frmfont_name);
		fontget_first = 0;
	}
	if (fstruct->frame_flds[fldno].font_scale==1.0)
		return;
/*
.....replace the default font size which will be the 7th token in
.....font name string ("-*-courier-bold-r-normal-*-14-*-*-*-*-*-iso8859-1")
.....also replace following 5 item as * (because size change, they need
.....changed too)
*/
	len = strlen (UW_frmfont_name);
	j = 0; k = 0;
	m=0; n=0;
	for (i=0; i<len; i++)
	{
		if (UW_frmfont_name[i]=='-')
			j++;
		if ((j==7)&&(m==0))
		{
			tempstr[k] = '\0';
			strcpy(first_str, tempstr);
			k = 0;
			m = 1;
			continue;
		}
		if ((j==8)&&(n==0))
		{
			tempstr[k] = '\0';
			font_size = atoi(tempstr);
			k = 0;
			n = 1;
		}
		if (!((j>7) && (j<13)))
			tempstr[k++] = UW_frmfont_name[i];
	}
	tempstr[k] = '\0';
	font_size = font_size * fstruct->display_flds[fldno].font_scale;
	sprintf (font_name, "%s-%d-*-*-*-*-*%s", first_str, font_size, tempstr);
	fontstrct = XLoadQueryFont(XtDisplay(UW_display_frm[current_frmid]),
					font_name);
	if (fontstrct==NULL)
	{
/*
.....this font size does not exist, so we need recursively check
.....the font by adjust size +-1 to get the right font
*/
		n = 0;
		while ((n<12)&&(font_size-n>0))
		{
			n++;
			sprintf (font_name, "%s-%d-*-*-*-*-*%s", 
							first_str, font_size-n, tempstr);
			fontstrct = XLoadQueryFont(XtDisplay(UW_display_frm[current_frmid]),
					font_name);
			if (fontstrct!=NULL)
				break;
			sprintf (font_name, "%s-%d-*-*-*-*-*%s", 
					first_str, font_size+n, tempstr);
			fontstrct = XLoadQueryFont(XtDisplay(UW_display_frm[current_frmid]),
					font_name);
			if (fontstrct!=NULL)
				break;
		}
		if (n>=12)
			return;
	}
	if (fontstrct==NULL)
		return;
	frm_fontlist =  XmFontListCreate (fontstrct, XmSTRING_DEFAULT_CHARSET);
	if ((m_frametitle[current_frmid]!=NULL) &&
			(m_frametitle[current_frmid][fldno]!=NULL))
		XtVaSetValues(m_frametitle[current_frmid][fldno], 
					XmNfontList, frm_fontlist, NULL);
}

/***********************************************************************
c
c   SUBROUTINE:  uw_mfadj_frmfld2(fstruct, current_frmid, fldno)
c
c   FUNCTION:  This function adjust COLOR, FONT, JUSTIFY data
c				of form display fields
c
c   INPUT:  fstruct: form strcut
c				current_frmid: form id
c				fldno: form field number
c
c   OUTPUT: none
c
c***********************************************************************
*/
void uw_mfadj_frmfld2(fstruct, current_frmid, fldno)
int current_frmid, fldno;
UD_FSTRUCT *fstruct;
{ 
	unsigned long new_bg, fg, bg, new_fg;
	XColor current_color;
	Colormap cmap;
	Display *display;
	char font_name[256];
	int font_size;
	XmFontList frm_fontlist;
	XFontStruct *fontstrct;
	int  i,j,k, len,m,n, stat;
	char tempstr[256], first_str[256];

	if (strcmp (fstruct->display_flds[fldno].fcolor, "DEFAULT")!=0)
	{
		XtVaGetValues(m_FormDisp[current_frmid][fldno], XmNcolormap, &cmap,
							XmNforeground, &fg, NULL);
		uw_get_rgb (fstruct->display_flds[fldno].fcolor, &current_color);
		display = XtDisplay(m_FormDisp[current_frmid][fldno]);
		stat = XAllocColor(display, cmap, &current_color);
		if (stat == 0) new_fg = fg;
		else new_fg = current_color.pixel;
		XtVaSetValues(m_FormDisp[current_frmid][fldno], 
				XmNforeground, new_fg, NULL);
	} 
	if (strcmp (fstruct->display_flds[fldno].bcolor, "DEFAULT")!=0)
	{
		XtVaGetValues(m_FormDisp[current_frmid][fldno], XmNcolormap, &cmap,
							XmNbackground, &bg, NULL);
		uw_get_rgb (fstruct->display_flds[fldno].bcolor, &current_color);
		display = XtDisplay(m_FormDisp[current_frmid][fldno]);
		stat = XAllocColor(display, cmap, &current_color);
		if (stat == 0) new_bg = bg;
		else new_bg = current_color.pixel;
		XtVaSetValues(m_FormDisp[current_frmid][fldno], 
				XmNbackground, new_bg, NULL);
	} 
	if (fontget_first==1)
	{
		uw_mfget_winfont(m_FormDisp[current_frmid][fldno], UW_frmfont_name);
		fontget_first = 0;
	}
	if (fstruct->display_flds[fldno].font_scale==1.0)
		return;
/*
.....replace the default font size which will be the 7th token in
.....font name string ("-*-courier-bold-r-normal-*-14-*-*-*-*-*-iso8859-1")
.....also replace following 5 item as * (because size change, they need
.....changed too)
*/
	len = strlen (UW_frmfont_name);
	j = 0; k = 0;
	m=0; n=0;
	for (i=0; i<len; i++)
	{
		if (UW_frmfont_name[i]=='-')
			j++;
		if ((j==7)&&(m==0))
		{
			tempstr[k] = '\0';
			strcpy(first_str, tempstr);
			k = 0;
			m = 1;
			continue;
		}
		if ((j==8)&&(n==0))
		{
			tempstr[k] = '\0';
			font_size = atoi(tempstr);
			k = 0;
			n = 1;
		}
		if (!((j>7) && (j<13)))
			tempstr[k++] = UW_frmfont_name[i];
	}
	tempstr[k] = '\0';
	font_size = font_size * fstruct->display_flds[fldno].font_scale;
	sprintf (font_name, "%s-%d-*-*-*-*-*%s", first_str, font_size, tempstr);
	fontstrct = XLoadQueryFont(XtDisplay(UW_display_frm[current_frmid]),
					font_name);
	if (fontstrct==NULL)
	{
/*
.....this font size does not exist, so we need recursively check
.....the font by adjust size +-1 to get the right font
*/
		n = 0;
		while ((n<12)&&(font_size-n>0))
		{
			n++;
			sprintf (font_name, "%s-%d-*-*-*-*-*%s", 
							first_str, font_size-n, tempstr);
			fontstrct = XLoadQueryFont(XtDisplay(UW_display_frm[current_frmid]),
					font_name);
			if (fontstrct!=NULL)
				break;
			sprintf (font_name, "%s-%d-*-*-*-*-*%s", 
					first_str, font_size+n, tempstr);
			fontstrct = XLoadQueryFont(XtDisplay(UW_display_frm[current_frmid]),
					font_name);
			if (fontstrct!=NULL)
				break;
		}
		if (n>=12)
			return;
	}
	if (fontstrct==NULL)
		return;
	frm_fontlist =  XmFontListCreate (fontstrct, XmSTRING_DEFAULT_CHARSET);
	if ((m_FormDisp[current_frmid]!=NULL) &&
			(m_FormDisp[current_frmid][fldno]!=NULL))
		XtVaSetValues(m_FormDisp[current_frmid][fldno], 
					XmNfontList, frm_fontlist, NULL);
}

/***********************************************************************
c
c   SUBROUTINE:  uw_mfadj_frmfld1(fstruct, current_frmid, fldno)
c
c   FUNCTION:  This function adjust COLOR, FONT, JUSTIFY data
c				of form input fields
c
c   INPUT:  fstruct: form strcut
c				current_frmid: form id
c				fldno: form field number
c
c   OUTPUT: none
c
c***********************************************************************
*/
void uw_mfadj_frmfld1(fstruct, current_frmid, fldno)
int current_frmid, fldno;
UD_FSTRUCT *fstruct;
{ 
	unsigned long new_bg, fg, bg, new_fg;
	XColor current_color;
	Colormap cmap;
	Display *display;
	char font_name[256];
	int font_size;
	XmFontList frm_fontlist;
	XFontStruct *fontstrct;
	int  i,j,k, len,m,n, stat;
	char tempstr[256], first_str[256];

	if (strcmp (fstruct->input_flds[fldno].pfcolor, "DEFAULT")!=0)
	{
		XtVaGetValues(m_formPrompt[current_frmid][fldno], XmNcolormap, &cmap,
							XmNforeground, &fg, NULL);
		uw_get_rgb (fstruct->input_flds[fldno].pfcolor, &current_color);
		display = XtDisplay(m_formPrompt[current_frmid][fldno]);
		stat = XAllocColor(display, cmap, &current_color);
		if (stat == 0) new_fg = fg;
		else new_fg = current_color.pixel;
		XtVaSetValues(m_formPrompt[current_frmid][fldno], 
				XmNforeground, new_fg, NULL);
	} 
	if (strcmp (fstruct->input_flds[fldno].pbcolor, "DEFAULT")!=0)
	{
		XtVaGetValues(m_formPrompt[current_frmid][fldno], XmNcolormap, &cmap,
							XmNbackground, &bg, NULL);
		uw_get_rgb (fstruct->input_flds[fldno].pbcolor, &current_color);
		display = XtDisplay(m_formPrompt[current_frmid][fldno]);
		stat = XAllocColor(display, cmap, &current_color);
		if (stat == 0) new_bg = bg;
		else new_bg = current_color.pixel;
		XtVaSetValues(m_formPrompt[current_frmid][fldno], 
				XmNbackground, new_bg, NULL);
	} 
	if (fontget_first==1)
	{
		uw_mfget_winfont(m_formPrompt[current_frmid][fldno], UW_frmfont_name);
		fontget_first = 0;
	}
	if (fstruct->input_flds[fldno].font_scale==1.0)
		goto input;
/*
.....replace the default font size which will be the 7th token in
.....font name string ("-*-courier-bold-r-normal-*-14-*-*-*-*-*-iso8859-1")
.....also replace following 5 item as * (because size change, they need
.....changed too)
*/
	len = strlen (UW_frmfont_name);
	j = 0; k = 0;
	m=0; n=0;
	for (i=0; i<len; i++)
	{
		if (UW_frmfont_name[i]=='-')
			j++;
		if ((j==7)&&(m==0))
		{
			tempstr[k] = '\0';
			strcpy(first_str, tempstr);
			k = 0;
			m = 1;
			continue;
		}
		if ((j==8)&&(n==0))
		{
			tempstr[k] = '\0';
			font_size = atoi(tempstr);
			k = 0;
			n = 1;
		}
		if (!((j>7) && (j<13)))
			tempstr[k++] = UW_frmfont_name[i];
	}
	tempstr[k] = '\0';
	font_size = font_size * fstruct->input_flds[fldno].font_scale;
	sprintf (font_name, "%s-%d-*-*-*-*-*%s", first_str, font_size, tempstr);
	fontstrct = XLoadQueryFont(XtDisplay(UW_display_frm[current_frmid]),
					font_name);
	if (fontstrct==NULL)
	{
/*
.....this font size does not exist, so we need recursively check
.....the font by adjust size +-1 to get the right font
*/
		n = 0;
		while ((n<12)&&(font_size-n>0))
		{
			n++;
			sprintf (font_name, "%s-%d-*-*-*-*-*%s", 
							first_str, font_size-n, tempstr);
			fontstrct = XLoadQueryFont(XtDisplay(UW_display_frm[current_frmid]),
					font_name);
			if (fontstrct!=NULL)
				break;
			sprintf (font_name, "%s-%d-*-*-*-*-*%s", 
					first_str, font_size+n, tempstr);
			fontstrct = XLoadQueryFont(XtDisplay(UW_display_frm[current_frmid]),
					font_name);
			if (fontstrct!=NULL)
				break;
		}
		if (n>=12)
			goto input;
	}
	if (fontstrct==NULL)
		goto input;
	frm_fontlist =  XmFontListCreate (fontstrct, XmSTRING_DEFAULT_CHARSET);
/*
......don't apply font size to prompt label
......Yurong
	if ((m_formPrompt[current_frmid]!=NULL) &&
			(m_formPrompt[current_frmid][fldno]!=NULL))
		XtVaSetValues(m_formPrompt[current_frmid][fldno], 
					XmNfontList, frm_fontlist, NULL);
*/
	if ((m_formMenu[current_frmid]!=NULL)&&
			(m_formMenu[current_frmid][fldno]!=NULL))
	{
		XtVaSetValues(m_formMenu[current_frmid][fldno], 
					XmNfontList, frm_fontlist, NULL);
		for (i=0; i<MAX_CHOICE; i++)
		{
			if (m_formChoice[current_frmid][fldno][i] != NULL)
			{
				XtVaSetValues(m_formChoice[current_frmid][fldno][i], 
					XmNfontList, frm_fontlist, NULL);
			}
			else
				break;
		}
	}
	if ((m_formEntry[current_frmid]!=NULL) &&
			(m_formEntry[current_frmid][fldno]!=NULL))
	{
/*
.....set the font will change the size of the button
.....so we set it
*/
/***
		XtVaGetValues(m_formEntry[current_frmid][fldno], 
				XmNwidth, &width,
				XmNheight, &height, NULL);
*/
		XtVaSetValues(m_formEntry[current_frmid][fldno], 
					XmNfontList, frm_fontlist, 
/*
					XmNwidth, width,
					XmNheight, height,
*/
					NULL);
	}
	if ((m_list[current_frmid]!=NULL)&&
			(m_list[current_frmid][fldno]!=NULL))
	{
		XtVaSetValues(m_list[current_frmid][fldno], 
					XmNfontList, frm_fontlist, NULL);
	}
input:;
	if (strcmp (fstruct->input_flds[fldno].fcolor, "DEFAULT")!=0)
	{
		XtVaGetValues(m_formEntry[current_frmid][fldno], XmNcolormap, &cmap,
							XmNforeground, &fg, NULL);
		uw_get_rgb (fstruct->input_flds[fldno].fcolor, &current_color);
		display = XtDisplay(m_formEntry[current_frmid][fldno]);
		stat = XAllocColor(display, cmap, &current_color);
		if (stat == 0) new_fg = fg;
		else new_fg = current_color.pixel;
		if ((m_formMenu[current_frmid]!=NULL) &&
				(m_formMenu[current_frmid][fldno]!=NULL))
		{
			XtVaSetValues(m_formMenu[current_frmid][fldno], 
					XmNforeground, new_fg, NULL);
		}
		if ((m_formEntry[current_frmid]!=NULL) &&
				(m_formEntry[current_frmid][fldno]!=NULL))
		{
			XtVaSetValues(m_formEntry[current_frmid][fldno], 
					XmNforeground, new_fg, NULL);
		}
		if ((m_list[current_frmid]!=NULL) && 
				(m_list[current_frmid][fldno]!=NULL))
		{
			XtVaSetValues(m_list[current_frmid][fldno], 
					XmNforeground, new_fg, NULL);
		}
	} 
	if (strcmp (fstruct->input_flds[fldno].bcolor, "DEFAULT")!=0)
	{
		XtVaGetValues(m_formEntry[current_frmid][fldno], XmNcolormap, &cmap,
						XmNbackground, &bg, NULL);
		uw_get_rgb (fstruct->input_flds[fldno].bcolor, &current_color);
		display = XtDisplay(m_formEntry[current_frmid][fldno]);
		stat = XAllocColor(display, cmap, &current_color);
		if (stat == 0) new_bg = bg;
		else new_bg = current_color.pixel;
		if ((m_formMenu[current_frmid]!=NULL) &&
				(m_formMenu[current_frmid][fldno]!=NULL))
		{
			XtVaSetValues(m_formMenu[current_frmid][fldno], 
					XmNbackground, new_bg, NULL);
		}
		if ((m_formEntry[current_frmid]!=NULL) &&
				(m_formEntry[current_frmid][fldno]!=NULL))
		{
			XtVaSetValues(m_formEntry[current_frmid][fldno], 
					XmNbackground, new_bg, NULL);
		}
		if ((m_list[current_frmid]!=NULL) &&
				(m_list[current_frmid][fldno]!=NULL))
		{
			XtVaSetValues(m_list[current_frmid][fldno], 
					XmNbackground, new_bg, NULL);
		}
	}
}
/**********************************************************************
**    E_FUNCTION :  uw_putinput_entry(parent, current_frmid, i, fstruct,
fdata, pposx, pposy,posx, posy, rposx, rposy)
**      Displays a "Input field" item on the form.
**    PARAMETERS
**       INPUT  :
**          parent: parent widget
**				current_frmid: current form ID
**				i: "display" item number
**				fstruct: form structure 
**				fdata : form data
**				pposx, pposy: "INPUT" field prompt position
**				posx, posy: optional position of "Input field"
**				rposx, rposy: end position of "Input field"
**				cx, cy: parent window size
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_putinput_entry(parent, current_frmid, i, fstruct, fdata, pposx, pposy,
							posx, posy, rposx, rposy, cx, cy)
Widget parent;
int current_frmid, i;
UD_FSTRUCT *fstruct;
UD_FDATA *fdata;
int pposx, pposy, posx, posy,rposx,rposy;
int cx, cy;
{
	int item_len, k, defsel, istat;
	char perc_str[10];
	char sample[100], tmpans[80], tempstr[40];
	int ktyp, ksub;
	int n,j,status,state, inc,lnth;
	Arg args[20];
	char buf[120],ldat[80];
	XmString defstr;
	XmStringTable str_list;
	UD_LIST *form_list, *choice_list;
	UD_TLIST *table_list;
	int picwid, pichgt;
	int tmphgt, topy, tmpwid;
	int rposxt, wid, textlen;
	int def_choice;
	static XmTextScanType sarray[] =
		{XmSELECT_ALL, XmSELECT_POSITION, XmSELECT_WORD, XmSELECT_LINE};
	status = UU_SUCCESS;
	m_formPrompt[current_frmid][i] = NULL;
	m_formEntry[current_frmid][i] = NULL;
	m_textPrompt[current_frmid][i] = NULL;
	m_list[current_frmid][i] = NULL;
/*
.....Put up Form input entries
*/
	inc = fstruct->n_display_fields;
/*
........Set prompt position
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition,pposy); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition,pposx); n++;

/*
........Choice field
*/
	if (  (fstruct->input_flds[i].toggle == 2)
			|| (fstruct->input_flds[i].toggle == 10))
	{
		m_formPrompt[current_frmid][i] = 
				XtCreateWidget(fstruct->input_flds[i].prompt,
				xmLabelWidgetClass,parent,args,n);
/*
........Set field position
*/
		n = 0;
		XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
		if (posy<0)
		{
			XtSetArg(args[n],XmNtopPosition,pposy); n++;
		}
		else
		{
			XtSetArg(args[n],XmNtopPosition,posy); n++;
		}
		if (posx<0)
		{
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
			XtSetArg(args[n],XmNleftWidget, m_formPrompt[current_frmid][i]); 
			n++;
		}
		else
		{
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNleftPosition,posx); n++;
		}
/*
.....The right position not work for this type, it only cut off the lenght 
.....if choice text have more length but if the choice text is short,
.....it doesn't extend the button length. This made thing worse than default
.....Yurong
		XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNrightPosition, rposx); n++;
*/
/*
...........Create the Pull Down Menu
...........To hold the choices
*/
		sprintf(buf,"pulldn_%d",i);
		m_formMenu[current_frmid][i] = 
					(Widget)XmCreatePulldownMenu(parent, buf,NULL,0);
/*
...........Create an Option button
*/
		XtSetArg(args[n], XmNsubMenuId, m_formMenu[current_frmid][i]); n++;
		XtSetArg(args[n], XmNmarginHeight, 0); n++;
		XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
		XtSetArg(args[n], XmNtraversalOn, True); n++;
		XtSetArg(args[n], XmNuserData, (XtPointer)2); n++;
		m_formEntry[current_frmid][i] = 
					(Widget)XmCreateOptionMenu(parent, "", args, n);
/*
...........Create the options
*/
		if (fstruct->input_flds[i].toggle == 2)
		{
			for (j=0;j<fstruct->input_flds[i].n_defaults;j++)
			{
				m_formChoice[current_frmid][i][j] = 
								XtVaCreateManagedWidget(
								fstruct->input_flds[i].defaults[j].dstr,
								xmPushButtonGadgetClass, 
								m_formMenu[current_frmid][i],
								NULL);

				if (m_formChoice[current_frmid][i][j] == NULL) 
					goto failed;
/*
...........Add the callback routine
*/
				m_pStruct[current_frmid][m_Nps[current_frmid]] = 
										(Pstruct *)uu_malloc(sizeof(Pstruct));
				m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 2;
				m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
				m_pStruct[current_frmid][m_Nps[current_frmid]]->chc = j;
				m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																			current_frmid;
				XtAddCallback(m_formChoice[current_frmid][i][j],XmNarmCallback,
								uw_mfform_focusCB,
								m_pStruct[current_frmid][m_Nps[current_frmid]]);
				XtAddCallback(m_formChoice[current_frmid][i][j],
								XmNactivateCallback,
								uw_mfform_activateCB,
								m_pStruct[current_frmid][m_Nps[current_frmid]]);
								(m_Nps[current_frmid])++;
			}
/*
...........Set the default choice
*/
			def_choice = 0;
			if (m_fData[current_frmid].ud_data[i].ud_delem.frmint[0]>=0)
			{
				def_choice = m_fData[current_frmid].ud_data[i].ud_delem.frmint[0];
			}
			else if (fstruct->input_flds[i].ud_input==1)
			{
				def_choice = 16;
			} 
			XtVaSetValues(m_formEntry[current_frmid][i],XmNmenuHistory,
					m_formChoice[current_frmid][i][def_choice],NULL);
		}
		if (fstruct->input_flds[i].toggle == 10)
		{
			defsel = 0;
			choice_list =
				 (UD_LIST*)(fdata->ud_data[i].ud_delem.frmint);
			for (j=0;j<choice_list->num_item;j++)
			{
				m_formChoice[current_frmid][i][j] = 
								XtVaCreateManagedWidget(
								choice_list->item[j],
								xmPushButtonGadgetClass, 
								m_formMenu[current_frmid][i],
								NULL);

				if (m_formChoice[current_frmid][i][j] == NULL) 
					goto failed;
				if (strcmp(choice_list->item[j], choice_list->answer)==0)
					defsel = j;
/*
...........Add the callback routine
*/
				m_pStruct[current_frmid][m_Nps[current_frmid]] = 
										(Pstruct *)uu_malloc(sizeof(Pstruct));
				m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 10;
				m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
				m_pStruct[current_frmid][m_Nps[current_frmid]]->chc = j;
				m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																		current_frmid;
				XtAddCallback(m_formChoice[current_frmid][i][j],
							XmNarmCallback,
							uw_mfform_focusCB,
							m_pStruct[current_frmid][m_Nps[current_frmid]]);
				XtAddCallback(m_formChoice[current_frmid][i][j],
							XmNactivateCallback,
							uw_mfform_activateCB,
							m_pStruct[current_frmid][m_Nps[current_frmid]]);
				(m_Nps[current_frmid])++;
			}
/*
...........Set the default choice
*/
			XtVaSetValues(m_formEntry[current_frmid][i],XmNmenuHistory,
					m_formChoice[current_frmid][i][defsel],NULL);
		}	
	}
/*
.....added List field
.....Yurong  8/11/97
*/
/*
......display table list as normal list now
*/
	else if ((fstruct->input_flds[i].toggle == 5)||
				(fstruct->input_flds[i].toggle == 17))
	{
/*
........Put up list Prompt
*/
		m_formPrompt[current_frmid][i] = 
					XtCreateWidget(fstruct->input_flds[i].prompt,
					xmLabelWidgetClass,parent,args,n);
/*
.....Create text field for list
*/
/*
.....list box can be not accociate a text filed now
.....Yurong 4/13/00
*/
		if (fstruct->input_flds[i].n_defaults!=0)
		{
/*
........Set field position
*/
			n = 0;
			XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
			if (posy<0)
			{
				XtSetArg(args[n],XmNtopPosition,pposy); n++;
				topy = pposy;
			}
			else
			{
				XtSetArg(args[n],XmNtopPosition,posy); n++;
				topy = posy;
			}
			if (posx<0)
			{
				XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
				XtSetArg(args[n],XmNleftWidget, m_formPrompt[current_frmid][i]);
				n++;
			}
			else
			{
				XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
				XtSetArg(args[n],XmNleftPosition,posx); n++;
			}
			XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
			if (fstruct->input_flds[i].n_defaults==2)
			{
				textlen = fstruct->input_flds[i].ud_flen*4;
				wid = (fstruct->ud_frmdf).ud_frc.ud_c;
				rposxt = posx + (textlen * 100) /wid;
				XtSetArg(args[n],XmNrightPosition, rposxt); n++;
			}
			else
			{
				XtSetArg(args[n],XmNrightPosition, rposx); n++;
			}
/*
...........Format the default answer
*/
			XtSetArg(args[n],XmNeditable,True); n++;
			XtSetArg(args[n],XmNeditMode,XmSINGLE_LINE_EDIT); n++;
			XtSetArg(args[n],XmNmarginHeight,1); n++;
			XtSetArg(args[n], XmNtraversalOn, True); n++;
			XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
			XtSetArg(args[n], XmNuserData, (XtPointer)5);n++;
			XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
			sprintf(buf,"form_%d",i);
			m_formEntry[current_frmid][i] = XtCreateWidget(buf,
					xmTextWidgetClass,parent,args,n);
/*
...........Add the callback routine
*/
			m_pStruct[current_frmid][m_Nps[current_frmid]] = 
										(Pstruct *)uu_malloc(sizeof(Pstruct));
			m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 1;
			m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
			m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																		current_frmid;
/*
.....This callback give the select answer
*/
			XtAddCallback(m_formEntry[current_frmid][i],XmNfocusCallback,
							uw_mfform_focusCB,
							m_pStruct[current_frmid][m_Nps[current_frmid]]);
			XtAddCallback(m_formEntry[current_frmid][i],
					XmNlosingFocusCallback,
					uw_mfform_editdoneCB,
					m_pStruct[current_frmid][m_Nps[current_frmid]]);
			(m_Nps[current_frmid])++;
		}
/*
........Set field position
*/
		sprintf(buf,"list_%d",i);
/*
.....added items to list
*/
/****
......we don't support table-list on UNIX
......so we just convert a UD_TLIST into a UD_LIST in some way
*/
		if (fstruct->input_flds[i].toggle==17)
		{
			table_list = (UD_TLIST*)(fdata->ud_data[i].ud_delem.frmint);
			Sconv_list1 (table_list, &Sflist[i]);
			form_list = &Sflist[i];
		}
		else
			form_list =
				 (UD_LIST*)(fdata->ud_data[i].ud_delem.frmint);
		if ((m_formEntry[current_frmid]!=NULL) &&
				(m_formEntry[current_frmid][i]!=NULL))
		{
			if (form_list->answer!=NULL)
				XmTextSetString(m_formEntry[current_frmid][i], form_list->answer);
			else
				XmTextSetString(m_formEntry[current_frmid][i], "");
		}	
		if (form_list->num_item > 0)
			str_list = (XmStringTable)XtMalloc( (form_list->num_item)*
										sizeof(XmString *));
		for (j=0;j<form_list->num_item;j++)
		{
			item_len = strlen(form_list->item[j]);
			strcpy(sample, form_list->item[j]);
/*
.....make a item that length is list length
.....in order to set list length.
*/
			for (k = item_len; k<fstruct->input_flds[i].ud_flen; k++)
				sample[k] = ' ';
			sample[k] = '\0';
			str_list[j] = XmStringCreateSimple(sample);
		}
		n = 0;

		if (fstruct->input_flds[i].n_defaults==2)
		{
/*
........Set field position
*/
			n = 0;
			XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
			if (posy<0)
			{
				XtSetArg(args[n],XmNtopPosition,pposy); n++;
			}
			else
			{
				XtSetArg(args[n],XmNtopPosition,posy); n++;
			}
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
			XtSetArg(args[n],XmNleftWidget, m_formEntry[current_frmid][i]); 
			n++;
/*
...........Create the Pull Down Menu
...........To hold the choices
*/
			sprintf(buf,"pulldn_%d",i);
			m_formMenu[current_frmid][i] = 
					(Widget)XmCreatePulldownMenu(parent, buf,NULL,0);
/*
...........Create an Option button
*/
			XtSetArg(args[n], XmNsubMenuId, m_formMenu[current_frmid][i]); n++;
			XtSetArg(args[n], XmNmarginHeight, 0); n++;
			XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
			XtSetArg(args[n], XmNtraversalOn, True); n++;
			XtSetArg(args[n], XmNuserData, (XtPointer)2); n++;
			m_list[current_frmid][i] = 
					(Widget)XmCreateOptionMenu(parent, "", args, n);
/*
...........Create the options
*/
			defsel = 0;
			choice_list =
				 (UD_LIST*)(fdata->ud_data[i].ud_delem.frmint);
			for (j=0;j<choice_list->num_item;j++)
			{
				m_formChoice[current_frmid][i][j] = 
								XtVaCreateManagedWidget(
								choice_list->item[j],
								xmPushButtonGadgetClass, 
								m_formMenu[current_frmid][i],
								NULL);

				if (m_formChoice[current_frmid][i][j] == NULL) 
					goto failed;
				if (strcmp(choice_list->item[j], choice_list->answer)==0)
					defsel = j;
/*
...........Add the callback routine
*/
				m_pStruct[current_frmid][m_Nps[current_frmid]] = 
										(Pstruct *)uu_malloc(sizeof(Pstruct));
				m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 10;
				m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
				m_pStruct[current_frmid][m_Nps[current_frmid]]->chc = j;
				m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																		current_frmid;
				XtAddCallback(m_formChoice[current_frmid][i][j],
							XmNarmCallback,
							uw_mfform_focusCB,
							m_pStruct[current_frmid][m_Nps[current_frmid]]);
				XtAddCallback(m_formChoice[current_frmid][i][j],
							XmNactivateCallback,
							uw_mfform_activateCB,
							m_pStruct[current_frmid][m_Nps[current_frmid]]);
				(m_Nps[current_frmid])++;
			}
			XtVaSetValues(m_list[current_frmid][i],XmNmenuHistory,
					m_formChoice[current_frmid][i][defsel],NULL);
		}          
		else
		{
			if (fstruct->input_flds[i].n_defaults==0)
			{
				XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
				if (posy>=0)
				{
					XtSetArg(args[n],XmNtopPosition,posy); n++;
					topy = posy;
				}
				else
				{
					XtSetArg(args[n],XmNtopPosition,pposy); n++;
					topy = pposy;
				}
			}
			else
			{
				XtSetArg(args[n],XmNtopAttachment, XmATTACH_WIDGET); n++;
				XtSetArg(args[n],XmNtopWidget, m_formEntry[current_frmid][i]);  
				n++;
			}
			if (posx<0)
			{
				XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
				XtSetArg(args[n],XmNleftWidget, m_formPrompt[current_frmid][i]); 
				n++;
			}
			else
			{
				XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
				XtSetArg(args[n],XmNleftPosition,posx); n++;
			}
			XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNrightPosition, rposx); n++;

			XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNbottomPosition, rposy); n++;
			tmphgt = (float)(rposy - topy)*cy/100.00;
			if (fstruct->input_flds[i].n_defaults!=0)
				tmphgt -= 15;
			XtSetArg(args[n],XmNheight, tmphgt); n++;
/****/
			tmpwid = fstruct->input_flds[i].ud_fldloc.ud_c;
			tmpwid = ((float)tmpwid)*cx/100.00;
			XtSetArg(args[n],XmNwidth, tmphgt); n++;

			if (form_list->num_item > 0)
			{
				XtSetArg(args[n], XmNitemCount, form_list->num_item); n++;
				XtSetArg(args[n], XmNitems, str_list); n++;
			}
			XtSetArg(args[n], XmNselectionPolicy, XmSINGLE_SELECT); n++;
			if ((fstruct->input_flds[i].toggle==17)
				|| (fstruct->input_flds[i].ud_flen==-1))
			{
				XtSetArg(args[n], XmNscrollingPolicy,  XmAUTOMATIC); n++;
				XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmAS_NEEDED); n++;
				XtSetArg(args[n], XmNlistSizePolicy, XmCONSTANT); n++;
			}
			else
			{
				XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
			}
			m_list[current_frmid][i] = 
					(Widget)XmCreateScrolledList(parent, buf,args, n);
/*
.....Set the default select
*/
			if ((form_list->answer!=NULL)&&(form_list->answer[0]!='\0'))
			{
				strcpy(tmpans, form_list->answer);
				if (strtok(tmpans, " \0\n")!=NULL)
				{
					item_len = strlen(form_list->answer);
					strcpy(sample, form_list->answer);
					for (k = item_len; k<fstruct->input_flds[i].ud_flen; k++)
						sample[k] = ' ';
					sample[k] = '\0';
					defstr = XmStringCreateSimple(sample);
					XmListSelectItem(m_list[current_frmid][i], defstr, True);
				}
			}
/*
...........Add the callback routine
*/
			m_pStruct[current_frmid][m_Nps[current_frmid]] = 
						(Pstruct *)uu_malloc(sizeof(Pstruct));
			if (fstruct->input_flds[i].toggle==17)
				m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 17;
			else
				m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 5;
			m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
			m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																			current_frmid;
			XtAddCallback(m_list[current_frmid][i], XmNsingleSelectionCallback,
					(XtCallbackProc)uw_mflistCB, 
					m_pStruct[current_frmid][m_Nps[current_frmid]]);
			(m_Nps[current_frmid])++;
		}
	}
	else if (fstruct->input_flds[i].toggle == 6)
/*
......added for option button, only one button and can
......be ussed for browse button. it can have a text field
......to echo the answer
.....Yurong 8/28/97
*/
	{
		n = 0;
		XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNtopPosition,pposy); n++;
/**add temp */
		XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNbottomPosition, rposy); n++;

		XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNleftPosition,pposx); n++;

		XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNrightPosition, rposx); n++;

		XtSetArg(args[n], XmNtraversalOn, True); n++;
		XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
		XtSetArg(args[n], XmNuserData, (XtPointer)6);n++;
		m_formEntry[current_frmid][i] = 
						XmCreatePushButton(parent,
						fstruct->input_flds[i].prompt,args, n);
/*
...........Add the callback routine
*/
		m_pStruct[current_frmid][m_Nps[current_frmid]] = 
					(Pstruct *)uu_malloc(sizeof(Pstruct));
		m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 6;
		m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
		m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																		current_frmid;
		XtAddCallback(m_formEntry[current_frmid][i], XmNactivateCallback,
							uw_mfform_activateCB,
							m_pStruct[current_frmid][m_Nps[current_frmid]]);
		(m_Nps[current_frmid])++;
/*
......check if we need a shortcut key
*/
		if (fstruct->input_flds[i].shortcut[0]!='\0')
		{
			strcpy(tempstr, fstruct->input_flds[i].shortcut);
			istat = uz_mfparse_frmkey(tempstr, strlen(tempstr), &ktyp, &ksub);
			UW_form_acclkey[UW_form_acclnum].type = ktyp;
			UW_form_acclkey[UW_form_acclnum].event = ksub;
			UW_form_acclkey[UW_form_acclnum].frmid = current_frmid;
			UW_form_acclkey[UW_form_acclnum].fldno = i;
			UW_form_acclnum++;
		}
	}
/*
........Toggle field
*/
	else if (fstruct->input_flds[i].toggle == 3 ||
			fstruct->input_flds[i].toggle == 4)
	{
/*
........Put up Prompt
*/
		m_formPrompt[current_frmid][i] = 
			XtCreateWidget(fstruct->input_flds[i].prompt,
			xmLabelWidgetClass,parent,args,n);
/*
........Set field position
*/
		n = 0;
		XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNtopPosition,pposy); n++;
		XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
		XtSetArg(args[n],XmNleftWidget, m_formPrompt[current_frmid][i]); 
		n++;
		XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNrightPosition, rposx); n++;
/*
........Put up the toggle button
*/
		XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
		XtSetArg(args[n], XmNtraversalOn, True); n++;
		XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
		XtSetArg(args[n], XmNuserData, (XtPointer)3);n++;
		m_formEntry[current_frmid][i] = 
					XtCreateWidget(""/*fstruct->input_flds[i].prompt*/,
					xmToggleButtonWidgetClass,parent,
					args, n);
		setState(fstruct->input_flds[i].toggle,
				fdata->ud_data[i].ud_delem.frmint[0],
				&state);
		XmToggleButtonSetState(m_formEntry[current_frmid][i],state,False);
/*
...........Add the callback routine
*/
		m_pStruct[current_frmid][m_Nps[current_frmid]] = 
					(Pstruct *)uu_malloc(sizeof(Pstruct));
		m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 
									fstruct->input_flds[i].toggle;
		m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
		m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																		current_frmid;
		XtAddCallback(m_formEntry[current_frmid][i],XmNvalueChangedCallback,
					uw_mfform_activateCB,
					m_pStruct[current_frmid][m_Nps[current_frmid]]);
		(m_Nps[current_frmid])++;
	}
/*
.....added for drawing area
*/
	else if (fstruct->input_flds[i].toggle == 7)
	{
/*
........Put up Prompt
*/
		m_formPrompt[current_frmid][i] = 
			XtCreateWidget(fstruct->input_flds[i].prompt,
			xmLabelWidgetClass,m_winid[current_frmid],args,n);
/*
........Set field position
*/
		n = 0;
		XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
		if (posy<0)
		{
			XtSetArg(args[n],XmNtopPosition,pposy); n++;
		}
		else
		{
			XtSetArg(args[n],XmNtopPosition,posy); n++;
		}
		if (posx<0)
		{
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
			XtSetArg(args[n],XmNleftWidget, m_formPrompt[current_frmid][i]); 
			n++;
		}
		else
		{
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNleftPosition,posx); n++;
		}
		lnth = abs(fstruct->input_flds[i].ud_flen);
		picwid = abs(fstruct->input_flds[i].ud_fprec);
		pichgt = abs(fstruct->input_flds[i].ud_flen);
		m_Draw_Width[current_frmid][i] = picwid;
		m_Draw_Height[current_frmid][i] = pichgt;
		m_WC_Width[current_frmid][i] =
				abs(fstruct->input_flds[i].range[0].dint);
		m_WC_Height[current_frmid][i] =
				abs(fstruct->input_flds[i].range[1].dint);
		XtSetArg(args[n],XmNwidth, picwid); n++;
		XtSetArg(args[n],XmNheight, pichgt); n++;
		m_draw_area[current_frmid][i] = (Widget) XmCreateDrawingArea(
					parent, "draw_area", args,n);
		m_draw_array[current_frmid][i] =
				 (int*)(fdata->ud_data[i].ud_delem.frmint);
/*
...........Add the callback routine
*/
		m_pStruct[current_frmid][m_Nps[current_frmid]] = 
					(Pstruct *)uu_malloc(sizeof(Pstruct));
		m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 7;
		m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
		m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																		current_frmid;
		XtAddCallback(m_draw_area[current_frmid][i], XmNexposeCallback,
					(XtCallbackProc)drawCB,
					m_pStruct[current_frmid][m_Nps[current_frmid]]);
		(m_Nps[current_frmid])++;
		XtManageChild(m_draw_area[current_frmid][i]);
	}
/*
.....added for processbar
*/
	else if (fstruct->input_flds[i].toggle == 9)
	{
/*
........Put up Prompt
*/
		m_formPrompt[current_frmid][i] = 
			XtCreateWidget(fstruct->input_flds[i].prompt,
			xmLabelWidgetClass,m_winid[current_frmid],args,n);
/*
........Set field position
*/
		n = 0;
		XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
		if (posy<0)
		{
			XtSetArg(args[n],XmNtopPosition,pposy); n++;
		}
		else
		{
			XtSetArg(args[n],XmNtopPosition,posy); n++;
		}
		if (posx<0)
		{
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
			XtSetArg(args[n],XmNleftWidget, m_formPrompt[current_frmid][i]); 
			n++;
		}
		else
		{
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNleftPosition,posx); n++;
		}

		XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
/*
.....10 pixel for percentage label if label need displayed
*/
		if (fstruct->input_flds[i].ud_datatyp==UD_DASINT)
		{
			XtSetArg(args[n],XmNrightPosition, rposx-10); n++;
		}
		else
		{
			XtSetArg(args[n],XmNrightPosition, rposx); n++;
		}
		XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
		XtSetArg(args[n],XmNbottomPosition, rposy); n++;
		m_process_bar[current_frmid][i] = (Widget) XmCreateDrawingArea(
					parent, "processbar", args,n);
/*
...........Add the callback routine
*/
		m_pStruct[current_frmid][m_Nps[current_frmid]] = 
					(Pstruct *)uu_malloc(sizeof(Pstruct));
		m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 9;
		m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
		m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																		current_frmid;
		XtAddCallback(m_process_bar[current_frmid][i], XmNexposeCallback,
					(XtCallbackProc)drawCB2,
					m_pStruct[current_frmid][m_Nps[current_frmid]]);
		(m_Nps[current_frmid])++;
		if (fdata->ud_data[i].ud_delem.frmint!=NULL)
			m_percent[current_frmid][i] = 
						m_fData[current_frmid].ud_data[i].ud_delem.frmint[0];
		else
			m_percent[current_frmid][i] = 10;
		XtManageChild(m_process_bar[current_frmid][i]);
/*
......if TYPE =DASINT, display % above the center of progress bar
*/
		if (fstruct->input_flds[i].ud_datatyp==UD_DASINT)
		{
/*
........Set field position
*/
			n = 0;
/*
			XtSetArg(args[n],XmNbottomAttachment, XmATTACH_WIDGET); n++;
			XtSetArg(args[n],XmNbottomWidget, 
								m_process_bar[current_frmid][i]); 
			n++;
*/
			XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNbottomPosition, rposy); n++;
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
			XtSetArg(args[n],XmNleftWidget, m_process_bar[current_frmid][i]); 
			n++;
/*
			if (posx<0)
			{
				XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
				XtSetArg(args[n],XmNleftWidget, m_formPrompt[current_frmid][i]); 
				n++;
			}
			else
			{
				XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
				XtSetArg(args[n],XmNleftPosition,posx); n++;
			}
			XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNrightPosition, rposx); n++;
*/
			sprintf(perc_str, "%d%%", 100);
			m_process_label[current_frmid][i] = XtCreateWidget(perc_str,
							xmLabelWidgetClass,m_winid[current_frmid],args,n);
			XtManageChild(m_process_label[current_frmid][i]);
		}
	}
/*
........Text field entry
*/
	else
	{
/*
........Put up Prompt or "PICK", "LOCATE" button
........depend on ud_input value
*/
		if (fstruct->input_flds[i].ud_input==FORM_STRING)
		{
			m_formPrompt[current_frmid][i] = 
							XtCreateWidget(fstruct->input_flds[i].prompt,
							xmLabelWidgetClass,parent,args,n);
		}
		else if (fstruct->input_flds[i].ud_input==FORM_PICK)
		{
			XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
			if (posx>0)
			{
				XtSetArg(args[n],XmNrightPosition, posx); n++;
			}
			else
			{
				XtSetArg(args[n],XmNrightPosition, rposx); n++;
			}
			m_formPrompt[current_frmid][i] = 
							XmCreatePushButton(parent,
							fstruct->input_flds[i].prompt, args, n);
			m_pStruct[current_frmid][m_Nps[current_frmid]] = 
								(Pstruct *)uu_malloc(sizeof(Pstruct));
			m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 1;
			m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
			m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																		current_frmid;
			XtAddCallback(m_formPrompt[current_frmid][i], 
						XmNactivateCallback,
						uw_mfform_pickCB, 
						m_pStruct[current_frmid][m_Nps[current_frmid]]);
			(m_Nps[current_frmid])++;
		}
		else if (fstruct->input_flds[i].ud_input==FORM_LOCATE)
		{
			XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
			if (posx>0)
			{
				XtSetArg(args[n],XmNrightPosition, posx); n++;
			}
			else
			{
				XtSetArg(args[n],XmNrightPosition, rposx); n++;
			}
			m_formPrompt[current_frmid][i] = 
						XmCreatePushButton(parent,
						fstruct->input_flds[i].prompt, args, n);
			m_pStruct[current_frmid][m_Nps[current_frmid]] = 
						(Pstruct *)uu_malloc(sizeof(Pstruct));
			m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 2;
			m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
			m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																		current_frmid;
			XtAddCallback(m_formPrompt[current_frmid][i], 
						XmNactivateCallback,
						uw_mfform_pickCB, 
						m_pStruct[current_frmid][m_Nps[current_frmid]]);
			(m_Nps[current_frmid])++;
		}
		else
		{
			m_formPrompt[current_frmid][i] = 
							XtCreateWidget(fstruct->input_flds[i].prompt,
							xmLabelWidgetClass,parent,args,n);
		}
/*
........Set field position
*/
		n = 0;
		XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
		if (posy<0)
		{
			XtSetArg(args[n],XmNtopPosition,pposy); n++;
		}
		else
		{
			XtSetArg(args[n],XmNtopPosition,posy); n++;
		}
/*
.....not use attach widget but use position now 
.....if has the option position set
*/
		if (posx<0)
		{
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
			XtSetArg(args[n],XmNleftWidget, m_formPrompt[current_frmid][i]);
			n++;
		}
		else
		{
			XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNleftPosition,posx); n++;
		}
		if (fstruct->input_flds[i].toggle == 11)
		{
			XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNbottomPosition, rposy); n++;
			XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
			XtSetArg(args[n],XmNrightPosition, rposx); n++;
		}
		else
		{
			lnth = abs(fstruct->input_flds[i].ud_flen);
			XtSetArg(args[n],XmNcolumns, lnth); n++;
		}
/*
.....if it is display type, create as readonly and multiple line
*/
		if (fstruct->input_flds[i].toggle == 8)
		{
			XtSetArg(args[n],XmNeditable,False); n++;
			XtSetArg(args[n],XmNeditMode,XmMULTI_LINE_EDIT); n++;
		}
		else
		{
			XtSetArg(args[n],XmNeditable,True); n++;
			if (fstruct->input_flds[i].toggle == 11)
			{
				XtSetArg(args[n],XmNeditMode,XmMULTI_LINE_EDIT); n++;
			}
			else
			{
				XtSetArg(args[n],XmNeditMode,XmSINGLE_LINE_EDIT); n++;
			}
		}
		XtSetArg(args[n],XmNmarginHeight,2); n++;
/*
...........Format the default answer
*/
		if (fdata->ud_data[i].ud_delem.frmstr!=NULL)
		{
			ud_formstr_format(fstruct->input_flds[i].ud_datatyp,
				fdata->ud_data[i].ud_delem,fstruct->input_flds[i].ud_fprec,
				fstruct->input_flds[i].ud_flen,ldat);
		}
		else
		{
			ldat[0] = '\0';
		}
		sprintf(buf,"form_%d",i);
		if ((fstruct->input_flds[i].toggle == 1)
				|| (fstruct->input_flds[i].toggle == 11))
		{
			XtSetArg(args[n], XmNtraversalOn, True); n++;
			XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
		}
		else
		{
			XtSetArg(args[n], XmNtraversalOn, False); n++;
		}

		XtSetArg(args[n], XmNuserData, (XtPointer)1);n++;
/*
.....this statement will cause the end of text not showing
.....if the text is longer than the text field
.....Yurong 10/31/00
				XtSetArg(args[n], XmNautoShowCursorPosition, False);n++;
*/
/*
.....Set text selection method
*/
		if (UW_frmtext_select)
		{
			XtSetArg(args[n], XmNselectionArray, sarray);n++;
			XtSetArg(args[n], XmNselectionArrayCount, 4);n++;
		}
		XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
		m_formEntry[current_frmid][i] = XtCreateWidget(buf,
					xmTextWidgetClass,parent,args,n);
/*
.....set default answer
*/
		XmTextSetString(m_formEntry[current_frmid][i], ldat);
/*
...........Add the callback routine
*/
		m_pStruct[current_frmid][m_Nps[current_frmid]] = 
									(Pstruct *)uu_malloc(sizeof(Pstruct));
		m_pStruct[current_frmid][m_Nps[current_frmid]]->type = 1;
		m_pStruct[current_frmid][m_Nps[current_frmid]]->fld = i;
		m_pStruct[current_frmid][m_Nps[current_frmid]]->frmid =
																			current_frmid;
		XtAddCallback(m_formEntry[current_frmid][i],XmNfocusCallback,
							uw_mfform_focusCB,
							m_pStruct[current_frmid][m_Nps[current_frmid]]);
/*
.....added called back for return
*/
		XtAddCallback(m_formEntry[current_frmid][i],XmNactivateCallback,
						uw_mfform_textretCB, 
						m_pStruct[current_frmid][m_Nps[current_frmid]]);
		XtAddCallback(m_formEntry[current_frmid][i],
					XmNlosingFocusCallback,
					uw_mfform_editdoneCB,
					m_pStruct[current_frmid][m_Nps[current_frmid]]);
		(m_Nps[current_frmid])++;
	}
	uw_mfadj_frmfld1(fstruct, current_frmid, i);
/*
........Set the Field sensitivity
*/
	if (fstruct->traverse_mask[i] == 0)
	{
		if (m_formPrompt[current_frmid][i] != NULL) 
			XtSetSensitive(m_formPrompt[current_frmid][i],False);
		if ((m_formEntry[current_frmid]!=NULL)&&
				(m_formEntry[current_frmid][i]!=NULL))
			XtSetSensitive(m_formEntry[current_frmid][i],False);
		if (fstruct->input_flds[i].toggle == 5)
		{
			if ((m_list[current_frmid]!=NULL) &&
					(m_list[current_frmid][i]!=NULL))
				XtSetSensitive(m_list[current_frmid][i], False);
			if ((m_textPrompt[current_frmid]!=NULL) &&
					(m_textPrompt[current_frmid][i]!=NULL))
				XtSetSensitive(m_textPrompt[current_frmid][i], False);
		}
	}
/*
........Manage the form entry
........if visible
*/
	if (fstruct->ud_display_mask[inc+i] == 1)
	{
		if (m_formPrompt[current_frmid][i] != NULL) 
			XtManageChild(m_formPrompt[current_frmid][i]);
		if (m_formEntry[current_frmid][i] != NULL) 
			XtManageChild(m_formEntry[current_frmid][i]);
/*
.....added for List
.....Yurong 8/15/97
*/
		if ((fstruct->input_flds[i].toggle == 5)
				|| (fstruct->input_flds[i].toggle == 17))
		{
			if ((m_textPrompt[current_frmid]!=NULL) &&
					(m_textPrompt[current_frmid][i]!=NULL))
				XtManageChild(m_textPrompt[current_frmid][i]);
			XtManageChild(m_list[current_frmid][i]);
			if (form_list->num_item > 0)
			{
				for (j=0;j<form_list->num_item;j++)
					XmStringFree(str_list[j]);
				XtFree((char *)str_list);
			}
		}
	}
	goto done;
failed:;
	status = UU_FAILURE;
done:;
	return status;
}

/**********************************************************************
**    I_FUNCTION : uw_mfform_playb(fstruct,fdata);
**       Read the record file to get form's data
**    PARAMETERS
**       INPUT  :
**          fstruct	Form structure.
**				fdata		Default form data.
**       OUTPUT :
**          fstruct	Form structure.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_mfform_playb(frmid, fstruct,fdata)
int frmid;
UD_FSTRUCT *fstruct;
UD_FDATA *fdata;
{
	int status;
	if (frmid<0) return -1;
   uu_move_byte((char*)fstruct, (char*)&(m_fStruct[frmid]), 
						sizeof(UD_FSTRUCT));
	uu_move_byte((char*)fdata, (char*)&(m_fData[frmid]), 
						sizeof(UD_FDATA));
	status = ud_rdform(&m_fStruct[frmid], &m_fData[frmid], fstruct, fdata);
	return status;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfform_ckdata(fno,buf)
**       Checks form input data for correct type and values.
**    PARAMETERS
**       INPUT  :
**          fno     = Field number to check.
**				buf     = Form input data to check.
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfform_ckdata(frmid, fno,buf)
int fno, frmid;
char *buf;
{
	UD_DASDATA dsda;
	UD_DASIN de;
	UD_LIST *list_ans;
	int irtn,typ,nc,i,ifl, stat;
	char text[80], erms[80];

	if (frmid<0) return UU_FALSE;
/*
.....Initialize routine
*/
	irtn = UU_TRUE;
	typ = m_fStruct[frmid].input_flds[fno].ud_datatyp;
	stat = 0;
	for (i=strlen(buf)-1;i>=0;i--) if (buf[i] > ' ') break;
	buf[i+1] = '\0';

	if ((typ>=UD_DASSCALAR) && (typ<=UD_SCAUNITLESS))
	{
/*
.....Check if the string is the scalar value/string
.....even though we may keep and return string, but
.....we still need get the value in order to check the range
*/
		strcpy(text,buf);
/*
.....Get the scalar and convert to value
*/
		stat = ncl_parse_scalar_values(text, text, typ);
		if (stat==-1)
		{
			sprintf(erms,"%s is not a valid scalar value",buf);
			uw_mferror(erms);
			return UU_FALSE;
		}
	}

	if (typ == UD_DASVAL || typ == UD_DASUNITLESS ||
			typ == UD_DASDISTANCE || typ == UD_DASANGLE ||
			typ == UD_DASINT || typ == UD_DASVEC ||
			typ == UD_DASCART )
	{
		strcpy(text,buf);
/*
.....Get the scalar and convert to value
*/
		stat = ncl_parse_scalar_values(text, text,typ);
		if (stat==-1)
		{
			sprintf(erms,"%s is not a valid scalar value",buf);
			uw_mferror(erms);
			return UU_FALSE;
		}
	}
/*
.....Parse if not string input
*/
	if (typ != UD_DASSTRING)
	{
/*
.....if it's a scalar string value, use text get above
*/
		if (stat!=1)
			strcpy(text,buf);
		nc = strlen(text);
		ul_strip_blanks(text,&nc);
		ifl = 0;
		if (typ == UD_DASVAL || typ == UD_DASUNITLESS ||
		    typ == UD_DASINT || typ == UD_DASVEC || typ == UD_DASNDC)
		        ifl = UD_UNITLESS;
		else if (typ == UD_DASANGLE) ifl = UD_DANGLE;
		irtn = ud_dasin(text,&dsda,ifl);
		if (!irtn) goto failed;
/*
.....handle for for scalar
*/
		if ((typ>=UD_DASSCALAR) && (typ<=UD_SCAUNITLESS))
		{
			ud_scatodas(&dsda, &de, typ);
			stat = ud_ckdata(m_fStruct[frmid], &de, fno);
			if (stat != UD_VALOK) goto failed;
			else
			{
				strcpy(m_fData[frmid].ud_data[fno].ud_delem.frmstr, buf);		
				goto done;
			}
		}
	}
/*
.....Verify correct type of input
*/
	switch (typ)
	{
/*
.....Allow for the MODSYS matrix
*/
	case UD_DASVEC:
		if (dsda.dtype != 2) goto failed;
		um_ccstomcs(1,dsda.stval.stcord.coord,
				m_fData[frmid].ud_data[fno].ud_delem.frmvec);
/*
......adjust the value
*/
		UM_cc_exttoint(m_fData[frmid].ud_data[fno].ud_delem.frmvec, 
			m_fData[frmid].ud_data[fno].ud_delem.frmvec);
		break;
	case UD_DASCART:
		if (dsda.dtype != 2) goto failed;
		um_ccstomcs(0,dsda.stval.stcord.coord,
				m_fData[frmid].ud_data[fno].ud_delem.frmvec);
/*
......adjust the value
*/
		UM_cc_exttoint(m_fData[frmid].ud_data[fno].ud_delem.frmvec, 
			m_fData[frmid].ud_data[fno].ud_delem.frmvec);
		break;
	case UD_DASNDC:
		if (dsda.dtype != 2) goto failed;
		m_fData[frmid].ud_data[fno].ud_delem.frmvec[0] = 
											dsda.stval.stcord.coord[0];
		m_fData[frmid].ud_data[fno].ud_delem.frmvec[1] = 
											dsda.stval.stcord.coord[1];
		m_fData[frmid].ud_data[fno].ud_delem.frmvec[2] = 
											dsda.stval.stcord.coord[2];
		break;
	case UD_DASVAL:
	case UD_DASUNITLESS:
		if (dsda.dtype != 1) goto failed;
		m_fData[frmid].ud_data[fno].ud_delem.frmflt[0] = dsda.stval.dval;
		break;
	case UD_DASDISTANCE:
		if (dsda.dtype != 1) goto failed;
		m_fData[frmid].ud_data[fno].ud_delem.frmflt[0] = dsda.stval.dval;
/*
......adjust the value
*/
		UM_len_exttoint(m_fData[frmid].ud_data[fno].ud_delem.frmflt[0],
			m_fData[frmid].ud_data[fno].ud_delem.frmflt[0]);
		break;
	case UD_DASANGLE:
		if (dsda.dtype != 1) goto failed;
		m_fData[frmid].ud_data[fno].ud_delem.frmflt[0] = dsda.stval.dval;
/*
......adjust the value
*/
		UM_ang_inttoext(m_fData[frmid].ud_data[fno].ud_delem.frmflt[0],
			m_fData[frmid].ud_data[fno].ud_delem.frmflt[0]);
		break;
	case UD_DASINT:
		if (dsda.dtype != 1) goto failed;
		m_fData[frmid].ud_data[fno].ud_delem.frmint[0] = dsda.stval.dval;
		break;
	case UD_DASSTRING:
/*
.....added List here
.....Yurong 8/15/97
*/
		if (m_fStruct[frmid].input_flds[fno].toggle==5)
		{
			list_ans = (UD_LIST *)(m_fData[frmid].ud_data[fno].ud_delem.frmint) ;
			if ((i!=-1)&&(list_ans->answer!=NULL))
				strcpy(list_ans->answer, buf);
		}
		else
			strcpy(m_fData[frmid].ud_data[fno].ud_delem.frmstr,buf);

		break;
	default:
		goto failed;
	}
/*
.....Check for valid range
*/
	if (typ != UD_DASSTRING)
	{
		ud_todas(&m_fData[frmid].ud_data[fno].ud_delem,&de,typ);
		stat = ud_ckdata(m_fStruct[frmid], &de,fno);
		if (stat != UD_VALOK) goto failed;
	}
	goto done;
/*
.....End of routine
*/
failed:;
	irtn = UU_FALSE;
done:;
	return(irtn);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfcreate_action(parent,actions,num_actions)
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
Widget uw_mfcreate_action(parent,actions,num_actions)
Widget parent;
ActionAreaItem *actions;
int num_actions;
{
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

/**********************************************************************
**    I_FUNCTION :  uw_mfform_redisplay(frmid)
**       Redisplays the form fields.  Usually called when enabling or
**			disabling fields based on a toggle response.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Form fields may appear, disappear, or change
**							their active state.
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_redisplay(frmid)
int frmid;
{
	int i,state, inc;
	char ldat[80];
	XmString lstr;

	if (frmid<0) return; 
	inc = m_fStruct[frmid].n_display_fields;
/*
......only if the form still exist, we redisplay them
*/
	if (UW_display_frm[frmid] == NULL) 
		return;
/*
.....Don't loop in callback routines
*/
	Sfdisplay = UU_TRUE;

/*
........Manage the form display entry
........if visible
*/
	for (i=0;i<m_fStruct[frmid].n_display_fields;i++)
	{
		if (m_fStruct[frmid].ud_display_mask[i] == 1 
						&& !XtIsManaged(m_FormDisp[frmid][i]))
			XtManageChild(m_FormDisp[frmid][i]);
		if (m_fStruct[frmid].ud_display_mask[i] == 0 
						&& XtIsManaged(m_FormDisp[frmid][i]))
			XtUnmanageChild(m_FormDisp[frmid][i]);
	}
/*
.....Set the default answers
*/
	for (i=0;i<m_fStruct[frmid].n_input_fields;i++)
	{
/*
........Prompt
*/
		if (m_formPrompt[frmid][i] != NULL)
		{
/*
......added update prompt label
*/
			if (m_fStruct[frmid].input_flds[i].ud_echo==2)
			{
				lstr = XmStringCreateSimple(m_fStruct[frmid].input_flds[i].prompt);
				XtVaSetValues(m_formPrompt[frmid][i], 
								XmNlabelString, lstr, NULL);
				XmStringFree(lstr);
				m_fStruct[frmid].input_flds[i].ud_echo = 0;
			}
			if (m_fStruct[frmid].traverse_mask[i] == 0)
				XtSetSensitive(m_formPrompt[frmid][i],False);
			else
				XtSetSensitive(m_formPrompt[frmid][i],True);
			if (m_fStruct[frmid].ud_display_mask[i+inc] == 1 &&
										!XtIsManaged(m_formPrompt[frmid][i]))
				XtManageChild(m_formPrompt[frmid][i]);
			if (m_fStruct[frmid].ud_display_mask[i+inc] == 0 &&
										XtIsManaged(m_formPrompt[frmid][i]))
				XtUnmanageChild(m_formPrompt[frmid][i]);
		}
		if (m_fStruct[frmid].input_flds[i].toggle == 5)
		{
			if (m_fStruct[frmid].traverse_mask[i] == 0)
			{
				XtSetSensitive(m_list[frmid][i], False);
				if ((m_textPrompt[frmid]!=NULL) &&
						(m_textPrompt[frmid][i]!=NULL))
					XtSetSensitive(m_textPrompt[frmid][i], False);
			}
			else
			{
				XtSetSensitive(m_list[frmid][i], True);
				if ((m_textPrompt[frmid]!=NULL) &&
					(m_textPrompt[frmid][i]!=NULL))
					XtSetSensitive(m_textPrompt[frmid][i], True);
			}
			if (m_fStruct[frmid].ud_display_mask[i+inc] == 1 &&
										!XtIsManaged(m_formPrompt[frmid][i]))
			{
				XtManageChild(m_list[frmid][i]);
				if ((m_textPrompt[frmid]!=NULL) && 
						(m_textPrompt[frmid][i]!=NULL))
					XtManageChild(m_textPrompt[frmid][i]);
			}
			if (m_fStruct[frmid].ud_display_mask[i+inc] == 0 &&
										XtIsManaged(m_formPrompt[frmid][i]))
			{
				XtUnmanageChild(m_list[frmid][i]);
				if ((m_textPrompt[frmid]!=NULL) &&
						(m_textPrompt[frmid][i]!=NULL))
					XtUnmanageChild(m_textPrompt[frmid][i]);
			}
		}
/*
........Choice field
*/
		if (m_fStruct[frmid].input_flds[i].toggle == 2)
		{
			if ((m_fData[frmid].ud_data[i].ud_delem.frmint[0]<0)
					&& (m_fStruct[frmid].input_flds[i].ud_input==1))
				XtVaSetValues(m_formEntry[frmid][i],XmNmenuHistory,
					m_formChoice[frmid][i][16],NULL);
			else
				XtVaSetValues(m_formEntry[frmid][i],XmNmenuHistory,
					m_formChoice[frmid][i][m_fData[frmid].ud_data[i].ud_delem.frmint[0]],NULL);
		}
/*
.....added for button
.....Yurong 8/28/97
*/
		else if (m_fStruct[frmid].input_flds[i].toggle == 6)
		{
			;
		}
/*
........Toggle field
*/
		else if (m_fStruct[frmid].input_flds[i].toggle == 3 ||
			m_fStruct[frmid].input_flds[i].toggle == 4)
		{
			setState(m_fStruct[frmid].input_flds[i].toggle,
				m_fData[frmid].ud_data[i].ud_delem.frmint[0],
				&state);
			XmToggleButtonSetState(m_formEntry[frmid][i],state,False);
		}
/*
......picture area
*/
		else if (m_fStruct[frmid].input_flds[i].toggle == 7)
		{
			if (m_fData[frmid].ud_data[i].dflg==2)
			{
				m_fData[frmid].ud_data[i].dflg = 1;
				m_draw_array[frmid][i] =
						 (int*)(m_fData[frmid].ud_data[i].ud_delem.frmint);

				m_pStruct[frmid][m_Nps[frmid]] = 
							(Pstruct *)uu_malloc(sizeof(Pstruct));
				m_pStruct[frmid][m_Nps[frmid]]->type = 7;
				m_pStruct[frmid][m_Nps[frmid]]->fld = i;
				m_pStruct[frmid][m_Nps[frmid]]->frmid = frmid;
				drawCB(m_draw_area[frmid][i], 
								m_pStruct[frmid][m_Nps[frmid]],
								NULL);
				(m_Nps[frmid])++;
			}
		}
		else if (m_fStruct[frmid].input_flds[i].toggle == 9)
		{
			m_percent[frmid][i] = 
						m_fData[frmid].ud_data[i].ud_delem.frmint[0];
			m_pStruct[frmid][m_Nps[frmid]] = 
						(Pstruct *)uu_malloc(sizeof(Pstruct));
			m_pStruct[frmid][m_Nps[frmid]]->type = 9;
			m_pStruct[frmid][m_Nps[frmid]]->fld = i;
			m_pStruct[frmid][m_Nps[frmid]]->frmid = frmid;
			drawCB2(m_process_bar[frmid][i], 
							m_pStruct[frmid][m_Nps[frmid]],
							NULL);
			(m_Nps[frmid])++;
		}
/*
........Text field
*/
		else
		{
/*
.....check flag to see if we need reset
.....added Yurong 6/17/98
*/
			if (m_fData[frmid].ud_data[i].dflg==2)
			{
				ud_formstr_format(m_fStruct[frmid].input_flds[i].ud_datatyp,
					m_fData[frmid].ud_data[i].ud_delem,
					m_fStruct[frmid].input_flds[i].ud_fprec,
					m_fStruct[frmid].input_flds[i].ud_flen,ldat);
/*
.....Reset fData->ud_data[i].dflg back to 1
.....to avoid to goes to endless loop
.....because now we allow user have callback on text value
.....change
.....Yurong 4/25/00
*/
				m_fData[frmid].ud_data[i].dflg = 1;
				XmTextSetString(m_formEntry[frmid][i],ldat);
			}
		}
/*
.....Display the field
*/
		if ((m_formEntry[frmid]!=NULL) &&
				(m_formEntry[frmid][i]!=NULL))
		{
			if (m_fStruct[frmid].traverse_mask[i] == 0)
				XtSetSensitive(m_formEntry[frmid][i],False);
			else
				XtSetSensitive(m_formEntry[frmid][i],True);
			if (m_fStruct[frmid].ud_display_mask[i+inc] == 1 &&
											!XtIsManaged(m_formEntry[frmid][i]))
				XtManageChild(m_formEntry[frmid][i]);
			if (m_fStruct[frmid].ud_display_mask[i+inc] == 0 &&
											XtIsManaged(m_formEntry[frmid][i]))
				XtUnmanageChild(m_formEntry[frmid][i]);
		}
	}
	Sfdisplay = UU_FALSE;
}

/**********************************************************************
**    I_FUNCTION :  uw_removechild(frmid, parentID)
**       Remove a child form from a child list of parent 
**    PARAMETERS
**       INPUT  :
**          frmid   = Child form ID
**				parentID: Parent form ID
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void uw_removechild(frmid, parentID)
int frmid, parentID;
{
	int i, rmnum;
	rmnum = -1; 
	if (frmid<0) return;
	for (i=0; i<m_childnum[parentID];i++)
	{
		if (m_childfrm[parentID][i]==frmid)
		{
			m_childfrm[parentID][i] = -1;
			rmnum = i;
			break;
		}
	}
	if (rmnum==-1)
		return;
	for (i=rmnum; i<m_childnum[parentID]-1;i++)
	{
		m_childfrm[parentID][i] = m_childfrm[parentID][i+1];
	}
	m_childnum[parentID]--;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfform_close(frmid)
**       Closes the requested form.
**    PARAMETERS
**       INPUT  :
**          frmid   = Form widget ID.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_close(frmid)
int frmid;
{
	int i, fnum, stat;
	if (frmid<0) return;
	uw_mfsetcursor(21);
/*
.....close child form first
*/
	if (m_childnum[frmid]!=0)
	{
		for (i=0; i<m_childnum[frmid];i++)
			uw_mfclose_dispfrm(m_childfrm[frmid][i]);
	}
	m_childnum[frmid] = 0;
	if (m_parentID[frmid]!=-1)
	{
		uw_removechild(frmid, m_parentID[frmid]);
		active_frmid = m_parentID[frmid];
		m_parentID[frmid] = -1;
	}
	else
		active_frmid = -1;

/*
.....Let the calling routines know that
.....form input is completed
*/
	m_formEof[frmid] = True;
	if (frmid==0)
	{
		formActive = False;
		formOn = False;
/*
.....always set UD_form_bypick = 0, only
.....when form in pick mode, UD_form_bypick = 1;
.....we reset this value to make sure UD_form_bypick is
.....always = 0 when form not displayed (this value is
.....used by ud_gevt1, if we are in form PICK mode,
.....we will redisplay form instead of accept string value
.....when "BY TEXT"
.....Yurong 12/5/00
*/
		UD_form_bypick = 0;
/*
.....close help window and pocket window that opened for form
*/

		if (UW_Form_pocket == 1)
			um_close_pocket_window();
		if (UW_Form_help == 1)
			uw_close_hlpwin();

		UW_Form_pocket = 0;
		UW_Form_help = 0;
		active_frmid = -1;
	}
	else
	{
/*
......allow user call "close" function
*/
/*
.....when close display type form, save the data
*/
		fnum = m_fStruct[frmid].n_input_fields;
		stat = uw_mfaccept_formdata(frmid);
		if (stat==0) return;
		if (formActFrm==frmid)
		{
			formActFrm = -1;
		}
		if (m_fStruct[frmid].input_flds[fnum].method_returns &&
				m_fStruct[frmid].input_flds[fnum].method != NULL)
		{
			stat = (*m_fStruct[frmid].input_flds[fnum].method)
					(&frmid, NULL, UD_TFWD);
		}
	}
/*
......free form structure & data space
*/
/*
.....free this the last because we may use it when we free form widget data
.....
	ud_delform(&(m_fStruct[frmid]));
	uu_free(m_fData[frmid].ud_data);
*/
	if (formActFrm==frmid)
	{
		formActFrm = -1;
	}
/*
.....Get rid of the form
*/
	XtUnmanageChild(UW_display_frm[frmid]);
	XtDestroyWidget(UW_display_frm[frmid]);
	XmUpdateDisplay(UW_display_frm[frmid]);
	UW_display_frm[frmid] = NULL;
	UW_first_resize[frmid] = 0;
/*
...added for recording
*/
	if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
	{
		if (frmid==0)
		{
			ud_rpwrform("DONE","0",NULL);
		}
		else
			ud_rpwrform("DONE","1",NULL);
	}
/*
.....Free allocated form data
*/
	uw_mffree_formdata(frmid);
/*
......free form structure & data space
*/
	ud_delform(&(m_fStruct[frmid]));
	uu_free(m_fData[frmid].ud_data);
}

void uw_mffree_formdata(frmid)
int frmid;
{
	int i;

	if (frmid<0) return;
	if (m_formFrame[frmid] != NULL)
	{
		uu_free(m_formFrame[frmid]); 
		m_formFrame[frmid] = NULL;
	}
	if (m_formForm[frmid] != NULL)
	{
		uu_free(m_formForm[frmid]); 
		m_formForm[frmid] = NULL;
	}
	if (m_frametitle[frmid] != NULL)
	{
		uu_free(m_frametitle[frmid]);
		m_frametitle[frmid] = NULL;
	}
	if (m_formEntry[frmid] != NULL)
	{
		uu_free(m_formEntry[frmid]); 
		m_formEntry[frmid] = NULL;
	}
	if (m_FormDisp[frmid] != NULL)
	{
		uu_free(m_FormDisp[frmid]); 
		m_FormDisp[frmid] = NULL;
	}
	if (m_formChoice[frmid] != NULL)
	{
		for (i=0; i<m_fStruct[frmid].n_input_fields; i++)
		{
			if (m_formChoice[frmid][i] != NULL)
			{
				uu_free(m_formChoice[frmid][i]); 
				m_formChoice[frmid][i] = NULL;
			}
		}
		uu_free(m_formChoice[frmid]); 
		m_formChoice[frmid] = NULL;
	}
	if (m_formPrompt[frmid] != NULL)
	{
		uu_free(m_formPrompt[frmid]);
		m_formPrompt[frmid] = NULL;
	}
	if (m_list[frmid] != NULL)
	{
		uu_free(m_list[frmid]);
		m_list[frmid] = NULL;
	}
	if (m_textPrompt[frmid] != NULL)
	{
		uu_free(m_textPrompt[frmid]);
		m_textPrompt[frmid] = NULL;
	}
	if (m_formMenu[frmid] != NULL)
	{
		uu_free(m_formMenu[frmid]);
		m_formMenu[frmid] = NULL;
	}
	if (m_draw_area[frmid] != NULL)
	{
		uu_free(m_draw_area[frmid]);
		m_draw_area[frmid] = NULL;
	}
	if (m_Draw_gc[frmid] != NULL)
	{
		uu_free(m_Draw_gc[frmid]);
		m_Draw_gc[frmid] = NULL;
	}
	if (m_Draw_Width[frmid] != NULL)
	{
		uu_free(m_Draw_Width[frmid]);
		m_Draw_Width[frmid] = NULL;
	}
	if (m_WC_Width[frmid] != NULL)
	{
		uu_free(m_WC_Width[frmid]);
		m_WC_Width[frmid] = NULL;
	}
	if (m_Draw_Height[frmid] != NULL)
	{
		uu_free(m_Draw_Height[frmid]);
		m_Draw_Height[frmid] = NULL;
	}
	if (m_WC_Height[frmid] != NULL)
	{
		uu_free(m_WC_Height[frmid]);
		m_WC_Height[frmid] = NULL;
	}
	if (m_draw_array[frmid] != NULL)
	{
		uu_free(m_draw_array[frmid]);
		m_draw_array[frmid] = NULL;
	}
	if (m_process_bar[frmid] != NULL)
	{
		uu_free(m_process_bar[frmid]);
		m_process_bar[frmid] = NULL;
	}
	if (m_process_gc[frmid] != NULL)
	{
		uu_free(m_process_gc[frmid]);
		m_process_gc[frmid] = NULL;
	}
	if (m_percent[frmid] != NULL)
	{
		uu_free(m_percent[frmid]);
		m_percent[frmid] = NULL;
	}
	if (m_pStruct[frmid] != NULL)
	{
		for (i=0;i<m_Nps[frmid];i++)
		{
			if ((m_pStruct[frmid]!=NULL) && (m_pStruct[frmid][i]!=NULL))
			{
				uu_free(m_pStruct[frmid][i]);
				m_pStruct[frmid][i] = NULL;
			}
		}
		uu_free(m_pStruct[frmid]);
		m_pStruct[frmid] = NULL;
	}	
	for (i=0; i<200;i++)
	{
		ud_free_flist(&Sflist[i]);
	}
	return;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfform_editdoneCB(widget, client_data, call_data)
**       Callback for losing focus of a edit field
**			
**    PARAMETERS
**       INPUT  :
**          widget      = Text field widget.
**				client_data = Contains field number and type.
**				call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_editdoneCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	XmTextVerifyCallbackStruct *cb=(XmTextVerifyCallbackStruct *)call_data;
	Pstruct *ps=(Pstruct *)client_data;
/*
.....Text field entry
*/
	if (ps->type == 1 && cb->reason ==  XmCR_LOSING_FOCUS)
	{
		formInpWin = NULL;
/*
......save current focus field
*/
		current_field.fld = ps->fld;
		current_field.frmid = ps->frmid;
		current_field.type = ps->type;
		current_field.chc = ps->chc;
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_mfchk_field(frmid, i, flag)
**       Check form field data format and optional call
**			user callback function
**    PARAMETERS
**       INPUT  :
**   			frmid: form ID
**				i: field number			
**				flag: 1: call user defined function if check data no err
**						0: don't call user defined function	
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfchk_field(frmid, i, flag)
int frmid, i, flag;
{
	char *text;
	char ldat[80];
	char inptext[80];
	int len, stat;
	Widget widgt;
	UD_LIST *list;
	UD_DDATA seldata;
	UD_FSTAT stat2;

	if (frmid<0) return UU_TRUE;
	stat = UU_TRUE;
	widgt = m_formEntry[frmid][i];
	text = XmTextGetString(widgt);
	if(text!=NULL)
	{
		strcpy(inptext, text);
		XtFree(text);
	}
	else
		inptext[0] = '\0';
	len = strlen(inptext);
	stat2 = UD_FLDOK;
	if ((m_fStruct[frmid].input_flds[i].toggle == 5)
			&& (m_fStruct[frmid].input_flds[i].n_defaults==2))
	{
		list = (UD_LIST *)(m_fData[frmid].ud_data[i].ud_delem.frmint);
		strcpy(list->answer, text);
		if (flag)
		{
			seldata.frmstr = (char*)&inptext;
			stat2 = (*m_fStruct[frmid].input_flds[i].method)
				(&i, &seldata, UD_TFWD);
		}
		if (stat2==UD_BADREQ)
			return -1;
		return 1;
	}
	else
	{
		stat = uw_mfform_ckdata(frmid, i,inptext);
		if (stat==UU_FALSE)
		{
/**
.....can't call XmProcessTraversal inside  focusCallback routine
*/
/***
		temp = m_formEntry[frmid][i];
		XmProcessTraversal(temp, XmTRAVERSE_CURRENT);
		formField = i;
		formInpWin = temp;
***/
			return -1;
		}
/*
.....if it is a scalar value, leave it as it is
.....do not format to numbers
*/ 
		if ((!((m_fStruct[frmid].input_flds[i].ud_datatyp>=UD_DASSCALAR) && 
			 (m_fStruct[frmid].input_flds[i].ud_datatyp<=UD_SCAUNITLESS)))
				&& (m_fStruct[frmid].input_flds[i].toggle==1))
		{
			ud_formstr_format(m_fStruct[frmid].input_flds[i].ud_datatyp,
					m_fData[frmid].ud_data[i].ud_delem,
					m_fStruct[frmid].input_flds[i].ud_fprec,
					m_fStruct[frmid].input_flds[i].ud_flen,ldat);
			XmTextSetString(m_formEntry[frmid][i], ldat);
		}
	}
/*
.....Call user defined method
.....if present
*/
	if (flag==0)
		return -1;
	if (m_fStruct[frmid].input_flds[i].method_returns & UD_TOGGLE &&
				m_fStruct[frmid].input_flds[i].method != NULL)
	{
		stat2 = (*m_fStruct[frmid].input_flds[i].method)
				(&i, &(m_fData[frmid].ud_data[i].ud_delem), UD_TFWD);
/*
.....allow cancel the redisplay
.....from method
.....in most text value change callback function
.....we don't want to redisplay it because in edit one text field
.....may call this function many times and we may only want update
.....form once
.....Yurong 4/24/00
*/
		if (stat2==UD_BADREQ)
			return -1;	
		if (i != -1)
		{
			uw_mfform_redisplay(frmid);
		}
	}
	return 1;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfform_focusCB(widget, client_data, call_data)
**       Callback for getting focus of a edit field
**			
**    PARAMETERS
**       INPUT  :
**          widget      = Text field widget.
**				client_data = Contains field number and type.
**				call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_focusCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int stat;
	XmAnyCallbackStruct *cb=(XmAnyCallbackStruct *)call_data;
	Pstruct *ps=(Pstruct *)client_data;
/*
.....Set current field
.....set this before we call any other function in case it may use it
*/
	formField = ps->fld;
	formActFrm = ps->frmid;
/*
.....Text field entry
*/
	if (ps->type == 1 && cb->reason == XmCR_FOCUS)
	{
/*
......get focus from other edit field
......reformat text in previous text filed
*/
		if ((current_field.frmid == ps->frmid)&&
				(current_field.fld == ps->fld))
		{
			formInpWin = m_formEntry[ps->frmid][ps->fld];
			formField = ps->fld;
			formActFrm = ps->frmid;
			return;
		}
		if ((current_field.frmid==ps->frmid)&&(current_field.fld!=-1))
		{
			formActFrm = ps->frmid;
			stat = uw_mfchk_field(current_field.frmid, current_field.fld, 1);
			if (stat==-1)
				return;
		}
		formField = ps->fld;
		formInpWin = m_formEntry[ps->frmid][ps->fld];
		formActFrm = ps->frmid;
	}
/*
.....Other field entry
*/
	else
	{
		formInpWin = NULL;
	}
/*
.....Set current field
*/
	formField = ps->fld;
	formActFrm = ps->frmid;
}
/**********************************************************************
**    I_FUNCTION :  uw_mflistCB(widget, client_data, call_data)
**       Call back routine for List fields.
**    PARAMETERS
**       INPUT  :
**          widget      = List field widget.
**				client_data = Contains field number (this number -1 is text
**									field number we use to echo user choice.
**				callData   = Motif callback structure, contain the select
**                       item.
**       OUTPUT :
**				client_data = Returns choice.
**    RETURNS      : none
**    SIDE EFFECTS : Disables the PICK and LOCATE buttons.
**    WARNINGS     : none
*********************************************************************/
void uw_mflistCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	XmListCallbackStruct *ptr;
	UD_FSTAT stat;
	int nc,i;
	char *string,lstr[500];
	UD_DDATA seldata;
	UD_TLIST *table_list;
	UD_TABLEINFO info;
	Pstruct *ps=(Pstruct *)client_data;
	int fnum = ps->fld;
	int frmid = ps->frmid;
	int type = ps->type;
	Widget text_entry = (Widget) m_formEntry[frmid][fnum];
	ptr = (XmListCallbackStruct*) call_data;
	XmStringGetLtoR(ptr->item, XmSTRING_DEFAULT_CHARSET,&string);
	strcpy(lstr,string);
	nc = strlen(lstr);
	formActFrm = ps->frmid;
	if (text_entry!=NULL)
	{
		for (i=500;i>nc;i--)
		{
			if (lstr[i] != ' ') lstr[i+1] = 0;
		}
		XmTextSetString(text_entry, lstr);
	}
/*
.....if the callback is from unselect
.....set seldata.frmstr NULL
*/
	if (ptr->selected_item_count==1)
		seldata.frmstr = (char*)&lstr;
	else
		seldata.frmstr = NULL;
	if (type==17)
	{
		table_list = (UD_TLIST *)((m_fData[frmid].ud_data[fnum]).ud_delem.frmint);
		info.flag = 1;
		if (ptr->selected_item_count==1)
		{
			info.row = ptr->item_position-1;
			table_list->answer = info.row;
		}
		else
		{
			info.row = -1;
			table_list->answer = -1;
		}
		if (info.row>=0)
			info.data_ptr = (int*)&(table_list->data[info.row]);
		else
			info.data_ptr = NULL;
		seldata.frmint = (int *)&info;
	}
/*
.....Call user defined method
.....if present
*/
	if (m_fStruct[frmid].input_flds[fnum].method_returns &&
			m_fStruct[frmid].input_flds[fnum].method != NULL)
	{
		stat = (*m_fStruct[frmid].input_flds[fnum].method)
				(&fnum,&seldata, UD_TFWD);
			
		if (stat==UD_BADREQ)
			return;
/*
......allow cancel redisplay function
*/
		if (fnum!=-1)
			uw_mfform_redisplay(frmid);
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_mfform_activateCB(widget, client_data, call_data)
**       Call back routine for Toggle and Choice fields.
**    PARAMETERS
**       INPUT  :
**          widget      = Toggle/Choice field widget.
**				client_data = Contains field number and choice.
**				call_data   = Motif callback structure.
**       OUTPUT :
**				client_data = Returns choice.
**    RETURNS      : none
**    SIDE EFFECTS : Disables the PICK and LOCATE buttons.
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_activateCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int stat, old_field, pi, pfrmid;
	UD_LIST *choice_list;
	XmToggleButtonCallbackStruct *cb;
	UD_DDATA seldata;
	char lstr[256];
	Pstruct *ps=(Pstruct *)client_data;
	int frmid = ps->frmid;
/*
......check previous input field to see if data format is right
*/
	formActFrm = ps->frmid;
	if ((current_field.frmid==ps->frmid)&&(current_field.fld!=-1))
	{
		stat = uw_mfchk_field(current_field.frmid, current_field.fld, 1);
		if (stat==-1)
		{
			pi = current_field.fld;
			pfrmid = current_field.frmid;
			XmProcessTraversal(m_formEntry[pfrmid][pi], XmTRAVERSE_CURRENT);
			formField = pi;
			formInpWin = m_formEntry[pfrmid][pi];
			formActFrm = ps->frmid;
			return;
		}
	} 
/*
.....Choice field
*/
	if (ps->type == 2)
	{
		m_fData[frmid].ud_data[ps->fld].ud_delem.frmint[0] = ps->chc;
/*
.....reset choice pick because uw_mfchk_field may change 'focus' to other
.....field, thus the pick will not active
*/
		XtVaSetValues(m_formEntry[frmid][ps->fld],XmNmenuHistory, 
			m_formChoice[frmid][ps->fld][ps->chc], NULL);
	}
	else if (ps->type == 10)
	{
		choice_list =
			 (UD_LIST*)(m_fData[frmid].ud_data[ps->fld].ud_delem.frmint);
		strcpy(choice_list->answer, choice_list->item[ps->chc]);
	}
/*
.....Toggle field
*/
	else if (ps->type == 3 || ps->type == 4)
	{
		cb = (XmToggleButtonCallbackStruct *)call_data;
		getState(ps->type,
			cb->set,
			&m_fData[frmid].ud_data[ps->fld].ud_delem.frmint[0]);
		formInpWin = NULL;
	}
/*
.....Call user defined method
.....if present
*/
	if (m_fStruct[frmid].input_flds[ps->fld].method_returns & UD_TOGGLE &&
			m_fStruct[frmid].input_flds[ps->fld].method != NULL)
	{
		old_field = ps->fld;
		if (ps->type == 10)
		{
			strcpy(lstr, choice_list->item[ps->chc]);
			seldata.frmstr = (char*)&lstr;
			(*m_fStruct[frmid].input_flds[ps->fld].method)
					(&ps->fld, &seldata, UD_TFWD);
		}
		else
		{
			(*m_fStruct[frmid].input_flds[ps->fld].method)
				(&ps->fld,&(m_fData[frmid].ud_data[ps->fld].ud_delem),UD_TFWD);
		}
/*
.....remember the active field number
.....used by button.
.....Yurong 8/29/97
*/
		active_number = ps->fld;
		ps->fld = old_field;
/*
.....allow cancel the redisplay
.....from method
.....Yurong 8/29/97
*/
		if (active_number != -1)
			uw_mfform_redisplay(frmid);
	}
/****0629*/
	formField = ps->fld;
	formActFrm = ps->frmid;
	current_field.frmid = ps->frmid;
	current_field.type = ps->type;
	current_field.fld = -1;
	current_field.chc = -1;
}

/**********************************************************************
**    I_FUNCTION :  uw_mfform_pickCB(widget, client_data, call_data)
**       Call back routine for the PICK and LOCATE action buttons.
**			Places NCL in picking mode and awaits for user input.
**    PARAMETERS
**       INPUT  :
**          widget      = Ignored.
**				client_data = 1 = PICK button.  2 = LOCATE button.
**				call_data   = Motif callback structure.
**       OUTPUT :
**				Places user selection in active field.
**    RETURNS      : none
**    SIDE EFFECTS : Transfers control to the DAS event loop.
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_pickCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int stat,i,prec,len,markval;
	char lstr[82], *text;
	UD_DASTAT status,ud_pick1(),ud_str1(),ud_vec1(),ud_cart1(),ud_get_pickstr();
	int frmid;
	Pstruct *ps=(Pstruct *)client_data;
	int inc, pi, pfrmid, input, modal;
	char *ud_uncord(),*ud_unvec();
/*
......check previous input field to see if data format is right
*/
	if ((current_field.frmid==ps->frmid)&&(current_field.fld!=-1))
	{
		stat = uw_mfchk_field(current_field.frmid, current_field.fld, 1);
		if (stat==-1)
		{
			pi = current_field.fld;
			pfrmid = current_field.frmid;
			XmProcessTraversal(m_formEntry[pfrmid][pi], XmTRAVERSE_CURRENT);
			formField = pi;
			formInpWin = m_formEntry[pfrmid][pi];
			return;
		}
	} 
/*
.....Initialize routine
*/
	inc = ps->type;
	i = ps->fld;
	frmid = ps->frmid;
	prec = m_fStruct[frmid].input_flds[i].ud_fprec;
	len = m_fStruct[frmid].input_flds[i].ud_flen;
	input = m_fStruct[frmid].input_flds[i].ud_input;
	modal = m_fStruct[frmid].input_flds[i].ud_modal;
	text = XmTextGetString(m_formEntry[frmid][i]);
	if (text!=NULL)
	{
		strcpy(lstr, text);
		XtFree(text);
	}
	else
		lstr[0] = '\0';
/*
.....Take down form
*/
	XtUnmanageChild(UW_display_frm[0]);
/*
.....set UD_form_bypick = 1
.....to let ud_gevt1 know we don't
.....need "BY TEXT" active
*/
	UD_form_bypick = 1;
/*
.....Trap Reject Op
*/
	UD_MARK(markval,UU_TRUE);
	if (markval != 0) goto done;
	while (1)
	{
		status = ud_get_pickstr(m_fStruct[frmid].input_flds[i].prompt, 
						m_fStruct[frmid].input_flds[i].ud_datatyp, 
						lstr, prec, 
						len, input-1);
		if (UD_form_bypick==0)
			goto done;
		if (status == DE_DONE)
		{
			goto done;
		}
/*
.....Set the new text string
*/
		XmTextSetString(m_formEntry[frmid][i],lstr);
/*
....set the focus to this field also
*/
		XmProcessTraversal(m_formEntry[frmid][i],XmTRAVERSE_CURRENT);
		if (modal==0)
			goto done;
		i++;
		if (!((i<m_fStruct[frmid].n_input_fields) && ((input==FORM_PICK) || 
				(input==FORM_LOCATE))))
			goto done;

		prec = m_fStruct[frmid].input_flds[i].ud_fprec;
		len = m_fStruct[frmid].input_flds[i].ud_flen;
		input = m_fStruct[frmid].input_flds[i].ud_input;
		modal = m_fStruct[frmid].input_flds[i].ud_modal;
/*
.....initialize the string
*/
		text = XmTextGetString(m_formEntry[frmid][i]);
		if (text!=NULL)
		{
			strcpy(lstr, text);
			XtFree(text);
		}
		else
			lstr[0] = '\0';
/*
....set the focus to this field also
*/
		XmProcessTraversal(m_formEntry[frmid][i],XmTRAVERSE_CURRENT);
	}
/*
.....Bring back the form
*/
done:;
	UD_UNMARK(markval);
	XtManageChild(UW_display_frm[frmid]);
/*
.....Set the new text field as active focus
.....move to next text field but we need set focus to current text
.....field first, because now, focus on "By Pick" button
.....changed by Yurong 4/4/00
*/
	XmProcessTraversal(m_formEntry[frmid][i],XmTRAVERSE_CURRENT);
	uw_mftxt_next(i, frmid, 1);
	UD_form_bypick = 0;
	return;
}

/**********************************************************************
**    I_FUNCTION :  uw_mfform_helpCB(widget, client_data, call_data)
**       Call back routine for the HELP action button.
**			Display help text or a pocket window
**    PARAMETERS
**       INPUT  :
**          widget      = Ignored.
**				client_data = Form dialog widget.
**				call_data   = Motif callback structure.
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_helpCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	UX_pathname dwfile, dir, fdir;
	char *indx;
	int status;
	int frmid = (int)client_data;
/*
......first check if we have a drawing file need to be displayed
......check the current directory first, then NCL_FORMS directory
*/
	strcpy(dwfile,m_fStruct[frmid].frmname);
	indx = strstr(dwfile, ".frm");
	if (indx!=NULL)
		*indx = '\0';
/*
......don't need to check the file here, um_load_pocket_drawing will
......do it. Yurong 6/3/05
*/
/*
	ul_get_full_dir(".",dir);
	mode = UX_EXISTS|UX_READ;
	status = ux_mk_chk_syspath(UU_NULL,dir,dwfile,UU_NULL,
				"UM_DRAWING_SUFFIX",&mode,fullname,UX_NPRTERRS);
	if (status != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		mode = UX_EXISTS|UX_READ;
*/
/*
.....we can't pass in NCL_FORMS into ux_mk_chk_syspath
.....it will not work
.....Yurong 12/8/00
*/
/*		dir[0] = '\0';
		ux_search_for_path("NCL_FORMS",dir,
							UX_PRTERRS|UX_NCHK|UX_NQUOTES);
		status = ux_mk_chk_syspath(UU_NULL,dir,dwfile,UU_NULL,
							"UM_DRAWING_SUFFIX",&mode,fullname,UX_NPRTERRS);
		if (status != UU_SUCCESS || mode == (mode|UX_NEXISTS))
		{
			goto textdisp;
		}
	}
	ul_remove_quotes(fullname);
	status = um_load_pocket_drawing(fullname, fullname);
*/
/*
.....don't use NCL_FORMS now, but use UU_USER_SETTINGS/forms directory first, 
.....don't display error message if not load
.....use UD_FORMDIR (which we use for loading form file)
*/
	dir[0] = '\0';
	ux_search_for_path("UU_USER_SETTINGS", dir,
						  UX_NPRTERRS|UX_NCHK|UX_NQUOTES);	
	ul_build_full_dir(dir,"forms",fdir);
/*	status = um_load_pocket_drawing(NULL, dwfile, dwfile, "NCL_FORMS", 0); */
	status = um_load_pocket_drawing(NULL, dwfile, dwfile, fdir, 0);
	if (status != UU_SUCCESS)
	{
		status = um_load_pocket_drawing(NULL, dwfile, dwfile, "UD_FORMDIR", 0);
	}

	if (status == UU_SUCCESS)
	{
		 UW_Form_pocket = 1;
	}
textdisp:;
	if (m_fStruct[frmid].frmhelp==NULL)
	{
		if (UW_Form_pocket!=1)
		{
			uw_mferror("No help text and drawing file available!");
		}
		return;
	}
/*
.....open a help scroll window to display help text
*/
	uw_open_hlpwin(m_fStruct[frmid].ud_frmdf.ud_fid);
	if (m_fStruct[frmid].frmhelp!=NULL)
	{
		uw_set_helptxt(m_fStruct[frmid].frmhelp);
	}
	UW_Form_help = 1;
}

/**********************************************************************
**    I_FUNCTION :  uw_mfform_okCB(widget, client_data, call_data)
**       Call back routine for the ACCEPT action button.  Checks all
**			fields for proper values and takes down the Motif form.
**		This function only for "INPUT type form"
**    PARAMETERS
**       INPUT  :
**          widget      = Ignored.
**				client_data = Form dialog widget.
**				call_data   = Motif callback structure.
**       OUTPUT :
**				Places user selection in active field.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_okCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int stat;
/*
.....changed by Yurong 9/5/97
*/
	stat = uw_mfaccept_formdata(0);
	if (stat) uw_mfform_close(0);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfaccept_formdata(frmid)
**       Checks all fields for proper values
**    PARAMETERS
**       INPUT  :
**          widget      = Ignored.
**				client_data = Form dialog widget.
**				call_data   = Motif callback structure.
**       OUTPUT :
**				Places user selection in active field.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfaccept_formdata(frmid)
int frmid;
{
	int i,j,stat, inc;
	UD_LIST *list;
	char *text;
	char num[4], num2[8], buf[20];
	if (frmid<0) return UU_TRUE;
	inc = m_fStruct[frmid].n_display_fields;
/*
.....Verify Text field answers
*/
	stat = UU_TRUE;
	ud_rpwrform("START", NULL, NULL);
	for (i=0;i<m_fStruct[frmid].n_input_fields;i++)
	{
/*
.....added for List text field
.....8/15/97
*/
/*
.....only if field displayed and enabled
.....Yurong 4/14/00
*/
		if (((m_fStruct[frmid].input_flds[i].toggle == 1)
				||(m_fStruct[frmid].input_flds[i].toggle == 11)
				|| (m_fStruct[frmid].input_flds[i].toggle == 5))&&
				(m_fStruct[frmid].traverse_mask[i]==1)&&
				(m_fStruct[frmid].ud_display_mask[i+inc] == 1))
		{
/*
.....formEntry[i] could be null
.....Yurong 4/12/00
*/
			if ((m_formEntry[frmid]!=NULL) &&
					(m_formEntry[frmid][i]!=NULL))
			{
				if (text = XmTextGetString(m_formEntry[frmid][i]))
				{
					if ((m_fStruct[frmid].input_flds[i].toggle == 5)
							&& (m_fStruct[frmid].input_flds[i].n_defaults==2))
					{
						list = (UD_LIST *)(m_fData[frmid].ud_data[i].ud_delem.frmint);
						strcpy(list->answer, text);
					}
					else
					{
						stat = uw_mfform_ckdata(frmid, i,text);
					}
/*
...added to record form data
...Yurong
*/
					if(UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
					{
						sprintf(num,"%d", i);
						ud_rpwrform("DATA", num, text);
					}
					XtFree(text);
					if (!stat) break;
				}
			}
			else if ((m_fStruct[frmid].input_flds[i].toggle == 5)
				&& (UD_Rpstate[UD_Rpstate_ptr].flag==RECORD))
			{
				list = (UD_LIST *)(m_fData[frmid].ud_data[i].ud_delem.frmint);
				sprintf(num,"%d", i);
				ud_rpwrform("DATA",num,list->answer);
			}
			if (m_fStruct[frmid].input_flds[i].toggle == 5)
			{
				if ((m_fStruct[frmid].input_flds[i].ud_input==FORM_RECORD)
						&& (UD_Rpstate[UD_Rpstate_ptr].flag==RECORD))
				{
					sprintf(num2,"%d", list->num_item);
					ud_rpwrform("LIST", "START", num2);
					for (j=0; j<list->num_item; j++)
					{
						ud_rpwrform("LIST", num, list->item[j]);
					}
					ud_rpwrform("LIST", "DONE", num);
				}
			}
		}
		if ((m_fStruct[frmid].input_flds[i].toggle == 2)
				&& (m_fStruct[frmid].traverse_mask[i]==1)&&
				(m_fStruct[frmid].ud_display_mask[i+inc] == 1))
		{
/*
......if it is #COLOR# field, set the accept value 16 to -1
*/
			if (m_fStruct[frmid].input_flds[i].ud_input==1)
			{
				if (m_fData[frmid].ud_data[i].ud_delem.frmint[0]==16)
					m_fData[frmid].ud_data[i].ud_delem.frmint[0] = -1;
			}
		}
/*
.....only if field displayed and enabled, then record data
.....Yurong 2/15/02
*/
		else if ((m_fStruct[frmid].traverse_mask[i]==1)&&
				(m_fStruct[frmid].ud_display_mask[i+inc] == 1))
		{
/*
...added to record form data
...Yurong
*/
			if (UD_Rpstate[UD_Rpstate_ptr].flag==RECORD)
			{
				if (m_fStruct[frmid].input_flds[i].toggle == 10)
				{
					list = (UD_LIST*)(m_fData[frmid].ud_data[i].ud_delem.frmint);
					sprintf(num,"%d", i);
					ud_rpwrform("DATA",num,list->answer);
					if (m_fStruct[frmid].input_flds[i].ud_input==FORM_RECORD)
					{
						sprintf(num2,"%d", list->num_item);
						ud_rpwrform("LIST", "START", num2);
						for (j=0; j<list->num_item; j++)
						{
							ud_rpwrform("LIST", num, list->item[j]);
						}
						ud_rpwrform("LIST", "DONE", num);
					}
				}
				else
				{
					sprintf(num,"%d", i);
					if (m_fStruct[frmid].input_flds[i].toggle != 6)
					{
						sprintf(buf,"%d",
							m_fData[frmid].ud_data[i].ud_delem.frmint[0]);
						ud_rpwrform("DATA",num,buf);
					}
					else
						ud_rpwrform("DATA",num,NULL);
				}
			}
		}
	}
	return stat;
}

/**********************************************************************
**    I_FUNCTION :  uw_mfform_cancelCB(widget, client_data, call_data)
**       Call back routine for the CANCEL action button.  Takes down
**			the form and performs a REJECT OP (long jump).
**    PARAMETERS
**       INPUT  :
**          widget      = Ignored.
**				client_data = Form dialog widget.
**				call_data   = Motif callback structure.
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : Performs a long jump.
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_cancelCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int frmid = (int)client_data;
	if (frmid<0) return;
	uw_mfform_close(frmid);
	if (frmid==0)
		ud_jump(-1,UU_FALSE);
}

/**********************************************************************
**    I_FUNCTION :  setState(toggle,value,state)
**       Sets the ON/OFF state for a toggle button.
**    PARAMETERS
**       INPUT  :
**          toggle      = Toggle type.  3 = OFF/ON, 4 = ON/OFF.
**				value       = Toggle value (0/1);
**       OUTPUT :
**				state       = Motif toggle state.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void setState(toggle,value,state)
int toggle,value,*state;
{
	*state = False;
	if ((toggle == 3 && value == 1) || (toggle == 4 && value == 0))
		*state = True;
}

/**********************************************************************
**    I_FUNCTION :  getState(toggle,value,state)
**       Gets the ON/OFF state for a toggle button.
**    PARAMETERS
**       INPUT  :
**          toggle      = Toggle type.  3 = OFF/ON, 4 = ON/OFF.
**				value       = Motif toggle state.
**       OUTPUT :
**				value       = Toggle value (0/1);
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void getState(toggle,value,state)
int toggle,value,*state;
{
	*state = 0;
	if ((toggle == 3 && value == True) || (toggle == 4 && value == False))
		*state = 1;
}
/**********************************************************************
**    I_FUNCTION :  uw_mfreset_list(fno,form_list)
**       Redisplays the form list field fno.
**		This routine is only for "INPUT type form"
**    PARAMETERS
**       INPUT  :
**          fno : field number
**				form_list: UD_LIST structure for redisplay
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfreset_list(fno,form_list)
int fno;
UD_LIST *form_list;
{
	uw_mffrm_setlist(0,fno,form_list);
}

/**********************************************************************
**    I_FUNCTION :  uw_mffrm_setlist(frmid, fno,form_list)
**       Redisplays the form list field fno.
**    PARAMETERS
**       INPUT  :
**				frmid: form we are using to redisplay list
**          fno : field number
**				form_list: UD_LIST structure for redisplay
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mffrm_setlist(frmid, fno,form_list)
int frmid, fno;
UD_LIST *form_list;
{
	if (frmid<0) return;
	if (m_fStruct[frmid].input_flds[fno].toggle == 10)
	{
		uw_mffrm_setlist2(frmid, fno,form_list);
	}
	else
	{
		if (m_fStruct[frmid].input_flds[fno].n_defaults==2)
			uw_mffrm_setlist2(frmid, fno,form_list);
		else
			uw_mffrm_setlist1(frmid, fno,form_list);
	}
}		

/**********************************************************************
**    I_FUNCTION :  uw_mffrm_setlist2(frmid, fno,form_list)
**       Redisplays the form choice_list field fno.
**    PARAMETERS
**       INPUT  :
**				frmid: form we are using to redisplay list
**          fno : field number
**				form_list: UD_LIST structure for redisplay
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mffrm_setlist2(frmid, fno, choice_list)
int frmid, fno;
UD_LIST *choice_list;
{
	XmString string[10000];
	UD_DDATA seldata;
	char lstr[256];
	int i, def;
	UD_FSTAT stat;
	def = 0;
	if (frmid<0) return;
	for (i=0; i<MAX_CHOICE; i++)
	{
		if ((m_formChoice[frmid]!=NULL) &&
				(m_formChoice[frmid][fno][i]!=NULL))
		{
   		XtUnmanageChild(m_formChoice[frmid][fno][i]);
   		XtDestroyWidget(m_formChoice[frmid][fno][i]);
		}
	}
	for (i=0; i<choice_list->num_item; i++)
	{
		m_formChoice[frmid][fno][i] = 
								XtVaCreateManagedWidget(
								choice_list->item[i],
								xmPushButtonGadgetClass, 
								m_formMenu[frmid][fno],
								NULL);
	}
	for (i=0; i<choice_list->num_item; i++)
	{
		string[i] = XmStringCreateSimple(choice_list->item[i]);
		XtVaSetValues(m_formChoice[frmid][fno][i], XmNlabelString, 
					string[i], NULL);
		if (strcmp(choice_list->item[i], choice_list->answer)==0)
			def = i;
	}
	if ((m_fStruct[frmid].input_flds[fno].n_defaults==2)
		&& (m_fStruct[frmid].input_flds[fno].toggle == 5))
	{
		XtVaSetValues(m_list[frmid][fno],XmNmenuHistory,
				m_formChoice[frmid][fno][def], NULL);
	}
	else
	{
		XtVaSetValues(m_formEntry[frmid][fno],XmNmenuHistory, 
			m_formChoice[frmid][fno][def], NULL);
	}
	if (m_fStruct[frmid].input_flds[fno].method_returns & UD_TOGGLE &&
			m_fStruct[frmid].input_flds[fno].method != NULL)
	{
		strcpy(lstr, choice_list->item[def]);
		seldata.frmstr = (char*)&lstr;
		stat = (*m_fStruct[frmid].input_flds[fno].method)
					(&fno, &seldata, UD_TFWD);
		if (stat==UD_BADREQ)
			return;
		active_number = fno;
		if (active_number != -1)
			uw_mfform_redisplay(frmid);
	}
	if (m_fStruct[frmid].input_flds[fno].n_defaults==2)
	{
		XmTextSetString(m_formEntry[frmid][fno], choice_list->answer);
	}
}
/**********************************************************************
**    I_FUNCTION :  uw_mffrm_setlist1(frmid, fno,form_list)
**       Redisplays the form list field fno for listbox.
**    PARAMETERS
**       INPUT  :
**				frmid: form we are using to redisplay list
**          fno : field number
**				form_list: UD_LIST structure for redisplay
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mffrm_setlist1(frmid, fno,form_list)
int frmid, fno;
UD_LIST *form_list;
{
	int item_num, j , k, item_len;
	char sample[100], defans[256];
	XmString defstr;
	XmStringTable str_list;
	Widget listw;
	if (frmid<0) return;
	listw = m_list[frmid][fno];
/*
.....get old list item number and delete all old items
*/
	XtVaGetValues (listw, XmNitemCount, &item_num, NULL);
	if (item_num!=0)
		XmListDeleteAllItems(listw);
/*
.....add new list
*/
	str_list = (XmStringTable)XtMalloc( (form_list->num_item)*
								sizeof(XmString *));
	for (j=0;j<form_list->num_item;j++)
	{
		item_len = strlen(form_list->item[j]);
		strcpy(sample, form_list->item[j]);
/*
.....make a item that length is list length
.....in order to set list length.
*/
		for (k = item_len; k<m_fStruct[frmid].input_flds[fno].ud_flen; k++)
			sample[k] = ' ';
		sample[k] = '\0';
		str_list[j] = XmStringCreateSimple(sample);
	}
	XtVaSetValues (listw, XmNitemCount, form_list->num_item,
						XmNitems, str_list, NULL);
/*
.....set default answer, strtok will change string value, so copy anwser
*/
	if (form_list->answer!=NULL)
		strcpy(defans, form_list->answer);
	if ((form_list->answer!=NULL)&&(strtok(defans, " \0\n")!=NULL))
	{
		item_len = strlen(form_list->answer);
		strcpy(sample, form_list->answer);
		for (k = item_len; k<m_fStruct[frmid].input_flds[fno].ud_flen; k++)
			sample[k] = ' ';
		sample[k] = '\0';
		defstr = XmStringCreateSimple(sample);
		XmListSelectItem(listw, defstr, True);
	}
	else
		XmListSelectPos(listw, 1, True);
/*
.....reset text field
*/
	if ((m_formEntry[frmid]!=NULL) && (m_formEntry[frmid][fno]!=NULL))
	{
		if (form_list->answer!=NULL)
			XmTextSetString(m_formEntry[frmid][fno], form_list->answer);
	}
	XtFree((char *)str_list);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfget_field(fieldno, data)
**       Get the UD_DDATA from give fieldno
**		This routine is only for "INPUT type form"
**    PARAMETERS
**       INPUT  :
**          fieldno : field number
**				data: UD_DDATA structure
**       OUTPUT :
**          data: UD_DDATA structure
**						if text field: string store in frmstr
**						       others: data store in frmint[0]
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....Need malloc space for UD_DDATA data
.....Before get in this function
.....Yurong added 10/2/97
*/
void uw_mfget_field(fieldno, data)
int fieldno;
UD_DDATA data;
{
	uw_mfget_frmfield(0, fieldno, data);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfget_frmfield(frmid, fieldno, data)
**       Get the UD_DDATA from give fieldno nd giving form
**    PARAMETERS
**       INPUT  :
**				frmid: form we are using to get field info
**          fieldno : field number
**				data: UD_DDATA structure
**       OUTPUT :
**          data: UD_DDATA structure
**						if text field: string store in frmstr
**						       others: data store in frmint[0]
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfget_frmfield(frmid, fieldno, data)
int frmid, fieldno;
UD_DDATA data;
{
	char *text;
	UD_LIST *choice_list;
	if (frmid<0) return;
/*
.....for text field and list text field
......value saved in data.frmstr
*/
	if ((m_fStruct[frmid].input_flds[fieldno].toggle == 1) ||
			(m_fStruct[frmid].input_flds[fieldno].toggle == 3) ||
			(m_fStruct[frmid].input_flds[fieldno].toggle == 5) ||
			(m_fStruct[frmid].input_flds[fieldno].toggle == 11) ||
			(m_fStruct[frmid].input_flds[fieldno].toggle == 16))
	{
		if ((m_formEntry[frmid]!=NULL) &&
				(m_formEntry[frmid][fieldno]!=NULL))
		{
			text = XmTextGetString(m_formEntry[frmid][fieldno]);
			if(text!=NULL)
			{
				strcpy(data.frmstr, text);
			}
		}
	}
	else if (m_fStruct[frmid].input_flds[fieldno].toggle == 10)
	{
		choice_list =
			 (UD_LIST*)(m_fData[frmid].ud_data[fieldno].ud_delem.frmint);
		strcpy(data.frmstr, choice_list->answer);
	}
/*
......For choice buttom and toggle field
......value saved in data.frmint[0]
*/
	else
		data.frmint[0] = m_fData[frmid].ud_data[fieldno].ud_delem.frmint[0];
}

/**********************************************************************
**    E_FUNCTION :  ButtonEventHandler(widget,client_data,event)
**       Event handler for mouse button
**    PARAMETERS
**       INPUT  :
**          widget      = field widget.
**				client_data = Contains field structure information which
**									we will use.
**				event   = Motif event structure.
**          
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void ButtonEventHandler(widget,
		client_data,
		event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	int stat, frmid;
	XButtonReleasedEvent *bevent;
	if (event->type!=ButtonRelease)
		return;
	frmid = (int) client_data;
	if (frmid<0) return;
	bevent = (XButtonReleasedEvent *)event;
	switch(bevent->button)
	{
/*
.....accept form
*/
		case Button2:
			stat = uw_mfaccept_formdata(frmid);
			if (stat) uw_mfform_close(frmid);
			break;
/*
.....cancel the form
*/
		case Button3:
			uw_mfform_close(frmid);
			if (frmid==0)
				ud_jump(-1,UU_FALSE);
			break;
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_mfform_textretCB(widget, client_data, call_data)
**       Call back routine for text fields.
**    PARAMETERS
**       INPUT  :
**          widget      = Text field widget.
**				client_data = Contains field structure information which
**									we will use.
**				callData   = Motif callback structure.
**
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_textretCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int i, frmid;
	Pstruct *ps=(Pstruct *)client_data;

	i = ps->fld;
	frmid = ps->frmid;
	if (frmid<0) return;
	uw_mftxt_next(i, frmid, 1);
}

void uw_mftxt_next(i, frmid, flag)
int i, frmid, flag;
{
	char *text;
	char ldat[80];
	char inptext[80];
	int j, len, stat;
	Widget temp, widget;
	if (frmid<0) return;
	widget = m_formEntry[frmid][i];
	stat = UU_TRUE;
	if (flag)
	{
		text = XmTextGetString(widget);
		if(text!=NULL)
		{
			strcpy(inptext, text);
			XtFree(text);
		}
		else
			inptext[0] = '\0';
		len = strlen(inptext);
		stat = uw_mfform_ckdata(frmid, i,inptext);
		if (stat==UU_FALSE)
			return;
		ud_formstr_format(m_fStruct[frmid].input_flds[i].ud_datatyp,
					m_fData[frmid].ud_data[i].ud_delem,
					m_fStruct[frmid].input_flds[i].ud_fprec,
					m_fStruct[frmid].input_flds[i].ud_flen,ldat);
		XmTextSetString(m_formEntry[frmid][i], ldat);
/*
.....Call user defined method
.....if present
*/
		if (m_fStruct[frmid].input_flds[i].method_returns & UD_TOGGLE &&
				m_fStruct[frmid].input_flds[i].method != NULL)
		{
			(*m_fStruct[frmid].input_flds[i].method)
					(&i, &(m_fData[frmid].ud_data[i].ud_delem), UD_TFWD);
/*
.....allow cancel the redisplay
.....from method
.....in most text value change callback function
.....we don't want to redisplay it because in edit one text field
.....may call this function many times and we may only want update
.....form once
.....Yurong 4/24/00
*/
			if (i != -1)
			{
				uw_mfform_redisplay(frmid);
			}
		}
	}
#if UU_COMP != UU_HPUX
	if (stat==UU_TRUE)
	{
/*
.....tab to next enabled field
*/
		XmProcessTraversal(widget, XmTRAVERSE_NEXT_TAB_GROUP);
/*
.....We want it call uw_mfform_focusCB function
.....but it doesn't because for the choice button, it focus
.....not in FormEntry but formChoice. We don't setup focus callback
.....for formChoice, only FormEntry, so do action in uw_mfform_focusCB
.....here. Get the next formEntry first
.....Yurong 4/4/00
*/
		temp = XmGetFocusWidget(widget);
		widget = XmGetTabGroup(temp);
		for (j = formField; j<m_fStruct[frmid].n_input_fields; j++)
		{
			if (widget==m_formEntry[frmid][j])
				break;
		}
		if (j==m_fStruct[frmid].n_input_fields)
			j = 0;
		if (m_fStruct[frmid].input_flds[j].toggle != 1)
		{
			formInpWin = NULL;
		}
		else
		{
			formInpWin = m_formEntry[frmid][j];
			formActFrm = frmid;
		}
	}
#endif
}

/**********************************************************************
**    E_FUNCTION :  drawCB(widget, client_data, call_data)
**       Callback for draw a picture area
**    PARAMETERS
**       INPUT  :
**          widget      = field widget.
**				client_data = Contains field structure information which
**									we will use.
**				callData   = Motif callback structure.
**          
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void drawCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data, call_data;
{
	GC gc_ptr;
	Display *display;
	Dimension width, height;
	int end, intpt, x,y,n,red, green, blue,cx,cy;
	int x0, y0, x1,y1, x2, y2, x3, y3, x4, y4, r, dir;
	int iang1,iang2;
	char *textstr;
	double ang0, ang1, ang2, temp;
	int down;
	XTextItem textitem;
	double rate_x, rate_y;
	unsigned long text_fg, line_fg, bg;
	XColor current_color;
	Colormap cmap;
	int color_set;
	Font tfont;
	int drawnum, stat, frmid;
int *test_array;
	int current_x = 0;
	int current_y = 0;
	int current_style = LineSolid;
	int current_width = 1;
	Pstruct *ps=(Pstruct *)client_data;
	drawnum = ps->fld;
	frmid = ps->frmid;

/*
.....if GC have not create it, return
*/
/*	if (m_Draw_gc[current_frmid][drawnum]==NULL) */
	if (m_Draw_gc[frmid][drawnum]==NULL)
		return;

	display = XtDisplay(m_draw_area[frmid][drawnum]);

	rate_x = (double)m_Draw_Width[frmid][drawnum]/
										(double)m_WC_Width[frmid][drawnum];
	rate_y = (double)m_Draw_Height[frmid][drawnum]/
										(double)m_WC_Height[frmid][drawnum];

	intpt = 0;
	end = 0;
/*	gc_ptr = m_Draw_gc[current_frmid][drawnum]; */
	gc_ptr = m_Draw_gc[frmid][drawnum];
	XtVaGetValues(widget, XmNwidth, &width,
				XmNheight, &height, NULL);
	XtVaGetValues(widget, XmNcolormap, &cmap,
							XmNbackground, &bg, NULL);
	current_color.red = 169*256;
	current_color.green = 169*256;
	current_color.blue = 169*256;
	stat = XAllocColor(display, cmap, &current_color);
	if (stat == 0) line_fg = bg;
	else line_fg = current_color.pixel;
	XSetForeground(display, gc_ptr, line_fg);
	XFillRectangle(display, XtWindow(widget), gc_ptr, 0,0,width,height);
	current_color.red = 0*256;
	current_color.green = 0*256;
	current_color.blue = 0*256;
	stat = XAllocColor(display, cmap, &current_color);
	text_fg = line_fg = current_color.pixel;
	XSetForeground(display, gc_ptr, line_fg);
	XSetLineAttributes(display, gc_ptr, current_width, current_style,
					CapNotLast, JoinRound);
	m_draw_array[frmid][drawnum] =
						 (int*)(m_fData[frmid].ud_data[drawnum].ud_delem.frmint);
	test_array = (int*)(m_fData[frmid].ud_data[drawnum].ud_delem.frmint);
/*
......draw a border
*/
	x = m_draw_array[frmid][drawnum][intpt+1];
	y = m_draw_array[frmid][drawnum][intpt+2];

	XDrawLine(display, XtWindow(widget), gc_ptr, 1,
						1, m_Draw_Width[frmid][drawnum]-1, 1);
	XDrawLine(display, XtWindow(widget), gc_ptr, m_Draw_Width[frmid][drawnum]-1,
						1, m_Draw_Width[frmid][drawnum]-1, 
						m_Draw_Height[frmid][drawnum]-1);
	XDrawLine(display, XtWindow(widget), gc_ptr, m_Draw_Width[frmid][drawnum]-1,
						m_Draw_Height[frmid][drawnum]-1, 1, 
						m_Draw_Height[frmid][drawnum]-1);
	XDrawLine(display, XtWindow(widget), gc_ptr, 1, 
						m_Draw_Height[frmid][drawnum]-1,
						1, 1);
	color_set = 0;
draw:
	switch (m_draw_array[frmid][drawnum][intpt])
	{
		case 1:
/*
.....MoveTo
*/
			x = m_draw_array[frmid][drawnum][intpt+1];
			y = m_draw_array[frmid][drawnum][intpt+2];
			current_x = x;
			current_y = y;
			intpt = intpt + 3;
			break;
		case 2:
/*
.....LineTo
*/
/*
......check the color first, it may changed because of text color change
*/
			if (color_set==1)
/*
......color changed because of text color change
*/
			{
				XSetForeground(display, gc_ptr, line_fg);
				color_set = 2;
			}
			x = m_draw_array[frmid][drawnum][intpt+1] * rate_x;
			y = m_draw_array[frmid][drawnum][intpt+2] * rate_x;
			cx = current_x * rate_x; cy = current_y * rate_y;
			XDrawLine(display, XtWindow(widget), gc_ptr, cx,cy,x,y);
			current_x = m_draw_array[frmid][drawnum][intpt+1];
			current_y = m_draw_array[frmid][drawnum][intpt+2];
			intpt = intpt + 3;
			break;
		case 3:
/*
.....arrow
*/
			if (color_set==1)
/*
......color changed because of text color change
*/
			{
				XSetForeground(display, gc_ptr, line_fg);
				color_set = 2;
			}
			x1 = current_x;
			y1 = current_y;
			x2 = m_draw_array[frmid][drawnum][intpt+1];
			y2 = m_draw_array[frmid][drawnum][intpt+2];
			if (y2!=y1)
			{
				down = abs(y2-y1)/(y2-y1);
				temp = (double)(x2-x1)/(double)(y2-y1);
				ang0 = atan(temp);
			}
			else if (x2==x1)
			{
				intpt = intpt + 3;
				break;
			}
			else
			{
				down = abs(x2-x1)/(x2-x1);
				ang0 = M_PI/2;
			}
			ang1 = ang0 + Arrow_angle;
			ang2 = Arrow_angle - ang0;
			y3 = y2 - down*Arrow_len*cos(ang1);
			x3 = x2 - down*Arrow_len*sin(ang1);
			y4 = y2 - down*Arrow_len*cos(ang2);
			x4 = x2 + down*Arrow_len*sin(ang2);
			cx = x1*rate_x; cy = y1*rate_y; x = x2*rate_x; y = y2*rate_y;
			XDrawLine(display, XtWindow(widget), gc_ptr, cx,cy,x,y);
			cx = x3*rate_x; cy = y3*rate_y;
			XDrawLine(display, XtWindow(widget), gc_ptr, x,y,cx,cy);
			cx = x4*rate_x; cy = y4*rate_y;
			XDrawLine(display, XtWindow(widget), gc_ptr, x,y,cx,cy);
			current_x = x2;
			current_y = y2;
			intpt = intpt + 3;
			break;
		case 4:
/*
......Draw Circle
*/
			if (color_set==1)
/*
......color changed because of text color change
*/
			{
				XSetForeground(display, gc_ptr, line_fg);
				color_set = 2;
			}
			x0 = m_draw_array[frmid][drawnum][intpt+1];
			y0 = m_draw_array[frmid][drawnum][intpt+2];
			r = m_draw_array[frmid][drawnum][intpt+3];
			x1 = x0 - r;
			y1 = y0 - r;
			x2 = x0 + r;
			y2 = y0 + r;
			dir = m_draw_array[frmid][drawnum][intpt+4];
			x3 = current_x;
			y3 = current_y;
			x4 = m_draw_array[frmid][drawnum][intpt+5];
			y4 = m_draw_array[frmid][drawnum][intpt+6];
			current_x = x4;
			current_y = y4;

/*
.....ang1: current angle from 3 o'clock
.....ang0: end angle from 3 o'clock
*/
			if (y0!=y3)
			{
				if (x3!=x0)
				{
					temp = (double)(y0-y3)/(double)(x3-x0);
					ang1 = atan(temp);
					if ((x3-x0<0)&&(y0-y3<0))
						ang1 = ang1 + M_PI;
					else if ((x3-x0<0)&&(y0-y3>0))
						ang1 = ang1 + M_PI;
				}
				else if (y3>y0)
					ang1 = M_PI*3/2;
				else
					ang1 = M_PI/2;
			}
			else if (x3==x0)
			{
				intpt = intpt + 7;
				break;
			}
			else if (x3>x0)
				ang1 = 0;
			else
				ang1 = M_PI;

			if (y0!=y4)
			{
				if (x4!=x0)
				{
					temp = (double)(y0-y4)/(double)(x4-x0);
					ang0 = atan(temp);
					if ((x4-x0<0)&&(y0-y4<0))
						ang0 = ang0 + M_PI;
					else if ((x4-x0<0)&&(y0-y4>0))
						ang0 = ang0 + M_PI;
				}
				else if (y4>y0)
					ang0 = M_PI*3/2;
				else
					ang0 = M_PI/2;
			}
			else if (x4==x0)
			{
				intpt = intpt + 7;
				break;
			}
			else if (x4>x0)
				ang0 = 0;
			else
				ang0 = M_PI;


			if (dir==1)
/*
......COUNTERCLOCKWISE
*/
			{
				ang2 = ang0 - ang1;
				if (ang2<0)
					ang2 = 2*M_PI + ang2;
			}
			else
/*
.....CLOCKWISE
*/
			{
				ang2 = ang0 - ang1;
				if (ang2>0)
					ang2 = -2*M_PI + ang2;
			}
			if (ang1<0)
				ang1 = 2*M_PI + ang1;
			ang1 = ang1 * 64 * 360/(2*M_PI);
			ang2 = ang2 * 64 * 360/(2*M_PI);
			if (ang2==ang1)
			{
				ang2 = ang2 + 64 * 360;
			}
			x1 = x1*rate_x; y1 = y1*rate_y; cx = 2*r*rate_x; cy = 2*r*rate_y;
			iang1 = ang1 ; iang2 = ang2;
			XDrawArc(display, XtWindow(widget), gc_ptr, x1, y1, cx, cy,
				iang1, iang2);
			intpt = intpt + 7;
			break;
		case 6:
/*
.....text color
*/
			red = m_draw_array[frmid][drawnum][intpt+1];
			green = m_draw_array[frmid][drawnum][intpt+2];
			blue = m_draw_array[frmid][drawnum][intpt+3];
			current_color.red = 255*red;
			current_color.green = 255*green;
			current_color.blue = 255*blue;
			stat = XAllocColor(display, cmap, &current_color);
			text_fg = current_color.pixel;
			XSetForeground(display, gc_ptr, text_fg);
			color_set = 1;
			intpt = intpt + 4;
			break;
		case 10:
/*
.....line color
*/
			red = m_draw_array[frmid][drawnum][intpt+1];
			green = m_draw_array[frmid][drawnum][intpt+2];
			blue = m_draw_array[frmid][drawnum][intpt+3];
			current_color.red = 255*red;
			current_color.green = 255*green;
			current_color.blue = 255*blue;
			stat = XAllocColor(display, cmap, &current_color);
			line_fg = current_color.pixel;
			XSetForeground(display, gc_ptr, line_fg);
			color_set = 2;
			intpt = intpt + 4;
			break;
		case 5:
/*
.....text
*/
			if (color_set==2)
/*
......color changed because of text color change
*/
			{
				XSetForeground(display, gc_ptr, text_fg);
				color_set = 1;
			}
			n = m_draw_array[frmid][drawnum][intpt+1];
			textstr = (char*)m_draw_array[frmid][drawnum][intpt+2];
			textitem.chars = textstr;
			textitem.nchars = n;
			textitem.delta = 0;
			tfont = XLoadFont(display,
				"-adobe-helvetica-bold-r-normal-*-12-100-*-*-*-*-iso8859-1");
			textitem.font = tfont;
			cx = current_x*rate_x; cy = (current_y+Text_offy)*rate_y;
			XDrawText(display, XtWindow(widget), gc_ptr, cx, cy, &textitem, 1);
			intpt = intpt + 3;
			break;
		case 8:
/*
......line style
*/
			n = 0;
			if (m_draw_array[frmid][drawnum][intpt+1]==0)
				current_style = LineSolid;
			else if (m_draw_array[frmid][drawnum][intpt+1]==1)
			{
				current_style = LineDoubleDash;
			}
			else if (m_draw_array[frmid][drawnum][intpt+1]==2)
			{
				current_style = LineOnOffDash;
			}
			else if (m_draw_array[frmid][drawnum][intpt+1]==3)
			{
				current_style = LineDoubleDash;
			}
			XSetLineAttributes(display, gc_ptr, current_width, current_style,
					CapNotLast, JoinRound);
			intpt = intpt + 2;
			break;

		case 9:
/*
......Line Width
*/
			current_width = m_draw_array[frmid][drawnum][intpt+1];
			XSetLineAttributes(display, gc_ptr, current_width, current_style,
					CapNotLast, JoinRound);
			intpt = intpt + 2;
			break;
		case 99:
			end = 1;
			break;
	}
	if (end!=1)
		goto draw;
	return;
}

/**********************************************************************
**    E_FUNCTION :  uw_mfform_vis()
**       Invisibles the active form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_invis()
{
	if (UW_display_frm[0]==NULL)
		return;
	if (formActFrm==0)
	{
		formActFrm = -1;
	}
	XtUnmanageChild(UW_display_frm[0]);
	formActive=False;
}

/**********************************************************************
**    E_FUNCTION :  uw_mfform_vis()
**       Redisplays the active form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_vis()
{
	if (UW_display_frm[0]==NULL)
		return;
	XtManageChild(UW_display_frm[0]);
	formActive=True;
}

/**********************************************************************
**    E_FUNCTION :  uw_mfclose_dispfrm(frmid)
**       Close form
**    PARAMETERS
**       INPUT  :
**          frmid: form's ID which need to be close
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void uw_mfclose_dispfrm(frmid)
int frmid;
{
	if (frmid<0) return;
	if (UW_display_frm[frmid]!=NULL)
		uw_mfform_close(frmid);
}

/**********************************************************************
**    E_FUNCTION :  uw_mfupdate_form(frmid)
**       Update the form display
**    PARAMETERS
**       INPUT  :
**          frmid: form's ID which need to be updated
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void uw_mfupdate_form(frmid)
int frmid;
{
	if (frmid<0) return;
	if (UW_display_frm[frmid]!=NULL)
		uw_mfform_redisplay(frmid);
	uw_mf_xflush_win();
}

/**********************************************************************
**    E_FUNCTION :  uw_ifpos_inarea(x0, y0, x,y,cx,cy)
**       Check if a point is inside an area
**    PARAMETERS
**       INPUT  :
**          x0, y0: position need to be checked
**				x,y,cx,cy: area's left point and size
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
uw_ifpos_inarea(x0, y0, x,y,cx,cy)
int x0, y0, x,y,cx,cy;
{
	if ((x0>x)&&(x0<x+cx)&&(y0>y)&&(y0<y+cy))
		return 1;
	return 0;
}
/**********************************************************************
**    E_FUNCTION :  uw_mfdspfrm_invis(frmid)
**       Invisibles the display form.
**    PARAMETERS
**       INPUT  :
**          frmid: form ID
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void uw_mfdspfrm_invis(frmid)
int frmid;
{
	if (frmid<0) return;
	if (UW_display_frm[frmid]==NULL)
		return;
	if (formActFrm==frmid)
	{
		formActFrm = -1;
	}
	XtUnmanageChild(UW_display_frm[frmid]);
	if (frmid==0)
		formActive=False;
}

/**********************************************************************
**    E_FUNCTION :  uw_mfdspfrm_vis(frmid)
**       Redisplays the display form. 
**    PARAMETERS
**       INPUT  :
**          frmid: form ID
**       OUTPUT :
**				None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void uw_mfdspfrm_vis(frmid)
int frmid;
{
	if (frmid<0) return;
	if (UW_display_frm[frmid]==NULL)
		return;
	XtManageChild(UW_display_frm[frmid]);
	if (frmid==0)
		formActive=True;
}
/***********************************************************************
c
c   FUNCTION:  drawCB2(widget, client_data, call_data)
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
void drawCB2(widget, client_data, call_data)
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
	int drawnum, frmid;
	Pstruct *ps=(Pstruct *)client_data;
	drawnum = ps->fld;
	frmid = ps->frmid;

	if (m_process_gc[frmid][drawnum]==NULL)
		return;
	display = XtDisplay(m_process_bar[frmid][drawnum]);

	gc_ptr = m_process_gc[frmid][drawnum];
	XtVaGetValues(widget, XmNwidth, &width,
				XmNheight, &height, NULL);

	XtVaGetValues(widget, XmNcolormap, &cmap, NULL);
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
	XFillRectangle(display, XtWindow(widget), gc_ptr, 0,0,width,height);
	x = 0; y = 0;
	cx = width; cy = height;
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
	XFlush(display);

	current_color.red = 0;
	current_color.green = 0;
	current_color.blue = 255*255;
	XAllocColor(display, cmap, &current_color);
	XSetForeground(display, 
						gc_ptr,
						current_color.pixel);

	box = (m_percent[frmid][drawnum]/5)+0.5;
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
/*
......update the label
*/
	if (m_fStruct[frmid].input_flds[drawnum].ud_datatyp==UD_DASINT)
	{
		sprintf(perc_str, "%d%%", m_percent[frmid][drawnum]);
		lstr = XmStringCreateSimple(perc_str);
		XtVaSetValues(m_process_label[frmid][drawnum],
								XmNlabelString, lstr, NULL);
		XmStringFree(lstr);
	}
}
/**********************************************************************
**    I_FUNCTION :  uw_mfdisfrm_set_label(frmid, fno,label)
**       Redisplays the form list field fno.
**    PARAMETERS
**       INPUT  :
**				frmid: form we are using to redisplay list
**          fno : field number
**				label: label to be set
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfdisfrm_set_label(frmid, fno,label)
int frmid, fno;
char *label;
{
	XmString lstr;
	if (frmid<0) return;
				
	lstr = XmStringCreateSimple(label);
	XtVaSetValues(m_FormDisp[frmid][fno], 
								XmNlabelString, lstr, NULL);
	XmStringFree(lstr);
}
/***********************************************************************
c
c   FUNCTION: uw_dispfrm_wait(int fid)
c
c       Wait a display form to be finished, this will not disactive other 
c			form/window event unless other form/window
c		 is disactive before this form display.
c
c   INPUT:  frmid: form id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void uw_dispfrm_wait(frmid)
int frmid;
{
	XEvent x_event;
	int stat;

	if (frmid<0) return;
	if (UW_display_frm[frmid]==NULL)
		return;

	while (UW_display_frm[frmid]!=NULL)
	{
		XtAppNextEvent(uw_mf.application,&x_event);
/*
......if it is a special key, we need handle it here as accelerator key
*/
		stat = uw_mfaccel_key(&x_event);
		if (stat==0)
			XtDispatchEvent(&x_event);
	}
}
/***********************************************************************
c
c   FUNCTION: uw_mfset_frmpocket(frmid, flag)
c
c       Set pocket flag of the form.
c
c   INPUT:  frmid: form id
c			flag: pocket flag to be set
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void uw_mfset_frmpocket(frmid, flag)
int frmid, flag;
{
	UW_Form_pocket = flag;
}
void uw_mfsave_actform_id()
{
   save_formActFrm = formActFrm;
	save_formField = formField;
}

void uw_mfreset_actform_id()
{
    formActFrm = save_formActFrm;
	formField = save_formField;
	if ((formActFrm>=0) && (formField>=0))
	{
		if ((m_formEntry[formActFrm]!=NULL)&&
					(m_formEntry[formActFrm][formField]!=NULL))
		XmProcessTraversal(m_formEntry[formActFrm][formField],XmTRAVERSE_CURRENT);
	}
	save_formActFrm = -1;
	save_formField = -1;
}

/***********************************************************************
c
c   FUNCTION: uw_formupd_input(text)
c
c      Update the active text field with the new input text
c     insert the text at the insert cursor position
c
c   INPUT:  text: input to be update
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int uw_formupd_input(text)
char *text;
{
	int typ, start, end, len;
	char *string;
	Boolean stat;
	XmTextPosition left, right, ipos;

	uw_mf_xflush_win();

	if ((formActFrm<0) || (formField==-1))
		return -1;
	if (UW_display_frm[formActFrm]==NULL)
		return -1;
	typ = m_fStruct[formActFrm].input_flds[formField].ud_datatyp;
/*
.....check the field type, if it is not single value, then
.....we append the text, if it is single data/value type, replace it
*/
	string = XmTextGetString(m_formEntry[formActFrm][formField]);
	start = 0;
	if (string!=NULL)
	{
		len  = strlen (string);
		XtFree(string);
	}
	else
		len = 0;
	ipos = XmTextGetInsertionPosition(m_formEntry[formActFrm][formField]);
	start = end = ipos;
	if (!( (typ==UD_DASVAL) || (typ==UD_DASDISTANCE)
		|| (typ==UD_DASUNITLESS) || (typ==UD_DASANGLE) || (typ==UD_DASINT)
		|| ((typ>=UD_DASSCALAR) && (typ<=UD_SCAUNITLESS)) ))
	{
		stat = XmTextGetSelectionPosition(m_formEntry[formActFrm][formField],
					&left, &right);
		if (stat)
		{
			start = left;
			end = right;
		}
	}
	else
	{
		start = 0;
		end = len;
	}
	XmTextReplace(m_formEntry[formActFrm][formField], start, end, text);
	return 0;
}
void uw_mfget_focusfrm(frmid, fldno)
int *frmid, *fldno;
{
	*frmid = formActFrm;
	*fldno = formField;
}
/**********************************************************************
**    E_FUNCTION :  uw_mfdisfrm_set_butlabel(int frmid, int fieldno, char
*label)
**       set the button new label
**    PARAMETERS
**       INPUT  :
**          frmid:   Form ID.
**       fieldno: field number
**       label: label to be set
**       OUTPUT :
**          none
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
void uw_mfdisfrm_set_butlabel(frmid, fieldno, label)
int frmid, fieldno;
char *label;
{
	XmString lstr;
	if (frmid<0) return;
				
	lstr = XmStringCreateSimple(label);
	XtVaSetValues(m_formEntry[frmid][fieldno], 
								XmNlabelString, lstr, NULL);
	XmStringFree(lstr);
}
/**********************************************************************
**    I_FUNCTION :  execute_frmcmd(frmid, fldno)
**       Call function executed by push the particular button of the form
**    PARAMETERS
**       INPUT  :
**				frmid: from ID
**				fldno: field number
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void execute_frmcmd(frmid, fldno)
int frmid, fldno;
{
	int old_field;
/*
......check previous input field to see if data format is right
*/
	formActFrm = frmid;
/*
.....Call user defined method
.....if present
*/
	if (m_fStruct[frmid].input_flds[fldno].method_returns & UD_TOGGLE &&
			m_fStruct[frmid].input_flds[fldno].method != NULL)
	{
		old_field = fldno;
		(*m_fStruct[frmid].input_flds[fldno].method)
			(&fldno, &(m_fData[frmid].ud_data[fldno].ud_delem),UD_TFWD);
/*
.....remember the active field number
.....used by button.
.....Yurong 8/29/97
*/
		active_number = fldno;
		fldno = old_field;
/*
.....allow cancel the redisplay
.....from method
.....Yurong 8/29/97
*/
		if (active_number != -1)
			uw_mfform_redisplay(frmid);
	}
}
/**********************************************************************
**    I_FUNCTION :  uw_if_formopen()
**       Check if a form is opened
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_if_formopen()
{
	int i;
/*
......search for available form
*/
   for (i=0; i<50; i++)
  	{
     	if (UW_display_frm[i]!=NULL)
     	   return 1;
  	}
	return 0;
}
/**********************************************************************
**    I_FUNCTION :  Sconv_list1(table_list, flist)
**       Read a table list structure and put into a simple UD_LIST
**			structure because we don't support table-list on UNIX
**    PARAMETERS
**       INPUT  :
**				table_list: table list structure
**       OUTPUT :
**				flist: UD_LIST structure 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Sconv_list1(table_list, flist)
UD_TLIST *table_list;
UD_LIST *flist;
{
	char liststr[500], items[22];
	int i,j,k, len;
/*
......we will ignore the col_label field and convert
......the UD_ITEMDATA into UD_LIST data
*/
	flist->num_item = table_list->num_item;
	flist->answer = (char *)uu_malloc(300*sizeof(char));
	if (flist->num_item<=0)
	{
		flist->num_item = 1;
		flist->item = (char**)uu_malloc ((sizeof (char*))*flist->num_item);
		flist->item[0] = (char *)uu_malloc(10*sizeof(char));		
		flist->item[0][0] = '\0';
		flist->answer[0] = '\0';
		return;
	}
	flist->item = (char**)uu_malloc ((sizeof (char*))*flist->num_item);
/*
......the item data label is limited to 20chars, if longer,
......will cut to 19+'~'.
*/
	for (k=0; k<flist->num_item; k++)
	{
		liststr[0] = '\0';
		for (i=0; i<table_list->num_col;i++)
		{
			len = strlen(table_list->data[k].data_items[i]);
			if (len<=20)
			{
				strcpy(items, (char*)((table_list->data[k]).data_items[i]));
			}
			else
			{
				strncpy(items, (char*)((table_list->data[k]).data_items[i]), 19);
				items[20] = '~';
				len = 20;
			}		
/*
......fill space till 21 char (one space in between)
*/
			for (j=len; j<21; j++)
			{
				items[j] = ' ';
			}
			items[21] = '\0';
			strcat (liststr, items);
		}
		len = strlen(liststr)+1;
		flist->item[k] = (char *)uu_malloc(len*sizeof(char));
		strcpy(flist->item[k], liststr);
		if ((table_list->answer>=0)&&(table_list->answer==k))
			strcpy(flist->answer, liststr);
	}
}
void uw_mffrm_settlist(frmid, fieldno, form_tlist)
int frmid, fieldno;
UD_TLIST *form_tlist;
{
	if (frmid<0) return;
	if (m_fStruct[frmid].input_flds[fieldno].toggle != 17)
		return;
	
	m_fData[frmid].ud_data[fieldno].ud_delem.frmint = (int*)form_tlist;
	ud_free_flist(&Sflist[fieldno]);
	Sconv_list1 (form_tlist, &Sflist[fieldno]);
	uw_mffrm_setlist1(frmid, fieldno, &Sflist[fieldno]);
}
/*********************************************************************
**    E_FUNCTION : uw_mfform_resizeCB(widget,client_data,event)
**
**    DESCRIPTION:
**        Callback for resize/move form
**
**    PARAMETERS  
**       INPUT  :
**          widget      = Ignored.
**          client_data = form ID.
**          call_data   = Motif callback structure containing event.
**       OUTPUT : 
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfform_resizeCB(widget, client_data, event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	Position x, y;
	Dimension width, height;
	int status, frmid, x1, y1, cx, cy;
	int wrect[4], att_win, ref, pos[4];
	XConfigureEvent *cevent = (XConfigureEvent *)event;
	if(cevent->type !=ConfigureNotify)
		return;
	frmid = (int)client_data;
/*
...get size and position
*/
	XtVaGetValues(widget, XmNx, &x, XmNy, &y, NULL);
	XtVaGetValues(widget, XmNwidth, &width, XmNheight, &height, NULL);

	x1 = x - XBorder[1];
	y1 = y - YBorder[1];
	cx = width;
	cy = height;
	if (UW_first_resize[frmid]==0)
	{
		att_win = m_fStruct[frmid].att_win;
		ref = m_fStruct[frmid].ref;
		status = ud_read_posform(m_fStruct[frmid].frmname, &x1, &y1, 
					&cx, &cy, &att_win, &ref);
   	if (status==0)
   	{
			uw_mfadj_frmpos2(pos, x1, y1, cx, cy, att_win, ref);
			pos[0] += XBorder[1];
			pos[1] += YBorder[1];
			x = pos[0];
			y = pos[1];
			width = cx;
			height = cy;
			XtVaSetValues(widget, XmNx, x, XmNy, y, 
								XmNwidth, width, XmNheight, height, NULL);
		}
		UW_first_resize[frmid] = 1;
		return;
   }
	if (m_fStruct[frmid].att_win==1)
/*
......SCREEN
*/
	{
		wrect[0] = 0;
		wrect[1] = 0;
		wrect[2] = DisplayWidth(uw_xw.disp, uw_xw.screen_no);
		wrect[3] = DisplayHeight(uw_xw.disp, uw_xw.screen_no);
	}
	else
	{
/*
......WINDOW
*/
		XtVaGetValues(uw_mf.graphic, XmNx, &x, XmNy, &y, NULL);
		XtVaGetValues(uw_mf.graphic, XmNwidth, &width, XmNheight, &height, NULL);
		wrect[0] = x - XBorder[0];
		wrect[1] = y - YBorder[0];	
		wrect[2] = width;
		wrect[3] = height;
	}
	ud_save_frmpos(m_fStruct[frmid].frmname, x1, y1, cx, cy,&wrect[0],
				m_fStruct[frmid].att_win, m_fStruct[frmid].ref,
				XBorder[1], YBorder[1]);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfadj_frmpos(x,y,cx,cy, frmid)
**       Adjust form position according to the form structure position
definition.
**    PARAMETERS
**       INPUT  :
**          x,y,cx,cy : form old postion and size
**				frmid: form ID
**       OUTPUT :
**          x,y,cx,cy : Window new postion and size
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_mfadj_frmpos(x,y,cx,cy, frmid)
int *x,*y,*cx,*cy,frmid;
{
	int pos[4];
	int fpos[2];
	fpos[0] = m_fStruct[frmid].ud_frmdf.ud_fwin.ll.x;
	fpos[1] = m_fStruct[frmid].ud_frmdf.ud_fwin.ll.y;
	pos[0] = *x;
	pos[1] = *y;
	uw_mfadj_frmpos2(pos, fpos[0], fpos[1], *cx, *cy, 
							m_fStruct[frmid].att_win, m_fStruct[frmid].ref);
	*x = pos[0];
	*y = pos[1];
}
/**********************************************************************
**    I_FUNCTION :  uw_mfadj_frmpos2(pos, x,y,fcx,fcy, att_win, ref_flag)
**       Adjust form position according to the form structure position
definition.
**    PARAMETERS  
**       INPUT  :
**          pos[4] : Window old postion
**       x,y: relative position
**       fcx, fcy: form size
**       att_win: relate attach window
**       ref_flag: reference flag
**       OUTPUT : 
**          pos[4] : Window new postion
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfadj_frmpos2(pos, x, y, fcx, fcy, att_win, ref_flag)
int *pos, x,y, fcx, fcy, att_win, ref_flag;
{
	int cx, cy;
	int ref[2], fpos[2];
	Dimension width, height;
	Position wx, wy;
	fpos[0] = x;
	fpos[1] = y;
		
	if (att_win==1)
/*
......attached to SCREEN
*/
	{
/*
......Get screen size
*/
		cx = DisplayWidth(uw_xw.disp, uw_xw.screen_no);
		cy = DisplayHeight(uw_xw.disp, uw_xw.screen_no);
		if (ref_flag==0)
/*
......LOWER_LEFT
*/
		{
/*
......screen's lower-left
*/
			ref[0]= 0;
			ref[1] = cy;
/*
......form's lower left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = fpos[1] - fcy - YBorder[1];
		}
		else if (ref_flag==1)
/*
......LOWER_CENTER
*/
		{
/*
......screen's lower-center
*/
			ref[0]= 0.5*cx;
			ref[1] = cy;
/*
......form's lower center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - 0.5*fcx;
			pos[1] = fpos[1] - fcy - YBorder[1];
		}
		else if (ref_flag==2)
/*
......LOWER_RIGHT
*/
		{
/*
......screen's lower-right
*/
			ref[0]= cx;
			ref[1] = cy;
/*
......form's lower right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx - XBorder[1];
			pos[1] = fpos[1] - fcy - YBorder[1];


			x = ref[0] + fpos[0] - fcx - XBorder[1];
			y = ref[1] - fpos[1] - fcy - YBorder[1];
		}
		else if (ref_flag==3)
/*
......CENTER_LEFT
*/
		{
/*
......screen's center-left
*/
			ref[0]= 0;
			ref[1] = 0.5*cy;
/*
......form's center left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = fpos[1] - 0.5*fcy;
		}
		else if (ref_flag==4)
/*
......CENTER_CENTER
*/
		{
/*
......screen's center-center
*/
			ref[0]= 0.5*cx;
			ref[1] = 0.5*cy;
/*
......form's center center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - 0.5*fcx;
			pos[1] = fpos[1] - 0.5*fcy;
		}
		else if (ref_flag==5)
/*
......CENTER_RIGHT
*/
		{
/*
......screen's center-right
*/
			ref[0]= cx;
			ref[1] = 0.5*cy;
/*
......form's center right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx - XBorder[1];
			pos[1] = fpos[1] - 0.5*fcy;
		}
		else if (ref_flag==6)
/*
......UPPER_LEFT
*/
		{
/*
......screen's upper-left
*/
			ref[0]= 0;
			ref[1] = 0;
/*
......form's upper left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = fpos[1];
		}
		else if (ref_flag==7)
/*
......UPPER_CENTER
*/
		{
/*
......screen's upper-center
*/
			ref[0]= 0.5*cx;
			ref[1] = 0;
/*
......form's upper center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - 0.5*fcx;
			pos[1] = fpos[1];
		}
		else if (ref_flag==8)
/*
......UPPER_RIGHT
*/
		{
/*
......screen's upper-right
*/
			ref[0]= cx;
			ref[1] = 0;
/*
......form's upper right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx - XBorder[1];
			pos[1] = fpos[1];
		}
	}
	else
	{
/*
......Get NCL window size
*/
		XtVaGetValues(uw_mf.graphic, XmNx, &wx, XmNy, &wy, NULL);
		XtVaGetValues(uw_mf.graphic, XmNwidth, &width, XmNheight, &height, NULL);
		cx = width;
		cy = height;
		wx -= XBorder[0];
		wy -= YBorder[0];
		if (ref_flag==0)
/*
......LOWER_LEFT
*/
		{
/*
......window's lower-left
*/
			ref[0]= wx;
			ref[1] = wy + cy;
/*
......form's lower left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = fpos[1] - fcy - YBorder[1];
		}
		else if (ref_flag==1)
/*
......LOWER_CENTER
*/
		{
/*
......window's lower-center
*/
			ref[0]= wx + 0.5*cx ;
			ref[1] = wy + cy;
/*
......form's lower center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - 0.5*fcx;
			pos[1] = fpos[1] - fcy - YBorder[1];
		}
		else if (ref_flag==2)
/*
......LOWER_RIGHT
*/
		{
/*
......window's lower-right
*/
			ref[0]= wx + cx;
			ref[1] = wy + cy;
/*
......form's lower right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx - XBorder[1];
			pos[1] = fpos[1] - fcy - YBorder[1];
		}
		else if (ref_flag==3)
/*
......CENTER_LEFT
*/
		{
/*
......window's center-left
*/
			ref[0]= wx;
			ref[1] = wy + 0.5*cy;
/*
......form's center left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = fpos[1] - 0.5*fcy;
		}
		else if (ref_flag==4)
/*
......CENTER_CENTER
*/
		{
/*
......window's center-center
*/
			ref[0]= wx + 0.5*cx;
			ref[1] = wy + 0.5*cy;
/*
......form's center center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - 0.5*fcx;
			pos[1] = fpos[1] - 0.5*fcy;
		}
		else if (ref_flag==5)
/*
......CENTER_RIGHT
*/
		{
/*
......window's center-right
*/
			ref[0]= wx + cx;
			ref[1] = wy + 0.5*cy;
/*
......form's center right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx - XBorder[1];
			pos[1] = fpos[1] - 0.5*fcy;
		}
		else if (ref_flag==6)
/*
......UPPER_LEFT
*/
		{
/*
......window's upper-left
*/
			ref[0]= wx;
			ref[1] = wy;
/*
......form's upper left
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0];
			pos[1] = fpos[1];
		}
		else if (ref_flag==7)
/*
......UPPER_CENTER
*/
		{
/*
......window's upper-center
*/
			ref[0]= wx + 0.5*cx;
			ref[1] = wy;
/*
......form's upper center
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - 0.5*fcx;
			pos[1] = fpos[1];
		}
		else if (ref_flag==8)
/*
......UPPER_RIGHT
*/
		{
/*
......window's upper-right
*/
			ref[0]= wx + cx;
			ref[1] = wy;
/*
......form's upper right
*/
			fpos[0] = ref[0] + fpos[0];
			fpos[1] = ref[1] - fpos[1];
/*
......form's position (top-left always)
*/
			pos[0] = fpos[0] - fcx - XBorder[1];
			pos[1] = fpos[1];
		}
	}
}
/**********************************************************************
**    I_FUNCTION : uw_mffrm_set_focus(frmid, fieldno)
**       set the focus of a form field
**    PARAMETERS  
**       INPUT  : frmid, fieldno
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mffrm_set_focus(frmid, fieldno)
{
	XmProcessTraversal(m_formEntry[frmid][fieldno],XmTRAVERSE_CURRENT);
    formActFrm = frmid;
	formField = fieldno;
}

#endif

