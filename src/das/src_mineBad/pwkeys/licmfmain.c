#if !defined WNT
/*********************************************************************
**    NAME         :  licmfmain.c 
**       CONTAINS:
**             main
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        licmfmain.c , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:11
c
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
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/MwmUtil.h>
#include <Xm/Protocols.h>
#include <Xm/SashP.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <X11/cursorfont.h>
#include <Xm/Separator.h>
#include "pwenv.h"
#include "licfc.h"


typedef struct
{
   int sel;
   char *file;
} FSELECT;

XtAppContext LIC_App;
Widget LIC_Parent, lic_edit1, lic_edit2, lic_edit3, lic_edit4, lic_edit5,
			lic_edit6, lic_edit7, lic_edit8, lic_edit9, lic_check1, lic_check2,
			lic_check3, lic_check4, lic_check5, lic_check6, lic_check7,
			lic_check8, lic_check9;
Widget fdialog;
Cursor normal_cursor, wait_cursor;
static int mfanswer;
static int yes_or_no;
static XmFontList frm_fontlist;
static XFontStruct *fontstrct;


add_trail_spaces(string, len)
char *string;
int len;
{
	int slen;
	slen = strlen(string);
	while (slen<len)
		string[slen++] = ' ';
}

mfdisfld(kfl, buf, len)
int *kfl, *len;
char *buf;
{
	buf[*len] = '\0';
	switch (*kfl)
	{
		case 1:
			XmTextSetString(lic_edit1, buf);
			break;
		case 2:
			XmTextSetString(lic_edit2, buf);
			break;
		case 3:
			XmTextSetString(lic_edit3, buf);
			break;
		case 4:
			XmTextSetString(lic_edit4, buf);
			break;
		case 5:
			XmTextSetString(lic_edit5, buf);
			break;
		case 6:
			XmTextSetString(lic_edit6, buf);
			break;
		case 7:
			XmTextSetString(lic_edit7, buf);
			break;
		case 8:
			XmTextSetString(lic_edit8, buf);
			break;
		case 9:
			XmTextSetString(lic_edit9, buf);
			break;
	}
}
			
/*********************************************************************
**    I_FUNCTION     :  Clear_Record()
**       Clear the License Input fields
**    PARAMETERS
**       INPUT  :
**          None
**       OUTPUT :
**         None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Clear_Record()
{
	XmTextSetString(lic_edit1, "");
	XmTextSetString(lic_edit2, "");
	XmTextSetString(lic_edit3, "");
	XmTextSetString(lic_edit4, "");
	XmTextSetString(lic_edit5, "");
	XmTextSetString(lic_edit6, "");
	XmTextSetString(lic_edit7, "");
	XmTextSetString(lic_edit8, "");
	XmTextSetString(lic_edit9, "");
}

/*********************************************************************
**    I_FUNCTION     :  Get_Record()
**       Get the License Input and save into global variables
**    PARAMETERS
**       INPUT  :
**          flag    - =1, output error messages.
**       OUTPUT :
**         None
**    RETURNS      : 1 on error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int Get_Record(flag)
int flag;
{
	int krec;
	char *text;
	char buf[133];
	krec = 1;
	text = XmTextGetString(lic_edit1);
	if(text[0]!='\0')
		strcpy(buf, text);
	else if (flag==1)
	{
		lic_mfmsg_box(LIC_Parent, "Error!", "You must fill out all the fields");
		return 1;
	}
	else
		buf[0] = '\0';
	XtFree(text);
/*
.....we need add spaces to allow fortran recognize
*/
   add_trail_spaces(buf, 132);
	putbuf(&krec, buf, &flag);
	krec++;

	text = XmTextGetString(lic_edit2);
	if(text[0]!='\0')
		strcpy(buf, text);
	else if (flag==1)
	{
		lic_mfmsg_box(LIC_Parent, "Error!", "You must fill out all the fields");
		return 1;
	}
	else
		buf[0] = '\0';
	XtFree(text);
/*
.....we need add spaces to allow fortran recognize
*/
   add_trail_spaces(buf, 132);
	putbuf(&krec, buf, &flag);
	krec++;

	text = XmTextGetString(lic_edit3);
	if(text[0]!='\0')
		strcpy(buf, text);
	else if (flag==1)
	{
		lic_mfmsg_box(LIC_Parent, "Error!", "You must fill out all the fields");
		return 1;
	}
	else
		buf[0] = '\0';
	XtFree(text);
/*
.....we need add spaces to allow fortran recognize
*/
   add_trail_spaces(buf, 132);
	putbuf(&krec, buf, &flag);
	krec++;

	text = XmTextGetString(lic_edit4);
	if(text[0]!='\0')
		strcpy(buf, text);
/*
.....option could be empty
*/
	else
		buf[0] = '\0';
	XtFree(text);
/*
.....we need add spaces to allow fortran recognize
*/
   add_trail_spaces(buf, 132);
	putbuf(&krec, buf, &flag);
	krec++;

	text = XmTextGetString(lic_edit5);
	if(text[0]!='\0')
		strcpy(buf, text);
	else if (flag==1)
	{
		lic_mfmsg_box(LIC_Parent, "Error!", "You must fill out all the fields");
		return 1;
	}
	else
		buf[0] = '\0';
	XtFree(text);
/*
.....we need add spaces to allow fortran recognize
*/
   add_trail_spaces(buf, 132);
	putbuf(&krec, buf, &flag);
	krec++;

	text = XmTextGetString(lic_edit6);
	if(text[0]!='\0')
		strcpy(buf, text);
	else if (flag==1)
	{
		lic_mfmsg_box(LIC_Parent, "Error!", "You must fill out all the fields");
		return 1;
	}
	else
		buf[0] = '\0';
	XtFree(text);
/*
.....we need add spaces to allow fortran recognize
*/
   add_trail_spaces(buf, 132);
	putbuf(&krec, buf, &flag);
	krec++;

	text = XmTextGetString(lic_edit7);
	if(text[0]!='\0')
		strcpy(buf, text);
	else if (flag==1)
	{
		lic_mfmsg_box(LIC_Parent, "Error!", "You must fill out all the fields");
		return 1;
	}
	else
		buf[0] = '\0';
	XtFree(text);
/*
.....we need add spaces to allow fortran recognize
*/
   add_trail_spaces(buf, 132);
	putbuf(&krec, buf, &flag);
	krec++;

	text = XmTextGetString(lic_edit8);
	if(text[0]!='\0')
		strcpy(buf, text);
	else if (flag==1)
	{
		lic_mfmsg_box(LIC_Parent, "Error!", "You must fill out all the fields");
		return 1;
	}
	else
		buf[0] = '\0';
	XtFree(text);
/*
.....we need add spaces to allow fortran recognize
*/
   add_trail_spaces(buf, 132);
	putbuf(&krec, buf, &flag);
	krec++;

	text = XmTextGetString(lic_edit9);
	if(text[0]!='\0')
		strcpy(buf, text);
	else if (flag==1)
	{
		lic_mfmsg_box(LIC_Parent, "Error!", "You must fill out all the fields");
		return 1;
	}
	else
		buf[0] = '\0';
	XtFree(text);
/*
.....we need add spaces to allow fortran recognize
*/
   add_trail_spaces(buf, 132);
	putbuf(&krec, buf, &flag);
	return 0;
}

Widget CreatePushbutton(parent, name, callback, client_data)
Widget parent;
char *name;
XtCallbackProc callback;
XtPointer client_data;
{
	Widget push;
	Arg args[20];
	Cardinal n;
	n = 0;
	push = XmCreatePushButton(parent, name, args, n);

	XtAddCallback(push,
						XmNactivateCallback,
						callback,
						client_data);
	XtManageChild(push);
	return push;
}

Widget CreatePulldownMenu(parent, name)
Widget parent;
char *name;
{
	Widget menu, cascade;
	Arg args[20];
	Cardinal n;
	n = 0;
	menu = XmCreatePulldownMenu(parent, name, args, n);
	n=0;
	XtSetArg(args[n], XmNsubMenuId, menu); n++;
	cascade = (Widget)XmCreateCascadeButton(parent, name, args, n);
	XtManageChild(cascade);
	return menu;
}
	
/*********************************************************************
**    I_FUNCTION     :  ExitCB(widget, client_data, call_data)
**       Callback for menu item or buuton "Exit" 
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ExitCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int fil = 1;
	XtUnmanageChild((Widget)client_data); 
	XtDestroyWidget((Widget)client_data);
	XmUpdateDisplay((Widget)client_data);
	clsfil (&fil);
	exit(0);
}

/*********************************************************************
**    I_FUNCTION     :  AddCB
**       Callback for button "Add"
**       
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void AddCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int ierr;
	char msg[80];
	if (Get_Record(1) == 0) addrec(msg, &ierr);
}

/*********************************************************************
**    I_FUNCTION     :  ClearCB
**       Callback for button "Clear"
**			This routine clears out all fields in the nccs_license
**			dialog and resets the search pointer to the beginning of the file.
**       
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ClearCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	Clear_Record();
	clrrec();
}

/*********************************************************************
**    I_FUNCTION     :  DeleteCB
**       Callback for button "Delete"
**       
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void DeleteCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int ierr;
	char msg[80];
	if (Get_Record(1) == 0) delrec(msg, &ierr);
}

/*********************************************************************
**    I_FUNCTION     :  SearchCB
**       Callback for button "Search"
**       
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void SearchCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
/*
	Get_Record(2);
*/
	search();
}

/*********************************************************************
**    I_FUNCTION     :  TsearchCB
**       Callback for button "Toggle Search"
**       
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void TsearchCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int parm;
	Widget temp = XmGetFocusWidget(LIC_Parent);
	if (temp == lic_edit1)
	{
		parm = XmToggleButtonGetState(lic_check1);
		if (parm)
			XmToggleButtonSetState(lic_check1, 0, True);
		else
			XmToggleButtonSetState(lic_check1, 1, True);
		return;
	}
	else if (temp == lic_edit2)
	{
		parm = XmToggleButtonGetState(lic_check2);
		if (parm)
			XmToggleButtonSetState(lic_check2, 0, True);
		else
			XmToggleButtonSetState(lic_check2, 1, True);
		return;
	}
	else if (temp == lic_edit3)
	{
		parm = XmToggleButtonGetState(lic_check3);
		if (parm)
			XmToggleButtonSetState(lic_check3, 0, True);
		else
			XmToggleButtonSetState(lic_check3, 1, True);
		return;
	}
	else if (temp == lic_edit4)
	{
		parm = XmToggleButtonGetState(lic_check4);
		if (parm)
			XmToggleButtonSetState(lic_check4, 0, True);
		else
			XmToggleButtonSetState(lic_check4, 1, True);
		return;
	}
	else if (temp == lic_edit5)
	{
		parm = XmToggleButtonGetState(lic_check5);
		if (parm)
			XmToggleButtonSetState(lic_check5, 0, True);
		else
			XmToggleButtonSetState(lic_check5, 1, True);
		return;
	}
	else if (temp == lic_edit6)
	{
		parm = XmToggleButtonGetState(lic_check6);
		if (parm)
			XmToggleButtonSetState(lic_check6, 0, True);
		else
			XmToggleButtonSetState(lic_check6, 1, True);
		return;
	}
	else if  (temp == lic_edit7)
	{
		parm = XmToggleButtonGetState(lic_check7);
		if (parm)
			XmToggleButtonSetState(lic_check7, 0, True);
		else
			XmToggleButtonSetState(lic_check7, 1, True);
		return;
	}
	else if (temp == lic_edit8)
	{
		parm = XmToggleButtonGetState(lic_check8);
		if (parm)
			XmToggleButtonSetState(lic_check8, 0, True);
		else
			XmToggleButtonSetState(lic_check8, 1, True);
		return;
	}
	else if (temp == lic_edit9)
	{
		parm = XmToggleButtonGetState(lic_check9);
		if (parm)
			XmToggleButtonSetState(lic_check9, 0, True);
		else
			XmToggleButtonSetState(lic_check9, 1, True);
		return;
	}
}

/*********************************************************************
**    I_FUNCTION     :  SsearchCB
**       Callback for button "Show Search"
**       
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void SsearchCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	char buf[10][132];
	int parm[10];
	char label[10000];

	label[0] = '\0';
	getsparm(buf, parm);
	
	if (parm[0]==1)
	{
		XmTextSetString(lic_edit1, buf[0]);
	}
	else
		XmTextSetString(lic_edit1, "");
	if (parm[1]==1)
	{
		XmTextSetString(lic_edit2, buf[1]);
	}
	else
		XmTextSetString(lic_edit2, "");
	if (parm[2]==1)
	{
		XmTextSetString(lic_edit3, buf[2]);
	}
	else
		XmTextSetString(lic_edit3, "");
	if (parm[3]==1)
	{
		XmTextSetString(lic_edit4, buf[3]);
	}
	else
		XmTextSetString(lic_edit4, "");
	if (parm[4]==1)
	{
		XmTextSetString(lic_edit5, buf[4]);
	}
	else
		XmTextSetString(lic_edit5, "");
	if (parm[5]==1)
	{
		XmTextSetString(lic_edit6, buf[5]);
	}
	else
		XmTextSetString(lic_edit6, "");
	if (parm[6]==1)
	{
		XmTextSetString(lic_edit7, buf[6]);
	}
	else
		XmTextSetString(lic_edit7, "");
	if (parm[7]==1)
	{
		XmTextSetString(lic_edit8, buf[8]);
	}
	else
		XmTextSetString(lic_edit8, "");
	if (parm[8]==1)
	{
		XmTextSetString(lic_edit9, buf[8]);
	}
	else
		XmTextSetString(lic_edit9, "");
}

/*********************************************************************
**    I_FUNCTION     :  checkCB
**       Callback for check box
**       
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void checkCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int parm;
	char buf[132], *text;
	int check = (int) client_data;
	parm = XmToggleButtonGetState(widget);
	buf[0] = '\0';
	if (check==1)
		text = XmTextGetString(lic_edit1);
	else if (check==2)
		text = XmTextGetString(lic_edit2);
	else if (check==3)
		text = XmTextGetString(lic_edit3);
	else if (check==4)
		text = XmTextGetString(lic_edit4);
	else if (check==5)
		text = XmTextGetString(lic_edit5);
	else if (check==6)
		text = XmTextGetString(lic_edit6);
	else if (check==7)
		text = XmTextGetString(lic_edit7);
	else if (check==8)
		text = XmTextGetString(lic_edit8);
	else if (check==9)
		text = XmTextGetString(lic_edit9);
	if(text[0]!='\0')
		strcpy(buf, text);
	
	savsparm(&parm, buf, &check);
}
/*********************************************************************
**    I_FUNCTION     :  LoadFileCB
**       Callback for menuitem "Load File..."
**       
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void LoadFileCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	char filename[UX_MAX_PATH];
	int err, len;
	filename[0] = '\0';
	lic_mffilename(widget, "Open License File", "*.dat", filename, &len);
	if (len > 0) mfbatchlic(filename, &len, &err);
}

/*********************************************************************
**    I_FUNCTION     :  lic_cb1
**       Callback for File menu
**       
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void lic_cb1(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	char filename[UX_MAX_PATH];
	int err, len, fil;
	int pick_no = (int) client_data;
	filename[0] = '\0';
	if (pick_no == 0)
	{
		lic_mffilename(LIC_Parent, "Open License File", "*.lic", filename, &len);
		if (len > 0) mfbatchlic(filename, &len, &err);
		return;
	}
	if (pick_no == 1)
	{
		fil = 1;
		clsfil (&fil);
		exit(0);
	}
}

/*********************************************************************
**    I_FUNCTION     :  lic_cb2
**       Callback for File menu
**       
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void lic_cb2(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int ierr;
	char msg[80];
	int pick_no = (int) client_data;
	if (pick_no==0)
	{
		if (Get_Record(1) == 0) addrec(msg, &ierr);
		return;
	}
	if (pick_no==1)
	{
		if (Get_Record(1) == 0) delrec(msg, &ierr);
		return;
	}
	if (pick_no==2)
	{
		Clear_Record();
		clrrec();
		return;
	}
	if (pick_no==3)
	{
		search();
		return;
	}
	if (pick_no==4)
	{
		TsearchCB(NULL, NULL, NULL);	
		return;
	}
	if (pick_no==5)
	{
		SsearchCB(NULL, NULL, NULL);	
		return;
	}
}
/*********************************************************************
**    I_FUNCTION     :  main
**       Main routine nccs_license of MOTIF version.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
main(argc, argv)
int argc;
char **argv;
{
	Arg args[20];
	XmString lstr;
	XEvent event;
	int rows1;
	char *p;
	char pbuf[80];
	Widget mainwindow, label, menubar, menu1, menu2, Main_form;
	Widget sep1, sep2, add, delete, clear, search;
	int n,i,j,status,flen;
	char msg[300];
	XmString str1, str2, str3, str4, str5, str6, file, edit, 
				key1, key2, key3, key4, key5, key6;
	int err;
	int fil = 1;
   setmotif();
	if (argc>1)
	{
/*
.....init data
*/
      setbatch();
		init();
   	autini();
   	mfopndat (msg,&err);
		if (err != 0) 
		{
			printf(msg);
			return;
		}
      flen = strlen(argv[1]);
		mfbatchlic(argv[1], &flen, &err);
		if (err==0)
		{
			clsfil (&fil);
			return;
		}
	}
/*
.....Initialize MOTIF
*/
	i = 0;
	n = 0;
	XtSetArg(args[n],XmNmappedWhenManaged,False); n++;
	XtSetArg(args[n],XmNwidth, 500); n++;
	XtSetArg(args[n],XmNheight, 500); n++;
	XtSetArg(args[n],XmNtitle, "NCCS Licensing Facility"); n++;
	LIC_Parent = XtAppInitialize (&LIC_App, "nccs_license", NULL,0,
							&i,NULL,NULL,args,n);
	n = 0;
	mainwindow = (Widget)XmCreateMainWindow(LIC_Parent,"lic_main", args, n);

	fontstrct = XLoadQueryFont(XtDisplay(mainwindow),
							"-*-courier-medium-r-normal--20-*-*-*-*-*-iso8859-1");
	frm_fontlist =  XmFontListCreate (fontstrct, XmSTRING_DEFAULT_CHARSET);
	XtVaSetValues(mainwindow, XmNdefaultFontList, frm_fontlist,
						XmNtextFontList, frm_fontlist,
						XmNlabelFontList, frm_fontlist,
						XmNbuttonFontList, frm_fontlist,
						NULL);
/*
......Create menubar
*/
	file = XmStringCreateSimple("File");
	edit = XmStringCreateSimple("Edit");
	menubar = XmVaCreateSimpleMenuBar(mainwindow, "menubar",
					XmVaCASCADEBUTTON, file, 'i',
					XmVaCASCADEBUTTON, edit, 'E',
					XmNlabelFontList, frm_fontlist,
					XmNbuttonFontList, frm_fontlist,
					NULL);
	XmStringFree(file);
	XmStringFree(edit);
	str1 = XmStringCreateSimple("Load License...");
	str2 = XmStringCreateSimple("Exit");
	key1 = XmStringCreateSimple("Alt   o");
	key2 = XmStringCreateSimple("Alt   x");
	menu1 = XmVaCreateSimplePulldownMenu(menubar, "file_menu", 0, lic_cb1,
							XmVaPUSHBUTTON, str1, 'o', "Alt<Key>o", key1,
							XmVaPUSHBUTTON, str2, 'x', "Alt<Key>x", key2,
							NULL);
	XmStringFree(str1);
	XmStringFree(str2);
	XmStringFree(key1);
	XmStringFree(key2);

	str1 = XmStringCreateSimple("Add");
	str2 = XmStringCreateSimple("Delete");
	str3 = XmStringCreateSimple("Clear Form");
	str4 = XmStringCreateSimple("Search");
	str5 = XmStringCreateSimple("Toggle Search");
	str6 = XmStringCreateSimple("Show Search");
	key1 = XmStringCreateSimple("Alt   a");
	key2 = XmStringCreateSimple("Alt   d");
	key3 = XmStringCreateSimple("Alt   u");
	key4 = XmStringCreateSimple("Alt   f");
	key5 = XmStringCreateSimple("Alt   t");
	key6 = XmStringCreateSimple("Alt   s");
	menu2 = XmVaCreateSimplePulldownMenu(menubar, "edit_menu", 1, lic_cb2,
							XmVaPUSHBUTTON, str1, 'A', "Alt<Key>a", key1,
							XmVaPUSHBUTTON, str2, 'D', "Alt<Key>d", key2,
							XmVaPUSHBUTTON, str3, 'u', "Alt<Key>u", key3,
							XmVaPUSHBUTTON, str4, 'f', "Alt<Key>f", key4,
							XmVaPUSHBUTTON, str5, 'T', "Alt<Key>t", key5,
							XmVaPUSHBUTTON, str6, 'S', "Alt<Key>s", key6,
							NULL);
	XmStringFree(str1);
	XmStringFree(str2);
	XmStringFree(str3);
	XmStringFree(str4);
	XmStringFree(str5);
	XmStringFree(str6);
	XmStringFree(key1);
	XmStringFree(key2);
	XmStringFree(key3);
	XmStringFree(key4);
	XmStringFree(key5);
	XmStringFree(key6);
	XtManageChild(menubar);
/*
.....Create a Form widget
*/
	n = 0;
	Main_form = (Widget) XtCreateWidget("Lic_Form", xmFormWidgetClass, 
							mainwindow, args,n);
	if (Main_form == NULL) return;
	XtVaSetValues(Main_form, XmNdefaultFontList, frm_fontlist,
						XmNtextFontList, frm_fontlist,
						XmNlabelFontList, frm_fontlist,
						XmNbuttonFontList, frm_fontlist,
						NULL);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 6); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2); n++;
	label = XtCreateManagedWidget("NCCS Licensing Facility", 
					xmLabelWidgetClass, Main_form, args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 6); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	sep1 = XtCreateManagedWidget("sep1", xmSeparatorWidgetClass,
				Main_form, args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 8); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 17); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2); n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
/*
	label = XtCreateManagedWidget("Company:", xmLabelWidgetClass, Main_form, 
					args,n);
*/
	lic_check1 = XtCreateManagedWidget("Company:    ", xmToggleButtonWidgetClass,
				Main_form, args,n);
	XtAddCallback(lic_check1, XmNvalueChangedCallback,
		   (XtCallbackProc) checkCB, (XtPointer)1);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 9); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 17); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, lic_check1); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	XtSetArg(args[n], XmNtraversalOn, True); n++;
	lic_edit1 = XtCreateManagedWidget("Lic_edit1", 
						xmTextWidgetClass, Main_form,
						args,n);
	XmTextSetString(lic_edit1, "");
	XmProcessTraversal(lic_edit1, XmTRAVERSE_CURRENT);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 17); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 26); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 2);  n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
/*
	label = XtCreateManagedWidget("Hardware:", xmLabelWidgetClass, Main_form, 
					args,n);
*/
	lic_check2 = XtCreateManagedWidget("Hardware:   ", xmToggleButtonWidgetClass,
				Main_form, args,n);
	XtAddCallback(lic_check2, XmNvalueChangedCallback,
		   (XtCallbackProc) checkCB, (XtPointer)2);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 18); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 26); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, lic_check2); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	lic_edit2 = XtCreateManagedWidget("Lic_edit2", 
						xmTextWidgetClass, Main_form,
						args,n);
	XmTextSetString(lic_edit2, "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 26); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 35); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 2);  n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
/*
	label = XtCreateManagedWidget("Software:", xmLabelWidgetClass, Main_form, 
					args,n);
*/
	lic_check3 = XtCreateManagedWidget("Software:   ", xmToggleButtonWidgetClass,
				Main_form, args,n);
	XtAddCallback(lic_check3, XmNvalueChangedCallback,
		   (XtCallbackProc) checkCB, (XtPointer)3);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 27); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 35); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, lic_check3); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	lic_edit3 = XtCreateManagedWidget("Lic_edit3", 
						xmTextWidgetClass, Main_form,
						args,n);
	XmTextSetString(lic_edit3, "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 35); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 44); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 2);  n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
/*
	label = XtCreateManagedWidget("Options:", xmLabelWidgetClass, Main_form, 
					args,n);
*/
	lic_check4 = XtCreateManagedWidget("Options:    ", xmToggleButtonWidgetClass,
				Main_form, args,n);
	XtAddCallback(lic_check4, XmNvalueChangedCallback,
		   (XtCallbackProc) checkCB, (XtPointer)4);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 36); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 44); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, lic_check4); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	lic_edit4 = XtCreateManagedWidget("Lic_edit4", 
						xmTextWidgetClass, Main_form,
						args,n);
	XmTextSetString(lic_edit4, "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 44); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 53); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 2);  n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
/*
	label = XtCreateManagedWidget("Number of Users:", xmLabelWidgetClass, 
					Main_form, 
					args,n);
*/
	lic_check5 = XtCreateManagedWidget("Users:      ", xmToggleButtonWidgetClass,
				Main_form, args,n);
	XtAddCallback(lic_check5, XmNvalueChangedCallback,
		   (XtCallbackProc) checkCB, (XtPointer)5);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 45); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 53); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, lic_check5); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	lic_edit5 = XtCreateManagedWidget("Lic_edit5", 
						xmTextWidgetClass, Main_form,
						args,n);
	XmTextSetString(lic_edit5, "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 53); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 62); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 2);  n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
/*
	label = XtCreateManagedWidget("License Termination:", xmLabelWidgetClass, 
					Main_form, 
					args,n);
*/
	lic_check6 = XtCreateManagedWidget("Termination:", xmToggleButtonWidgetClass,
				Main_form, args,n);
	XtAddCallback(lic_check6, XmNvalueChangedCallback,
		   (XtCallbackProc) checkCB, (XtPointer)6);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 54); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 62); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, lic_check6 ); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	lic_edit6 = XtCreateManagedWidget("Lic_edit6", 
						xmTextWidgetClass, Main_form,
						args,n);
	XmTextSetString(lic_edit6, "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 62); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 71); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 2);  n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
/*
	label = XtCreateManagedWidget("Version:", xmLabelWidgetClass, Main_form, 
					args,n);
*/
	lic_check7 = XtCreateManagedWidget("Version:    ", xmToggleButtonWidgetClass,
				Main_form, args,n);
	XtAddCallback(lic_check7, XmNvalueChangedCallback,
		   (XtCallbackProc) checkCB, (XtPointer)7);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 63); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 71); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, lic_check7); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	lic_edit7 = XtCreateManagedWidget("Lic_edit7", 
						xmTextWidgetClass, Main_form,
						args,n);
	XmTextSetString(lic_edit7, "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 72); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 80); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 2);  n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
/*
	label = XtCreateManagedWidget("System:", xmLabelWidgetClass, Main_form, 
					args,n);
*/
	lic_check8 = XtCreateManagedWidget("System:     ", xmToggleButtonWidgetClass,
				Main_form, args,n);
	XtAddCallback(lic_check8, XmNvalueChangedCallback,
		   (XtCallbackProc) checkCB, (XtPointer)8);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 71); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 80); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, lic_check8); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	lic_edit8 = XtCreateManagedWidget("Lic_edit8", 
						xmTextWidgetClass, Main_form,
						args,n);
	XmTextSetString(lic_edit8, "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 80); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 89); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 2);  n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
/*
	label = XtCreateManagedWidget("Password:", xmLabelWidgetClass, Main_form, 
					args,n);
*/
	lic_check9 = XtCreateManagedWidget("Password:   ", xmToggleButtonWidgetClass,
				Main_form, args,n);
	XtAddCallback(lic_check9, XmNvalueChangedCallback,
		   (XtCallbackProc) checkCB, (XtPointer)9);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 81); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 89); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, lic_check9); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 20); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
/* XmNhighlightOnEnter XmNtraversalOn */
	lic_edit9 = XtCreateManagedWidget("Lic_edit9", 
						xmTextWidgetClass, Main_form,
						args,n);
	XmTextSetString(lic_edit9, "");

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 91); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	sep2 = XtCreateManagedWidget("sep2", xmSeparatorWidgetClass,
				Main_form, args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 92); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 98); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 4);  n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNrightPosition, 24); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
	add = XtCreateManagedWidget("Add", xmPushButtonWidgetClass, Main_form,
			args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 92); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 98); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 28);  n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNrightPosition, 48); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
	delete = XtCreateManagedWidget("Delete", xmPushButtonWidgetClass, 
			Main_form,
			args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 92); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 98); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 52);  n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNrightPosition, 72); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
	clear = XtCreateManagedWidget("Clear", xmPushButtonWidgetClass, Main_form,
			args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 92); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 98); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 76);  n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNrightPosition, 96); n++;
	XtSetArg(args[n], XmNtraversalOn, False); n++;
	search = XtCreateManagedWidget("Search", xmPushButtonWidgetClass, 
			Main_form,
			args,n);

	XtAddCallback(add, 
			XmNactivateCallback,
		   (XtCallbackProc) AddCB,
		   NULL);

	XtAddCallback(delete, 
			XmNactivateCallback,
		   (XtCallbackProc) DeleteCB,
		   NULL);

	XtAddCallback(clear, 
			XmNactivateCallback,
		   (XtCallbackProc) ClearCB,
		   NULL);

	XtAddCallback(search,
			XmNactivateCallback,
			(XtCallbackProc) SearchCB,
			mainwindow);
		
	XtManageChild(Main_form);
	XmMainWindowSetAreas(mainwindow, menubar, NULL, NULL, NULL, Main_form);

	XtManageChild(mainwindow);

	XtRealizeWidget(LIC_Parent);
	XtMapWidget(LIC_Parent);
/*
.....init data
*/
	if (argc==1)
	{
		init();
   	autini();
   	mfopndat (msg,&err);
		if (err != 0) 
		{
			lic_mfmsg_box(LIC_Parent, "Error!", msg);
			return;
		}
	}
	XtAppMainLoop(LIC_App);
	for(;;)
	{
		XDefineCursor(XtDisplay(LIC_Parent), XtWindow(LIC_Parent),
							normal_cursor);
		XFlush(XtDisplay(LIC_Parent));
		XtAppNextEvent(LIC_App,&event);
		XDefineCursor(XtDisplay(LIC_Parent), XtWindow(LIC_Parent),
							wait_cursor);
		XFlush(XtDisplay(LIC_Parent));
		XtDispatchEvent(&event);
	}
	return 0;

}

/**********************************************************************
**    I_FUNCTION :  lic_mfmsg_box(parent, title, msg)
**       Creates and displays the message dialog.
**    PARAMETERS   
**       INPUT  : 
**          msg     = text to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Disables user interaction until the error
**				         message is acknowledged.
**    WARNINGS     : none
*********************************************************************/
lic_mfmsg_box(parent, title, msg)
Widget parent;
char *msg, *title;
{
	int n;
	Arg args[20];
	Widget dlg,but;
	XmString lstr;
	Position x,y,h,w;
/*
.....Bring up Message Dialog
*/
	n = 0;
#ifdef IBM
   lstr = XmStringCreateSimple(msg);
#else
	lstr = XmStringCreateLtoR(msg, XmFONTLIST_DEFAULT_TAG);
#endif
	XtSetArg(args[n],XmNmessageString,lstr); n++;
/*
........Disable user interaction until
........Error message is acknowleged
*/
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
	XtSetArg(args[n],XmNtitle,title); n++;
	if(parent==NULL)
		dlg = (Widget) XmCreateErrorDialog(LIC_Parent, "MSG",args,n);
	else
		dlg = (Widget) XmCreateErrorDialog(parent,"MSG",args,n);
	XtUnmanageChild((Widget)XmMessageBoxGetChild(dlg,XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild((Widget)XmMessageBoxGetChild(dlg,XmDIALOG_HELP_BUTTON));
	but = (Widget) XmMessageBoxGetChild(dlg,XmDIALOG_OK_BUTTON);
	XmStringFree(lstr);
	XtAddCallback(dlg,XmNokCallback,(XtPointer)XtDestroyWidget,(XtPointer) NULL);
	XtManageChild(dlg);
/*
....Warp cursor to OK Button
*/
	n = 0;
	XtSetArg(args[n],XmNx,&x); n++;
	XtSetArg(args[n],XmNy,&y); n++;
	XtSetArg(args[n],XmNwidth,&w); n++;
	XtSetArg(args[n],XmNheight,&h); n++;
	XtGetValues(but,args,n);
	x = x + w/2 ;
	y = y + h/2 ;
	XWarpPointer(XtDisplay(LIC_Parent),None,XtWindow(dlg),0,0,0,0,x,y);
}

int lic_mfprompt(prompt, plen, title, tlen, batch)
char *prompt, *title;
int *plen, *tlen, *batch;
{
	char c;
	int ret;
	title[*tlen] = '\0';
	prompt[*plen] = '\0';

	if (*batch==1)
	{
		printf("%s", prompt);
		c = getchar();
		if (c=='y')
			return 1;
		else
			return 2;
	}
	else
	{	
		ret = lic_mfyesno(LIC_Parent, title, prompt);
		if (ret)
			return 1;
		else
			return 2;
	}
}

lic_mfmessag(msg, mlen, title, tlen, batch)
char *msg, *title;
int *mlen, *tlen, *batch;
{
	msg[*mlen] = '\0';
	title[*tlen] = '\0';
	if (*batch)
	{
		if (LIC_Parent!=NULL)
			lic_mfmsg_box(LIC_Parent,  title, msg);
		else
			printf("%s\n", msg);
	}
	else
		lic_mfmsg_box(LIC_Parent,  title, msg);
}

/**********************************************************************
**    I_FUNCTION : yes_or_noCB()
**       action for yes or no choice
**    PARAMETERS
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Motif callback structure.  Contains
**                        answer selected by the user.
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void yes_or_noCB(w, client_data, call_data)
Widget w;
XtPointer client_data,call_data;
{
	XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)call_data;
	switch(cbs->reason)
	{
		case XmCR_OK:
			yes_or_no = 1;
			XtPopdown(XtParent(w));
			break;
		case XmCR_CANCEL:
			yes_or_no = 0;
			XtPopdown(XtParent(w));
			break;
	}
	mfanswer = 1;
}

/**********************************************************************
**    I_FUNCTION : lic_mfyesno(widget, title, msg)
**			popup a dialog box for yes or no choice
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**         			none 
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
lic_mfyesno(parent, title, msg)
Widget parent;
char *msg, *title;
{
	Widget dialog, no_button;
	XEvent x_event;
	Arg args[5];
	XmString m;
	int n  = 0;
	XmString yes = XmStringCreateSimple("Yes");
	XmString no = XmStringCreateSimple("No");
/*
.....default answer to "No"
*/
	yes_or_no = 0;
/*
...create a yesno dialog
*/
#ifdef IBM
   m = XmStringCreateSimple(msg);
#else
	m = XmStringCreateLtoR(msg, XmFONTLIST_DEFAULT_TAG);
#endif
	XtSetArg(args[n], XmNautoUnmanage, False); n++;
	XtSetArg(args[n], XmNmessageString, m); n++;
	XtSetArg(args[n], XmNokLabelString, yes); n++;
	XtSetArg(args[n], XmNcancelLabelString, no); n++;
	if(parent==NULL)
	{
		dialog = (Widget) XmCreateQuestionDialog(LIC_Parent, 
											title, args, n);
	}
	else
	{
		dialog = (Widget) XmCreateQuestionDialog(parent, 
											title, args, n);
	}
	XtVaSetValues(XtParent(dialog),XmNtitle,title,NULL);
/*
...callback
*/
	XtAddCallback(dialog, XmNokCallback, yes_or_noCB, (XtPointer)NULL);
	XtAddCallback(dialog, XmNcancelCallback, yes_or_noCB, (XtPointer)NULL);
	XmStringFree(m);
	XmStringFree(no);
	XmStringFree(yes);
/*
...no help used
*/
	XtUnmanageChild((Widget)XmMessageBoxGetChild((Widget)dialog,
									XmDIALOG_HELP_BUTTON)); 
	XtManageChild(dialog);
	XtPopup(XtParent(dialog), XtGrabNone);
	mfanswer = 0;
	while (!mfanswer)
	{
		XtAppNextEvent(LIC_App, &x_event);
		XtDispatchEvent(&x_event);
	}
	return yes_or_no;
}

/**********************************************************************
**    I_FUNCTION :  FileOkCB(widget,client_data,call_data)
**       Processes the OK button in the File Selection dialog.  Returns
**       the filename selected by the user.
**    PARAMETERS
**       INPUT  :
**          widget      = File Selection dialog shell.
**          client_data = Pointer to structure which will contain the
**                        selected filename.
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
static void FileOkCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
   char *filen, *dir;
   FSELECT *p=(FSELECT *)client_data;

/*
.....Get the directory path and filename entered
*/
   XmFileSelectionBoxCallbackStruct *cbs =
   (XmFileSelectionBoxCallbackStruct *)call_data;
   if (!XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&filen))
   {
      p->file[0] = '\0';
   }
   else
   {
      strcpy(p->file,filen);
      XtFree(filen);
   }
   p->sel = 1;
/*
.....Destroy the dialog
*/
   XtUnmanageChild(widget);
   XtDestroyWidget(widget);
}

/**********************************************************************
**    I_FUNCTION :  FileCancelCB(widget,client_data,call_data)
**       Processes the CANCEL button in the File Selection dialog.
**    PARAMETERS
**       INPUT  :
**          widget      = File Selection dialog shell.
**          client_data = Pointer to structure which will contain the
**                        blanked out filename.
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
static void FileCancelCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
/*
.....Set the filename to NULL
*/
   FSELECT *p = (FSELECT *)client_data;
   p->sel = 1;
   p->file[0] = '\0';
/*
.....Destroy the dialog
*/
   XtUnmanageChild(widget);
   XtDestroyWidget(widget);
}

/**********************************************************************
**    I_FUNCTION :  do_search (widget,sdata)
**       Creates the FileName list for the File Selection Widget using
**       the specialized file mask (file1,file2,...,filen).
**    PARAMETERS
**       INPUT  :
**          widget = File selection widget.
**          sdata  = Motif callback structure.
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
void do_search(widget, sdata)
Widget widget;
XtPointer sdata;
{
	XmString names[1024];
	char *list,*listhead;
	char fname[UX_MAX_PATH];
	int nfile,i,nc;
	char *dir,*pattern,direc[UX_MAX_PATH],file[UX_MAX_PATH];
   XmFileSelectionBoxCallbackStruct *cbs =
      (XmFileSelectionBoxCallbackStruct *)sdata;
	if (!XmStringGetLtoR(cbs->dir,XmSTRING_DEFAULT_CHARSET,&dir))
		return;
	strcpy(direc,dir);
	XtFree(dir);
	if (!XmStringGetLtoR(cbs->pattern,XmSTRING_DEFAULT_CHARSET,&pattern))
		return;
	strcpy(file,pattern);

	ul_get_flist(direc,file,&list,0,(UX_NPRTERRS|UX_NCHK));
	listhead = list;
	nfile = 0;
	if (list == NULL)
	{
		XtVaSetValues(widget,
		XmNfileListItems, NULL,
		XmNfileListItemCount, nfile,
		XmNlistUpdated, True,
		NULL);
	}
	else
	{
		while (ux_nxt_file(&list,fname,UX_NPRTERRS) == UU_SUCCESS)
		{
#ifdef VMS
			strcpy(file,fname);
			nc = strlen(file); ul_strip_blanks(file,&nc);
#else
			strcpy(file,direc); strcat(file,fname);
#endif
			names[nfile++] = XmStringCreateSimple(file);
		}
		XtVaSetValues(widget,
			XmNfileListItems, names,
			XmNfileListItemCount, nfile,
			XmNdirSpec, cbs->dir,
			XmNlistUpdated, True,
			NULL);
		if (listhead != NULL) uu_lsdel(listhead);
		for (i=0;i<nfile;i++) XmStringFree(names[i]);
	}
}
*/


/**********************************************************************
**    I_FUNCTION :  lic_mffilename(title,filter,filename,nc)
**       Opens a File Selection dialog and returns the user selected
**       filename.
**    PARAMETERS
**       INPUT  :
**          title     = Title of File Selection dialog.
**          filter    = Filename filter to use for list of available
**                      files.
**       OUTPUT :
**          filename  = Name of selected file.
**          nc        = Number of chars in 'filename'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
lic_mffilename(parent, title,filter,filename,nc)
Widget parent;
char *title,*filter,*filename;
int *nc;
{
   int i, n,jmpflag;
   void do_search();
   Arg args[20];
   Widget list;
   XmString labstr;
   XEvent event;
   FSELECT Fselect;
/*
.....Create file selection dialog
*/
   n = 0;
   labstr = XmStringCreateSimple(filter);
   XtSetArg(args[n],XmNdirMask,labstr); n++; 
/*   XtSetArg(args[n],XmNfileSearchProc,do_search); n++;  */
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
   if(parent==NULL)
   {
      fdialog = (Widget) XmCreateFileSelectionDialog
         (LIC_Parent,"file_select",args,n);
   }
   else
	{
      fdialog = (Widget) XmCreateFileSelectionDialog
         (parent,"file_select",args,n);
   }
   XtVaSetValues(XtParent(fdialog),XmNtitle,title,NULL);
   XmStringFree(labstr);
#if SUN
   list = (Widget)XmFileSelectionBoxGetChild(fdialog, XmDIALOG_LIST);
   XtVaSetValues(list, XmNscrollBarDisplayPolicy , XmSTATIC,
            NULL);
#endif
/*
.....Get rid of the HELP button
*/
	XtUnmanageChild((Widget)XmFileSelectionBoxGetChild(fdialog,XmDIALOG_HELP_BUTTON));
/*
.....Add Callbacks
*/
   Fselect.file = filename;
   XtAddCallback(fdialog,XmNcancelCallback,FileCancelCB,&Fselect);
   XtAddCallback(fdialog,XmNokCallback,FileOkCB,&Fselect);
/*
.....Manage the File Selection Dialog
*/
   XtManageChild(fdialog);

/*
.....Loop until user selects a file
*/
   Fselect.sel = 0;
   do
   {
      XtAppNextEvent(LIC_App,&event);
      XtDispatchEvent(&event);
   } while (Fselect.sel==0);
/*
.....Return filename
*/
   *nc = strlen(Fselect.file);
done:;
	return;
}
#endif
