#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**
**    NAME         :  wsmf.c
**
**       CONTAINS:
**  			  uw_mfsave_layout
**  			  uw_mfload_layout
**				  uw_mflay_fexist
**    		  yes_or_noCB()
**				  uw_mfwrite_layout(fullname)
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsmflay.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:11
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:cursorfont.h>
#include <decw$include:keysym.h>
#include <decw$include:Xm.h>
#include <decw$include:PushB.h>
#include <decw$include:RowColumn.h>
#include <decw$include:DialogS.h>
#include <decw$include:PanedW.h>
#include <decw$include:Form.h>
#include <decw$include:MwmUtil.h>
#include <decw$include:Protocols.h>
#include <decw$include:Label.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/MwmUtil.h>
#include <Xm/Protocols.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#endif

#include "uims.h"
#include "dinput.h"
#include "dasnog.h"
#include "dasg.h"
#include "uhep.h"
#include "xenv1.h"
#include "wsmf.h"
#include "xfsys1.h"
#include "wsxw.h"

/*
...store layout file name
*/
static UX_pathname Slayout_file;
/*
.....added by Yurong
.....8/27/97
*/
extern UWS_MF uw_mf;
extern UWS_MFLAYOUT uw_mflayout;
extern UDM_LAYOUT UDM_layout; 
extern int UDM_menu_mapped[UDM_MAX_MENU];
extern int XBorder, YBorder;

/**********************************************************************
**    I_FUNCTION : yes_or_noCB()
**			action for yes or no choice
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = layout filename.
**				call_data   = Motif callback structure.  Contains 
**				              answer selected by the user.
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void yes_or_noCB(w, client_data, call_data)
Widget w;
XtPointer client_data,call_data;
{
	int status;
	char msg[UX_MAX_PATH_LEN+40];
	XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)call_data;
	switch(cbs->reason)
	{
		case XmCR_OK:
			status = ux_delete(Slayout_file, UX_PRTERRS);
			if (status==0)
				uw_mfwrite_layout(Slayout_file);
			else
			{
				sprintf(msg, "Can not delete file %s!", Slayout_file);
				uw_mfmessage(uw_mf.graphic_app, msg);
			}
	 		XtPopdown(XtParent(w));
			break;
		case XmCR_CANCEL:
	 		XtPopdown(XtParent(w));
			break;
	}
}

/**********************************************************************
**    I_FUNCTION : uw_mflay_fexist(widget)
**			create a question dialog.
**    PARAMETERS   
**       INPUT  : 
**              widget:  parent od dialog 
**              filename: save layout file name
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void uw_mflay_fexist(widget)
Widget widget;
{
	Widget dialog;
	Arg args[5];
	int n  = 0;
/*
...create a yesno dialog
*/
	XmString m = XmStringCreateSimple("File Exists, Overwrite?");
	XmString yes = XmStringCreateSimple("Yes");
	XmString no = XmStringCreateSimple("No");
	XtSetArg(args[n], XmNautoUnmanage, False); n++;
	XtSetArg(args[n], XmNmessageString, m); n++;
	XtSetArg(args[n], XmNokLabelString, yes); n++;
	XtSetArg(args[n], XmNcancelLabelString, no); n++;
	dialog = (Widget)XmCreateQuestionDialog(widget, "File Exists", args, n);
	XtVaSetValues(XtParent(dialog),XmNtitle,"File Exists",NULL);
/*
...callback
*/
	XtAddCallback(dialog, XmNokCallback, (XtCallbackProc)yes_or_noCB, NULL);
	XtAddCallback(dialog, XmNcancelCallback, yes_or_noCB,NULL);
	XmStringFree(m);
	XmStringFree(no);
	XmStringFree(yes);
	XtUnmanageChild((Widget)XmMessageBoxGetChild((Widget)dialog,XmDIALOG_HELP_BUTTON)); 

	XtManageChild(dialog);
	XtPopup(XtParent(dialog), XtGrabNone);
}


/*************************************************************************
**
**  E_FUNCTION         :  uw_mfsave_layout(infile)
**     select and save the layout into a layout file 
**
**  PARAMETERS   
**      INPUT  : 
**          infile = Name of file to save, or blank if the file should be
**                   prompted for.
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

void uw_mfsave_layout(infile)
char *infile;
{

	UX_pathname fname, fullname, name, dir, fdir;
	int mode, status;
	int len, found;
	char *pathlistptr = 0;
/*
.....The filename is provided
*/
	if (infile[0] != '\0')
	{
		strcpy(fname,infile);
	}
/*
.....Get the filename
*/
	else
	{
/*
.....default the saving directory to UU_USER_SETTING directory, if this
.....directory is not there, create one
*/
		fname[0] = '\0';
		status = ul_open_mod_file("UU_USER_SETTINGS", "layout", (char*)UU_NULL,
					(char*)UU_NULL, fname, 1, (FILE**)UU_NULL);
      if ((status == UU_SUCCESS) && (fname[0]!='\0'))
         strcat (fname, "/*.lay");
		uw_mfget_filename(uw_mf.graphic_app, "Save Layout File","*.lay", fname,
			&len);
		if((fname[len-1]=='/')||(fname[0]=='\0'))
         return;
	}
/*
.....open for read to check if file exist
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "layout", (char*)UU_NULL,
				(char*)UU_NULL, fname, 0, (FILE**)UU_NULL);
/*
.....open for read to check if file exist
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "layout", (char*)UU_NULL,
			(char*)UU_NULL, fname, 0, (FILE**)UU_NULL);
/*
...file exists, overwrite (y/n)?
*/
   if ((status == UU_SUCCESS) && (fname[0]!='\0') && !ul_session_active())
   {
		strcpy(Slayout_file, fname);
		uw_mflay_fexist(uw_mf.graphic_app);
		return;
   }
/*
.....Save the layout file
*/
	uw_mfwrite_layout(fname);
}
	
/*************************************************************************
**
**  E_FUNCTION         :  uw_mfwrite_layout()
**     save the layout into a layout file 
**
**  PARAMETERS   
**      INPUT  : 
**          none
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

uw_mfwrite_layout(fullname)
char* fullname;
{
	int layout, i;
	FILE *fd;
	char *ptr2;
	char num[UX_MAX_PATH_LEN+40];
	UX_pathname dir1,dir,filename,ptr1;
	float temp1, temp2, temp3, temp4;
	int status;
	char msg[UX_MAX_PATH_LEN+40];
	status = ux_create_file(fullname, 0666, UU_NULL, "STREAM", "ASCII",
		"UX_NOHEADER", &layout, UX_PRTERRS);
	if (status == 0)
	{
		ux_get_os_filedesc(layout, &fd, UX_PRTERRS);
/*
.....save the menu directory
.....default to NCL_MENU
.....added Yurong 1/21/1999
*/
		ux_fputs0("\n#DIRECTORY#\n", fd);
		ux_search_for_path("NCL_MENU", dir1, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
		sprintf(dir, "/MENU/ \"%s\"\n", dir1);
		ux_fputs0(dir, fd);
/*
...save the menu layout
*/
		ux_fputs0("\n#MENUS#\n", fd);
		for( i = 0; i< UDM_menu_count; i++)
		{
			if((uw_mflayout.menu_app[i]!=NULL)&&(UDM_menu_mapped[i]==1))
			{
/*
...menu_pos is pixel number, need change to percentage of screen
*/
				temp1 = UDM_layout.menu_pos[i][0];
				temp2 = UDM_layout.menu_pos[i][1];
				if (temp1<0) temp1 = 0;
				if (temp2<0) temp2 = 0;
				temp1 = temp1/(float)uw_xw.dev_xmax;
				temp2 = temp2/(float)uw_xw.dev_ymax;
				temp3 = UDM_layout.menu_size[i][0];
				temp4 = UDM_layout.menu_size[i][1];			
				temp3 = temp3/(float)uw_xw.dev_xmax;
				temp4 = temp4/(float)uw_xw.dev_ymax;
/*
.....only save the menu file name, exclude whole path
.....Yurong 12/17/97
*/
				ptr2 = ptr1;
				if((ptr2 = strrchr(UDM_menu[i].file,'/'))!=NULL)
					strcpy(filename, ptr2+1) ;
				else
					strcpy(filename, UDM_menu[i].file);
				sprintf(num, "/MENU/ %s,%f,%f,%f,%f\n",filename,
							temp1, temp2, temp3, temp4);
				ux_fputs0(num, fd);
			}
		}
/*
...save graphic layout
*/
		ux_fputs0("\n#GRAPHICS#\n",fd);
		temp1 = (float)(UDM_layout.graphic_pos[0])/(float)uw_xw.dev_xmax;
		temp2 = (float)(UDM_layout.graphic_pos[1])/(float)uw_xw.dev_ymax;
		sprintf(num, "/POSITION/ %f,%f\n", temp1, temp2);
		ux_fputs0(num, fd);
		temp1 = (float)(UDM_layout.graphic_size[0])/(float)uw_xw.dev_xmax;
		temp2 = (float)(UDM_layout.graphic_size[1])/(float)uw_xw.dev_ymax;
		sprintf(num, "/SIZE/ %f,%f\n", temp1, temp2);
		ux_fputs0(num, fd);
		
/*
...save prompts layout
*/
		ux_fputs0("\n#PROMPTS#\n",fd);
		temp1 = (float)(UDM_layout.prompt_pos[0])/(float)uw_xw.dev_xmax;
		temp2 = (float)(UDM_layout.prompt_pos[1])/(float)uw_xw.dev_ymax;
		sprintf(num, "/POSITION/ %f,%f\n", temp1, temp2);
		ux_fputs0(num, fd);
		temp1 = (float)(UDM_layout.prompt_size[0])/(float)uw_xw.dev_xmax;
		temp2 = (float)(UDM_layout.prompt_size[1])/(float)uw_xw.dev_ymax;
		sprintf(num, "/SIZE/ %f,%f\n", temp1, temp2);
		ux_fputs0(num, fd);
/*
...save status layout
*/
		ux_fputs0("\n#STATUS#\n",fd);
		temp1 = (float)(UDM_layout.status_pos[0])/(float)uw_xw.dev_xmax;
		temp2 = (float)(UDM_layout.status_pos[1])/(float)uw_xw.dev_ymax;
		sprintf(num, "/POSITION/ %f,%f\n", temp1, temp2);
		ux_fputs0(num, fd);
		temp1 = (float)(UDM_layout.status_size[0])/(float)uw_xw.dev_xmax;
		temp2 = (float)(UDM_layout.status_size[1])/(float)uw_xw.dev_ymax;
		sprintf(num, "/SIZE/ %f,%f\n", temp1, temp2);
		ux_fputs0(num, fd);

		ux_close(layout, UX_PRTERRS);
	}
	else
	{
		sprintf(msg, "Can not create file %s!", fullname);
		uw_mfmessage(uw_mf.graphic_app, msg);
	}
	return (UU_TRUE);
}
/**************************************************************************
**
**  E_FUNCTION         :  uw_mfload_layout(infile)
**     load the layout from a layout file 
**		 	
**  PARAMETERS   
**      INPUT  : 
**          infile = Name of file to load, or blank if the file should be
**          none
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

int uw_mfload_layout(infile)
char *infile;
{
	UX_pathname fname, dir, fdir,name;
	int i, mode, status;
	void uw_mfgraphicCB();
	int len;
/*
.....The filename is provided
*/
	if (infile[0] != '\0')
	{
		strcpy(fname,infile);
	}
/*
.....Get the filename
*/
	else
	{
		uw_mfget_filename(uw_mf.graphic_app,"Load Layout File","*.lay", fname, &len);
/*
...check if we get the file name
*/
		if((fname[len-1]=='/')||(fname[0]=='\0'))
			return; 
		strcpy(infile, fname);
	}
/* 
...Check for file existence 
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "layout", "M_UD_LAYOUT_DIR",
					(char*)UU_NULL, fname, 0, (FILE**)UU_NULL);
	if (!((status == UU_SUCCESS) && (fname[0]!='\0')))
	{
 		uu_uerror0(UD_DASHEP, 61);
		return;
	}
/*
...destroy the displayed layout
*/
	if(uw_mf.graphic_app!=NULL)
		XtDestroyWidget(uw_mf.graphic_app);
	if(uw_mf.status_app!=NULL)
		XtDestroyWidget(uw_mf.status_app);
	if(uw_mf.prompt_app!=NULL)
		XtDestroyWidget(uw_mf.prompt_app);
	for( i = 0; i< UDM_menu_count; i++)
	{
		if(uw_mflayout.menu_app[i]!=NULL)
		{
			XtUnrealizeWidget(uw_mflayout.menu_app[i]);
			XtDestroyWidget(uw_mflayout.menu_app[i]);
			uw_mflayout.menu_app[i] = NULL;
		}
	}
/*
...if Menu Design is active, destroy it
*/
	uw_mfremove_desgn_menu();
/*
...read layout file
*/
	udm_read_layout(&UD_duimsdeflt, fname); 
/*
...we need display all the Interface layout again
*/
	XtMapWidget(uw_mf.prompt_app);
	XtMapWidget(uw_mf.status_app);
	XtMapWidget(uw_mf.graphic_app);
	for (i=0;i<UDM_layout.nmenu;i++)
		XtMapWidget(uw_mflayout.menu_app[i]);
/*
.....Trap Resize events
*/
	XtAddCallback(uw_mf.graphic,XmNresizeCallback,uw_mfgraphicCB,NULL);
	uw_mfresize_graphics();
	znu_copyright();
	uz_status();
	uw_mfflush();
	ud_updatews(UG_SUPPRESS);
}
#endif

