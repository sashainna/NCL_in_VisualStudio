/*********************************************************************
**    NAME         :  tigmf.h
**       CONTAINS: 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigmf.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:54
*********************************************************************/
#ifndef IGMF
#define IGMF


#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>

#ifdef MFIGPRM
#define EXT
#else
#define EXT extern
#endif

typedef struct
{
   char *label;
   void (*callback)();
   XtPointer data;
} ActionAreaItem;


EXT int UW_MOTIF;
EXT XtAppContext TOOL_App;
EXT Widget TOOL_Parent;
EXT Widget Main_form;
EXT Widget mform_item[25], mform_choice[2];
EXT Widget tool_stat_win;

#endif
