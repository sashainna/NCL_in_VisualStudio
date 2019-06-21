/*********************************************************************
**  NAME:  PtdMenuEntries.h
**  Description:
**		Menu Entries and command IDs for Ptd
**				
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdMenuEntries.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:31
*********************************************************************/
#ifndef PTDMENUENTRIESH
#define PTDMENUENTRIESH

#include "PtdCommandInterface.h"

#ifdef PTDMAIN

PtdCmdInterfaceStruct mainFileMenu[25] = 
{
    {"New",			PtdEPushButton,	"Ctrl <Key> n", "Ctl+n", NULL,0}, 
    {"Open...",		PtdEPushButton, "Ctrl <Key> o", "Ctl+o", NULL,0}, 
	{"Include...", PtdEPushButton, NULL, NULL, NULL, 0}, 
	{"Open Cutter File", PtdEPushButton, NULL, NULL, NULL, 0}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Save",	PtdEPushButton, "Ctrl <Key> s", "Ctl+s", NULL,0},
	{"Save As",	PtdEPushButton, "Shift Ctrl <Key> s", "Shift+Ctl+s", NULL,0},
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Run Mpost",	PtdEPushButton, NULL, NULL, NULL,0},
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Load MDF",	PtdEPushButton, NULL, NULL, NULL,0},
	{"Load Input MDF",	PtdEPushButton, NULL, NULL, NULL,0},
	{"Load Output MDF",	PtdEPushButton, NULL, NULL, NULL,0},
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Close",		PtdEPushButton, NULL, NULL, NULL,0}, 
	{"Exit",		PtdEPushButton,	"Ctrl <Key> q", "Ctl+q", NULL,0}
};

PtdCmdInterfaceStruct maineditMenu[15] = 
{
    {"Cut",	PtdEPushButton,	"Ctrl <Key> x", "Ctl+x", NULL,0}, 
    {"Copy",	PtdEPushButton,	"Ctrl <Key> c", "Ctl+c", NULL,0}, 
    {"Paste",		PtdEPushButton,	"Ctrl <Key> v", "Ctl+v", NULL,0}, 
    {"Delete",		PtdEPushButton,	"Backspace <Key>", "Backspace", NULL,0},
	{"Insert Line", PtdEPushButton, "Ctrl <Key> i", "Ctl+i",NULL,0},
	{"Delete Line", PtdEPushButton, "Ctrl <Key> d", "Ctl+d",NULL,0}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Reverse", PtdEPushButton, NULL, NULL, NULL, 0}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Edit Input MDF",	PtdEPushButton, NULL, NULL, NULL,0},
	{"Edit Output MDF",	PtdEPushButton, NULL, NULL, NULL,0}
};

PtdCmdInterfaceStruct SubConvertMenu[2] =
{
	{"Convert", PtdEPushButton,   NULL, NULL, NULL, 0},
	{"Convert Range...", PtdEPushButton,   NULL, NULL, NULL, 0}
};

PtdCmdInterfaceStruct SubFormatMenu[2] =
{
	{"Format", PtdEPushButton,   NULL, NULL, NULL, 0},
	{"Format Range...", PtdEPushButton,   NULL, NULL, NULL, 0}
};

PtdCmdInterfaceStruct SubUnformMenu[2] =
{
	{"Unformat", PtdEPushButton,   NULL, NULL, NULL, 0},
	{"Unformat Range...", PtdEPushButton,   NULL, NULL, NULL, 0}
};

PtdCmdInterfaceStruct SubBadMenu[2] =
{
	{"Bad Blocks", PtdEPushButton,   NULL, NULL, NULL, 0},
	{"Bad Blocks Range...", PtdEPushButton,   NULL, NULL, NULL, 0}
};

PtdCmdInterfaceStruct SubLengthMenu[2] =
{
	{"Length", PtdEPushButton,   NULL, NULL, NULL, 0},
	{"Length Range...", PtdEPushButton,   NULL, NULL, NULL, 0}
};

PtdCmdInterfaceStruct mainconvertMenu[9] = 
{
	{"Convert",	PtdEPushButton,	NULL, NULL, SubConvertMenu, 2}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Resequence",	PtdEPushButton,	NULL, NULL, NULL,0}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Format",	PtdEPushButton,	NULL, NULL, SubFormatMenu, 2} ,
	{"Unformat",	PtdEPushButton,	NULL, NULL, SubUnformMenu, 2}, 
	{"Bad Blocks",	PtdEPushButton,	NULL, NULL, SubBadMenu, 2}, 
	{"Length",	PtdEPushButton,	NULL, NULL, SubLengthMenu, 2}, 
	{"Set Register",	PtdEPushButton,	NULL, NULL, NULL,0}
};

PtdCmdInterfaceStruct childconvertMenu[9] = 
{
	{"Convert",	PtdEPushButton,	NULL, NULL, SubConvertMenu, 2}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Resequence",	PtdEPushButton,	NULL, NULL, NULL,0}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Format",	PtdEPushButton,	NULL, NULL, SubFormatMenu, 2} ,
	{"Unformat",	PtdEPushButton,	NULL, NULL, SubUnformMenu, 2}, 
	{"Bad Blocks",	PtdEPushButton,	NULL, NULL, SubBadMenu, 2}, 
	{"Length",	PtdEPushButton,	NULL, NULL, SubLengthMenu, 2}, 
	{"Set Register",	PtdEPushButton,	NULL, NULL, NULL,0}
};

PtdCmdInterfaceStruct mainhelpMenu[1] = 
{
    {"About...",	PtdEPushButton,	NULL, NULL, NULL,0} 
};

PtdCmdInterfaceStruct childFileMenu[] = 
{
   {"Open...",		PtdEPushButton, "Ctrl <Key> o", "Ctl+o", NULL,0}, 
	{"Include...", PtdEPushButton, NULL, NULL, NULL, 0}, 
	{"Open Cutter File", PtdEPushButton, NULL, NULL, NULL, 0}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Save",	PtdEPushButton, "Ctrl <Key> s", "Ctl+s", NULL,0},
	{"Save As",	PtdEPushButton, "Shift Ctrl <Key> s", "Shift+Ctl+s", NULL,0},
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Close",		PtdEPushButton,	"Ctrl <Key> q", "Ctl+q", NULL,0}
};

PtdCmdInterfaceStruct textFileMenu[1] = 
{
	{"Close",		PtdEPushButton,	"Ctrl <Key> q", "Ctl+q", NULL,0}
};

PtdCmdInterfaceStruct childeditMenu[20] = 
{
    {"Cut",	PtdEPushButton,	"Ctrl <Key> x", "Ctl+x", NULL,0}, 
    {"Copy",	PtdEPushButton,	"Ctrl <Key> c", "Ctl+c", NULL,0}, 
    {"Paste",		PtdEPushButton,	"Ctrl <Key> v", "Ctl+v", NULL,0}, 
    {"Delete",		PtdEPushButton,	"Backspace <Key>", "Backspace", NULL,0},
	{"Insert Line", PtdEPushButton, "Ctrl <Key> i", "Ctl+i",NULL,0},
	{"Delete Line", PtdEPushButton, "Ctrl <Key> d", "Ctl+d",NULL,0}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Reverse", PtdEPushButton, NULL, NULL, NULL, 0}, 
};

PtdCmdInterfaceStruct backupFileMenu[6] = 
{
	{"Include...", PtdEPushButton, NULL, NULL, NULL, 0}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Save As",	PtdEPushButton, "Shift Ctrl <Key> s", "Shift+Ctl+s", NULL,0},
	{"Save",	PtdEPushButton, "Ctrl <Key> s", "Ctl+s", NULL,0},
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Close",		PtdEPushButton,	"Ctrl <Key> q", "Ctl+q", NULL,0} 
};

PtdCmdInterfaceStruct SubFtypeMenu[6] =
{
	{"Text File",	PtdEToggleButton, NULL, NULL, NULL, 0},
	{"Control Data",	PtdEToggleButton, NULL, NULL, NULL, 0},
	{"Binary CL File",	PtdEToggleButton, NULL, NULL, NULL, 0},
	{"APT Source",	PtdEToggleButton, NULL, NULL, NULL, 0},
	{"Simulate File",	PtdEToggleButton, NULL, NULL, NULL, 0},
	{"Cutter File",	PtdEToggleButton, NULL, NULL, NULL, 0}
};

PtdCmdInterfaceStruct mainwindowMenu[8] = 
{
	{"File Type",	PtdEPushButton,	NULL, NULL, SubFtypeMenu, 6}, 
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"To Control Data", PtdEPushButton,   NULL, NULL, NULL,0},
	{"To APT Source", PtdEPushButton,   NULL, NULL, NULL,0},
	{"To Sim File", PtdEPushButton,   NULL, NULL, NULL,0},
	{"Load Cutter Data", PtdEPushButton,   NULL, NULL, NULL,0},
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Commmand Window On", PtdEToggleButton,   NULL, NULL, NULL,0}
};

PtdCmdInterfaceStruct mainViewMenu[3] =
{
	{"Top of file", PtdEPushButton,   "Ctrl <Key> t", "Ctl+t", NULL,0},
	{"Bottom of file", PtdEPushButton, "Ctrl <Key> g", "Ctl+g",NULL,0},
	{"Status", PtdEPushButton, NULL, NULL, NULL, 0}
};

PtdCmdInterfaceStruct mainFindMenu[6] =
{
	{"Find", PtdEPushButton, "Ctrl <Key> f", "Ctl+f", NULL,0},
	{"Find Next", PtdEPushButton, "<Key>F3", "F3", NULL,0},
	{"Find Prev", PtdEPushButton, "Shift <Key>F3", "Shift+F3", NULL,0},
	{"Find All", PtdEPushButton, "Ctrl <Key> a", "Ctl+a", NULL,0},
	{"",        PtdESeparator, 0, 0, NULL,0},
	{"Replace...", PtdEPushButton, NULL, NULL, NULL,0}
}; 

PtdCmdInterfaceStruct mainoptionMenu[1] = 
{
    {"PWorks Options...",	PtdEPushButton,	NULL, NULL, NULL,0} 
};

PtdCmdInterfaceStruct childoptionMenu[1] = 
{
    {"PWorks Options...",	PtdEPushButton,	NULL, NULL, NULL,0} 
};

#endif

#ifndef PTDMAIN
extern PtdCmdInterfaceStruct mainFileMenu[25];
extern PtdCmdInterfaceStruct maineditMenu[15];
extern PtdCmdInterfaceStruct SubConvertMenu[2];
extern PtdCmdInterfaceStruct SubFormatMenu[2];
extern PtdCmdInterfaceStruct SubUnformMenu[2] ;
extern PtdCmdInterfaceStruct SubBadMenu[2];
extern PtdCmdInterfaceStruct SubLengthMenu[2];
extern PtdCmdInterfaceStruct mainconvertMenu[9];
extern PtdCmdInterfaceStruct childconvertMenu[9];
extern PtdCmdInterfaceStruct childFileMenu[8];
extern PtdCmdInterfaceStruct textFileMenu[1] ;
extern PtdCmdInterfaceStruct childeditMenu[20];
extern PtdCmdInterfaceStruct backupFileMenu[6];
extern PtdCmdInterfaceStruct SubFtypeMenu[6];
extern PtdCmdInterfaceStruct mainwindowMenu[8];
extern PtdCmdInterfaceStruct mainViewMenu[3];
extern PtdCmdInterfaceStruct mainFindMenu[6];
extern PtdCmdInterfaceStruct mainoptionMenu[1];
extern PtdCmdInterfaceStruct childoptionMenu[1];
extern PtdCmdInterfaceStruct mainhelpMenu[1]; 
#endif

#endif
