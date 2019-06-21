/*
 *	Menu Entries and command IDs for PostWorks
 *
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PwMenuEntries.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:37
c
 */


#ifndef suicommandinterface_h
#define suicommandinterface_h
#include "SUiCommandInterface.h"
#endif

SUiCmdInterfaceStruct mainFileMenu[] = 
{
//    {"New",			SUiEPushButton,	"Alt <Key> n", "Alt+n", NULL,0}, 
//    {"Open...",		SUiEPushButton, "Alt <Key> o", "Alt+o", NULL,0}, 
//	{"",			SUiESeparator,	0,	0, NULL,0},
	{"Load MDF File...",	SUiEPushButton, NULL, NULL, NULL,0},
	{"Save MDF File",	SUiEPushButton, NULL, NULL, NULL,0},
	{"",			SUiESeparator,	0,	0, NULL,0},
        {"Create Documentation...",     SUiEPushButton, NULL, NULL, NULL,0},
        {"Create Machine",      SUiEPushButton, NULL, NULL, NULL,0},
	{"",			SUiESeparator,	0,	0, NULL,0},
	{"Quit",		SUiEPushButton,	"Alt <Key> q", "Alt+q", NULL,0}, 
};

static SUiCmdInterfaceStruct mainviewMenu[] = 
{
    {"View Document...",	SUiEPushButton,	NULL, NULL, NULL,0}, 
    {"View Machine...",		SUiEPushButton,	NULL, NULL, NULL,0}, 
};

static SUiCmdInterfaceStruct mainhelpMenu[] = 
{
    {"About...",	SUiEPushButton,	NULL, NULL, NULL,0}, 
};
