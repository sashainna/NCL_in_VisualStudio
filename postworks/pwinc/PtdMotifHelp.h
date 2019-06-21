/*********************************************************************
**  NAME:  PtdMotifHelp.h
**  Description:
**				Simple Window manager functions to provide
**				sanity to the application
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdMotifHelp.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:31
*********************************************************************/

/*
.....On VAX, we have to compile SUiMotifHelp.C using C compiler, not C++
.....Otherwise, it will have problem with passing parameters
.....added by Yurong 3/9/98
*/
#ifdef VAXVMS
#ifdef __cplusplus
extern "C"
{
#endif
#endif

extern Widget XkGetVendorShell(Widget w);

extern void XkRemapMotifDeleteButton(Widget w, 
		void (*func) (Widget, XtPointer , XtPointer),	
		XtPointer user_data);		/* User data for func */
extern void XkDisableMotifResizeButton(Widget w);

// Read a file into a buffer
extern char *XkGetSource(char *filename);

// Spread a list of buttons evenly inside a form widget
extern void XkSpreadEven(Widget *,Cardinal,Dimension,Dimension);

// Search for a file in the specified environment
extern char *XkSearchInEnv(char *);

// get the file length
extern int XkGetFileLength(int);

// support function for XkSearchInEnv
extern char *XkNextCap(char *, int);

// does file exist ??
extern int XkFileExist(char *);

// system call
extern void XkSystemCall(char *);

// convert from a char * to font list type
extern XmFontList XkConvertFontList(Widget,char *);

/* The following macros are used in converting string values to the form
   required by the widgets */

#define	RES_CONVERT( res_name, res_value) \
	XtVaTypedArg, (res_name), XmRString, (res_value), strlen(res_value) + 1

#define	XkPutStrRes( wgt, res_name, res_value ) \
	XtVaSetValues( wgt, RES_CONVERT( res_name, res_value ), NULL )

#ifdef VAXVMS
#ifdef __cplusplus
}
#endif
#endif

