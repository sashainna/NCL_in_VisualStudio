/************************************************************************
c
c   FILE NAME: nccsid_writent.C
c   CONTAINS:
c      int WinMain
c      int read_key(char*key, int*num) 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        nccsid_writent.c , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:12
c
c**********************************************************************
*/
#ifdef WNT

#include <windows.h>               /* for all Window apps                  */
#include <stdio.h>                 /* for sscanf()                         */
#include <string.h>                /* for strcpy()                         */
#include <ctype.h>                 /* for isxdigit() isascii()             */
#include <stdlib.h>                /* for atoi()                           */
#include "scribe_nt.h"                /* Netinel-C API function prototypes    */
#include "spromeps.h"
#include "nccs_mid.h"

#if (defined(WIN32) || defined(_WIN32) || defined(_WIN32_))
#undef _WIN32_
#define _WIN32_
#endif

#define WINDOW_TITLE (LPSTR)"Sentinel-Scribe Driver Evaluation Program"
#define APP_NAME     (LPSTR)"SCRIBE"
#define ICON_NAME    (LPSTR)"RNBO"

#if     defined(_MSC_VER)          /* Microsoft C/C++ compiler             */
#if     defined(_WIN32_)           /* for WIN32/WIN32s                     */
#define EXPORT
#else                              /* for WIN16                            */
#define EXPORT _export             /* for exported callback routines       */
#endif
#elif  defined(__BORLANDC__)       /* Borland C/C++ Compiler               */
#define EXPORT _export             /* for exported callback routines       */
#endif

#if    defined(__cplusplus)
#define EXPORT_PROC    extern "C"
#else
#define EXPORT_PROC    extern
#endif
#define LOCAL_PROC     static

#define DEF_PORT_NUM    0x0004     /* for LPT1 BUSY keys                   */

#define START_EVAL      0x1234     /* Message to start eval program        */
#define PPROC           FARPROC    /* for MakeProcInstance()               */

#if    defined(_WIN32_)              /* Win32 and Win32s 32 bit              */
#define MESSAGE         UINT
#define GET_LOWORD(param)  LOWORD(param)
#define GET_HIWORD(param)  HIWORD(param)
#else                              /* Windows 16 bit                       */
#define MESSAGE         WORD
#define WPARAM          WORD
#define GET_LOWORD(param)   param
#define GET_HIWORD(param)   param
#endif

#if defined(_WIN32_)
#define xFreeProcInstance( param )
#else
#define xFreeProcInstance( param )   FreeProcInstance( param )
#endif

#define STR_BUF_SIZE 255
#define USB_KEY 0
#define PAR_KEY 1

int gDevType = USB_KEY;
RB_WORD gDevId = 0x9DE7;
RB_WORD gPasswd = 0xE350;
RB_WORD gPasswd1 = 0xC98A;
RB_WORD gPasswd2 = 0x30F3;
RB_BYTE WriteAccess = 0;

RB_SCRIBE_APIPACKET gSKApiPacket;  /* Scribe packet */
RB_SPRO_APIPACKET gSproApiPacket;

HWND    ghMainDlg = NULL;
HWND  ghWndMain = NULL;
HANDLE  ghInst;

int PASCAL WinMain( HANDLE hInstance,
                    HANDLE hPrevInstance,
                    LPSTR  lpszCmdLine,
                    int    nCmdShow );

EXPORT_PROC
BOOL WINAPI MAINMsgProc( HWND    hWndDlg,
                             MESSAGE Message,
                             WPARAM  wParam,
                             LPARAM  lParam );
int get_key_info(HWND hWnd, char*keystring, int*num);
int verify_key(HWND hWnd);
int write_key(char*key, int*num);
int read_key2(char*key, int*num, int *idat);


//****************************************************************************/
/***************************************************************************
* Function : WinProc
*
* Purpose  : Windows Main Procedure
*
* Inputs   : hWnd    - handle to the current window.
*            Message - is the message to process.
*            wParam  - is a WPARAM size parameter, if any.
*            lParam  - is a LPARAM size parameter, if any.
*
* Outputs  : none.
*
* Returns  : LONG    
****************************************************************************/

#if    defined(__BORLANDC__)
#pragma argsused
#endif
EXPORT_PROC
LONG WINAPI WndProc( HWND    hWnd,
						MESSAGE Message,
						WPARAM  wParam,
						LPARAM  lParam )
{
	PPROC lpfnMAINMsgProc;

	switch ( Message )
	{
		case WM_COMMAND:
			switch( GET_LOWORD(wParam) )
			{
				case ID_START:lpfnMAINMsgProc = MakeProcInstance( (PPROC)MAINMsgProc,
													ghInst );
					if ( lpfnMAINMsgProc )
					{
						ghMainDlg = CreateDialog( ghInst,MID_DIALOG,
										hWnd,
										lpfnMAINMsgProc );
						if ( ghMainDlg )
							ShowWindow( ghMainDlg, SW_SHOW );
					}
					break;

				default:
					return( DefWindowProc(hWnd, Message, wParam, lParam) );
			}
			break;

		case WM_CREATE:
			PostMessage( hWnd,
						(MESSAGE)WM_COMMAND,
						(WPARAM)ID_START,
						(LPARAM)NULL );
			break;

		case WM_CLOSE:
			if ( ghMainDlg )
				PostMessage( ghMainDlg,
							(MESSAGE)WM_CLOSE,
							(WPARAM)0,
							(LPARAM)0 );
			xFreeProcInstance( lpfnMAINMsgProc );
			DestroyWindow( hWnd );
			if ( hWnd == ghWndMain ) PostQuitMessage(0); /* Quit */
			break;

		default:
			return( DefWindowProc(hWnd, Message, wParam, lParam) );
	}
	return( 0L );
} /* end of WndProc */

/***************************************************************************
* Function : MAINMsgProc
*
* Purpose  : To handle the main dialog box.
*
* Inputs   : hWndDlg - handle to a dialog.
*            Message - is the message to process.
*            wParam  - is a WPARAM size parameter, if any.
*            lParam  - is a LPARAM size parameter, if any.
*
* Outputs  : none.
*
* Returns  : BOOL    - TRUE  if we process the message
*                      FALSE if not.
****************************************************************************/
#if    defined(__BORLANDC__)
#pragma argsused
#endif
EXPORT_PROC
BOOL WINAPI MAINMsgProc( HWND    hWndDlg,
							MESSAGE Message,
							WPARAM  wParam,
							LPARAM  lParam )
{
	char keystring[116];
	int hnum;
	switch( Message )
	{
		case WM_CLOSE:
			PostMessage( hWndDlg, (MESSAGE)WM_COMMAND, IDCANCEL, (LPARAM)NULL );
			break;
		case WM_COMMAND:
			switch( GET_LOWORD(wParam) )
			{
				case ID_WRITE:
					if (get_key_info(hWndDlg, keystring, &hnum)==0)
					{
						if(write_key(keystring, &hnum)!=1)
						{
							if(MessageBox( hWndDlg, "Write Device key failed", "Error", MB_OK)==IDOK)
								break;
						}
						else
						{
							if(MessageBox( hWndDlg, "Write Device key completed", "Write success", MB_OK)==IDOK)
								break;
						}
					}

				case ID_READ:
					verify_key(hWndDlg);
					break;

				case IDCANCEL:
					DestroyWindow( hWndDlg );
					ghMainDlg    = NULL;
					PostMessage( ghWndMain,
								(MESSAGE)WM_CLOSE,
								(WPARAM)0,
								(LPARAM)0 );
					break;
				}
			break;

		default:
			return( FALSE );
	}
	return( TRUE );
} /* end MAINMsgProc  */
/****************************************************************
c
c   FUNCTION: get_key_info(HWND hWnd, char*keystring, int*num)
c
c     This utility is used to get user input for Key device
c	INPUT
C		HWND:hWnd Dialog box handle 
c	OUTPUT
C		keystring: License text
c		num: Host ID
c	Return 0: success
c		   not 0: failed	
c
c****************************************************************/
int get_key_info(HWND hWnd, char*keystring, int*num)
{
	char cust_name[40];
	char cust_num[8], comp_num[3], hostid[10];
	int len;
	len = GetDlgItemText( hWnd, ID_EDIT1,cust_name, 40);
	cust_name[len+1]= '\0';
	sprintf(keystring, "Numerical Control Computer Sciences Licensed for: %s", cust_name);
	len = GetDlgItemText( hWnd, ID_EDIT2,cust_num, 8);
	if (len<8)
		cust_num[len+1] = '\0';
	len = GetDlgItemText( hWnd, ID_EDIT3,comp_num, 3);
	if (len<3)
		comp_num[len+1] = '\0';
	sprintf(hostid, "1%s%s",comp_num, cust_num);
	*num = atoi(hostid);
	return 0;
}
/****************************************************************
c
c   FUNCTION: verify_key(HWND hWnd)
c
c     This utility is used to read the SCRIBE key device
c		and display a box with all the key content
c	INPUT
C		none
c	OUTPUT
c		HWND: parent of MessageBox
c	Return 1: success
c		   0: failed	
c
c****************************************************************/

int verify_key(HWND hWnd)
{
	char keystring[120];
	int hnum,dnum,knc;
	char string1[120],sbuf[20];
	char* string2;
	char textstring[300];

	if (!read_key2(keystring, &hnum, &dnum))
	{
		if(MessageBox( hWnd, "Read Device key failed", "Error", MB_OK)==IDOK)
			return 0;
	}
	strncpy(string1, keystring, 40);
	string1[35] = '\0';
	string2 = &(keystring[35]);

	if (gDevType == USB_KEY)
	{
		pwddvc(&dnum,&sbuf,&knc);
		sprintf(textstring,
			" \n  %s\n\n  %s\n\n   Host ID: %09d\n\n   Date Written: %s",
			string1, string2, hnum, sbuf);
	}
	else
	{
		sprintf(textstring,
			" \n  %s\n\n  %s\n\n   Host ID: %09d",
			string1, string2, hnum);
	}
	if(MessageBox( hWnd, textstring, "NCCS KEY DEVICE CONTENT", MB_OK)==IDOK)
		return 1;
}
/****************************************************************
c
c   FUNCTION: write_key(char*key, int*num)
c
c     This utility is used to write the SCRIBE key device
c	INPUT
C		key: License text
c		num: Host ID
c	OUTPUT
C		None
c	Return 1: successfully write
c		   -1: failed	
c
c****************************************************************/

int write_key(char*key, int*num)
{
	static RB_WORD      rCell;
	static RB_WORD      rData;
	int i,ist,nc1,nc2,kerr;
	char lbuf1[20],lbuf2[20];
	BOOL status;
	union KEYWD 
	{
		RB_WORD sint[58];
		char chstr[116];
	} key_union;
	union KEYNUM
	{
		RB_WORD sint[2];
		int host_num;
	} key_num;
	union KEYDATE
	{
		RB_WORD sint[2];
		int host_date;
	} key_date;

	for (i=0; i<116; i++)
		key_union.chstr[i] = key[i];
	key_num.host_num = *num;
	pwddnm (&key_date.host_date);

	ist = 0;
	if (gDevType == USB_KEY) ist = 8;
	for (rCell=ist; rCell<60; rCell++)
	{
		if ((gDevType == PAR_KEY && rCell<58) ||
			(gDevType == USB_KEY && rCell<56))
			rData = key_union.sint[rCell-ist];
		else if (rCell < 58)
			rData = key_date.sint[rCell-56];
		else
			rData = key_num.sint[rCell-58];
		if (gDevType == USB_KEY)
			status = RNBOsproOverwrite(&gSproApiPacket,gPasswd,gPasswd1,gPasswd2,
				rCell,rData,WriteAccess);
		else
			status = RNBOskWrite( &gSKApiPacket,rCell,rData);
		if (status!=0) 
		{
			if (rCell==ist) return 0;
			break;
		}
	
	}
	return 1;
}
/****************************************************************
c
c   FUNCTION: read_key(char*key, int*num)
c
c     This utility is used to read the SCRIBE key device
c	INPUT
C		none
c	OUTPUT
C		key: License text
c		num: Host ID
c	Return 1: success
c		   0: failed	
c
c****************************************************************/

int read_key2(char*key, int*num, int *idat)
{
	static RB_WORD      rCell;
	static RB_WORD      rData;
	int i,ist;
	BOOL status;
	union KEYWD 
	{
		RB_WORD sint[58];
		char chstr[116];
	} key_union;
	union KEYNUM
	{
		RB_WORD sint[2];
		int host_num;
	} key_num;
	union KEYDATE
	{
		RB_WORD sint[2];
		int host_date;
	} key_date;


	key[0] = '\0';
	ist = 0;
	if (gDevType == USB_KEY) ist = 8;
	for (rCell=ist; rCell<60; rCell++)
	{
		if (gDevType == USB_KEY)
			status = RNBOsproRead( &gSproApiPacket, rCell, &rData );
		else
			status = RNBOskRead( &gSKApiPacket, rCell, &rData );
		if (status!=0) 
		{
			key_union.sint[rCell-ist] = '\0';
			if (rCell==ist) return 0;
			break;
		}
		if ((gDevType == PAR_KEY && rCell<58) ||
			(gDevType == USB_KEY && rCell<56))
			key_union.sint[rCell-ist] = rData;
		else if (rCell < 58)
			key_date.sint[rCell-56] = rData;
		else
			key_num.sint[rCell-58] = rData;
	}
	for (i=0; i<116; i++)
		key[i] = key_union.chstr[i];
	key[116]= '\0';
	*num = key_num.host_num;
	*idat = key_date.host_date;
	return 1;
}

/***************************************************************************
* Function : WinMain
*
* Purpose  : Main function for nccs_id
*
* Inputs   : hInstance     - a handle of the current instance.
*            hPrevInstance - a handle of the previouse instance.
*            lpszCmdLine   - pointer to command line parameters.
*            nCmdShow      - flag indicating if window should be visible.
*
* Outputs  : none.
*
* Returns  :
****************************************************************************/

#if   defined(__BORLANDC__)
#pragma argsused
#endif
EXPORT_PROC
int PASCAL WinMain( HANDLE hInstance,
                    HANDLE hPrevInstance,
                    LPSTR  lpszCmdLine,
                    int    nCmdShow )
{
	static char szAppName[] = "OFWin";
	MSG          msg;
	BYTE password1[20] = "mqJTkdevdxEbhyZh";
    BYTE password2[20] = "dRddddtZaTdIvpma";
	SCRIBE_STATUS    status;

	WINUSERAPI
    HCURSOR WINAPI SetCursor(HCURSOR hCursor);
	HCURSOR  wait_csr, arror_csr;
	HANDLE      hMem;
	PWNDCLASS   pWndClass;
	BOOL        bSuccess;
	ghInst = hInstance ;
	hMem = LocalAlloc( LMEM_FIXED | LMEM_ZEROINIT, sizeof(WNDCLASS) );
	if ( !hMem ) return( FALSE );
	pWndClass = (PWNDCLASS)LocalLock( hMem );

	if ( !pWndClass )
	{
		LocalUnlock( hMem );
		LocalFree( hMem );
		return( FALSE );
	}

	pWndClass->style       = 0;
#if defined(__BORLANDC__)
	((PPROC)pWndClass->lpfnWndProc) = (PPROC)WndProc;
#else
	pWndClass->lpfnWndProc = WndProc;
#endif    
	pWndClass->cbClsExtra = 0;
	pWndClass->cbWndExtra = 0;
	pWndClass->hInstance  = hInstance;
	pWndClass->hIcon      = LoadIcon(ghInst, IDI_APPLICATION);
	pWndClass->hCursor    = NULL;

	pWndClass->hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
	pWndClass->lpszMenuName  = (LPSTR)NULL;
	pWndClass->lpszClassName = "NCCS_MAKEID";         

  
	bSuccess = RegisterClass( pWndClass );
	LocalUnlock( hMem );
	LocalFree( hMem );


	ghWndMain = CreateWindow( "NCCS_MAKEID",   
								"NCCS MAKE ID", 
								WS_CLIPCHILDREN |
								WS_MAXIMIZE |
								WS_OVERLAPPED,
								CW_USEDEFAULT, 0,
								CW_USEDEFAULT, 0,
								NULL,          
								NULL,         
								hInstance,      
								NULL );       


	wait_csr = LoadCursor(NULL, IDC_WAIT);
	arror_csr = LoadCursor (NULL, IDC_ARROW);

/*
.....Initialize the USB device
*/
	status = RNBOsproFormatPacket(&gSproApiPacket,sizeof(gSproApiPacket));
	status = RNBOsproInitialize(&gSproApiPacket);
	status = RNBOsproSetContactServer(&gSproApiPacket,"no-net");
	status = RNBOsproFindFirstUnit(&gSproApiPacket,gDevId);
	gDevType = USB_KEY;
/*
.....USB device not found
.....Initialize the parallel device
*/
	if (status != SP_SUCCESS)
	{
/* 
.....RNBOskInitialize: Initialize the RB_NC_APIPACKET and driver.
*/  
		status = RNBOskFormatPacket( &gSKApiPacket, SCRIBE_APIPACKET_SIZE );
		status = RNBOskInitialize( &gSKApiPacket );
/*
.....pass in password
*/
		status = RNBOskEnable( &gSKApiPacket,
                 password1  ,
                (RB_WORD)(strlen(password1)));
		status = RNBOskEnable( &gSKApiPacket,
                 password2  ,
                (RB_WORD)(strlen(password2)));
		gDevType = PAR_KEY;
	}

	while( GetMessage( &msg, NULL, 0, 0 ) )       
	{
		if ( !IsDialogMessage( ghMainDlg, &msg ) )
		{
			SetCursor(wait_csr);
			TranslateMessage( &msg );
			DispatchMessage( &msg );
			SetCursor(arror_csr);
		}
	}

	return( msg.wParam );
} /* end WinMain */
#endif
/* end of file */
