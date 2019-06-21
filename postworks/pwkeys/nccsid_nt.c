/************************************************************************
c
c   FILE NAME: nccsid_nt.C
c   CONTAINS:
c      int WinMain
c      int read_key(char*key, int*num) 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        nccsid_nt.c , 23.1
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

int PASCAL WinMain( HANDLE hInstance,
                    HANDLE hPrevInstance,
                    LPSTR  lpszCmdLine,
                    int    nCmdShow );
#include <nb30.h>

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
	int hnum;
	int cnum, knum;
	char textstring[300];
	char string1[120];
	char* string2;
	int i, klen, clen, cfil0,kfil0;
	char num_str[20], cfil0_str[20], kfil0_str[20];
	WINUSERAPI
    HCURSOR WINAPI SetCursor(HCURSOR hCursor);
	HCURSOR  wait_csr, arror_csr;
	wait_csr = LoadCursor(NULL, IDC_WAIT);
	arror_csr = LoadCursor (NULL, IDC_ARROW);
	SetCursor(wait_csr);

	i = 0; hnum = hostid("NCLCAM",&cnum, &i);
	i = 1; hnum = hostid("NCLCAM",&knum, &i);
	if (knum == -1 || knum == 0)
	{
		i = 2; hnum = hostid("NCLCAM",&knum, &i);
	}
	cfil0_str[0] = '\0';
	kfil0_str[0] = '\0';
	sprintf(num_str, "%d", cnum);
	clen = strlen(num_str);
	cfil0 = 0;
	if ((clen<9)&&(cnum!=-1))
		cfil0 = 9-clen;
	for (i=0; i<cfil0;i++)
		cfil0_str[i] = '0';
	cfil0_str[i] = '\0';
	sprintf(num_str, "%d", knum);
	klen = strlen(num_str);
	kfil0 = 0;
	if ((klen<9)&&(knum!=-1))
		kfil0 = 9-klen;
	for (i=0; i<kfil0;i++)
		kfil0_str[i] = '0';
	kfil0_str[i] = '\0';

//	sprintf(textstring, "Numerical Control Computer Sciences\n\n   Ethernet Card #: %d\n   Dongle Key #:    %d\n", cnum, knum);
	sprintf(textstring, "Numerical Control Computer Sciences\n\n   Ethernet Card #: %09d\n   Dongle Key #:    %09d\n",
					cnum, knum);
	SetCursor(arror_csr);
	if(MessageBox( NULL, textstring, "NCCS KEY DEVICE CONTENT", MB_OK)==IDOK)
		return( msg.wParam );
} /* end WinMain */

#endif
/* end of file */
