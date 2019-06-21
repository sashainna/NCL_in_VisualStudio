/************************************************************************
c
c   FILE NAME: Hostid_nt.C
c   CONTAINS:
c      int hostid(int*num)
c      int read_key(char*key, int*num) 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			hostid_nt.c , 14.6
c     DATE AND TIME OF LAST  MODIFICATION
c        2/17/0 , 17:42:29
c
c**********************************************************************
*/
#ifdef WNT

#include <windows.h>               /* for all Window apps                  */
#include "scribe_nt.h"                /* Netinel-C API function prototypes    */


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




RB_SCRIBE_APIPACKET gSKApiPacket;  /* Scribe packet    */

#include <nb30.h>

typedef struct _ASTAT_
{
   ADAPTER_STATUS adapt;
   NAME_BUFFER    NameBuff [30];
}ASTAT, * PASTAT;

ASTAT Adapter;
                                    

/****************************************************************
c
c   FUNCTION:
c
c     This utility is used to read the SCRIBE key device
c	INPUT
C		none
c	OUTPUT
C		key: License text
c		num: Host ID
c	Return none
c
c****************************************************************/


int read_key(char*key, int*num)
{
   static RB_WORD      rCell;
   static RB_WORD      rData;
   int i;
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


	key[0] = '\0';
	for (rCell=58; rCell<60; rCell++)
	{
		status = RNBOskRead( &gSKApiPacket,
			                 rCell,
				             &rData );
		if (status!=0) 
		{
			key_union.sint[rCell] = '\0';
			break;
		}
		if (rCell<58)
			key_union.sint[rCell] = rData;
		else
			key_num.sint[rCell-58] = rData;
	}
	for (i=0; i<116; i++)
		key[i] = key_union.chstr[i];
	key[116]= '\0';
	*num = key_num.host_num;
	return 1;
}

/***************************************************************************
* Function : hostid(num)
*
* Purpose  : Get the maching ID
*
* Inputs   : none
*
* Outputs  : num: Host ID
*
* Returns  : none
****************************************************************************/
hostid(knum, kflag)
int *knum, *kflag;
{

   NCB Ncb;
   UCHAR uRetCode;
   int isav,inum;
/*
   union pw
	{
		char buf[8];
		int ibuf;
	};
	union pw un;
*/
   union pw
	{
		UCHAR buf[4];
		unsigned int ibuf;
	};
	union pw un1;
	union pw un2;

	if (*kflag==1)
	{
		inum = gethost_key();
		*knum = inum;
		return;
	}
   memset( &Ncb, 0, sizeof(Ncb) );
   Ncb.ncb_lana_num = -1;
repeat:;
   Ncb.ncb_command = NCBRESET;
   Ncb.ncb_lana_num++;

   uRetCode = Netbios( &Ncb );
   if (uRetCode != 0 && Ncb.ncb_lana_num < 25) goto repeat;
   isav = Ncb.ncb_lana_num;
   memset( &Ncb, 0, sizeof (Ncb) );
   Ncb.ncb_command = NCBASTAT;
   Ncb.ncb_lana_num = isav;
 
   strcpy( Ncb.ncb_callname,  "*               " );
   Ncb.ncb_buffer = (char *) &Adapter;
   Ncb.ncb_length = sizeof(Adapter);
 
   uRetCode = Netbios( &Ncb );
   if (uRetCode != 0 && Ncb.ncb_lana_num < 25) goto repeat;
   if ( uRetCode == 0 )
   {
/*
		if (Adapter.adapt.adapter_address[0] == 0 &&
			Adapter.adapt.adapter_address[1] == 0)
		{
               un.buf[0] = Adapter.adapt.adapter_address[2];
               un.buf[1] = Adapter.adapt.adapter_address[3];
               un.buf[2] = Adapter.adapt.adapter_address[4];
               un.buf[3] = Adapter.adapt.adapter_address[5];
		}
		else
		{
               un.buf[0] = Adapter.adapt.adapter_address[0];
               un.buf[1] = Adapter.adapt.adapter_address[1];
               un.buf[2] = Adapter.adapt.adapter_address[2];
               un.buf[3] = Adapter.adapt.adapter_address[3];
		}
	inum = un.ibuf;
*/
		un1.buf[0] = Adapter.adapt.adapter_address[0];
		un1.buf[1] = Adapter.adapt.adapter_address[1];
		un1.buf[2] = Adapter.adapt.adapter_address[2];
		un1.buf[3] = Adapter.adapt.adapter_address[3];

		un2.buf[0] = Adapter.adapt.adapter_address[4];
		un2.buf[1] = Adapter.adapt.adapter_address[5];
		un2.buf[2] = 0;
		un2.buf[3] = 0;
		inum = un1.ibuf + un2.ibuf;
		if (inum == 0 && Ncb.ncb_lana_num < 25) goto repeat;
  }
   else inum = -1;
/*
.....added to check key
*/
	if ((inum==-1)||(inum==0))
		inum = gethost_key();
	*knum = inum;
}
/***************************************************************************
* Function : gethost_key()
*
* Purpose  : Get the maching ID
*
* Inputs   : none
*
* Outputs  : num: Host ID
*
* Returns  : none
****************************************************************************/

#if   defined(__BORLANDC__)
#pragma argsused
#endif

int gethost_key()
{
	char keystring[120];
	int num;
    BYTE password[20] = "mqJTkdevdxEbhyZh";
	SCRIBE_STATUS    status;
	static char nccs[] = "Numerical Control Computer Sciences";
/* 
.....RNBOskInitialize: Initialize the RB_NC_APIPACKET and driver.
*/  
	status = RNBOskFormatPacket( &gSKApiPacket, SCRIBE_APIPACKET_SIZE );
	status = RNBOskInitialize( &gSKApiPacket );
/*
.....pass in password
*/
	status = RNBOskEnable( &gSKApiPacket,
                 password  ,
                (RB_WORD)(strlen(password)));
	if (status!=0) return -1; 
/*
.....read key device
*/   
	read_key(keystring, &num);

	return num;
} 
#endif
/* end of file */


