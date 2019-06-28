/************************************************************************
c
c   FILE NAME: Hostid_nt.C
c   CONTAINS:
c             hostid
c             hostid_ck
c             pwcall
c             pwcdea
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			hostid_nt.c , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        01/06/14 , 11:24:32
c
c**********************************************************************
*/
#ifdef WNT

#include <windows.h>               /* for all Window apps                  */
#include <winuser.h>
#include "IPHlpApi.h"
#include <stdio.h>
#include <time.h>
#include <sys/timeb.h>
#include "scribe_nt.h"
#include "spromeps.h"

RB_SCRIBE_APIPACKET gSKApiPacket;	/* Scribe packet */
RB_SPRO_APIPACKET gSproApiPacket;	/* Scribe packet */

#include <nb30.h>

typedef struct _ASTAT_
{
	ADAPTER_STATUS adapt;
	NAME_BUFFER    NameBuff [30];
}ASTAT, * PASTAT;

#define USBKEY 0
#define PARKEY 1
#define NETKEY 2

#define NPROG 10

VOID CALLBACK host_heartbeat (hwnd,uMsg,idEvent,dwTime);

static int gDevType = USBKEY;
static RB_WORD gDevId = 0x9DE7;
static RB_WORD gPasswd = 0xE350;
static RB_BYTE WriteAccess = 0;
static RB_BYTE ReadAccess = 1;
static BOOL KeyInit = FALSE, SysAbort = FALSE;
static UINT SysTimer = 0;
static int sysid=0;
static int sysid_flag[NPROG]={0,0,0,0,0,0,0,0,0,0};
#define BASELIC 45
static char *Sprog[NPROG] = {"DUMMY","NCLCAM","NCLCADD","NCLIPV","SIMULATE",
	"IGES","SOLIDWORKS","MAKEPOST","POSTWORKS","PTED"};
static int Salloc[NPROG] = {0,0,0,0,0,0,0,0,0,0};

ASTAT Adapter;

union pw
{
	UCHAR buf[4];
	unsigned int ibuf;
};

/***********************************************************************
**   E-FUNCTION: hostid (cprog,knum,kflag)
**      This routine returns the host id value of the computer.
**
**   INPUT:
**      cprog   =  Name of program to get host id for.
**      kflag   =  0 = Get ethernet card number.  1 = Get local dongle ID,
**                 2 = Get floating dongle ID.
**
**   OUTPUT:
**      knum    =  Host ID number.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS:  none
***********************************************************************/
int hostid(cprog,knum,kflag)
char *cprog;
int *knum, *kflag;
{
   NCB Ncb;
   UCHAR uRetCode;
   int isav,inum,iprog;
	union pw un1;
	union pw un2;
/*char sbuf[80];*/
/*
.....Get program index
*/
	iprog = S_get_prog(cprog);
/*
sprintf(sbuf,"H - %s [%d] = %d",cprog,iprog,*kflag);
NclxDbgPstr(sbuf);
*/
/*
.....Get System ID from Dongle
*/
	if (*kflag != 0)
	{
		isav = sysid_flag[iprog];
		sysid_flag[iprog] = *kflag;
		if (strcmp(cprog,"POSTWORKS") == 0)
		{
			inum = S_get_prog("MAKEPOST");
			sysid_flag[inum] = *kflag;
			inum = S_get_prog("PTED");
			sysid_flag[inum] = isav;
		}
		sysid = gethost_key(*kflag,FALSE);
		*knum = sysid;
		if (sysid == -1) sysid_flag[iprog] = isav;
		return 0;
	}
/*
.....Windows 2000 method
.....for obtaining System ID
*/
	id2000(&inum);
	*knum = inum;
	if (*knum != 0) return 0;
/*
.....Window NT method
.....for obtaining System ID
*/
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
	{
		inum = gethost_key(*kflag,FALSE);
		if (inum != -1 && inum != 0) sysid_flag[iprog] = 1;
	}
	*knum = inum;
	return 0;
}

/***********************************************************************
**   E-FUNCTION: hostid_ck ()
**      This routine returns the host id value of the computer.
**
**   INPUT: none.
**   OUTPUT: none.
**   RETURNS: 0 if the system ID is the same, 1 if it is not.
**   SIDE EFFECTS: none
**   WARNINGS:  none
***********************************************************************/
int hostid_ck()
{
/*
.....Key number is checked by heartbeat routine
*/
	if (SysAbort) return 0;
	else return 1;
}

/***********************************************************************
**   E-FUNCTION: pwcall (cprog,kerr)
**      This routine allocates a user for NCLCAM, NCLCADD, or NCLIPV on
**      Windows NT platforms.  It assumes that the calling program has
**      already verified that it is authorized to run on this computer.
**
**   INPUT:
**      cprog   =  Null terminated text string that contains the name of
**                 the calling program.
**
**   OUTPUT:
**      kerr    =  0 = Success.  1 = Number of authorized users exceeded.
**                 2 = License manager has not been run.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS:  none
***********************************************************************/
void pwcall(cprog,kerr)
char *cprog;
int *kerr;
{
	int status,i,inc;
	RB_WORD rCell;
/*char sbuf[80];*/
/*
.....Only allocate users for NetWork Licensing
.....The heartbeat is still used for
.....any kind of hardware key
*/
	*kerr = 0;
	inc = S_get_prog(cprog);
/*
sprintf(sbuf,"A - %s [%d] = %d",cprog,inc,sysid_flag[inc]);
NclxDbgPstr(sbuf);
*/
	if (sysid_flag[inc] == 0) goto done;
/*
.....Find the program to allocate
*/
	if (sysid_flag[inc] == 2)
	{
		if (!KeyInit) gethost_key(sysid_flag[inc],TRUE);
/*
.....Allocate a sub-license
*/
		rCell = BASELIC + inc - 1;
		status = RNBOsproGetSubLicense(&gSproApiPacket, rCell);
		if (status != SP_SUCCESS) goto max_user;
		Salloc[inc] = 1;
	}
/*
.....Setup a heartbeat message
*/
	SysAbort = FALSE;
	if (SysTimer == 0)
		SysTimer = SetTimer(NULL,1,80000,(TIMERPROC)host_heartbeat);
	goto done;
/*
.....License manager not loaded
*/
no_lic:;
	*kerr = 2;
	goto done;
/*
.....Exceeded number of users
*/
max_user:
	*kerr = 1;
	goto done;
/*
.....End of routine
*/
done:;
	return;
}

/***********************************************************************
**   E-FUNCTION: pwcdea (cprog)
**      This routine deallocates a user on Windows NT systems.
**
**   INPUT:
**      cprog   =  Null terminated text string that contains the name of
**                 the calling program.  A blank string will deallocate
**                 all licenses.
**
**   OUTPUT: none
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS:  none
***********************************************************************/
void pwcdea(cprog)
char *cprog;
{
	int i,status,inc;
	RB_WORD nlic,rCell;
/*char sbuf[80];*/
/*
.....Check for dongle or floating licenses
*/
/*
sprintf(sbuf,"D - %s",cprog);
NclxDbgPstr(sbuf);
*/
	if (cprog[0] == '\0')
	{
		inc = 0;
		for (i=0;i<NPROG;i++) if (sysid_flag[i] == 2) inc = i;
	}
/*
.....Only deallocate users for NetWork Licensing
*/
	else
	{
		inc = S_get_prog(cprog);
		if (sysid_flag[inc] == 0) goto done;
	}
/*
.....Find the program to deallocate
........Zeroing out 'sysid_flag' can cause
........timing problems where the license
........will no longer be deemed 'floating'
........Previous versions did not zero out
........the 'sysid_flag'
*/
/*
sprintf(sbuf,"D%d - %d",inc,sysid_flag[inc]);
NclxDbgPstr(sbuf);
*/
	if (sysid_flag[inc] == 2)
	{
		if (cprog[0] == '\0')
		{
			rCell = 0;
			KeyInit = FALSE;
			for (i=0;i<NPROG;i++)
			{
				Salloc[i] = 0;
//				sysid_flag[i] = 0;
			}
		}
		else
		{
			rCell = BASELIC + inc - 1;
			Salloc[inc] = 0;
//			sysid_flag[inc] = 0;
		}
/*
.....Deallocate a sub-license
*/
		nlic = 1;
		status = RNBOsproReleaseLicense(&gSproApiPacket, rCell, &nlic);
	}
/*
.....Deallocate the timer
*/
	if (rCell == 0 && SysTimer != 0)
	{
		KillTimer(NULL,SysTimer);
		SysTimer = 0;
	}
/*
.....End of routine
*/
done:;
	return;
}

/***********************************************************************
**   I-FUNCTION: gethost_key (flag,kalloc)
**      This routine returns the dongle key value.
**
**   INPUT:
**       flag   =  0 = Get ethernet card number.  1 = Get local dongle ID,
**                 2 = Get floating dongle ID.
**
**       kalloc =  TRUE = This call is to setup a user to be allocated.
**                 FALSE = The calling program does not allocate users.
**
**   OUTPUT:
**      knum    =  Host ID number.
**
**   RETURNS: 0 on success, -1 if an error occured reading the key.
**   SIDE EFFECTS: none
**   WARNINGS:  none
***********************************************************************/
#if   defined(__BORLANDC__)
#pragma argsused
#endif

static int gethost_key(flag,kalloc)
int flag,kalloc;
{
	char keystring[120];
	int num,init;
    BYTE password[20] = "mqJTkdevdxEbhyZh";
	SCRIBE_STATUS    status;
	static char nccs[] = "Numerical Control Computer Sciences";
	static int last_flag = -1;
   RB_WORD rCell,nlic;
/* 
.....RNBOskInitialize: Initialize the RB_NC_APIPACKET and driver.
*/  
	if (flag != last_flag) KeyInit = FALSE;
	init = KeyInit;
	if (!KeyInit)
	{
		status = RNBOsproFormatPacket( &gSproApiPacket, SCRIBE_APIPACKET_SIZE );
		status = RNBOsproInitialize( &gSproApiPacket );
		if (flag != 2)
			status = RNBOsproSetContactServer(&gSproApiPacket,"no-net");
		status = RNBOsproFindFirstUnit(&gSproApiPacket,gDevId);
		gDevType = USBKEY;
		if (status != SP_SUCCESS && flag == 2) return -1;
		KeyInit = TRUE;
		last_flag = flag;
	}
	else
		status = SP_SUCCESS;
/*
.....USB device not found
.....Initialize the parallel device
*/
	if (status != SP_SUCCESS) return -1;
/*
	{
		status = RNBOskFormatPacket( &gSKApiPacket, SCRIBE_APIPACKET_SIZE );
		status = RNBOskInitialize( &gSKApiPacket );
		gDevType = PARKEY;
*/
/*
.....pass in password
*/
/*
		status = RNBOskEnable( &gSKApiPacket,
   	              password  ,
   	             (RB_WORD)(strlen(password)));
		if (status!=0) return -1; 
	}
*/
/*
.....read key device
*/   
	read_key(keystring, &num, flag);
	if (flag == 2 && !init && !kalloc)
	{
		rCell = 0;
		nlic = 1;
		status = RNBOsproReleaseLicense(&gSproApiPacket, rCell, &nlic);
		KeyInit = FALSE;
	}
	return num;
} 

/***********************************************************************
**   I-FUNCTION: id2000 (knum)
**      This routine returns the host id value of the computer running
**      Windows 2000 or later operating system.
**
**   INPUT: none
**   OUTPUT:
**      knum    =  Host ID number.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS:  none
***********************************************************************/
static int id2000(knum)
int *knum;
{
	DWORD Err;
	PIP_ADAPTER_INFO pAdapterInfo, pAdapt;
	DWORD AdapterInfoSize;
	union pw un1;
	union pw un2;
/*
.....Enumerate all of the adapter specific information
.....using the IP_ADAPTER_INFO structure.
.....Note:  IP_ADAPTER_INFO contains a linked list of adapter entries.
*/
	*knum = 0;
	AdapterInfoSize = 0;
	if ((Err = GetAdaptersInfo(NULL, &AdapterInfoSize)) != 0)
	{
		if (Err != ERROR_BUFFER_OVERFLOW)
		{
//printf("AdapterInfoSize = %d\n",Err);
			return 0;
		}
	}
/*
.....Allocate memory from sizing information
*/
	pAdapterInfo = (PIP_ADAPTER_INFO) GlobalAlloc(GPTR, AdapterInfoSize);
	if (pAdapterInfo == NULL)
	{
//printf("pAdapterInfo = %d\n",Err);
		return 0;
	}
/*
.....Get actual adapter information
*/
	if ((Err = GetAdaptersInfo(pAdapterInfo, &AdapterInfoSize)) != 0)
	{
//printf("GetAdaptersInfo = %d\n",Err);
		goto done;
	}
/*
.....Loop through all adapters
*/
	pAdapt = pAdapterInfo;
	while (pAdapt)
	{
		switch (pAdapt->Type)
		{
		case MIB_IF_TYPE_ETHERNET:
			un1.buf[0] = pAdapt->Address[0];
			un1.buf[1] = pAdapt->Address[1];
			un1.buf[2] = pAdapt->Address[2];
			un1.buf[3] = pAdapt->Address[3];

			un2.buf[0] = pAdapt->Address[4];
			un2.buf[1] = pAdapt->Address[5];
			un2.buf[2] = 0;
			un2.buf[3] = 0;
			*knum = un1.ibuf + un2.ibuf;
			if (*knum != 0) goto done;
			break;
		case MIB_IF_TYPE_TOKENRING:
		case MIB_IF_TYPE_FDDI:
		case MIB_IF_TYPE_PPP:
		case MIB_IF_TYPE_LOOPBACK:
		case MIB_IF_TYPE_SLIP:
		case MIB_IF_TYPE_OTHER:
		default:;
		}
		pAdapt = pAdapt->Next;
	}
/*
.....End of routine
*/
done:;
	GlobalFree((HGLOBAL)pAdapterInfo);
//printf("ID = %d\n",*knum);
	return 0;
}

/***********************************************************************
**   I-FUNCTION: host_heartbeat (hwnd,uMsg,idEvent,dwTime)
**      This routine allocates a user for NCLCAM, NCLCADD, or NCLIPV on
**      Windows NT platforms.  It assumes that the calling program has
**      already verified that it is authorized to run on this computer.
**
**   INPUT:
**      (operating system parameters are ignored).
**   OUTPUT: none
**
**   RETURNS: none
**   SIDE EFFECTS:
**      Sets the static variable SysAbort if a connection cannot be made
**      to the dongle.
**   WARNINGS:  none
***********************************************************************/
VOID CALLBACK host_heartbeat (hwnd,uMsg,idEvent,dwTime)
HWND hwnd;
UINT uMsg;
UINT idEvent;
DWORD dwTime;
{
	int isav,inum,i,ierr,inc1,inc2,ssav;
/*char sbuf[80];*/
/*
.....Check for dongle or floating licenses
*/
	inc1 = 0;
	inc2 = 0;
	for (i=0;i<NPROG;i++)
	{
/*
sprintf(sbuf,"SI - %d",sysid_flag[i]);
NclxDbgPstr(sbuf);
*/
		if (sysid_flag[i] == 1) inc1 = i;
		else if (sysid_flag[i] == 2)
		{
			inc2 = i;
			break;
		}
	}
	if (inc2 == 0) inc2 = inc1;
/*
.....Get the system ID
*/
	isav = sysid;
	ssav = sysid_flag[inc2];
	hostid(Sprog[inc2],&inum,&sysid_flag[inc2]);
	sysid = isav;
	sysid_flag[inc2] = ssav;
/*
.....Seem to have lost our licenses
.....Let's try to get them back
.....(Process may have been sleeping, for example, *EDT can do this)
*/
/*
sprintf(sbuf,"%s - %d",Sprog[inc2],inc2);
NclxDbgPstr(sbuf);
sprintf(sbuf,"%d - %d",sysid,inum);
NclxDbgPstr(sbuf);
*/
	if (inum != sysid)
	{
		if (sysid_flag[inc2] == 2)
		{
			KeyInit = FALSE;
			for (i=0;i<NPROG;i++)
			{
				if (Salloc[i] == 1)
				{
					pwcall(Sprog[i],&ierr);
/*
sprintf(sbuf,"%s - F%d",Sprog[i],ierr);
NclxDbgPstr(sbuf);
*/
					if (ierr != 0) goto failed;
				}
			}
		}
		else goto failed;
	}
/*
.....End of routine
*/
done:;
/*	else SysTimer = SetTimer(NULL,1,80000,(TIMERPROC)host_heartbeat);*/
	return;
/*
.....Could not access dongle
*/
failed:;
	SysAbort = TRUE;
	goto done;
}

/****************************************************************
c
c   I-FUNCTION:  read_key (key,num,flag)
c     This utility is used to read the SCRIBE key device
c  PARAMETERS
c	    INPUT  :
c        flag   = Type of key to read, PARKEY = Parallel key,
c                 USBKEY = Local USB key, NETKEY = Network USB floating
c                 key.
c	    OUTPUT :
C		   key    = Blank string (not used at this time).
c        num    = Host id number.
c	RETURNS: none
c  SIDE EFFECTS: none
c  WARNINGS:  none
c****************************************************************/
static int read_key(char *key, int *num, int flag)
{
   static RB_WORD      rCell;
   static RB_WORD      rData;
   int i,ist,idat,ndat;
   BOOL status;
   union KEYUNION 
   {
	   RB_WORD sint[2];
		int num;
	   char chstr[4];
   } key_union;
/*
.....Initialize routine
*/
	key[0] = '\0';
	*num = 0;
/*
.....Get the date written
*/
	if (gDevType == USBKEY)
	{
	   rCell = (flag == NETKEY) ? 28 : 56;
   	status = read_cell(rCell,&key_union.sint[0]);
   	if (status != SP_SUCCESS) goto done;
   	rCell++;
   	status = read_cell(rCell,&key_union.sint[1]);
   	if (status != SP_SUCCESS) goto done;
		ndat = key_union.num;
	}
/*
.....Get the System ID
*/
	if (gDevType == PARKEY) rCell = 58;
	else rCell = (flag == NETKEY) ? 30 : 58;
  	status = read_cell(rCell,&key_union.sint[0]);
  	if (status != SP_SUCCESS) goto done;
  	rCell++;
  	status = read_cell(rCell,&key_union.sint[1]);
  	if (status != SP_SUCCESS) goto done;
	*num = key_union.num;
/*
.....Make sure date was not changed back
*/
	if (gDevType == USBKEY && flag != NETKEY)
	{
		pwddnm(&idat);
		if (idat < ndat)
		{
			key_union.num = *num;
			key_union.num = key_union.num * -1.;
			rCell = 58; rData = key_union.sint[0];
			status = RNBOsproWrite(&gSproApiPacket,gPasswd,rCell,rData,ReadAccess);
			if (status != SP_SUCCESS)
				RNBOsproWrite(&gSproApiPacket,gPasswd,rCell,rData,ReadAccess);
			rCell = 59; rData = key_union.sint[1];
			status = RNBOsproWrite(&gSproApiPacket,gPasswd,rCell,rData,ReadAccess);
			if (status != SP_SUCCESS)
				RNBOsproWrite(&gSproApiPacket,gPasswd,rCell,rData,ReadAccess);
		}
		else
		{
			key_union.num = idat;
			rCell = 56; rData = key_union.sint[0];
			status = RNBOsproWrite(&gSproApiPacket,gPasswd,rCell,rData,
				WriteAccess);
			if (status != SP_SUCCESS)
				RNBOsproWrite(&gSproApiPacket,gPasswd,rCell,rData,WriteAccess);
			rCell = 57; rData = key_union.sint[1];
			status = RNBOsproWrite(&gSproApiPacket,gPasswd,rCell,rData,
				WriteAccess);
			if (status != SP_SUCCESS)
				RNBOsproWrite(&gSproApiPacket,gPasswd,rCell,rData,WriteAccess);
		}
		
	}
done:;
	return 1;
}

/****************************************************************
c
c  I-FUNCTION:  read_cell (rCell,rData)
c     Reads an individual cell from the SCRIBE key device.
c  PARAMETERS
c	    INPUT  :
c        rCell  = The cell to read data from.
c	    OUTPUT :
C		   rData    = Integer word read.
c	RETURNS: none
c  SIDE EFFECTS: none
c  WARNINGS:  none
c****************************************************************/
static int read_cell (rCell,rData)
RB_WORD rCell;
RB_WORD *rData;
{
	int status;
	if (gDevType == USBKEY)
	{
		status = RNBOsproRead( &gSproApiPacket, rCell, rData );
		if (status != 0)
			status = RNBOsproRead( &gSproApiPacket, rCell, rData );
	}
	else
	{
		status = -1;
/*		status = RNBOskRead( &gSKApiPacket, rCell, rData );
		if (status != 0)
			status = RNBOskRead( &gSKApiPacket, rCell, rData );*/
	}
	return(status);
}

/****************************************************************
c
c  I-FUNCTION:  S_get_prog (cprog)
c     Matches the provided program name with the list of available
c     licensed programs that count users.
c  PARAMETERS
c	    INPUT  :
c        cprog  = Program to find in the list of available programs.
c	    OUTPUT :
C		   none
c	RETURNS: Index of specified program.  -1 if program is not found.
c  SIDE EFFECTS: none
c  WARNINGS:  none
c****************************************************************/
static int S_get_prog (cprog)
char *cprog;
{
	int inc;
/*
.....Determine if program is authorized for usage count
.....If so, then find its index in the array of available programs
*/
	for (inc=0;inc<NPROG;inc++)
	{
		if (strcmp(cprog,Sprog[inc]) == 0) break;
	}
	if (inc >= NPROG) inc = 0;
	return(inc);
}
#endif
