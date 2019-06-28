/***********************************************************************
** 
**   FILE NAME: hostid.c
**   CONTAINS:
**              hostid  pwcall  pwcdea
** 
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        hostid.c , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:10
c
***********************************************************************/
#if !defined WNT && !defined DOS && !defined VAXVMS

#include <sys/types.h>
#include <sys/time.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/resource.h>
#include <errno.h>
#include <pwd.h>
#include <unistd.h>
#ifdef OSF
#include <sys/systeminfo.h>
#endif
#ifdef SGI
#include <sys/prctl.h>
#endif

#define MAXKEY 31
#define KEYLEN 16

#if defined SUN || defined HP
typedef char PTR;
#else
typedef signed char PTR;
#endif

#if !defined IBM && !defined HP
#define hostid hostid_
#define pwcall pwcall_
#define pwcdea pwcdea_
#endif

/***********************************************************************
**
**   SUBROUTINE: hostid (knum)
**
**   FUNCTION:  This routine returns a unique Host Identification
**              Number (HIN) for this computer.  (It is not 100%
**              reliable as it sometimes returns the same number for
**              multiple computers of the same type.  Most notably IBM
**              and the GT series of SGIs).
**
**   INPUT:  none.
**
**   OUTPUT: knum    I*4  D1    -  Host ID for this machine.
**
***********************************************************************/
hostid(knum)

int *knum;
{
#ifdef SGI
	int	inum,iary[8];
	inum = sysid (iary);
	*knum = inum;
#else
#ifdef OSF
	int com=SI_HOSTNAME;
	union
	{
		char buf[80];
		int inum;
	} hid;
	sysinfo(com,hid.buf,sizeof(hid.buf));
	*knum = hid.inum;
#else
	*knum = gethostid();
#endif
#endif
	return;
}

/***********************************************************************
**
**   SUBROUTINE: pwcall (cprog,kerr)
**
**   FUNCTION:  This routine allocates a user for NCLCAM, NCLCADD, or
**              NCLIPV on Unix platforms.  It assumes that the calling
**              program has already verified that it is authorized to
**              run on this computer.
**
**   INPUT:  clprog  C*1  D20   -  Null terminated text string that con-
**                                 tains the name of the calling program.
**
**   OUTPUT: kerr    I*4  D1    -  0 = Success.  1 = Number of authorized
**                                 users exceeded.  2 = License manager
**                                 has not been run.
**
***********************************************************************/
pwcall(cprog,kerr)
int *kerr;
char *cprog;
{
	key_t lsect;
	int size,shmflg,shmid,kpt,ist,nc;
	int *addr = 0;
	char lpid[10],*buf;
	PTR *ptr;
	long pid;
/*
.....Default to no error
*/
	*kerr = 0;
/*
.....Deallocate inactive users
*/
	pwcdea(cprog);
/*
.....Initialize routine
*/
	if (strcmp(cprog,"NCLCAM") == 0) lsect = 0x123457;
	else if (strcmp(cprog,"NCLCADD") == 0) lsect = 0x123458;
	else if (strcmp(cprog,"NCLIPV") == 0) lsect = 0x123459;
	else return;
/*
.....Get process id
*/
	pid = getpid();
	sprintf(lpid,"%8d",pid);
/*
.....Map to global section
*/
	shmflg = 438;
	size = 512;
	shmid = shmget(lsect,size,shmflg);
	if (shmid < 0) goto no_lic;
/*
.....Attach global section
*/
	shmflg = 0;
	ptr = (PTR *) shmat(shmid,addr,shmflg);
	if ((int)ptr == -1) goto no_lic;
/*
.....Check to see if this user
.....has already been authorized
*/
	ist = -1;
    kpt = 0;
	do
	{
		if (ptr[kpt] == 0 && ist == -1) ist = kpt;
		kpt += KEYLEN;
	} while (kpt < 512 && (ptr[kpt] != -1 || ptr[kpt] == '\377'));
/*
.....Maximum number of users exceeded
*/
	if (ist == -1) goto max_user;
	kpt   = ist;
/*
.....Store job process number
*/
    strncpy(&(ptr[kpt]),lpid,8);
/*
.....Store job process name
*/
	buf = cuserid(NULL);
	nc = strlen(buf);
	if (nc > 7) nc = 7;
	strncpy(&(ptr[kpt+8]),buf,8);
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
max_user:;
	*kerr = 1;
	goto done;
/*
.....End of routine
*/
done:;
	shmdt(ptr);
	return;
}

/***********************************************************************
**
**   SUBROUTINE: pwcdea (cprog)
**
**   FUNCTION:  This routine deallocates a user of NCLCAM, NCLCADD, or
**              NCLIPV on Unix platforms.  It also deallocates any users
**              who were abnormally terminated.
**
**   INPUT:  clprog  C*1  D20   -  Null terminated text string that con-
**                                 tains the name of the calling program.
**
**   OUTPUT: none.
**
***********************************************************************/
pwcdea(cprog)
char *cprog;
{
	key_t lsect;
	int size,shmflg,shmid,kpt,ipid,stat,pid;
	int *addr = 0;
	char lpid[10],buf[10];
	PTR *ptr;
/*
.....Initialize routine
*/
	if (strcmp(cprog,"NCLCAM") == 0) lsect = 0x123457;
	else if (strcmp(cprog,"NCLCADD") == 0) lsect = 0x123458;
	else if (strcmp(cprog,"NCLIPV") == 0) lsect = 0x123459;
	else return;
/*
.....Map to global section
*/
	shmflg = 438;
	size = 512;
	shmid = shmget(lsect,size,shmflg);
	if (shmid < 0) goto done;
/*
.....Attach global section
*/
	shmflg = 0;
	ptr = (PTR *)shmat(shmid,addr,shmflg);
	if ((int)ptr == -1) goto done;
/*
.....Get process id
*/
	pid = getpid();
	sprintf(lpid,"%8d",pid);
/*
.....Check to see if this user
.....has already been authorized
*/
    kpt = 0;
	do
	{
		strncpy(buf,&(ptr[kpt]),8);
		buf[8] = '\0';
		if (strncmp(buf,lpid,8) == 0)
		{
			ptr[kpt] = 0;
		}
/*
........Deallocate inactive user
*/
		sscanf(buf,"%d",&ipid);
/*
.....IRIX V5.2 does not let us use 'getpriority'
.....Bobby  -  10/7/94
*/
#ifdef SGI
		stat = prctl(PR_ISBLOCKED,ipid);
#else
		stat = getpriority(PRIO_PROCESS,ipid);
#endif
#ifdef SUN
		if (stat == -1 && (errno == ESRCH || errno == EINVAL)) ptr[kpt] = 0;
#else
		if (stat == -1 && errno == ESRCH) ptr[kpt] = 0;
#endif
		kpt += KEYLEN;
	} while (kpt < 512 && (ptr[kpt] != -1 && ptr[kpt] != '\377'));
/*
.....End of routine
*/
done:;
	shmdt(ptr);
	return;
}
#endif
