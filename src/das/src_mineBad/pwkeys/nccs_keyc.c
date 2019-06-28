#if !defined WNT
/***********************************************************************
** 
**   FILE NAME: nccs_key.c
**   CONTAINS:
**              main  chkflg  clrflg  defsec
** 
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        nccs_keyc.c , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:12
c
***********************************************************************/
#if !defined VAXVMS

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

#define MAXKEY 31
#define KEYLEN 16

#if defined SUN || defined HP
typedef char PTR;
#else
typedef signed char PTR;
#endif

#if !defined IBM & !defined HP
#define pwdaut pwdaut_
#define pwdusr pwdusr_
#endif

#ifdef OSF
#define main MAIN__
#endif

PTR *ptr;

/***********************************************************************
**
**   SUBROUTINE: main
**
**   FUNCTION:  This is the main routine for initializing the NCCS
**              License Manager for NCLCAM, NCLCADD, and NCLIPV.
**
**   INPUT:  none.
**
**   OUTPUT: none.
**
***********************************************************************/
main()
{

	int iopt[8],ierr1,ierr2,ierr3,ncam,ncad,nipv;
	char msg1[80],msg2[80],msg3[80];
	char *nclcam = "NCLCAM";
	char *nclcad = "NCLCADD";
	char *nclipv = "NCLIPV";
	char *ver = "01-JAN-1990";
	
	key_t key;
	int size,shmflg,shmid;
/*
.....Load CAM license
*/
	pwdaut(nclcam,nclcam,ver,iopt,msg1,&ierr1);
	if (ierr1 != 0)
	{
		ncam = 0;
	}
	else
	{
		msg1[0] = '\0';
		pwdusr (&ncam);
		if (ncam > MAXKEY) ncam = 0;
	}
/*
.....Define NCLCAM license manager
.....global section
*/
	defsec(nclcam,ncam,msg1,&ierr1);
/*
.....Load CADD license
*/
	pwdaut(nclcad,nclcad,ver,iopt,msg2,&ierr2);
	if (ierr2 != 0)
	{
		ncad = 0;
	}
	else
	{
		msg2[0] = '\0';
		pwdusr (&ncad);
		if (ncad > MAXKEY) ncad = 0;
	}
/*
.....Define NCLCADD license manager
.....global section
*/
	defsec(nclcad,ncad,msg2,&ierr2);
/*
.....Load NCLIPV license
*/
	pwdaut(nclipv,nclipv,ver,iopt,msg3,&ierr3);
	if (ierr3 != 0)
	{
		nipv = 0;
	}
	else
	{
		msg3[0] = '\0';
		pwdusr (&nipv);
		if (nipv > MAXKEY) nipv = 0;
	}
/*
.....Define NCLIPV license manager
.....global section
*/
	defsec(nclipv,nipv,msg3,&ierr3);
/*
.....Make sure at least 1 license loaded
*/
	if (ierr1 != 0 && ierr2 != 0 && ierr3 != 0)
	{
		printf("An error occurred trying to load the License Manager.\n");
		printf("%s\n",msg1);
	}
/*
.....Could not load NCLCAM license
*/
	else if (ierr1 != 0)
	{
		printf("Could not load NCLCAM license.\n%s\n",msg1);
	}
/*
.....Could not load NCLCADD license
*/
	else if (ierr2 != 0)
	{
		printf("Could not load NCLCADD license.\n%s\n",msg2);
	}
/*
.....Could not load NCLIPV license
*/
	else if (ierr3 != 0)
	{
		printf("Could not load NCLIPV license.\n%s\n",msg3);
	}
}

/***********************************************************************
**
**   SUBROUTINE: chkflg (kerr)
**
**   FUNCTION:  This routine checks to see if there is any users
**              currently running NCLCAM, NCLCADD, or NCLIPV.
**
**   INPUT:  none.
**
**   OUTPUT: kerr    I*4  D1    -  0 = Success.  1 = Program is
**                                 currently active.
**
***********************************************************************/
chkflg(kerr)
int *kerr;
{
	int i;
/*
.....Check to see if licensed product
.....is currently active
*/
	*kerr = 0;
	for (i=0;i<MAXKEY;i++)
	{
		if (ptr[i*KEYLEN] == -1 || ptr[i*KEYLEN] == '\377') goto done;
		if (ptr[i*KEYLEN] > 0) goto failed;
	}
	goto done;
/*
.....Software is currently active
*/
failed:;
	*kerr = 1;
/*
.....End of routine
*/
done:;
	return;
}

/***********************************************************************
**
**   SUBROUTINE: clrflg (kusr)
**
**   FUNCTION:  This routine clears out all available user slots for
**              NCLCAM, NCLCADD, and NCLIPV.
**
**   INPUT:  kusr    I*4  D1    -  Number of users authorized to run
**                                 this program.
**
**   OUTPUT: none.
**
***********************************************************************/
clrflg(kusr)
int kusr;
{
	int i;
/*
.....Clear out slots
*/
	for (i=0;i<MAXKEY;i++)
	{
		ptr[i*KEYLEN] = 0;
	}
	if (kusr != MAXKEY) ptr[kusr*KEYLEN] = (PTR)-1;
	ptr[505] = 8;
/*
.....End of routine
*/
done:;
	return;
}

/***********************************************************************
**
**   SUBROUTINE: defsec (cprog,kusr,cmsg,kerr)
**
**   FUNCTION:  This routine creates the authorization tables for
**              NCLCAM, NCLCADD, and NCLIPV.
**
**   INPUT:  cprog   C*1  D20   -  Null terminated text string that con-
**                                 tains the name of the program to
**                                 create the table for.
**
**           kusr    I*4  D1    -  Number of users to create table for.
**
**   OUTPUT: cmsg    C*1  D80   -  Text of message when an error occurs.
**
**           kerr    I*4  D1    -  0 = Success.  1 = An error occured
**                                 trying to create the authorization
**                                 table.
**
***********************************************************************/
defsec(cprog,kusr,cmsg,kerr)
char *cprog,*cmsg;
int kusr,*kerr;
{
	int lsect;
	int *addr,shmid,shmflg,size;
/*
.....Initialize routine
*/
	if (kusr == 0) goto failed;
	*kerr = 0;
	if (strcmp(cprog,"NCLCAM") == 0) lsect = 0x123457;
	else if (strcmp(cprog,"NCLCADD") == 0) lsect = 0x123458;
	else if (strcmp(cprog,"NCLIPV") == 0) lsect = 0x123459;
	else goto failed;
/*
.....Try to map to global section
*/
	shmflg = 438;	/* Allow everybody Read/Write access */
	size = 512;
	shmid = shmget(lsect,size,shmflg);
/*
.....No such global section
.....Let's create it
*/
/*	if (shmid < 0 && errno == ENOENT)*/
	if (shmid < 0)
	{
		shmflg = IPC_CREAT | shmflg;
		shmid = shmget(lsect,size,shmflg);
		if (shmid < 0) goto failed;
/*
........Attach to global section
*/
		addr = 0;
		shmflg = 0;
		ptr = (PTR *)shmat(shmid,addr,shmflg);
		if ((int)ptr == -1) goto failed;
	}
/*
.....Map succeeded
.....Check for active NCL
*/
	else
	{
/*
........Attach to global section
*/
		addr = 0;
		shmflg = 0;
		ptr = (PTR *)shmat(shmid,addr,shmflg);
		if ((int)ptr == -1) goto failed;
		chkflg(kerr);
		if (*kerr != 0)
		{
			strcpy(cmsg,"*FATAL* You may not reload the License Manager while NCL is active.");
			goto done;
		}
	}
/*
.....Clear out users
*/
	clrflg(kusr);
/*
.....Detach global section
*/
	shmdt(ptr);
	goto done;
/*
.....Error trying to allocate memory
*/
failed:;
	*kerr = 1;
	sprintf(cmsg,"*FATAL* Could not load license manager.  Error = %d",errno);
/*
.....End of routine
*/
done:;
	return;
}
#endif
#endif
