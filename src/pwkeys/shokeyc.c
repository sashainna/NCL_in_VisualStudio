
/***********************************************************************
** 
**   FILE NAME: shokey.c
**   CONTAINS:
**              main    shokey
** 
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        shokeyc.c , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:13
c
***********************************************************************/
#if !defined VAXVMS
#if !defined WNT

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>
#include <pwd.h>
#include <unistd.h>

#if defined SUN || defined HP
typedef char PTR;
#else
typedef signed char PTR;
#endif

#define MAXKEY 31
#define KEYLEN 16

/***********************************************************************
**
**   SUBROUTINE: main
**
**   FUNCTION:  This is the controlling routine for displaying the
**              active NCLCAM, NCLCADD, and NCLIPV users.
**
**   INPUT:  none.
**
**   OUTPUT: none.
**
***********************************************************************/
main()
{
	int size,shmflg,shmid,*addr,i;
	PTR *ptr;
	key_t lsect[3];
	char hd[3][20];
/*
.....Initialize routine
*/
	lsect[0] = 0x123457;
	lsect[1] = 0x123458;
	lsect[2] = 0x123459;
	strcpy(hd[0],"Active CAM Users");
	strcpy(hd[1],"Active CAD Users");
	strcpy(hd[2],"Active IPV Users");
/*
.....Loop thru NCLCAM, NCLCADD, & NCLIPV global sections
*/
	for (i=0;i<3;i++)
	{
		printf("\n%s\n\n",hd[i]);
/*
........Map to global section
*/
		shmflg = 438;
		size = 512;
		shmid = shmget(lsect[i],size,shmflg);
		if (shmid < 0) goto no_lic;
/*
........Attach global section
*/
		addr = 0;
		shmflg = 0;
		ptr = (PTR *)shmat(shmid,addr,shmflg);
		if ((int)ptr == -1) goto no_lic;
/*
........Display users
*/
		shokey(ptr);
		shmdt(ptr);
no_lic:;
	}
}

/***********************************************************************
**
**   SUBROUTINE: shokey (cptr)
**
**   FUNCTION:  This routine displays the active users for NCLCAM,
**              NCLCADD, and NCLIPV.
**
**   INPUT:  cptr    I*4  D1    -  Pointer to authorization table for
**                                 this program.
**
**   OUTPUT: none.
**
***********************************************************************/
shokey(cptr)
PTR *cptr;
{
	int kpt,nc;
	char obuf[80];
/*
.....Initialize routine
*/
    kpt = 0;
	strcpy(obuf," ");
	nc = 1;
/*
.....Loop through authorization table
*/
	do
	{
/*
........Buffer is full
........Output it
*/
		if (nc+19 > 80)
		{
			printf("%s\n",obuf);
			strcpy(obuf," ");
			nc = 1;
		}
/*
........Check for active user and
........Append to buffer
*/
		if (cptr[kpt] != 0)
		{
			strncat(obuf,&(cptr[kpt]),8);
			strcat(obuf," / ");
			strcat(obuf,&(cptr[kpt+8]));
			strcat(obuf,"   ");
			nc = strlen(obuf);
		}
		kpt += KEYLEN;
	} while (kpt < 512 && cptr[kpt] != -1 && cptr[kpt] != '\377');
/*
.....Output last buffer
*/
	if (nc > 1) printf("%s\n",obuf);
	return;
}
#endif
#endif
