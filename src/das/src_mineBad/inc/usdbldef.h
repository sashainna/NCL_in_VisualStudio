/*********************************************************************
**    NAME         :  usdbldef.h -- system wide defines.
**			This version make all reals "double"
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       usdbldef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:05
*********************************************************************/

#ifndef USDBLDEFH

/* define the operating systems */
#define UU_B42 0
#define UU_SYS5 1
#define UU_VMS 2
#define UU_OTHERUNIX 3
#define UU_XENIX 4
#define UU_ULTRIX 5
#define UU_SYS53 6
#define UU_ALPHAVMS 7
#define UU_AIX64 8

/* define the machines we have */
#define UU_RIDGE 0
#define UU_APOLLO 1
#define UU_SUN 2
#define UU_SUN2 3
#define UU_IRIS 4
#define UU_PYRAMID 5
#define UU_VAXVMS 6
#define UU_IBMAT 7
#define UU_TEK6130 8
#define UU_MASSCOMP 9
#define UU_VAXULTRIX 10
#define UU_IRIS4D 11
#define UU_TEK 12
#define UU_CIM 13
/* MILLS: to resolve undefined on VMS */
#define UU_386 14
#define UU_DECUNIX 16
#define UU_HPUX 17
#define UU_WINNT 18
#define UU_WIN2K 19

/*
** define specific sun machines.
** the symbol UU_SUNTYPE should be set to one
** of these values
*/
#define UU_SUN_SUN2 2
#define UU_SUN_SUN3 3
#define UU_SUN_SUN4 4

/* MILLS: to resolve undefined on VMS */
#if UU_COMP!=UU_SUN
#define UU_SUNTYPE 0
#endif

/* define the cpu's */
#define UU_M68000 0
#define UU_I8086 1
#define UU_I80286 2

#define UU_OPENGL 

/* now define UU_OPSYS to be the correct operating system for this machine,
	and define UU_CPU to be the correct cpu for this machine.
	If some version of unix, define the symbol UU_UNIX */
#if UU_COMP==UU_SUN
#define UU_OPSYS UU_SYS5
#define UU_UNIX
#define UU_CPU UU_M68000
#endif
#if UU_COMP==UU_TEK
#define UU_OPSYS UU_B42
#define UU_UNIX
#define UU_CPU UU_M68000
#endif
#if UU_COMP==UU_PYRAMID
#define UU_OPSYS UU_B42
#define UU_UNIX
#endif
#if UU_COMP==UU_VAXULTRIX
#define UU_OPSYS UU_ULTRIX
#define UU_UNIX
#endif
#if UU_COMP==UU_IRIS
#define UU_OPSYS UU_SYS5
#define UU_UNIX
#define UU_CPU UU_M68000
#endif
#if UU_COMP==UU_IRIS4D
/* Changed for port to IRIX 5.2 */
#define UU_OPSYS UU_B42
#define UU_UNIX
#define UU_CPU UU_M68000
#endif
#if UU_COMP==UU_RIDGE
#define UU_OPSYS UU_SYS5
#define UU_UNIX
#endif
#if UU_COMP==UU_APOLLO
#define UU_OPSYS UU_OTHERUNIX
#define UU_UNIX
#define UU_CPU UU_M68000
#endif
#if UU_COMP==UU_VAXVMS
#ifdef UU_ALPHA
#define UU_OPSYS UU_ALPHAVMS
#else
#define UU_OPSYS UU_VMS
#endif
#include <ssdef.h>
#define UU_NORMAL 	SS$_NORMAL
#define UU_ABORT		SS$_ABORT
#else
#define UU_NORMAL		0
#define UU_ABORT		1
#endif
#if UU_COMP==UU_IBMAT
#define UU_OPSYS UU_XENIX
#define UU_CPU UU_I80286
#define UU_UNIX
#endif
#if UU_COMP==UU_TEK6130
#define UU_OPSYS UU_B42
#define UU_UNIX
#endif
#if UU_COMP==UU_MASSCOMP
#define UU_OPSYS UU_SYS5
#define UU_UNIX
#endif
#if UU_COMP==UU_CIM
#define UU_OPSYS UU_B42
#define UU_UNIX
#define UU_CPU UU_M68000
#endif
#if UU_COMP==UU_DECUNIX
#define UU_OPSYS UU_B42
#define UU_UNIX
#define UU_CPU UU_M68000
#endif
#if UU_COMP==UU_HPUX
#define UU_OPSYS UU_SYS5
#define UU_UNIX
#endif
#if UU_COMP==UU_WINNT
#define UU_OPSYS UU_SYS5
#define UU_UNIX
#endif

#if UU_COMP==UU_WIN2K
#define UU_OPSYS UU_SYS5
#define UU_UNIX
#endif


#define UU_LOGICAL int
#if UU_COMP==UU_IRIS
#define UU_REAL long float
#define UU_TRUEDOUBLE long float
#else
#define UU_REAL double
#define UU_TRUEDOUBLE double
#endif
#define UU_LONG int
#define UU_ENTID unsigned int
#define UU_ALTACTION 2
#define UU_TRUE 1
#define UU_FALSE 0
#if UU_COMP == UU_WIN2K || UU_COMP == UU_IRIS4D
#define UU_NULL (void *)0
#else
#define UU_NULL 0
#endif
#define UU_SYSDEF
#define UU_DBLDEF
#define UU_KEY_ID unsigned UU_LONG
#define UU_REL_ID unsigned UU_LONG

#define UU_BITPW	32	/* bits per word		*/
/* remove following if UU_BITPW not a power of 2 */
#define UU_P2BITPW
#define UU_BPWSHFT 5	/* bitmap shift count */
#define UU_BPWMASK 31	/* bitmap mask */
#define UU_BYTEPW	4	/* bytes per word		*/

#define UU_INTERRUPT -2
#define UU_FAILURE -1
#define UU_SUCCESS 0			/* function return flag for success */

#define USDBLDEFH
#endif
