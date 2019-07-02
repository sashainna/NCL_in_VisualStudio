/********************************************************************* 
**  NAME:  umasks.h
**
**      Contains definitions common to both udebon.h and udeboff.h
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       umasks.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:05
**
*********************************************************************/

#ifndef UMASKSH

#define UU_GTRC 0x1						/* GKS Subsystem */
#define UU_DTRC 0x2						/* Data Acquisition Subsystem */
#define UU_MTRC 0x4						/* Geometric Modeling Subsystem */
#define UU_XTRC 0x8						/* Disk I/O Subsystem */
#define UU_RTRC 0x10						/* RDBMS Subsystem */
#define UU_STRC 0x20						/* SAL Subsystem */
#define UU_KTRC 0x40						/* Kernal Operating Subsystem */
#define UU_ITRC 0x80						/* User Interface Subsystem */
#define UU_NTRC 0x100               /* MATH Subsystem */
#define UU_GITRC 0x200					/* GKS internal */
#define UU_DITRC 0x400					/* Data Acquisition Subsystem */
#define UU_MITRC 0x800					/* Geometric Modeling Subsystem */
#define UU_XITRC 0x1000					/* Disk I/O Subsystem */
#define UU_RITRC 0x2000					/* RDBMS Subsystem */
#define UU_SITRC 0x4000					/* SAL Subsystem */
#define UU_KITRC 0x8000					/* Kernal Operating Subsystem */
#define UU_IITRC 0x10000				/* User Interface Subsystem */
#define UU_NITRC 0x20000            /* MATH Subsystem */
#define UU_UITRC 0x40000				/* utl sub-system */
#define UU_BTRC  0x80000				/* symbol subsystem */
#define UU_BITRC 0x100000				/* symbol subsystem internal trace */ 
#define UU_ETRC  0x200000				/* space planning  */
#define UU_R1ITRC 0x400000				/* for internal associativity functions */
#define UU_U1TRC 0x800000				/* for list and hash functions */
#define UU_R1TRC 0x1000000				/* for external associativity functions */

#define UU_STKDEPTH 		128
#define UU_DEBUG_INT		1
#define UU_DEBUG_STRING 0


#define UMASKSH
#endif
