/********************************************************************* 
**  NAME:  gkserrorid.h
**
**      symbolic names of GKS error messages.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gerrorid.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:17
**
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  NO_ERROR if all went OK.
**
**  SIDE EFFECTS :  none
**
**  WARNINGS     :  none
*********************************************************************/

#ifndef GKSERRORIDH
/* implementation dependent errors */
#define ENOFLEVL 300			/* specified function not supported at this level of GKS */
#define EMEMSPAC 301			/* storage overflow has occurred in GKS */
#define ESEGSPAC 302			/* storage overflow has occurred in segment storage */
#define EIO_READ 303			/* I/O error has occurred while reading */
#define EIOWRITE 304			/* I/O error occurred while writing */
#define EIOSENDD 305			/* I/O error occurred while sending data to wkstn */
#define EIORECDA 306			/* I/O error occurred while receiving data from wkstn */
#define EIOLIBMG 307			/* I/O error occurrred during program library management */
#define EIORDWDT 308			/* I/O error occurred while reading wkstn desc table */
#define EINSTEIN 309			/* arithmetic error has occurred */
#define EBADAD 310			/* illegal address has been detected */
#define EFONTNF  311        /* font not found */
#define EFONTRERR 312       /* font file read error */
#define GARGERR 313			/* DIGS argument error */
#define GKSERRORIDH
#endif
