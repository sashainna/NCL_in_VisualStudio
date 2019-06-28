/*********************************************************************
**    NAME         :  gqelt.c -- element inquiry functions.
**       CONTAINS:
**		Gerror gqeltptr(eltno) -- inquire element pointer.
**		Gerror gqelttype(typ,size)	-- inquire cur elt type, size 
**		Gerror gqeltcont(p) -- inquire element content.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       gqelt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:23
*********************************************************************/
#include <stdio.h>
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "ginq.h"
#include "udebug.h"
#include "gsegac.h"
#include "gsegop.h"
#include "zsysdep.h"

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gqelt.c 2.1 4/7/86 23:00:21 single"};
#else
static char uu_sccsident[]={"@(#) gqelt.c 2.1 4/7/86 23:00:21 double"};
#endif

char *ug_lsielt();

/*********************************************************************
**    E_FUNCTION :  Gerror gqeltptr(eltno) -- inquire element pointer.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :   Gint *eltno -- element pointer.
**    RETURNS      : NCL_NO_ERROR if all went OK, ENOTSGOP if no open struct.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gqeltptr(eltno)					/* inquire element pointer */
/*$ OUTPUT */
Gint *eltno;								/* element pointer (0 to ncmds) */
{
	Gerror irtn;
	char us[100];

	irtn=NCL_NO_ERROR;
	if (ug_gksos.sysstate!=UG_SGOP) {
		irtn=ENOTSGOP; ug_errorhand(ENOTSGOP,"gqeltptr",NULL);
		*eltno= -1;
	}
	else {								/* no errors */
		*eltno=ug_gksstli.curelptr;
	}
	uu_denter2(UU_GTRC,(us,"%d=gqeltptr(%d)",irtn,*eltno));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  Gerror gqelttype(typ,size) -- inquire cur elt type, size.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  Gint *typ -- element type (NULL, POLYLN3OP,...)
**						 Gint *size --size in bytes of the element, including opcode.
**    RETURNS      : NCL_NO_ERROR if all went OK. else ENOTSGOP.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gqelttype(typ,size)			/* inquire cur elt type, size */
/*$ OUTPUT */
Gint *typ;								/* type (NULL if curelptr==0) */
Gint *size;								/* size of elt in bytes, excluding op code */
{
	Gerror irtn;
	int *p;							/* pointer to current element */
	UG_segstli *segptr;			/* pointer to segment header */
	char us[100];

	irtn=NCL_NO_ERROR;
	if (ug_gksos.sysstate!=UG_SGOP) {
		irtn=ENOTSGOP; ug_errorhand(ENOTSGOP,"gqelttype",NULL);
		*typ=NULL; *size=0;
	}
	if ((ug_gksos.sysstate==UG_SGOP)&&(ug_gksstli.curelptr==0)) {
		irtn=ENOTSGOP; ug_errorhand(ENOTSGOP,"gqelttype",NULL);
		*typ=NULL; *size=0;
	}
	if (irtn==NCL_NO_ERROR) {
		/* get pointer to open segment's headr */
  		segptr=ug_segac(ug_gksstli.opnseg);
		/* get pointer to current element */
	 	p = (int *)ug_lsielt(segptr->seglist,ug_gksstli.curelptr-1);
		/* get size of current element, excluding opcode */
		*size = ug_lsilen(segptr->seglist,ug_gksstli.curelptr-1);
		*typ=(*(UG_plylna3op *)p).elttype;			/* get op-code from element */
	}
	uu_denter2(UU_GTRC,(us,"%d=gqelttype(%d,%d)",irtn,*typ,*size));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  Gerror gqeltcont(p) -- inquire element content.
**    PARAMETERS   
**       INPUT  : 	none
**       OUTPUT :   Gint **p -- pointer to element data (incl opcode).
**    RETURNS      : NCL_NO_ERROR  if all went OK, else ENOTSGOP.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gqeltcont(p) 					/* inquire element content. */
/*$ OUTPUT */
Gint **p;
{
	Gerror irtn;
	UG_segstli *segptr;			/* pointer to segment header */
	char us[100];

	irtn=NCL_NO_ERROR;
	if (ug_gksos.sysstate!=UG_SGOP) {
		irtn=ENOTSGOP; ug_errorhand(ENOTSGOP,"gqeltcont",NULL);
		*p=NULL;
	}
	if ((ug_gksos.sysstate==UG_SGOP)&&(ug_gksstli.curelptr==0)) {
		irtn=ENOTSGOP; ug_errorhand(ENOTSGOP,"gqeltcont",NULL);
		*p=NULL;
	}
	if (irtn==NCL_NO_ERROR) {
		/* get pointer to open segment's headr */
  		segptr=ug_segac(ug_gksstli.opnseg);

		/* get pointer to current element, including opcode */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		{
			Gint *temp;
			temp = (int *)ug_lsielt(segptr->seglist,ug_gksstli.curelptr-1);

			convert_seg(p,temp);

		}
#else
	 	*p = (int *)ug_lsielt(segptr->seglist,ug_gksstli.curelptr-1);
#endif
	}
	uu_denter2(UU_GTRC,(us,"%d=gqeltcont(%x). curelptr=%d, openseg=%d",
		irtn,*p,ug_gksstli.curelptr,ug_gksstli.opnseg));
	uu_dexit;
	return(irtn);
}

