
/*********************************************************************
**    NAME         :   tlangio2.c
**       CONTAINS:
**				uti_prcord		ut_ctread		
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       qlangio1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:54
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "gmat4.h"
#include "ginq.h"
#include "dasnog.h"
#include "dmark.h"
#include "dinput.h"
#include "mdcpln.h"
#include "calcom.h"
#include "cdef.h"

#define CHECK 1

extern int  UQI_sciflag;
extern char UQ_dpt[];


/*********************************************************************
**    I_FUNCTION     :  ut_ctread(in,kf,len,cendindex)		
**       read terminal and convert ascii codes to langpak internal 
**			codes, Recognize %filename at beginning of line and change
**			to the specified file for input
**    PARAMETERS   
**       INPUT  : 
**          kf - index of where to start placing data
**       OUTPUT :  
**          in - array to receive data 
**          len - on return, length of data read in 
**          cendindex - times of calc being called       
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ut_ctread(in,kf,len,cendindex)		
int in[];				/* array to receive data */
int kf;					/* index of where to start placing data*/
int *len;				/* on return, length of data read in */
int *cendindex;       /* times of calc being called        */
{
	char  ibuf[MAXCALCBUF+50], pbuf[MAXCALCBUF];
	char  cbuf[10];
	char *p;
	int i,j, k, prej[10];
   int cdone, i1;
	UU_REAL ploc[3], pt[3], pt1[3], cord[3];
	UD_DEVENT event;
	UD_PPICKREC  picker;
	UD_NDCLOCREC  pndc;
	char us[120];
	int stat;
	
	/* get a line of input in ibuf */
again2: ibuf[0]='\0';
	pbuf[0] = '\n';
	cdone = UU_FALSE;
	j = k = 0;
	prej[k] = 0;
	sprintf(cbuf,"calc%d: ",*cendindex);
again: 	UD_MARK(i1, UU_TRUE);				/* setjmp */
	if (i1==-1)
		{
       UD_UNMARK(i1);
		 goto again;
		}
	else
		{
		 if (i1==0)
		 	ud_gevt(&event,UD_STRING,cbuf,1,UD_windev,22, NULL);
		 if (i1 > 0)		*cendindex = 0;
       UD_UNMARK(i1);
	   }

#define	 eind		event.indata

	while (cdone == UU_FALSE)
	{
	if (k>9)  k = 0;
	switch(event.evclass) 
	{
	 case UD_NONE: 
		break;

	 case UD_LOCATOR: 
		pndc.transform = eind.locdata.transform;
		pndc.cord[0] = eind.locdata.position[0];
		pndc.cord[1] = eind.locdata.position[1];
		pndc.cord[2] = 0.5;
		uv_ndctocc(pndc.cord, cord, pndc.transform);
      uv_projcpln(cord, pt, pndc.transform);
		um_mcstoccs (UQ_COORD, pt, ploc);
      um_vctmsc(UM_cpln.zaxis,UM_cpln.zdepth, cord);
      um_vcplvc(cord,ploc,ploc);
uu_dprint(UU_MTRC,(us,"after vcplvc,ploc=%g,%g,%g",ploc[0],ploc[1],ploc[2]));
		UM_cc_inttoext(ploc,ploc);
uu_dprint(UU_MTRC,(us,"after toext,ploc=%g,%g,%g",ploc[0],ploc[1],ploc[2]));
		uti_prcord (ibuf, j, ploc);
		prej[k++] = j;
		j = strlen (ibuf);
		break;

	case UD_STROKE:/* gmessage(D_ksws, "tread stroke not handled yet.\n"); */
		      uu_uerror0 (UQ_CALCERROR, 43);
		break;

	case UD_VALUATOR:
		if (UQI_sciflag == UQ_FLT)
			sprintf(&ibuf[j],UQ_dpt,eind.valdata);
		else
			sprintf(&ibuf[j],"%e ",eind.valdata);
		prej[k++] = j;
		j = strlen (ibuf);
		break;

	case UD_CHOICE:
	  if (event.evdev == UD_AUXMENU)
		switch (eind.choicedata)
		{
		 case	 UD_CDONE  	:  cdone = UU_TRUE;		break;
		 case  UD_CRESTART :  j = k = 0;
									prej[k] = 0;
									break;
		 case  UD_CRLAST   :  if (k>0)	 j = prej[--k];	
									else
										{	j = k = 0;
											prej[k] = 0;
										}
									break;
		 case  UD_CREJCHAR :	if (k>0)
										{
										 ibuf[--j] = '\0';
										 if (j==prej[k])  k--;
										}
									break;
		 default				:  ud_printmsg ("error - invalid choice\n");
									break;
		}	/* switch */
     else
		  /* gmessage (D_ksws, " eror - invalid device"); */
		  uu_uerror0 (UQ_CALCERROR, 44);
		break;

	case UD_PICK:
		if(eind.pickdata.depth == 0)
		  uu_uerror0 (UQ_CALCERROR,45); /* -- no entity exists -- */
		else
	   {
		pndc.transform = eind.pickdata.transform;
		pndc.cord[0] = eind.pickdata.position[0]; 
		pndc.cord[1] = eind.pickdata.position[1];
		pndc.cord[2] = 0.0;
		ug_mcopy(pndc.wndc3_mat, gqnormmat(pndc.transform));
		ug_invrt(pndc.ndcw3_mat, pndc.wndc3_mat);
		picker.depth = eind.pickdata.depth;
		for (i=0; i<picker.depth; i++)
			{
		    picker.pickpath[i] = eind.pickdata.pickpath[i];
			}
		/* if (um_d_nrendpt(2, &picker, &pndc, pt)==0) */
		if ((uc_ploc_to_coord(2, &picker, &pndc, pt))==UU_SUCCESS)
		  {
			um_mcstoccs (UQ_COORD, pt, pt1);
			uu_dprint(UU_MTRC,(us,"after toccs,pt1=%g,%g,%g",pt1[0],pt1[1],pt1[2]));
			UM_cc_inttoext(pt1,pt1);
			uu_dprint(UU_MTRC,(us,"after toext,pt1=%g,%g,%g",pt1[0],pt1[1],pt1[2]));
			uti_prcord (ibuf, j, pt1);
			prej[k++] = j;
			j = strlen (ibuf);
		  }
		else
		  uu_uerror0(/*pick wron entity*/UM_MODEL,49);
		}
		break;

	case UD_STRING: 
		sprintf (&ibuf[j], "%s", eind.stringdata);
		prej[k++] = j;
		j = strlen( ibuf);
	   if ((j==0) || (ibuf[j-1] != '\\')) 
			cdone = UU_TRUE;
		else
			j = j - 1;
		break;

	case UD_VECTOR:
		pt[0] = eind.vecdata[0];
		pt[1] = eind.vecdata[1];
		pt[2] = eind.vecdata[2];
		um_mcstoccs (UQ_VECT, pt, pt1);
uu_dprint(UU_MTRC,(us,"after vect,toccs,pt1=%g,%g,%g",pt1[0],pt1[1],pt1[2]));
/*
		UM_cc_inttoext(pt1,pt1);
uu_dprint(UU_MTRC,(us,"after vect,toext,pt1=%g,%g,%g",pt1[0],pt1[1],pt1[2]));
*/
		sprintf (&ibuf[j++], "v");
		uti_prcord (ibuf, j, pt1);
		prej[k++] = j - 1;
		j = strlen (ibuf);
		break;

	default	:	ud_printmsg (" error - invalid class\n");
					break;
	}	/* switch  */

	if (j > MAXCALCBUF)
	  {
		cdone = UU_TRUE;
	   uu_uerror0 (UQ_CALCERROR, 55); /* Input string is longer than the buffer size */
		j = MAXCALCBUF - 2;
		return(UU_FALSE);
	  }
	ibuf[j] = '\n';
	ibuf[j+1] = '\0';
	if (cdone == UU_FALSE) 
	   {
	  	 UD_MARK(i1, UU_TRUE);				/* setjmp */
		 if (i1==-1)
			{
       	 UD_UNMARK(i1);
		 	 goto again2;
			}
		 else
			{
		 	 if (i1==0)
				{
				 strcpy(&pbuf[1], ibuf);
		 		 ud_gevt(&event,UD_STRING,pbuf,1,UD_windev,22, NULL);
			   }
		 	 if (i1 > 0)		*cendindex = 0;
       	 UD_UNMARK(i1);
	   	}
		}
	}	/* while */

	ibuf[j] = '\0';
	*len=strlen(ibuf);
	while (ibuf[*len-1] == ' ')      (*len)--;
	ibuf[*len] = '\0';
	ut_cin_intern( in,*len,ibuf,kf);
	return(UU_TRUE);
}	/* ut_ctread */



/*********************************************************************
**    I_FUNCTION     :  uti_prcord (ibuf, j, val)
**       Pack a coordiante value into a string with '<' and '>' at
**			both end
**    PARAMETERS   
**       INPUT  : 
**          j - the last character in the string
**				val - an array contains the coordinate value
**       OUTPUT :  
**          ibuf - string buffer
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uti_prcord (ibuf, j, val)
char  ibuf[];
int   j;
UU_REAL  val[];

{
	register  i;

	sprintf (&ibuf[j++],"<" );
	for (i=0; i<3; i++)
		{
		 if (UQI_sciflag == UQ_FLT)
			 sprintf (&ibuf[j], UQ_dpt, val[i]);
		 else
			 sprintf (&ibuf[j], "%e", val[i]);
		 j = strlen (ibuf);
		 ibuf[j++] = ',';
		}
   sprintf (&ibuf[--j], ">");
}	/* uti_prcord */

