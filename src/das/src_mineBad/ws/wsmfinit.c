
#include "usysdef.h"
#if UU_COMP != UU_WIN2K

/*********************************************************************
**  NAME:  wsmfinit.c
**
**      GKS workstation: control functions section.
**
**		CONTAINS:
**			uw_mfquery_font
**			uw_mfflush
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			wsmfinit.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:11
**
*********************************************************************/
#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:cursorfont.h>
#include <decw$include:Xm.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#endif

#include "udebug.h"
#include "ws.h"
#define MFPGM
#include "wsxw.h"
#include "wsmf.h"
#undef MFPGM

/**********************************************************************
**    I_FUNCTION :  uw_mfquery_font(wid,hgt)
**              Select the "best" font to use based on the request.
**    PARAMETERS   
**       INPUT  :
**          wid     = Requested font width.
**          hgt     = Requested font height.
**       OUTPUT : None 
**          wid     = Actual font width.
**          hgt     = Actual font height.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfquery_font(wid,hgt)
int *wid,*hgt;
{
	int i,j,inc,dif,abs(),siz,num;
	int n[4];
	static int *fx,*fy,nfont=0;
	char **fc[4],*p,sx[8],sy[8];
/*
.....If first time here
.....get list of available fonts
*/
	if (nfont == 0)
	{
		fc[0] = XListFonts(uw_xw.disp,"?x?",100,&n[0]);
		fc[1] = XListFonts(uw_xw.disp,"?x??",100,&n[1]);
		fc[2] = XListFonts(uw_xw.disp,"??x?",100,&n[2]);
		fc[3] = XListFonts(uw_xw.disp,"??x??",100,&n[3]);
		nfont = n[0] + n[1] + n[2] + n[3];
		if (nfont == 0)
		{
			printf("Could not load fonts.\n");
			return;
		}
		fx = (int *)uu_malloc(nfont*sizeof(int));
		fy = (int *)uu_malloc(nfont*sizeof(int));
		inc = 0;
		for (j=0;j<4;j++)
		{
			for (i=0;i<n[j];i++)
			{
				strcpy(sx,fc[j][i]);
				p = strchr(sx,'x');
				if (p != 0)
				{
					strcpy(sy,p+1);
					*p = '\0';
					ul_to_number(sx,&fx[inc]);
					ul_to_number(sy,&fy[inc]);
					if (fx[inc] != 0 && fy[inc] != 0) inc++;
				}
			}
		}
		nfont = inc;
	}
/*
.....Determine best font to use
*/
	siz = *wid + *hgt;
	dif = 1000;
	inc = 0;
	for (i=0;i<nfont;i++)
	{
		num = abs(*wid-fx[i]) + abs(*hgt-fy[i]);
		if (num == 0)
		{
			inc = i;
			break;
		}
		if (num < dif)
		{
			inc = i;
			dif = num;
		}
	}
/*
.....Return font
*/
	*wid = fx[inc];
	*hgt = fy[inc];
	return;
}

/*********************************************************************
**    I_FUNCTION :  uw_mfflush()
**      
**	DESCRIPTION:
**	Flushes the output buffer.
**
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfflush()

{
	uw_glflush();
	XFlush(uw_xw.disp);
}
#endif
