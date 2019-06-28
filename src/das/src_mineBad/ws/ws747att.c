
/*********************************************************************
**    NAME         :  ws747att.c
**    CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       ws747att.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:04
*********************************************************************/

#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "ws7475.h"


extern Gs7475 uw_7475;					/* declare workstation local data */
/*------------------------- local declarations --------------------------*/

/*------------------------- output attributes ---------------------------*/

/*--------------------------------------------------------> UG_DLNINDEX */
uw_7475lnindex(prms,reply)				/* set polyline bundle index */
int prms[],reply[];
{
	/* bundles not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475lnindex()"));
	uu_dexit;
}

/*--------------------------------------------------------> UG_DLINETYPE */
/*********************************************************************
**    I_FUNCTION :  uw_7475linetype(prms,reply)			
**		Change the current polyline linetype to the type contained in
**		prms[2]. This value has also been recorded by the rest of GKS
**		in ug_gksstli.curprats, so no workstation action is required if
**		the workstation hardware does not maintain a current linetype 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_7475linetype(prms,reply)			/* set linetype */
int prms[3];
int reply[];						/* no reply parameters */

{
	int n;
	char	num[3];

	uu_denter(UU_GITRC,(us,"uw_7475linetype(%d)",prms[2]));
	n=prms[2] - 1; 
	if ((n<0)||(n>6)) n=0;		/* if error, use solid lines */
	if (n==1)	n = 2;			/* map GKS standard to dscan code */
	if (n==2)	n = 0;			/* map dot type to solid type */
	/* uw_dscan.curr_line_style=n;  */
	uu_ttput(uw_7475.ttfd,"LT ",3); 	/* set line type */
	if (n==0)
		uu_ttput(uw_7475.ttfd,";",1); 	/* set line type solid */
	else
	  {
		sprintf(num, "%d;", n);
		uu_ttput(uw_7475.ttfd,num,strlen(num)); 	/* set line type */
	  }
	uu_dexit;
}	/* uw_7475linetype */



/*-------------------------------------------------------> UG_DLINEWIDTH */
uw_7475linwid(prms,reply) 		/* set linewidth scale factor */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475linwid()"));
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  uw_7475pen(pen)
**      Selects a new pen if the requested pen is different than the
**      currently loaded pen.
**    PARAMETERS   
**       INPUT  : 
**          pen  -  Pen number to load.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_7475pen(pen)
int pen;
{
static int last_pen;
char num[8];
/*
.....Select a new pen
.....New routine
.....Bobby  -  7/15/91
*/
	if (pen == -1)
	{
		last_pen = pen;
	}
	else if (pen != last_pen)
	{
		sprintf (num,"SP %d;",pen);
		uu_ttput(uw_7475.ttfd,num,strlen(num));
		last_pen = pen;
	}
}
		
/*-------------------------------------------------------> UG_DLNCINDEX */
uw_7475lncindex(prms,reply) 			/* set polyline color index */
int prms[3],reply[];				/* no output parameters */
{
	/* Change the current line color index to the index contained
	in prms[2]. This value is also recorded in ug_gksstli.curprats, so
	no workstation action is required unless the hardware maintains
	a current line color. */
	uu_denter(UU_GITRC,(us,"uw_7475lncindex()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DMKINDEX */
uw_7475mkridx(prms,reply) 			/* set polymarker index */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475mkridx()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DMKTYPE */
uw_7475mktype(prms,reply) 			/* set marker type */
int prms[3],reply[];
{
	/* Set the current marker type to the type contained in prms[2].
	This value has also been recorded in ug_gksstli.curprats. */
	uu_denter(UU_GITRC,(us,"uw_7475mktype()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DMKSIZE */
uw_7475mksiz(prms,reply)				/* set marker size scale factor */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475mksiz()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DMKCINDEX */
uw_7475mkcindex(prms,reply) 			/* set polymarker color index */
int prms[3],reply[];				/* no output parameters */
{
	/* Change the current marker color index to the index contained
	in prms[2]. This value is also recorded in ug_gksstli.curprats, so
	no workstation action is required unless the hardware maintains
	a current marker color. */
	uu_denter(UU_GITRC,(us,"uw_7475mkcindex()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTEXTINDEX */
uw_7475txtidx(prms,reply)			/* set text index */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475txtidx()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTEXTFP */
uw_7475txtfp(prms,reply) 			/* set text font and precision */
int prms[4],reply[];
{
	/* Change the current text font to prms[2] and precision
	to prms[3]. These values have also been stored in
	ug_gksstli.curprats.txbundl.fp. */
	uu_denter(UU_GITRC,(us,"uw_7475txtfp()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCHAREXP */
uw_7475chrexp(prms,reply)			/* set character expansion factor */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475chrexp()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCHARSPACE */
uw_7475chrspc(prms,reply)			/* set character spacing */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475chrspc()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTXCINDEX */
uw_7475txcindex(prms,reply)			/* set text color index */
int prms[3],reply[];				/* no output parameters */
{
	/* Change the current text color index to the index contained
	in prms[2]. This value is also recorded in ug_gksstli.curprats, so
	no workstation action is required unless the hardware maintains
	a current text color. */
	uu_denter(UU_GITRC,(us,"uw_7475txcindex()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCHARHT */
uw_7475chrht(prms,reply)				/* set character height */
struct { Gint op; Gws id; Gchrht ht; } *prms;
int reply[];						/* no reply parameters */
{
	/* Change the current character height to the value specified
	in (*prms).ht. This value is in world coordinate units. Simulation
	routine ug_dcharheight may be used to change the value to NDC
	coordinates and call the UG_DCHHTNDC secondary entry point. The
	new char height has also been recorded in ug_gksstli.curprats. */
	uu_denter(UU_GITRC,(us,"uw_7475chrht()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTXPLANE */
uw_7475txtpln(prms,reply)			/* set character plane */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475txtpln()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCHARUP */
uw_7475chrup(prms,reply)				/* set character up vector */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475chrup()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCHARUP3 */
uw_7475chrup3(prms,reply)			/* set character up vector (3-d) */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475chrup3()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTEXTPATH */
uw_7475txtpath(prms,reply)			/* set text path */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475txtpath()"));
	uu_dexit;
}

/*-------------------------------------------------------> GDTXADAPT */
uw_7475txtadpt(prms,reply)			/* set text adaptation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475txtadpt()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTEXTALIGN */
uw_7475txtalgn(prms,reply)			/* set text alignment */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475txtalgn()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DFAINDEX */
uw_7475faidx(prms,reply)			/* set fill area index */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475faidx()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DFAINTSTYLE */
uw_7475faintstyl(prms,reply)		/* set fill area interior style */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475faintstyl()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DFASTYLEINDEX */
uw_7475fastylidx(prms,reply)		/* set fill area style index */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475fastylidx()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DFACINDEX */
uw_7475facidx(prms,reply)			/* set fill area color index */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475facidx()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DPATSIZE */
uw_7475patsiz(prms,reply)			/* set pattern size */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475patsiz()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DPATREFPT */
uw_7475patrefpt(prms,reply)			/* set pattern reference point */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475patrefpt()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DASFS */
uw_7475asfs(prms,reply)				/* set aspect source flags */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475asfs()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DPICKID */
uw_7475pkid(prms,reply)				/* set pick identifier */
int prms[3],reply[];
{
	/* Set the current pick id to prms[2] */
	uu_denter(UU_GITRC,(us,"uw_7475pkid()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DLNREP */
uw_7475linrep(prms,reply)				/* set polyline representation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475linrep()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DMKREP */
uw_7475mkrrep(prms,reply)				/* set polymarker representation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475mkrrep()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTEXTREP */
uw_7475txtrep(prms,reply)			/* set text representation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475txtrep()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DFAREP */
uw_7475farep(prms,reply)				/* set fill area representation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475farep()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DPATREP */
uw_7475patrep(prms,reply)				/* set pattern representation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475patrep()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCOLORREP */
uw_7475colorrep(prms,reply)			/* set color representation */
struct {int op; int wsid; int indx; Gfloat rgb[3];} *prms;
int reply[];
{
	/* Change the color representation of color index (*prms).indx
	to the color specified in (*prms).rgb. */
	uu_denter(UU_GITRC,(us,"uw_7475colorrep()"));
	uu_dexit;

}

/*********************************************************************
**    I_FUNCTION     :  uw_7475linwid(prms,reply)
**       Set linewidth scale factor
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_7475linewid(prms,reply)
struct { Gint op; Gws id; Gscale wid; } *prms;
int *reply;

{
   static firstime = UU_TRUE;

   uu_denter(UU_GITRC,(us,"uw_7475linewid() width = %g",(*prms).wid));

   if (firstime)
     {
      ug_gksstli.wsopen[(*prms).id].wdtptr->outwdtpt->lnfac.nom =
      (*prms).wid / uw_7475.wsxform.scale;
      uu_dprint(UU_GITRC,(us,"line width set to %g",
         ug_gksstli.wsopen[(*prms).id].wdtptr->outwdtpt->lnfac.nom));
      firstime = UU_FALSE;
     }

   uu_dexit;
}  /* uq_7475linewid */

