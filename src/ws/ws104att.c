
/********************************************************************* 
**  NAME:  ws104att.c
**
**      attribute control functions section.
**
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       ws104att.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:03
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "ws104.h"


extern Gs104 uw_104;			/* declare workstation local data */
/*------------------------- local declarations --------------------------*/

/*------------------------- output attributes ---------------------------*/

/*--------------------------------------------------------> UG_DLNINDEX */
uw_104lnindex(prms,reply)				/* set polyline bundle index */
int prms[],reply[];
{
	/* bundles not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104lnindex()"));
	uu_dexit;
}

/*--------------------------------------------------------> UG_DLINETYPE */
/*********************************************************************
**    I_FUNCTION :  uw_104linetype(prms,reply)			
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

uw_104linetype(prms,reply)			/* set linetype */
int prms[3];
int reply[];						/* no reply parameters */

{
	int n;
	char	num[3];

	uu_denter(UU_GITRC,(us,"uw_104linetype(%d)",prms[2]));
	/* n=prms[2] - 1; 		*/
	/*if ((n<0)||(n>2)) n=0;		/* if error, use solid lines */
	/*if (n==2)	n = 5;			/* map GKS standard to calcom code */
	uw_104.lntype = prms[2];
	uu_dexit;
}	/* uw_104linetype */



/*-------------------------------------------------------> UG_DLINEWIDTH */
uw_104linwid(prms,reply) 		/* set linewidth scale factor */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104linwid()"));
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  uw_104pen(pen)
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
uw_104pen(pen)
int pen;
{
static int last_pen;
char num[8],penselect[3];
/*
.....Select a new pen
.....New routine
.....Bobby  -  8/26/91
*/
	if (pen == -1)
	{
		last_pen = pen;
	}
	else if (pen != last_pen)
	{
		penselect[0] = BIAS104 + 4;
		penselect[1] = BIAS104 + pen;
		uu_ttput(uw_104.ttfd,penselect,2);
		buflen = buflen + 2;
		last_pen = pen;
	}
}

/*-------------------------------------------------------> UG_DLNCINDEX */
uw_104lncindex(prms,reply) 			/* set polyline color index */
int prms[3],reply[];				/* no output parameters */
{
	/* Change the current line color index to the index contained
	in prms[2]. This value is also recorded in ug_gksstli.curprats, so
	no workstation action is required unless the hardware maintains
	a current line color. */
	uu_denter(UU_GITRC,(us,"uw_104lncindex()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DMKINDEX */
uw_104mkridx(prms,reply) 			/* set polymarker index */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104mkridx()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DMKTYPE */
uw_104mktype(prms,reply) 			/* set marker type */
int prms[3],reply[];
{
	/* Set the current marker type to the type contained in prms[2].
	This value has also been recorded in ug_gksstli.curprats. */
	uu_denter(UU_GITRC,(us,"uw_104mktype()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DMKSIZE */
uw_104mksiz(prms,reply)				/* set marker size scale factor */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104mksiz()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DMKCINDEX */
uw_104mkcindex(prms,reply) 			/* set polymarker color index */
int prms[3],reply[];				/* no output parameters */
{
	/* Change the current marker color index to the index contained
	in prms[2]. This value is also recorded in ug_gksstli.curprats, so
	no workstation action is required unless the hardware maintains
	a current marker color. */
	uu_denter(UU_GITRC,(us,"uw_104mkcindex()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTEXTINDEX */
uw_104txtidx(prms,reply)			/* set text index */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104txtidx()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTEXTFP */
uw_104txtfp(prms,reply) 			/* set text font and precision */
int prms[4],reply[];
{
	/* Change the current text font to prms[2] and precision
	to prms[3]. These values have also been stored in
	ug_gksstli.curprats.txbundl.fp. */
	uu_denter(UU_GITRC,(us,"uw_104txtfp()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCHAREXP */
uw_104chrexp(prms,reply)			/* set character expansion factor */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104chrexp()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCHARSPACE */
uw_104chrspc(prms,reply)			/* set character spacing */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104chrspc()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTXCINDEX */
uw_104txcindex(prms,reply)			/* set text color index */
int prms[3],reply[];				/* no output parameters */
{
	/* Change the current text color index to the index contained
	in prms[2]. This value is also recorded in ug_gksstli.curprats, so
	no workstation action is required unless the hardware maintains
	a current text color. */
	uu_denter(UU_GITRC,(us,"uw_104txcindex()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCHARHT */
uw_104chrht(prms,reply)				/* set character height */
struct { Gint op; Gws id; Gchrht ht; } *prms;
int reply[];						/* no reply parameters */
{
	/* Change the current character height to the value specified
	in (*prms).ht. This value is in world coordinate units. Simulation
	routine ug_dcharheight may be used to change the value to NDC
	coordinates and call the UG_DCHHTNDC secondary entry point. The
	new char height has also been recorded in ug_gksstli.curprats. */
	uu_denter(UU_GITRC,(us,"uw_104chrht()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTXPLANE */
uw_104txtpln(prms,reply)			/* set character plane */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104txtpln()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCHARUP */
uw_104chrup(prms,reply)				/* set character up vector */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104chrup()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCHARUP3 */
uw_104chrup3(prms,reply)			/* set character up vector (3-d) */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104chrup3()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTEXTPATH */
uw_104txtpath(prms,reply)			/* set text path */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104txtpath()"));
	uu_dexit;
}

/*-------------------------------------------------------> GDTXADAPT */
uw_104txtadpt(prms,reply)			/* set text adaptation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104txtadpt()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTEXTALIGN */
uw_104txtalgn(prms,reply)			/* set text alignment */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104txtalgn()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DFAINDEX */
uw_104faidx(prms,reply)			/* set fill area index */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104faidx()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DFAINTSTYLE */
uw_104faintstyl(prms,reply)		/* set fill area interior style */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104faintstyl()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DFASTYLEINDEX */
uw_104fastylidx(prms,reply)		/* set fill area style index */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104fastylidx()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DFACINDEX */
uw_104facidx(prms,reply)			/* set fill area color index */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104facidx()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DPATSIZE */
uw_104patsiz(prms,reply)			/* set pattern size */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104patsiz()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DPATREFPT */
uw_104patrefpt(prms,reply)			/* set pattern reference point */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104patrefpt()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DASFS */
uw_104asfs(prms,reply)				/* set aspect source flags */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104asfs()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DPICKID */
uw_104pkid(prms,reply)				/* set pick identifier */
int prms[3],reply[];
{
	/* Set the current pick id to prms[2] */
	uu_denter(UU_GITRC,(us,"uw_104pkid()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DLNREP */
uw_104linrep(prms,reply)				/* set polyline representation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104linrep()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DMKREP */
uw_104mkrrep(prms,reply)				/* set polymarker representation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104mkrrep()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DTEXTREP */
uw_104txtrep(prms,reply)			/* set text representation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104txtrep()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DFAREP */
uw_104farep(prms,reply)				/* set fill area representation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104farep()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DPATREP */
uw_104patrep(prms,reply)				/* set pattern representation */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_104patrep()"));
	uu_dexit;
}

/*-------------------------------------------------------> UG_DCOLORREP */
uw_104colorrep(prms,reply)			/* set color representation */
struct {int op; int wsid; int indx; Gfloat rgb[3];} *prms;
int reply[];
{
	/* Change the color representation of color index (*prms).indx
	to the color specified in (*prms).rgb. */
	uu_denter(UU_GITRC,(us,"uw_104colorrep()"));
	uu_dexit;

}


/*********************************************************************
**    I_FUNCTION     :  uw_104linwid(prms,reply)
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
uw_104linewid(prms,reply)
struct { Gint op; Gws id; Gscale wid; } *prms;
int *reply;

{
   static firstime = UU_TRUE;

   uu_denter(UU_GITRC,(us,"uw_104linewid() width = %g",(*prms).wid));

   if (firstime)
     {
      ug_gksstli.wsopen[(*prms).id].wdtptr->outwdtpt->lnfac.nom =
      (*prms).wid / uw_104.wsxform.scale;
      uu_dprint(UU_GITRC,(us,"line width set to %g",
         ug_gksstli.wsopen[(*prms).id].wdtptr->outwdtpt->lnfac.nom));
      firstime = UU_FALSE;
     }

   uu_dexit;
}  /* uw_104linewid */

