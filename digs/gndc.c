/*********************************************************************
**    NAME         :  gndc.c
**       CONTAINS:
**		ug_segwcbox
**		ug_segndcbox
**		ug_ndcbox
**		ug_segndcbox3
**		ug_altersegbox
**		ug_adjustndcbox
**		ug_segndcboxl2
**		ug_segwcboxl2
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gndc.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:56:44
*********************************************************************/
#include "usysdef.h"
#include "zsysdep.h"
#include "umath.h"
#include "udebug.h"
#include "gsegop.h"
#include "gviw.h"
#include "gtbl.h"
#include "ginq.h"
#include "gdidd.h"
#include "gvlib.h"
#include "gsegac.h"
#include "gmat4.h"
#include "mdcoord.h"

extern int ug_viwseg;	 /* segment number for output clip/exp routines */
extern int Salter=-1;
/*********************************************************************
**    I_FUNCTION     :  ug_segwcbox(n)  
**			Calculate segment n's wc box.
**    PARAMETERS   
**       INPUT  : n 					-- segment number.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_segwcbox(n)                 
int n;
{
	UG_plylna3op *cmd;			/* pointer to each cmd */
	int ncmd;						/* number of commands in a segment */
	int icmd;						/* ith command */
	int m,len,opcode;
	char *ug_lsielt();			/* function to return list element */
	UG_segstli *segptr;			/* pointer to segment n's header */
	Gos systate;					/* save GKS state here */
	int oldviwseg;					/* save ug_viwseg here */
	UG_LSI *listp;					/* graphics command list for this seg */
	UU_LOGICAL reset;
	Gtxfp *fp;
	Gtxprec prec;
	Gwpoint3 *ptx,gpt[5],concat;
	Gwrect3 extent;

	uu_denter(UU_GITRC,(us,"ug_segndc(%d)", n));

	segptr=ug_segac(n);

/*
.....Don't recalculate box if it already exists
*/
	if (segptr->wcboxok == 1) return 0;

	if( segptr == NULL ) {
		uu_dprint(UU_GITRC,(us,"ug_segndc. seg %d doesn't exist",n));
		uu_dexit; return 0;
	}


  systate=ug_gksos.sysstate;			/* save GKS system state */

/*
... don't traverse in segment open state because we make calls to
... user callable GKS routines, which would add cmds to segments
*/

	if (systate==UG_SGOP) ug_gksos.sysstate=UG_WSAC;
	oldviwseg=ug_viwseg;			/* save ug_viwseg */
	ug_viwseg=n;						/* for clip/exp routines */
	reset = UU_FALSE;
	prec = UG_STRING;

	listp=(UG_LSI *)segptr->seglist;
	ncmd = ug_lsinelt((*listp));
	uu_dprint(UU_GITRC,(us,"ug_segndc. ncmd=%d",ncmd));

/*
... for each command in seg n
*/
	for( icmd=0; icmd < ncmd; ++icmd ) {
	 cmd = (UG_plylna3op *)ug_lsielt(listp,icmd);
    opcode=(*cmd).elttype;

    if (opcode<=UG_OPPTR) opcode=opcode&(UG_OPPTR-1);
/*
... Switch on segment element type
*/
    switch (opcode) 
    {
    case UG_NOOP: break;
    case UG_PAGEOP: break;

    case UG_PLYLNA3OP: 
			len=(*(UG_plylna3op *)cmd).len;
			ug_boxexpln3(segptr,(*(UG_plylna3op *)cmd).pts,len);
			break;
    case UG_PLYLNA2OP: 
			len=(*(UG_plylna2op *)cmd).len;
			ug_boxexpln2(segptr,(*(UG_plylna2op *)cmd).pts,len);
			break;
	 case UG_PLYLNRASOP: 
			break;
    case UG_PLYMKA3OP: 
			len=(*(UG_plymka3op *)cmd).len;
			ug_boxexpmk3(segptr,(*(UG_plymka3op *)cmd).pts,len);
			break;
    case UG_PLYMKA2OP: 
			len=(*(UG_plymka2op *)cmd).len;
			ug_boxexpmk2(segptr,(*(UG_plymka2op *)cmd).pts,len);
	 case UG_PLYMKRASOP: 
			break;
/*
.....Can't calculate a 3D box when the segment
.....contains 2D text.
*/
    case UG_TEXTOP:
/*
.....do not reset the text box when calculating box for altered label poistion
*/
		if(Salter<0)
			reset = UU_TRUE;
		else
		{
/*
........Calculate NDC box around text
*/
			ug_txbox3(&(*(UG_textop *)cmd).position,
				(*(UG_textop *)cmd).string,gpt);
/*
........Expand segment box
*/
			ug_boxexpln3(segptr,gpt,4);
		}
			break;
	 case UG_TEXTRASOP: 
			break;
	 case UG_FLAREA3OP: 
			len=(*(UG_flarea3op *)cmd).len;
			ug_boxexpfa3(segptr,(*(UG_flarea3op *)cmd).pts,len);
			break;
	 case UG_SHADEAREAOP: 
			len=((UG_shadearea *)cmd)->len;
			ug_boxexpshade(segptr,((UG_flarea3op *)cmd)->pts,len);
			break;
	 case UG_FLAREAOP: 
			len=(*(UG_flareaop *)cmd).len;
			ug_boxexpfa2(segptr,(*(UG_flareaop *)cmd).pts,len);
			break;
	 case UG_FLAREARASOP: 
			break;
	case UG_CELLOP:	
			break;
	case UG_CELLRUNOP:
			break;
	case UG_CELLRASOP:
			break;
	case UG_CELLRUNRASOP:
			break;
	case UG_PROCOP: 
			break;
   case UG_CALLOP: 
			m=(*(UG_callop *)cmd).segno;  /* call seg m */
         ug_segwcbox(m);						/* recursive call */
			break;
	case UG_SNTRANOP: 
			ug_sntran((*(UG_sntranop *)cmd).xform); 
			break;
	case UG_MTRANOP:
			ug_smodxf((*(UG_mtranop *)cmd).xf,(*(UG_mtranop *)cmd).type); 
			break;
	case UG_LMTRANOP:
			ug_slmodxf((*(UG_lmtranop *)cmd).xf,(*(UG_lmtranop *)cmd).type); 
			break;
   case UG_DFATSOP:  break;
   case UG_INTENSOP: break;	/* un comment */
   case UG_LSTYLOP: break;
   case UG_LWIDOP: /*ug_linewidth((*(UG_lwidop *)cmd).width); */break;
   case UG_FONTOP:
		fp = (Gtxfp *)&(*(UG_fontop *)cmd).p;
		prec = fp->prec;
		ug_textfp(&(*(UG_fontop *)cmd).p);
		break;
   case UG_CHHGTOP: ug_charheight((*(UG_chhgtop *)cmd).height); break;
	case UG_CHEXPOP: ug_charexp((*(UG_chexpop *)cmd).expn); break;
   case UG_CHPLANEOP: ug_txplane(&(*(UG_chplaneop *)cmd).txpvc); break;
   case UG_CHUP3OP: ug_charup3(&(*(UG_chup3op *)cmd).upvec); break;
   case UG_CHUP2OP: ug_charup(&(*(UG_chup2op *)cmd).upvec); break;
   case UG_CHPATHOP: ug_textpath((*(UG_chpathop *)cmd).path); break;
   case UG_CHSPACEOP: ug_charspace((*(UG_chspaceop *)cmd).spacing); break;
   case UG_CHJUSTOP: ug_textalign(&(*(UG_chjustop *)cmd).align); break;
   case UG_SYMBOLOP: ug_marktype((*(UG_symbolop *)cmd).type); break;
   case UG_PICKIDOP: ug_spickid((*(UG_pickidop *)cmd).pid); break;
	case UG_LNCOLROP: break;
	case UG_MKCOLROP: break;
	case UG_TXCOLROP: break;
	case UG_FACOLROP: break;
	case UG_LUCENCYOP: break;
	case UG_SHDCOLROP: break;
	case UG_MATERIALOP: break;
	case UG_EDGEFLAGOP: ug_sedgeflag((*(UG_edgeflagop *)cmd).f); break;
    default:/* fprintf(ug_gksos.erfile,"giewsg illegal opcode=%d\n",opcode);*/
         break;
    };                             /* case */
    }                              /* normal command */

	if (segptr->wcbox.urb.x >= segptr->wcbox.llf.x) (*segptr).wcboxok=1;
	if (reset) UG_RESET_WCBOX(segptr);

	/* reset NDC COORDS OK flag */
	(*segptr).xforms &= ~UG_SEGNDCBOX;	
	ug_gksos.sysstate=systate;		/* restore GKS state */
	ug_viwseg=oldviwseg;			/* restore viwseg */
	
	uu_dprint(UU_GITRC,(us,"ug_segndc returning."));
	uu_dexit;
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  ug_segndcbox(n)  
**			Calculate segment n's wc box into ndc box.
**    PARAMETERS   
**       INPUT  : n 					-- segment number.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_segndcbox(n,nrect)
Gnrect *nrect;
{
	int xf,mask;
	Gstate state;
	UG_segstli *segptr;
	Gfloat tmpxform[4][4];		/* holds global modelling xform temporarily */
/*
.....Calculate box if necessary
*/
	segptr = ug_segac(n);
	if (segptr->wcboxok != 1) ug_segwcbox(n);
/*
.....Push current transformations if neccessary.  Only required if this
.....segment changes modelling xform. 
*/
	if(segptr->xforms & (UG_SEGMODXF))
		ug_savestate(&state);
/*
.....Concatenate local mod xform to global mod xform if it's not identity
*/
	if(!ug_lmodidnt)
	{
		ug_matmp(tmpxform,ug_modxform,ug_lmodxform);
		ug_mcopy(ug_modxform,tmpxform);
		ug_ident(ug_lmodxform);			/* set local model xform to identity*/
		ug_lmodidnt = 1;					/* ug_lmodxform is now identity */
	}
/*
.....Find active XFORM in segment
.....We'll settle for the first one
.....at this time
*/
	for (xf=0;xf<UG_MAXNTRAN;xf++)
	{
		mask = (1 << xf) | UG_SEGINNTRAN;
		if (segptr->xforms & mask) break;
	}
/*
.....Convert segment's wc box to ndc box
*/
	ug_ndcbox(&(segptr->wcbox),nrect,xf);
/*
.....Restore digs state (xforms)
*/
	if (segptr->xforms & (UG_SEGMODXF))
		ug_resstate(&state);
/*
.....End of routine
*/
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  ug_ndcbox(wcbox,ndcbox,xf)  
**			Convert a World Coordinate box to an NDC box.
**    PARAMETERS   
**       INPUT  :
**          wcbox   = World Coordinate box.
**          xf      = Active viewport transformation.
**       OUTPUT :
**          nrect   = Normalized Device Coordinate box.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_ndcbox(wcbox,nrect,xf)
Gwrect3 *wcbox;
Gnrect *nrect;
int xf;
{
	int i,j;
	Gnpoint3 np1;
	Gfloat wc1[6],wcp[3];
	static int ix[]={0,1,2, 3,1,2, 3,4,2, 0,4,2, 0,1,5, 3,1,5, 3,4,5, 0,4,5};
/*
.....Convert wc box to ndc box
*/
	if (xf == 0)
	{
		nrect->ll.x = wcbox->llf.x;
		nrect->ll.y = wcbox->llf.y;
		nrect->ur.x = wcbox->urb.x;
		nrect->ur.y = wcbox->urb.y;
		return 0;
	}
	wc1[0] = wcbox->llf.x;
	wc1[1] = wcbox->llf.y;
	wc1[2] = wcbox->llf.z;
	wc1[3] = wcbox->urb.x;
	wc1[4] = wcbox->urb.y;
	wc1[5] = wcbox->urb.z;
	j = 0;
	nrect->ll.x = 10000;
	nrect->ll.y = 10000;
	nrect->ur.x = -10000;
	nrect->ur.y = -10000;
	for (i=0;i<8;i++)
	{
		wcp[0] = wc1[ix[j]];
		wcp[1] = wc1[ix[j+1]];
		wcp[2] = wc1[ix[j+2]];
		j+=3;
		ug_xform(wcp[0],wcp[1],wcp[2],&np1,ug_cxform[xf]);
		if (np1.x < nrect->ll.x) nrect->ll.x = np1.x;
		if (np1.y < nrect->ll.y) nrect->ll.y = np1.y;
		if (np1.x > nrect->ur.x) nrect->ur.x = np1.x;
		if (np1.y > nrect->ur.y) nrect->ur.y = np1.y;
	}
/*
.....End of routine
*/
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  ug_segndcbox3(n)  
**			Calculate segment n's wc box into ndc box (3D).
**    PARAMETERS   
**       INPUT  : n 					-- segment number.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_segndcbox3(n,nrect)
Gnrect3 *nrect;
{
	int xf,mask,i,j;
	Gnpoint3 np1;
	Gstate state;
	UG_segstli *segptr;
	Gfloat tmpxform[4][4];		/* holds global modelling xform temporarily */
	Gfloat wc1[6],wcp[3];
	static int ix[]={0,1,2, 3,1,2, 3,4,2, 0,4,2, 0,1,5, 3,1,5, 3,4,5, 0,4,5};
/*
.....Calculate box if necessary
*/
	segptr = ug_segac(n);
	if (segptr->wcboxok != 1) ug_segwcbox(n);
/*
.....Push current transformations if neccessary.  Only required if this
.....segment changes modelling xform. 
*/
	if(segptr->xforms & (UG_SEGMODXF))
		ug_savestate(&state);
/*
.....Concatenate local mod xform to global mod xform if it's not identity
*/
	if(!ug_lmodidnt)
	{
		ug_matmp(tmpxform,ug_modxform,ug_lmodxform);
		ug_mcopy(ug_modxform,tmpxform);
		ug_ident(ug_lmodxform);			/* set local model xform to identity*/
		ug_lmodidnt = 1;					/* ug_lmodxform is now identity */
	}
/*
.....Find active XFORM in segment
.....We'll settle for the first one
.....at this time
*/
	for (xf=0;xf<UG_MAXNTRAN;xf++)
	{
		mask = (1 << xf) | UG_SEGINNTRAN;
		if (segptr->xforms & mask) break;
	}
/*
.....Convert segment's wc box to ndc box
*/
	if (xf == 0)
	{
		nrect->llf.x = segptr->wcbox.llf.x;
		nrect->llf.y = segptr->wcbox.llf.y;
		nrect->llf.z = segptr->wcbox.llf.z;
		nrect->urb.x = segptr->wcbox.urb.x;
		nrect->urb.y = segptr->wcbox.urb.y;
		nrect->urb.z = segptr->wcbox.urb.z;
		return 0;
	}
	wc1[0] = segptr->wcbox.llf.x;
	wc1[1] = segptr->wcbox.llf.y;
	wc1[2] = segptr->wcbox.llf.z;
	wc1[3] = segptr->wcbox.urb.x;
	wc1[4] = segptr->wcbox.urb.y;
	wc1[5] = segptr->wcbox.urb.z;
	j = 0;
	nrect->llf.x = 10000;
	nrect->llf.y = 10000;
	nrect->llf.z = 10000;
	nrect->urb.x = -10000;
	nrect->urb.y = -10000;
	nrect->urb.z = -10000;
	for (i=0;i<8;i++)
	{
		wcp[0] = wc1[ix[j]];
		wcp[1] = wc1[ix[j+1]];
		wcp[2] = wc1[ix[j+2]];
		j+=3;
		ug_xform(wcp[0],wcp[1],wcp[2],&np1,ug_cxform[xf]);
		if (np1.x < nrect->llf.x) nrect->llf.x = np1.x;
		if (np1.y < nrect->llf.y) nrect->llf.y = np1.y;
		if (np1.z < nrect->llf.z) nrect->llf.y = np1.z;
		if (np1.x > nrect->urb.x) nrect->urb.x = np1.x;
		if (np1.y > nrect->urb.y) nrect->urb.y = np1.y;
		if (np1.z > nrect->urb.z) nrect->urb.z = np1.z;
	}
/*
.....Restore digs state (xforms)
*/
	if (segptr->xforms & (UG_SEGMODXF))
		ug_resstate(&state);
/*
.....End of routine
*/
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  ug_altersegbox(n,zpt) 
**			Calculate segment n's wc box , and the center of the fron face of 
**			this box
**    PARAMETERS   
**       INPUT  : n 					-- segment number.
**				  zpt					-- center of box
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_altersegbox(n,zpt)                 
int n;
UM_coord zpt;
{
	UG_segstli *segptr;	

	Salter = 0;
	ug_segwcbox(n);
	segptr=ug_segac(n);
	zpt[0]=segptr->wcbox.urb.x;
	zpt[1]=segptr->wcbox.urb.y;
	zpt[2] =segptr->wcbox.urb.z;
	Salter = -1;
}

/*********************************************************************
**    E_FUNCTION     :  ug_adjustndcbox(rect1,rect2)
**			Adjust the size of the NDC box 'rect1' by the NDC box 'rect2'.
**    PARAMETERS   
**       INPUT  :
**          rect1     NDC box to update.
**          rect2     NDC box used to update 'rect1'.
**       OUTPUT :
**          rect1     Updated NDC box.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_adjustndcbox(rect1,rect2)
Gnrect *rect1;
Gnrect *rect2;
{
/*
.....Adjust the first NDC box
.....by the second NDC box
*/
	if (rect2->ll.x < rect1->ll.x) rect1->ll.x = rect2->ll.x;
	if (rect2->ll.y < rect1->ll.y) rect1->ll.y = rect2->ll.y;
	if (rect2->ur.x > rect1->ur.x) rect1->ur.x = rect2->ur.x;
	if (rect2->ur.y > rect1->ur.y) rect1->ur.y = rect2->ur.y;
}


/*********************************************************************
**    I_FUNCTION     :  ug_segwcboxl2(n, pickid, wcbox)  
**			Calculate segment n level 2 (by pick id) wc box.
**    PARAMETERS   
**       INPUT  : n 					-- segment number.
**				pickid:		pickid of the level 2 section of segment n
**       OUTPUT : wc: world cord box
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_segwcboxl2(n, pickid, wcbox)                 
int n, pickid;
Gwrect3 *wcbox;
{
	UG_plylna3op *cmd;
	int ncmd;
	int icmd;
	int m,len,opcode,start_box;
	char *ug_lsielt();
	UG_segstli *segptr;
	Gos systate;
	int oldviwseg;
	UG_LSI *listp;
	UU_LOGICAL reset;
	Gtxfp *fp;
	Gtxprec prec;
	Gwpoint3 *ptx,gpt[5],concat;
	Gwrect3 extent;

	segptr=ug_segac(n);

	if( segptr == NULL ) 
	{
		return;
	}
	systate=ug_gksos.sysstate;			/* save GKS system state */
/*
... don't traverse in segment open state because we make calls to
... user callable GKS routines, which would add cmds to segments
*/
	if (systate==UG_SGOP) ug_gksos.sysstate=UG_WSAC;
	oldviwseg=ug_viwseg;			/* save ug_viwseg */
	ug_viwseg=n;						/* for clip/exp routines */
	reset = UU_FALSE;
	prec = UG_STRING;

	listp=(UG_LSI *)segptr->seglist;
	ncmd = ug_lsinelt((*listp));
	start_box = 0;
/*
... for each command in seg n
*/
	for ( icmd=0; icmd < ncmd; ++icmd ) 
	{
		cmd = (UG_plylna3op *)ug_lsielt(listp,icmd);
		opcode=(*cmd).elttype;
		if (opcode<=UG_OPPTR) opcode=opcode&(UG_OPPTR-1);
/*
... Switch on segment element type
*/
		switch (opcode) 
		{
		case UG_NOOP: break;
		case UG_PAGEOP: break;
		case UG_PLYLNA3OP: 
			if (start_box==0)
				break;
			len=(*(UG_plylna3op *)cmd).len;
			ug_box_exp(wcbox, opcode, (*(UG_plylna3op *)cmd).pts,len);
			break;
		case UG_PLYLNA2OP: 
			if (start_box==0)
				break;
			len=(*(UG_plylna2op *)cmd).len;
			ug_box_exp(wcbox, opcode, (*(UG_plylna2op *)cmd).pts,len);
			break;
		case UG_PLYLNRASOP: 
			break;
		case UG_PLYMKA3OP: 
			if (start_box==0)
				break;
			len=(*(UG_plymka3op *)cmd).len;
			ug_box_exp(wcbox, opcode, (*(UG_plylna3op *)cmd).pts,len);
			break;
		case UG_PLYMKA2OP: 
			if (start_box==0)
				break;
			len=(*(UG_plymka2op *)cmd).len;
			ug_box_exp(wcbox, opcode, (*(UG_plylna2op *)cmd).pts,len);
		case UG_PLYMKRASOP: 
			break;
/*
.....Can't calculate a 3D box when the segment
.....contains 2D text.
*/
		case UG_TEXTOP:
			if (start_box==0)
				break;
/*
.....do not reset the text box when calculating box for altered label poistion
*/
			if(Salter<0)
				reset = UU_TRUE;
			else
			{
/*
........Calculate NDC box around text
*/
				ug_txbox3(&(*(UG_textop *)cmd).position,
					(*(UG_textop *)cmd).string,gpt);
/*
........Expand segment box
*/
				ug_box_exp(wcbox, UG_PLYLNA3OP, gpt,4);
			}
			break;
		case UG_TEXTRASOP: 
			break;
		case UG_FLAREA3OP: 
			if (start_box==0)
				break;
			len=(*(UG_flarea3op *)cmd).len;
			ug_box_exp(wcbox, opcode, (*(UG_flarea3op *)cmd).pts,len);
			break;
		case UG_SHADEAREAOP: 
			if (start_box==0)
				break;
			len=((UG_shadearea *)cmd)->len;
			ug_box_exp(wcbox, opcode, (*(UG_flarea3op *)cmd).pts,len);
			break;
		case UG_FLAREAOP: 
			if (start_box==0)
				break;
			len=(*(UG_flareaop *)cmd).len;
			ug_box_exp(wcbox, opcode, (*(UG_flareaop *)cmd).pts,len);
			break;
		case UG_FLAREARASOP: 
			break;
		case UG_CELLOP:	
			break;
		case UG_CELLRUNOP:
				break;
		case UG_CELLRASOP:
				break;
		case UG_CELLRUNRASOP:
				break;
		case UG_PROCOP: 
			break;
		case UG_CALLOP: 
			if (start_box==0)
				break;
			m=(*(UG_callop *)cmd).segno;
			ug_segwcboxl2(m, pickid, wcbox);
			break;
		case UG_SNTRANOP: 
			ug_sntran((*(UG_sntranop *)cmd).xform); 
			break;
		case UG_MTRANOP:
			ug_smodxf((*(UG_mtranop *)cmd).xf,(*(UG_mtranop *)cmd).type); 
			break;
		case UG_LMTRANOP:
			ug_slmodxf((*(UG_lmtranop *)cmd).xf,(*(UG_lmtranop *)cmd).type); 
			break;
		case UG_DFATSOP:  break;
		case UG_INTENSOP: break;
		case UG_LSTYLOP: break;
		case UG_LWIDOP: break;
		case UG_FONTOP:
			fp = (Gtxfp *)&(*(UG_fontop *)cmd).p;
			prec = fp->prec;
			ug_textfp(&(*(UG_fontop *)cmd).p);
			break;
		case UG_CHHGTOP: ug_charheight((*(UG_chhgtop *)cmd).height); break;
		case UG_CHEXPOP: ug_charexp((*(UG_chexpop *)cmd).expn); break;
		case UG_CHPLANEOP: ug_txplane(&(*(UG_chplaneop *)cmd).txpvc); break;
		case UG_CHUP3OP: ug_charup3(&(*(UG_chup3op *)cmd).upvec); break;
		case UG_CHUP2OP: ug_charup(&(*(UG_chup2op *)cmd).upvec); break;
		case UG_CHPATHOP: ug_textpath((*(UG_chpathop *)cmd).path); break;
		case UG_CHSPACEOP: ug_charspace((*(UG_chspaceop *)cmd).spacing); break;
		case UG_CHJUSTOP: ug_textalign(&(*(UG_chjustop *)cmd).align); break;
		case UG_SYMBOLOP: ug_marktype((*(UG_symbolop *)cmd).type); break;
		case UG_PICKIDOP: 
			ug_spickid((*(UG_pickidop *)cmd).pid);
			if (start_box==1)
				goto done;
			if ((*(UG_pickidop *)cmd).pid==pickid)
			{
				start_box=1;
			}
			break;
		case UG_LNCOLROP: break;
		case UG_MKCOLROP: break;
		case UG_TXCOLROP: break;
		case UG_FACOLROP: break;
		case UG_LUCENCYOP: break;
		case UG_SHDCOLROP: break;
		case UG_MATERIALOP: break;
		case UG_EDGEFLAGOP: ug_sedgeflag((*(UG_edgeflagop *)cmd).f); break;
		default:
			break;
		};                             
	}
done:;
	ug_gksos.sysstate=systate;
	ug_viwseg=oldviwseg;	
}

/*********************************************************************
**    I_FUNCTION     :  ug_segndcboxl2(n, pickid, nrect)
**			Calculate segment n level 2 (by pick id) nc box.
**    PARAMETERS   
**       INPUT  : n 					-- segment number.
**				pickid:		pickid of the level 2 section of segment n
**       OUTPUT : wc: world cord box
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_segndcboxl2(n, pickid, nrect)
Gnrect *nrect;
int n, pickid;
{
	int xf,mask;
	Gstate state;
	UG_segstli *segptr;
	Gwrect3 wcbox;
	Gfloat tmpxform[4][4];		/* holds global modelling xform temporarily */

	wcbox.llf.x = 1.0e+10;
	wcbox.llf.y = 1.0e+10; 
	wcbox.llf.z = 1.0e+10;
	wcbox.urb.x = -1.0e+10; 
	wcbox.urb.y = -1.0e+10;
	wcbox.urb.z = -1.0e+10;
/*
.....Calculate box if necessary
*/
	segptr = ug_segac(n);
	ug_segwcboxl2(n, pickid, &wcbox);
/*
.....Push current transformations if neccessary.  Only required if this
.....segment changes modelling xform. 
*/
	if(segptr->xforms & (UG_SEGMODXF))
		ug_savestate(&state);
/*
.....Concatenate local mod xform to global mod xform if it's not identity
*/
	if(!ug_lmodidnt)
	{
		ug_matmp(tmpxform,ug_modxform,ug_lmodxform);
		ug_mcopy(ug_modxform,tmpxform);
		ug_ident(ug_lmodxform);	
		ug_lmodidnt = 1;
	}
/*
.....Find active XFORM in segment
.....We'll settle for the first one
.....at this time
*/
	for (xf=0;xf<UG_MAXNTRAN;xf++)
	{
		mask = (1 << xf) | UG_SEGINNTRAN;
		if (segptr->xforms & mask) break;
	}
/*
.....Convert segment's wc box to ndc box
*/
	ug_ndcbox(&wcbox,nrect,xf);
/*
.....Restore digs state (xforms)
*/
	if (segptr->xforms & (UG_SEGMODXF))
		ug_resstate(&state);
}

