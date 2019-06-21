/*********************************************************************
**    NAME         :  gfind.c -- picking simulation routines.
**       CONTAINS:
**		int gfindreg(mode,loc,dx,dy,buf,buflen,n,len,vis)
**  	int gfindreg2(mode,loc,dx,dy,buf,buflen,n,len,vis)
**		ug_otone(len,segs,find) 
**		gpckm(u, v, pickrec,xform) -- scans segments for entity to pick.
**		gtrav(segno) -- Initialize traversal via gtravnxt.
**		int ug_travnx2(type,n,coords,path,depth)
**		int gtravnxt(type,n,coords,path,depth)  -- get next polyln or mk.
**		int gtravxfm(n,b,a) -- b=a xformed by modxform[vlen-1].
**		int gtravxfm2(n,b,a) -- b=a(2d) xformed by modxform[vlen-1].
**		static int ug_nxtseg() -- return next seg number.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       gfind.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:19
*********************************************************************/
#include <stdio.h>
#include	"gtbl.h"
#include "g.h"
#include "gviw.h"
#include "gsegop.h"
#include "gstack.h"
#include "udebug.h"
#include "zsysdep.h"
#include "gsegac.h"
#include "gmat4.h"
#include "gconvert.h"
#include "gdidd.h"

static int noitems, buffindx,*buff,bufflen,irtn;
static Gfloat ddx,ddy;

		/* used for conversion routines - to free used memory */
static Gwpoint3 *temp3;
static Gwpoint *temp;

static int nseg;			/* segment number passed to gtrav */
static int first=1;		/*1= first time after gtrav called */
static int xmodel=1;		/* 1=apply modelling xform, 0=dont */
static int vwindex;			/* normalization xform */

/*********************************************************************
**    I_FUNCTION     :  static int ug_nxtseg() -- return next seg number.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ug_nxtseg()
/* return next segment number, or -1. If first==1, start over */
{
	static int i=0;
	UG_segstli *p;
	if (nseg>=0) {
		if (first==1) return(nseg);
		else return(-1);
	}

	if (first==1) ug_seginitscan();
	while ((p=ug_segscan())!=NULL) {
		return((*p).segid);
	}
	return(-1);
}

/********************************************************************* 
**  E_FUNCTION:  int gfindreg(mode,loc,dx,dy,buf,buflen,n,len,vis)
**      Find all graphics primitives within or without a
**			rectangle.
**  PARAMETERS   
**      INPUT:  int mode -- 0=partially within, 1=at least partially 
**									   without, 2=wholly within, 3=wholly without.
**										Only modes 0 and 3 currently work.
**					 Gnpoint *loc -- center of search rectangle.
**					 Gfloat dx,dy --half lengths of search rectangle.
**					 int buflen -- length of buf.
**					 int vis -- 0=visible, 1=invis, 2=both.
**      OUTPUT: int *n-- number of items found.
**					 int *len -- length of the items in buf.
**					 int buf[] -- buffer to hold items. Each item consists of:
**								depth of this item.
**								depth-1 segment numbers.
**							   pick-id of item.
**					 	The first item starts at buf[0]. Subsequent items
**						follow each other contiguously in buf.
**
**  RETURNS      :  0 if all went OK, 1 if buf was not big enough
**							to hold all items found. In this case, len will
**							be the number of items that would fit into buf.
**
**  SIDE EFFECTS :  none
** **  WARNINGS     :  none
*********************************************************************/
int gfindreg(fmode,loc,dx,dy,buf,buflen,n,len,vis)
int fmode;			/* 0=part in, 1=part out, 2=all in, 3=all out.*/
Gnpoint *loc;		/* center of search rect*/
Gfloat dx,dy;		/* size of search rect */
int buf[],buflen;	/* output buffer */
int *n;				/* number of items found */
int *len;			/* length of buffer */
int vis;				/* 0=vis, 1=invis, 2=both */
{
	int ug_otone();
	uu_denter(UU_GTRC,(us,
		"gfindreg(%d,loc=%g %g, dx,dy=%g %g,buflen=%d, vis=%d)",fmode,
					(*loc).x,(*loc).y,dx,dy,buflen,vis));
	irtn=0;
	*len=0;
	*n=0;
	noitems=0;
	buffindx=0;
	bufflen=buflen;
	buff=buf;
	ddx=dx; ddy=dy;
	gfindndc(fmode,loc,dx,dy,ug_otone,vis);
	*n=noitems;
	*len=buffindx;
	uu_denter2(UU_GTRC,(us,"gfindreg returns %d. n=%d,len=%d",irtn,*n,*len));
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  int gfindreg2(mode,loc,dx,dy,buf,buflen,n,len,vis)
**      Find all graphics primitives within or without a
**			rectangle.
**  PARAMETERS   
**      INPUT:  int mode -- 0=partially within, 1=at least partially 
**									   without, 2=wholly within, 3=wholly without.
**					 Gnpoint *loc -- center of search rectangle.
**					 Gfloat dx,dy --half lengths of search rectangle.
**					 int buflen -- length of buf.
**					 int vis -- 0=visible, 1=invis, 2=both.
**      OUTPUT: int *n-- number of items found.
**					 int *len -- length of the items in buf.
**					 int buf[] -- buffer to hold items. Each item consists of:
**								depth of this item.
**								depth-1 segment numbers.
**							   pick-id of item.
**					 	The first item starts at buf[0]. Subsequent items
**						follow each other contiguously in buf.
**
**  RETURNS      :  0 if all went OK, 1 if buf was not big enough
**							to hold all items found. In this case, len will
**							be the number of items that would fit into buf.
**
**  SIDE EFFECTS :  none
** **  WARNINGS     :  none
*********************************************************************/
int gfindreg2(fmode,loc,dx,dy,buf,buflen,n,len,vis)
int fmode;			/* 0=part in, 1=part out, 2=all in, 3=all out.*/
Gnpoint *loc;		/* center of search rect*/
Gfloat dx,dy;		/* size of search rect */
int buf[],buflen;	/* output buffer */
int *n;				/* number of items found */
int *len;			/* length of buffer */
int vis;				/* 0=vis, 1=invis, 2=both */
{
	int ug_otone();
	UG_prat3 oldatts;		/* store primitive attributes here */
	int i;
	int segno;				/* Segment number to search */
	UG_segstli *p;
	UG_findprms findprms;
	int foundall;			/* 1=found everything within a single segment */
	int oldindx,olditems;
	UU_LOGICAL vislist_scan;	/* search visible list or all segments? */

	uu_denter(UU_GTRC,(us,
		"gfindreg2(%d,loc=%g %g, dx,dy=%g %g,buflen=%d, vis=%d)",fmode,
					(*loc).x,(*loc).y,dx,dy,buflen,vis));
	irtn=0;
	*len=0;
	*n=0;
	noitems=0;
	buffindx=0;
	bufflen=buflen;
	buff=buf;
	ddx=dx; ddy=dy;

	uu_dprint(UU_GTRC,(us,"gfindreg2 gfindndc2(%d,%g,%g,%g,%g,%d,%d)",
		fmode,(*loc).x,(*loc).y,dx,dy,ug_otone,vis));
	ug_find.x=(*loc).x; ug_find.y=(*loc).y;
	ug_find.find=1;
	ug_find.func=ug_otone;
	ug_find.pikmode=fmode;
	ug_find.vis=vis;
	oldindx= buffindx;
	olditems=noitems;

	uu_dprint(UU_GITRC,(us,"vislistok=%d",ug_vislistok));

	/* if the visible list is ok & not looking for something invis. then TRUE */
	vislist_scan = (!vis && ug_vislistok) ? UU_TRUE : UU_FALSE;

	/* search the visible list (speed picking) or search all segments */
	for( vislist_scan ? i=0 : ug_seginitscan();
		  vislist_scan ? i<UU_IALEN(ug_vislist) : (p=ug_segscan())!=NULL;
		  i++ ) {

		if( vislist_scan )
			segno = UU_IAVAL(ug_vislist, i);
		else 
			segno = p->segid;

		if (segno<UG_MAXSEGNO) {			/* don't look at "fake" icon segments */
			int vlen; int vlst[20];
			ug_find.epsx=dx; ug_find.epsy=dy;
			ug_find.found=0;
			findprms.curxform=ug_gksstli.curvwindex;
			findprms.pkid=ug_gksstli.curprats.pickid;
			zbytecp(findprms.prats,ug_gksstli.curprats);
			ug_ident(findprms.gmodxf);
			vlen=0;
			foundall=ug_findsgpk(segno,&findprms,vlst,&vlen);	/* search segno */

			/* For modes which require all of the segment to be selected,
			 * we need to remove that part of the buffer corresponding to
			 * the partially selected segment. 
			 */
			if(foundall!=1 && (fmode==2 || fmode==3) ) {
				uu_dprint(UU_GITRC,(us,"gfindreg2 removing buf for seg %d",segno));
				uu_dprint(UU_GITRC,(us,"fmode=%d",fmode));
				buffindx=oldindx;	
				noitems=olditems;
			}

			oldindx= buffindx;
			olditems=noitems;
			if (ug_find.find==0) break;
		}
	}

	ug_find.find=0;
	*n=noitems;
	*len=buffindx;
	uu_dprint(UU_GTRC,(us,"gfindreg2 returns %d. n=%d,len=%d",irtn,*n,*len));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  ug_otone(len,segs,find) 
**			Part of gfindreg or gfindreg2. Called once per item found.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_otone(len,segs,find)					/* called once per item */
int len;			/* length of segs */
int segs[];		/* segment nos. */
UG_findit *find;
{
	int i;
	uu_denter(UU_GITRC,(us,"ug_otone(len=%d, segs[0..1]=%d %d),pickid=%d",
				len,segs[0],segs[1],(*find).pickid));
	if ((len+1+buffindx)>=bufflen) {
		/* filled up buffer */
		irtn=1;
		(*find).find=0;
		uu_dexit;
		return;
	}
	/* gets here if buffer not full */
	buff[buffindx]=len+1; 
	buffindx++;	/* put pick depth in buffer*/
	for (i=0; i<len; i++)  
	{
		buff[buffindx]=segs[i]; 
		buffindx++;
	}
	buff[buffindx]=(*find).pickid;
	buffindx++;
	noitems++;				/* bump number of items in buff */
	(*find).epsx=ddx; 
	(*find).epsy=ddy;
	uu_dexit;
}

/**************************************************************************
**  E_FUNCTION   :  gpckm(u, v, pickrec,eps,xform) -- scans segments for pick 
**							entity
**  PARAMETERS   
**      INPUT  : 
**          	Gfloat u,v -- location in ndc space
**					Gfloat eps -- ndc pick aperture half-width.
**					Gint xform -- normtran number, or -1 to look thru all segs.
**		  OUTPUT: pickrec = pick record
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
gpckm(u, v, pickrec,eps,xform)
Gfloat u;							/* u position */
Gfloat v;							/* v position */
Gqpicks *pickrec;					/* pick record to return */
Gfloat eps;							/* pick aperture */
Gint xform;
{
	int depth,i;
	Gnpoint loc;
	static int seg[20];			/* save pickpath */
	uu_denter(UU_GITRC,(us,"gpckm(%g %g, pickrec, aperture=%g, xform=%d)",
			u,v,eps,xform));
	depth = 0;
	loc.x=u; loc.y=v;
	(*(ug_gksstli.wsopen[0].connid)[UG_RPPICK_SEG])(&loc,seg,&depth,eps,xform);
	(*pickrec).pickpath=seg;
	if (depth>0) {						/* found something */
		(*pickrec).status=UG_OK;
		(*pickrec).depth=depth;
	}
	else {							/* didnt find anything */
		(*pickrec).status=UG_NONE;
		(*pickrec).depth=0;
	}
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  gtrav(segno) -- Initialize traversal via gtravnxt.
**  PARAMETERS   
**      INPUT:  int segno -- the segment to traverse, or -1 to
**									  traverse everything.
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  Call once, before calling gtravnxt
****************************************************************/
int gtrav(segno)
int segno;					/* root of subtree to traverse */
{
	uu_denter(UU_GITRC,(us,"gtrav(%d)",segno));
	nseg=segno;
	first=1;
	xmodel=1;				/* apply modelling xform */
	vwindex=ug_gksstli.curvwindex;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_travnx2(type,n,coords,path,depth)
**		Like gtravnxt but no model xform applied to coords.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : normalization transformation number at primitive.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_travnx2(type,n,coords,path,depth)		/* like gtravnxt but no model
															xform applied to coords */
int *type,*n,path[],*depth;
Gfloat (**coords)[3];
{
	xmodel=0;
	return(gtravnxti(type,n,coords,path,depth,1,1));
}

/********************************************************************* 
**  E_FUNCTION:  int gtravnxt(type,n,coords,path,depth)
**		Get next polyline or polymarker.
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: 	int *type -- 0=coordinates are for a polymarker,
**										1=coordinates are for a polyline.
**										-1 = done (no more polylines or markers).
**						** if type is 0 or 1, the following are valid **
**						int *n -- number of coordinates in coords.
**						Gfloat (**coords)[3] -- world coordinates.
**						int *depth -- length of path.
**						int path[] -- contains depth-1 segment numbers,
**										followed by a pickid.
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  Call gtrav once before calls to this function.
*********************************************************************/
int gtravnxt(type,n,coords,path,depth)
int *type;			/* 0=polymarker, 1=polyline, -1=no more */
int *n;				/* number of coordinates */
Gfloat (**coords)[3];	/* address of pointer to a coordinate array */
int path[];			/* segment numbers, pickid */
int *depth;			/* length of path */
{
	gtravnxti(type,n,coords,path,depth,0,0);	/* don't pick UNDETECTABLE 
																or INVISIBLE */
}

/********************************************************************* 
**  I_FUNCTION:  int gtravnxti(type,n,coords,path,depth,det,vis)
**		Get next polyline or polymarker.
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: 	int *type -- 0=coordinates are for a polymarker,
**										1=coordinates are for a polyline.
**										2=coords are for text.
**										3=fillarea.
**										-1 = done (no more polylines or markers).
**						** if type is 0,1 or 3, the following are valid **
**						int *n -- number of coordinates in coords.
**						Gfloat (**coords)[3] -- world coordinates.
**						int *depth -- length of path.
**						int path[] -- contains depth-1 segment numbers,
**										followed by a pickid.
**						int det -- 0=don't return UNDETECTABLE, 1=do return them.
**						int vis -- 0=don't return INVISIBLE, 1=do return them.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  Call gtrav once before calls to this function.
*********************************************************************/

/* Stack definition -- for recursive calls to segments */
typedef struct {
	Gtran modxf;
	int segid;
	int pkid;
	int icmd;
	int ncmd;
} state;

static UG_FBS_STACK( STK, 20, state );

int gtravnxti(type,n,coords,path,depth,det,vis)

int *type;					/* 0=polymarker, 1=polyline, 2=text, 3=fillarea
									-1=no more */
int *n;						/* number of coordinates */
Gfloat (**coords)[3];	/* coordinate array */
int path[];					/* segment numbers, pickid */
int *depth;					/* length of path */
int det;						/* 0=don't return UNDETECTABLE, 1=do */
int vis;						/* 0=don't return INVISIBLE, 1=do */
{

	/* Local variables which must be pushed for recursive calls */
	Gtran modxf;					/* Segment modeling transformation */
	int segid;						/* Number of current segment */
	int ncmd;						/* Number of commands in the segment */
	int icmd;						/* Current command in a segment */

	/* Other local variables */
	state *stkptr;					/* pointer to top of stack */
	int initcmd;					/* initial command in segment traversal */
	int curseg;						/* current segment during traversal */
	int found;						/* flag showing graphics found */
	int i, m, opcode, ptr;
	Gfloat a[4][4];
	UG_plylna3op *cmd;
	UG_segstli *segptr;
	int *p, cmdlen;
	char *ug_lsielt();					/* Returns next list element */
	int newlen;								/* New coords length */
	static int coordlen=0;				/* Current length of coords */
/*
.....Changed from 100 to 200, because
.....Doing a Chain select with a POLYLINE entity
.....displayed on the screen with more than 100
.....entities would crash NCL
.....Bobby  -  10/14/94
*/
	static Gfloat lcoords[200][3];	/* Coords if n<200 */
	static Gfloat *lastcoords=NULL;	/* Value of coords at previous call */
   Gwpoint3 *Getpoints3();
	Gwpoint *Getpoints();

/* This macro used to set coords to buffer of proper size.  Uses lcoords
 * unless segment has more than 200 elements.  Mallocs for larger segs.
 */
#define ALOCCOORDS(seglen) \
	uu_dprint(UU_GTRC,(us,"seglen=%d, coordlen=%d", seglen, coordlen)); \
	newlen = (seglen) > 200 ? (seglen) : 200; \
	uu_dprint(UU_GTRC,(us,"newlen=%d",newlen)); \
	uu_dprint(UU_GTRC,(us,"coords %x, lcoords %x", coords, lcoords)); \
	if( newlen != coordlen ) { \
		if( coordlen > 200 ) {	/* Free previously allocated coords */ \
			uu_dprint(UU_GTRC,(us,"freeing %x", coords)); \
			uu_toolfree(coords); \
		} \
		coordlen = newlen;	/* Save new coordlen */ \
		if( coordlen > 200 ) {	/* Allocate new coords */ \
			uu_dprint(UU_GTRC,(us,"mallocing %d", coordlen*3*sizeof(Gfloat))); \
			*coords = (Gfloat (*)[3])uu_toolmalloc(coordlen*3*sizeof(Gfloat)); \
		} \
		else \
			uu_dprint(UU_GTRC,(us,"coords = lcoords =  %x", lcoords)); \
			*coords = lcoords; 	/* Use statically allocated buffer */ \
	}


	uu_denter(UU_GITRC,(us,"gtravnxti()"));

	/* If first call, initialize stack and loop variables */
	if (first==1) {				/* first time called after gtrav */

		ug_fbs_init(STK, 20, sizeof(state));			/* initialize stack */
		uu_denter2(UU_GITRC,(us,"stack initialized")); uu_dexit;

		/* Get storage for top of stack */
		stkptr = ug_fbs_push(STK);
		if( stkptr == NULL ){
			uu_denter2(-1,(us,"gtravnxti overflowed stack"));
			uu_dexit;
		}

		/* initcmd =  first command in the segment */
		ug_seginitscan();
		initcmd = 0;

		/* current seg = first segment */
		if (nseg<0) segid = ug_nxtseg();
		else segid=nseg;
		segptr=ug_segac(segid);

		/* ncmd = number of commands in the segment */
		ncmd = ug_lsinelt(segptr->seglist);

		/* Initialize modeling xform to identity */
		ug_ident(modxf);

		/* Initialize stack */
		stkptr->segid = segid;
		stkptr->ncmd = ncmd;
		stkptr->icmd = initcmd;
		ug_mcopy(stkptr->modxf, modxf);
	}

	/* If not first call, recover previous state from stack */
	else{
		stkptr  = ug_fbs_top(STK);
		segid   = stkptr->segid;
		ncmd    = stkptr->ncmd;
		initcmd = stkptr->icmd;
		ug_mcopy(modxf,stkptr->modxf);
	}

	if( lastcoords ) uu_toolfree(lastcoords);		/* Free last calls malloc */
	*coords = lcoords;									/* Init coords */
	coordlen=0;

	uu_dprint(UU_GITRC,(us,"top of for each segment loop"));

	/* For each segment */
	for( curseg = segid; curseg >= 0; curseg = ug_nxtseg() ){

		first=0;
		segptr=ug_segac(curseg);

		if( curseg<UG_MAXSEGNO  &&							/* if user graphic seg	*/
			 ((((*segptr).segatts.gvis==UG_VISIBLE)||(vis==1)) &&	/* and visible*/
			 (((*segptr).segatts.gdtect==UG_DETECTABLE) /* and detectable, 	*/
				||(det==1))) ) {								/* then traverse it 		*/

		uu_denter2(UU_GITRC,(us,"segment is visible and detectable")); uu_dexit;
		
			/*
			* segment curseg is VISIBLE and DETECTABLE traverse it, returning
			*	at each graphics call (FOUND = TRUE)
			*/

			/* ncmd = number of commands in the segment */
			ncmd = ug_lsinelt(segptr->seglist);

			/* if we just finished a segment and found nothing and that segment
				had no caller, the stack will be empty here (because it got
				popped at end of for each cmd loop. */
			if (stkptr==NULL) {
				stkptr = ug_fbs_push(STK);
				stkptr->segid = segid;
				stkptr->ncmd = ncmd;
				stkptr->icmd = initcmd;
				ug_mcopy(stkptr->modxf, modxf);
			}	
			/* for each command in curseg starting where we left off */
			uu_denter2(UU_GITRC,(us,"for each command in seg")); uu_dexit;

			for( icmd = initcmd; icmd < ncmd; ++icmd ){

				cmd = (UG_plylna3op *)ug_lsielt(segptr->seglist,icmd);
				cmdlen = ug_lsilen(segptr->seglist,icmd)/sizeof(int);
				/*for (i=0; i<cmdlen; i++) (*cmd).gclist[i]=p[i];*/
				opcode=(*cmd).elttype;
				ptr=0;

				/*
				* Set bit 8, flag indicating pointers are stored rather than
				*  values.  This won't work!!!
				*/
				if (opcode<=UG_OPPTR) {
					if ((opcode&UG_OPPTR)!=0) ptr=1;
					opcode=opcode&(UG_OPPTR-1);
				}

				ptr = (((*cmd).elttype&UG_OPPTR)!=0) ? 1 : 0;
				uu_denter2(UU_GITRC,(us,"gtravnxti: opcode=%d, ptr=%d, seg=%d, stkptr=%x",
					opcode,ptr, segid, stkptr));
				uu_dexit;

				found = 0; 						/* Set found = false */

				switch(opcode) {

				case UG_PLYLNA3OP: 			/* found a polyline */
					if ((segptr->segatts.gdtect==UG_UNDETECTABLE)
							&&(xmodel==1)) break;
					*type=1;
					*n=(*(UG_plylna3op *)cmd).len;
					ALOCCOORDS(*n); 
											/* xform coords*/
					gtravxfm(*n,*coords,
						temp3=Getpoints3((*(UG_plylna3op *)cmd).pts,*n));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
					uu_toolfree(temp3);
#endif
					found = 1;
					break;
	
				case UG_PLYLNA2OP: 			/* found a polyline */
					if ((segptr->segatts.gdtect==UG_UNDETECTABLE)
							&&(xmodel==1)) break;
					*type=1;
					*n=(*(UG_plylna2op *)cmd).len;
					ALOCCOORDS(*n);
											/*xform coords*/
					gtravxfm2(*n,*coords,
						temp=Getpoints((*(UG_plylna2op *)cmd).pts,*n));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
					uu_toolfree(temp);
#endif
					found = 1;
					break;
	
				case UG_PLYMKA3OP:		/* found a polymarker */
					if ((segptr->segatts.gdtect==UG_UNDETECTABLE) && (xmodel==1)) break;
					*type=0;
					*n=(*(UG_plymka3op *)cmd).len;
					ALOCCOORDS(*n);
													/* xform coords*/
					gtravxfm(*n,*coords,
						temp3=Getpoints3((*(UG_plymka3op *)cmd).pts,*n));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
					uu_toolfree(temp3);
#endif
					found = 1;
					break;
	
				case UG_PLYMKA2OP:		/* found a polymarker */
					if ((segptr->segatts.gdtect==UG_UNDETECTABLE) && (xmodel==1)) break;
					*type=0;
					*n=(*(UG_plymka2op *)cmd).len;
					ALOCCOORDS(*n);
															/* xform coords*/
					gtravxfm2(*n,*coords,
						temp=Getpoints((*(UG_plymka2op *)cmd).pts,*n));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
					uu_toolfree(temp);
#endif
					found = 1;
					break;

				case UG_FLAREA3OP: 			/* found a 3D fillarea */
					if ((segptr->segatts.gdtect==UG_UNDETECTABLE)
							&&(xmodel==1)) break;
					*type=3;
					*n=(*(UG_flarea3op *)cmd).len;
					ALOCCOORDS(*n);
															/* xform coords*/
					gtravxfm(*n,*coords,
						temp3=Getpoints3((*(UG_flarea3op *)cmd).pts,*n));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
					uu_toolfree(temp3);
#endif
					found = 1;
					break;

				case UG_FLAREAOP:		/* found a 2D fillarea */
					if ((segptr->segatts.gdtect==UG_UNDETECTABLE) 
						&& (xmodel==1)) break;
					*type=3;
					*n=(*(UG_flareaop *)cmd).len;
					ALOCCOORDS(*n);
					/* xform coordinates*/
					gtravxfm2(*n,*coords,
						temp=Getpoints((*(UG_flareaop *)cmd).pts,*n));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
					uu_toolfree(temp);
#endif
					found = 1;
					break;
	
				case UG_TEXTOP:				/* text */
					if ((segptr->segatts.gdtect==UG_UNDETECTABLE)&&(xmodel==1)) 
						break;
					*type=2;
					*n=1;
					ALOCCOORDS(*n);
					gtravxfm(*n,*coords,
						Getpoints3(&(*(UG_textop *)cmd).position,1));  /* xform coords */
					found=1;
					break;

				case UG_PICKIDOP:		/* pick id */
					stkptr->pkid = (*(UG_pickidop *)cmd).pid;
					uu_denter2(UU_GITRC,(us,"pkid op = %d",stkptr->pkid));
					uu_dexit;
					break;
	
				case UG_CALLOP:			/* call another segment */
					m = (*(UG_callop *)cmd).segno;		/* segment to call */

					if (ug_segac(m)->segatts.gvis==UG_INVISIBLE) 
						break;
					if ((ug_segac(m)->segatts.gdtect==UG_UNDETECTABLE)&&(xmodel==1)) 
						break;
	
					/*
					*  Segment is VISIBLE and DETECTABLE, save current state, and
					*	traverse called segment
					*/
					stkptr->icmd = icmd;
					stkptr->ncmd = ncmd;
					stkptr->segid = curseg;
					ug_mcopy(stkptr->modxf,modxf);

					/* Push to get new stack element */
					stkptr = ug_fbs_push(STK);

					/* Go to beginning of this segment */
					curseg = m;
					icmd = 0; initcmd = 0;
					segptr = ug_segac(curseg);
					ncmd = ug_lsinelt(segptr->seglist);
	
					/* Multiply old modxform by this seg xform */
					/*if (ug_mident( segptr->segatts.gsgxfm) != 1 ) {
					/*	ug_mcopy(a,modxf);
					/*	ug_matmp(modxf,a,segptr->segatts.gsgxfm);
					/*} */
	
					break;
	
				case UG_SNTRANOP:
					vwindex=(*(UG_sntranop *)cmd).xform;
					break;

				case UG_MTRANOP:
					switch((*(UG_mtranop *)cmd).type) {
					case UG_PRECONCATENATE:
						ug_matmp(a,(*(UG_mtranop *)cmd).xf,modxf);
						ug_mcopy(modxf,a);
						break;
					case UG_POSTCONCATENATE:
						ug_matmp(a,modxf,(*(UG_mtranop *)cmd).xf);
						ug_mcopy(modxf,a);
						break;
					case UG_MODREPLACE:
		/* Do ptr assignment if same precision, individual element copy if not. */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
						ug_ItoGcopy(modxf,(*(UG_mtranop *)cmd).xf);
#else
						ug_mcopy(modxf,(*(UG_mtranop *)cmd).xf);
#endif
						break;
	
					default: 
						uu_denter2(-1,(us,"gtravnxti bad modeling option %d",
						(*(UG_mtranop *)cmd).type)); uu_dexit;
						break;
					}

					break;

				}							/* end of switch(opcode) */

				/* If found something, save current state onto stack and return */
				if( found ){
					stkptr->icmd  = icmd+1;
					stkptr->ncmd  = ncmd;
					stkptr->segid = curseg;
					ug_mcopy(stkptr->modxf,modxf);

					/* depth is the pick depth (#segments deep at this pick) */
					*depth = ug_fbs_depth(STK);
	
					/* Create path, the path of segments leading to this pick */
					for(i=0; i < *depth; ++i){
						stkptr  = ug_fbs_ielem(STK,i);
						path[i] = stkptr->segid;
					}
					path[*depth] = stkptr->pkid;
					*depth += 1;
	
					uu_dprint(UU_GITRC,
						(us,"gtravnxti returns type=%d, n=%d, depth=%d",
							*type,*n,*depth)); 
					uu_dprint(UU_GITRC,(us,
							"gtravnxti returns coords=%g %g %g, path=%d %d",
							(*coords)[0][0],(*coords)[0][1],(*coords)[0][2],
							path[0],path[1])); 

/*					uu_dexit; */
/*					return(vwindex); */
					goto exit;
				}

			}								/* end of for each command */

		}									/* end of if visible and detectable */

		/*
		* If at last command of a segment, pop stack and return to
		* calling segment.
		*/
	
		if( icmd == ncmd-1 ){
			stkptr = ug_fbs_pop(STK);
			if( stkptr != NULL ){
				icmd   = stkptr->icmd;
				ncmd   = stkptr->ncmd;
				curseg = stkptr->segid;
				ug_mcopy(modxf, stkptr->modxf);
				segptr = ug_segac(curseg);
			}
		}

		initcmd = 0;				/* At end of segment, reinitialize for next */
		if (nseg>=0) break;		/* if we are only supposed to traverse 1 segment,
											quit now */
	}									/* end of for each segment */

	/* At this point, all segments exhausted, nothing found */

	*type = -1;
	*n = 0;
	*depth = 0;

exit:
	/* Save coords pointer so we can free on next call */
	if( coordlen > 200 )
		lastcoords = **coords;
	else
		lastcoords = NULL;

	uu_dexit;
	return(vwindex);
}

/*********************************************************************
**    I_FUNCTION     :  int gtravxfm(n,b,a)
**			b = a transformed by modeling transform on top of stack.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtravxfm(n,b,a)		/*b=a xformed by modeling transform on top of stack */
int n;						/* number of coordinates */
Gfloat a[][3],b[][3];
{
	int i,j;
	state *stkptr;

	uu_denter(UU_GITRC,(us,"gtravxfm(%d,b,%g %g %g, %g %g %g)",
				n,a[0][0],a[0][1],a[0][2],a[1][0],a[1][1],a[1][2]));

	stkptr = ug_fbs_top(STK);
	for (i=0; i<n; i++) {
		if (xmodel==1)
		ug_xform(a[i][0],a[i][1],a[i][2],&b[i][0],stkptr->modxf);
		else {
			b[i][0]=a[i][0]; b[i][1]=a[i][1];  b[i][2]=a[i][2];
		}
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int gtravxfm2(n,b,a) 
**			b = a(2d) xformed by modeling transform on top of stack.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gtravxfm2(n,b,a)
int n;							/* number of coordinates */
Gfloat a[][2],b[][3];
{
	int i,j;
	state *stkptr;

	uu_denter(UU_GITRC,(us,"gtravxfm2(%d,b,%g %g, %g %g)",
				n,a[0][0],a[0][1],a[1][0],a[1][1]));

	stkptr = ug_fbs_top(STK);
	for (i=0; i<n; i++) {
		if (xmodel==1)
		ug_xform(a[i][0],a[i][1],(UU_REAL) 0.,&b[i][0],stkptr->modxf);
		else {
			b[i][0]=a[i][0]; b[i][1]=a[i][1]; b[i][2]=0.;
		}
	}
	uu_dexit;
}
/********************************************************************* 
**  E_FUNCTION:  int gfindclip(buf,buflen,n,len)
**      Find all graphics primitives within a clip plane region
**  PARAMETERS   
**      INPUT:  
**					 int buflen -- length of buf.
**      OUTPUT: int *n-- number of items found.
**				int *len -- length of the items in buf.
**				int buf[] -- buffer to hold items. Each item consists of:
**								depth of this item.
**								depth-1 segment numbers.
**							   pick-id of item.
**					The first item starts at buf[0]. Subsequent items
**					follow each other contiguously in buf.
**
**  RETURNS      :  0 if all went OK, 1 if buf was not big enough
**							to hold all items found. In this case, len will
**							be the number of items that would fit into buf.
**
**  SIDE EFFECTS :  none
** **  WARNINGS     :  none
*********************************************************************/
int gfindclip(buf,buflen,n,len)
int buf[],buflen;	/* output buffer */
int *n;				/* number of items found */
int *len;			/* length of buffer */
{
	UG_prat3 oldatts;		/* store primitive attributes here */
	int i;
	int segno;				/* Segment number to search */
	UG_segstli *p;
	int foundall;			/* 1=found everything within a single segment */
	int oldindx,olditems;
	UU_LOGICAL vislist_scan;	/* search visible list or all segments? */
	UG_findprms findprms;

	irtn=0;
	*len=0;
	*n=0;
	noitems=0;
	buffindx=0;
	bufflen=buflen;
	buff=buf;

	ug_find.find=1;
	ug_find.func=ug_otone;
	oldindx= buffindx;
	olditems=noitems;

	ug_seginitscan();

	while( (p=ug_segscan()) != NULL ) 
	{
		segno = p->segid;
		ug_find.found=0;
		if (segno<UG_MAXSEGNO) 
		{
			int vlen; int vlst[20];
			ug_find.found=0;
			findprms.curxform=ug_gksstli.curvwindex;
			findprms.pkid=ug_gksstli.curprats.pickid;
			zbytecp(findprms.prats,ug_gksstli.curprats);
			ug_ident(findprms.gmodxf);
			vlen=0;
			foundall = ug_findsgclip(segno,&findprms,vlst,&vlen);
			if (foundall!=1) 
			{
				buffindx=oldindx;	
				noitems=olditems;
			}
			oldindx= buffindx;
			olditems=noitems;
			if (ug_find.find==0) break;
		}
	}
	ug_find.find=0;
	*n=noitems;
	*len=buffindx;
	return(irtn);
}
