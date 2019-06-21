/*********************************************************************
**    NAME         :  gctrl.c -- DIGS control functions.
**       CONTAINS:
**		Gerror gopengks(mem,errfile)  -- Open GKS.
**		Gerror gclosegks() -- Close GKS.
**		Gws *gopenws(conn,connid) -- Open a GKS workstation.
**		Gerror gclosews(id) -- Close GKS workstation
**		Gerror gactivatews(ws) -- activate workstation.
**		Gerror gdeactivatews(ws) -- deactivage workstation.
**		Gerror gclearws(ws,clearflag) --  Clear workstation.
**		Gerror gredrawsegws(ws) --  Redraw all segments on workstation.
**		Gerror gredrawsegs() -- redraw all segs on all workstations.
**		Gerror gredrawsegview(xform) -- redraw segs in a particular view.
**		ug_dredrawsegrect(segrect,segno)--redraw segs, clip to segrect.
**    ug_drectunion(resultbox,box1,box2) -- union of 2  rectangles.
**    int ug_drectintersect(resultbox,box1,box2) -- intersection of 2  rects.
**    int ug_drectintersect2(resultbox,box1,box2) -- intersection of 2 2-D rects
**		Gerror gupdatews(ws,regenflag) -- update workstation.
**		Gerror gsdus(ws,defmode,modmode) -- set display update state.
**		Gerror gmessage(wkid,msg) -- message on workstation.
**		Gchar *gescape(function,data) -- escape.
**		gprompt(ws,loc,string,num) -- char prompt on workstation.
**		gdnprompt(ws,num) -- take down a prompt on a workstation. 
**		ug_dredraw() -- redraw all segs  on a workstation.
**		ug_dredrawvis() -- redraw all visible segs  on a workstation.
**		int ug_wsoff(ws) -- disable all but 1 ws.
**		int ug_wson() -- reactivate disabled workstations.
**		gwkin(wsid,parms,reply) 
**		ug_wkcal(wsid,parms,reply) -- call specified ws.
**		ug_wkout(prms,len) -- call output wkstns with no reply.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gctrl.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:18
*********************************************************************/
#define GMAIN 1
#include <stdio.h>
#include "usysdef.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gerror.h"
#include "ginq.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gfont.h"
#include "udebug.h"
#include "gsegac.h"
#include "jplot.h"
#define UG_FALSE 0
#define UG_TRUE 1
Gerror ug_wschk(),ug_chkad();

extern int NCL_mot_seg;

void ug_wkcal(),ug_wkout();

/*********************************************************************
**    E_FUNCTION         : Gerror gopengks(mem,errfile)  -- Open GKS.
**    PARAMETERS   
**       INPUT  : long mem--number of bytes of memory available for
**								GKS to use. 
**						FILE *errfile -- an open file on which GKS error
**								messages will be printed.
**       OUTPUT :  
**    RETURNS      : NCL_NO_ERROR if all went ok. Else a GKS error message.
**    SIDE EFFECTS : none
**    WARNINGS     : Call this once before any other GKS routines.
*********************************************************************/
Gerror gopengks(mem,errfile)		/* open GKS */
/*$ INPUT */
Glong mem;			/* bytes memory available for use by GKS */
FILE *errfile;			/* error messages go to this file */
{
	Gerror irtn;
	int i;
	Gerror ug_chkerrfile();
	uu_denter(UU_GTRC,(us," gopengks(%d,errfile)",mem));
	ug_gksos.erfile=errfile;				/* get error file */
	irtn=NCL_NO_ERROR;
	ug_gksos.erfile=errfile;	/* get error file */
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	irtn=ug_chkerrfile("gopengks");
	if (ug_gksos.sysstate!=UG_GKCL) {
		ug_errorhand(ENOTGKCL,"gopengks",NULL); 
		irtn=ENOTGKCL;
	}
	else {
#endif
		uu_toolmalloc_init();			/* init the tool storage manager */
		/* initialize gks operating state */
		ug_gksos.sysstate=UG_GKOP;
		ug_rratio=(sizeof(Gfloat))/(sizeof(int));	/* number integers in a float */
		ug_pratio=(sizeof(int *))/(sizeof(int));	/* no. integers in a pointer */
		ug_find.find=UG_FALSE;
		ug_gksstli.novtran=UG_MAXNTRAN;			/* number of viewing xforms */
		for (i=0; i<ug_gksstli.novtran; i++) {		/* init all viewing xforms*/
			ug_ident(ug_vxform[i]);				/* init view matrix */
			ug_ident(ug_cxform[i]);				/* init composite matrix */
			ug_ident(ug_cxinv[i]);  			/* init cxform inverse matrix */
			ug_cxchg[i]=UG_FALSE;
			ug_gksstli.vtran[i]=ug_defvtran;	
		}
		ug_ident(ug_modxform);
		ug_ident(ug_lmodxform);
		ug_lmodidnt=1;					/* ug_lmodxform is identity */
		ug_ndcmax[0]=1.0; ug_ndcmax[1]=1.0; ug_ndcmax[2]=400.0;
		ug_ndcset=UG_FALSE;
		/* initialize gks state list */
		ug_gksstli.curvwindex=0;		/* current view xform */
		/* set prim atts to defaults*/
		zbytecp(ug_gksstli.curprats,ug_gksdesctbl.defprats);
		ug_gksstli.nowsopen=0;	/* number of open workstations */
		ug_gksstli.nsegstli=0;	/* number of segment state lists */
		for (i=0; i<ug_gksdesctbl.maxopws; i++) {	/* mark all workstations closed */
			ug_gksstli.wsopen[i].connid=NULL;
		}
		ug_segacinit();		/* initialize segment storage */
#ifdef UU_CHECK
	}
#endif
	ug_ndcseg=0;				/* no workstation needs NDC segments */
	UG_fb_h = UU_NULL;            /* init font control blocks to none */
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION         :  Gerror gclosegks() -- Close GKS.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : NCL_NO_ERROR if all went OK. Else one of the
**							GKS error message numbers
**    SIDE EFFECTS : none
**    WARNINGS     : No other calls to GKS allowed after this one.
*********************************************************************/
Gerror gclosegks()					/* close GKS */
{
	 uu_denter(UU_GTRC,(us,"gclosegks()"));
#ifdef UU_CHECK
if (ug_gksos.sysstate!=UG_GKOP) {
		ug_errorhand(ENOTGKOP,"gclosegks",NULL); uu_dexit; return(ENOTGKOP);
	}
#endif
	ug_gksos.sysstate=UG_GKCL;
	ug_segacterm();						/* free segment storage */
	uu_toolmalloc_term();				/* terminate the tool storage manager */
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    E_FUNCTION         :  Gws *gopenws(conn,connid) -- Open a GKS workstation.
**    PARAMETERS   
**       INPUT  : Gchar *conn -- file name for UG_MI or UG_MO type workstations.
**						UG_wsenttbl *connid -- address of workstation entry table.
**       OUTPUT : none 
**    RETURNS      : A pointer to the workstation if all was OK.
**							Else NULL.
**    SIDE EFFECTS : none
**    WARNINGS     : Must open workstation before any output goes to it.
*********************************************************************/
Gws *gopenws(conn,connid)  /* initialize a workstation */
/*$ INPUT */
Gchar *conn;   		/* connection - a file name for UG_MI or MO */
UG_wsenttbl connid;   /* workstation entry point table */
							/* should be char string name of workstation*/
{ int i,k;
	Gws *irtn;
	Gwscat cat;
  struct {int op; int wsid; Gchar *conn;} prms;
  UG_rwsinit reply;
  UG_outwssl *p;
  UG_inwssl *inwsslpt; 
  UG_inwdt *inwdtpt;
  UG_wssl *wsslptr;
  Gwpoint *pts;			/* pointer to storage for stroke points */
/*  char us[200];*/
#ifdef UU_CHECK
	ug_chkad(conn,"gopenws conn");
	ug_chkad(connid,"gopenws connid");
#endif
	 uu_denter2(UU_GTRC,(us," gopenws(%s,%d)",conn,connid));
	k=(-1);
#ifdef UU_CHECK
	if (ug_gksos.sysstate==UG_GKCL) {
		ug_errorhand(ENOTGWWS,"gopenws",NULL); 
	}
	else {
#endif
		for (i=0; i<ug_gksdesctbl.maxopws; i++) {
			if (ug_gksstli.wsopen[i].connid==NULL) {k=i; break;}
		}
		if (k<0) {
			ug_errorhand(EWSCNTOP,"gopenws",NULL); irtn=NULL;
		}
		else {
			wsslptr= &(ug_gksstli.wsopen[k]);		/* pointer to wssl */
			(*wsslptr).id=k;
			(*wsslptr).state=UG_INACTIVE;
			(*wsslptr).connid=(Gint (**)())connid;
  			prms.op=UG_DOPENWS; prms.wsid=k;
  			prms.conn=conn;
  			/* initialize has one conn-id parm. */
			(*(connid)[UG_DOPENWS])(&prms,&reply);	/* call ws init routine */
			ug_gksstli.nowsopen++;			/* bump number of open workstations */
			(*wsslptr).wdtptr=reply.wdtptr;
			cat=(*reply.wdtptr).category;
			uu_dprint(UU_GTRC,(us,"gopenws. type=%s, category=%d\n",
						(*reply.wdtptr).type,cat));
			(*wsslptr).wsct.type=(*reply.wdtptr).type; /* save workstation type */
			if ((*reply.wdtptr).needndc!=0) {	/* does this ws need NDC boxes */
				ug_ndcseg++;							/* no. ws needing NDC boxes */
			}
			/* if categor UG_MI or UG_MO, open file */
			if (cat==UG_MI) 
				(*wsslptr).wsct.r=fopen(conn,"r");	/* open for reading */
			if (cat==UG_MO) 
				(*wsslptr).wsct.w=fopen(conn,"w");	/* open for writing */
			(*wsslptr).outptr=NULL;
			/* if workstation of type OUTIN, OUTPUT, or MO, allocate and 
				initialize space for output part of wssl */
			if ((cat==UG_OUTIN)||(cat==UG_OUTPUT)||(cat==UG_MO)) {
				struct {Gint op; Gws wsid; Gnrect3 window;} prms;
				int rply[4];
				p=(UG_outwssl *)(uu_toolmalloc(sizeof(UG_outwssl)));
				if (p==NULL) {
					ug_errorhand(EMEMSPAC,"gopenws",NULL);
					uu_dexit; return(NULL);
				}
				(*wsslptr).outptr=p;
				zbytecp((*p).curxform,ug_defwsxform);		/* structure assignment */
				(*p).curxform.v.urb.x=(*reply.wdtptr).dspsize.device.x;
				(*p).curxform.v.urb.y=(*reply.wdtptr).dspsize.device.y;

				/* deferral mode */
				(*p).defmode=(*(*reply.wdtptr).outwdtpt).defdef;	

				/* modification mode */
				(*p).modmode=(*(*reply.wdtptr).outwdtpt).defmod;

				/* HLHSR update state and modes */
				p->hlhsrupdate = 0; 		/* HLHSR update state (0=notpending) */
				p->hlhsrmode = 0;			/* Current hlhsr mode */
				p->rhlhsrmode = 0;		/* Requested hlhsr mode */

				(*p).dspsurf=UG_EMPTY;				/* display surf empty */
				(*p).nframe=UG_NO;					/* no newframe action on update*/
				/* (*p).curxform.v.urb.z=1.; */
				/* call workstation's UG_DWSWIND entry to initialize the ws xform */
				/* don't use ug_wkout since ws isn't active yet */
				zbytecp(prms.window,ug_defwsxform.w);		/* structure assignment */
  				prms.op=UG_DWSWIND;
				prms.wsid= k;
				(*(ug_gksstli.wsopen[k].connid)[prms.op])(&prms,rply);

				zbytecp((*p).reqxform,(*p).curxform);	/* structure assignment */
			}
			/* if workstation is of type INPUT or OUTIN, allocate and
				initialize space for input part of wssl */
			if ((cat==UG_INPUT)||(cat==UG_OUTIN)) {
				inwsslpt=(UG_inwssl *)(uu_toolmalloc(sizeof(UG_inwssl)));
				if (inwsslpt==NULL) {
					ug_errorhand(EMEMSPAC,"gopenws",NULL);
					uu_dexit; return(NULL);
				}
				(*wsslptr).inptr=inwsslpt;
				inwdtpt=(*reply.wdtptr).inwdtpt;	/* pointer to INPUT wdt data */
				/* obtain number of each device type from wdt */
				(*inwsslpt).nloc=(*inwdtpt).nloc;
				(*inwsslpt).nstroke=(*inwdtpt).nstroke;
				(*inwsslpt).nval=(*inwdtpt).nval;
				(*inwsslpt).nchoice=(*inwdtpt).nchoice;
				(*inwsslpt).npick=(*inwdtpt).npick;
				(*inwsslpt).nstring=(*inwdtpt).nstring;

				/* allocate and initialize space in UG_inwssl 
				 * for locator device states */
				(*inwsslpt).locdata = 
					(Glocst *)uu_toolmalloc((*inwsslpt).nloc*sizeof(Glocst));
					if (inwsslpt==NULL) {
						ug_errorhand(EMEMSPAC,"gopenws",NULL);
						uu_dexit; return(NULL);
					}

				/* initialize loc devices */
				for (i=0; i<(*inwsslpt).nloc; i++) {	
					(*inwsslpt).locdata[i].mode=UG_REQUEST; 
					(*inwsslpt).locdata[i].esw=UG_ECHO;
					(*inwsslpt).locdata[i].loc.transform=0;
					(*inwsslpt).locdata[i].pet=1;
					zbytecp((*inwsslpt).locdata[i].loc.position,
								(*inwdtpt).defloc[i].position);
					zbytecp((*inwsslpt).locdata[i].e_area,
								(*inwdtpt).defloc[i].e_area);
					zbytecp((*inwsslpt).locdata[i].record,
								(*inwdtpt).defloc[i].record); 
				}

				/* allocate storage for stroke state records  */
				(*inwsslpt).strokedata =
					(Gstrokest *)uu_toolmalloc((*inwsslpt).nstroke*sizeof(Gstrokest));
				/* alloc storage for max number of points in each state record*/
				pts=(Gwpoint *)uu_toolmalloc(
					(*inwsslpt).nstroke*UG_MAXSTROKEPTS*sizeof(Gwpoint));
				if (((*inwsslpt).strokedata==NULL)||(pts==NULL)) {
					ug_errorhand(EMEMSPAC,"gopenws",NULL);
					uu_dexit; return(NULL);
				}
				
				/* initialize stroke devices */
				uu_denter2(UU_GTRC,(us,"nstroke =%d",inwsslpt->nstroke));
				uu_dexit;
				for (i=0; i<(*inwsslpt).nstroke; i++) {
					(*inwsslpt).strokedata[i].mode=UG_REQUEST;
					(*inwsslpt).strokedata[i].esw=UG_ECHO;
					/* initialize stroke data */
					(*inwsslpt).strokedata[i].stroke.n_points=0;
					(*inwsslpt).strokedata[i].stroke.transform=0;
					(*inwsslpt).strokedata[i].stroke.points=pts;
					/* bump pointer to next array of points */
					pts=pts+UG_MAXSTROKEPTS;	/* we assume here that "C" multiplies
														by sizeof(Gwpoint) */
					(*inwsslpt).strokedata[i].pet=1;
					zbytecp((*inwsslpt).strokedata[i].e_area,
							(*inwdtpt).defstk[i].e_area);
					zbytecp((*inwsslpt).strokedata[i].record,
						(*inwdtpt).defstk[i].record); 
				}

				/* allocate storage for valuator state records */
				(*inwsslpt).valdata = 
					(Gvalst *)uu_toolmalloc((*inwsslpt).nval*sizeof(Gvalst));
				if ((*inwsslpt).valdata==NULL) {
					ug_errorhand(EMEMSPAC,"gopenws",NULL);
					uu_dexit; return(NULL);
				}
				/* initialize valuator devices */
				for (i=0; i<(*inwsslpt).nval; i++) {
					(*inwsslpt).valdata[i].mode=UG_REQUEST;
					(*inwsslpt).valdata[i].esw=UG_ECHO;
					(*inwsslpt).valdata[i].val=(*inwdtpt).defval[i].value;
					(*inwsslpt).valdata[i].pet=1;
					zbytecp((*inwsslpt).valdata[i].e_area,
							(*inwdtpt).defval[i].e_area);
					zbytecp((*inwsslpt).valdata[i].record,
							(*inwdtpt).defval[i].record); 
				}

				(*inwsslpt).choicedata = 
					(Gchoicest *)uu_toolmalloc((*inwsslpt).nchoice*sizeof(Gchoicest));
				if ((*inwsslpt).choicedata==NULL) {
					ug_errorhand(EMEMSPAC,"gopenws",NULL);
					uu_dexit; return(NULL);
				}

				/*  initialize choice devices */
				for (i=0; i<(*inwsslpt).nchoice; i++) {
					(*inwsslpt).choicedata[i].mode=UG_REQUEST;
					(*inwsslpt).choicedata[i].esw=UG_ECHO;
					(*inwsslpt).choicedata[i].choice=0;
					(*inwsslpt).choicedata[i].pet=1;
					zbytecp((*inwsslpt).choicedata[i].e_area,
							(*inwdtpt).defcho[i].e_area);
					zbytecp((*inwsslpt).choicedata[i].record,
							(*inwdtpt).defcho[i].record); 
				}

				(*inwsslpt).pickdata = 
					(Gpickst *)uu_toolmalloc((*inwsslpt).npick*sizeof(Gpickst));
				if ((*inwsslpt).pickdata==NULL) {
					ug_errorhand(EMEMSPAC,"gopenws",NULL);
					uu_dexit; return(NULL);
				}

				/* initialize pick devices */
				for (i=0; i<(*inwsslpt).npick; i++) {
					(*inwsslpt).pickdata[i].mode=UG_REQUEST;
					(*inwsslpt).pickdata[i].esw=UG_ECHO;
					(*inwsslpt).pickdata[i].pet=1;
					zbytecp((*inwsslpt).pickdata[i].e_area,
							(*inwdtpt).defpick[i].e_area);
					zbytecp((*inwsslpt).pickdata[i].record,
							(*inwdtpt).defpick[i].record); 
				}

				(*inwsslpt).stringdata = 
					(Gstringst *)uu_toolmalloc((*inwsslpt).nstring*sizeof(Gstringst));
				if ((*inwsslpt).stringdata==NULL) {
					ug_errorhand(EMEMSPAC,"gopenws",NULL);
					uu_dexit; return(NULL);
				}

				/* initialize string devices */
				for (i=0; i<(*inwsslpt).nstring; i++) {
					(*inwsslpt).stringdata[i].mode=UG_REQUEST;
					(*inwsslpt).stringdata[i].esw=UG_ECHO;
					(*inwsslpt).stringdata[i].pet=1;
					zbytecp((*inwsslpt).stringdata[i].e_area,
							(*inwdtpt).defstr[i].e_area);
					zbytecp((*inwsslpt).stringdata[i].record,
							(*inwdtpt).defstr[i].record); 
					/* allocate space for current string measure of size bufsiz*/
					(*inwsslpt).stringdata[i].string=
							(char *)uu_toolmalloc((*inwsslpt).stringdata[i].record.bufsiz);
					if ((*inwsslpt).stringdata[i].string==NULL) {
						ug_errorhand(EMEMSPAC,"gopenws",NULL);
						uu_dexit; return(NULL);
					}
				}
			}
			if (ug_gksos.sysstate==UG_GKOP) ug_gksos.sysstate=UG_WSOP;
			irtn=(&ug_gksstli.wsopen[k].id);	/* ptr to index in wsopen */
		}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);			/* return pointer to index in wsopen */
}

/*********************************************************************
**    E_FUNCTION         :  Gerror gclosews(id) -- Close GKS workstation
**    PARAMETERS   
**       INPUT  : Gws *id -- pointer to workstation, returned from gopenws.
**       OUTPUT :  none
**    RETURNS      : NCL_NO_ERROR if all went OK. Else one of the GKS errors.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gclosews(id)        /* close a workstation */
/*$ INPUT */
Gws *id;
{
	int i;
	Gerror irtn;
	int prms[2],reply;
/*	char us[200];*/
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	irtn=ug_chkad(id,"gclosews");
#endif
	uu_denter2(UU_GTRC,(us,"gclosews(%d)",*id));
	i= *id;		/* i=workstation's index in wsopen array */
#ifdef UU_CHECK
	if ((i<0)||(i>4)) {
		ug_errorhand(EWSIDINV,"gclosews",id);	irtn=EWSIDINV;
	}
	if ((ug_gksos.sysstate!=UG_WSOP)&&(ug_gksos.sysstate!=UG_WSAC)
			&&(ug_gksos.sysstate!=UG_SGOP)) {
		ug_errorhand(ENOTWSOP,"gclosews",NULL); irtn=ENOTWSOP;
	}
	if (ug_gksstli.wsopen[i].connid==NULL) {		/* workstation is not open */
  		ug_errorhand(EWSNOTOP,"gclosews",i);
		irtn=EWSNOTOP;
	}
	if ((ug_gksstli.wsopen[i].connid!=NULL)&&
		(ug_gksstli.wsopen[i].state==UG_ACTIVE)) {
		ug_errorhand(EWSISACT,"gclosews",i); irtn=EWSISACT;
	}
	if (irtn==NCL_NO_ERROR) {
#endif
  	  	/* no parameters for a termination op */
  	  	prms[0]=UG_DCLOSEWS; prms[1]=i;
		(*(ug_gksstli.wsopen[i].connid)[UG_DCLOSEWS])(prms,&reply);
  	  	/* no reply from workstation termination command */
		/* decrement number ws needing NDC segments, if necessary */
		if ((*ug_gksstli.wsopen[i].wdtptr).needndc!=0) ug_ndcseg--;
  	  	ug_gksstli.wsopen[i].connid=NULL;
		ug_gksstli.nowsopen=ug_gksstli.nowsopen-1;
		if (ug_gksstli.nowsopen==0) ug_gksos.sysstate=UG_GKOP;
		irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION         :  Gerror gactivatews(ws) -- activate workstation.
**    PARAMETERS   
**       INPUT  :  Gws *ws-- pointer to workstation, returned from gopenws.
**       OUTPUT :  none
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gactivatews(ws)		/* activate workstation */
/*$ INPUT */
Gws *ws;
{
	Gerror irtn;
/*	char us[200];*/
	int prms[2],reply;
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	irtn=ug_chkad(ws,"gactivatews");
#endif
	 uu_denter2(UU_GTRC,(us," gactivatews(%d)",*ws));
#ifdef UU_CHECK
	if ((*ws<0)||(*ws>4)) {
		ug_errorhand(EWSIDINV,"gactivatews",ws);
		irtn=EWSIDINV;
	}
	if ((ug_gksos.sysstate!=UG_WSOP)&&(ug_gksos.sysstate!=UG_WSAC)) {	
		/* wrong op state */
		ug_errorhand(ENOTOPAC,"gactivatews",NULL); irtn=ENOTOPAC;
	}
	else if (ug_gksstli.wsopen[*ws].connid==NULL) {	/* workstation is closed */
		ug_errorhand(EWSNOTOP,"gactivatews",NULL); irtn=EWSNOTOP;
	}
	else if (ug_gksstli.wsopen[*ws].state==UG_ACTIVE) {	
		/* workstation is active */
		ug_errorhand(EWSISACT,"gactivatews",NULL); irtn=EWSISACT;
	}
	if (irtn==NCL_NO_ERROR) {				/* no errors */
#endif
		ug_gksstli.wsopen[*ws].state=UG_ACTIVE;
		ug_gksos.sysstate=UG_WSAC;
		ug_gksstli.nowsact++;				/* bump number of active workstations*/
  	  	prms[0] = UG_DACTWS;  prms[1] = *ws;
		(*(ug_gksstli.wsopen[*ws].connid)[UG_DACTWS])(prms,&reply);
#ifdef  UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  Gerror gdeactivatews(ws) -- deactivage workstation.
**    PARAMETERS   
**       INPUT  :  Gws *ws -- pointer to workstation returned by gopenws.
**       OUTPUT :  none
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gdeactivatews(ws)			/* deactivate workstation */
/*$ INPUT */
Gws *ws;
{
	Gerror irtn;
/*	char us[200];*/
	int prms[2],reply;
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	irtn=ug_chkad(ws,"gdeactivatews");
#endif
	 uu_denter2(UU_GTRC,(us," gdeactivatews(%d)",*ws));
#ifdef UU_CHECK
	irtn=ug_wschk(*ws,"gdeactivatews");
	if (irtn==NCL_NO_ERROR) {								/* no error */
#endif
		ug_gksstli.wsopen[*ws].state=UG_INACTIVE;
		ug_gksstli.nowsact=ug_gksstli.nowsact-1;
  	  	prms[0] = UG_DDEACTWS;  prms[1] = *ws;
		(*(ug_gksstli.wsopen[*ws].connid)[UG_DDEACTWS])(prms,&reply);
		if (ug_gksstli.nowsact==0) ug_gksos.sysstate=UG_WSOP;
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gclearws(ws,clearflag) --  Clear workstation.
** **    PARAMETERS   
**       INPUT  :  Gws *ws -- pointer to the workstation to be cleared.
						 Gclrflag clearflag.
**       OUTPUT :  none
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gclearws(ws,clearflag)
/*$ INPUT */
Gws *ws;
Gclrflag clearflag;
{
	int prms[4],reply[4];
	Gerror irtn;
/*	char us[200];*/
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	irtn=ug_chkad(ws,"gclearws");
#endif
	 uu_denter2(UU_GTRC,(us,"gclearws(%d,%d)",*ws,clearflag));
#ifdef UU_CHECK
	if ((*ws<0)||(*ws>4)) {
		ug_errorhand(EWSIDINV,"gclearws",ws); irtn=EWSIDINV;
	}
	else if ((ug_gksos.sysstate!=UG_WSOP)&&(ug_gksos.sysstate!=UG_WSAC)) {
		ug_errorhand(ENOTOPAC,"gclearws",NULL); irtn=ENOTOPAC;
	}
	else if (ug_gksstli.wsopen[*ws].connid==NULL ) {	/* ws not open */
		ug_errorhand(EWSNOTOP,"gclearws",ws); irtn=EWSNOTOP;
	}
	else {
#endif
	prms[0]=UG_DCLEARWS;
	prms[1]= *ws;
	ug_wkcal(*ws,prms,reply);
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gredrawsegws(ws) --  Redraw all segments on workstation.
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gredrawsegws(ws)	/*  Redraw all segments on workstation */
/*$ INPUT */
Gws *ws;						/* workstation identifier */
{
	int prms[3],reply[3];
	Gerror irtn;
/*	char us[200];*/
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	ug_chkad(ws,"gredrawsegws");
#endif
	uu_denter2(UU_GTRC,(us,"gredrawsegws(%d)",*ws));
#ifdef UU_CHECK
	irtn=ug_wschk(*ws,"gredrawsegws");
	if (irtn==NCL_NO_ERROR) {
#endif
	prms[0]=UG_DREDRAWWS;
	prms[2]= -1;					/* redraw everything */
	ug_wkcal(*ws,prms,reply);	/* clear screen and redraw */
	/* must put following call in workstations that cant redraw */
	/* ug_dredraw();						 redraw all segs */
#ifdef UU_CHECK
	}
#endif
	uu_dexit; 
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gredrawsegs() -- redraw all segs on all workstations.
**			Extension, not in GKS.
**  PARAMETERS   
**      INPUT:  none
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gredrawsegs()				/*  Redraw all segments */
{
	int prms[3];
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gredrawsegs()"));
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	if ((ug_gksos.sysstate!=UG_WSOP)&&(ug_gksos.sysstate!=UG_WSAC)
		&&(ug_gksos.sysstate!=UG_SGOP)) {
		ug_errorhand(ENOTWSOP,"gclearws",NULL); irtn=ENOTWSOP;
	}
	else {
#endif
	prms[0]=UG_DREDRAWWS;
	prms[2]= -1;			/* redraw everything */
	ug_wkout(prms,3);		/* clear screen and redraw */
#ifdef UU_CHECK
	}
#endif
	uu_dexit; 
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gredrawsegview(xform) -- redraw segs in view xform
**			Extension, not in GKS.
**  PARAMETERS   
**      INPUT:  int xform -- normtran number.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gredrawsegview(xform)				/*  Redraw all segments in a view */
/*$ INPUT */
int xform;								
{
	int prms[3];
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gredrawsegview(%d)",xform));
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	if ((ug_gksos.sysstate!=UG_WSOP)&&(ug_gksos.sysstate!=UG_WSAC)
		&&(ug_gksos.sysstate!=UG_SGOP)) {
		ug_errorhand(ENOTWSOP,"gredrawsegview",NULL); irtn=ENOTWSOP;
	}
	if ((xform<0)||(xform>=UG_MAXNTRAN)) {
		irtn=EBADXFRM; ug_errorhand(irtn,"gredrawsegview",&xform);
	}
	if (irtn==NCL_NO_ERROR) {
#endif
	prms[0]=UG_DREDRAWWS;
	prms[2]=xform;
	ug_wkout(prms,3);		/* clear screen and redraw */
#ifdef UU_CHECK
	}
#endif
	uu_dexit; 
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_drectunion(resultbox,box1,box2) -- union of 2 
**													rectangles.
**    PARAMETERS   
**       INPUT  : 		Gnrect3 *box1,*box2 -- input rectangles.
**       OUTPUT :  		Gnrect3 *resultbox -- rect containg both
**									box1 and box2. Right handed coord system
**									assumed (urb.z < llf.z).
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_drectunion(resultbox,box1,box2)		/* union of 2 rects */
Gnrect3 *resultbox;								/* rect containing box1,box2 */
Gnrect3 *box1,*box2;
{
	uu_denter(UU_GITRC,(us,"ug_drectunion(%g %g %g, %g %g %g)",
		(*box1).llf.x,(*box1).llf.y,(*box1).llf.z,
		(*box1).urb.x,(*box1).urb.y,(*box1).urb.z));
	(*resultbox).llf.x=((*box1).llf.x<(*box2).llf.x) ? 
							(*box1).llf.x : (*box2).llf.x;
	(*resultbox).llf.y=((*box1).llf.y<(*box2).llf.y) ? 
							(*box1).llf.y : (*box2).llf.y;
	(*resultbox).llf.z=((*box1).llf.z>(*box2).llf.z) ? 
							(*box1).llf.z : (*box2).llf.z;
	(*resultbox).urb.x=((*box1).urb.x>(*box2).urb.x) ? 
							(*box1).urb.x : (*box2).urb.x;
	(*resultbox).urb.y=((*box1).urb.y>(*box2).urb.y) ? 
							(*box1).urb.y : (*box2).urb.y;
	(*resultbox).urb.z=((*box1).urb.z<(*box2).urb.z) ? 
							(*box1).urb.z : (*box2).urb.z;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_drectintersect(resultbox,box1,box2) -- intersect of 2 
**													rectangles.
**    PARAMETERS   
**       INPUT  : 		Gnrect3 *box1,*box2 -- input rectangles.
**       OUTPUT :  		Gnrect3 *resultbox -- rect containg both
**									box1 and box2. Right handed coord system
**									assumed (urb.z < llf.z).
**    RETURNS      : 1 if intersection null, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_drectintersect(resultbox,box1,box2)		/* intersection of 2 rects */
Gnrect3 *resultbox;								/* rect containing box1,box2 */
Gnrect3 *box1,*box2;
{
	int irtn;
/*	char us[200];*/
	(*resultbox).llf.x=((*box1).llf.x>(*box2).llf.x) ? 
							(*box1).llf.x : (*box2).llf.x;
	(*resultbox).llf.y=((*box1).llf.y>(*box2).llf.y) ? 
							(*box1).llf.y : (*box2).llf.y;
	(*resultbox).llf.z=((*box1).llf.z<(*box2).llf.z) ? 
							(*box1).llf.z : (*box2).llf.z;
	(*resultbox).urb.x=((*box1).urb.x<(*box2).urb.x) ? 
							(*box1).urb.x : (*box2).urb.x;
	(*resultbox).urb.y=((*box1).urb.y<(*box2).urb.y) ? 
							(*box1).urb.y : (*box2).urb.y;
	(*resultbox).urb.z=((*box1).urb.z>(*box2).urb.z) ? 
							(*box1).urb.z : (*box2).urb.z;
	if (((*resultbox).llf.x<(*resultbox).urb.x) &&
		((*resultbox).llf.y<(*resultbox).urb.y) &&
		((*resultbox).llf.z>(*resultbox).urb.z))
			irtn=0;					/* intersection not null */
	else irtn=1;				/* intersection is null */
	uu_denter2(UU_GITRC,(us,"%d=ug_drectintersect(%g %g %g, %g %g %g)",irtn,
		(*box1).llf.x,(*box1).llf.y,(*box1).llf.z,
		(*box1).urb.x,(*box1).urb.y,(*box1).urb.z));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_drectintersect2(resultbox,box1,box2) -- intersect of 2 
**													2-D rectangles.
**    PARAMETERS   
**       INPUT  : 		Gnrect *box1,*box2 -- input rectangles.
**       OUTPUT :  		Gnrect *resultbox -- rect containg both
**									box1 and box2.
**    RETURNS      : 1 if intersection null, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_drectintersect2(resultbox,box1,box2)		/* intersection of 2 rects */
Gnrect *resultbox;								/* rect containing box1,box2 */
Gnrect *box1,*box2;
{
	int irtn;
	(*resultbox).ll.x=((*box1).ll.x>(*box2).ll.x) ? 
							(*box1).ll.x : (*box2).ll.x;
	(*resultbox).ll.y=((*box1).ll.y>(*box2).ll.y) ? 
							(*box1).ll.y : (*box2).ll.y;
	(*resultbox).ur.x=((*box1).ur.x<(*box2).ur.x) ? 
							(*box1).ur.x : (*box2).ur.x;
	(*resultbox).ur.y=((*box1).ur.y<(*box2).ur.y) ? 
							(*box1).ur.y : (*box2).ur.y;
	if (((*resultbox).ll.x>(*resultbox).ur.x) ||
		((*resultbox).ll.y>(*resultbox).ur.y))
			irtn=1;					/* intersection is null */
	else irtn=0;				/* intersection not null */
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gupdatews(ws,regenflag) -- update workstation.
**      Cause all of the deferred actions for the
**		  workstation specified by ws to be performed without any
**		  intermediate clearing of the display surface.
**			  If the regeneration flag regenflag is set to PERFORM the
**		  display surface is cleared if necessary. The workstation
**		  transformation is updated if one is pending. The segments
**		  stored on the workstation are redrawn.
**			  None of the additional functions are executed if regenflag
**		  is set to SUPPRESS.
**
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id, returned from gopenws.
**					 Gregen regenflag -- regeneration flag.
**  RETURNS      :  NCL_NO_ERROR if things went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gupdatews(ws,regenflag)
/*$ INPUT */
Gws *ws;
Gregen regenflag;
{
	Gerror irtn;
	int prms[3],reply[3];
/*	char us[200];*/
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	irtn=ug_chkad(ws,"gupdatews");
#endif
	uu_denter2(UU_GTRC,(us,"gupdatews(%d,%d)",*ws,regenflag));
#ifdef UU_CHECK
	if ((*ws<0)||(*ws>4)) {
		ug_errorhand(EWSIDINV,"gdeactivatews",ws);
		irtn=EWSIDINV;
	}
	if (ug_gksos.sysstate!=UG_WSAC) {
		ug_errorhand(ENOTWSAC,"gdeactivatews",NULL); irtn=ENOTWSAC;
	}
	else if (ug_gksstli.wsopen[*ws].connid==NULL) {		/*workstation is closed */
		ug_errorhand(EWSNOTOP,"gdeactivatews",NULL); irtn=EWSNOTOP;
	}
	/* should check for MI or INPUT */
	if (irtn==NCL_NO_ERROR) {								/* no error */
#endif
	prms[0]=UG_DUPDATE;
	prms[2]=(int)regenflag;
	ug_wkcal(*ws,prms,reply);
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsdus(ws,defmode,modmode) -- set display update state.
**      Set the deferral and modification modes.
**		  on the specified workstation.
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**					 Gdefmode defmode -- deferral mode. Specifies when the updates
**					 are to be performed. Values are:
**							ASAP:		As Soon As Possible
**							BNIG:		Before Next Interaction Globally
**							BNIL:		Before Next Interaction Locally
**							ASTI:		At Some TIme.
**					 Only ASAP and BNIG are implemented so far.  The unicad
**					 system uses BNIG as the default deferral mode.
**
**					 Gmodmode modmode -- modification mode. Specifies when 
**					 automatic regenerations are to be done.
**							Values are:
**							NIVE:		No Immediate Visual Effects.
**							UWOR:		Update WithOut Regeneration.
**							UQUM:		Use Quick Update Methods.
**							PRIN:		Perform Regeneration If Necesssary.
**					 Modification modes UG_NIVE and UG_UQUM are supported.  The
**					 only difference is that in UG_NIVE, some workstations will
**					 delete segments as a block instead of redrawing in background.
**					 The unicad system uses UG_UQUM as the default mode.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsdus(ws,defmode,modmode)
/*$ INPUT */
Gws *ws;
Gdefmode defmode;		/* deferral mode (ASAP, BNIG, BNIL, ASTI) */
Gmodmode modmode;		/* modification mode (NIVE,UWOR,UQUM,PRIN) */
{
	Gerror irtn;
	int prms[4],reply[4];
/*	char us[200];*/
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	irtn=ug_chkad(ws,"gsdus");
#endif
	uu_denter2(UU_GTRC,(us,"gsdus(%d,%d,%d)",*ws,defmode,modmode));
#ifdef UU_CHECK
	irtn=ug_wschk(*ws,"gsdus");
	if (irtn==NCL_NO_ERROR) {
#endif
	/* if changing to UG_ASAP, update the workstation first */
	if (defmode==UG_ASAP) {
		prms[0]=UG_DUPDATE;
		prms[2]=(int)UG_SUPPRESS;
		ug_wkcal(*ws,prms,reply);
	}
	/* notify workstation of pending chang in modes */
	prms[0]=UG_DSETDEFST;
	prms[2]=(int)defmode; prms[3]=(int)modmode;
	ug_wkcal(*ws,prms,reply);
	/* set modes in workstation state list */
	(*ug_gksstli.wsopen[*ws].outptr).defmode=defmode;
	(*ug_gksstli.wsopen[*ws].outptr).modmode=modmode;
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION    :  Gerror gmessage(wkid,msg) -- message on workstation.
**       Send a message to a GKS workstation. The message will be
**			printed in some convenient place on the workstation,
**			usually a scrolling window of about 10 lines.
**    PARAMETERS   
**       INPUT  :  Gws *wkid -- pointer to workstation.
**						 char *msg -- message.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gmessage(wkid,msg)	/* send a message to workstation */
/*$ INPUT */
Gws *wkid; 
Gchar *msg;
{
	struct {
		int intprms[2];
		char *p;
	} prms;
	int reply[4];
	Gerror irtn;
/*	char us[200];*/
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	irtn=ug_chkad(wkid,"gmessage wkid");
	irtn=ug_chkad(msg,"gmessage msg");
#endif
	 uu_denter2(UU_GTRC,(us," gmessage(%d,%s)",*wkid,msg));
#ifdef UU_CHECK
	if (irtn==NCL_NO_ERROR) irtn=ug_wschk(*wkid,"gmessage");
	len=strlen(msg);
	if (len>150) {
		ug_errorhand(EBADMSG,"gmessage"); irtn=EBADMSG;
	}
	if (irtn==NCL_NO_ERROR) {
#endif
		prms.intprms[0]=UG_DMSG; prms.intprms[1]= *wkid;
		prms.p=msg;
		ug_wkcal(*wkid,&prms,reply);
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gchar *gescape(function,data) -- escape.
**      Provides a standard way of doing non-standard things.
**			************not implemented yet *************
**  PARAMETERS   
**      INPUT:  (*function)() -- pointer the escape function.
**					 Gchar *data -- pointer to the function data record.
**      OUTPUT: none
**  RETURNS      :  Implementation dependent
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void gescape(funct,datarec)
/*$ INPUT */
Gint (*funct)();
Gchar *datarec;
{
/*	char us[200];*/
#ifdef UU_CHECK
	ug_chkad(datarec,"gclearws");
#endif
	uu_denter2(UU_GTRC,(us,"gescape(funct,data)"));
	/* not implemented yet */
	(*funct)(datarec);			/* call the escape function */
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  gprompt(ws,loc,string,num) -- char prompt on workstation.
**      Put up char string prompt on a workstation 
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**					 Gnpoint3 *loc -- Ndc coord location of prompt.
**					 char *string -- text of the prompt.
**					 int num -- prompt number, 0 to 10.
**  RETURNS      :  none
*********************************************************************/
int gprompt(ws,loc,string,num)
/*$ INPUT */
Gws *ws;						/* workstation */
Gnpoint3 *loc;				/* ndc coordinate location of the prompt */
char string[];				/* the text of the prompt */
int num;						/* number of the prompt,0 to 10 */
{
	Gipoint xy;
	Gerror irtn;
	Gint segno;						/* Segment to temporarily close */
	extern int ug_viwseg;		/* Currently open segment number */
/*	char us[200];*/

	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	irtn=ug_chkad(ws,"gprompt ws");
	irtn=ug_chkad(loc,"gprompt loc");
	irtn=ug_chkad(string,"gprompt string");
#endif
	uu_denter2(UU_GTRC,(us,"gprompt(%d, %g %g %g,%s,%d)",
					*ws,(*loc).x,(*loc).y,(*loc).z,string,num));
#ifdef UU_CHECK
	irtn=ug_chkwsac(*ws,"gprompt");
	if (strlen(string)>150) {
		ug_errorhand(EBADMSG,"gdprompt"); irtn=EBADMSG;
	}
	if ((num<0)||(num>10)) {
		ug_errorhand(EBADPRMTNO,"gdprompt"); irtn=EBADPRMTNO;
	}
	if (irtn==NCL_NO_ERROR) {								/* no error */
#endif

	/* If inside open segment, close it */
	if( ug_gksos.sysstate==UG_SGOP) {
		segno = ug_viwseg;
		gcloseseg();
	}
	else
		segno = -1;

	/* Change from NDC to raster coordinats */
	(*(ug_gksstli.wsopen[*ws].connid)[UG_DNDCDEV])(loc,&xy,*ws);

	/* call the workstation to put up prompt */
	(*(ug_gksstli.wsopen[*ws].connid)[UG_DPROMPT])(*ws,string,&xy,num);

	/* If we closed a segment, reopen here */
	if( segno >= 0 ) {
		gopenseg(segno);
	}

#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return irtn;
}

/********************************************************************* 
**  E_FUNCTION:  gdnprompt(ws,num) -- take down a prompt on a workstation. 
**	 PARAMETERS:
**      INPUT:  Gws *ws -- workstation id of workstation containing the prompt.
**					 int num -- number of the prompt to take down, 0 to 10.
**      OUTPUT: none
**  RETURNS      :  none
**	 WARNINGS	  :  The prompt number "num" must have previously been
**							displayed by gprompt on the same workstation.
*********************************************************************/
int gdnprompt(ws,num)			/* take down a prompt */
/*$ INPUT */
Gws *ws;						/* workstation id */
int num;						/* number of the prompt */
{
	Gerror irtn;
/*	char us[200];*/
#ifdef UU_CHECK
	irtn=ug_chkad(ws,"gdnprompt");
#endif
	uu_denter2(UU_GTRC,(us,"gdnprompt(%d, %d)",*ws,num));
	/*  call workstation to take down the prompt */
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	irtn=ug_wschk(*ws,"gdnprompt");
	if ((num<0)||(num>10)) {
		ug_errorhand(EBADPRMTNO,"gdnprompt"); irtn=EBADPRMTNO;
	}
	if (irtn==NCL_NO_ERROR) {								/* no error */
#endif
	(*(ug_gksstli.wsopen[*ws].connid)[UG_DDNPROMPT])(*ws,num);
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return irtn;
}

/**************** routines below are not user callable *******************/

/********************************************************************* 
**  S_FUNCTION:  ug_dredraw(prms) -- redraw segs in view prms[2].
**  PARAMETERS   
**      INPUT:  int prms[3] -- prms[2] contains view to redraw, or -1.
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_dredraw(prms)			/* redraw all segs in view prms[2] */
int prms[];
{
	UG_segstli *p;
	int i,mask;
	int segno;

	uu_denter(UU_GITRC,(us,"ug_dredraw(%d)", prms[2]));

	/* mask represents this normtran, or any normtran */
	if (prms[2]>=0) {
		mask = (1<<prms[2])|UG_SEGINNTRAN;
		uu_dprint(UU_GITRC,(us,"mask=x%x",mask));
	}

	/* Redraw from visible visible list if it exists */
	uu_dprint(UU_GITRC,(us,"ug_vislistok=%d",ug_vislistok));
	if( ug_vislistok ) {    
		uu_dprint(UU_GITRC,(us,"redrawing from vislist"));
		for(i=0; i<UU_IALEN(ug_vislist); i++) {
			segno = UU_IAVAL(ug_vislist,i);
			if (segno<UG_MAXSEGNO) {
				p = ug_segac(segno);
				if( p == NULL ) continue;
/*
.....Don't redraw this seg unless it depends
.....upon this ntran, or upon any ntran 
*/
				if (prms[2]>=0)
				{
					if (((*p).xforms&mask)==0) continue;
				}
				ug_view0(segno,0);
			}
		}
/*
.....Redraw motion in this viewport
*/
		ncl_display_motion(prms[2],0,0,0,UU_TRUE,UU_FALSE,UU_NULL);
	}

	else {
		ug_vislistok = 1;					/* We're going to generate vislist */
		ug_seginitscan();
		while ((p=ug_segscan())!=NULL) {
			if ((*p).segid<UG_MAXSEGNO) {
/*
.....Redraw motion in this viewport
*/
				if (p->segid == NCL_mot_seg)
				{
					if (p->segatts.gvis == UG_VISIBLE)
						ncl_display_motion(prms[2],0,0,0,UU_TRUE,UU_FALSE,UU_NULL);
				}
				else
				{
					if (prms[2]>=0) {			/* don't redraw this seg unless it depends
													upon this ntran, or upon any ntran */
						if (((*p).xforms&mask)==0) continue;
					}
					ug_view0((*p).segid,0);
				}
			}
		}
	}


	uu_dexit;
}

/********************************************************************* 
**  S_FUNCTION:  ug_dredrawvis() -- redraw all segs  on a workstation.
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_dredrawvis()						/* redraw all visible segs */
{
	UG_segstli *p;
	int segno;
	int i;

	uu_denter(UU_GITRC,(us,"ug_dredrawvis()"));

	/* Redraw from visible visible list if it exists */
	if( ug_vislistok ) {
		uu_dprint(UU_GITRC,(us,"redrawing from vislist"));
		for(i=0; i<UU_IALEN(ug_vislist); i++) {
			segno = UU_IAVAL(ug_vislist,i);
			if (segno<UG_MAXSEGNO)
			{
				p = ug_segac(segno);
/*
....I added the following IF statement working with *SET/LABEL,MODIFY,ALL command.
....When I ran a partprogram with the following comand sequence at this point
....I have got segmentation violation.
....                   **SET/LABEL,MODIFY,ALL
....                   DRAFT/REPAINT,ALL
....This IF fixes it.
....Paul. 07/23/93
*/
                if(p != NULL) 
                {
    				if (p->segid == NCL_mot_seg)
    				{
    					ncl_display_motion(-1,0,0,0,UU_TRUE,UU_FALSE,UU_NULL);
    				}
    				else
    				{
    					ug_view0(segno,0);
    				}
                }
			}
		}
	}

	else {
		ug_vislistok = 1;					/* We're going to generate vislist */
		ug_seginitscan();
		while ((p=ug_segscan())!=NULL) {
			if ((*p).segid<UG_MAXSEGNO)
			{
				if (p->segid == NCL_mot_seg)
				{
					ncl_display_motion(-1,0,0,0,UU_TRUE,UU_FALSE,UU_NULL);
				}
				else
				{
					ug_view0((*p).segid,0);
				}
			}
		}
/*
.....added for plot
.....Yurong 10/12/98
*/
		if (UJ_plotting==1)
			ug_vislistok = 0;	 				
	}
	uu_dexit;
}

/********************************************************************* 
**  S_FUNCTION:  ug_dredrawallvis() -- redraw all segs  on a workstation.
**						Just like ug_dredrawvis, but redraws icons also.
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_dredrawallvis()						/* redraw ALL visible segs */
{
	UG_segstli *p;
	int segno;
	int i;

	uu_denter(UU_GITRC,(us,"ug_dredrawallvis()"));

	/* Redraw from visible visible list if it exists */
	if( ug_vislistok ) {
		uu_dprint(UU_GITRC,(us,"redrawing from vislist"));
		for(i=0; i<UU_IALEN(ug_vislist); i++) {
			segno = UU_IAVAL(ug_vislist,i);
			ug_view0(segno,0);
		}
	}

	else {
		ug_vislistok = 1;					/* We're going to generate vislist */
		ug_seginitscan();
		while ((p=ug_segscan())!=NULL) {
			ug_view0((*p).segid,0);
		}
	}

	uu_dexit;
}

static int gdabl[UG_MAXOPWS];
static int gndabl;

/*********************************************************************
**    I_FUNCTION     :  int ug_wsoff(ws) -- disable all but 1 ws.
**			disable all but one ws, remembering which ones we disabled
**    PARAMETERS   
**       INPUT  : 
**					Gws ws		the one workstation to not disable
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_wsoff(ws)
Gws ws;					/* the one workstation to not disable */
{
	int i;
	/* disable all but one ws, remembering which ones we disabled. */
	gndabl=0;
	for (i=0; i<ug_gksdesctbl.maxopws; i++) {
		if (i!=(int)ws) {
			if (ug_gksstli.wsopen[i].connid!=NULL) {
				if (ug_gksstli.wsopen[i].state==UG_ACTIVE) {	/* found one to disable*/
					gdabl[gndabl]=i;
					gndabl++;
					ug_gksstli.wsopen[i].state=UG_INACTIVE;
				}
			}
		}
	}
}

/*********************************************************************
**    I_FUNCTION     :  int ug_wson() -- reactivate disabled workstations.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_wson()
{
	int i;
	/* re-activate all the workstations disabled above. */
	for (i=0; i<gndabl; i++)	 
		ug_gksstli.wsopen[gdabl[i]].state=UG_ACTIVE;
}

/*********************************************************************
**    I_FUNCTION     :  gwkin(wsid,parms,reply) 
**			Call all INPUT or OUTIN wkstns with the specified parms and
**       replies until one of them returns a replyop not equal NONE.
**    PARAMETERS   
**       INPUT  : 
**				parms:	input parameters (varies according to function
**							called)
**       OUTPUT :
**          wsid:		pointer to workstation data structure (containing
**							workstation id)
**				reply:	output return values (varies according to function
**							called)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gwkin(wsid,parms,reply)    /* call all INPUT or OUTIN wkstns
             with the specified parms and replies until one of
             them returns a replyop not equal NONE. */
Gws *wsid; 	/*ptr to id of workstation that returned something */
int parms[],reply[];
{
	int i,found,gotit;
	 uu_denter(UU_GITRC,(us,"gwkin"));
#ifdef UU_CHECK
	ug_chkad(wsid,"gwkin wsid");
	ug_chkad(parms,"gwkin parms");
	ug_chkad(reply,"gwkin reply");
	if ((*wsid>0)||(*wsid>4)) {
		fprintf(ug_gksos.erfile,"gwkin bad ws=%d\n",*wsid); uu_dexit; exit(1);
	}
#endif
	gotit=0;
	while (gotit==0) {
	found=0;
	for (i=0; i<ug_gksdesctbl.maxopws; i++) {
		if ((ug_gksstli.wsopen[i].connid!=NULL)&&(ug_gksstli.wsopen[i].state==UG_ACTIVE)) {
        if ((ug_gksstli.wsopen[i].wdtptr->category==UG_INPUT)||
        (ug_gksstli.wsopen[i].wdtptr->category==UG_OUTIN)) {
          parms[1]=ug_gksstli.wsopen[i].id;
			 (*(ug_gksstli.wsopen[i].connid)[parms[0]])(parms,reply);
          found=1;
          if (parms[0]!=(int)(UG_NONE)) { *wsid=ug_gksstli.wsopen[i].id; gotit=1;};
        }
    }
  }
  if (found==0) {/*fprintf(ug_gksos.erfile,
                "ug_wkcal. no active workstations\n");*/
                 gotit=1; }
  }        /* while */
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_wkcal(wsid,parms,reply) -- call specified
**								workstation
**    PARAMETERS   
**       INPUT  : 
**          wsid		workstation id of desired workstation
**				parms		the argument list of the function called
**       OUTPUT :  
**          reply		pointer to return value list of function called
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_wkcal(wsid,parms,reply)    /* call specified workstation */
int parms[],reply[];
Gws wsid;							/* index of workstation */
{
	 uu_denter(UU_GITRC,(us,"ug_wkcal(%d,%d,reply)",wsid,parms[0]));
#ifdef UU_CHECK
	ug_chkad(parms,"ug_wkcal parms");
	ug_chkad(reply,"ug_wkcal reply");
	if ((wsid<0)||(wsid>4)) {
/*		fprintf(ug_gksos.erfile,"ug_wkcal bad wsid=%d\n",wsid);*/
		uu_dexit; exit(1);
	}
#endif
/*
.....WinNT
*/
	reply[0] = 0;
	if (ug_gksstli.wsopen[wsid].connid!=NULL) {
      if (ug_gksstli.wsopen[wsid].state==UG_ACTIVE) {
			parms[1]=wsid;
			/* call workstation entry point thru table */
			(*(ug_gksstli.wsopen[wsid].connid)[parms[0]])(parms,reply);
		}
	}
/*	else fprintf(ug_gksos.erfile,"ug_wkcal(%d,p,r). no such workstation id\n",wsid);*/
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_wkout(prms,len) -- call output wkstns with
**					no reply.
**    PARAMETERS   
**       INPUT  : 
**          prms		input arguments and function id to call
**				len		length of input parameter array in ints
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_wkout(prms,len)       /* call output wkstns with no reply */
int prms[],len;          /* input parameter array and length */
{ int i;
	Gwscat k;
	int reply[2];
/*  char us[200];*/
#ifdef UU_CHECK
	ug_chkad(prms,"ug_wkout");
#endif
	 uu_denter2(UU_GITRC,(us,"ug_wkout(%d,%d)",prms[0],len));
	for (i=0; i<ug_gksdesctbl.maxopws; i++) {    /* for each workstation*/
    if (ug_gksstli.wsopen[i].connid!=NULL) {
      if (ug_gksstli.wsopen[i].state==UG_ACTIVE) {
        k=ug_gksstli.wsopen[i].wdtptr->category;    /* get wkstn category */
        if ((k==UG_OUTPUT)||(k==UG_OUTIN)||(k==UG_MO)) { /* found output wkstn*/
        	prms[1]=ug_gksstli.wsopen[i].id;
			/* call workstation entry point thru table */
			 (*(ug_gksstli.wsopen[i].connid)[prms[0]])(prms,reply);
		  }
      }
    }
  }
	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION    :  Gerror ug_signal(wkid,func)
**       Provide a function to be called on user-generated asynchronous
**			signals.
**    PARAMETERS   
**       INPUT  :  Gws *wkid   -- pointer to workstation.
**						 int *func() -- address of function to be called 
**											 when a signal is generated.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror ug_signal(wkid,func)
/*$ INPUT */
Gws *wkid; 
Gint (*func)();
{
/*	char us[80];*/
	Gerror irtn=NCL_NO_ERROR;

#ifdef UU_CHECK
	irtn=ug_chkad(wkid,"ug_signal wkid");
	irtn=ug_chkad(func,"ug_signal func");
#endif

	uu_denter2(UU_GTRC,(us,"ug_signal(%d,%x)",*wkid,func));

#ifdef UU_CHECK
	if (irtn==NCL_NO_ERROR) irtn=ug_wschk(*wkid,"ug_signal");
	if (irtn==NCL_NO_ERROR)
#endif
	(*(ug_gksstli.wsopen[*wkid].connid)[UG_DSIGNAL])(func);

	uu_dexit;
	return(irtn);
}
