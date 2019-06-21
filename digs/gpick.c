/*********************************************************************
**    NAME         :  gpick.c -- ordinary (not area) picking routines.
**       CONTAINS:
**    int ug_findsgpk(n,ats)  
**    ug_findpolyln3(n,points,cxform,rect,segptr)
**    ug_findpolyln(n,points,cxform,rect,segptr)
**    ug_findndcln(&np1,&np2,rect,segptr,epsx,epsy)
**    ug_findpolymk3(n,points,cxform,rect,segptr)
**    ug_findpolymk2(n,points,cxform,rect,segptr)
**    ug_findndcpmk(&np,rect,segptr,epsx,epsy)
**    ug_findtext(posn,str,cxform,rect,segptr,prats) -- find text.
**    ug_findpolygon3(len,points,cxform,rect,segptr);
**    ug_findpolygon2(len,points,cxform,rect,segptr);
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**         gpick.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**         04/29/15 , 15:05:23
*********************************************************************/
#include "umath.h"
#include "zsysdep.h"
#include "udebug.h"
#include "gtbl.h"
#include "gsegac.h"
#include "gviw.h"
#include "gsegop.h"
#include "gdidd.h"
#include "gconvert.h"
#include "dselect.h"
#include "driver.h"
#include "view.h"
#define false 0
#define true 1
#define UG_FALSE 0
#define UG_TRUE 1
/* extern char ug_ops[74][16]; */
		/* used for conversion routines - to free used memory */
static Gwpoint3 *temp3;
static Gwpoint *temp;
/*********************************************************************
**    I_FUNCTION     :  int ug_findsgpk(n,findprms,vlst,vlen) -- Find seg n. 
**    PARAMETERS   
**							We need all these parameters to make ug_findsgpk
**							recursively callable for heirarchial segments.
**       INPUT/OUTPUT  : 
**						int n -- segment to find.
**						UG_findprms *findprms;
**						typedef struct {
**							UG_prat3 prats; 		/* primitive atts (only text used). 
**							int curxform;			/*current viewing xform.
**							int pkid; 				/*current pick id.
**							Gtran gmodxf;			/* global modelling xform.
**						} UG_findprms;
**    RETURNS      : 1 if everything in seg was found, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_findsgpk(n,findprms,vlst,vlen) 				/* find segment n. */
int n;
UG_findprms *findprms;
int vlst[10];								/* segment id list of seg found */
int *vlen;									/* length of vlst */
{
	int irtn;
  UG_plylna2op *cmd;  					/* storage for a UG_cmd */
  int ncmd;								/* number of commands in a segment */
  int icmd;								/* ith command */
  int *p,cmdlen;
  int i,j,k,m,len,opcode;
  Gfloat x,y,z;
  Gtran lmodxf;					/* local modelling xform, starts out identity */
  Gtran cxform;					/* composite (model to NDC) xform */
  Gfloat (*q)[];           	/* q is pointer to float array */
  char *s;
  char *ug_lsielt();			/* function to return list element */
  UG_segstli *segptr;		/* pointer to segment n's header */
  UG_segstli *segptr2;		/* pointer to segment n's header if we need to
										expand seg's ndcbox, or NULL*/
  Gwpoint3 pt;
  Gwpoint3 *Getpoints3();
  Gwpoint *Getpoints();
  int foundit;					/* false=didnt find anything */
  Gnrect3 nrect;				/* NDC text extent rectangle */
  Gfloat eps;					/* biggest side of pick aperture */
  int inbox;
	int wireframe_disp = 1;
	int hid_line = 0;
	UV_vport vport;
	int vp;
#define EPS (UU_REAL) .01 

  uu_denter(UU_GITRC,(us,"ug_findsgpk(%d) vlen=%d",n,*vlen));

	segptr=ug_segac(n);
	if (segptr==NULL) { 
		uu_dprint(-1,(us,"ug_findsgpk error. segment %d doesn't exist",n));
		uu_dexit; return(0);
	}
	/* make sure segment has correct visibility,detectable  
		and within (without) ndcbox */
	if ((ug_find.vis!=2)&&((segptr->segatts).gvis!=(Gsegvis)ug_find.vis)) {
			uu_dexit; 	return(0);
	}
	if (segptr->segatts.gdtect==UG_UNDETECTABLE) { uu_dexit; return(0);}
	inbox=ug_notinbox(segptr,ug_find.x,ug_find.y,ug_find.epsx,ug_find.epsy);
	if ((ug_find.pikmode==0)||(ug_find.pikmode==2)) {	/* part or all in */
		if (inbox==1) {	/* out of box*/
			uu_dexit; return(0);
		}
	}
	/* segment is VISIBLE and DETECTABLE, traverse it */
	/* if a raster copy of the seg exists, use it instead. */
	if (segptr->rassegpt!=NULL) {
		Gipoint rasloc;				/* raster version of ug_find.x,y */
		int rasepsx,rasepsy;			/* raster version of pick aperture */
		int newdist;
		int irtnras;
		uu_dprint(UU_GITRC,(us,"ug_findsgpk using raster copy"));
		/* FIX next line. should go thru ws xform. Also, use current ws, not 0*/
		rasepsx=ug_find.epsx*((*(ug_gksstli.wsopen[0].wdtptr)).dspsize.raster.x);
		rasepsy=ug_find.epsy*((*(ug_gksstli.wsopen[0].wdtptr)).dspsize.raster.x);
		uu_dprint(UU_GITRC,(us,"ug_findsgpk. rasepsx,y=%d %d, rasater.x=%d",
			rasepsx,rasepsy,(*(ug_gksstli.wsopen[0].wdtptr)).dspsize.raster.x));
		(*(ug_gksstli.wsopen[0].connid)[UG_DNDCDEV])(&ug_find.x,&rasloc,0);
		if ((irtnras=ug_findsgpkras(segptr,findprms,vlst,vlen,&rasloc,rasepsx,
			rasepsy,ug_find.func,ug_find.pikmode,&(*findprms).pkid,&newdist))>0) {
			ug_find.found=true;
			ug_find.epsx=newdist;  
			ug_find.epsx=ug_find.epsx/
				((*(ug_gksstli.wsopen[0].wdtptr)).dspsize.raster.x);
			ug_find.epsy=newdist;
			ug_find.epsy=ug_find.epsy/
				((*(ug_gksstli.wsopen[0].wdtptr)).dspsize.raster.y);
			if (irtnras==2) irtn=1;			/* found everything */
		}
		else {ug_find.found=false; irtn=0;}
		goto rtn;
	}
	irtn=1;			/* assume everything will be found until something fails */
	vlst[*vlen]=n;  (*vlen)++; 	/* record this ug_findsgpk activation */
	ug_ident(lmodxf);					/* set local model xform to identity*/
	/* calculate cxform = composite xform matrix (Model-->NDC) */
	ug_matmp(cxform,(*findprms).gmodxf,ug_vxform[(*findprms).curxform]);
	ug_find.found=false;
	foundit=false;
	ncmd = ug_lsinelt(segptr->seglist);
	/* see if we need to expand ndc box as we search. */
	if ((ug_ndcseg>0)&&((*segptr).wcboxok==0)) segptr2=segptr;
	else segptr2=NULL;
	uu_dprint(UU_GITRC,(us,"ug_findsgpk. ncmd=%d",ncmd));
	vp = (*findprms).curxform;
	if (vp>0)
	{
		uv_getvpid(UV_act_screen[0].vports[vp-1],&vport);
		if (vport.wireframe)
			wireframe_disp = 1;
		else
			wireframe_disp = 0;
		if (vport.disp_mode==3)
			hid_line = 1;
		else
			 hid_line = 0;
	}

 	/* for each command in seg n */
	for( icmd=0; icmd < ncmd; ++icmd ){
	cmd = (UG_plylna2op *)ug_lsielt(segptr->seglist,icmd);
	cmdlen = ug_lsilen(segptr->seglist,icmd)/sizeof(int);
	opcode=(*cmd).elttype;
/*	uu_dprint(UU_GITRC,(us,"ug_findsgpk. opcode[%d]=%s cmdlen=%d",
				icmd,&ug_ops[opcode][0],cmdlen)); */
	if (opcode>UG_OPPTR) {       /* library command */
      /*ug_viwlib(cmd);                     /* call library proc */
		uu_dprint(-1,(us,"ug_findsgpk can't handle library command"));
	}
	else {                     /* normal command */
    switch (opcode) {
/*
.....added for shade segment
.....Yurong 2/16/99
*/
	 case UG_SHADEAREAOP:
			if (!hid_line) 
			{
				ug_findshd((UG_shadearea *)cmd, cxform, 
							&ug_gksstli.vtran[(*findprms).curxform].vport,segptr2);
			}
			if (ug_find.found!=true) irtn=0;
			break;

    case UG_PLYLNA3OP:
/*			ug_findpolyln3((*(UG_plylna3op *)cmd).len,
					(*(UG_plylna3op *)cmd).pts,cxform,
					&ug_gksstli.vtran[(*findprms).curxform].vport,segptr2); -iris */

		if (wireframe_disp)
		{
			len=(*(UG_plylna3op *)cmd).len;

			/* Getpoints3 checks precision and returns array of Gwpoint3 */
			ug_findpolyln3(len, temp3=Getpoints3((*(UG_plylna3op *)cmd).pts,len),
					cxform, &ug_gksstli.vtran[(*findprms).curxform].vport,segptr2);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
		}
			if (ug_find.found!=true) irtn=0;
			break;
    case UG_PLYLNA2OP:
/*			ug_findpolyln((*(UG_plylna2op *)cmd).len,
					(*(UG_plylna2op *)cmd).pts,cxform,
					&ug_gksstli.vtran[(*findprms).curxform].vport,segptr2); -iris */
		if (wireframe_disp)
		{
			len = (*(UG_plylna2op *)cmd).len;
			ug_findpolyln(len, temp=Getpoints((*(UG_plylna2op *)cmd).pts,len),
					cxform, &ug_gksstli.vtran[(*findprms).curxform].vport,segptr2);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp);
#endif
		}
			if (ug_find.found!=true) irtn=0;
			break;
	 case UG_PLYLNRASOP:
			/*len=(*(UG_plylnrasop *)cmd).len;
			/*ug_polylnras(len,&(*cmd).gcint[2]);*/
			/*if (ug_find.found!=true) irtn=0;*/
			break;
    case UG_PLYMKA3OP: /*ug_polymk3((*cmd).gcint[1],&(*cmd).gcint[2]);*/
/*			ug_findpolymk3((*(UG_plymka3op *)cmd).len,
					(*(UG_plymka3op *)cmd).pts,cxform,
					&ug_gksstli.vtran[(*findprms).curxform].vport,segptr2); -iris */
			len = (*(UG_plymka3op *)cmd).len;
			ug_findpolymk3(len, temp3=Getpoints3((*(UG_plymka3op *)cmd).pts,len),
					cxform, &ug_gksstli.vtran[(*findprms).curxform].vport,segptr2);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
			if (ug_find.found!=true) irtn=0;
			break;
    case UG_PLYMKA2OP: len=(*(UG_plymka2op *)cmd).len;
			ug_findpolymk2(len,temp=Getpoints((*(UG_plymka2op *)cmd).pts,len),
				cxform, &ug_gksstli.vtran[(*findprms).curxform].vport,segptr2);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp);
#endif
			if (ug_find.found!=true) irtn=0;
			break;
	 case UG_PLYMKRASOP:
			/*len=(*(UG_plymkrasop *)cmd).len;
			/*ug_polymkras(len,&(*cmd).gcint[2]); */
			/*if (ug_find.found!=true) irtn=0;*/
			break;
    case UG_TEXTOP:
			ug_findtext(temp3=Getpoints3(&(*(UG_textop *)cmd).position,1),
					(*(UG_textop *)cmd).string,cxform,
					&ug_gksstli.vtran[(*findprms).curxform].vport,
					segptr2,&(*findprms).prats);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
         /*gtext(&(*cmd).gcreal[2],&(*cmd).gcreal[5]);
			/*if ((ug_ndcseg>0)&&((*segptr).ndcboxok==0)) {
				/* expand the box by string extent rect */
				/* for now, estimate the string rectangle by using
					char height and estimating each char's width
					is .6 its height */
			/*	ug_txrect(&(*cmd).gcreal[2],&(*cmd).gcreal[5],&nrect);
			/*	ug_ndcboxadd(segptr,&nrect.llf,&nrect.urb);	
			/*}
         /*else {
				/* check the following */
          /* 	s=(char *)((*cmd).gcptr[(4+ug_pratio-1)/ug_pratio]); 
			/*	gtext((*cmd).gcreal[2],s);
         /*} */
			/*if (ug_find.found!=true) irtn=0;*/
			break;
	 case UG_TEXTRASOP: len=(*(UG_textrasop *)cmd).len;
			/*ug_textras(&(*cmd).gcint[2],&(*cmd).gcreal[5],(*segptr).userdata[0]);*/
			/*if (ug_find.found!=true) irtn=0;*/
			break;
	 case UG_FLAREA3OP: len=(*(UG_flarea3op *)cmd).len;
			ug_findpolygon3(len,temp3=Getpoints3((*(UG_flarea3op *)cmd).pts,len),
				cxform, &ug_gksstli.vtran[(*findprms).curxform].vport,segptr2);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
			if (ug_find.found!=true) irtn=0;
			break;
	 case UG_FLAREAOP: len=(*(UG_flareaop *)cmd).len;
			ug_findpolygon2(len,temp=Getpoints((*(UG_flareaop *)cmd).pts,len),
				cxform, &ug_gksstli.vtran[(*findprms).curxform].vport,segptr2);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp);
#endif
			if (ug_find.found!=true) irtn=0;
			break;
	 case UG_FLAREARASOP: len=(*(UG_flarearasop *)cmd).len;
			/*ug_flaras(len,&(*cmd).gcint[2],(*segptr).userdata[0]); */
			/*if (ug_find.found!=true) irtn=0;*/
			break;
	case UG_CELLOP:	
			/*ug_dcellarray(&(*cmd).gcreal[1],&(*cmd).gcint[5],&(*cmd).gcint[7]);*/
			/*if (ug_find.found!=true) irtn=0;*/
			break;
	case UG_CELLRUNOP:
			/*{
				struct celrunop {
						int op;
						Gwrect rect;
						Gipoint dims;
						int totalruns;
						int arrays[10000];
				} *prms;		
				int dy,totalruns;
				prms=(struct celrunop *)(*cmd).gcint;
				dy=(*prms).dims.y;
				totalruns=(*prms).totalruns;
				ug_dcellrunarray(&(*prms).rect,&(*prms).dims,totalruns,
						&(*prms).arrays[0],&(*prms).arrays[dy],
						&(*prms).arrays[dy+totalruns]);
			}*/
			/*if (ug_find.found!=true) irtn=0;*/
			break;
	case UG_CELLRASOP:
			/*ug_dcellras(&(*cmd).gcint[1],&(*cmd).gcint[5],&(*cmd).gcint[7]);*/
			/*if (ug_find.found!=true) irtn=0;*/
			break;
	case UG_CELLRUNRASOP:
			/*{
				/*struct celrunrasop {			
				/*int op;
				/*Gipoint np,nq,nr;				/* parallelogram to put cellarray */
				/*Gipoint dims;					/* dimensions of colorarray */
				/*int totalruns;					/* total number of runs */
				/*	int arrays[10000];			/* really as long as needed to hold
															nrunrow,lens, and colorarray */
				/*} *runrasseg;
				/*Gipoint rect[2];
				/*int *nrunrow,*lens;
				/*Gcolor *a;
				/*int aindx,lensindx,nrunrowindx;

				/* call workstation for cell run raster */
				/*runrasseg=(struct celrunrasop *)(*cmd).gcint;
				/*rect[0].x=(*runrasseg).np.x;
				/*rect[0].y=(*runrasseg).np.y;
				/*rect[1].x=(*runrasseg).nq.x;
				/*rect[1].y=(*runrasseg).nr.y;
				/*nrunrowindx=0;
				/*lensindx=(*runrasseg).dims.y;
				/*aindx=lensindx+(*runrasseg).totalruns;
				/*ug_dcellrunras(rect,&(*runrasseg).dims,	
					&(*runrasseg).arrays[nrunrowindx],
					&(*runrasseg).arrays[lensindx],&(*runrasseg).arrays[aindx]);
			/*}*/
			/*if (ug_find.found!=true) irtn=0;*/
			break;
	case UG_PROCOP: 
		/*	{ 
				/*int (*p)();      /* p points to fctn returning int */
         	/*int len;
         	/*p=(int (*)())((*cmd).gcptr[1]);   /* cast ptr to an int fctn */
         	/*len=(int)((*cmd).gcptr[2]);
				/*uu_denter2(UU_GITRC,(us,"gprocop. p=%x, len=%d, prms=",p,len));
				/*uu_dexit;
          	/*for (i=0; i<len; i++) {
					/*uu_dprint(UU_GITRC,(us,"%x ",(*cmd).gcint[i+3]));
				/*}
         	/*(*p)(len,&(*cmd).gcint[3]);           /* call the user fctn */
         /*} */
			break;
    case UG_CALLOP: 
	 	{
			int irtn2;
			UG_findprms newprms;
			m=(*(UG_callop *)cmd).segno; 					/* call seg m */
			zbytecp(newprms,(*findprms));
        	ug_matmp(newprms.gmodxf,(*findprms).gmodxf,lmodxf);
         irtn2=ug_findsgpk(m,&newprms,vlst,vlen);			/* recursive call */
			if (irtn2!=1) irtn=0;
		}
		break;
	case UG_SNTRANOP: 
		if ((*findprms).curxform!=(*(UG_sntranop *)cmd).xform) {
			Gtran tmp;
			(*findprms).curxform=(*(UG_sntranop *)cmd).xform;
			ug_matmp(tmp,(*findprms).gmodxf,lmodxf);
			ug_mat3mp(cxform,
				(*findprms).gmodxf,lmodxf,ug_vxform[(*findprms).curxform]);
		}
		break;
	case UG_MTRANOP: 
			/*ug_smodxf(&((*cmd).gcreal[1]), (*cmd).gcint[17]); */
			break;
	case UG_LMTRANOP: 
			/*ug_slmodxf(&((*cmd).gcreal[1]), (*cmd).gcint[17]); */
			break;
   case UG_FONTOP:
			zbytecp((*findprms).prats.txbundl.fp,(*(UG_fontop *)cmd).p);
			break;
   case UG_CHHGTOP:
			(*findprms).prats.txht=(*(UG_chhgtop *)cmd).height; 
			break;
	case UG_CHEXPOP: 
			(*findprms).prats.txbundl.expn=(*(UG_chexpop *)cmd).expn;
			break;
   case UG_CHPLANEOP: 
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			ug_ItoGpoint3((*findprms).prats.txpvec,(*(UG_chplaneop *)cmd).txpvc);
#else
			zbytecp((*findprms).prats.txpvec,(*(UG_chplaneop *)cmd).txpvc);
#endif
  			break;
   case UG_CHUP3OP: 
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			ug_ItoGpoint3((*findprms).prats.txuv,(*(UG_chup3op *)cmd).upvec);
#else
			zbytecp((*findprms).prats.txuv,(*(UG_chup3op *)cmd).upvec);
#endif
        break;
   case UG_CHUP2OP:
			(*findprms).prats.txuv.x=(*(UG_chup2op *)cmd).upvec.x;
			(*findprms).prats.txuv.y=(*(UG_chup2op *)cmd).upvec.y;
			(*findprms).prats.txuv.z=0.;
			break;
   case UG_CHPATHOP: 
			(*findprms).prats.txpath=(*(UG_chpathop *)cmd).path;
			break;
   case UG_CHSPACEOP:
			(*findprms).prats.txbundl.space=(*(UG_chspaceop *)cmd).spacing;
			break;
   case UG_CHJUSTOP:
			zbytecp((*findprms).prats.txalign,(*(UG_chjustop *)cmd).align);
			break;
   case UG_SYMBOLOP:
			(*findprms).prats.mkbundl.type=(*(UG_symbolop *)cmd).type;
			break;
   case UG_PICKIDOP: (*findprms).pkid=(*(UG_pickidop *)cmd).pid;
			break;
    }											/* switch(opcode)  */
	}                 					/* normal command */
/*
.....Let's call the Pick verification routine now,
.....so that it can disallow the currently
.....active picked segment prior to saying
.....that we found it.
.....These changes have been made because Roberta's
.....previous changes did not allow for picking
.....individual commands in a 'Feature' segment.
.....It would always pick the last command in the segment.
.....Bobby  -  2/25/92
*/
	if (ug_find.found==true)
	{
		if (ug_find.func!=NULL)
			(*ug_find.func)(*vlen,vlst,&ug_find);
	}
/*
.....This segment command was inside the
.....pick area and passed verification
*/
	if (ug_find.found==true) {
		foundit=true;						/* remember we found something */
		ug_find.found=false;
/*
.....Do not decrease aperture
.....if doing a complete region in or out
.....Else it will not find multiple entity segments
.....such as composite curves and drafting entities
.....Bobby  -  6/3/92
*/
		if (ug_find.pikmode != 2 && ug_find.pikmode != 3)
		{
			ug_find.epsx=ug_find.dist;	/* decrease distance. keep looking */
			ug_find.epsy=ug_find.dist;
		}
		ug_find.pickid=(*findprms).pkid;	/* remember pkid of found*/

/*		if (ug_find.func!=NULL){ 		/* call a user supplied function */
/*			(*ug_find.func)(*vlen,vlst,&ug_find);
/*
............Keep looking through segment in case we find that a label is displayed.
............The user might have selected the label.....
............commented out next two lines to continue search. - RAZ
*/
			/* if (ug_find.find==false) break;	/* user func says stop looking */
			/* foundit=false;					/* keep looking */
/*		}*/
   }
  }                                /* for each command in seg */
  /*if (ug_ndcseg>0) {
		/*(*segptr).ndcboxok=1;			/* ndc box is now up to date */
		/*if (((*segptr).ndcbox.ll.x>(*segptr).ndcbox.ur.x)||
		((*segptr).ndcbox.ll.y>(*segptr).ndcbox.ur.y)) {
			(*segptr).ndcbox.ll.x=0.; (*segptr).ndcbox.ll.y=0.;
			(*segptr).ndcbox.ur.x=EPS; (*segptr).ndcbox.ur.y=EPS;
		}
		uu_dprint(UU_GITRC,(us,"ug_findsgpk. seg %d box=%g %g, %g %g",
			(*segptr).segid,
			(*segptr).ndcbox.ll.x,(*segptr).ndcbox.ll.y,
			(*segptr).ndcbox.ur.x,(*segptr).ndcbox.ur.y));
	}
	/* reset NDC COORDS OK flag */
	/*msk=(1<<(UG_MAXNTRAN+2));
	/*(*segptr).xforms = (*segptr).xforms & (~msk);	*/

/*
.......So that search will continue...commented out next line: (RAZ)
.......  ug_find.found=foundit; 
*/
/*
.....Let's put this line back, because
.....we could have traversed multiple commands
.....in the segment, with only some of the
.....commands located inside the pick window
.....Bobby  -  2/25/92
*/
	ug_find.found = foundit;

	if (foundit==false) 
		*vlen= *vlen-1;				/* remove this viewsg activation from list */
	
rtn:	uu_dprint(UU_GITRC,(us,"%d=ug_findsgpk.",irtn));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_findpolyln3(n,points,cxform,rect,segptr)
**       find polyline-3D
**    PARAMETERS   
**       INPUT  : 
**							int n;
**							Gwpoint3 points[];
**							Gtran cxform; -- composite (modelling,viewing) xform 
**							Gnrect3 *rect;	--  clipping rectangle
**							UG_segstli *segptr;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_findpolyln3(n,points,cxform,rect,segptr)
int n;
Gwpoint3 points[];
Gtran cxform;						/* composite (modelling,viewing) xform */
Gnrect3 *rect;						/* clipping rectangle */
UG_segstli *segptr;				/* segment to expand ndc box, or NULL*/
{
	int i;
	Gfloat dist;
	int onscr,chg2;
	Gfloat epsx,epsy;
	Gnpoint3 np1,np2;
	Gnpoint3 pos1,pos2;

	uu_denter(UU_GITRC,(us,"ug_findpolyln3(%d, %g %g %g...)",n,
		points[0].x,points[0].y,points[0].z));
	epsx=ug_find.epsx;			/* save the finding aperture */
	epsy=ug_find.epsy;
	ug_xform(points[0].x,points[0].y,points[0].z,&np1,cxform);
	for (i=1; i<n; i++) { 
		ug_xform(points[i].x,points[i].y,points[i].z,&np2,cxform);
		if (ug_findndcln(&np1,&np2,rect,segptr,epsx,epsy)) break;
		np1=np2;
	}
	if (segptr!=NULL) {
		/* FIX here should expand ndcbox for j=i+1 to n */
	}
	uu_dprint(UU_GITRC,(us,"ug_findpolyln3 returns found=%d",ug_find.found));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION : ug_findshd(cmd, cxform, 
**								(*findprms).curxform,segptr2)
**       find shading segment
**    PARAMETERS   
**       INPUT  : 
**							cmd:
**							Gtran cxform; -- composite (modelling,viewing) xform 
**							Gnrect3 *rect;	--  clipping rectangle
**							UG_segstli *segptr;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added for shading
.....Yurong
*/
ug_findshd(cmd, cxform,rect,segptr)
UG_shadearea *cmd;
Gtran cxform;						/* composite (modelling,viewing) xform */
Gnrect3 *rect;						/* clipping rectangle */
UG_segstli *segptr;				/* segment to expand ndc box, or NULL*/
{
	int i, n;
	Gwpoint3 *points;
	Gfloat dist;
	int onscr,chg2;
	Gfloat epsx,epsy;
	Gnpoint3 np1,np2;
	Gnpoint3 pos1,pos2;

	epsx=ug_find.epsx;			/* save the finding aperture */
	epsy=ug_find.epsy;
	points = cmd->pts;
	n = cmd->len;
	ug_xform(points[0].x,points[0].y,points[0].z,&np1,cxform);
	for (i=1; i<n; i++) { 
		ug_xform(points[i].x,points[i].y,points[i].z,&np2,cxform);
		if (ug_findndcln(&np1,&np2,rect,segptr,epsx,epsy)) break;
		np1=np2;
	}
	if (segptr!=NULL) {
		/* FIX here should expand ndcbox for j=i+1 to n */
	}
	uu_dprint(UU_GITRC,(us,"ug_findpolyln3 returns found=%d",ug_find.found));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_findpolyln(n,points,cxform,rect,segptr)
**       find polyline-2D
**    PARAMETERS   
**       INPUT  : 
**							int n;
**							Gwpoint points[];
**							Gtran cxform; -- composite (modelling,viewing) xform 
**							Gnrect3 *rect;	--  clipping rectangle
**							UG_segstli *segptr;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_findpolyln(n,points,cxform,rect,segptr)
int n;
Gwpoint points[];
Gtran cxform;						/* composite (modelling,viewing) xform */
Gnrect3 *rect;						/* clipping rectangle */
UG_segstli *segptr;				/* segment to expand ndc box, or NULL*/
{
	int i;
	Gfloat dist;
	int onscr,chg2;
	Gfloat epsx,epsy;
	Gnpoint3 np1,np2;
	Gnpoint3 pos1,pos2;

	uu_denter(UU_GITRC,(us,"ug_findpolyln(%d,%g %g...)",n,
		points[0].x,points[0].y));
	epsx=ug_find.epsx;			/* save the finding aperture */
	epsy=ug_find.epsy;
	ug_xform(points[0].x,points[0].y,0.,&np1,cxform);
	for (i=1; i<n; i++) { 
		ug_xform(points[i].x,points[i].y,0.,&np2,cxform);
		if (ug_findndcln(&np1,&np2,rect,segptr,epsx,epsy))  {
			/* this point failed  and we wanted all in or all out, so don't
				need to try any more points in this polymarker.*/
			if (segptr!=NULL)  {
				/** FIX Here should expand ndcbox with rest of points */
			}
			break;
		}
		np1=np2;
	}													/* end for (i=1; i<n) */
	uu_dprint(UU_GITRC,(us,"ug_findpolyln returns found=%d",ug_find.found));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  int ug_findndcln(&np1,&np2,rect,segptr,epsx,epsy)
**       Find out if NDC line from np1 to np2 is within (without,etc) 
**			epsx,epsy distance of ug_find.x,y. Also must be within rect.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      :  1 if looking for all in or all out and this line
**							failed. Should cause the loop containing ug_findndcln
**							call to stop calling ug_Findndcln and just calculate
**							NDCbox if needed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_findndcln(np1,np2,rect,segptr,epsx,epsy)
Gnpoint3 *np1,*np2;					/* ndc endpoints of line */
Gnrect3 *rect;							/* clipping rectangle */
UG_segstli *segptr;					/* non-NULL -> expand ndcbox */
Gfloat epsx,epsy;
{
	int irtn;
	Gnpoint3 pos1,pos2;
	int onscr,chg2;
	Gfloat dist;

	uu_denter(UU_GITRC,(us,"ug_findndcln(%g %g %g, %g %g %g,..)",
		(*np1).x,(*np1).y,(*np1).z,(*np2).x,(*np2).y,(*np2).z));

	irtn=0;
	ug_cliprect(np1,np2,&pos1,&pos2,&onscr,&chg2,rect,UG_CLIP);
	if (onscr!=0) {
/*		if (segptr!=NULL)*/
/*			ug_ndcboxadd(segptr,&pos1,&pos2);	/* expand seg's ndc box */
		/* see if the line from pos1 to pos2 is within (without,
			part in/out) epsx,epsy of ug_find.x,y. Use  the same
			epsx,epsy for each line segment in this polyline. */
     	if (ug_closln(&ug_find.x,&pos2,&pos1,epsx,epsy,
				&dist,ug_find.pikmode)==1) {		/* found it */
			/* remember we found it */
			if ((ug_find.found==UG_FALSE)||(dist<ug_find.dist)) {
				/* 1st segment found, or closest so far */
				ug_find.dist=dist;
				ug_find.epsx=dist;
				ug_find.epsy=dist;
				ug_find.found=UG_TRUE; 
			}
			/* if we are looking for part in or partout, this line segment
				passing the test causes the whole polyline to pass */
			if ((ug_find.pikmode==0)||(ug_find.pikmode==1))  {
				/* FIX: here should finish expanding box if segptr!=NULL */
				irtn=1; goto rtn;			/* exit the for each line segment loop */
			}
		}
		else {					/*  didn't find it */
			/* if we are looking for all in or all out for this
				polyline, this line segment failing the test causes
				the whole polyline to fail */
			if ((ug_find.pikmode==2)||(ug_find.pikmode==3)) {
				ug_find.found=UG_FALSE;
				ug_find.epsx=epsx;		/* put back the original aperture*/
				ug_find.epsy=epsy;
				/* FIX: here should finish expanding box if segptr!=NULL */
				irtn=1; goto rtn;			/* exit the for each line segment loop */
			}
		}
	}												/* end if (onscr!=0) */
rtn:	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_findpolymk3(n,points,cxform,rect,segptr)
**       find polymarker-3D
**    PARAMETERS   
**       INPUT  : 
**							int n;
**							Gwpoint3 points[];
**							Gtran cxform; -- composite (modelling,viewing) xform 
**							Gnrect3 *rect;	--  clipping rectangle
**							UG_segstli *segptr;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_findpolymk3(n,points,cxform,rect,segptr)
int n;
Gwpoint3 points[];
Gtran cxform;						/* composite (modelling,viewing) xform */
Gnrect3 *rect;						/* clipping rectangle */
UG_segstli *segptr;				/* segment to expand ndc box, or NULL*/
{
	int i;
	Gfloat dist;
	int in,wantin;
	Gfloat dx,dy;
	Gfloat epsx,epsy;
	Gnpoint3 np;
#define EPSMRK (UU_REAL) .0001

	uu_denter(UU_GITRC,(us,"ug_findpolymk3(%d, %g %g %g...)",n,
			points[0].x,points[0].y,points[0].z));
	epsx=ug_find.epsx;			/* save the finding aperture */
	epsy=ug_find.epsy;
	for (i=0; i<n; i++) { 
		/* convert marker point to NDC coordinates */
		ug_xform(points[i].x,points[i].y,points[i].z,&np,cxform);
		/* see if NDC marker point is withhin (without, etc.),
			Set ug_find stuff */
		if (ug_findndcpmk(&np,rect,segptr,epsx,epsy)){
			/* this point failed  and we wanted all in or allout, so don't
				need to try any more points in this polymarker.*/
			if (segptr!=NULL)  {
				/*	Here should expand ndcbox with rest of points */
			}
			break;
		}
	}												/* end for (i=1; i<n) */
	uu_dprint(UU_GITRC,(us,"ug_findpolymk3 returns found=%d",ug_find.found));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_findpolymk2(n,points,cxform,rect,segptr)
**       find polymarker-2D
**    PARAMETERS   
**       INPUT  : 
**							int n;
**							Gwpoint points[];
**							Gtran cxform; -- composite (modelling,viewing) xform 
**							Gnrect3 *rect;	--  clipping rectangle
**							UG_segstli *segptr;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_findpolymk2(n,points,cxform,rect,segptr)
int n;
Gwpoint points[];
Gtran cxform;						/* composite (modelling,viewing) xform */
Gnrect3 *rect;						/* clipping rectangle */
UG_segstli *segptr;				/* segment to expand ndc box, or NULL*/
{
	int i;
	Gfloat dist;
	int in,wantin;
	Gfloat dx,dy;
	Gfloat epsx,epsy;
	Gnpoint3 np;

	uu_denter(UU_GITRC,(us,"ug_findpolymk2(%d, %g %g...)",n,
			points[0].x,points[0].y));
	epsx=ug_find.epsx;			/* save the finding aperture */
	epsy=ug_find.epsy;
	for (i=0; i<n; i++) { 
		/* convert marker point to NDC coordinates */
		ug_xform(points[i].x,points[i].y,0.,&np,cxform);
		/* see if NDC marker point is withhin (without, etc.),
			Set ug_find stuff */
		if (ug_findndcpmk(&np,rect,segptr,epsx,epsy)){
			/* this point failed  and we wanted all in or allout, so don't
				need to try any more points in this polymarker.*/
			if (segptr!=NULL)  {
				/*	Here should expand ndcbox with rest of points */
			}
			break;
		}
	}												/* end for (i=1; i<n) */
	uu_dprint(UU_GITRC,(us,"ug_findpolymk2 returns found=%d",ug_find.found));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  int ug_findndcpmk(&np,rect,segptr,epsx,epsy)
**       description
**    PARAMETERS   
**       INPUT  : 
**							Gnpoint *np;
**							Gnrect3 *rect;
**							UG_segstli *segptr;
**							Gfloat epsx,epsy;
**       OUTPUT :  
**    RETURNS      : 1 if we want all in or allout and this point fails.
**							This should break the loop calling ug_findndcpmk.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_findndcpmk(np,rect,segptr,epsx,epsy)
Gnpoint *np;							/* NDC marker point */
Gnrect3 *rect;							/* clip rect */
UG_segstli *segptr;					/* non-NULL -> expand seg's NDC box */
Gfloat epsx,epsy;						/* original pick half-aperture */
{
	Gfloat dx,dy;
	int wantin,in;
	Gfloat dist;
	int irtn;
	uu_denter(UU_GITRC,(us,"ug_findndcpmk(%g %g...)",(*np).x,(*np).y));
	
	irtn=0;
	if (segptr!=NULL) {
		/* see if if marker is within clip rect */
		if (((*np).x>=(*rect).llf.x)&&((*np).y>=(*rect).llf.y)
			&&((*np).x<=(*rect).urb.x)&&((*np).y>=(*rect).urb.y)) {
			/* marker is within clip rect. expand box */		
/*			(*segptr).ndcbox.ll.x= 
/*				(((*np).x-EPSMRK)<(*segptr).ndcbox.ll.x) ? 
/*				((*np).x-EPSMRK) : (*segptr).ndcbox.ll.x;
/*			(*segptr).ndcbox.ll.y= 
/*				(((*np).y-EPSMRK)<(*segptr).ndcbox.ll.y) ? 
/*				((*np).y-EPSMRK) : (*segptr).ndcbox.ll.y;
/*			(*segptr).ndcbox.ur.x= 
/*				(((*np).x+EPSMRK)>=(*segptr).ndcbox.ur.x) ? 
/*				((*np).x+EPSMRK) : (*segptr).ndcbox.ur.x;
/*			(*segptr).ndcbox.ur.y= 
/*				(((*np).y+EPSMRK)>=(*segptr).ndcbox.ur.y) ? 
/*				((*np).y+EPSMRK) : (*segptr).ndcbox.ur.y;
/*			uu_dprint(UU_GITRC,(us,"ug_findndcpmk. new ndcbox=%g %g %g %g",
/*				(*segptr).ndcbox.ll.x,(*segptr).ndcbox.ll.y,
/*				(*segptr).ndcbox.ur.x,(*segptr).ndcbox.ur.y));*/
		}
	}
	/* see if marker is within (without) rect centered at ug_find.x,y.
		Use same epsx,epsy for each marker in polymarker. */
	if ((ug_find.pikmode==0)||(ug_find.pikmode==2)) wantin=1;
	else wantin=0;
	dx=fabs(ug_find.x-(*np).x);
	dy=fabs(ug_find.y-(*np).y);
	if ((dx<ug_find.epsx)&&(dy<ug_find.epsy)) in=1;
	else in=0;
	uu_dprint(UU_GITRC,(us,
	"ug_findndcpmk. find.mode=%d,find.xy=%g %g, find.eps=%g %g",
			ug_find.pikmode,ug_find.x,ug_find.y,ug_find.epsx,ug_find.epsy));
	if (wantin==in) {						/* found it, either in or out*/
		dist=(dx>dy)?dx:dy;	/* max(dx,dy) */
		/* remember we found it. */
		if ((ug_find.found==UG_FALSE)||(dist<ug_find.dist)) {
			/* 1st segment found, or closest so far */
			ug_find.dist=dist;
			ug_find.epsx=dist;
			ug_find.epsy=dist;
			ug_find.found=UG_TRUE; 
			/* if we are looking for part in or partout, this marker
				passing the test causes the whole polymarker to pass */
			if ((ug_find.pikmode==0)||(ug_find.pikmode==1))  {
				/* here should finish expanding box if segptr!=NULL */
				irtn=1; goto rtn;			/* exit the for each line segment loop */
			}
		}
		uu_dprint(UU_GITRC,(us,"ug_findndcpmk. found it. dist=%g",
				ug_find.dist));
	}											/* end found it */
	else {									/*  didn't find it */
		/* if we are looking for all in or all out for this
			polymarker, this marker failing the test causes
			the whole polymarker to fail */
		if ((ug_find.pikmode==2)||(ug_find.pikmode==3)) {
			ug_find.found=UG_FALSE;
			ug_find.epsx=epsx;			/* put back the original aperture*/
			ug_find.epsy=epsy;
			/* here should finish expanding box if segptr!=NULL */
			irtn=1; goto rtn;			/* exit the for each marker loop */
		}
	}											/* end didn't find it */
rtn:	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_findtext(posn,str,cxform,rect,segptr,prats) -- find text.
**    PARAMETERS   
**       INPUT  : 
**							Gwpoint3 *posn; -- start posn of text
**							char *str; -- the text.
**							Gtran cxform; -- composite (modelling,viewing) xform 
**							Gnrect3 *rect;	--  clipping rectangle
**							UG_segstli *segptr;
**							UG_prat3 *prats;
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_findtext(posn,str,cxform,rect,segptr,prats)	/* find text.*/
Gwpoint3 *posn; 				/* start posn of text */
char *str; 						/* the text. */
Gtran cxform; 					/* composite (modelling,viewing) xform  */
Gnrect3 *rect;					/* clipping rectangle */
UG_segstli *segptr;
UG_prat3 *prats;				/* prim atts (only text attributes used) */
{
	Gnrect3 nrect;				/* text NDC rectangle */
	Gnpoint3 npos;				/* NDC text start posn */
	Gfloat dx,dy;
	Gfloat dxl,dyl,dxr,dyu;

	uu_denter(UU_GITRC,(us,"ug_findtext(%g %g %g,%s..) piktype=%d",
		(*posn).x,(*posn).y,(*posn).z,str,ug_find.pikmode));
	ug_xform((*posn).x,(*posn).y,(*posn).z,&npos,cxform);
	ug_txrect2(posn,str,&nrect,cxform,prats);	/* get nrect=text NDC rectangle */
	dxl=nrect.llf.x-ug_find.x;
	dxr=ug_find.x-nrect.urb.x;
	dx=(dxl>dxr)?dxl:dxr;	/* dx=distance ug_find.x is from text rect, 
										negative if within rectangle */
	dyl=nrect.llf.y-ug_find.y;
	dyu=ug_find.y-nrect.urb.y;
	dy=(dyl>dyu)?dyl:dyu;	/* dy=distance ug_find.y is from text rect 
												or negative if within rectangle */
	switch(ug_find.pikmode) {
	case 0:							/* part or all in */
		/* see if ug_find posn is within epsx,epsy of text rectangle */
		if ((dx<ug_find.epsx)&&(dy<ug_find.epsy)) {
			ug_find.found=UG_TRUE;
			ug_find.dist=(dx>dy)?dx:dy;
			if (ug_find.dist<0.) ug_find.dist=0.;
		}
		uu_dprint(UU_GITRC,(us,
				"ug_findtext. finding loc=%g %g, rect=%g %g, %g %g, found=%d",
				ug_find.x,ug_find.y,nrect.llf.x,nrect.llf.y,nrect.urb.x,
						nrect.urb.y,ug_find.found));
		goto rtn;
	case 1:										/* all or part out */
		if ((nrect.llf.x>(ug_find.x+ug_find.epsx))||
			(nrect.llf.y>(ug_find.y+ug_find.epsy))||
			(nrect.urb.x<(ug_find.x-ug_find.epsx))||
			(nrect.urb.y<(ug_find.y-ug_find.epsy))) {
				ug_find.found=UG_TRUE;
				ug_find.dist=(dx>dy)?dx:dy;
				if (ug_find.dist<0.) ug_find.dist=0.;
			}
			break;
	case 2:									/* text is all within pick aperture */
		if ((nrect.llf.x>(ug_find.x-ug_find.epsx))&&
			(nrect.llf.y>(ug_find.y-ug_find.epsy))&&
			(nrect.urb.x<(ug_find.x+ug_find.epsx))&&
			(nrect.urb.y<(ug_find.y+ug_find.epsy))) {
			ug_find.found=UG_TRUE;
			ug_find.dist=(dx>dy)?dx:dy;
			if (ug_find.dist<0.) ug_find.dist=0.;
		}
		break;
	case 3:										/* all out */
		if ((nrect.llf.x>(ug_find.x+ug_find.epsx))||
			(nrect.llf.y>(ug_find.y+ug_find.epsy))||
			(nrect.urb.x<(ug_find.x-ug_find.epsx))||
			(nrect.urb.y<(ug_find.y-ug_find.epsy))) {
				ug_find.found=UG_TRUE;
				ug_find.dist=(dx>dy)?dx:dy;
				if (ug_find.dist<0.) ug_find.dist=0.;
		}
		break;
	}												/* end switch(ug_find.pikmode) */
rtn:	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION : ug_findpolygon3(len,points,cxform,rect,segptr);
**    PARAMETERS   
**       INPUT  : 
**					int len;							/* number of points
**					Gwpoint3 points[];			/* vertices of polygon
**					Gnrect3 *rect;					/* clipping rectangle
**					UG_segstli *segptr;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_findpolygon3(len,points,cxform,rect,segptr)
int len;							/* number of points */
Gwpoint3 points[];			/* vertices of polygon */
Gtran cxform;
Gnrect3 *rect;					/* clipping rectangle */
UG_segstli *segptr;			/* non-NULL  -> expand ndcbox of segment pointed
										to by segptr */
{
	Gfloat epsx, epsy;	/* Pick box */
	Gint mode;				/* Pick mode */
	Gint count;				/* # of polygons satisfying mode */
	Gnpoint3 ndcpnt[200];/* Polygon NDC coordinates */
	Gfloat dist, minimum;/* Current/minimum distances from line */
	Gint onpoly;			/* Pick box center in polygon */
	Gint i;

	uu_denter(UU_GITRC,(us,"ug_findpolygon3(%d,%g %g %g..)",len,
		points[0].x,points[0].y,points[0].z));

	/* Save find parameter on entry aperature values */
	epsx = ug_find.epsx;
	epsy = ug_find.epsy;

	/* Initialize mode satisfaction count and minimum distance values */
	count = 0;
	minimum = epsx > epsy ? epsx : epsy;

	/* Convert first find point to NDC to boot loop */
	ug_xform(points[0].x,points[0].y,points[0].z,&ndcpnt[0],cxform);

	/* Loop through each point in the polygon */
	for (i = 1; i < len; i++) {
		/* Convert next polygon to NDC */
	   ug_xform(points[i].x,points[i].y,points[i].z,&ndcpnt[i],cxform);

		/* Check polygon side against region box and mode */
		if (ug_closln(&ug_find.x, &ndcpnt[i-1], &ndcpnt[i], epsx, epsy,
			&dist, ug_find.pikmode)) {
			uu_dprint(UU_GITRC,(us,"segment %d passed test",i));
			if (dist < minimum)
				minimum = dist;
			++count;
		}
	}

	/* Display number of segments passing test */
	uu_dprint(UU_GITRC,(us,"%d segments passed, mode %d",count,ug_find.pikmode));

	/* Determine if pick box center point is inside the polygon */
	if (onpoly = ug_inside(&ug_find.x,len,ndcpnt)) {
		uu_dprint(UU_GITRC,(us,"pick box center in polygon %d",onpoly));
		dist = 0.0;
	}

	/* Decide fate of polygon based on pick mode and satisaction count */
	switch (ug_find.pikmode) {
		case UD_SELECTXIN:     /* Select in or cross region */
			/* at least 1 seg in/cross or pick center on poly */
			if (count || onpoly) {
				uu_dprint(UU_GITRC,(us,"satisfies in or cross region"));
				ug_find.found = UG_TRUE;
			}
			break;

		case UD_SELECTXOUT:     /* Select out or cross region */
			/* at least 1 seg out/cross */
			if (count) {
				uu_dprint(UU_GITRC,(us,"satisfies out or cross region"));
				ug_find.found = UG_TRUE;
			}
			break;

		case UD_SELECTIN:     /* Select inside region */
			/* segs all in --> polygon all in */
			if (count == len - 1) {
				uu_dprint(UU_GITRC,(us,"satisfies in region"));
				ug_find.found = UG_TRUE;
			}
			break;

		case UD_SELECTOUT:     /* Select outside region */
			/* segs all out and pick center not on poly --> poly all out */ 
			if (count == len - 1 && !onpoly) {
				uu_dprint(UU_GITRC,(us,"satisfies out region"));
				ug_find.found = UG_TRUE;
			}
			break;
	}

	/* Reset find distance and box size if found */
	if (ug_find.found == UG_TRUE) {
		uu_dprint(UU_GITRC,(us,"set distance and epsilon %f",dist));
		ug_find.dist = ug_find.epsx = ug_find.epsy = dist;
	}

	/* Update NDC box if pointer to segment provided */
	if (segptr!=NULL)
		ug_boxexpfa3(segptr,points,len);

	uu_dprint(UU_GITRC,(us,"ug_findpolygon3 returning %d",ug_find.found));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION : ug_findpolygon2(len,points,cxform,rect,segptr);
**    PARAMETERS   
**       INPUT  : 
**					int len;							/* number of points
**					Gwpoint points[];				/* vertices of polygon
**					Gnrect3 *rect;					/* clipping rectangle
**					UG_segstli *segptr;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_findpolygon2(len,points,cxform,rect,segptr)
int len;							/* number of points */
Gwpoint points[];				/* vertices of polygon */
Gtran cxform;
Gnrect3 *rect;					/* clipping rectangle */
UG_segstli *segptr;			/* non-NULL  -> expand ndcbox of segment pointed
										to by segptr */
{
	int i;
	Gnpoint3 *p;
	Gnpoint3 ndcpts[100];
	int irtn;
	Gfloat dx,dy;

	uu_denter(UU_GITRC,(us,"ug_findpolygon3(%d,%g %g..)",len,
		points[0].x,points[0].y));
	irtn=0;
	if (len<100) p=ndcpts;
	else {						/* polygon is too big to fit in ndcpts */
		p=(Gnpoint3 *)uu_toolmalloc(len*sizeof(Gnpoint3));
		if (p==NULL) {
			irtn=1; uu_dprint(-1,(us,"ug_findpolygon3. out of memory"));
		}
	}
	if (irtn==0) {						/* not out of memory */
		/* FIX: here should clip polygon to rect */
		/* first see if near the 1st vertex, for small polygons */
		/* really should pick it here as a polyline */
		ug_xform(points[0].x,points[0].y,0.,&p[0],cxform);	/* to NDC */
		dx=fabs(p[0].x-ug_find.x);
		dy=fabs(p[0].y-ug_find.y);
		if ((dx<ug_find.epsx)&&(dy<ug_find.epsy)) {	
			ug_find.found=UG_TRUE;
			ug_find.dist=(dx>dy)?dx:dy;	/* max(dx,dy) */
			ug_find.epsx=ug_find.dist;
			ug_find.epsy=ug_find.dist;
		}
		else {							/* wasn't near 1st vertex, see if inside */
			for (i=1; i<len; i++) 		/* xform to NDC */
				ug_xform(points[i].x,points[i].y,0.,&p[i],cxform);	
			if (ug_inside(&ug_find.x,len,p)==1) {		/* point is inside */
				ug_find.found=UG_TRUE;
				ug_find.dist=0.;
				ug_find.epsx=0.;
				ug_find.epsy=0.;
			}
		}										/* end wasn't near 1st vertex. */
		if (len>=100) uu_toolfree(p);
		if (segptr!=NULL) {				/* update ndc box */
			ug_boxexpfa2(segptr,points,len);	/* should do NDC box update */
		}
	}											/* end not out of memory */
	uu_dexit;
}
