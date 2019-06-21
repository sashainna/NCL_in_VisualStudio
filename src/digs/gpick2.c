/*********************************************************************
**    NAME         :  gpick2.c -- ordinary (not area) picking routines.
**       CONTAINS:
**    int  ug_findsgclip(n,findprms,vlst,vlen) 
**		int ug_findclipln(p1,p2, found)
**		ug_findmkclip(pt, found)
**    ug_findpolyln3clip(n,points, found)
**    ug_findpolylnclip(n,points, found)
**    ug_findpolymk3clip(n,points, found)
**    ug_findpolymk2clip(n,points, found)
**    ug_findtext(posn,str,cxform,rect,segptr,prats) -- find text.
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gpick2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:23
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
#include "mfort.h"
#include "mdcoord.h"
#include "modef.h"
#include "nclfc.h"

#define false 0
#define true 1
#define UG_FALSE 0
#define UG_TRUE 1
/* extern char ug_ops[74][16]; */
		/* used for conversion routines - to free used memory */
static Gwpoint3 *temp3;
static Gwpoint *temp;

/*********************************************************************
**    E_FUNCTION     : ug_findmkclip(pt, found)
**			check the marker cliped to a maximum of five user
**			defined planes.
**    PARAMETERS   
**       INPUT  : pt
**
**       OUTPUT :  
**
**    RETURNS      :  1: testing failed
**						0: within planes
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_findmkclip(pt, found)
Gwpoint3 *pt;
int found[6];
{
	int i, iside[6], inc;
	int is1,isw,irtn;
	UU_REAL d1[6],d2[6],tl,tv[3],rat,vdis,svc[3];
	UM_int2 itra, ifl;
	UU_REAL newpt[3];
	UM_real8 refmax[12], sval, buf[11];
	UM_int2 idx, ival,nwds;
	UU_REAL rplane[6][4];
	UM_transf mcstf;
	Gtran mx, gmx;
	UM_real8 wkmax[12], invmax[12];

	for (i=0; i<6; i++)
	{
		rplane[i][0] = UD_selclip.rplane[i][0];
		rplane[i][1] = UD_selclip.rplane[i][1];
		rplane[i][2] = UD_selclip.rplane[i][2];
		rplane[i][3] = UD_selclip.rplane[i][3];
	}
/*
.....if MODSYS
*/
/*
.....this geometry (POINT) and plane values come in are the current value
.....in the origial plane, not count in MODSYS
.....so we need convert to the current working plane value
.....if there is a MODSYS
*/
	mcsmx (&ifl, wkmax);
	if (ifl==1)
	{
		nclf_invmx (wkmax, invmax);
		buf[0] = pt->x;
		buf[1] = pt->y;
		buf[2] = pt->z;
		nwds = 3;
		itra = 3;
		nclf_transf(&(buf[0]),invmax, &nwds, &itra);
		pt->x = buf[0];
		pt->y = buf[1];
		pt->z = buf[2];

		nwds = 7;
		itra = 6;
		for (i=0;i<UD_selclip_npl;i++)
		{
			buf[0] = rplane[i][0];
			buf[1] = rplane[i][1];
			buf[2] = rplane[i][2];
			buf[3] = rplane[i][3];
			buf[4] = buf[5] = buf[6] = buf[7] = 0.0;
			nclf_transf (&(buf[0]), invmax, &nwds, &itra);
			rplane[i][0] = buf[0];
			rplane[i][1] = buf[1];
			rplane[i][2] = buf[2];
			rplane[i][3] = buf[3];
		}
	}
/*
......then sc(68)-sc(79) contain the current refsys reverse matrix
......if REFSYS
*/
	idx = 72; getifl (&idx, &ival);
	if (ival==1)
	{
		for (i=0;i<12;i++)
		{
			idx = (UM_int2)(i+68);
			getsc (&idx, &sval);
			refmax[i] = sval;
		}
		buf[0] = pt->x;
		buf[1] = pt->y;
		buf[2] = pt->z;
		nwds = 3;
		itra = 3;
		nclf_transf(&(buf[0]),refmax, &nwds, &itra);
		pt->x = buf[0];
		pt->y = buf[1];
		pt->z = buf[2];

		itra = 6;
		nwds = 7;
		for (i=0;i<UD_selclip_npl;i++)
		{
			buf[0] = rplane[i][0];
			buf[1] = rplane[i][1];
			buf[2] = rplane[i][2];
			buf[3] = rplane[i][3];
			buf[4] = buf[5] = buf[6] = buf[7] = 0.0;
			nclf_transf (&(buf[0]), refmax, &nwds, &itra);
			rplane[i][0] = buf[0];
			rplane[i][1] = buf[1];
			rplane[i][2] = buf[2];
			rplane[i][3] = buf[3];
		}
	}
/*
.....Loop through clipping planes
*/
	for (inc=0;inc<UD_selclip_npl;inc++)
	{
/*
........Determine side of plane to keep
*/
		iside[inc] = 1;
		if (UD_selclip.side[inc] == 0 && rplane[inc][0] > 0.)
			iside[inc] = 0;
		else if (UD_selclip.side[inc] == 1 && rplane[inc][0] < 0.)
			iside[inc] = 0;
		else if (UD_selclip.side[inc] == 2 && rplane[inc][1] > 0.)
			iside[inc] = 0;
		else if (UD_selclip.side[inc] == 3 && rplane[inc][1] < 0.)
			iside[inc] = 0;
		else if (UD_selclip.side[inc] == 4 && rplane[inc][2] > 0.)
			iside[inc] = 0;
		else if (UD_selclip.side[inc] == 5 && rplane[inc][2] < 0.)
			iside[inc] = 0;
		else if (UD_selclip.side[inc] == 6)
			iside[inc] = 2;
/*
........Determine which side of plane
........points are on
*/
		d1[inc] = (pt->x*rplane[inc][0] +
		           pt->y*rplane[inc][1] +
		           pt->z*rplane[inc][2]) - rplane[inc][3];
      if (fabs(d1[inc]) < UM_DFUZZ) d1[inc] = 0.;
/*
...........Check plane side condition
*/
		if (iside[inc]==2)
		{
			if (d1[inc] == 0.)
			{
				if (UD_selclip.cross[inc]==2)
					found[inc] = 1;
			}
			else if ((UD_selclip.cross[inc]==0)
				|| (UD_selclip.cross[inc]==1))
				found[inc] = 0;
		}
		else
		{
			if ((iside[inc] == 0 && d1[inc] == 0.) ||
				(iside[inc] == 1 && d1[inc] == 0.))
			{
/*
.....1: if it is "ON"(include 'SAME'), this PT then is consider 'in', but not change found[inc]
.....								it is default to 1, no change
.....0: if it is "OFF"(not include 'SAME'), this PT then is not consider 'in', no cross
.....								it is default to 1, change to 0
.....2: if it is "CROSS", this PT then consider 'in', 
.....								it is default to 0, change to 1
*/
				if (UD_selclip.cross[inc]==0)
					found[inc] = 0;
				if (UD_selclip.cross[inc]==2)
					found[inc] = 1;
/*
.....if it is "OFF", this PT then consider on, but not change found[inc] because
.....the default is 1 and if it is 0, it mean other entity is set to 0
.....since it is not cross, do not change it
*/
			}
			else if ((iside[inc] == 0 && d1[inc] < 0.) ||
				(iside[inc] == 1 && d1[inc] > 0.))
			{
				if (UD_selclip.cross[inc]==2)
					found[inc] = 1;
			}
			else
			{
/*
.....if there are point already not on the plane and 'cross' not checked
.....then consider it is not in, otherwise, stay
*/
				if ((UD_selclip.cross[inc]==0)||(UD_selclip.cross[inc]==1))
					found[inc] = 0;
			}
		}
	}
	if ((found[0]==1)&&(found[1]==1)&&(found[2]==1)
		&&(found[3]==1)&&(found[4]==1)&&(found[5]==1))
		ug_find.found = true;
	else
		ug_find.found = false;
	return 0;
}


/*********************************************************************
**    E_FUNCTION     : ug_findclipln(p1,p2, found)
**			check the 2 pts line is within to a maximum of five user
**			defined planes.
**    PARAMETERS   
**       INPUT  : p1, p2: 2 pts line to be tested
**
**       OUTPUT :  
**
**    RETURNS      :  1 finding done. Should cause the loop containing ug_findclipln
**							call to stop calling ug_findclipln
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_findclipln(p1,p2, found)
Gwpoint3 *p1, *p2;
int found[6];
{
	UU_LOGICAL p1_is_in, both_are_in, both_in_one[6];
	int iside[6],inc,i;
	int is1,isw,irtn;
	UU_REAL d1[6],d2[6],tl,tv[3],rat,vdis,svc[3];

	ug_findmkclip(p1, found);
	ug_findmkclip(p2, found);
	return 0;
}

/*********************************************************************
**    I_FUNCTION :  ug_findpolyln3clip(n,points, found)
**       find polyline-3D
**    PARAMETERS   
**       INPUT  : 
**							int n;
**							Gwpoint3 points[];
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_findpolyln3clip(n,points, found)
int n;
Gwpoint3 points[];
int found[6];
{
	int i;
	Gwpoint3 p1, p2;

	p1.x = points[0].x;
	p1.y = points[0].y;
	p1.z = points[0].z;
	for (i=1; i<n; i++) 
	{ 
		p2.x = points[i].x;
		p2.y = points[i].y;
		p2.z = points[i].z;
		ug_findclipln(&p1,&p2, found);
		p1.x = p2.x;
		p1.y = p2.y;
		p1.z = p2.z;
	}
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
ug_findshdclip(cmd, found)
UG_shadearea *cmd;
int found[6];
{
	int i, n;
	Gwpoint3 *points;
	Gwpoint3 p1, p2;

	points = cmd->pts;
	n = cmd->len;

	p1.x = points[0].x;
	p1.y = points[0].y;
	p1.z = points[0].z;
	for (i=1; i<n; i++) 
	{ 
		p2.x = points[i].x;
		p2.y = points[i].y;
		p2.z = points[i].z;
		ug_findclipln(&p1,&p2, found);
		p1.x = p2.x;
		p1.y = p2.y;
		p1.z = p2.z;
	}
}

/*********************************************************************
**    I_FUNCTION :  ug_findpolyln(n,points,found)
**       find polyline-2D
**    PARAMETERS   
**       INPUT  : 
**							int n;
**							Gwpoint points[];
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_findpolylnclip(n,points, found)
int n;
Gwpoint points[];
int found[6];
{
	int i;
	Gwpoint3 p1, p2;

	p1.x = points[0].x;
	p1.y = points[0].y;
	p1.z = 0;
	for (i=1; i<n; i++) 
	{ 
		p2.x = points[i].x;
		p2.y = points[i].y;
		p2.z = 0;
		ug_findclipln(&p1,&p2, found);
		p1.x = p2.x;
		p1.y = p2.y;
		p1.z = 0;
	}
}
/*********************************************************************
**    I_FUNCTION :  ug_findpolymk3clip(n,points)
**       find polymarker-3D
**    PARAMETERS   
**       INPUT  : 
**							int n;
**							Gwpoint3 points[];
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_findpolymk3clip(n,points, found)
int n;
Gwpoint3 points[];
int found[6];
{
	int i;
	Gwpoint3 pt;

	for (i=0; i<n; i++) 
	{ 
		pt.x = points[i].x;
		pt.y = points[i].y;
		pt.z = points[i].z;
		ug_findmkclip(&pt, found);
	}	
}
/*********************************************************************
**    I_FUNCTION :  ug_findpolymk2(n,points)
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
ug_findpolymk2clip(n,points, found)
int n;
Gwpoint3 points[];
int found[6];
{
	int i, count;
	Gwpoint3 pt;

	count = 0;
	for (i=0; i<n; i++) 
	{ 
		pt.x = points[i].x;
		pt.y = points[i].y;
		pt.z = 0;
		ug_findmkclip(&pt, found);
	}	
}

/*********************************************************************
**    I_FUNCTION :  ug_findtext(posn,str) -- find text.
**    PARAMETERS   
**       INPUT  : 
**							Gwpoint3 *posn; -- start posn of text
**							char *str; -- the text.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_findtextclip(posn,str,cxform,rect,segptr,prats)	/* find text.*/
Gwpoint3 *posn; 				/* start posn of text */
char *str; 						/* the text. */
{
}

/*********************************************************************
**    I_FUNCTION     :  int ug_findsgclip(n,findprms,vlst,vlen) -- Find seg n. 
**			find segment n in clip plane range
**    PARAMETERS   
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
int ug_findsgclip(n,findprms,vlst,vlen) 				/* find segment n. */
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
	int found[6];
#define EPS (UU_REAL) .01 

	segptr=ug_segac(n);
	if (segptr==NULL) 
	{ 
		return(0);
	}
	if (segptr->segatts.gdtect==UG_UNDETECTABLE) 
	{ 
		return(0);
	}
	irtn=1;
	vlst[*vlen]=n;  (*vlen)++;
	ug_find.found=false;
	found[0] = found[1] = found[2] = found[3] = found[4] = found[5] = 1;
	for (i=0; i<UD_selclip_npl;i++)
	{
/*
.....FOR "CROSS", if there is a entity on the plane, it is in
*/
		if (UD_selclip.cross[i]==2)
			found[i] = 0;
/*
.....for "OFF/ON", we default all in and if there is an entity is
.....not in (or on), then it is not in, so found[i] default to 1
*/
	}
	ncmd = ug_lsinelt(segptr->seglist);
	if ((ug_ndcseg>0)&&((*segptr).wcboxok==0)) 
		segptr2=segptr;
	else 
		segptr2=NULL;
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
	for( icmd=0; icmd < ncmd; ++icmd )
	{
		cmd = (UG_plylna2op *)ug_lsielt(segptr->seglist,icmd);
		cmdlen = ug_lsilen(segptr->seglist,icmd)/sizeof(int);
		opcode=(*cmd).elttype;
		if (opcode<UG_OPPTR) 
		{
			switch (opcode) 
			{
			case UG_SHADEAREAOP:
				if (!hid_line) 
				{
					ug_findshdclip((UG_shadearea *)cmd, found);
				}
				break;

			case UG_PLYLNA3OP:
				if (wireframe_disp)
				{
					len=(*(UG_plylna3op *)cmd).len;
					ug_findpolyln3clip(len, temp3=Getpoints3((*(UG_plylna3op *)cmd).pts,len), found);
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
					uu_toolfree(temp3);
#endif
				}
				break;
			case UG_PLYLNA2OP:
				if (wireframe_disp)
				{
					len = (*(UG_plylna2op *)cmd).len;
					ug_findpolylnclip(len, temp=Getpoints((*(UG_plylna2op *)cmd).pts,len), found);
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
					uu_toolfree(temp);
#endif
				}
				break;
			case UG_PLYLNRASOP:
				break;
			case UG_PLYMKA3OP:
				len = (*(UG_plymka3op *)cmd).len;
				ug_findpolymk3clip(len, temp3=Getpoints3((*(UG_plymka3op *)cmd).pts,len), found);
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
				uu_toolfree(temp3);
#endif
				break;
			case UG_PLYMKA2OP: len=(*(UG_plymka2op *)cmd).len;
				ug_findpolymk2clip(len,temp=Getpoints((*(UG_plymka2op *)cmd).pts,len), found);
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
				uu_toolfree(temp);
#endif
				break;
			case UG_PLYMKRASOP:
				break;
			case UG_TEXTOP:
				ug_findtextclip(temp3=Getpoints3(&(*(UG_textop *)cmd).position,1),
						(*(UG_textop *)cmd).string,cxform,
						&ug_gksstli.vtran[(*findprms).curxform].vport,
						segptr2,&(*findprms).prats);
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
				uu_toolfree(temp3);
#endif
				break;
			 case UG_TEXTRASOP: len=(*(UG_textrasop *)cmd).len;
				break;
			 case UG_FLAREA3OP: len=(*(UG_flarea3op *)cmd).len;
				ug_findpolyln3clip(len,temp3=Getpoints3((*(UG_flarea3op *)cmd).pts,len), found);
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
				uu_toolfree(temp3);
#endif
				break;
			case UG_FLAREAOP: len=(*(UG_flareaop *)cmd).len;
				ug_findpolylnclip(len,temp=Getpoints((*(UG_flareaop *)cmd).pts,len), found);
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
				uu_toolfree(temp);
#endif
				break;
			case UG_FLAREARASOP: len=(*(UG_flarearasop *)cmd).len;
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
	 		{
				int irtn2;
				UG_findprms newprms;
				m=(*(UG_callop *)cmd).segno; 	
				zbytecp(newprms,(*findprms));
        		ug_matmp(newprms.gmodxf,(*findprms).gmodxf,lmodxf);
				irtn2=ug_findsgclip(m,&newprms,vlst,vlen);			/* recursive call */
				if (irtn2!=1) irtn=0;
			}
			break;
			case UG_SNTRANOP: 
				break;
			case UG_MTRANOP: 
				break;
			case UG_LMTRANOP: 
				break;
			case UG_FONTOP:
				break;
			case UG_CHHGTOP:
				break;
			case UG_CHEXPOP: 
				break;
			case UG_CHPLANEOP: 
  				break;
			case UG_CHUP3OP: 
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
		}
	}                                /* for each command in seg */
	if (ug_find.found==true)
	{
		if (ug_find.func!=NULL)
			(*ug_find.func)(*vlen,vlst,&ug_find);
	}
	else
		*vlen= *vlen-1;		
rtn:
	return(irtn);
}

