/*********************************************************************
**  NAME:  SwNclUnibase1.cpp
**
**       Controlling routines for converting Sketch geometry to
**       the Unibase format.
**
** CONTAINS: CNclUnibase class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			SwNclUnibase1.cpp , 24.2
**    DATE AND TIME OF LAST  MODIFICATION
**			08/13/14 , 14:46:35
*********************************************************************/
//$c1   PSB 09/05/97 Revamp the samples program
//

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <io.h>
#include <fcntl.h>
#include <share.h>
#include <sys/stat.h>
#include <sys/types.h>

// Precompiled header
#include "SwStdAfx.h"


// SolidWorks includes
//#include <amapp.h>
//#include <swconst.h>

// User Dll includes
//#include "userapp.h"

// NCL includes
#include "SwUniOpt.h"
#include "SwUniStat.h"
#include "SwNclUnibase.h"
#include "mcrv.h"
#include "nccs.h"

extern "C" int NclxMdlLabelEntity(char *, int *, int, char *);
extern "C" int NclxMdlSetupData(NCLX_mdl_struct*,int,char *,int);
extern "C" int NclxMdlStoreCurve(NCLX_mdl_curve*,int,int*);
extern "C" int NclxMdlStorePoint (NCLX_mdl_point*, int);
extern "C" int NclxMdlStoreLine (NCLX_mdl_line*, int);
extern "C" int NclxMdlStoreCircle (NCLX_mdl_circle*, int);
extern "C" double um_dot(double *,double *);
extern "C" double um_mag(double *);
extern "C" int    um_vctmsc(double *,double,double *);
extern "C" int    um_unitvc(double *,double *);
extern "C" int    um_cctmtf(double *, double *, double *);
extern "C" int    um_vctmtf(double *, double *, double *);
extern "C" int    um_tftmtf(double *, double *, double *);
extern "C" int    um_cross (double *, double *, double *);
extern "C" char* uu_malloc(int);
extern "C" void uu_free(void  *);
extern "C" int uy_circ_2pts(double*,double*,double*,double,NCLX_mdl_circle*);

/*********************************************************************
**    I_FUNCTION     :  CreateSketches(sklist,nmlist,tfmat)
**       Loops through all SolidWorks sketches, then converts all
**       geometry stored in the sketches and stores them in the Unibase.
**    PARAMETERS
**       INPUT  :
**          sklist    = List array containing list of defined sketches.
**          nmlist    = Part component from SolidWorks.
**          cdsys     = Coordinate system transformation to apply to
**                      sketch geometry.  It can be set to NULL, meaning
**                      do not apply a coordinate system.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreateSketches(LPSKETCH *sklist, char *nmlist, double *cdsys)
{
	int cisub,lnsub,ptsub,i,cvsub,ncvpt,nknot,degree,closed,dim,*iparm,npts;
	long n,size,iret;
	double *ptary,tfmat[16],*cvpts,*knots;
	char *names,sbuf[80];
	HRESULT hres;
	LPSKETCH sketch,*skp;
	LPMATHTRANSFORM xform;
	LPENUMSKETCHSEGMENTS pSketchSegEnum;
	LPSKETCHSEGMENT pSketchSeg;
	LPCURVE pCurve;
//
//...Initialize routine
//
	skp = (LPSKETCH *)uu_lsnext((char *)sklist);
	names = uu_lsnext(nmlist);
	cisub = 0;
	lnsub = 0;
	ptsub = 0;
	cvsub = 0;
	npts = 0;
	ptary = 0;
//
//...Loop through sketches
//
	while (skp != NULL)
	{
		sketch = *skp;
		sprintf(sbuf,"Processing Sketch: %s",names);
		m_statDlg->AddList(" ");
		m_statDlg->AddList(sbuf);
		sprintf(sbuf,"\nSketch: %s\n",names);
		my_this->list_file_out(sbuf);
//
//...Get Sketch to Model transform
//
		sketch->get_ModelToSketchTransform(&xform);
		xform->IInverse(&xform);
		xform->get_IArrayData(tfmat);
		if (tfmat[12] != 1.0)
		{
			for (i=0;i<9;i++) tfmat[i] = tfmat[12];
		}
		tfmat[9] /= .0254;
		tfmat[10] /= .0254;
		tfmat[11] /= .0254;
//
//...Apply coordinate system matrix
//
		if (cdsys != NULL) um_tftmtf(tfmat,cdsys,tfmat);
		xform->Release();
//
//...Convert points
//
		sketch->GetUserPointsCount(&n);
		if (n != 0)
		{
			sprintf(sbuf,"Converting %d Points...",n);
			m_statDlg->AddList(sbuf);
			size = sizeof(double) * 8 * n;
			if (size > npts)
			{
				if (ptary != 0) uu_free(ptary);
				ptary = (double *)uu_malloc(size);
				npts = size;
			}
			hres = sketch->IGetUserPoints2(n,ptary);
			if (hres == S_OK)
			{
				for (i=0;i<n;i++) CreatePoint(&ptary[i*8],&ptsub,tfmat);
			}
			sprintf(sbuf,"Converted %d Points\n",n);
			my_this->list_file_out(sbuf);
			m_sketchCount += n;
		}
//
//...Convert lines
//
		sketch->GetLineCount2(1,&n);
		if (n != 0)
		{
			sprintf(sbuf,"Converting %d Lines...",n);
			m_statDlg->AddList(sbuf);
			size = sizeof(double) * 12 * n;
			if (size > npts)
			{
				if (ptary != 0) uu_free(ptary);
				ptary = (double *)uu_malloc(size);
				npts = size;
			}
			hres = sketch->IGetLines2(1,n,ptary);
			if (hres == S_OK)
			{
				for (i=0;i<n;i++) CreateLine(&ptary[i*12],&lnsub,tfmat);
			}
			sprintf(sbuf,"Converted %d Lines\n",n);
			my_this->list_file_out(sbuf);
			m_sketchCount += n;
		}
//
//...Convert circles
//
		sketch->GetArcCount(&n);
		if (n != 0)
		{
			sprintf(sbuf,"Converting %d Arcs...",n);
			m_statDlg->AddList(sbuf);
			size = sizeof(double) * 16 * n;
			if (size > npts)
			{
				if (ptary != 0) uu_free(ptary);
				ptary = (double *)uu_malloc(size);
				npts = size;
			}
			hres = sketch->IGetArcs2(n,ptary);
			if (hres == S_OK)
			{
				for (i=0;i<n;i++) CreateCircle(&ptary[i*16],&cisub,tfmat);
			}
			sprintf(sbuf,"Converted %d Arcs\n",n);
			my_this->list_file_out(sbuf);
			m_sketchCount += n;
		}
//
//...Convert splines
//
		pSketchSeg = UU_NULL;
		sketch->IEnumSketchSegments(&pSketchSegEnum);
		if (pSketchSegEnum != NULL)
		{
			iret = -1;
			hres = pSketchSegEnum->Next(1,&pSketchSeg,&iret);
			i = 0;
			while (hres == S_OK)
			{
				hres = pSketchSeg->GetType(&n);
				if (n == swSketchELLIPSE || n == swSketchSPLINE ||
					n == swSketchPARABOLA) i++;
				pSketchSeg->Release();
				hres = pSketchSegEnum->Next(1,&pSketchSeg,&iret);
			}
			if (i != 0)
			{
				sprintf(sbuf,"Converting %d Curves...",i);
				m_statDlg->AddList(sbuf);
				iret = -1;
				pSketchSegEnum->Reset();
				hres = pSketchSegEnum->Next(1,&pSketchSeg,&iret);
				while (hres == S_OK)
				{
					hres = pSketchSeg->GetType(&n);
					if (n == swSketchELLIPSE || n == swSketchSPLINE ||
						n == swSketchPARABOLA)
					{
						pSketchSeg->IGetCurve(&pCurve);
						pCurve->IGetBCurveParamsSize2(TRUE,FALSE,&size);
						size = size * sizeof(double);
						if (size > npts)
						{
							if (ptary != 0) uu_free(ptary);
							ptary = (double *)uu_malloc(size);
							npts = size;
						}
						hres = pCurve->IGetBCurveParams(ptary);
						iparm = (int *)ptary;
						dim = iparm[0];
						degree = iparm[1];
						ncvpt = iparm[2];
						closed = iparm[3];
						knots = &ptary[2];
						nknot = ncvpt + degree;
						cvpts = &ptary[2+nknot];
						CreateSpline(cvpts,ncvpt,knots,nknot,dim,degree,closed,&cvsub,
							tfmat);
						pCurve->Release();
					}
					pSketchSeg->Release();
					hres = pSketchSegEnum->Next(1,&pSketchSeg,&iret);
				}
				sprintf(sbuf,"Converted %d Curves\n",i);
				my_this->list_file_out(sbuf);
				m_sketchCount += i;
			}
		}
//
//......Point to next sketch
//
		sketch->Release();
		skp = (LPSKETCH *)uu_lsnext((char *)skp);
		names = uu_lsnext(names);
	}
//
//...Free memory
//
	if (ptary != 0) uu_free(ptary);
}

/*********************************************************************
**    I_FUNCTION     :  CreatePoint(ptrec,sub,tfmat)
**       Creates a Unibase point entity from a SolidWorks point and
**       stores it in the Unibase.
**    PARAMETERS
**       INPUT  :
**          ptrec     = SolidWorks point.
**          sub       = Subscript number for points.  Initial value should
**                      be 0.
**          tfmat     = Sketch to Model system xform, including any user
**                      activated transformation.  Can be set to NULL if
**                      there is no transformation to apply.
**       OUTPUT :
**          sub       = Updated subscript number.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreatePoint(double *ptrec, int *sub, double *tfmat)
{
	int subscr;
	char lab[64];
	NCLX_mdl_point point;
//
//...Label point
//
	(*sub)++;
	if (m_swopt.label_opts == 1)
	{
		NclxMdlLabelEntity(lab,sub,1,"QZ");
		subscr = *sub;
	}
	else
	{
		NclxMdlLabelEntity(lab,sub,m_swopt.ptlab_type,m_swopt.ptlab);
		subscr = *sub;
		if (m_swopt.ptlab_type == 0) subscr = 0;
	}
//
//...Initialize point record
//
	point.header.attrib.displayable = 0;
	NclxMdlSetupData(&point.header,NCLX_MDL_POINT,lab,subscr);
	point.header.attrib.color = NCLX_RED;
//
//...Convert SolidsWorks point to Unibase point
//
	um_vctmsc(&ptrec[5],1./.0254,point.pt);
//
//...Run point through transformation
//
	if (tfmat) um_cctmtf(point.pt,tfmat,point.pt);
//
//...Store circle in Unibase
//
	NclxMdlStorePoint(&point,NCLX_TRUE);
}

/*********************************************************************
**    I_FUNCTION     :  CreateLine(lnrec,sub,tfmat)
**       Creates a Unibase line entity from a SolidWorks line and
**       stores it in the Unibase.
**    PARAMETERS
**       INPUT  :
**          lnrec     = SolidWorks line.
**          sub       = Subscript number for lines.  Initial value should
**                      be 0.
**          tfmat     = Sketch to Model system xform, including any user
**                      activated transformation.  Can be set to NULL if
**                      there is no transformation to apply.
**       OUTPUT :
**          sub       = Updated subscript number.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreateLine(double *lnrec, int *sub, double *tfmat)
{
	int subscr;
	char lab[64];
	NCLX_mdl_line line;
//
//...Label line
//
	(*sub)++;
	if (m_swopt.label_opts == 1)
	{
		NclxMdlLabelEntity(lab,sub,1,"QZ");
		subscr = *sub;
	}
	else
	{
		NclxMdlLabelEntity(lab,sub,m_swopt.lnlab_type,m_swopt.lnlab);
		subscr = *sub;
		if (m_swopt.lnlab_type == 0) subscr = 0;
	}
//
//...Initialize circle record
//
	line.header.attrib.displayable = 0;
	NclxMdlSetupData(&line.header,NCLX_MDL_LINE,lab,subscr);
	line.header.attrib.color = NCLX_GREEN;
//
//...Convert SolidsWorks line to Unibase line
//
	um_vctmsc(&lnrec[6],1./.0254,line.spt);
	um_vctmsc(&lnrec[9],1./.0254,line.ept);
//
//...Run point through transformation
//
	if (tfmat)
	{
		um_cctmtf(line.spt,tfmat,line.spt);
		um_cctmtf(line.ept,tfmat,line.ept);
	}
//
//...Store line in Unibase
//
	NclxMdlStoreLine(&line,NCLX_TRUE);
}

/*********************************************************************
**    I_FUNCTION     :  CreateCircle(circle,sub,tfmat)
**       Creates a Unibase circle entity from a SolidWorks circle and
**       stores it in the Unibase.
**    PARAMETERS
**       INPUT  :
**          cirrec    = SolidWorks circle.
**          sub       = Subscript number for circles.  Initial value should
**                      be 0.
**          xform     = Sketch to Model system xform, including any user
**                      activated transformation.  Can be set to NULL if
**                      there is no transformation to apply.
**       OUTPUT :
**          sub       = Updated subscript number for circles.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreateCircle(double *cirrec, int *sub, double *tfmat)
{
	int subscr;
	char lab[64];
	UM_vector v0,v1;
	NCLX_mdl_circle circle;
//
//...Label circle
//
	(*sub)++;
	if (m_swopt.label_opts == 1)
	{
		NclxMdlLabelEntity(lab,sub,1,"QZ");
		subscr = *sub;
	}
	else
	{
		NclxMdlLabelEntity(lab,sub,m_swopt.cilab_type,m_swopt.cilab);
		subscr = *sub;
		if (m_swopt.cilab_type == 0) subscr = 0;
	}
//
//...Initialize circle record
//
	circle.header.attrib.displayable = 0;
	NclxMdlSetupData(&circle.header,NCLX_MDL_CIRCLE,lab,subscr);
	circle.header.attrib.color = NCLX_BLUE;
//
//...Convert SolidsWorks circle to Unibase Circle
//
	uy_circ_2pts(&cirrec[12],&cirrec[6],&cirrec[9],cirrec[15],&circle);
//
//...Convert circle to inches
//
	um_vctmsc(circle.center,1./.0254,circle.center);
	circle.radius /= .0254;
//
//...Run circle through transformation
//
	if (tfmat)
	{
		um_cross(circle.nvec,circle.svec,v0);
		um_vctmtf(v0,tfmat,v0);
		um_cctmtf(circle.center,tfmat,circle.center);
		um_vctmtf(circle.nvec,tfmat,circle.nvec);
		um_unitvc(circle.nvec, circle.nvec);
		um_vctmtf(circle.svec,tfmat,circle.svec);
		circle.radius *= um_mag(circle.svec);
		um_unitvc(circle.svec, circle.svec);
		um_cross(circle.nvec,circle.svec,v1);
		if (um_dot(v0,v1) < 0.0) circle.dang = -circle.dang;
	}
//
//...Store circle in Unibase
//
	NclxMdlStoreCircle(&circle,NCLX_TRUE);
}

/*********************************************************************
**    I_FUNCTION     :  CreateSpline(cvpts,ncvpt,knots,nknot,dim,degree,
**                                   closed,sub,tfmat);
**       Creates a Unibase spline entity from a SolidWorks sketch spline and
**       stores it in the Unibase.
**    PARAMETERS
**       INPUT  :
**          cvpts     = Control points.
**          ncvpt     = Number of control points.
**          knots     = Knot values.
**          nknot     = Number of knot values.
**          dim       = 2 = Control points are stored in XY, 3 = XYZ.
**          degree    = Order of curve.
**          closed    = 0 = spline is open, 1 = closed.
**          sub       = Subscript number for curves.  Initial value should
**                      be 0.
**          xform     = Sketch to Model system xform, including any user
**                      activated transformation.  Can be set to NULL if
**                      there is no transformation to apply.
**       OUTPUT :
**          sub       = Updated subscript number for circles.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreateSpline(double *cvpts, int ncvpt,double *knots,
	int nknot, int dim, int degree, int closed, int *sub, double *tfmat)
{
	int subscr,i,inc,sst[10];
	char lab[64];
	UU_REAL *knpt;
	UM_coord *pts;
	NCLX_mdl_curve curve;
//
//...Label curve
//
	(*sub)++;
	if (m_swopt.label_opts == 1)
	{
		NclxMdlLabelEntity(lab,sub,1,"QZ");
		subscr = *sub;
	}
	else
	{
		NclxMdlLabelEntity(lab,sub,m_swopt.kvlab_type,m_swopt.kvlab);
		subscr = *sub;
		if (m_swopt.kvlab_type == 0) subscr = 0;
	}
//
//...Allocate memory for curve points & knots
//
	knpt = (double *)uu_malloc(sizeof(double)*nknot);
	pts = (UM_coord *)uu_malloc(sizeof(UM_coord)*ncvpt);
//
//...Store knots
//
	for (i=0;i<nknot;i++) knpt[i] = knots[i];
//
//...Store weights
//
	if (dim == 4) 
	{
		curve.nwgt = ncvpt;
		curve.wgt = (double *)uu_malloc(sizeof(double)*ncvpt);
	}
	else
	{
		curve.nwgt = 0;
		curve.wgt = 0;
	}
//
//...Store curve points
//
	inc = 0;
	for (i=0;i<ncvpt*dim;i=i+dim)
	{
		pts[inc][0] = cvpts[i]/.0254; pts[inc][1] = cvpts[i+1]/.0254;
		pts[inc][2] = cvpts[i+2]/.0254;
		if (curve.nwgt != 0) curve.wgt[inc] = cvpts[i+3];
		if (tfmat) um_cctmtf(pts[inc],tfmat,pts[inc]);
		inc++;
	}
//
//...Initialize curve record
//
	curve.header.attrib.displayable = 0;
	NclxMdlSetupData(&curve.header,NCLX_MDL_BSPLINE,lab,subscr);
	curve.header.attrib.color = NCLX_CYAN;

	curve.cvhead.closed = closed;
	curve.cvhead.degree = degree;
	curve.cvhead.degseg = ncvpt - (degree-1);
	curve.cvhead.t0 = 0.;
	curve.cvhead.t1 = 1.;
	curve.cvhead.eval = (curve.cvhead.t1 - curve.cvhead.t0) / 2.;
	curve.cvhead.tlen = 0.;

	curve.evaluator = 0;
	curve.nparm = 0;
	curve.parms = 0;
	curve.ntparm = nknot;
	curve.tparms = knpt;
	curve.npt = ncvpt;
	curve.pt = (UU_REAL *)pts;
	curve.nsegment = 0;
	curve.segment = 0;
//
//...Store curve in Unibase
//
	NclxMdlStoreCurve(&curve,NCLX_TRUE,sst);
//
//...Free the allocated memory
//
	uu_free(knpt);
	uu_free(pts);
	if (curve.nwgt != 0) uu_free(curve.wgt);
}
