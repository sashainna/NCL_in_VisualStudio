/*********************************************************************
**  NAME:  SwNclUnibase.cpp
**
**       Implementation of NCL/Solid class functions.
**
** CONTAINS: CNclUnibase class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwNclUnibase.h , 24.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/13/14 , 14:45:49
*********************************************************************/
#ifndef NCLUNIBASE_H
#define NCLUNIBASE_H

// SolidWorks includes
//#include <swconst.h>

// NCL includes
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxunib.h"


class CNclUnibase
{
private:
	NCLX_sw_options m_swopt;
	CUniStat *m_statDlg;
	int m_srfCount;
	int m_faceNum;
	int m_curveNum;
	int m_sketchCount;
	int m_nlevel;
	int m_nmatch;
public:
	CNclUnibase()
	{
		m_srfCount = 0;
		m_faceNum = 0;
		m_curveNum = 0;
		m_sketchCount = 0;
	}

	~CNclUnibase()
	{
	}

public:
	void SaveUnibase(LPSLDWORKS pIswApp, NCLX_sw_options *swopt);
	static void CountMatches(int level, int matches);
	static void GetInterrupt(int *stat);
	static void ListOut(char *sbuf);
	static void open_listing_file(char ifname[],char ofname[]);
	static void close_listing_file();
	static void list_file_out(char line[]);

private:
	void TraverseSurfs(LPPARTDOC part, LPCOMPONENT2 compon);
	void CreateUnibase(LPPARTDOC part, LPCOMPONENT2 compon);
	void CountSurfs(LPPARTDOC part, LPCOMPONENT2 compon);
	void CreateTrimSf(NCLX_mdl_trimsf *tsf, double *Sparam, double *pdat,
		int *ipt, int *rpt, int *sub);
	void CreateBaseSf(NCLX_mdl_trimsf *tsf, double *Sparam, double *pdat);
	void CreateTrimCv(NCLX_mdl_trimsf *tsf, double *Sparam, int ipt[],
		int rpt[]);
	void CreateCurves(NCLX_mdl_trimsf *tsf, int *sub);
	void FreeTrimSf(NCLX_mdl_trimsf *tsf);
	void CreateAttrib(NCLX_mdl_trimsf *tsf, double *attrib);
	void CreateSketches(LPSKETCH *sklist, char *nmlist, double *cdsys);
	void CreatePoint(double *ptrrec, int *sub, double *tfmat);
	void CreateLine(double *lnrrec, int *sub, double *tfmat);
	void CreateCircle(double *cirrec, int *sub, double *tfmat);
	void CreateSpline(double *cvpts, int ncvpt,double *knots, int nknot,
		int dim, int degree, int closed, int *sub, double *tfmat);
};

static CNclUnibase *my_this;
#endif // NCLUNIBASE_H
