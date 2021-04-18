//*********************************************************** 
// Author             : Thomas Razier 
// Date               :
//*********************************************************** 
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <iostream>

#ifndef DS_TESTOBJECT
//#define DS_TESTOBJECT
#endif
// --- External ---------------------------------------------
int print_cl(NCLX_mot_clrec *);
extern "C" int  NclxMotInterrupt  ();
extern "C" int NclxMdlEvalSurf   ();
extern "C" int NclxMdlEvalComposite();
extern "C" int NclxMdlEvalCurve   ();
extern "C" int  NclxMotGoto       (NCLX_mdl_pntvec*,NCLX_mot_clrec*);
extern "C" int  NclxMotGo3        (NCLX_mdl_data*,int,NCLX_mdl_data*,int,NCLX_mdl_data*,NCLX_mot_clrec*);
extern "C" int  NclxMotSetCutter  (NCLX_mot_cutter*);
extern "C" int  NclxMotSetPSCond   (NCLX_mot_pscond * );
extern "C" void NclxMotLibClose   ();
extern "C" int  NclxMotLibInit    (char[], int, double *);
extern "C" int  NclxMotSetThick   (NCLX_mot_thick    *);
extern "C" int  NclxMotSetNumpts  (int               *);
extern "C" int  NclxMotSetToler   (NCLX_mot_toler    *);
extern "C" int  NclxMotSetMaxdp   (NCLX_mot_maxdp    *);
extern "C" int  NclxMotSetTlaxis  (NCLX_mot_tlaxis   *);
extern "C" void NclxMotGetTlaxis  (NCLX_mot_tlaxis   *);
extern "C" int  NclxMotSetFeedrate(NCLX_mot_feedrate *);
extern "C" void NclxMotGetToler   (NCLX_mot_toler    *);
extern "C" int  NclxMotSetMaxang  (double            *);
extern "C" int  NclxMotDrive1     (NCLX_mdl_data*,int,int,NCLX_mdl_data*,NCLX_mot_cs*,NCLX_mot_clrec*);
extern "C" int  NclxMotSetAutost  (int *);
extern "C" int  NclxMotSetAutost1  (int *,int *);
extern "C" int  NclxMotSetGougck  (int *);
extern "C" int  NclxMotSetGougck1 (int *,int *,int *);
extern "C" int  NclxMotSetContact (int *);
extern "C" void NclxMotClRead     (NCLX_mot_clrec  *);
extern "C" int  NclxMotIndirv     (double  *);
extern "C" int  NclxMotFillet     (NCLX_mot_fillet *);
extern "C" int  NclxMdlFree       (double * );
extern "C" int  NclxMotSetFanInterp(int *, double * );
// --- External ---------------------------------------------

#ifdef DS_TESTOBJECT
int DStestobjectname()
#else
int main()
#endif
{ 
	using namespace std;
 int status (0),statusDrive1 (0);
 double	NCL_Version(9.0);
 char NCL_PWD[19] = { '6','8','5','2','-','9','2','9','9','-','4','7','4','0','-','9','7','0','5'};
 int unit(1);	// V5 => toujours MKS donc 1 (mm)

 status =status+ NclxMotLibInit (NCL_PWD, unit, &NCL_Version);

		NCLX_mot_thick    Thickness; 
		NCLX_mot_feedrate NCCSFeedrate; 
		int	            lev_goug, contct, Numpts, autost,omit;
		int             RateForInterp;
		int	            lev_gougPS, lev_gougDS, lev_gougCS;
		double	         Maxang;
		double	         DegreeForInterp=1.0;
		double	         ForwardVec[3];
		NCLX_mot_tlaxis	NCCSAxis;
	    NCLX_mdl_data         tempo_guid_crv; 
		NCLX_mot_maxdp    maxstep;
		NCLX_mot_toler    machtol;
		NCLX_mot_pscond	cond;
		NCLX_mdl_pntvec  NCCSRefPointValue ;
		NCLX_mot_clrec   clrec ;
		NCLX_mot_cs      NCCSCheckStruct;
		NCLX_mot_cs_rec  ioS_csrec[5] ;
		NCLX_mot_cutter NCCSTool;
		NCLX_mdl_cmp_entity CurrentEntity;
		NCLX_mot_fillet fillet;
// Address where the computation starts after GO3;
 	char * CLREC_ToolPathBeginning=NULL;
//  
		Numpts      =1000; 
//  
		status =status+ NclxMotSetNumpts(&Numpts); 
    cout << " NclxMotSetNumpts " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		machtol.chordal  = 4.00000000e-03; 
		machtol.dsps     = 4.00000000e-03; 
		machtol.cs       = 4.00000000e-03; 
		machtol.start_pt = 7.50000000e-03; 
		machtol.start_cos= -9.99847695e-01; 
//  
		status =status+ NclxMotSetToler (&machtol); 
    cout << " NclxMotSetToler " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		Maxang      =1.79000000e+02; 
//  
	   status =status+ NclxMotSetMaxang(&Maxang); 
    cout << " NclxMotSetMaxang " << " status= " << status<<endl;  
//    if (status != 0) goto NclError; 
//  
//  
		maxstep.min      = 1.00000000e-02; 
		maxstep.max      = 5.00000000e+01; 
		maxstep.mauto    = 1; 
		maxstep.step     = 0; 
		maxstep.attempts = 10; 
//  
		status =status+ NclxMotSetMaxdp (&maxstep); 
    cout << " NclxMotSetMaxdp " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		cond      = (NCLX_mot_pscond) 0; 
//  
		status =status+ NclxMotSetPSCond (&cond); 
    cout << " NclxMotSetPSCond " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		autost      =1; 
		omit      =1; 
//  
		status =status+ NclxMotSetAutost1 (&autost,&omit); 
    cout << " NclxMotSetAutost1 " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
	   NCCSTool.diameter   = 2.00000000e+01; 
	   NCCSTool.radius     = 1.00000000e+00; 
	   NCCSTool.height     = 1.00000000e+02; 
	   NCCSTool.side_angle = 0.00000000e+00; 
	   NCCSTool.zheight    = 0.00000000e+00; 
	   NCCSTool.flat_angle = 0.00000000e+00; 
//  
      status =status+ NclxMotSetCutter (&NCCSTool); 
    cout << " NclxMotSetCutter " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		NCLX_mdl_plane *  GEOM_ELEM_1=0;
		NCLX_mdl_data  *  GEOM_ELEM_1data=0;
		GEOM_ELEM_1data = (NCLX_mdl_data *) malloc(sizeof(NCLX_mdl_data));
		if ( NULL!= GEOM_ELEM_1data )
		{
		  GEOM_ELEM_1 = (NCLX_mdl_plane *) GEOM_ELEM_1data;
//  
		  if ( NULL!= GEOM_ELEM_1 )
		  {
		    GEOM_ELEM_1->header.key      = 1;
		    GEOM_ELEM_1->header.relnum   = NCLX_MDL_PLANE;
		    GEOM_ELEM_1->header.subscript= 0;
		    GEOM_ELEM_1->dist  = 0.00000000e+00;
		    GEOM_ELEM_1->pt[0] = -1.00001592e+07;
		    GEOM_ELEM_1->pt[1] = 8.22907550e+00;
		    GEOM_ELEM_1->pt[2] = -1.00000869e+07;
		    GEOM_ELEM_1->vec[0]= -3.90041668e-14;
		    GEOM_ELEM_1->vec[1]= 1.00000000e+00;
		    GEOM_ELEM_1->vec[2]= -2.94773065e-14;
		   }
          else  status=36;
//
		}
          else  status=36;
//
//  
//  
		NCLX_mdl_trimsf *  GEOM_ELEM_4=0;
		NCLX_mdl_data   *  GEOM_ELEM_4data=0;
		NCLX_mdl_surf   *  GEOM_ELEM_4surface=0;
		NCLX_mdl_polyline  *  GEOM_ELEM_4polyline=0;
//  
		GEOM_ELEM_4data = (NCLX_mdl_data *) malloc(sizeof(NCLX_mdl_data));
		if ( NULL!= GEOM_ELEM_4data )
		{
		  GEOM_ELEM_4 = (NCLX_mdl_trimsf *) GEOM_ELEM_4data;
//  
		  GEOM_ELEM_4surface = (NCLX_mdl_surf *) malloc(sizeof(NCLX_mdl_surf));
//  Set surface of Trimsf 
		  if ( NULL!= GEOM_ELEM_4surface )
		  {
		    GEOM_ELEM_4surface->header.key      = 2;
		    GEOM_ELEM_4surface->header.relnum   = NCLX_MDL_NSURF;
		    GEOM_ELEM_4surface->header.subscript= 0;
		    GEOM_ELEM_4surface->primitive       = NCLX_FREEFORM;
		    GEOM_ELEM_4surface->evaluator    = (int *) NclxMdlEvalSurf ;
		    GEOM_ELEM_4surface->no_boxlst    = 0;
	       GEOM_ELEM_4surface->boxlst       = 0;
		    GEOM_ELEM_4surface->no_bndrylst  = 0;
		    GEOM_ELEM_4surface->bndrylst     = 0;
		    GEOM_ELEM_4surface->sfhead.urld   = -1;
		    GEOM_ELEM_4surface->sfhead.vrld   = -1;
		    GEOM_ELEM_4surface->sfhead.offset = 0;
		    GEOM_ELEM_4surface->sfhead.offdist= 0.00000000e+00;
		    GEOM_ELEM_4surface->sfhead.eval[0]= 5.00000000e-01;
		    GEOM_ELEM_4surface->sfhead.eval[1]= 5.00000000e-01;
		    GEOM_ELEM_4surface->sfhead.uclosed= 0;
		    GEOM_ELEM_4surface->sfhead.vclosed= 0;
		    GEOM_ELEM_4surface->sfhead.udegree= 6;
		    GEOM_ELEM_4surface->sfhead.vdegree= 2;
		    GEOM_ELEM_4surface->sfhead.udegseg= 70;
		    GEOM_ELEM_4surface->sfhead.vdegseg= 1;
		    GEOM_ELEM_4surface->sfhead.shaded = 0;
		    GEOM_ELEM_4surface->sfhead.lucency= 0;
		  GEOM_ELEM_4surface->ntu = 81;
		  GEOM_ELEM_4surface->tu = (double *) malloc(GEOM_ELEM_4surface->ntu*sizeof(double));
	     if ( NULL!= GEOM_ELEM_4surface->tu )
	     {
		    GEOM_ELEM_4surface->tu[0] = 0.00000000e+00 ;
		    GEOM_ELEM_4surface->tu[1] = 0.00000000e+00 ;
		    GEOM_ELEM_4surface->tu[2] = 0.00000000e+00 ;
		    GEOM_ELEM_4surface->tu[3] = 0.00000000e+00 ;
		    GEOM_ELEM_4surface->tu[4] = 0.00000000e+00 ;
		    GEOM_ELEM_4surface->tu[5] = 0.00000000e+00 ;
		    GEOM_ELEM_4surface->tu[6] = 1.15026528e+02 ;
		    GEOM_ELEM_4surface->tu[7] = 1.15026528e+02 ;
		    GEOM_ELEM_4surface->tu[8] = 1.15026528e+02 ;
		    GEOM_ELEM_4surface->tu[9] = 2.52828258e+02 ;
		    GEOM_ELEM_4surface->tu[10] = 2.52828258e+02 ;
		    GEOM_ELEM_4surface->tu[11] = 2.52828258e+02 ;
		    GEOM_ELEM_4surface->tu[12] = 3.82067641e+02 ;
		    GEOM_ELEM_4surface->tu[13] = 3.82067641e+02 ;
		    GEOM_ELEM_4surface->tu[14] = 3.82067641e+02 ;
		    GEOM_ELEM_4surface->tu[15] = 4.10860535e+02 ;
		    GEOM_ELEM_4surface->tu[16] = 4.10860535e+02 ;
		    GEOM_ELEM_4surface->tu[17] = 4.10860535e+02 ;
		    GEOM_ELEM_4surface->tu[18] = 4.31288325e+02 ;
		    GEOM_ELEM_4surface->tu[19] = 4.31288325e+02 ;
		    GEOM_ELEM_4surface->tu[20] = 4.31288325e+02 ;
		    GEOM_ELEM_4surface->tu[21] = 4.48448974e+02 ;
		    GEOM_ELEM_4surface->tu[22] = 4.48448974e+02 ;
		    GEOM_ELEM_4surface->tu[23] = 4.48448974e+02 ;
		    GEOM_ELEM_4surface->tu[24] = 4.60971634e+02 ;
		    GEOM_ELEM_4surface->tu[25] = 4.60971634e+02 ;
		    GEOM_ELEM_4surface->tu[26] = 4.60971634e+02 ;
		    GEOM_ELEM_4surface->tu[27] = 4.69119876e+02 ;
		    GEOM_ELEM_4surface->tu[28] = 4.69119876e+02 ;
		    GEOM_ELEM_4surface->tu[29] = 4.69119876e+02 ;
		    GEOM_ELEM_4surface->tu[30] = 4.76084376e+02 ;
		    GEOM_ELEM_4surface->tu[31] = 4.76084376e+02 ;
		    GEOM_ELEM_4surface->tu[32] = 4.76084376e+02 ;
		    GEOM_ELEM_4surface->tu[33] = 4.85070061e+02 ;
		    GEOM_ELEM_4surface->tu[34] = 4.85070061e+02 ;
		    GEOM_ELEM_4surface->tu[35] = 4.85070061e+02 ;
		    GEOM_ELEM_4surface->tu[36] = 5.02854233e+02 ;
		    GEOM_ELEM_4surface->tu[37] = 5.02854233e+02 ;
		    GEOM_ELEM_4surface->tu[38] = 5.02854233e+02 ;
		    GEOM_ELEM_4surface->tu[39] = 5.31379052e+02 ;
		    GEOM_ELEM_4surface->tu[40] = 5.31379052e+02 ;
		    GEOM_ELEM_4surface->tu[41] = 5.31379052e+02 ;
		    GEOM_ELEM_4surface->tu[42] = 5.61485097e+02 ;
		    GEOM_ELEM_4surface->tu[43] = 5.61485097e+02 ;
		    GEOM_ELEM_4surface->tu[44] = 5.61485097e+02 ;
		    GEOM_ELEM_4surface->tu[45] = 5.92240302e+02 ;
		    GEOM_ELEM_4surface->tu[46] = 5.92240302e+02 ;
		    GEOM_ELEM_4surface->tu[47] = 5.92240302e+02 ;
		    GEOM_ELEM_4surface->tu[48] = 6.41677378e+02 ;
		    GEOM_ELEM_4surface->tu[49] = 6.41677378e+02 ;
		    GEOM_ELEM_4surface->tu[50] = 6.41677378e+02 ;
		    GEOM_ELEM_4surface->tu[51] = 7.12557289e+02 ;
		    GEOM_ELEM_4surface->tu[52] = 7.12557289e+02 ;
		    GEOM_ELEM_4surface->tu[53] = 7.12557289e+02 ;
		    GEOM_ELEM_4surface->tu[54] = 7.72935753e+02 ;
		    GEOM_ELEM_4surface->tu[55] = 7.72935753e+02 ;
		    GEOM_ELEM_4surface->tu[56] = 7.72935753e+02 ;
		    GEOM_ELEM_4surface->tu[57] = 8.23643847e+02 ;
		    GEOM_ELEM_4surface->tu[58] = 8.23643847e+02 ;
		    GEOM_ELEM_4surface->tu[59] = 8.23643847e+02 ;
		    GEOM_ELEM_4surface->tu[60] = 8.37149873e+02 ;
		    GEOM_ELEM_4surface->tu[61] = 8.37149873e+02 ;
		    GEOM_ELEM_4surface->tu[62] = 8.37149873e+02 ;
		    GEOM_ELEM_4surface->tu[63] = 8.59431579e+02 ;
		    GEOM_ELEM_4surface->tu[64] = 8.59431579e+02 ;
		    GEOM_ELEM_4surface->tu[65] = 8.59431579e+02 ;
		    GEOM_ELEM_4surface->tu[66] = 8.78957971e+02 ;
		    GEOM_ELEM_4surface->tu[67] = 8.78957971e+02 ;
		    GEOM_ELEM_4surface->tu[68] = 8.78957971e+02 ;
		    GEOM_ELEM_4surface->tu[69] = 9.07756082e+02 ;
		    GEOM_ELEM_4surface->tu[70] = 9.07756082e+02 ;
		    GEOM_ELEM_4surface->tu[71] = 9.07756082e+02 ;
		    GEOM_ELEM_4surface->tu[72] = 1.02967288e+03 ;
		    GEOM_ELEM_4surface->tu[73] = 1.02967288e+03 ;
		    GEOM_ELEM_4surface->tu[74] = 1.02967288e+03 ;
		    GEOM_ELEM_4surface->tu[75] = 1.15542961e+03 ;
		    GEOM_ELEM_4surface->tu[76] = 1.15542961e+03 ;
		    GEOM_ELEM_4surface->tu[77] = 1.15542961e+03 ;
		    GEOM_ELEM_4surface->tu[78] = 1.15542961e+03 ;
		    GEOM_ELEM_4surface->tu[79] = 1.15542961e+03 ;
		    GEOM_ELEM_4surface->tu[80] = 1.15542961e+03 ;
		  }
          else  status=36;
//
		  GEOM_ELEM_4surface->ntv = 4;
		  GEOM_ELEM_4surface->tv = (double *) malloc(GEOM_ELEM_4surface->ntv*sizeof(double));
		  if ( NULL!= GEOM_ELEM_4surface->tv )
		  {
          GEOM_ELEM_4surface->tv[0] = 0.00000000e+00 ;
          GEOM_ELEM_4surface->tv[1] = 0.00000000e+00 ;
          GEOM_ELEM_4surface->tv[2] = 1.04654392e+02 ;
          GEOM_ELEM_4surface->tv[3] = 1.04654392e+02 ;
		  }
          else  status=36;
//
		  GEOM_ELEM_4surface->npt = 150;
		  GEOM_ELEM_4surface->pt = (double *) malloc(GEOM_ELEM_4surface->npt*3*sizeof(double));
		  if ( NULL!= GEOM_ELEM_4surface->pt )
		  {
          GEOM_ELEM_4surface->pt[0]=2.63177481e+02; GEOM_ELEM_4surface->pt[1]=6.93720011e-01; GEOM_ELEM_4surface->pt[2]=-1.17541158e+02;
          GEOM_ELEM_4surface->pt[3]=2.40255547e+02; GEOM_ELEM_4surface->pt[4]=6.94441415e-01; GEOM_ELEM_4surface->pt[5]=-1.15774624e+02;
          GEOM_ELEM_4surface->pt[6]=2.17339530e+02; GEOM_ELEM_4surface->pt[7]=6.95172254e-01; GEOM_ELEM_4surface->pt[8]=-1.13931341e+02;
          GEOM_ELEM_4surface->pt[9]=1.93752602e+02; GEOM_ELEM_4surface->pt[10]=6.95935304e-01; GEOM_ELEM_4surface->pt[11]=-1.11954767e+02;
          GEOM_ELEM_4surface->pt[12]=1.43413733e+02; GEOM_ELEM_4surface->pt[13]=6.97589441e-01; GEOM_ELEM_4surface->pt[14]=-1.07539720e+02;
          GEOM_ELEM_4surface->pt[15]=9.31103291e+01; GEOM_ELEM_4surface->pt[16]=6.99292032e-01; GEOM_ELEM_4surface->pt[17]=-1.02735984e+02;
          GEOM_ELEM_4surface->pt[18]=6.36866699e+01; GEOM_ELEM_4surface->pt[19]=7.00301865e-01; GEOM_ELEM_4surface->pt[20]=-9.98244027e+01;
          GEOM_ELEM_4surface->pt[21]=1.05960104e+01; GEOM_ELEM_4surface->pt[22]=7.02152434e-01; GEOM_ELEM_4surface->pt[23]=-9.43439437e+01;
          GEOM_ELEM_4surface->pt[24]=-4.24572057e+01; GEOM_ELEM_4surface->pt[25]=7.04047961e-01; GEOM_ELEM_4surface->pt[26]=-8.85124993e+01;
          GEOM_ELEM_4surface->pt[27]=-6.82948500e+01; GEOM_ELEM_4surface->pt[28]=7.04978764e-01; GEOM_ELEM_4surface->pt[29]=-8.56122910e+01;
          GEOM_ELEM_4surface->pt[30]=-9.96725242e+01; GEOM_ELEM_4surface->pt[31]=7.06121417e-01; GEOM_ELEM_4surface->pt[32]=-8.19960285e+01;
          GEOM_ELEM_4surface->pt[33]=-1.31040253e+02; GEOM_ELEM_4surface->pt[34]=7.07274829e-01; GEOM_ELEM_4surface->pt[35]=-7.82951168e+01;
          GEOM_ELEM_4surface->pt[36]=-1.34513839e+02; GEOM_ELEM_4surface->pt[37]=7.07402951e-01; GEOM_ELEM_4surface->pt[38]=-7.78822172e+01;
          GEOM_ELEM_4surface->pt[39]=-1.44283313e+02; GEOM_ELEM_4surface->pt[40]=7.07762530e-01; GEOM_ELEM_4surface->pt[41]=-7.67268756e+01;
          GEOM_ELEM_4surface->pt[42]=-1.54050708e+02; GEOM_ELEM_4surface->pt[43]=7.08124317e-01; GEOM_ELEM_4surface->pt[44]=-7.55541183e+01;
          GEOM_ELEM_4surface->pt[45]=-1.58052530e+02; GEOM_ELEM_4surface->pt[46]=7.08279458e-01; GEOM_ELEM_4surface->pt[47]=-7.50202987e+01;
          GEOM_ELEM_4surface->pt[48]=-1.65476873e+02; GEOM_ELEM_4surface->pt[49]=7.08591919e-01; GEOM_ELEM_4surface->pt[50]=-7.38394828e+01;
          GEOM_ELEM_4surface->pt[51]=-1.72803894e+02; GEOM_ELEM_4surface->pt[52]=7.08966377e-01; GEOM_ELEM_4surface->pt[53]=-7.21638721e+01;
          GEOM_ELEM_4surface->pt[54]=-1.76048824e+02; GEOM_ELEM_4surface->pt[55]=7.09150157e-01; GEOM_ELEM_4surface->pt[56]=-7.12831422e+01;
          GEOM_ELEM_4surface->pt[57]=-1.81703711e+02; GEOM_ELEM_4surface->pt[58]=7.09512250e-01; GEOM_ELEM_4surface->pt[59]=-6.94254288e+01;
          GEOM_ELEM_4surface->pt[60]=-1.87035319e+02; GEOM_ELEM_4surface->pt[61]=7.09966831e-01; GEOM_ELEM_4surface->pt[62]=-6.67997293e+01;
          GEOM_ELEM_4surface->pt[63]=-1.89107634e+02; GEOM_ELEM_4surface->pt[64]=7.10170203e-01; GEOM_ELEM_4surface->pt[65]=-6.55731422e+01;
          GEOM_ELEM_4surface->pt[66]=-1.92489435e+02; GEOM_ELEM_4surface->pt[67]=7.10557955e-01; GEOM_ELEM_4surface->pt[68]=-6.31398826e+01;
          GEOM_ELEM_4surface->pt[69]=-1.95264618e+02; GEOM_ELEM_4surface->pt[70]=7.11016482e-01; GEOM_ELEM_4surface->pt[71]=-6.00594272e+01;
          GEOM_ELEM_4surface->pt[72]=-1.96346052e+02; GEOM_ELEM_4surface->pt[73]=7.11228229e-01; GEOM_ELEM_4surface->pt[74]=-5.86036275e+01;
          GEOM_ELEM_4surface->pt[75]=-1.97896119e+02; GEOM_ELEM_4surface->pt[76]=7.11599293e-01; GEOM_ELEM_4surface->pt[77]=-5.59952537e+01;
          GEOM_ELEM_4surface->pt[78]=-1.98962986e+02; GEOM_ELEM_4surface->pt[79]=7.11989519e-01; GEOM_ELEM_4surface->pt[80]=-5.31586655e+01;
          GEOM_ELEM_4surface->pt[81]=-1.99376406e+02; GEOM_ELEM_4surface->pt[82]=7.12175462e-01; GEOM_ELEM_4surface->pt[83]=-5.17913066e+01;
          GEOM_ELEM_4surface->pt[84]=-2.00075791e+02; GEOM_ELEM_4surface->pt[85]=7.12593951e-01; GEOM_ELEM_4surface->pt[86]=-4.86754434e+01;
          GEOM_ELEM_4surface->pt[87]=-2.00351431e+02; GEOM_ELEM_4surface->pt[88]=7.13012309e-01; GEOM_ELEM_4surface->pt[89]=-4.54902566e+01;
          GEOM_ELEM_4surface->pt[90]=-2.00412018e+02; GEOM_ELEM_4surface->pt[91]=7.13253995e-01; GEOM_ELEM_4surface->pt[92]=-4.36338348e+01;
          GEOM_ELEM_4surface->pt[93]=-2.00301050e+02; GEOM_ELEM_4surface->pt[94]=7.13945223e-01; GEOM_ELEM_4surface->pt[95]=-3.82771648e+01;
          GEOM_ELEM_4surface->pt[96]=-1.99347080e+02; GEOM_ELEM_4surface->pt[97]=7.14610306e-01; GEOM_ELEM_4surface->pt[98]=-3.29824770e+01;
          GEOM_ELEM_4surface->pt[99]=-1.98361909e+02; GEOM_ELEM_4surface->pt[100]=7.15038208e-01; GEOM_ELEM_4surface->pt[101]=-2.95143185e+01;
          GEOM_ELEM_4surface->pt[102]=-1.95089693e+02; GEOM_ELEM_4surface->pt[103]=7.16092271e-01; GEOM_ELEM_4surface->pt[104]=-2.08307948e+01;
          GEOM_ELEM_4surface->pt[105]=-1.90241327e+02; GEOM_ELEM_4surface->pt[106]=7.17018284e-01; GEOM_ELEM_4surface->pt[107]=-1.28745497e+01;
          GEOM_ELEM_4surface->pt[108]=-1.86821131e+02; GEOM_ELEM_4surface->pt[109]=7.17544365e-01; GEOM_ELEM_4surface->pt[110]=-8.24399811e+00;
          GEOM_ELEM_4surface->pt[111]=-1.79229147e+02; GEOM_ELEM_4surface->pt[112]=7.18540031e-01; GEOM_ELEM_4surface->pt[113]=7.05587074e-01;
          GEOM_ELEM_4surface->pt[114]=-1.70676843e+02; GEOM_ELEM_4surface->pt[115]=7.19397258e-01; GEOM_ELEM_4surface->pt[116]=8.74539718e+00;
          GEOM_ELEM_4surface->pt[117]=-1.66127225e+02; GEOM_ELEM_4surface->pt[118]=7.19807305e-01; GEOM_ELEM_4surface->pt[119]=1.26673532e+01;
          GEOM_ELEM_4surface->pt[120]=-1.56641229e+02; GEOM_ELEM_4surface->pt[121]=7.20590377e-01; GEOM_ELEM_4surface->pt[122]=2.02894355e+01;
          GEOM_ELEM_4surface->pt[123]=-1.46740963e+02; GEOM_ELEM_4surface->pt[124]=7.21293828e-01; GEOM_ELEM_4surface->pt[125]=2.73654595e+01;
          GEOM_ELEM_4surface->pt[126]=-1.41645635e+02; GEOM_ELEM_4surface->pt[127]=7.21634918e-01; GEOM_ELEM_4surface->pt[128]=3.08453947e+01;
          GEOM_ELEM_4surface->pt[129]=-1.28243941e+02; GEOM_ELEM_4surface->pt[130]=7.22485910e-01; GEOM_ELEM_4surface->pt[131]=3.96420304e+01;
          GEOM_ELEM_4surface->pt[132]=-1.14406764e+02; GEOM_ELEM_4surface->pt[133]=7.23237860e-01; GEOM_ELEM_4surface->pt[134]=4.77460306e+01;
          GEOM_ELEM_4surface->pt[135]=-1.05610154e+02; GEOM_ELEM_4surface->pt[136]=7.23672789e-01; GEOM_ELEM_4surface->pt[137]=5.25650623e+01;
          GEOM_ELEM_4surface->pt[138]=-8.42541291e+01; GEOM_ELEM_4surface->pt[139]=7.24646479e-01; GEOM_ELEM_4surface->pt[140]=6.36296360e+01;
          GEOM_ELEM_4surface->pt[141]=-6.23796362e+01; GEOM_ELEM_4surface->pt[142]=7.25472843e-01; GEOM_ELEM_4surface->pt[143]=7.36423940e+01;
          GEOM_ELEM_4surface->pt[144]=-4.95737803e+01; GEOM_ELEM_4surface->pt[145]=7.25915601e-01; GEOM_ELEM_4surface->pt[146]=7.91868348e+01;
          GEOM_ELEM_4surface->pt[147]=-2.52951925e+01; GEOM_ELEM_4surface->pt[148]=7.26681661e-01; GEOM_ELEM_4surface->pt[149]=8.91340679e+01;
          GEOM_ELEM_4surface->pt[150]=-7.38865646e-01; GEOM_ELEM_4surface->pt[151]=7.27350154e-01; GEOM_ELEM_4surface->pt[152]=9.83719065e+01;
          GEOM_ELEM_4surface->pt[153]=1.04821649e+01; GEOM_ELEM_4surface->pt[154]=7.27632257e-01; GEOM_ELEM_4surface->pt[155]=1.02413439e+02;
          GEOM_ELEM_4surface->pt[156]=3.14935153e+01; GEOM_ELEM_4surface->pt[157]=7.28111017e-01; GEOM_ELEM_4surface->pt[158]=1.09598669e+02;
          GEOM_ELEM_4surface->pt[159]=5.27277761e+01; GEOM_ELEM_4surface->pt[160]=7.28495449e-01; GEOM_ELEM_4surface->pt[161]=1.16092086e+02;
          GEOM_ELEM_4surface->pt[162]=6.23890110e+01; GEOM_ELEM_4surface->pt[163]=7.28644729e-01; GEOM_ELEM_4surface->pt[164]=1.18848469e+02;
          GEOM_ELEM_4surface->pt[165]=7.48180700e+01; GEOM_ELEM_4surface->pt[166]=7.28794677e-01; GEOM_ELEM_4surface->pt[167]=1.22069860e+02;
          GEOM_ELEM_4surface->pt[168]=8.73530006e+01; GEOM_ELEM_4surface->pt[169]=7.28882305e-01; GEOM_ELEM_4surface->pt[170]=1.24827185e+02;
          GEOM_ELEM_4surface->pt[171]=9.06444104e+01; GEOM_ELEM_4surface->pt[172]=7.28902328e-01; GEOM_ELEM_4surface->pt[173]=1.25528183e+02;
          GEOM_ELEM_4surface->pt[174]=9.76599796e+01; GEOM_ELEM_4surface->pt[175]=7.28932609e-01; GEOM_ELEM_4surface->pt[176]=1.26926515e+02;
          GEOM_ELEM_4surface->pt[177]=1.04727192e+02; GEOM_ELEM_4surface->pt[178]=7.28925362e-01; GEOM_ELEM_4surface->pt[179]=1.28043683e+02;
          GEOM_ELEM_4surface->pt[180]=1.09107534e+02; GEOM_ELEM_4surface->pt[181]=7.28910991e-01; GEOM_ELEM_4surface->pt[182]=1.28659718e+02;
          GEOM_ELEM_4surface->pt[183]=1.17396900e+02; GEOM_ELEM_4surface->pt[184]=7.28869802e-01; GEOM_ELEM_4surface->pt[185]=1.29717629e+02;
          GEOM_ELEM_4surface->pt[186]=1.25707445e+02; GEOM_ELEM_4surface->pt[187]=7.28804467e-01; GEOM_ELEM_4surface->pt[188]=1.30592463e+02;
          GEOM_ELEM_4surface->pt[189]=1.28519899e+02; GEOM_ELEM_4surface->pt[190]=7.28777650e-01; GEOM_ELEM_4surface->pt[191]=1.30852187e+02;
          GEOM_ELEM_4surface->pt[192]=1.38137068e+02; GEOM_ELEM_4surface->pt[193]=7.28686439e-01; GEOM_ELEM_4surface->pt[194]=1.31744210e+02;
          GEOM_ELEM_4surface->pt[195]=1.47755420e+02; GEOM_ELEM_4surface->pt[196]=7.28593505e-01; GEOM_ELEM_4surface->pt[197]=1.32622906e+02;
          GEOM_ELEM_4surface->pt[198]=1.55461908e+02; GEOM_ELEM_4surface->pt[199]=7.28516555e-01; GEOM_ELEM_4surface->pt[200]=1.33307932e+02;
          GEOM_ELEM_4surface->pt[201]=1.85469491e+02; GEOM_ELEM_4surface->pt[202]=7.28212486e-01; GEOM_ELEM_4surface->pt[203]=1.35940418e+02;
          GEOM_ELEM_4surface->pt[204]=2.15495049e+02; GEOM_ELEM_4surface->pt[205]=7.27880944e-01; GEOM_ELEM_4surface->pt[206]=1.38364591e+02;
          GEOM_ELEM_4surface->pt[207]=2.40784351e+02; GEOM_ELEM_4surface->pt[208]=7.27585928e-01; GEOM_ELEM_4surface->pt[209]=1.40282857e+02;
          GEOM_ELEM_4surface->pt[210]=2.90156979e+02; GEOM_ELEM_4surface->pt[211]=7.26987571e-01; GEOM_ELEM_4surface->pt[212]=1.43858254e+02;
          GEOM_ELEM_4surface->pt[213]=3.39556409e+02; GEOM_ELEM_4surface->pt[214]=7.26338273e-01; GEOM_ELEM_4surface->pt[215]=1.47042451e+02;
          GEOM_ELEM_4surface->pt[216]=3.65304276e+02; GEOM_ELEM_4surface->pt[217]=7.25983243e-01; GEOM_ELEM_4surface->pt[218]=1.48577044e+02;
          GEOM_ELEM_4surface->pt[219]=3.90399304e+02; GEOM_ELEM_4surface->pt[220]=7.25626805e-01; GEOM_ELEM_4surface->pt[221]=1.49987799e+02;
          GEOM_ELEM_4surface->pt[222]=4.15498821e+02; GEOM_ELEM_4surface->pt[223]=7.25259003e-01; GEOM_ELEM_4surface->pt[224]=1.51313599e+02;
          GEOM_ELEM_4surface->pt[225]=2.62552223e+02; GEOM_ELEM_4surface->pt[226]=1.05341726e+02; GEOM_ELEM_4surface->pt[227]=-1.17652882e+02;
          GEOM_ELEM_4surface->pt[228]=2.39600172e+02; GEOM_ELEM_4surface->pt[229]=1.05342449e+02; GEOM_ELEM_4surface->pt[230]=-1.15876420e+02;
          GEOM_ELEM_4surface->pt[231]=2.16654396e+02; GEOM_ELEM_4surface->pt[232]=1.05343182e+02; GEOM_ELEM_4surface->pt[233]=-1.14018919e+02;
          GEOM_ELEM_4surface->pt[234]=1.93036841e+02; GEOM_ELEM_4surface->pt[235]=1.05343946e+02; GEOM_ELEM_4surface->pt[236]=-1.12028397e+02;
          GEOM_ELEM_4surface->pt[237]=1.42631917e+02; GEOM_ELEM_4surface->pt[238]=1.05345603e+02; GEOM_ELEM_4surface->pt[239]=-1.07591824e+02;
          GEOM_ELEM_4surface->pt[240]=9.22632921e+01; GEOM_ELEM_4surface->pt[241]=1.05347309e+02; GEOM_ELEM_4surface->pt[242]=-1.02758962e+02;
          GEOM_ELEM_4surface->pt[243]=6.28013394e+01; GEOM_ELEM_4surface->pt[244]=1.05348320e+02; GEOM_ELEM_4surface->pt[245]=-9.98322004e+01;
          GEOM_ELEM_4surface->pt[246]=9.64224812e+00; GEOM_ELEM_4surface->pt[247]=1.05350175e+02; GEOM_ELEM_4surface->pt[248]=-9.43184309e+01;
          GEOM_ELEM_4surface->pt[249]=-4.34784060e+01; GEOM_ELEM_4surface->pt[250]=1.05352074e+02; GEOM_ELEM_4surface->pt[251]=-8.84462313e+01;
          GEOM_ELEM_4surface->pt[252]=-6.93488893e+01; GEOM_ELEM_4surface->pt[253]=1.05353007e+02; GEOM_ELEM_4surface->pt[254]=-8.55263470e+01;
          GEOM_ELEM_4surface->pt[255]=-1.00765638e+02; GEOM_ELEM_4surface->pt[256]=1.05354153e+02; GEOM_ELEM_4surface->pt[257]=-8.18796864e+01;
          GEOM_ELEM_4surface->pt[258]=-1.32172100e+02; GEOM_ELEM_4surface->pt[259]=1.05355310e+02; GEOM_ELEM_4surface->pt[260]=-7.81461186e+01;
          GEOM_ELEM_4surface->pt[261]=-1.35650085e+02; GEOM_ELEM_4surface->pt[262]=1.05355438e+02; GEOM_ELEM_4surface->pt[263]=-7.77305359e+01;
          GEOM_ELEM_4surface->pt[264]=-1.45431253e+02; GEOM_ELEM_4surface->pt[265]=1.05355799e+02; GEOM_ELEM_4surface->pt[266]=-7.65619673e+01;
          GEOM_ELEM_4surface->pt[267]=-1.55210890e+02; GEOM_ELEM_4surface->pt[268]=1.05356162e+02; GEOM_ELEM_4surface->pt[269]=-7.53806739e+01;
          GEOM_ELEM_4surface->pt[270]=-1.59217263e+02; GEOM_ELEM_4surface->pt[271]=1.05356318e+02; GEOM_ELEM_4surface->pt[272]=-7.48400643e+01;
          GEOM_ELEM_4surface->pt[273]=-1.66650434e+02; GEOM_ELEM_4surface->pt[274]=1.05356631e+02; GEOM_ELEM_4surface->pt[275]=-7.36504971e+01;
          GEOM_ELEM_4surface->pt[276]=-1.73985471e+02; GEOM_ELEM_4surface->pt[277]=1.05357006e+02; GEOM_ELEM_4surface->pt[278]=-7.19648006e+01;
          GEOM_ELEM_4surface->pt[279]=-1.77233707e+02; GEOM_ELEM_4surface->pt[280]=1.05357190e+02; GEOM_ELEM_4surface->pt[281]=-7.10789219e+01;
          GEOM_ELEM_4surface->pt[282]=-1.82893557e+02; GEOM_ELEM_4surface->pt[283]=1.05357553e+02; GEOM_ELEM_4surface->pt[284]=-6.92102855e+01;
          GEOM_ELEM_4surface->pt[285]=-1.88226984e+02; GEOM_ELEM_4surface->pt[286]=1.05358008e+02; GEOM_ELEM_4surface->pt[287]=-6.65700968e+01;
          GEOM_ELEM_4surface->pt[288]=-1.90299376e+02; GEOM_ELEM_4surface->pt[289]=1.05358211e+02; GEOM_ELEM_4surface->pt[290]=-6.53371415e+01;
          GEOM_ELEM_4surface->pt[291]=-1.93679562e+02; GEOM_ELEM_4surface->pt[292]=1.05358599e+02; GEOM_ELEM_4surface->pt[293]=-6.28920634e+01;
          GEOM_ELEM_4surface->pt[294]=-1.96450440e+02; GEOM_ELEM_4surface->pt[295]=1.05359057e+02; GEOM_ELEM_4surface->pt[296]=-5.98002607e+01;
          GEOM_ELEM_4surface->pt[297]=-1.97530069e+02; GEOM_ELEM_4surface->pt[298]=1.05359268e+02; GEOM_ELEM_4surface->pt[299]=-5.83401857e+01;
          GEOM_ELEM_4surface->pt[300]=-1.99076643e+02; GEOM_ELEM_4surface->pt[301]=1.05359639e+02; GEOM_ELEM_4surface->pt[302]=-5.57251631e+01;
          GEOM_ELEM_4surface->pt[303]=-2.00141265e+02; GEOM_ELEM_4surface->pt[304]=1.05360028e+02; GEOM_ELEM_4surface->pt[305]=-5.28834990e+01;
          GEOM_ELEM_4surface->pt[306]=-2.00553593e+02; GEOM_ELEM_4surface->pt[307]=1.05360213e+02; GEOM_ELEM_4surface->pt[308]=-5.15138211e+01;
          GEOM_ELEM_4surface->pt[309]=-2.01250779e+02; GEOM_ELEM_4surface->pt[310]=1.05360630e+02; GEOM_ELEM_4surface->pt[311]=-4.83931177e+01;
          GEOM_ELEM_4surface->pt[312]=-2.01524882e+02; GEOM_ELEM_4surface->pt[313]=1.05361047e+02; GEOM_ELEM_4surface->pt[314]=-4.52035572e+01;
          GEOM_ELEM_4surface->pt[315]=-2.01585174e+02; GEOM_ELEM_4surface->pt[316]=1.05361288e+02; GEOM_ELEM_4surface->pt[317]=-4.33446331e+01;
          GEOM_ELEM_4surface->pt[318]=-2.01471893e+02; GEOM_ELEM_4surface->pt[319]=1.05361977e+02; GEOM_ELEM_4surface->pt[320]=-3.79808310e+01;
          GEOM_ELEM_4surface->pt[321]=-2.00513541e+02; GEOM_ELEM_4surface->pt[322]=1.05362640e+02; GEOM_ELEM_4surface->pt[323]=-3.26795110e+01;
          GEOM_ELEM_4surface->pt[324]=-1.99525554e+02; GEOM_ELEM_4surface->pt[325]=1.05363066e+02; GEOM_ELEM_4surface->pt[326]=-2.92072912e+01;
          GEOM_ELEM_4surface->pt[327]=-1.96247318e+02; GEOM_ELEM_4surface->pt[328]=1.05364116e+02; GEOM_ELEM_4surface->pt[329]=-2.05130398e+01;
          GEOM_ELEM_4surface->pt[330]=-1.91399600e+02; GEOM_ELEM_4surface->pt[331]=1.05365039e+02; GEOM_ELEM_4surface->pt[332]=-1.25426024e+01;
          GEOM_ELEM_4surface->pt[333]=-1.87982357e+02; GEOM_ELEM_4surface->pt[334]=1.05365563e+02; GEOM_ELEM_4surface->pt[335]=-7.90080414e+00;
          GEOM_ELEM_4surface->pt[336]=-1.80400086e+02; GEOM_ELEM_4surface->pt[337]=1.05366557e+02; GEOM_ELEM_4surface->pt[338]=1.07726905e+00;
          GEOM_ELEM_4surface->pt[339]=-1.71860759e+02; GEOM_ELEM_4surface->pt[340]=1.05367414e+02; GEOM_ELEM_4surface->pt[341]=9.15334483e+00;
          GEOM_ELEM_4surface->pt[342]=-1.67317819e+02; GEOM_ELEM_4surface->pt[343]=1.05367824e+02; GEOM_ELEM_4surface->pt[344]=1.30952913e+01;
          GEOM_ELEM_4surface->pt[345]=-1.57844245e+02; GEOM_ELEM_4surface->pt[346]=1.05368608e+02; GEOM_ELEM_4surface->pt[347]=2.07588139e+01;
          GEOM_ELEM_4surface->pt[348]=-1.47952368e+02; GEOM_ELEM_4surface->pt[349]=1.05369312e+02; GEOM_ELEM_4surface->pt[350]=2.78745767e+01;
          GEOM_ELEM_4surface->pt[351]=-1.42859758e+02; GEOM_ELEM_4surface->pt[352]=1.05369654e+02; GEOM_ELEM_4surface->pt[353]=3.13731903e+01;
          GEOM_ELEM_4surface->pt[354]=-1.29463014e+02; GEOM_ELEM_4surface->pt[355]=1.05370505e+02; GEOM_ELEM_4surface->pt[356]=4.02163948e+01;
          GEOM_ELEM_4surface->pt[357]=-1.15627964e+02; GEOM_ELEM_4surface->pt[358]=1.05371258e+02; GEOM_ELEM_4surface->pt[359]=4.83664715e+01;
          GEOM_ELEM_4surface->pt[360]=-1.06830665e+02; GEOM_ELEM_4surface->pt[361]=1.05371693e+02; GEOM_ELEM_4surface->pt[362]=5.32123253e+01;
          GEOM_ELEM_4surface->pt[363]=-8.54698358e+01; GEOM_ELEM_4surface->pt[364]=1.05372667e+02; GEOM_ELEM_4surface->pt[365]=6.43377601e+01;
          GEOM_ELEM_4surface->pt[366]=-6.35822670e+01; GEOM_ELEM_4surface->pt[367]=1.05373493e+02; GEOM_ELEM_4surface->pt[368]=7.43997976e+01;
          GEOM_ELEM_4surface->pt[369]=-5.07663862e+01; GEOM_ELEM_4surface->pt[370]=1.05373935e+02; GEOM_ELEM_4surface->pt[371]=7.99683841e+01;
          GEOM_ELEM_4surface->pt[372]=-2.64655983e+01; GEOM_ELEM_4surface->pt[373]=1.05374699e+02; GEOM_ELEM_4surface->pt[374]=8.99543907e+01;
          GEOM_ELEM_4surface->pt[375]=-1.88118754e+00; GEOM_ELEM_4surface->pt[376]=1.05375364e+02; GEOM_ELEM_4surface->pt[377]=9.92176069e+01;
          GEOM_ELEM_4surface->pt[378]=9.35262382e+00; GEOM_ELEM_4surface->pt[379]=1.05375645e+02; GEOM_ELEM_4surface->pt[380]=1.03270786e+02;
          GEOM_ELEM_4surface->pt[381]=3.03889716e+01; GEOM_ELEM_4surface->pt[382]=1.05376120e+02; GEOM_ELEM_4surface->pt[383]=1.10474822e+02;
          GEOM_ELEM_4surface->pt[384]=5.16487635e+01; GEOM_ELEM_4surface->pt[385]=1.05376502e+02; GEOM_ELEM_4surface->pt[386]=1.16986437e+02;
          GEOM_ELEM_4surface->pt[387]=6.13208056e+01; GEOM_ELEM_4surface->pt[388]=1.05376651e+02; GEOM_ELEM_4surface->pt[389]=1.19753701e+02;
          GEOM_ELEM_4surface->pt[390]=7.37633636e+01; GEOM_ELEM_4surface->pt[391]=1.05376800e+02; GEOM_ELEM_4surface->pt[392]=1.22991031e+02;
          GEOM_ELEM_4surface->pt[393]=8.63113019e+01; GEOM_ELEM_4surface->pt[394]=1.05376888e+02; GEOM_ELEM_4surface->pt[395]=1.25769142e+02;
          GEOM_ELEM_4surface->pt[396]=8.96063248e+01; GEOM_ELEM_4surface->pt[397]=1.05376908e+02; GEOM_ELEM_4surface->pt[398]=1.26474613e+02;
          GEOM_ELEM_4surface->pt[399]=9.66278759e+01; GEOM_ELEM_4surface->pt[400]=1.05376940e+02; GEOM_ELEM_4surface->pt[401]=1.27891894e+02;
          GEOM_ELEM_4surface->pt[402]=1.03701896e+02; GEOM_ELEM_4surface->pt[403]=1.05376934e+02; GEOM_ELEM_4surface->pt[404]=1.29027230e+02;
          GEOM_ELEM_4surface->pt[405]=1.08087374e+02; GEOM_ELEM_4surface->pt[406]=1.05376920e+02; GEOM_ELEM_4surface->pt[407]=1.29649884e+02;
          GEOM_ELEM_4surface->pt[408]=1.16386243e+02; GEOM_ELEM_4surface->pt[409]=1.05376879e+02; GEOM_ELEM_4surface->pt[410]=1.30721744e+02;
          GEOM_ELEM_4surface->pt[411]=1.24706756e+02; GEOM_ELEM_4surface->pt[412]=1.05376815e+02; GEOM_ELEM_4surface->pt[413]=1.31608799e+02;
          GEOM_ELEM_4surface->pt[414]=1.27522513e+02; GEOM_ELEM_4surface->pt[415]=1.05376788e+02; GEOM_ELEM_4surface->pt[416]=1.31873833e+02;
          GEOM_ELEM_4surface->pt[417]=1.37152085e+02; GEOM_ELEM_4surface->pt[418]=1.05376697e+02; GEOM_ELEM_4surface->pt[419]=1.32772283e+02;
          GEOM_ELEM_4surface->pt[420]=1.46782534e+02; GEOM_ELEM_4surface->pt[421]=1.05376604e+02; GEOM_ELEM_4surface->pt[422]=1.33660593e+02;
          GEOM_ELEM_4surface->pt[423]=1.54498938e+02; GEOM_ELEM_4surface->pt[424]=1.05376527e+02; GEOM_ELEM_4surface->pt[425]=1.34351169e+02;
          GEOM_ELEM_4surface->pt[426]=1.84544892e+02; GEOM_ELEM_4surface->pt[427]=1.05376223e+02; GEOM_ELEM_4surface->pt[428]=1.37007870e+02;
          GEOM_ELEM_4surface->pt[429]=2.14609199e+02; GEOM_ELEM_4surface->pt[430]=1.05375891e+02; GEOM_ELEM_4surface->pt[431]=1.39453472e+02;
          GEOM_ELEM_4surface->pt[432]=2.39931261e+02; GEOM_ELEM_4surface->pt[433]=1.05375596e+02; GEOM_ELEM_4surface->pt[434]=1.41388993e+02;
          GEOM_ELEM_4surface->pt[435]=2.89367995e+02; GEOM_ELEM_4surface->pt[436]=1.05374997e+02; GEOM_ELEM_4surface->pt[437]=1.44997079e+02;
          GEOM_ELEM_4surface->pt[438]=3.38832034e+02; GEOM_ELEM_4surface->pt[439]=1.05374347e+02; GEOM_ELEM_4surface->pt[440]=1.48209692e+02;
          GEOM_ELEM_4surface->pt[441]=3.64613582e+02; GEOM_ELEM_4surface->pt[442]=1.05373992e+02; GEOM_ELEM_4surface->pt[443]=1.49759753e+02;
          GEOM_ELEM_4surface->pt[444]=3.89741529e+02; GEOM_ELEM_4surface->pt[445]=1.05373635e+02; GEOM_ELEM_4surface->pt[446]=1.51184644e+02;
          GEOM_ELEM_4surface->pt[447]=4.14874003e+02; GEOM_ELEM_4surface->pt[448]=1.05373267e+02; GEOM_ELEM_4surface->pt[449]=1.52524678e+02;
		  }
          else  status=36;
//
		  GEOM_ELEM_4surface->nwgt= 150;
		  GEOM_ELEM_4surface->wgt = (double *) malloc(GEOM_ELEM_4surface->nwgt*sizeof(double));
		  if ( NULL!= GEOM_ELEM_4surface->wgt )
		  {
		     for (int IBoucle = 0; IBoucle < GEOM_ELEM_4surface->nwgt; IBoucle++)
		     {
		       GEOM_ELEM_4surface->wgt[IBoucle] = 1.0000;
		     }
		  }
          else  status=36;
//
	  }
          else  status=36;
//
		   GEOM_ELEM_4polyline = (NCLX_mdl_polyline *) malloc(sizeof(NCLX_mdl_polyline));
//  Set polyline boundary of Trimsf 
		   GEOM_ELEM_4polyline->header.key      = 3;
		   GEOM_ELEM_4polyline->header.relnum   = NCLX_MDL_POLYLINE;
		   GEOM_ELEM_4polyline->header.subscript= 0;
		   GEOM_ELEM_4polyline->npts = 624;
		   GEOM_ELEM_4polyline->pts = (double *) malloc(GEOM_ELEM_4polyline->npts*3*sizeof(double));
		   if ( NULL!= GEOM_ELEM_4polyline->pts )
		   {
		     for (int IBoucle = 0; IBoucle < GEOM_ELEM_4polyline->npts; IBoucle++)
		     {
		       GEOM_ELEM_4polyline->pts[((3*IBoucle)+2)] = 0.0000;
		     }
           GEOM_ELEM_4polyline->pts[0]=8.44955009e+02; GEOM_ELEM_4polyline->pts[1]=8.53093590e+01;   
           GEOM_ELEM_4polyline->pts[3]=8.44701963e+02; GEOM_ELEM_4polyline->pts[4]=1.92653297e+01;   
           GEOM_ELEM_4polyline->pts[6]=8.42232517e+02; GEOM_ELEM_4polyline->pts[7]=1.92653273e+01;   
           GEOM_ELEM_4polyline->pts[9]=8.39019117e+02; GEOM_ELEM_4polyline->pts[10]=1.92653286e+01;   
           GEOM_ELEM_4polyline->pts[12]=8.36012006e+02; GEOM_ELEM_4polyline->pts[13]=1.92653348e+01;   
           GEOM_ELEM_4polyline->pts[15]=8.34566790e+02; GEOM_ELEM_4polyline->pts[16]=1.92653399e+01;   
           GEOM_ELEM_4polyline->pts[18]=8.33142018e+02; GEOM_ELEM_4polyline->pts[19]=1.92653461e+01;   
           GEOM_ELEM_4polyline->pts[21]=8.31776865e+02; GEOM_ELEM_4polyline->pts[22]=1.92653532e+01;   
           GEOM_ELEM_4polyline->pts[24]=8.28581650e+02; GEOM_ELEM_4polyline->pts[25]=1.92653731e+01;   
           GEOM_ELEM_4polyline->pts[27]=8.27097435e+02; GEOM_ELEM_4polyline->pts[28]=1.92653835e+01;   
           GEOM_ELEM_4polyline->pts[30]=8.25717122e+02; GEOM_ELEM_4polyline->pts[31]=1.92653935e+01;   
           GEOM_ELEM_4polyline->pts[33]=8.25284722e+02; GEOM_ELEM_4polyline->pts[34]=1.92653967e+01;   
           GEOM_ELEM_4polyline->pts[36]=8.24922735e+02; GEOM_ELEM_4polyline->pts[37]=1.92704614e+01;   
           GEOM_ELEM_4polyline->pts[39]=8.24560449e+02; GEOM_ELEM_4polyline->pts[40]=1.92858820e+01;   
           GEOM_ELEM_4polyline->pts[42]=8.24196297e+02; GEOM_ELEM_4polyline->pts[43]=1.93117272e+01;   
           GEOM_ELEM_4polyline->pts[45]=8.23831665e+02; GEOM_ELEM_4polyline->pts[46]=1.93480022e+01;   
           GEOM_ELEM_4polyline->pts[48]=8.23468115e+02; GEOM_ELEM_4polyline->pts[49]=1.93946248e+01;   
           GEOM_ELEM_4polyline->pts[51]=8.23106718e+02; GEOM_ELEM_4polyline->pts[52]=1.94514707e+01;   
           GEOM_ELEM_4polyline->pts[54]=8.22747427e+02; GEOM_ELEM_4polyline->pts[55]=1.95185122e+01;   
           GEOM_ELEM_4polyline->pts[57]=8.22390320e+02; GEOM_ELEM_4polyline->pts[58]=1.95957218e+01;   
           GEOM_ELEM_4polyline->pts[60]=8.22035652e+02; GEOM_ELEM_4polyline->pts[61]=1.96830425e+01;   
           GEOM_ELEM_4polyline->pts[63]=8.21683712e+02; GEOM_ELEM_4polyline->pts[64]=1.97804026e+01;   
           GEOM_ELEM_4polyline->pts[66]=8.21334786e+02; GEOM_ELEM_4polyline->pts[67]=1.98877217e+01;   
           GEOM_ELEM_4polyline->pts[69]=8.20989162e+02; GEOM_ELEM_4polyline->pts[70]=2.00049110e+01;   
           GEOM_ELEM_4polyline->pts[72]=8.20647123e+02; GEOM_ELEM_4polyline->pts[73]=2.01318735e+01;   
           GEOM_ELEM_4polyline->pts[75]=8.20308948e+02; GEOM_ELEM_4polyline->pts[76]=2.02685047e+01;   
           GEOM_ELEM_4polyline->pts[78]=8.19974912e+02; GEOM_ELEM_4polyline->pts[79]=2.04146924e+01;   
           GEOM_ELEM_4polyline->pts[81]=8.19645282e+02; GEOM_ELEM_4polyline->pts[82]=2.05703182e+01;   
           GEOM_ELEM_4polyline->pts[84]=8.19320321e+02; GEOM_ELEM_4polyline->pts[85]=2.07352574e+01;   
           GEOM_ELEM_4polyline->pts[87]=8.19000281e+02; GEOM_ELEM_4polyline->pts[88]=2.09093801e+01;   
           GEOM_ELEM_4polyline->pts[90]=8.18901675e+02; GEOM_ELEM_4polyline->pts[91]=2.09654497e+01;   
           GEOM_ELEM_4polyline->pts[93]=8.14576100e+02; GEOM_ELEM_4polyline->pts[94]=2.34539059e+01;   
           GEOM_ELEM_4polyline->pts[96]=8.10265506e+02; GEOM_ELEM_4polyline->pts[97]=2.59342258e+01;   
           GEOM_ELEM_4polyline->pts[99]=8.08261059e+02; GEOM_ELEM_4polyline->pts[100]=2.70876565e+01;   
           GEOM_ELEM_4polyline->pts[102]=8.07943710e+02; GEOM_ELEM_4polyline->pts[103]=2.72644865e+01;   
           GEOM_ELEM_4polyline->pts[105]=8.07621693e+02; GEOM_ELEM_4polyline->pts[106]=2.74320321e+01;   
           GEOM_ELEM_4polyline->pts[108]=8.07294854e+02; GEOM_ELEM_4polyline->pts[109]=2.75903891e+01;   
           GEOM_ELEM_4polyline->pts[111]=8.06963439e+02; GEOM_ELEM_4polyline->pts[112]=2.77394307e+01;   
           GEOM_ELEM_4polyline->pts[114]=8.06627701e+02; GEOM_ELEM_4polyline->pts[115]=2.78790342e+01;   
           GEOM_ELEM_4polyline->pts[117]=8.06287903e+02; GEOM_ELEM_4polyline->pts[118]=2.80090816e+01;   
           GEOM_ELEM_4polyline->pts[120]=8.05944316e+02; GEOM_ELEM_4polyline->pts[121]=2.81294603e+01;   
           GEOM_ELEM_4polyline->pts[123]=8.05597216e+02; GEOM_ELEM_4polyline->pts[124]=2.82400639e+01;   
           GEOM_ELEM_4polyline->pts[126]=8.05246887e+02; GEOM_ELEM_4polyline->pts[127]=2.83407931e+01;   
           GEOM_ELEM_4polyline->pts[129]=8.04893617e+02; GEOM_ELEM_4polyline->pts[130]=2.84315560e+01;   
           GEOM_ELEM_4polyline->pts[132]=8.04537697e+02; GEOM_ELEM_4polyline->pts[133]=2.85122685e+01;   
           GEOM_ELEM_4polyline->pts[135]=8.04179423e+02; GEOM_ELEM_4polyline->pts[136]=2.85828554e+01;   
           GEOM_ELEM_4polyline->pts[138]=8.03819091e+02; GEOM_ELEM_4polyline->pts[139]=2.86432498e+01;   
           GEOM_ELEM_4polyline->pts[141]=8.03456998e+02; GEOM_ELEM_4polyline->pts[142]=2.86933940e+01;   
           GEOM_ELEM_4polyline->pts[144]=8.03093440e+02; GEOM_ELEM_4polyline->pts[145]=2.87332391e+01;   
           GEOM_ELEM_4polyline->pts[147]=8.02728711e+02; GEOM_ELEM_4polyline->pts[148]=2.87627453e+01;   
           GEOM_ELEM_4polyline->pts[150]=8.02363099e+02; GEOM_ELEM_4polyline->pts[151]=2.87818819e+01;   
           GEOM_ELEM_4polyline->pts[153]=8.01996883e+02; GEOM_ELEM_4polyline->pts[154]=2.87906260e+01;   
           GEOM_ELEM_4polyline->pts[156]=8.01878503e+02; GEOM_ELEM_4polyline->pts[157]=2.87912269e+01;   
           GEOM_ELEM_4polyline->pts[159]=7.97873384e+02; GEOM_ELEM_4polyline->pts[160]=2.87912833e+01;   
           GEOM_ELEM_4polyline->pts[162]=7.93084806e+02; GEOM_ELEM_4polyline->pts[163]=2.87913554e+01;   
           GEOM_ELEM_4polyline->pts[165]=7.88153694e+02; GEOM_ELEM_4polyline->pts[166]=2.87914346e+01;   
           GEOM_ELEM_4polyline->pts[168]=7.83059345e+02; GEOM_ELEM_4polyline->pts[169]=2.87915214e+01;   
           GEOM_ELEM_4polyline->pts[171]=7.77785387e+02; GEOM_ELEM_4polyline->pts[172]=2.87916162e+01;   
           GEOM_ELEM_4polyline->pts[174]=7.72297098e+02; GEOM_ELEM_4polyline->pts[175]=2.87917198e+01;   
           GEOM_ELEM_4polyline->pts[177]=7.66565432e+02; GEOM_ELEM_4polyline->pts[178]=2.87918329e+01;   
           GEOM_ELEM_4polyline->pts[180]=7.60696864e+02; GEOM_ELEM_4polyline->pts[181]=2.87919536e+01;   
           GEOM_ELEM_4polyline->pts[183]=7.54777910e+02; GEOM_ELEM_4polyline->pts[184]=2.87920802e+01;   
           GEOM_ELEM_4polyline->pts[186]=7.48781457e+02; GEOM_ELEM_4polyline->pts[187]=2.87922132e+01;   
           GEOM_ELEM_4polyline->pts[189]=7.42671272e+02; GEOM_ELEM_4polyline->pts[190]=2.87923537e+01;   
           GEOM_ELEM_4polyline->pts[192]=7.36437242e+02; GEOM_ELEM_4polyline->pts[193]=2.87925021e+01;   
           GEOM_ELEM_4polyline->pts[195]=7.30103666e+02; GEOM_ELEM_4polyline->pts[196]=2.87926579e+01;   
           GEOM_ELEM_4polyline->pts[198]=7.23706531e+02; GEOM_ELEM_4polyline->pts[199]=2.87928205e+01;   
           GEOM_ELEM_4polyline->pts[201]=7.17254117e+02; GEOM_ELEM_4polyline->pts[202]=2.87929896e+01;   
           GEOM_ELEM_4polyline->pts[204]=7.10689616e+02; GEOM_ELEM_4polyline->pts[205]=2.87931667e+01;   
           GEOM_ELEM_4polyline->pts[207]=7.03995520e+02; GEOM_ELEM_4polyline->pts[208]=2.87933524e+01;   
           GEOM_ELEM_4polyline->pts[210]=6.97481258e+02; GEOM_ELEM_4polyline->pts[211]=2.87935382e+01;   
           GEOM_ELEM_4polyline->pts[213]=6.91194893e+02; GEOM_ELEM_4polyline->pts[214]=2.87937224e+01;   
           GEOM_ELEM_4polyline->pts[216]=6.85056958e+02; GEOM_ELEM_4polyline->pts[217]=2.87939071e+01;   
           GEOM_ELEM_4polyline->pts[219]=6.79022633e+02; GEOM_ELEM_4polyline->pts[220]=2.87940938e+01;   
           GEOM_ELEM_4polyline->pts[222]=6.73773675e+02; GEOM_ELEM_4polyline->pts[223]=2.87942605e+01;   
           GEOM_ELEM_4polyline->pts[225]=6.73457445e+02; GEOM_ELEM_4polyline->pts[226]=2.87905082e+01;   
           GEOM_ELEM_4polyline->pts[228]=6.73091773e+02; GEOM_ELEM_4polyline->pts[229]=2.87765004e+01;   
           GEOM_ELEM_4polyline->pts[231]=6.72726633e+02; GEOM_ELEM_4polyline->pts[232]=2.87521336e+01;   
           GEOM_ELEM_4polyline->pts[234]=6.72362254e+02; GEOM_ELEM_4polyline->pts[235]=2.87174224e+01;   
           GEOM_ELEM_4polyline->pts[237]=6.71998905e+02; GEOM_ELEM_4polyline->pts[238]=2.86723886e+01;   
           GEOM_ELEM_4polyline->pts[240]=6.71636860e+02; GEOM_ELEM_4polyline->pts[241]=2.86170619e+01;   
           GEOM_ELEM_4polyline->pts[243]=6.71276399e+02; GEOM_ELEM_4polyline->pts[244]=2.85514801e+01;   
           GEOM_ELEM_4polyline->pts[246]=6.70917806e+02; GEOM_ELEM_4polyline->pts[247]=2.84756892e+01;   
           GEOM_ELEM_4polyline->pts[249]=6.70561372e+02; GEOM_ELEM_4polyline->pts[250]=2.83897441e+01;   
           GEOM_ELEM_4polyline->pts[252]=6.70207386e+02; GEOM_ELEM_4polyline->pts[253]=2.82937089e+01;   
           GEOM_ELEM_4polyline->pts[255]=6.69856142e+02; GEOM_ELEM_4polyline->pts[256]=2.81876566e+01;   
           GEOM_ELEM_4polyline->pts[258]=6.69507932e+02; GEOM_ELEM_4polyline->pts[259]=2.80716692e+01;   
           GEOM_ELEM_4polyline->pts[261]=6.69163051e+02; GEOM_ELEM_4polyline->pts[262]=2.79458384e+01;   
           GEOM_ELEM_4polyline->pts[264]=6.68821796e+02; GEOM_ELEM_4polyline->pts[265]=2.78102666e+01;   
           GEOM_ELEM_4polyline->pts[267]=6.68484457e+02; GEOM_ELEM_4polyline->pts[268]=2.76650637e+01;   
           GEOM_ELEM_4polyline->pts[270]=6.68151321e+02; GEOM_ELEM_4polyline->pts[271]=2.75103491e+01;   
           GEOM_ELEM_4polyline->pts[273]=6.67822676e+02; GEOM_ELEM_4polyline->pts[274]=2.73462511e+01;   
           GEOM_ELEM_4polyline->pts[276]=6.67498802e+02; GEOM_ELEM_4polyline->pts[277]=2.71729064e+01;   
           GEOM_ELEM_4polyline->pts[279]=6.67179975e+02; GEOM_ELEM_4polyline->pts[280]=2.69904597e+01;   
           GEOM_ELEM_4polyline->pts[282]=6.66866465e+02; GEOM_ELEM_4polyline->pts[283]=2.67990629e+01;   
           GEOM_ELEM_4polyline->pts[285]=6.66558534e+02; GEOM_ELEM_4polyline->pts[286]=2.65988740e+01;   
           GEOM_ELEM_4polyline->pts[288]=6.66256437e+02; GEOM_ELEM_4polyline->pts[289]=2.63900569e+01;   
           GEOM_ELEM_4polyline->pts[291]=6.65960419e+02; GEOM_ELEM_4polyline->pts[292]=2.61727799e+01;   
           GEOM_ELEM_4polyline->pts[294]=6.65670714e+02; GEOM_ELEM_4polyline->pts[295]=2.59472152e+01;   
           GEOM_ELEM_4polyline->pts[297]=6.65387549e+02; GEOM_ELEM_4polyline->pts[298]=2.57135376e+01;   
           GEOM_ELEM_4polyline->pts[300]=6.65111140e+02; GEOM_ELEM_4polyline->pts[301]=2.54719240e+01;   
           GEOM_ELEM_4polyline->pts[303]=6.64841689e+02; GEOM_ELEM_4polyline->pts[304]=2.52225520e+01;   
           GEOM_ELEM_4polyline->pts[306]=6.64690861e+02; GEOM_ELEM_4polyline->pts[307]=2.50765830e+01;   
           GEOM_ELEM_4polyline->pts[309]=6.59904053e+02; GEOM_ELEM_4polyline->pts[310]=2.03635990e+01;   
           GEOM_ELEM_4polyline->pts[312]=6.54352563e+02; GEOM_ELEM_4polyline->pts[313]=1.48947499e+01;   
           GEOM_ELEM_4polyline->pts[315]=6.48927718e+02; GEOM_ELEM_4polyline->pts[316]=9.54799574e+00;   
           GEOM_ELEM_4polyline->pts[318]=6.45557483e+02; GEOM_ELEM_4polyline->pts[319]=6.22582038e+00;   
           GEOM_ELEM_4polyline->pts[321]=6.45329604e+02; GEOM_ELEM_4polyline->pts[322]=6.00654649e+00;   
           GEOM_ELEM_4polyline->pts[324]=6.45059320e+02; GEOM_ELEM_4polyline->pts[325]=5.75988489e+00;   
           GEOM_ELEM_4polyline->pts[327]=6.44782178e+02; GEOM_ELEM_4polyline->pts[328]=5.52100703e+00;   
           GEOM_ELEM_4polyline->pts[330]=6.44498353e+02; GEOM_ELEM_4polyline->pts[331]=5.29006227e+00;   
           GEOM_ELEM_4polyline->pts[333]=6.44208051e+02; GEOM_ELEM_4polyline->pts[334]=5.06722125e+00;   
           GEOM_ELEM_4polyline->pts[336]=6.43911488e+02; GEOM_ELEM_4polyline->pts[337]=4.85265412e+00;   
           GEOM_ELEM_4polyline->pts[339]=6.43608890e+02; GEOM_ELEM_4polyline->pts[340]=4.64652941e+00;   
           GEOM_ELEM_4polyline->pts[342]=6.43300495e+02; GEOM_ELEM_4polyline->pts[343]=4.44901290e+00;   
           GEOM_ELEM_4polyline->pts[345]=6.42986545e+02; GEOM_ELEM_4polyline->pts[346]=4.26026656e+00;   
           GEOM_ELEM_4polyline->pts[348]=6.42667294e+02; GEOM_ELEM_4polyline->pts[349]=4.08044749e+00;   
           GEOM_ELEM_4polyline->pts[351]=6.42342999e+02; GEOM_ELEM_4polyline->pts[352]=3.90970691e+00;   
           GEOM_ELEM_4polyline->pts[354]=6.42013926e+02; GEOM_ELEM_4polyline->pts[355]=3.74818928e+00;   
           GEOM_ELEM_4polyline->pts[357]=6.41680342e+02; GEOM_ELEM_4polyline->pts[358]=3.59603149e+00;   
           GEOM_ELEM_4polyline->pts[360]=6.41342519e+02; GEOM_ELEM_4polyline->pts[361]=3.45336215e+00;   
           GEOM_ELEM_4polyline->pts[363]=6.41000731e+02; GEOM_ELEM_4polyline->pts[364]=3.32030148e+00;   
           GEOM_ELEM_4polyline->pts[366]=6.40655261e+02; GEOM_ELEM_4polyline->pts[367]=3.19696444e+00;   
           GEOM_ELEM_4polyline->pts[369]=6.40306403e+02; GEOM_ELEM_4polyline->pts[370]=3.08346071e+00;   
           GEOM_ELEM_4polyline->pts[372]=6.39954456e+02; GEOM_ELEM_4polyline->pts[373]=2.97989093e+00;   
           GEOM_ELEM_4polyline->pts[375]=6.39599719e+02; GEOM_ELEM_4polyline->pts[376]=2.88634659e+00;   
           GEOM_ELEM_4polyline->pts[378]=6.39242494e+02; GEOM_ELEM_4polyline->pts[379]=2.80290964e+00;   
           GEOM_ELEM_4polyline->pts[381]=6.38883083e+02; GEOM_ELEM_4polyline->pts[382]=2.72965228e+00;   
           GEOM_ELEM_4polyline->pts[384]=6.38521785e+02; GEOM_ELEM_4polyline->pts[385]=2.66663688e+00;   
           GEOM_ELEM_4polyline->pts[387]=6.38158897e+02; GEOM_ELEM_4polyline->pts[388]=2.61391605e+00;   
           GEOM_ELEM_4polyline->pts[390]=6.37794713e+02; GEOM_ELEM_4polyline->pts[391]=2.57153283e+00;   
           GEOM_ELEM_4polyline->pts[393]=6.37429520e+02; GEOM_ELEM_4polyline->pts[394]=2.53952098e+00;   
           GEOM_ELEM_4polyline->pts[396]=6.37063601e+02; GEOM_ELEM_4polyline->pts[397]=2.51790544e+00;   
           GEOM_ELEM_4polyline->pts[399]=6.36697231e+02; GEOM_ELEM_4polyline->pts[400]=2.50670283e+00;   
           GEOM_ELEM_4polyline->pts[402]=6.36488663e+02; GEOM_ELEM_4polyline->pts[403]=2.50498038e+00;   
           GEOM_ELEM_4polyline->pts[405]=6.32486174e+02; GEOM_ELEM_4polyline->pts[406]=2.50513858e+00;   
           GEOM_ELEM_4polyline->pts[408]=6.27747233e+02; GEOM_ELEM_4polyline->pts[409]=2.50533080e+00;   
           GEOM_ELEM_4polyline->pts[411]=6.23115989e+02; GEOM_ELEM_4polyline->pts[412]=2.50552400e+00;   
           GEOM_ELEM_4polyline->pts[414]=6.18576351e+02; GEOM_ELEM_4polyline->pts[415]=2.50571865e+00;   
           GEOM_ELEM_4polyline->pts[417]=6.14122492e+02; GEOM_ELEM_4polyline->pts[418]=2.50591477e+00;   
           GEOM_ELEM_4polyline->pts[420]=6.09761024e+02; GEOM_ELEM_4polyline->pts[421]=2.50611184e+00;   
           GEOM_ELEM_4polyline->pts[423]=6.05497843e+02; GEOM_ELEM_4polyline->pts[424]=2.50630935e+00;   
           GEOM_ELEM_4polyline->pts[426]=6.01329193e+02; GEOM_ELEM_4polyline->pts[427]=2.50650724e+00;   
           GEOM_ELEM_4polyline->pts[429]=5.97240752e+02; GEOM_ELEM_4polyline->pts[430]=2.50670603e+00;   
           GEOM_ELEM_4polyline->pts[432]=5.93212163e+02; GEOM_ELEM_4polyline->pts[433]=2.50690664e+00;   
           GEOM_ELEM_4polyline->pts[435]=5.89224223e+02; GEOM_ELEM_4polyline->pts[436]=2.50711007e+00;   
           GEOM_ELEM_4polyline->pts[438]=5.85250178e+02; GEOM_ELEM_4polyline->pts[439]=2.50731767e+00;   
           GEOM_ELEM_4polyline->pts[441]=5.81264791e+02; GEOM_ELEM_4polyline->pts[442]=2.50753079e+00;   
           GEOM_ELEM_4polyline->pts[444]=5.77305256e+02; GEOM_ELEM_4polyline->pts[445]=2.50774747e+00;   
           GEOM_ELEM_4polyline->pts[447]=5.73410319e+02; GEOM_ELEM_4polyline->pts[448]=2.50796555e+00;   
           GEOM_ELEM_4polyline->pts[450]=5.69615541e+02; GEOM_ELEM_4polyline->pts[451]=2.50818292e+00;   
           GEOM_ELEM_4polyline->pts[453]=5.65946145e+02; GEOM_ELEM_4polyline->pts[454]=2.50839798e+00;   
           GEOM_ELEM_4polyline->pts[456]=5.62414162e+02; GEOM_ELEM_4polyline->pts[457]=2.50860987e+00;   
           GEOM_ELEM_4polyline->pts[459]=5.59021366e+02; GEOM_ELEM_4polyline->pts[460]=2.50881831e+00;   
           GEOM_ELEM_4polyline->pts[462]=5.55763295e+02; GEOM_ELEM_4polyline->pts[463]=2.50902335e+00;   
           GEOM_ELEM_4polyline->pts[465]=5.52630340e+02; GEOM_ELEM_4polyline->pts[466]=2.50922531e+00;   
           GEOM_ELEM_4polyline->pts[468]=5.49613642e+02; GEOM_ELEM_4polyline->pts[469]=2.50942454e+00;   
           GEOM_ELEM_4polyline->pts[471]=5.46704986e+02; GEOM_ELEM_4polyline->pts[472]=2.50962137e+00;   
           GEOM_ELEM_4polyline->pts[474]=5.43897267e+02; GEOM_ELEM_4polyline->pts[475]=2.50981609e+00;   
           GEOM_ELEM_4polyline->pts[477]=5.41184305e+02; GEOM_ELEM_4polyline->pts[478]=2.51000891e+00;   
           GEOM_ELEM_4polyline->pts[480]=5.38560645e+02; GEOM_ELEM_4polyline->pts[481]=2.51020005e+00;   
           GEOM_ELEM_4polyline->pts[483]=5.36021451e+02; GEOM_ELEM_4polyline->pts[484]=2.51038962e+00;   
           GEOM_ELEM_4polyline->pts[486]=5.33562448e+02; GEOM_ELEM_4polyline->pts[487]=2.51057775e+00;   
           GEOM_ELEM_4polyline->pts[489]=5.31179892e+02; GEOM_ELEM_4polyline->pts[490]=2.51076450e+00;   
           GEOM_ELEM_4polyline->pts[492]=5.28870390e+02; GEOM_ELEM_4polyline->pts[493]=2.51094992e+00;   
           GEOM_ELEM_4polyline->pts[495]=5.26629917e+02; GEOM_ELEM_4polyline->pts[496]=2.51113417e+00;   
           GEOM_ELEM_4polyline->pts[498]=5.24454172e+02; GEOM_ELEM_4polyline->pts[499]=2.51131743e+00;   
           GEOM_ELEM_4polyline->pts[501]=5.22339845e+02; GEOM_ELEM_4polyline->pts[502]=2.51149978e+00;   
           GEOM_ELEM_4polyline->pts[504]=5.20283881e+02; GEOM_ELEM_4polyline->pts[505]=2.51168126e+00;   
           GEOM_ELEM_4polyline->pts[507]=5.18283487e+02; GEOM_ELEM_4polyline->pts[508]=2.51186191e+00;   
           GEOM_ELEM_4polyline->pts[510]=5.16336059e+02; GEOM_ELEM_4polyline->pts[511]=2.51204172e+00;   
           GEOM_ELEM_4polyline->pts[513]=5.14439098e+02; GEOM_ELEM_4polyline->pts[514]=2.51222070e+00;   
           GEOM_ELEM_4polyline->pts[516]=5.12590154e+02; GEOM_ELEM_4polyline->pts[517]=2.51239887e+00;   
           GEOM_ELEM_4polyline->pts[519]=5.10786798e+02; GEOM_ELEM_4polyline->pts[520]=2.51257625e+00;   
           GEOM_ELEM_4polyline->pts[522]=5.09026618e+02; GEOM_ELEM_4polyline->pts[523]=2.51275286e+00;   
           GEOM_ELEM_4polyline->pts[525]=5.07307283e+02; GEOM_ELEM_4polyline->pts[526]=2.51292875e+00;   
           GEOM_ELEM_4polyline->pts[528]=5.05626537e+02; GEOM_ELEM_4polyline->pts[529]=2.51310398e+00;   
           GEOM_ELEM_4polyline->pts[531]=5.03982141e+02; GEOM_ELEM_4polyline->pts[532]=2.51327859e+00;   
           GEOM_ELEM_4polyline->pts[534]=5.02371437e+02; GEOM_ELEM_4polyline->pts[535]=2.51345273e+00;   
           GEOM_ELEM_4polyline->pts[537]=5.00794047e+02; GEOM_ELEM_4polyline->pts[538]=2.51362631e+00;   
           GEOM_ELEM_4polyline->pts[540]=4.99251771e+02; GEOM_ELEM_4polyline->pts[541]=2.51379905e+00;   
           GEOM_ELEM_4polyline->pts[543]=4.97742043e+02; GEOM_ELEM_4polyline->pts[544]=2.51397104e+00;   
           GEOM_ELEM_4polyline->pts[546]=4.96261447e+02; GEOM_ELEM_4polyline->pts[547]=2.51414247e+00;   
           GEOM_ELEM_4polyline->pts[549]=4.94807402e+02; GEOM_ELEM_4polyline->pts[550]=2.51431339e+00;   
           GEOM_ELEM_4polyline->pts[552]=4.93377959e+02; GEOM_ELEM_4polyline->pts[553]=2.51448381e+00;   
           GEOM_ELEM_4polyline->pts[555]=4.91971487e+02; GEOM_ELEM_4polyline->pts[556]=2.51465366e+00;   
           GEOM_ELEM_4polyline->pts[558]=4.90586424e+02; GEOM_ELEM_4polyline->pts[559]=2.51482291e+00;   
           GEOM_ELEM_4polyline->pts[561]=4.89221103e+02; GEOM_ELEM_4polyline->pts[562]=2.51499154e+00;   
           GEOM_ELEM_4polyline->pts[564]=4.87873641e+02; GEOM_ELEM_4polyline->pts[565]=2.51515959e+00;   
           GEOM_ELEM_4polyline->pts[567]=4.86541869e+02; GEOM_ELEM_4polyline->pts[568]=2.51532717e+00;   
           GEOM_ELEM_4polyline->pts[570]=4.85223534e+02; GEOM_ELEM_4polyline->pts[571]=2.51549441e+00;   
           GEOM_ELEM_4polyline->pts[573]=4.83917048e+02; GEOM_ELEM_4polyline->pts[574]=2.51566153e+00;   
           GEOM_ELEM_4polyline->pts[576]=4.82625046e+02; GEOM_ELEM_4polyline->pts[577]=2.51582860e+00;   
           GEOM_ELEM_4polyline->pts[579]=4.81346621e+02; GEOM_ELEM_4polyline->pts[580]=2.51599565e+00;   
           GEOM_ELEM_4polyline->pts[582]=4.80068230e+02; GEOM_ELEM_4polyline->pts[583]=2.51616384e+00;   
           GEOM_ELEM_4polyline->pts[585]=4.78800287e+02; GEOM_ELEM_4polyline->pts[586]=2.51633106e+00;   
           GEOM_ELEM_4polyline->pts[588]=4.77567758e+02; GEOM_ELEM_4polyline->pts[589]=2.51649338e+00;   
           GEOM_ELEM_4polyline->pts[591]=4.76387453e+02; GEOM_ELEM_4polyline->pts[592]=2.51664838e+00;   
           GEOM_ELEM_4polyline->pts[594]=4.75314374e+02; GEOM_ELEM_4polyline->pts[595]=2.51678917e+00;   
           GEOM_ELEM_4polyline->pts[597]=4.74241635e+02; GEOM_ELEM_4polyline->pts[598]=2.51693017e+00;   
           GEOM_ELEM_4polyline->pts[600]=4.73212123e+02; GEOM_ELEM_4polyline->pts[601]=2.51706563e+00;   
           GEOM_ELEM_4polyline->pts[603]=4.72199526e+02; GEOM_ELEM_4polyline->pts[604]=2.51719854e+00;   
           GEOM_ELEM_4polyline->pts[606]=4.71196940e+02; GEOM_ELEM_4polyline->pts[607]=2.51732924e+00;   
           GEOM_ELEM_4polyline->pts[609]=4.70208702e+02; GEOM_ELEM_4polyline->pts[610]=2.51745666e+00;   
           GEOM_ELEM_4polyline->pts[612]=4.69241470e+02; GEOM_ELEM_4polyline->pts[613]=2.51757974e+00;   
           GEOM_ELEM_4polyline->pts[615]=4.68300100e+02; GEOM_ELEM_4polyline->pts[616]=2.51769812e+00;   
           GEOM_ELEM_4polyline->pts[618]=4.67396349e+02; GEOM_ELEM_4polyline->pts[619]=2.51781117e+00;   
           GEOM_ELEM_4polyline->pts[621]=4.66522455e+02; GEOM_ELEM_4polyline->pts[622]=2.51792008e+00;   
           GEOM_ELEM_4polyline->pts[624]=4.65645046e+02; GEOM_ELEM_4polyline->pts[625]=2.51802863e+00;   
           GEOM_ELEM_4polyline->pts[627]=4.64745949e+02; GEOM_ELEM_4polyline->pts[628]=2.51813823e+00;   
           GEOM_ELEM_4polyline->pts[630]=4.63825631e+02; GEOM_ELEM_4polyline->pts[631]=2.51824775e+00;   
           GEOM_ELEM_4polyline->pts[633]=4.62900391e+02; GEOM_ELEM_4polyline->pts[634]=2.51835422e+00;   
           GEOM_ELEM_4polyline->pts[636]=4.61983196e+02; GEOM_ELEM_4polyline->pts[637]=2.51845568e+00;   
           GEOM_ELEM_4polyline->pts[639]=4.61064693e+02; GEOM_ELEM_4polyline->pts[640]=2.51855335e+00;   
           GEOM_ELEM_4polyline->pts[642]=4.60115537e+02; GEOM_ELEM_4polyline->pts[643]=2.51865100e+00;   
           GEOM_ELEM_4polyline->pts[645]=4.59116267e+02; GEOM_ELEM_4polyline->pts[646]=2.51875050e+00;   
           GEOM_ELEM_4polyline->pts[648]=4.58076345e+02; GEOM_ELEM_4polyline->pts[649]=2.51885041e+00;   
           GEOM_ELEM_4polyline->pts[651]=4.56998765e+02; GEOM_ELEM_4polyline->pts[652]=2.51895011e+00;   
           GEOM_ELEM_4polyline->pts[654]=4.55884681e+02; GEOM_ELEM_4polyline->pts[655]=2.51904928e+00;   
           GEOM_ELEM_4polyline->pts[657]=4.54733906e+02; GEOM_ELEM_4polyline->pts[658]=2.51914782e+00;   
           GEOM_ELEM_4polyline->pts[660]=4.53545718e+02; GEOM_ELEM_4polyline->pts[661]=2.51924575e+00;   
           GEOM_ELEM_4polyline->pts[663]=4.52318954e+02; GEOM_ELEM_4polyline->pts[664]=2.51934318e+00;   
           GEOM_ELEM_4polyline->pts[666]=4.51051992e+02; GEOM_ELEM_4polyline->pts[667]=2.51944026e+00;   
           GEOM_ELEM_4polyline->pts[669]=4.49740914e+02; GEOM_ELEM_4polyline->pts[670]=2.51953722e+00;   
           GEOM_ELEM_4polyline->pts[672]=4.48376671e+02; GEOM_ELEM_4polyline->pts[673]=2.51963443e+00;   
           GEOM_ELEM_4polyline->pts[675]=4.46948117e+02; GEOM_ELEM_4polyline->pts[676]=2.51973212e+00;   
           GEOM_ELEM_4polyline->pts[678]=4.45454383e+02; GEOM_ELEM_4polyline->pts[679]=2.51982992e+00;   
           GEOM_ELEM_4polyline->pts[681]=4.43890265e+02; GEOM_ELEM_4polyline->pts[682]=2.51992786e+00;   
           GEOM_ELEM_4polyline->pts[684]=4.42253565e+02; GEOM_ELEM_4polyline->pts[685]=2.52002582e+00;   
           GEOM_ELEM_4polyline->pts[687]=4.40539121e+02; GEOM_ELEM_4polyline->pts[688]=2.52012392e+00;   
           GEOM_ELEM_4polyline->pts[690]=4.38740221e+02; GEOM_ELEM_4polyline->pts[691]=2.52022240e+00;   
           GEOM_ELEM_4polyline->pts[693]=4.36850466e+02; GEOM_ELEM_4polyline->pts[694]=2.52032150e+00;   
           GEOM_ELEM_4polyline->pts[696]=4.34864919e+02; GEOM_ELEM_4polyline->pts[697]=2.52042140e+00;   
           GEOM_ELEM_4polyline->pts[699]=4.32777593e+02; GEOM_ELEM_4polyline->pts[700]=2.52052226e+00;   
           GEOM_ELEM_4polyline->pts[702]=4.30572712e+02; GEOM_ELEM_4polyline->pts[703]=2.52062455e+00;   
           GEOM_ELEM_4polyline->pts[705]=4.28228272e+02; GEOM_ELEM_4polyline->pts[706]=2.52072876e+00;   
           GEOM_ELEM_4polyline->pts[708]=4.25749256e+02; GEOM_ELEM_4polyline->pts[709]=2.52083431e+00;   
           GEOM_ELEM_4polyline->pts[711]=4.23106587e+02; GEOM_ELEM_4polyline->pts[712]=2.52094222e+00;   
           GEOM_ELEM_4polyline->pts[714]=4.20240329e+02; GEOM_ELEM_4polyline->pts[715]=2.52105486e+00;   
           GEOM_ELEM_4polyline->pts[717]=4.17046117e+02; GEOM_ELEM_4polyline->pts[718]=2.52117632e+00;   
           GEOM_ELEM_4polyline->pts[720]=4.13332949e+02; GEOM_ELEM_4polyline->pts[721]=2.52131413e+00;   
           GEOM_ELEM_4polyline->pts[723]=4.09085017e+02; GEOM_ELEM_4polyline->pts[724]=2.52146981e+00;   
           GEOM_ELEM_4polyline->pts[726]=4.07748718e+02; GEOM_ELEM_4polyline->pts[727]=2.52151805e+00;   
           GEOM_ELEM_4polyline->pts[729]=4.06071859e+02; GEOM_ELEM_4polyline->pts[730]=2.52157741e+00;   
           GEOM_ELEM_4polyline->pts[732]=4.04471545e+02; GEOM_ELEM_4polyline->pts[733]=2.52163260e+00;   
           GEOM_ELEM_4polyline->pts[735]=4.02850379e+02; GEOM_ELEM_4polyline->pts[736]=2.52168701e+00;   
           GEOM_ELEM_4polyline->pts[738]=4.01213842e+02; GEOM_ELEM_4polyline->pts[739]=2.52174051e+00;   
           GEOM_ELEM_4polyline->pts[741]=3.99469796e+02; GEOM_ELEM_4polyline->pts[742]=2.52179624e+00;   
           GEOM_ELEM_4polyline->pts[744]=3.97472722e+02; GEOM_ELEM_4polyline->pts[745]=2.52185895e+00;   
           GEOM_ELEM_4polyline->pts[747]=3.94788470e+02; GEOM_ELEM_4polyline->pts[748]=2.52194272e+00;   
           GEOM_ELEM_4polyline->pts[750]=3.92601319e+02; GEOM_ELEM_4polyline->pts[751]=2.52201175e+00;   
           GEOM_ELEM_4polyline->pts[753]=3.90805389e+02; GEOM_ELEM_4polyline->pts[754]=2.52206969e+00;   
           GEOM_ELEM_4polyline->pts[756]=3.89155412e+02; GEOM_ELEM_4polyline->pts[757]=2.52212430e+00;   
           GEOM_ELEM_4polyline->pts[759]=3.87521583e+02; GEOM_ELEM_4polyline->pts[760]=2.52217984e+00;   
           GEOM_ELEM_4polyline->pts[762]=3.85943311e+02; GEOM_ELEM_4polyline->pts[763]=2.52223490e+00;   
           GEOM_ELEM_4polyline->pts[765]=3.84325389e+02; GEOM_ELEM_4polyline->pts[766]=2.52229262e+00;   
           GEOM_ELEM_4polyline->pts[768]=3.82528023e+02; GEOM_ELEM_4polyline->pts[769]=2.52235778e+00;   
           GEOM_ELEM_4polyline->pts[771]=3.79159843e+02; GEOM_ELEM_4polyline->pts[772]=2.52248046e+00;   
           GEOM_ELEM_4polyline->pts[774]=3.78814623e+02; GEOM_ELEM_4polyline->pts[775]=2.52601200e+00;   
           GEOM_ELEM_4polyline->pts[777]=3.78393506e+02; GEOM_ELEM_4polyline->pts[778]=2.53976162e+00;   
           GEOM_ELEM_4polyline->pts[780]=3.77973943e+02; GEOM_ELEM_4polyline->pts[781]=2.56385629e+00;   
           GEOM_ELEM_4polyline->pts[783]=3.77556161e+02; GEOM_ELEM_4polyline->pts[784]=2.59825722e+00;   
           GEOM_ELEM_4polyline->pts[786]=3.77140384e+02; GEOM_ELEM_4polyline->pts[787]=2.64292092e+00;   
           GEOM_ELEM_4polyline->pts[789]=3.76726846e+02; GEOM_ELEM_4polyline->pts[790]=2.69779828e+00;   
           GEOM_ELEM_4polyline->pts[792]=3.76315787e+02; GEOM_ELEM_4polyline->pts[793]=2.76283385e+00;   
           GEOM_ELEM_4polyline->pts[795]=3.75907456e+02; GEOM_ELEM_4polyline->pts[796]=2.83796524e+00;   
           GEOM_ELEM_4polyline->pts[798]=3.75502111e+02; GEOM_ELEM_4polyline->pts[799]=2.92312242e+00;   
           GEOM_ELEM_4polyline->pts[801]=3.75100014e+02; GEOM_ELEM_4polyline->pts[802]=3.01822717e+00;   
           GEOM_ELEM_4polyline->pts[804]=3.74701437e+02; GEOM_ELEM_4polyline->pts[805]=3.12319255e+00;   
           GEOM_ELEM_4polyline->pts[807]=3.74306654e+02; GEOM_ELEM_4polyline->pts[808]=3.23792250e+00;   
           GEOM_ELEM_4polyline->pts[810]=3.73915944e+02; GEOM_ELEM_4polyline->pts[811]=3.36231224e+00;   
           GEOM_ELEM_4polyline->pts[813]=3.73529598e+02; GEOM_ELEM_4polyline->pts[814]=3.49624413e+00;   
           GEOM_ELEM_4polyline->pts[816]=3.73147905e+02; GEOM_ELEM_4polyline->pts[817]=3.63959165e+00;   
           GEOM_ELEM_4polyline->pts[819]=3.72771156e+02; GEOM_ELEM_4polyline->pts[820]=3.79221694e+00;   
           GEOM_ELEM_4polyline->pts[822]=3.72399648e+02; GEOM_ELEM_4polyline->pts[823]=3.95397040e+00;   
           GEOM_ELEM_4polyline->pts[825]=3.72033678e+02; GEOM_ELEM_4polyline->pts[826]=4.12469069e+00;   
           GEOM_ELEM_4polyline->pts[828]=3.71673542e+02; GEOM_ELEM_4polyline->pts[829]=4.30420479e+00;   
           GEOM_ELEM_4polyline->pts[831]=3.71319538e+02; GEOM_ELEM_4polyline->pts[832]=4.49232825e+00;   
           GEOM_ELEM_4polyline->pts[834]=3.70971958e+02; GEOM_ELEM_4polyline->pts[835]=4.68886552e+00;   
           GEOM_ELEM_4polyline->pts[837]=3.70631090e+02; GEOM_ELEM_4polyline->pts[838]=4.89361044e+00;   
           GEOM_ELEM_4polyline->pts[840]=3.70297219e+02; GEOM_ELEM_4polyline->pts[841]=5.10634680e+00;   
           GEOM_ELEM_4polyline->pts[843]=3.69970620e+02; GEOM_ELEM_4polyline->pts[844]=5.32684909e+00;   
           GEOM_ELEM_4polyline->pts[846]=3.69651561e+02; GEOM_ELEM_4polyline->pts[847]=5.55488322e+00;   
           GEOM_ELEM_4polyline->pts[849]=3.69340300e+02; GEOM_ELEM_4polyline->pts[850]=5.79020746e+00;   
           GEOM_ELEM_4polyline->pts[852]=3.69037085e+02; GEOM_ELEM_4polyline->pts[853]=6.03257335e+00;   
           GEOM_ELEM_4polyline->pts[855]=3.68742154e+02; GEOM_ELEM_4polyline->pts[856]=6.28172670e+00;   
           GEOM_ELEM_4polyline->pts[858]=3.68455730e+02; GEOM_ELEM_4polyline->pts[859]=6.53740866e+00;   
           GEOM_ELEM_4polyline->pts[861]=3.68178025e+02; GEOM_ELEM_4polyline->pts[862]=6.79935677e+00;   
           GEOM_ELEM_4polyline->pts[864]=3.67909239e+02; GEOM_ELEM_4polyline->pts[865]=7.06730596e+00;   
           GEOM_ELEM_4polyline->pts[867]=3.67649565e+02; GEOM_ELEM_4polyline->pts[868]=7.34098136e+00;   
           GEOM_ELEM_4polyline->pts[870]=3.67399180e+02; GEOM_ELEM_4polyline->pts[871]=7.62010988e+00;   
           GEOM_ELEM_4polyline->pts[873]=3.67158244e+02; GEOM_ELEM_4polyline->pts[874]=7.90442015e+00;   
           GEOM_ELEM_4polyline->pts[876]=3.66926904e+02; GEOM_ELEM_4polyline->pts[877]=8.19364125e+00;   
           GEOM_ELEM_4polyline->pts[879]=3.66705293e+02; GEOM_ELEM_4polyline->pts[880]=8.48750456e+00;   
           GEOM_ELEM_4polyline->pts[882]=3.66493530e+02; GEOM_ELEM_4polyline->pts[883]=8.78574544e+00;   
           GEOM_ELEM_4polyline->pts[885]=3.66291719e+02; GEOM_ELEM_4polyline->pts[886]=9.08810468e+00;   
           GEOM_ELEM_4polyline->pts[888]=3.66099952e+02; GEOM_ELEM_4polyline->pts[889]=9.39432966e+00;   
           GEOM_ELEM_4polyline->pts[891]=3.65918305e+02; GEOM_ELEM_4polyline->pts[892]=9.70417534e+00;   
           GEOM_ELEM_4polyline->pts[894]=3.65746844e+02; GEOM_ELEM_4polyline->pts[895]=1.00174049e+01;   
           GEOM_ELEM_4polyline->pts[897]=3.65585623e+02; GEOM_ELEM_4polyline->pts[898]=1.03337905e+01;   
           GEOM_ELEM_4polyline->pts[900]=3.65434686e+02; GEOM_ELEM_4polyline->pts[901]=1.06531129e+01;   
           GEOM_ELEM_4polyline->pts[903]=3.65294068e+02; GEOM_ELEM_4polyline->pts[904]=1.09751618e+01;   
           GEOM_ELEM_4polyline->pts[906]=3.65163799e+02; GEOM_ELEM_4polyline->pts[907]=1.12997300e+01;   
           GEOM_ELEM_4polyline->pts[909]=3.65043901e+02; GEOM_ELEM_4polyline->pts[910]=1.16266214e+01;   
           GEOM_ELEM_4polyline->pts[912]=3.64934385e+02; GEOM_ELEM_4polyline->pts[913]=1.19556570e+01;   
           GEOM_ELEM_4polyline->pts[915]=3.64835259e+02; GEOM_ELEM_4polyline->pts[916]=1.22866661e+01;   
           GEOM_ELEM_4polyline->pts[918]=3.64746529e+02; GEOM_ELEM_4polyline->pts[919]=1.26194872e+01;   
           GEOM_ELEM_4polyline->pts[921]=3.64668195e+02; GEOM_ELEM_4polyline->pts[922]=1.29539681e+01;   
           GEOM_ELEM_4polyline->pts[924]=3.64600257e+02; GEOM_ELEM_4polyline->pts[925]=1.32899661e+01;   
           GEOM_ELEM_4polyline->pts[927]=3.64542713e+02; GEOM_ELEM_4polyline->pts[928]=1.36273480e+01;   
           GEOM_ELEM_4polyline->pts[930]=3.64495563e+02; GEOM_ELEM_4polyline->pts[931]=1.39659902e+01;   
           GEOM_ELEM_4polyline->pts[933]=3.64458804e+02; GEOM_ELEM_4polyline->pts[934]=1.43057779e+01;   
           GEOM_ELEM_4polyline->pts[936]=3.64432438e+02; GEOM_ELEM_4polyline->pts[937]=1.46466054e+01;   
           GEOM_ELEM_4polyline->pts[939]=3.64416468e+02; GEOM_ELEM_4polyline->pts[940]=1.49883749e+01;   
           GEOM_ELEM_4polyline->pts[942]=3.64411536e+02; GEOM_ELEM_4polyline->pts[943]=1.52224784e+01;   
           GEOM_ELEM_4polyline->pts[945]=3.64094589e+02; GEOM_ELEM_4polyline->pts[946]=4.54508853e+01;   
           GEOM_ELEM_4polyline->pts[948]=3.64094660e+02; GEOM_ELEM_4polyline->pts[949]=4.54508853e+01;   
           GEOM_ELEM_4polyline->pts[951]=3.63677234e+02; GEOM_ELEM_4polyline->pts[952]=8.53320762e+01;   
           GEOM_ELEM_4polyline->pts[954]=3.83802230e+02; GEOM_ELEM_4polyline->pts[955]=8.53313428e+01;   
           GEOM_ELEM_4polyline->pts[957]=3.84125853e+02; GEOM_ELEM_4polyline->pts[958]=8.53352638e+01;   
           GEOM_ELEM_4polyline->pts[960]=3.84492367e+02; GEOM_ELEM_4polyline->pts[961]=8.53494530e+01;   
           GEOM_ELEM_4polyline->pts[963]=3.84860050e+02; GEOM_ELEM_4polyline->pts[964]=8.53740239e+01;   
           GEOM_ELEM_4polyline->pts[966]=3.85229043e+02; GEOM_ELEM_4polyline->pts[967]=8.54090261e+01;   
           GEOM_ELEM_4polyline->pts[969]=3.85599352e+02; GEOM_ELEM_4polyline->pts[970]=8.54545147e+01;   
           GEOM_ELEM_4polyline->pts[972]=3.85970925e+02; GEOM_ELEM_4polyline->pts[973]=8.55105481e+01;   
           GEOM_ELEM_4polyline->pts[975]=3.86343652e+02; GEOM_ELEM_4polyline->pts[976]=8.55771821e+01;   
           GEOM_ELEM_4polyline->pts[978]=3.86717363e+02; GEOM_ELEM_4polyline->pts[979]=8.56544655e+01;   
           GEOM_ELEM_4polyline->pts[981]=3.87091823e+02; GEOM_ELEM_4polyline->pts[982]=8.57424341e+01;   
           GEOM_ELEM_4polyline->pts[984]=3.87466737e+02; GEOM_ELEM_4polyline->pts[985]=8.58411056e+01;   
           GEOM_ELEM_4polyline->pts[987]=3.87841741e+02; GEOM_ELEM_4polyline->pts[988]=8.59504742e+01;   
           GEOM_ELEM_4polyline->pts[990]=3.88216412e+02; GEOM_ELEM_4polyline->pts[991]=8.60705051e+01;   
           GEOM_ELEM_4polyline->pts[993]=3.88590263e+02; GEOM_ELEM_4polyline->pts[994]=8.62011308e+01;   
           GEOM_ELEM_4polyline->pts[996]=3.88962755e+02; GEOM_ELEM_4polyline->pts[997]=8.63422465e+01;   
           GEOM_ELEM_4polyline->pts[999]=3.89333299e+02; GEOM_ELEM_4polyline->pts[1000]=8.64937085e+01;   
           GEOM_ELEM_4polyline->pts[1002]=3.89701258e+02; GEOM_ELEM_4polyline->pts[1003]=8.66553275e+01;   
           GEOM_ELEM_4polyline->pts[1005]=3.90065960e+02; GEOM_ELEM_4polyline->pts[1006]=8.68268697e+01;   
           GEOM_ELEM_4polyline->pts[1008]=3.90426717e+02; GEOM_ELEM_4polyline->pts[1009]=8.70080615e+01;   
           GEOM_ELEM_4polyline->pts[1011]=3.90782843e+02; GEOM_ELEM_4polyline->pts[1012]=8.71985950e+01;   
           GEOM_ELEM_4polyline->pts[1014]=3.91133668e+02; GEOM_ELEM_4polyline->pts[1015]=8.73981330e+01;   
           GEOM_ELEM_4polyline->pts[1017]=3.91478546e+02; GEOM_ELEM_4polyline->pts[1018]=8.76063164e+01;   
           GEOM_ELEM_4polyline->pts[1020]=3.91816871e+02; GEOM_ELEM_4polyline->pts[1021]=8.78227709e+01;   
           GEOM_ELEM_4polyline->pts[1023]=3.92148084e+02; GEOM_ELEM_4polyline->pts[1024]=8.80471153e+01;   
           GEOM_ELEM_4polyline->pts[1026]=3.92471677e+02; GEOM_ELEM_4polyline->pts[1027]=8.82789685e+01;   
           GEOM_ELEM_4polyline->pts[1029]=3.92787204e+02; GEOM_ELEM_4polyline->pts[1030]=8.85179571e+01;   
           GEOM_ELEM_4polyline->pts[1032]=3.93094275e+02; GEOM_ELEM_4polyline->pts[1033]=8.87637214e+01;   
           GEOM_ELEM_4polyline->pts[1035]=3.93392564e+02; GEOM_ELEM_4polyline->pts[1036]=8.90159215e+01;   
           GEOM_ELEM_4polyline->pts[1038]=3.93429928e+02; GEOM_ELEM_4polyline->pts[1039]=8.90485033e+01;   
           GEOM_ELEM_4polyline->pts[1041]=3.94765390e+02; GEOM_ELEM_4polyline->pts[1042]=9.02115621e+01;   
           GEOM_ELEM_4polyline->pts[1044]=3.97675463e+02; GEOM_ELEM_4polyline->pts[1045]=9.27256168e+01;   
           GEOM_ELEM_4polyline->pts[1047]=3.99197974e+02; GEOM_ELEM_4polyline->pts[1048]=9.40481622e+01;   
           GEOM_ELEM_4polyline->pts[1050]=4.00408185e+02; GEOM_ELEM_4polyline->pts[1051]=9.51114538e+01;   
           GEOM_ELEM_4polyline->pts[1053]=4.01495870e+02; GEOM_ELEM_4polyline->pts[1054]=9.60800507e+01;   
           GEOM_ELEM_4polyline->pts[1056]=4.02510625e+02; GEOM_ELEM_4polyline->pts[1057]=9.69971752e+01;   
           GEOM_ELEM_4polyline->pts[1059]=4.03517097e+02; GEOM_ELEM_4polyline->pts[1060]=9.79213356e+01;   
           GEOM_ELEM_4polyline->pts[1062]=4.04004171e+02; GEOM_ELEM_4polyline->pts[1063]=9.83741161e+01;   
           GEOM_ELEM_4polyline->pts[1065]=4.04241360e+02; GEOM_ELEM_4polyline->pts[1066]=9.85906663e+01;   
           GEOM_ELEM_4polyline->pts[1068]=4.04531331e+02; GEOM_ELEM_4polyline->pts[1069]=9.88425466e+01;   
           GEOM_ELEM_4polyline->pts[1071]=4.04828558e+02; GEOM_ELEM_4polyline->pts[1072]=9.90870399e+01;   
           GEOM_ELEM_4polyline->pts[1074]=4.05132742e+02; GEOM_ELEM_4polyline->pts[1075]=9.93238624e+01;   
           GEOM_ELEM_4polyline->pts[1077]=4.05443453e+02; GEOM_ELEM_4polyline->pts[1078]=9.95526422e+01;   
           GEOM_ELEM_4polyline->pts[1080]=4.05760228e+02; GEOM_ELEM_4polyline->pts[1081]=9.97730143e+01;   
           GEOM_ELEM_4polyline->pts[1083]=4.06082585e+02; GEOM_ELEM_4polyline->pts[1084]=9.99846269e+01;   
           GEOM_ELEM_4polyline->pts[1086]=4.06410025e+02; GEOM_ELEM_4polyline->pts[1087]=1.00187148e+02;   
           GEOM_ELEM_4polyline->pts[1089]=4.06742046e+02; GEOM_ELEM_4polyline->pts[1090]=1.00380272e+02;   
           GEOM_ELEM_4polyline->pts[1092]=4.07078149e+02; GEOM_ELEM_4polyline->pts[1093]=1.00563724e+02;   
           GEOM_ELEM_4polyline->pts[1095]=4.07417846e+02; GEOM_ELEM_4polyline->pts[1096]=1.00737262e+02;   
           GEOM_ELEM_4polyline->pts[1098]=4.07760673e+02; GEOM_ELEM_4polyline->pts[1099]=1.00900684e+02;   
           GEOM_ELEM_4polyline->pts[1101]=4.08106199e+02; GEOM_ELEM_4polyline->pts[1102]=1.01053823e+02;   
           GEOM_ELEM_4polyline->pts[1104]=4.08454027e+02; GEOM_ELEM_4polyline->pts[1105]=1.01196551e+02;   
           GEOM_ELEM_4polyline->pts[1107]=4.08803801e+02; GEOM_ELEM_4polyline->pts[1108]=1.01328772e+02;   
           GEOM_ELEM_4polyline->pts[1110]=4.09155201e+02; GEOM_ELEM_4polyline->pts[1111]=1.01450424e+02;   
           GEOM_ELEM_4polyline->pts[1113]=4.09507972e+02; GEOM_ELEM_4polyline->pts[1114]=1.01561477e+02;   
           GEOM_ELEM_4polyline->pts[1116]=4.09861919e+02; GEOM_ELEM_4polyline->pts[1117]=1.01661928e+02;   
           GEOM_ELEM_4polyline->pts[1119]=4.10216908e+02; GEOM_ELEM_4polyline->pts[1120]=1.01751798e+02;   
           GEOM_ELEM_4polyline->pts[1122]=4.10572904e+02; GEOM_ELEM_4polyline->pts[1123]=1.01831129e+02;   
           GEOM_ELEM_4polyline->pts[1125]=4.10929837e+02; GEOM_ELEM_4polyline->pts[1126]=1.01899958e+02;   
           GEOM_ELEM_4polyline->pts[1128]=4.11287749e+02; GEOM_ELEM_4polyline->pts[1129]=1.01958347e+02;   
           GEOM_ELEM_4polyline->pts[1131]=4.11647037e+02; GEOM_ELEM_4polyline->pts[1132]=1.02006410e+02;   
           GEOM_ELEM_4polyline->pts[1134]=4.12007822e+02; GEOM_ELEM_4polyline->pts[1135]=1.02044167e+02;   
           GEOM_ELEM_4polyline->pts[1137]=4.12369799e+02; GEOM_ELEM_4polyline->pts[1138]=1.02071580e+02;   
           GEOM_ELEM_4polyline->pts[1140]=4.12732726e+02; GEOM_ELEM_4polyline->pts[1141]=1.02088620e+02;   
           GEOM_ELEM_4polyline->pts[1143]=4.13096359e+02; GEOM_ELEM_4polyline->pts[1144]=1.02095268e+02;   
           GEOM_ELEM_4polyline->pts[1146]=4.13143975e+02; GEOM_ELEM_4polyline->pts[1147]=1.02095368e+02;   
           GEOM_ELEM_4polyline->pts[1149]=4.17721629e+02; GEOM_ELEM_4polyline->pts[1150]=1.02095197e+02;   
           GEOM_ELEM_4polyline->pts[1152]=4.21161713e+02; GEOM_ELEM_4polyline->pts[1153]=1.02095065e+02;   
           GEOM_ELEM_4polyline->pts[1155]=4.24193988e+02; GEOM_ELEM_4polyline->pts[1156]=1.02094944e+02;   
           GEOM_ELEM_4polyline->pts[1158]=4.26950081e+02; GEOM_ELEM_4polyline->pts[1159]=1.02094829e+02;   
           GEOM_ELEM_4polyline->pts[1161]=4.29504913e+02; GEOM_ELEM_4polyline->pts[1162]=1.02094718e+02;   
           GEOM_ELEM_4polyline->pts[1164]=4.31899564e+02; GEOM_ELEM_4polyline->pts[1165]=1.02094608e+02;   
           GEOM_ELEM_4polyline->pts[1167]=4.34158307e+02; GEOM_ELEM_4polyline->pts[1168]=1.02094501e+02;   
           GEOM_ELEM_4polyline->pts[1170]=4.36287125e+02; GEOM_ELEM_4polyline->pts[1171]=1.02094395e+02;   
           GEOM_ELEM_4polyline->pts[1173]=4.38296995e+02; GEOM_ELEM_4polyline->pts[1174]=1.02094290e+02;   
           GEOM_ELEM_4polyline->pts[1176]=4.40200138e+02; GEOM_ELEM_4polyline->pts[1177]=1.02094187e+02;   
           GEOM_ELEM_4polyline->pts[1179]=4.42007496e+02; GEOM_ELEM_4polyline->pts[1180]=1.02094084e+02;   
           GEOM_ELEM_4polyline->pts[1182]=4.43727527e+02; GEOM_ELEM_4polyline->pts[1183]=1.02093982e+02;   
           GEOM_ELEM_4polyline->pts[1185]=4.45362875e+02; GEOM_ELEM_4polyline->pts[1186]=1.02093880e+02;   
           GEOM_ELEM_4polyline->pts[1188]=4.46998609e+02; GEOM_ELEM_4polyline->pts[1189]=1.02093773e+02;   
           GEOM_ELEM_4polyline->pts[1191]=4.48482136e+02; GEOM_ELEM_4polyline->pts[1192]=1.02093671e+02;   
           GEOM_ELEM_4polyline->pts[1194]=4.49967863e+02; GEOM_ELEM_4polyline->pts[1195]=1.02093564e+02;   
           GEOM_ELEM_4polyline->pts[1197]=4.51319169e+02; GEOM_ELEM_4polyline->pts[1198]=1.02093464e+02;   
           GEOM_ELEM_4polyline->pts[1200]=4.52674751e+02; GEOM_ELEM_4polyline->pts[1201]=1.02093359e+02;   
           GEOM_ELEM_4polyline->pts[1203]=4.53976186e+02; GEOM_ELEM_4polyline->pts[1204]=1.02093254e+02;   
           GEOM_ELEM_4polyline->pts[1206]=4.55230240e+02; GEOM_ELEM_4polyline->pts[1207]=1.02093149e+02;   
           GEOM_ELEM_4polyline->pts[1209]=4.56440013e+02; GEOM_ELEM_4polyline->pts[1210]=1.02093044e+02;   
           GEOM_ELEM_4polyline->pts[1212]=4.57607060e+02; GEOM_ELEM_4polyline->pts[1213]=1.02092938e+02;   
           GEOM_ELEM_4polyline->pts[1215]=4.58733152e+02; GEOM_ELEM_4polyline->pts[1216]=1.02092831e+02;   
           GEOM_ELEM_4polyline->pts[1218]=4.59820142e+02; GEOM_ELEM_4polyline->pts[1219]=1.02092724e+02;   
           GEOM_ELEM_4polyline->pts[1221]=4.60868481e+02; GEOM_ELEM_4polyline->pts[1222]=1.02092617e+02;   
           GEOM_ELEM_4polyline->pts[1224]=4.61824892e+02; GEOM_ELEM_4polyline->pts[1225]=1.02092516e+02;   
           GEOM_ELEM_4polyline->pts[1227]=4.62777805e+02; GEOM_ELEM_4polyline->pts[1228]=1.02092411e+02;   
           GEOM_ELEM_4polyline->pts[1230]=4.63687859e+02; GEOM_ELEM_4polyline->pts[1231]=1.02092307e+02;   
           GEOM_ELEM_4polyline->pts[1233]=4.64593758e+02; GEOM_ELEM_4polyline->pts[1234]=1.02092200e+02;   
           GEOM_ELEM_4polyline->pts[1236]=4.65511857e+02; GEOM_ELEM_4polyline->pts[1237]=1.02092089e+02;   
           GEOM_ELEM_4polyline->pts[1239]=4.66434896e+02; GEOM_ELEM_4polyline->pts[1240]=1.02091975e+02;   
           GEOM_ELEM_4polyline->pts[1242]=4.67344291e+02; GEOM_ELEM_4polyline->pts[1243]=1.02091862e+02;   
           GEOM_ELEM_4polyline->pts[1245]=4.68233739e+02; GEOM_ELEM_4polyline->pts[1246]=1.02091751e+02;   
           GEOM_ELEM_4polyline->pts[1248]=4.69117282e+02; GEOM_ELEM_4polyline->pts[1249]=1.02091640e+02;   
           GEOM_ELEM_4polyline->pts[1251]=4.70023024e+02; GEOM_ELEM_4polyline->pts[1252]=1.02091526e+02;   
           GEOM_ELEM_4polyline->pts[1254]=4.70960431e+02; GEOM_ELEM_4polyline->pts[1255]=1.02091405e+02;   
           GEOM_ELEM_4polyline->pts[1257]=4.71920799e+02; GEOM_ELEM_4polyline->pts[1258]=1.02091281e+02;   
           GEOM_ELEM_4polyline->pts[1260]=4.72902844e+02; GEOM_ELEM_4polyline->pts[1261]=1.02091152e+02;   
           GEOM_ELEM_4polyline->pts[1263]=4.73902409e+02; GEOM_ELEM_4polyline->pts[1264]=1.02091021e+02;   
           GEOM_ELEM_4polyline->pts[1266]=4.74914055e+02; GEOM_ELEM_4polyline->pts[1267]=1.02090889e+02;   
           GEOM_ELEM_4polyline->pts[1269]=4.75939094e+02; GEOM_ELEM_4polyline->pts[1270]=1.02090754e+02;   
           GEOM_ELEM_4polyline->pts[1272]=4.76993099e+02; GEOM_ELEM_4polyline->pts[1273]=1.02090617e+02;   
           GEOM_ELEM_4polyline->pts[1275]=4.78097478e+02; GEOM_ELEM_4polyline->pts[1276]=1.02090472e+02;   
           GEOM_ELEM_4polyline->pts[1278]=4.79246320e+02; GEOM_ELEM_4polyline->pts[1279]=1.02090321e+02;   
           GEOM_ELEM_4polyline->pts[1281]=4.80444009e+02; GEOM_ELEM_4polyline->pts[1282]=1.02090164e+02;   
           GEOM_ELEM_4polyline->pts[1284]=4.81689118e+02; GEOM_ELEM_4polyline->pts[1285]=1.02090000e+02;   
           GEOM_ELEM_4polyline->pts[1287]=4.82964263e+02; GEOM_ELEM_4polyline->pts[1288]=1.02089835e+02;   
           GEOM_ELEM_4polyline->pts[1290]=4.84246335e+02; GEOM_ELEM_4polyline->pts[1291]=1.02089670e+02;   
           GEOM_ELEM_4polyline->pts[1293]=4.85529868e+02; GEOM_ELEM_4polyline->pts[1294]=1.02089507e+02;   
           GEOM_ELEM_4polyline->pts[1296]=4.86827238e+02; GEOM_ELEM_4polyline->pts[1297]=1.02089343e+02;   
           GEOM_ELEM_4polyline->pts[1299]=4.88135118e+02; GEOM_ELEM_4polyline->pts[1300]=1.02089179e+02;   
           GEOM_ELEM_4polyline->pts[1302]=4.89454394e+02; GEOM_ELEM_4polyline->pts[1303]=1.02089016e+02;   
           GEOM_ELEM_4polyline->pts[1305]=4.90787576e+02; GEOM_ELEM_4polyline->pts[1306]=1.02088852e+02;   
           GEOM_ELEM_4polyline->pts[1308]=4.92136890e+02; GEOM_ELEM_4polyline->pts[1309]=1.02088688e+02;   
           GEOM_ELEM_4polyline->pts[1311]=4.93504301e+02; GEOM_ELEM_4polyline->pts[1312]=1.02088524e+02;   
           GEOM_ELEM_4polyline->pts[1314]=4.94891540e+02; GEOM_ELEM_4polyline->pts[1315]=1.02088359e+02;   
           GEOM_ELEM_4polyline->pts[1317]=4.96300161e+02; GEOM_ELEM_4polyline->pts[1318]=1.02088194e+02;   
           GEOM_ELEM_4polyline->pts[1320]=4.97731638e+02; GEOM_ELEM_4polyline->pts[1321]=1.02088029e+02;   
           GEOM_ELEM_4polyline->pts[1323]=4.99187525e+02; GEOM_ELEM_4polyline->pts[1324]=1.02087864e+02;   
           GEOM_ELEM_4polyline->pts[1326]=5.00669671e+02; GEOM_ELEM_4polyline->pts[1327]=1.02087698e+02;   
           GEOM_ELEM_4polyline->pts[1329]=5.02180156e+02; GEOM_ELEM_4polyline->pts[1330]=1.02087532e+02;   
           GEOM_ELEM_4polyline->pts[1332]=5.03722394e+02; GEOM_ELEM_4polyline->pts[1333]=1.02087366e+02;   
           GEOM_ELEM_4polyline->pts[1335]=5.05299155e+02; GEOM_ELEM_4polyline->pts[1336]=1.02087198e+02;   
           GEOM_ELEM_4polyline->pts[1338]=5.06906850e+02; GEOM_ELEM_4polyline->pts[1339]=1.02087031e+02;   
           GEOM_ELEM_4polyline->pts[1341]=5.08547179e+02; GEOM_ELEM_4polyline->pts[1342]=1.02086862e+02;   
           GEOM_ELEM_4polyline->pts[1344]=5.10222130e+02; GEOM_ELEM_4polyline->pts[1345]=1.02086694e+02;   
           GEOM_ELEM_4polyline->pts[1347]=5.11933756e+02; GEOM_ELEM_4polyline->pts[1348]=1.02086525e+02;   
           GEOM_ELEM_4polyline->pts[1350]=5.13684163e+02; GEOM_ELEM_4polyline->pts[1351]=1.02086355e+02;   
           GEOM_ELEM_4polyline->pts[1353]=5.15475494e+02; GEOM_ELEM_4polyline->pts[1354]=1.02086185e+02;   
           GEOM_ELEM_4polyline->pts[1356]=5.17309911e+02; GEOM_ELEM_4polyline->pts[1357]=1.02086015e+02;   
           GEOM_ELEM_4polyline->pts[1359]=5.19189580e+02; GEOM_ELEM_4polyline->pts[1360]=1.02085843e+02;   
           GEOM_ELEM_4polyline->pts[1362]=5.21116670e+02; GEOM_ELEM_4polyline->pts[1363]=1.02085671e+02;   
           GEOM_ELEM_4polyline->pts[1365]=5.23093354e+02; GEOM_ELEM_4polyline->pts[1366]=1.02085499e+02;   
           GEOM_ELEM_4polyline->pts[1368]=5.25121844e+02; GEOM_ELEM_4polyline->pts[1369]=1.02085326e+02;   
           GEOM_ELEM_4polyline->pts[1371]=5.27204432e+02; GEOM_ELEM_4polyline->pts[1372]=1.02085152e+02;   
           GEOM_ELEM_4polyline->pts[1374]=5.29343555e+02; GEOM_ELEM_4polyline->pts[1375]=1.02084977e+02;   
           GEOM_ELEM_4polyline->pts[1377]=5.31541703e+02; GEOM_ELEM_4polyline->pts[1378]=1.02084801e+02;   
           GEOM_ELEM_4polyline->pts[1380]=5.33801092e+02; GEOM_ELEM_4polyline->pts[1381]=1.02084625e+02;   
           GEOM_ELEM_4polyline->pts[1383]=5.36126062e+02; GEOM_ELEM_4polyline->pts[1384]=1.02084447e+02;   
           GEOM_ELEM_4polyline->pts[1386]=5.38520142e+02; GEOM_ELEM_4polyline->pts[1387]=1.02084269e+02;   
           GEOM_ELEM_4polyline->pts[1389]=5.40986060e+02; GEOM_ELEM_4polyline->pts[1390]=1.02084089e+02;   
           GEOM_ELEM_4polyline->pts[1392]=5.43526915e+02; GEOM_ELEM_4polyline->pts[1393]=1.02083908e+02;   
           GEOM_ELEM_4polyline->pts[1395]=5.46146181e+02; GEOM_ELEM_4polyline->pts[1396]=1.02083726e+02;   
           GEOM_ELEM_4polyline->pts[1398]=5.48847713e+02; GEOM_ELEM_4polyline->pts[1399]=1.02083542e+02;   
           GEOM_ELEM_4polyline->pts[1401]=5.51635779e+02; GEOM_ELEM_4polyline->pts[1402]=1.02083356e+02;   
           GEOM_ELEM_4polyline->pts[1404]=5.54515111e+02; GEOM_ELEM_4polyline->pts[1405]=1.02083169e+02;   
           GEOM_ELEM_4polyline->pts[1407]=5.57491011e+02; GEOM_ELEM_4polyline->pts[1408]=1.02082979e+02;   
           GEOM_ELEM_4polyline->pts[1410]=5.60569491e+02; GEOM_ELEM_4polyline->pts[1411]=1.02082788e+02;   
           GEOM_ELEM_4polyline->pts[1413]=5.63757254e+02; GEOM_ELEM_4polyline->pts[1414]=1.02082594e+02;   
           GEOM_ELEM_4polyline->pts[1416]=5.67061127e+02; GEOM_ELEM_4polyline->pts[1417]=1.02082397e+02;   
           GEOM_ELEM_4polyline->pts[1419]=5.70487849e+02; GEOM_ELEM_4polyline->pts[1420]=1.02082197e+02;   
           GEOM_ELEM_4polyline->pts[1422]=5.74040454e+02; GEOM_ELEM_4polyline->pts[1423]=1.02081994e+02;   
           GEOM_ELEM_4polyline->pts[1425]=5.77717265e+02; GEOM_ELEM_4polyline->pts[1426]=1.02081789e+02;   
           GEOM_ELEM_4polyline->pts[1428]=5.81508254e+02; GEOM_ELEM_4polyline->pts[1429]=1.02081582e+02;   
           GEOM_ELEM_4polyline->pts[1431]=5.81769899e+02; GEOM_ELEM_4polyline->pts[1432]=1.02081568e+02;   
           GEOM_ELEM_4polyline->pts[1434]=5.82088131e+02; GEOM_ELEM_4polyline->pts[1435]=1.02077680e+02;   
           GEOM_ELEM_4polyline->pts[1437]=5.82459295e+02; GEOM_ELEM_4polyline->pts[1438]=1.02063506e+02;   
           GEOM_ELEM_4polyline->pts[1440]=5.82829561e+02; GEOM_ELEM_4polyline->pts[1441]=1.02038974e+02;   
           GEOM_ELEM_4polyline->pts[1443]=5.83198674e+02; GEOM_ELEM_4polyline->pts[1444]=1.02004109e+02;   
           GEOM_ELEM_4polyline->pts[1446]=5.83566362e+02; GEOM_ELEM_4polyline->pts[1447]=1.01958944e+02;   
           GEOM_ELEM_4polyline->pts[1449]=5.83932350e+02; GEOM_ELEM_4polyline->pts[1450]=1.01903519e+02;   
           GEOM_ELEM_4polyline->pts[1452]=5.84296357e+02; GEOM_ELEM_4polyline->pts[1453]=1.01837884e+02;   
           GEOM_ELEM_4polyline->pts[1455]=5.84658098e+02; GEOM_ELEM_4polyline->pts[1456]=1.01762095e+02;   
           GEOM_ELEM_4polyline->pts[1458]=5.85017287e+02; GEOM_ELEM_4polyline->pts[1459]=1.01676219e+02;   
           GEOM_ELEM_4polyline->pts[1461]=5.85373637e+02; GEOM_ELEM_4polyline->pts[1462]=1.01580330e+02;   
           GEOM_ELEM_4polyline->pts[1464]=5.85726857e+02; GEOM_ELEM_4polyline->pts[1465]=1.01474511e+02;   
           GEOM_ELEM_4polyline->pts[1467]=5.86076663e+02; GEOM_ELEM_4polyline->pts[1468]=1.01358855e+02;   
           GEOM_ELEM_4polyline->pts[1470]=5.86422765e+02; GEOM_ELEM_4polyline->pts[1471]=1.01233463e+02;   
           GEOM_ELEM_4polyline->pts[1473]=5.86764874e+02; GEOM_ELEM_4polyline->pts[1474]=1.01098446e+02;   
           GEOM_ELEM_4polyline->pts[1476]=5.87102710e+02; GEOM_ELEM_4polyline->pts[1477]=1.00953921e+02;   
           GEOM_ELEM_4polyline->pts[1479]=5.87435994e+02; GEOM_ELEM_4polyline->pts[1480]=1.00800017e+02;   
           GEOM_ELEM_4polyline->pts[1482]=5.87764453e+02; GEOM_ELEM_4polyline->pts[1483]=1.00636868e+02;   
           GEOM_ELEM_4polyline->pts[1485]=5.88087815e+02; GEOM_ELEM_4polyline->pts[1486]=1.00464616e+02;   
           GEOM_ELEM_4polyline->pts[1488]=5.88405817e+02; GEOM_ELEM_4polyline->pts[1489]=1.00283411e+02;   
           GEOM_ELEM_4polyline->pts[1491]=5.88718204e+02; GEOM_ELEM_4polyline->pts[1492]=1.00093410e+02;   
           GEOM_ELEM_4polyline->pts[1494]=5.89024727e+02; GEOM_ELEM_4polyline->pts[1495]=9.98947731e+01;   
           GEOM_ELEM_4polyline->pts[1497]=5.89325146e+02; GEOM_ELEM_4polyline->pts[1498]=9.96876665e+01;   
           GEOM_ELEM_4polyline->pts[1500]=5.89619233e+02; GEOM_ELEM_4polyline->pts[1501]=9.94722592e+01;   
           GEOM_ELEM_4polyline->pts[1503]=5.89906766e+02; GEOM_ELEM_4polyline->pts[1504]=9.92487233e+01;   
           GEOM_ELEM_4polyline->pts[1506]=5.90187536e+02; GEOM_ELEM_4polyline->pts[1507]=9.90172325e+01;   
           GEOM_ELEM_4polyline->pts[1509]=5.90461345e+02; GEOM_ELEM_4polyline->pts[1510]=9.87779612e+01;   
           GEOM_ELEM_4polyline->pts[1512]=5.90728003e+02; GEOM_ELEM_4polyline->pts[1513]=9.85310839e+01;   
           GEOM_ELEM_4polyline->pts[1515]=5.90904194e+02; GEOM_ELEM_4polyline->pts[1516]=9.83599022e+01;   
           GEOM_ELEM_4polyline->pts[1518]=5.94817195e+02; GEOM_ELEM_4polyline->pts[1519]=9.44815362e+01;   
           GEOM_ELEM_4polyline->pts[1521]=5.98721106e+02; GEOM_ELEM_4polyline->pts[1522]=9.06029795e+01;   
           GEOM_ELEM_4polyline->pts[1524]=6.02660612e+02; GEOM_ELEM_4polyline->pts[1525]=8.66780813e+01;   
           GEOM_ELEM_4polyline->pts[1527]=6.06651962e+02; GEOM_ELEM_4polyline->pts[1528]=8.26901378e+01;   
           GEOM_ELEM_4polyline->pts[1530]=6.09829762e+02; GEOM_ELEM_4polyline->pts[1531]=7.95075473e+01;   
           GEOM_ELEM_4polyline->pts[1533]=6.10054238e+02; GEOM_ELEM_4polyline->pts[1534]=7.92881706e+01;   
           GEOM_ELEM_4polyline->pts[1536]=6.10319740e+02; GEOM_ELEM_4polyline->pts[1537]=7.90422381e+01;   
           GEOM_ELEM_4polyline->pts[1539]=6.10592162e+02; GEOM_ELEM_4polyline->pts[1540]=7.88040313e+01;   
           GEOM_ELEM_4polyline->pts[1542]=6.10871333e+02; GEOM_ELEM_4polyline->pts[1543]=7.85736987e+01;   
           GEOM_ELEM_4polyline->pts[1545]=6.11157051e+02; GEOM_ELEM_4polyline->pts[1546]=7.83514111e+01;   
           GEOM_ELEM_4polyline->pts[1548]=6.11449104e+02; GEOM_ELEM_4polyline->pts[1549]=7.81373383e+01;   
           GEOM_ELEM_4polyline->pts[1551]=6.11747268e+02; GEOM_ELEM_4polyline->pts[1552]=7.79316491e+01;   
           GEOM_ELEM_4polyline->pts[1554]=6.12051314e+02; GEOM_ELEM_4polyline->pts[1555]=7.77345096e+01;   
           GEOM_ELEM_4polyline->pts[1557]=6.12360999e+02; GEOM_ELEM_4polyline->pts[1558]=7.75460826e+01;   
           GEOM_ELEM_4polyline->pts[1560]=6.12676075e+02; GEOM_ELEM_4polyline->pts[1561]=7.73665269e+01;   
           GEOM_ELEM_4polyline->pts[1563]=6.12996286e+02; GEOM_ELEM_4polyline->pts[1564]=7.71959959e+01;   
           GEOM_ELEM_4polyline->pts[1566]=6.13321368e+02; GEOM_ELEM_4polyline->pts[1567]=7.70346374e+01;   
           GEOM_ELEM_4polyline->pts[1569]=6.13651051e+02; GEOM_ELEM_4polyline->pts[1570]=7.68825922e+01;   
           GEOM_ELEM_4polyline->pts[1572]=6.13985062e+02; GEOM_ELEM_4polyline->pts[1573]=7.67399937e+01;   
           GEOM_ELEM_4polyline->pts[1575]=6.14323121e+02; GEOM_ELEM_4polyline->pts[1576]=7.66069672e+01;   
           GEOM_ELEM_4polyline->pts[1578]=6.14664949e+02; GEOM_ELEM_4polyline->pts[1579]=7.64836295e+01;   
           GEOM_ELEM_4polyline->pts[1581]=6.15010262e+02; GEOM_ELEM_4polyline->pts[1582]=7.63700883e+01;   
           GEOM_ELEM_4polyline->pts[1584]=6.15358779e+02; GEOM_ELEM_4polyline->pts[1585]=7.62664430e+01;   
           GEOM_ELEM_4polyline->pts[1587]=6.15710203e+02; GEOM_ELEM_4polyline->pts[1588]=7.61727869e+01;   
           GEOM_ELEM_4polyline->pts[1590]=6.16064244e+02; GEOM_ELEM_4polyline->pts[1591]=7.60892025e+01;   
           GEOM_ELEM_4polyline->pts[1593]=6.16420615e+02; GEOM_ELEM_4polyline->pts[1594]=7.60157625e+01;   
           GEOM_ELEM_4polyline->pts[1596]=6.16779029e+02; GEOM_ELEM_4polyline->pts[1597]=7.59525305e+01;   
           GEOM_ELEM_4polyline->pts[1599]=6.17139203e+02; GEOM_ELEM_4polyline->pts[1600]=7.58995611e+01;   
           GEOM_ELEM_4polyline->pts[1602]=6.17500855e+02; GEOM_ELEM_4polyline->pts[1603]=7.58569001e+01;   
           GEOM_ELEM_4polyline->pts[1605]=6.17863711e+02; GEOM_ELEM_4polyline->pts[1606]=7.58245848e+01;   
           GEOM_ELEM_4polyline->pts[1608]=6.18227499e+02; GEOM_ELEM_4polyline->pts[1609]=7.58026444e+01;   
           GEOM_ELEM_4polyline->pts[1611]=6.18591958e+02; GEOM_ELEM_4polyline->pts[1612]=7.57911003e+01;   
           GEOM_ELEM_4polyline->pts[1614]=6.18817415e+02; GEOM_ELEM_4polyline->pts[1615]=7.57891704e+01;   
           GEOM_ELEM_4polyline->pts[1617]=6.23281703e+02; GEOM_ELEM_4polyline->pts[1618]=7.57889791e+01;   
           GEOM_ELEM_4polyline->pts[1620]=6.27729117e+02; GEOM_ELEM_4polyline->pts[1621]=7.57887936e+01;   
           GEOM_ELEM_4polyline->pts[1623]=6.32261335e+02; GEOM_ELEM_4polyline->pts[1624]=7.57886096e+01;   
           GEOM_ELEM_4polyline->pts[1626]=6.36881314e+02; GEOM_ELEM_4polyline->pts[1627]=7.57884270e+01;   
           GEOM_ELEM_4polyline->pts[1629]=6.41611431e+02; GEOM_ELEM_4polyline->pts[1630]=7.57882451e+01;   
           GEOM_ELEM_4polyline->pts[1632]=6.46485216e+02; GEOM_ELEM_4polyline->pts[1633]=7.57880624e+01;   
           GEOM_ELEM_4polyline->pts[1635]=6.51504812e+02; GEOM_ELEM_4polyline->pts[1636]=7.57878793e+01;   
           GEOM_ELEM_4polyline->pts[1638]=6.56653828e+02; GEOM_ELEM_4polyline->pts[1639]=7.57876964e+01;   
           GEOM_ELEM_4polyline->pts[1641]=6.61923658e+02; GEOM_ELEM_4polyline->pts[1642]=7.57875143e+01;   
           GEOM_ELEM_4polyline->pts[1644]=6.67317927e+02; GEOM_ELEM_4polyline->pts[1645]=7.57873328e+01;   
           GEOM_ELEM_4polyline->pts[1647]=6.72844170e+02; GEOM_ELEM_4polyline->pts[1648]=7.57871520e+01;   
           GEOM_ELEM_4polyline->pts[1650]=6.78505284e+02; GEOM_ELEM_4polyline->pts[1651]=7.57869717e+01;   
           GEOM_ELEM_4polyline->pts[1653]=6.84294036e+02; GEOM_ELEM_4polyline->pts[1654]=7.57867923e+01;   
           GEOM_ELEM_4polyline->pts[1656]=6.90193673e+02; GEOM_ELEM_4polyline->pts[1657]=7.57866142e+01;   
           GEOM_ELEM_4polyline->pts[1659]=6.96187306e+02; GEOM_ELEM_4polyline->pts[1660]=7.57864379e+01;   
           GEOM_ELEM_4polyline->pts[1662]=7.02275125e+02; GEOM_ELEM_4polyline->pts[1663]=7.57862634e+01;   
           GEOM_ELEM_4polyline->pts[1665]=7.08487930e+02; GEOM_ELEM_4polyline->pts[1666]=7.57860899e+01;   
           GEOM_ELEM_4polyline->pts[1668]=7.14893496e+02; GEOM_ELEM_4polyline->pts[1669]=7.57859157e+01;   
           GEOM_ELEM_4polyline->pts[1671]=7.21542882e+02; GEOM_ELEM_4polyline->pts[1672]=7.57857400e+01;   
           GEOM_ELEM_4polyline->pts[1674]=7.28139970e+02; GEOM_ELEM_4polyline->pts[1675]=7.57855710e+01;   
           GEOM_ELEM_4polyline->pts[1677]=7.34616804e+02; GEOM_ELEM_4polyline->pts[1678]=7.57854104e+01;   
           GEOM_ELEM_4polyline->pts[1680]=7.41024264e+02; GEOM_ELEM_4polyline->pts[1681]=7.57852569e+01;   
           GEOM_ELEM_4polyline->pts[1683]=7.47371876e+02; GEOM_ELEM_4polyline->pts[1684]=7.57851100e+01;   
           GEOM_ELEM_4polyline->pts[1686]=7.53625854e+02; GEOM_ELEM_4polyline->pts[1687]=7.57849706e+01;   
           GEOM_ELEM_4polyline->pts[1689]=7.59753182e+02; GEOM_ELEM_4polyline->pts[1690]=7.57848390e+01;   
           GEOM_ELEM_4polyline->pts[1692]=7.65756947e+02; GEOM_ELEM_4polyline->pts[1693]=7.57847151e+01;   
           GEOM_ELEM_4polyline->pts[1695]=7.71674304e+02; GEOM_ELEM_4polyline->pts[1696]=7.57845981e+01;   
           GEOM_ELEM_4polyline->pts[1698]=7.77543799e+02; GEOM_ELEM_4polyline->pts[1699]=7.57844872e+01;   
           GEOM_ELEM_4polyline->pts[1701]=7.83299342e+02; GEOM_ELEM_4polyline->pts[1702]=7.57843841e+01;   
           GEOM_ELEM_4polyline->pts[1704]=7.88795315e+02; GEOM_ELEM_4polyline->pts[1705]=7.57842911e+01;   
           GEOM_ELEM_4polyline->pts[1707]=7.94066360e+02; GEOM_ELEM_4polyline->pts[1708]=7.57842075e+01;   
           GEOM_ELEM_4polyline->pts[1710]=7.99146114e+02; GEOM_ELEM_4polyline->pts[1711]=7.57841325e+01;   
           GEOM_ELEM_4polyline->pts[1713]=8.02145215e+02; GEOM_ELEM_4polyline->pts[1714]=7.57840909e+01;   
           GEOM_ELEM_4polyline->pts[1716]=8.02511881e+02; GEOM_ELEM_4polyline->pts[1717]=7.57894577e+01;   
           GEOM_ELEM_4polyline->pts[1719]=8.02877941e+02; GEOM_ELEM_4polyline->pts[1720]=7.58052003e+01;   
           GEOM_ELEM_4polyline->pts[1722]=8.03243483e+02; GEOM_ELEM_4polyline->pts[1723]=7.58313101e+01;   
           GEOM_ELEM_4polyline->pts[1725]=8.03608254e+02; GEOM_ELEM_4polyline->pts[1726]=7.58677739e+01;   
           GEOM_ELEM_4polyline->pts[1728]=8.03971938e+02; GEOM_ELEM_4polyline->pts[1729]=7.59145661e+01;   
           GEOM_ELEM_4polyline->pts[1731]=8.04334239e+02; GEOM_ELEM_4polyline->pts[1732]=7.59716535e+01;   
           GEOM_ELEM_4polyline->pts[1734]=8.04694858e+02; GEOM_ELEM_4polyline->pts[1735]=7.60389939e+01;   
           GEOM_ELEM_4polyline->pts[1737]=8.05053493e+02; GEOM_ELEM_4polyline->pts[1738]=7.61165360e+01;   
           GEOM_ELEM_4polyline->pts[1740]=8.05409844e+02; GEOM_ELEM_4polyline->pts[1741]=7.62042189e+01;   
           GEOM_ELEM_4polyline->pts[1743]=8.05763607e+02; GEOM_ELEM_4polyline->pts[1744]=7.63019726e+01;   
           GEOM_ELEM_4polyline->pts[1746]=8.06114483e+02; GEOM_ELEM_4polyline->pts[1747]=7.64097179e+01;   
           GEOM_ELEM_4polyline->pts[1749]=8.06462173e+02; GEOM_ELEM_4polyline->pts[1750]=7.65273669e+01;   
           GEOM_ELEM_4polyline->pts[1752]=8.06806385e+02; GEOM_ELEM_4polyline->pts[1753]=7.66548230e+01;   
           GEOM_ELEM_4polyline->pts[1755]=8.07146830e+02; GEOM_ELEM_4polyline->pts[1756]=7.67919817e+01;   
           GEOM_ELEM_4polyline->pts[1758]=8.07483226e+02; GEOM_ELEM_4polyline->pts[1759]=7.69387314e+01;   
           GEOM_ELEM_4polyline->pts[1761]=8.07815296e+02; GEOM_ELEM_4polyline->pts[1762]=7.70949536e+01;   
           GEOM_ELEM_4polyline->pts[1764]=8.08142774e+02; GEOM_ELEM_4polyline->pts[1765]=7.72605237e+01;   
           GEOM_ELEM_4polyline->pts[1767]=8.08465401e+02; GEOM_ELEM_4polyline->pts[1768]=7.74353123e+01;   
           GEOM_ELEM_4polyline->pts[1770]=8.08557654e+02; GEOM_ELEM_4polyline->pts[1771]=7.74874974e+01;   
           GEOM_ELEM_4polyline->pts[1773]=8.13040136e+02; GEOM_ELEM_4polyline->pts[1774]=8.00444135e+01;   
           GEOM_ELEM_4polyline->pts[1776]=8.17524857e+02; GEOM_ELEM_4polyline->pts[1777]=8.26022183e+01;   
           GEOM_ELEM_4polyline->pts[1779]=8.19291367e+02; GEOM_ELEM_4polyline->pts[1780]=8.36094767e+01;   
           GEOM_ELEM_4polyline->pts[1782]=8.19613028e+02; GEOM_ELEM_4polyline->pts[1783]=8.37866791e+01;   
           GEOM_ELEM_4polyline->pts[1785]=8.19939256e+02; GEOM_ELEM_4polyline->pts[1786]=8.39545433e+01;   
           GEOM_ELEM_4polyline->pts[1788]=8.20270147e+02; GEOM_ELEM_4polyline->pts[1789]=8.41131345e+01;   
           GEOM_ELEM_4polyline->pts[1791]=8.20605444e+02; GEOM_ELEM_4polyline->pts[1792]=8.42623262e+01;   
           GEOM_ELEM_4polyline->pts[1794]=8.20944884e+02; GEOM_ELEM_4polyline->pts[1795]=8.44019967e+01;   
           GEOM_ELEM_4polyline->pts[1797]=8.21288194e+02; GEOM_ELEM_4polyline->pts[1798]=8.45320305e+01;   
           GEOM_ELEM_4polyline->pts[1800]=8.21635098e+02; GEOM_ELEM_4polyline->pts[1801]=8.46523184e+01;   
           GEOM_ELEM_4polyline->pts[1803]=8.21985314e+02; GEOM_ELEM_4polyline->pts[1804]=8.47627584e+01;   
           GEOM_ELEM_4polyline->pts[1806]=8.22338555e+02; GEOM_ELEM_4polyline->pts[1807]=8.48632564e+01;   
           GEOM_ELEM_4polyline->pts[1809]=8.22694532e+02; GEOM_ELEM_4polyline->pts[1810]=8.49537264e+01;   
           GEOM_ELEM_4polyline->pts[1812]=8.23052954e+02; GEOM_ELEM_4polyline->pts[1813]=8.50340909e+01;   
           GEOM_ELEM_4polyline->pts[1815]=8.23413617e+02; GEOM_ELEM_4polyline->pts[1816]=8.51042969e+01;   
           GEOM_ELEM_4polyline->pts[1818]=8.23776033e+02; GEOM_ELEM_4polyline->pts[1819]=8.51642485e+01;   
           GEOM_ELEM_4polyline->pts[1821]=8.24139867e+02; GEOM_ELEM_4polyline->pts[1822]=8.52139332e+01;   
           GEOM_ELEM_4polyline->pts[1824]=8.24505840e+02; GEOM_ELEM_4polyline->pts[1825]=8.52534510e+01;   
           GEOM_ELEM_4polyline->pts[1827]=8.24872426e+02; GEOM_ELEM_4polyline->pts[1828]=8.52825685e+01;   
           GEOM_ELEM_4polyline->pts[1830]=8.25238031e+02; GEOM_ELEM_4polyline->pts[1831]=8.53011304e+01;   
           GEOM_ELEM_4polyline->pts[1833]=8.25601368e+02; GEOM_ELEM_4polyline->pts[1834]=8.53090894e+01;   
           GEOM_ELEM_4polyline->pts[1836]=8.25697973e+02; GEOM_ELEM_4polyline->pts[1837]=8.53094241e+01;   
           GEOM_ELEM_4polyline->pts[1839]=8.26692828e+02; GEOM_ELEM_4polyline->pts[1840]=8.53094168e+01;   
           GEOM_ELEM_4polyline->pts[1842]=8.28057902e+02; GEOM_ELEM_4polyline->pts[1843]=8.53094071e+01;   
           GEOM_ELEM_4polyline->pts[1845]=8.29468617e+02; GEOM_ELEM_4polyline->pts[1846]=8.53093976e+01;   
           GEOM_ELEM_4polyline->pts[1848]=8.31235004e+02; GEOM_ELEM_4polyline->pts[1849]=8.53093866e+01;   
           GEOM_ELEM_4polyline->pts[1851]=8.32857261e+02; GEOM_ELEM_4polyline->pts[1852]=8.53093777e+01;   
           GEOM_ELEM_4polyline->pts[1854]=8.34246408e+02; GEOM_ELEM_4polyline->pts[1855]=8.53093712e+01;   
           GEOM_ELEM_4polyline->pts[1857]=8.35671355e+02; GEOM_ELEM_4polyline->pts[1858]=8.53093659e+01;   
           GEOM_ELEM_4polyline->pts[1860]=8.37088165e+02; GEOM_ELEM_4polyline->pts[1861]=8.53093618e+01;   
           GEOM_ELEM_4polyline->pts[1863]=8.40187526e+02; GEOM_ELEM_4polyline->pts[1864]=8.53093569e+01;   
           GEOM_ELEM_4polyline->pts[1866]=8.43265705e+02; GEOM_ELEM_4polyline->pts[1867]=8.53093571e+01;   
           GEOM_ELEM_4polyline->pts[1869]=8.44955009e+02; GEOM_ELEM_4polyline->pts[1870]=8.53093590e+01;   
		   }
          else  status=36;
//
//  Fill Trimsf itself 
//  
		if ( NULL!= GEOM_ELEM_4data )
		{
		   GEOM_ELEM_4->header.key      = 4;
		   GEOM_ELEM_4->header.relnum   = NCLX_MDL_TRIMSF;
		   GEOM_ELEM_4->header.subscript= 0;
		   GEOM_ELEM_4->surf       = GEOM_ELEM_4surface;
		   GEOM_ELEM_4->uv_cv      = GEOM_ELEM_4polyline;
		   GEOM_ELEM_4->ncurve     = 0;
		   GEOM_ELEM_4->xyz_cv     = 0;
		   GEOM_ELEM_4->offdist    = 0.00000000e+00;
		   GEOM_ELEM_4->inner      = 0;
		   GEOM_ELEM_4->u_min      = 0.00000000e+00;
		   GEOM_ELEM_4->u_max      = 1.15542961e+03;
		   GEOM_ELEM_4->v_min      = 0.00000000e+00;
		   GEOM_ELEM_4->v_max      = 1.04654392e+02;
		   GEOM_ELEM_4->trim_type  = NCLX_FACE;
		   GEOM_ELEM_4->no_boxlst  = 0;
		   GEOM_ELEM_4->boxlst     = 0;
		   GEOM_ELEM_4->no_bndrylst= 0;
		   GEOM_ELEM_4->bndrylst   = 0;
		}
          else  status=36;
//
		}
          else  status=36;
//
//  
//  
		NCLX_mdl_plane *  GEOM_ELEM_5=0;
		NCLX_mdl_data  *  GEOM_ELEM_5data=0;
		GEOM_ELEM_5data = (NCLX_mdl_data *) malloc(sizeof(NCLX_mdl_data));
		if ( NULL!= GEOM_ELEM_5data )
		{
		  GEOM_ELEM_5 = (NCLX_mdl_plane *) GEOM_ELEM_5data;
//  
		  if ( NULL!= GEOM_ELEM_5 )
		  {
		    GEOM_ELEM_5->header.key      = 5;
		    GEOM_ELEM_5->header.relnum   = NCLX_MDL_PLANE;
		    GEOM_ELEM_5->header.subscript= 0;
		    GEOM_ELEM_5->dist  = 0.00000000e+00;
		    GEOM_ELEM_5->pt[0] = 1.02995037e+02;
		    GEOM_ELEM_5->pt[1] = 5.30130764e+01;
		    GEOM_ELEM_5->pt[2] = 1.28301411e+02;
		    GEOM_ELEM_5->vec[0]= -9.88263689e-01;
		    GEOM_ELEM_5->vec[1]= -4.30488978e-14;
		    GEOM_ELEM_5->vec[2]= -1.52757590e-01;
		   }
          else  status=36;
//
		}
          else  status=36;
//
//  
//  
		NCLX_mdl_plane *  GEOM_ELEM_6=0;
		NCLX_mdl_data  *  GEOM_ELEM_6data=0;
		GEOM_ELEM_6data = (NCLX_mdl_data *) malloc(sizeof(NCLX_mdl_data));
		if ( NULL!= GEOM_ELEM_6data )
		{
		  GEOM_ELEM_6 = (NCLX_mdl_plane *) GEOM_ELEM_6data;
//  
		  if ( NULL!= GEOM_ELEM_6 )
		  {
		    GEOM_ELEM_6->header.key      = 6;
		    GEOM_ELEM_6->header.relnum   = NCLX_MDL_PLANE;
		    GEOM_ELEM_6->header.subscript= 0;
		    GEOM_ELEM_6->dist  = 0.00000000e+00;
		    GEOM_ELEM_6->pt[0] = -1.02243687e+02;
		    GEOM_ELEM_6->pt[1] = 6.82473172e+01;
		    GEOM_ELEM_6->pt[2] = -8.16755349e+01;
		    GEOM_ELEM_6->vec[0]= 9.93260877e-01;
		    GEOM_ELEM_6->vec[1]= 3.53248941e-14;
		    GEOM_ELEM_6->vec[2]= -1.15900084e-01;
		   }
          else  status=36;
//
		}
          else  status=36;
//
//  
//  
		NCLX_mdl_pntvec *  GEOM_ELEM_7=0;
		NCLX_mdl_data  *  GEOM_ELEM_7data=0;
		GEOM_ELEM_7data = (NCLX_mdl_data *) malloc(sizeof(NCLX_mdl_data));
		if ( NULL!= GEOM_ELEM_7data )
		{
		   GEOM_ELEM_7 = (NCLX_mdl_pntvec *) GEOM_ELEM_7data;
//  
		   GEOM_ELEM_7->header.key      = 7;
		   GEOM_ELEM_7->header.relnum   = NCLX_MDL_PNTVEC;
		   GEOM_ELEM_7->header.subscript= 0;
		   GEOM_ELEM_7->pt[0]  = 7.87074297e+01;
		   GEOM_ELEM_7->pt[1]  = 1.96708889e+01;
		   GEOM_ELEM_7->pt[2]  = 1.54542819e+02;
		   GEOM_ELEM_7->vec[0] = 0.00000000e+00;
		   GEOM_ELEM_7->vec[1] = 1.00000000e+00;
		   GEOM_ELEM_7->vec[2] = 0.00000000e+00;
		}
          else  status=36;
//
//  
//  
		Thickness.ps =0.00000000e+00; 
		Thickness.ds =0.00000000e+00; 
		Thickness.cs =0.00000000e+00; 
		Thickness.cs2=0.00000000e+00; 
		Thickness.cs3=0.00000000e+00; 
		Thickness.cs4=0.00000000e+00; 
		Thickness.cs5=0.00000000e+00; 
//  
		status =status+ NclxMotSetThick(&Thickness); 
    cout << " NclxMotSetThick " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		NCCSRefPointValue.header.key   = 7;
		NCCSRefPointValue.header.relnum= NCLX_MDL_PNTVEC;
		NCCSRefPointValue.pt[0]  = 7.87074297e+01; 
		NCCSRefPointValue.pt[1]  = 1.96708889e+01; 
		NCCSRefPointValue.pt[2]  = 1.54542819e+02; 
		NCCSRefPointValue.vec[0] = 0.00000000e+00; 
		NCCSRefPointValue.vec[1] = 1.00000000e+00; 
		NCCSRefPointValue.vec[2] = 0.00000000e+00; 
//  
		status =status+ NclxMotGoto (&NCCSRefPointValue,&clrec); 
    cout << " NclxMotGoto " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
		 print_cl(&clrec);
//  
//  
		lev_goug  =0; 
//  
		status =status+ NclxMotSetGougck(&lev_goug); 
    cout << " NclxMotSetGougck " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		contct      =2; 
//  
		status =status+ NclxMotSetContact(&contct); 
    cout << " NclxMotSetContact " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		RateForInterp        =0; 
		DegreeForInterp      =0.00000000e+00; 
//  
	   status =status+ NclxMotSetFanInterp(&RateForInterp, &DegreeForInterp); 
    cout << " NclxMotSetFanInterp " << " status= " << status<<endl;  
//    if (status != 0) goto NclError; 
//  
//
	   NCCSAxis.mode  		= (NCLX_mot_tlaxis_type) 5; 
//
	   NCCSAxis.normal		= 0; 
	   NCCSAxis.perpto_flag	= 0; 
	   NCCSAxis.angle			= 0.00000000e+00; 
	   NCCSAxis.contact		= 0; 
	   NCCSAxis.heel			= 0.00000000e+00; 
	   NCCSAxis.height		= 0.00000000e+00; 
	   NCCSAxis.curve_dist	= 0.00000000e+00; 
	   NCCSAxis.cmb_depart	= 0.00000000e+00; 
	   NCCSAxis.cmb_approach= 0.00000000e+00; 
	   NCCSAxis.parelm		= 0; 
	   NCCSAxis.center    = NCLX_TOOL_CENTER_OFF; 
	   NCCSAxis.adjust_flag			   = 1; 
	   NCCSAxis.adjust.right_offset	= 0.00000000e+00; 
	   NCCSAxis.adjust.fwd_offset		= 0.00000000e+00; 
	   NCCSAxis.adjust.up_offset		= 0.00000000e+00; 
	   NCCSAxis.adjust.right_tilt		= 0.00000000e+00; 
	   NCCSAxis.adjust.fwd_tilt		= 0.00000000e+00; 
	   NCCSAxis.modify.angle_flag		= 0; 
	   NCCSAxis.modify.right_angle	= 0.00000000e+00; 
	   NCCSAxis.modify.fwd_angle		= 0.00000000e+00; 
	   NCCSAxis.modify.guide_flag		= 0; 
	   NCCSAxis.modify.gouge			= 0; 
	   NCCSAxis.modify.guide_offset = 0.00000000e+00; 
	   NCCSAxis.modify.guide        = NULL; 
	   NCCSAxis.modify.lock_mode       = NCLX_LOCK_OFF; 
	   NCCSAxis.modify.lock_transition = 0; 
	   NCCSAxis.modify.lock_radius     = 0; 
	   NCCSAxis.modify.lock_dist       = 0.00000000e+00; 
	   NCCSAxis.modify.lock_interp_dist= 0.00000000e+00; 
	   NCCSAxis.modify.secps_flag		= 0; 
	   NCCSAxis.modify.secps   		= 0; 
	   NCCSAxis.modify.guide_contact= 0; 
	   NCCSAxis.modify.guide_cond   = 0; 
	   NCCSAxis.modify.secps_flag		= 0; 
	   NCCSAxis.modify.secps   		= 0; 
	   NCCSAxis.perpto[0]	  = 0.00000000e+00; 
	   NCCSAxis.perpto[1]	  = 0.00000000e+00; 
	   NCCSAxis.perpto[2]     = 0.00000000e+00; 
	   NCCSAxis.cmb_depart	  = 0.00000000e+00; 
	   NCCSAxis.cmb_approach  = 0.00000000e+00; 
	   NCCSAxis.point[0]     = 0.00000000e+00; 
	   NCCSAxis.point[1]     = 0.00000000e+00; 
	   NCCSAxis.point[2]     = 0.00000000e+00; 
	   NCCSAxis.curve        = 0; 
	   NCCSAxis.vector[0]    = 0.00000000e+00; 
	   NCCSAxis.vector[1]    = 0.00000000e+00; 
	   NCCSAxis.vector[2]    = 0.00000000e+00; 
//  
		status =status+ NclxMotSetTlaxis(&NCCSAxis); 
    cout << " NclxMotSetTlaxis " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
//  
		if(status==0) status =status+ NclxMotGo3 ( (NCLX_mdl_data *) GEOM_ELEM_1 , 714 ,  (NCLX_mdl_data *) GEOM_ELEM_4 , 71 , (NCLX_mdl_data *) GEOM_ELEM_5 , &clrec ); 
    cout << " NclxMotGo3 " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
		 if (status == 0) print_cl(&clrec);
//  
//  
		ForwardVec[0]   = 7.47365610e-01;
		ForwardVec[1]   = -3.71341423e-01;
		ForwardVec[2]   = -5.50953865e-01;
//  
		status =status+ NclxMotIndirv (ForwardVec); 
    cout << " NclxMotIndirv " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
		 print_cl(&clrec);
//  
//  
		NCLX_mdl_pntvec *  GEOM_ELEM_8=0;
		NCLX_mdl_data  *  GEOM_ELEM_8data=0;
		GEOM_ELEM_8data = (NCLX_mdl_data *) malloc(sizeof(NCLX_mdl_data));
		if ( NULL!= GEOM_ELEM_8data )
		{
		   GEOM_ELEM_8 = (NCLX_mdl_pntvec *) GEOM_ELEM_8data;
//  
		   GEOM_ELEM_8->header.key      = 8;
		   GEOM_ELEM_8->header.relnum   = NCLX_MDL_PNTVEC;
		   GEOM_ELEM_8->header.subscript= 0;
		   GEOM_ELEM_8->pt[0]  = 1.01540749e+02;
		   GEOM_ELEM_8->pt[1]  = 8.32576275e+00;
		   GEOM_ELEM_8->pt[2]  = 1.37710220e+02;
		   GEOM_ELEM_8->vec[0] = -1.64058490e-03;
		   GEOM_ELEM_8->vec[1] = 9.99942362e-01;
		   GEOM_ELEM_8->vec[2] = 1.06137469e-02;
		}
          else  status=36;
//
//  
//  
       CLREC_ToolPathBeginning = clrec.start;
//  
//  
		autost      =1; 
		omit      =0; 
//  
		status =status+ NclxMotSetAutost1 (&autost,&omit); 
    cout << " NclxMotSetAutost1 " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
      NCCSCheckStruct.numchk = 1; 
      NCCSCheckStruct.csrec  = ioS_csrec;
      ioS_csrec[0].nrpt_flag = 0  ;
      ioS_csrec[0].nrpt      = NULL;
      ioS_csrec[0].nintof    = 3  ;
      ioS_csrec[0].csatt     = 71  ;
      ioS_csrec[0].cs        = (NCLX_mdl_data *) GEOM_ELEM_6 ;
      ioS_csrec[0].avoid     = 0  ;
//  
//  
		Thickness.ps =0.00000000e+00; 
		Thickness.ds =0.00000000e+00; 
		Thickness.cs =0.00000000e+00; 
		Thickness.cs2=0.00000000e+00; 
		Thickness.cs3=0.00000000e+00; 
		Thickness.cs4=0.00000000e+00; 
		Thickness.cs5=0.00000000e+00; 
//  
		status =status+ NclxMotSetThick(&Thickness); 
    cout << " NclxMotSetThick " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		lev_goug  =0; 
//  
		status =status+ NclxMotSetGougck(&lev_goug); 
    cout << " NclxMotSetGougck " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		contct      =2; 
//  
		status =status+ NclxMotSetContact(&contct); 
    cout << " NclxMotSetContact " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
		RateForInterp        =0; 
		DegreeForInterp      =0.00000000e+00; 
//  
	   status =status+ NclxMotSetFanInterp(&RateForInterp, &DegreeForInterp); 
    cout << " NclxMotSetFanInterp " << " status= " << status<<endl;  
//    if (status != 0) goto NclError; 
//  
//
	   NCCSAxis.mode  		= (NCLX_mot_tlaxis_type) 5; 
//
	   NCCSAxis.normal		= 0; 
	   NCCSAxis.perpto_flag	= 0; 
	   NCCSAxis.angle			= 0.00000000e+00; 
	   NCCSAxis.contact		= 0; 
	   NCCSAxis.heel			= 0.00000000e+00; 
	   NCCSAxis.height		= 0.00000000e+00; 
	   NCCSAxis.curve_dist	= 0.00000000e+00; 
	   NCCSAxis.cmb_depart	= 0.00000000e+00; 
	   NCCSAxis.cmb_approach= 0.00000000e+00; 
	   NCCSAxis.parelm		= 0; 
	   NCCSAxis.center    = NCLX_TOOL_CENTER_OFF; 
	   NCCSAxis.adjust_flag			   = 1; 
	   NCCSAxis.adjust.right_offset	= 0.00000000e+00; 
	   NCCSAxis.adjust.fwd_offset		= 0.00000000e+00; 
	   NCCSAxis.adjust.up_offset		= 0.00000000e+00; 
	   NCCSAxis.adjust.right_tilt		= 0.00000000e+00; 
	   NCCSAxis.adjust.fwd_tilt		= 0.00000000e+00; 
	   NCCSAxis.modify.angle_flag		= 0; 
	   NCCSAxis.modify.right_angle	= 0.00000000e+00; 
	   NCCSAxis.modify.fwd_angle		= 0.00000000e+00; 
	   NCCSAxis.modify.guide_flag		= 0; 
	   NCCSAxis.modify.gouge			= 0; 
	   NCCSAxis.modify.guide_offset = 0.00000000e+00; 
	   NCCSAxis.modify.guide        = NULL; 
	   NCCSAxis.modify.lock_mode       = NCLX_LOCK_OFF; 
	   NCCSAxis.modify.lock_transition = 0; 
	   NCCSAxis.modify.lock_radius     = 0; 
	   NCCSAxis.modify.lock_dist       = 0.00000000e+00; 
	   NCCSAxis.modify.lock_interp_dist= 0.00000000e+00; 
	   NCCSAxis.modify.secps_flag		= 0; 
	   NCCSAxis.modify.secps   		= 0; 
	   NCCSAxis.modify.guide_contact= 0; 
	   NCCSAxis.modify.guide_cond   = 0; 
	   NCCSAxis.modify.secps_flag		= 0; 
	   NCCSAxis.modify.secps   		= 0; 
	   NCCSAxis.perpto[0]	  = 0.00000000e+00; 
	   NCCSAxis.perpto[1]	  = 0.00000000e+00; 
	   NCCSAxis.perpto[2]     = 0.00000000e+00; 
	   NCCSAxis.cmb_depart	  = 0.00000000e+00; 
	   NCCSAxis.cmb_approach  = 0.00000000e+00; 
	   NCCSAxis.point[0]     = 0.00000000e+00; 
	   NCCSAxis.point[1]     = 0.00000000e+00; 
	   NCCSAxis.point[2]     = 0.00000000e+00; 
	   NCCSAxis.curve        = 0; 
	   NCCSAxis.vector[0]    = 0.00000000e+00; 
	   NCCSAxis.vector[1]    = 0.00000000e+00; 
	   NCCSAxis.vector[2]    = 0.00000000e+00; 
//  
		status =status+ NclxMotSetTlaxis(&NCCSAxis); 
    cout << " NclxMotSetTlaxis " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//GetTlaxis
	   NCCSAxis.mode  		= NCLX_MOT_TLAXIS_FAN; 
//
	   NCCSAxis.normal		= 0; 
	   NCCSAxis.perpto_flag	= 0; 
	   NCCSAxis.angle			= 0.00000000e+00; 
	   NCCSAxis.contact		= 0; 
	   NCCSAxis.heel			= 0.00000000e+00; 
	   NCCSAxis.height		= 0.00000000e+00; 
	   NCCSAxis.curve_dist	= 0.00000000e+00; 
	   NCCSAxis.cmb_depart	= 0.00000000e+00; 
	   NCCSAxis.cmb_approach= 0.00000000e+00; 
	   NCCSAxis.parelm		= 0; 
	   NCCSAxis.center    = NCLX_TOOL_CENTER_OFF; 
	   NCCSAxis.adjust_flag			   = 1; 
	   NCCSAxis.adjust.right_offset	= 0.00000000e+00; 
	   NCCSAxis.adjust.fwd_offset		= 0.00000000e+00; 
	   NCCSAxis.adjust.up_offset		= 0.00000000e+00; 
	   NCCSAxis.adjust.right_tilt		= 0.00000000e+00; 
	   NCCSAxis.adjust.fwd_tilt		= 0.00000000e+00; 
	   NCCSAxis.modify.angle_flag		= 0; 
	   NCCSAxis.modify.right_angle	= 0.00000000e+00; 
	   NCCSAxis.modify.fwd_angle		= 0.00000000e+00; 
	   NCCSAxis.modify.guide_flag		= 0; 
	   NCCSAxis.modify.gouge			= 0; 
	   NCCSAxis.modify.guide_offset = 0.00000000e+00; 
	   //NCLX_mdl_data         tempo_guid_crv;//moved to the top  
	   NCCSAxis.modify.guide        = &tempo_guid_crv; 
	   NCCSAxis.modify.lock_mode       = NCLX_LOCK_OFF; 
	   NCCSAxis.modify.lock_transition = 0; 
	   NCCSAxis.modify.lock_radius     = 0; 
	   NCCSAxis.modify.lock_dist       = 0.00000000e+00; 
	   NCCSAxis.modify.lock_interp_dist= 0.00000000e+00; 
	   NCCSAxis.modify.secps_flag		= 0; 
	   NCCSAxis.modify.secps   		= 0; 
	   NCCSAxis.modify.guide_contact= 0; 
	   NCCSAxis.modify.guide_cond   = 0; 
	   NCCSAxis.modify.secps_flag		= 0; 
	   NCCSAxis.modify.secps   		= 0; 
	   NCCSAxis.perpto[0]	  = 0.00000000e+00; 
	   NCCSAxis.perpto[1]	  = 0.00000000e+00; 
	   NCCSAxis.perpto[2]     = 0.00000000e+00; 
	   NCCSAxis.cmb_depart	  = 0.00000000e+00; 
	   NCCSAxis.cmb_approach  = 0.00000000e+00; 
	   NCCSAxis.point[0]     = 0.00000000e+00; 
	   NCCSAxis.point[1]     = 0.00000000e+00; 
	   NCCSAxis.point[2]     = 0.00000000e+00; 
	   NCCSAxis.curve        = 0; 
	   NCCSAxis.vector[0]    = 0.00000000e+00; 
	   NCCSAxis.vector[1]    = 0.00000000e+00; 
	   NCCSAxis.vector[2]    = 0.00000000e+00; 
//  
		NclxMotGetTlaxis(&NCCSAxis); 
//  
//  
		Maxang      =9.00000000e+01; 
//  
	   status =status+ NclxMotSetMaxang(&Maxang); 
    cout << " NclxMotSetMaxang " << " status= " << status<<endl;  
//    if (status != 0) goto NclError; 
//  
//  
		NCCSFeedrate.accel_flag       =0; 
		NCCSFeedrate.accel_dist       =0.00000000e+00; 
		NCCSFeedrate.accel_feedrate   =0.00000000e+00; 
		NCCSFeedrate.slowdown_flag    =0; 
		NCCSFeedrate.slowdown_dist    =0.00000000e+00; 
		NCCSFeedrate.slowdown_feedrate=0.00000000e+00; 
		NCCSFeedrate.height           =0.00000000e+00; 
		NCCSFeedrate.base_feedrate		=1.00000000e+02; 
//  
		status =status+ NclxMotSetFeedrate(&NCCSFeedrate); 
    cout << " NclxMotSetFeedrate " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
//  
//  
		if(statusDrive1<=0) statusDrive1 = NclxMotDrive1 ( (NCLX_mdl_data *) GEOM_ELEM_1 , -1 , 705 ,(NCLX_mdl_data *) GEOM_ELEM_4 , &NCCSCheckStruct,&clrec ); 
       cout << " NclxMotDrive1 " << " statusDrive1= " << statusDrive1<<endl;  
//      if (statusDrive1 > 0) goto NclError; 
        if (statusDrive1 > 0) status+=statusDrive1; 
//  
		 if (statusDrive1 == 0) print_cl(&clrec);
//  
//  
       clrec.start = CLREC_ToolPathBeginning ;
//  
//  
		fillet.clrange   = clrec;
		fillet.rad       = 1.000000;
		fillet.tol	     = 0.005000;
		fillet.fedrt     = 0.0;  // hard coded 
		fillet.fmax	     = 0.000000;
		fillet.direction = NCLX_TLRGT;
		fillet.cdia      = 1.000000;
		fillet.same      = NCLX_FALSE;
		fillet.maxang    = 180.;
		fillet.combine   = NCLX_TRUE;
		fillet.fedctl    = NCLX_FALSE;
		status = NclxMotFillet(&fillet);
    cout << " NclxMotFillet " << " status= " << status<<endl;  
//      if (status != 0) goto NclError; 
//  
		 print_cl(&clrec);
//  
//  
       clrec.start = CLREC_ToolPathBeginning ;
//  
//  
//  FREE FACES
//  
		   if ( NULL!= GEOM_ELEM_4 )
		   {
			  if ( GEOM_ELEM_4->boxlst!= NULL) { status=status+ NclxMdlFree(GEOM_ELEM_4->boxlst); GEOM_ELEM_4->boxlst= NULL;} 
			  GEOM_ELEM_4->no_boxlst  = 0;
			  if ( GEOM_ELEM_4->bndrylst!= NULL) {status=status+ NclxMdlFree(GEOM_ELEM_4->bndrylst); GEOM_ELEM_4->bndrylst = NULL;}
			  GEOM_ELEM_4->no_bndrylst= 0;
		   }
//  
		   if ( NULL!= GEOM_ELEM_4surface )
		   {
			  if ( NULL!= GEOM_ELEM_4surface->tu ) {free (GEOM_ELEM_4surface->tu);  GEOM_ELEM_4surface->tu  = NULL;
}			  if ( NULL!= GEOM_ELEM_4surface->tv ) {free (GEOM_ELEM_4surface->tv);  GEOM_ELEM_4surface->tv  = NULL;
}			  if ( NULL!= GEOM_ELEM_4surface->pt ) {free (GEOM_ELEM_4surface->pt);  GEOM_ELEM_4surface->pt  = NULL;
}			  if ( NULL!= GEOM_ELEM_4surface->wgt ) {free (GEOM_ELEM_4surface->wgt);  GEOM_ELEM_4surface->wgt  = NULL;
}			  GEOM_ELEM_4surface->ntu  = 0;
			  GEOM_ELEM_4surface->ntv  = 0;
			  GEOM_ELEM_4surface->npt  = 0;
			  GEOM_ELEM_4surface->nwgt = 0;
//  
			  if ( GEOM_ELEM_4surface ->boxlst   != NULL) {status=status+ NclxMdlFree( GEOM_ELEM_4surface->boxlst); GEOM_ELEM_4surface->boxlst= NULL;}
			  GEOM_ELEM_4surface->no_boxlst   = 0;
			  if ( GEOM_ELEM_4surface ->bndrylst != NULL) {status=status+ NclxMdlFree (GEOM_ELEM_4surface->bndrylst);  GEOM_ELEM_4surface->bndrylst = NULL;}
			  GEOM_ELEM_4surface->no_bndrylst = 0;
			  free(GEOM_ELEM_4surface); GEOM_ELEM_4surface = NULL;
		   }
//  
		   if ( NULL!= GEOM_ELEM_4polyline )
		   {
			  if (NULL!= GEOM_ELEM_4polyline->pts) {free(GEOM_ELEM_4polyline->pts); GEOM_ELEM_4polyline->pts = NULL;  GEOM_ELEM_4polyline->npts = 0;}
			  free (GEOM_ELEM_4polyline);     GEOM_ELEM_4polyline = NULL;
		   }
//  
//  
//  FREE MAIN GEOMETRY 
			if (GEOM_ELEM_4data != NULL) { free(GEOM_ELEM_4data); GEOM_ELEM_4data = NULL; };
//  
//  
//  FREE MAIN GEOMETRY 
			if (GEOM_ELEM_1data != NULL) { free(GEOM_ELEM_1data); GEOM_ELEM_1data = NULL; };
//  
//  
//  FREE MAIN GEOMETRY 
			if (GEOM_ELEM_5data != NULL) { free(GEOM_ELEM_5data); GEOM_ELEM_5data = NULL; };
//  
//  
//  FREE MAIN GEOMETRY 
			if (GEOM_ELEM_6data != NULL) { free(GEOM_ELEM_6data); GEOM_ELEM_6data = NULL; };
//  
//  
//  FREE MAIN GEOMETRY 
			if (GEOM_ELEM_7data != NULL) { free(GEOM_ELEM_7data); GEOM_ELEM_7data = NULL; };
//  
//  
//  FREE MAIN GEOMETRY 
			if (GEOM_ELEM_8data != NULL) { free(GEOM_ELEM_8data); GEOM_ELEM_8data = NULL; };
//  
////////////////////////////////////////////////// 
//NclError : 
     if (status != 0) cout << "status =" << status<<endl;  
     NclxMotLibClose(); 
     return status;
} 
#ifndef DS_TESTOBJECT
// 
 //
 //
int NclxMotInterrupt()
{ 
    return (NCLX_FALSE);
} 
#endif
