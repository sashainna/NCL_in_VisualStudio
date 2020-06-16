/*********************************************************************
**  NAME:  SwNclUnibase.cpp
**
**       Controlling routines for converting SolidWorks geometry to
**       the Unibase format.
**
** CONTAINS: CNclUnibase class functions
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**			SwNclUnibase.cpp , 21.1
**    DATE AND TIME OF LAST  MODIFICATION
**			12/10/09 , 18:02:15
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
#include "SwUnibase.h"
#include "mcrv.h"
#include "nccs.h"
#include "riddle.h"

extern "C" int NclxMdlUnibaseInit();
extern "C" int NclxMdlUnibaseClose();
extern "C" int NclxMdlStoreTrimSurf(NCLX_mdl_trimsf*,int, int *);
extern "C" int NclxMdlUnibaseSave(char *);
extern "C" int NclxMdlLoadSecondary(char *);
extern "C" int NclxMdlUpdateUnistat(struct UR_unistat_rec *);
extern "C" int NclxMdlLabelEntity(char *, int *, int, char *);
extern "C" int NclxMdlMatchLabels(int, int, int, double, int, void *,
	NCLX_label_struc *, void *, int layers[], int colors[], void *);
extern "C" int NclxMdlSetupData(NCLX_mdl_struct*,int,char *,int);
extern "C" int NclxMdlEvalCurve(NCLX_mdl_curve*,double,NCLX_mdl_curve_eval*);
extern "C" int NclxMdlEvalSurf(NCLX_mdl_surf*,double,double,NCLX_mdl_surf_eval*,int);
extern "C" int NclxMdlInterpCurve(NCLX_mdl_curve*,double*,int);
extern "C" int NclxMdlStoreCurve(NCLX_mdl_curve*,int,int*);
extern "C" int NclxMdlStoreLine (NCLX_mdl_line*, int);
extern "C" int NclxMdlStoreCircle (NCLX_mdl_circle*, int);
extern "C" int NclxMdlStoreComposite (NCLX_mdl_composite*, int, int*);
extern "C" int NclxMdlGetCone(double*,double*,double*);
extern "C" int gtimx(int *,int *);
extern "C" double um_dot(double *,double *);
extern "C" double um_mag(double *);
extern "C" double um_dcccc(double *,double *);
extern "C" int    um_vctovc(double *,double *);
extern "C" int    um_vcmnvc(double *,double *,double *);
extern "C" int    um_unitvc(double *,double *);
extern "C" int    um_cctmtf(double *, double *, double *);
extern "C" int    um_vctmtf(double *, double *, double *);
extern "C" int    um_cross (double *, double *, double *);
extern "C" char* uu_malloc(int);
extern "C" void uu_free(void  *);
extern "C" void getver(double  *);
//extern "C" char *uu_lsnew(), *uu_lsinsrt(char *, int);
//extern "C" char *uu_lsinsrt(char *, int);
extern "C" int uy_update_tfmat(int, double *);
extern "C" int uy_circ_3pts(double*,double*,double*,NCLX_mdl_circle*);
extern "C" int uig_match_line(UM_line_rec*, int);
extern "C" int uig_match_circle(UM_circle_rec*, int);
extern "C" int uig_match_rbsplcrv2(UM_rbsplcrv_rec*, double*, int);
extern "C" void pwdall(char *, char *, int *);
extern "C" void pwddea(char *);
extern "C" int ncl_sf_prim_analyz(UM_int4 *, UM_int2*, UM_real8 [16]);
extern "C" void ul_short_filename(char *fin,char *fout,int maxc);	
extern "C" void ul_break_fname(char *fullname,char *dir,char *fname);
extern "C" void ul_build_full_fname(char *fdir,char *fname,char *fext,char *fullname);
extern "C" void ul_short_filename(char *fin,char *fout,int maxc);
extern "C" void uk_list_out(char line[],UU_LOGICAL display);
extern "C" void getver(double  *);
extern "C" void ur_retrieve_unibase_stat(UR_unistat_rec *);

static char save_str[400];
FILE *listfd;	
char p_buff[180];
int GeomTs[20];

extern "C" double UIG_match_tol;

static double tfmat[16], *tfmatp;

#define TWOPI (2.0 * 3.14159265359)


/*********************************************************************
**    I_FUNCTION     :  uk_create_unibase(pIswApp,swopt)
**       C-wrapper interface to the CNclUnibase class.
**    PARAMETERS
**       INPUT  :
**          (See SaveUnibase)
**       OUTPUT :
**          (See SaveUnibase)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uk_create_unibase(LPSLDWORKS *SldWorks,NCLX_sw_options *swopt)
{
	int ierr;
	char lmsg[80];
	TCHAR bstr[80];
	LPSLDWORKS pIswApp;
int nc;
char stbuf[80];
TCHAR tbuf[80];
LPMODELDOC pIswModelDocPart;
HRESULT hRes;
	pIswApp = *SldWorks;
	pwdall("SOLIDWORKS",lmsg,&ierr);
	if (ierr != 0)
	{
//	UNICODE
//		MessageBox(NULL,lmsg,"NCL/SOLID",MB_OK);
		mbstowcs(bstr,lmsg,strlen(lmsg)+1);
		MessageBox(NULL,bstr,_T("NCL/SOLID"),MB_OK);
		pwddea("");
		return;
	}

//	iswApp = GetSldWorksPtr();
	hRes = pIswApp->get_IActiveDoc(&pIswModelDocPart);
//sprintf(stbuf,"pIswApp = 0x%x",pIswApp);
sprintf(stbuf,"pIswModlDocPart = 0x%x",pIswModelDocPart);
nc = strlen(stbuf);
mbstowcs(tbuf,stbuf,nc+1);
MessageBox(NULL,tbuf,_T("NCL/SOLID"),MB_OK);
	CNclUnibase *Nuni = new CNclUnibase();
	Nuni->SaveUnibase(pIswApp,swopt);
	delete Nuni;
	pwddea("SOLIDWORKS");
	pwddea("");
}

/*********************************************************************
**    I_FUNCTION     :  SaveUnibase(pIswApp,swopt)
**       Controlling routine for converting the SolidsWorks geometry
**       to a Unibase format and saving the Unibase to an external file.
**    PARAMETERS
**       INPUT  :
**          pIswApp   = Pointer to SolidWorks application classes.
**          swopt     = Input structure containing NCL Unibase form
**                      entries.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::SaveUnibase(LPSLDWORKS pIswApp,NCLX_sw_options *swopt)
{
	LPMODELDOC	pIswModelDoc = NULL;
	LPCONFIGURATION pIswConfig = NULL;
	LPCOMPONENT2 comproot = NULL;
//	LPSLDWORKS  pIswApp = UserApp->getSWApp();
	HRESULT hRes;
	HRESULT status;
	LPMODELDOC  pIswModelDocPart = NULL;
	LPPARTDOC part = NULL;
	LPFEATURE feat,nextfeat;
	IMathTransform *swCoordSys, *swCoordSysInv;
	CComPtr<IModelDocExtension> ext;
//	LPMODELDOC2  pIswModelDocPart2 = NULL;
	CComPtr<IModelDoc2> pIswModelDocPart2 = NULL;
	CComPtr<ISldWorks> iswApp;
	BSTR dstr,partname;
	BSTR Name,featName,bstr;
	NCLX_label_struc labstr[2];
	double ver;
	char sbuf[UX_MAX_PATH_LEN],*p, *mxlist, *p1, *p2, *skname;
	int j,nc,stat;
	long type;
	char ifname[UX_MAX_PATH_LEN],ofname[UX_MAX_PATH_LEN],ufname[UX_MAX_PATH_LEN];
	char inputname[62],filename[UX_MAX_PATH_LEN],cstr[80],dir[UX_MAX_PATH_LEN];
	LPUNKNOWN iUnk;
	LPSKETCH pSketch, *sklist, *p3;
	LPFEATURE pSubFeat,pNextSubFeat;
	struct UR_unistat_rec unistat;
char stbuf[80];
TCHAR tbuf[80];

//	iswApp = GetSldWorksPtr();
	hRes = pIswApp->get_IActiveDoc(&pIswModelDocPart);
//sprintf(stbuf,"pIswApp = 0x%x",pIswApp);
sprintf(stbuf,"pIswModlDocPart = 0x%x",pIswModelDocPart);
nc = strlen(stbuf);
mbstowcs(tbuf,stbuf,nc+1);
MessageBox(NULL,tbuf,_T("NCL/SOLID"),MB_OK);
	if (pIswModelDocPart == UU_NULL) return;
	hRes = pIswModelDocPart->GetType(&type);
//
//...Assembly is active document
//
	if (pIswModelDocPart != NULL && type == swDocASSEMBLY)
	{
		hRes = pIswModelDocPart->IGetActiveConfiguration(&pIswConfig);
		pIswConfig->IGetRootComponent2(&comproot);
	}
//
//...Part is active document
//
	else if (pIswModelDocPart != NULL && type == swDocPART)
	{
		hRes = pIswModelDocPart->QueryInterface(IID_IPartDoc,(void **)&part);
	}
//
//...Get the Unibase file name
//
	if (hRes == S_OK)
	{
		pIswApp->GetCurrentWorkingDirectory(&dstr);
		nc = wcslen(dstr);
		wcstombs(swopt->dir,dstr,nc+1);
		SysFreeString(dstr);
		if (strlen(swopt->unibase) == 0)
		{
			pIswModelDocPart->GetPathName(&partname);
			nc = wcslen(partname);
			wcstombs(swopt->unibase,partname,nc+1);
			strcpy(filename,swopt->unibase);
			SysFreeString(partname);
			p = strrchr(swopt->unibase,'.');
			if (p != 0) *p = '\0';
			strcat(swopt->unibase,".u");
		}
		strcpy(ifname,swopt->unibase);
//
//......Get defined Coordinate Systems &
//......Sketches
//
		mxlist = NULL;
		skname = NULL;
		sklist = NULL;
		status = pIswModelDocPart->IFirstFeature( &feat );
		if (status == S_OK) mxlist = (char *)uu_lsnew();
		if (!mxlist) status = !S_OK;
		if (status == S_OK) skname = (char *)uu_lsnew();
		if (!skname) status = !S_OK;
		if (status == S_OK) sklist = (LPSKETCH *)uu_lsnew();
		if (!sklist) status = !S_OK;
		p1 = mxlist;
		p2 = skname;
		p3 = sklist;
//
//......Loop through features
//
		while (status == S_OK && feat)
		{
//
//.........Look for Sketches in feature
//
			status = feat->GetTypeName(&Name);
			if (status != S_OK) pSubFeat = NULL;
			else
			{
				status = feat->IGetSpecificFeature(&iUnk);
				if (!iUnk) status = -1;
				if (status == S_OK)
					status = iUnk->QueryInterface(IID_ISketch,(LPVOID *)&pSketch);
				if (status == S_OK)
					pSubFeat = feat;
				else status = feat->IGetFirstSubFeature(&pSubFeat);
			}
			while (status == S_OK && pSubFeat)
			{
				status = pSubFeat->IGetSpecificFeature(&iUnk);
				if (!iUnk) status = -1;
				if (status == S_OK)
					status = iUnk->QueryInterface(IID_ISketch,(LPVOID *)&pSketch);
				if (status == S_OK)
				{
					status = pSubFeat->get_Name(&featName);
					if (status == S_OK)
					{
						wcstombs(sbuf,featName,80);
						nc = strlen(sbuf);
						if (nc)
						{
							nc++;
							p2 = uu_lsinsrt(p2,nc);
							if (p2) strcpy(p2,sbuf);
						}
						p3 = (LPSKETCH *)uu_lsinsrt((char *)p3,sizeof(LPSKETCH));
						*p3 = pSketch;
					}
				}
				if (pSubFeat == feat) break;
				status = pSubFeat->IGetNextSubFeature(&pNextSubFeat);
				pSubFeat->Release();
				pSubFeat = pNextSubFeat;
			}
//
//.........Look for Coordinate Systems in feature
//
			if (status == S_OK)
			{
				wcstombs(sbuf,Name,80);
				if (!strcmp(sbuf,"CoordSys"))
				{
					status = feat->get_Name(&Name);
					if (status == S_OK)
					{
						wcstombs(sbuf,Name,80);
						nc = strlen(sbuf);
						if (nc)
						{
							nc++;
							p1 = uu_lsinsrt(p1,nc);
							if (p1) strcpy(p1,sbuf);
						}
					}
				}
			status = feat->IGetNextFeature(&nextfeat);
			feat->Release();
			feat = nextfeat;
			}
		}

		getver(&ver);
		CUniOpt *optDlg = new CUniOpt(swopt,ver, mxlist);
//
//...Create a unibase
//
		if (optDlg->DoModal() == IDOK)
		{
//
//...Initialize the listing file 
//
			strcpy(ofname,swopt->unibase);
			my_this->open_listing_file(ifname, ofname);
			if(!swopt->label_opts)
				sprintf(sbuf, "Labels generated by NCL\n\n");
			else
			{
				sprintf(sbuf, "Labels generated from existing Unibase\n");
				my_this->list_file_out(sbuf);
				strcpy(ufname,swopt->unimatch);
				ul_short_filename(ufname,inputname,60);
				sprintf(sbuf, "Labelling Unibase: %s\n\n", inputname);
			}
			my_this->list_file_out(sbuf);
//
//......Display the status window
//
			m_statDlg = new CUniStat();
//
//......Initialize the Unibase
//
			NclxMdlUnibaseInit();
//
//......Define the statistics record
//
			ur_retrieve_unibase_stat(&unistat);

			pIswApp->RevisionNumber(&bstr);
			nc = wcslen(bstr);
			wcstombs(cstr,bstr,nc+1);
			SysFreeString(bstr);
			sprintf(unistat.system,"SolidWorks %s",cstr);

			strcpy(unistat.fname,filename);

			sprintf(unistat.translator,"NCL/Solid V%.3f",ver);
			NclxMdlUpdateUnistat(&unistat);
//
//......Load the secondary Unibase
//......If doing label matching
//
			if (swopt->label_opts == 1)
			{
				ul_short_filename(swopt->unimatch,dir,50);
				sprintf(sbuf,"Loading %s",dir);
				m_statDlg->AddList(sbuf);
				ul_break_fname(ofname,dir,filename);
				ul_build_full_fname(dir,swopt->unimatch,"",ofname);
				stat = NclxMdlLoadSecondary(ofname);
				if (stat != NCLX_SUCCESS)
				{
					ul_short_filename(ofname,dir,50);
					sprintf(sbuf,"An error occured trying to load %s",
						dir);
					m_statDlg->AddList(sbuf);
					m_statDlg->Finished();
					m_statDlg->Wait();
					delete m_statDlg;
					my_this->list_file_out(sbuf);
					my_this->list_file_out("\n");
					return;
				}
			}

			UIG_match_tol = swopt->toler;
			tfmatp = NULL;

			if (strlen(swopt->mxname))
			{
				Name = A2BSTR(swopt->mxname);
				status = pIswApp->get_IActiveDoc2(&pIswModelDocPart2);
				if (status == S_OK)
					status = pIswModelDocPart2->get_Extension(&ext);
				if (status == S_OK)
					status = ext->GetCoordinateSystemTransformByName ( Name, &swCoordSys);
				SysFreeString(Name);
				if (status == S_OK)
				{
					status = swCoordSys->IInverse(&swCoordSysInv);
				}
				if (!swCoordSysInv) status = !S_OK;
				if (status == S_OK)
				{
					status = swCoordSysInv->get_IArrayData ( tfmat );
				}
				if (status == S_OK)
				{
					if (tfmat[12] != 1.0)
					{
						for (j=0;j<9;j++) tfmat[j]  *= tfmat[12];
					}
					tfmat[9]  /= .0254;
					tfmat[10] /= .0254;
					tfmat[11] /= .0254;
					tfmatp = tfmat;
					sprintf(sbuf,"Cooordinate System: %s\n\n",swopt->mxname);
					my_this->list_file_out(sbuf);
				}
			}
//
//......Count the surfaces
//......to be converted
//
			m_statDlg->AddList("Preprocessing Geometry ...");
			CountSurfs(part,comproot);
			m_statDlg->ShowProgress(0);
			sprintf(sbuf,"Converting %d Surfaces ...",m_srfCount);
			m_statDlg->AddList(sbuf);
//
//......Loop through the parts/assemblies
//
			m_swopt = *swopt;
			TraverseSurfs(part,comproot);
			sprintf(sbuf,"Converted %d Surfaces\n",m_srfCount);
			my_this->list_file_out(sbuf);
			if (m_curveNum > 0)
			{
				sprintf(sbuf,"Converted %d Curves",m_curveNum);
				m_statDlg->AddList(sbuf);
				strcat(sbuf,"\n");
				my_this->list_file_out(sbuf);
			}
//
//......Convert sketch geometry
//
			if (swopt->import_sketch) CreateSketches(sklist,skname,tfmatp);
//
//......Match labels with existing Unibase
//
			if (swopt->label_opts == 1 && !(m_statDlg->GetInterrupt()))
			{
				m_nlevel = -1;
				m_nmatch = -1;
				my_this = this;
				labstr[0].type = m_swopt.sflab_type;
				labstr[1].type = m_swopt.cvlab_type;
				strcpy(labstr[0].prefix,m_swopt.sflab);
				strcpy(labstr[1].prefix,m_swopt.cvlab);
				labstr[1].sub = 1;
				labstr[0].sub = 1;
				m_srfCount = m_srfCount;
				stat = NclxMdlMatchLabels(m_srfCount,m_curveNum,m_sketchCount,
					swopt->toler,
					swopt->exact_match,&CNclUnibase::CountMatches,labstr,
					&CNclUnibase::GetInterrupt,swopt->layer,swopt->color,
					&CNclUnibase::ListOut);
			}
//
//......Process was interrupted
//
			if (m_statDlg->GetInterrupt())
			{
				NclxMdlUnibaseClose();
				m_statDlg->AddList(" ");
				m_statDlg->AddList("               *** INTERRUPTED ***");
			}
//
//......Write out the Unibase
//
			else
			{
				sprintf(sbuf,"Writing %s",swopt->unibase);
				m_statDlg->AddList(sbuf);
				stat = NclxMdlUnibaseSave(swopt->unibase);
				if (stat != NCLX_SUCCESS)
				{
					sprintf(sbuf,"An error occured trying to save %s",
						swopt->unibase);
					m_statDlg->AddList(sbuf);
					my_this->list_file_out(sbuf);
					my_this->list_file_out("\n\n");
				}
				NclxMdlUnibaseClose();
				m_statDlg->AddList(" ");
				m_statDlg->AddList("               *** FINISHED ***");
			}
//
//......Close listing file
//
			my_this->close_listing_file();
//
//......Get rid of the status window
//
			m_statDlg->Finished();
			m_statDlg->Wait();
			delete m_statDlg;
		}
//
//.....Free the memory
//
		if (skname != NULL) uu_lsdel((char *)skname);
		if (sklist != NULL) uu_lsdel((char *)sklist);
//
//...Release the part document
//
		if (type == swDocASSEMBLY)
		{
			pIswConfig->Release();
			comproot->Release();
		}
		else
		{
			part->Release(); // New
		}
		pIswModelDocPart->Release();
		delete optDlg;
	}
	return;
}

/*********************************************************************
**    I_FUNCTION     :  TraverseSurfs(part,comproot)
**       Calls the routine to create the Unibase with either a single
**       part or an assembly of parts.
**    PARAMETERS
**       INPUT  :
**          part      = Part document from SolidWorks.  If set to NULL,
**                      then an assembly is output.
**          comproot  = Root component from SolidWorks.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::TraverseSurfs(LPPARTDOC part, LPCOMPONENT2 comproot)
{
	int i,nchild;
	LPCOMPONENT2 *compon;
//
//...Single part
//
	if (part != NULL)
	{
		CreateUnibase(part,comproot);
	}
//
//...Assembly
//
	else
	{
		comproot->IGetChildrenCount(&nchild);
		if (nchild != 0)
		{
			compon = new LPCOMPONENT2[nchild];
			comproot->IGetChildren(compon);
			for (i=0;i<nchild;i++)
			{
				CreateUnibase(part,compon[i]);
				TraverseSurfs(part,compon[i]);
				compon[i]->Release();
  			}
			if (nchild != 0) delete [] compon;
		}
	}
//
//...End of routine
//
}

/*********************************************************************
**    I_FUNCTION     :  CreateUnibase(part,compon)
**       Loops through all SolidWorks surfaces, then converts and stores
**       them in the Unibase.
**    PARAMETERS
**       INPUT  :
**          part      = Part document from SolidWorks.  If set to NULL,
**                      then a part component is supplied.
**          compon    = Part component from SolidWorks.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreateUnibase(LPPARTDOC part, LPCOMPONENT2 compon)
{
	LPENUMBODIES2 pBodyEnum = NULL;
	HRESULT hres;
	LPBODY2 pOldBody,pNewBody;
	LPMODELDOC2 modelDoc;
	double *Sparam,attrib[9],b_attrib[9],pdat[17],ev1[9],ev2[9],tdat[17];
	double Srange[11];
	int *Siparam,ipt[5],rpt[3],inc,i,prog;
	long Ssize,count;
	VARIANT vdat;
	VARIANT_BOOL lstat;
	SAFEARRAY *psa;
	NCLX_mdl_trimsf tsf;
	NCLX_mdl_surf surf;
	LPMATHTRANSFORM xform,xform1;
	int pbody_t,enumf_t,nextf_t,trimcv_t,tsfc_t;
	UM_int4 key;
	UM_int2 primtyp;
	UM_real8 primdata[16];
	int prim_t,attr_t,store_t,free_t,facer_t,bodyr_t,setup_t,cvsize_t,ssf_t[10];
#ifdef DEBUG_TIME
	int sm,ss,s,m,t,start_s,start_m;
	FILE *fd;
#define START_TIME gtimx(&ss,&sm)
#define END_TIME(tvar) gtimx(&s,&m); tvar = tvar + (s-ss)*1000 + (m-sm)
#else
#define START_TIME
#define END_TIME
#endif
//
//...Initialize routine
//
	if (part != NULL)
	{
		hres = part->EnumBodies3(swAllBodies,FALSE,&pBodyEnum);
		if (pBodyEnum == NULL) return;
		hres = part->get_IMaterialPropertyValues(b_attrib);
	}
	else
	{
		hres = compon->EnumBodies(swAllBodies,&pBodyEnum);
		if (pBodyEnum == NULL) return;
		hres = compon->get_IMaterialPropertyValues(b_attrib);
		if (hres != S_OK)
		{
			hres = compon->IGetModelDoc(&modelDoc);
			if (hres == S_OK)
			{
				modelDoc->get_IMaterialPropertyValues(b_attrib);
				modelDoc->Release();
			}
		}
		hres = compon->get_Transform2(&xform);
		if (hres == S_OK) hres = xform->IInverse(&xform1);
	}
//
//...Initialize the Unibase
//
#ifdef DEBUG_TIME
	fd = fopen("c:\\user\\bob\\solidworks\\api\\swunibase\\sw.lis","w");
	pbody_t = 0;
	enumf_t = 0;
	nextf_t = 0;
	trimcv_t = 0;
	tsfc_t = 0;
	prim_t = 0;
	attr_t = 0;
	store_t = 0;
	free_t = 0;
	facer_t = 0;
	bodyr_t = 0;
	setup_t = 0;
	cvsize_t = 0;
	for (i=0;i<10;i++) ssf_t[i] = 0;
	t = 0;
	START_TIME;
	start_s = ss; start_m = sm;
	END_TIME(t);
#endif
//
//...Look through the bodies
//
	START_TIME;
	hres = pBodyEnum->Next(1,&pOldBody,NULL); // 2 bodies per RefSurf
	if (compon != NULL) pOldBody->ApplyTransform(xform,&lstat);
	END_TIME(pbody_t);
	while (hres == S_OK)
	{
		LPENUMFACES2 pFaceEnum = NULL;
		START_TIME;
		hres = pOldBody->IGetProcessedBody(&pNewBody);
		hres = pNewBody->EnumFaces(&pFaceEnum);
		END_TIME(enumf_t);
		LPFACE2 pFace;
		START_TIME;
		hres = pFaceEnum->Next(1,&pFace,&count);
		END_TIME(nextf_t);
//
//...Loop through the faces in each body
//
		while (hres == S_OK)
		{
			LPSURFACE pSurface;
//			BSTR bst;
//			char buf[256];
//			int nc;
//			part->IGetEntityName((LPENTITY)pFace,&bst);
//			nc = wcslen(bst);
//			wcstombs(buf,bst,nc+1);
//			SysFreeString(bst);
//			pFace->GetFaceId(&nc);
				m_faceNum++;

//			LPSURFACE pSurface;
			START_TIME;
//			hres = pFace->IGetTrimCurveSize2(TRUE,FALSE,&Ssize);
Ssize = 10;
			END_TIME(cvsize_t);
			if (Ssize > 0)
			{
				START_TIME;
				hres = pFace->GetTrimCurves2(TRUE,FALSE,&vdat);
				END_TIME(trimcv_t);
				START_TIME;
				psa = V_ARRAY(&vdat);
				hres = SafeArrayAccessData(psa,(void **)&Sparam);
				Siparam = (int *)Sparam;
				hres = pFace->get_IMaterialPropertyValues(attrib);
				if (hres != S_OK)
					for (i=0;i<9;i++) attrib[i] = b_attrib[i];
//
//...Set up pointers to Trimmed Surface data
//...
//...ipt[0] = Number of loops
//...   [1] = Total number of SP curves
//...   [2] = Number of SP curves in each loop
//...   [3] = Order of each curve, Closed Flag
//...         Dimension (2,3), Number of control points
//...   [4] = Number of trimmed surfaces, Current trimmed surface
//...
//...rpt[0] = Knot values
//...rpt[1] = Control points
//...rpt[2] = Surface data
//
				ipt[0] = 0;
				ipt[1] = 1;
				ipt[2] = 2;
				ipt[3] = ipt[2] + (Siparam[ipt[0]]+1)/2*2;

				rpt[0] = (ipt[3]+Siparam[ipt[1]]*4) / 2;
				rpt[1] = rpt[0];
				inc = ipt[3];
				for (i=0;i<Siparam[1];i++)
				{
					rpt[1] = rpt[1] + Siparam[inc] + Siparam[inc+3];
					inc = inc + 4;
				}
				rpt[2] = rpt[1];
				inc = ipt[3];
				for (i=0;i<Siparam[ipt[1]];i++)
				{
					rpt[2] = rpt[2] + Siparam[inc+3] * Siparam[inc+2];
					inc = inc + 4;
				}
				ipt[4] = (rpt[2]-1) * 2;
				END_TIME(setup_t);
//
//...Get the surface type
//
				START_TIME;
				pdat[0] = NCLX_UNKNOWN;
				hres = pFace->IGetSurface(&pSurface);
				pSurface->IsCone(&lstat);
				if (lstat)
				{
					pSurface->get_IConeParams(tdat);
					pdat[0] = NCLX_CONE;
					tdat[0] = tdat[0] / .0254;
					tdat[1] = tdat[1] / .0254;
					tdat[2] = tdat[2] / .0254;
					tdat[6] = tdat[6] / .0254;
					ev1[0] = tdat[0] + tdat[3];
					ev1[1] = tdat[1] + tdat[4];
					ev1[2] = tdat[2] + tdat[5];
					NclxMdlGetCone(tdat,ev1,&pdat[1]);
				}
				pSurface->IsCylinder(&lstat);
				if (lstat)
				{
					pSurface->get_ICylinderParams(&pdat[1]);
					pdat[0] = NCLX_CYLINDER;
					pdat[1] = pdat[1] / .0254;
					pdat[2] = pdat[2] / .0254;
					pdat[3] = pdat[3] / .0254;
					pdat[7] = pdat[7] / .0254;
					hres = pFace->IGetUVBounds(Srange);
					hres = pSurface->IEvaluate(Srange[0],Srange[2],0,0,ev1);
					hres = pSurface->IEvaluate(Srange[0],Srange[3],0,0,ev2);
					pdat[8] = um_dcccc(ev1,ev2) / .0254;
				}
				pSurface->IsPlane(&lstat);
				if (lstat)
				{
					pSurface->get_IPlaneParams(&pdat[1]);
					pdat[0] = NCLX_PLANE;
					pdat[7] = pdat[6] / .0254;
					pdat[6] = pdat[5] / .0254;
					pdat[5] = pdat[4] / .0254;
					pdat[4] = um_dot(&pdat[1],&pdat[5]);
				}
				pSurface->IsSphere(&lstat);
				if (lstat)
				{
					pSurface->get_ISphereParams(&pdat[1]);
					pdat[0] = NCLX_SPHERE;
					pdat[1] = pdat[1] / .0254;
					pdat[2] = pdat[2] / .0254;
					pdat[3] = pdat[3] / .0254;
					pdat[4] = pdat[4] / .0254;
				}
				pSurface->Release(); // New
				END_TIME(prim_t);
//
//...Create the trimmed surface structure
//
				tsf.surf = &surf;
				START_TIME;
				CreateTrimSf(&tsf,Sparam,pdat,ipt,rpt,&m_faceNum);
				END_TIME(tsfc_t);
//
//...Define the entity attributes
//
				START_TIME;
				CreateAttrib(&tsf,attrib);
				END_TIME(attr_t);
//
//...Store the trimmed surface
//
				START_TIME;
				NclxMdlStoreTrimSurf(&tsf,NCLX_FALSE,ssf_t);
				if (tsf.header.key && tfmatp) uy_update_tfmat(tsf.header.key,tfmatp);
				END_TIME(store_t);
//
//...Create and store 3-D curves
//
				START_TIME;
				if (m_swopt.import_curves) CreateCurves(&tsf,&m_curveNum);
				END_TIME(free_t);
//
//...Analyze surface if primitive type is unknown.
//
				if(pdat[0] == NCLX_UNKNOWN)
				{
					key = tsf.surf->header.key;
					ncl_sf_prim_analyz(&key,&primtyp,primdata);						
				}
//
//...Free the trimmed surface memory
//
				FreeTrimSf(&tsf);
				hres = SafeArrayDestroy(psa);
				VariantClear(&vdat); // New
			}
//
//...Get the next face
//
			START_TIME;
			pFace->Release();
			END_TIME(facer_t);
			pFace = NULL;
			if (m_statDlg->GetInterrupt())
				hres = S_OK + 1;
			else
			{
				START_TIME;
				hres = pFaceEnum->Next(1,&pFace,NULL);
				END_TIME(nextf_t);
				prog = (int)(((double)m_faceNum / (double)m_srfCount) * 100);
				m_statDlg->ShowProgress(prog);
			}
		}
//
//...Get the next body
//
		START_TIME;
		if (pFaceEnum != NULL) pFaceEnum->Release();
		END_TIME(facer_t);

		START_TIME;
		if (compon != NULL) pOldBody->ApplyTransform(xform1,&lstat);
		pOldBody->Release();
		pNewBody->Release();
		END_TIME(bodyr_t);
		pOldBody = NULL;
		if (m_statDlg->GetInterrupt())
			hres = S_OK + 1;
		else
		{
			START_TIME;
			hres = pBodyEnum->Next(1,&pOldBody,NULL);
			END_TIME(pbody_t);
		}
	}
//
//...Free the allocated memory
//
	if (part == NULL) // New
	{
		xform->Release();
		xform1->Release();
	}
	START_TIME;
	if (pBodyEnum != NULL) pBodyEnum->Release();
	END_TIME(bodyr_t);
//
//...Save the Unibase
//
#ifdef DEBUG_TIME
	fprintf(fd,"pBody Next = %d\n",pbody_t);
	fprintf(fd,"pBody Release = %d\n",bodyr_t);
	fprintf(fd,"pFace Enum = %d\n",enumf_t);
	fprintf(fd,"pFace Next = %d\n",nextf_t);
	fprintf(fd,"pFace Release = %d\n",facer_t);
	fprintf(fd,"Get Curve Size = %d\n",cvsize_t);
	fprintf(fd,"Get Curve = %d\n",trimcv_t);
	fprintf(fd,"Setup data = %d\n",setup_t);
	fprintf(fd,"Create Surface = %d\n",tsfc_t);
	fprintf(fd,"Surface Primitive = %d\n",prim_t);
	fprintf(fd,"Attributes = %d\n",attr_t);
	fprintf(fd,"Store Surface = %d\n",store_t);
	fprintf(fd,"Store Curve = %d\n",free_t);
	fprintf(fd,"----------------------------\n");
	fprintf(fd,"Inquire = %d\n",ssf_t[0]);
	fprintf(fd,"Surface = %d\n",ssf_t[1]);
	fprintf(fd,"Outer Boundary = %d\n",ssf_t[2]);
	fprintf(fd,"Inner Boundary = %d\n",ssf_t[3]);
	fprintf(fd,"Create Geometry = %d\n",ssf_t[4]);
	fprintf(fd,"Fixed Data = %d\n",ssf_t[5]);
	fprintf(fd,"Variable Data = %d\n",ssf_t[6]);
	fprintf(fd,"Attributes = %d\n",ssf_t[7]);
	fprintf(fd,"----------------------------\n");
	t = 0;
	fprintf(fd,"CreateSwitch = %d\n",GeomTs[0]);
	fprintf(fd,"CreateData = %d\n",GeomTs[1]);
	fprintf(fd,"CreateTf = %d\n",GeomTs[2]);
	fprintf(fd,"CreateAttr = %d\n",GeomTs[3]);
	fprintf(fd,"CreateView = %d\n",GeomTs[4]);
	fprintf(fd,"ViewPkt = %d\n",GeomTs[5]);
	fprintf(fd,"ViewTuple = %d\n",GeomTs[6]);
	fprintf(fd,"ViewNotify = %d\n",GeomTs[7]);
	START_TIME;
	t = (s-start_s)*1000 + (m-start_m);
	fprintf(fd,"Total Time = %d\n",t);
	fclose(fd);
#endif
//done:;
}

/*********************************************************************
**    I_FUNCTION     :  CountSurfs(part,comproot)
**       Counts the number of surfaces to be converted.
**    PARAMETERS
**       INPUT  :
**          part      = Part document from SolidWorks.  If set to NULL,
**                      then an assembly is output.
**          comproot  = Root component from SolidWorks.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CountSurfs(LPPARTDOC part,LPCOMPONENT2 comproot)
{
	LPCOMPONENT2 *compon;
	LPENUMBODIES2 pBodyEnum = NULL;
	HRESULT hres;
	LPBODY2 pOldBody,pNewBody;
	LPENUMFACES2 pFaceEnum = NULL;
	int nchild,i;
	long faceNum = 0;
	long nc;
//
//...Initialize routine
//
	if (comproot != NULL)
	{
		comproot->IGetChildrenCount(&nchild);
		if (nchild == 0) return;
		compon = new LPCOMPONENT2[nchild];
		comproot->IGetChildren(compon);
	}
	else
	{
		if (part == UU_NULL) return;
		nchild = 1;
	}
//
//...Get assembly component
//
	for (i=0;i<nchild;i++)
	{
//
//...Get enumerated bodies
//
		if (part != NULL) 
			hres = part->EnumBodies3(swAllBodies,FALSE,&pBodyEnum);
		else
			hres = compon[i]->EnumBodies(swAllBodies,&pBodyEnum);
		if (pBodyEnum == NULL) continue;
//
//...Loop through the bodies
//
		hres = pBodyEnum->Next(1,&pOldBody,&nc); // 2 bodies per RefSurf
		if (nc != 0)
		{
			hres = pOldBody->IGetProcessedBody(&pNewBody);
			hres = pNewBody->GetFaceCount(&faceNum);
			pOldBody->Release();
			pNewBody->Release();
			m_srfCount = m_srfCount + faceNum;
		}
//
//...Release enumerated body
//
		if (pBodyEnum != NULL) pBodyEnum->Release();
//
//...Get next assembly
//
		if (comproot != NULL) compon[i]->Release();
	}
//
//...End of routine
//
	if (comproot != NULL && nchild != 0) delete [] compon;
	return;
}

/*********************************************************************
**    I_FUNCTION     :  CreateTrimSf(tsf,Sparam,pdat,ipt,rpt,sub)
**       Creates a trimmed surface structure, along with the underlying
**       base surface and boundary curves.
**    PARAMETERS
**       INPUT  :
**          Sparam    = Surface parameters.
**          pdat      = Primitive data if surface is a primitive (Sphere, etc.).
**          ipt       = Pointer to integer surface data within 'Sparam'.
**          rpt       = Pointer to real surface data within 'Sparam'.
**          sub       = Subscript number for surface.
**       OUTPUT :
**          tsf       = Trimmed surface structure.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreateTrimSf(NCLX_mdl_trimsf *tsf, double *Sparam,
	double *pdat, int *ipt, int *rpt, int *sub)
{
	char sflab[64];
	int subscr;
//
//...Initialize the structure
//
	if (m_swopt.label_opts == 1)
	{
		NclxMdlLabelEntity(sflab,sub,1,"QZ");
		subscr = *sub;
	}
	else
	{
		NclxMdlLabelEntity(sflab,sub,m_swopt.sflab_type,m_swopt.sflab);
		subscr = *sub;
		if (m_swopt.sflab_type == 0) subscr = 0;
	}
	NclxMdlSetupData(&(tsf->header),NCLX_MDL_TRIMSF,sflab,subscr);
	tsf->xyz_cv = 0;
	tsf->offdist = 0.;
	tsf->trim_type = NCLX_FACE;
	tsf->no_boxlst = 0;
	tsf->boxlst = 0;
	tsf->no_bndrylst = 0;
	tsf->bndrylst = 0;
//
//...Define the base surface structure
//
	CreateBaseSf(tsf,&Sparam[rpt[2]],pdat);
//
//...Define the trimming curves
//
	CreateTrimCv(tsf,Sparam,ipt,rpt);
//
//...End of routine
//
//done:;
	return;
}

/*********************************************************************
**    I_FUNCTION     :  CreateBaseSf(tsf,Sparam,pdat)
**       Creates a base surface structure.
**    PARAMETERS
**       INPUT  :
**          tsf       = Trimmed surface structure.
**          Sparam    = Surface parameters.
**          pdat      = Primitive data if surface is a primitive (Sphere, etc.).
**       OUTPUT :
**          tsf->surf = Base surface structure.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreateBaseSf(NCLX_mdl_trimsf *tsf,double *Sparam,
	double *pdat)
{
	int inc,iend,nc,i,j,k,pt;
	int *Siparam;
	NCLX_mdl_surf *surf;
//
//...Initialize routine
//
	Siparam = (int *)Sparam;
	surf = tsf->surf;
//
//...Initialize the surface structure
//
	NclxMdlSetupData(&(surf->header),NCLX_MDL_NSURF,"@UN",0);
	surf->sfhead.material = 0;
	surf->sfhead.upaths = surf->sfhead.vpaths = 5;
	surf->sfhead.upts = surf->sfhead.vpts = 0;
	surf->sfhead.urld = surf->sfhead.vrld = NCLX_FALSE;
	surf->sfhead.reverse = NCLX_FALSE;
	surf->sfhead.uclosed = Siparam[6];
	surf->sfhead.vclosed = Siparam[7];
	surf->sfhead.eval[0] = surf->sfhead.eval[1] = .5;
	surf->sfhead.offset = NCLX_FALSE;
	surf->sfhead.offdist = 0.;
	surf->sfhead.type = 0;
	surf->sfhead.udegree = Siparam[2];
	surf->sfhead.vdegree = Siparam[3];
	surf->sfhead.udegseg = Siparam[4] - (surf->sfhead.udegree-1);
	surf->sfhead.vdegseg = Siparam[5] - (surf->sfhead.vdegree-1);

	surf->evaluator = 0;
	surf->npanel = 0;
	surf->panel = 0;
	pt = (int)pdat[0];
	surf->primitive = (NCLX_prim_type)pt;
	for (i=0;i<16;i++) surf->prim_param[i] = pdat[i+1];
	surf->no_boxlst = 0;
	surf->boxlst = 0;
	surf->no_bndrylst = 0;
	surf->bndrylst = 0;
//
//...Create the UKNOT data
//
	inc = 4; nc = Siparam[4] + Siparam[2] ; iend = inc + nc;
	tsf->u_min = Sparam[inc] ; tsf->u_max = Sparam[iend-1];
	surf->ntu = nc;
	surf->tu = (double *)uu_malloc(sizeof(double)*nc);
	for (i=inc;i<iend;i++) surf->tu[i-inc] = Sparam[i];
//
//...Create the VKNOT data
//
	inc = i; nc = Siparam[5] + Siparam[3] ; iend = inc + nc;
	tsf->v_min = Sparam[inc] ; tsf->v_max = Sparam[iend-1];
	surf->ntv = nc;
	surf->tv = (double *)uu_malloc(sizeof(double)*nc);
	for (i=inc;i<iend;i++) surf->tv[i-inc] = Sparam[i];
//
//...Create the POINT data
//
	inc = i; nc = Siparam[4] * Siparam[5] ; iend = inc + nc * Siparam[0];
	surf->npt = nc;
	surf->pt = (double *)uu_malloc(sizeof(double)*nc*Siparam[0]);
	k = 0;
	for (i=inc;i<iend;i=i+Siparam[0])
	{
		for (j=i;j<i+3;j++)
		{
			surf->pt[k] = Sparam[j] / .0254;
			k++;
		}
	}
//
//...Create the WEIGHT data
//
	if (Siparam[0] == 4)
	{
		surf->nwgt = nc;
		surf->wgt = (double *)uu_malloc(sizeof(double)*nc);
		k = 0;
		for (i=inc;i<iend;i=i+Siparam[0])
		{
			surf->wgt[k] = Sparam[i+3];
			k++;
		}
	}
	else
	{
		surf->nwgt = 0;
		surf->wgt = 0;
	}
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    I_FUNCTION     :  CreateTrimCv(tsf,Sparam,ipt,rpt)
**       Creates the boundary curves for a trimmed surface.
**    PARAMETERS
**       INPUT  :
**          tsf         = Trimmed surface structure.
**          Sparam      = Surface parameters.
**          ipt         = Pointer to integer surface data within 'Sparam'.
**          rpt         = Pointer to real surface data within 'Sparam'.
**       OUTPUT :
**          tsf->uv_cv  = Outer boundary curve.
**          tsf->inner  = Inner boundary curves.
**          tsf->ncurve = Number of inner boundary curves.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreateTrimCv(NCLX_mdl_trimsf *tsf, double *Sparam,int ipt[],
	int rpt[])
{
	int inc,iend,nc,i,j,ipt2,ipt3,rpt0,rpt1,ncv,npt,ist,nknot,ncvpt,nwgt;
	int *Siparam,neval;
	double u,cvt,spt[3];
	double *knpt,*cvpt,*wtpt;
	NCLX_mdl_composite *comp;
	NCLX_mdl_curve cvdum,*curve;
	NCLX_mdl_cmp_entity *cvid;
	NCLX_mdl_curve_eval eval;
//
//...Initialize routine
//
	Siparam = (int *)Sparam;
	ipt2 = ipt[2];
	ipt3 = ipt[3];
	rpt0 = rpt[0];
	rpt1 = rpt[1];
//
//...Setup dummy curve for XYZ curves
//
	NclxMdlSetupData(&(cvdum.header),NCLX_MDL_BSPLINE,"@UN",0);
//
//...Allocate memory
//
	tsf->ncurve = (Siparam[ipt[0]]-1)*2;
	tsf->inner = (NCLX_mdl_ptr **)uu_malloc(sizeof(NCLX_mdl_ptr *)*tsf->ncurve);
//
//...Define all Knot, Point, and Weight values
//
	ncv = 0;
	for (inc=0;inc<Siparam[ipt[0]];inc++)
	{
//
//......Allocate memory for composite curve
//
		comp = (NCLX_mdl_composite *)uu_malloc(sizeof(NCLX_mdl_composite));
		if (inc == 0) tsf->uv_cv = (NCLX_mdl_ptr *)comp;
		else
		{
			tsf->inner[ncv] = (NCLX_mdl_ptr *)&cvdum; ncv++;
			tsf->inner[ncv] = (NCLX_mdl_ptr *)comp; ncv++;
		}
//
//......Initialize composite curve structure
//
		NclxMdlSetupData(&(comp->header),NCLX_MDL_COMPOSITE,"@UN",0);
		comp->cvhead.closed = NCLX_TRUE;
		comp->cvhead.length = 0.;
		comp->cvhead.eval = .5;
		comp->cvhead.planar = NCLX_TRUE;
		comp->cvhead.continuity = 0;
		comp->cvhead.fcolor = 0;

		comp->evaluator = 0;
		comp->ncurve = Siparam[ipt2];
		comp->cvid = (NCLX_mdl_cmp_entity *)
			uu_malloc(sizeof(NCLX_mdl_cmp_entity)*Siparam[ipt2]);
//
//......Process curves in loops
//
		for (i=0;i<Siparam[ipt2];i++)
		{
			npt = Siparam[ipt3+2];
//
//.........Allocate memory for curve
//
			cvid = &(comp->cvid[i]);
			curve = (NCLX_mdl_curve *) uu_malloc(sizeof(NCLX_mdl_curve));
			cvid->curve = (NCLX_mdl_ptr *)curve;
			cvid->reverse = NCLX_FALSE;
			cvid->endparam = 0.;

			nknot = Siparam[ipt3] + Siparam[ipt3+3];
			ncvpt = Siparam[ipt3+3];
			if (Siparam[ipt3+2] == 3) nwgt = ncvpt;
			else nwgt = 0;

			knpt = (double *)uu_malloc(sizeof(double)*nknot);
			cvpt = (double *)uu_malloc(sizeof(double)*ncvpt*3);
			if (nwgt != 0)
				wtpt = (double *)uu_malloc(sizeof(double)*Siparam[ipt[0]]);
			else
				wtpt = 0;
//
//......Initialize the curve structure
//
			ist = rpt0; iend = ist + nknot;
			NclxMdlSetupData(&(curve->header),NCLX_MDL_BSPLINE,"@UN",0);
			curve->cvhead.closed = Siparam[ipt3+1];
			curve->cvhead.degree = Siparam[ipt3];
			curve->cvhead.degseg = Siparam[ipt3+3] - (Siparam[ipt3]-1);
			curve->cvhead.t0 = Sparam[ist];
			curve->cvhead.t1 = Sparam[iend-1];
			curve->cvhead.eval = (curve->cvhead.t1 - curve->cvhead.t0) / 2.;
			curve->cvhead.tlen = 0.;

			curve->evaluator = 0;
			curve->nparm = 0;
			curve->parms = 0;
			curve->ntparm = nknot;
			curve->tparms = knpt;
			curve->npt = ncvpt;
			curve->pt = cvpt;
			curve->nwgt = nwgt;
			curve->wgt = wtpt;
			curve->nsegment = 0;
			curve->segment = 0;
//
//......Save the KNOT data
//
			for (j=ist;j<iend;j++) knpt[j-ist] = Sparam[j];
			rpt0 = iend;
//
//......Save the POINT data
//
			ist = rpt1; iend = ist + ncvpt*npt;
			nc = 0;
			for (j=ist;j<iend;j=j+npt)
			{
				cvpt[nc] = Sparam[j];
				cvpt[nc+1] = Sparam[j+1];
				cvpt[nc+2] = 0.;
				nc = nc + 3;
			}
//
//......Save the WEIGHT data
//
			nc = 0;
			if (nwgt != 0)
			{
				for (j=ist;j<iend;j=j+npt)
				{
					wtpt[nc] = Sparam[j+2];
					nc++;
				}
			}
//
//......Get the length of the curve
//
			cvt = curve->cvhead.t1 - curve->cvhead.t0;
			curve->header.key = -1;
			NclxMdlEvalCurve(curve,0.,&eval);
			um_vctovc(eval.pt,spt);
			neval = curve->cvhead.degseg * (curve->cvhead.degree*2);
			for (j=1;j<=neval;j++)
			{
				u = curve->cvhead.t0 + (double)j/neval * cvt;
				NclxMdlEvalCurve(curve,u,&eval);
				cvid->endparam = cvid->endparam + um_dcccc(eval.pt,spt);
				um_vctovc(eval.pt,spt);
			}
			curve->header.key = 0;
			comp->cvhead.length = comp->cvhead.length + cvid->endparam;
/*
.........Increment curve pointer
*/
			rpt1 = iend;
			ipt3 = ipt3 + 4;
		}
/*
.....Set the end parameter for each of
.....the component curves
*/
		cvt = 0.;
		for (i=0;i<Siparam[ipt2];i++)
		{
			cvid = &(comp->cvid[i]);
			cvt = cvt + cvid->endparam;
			cvid->endparam = cvt / comp->cvhead.length;
		}
/*
......Increment loop pointer
*/
		ipt2 = ipt2 + 1;
	}
}

/*********************************************************************
**    I_FUNCTION     :  CreateCurves(tsf,sub)
**       Calculates and stores in the Unibase, curves createt from the
**       boundary curves of a trimmed surface.
**    PARAMETERS
**       INPUT  :
**          tsf      = Trimmed surface structure.
**          sub      = Beginning Subscript for curves.
**       OUTPUT :
**          sub      = Updated subscript for curves.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreateCurves(NCLX_mdl_trimsf *tsf, int *sub)
{
	int inc,sst[10],i,j,neval,subscr,jpt,status;
	int ix2, ix3;
	double u,v,u_min,v_min,u_ratio,v_ratio,cvt,*pts;
	double v0[3], v1[3], d1, r1, co, spt[3];
	int bline, bccv;
	char cvlab[64];
//	NCL_crvgen_rec *cvgen;
	NCLX_KEY_ID cvkey,sfkey;
	NCLX_mdl_circle circ_rec, *circle;
	NCLX_mdl_composite *cpt;
	NCLX_mdl_cmp_entity *cvid;
	NCLX_mdl_curve *uvcv,*curve;
	NCLX_mdl_curve_eval eval;
	NCLX_mdl_surf_eval sfeval;
	NCLX_mdl_line line_rec, *line;
	NCLX_mdl_composite comp_rec;
	UM_line_rec nline;
	UM_circle_rec ncirc;
	NCL_fixed_databag e1;

#define NCVPT 25

	bccv = FALSE;
	if (m_swopt.cvopt == 1) bccv = TRUE;
//
//...Calculate the curve to surface UV mapping
//...based on the u-min & u-max parameters
//
	u_ratio = tsf->u_max - tsf->u_min;
	v_ratio = tsf->v_max - tsf->v_min;
	u_min = tsf->u_min;
	v_min = tsf->v_min;
//
//...Define all curves
//
	for (inc=-1;inc<tsf->ncurve;inc=inc+2)
	{
		if (inc < 0) cpt = (NCLX_mdl_composite *)(tsf->uv_cv);
		else cpt = (NCLX_mdl_composite *)(tsf->inner[inc]);
		if (bccv)
		{
			(*sub)++;
			if (m_swopt.label_opts == 1)
			{
				NclxMdlLabelEntity(cvlab,sub,1,"QZ");
				subscr = *sub;
			}
			else
			{
				NclxMdlLabelEntity(cvlab,sub,m_swopt.cvlab_type,m_swopt.cvlab);
				subscr = *sub;
				if (m_swopt.cvlab_type == 0) subscr = 0;
			}
			NclxMdlSetupData(&(comp_rec.header),NCLX_MDL_COMPOSITE,cvlab,subscr);
			comp_rec.header.attrib.color = NCLX_YELLOW;
			comp_rec.header.attrib.displayable = 0;
			comp_rec.cvhead.closed = NCLX_TRUE;
			comp_rec.cvhead.length = 0.;
			comp_rec.cvhead.eval = .5;
			comp_rec.cvhead.planar = NCLX_FALSE;
			comp_rec.cvhead.continuity = 0;
			comp_rec.cvhead.fcolor = 0;

			comp_rec.evaluator = 0;
			comp_rec.ncurve = cpt->ncurve;
			comp_rec.cvid = (NCLX_mdl_cmp_entity *)
				uu_malloc(sizeof(NCLX_mdl_cmp_entity)*cpt->ncurve);
		}
//
//...Process curves in loops
//
		for (i=0;i<cpt->ncurve;i++)
		{
			cvid = &(cpt->cvid[i]);
			uvcv = (NCLX_mdl_curve *)cvid->curve;
//
//......Calculate XYZ points on curve
//
			neval = uvcv->cvhead.degseg * (uvcv->cvhead.degree*2);
			j = neval+1;
			if (j<NCVPT) j = NCVPT;
			pts = (double *) uu_malloc(j*3*sizeof(double));
			cvt = uvcv->cvhead.t1 - uvcv->cvhead.t0;
			cvkey = uvcv->header.key;
			sfkey = tsf->surf->header.key;
			uvcv->header.key = -1;
			NclxMdlEvalCurve(uvcv,0.,&eval);
			u = (eval.pt[0] - u_min) / u_ratio;
			v = (eval.pt[1] - v_min) / v_ratio;
			NclxMdlEvalSurf(tsf->surf,u,v,&sfeval,1);
			um_vctovc(sfeval.pt,&pts[0]);
			for (j=1,jpt=3;j<=neval;j++,jpt=jpt+3)
			{
				u = uvcv->cvhead.t0 + (double)j/neval * cvt;
				NclxMdlEvalCurve(uvcv,u,&eval);
				u = (eval.pt[0] - u_min) / u_ratio;
				v = (eval.pt[1] - v_min) / v_ratio;
				NclxMdlEvalSurf(tsf->surf,u,v,&sfeval,1);
				um_vctovc(sfeval.pt,&pts[jpt]);
			}

			uvcv->header.key = cvkey;
			tsf->surf->header.key = sfkey;

			um_vcmnvc(&pts[3],pts,v0);
			um_unitvc(v0,v0);
			bline = TRUE;
			for (j=1,jpt=3;j<neval && bline;j++,jpt+=3)
			{
				um_vcmnvc(&pts[jpt+3],&pts[jpt],v1);
				um_unitvc(v1,v1);
				if (um_mag(v1) > 1.0e-6 && um_dot(v1,v0) < .9999) bline = FALSE;
			}

			if (bline)
			{
				if (bccv)
				{
					strcpy(cvlab,"@UN");
					subscr = 0;
					line = (NCLX_mdl_line *) uu_malloc(sizeof(NCLX_mdl_line));
					line->header.attrib.displayable = 1;
				}
				else
				{
					(*sub)++;
					if (m_swopt.label_opts == 1)
					{
						NclxMdlLabelEntity(cvlab,sub,1,"QZ");
						subscr = *sub;
					}
					else
					{
						NclxMdlLabelEntity(cvlab,sub,m_swopt.cvlab_type,m_swopt.cvlab);
						subscr = *sub;
						if (m_swopt.cvlab_type == 0) subscr = 0;
					}
					line_rec.header.attrib.displayable = 0;
					line = &line_rec;
				}
				NclxMdlSetupData(&(line->header),NCLX_MDL_LINE,cvlab,subscr);
				line->header.attrib.color = NCLX_GREEN;
				um_vctovc(pts,line->spt);
				um_vctovc(&pts[neval*3],line->ept);
				if (tfmatp)
				{
					um_cctmtf(line->spt,tfmatp,line->spt);
					um_cctmtf(line->ept,tfmatp,line->ept);
				}
				if (bccv)
				{
					cvid = &(comp_rec.cvid[i]);
					cvid->curve = line;
					cvid->reverse = NCLX_FALSE;
					cvid->endparam = um_dcccc(line->ept,line->spt);
					comp_rec.cvhead.length += cvid->endparam;
				}
				else
				{
					if (m_swopt.cvopt == 2)
					{
						um_vctovc(line->spt,nline.spt);
						um_vctovc(line->ept,nline.ept);
						if (uig_match_line(&nline,0) != NCLX_SUCCESS)
						{
							NclxMdlStoreLine(line,NCLX_TRUE);
						}
						else
						{
							(*sub)--;
						}
					}
					else
						NclxMdlStoreLine(line,NCLX_TRUE);
				}
				goto loop;
			}

			ix2 = neval/2 - 1;
			if (ix2<=0) ix2=1;
			ix3 = ix2*2;
			if (ix3>=neval) ix3--;
			if (ix3<=ix2) ix3=ix2+1;
			ix2*=3;
			ix3*=3;

			status = uy_circ_3pts(pts,&pts[ix2],&pts[ix3],&circ_rec);
			if (status == NCLX_SUCCESS)
			{
				d1 = um_dot(circ_rec.center,circ_rec.nvec);
				r1 = circ_rec.radius;
				for (j=1,jpt=3;j<=neval && status == NCLX_SUCCESS;j++,jpt+=3)
				{
					if (fabs(d1-um_dot(&pts[jpt],circ_rec.nvec)) > .0001 ||
						fabs(r1-um_dcccc(&pts[jpt],circ_rec.center)) > .0001)
						status = NCLX_FAILURE;
				}
			}
			if (status == NCLX_SUCCESS)
			{
				if (bccv)
				{
					strcpy(cvlab,"@UN");
					subscr = 0;
					circle = (NCLX_mdl_circle *) uu_malloc(sizeof(NCLX_mdl_circle));
					um_vctovc(circ_rec.center,circle->center);
					um_vctovc(circ_rec.nvec,circle->nvec);
					circle->radius = circ_rec.radius;
					circle->header.attrib.displayable = 1;
				}
				else
				{
					(*sub)++;
					if (m_swopt.label_opts == 1)
					{
						NclxMdlLabelEntity(cvlab,sub,1,"QZ");
						subscr = *sub;
					}
					else
					{
						NclxMdlLabelEntity(cvlab,sub,m_swopt.cvlab_type,m_swopt.cvlab);
						subscr = *sub;
						if (m_swopt.cvlab_type == 0) subscr = 0;
					}
					circ_rec.header.attrib.displayable = 0;
					circle = &circ_rec;
				}
				NclxMdlSetupData(&(circle->header),NCLX_MDL_CIRCLE,cvlab,subscr);
				circle->header.attrib.color = NCLX_BLUE;
				um_vcmnvc(pts,circle->center,v1);
				um_unitvc(v1,circle->svec);
				if (um_dcccc(pts,&pts[neval*3]) < .0001)
					circle->dang = TWOPI;
				else
				{
					um_vcmnvc(&pts[neval*3],circle->center,v0);
					um_unitvc(v0,v0);
					co = um_dot(circle->svec,v0);
					if (co > 1.0) co = 1.0;
					if (co < -1.0) co = -1.0;
					circle->dang = acos(co);
					um_cross(circle->svec,v0,v1);
					if (um_dot(v1,circle->nvec) < 0.0) circle->dang = TWOPI - circle->dang;
				}
				if (tfmatp)
				{
					um_cross(circle->nvec,circle->svec,v0);
					um_vctmtf(v0,tfmatp,v0);
					um_cctmtf(circle->center,tfmatp,circle->center);
					um_vctmtf(circle->nvec,tfmatp,circle->nvec);
					um_unitvc(circle->nvec, circle->nvec);
					um_vctmtf(circle->svec,tfmatp,circle->svec);
					circle->radius *= um_mag(circle->svec);
					um_unitvc(circle->svec, circle->svec);
					um_cross(circle->nvec,circle->svec,v1);
					if (um_dot(v0,v1) < 0.0) circle->dang = -circle->dang;
				}
				if (bccv)
				{
					cvid = &(comp_rec.cvid[i]);
					cvid->curve = circle;
					cvid->reverse = NCLX_FALSE;
					cvid->endparam = circle->radius * circle->dang;
					comp_rec.cvhead.length += cvid->endparam;
				}
				else
				{
					if (m_swopt.cvopt == 2)
					{
						um_vctovc(circle->center,ncirc.center);
						um_vctovc(circle->nvec,ncirc.nvec);
						um_vctovc(circle->svec,ncirc.svec);
						ncirc.radius = circle->radius;
						ncirc.dang   = circle->dang;
						if (uig_match_circle(&ncirc,0) != NCLX_SUCCESS)
						{
							NclxMdlStoreCircle(circle,NCLX_TRUE);
						}
						else
						{
							(*sub)--;
						}
					}
					else
					{
						NclxMdlStoreCircle(circle,NCLX_TRUE);
					}
				}
				goto loop;
			}
//
//......Allocate memory for curve
//
			curve = (NCLX_mdl_curve *) uu_malloc(sizeof(NCLX_mdl_curve));
//
//......Calculate knot & control points for curve
//
			status = NclxMdlInterpCurve(curve,pts,neval+1);
//
//......Store curve data
//
			if (status == NCLX_SUCCESS)
			{
				if (bccv)
				{
					strcpy(cvlab,"@UN");
					subscr = 0;
					curve->header.attrib.displayable = 1;
				}
				else
				{
					*sub = *sub + 1;
					if (m_swopt.label_opts == 1)
					{
						NclxMdlLabelEntity(cvlab,sub,1,"QZ");
						subscr = *sub;
					}
					else
					{
						NclxMdlLabelEntity(cvlab,sub,m_swopt.cvlab_type,m_swopt.cvlab);
						subscr = *sub;
						if (m_swopt.cvlab_type == 0) subscr = 0;
					}
					curve->header.attrib.displayable = 0;
				}
				NclxMdlSetupData(&(curve->header),NCLX_MDL_BSPLINE,cvlab,subscr);
				(*curve).header.attrib.color = NCLX_CYAN;
				curve->cvhead.closed = uvcv->cvhead.closed;
				if (tfmatp)
				{
					for (jpt=0;jpt<curve->npt*3;jpt+=3)
					{
						um_cctmtf(&curve->pt[jpt],tfmatp,&curve->pt[jpt]);
					}
				}
//
//...Store the curve in the Unibase
//
				if (bccv)
				{
					cvid = &(comp_rec.cvid[i]);
					cvid->curve = curve;
					cvid->reverse = NCLX_FALSE;
					cvid->endparam = 0.0;
					cvt = curve->cvhead.t1 - curve->cvhead.t0;
					cvkey = curve->header.key;
					curve->header.key = -1;
					NclxMdlEvalCurve(curve,0.,&eval);
					um_vctovc(eval.pt,spt);
					neval = curve->cvhead.degseg * (curve->cvhead.degree*2);
					for (j=1;j<=neval;j++)
					{
						u = curve->cvhead.t0 + (double)j/neval * cvt;
						NclxMdlEvalCurve(curve,u,&eval);
						cvid->endparam += um_dcccc(eval.pt,spt);
						um_vctovc(eval.pt,spt);
					}
					curve->header.key = cvkey;
					comp_rec.cvhead.length += cvid->endparam;
				}
				else
				{
					if (m_swopt.cvopt == 2)
					{
						curve->header.key = -1;
						for (j=0,jpt=0;j<NCVPT;j++,jpt=jpt+3)
						{
							u = (UU_REAL)j/((UU_REAL)NCVPT-1.);
							NclxMdlEvalCurve(curve,u,&eval);
							um_vctovc(eval.pt,&pts[jpt]);
						}
						curve->header.key = 0;
						e1.key = 0;
						if (uig_match_rbsplcrv2((UM_rbsplcrv_rec *)&e1,pts,0) != NCLX_SUCCESS)
						{
							NclxMdlStoreCurve(curve,NCLX_TRUE,sst);
						}
						else
						{
							(*sub)--;
						}
					}
					else
					{
						NclxMdlStoreCurve(curve,NCLX_TRUE,sst);
					}
				}
			}
//
//...Free the memory
//
			if (!bccv && curve)
			{
				uu_free(curve->tparms);
				uu_free(curve->pt);
				uu_free(curve);
			}
loop:;
			uu_free(pts);
		}
		if (bccv)
		{
			cvt = 0.0;
			for (i=0;i<cpt->ncurve;i++)
			{
				cvid = &comp_rec.cvid[i];
				cvt += cvid->endparam;
				cvid->endparam = cvt / comp_rec.cvhead.length;
			}

			NclxMdlStoreComposite(&comp_rec,NCLX_TRUE,sst);

			for (i=0;i<cpt->ncurve;i++)
			{
				curve = (NCLX_mdl_curve *)comp_rec.cvid[i].curve;
				if (curve)
				{
					if (curve->header.relnum == NCLX_MDL_BSPLINE)
					{
						uu_free(curve->tparms);
						uu_free(curve->pt);
					}
					uu_free(curve);
				}
			}
			if (comp_rec.cvid) uu_free (comp_rec.cvid);
		}
	}
}

/*********************************************************************
**    I_FUNCTION     :  FreeTrimSf(tsf)
**       Frees the trimmed surface storage.
**    PARAMETERS
**       INPUT  :
**          tsf      = Trimmed surface structure.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::FreeTrimSf(NCLX_mdl_trimsf *tsf)
{
	int i,j;
	NCLX_mdl_composite *comp;
	NCLX_mdl_curve *curve;
	NCLX_mdl_surf *surf;
//
//...Free the surface data
//
	surf = (NCLX_mdl_surf *)tsf->surf;
	uu_free(surf->tu);
	uu_free(surf->tv);
	uu_free(surf->pt);
	if (surf->nwgt != 0) uu_free(surf->wgt);
//
//...Free the boundary curves
//
	comp = (NCLX_mdl_composite *)tsf->uv_cv;
	for (j=0;j<comp->ncurve;j++)
	{
		curve = (NCLX_mdl_curve *)((comp->cvid[j]).curve);
		uu_free(curve->tparms);
		uu_free(curve->pt);
		if (curve->wgt != 0) uu_free(curve->wgt);
		uu_free(comp->cvid[j].curve);
	}
	uu_free(comp->cvid);
	uu_free(tsf->uv_cv);

	for (i=1;i<tsf->ncurve;i=i+2)
	{
		comp = (NCLX_mdl_composite *)tsf->inner[i];
		for (j=0;j<comp->ncurve;j++)
		{
			curve = (NCLX_mdl_curve *)((comp->cvid[j]).curve);
			uu_free(curve->tparms);
			uu_free(curve->pt);
			if (curve->wgt != 0) uu_free(curve->wgt);
			uu_free(comp->cvid[j].curve);
		}
		uu_free(comp->cvid);
		uu_free(tsf->inner[i]);
	}
	uu_free(tsf->inner);
}

/*********************************************************************
**    I_FUNCTION     :  CreateAttrib(tsf,attrib)
**       Stores the trimmed surface display attributes.
**    PARAMETERS
**       INPUT  :
**          attrib   = Display attributes from SolidWorks.
**       OUTPUT : 
**          tsf      = Trimmed surface structure to receive the attributes.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CreateAttrib(NCLX_mdl_trimsf *tsf,double *attrib)
{
	int i,icol;
	double dx,dy,dz,d,dsav,trans;

	static double color[16][3]={
		0.,0.,0.,      // Black
		1.,1.,1.,      // White
		0.,0.,1.,      // Blue
		1.,0.,0.,      // Red
		0.,1.,0.,      // Green
		1.,0.,1.,      // Magenta
		1.,1.,0.,      // Yellow
		0.,1.,1.,      // Cyan
		.72,.53,.04,   // Brown
		.82,.71,.55,   // Tan
		.68,.85,.90,   // Lt Blue
		.33,1.,.62,    // Sea Green
		1.,.65,0.,     // Orange
		1.,.76,.80,    // Pink
		.87,.63,.87,   // Plum
		.75,.75,.75};   // Grey
//
//...Find closest color match
//
	icol = 15;
	dsav = 1000.;
	for (i=0;i<16;i++)
	{
		dx = attrib[0]-color[i][0];
		dy = attrib[1]-color[i][1];
		dz = attrib[2]-color[i][2];
		d = dx*dx + dy*dy + dz*dz;
		if (d < dsav)
		{
			dsav = d;
			icol = i;
		}
	}
//
//...Set translucency
//
	trans = (1.-attrib[7]) * 100.;
//
//...Set attributes
//
	(*tsf).header.attrib.color = icol;
	((*tsf).surf)->sfhead.lucency = (int)trans;
	((*tsf).surf)->sfhead.shaded = m_swopt.shade_surfs;
	tsf->header.attrib.displayable = 0;
}

/*********************************************************************
**    I_FUNCTION     :  CountMatches(level,matches)
**       Updates the match count when using a secondary Unibase to label
**       entities for each level and displays the results in the status
**       window.  The progress bar is also updated.
**    PARAMETERS
**       INPUT  :
**          level    = Level of matching performed.
**          matches  = Number of matches found at this level.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::CountMatches(int level, int matches)
{
	int prog;
	char sbuf[80];
//
//...Level Changed
//
	if (level != my_this->m_nlevel)
	{
		if (level == 0)
		{
			(*my_this).m_statDlg->AddList("Performing Exact Match...");
			sprintf(sbuf,"\nExact Match\n");
			(*my_this).list_file_out(sbuf);
		}
		else
		{
			if (matches != my_this->m_nmatch)
			{
				sprintf(sbuf,"...%d Matches",matches);
				(*my_this).m_statDlg->AddList(sbuf);
				(*my_this).list_file_out(sbuf);
				(*my_this).list_file_out("\n\n");
			}
			else
			{
				sprintf(sbuf,"...No Matches");
				(*my_this).m_statDlg->AddList(sbuf);
				(*my_this).list_file_out(sbuf);
				(*my_this).list_file_out("\n\n");
			}

			if (level == -1)
			{
				sprintf(sbuf,"Labeling Remaining %d Entities",
					(*my_this).m_srfCount-matches);
				(*my_this).m_statDlg->AddList(sbuf);
				sprintf(sbuf,"Unmathced Entities\n");
				(*my_this).list_file_out(sbuf);
				sprintf(sbuf,"...%d\n",
					(*my_this).m_srfCount-matches);
				(*my_this).list_file_out(sbuf);
			}
			else
			{
				sprintf(sbuf,"Performing Level %d Match...",level);
				(*my_this).m_statDlg->AddList(sbuf);
				sprintf(sbuf,"Level %d Match\n",level);
				(*my_this).list_file_out(sbuf);
			}
		}
		(*my_this).m_nlevel = level;
		(*my_this).m_nmatch = matches;
	}
//
//...Show progress
//
	prog = (int)(((double)matches / (double)(*my_this).m_srfCount) * 100);
	(*my_this).m_statDlg->ShowProgress(prog);
}

/*********************************************************************
**    I_FUNCTION     :  GetInterrupt(stat)
**       Determines if user interrupted the Unibase creation.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          stat     = 1 = User interrupted the transfer.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::GetInterrupt(int *stat)
{
	*stat = (*my_this).m_statDlg->GetInterrupt();
	return;
}

/*********************************************************************
**    I_FUNCTION     :  ListOut(sbuf)
**       Outputs a line to the status window.
**    PARAMETERS
**       INPUT  :
**          sbuf     = Line to output to status window.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::ListOut(char *sbuf)
{
	(*my_this).m_statDlg->AddList(sbuf);
}

/*********************************************************************
**    I_FUNCTION     :  open_listing_file(ifname,ofname)
**          Initialize output listing file.
**    PARAMETERS   
**       INPUT  : 
**          ifname  					 input file name
**          ofname  					 output file name
**       OUTPUT :  
**          none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::open_listing_file(char ifname[],char ofname[])
{
	int i;
	char inputname[62];
	UX_pathname listname,filename,dir;
	double ver;
/*
.....Create listing file name
*/
	ul_break_fname(ofname, dir, filename);
	i = strcspn(filename, ".");
	if (i != 0) filename[i] = '\0';
	ul_build_full_fname(dir,filename,"lst",listname);
/*
.....Create listing file
*/
//	_sopen_s(&listfd,listname, _O_CREAT|_O_TRUNC,_SH_DENYNO,_S_IREAD|S_IWRITE);
//	listfd = _creat(listname, 0664);
	listfd = fopen(listname,"w+");
	ul_short_filename(ifname,inputname,60);
	if (listfd == NULL)
	{
		printf("Error open listing file %s\n", inputname);
	}
	getver(&ver);
	sprintf(p_buff,"NCL/Solid  Conversion Program Version %.3f\n\n",ver);
	(*my_this).list_file_out(p_buff);								
	ul_short_filename(ifname,inputname,60);
	sprintf(p_buff, "Input File: %s\n", inputname);
	(*my_this).list_file_out(p_buff);
	ul_short_filename(ofname,inputname,60);
	sprintf(p_buff, "Output File: %s\n\n", inputname);
	(*my_this).list_file_out(p_buff);
}

/*********************************************************************
**    I_FUNCTION     :  close_listing_file()
**          Close output listing file.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**          none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::close_listing_file()
{
/*
.....Close listing file
*/
	fclose(listfd);
}

/*********************************************************************
**    I_FUNCTION     :  list_file_out(line)
**          Write line to listing file.
**    PARAMETERS   
**       INPUT  : 
**          line						string to write to file 
**       OUTPUT :  
**          none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNclUnibase::list_file_out(char line[])	
{
   int n;
	int ll;
   char buff[400];  

   if (listfd != NULL)
	{
		strcpy(buff, line);
		n = strlen(line);
		if (buff[n-1] == '\n')
		{
			n--;
			if(ll = strlen(save_str) > 0)
			{
				strcat(save_str, buff);
				ll = strlen(save_str);
				fwrite(save_str,sizeof(char),ll,listfd);
				save_str[0] = '\0';
			}
			else
			{
				if (n > 0)
				{
//					for (i=n;i<80;i++)
//					buff[i] = ' ';
//					buff[80] = '\n';
					ll = strlen(buff);
					fwrite(buff,sizeof(char),ll,listfd);
					save_str[0] = '\0';
				}
			}
		}
		else
		{
			strcpy(save_str, buff);
		}
	}
}
