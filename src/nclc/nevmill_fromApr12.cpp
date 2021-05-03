/********************************************************************* 
**  NAME:  nevmill.cpp
**
**			Interface routines to the VoluMill libraries.
**	CONTAINS: 
**       ncl_nvmill_reset_calls()
**			nclf_vmill_pocket()
**			nclf_vmill_pocket3()
**       nclf_vm_push_entry()
**       nclf_vm_clpath_getnpaths()
**       nclf_vm_clpath_gettype()
**       nclf_vm_clpath_getfeed()
**       nclf_vm_clpath_getspindle()
**       nclf_vm_clpath_getstartpt()
**       nclf_vm_clpath_getpt()
**       nclf_vm_clpath_getarc()
**       nclf_vm_finish()
**    COPYRIGHT 2011 (c) NCCS.  All Rights Reserved. 
**    MODULE NAME AND RELEASE LEVEL
**       nevmill.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/05/18 , 14:52:02
*********************************************************************/
/*
.....Debug declarations
*/
#define DEBUGL 0
#define DEBUGX 0
#define DEBUG_XML 1
//#define DUMMY 1
#define WIN32

#if DUMMY == 1
extern "C" void nclf_vmill_pocket() {}
extern "C" void nclf_vmill_pocket3() {}
extern "C" void nclf_vm_push_entry() {}
extern "C" void nclf_vm_clpath_getnpaths() {}
extern "C" void nclf_vm_clpath_gettype() {}
extern "C" void nclf_vm_clpath_getfeed() {}
extern "C" void nclf_vm_clpath_getspindle() {}
extern "C" void nclf_vm_clpath_getstartpt() {}
extern "C" void nclf_vm_clpath_getpt() {}
extern "C" void nclf_vm_clpath_getarc() {}
extern "C" void nclf_vm_finish() {}
extern "C" void ncl_nvmill_reset_calls(){}
#else
/*
.....NCL headers
*/
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "mdcpln.h"
#include "modef.h"
#include "ncldef.h"
#include "nclfc.h"
#include "gobas.h"
#include "ulist.h"
#include "mgeom.h"

//#include <ostream>

//#include <stdlib.h>

//#include  "usysdef.h"
//#include  "umath.h"
//#include	 "udebug.h"
//#include  "mdcoord.h"
//#include  "modef.h"
//#include  "calcom.h"
//#include  "cdef.h"
//#include  "uhep.h"
//#include	 "tlangtool.h"
/*
.....VoluMill headers
*/
//#include "C:\VoluMillSDK_70\Code\Common\Util/IncludeEverywhere.h"
#include "C:\VoluMillSDK_Apr12\Code\Common\Util/IncludeEverywhere.h"
#include "C:\VoluMillSDK_Apr12\Code\Common\Util/ParamIds.h"
#include "C:\VoluMillSDK_Apr12\Code\Common\Util/Params.h"
#include "C:\VoluMillSDK_Apr12\Code\Common\Exchange\ProgressData.h"
#include "Version.h"
#include "C:\VoluMillSDK_Apr12\Code\Common\License\LicenseAPI.h"
#include "ToolpathRecords.h"
#include "VoluMillExchange.h"
#include "CurveAlg.h"
#include "nclvmptool.h"
#include <iostream>
#include <ostream>
#include <fstream>
#include "TriMeshFacets.h"
#include "TriMesh3d.h"
#include "TriMeshExchange.h"
#include "C:\VoluMillSDK_Apr12\Code\VoluMillUniversalClient\VoluMillUniversalClient\VMUDef.h"
#include "C:\VoluMillSDK_Apr12\Code\VoluMillUniversalClient\Graphics\GNode.h"

#include "C:\VoluMillSDK_Apr12\Code\VoluMillUniversalClient\Component/TriMeshComponent.h"
#include "C:\VoluMillSDK_Apr12\Code\VoluMillUniversalClient\Component/FaceComponent.h"
//#include "C:\VoluMillSDK\Code\VoluMillUniversalClient\Component/OperationSetupComponent.h"
#include "C:\VoluMillSDK_Apr12\Code\VoluMillUniversalClient\Component/InitParams.h"

#include "C:\VoluMillSDK_Apr12\Code\VoluMillUniversalClient\Component\Project.h"
#include "C:\VoluMillSDK_Apr12\Code\VoluMillUniversalClient\Component\ComponentParamIds.h"
#include "C:\VoluMillSDK_Apr12\Code\VoluMillUniversalClient\Component\ComponentId.h"
#include "C:\VoluMillSDK_Apr12\Code\VoluMillResources\VoluMillResources.h"

#include "C:\VoluMillSDK_Apr12\Code\VoluMillResources\MultiProgressDialog.h"

//#include "C:\VoluMillSDK\Code\VoluMillUniversalClient\VoluMill3x\VoluMill5xOperation.h"

//#include <TriMeshComponent.h>
//#include <Project.h>
//#include "FaceComponent.h"
//#include "PluginAPI.h"
//#include "ModelingKernelInterface.h"

// typedef _W64 int INT_PTR

#ifdef DEBUG_XML 
//#if DEBUG_XML == 1
#include <boost/serialization/deque.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/variant.hpp>
#include <boost/serialization/shared_ptr.hpp>
#include <boost\smart_ptr\shared_ptr.hpp>

//#include <boost/serialization/map.hpp>
//#include <boost/serialization/vector.hpp>
//#include <boost/serialization/deque.hpp>
//#include <boost/serialization/shared_ptr.hpp>
//#include <boost/serialization/variant.hpp>
//#include <boost/serialization/export.hpp>

inline void fromVoluMill2dInputToXml (std::ostream& os, const exchange::VoluMill2dInput& input)
{
	boost::archive::xml_oarchive oa(os);
	oa << BOOST_SERIALIZATION_NVP(input);
}
inline void fromVoluMill3dInputToXml (std::ostream& os, const exchange::VoluMill3dInput& input)
{
	boost::archive::xml_oarchive oa(os);
	oa << BOOST_SERIALIZATION_NVP(input);
}

inline void fromVoluMill5xInputToXml (std::ostream& os, const exchange::VoluMill5xInput& input)
   {
      boost::archive::xml_oarchive oa(os);
      oa << BOOST_SERIALIZATION_NVP(input);
   }
#endif

//inline boost::shared_ptr<topo::mesh::TriMeshFacets3d>& getMeshForWrite() { component::preTouch(); return m_pMesh; }

//struct ShellIdToShellData
//   {
//      component::ComponentId m_shellId;
//      boost::shared_ptr<void> m_shellPtr;
//   };
//
//   struct FaceIdsToFacesData
//   {
//      component::ComponentIdVec m_faceIds;
//      std::vector<boost::shared_ptr<void> > m_facePtrs;
//   };

//namespace
//{
//   template<class MeshPtr>
//   void transformMesh (MeshPtr& pMesh, const geom::Transform& trf)
//   {
//      typename MeshPtr::element_type::Vertex v, vEnd;
//      for (boost::tie (v, vEnd) = topo::mesh::getVertices (*pMesh); v != vEnd; ++v)
//      {
//         topo::mesh::setVertexLocation (v, trf.transformPoint (topo::mesh::getVertexLocation (v)));
//      }
//   }
//   void addTriMeshFacets (const topo::mesh::TriMeshFacets3d& facetsMesh, topo::mesh::TriMeshFacets3dPtr* ppMesh, bool resetMesh)
//   {
//      using namespace topo::mesh;
//      int baseVertices = 0;
//      if (resetMesh)
//         ppMesh->reset (new TriMeshFacets3d);
//      else
//         baseVertices = (int) (*ppMesh)->m_vertices.size();
//
//      // Copy over the vertices first, then the faces, adding in the vertex offset for each face
//      std::copy (facetsMesh.m_vertices.begin(), facetsMesh.m_vertices.end(), std::back_inserter ((*ppMesh)->m_vertices));
//      for (TriMeshFacets3d::Faces::const_iterator it = facetsMesh.m_faces.begin(); it != facetsMesh.m_faces.end(); ++it)
//      {
//         const TriMeshFacet& facet = *it;
//         (*ppMesh)->m_faces.push_back (TriMeshFacet (facet.m_idx[0] + baseVertices, facet.m_idx[1] + baseVertices, facet.m_idx[2] + baseVertices));
//      }
//   }
//}

//namespace
//{
//   void triangulateAndAddMeshes (
//      const boost::shared_ptr<exchange::ModelingKernelInterface>& pMKI,
//      const component::ProjectPtr& pProject,
//      const geom::Transform& trfInverse,
//      const component::ComponentIdVec& ids,
//      double tol,
//      const topo::mesh::TriMeshFacets3dPtr& pMesh)
//   {
//      component::ComponentId shellId;
//      boost::shared_ptr<void> pShell;
//      boost::shared_ptr<ShellIdToShellData> pShellData (new ShellIdToShellData);
//      boost::shared_ptr<FaceIdsToFacesData> pData (new FaceIdsToFacesData);
//      for (int ii=0; ii < (int) ids.size(); ++ii)
//      {
//         component::TriMeshComponentPtr pComp = boost::shared_dynamic_cast<component::TriMeshComponent> (pProject->getComponent (ids[ii]));
//         if (pComp)
//         {
//            if (pComp->getOwningFaceId().isValid())
//               pData->m_faceIds.push_back (pComp->getOwningFaceId());
//            else
//            {
//               // Take the mesh "as-is" and transform it
//               topo::mesh::TriMeshFacets3dPtr pMeshCopy (new topo::mesh::TriMeshFacets3d (*pComp->getMesh()));
//               transformMesh (pMeshCopy, trfInverse);
//               topo::mesh::unionWithOtherTriMeshFacets (*pMesh, *pMeshCopy);
//            }
//         }
//      }
//      plugin::callPluginFunction2 ("OCCBridge.dll", "faceIdsToFaces", pProject, pData);
//      for (int ii=0; ii < (int) pData->m_faceIds.size(); ++ii)
//      {
//         component::FaceComponentPtr pFace = boost::shared_dynamic_cast<component::FaceComponent> (pProject->getComponent (pData->m_faceIds[ii]));
//         if (pFace && pFace->getShellId().isValid())
//         {
//            if (shellId != pFace->getShellId())
//            {
//               pShellData->m_shellId = pFace->getShellId();
//               plugin::callPluginFunction2 ("OCCBridge.dll", "shellIdToShell", pProject, pShellData);
//               pShell = pShellData->m_shellPtr;
//            }
//            topo::mesh::TriMeshFacets3dPtr pFacets;
//            std::vector<geom::Point2d> uvs;
//            pMKI->triangulateFace (pShell, pData->m_facePtrs[ii], tol, 5.0 * util::pi / 180.0, &pFacets, &uvs, 0);
//            if (pFacets)
//               topo::mesh::unionWithOtherTriMeshFacets (*pMesh, *pFacets);
//            else
//               WARN (0)("Failed to create mesh on face id")(pFace->getId().getId());
//         }
//      }
//   }
//}

/*
.....external C/Fortran function prototype 
*/
extern "C"
{
	void nclf_vmill_pocket(UM_int2 *, UM_real8 *, char [120][64], UM_int4 *,
		UM_int2 *);
	void nclf_vmill_pocket3(UM_int2 *);
	void nclf_vmill_pocket5(UM_int2 *);
	void nclf_vm_push_entry(UM_real8 *);
	void gtdesc(UM_real8 &, UM_int4 &, UM_int2 &, UM_int2 &);
	int vxchk(char *, UM_int4 *, UM_int4 *, UM_int4 *, UM_int4 *, UM_int2 *,
		UM_int2 *);
	void vmpprm(UM_int4 *, UU_REAL *, UU_REAL *);
	void vmptrn(UU_REAL *,UM_int2 *);

	void cptend();
	void cptint(UM_int4 *, UU_REAL *, UM_int2 *, UM_int2 *npts);
	void cptnxt(UU_REAL *,UU_REAL *);
	void iscmpc(UM_int4 *, UM_int2 *);
	void getend(UU_REAL *);
	void gtccnm(UM_int4 *, UM_int2 *);
	void gtcent(UM_int4 *, UM_int2 *,UM_int4 *, UM_real8 *,UM_int2 *, UM_int2 *);
	void gtcntt(UM_int4 *, UM_int2 *,UM_int4 *, UM_real8 *,UM_int2 *, UM_int2 *,
		UM_int2 *);
	void gtgeo(UM_int4 *, UM_real8 *);
	void gtnpt(UM_int2 *, UM_int2 *, UM_int2 *);
	void gtpnnp(UM_int4 *, UM_int2 *, UM_int2 *);
	void gpnptt(UU_REAL *, UM_int4 *, UM_int2 *, UM_int2 *);
	void gtpts(UU_REAL *, UM_int2 *, UM_int2 *, UM_int2 *);
	UU_LOGICAL wcsact();
	void ncl_wcstomcs(int, UU_REAL *, UU_REAL *);
	void wgtnpt(UM_int2 *, UM_int2 *, UM_int2 *, UM_int2 *);
	void wgtci(UU_REAL *, UM_int2 *, UM_int2 *);
	void wgtpt(UU_REAL *,UM_int2 *, UM_int2 *, UM_int2 *, UM_int2 *, UM_int2 *);

	UU_LOGICAL um_cceqcc_tol(UM_coord, UM_coord, UU_REAL);
	int um_get_circle_data(int, UM_coord, UU_REAL *, UU_REAL *, UM_vector,
		UM_coord, UM_coord, UU_REAL *);
	void um_vctovc(UM_vector,UM_vector);

	void ncl_cutter_circle(struct UM_circle_rec *,Gwpoint3 *gpt,int) ;
	void nclf_vm_clpath_getnpaths(UM_int4 *);
	void nclf_vm_clpath_gettype(UM_int4 *, UM_int4 *);
	void nclf_vm_clpath_getfeed(UM_int4 *, UM_real8 *, UM_real8 *);
	void nclf_vm_clpath_getspindle(UM_int4 *, UM_real8 *);
	void nclf_vm_clpath_getstartpt(UM_real8[6], UM_real8 *);
	void nclf_vm_clpath_getpt(UM_int4 *, UM_real8[6], UM_real8 *);
	void nclf_vm_clpath_getarc(UM_int4 *,UM_int4 *, UM_real8[9], UM_real8[9],
		UM_real8[7], UM_real8 *);
	void nclf_vm_finish();

	void nclf_genpocket_bound(UM_int4 *,UU_REAL *,UM_int2 *,UM_int2 *,UM_int2 *,
		UM_int2 *);
	void nclf_genpocket_getpt(UM_int2 *,UU_REAL *);
	void ncl_genpocket_mark_open();
	void nclf_genpocket_open(UU_REAL *,UM_int2 *,UM_int2 *,UM_int2 *);
	void nclf_clear_ptlst();

	void uu_delay(int);
	void nclu_vmill_open_progress(UU_LOGICAL *);
	void nclu_vmill_close_progress();
	void nclu_vmill_update_progress(int);
	void NclxDbgPstr(char *);
	void uu_list_init(UU_LIST *, int, int, int);
	void uu_list_push(UU_LIST *, char *);
	void uu_list_free(UU_LIST *);
	UU_LOGICAL ncl_pocket_match_open(UU_KEY_ID);
	UU_LOGICAL ncl_genpocket_check_index(int);
	void ncl_genpocket_reset();
	void ncl_get_tstk (int *);
	void ncl_nvmill_reset_calls();
	void getsc(UM_int2 *, UU_REAL *);
	void ncl_vmill_get_tess(UM_tessellation **, int);
	void ncl_vmill_get_box(UU_REAL *);
	void ncl_vmill_get_bndpts(UU_LIST **);
	void ncl_vmill_get_boxfl(int *);
	void ncl_get_stklst (UU_LIST **);
	void ncl_wat_get_sfnums (int *, int *, int *);
	void ncl_vmill_get_expand(UU_LOGICAL *, UU_REAL *);
	void NclxDbgPstr(char *);

#if DEBUGX == 2
	extern "C" void ncl_test_vmill(exchange::ToolpathRecords **);
#endif
} 
/*
.....Internal static routines
*/
static void S_init_params (util::Parameterized*, UU_REAL *, int);
static int S_getBoundaryChains (UM_int2 *, UM_real8 *, char [120][64],
	UM_int4 *, UU_REAL, exchange::VoluMill2dBoundaries *);
static int S_getBoundaryChains3D (exchange::VoluMill3dBoundaries *, UU_REAL);
static int S_getBoundaryChains5x (exchange::VoluMill5xBoundaries *, UU_REAL);
static int S_getPredrilledHoles(exchange::VoluMillDrillHoles *);
static void S_freePredrilledHoles();
static int S_makeline(std::vector<geom::CurvePtr2d>*, UM_coord, UM_coord,
	UU_REAL, int);
static void S_openLog();
static void S_closeLog();
static void S_printLog(char *);
static void S_printSetKParam(char *, int);
static void S_printSetGParam(char *, UU_REAL);
static void S_printSetCParam(char *, char *);
static void S_printMakeLine(UU_REAL *, UU_REAL *);
static void S_printMakeWaterLine(int, UU_REAL *, UU_REAL *, UU_REAL);
static void S_printMakeCircle(int, UU_REAL *, UU_REAL, UU_REAL);
static void S_printMakeCCWArc(UU_REAL *, UU_REAL *, UU_REAL *);
static void S_printMakeCWArc(UU_REAL *, UU_REAL *, UU_REAL *);
static void S_printCurves();
static void S_printHole(exchange::VoluMillDrillHole *);
static UM_int2 S_get_error(int);
static int S_getBoundaryChains5x(exchange::VoluMill5xBoundaries *pBoundaries, UU_REAL dtol);

/*
.....Internal variables
*/
static FILE *Sfp=0;
static UU_LOGICAL Stranfl,Soffpart;
static UU_REAL Slevel,Sgpl;
static int Scalls,Snentry=0;
static UU_LIST Sentry_list;
/*
.....VoluMill toolpath records
*/

//boost::shared_ptr<exchange::ToolpathRecords> ppRecords;
exchange::ToolpathRecords *pRecords;
exchange::ToolpathRecords *ppRecords;
static int pnumRecords = 0;
static int snumRecords = 0;

static int fiveaxis = 0;
/*
.....VoluMill License
*/

namespace license
{
   int generateLicenseFromMasterLicense (LicenseKey* pKey);
}

//namespace license
//{
//int __cdecl generateLicenseFromMasterLicense (struct license::LicenseKey* pKey);
//}

#define ERROR_VOLUMILL_INTERNAL_ERROR 1007
#define ERROR_OPEN_POCKETS_NOT_ALLOWED 1010
#define ERROR_TOOL_DIAMETER_TOO_SMALL 1012
#define ERROR_NO_TOOLPATH_GENERATED 1013
#define ERROR_NO_STOCK_DEFINED 1022
#define ERROR_NO_PART_DEFINED 1023
#define ERROR_PREDRILLED_HOLES_CORRUPTED 1025 
#define ERROR_PREDRILLED_HOLES_DIAMETER 1026 
#define ERROR_PREDRILLED_HOLES_NO_CLOSE_POINT 1027 
#define ERROR_PREDRILLED_HOLES_SIDE_CUT_HIT_BOUNDARY 1028 
#define ERROR_PREDRILLED_HOLES_HOLE_HIT_BOUNDARY 1029 
#define ERROR_PREDRILLED_HOLES_HOLE_NOT_DEEP 1030
#define WARNING_WILL_LEAVE_SCALLOPS 2000

/*********************************************************************
**    E_FUNCTION     : ncl_nvmill_reset_calls()
**       Reset the count of calls.  The count is used to determine
**       if the input boundary from waterline is the stock boundary.
**       INPUT  : none
**       OUTPUT  :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_nvmill_reset_calls()
{
	Scalls = 0;
}

/*********************************************************************
**    E_FUNCTION     : nclf_vmill_pocket(nbound4,kbound8,token,ivxsub)
**       Interface to call VoluMill SDK from NCL
**       INPUT  :
**          nbound4       - Number of boundaries.
**          kbound8       - Boundary keys.
**          token         - Labels of boundaries.
**          ivxsub        - Subscripts for labels.
**       OUTPUT  :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vmill_pocket(UM_int2 *nbound4, UM_real8 *kbound8, char token[120][64],
	UM_int4 *ivxsub,UM_int2 *ier)
{
	int iprog;
	UU_LOGICAL interrupt;
	UU_REAL prog,dtol;
/*
.....Run debug routine
*/
#if DEBUGX == 2
	ncl_test_vmill(&pRecords);
	return;
#endif
/*
.....Open Progress window
*/
	interrupt = UU_FALSE;
	nclu_vmill_open_progress(&interrupt);
/*
.....Open log file
*/
	S_openLog();

/*
.....Get the VoluMill license
*/
	license::LicenseKey licenseKey;
	iprog=generateLicenseFromMasterLicense (&licenseKey);
	//iprog=generateLicenseFromMasterLicense (&licenseKey);
	//iprog=generateLicenseFromMasterLicense (&licenseKey);
	//iprog = 100;
#if DEBUG_XML
//	std::ostream *fos;
//	fos = new std::ofstream("key.xml");
//	license::fromLicenseKeyToXml(*fos,licenseKey);
#endif
	//license::loadLicenseKey (&licenseKey, getVoluMillVersionUniversal2x(), license::VOLUMILL_2X_COMPLETE_PRODUCT_LEVEL);
/*
.....Initilize VoluMill parameters
*/
	exchange::VoluMill2dInput input;
	S_init_params(&input.m_parameters,&dtol,2);
/*
.....Get boundary curves
*/
	S_getBoundaryChains (nbound4, kbound8, token, ivxsub, dtol,
		&input.m_boundaries);
	Scalls++;
/*
.....Get predrilled holes
*/
	S_getPredrilledHoles(&input.m_preDrilledHoles);
/*
.....Run the VoluMill 2d toolpath engine
*/
	int jobId = exchange::submitVoluMill2dJob (input, licenseKey);
/*
.....Update Progress window while job is running
*/
	for (;;)
	{
		uu_delay(100);
		prog = exchange::getProgress(jobId);
		if (prog < 0.) break;
		if (interrupt)
		{
			exchange::cancelJob(jobId);
			break;
		}
		iprog = prog;
		if (iprog > 100) iprog = 100;
		nclu_vmill_update_progress(iprog);
	}
	*ier = S_get_error(-iprog);
	if (!interrupt) iprog = 100;
	nclu_vmill_close_progress();
#if DEBUG_XML
	{
		std::ostream *fos;
		fos = new std::ofstream("nccs.xml");
		::fromVoluMill2dInputToXml(*fos,input);
	}
#endif
/*
.....Get the Toolpath Records
*/
	pRecords = new exchange::ToolpathRecords;
	int numRecords = exchange::getToolpathRecords (jobId, pRecords);
	// if (numRecords <=0)	numRecords = 1;
	//unsigned long numRecords = exchange::getToolpathRecords (jobId, pRecords);
/*
.....Make sure at least one Pre-drilled hole is used
*/
	if (Snentry > 0 && numRecords > 0 && *ier == 0)
	{
		exchange::VoluMillDrillHoles pPreDrilledHoles;
		exchange::VoluMillHelixes pHelixes;
		getPreDrilledHoles (jobId, &pPreDrilledHoles, &pHelixes);
		if (pPreDrilledHoles.size() == 0) *ier = -547;
	}
/*
.....Close log file
*/
	S_freePredrilledHoles();
	S_closeLog();
}

/*********************************************************************
**    E_FUNCTION     : nclf_vmill_pocket3(ier)
**       Interface to call VoluMill SDK from NCL (5-Axis)
**       INPUT  : none
**       OUTPUT  :
**          ier        - Non-zero if an error occurred.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vmill_pocket3(UM_int2 *ier)
{
	int iprog,tprog;
	UU_LOGICAL interrupt,use;
	UU_REAL prog,dtol,offset;
/*
.....Run debug routine
*/
#if DEBUGX == 2
	ncl_test_vmill(&pRecords);
	return;
#endif
/*
.....Open Progress window
*/
	interrupt = UU_FALSE;
	nclu_vmill_open_progress(&interrupt);
/*
.....Open log file
*/
	S_openLog();
/*
.....Get the VoluMill license
*/
	license::LicenseKey licenseKey;
	iprog=license::generateLicenseFromMasterLicense (&licenseKey);
	//iprog=generateLicenseFromMasterLicense (&licenseKey);
	//iprog = 100;
/*
....Initilize VoluMill parameters
*/
	exchange::VoluMill3dInput input;	// VoluMill5xInput also available
	S_init_params(&input.m_parameters,&dtol,3);
/*
....Get boundary curves
*/
	S_getBoundaryChains3D (&input.m_boundaries,dtol);
#if DEBUG_XML
	{
		std::ostream *fos;
		fos = new std::ofstream("nccs3.xml");
		::fromVoluMill3dInputToXml(*fos,input);
	}
#endif
/*
.....Get predrilled holes
*/
	S_getPredrilledHoles(&input.m_preDrilledHoles);
/*
.....Run the VoluMill 3d toolpath engine
*/
	int jobId = exchange::submitVoluMill3dJob (input, licenseKey);	//5xJob also available, Sasha 02/02/2021
/*
.....Update Progress window while job is running
*/
	for (;;)
	{
		uu_delay(100);
		prog = exchange::getProgress(jobId);
		if (prog < 0.) 
			break;
		if (interrupt)
		{
			exchange::cancelJob(jobId);
			break;
		}
		iprog = prog;
		if (iprog > 100) iprog = 100;
		nclu_vmill_update_progress(iprog);
	}
	tprog = -prog;
	*ier = S_get_error(tprog);
	if (!interrupt) iprog = 100;
	nclu_vmill_close_progress();
/*
.....Get the Toolpath Records
*/
	pRecords = new exchange::ToolpathRecords;
	/*int */snumRecords = exchange::getToolpathRecords (jobId, pRecords);
/*
.....Make sure at least one Pre-drilled hole is used
*/
	if (Snentry > 0 && snumRecords > 0 && *ier == 0)
	{
		exchange::VoluMillDrillHoles pPreDrilledHoles;
		exchange::VoluMillHelixes pHelixes;
		getPreDrilledHoles (jobId, &pPreDrilledHoles, &pHelixes);
		if (pPreDrilledHoles.size() == 0) *ier = -547;
	}
/*
.....Close log file
*/
	S_freePredrilledHoles();
	S_closeLog();
}

void nclf_vmill_pocket5(UM_int2 *ier)	//@@@@@@@@@@@@@  Sasha. Feb04, 2021
{
	int iprog,tprog;
	UU_LOGICAL interrupt,use;
	UU_REAL prog,dtol,offset;

	fiveaxis = 1;


/*
.....Run debug routine
*/
#if DEBUGX == 2
	ncl_test_vmill(&pRecords);
	return;
#endif

	//const component::ProjectPtr& pProject = NULL;
	//const component::ProjectPtr pProject(new component::ProjectPtr);
	//component::
	//component::GeomGroupComponentPtr pGGroup (new component::GeomGroupComponent);
	
/*
.....Open Progress window
*/
	interrupt = UU_FALSE;
	nclu_vmill_open_progress(&interrupt);
/*
.....Open log file
*/
	S_openLog();
/*


.....Get the VoluMill license
*/
	license::LicenseKey licenseKey;
	//iprog=license::generateLicenseFromMasterLicense (&licenseKey);
	int errorCode = license::loadLicenseKey (&licenseKey, getVoluMillVersionMastercam3x(), license::VOLUMILL_5X_ROUGH_PRODUCT_LEVEL);
	
/*
....Initilize VoluMill parameters
*/
	exchange::VoluMill5xInput input;	// VoluMill5xInput 
	
	S_init_params(&input.m_parameters,&dtol,5);


	/*geom::Point3d ll, ur;
    topo::mesh::getBoundingBox (*input.m_boundaries.m_pPartMesh, &ll, &ur);
    topo::mesh::rectangleToMesh (ll, ur, &input.m_boundaries.m_pMaterialMesh);*/
		
/*

....Get boundary curves
*/
	S_getBoundaryChains5x (&input.m_boundaries,dtol);

	geom::Point3d ll, ur;
    topo::mesh::getBoundingBox (*input.m_boundaries.m_pPartMesh, &ll, &ur);
    topo::mesh::rectangleToMesh (ll, ur, &input.m_boundaries.m_pMaterialMesh);


		input.m_boundaries.m_containmentBoundaries.m_materialBoundaries.clear();	// Evans suggestion, Mar.23, 2021

/*
.....Get predrilled holes
*/
	S_getPredrilledHoles(&input.m_preDrilledHoles);
/*
.....Run the VoluMill 3d toolpath engine
*/

/*
	#if DEBUG_XML
	{
		std::ostream *fos;
		fos = new std::ofstream("nccs5_2019_Apr09.xml");
		::fromVoluMill5xInputToXml(*fos,input);
	}
#endif
*/
	/*
	boost::shared_ptr<exchange::ProgressData> ppd (exchange::createLocallyLinkedProgressData());
	int jobId = exchange::submitVoluMill5xJob (input, licenseKey);	// Sasha 02/02/2021
	////boost::shared_ptr<exchange::ToolpathRecords> pRecords (new exchange::ToolpathRecords);

	ppd->setJobId (jobId);
         INT_PTR result = VoluMillResources::displayMultiProgressDialog (ppd);
         boost::shared_ptr<exchange::ToolpathRecords> ppRecords (new exchange::ToolpathRecords);
		 //ppRecords = new exchange::ToolpathRecords;
		 //pRecords.get();
         int numRecords = exchange::getToolpathRecords (ppd->getJobId(), ppRecords.get());
*/		 
		 //*pRecords = ppRecords;

	
/*
.....Update Progress window while job is running
*/

	int jobId = exchange::submitVoluMill5xJob (input, licenseKey);	// Sasha 02/02/2021
		 
	for (;;)
	{
		uu_delay(100);
		//assert(exchange::getProgress(jobId)<100.0);
		//prog = min(100,exchange::getProgress(jobId));
		prog = exchange::getProgress(jobId);
		if (prog < 0.) 
			break;
		////if (prog < 0.) continue;
		if (interrupt)
		{
			exchange::cancelJob(jobId);
			break;
		}
		

		iprog = prog;
		//if (iprog==90)
		//{
		//	prog = iprog;
			
		//}
		if (iprog > 100) iprog = 100;
		nclu_vmill_update_progress(iprog);

		//_sleep(100);
	}
	tprog = -prog;
	*ier = S_get_error(tprog);
	if (!interrupt) iprog = 100;
	nclu_vmill_close_progress();

	

	//(new exchange::ToolpathRecords);
	////boost::shared_ptr<exchange::ToolpathRecords> ppRecords (new exchange::ToolpathRecords);
	ppRecords = new exchange::ToolpathRecords;

	
	/*int */pnumRecords = exchange::getToolpathRecords (jobId, ppRecords);
	pnumRecords = ppRecords->size();

	
/*
.....Make sure at least one Pre-drilled hole is used
*/
	if (Snentry > 0 && pnumRecords > 0 && *ier == 0)
	{
		exchange::VoluMillDrillHoles pPreDrilledHoles;
		exchange::VoluMillHelixes pHelixes;
		getPreDrilledHoles (jobId, &pPreDrilledHoles, &pHelixes);
		if (pPreDrilledHoles.size() == 0) *ier = -547;
	}
/*
.....Close log file
*/
	S_freePredrilledHoles();
	S_closeLog();
}

/*********************************************************************
**    E_FUNCTION     : nclf_vm_push_entry(pt)
**       Pushes a pre-drilled hole onto the stack.
**    PARAMETERS
**       INPUT	   :
**          pt     = Point to push onto stack.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vm_push_entry(UM_real8 *pt)
{
/*
.....Initialize list if necessary
*/
	if (Snentry == 0)
		uu_list_init(&Sentry_list,sizeof(UM_coord),10,10);
/*
.....Push point onto stack
*/
	uu_list_push(&Sentry_list,(char *)pt);
	Snentry++;
}

/*********************************************************************
**    E_FUNCTION     : nclf_vm_clpath_getnpaths(npaths)
**       Get the number of passes from the VOLUMILL toolpath record. 
**    PARAMETERS
**       INPUT	   : none
**       OUTPUT : 
**			npasses	- The number of passes 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vm_clpath_getnpaths(UM_int4 *npaths)
{
	
	//if (ppRecords.get()->size()==0)	// == UU_NULL)
	
	if (fiveaxis==0)
	{
	if (snumRecords==0)	// == UU_NULL)
		*npaths = 0;
	else
		//*npaths = (UM_int4)pRecords->size();
	
	*npaths = (UM_int4)snumRecords;
	}
	else
	{
		if (pnumRecords==0)	// == UU_NULL)
		*npaths = 0;
	else
		//*npaths = (UM_int4)pRecords->size();
	
	*npaths = (UM_int4)pnumRecords;
	}
	/*ppRecords.get()->size();*/
	//if (*npaths==0) *npaths=1;	//??
}

/*********************************************************************
**    E_FUNCTION     : nclf_vm_clpath_gettype(ix,ntype)
**       Get clpath line end position from the VOLUMILL toolpath record. 
**    PARAMETERS
**       INPUT  :
**			ix		- the ix-th cl point
**       OUTPUT : 
**			ntype	- 0: Rapid 1:Line 2: ARC 3:CWARC
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vm_clpath_gettype(UM_int4 *ix, UM_int4 *ntype)
{
	int itype;
	int idx(*ix-1);
	if (fiveaxis==0)
		itype = exchange::getType (*pRecords, idx);
	else
		itype = exchange::getType (*ppRecords, idx);

	*ntype = itype;
}

/*********************************************************************
**    E_FUNCTION     : nclf_vm_clpath_getfeed(ix,feedrate)
**       Get the ix-th feedrate from the VOLUMILL toolpath record. 
**    PARAMETERS
**       INPUT  :
**			ix		- the ix-th cl point
**       OUTPUT : 
**		feedrate	- feedrate
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vm_clpath_getfeed(UM_int4 *ix, UM_real8 *feedrate, UM_real8 *cnv)
{
	double dFeedrate;
	int idx(*ix-1);
	if (fiveaxis==0)
		exchange::getFeedRate (*pRecords, idx, &dFeedrate);
	else
		exchange::getFeedRate (*ppRecords, idx, &dFeedrate);

	*feedrate = dFeedrate * *cnv;
}


/*********************************************************************
**    E_FUNCTION     : nclf_vm_clpath_getspindle(ix,spindle)
**       Get the ix-th spindle speed from the VOLUMILL toolpath record. 
**    PARAMETERS
**       INPUT  :
**          ix      - Toolpath record number.
**       OUTPUT : 
**          spindle - Spindle speed
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vm_clpath_getspindle(UM_int4 *ix, UM_real8 *spindle)
{
	double dSpindle;
	int idx(*ix-1);
	if (fiveaxis==0)
		dSpindle = exchange::getSpindle (*pRecords, idx);
	else
		dSpindle = exchange::getSpindle (*ppRecords, idx);
	//dSpindle = exchange::getSpindle (*pRecords, idx);

	*spindle = dSpindle;
}

/*********************************************************************
**    E_FUNCTION     : nclf_vm_clpath_getstartpt(ix,pte,cnv)
**       Get clpath start position from the VOLUMILL toolpath record. 
**    PARAMETERS
**       INPUT  :
**			ix		- the ix-th cl point
**       OUTPUT : 
**          pte		- start tool end, tool axis
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vm_clpath_getstartpt(UM_real8 pte[6], UM_real8 *cnv)
{
	geom::Point3d pt;
	if (fiveaxis==0)
		exchange::getStartPoint (*pRecords,  &pt);
	else
		exchange::getStartPoint (*ppRecords,  &pt);

	//exchange::getStartPoint (*pRecords,  &pt);

	for (int k = 0; k < 3; k++)
	{
		pte[k] = pt[k] * *cnv;
	}

	pte[3] = 0.0;
	pte[4] = 0.0;
	pte[5] = 1.0;
}

/*********************************************************************
**    E_FUNCTION     : nclf_vm_clpath_getpt(ix,pte,cnv)
**       Get clpath line end position from the VOLUMILL toolpath record. 
**    PARAMETERS
**       INPUT  :
**			ix		- the ix-th cl point
**       OUTPUT : 
**          pte		- tool end, tool axis
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vm_clpath_getpt(UM_int4 *ix, UM_real8 pte[6], UM_real8 *cnv)
{
	geom::Point3d pt;
	
	int idx(*ix-1);
	if (fiveaxis==0)
		exchange::getEndPoint (*pRecords, idx, &pt);
	else
		exchange::getEndPoint (*ppRecords, idx, &pt);
	

	for (int k = 0; k < 3; k++)
	{
		pte[k] = pt[k] * *cnv;
	}

	pte[3] = 0.0;
	pte[4] = 0.0;
	pte[5] = 1.0;
}

/*********************************************************************
**    E_FUNCTION     : nclf_vm_clpath_getarc(ntype,ix,ps,pe,cir)
**       Get VOLUMILL tool positi from the toolpath record. 
**    PARAMETERS
**       INPUT  :
**			ntype	- 2:CW Arc 3:CCW Arc
**			ix		- The ix-th cl point
**       OUTPUT : 
**          ps		- Arc start point
**			pe		- Arc end point
**			cir		- Circle record(x,y,z,i,j,k,radius)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vm_clpath_getarc(UM_int4 *ntype, UM_int4 *ix, 
				UM_real8 ps[9], UM_real8 pe[9], UM_real8 cir[7], UM_real8 *cnv)
{
	geom::Point3d endPt;	
	geom::Point3d startPt(ps[0],ps[1],ps[2]);
/*
....Get Arc end point
*/
	double radius;

	int idx(*ix-1);
	if (fiveaxis==0)
		exchange::getEndPoint (*pRecords, idx, &endPt);
	else
		exchange::getEndPoint (*ppRecords, idx, &endPt);
	exchange::getEndPoint (*pRecords, idx, &endPt);
	endPt[0] = endPt[0] * *cnv;
	endPt[1] = endPt[1] * *cnv;
	endPt[2] = endPt[2] * *cnv;
/*
....Get arc radius

*/
	if (fiveaxis==0)
		radius = exchange::getRadius (*pRecords, idx) * *cnv;
	else
		radius = exchange::getRadius (*ppRecords, idx) * *cnv;
	

	
/*
....Arc center
*/
	geom::Point2d centerPt;
	if (*ntype == 3)
		centerPt = geom::getCCWArcCenter(geom::Point2d(startPt[0],startPt[1]),
							geom::Point2d(endPt[0],endPt[1]), radius);
	else if (*ntype == 2)
		centerPt = geom::getCCWArcCenter(geom::Point2d(endPt[0],endPt[1]),
		                    geom::Point2d(startPt[0],startPt[1]), radius);
/*
.....Circle records
*/
	for (int k = 0; k < 3; k++)
	{
		pe[k] = endPt[k];
		if (k == 2)
			cir[k] = endPt[k];
		else
			cir[k] = centerPt[k];
	}

/*
....Circle vector and Start point center forward vector
*/	
	ps[3] = 0.0;
	ps[4] = 0.0;
	ps[5] = 1.0;

	geom::Point3d cPt(centerPt[0],centerPt[1],startPt[2]);
	geom::Vec3d zVec(0.0,0.0,1.0);
	geom::Vec3d sVec (startPt-cPt);
	geom::Vec3d sfVec;

	if (*ntype == 3)      /*CCW*/
		sfVec = geom::normalize(geom::crossProduct(zVec,sVec));
	else if (*ntype == 2) /*CW*/
		sfVec = geom::normalize(geom::crossProduct(sVec,zVec));

	ps[6] = sfVec[0];
	ps[7] = sfVec[1];
	ps[8] = sfVec[2];
/*
....Cicle vector and end point forward vector
*/	
	pe[3] = 0.0;
	pe[4] = 0.0;
	pe[5] = 1.0;

	geom::Point3d eVec (endPt-cPt);
	geom::Vec3d efVec;

	if (*ntype == 3)	  /*CCW*/
		efVec = geom::normalize(geom::crossProduct(zVec,sVec));
	else if (*ntype == 2) /*CW*/
		efVec = geom::normalize(geom::crossProduct(sVec,zVec));

	pe[6] = efVec[0];
	pe[7] = efVec[1];
	pe[8] = efVec[2];
/*
...circle records
*/
	cir[3] = 0.0;
	cir[4] = 0.0;
	cir[5] = 1.0;
	cir[6] = radius;
}

/*********************************************************************
**    E_FUNCTION     : nclf_vm_finish()
**       Cleanup the VoluMill variables.
**    PARAMETERS
**       INPUT  :
**          none. 
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_vm_finish()
{
	delete pRecords;
	//delete ppRecords;
	//ppRecords->clear();
	//ppRecords->empty();
	//ppRecords->erase();
	delete ppRecords;
	return;
}

/*********************************************************************
**    I_FUNCTION     : S_init_params(pParams,dtol,mode)
**       Initializes the VoluMill 2D parameters based on the VMPMOD
**       command settings.
**    PARAMETERS
**       INPUT  :
**          pParams = VoluMill parameter settings.
**          mode    = 2 = Set options for 2-axis pocketing, 3 = 3-axis.
**       OUTPUT :
**          dtol    = Tolerance used for pocketing. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_params (util::Parameterized* pParams, UU_REAL* dtol,int mode)
{
	int i;
	UU_LOGICAL use_stkoff;
	UM_int4 kparm[15];
	UU_REAL gparm[30],gpos[10],rnum,grad,stp,fed,stkoff;
/*
.....Get VOLMOD settings
*/
	vmpprm(kparm,gparm,gpos);
	Stranfl = kparm[7] == 1;
	/*if (mode == 5)
		Stranfl = 1;*/
	//@@@@@@@@@@@ Sasha 02/02/2021
	// Stranfl = 1;	// it should be only in nclf_vmill_pocket5 or(?) if mode == 5
	//@@@@@@@@@@@@
	Slevel = gpos[1] - gpos[0];
	Soffpart = kparm[12];
	Sgpl = gpos[1];
/*
.....Initialize VoluMIll parameters
*/
	util::Parameterized& params = *pParams;
/*
.....Initialize VoluMIll parameters
*/
	params.setParam (util::PARAM_TOOLPATH_CCW, kparm[0]);
	S_printSetKParam("PARAM_TOOLPATH_CCW",kparm[0]);
	params.setParam (util::PARAM_CUT_TOLERANCE, gparm[21]);
	S_printSetGParam("PARAM_CUT_TOLERANCE",gparm[21]);
	*dtol = gparm[21]*2.;
	params.setParam (util::PARAM_DEPTH_FIRST, kparm[4]);
	S_printSetKParam ("PARAM_DEPTH_FIRST", kparm[4]);

	params.setParam (util::PARAM_TOOL_DIAMETER, gparm[19]);
	S_printSetGParam ("PARAM_TOOL_DIAMETER", gparm[19]);
	params.setParam (util::PARAM_TOOL_CORNER_RADIUS, gparm[20]);
	S_printSetGParam ("PARAM_TOOL_CORNER_RADIUS", gparm[20]);
	params.setParam (util::PARAM_TOOL_NUM_FLUTES, kparm[2]);
	S_printSetKParam ("PARAM_TOOL_NUM_FLUTES", kparm[2]);
	params.setParam (util::PARAM_TOOL_NUMBER, kparm[5]);
	S_printSetKParam ("PARAM_TOOL_NUMBER", kparm[5]);
	params.setParam (util::PARAM_TOOL_LENGTH_OFFSET, 0);
	S_printSetKParam ("PARAM_TOOL_LENGTH_OFFSET", 0);
	params.setParam (util::PARAM_TOOL_WORK_OFFSET, "");
	S_printSetCParam ("PARAM_TOOL_WORK_OFFSET", "");
	params.setParam (util::PARAM_COOLANT, kparm[6]+1);
	S_printSetKParam ("PARAM_COOLANT", kparm[6]+1);
	if (gparm[22] > UM_FUZZ)
	{
		params.setParam (util::PARAM_TOOL_OVERALL_LENGTH, gparm[22]);
		S_printSetGParam ("PARAM_TOOL_OVERALL_LENGTH", gparm[22]);
	}
	if (gparm[23] > UM_FUZZ)
	{
		params.setParam (util::PARAM_TOOL_LENGTH_OF_CUT, gparm[23]);
		S_printSetGParam ("PARAM_TOOL_LENGTH_OF_CUT", gparm[23]);
	}
	
	if (kparm[11])
	{
		params.setParam (util::PARAM_MINIMUM_FEEDRATE, gparm[24]);
		S_printSetGParam ("PARAM_MINIMUM_FEEDRATE", gparm[24]);
	}
	fed = (gparm[11] > UM_FUZZ)? gparm[11] : 100.0;
	params.setParam (util::PARAM_FEEDRATE, fed);
	S_printSetGParam ("PARAM_FEEDRATE", fed);
	stp = (gparm[12] > UM_FUZZ)? gparm[12] : fed*2.;
	params.setParam (util::PARAM_HIGH_FEEDRATE, stp);
	S_printSetGParam ("PARAM_HIGH_FEEDRATE", stp);
	stp = (gparm[13] > UM_FUZZ)? gparm[11] : fed/2.;
	params.setParam (util::PARAM_RAMP_FEEDRATE, stp);
	S_printSetGParam ("PARAM_RAMP_FEEDRATE", stp);
	if (gparm[25] > UM_FUZZ)
	{
		params.setParam (util::PARAM_XY_RAPID_FEEDRATE, gparm[25]);
		S_printSetKParam ("PARAM_XY_RAPID_FEEDRATE", gparm[25]);
	}
	if (gparm[26] > UM_FUZZ)
	{
		params.setParam (util::PARAM_Z_RAPID_FEEDRATE, gparm[26]);
		S_printSetKParam ("PARAM_Z_RAPID_FEEDRATE", gparm[26]);
	}

	if (gparm[15] != 0.)
	{
		i = gparm[15];
		params.setParam (util::PARAM_SPINDLE_SPEED, i);
		S_printSetKParam ("PARAM_SPINDLE_SPEED", i);
	}
	if (gparm[16] != 0.)
	{
		i = gparm[16];
		params.setParam (util::PARAM_RAMP_SPINDLE_SPEED, i);
		S_printSetKParam ("PARAM_RAMP_SPINDLE_SPEED", i);
	}
	params.setParam (util::PARAM_DWELL_AFTER_PLUNGE, gparm[18]);
	S_printSetGParam ("PARAM_DWELL_AFTER_PLUNGE", gparm[18]);

	params.setParam (util::PARAM_RAMP_ANGLE, gparm[9]);
	S_printSetGParam ("PARAM_RAMP_ANGLE", gparm[9]);

	i = 2 - kparm[3];
	if (Snentry > 0)
	{
		i = 2;
		if (gparm[28] == 0.)
		{
			params.setParam (util::PARAM_DRILL_DIAMETER,gparm[19]+gparm[21]*2.);
			S_printSetGParam ("PARAM_DRILL_DIAMETER", gparm[19]+gparm[21]*2.);
		}
		else
		{
			params.setParam (util::PARAM_DRILL_DIAMETER,gparm[28]);
			S_printSetGParam ("PARAM_DRILL_DIAMETER", gparm[28]);
		}
		params.setParam (util::PARAM_DRILL_ANGLE,gparm[29]*2.);
		S_printSetGParam ("PARAM_DRILL_ANGLE",gparm[29]*2.);
		params.setParam (util::PARAM_DRILL_POINT_RANGE, 10000.0);
		S_printSetGParam ("PARAM_DRILL_POINT_RANGE",10000.0);

		params.setParam (util::PARAM_DETECT_OPTIMAL_PREDRILL_POINTS,1);
		S_printSetKParam ("PARAM_DETECT_OPTIMAL_PREDRILL_POINTS", 1);
	}
	params.setParam (util::PARAM_PLUNGE_TYPE, i);
	S_printSetKParam ("PARAM_PLUNGE_TYPE", i);
	
	rnum = gparm[10];
	grad = gparm[19]/2.; // - gparm[20];
	if (rnum < .5*grad) rnum = .5 * grad;
	if (rnum > .9*grad) rnum = .9 * grad;
	params.setParam (util::PARAM_PLUNGE_RADIUS, rnum);
	S_printSetGParam ("PARAM_PLUNGE_RADIUS", rnum);

	params.setParam (util::PARAM_RAPID_PLANE_Z, gpos[2]);
	S_printSetGParam ("PARAM_RAPID_PLANE_Z", gpos[2]);
	params.setParam (util::PARAM_TOP_MATERIAL_Z, gpos[1]);
	S_printSetGParam ("PARAM_TOP_MATERIAL_Z", gpos[1]);
	params.setParam (util::PARAM_PLUNGE_CLEARANCE, gpos[3]);
	S_printSetGParam ("PARAM_PLUNGE_CLEARANCE", gpos[3]);
	params.setParam (util::PARAM_REPOSITION_CLEARANCE, gpos[4]);
	S_printSetGParam ("PARAM_REPOSITION_CLEARANCE", gpos[4]);
	params.setParam (util::PARAM_HIGH_FEED_CLEARANCE, gpos[5]);
	S_printSetGParam ("PARAM_HIGH_FEED_CLEARANCE", gpos[5]);

	params.setParam (util::PARAM_DEPTH_OF_CUT, gparm[0]);
	S_printSetGParam ("PARAM_DEPTH_OF_CUT", gparm[0]);

	params.setParam (util::PARAM_STEPOVER, gparm[2]);
	S_printSetGParam ("PARAM_STEPOVER", gparm[2]);
	params.setParam (util::PARAM_XY_STOCK_TO_LEAVE, gparm[8]);
	S_printSetGParam ("PARAM_XY_STOCK_TO_LEAVE", gparm[8]);
	//params.setParam (util::PARAM_Z_STOCK_TO_LEAVE, 0.01);	// added Sasha 02/25/2021
	params.setParam (util::PARAM_SMOOTHING_RADIUS, gparm[5]);
	S_printSetGParam ("PARAM_SMOOTHING_RADIUS", gparm[5]);
	if (gparm[4] > 0.)
	{
		if (gparm[4] < gparm[5]) gparm[4] = gparm[5];
		params.setParam (util::PARAM_MINIMUM_POCKET_RADIUS, gparm[4]);
		S_printSetGParam ("PARAM_MINIMUM_POCKET_RADIUS", gparm[4]);
	}

	params.setParam (util::PARAM_USE_LOOP_IDS, 1);
	S_printSetKParam ("PARAM_USE_LOOP_IDS", 1);
	params.setParam (util::PARAM_DRILL_DIAMETER, gparm[19]+0.01);
/*
.....Slot machining options
*/
	if (kparm[8] == 0)
	{
		params.setParam (util::PARAM_SIDE_MILL_STEPOVER, gparm[3]);
		S_printSetGParam ("PARAM_SIDE_MILL_STEPOVER", gparm[3]);
		params.setParam (util::PARAM_SLOT_DEPTH_OF_CUT, gparm[1]);
		S_printSetGParam ("PARAM_SLOT_DEPTH_OF_CUT", gparm[1]);
		params.setParam (util::PARAM_SLOT_FEEDRATE, gparm[14]);
		S_printSetGParam ("PARAM_SLOT_FEEDRATE", gparm[14]);
		params.setParam (util::PARAM_SLOT_SPINDLE_SPEED, gparm[17]);
		S_printSetKParam ("PARAM_SLOT_SPINDLE_SPEED", gparm[17]);
	}
/*
.....3-axis Options only
*/
	if ((mode == 3)/*||(mode == 5)*/)
	{
		//@@@@@@@@@@@@@@@@@@@@@@@@@
		//if (gparm[0]<0.0)
		//{
		//	params.setParam (util::PARAM_DEPTH_OF_CUT, -gparm[0]);
		//	S_printSetGParam ("PARAM_DEPTH_OF_CUT", -gparm[0]);
		//}

		//params.setParam (util::PARAM_BOTTOM_MATERIAL_Z, 0.05);

		//ncl_vmill_get_expand(&use_stkoff,&stkoff);
		//if (use_stkoff) stp = gparm[27] + stkoff;
		//else stp = gparm[27];
		//params.setParam (util::PARAM_BOTTOM_MATERIAL_Z, gpos[6]);
		//params.setParam (util::PARAM_MATERIAL_CLEARANCE, stp);
	 //  params.setParam (util::PARAM_CHECK_CLEARANCE, 0.0);
	 //  params.setParam (util::PARAM_CAVITY_MILLING_ONLY, i);
	 //  params.setParam (util::PARAM_HOLDER_CLEARANCE, 0.0);
	 //  params.setParam (util::PARAM_SHANK_CLEARANCE, 0.0);
	 //  params.setParam (util::PARAM_HOLDER_DXF_PATH, "");
	 //  params.setParam (util::PARAM_FINAL_STEP_HEIGHT, 0.0);
	 //  params.setParam (util::PARAM_STOCK_TO_LEAVE,  0.01);	//Evans recommendation, Mar.23, 2021

	 //   params.setParam (util::PARAM_Z_STOCK_TO_LEAVE, 0.01);
	 // 
	 //  params.setParam (util::PARAM_DRILL_DIAMETER, 0.0);
	 //  params.setParam (util::PARAM_DRILL_ANGLE, 140.0);
	 //  params.setParam (util::PARAM_DRILL_POINT_RANGE, 500.0);
	 //  params.setParam (util::PARAM_DETECT_OPTIMAL_PREDRILL_POINTS, 1);

	 //  params.setParam (util::PARAM_ADJUST_FEEDRATE_HELIX, 1);
	 //  params.setParam (util::PARAM_ADJUST_FEEDRATE_RAMP, 1);
	 //  params.setParam (util::PARAM_USE_FREEWAY_LINKING, 1);
	 //  params.setParam (util::PARAM_CONTOUR_RAMPING, 0);
	 //  params.setParam (util::PARAM_ALLOW_INITIAL_SPIRAL, 1);
	 //  params.setParam (util::PARAM_ZAG_FEEDRATE_PERCENT, 0.0);
	 //  params.setParam (util::PARAM_RACETRACK_FEEDRATE, 0.0);
	 //  params.setParam (util::PARAM_HIGH_FEEDRATE_Z_NEGATIVE, 0.0);
	 //  params.setParam (util::PARAM_LINEARIZATION_TOLERANCE, 0.0);
	 //  params.setParam (util::PARAM_MAXIMUM_SEGMENT_LENGTH, 0.0);
	 //  params.setParam (util::PARAM_SIDE_MILL_SLOT_MILL_RATIO, 1.0);

	 //  params.setParam (util::PARAM_REPOSITIONING_RADIUS, 0.1);
	 //  params.setParam (util::PARAM_REPOSITIONING_ANGLE, 6.0);
	 //  params.setParam (util::PARAM_REPOSITIONING_TESSELLATION_ANGLE, 20.0);
	 //  params.setParam (util::PARAM_MIN_REPOSITIONING_RADIUS, 0.001);
	 //  params.setParam (util::PARAM_REPOSITIONING_KEEP_RAPID, 0);
	 //  params.setParam (util::PARAM_REPOSITIONING_2D_ONLY, 0);

	 //  params.setParam (util::PARAM_TOOLPATH_EVENTS, 1);

	 //  if (gpos[7] > UM_FUZZ)
		//{
		//	params.setParam(util::PARAM_FINAL_STEP_HEIGHT,gpos[7]);
		//}

	 //  params.setParam (util::PARAM_BOTTOM_MATERIAL_Z, gpos[6]);
		//S_printSetGParam ("PARAM_BOTTOM_MATERIAL_Z", gpos[6]);
		//params.setParam (util::PARAM_STOCK_TO_LEAVE, gparm[7]);
		//S_printSetGParam ("PARAM_STOCK_TO_LEAVE", gparm[7]);
		//i = (kparm[13] == 0)? 1 : 0;
		//params.setParam (util::PARAM_CAVITY_MILLING_ONLY, i);
		//S_printSetKParam ("PARAM_CAVITY_MILLING_ONLY", i);
		//ncl_vmill_get_expand(&use_stkoff,&stkoff);
		//if (use_stkoff) stp = gparm[27] + stkoff;
		//else stp = gparm[27];
		//params.setParam (util::PARAM_MATERIAL_CLEARANCE, stp);
		//S_printSetKParam ("PARAM_MATERIAL_CLEARANCE", stp);
		//params.setParam (util::PARAM_CHECK_CLEARANCE, 0.);//gparm[27]);
		//S_printSetKParam ("PARAM_CHECK_CLEARANCE", 0.);//gparm[27]);
		//params.setParam (util::PARAM_HIT_FLATS, kparm[14]);
		//S_printSetKParam ("PARAM_HIT_FLATS", kparm[14]);
		//if (gpos[7] > UM_FUZZ)
		//{
		//	params.setParam(util::PARAM_FINAL_STEP_HEIGHT,gpos[7]);
		//	S_printSetGParam("PARAM_FINAL_STEP_HEIGHT",gpos[7]);
		//}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
		params.setParam (util::PARAM_BOTTOM_MATERIAL_Z, gpos[6]);
		S_printSetGParam ("PARAM_BOTTOM_MATERIAL_Z", gpos[6]);
		params.setParam (util::PARAM_STOCK_TO_LEAVE, gparm[7]);
		S_printSetGParam ("PARAM_STOCK_TO_LEAVE", gparm[7]);
		i = (kparm[13] == 0)? 1 : 0;
		params.setParam (util::PARAM_CAVITY_MILLING_ONLY, i);
		S_printSetKParam ("PARAM_CAVITY_MILLING_ONLY", i);
		ncl_vmill_get_expand(&use_stkoff,&stkoff);
		if (use_stkoff) stp = gparm[27] + stkoff;
		else stp = gparm[27];
		params.setParam (util::PARAM_MATERIAL_CLEARANCE, stp);
		S_printSetKParam ("PARAM_MATERIAL_CLEARANCE", stp);
		params.setParam (util::PARAM_CHECK_CLEARANCE, 0.);//gparm[27]);
		S_printSetKParam ("PARAM_CHECK_CLEARANCE", 0.);//gparm[27]);
		params.setParam (util::PARAM_HIT_FLATS, kparm[14]);
		S_printSetKParam ("PARAM_HIT_FLATS", kparm[14]);
		if (gpos[7] > UM_FUZZ)
		{
			params.setParam(util::PARAM_FINAL_STEP_HEIGHT,gpos[7]);
			S_printSetGParam("PARAM_FINAL_STEP_HEIGHT",gpos[7]);
		}
	}
/*
.....2-axis Options only
*/
	else if (mode == 2)
	{
		params.setParam (util::PARAM_PROCESS_REGIONS_SEPARATELY, 1);
		S_printSetKParam ("PARAM_PROCESS_REGIONS_SEPARATELY", 1);
		params.setParam (util::PARAM_CHAINING_TOLERANCE, gparm[21]*2.);
		S_printSetGParam ("PARAM_CHAINING_TOLERANCE", gparm[21]*2.);
		params.setParam (util::PARAM_TOTAL_DEPTH, gpos[0]);
		S_printSetGParam ("PARAM_TOTAL_DEPTH", gpos[0]);
		params.setParam (util::PARAM_Z_STOCK_TO_LEAVE, gparm[7]);
		S_printSetGParam ("PARAM_Z_STOCK_TO_LEAVE", gparm[7]);
		params.setParam (util::PARAM_SMOOTH_PART_BOUNDARIES, kparm[1]);
		S_printSetKParam ("PARAM_SMOOTH_PART_BOUNDARIES", kparm[1]);
		params.setParam (util::PARAM_SMOOTH_PART_BOUNDARIES_ANGLE, gparm[6]);
		S_printSetGParam ("PARAM_SMOOTH_PART_BOUNDARIES_ANGLE", gparm[6]);
	}
	
	else if (mode == 5)
	{


		//params.setParam (util::PARAM_BOTTOM_MATERIAL_Z, gpos[6]);
		//S_printSetGParam ("PARAM_BOTTOM_MATERIAL_Z", gpos[6]);
		//params.setParam (util::PARAM_STOCK_TO_LEAVE, gparm[7]);
		//S_printSetGParam ("PARAM_STOCK_TO_LEAVE", gparm[7]);
		//i = (kparm[13] == 0)? 1 : 0;
		//params.setParam (util::PARAM_CAVITY_MILLING_ONLY, i);
		//S_printSetKParam ("PARAM_CAVITY_MILLING_ONLY", i);
		//ncl_vmill_get_expand(&use_stkoff,&stkoff);
		//if (use_stkoff) stp = gparm[27] + stkoff;
		//else stp = gparm[27];
		//params.setParam (util::PARAM_MATERIAL_CLEARANCE, stp);
		//S_printSetKParam ("PARAM_MATERIAL_CLEARANCE", stp);
		//params.setParam (util::PARAM_CHECK_CLEARANCE, 0.);//gparm[27]);
		//S_printSetKParam ("PARAM_CHECK_CLEARANCE", 0.);//gparm[27]);
		//params.setParam (util::PARAM_HIT_FLATS, kparm[14]);
		//S_printSetKParam ("PARAM_HIT_FLATS", kparm[14]);

		//if (gpos[7] > UM_FUZZ)
		//{
		//	params.setParam(util::PARAM_FINAL_STEP_HEIGHT,gpos[7]);
		//	S_printSetGParam("PARAM_FINAL_STEP_HEIGHT",gpos[7]);
		//}

		// The block above is copy from case 3

		//params.setParam (util::PARAM_DEPTH_OF_CUT, gparm[0]);
		if (gparm[0]<0.0)
		{
			params.setParam (util::PARAM_DEPTH_OF_CUT, -gparm[0]);
			S_printSetGParam ("PARAM_DEPTH_OF_CUT", -gparm[0]);
		}

		////params.setParam (util::PARAM_BOTTOM_MATERIAL_Z, 0.05);
		params.setParam (util::PARAM_BOTTOM_MATERIAL_Z, -0.01);

		ncl_vmill_get_expand(&use_stkoff,&stkoff);
		if (use_stkoff) stp = gparm[27] + stkoff;
		else stp = gparm[27];
		
		////params.setParam (util::PARAM_BOTTOM_MATERIAL_Z, gpos[6]);
		
		params.setParam (util::PARAM_MATERIAL_CLEARANCE, stp);
	   params.setParam (util::PARAM_CHECK_CLEARANCE, 0.0);
	   params.setParam (util::PARAM_CAVITY_MILLING_ONLY, i);
	  // params.setParam (util::PARAM_CAVITY_MILLING_ONLY, 0);
	   params.setParam (util::PARAM_HOLDER_CLEARANCE, 0.0);
	   params.setParam (util::PARAM_SHANK_CLEARANCE, 0.0);
	   params.setParam (util::PARAM_HOLDER_DXF_PATH, "");

	 
	   params.setParam (util::PARAM_FINAL_STEP_HEIGHT, 0.0);
	  
	   params.setParam (util::PARAM_STOCK_TO_LEAVE,  0.05);	//Evans recommendation, Mar.23, 2021
	  
	   params.setParam (util::PARAM_Z_STOCK_TO_LEAVE, 0.05);
	  
	   params.setParam (util::PARAM_DRILL_DIAMETER, 0.1);
	   params.setParam (util::PARAM_DRILL_ANGLE, 140.0);
	   params.setParam (util::PARAM_DRILL_POINT_RANGE, 500.0);
	   params.setParam (util::PARAM_DETECT_OPTIMAL_PREDRILL_POINTS, 1);


	   params.setParam (util::PARAM_ADJUST_FEEDRATE_HELIX, 1);
	   params.setParam (util::PARAM_ADJUST_FEEDRATE_RAMP, 1);
	   params.setParam (util::PARAM_USE_FREEWAY_LINKING, 1);
	  
	   params.setParam (util::PARAM_CONTOUR_RAMPING, 0);
	   params.setParam (util::PARAM_ALLOW_INITIAL_SPIRAL, 1);
	   
	   params.setParam (util::PARAM_ZAG_FEEDRATE_PERCENT, 0.0);
	   params.setParam (util::PARAM_RACETRACK_FEEDRATE, 0.0);
	   params.setParam (util::PARAM_HIGH_FEEDRATE_Z_NEGATIVE, 0.0);
	 
	   params.setParam (util::PARAM_LINEARIZATION_TOLERANCE, 0.0);
	   params.setParam (util::PARAM_MAXIMUM_SEGMENT_LENGTH, 0.0);
	   params.setParam (util::PARAM_SIDE_MILL_SLOT_MILL_RATIO, 1.0);

	   params.setParam (util::PARAM_REPOSITIONING_RADIUS, 0.1);
	   params.setParam (util::PARAM_REPOSITIONING_ANGLE, 6.0);
	   params.setParam (util::PARAM_REPOSITIONING_TESSELLATION_ANGLE, 20.0);
	   params.setParam (util::PARAM_MIN_REPOSITIONING_RADIUS, 0.001);
	   params.setParam (util::PARAM_REPOSITIONING_KEEP_RAPID, 0);
	   params.setParam (util::PARAM_REPOSITIONING_2D_ONLY, 0);

	   params.setParam (util::PARAM_TOOLPATH_EVENTS, 1);
	   //@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	   params.setParam (util::PARAM_TOOL_NUMBER, 1);
      params.setParam (util::PARAM_TOOL_LENGTH_OFFSET, 1);
      params.setParam (util::PARAM_TOOL_WORK_OFFSET, "54");
      params.setParam (util::PARAM_TOOL_NUM_FLUTES, 4);
      params.setParam (util::PARAM_COOLANT, 1);
      params.setParam (util::PARAM_TOOL_DIAMETER, 0.5);
      params.setParam (util::PARAM_TOOL_CORNER_RADIUS, 0.0);
      params.setParam (util::PARAM_TOOL_LENGTH_OF_CUT, 1.25);
      params.setParam (util::PARAM_TOOL_OVERALL_LENGTH, 1.5);
      params.setParam (util::PARAM_SPINDLE_SPEED, 10000.0);
      params.setParam (util::PARAM_FEEDRATE, 100.0);
      params.setParam (util::PARAM_HIGH_FEEDRATE, 500.0);
      params.setParam (util::PARAM_RAMP_FEEDRATE, 50.0);
      params.setParam (util::PARAM_TOOLPATH_CCW, 1);

      params.setParam (util::PARAM_RAPID_PLANE_Z, 1.0);
      params.setParam (util::PARAM_INITIAL_RAPID_PLANE_Z, 1.0);
      params.setParam (util::PARAM_FINAL_RAPID_PLANE_Z, 1.0);
      params.setParam (util::PARAM_TOP_MATERIAL_Z, 0.0);
      //params.setParam (util::PARAM_BOTTOM_MATERIAL_Z, -1.0);
      params.setParam (util::PARAM_PLUNGE_CLEARANCE, 0.1);
      params.setParam (util::PARAM_REPOSITION_CLEARANCE, 0.01);
      //params.setParam (util::PARAM_MATERIAL_CLEARANCE, 0.0);
      //params.setParam (util::PARAM_CHECK_CLEARANCE, 0.0);
      //params.setParam (util::PARAM_CAVITY_MILLING_ONLY, 0);
      params.setParam (util::PARAM_HOLDER_CLEARANCE, 0.0);
      params.setParam (util::PARAM_SHANK_CLEARANCE, 0.0);
      params.setParam (util::PARAM_HOLDER_DXF_PATH, "");
      //////params.setParam (util::PARAM_HOLDER_3D_PATH, "");

      params.setParam (util::PARAM_STEPOVER, 0.15);
      //params.setParam (util::PARAM_DEPTH_OF_CUT, 0.0);
      params.setParam (util::PARAM_FINAL_STEP_HEIGHT, 0.0);
      params.setParam (util::PARAM_RAMP_ANGLE, 3.0);
      params.setParam (util::PARAM_XY_STOCK_TO_LEAVE, 0.01);
      //params.setParam (util::PARAM_Z_STOCK_TO_LEAVE, 0.01);
      params.setParam (util::PARAM_CUT_TOLERANCE, 0.01);
      params.setParam (util::PARAM_SMOOTHING_RADIUS, 0.225);

      params.setParam (util::PARAM_SIDE_MILL_STEPOVER, 0.15);
      params.setParam (util::PARAM_SLOT_DEPTH_OF_CUT, 0.25);
      params.setParam (util::PARAM_SLOT_FEEDRATE, 50.0);

      params.setParam (util::PARAM_PLUNGE_TYPE, 0);
      //params.setParam (util::PARAM_DRILL_DIAMETER, 0.0);
      params.setParam (util::PARAM_DRILL_ANGLE, 140.0);
      params.setParam (util::PARAM_DRILL_POINT_RANGE, 500.0);
      params.setParam (util::PARAM_DETECT_OPTIMAL_PREDRILL_POINTS, 1);

      params.setParam (util::PARAM_RAMP_SPINDLE_SPEED, 10000.0);
      params.setParam (util::PARAM_DWELL_AFTER_PLUNGE, 0.0);
      params.setParam (util::PARAM_DEPTH_FIRST, 1);

      params.setParam (util::PARAM_ADJUST_FEEDRATE_HELIX, 1);
      params.setParam (util::PARAM_ADJUST_FEEDRATE_RAMP, 1);
      params.setParam (util::PARAM_USE_FREEWAY_LINKING, 1);
      params.setParam (util::PARAM_XY_RAPID_FEEDRATE, 1000.0);
      params.setParam (util::PARAM_Z_RAPID_FEEDRATE, 1000.0);
      params.setParam (util::PARAM_CONTOUR_RAMPING, 0);
      params.setParam (util::PARAM_ALLOW_INITIAL_SPIRAL, 1);
      params.setParam (util::PARAM_MINIMUM_FEEDRATE, 0.0);
      params.setParam (util::PARAM_ZAG_FEEDRATE_PERCENT, 0.0);
      params.setParam (util::PARAM_RACETRACK_FEEDRATE, 0.0);
      params.setParam (util::PARAM_HIGH_FEEDRATE_Z_NEGATIVE, 0.0);
      params.setParam (util::PARAM_START_POINT_SORT_ANGLE, 0.0);
      params.setParam (util::PARAM_MINIMUM_POCKET_RADIUS, 0.225); // equals to smoothing radius
      params.setParam (util::PARAM_LINEARIZATION_TOLERANCE, 0.0);
      params.setParam (util::PARAM_MAXIMUM_SEGMENT_LENGTH, 0.0);
      params.setParam (util::PARAM_SIDE_MILL_SLOT_MILL_RATIO, 1.0);

      params.setParam (util::PARAM_REPOSITIONING_RADIUS, 0.1);
      params.setParam (util::PARAM_REPOSITIONING_ANGLE, 6.0);
      params.setParam (util::PARAM_REPOSITIONING_TESSELLATION_ANGLE, 20.0);
      params.setParam (util::PARAM_MIN_REPOSITIONING_RADIUS, 0.001);
      params.setParam (util::PARAM_REPOSITIONING_KEEP_RAPID, 0);
      params.setParam (util::PARAM_REPOSITIONING_2D_ONLY, 0);

      params.setParam (util::PARAM_TOOLPATH_EVENTS, 1);
      params.setParam (util::PARAM_CUTTING_TECHNIQUE, 0);
   //}

   
	 //  if (gpos[7] > UM_FUZZ)
		//{
		//	params.setParam(util::PARAM_FINAL_STEP_HEIGHT,gpos[7]);
		//	//S_printSetGParam("PARAM_FINAL_STEP_HEIGHT",gpos[7]);
		//}

	 //  params.setParam (util::PARAM_BOTTOM_MATERIAL_Z, gpos[6]);
		//S_printSetGParam ("PARAM_BOTTOM_MATERIAL_Z", gpos[6]);
		//params.setParam (util::PARAM_STOCK_TO_LEAVE, gparm[7]);
		//S_printSetGParam ("PARAM_STOCK_TO_LEAVE", gparm[7]);
		//i = (kparm[13] == 0)? 1 : 0;
		//params.setParam (util::PARAM_CAVITY_MILLING_ONLY, i);
		//S_printSetKParam ("PARAM_CAVITY_MILLING_ONLY", i);
		//ncl_vmill_get_expand(&use_stkoff,&stkoff);
		//if (use_stkoff) stp = gparm[27] + stkoff;
		//else stp = gparm[27];
		//params.setParam (util::PARAM_MATERIAL_CLEARANCE, stp);
		//S_printSetKParam ("PARAM_MATERIAL_CLEARANCE", stp);
		//params.setParam (util::PARAM_CHECK_CLEARANCE, 0.);//gparm[27]);
		//S_printSetKParam ("PARAM_CHECK_CLEARANCE", 0.);//gparm[27]);
		//params.setParam (util::PARAM_HIT_FLATS, kparm[14]);
		//S_printSetKParam ("PARAM_HIT_FLATS", kparm[14]);
		//if (gpos[7] > UM_FUZZ)
		//{
		//	params.setParam(util::PARAM_FINAL_STEP_HEIGHT,gpos[7]);
		//	S_printSetGParam("PARAM_FINAL_STEP_HEIGHT",gpos[7]);
		//}

	}
}

/*********************************************************************
**    E_FUNCTION     : S_getBoundaryChains (nbound4,kbound8,token,ivxsub,
**                                          pBoundaries)
**       Convert NCL pocket boundary to VoluMill boundary input. 
**       INPUT  :
**          nbound4       - Number of boundaries.
**          kbound8       - Boundary keys.
**          token         - Labels of boundaries.
**          ivxsub        - Subscripts for labels.
**       OUTPUT :
**          pBoundaries   - VoluMill boundary
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_getBoundaryChains (UM_int2 *nbound4, UM_real8 *kbound8,
	char token[120][64], UM_int4 *ivxsub, UU_REAL dtol,
   exchange::VoluMill2dBoundaries* pBoundaries)
{
	int i,j,k,lanes,status,stat,iLoop,cstk;
	UU_REAL asw,buf[14],tol,radius,dang,length;
	UM_int4 nclkey,keyout;
	UM_int2 nwds,nw,ietype,i2,irev,ix,ierr,pntype,itype,i0=0,jnext,lcirc;
	UM_real8 ver,tend[6], ttol;
	UM_coord spt,ept,center;
	UM_vector nvec;
	UU_LIST openlst;
	UU_LOGICAL open,*openfl,ver_101;
	char sbuf[132];
	union {UU_REAL asn; UM_int2 ksn[4];} tv;
/*
.....Initialize routine
.......Added the option to label open sides of boundary defined by
.......composite curve or point array.  Also added new handling of
.......waterline stock.  The boundary defined by the stock will now
.......be treated as open - ASF 11/21/13.
*/
	i2 = 169; getsc(&i2,&ver);
	ver_101 = (ver > 10.049);
	status = UU_FAILURE;
	lanes = *nbound4;
	iLoop = 0;
	ncl_get_tstk(&cstk);
	uu_list_init(&openlst,sizeof(UU_LOGICAL),200,200);
#if DEBUGL
			NclxDbgPstr("*stop");
			NclxDbgPstr("erase/all");
#endif
/*
.....Loop through boundaries
*/
	for (i=0; i<lanes; i++)
	{
		S_printLog("	");
		asw = kbound8[i];			
		gtdesc(asw,nclkey,nwds,ietype);
		std::vector<geom::CurvePtr2d> curves;
		S_printLog("{");
		S_printLog("	std::vector<geom::CurvePtr2d> curves;");
		switch (ietype)
		{
/*
.....Curve or Composite curve
*/
		case NCLI_CURVE:
			UM_int2 jcomp;	
			UM_int2 nents,ierr;
			iscmpc(&nclkey,&jcomp);
/*
........Composite curve
*/
			if (jcomp == 1)
			{
				gtccnm(&nclkey, &nents);
/*
.......Loop through curve components
*/
				for (i2=1; i2<=nents; i2++)
				{
					gtcntt(&nclkey,&i2,&keyout,buf,&ietype,&i0,&ierr);
					open = ncl_genpocket_check_index(i2-1);
/*
...........Line
*/
					if (ietype == NCLI_LINE)
					{			
						if (Stranfl)
						{
							itype = 3;
							vmptrn(buf,&itype);
							vmptrn(&buf[3],&itype);
						}
						um_vctovc(&buf[3],ept);
						stat = S_makeline(&curves,buf,ept,dtol,-1);
						if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
					}
/*
...........Circle
*/
					else if (ietype == NCLI_CIRCLE)
					{
						if (Stranfl)
						{
							itype = 3;
							vmptrn(buf,&itype);
							vmptrn(&buf[3],&itype);
							vmptrn(&buf[6],&itype);
							itype = 4;
							vmptrn(&buf[9],&itype);
						}
						geom::Point2d startPt(buf[0],buf[1]);
						geom::Point2d centerPt(buf[3],buf[4]);
						geom::Point2d endPt(buf[6],buf[7]);
						if (buf[11] < 0.) buf[13] = buf[13] * -1.;
						if (abs(buf[13]) == UM_TWOPI)
						{
							curves.push_back(geom::makeCircle(centerPt,buf[12]));
							uu_list_push(&openlst,(char *)&open);
							S_printMakeCircle(i,&buf[0],buf[12],0.);
						}
						else if (buf[13] > 0.0)
						{
							curves.push_back(geom::makeCCWArcCenter(centerPt,
								startPt,endPt));
							uu_list_push(&openlst,(char *)&open);
							S_printMakeCCWArc(&buf[3],&buf[0],&buf[6]);
						}
						else											
						{
							curves.push_back(geom::makeCWArcCenter(centerPt,
								startPt,endPt));
							uu_list_push(&openlst,(char *)&open);
							S_printMakeCWArc(&buf[3],&buf[0],&buf[6]);
						}
						um_vctovc(&buf[6],ept);
					}
/*
...........Curve
*/
					else
					{
						tol = .001;
						irev = 0;
						if (keyout < 0)
						{
							irev = -1;
							keyout = -keyout;
						}
						cptint(&keyout,&tol,&irev,&nwds);
						for (j=0;j<nwds;j++)
						{
							cptnxt(buf,&buf[4]);
							if (wcsact()) ncl_wcstomcs(0,buf,buf);
							if (Stranfl)
							{
								itype = 3;
								vmptrn(buf,&itype);
							}
							if (j != 0 || i2 != 1)
							{
								stat = S_makeline(&curves,ept,buf,dtol,-1);
								if (stat == UU_SUCCESS)
									uu_list_push(&openlst,(char *)&open);
							}
							else
								um_vctovc(buf,ept);
						}
						cptend();
					}
				}
			}
/*
........Curve
*/
			else
			{
				tol = .001;
				irev = 0; open = UU_FALSE;
				cptint(&nclkey,&tol,&irev,&nwds);
				for (j=0;j<nwds;j++)
				{
					cptnxt(buf,&buf[4]);
					if (wcsact()) ncl_wcstomcs(0,buf,buf);
					if (Stranfl)
					{
						itype = 3;
						vmptrn(buf,&itype);
					}
					if (j == 0)
					{
						um_vctovc(buf,spt);
						um_vctovc(buf,ept);
					}
					else
					{
						stat = S_makeline(&curves,ept,buf,dtol,-1);
						if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
					}
				}
				stat = S_makeline(&curves,ept,spt,dtol,-1);
				if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
				cptend();
			}
			break;
/*
.....Circle
*/
		case NCLI_CIRCLE:
		{
			um_get_circle_data(nclkey,center,&radius,&dang,nvec,spt,ept,&length);
			if (wcsact())
			{
				ncl_wcstomcs(0,spt,spt);
				ncl_wcstomcs(0,ept,ept);
				ncl_wcstomcs(0,center,center);
				ncl_wcstomcs(1,nvec,nvec);
			}
			if (Stranfl)
			{
				itype = 3;
				vmptrn(spt,&itype);
				vmptrn(ept,&itype);
				vmptrn(center,&itype);
				itype = 4;
				vmptrn(nvec,&itype);
			}
			geom::Point2d centerPt(center[0],center[1]);
			curves.push_back(geom::makeCircle(centerPt,radius));
			open = UU_FALSE; uu_list_push(&openlst,(char *)&open);
			S_printMakeCircle(i,center,radius,0.);
			break;
		}
/*
.....Point array
*/
		case NCLI_RESERVED_ID:
			tv.asn = asw;
			nwds = tv.ksn[0];
			for (j=1;j<=nwds;j++)
			{
				status = vxchk(token[i],&j,&nclkey,&k,&k,&nw,&ietype);
				if (status == UU_SUCCESS && nclkey != 0 && ietype == NCLI_POINT)
				{
					gtgeo(&nclkey,buf);
					if (wcsact()) ncl_wcstomcs(0,buf,buf);
					if (Stranfl)
					{
						itype = 3;
						vmptrn(buf,&itype);
					}
					if (j == 1)
					{
						um_vctovc(buf,spt);
						um_vctovc(buf,ept);
					}
					else
					{
						open = (ncl_genpocket_check_index(j-1) &&
									ncl_genpocket_check_index(j-2));
						stat = S_makeline(&curves,ept,buf,dtol,-1);
						if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
					}
				}
			}
			open = (ncl_genpocket_check_index(0) &&
						ncl_genpocket_check_index(nwds-1));
			stat = S_makeline(&curves,ept,spt,dtol,-1);
			if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
			break;
/*
.....Pattern
*/
		case NCLI_PATERN:
			gtpnnp (&nclkey,&nwds,&pntype);
			irev = 0; open = UU_FALSE;
			for (i2=1;i2<=nwds;i2++)
			{
				gpnptt(buf,&nclkey,&i2,&irev);
				if (Stranfl)
				{
					itype = 3;
					vmptrn(buf,&itype);
				}
				if (i2 == 1)
				{
					um_vctovc(buf,spt);
					um_vctovc(buf,ept);
				}
				else
				{
					stat = S_makeline(&curves,ept,buf,dtol,-1);
					if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
				}
			}
			stat = S_makeline(&curves,ept,spt,dtol,-1);
			if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
			break;
/*
.....Point vector
*/
		case NCLI_POINTVEC:
			ix = i+1; open = UU_FALSE;
			gtnpt (&ix,&nwds,&ierr);
			 
			for (i2=1;i2<=nwds;i2++)
			{
				gtpts(buf,&ix,&i2,&ierr);
				if (wcsact()) ncl_wcstomcs(0,buf,buf);
				if (Stranfl)
				{
					itype = 3;
					vmptrn(buf,&itype);
				}
				if (i2 == 1)
				{
					um_vctovc(buf,spt);
					um_vctovc(buf,ept);
				}
				else
				{
					stat = S_makeline(&curves,ept,buf,dtol,-1);
					if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
				}
			}
			stat = S_makeline(&curves,ept,spt,dtol,-1);
			if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
			break;
/*
.....Surface
*/
		case NCLI_SURF:
		case NCLI_POCKET:
			ix = i + 1;
			open = UU_FALSE;
			getend(tend);
//tend[3] = 0; tend[4] = 0.; tend[5] = 1.;
			i2 = 0;
			if (ietype == NCLI_SURF)
			{
				nclf_genpocket_bound(&nclkey,&tend[3],&ix,&nwds,&i2,&ierr);
				if (ix == 1) ncl_genpocket_mark_open();
			}
			else
			{
				nclf_genpocket_open(&tend[3],&nwds,&i2,&ierr);
				if (ix == 1) ncl_genpocket_mark_open();
			}
			if (ierr != 0) goto failed;
			for (j=1;j<=nwds;j++)
			{
				i2 = j;
				nclf_genpocket_getpt(&i2,buf);
				if (wcsact()) ncl_wcstomcs(0,buf,buf);
				if (Stranfl)
				{
					itype = 3;
					vmptrn(buf,&itype);
				}
				if (j == 1)
				{
					um_vctovc(buf,spt);
					um_vctovc(buf,ept);
				}
				else
				{
					if (ix == 1)
						open = (ncl_genpocket_check_index(j-1) &&
									ncl_genpocket_check_index(j-2));
#if DEBUGL
					if (open) NclxDbgPstr("DRAFT/MODIFY,COLOR=ltblue");
					else NclxDbgPstr("DRAFT/MODIFY,COLOR=blue");
					if (!um_cceqcc_tol(ept,buf,dtol))
					{
						sprintf(sbuf,"LINE/%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f",
							ept[0],ept[1],ept[2],buf[0],buf[1],buf[2]);
						NclxDbgPstr(sbuf);
						sprintf(sbuf,"POINT/%10.4f,%10.4f,%10.4f",buf[0],buf[1],
							buf[2]);
						NclxDbgPstr(sbuf);
					}
#endif
					stat = S_makeline(&curves,ept,buf,dtol,-1);
					if (stat!=UU_SUCCESS)
					{
						ttol = sqrt((ept[0]-buf[0])*(ept[0]-buf[0])+(ept[1]-buf[1])*(ept[1]-buf[1])+(ept[2]-buf[2])*(ept[2]-buf[2]));
						dtol=ttol/2.0;
						stat = S_makeline(&curves,ept,buf,dtol,-1);
					}
					if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
				}
			}
			if (ix == 1)
				open = (ncl_genpocket_check_index(0) &&
							ncl_genpocket_check_index(nwds-1));
#if DEBUGL
				if (open) NclxDbgPstr("DRAFT/MODIFY,COLOR=ltblue");
				else NclxDbgPstr("DRAFT/MODIFY,COLOR=blue");
				sprintf(sbuf,"LINE/%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f",
					ept[0],ept[1],ept[2],spt[0],spt[1],spt[2]);
				NclxDbgPstr(sbuf);
#endif
			stat = S_makeline(&curves,ept,spt,dtol,-1);
			if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
			ncl_genpocket_reset();
			nclf_clear_ptlst();
			break;
/*
.....Waterline data
*/
		case 0:
			ix = i + 1;
			jnext = 0;
			wgtnpt(&ix,&nwds,&lcirc,&ierr);
			if (ierr != 0) goto failed;
			if (i == 0 && Scalls == cstk && ver_101 && Soffpart) open = UU_TRUE;
			else open = UU_FALSE; 
			if (lcirc == 1)
			{
				wgtci(buf,&ix,&ierr);
				if (ierr != 0) goto failed;
				UM_cc_exttoint(buf,buf);
				UM_len_exttoint(buf[6],buf[6]);
				if (Stranfl)
				{
					itype = 3;
					vmptrn(buf,&itype);
				}
				geom::Point2d centerPt(buf[0],buf[1]);
				curves.push_back(geom::makeCircle(centerPt,buf[6]));
				uu_list_push(&openlst,(char *)&open);
				S_printMakeCircle(i,buf,buf[6],Slevel);
#if UU_DEBUGL
				if (open) NclxDbgPstr("DRAFT/MODIFY,COLOR=ltblue");
				else NclxDbgPstr("DRAFT/MODIFY,COLOR=blue");
				sprintf(sbuf,
					"CIRCLE/CANON,%10.6f,%10.6f,%10.6f,%10.6f,%10.6f,%10.6f,%10.6f",
					buf[0],buf[1],buf[2],buf[3],buf[4],buf[5],buf[6]);
#endif
/*
				Gwpoint3 gpt[500];
				struct UM_circle_rec e;
				int n;
				e.svec[0] = 1.; e.svec[1] = 0.; e.svec[2] = 0.;
				e.center[0] = buf[0]; e.center[1] = buf[1]; e.center[2] = Slevel;
				e.nvec[0] = 0.; e.nvec[1] = 0.; e.nvec[2] = 1.;
				e.radius = buf[6];
				e.dang = UM_TWOPI;
				n = 20;
				ncl_cutter_circle(&e,gpt,n);
				for (k=0;k<n-1;k=k+1)
					S_makeline(&curves,(UU_REAL *)&gpt[k],(UU_REAL *)&gpt[k+1],dtol,k);
*/
			}
			else
			{
#if DEBUGL
				if (open) NclxDbgPstr("DRAFT/MODIFY,COLOR=seagrn");
				else NclxDbgPstr("DRAFT/MODIFY,COLOR=green");
#endif
				spt[2] = ept[2] = buf[2] = Slevel;
				for (i2=1;i2<=nwds;i2++)
				{
					wgtpt(buf,&ix,&i2,&ierr,&lcirc,&jnext);
					if (ierr != 0) goto failed;
					UM_cc_exttoint(buf,buf);
					if (Stranfl)
					{
						itype = 3;
						vmptrn(buf,&itype);
					}
					if (i2 == 1)
					{
						um_vctovc(buf,spt);
						um_vctovc(buf,ept);
					}
					else
					{
#if DEBUGL
				sprintf(sbuf,"LINE/%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f",
					ept[0],ept[1],ept[2],buf[0],buf[1],buf[2]);
				NclxDbgPstr(sbuf);
#endif
						stat = S_makeline(&curves,ept,buf,dtol,i);
						if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
					}
				}
#if DEBUGL
/*
				sprintf(sbuf,"LINE/%10.4f,%10.4f,%10.4f,%10.4f,%10.4f,%10.4f",
					ept[0],ept[1],ept[2],spt[0],spt[1],spt[2]);
				NclxDbgPstr(sbuf);
*/
#endif
				stat = S_makeline(&curves,ept,spt,dtol,i);
				if (stat == UU_SUCCESS) uu_list_push(&openlst,(char *)&open);
			}
			break;
/*
........Unrecognized entity
*/
		default:
			break;
		}
/*
.....Pass curves to VoluMill
*/
		openfl = (UU_LOGICAL *) UU_LIST_ARRAY (&openlst);
		for (int kk=0; kk < (int) curves.size(); ++kk)        
		{
/*
.....Open curve.
*/
			if (openfl[kk])
			{
				pBoundaries->m_materialBoundaries.push_back (curves[kk]);
				pBoundaries->m_materialBoundaries.back()->setParam
					(util::PARAM_LOOP_ID, iLoop);
				pBoundaries->m_materialBoundaries.back()->deleteParam
					(util::PARAM_Z_MAX);
			}
/*
.....Closed curve.
*/
			else
			{
				pBoundaries->m_partBoundaries.push_back (curves[kk]);
				pBoundaries->m_partBoundaries.back()->setParam
					(util::PARAM_LOOP_ID, iLoop);
				pBoundaries->m_partBoundaries.back()->deleteParam
					(util::PARAM_Z_MAX);
			}
		}
		S_printCurves();
		S_printLog("}");
		++iLoop;
		UU_LIST_EMPTY(&openlst);
	}
	goto done;
/*
.....Could not get pocket boundary
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	uu_list_free(&openlst);
	ncl_genpocket_reset();
	nclf_clear_ptlst();
	return(status);
}
/*********************************************************************
**    E_FUNCTION     : S_getBoundaryChains3D (pBoundaries,dtol)	// Need to write S_getBoundaryChains5D with Strafl = 1
**       Convert NCL pocket boundary to VoluMill boundary input. 
**       INPUT  :
**          dtol       - Tolerance.
**       OUTPUT :
**          pBoundaries   - VoluMill 3D boundary
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_getBoundaryChains3D (exchange::VoluMill3dBoundaries *pBoundaries,	/* VoluMill5dBoundaries also available*/
	UU_REAL dtol)
{
		//
	int i,npts,status,numa,numb,numc;
	UM_int2 itype;
	UM_tessellation *tess;
	UM_coord *verts;
	UM_tript *tript;
	UU_REAL box[6];
	UU_LIST *stock;
	int boxfl;
	UM_coord spt,ept;
	UM_2Dcoord *pts;
	geom::Point3d pt1,pt2;
	char sbuf[80];

	status = UU_SUCCESS;
	ncl_vmill_get_box(box);
	if (Stranfl)
	{
		itype = 3;
		vmptrn(box,&itype);
		vmptrn(&box[3],&itype);
	}
/*
.....Build box for stock.
*/
	ncl_vmill_get_boxfl(&boxfl);
	if (boxfl == 1)
	{
		pt1[0] = box[0]; pt1[1] = box[1]; pt1[2] = box[2];
		pt2[0] = box[3]; pt2[1] = box[4]; pt2[2] = box[5];
		boost::shared_ptr<topo::mesh::TriMesh3d> pMesh;
		rectangleToMesh (pt1,pt2,&pMesh);
		topo::mesh::triMeshToTriMeshFacets3d (pMesh, 
			&pBoundaries->m_pMaterialMesh);
	}
/*
.....Use contour to define stock.
*/
	else if (boxfl == 0)
	{
		ncl_get_stklst(&stock);
		pts = (UM_2Dcoord *)UU_LIST_ARRAY(stock);
		npts = stock->cur_cnt;
		spt[2] = ept[2] = box[2];
		std::vector<geom::CurvePtr2d> curves;
		spt[0] = pts[0][0]; spt[1] = pts[0][1];
		UM_cc_exttoint(spt,spt);
		if (Stranfl)
		{
			itype = 3;
			vmptrn(spt,&itype);
		}
		for (i=0;i<npts-1;i++)
		{
			ept[0] = pts[i+1][0]; ept[1] = pts[i+1][1]; ept[2] = box[2];
			UM_cc_exttoint(ept,ept);
			if (Stranfl)
			{
				itype = 3;
				vmptrn(ept,&itype);
			}
			S_makeline(&curves,spt,ept,dtol,0);
		}
		ept[0] = pts[0][0]; ept[1] = pts[0][1]; ept[2] = box[2];
		UM_cc_exttoint(ept,ept);
		if (Stranfl)
		{
			itype = 3;
			vmptrn(ept,&itype);
		}
		S_makeline(&curves,spt,ept,dtol,0);
		for (i=0;i<npts-1;i++) 
		{
			pBoundaries->m_materialBoundaries.push_back (curves[i]);
			pBoundaries->m_materialBoundaries.back()->setParam
				(util::PARAM_LOOP_ID, 0);
			pBoundaries->m_materialBoundaries.back()->deleteParam
				(util::PARAM_Z_MAX);
		}
	}
/*
.....Use stock mesh.
*/
	else if (boxfl == 2)
	{
		ncl_vmill_get_tess(&tess,1);
		boost::shared_ptr<topo::mesh::TriMeshFacets3d> *tpMesh;
		tpMesh = &pBoundaries->m_pMaterialMesh;
		tpMesh->reset (new topo::mesh::TriMeshFacets3d());
		tript = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
		verts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
/*
.....Store tessellation points.
*/
		for (i=0;i<tess->np;i++)
		{
			if (Stranfl)
			{
				itype = 3;
				vmptrn(verts[i],&itype);
			}
			pt1[0] = verts[i][0]; pt1[1] = verts[i][1]; pt1[2] = verts[i][2];
			pBoundaries->m_pMaterialMesh->m_vertices.push_back(pt1);
		}
/*
.....Store tessellation facets.
*/
		for (i=0;i<tess->ntri;i++)
		{
			topo::mesh::TriMeshFacet tr (tript[i].n1,tript[i].n2,tript[i].n3);
			pBoundaries->m_pMaterialMesh->m_faces.push_back(tr);
		}
	}
/*
.....Store part surface tesselation.
*/
	ncl_vmill_get_tess(&tess,0);
	boost::shared_ptr<topo::mesh::TriMeshFacets3d> *ppMesh;
	ppMesh = &pBoundaries->m_pPartMesh;
	ppMesh->reset (new topo::mesh::TriMeshFacets3d());
	tript = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
	verts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
/*
.....Store tessellation points.
*/
	for (i=0;i<tess->np;i++)
	{
		if (Stranfl)
		{
			itype = 3;
			vmptrn(verts[i],&itype);
		}
		pt1[0] = verts[i][0]; pt1[1] = verts[i][1]; pt1[2] = verts[i][2];
		pBoundaries->m_pPartMesh->m_vertices.push_back(pt1);
	}
/*
.....Store tessellation facets.
*/
	for (i=0;i<tess->ntri;i++)
	{
		topo::mesh::TriMeshFacet tr (tript[i].n1,tript[i].n2,tript[i].n3);
		pBoundaries->m_pPartMesh->m_faces.push_back(tr);
	}
/*
.....Check surfaces
*/
	ncl_wat_get_sfnums (&numa,&numb,&numc);
	if (numc > 0)
	{
		ncl_vmill_get_tess(&tess,2);
		ppMesh = &pBoundaries->m_pCheckMesh;
		ppMesh->reset (new topo::mesh::TriMeshFacets3d());
		tript = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
		verts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
/*
.....Store tessellation points.
*/
		for (i=0;i<tess->np;i++)
		{
			if (Stranfl)
			{
				itype = 3;
				vmptrn(verts[i],&itype);
			}
			pt1[0] = verts[i][0]; pt1[1] = verts[i][1]; pt1[2] = verts[i][2];
			pBoundaries->m_pCheckMesh->m_vertices.push_back(pt1);
		}
/*
.....Store tessellation facets.
*/
		for (i=0;i<tess->ntri;i++)
		{
			topo::mesh::TriMeshFacet tr (tript[i].n1,tript[i].n2,tript[i].n3);
			pBoundaries->m_pCheckMesh->m_faces.push_back(tr);
		}
	}
/*.....Sample defining drill holes
	exchange::VoluMillDrillHole drillHole;
	drillHole.m_center[0] = -1.5;
	drillHole.m_center[1] = 1.5;
	drillHole.m_usableDepth = 25.0;
//means drill cut all the way through the part if TRUE
	drillHole.m_through = UU_FALSE; 
	input.m_preDrilledHoles.push_back(drillHole);
*/
	goto done;
/*
.....Could not get pocket boundary
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

int S_getBoundaryChains5x (exchange::VoluMill5xBoundaries *pBoundaries,	/* VoluMill5dBoundaries also available*/
	UU_REAL dtol)
{
		//
	int i,npts,status,numa,numb,numc;
	UM_int2 itype;
	UM_tessellation *tess;
	UM_coord *verts, *mverts;
	UM_tript *tript, *mtript;
	UU_REAL box[6];
	UU_LIST *stock;
	int boxfl;
	UM_coord spt,ept;
	UM_2Dcoord *pts;
	geom::Point3d pt1,pt2;
	char sbuf[80];

	status = UU_SUCCESS;
	Stranfl = 1;
	ncl_vmill_get_box(box);

	/*pt1[0] = box[0]; pt1[1] = box[1]; pt1[2] = box[2];
	pt2[0] = box[3]; pt2[1] = box[4]; pt2[2] = box[5];
	pBoundaries->m_pMaterialMesh->m_vertices.push_back(pt1);
	pBoundaries->m_pMaterialMesh->m_vertices.push_back(pt2);
	topo::mesh::TriMeshFacet m_tr1 (pt1[0],pt1[1],pt1[2]);
	pBoundaries->m_pMaterialMesh->m_faces.push_back(m_tr1);
	topo::mesh::TriMeshFacet m_tr2 (pt2[0],pt2[1],pt2[2]);
	pBoundaries->m_pMaterialMesh->m_faces.push_back(m_tr2);*/

	if (Stranfl)
	{
		itype = 3;
		vmptrn(box,&itype);
		vmptrn(&box[3],&itype);
	}
/*
.....Build box for stock.
*/
	ncl_vmill_get_boxfl(&boxfl);
	if (boxfl == 1)
	{
		pt1[0] = box[0]; pt1[1] = box[1]; pt1[2] = box[2];
		pt2[0] = box[3]; pt2[1] = box[4]; pt2[2] = box[5];
		boost::shared_ptr<topo::mesh::TriMesh3d> pMesh;
		rectangleToMesh (pt1,pt2,&pMesh);
		topo::mesh::triMeshToTriMeshFacets3d (pMesh, 
			&pBoundaries->m_pMaterialMesh);
	}
/*
.....Use contour to define stock.
*/
	else if (boxfl == 0)
	{

		pt1[0] = box[0]; pt1[1] = box[1]; pt1[2] = box[2];
		pt2[0] = box[3]; pt2[1] = box[4]; pt2[2] = box[5];
		boost::shared_ptr<topo::mesh::TriMesh3d> pMesh;
		rectangleToMesh (pt1,pt2,&pMesh);
		topo::mesh::triMeshToTriMeshFacets3d (pMesh, 
			&pBoundaries->m_pMaterialMesh);

		ncl_get_stklst(&stock);
		pts = (UM_2Dcoord *)UU_LIST_ARRAY(stock);
		npts = stock->cur_cnt;
		spt[2] = ept[2] = box[2];
		std::vector<geom::CurvePtr2d> curves;
		spt[0] = pts[0][0]; spt[1] = pts[0][1];
		UM_cc_exttoint(spt,spt);
		if (Stranfl)
		{
			itype = 3;
			vmptrn(spt,&itype);
		}
		for (i=0;i<npts-1;i++)
		{
			ept[0] = pts[i+1][0]; ept[1] = pts[i+1][1]; ept[2] = box[2];
			UM_cc_exttoint(ept,ept);
			if (Stranfl)
			{
				itype = 3;
				vmptrn(ept,&itype);
			}
			S_makeline(&curves,spt,ept,dtol,0);
		}
		ept[0] = pts[0][0]; ept[1] = pts[0][1]; ept[2] = box[2];
		UM_cc_exttoint(ept,ept);
		if (Stranfl)
		{
			itype = 3;
			vmptrn(ept,&itype);
		}
		S_makeline(&curves,spt,ept,dtol,0);
		for (i=0;i<npts-1;i++) 
		{
			
			pBoundaries->m_containmentBoundaries.m_materialBoundaries.push_back (curves[i]);
			
			pBoundaries->m_containmentBoundaries.m_materialBoundaries.back()->setParam
				(util::PARAM_LOOP_ID, 0);
			pBoundaries->m_containmentBoundaries.m_materialBoundaries.back()->deleteParam
				(util::PARAM_Z_MAX);

			
		}

	}
/*
.....Use stock mesh.
*/
	else if (boxfl == 2)
	{
		ncl_vmill_get_tess(&tess,1);
		boost::shared_ptr<topo::mesh::TriMeshFacets3d> *tpMesh;
		tpMesh = &pBoundaries->m_pMaterialMesh;
		tpMesh->reset (new topo::mesh::TriMeshFacets3d());
		tript = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
		verts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
/*
.....Store tessellation points.
*/
		for (i=0;i<tess->np;i++)
		{
			if (Stranfl)
			{
				itype = 3;
				vmptrn(verts[i],&itype);
			}
			pt1[0] = verts[i][0]; pt1[1] = verts[i][1]; pt1[2] = verts[i][2];
			pBoundaries->m_pMaterialMesh->m_vertices.push_back(pt1);
		}
/*
.....Store tessellation facets.
*/
		for (i=0;i<tess->ntri;i++)
		{
			topo::mesh::TriMeshFacet tr (tript[i].n1,tript[i].n2,tript[i].n3);
			pBoundaries->m_pMaterialMesh->m_faces.push_back(tr);
		}
	}
/*
.....Store part surface tesselation.
*/
	ncl_vmill_get_tess(&tess,0);
	//topo::mesh::
	boost::shared_ptr<topo::mesh::TriMeshFacets3d> *ppMesh;
	ppMesh = &pBoundaries->m_pPartMesh;
	ppMesh->reset (new topo::mesh::TriMeshFacets3d());
	tript = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
	verts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);

	/*boost::shared_ptr<topo::mesh::TriMeshFacets3d> *pmMesh;
	pmMesh = &pBoundaries->m_pMaterialMesh;
	pmMesh->reset (new topo::mesh::TriMeshFacets3d());
	mtript = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
	mverts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);*/
/*
.....Store tessellation points.
*/
	for (i=0;i<tess->np;i++)
	{
		if (Stranfl)
		{
			itype = 3;
			vmptrn(verts[i],&itype);
		}
		pt1[0] = verts[i][0]; pt1[1] = verts[i][1]; pt1[2] = verts[i][2];
		pBoundaries->m_pPartMesh->m_vertices.push_back(pt1);
		pBoundaries->m_pMaterialMesh->m_vertices.push_back(pt1);
	}
/*
.....Store tessellation facets.
*/
	for (i=0;i<tess->ntri;i++)
	{
		topo::mesh::TriMeshFacet tr (tript[i].n1,tript[i].n2,tript[i].n3);
		pBoundaries->m_pPartMesh->m_faces.push_back(tr);
		pBoundaries->m_pMaterialMesh->m_faces.push_back(tr);
	}
/*
.....Check surfaces
*/
	ncl_wat_get_sfnums (&numa,&numb,&numc);
	if (numc > 0)
	{
		ncl_vmill_get_tess(&tess,2);
		ppMesh = &pBoundaries->m_pCheckMesh;
		ppMesh->reset (new topo::mesh::TriMeshFacets3d());
		tript = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
		verts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
/*
.....Store tessellation points.
*/
		for (i=0;i<tess->np;i++)
		{
			if (Stranfl)
			{
				itype = 3;
				vmptrn(verts[i],&itype);
			}
			pt1[0] = verts[i][0]; pt1[1] = verts[i][1]; pt1[2] = verts[i][2];
			pBoundaries->m_pCheckMesh->m_vertices.push_back(pt1);
		}
/*
.....Store tessellation facets.
*/
		for (i=0;i<tess->ntri;i++)
		{
			topo::mesh::TriMeshFacet tr (tript[i].n1,tript[i].n2,tript[i].n3);
			pBoundaries->m_pCheckMesh->m_faces.push_back(tr);
		}
	}
/*.....Sample defining drill holes
	exchange::VoluMillDrillHole drillHole;
	drillHole.m_center[0] = -1.5;
	drillHole.m_center[1] = 1.5;
	drillHole.m_usableDepth = 25.0;
//means drill cut all the way through the part if TRUE
	drillHole.m_through = UU_FALSE; 
	input.m_preDrilledHoles.push_back(drillHole);
*/
	goto done;
/*
.....Could not get pocket boundary
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}



/*********************************************************************
**    I_FUNCTION     : S_getPredrilledHoles(holes)
**       Store user defined pre-drilled holes in pocketing parameters.
**       INPUT  : none
**       OUTPUT :
**          holes     - Volumill structure to receive the pre-drilled
**                      holes.
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_getPredrilledHoles(exchange::VoluMillDrillHoles *holes)
{
	int i;
	UM_coord *pts;
	exchange::VoluMillDrillHole drillHole;
/*
.....Store the predrilled holes
*/
	pts = (UM_coord *)UU_LIST_ARRAY(&Sentry_list);
	for (i=0;i<Snentry;i++)
	{
		drillHole.m_center[0] = pts[i][0];
		drillHole.m_center[1] = pts[i][1];
		drillHole.m_usableDepth = fabs(Sgpl-pts[i][2]);
// drill all the way through the part if TRUE
		drillHole.m_through = UU_TRUE;
		holes->push_back(drillHole);
		S_printHole(&drillHole);
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_freePredrilledHoles()
**       Store user defined pre-drilled holes in pocketing parameters.
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_freePredrilledHoles()
{
/*
.....Store the predrilled holes
*/
	if (Snentry > 0) uu_list_free(&Sentry_list);
	Snentry = 0;
}

/*********************************************************************
**    I_FUNCTION     : S_openLog
**       Opens the output log file and writes the header.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_openLog()
{
#if DEBUGX == 1
	Sfp = fopen("test_vmill.cpp","w");

	S_printLog("#include <Util/IncludeEverywhere.h>");
	S_printLog("#include <Util/Version.h>");
	S_printLog("#include <License/LicenseAPI.h>");
	S_printLog("#include <Exchange/ToolpathRecords.h>");
	S_printLog("#include <Exchange/VoluMillExchange.h>");
	S_printLog("#include <Geom/CurveAlg.h>");
	S_printLog("");

	S_printLog("namespace license");
	S_printLog("{");
	S_printLog("	int generateLicenseFromMasterLicense (LicenseKey* pKey);");
	S_printLog("}");
	S_printLog("");

	S_printLog("extern \"C\" void ncl_test_vmill(exchange::ToolpathRecords **pRecords)");
	S_printLog("{");
	S_printLog("	int iLoop=0;");
	S_printLog("	license::LicenseKey licenseKey;");
	S_printLog("	license::generateLicenseFromMasterLicense (&licenseKey);");
	S_printLog("	exchange::VoluMill2dInput input;");
	S_printLog("	util::Parameterized& params = input.m_parameters;");
	S_printLog("");

#endif
}

/*********************************************************************
**    I_FUNCTION     : S_makeline(curves,spt,ept,dtol)
**       Calls the VoluMill routine to create a line segment in the
**       pocketing geometry if the distance between the points are
**       larger than the tolerance.
**    PARAMETERS
**       INPUT  :
**          spt    - Starting point of line.
**          ept    - Ending point of line.
**          dtol   - Tolerance to determine if points are the same.
**          flag   - -1 = Standard pocket, 0+ = Waterline pocket.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS :
**          ept will be stored in spt on return.
**    WARNINGS     : none
*********************************************************************/
static int S_makeline(std::vector<geom::CurvePtr2d>* curves, UM_coord spt,
	UM_coord ept, UU_REAL dtol, int flag)
{
	char sbuf[80];
	if (!um_cceqcc_tol(spt,ept,dtol))
	{
		curves->push_back(geom::makeLine<2>
			(geom::Point2d(spt[0],spt[1]),geom::Point2d(ept[0],ept[1])));
#if DEBUGX == 1
		if (flag < 0)
			S_printMakeLine(spt,ept);
		else
			S_printMakeWaterLine(flag,spt,ept,Slevel);
#endif
		um_vctovc(ept,spt);
		return (UU_SUCCESS);
	}
#if DEBUGX == 1
	else
	{
		sprintf(sbuf,"POINT/%g,%g,%g",spt[0],spt[1],spt[2]);
		NclxDbgPstr(sbuf);
	}
#endif
	return (UU_FAILURE);
}

/*********************************************************************
**    I_FUNCTION     : S_closeLog
**       Closes the log file and writes the trailer.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_closeLog()
{
#if DEBUGX == 1
	S_printLog("	int jobId = exchange::submitVoluMill2dJob (input, licenseKey);");
	S_printLog("	for (;;)");
	S_printLog("	{");
	S_printLog("		util::msleep(100);");
	S_printLog("		if (exchange::getProgress(jobId) < 0) break;");
	S_printLog("	}");
	S_printLog("");
	S_printLog("*pRecords = new exchange::ToolpathRecords;");
	S_printLog("int numRecords = exchange::getToolpathRecords (jobId, *pRecords);");
	S_printLog("}");
	if (Sfp != 0) fclose(Sfp);
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_printLog(buf)
**       Writes a record to the log file.
**    PARAMETERS
**       INPUT  :
**          buf    - Character string to output to log file.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printLog(char *buf)
{
#if DEBUGX == 1
	if (Sfp != 0)
	{
		fputs(buf,Sfp);
		fputs("\n",Sfp);
	}
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_printSetKParam(cparm,kparm)
**       Writes an integer setParam record to the log file.
**    PARAMETERS
**       INPUT  :
**          cparm  - Parameter string to output to log file.
**          kparm  - Parameter value to output to log file.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printSetKParam(char *cparm, int kparm)
{
#if DEBUGX == 1
	char sbuf[200];
	sprintf(sbuf,"	params.setParam (util::%s,%d);",cparm,kparm);
	S_printLog(sbuf);
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_printSetGParam(cparm,gparm)
**       Writes a real setParam record to the log file.
**    PARAMETERS
**       INPUT  :
**          cparm  - Parameter string to output to log file.
**          gparm  - Parameter value to output to log file.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printSetGParam(char *cparm, UU_REAL gparm)
{
#if DEBUGX == 1
	char sbuf[200];
	sprintf(sbuf,"	params.setParam (util::%s,%f);",cparm,gparm);
	S_printLog(sbuf);
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_printSetCParam(cparm,cin)
**       Writes a string setParam record to the log file.
**    PARAMETERS
**       INPUT  :
**          cparm  - Parameter string to output to log file.
**          cin    - Parameter value to output to log file.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printSetCParam(char *cparm, char *cin)
{
#if DEBUGX == 1
	char sbuf[200];
	sprintf(sbuf,"	params.setParam (util::%s,\"%s\");",cparm,cin);
	S_printLog(sbuf);
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_printMakeLine(spt,ept)
**       Writes a MakeLine record to the log file.
**    PARAMETERS
**       INPUT  :
**          spt    - Starting point of line.
**          ept    - Ending point of line.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printMakeLine(UU_REAL *spt,UU_REAL *ept)
{
#if DEBUGX == 1
	char sbuf[200];
	sprintf(sbuf,"	curves.push_back(geom::makeLine<2>(geom::Point2d(%g,%g),geom::Point2d(%g,%g)));",spt[0],spt[1],ept[0],ept[1]);
	S_printLog(sbuf);
	sprintf(sbuf,"LINE/%g,%g,%g,%g",spt[0],spt[1],ept[0],ept[1]);
	NclxDbgPstr(sbuf);
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_printMakeWaterLine(i,spt,ept,zlev)
**       Writes a MakeLine record to the log file that was generated
**       by the Waterline routine.  It prints the outer boundary in
**       Blue and the islands in Green.  The lines will also be displayed
**       at the correct Z-level.
**    PARAMETERS
**       INPUT  :
**          i      - 0 = Outer boundary, + = Island.
**          spt    - Starting point of line.
**          ept    - Ending point of line.
**          zlev   - Zlevel of line.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printMakeWaterLine(int i, UU_REAL *spt,UU_REAL *ept,UU_REAL zlev)
{
#if DEBUGX == 1
	char sbuf[200];
	sprintf(sbuf,"	curves.push_back(geom::makeLine<2>(geom::Point2d(%g,%g),geom::Point2d(%g,%g)));",spt[0],spt[1],ept[0],ept[1]);
	S_printLog(sbuf);
/*
.....Output lines to debug file
*/
	if (i == 0)
	{
		NclxDbgPstr("");
		NclxDbgPstr("DRAFT/MODIFY,COLOR=GREEN");
	}
	else if (i == 1) NclxDbgPstr("DRAFT/MODIFY,COLOR=MAGNTA");
	sprintf(sbuf,"LINE/%g,%g,%g,%g,%g,%g",spt[0],spt[1],zlev,ept[0],ept[1],
		zlev);
	NclxDbgPstr(sbuf);
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_printMakeCircle(i,cpt,rad,zlev)
**       Writes a MakeCircle record to the log file.
**    PARAMETERS
**       INPUT  :
**          i      - 0 = Outer boundary, + = Island.
**          cpt    - Center point of circle.
**          rad    - Radius of circle.
**          zlev   - Zlevel of circle.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printMakeCircle(int i,UU_REAL *cpt,UU_REAL rad,UU_REAL zlev)
{
#if DEBUGX == 1
	char sbuf[200];
	sprintf(sbuf,"	curves.push_back(geom::makeCircle(geom::Point2d(%g,%g),%g));",
		cpt[0],cpt[1],rad);
	S_printLog(sbuf);
/*
.....Output lines to debug file
*/
	if (i == 0)
	{
		NclxDbgPstr("");
		NclxDbgPstr("DRAFT/MODIFY,COLOR=GREEN");
	}
	else if (i == 1) NclxDbgPstr("DRAFT/MODIFY,COLOR=MAGNTA");
	sprintf(sbuf,"CIRCLE/%g,%g,%g,%g",cpt[0],cpt[1],zlev,rad);
	NclxDbgPstr(sbuf);
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_printMakeCCWArc(cpt,spt,ept)
**       Writes a MakeCCWArc record to the log file.
**    PARAMETERS
**       INPUT  :
**          cpt    - Center point of circle.
**          spt    - Starting point of arc.
**          ept    - Ending point of arc.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printMakeCCWArc(UU_REAL *cpt,UU_REAL *spt,UU_REAL *ept)
{
#if DEBUGX == 1
	char sbuf[200];
	sprintf(sbuf,"	curves.push_back(geom::makeCCWArcCenter(geom::Point2d(%g,%g),geom::Point2d(%g,%g),geom::Point2d(%g,%g)));",
		cpt[0],cpt[1],spt[0],spt[1],ept[0],ept[1]);
	S_printLog(sbuf);
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_printMakeCWArc(cpt,spt,ept)
**       Writes a MakeCWArc record to the log file.
**    PARAMETERS
**       INPUT  :
**          cpt    - Center point of circle.
**          spt    - Starting point of arc.
**          ept    - Ending point of arc.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printMakeCWArc(UU_REAL *cpt,UU_REAL *spt,UU_REAL *ept)
{
#if DEBUGX == 1
	char sbuf[200];
	sprintf(sbuf,"	curves.push_back(geom::makeCWArcCenter(geom::Point2d(%g,%g),geom::Point2d(%g,%g),geom::Point2d(%g,%g)));",
		cpt[0],cpt[1],spt[0],spt[1],ept[0],ept[1]);
	S_printLog(sbuf);
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_get_error(verr)
**       Gets the NCL error number for the given VoluMill error.
**    PARAMETERS
**       INPUT  :
**          verr - VoluMill error value
**       OUTPUT :
**          none. 
**    RETURNS      : NCL error number
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UM_int2 S_get_error(int verr)
{
	switch(verr)
	{
	case ERROR_VOLUMILL_INTERNAL_ERROR:
		return (542);
	case ERROR_OPEN_POCKETS_NOT_ALLOWED:
		return (543);
	case ERROR_TOOL_DIAMETER_TOO_SMALL:
		return (544);
	case ERROR_NO_STOCK_DEFINED:
		return (545);
	case ERROR_NO_PART_DEFINED:
		return (546);
	case ERROR_PREDRILLED_HOLES_CORRUPTED:
		return (-547);
	case ERROR_PREDRILLED_HOLES_DIAMETER:
		return (-548);
	case ERROR_PREDRILLED_HOLES_NO_CLOSE_POINT:
		return (-549);
	case ERROR_PREDRILLED_HOLES_SIDE_CUT_HIT_BOUNDARY:
		return (-550);
	case ERROR_PREDRILLED_HOLES_HOLE_HIT_BOUNDARY:
		return (-550);
	case ERROR_PREDRILLED_HOLES_HOLE_NOT_DEEP:
		return (-551);
	case WARNING_WILL_LEAVE_SCALLOPS:
		return (-552);
	}
	return (0);
}

/*********************************************************************
**    I_FUNCTION     : S_printCurves()
**       Writes out the Curve push records to the log file.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printCurves()
{
#if DEBUGX == 1
	char sbuf[200];
	S_printLog("	");
	S_printLog("	for (int kk=0; kk < (int) curves.size(); ++kk)");
	S_printLog("	{");
	S_printLog("		input.m_boundaries.m_partBoundaries.push_back (curves[kk]);");
	S_printLog("		input.m_boundaries.m_partBoundaries.back()->setParam(util::PARAM_LOOP_ID, iLoop);");
	S_printLog("		input.m_boundaries.m_partBoundaries.back()->deleteParam(util::PARAM_Z_MAX);");
	S_printLog("	}");
	S_printLog("	++iLoop;");
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_printHole(drillHole)
**       Writes a Predrilled hole record to the log file.
**    PARAMETERS
**       INPUT  :
**          drillHole - Predrilled hole structure.
**       OUTPUT :
**          none. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_printHole(exchange::VoluMillDrillHole *drillHole)
{
#if DEBUGX == 1
	char sbuf[200];
	sprintf(sbuf,"drillHole.m_center[0] = %g",drillHole->m_center[0]);
	S_printLog(sbuf);
	sprintf(sbuf,"drillHole.m_center[1] = %g",drillHole->m_center[1]);
	S_printLog(sbuf);
	sprintf(sbuf,"drillHole.m_usableDepth = %g",drillHole->m_usableDepth);
	S_printLog(sbuf);
	sprintf(sbuf,"drillHole.m_through = %d",drillHole->m_through);
	S_printLog(sbuf);
	S_printLog("input.m_preDrilledHoles.push_back(drillHole)");
#endif
}

#endif

