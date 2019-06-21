/*********************************************************************
**  NAME:  SwUnibase.h
**
**       SolidWorks generated file.
**
** CONTAINS:
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwUnibase.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:09
*********************************************************************/
// SwUnibase.h : Declaration of the CSwUnibase

#pragma once

#include "SwUnibase_i.h"

#include "Swresource.h"       // main symbols
#include <comsvcs.h>
#include <map>
#include "nclx.h"
#include "nclxunib.h"

class CPMPage;
class CSwDocument;
typedef std::map<IUnknown*, CSwDocument*> TMapIUnknownToDocument;

class CBitmapHandler;

#define ID_SLDWORKS_EVENTS 0
#define MAIN_CMD_GROUP_ID 0 
#define MAIN_ITEM_ID1 0
#define MAIN_ITEM_ID2 1
#define FLYOUT_GROUP_ID 99


// CSwUnibase




class ATL_NO_VTABLE CSwUnibase :
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CSwUnibase, &CLSID_SwUnibase>,
	public IDispatchImpl<ISwUnibase, &IID_ISwUnibase, &LIBID_SwUnibaseLib, 1, 0>,
	public ISwAddin,
	public IDispEventImpl<ID_SLDWORKS_EVENTS, CSwUnibase, &__uuidof(DSldWorksEvents), &LIBID_SldWorks, ID_SLDWORKS_TLB_MAJOR, ID_SLDWORKS_TLB_MINOR> 
	{
	typedef IDispEventImpl<ID_SLDWORKS_EVENTS, CSwUnibase, &__uuidof(DSldWorksEvents), &LIBID_SldWorks, ID_SLDWORKS_TLB_MAJOR, ID_SLDWORKS_TLB_MINOR> CSldWorksEvents;

private:
	CComPtr<ISldWorks> iSwApp;
	CComPtr<ICommandManager> iCmdMgr;
	CComObject<CBitmapHandler> *iBmp;
	long addinID;
	long toolbarID;
	long m_swMajNum;
	long m_swMinNum;
/*
.....NCCS
*/
	DWORD m_dwSldWorksEvents;
	long m_swCookie;
	long m_lToolbarID;
	CComObject<CPMPage> *m_pPMPage;
	CComPtr<ISldWorks> m_iSldWorks;
	TMapIUnknownToDocument m_swDocMap;
	NCLX_sw_options m_swUniOptions;

//This mapping will contain references to all open Documents, and ensure 
//that we do not attempt to attach event handlers to an already opened doc. 
	TMapIUnknownToDocument openDocs;

public:
	CSwUnibase() : m_swCookie(0)
	{
/*
.....NCCS
*/
		m_lToolbarID = 0;
		m_pPMPage = NULL;
		m_dwSldWorksEvents = 0;
		m_swUniOptions.unibase[0] = '\0';
		m_swUniOptions.label_opts = 0;
		m_swUniOptions.import_curves = 0;
		m_swUniOptions.shade_surfs = 1;
		m_swUniOptions.sflab_type = 0;
		strcpy(m_swUniOptions.sflab,"SF");
		m_swUniOptions.cvlab_type = 0;
		strcpy(m_swUniOptions.cvlab,"CV");
		m_swUniOptions.ptlab_type = 0;
		strcpy(m_swUniOptions.ptlab,"PT");
		m_swUniOptions.lnlab_type = 0;
		strcpy(m_swUniOptions.lnlab,"LN");
		m_swUniOptions.cilab_type = 0;
		strcpy(m_swUniOptions.cilab,"CI");
		m_swUniOptions.kvlab_type = 0;
		strcpy(m_swUniOptions.kvlab,"KV");
		m_swUniOptions.unimatch[0] = '\0';
		m_swUniOptions.exact_match = 4;
		m_swUniOptions.toler = .001;
		m_swUniOptions.cvopt    = 0;
		m_swUniOptions.layer[0] = 1;
		m_swUniOptions.layer[1] = 100;
		m_swUniOptions.layer[2] = 200;
		m_swUniOptions.layer[3] = 300;
		m_swUniOptions.layer[4] = 400;
		m_swUniOptions.layer[5] = 500;
		m_swUniOptions.color[0] = NCLX_GREEN;
		m_swUniOptions.color[1] = NCLX_YELLOW;
		m_swUniOptions.color[2] = NCLX_PINK;
		m_swUniOptions.color[3] = NCLX_ORANGE;
		m_swUniOptions.color[4] = NCLX_MAGENTA;
		m_swUniOptions.color[5] = NCLX_RED;
	}

	DECLARE_PROTECT_FINAL_CONSTRUCT()

	HRESULT FinalConstruct()
	{
		return S_OK;
	}

	void FinalRelease()
	{
	}

	DECLARE_REGISTRY_RESOURCEID(IDR_SwUnibase)

	DECLARE_NOT_AGGREGATABLE(CSwUnibase)

	BEGIN_COM_MAP(CSwUnibase)
		COM_INTERFACE_ENTRY(ISwUnibase)
		COM_INTERFACE_ENTRY(IDispatch)
		COM_INTERFACE_ENTRY(ISwAddin)
	END_COM_MAP()

	// ISwUnibase
public:

	// ISwAddin Methods
public:
	CComPtr<ISldWorks> GetSldWorksPtr() { return iSwApp != NULL ? iSwApp : NULL; }
		//These methods will connect and disconnect this addin to the SolidWorks events
	VARIANT_BOOL AttachEventHandlers();
	VARIANT_BOOL DetachEventHandlers();
	//These methods will connect and disconnect this addin to the SolidWorks Model events
	VARIANT_BOOL AttachModelEventHandler(CComPtr<IModelDoc2> iModelDoc2);	
	VARIANT_BOOL DetachModelEventHandler(IUnknown *iUnk);
	HRESULT AttachEventsToAllDocuments();
TMapIUnknownToDocument OpenDocumentsTable() { return openDocs; }
	
	int GetSldWorksTlbMajor() {return (m_swMajNum >= ID_SLDWORKS_TLB_MAJOR ) ? m_swMajNum : 0;}
	int GetSldWorksTlbMinor() {return m_swMinNum;}

	void AddCommandManager();
	void AddUserInterface();
	void AddMenus();
	void RemoveCommandManager();
	bool CompareIDs(long * storedIDs, long storedSize, long * addinIDs, long addinSize);
	BSTR GetCurrentFile();

		//Event Handlers
	//These are the methods that are called when certain SolidWorks events are fired
	STDMETHOD(OnDocChange)(void);
	STDMETHOD(OnModelDocChange)(void);
	STDMETHOD(OnDocLoad)(BSTR docTitle, BSTR docPath);
	STDMETHOD(OnFileNew)(LPDISPATCH newDoc, long docType, BSTR templateName);
	STDMETHOD(OnFileOpenPostNotify)(BSTR fileName);

	
	BEGIN_SINK_MAP(CSwUnibase)
		SINK_ENTRY_EX(ID_SLDWORKS_EVENTS, __uuidof(DSldWorksEvents), swAppActiveDocChangeNotify, OnDocChange)
		SINK_ENTRY_EX(ID_SLDWORKS_EVENTS, __uuidof(DSldWorksEvents), swAppDocumentLoadNotify, OnDocLoad)
		SINK_ENTRY_EX(ID_SLDWORKS_EVENTS, __uuidof(DSldWorksEvents), swAppFileNewNotify2, OnFileNew)
		SINK_ENTRY_EX(ID_SLDWORKS_EVENTS, __uuidof(DSldWorksEvents), swAppActiveModelDocChangeNotify, OnModelDocChange)
		SINK_ENTRY_EX(ID_SLDWORKS_EVENTS, __uuidof(DSldWorksEvents), swAppFileOpenPostNotify, OnFileOpenPostNotify)
	END_SINK_MAP( )
	
	// ISwAddin Methods
	//These are the methods inherited from the ISwAddin interface that 
	//need to be implemented in order to connect the addin to SolidWorks
public:
	STDMETHOD(ConnectToSW)(LPDISPATCH ThisSW, long Cookie, VARIANT_BOOL * IsConnected);
	STDMETHOD(DisconnectFromSW)(VARIANT_BOOL * IsDisconnected);


	STDMETHOD(ToolbarCallback0)(void);
	STDMETHOD(ToolbarEnable0)(long* status);
	STDMETHOD(FlyoutCallback)(void);
	STDMETHOD(FlyoutCallback0)(void);
	STDMETHOD(FlyoutCallback1)(void);
	STDMETHOD(FlyoutEnable0)(long* status);
	STDMETHOD(FlyoutEnableCallback0)(long* status);
};

OBJECT_ENTRY_AUTO(__uuidof(SwUnibase), CSwUnibase)
