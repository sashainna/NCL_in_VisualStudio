/*********************************************************************
**  NAME:  SwDocument.h
**
**       SolidWorks generated file.
**
** CONTAINS:
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwDocument.h , 24.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/13/14 , 14:43:54
*********************************************************************/
// SwDocument.h : Declaration of the CSwDocument

#pragma once

#include "SwUnibase.h"

#include "Swresource.h"       // main symbols
//Defines
#define ID_PART_EVENTS 1
#define ID_ASSEMBLY_EVENTS 2
#define ID_DRAWING_EVENTS 3
class CSwUnibase;

#include <map>
class CDocView;
typedef std::map<IUnknown*, CDocView*> TMapIUnknownToModelView;

// CSwDocument

class ATL_NO_VTABLE CSwDocument :
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CSwDocument, &CLSID_SwDocument>,
	public IDispatchImpl<ISwDocument, &IID_ISwDocument, &LIBID_SwUnibaseLib, /*wMajor =*/ 1, /*wMinor =*/ 0>,
	public IDispEventImpl<ID_PART_EVENTS, CSwDocument, &__uuidof(DPartDocEvents), &LIBID_SldWorks, ID_SLDWORKS_TLB_MAJOR, ID_SLDWORKS_TLB_MINOR>,	
	public IDispEventImpl<ID_ASSEMBLY_EVENTS, CSwDocument, &__uuidof(DAssemblyDocEvents), &LIBID_SldWorks, ID_SLDWORKS_TLB_MAJOR, ID_SLDWORKS_TLB_MINOR>,	
	public IDispEventImpl<ID_DRAWING_EVENTS, CSwDocument, &__uuidof(DDrawingDocEvents), &LIBID_SldWorks, ID_SLDWORKS_TLB_MAJOR, ID_SLDWORKS_TLB_MINOR>

{

	typedef IDispEventImpl<ID_PART_EVENTS, CSwDocument, &__uuidof(DPartDocEvents), &LIBID_SldWorks, ID_SLDWORKS_TLB_MAJOR, ID_SLDWORKS_TLB_MINOR> CPartEvents;
	typedef IDispEventImpl<ID_ASSEMBLY_EVENTS, CSwDocument, &__uuidof(DAssemblyDocEvents), &LIBID_SldWorks, ID_SLDWORKS_TLB_MAJOR, ID_SLDWORKS_TLB_MINOR> CAssemblyEvents;
	typedef IDispEventImpl<ID_DRAWING_EVENTS, CSwDocument, &__uuidof(DDrawingDocEvents), &LIBID_SldWorks, ID_SLDWORKS_TLB_MAJOR, ID_SLDWORKS_TLB_MINOR> CDrawingEvents;

private:
	long type;
	CComPtr<IModelDoc2> iDocument;
	CComPtr<ISldWorks> iSwApp;
	CSwUnibase* userAddin;

	TMapIUnknownToModelView openModelViews;
public:
	CSwDocument();

	~CSwDocument();

	DECLARE_PROTECT_FINAL_CONSTRUCT()

	HRESULT FinalConstruct()
	{
		return S_OK;
	}

	void FinalRelease()
	{
	}

DECLARE_REGISTRY_RESOURCEID(IDR_SWDOCUMENT)

DECLARE_NOT_AGGREGATABLE(CSwDocument)

BEGIN_COM_MAP(CSwDocument)
	COM_INTERFACE_ENTRY(ISwDocument)
	COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()




// ISwDocument
public:

	void Init(CSwUnibase* addinPtr, IModelDoc2* modDoc);
	long GetType();

	VARIANT_BOOL AttachEventHandlers(void);
	VARIANT_BOOL AttachModelViewEventHandlers(void);
	VARIANT_BOOL DetachEventHandlers(void);
	VARIANT_BOOL DetachModelViewEventHandler(IUnknown *IUnk);
	HRESULT ComponentStateChange(LPDISPATCH componentModel, long newCompState);

	//Event Handlers
	STDMETHOD(OnPartDestroy)(void);
	STDMETHOD(OnPartNewSelection)(void);

	STDMETHOD(OnAssemblyDestroy)(void);
	STDMETHOD(OnAssemblyNewSelection)(void);
        STDMETHOD(OnAssemblyComponentStateChangeNotify2)(LPDISPATCH componentModel, BSTR compName, long oldCompState, long newCompState);
    STDMETHOD(OnAssemblyComponentStateChangeNotify)(LPDISPATCH componentModel, long oldCompState, long newCompState);
	STDMETHOD(OnAssemblyComponentVisualPropertiesChangeNotify)(LPDISPATCH swObject);
	STDMETHOD(OnAssemblyComponentDisplayStateChangeNotify)(LPDISPATCH swObject);
	STDMETHOD(OnDrawingDestroy)(void);
	STDMETHOD(OnDrawingNewSelection)(void);

		//Event Sinks
	//The SINK_MAP connects the specified Model event to a specific event handler
	BEGIN_SINK_MAP(CSwDocument)
		SINK_ENTRY_EX(ID_PART_EVENTS, __uuidof(DPartDocEvents), swPartDestroyNotify, OnPartDestroy)
		SINK_ENTRY_EX(ID_PART_EVENTS, __uuidof(DPartDocEvents), swPartNewSelectionNotify, OnPartNewSelection)
		SINK_ENTRY_EX(ID_ASSEMBLY_EVENTS, __uuidof(DAssemblyDocEvents), swAssemblyDestroyNotify, OnAssemblyDestroy)
		SINK_ENTRY_EX(ID_ASSEMBLY_EVENTS, __uuidof(DAssemblyDocEvents), swAssemblyNewSelectionNotify, OnAssemblyNewSelection)
		SINK_ENTRY_EX(ID_ASSEMBLY_EVENTS, __uuidof(DAssemblyDocEvents), swAssemblyComponentStateChangeNotify2, OnAssemblyComponentStateChangeNotify2)
		SINK_ENTRY_EX(ID_ASSEMBLY_EVENTS, __uuidof(DAssemblyDocEvents), swAssemblyComponentStateChangeNotify, OnAssemblyComponentStateChangeNotify)
		SINK_ENTRY_EX(ID_ASSEMBLY_EVENTS, __uuidof(DAssemblyDocEvents), swAssemblyComponentVisualPropertiesChangeNotify , OnAssemblyComponentVisualPropertiesChangeNotify)
		SINK_ENTRY_EX(ID_ASSEMBLY_EVENTS, __uuidof(DAssemblyDocEvents), swAssemblyComponentDisplayStateChangeNotify  , OnAssemblyComponentDisplayStateChangeNotify)
		SINK_ENTRY_EX(ID_DRAWING_EVENTS, __uuidof(DDrawingDocEvents), swDrawingDestroyNotify, OnDrawingDestroy)
		SINK_ENTRY_EX(ID_DRAWING_EVENTS, __uuidof(DDrawingDocEvents), swDrawingNewSelectionNotify, OnDrawingNewSelection)
	END_SINK_MAP( )
};

OBJECT_ENTRY_AUTO(__uuidof(SwDocument), CSwDocument)
