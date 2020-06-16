/*********************************************************************
**  NAME:  SwDocument.cpp
**
**       SolidWorks generated file.
**
** CONTAINS:
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwDocument.cpp , 24.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/13/14 , 16:41:33
*********************************************************************/
// SwDocument.cpp : Implementation of CSwDocument
// SAN - 02282012 - Removed redundant IF statements in AttachModelViewEventHandler

#include "assert.h"
#include "SwStdAfx.h"
#include "SwDocument.h"
#include "SwUnibase.h"
#include "SwDocView.h"

// CSwDocument

CSwDocument::CSwDocument()
{
	return;
}

CSwDocument::~CSwDocument()
{
	assert(openModelViews.size() == 0);

	return;
}

void CSwDocument::Init(CSwUnibase* addinPtr, IModelDoc2* modDoc)
{
	userAddin = addinPtr;
	iSwApp = userAddin->GetSldWorksPtr();
	iDocument = modDoc;
	iDocument->GetType(&type);
}

//Returns the document type
long CSwDocument::GetType()
{
	return type;
}

//Listen for the events fired by this document
VARIANT_BOOL CSwDocument::AttachEventHandlers()
{
	VARIANT_BOOL attached = VARIANT_TRUE;

	//Connect to Document Events
	HRESULT success = E_FAIL;
	switch(type)
	{
		case swDocPART:
			{
				//Listen for PartDoc events
				CPartEvents::m_wMajorVerNum = userAddin->GetSldWorksTlbMajor();
				CPartEvents::_tih.m_wMajor = CPartEvents::m_wMajorVerNum;

				success = CPartEvents::DispEventAdvise(iDocument, &__uuidof(DPartDocEvents));
				break;
			}
		case swDocASSEMBLY:
			{
				//Listen for AssemblyDoc events
				CAssemblyEvents::m_wMajorVerNum = userAddin->GetSldWorksTlbMajor();
				CAssemblyEvents::_tih.m_wMajor = CAssemblyEvents::m_wMajorVerNum;

				success = CAssemblyEvents::DispEventAdvise(iDocument, &__uuidof(DAssemblyDocEvents));
				break;
			}
		case swDocDRAWING:
			{
				//Listen for DrawingDoc events
				CDrawingEvents::m_wMajorVerNum = userAddin->GetSldWorksTlbMajor();
				CDrawingEvents::_tih.m_wMajor = CDrawingEvents::m_wMajorVerNum;

				success = CDrawingEvents::DispEventAdvise(iDocument, &__uuidof(DDrawingDocEvents));
				break;
			}
		default:
			return VARIANT_FALSE;
	}
	if (success != S_OK)
		return VARIANT_FALSE;


	attached = this->AttachModelViewEventHandlers();
	return attached;
}
VARIANT_BOOL CSwDocument::AttachModelViewEventHandlers()
{
	VARIANT_BOOL attached = VARIANT_TRUE;
	VARIANT_BOOL         bIsVisible = VARIANT_FALSE;
	CComPtr<IModelDoc2>  swActiveDocument;

	iDocument->get_Visible(&bIsVisible);
	
	iSwApp->get_IActiveDoc2(&swActiveDocument);

	if (swActiveDocument == NULL)
	{
		return VARIANT_FALSE;
	}

	CComQIPtr<IUnknown, &IID_IUnknown>  pUnk1(swActiveDocument);
	CComQIPtr<IUnknown, &IID_IUnknown>  pUnk2(iDocument);

	// Connect event handlers to all previously open model views on this document
	TMapIUnknownToModelView::iterator iter;

	CComPtr<IModelView> iModelView;
	iDocument->IGetFirstModelView(&iModelView);
	while (iModelView != NULL)
		{
			iter = openModelViews.find(iModelView);
			if (iter == openModelViews.end())
			{
				//Create the ModelView handler
				CComObject<CDocView> *pMView;
				CComObject<CDocView>::CreateInstance( &pMView);
				pMView->Init(userAddin, iModelView, this);

				//Attach its event handlers
				attached = pMView->AttachEventHandlers();
				if(!attached)
					return VARIANT_FALSE;

					//Add it to the list of open ModelViews
				openModelViews.insert(openModelViews.end(), TMapIUnknownToModelView::value_type(iModelView, pMView));
				pMView->AddRef();
			}
			CComPtr<IModelView> iNextModelView;
			iModelView->IGetNext(&iNextModelView);
			iModelView = iNextModelView;
		}
	
	return attached;
}
//Stop listening for the events fired by this document
VARIANT_BOOL CSwDocument::DetachEventHandlers()
{
	//Disconnect from the Document Events
	HRESULT success = E_FAIL;
	switch(type)
	{
		case swDocPART:
			{
				//Stop listening for PartDoc events
				success = CPartEvents::DispEventUnadvise(iDocument, &__uuidof(DPartDocEvents));

				CPartEvents::_tih.m_plibid = &GUID_NULL;

				break;
			}
		case swDocASSEMBLY:
			{
				//Stop listening for AssemblyDoc events
				success = CAssemblyEvents::DispEventUnadvise(iDocument, &__uuidof(DAssemblyDocEvents));

				CAssemblyEvents::_tih.m_plibid = &GUID_NULL;

				break;
			}
		case swDocDRAWING:
			{
				//Stop listening for DrawingDoc events
				success = CDrawingEvents::DispEventUnadvise(iDocument, &__uuidof(DDrawingDocEvents));

				CDrawingEvents::_tih.m_plibid = &GUID_NULL;

				break;
			}
		default:
			return VARIANT_FALSE;
	}

	//Disconnect all model view event handlers
	TMapIUnknownToModelView::iterator iter;

	for (iter = openModelViews.begin(); iter != openModelViews.end(); /*iter++*/)
	{
		TMapIUnknownToModelView::value_type obj = *iter;
		CComObject<CDocView> *pMView = (CComObject<CDocView>*)obj.second;
		iter++;
		pMView->DetachEventHandlers();
	}

	userAddin->DetachModelEventHandler(iDocument);
	return VARIANT_TRUE;
}

//Stop listening for the events fired by the specified ModelView event handler
VARIANT_BOOL CSwDocument::DetachModelViewEventHandler(IUnknown *iUnk)
{
	TMapIUnknownToModelView::iterator iter;

	iter = openModelViews.find(iUnk);
	if (iter != openModelViews.end())
	{
		TMapIUnknownToModelView::value_type obj = *iter;
		obj.first->Release();
		CComObject<CDocView> *pMView = (CComObject<CDocView>*)obj.second;
		pMView->Release();
		openModelViews.erase(iter);
	}
	return VARIANT_TRUE;
}

//Event Handlers
STDMETHODIMP CSwDocument::OnPartDestroy(void)
{
	// TODO: Add your implementation code here
	this->DetachEventHandlers();
	return S_OK;
}

STDMETHODIMP CSwDocument::OnPartNewSelection(void)
{
	// TODO: Add your implementation code here
	ATLTRACE("\tCSwDocument::PartNewSelectionNotify called\n");
	return S_OK;
}

STDMETHODIMP CSwDocument::OnAssemblyDestroy(void)
{
	// TODO: Add your implementation code here
	this->DetachEventHandlers();
	return S_OK;
}

STDMETHODIMP CSwDocument::OnAssemblyNewSelection(void)
{
	// TODO: Add your implementation code here
	ATLTRACE("\tCSwDocument::AssemblyNewSelectionNotify called\n");
	return S_OK;
}

//attach to a component if it becomes resolved
HRESULT CSwDocument::ComponentStateChange(LPDISPATCH componentModel, long newCompState)
{
	if(componentModel == NULL)
		return S_OK;

	CComPtr<IModelDoc2> modDoc;
	
	componentModel->QueryInterface(__uuidof(IModelDoc2), reinterpret_cast<void**>(&modDoc));

    swComponentSuppressionState_e newState =  (swComponentSuppressionState_e)newCompState;

	switch(newState)
	{
		case swComponentSuppressionState_e::swComponentFullyResolved:
			{
				VARIANT_BOOL attached = VARIANT_TRUE;
				USES_CONVERSION;

				if(modDoc != NULL)
				{
					TMapIUnknownToDocument::iterator iter;
					TMapIUnknownToDocument openDocs = userAddin->OpenDocumentsTable();
					iter = openDocs.find(modDoc);
					if (iter == openDocs.end())
					{
						attached = userAddin->AttachModelEventHandler(modDoc);
					}
				}
				modDoc.Release();
				modDoc = NULL;
			}
		break;

		case swComponentSuppressionState_e::swComponentResolved:
			{
				VARIANT_BOOL attached = VARIANT_TRUE;
				USES_CONVERSION;

				if(modDoc != NULL)
				{
					TMapIUnknownToDocument::iterator iter;
					TMapIUnknownToDocument openDocs = userAddin->OpenDocumentsTable();
					iter = openDocs.find(modDoc);
					if (iter == openDocs.end())
					{
						attached = userAddin->AttachModelEventHandler(modDoc);
					}
				}
				modDoc.Release();
				modDoc = NULL;
			}
			break;

		default:
			break;
	}
	return S_OK;
}


STDMETHODIMP CSwDocument::OnAssemblyComponentStateChangeNotify2(LPDISPATCH componentModel, BSTR compName, long oldCompState, long newCompState)
{
    return ComponentStateChange(componentModel, newCompState);
}

STDMETHODIMP CSwDocument::OnAssemblyComponentStateChangeNotify(LPDISPATCH componentModel, long oldCompState, long newCompState)
{
    return ComponentStateChange(componentModel, newCompState);
}

STDMETHODIMP CSwDocument::OnAssemblyComponentVisualPropertiesChangeNotify(LPDISPATCH swObject)
{
	if(swObject == NULL)
		return S_OK;

	CComPtr<IModelDoc2> modDoc;
	CComPtr<IComponent2> component;
	
	swObject->QueryInterface(__uuidof(IComponent2), reinterpret_cast<void**>(&component));

	component->IGetModelDoc(&modDoc);
	swComponentSuppressionState_e newState =  swComponentSuppressionState_e::swComponentFullyResolved;

    return ComponentStateChange(modDoc, newState);
}

STDMETHODIMP CSwDocument::OnAssemblyComponentDisplayStateChangeNotify(LPDISPATCH swObject)
{
	if(swObject == NULL)
		return S_OK;

	CComPtr<IModelDoc2> modDoc;
	CComPtr<IComponent2> component;
	
	swObject->QueryInterface(__uuidof(IComponent2), reinterpret_cast<void**>(&component));

	component->IGetModelDoc(&modDoc);
	swComponentSuppressionState_e newState =  swComponentSuppressionState_e::swComponentFullyResolved;

    return ComponentStateChange(modDoc, newState);
}


STDMETHODIMP CSwDocument::OnDrawingDestroy(void)
{
	// TODO: Add your implementation code here
	this->DetachEventHandlers();
	return S_OK;
}

STDMETHODIMP CSwDocument::OnDrawingNewSelection(void)
{
	// TODO: Add your implementation code here
	ATLTRACE("\tCSwDocument::DrawingNewSelectionNotify called\n");
	return S_OK;
}
