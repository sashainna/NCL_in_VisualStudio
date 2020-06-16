/*********************************************************************
**  NAME:  SwDocView.cpp
**
**       SolidWorks generated file.
**
** CONTAINS:
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       SwDocView.cpp , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       08/13/14 , 14:41:43
*********************************************************************/
// DocView.cpp : Implementation of CDocView

#include "SwStdAfx.h"
#include "SwDocView.h"
#include "SwDocument.h"

// CDocView
void CDocView::Init(CSwUnibase *theApp, IModelView *iModelView, CSwDocument *iParent)
{
	userAddin = theApp;
	iModelView->AddRef();
	modelView = iModelView;
	parentDoc = iParent;
}

VARIANT_BOOL CDocView::AttachEventHandlers()
{
	// Connect to the ModelView event sink
	this->m_libid = LIBID_SldWorks;
	this->m_wMajorVerNum = userAddin->GetSldWorksTlbMajor();
	this->m_wMinorVerNum = 0;

	CModelViewEvents::_tih.m_wMajor = this->m_wMajorVerNum;

	HRESULT success = this->DispEventAdvise(modelView, &__uuidof(DModelViewEvents));
	if (success == S_OK)
		return VARIANT_TRUE;
	return VARIANT_FALSE;
}

VARIANT_BOOL CDocView::DetachEventHandlers()
{
	VARIANT_BOOL detached = VARIANT_TRUE;

	// Disconnect from the ModelView event sink
	HRESULT success = DispEventUnadvise(modelView);

	CModelViewEvents::_tih.m_plibid = &GUID_NULL;

	if (success != S_OK)
		return VARIANT_FALSE;

	CComObject<CSwDocument> *pDoc;
	pDoc = (CComObject<CSwDocument>*)parentDoc;
	detached = pDoc->DetachModelViewEventHandler(modelView);
	return detached;
}

//Event Handlers
//Called when the ModelView is destroyed
STDMETHODIMP CDocView::OnDestroy(long destroyType)
{
	// TODO: Add your implementation code here
	this->DetachEventHandlers();
	return S_OK;
}

//Called when the ModelView is repainted
STDMETHODIMP CDocView::OnRepaint(long paintType)
{
	// TODO: Add your implementation code here
	ATLTRACE("\tCPartHandler::OnRepaint called\n");
	return S_OK;
}
