/********************************************************************* 
**  NAME:  wsntdroptarget.cpp
**
**			implementation of CNCLDropTarget class functions
**	CONTAINS: CNCLDropTarget  class functions
**			all functions declared in wsntdroptarget.h
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**     wsntdroptarget.cpp , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**     04/29/15 , 15:12:22
*********************************************************************/
#include "wsntstdafx.h"
#include "wsntdroptarget.h"

/***********************************************************************
**
**   FUNCTION: CNCLDropTarget()
**
**              Constructor of class CNCLDropTarget
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDropTarget::CNCLDropTarget()
	:	m_DropTargetWnd(NULL),
		m_dwRefCount(1),
		m_RegisterType(0),
		m_KeyState(0L),
		m_Data(NULL)
{
	OleInitialize(NULL);
	CoLockObjectExternal(this, TRUE, 0);
}

CNCLDropTarget::~CNCLDropTarget()
{
	CoLockObjectExternal(this, FALSE, 0);
	OleUninitialize();
}

/***********************************************************************
**
**   FUNCTION: QueryInterface(REFIID iid, void **ppvObject)
**
**             retreave the pointer to the support interface
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
HRESULT CNCLDropTarget::QueryInterface(REFIID iid, void **ppvObject)
{
	if(ppvObject == NULL)
		return E_FAIL;
	
    if (iid == IID_IUnknown)
    {
		AddRef();
		(*ppvObject) = this;
		return S_OK;
    }

	if (iid == IID_IDropTarget)
	{
		AddRef();
		(*ppvObject) = this;
		return S_OK;
	}
	return E_FAIL;
}

ULONG CNCLDropTarget::AddRef(void)
{
	m_dwRefCount++;

	return m_dwRefCount;
}

ULONG CNCLDropTarget::Release(void)
{
	m_dwRefCount--;

	return m_dwRefCount;
}

/***********************************************************************
**
**   FUNCTION: Register()
**
**        Register as a target
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
BOOL CNCLDropTarget::Register(CWnd* pWnd, UINT pDataType)
{
	if(NULL == pWnd)
		return E_FAIL;
	
	if(0L == pDataType)
		return E_FAIL;

	m_DropTargetWnd = pWnd;
	m_RegisterType = pDataType;

	DWORD hRes = ::RegisterDragDrop(m_DropTargetWnd->m_hWnd, this);
	if(SUCCEEDED(hRes))
		return TRUE;

	return FALSE;
}

/***********************************************************************
**
**   FUNCTION: Revoke()
**
**        Unregister as a target
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
void CNCLDropTarget::Revoke()
{
	if(m_DropTargetWnd==NULL)
		return;

	RevokeDragDrop(m_DropTargetWnd->m_hWnd);
}
 
/***********************************************************************
**
**   FUNCTION: DragEnter()
**
**        Called if we have a drop text enter here.
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
HRESULT	CNCLDropTarget::DragEnter(struct IDataObject *pDataObject, unsigned long grfKeyState, struct _POINTL pMouse, unsigned long * pDropEffect)
{
	if(pDataObject == NULL)
		return E_FAIL;	//	must have data

	//	keep point
	m_DropPoint.x = pMouse.x;
	m_DropPoint.y = pMouse.y;

	//	keep key
	m_KeyState = grfKeyState;

	//	call top
	*pDropEffect = GotEnter();

	return S_OK;
}

/***********************************************************************
**
**   FUNCTION: DragOver()
**
**        Called if we have a drop text drag over.
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
HRESULT	CNCLDropTarget::DragOver(unsigned long grfKeyState, struct _POINTL pMouse, unsigned long *pEffect)
{
	m_DropPoint.x = pMouse.x;
	m_DropPoint.y = pMouse.y;

	m_KeyState = grfKeyState;

	*pEffect = GotDrag();
	return S_OK;
}

/***********************************************************************
**
**   FUNCTION: DragOver()
**
**        Called if we have a drop text leave here.
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
HRESULT	CNCLDropTarget::DragLeave(void)
{
	GotLeave();

	return S_OK;
}
/***********************************************************************
**
**   FUNCTION: Drop()
**
**        Released stuff here
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
HRESULT	CNCLDropTarget::Drop(struct IDataObject *pDataObject, unsigned long grfKeyState, struct _POINTL pMouse, unsigned long *pdwEffect)
{
	if(NULL == pDataObject)
		return E_FAIL;

	*pdwEffect = DROPEFFECT_COPY;
	
	FORMATETC iFormat;
	ZeroMemory(&iFormat, sizeof(FORMATETC));

	STGMEDIUM iMedium;
	ZeroMemory(&iMedium, sizeof(STGMEDIUM));

	iFormat.cfFormat = m_RegisterType;
	iFormat.dwAspect = DVASPECT_CONTENT;
	iFormat.lindex = -1;
	iFormat.tymed = TYMED_HGLOBAL;

	HRESULT hRes = pDataObject->GetData(&iFormat, &iMedium);
	if(FAILED(hRes))
	{
		TRACE("Bad data format");
		return hRes;
	}

/*
.....we have the data, get it
*/
	BYTE *iMem = (BYTE *)::GlobalLock(iMedium.hGlobal);
	DWORD iLen = ::GlobalSize(iMedium.hGlobal);

	m_Data = iMem;
	
	m_DropPoint.x = pMouse.x;
	m_DropPoint.y = pMouse.y;

	m_KeyState = grfKeyState;

//	GotDrop();

	::GlobalUnlock(iMedium.hGlobal);

	if(iMedium.pUnkForRelease != NULL)
		iMedium.pUnkForRelease->Release();

		GotDrop();

	return S_OK;
}

/***********************************************************************
**
**   FUNCTION: GotDrop()
**
**        Called if we have a drop text drop here.
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
void CNCLDropTarget::GotDrop()
{
}

/***********************************************************************
**
**   FUNCTION: GotDrag()
**
**      Dragging in the control. Check the runtime class to see if it
**		is one of the class we want to drag
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
DWORD CNCLDropTarget::GotDrag(void)
{
	return DROPEFFECT_LINK;
}

/***********************************************************************
**
**   FUNCTION: GotLeave()
**
**        Called if we have a drop text leave here.
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
void CNCLDropTarget::GotLeave(void)
{
}

/***********************************************************************
**
**   FUNCTION: GotEnter()
**
**        Called if we have a drop text enter here.
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
DWORD CNCLDropTarget::GotEnter(void)
{
	return DROPEFFECT_LINK;
}

