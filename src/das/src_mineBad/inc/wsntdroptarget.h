/********************************************************************* 
**  NAME:  wsntdroptarget.h
**
**  Description - Functions and struct declarations for
**              CNCLDropTarget class (Class that inherits from IDropTarget)
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**     wsntdroptarget.h , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**     04/29/15 , 15:07:17
*********************************************************************/
#ifndef WSNTDROPTARGET_H
#define WSNTDROPTARGET_H

interface CNCLDropTarget : public IDropTarget
{
public:
	CNCLDropTarget();
	~CNCLDropTarget();

	HRESULT	STDMETHODCALLTYPE	QueryInterface(REFIID iid, void ** ppvObject); 
	ULONG	STDMETHODCALLTYPE	AddRef(void); 
	ULONG	STDMETHODCALLTYPE	Release(void); 

	HRESULT	STDMETHODCALLTYPE	DragEnter(struct IDataObject *,unsigned long,struct _POINTL,unsigned long *); 
	HRESULT	STDMETHODCALLTYPE	DragOver(unsigned long,struct _POINTL,unsigned long *); 
	HRESULT	STDMETHODCALLTYPE	DragLeave(void);
	HRESULT	STDMETHODCALLTYPE	Drop(struct IDataObject *,unsigned long,struct _POINTL,unsigned long *);

	BOOL						Register(CWnd* pWnd, UINT pDataType);
	void						Revoke();

	virtual	void				GotDrop(void);
	virtual	DWORD				GotDrag(void);
	virtual	void				GotLeave(void);
	virtual	DWORD				GotEnter(void);
public:
	BYTE			*m_Data;
	CPoint			m_DropPoint;
	DWORD			m_KeyState;

protected:
	CWnd*			m_DropTargetWnd;
	UINT			m_RegisterType;
	DWORD			m_dwRefCount;
};

#endif
