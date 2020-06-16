// BitmapHandler.h : Declaration of the CBitmapHandler

#pragma once
#include "SwUnibase_i.h"
#include "Swresource.h"       // main symbols
#include <comsvcs.h>

#include <list>

// CBitmapHandler

class ATL_NO_VTABLE CBitmapHandler :
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CBitmapHandler, &CLSID_BitmapHandler>,
	public IDispatchImpl<IBitmapHandler, &IID_IBitmapHandler, &LIBID_SwUnibaseLib, /*wMajor =*/ 1, /*wMinor =*/ 0>
{
public:
	CBitmapHandler()
	{
	}

	DECLARE_PROTECT_FINAL_CONSTRUCT()

	HRESULT FinalConstruct()
	{
		return S_OK;
	}

	void FinalRelease()
	{
	}

DECLARE_REGISTRY_RESOURCEID(IDR_BITMAPHANDLER)

DECLARE_NOT_AGGREGATABLE(CBitmapHandler)

BEGIN_COM_MAP(CBitmapHandler)
	COM_INTERFACE_ENTRY(IBitmapHandler)
	COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

public:
	STDMETHOD(CreateBitmapFileFromResource)(DWORD resID, BSTR* retval);
	STDMETHOD(Dispose)();

protected:
	CComBSTR CreateUniqueFileName();
	BOOL SaveHBitmapToDisk(CComBSTR filename, HBITMAP hBmp, HPALETTE hPal);
	BOOL CleanFilesFromDisk();
	
private:
	std::list<CString> createdFiles;


// IBitmapHandler
public:
};

OBJECT_ENTRY_AUTO(__uuidof(BitmapHandler), CBitmapHandler)
