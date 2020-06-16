// SwAddin.cpp : Implementation of DLL Exports.


#include "SwStdAfx.h"
#include "Swresource.h"
#include "SwUnibase.h"


class CSwUnibaseModule : public CAtlDllModuleT< CSwUnibaseModule >
{
public :

	DECLARE_LIBID(LIBID_SwUnibaseLib)

};

CSwUnibaseModule _AtlModule;

class CSwUnibaseApp : public CWinApp
{
public:

// Overrides
    virtual BOOL InitInstance();
    virtual int ExitInstance();

    DECLARE_MESSAGE_MAP()
};

BEGIN_MESSAGE_MAP(CSwUnibaseApp, CWinApp)
END_MESSAGE_MAP()

CSwUnibaseApp theApp;

BOOL CSwUnibaseApp::InitInstance()
{
    return CWinApp::InitInstance();
}

int CSwUnibaseApp::ExitInstance()
{
    return CWinApp::ExitInstance();
}


// Used to determine whether the DLL can be unloaded by OLE
STDAPI DllCanUnloadNow(void)
{
    AFX_MANAGE_STATE(AfxGetStaticModuleState());
    return (AfxDllCanUnloadNow()==S_OK && _AtlModule.GetLockCount()==0) ? S_OK : S_FALSE;
}


// Returns a class factory to create an object of the requested type
STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, LPVOID* ppv)
{
    return _AtlModule.DllGetClassObject(rclsid, riid, ppv);
}


// DllRegisterServer - Adds entries to the system registry
STDAPI DllRegisterServer(void)
{
    // registers object, typelib and all interfaces in typelib
    HRESULT hr = _AtlModule.DllRegisterServer();
	return hr;
}


// DllUnregisterServer - Removes entries from the system registry
STDAPI DllUnregisterServer(void)
{
	HRESULT hr = _AtlModule.DllUnregisterServer();
	return hr;
}

