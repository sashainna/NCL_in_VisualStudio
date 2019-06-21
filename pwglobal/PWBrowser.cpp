/************************************************************************
c
c   FILE NAME: PWBrowser.cpp
c
c	 CONTAINS: 
c	      Browser routines.
c
c     COPYRIGHT 2009 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
      MODULE NAME AND RELEASE LEVEL
c        PWBrowser.cpp , 24.6
c     DATE AND TIME OF LAST  MODIFICATION
c        06/10/14 , 11:06:04
c
c**********************************************************************
*/
#include "afxdisp.h"
#define STRICT_TYPED_ITEMIDS
#include <shlobj.h>
#include <objbase.h>      // For COM headers
#include <shobjidl.h>     // for IFileDialogEvents and IFileDialogControlEvents
#include <shlwapi.h>
#include <knownfolders.h> // for KnownFolder APIs/datatypes/function headers
#include <propvarutil.h>  // for PROPVAR-related functions
#include <propkey.h>      // for the Property key APIs/datatypes
#include <propidl.h>      // for the Property System APIs
#include <strsafe.h>      // for StringCchPrintfW
#include <shtypes.h>      // for COMDLG_FILTERSPEC
#include <new>
#include <string.h>
#include <io.h>
#include <direct.h>
#include "PwFDialogevent.h"
#include "pwenv.h"
/*
.....those command will enable visual style window which we don't want to do it here
.....but in code when we need it, so remove it, otherwise, will cause some problems.
*/
/*
#if defined _M_IX86
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='x86' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_IA64
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='ia64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_X64
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='amd64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#else
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif
  */
#define CONTROL_GROUP           2000
#define IDD_MACH_LABEL 30001

static int m_descfl;
extern "C" void pwg_mdfdsc(char *, char *);
extern "C" void pw_break_fname(char *fullname, char *dir, char *fname);

HRESULT CDialogEventHandler::OnTypeChange(IFileDialog *pfd)
{
    IFileSaveDialog *pfsd;
    HRESULT hr = pfd->QueryInterface(&pfsd);
    if (SUCCEEDED(hr))
    {
        UINT uIndex;
        hr = pfsd->GetFileTypeIndex(&uIndex);
    }
    return hr;
}

HRESULT CDialogEventHandler::OnSelectionChange(IFileDialog *pfd) 
{
	if (m_descfl!=1)
		return S_OK;
	int nc;
	char fnam[UX_MAX_PATH];
	IShellItem *psiResult;
	HRESULT hr = pfd->GetCurrentSelection(&psiResult);
	if (SUCCEEDED(hr))
	{
		PWSTR pszFilePath = NULL;
		hr = psiResult->GetDisplayName(SIGDN_FILESYSPATH, &pszFilePath);
		if (SUCCEEDED(hr))
		{
			char *temp = new char[UX_MAX_PATH];
			hr = WideCharToMultiByte(CP_ACP, 0, pszFilePath, -1, temp, UX_MAX_PATH, NULL, NULL);
			nc = strlen(temp);
			strcpy_s(fnam, UX_MAX_PATH-1, temp);
		}
		psiResult->Release();
/*
.....use the fnam to update the description area
*/
		char mdesc[82];
		if (nc > 0) pwg_mdfdsc(fnam, mdesc);
		else mdesc[0] = '\0';
/*
........Display the Machine Description
*/
		IFileDialogCustomize *pfdc = NULL;
		hr = pfd->QueryInterface(IID_PPV_ARGS(&pfdc));
		if (SUCCEEDED(hr))
		{
			wchar_t wdesc[82];
			if (strlen(mdesc)>0)
			{
				mbstowcs(wdesc, mdesc, strlen(mdesc)+1);
				pfdc->SetControlLabel(IDD_MACH_LABEL, wdesc);
			}
			else
				pfdc->SetControlLabel(IDD_MACH_LABEL, L" ");
		}
	}
	return S_OK;
}

HRESULT CDialogEventHandler_CreateInstance(REFIID riid, void **ppv)
{
    *ppv = NULL;
    CDialogEventHandler *pDialogEventHandler = new (std::nothrow) CDialogEventHandler();
    HRESULT hr = pDialogEventHandler ? S_OK : E_OUTOFMEMORY;
    if (SUCCEEDED(hr))
    {
        hr = pDialogEventHandler->QueryInterface(riid, ppv);
        pDialogEventHandler->Release();
    }
    return hr;
}

extern "C" HRESULT CustomFileOpen(char* title, char *ntflt, char* fnam, int *nc, int flag, int definx, int descfl)
{				
    IFileDialog *pfd = NULL;
    HRESULT hr;
	if (flag)
		hr = CoCreateInstance(CLSID_FileOpenDialog, NULL, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&pfd));
    else
		hr = CoCreateInstance(CLSID_FileSaveDialog, NULL, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&pfd));
	if (SUCCEEDED(hr))
    {
		if(title != NULL)
		{
			CStringW strTitle(title);
			hr = (static_cast<IFileDialog*>(pfd))->SetTitle(strTitle.GetString());
			ENSURE(SUCCEEDED(hr));
		}
		if (ntflt != NULL)
		{
			CString strFilter = ntflt;
			LPTSTR pch = strFilter.GetBuffer(0);
			while ((pch = _tcschr(pch, '|')) != NULL)
				*pch++ = '\0';
			UINT nFilterCount = 0;
			LPCTSTR lpstrFilter = strFilter;
			while(lpstrFilter[0])
			{
				lpstrFilter += _tcslen(lpstrFilter)+1;
				lpstrFilter += _tcslen(lpstrFilter)+1;
				nFilterCount ++;
			}
			if (nFilterCount > 0)
			{
				COMDLG_FILTERSPEC* pFilter = NULL;
				pFilter = new COMDLG_FILTERSPEC[nFilterCount];
				ASSERT(pFilter != NULL);
				if (pFilter == NULL)
				{
					AfxThrowMemoryException();
				}
				lpstrFilter = strFilter;
				size_t nFilterIndex = 0;
				size_t filterSize;
				LPWSTR lpwstrFilter;
				while (nFilterIndex < nFilterCount)
				{
					CStringW strTemp;

					filterSize = _tcslen(lpstrFilter)+1;
					lpwstrFilter = static_cast<LPWSTR>(new WCHAR[filterSize]);
					ASSERT(lpwstrFilter != NULL);
					if (lpwstrFilter == NULL)
					{
						AfxThrowMemoryException();
					}
					strTemp = lpstrFilter;
					memcpy_s(lpwstrFilter, (strTemp.GetLength()+1)*sizeof(WCHAR),
						strTemp.GetString(), (strTemp.GetLength()+1)*sizeof(WCHAR));
					pFilter[nFilterIndex].pszName = lpwstrFilter;
					lpstrFilter += filterSize;

					filterSize = _tcslen(lpstrFilter)+1;
					lpwstrFilter = static_cast<LPWSTR>(new WCHAR[filterSize]);
					ASSERT(lpwstrFilter != NULL);
					if (lpwstrFilter == NULL)
					{
						AfxThrowMemoryException();
					}
					strTemp = lpstrFilter;
					memcpy_s(lpwstrFilter, (strTemp.GetLength()+1)*sizeof(WCHAR),
							strTemp.GetString(), (strTemp.GetLength()+1)*sizeof(WCHAR));
					pFilter[nFilterIndex].pszSpec = lpwstrFilter;	
					lpstrFilter += filterSize;
					nFilterIndex ++;
				}
				hr = (static_cast<IFileDialog*>(pfd))->SetFileTypes(nFilterCount, pFilter);
				ENSURE(SUCCEEDED(hr));

				for (nFilterIndex = 0; nFilterIndex < nFilterCount; nFilterIndex++)
				{
					delete[] pFilter[nFilterIndex].pszName;
					delete[] pFilter[nFilterIndex].pszSpec;
				}
				delete[] pFilter;

				hr = (static_cast<IFileDialog*>(pfd))->SetFileTypeIndex(definx+1);
				ENSURE(SUCCEEDED(hr));
			}
		}
		wchar_t wpath[UX_MAX_PATH];
		char path[UX_MAX_PATH], lfil[UX_MAX_PATH];
		if (strlen(fnam)>0)
		{
			pw_break_fname (fnam, path, lfil);
			CComPtr<IShellItem> psiFolder;
			mbstowcs(wpath, path, strlen(path)+1);
			HRESULT thr = SHCreateItemFromParsingName (wpath, NULL, IID_PPV_ARGS(&psiFolder) );
			if ( SUCCEEDED(thr) )
				pfd->SetFolder (psiFolder);									
			mbstowcs(wpath, lfil, strlen(lfil)+1);
			pfd->SetFileName (wpath);									
		}
		IFileDialogEvents   *pfde       = NULL;
		DWORD               dwCookie    = 0;
		hr = CDialogEventHandler_CreateInstance(IID_PPV_ARGS(&pfde));
		if (SUCCEEDED(hr))
		{
			hr = pfd->Advise(pfde, &dwCookie);
			if (SUCCEEDED(hr))
			{
				if (descfl)
				{
					IFileDialogCustomize *pfdc = NULL;
					hr = pfd->QueryInterface(IID_PPV_ARGS(&pfdc));
					if (SUCCEEDED(hr))
					{
						hr = pfdc->StartVisualGroup(CONTROL_GROUP, L"Description:");
						if (SUCCEEDED(hr))
						{
							hr = pfdc->AddText(IDD_MACH_LABEL, L" ");
							if (SUCCEEDED(hr))
							{
								hr = pfdc->SetControlState(IDD_MACH_LABEL, CDCS_VISIBLE | CDCS_ENABLED);
								if (SUCCEEDED(hr))
								{
									pfdc->EndVisualGroup();
								}
							}
						}
						pfdc->Release();
					}
					if (FAILED(hr))
					{
						pfd->Unadvise(dwCookie);
					}
				}
			}
			pfde->Release();
		}
		*nc = 0;
		fnam[0] = '\0';
		hr = pfd->Show(NULL);
		if (SUCCEEDED(hr))    
		{
			IShellItem *psiResult;
			hr = pfd->GetResult(&psiResult);
			if (SUCCEEDED(hr))
			{
				PWSTR pszFilePath = NULL;
				hr = psiResult->GetDisplayName(SIGDN_FILESYSPATH, &pszFilePath);
				if (SUCCEEDED(hr))
				{
					char *temp = new char[UX_MAX_PATH];
					hr = WideCharToMultiByte(CP_ACP, 0, pszFilePath, -1, temp, UX_MAX_PATH, NULL, NULL);
					*nc = strlen(temp);
					strcpy_s(fnam, UX_MAX_PATH-1, temp);
				}
				psiResult->Release();
            }
            pfd->Unadvise(dwCookie);
        }
        pfd->Release();
    }
    return hr;
}

int browsefile(char*filter, char*FileName, int flag=TRUE, int definx=0,
	int descfl=FALSE)
{
	int nc;
	char ntflt[10000];
/*
.....replace '\0' with'|' inside filter string
*/
	for (int i=0; i<10000;i++)
	{
		if (filter[i]!='\0')
		{
			ntflt[i] = filter[i];
		}
		else
		{
			ntflt[i] = '|';
			if (filter[i+1]=='\0')
			{
/*
......end filter
*/
				ntflt[i+1] = '|';
				ntflt[i+2] = '\0';
				break;
			}
		}
	}
		
	char save_dir[UX_MAX_PATH];
	int win_enable = 0;
	CWnd *MainWin = AfxGetMainWnd();
	GetCurrentDirectory(UX_MAX_PATH, save_dir);

	m_descfl = descfl;
	win_enable = MainWin->IsWindowEnabled();
	if (win_enable)
	{
		::EnableWindow(MainWin->GetSafeHwnd(), FALSE);
	}
	if (flag)
		CustomFileOpen("Open File:", ntflt, FileName, &nc, flag, definx, descfl);
	else
		CustomFileOpen("Save File:", ntflt, FileName, &nc, flag, definx, descfl);
	if (win_enable)
		::EnableWindow(MainWin->GetSafeHwnd(), TRUE);
	_chdir(save_dir);
	return nc;
}
