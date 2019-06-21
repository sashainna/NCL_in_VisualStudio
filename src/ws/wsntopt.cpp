/********************************************************************* 
**  NAME:  wsntopt.cpp
**
**			Native WinNT advanced file browser functions
**			implementation of Customed IFileDialog functions
**			and related class CDialogEventHandler
**	CONTAINS: CDialogEventHandler class functions
**			HRESULT CDialogEventHandler_CreateInstance(REFIID riid, void **ppv)
**			uw_ntMDFileOpen(char* title, char* ntflt, int nfld, int *ifld, char **ftext,
**								char **fdir, char* fnam, int *nc, char *descrip, CWnd *MainWin)
**
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntopt.cpp , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**			12/01/15 , 10:31:41
*********************************************************************/
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
#include "wsntcfunc.h"
#include "wsntopt.h"

#if defined _M_IX86
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='x86' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_IA64
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='ia64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_X64
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='amd64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#else
  #pragma comment(linker, "/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif

// Controls
#define CONTROL_GROUP           2000
#define CONTROL_RADIOBUTTONLIST 2
#define CONTROL_RADIOBUTTON1    1
#define CONTROL_RADIOBUTTON2    2
#define CONTROL_RADIOBUTTON3    3
#define CONTROL_RADIOBUTTON4    4
#define CONTROL_RADIOBUTTON5    5       
#define CONTROL_RADIOBUTTON6    6
#define CONTROL_RADIOBUTTON7    7
#define CONTROL_RADIOBUTTON8    8
#define CONTROL_RADIOBUTTON9    9
#define CONTROL_RADIOBUTTON10    10       
#define CONTROL_RADIOBUTTON11    11
#define CONTROL_RADIOBUTTON12    12
#define CONTROL_RADIOBUTTON13    13
#define CONTROL_RADIOBUTTON14    14
#define CONTROL_RADIOBUTTON15    15       
#define CONTROL_RADIOBUTTON16    16
#define CONTROL_RADIOBUTTON17    17
#define CONTROL_RADIOBUTTON18    18
#define CONTROL_RADIOBUTTON19    19
#define CONTROL_RADIOBUTTON20    20      
                                      
static char m_dirtext[20][UX_MAX_PATH_LEN], m_dir[20][UX_MAX_PATH_LEN]; 
static int m_dirnum = 0;
static int m_dirstat = 0;

bool PathFromShellItem ( IShellItem* pItem, CString& sPath )
{
	HRESULT hr;
	LPOLESTR pwsz = NULL;
    hr = pItem->GetDisplayName ( SIGDN_FILESYSPATH, &pwsz );

    if ( FAILED(hr) )
        return false;

    sPath = pwsz;
    CoTaskMemFree ( pwsz );
    return true;
}

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

HRESULT CDialogEventHandler::OnItemSelected(IFileDialogCustomize *pfdc, DWORD dwIDCtl, DWORD dwIDItem)
{
    IFileDialog *pfd = NULL;
    HRESULT thr, hr = pfdc->QueryInterface(&pfd);
    if (SUCCEEDED(hr))
    {
        if (dwIDCtl == CONTROL_RADIOBUTTONLIST)
        {
			CComPtr<IShellItem> psiFolder;
			wchar_t wpath[UX_MAX_PATH_LEN];
            switch (dwIDItem)
            {
            case CONTROL_RADIOBUTTON1:
				mbstowcs(wpath, m_dir[0],strlen(m_dir[0])+1);
				thr = SHCreateItemFromParsingName (wpath, NULL, IID_PPV_ARGS(&psiFolder) );
				if ( SUCCEEDED(thr) )
					pfd->SetFolder ( psiFolder );									
                break;
            case CONTROL_RADIOBUTTON2:
				mbstowcs(wpath, m_dir[1],strlen(m_dir[1])+1);
				thr = SHCreateItemFromParsingName (wpath, NULL, IID_PPV_ARGS(&psiFolder) );
				if ( SUCCEEDED(thr) )
					pfd->SetFolder ( psiFolder );									
                break;
            case CONTROL_RADIOBUTTON3:
				mbstowcs(wpath, m_dir[2],strlen(m_dir[2])+1);
				thr = SHCreateItemFromParsingName (wpath, NULL, IID_PPV_ARGS(&psiFolder) );
				if ( SUCCEEDED(thr) )
					pfd->SetFolder ( psiFolder );									
                break;
            case CONTROL_RADIOBUTTON4:
				mbstowcs(wpath, m_dir[3],strlen(m_dir[3])+1);
				thr = SHCreateItemFromParsingName (wpath, NULL, IID_PPV_ARGS(&psiFolder) );
				if ( SUCCEEDED(thr) )
					pfd->SetFolder ( psiFolder );									
                break;
            case CONTROL_RADIOBUTTON5:
				mbstowcs(wpath, m_dir[4],strlen(m_dir[4])+1);
				thr = SHCreateItemFromParsingName (wpath, NULL, IID_PPV_ARGS(&psiFolder) );
				if ( SUCCEEDED(thr) )
					pfd->SetFolder ( psiFolder );									
                break;
            }
        }
        pfd->Release();
    }
    return hr;
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

static void SetDirectory(int nfld, int *ifld, char **ftext, char **fdir)
{
	int i;
	char tempdir[UX_MAX_PATH_LEN], direc[UX_MAX_PATH_LEN], defdirec[UX_MAX_PATH_LEN];

	m_dirnum = nfld;
	m_dirstat = *ifld;

	for (i=0; (i<m_dirnum)&&(i<20); i++)
	{
		strcpy(m_dirtext[i], ftext[i]);
		strcpy_s(tempdir, sizeof(tempdir), fdir[i]);
		if (tempdir[0]!='\0')
		{
			ul_getvalid_fulldir(tempdir, direc);
			if (direc[0]=='\0')
			{
				GetCurrentDirectory(UX_MAX_PATH_LEN, defdirec);
				strcpy(m_dir[i], defdirec);
			}
			else
				strcpy(m_dir[i], direc);
		}
		else
		{
			GetCurrentDirectory(UX_MAX_PATH_LEN, defdirec);
			strcpy(m_dir[i], defdirec);
		}
	}
}

extern "C" HRESULT CustomFileOpen(char* title, char* ntflt, int nfld, int *ifld, char **ftext,
								char **fdir, char* fnam, int *nc, char *descrip, CWnd *MainWin, int flag)
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
			// MFC delimits with '|' not '\0'
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

				hr = (static_cast<IFileDialog*>(pfd))->SetFileTypeIndex(1);
				ENSURE(SUCCEEDED(hr));
			}
		}		
		char dir[UX_MAX_PATH_LEN], name[UX_MAX_PATH_LEN];
		IFileDialogEvents   *pfde       = NULL;
        DWORD               dwCookie    = 0;
        hr = CDialogEventHandler_CreateInstance(IID_PPV_ARGS(&pfde));
		wchar_t wpath1[UX_MAX_PATH_LEN], wpath2[UX_MAX_PATH_LEN];
		wchar_t wpath3[UX_MAX_PATH_LEN], wpath4[UX_MAX_PATH_LEN], wpath5[UX_MAX_PATH_LEN];
		wchar_t wpath6[UX_MAX_PATH_LEN], wpath7[UX_MAX_PATH_LEN];
		wchar_t wpath8[UX_MAX_PATH_LEN], wpath9[UX_MAX_PATH_LEN], wpath10[UX_MAX_PATH_LEN];
		wchar_t wpath11[UX_MAX_PATH_LEN], wpath12[UX_MAX_PATH_LEN];
		wchar_t wpath13[UX_MAX_PATH_LEN], wpath14[UX_MAX_PATH_LEN], wpath15[UX_MAX_PATH_LEN];
		wchar_t wpath16[UX_MAX_PATH_LEN], wpath17[UX_MAX_PATH_LEN];
		wchar_t wpath18[UX_MAX_PATH_LEN], wpath19[UX_MAX_PATH_LEN], wpath20[UX_MAX_PATH_LEN];

		wchar_t wlabel1[UX_MAX_PATH_LEN], wlabel2[UX_MAX_PATH_LEN];
		wchar_t wlabel3[UX_MAX_PATH_LEN], wlabel4[UX_MAX_PATH_LEN], wlabel5[UX_MAX_PATH_LEN];
		wchar_t wlabel6[UX_MAX_PATH_LEN], wlabel7[UX_MAX_PATH_LEN];
		wchar_t wlabel8[UX_MAX_PATH_LEN], wlabel9[UX_MAX_PATH_LEN], wlabel10[UX_MAX_PATH_LEN];
		wchar_t wlabel11[UX_MAX_PATH_LEN], wlabel12[UX_MAX_PATH_LEN];
		wchar_t wlabel13[UX_MAX_PATH_LEN], wlabel14[UX_MAX_PATH_LEN], wlabel15[UX_MAX_PATH_LEN];
		wchar_t wlabel16[UX_MAX_PATH_LEN], wlabel17[UX_MAX_PATH_LEN];
		wchar_t wlabel18[UX_MAX_PATH_LEN], wlabel19[UX_MAX_PATH_LEN], wlabel20[UX_MAX_PATH_LEN];
        if (SUCCEEDED(hr))
        {
            hr = pfd->Advise(pfde, &dwCookie);
            if (SUCCEEDED(hr))
            {
                IFileDialogCustomize *pfdc = NULL;
                hr = pfd->QueryInterface(IID_PPV_ARGS(&pfdc));
                if (SUCCEEDED(hr))
                {
					SetDirectory(nfld, ifld, ftext, fdir);
                    hr = pfdc->StartVisualGroup(CONTROL_GROUP, L"Folder:");
                    if (SUCCEEDED(hr))
                    {
						hr = pfdc->AddComboBox(CONTROL_RADIOBUTTONLIST);
                        if (SUCCEEDED(hr))
                        {
                            hr = pfdc->SetControlState(CONTROL_RADIOBUTTONLIST, CDCS_VISIBLE | CDCS_ENABLED);
                            if (SUCCEEDED(hr))
                            {
								if (nfld>=1)
								{
									mbstowcs(wpath1, m_dir[0],strlen(m_dir[0])+1);
									mbstowcs(wlabel1, m_dirtext[0],strlen(m_dirtext[0])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																  CONTROL_RADIOBUTTON1,
																  wlabel1);
								}
								if (nfld>=2)
								{
									mbstowcs(wpath2, m_dir[1],strlen(m_dir[1])+1);
									mbstowcs(wlabel2, m_dirtext[1],strlen(m_dirtext[1])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON2,
																 wlabel2);
								}
								if (nfld>=3)
								{
									mbstowcs(wpath3, m_dir[2],strlen(m_dir[2])+1);
									mbstowcs(wlabel3, m_dirtext[2],strlen(m_dirtext[2])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON3,
																 wlabel3);
								}
								if (nfld>=4)
								{
									mbstowcs(wpath4, m_dir[3],strlen(m_dir[3])+1);
									mbstowcs(wlabel4, m_dirtext[3],strlen(m_dirtext[3])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON4,
																 wlabel4);
								}
								if (nfld==5)
								{
									mbstowcs(wpath5, m_dir[4],strlen(m_dir[4])+1);
									mbstowcs(wlabel5, m_dirtext[4],strlen(m_dirtext[4])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON5,
																 wlabel5);
								}
								if (nfld>=6)
								{
									mbstowcs(wpath6, m_dir[5],strlen(m_dir[5])+1);
									mbstowcs(wlabel6, m_dirtext[5],strlen(m_dirtext[5])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON6,
																 wlabel6);
								}
								if (nfld==7)
								{
									mbstowcs(wpath7, m_dir[6],strlen(m_dir[6])+1);
									mbstowcs(wlabel7, m_dirtext[6],strlen(m_dirtext[6])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON7,
																 wlabel7);
								}
								if (nfld==8)
								{
									mbstowcs(wpath8, m_dir[7],strlen(m_dir[7])+1);
									mbstowcs(wlabel8, m_dirtext[7],strlen(m_dirtext[7])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON8,
																 wlabel8);
								}
								if (nfld==9)
								{
									mbstowcs(wpath9, m_dir[8],strlen(m_dir[8])+1);
									mbstowcs(wlabel9, m_dirtext[8],strlen(m_dirtext[8])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON9,
																 wlabel9);
								}
								if (nfld==10)
								{
									mbstowcs(wpath10, m_dir[9],strlen(m_dir[9])+1);
									mbstowcs(wlabel10, m_dirtext[9],strlen(m_dirtext[9])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON10,
																 wlabel10);
								}
								if (nfld>=11)
								{
									mbstowcs(wpath11, m_dir[10],strlen(m_dir[10])+1);
									mbstowcs(wlabel11, m_dirtext[10],strlen(m_dirtext[10])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																  CONTROL_RADIOBUTTON11,
																  wlabel11);
								}
								if (nfld>=12)
								{
									mbstowcs(wpath12, m_dir[11],strlen(m_dir[11])+1);
									mbstowcs(wlabel12, m_dirtext[11],strlen(m_dirtext[11])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON12,
																 wlabel12);
								}
								if (nfld>=13)
								{
									mbstowcs(wpath13, m_dir[12],strlen(m_dir[12])+1);
									mbstowcs(wlabel13, m_dirtext[12],strlen(m_dirtext[12])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON13,
																 wlabel13);
								}
								if (nfld>=14)
								{
									mbstowcs(wpath14, m_dir[13],strlen(m_dir[13])+1);
									mbstowcs(wlabel14, m_dirtext[13],strlen(m_dirtext[13])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON14,
																 wlabel14);
								}
								if (nfld==15)
								{
									mbstowcs(wpath15, m_dir[14],strlen(m_dir[14])+1);
									mbstowcs(wlabel15, m_dirtext[14],strlen(m_dirtext[14])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON15,
																 wlabel15);
								}
								if (nfld>=16)
								{
									mbstowcs(wpath16, m_dir[15],strlen(m_dir[15])+1);
									mbstowcs(wlabel16, m_dirtext[15],strlen(m_dirtext[15])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON16,
																 wlabel16);
								}
								if (nfld==17)
								{
									mbstowcs(wpath17, m_dir[16],strlen(m_dir[16])+1);
									mbstowcs(wlabel17, m_dirtext[16],strlen(m_dirtext[16])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON17,
																 wlabel17);
								}
								if (nfld==18)
								{
									mbstowcs(wpath18, m_dir[17],strlen(m_dir[17])+1);
									mbstowcs(wlabel18, m_dirtext[17],strlen(m_dirtext[17])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON18,
																 wlabel18);
								}
								if (nfld==19)
								{
									mbstowcs(wpath19, m_dir[18],strlen(m_dir[18])+1);
									mbstowcs(wlabel19, m_dirtext[18],strlen(m_dirtext[18])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON19,
																 wlabel19);
								}
								if (nfld==20)
								{
									mbstowcs(wpath20, m_dir[19],strlen(m_dir[19])+1);
									mbstowcs(wlabel20, m_dirtext[19],strlen(m_dirtext[19])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON20,
																 wlabel20);
								}
								if (SUCCEEDED(hr))
								{
									ul_break_fname(fnam, dir, name);
/*
.....if the directory match one of the directory group, set it
*/
									if ((nfld>1)&&(fnam[0]!='\0'))
									{
										for (int m=0;m<nfld;m++)
										{
											if (stricmp(m_dir[m], dir)==0)
											{
												hr = pfdc->SetSelectedControlItem(CONTROL_RADIOBUTTONLIST,
																			CONTROL_RADIOBUTTON1+m);					
												break;
											}
										}
									}
									else
									{
										hr = pfdc->SetSelectedControlItem(CONTROL_RADIOBUTTONLIST,
															CONTROL_RADIOBUTTON1);					
										CComPtr<IShellItem> psiFolder;
										hr = SHCreateItemFromParsingName (wpath1, NULL, IID_PPV_ARGS(&psiFolder) );

										if ( SUCCEEDED(hr) )
											pfd->SetFolder ( psiFolder );	
									}
								}
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
            pfde->Release();
		}

		if (SUCCEEDED(hr))
        {
			*nc = 0;
			wchar_t wfnam[UX_MAX_PATH_LEN], wfolder[UX_MAX_PATH_LEN];
			char dir[UX_MAX_PATH_LEN], name[UX_MAX_PATH_LEN];
			CComPtr<IShellItem> psiFolder;
			if (fnam[0] != '\0')
			{
				mbstowcs(wfnam, fnam, strlen(fnam)+1);
				pfd->SetFileName(wfnam);
				ul_break_fname(fnam, dir, name);
				if (dir[0] != '\0')
				{
					mbstowcs(wfolder, dir, strlen(dir)+1);
					hr = SHCreateItemFromParsingName (wfolder, NULL, IID_PPV_ARGS(&psiFolder) );
					if ( SUCCEEDED(hr) )
						pfd->SetFolder ( psiFolder );									
/*
.....reset file name to name only
*/
					mbstowcs(wfnam, name, strlen(name)+1);
					pfd->SetFileName(wfnam);
				}
			}
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
						char *temp = new char[UX_MAX_PATH_LEN];
						hr = WideCharToMultiByte(CP_ACP, 0, pszFilePath, -1, temp, UX_MAX_PATH_LEN, NULL, NULL);
						*nc = strlen(temp);
						strcpy_s(fnam, UX_MAX_PATH_LEN-1, temp);
					}
					psiResult->Release();
				}
            }
            pfd->Unadvise(dwCookie);
        }
        pfd->Release();
    }
    return hr;
}
/*
......the old code using radio box
*/
extern "C" HRESULT CustomFileOpen2(char* title, char* ntflt, int nfld, int *ifld, char **ftext,
								char **fdir, char* fnam, int *nc, char *descrip, CWnd *MainWin)
{
    IFileDialog *pfd = NULL;
    HRESULT hr = CoCreateInstance(CLSID_FileOpenDialog, NULL, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&pfd));
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
			// MFC delimits with '|' not '\0'
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

				hr = (static_cast<IFileDialog*>(pfd))->SetFileTypeIndex(1);
				ENSURE(SUCCEEDED(hr));
			}
		}		
		IFileDialogEvents   *pfde       = NULL;
        DWORD               dwCookie    = 0;
        hr = CDialogEventHandler_CreateInstance(IID_PPV_ARGS(&pfde));
		wchar_t wpath1[UX_MAX_PATH_LEN], wpath2[UX_MAX_PATH_LEN];
		wchar_t wpath3[UX_MAX_PATH_LEN], wpath4[UX_MAX_PATH_LEN], wpath5[UX_MAX_PATH_LEN];
		wchar_t wpath6[UX_MAX_PATH_LEN], wpath7[UX_MAX_PATH_LEN];
		wchar_t wpath8[UX_MAX_PATH_LEN], wpath9[UX_MAX_PATH_LEN], wpath10[UX_MAX_PATH_LEN];
		wchar_t wpath11[UX_MAX_PATH_LEN], wpath12[UX_MAX_PATH_LEN];
		wchar_t wpath13[UX_MAX_PATH_LEN], wpath14[UX_MAX_PATH_LEN], wpath15[UX_MAX_PATH_LEN];
		wchar_t wpath16[UX_MAX_PATH_LEN], wpath17[UX_MAX_PATH_LEN];
		wchar_t wpath18[UX_MAX_PATH_LEN], wpath19[UX_MAX_PATH_LEN], wpath20[UX_MAX_PATH_LEN];

		wchar_t wlabel1[UX_MAX_PATH_LEN], wlabel2[UX_MAX_PATH_LEN];
		wchar_t wlabel3[UX_MAX_PATH_LEN], wlabel4[UX_MAX_PATH_LEN], wlabel5[UX_MAX_PATH_LEN];
		wchar_t wlabel6[UX_MAX_PATH_LEN], wlabel7[UX_MAX_PATH_LEN];
		wchar_t wlabel8[UX_MAX_PATH_LEN], wlabel9[UX_MAX_PATH_LEN], wlabel10[UX_MAX_PATH_LEN];
		wchar_t wlabel11[UX_MAX_PATH_LEN], wlabel12[UX_MAX_PATH_LEN];
		wchar_t wlabel13[UX_MAX_PATH_LEN], wlabel14[UX_MAX_PATH_LEN], wlabel15[UX_MAX_PATH_LEN];
		wchar_t wlabel16[UX_MAX_PATH_LEN], wlabel17[UX_MAX_PATH_LEN];
		wchar_t wlabel18[UX_MAX_PATH_LEN], wlabel19[UX_MAX_PATH_LEN], wlabel20[UX_MAX_PATH_LEN];
        if (SUCCEEDED(hr))
        {
            hr = pfd->Advise(pfde, &dwCookie);
            if (SUCCEEDED(hr))
            {
                IFileDialogCustomize *pfdc = NULL;
                hr = pfd->QueryInterface(IID_PPV_ARGS(&pfdc));
                if (SUCCEEDED(hr))
                {
					SetDirectory(nfld, ifld, ftext, fdir);
                    hr = pfdc->StartVisualGroup(CONTROL_GROUP, L"Folder:");
                    if (SUCCEEDED(hr))
                    {
                        hr = pfdc->AddRadioButtonList(CONTROL_RADIOBUTTONLIST);
                        if (SUCCEEDED(hr))
                        {
                            hr = pfdc->SetControlState(CONTROL_RADIOBUTTONLIST, CDCS_VISIBLE | CDCS_ENABLED);
                            if (SUCCEEDED(hr))
                            {
								if (nfld>=1)
								{
									mbstowcs(wpath1, m_dir[0],strlen(m_dir[0])+1);
									mbstowcs(wlabel1, m_dirtext[0],strlen(m_dirtext[0])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																  CONTROL_RADIOBUTTON1,
																  wlabel1);
								}
								if (nfld>=2)
								{
									mbstowcs(wpath2, m_dir[1],strlen(m_dir[1])+1);
									mbstowcs(wlabel2, m_dirtext[1],strlen(m_dirtext[1])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON2,
																 wlabel2);
								}
								if (nfld>=3)
								{
									mbstowcs(wpath3, m_dir[2],strlen(m_dir[2])+1);
									mbstowcs(wlabel3, m_dirtext[2],strlen(m_dirtext[2])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON3,
																 wlabel3);
								}
								if (nfld>=4)
								{
									mbstowcs(wpath4, m_dir[3],strlen(m_dir[3])+1);
									mbstowcs(wlabel4, m_dirtext[3],strlen(m_dirtext[3])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON4,
																 wlabel4);
								}
								if (nfld==5)
								{
									mbstowcs(wpath5, m_dir[4],strlen(m_dir[4])+1);
									mbstowcs(wlabel5, m_dirtext[4],strlen(m_dirtext[4])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON5,
																 wlabel5);
								}
								if (nfld>=6)
								{
									mbstowcs(wpath6, m_dir[5],strlen(m_dir[5])+1);
									mbstowcs(wlabel6, m_dirtext[5],strlen(m_dirtext[5])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON6,
																 wlabel6);
								}
								if (nfld==7)
								{
									mbstowcs(wpath7, m_dir[6],strlen(m_dir[6])+1);
									mbstowcs(wlabel7, m_dirtext[6],strlen(m_dirtext[6])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON7,
																 wlabel7);
								}
								if (nfld==8)
								{
									mbstowcs(wpath8, m_dir[7],strlen(m_dir[7])+1);
									mbstowcs(wlabel8, m_dirtext[7],strlen(m_dirtext[7])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON8,
																 wlabel8);
								}
								if (nfld==9)
								{
									mbstowcs(wpath9, m_dir[8],strlen(m_dir[8])+1);
									mbstowcs(wlabel9, m_dirtext[8],strlen(m_dirtext[8])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON9,
																 wlabel9);
								}
								if (nfld==10)
								{
									mbstowcs(wpath10, m_dir[9],strlen(m_dir[9])+1);
									mbstowcs(wlabel10, m_dirtext[9],strlen(m_dirtext[9])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON10,
																 wlabel10);
								}
								if (nfld>=11)
								{
									mbstowcs(wpath11, m_dir[10],strlen(m_dir[10])+1);
									mbstowcs(wlabel11, m_dirtext[10],strlen(m_dirtext[10])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																  CONTROL_RADIOBUTTON11,
																  wlabel11);
								}
								if (nfld>=12)
								{
									mbstowcs(wpath12, m_dir[11],strlen(m_dir[11])+1);
									mbstowcs(wlabel12, m_dirtext[11],strlen(m_dirtext[11])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON12,
																 wlabel12);
								}
								if (nfld>=13)
								{
									mbstowcs(wpath13, m_dir[12],strlen(m_dir[12])+1);
									mbstowcs(wlabel13, m_dirtext[12],strlen(m_dirtext[12])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON13,
																 wlabel13);
								}
								if (nfld>=14)
								{
									mbstowcs(wpath14, m_dir[13],strlen(m_dir[13])+1);
									mbstowcs(wlabel14, m_dirtext[13],strlen(m_dirtext[13])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON14,
																 wlabel14);
								}
								if (nfld==15)
								{
									mbstowcs(wpath15, m_dir[14],strlen(m_dir[14])+1);
									mbstowcs(wlabel15, m_dirtext[14],strlen(m_dirtext[14])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON15,
																 wlabel15);
								}
								if (nfld>=16)
								{
									mbstowcs(wpath16, m_dir[15],strlen(m_dir[15])+1);
									mbstowcs(wlabel16, m_dirtext[15],strlen(m_dirtext[15])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON16,
																 wlabel16);
								}
								if (nfld==17)
								{
									mbstowcs(wpath17, m_dir[16],strlen(m_dir[16])+1);
									mbstowcs(wlabel17, m_dirtext[16],strlen(m_dirtext[16])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON17,
																 wlabel17);
								}
								if (nfld==18)
								{
									mbstowcs(wpath18, m_dir[17],strlen(m_dir[17])+1);
									mbstowcs(wlabel18, m_dirtext[17],strlen(m_dirtext[17])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON18,
																 wlabel18);
								}
								if (nfld==19)
								{
									mbstowcs(wpath19, m_dir[18],strlen(m_dir[18])+1);
									mbstowcs(wlabel19, m_dirtext[18],strlen(m_dirtext[18])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON19,
																 wlabel19);
								}
								if (nfld==20)
								{
									mbstowcs(wpath20, m_dir[19],strlen(m_dir[19])+1);
									mbstowcs(wlabel20, m_dirtext[19],strlen(m_dirtext[19])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON20,
																 wlabel20);
								}
								if (SUCCEEDED(hr))
								{
									hr = pfdc->SetSelectedControlItem(CONTROL_RADIOBUTTONLIST,
															CONTROL_RADIOBUTTON1);					
									CComPtr<IShellItem> psiFolder;
									hr = SHCreateItemFromParsingName (wpath1, NULL, IID_PPV_ARGS(&psiFolder) );

									if ( SUCCEEDED(hr) )
										pfd->SetFolder ( psiFolder );									
								}
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
            pfde->Release();
		}

		if (SUCCEEDED(hr))
        {
/*			IShellItem *psi = NULL;
			LPWSTR wszPath_add = wpath1;
			hr = SHCreateItemFromParsingName (wszPath_add, NULL, IID_PPV_ARGS(&psi) );
			if (SUCCEEDED(hr))
			{
				if (nfld>=1)
					hr = pfd->AddPlace(psi, FDAP_BOTTOM);
			}
			if (nfld>=2)
			{
				wszPath_add = wpath2;
				hr = SHCreateItemFromParsingName (wszPath_add, NULL, IID_PPV_ARGS(&psi) );
				if (SUCCEEDED(hr))
				{
					hr = pfd->AddPlace(psi, FDAP_BOTTOM);
				}
			}
			if (nfld>=3)
			{
				wszPath_add = wpath3;
				hr = SHCreateItemFromParsingName (wszPath_add, NULL, IID_PPV_ARGS(&psi) );
				if (SUCCEEDED(hr))
				{
					hr = pfd->AddPlace(psi, FDAP_BOTTOM);
				}
			}
			if (nfld>=4)
			{
				wszPath_add = wpath4;
				hr = SHCreateItemFromParsingName (wszPath_add, NULL, IID_PPV_ARGS(&psi) );
				if (SUCCEEDED(hr))
				{
					hr = pfd->AddPlace(psi, FDAP_BOTTOM);
				}
			}
			if (nfld>=5)
			{
				wszPath_add = wpath5;
				hr = SHCreateItemFromParsingName (wszPath_add, NULL, IID_PPV_ARGS(&psi) );
				if (SUCCEEDED(hr))
				{
					hr = pfd->AddPlace(psi, FDAP_BOTTOM);
				}
			}
*/
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
						char *temp = new char[UX_MAX_PATH_LEN];
						hr = WideCharToMultiByte(CP_ACP, 0, pszFilePath, -1, temp, UX_MAX_PATH_LEN, NULL, NULL);
						*nc = strlen(temp);
						strcpy_s(fnam, UX_MAX_PATH_LEN-1, temp);
					}
					psiResult->Release();
				}
            }
            pfd->Unadvise(dwCookie);
        }
        pfd->Release();
    }
    return hr;
}

extern "C" HRESULT CustomFileOpenDir(char* title, int nfld, int *ifld, char **ftext,
								char **fdir, char* fnam, int *nc, CWnd *MainWin, int flag)
{
    IFileDialog *pfd = NULL;
    HRESULT hr;
//	hr = CoCreateInstance(CLSID_FileOpenDialog, NULL, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&pfd));
	if (flag)
		hr = CoCreateInstance(CLSID_FileOpenDialog, NULL, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&pfd));
    else
		hr = CoCreateInstance(CLSID_FileSaveDialog, NULL, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&pfd));
    if (SUCCEEDED(hr))
    {
		DWORD dwOption;
		if (SUCCEEDED(pfd->GetOptions(&dwOption)))
		{
			pfd->SetOptions(dwOption|FOS_PICKFOLDERS);
		}
		if(title != NULL)
		{
			CStringW strTitle(title);
			hr = (static_cast<IFileDialog*>(pfd))->SetTitle(strTitle.GetString());
			ENSURE(SUCCEEDED(hr));
		}
		char dir[UX_MAX_PATH_LEN], name[UX_MAX_PATH_LEN];
		IFileDialogEvents   *pfde       = NULL;
        DWORD               dwCookie    = 0;
        hr = CDialogEventHandler_CreateInstance(IID_PPV_ARGS(&pfde));
		wchar_t wpath1[UX_MAX_PATH_LEN], wpath2[UX_MAX_PATH_LEN];
		wchar_t wpath3[UX_MAX_PATH_LEN], wpath4[UX_MAX_PATH_LEN], wpath5[UX_MAX_PATH_LEN];
		wchar_t wpath6[UX_MAX_PATH_LEN], wpath7[UX_MAX_PATH_LEN];
		wchar_t wpath8[UX_MAX_PATH_LEN], wpath9[UX_MAX_PATH_LEN], wpath10[UX_MAX_PATH_LEN];
		wchar_t wpath11[UX_MAX_PATH_LEN], wpath12[UX_MAX_PATH_LEN];
		wchar_t wpath13[UX_MAX_PATH_LEN], wpath14[UX_MAX_PATH_LEN], wpath15[UX_MAX_PATH_LEN];
		wchar_t wpath16[UX_MAX_PATH_LEN], wpath17[UX_MAX_PATH_LEN];
		wchar_t wpath18[UX_MAX_PATH_LEN], wpath19[UX_MAX_PATH_LEN], wpath20[UX_MAX_PATH_LEN];

		wchar_t wlabel1[UX_MAX_PATH_LEN], wlabel2[UX_MAX_PATH_LEN];
		wchar_t wlabel3[UX_MAX_PATH_LEN], wlabel4[UX_MAX_PATH_LEN], wlabel5[UX_MAX_PATH_LEN];
		wchar_t wlabel6[UX_MAX_PATH_LEN], wlabel7[UX_MAX_PATH_LEN];
		wchar_t wlabel8[UX_MAX_PATH_LEN], wlabel9[UX_MAX_PATH_LEN], wlabel10[UX_MAX_PATH_LEN];
		wchar_t wlabel11[UX_MAX_PATH_LEN], wlabel12[UX_MAX_PATH_LEN];
		wchar_t wlabel13[UX_MAX_PATH_LEN], wlabel14[UX_MAX_PATH_LEN], wlabel15[UX_MAX_PATH_LEN];
		wchar_t wlabel16[UX_MAX_PATH_LEN], wlabel17[UX_MAX_PATH_LEN];
		wchar_t wlabel18[UX_MAX_PATH_LEN], wlabel19[UX_MAX_PATH_LEN], wlabel20[UX_MAX_PATH_LEN];
        if (SUCCEEDED(hr))
        {
            hr = pfd->Advise(pfde, &dwCookie);
            if (SUCCEEDED(hr))
            {
                IFileDialogCustomize *pfdc = NULL;
                hr = pfd->QueryInterface(IID_PPV_ARGS(&pfdc));
                if (SUCCEEDED(hr))
				{
					SetDirectory(nfld, ifld, ftext, fdir);
                    hr = pfdc->StartVisualGroup(CONTROL_GROUP, L"Folder:");
                    if (SUCCEEDED(hr))
                    {
						hr = pfdc->AddComboBox(CONTROL_RADIOBUTTONLIST);
                        if (SUCCEEDED(hr))
                        {
                            hr = pfdc->SetControlState(CONTROL_RADIOBUTTONLIST, CDCS_VISIBLE | CDCS_ENABLED);
                            if (SUCCEEDED(hr))
                            {
								if (nfld>=1)
								{
									mbstowcs(wpath1, m_dir[0],strlen(m_dir[0])+1);
									mbstowcs(wlabel1, m_dirtext[0],strlen(m_dirtext[0])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																  CONTROL_RADIOBUTTON1,
																  wlabel1);
								}
								if (nfld>=2)
								{
									mbstowcs(wpath2, m_dir[1],strlen(m_dir[1])+1);
									mbstowcs(wlabel2, m_dirtext[1],strlen(m_dirtext[1])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON2,
																 wlabel2);
								}
								if (nfld>=3)
								{
									mbstowcs(wpath3, m_dir[2],strlen(m_dir[2])+1);
									mbstowcs(wlabel3, m_dirtext[2],strlen(m_dirtext[2])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON3,
																 wlabel3);
								}
								if (nfld>=4)
								{
									mbstowcs(wpath4, m_dir[3],strlen(m_dir[3])+1);
									mbstowcs(wlabel4, m_dirtext[3],strlen(m_dirtext[3])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON4,
																 wlabel4);
								}
								if (nfld==5)
								{
									mbstowcs(wpath5, m_dir[4],strlen(m_dir[4])+1);
									mbstowcs(wlabel5, m_dirtext[4],strlen(m_dirtext[4])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON5,
																 wlabel5);
								}
								if (nfld>=6)
								{
									mbstowcs(wpath6, m_dir[5],strlen(m_dir[5])+1);
									mbstowcs(wlabel6, m_dirtext[5],strlen(m_dirtext[5])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON6,
																 wlabel6);
								}
								if (nfld==7)
								{
									mbstowcs(wpath7, m_dir[6],strlen(m_dir[6])+1);
									mbstowcs(wlabel7, m_dirtext[6],strlen(m_dirtext[6])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON7,
																 wlabel7);
								}
								if (nfld==8)
								{
									mbstowcs(wpath8, m_dir[7],strlen(m_dir[7])+1);
									mbstowcs(wlabel8, m_dirtext[7],strlen(m_dirtext[7])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON8,
																 wlabel8);
								}
								if (nfld==9)
								{
									mbstowcs(wpath9, m_dir[8],strlen(m_dir[8])+1);
									mbstowcs(wlabel9, m_dirtext[8],strlen(m_dirtext[8])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON9,
																 wlabel9);
								}
								if (nfld==10)
								{
									mbstowcs(wpath10, m_dir[9],strlen(m_dir[9])+1);
									mbstowcs(wlabel10, m_dirtext[9],strlen(m_dirtext[9])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON10,
																 wlabel10);
								}
								if (nfld>=11)
								{
									mbstowcs(wpath11, m_dir[10],strlen(m_dir[10])+1);
									mbstowcs(wlabel11, m_dirtext[10],strlen(m_dirtext[10])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																  CONTROL_RADIOBUTTON11,
																  wlabel11);
								}
								if (nfld>=12)
								{
									mbstowcs(wpath12, m_dir[11],strlen(m_dir[11])+1);
									mbstowcs(wlabel12, m_dirtext[11],strlen(m_dirtext[11])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON12,
																 wlabel12);
								}
								if (nfld>=13)
								{
									mbstowcs(wpath13, m_dir[12],strlen(m_dir[12])+1);
									mbstowcs(wlabel13, m_dirtext[12],strlen(m_dirtext[12])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON13,
																 wlabel13);
								}
								if (nfld>=14)
								{
									mbstowcs(wpath14, m_dir[13],strlen(m_dir[13])+1);
									mbstowcs(wlabel14, m_dirtext[13],strlen(m_dirtext[13])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON14,
																 wlabel14);
								}
								if (nfld==15)
								{
									mbstowcs(wpath15, m_dir[14],strlen(m_dir[14])+1);
									mbstowcs(wlabel15, m_dirtext[14],strlen(m_dirtext[14])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON15,
																 wlabel15);
								}
								if (nfld>=16)
								{
									mbstowcs(wpath16, m_dir[15],strlen(m_dir[15])+1);
									mbstowcs(wlabel16, m_dirtext[15],strlen(m_dirtext[15])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON16,
																 wlabel16);
								}
								if (nfld==17)
								{
									mbstowcs(wpath17, m_dir[16],strlen(m_dir[16])+1);
									mbstowcs(wlabel17, m_dirtext[16],strlen(m_dirtext[16])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON17,
																 wlabel17);
								}
								if (nfld==18)
								{
									mbstowcs(wpath18, m_dir[17],strlen(m_dir[17])+1);
									mbstowcs(wlabel18, m_dirtext[17],strlen(m_dirtext[17])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON18,
																 wlabel18);
								}
								if (nfld==19)
								{
									mbstowcs(wpath19, m_dir[18],strlen(m_dir[18])+1);
									mbstowcs(wlabel19, m_dirtext[18],strlen(m_dirtext[18])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON19,
																 wlabel19);
								}
								if (nfld==20)
								{
									mbstowcs(wpath20, m_dir[19],strlen(m_dir[19])+1);
									mbstowcs(wlabel20, m_dirtext[19],strlen(m_dirtext[19])+1);
									hr = pfdc->AddControlItem(CONTROL_RADIOBUTTONLIST,
																 CONTROL_RADIOBUTTON20,
																 wlabel20);
								}
								if (SUCCEEDED(hr))
								{
									ul_break_fname(fnam, dir, name);
/*
.....if the directory match one of the directory group, set it
*/
									if ((nfld>1)&&(fnam[0]!='\0'))
									{
										for (int m=0;m<nfld;m++)
										{
											if (stricmp(m_dir[m], dir)==0)
											{
												hr = pfdc->SetSelectedControlItem(CONTROL_RADIOBUTTONLIST,
																			CONTROL_RADIOBUTTON1+m);					
												break;
											}
										}
									}
									else
									{
										hr = pfdc->SetSelectedControlItem(CONTROL_RADIOBUTTONLIST,
															CONTROL_RADIOBUTTON1);					
										CComPtr<IShellItem> psiFolder;
										hr = SHCreateItemFromParsingName (wpath1, NULL, IID_PPV_ARGS(&psiFolder) );

										if ( SUCCEEDED(hr) )
											pfd->SetFolder ( psiFolder );	
									}
								}
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
            pfde->Release();
		}

		if (SUCCEEDED(hr))
        {
			*nc = 0;
			wchar_t wfnam[UX_MAX_PATH_LEN], wfolder[UX_MAX_PATH_LEN];
			char dir[UX_MAX_PATH_LEN], name[UX_MAX_PATH_LEN];
			CComPtr<IShellItem> psiFolder;
			if (fnam[0] != '\0')
			{
				mbstowcs(wfnam, fnam, strlen(fnam)+1);
				pfd->SetFileName(wfnam);
				ul_break_fname(fnam, dir, name);
				if (dir[0] != '\0')
				{
					mbstowcs(wfolder, dir, strlen(dir)+1);
					hr = SHCreateItemFromParsingName (wfolder, NULL, IID_PPV_ARGS(&psiFolder) );
					if ( SUCCEEDED(hr) )
						pfd->SetFolder ( psiFolder );									
/*
.....reset file name to name only
*/
					mbstowcs(wfnam, name, strlen(name)+1);
					pfd->SetFileName(wfnam);
				}
			}
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
						char *temp = new char[UX_MAX_PATH_LEN];
						hr = WideCharToMultiByte(CP_ACP, 0, pszFilePath, -1, temp, UX_MAX_PATH_LEN, NULL, NULL);
						*nc = strlen(temp);
						strcpy_s(fnam, UX_MAX_PATH_LEN-1, temp);
					}
					psiResult->Release();
				}
            }
            pfd->Unadvise(dwCookie);
        }
        pfd->Release();
    }
    return hr;
}
extern "C" void uw_ntMDFileOpen(char* title, char* ntflt, int nfld, int *ifld, char **ftext,
								char **fdir, char* fnam, int *nc, char *descrip, CWnd *MainWin, int flag)
{
	CustomFileOpen(title, ntflt, nfld, ifld, ftext,
								fdir, fnam, nc, descrip, MainWin, flag);
}
extern "C" void uw_ntMDGetDir(char* title, int nfld, int *ifld, char **ftext,
								char **fdir, char* dnam, int *nc, CWnd *MainWin)
{
	CustomFileOpenDir(title, nfld, ifld, ftext,
								fdir, dnam, nc, MainWin, 1);
}
