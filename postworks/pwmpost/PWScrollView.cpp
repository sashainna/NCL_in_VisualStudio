/************************************************************************
c
c   FILE NAME: PWScrollView.cpp
c
c	 CONTAINS: 
c		CPWScrollView::CPWScrollView()
c		CPWScrollView::~CPWScrollView()
c		CPWScrollView::SetDlgTemp()
c		CPWScrollView::CreateIndirect()
c		CPWScrollView::CreateDlgView()
c		CPWScrollView::initview()
c		CPWScrollView::OnDraw()
c		CPWScrollView::OnInitialUpdate()
c		CPWScrollView::OnUpdate()
c		CPWScrollView::OnPaint()
c		CPWScrollView::OnChoicePicked()
c		CPWScrollView::Create()
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PWScrollView.cpp , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/05/14 , 14:36:39
c
c**********************************************************************
*/
#include "Pwstdafx.h"
#include "mpost.h"
#include "mpostres.h"
#include "PWScrollView.h"
#include "dialogitem.h"
#include "dialogprompt.h"
#ifndef _UNICODE
#define _UNICODE_SUFFIX
#else
#define _UNICODE_SUFFIX _T("u")
#endif

#ifndef _DEBUG
#define _DEBUG_SUFFIX
#else
#define _DEBUG_SUFFIX _T("d")
#endif

#ifdef _AFXDLL
#define _STATIC_SUFFIX
#else
#define _STATIC_SUFFIX _T("s")
#endif

#define AFX_WNDCLASS(s) _T("Afx") _T(s) _T("42") _STATIC_SUFFIX _UNICODE_SUFFIX _DEBUG_SUFFIX

#define AFX_WNDCONTROLBAR   AFX_WNDCLASS("ControlBar")

BOOL AFXAPI AfxEndDeferRegisterClass(LONG fToRegister);

#define AfxDeferRegisterClass(fClass) \
	((afxRegisteredClasses & fClass) ? TRUE : AfxEndDeferRegisterClass(fClass))

#define AFX_WNDCOMMCTLS_REG     (0x0010)
#define AFX_WNDCOMMCTLSNEW_REG          0x3C000 

const TCHAR _afxWndControlBar[] = AFX_WNDCONTROLBAR;

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern char Pw_promptdlg_font[];

/////////////////////////////////////////////////////////////////////////////
// CPWScrollView

IMPLEMENT_DYNCREATE(CPWScrollView, CScrollView)

/***********************************************************************
c
c   SUBROUTINE: CPWScrollView()	
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPWScrollView::CPWScrollView()
{
	m_create = 0;
}

/***********************************************************************
c
c   SUBROUTINE:  ~CPWScrollView()
c   FUNCTION:  Deconstructor
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPWScrollView::~CPWScrollView()
{
}
/***********************************************************************
c
c   FUNCTION: OnUpdate(CView*, LPARAM, CObject*)
c
c          Called by the framework after the view's document 
c			has been modified; this function is 
c			called by CDocument::UpdateAllViews
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPWScrollView::OnUpdate(CView*, LPARAM, CObject*)
{
	CRect rectTemplate;
	GetWindowRect(rectTemplate);
	SetScrollSizes(MM_TEXT, rectTemplate.Size());
}



BEGIN_MESSAGE_MAP(CPWScrollView, CScrollView)
	//{{AFX_MSG_MAP(CPWScrollView)
		ON_WM_PAINT()
		ON_CONTROL_RANGE(CBN_SELCHANGE, ID_CHOICE0, ID_CHOICE39, OnChoicePicked)		// NOTE - the ClassWizard will add and remove mapping macros here.
		ON_CONTROL_RANGE(EN_SETFOCUS, ID_PRMP0, ID_PRMP39, FormUserCallbacks1)
		ON_CONTROL_RANGE(EN_KILLFOCUS, ID_PRMP0, ID_PRMP39, FormUserCallbacks2)
		ON_CONTROL_RANGE(CBN_KILLFOCUS, ID_CHOICE0, ID_CHOICE39, FormUserCallbacks4)
		ON_CONTROL_RANGE(CBN_SETFOCUS, ID_CHOICE0, ID_CHOICE39, FormUserCallbacks3)
		ON_COMMAND(ID_FORM_TABED, OnFormTabbed)
		ON_COMMAND(ID_FORM_STABED, OnFormSTabbed)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
/***********************************************************************
c
c   FUNCTION: OnChoicePicked(UINT id)
c
c          Callback function for choice button (pop down)
c
c   INPUT:  id: choice button id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPWScrollView::OnChoicePicked(UINT id)
{
	((CDialogPrompt*)m_parent)->PostMessage(CBN_VIEWSELCHANGE, (WPARAM)id);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks3(UINT id)
c
c          Callback function for comb-box control set focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPWScrollView::FormUserCallbacks3(UINT id)
{
	if (m_create==0)
		return;
	((CDialogPrompt*)m_parent)->PostMessage(CBN_VIEWSETFOCUS, (WPARAM)id);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks1(UINT id)
c
c          Callback function for edit control set focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPWScrollView::FormUserCallbacks1(UINT id)
{
	if (m_create==0)
		return;
	((CDialogPrompt*)m_parent)->PostMessage(EN_VIEWSETFOCUS, (WPARAM)id);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks2(UINT id)
c
c          Callback function for edit-box control lose focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPWScrollView::FormUserCallbacks2(UINT id)
{
	if (m_create==0)
		return;
	((CDialogPrompt*)m_parent)->SendMessage(EN_VIEWKILLFOCUS, (WPARAM)id);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks4(UINT id)
c
c          Callback function for combo-box control lose focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPWScrollView::FormUserCallbacks4(UINT id)
{
	if (m_create==0)
		return;
	((CDialogPrompt*)m_parent)->SendMessage(CBN_VIEWKILLFOCUS, (WPARAM)id);
}

/***********************************************************************
c
c   FUNCTION: OnInitialUpdate()
c
c       Called by the framework before the view is initially displayed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPWScrollView::OnInitialUpdate()
{
	CScrollView::OnInitialUpdate();

	CRect rectTemplate;
	GetWindowRect(rectTemplate);
	SetScrollSizes(MM_TEXT, rectTemplate.Size());
}

/***********************************************************************
**
**   FUNCTION: OnDraw(CDC* pDC)
**
**       The framework calls this function to perform 
**		 view display, and it passes a different device 
**		 context in each case. Not drawing any thing now
**
**   INPUT:  CDC* pDC: device context
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CPWScrollView::OnDraw(CDC* pDC)
{
	CDocument* pDoc = GetDocument();
}

#define UU_BYTEPW	4

char* uu_move_byte(register char * from_ptr, register char * to_ptr, 
				   register int length)
{
	char	*ptr1;

	ptr1 = to_ptr;
	if ( !((int)to_ptr & (UU_BYTEPW - 1)) &&
	     !((int)from_ptr & (UU_BYTEPW - 1)) )
	{
		while (length >= UU_BYTEPW)				/* whole words left? */
		{
			*(int *)to_ptr = *(int *)from_ptr;	/* move a word */
			to_ptr += UU_BYTEPW;
			from_ptr += UU_BYTEPW;
			length -= UU_BYTEPW;
		}
	}
	while (length)							/* move any bytes remaining or unaligned */
	{
		*to_ptr++ = *from_ptr++;		/* move a byte */
		length--;
	}
	return(ptr1);
}

/***********************************************************************
**
**   FUNCTION: SetDlgTemp(DLGTEMPLATE *dlgTempl,  CDialogItem dlgItem[150])
**
**       Set dialog template member value
**
**   INPUT:  dlgTemp:
**			dlgItem:
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CPWScrollView::SetDlgTemp(DLGTEMPLATE *dlgTempl,  CDialogItem dlgItem[150])
{
	uu_move_byte((char*)dlgTempl, (char*)(&m_dlgTempl), 
								sizeof(DLGTEMPLATE));
	for (int i=0; i<m_dlgTempl.cdit; i++)
	{
		m_rgDlgItem[i] = dlgItem[i];
	}
	return 1;
}

/***********************************************************************
**
**   FUNCTION: Create()
**
**       Create view
**
**   INPUT:  lpszClassName: class name
**			lpszWindowName: window name
**			dwRequestedStyle:
**			rect:pParentWnd
**			pContext:
**			
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
BOOL CPWScrollView::Create(LPCTSTR lpszClassName, LPCTSTR lpszWindowName,
	DWORD dwRequestedStyle, const RECT& rect, CWnd* pParentWnd, UINT nID,
	CCreateContext* pContext)
{
	ASSERT(pParentWnd != NULL);

	m_parent = pParentWnd;
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
	AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);


	CREATESTRUCT cs; memset(&cs, 0, sizeof(CREATESTRUCT));
	if (dwRequestedStyle == 0)
		dwRequestedStyle = AFX_WS_DEFAULT_VIEW;
	cs.style = dwRequestedStyle;
	if (!PreCreateWindow(cs))
		return FALSE;

	BOOL bSuccess = CreateDlgView(pParentWnd);
	if (bSuccess==1)
		m_create = 1;
	else
		return bSuccess;
	CRect rectTemplate;
	GetWindowRect(rectTemplate);
	SetScrollSizes(MM_TEXT, rectTemplate.Size());
	
	if (dwRequestedStyle & WS_VISIBLE)
		ShowWindow(SW_NORMAL);

	return TRUE;
}



/////////////////////////////////////////////////////////////////////////////
// CPWScrollView diagnostics

#ifdef _DEBUG
void CPWScrollView::AssertValid() const
{
	CScrollView::AssertValid();
}

void CPWScrollView::Dump(CDumpContext& dc) const
{
	CScrollView::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CPWScrollView message handlers

/***********************************************************************
**
**   FUNCTION: CreateIndirect(CWnd* pParentWnd, DLGTEMPLATE *dlgTempl, 
**					CDialogItem rgDlgItem[20], int itemnum)
**
**       This function Create Dynamic Template Dialog
**
**   INPUT:  pParentWnd: parent window
**				dlgTempl: dialog template
**				rgDlgItem: CDialogItem class array
**				itemnum: total field in the dialog
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CPWScrollView::CreateIndirect(CWnd* pParentWnd, DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20], int itemnum)
{
	int		nChars, nActualChars, nfChars;
	WCHAR *szFontName;
	nChars = strlen(Pw_promptdlg_font) + 1;
	szFontName = new WCHAR[nChars];
	nfChars = MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, Pw_promptdlg_font, nChars, 
							szFontName, nChars);
/*
......We will first convert the control captions to UNICODE
*/
	int		nTotalLength = 0;  
	int		i;

/*
...... catch memory exceptions and don't worry about allocation failures
*/
	TRY  
	{
		int nBufferSize =  sizeof(DLGTEMPLATE) + (3*sizeof(WORD));
		nBufferSize += sizeof(WORD);
		nBufferSize += nfChars * sizeof(WCHAR);
		nBufferSize = (nBufferSize + 3) & ~3; 

		for (i = 0; i < itemnum; i++)
		{
			int nItemLength = sizeof(DLGITEMTEMPLATE) + 3 * sizeof(WORD);
			nItemLength += (rgDlgItem[i].m_strCaption.GetLength() + 1)
												 * sizeof(WCHAR);

			if (i != itemnum -1 )  
/*
...... take into account gap so next control is DWORD aligned
...... but the last control does not need extra bytes
*/
				nItemLength = (nItemLength + 3) & ~3;  

			nBufferSize += nItemLength;
		}

		HLOCAL hLocal = LocalAlloc(LHND, nBufferSize);
		if (hLocal == NULL)
			AfxThrowMemoryException();

		BYTE*	pBuffer = (BYTE*)LocalLock(hLocal);
		if (pBuffer == NULL)
		{
			LocalFree(hLocal);
			AfxThrowMemoryException();
		}

		BYTE*	pdest = pBuffer;
/*
......transfer DLGTEMPLATE structure to the buffer
*/
		memcpy(pdest, dlgTempl, sizeof(DLGTEMPLATE));
		pdest += sizeof(DLGTEMPLATE);
		*(WORD*)pdest = 0; 
/*
.....use default window class
*/
		*(WORD*)(pdest + 1) = 0;  // use default window class
		pdest += 2 * sizeof(WORD);
/*
.....no title
*/
		*(WORD*)pdest = 0; 
		pdest += sizeof(WORD);

		*(WORD*)pdest = 8;//font size
		pdest += sizeof(WORD);
		memcpy(pdest, szFontName, nfChars * sizeof(WCHAR));
		pdest += nfChars * sizeof(WCHAR);
		delete szFontName ;
/* 
......We will now transfer the information for each one of the item templates
*/
		for (i = 0; i < itemnum; i++)
		{
			pdest = (BYTE*)(((DWORD)pdest + 3) & ~3);  
			memcpy(pdest, (void *)&rgDlgItem[i].m_dlgItemTemplate, 
										sizeof(DLGITEMTEMPLATE));
			pdest += sizeof(DLGITEMTEMPLATE);
			*(WORD*)pdest = 0xFFFF; 
			pdest += sizeof(WORD);
			*(WORD*)pdest = rgDlgItem[i].m_controltype;
			pdest += sizeof(WORD);

			WCHAR*	pchCaption;
			int		nChars, nActualChars;
/*
......transfer the caption even when it is an empty string
*/
			nChars = rgDlgItem[i].m_strCaption.GetLength() + 1;
			pchCaption = new WCHAR[nChars];
			nActualChars = MultiByteToWideChar(CP_ACP, 0, 
						rgDlgItem[i].m_strCaption, -1, pchCaption, nChars);
			ASSERT(nActualChars > 0);
			memcpy(pdest, pchCaption, nActualChars * sizeof(WCHAR));
			pdest += nActualChars * sizeof(WCHAR);
			delete pchCaption;

			*(WORD*)pdest = 0;  // How many bytes in data for control
			pdest += sizeof(WORD);
		}
		ASSERT(pdest - pBuffer == nBufferSize); 
		HINSTANCE hInst = AfxGetInstanceHandle();
		DLGTEMPLATE* temp = (DLGTEMPLATE*)pBuffer;
		BOOL bSuccess = CreateDlgIndirect((DLGTEMPLATE*)pBuffer, pParentWnd, hInst);
		LocalUnlock(hLocal);
		LocalFree(hLocal);
	}
	CATCH(CMemoryException, e)
	{
		MessageBox("Memory allocation for dialog template failed.  Demo aborted!",
			"Allocation Failure", MB_ICONEXCLAMATION | MB_OK);
	}
	END_CATCH
	return TRUE;
}



/***********************************************************************
**
**   FUNCTION: OnPaint
**
**       The framework calls this function to perform 
**		 view display
**
**   INPUT: none
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CPWScrollView::OnPaint()
{
	CPaintDC dc(this);
	OnPrepareDC(&dc);
	OnDraw(&dc);
}
/***********************************************************************
**
**   FUNCTION:CreateDlgView(CWnd *parent)
**
**       Create view
**
**   INPUT:  parent: parent window		
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
BOOL CPWScrollView::CreateDlgView(CWnd *parent)
{
	int itemnum = m_dlgTempl.cdit;
	BOOL bSuccess = CreateIndirect(parent, &m_dlgTempl, m_rgDlgItem, itemnum);
	return bSuccess;
}

/***********************************************************************
**
**   FUNCTION:initview(NpwDynWinStruct DynWinStruct)
**
**       initialize view using DynWinStruct
**
**   INPUT:  DynWinStruct: structure include initalize value	
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
BOOL CPWScrollView::initview(NpwDynWinStruct DynWinStruct)
{
	int j, k, id, defsel;
/*
.....j, k for a set of of prompt number
*/
	for( j=0;j<DynWinStruct.numButtons;j++)
	{
		switch (DynWinStruct.Buttype[j])
		{
			case NpwEChoiceB:
			case NpwEDynChoiceB:
			{
				id = ID_CHOICE0 + j;
				for(k=0; k < DynWinStruct.Butnumchoice[j]; k++)
				{
					((CComboBox*)GetDlgItem(id))->AddString(
					DynWinStruct.Butchoicestring[j][k]);
					if (strcmp(DynWinStruct.Butdefault[j], 
								DynWinStruct.Butchoicestring[j][k])==0)
						defsel = k;
				}
				((CComboBox*)GetDlgItem(id))->SetCurSel(defsel); 
				break;
			}
			case NpwETextB:
			{
				if (j==0)
				{
					id = ID_PRMP0;			
					GetDlgItem(id)->SetFocus();
				}
				break;
			}
		}
	}
// return true only if we set the item focus, otherwise, use default, set the focus to first item
	
	return true;
//	return 0;
}
/**********************************************************************
**    E_FUNCTION :  OnFormTabbed()
**       Handle tab event of form.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void CPWScrollView::OnFormTabbed()
{
	if (m_create==0)
		return;
	((CDialogPrompt*)m_parent)->SendMessage(WM_COMMAND, ID_FORM_TABED);
}

/**********************************************************************
**    E_FUNCTION :  OnFormSTabbed()
**       Handle tab event of form.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT : 
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void CPWScrollView::OnFormSTabbed()
{
	if (m_create==0)
		return;
	((CDialogPrompt*)m_parent)->SendMessage(WM_COMMAND, ID_FORM_STABED);
}
