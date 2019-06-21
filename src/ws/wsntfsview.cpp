/************************************************************************
c
c   FILE NAME: wsntfsview.cpp
c
c	 CONTAINS: 
c		CNCLFormScrollView::CNCLFormScrollView()
c		CNCLFormScrollView::~CNCLFormScrollView()
c		CNCLFormScrollView::SetDlgTemp()
c		CNCLFormScrollView::CreateIndirect()
c		CNCLFormScrollView::CreateDlgView()
c		CNCLFormScrollView::initview()
c		CNCLFormScrollView::OnDraw()
c		CNCLFormScrollView::OnInitialUpdate()
c		CNCLFormScrollView::OnUpdate()
c		CNCLFormScrollView::OnPaint()
c		CNCLFormScrollView::Create()
c		CNCLFormScrollView::FormUserCallbacks1()
c		CNCLFormScrollView::FormUserCallbacks2()
c		CNCLFormScrollView::FormUserCallbacks3()
c		CNCLFormScrollView::FormUserCallbacks4()
c		CNCLFormScrollView::FormUserCallbacks5()
c		CNCLFormScrollView::FormUserCallbacks6()
c		CNCLFormScrollView::FormUserCallbacks7()
c		CNCLFormScrollView::FormUserCallbacks8()
c		CNCLFormScrollView::FormUserCallbacks14()
c		CNCLFormScrollView::OnMouseActivate()
c
c     COPYRIGHT 2005 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        wsntfsview.cpp , 26.3
c     DATE AND TIME OF LAST  MODIFICATION
c        04/16/18 , 15:37:41
c
c**********************************************************************
*/
#include "wsntstdafx.h"
#include "wsntctl.h"
#include "wsntframe.h"
#include "wsntres.h"
#include "wsgl.h"
#include "wsntfsview.h"
#include "wsntdlgitem.h"
#include "wsntform.h"
#include "wsntsecbtn.h"
#include "wsntgettxt2.h"
#include "wsntsliderctrl.h"
#include "wsntbitmap.h"

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
char UW_formdlg_font[] = "MS Sans Serif";
extern CMainFrame *NCL_MainFrame;
extern "C" char UW_form_font[20];
extern "C" int UW_form_fontsize;
#define SEP_WID 3
extern "C" void ud_free_itemdata(UD_ITEMDATA *data);
extern "C" int uw_ntget_strsize(char *string, int pt, char *fntname, int *wid, int* hgt);
/////////////////////////////////////////////////////////////////////////////
// CNCLFormScrollView

IMPLEMENT_DYNCREATE(CNCLFormScrollView, CScrollView)

/***********************************************************************
c
c   SUBROUTINE: CNCLFormScrollView()	
c
c   FUNCTION:  constructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLFormScrollView::CNCLFormScrollView()
{
	m_create = 0;
	for (int i=0; i<20; i++)
	{
		m_picture[i] = NULL;
	}
/*
......scroll view, no select buttons
*/
	m_dlgtyp = 1;
	m_secno = 0;
	m_selsec = -1;
//	strcpy(UW_formdlg_font, UW_form_font);
	strcpy(UW_formdlg_font, "MS Sans Serif");
}

/***********************************************************************
c
c   SUBROUTINE:  ~CNCLFormScrollView()
c   FUNCTION:  Deconstructor
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNCLFormScrollView::~CNCLFormScrollView()
{
	m_create = 0;
	for (int i=0; i<20; i++)
	{
		if (m_picture[i] != NULL)
			delete m_picture[i];
	}
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
void CNCLFormScrollView::OnUpdate(CView*, LPARAM, CObject*)
{
	CRect rectTemplate, rectTemplate1;
	GetWindowRect(rectTemplate);

	rectTemplate1 = rectTemplate;
	rectTemplate1.bottom = rectTemplate1.top+20;
	rectTemplate1.right = rectTemplate1.left+20;

//	if (m_dlgtyp)
//		SetScrollSizes(MM_TEXT, rectTemplate.Size());
//	else
		SetScrollSizes(MM_TEXT, rectTemplate1.Size());
}



BEGIN_MESSAGE_MAP(CNCLFormScrollView, CScrollView)
	//{{AFX_MSG_MAP(CNCLFormScrollView)
		ON_WM_PAINT()
		ON_WM_MOUSEMOVE()
		ON_WM_MOUSEACTIVATE()
		ON_WM_CTLCOLOR()
		ON_WM_VSCROLL()
		ON_WM_HSCROLL()
		ON_COMMAND(ID_FORM_TABED, OnFormTabbed)
		ON_COMMAND(ID_FORM_STABED, OnFormSTabbed)
		ON_COMMAND_RANGE(ID_FORM_HOTKEY1, ID_FORM_HOTKEY4, OnAccelFunctions)
		ON_COMMAND(IDF_FORM_RETURN, FormUserCallbacks0)
			
		ON_COMMAND_RANGE(IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks1)

		ON_COMMAND(ID_FILTER_TLIST, FormUserCallbacks0)

		ON_COMMAND_RANGE(IDC_FORMSECTION1, IDC_FORMSECTION_LAST, FormUserCallbacks14)
		ON_CONTROL_RANGE(CBN_SELCHANGE, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks2)
		ON_CONTROL_RANGE(CBN_DBLCLK, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCBLDblclk)
		ON_CONTROL_RANGE(LBN_SELCHANGE, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks3)
		ON_CONTROL_RANGE(EN_SETFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks4)
		ON_CONTROL_RANGE(EN_KILLFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks5)
		ON_CONTROL_RANGE(CBN_KILLFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks9)
		ON_CONTROL_RANGE(BN_SETFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks6)
		ON_CONTROL_RANGE(LBN_SETFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks8)
		ON_CONTROL_RANGE(CBN_SETFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks7)
		ON_NOTIFY_RANGE(NM_SETFOCUS, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks12)
		ON_NOTIFY_RANGE(LVN_COLUMNCLICK, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks10)
		ON_NOTIFY_RANGE(LVN_ITEMCHANGED, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks11)
		ON_NOTIFY_RANGE(NM_CLICK, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks13)
		ON_NOTIFY_RANGE(NM_DBLCLK, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks15)	
		ON_NOTIFY_RANGE(HDN_DROPDOWN, IDC_FORMITEM1, IDC_FORMITEM_LAST, FormUserCallbacks16)
		ON_MESSAGE(WM_UPDATEUISTATE, OnUpdateUIState)
		ON_MESSAGE(ID_LISTITEM_CLICK, OnItemClick)
		ON_MESSAGE(ID_PICARAE_MSG, OnPicAreaClick)
		ON_MESSAGE(ID_FILTER_TLIST, OnFilterTlist)
		//}}AFX_MSG_MAP
END_MESSAGE_MAP()
/***********************************************************************
c
c   FUNCTION: OnPicAreaClick(WPARAM wParam, LPARAM lParam)
c
c          Callback for picture area click
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
LRESULT CNCLFormScrollView::OnPicAreaClick(WPARAM wParam, LPARAM lParam)
{
	UINT fid = (UINT)lParam;
	char *text = (char*)wParam;
	((CNCLForm*)m_parent)->PicAreaClick(fid, text);
	return 0;
}
LRESULT CNCLFormScrollView::OnFilterTlist(WPARAM wParam, LPARAM lParam)
{
	int frmfld = (UINT)lParam;
	((CNCLForm*)m_parent)->OnFilterTlist(frmfld);
	m_listctl[frmfld].OnFilterTlist(NULL, NULL);
	return 0;
}
void CNCLFormScrollView::GetCurrentTlist(int itemnum, UD_TLIST *list)
{
	m_listctl[itemnum].GetCurrentTlist(list);
}
/***********************************************************************
c
c   FUNCTION: OnItemClick(WPARAM wParam, LPARAM lParam)
c
c          Callback for listitem click
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
LRESULT CNCLFormScrollView::OnItemClick(WPARAM wParam, LPARAM lParam)
{
	CNCLListCtrl2 *dlist;
	UINT id = (UINT) lParam;
	UD_TABLEINFO info, *info1 = (UD_TABLEINFO*) wParam;
	CWnd *list = GetDlgItem(id);

	info.flag = 1;
	info.frmid = ((CNCLForm*)m_parent)->m_frmid;
	info.fldid = id;
	info.col = info1->col;
	info.row = info1->row;

	UD_ITEMDATA data;

	if (list->IsKindOf(RUNTIME_CLASS(CNCLListCtrl2)))
	{
		dlist = (CNCLListCtrl2*)list;
		data.flag = info.flag;
		data.fldno = ((CNCLForm*)m_parent)->Get_fldno_from_ID(info.fldid);
		data.frmid = info.frmid;
		dlist->fill_item_data(info.row, &data);
		info.data_ptr = (int*)&data;
		((CNCLForm*)m_parent)->CtrlListCallback(id, &info);
		dlist->m_selitem = info1->row;
		dlist->m_selSitem = info1->col;
		dlist->m_change = 1;
		ud_free_itemdata(&data);
	}
	return 1;
}
/***********************************************************************
c
c   FUNCTION: OnUpdateUIState(WPARAM wParam, LPARAM lParam)
c
c          Called to to change the user interface (UI) state for 
c			the specified window and all its child windows.
c		we write our own OnUpdateUIState to eat up WM_UPDATEUISTATE message
c			in order to showng checkbox focused.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
LRESULT CNCLFormScrollView::OnUpdateUIState(WPARAM wParam, LPARAM lParam)
{
	switch (LOWORD(wParam))
	{
		case UIS_CLEAR:
			OutputDebugString("UIS_CLEAR"); 
			break;
		case UIS_INITIALIZE: 
			OutputDebugString("UIS_INITIALIZE"); 
			break;
		case UIS_SET: 
			OutputDebugString("UIS_SET");
			break;            
	}
	if(HIWORD(wParam) & UISF_HIDEACCEL)             
		OutputDebugString(" UISF_HIDEACCEL");
	if (HIWORD(wParam) & UISF_HIDEFOCUS) 
		OutputDebugString(" UISF_HIDEFOCUS");
	if (HIWORD(wParam) & UISF_ACTIVE) 
		OutputDebugString("UISF_ACTIVE");
	return 1;
} 

/////////////////////////////////////////////////////////////////////////////
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks1(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks1(UINT id)
{
	if (m_create==0)
		return;
/*
.....note: all message handle here should using SendMessage instead of
.....SendMessage (we can all using SendMessage in all callbacks here)
.....in order to follow the correct order of the message
.....otherwise, some message may not get executed (since it may have other window message
.....while execute the form callbacks. But if we all using SendMessage
.....if may have problem because this function will contine within CNCLFormScrollView
.....without execute any form callbacks
*/
	((CNCLForm*)m_parent)->SendMessage(WM_VIEWCOMMAND, (WPARAM)id);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks1(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks14(UINT id)
{
	if (m_create==0)
		return;
	int page = id - IDC_FORMSECTION1;
	((CNCLForm*)m_parent)->OnSecButton(page);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks2(UINT id)
c
c          Callback function for comb-box control select changes
c		for some reason, the ListBox change select call the callback here
c        not the FormUserCallbacks3 as it supposes, also the ListBox set focus
c        not be called when we select listbox, so, we will force it to call setfocus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks2(UINT id)
{
	if (m_create==0)
		return;
/*
.....check if it is listbox, if yes, call listbox callback
.....and force to call setfocus function
*/ 
	int type = ((CNCLForm*)m_parent)->get_idtype(id);
	if (type==5)
	{
		((CNCLForm*)m_parent)->SendMessage(LBN_VIEWSETFOCUS, (WPARAM)id);
		((CNCLForm*)m_parent)->SendMessage(LBN_VIEWSELCHANGE, (WPARAM)id);
	}
	else
		((CNCLForm*)m_parent)->SendMessage(CBN_VIEWSELCHANGE, (WPARAM)id);
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks3(UINT id)
c
c          Callback function for list-box control select changes
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks3(UINT id)
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(LBN_VIEWSELCHANGE, (WPARAM)id);
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks4(UINT id)
c
c          Callback function for edit-box control set focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks4(UINT id)
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(EN_VIEWSETFOCUS, (WPARAM)id);
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks5(UINT id)
c
c          Callback function for edit-box control kill focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks5(UINT id)
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(EN_VIEWKILLFOCUS, (WPARAM)id);
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks6(UINT id)
c
c          Callback function for button control set focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks6(UINT id)
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(BN_VIEWSETFOCUS, (WPARAM)id);
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks7(UINT id)
c
c          Callback function for comb-box control set focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks7(UINT id)
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(CBN_VIEWSETFOCUS, (WPARAM)id);
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks8(UINT id)
c
c          Callback function for list-box control set focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks8(UINT id)
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(LBN_VIEWSETFOCUS, (WPARAM)id);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks9(UINT id)
c
c          Callback function for combo-box control kill focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks9(UINT id)
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(CBN_VIEWKILLFOCUS, (WPARAM)id);
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks12(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
c
c          Callback function for table list set focus
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks12(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->OntlistFocus(id);
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
void CNCLFormScrollView::OnInitialUpdate()
{
	CScrollView::OnInitialUpdate();

	CRect rectTemplate, rectTemplate1;
	GetWindowRect(rectTemplate);

	rectTemplate1 = rectTemplate;
	rectTemplate1.bottom = rectTemplate1.top+20;
	rectTemplate1.right = rectTemplate1.left+20;

//	if (m_dlgtyp)
//		SetScrollSizes(MM_TEXT, rectTemplate.Size());
//	else
		SetScrollSizes(MM_TEXT, rectTemplate1.Size());
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
void CNCLFormScrollView::OnDraw(CDC* pDC)
{
//test 
return;
	if (m_create==0)
		return;
	CView::OnDraw(pDC);
	CDocument* pDoc = GetDocument();
	((CNCLForm*)m_parent)->repaint_pic();
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
int CNCLFormScrollView::SetDlgTemp(DLGTEMPLATE *dlgTempl,  CDialogItem dlgItem[150])
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
BOOL CNCLFormScrollView::Create(LPCTSTR lpszClassName, LPCTSTR lpszWindowName,
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
	m_create = 1;

	BOOL bSuccess = CreateDlgView(pParentWnd);
	if (bSuccess!=1)
		return bSuccess;
	CRect rectTemplate, rectTemplate1;
	GetWindowRect(rectTemplate);

	rectTemplate1 = rectTemplate;
	rectTemplate1.bottom = rectTemplate1.top+20;
	rectTemplate1.right = rectTemplate1.left+20;

//	if (m_dlgtyp)
//		SetScrollSizes(MM_TEXT, rectTemplate.Size());
//	else
		SetScrollSizes(MM_TEXT, rectTemplate1.Size());
	
	if (dwRequestedStyle & WS_VISIBLE)
		ShowWindow(SW_NORMAL);

	return TRUE;
}



/////////////////////////////////////////////////////////////////////////////
// CNCLFormScrollView diagnostics

#ifdef _DEBUG
void CNCLFormScrollView::AssertValid() const
{
	CScrollView::AssertValid();
}

void CNCLFormScrollView::Dump(CDumpContext& dc) const
{
	CScrollView::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CNCLFormScrollView message handlers

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
BOOL CNCLFormScrollView::CreateIndirect(CWnd* pParentWnd, DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20], int itemnum)
{
	int		nChars, nfChars, nActualChars;
	WCHAR *szFontName;
	nChars = strlen(UW_formdlg_font) + 1;
	szFontName = new WCHAR[nChars];
	nfChars = MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, UW_formdlg_font, nChars, 
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
//			int nItemLength = sizeof(DLGITEMTEMPLATE) + 3 * sizeof(WORD);
//			nItemLength += (rgDlgItem[i].m_strCaption.GetLength() + 1)
//												 * sizeof(WCHAR);
			int nItemLength;
			if (m_rgDlgItem[i].m_controltype!=0x1080)
				nItemLength = sizeof(DLGITEMTEMPLATE) + 3 * sizeof(WORD);
			else
				nItemLength = sizeof(DLGITEMTEMPLATE) + 1 * sizeof(WORD)
								+ 18*sizeof(WCHAR);
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

		*(WORD*)pdest = UW_form_fontsize; //font size
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
			if (m_rgDlgItem[i].m_controltype!=0x1080)
			{
				*(WORD*)pdest = 0xFFFF; 
				pdest += sizeof(WORD);
				*(WORD*)pdest = m_rgDlgItem[i].m_controltype;
				pdest += sizeof(WORD);
			}
			else
			{
				WCHAR*	pchClass;
				pchClass = new WCHAR[18];
				nActualChars = MultiByteToWideChar(CP_ACP, 0, 
						"msctls_progress32", -1, pchClass, 18);
				ASSERT(nActualChars > 0);
				memcpy(pdest, pchClass, nActualChars * sizeof(WCHAR));
				pdest += nActualChars * sizeof(WCHAR);
				delete pchClass;
			}

			WCHAR*	pchCaption;
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

			*(WORD*)pdest = 0;
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
void CNCLFormScrollView::OnPaint()
{
	CScrollView::OnPaint();

//test
return;
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
BOOL CNCLFormScrollView::CreateDlgView(CWnd *parent)
{
	int itemnum = m_dlgTempl.cdit;
	BOOL bSuccess = CreateIndirect(parent, &m_dlgTempl, m_rgDlgItem, itemnum);
	return bSuccess;
}
/***********************************************************************
**
**   FUNCTION:OnMouseActivate(CWnd* pDesktopWnd, UINT nHitTest, UINT message)
**
**       The framework calls this member function when the cursor is in an 
**			inactive window and the user presses a mouse button. 
**			this function overwrite CView::OnMouseActivate because we use NULL
**			as parent frame, but in CView::OnMouseActivate function, it will tried
**			to treat main frame as parent window if it is null which we don't want.
**
**   INPUT:  pDesktopWnd:   Specifies a pointer to the top-level parent 
**							window of the window being activated	
**			nHitTest:   Specifies the hit-test area code. A hit test is a 
**						test that determines the location of the cursor.
**			message   Specifies the mouse message number.
**   OUTPUT :   none
**   RETURN:    Specifies whether to activate the CWnd and whether to discard the mouse event
**				MA_ACTIVATE   Activate CWnd object.
**				MA_NOACTIVATE   Do not activate CWnd object.
**				MA_ACTIVATEANDEAT   Activate CWnd object and discard the mouse event.
**				MA_NOACTIVATEANDEAT   Do not activate CWnd object and discard the mouse event. 
**
**********************************************************************/
int CNCLFormScrollView::OnMouseActivate(CWnd* pDesktopWnd, UINT nHitTest, UINT message)
{
	int nResult = CWnd::OnMouseActivate(pDesktopWnd, nHitTest, message);
	if (nResult == MA_NOACTIVATE || nResult == MA_NOACTIVATEANDEAT)
		return nResult; 
	return nResult;
}
/***********************************************************************
c
c   SUBROUTINE:  OnCtlColor() 
c
c   FUNCTION:  This function called when a child control 
c				is about to be drawn. We use override this
c				method to change background color oof a control
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
HBRUSH CNCLFormScrollView::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	HBRUSH brush;
	if (m_create==0)
		return CScrollView::OnCtlColor(pDC, pWnd, nCtlColor);
	switch (nCtlColor)
	{
	case CTLCOLOR_BTN:
	case CTLCOLOR_EDIT:
	case CTLCOLOR_LISTBOX:
	case CTLCOLOR_MSGBOX:
	case CTLCOLOR_STATIC: 
		brush = ((CNCLForm*)m_parent)->Onctlcolor2(pDC, pWnd, nCtlColor);
		if (brush!=NULL)
			return brush;
		else
			return CScrollView::OnCtlColor(pDC, pWnd, nCtlColor);
	default:
		return CScrollView::OnCtlColor(pDC, pWnd, nCtlColor);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  Recreate_button() 
c
c   FUNCTION:  This function create a button using input caption, ID and attributes
c
c   INPUT:  i: button index
c			lpszCaption: button caption
c			dwStyle: button style
c			rect: button size
c			nID: button ID
c			bcolor: button background color
c			fcolor: button foreground color
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormScrollView::Recreate_button(int i, LPCTSTR lpszCaption, DWORD dwStyle, 
										 RECT& rect, UINT nID, COLORREF bcolor, COLORREF fcolor)
{
	CFont aFont;
	ScreenToClient(&rect);
	m_button[i].Create(lpszCaption, dwStyle, rect, this, nID);
	m_button[i].set_color(bcolor, fcolor);
	m_button[i].ShowWindow(SW_SHOW);
}

void CNCLFormScrollView::Recreate_imgbutton(int i, char* imgfile, DWORD dwStyle, RECT& rect, UINT nID,
		COLORREF bcolor)
{
	ScreenToClient(&rect);
	m_button4[i].SetBitMapFile(imgfile);
	m_button4[i].Create(" ", WS_VISIBLE | WS_CHILD | BS_BITMAP, rect, this, nID);
	m_button4[i].ShowWindow(SW_SHOW);
}

/***********************************************************************
c
c   SUBROUTINE:  Recreate_button2() 
c
c   FUNCTION:  This function create a button using input caption, ID and attributes
c
c   INPUT:  i: button index
c			lpszCaption: button caption
c			dwStyle: button style
c			rect: button size
c			nID: button ID
c			bcolor: button background color
c			fcolor: button foreground color
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormScrollView::Recreate_button2(int i, LPCTSTR lpszCaption, DWORD dwStyle, 
										 RECT& rect, UINT nID, COLORREF bcolor, COLORREF fcolor)
{
	CFont aFont;
	ScreenToClient(&rect);
	m_button2[i].Create(lpszCaption, dwStyle, rect, this, nID);
	m_button2[i].set_color(bcolor, fcolor);
	m_button2[i].ShowWindow(SW_SHOW);
}

void CNCLFormScrollView::Recreate_button3(int i, LPCTSTR lpszCaption, DWORD dwStyle, 
										 RECT& rect, UINT nID, COLORREF bcolor, COLORREF fcolor, int type)
{
	CFont aFont;
	ScreenToClient(&rect);
	m_button3[i].SetType(type);
	m_button3[i].set_color(bcolor, fcolor);
	m_button3[i].Create(lpszCaption, dwStyle, rect, this, nID);
	m_button3[i].ShowWindow(SW_SHOW);
}

void CNCLFormScrollView::Create_Slider(int i, UINT fid, DWORD dwStyle, RECT& rbtn, 
		int range1, int range2, int init_value, UINT budid, int vert)
{
	ScreenToClient(&rbtn);
/*
	if (vert==0)
	{
		if (rbtn.bottom-rbtn.top>20)
			rbtn.bottom = rbtn.top + 20;
	}
	else
	{
		if (rbtn.right-rbtn.left>20)
			rbtn.right = rbtn.left + 20;
	}
*/
	m_slider[i].Create(dwStyle, rbtn, this, fid);
	m_slider[i].SetParent(this);
	m_slider[i].SetBuddy(GetDlgItem(budid));
	m_slider[i].SetRange(range1, range2);
	m_slider[i].SetPos(init_value);
	m_slider[i].ShowWindow(SW_SHOW);
}
void CNCLFormScrollView::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	int i, fldno;
	UINT fid;
	CWnd *wnd;
	if (pScrollBar==NULL)
	{
//		return CScrollView::OnVScroll(nSBCode, nPos, pScrollBar);

		int xOrig = GetScrollPos(SB_HORZ);
		int yOrig = GetScrollPos(SB_VERT);
		CScrollView::OnVScroll(nSBCode, nPos, pScrollBar);
//		((CNCLForm*)m_parent)->ReShowFrame();
return;
//need force redraw
/*		GetScroll_xy(MAKEWORD(0xff, nSBCode), nPos, &x, &y);
		int x = GetScrollPos(SB_HORZ);
		int y = GetScrollPos(SB_VERT);
		CRect rect;
		if ((y-yOrig)>0)
		{
			((CNCLForm*)m_parent)->GetWindowRect(&rect);
			rect.right += 1;
			((CNCLForm*)m_parent)->MoveWindow(&rect);
		}
		return;
*/
	}
	if ((pScrollBar->IsKindOf(RUNTIME_CLASS(CNCLSliderCtrl)))==0)
		return CScrollView::OnVScroll(nSBCode, nPos, pScrollBar);

	CNCLSliderCtrl* pSlider = (CNCLSliderCtrl*)pScrollBar;		
	pSlider->ReflectedScrollMessage();
	CScrollView::OnVScroll(nSBCode, nPos, pScrollBar);
	((CNCLForm*)m_parent)->HandleVScroll(nSBCode, nPos, pScrollBar);
}
void CNCLFormScrollView::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar) 
{
	int i, fldno;
	UINT fid;
	CWnd *wnd;
	if (pScrollBar==NULL)
		return CScrollView::OnHScroll(nSBCode, nPos, pScrollBar);
	if ((pScrollBar->IsKindOf(RUNTIME_CLASS(CNCLSliderCtrl)))==0)
		return CScrollView::OnHScroll(nSBCode, nPos, pScrollBar);

	CNCLSliderCtrl* pSlider = (CNCLSliderCtrl*)pScrollBar;		
	pSlider->ReflectedScrollMessage();
	CScrollView::OnVScroll(nSBCode, nPos, pScrollBar);
	((CNCLForm*)m_parent)->HandleHScroll(nSBCode, nPos, pScrollBar);
}

void CNCLFormScrollView::create_secbutton(char *name, int indx, int page, UINT fid, COLORREF &color, int type)
{
	int sep = 0;
	CRect rect, rect_b;
	GetDlgItem(IDC_DLG_BOX1)->GetWindowRect(&rect_b);
	GetDlgItem(IDC_DLG_BOX2)->GetWindowRect(&rect);
	ScreenToClient(rect_b);
	rect.left = rect_b.left+2;
	rect.right = rect_b.right-4;
		
	int chrwid, phgt;
	uw_ntget_strsize("X", UW_form_fontsize*10, UW_formdlg_font, &chrwid, &phgt);

	sep = indx - page;
		
	if (type==3)
	{
		rect.top = rect_b.top+ (phgt+2) + page * (phgt+15+2) + sep*SEP_WID;
		rect.bottom = rect.top + SEP_WID;
	}
	else
	{
		rect.top = rect_b.top+ (phgt+2) + page * (phgt+15+2) + sep*SEP_WID;
		rect.bottom = rect.top + (phgt+15);
	}
	m_secbut[indx].SetParent(this);
	m_secbut[indx].SetType(type);
	m_secbut[indx].set_itemnum(indx);
	if (type==3)
		m_secbut[indx].SetPageNum(-1);
	else
		m_secbut[indx].SetPageNum(page);
	if (type!=3)
	{	
		m_secbut[indx].SetText(name);
		m_secbut[indx].Create(name, WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT | WS_TABSTOP | BS_OWNERDRAW, 
						rect, this, fid);
		m_secbut[indx].SetButID(fid);
		m_secbut[indx].SetTColor(color);
	}
	else
	{
		m_secbut[indx].Create("", WS_VISIBLE | WS_CHILD | BS_PUSHBUTTON | BS_LEFT | WS_TABSTOP | BS_OWNERDRAW, 
						rect, this, -1);
	}
	m_secbut[indx].ShowWindow(SW_SHOW);
}


int CNCLFormScrollView::GetInputText(CString &input_text, CString prompt_text, int size[2])
{
	CNCLGetText2 *dlg = new CNCLGetText2(this);
	dlg->SetTextString(input_text.GetBuffer());
	dlg->SetPromptString(prompt_text.GetBuffer());
	CPoint pt;
	GetCursorPos(&pt);
	dlg->SetPos(pt, size);
	dlg->DoModal();
	CString text;
	dlg->GetTextString(input_text);
	delete dlg;
	return 0;
}

void CNCLFormScrollView::OnItemMouseMove(int itemnum)
{
/*
.....highlight this section item if not highlight
*/
	for(int i = 0; i < m_secno; i++ )
	{
		if (i==itemnum)
		{
			if (m_secbut[i].m_focus==0)
			{
				m_secbut[i].DrawHighLight2();
				m_secbut[i].m_focus = 1;
			}
		}
		else
		{
			if (m_secbut[i].m_focus==1)
			{
				m_secbut[i].DrawNormal2();
				m_secbut[i].m_focus = 0;
				m_secbut[i].CloseToolTip();
			}
		}
	}
}

/***********************************************************************
**
**   FUNCTION: OnMouseMove(UINT nFlags, CPoint pt)
**
**       mouse move callback
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLFormScrollView::OnMouseMove(UINT nFlags, CPoint point) 
{	
	CScrollView::OnMouseMove(nFlags, point);
	CPoint wpt;
/*
.....check if the mouse is inside the button
*/
	for(int i = 0; i < m_secno; i++ )
	{
		CRect rect;
		m_secbut[i].GetWindowRect(rect);
		ScreenToClient(&rect);
		if(rect.PtInRect(point))
		{
			if (m_secbut[i].m_focus==0)
			{
				if (m_secbut[i].m_disabled==0)
				{
					m_secbut[i].DrawHighLight2();
					m_secbut[i].m_focus = 1;
				}
				else
				{
					m_secbut[i].DrawNormal2();
					m_secbut[i].m_focus = 0;
				}
				wpt = point;
				ClientToScreen(&wpt);
				m_secbut[i].ShowToolTip(wpt);
			}
		}
		else
		{
			if (m_secbut[i].m_focus==1)
			{
				m_secbut[i].DrawNormal2();
				m_secbut[i].m_focus = 0;
				m_secbut[i].CloseToolTip();
			}
		}
	}
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
void CNCLFormScrollView::OnFormTabbed()
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(WM_COMMAND, ID_FORM_TABED);
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
void CNCLFormScrollView::OnFormSTabbed()
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(WM_COMMAND, ID_FORM_STABED);
}

/**********************************************************************
**    I_FUNCTION :  OnAccelFunctions(UINT id)
**		Callback function for hotkey for CAM_SCALAR
**
**    PARAMETERS   
**       INPUT  : 
**          UINT id: Accelerator ID
**			
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLFormScrollView::OnAccelFunctions(UINT id)
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(WM_COMMAND, id);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks0()
c
c          Callback function for return
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks0()
{
	if (m_create==0)
		return;
	((CNCLForm*)m_parent)->SendMessage(WM_COMMAND, IDF_FORM_RETURN);
}
/***********************************************************************
c
c   SUBROUTINE:  Recreate_listctl() 
c
c   FUNCTION:  This function create a listctl using input list struction
c
c   INPUT:  i: list index
c			listptr: list struction
c			dwStyle: window style
c			rect: list size
c			nID: list ID
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormScrollView::Recreate_listctl(int i, char *listptr, DWORD dwStyle,
										 RECT& rect, UINT nID)
{
	ScreenToClient(&rect);
	m_listctl[i].SetParent(this);
	m_listctl[i].SetFrmFld(((CNCLForm*)m_parent)->m_frmid, i);
	m_listctl[i].Create(dwStyle, rect, this, nID);
	m_listctl[i].ShowWindow(SW_SHOW);
}

/***********************************************************************
c
c   SUBROUTINE:  Recreate_listctl2() 
c
c   FUNCTION:  This function create a listctl using input list struction
c
c   INPUT:  i: list index
c			listptr: list struction
c			dwStyle: window style
c			rect: list size
c			nID: list ID
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormScrollView::Recreate_listctl2(int i, char *listptr, DWORD dwStyle,
										 RECT& rect, UINT nID)
{
	ScreenToClient(&rect);
	m_listctl2[i].Create(dwStyle, rect, this, nID);
	m_listctl2[i].SetParent_id(this, nID);
	m_listctl2[i].ShowWindow(SW_SHOW);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks10(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks10(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
{
	if (m_create==0)
		return;
	NMLISTVIEW *pLV = (NMLISTVIEW *) pNMHDR;	

	UD_TABLEINFO info;
	info.flag = 2;
	info.frmid = ((CNCLForm*)m_parent)->m_frmid;
	info.fldid = id;
	info.col = pLV->iSubItem;
	info.row = -1;
	info.data_ptr = NULL;
	((CNCLForm*)m_parent)->CtrlListCallback(id, &info);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks11(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks11(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
{
	*pResult = 0;
	if (m_create==0)
		return;
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	CWnd *list;
	CNCLListCtrl *tlist;
	CNCLListCtrl2 *dlist;
	list = GetDlgItem(id);
	if (list->IsKindOf(RUNTIME_CLASS(CNCLListCtrl)))
	{
		if (((CNCLListCtrl*)list)->m_update==0)
			return;
	}
	if ((pNMListView->uNewState & LVIS_SELECTED )&&( pNMListView->uOldState==0))
	{
		int selectedItem = pNMListView->iItem;
		UD_TABLEINFO info;
		info.flag = 1;
		info.frmid = ((CNCLForm*)m_parent)->m_frmid;
		info.fldid = id;
		info.col = pNMListView->iSubItem;
		info.row = selectedItem;
		if (list->IsKindOf(RUNTIME_CLASS(CNCLListCtrl)))
		{
			tlist = (CNCLListCtrl*)list;
			info.data_ptr = (int*)tlist->GetItemData(info.row);
			tlist->m_selitem = pNMListView->iItem;
			((CNCLForm*)m_parent)->CtrlListCallback(id, &info);
			tlist->m_selitem = pNMListView->iItem;
			tlist->m_change = 1;
		}
/*		else if (list->IsKindOf(RUNTIME_CLASS(CNCLListCtrl2)))
		{

			dlist = (CNCLListCtrl2*)list;
			info.data_ptr = (int*)dlist->GetItemData(info.row);
UD_ITEMDATA *data = (UD_ITEMDATA *)(info.data_ptr);

return;
			((CNCLForm*)m_parent)->CtrlListCallback(id, &info);
			dlist->m_selitem = pNMListView->iItem;
			dlist->m_change = 1;
		}
*/		else
			return;
	}
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks13(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks13(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
{
	*pResult = 0;
	if (m_create==0)
		return;
/*
.....there is no unselect for CNCLListCtrl
*/
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	CNCLListCtrl *list = (CNCLListCtrl*)GetDlgItem(id);
/*
	if (  ( pNMListView->uOldState & LVIS_SELECTED ) && 
         ( pNMListView->uNewState == 0 )  )
    {
		if ((list->m_selitem==pNMListView->iItem)&&(list->m_change==0))
		{
			list->SetItemState(pNMListView->iItem, 0, LVIS_SELECTED | LVIS_FOCUSED);	
			list->m_selitem = -1;
			UD_TABLEINFO info;
			info.flag = 1;
			info.frmid = ((CNCLForm*)m_parent)->m_frmid;
			info.fldid = id;
			info.col = -1;
			info.row = -1;
			info.data_ptr = NULL;
			((CNCLForm*)m_parent)->CtrlListCallback(id, &info);
			*pResult = 1;
			return;
		}
	}
	list->m_change = 0;
	list->m_selitem = pNMListView->iItem;
*/
	CNCLListCtrl *tlist;
	int selectedItem = pNMListView->iItem;
	UD_TABLEINFO info;
	info.flag = 1;
	info.frmid = ((CNCLForm*)m_parent)->m_frmid;
	info.fldid = id;
	info.col = pNMListView->iSubItem;
	info.row = selectedItem;
	if (list->IsKindOf(RUNTIME_CLASS(CNCLListCtrl)))
	{
		tlist = (CNCLListCtrl*)list;
		if (info.row>=0)
			info.data_ptr = (int*)tlist->GetItemData(info.row);
		((CNCLForm*)m_parent)->CtrlListCallback(id, &info);
		tlist->m_selitem = pNMListView->iItem;
		tlist->m_change = 1;
	}
}
/***********************************************************************
c
c   FUNCTION: FormUserCallbacks13(UINT id)
c
c          Callback function for command
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks15(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
{
	*pResult = 0;
	if (m_create==0)
		return;
	NM_LISTVIEW* pNMListView = (NM_LISTVIEW*)pNMHDR;
	CNCLListCtrl *list = (CNCLListCtrl*)GetDlgItem(id);

    if (  ( pNMListView->uOldState & LVIS_SELECTED ) && 
         ( pNMListView->uNewState == 0 )  )
    {
		if ((list->m_selitem==pNMListView->iItem)&&(list->m_change==0))
		{
			list->SetItemState(pNMListView->iItem, 0, LVIS_SELECTED | LVIS_FOCUSED);	
			list->m_selitem = -1;
			UD_TABLEINFO info;
			info.flag = 1;
			info.frmid = ((CNCLForm*)m_parent)->m_frmid;
			info.fldid = id;
			info.col = -1;
			info.row = -1;
			info.data_ptr = NULL;
			((CNCLForm*)m_parent)->CtrlListCallback(id, &info);
			*pResult = 1;
			return;
		}
	}
	list->m_change = 0;
	list->m_selitem = pNMListView->iItem;
/*
.....Let form handle other form related function
*/
	((CNCLForm*)m_parent)->SendMessage(NM_DBLCLK_NCL, (WPARAM)id);
}

/***********************************************************************
c
c   FUNCTION: FormUserCallbacks16(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
c
c          Callback function for dropdown arrow of listctrl
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCallbacks16(UINT id, NMHDR *pNMHDR, LRESULT *pResult)
{
	*pResult = 0;
	if (m_create==0)
		return;
	int item;
	NMHEADER* nmkd = (NMHEADER*)pNMHDR;
	item = nmkd->iItem;

/*	CNCLListCtrl *list = (CNCLListCtrl*)GetDlgItem(id);
    if (  ( pNMListView->uOldState & LVIS_SELECTED ) && 
         ( pNMListView->uNewState == 0 )  )
    {
		if ((list->m_selitem==pNMListView->iItem)&&(list->m_change==0))
		{
			list->SetItemState(pNMListView->iItem, 0, LVIS_SELECTED | LVIS_FOCUSED);	
			list->m_selitem = -1;
			UD_TABLEINFO info;
			info.flag = 1;
			info.frmid = ((CNCLForm*)m_parent)->m_frmid;
			info.fldid = id;
			info.col = -1;
			info.row = -1;
			info.data_ptr = NULL;
			((CNCLForm*)m_parent)->CtrlListCallback(id, &info);
			*pResult = 1;
			return;
		}
	}
	list->m_change = 0;
	list->m_selitem = pNMListView->iItem;
/*
.....Let form handle other form related function
*/
//	((CNCLForm*)m_parent)->SendMessage(NM_DBLCLK_NCL, (WPARAM)id);
}

/***********************************************************************
c
c   SUBROUTINE:  set_button_color(COLORREF bcolor, COLORREF fcolor)
c
c   FUNCTION:  This function set a button color
c
c   INPUT:  i: button index
c			bcolor: button background color
c			fcolor: button foreground color
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormScrollView::set_button_color(int i, COLORREF bcolor, COLORREF fcolor)
{
	m_button[i].set_color(bcolor, fcolor);
	m_button[i].Invalidate();
	m_button[i].UpdateWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  set_button_color(COLORREF bcolor, COLORREF fcolor)
c
c   FUNCTION:  This function set a button color
c
c   INPUT:  i: button index
c			bcolor: button background color
c			fcolor: button foreground color
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormScrollView::set_button_color2(int i, COLORREF bcolor, COLORREF fcolor)
{
	m_button2[i].set_color(bcolor, fcolor);
	m_button2[i].Invalidate();
	m_button2[i].UpdateWindow();
}

void CNCLFormScrollView::set_button_color3(int i, COLORREF bcolor, COLORREF fcolor)
{
	m_button3[i].set_color(bcolor, fcolor);
	m_button3[i].Invalidate();
	m_button3[i].UpdateWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  Create_PicArea() 
c
c   FUNCTION:  This function create a picture area
c
c   INPUT:  filename: image file to displayed in picture window
c			dwStyle: window style
c			rect: list size
c			nID: list ID
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormScrollView::Create_PicArea(int indx, char *name,  char* filename, RECT& rect, UINT nID)
{
	ScreenToClient(&rect);
	m_picture[indx] = new CNCLPicSelWin(name, filename, this);
	m_picture[indx]->Create(WS_CHILD|WS_VISIBLE|WS_TABSTOP|WS_BORDER, rect, this, nID);
	m_picture[indx]->ShowWindow(SW_SHOW);
}

/***********************************************************************
c
c   SUBROUTINE:  Reset_picture(int indx, char* filename)
c
c   FUNCTION:  Reset the picture field a new image file
c
c   INPUT:  filename: image file to displayed in picture window
c			indx: picture field index
c			
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormScrollView::Reset_picture(int indx, char* filename)
{
	if (m_picture[indx]==NULL)
		return;
	m_picture[indx]->Reset_picture(filename);
	RedrawWindow();
	UpdateWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  set_picarea(int picno, int n_picarea, UD_PICAREA *picarea, UINT pid)
c
c   FUNCTION:  Set picture area information
c
c   INPUT:  filename: image file to displayed in picture window
c			indx: picture field index
c			
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLFormScrollView::set_picarea(int picno, int n_picarea, UD_PICAREA *picarea, UINT pid)
{
	for (int i=0; i<picno; i++)
	{
		for (int j=0; j<n_picarea; j++)
		{
			if (stricmp(m_picture[i]->m_name, picarea[j].name)==0)
			{
				m_picture[i]->set_picarea(picarea[j], pid);
			}
		}
	}
}
/*
.....not working
*/
void CNCLFormScrollView::close_tooltip()
{
/*
	for (int i=0; i<20; i++)
	{
		if (m_picture[i]!=NULL)
			m_picture[i]->close_tooltip();
	}
*/
}
/*
......here page is not index of select button, but page number (not include seperator)
*/
void CNCLFormScrollView::OnSecButton(int page)
{
	if (m_selsec==page)
		return;
	if (m_selsec!=-1)
		m_secbut[m_selsec].m_select = 0;
	m_selsec = page;
	int indx; 
	CPoint point, wpt;
	GetCursorPos(&point);
	ScreenToClient(&point);
/*
.....check if the mouse is inside the button
*/
	for(int i = 0; i < m_secno; i++ )
	{
		if (m_secbut[i].m_type==3)
			continue;
		CRect rect;
		m_secbut[i].GetWindowRect(rect);
		ScreenToClient(&rect);
		if (m_secbut[i].m_page==page)
		{
			m_secbut[i].m_select = 1;
			indx = i;
		}
		else
			m_secbut[i].m_select = 0;

		if(rect.PtInRect(point))
		{
			if (m_secbut[i].m_focus==0)
			{
				m_secbut[i].DrawHighLight2();
				m_secbut[i].m_focus = 1;
			}
		}
		else
		{
			m_secbut[i].DrawNormal2();
			m_secbut[i].m_focus = 0;
			m_secbut[i].CloseToolTip();
		}
	}
}
void CNCLFormScrollView::EnableSection(int but, int flag)
{
	if (flag)
	{
		m_secbut[but].EnableWindow(1);
	}
	else
	{
/*
......we need dis-select this button before disable it
*/
		m_secbut[but].m_select = 0;
		m_secbut[but].DrawNormal2();
		m_secbut[but].m_focus = 0;
		m_secbut[but].EnableWindow(0);
	}
	m_secbut[but].ShowWindow(SW_SHOW);
}

void CNCLFormScrollView::SetSecColor(int but, int color[3], int bold)
{
	COLORREF rcolor;
	rcolor = RGB (color[0], color[1], color[2]);
	m_secbut[but].SetTColor(rcolor);
	m_secbut[but].SetBoldFont(bold);
	m_secbut[but].ShowWindow(SW_SHOW);
	m_secbut[but].RedrawWindow();
}
int CNCLFormScrollView::Getindx(int page)
{
	int i;
	for (i=0; i<m_secno; i++)
	{
		if ( m_secbut[i].m_page==page)
		{
			return i;
		}
	}
	return -1;
}

/***********************************************************************
c
c   FUNCTION: FormUserCBLDblclk(UINT id)
c
c          Callback function for comb-box control double click event
c
c   INPUT:  id: control's id
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormScrollView::FormUserCBLDblclk(UINT id)
{
	if (m_create==0)
		return;
/*
.....check if it is listbox, if yes, call listbox callback
.....and force to call setfocus function
*/ 
	int type = ((CNCLForm*)m_parent)->get_idtype(id);
	if (type==5)
	{
		((CNCLForm*)m_parent)->SendMessage(LBN_DBLCLK, (WPARAM)id);
	}
	else
		((CNCLForm*)m_parent)->SendMessage(CBN_DBLCLK, (WPARAM)id);
}

int CNCLFormScrollView::ifFormViewValidId(UINT id)
{
	CWnd *wnd = (CNCLListCtrl*)GetDlgItem(id);
	if (wnd==NULL)
		return 0;
/*
......if the window is hiding, do not consider it valid
*/
	if (wnd->IsWindowVisible())
		return 1;
	else
		return 0;
}

