#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/************************************************************************
**		FILE NAME: CNCLListCtrl2.cpp
**
**	 Description - Functions and implementations for
**		CNCLListCtrl2 class (NCL list control for DATA)
**		
**	 CONTAINS:
**		member function of CNCLListCtrl2
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntlstctl2.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			01/20/17 , 12:21:06
**********************************************************************
*/
#include "wsntstdafx.h"
#include "wsntlstctl2.h"
#include "wsntres.h"
#include "wsntfsview.h"
#include "udforms.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern "C" char * uu_malloc(int);
/////////////////////////////////////////////////////////////////////////////
// CNCLListCtrl2

CNCLListCtrl2::CNCLListCtrl2()
{
	m_selitem = m_selSitem = -1;
	m_change = 0;
	m_sel_row = -1;
	m_sel_col = -1;
	m_nNumberOfRows = 0;
	m_nNumberOfCols = 0;
}

CNCLListCtrl2::~CNCLListCtrl2()
{
}

BEGIN_MESSAGE_MAP(CNCLListCtrl2, CListCtrl)
	//{{AFX_MSG_MAP(CNCLListCtrl2)
	ON_WM_NCCALCSIZE()
	ON_NOTIFY_REFLECT(NM_CLICK, OnClick)
	ON_NOTIFY_REFLECT(LVN_KEYDOWN, OnKeydown)
	//}}AFX_MSG_MAP
	ON_NOTIFY_REFLECT(NM_CUSTOMDRAW, OnCustomDraw)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNCLListCtrl2 message handlers

//****************************************************************************************

void CNCLListCtrl2::OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS FAR* lpncsp) 
{
/*	ModifyStyle(WS_HSCROLL | WS_VSCROLL,0,0); */
	CListCtrl::OnNcCalcSize(bCalcValidRects, lpncsp);
}


/**********************************************************************
**    I_FUNCTION :  OnCustomDraw(NMHDR* pNMHDR, LRESULT* pResult)
**       command callback routine for NM_CUSTOMDRAW on this control list
**    PARAMETERS   
**       INPUT  : 
**				pNMHDR             
**       OUTPUT :  
**				pResult
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLListCtrl2::OnCustomDraw(NMHDR *pNMHDR, LRESULT *pResult)
{
	NMLVCUSTOMDRAW* nmcd = (NMLVCUSTOMDRAW*)pNMHDR;
	*pResult = CDRF_DODEFAULT;	
	int row;
	int col;

	switch(nmcd->nmcd.dwDrawStage)
	{
		case CDDS_PREPAINT:
			*pResult = CDRF_NOTIFYITEMDRAW;
			return;

		case CDDS_ITEMPREPAINT:
			*pResult = CDRF_NOTIFYSUBITEMDRAW;
			return;
		
		case CDDS_SUBITEM|CDDS_ITEMPREPAINT:
		{
			*pResult = 0;

			row = nmcd->nmcd.dwItemSpec;
			col = nmcd->iSubItem;

			CString str = GetItemText(row,col);

			CRect rect;
			CDC* pDC=CDC::FromHandle(nmcd->nmcd.hdc);

			if(col>0)
				GetSubItemRect(row,col,LVIR_BOUNDS,rect);
			else
				GetItemRect(row,&rect,LVIR_LABEL);

			UINT uCode = DT_LEFT;
			
			if(row==m_sel_row && col==m_sel_col)
			{
				UD_ITEMDATA *data = (UD_ITEMDATA *)GetItemData(row);
				if ((col!=0)&&(col<data->itemnum))
				{
					COLORREF color = RGB(220,220,220);

					if(GetFocus()==this)
						color = GetSysColor(COLOR_HIGHLIGHT);

					CBrush brush(color);
					pDC->FillRect(&rect,&brush);
				}
				else if (col==0)
				{
					COLORREF color = RGB(220,220,220);

					if(GetFocus()==this)
						color = RGB(125,100,255);

					CBrush brush(color);
					pDC->FillRect(&rect,&brush);
				}
			}
/*
.....if we need offset, need redraw background, otherwise, there maybe junk drawing in the offset area
*/
//			rect.OffsetRect(4,0);
			pDC->DrawText(str,&rect,uCode);
			*pResult = CDRF_SKIPDEFAULT;
			break;
		}
	}
}

/**********************************************************************
**    I_FUNCTION :  OnClick(NMHDR* pNMHDR, LRESULT* pResult)
**       command callback routine for NM_CLICK on this control list
**    PARAMETERS   
**       INPUT  : 
**				pNMHDR             
**       OUTPUT :  
**				pResult
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLListCtrl2::OnClick(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NMITEMACTIVATE* nm = (NMITEMACTIVATE*)pNMHDR;
	
	if (nm->iItem<0)
	{
		*pResult = 0;
		return;
	}
	UD_ITEMDATA *data = (UD_ITEMDATA *)GetItemData(nm->iItem);
/*
.....we will accept the item=0 (the head label)
*/
//	if ((nm->iSubItem!=0)&&(nm->iSubItem<data->itemnum))
	if (nm->iSubItem<data->itemnum)
	{
		invalidate_grid(m_sel_row,m_sel_col);
		m_sel_row = nm->iItem;
		m_sel_col = nm->iSubItem;
		invalidate_grid(m_sel_row,m_sel_col);

		UD_TABLEINFO info;
		info.fldid = m_id;
		info.col = nm->iSubItem;
		info.row = nm->iItem;

		m_parent->PostMessage(ID_LISTITEM_CLICK, (WPARAM)&info, (LPARAM)m_id);
	}
	*pResult = 0;
}

/**********************************************************************
**    I_FUNCTION :  SetItemSel(int row, int col)
**       Set (row, col) item as the current select item
**    PARAMETERS   
**       INPUT  : 
**				row, col   
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLListCtrl2::SetItemSel(int row, int col)
{
	invalidate_grid(m_sel_row,m_sel_col);
	m_sel_row = row;
	m_sel_col = col;
	invalidate_grid(m_sel_row,m_sel_col);

	UD_TABLEINFO info;
	info.fldid = m_id;
	info.col = col;
	info.row = row;
	m_parent->PostMessage(ID_LISTITEM_CLICK, (WPARAM)&info, (LPARAM)m_id);
}
/**********************************************************************
**    I_FUNCTION :  DeSelAll
**       De-select the selection
**    PARAMETERS   
**       INPUT  : 
**				none    
**       OUTPUT :  
**				row, col
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLListCtrl2::DeSelAll()
{
	invalidate_grid(m_sel_row,m_sel_col);
	m_sel_row = -1;
	m_sel_col = -1;
	invalidate_grid(m_sel_row,m_sel_col);
}
/**********************************************************************
**    I_FUNCTION :  GetSelectedItem(int *row, int *col)
**       Get current select item's col and row
**    PARAMETERS   
**       INPUT  : 
**				none    
**       OUTPUT :  
**				row, col
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLListCtrl2::GetSelectedItem(int *row, int *col)
{
	*row = m_sel_row;
	*col = m_sel_col;
}

/**********************************************************************
**    I_FUNCTION :  fill_item_data(int row, UD_ITEMDATA *data)
**       fill the item data into structure UD_ITEMDATA
**    PARAMETERS   
**       INPUT  : 
**				row: row data needed        
**       OUTPUT :  
**				data: structure UD_ITEMDATA to fill data
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLListCtrl2::fill_item_data(int row, UD_ITEMDATA *data)
{
	int i,j,len,col;
	CString str;

	col = data->itemnum = m_nNumberOfCols;
	data->data_items = (char **) uu_malloc(col*sizeof(char*));
	for (i=0; i<col;i++)
	{
		str = GetItemText(row, i);
		len = str.GetLength();
		data->data_items[i] = (char*)uu_malloc((len+1)*sizeof(char));
		for (j=0; j<len; j++)
			data->data_items[i][j] = str[j];
		data->data_items[i][j] = '\0';
	}
}

/**********************************************************************
**    I_FUNCTION :  OnKeyDown(NMHDR* pNMHDR, LRESULT* pResult)
**       command callback routine for key down on this control list
**    PARAMETERS   
**       INPUT  : 
**				pNMHDR             
**       OUTPUT :  
**				pResult
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLListCtrl2::OnKeydown(NMHDR* pNMHDR, LRESULT* pResult) 
{
	int next;
	UD_ITEMDATA *data;
	NMLVKEYDOWN* nmkd = (NMLVKEYDOWN*)pNMHDR;
	
	switch(nmkd->wVKey)
	{
		case VK_LEFT: 
			m_sel_col--;
			if(m_sel_col<1)
				m_sel_col = 1;
			RedrawWindow();
			UpdateWindow();
			invalidate_grid(m_sel_row,m_sel_col+1);
			break;
		case VK_RIGHT:
			data = (UD_ITEMDATA *)GetItemData(m_sel_row);
			m_sel_col++;
			if(m_sel_col>data->itemnum-1)
				m_sel_col = data->itemnum-1;
			invalidate_grid(m_sel_row,m_sel_col-1);
			RedrawWindow();
			UpdateWindow();
			break;
		
		case VK_UP:   
			m_sel_row--;
			if(m_sel_row<0)
				m_sel_row = 0;
			data = (UD_ITEMDATA *)GetItemData(m_sel_row);
			if(m_sel_col>data->itemnum-1)
				m_sel_col = data->itemnum-1;
			invalidate_grid(m_sel_row+1,m_sel_col);
			break;
		
		case VK_DOWN: 
			m_sel_row++;
			if(m_sel_row>m_nNumberOfRows-1)
				m_sel_row = m_nNumberOfRows-1;
			data = (UD_ITEMDATA *)GetItemData(m_sel_row);
			if(m_sel_col>data->itemnum-1)
				m_sel_col = data->itemnum-1;
			invalidate_grid(m_sel_row-1,m_sel_col);
			break;
		case VK_PRIOR:
			invalidate_grid(m_sel_row,m_sel_col);
			m_sel_row = 0;
			break;
		case VK_NEXT:
			invalidate_grid(m_sel_row,m_sel_col);
			m_sel_row = m_nNumberOfRows-1;
			break;
		case VK_HOME:
			invalidate_grid(m_sel_row,m_sel_col);
			m_sel_col = 1;
			if(GetKeyState(VK_CONTROL)<0)
				m_sel_row = 0;
			SetItemState(m_sel_row,LVIS_FOCUSED,LVIS_FOCUSED);
			*pResult = CDRF_SKIPDEFAULT;
			invalidate_grid(m_sel_row,m_sel_col);
			return;
			break;
		case VK_END:
			if (m_nNumberOfRows<=0)
				return;
			if(GetKeyState(VK_CONTROL)<0)
				next = m_nNumberOfRows-1;
			else
				next = m_sel_row;

			data = (UD_ITEMDATA *)GetItemData(next);
			invalidate_grid(m_sel_row,m_sel_col);

			m_sel_row = next;
			m_sel_col = m_nNumberOfCols-1;
			if (m_sel_col>data->itemnum-1)
				m_sel_col = data->itemnum - 1;
			SetItemState(m_sel_row,LVIS_FOCUSED,LVIS_FOCUSED);
			*pResult=CDRF_SKIPDEFAULT;
			invalidate_grid(m_sel_row,m_sel_col);
			return;
	}
	
	*pResult = 0;
}

/**********************************************************************
**    I_FUNCTION :  invalidate_grid(int row, int col)
**       Invalidate the (row,col) item area
**    PARAMETERS   
**       INPUT  : 
**				row  col           
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
void CNCLListCtrl2::invalidate_grid(int row, int col)
{
	CRect r;

	if(col==0)
		GetItemRect(row,&r,LVIR_LABEL);
	else
		GetSubItemRect(row,col,LVIR_BOUNDS,r);

	InvalidateRect(&r);
}
IMPLEMENT_DYNAMIC(CNCLListCtrl2, CDialog)

//****************************************************************************************
#endif


