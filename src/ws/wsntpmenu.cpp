/********************************************************************* 
**  NAME:  wsntpmenu.cpp
**
**			Native WinNT popup menu (include menu bar menu )functions
**			implementation of CNCLMenu class functions
**	CONTAINS: CNCLMenu class functions
**			all functions declared in wsntpmenu.h
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntpmenu.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:30
*********************************************************************/

#include "wsntstdafx.h"
#include "wsntctl.h"
#include "wsntpmenu.h"
#include "wsntframe.h"
#include "wsntres.h"
#include "dmotif.h"
#include "zkeysym.h"
#define NBUTTONNUM	5000

extern CMainFrame *NCL_MainFrame;
extern int NCL_popmenu;
extern "C" int UW_icon_size, UW_menu_fmt;
extern "C" int ud_is_playback();
extern "C" int uz_load_keys2(char *keyfile, int flag);
extern "C" int uz_load_accel2(char *keyfile);

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

#define SEP_LEN 9
#define DRAGBAR_LEN 8
#define MENU_GAP 6
/***********************************************************************
**
**   FUNCTION: TransparentStretchBlt( HDC hdcDest, int nXDest, int nYDest, int nWidth, 
**				int nHeight, HBITMAP hBitmap, int nXSrc, int nYSrc,int nWidthSrc, 
**				int nHeightSrc, 
**				COLORREF colorTransparent)
**
**             drawing bitmap transparently
**
**   INPUT:  hdcDest		- Handle to destination device context 
**			nXDest		- x-coordinate of destination rectangle's upper-left corner 
**			nYDest		- y-coordinate of destination rectangle's upper-left corner 
**			nWidth		- Width of destination rectangle 
**			nHeight		- height of destination rectangle 
**			hBitmap		- Handle of the source bitmap
**			nXSrc		- x-coordinate of source rectangle's upper-left corner 
**			nYSrc		- y-coordinate of source rectangle's upper-left corner 
**			nWidthSrc,  - Specifies the width, in logical units, of the source rectangle. 
**			nHeightSrc  - Specifies the height, in logical units, of the source rectangle
**			colorTransparent	- The transparent color
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void TransparentStretchBlt( HDC hdcDest, int nXDest, int nYDest, int nWidth, 
			int nHeight, HBITMAP hBitmap, int nXSrc, int nYSrc,   int nWidthSrc, 
			int nHeightSrc, COLORREF colorTransparent)
{
	CDC dc, memDC, maskDC, tempDC;
	dc.Attach( hdcDest );
	maskDC.CreateCompatibleDC(&dc);
	CBitmap maskBitmap;
	
	CBitmap* pOldMemBmp = NULL;
	CBitmap* pOldMaskBmp = NULL;
	HBITMAP hOldTempBmp = NULL;
	
	memDC.CreateCompatibleDC(&dc);
	tempDC.CreateCompatibleDC(&dc);
	CBitmap bmpImage;
////
	bmpImage.CreateCompatibleBitmap(&dc, nWidth, nHeight);
	pOldMemBmp = memDC.SelectObject(&bmpImage );
	
	hOldTempBmp = (HBITMAP) ::SelectObject( tempDC.m_hDC, hBitmap );
	
	memDC.StretchBlt( 0,0,nWidth, nHeight, &tempDC, nXSrc, nYSrc, nWidthSrc, nHeightSrc, SRCCOPY );	
/*
......Create monochrome bitmap for the mask
*/
////
	maskBitmap.CreateBitmap( nWidth, nHeight, 1, 1, NULL );
	pOldMaskBmp = maskDC.SelectObject( &maskBitmap );
	memDC.SetBkColor( colorTransparent);
/*
......Create the mask from the memory DC
*/
//	maskDC.StretchBlt( 0, 0, nWidth, nHeight, &memDC, 
//		0, 0, nWidthSrc, nHeightSrc, SRCCOPY );
	maskDC.BitBlt( 0, 0, nWidth, nHeight, &memDC, 
		0, 0, SRCCOPY );
/* 
.....Set the background in memDC to black. Using SRCPAINT with black 
.....and any other color results in the other color, thus making 
.....black the transparent color
*/
	memDC.SetBkColor(RGB(0,0,0));
	memDC.SetTextColor(RGB(255,255,255));
//	memDC.StretchBlt(0, 0, nWidth, nHeight, &maskDC, 0, 0, nWidthSrc, nHeightSrc, SRCAND);
	memDC.BitBlt(0, 0, nWidth, nHeight, &maskDC, 0, 0, SRCAND);
/* 
.....Set the foreground to black. See comment above.
*/
	dc.SetBkColor(RGB(255,255,255));
	dc.SetTextColor(RGB(0,0,0));
//	dc.StretchBlt(nXDest, nYDest, nWidth, nHeight, &maskDC, 0, 0, nWidthSrc, nHeightSrc, SRCAND);
	dc.BitBlt(nXDest, nYDest, nWidth, nHeight, &maskDC, 0, 0, SRCAND);
/*
.....Combine the foreground with the background
*/
//	dc.StretchBlt(nXDest, nYDest, nWidth, nHeight, &memDC, 
//		0, 0, nWidthSrc, nHeightSrc, SRCPAINT);
	dc.BitBlt(nXDest, nYDest, nWidth, nHeight, &memDC, 
		0, 0, SRCPAINT);
		
	if (hOldTempBmp)
		::SelectObject( tempDC.m_hDC, hOldTempBmp);
	if (pOldMaskBmp)
		maskDC.SelectObject( pOldMaskBmp );
	if (pOldMemBmp)
		memDC.SelectObject( pOldMemBmp );
	
	dc.Detach();
}

/***********************************************************************
**
**   FUNCTION: TransparentBlt( HDC hdcDest, int nXDest, int nYDest, int nWidth, 
**				int nHeight, HBITMAP hBitmap, int nXSrc, int nYSrc,
**				COLORREF colorTransparent)
**
**             drawing bitmap transparently
**
**   INPUT:  hdcDest		- Handle to destination device context 
**			nXDest		- x-coordinate of destination rectangle's upper-left corner 
**			nYDest		- y-coordinate of destination rectangle's upper-left corner 
**			nWidth		- Width of destination rectangle 
**			nHeight		- height of destination rectangle 
**			hBitmap		- Handle of the source bitmap
**			nXSrc		- x-coordinate of source rectangle's upper-left corner 
**			nYSrc		- y-coordinate of source rectangle's upper-left corner 
**			colorTransparent	- The transparent color
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void TransparentBlt( HDC hdcDest, int nXDest, int nYDest, int nWidth, 
			int nHeight, HBITMAP hBitmap, int nXSrc, int nYSrc,
			COLORREF colorTransparent)
{
	CDC dc, memDC, maskDC, tempDC;
	dc.Attach( hdcDest );
	maskDC.CreateCompatibleDC(&dc);
	CBitmap maskBitmap;
	
	CBitmap* pOldMemBmp = NULL;
	CBitmap* pOldMaskBmp = NULL;
	HBITMAP hOldTempBmp = NULL;
	
	memDC.CreateCompatibleDC(&dc);
	tempDC.CreateCompatibleDC(&dc);
	CBitmap bmpImage;
	bmpImage.CreateCompatibleBitmap( &dc, nWidth, nHeight );
	pOldMemBmp = memDC.SelectObject( &bmpImage );
	
	hOldTempBmp = (HBITMAP) ::SelectObject( tempDC.m_hDC, hBitmap );
	
	memDC.BitBlt( 0,0,nWidth, nHeight, &tempDC, nXSrc, nYSrc, SRCCOPY );
/*
......Create monochrome bitmap for the mask
*/
	maskBitmap.CreateBitmap( nWidth, nHeight, 1, 1, NULL );
	pOldMaskBmp = maskDC.SelectObject( &maskBitmap );
	memDC.SetBkColor( colorTransparent );
	
/*
......Create the mask from the memory DC
*/
	maskDC.BitBlt( 0, 0, nWidth, nHeight, &memDC, 
		0, 0, SRCCOPY );
	
/* 
.....Set the background in memDC to black. Using SRCPAINT with black 
.....and any other color results in the other color, thus making 
.....black the transparent color
*/
	memDC.SetBkColor(RGB(0,0,0));
	memDC.SetTextColor(RGB(255,255,255));
	memDC.BitBlt(0, 0, nWidth, nHeight, &maskDC, 0, 0, SRCAND);
	
/* 
.....Set the foreground to black. See comment above.
*/
	dc.SetBkColor(RGB(255,255,255));
	dc.SetTextColor(RGB(0,0,0));
	dc.BitBlt(nXDest, nYDest, nWidth, nHeight, &maskDC, 0, 0, SRCAND);
	
/*
.....Combine the foreground with the background
*/
	dc.BitBlt(nXDest, nYDest, nWidth, nHeight, &memDC, 
		0, 0, SRCPAINT);
		
	if (hOldTempBmp)
		::SelectObject( tempDC.m_hDC, hOldTempBmp);
	if (pOldMaskBmp)
		maskDC.SelectObject( pOldMaskBmp );
	if (pOldMemBmp)
		memDC.SelectObject( pOldMemBmp );
	
	dc.Detach();
}



class CDisplayIC : public CDC
{
public:
	CDisplayIC() { CreateIC(_T("DISPLAY"), NULL, NULL, NULL); }
};

IMPLEMENT_DYNAMIC(CNCLMenu,CMenu)

/////////////////////////////////////////////////////////////////////////////
// CNCLMenu

/***********************************************************************
**
**   FUNCTION: CNCLMenu
**
**              Constructor of class CNCLMenu
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLMenu::CNCLMenu()
{
	int i;
	m_firstdraw = 1;
	for (i=0; i<100; i++)
	{
		m_id[i] = 0;
		m_hBitmap[i] = NULL;
	}
	m_bkcolor = NULL;
	m_idnum = -1;
	m_dragidnum = -1;
	m_hCBitmap = NULL;
	m_parent = m_subnum = -1;
	m_iconsize = 16;
	if ((UW_icon_size==0)||(UW_icon_size==1)||(UW_icon_size==2))
	{
		m_iconsize = 16;
	}
	else if ((UW_icon_size==3)||(UW_icon_size==4))
	{
		m_iconsize = 24;
	}
}


/***********************************************************************
**
**   FUNCTION: HasAllMenuIcon
**
**         check if this menu have icon on every item
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    1: yes
**
**********************************************************************/
int CNCLMenu::HasAllMenuIcon()
{
	unsigned int i;
	CString rString;

	for (i=0; i<GetMenuItemCount(); i++)
	{
		if ((m_hBitmap[i]==NULL)&&(m_id[i]!=0))
			return 0;
	}
	return 1;
}

/***********************************************************************
**
**   FUNCTION: HasNoMenuIcon
**
**         check if this menu have no icon
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    1: yes
**
**********************************************************************/
int CNCLMenu::HasNoMenuIcon()
{
	unsigned int i;
	CString rString;
	for (i=0; i<GetMenuItemCount(); i++)
	{
		if (m_hBitmap[i]!=NULL)
			return 0;
	}
	return 1;
}


/***********************************************************************
**
**   FUNCTION: SetBackColor(int r, int g, int b)
**
**         Set back ground color of popup menu
**
**   INPUT:  r,g,b: backgroung color to be set
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLMenu::SetBackColor(int r, int g, int b)
{
	m_bkcolor = RGB(r,g,b);
}

/***********************************************************************
**
**   FUNCTION: SetBitmap(HBITMAP hbmap, HBITMAP hchkbmap, int num, UINT id, int bnum)
**
**         Set bitmap used by popup menu
**
**   INPUT:  hbmap: bitmap handle for unchecked menu item
**			hchkbmap: bitmap handle for checked menu item
**			num: menu item index number
**			id: menu item id
**			bnum: bitmap number inside hbmap
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLMenu::SetBitmap(HBITMAP hbmap, HBITMAP hchkbmap, int num, UINT id, int bnum)
{
/*
.....if hbmap == NULL but bnum != -1
.....then use m_hCBitmap (current bitmap handler)
*/
	if ((hbmap == NULL)&&(bnum != -1))
		m_hBitmap[num] = m_hCBitmap;
	else
	{
		m_hBitmap[num] = hbmap;
		if (m_hCBitmap==NULL)
			m_hCBitmap = hbmap;
	}

	m_bmpnum[num] = bnum;
	m_hchkBitmap[num] = hchkbmap;
	if (id!=-1)
		m_id[num] = id;

	if (id>100000)
		id = id;
	if ((UW_icon_size==0)||(UW_icon_size==1)||(UW_icon_size==2))
	{
		m_iconsize = 16;
	}
	else if ((UW_icon_size==3)||(UW_icon_size==4))
	{
		m_iconsize = 24;
	}
}
/***********************************************************************
**
**   FUNCTION: DrawItem(LPDRAWITEMSTRUCT lpDIS)
**
**         Called by the framework when a visual aspect of 
**			an owner-drawn menu changes
**
**   INPUT:  lpDIS:   A pointer to a 
**				DRAWITEMSTRUCT structure that contains 
**				information about the type of drawing required.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLMenu::DrawItem(LPDRAWITEMSTRUCT lpDIS)
{
	ASSERT(lpDIS->CtlType == ODT_MENU);
//	CString itemtext = ( LPCTSTR) lpDIS->itemData;
/*
......do not draw anything if drawitem flag is not 1
*/
	if (UDM_menu[m_idnum].drawitem!=1)
		return;

	CImageList imagelst;
	CBitmap cBmap;
	int i, itemnum;
	CBitmap *bcBmp = NULL;
	COLORREF sav_bkcolor;
	CRect hrc(lpDIS->rcItem);
/*
.....save the display menu number to draging
*/
	NCL_popmenu = m_idnum;
	if (m_idnum==54)
		NCL_popmenu = m_idnum;

/*	if (UDM_menu[m_idnum].key_loaded==0)*/
	{
		UDM_menu[m_idnum].key_loaded = 1;
		if (UDM_menu[m_idnum].keyfile[0]!='\0')
		{
			uz_load_keys2(UDM_menu[m_idnum].keyfile, 0);
			uz_load_accel2(UDM_menu[m_idnum].keyfile);
		}
	}

	itemnum = -1;
	for (i=0; i<100; i++)
	{
		if (lpDIS->itemID == m_id[i])
		{
			itemnum = i;
			break;
		}
	}
	if (itemnum!=-1)
		bcBmp = cBmap.FromHandle(m_hBitmap[itemnum]);

	CPoint pt;            // Position for upper left corner of bitmap
	CSize size;        // Size (width and height) of bitmap
	POINT ptOrigin;

	CDC *dc = new CDC();
	dc->Attach(lpDIS->hDC);
	if (m_bkcolor!=NULL)
	{
		sav_bkcolor = dc->SetBkColor(m_bkcolor);
	}
	CRect rc(lpDIS->rcItem);
	CRect savrc(lpDIS->rcItem);
	ASSERT(rc.Width() < 500);
	if (lpDIS->itemState & ODS_FOCUS)
		dc->DrawFocusRect(&rc);
	dc->DrawFocusRect(&rc);
	COLORREF cr;
	BYTE red, green, blue;

	if ((lpDIS->itemID<(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM))
		|| (lpDIS->itemID>(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM + UDM_MAX_MENU)))
	{
		cr = (lpDIS->itemState & ODS_SELECTED) ? 
			::GetSysColor(COLOR_HIGHLIGHT) :
			dc->GetBkColor();
	}
	else
	{
		cr = dc->GetBkColor();
		red = GetRValue(cr);
		green = GetGValue(cr);
		blue = GetBValue(cr);
		cr = (lpDIS->itemState & ODS_SELECTED) ? 
			::GetSysColor(COLOR_HIGHLIGHT) :
			RGB(red-0x20, green-0x20, blue-0x20); 
	}
	CBrush brushFill(cr);
	cr = dc->GetTextColor();
	if (lpDIS->itemState & ODS_SELECTED)
		dc->SetTextColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));

	int nBkMode = dc->SetBkMode(TRANSPARENT);

	dc->FillRect(&rc, &brushFill);

	if (itemnum==-1)
		goto done;

	rc.left += m_iconsize+4;
	rc.top += MENU_GAP/2 + 1;

	if ((lpDIS->itemID<(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM))
		|| (lpDIS->itemID>(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM + UDM_MAX_MENU)))
/*
.....always output text
*/
	{
		CString itemtext = ( LPCTSTR) lpDIS->itemData;
		dc->TextOut(rc.left,rc.top,itemtext,itemtext.GetLength());
		if (itemtext[0] < 32 || itemtext[0] > 128)
		{
			rc.left += 0;
		}
	}
		
	if ((!(lpDIS->itemState&ODS_CHECKED))&&(m_hBitmap[itemnum]==NULL))
		goto done;
	if (UDM_menu[m_idnum].menu_format==1)
		goto done;

	if (lpDIS->itemState & ODS_CHECKED)
		bcBmp = cBmap.FromHandle(m_hchkBitmap[itemnum]);
	else
		bcBmp = cBmap.FromHandle(m_hBitmap[itemnum]);

	size.cx = m_iconsize;
	size.cy = m_iconsize;
	pt.x = 1;
	pt.y = (savrc.top + savrc.bottom)/2 - m_iconsize/2;
	ptOrigin.x = m_iconsize*m_bmpnum[itemnum];
	ptOrigin.y = 0;
	if (((lpDIS->itemID<(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM))
		|| (lpDIS->itemID>(WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM + UDM_MAX_MENU)) )
		&& (lpDIS->itemState & ODS_SELECTED))
	{
		cr = dc->GetBkColor();
		CBrush hbrushFill(cr);
 		hrc.right = pt.x + size.cx + 2;
		dc->FillRect(&hrc, &hbrushFill);
	}
	TransparentBlt(lpDIS->hDC, pt.x, pt.y, m_iconsize, 
			m_iconsize, m_hBitmap[itemnum], ptOrigin.x, ptOrigin.y, 0xC0C0C0);
/*	if (UW_icon_size==0)
	{
		sizex = sizey = 16;
	}
	else if (UW_icon_size==1)
	{
		sizex = sizey = 24;
	}
	else if (UW_icon_size==2)
	{
		sizex = sizey = 32;
	}
	else if (UW_icon_size==3)
	{
		sizex = sizey = 40;
	}
	else if (UW_icon_size==4)
	{
		sizex = sizey = 48;
	}
	TransparentStretchBlt(lpDIS->hDC, pt.x, pt.y, m_iconsize, 
			m_iconsize, m_hBitmap[itemnum], ptOrigin.x, ptOrigin.y, sizex, sizey, 0xC0C0C0);
*/
/*
.....draw hight rectangle when highlighted
*/
	if (lpDIS->itemState & ODS_SELECTED)
	{
        dc->Draw3dRect(hrc,::GetSysColor(COLOR_3DHIGHLIGHT),::GetSysColor(COLOR_3DSHADOW));
	}
done:;
	dc->SetTextColor(cr);
	dc->SetBkMode(nBkMode);
	if (m_bkcolor!=NULL)
		dc->SetBkColor(sav_bkcolor);
	dc->Detach();
	delete dc;
/*
.....if the popup menu displayed while in PLAYBACK on,
.....we should continue with record/playback command
.....but by default, the popup menu will stay and capture 
.....the event until we have a event (left mouse button down)
.....so, we have to post a message here.
*/
	if (ud_is_playback()==1)
	{
		NCL_MainFrame->PostMessage(WM_LBUTTONDOWN, MK_LBUTTON);
		return;
	}
}

/***********************************************************************
**
**   FUNCTION: MeasureItem(LPMEASUREITEMSTRUCT lpMIS)
**
**         Set menu item size
**
**   INPUT:  lpDIS:   A pointer to a 
**				DRAWITEMSTRUCT structure that contains 
**				information about the type of drawing required.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLMenu::MeasureItem(LPMEASUREITEMSTRUCT lpMIS)
{
	ASSERT(lpMIS->CtlType == ODT_MENU);
	CString itemtext = ( LPCTSTR) lpMIS->itemData;
	CDisplayIC Ddc;
	CWnd *picctl = NCL_MainFrame;
	CSize sizeText;

	if (picctl!=NULL)
	{
		CClientDC dc(picctl);
		sizeText = dc.GetTextExtent(itemtext,itemtext.GetLength());
	}
	else
		sizeText = Ddc.GetTextExtent(itemtext,itemtext.GetLength());
	ASSERT(sizeText.cx < 500);
	if ((lpMIS->itemID>=WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM)
		&&(lpMIS->itemID<=WM_APP+UDM_MAX_MENU+UZ_MAX_KEYITEM+NBUTTONNUM + UDM_MAX_MENU))
	{
		lpMIS->itemHeight = DRAGBAR_LEN;
		lpMIS->itemWidth = 2;
	}
	else
	{
//		if (m_hCBitmap==NULL)
//		{
//			lpMIS->itemHeight = sizeText.cy + MENU_GAP;
//			lpMIS->itemWidth = sizeText.cx + 16;
//		}
//		else
		{
			lpMIS->itemHeight = m_iconsize + MENU_GAP;
			lpMIS->itemWidth = sizeText.cx + m_iconsize;
		}
	}
}
