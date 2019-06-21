/********************************************************************* 
**  NAME:  wshdibapi.cpp
**
**			Source file for Device-Independent Bitmap (DIB) 
**			
**	CONTAINS: 
**			uw_ntpaintDIB()   
**			uw_saveDIB(HDIB hDib, HANDLE file)
**			uw_cywin_DIB(HWND hWnd, int size, int fit)
**			uw_cyscrn_rgb
**			and some support static functions
**
**    COPYRIGHT 2002 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       wshdibapi.cpp , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:15
*********************************************************************/
#include "stdafx.h"
#include <assert.h>
#include "usysdef.h"
#include "wsntbitmap.h"
#include "mdrwsize.h"
#include "wsntdoc.h"
#include "wsntview.h"
#include "wsgl.h"

extern "C" HDC  UW_NTgraphicDC;
extern HWND UW_NTgraphicIPV;
extern "C" int UD_printipv;
extern CClientDC *uw_curhDC;
extern CClientDC *uw_curIPVhDC;
extern "C" char * uu_malloc(int size );
extern "C" void uu_free(char*);

extern LPSTR     WINAPI  FindDIBBits (LPSTR lpbi);
extern DWORD     WINAPI  DIBWidth (LPSTR lpDIB);
extern DWORD     WINAPI  DIBHeight (LPSTR lpDIB);
extern WORD      WINAPI  PaletteSize (LPSTR lpbi);
extern WORD      WINAPI  DIBNumColors (LPSTR lpbi);


static HDIB WINAPI  CopyScreenToDIB(LPRECT lpRect, int size, int fit, int bcolor);
static HBITMAP WINAPI  CopyScreenToBitmap(LPRECT lpRect, int paper_size, int fit, int bcolor);
static void WINAPI  DIBError (int ErrNo);
static void WINAPI  InitBitmapInfoHeader (LPBITMAPINFOHEADER lpBmInfoHdr,
                                        DWORD dwWidth,
                                        DWORD dwHeight,
                                          int nBPP);
static HANDLE WINAPI  BitmapToDIB (HBITMAP hBitmap, HPALETTE hPal);
static int WINAPI  PalEntriesOnDevice (HDC hDC);
static HPALETTE WINAPI  GetSystemPalette (void);

extern CWnd *NCL_Current_View;
extern "C" void uw_get_bkcolor(int *, int *, int *);

void TransparentBlt( HDC hdcDest, int nXDest, int nYDest, int nWidth, 
			int nHeight, HBITMAP hBitmap, int nXSrc, int nYSrc,
			COLORREF colorTransparent);
/***********************************************************************
**
**   FUNCTION: uw_ntpaintDIB(HDC hDC, LPRECT lpDCRect, HDIB hDIB, 
**					 LPRECT lpDIBRect, HPALETTE hPal)
**
**             print DIB to device context (printer)
**
**   INPUT:  hDC		- Handle to source device context 
**			lpDCRect	- destination rectangle
**			hDIB		- DIB handler (to be print)
**			lpDIBRect	- DIB rectangle 
**			hPal		- palette to use
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL WINAPI uw_ntpaintDIB(HDC hDC, LPRECT lpDCRect, HDIB hDIB, 
					 LPRECT lpDIBRect, HPALETTE hPal)
{
	LPSTR    lpDIBHdr;            
	LPSTR    lpDIBBits;           
	BOOL     bSuccess=FALSE;    
	HPALETTE hOldPal=NULL;     

	if (hDIB == NULL)
		return FALSE;

	lpDIBHdr  = (LPSTR) ::GlobalLock((HGLOBAL) hDIB);
	lpDIBBits = FindDIBBits(lpDIBHdr);

	hOldPal = ::SelectPalette(hDC, hPal, TRUE);
/* 
......Make sure to use the stretching mode best for color pictures 
*/
	::SetStretchBltMode(hDC, HALFTONE);

	if ((RECTWIDTH(lpDCRect)  == RECTWIDTH(lpDIBRect)) &&
		(RECTHEIGHT(lpDCRect) == RECTHEIGHT(lpDIBRect)))
		bSuccess = ::SetDIBitsToDevice(hDC,                   
							lpDCRect->left,            
							lpDCRect->top,             
							RECTWIDTH(lpDCRect),    
							RECTHEIGHT(lpDCRect),  
							lpDIBRect->left,           
							(int)DIBHeight(lpDIBHdr) -
							lpDIBRect->top -
							RECTHEIGHT(lpDIBRect),  
							0,                          
							(WORD)DIBHeight(lpDIBHdr), 
							lpDIBBits,                
							(LPBITMAPINFO)lpDIBHdr,    
							DIB_RGB_COLORS);        
	else
		bSuccess = ::StretchDIBits(hDC,                   
							lpDCRect->left,        
							lpDCRect->top,             
							RECTWIDTH(lpDCRect), 
							RECTHEIGHT(lpDCRect),  
							lpDIBRect->left,          
							lpDIBRect->top,         
							RECTWIDTH(lpDIBRect),       
							RECTHEIGHT(lpDIBRect),    
							lpDIBBits,                    
							(LPBITMAPINFO)lpDIBHdr,       
							DIB_RGB_COLORS,            
							SRCCOPY);                  

	::GlobalUnlock((HGLOBAL) hDIB);

	if (hOldPal != NULL)
	{
		::SelectPalette(hDC, hOldPal, TRUE);
	}

	return bSuccess;
}
#define DIB_HEADER_MARKER   ((WORD) ('M' << 8) | 'B')

/*************************************************************************
**
**  Function:  uw_saveDIB(HDIB hDib, HANDLE file)
**
**		Save DIB into a file
**			 .
**	 INPUT:  file: file handle
**			hDib: DIB handle to be save
**   OUTPUT : none
**   RETURN:   FALSE: not saved
**				TRUE: saved
**
**
*************************************************************************/
BOOL WINAPI uw_saveDIB(HDIB hDib, HANDLE file)
{
	BITMAPFILEHEADER bmfHdr; 
	LPBITMAPINFOHEADER lpBI;  
	DWORD dwDIBSize, len;

	if (hDib == NULL)
		return FALSE;

	lpBI = (LPBITMAPINFOHEADER) ::GlobalLock((HGLOBAL) hDib);
	if (lpBI == NULL)
		return FALSE;

	if (!IS_WIN30_DIB(lpBI))
	{
		::GlobalUnlock((HGLOBAL) hDib);
		return FALSE;   
	}


	bmfHdr.bfType = DIB_HEADER_MARKER;  // "BM"

/*
......Calculating the size of the DIB is a bit tricky (if we want to
......do it right).  The easiest way to do this is to call GlobalSize()
......on our global handle, but since the size of our global memory may have
......been padded a few bytes, we may end up writing out a few too
......many bytes to the file (which may cause problems with some apps).
......So, instead let's calculate the size manually (if we can)
......First, find size of header plus size of color table.  Since the
......first DWORD in both BITMAPINFOHEADER and BITMAPCOREHEADER conains
......the size of the structure, let's use this.
*/
	dwDIBSize = *(LPDWORD)lpBI + ::PaletteSize((LPSTR)lpBI);  // Partial Calculation

	if ((lpBI->biCompression == BI_RLE8) || (lpBI->biCompression == BI_RLE4))
	{
		dwDIBSize += lpBI->biSizeImage;
	}
	else
	{
		DWORD dwBmBitsSize; 

		dwBmBitsSize = WIDTHBYTES((lpBI->biWidth)*((DWORD)lpBI->biBitCount)) * lpBI->biHeight;

		dwDIBSize += dwBmBitsSize;
		lpBI->biSizeImage = dwBmBitsSize;
	}

	bmfHdr.bfSize = dwDIBSize + sizeof(BITMAPFILEHEADER);
	bmfHdr.bfReserved1 = 0;
	bmfHdr.bfReserved2 = 0;

	bmfHdr.bfOffBits = (DWORD)sizeof(BITMAPFILEHEADER) + lpBI->biSize
											  + PaletteSize((LPSTR)lpBI);

	::WriteFile(file, (LPSTR)&bmfHdr, sizeof(BITMAPFILEHEADER), &len, NULL);
	::WriteFile(file, lpBI, dwDIBSize, &len, NULL);

	::GlobalUnlock((HGLOBAL) hDib);
	::GlobalFree((HGLOBAL) hDib);
	hDib = NULL;

	return TRUE;
}

/*************************************************************************
**
**  Function:  InitBitmapInfoHeader ()
**				Initialize bitmap BITMAPINFOHEADER structure
**			 .
**	 INPUT:  lpBmInfoHdr: BITMAPINFOHEADER structure to be initialize
**				dwWidth, dwHeight: bitmap width and height
**				nBPP: bitmap color bits
**   OUTPUT : none
**   RETURN:   none
**
**
*************************************************************************/
void WINAPI  InitBitmapInfoHeader (LPBITMAPINFOHEADER lpBmInfoHdr,
                                        DWORD dwWidth,
                                        DWORD dwHeight,
                                          int nBPP)
{
	memset (lpBmInfoHdr, 0, sizeof (BITMAPINFOHEADER));

	lpBmInfoHdr->biSize      = sizeof (BITMAPINFOHEADER);
	lpBmInfoHdr->biWidth     = dwWidth;
	lpBmInfoHdr->biHeight    = dwHeight;
	lpBmInfoHdr->biPlanes    = 1;

	if (nBPP <= 1)
		nBPP = 1;
	else if (nBPP <= 4)
		nBPP = 4;
	else if (nBPP <= 8)
		nBPP = 8;
	else
		nBPP = 24;

	lpBmInfoHdr->biBitCount  = nBPP;
	lpBmInfoHdr->biSizeImage = WIDTHBYTES (dwWidth * nBPP) * dwHeight;
}

/*************************************************************************
**
**  Function:  BitmapToDIB (HBITMAP hBitmap, HPALETTE hPal)
**		Create DIB from bitmap handle
**		
**			 .
**	 INPUT:  hBitmap: bitmap handle
**			hPal:  palette handle
**   OUTPUT : none
**   RETURN:   DIB handle
**
**
*************************************************************************/
HANDLE WINAPI  BitmapToDIB (HBITMAP hBitmap, HPALETTE hPal)
{
	BITMAP             Bitmap;
	BITMAPINFOHEADER   bmInfoHdr;
	LPBITMAPINFOHEADER lpbmInfoHdr;
	LPSTR              lpBits;
	HDC                hMemDC;
	HANDLE             hDIB;
	HPALETTE           hOldPal = NULL;

	if (!hBitmap)
		return NULL;

	if (!GetObject (hBitmap, sizeof (Bitmap), (LPSTR) &Bitmap))
		return NULL;

	InitBitmapInfoHeader (&bmInfoHdr,
                         Bitmap.bmWidth,
                         Bitmap.bmHeight,
                         Bitmap.bmPlanes * Bitmap.bmBitsPixel);

	hDIB = GlobalAlloc (GHND, sizeof (BITMAPINFOHEADER) +
             PaletteSize ((LPSTR) &bmInfoHdr) + bmInfoHdr.biSizeImage);

	if (!hDIB)
		return NULL;

	lpbmInfoHdr  = (LPBITMAPINFOHEADER) GlobalLock (hDIB);
	*lpbmInfoHdr = bmInfoHdr;
	lpBits       = FindDIBBits ((LPSTR) lpbmInfoHdr);

	hMemDC       = GetDC (NULL);
   
	if (hPal)
	{
		hOldPal = SelectPalette (hMemDC, hPal, FALSE);
		RealizePalette (hMemDC);
	}
	if (!GetDIBits (hMemDC,
                   hBitmap,
                   0,
                   Bitmap.bmHeight,
                   lpBits,
                   (LPBITMAPINFO) lpbmInfoHdr,
                   DIB_RGB_COLORS))
	{
		GlobalUnlock (hDIB);
		GlobalFree (hDIB);
		hDIB = NULL;
	}
	else
		GlobalUnlock (hDIB);

	if (hOldPal)
		SelectPalette (hMemDC, hOldPal, FALSE);

	ReleaseDC (NULL, hMemDC);

	return hDIB;
}



/*************************************************************************
**
**  Function:  PalEntriesOnDevice (HDC hDC)
**		Find out the number of palette entries on this device
**		
**			 .
**	 INPUT:  HDC hDC: device contex handle
**			
**   OUTPUT : none
**   RETURN:   number of palette entries
**
**
*************************************************************************/
int WINAPI  PalEntriesOnDevice (HDC hDC)
{
	int nColors;

	nColors = GetDeviceCaps (hDC, SIZEPALETTE);
/*
......For non-palette devices, we'll use the # of system
......colors for our palette size.
*/
	if (!nColors)
		nColors = GetDeviceCaps (hDC, NUMCOLORS);

	assert (nColors);

	return nColors;
}

/*************************************************************************
**
**  Function:  GetSystemPalette
**		Get System palette
**		
**			 .
**	 INPUT:  none
**			
**   OUTPUT : none
**   RETURN:   system palette handle
**
**
*************************************************************************/
HPALETTE WINAPI  GetSystemPalette (void)
{
	HDC           hDC;
	HPALETTE      hPal = NULL;
	HANDLE        hLogPal;
	LPLOGPALETTE  lpLogPal;
	int           i, nColors;

	hDC = GetDC (NULL);
	if (!hDC)
	{
		DIBError (ERR_GETDC);
		return NULL;
	}

	nColors = PalEntriesOnDevice (hDC);
	ReleaseDC (NULL, hDC);

	hLogPal = GlobalAlloc (GHND, sizeof (LOGPALETTE) +
                           nColors * sizeof (PALETTEENTRY));

	if (!hLogPal)
	{
		DIBError (ERR_CREATEPAL);
		return NULL;
	}

	lpLogPal = (LPLOGPALETTE) GlobalLock (hLogPal);

	lpLogPal->palVersion    = PALVERSION;
	lpLogPal->palNumEntries = nColors;

	for (i = 0;  i < nColors;  i++)
	{
		lpLogPal->palPalEntry[i].peBlue  = 0;
		*((LPWORD) (&lpLogPal->palPalEntry[i].peRed)) = i;
		lpLogPal->palPalEntry[i].peFlags = PC_EXPLICIT;
	}

	hPal = CreatePalette (lpLogPal);

	GlobalUnlock (hLogPal);
	GlobalFree (hLogPal);

	return hPal;
}

HDIB WINAPI uw_cypixels_DIB(unsigned char *pixels, int width, int height, int size, int paper_size, int fit, int bcolor)
{
	if (pixels==NULL)
		return NULL;
	if ((width==0)||(height==0)||(size==0))
		return NULL;
	HPALETTE hPalette;

	HDC hScrDC, hMemDC,hMemDC2, hMemDC3, hMemDC4;
	HBITMAP hBitmap, hOldBitmap, hBitmap2, hBitmap3, hBitmap4,hOldBitmap2;
	int nX, nY, nX2, nY2;
	int nWidth, nHeight; 
	int xScrn, yScrn;
	int scale_xin, scale_yin;
	float rate;
	int marg_x, marg_y, red, green, blue;
	float size_x, size_y;
	HPALETTE hOldPal=NULL;     
	HDIB hDIB=NULL;

	hScrDC = CreateDC("DISPLAY", NULL, NULL, NULL);
	hMemDC = CreateCompatibleDC(hScrDC);
	hBitmap = CreateCompatibleBitmap(hScrDC, width, height);
	hOldBitmap = (HBITMAP)SelectObject(hMemDC, hBitmap);

	int i,j,k, r,g,b;
	COLORREF rgb;
	k=0;
	for (i=height-1; i>=0; i--)
	{
		for (j=0;j<width; j++)
		{
			r = pixels[k++];
			g = pixels[k++];
			b = pixels[k++];
			rgb = RGB(r,g,b);
			SetPixel(hMemDC, j, i, rgb);
		}
	}
/*
.....if paper size is AV, rotate graphic 90 degree
*/
/*
	if (paper_size==1)
	{
		hMemDC2 = CreateCompatibleDC(hScrDC);
		hBitmap2 = CreateCompatibleBitmap(hScrDC, height, width);
		hOldBitmap = (HBITMAP)SelectObject(hMemDC2, hBitmap2);
		for (i=0; i<height; i++)
		{
			for (j=0;j<width; j++)
			{
				rgb = GetPixel(hMemDC, j, i);
				SetPixel(hMemDC2, height-i, j, rgb);
			}
		}
	}
*/
/*
......write the bitmap to another memDC (page info)
*/
	int cxInch = 100;
	int cyInch = 100;
	marg_x = (int)(UM_drawing_size[paper_size+32][2]/2);
	marg_y = (int)(UM_drawing_size[paper_size+32][3]/2);
	size_x = (float)(UM_drawing_size[paper_size+32][0]/2.54);
	size_y = (float)(UM_drawing_size[paper_size+32][1]/2.54);
	rate = (float)width/(float)height;
	int xin, yin;
//	if (paper_size!=1)
	{
		scale_xin = (int)(cxInch * size_x - marg_x);
		scale_yin = (int)(scale_xin / rate);

		xin = (int)(cxInch * size_x);
		yin = (int)(cyInch * size_y);

		if (scale_yin > cyInch * size_y - marg_y)
		{
			scale_yin = (int)(cyInch*size_y - marg_y);
			scale_xin = (int)(scale_yin * rate);
		}
	}
/*	else
	{
		scale_yin = (int)(cxInch * size_y - marg_y);
		scale_xin = (int)(scale_yin / rate);

		xin = (int)(cxInch * size_x);
		yin = (int)(cyInch * size_y);

		if (scale_xin > cyInch * size_x - marg_x)
		{
			scale_xin = (int)(cyInch*size_x - marg_x);
			scale_yin = (int)(scale_xin * rate);
		}
	}
*/
	hMemDC3 = CreateCompatibleDC(hScrDC);	
	if (paper_size!=1)
	{
		if (fit==1)	
			hBitmap3 = CreateCompatibleBitmap(hScrDC, scale_xin+marg_x, scale_yin+marg_y);
		else
			hBitmap3 = CreateCompatibleBitmap(hScrDC, xin, yin);

		hOldBitmap = (HBITMAP)SelectObject(hMemDC3, hBitmap3);
		if (fit==1)	
		{
			BitBlt(hMemDC3, 0, 0, scale_xin+marg_x, scale_yin+marg_y, hMemDC, 0, 0, WHITENESS) ;
			SetStretchBltMode(hMemDC3, HALFTONE);
			StretchBlt(hMemDC3, marg_x/2, marg_y/2, scale_xin, scale_yin, hMemDC, 0, 0, width, height, SRCCOPY) ;
		}
		else
		{
			BitBlt(hMemDC3, 0, 0, xin, yin, hMemDC, 0, 0, WHITENESS) ;
			BitBlt(hMemDC3, marg_x/2, marg_y/2, width, height, hMemDC, 0, 0, SRCCOPY) ;
		}
	}
	else
	{
		if (fit==1)	
			hBitmap3 = CreateCompatibleBitmap(hScrDC, scale_xin+marg_x, scale_yin+marg_y);
		else
			hBitmap3 = CreateCompatibleBitmap(hScrDC, xin, yin);
		hOldBitmap = (HBITMAP)SelectObject(hMemDC3, hBitmap3);
		if (fit==1)	
		{
			BitBlt(hMemDC3, 0, 0, scale_xin+marg_x, scale_yin+marg_y, hMemDC, 0, 0, WHITENESS) ;
			SetStretchBltMode(hMemDC3,HALFTONE );
			StretchBlt(hMemDC3, marg_y/2, marg_x/2, scale_xin, scale_yin, hMemDC, 0, 0, width, height, SRCCOPY) ;
		}
		else
		{
			BitBlt(hMemDC3, 0, 0, xin, yin, hMemDC, 0, 0, WHITENESS) ;
			BitBlt(hMemDC3, marg_x/2, marg_y/2, width, height, hMemDC, 0, 0, SRCCOPY) ;
		}
	}
	hBitmap3 = (HBITMAP)SelectObject(hMemDC3, hOldBitmap);

	DeleteDC(hScrDC);
	DeleteDC(hMemDC);
	DeleteDC(hMemDC2);
	DeleteDC(hMemDC3);

	if (!hBitmap3)
		return NULL;
	hPalette = GetSystemPalette();
/* 
......convert the bitmap to a DIB 
*/
	hDIB = (HDIB)BitmapToDIB(hBitmap3, hPalette);

	DeleteObject(hBitmap);
	DeleteObject(hPalette);

	return hDIB;
}

/*************************************************************************
**
**  Function:  uw_cywin_DIB
**		Copy Window to a DIB
**		
**			 .
**	 INPUT:  hWnd: window to be copyed
**			size: print paper size
**			fit: fit flag
**			bcolor: background color
**   OUTPUT : none
**   RETURN:   DIB handle
**
**
*************************************************************************/
HDIB WINAPI  uw_cywin_DIB(HWND hWnd, int size, int fit, int bcolor)
{
	HDIB hDIB=NULL;

	if (!hWnd)
		return NULL;


	RECT rectClient;
	POINT pt1, pt2;

	GetClientRect(hWnd, &rectClient);

/* 
......convert client coords to screen coords 
*/
	pt1.x = rectClient.left;
	pt1.y = rectClient.top;
	pt2.x = rectClient.right;
	pt2.y = rectClient.bottom;
	ClientToScreen(hWnd, &pt1);
	ClientToScreen(hWnd, &pt2);
	rectClient.left   = pt1.x;
	rectClient.top    = pt1.y;
	rectClient.right  = pt2.x;
	rectClient.bottom = pt2.y;
/*  
......get the DIB of the client area by calling
......CopyScreenToDIB and passing it the client rect
*/
	hDIB = CopyScreenToDIB(&rectClient, size, fit, bcolor);

	return hDIB;
}

/*************************************************************************
**
**  Function:  CopyScreenToDIB(LPRECT lpRect, int size)
**		Copy Screen to a DIB
**		
**			 .
**	 INPUT:  lpRect: rect to copy
**				size: print paper size
**			
**   OUTPUT : none
**   RETURN:   DIB handle
**
**
*************************************************************************/
HDIB WINAPI  CopyScreenToDIB(LPRECT lpRect, int size, int fit, int bcolor)
{
	HBITMAP hBitmap;    
	HPALETTE hPalette;
	HDIB hDIB=NULL;
/*  
......get the device-dependent bitmap in lpRect by calling
......CopyScreenToBitmap and passing it the rectangle to grab
*/
	hBitmap = CopyScreenToBitmap(lpRect, size, fit, bcolor);

	if (!hBitmap)
		return NULL;
	hPalette = GetSystemPalette();
/* 
......convert the bitmap to a DIB 
*/
	hDIB = (HDIB)BitmapToDIB(hBitmap, hPalette);

	DeleteObject(hBitmap);
	DeleteObject(hPalette);

	return hDIB;
}

/*************************************************************************
**
**  Function:  CopyScreenToBitmap(LPRECT lpRect, int paper_size, int fit, int bcolor)
**		Copy Screen to a Bitmap
**		
**			 .
**	 INPUT:  lpRect: rect to copy
**			paper_size: print paper size
**			
**   OUTPUT : none
**   RETURN:   Bitmap handle
**
**
*************************************************************************/
HBITMAP WINAPI  CopyScreenToBitmap(LPRECT lpRect, int paper_size, int fit, int bcolor)
{
	HDC hScrDC, hMemDC,hMemDC2, hMemDC3, hMemDC4;
	HBITMAP hBitmap, hOldBitmap, hBitmap2, hBitmap3, hBitmap4,hOldBitmap2;
	int nX, nY, nX2, nY2;
	int nWidth, nHeight; 
	int xScrn, yScrn,i,j;
	int scale_xin, scale_yin;
	float rate;
	int marg_x, marg_y, red, green, blue;
	float size_x, size_y;
	COLORREF rgb, bkcolor;
	HPALETTE hOldPal=NULL;     

	if (IsRectEmpty(lpRect))
	return NULL;
	if (bcolor==0)
	{
		uw_get_bkcolor(&red, &green, &blue);
		bkcolor = RGB(red, green, blue);
	}
/*  
......create a DC for the screen and create
......a memory DC compatible to screen DC
*/
	hScrDC = CreateDC("DISPLAY", NULL, NULL, NULL);
	hMemDC = CreateCompatibleDC(hScrDC);
/* 
......get points of rectangle to grab 
*/
	nX = lpRect->left;
	nY = lpRect->top;
	nX2 = lpRect->right;
	nY2 = lpRect->bottom;
/* 
......get screen resolution 
*/
	xScrn = GetDeviceCaps(hScrDC, HORZRES);
	yScrn = GetDeviceCaps(hScrDC, VERTRES);

	if (nX < 0)
		nX = 0;
	if (nY < 0)
		nY = 0;
	if (nX2 > xScrn)
		nX2 = xScrn;
	if (nY2 > yScrn)
		nY2 = yScrn;
	nWidth = nX2 - nX;
	nHeight = nY2 - nY;
/* 
......create a bitmap compatible with the screen DC 
*/
	hBitmap = CreateCompatibleBitmap(hScrDC, nWidth, nHeight);
	hOldBitmap = (HBITMAP)SelectObject(hMemDC, hBitmap);

	BitBlt(hMemDC, 0, 0, nWidth, nHeight, hScrDC, nX, nY, SRCCOPY) ;

	if (fit==0)
	{
		hBitmap = (HBITMAP)SelectObject(hMemDC, hOldBitmap);
		if (bcolor==0)
		{
/*
......white background
*/
			hMemDC4 = CreateCompatibleDC(hScrDC);
			hBitmap4 = CreateCompatibleBitmap(hScrDC, nWidth, nHeight);
			hOldBitmap2 = (HBITMAP)SelectObject(hMemDC4, hBitmap4);
			BitBlt(hMemDC4, 0, 0, nWidth, nHeight, hScrDC, nX, nY, WHITENESS) ;
/*			TransparentBlt(hMemDC4, 0, 0, nWidth, 
				nHeight, hBitmap, 0, 0, 0x000000); */
			TransparentBlt(hMemDC4, 0, 0, nWidth, 
				nHeight, hBitmap, 0, 0, (UINT)bkcolor);
			
			hBitmap4 = (HBITMAP)SelectObject(hMemDC4, hOldBitmap2);
			DeleteDC(hScrDC);
			DeleteDC(hMemDC);
			DeleteDC(hMemDC4);
			return hBitmap4;
		}
		else
		{
			DeleteDC(hScrDC);
			DeleteDC(hMemDC);
			return hBitmap;
		}
	}
/*
.....if paper size is not AV, rotate graphic 90 degree
*/
	if (paper_size!=1)
	{
		hMemDC2 = CreateCompatibleDC(hScrDC);
		hBitmap2 = CreateCompatibleBitmap(hScrDC, nHeight, nWidth);
		hOldBitmap = (HBITMAP)SelectObject(hMemDC2, hBitmap2);
/*
......if the paper size is not AV, ratation 90 degree
*/
		for (i=0; i<nHeight; i++)
		{
			for (j=0;j<nWidth; j++)
			{
				rgb = GetPixel(hMemDC, j, i);
				SetPixel(hMemDC2, nHeight-i, j, rgb);
			}
		}
	}
/*
......write the bitmap to another memDC (page info)
*/
	int cxInch = 100;
	int cyInch = 100;

	marg_x = (int)(UM_drawing_size[paper_size+32][2]/2);
	marg_y = (int)(UM_drawing_size[paper_size+32][3]/2);
	size_x = (float)(UM_drawing_size[paper_size+32][0]/2.54);
	size_y = (float)(UM_drawing_size[paper_size+32][1]/2.54);

	rate = (float)nWidth/(float)nHeight;

	scale_xin = (int)(cxInch * size_x - marg_x);
	scale_yin = (int)(scale_xin / rate);
	if (scale_yin > cyInch * size_y - marg_y)
	{
		scale_yin = (int)(cyInch*size_y - marg_y);
		scale_xin = (int)(scale_yin * rate);
	}
	hMemDC3 = CreateCompatibleDC(hScrDC);	
	if (paper_size==1)
	{
		hBitmap3 = CreateCompatibleBitmap(hScrDC, scale_xin+marg_x, scale_yin+marg_y);
		hOldBitmap = (HBITMAP)SelectObject(hMemDC3, hBitmap3);
		SetStretchBltMode(hMemDC3,HALFTONE);
		StretchBlt(hMemDC3, marg_x/2, marg_y/2, scale_xin, scale_yin, hMemDC, 0, 0, nWidth, nHeight, SRCCOPY) ;
	}
	else
	{
		hBitmap3 = CreateCompatibleBitmap(hScrDC, scale_yin+marg_y, scale_xin+marg_x);
		hOldBitmap = (HBITMAP)SelectObject(hMemDC3, hBitmap3);
		SetStretchBltMode(hMemDC3,HALFTONE );
		StretchBlt(hMemDC3, marg_y/2, marg_x/2, scale_yin, scale_xin, hMemDC2, 0, 0, nHeight, nWidth, SRCCOPY) ;
	}
/*
......change color white to black and black to white
......if the paper size is AV, ratation 90 degree
*/
	if (paper_size==1)
	{
		nHeight = scale_yin+marg_y;
		nWidth = scale_xin+marg_x;
	}
	else
	{
		nHeight = scale_xin+marg_x;
		nWidth = scale_yin+marg_y;
	}

	hBitmap3 = (HBITMAP)SelectObject(hMemDC3, hOldBitmap);
	if (bcolor==0)
	{
		hMemDC4 = CreateCompatibleDC(hScrDC);
		hBitmap4 = CreateCompatibleBitmap(hScrDC, nWidth, nHeight);
		hOldBitmap2 = (HBITMAP)SelectObject(hMemDC4, hBitmap4);
		BitBlt(hMemDC4, 0, 0, nWidth, nHeight, hScrDC, nX, nY, WHITENESS) ;
/*		TransparentBlt(hMemDC4, 0, 0, nWidth, 
			nHeight, hBitmap3, 0, 0, 0x000000); */
		TransparentBlt(hMemDC4, 0, 0, nWidth, 
			nHeight, hBitmap3, 0, 0, (UINT)bkcolor);
		hBitmap4 = (HBITMAP)SelectObject(hMemDC4, hOldBitmap2);
		DeleteDC(hScrDC);
		DeleteDC(hMemDC);
		DeleteDC(hMemDC2);
		DeleteDC(hMemDC3);
		DeleteDC(hMemDC4);
		return hBitmap4;
	}
	else
	{
		DeleteDC(hScrDC);
		DeleteDC(hMemDC);
		DeleteDC(hMemDC2);
		DeleteDC(hMemDC3);
		return hBitmap3;
	}
}

static char *szErrors[] = {"Not a DIB file!",
                           "Couldn't allocate memory!",
                           "Error reading file!",
                           "Error locking memory!",
                           "Error opening file!",
                           "Error creating palette!",
                           "Error getting a DC!",
                           "Error creating MDI Child!",
                           "Error creating Device Dependent Bitmap",
                           "StretchBlt() failed!",
                           "StretchDIBits() failed!",
                           "Paint requires both DDB and DIB!",
                           "SetDIBitsToDevice() failed!",
                           "Printer: StartDoc failed!",
                           "Printing: GetModuleHandle() couldn't find GDI!",
                           "Printer: SetAbortProc failed!",
                           "Printer: StartPage failed!",
                           "Printer: NEWFRAME failed!",
                           "Printer: EndPage failed!",
                           "Printer: EndDoc failed!",
                           "Only one DIB can be animated at a time!",
                           "No timers available for animation!",
                           "Can't copy to clipboard -- no current DIB!",
                           "Clipboard is busy -- operation aborted!",
                           "Can't paste -- no DIBs or DDBs in clipboard!",
                           "SetDIBits() failed!",
                           "File Not Found!",
                           "Error writing DIB file!"
                          };

/*************************************************************************
**
**  Function:  DIBError
**		handle error message
**		
**			 .
**	 INPUT:  ErrNo: error number
**			
**			
**   OUTPUT : none
**   RETURN:   none
**
**
*************************************************************************/
void WINAPI  DIBError (int ErrNo)
{
	if ((ErrNo < ERR_MIN) || (ErrNo >= ERR_MAX))
	{
		MessageBox (GetFocus (), "Undefined Error!", NULL, MB_OK | MB_ICONHAND);
	}
	else
	{
		MessageBox (GetFocus (), szErrors[ErrNo], NULL, MB_OK | MB_ICONHAND);
	}
}
/*************************************************************************
**
**  Function:  uw_cyscrn_rgb
**		Copy screen into a RGB string
**		
**			 .
**	 INPUT:  
**			paper_size: paper size
**			wid, hgt: size of screen
**			
**   OUTPUT : opixels: output RGB string
**   RETURN:   none
**
**
*************************************************************************/
extern "C" int uw_cyscrn_rgb(unsigned char ** opixels, int paper_size, int *wid, int *hgt, int fit)
{
	RECT rectClient;
	POINT pt1, pt2;
	unsigned char * pixels;
	HWND graphic;

	if (UD_printipv)
		graphic = UW_NTgraphicIPV;
	else
		graphic = UW_NTgraphic;

	GetClientRect(graphic, &rectClient);
/* 
......convert client coords to screen coords 
*/
	pt1.x = rectClient.left;
	pt1.y = rectClient.top;
	pt2.x = rectClient.right;
	pt2.y = rectClient.bottom;
	ClientToScreen(graphic, &pt1);
	ClientToScreen(graphic, &pt2);
	rectClient.left   = pt1.x;
	rectClient.top    = pt1.y;
	rectClient.right  = pt2.x;
	rectClient.bottom = pt2.y;
	
	HDC hScrDC, hMemDC,hMemDC2, hMemDC3;
	HBITMAP hBitmap, hOldBitmap, hBitmap2, hBitmap3;
	int nX, nY, nX2, nY2;
	int nWidth, nHeight; 
	int xScrn, yScrn,i,j;
	int scale_xin, scale_yin;
	float rate;
	int marg_x, marg_y;
	float size_x, size_y;
	COLORREF rgb;

	if (IsRectEmpty(&rectClient))
	return NULL;

/*  
......create a DC for the screen and create
......a memory DC compatible to screen DC
*/
	hScrDC = CreateDC("DISPLAY", NULL, NULL, NULL);
	hMemDC = CreateCompatibleDC(hScrDC);
/* 
......get points of rectangle to grab 
*/
	nX = rectClient.left;
	nY = rectClient.top;
	nX2 = rectClient.right;
	nY2 = rectClient.bottom;
/* 
......get screen resolution 
*/
	xScrn = GetDeviceCaps(hScrDC, HORZRES);
	yScrn = GetDeviceCaps(hScrDC, VERTRES);

	if (nX < 0)
		nX = 0;
	if (nY < 0)
		nY = 0;
	if (nX2 > xScrn)
		nX2 = xScrn;
	if (nY2 > yScrn)
		nY2 = yScrn;
	nWidth = nX2 - nX;
	nHeight = nY2 - nY;
/* 
......create a bitmap compatible with the screen DC 
*/
	hBitmap = CreateCompatibleBitmap(hScrDC, nWidth, nHeight);
/* 
......select new bitmap into memory DC 
*/
	hOldBitmap = (HBITMAP)SelectObject(hMemDC, hBitmap);

	BitBlt(hMemDC, 0, 0, nWidth, nHeight, hScrDC, nX, nY, SRCCOPY) ;

	int inc = 0;
	if (fit==0)
	{
		*wid = nWidth;
		*hgt = nHeight;
		pixels = (unsigned char *)uu_malloc(3*nHeight*nWidth*sizeof(unsigned char));
		for (i=0; i<nHeight; i++)
		{
			for (j=0;j<nWidth; j++)
			{
				rgb = GetPixel(hMemDC, j, nHeight-i);
				pixels[inc++] = (unsigned char)(rgb & 0xff);
				pixels[inc++] = (unsigned char)((rgb & 0xff00) >> 8);
				pixels[inc++] = (unsigned char)((rgb & 0xff0000) >> 16);
			}
		}
		*opixels = pixels;
		return 1;
	}
/*
.....if paper size is not AV, rotate graphic 90 degree
*/
	if (paper_size!=1)
	{
		hMemDC2 = CreateCompatibleDC(hScrDC);
		hBitmap2 = CreateCompatibleBitmap(hScrDC, nHeight, nWidth);
		hOldBitmap = (HBITMAP)SelectObject(hMemDC2, hBitmap2);
/*
......if the paper size is AV, ratation 90 degree
*/
		for (i=0; i<nHeight; i++)
		{
			for (j=0;j<nWidth; j++)
			{
				rgb = GetPixel(hMemDC, j, i);
				SetPixel(hMemDC2, i, j, rgb);
			}
		}
	}
/*
......write the bitmap to another memDC (page info)
*/
	int cxInch = 100;
	int cyInch = 100;

	marg_x = (int)(UM_drawing_size[paper_size+32][2]/2);
	marg_y = (int)(UM_drawing_size[paper_size+32][3]/2);
	size_x = (float)(UM_drawing_size[paper_size+32][0]/2.54);
	size_y = (float)(UM_drawing_size[paper_size+32][1]/2.54);

	rate = (float)nWidth/(float)nHeight;

	scale_xin = (int)(cxInch * size_x - marg_x);
	scale_yin = (int)(scale_xin / rate);
	if (scale_yin > cyInch * size_y - marg_y)
	{
		scale_yin = (int)(cyInch*size_y - marg_y);
		scale_xin = (int)(scale_yin * rate);
	}

	hMemDC3 = CreateCompatibleDC(hScrDC);	
	if (paper_size==1)
	{
		hBitmap3 = CreateCompatibleBitmap(hScrDC, scale_xin+marg_x, scale_yin+marg_y);
		hOldBitmap = (HBITMAP)SelectObject(hMemDC3, hBitmap3);
		SetStretchBltMode(hMemDC3,HALFTONE);
		StretchBlt(hMemDC3, marg_x/2, marg_y/2, scale_xin, scale_yin, hMemDC, 0, 0, nWidth, nHeight, SRCCOPY) ;
	}
	else
	{
		hBitmap3 = CreateCompatibleBitmap(hScrDC, scale_yin+marg_y, scale_xin+marg_x);
		hOldBitmap = (HBITMAP)SelectObject(hMemDC3, hBitmap3);
		SetStretchBltMode(hMemDC3,HALFTONE );
		StretchBlt(hMemDC3, marg_y/2, marg_x/2, scale_yin, scale_xin, hMemDC2, 0, 0, nHeight, nWidth, SRCCOPY) ;
	}
/*
......if the paper size is AV, ratation 90 degree
*/
	if (paper_size==1)
	{
		nHeight = scale_yin+marg_y;
		nWidth = scale_xin+marg_x;
	}
	else
	{
		nHeight = scale_xin+marg_x;
		nWidth = scale_yin+marg_y;
	}
	*wid = nWidth;
	*hgt = nHeight;
	pixels = (unsigned char *)uu_malloc(3*nHeight*nWidth*sizeof(unsigned char));

	for (i=0; i<nHeight; i++)
	{
		for (j=0;j<nWidth; j++)
		{
			if (paper_size==1)
				rgb = GetPixel(hMemDC3, j, nHeight-i);
			else
				rgb = GetPixel(hMemDC3, j, i);
			pixels[inc++] = (unsigned char)(rgb & 0xff);
			pixels[inc++] = (unsigned char)((rgb & 0xff00) >> 8);
			pixels[inc++] = (unsigned char)((rgb & 0xff0000) >> 16);
		}
	}
	*opixels = pixels;
	return 1;
}
/*************************************************************************
**
**  Function:  CopyWindowToBitmap(LPRECT lpRect, CWnd* pWnd)
**		Copy Window to a Bitmap
**		
**			 .
**	 INPUT: CWnd *pWnd
**			lpRect: Window's rect
**			
**   OUTPUT : none
**   RETURN:   Bitmap handle
**
**
*************************************************************************/
HBITMAP WINAPI  CopyWindowToBitmap(LPRECT lpRect, CWnd *pWnd)
{
	HDC hMemDC,hMemDC2, hMemDC3, hMemDC4;
	HBITMAP hBitmap, hOldBitmap, hBitmap2, hBitmap3, hBitmap4,hOldBitmap2;
	int nX, nY, nX2, nY2;
	int nWidth, nHeight; 
	int xScrn, yScrn,i,j;

	CPaintDC dc(pWnd);

	hMemDC = CreateCompatibleDC(dc.m_hDC);
/* 
......get points of rectangle to grab 
*/
	nX = lpRect->left;
	nY = lpRect->top;
	nX2 = lpRect->right;
	nY2 = lpRect->bottom;
/* 
......get screen resolution 
*/
	xScrn = GetDeviceCaps(dc.m_hDC, HORZRES);
	yScrn = GetDeviceCaps(dc.m_hDC, VERTRES);

	if (nX < 0)
		nX = 0;
	if (nY < 0)
		nY = 0;
	if (nX2 > xScrn)
		nX2 = xScrn;
	if (nY2 > yScrn)
		nY2 = yScrn;

	nWidth = nX2 - nX;
	nHeight = nY2 - nY;

	hBitmap = CreateCompatibleBitmap(dc.m_hDC, nWidth, nHeight);
	hOldBitmap = (HBITMAP)SelectObject(hMemDC, hBitmap);

	BitBlt(hMemDC, 0, 0, nWidth, nHeight, dc.m_hDC, nX, nY, SRCCOPY) ;

	hBitmap = (HBITMAP)SelectObject(hMemDC, hOldBitmap);
	DeleteDC(hMemDC);
	
	return hBitmap;
}

/*************************************************************************
**
**  Function:  uw_cyscrn_pixelrgb
**		Copy screen pixels into a RGB string
**		
**			 .
**	 INPUT:  
**			paper_size: paper size
**			wid, hgt: size of screen
**			
**   OUTPUT : opixels: output RGB string
**   RETURN:   none
**
**
*************************************************************************/
extern "C" int uw_cyscrn_pixelrgb(unsigned char ** opixels, int paper_size, int *wid, int *hgt, int fit)
{
	RECT rectClient;
	POINT pt1, pt2;
	
	HDC hScrDC, hMemDC,hMemDC2, hMemDC3;
	HBITMAP hBitmap, hOldBitmap, hBitmap2, hBitmap3;
	int width, height; 
	int scale_xin, scale_yin;
	float rate;
	int marg_x, marg_y;
	float size_x, size_y;
	int i,j,k, r,g,b;
	COLORREF rgb;
	unsigned char * new_pixels;
	unsigned char * pixels = *opixels;
	
	width = *wid;
	height = *hgt;

	if (pixels==NULL)
		return NULL;
	if ((width==0)||(height==0))
		return NULL;

	HPALETTE hPalette;

	hScrDC = CreateDC("DISPLAY", NULL, NULL, NULL);
	hMemDC = CreateCompatibleDC(hScrDC);
	hBitmap = CreateCompatibleBitmap(hScrDC, width, height);
	hOldBitmap = (HBITMAP)SelectObject(hMemDC, hBitmap);

	k=0;
	for (i=height-1; i>=0; i--)
	{
		for (j=0;j<width; j++)
		{
			r = pixels[k++];
			g = pixels[k++];
			b = pixels[k++];
			rgb = RGB(r,g,b);
			SetPixel(hMemDC, j, i, rgb);
		}
	}
/*
.....if paper size is AV, rotate graphic 90 degree
*/
/*	if (paper_size==1)
	{
		hMemDC2 = CreateCompatibleDC(hScrDC);
		hBitmap2 = CreateCompatibleBitmap(hScrDC, height, width);
		hOldBitmap = (HBITMAP)SelectObject(hMemDC2, hBitmap2);
		for (i=0; i<height; i++)
		{
			for (j=0;j<width; j++)
			{
				rgb = GetPixel(hMemDC, j, i);
				SetPixel(hMemDC2, height-i, j, rgb);
			}
		}
	}
*/
/*
......write the bitmap to another memDC (page info)
*/
	int cxInch = 100;
	int cyInch = 100;
	marg_x = (int)(UM_drawing_size[paper_size+32][2]/2);
	marg_y = (int)(UM_drawing_size[paper_size+32][3]/2);
	size_x = (float)(UM_drawing_size[paper_size+32][0]/2.54);
	size_y = (float)(UM_drawing_size[paper_size+32][1]/2.54);
	rate = (float)width/(float)height;
	int xin, yin;
//	if (paper_size!=1)
	{
		scale_xin = (int)(cxInch * size_x - marg_x);
		scale_yin = (int)(scale_xin / rate);

		xin = (int)(cxInch * size_x);
		yin = (int)(cyInch * size_y);

		if (scale_yin > cyInch * size_y - marg_y)
		{
			scale_yin = (int)(cyInch*size_y - marg_y);
			scale_xin = (int)(scale_yin * rate);
		}
	}
/*	else
	{
		scale_yin = (int)(cxInch * size_y - marg_y);
		scale_xin = (int)(scale_yin / rate);

		xin = (int)(cxInch * size_x);
		yin = (int)(cyInch * size_y);

		if (scale_xin > cyInch * size_x - marg_x)
		{
			scale_xin = (int)(cyInch*size_x - marg_x);
			scale_yin = (int)(scale_xin * rate);
		}
	}
*/	hMemDC3 = CreateCompatibleDC(hScrDC);	
	if (paper_size!=1)
	{
		if (fit==1)	
			hBitmap3 = CreateCompatibleBitmap(hScrDC, scale_xin+marg_x, scale_yin+marg_y);
		else
			hBitmap3 = CreateCompatibleBitmap(hScrDC, xin, yin);

		hOldBitmap = (HBITMAP)SelectObject(hMemDC3, hBitmap3);
		if (fit==1)	
		{
			BitBlt(hMemDC3, 0, 0, scale_xin+marg_x, scale_yin+marg_y, hMemDC, 0, 0, WHITENESS) ;
			SetStretchBltMode(hMemDC3, HALFTONE);
			StretchBlt(hMemDC3, marg_x/2, marg_y/2, scale_xin, scale_yin, hMemDC, 0, 0, width, height, SRCCOPY) ;
			*hgt = scale_yin+marg_y;
			*wid = scale_xin+marg_x;
		}
		else
		{
			BitBlt(hMemDC3, 0, 0, xin, yin, hMemDC, 0, 0, WHITENESS) ;
			BitBlt(hMemDC3, marg_x/2, marg_y/2, width, height, hMemDC, 0, 0, SRCCOPY) ;
			*wid = xin;
			*hgt = yin;
		}
	}
	else
	{
		if (fit==1)	
			hBitmap3 = CreateCompatibleBitmap(hScrDC, scale_xin+marg_x, scale_yin+marg_y);
		else
			hBitmap3 = CreateCompatibleBitmap(hScrDC, xin, yin);
		hOldBitmap = (HBITMAP)SelectObject(hMemDC3, hBitmap3);
		if (fit==1)	
		{
			BitBlt(hMemDC3, 0, 0, scale_xin+marg_x, scale_yin+marg_y, hMemDC, 0, 0, WHITENESS) ;
			SetStretchBltMode(hMemDC3,HALFTONE );
			StretchBlt(hMemDC3, marg_y/2, marg_x/2, scale_xin, scale_yin, hMemDC, 0, 0, width, height, SRCCOPY);
			*hgt = scale_yin+marg_y;
			*wid = scale_xin+marg_x;
		}
		else
		{
			BitBlt(hMemDC3, 0, 0, xin, yin, hMemDC, 0, 0, WHITENESS) ;
			BitBlt(hMemDC3, marg_x/2, marg_y/2, width, height, hMemDC, 0, 0, SRCCOPY) ;
			*wid = xin;
			*hgt = yin;
		}
	}
	uu_free((char *)pixels);
	new_pixels = (unsigned char *)uu_malloc(3*(*wid)*(*hgt)*sizeof(unsigned char));
	k=0;
	for (i=*hgt-1; i>=0; i--)
	{
		for (j=0;j<*wid; j++)
		{
			rgb = GetPixel(hMemDC3, j, i);
			new_pixels[k++] = (unsigned char)(rgb & 0xff);
			new_pixels[k++] = (unsigned char)((rgb & 0xff00) >> 8);
			new_pixels[k++] = (unsigned char)((rgb & 0xff0000) >> 16);
		}
	}

	*opixels = new_pixels;
	return 1;
}
