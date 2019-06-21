/************************************************************************
**
**   FILE NAME: wsntgraphic.cpp
**
**       Description - Functions inplementation for class CNCLgraphic
**						
**    COPYRIGHT 2005 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       wsntgraphic.cpp , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:26
**
************************************************************************
*/
#include "stdafx.h"
#include "wsntgraphic.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define HIMETRIC_INCH 2540
extern void TransparentBlt( HDC hdcDest, int nXDest, int nYDest, int nWidth, 
			int nHeight, HBITMAP hBitmap, int nXSrc, int nYSrc,
			COLORREF colorTransparent);

/***********************************************************************
**
**   FUNCTION: CNCLgraphic()
**
**              Constructor of class CNCLgraphic
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLgraphic::CNCLgraphic()
{
	m_picture = NULL;
	m_height = 0;
	m_size = 0;
	m_wid = 0;
	m_hBmp = NULL;
	m_BufferBytes = NULL;
	m_memdc = NULL;
	m_trans = 0;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLgraphic()
**
**              Destructor of class CNCLgraphic
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLgraphic::~CNCLgraphic()
{
	if(m_picture != NULL) FreePictureData();
}

/***********************************************************************
**
**   FUNCTION: FreePictureData()
**
**              Free picture data memory
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLgraphic::FreePictureData()
{
	if(m_picture != NULL)
	{
		m_picture->Release();
		m_picture = NULL;
		m_height = 0;
		m_size = 0;
		m_wid = 0;
		if (m_BufferBytes!=NULL)
		{
			free(m_BufferBytes);
			m_BufferBytes = NULL;
		}
		if (m_memdc!= NULL) DeleteDC(m_memdc); 
		m_memdc = NULL;
		if (m_hBmp != NULL) DeleteObject(m_hBmp);
		m_hBmp = NULL;
	}
	m_picture = NULL;
}

/***********************************************************************
**
**   FUNCTION: Load(CString sFilePathName)
**
**              Load the picture data from picture file
**
**   INPUT:  sFilePathName: picture file name
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLgraphic::Load(CString sFilePathName)
{
	BOOL bResult = FALSE;
	CFile PictureFile;
	CFileException e;
	int	nSize = 0;

	if(m_picture != NULL) FreePictureData();

	if(PictureFile.Open(sFilePathName, CFile::modeRead | CFile::typeBinary, &e))
	{
		nSize = (int)PictureFile.GetLength();
		BYTE* pBuffer = new BYTE[nSize];
	
		if(PictureFile.Read(pBuffer, nSize) > 0)
			{
			if(LoadPictureData(pBuffer, nSize))	bResult = TRUE;
			}

		PictureFile.Close();
		delete [] pBuffer;
		}
	else
		{
/*
.....if file error, do not display message here but use default image
*/
/*		TCHAR szCause[255];
		e.GetErrorMessage(szCause, 255, NULL);
		HWND hWnd = AfxGetApp()->GetMainWnd()->m_hWnd;
		MessageBoxEx(hWnd, szCause, "CNCLgraphic Error", MB_OK | MB_ICONSTOP, LANG_ENGLISH);
*/
			bResult = FALSE;
		}

	m_size = nSize;

	if(m_picture != NULL)
		{ 
		m_picture->get_Height(&m_height);
		m_picture->get_Width(&m_wid);
	    m_height = MulDiv(m_height, 96, HIMETRIC_INCH);
	    m_wid  = MulDiv(m_wid,  96, HIMETRIC_INCH);
		}
	else 
		{
		m_height = 0;
		m_wid = 0;
		bResult = FALSE;
		}

	return(bResult);
}

/***********************************************************************
**
**   FUNCTION: LoadPictureData(BYTE *pBuffer, int nSize)
**
**              Load the picture data from picture file
**
**   INPUT:  pBuffer: data to be loaded into m_picture
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLgraphic::LoadPictureData(BYTE *pBuffer, int nSize)
{
	BOOL bResult = FALSE;

	HGLOBAL hGlobal = GlobalAlloc(GMEM_MOVEABLE, nSize);

	if(hGlobal == NULL)
		{
		HWND hWnd = AfxGetApp()->GetMainWnd()->m_hWnd;
		MessageBoxEx(hWnd, "Can not allocate enough memory\t", "CNCLgraphic Error", MB_OK | MB_ICONSTOP, LANG_ENGLISH);
		return(FALSE);
		}

	void* pData = GlobalLock(hGlobal);
	memcpy(pData, pBuffer, nSize);
	GlobalUnlock(hGlobal);

	IStream* pStream = NULL;

	if(CreateStreamOnHGlobal(hGlobal, TRUE, &pStream) == S_OK)
	{
		HRESULT hr;
		if((hr = OleLoadPicture(pStream, nSize, FALSE, IID_IPicture, (LPVOID *)&m_picture)) == E_NOINTERFACE)
		{
			HWND hWnd = AfxGetApp()->GetMainWnd()->m_hWnd;
			MessageBoxEx(hWnd, "IPicture interface is not supported\t", "CNCLgraphic Error", MB_OK | MB_ICONSTOP, LANG_ENGLISH);
			if (pStream!=NULL)
				pStream->Release();
			return(FALSE);
		}
		else 
		{
			pStream->Release();
			pStream = NULL;
			bResult = TRUE;
		}
	}

	FreeResource(hGlobal); 

	return(bResult);
}

BOOL CNCLgraphic::Show(CDC *pDC, CPoint LeftTop, CPoint WidthHeight, int MagnifyX, int MagnifyY)
{
	int status=0;
    if (pDC == NULL || m_picture == NULL) return FALSE;
    
    long Width  = 0;
    long Height = 0;
    m_picture->get_Width(&Width);
    m_picture->get_Height(&Height);
	if(MagnifyX == NULL) MagnifyX = 0;
	if(MagnifyY == NULL) MagnifyY = 0;
	MagnifyX = int(MulDiv(Width, pDC->GetDeviceCaps(LOGPIXELSX), HIMETRIC_INCH) * MagnifyX);
	MagnifyY = int(MulDiv(Height,pDC->GetDeviceCaps(LOGPIXELSY), HIMETRIC_INCH) * MagnifyY);
/*
......fit in the same ratio
*/
	int right, bottom;
	float rat = (float)Height/(float)Width;
	float rat2 = (float)WidthHeight.y/(float)WidthHeight.x;

	if (rat<rat2)
	{
		right = WidthHeight.x;
		bottom = WidthHeight.x*rat;
	}
	else
	{
		bottom = WidthHeight.y;
		right = WidthHeight.y/rat;
	}
	if (m_hBmp==NULL)
		RecreateBitmap(pDC);
	if (m_hBmp==NULL)
		return 0;
	if (m_trans)
	{
		TransparentBlt(pDC->m_hDC, 2, 2, 16, 
			16, m_hBmp, 0, 0, 0xC0C0C0);
		return 0;
	}
	if (m_memdc==NULL)
		m_memdc=::CreateCompatibleDC(pDC->m_hDC);
	::SelectObject(m_memdc, m_hBmp);
	SetStretchBltMode(pDC->m_hDC, HALFTONE);
	
	POINT pt;
	SetBrushOrgEx(pDC->m_hDC, 0, 0, &pt);
	status = StretchBlt(pDC->m_hDC, 0,0, right+1, bottom+1,  m_memdc, 0, 0, m_wid, m_height, SRCCOPY );	
	return status;
}

/***********************************************************************
c
c   FUNCTION: CreatePicBitmap()
c
c    Creates a Bitmap from the IPicture that was loaded.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    TRUE: Succeeded
c				FALSE: failed
c
**********************************************************************/
BOOL CNCLgraphic::CreatePicBitmap(CDC *pDC)
{
	BOOL bResult = FALSE;
	ILockBytes *Buffer = 0;
	IStorage   *pStorage = 0;
	IStream    *FileStream = 0;
	BYTE	   *pBufferBytes;
	STATSTG		BytesStatistics;
	DWORD		OutData;
	long		OutStream;
	CFile		BitmapFile;	CFileException e;
	double		SkipFloat = 0;
	DWORD		ByteSkip = 0;
	_ULARGE_INTEGER RealData;

	if (m_picture==NULL)
		return 0;
	CreateILockBytesOnHGlobal(NULL, TRUE, &Buffer);

	HRESULT hr = ::StgCreateDocfileOnILockBytes(Buffer,
				 STGM_SHARE_EXCLUSIVE | STGM_CREATE | STGM_READWRITE, 0, &pStorage);

	hr = pStorage->CreateStream(L"PICTURE",
		 STGM_SHARE_EXCLUSIVE | STGM_CREATE | STGM_READWRITE, 0, 0, &FileStream);

	m_picture->SaveAsFile(FileStream, TRUE, &OutStream);
	FileStream->Release();
	pStorage->Release();
	Buffer->Flush(); 

	Buffer->Stat(&BytesStatistics, STATFLAG_NONAME);

	SkipFloat = (double(OutStream) / 512);
	if ( SkipFloat > DWORD(SkipFloat) ) 
		ByteSkip = (DWORD)SkipFloat + 1;
	else 
		ByteSkip = (DWORD)SkipFloat;
	ByteSkip = ByteSkip * 512;
	
	ByteSkip = (DWORD)(BytesStatistics.cbSize.QuadPart - ByteSkip);

	RealData.LowPart = 0;
	RealData.HighPart = 0;
	RealData.QuadPart = ByteSkip;
	m_BufferBytes = (BYTE*)malloc(OutStream);
	pBufferBytes = m_BufferBytes;
	if (m_BufferBytes == NULL)
	{
		Buffer->Release();
		HWND hWnd = AfxGetApp()->GetMainWnd()->m_hWnd;
		MessageBoxEx(hWnd, "Can not allocate enough memory\t", "Error", MB_OK | MB_ICONSTOP, LANG_ENGLISH);
	}

	Buffer->ReadAt(RealData, m_BufferBytes, OutStream, &OutData);

	BITMAPFILEHEADER  bmfHeader;
	memcpy(&bmfHeader, m_BufferBytes,sizeof(bmfHeader));

	DWORD bmfHeaderSize = sizeof(BITMAPFILEHEADER);
	m_BufferBytes += bmfHeaderSize;

	BITMAPINFOHEADER &bmiHeader = *(LPBITMAPINFOHEADER)m_BufferBytes ;
	BITMAPINFO &bmInfo = *(LPBITMAPINFO)m_BufferBytes ;

	int nColors = bmiHeader.biClrUsed ? bmiHeader.biClrUsed : 
						1 << bmiHeader.biBitCount;

	LPVOID lpDIBBits;
	if ( bmInfo.bmiHeader.biBitCount > 8 )
		lpDIBBits = (LPVOID)((LPDWORD)(bmInfo.bmiColors + bmInfo.bmiHeader.biClrUsed) + 
			((bmInfo.bmiHeader.biCompression == BI_BITFIELDS) ? 3 : 0));
	else
		lpDIBBits = (LPVOID)(bmInfo.bmiColors + nColors);

	if (m_hBmp != NULL) DeleteObject(m_hBmp);
	m_hBmp = CreateDIBitmap(pDC->m_hDC,
				&bmiHeader,			
				CBM_INIT,			
				lpDIBBits,			
				&bmInfo,			
				DIB_RGB_COLORS);	

	Buffer->Release();
	m_BufferBytes = pBufferBytes;
	return TRUE;
}


void CNCLgraphic::RecreateBitmap(CDC *pDC)
{
	BYTE	   *pBufferBytes;
	pBufferBytes = m_BufferBytes;
	BITMAPFILEHEADER  bmfHeader;
	memcpy(&bmfHeader, m_BufferBytes,sizeof(bmfHeader));

	DWORD bmfHeaderSize = sizeof(BITMAPFILEHEADER);
	m_BufferBytes += bmfHeaderSize;

	BITMAPINFOHEADER &bmiHeader = *(LPBITMAPINFOHEADER)m_BufferBytes ;
	BITMAPINFO &bmInfo = *(LPBITMAPINFO)m_BufferBytes ;

	int nColors = bmiHeader.biClrUsed ? bmiHeader.biClrUsed : 
						1 << bmiHeader.biBitCount;

	LPVOID lpDIBBits;
	if ( bmInfo.bmiHeader.biBitCount > 8 )
		lpDIBBits = (LPVOID)((LPDWORD)(bmInfo.bmiColors + bmInfo.bmiHeader.biClrUsed) + 
			((bmInfo.bmiHeader.biCompression == BI_BITFIELDS) ? 3 : 0));
	else
		lpDIBBits = (LPVOID)(bmInfo.bmiColors + nColors);

	if (m_hBmp != NULL) DeleteObject(m_hBmp);
	m_hBmp = CreateDIBitmap(pDC->m_hDC,
				&bmiHeader,			
				CBM_INIT,			
				lpDIBBits,			
				&bmInfo,			
				DIB_RGB_COLORS);	
	m_BufferBytes = pBufferBytes;
}

/***********************************************************************
**
**   FUNCTION: UpdateSizeOnDC(CDC *pDC)
**
**            Update the graphic size according a device context
**
**   INPUT:  pDC: device context
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLgraphic::UpdateSizeOnDC(CDC *pDC)
{
	if(pDC == NULL || m_picture == NULL) { m_height = 0; m_wid = 0; return(FALSE); };

    m_picture->get_Height(&m_height);
    m_picture->get_Width(&m_wid);

    int CurrentDPI_X = pDC->GetDeviceCaps(LOGPIXELSX);
    int CurrentDPI_Y = pDC->GetDeviceCaps(LOGPIXELSY);

    if(pDC->IsPrinting())
	{
		CurrentDPI_X = 96;
        CurrentDPI_Y = 96;
	}

    m_height = MulDiv(m_height, CurrentDPI_Y, HIMETRIC_INCH);
    m_wid  = MulDiv(m_wid,  CurrentDPI_X, HIMETRIC_INCH);

    return(TRUE);
}
