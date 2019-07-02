/************************************************************************
**
**   FILE NAME: wsntgraphic.h
**
**       Description - Functions and data declarations for class CNCLgraphic
**						handle the picture/graphic data
**              
**    COPYRIGHT 2005 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       wsntgraphic.h , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:19
**
************************************************************************
*/
#ifndef  WSNTGRAPHIC_H
#define WSNTGRAPHIC_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CNCLgraphic
{
public:
	void FreePictureData();
	BOOL Load(CString sFilePathName);
	BOOL LoadPictureData(BYTE* pBuffer, int nSize);
	BOOL Show(CDC* pDC, CPoint LeftTop, CPoint WidthHeight, int MagnifyX, int MagnifyY);
	BOOL UpdateSizeOnDC(CDC* pDC);

	CNCLgraphic();
	virtual ~CNCLgraphic();

	IPicture* m_picture;
	HBITMAP m_hBmp;
	BYTE* m_BufferBytes;
	HDC m_memdc;

	LONG      m_height; 
	LONG      m_size;
	LONG      m_wid;  
	int m_trans;
	BOOL CreatePicBitmap(CDC *pDC);
	void RecreateBitmap(CDC *pDC);
};

#endif 
