/************************************************************************
**
**   FILE NAME: wsntmmap.h
**	 CONTAINS: Header file for CMemoryMappedFile
**				Class supports memory mapped files
**		This file is mostly copied from MMFILE.CPP from 
**		support.microsoft.com knowledge base Article ID: 143277
**		with little change to suit for NCL/NCQ
**		This file current used for NCL and NCQ
**
**     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       wsntmmap.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:20
**
************************************************************************
*/
#ifndef WSNTMMAP_H
#define WSNTMMAP_H

class CMemoryMappedFile : public CMemFile
{
public:

	CMemoryMappedFile();
	virtual ~CMemoryMappedFile();

	virtual int Open(LPCTSTR szName, 
		HANDLE hFile = INVALID_HANDLE_VALUE,
		DWORD flProtect = PAGE_READWRITE, 
		DWORD dwMaximumSizeHigh = 0, 
		DWORD dwMaximumSizeLow = 1024 * 4,
		DWORD dwNumberOfBytesToMap = 0,
		DWORD dwDesiredAccess = FILE_MAP_WRITE | FILE_MAP_READ,
		DWORD dwFileOffsetHigh = 0, 
		DWORD dwFileOffsetLow = 0);

	BOOL Attach(HANDLE *hFile, 
		DWORD dwNumberOfBytesToMap = 0,
		DWORD dwDesiredAccess = FILE_MAP_WRITE | FILE_MAP_READ, 
		DWORD dwFileOffsetHigh = 0, 
		DWORD dwFileOffsetLow = 0);

	BOOL MapViewOfFile(DWORD  dwDesiredAccess, 
		DWORD  dwFileOffsetHigh = 0, 
		DWORD  dwFileOffsetLow = 0,
		DWORD  dwNumberOfBytesToMap = 0);

	BOOL UnmapViewOfFile();

	HANDLE Detach();
	void Close(void);

	operator BYTE* () { return m_lpBuffer; } 

	virtual void Flush(DWORD startBytePosition = 0, DWORD  dwNumberOfBytesToFlush = 0);

	BOOL ReadFile(void* lpBuf, UINT nCount) { return CFile::Read(lpBuf, nCount); }
	void WriteFile(const void* lpBuf, UINT nCount) { CFile::Write(lpBuf, nCount); }

    // prevent direct buffering - undocumented function
    virtual UINT GetBufferPtr(UINT nCommand, UINT nCount,void** ppBufStart, void**ppBufMax)
        { return 0;}



protected:
    BOOL m_bAutoDelete;
	int m_ftype;
   
#if _MFC_VER < 0x400
    BYTE* BaseDetach() 
    { 	
        BYTE* lpBuffer = m_lpBuffer;
	    m_lpBuffer = NULL;
	    m_nFileSize = 0;
	    m_nBufferSize = 0;
	    m_nPosition = 0;

    	return lpBuffer;
    }
#endif

    void BaseAttach( BYTE* lpBuffer, UINT nBufferSize);


protected:
	virtual BYTE* Alloc(DWORD nBytes) { return NULL; }
	virtual BYTE* Realloc(BYTE* lpMem, DWORD nBytes) { return NULL; }
	virtual void Free(BYTE* lpMem) { ASSERT(FALSE); }
    virtual void GrowFile(DWORD dwNewLen) {
		if (m_ftype==1)
			ASSERT(FALSE);
		else
			m_nBufferSize = dwNewLen;
	}
	virtual void SetLength(DWORD dwNewLen) { ASSERT(FALSE);}


};


#if _MFC_VER < 0x400

#define BASEDETACH  CMemoryMappedFile::BaseDetach()

#define BASEATTACH(lpBuffer, nBUfferSize, nGrow)  	ASSERT(m_lpBuffer == NULL);\
    m_nGrowBytes = nGrowBytes;\
	m_nPosition = 0;\
	m_nBufferSize = nBufferSize;\
	m_nFileSize = nGrowBytes == 0 ? nBufferSize : 0;\
	m_lpBuffer = lpBuffer;

#else

#define BASEDETACH()  CMemFile::Detach()

#define BASEATTACH(lpBuffer, nBUfferSize, nGrow)   CMemFile::Attach(lpBuffer, nBUfferSize, 0)
	
#endif

#endif
