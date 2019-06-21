/************************************************************************
**
**   FILE NAME: wsntmmap.cpp
**	 CONTAINS: Implementation file for CMemoryMappedFile
**				Class supports memory mapped files
**		This file is mostly copied from MMFILE.CPP from 
**		support.microsoft.com knowledge base Article ID: 143277
**		with little change to suit for NCL/NCQ
**		This file current used for NCL and NCQ
**
**     COPYRIGHT 2005 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       wsntmmap.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:29
**
************************************************************************
*/
#include "stdafx.h"
#include "wsntmmap.h"


/***********************************************************************
**
**   SUBROUTINE: CMemoryMappedFile()
**
**   FUNCTION:  Constructor of class CMemoryMappedFile
**
**   INPUT:  none
**
**   OUTPUT: none
**
***********************************************************************/
CMemoryMappedFile::CMemoryMappedFile()
{
	m_hFile = CFile::hFileNull;
	m_bAutoDelete = TRUE;
	m_ftype = 0;
}

/***********************************************************************
**
**   SUBROUTINE: ~CMemoryMappedFile()
**
**   FUNCTION:  Destructor of class CMemoryMappedFile
**
**   INPUT:  none
**
**   OUTPUT: none
**
***********************************************************************/
CMemoryMappedFile::~CMemoryMappedFile()
{
	if (m_hFile)
		Close();
}


/***********************************************************************
**
**   SUBROUTINE: Open 
**
**   FUNCTION:  open the memory block
**
**   INPUT:  szName: name of the memory block
**			hFile: Handle to an open handle of a file-mapping object
**			flProtect: protection for mapping object
**			dwMaximumSizeHigh: high-order 32 bits of object size
**			dwMaximumSizeLow: low-order 32 bits of object size
**			dwNumberOfBytesToMap: number of bytes to map
**			dwDesiredAccess: Specifies the type of access to the memory block	
**			dwFileOffsetHigh: Specifies the high-order 32 bits of the file offset where mapping is to begin
**			dwFileOffsetLow: Specifies the low-order 32 bits of the file offset where mapping is to begin
**			default value as show comment after the value
**   OUTPUT: none
**		RETURN: 0: Unable to open shared file/memory
**				1: Open the shared file/memory
**				-1: shared file/memory already exist
**
***********************************************************************/
int CMemoryMappedFile::Open(LPCTSTR szName, 
	HANDLE hFile				/* = INVALID_HANDLE_VALUE */,
	DWORD  flProtect			/* = PAGE_READWRITE */, 
	DWORD  dwMaximumSizeHigh	/* = 0*/, 
	DWORD  dwMaximumSizeLow		/* = 1024 * 4*/,
	DWORD  dwNumberOfBytesToMap /* = 0 */,
	DWORD  dwDesiredAccess		/* = FILE_MAP_WRITE | FILE_MAP_READ */, 
	DWORD  dwFileOffsetHigh		/* = 0*/, 
	DWORD  dwFileOffsetLow		/* = 0*/)
{
	int stat = 1;
	ASSERT(m_hFile == CFile::hFileNull);

	m_hFile = (HANDLE) CreateFileMapping(hFile,
		NULL,
		flProtect,
		dwMaximumSizeHigh,
		dwMaximumSizeLow,
		szName);
	int err = GetLastError();
	m_ftype = 1;
	if (!m_hFile)
	{
		AfxMessageBox("Unable to open shared file\n");
		return 0;
	}
	else if (err==ERROR_ALREADY_EXISTS)
	{
/*
......memory already exist
*/
		m_ftype = 0;
		stat = -1;
	}
	if (!MapViewOfFile(dwDesiredAccess, dwFileOffsetHigh, dwFileOffsetLow, dwMaximumSizeLow))
		return 0;
		
	return stat;
}

/***********************************************************************
**
**   SUBROUTINE: Attach()
**
**   FUNCTION:  Call this function to attach a block of memory to CMemoryMappedFile. 
**
**   INPUT:  
**			hFile: Handle to an open handle of a file-mapping object
**			dwNumberOfBytesToMap: number of bytes to map
**			dwDesiredAccess: Specifies the type of access to the memory block	
**			dwFileOffsetHigh: Specifies the high-order 32 bits of the file offset where mapping is to begin
**			dwFileOffsetLow: Specifies the low-order 32 bits of the file offset where mapping is to begin
**
**   OUTPUT: none
**	 RETURN: If the function succeeds, the return value is the starting address of the mapped view
**			If the function fails, the return value is NULL
**
***********************************************************************/
BOOL CMemoryMappedFile::Attach(HANDLE *hFile, 
	DWORD  dwNumberOfBytesToMap /* = 0 */,
	DWORD  dwDesiredAccess		/* = FILE_MAP_WRITE | FILE_MAP_READ */, 
	DWORD  dwFileOffsetHigh		/* = 0*/, 
	DWORD  dwFileOffsetLow		/* = 0*/)
{
	ASSERT(m_hFile == CFile::hFileNull);
	ASSERT(hFile != INVALID_HANDLE_VALUE);

	m_hFile = (HANDLE) hFile;
	return MapViewOfFile(dwDesiredAccess, 
		dwFileOffsetHigh, 
		dwFileOffsetLow,
		dwNumberOfBytesToMap);
}


/***********************************************************************
**
**   SUBROUTINE: MapViewOfFile()
**
**   FUNCTION:  maps a view of a file into the address space of the calling process
**
**   INPUT:  
**			dwDesiredAccess: Specifies the type of access to the memory block	
**			dwFileOffsetHigh: Specifies the high-order 32 bits of the file offset where mapping is to begin
**			dwFileOffsetLow: Specifies the low-order 32 bits of the file offset where mapping is to begin
**			dwNumberOfBytesToMap: number of bytes to map
**
**   OUTPUT: none
**	 RETURN: 0: Unable to map view of a file
**				1: map view of a file
**
***********************************************************************/
BOOL CMemoryMappedFile::MapViewOfFile(DWORD  dwDesiredAccess, 
		DWORD  dwFileOffsetHigh		/*= 0*/, 
		DWORD  dwFileOffsetLow		/*= 0*/,
		DWORD  dwNumberOfBytesToMap /*= 0*/)
{
	if (!UnmapViewOfFile())
		return FALSE;

	BYTE *pBuffer = (BYTE* ) ::MapViewOfFile((HANDLE) m_hFile,
		dwDesiredAccess,
		dwFileOffsetHigh, 
		dwFileOffsetLow,
		dwNumberOfBytesToMap);

	if (!pBuffer)
	{
		DWORD err = GetLastError();
		AfxMessageBox("Unable to map view of a file");
		return FALSE;
	}
	BASEATTACH(pBuffer, dwNumberOfBytesToMap, 0);
	m_bAutoDelete = TRUE;

	return TRUE;
}

/***********************************************************************
**
**   SUBROUTINE: UnmapViewOfFile()
**
**   FUNCTION:  Unmaps a view of a file from the address space of the calling process
**
**   INPUT:  none
**
**   OUTPUT: none
**	 RETURN: 0: Unable to unmap view of a file
**				1: unmap view of a file
**
***********************************************************************/
BOOL CMemoryMappedFile::UnmapViewOfFile()
{
	BYTE *p = BASEDETACH();

	if (!p)
		return TRUE;

	if (!::UnmapViewOfFile(p))
	{
		AfxMessageBox("Unable to Unmap View");
		return FALSE;
	}

	return TRUE;
}

	
/***********************************************************************
**
**   SUBROUTINE: Detach()
**
**   FUNCTION:  Call this function to dettach a block of memory to CMemoryMappedFile.
**
**   INPUT:  none
**
**   OUTPUT: none
**
***********************************************************************/
HANDLE CMemoryMappedFile::Detach()
{
	HANDLE hTemp = (HANDLE)m_hFile;
	m_bAutoDelete = FALSE;
	Close();
	m_hFile = CFile::hFileNull;
	m_bAutoDelete = TRUE;
	
	return hTemp;
}


/***********************************************************************
**
**   SUBROUTINE: Close()
**
**   FUNCTION:  Call this function to close memory mapping file.
**
**   INPUT:  none
**
**   OUTPUT: none
**
***********************************************************************/
void CMemoryMappedFile::Close(void)
{
	BOOL bAutoDelete = m_bAutoDelete;
	if (m_hFile)
	{
		UnmapViewOfFile();
        BASEDETACH();
    }

	if (bAutoDelete && m_hFile)
	{
		CloseHandle((HANDLE)m_hFile);
		m_hFile = CFile::hFileNull;
	}
}

/***********************************************************************
**
**   SUBROUTINE: Flush()
**
**   FUNCTION:  writes to the disk (memory block) a byte range within a mapped view of a file. 
**
**   INPUT:  startBytePosition: Pointer to the base address of the byte range to be flushed
**				dwNumberOfBytesToFlush: Specifies the number of bytes to flush. 
**
**   OUTPUT: none
**
***********************************************************************/
void CMemoryMappedFile::Flush(DWORD startBytePosition /* = 0 */, DWORD  dwNumberOfBytesToFlush /* = 0 */)
{
	if (m_lpBuffer)
	{
		if (startBytePosition < GetLength() / sizeof(BYTE))
			FlushViewOfFile(m_lpBuffer + startBytePosition, dwNumberOfBytesToFlush);
	}
}


