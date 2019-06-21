#include "usysdef.h"

/********************************************************************* 
**  NAME:  wsntbitmap.cpp
**
**       Bitmap functions implementation
**
**  CONTAINS: 
**
**       HBITMAP uw_get_bitmap(LPSTR filename, HDC hDC, int flag)
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       wsntbitmap.cpp , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:19
*********************************************************************/

#include "wsntstdafx.h"
#include "wsntbitmap.h"
#include <io.h>
#include <errno.h>
#include "wsntcfunc.h"
#include "xenv1.h"

static HBITMAP CreateDIBitmap(HDC hDC, HDIB hDIB);
static HDIB WINAPI  ReadDIBFile(CFile& file);
WORD WINAPI  DIBNumColors (LPSTR lpbi);
DWORD  WINAPI  DIBWidth (LPSTR lpDIB);
DWORD  WINAPI  DIBHeight (LPSTR lpDIB);
WORD WINAPI  PaletteSize (LPSTR lpbi);
LPSTR  WINAPI  FindDIBBits (LPSTR lpbi);


#define DIB_HEADER_MARKER   ((WORD) ('M' << 8) | 'B')
extern "C" int UW_icon_size;
extern "C" int ul_open_mod_file(char*, char*, char*, char*, char*, int, FILE**);

/***********************************************************************
**
**   FUNCTION: uw_get_bitmap(LPSTR fname, HDC hDC, int flag)
**              Read *.bmp file and return a bitmap handler
**   
**       INPUT:  HDC hDC: device context handler
**                       fname: bitmap file
**                       flag: 0: use size 16x16 bitmap
**                                      1: use size24x24 bitmap
**                                      2: use size 32x32 bitmap
**                                      3: use size 40x40 bitmap
**                                      4: use size 48x48 bitmap
**                                      -1: depend on UW_icon_size
**                                      -2: for popup/pulldown menu, depend on UW_icon_size
**
**   OUTPUT :   HBITMAP bitmap handler
**   RETURN:    None
**
**********************************************************************/
HBITMAP uw_get_bitmap(LPSTR fname, HDC hDC,int flag)
{
	UX_pathname filen;
	HDIB hDIB;
	CFile file;
	CFileException fe;
	char *p;
	int status;
/*
.....first open the bitmap file
*/
	strcpy_s(filen, UX_MAX_PATH_LEN, fname);
/*
.....consider the ICON_SIZE we used to
.....open diferent file
*/
	if (((UW_icon_size==0)&&(flag==-1))
		||(((UW_icon_size==0)||(UW_icon_size==1)||(UW_icon_size==2))&&(flag==-2))
		||(flag==0))
	{
		p = (char *)strchr(filen,'.');
		if (p != 0 && p != filen)
		{
			*p = '\0';
			strcat_s(filen, "_16.bmp");
		}
		else
			strcat_s(filen, "_16");
	}
	else if ((((UW_icon_size==1)&&(flag==-1))||(flag==1))
		||(((UW_icon_size==3)||(UW_icon_size==4))&&(flag==-2)))
	{
		p = (char *)strchr(filen,'.');
		if (p != 0 && p != filen)
		{
			*p = '\0';
			strcat_s(filen, "_24.bmp");
		}
		else
			strcat_s(filen, "_24");
	}
	else if (((UW_icon_size==2)&&(flag==-1))||(flag==2))
	{
		p = (char *)strchr(filen,'.');
		if (p != 0 && p != filen)
		{
			*p = '\0';
			strcat_s(filen, "_32.bmp");
		}
		else
			strcat_s(filen, "_32");
	}
	else if (((UW_icon_size==3)&&(flag==-1))||(flag==3))
	{
		p = (char *)strchr(filen,'.');
		if (p != 0 && p != filen)
		{
			*p = '\0';
			strcat_s(filen, "_40.bmp");
		}
		else
			strcat_s(filen, "_40");
	}
	else if (((UW_icon_size==4)&&(flag==-1))||(flag==4))
	{
		p = (char *)strchr(filen,'.');
		if (p != 0 && p != filen)
		{
			*p = '\0';
			strcat_s(filen, "_48.bmp");
		}
		else
			strcat_s(filen, "_48");
	}
/*
.....use UU_USER_SETTINGS/bitmaps directory first, 
.....if it is not there, use NCL_BITMAP 
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "bitmaps", "NCL_BITMAP", (char*)UU_NULL, 
		filen, 0, (FILE**)UU_NULL);
	if (status!=0)
		return NULL;
	if (!file.Open(filen, CFile::modeRead | CFile::shareDenyWrite, &fe))
	{
		return NULL;
	}
	TRY
	{
		hDIB = ::ReadDIBFile(file);
	}
	CATCH (CFileException, eLoad)
	{
		file.Abort(); 
		return NULL;
	}
	END_CATCH
	HBITMAP bitm = CreateDIBitmap(hDC, hDIB);
	return bitm;
}


/*************************************************************************
**
**  Function:  ReadDIBFile (CFile&)
**
**              Reads in the specified DIB file into a global chunk of
**                       memory.
**       INPUT:  CFile& file: DIB file
**   OUTPUT :
**   RETURN:   A handle to a dib (hDIB) if successful.
**                              NULL if an error occurs.
**
**   Comments:  BITMAPFILEHEADER is stripped off of the DIB.  Everything
**                       from the end of the BITMAPFILEHEADER structure on is
**                       returned in the global memory handle.
**
*************************************************************************/
HDIB WINAPI ReadDIBFile(CFile& file)
{
	BITMAPFILEHEADER bmfHeader;
	DWORD dwBitsSize;
	HDIB hDIB;
	LPSTR pDIB;

/*
......get length of DIB in bytes for use when reading
*/
	dwBitsSize = (DWORD) file.GetLength();
/*
......Go read the DIB file header and check if it's valid.
*/
	if (file.Read((LPSTR)&bmfHeader, sizeof(bmfHeader)) != sizeof(bmfHeader))
		return NULL;

	if (bmfHeader.bfType != DIB_HEADER_MARKER)
		return NULL;
/*
......Allocate memory for DIB
*/
	hDIB = (HDIB) ::GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, dwBitsSize);
	if (hDIB == 0)
	{
		return NULL;
	}
	pDIB = (LPSTR) ::GlobalLock((HGLOBAL) hDIB);
/*
......Read the bits.
*/
	if (file.Read(pDIB, dwBitsSize - sizeof(BITMAPFILEHEADER)) !=
		dwBitsSize - sizeof(BITMAPFILEHEADER) )
	{
		::GlobalUnlock((HGLOBAL) hDIB);
		::GlobalFree((HGLOBAL) hDIB);
		return NULL;
	}
	::GlobalUnlock((HGLOBAL) hDIB);
	return hDIB;
}


/***********************************************************************
**
**   FUNCTION: CreateDIBitmap(HDC hDC, HDIB hDIB)
**              Create a device independent bitmap handler 
**   
**       INPUT:  HDC hDC: device context handler
**                       HDIB hDIB
**
**   OUTPUT :   HBITMAP bitmap handler
**   RETURN:    None
**
**********************************************************************/
HBITMAP CreateDIBitmap(HDC hDC, HDIB hDIB)
{
	LPSTR    lpDIBHdr;            // Pointer to BITMAPINFOHEADER
	LPSTR    lpDIBBits;           // Pointer to DIB bits
	BOOL     bSuccess=FALSE;      // Success/fail flag
	HPALETTE hPal=NULL;           // Our DIB's palette

	if (hDIB == NULL)
		return FALSE;

/* 
......Lock down the DIB, and get a pointer to the beginning 
......of the bit buffer
*/
	lpDIBHdr  = (LPSTR) ::GlobalLock((HGLOBAL) hDIB);
	lpDIBBits = ::FindDIBBits(lpDIBHdr);

	HBITMAP newbitmap = CreateDIBitmap(hDC,
		(LPBITMAPINFOHEADER)lpDIBHdr,
		CBM_INIT, 
		lpDIBBits,
		(LPBITMAPINFO)lpDIBHdr, 
		DIB_RGB_COLORS);
   ::GlobalUnlock((HGLOBAL) hDIB);
 
	return newbitmap;
}


/***********************************************************************
**
**   FUNCTION:  FindDIBBits(LPSTR lpbi)
**              This function calculates the address of the DIB's bits 
**                      and returns a pointer to the DIB bits.
**   
**       INPUT:  LPSTR lpbi
**
**   OUTPUT :   a pointer to the DIB bits.
**   RETURN:    None
**
**********************************************************************/
LPSTR WINAPI FindDIBBits(LPSTR lpbi)
{
	return (lpbi + *(LPDWORD)lpbi + ::PaletteSize(lpbi));
}


/***********************************************************************
**
**   FUNCTION:  DIBWidth(LPSTR lpDIB)
**              This function gets the width of the DIB from the BITMAPINFOHEADER
**              width field if it is a Windows 3.0-style DIB or from the BITMAPCOREHEADER
**              width field if it is an other-style DIB.
**   
**       INPUT:  LPSTR lpDIB: pointer to packed-DIB memory block
**
**   OUTPUT :   DWORD: which contains width of the DIB
**   RETURN:    None
**
**********************************************************************/
DWORD WINAPI DIBWidth(LPSTR lpDIB)
{
	LPBITMAPINFOHEADER lpbmi;  // pointer to a Win 3.0-style DIB
	LPBITMAPCOREHEADER lpbmc;  // pointer to an other-style DIB

	/* point to the header (whether Win 3.0 and old) */

	lpbmi = (LPBITMAPINFOHEADER)lpDIB;
	lpbmc = (LPBITMAPCOREHEADER)lpDIB;

	/* return the DIB width if it is a Win 3.0 DIB */
	if (IS_WIN30_DIB(lpDIB))
		return lpbmi->biWidth;
	else  /* it is an other-style DIB, so return its width */
		return (DWORD)lpbmc->bcWidth;
}
/***********************************************************************
**
**   FUNCTION:  DIBWidth(LPSTR lpDIB)
**              This function gets the height of the DIB from the BITMAPINFOHEADER
**              height field if it is a Windows 3.0-style DIB or from the BITMAPCOREHEADER
**              height field if it is an other-style DIB.
**   
**       INPUT:  LPSTR lpDIB: pointer to packed-DIB memory block
**
**   OUTPUT :   DWORD            - height of the DIB
**   RETURN:    None
**
**********************************************************************/

DWORD WINAPI DIBHeight(LPSTR lpDIB)
{
	LPBITMAPINFOHEADER lpbmi;  // pointer to a Win 3.0-style DIB
	LPBITMAPCOREHEADER lpbmc;  // pointer to an other-style DIB

	lpbmi = (LPBITMAPINFOHEADER)lpDIB;
	lpbmc = (LPBITMAPCOREHEADER)lpDIB;

	/* return the DIB height if it is a Win 3.0 DIB */
	if (IS_WIN30_DIB(lpDIB))
		return lpbmi->biHeight;
	else  
		return (DWORD)lpbmc->bcHeight;
}

/***********************************************************************
**
**   FUNCTION:  PaletteSize(LPSTR lpbi)
**              This function gets the size required to store the DIB's palette by
**              multiplying the number of colors by the size of an RGBQUAD (for a
**              Windows 3.0-style DIB) or by the size of an RGBTRIPLE (for an other-
**              style DIB).
**   
**       INPUT:  LPSTR lpDIB: pointer to packed-DIB memory block
**
**   OUTPUT :   WORD             - size of the color palette of the DIB
**   RETURN:    None
**
**********************************************************************/

WORD WINAPI PaletteSize(LPSTR lpbi)
{
   if (IS_WIN30_DIB (lpbi))
	  return (WORD)(::DIBNumColors(lpbi) * sizeof(RGBQUAD));
   else
	  return (WORD)(::DIBNumColors(lpbi) * sizeof(RGBTRIPLE));
}

/***********************************************************************
**
**   FUNCTION:  DIBNumColors(LPSTR lpbi)
**              This function calculates the number of colors in the DIB's color table
**              by finding the bits per pixel for the DIB (whether Win3.0 or other-style
**              DIB). If bits per pixel is 1: colors=2, if 4: colors=16, if 8: colors=256,
**              if 24, no colors in color table.
**   
**       INPUT:  LPSTR lpDIB: pointer to packed-DIB memory block
**
**   OUTPUT :   WORD   - number of colors in the color table
 *
**   RETURN:    None
**
**********************************************************************/
WORD WINAPI DIBNumColors(LPSTR lpbi)
{
	WORD wBitCount;  // DIB bit count

	/*  If this is a Windows-style DIB, the number of colors in the
	 *  color table can be less than the number of bits per pixel
	 *  allows for (i.e. lpbi->biClrUsed can be set to some value).
	 *  If this is the case, return the appropriate value.
	 */

	if (IS_WIN30_DIB(lpbi))
	{
		DWORD dwClrUsed;

		dwClrUsed = ((LPBITMAPINFOHEADER)lpbi)->biClrUsed;
		if (dwClrUsed != 0)
			return (WORD)dwClrUsed;
	}

	/*  Calculate the number of colors in the color table based on
	 *  the number of bits per pixel for the DIB.
	 */
	if (IS_WIN30_DIB(lpbi))
		wBitCount = ((LPBITMAPINFOHEADER)lpbi)->biBitCount;
	else
		wBitCount = ((LPBITMAPCOREHEADER)lpbi)->bcBitCount;

	/* return number of colors based on bits per pixel */
	switch (wBitCount)
	{
		case 1:
			return 2;

		case 4:
			return 16;

		case 8:
			return 256;

		default:
			return 0;
	}
}


