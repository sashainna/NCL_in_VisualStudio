/************************************************************************
**
**   FILE NAME: wsnthls.h
**
**       Description - Functions and struct declarations for
**              CHls class
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnthls.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:19
**********************************************************************
*/
#ifndef NCL_HUELUMSAT_H
#define NCL_HUELUMSAT_H

#pragma once

class CHls
{
public:
	enum {HLSMAX  = 1 };

private:
	double m_hue;
	double m_lum;
	double m_sat;

public:
	CHls();
	CHls(double hue, double luminance, double saturation);
	virtual ~CHls();
	double GetHue()				{return m_hue;}
	double GetLuminance()		{return m_lum;}
	double GetSaturation()		{return m_sat;}
	void SetHue(double v)		{ASSERT(v>=0&&v<=HLSMAX);m_hue = v;}
	void SetLuminance(double v)	{ASSERT(v>=0&&v<=HLSMAX);m_lum = v;}
	void SetSaturation(double v){ASSERT(v>=0&&v<=HLSMAX);m_sat = v;}
	void SetColor(double hue, double luminance, double saturation);
	COLORREF ToColorref();	
	DWORD ToRGB();
	DWORD ToARGB(int iAlpha);
};

inline DWORD CHls::ToRGB()
{
	COLORREF co = ToColorref();
	return  GetRValue(co) << 16 | GetGValue(co) << 8 | GetBValue(co);
}

inline DWORD CHls::ToARGB(int iAlpha)
{
	return iAlpha << 24 | ToRGB();
}

inline void CHls::SetColor(double hue, double luminance, double saturation)
{
	m_hue = hue;
	m_lum = luminance;
	m_sat = saturation;
}
#endif // !defined(NCL_HUELUMSAT_H)
