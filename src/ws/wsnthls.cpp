/************************************************************************
**
**   FILE NAME: wsnthls.cpp
**
**	 Description - Functions implementation for
**		CHls class and related colo converion functions
**	 CONTAINS: 
**		all functions declared in wsnthls.h
**			HLStoRGB
**			RGBtoHSL
**
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnthls.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:27
**
************************************************************************
*/
#include "wsntstdafx.h"
#include "wsnthls.h"


//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

/**********************************************************************
**    E_FUNCTION :  HuetoRGB(double m1, double m2, double h )
**       convert Hue value to RGB value
**    PARAMETERS
**       INPUT  : m1,m2,h: values to be converted
**       OUTPUT :
**          none
**    RETURNS      : color value
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
static double HuetoRGB(double m1, double m2, double h )
{
   if( h < 0 ) h += 1.0;
   if( h > 1 ) h -= 1.0;
   if( 6.0*h < 1 )
      return (m1+(m2-m1)*h*6.0);
   if( 2.0*h < 1 )
      return m2;
   if( 3.0*h < 2.0 )
      return (m1+(m2-m1)*((2.0/3.0)-h)*6.0);
   return m1;
}

/**********************************************************************
**    E_FUNCTION :  HLStoRGB( double H, double L, double S)
**       convert HLS value to RGB value
**    PARAMETERS
**       INPUT  : H: Hue values
**				L: Lum value
**				S: Sat value
**       OUTPUT :
**          none
**    RETURNS      : color value
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
inline static COLORREF HLStoRGB( double H, double L, double S )
{
  double r,g,b;
  double m1, m2;

  if(S==0) {
     r=g=b=L;
  } else {
     if(L <=0.5)
        m2 = L*(1.0+S);
     else
        m2 = L+S-L*S;
     m1 = 2.0*L-m2;
     r = HuetoRGB(m1,m2,H+1.0/3.0);
     g = HuetoRGB(m1,m2,H);
     b = HuetoRGB(m1,m2,H-1.0/3.0);
  }
  return RGB((BYTE)(r*255),(BYTE)(g*255),(BYTE)(b*255));
}

/**********************************************************************
**    E_FUNCTION :  HLStoRGB(double H, double L, double S,  int &red, int &green, int &blue)
**       convert HLS value to RGB value
**    PARAMETERS
**       INPUT  : H: Hue values
**				L: Lum value
**				S: Sat value
**       OUTPUT :
**          red, green, blue: RGB value
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
void HLStoRGB(double H, double L, double S,  int &red, int &green, int &blue)
{
  double r,g,b;
  double m1, m2;

  if(S==0) {
     r=g=b=L;
  } else {
     if(L <=0.5)
        m2 = L*(1.0+S);
     else
        m2 = L+S-L*S;
     m1 = 2.0*L-m2;
     r = HuetoRGB(m1,m2,H+1.0/3.0);
     g = HuetoRGB(m1,m2,H);
     b = HuetoRGB(m1,m2,H-1.0/3.0);
  }
	red = (int)(r*255.0);
	green = (int)(g*255.0);
	blue = (int)(b*255.0);
}
/**********************************************************************
**    E_FUNCTION :  RGBtoHSL( COLORREF rgb, double *H, double *S, double *L )
**       convert RGB value to HLS value
**    PARAMETERS
**       INPUT  : 
**          rgb: RGB value
**       OUTPUT : H: Hue values
**				L: Lum value
**				S: Sat value
**    RETURNS      : None
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
void RGBtoHSL( COLORREF rgb, double *H, double *S, double *L )
{   
	double delta;
	double r = (double)GetRValue(rgb)/255;
	double g = (double)GetGValue(rgb)/255;
	double b = (double)GetBValue(rgb)/255;   
	double cmax = max(r,max(g,b));
	double cmin = min(r,min(g,b));   
	*L=(cmax+cmin)/2.0;   
	
	if(cmax==cmin) 
	{
      *S = 0;      
	  *H = 0; // it's really undefined   
	} else 
	{
      if(*L < 0.5) 
		  *S = (cmax-cmin)/(cmax+cmin);      
	  else
         *S = (cmax-cmin)/(2.0-cmax-cmin);      
	  
	  delta = cmax - cmin;
      if(r==cmax) 
		  *H = (g-b)/delta;      
	  else if(g==cmax)
          *H = 2.0 +(b-r)/delta;
      else          
		  *H=4.0+(r-g)/delta;
      *H /= 6.0; 
	  if(*H < 0.0)
          *H += 1;  
	}
}

CHls::CHls()
{
	SetColor(0, 0, 0);
}

CHls::CHls(double hue, double luminance, double saturation)
{
	SetColor(hue, luminance, saturation);
}

CHls::~CHls()
{
}

COLORREF CHls::ToColorref()
{
	return HLStoRGB(m_hue, m_lum, m_sat);
}

