/*********************************************************************
**  NAME:  wsimgcnv.c
**
**    CONTAINS:
**			uw_create_jpg(jpgfile, bcolor) 
**			and all other static functions for create JPEG file
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsimgcnv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:09
*********************************************************************/
#include "usysdef.h"
#include <stdio.h>
#include <math.h>

#include "wsjpgimg.h"
#include "driver.h"

extern int UD_printipv;

static JpegEncoder jpeg;
static BitmapImage image ;
static FILE *jpg_fptr;

#ifndef M_PI
#define M_PI acos (-1.0)
#endif

typedef double MATRIX [JpegSampleWidth][JpegSampleWidth] ;

/*
......These are sample quantization tables defined in the the JPEG Standard.
*/
static unsigned int default_luminance_table [JpegSampleSize]  =
{
16, 11, 10, 16,  24,  40,  51,  61,
12, 12, 14, 19,  26,  58,  60,  55,
14, 13, 16, 24,  40,  57,  69,  56,
14, 17, 22, 29,  51,  87,  80,  62,
18, 22, 37, 56,  68, 109, 103,  77,
24, 35, 55, 64,  81, 104, 113,  92,
49, 64, 78, 87, 103, 121, 120, 101,
72, 92, 95, 98, 112, 100, 103,  99,
} ;

static unsigned int default_chrominance_table [JpegSampleSize] =
{
17, 18, 24, 47, 99, 99, 99, 99,
18, 21, 26, 66, 99, 99, 99, 99,
24, 26, 56, 99, 99, 99, 99, 99,
47, 66, 99, 99, 99, 99, 99, 99,
99, 99, 99, 99, 99, 99, 99, 99,
99, 99, 99, 99, 99, 99, 99, 99,
99, 99, 99, 99, 99, 99, 99, 99,
99, 99, 99, 99, 99, 99, 99, 99,
} ;

MATRIX DctMatrix ;
MATRIX IDctMatrix ;

static int Initialized = 0;

int ScaleFactor = 12 ;
int ScaleValue = (1<<12) ;
int Rounding = (1<<11) ;

unsigned int JpegZigZagInputOrderCodes [JpegSampleSize] =
{
   0,  1,  8, 16,  9,  2,  3, 10,
  17, 24, 32, 25, 18, 11,  4,  5,
  12, 19, 26, 33, 40, 48, 41, 34,
  27, 20, 13,  6,  7, 14, 21, 28,
  35, 42, 49, 56, 57, 50, 43, 36,
  29, 22, 15, 23, 30, 37, 44, 51,
  58, 59, 52, 45, 38, 31, 39, 46,
  53, 60, 61, 54, 47, 55, 62, 63
} ;

unsigned int JpegZigZagOutputOrderCodes [JpegSampleSize] =
{
   0,  1,  5,  6, 14, 15, 27, 28,
   2,  4,  7, 13, 16, 26, 29, 42,
   3,  8, 12, 17, 25, 30, 41, 43,
   9, 11, 18, 24, 31, 40, 44, 53,
  10, 19, 23, 32, 39, 45, 52, 54,
  20, 21, 33, 38, 46, 51, 55, 60,
  21, 34, 37, 47, 50, 56, 59, 61,
  35, 36, 48, 49, 57, 58, 62, 63
} ;

static void image_GetRGB () ;


/********************************************************************
**    I_FUNCTION     :  SystemAdjustValue (UBYTE2 value)
**             Adjust value depend on machine. when write to the JPEG file
**			the binary write will different on diff system
**       
**
**    PARAMETERS
**       INPUT  :
**               value: value to be adjust
**                                                  
**       OUTPUT :
**               none 
**    RETURNS      : adjusted value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if UU_COMP!=UU_WIN2K
static UBYTE2 SystemAdjustValue (value)
UBYTE2 value;
{
	return value ;
}
#else
static UBYTE2 SystemAdjustValue (value)
UBYTE2 value;
{
	return (UBYTE2) ((value&0xFF) << 8)|((value&0xFF00)>>8) ;
}
#endif

/********************************************************************
**    I_FUNCTION     :  FlushBitBuffer
**        This function flushes the bit buffer
**			This causes buffered bit streams
**			of less than the bit buffer size to be written out.
**
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void FlushBitBuffer ()
{
	if (jpeg.bit_count != 0)
	{
		jpeg.bit_buffer <<= (8 - jpeg.bit_count) ;
		fwrite ((char *) &jpeg.bit_buffer, 1, 1, jpg_fptr);
		jpeg.bit_count = 0 ;
	}
	return ;
}

static int Scaled (value)
double value;
{
	return (int) (ScaleValue * value) ;
}

/********************************************************************
**    I_FUNCTION     :  jpg_ResetDcValues ()
**        This function resets the DC difference values for all
**		all the components in the scan.  We do this at the start of
**		each scan and whenever we output a restart marker.
**
**    PARAMETERS
**       INPUT  :
**					none
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void jpg_ResetDcValues ()
{
	unsigned int ii;
	for (ii = 0 ; ii < jpeg.scan_component_count ; ++ ii)
		(jpeg.scan_components [ii])->last_dc_value = 0;
	return ;
}
/********************************************************************
**    I_FUNCTION     :  YCbCrToR (yy, cb, cr)
**             convert YCbCr color value to R (red) value
**       
**
**    PARAMETERS
**       INPUT  :
**               yy, cb, cr: YCbCr color value
**                                      
**       OUTPUT :
**               none 
**    RETURNS      : R value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static JPEGSAMPLE YCbCrToR (yy, cb, cr)
int yy, cb, cr;
{
	int result = yy + ((Scaled (1.402) * (cr - JpegMidpointSampleValue) +
				Rounding) >> ScaleFactor) ;
	if (result < 0)
		result = 0 ;
	else if (result > JpegMaxSampleValue)
		result = JpegMaxSampleValue ;
	return (JPEGSAMPLE) result ;
}

/********************************************************************
**    I_FUNCTION     :  YCbCrToG (yy, cb, cr)
**             convert YCbCr color value to G (green) value
**       
**
**    PARAMETERS
**       INPUT  :
**               yy, cb, cr: YCbCr color value
**                                      
**       OUTPUT :
**               none 
**    RETURNS      : G value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static JPEGSAMPLE YCbCrToG (yy, cb, cr)
int yy, cb, cr;
{
	int result = yy - ((Scaled (0.34414) * (cb - JpegMidpointSampleValue)
					+ Scaled (0.71414) * (cr - JpegMidpointSampleValue)
					+ Rounding) >> ScaleFactor) ;

	if (result < 0)
		result = 0 ;
	else if (result > JpegMaxSampleValue)
		result = JpegMaxSampleValue ;
	return (JPEGSAMPLE) result ;
}

/********************************************************************
**    I_FUNCTION     :  YCbCrToB (yy, cb, cr)
**             convert YCbCr color value to B (blue) value
**       
**
**    PARAMETERS
**       INPUT  :
**               yy, cb, cr: YCbCr color value
**                                              
**       OUTPUT :
**               none 
**    RETURNS      : B value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static JPEGSAMPLE YCbCrToB (yy, cb, cr)
int yy, cb, cr;
{
	int result = yy + ((Scaled (1.772) * (cb - JpegMidpointSampleValue)
					+ Rounding) >> ScaleFactor) ;
	if (result < 0)
		result = 0 ;
	else if (result > JpegMaxSampleValue)
		result = JpegMaxSampleValue ;
	return (JPEGSAMPLE) result ;
}

/********************************************************************
**    I_FUNCTION     :  RGBToY (red, green, blue)
**             convert RGB color value to Y (luminance) value
**       
**
**    PARAMETERS
**       INPUT  :
**               red, green, blue: RGB color value
**                                                                   
**       OUTPUT :
**               none 
**    RETURNS      : Y value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static JPEGSAMPLE RGBToY (red, green, blue)
JPEGSAMPLE red, green, blue;
{
	int result = (Scaled (0.299) * red
					+ Scaled (0.587) * green + Scaled (0.114) * blue
					+ Rounding) >> ScaleFactor ;
	if (result > JpegMaxSampleValue)
		result = JpegMaxSampleValue ;
	else if (result < JpegMinSampleValue)
		result = JpegMinSampleValue ;
	return result ;
}

/********************************************************************
**    I_FUNCTION     :  RGBToCb (red, green, blue)
**             convert RGB color value to Cb (blueness of chrominance) value
**       
**
**    PARAMETERS
**       INPUT  :
**               red, green, blue: RGB color value
**                                                   
**       OUTPUT :
**               none 
**    RETURNS      : Cb value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static JPEGSAMPLE RGBToCb (red, green, blue)
JPEGSAMPLE red, green, blue;
{
	int result = ((JpegMidpointSampleValue<<ScaleFactor)
				+ Rounding - Scaled (0.1687) * red
				- Scaled (0.3313) * green + Scaled (0.5) * blue) >> ScaleFactor;
	if (result > JpegMaxSampleValue)
		result = JpegMaxSampleValue ;
	else if (result < JpegMinSampleValue)
		result = JpegMinSampleValue ;
	return result ;
}

/********************************************************************
**    I_FUNCTION     :  RGBToCr (red, green, blue)
**             convert RGB color value to Cr (redness of chrominance) value
**       
**
**    PARAMETERS
**       INPUT  :
**               red, green, blue: RGB color value
**                                                       
**       OUTPUT :
**               none 
**    RETURNS      : Cr value
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static JPEGSAMPLE RGBToCr (red, green, blue)
JPEGSAMPLE red, green, blue;
{
	int result = ((JpegMidpointSampleValue<<ScaleFactor)
					+ Rounding + Scaled (0.5) * red
					- Scaled (0.4187) * green - Scaled (0.0813) * blue) >> ScaleFactor;
	if (result > JpegMaxSampleValue)
		result = JpegMaxSampleValue ;
	else if (result < JpegMinSampleValue)
		result = JpegMinSampleValue ;
	return result ;
}

/********************************************************************
**    I_FUNCTION     :  InitializeDctMatrix
**             Initialize DctMatrix
**       
**
**    PARAMETERS
**       INPUT  :
**               none
**                                                       
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void InitializeDctMatrix ()
{
	unsigned int ii, jj ;

	if (Initialized)
		return ;

	Initialized = 1 ;

	for (jj = 0 ; jj < JpegSampleWidth ; ++ jj)
		DctMatrix [0][jj] = 1.0 / sqrt (8.0) ;

	for (ii = 1 ; ii < JpegSampleWidth ; ++ ii)
	{
		for (jj = 0 ; jj < JpegSampleWidth ; ++ jj)
		{
			DctMatrix [ii][jj] = 0.5 * cos (((2 * jj + 1) * ii) * M_PI /16.0) ;
		}
	}

	for (ii = 0 ; ii < JpegSampleWidth ; ++ ii)
	{
		for (jj = 0 ; jj < JpegSampleWidth ; ++ jj)
		{
			IDctMatrix [ii][jj] = DctMatrix [jj][ii] ;
		}
	}
	return ;
}

/********************************************************************
**    I_FUNCTION     :  Multiply(aa, bb, cc)
**             Multiply matrix aa and matrix bb and get matrix cc
**       
**    PARAMETERS
**       INPUT  :
**               aa, bb, cc
**                                                       
**       OUTPUT :
**               cc 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void Multiply (aa, bb, cc)
MATRIX aa, bb, cc;
{
	unsigned int ii, jj, kk ;
	for (ii = 0 ; ii < JpegSampleWidth ; ++ ii)
	{
		for (jj = 0 ; jj < JpegSampleWidth ; ++ jj)
		{
			cc [ii][jj] = 0.0 ;
			for (kk = 0 ; kk < JpegSampleWidth ; ++ kk)
			{
				cc [ii][jj] += aa [ii][kk] * bb [kk][jj] ;
			}
		}
	}
	return ;
}

/********************************************************************
**    I_FUNCTION     :  jpg_SetScanAttributes (scan, components, sse, ssa)
**             Set Image Scan Attributes
**       
**    PARAMETERS
**       INPUT  :
**               scan, components, sse, ssa
**                                                       
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static jpg_SetScanAttributes (scan, components, sse, ssa)
unsigned int scan, sse, ssa;
unsigned long components;
{
	jpeg.image_scans [scan].component_mask = components ;
	jpeg.image_scans [scan].spectral_selection_end = sse ;
	jpeg.image_scans [scan].successive_approximation = ssa ;
	return 0;
}

/********************************************************************
**    I_FUNCTION     :  jpg_SetGrayscale(grayscale)
**             Set Image Scan grayscale
**       
**    PARAMETERS
**       INPUT  :
**               grayscale: 1: grayscale picture
**							0: use color
**                                                       
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void jpg_SetGrayscale(grayscale)
int grayscale;
{
	jpeg.gray_scale = grayscale;
}

/********************************************************************
**    I_FUNCTION     :  jpg_Initialize()
**             Initialize JPEG data structure
**       
**    PARAMETERS
**       INPUT  :
**               none             
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
jpg_Initialize ()
{
	unsigned int ii, jj;	
	memset (jpeg.image_scans, 0, sizeof (jpeg.image_scans)) ;
	jpeg.image_scans [0].component_mask = (1<<YComponent)
                                 |(1<<CbComponent)
                                 |(1<<CrComponent) ;
	jpeg.image_quality = 75 ;
	jpeg.restart_interval = 0 ;
	jpeg.rows_per_restart = 4 ;
	for (ii = 0 ; ii < 4 ; ++ ii)
	{
		jpeg.image_components [ii].du_rows = 0 ;
		jpeg.image_components [ii].du_cols = 0 ;

		jpeg.image_components [ii].eob_run = 0 ;
		jpeg.image_components [ii].eob_start_du_row = 0 ;
		jpeg.image_components [ii].eob_start_du_col = 0 ;
		jpeg.image_components [ii].eob_start_position = 0 ;
		jpeg.image_components [ii].h_frequency = 1;
		jpeg.image_components [ii].v_frequency = 1;
		jpeg.image_components [ii].dct_coefficients = NULL ;
	}
	for (jj = 0 ; jj < 2 ; ++ jj)
	{
		memset (jpeg.ac_tables[jj].frequencies, 0,
					 sizeof (jpeg.ac_tables[jj].frequencies)) ;
		memset (jpeg.dc_tables[jj].frequencies, 0,
					 sizeof (jpeg.dc_tables[jj].frequencies)) ;
		jpeg.ac_tables[jj].frequencies [256] = 1;
		jpeg.dc_tables[jj].frequencies [256] = 1;
		jpeg.ac_tables[jj].sizes_found = 0;
		jpeg.dc_tables[jj].sizes_found = 0;
	}

	jpeg.image_components [YComponent].dc_table = &(jpeg.dc_tables [0]);
	jpeg.image_components [YComponent].ac_table = &(jpeg.ac_tables [0]);
	jpeg.image_components [CbComponent].dc_table = &(jpeg.dc_tables [1]);
	jpeg.image_components [CbComponent].ac_table = &(jpeg.ac_tables [1]);
	jpeg.image_components [CrComponent].dc_table = &(jpeg.dc_tables [1]);
	jpeg.image_components [CrComponent].ac_table = &(jpeg.ac_tables [1]);

	jpeg.image_components [YComponent].quantization_table = &jpeg.luminance_quanttbl;
	jpeg.image_components [CbComponent].quantization_table = &jpeg.chrominance_quanttbl;
	jpeg.image_components [CrComponent].quantization_table = &jpeg.chrominance_quanttbl;
	return 0;
}

/********************************************************************
**    I_FUNCTION     :  jpg_CreateQuantizationTables (quality)
**             Create JPEG Quantization Tables
**       
**    PARAMETERS
**       INPUT  :
**               quality             
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void jpg_CreateQuantizationTables (quality)
unsigned int quality;
{
	unsigned int ii ;

	double scale = exp ((double)((6 * (50 - (int) quality)/50.0) * log (2.0))) ;

	for (ii = 0 ; ii < JpegSampleSize ; ++ ii)
	{
		unsigned int value =
					default_luminance_table [JpegZigZagInputOrderCodes [ii]]
						* scale ;
		if (value < JpegMinQuantizationValue)
			jpeg.luminance_quanttbl.data_values[ii] = JpegMinQuantizationValue ;
		else if (value > JpegMax8BitQuantizationValue)
			jpeg.luminance_quanttbl.data_values[ii] = JpegMax8BitQuantizationValue ;
		else
			jpeg.luminance_quanttbl.data_values[ii] = value ;
	}

	for (ii = 0 ; ii < JpegSampleSize ; ++ ii)
	{
		unsigned int value =
					default_chrominance_table [JpegZigZagInputOrderCodes [ii]]
					* scale ;
		if (value < 1)
			jpeg.chrominance_quanttbl.data_values[ii] = JpegMinQuantizationValue ;
		else if (value > JpegMax8BitQuantizationValue)
			jpeg.chrominance_quanttbl.data_values[ii] = JpegMax8BitQuantizationValue ;
		else
			jpeg.chrominance_quanttbl.data_values[ii] = value ;
	}
	return ;
}

/********************************************************************
**    I_FUNCTION     :  jpg_CalculateMcuDimensions ()
**        This function determines the dimensions of an MCU using the maximum
**			sampling frequencies of the components.
**
**    PARAMETERS
**       INPUT  :
**               none             
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void jpg_CalculateMcuDimensions ()
{
	unsigned int ii, mcuheight, mcuwidth;
	jpeg.max_horizontal_frequency = 1 ;
	jpeg.max_vertical_frequency = 1 ;

	if (!jpeg.gray_scale)
	{
		for (ii = YComponent ; ii <= CrComponent ; ++ ii)
		{
			if (jpeg.image_components [ii].h_frequency
							> jpeg.max_horizontal_frequency)
			{
				jpeg.max_horizontal_frequency
						= jpeg.image_components [ii].h_frequency;
			}

			if (jpeg.image_components [ii].v_frequency 
							> jpeg.max_vertical_frequency)
			{
				jpeg.max_vertical_frequency
						= jpeg.image_components [ii].v_frequency ;
			}
		}
	}
	else
	{
		jpeg.max_horizontal_frequency
				= jpeg.image_components [YComponent].h_frequency;
		jpeg.max_vertical_frequency
				= jpeg.image_components [YComponent].v_frequency ;
	}

	mcuheight = jpeg.max_vertical_frequency * JpegSampleWidth ;
	mcuwidth = jpeg.max_horizontal_frequency * JpegSampleWidth ;
	jpeg.mcu_rows = (jpeg.frame_height + mcuheight - 1) / mcuheight ;
	jpeg.mcu_cols = (jpeg.frame_width + mcuwidth - 1) / mcuwidth ;
	return ;
}


/********************************************************************
**    I_FUNCTION     :  jpg_ValidateParameters ()
**        This function suppose validate the data but ignored now.
**
**    PARAMETERS
**       INPUT  :
**               none          
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void jpg_ValidateParameters ()
{
	if (! jpeg.gray_scale)
	{
		if (jpeg.image_scans [2].component_mask != 0)
			jpeg.scan_count = 3 ;
		else if (jpeg.image_scans [1].component_mask != 0)
			jpeg.scan_count = 2 ;
		else
			jpeg.scan_count = 1 ;
	}
	else
	{
		jpeg.scan_count = 1 ;
	}
}
/********************************************************************
**    I_FUNCTION     :  OutputByte (value)
**        This function writes a single byte to the output stream.
**
**    PARAMETERS
**       INPUT  :
**               value: The byte to be written to the output stream.         
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void OutputByte (value)
UBYTE1 value;
{
	if (jpeg.bit_count != 0)
		FlushBitBuffer () ;

	fwrite ((UBYTE1 *) &value, 1, 1, jpg_fptr);
	return ;
}
/********************************************************************
**    I_FUNCTION     :  OutputWord (UBYTE2 value)
**        This function a 2-byte integer in system format to the output
**			stream in bit-endian order (most significant byte first).
**
**    PARAMETERS
**       INPUT  :
**               value: The byte to be written to the output stream.         
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void OutputWord (value)
UBYTE2 value;
{
	UBYTE2 data;
	if (jpeg.bit_count != 0)
		FlushBitBuffer () ;
	data = SystemAdjustValue (value) ;
	fwrite ((char *) &data, sizeof (data), 1, jpg_fptr);

	return ;
}

/********************************************************************
**    I_FUNCTION     :  OutputBits (int bits, unsigned int count)
**        This function outputs a single bit to the output stream.
**			Individual bits are buffered using the bit_buffer and 
**			bit_count member variables
**
**    PARAMETERS
**       INPUT  :
**			bits: The bit string to be output
**		    count: The number of bits to output
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void OutputBits (bits, count)
int bits;
unsigned int count;
{
	unsigned int ii;
	for (ii = 0 ; ii < count ; ++ ii)
	{
		jpeg.bit_buffer <<= 1 ;
		++ (jpeg.bit_count) ;
		jpeg.bit_buffer |= ((bits >> (count - ii - 1)) & 0x1) ;
		if (jpeg.bit_count == 8)
		{
			fwrite ((char *) &jpeg.bit_buffer, 1, 1, jpg_fptr);
			if (jpeg.bit_buffer == SOB)
			{
				jpeg.bit_buffer = 0 ;
				fwrite ((char *) &(jpeg.bit_buffer), 1, 1, jpg_fptr);
			}
			jpeg.bit_count = 0 ;
		}
	}
	return ;
}

/********************************************************************
**    I_FUNCTION     :  OutputMarker (UBYTE1 marker)
**        This function writes a marker to the output stream.
**			
**
**    PARAMETERS
**       INPUT  :
**				marker: The marker to be written to the output stream
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void OutputMarker (marker)
UBYTE1 marker;
{
	OutputByte (SOB) ;
	OutputByte (marker) ;
	return ;
}

/********************************************************************
**    I_FUNCTION     :  OutputJfifHeader
**        This function writes the JFIF header for the image.
**			
**
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void OutputJfifHeader ()
{
	JfifHeader jfif ;
	jfif.length = SystemAdjustValue ((UBYTE2) sizeof (jfif)) ;
	jfif.identifier [0] = 'J' ;
	jfif.identifier [1] = 'F' ;
	jfif.identifier [2] = 'I' ;
	jfif.identifier [3] = 'F' ;
	jfif.version [0] = 1 ;
	jfif.version [1] = 1 ;
	jfif.units = 0 ;
	jfif.xdensity = SystemAdjustValue ((UBYTE2) 1) ;
	jfif.ydensity = SystemAdjustValue ((UBYTE2) 1) ;
	jfif.xthumbnail = 0 ;
	jfif.ythumbnail = 0 ;
	fwrite ((char*) &jfif, sizeof(jfif), 1, jpg_fptr);
	return ;
}

/********************************************************************
**    I_FUNCTION     :  cmpt_CalculateDuDimensions (indx, maxhf, maxvf)
**        This function allocates the buffer used to hold DCT coefficients.
**
**    PARAMETERS
**       INPUT  :
**				indx: The image components indx being output
**				maxhf: The maximum horizontal frequency for all components.
**				maxvf: The maximum vertical frequency for all components.
**       OUTPUT :
**               none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void cmpt_CalculateDuDimensions (indx, maxhf, maxvf)
unsigned int indx, maxhf, maxvf;
{
	unsigned int h_frequency, v_frequency, du_cols, du_rows ;
	h_frequency = jpeg.image_components [indx].h_frequency;
	v_frequency = jpeg.image_components [indx].v_frequency;
	jpeg.image_components [indx].du_cols = du_cols = 
			(image.image_width * h_frequency + (JpegSampleWidth * maxhf - 1))
			/ (JpegSampleWidth * maxhf) ;  
	jpeg.image_components [indx].du_rows = du_rows = 
			(image.image_height * v_frequency + (JpegSampleWidth * maxvf - 1))
			/ (JpegSampleWidth * maxvf) ;
	jpeg.image_components [indx].v_period = maxvf / v_frequency ;
	jpeg.image_components [indx].h_period = maxhf / h_frequency ;

	jpeg.image_components [indx].dct_coefficients =
				(JpegEncoderCoefficientBlock *) malloc (
				du_cols * du_rows * sizeof (JpegEncoderCoefficientBlock));
	return ;
}

/********************************************************************
**    I_FUNCTION     :  ForwardDct (data, qt, output)
**        
**
**    PARAMETERS
**       INPUT  :
**				data: 
**				qt: 
**				output: 
**       OUTPUT :
**               output 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ForwardDct (data, qt, output)
JpegEncoderDataUnit *data; 
JpegEncoderQuantizationTable *qt;
JpegEncoderCoefficientBlock *output;
{
	MATRIX source ;
	MATRIX destination ;
	MATRIX temp ;
	unsigned int ii, jj ;

	InitializeDctMatrix () ;

	for (ii = 0 ; ii < JpegSampleWidth ; ++ ii)
	{
		for (jj = 0 ; jj < JpegSampleWidth ; ++ jj)
		{
			source [ii][jj] = data->data [ii*JpegSampleWidth + jj] - JpegMidpointSampleValue ;
		}
	}
	Multiply (DctMatrix, source, temp) ;
	Multiply (temp, IDctMatrix, destination) ;
	for (ii = 0 ; ii < JpegSampleWidth ; ++ ii)
	{
		for (jj = 0 ; jj < JpegSampleWidth ; ++ jj)
		{
			destination [ii][jj] /= 
				qt->data_values [JpegZigZagOutputOrderCodes [ii * JpegSampleWidth
				+ jj]] ;
			if (destination [ii][jj] < 0)
				output->data [ii*JpegSampleWidth + jj] = destination [ii][jj] - .5 ;
			else
				output->data [ii*JpegSampleWidth + jj] = destination [ii][jj] + .5 ;
		}
	}
	return ;
}

/********************************************************************
**    I_FUNCTION     :  cmpt_Sample1to1Component (indx, function)
**        This function samples a component where the horizontal and
**			vertical sampling frequencies are equal to the maximum values
**			for all components.
**
**    PARAMETERS
**       INPUT  :
**				image:  The image to sample
**				function:  A color conversion function
**       OUTPUT :
**               output 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void cmpt_Sample1to1Component (indx, function)
unsigned int indx;
RGBTOYCBCRFUNCTION function;
{
	JpegEncoderDataUnit data ;
	
	UBYTE1 red ;
	UBYTE1 green ;
	UBYTE1 blue ;
	unsigned int ii, jj, row, col, dr, dc, du_cols, du_rows;
	JpegEncoderQuantizationTable *quantization_table; 
	JpegEncoderCoefficientBlock *dct_coefficients;
	unsigned int index = 0 ;

	du_rows = jpeg.image_components [indx].du_rows;
	du_cols = jpeg.image_components [indx].du_cols;

	for (ii = 0 ; ii < du_rows ; ++ ii)
	{
		for (jj = 0 ; jj < du_cols ; ++ jj, ++ index)
		{
			row = ii * JpegSampleWidth ;
			for (dr = 0 ; dr < JpegSampleWidth ; ++ dr, ++ row)
			{
				col = jj * JpegSampleWidth ;
				for (dc = 0 ; dc < JpegSampleWidth ; ++ dc, ++ col)
				{
					if (row < image.image_height)
					{
						if (col < image.image_width)
						{
							image_GetRGB (row, col, &red, &green, &blue) ;
							data.data [dr*JpegSampleWidth+dc] = function (red, green, blue) ;
						}
						else if (dc != 0)
						{
							data.data [dr*JpegSampleWidth+dc] = data.data [dr*JpegSampleWidth+dc-1] ;
						}
						else
						{
							data.data [dr*JpegSampleWidth+dc] = JpegMidpointSampleValue ;
						}
					}
					else if (dr != 0)
					{
						data.data [dr*JpegSampleWidth+dc] = data.data [(dr-1)*JpegSampleWidth+dc] ;
					}
					else
					{
						data.data [dr*JpegSampleWidth+dc] = JpegMidpointSampleValue ;
					}
				}
			}
			quantization_table = 
						jpeg.image_components [indx].quantization_table;
			dct_coefficients =
						jpeg.image_components [indx].dct_coefficients;

			ForwardDct (&data, quantization_table, &dct_coefficients [index]) ;
		}
	}
	return ;
}


/********************************************************************
**    I_FUNCTION     :  cmpt_Sample1to1Component (indx, function)
**        This function downsamples a component where the horizontal and
**			vertical sampling frequencies are not equal to the maximum values
**			for all components.
**
**    PARAMETERS
**       INPUT  :
**				image:  The image to sample
**				function:  A color conversion function
**       OUTPUT :
**               output 
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void cmpt_SampleNtoNComponent (indx, function)
unsigned int indx;
RGBTOYCBCRFUNCTION function;
{
	JpegEncoderDataUnit data ;
	unsigned int imagecol, xx, yy, outrow, outcol, value, count, srcrow,
			srccol, row, col;
	UBYTE1 red, green, blue ;
	JpegEncoderQuantizationTable *quantization_table;
	JpegEncoderCoefficientBlock *dct_coefficients;
	unsigned int du_rows, du_cols, h_period, v_period;
	unsigned int index = 0 ;
	unsigned int imagerow = 0 ;
	du_rows = jpeg.image_components [indx].du_rows;
	du_cols = jpeg.image_components [indx].du_cols;
	v_period = jpeg.image_components [indx].v_period;
	h_period = jpeg.image_components [indx].h_period;

	for (yy = 0 ; yy < du_rows ;
			++ yy, imagerow += JpegSampleWidth * v_period)
	{
		imagecol = 0 ;
		for (xx = 0 ; xx < du_cols ;
			++ xx, ++ index, imagecol += JpegSampleWidth * h_period)
		{
			for (outrow = 0 ; outrow < JpegSampleWidth  ; ++ outrow)
			{
				for (outcol = 0 ; outcol < JpegSampleWidth ; ++ outcol)
				{
					value = 0 ;  
					count = 0 ;
					for (srcrow = 0 ; srcrow < v_period ; ++ srcrow)
					{
						row = imagerow + srcrow + outrow * v_period ;
						for (srccol = 0 ; srccol < h_period ; ++ srccol)
						{
							col = imagecol + srccol + outcol * h_period ;
							if (row < image.image_height && col < image.image_width)
							{
								++ count ;
								image_GetRGB (row, col, &red, &green, &blue) ;
								value += function (red, green, blue) ;
							}
						}
					}
					if (count != 0)
					{
						data.data [outrow*JpegSampleWidth+outcol] = (value + count - 1) / count ;
					}
					else
					{
/*
......If we get here that means the data unit goes past the edge of
......the input image. We attempt to duplicate the last column and
......if we can't to that we copy the last row. If the entire data unit
......is off the edge then we use zero for the value.
*/
						if (outcol != 0)
							data.data [outrow*JpegSampleWidth+outcol] = data.data [(outrow*JpegSampleWidth)+outcol - 1] ;
						else if (outrow != 0)
							data.data [outrow*JpegSampleWidth+outcol] = data.data [(outrow-1)*JpegSampleWidth+outcol] ;
						else
							data.data [outrow*JpegSampleWidth+outcol] = 0 ;
					}
				}
			}

			quantization_table = 
							jpeg.image_components [indx].quantization_table;
			dct_coefficients =
							jpeg.image_components [indx].dct_coefficients;

			ForwardDct (&data, quantization_table, &(dct_coefficients [index])) ;
		}
	}
	return ;
}





/********************************************************************
**    I_FUNCTION     :  cmpt_SampleYComponent (indx, maxhf, maxvf)
**        This function samples the Y component for the image.
**			What we do here is determine how much memory is required
**			to hold the image, what color conversion function to use
**			for the component, and if downsampling is required.
**
**    PARAMETERS
**       INPUT  :
**			image:  The image Component index to be sampled
**			maxhf:  The maximum horizontal frequency of all components
**			maxvf:  The maximum vertical frequency of all components
**       OUTPUT :
**               output 
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void cmpt_SampleYComponent (indx, maxhf, maxvf)
unsigned int indx, maxhf, maxvf;
{
	unsigned int v_frequency, h_frequency ;

	cmpt_CalculateDuDimensions (indx, maxhf, maxvf) ;

	v_frequency = jpeg.image_components [indx].v_frequency;
	h_frequency = jpeg.image_components [indx].h_frequency;

	if (maxhf == h_frequency && maxvf == v_frequency)
	{
		cmpt_Sample1to1Component (indx, RGBToY) ;
	}
	else
	{
		cmpt_SampleNtoNComponent (indx, RGBToY) ;
	}

	return ;
}

/********************************************************************
**    I_FUNCTION     :  cmpt_SampleCbComponent (indx, maxhf, maxvf)
**        This function samples the Cb component for the image.
**			What we do here is determine how much memory is required
**			to hold the image, what color conversion function to use
**			for the component, and if downsampling is required.
**
**    PARAMETERS
**       INPUT  :
**			image:  The image Component index to be sampled
**			maxhf:  The maximum horizontal frequency of all components
**			maxvf:  The maximum vertical frequency of all components
**       OUTPUT :
**               output 
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void cmpt_SampleCbComponent (indx, maxhf, maxvf)
unsigned int indx, maxhf, maxvf;
{
	unsigned int v_frequency, h_frequency ;

	cmpt_CalculateDuDimensions (indx, maxhf, maxvf) ;

	v_frequency = jpeg.image_components [indx].v_frequency;
	h_frequency = jpeg.image_components [indx].h_frequency;

	if (maxhf == h_frequency && maxvf == v_frequency)
	{
		cmpt_Sample1to1Component (indx, RGBToCb) ;
	}
	else
	{
		cmpt_SampleNtoNComponent (indx, RGBToCb) ;
	}

	return ;
}
/********************************************************************
**    I_FUNCTION     :  cmpt_SampleCrComponent (indx, maxhf, maxvf)
**        This function samples the Cr component for the image.
**			What we do here is determine how much memory is required
**			to hold the image, what color conversion function to use
**			for the component, and if downsampling is required.
**
**    PARAMETERS
**       INPUT  :
**			image:  The image Component index to be sampled
**			maxhf:  The maximum horizontal frequency of all components
**			maxvf:  The maximum vertical frequency of all components
**       OUTPUT :
**               output 
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void cmpt_SampleCrComponent (indx, maxhf, maxvf)
unsigned int indx, maxhf, maxvf;
{
	unsigned int v_frequency, h_frequency ;
	cmpt_CalculateDuDimensions (indx, maxhf, maxvf) ;

	v_frequency = jpeg.image_components [indx].v_frequency;
	h_frequency = jpeg.image_components [indx].h_frequency;
	if (maxhf == h_frequency && maxvf == v_frequency)
	{
		cmpt_Sample1to1Component (indx, RGBToCr) ;
	}
	else
	{
		cmpt_SampleNtoNComponent (indx, RGBToCr) ;
	}
	return ;
}

/********************************************************************
**    I_FUNCTION     :  PrintQuantizationTables ()
**        This function writes the quantization tables to the output stream.
**
**    PARAMETERS
**       INPUT  :
**					none
**       OUTPUT :
**               output 
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void PrintQuantizationTables ()
{
	unsigned int ii ;
	int index ;
	UBYTE1 pqtq ;
	int precision = 0 ; 

	if (jpeg.gray_scale)
	{
		OutputMarker (DQT) ;
		OutputWord (sizeof(UBYTE2) + (sizeof (pqtq) + JpegSampleSize)) ;

		index = 0 ;
		pqtq = (precision << 4) | index ;
		OutputByte (pqtq) ;
		for (ii = 0 ; ii < JpegSampleSize ; ++ ii)
			OutputByte (jpeg.luminance_quanttbl.data_values[ii]) ;
	}
	else
	{
		OutputMarker (DQT) ;

		OutputWord (sizeof(UBYTE2) + 2 * (sizeof (pqtq) + JpegSampleSize)) ;

		index = 0 ;
		pqtq = (precision << 4) | index ;
		OutputByte (pqtq) ;
		for (ii = 0 ; ii < JpegSampleSize ; ++ ii)
			OutputByte (jpeg.luminance_quanttbl.data_values[ii]) ;

		index = 1 ;
		pqtq = (precision << 4) | index ;
		OutputByte (pqtq) ;
		for (ii = 0 ; ii < JpegSampleSize ; ++ ii)
		OutputByte (jpeg.chrominance_quanttbl.data_values[ii]) ;
	}
	return ;
}
/********************************************************************
**    I_FUNCTION     :  OutputRestartInterval (restartinterval)
**        This function is called by a scan to write a restart interval to the
**			output stream. We do not output a Define Restart Interval marker if
**			the restart interval is not changing.
**
**    PARAMETERS
**       INPUT  :
**					none
**       OUTPUT :
**               restartinterval: The new restart interval
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void OutputRestartInterval (restartinterval)
unsigned int restartinterval;
{
	if (restartinterval == jpeg.restart_interval)
		return ;
	jpeg.restart_interval =  restartinterval ;

	OutputMarker (DRI) ;
	OutputWord (4) ;
	OutputWord (restartinterval) ;
	return ;
}

/********************************************************************
**    I_FUNCTION     :  InterleavedPass (writedata, passfunction, dcfunction, acfunction, sss, sse, ssa)
**        This function makes a pass through the data units for an interleaved
**			Scan.
**
**    PARAMETERS
**       INPUT  :
**			writedata: false => This is a statistics gathering pass
**						true => This pass is writing data to the output file.
**			passfunctions: Pointer to EncoderComponent member function used
**						to process this pass.
**			dcfunction:    Pointer to the EncoderComponent member function used
**						to process DC coefficient values.
**			acfunction:    Pointer to the EncoderComponent member function used
**						to process AC coefficient values.
**			sss:           Spectral Selection Start (0-63)
**			sse:           Spectral Selection End (0-63)
**			ssa:           Successive Approximation (0-13)
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void InterleavedPass (writedata, passfunction, dcfunction, acfunction, sss, sse, ssa)
int writedata;
UW_METHOD passfunction, dcfunction, acfunction;
unsigned int sss, sse, ssa;
{
	unsigned int progress ;
	unsigned int intervalcount, restartmarker, mcucol, mcurow, cc, cx, cy, durow,
ducol;
	int progressscale = 8 ;
	if (writedata)
		progress = 50 << progressscale ;
	else
		progress = 0 ;

	jpg_ResetDcValues () ;

	intervalcount = 0 ;
	restartmarker = 0 ;
	for (mcurow = 0 ;
		mcurow < jpeg.mcu_rows ;
		++ mcurow)
	{
		for (mcucol = 0 ;
				mcucol < jpeg.mcu_cols ;
					++ mcucol, ++ intervalcount)
		{
			if (jpeg.restart_interval != 0)
			{
				if (intervalcount == jpeg.restart_interval 
											&& jpeg.restart_interval != 0)
				{
					if (writedata)
						OutputMarker (RST0 | restartmarker) ;
					jpg_ResetDcValues () ;
					++ restartmarker ;
					restartmarker %= 8 ;
					intervalcount = 0 ;
				}
			}

			for (cc = 0 ; cc < jpeg.scan_component_count ; ++ cc)
			{
				for (cy = 0 ;
						cy < (jpeg.scan_components [cc])->v_frequency; ++ cy)
				{
					durow = (jpeg.scan_components [cc])->v_frequency
                             * mcurow + cy ;
					for (cx = 0 ;cx < (jpeg.scan_components [cc])->h_frequency;
						++ cx)
					{
						ducol = (jpeg.scan_components [cc])->h_frequency
											* mcucol + cx ;
						passfunction((jpeg.scan_components [cc]),
															durow, ducol,
                                              dcfunction, acfunction,
                                              sss, sse, ssa) ;
					}
				}
			}
		}
	}
	return ;
}

/********************************************************************
**    I_FUNCTION     :  OutputSize (tbl) 
**        This function determines the size of the Huffman table when it is
**			written to a DHT marker. This function is used to calculate the
**			2-byte marker length that comes right after the FF-DHT sequence in
**			the output stream. Therefore we need to find the length before we
**			actually write the table.
**
**    PARAMETERS
**       INPUT  :
**					tbl: Huffman table structure
**       OUTPUT :
**               none
**
**    RETURNS      : the size of the Huffman table
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static unsigned int OutputSize (tbl) 
JpegEncoderHuffmanTable *tbl;
{
	unsigned int ii;
	unsigned int count = 0 ;
	for (ii = 0 ; ii < JpegMaxHuffmanCodeLength ; ++ ii)
	{
		count += tbl->huff_bits [ii] ;
	}

	return count + JpegMaxHuffmanCodeLength + 1 ;
}

/********************************************************************
**    I_FUNCTION     :  tbl_PrintTable (tbl)
**        This function writes the Huffman table data to the output stream in the
**			format specified by the JPEG standard.
**
**    PARAMETERS
**       INPUT  :
**					tbl: Huffman table structure
**       OUTPUT :
**               restartinterval: The new restart interval
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void tbl_PrintTable (tbl)
JpegEncoderHuffmanTable *tbl;
{
	unsigned int ii ;

	UBYTE1 data ;
	unsigned int count = 0 ; 

	for (ii = 0 ; ii < JpegMaxHuffmanCodeLength ; ++ ii)
	{
		count += tbl->huff_bits [ii] ;
		data = tbl->huff_bits [ii] ;
		OutputByte (data) ;
	}

	for (ii = 0 ; ii < count ; ++ ii)
	{
		data = tbl->huff_values [ii] ;
		OutputByte (data) ;
	}
	return ;
}

/********************************************************************
**    I_FUNCTION     :  jpg_PrintHuffmanTables (scan, usedc, useac)
**        This function writes the Huffman tables used by a scan to the output
**			stream. 
**
**    PARAMETERS
**       INPUT  :
**			scan:     Here the scan structure is used to determine which components
**			    are part of the scan (Y and/or Cb/Cr)
**			usedc:    Set to true if the scan includes DC components. (Sequential
**			    Scan or Progressive Scan with the spectral selection 0)
**			useac:    Set to true if the scan includes AC components. (Sequential
**			    Scan or Progressive Scan with the spectral selection start
**			    not zero)
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void jpg_PrintHuffmanTables (scan, usedc, useac)
Scan scan;
int usedc, useac;
{
	unsigned int size = sizeof (UBYTE2) ;
	if ((scan.component_mask & (1 << YComponent)) != 0)
	{
		if (jpeg.scan_component_count != 1)
		{
/*
......We have at least two components and the first is the Y component.
......This means we need the both of the huffman tables.
*/
			OutputMarker (DHT) ;
			if (usedc)
			{
				size += OutputSize (&(jpeg.dc_tables [0]))
				+ OutputSize (&(jpeg.dc_tables [1])) ;
			}
			if (useac)
			{
				size += OutputSize (&(jpeg.ac_tables [0]))
				+ OutputSize (&(jpeg.ac_tables [1])) ;
			}
			OutputWord (size) ;
			if (usedc)
			{
				OutputByte (0x00) ;
				tbl_PrintTable (&(jpeg.dc_tables [0])) ;
			}
			if (useac)
			{
				OutputByte (0x10) ;
				tbl_PrintTable (&(jpeg.ac_tables [0])) ;
			}
			if (usedc)
			{
				OutputByte (0x01) ;
				tbl_PrintTable (&(jpeg.dc_tables [1])) ;
			}
			if (useac)
			{
				OutputByte (0x11) ;
				tbl_PrintTable (&(jpeg.ac_tables [1])) ;
			}
		}
		else
		{
			size = sizeof (UBYTE2) ;
			if (usedc)
			{
				size += OutputSize (&(jpeg.dc_tables [0])) ;
			}
			if (useac)
			{
				size += OutputSize (&(jpeg.ac_tables [0])) ;
			}

			OutputMarker (DHT) ;
			OutputWord (size) ;
			if (usedc)
			{
				OutputByte (0x00) ;
				tbl_PrintTable (&(jpeg.dc_tables [0])) ;
			}
			if (useac)
			{
				OutputByte (0x10) ;
				tbl_PrintTable (&(jpeg.ac_tables [0])) ;
			}
		}
	}
	else
	{
/*
......The Y component is not present. Output is the same for
......Cb, Cr, or Cb & Cr.
*/
		size = sizeof (UBYTE2) ;
		if (usedc)
		{
			size += OutputSize (&(jpeg.dc_tables [1])) ;
		}
		if (useac)
		{
			size += OutputSize (&(jpeg.ac_tables [1])) ;
		}

		OutputMarker (DHT) ;
		OutputWord (size) ;
		if (usedc)
		{
			OutputByte (0x01) ;
			tbl_PrintTable (&(jpeg.dc_tables [1])) ;
		}
		if (useac)
		{
			OutputByte (0x11) ;
			tbl_PrintTable (&(jpeg.ac_tables [1])) ;
		}
	}
	return ;

}
/********************************************************************
**    I_FUNCTION     :  NoninterleavedPass (writedata, passfunction, dcfunction, acfunction, sss, sse, ssa)
**        This function makes a pass through the data units for an non-interleaved
**			Scan.
**
**    PARAMETERS
**       INPUT  :
**			writedata: false => This is a statistics gathering pass
**						true => This pass is writing data to the output file.
**			passfunctions: Pointer to EncoderComponent member function used
**						to process this pass.
**			dcfunction:    Pointer to the EncoderComponent member function used
**						to process DC coefficient values.
**			acfunction:    Pointer to the EncoderComponent member function used
**						to process AC coefficient values.
**			sss:           Spectral Selection Start (0-63)
**			sse:           Spectral Selection End (0-63)
**			ssa:           Successive Approximation (0-13)
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void NoninterleavedPass (writedata, passfunction, dcfunction, acfunction, sss, sse, ssa)
int writedata;
UW_METHOD passfunction, dcfunction, acfunction;
unsigned int sss, sse, ssa;
{
	unsigned int progress, row, col ;

	unsigned int intervalcount = 0 ;
	unsigned int restartmarker = 0 ;
	int progressscale = 8 ;
	if (writedata)
		progress = 50 << progressscale ;
	else
		progress = 0 ;

	jpg_ResetDcValues () ;

	for (row = 0 ;
		row < (jpeg.scan_components [0])->du_rows; ++ row)
	{
		for (col = 0 ; col < (jpeg.scan_components [0])->du_cols;
			++ col, ++ intervalcount)
		{
			if (jpeg.restart_interval != 0)
			{
				if (intervalcount == jpeg.restart_interval && 
										jpeg.restart_interval != 0)
				{
					if (writedata)
						OutputMarker (RST0 | restartmarker) ;
					jpg_ResetDcValues () ;
					++ restartmarker ;
					restartmarker %= 8 ;
					intervalcount = 0 ;
				}
			}
			passfunction ((jpeg.scan_components [0]),
                                row,
                                col,
                                dcfunction,
                                acfunction,
                                sss, sse,
                                ssa) ;

		}
	}
	return ;
}


/********************************************************************
**    I_FUNCTION     :  cmpt_GatherDcData (cmpt, huffvalue, a)
**        This function is use to gather Huffman statistics for DC coefficients.
**		The calling sequences is idential to PrintDcData (). Only the huffvalue
**		parameter is used here.
**    PARAMETERS
**       INPUT  :
**			huffvalue:  The Huffman value to process.
**	
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static int cmpt_GatherDcData (cmpt, huffvalue, a)
JpegEncoderComponent *cmpt;
int huffvalue, a;
{
	++((cmpt->dc_table)->frequencies [huffvalue]); 
	return 0;
}

/********************************************************************
**    I_FUNCTION     :  cmpt_GatherAcData (cmpt, huffvalue, a, b)
**        This function is use to gather Huffman statistics for AC coefficients.
**		The calling sequences is idential to PrintAcData (). Only the huffvalue
**		parameter is used here.
**    PARAMETERS
**       INPUT  :
**			huffvalue:  The Huffman value to process
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static int cmpt_GatherAcData (cmpt, huffvalue, a, b)
JpegEncoderComponent *cmpt;
int huffvalue, a, b;
{
	if (huffvalue >= 0)
	{
		++((cmpt->ac_table)->frequencies [huffvalue]);
	}
	return 0;
}

/********************************************************************
**    I_FUNCTION: cmpt_PrintDcData (cmpt, huffvalue, bits)
**        This function is use to output Huffman-encoded data for a DC value.
**
**    PARAMETERS
**       INPUT  :
**			 huffvalue:  The Huffman value
**			bits: The additional data bits
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static int cmpt_PrintDcData (cmpt, huffvalue, bits)
JpegEncoderComponent *cmpt;
int huffvalue, bits;
{
	UBYTE2 huffcode ;
	UBYTE1 huffsize ;

	huffcode = cmpt->dc_table->ehufco [huffvalue];
	huffsize = cmpt->dc_table->ehufsi [huffvalue];
	OutputBits (huffcode, huffsize) ;
	if (huffvalue != 0)
		OutputBits (bits, huffvalue) ;
	return 0;
}

/********************************************************************
**    I_FUNCTION     :  cmpt_PrintAcData (cmpt, huffvalue, value, size)
**        This function is use to output Huffman-encoded data for an AC value.
**		
**
**    PARAMETERS
**       INPUT  :
**			huffvalue:  The Huffman value
**			value: The additional data bits
**			size: The number of additional data bits.
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static int cmpt_PrintAcData (cmpt, huffvalue, value, size)
JpegEncoderComponent *cmpt;
int huffvalue, value, size;
{
	UBYTE2 huffcode ;
	UBYTE1 huffsize ;

	if (huffvalue >= 0)
	{
		huffcode = cmpt->ac_table->ehufco [huffvalue];
		huffsize = cmpt->ac_table->ehufsi [huffvalue];
		OutputBits (huffcode, huffsize) ;
	}
	if (size != 0)
		OutputBits (value, size) ;
	return 0;
}
/********************************************************************
**    I_FUNCTION: cmpt_EncodeSequential (cmpt, row, col, dcfunction, acfunction, a, b, c)
**    This function is used for two purposes in a sequential scan:
**
**      o To gather statistics for generating Huffman Tables
**      o To encode and output a data unit.
**
**    PARAMETERS
**       INPUT  :
**			row,col: Data unit position
**			dcfunction:    Pointer to the EncoderComponent member function used
**						to process DC coefficient values.
**			acfunction:    Pointer to the EncoderComponent member function used
**						to process AC coefficient values.
**			cmpt:          compaonent structure
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static int cmpt_EncodeSequential (cmpt, row, col, dcfunction, acfunction, a, b, c)
JpegEncoderComponent *cmpt;
unsigned int row, col, a,b,c;
UW_METHOD dcfunction, acfunction;
{
	int bits, value, ssss, rrrrssss, diff ;
	unsigned int index;
	int zerorun = 0 ;
	JpegEncoderCoefficientBlock *du;
	
	du = &(cmpt->dct_coefficients[row * (cmpt->du_cols) + col] );
	diff = du->data [0] - cmpt->last_dc_value ;
	cmpt->last_dc_value = du->data[0] ;

	if (diff >= 0)
	{
		bits = diff ;
	}
	else
	{
		diff = -diff ;
		bits = ~diff ;
	}
	ssss = 0 ;
	while (diff != 0)
	{
		++ ssss ;
		diff >>= 1 ;
	}
	dcfunction(cmpt, ssss, bits) ;

	for (index = 1 ; index < JpegSampleSize ; ++ index)
	{
		if (du->data [JpegZigZagInputOrderCodes [index]] !=0)
		{
/*
......16 is the longest run of zeros that can be encoded except for the
......final EOB code.
*/
			while (zerorun >= 16)
			{
/*
......0xF0 is the code to skip 16 zeros (Figure F.1)
*/
				acfunction (cmpt,0xF0, 0, 0) ;
				zerorun -= 16 ;
			}
/*
......Non-zero AC coefficients are encoded with
......8-bit Huffman-encoded values of the form rrrrssss followed by
......1..10 additional bits of data. rrrr is the number of zero values
......to skip (0..15). ssss is the category (1..10) which specifies the
......number of additional raw bits to follow. (Figure F.1)
*/
			value = du->data [JpegZigZagInputOrderCodes [index]];
			if (value >= 0)
			{
				bits = value ;
			}
			else
			{
				value = -value ;
				bits = ~value ;
			}
			ssss = 0 ;
			while (value != 0)
			{
				value >>= 1 ;
				++ ssss ;
			}

			rrrrssss = (zerorun << 4) | ssss ;
			acfunction (cmpt, rrrrssss, bits, ssss) ;
			zerorun = 0 ;
		}
		else
		{
			++ zerorun ;
		}
	}
/*
......The code 0x00 indicates all remaining AC coefficients are zero.
*/
	if (zerorun > 0)
	{
		acfunction (cmpt, 0, 0, 0) ;
	}

	return 0;
}



/********************************************************************
**    I_FUNCTION     :  InterleavedPass (writedata, passfunction, dcfunction, acfunction, sss, sse, ssa)
**		This function generates the Huffman Codes using the frequency data. The code
**		generation process is taken directly from the JPEG standard.
**		The outputs from this function are the following member variables:
**
**		ehufsi [n] : The length of the Huffman Code for value "n"
**		ehufco [n] : The Huffman Code for value "n"
**		huff_bits [n] : The number of Huffman codes of length "n+1"
**		huff_values [n] : The Huffman Values sorted by code length.
**
**		The first two arrays are used to encode Huffman values. The last two
**		are for writing the table to the output file.
**		
**		The code generation process is:
**		
**		1. Arrange the Huffman Values into a binary tree so that the most
**		frequently used codes are closest to the root of the tree. At the end
**		of this process the temporary array codesize [n] contains the length
**		of the pure Huffman code for the value "n"
**		
**		2. Determine the number of Huffman Codes of for each code length. This
**		step places the number of codes of length "n+1" in huff_bits[].
**		
**		3. The JPEG standard only allows Huffman codes of up to 16-bits. If any
**		 codes longer than 16-bits were generated in the previous steps then
**		 we need to reduce the maximum depth of the tree created in step 1.
**		 The input and output to this step is the array huff_bits[] created in
**		 the previous step.
**		
**		4. Remove the dummy all 1-bit code (See the Reset() function).
**		
**		5. Sort the Huffman values in code length order. codesize [n] is the
**		 input to this step and huff_values [n] is the output. At this point
**		 all the information needed to write the Huffman Table to the output
**		 stream has been found.
**		
**		6. Determine the code size for each value. At the end of this step
**		   the temporary array huffsizes [n] is the Huffman code length for
**		   huff_values [n].
**		
**		7. Determine the Huffman code for each value. The temporary array
**		 huffcodes [n] is the Huffman Code of length huffsizes [n] for
**		 the value huff_value [n].
**		
**		8. Using huffsizes [] and huffcodes created in steps 6 and 7 create
**		 the arrays ehufco [n] and ehufsi [n] giving the Huffman Code and
**		 Code Size for n.
**
**    PARAMETERS
**       INPUT  :
**				JpegEncoderHuffmanTable: tbl
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void BuildTable (tbl)
JpegEncoderHuffmanTable *tbl;
{
	int ii, kk,jj ;
	unsigned int huffsizes [JpegMaxNumberOfHuffmanCodes] ;
	unsigned int tmp [JpegMaxNumberOfHuffmanCodes + 1] ;
	int others [JpegMaxNumberOfHuffmanCodes + 1] ;
	unsigned int codesize [JpegMaxNumberOfHuffmanCodes + 1] ;
	int v1, v2;
	unsigned int huffcodes [JpegMaxNumberOfHuffmanCodes] ;
	unsigned int si ;
	unsigned int count = 0 ;
	UBYTE2 code = 0 ;

	if (tbl->sizes_found)
		return ;
/*
......The tmp array is used for validating the integrity of the Huffman code
......table. We need a temporary array since frequencies [] gets trashed
......during the code generation process.
*/
	for (ii = 0 ; ii < JpegMaxNumberOfHuffmanCodes + 1 ; ++ ii)
		tmp [ii] = tbl->frequencies [ii] ;

/*
......Build the Huffman Code Length Lists
*/
	for (ii = 0 ; ii < JpegMaxNumberOfHuffmanCodes + 1 ; ++ ii)
		others [ii] = -1 ;

	memset (codesize, 0, sizeof (codesize)) ;
	while (1)
	{
/*
......Find the two smallest non-zero values
*/
		v1 = -1 ;
		v2 = -1 ;
		for (ii = 0 ; ii < JpegMaxNumberOfHuffmanCodes + 1 ; ++ ii)
		{
			if (tbl->frequencies [ii] != 0)
			{
				if (v1 < 0 || tbl->frequencies [ii] <= tbl->frequencies [v1])
				{
					v2 = v1 ;
					v1 = ii ;
				}
				else if (v2 < 0 || tbl->frequencies [ii] <= tbl->frequencies [v2])
				{
					v2 = ii ;
				}
			}
		}
		if (v2 < 0)
			break ;

		tbl->frequencies [v1] = tbl->frequencies [v1] + tbl->frequencies [v2] ;
		tbl->frequencies [v2] = 0 ;

		for (++ codesize [v1] ; others [v1] >= 0 ; ++ codesize [v1])
			v1 = others [v1] ;

		others [v1] = v2 ;

		for (++ codesize [v2] ; others [v2] >= 0 ; ++ codesize [v2])
			v2 = others [v2] ;
	}

	memset (tbl->huff_bits, 0, sizeof (tbl->huff_bits)) ;
	for (ii = 0 ; ii < JpegMaxNumberOfHuffmanCodes + 1 ; ++ ii)
	{
		if (codesize [ii] != 0)
		{
			++ tbl->huff_bits [codesize [ii] - 1] ;
		}
	}

	for (ii = 2 * JpegMaxHuffmanCodeLength -  1 ;
				ii >= JpegMaxHuffmanCodeLength ;
				-- ii)
	{
		while (tbl->huff_bits [ii] != 0)
		{
			unsigned int jj = ii - 1 ;
			do
			{
				-- jj ;
			}
			while (tbl->huff_bits [jj] == 0) ;

			tbl->huff_bits [ii] -= 2 ;
			++ tbl->huff_bits [ii - 1] ;
			tbl->huff_bits [jj + 1] += 2 ;
			-- (tbl->huff_bits [jj] );
		}
	}

/* 
......Remove the reserved code from the end of the list.
*/
	for (ii = JpegMaxHuffmanCodeLength - 1 ; ii >= 0 ; -- ii)
	{
		if (tbl->huff_bits [ii] != 0)
		{
			-- (tbl->huff_bits [ii]) ;
			break ;
		}
	}

	for (ii = 0 ; ii < JpegMaxHuffmanCodeLength ; ++ ii)
	{
		count += tbl->huff_bits [ii] ;
	}

	memset (tbl->huff_values, 0, sizeof (tbl->huff_values)) ;
	for (ii = 1, kk = 0 ; ii < 2 * JpegMaxHuffmanCodeLength ; ++ ii)
	{
		for (jj = 0 ; jj < JpegMaxNumberOfHuffmanCodes ; ++ jj)
		{
			if (codesize [jj]  == ii)
			{
				tbl->huff_values [kk] = jj ;
				++ kk ;
			}
		}
	}

	memset (huffsizes, 0, sizeof (huffsizes)) ;
	for (ii = 0, kk = 0 ; ii < JpegMaxHuffmanCodeLength ; ++ ii)
	{
		for (jj = 0 ; jj < tbl->huff_bits [ii] ; ++ jj)
		{
			huffsizes [kk] = ii + 1 ;
			++ kk ;
		}
		huffsizes [kk] = 0 ;
	}


	memset (huffcodes, 0, sizeof (huffcodes)) ;
	for (kk = 0, si = huffsizes [0] ;
				huffsizes [kk] != 0  ;
				++ si, code <<= 1)
	{
		for ( ; huffsizes [kk] == si ; ++ code, ++ kk)
		{
			huffcodes [kk] = code ;
		}
	}

	memset (tbl->ehufco, 0, sizeof (tbl->ehufco)) ;
	memset (tbl->ehufsi, 0, sizeof (tbl->ehufsi)) ;
	for (kk = 0 ; kk < JpegMaxNumberOfHuffmanCodes ; ++ kk)
	{
		if (huffsizes [kk] != 0)
		{
			ii = tbl->huff_values [kk] ;
			tbl->ehufco [ii] = huffcodes [kk] ;
			tbl->ehufsi [ii] = huffsizes [kk] ;
		}
	}

	for (ii = 0 ; ii < JpegMaxNumberOfHuffmanCodes ; ++ ii)
	{
		count = 0 ;
		if (tmp [ii] != 0)
		{

			for (jj = 0 ; jj < JpegMaxNumberOfHuffmanCodes ; ++ jj)
			{
				if (ii == tbl->huff_values [jj] && huffsizes [jj] != 0)
					++ count ;
			}
		}
	}

	tbl->sizes_found = 1 ;
	return ;
}
/********************************************************************
**    I_FUNCTION: PrintSequentialScan (scan)
**        This function writes a sequential scan to the output stream.
**
**    PARAMETERS
**       INPUT  :
**			scan:   The scan descriptor
**				
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void PrintSequentialScan (scan)
Scan scan;
{
	if (jpeg.scan_component_count != 1)
	{
		OutputRestartInterval (jpeg.rows_per_restart * jpeg.mcu_cols) ;
	}
	else
	{
		OutputRestartInterval (jpeg.rows_per_restart
			* (jpeg.scan_components [0])->du_cols) ;
	}

	if ((scan.component_mask & (1<<YComponent)) != 0)
	{
		jpeg.dc_tables [0].frequencies [256] = 1 ;
		jpeg.ac_tables [0].frequencies [256] = 1 ;
		jpeg.dc_tables [0].sizes_found = 0;
		jpeg.ac_tables [0].sizes_found = 0;
	}
	if ((scan.component_mask & (1<<YComponent)) == 0
		|| jpeg.scan_component_count != 1)
	{
		jpeg.dc_tables [1].frequencies [256] = 1 ;
		jpeg.ac_tables [1].frequencies [256] = 1 ;
		jpeg.dc_tables [1].sizes_found = 0;
		jpeg.ac_tables [1].sizes_found = 0;
	}

	if (jpeg.scan_component_count != 1)
	{
		InterleavedPass (0,
				cmpt_EncodeSequential,
				cmpt_GatherDcData,
				cmpt_GatherAcData,
				0, JpegSampleSize - 1, 0) ;
	}
	else
	{
		NoninterleavedPass (0,
			cmpt_EncodeSequential,
			cmpt_GatherDcData,
			cmpt_GatherAcData,
			0, JpegSampleSize - 1, 0) ;
	}

	if ((scan.component_mask & (1<<YComponent)) != 0)
	{
		BuildTable (&(jpeg.dc_tables [0])) ;
		BuildTable (&(jpeg.ac_tables [0])) ;
	}
	if ((scan.component_mask & (1<<YComponent)) == 0
		|| jpeg.scan_component_count != 1)
	{
		BuildTable (&(jpeg.dc_tables [1])) ;
		BuildTable (&(jpeg.ac_tables [1])) ;
	}

	jpg_PrintHuffmanTables (scan, 1, 1) ;

	OutputMarker (SOS) ;
	OutputWord (6 + 2 * jpeg.scan_component_count) ;
	OutputByte (jpeg.scan_component_count) ;

	if ((scan.component_mask & (1<<YComponent)) != 0)
	{
		OutputByte (0x01) ;
		OutputByte (0x00) ;
	}
	if (! jpeg.gray_scale)
	{
		if ((scan.component_mask & (1<<CbComponent)) != 0)
		{
			OutputByte (0x02) ;
			OutputByte (0x11) ;
		}
		if ((scan.component_mask & (1<<CrComponent)) != 0)
		{
			OutputByte (0x03) ;
			OutputByte (0x11) ;
		}
	}

	OutputByte (0) ;
	OutputByte (JpegSampleSize - 1) ;
	OutputByte (0) ;

	if (jpeg.scan_component_count != 1)
	{
		InterleavedPass (1,
				cmpt_EncodeSequential,
				cmpt_PrintDcData,
				cmpt_PrintAcData,
				0, JpegSampleSize - 1, 0) ;
	}
	else
	{
		NoninterleavedPass (1,
				cmpt_EncodeSequential,
				cmpt_PrintDcData,
				cmpt_PrintAcData,
				0, JpegSampleSize - 1, 0) ;
	}

	return ;
}

/********************************************************************
**    I_FUNCTION : jpg_FindComponentsInScan (scan)
**        This function determines which components is in a given scan.
**
**    PARAMETERS
**       INPUT  :
**			scan:  The structure that contains the scan parameters.
**			
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void jpg_FindComponentsInScan (scan)
Scan scan;
{
   jpeg.scan_component_count = 0 ;
	if ((scan.component_mask & (1 <<YComponent)) != 0)
	{
		jpeg.scan_components [jpeg.scan_component_count] = 
					&(jpeg.image_components [YComponent]) ;
		++ jpeg.scan_component_count ;
	}
	if (! jpeg.gray_scale)
	{
		if ((scan.component_mask & (1 <<CbComponent)) != 0)
		{
			jpeg.scan_components [jpeg.scan_component_count]
					= &(jpeg.image_components [CbComponent]) ;
			++ (jpeg.scan_component_count) ;
		}
		if ((scan.component_mask & (1 <<CrComponent)) != 0)
		{
			jpeg.scan_components [jpeg.scan_component_count]
					= &(jpeg.image_components [CrComponent]) ;
			++ (jpeg.scan_component_count) ;
		}

	}
	return ;
}
/********************************************************************
**    I_FUNCTION: PrintSequentialFrame ()
**        This function writes a sequential frame to the output stream. We create
**			frames with either one (black & white) or three (color) components.
**
**    PARAMETERS
**       INPUT  :
**			none
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void PrintSequentialFrame ()
{
	int ii;
  if (jpeg.gray_scale)
  {
    ++ jpeg.current_pass ;
    cmpt_SampleYComponent (YComponent, jpeg.max_horizontal_frequency,
                                     jpeg.max_vertical_frequency) ;
  }
  else
  {
    ++ jpeg.current_pass ;
    cmpt_SampleYComponent (YComponent, jpeg.max_horizontal_frequency,
                                     jpeg.max_vertical_frequency) ;
    ++ jpeg.current_pass ;
    cmpt_SampleCbComponent (CbComponent, jpeg.max_horizontal_frequency,
                                     jpeg.max_vertical_frequency) ;
    ++ jpeg.current_pass ;
    cmpt_SampleCrComponent (CrComponent, jpeg.max_horizontal_frequency,
                                     jpeg.max_vertical_frequency) ;
  }

  OutputMarker (SOF0) ;
  if (jpeg.gray_scale) 
    OutputWord (8 + 3) ;
  else
    OutputWord (8 + 3 * 3) ;

  OutputByte (8) ;
  OutputWord (image.image_height) ;
  OutputWord (image.image_width) ;

  if (jpeg.gray_scale)
  {
    OutputByte (1) ;
    OutputByte(YComponent) ;
    OutputByte (
        (jpeg.image_components [YComponent].h_frequency << 4)
        | jpeg.image_components [YComponent].v_frequency) ;
    OutputByte (0) ;


    PrintQuantizationTables () ;
    jpeg.scan_component_count = 1 ;
    jpeg.scan_components [0] = &jpeg.image_components [YComponent] ;
    ++ jpeg.current_pass ;
    PrintSequentialScan (jpeg.image_scans [0]) ;
  }
  else
  {
    OutputByte (3) ;
    OutputByte(YComponent) ;
    OutputByte (
        (jpeg.image_components [YComponent].h_frequency << 4)
        | jpeg.image_components [YComponent].v_frequency) ;
    OutputByte (0) ;

    OutputByte(CbComponent) ;
    OutputByte (
        (jpeg.image_components [CbComponent].h_frequency << 4)
        | jpeg.image_components [CbComponent].v_frequency) ;
    OutputByte (1) ;

    OutputByte(CrComponent) ;
    OutputByte (
        (jpeg.image_components [CrComponent].h_frequency << 4)
        | jpeg.image_components [CrComponent].v_frequency) ;
    OutputByte (1) ;

    PrintQuantizationTables () ;

    for (ii = 0 ; ii < jpeg.scan_count ; ++ ii)
    {
      ++ jpeg.current_pass ;
      jpg_FindComponentsInScan (jpeg.image_scans [ii]) ;
      PrintSequentialScan (jpeg.image_scans [ii]) ;
    }
  }

  return ;
}

/********************************************************************
**    I_FUNCTION     : jpg_WriteImage (jpgfile) 
**        This function write a saved image into a JPEG file
**		
**
**    PARAMETERS
**       INPUT  :
**			jpgfile:  JPEG file name
**			
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static jpg_WriteImage (jpgfile) 
char *jpgfile;
{
	jpg_fptr = fopen(jpgfile,"wb");
	jpeg.bit_count = 0 ;
	jpg_CreateQuantizationTables (jpeg.image_quality) ;

	jpeg.frame_height = image.image_height;
	jpeg.frame_width = image.image_width ;

	jpg_CalculateMcuDimensions () ;

	jpg_ValidateParameters () ;

	jpeg.current_pass = 0 ;
	if (jpeg.gray_scale)
		jpeg.total_passes = 2 ;
	else
		jpeg.total_passes = 3 + jpeg.scan_count ;

	OutputMarker (SOI) ;
	OutputMarker (APP0) ;
	OutputJfifHeader () ;
	PrintSequentialFrame () ;

	OutputMarker (EOI) ;
	free (jpeg.image_components [YComponent].dct_coefficients);
	free (jpeg.image_components [CbComponent].dct_coefficients);
	free (jpeg.image_components [CrComponent].dct_coefficients);
	fclose(jpg_fptr);
	return 0;
}

/********************************************************************
**    I_FUNCTION :image_GetRGB (row, col, red, green, blue)
**        This function returns the RGB values for a pixel in the bitmap at the
**			point [row,col] where row=[0..height-1] and col=[0..width-1].
**
**    PARAMETERS
**       INPUT  :
**			row, col: The position in the image to return data from
**			red, green, blue: RGB color
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void image_GetRGB (row, col, red, green, blue)
unsigned int row, col;
UBYTE1 *red, *green, *blue;
{
	uw_glget_pixel(image.image_height, image.image_width, row, col, red, green,
		blue);
	return ;
}

#define RowRounding 4
unsigned int RoundRow (width)
unsigned int width;
{
	unsigned int result = (width + RowRounding - 1)
                      & ~(RowRounding - 1) ;
	return result ;
}


/********************************************************************
**    I_FUNCTION     :  image_SetSize (cc, bits,ww,hh)
**			Set image size
**
**    PARAMETERS
**       INPUT  :
**			cc:    color count
**			bits:  color bits
**			ww:   image width
**			hh:   image height
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void image_SetSize (cc, bits,ww,hh)
unsigned int cc, bits,ww,hh;
{
	unsigned int bitsize, bytecount;
	image.bit_count = bits ;
	image.color_count = cc ;
	image.image_width = ww ;
	image.image_height = hh ;
	bitsize = image.bit_count * image.image_width ;
	image.row_width = RoundRow ((bitsize + 7)/8) ;
	bytecount = image.row_width * image.image_height ;
	image.image_data =  (unsigned char *) malloc (bytecount*
										sizeof (UBYTE1)) ;
}

/********************************************************************
**    I_FUNCTION     :  img_Initialize ()
**			Initial image data
**
**    PARAMETERS
**       INPUT  :
**			none
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
static void img_Initialize ()
{
	image.bit_count = 0 ;
	image.image_width = 0 ;
	image.image_height = 0 ;
	image.color_count = 0 ;
	image.color_map = NULL ;
	image.image_data = NULL ;
}

/********************************************************************
**    I_FUNCTION     :  uw_create_jpg(jpgfile, bcolor)
**			This function create a JPEG file from current screen
**
**    PARAMETERS
**       INPUT  :
**			jpgfile: JPEG file name
**			bcolor: background color (white/black)
**			
**       OUTPUT :
**               none
**
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none
*********************************************************************/
uw_create_jpg(jpgfile, bcolor)
char *jpgfile;
int bcolor;
{
	int width, height;

	jpg_Initialize ();
	jpg_SetScanAttributes (0, 2, 0, 0) ;
	jpg_SetScanAttributes (1, 4, 0, 0) ;
	jpg_SetScanAttributes (2, 8, 0, 0) ;

	uw_get_prtsize(&width, &height);
	img_Initialize ();
	image_SetSize (256, 8, width, height) ;

	jpg_SetGrayscale(0);
	jpg_WriteImage (jpgfile) ;
	if (image.image_data!=NULL)
		free (image.image_data);
	return 0 ;
}
