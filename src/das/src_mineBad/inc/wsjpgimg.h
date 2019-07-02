/*********************************************************************
**  NAME:  wsjpgimg.h
**
**    CONTAINS:
**			definition and variables for JPEG file
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsjpgimg.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:11
*********************************************************************/
#ifndef _WSJPGIMG_H
#define _WSJPGIMG_H

typedef char BYTE1 ;
typedef unsigned char UBYTE1 ;

typedef short BYTE2 ;
typedef unsigned short UBYTE2 ;
typedef long BYTE4 ;
typedef unsigned long UBYTE4 ;

typedef int (*UW_METHOD)();
enum JpegMarkers
{
	SOF0 = 0xC0,    /* Baseline DCT */
	SOF1 = 0xC1,    /* Sequential DCT  */
	SOF2 = 0xC2,    /* Progressive DCT  */
	SOF3 = 0xC3,    /* Spatial (sequential) lossless  */
	SOF5 = 0xC5,    /* Differential Sequential DCT  */
	SOF6 = 0xC6,    /* Differential Progressive DCT  */
	SOF7 = 0xC7,    /* Differential Spatial  */
	SOF9 = 0xC9,    /* Extended Sequential DCT  */
	SOFA = 0xCA,    /* Progressive DCT  */
	SOFB = 0xCB,    /* Spacial (sequential) Lossless  */
	SOFD = 0xCD,    /* Differential Sequential DCT  */
	SOFE = 0xCE,    /* Differential Progressive DCT  */
	SOFF = 0xCF,    /* Differential Spatial  */
	DHT = 0xC4,     /* Define Huffman Tables  */
	DAC = 0xCC,     /* Define Arithmetic Coding Conditions  */
	RST0 = 0xD0,    /* Restart Marker  */
	RST1 = 0xD1,    /* Restart Marker  */
	RST2 = 0xD2,    /* Restart Marker  */
	RST3 = 0xD3,    /* Restart Marker  */
	RST4 = 0xD4,    /* Restart Marker  */
	RST5 = 0xD5,    /* Restart Marker  */
	RST6 = 0xD6,    /* Restart Marker  */
	RST7 = 0xD7,    /* Restart Marker  */
	SOI = 0xD8,     /* Start of Image  */
	EOI = 0xD9,     /* End of Image  */
	SOS = 0xDA,     /* Start of Scan  */
	DQT = 0xDB,     /* Define Quantization Table  */
	DNL = 0xDC,     /* Define Number of Lines  */
	DRI = 0xDD,     /* Define Restart Intervale  */
	DHP = 0xDE,     /* Define Hierarchical Progression  */
	EXP = 0xDF,     /* Expand Reference Components  */
	APP0 = 0xE0,    /* Application Segments  */
	APP1 = 0xE1,    /* Application Segments  */
	APP2 = 0xE2,    /* Application Segments  */
	APP3 = 0xE3,    /* Application Segments  */
	APP4 = 0xE4,    /* Application Segments  */
	APP5 = 0xE5,    /* Application Segments  */
	APP6 = 0xE6,    /* Application Segments  */
	APP7 = 0xE7,    /* Application Segments  */
	APP8 = 0xE8,    /* Application Segments  */
	APP9 = 0xE9,    /* Application Segments  */
	APPA = 0xEA,    /* Application Segments  */
	APPB = 0xEB,    /* Application Segments  */
	APPC = 0xEC,    /* Application Segments  */
	APPD = 0xED,    /* Application Segments  */
	APPE = 0xEE,    /* Application Segments  */
	APPF = 0xEF,    /* Application Segments  */
	COM = 0xFE,     /* Comment  */
	SOB = 0xFF     /* Start of Block
					- Byte that precedes all others - not in the standard.  */
} ;


#define JpegSampleWidth 8
#define JpegSampleSize  64

typedef UBYTE1 JPEGSAMPLE ; 
typedef BYTE1 JPEGCOEFFICIENT ;

#define JpegMinSampleValue 0
#define JpegMaxSampleValue 255  
#define JpegMidpointSampleValue  128  


#define JpegMaxHuffmanCodeLength 16
#define JpegMaxNumberOfHuffmanCodes 256

#define JpegMax8BitQuantizationValue   255
#define JpegMinQuantizationValue 1

typedef struct
{
	UBYTE2 length ;
	UBYTE1 identifier [5] ;
	UBYTE1 version [2] ;
	UBYTE1 units ;
	UBYTE2 xdensity ;
	UBYTE2 ydensity ;
	UBYTE1 xthumbnail ;
	UBYTE1 ythumbnail ;
} JfifHeader;

enum { JpegThumbnail = 0x10,
		OneByteThumbnail = 0x11,
		ThreeByteThumbnail = 0x13 };

struct JfifExtension
{
	UBYTE2 length ;
	UBYTE1 identifier [5] ;
	UBYTE1 extension_code ;
} ;

typedef struct
{
	UBYTE1 blue ;
	UBYTE1 green ;
	UBYTE1 red ;
} ColorMapEntry;

typedef JPEGSAMPLE DATAUNITVALUE ;

typedef struct
{
	DATAUNITVALUE data [64] ;
} JpegEncoderDataUnit;
	
typedef struct
{
	unsigned int row_width ;
	unsigned int bit_count ;
	unsigned int image_width ;
	unsigned int image_height ;
	unsigned char *image_data ;
	unsigned int color_count ;
	ColorMapEntry * color_map ;
} BitmapImage;

typedef struct
{
	unsigned long component_mask ;
	unsigned int spectral_selection_start ;
	unsigned int spectral_selection_end ;
	unsigned int successive_approximation ;
	int successive_approximation_high ;
	int successive_approximation_low ;
} Scan;

#if UU_COMP == UU_HPUX
typedef JPEGSAMPLE (*RGBTOYCBCRFUNCTION)();
#else
typedef JPEGSAMPLE (*RGBTOYCBCRFUNCTION)(
						JPEGSAMPLE red,
						JPEGSAMPLE green,
						JPEGSAMPLE blue) ;
#endif

typedef struct
{
/*
......frequencies [n] is the number of times the value "n" needs to
......be encoded.
*/
	unsigned int frequencies [JpegMaxNumberOfHuffmanCodes + 1] ;
/*
......Values used to represent Huffman tables in a JPEG stream
......huff_bits [n] is the number of codes of length "n+1"
......huff_values is the list of Huffman values sorted in order
......of code length.
*/
	UBYTE1 huff_bits [2 * JpegMaxHuffmanCodeLength] ;
	UBYTE1 huff_values [JpegMaxNumberOfHuffmanCodes] ;
/*
......Values used to encode values.
......ehufsi [n] is the number of bits required to code "n"
......ehufco [n] is the Huffman code for "n"
*/
	UBYTE1 ehufsi [JpegMaxNumberOfHuffmanCodes+ 1] ;
	UBYTE2 ehufco [JpegMaxNumberOfHuffmanCodes+1] ;
/*
......The flag is set to true when the Huffman code sizes has been
determined.
......It is cleared when the object is Reset().
*/
	int sizes_found ;
} JpegEncoderHuffmanTable;

typedef struct
{
	BYTE2 data [64] ;
/*
		BYTE2 data [JpegSampleWidth][JpegSampleWidth] ;
*/
} JpegEncoderCoefficientBlock;

typedef struct
{
	UBYTE2 data_values [JpegSampleSize] ;
/*
		double float_scaling [JpegSampleWidth][JpegSampleWidth] ;
*/
	double float_scaling [64] ;
} JpegEncoderQuantizationTable;


typedef struct 
{
	unsigned int du_rows ;
	unsigned int du_cols ;
	JpegEncoderCoefficientBlock *dct_coefficients ;

	unsigned int eob_run ;
	unsigned int eob_start_du_row ;
	unsigned int eob_start_du_col ;
	unsigned int eob_start_position ;

	unsigned int v_frequency ;
	unsigned int h_frequency ;
	unsigned int v_period ;
	unsigned int h_period ;

	JpegEncoderHuffmanTable *ac_table;
	JpegEncoderHuffmanTable *dc_table;
	JpegEncoderQuantizationTable *quantization_table ;

	int last_dc_value ;
} JpegEncoderComponent;

enum { MaxScans = 256 } ;
enum { YComponent = 1, CbComponent = 2, CrComponent = 3 } ;
enum { MaxComponents = 4 } ;

Scan jpg_image_scans [256] ;

typedef struct 
{
	unsigned int bit_count ;  
	UBYTE1 bit_buffer ;

	JpegEncoderQuantizationTable chrominance_quanttbl ; 
	JpegEncoderQuantizationTable luminance_quanttbl ;  

	JpegEncoderHuffmanTable ac_tables[2] ;
	JpegEncoderHuffmanTable dc_tables[2] ;

	int gray_scale ;
	unsigned int rows_per_restart ;
	unsigned int restart_interval ;
	unsigned int image_quality ;

	unsigned int total_passes ;
	unsigned int current_pass ;

	unsigned int frame_width ;
	unsigned int frame_height ;
	unsigned int max_horizontal_frequency ;
	unsigned int max_vertical_frequency ;
	unsigned int  mcu_rows ;
	unsigned int  mcu_cols ;

	unsigned int scan_count ;
	Scan image_scans [MaxScans] ;
	JpegEncoderComponent image_components [4] ;
	unsigned int scan_component_count ;
	JpegEncoderComponent *scan_components [4] ;
} JpegEncoder;	

#endif



