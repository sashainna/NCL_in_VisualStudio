/********************************************************************* 
**  NAME:  ogcom.h 
**
**		Global structure definitions
**
**	CONTAINS: 
**
**    COPYRIGHT 2003 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       ogcom.h , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:40
*********************************************************************/
typedef enum
{
	BUFFER_FRONT,
	BUFFER_BACK,
	BUFFER_BOTH,
	BUFFER_OVERLAY
} OG_buffer;

typedef enum
{
	COPY_SWAP,
	COPY_SMALL,
	COPY_LARGE,
	COPY_FULL
} OG_copy;

typedef enum
{
	CUTTER_WIRE,
	CUTTER_SHADED
} OG_cutter;

typedef enum
{
	STYLE_REDRAW,
	STYLE_PIXEL
} OG_style;

typedef struct
{
	int llx;
	int lly;
	int wid;
	int hgt;
} OG_boxarea;

typedef struct
{
	OG_boxarea win;
	OG_boxarea ebox;
	OG_buffer buffer;
	OG_copy copy;
	OG_cutter cutter;
	OG_style style;
	int depth;
#ifdef WIN32
   HDC hdc;
#endif
} OG_methods;
