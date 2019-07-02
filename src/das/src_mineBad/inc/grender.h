/********************************************************************* 
**  NAME:  grender.h
**
**      Rendering include file
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       grender.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:22
**
*********************************************************************/

#ifndef GRENDERH


/* Necessary include files */
#include "gobas.h"
#include "gobndl.h"

#define MAXRES 2048						/* Maximum resolution we can handle */

typedef struct {
	Gfloat p[3];			/* World coordinate position */
	Gfloat n[3];			/* World coordinate normal */
} Vertex;

struct Edge_struct {
					struct Polygon_struct *poly;	/* Pointer to parent polygon */
					Vertex *v1, *v2;					/* Pointer to verticies */
					struct Edge_struct *nxt;		/* Pointer to next edge */
};
typedef struct Edge_struct Edge;

struct Polygon_struct {
			Gfloat a, b, c, d;				/* Plane equation coeficients */
			int active;							/* Flag indicating this polygon is
													 * currently active */
			struct Edge_struct *edge;		/* Pointer to first edge of polygon */
			struct Polygon_struct *nxt;	/* Pointer to next polygon */
			Gflbundl *parent;					/* This polygon's parent */
};
typedef struct Polygon_struct Polygon;


/* One element of the spanbuffer list, a Spanpoint */
struct Span_struct {
	Gfloat x, z;						/* X and Z coordinates of this point */
	Gfloat dz;							/* Delta Z / Delta X */
	Gfloat xlen;						/* Length of span in X */
	Gfloat nx, ny, nz;				/* Normal at left end of span */
	Gfloat dnx, dny, dnz;			/* Delta normal / Delta X */
	Polygon *poly;						/* Pointer to visible polygon */
};
typedef struct Span_struct Span;

/* One element of the active edge list, an Active */
typedef struct {
	Edge *edge;							/* Pointer to the active edge */
	int used;							/* True if we've shaded this span */
}	Active;
	
/* A pixel element */
typedef struct {
	unsigned char r, g, b;
} Pixel;

#define GRENDERH
#endif
