
/********************************************************************* 
**  NAME:  tmedcut.h
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       tmedcut.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:55
**
*********************************************************************/

/* Typedefs for median cut algorithm */
#define R 0
#define G 1
#define B 2
struct color_struct {
	short val[3];
	int count;
	struct color_struct *nxt;
};

typedef struct color_struct Color;

struct node_struct {
	Color *color;
	int partition;
	int discriminator;
	struct node_struct *left;
	struct node_struct *right;
};

typedef struct node_struct Node;

typedef struct {
	Node *node;
	int rmin, rmax;
	int gmin, gmax;
	int bmin, bmax;
} Qelement;

/*	-- I  had to union the color struct to an int for the SGI 4D. The
		code rewquires that the structure is equivalent to an int.  -- */

typedef struct { 
		union {
			int scratch;
			struct { unsigned char red, green, blue, unused; } color;
		} uname;
	} UT_COLOR_REC;

#define MAXVLT 4096
