
/*********************************************************************
**    NAME         :  mnns.h
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       mnns.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:31
*********************************************************************/

#ifndef MNNSH


#include "usysdef.h"

#define LIST1		1
#define LIST2		2
#define SURFLIST	3

typedef enum {
					UM_EYE,
					UM_POS
}	UM_LIGHT;


typedef enum {
					UM_ZSORT,
					UM_NNS
}	UM_SORTA;

typedef enum {
					UM_SCREEN,
					UM_FILE,
					UM_ALPHA1
}	UM_POLYOUT;

typedef struct {
	UU_REAL	hue;
	UU_REAL	sat;
	UU_REAL	intmin;
	UU_REAL	intmax;
	int lovlt;
	int hivlt;
	int colorindex;
	unsigned int mtid;
	int segid;
	int lineid;
} Surfatts ;

typedef struct {
	UU_REAL mv1[3];
	UU_REAL mv2[3];
	UU_REAL mv3[3];
	UU_REAL v1[3];
	UU_REAL v2[3];
	UU_REAL v3[3];
	int   mark;
	Surfatts *parent;
} Polygon;

typedef struct {
	UU_REAL key;
	Polygon *poly;
} Sortpair;

extern Polygon 		*umi_list1_head,
  					  		*umi_list2_head;
extern Surfatts		*umi_slist_head;
  
extern UM_LIGHT		UMI_light;
extern UU_REAL			UMI_lightpos[3];
 
extern UU_LOGICAL    UMI_allvlt;
extern UM_SORTA		UMI_sorttype;
extern UM_POLYOUT		UMI_polyout;
  
extern int 				UMI_lu;			/* logical device number for xio */


#define MNNSH
#endif
