/*********************************************************************
**    NAME         :  nccs.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**       nccs.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:34
*********************************************************************/
#ifndef NCCS_H
#include "usysdef.h"
#include "ulist.h"
#include "mfort.h"

struct NCL_id_rec /* A structure holding identity data of Unibase entity*/
   {                     
   UU_KEY_ID key;
   int rel_num;
   char label[64];
   UU_REAL labloc[3];
   UU_REAL ldrloc[3];
   int subscr;
   };

struct NCL_fixed_databag /* A structure large enough to hold fixed data of */
   {                     /* largest entity                                 */
   UU_KEY_ID key;
   int rel_num;
   char label[64];
   UU_REAL labloc[3];
   UU_REAL ldrloc[3];
   int subscr;
   char data[1024];
   };

struct NCL_crvgen_rec
  {
  UU_REAL x;
  UU_REAL y;
  UU_REAL z;
  UU_REAL a;
  UU_REAL b;
  UU_REAL c;
  UU_REAL dx;
  UU_REAL dy;
  UU_REAL dz;
  UU_REAL ch;
  int inv;
  };

struct NCL_uvbncv /* A structure holding evolved trimmed surface boundary data*/
	{
	int no_csg;
	int *segix;
	};

struct NCL_uvconv /* A structure holding evolved trimmed surface boundaries data*/
   {                     
	int no_key;
   UU_KEY_ID *crvid;
   int no_bcv;
   struct NCL_uvbncv *bncrv;
   };

struct NCL_cvonsf_rec /* A structure holding evolved curve on surface */
{
   UU_KEY_ID cvkey;
   UU_KEY_ID sfkey;
   UM_real8 cvlen;
   UU_LIST *uvlst;
   UU_LIST *xylst;
};

/* 
... types of lists in a surface record to be stored/retrieved in motion
... with multiple PS
*/
typedef enum 
{
	DISPLAY_LIST, 
	TESSELLATION_LIST, 
	BOX_LIST, 
	UV_BOX_LIST,
	WHOLE_BOUNDARY_LIST,
	SSKEY_LIST 
} NCL_surflist_type;

typedef struct
{
	UU_LOGICAL blanked;
	int shaded;
} NCL_sfs_dispmode;

extern int sbsolv_insert;

/*
.....The following sizes must be multiples of 8
.....Bobby  -  12/1/95
*/
#define    NCL_NCLPT_BUFSZ       8
#define    NCL_NCLLN_BUFSZ       8
#define    NCL_NCLCI_BUFSZ       8
#define    NCL_VECTOR_BUFSZ      8
#define    NCL_NCLPL_BUFSZ       8
#define    NCL_MATRIX_BUFSZ      8
#define    NCL_NCLPV_BUFSZ       8

#ifndef TIGDEFSH
#define    NCL_CURVE_BUFSZ       4000
#define    NCL_PANEL_BUFSZ       7000
#define    NCL_SURFACE_BUFSZ     4000
#define    NCL_MESHSF_BUFSZ      4000
#define    NCL_QUILTSF_BUFSZ     4000
#define    NCL_NETSF_BUFSZ       4000
#define    NCL_PATERN_BUFSZ      1400
#endif

#define    NCL_SHAPE_BUFSZ       3200
#define    NCL_LABTBL_BUFSZ      1000
#define    NCL_EVALCV_BUFSZ      1000
#define    NCL_EVALSF_BUFSZ      4000
#define    UM_RBSPLSRF_BUFSZ     504
#define    NCL_TRIMSF_BUFSZ      504
#define    NCL_DATAST_BUFSZ      504
#define    NCL_REVSURF_BUFSZ     8

#include "nccsddl.h"
#define NCCS_H
#endif
