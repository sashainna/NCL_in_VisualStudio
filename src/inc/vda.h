/*********************************************************************
**    NAME         :  vda.h
**       CONTAINS: Definitions for VDA translator.
**
**    COPYRIGHT 1984 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       vda.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:07
*********************************************************************/

#ifndef VDAH


/***************************************************************************/
/*                                                                         */
/*                                                                         */
/*   include file for  VDA translator project                              */
/*                                                                         */
/*                                                                         */
/***************************************************************************/
/*                                                                         */
#define VDA_NUM 17

  
#define vda_header_type   1
#define vda_beginset_type 2
#define vda_endset_type   3
#define vda_group_type    4
#define vda_tmat_type     5
#define vda_tlist_type    6
#define vda_end_type      7
#define vda_point_type    8
#define vda_pset_type     9
#define vda_mdi_type     10
#define vda_circle_type  11
#define vda_curve_type   12
#define vda_surf_type    13
#define vda_cons_type    14
#define vda_face_type    15
#define vda_top_type     16

struct vda_ent {
   char name[9];  /* entity name; left justified, blank fill, ending with \0 */
   int  type;     /* entity type */
   int  recnum;   /* record number of entity in vda input file */
   int  pchar;    /* first char of param data (0 if no parameter) */
   int  tran;     /* directory entry index of associated transform */
   int  group;    /* directory entry index of group, else 0 */
   };


#define VDAH
#endif
