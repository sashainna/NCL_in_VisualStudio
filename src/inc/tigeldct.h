/*********************************************************************
**    NAME         :  rireldct.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 15:10:27  2015
*********************************************************************/
#include "rireldef.h"

static struct relblk	UR_rels[] = {
{"bptr", 1, 0, 2,},
{"prop", 1, 2, 2,},
{"pt2", 1, 4, 2,},
{"pt3", 1, 6, 2,},
{"pt6", 1, 8, 3,},
{"igespt", 1, 11, 5,},
{"igesline", 1, 16, 5,},
{"igespln", 1, 21, 7,},
{"igesarc", 1, 28, 7,},
{"cid", 1, 35, 2,},
{"cid1", 1, 37, 2,},
{"igescomp", 1, 39, 5,},
{"igesgrp", 1, 44, 5,},
{"viewvs", 1, 49, 7,},
{"plnassoc", 1, 56, 7,},
{"poly2d", 1, 63, 7,},
{"poly3d", 1, 70, 5,},
{"poly6d", 1, 75, 5,},
{"igescon", 1, 80, 12,},
{"igestran", 1, 92, 4,},
{"t", 1, 96, 2,},
{"t1", 1, 98, 2,},
{"t2", 1, 100, 2,},
{"w", 1, 102, 2,},
{"coef", 1, 104, 4,},
{"rpara", 1, 108, 4,},
{"rspara", 1, 112, 5,},
{"igesplin", 1, 117, 9,},
{"igesrspl", 1, 126, 13,},
{"igesrssf", 1, 139, 17,},
{"igesrlsf", 1, 156, 7,},
{"igesrvsf", 1, 163, 7,},
{"igestbcy", 1, 170, 5,},
{"igesofsf", 1, 175, 6,},
{"crvptr", 1, 181, 4,},
{"igesbndy", 1, 185, 5,},
{"igescvsf", 1, 190, 8,},
{"igesbdsf", 1, 198, 5,},
{"igestrsf", 1, 203, 8,},
{"tu", 1, 211, 2,},
{"tv", 1, 213, 2,},
{"patc", 1, 215, 2,},
{"igesplsf", 1, 217, 10,},
{"gnote", 1, 227, 11,},
{"igesnote", 1, 238, 5,},
{"igeslead", 1, 243, 9,},
{"igesangd", 1, 252, 10,},
{"igesdiad", 1, 262, 7,},
{"igeslind", 1, 269, 8,},
{"igesrad", 1, 277, 6,},
{"igeslabl", 1, 283, 5,},
{"igesgsym", 1, 288, 6,},
{"igessfd", 1, 294, 7,},
{"igessfi", 1, 301, 6,},
{"igesvie", 1, 307, 11,},
{"v_tab", 1, 318, 4,},
{"igesdrw", 1, 322, 5,},
{"igesprop", 1, 327, 5,},
{"igesvlst", 1, 332, 3,},
{"sp_crv", 1, 335, 6,},
{"igeselst", 1, 341, 3,},
{"par_sp", 1, 344, 3,},
{"edge", 1, 347, 7,},
{"igesloop", 1, 354, 3,},
{"igesface", 1, 357, 5,},
{"ornface", 1, 362, 3,},
{"igeshell", 1, 365, 5,},
{"ovshell", 1, 370, 3,},
{"igesolid", 1, 373, 7,},
{"igesclr", 1, 380, 5,}};
#ifdef DATDCT1H
#include "ridctdef.h"

static struct UR_dct UR_datadict = {
"", 0, UR_rels, 70, UR_attribs, 385};
#else
#define DATDCT1H 1
#endif
