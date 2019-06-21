/* NAME :  ag_stroper.h       MODULE : All 
** VERSION : 0.5              DATE LAST MODIFIED : 02/01/88
** CONTAINS: Structure operators
Copyright (c) 1987 Applied Geometry Corporation. All rights reserved.
********************************************************************/

#define  NEXT(b)  b = b->next
#define  PREV(b)  b = b->prev

#define UNEXT(b)  b = b->unext
#define VNEXT(b)  b = b->vnext
#define UPREV(b)  b = b->uprev
#define VPREV(b)  b = b->vprev
