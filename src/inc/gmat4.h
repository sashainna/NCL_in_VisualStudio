/*********************************************************************
**  NAME:   gmat4.h
**
**    CONTAINS:
**			ug_mcopy
**
**
**    COPYRIGHT  1985  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL
**       gmat4.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:21
**
*********************************************************************/
#ifndef GMAT4H

#include "gobas.h"

typedef struct { Gfloat a[16]; } ug_tmp_mcopy_st;
#define ug_mcopy(y,x) *(ug_tmp_mcopy_st *)(y) = *(ug_tmp_mcopy_st *)(x)

#define UG_XFORM(xx,yy,zz,npos,a) { Gfloat w;\
	(*npos).x=xx*a[0][0]+yy*a[1][0]+zz*a[2][0]+a[3][0];\
	(*npos).y=xx*a[0][1]+yy*a[1][1]+zz*a[2][1]+a[3][1];\
	(*npos).z=xx*a[0][2]+yy*a[1][2]+zz*a[2][2]+a[3][2];\
	w=xx*a[0][3]+yy*a[1][3]+zz*a[2][3]+a[3][3];\
	if ((w<0.00001)&&(w> -.00001)) { /*uu_dprint(-1,(us,"ug_xform. w=%g\n",w))*/; }\
	else {\
		(*npos).x=(*npos).x/w; (*npos).y=(*npos).y/w; (*npos).z=(*npos).z/w; } }

#define GMAT4H
#endif
