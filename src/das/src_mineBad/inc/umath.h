/*********************************************************************
**    NAME         : umathlib.h 
**       Mathematical library functions
**		CONTAINS:
**			UU_LOGICAL			um_cceqcc(cc1,cc2)
**			UU_REAL 				um_dcccc(cc1,cc2)
**			int					um_ccnearcc(pt,npts,ptary)
**			UU_REAL				um_mag(v1)
**			UU_REAL				um_dot(v1,v2)
**			UU_REAL				um_angle(vc1,vc2)
**			UU_REAL				um_angle2p(v1, v2, nvec)
**			UU_LOGICAL			um_is_idmat(tfmat);
**			UU_LOGICAL			um_tfeqtf(tfmat1, tfmat2);
**			UU_LOGICAL			um_scale_in_tf
**			int					um_inverttf(a, ainv)
**			UU_LOGICAL			um_vcperp(v1,v2)
**			UU_LOGICAL			um_vcparall(v1,v2)
**			int					umi_arclenfunc(u, resultptr)
**			int					umi_integrate(a, b, func, tol, valptr)
**			UU_REAL				um_getarclen(eptr, tfmat)
**			int					um_closept(pt1, pt2, cpt)
**			UU_LOGICAL			um_ptbtwn(pt1, ptb, pt2)
**			UU_LOGICAL			um_ptinseg(pt1, ptb, pt2)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       umath.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:05
*********************************************************************/

#ifndef UM_MATH

#include <math.h>
#include "usysdef.h"			/* system-wide def's */

extern			UU_LOGICAL			um_cceqcc();
extern			UU_REAL 				um_dcccc();
extern			int					um_ccnearcc();
extern			UU_REAL				um_mag();
extern			UU_REAL				um_dot();
extern			UU_REAL				um_angle();
extern			UU_REAL				um_angle2p();
extern			UU_LOGICAL			um_is_idmat();
extern			UU_LOGICAL			um_tfeqtf();
extern			UU_LOGICAL			um_scale_in_tf();
extern			int					um_inverttf();
extern			UU_LOGICAL			um_vcperp();
extern			UU_LOGICAL			um_vcparall();
extern			int					umi_arclenfunc();
extern			int					umi_integrate();
extern			UU_REAL				um_getarclen();
extern			int					um_closept();
extern			UU_LOGICAL			um_ptbtwn();
extern			UU_LOGICAL			um_ptinseg();

#if UU_COMP==UU_IRIS

#ifdef UU_DOUBLE
#define asin _lasin
#define acos _lacos
#define atan2 _latan2
#define sinh _lsinh
#define cosh _lcosh
#define tanh _ltanh
#define pow _lpow
#define sin _lsin
#define cos _lcos
#define atan _latan
#define exp _lexp
#define log _llog
#define sqrt _lsqrt
#define tan _ltan
#define log10 _llog10
#define floor _lfloor
#define ceil _lceil
#define ldexp _lldexp
#define frexp _lfrexp
#define atof _latof
#define fabs _lfabs
#define modf _lmodf
#define fmod _lfmod
#endif

#endif

#define UM_MATH
#endif

