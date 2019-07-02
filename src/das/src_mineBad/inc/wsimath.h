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
**    MODULE NAME AND RELEASE LEVEL 
**       wsimath.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:11
*********************************************************************/

#ifndef UW_MATH

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

#ifdef UU_SINGLE
#define UU_Rasin asin
#define UU_Racos acos
#define UU_Ratan2 atan2
#define UU_Rsinh sinh
#define UU_Rcosh cosh
#define UU_Rtanh tanh
#define UU_Rpow pow
#define UU_Rsin sin
#define UU_Rcos cos
#define UU_Ratan atan
#define UU_Rexp exp
#define UU_Rlog llog
#define UU_Rsqrt sqrt
#define UU_Rtan tan
#define UU_Rlog10 llog10
#define UU_Rfloor floor
#define UU_Rceil ceil
#define UU_Rldexp lldexp
#define UU_Rfrexp frexp
#define UU_Ratof atof
#define UU_Rfabs fabs
#define UU_Rmodf modf
#define UU_Rfmod fmod
#else
#define UU_Rasin _lasin
#define UU_Racos _lacos
#define UU_Ratan2 _latan2
#define UU_Rsinh _lsinh
#define UU_Rcosh _lcosh
#define UU_Rtanh _ltanh
#define UU_Rpow _lpow
#define UU_Rsin _lsin
#define UU_Rcos _lcos
#define UU_Ratan _latan
#define UU_Rexp _lexp
#define UU_Rlog _llog
#define UU_Rsqrt _lsqrt
#define UU_Rtan _ltan
#define UU_Rlog10 _llog10
#define UU_Rfloor _lfloor
#define UU_Rceil _lceil
#define UU_Rldexp _lldexp
#define UU_Rfrexp _lfrexp
#define UU_Ratof _latof
#define UU_Rfabs _lfabs
#define UU_Rmodf _lmodf
#define UU_Rfmod _lfmod
#endif


#define UW_MATH
#endif

