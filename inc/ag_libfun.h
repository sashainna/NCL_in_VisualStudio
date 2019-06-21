/* NAME :  ag_libfun.h        MODULE : Library Functions
** VERSION : 0.8              DATE LAST MODIFIED : 07/08/88
** CONTAINS: Library functions   (C library math.h)
Copyright (c) 1988 Applied Geometry Corporation. All rights reserved.
********************************************************************/

/*
extern double fabs(), floor(), ceil(), fmod(), modf();
extern double sqrt(), hypot(), atof();
extern double sin(), cos(), tan(), asin(), acos(), atan(), atan2();
extern double exp(), log(), log10();
extern double sinh(), cosh(), tanh();
*/

/* ***  numeric constants (21 digits only) *** */
#ifndef PI
#define PI       (double) (3.14159265358979323846)
#endif
#define TWOPI    (double) (6.28318530717958647692)
#define HALFPI   (double) (1.57079632679489661923)
#define ROOT2    (double) (1.41421356237309504880)
#define ROOT3    (double) (1.732050807568877)
#define DBL0     (double) (0.00)
#define DBL1     (double) (1.0)
#ifndef HUGE
#define HUGE     (float)  (0.3402823466e+39)
#endif
#define INF      (double) (1.0e15)

