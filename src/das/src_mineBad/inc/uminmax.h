

/*********************************************************************
**    NAME         :  uminmax.h
**       CONTAINS:
**
**				The max or min macros MAXi, and MINi (1 < i < 11).
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uminmax.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:05
**
**    RETURNS      : Each macro returns the max or min of its i arguments.
**	   SIDE EFFECTS : none
**    WARNINGS: The include file declares a static variable uu_minmax.
**
*************************************************************************/

#ifndef UMINMAXH


static double uu_minmax;

#define MAX2(arg1, arg2)	( (arg1) > (arg2) ? (arg1) : (arg2) )

#define MAX3(arg1, arg2, arg3) \
		( (arg1) > (uu_minmax = \
		MAX2((arg2),(arg3))) \
		? (arg1) : uu_minmax )

#define MAX4(arg1, arg2, arg3, arg4) \
		( (arg1) > (uu_minmax = \
		MAX3((arg2),(arg3),(arg4))) \
		? (arg1) : uu_minmax )

#define MAX5(arg1, arg2, arg3, arg4, arg5) \
		( (arg1) > (uu_minmax = \
		MAX4((arg2),(arg3),(arg4),(arg5))) \
		? (arg1) : uu_minmax )

#define MAX6(arg1, arg2, arg3, arg4, arg5, arg6) \
		( (arg1) > (uu_minmax = \
		MAX5((arg2),(arg3),(arg4),(arg5),(arg6))) \
		? (arg1) : uu_minmax )

#define MAX7(arg1, arg2, arg3, arg4, arg5, arg6, arg7) \
		( (arg1) > (uu_minmax = \
		MAX6((arg2),(arg3),(arg4),(arg5),(arg6),(arg7))) \
		? (arg1) : uu_minmax )

#define MAX8(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) \
		( (arg1) > (uu_minmax = \
		MAX7((arg2),(arg3),(arg4),(arg5),(arg6),(arg7),(arg8))) \
		? (arg1) : uu_minmax )

#define MAX9(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) \
		( (arg1) > (uu_minmax = \
		MAX8((arg2),(arg3),(arg4),(arg5),(arg6),(arg7),(arg8),(arg9))) \
		? (arg1) : uu_minmax )

#define MAX10(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) \
		( (arg1) > (uu_minmax = \
		MAX9((arg2),(arg3),(arg4),(arg5),(arg6),(arg7),(arg8),(arg9),(arg10))) \
		? (arg1) : uu_minmax )

/* Minimum functions */

#define MIN2(arg1, arg2)	( (arg1) < (arg2) ? (arg1) : (arg2) )

#define MIN3(arg1, arg2, arg3) \
		( (arg1) < (uu_minmax = \
		MIN2((arg2),(arg3))) \
		? (arg1) : uu_minmax )

#define MIN4(arg1, arg2, arg3, arg4) \
		( (arg1) < (uu_minmax = \
		MIN3((arg2),(arg3),(arg4))) \
		? (arg1) : uu_minmax )

#define MIN5(arg1, arg2, arg3, arg4, arg5) \
		( (arg1) < (uu_minmax = \
		MIN4((arg2),(arg3),(arg4),(arg5))) \
		? (arg1) : uu_minmax )

#define MIN6(arg1, arg2, arg3, arg4, arg5, arg6) \
		( (arg1) < (uu_minmax = \
		MIN5((arg2),(arg3),(arg4),(arg5),(arg6))) \
		? (arg1) : uu_minmax )

#define MIN7(arg1, arg2, arg3, arg4, arg5, arg6, arg7) \
		( (arg1) < (uu_minmax = \
		MIN6((arg2),(arg3),(arg4),(arg5),(arg6),(arg7))) \
		? (arg1) : uu_minmax )

#define MIN8(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) \
		( (arg1) < (uu_minmax = \
		MIN7((arg2),(arg3),(arg4),(arg5),(arg6),(arg7),(arg8))) \
		? (arg1) : uu_minmax )

#define MIN9(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) \
		( (arg1) < (uu_minmax = \
		MIN8((arg2),(arg3),(arg4),(arg5),(arg6),(arg7),(arg8),(arg9))) \
		? (arg1) : uu_minmax )

#define MIN10(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) \
		( (arg1) < (uu_minmax = \
		MIN9((arg2),(arg3),(arg4),(arg5),(arg6),(arg7),(arg8),(arg9),(arg10))) \
		? (arg1) : uu_minmax )


#define UMINMAXH
#endif
