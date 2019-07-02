/********************************************************************* 
**  NAME:  convert.h 
**
**      Iris workstation convert include file.
**
**  COPYRIGHT  1987  UNICAD, Inc.
**
**    MODULE NAME AND RELEASE LEVEL 
**       wsconvert.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:09
**
*********************************************************************/

#define uw_iris_matprt(a)		{ int i; char us[180];								\
											for (i=0; i<4; i++) {							\
												uu_denter2(UU_GITRC,(us,					\
													"     %g %g %g %g",						\
													(UU_REAL) a[i][0],						\
													(UU_REAL) a[i][1],						\
													(UU_REAL) a[i][2],						\
													(UU_REAL) a[i][3]));						\
												uu_dexit; }										\
										}

#define uw_iris_ident(a)		{ int i,j;												\
											uu_denter(UU_GITRC,(us,                   \
												"uw_iris_ident(a)"));                  \
											for (i=0; i<4; i++) {                     \
												for (j=0; j<4; j++) a[i][j]=0.0;       \
											}                                         \
											for (i=0; i<4; i++) a[i][i]=1.0;          \
											uu_dexit;                                 \
										}

#define uw_ItoGcopy(f,g)		{  int i,j;                                  \
											uu_denter(UU_GITRC,(us,"uw_ItoGcopy"));   \
											for (i=0; i<4; i++) {                     \
												for (j=0; j<4; j++) g[i][j]=f[i][j];   \
											}                                         \
											uu_dexit;                                 \
										}

#define uw_ItoGpoint3(f,g)		{ uu_denter(UU_GITRC,(us,                    \
												"uw_ItoGpoint3"));                     \
											g.x = (*f).x; g.y = (*f).y;					\
											g.z = (*f).z;										\
											uu_dexit;                                 \
										}
