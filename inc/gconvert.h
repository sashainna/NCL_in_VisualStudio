/********************************************************************* 
**  NAME:  gconvert.h 
**
**      Double/single precision conversion macros include file.
**
**  COPYRIGHT  1987  UNICAD, Inc.
**
**    MODULE NAME AND RELEASE LEVEL 
**       gconvert.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:16
**
*********************************************************************/

#ifndef GCONVERTH
#define GCONVERTH

#define ug_GtoIcopy(f,g)		{  int i,j;                                  \
											uu_denter(UU_GITRC,(us,"ug_GtoIcopy"));   \
											for (i=0; i<4; i++) {                     \
												for (j=0; j<4; j++) f[i][j]=g[i][j];   \
											}                                         \
											uu_dexit;                                 \
										}

	/* copy Gfloat to float 3-d point structure */
#define ug_GtoIpoint3(s,d)		{ uu_denter(UU_GITRC,(us,                    \
												"ug_GtoIpoint3"));                     \
											s.x = d.x; s.y = d.y;							\
											s.z = d.z;											\
											uu_dexit;                                 \
										}

	/* copy Gfloat to float point structure */
#define ug_GtoIpoint(s,d)		{ uu_denter(UU_GITRC,(us,                    \
												"ug_GtoIpoint"));                     	\
											s.x = d.x; s.y = d.y;							\
											uu_dexit;                                 \
										}

#define ug_GtoIlntype(f,g) {					\
	f.typeno = (*g).typeno;						\
	f.npatn = (*g).npatn;						\
	f.patnlen = (*g).patnlen;					\
	strcpy(f.typepatn,(*g).typepatn);		\
}

#define ug_ItoGcopy(f,g)		{  int i,j;                                  \
											uu_denter(UU_GITRC,(us,"ug_ItoGcopy"));   \
											for (i=0; i<4; i++) {                     \
												for (j=0; j<4; j++) f[i][j]=g[i][j];   \
											}                                         \
											uu_dexit;                                 \
										}

#define ug_ItoGpoint3(d,s)		{ uu_denter(UU_GITRC,(us,                    \
												"ug_ItoGpoint3"));                     \
											d.x = s.x; d.y = s.y;							\
											d.z = s.z;											\
											uu_dexit;                                 \
										}

#define ug_ItoGpoint(d,s)		{ uu_denter(UU_GITRC,(us,                    \
												"ug_ItoGpoint"));                     	\
											d.x = s.x; d.y = s.y;							\
											uu_dexit;                                 \
										}

#define ug_ItoGlntype(g,f) {					\
	g.typeno = (*f).typeno;						\
	g.npatn = (*f).npatn;						\
	g.patnlen = (*f).patnlen;					\
	strcpy(g.typepatn,(*f).typepatn);		\
}


#define ug_matmp(c,a,b)          /* 4x4 matrix multiply. c=a*b */		\
			{ int i,j;																	\
	 			uu_denter(UU_GITRC,(us,"ug_matmp(c,a,b)- macro"));			\
				for (i=0; i<4; i++) {												\
    				for (j=0; j<4; j++)												\
      				c[i][j]=a[i][0]*b[0][j]+a[i][1]*b[1][j]+a[i][2]*b[2][j]+	\
              		a[i][3]*b[3][j];												\
  				}																			\
  				uu_dexit;																\
			}

#endif
