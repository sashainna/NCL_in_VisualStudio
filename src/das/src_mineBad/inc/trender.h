/********************************************************************* 
**    NAME : trender.h
**
**		Include file for Romulus rendering code.
**
**  COPYRIGHT  1987  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**          trender.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**          04/29/15 , 15:06:57
**
*********************************************************************/

	typedef struct
	{
		char name[10];
		int *wrkstn_entry;
	}  wrkstn;

/*	-- GKS driver enabling table -- */

#if UU_COMP==UU_PYRAMID
	extern int ws411x, ws410x;
#define GKSTLEN 2
#endif

#if UU_COMP==UU_VAXULTRIX
	extern int ws411x, ws410x;
#define GKSTLEN 2
#endif

#if UU_COMP==UU_RIDGE
	extern int ws411x, ws410x;
#define GKSTLEN 2
#endif

#if UU_COMP==UU_TEK
	extern int ws433x, ws411x;
#define GKSTLEN 2
#endif

#if UU_COMP==UU_SUN
	extern int wssun;
#define GKSTLEN 1
#endif

#if UU_COMP==UU_IRIS
	extern int wsiris;
#define GKSTLEN 1
#endif

#if UU_COMP==UU_IRIS4D
	extern int ws4d;
#define GKSTLEN 1
#endif

#if UU_COMP==UU_APOLLO
	extern int wsapollo;
#define GKSTLEN 1
#endif

#if UU_COMP==UU_VAXVMS
	extern int ws411x, ws410x;
#define GKSTLEN 2
#endif

#if UU_COMP==UU_MASSCOMP
	extern int wsmc, ws411x;
#define GKSTLEN 2
#endif

#if UU_COMP==UU_CIM
	extern int wscim;
#define GKSTLEN 1
#endif

int gkstlen = GKSTLEN;			/* length of choice table */ 
static wrkstn gkstable[GKSTLEN] = {
#if UU_COMP==UU_PYRAMID
									"410x", 	&ws410x,
									"411x", 	&ws411x
#endif
#if UU_COMP==UU_VAXULTRIX
									"410x", 	&ws410x,
									"411x", 	&ws411x
#endif
#if UU_COMP==UU_RIDGE
									"410x", 	&ws410x,
									"411x", 	&ws411x
#endif
#if UU_COMP==UU_TEK
									"host",		&ws433x,
									"411x", 	&ws411x
#endif
#if UU_COMP==UU_SUN
									"host", 	&wssun
#endif
#if UU_COMP==UU_IRIS 
									"host", 	&wsiris
#endif
#if UU_COMP==UU_IRIS4D
									"host", 	&ws4d
#endif
#if UU_COMP==UU_APOLLO
									"host", 	&wsapollo
#endif
#if UU_COMP==UU_VAXVMS
									"410x", 	&ws410x,
									"411X", 	&ws411x
#endif

#if UU_COMP==UU_MASSCOMP
									"host",	&wsmc,
									"411x", 	&ws411x
#endif
#if UU_COMP==UU_CIM
									"host", 	&wscim
#endif
															};
