#ifndef RMTUPLEH
/*********************************************************************
**    NAME         :	rmtuple.h
**       CONTAINS:
**       Master entry
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tzmtuple.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:00
*********************************************************************/
/*
		define the master identifier relation
*/
#define	TZ_MTUPLE_REL		0

/*
		define indexes into attribute table within the mtid,
		and the maximum index allowed
*/

#define	TZ_DATA_INDX	0	/* data index into attribute table			*/
#define	TZ_TRANSF_INDX	1	/* transformation indx into attribute table*/
#define	TZ_ATTR_INDX	2	/* attribute bundle index into attribute table	*/
#define	TZ_UNUS_INDX	3	/* unused index at currrent time				*/
#define	TZ_ASSC_INDX	4	/* associativity index into attribute tbl	*/
#define	TZ_USER_INDX	5	/* a unicad user index into attribute tbl	*/
#define	TZ_MAX_INDX		5	/* maximum index allowed in attribute table*/
/*
		define the bit number for the bit table internal to a 
		master tuple
		unused right now
*/

/*
		define the structure necessary for the master tuple
*/
struct	TZ_mtuple_rec
{
		int	dsegid					;	/* display segment id					*/
		int	save1						;	/* save word 1								*/
		unsigned	long	view_key		;	/* the view key, managed by view sys*/

		/*	define the bit table */
		unsigned	long	bit_tbl		;  /* bit flags								*/

		/* define the attr table */
		unsigned	int	attr_tbl[TZ_MAX_INDX+1]	;
												/* 0 = for geometry tuple				*/
												/* 1 = transformation tuple			*/
												/* 2 = bundled attributes				*/
												/* 3 = unused								*/
												/* 4 = associativity index				*/
												/* 5 = user application index			*/
}	;
#define RMTUPLEH
#endif
