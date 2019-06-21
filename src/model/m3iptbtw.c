
/*********************************************************************
**    NAME         :  miptbtwn
**       CONTAINS:
**			um_ptbtwn
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3iptbtw.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:56
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "umath.h"
#include "mdcoord.h"
#include "modef.h"
#include	"mdebug.h"

#define 	PTBTTRC	0

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_ptbtwn( pt1, ptb, pt2 )
**			Return UU_TRUE if point ptb is between point pt1 and point pt2. 
**			(not really on segment between, but more or less between.
**			see also um_ptinseg().)
**    PARAMETERS   
**       INPUT  : 
**				pt1  the first  endpoint
**				ptb  the point to test to be between the endpoints
**				pt2  the second endpoint 
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_TRUE  if ptb is between pt1 and pt2;  UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_ptbtwn( pt1, ptb, pt2 )

	UM_coord		pt1;
	UM_coord		ptb;
	UM_coord		pt2;

	{
	UM_vector	v2;			/* endpoint vector */
	UM_vector	u2;			/* unit vector of v2 */
	UM_length	m2;			/* magnitude of endpoint vector */
	UM_coord		ptp;			/* the projection of the between point on
										the line */
	UM_vector	vp;			/* projection point vector */
	UM_length	mp;			/* magnitude of projection point vector */
	UU_LOGICAL between;		/* TRUE if "ptb" is between "pt1" and "pt2" */
	char		s1[120];

/*--------------------------------------------------------------------
**	Start of Executable Code
**--------------------------------------------------------------------
**
**	Test if point ptb is between point pt1 and pt2
*/
	uu_denter(UU_MTRC,(us,"um_ptbtwn(?,?,?)"));

#if 	PTBTTRC
		sprintf(UM_sbuf,
		"um_ptbtwn: pt1 <%f,%f,%f>, ptbtwn <%f,%f,%f>, pt2 <%f,%f,%f>.",
		pt1[0],pt1[1],pt1[2], ptb[0],ptb[1],ptb[2], pt2[0],pt2[1],pt2[2]);
		um_pscroll(UM_sbuf);
#endif

	between = UU_FALSE;

	if (um_cceqcc(pt1, pt2)) /* start and end points are the same */
		{
		if (um_cceqcc(pt1, ptb)) /* we say "ptb" is between "pt1" and "pt2" */
			between = UU_TRUE;
		else 
			between = UU_FALSE;

#if 	PTBTTRC
		sprintf(UM_sbuf, "um_ptbtwn: short segment, answer %d.", between);
		um_pscroll(UM_sbuf);
#endif

		goto Done;
		}

	/* line not too short	*/
	um_vcmnvc( pt2, pt1, v2 );
	um_unitvc( v2, u2 );
	m2 = um_mag(v2);

	/* get the projection of the point "ptb" onto the line determined by
	 * "pt1" and "u2". This will be "ptp"
	 */
	um_nptln( ptb, pt1, u2, ptp ); 

	/* if projection is not very near original point, 
	 * answer is no
	 */
/*dgh
	if (!um_cceqcc(ptp, ptb))
		{
#if 	PTBTTRC
		sprintf(UM_sbuf, "um_ptbtwn: point not equal projection");
		um_pscroll(UM_sbuf);
#endif
		between = UU_FALSE;
		goto Done;
		}
*/


	/* ptb == ptp == pt1 : good enough	*/
	if (um_cceqcc(ptp, pt1))
		{

#if 	PTBTTRC
		sprintf(UM_sbuf, "um_ptbtwn: transitive satisfaction.");
		um_pscroll(UM_sbuf);
#endif

		between = UU_TRUE;
		goto Done;
		}

	/* above test means vp not too short */
	um_vcmnvc( ptp, pt1, vp );
	mp = um_mag(vp);

	if (um_dot(u2, vp) > 0 && mp <= m2 + UM_FUZZ)
		{

#if 	PTBTTRC
		sprintf(UM_sbuf, "um_ptbtwn: final test positive");
		um_pscroll(UM_sbuf);
#endif

		between = UU_TRUE;
		goto Done;
		}
	else
		{

#if 	PTBTTRC
		sprintf(UM_sbuf, "um_ptbtwn: final test negative");
		um_pscroll(UM_sbuf);
#endif

		between = UU_FALSE;
		goto Done;
		}

Done:
#if 	PTBTTRC
		sprintf(UM_sbuf, "v2 <%f,%f,%f>;\tu2 <%f,%f,%f>;\tm2 %f",
		v2[0], v2[1], v2[2], u2[0], u2[1], u2[2], m2);
		um_pscroll(UM_sbuf);

		sprintf(UM_sbuf, "ptp <%f, %f,%f>;\tvp <%f,%f,%f>;\tmp %f",
		ptp[0], ptp[1], ptp[2], vp[0], vp[1], vp[2], mp);
		um_pscroll(UM_sbuf);
#endif
	uu_dexit;
	return (between);
	}


/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_ptinseg( pt1, ptb, pt2 )
**			Return UU_TRUE if point ptb is on line segment between point
**			pt1 and point pt2. 
**    PARAMETERS   
**       INPUT  : 
**				pt1  the first  endpoint
**				ptb  the point to test to be on the segment between pt1, pt2
**				pt2  the second endpoint 
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_TRUE  if ptb is between pt1 and pt2;  UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_ptinseg( pt1, ptb, pt2 )

	UM_coord		pt1;
	UM_coord		ptb;
	UM_coord		pt2;

	{
	UM_vector	v2;			/* endpoint vector */
	UM_vector	u2;			/* unit vector of v2 */
	UM_length	m2;			/* magnitude of endpoint vector */
	UM_coord		ptp;			/* the projection of the between point on
										the line */
	UM_vector	vp;			/* projection point vector */
	UM_length	mp;			/* magnitude of projection point vector */
	UU_LOGICAL between;		/* TRUE if "ptb" is between "pt1" and "pt2" */
	char		s1[120];

/*--------------------------------------------------------------------
**	Start of Executable Code
**--------------------------------------------------------------------
**
**	Test if point ptb is between point pt1 and pt2
*/
	uu_denter(UU_MTRC,(us,"um_ptinseg(?,?,?)"));

#if 	PTBTTRC
		sprintf(UM_sbuf,
		"um_ptinseg: pt1 <%f,%f,%f>, ptinseg <%f,%f,%f>, pt2 <%f,%f,%f>.",
		pt1[0],pt1[1],pt1[2], ptb[0],ptb[1],ptb[2], pt2[0],pt2[1],pt2[2]);
		um_pscroll(UM_sbuf);
#endif

	between = UU_FALSE;

	if (um_cceqcc(pt1, pt2)) /* start and end points are the same */
		{
		if (um_cceqcc(pt1, ptb)) /* we say "ptb" is between "pt1" and "pt2" */
			between = UU_TRUE;
		else 
			between = UU_FALSE;

#if 	PTBTTRC
		sprintf(UM_sbuf, "um_ptinseg: short segment, answer %d.", between);
		um_pscroll(UM_sbuf);
#endif

		goto Done;
		}

	/* line not too short	*/
	um_vcmnvc( pt2, pt1, v2 );
	um_unitvc( v2, u2 );
	m2 = um_mag(v2);

	/* get the projection of the point "ptb" onto the line determined by
	 * "pt1" and "u2". This will be "ptp"
	 */
	um_nptln( ptb, pt1, u2, ptp ); 

	/* if projection is not very near original point, 
	 * answer is no
	 */
	if (!um_cceqcc(ptp, ptb))
		{
#if 	PTBTTRC
		sprintf(UM_sbuf, "um_ptinseg: point not equal projection");
		um_pscroll(UM_sbuf);
#endif
		between = UU_FALSE;
		goto Done;
		}


	/* ptb == ptp == pt1 : good enough	*/
	if (um_cceqcc(ptp, pt1))
		{

#if 	PTBTTRC
		sprintf(UM_sbuf, "um_ptinseg: transitive satisfaction.");
		um_pscroll(UM_sbuf);
#endif

		between = UU_TRUE;
		goto Done;
		}

	/* above test means vp not too short */
	um_vcmnvc( ptp, pt1, vp );
	mp = um_mag(vp);

	if (um_dot(u2, vp) > 0 && mp <= m2 + UM_FUZZ)
		{

#if 	PTBTTRC
		sprintf(UM_sbuf, "um_ptinseg: final test positive");
		um_pscroll(UM_sbuf);
#endif

		between = UU_TRUE;
		goto Done;
		}
	else
		{

#if 	PTBTTRC
		sprintf(UM_sbuf, "um_ptinseg: final test negative");
		um_pscroll(UM_sbuf);
#endif

		between = UU_FALSE;
		goto Done;
		}

Done:
#if 	PTBTTRC
		sprintf(UM_sbuf, "v2 <%f,%f,%f>;\tu2 <%f,%f,%f>;\tm2 %f",
		v2[0], v2[1], v2[2], u2[0], u2[1], u2[2], m2);
		um_pscroll(UM_sbuf);

		sprintf(UM_sbuf, "ptp <%f, %f,%f>;\tvp <%f,%f,%f>;\tmp %f",
		ptp[0], ptp[1], ptp[2], vp[0], vp[1], vp[2], mp);
		um_pscroll(UM_sbuf);
#endif
	uu_dexit;
	return (between);
	}


