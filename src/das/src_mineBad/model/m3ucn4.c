
/*********************************************************************
**    NAME         :  m3ucn3.c
**       CONTAINS: Construction routines for conics using
**							algebraic techniques
**			umu_c4_iges()
**			umu_c4_5conds()
**			umu_c4_blend()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m3ucn4.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:58
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "modef.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mdcpln.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION :  umu_c4_iges()
**       Creates conic in construction plane from general second
**			order equation (Axx + Bxy + Cyy + Dx + Ey + F = 0).
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c4_iges()

	{
	struct UM_conic_rec e;
	UU_REAL	coeffs[6];
	int numint;
	int i, status;

	uu_denter(UU_MTRC,(us,"umu_c4_iges()"));
	ur_setup_data(UM_CONIC_REL, &e, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	/* load e.tfmat with construction plane
	 * pass it to um_normquad()
	 */
	um_cplntf(e.tfmat);

	for (i=0; i<6; i++) coeffs[i] = 0.0;
	status = umu_create_conic_form(coeffs);
	if (status==-1)
		return ;

	if (um_normquad(coeffs, &e, e.tfmat))
		{
		uu_uerror0(UM_MODEL/* Can't create conic from given constraint*/, 191);
		goto Done;
		}

	/* -set endpoints depending on type
	 * -fix branch of hyperbola
	 * -eventually, solve for endpoints/branch
	 *	WARNING: check for endpoints too close
	 */
	/* STUB */
	switch(e.type)
		{
	 case	UM_PARABOLA:
		e.t0 = 50;
		e.t1 = -50;
		break;
	 case	UM_HYPERBOLA:
		e.t0 = .9;
		e.t1 = -.9;
		break;
	 case	UM_ELLIPSE:
		e.t0 = -2;
		e.t1 =  2;
		break;
	default:
		goto Done;
		break;
		}

	um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
	um_p4_conic(&e);
	uc_display(&e);

Done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c4_5conds(ptsonly, curvetype)
**       Create conic segment from any five point/tangent constraints.
**    PARAMETERS   
**       INPUT  : 
**          ptsonly	-	if non-zero, only ask for points, no tgts.
**				curvetype	type of conic to create (UM_ELLIPSE, 
**								UM_HYPERBOLA, UM_PARABOLA, or UM_CN_UNKNOWN)
**								
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : creates conic entity, if successful
**    WARNINGS     : none
*********************************************************************/
umu_c4_5conds(ptsonly, curvetype)
	int	ptsonly;
	int	curvetype;

	{
	struct	UM_conic_rec	e;

	int		pindex = 0;
								/* index into list of point constraints	*/
	UU_REAL	pconst[5][3];
								/* each pconst[pindex][] is a point	*/
	int		tindex = 0;
								/* index into tangent constraints, tconst	*/
	UU_REAL	tconst[2][2][3];	
								/* an array of two tangent constraints,
								*	each consraint is a point tconst[tindex][0][]
								*	and a vector	tconst[tindex][1][]
								*/
/**	UU_REAL	pointbuf[3]; **/
    UD_NDCLOCREC pointbuf;

	int	numint;
	int	numtgint;
	int	i;

	int		dim;					/* used in coplanar test	*/
	UU_REAL	space[2][3];			/* used in coplanar test	*/
	UM_transf invtfmat;	/* used to map constraints to definition space */
	char name[12];

	/** STRATEGY	(Reference Faux and Pratt, pp 32ff.)
	 **	Five constraints must be gotten from the user.  They
	 **	are loaded into pconst (point constraints) and
	 **	tconst (tangent constraints).  The latter count for two.
	 **
	 **	Endpoints are picked by the user and established in the conic
	 **	entity record.
	 **/

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"umu_c4_5conds(ptsonly=%d, curvetype=%d)",
		ptsonly, curvetype));

	ur_setup_data(UM_CONIC_REL, &e, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	/** NOTE
	 **	Each tangent constraint counts as two degrees of freedom.
	 **	To specify a conic, there must be specified 5 degrees of
	 **	freedom.  That is, we are ready to calculate when
	 **	pindex + 2*tindex == 5.
	 **/
		
		while (pindex + 2*tindex < 5)
			{
			/* get point constraint	*/
			ud_ldas(UD_DASCART, /* Point to be on conic */ UM_MODEL, 215,
				&pointbuf, 1, &numint, UD_NODEFAULT);
			if (numint < 1)
				goto Done;

			/** point copied to proper constraint buffer below **/
			
			/* get tangent vector constraint	*/
			numtgint = 0;
			if (!ptsonly && (pindex + 2*tindex < 4) )	/* note 'ptsonly'	*/
				{
				/* just got another constraint above */
				ud_ldas(UD_DASVEC, /* OPTIONAL tangent vector */UM_MODEL, 216,
					tconst[tindex][1], 1, &numtgint, UD_NODEFAULT);
				}

			if (numtgint > 0)	/* user supplied tangent constraint	*/
				{
				/** copy point into vector constraint buffer	**/
				um_vctovc(&pointbuf, tconst[tindex][0]);

				/** FIX: check for zero vector? **/
				um_unitvc(tconst[tindex][1], tconst[tindex][1]);
				++tindex;
				}
			else
				{
				um_vctovc(&pointbuf, pconst[pindex]);
				++pindex;
				}
		}	/* now have five constraints: pindex + 2*tindex == 5	*/

	/** check that constraints lie in plane	**/
	dim = -1;
	for ( i = 0; i < pindex; ++i)
		um_netspan(dim, space, 0, pconst[i], &dim, space );
	for ( i = 0; i < tindex; ++i)
		um_netspan(dim, space, 1, tconst[i], &dim, space );

	if (dim != 2)
		{
		/** FIX: use error message here	**/
		um_pscroll("Conic constraints not coplanar");
		goto Done;
		}
	
	/** set up conic internal transformation, using
	 ** construction plane definition
	 **/
	um_vctovc(space[1], e.tfmat[2]);	/* normal	*/
	um_vctovc(space[0], e.tfmat[3]);	/* center	*/
	um_perpvc(space[1], e.tfmat[0]);	/* x-axis	*/
	um_unitvc(e.tfmat[0], e.tfmat[0]);
	um_cross(e.tfmat[2], e.tfmat[0], e.tfmat[1]);	/* y-axis	*/
	um_pscroll("umu_c4_5conds: transformation");
	um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, e.tfmat[0]);
	um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, e.tfmat[1]);
	um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, e.tfmat[2]);
	um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, e.tfmat[3]);

	/** map constraints from model space to definition space	**/
	um_inverttf(e.tfmat, invtfmat);
	for ( i = 0; i < tindex; ++i)
		{
		um_p_ary(UM_PFLOAT,"mcs point",3,tconst[i][0]);
		um_p_ary(UM_PFLOAT,"mcs tangent",3,tconst[i][1]);
		um_cctmtf(tconst[i][0], invtfmat, tconst[i][0]);
		um_vctmtf(tconst[i][1], invtfmat, tconst[i][1]);
		um_p_ary(UM_PFLOAT,"xy point",3,tconst[i][0]);
		um_p_ary(UM_PFLOAT,"xy tangent",3,tconst[i][1]);
		}
	for ( i = 0; i < pindex; ++i)
		{
		um_p_ary(UM_PFLOAT,"mcs point",3,pconst[i]);
		um_cctmtf(pconst[i], invtfmat, pconst[i]);
		um_p_ary(UM_PFLOAT,"xy point",3,pconst[i]);
		}

	/**	Build a conic from all of this	**/
	if (um_cnstrconic(&e, tindex, tconst, pindex, pconst))
		{
		/* Can't create conic from given constraint */
		uu_uerror0(UM_MODEL, 191);
		goto Done;
		}
	else if ((curvetype != UM_CN_UNKNOWN) && (e.type != curvetype))
		{
		/* Can't create conic from given constraint */
		switch (curvetype)
			{
			case UM_PARABOLA:
				strcpy(name, "PARABOLA");
				break;
			case UM_HYPERBOLA:
				strcpy(name, "HYPERBOLA");
				break;
			case UM_ELLIPSE:
				strcpy(name, "ELLIPSE");
				break;
			default:
				strcpy(name, "CONIC");
				break;
			}
		uu_uerror1(UM_MODEL, 279, name);
		goto Done;
		}
	else
		{	/** GET ENDPOINTS	**/
		UU_REAL	ptlist[5][3];	/*	list of endpoint candidates for um_nearto	*/
		int	index1;			/* index into ptlist for 1st endpoint	*/
		int	index2;			/* index into ptlist for 2nd endpoint	*/
		int	ptcount;
		int	i;
		int	exitflag = 0;

		/** copy supplied points into array for um_ccnearcc()	**/
		ptcount = 0;
		for (i = 0; i < tindex; i++)
			um_vctovc(tconst[i][0], ptlist[ptcount++]);
		for (i = 0; i < pindex; i++)
			um_vctovc(pconst[i], ptlist[ptcount++]);

		/** ask for and set endpoints	**/
		while(!exitflag)
			{
			int	promptnum;		/* prompt special for ellipse				*/
/***		UU_REAL	pickpt[3]; ***/
            UD_NDCLOCREC pickpt;


			/** Let user know if ellipse	(endpoints optional)	**/
			if (e.type == UM_ELLIPSE)
				promptnum = 218;	/* Pick first endpoint OPTIONAL	*/
			else
				promptnum = 217;	/* Pick first endpoint */

			ud_ldas(UD_DASCART,  UM_MODEL, promptnum,
				&pickpt, 1, &numint, UD_NODEFAULT);

			if (numint < 1)
				{
				switch (e.type)
					{
					case	UM_PARABOLA:
						e.t0 = 50;
						e.t1 = -50;
						goto Draw;
						break;
					case	UM_HYPERBOLA:
						e.t0 = .9;
						e.t1 = -.9;
						goto Draw;
						break;
					case	UM_ELLIPSE:
						e.t0 = -2;
						e.t1 =  2;
						goto Draw;
						break;
					default:
						goto Done;
						break;
					}
				}
			else /*  (numint == 1) */
				{
				um_p_ary(UM_PFLOAT,"conic pts",3*ptcount,ptlist);
				/* -- find vertex nearest to pick	-- */

				/** index into ptlist for 1st endpoint	**/
				um_cctmtf(&pickpt, invtfmat, &pickpt);
				um_p_ary(UM_PFLOAT,"first ept",3,&pickpt);
				index1 = um_ccnearcc(&pickpt, ptcount, ptlist);
				um_p_ary(UM_PINT,"index1",1,&index1);

				ud_ldas(UD_DASCART,  UM_MODEL, /* 2nd endpoint */219,
					&pickpt, 1, &numint, UD_NODEFAULT);

				if (numint < 1)
					goto Done;

				/* -- find vertex nearest to pick	-- */

				/** index into ptlist for 2nd endpoint	**/
				um_cctmtf(&pickpt, invtfmat, &pickpt);
				um_p_ary(UM_PFLOAT,"second ept",3,&pickpt);
				index2 = um_ccnearcc(&pickpt, ptcount, ptlist);
				um_p_ary(UM_PINT,"index2",1,&index2);
				}
				
			/* now have two picks	*/
			if (index1 != index2 )
				{
				um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
				switch( um_cn4_endpoints(&e, ptlist[index1], ptlist[index2],
					invtfmat))
					{
					/** FIX: put in uu_uerror0() here	**/

					case 3:			/* branch hyperbola	*/
						um_pscroll("Endpoints for hyperbola on different branches.");
						goto Done;
					case 2:
					case 1:
						um_pscroll("Endpoints are screwed up good.");
						goto Done;
					case 0:
						exitflag = UU_TRUE;
						goto Drawendpts;
					default:
						break;
					}
				}
			else 			/* picked same endpoint twice	*/
				uu_uerror0(UM_MODEL,	/* endpoints must be distinct	*/ 192);
			}	/* prompt for endpoints again	*/
		};

Drawendpts:
	um_update_geom(&e, UM_DEFAULT_TF);
	uc_display(&e);
	goto Done;
Draw:
	um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
	uc_display(&e);

Done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c4_blend()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

umu_c4_blend()
	{
	struct	UM_conic_rec	e;

	int		tindex = 0;
								/* index into tangent constraints, tconst	*/
	UU_REAL	tconst[2][2][3];	
								/* an array of two tangent constraints,
								*	each consraint is a point tconst[tindex][0][]
								*	and a vector	tconst[tindex][1][]
								*/
/**	UU_REAL	point[3];	 user selected point on curve	*/
    UD_NDCLOCREC point;


	int		prompt;		/* prompt selection number			*/
	UM_PLOCREC	pickloc;	/*	entity and location buffer		*/
	int		numint;
	int		status;
	int		dummy;
		
	int		spandim;		/* buffers to hold span of tgt. conditions	*/
	UU_REAL	span[2][3];

	UU_REAL	invtfmat[4][3];
	UU_REAL	endpoints[2][3];	/* save endpoints gotten from tgt conditions */


	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"umu_c4_blend()"));
	ur_setup_data(UM_CONIC_REL, &e, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	while (tindex < 2)
		{
		prompt = (tindex == 0)?	220: 221;
		/* message is: pick starting|terminating entity	*/
		um_dl_pldas(UD_DASPCKLOC, UM_MODEL, prompt, &pickloc, 1, &numint, 1);
		if (numint < 1)
			goto Done;

		if (
			um_m_nrendpt(1, &pickloc.pent, &pickloc.ploc, tconst[tindex][0])
			|| um_m_vec(1, &pickloc.pent, &pickloc.ploc, tconst[tindex][1])
			)
			{
			uu_uerror0(UM_MODEL, /* can't get endpoint conditions	*/ 193);
			}
		else
			{
			++tindex;
			}

		}

	/*	have two tangent constraints at this point	*/
	if (um_netspan(1, tconst[0], 1, tconst[1], &spandim, span ) != 2)
		{
		/* FIX: use uu_uerror here	*/
		um_p_ary(UM_PINT, "Tangent conditions are not in a plane.", 1, &spandim);
		goto Done;
		}

	/*	get third point	*/
	ud_ldas(UD_DASCART,  UM_MODEL, /* point on conic */ 215,
		&point, 1, &numint, UD_NODEFAULT);
	if (numint < 1)
		goto Done;

	/* project point to plane of endpoint conditions */
	um_ilnpln(&point, span[1], span[0], span[1], &dummy, &point);

	/* set up conic internal transformation, using
	 * plane spanning tangent conditions
	 */
	/* try to keep orientation close to what the user
	 * expects, i.e., similar to const. plane
	 */
	if (um_dot(span[1], UM_cpln.zaxis) < -UM_FUZZ)
		{
		um_vctmsc(span[1], (UU_REAL) -1.0, span[1]);
		}
	um_vctovc( span[1], e.tfmat[2]);
	um_vctovc( span[0], e.tfmat[3]);
	um_perpvc( span[1], e.tfmat[1]);
	um_unitvc( e.tfmat[1], e.tfmat[1]);
	um_cross( e.tfmat[1], e.tfmat[2], e.tfmat[0]);

	/** save endpoints for later	**/
	um_vctovc(tconst[0][0], endpoints[0]);
	um_vctovc(tconst[1][0], endpoints[1]);

	/** map constraints from model space to definition space	**/
	um_inverttf(e.tfmat, invtfmat);
	um_cctmtf(tconst[0][0], invtfmat, tconst[0][0]);
	um_vctmtf(tconst[0][1], invtfmat, tconst[0][1]);
	um_cctmtf(tconst[1][0], invtfmat, tconst[1][0]);
	um_vctmtf(tconst[1][1], invtfmat, tconst[1][1]);
	um_cctmtf(&point, invtfmat, &point);

	if (um_cnstrconic(&e, 2, tconst, 1, &point))
		{
		uu_uerror0(UM_MODEL/* Can't create conic from given constraint*/, 191);
		goto Done;
		}

	/**	--	set endpoints	--	**/
	if (status = um_cn4_endpoints(&e, endpoints[0], endpoints[1]))
		{
		/* FIX: use uu_uerror here	*/
		um_p_ary(UM_PINT, "Error with endpoints:", 1, &status);
		goto Done;
		}


	um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
	uc_display(&e);

Done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int umu_create_conic_form(coeff)
**       Put up a form for the user to define a generalized conic
**						Axx + Bxy + Cyy + Dx + Ey + F = 0
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          coeff					six coefficients defining conic
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_create_conic_form(coeff)
	UU_REAL  coeff[];

	{
	int i, status;
	int *ans[6];
	UU_REAL lcoeff[6];

	uu_denter(UU_MTRC,(us,"umu_create_conic_form()"));

	for (i=0; i<6; i++)
		{
		lcoeff[i] = coeff[i];
		ans[i] = (int *) &lcoeff[i];
		}

	status = ud_form("mconic.frm", ans, ans);
	if (status==-1)
		return -1;

	for (i=0; i<6; i++) coeff[i] = lcoeff[i];

	um_p_ary(UM_PFLOAT,"coeff",6,coeff);

	uu_dexit;
	return 0;
	}
