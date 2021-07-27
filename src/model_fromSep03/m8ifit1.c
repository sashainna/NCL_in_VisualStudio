/*********************************************************************
**    NAME:  m8ifit.c
**       CONTAINS:
**    		int umi_mkXsectionPlane(pt,delta,color,plnNormal,planeptr,
**    		int umi_createXsection(bodyptr, planeptr, color, xSectListptr)
**    		int umi_mkCompCrvProfile(keys, nbrEdges, color, 
**    		int umi_getOfstNormal(offsetptr,offsetvec,ptOn1stSubcrv,
**    		int umi_get_initial_offsetvec(eptr, pick, u, 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m8ifit1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:10
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdebug.h"	/* for UM_sbuf */
#include "mdclass.h" 
#include "mdrel.h"
#include "modef.h"	/* for UM_FUZZ */
#include "mcrv.h"
#include "msol.h"		/* for UM_MAXEDGE and UM_MAX_NBR_HITS */
#include "mattrddl.h"/* for struct UM_attrdata_rec */
#include "mdattr.h"	/* for modelling colors */
#include "mdcpln.h" 
#include "mromcom.h"
#include "misect.h"	/* for UM_isect data type */
#include "mdeval.h"	/* for UM_FRSTDERIV */
#include "mdcoord.h"	/* for UM_DEFAULT_TF */
#include "ulist.h"	/* for list processing data type, macros */

#if UU_SUNTYPE==UU_SUN_SUN2
	/* ain't nothing here; solids won't work on SUN2 */
#else

#define CURRENT_BODY 0
#define NEWEST_BODY 1
#define _(funct) { if (funct != UU_SUCCESS) goto failed; }
#define UM_FILLIN_CRV_FIELDS(crvptr) (crvptr)->key= -1; strcpy((crvptr)->label,"")

#define TRACE UU_TRUE /* for debugging only */

/*********************************************************************
**    I_FUNCTION: int umi_mkXsectionPlane(pt,delta,color,plnNormal,planeptr,
**										planeMadeptr)
**			This function creates the cross section plane, given the origin point
**			of the plane. It is created in both Romulus and Unibase.
**			Also, the plane is displayed on the screen.
**    PARAMETERS   
**       INPUT: 
**				pt			Point that corresponds to the origin of the cross section
**							plane.
**				color		Color to make the plane.
**       OUTPUT:  
**				plnNormal		Normal to the plane created.
**				planeptr			Pointer to cross section body.
**				planeMadeptr	UU_TRUE iff the cross section plane was created.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umi_mkXsectionPlane(pt,delta,color,plnNormal,planeptr,planeMadeptr)
	UM_coord pt;
	UM_vector delta;				/* length and width of cutting plane */
	int color;
	UM_vector plnNormal;
	struct UM_body_rec *planeptr;		/* cross section plane */
	UU_LOGICAL *planeMadeptr;
{
	UM_vector xaxis;
	UM_vector temp;
	UM_vector tmpDelta;
	UM_coord newPt;
	char cmd[200];					/* ROMULUS command */
	int tmpcolor, i, numint, status = UU_SUCCESS;
	uu_denter(UU_MTRC,(us,
	"umi_mkXsectionPlane(pt:%g,%g,%g,delta[0,1]:%g,%g,colr,plnNrml,plnptr,?)",
			pt[0],pt[1],pt[2],delta[0],delta[1]));

	um_cctoco(pt, UM_CARTESIAN, newPt);
	*planeMadeptr = UU_FALSE; /* no plane made yet */
	/* create the cutting plane as a sheet */
	um_cross(UM_cpln.yaxis,UM_cpln.zaxis,xaxis);
	um_vctmsc(xaxis,(delta[0]/2.0),temp);
	um_vcplvc(pt,temp,newPt);
	um_vctmsc(UM_cpln.yaxis,(delta[1]/2.0),temp);
	um_vcplvc(newPt,temp,newPt);
	for (i=0; i<2; i++) tmpDelta[i] = fabs(delta[i]);
	um_init_rombuf();
	sprintf(cmd,"CREATE BLOCK L %f W %f H 0.0 @",tmpDelta[1],tmpDelta[0]);
	um_add_rombuf(cmd);
	sprintf(cmd,"FROM %f,%f,%f @",newPt[0],newPt[1],newPt[2]);
	um_add_rombuf(cmd);
	sprintf(cmd,"DIRECTION %f,%f,%f @*", UM_cpln.zaxis[0], UM_cpln.zaxis[1],
					UM_cpln.zaxis[2]);
	um_add_rombuf(cmd);

	/* call Romulus sectioning plane */
	um_callromulus();

	_(ur_setup_data(UM_BODY_REL, planeptr, sizeof(struct UM_body_rec)));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (planeptr->label, "");
	planeptr->subscr = 0;

	um_get_by_name(CURRENT_BODY, &(planeptr->id), planeptr->name);
	um_get_by_edge(UM_MAXEDGE, &(planeptr->id), &(planeptr->no_edge), 
			planeptr->edge);

	tmpcolor = ur_get_attrmdl_color();/*save away current color for restoration*/
	/**** change color for plane ****/
	ur_put_attrmdl_color(color);

	_(um_create_geom(planeptr, UM_DEFAULT_TF, UM_CURRENT_ATTR));
	_(uc_display(planeptr));
	ur_put_attrmdl_color(tmpcolor);	/* reset color */
	_(ur_update_editability(planeptr->key, UU_FALSE)); /* can't edit */

	um_vctovc(UM_cpln.zaxis, plnNormal);

	*planeMadeptr = UU_TRUE;

	goto done;
failed: status = UU_FAILURE;
done: uu_dexitstatus("umi_mkXsectionPlane", status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION: umi_createXsection(bodyptr, planeptr, color, xSectListptr)
**			Create a cross section bodies from the body pointed to by "bodyptr";
**			note, the sectioning plane is pointed to by "planeptr".
**    PARAMETERS   
**       INPUT: 
**				bodyptr			Pointer to the body to be cross sectioned.
**				planeptr			Pointer to the plane with which to do the cross 
**									sectioning.
**				color				Color to display cross section.
**       OUTPUT:
**				xSectListptr	Pointer to the list of the newly created cross 
**									section. 
**									bodies as a Unibase entity.
**				xSectMadeptr	Pointer to UU_TRUE iff cross section made.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umi_createXsection(bodyptr, planeptr, color, xSectListptr, xSectMadeptr)
	struct UM_body_rec *bodyptr;	/* body to be cross sectioned */
	struct UM_body_rec *planeptr;	/* plane to cross section with */
	int color;
	UU_LIST *xSectListptr;	/* body to be created */
	UU_LOGICAL *xSectMadeptr;
{
	char cmd[80];			/* ROMULUS command buffer to send XSECTION command */
	UU_KEY_ID *keyptr;
	struct UM_body_rec xSectBody;
	int tmpcolor, i, status = UU_SUCCESS;
	uu_denter(UU_MTRC,(us,
		"umi_createXsection(body>key:%d,plane>key:%d,color:%d,?)",
						bodyptr->key,planeptr->key,color));

	*xSectMadeptr = UU_FALSE; /* not made yet */

	tmpcolor = ur_get_attrmdl_color();/*save away current color for restoration*/
	/**** change color for composite ****/
	ur_put_attrmdl_color(color);

	/* cross section the specified body by the cutting plane to
	 *	create a new sheet containing the cross section */
	um_init_rombuf();
	sprintf(cmd,"XSECTION %s %s @*",bodyptr->name,planeptr->name);
	um_add_rombuf(cmd);
	um_callromulus();

	um_cre_body_list(CURRENT_BODY, color, xSectListptr); 
	keyptr = (UU_KEY_ID *) UU_LIST_ARRAY(xSectListptr);
	for (i=0; i<UU_LIST_LENGTH(xSectListptr); i++)
	{
		xSectBody.key = *keyptr;
		keyptr++;
		_(uc_retrieve_data(&xSectBody, sizeof(struct UM_body_rec)));
		_(uc_display(&xSectBody));
		_(ur_update_editability(xSectBody.key, UU_FALSE)); /* make non-editable */
	}
	*xSectMadeptr = UU_TRUE;

	goto done;
failed: status = UU_FAILURE;
done: 
	ur_put_attrmdl_color(tmpcolor);
	uu_dexitstatus("umi_createXsection", status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION: int umi_mkCompCrvProfile(xSectBody, pickedEdgeNbr, color,
**									compCrvOkptr, compcrvptr)
**			This function creates a composite that is the profile of the
**			edges of the cross section body to do fit with.  Note, the 
**			composite's subcurves are NOT Romulus edges, but rather new
**			Unibase geometry that looks like the edges.
**    PARAMETERS   
**       INPUT: 
**				xSectBodyptr	Pointer to the cross section body to fit.
**				pickedEdgeNbr	Index of the picked edge in body list.
**				color				Color to put in the attribute bundle of the 
**									composite curve to be made.
**       OUTPUT :  
**				keys				Array of the keys of the subcurves of the composite.
**				compCrvOkptr 	Pointer to UU_TRUE iff a CLOSED composite was made.	
**				compcrvptr		Pointer to the composite curve made. 
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS: A Unibase copy of all edges is made.
**    WARNINGS     : none
*********************************************************************/
int umi_mkCompCrvProfile(xSectBodyptr, pickedEdgeNbr, color, compCrvOkptr, 
								compcrvptr)
	struct UM_body_rec *xSectBodyptr;
	int pickedEdgeNbr;
	int color;
	UU_LOGICAL *compCrvOkptr;
	struct UM_compcrv_rec *compcrvptr;
{
	struct UM_crvdatabag crvDataBag;
	UM_transf tfmat;
	struct UM_attrdata_rec attr;
	UU_KEY_ID keys[UM_MAXPICK], orderedKeys[UM_MAXPICK];
	UU_LOGICAL subcrvsMade = UU_FALSE;
	int nbrOrderedKeys, i, status = UU_SUCCESS;
	uu_denter(UU_MTRC,(us,
		"umi_mkCompCrvProfile(xSectBodyptr>key:%d,pikdEdgeNbr:%d,color:%d?,?)",
							xSectBodyptr->key,pickedEdgeNbr,color));

	/**** put the edges in Unibase as Unicad geometry ****/
#if (TRACE)
	um_pscroll("making composite from cross section body");
#endif
	*compCrvOkptr = UU_FALSE;

	/*** create copies of the edges in Unibase ***/
	if (xSectBodyptr->no_edge >= UM_MAXPICK)
	{
		/* Error: Too many edges in body. */
		uu_uerror0(UM_MODEL,311);
		goto failed;
	}

	_(umi_getSubcrvChain(xSectBodyptr,pickedEdgeNbr,orderedKeys,
									&nbrOrderedKeys));
	subcrvsMade = UU_TRUE;

	/**** change color for composite ****/
	ur_put_attrmdl_color(color);
	
	_(um_c5_mergecrv(nbrOrderedKeys, orderedKeys, compcrvptr));

	if (compcrvptr->open) /* compcrv is not in Unibase yet */
	{
		uu_uerror0(UM_MODEL,298);
		/* Error: The curves picked must constitute a simple closed 
		 * curve. */
		for (i=0; i<nbrOrderedKeys; i++)
			_(uc_delete(orderedKeys[i]));
		subcrvsMade = UU_FALSE;
	}
	else /* now put compcrv in Unibase */
	{
		*compCrvOkptr = UU_TRUE;
		_(uc_create_data(compcrvptr,UM_DEFAULT_TF,UM_CURRENT_ATTR));
		_(ur_update_editability(compcrvptr->key, UU_FALSE));/* can't edit it */

#if (TRACE)
		uu_dprint(UU_MTRC,(us,"compcrv made"));
		_(uc_print(compcrvptr));
#endif 
	}
	goto done;
failed: status = UU_FAILURE;
	if (subcrvsMade)
		for (i=0; i<nbrOrderedKeys; i++)
			if (uc_delete(orderedKeys[i]) != UU_SUCCESS) 
				uu_dprint(UU_MTRC,(us,"Can't do delete of entity with key:%d",
				orderedKeys[i]));

done: 
	uu_dexitstatus("umi_mkCompCrvProfile",status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int umi_getSubcrvChain(xSectBodyptr, pickedEdgeNbr, 
**														orderedKeys, nbrOrderedKeysptr)
**			This function takes the array of keys, "keys", (whose entries
**			ought to correspond to subcurves of a composite) and attempts
**			to re-arrange the keys in a manner such that their corresponding
**			subcurves are linked head to tail.
**    PARAMETERS   
**       INPUT: 
**				xSectBodyptr		Pointer to the cross section body.
**				pickedEdgeNbr		Index of edge picked.
**       OUTPUT:  
**				orderedKeys			Re-arranged keys.
**				nbrOrderedKeysptr	Pointer to the number of valid entries in 
**										"orderedKeys".
**    RETURNS: UU_SUCCESS iff no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umi_getSubcrvChain(xSectBodyptr, pickedEdgeNbr, orderedKeys, 
							nbrOrderedKeysptr)
	struct UM_body_rec *xSectBodyptr;
	int pickedEdgeNbr;
	UU_KEY_ID orderedKeys[];
	int *nbrOrderedKeysptr;
{
	UU_REAL um_dcccc();
	UU_LOGICAL um_cceqcc();

	typedef struct {
		UM_coord pt;
		union {	/* forcing alignment for an array of these suckers */
			UU_LOGICAL paired;
			UU_REAL dummy;
		} aligner;
	} pts;

	struct UM_crvdatabag crvDataBag;
	pts* start;
	pts* end;
	pts *nextPtptr, *newNextPtptr;
	UU_LOGICAL stillPairing;
	UM_transf tfmat;
	int indx, i, j, status = UU_SUCCESS;
	uu_denter(UU_MTRC, (us, 
		"umi_getSubcrvChain(xSectBodyptr>key:%d,pickedEdgeNbr:%d,?,?)", 
		xSectBodyptr->key,pickedEdgeNbr));

	start = (pts*) uu_malloc(sizeof(pts) * UM_MAXPICK);
	end = (pts*) uu_malloc(sizeof(pts) * UM_MAXPICK);
	for (i=0; i<xSectBodyptr->no_edge; i++)
	{
		_(um_ret_romgeom(xSectBodyptr->edge[i],&(crvDataBag.rel_num),
					&crvDataBag));
		_(um_get_endpts(&crvDataBag, UM_idmat, start[i].pt, end[i].pt));
		start[i].aligner.paired = end[i].aligner.paired = UU_FALSE;
	}
#if (TRACE)
	for (i=0; i<xSectBodyptr->no_edge; i++)
		uu_dprint(UU_MTRC,(us,"startpt:%g,%g,%g; endpt:%g,%g,%g",
			start[i].pt[0],start[i].pt[1],start[i].pt[2],
			end[i].pt[0],end[i].pt[1],end[i].pt[2]));
#endif
	i = pickedEdgeNbr;	/* index of next edge copy to pair up */
	indx = 0; /* next entry in "orderedKeys" to fill-in */
	if (um_cceqcc(start[i].pt, end[i].pt)) /* there is only one edge to use */
	{
		_(um_ret_romgeom(xSectBodyptr->edge[i],&(crvDataBag.rel_num),
						&crvDataBag));
		_(uc_create_data(&crvDataBag, UM_DEFAULT_TF, UM_CURRENT_ATTR));
#if (TRACE)
		um_pscroll("copy of edge:");
		_(uc_print(&crvDataBag));
#endif
		orderedKeys[indx] = crvDataBag.key;
		indx++;
	}
	else /*** else chain together the subcurves of the boundary that are connected ***/
	{
		nextPtptr = &(end[i]);
		stillPairing = UU_TRUE;
		while (stillPairing) /* while not paired up */
		{
			/* note, don't know whether to pair this pt with a start or end pt */
			for (j=0; j<xSectBodyptr->no_edge; j++) /* get a start point, if possible */
				if ( (i != j) && (!start[j].aligner.paired) 
						&& (um_dcccc(nextPtptr->pt,start[j].pt) < UM_FUZZ) )
				{
					_(um_ret_romgeom(xSectBodyptr->edge[j],&(crvDataBag.rel_num),
								&crvDataBag));
					_(uc_create_data(&crvDataBag, UM_DEFAULT_TF, UM_CURRENT_ATTR));
#if (TRACE)
					um_pscroll("copy of edge:");
					_(uc_print(&crvDataBag));
#endif
					orderedKeys[indx] = crvDataBag.key;/*this is the next subcrv in the chain*/
					indx++;
					start[j].aligner.paired = UU_TRUE;
					nextPtptr->aligner.paired = UU_TRUE;
					/* is other end of NEW subcrv paired? */
					if (end[j].aligner.paired)
						stillPairing = UU_FALSE;
					else /* "end" is not paired */
					{
						newNextPtptr = &(end[j]);
						i = j; 
					}
					break; /* the for loop */
				}
			if (!nextPtptr->aligner.paired) /* couldn't pair up, try to pair with an endpt */
				for (j=0; j<xSectBodyptr->no_edge; j++)
					if ( (i != j) && (!end[j].aligner.paired)
							&& (um_dcccc(nextPtptr->pt,end[j].pt) < UM_FUZZ) ) 
					{
						_(um_ret_romgeom(xSectBodyptr->edge[j],&(crvDataBag.rel_num),
									&crvDataBag));
						_(uc_create_data(&crvDataBag, UM_DEFAULT_TF, UM_CURRENT_ATTR));
#if (TRACE)
						um_pscroll("copy of edge:");
						_(uc_print(&crvDataBag));
#endif
						orderedKeys[indx] = crvDataBag.key;/*this is the next subcrv in the chain*/
						indx++;
						end[j].aligner.paired = UU_TRUE;
						nextPtptr->aligner.paired = UU_TRUE;
						/* is other end of NEW subcrv paired? */
						if (start[j].aligner.paired)
							stillPairing = UU_FALSE;
						else 
						{
							newNextPtptr = &(start[j]);
							i = j;
						}
						break; /* the for loop */
					}
			if (j == xSectBodyptr->no_edge) /* couldn't pair up, we're in trouble now! */
			{			
				uu_uerror0(UM_MODEL,310);
				/* Error: can't make closed curve out of picked cross section */
				goto failed;
			}
			nextPtptr = newNextPtptr;
		} /* end while */
	} /* end else */
	*nbrOrderedKeysptr = indx;

	goto done;
failed: status = UU_FAILURE;
done: 
#if (TRACE)
	uu_dprint(UU_MTRC,(us,"ordered keys"));
	for (i=0; i<indx; i++)
		uu_dprint(UU_MTRC,(us,"key:%d", orderedKeys[i]));
#endif
	uu_dexitstatus("umi_getSubcrvChain",status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION: int umi_getOfstNormal(offsetptr,offsetvec,unusualCompcrvPt,
**										compNormal,unusualOfstPt,offsetNormal)
**			This function calculates and returns an unusual point on the offset
**			the normal to the offset tolerance profile.
**    PARAMETERS   
**       INPUT: 
**				offsetptr		Pointer to the offset curve.
**				offsetvec		Offset vector.
**				unusualCompcrvPt	Coordinates of an "unusual" point on the picked 
**									subcurve of the composite associated with 
**									"offsetptr".
**				compNormal		Normal to the composite.
**       OUTPUT:  
**				unusualOfstPt	Point on offset not at a joint.
**				offsetNormal	Normal to the offset curve where this is the 
**									tangent to the offset curve cross the offset vector.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umi_getOfstNormal(offsetptr,offsetvec,unusualCompcrvPt,compNormal,
				unusualOfstPt,offsetNormal)
	struct UC_entitydatabag *offsetptr;
	UM_vector offsetvec;
	UM_coord unusualCompcrvPt;
	UM_coord compNormal;
	UM_coord unusualOfstPt;
	UM_vector offsetNormal;
{
	struct UM_line_rec ray;
	UM_vector unitOffsetvec;
	UU_REAL commonPlane[2][3];
	struct UM_evcrvout crvout;
	UU_LOGICAL minDefined;
	UM_isect ibuf[UM_MAX_NBR_HITS];
	int nbrHits, indxOfMin, i, status = UU_SUCCESS;
	uu_denter(UU_MTRC, (us, 
		"umi_getOfstNormal(ofstptr,ofstvec,unusualCompcrvPt,cmpNrml,?,?)"));
	uu_dprint(UU_MTRC,(us,"offsetvec:%g,%g,%g",
					offsetvec[0],offsetvec[1],offsetvec[2]));
	uu_dprint(UU_MTRC,(us,"unusualCompcrvPt:%g,%g,%g",
					unusualCompcrvPt[0],unusualCompcrvPt[1],unusualCompcrvPt[2]));
	uu_dprint(UU_MTRC,(us,"compNormal:%g,%g,%g",
					compNormal[0],compNormal[1],compNormal[2]));

	/********************************************************************/
	/**** now find parameter value for an unusual point on the curve ****/
	/********************************************************************/
	um_unitvc(offsetvec, unitOffsetvec); /* "offsevec" can be too small to use */
	_(uc_setup_data(UM_LINE_REL, &ray, sizeof(ray)));
	um_cctoco(unusualCompcrvPt, UM_CARTESIAN, ray.spt);
	um_vcplvc(unusualCompcrvPt, unitOffsetvec, ray.ept);

	/**** get the point where the ray hits the offset ****/
	um_cctoco(ray.spt, UM_CARTESIAN, commonPlane[0]);
	um_cctoco(compNormal, UM_CARTESIAN, commonPlane[1]);
#if (TRACE)
	_(uc_create_data(&ray,UM_DEFAULT_TF,UM_CURRENT_ATTR));
	_(uc_print(&ray));
	_(uc_delete(ray.key));
	sprintf(UM_sbuf,"commonPlane[0](ptOnPlane):%g,%g,%g",
			commonPlane[0][0],commonPlane[0][1],commonPlane[0][2]);
	um_pscroll(UM_sbuf);
	uu_dprint(UU_MTRC,(us,"%s",UM_sbuf));
	sprintf(UM_sbuf,"commonPlane[1](planeNormal):%g,%g,%g",
			commonPlane[1][0],commonPlane[1][1],commonPlane[1][2]);
	um_pscroll(UM_sbuf);
	uu_dprint(UU_MTRC,(us,"%s",UM_sbuf));
#endif
	_(uc_crv_intersect_sp
		(&ray,UM_idmat,offsetptr,UM_idmat,commonPlane,&nbrHits,
		UM_MAX_NBR_HITS,ibuf));
	if (nbrHits == 0) 
	{
		uu_dprint(UU_MTRC,(us,"no intersections"));
		um_pscroll("no intersections");
		goto failed;
	}
	else uu_dprint(UU_MTRC,(us,"intersections found:%d",nbrHits));

	minDefined = UU_FALSE;
	for (i=0; i<nbrHits; i++) /* get smallest positive hit on line */
	{
#if (TRACE)
		uu_dprint(UU_MTRC,(us,"intersection parameter for ray is:%g",ibuf[i].u0));
#endif
		if (0.0 <= ibuf[i].u0) 
			if (minDefined)
			{
				if (ibuf[i].u0 < ibuf[indxOfMin].u0)
					indxOfMin = i;
			}
			else 
			{
				indxOfMin = i;
				minDefined = UU_TRUE;
			}
	}
	uc_init_evcrvout(offsetptr, &crvout);
	if (uc_evcrv(UM_FRSTDERIV,ibuf[indxOfMin].u1,offsetptr,UM_DEFAULT_TF,&crvout)
		!= UM_VALID) goto failed;
	um_cctoco(crvout.cp, UM_CARTESIAN, unusualOfstPt);
	um_cross(crvout.dcdu, unitOffsetvec, offsetNormal);	

	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dprint(UU_MTRC,(us,"compNormal:%g,%g,%g", 
					offsetNormal[0],offsetNormal[1],offsetNormal[2]));
	uu_dprint(UU_MTRC,(us,"unusual pt on offset:%g,%g,%g",
					unusualOfstPt[0],unusualOfstPt[1],unusualOfstPt[2]));
	uu_dexitstatus("umi_getOfstNormal",status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int umi_get_initial_offsetvec(eptr, pick, u, 
**											offsetvec, normal)
**		This function determines the (unit) direction vector of the
**		offset at the point whose parameter value is "u" on the (planar, 
**		noncomposite) curve, pointed to by "eptr".
**    PARAMETERS   
**       INPUT  : 
**				eptr			Pointer to planar curve entity; can't be a composite.
**				pick			Model coordinates of the picked point.
**				u				Parameter value at which to get the direction vector.
**       OUTPUT :  
**				offsetvec	Unit vector giving the direction of the offset.
**				normal		Normal unit vector to the plane containing the offset.
**								This is: (the tangent vector at the point corresponding 
**								to the parameter value, "u", of the curve) cross (the 
**								offset vector). 
**    RETURNS:	UU_SUCCESS if the offset vector is found, otherwise UU_FAILURE.
**    SIDE EFFECTS: none
**    WARNINGS: none.
*********************************************************************/
int umi_get_initial_offsetvec(eptr, pick, u, offsetvec, normal)
	struct UM_crvdatabag *eptr;
	UM_coord pick; /* model coordinates of the picked location */
	UU_REAL u;
	UM_vector offsetvec;
	UM_vector normal;
{
	UU_REAL um_mag();
	UM_isect ibuf[UM_MAX_NBR_HITS];
	UM_vector tempnormal;
	int nbrinter;
	int i, status = UU_SUCCESS;
	uu_denter(UU_MTRC,(us,
		"umi_get_initial_offsetvec(eptr>key:%d,pick:%g,%g,%g,u,?,?)",
			eptr->key, pick[0], pick[1], pick[2], u));

	switch (eptr->rel_num)
	{
		case UM_LINE_REL:
		{
			struct UM_line_rec *lnptr;
			UM_vector tempvec, unitVecAlongLn;
			UM_coord nearpt;

			lnptr = (struct UM_line_rec *)eptr;
			/**** get unit vec in direction of line ****/
			um_vcmnvc(lnptr->ept, lnptr->spt, tempvec);
			um_unitvc(tempvec, unitVecAlongLn);

			/**** get point on (infinite) line closest to pick ****/
			um_nptln(pick, lnptr->spt, unitVecAlongLn, nearpt);

			/**** get unit vector perpendicular to line ****/
			um_vcmnvc(pick, nearpt, tempvec);
			if (um_mag(tempvec) <= UM_FUZZ)
			{
				uu_uerror1(UM_MODEL, 242, "umi_get_initial_offsetvec");
				/* error is: Offset direction vector is too small, try again,(%s).*/
				goto failed;
			}      
			um_unitvc(tempvec, offsetvec);

			/**** get normal to plane containing crv to offset ****/
			um_cross(unitVecAlongLn, offsetvec, tempnormal);
			um_unitvc(tempnormal, normal);
			break;
		}
		case UM_CIRCLE_REL:
		{
			struct UM_line_rec ln;
			struct UM_circle_rec *circptr;
			UU_REAL savdang;
			struct UM_evcrvout evout;
			UU_LOGICAL shrink;
			UU_LOGICAL offsetvecfound = UU_FALSE;

			circptr = (struct UM_circle_rec *)eptr;
			/* make line between center of arc and pick */
			_(ur_setup_data(UM_LINE_REL, &ln, sizeof(ln))); 
			/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
			strcpy (ln.label, "");
			ln.subscr = 0;

			/* get point on arc at parameter, u */
			uc_init_evcrvout(circptr, &evout);
			uc_evcrv(UM_FRSTDERIV, u, circptr, UM_DEFAULT_TF, &evout);

			um_vctovc(circptr->center, ln.spt);	/* start line at circle center */
			um_vctovc(pick, ln.ept);	/* PICK IS THE END OF THE LINE */
			UM_FILLIN_CRV_FIELDS(&ln); /* for debugging only */

			/* intersect circle and line */
			_(uc_crv_intersect(circptr,UM_idmat,&ln,UM_idmat,&nbrinter,
					UM_MAX_NBR_HITS, ibuf));

			for (i=0; i<nbrinter; i++)
			{
				uu_dprint(UU_MTRC,(us,"intersection:circle:%g;line:%g",ibuf[i].u0,
							ibuf[i].u1));
				if ( (0.0 <= ibuf[i].u0) && (ibuf[i].u0 <= 1.0) &&
					  (0.0 <= ibuf[i].u1) && (ibuf[i].u1 <= 1.0)  )
				{	/* then we going to expand the arc */
					um_vcmnvc(evout.cp,circptr->center,offsetvec);
					offsetvecfound = UU_TRUE;
				}
			}
			if (!offsetvecfound)
			{
				/* line and arc didn't intersect in range [0,1]; see if line is 
				 * inside the CIRCLE defined by the arc. */
				uu_dprint(UU_MTRC,(us,"line and arc didn't hit in [0,1]"));
				savdang = circptr->dang;
				circptr->dang = UM_TWOPI;
				_(uc_crv_intersect(circptr,UM_idmat,&ln,UM_idmat,
						&nbrinter, UM_MAX_NBR_HITS, ibuf));

				shrink = UU_TRUE;
				for (i=0; i<nbrinter; i++)
				{
					uu_dprint(UU_MTRC,(us,"intersection:circle:%g;line:%g",ibuf[i].u0,
							ibuf[i].u1));
					if ( (0.0 <= ibuf[i].u1) && (ibuf[i].u1 <= 1.0) )
						/* then an intersection was found in [0,1] */
						shrink = UU_FALSE;
				}
				if (shrink)
					um_vcmnvc(circptr->center,evout.cp,offsetvec);
				else 
				{
					uu_uerror1(UM_MODEL, 243, "umi_get_initial_offsetvec");
					/* error is: Can't determine how to offset, try again  (%s). */
					goto failed;
				}
				circptr->dang = savdang; /* restore original arc angle */
			}
			/* get normal to plane containing crv to offset */
			um_cross(evout.dcdu, offsetvec, tempnormal);
			um_unitvc(tempnormal, normal);
			break;
		}
		default:
			uu_uerror2(UM_MODEL, 241, eptr->rel_num, "umi_get_initial_offsetvec");
			/* error is: Illegal (sub)curve to offset of relation type, %d  (%s).*/
			goto failed;
	} /* end switch */

	goto done;
failed: status = UU_FAILURE;
done:;
#if (TRACE)
	sprintf(UM_sbuf,"RETURNING offsetvec:%g,%g,%g (umi_get_initial_offsetvec)",
			offsetvec[0], offsetvec[1], offsetvec[2]);
	uu_dprint(UU_MTRC,(us,"%s", UM_sbuf));
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf,"			NORMAL TO PLANE:%g,%g,%g",
			normal[0], normal[1], normal[2]);
	uu_dprint(UU_MTRC,(us,"%s", UM_sbuf));
	um_pscroll(UM_sbuf);
#endif
	uu_dexitstatus("umi_get_initial_offsetvec",status);
	return(status);
}
#endif
