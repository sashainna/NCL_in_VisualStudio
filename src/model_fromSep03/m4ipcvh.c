/*********************************************************************
**    NAME         :  m4ipcvh.c
**       CONTAINS:
**			um_plncvh
**    	um_iplncvh
**    	um_seminfline	(obsolete)
**			um_ptincvh
**			um_ilnplncvh(pt1,delta1,n2,cvh2,pt2,rintersect,rts,rtl)
**			um_debug_cvh	(conditional on UM_CVHTRC)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4ipcvh.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:04
*********************************************************************/
#include	"usysdef.h"
#include "umath.h"
#include "usysdef.h"
#include "udebug.h"
#include "mdebug.h"
#include "mcvh.h"
#include "modef.h"
#include	"mderror.h"
#include	"mdrel.h"
#include	"mdcoord.h"
#include	"mcrv.h"
#include	"mdgenent.h"

#define	UM_CVHTRC	0
#define	UM_LOCALTRC	0


struct hull	{								/* data for convex hull calculation */
	UU_REAL phi;
	UU_REAL rad;
	UU_REAL x;
	UU_REAL y;
	int index;
	};
/*********************************************************************
**    E_FUNCTION     : um_plncvh(cvh, plane)
**       Compute the convex hull of any planar set of points.
**       Uses the Graham algorithm as discribed in the paper
**			by Allison et. al. (BIT 24 ).
**    PARAMETERS   
**       INPUT  : 
**				cvh		--	pointer to convex hull structure.
**							integer field no_pt and pointer field pt must be
**							set upon entry.  
**				plane		--	plane in which points reside (point, vector)
**       OUTPUT :  
**				*cvh		--	remaining fields will be filled out
**				See mcvh.h for this information and comments
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : If the convex hull of your set has more than 
**					UM_MAXPTSET	points, this function will overwrite memory.
*********************************************************************/
um_plncvh(cvh, plane)
	C2vh	*cvh;
	UU_REAL	plane[2][3];		/* could be of type coord-system	*/

	{
	/* make these two registers	*/
	UU_REAL (*pt)[3]; 				/* fast access to cvh->pt	*/
	int *indx;			   			/* fast array of indices */

	struct hull *pm;  		   	/* convex hull data */
	int i,j,count;
	int minpt;							/* index of point with minimum y */
	int um_phicmp();					/* sort function for qsort */
	UU_REAL um_dcccc();
	UU_REAL um_dot();
	UU_REAL um_mag();

	uu_denter(UU_MTRC,(us,"um_plncvh()"));

	/*
	 *	subsections below are for cases where there are: fewer than three points,
	 * exactly three points, and more than three points.
	 */

	/* set up fast access to arrays in cvh strucure	*/
	/* (also avoid editing this routine)	*/
	pt = cvh->pt;
	indx = cvh->cv;	
	cvh->shape = 0;
	/* debug assist */
	cvh->no_cv = -19;

   pm  = (struct hull *) uu_toolmalloc ((cvh->no_pt+1)*sizeof(struct hull));

#if	UM_CVHTRC	
	sprintf(UM_sbuf, "enter um_plncvh: no_pts = %d", cvh->no_pt);
	um_pscroll(UM_sbuf);
#endif
/*
.....Fixed 2 or fewer points case so correct memory
.....is accessed - Andrew 1/31/13
*/
	if(cvh->no_pt <= 2)
		{
		indx[0] = 0;
		if (cvh->no_pt > 1)
		{
			indx[1] = 1;
			indx[2] = 0;
			cvh->no_cv = 3;
		}
		else
			cvh->no_cv = 1;
		cvh->min = -1;
		if (cvh->no_pt > 1)
			cvh->radius = um_dcccc(pt[0],pt[1]);
		else
			cvh->radius = 0.0;
		}

	else if(cvh->no_pt == 3)
		{
		UU_REAL	edge1, edge2;
		UU_REAL	vec1[3], vec2[3];

		cvh->min = -1;
		um_vcmnvc(pt[1], pt[0], vec1);
		edge1 = um_mag(vec1);
		um_vcmnvc(pt[2], pt[0], vec2);
		edge2 = um_mag(vec2);

		/* annoying checks are necessary 
		 * to handle points close together
		 */
		if (edge1 < UM_FUZZ)			/* first two points close together	*/
			{
			if (edge2 < UM_FUZZ)		/* all three points close together	*/
				{
				indx[0] = 0;
				cvh->no_cv = 1;
				cvh->radius = 0.0;
				}
			else							/* omit second point	*/
				{
				indx[0] = 0;
				indx[1] = 2;
				cvh->no_cv = 2;
				cvh->radius = edge2;
				}
			}
		else if (edge2 < UM_FUZZ)	/* omit third point	*/
			{
				indx[0] = 0;
				indx[1] = 1;
				cvh->no_cv = 2;
				cvh->radius = edge1;
			}
		else								/* edge1, edge2 > UM_FUZZ		*/
			{
			UU_REAL	projection;
			
			um_unitvc(vec1,vec1);				/* both calls will be OK	*/
			um_unitvc(vec2,vec2);

			projection = um_dot(vec1,vec2);
			
			/** colinearity test is for angle between edges	**/
			if(fabs(fabs(projection) - 1.0) < UM_FUZZ)
				{						
				cvh->shape |=   UM_CVH_LINEAR;
				/* note that we no longer eliminate a point in this case	*/

#if	UM_CVHTRC	
				sprintf(UM_sbuf, "\thull is linear: no_pt = 3");
				um_pscroll(UM_sbuf);
#endif
				}	
			/* three points not colinear,
			 * all belong in convex hull
			 */
			indx[0] = 0;
			indx[1] = 1;
			indx[2] = 2;
			indx[3] = 0;
			cvh->no_cv = 4;
			cvh->min = 0;
			cvh->radius = um_dcccc(pt[0],pt[1]);
			edge1 = um_dcccc(pt[0],pt[2]);
			edge2 = um_dcccc(pt[1],pt[2]);
			if(cvh->radius < edge1) cvh->radius = edge1;
			if(cvh->radius < edge2)
				{
				cvh->radius = edge2;
				cvh->min = 1;
				}
			} /* end: first point far from other two */
		}	/* end: case of three points	*/

	/*** number of pts is greater than three	**/
	else
		{
		UU_REAL	vec1[3], vec2[3];	/* basis for plane of intersection	*/
		UU_REAL	dispvec[3];			/* vector between "origin" (pt[0]) and pt[i]*/
		UU_REAL	xcoord, ycoord;	/* values for x, y w.r.t vec1, vec2	*/
		UU_REAL	xmin, ymin;			/* minimum values of x,y w.r.t vec1, vec2	*/
		UU_REAL	rmax, qrad, UM_Pfuzz;

      UM_Pfuzz = .01 * UM_FUZZ;

		/** set up basis for plane of hull	**/
		um_perpvc(plane[1], vec1);
		um_unitvc(vec1, vec1);
		um_cross(plane[1], vec1, vec2);

		/* determine point with min x or y 
		 * in the local coor. system 
		 */
		ymin =  0.0;
		xmin = 0.0;
		minpt = 0;
      rmax  = 0;
		for(i=1; i<cvh->no_pt; ++i)
			{

			um_vcmnvc(pt[i], pt[0], dispvec);
			xcoord = um_dot(dispvec, vec1);
			ycoord = um_dot(dispvec, vec2);

			/* same y- coord	*/
			if(fabs(ymin - ycoord) < .01 * UM_FUZZ)
				{
				/* compare x-coord	*/
				if(xmin > xcoord)
					{
					minpt = i;
					xmin = xcoord;
					}
				}
			else if (ymin > ycoord )
				{
				ymin = ycoord;
				minpt = i;
				xmin = xcoord;
				}
		 	}
		cvh->min = minpt;
		/** point with min y, x found. **
		 **
		 * now compute (theta,r) for each
		 *	point in control polygon
		 */

		/* compute coor. of origin point */			
		/* redundant: ** OUT	***
		um_vcmnvc(pt[minpt],pt[0],dispvec);
		xmin = um_dot(dispvec,vec1);
		ymin = um_dot(dispvec,vec2);
		*** OUT **/

		cvh->radius = 0.0;
		count = 0;

		 for(i=0; i<cvh->no_pt; ++i)
		 	{
	 		if(i != minpt)
				{
				UU_REAL	delta_x, delta_y;		/* pt[i] relative to pt[minpt]	*/
				UU_REAL	radius;

				um_vcmnvc(pt[i], pt[0], dispvec);
				xcoord = um_dot(dispvec, vec1);
				ycoord = um_dot(dispvec, vec2);

				/* know these are both >= 0	*/
				delta_x = xcoord - xmin;
				delta_y = ycoord - ymin;

				/* keep square	of radius	*/
				radius = (delta_x * delta_x + delta_y * delta_y);

				if (radius > UM_DFUZZ)
					{
					qrad = sqrt(radius);
					if (qrad > cvh->radius) cvh->radius = qrad;
					pm[count].phi = -delta_x/qrad;
					pm[count].rad = qrad;
					pm[count].x = xcoord;
					pm[count].y = ycoord;
					pm[count].index = i;
					count++;
					}

					/** else, omit pt[i]	**/

#if	UM_CVHTRC	
				else
					{
					sprintf(UM_sbuf,
						"dropping point because of small radius2: %g, i = %d",
						radius, i);
					um_pscroll(UM_sbuf);
					}
#endif

				 }
			 else		/* element for pt[minpt]	*/
				{
				pm[count].phi = -2.0;
				pm[count].rad = 0.0;
				pm[count].x = xmin;
		 		pm[count].y = ymin;
				pm[count].index = i;		/* == minpt	*/
				count++;
			   }
			}

#if	UM_CVHTRC	
			sprintf(UM_sbuf, "after phi calc., before sort: count = %d", count);
			um_pscroll(UM_sbuf);
#endif

			/* sort points by slope */
			uu_qsort((UU_REAL*)pm,count,sizeof(pm[0]),um_phicmp);

			/*** eliminate points along identical rays **/
			i = 0;
			while (i < count-1)
				{
				if (fabs(pm[i].phi - pm[i+1].phi) < UM_DFUZZ)
					{
					if (pm[i].rad < pm[i+1].rad) 
						{
#if	UM_CVHTRC	
						sprintf(UM_sbuf,
							"replacing radial conicident: i = %d, new count %d",
							i, count - 1);
						um_pscroll(UM_sbuf);
#endif
						um_replac(&pm[i],&pm[i+1]);
						}
					else
						{
						;
#if	UM_CVHTRC	
					sprintf(UM_sbuf, "replacing radial conicident: i = %d", i + 1);
					um_pscroll(UM_sbuf);
#endif
						}
					for(j=i+1; j<count-1; j++)	/* omit, and pack list	*/
						{
						um_replac(&pm[j],&pm[j+1]);
						}
					count--;
			  	  	}
				 else 
				  	{
				 	i++;
				  	}
				}

			/*** now have star-shaped polygon; test vertices for concavity **/

#if	UM_CVHTRC	
			sprintf(UM_sbuf, "after identical ray check: count = %d", count);
			um_pscroll(UM_sbuf);

			if (cvh->no_pt <= 4)
				{
				int ix;
				sprintf(UM_sbuf, "\nstart shaped poly:");
				um_pscroll(UM_sbuf);
				for (ix = 0; ix < count; ix++)
					{
					sprintf(UM_sbuf, "\tphi: %g;\tpoint: <%g,%g,%g>",
						pm[ix].phi,
						pt[pm[ix].index][0],
						pt[pm[ix].index][1],
						pt[pm[ix].index][0]);
					um_pscroll(UM_sbuf);
					}
				}
#endif

			if (count > 2)
				{
				UU_REAL	product1, product2;

				 i = 0;
				 while(i<count-2)
			   	{
					/* concavity test	*/
					product1 = (pm[i+1].x - pm[i].x) * (pm[i+2].y - pm[i].y);
					product2 = (pm[i+1].y - pm[i].y) * (pm[i+2].x - pm[i].x);
/*
...vp 24-may-95 FIX to eliminate colinear points
					if(product1 < product2 )
*/
					if(product1 < product2 + UM_DFUZZ &&
                             pm[i+1].rad < cvh->radius)
						  {						/* remove mid-point pm[i+1] from hull */
#if	UM_CVHTRC	
						sprintf(UM_sbuf,
			"concave midpoint: point index = %d, new count %d, prod1 %g, prod2 %g",
						pm[i+1].index, count - 1, product1, product2);
						um_pscroll(UM_sbuf);
#endif
                     for(j=i+1;j<count-1;j++)
                       {
                        um_replac(&pm[j],&pm[j+1]);
                       }
						   count--;
						   if(i != 0) i--;				/* back up one point	*/
				   	  }
					 else
				   	{
						i++;
				   	}
		  		 	}
#if	UM_CVHTRC	
				sprintf(UM_sbuf, "after concavity check: count = %d", count);
				um_pscroll(UM_sbuf);
#endif
				}
															/* check for linearity  */	
				if(count == 3)
					{
					/** another linearity test	**/
/*
..... since pm[0] is the leftmost lowest point, the cosines pm[i].phi must
..... be the same if the points are on a line.
*/
					if (fabs(pm[1].phi-pm[2].phi) < UM_FUZZ)
						{
						/** this guy is linear	**/
						cvh->shape |=   UM_CVH_LINEAR;

#if	UM_CVHTRC	
						sprintf(UM_sbuf, "\tphi test: three points, hull is linear");
						um_pscroll(UM_sbuf);
#endif
						}
					}
														/* now load indx array */
		  		for(i=0;i<count;i++)
					{
					indx[i] = pm[i].index;
					}
				indx[count] = indx[0];
				cvh->no_cv = count + 1;
/*
.....If hull has only on point, mark it as small to prevent loop.
*/
				if (count < 2)
				{
					cvh->shape |= UM_CVH_SMALL;
					cvh->radius = 0;
				}

#if	UM_CVHTRC	
						sprintf(UM_sbuf, "\tno_cv = count + 1 = %d", cvh->no_cv);
						um_pscroll(UM_sbuf);
#endif

/*          cvh->radius = sqrt(cvh->radius);   */
			}	/* end: number of points > 3	*/

		if	(cvh->no_cv <= 3)
			cvh->shape |=   UM_CVH_LINEAR;

	if (cvh->min != -1 && cvh->radius < UM_FUZZ)
		{
#if UM_CVHTRC
		um_p_ary(UM_PFLOAT,"(2) radius of small cvh",1,&(cvh->radius));
#endif
		cvh->shape |= UM_CVH_SMALL;
		}

#if	UM_CVHTRC	
		sprintf(UM_sbuf,
			"\tum_plncvh: returning: no_cv = %d, min = %d, rad= %f, shape = 0x%x",
			cvh->no_cv, cvh->min, cvh->radius, cvh->shape);
		um_pscroll(UM_sbuf);
		if (cvh->no_pt == 4 && cvh->no_cv <= 4)
			{
			sprintf(UM_sbuf, "4-point poly has been reduced:");
			um_pscroll(UM_sbuf);
			sprintf(UM_sbuf, "pt[0]: <%g, %g, %g>", pt[0][0], pt[0][1], pt[0][2] );
			um_pscroll(UM_sbuf);
			sprintf(UM_sbuf, "pt[1]: <%g, %g, %g>", pt[1][0], pt[1][1], pt[1][2] );
			um_pscroll(UM_sbuf);
			sprintf(UM_sbuf, "pt[2]: <%g, %g, %g>", pt[2][0], pt[2][1], pt[2][2] );
			um_pscroll(UM_sbuf);
			sprintf(UM_sbuf, "pt[3]: <%g, %g, %g>", pt[3][0], pt[3][1], pt[3][2] );
			um_pscroll(UM_sbuf);
			sprintf(UM_sbuf, "hull: indx[]: %d %d %d %d",
				indx[0], indx[1],indx[2],indx[3]);
			um_pscroll(UM_sbuf);

			}
#endif
   uu_toolfree (pm);
	uu_dexit;
	}
/**********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_iplncvh(n1,cvh1,n2,cvh2)
**       Determine if two coplanar convex hulls intersect.
**    PARAMETERS   
**       INPUT  : 
**				findall						?? look for all intersections ??
**				cvh1							pointer to first convex hull
**				cvh2							pointer to second convex hull
**       OUTPUT :  
**          *rintersect					UU_TRUE if intersect
**				*rcolinear					UU_TRUE is colinear
**				*nint							?? number of intersections ??
**				intpt							(first?) intersection point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : NOTE: this routine now says that two convex hulls
**		intersect if one is contained in the other (*rintersect will be
**		UU_TRUE).  However, unless a true intersection is found, *nint
**		will be zero, and intpt will be undefined.
*********************************************************************/
static int axis1[3] = 	{0,0,1	};
static int axis2[3] = 	{1,2,2	};

um_iplncvh(findall, cvh1, cvh2, rintersect, rcolinear, nint, intpt)
	UU_LOGICAL findall;
	C2vh	*cvh1;
	C2vh	*cvh2;
	UU_LOGICAL *rintersect;
	UU_LOGICAL *rcolinear;
	int *nint;
	UU_REAL intpt[3];

	{
	UU_LOGICAL	um_ptincvh();
	UU_LOGICAL	intersect = UU_FALSE;
	UU_LOGICAL	colinear = UU_FALSE;

	UU_REAL t1,t2;
	UU_REAL delta1[3];
	UU_REAL delta2[3];
	UU_REAL rhs[3];
	UU_REAL num;
	UU_REAL absden,den;
	UU_REAL abstemp,temp;
	UU_REAL num1,num2;
	UU_REAL temp1[3],temp2[3];
	UU_REAL npt[3];
	UU_REAL uvc1[3];
	UU_REAL UM_INTFUZZ;
	int x,xt,y,yt;
	int i,j,k;
	int ilast,jlast;

	/* these are set to freuently used pointers within structures	*/
	int	*CVH1,	*CVH2;
	UU_REAL	(*PT1)[3], (*PT2)[3];

	uu_denter(UU_MTRC, (us,"um_iplncvh()"));

	/** initialize convenient/fast access	**/
	CVH1 = cvh1->cv;
	CVH2 = cvh2->cv;
	PT1 = cvh1->pt;
	PT2 = cvh2->pt;

	*nint = 0;

	/** HANDLE SMALL CVX HULLS	**/
	if (cvh1->shape & UM_CVH_SMALL)
		{
		intersect = um_ptincvh(PT1[ CVH1[0] ], cvh2);
		colinear = UU_FALSE;
		*nint = 0;

#	if UM_CVHTRC
		sprintf(UM_sbuf,
		"\n------------\n\tum_iplncvh: hull 1 small. intersect = %d.", intersect);
		um_pscroll(UM_sbuf);
#	endif

		goto Done;
		}
	else if (cvh2->shape & UM_CVH_SMALL)
		{
		intersect = um_ptincvh(PT2[ CVH2[0] ], cvh1);
		colinear = UU_FALSE;
		*nint = 0;

#	if UM_CVHTRC
		sprintf(UM_sbuf,
		"\tum_iplncvh: hull 2 small. intersect = %d.", intersect);
		um_pscroll(UM_sbuf);
#	endif

		goto Done;
		}

	/** henceforth, neither hull is UM_CVH_SMALL	**/

	/** CIRCLE REJECTION TEST **/
		{
	UU_REAL dist = 0.0;
/* UU_REAL temp; */
	int		k;

	if ((cvh1->min != -1) && (cvh2->min != -1))
		{
		for (k = 0; k < 3; ++k)
			{
			temp = (cvh1->pt[cvh1->min][k] - cvh2->pt[cvh2->min][k]);
			dist = dist + (temp*temp);
			}
		dist = sqrt(dist) - cvh1->radius - cvh2->radius;

#		if UM_CVHTRC
			{
			sprintf(UM_sbuf,
			"\tcircle test: dist = %f",dist);
			um_pscroll(UM_sbuf);
			}
#		endif

		}

	if (dist > UM_FUZZ)
		{

#		if UM_CVHTRC
			um_pscroll("\thulls rejected by circle test");
#		endif

		goto Done;	/** Note that ninit, intersect, and colinear are set	**/
		}
	}

	if (cvh1->no_cv == 3) ilast = 1; else ilast = cvh1->no_cv - 1;
	if (cvh2->no_cv == 3) jlast = 1; else jlast = cvh2->no_cv - 1;
	UM_INTFUZZ = 0.0001;

	/** Big double loop	**/
	for (i=0; i < ilast; i++)
		{
		um_vcmnvc(PT1[CVH1[i+1]], PT1[CVH1[i]], delta1);

		/** try to  guarantee that delta1 is not teeny	**/
		if (um_mag(delta1) < UM_FUZZ)
			{
			continue;	/** FIX: any way to eliminate this guy?	**/
			}

		um_unitvc(delta1, uvc1);
		for (j=0; j < jlast; j++)
			{
			um_vcmnvc(PT2[CVH2[j+1]], PT2[CVH2[j]], delta2);
			um_vcmnvc(PT2[CVH2[j]], PT1[CVH1[i]], rhs);
			absden = 0.0;
			x = -1;
			y = -1;
			for (k=0; k<3; k++)
				{
				xt = axis1[k];
				yt = axis2[k];
				temp = (delta1[yt]*delta2[xt]) - (delta1[xt]*delta2[yt]);
				abstemp = fabs(temp);
				if (abstemp > absden)
					{
					absden = abstemp;
					den = temp;
					x = xt;
					y = yt;
					}
				}
			if (x == -1)
				{/* check if parallel line segments intersect */
				um_nptln(PT2[CVH2[j]], PT1[CVH1[i]], uvc1, npt);
				if (um_cceqcc(PT2[CVH2[j]], npt))
					{/* segments lie along same line */
					colinear = UU_TRUE;
					um_vcmnvc(PT2[CVH2[j]], PT1[CVH1[i]], temp1);
					um_vcmnvc(PT2[CVH2[j+1]], PT1[CVH1[i]], temp2);
					absden = 0.0;
					for (k=0; k<3; k++)
						{
						abstemp = fabs(delta1[k]);
						if (abstemp > absden)
							{
							absden = abstemp;
							den = delta1[k];
							num1 = temp1[k];
							num2 = temp2[k];
							}
						}
					t1 = num1/den;
					t2 = num2/den;
					intersect = (-UM_INTFUZZ < t1) && (t1 < (1.0+UM_INTFUZZ));
					if (!intersect)
						intersect = (-UM_INTFUZZ < t2) && (t2 < (1.0+UM_INTFUZZ));
					if (!intersect)
						intersect = (t1*t2) < 0.0;
					}
				}
			else /* check if non-parallel line segments intersect */
				{
				num = (rhs[y]*delta2[x]) - (rhs[x]*delta2[y]);
				t1 = num/den;
				intersect = (-UM_INTFUZZ < t1) && (t1 < (1.0+UM_INTFUZZ));
				if (intersect)
					{
					num = (rhs[y]*delta1[x]) - (rhs[x]*delta1[y]);
					t2 = num/den;
					intersect = (-UM_INTFUZZ < t2) && (t2 < (1.0+UM_INTFUZZ));
					}
				if (intersect)
					{
					*nint = *nint + 1;
					um_vctmsc(delta1, t1, temp1);
					um_vcplvc(PT1[CVH1[i]], temp1, intpt);
					}
				}
			if (!findall && intersect)
				goto Done;
			}
		}
		/** End big double 'for' loop	**/
		/* if findall is set, intersection may be found! */

	/** "CONCENTRIC" HULLS TEST	(replaces semi-infinite line test)	**/
	/** (one hull may be interior to the other)	**/

	if ( !intersect )
		{

#	if	UM_CVHTRC
		um_pscroll("\tconcentric hull test.");
#	endif

		colinear = UU_FALSE;
		*nint = 0;

		/* is first vertex of hull 2 within hull 1?	*/
		intersect = um_ptincvh(PT2[CVH2[0]], cvh1);

		if (!intersect)
			/* is first vertex of hull 1 within hull 2?	*/
			intersect = um_ptincvh(PT1[CVH1[0]], cvh2);

#	if	UM_CVHTRC
		if (intersect)
			um_pscroll("\tconcentric hull test: intersect.");
		else
			um_pscroll("\tconcentric hull test: no intersect.");
#	endif

		}

Done:		/** Note that *nint, intersect, and colinear must be set	**/

#	if	UM_CVHTRC
		{
		if (intersect)
			{
			um_pscroll("\tintersect; ");
			if (colinear)
				um_pscroll("\tcolinear; ");
			else
				um_pscroll("\tnot colinear; ");
			sprintf(UM_sbuf,
				"\tnint=%d intpt=(%f,%f,%f)",*nint,intpt[0],intpt[1],intpt[2]);
			um_pscroll(UM_sbuf);
			}
		else
			um_pscroll("\tdon't intersect; ");
		}
#	endif

	*rintersect = intersect;
	*rcolinear = colinear;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : um_seminfline(cvh, len, sinf)
**       Calculate a "semi-infinite" line going from the first point
**			in the same direction as the second point.
**    PARAMETERS   
**       INPUT  : 
**          cvh		pointer to convex hull
**				len		(half??) desired length of semi-inf line
**				sinf		pointer to convex hull structure for output
**				NOTE that the caller must allocate buffer sinf->pt
**       OUTPUT :  
**				*sinf		C2vh filled out as semi-inf. line
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : caller must allocate and set sinf->pt
*********************************************************************/
um_seminfline(cvh, len, sinf)
	C2vh		*cvh;
	UU_REAL	len;
	C2vh		*sinf;
	{
	UU_REAL dir[3];

	uu_denter(UU_MTRC,(us,"um_seminfline(?,?,?)"));

	/* set three elements of sinf->pt array	*/
	um_vctovc(cvh->pt[cvh->cv[0]], sinf->pt[0]);
	um_vcmnvc(cvh->pt[cvh->cv[1]], cvh->pt[cvh->cv[0]], dir);
	um_unitvc(dir, dir);
	um_vctmsc(dir, (2.0 * len), dir);
	um_vcplvc(cvh->pt[cvh->cv[0]], dir, sinf->pt[1]);

	/* set sinf->cvh	*/
	sinf->cv[0] = 0;
	sinf->cv[1] = 1;
	sinf->cv[2] = 0;

	/* set sinf-> remaining fields	*/
	sinf->no_pt = 3;
	sinf->no_cv = 3;			/* ???? 2 maybe? ???? */
	sinf->min = 0;
	sinf->radius = 2 * len;	/* factor by 2 may be wrong, but is conservative */
	sinf->shape  = UM_CVH_SEMINF | UM_CVH_LINEAR;

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION :  UU_LOGICAL	um_ptincvh(pt, cvh)
**       determines whether a point is within (the right parallelpiped
**			of) a planar convex hull
**    PARAMETERS   
**       INPUT  : 
**          pt			--	point in question
**				cvh		--	pointer to convex hull structure
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if pt is in cvh, else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL
um_ptincvh(pt, cvh)
	UU_REAL	pt[3];
	C2vh		*cvh;
	{
	UU_REAL	(*p)[3];		/* faster access to cvh->pt	 */
	int		*cv;			/* faster access to cvh->cv	 */
	int		last_vertex; /* faster access to cvh->no_cv - 1*/
	int		i;
	UU_LOGICAL	ret_val = UU_FALSE;

	UU_REAL	A[3];		/* left angle vector	*/
	UU_REAL	B[3];		/* right angle vector	*/
	UU_REAL	P[3];		/*	vector from vertex to point in question	*/


		/* dot products which are used more than once	*/
	UU_REAL	AdotP, AdotB, BdotP;
	UU_REAL AdotA,BdotB,dA,dB,dP;

	UU_LOGICAL	um_cceqcc();
	UU_LOGICAL	um_ptinseg();

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_ptincvh()"));

	/* initialize fast pointers	*/
	p = cvh->pt;
	cv = cvh->cv;
	last_vertex = cvh->no_cv - 1;	/* last equals first	*/

#	if UM_LOCALTRC
	sprintf(UM_sbuf, "||\nptincvh: pt <%f,%f,%f>", pt[0], pt[1], pt[2]);
	um_pscroll(UM_sbuf);
	um_debug_cvh(cvh);
#	endif

	/** test trivial cases	**/

	if (cvh->shape & UM_CVH_SMALL)
		{
		ret_val = um_cceqcc(pt, p[cv[0]]);

#if	UM_LOCALTRC
		sprintf(UM_sbuf, "\tptincvh: hull small: ret_val = %d", ret_val);
		um_pscroll(UM_sbuf);
#endif

		goto Done;
		}
	else if (cvh->no_cv  == 3)	/* but not small	*/
		{
		if (um_cceqcc(p[cv[0]], p[cv[1]]))
			ret_val = um_cceqcc(p[cv[0]], pt) ;
		else
			{
			ret_val = um_cceqcc(p[cv[1]], pt) ||
							um_cceqcc(p[cv[0]], pt) ||
								um_ptinseg(p[cv[0]], pt, p[cv[1]]);
			}

		/* note: above won't call um_ptinseg() if um_cceqcc() is TRUE	*/

#	if UM_LOCALTRC
		sprintf(UM_sbuf, "\tptincvh: hull linear: ret_val = %d", ret_val);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "endpoints: <%f, %f, %f>, <%f, %f, %f>",
			p[cv[0]][0],p[cv[0]][1],p[cv[0]][2],
			p[cv[1]][0],p[cv[1]][1],p[cv[1]][2]);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "\tpoint between: <%f, %f, %f> ", pt[0],pt[1],pt[2]);
		um_pscroll(UM_sbuf);
#	endif

		goto Done;
		}

	/** general case	**/
	if (cvh->min < 0)
		dP = um_dcccc (p[0],pt);
	else
		dP = um_dcccc (p[cvh->min],pt);
	if (dP > cvh->radius + UM_FUZZ)
		goto Done;

	for ( i = 1; i < last_vertex; i+=2)	/* note that we skip the first vertex	*/
		{
		um_vcmnvc(p[cv[i-1]], p[cv[i]], A);
		um_vcmnvc(p[cv[i+1]], p[cv[i]], B);
		um_vcmnvc(pt, p[cv[i]], P);
		AdotP = um_dot(A, P);
		AdotB	= um_dot(A, B);
		BdotP	= um_dot(B, P);

		AdotA	= um_dot(A, A); dA = sqrt(AdotA);
		BdotB	= um_dot(B, B); dB = sqrt(BdotB);
		dP = sqrt (um_dot(P,P));

#if	UM_LOCALTRC
		sprintf(UM_sbuf, "vertex: %d\n\tAB %g, AP %g, BP %g, AA %g, BB %g",
			i, AdotB, AdotP, BdotP, um_dot(A,A), um_dot(B,B));
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf, "\tA <%g,%g,%g>\tB <%g,%g,%g>,\tP <%g,%g,%g>",
			A[0], A[1], A[2], B[0], B[1], B[2], P[0], P[1], P[2]);
		um_pscroll(UM_sbuf);
#endif

		/* test for wrong side of A vector	*/
/*
..... the formula below checks if (AxB)(AxP) < 0 - which means B and P are on
..... different sides of A
*/
		if (AdotB * AdotP  - AdotA * BdotP  > UM_FUZZ * AdotA*dB*dP)
			{

#	if UM_LOCALTRC
			sprintf(UM_sbuf, "\twrong side of A vector.i = %d, last_vertex = %d",
			i, last_vertex);
			um_pscroll(UM_sbuf);
#	endif

			goto Done;	/* will return UU_FALSE	*/
			}

		/* test for wrong side of B vector	*/
		if (AdotB * BdotP  - BdotB * AdotP  > UM_FUZZ * BdotB*dA*dP)
			{

#	if UM_LOCALTRC
			sprintf(UM_sbuf, "\twrong side of B vector.i = %d, last_vertex = %d",
				i, last_vertex);
			um_pscroll(UM_sbuf);
#	endif

			goto Done;	/* will return UU_FALSE	*/
			}
		}

	if (i == last_vertex)	/* need to test last edge	*/
		{
		um_vcmnvc(p[cv[i-1]], p[cv[i]], A);
		um_vcmnvc(p[cv[1]], p[cv[i]], B);		/* note: i+1 = 1	*/
		um_vcmnvc(pt, p[cv[i]], P);

		AdotP = um_dot(A, P);
		AdotB	= um_dot(A, B);
		BdotP	= um_dot(B, P);

		AdotA	= um_dot(A, A);
		dB = sqrt(um_dot(B,B));
		dP = sqrt(um_dot(P,P));
		/** test for correctness this time	**/
		if (AdotB * AdotP  - AdotA * BdotP  > UM_FUZZ * AdotA*dB*dP)
			{
			goto Done;
			}

#if	UM_LOCALTRC
		sprintf(UM_sbuf,"\tptincvh: last edge: ret_val = %d, i = %d", ret_val, i);
		um_pscroll(UM_sbuf);
#endif

		}

#if	UM_LOCALTRC
	else
		{
		sprintf(UM_sbuf,"\tptincvh: all edges checked");
		um_pscroll(UM_sbuf);
		}
#endif
	ret_val = UU_TRUE;

Done:
	uu_dexit;
	return (ret_val);
	}

/*********************************************************************
**    E_FUNCTION     : um_ilnplncvh(pt1,delta1,n2,cvh2,pt2,
**												rintersect,rts,rtl)
**			Determine if a planar convex hull and a line known to lie
**			in the plane intersect.  If they do, return the parameter
**			value on the line of the smallest and largest intersection.
**    PARAMETERS   
**       INPUT  : 
**				pt1							point on line
**				delta1						delta defining parameterization
**													line = pt1 + (t * delta1)
**				n2								number of points in hull
**				cvh2							index to points in hull
**				pt2							points in hull
**       OUTPUT :  
**				rintersect					UU_TRUE iff infinite line intersects
**												convex hull
**				rts							parameter value of smallest intersection
**				rtl							parameter value of largest intersection
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ilnplncvh(pt1,delta1,n2,cvh2,pt2,rintersect,rts,rtl)
	UU_REAL pt1[3];
	UU_REAL delta1[3];
	int n2;
	int cvh2[];
	UU_REAL pt2[][3];
	UU_LOGICAL *rintersect;
	UU_REAL *rts;
	UU_REAL *rtl;

	{
	UU_LOGICAL intersect, colinear, parallel, segint;
	UU_LOGICAL um_cceqcc();
	UU_LOGICAL um_vcparall();
	UU_REAL t1,t2;
	UU_REAL ts,tl;
	UU_REAL delta2[3];
	UU_REAL rhs[3];
	UU_REAL num;
	UU_REAL absden,den;
	UU_REAL abstemp,temp;
	UU_REAL num1,num2;
	UU_REAL temp1[3],temp2[3];
	UU_REAL npt[3];
	UU_REAL uvc1[3];
	UU_REAL uvc2[3];
	UU_REAL UM_INTFUZZ;
	int x,xt,y,yt;
	int j,k;
	int jlast;

	uu_denter(UU_MTRC,(us,"um_ilnplncvh()"));
	UM_INTFUZZ = 0.001;
	intersect = UU_FALSE;
	ts =  1.0e10;
	tl = -1.0e10;
	if (n2 == 3) jlast = 1; else jlast = n2 - 1;
	um_unitvc(delta1,uvc1);
	for (j=0; j<jlast; j++)
		{
		um_vcmnvc(pt2[cvh2[j+1]],pt2[cvh2[j]],delta2);
		um_unitvc(delta2,uvc2);
		um_vcmnvc(pt2[cvh2[j]],pt1,rhs);
		absden = 0.0;
		x = -1;
		y = -1;
		for (k=0; k<3; k++)
			{/* find best axies to use in calculation */
			xt = axis1[k];
			yt = axis2[k];
			temp = (delta1[yt]*delta2[xt]) - (delta1[xt]*delta2[yt]);
			abstemp = fabs(temp);
			if (abstemp > absden)
				{
				absden = abstemp;
				den = temp;
				x = xt;
				y = yt;
				}
			}
		if (um_vcparall(uvc1,uvc2))
			{/* check if parallel line segments intersect */
			um_nptln(pt2[cvh2[j]],pt1,uvc1,npt);
			colinear = UU_FALSE;
			parallel = UU_TRUE;
			if (um_cceqcc(pt2[cvh2[j]],npt))
				{/* segments lie along same line */
				colinear = UU_TRUE;
				um_vcmnvc(pt2[cvh2[j]],pt1,temp1);
				um_vcmnvc(pt2[cvh2[j+1]],pt1,temp2);
				absden = 0.0;
				for (k=0; k<3; k++)
					{
					abstemp = fabs(delta1[k]);
					if (abstemp > absden)
						{
						absden = abstemp;
						den = delta1[k];
						num1 = temp1[k];
						num2 = temp2[k];
						}
					}
				t1 = num1/den;
				t2 = num2/den;
				if (t1 < ts) ts = t1;
				if (t1 > tl) tl = t1;
				if (t2 < ts) ts = t2;
				if (t2 > tl) tl = t2;
				intersect = UU_TRUE;
				}
			}
		else /* check if non-parallel line segments intersect */
			{
			parallel = UU_FALSE;
			segint = UU_FALSE;
			num = (rhs[y]*delta1[x]) - (rhs[x]*delta1[y]);
			t2 = num/den;
			if ((-UM_INTFUZZ < t2) && (t2 < (1.0+UM_INTFUZZ)))
				{
				num = (rhs[y]*delta2[x]) - (rhs[x]*delta2[y]);
				t1 = num/den;
				um_vctmsc(delta1,t1,temp1);
				um_vcplvc(pt1,temp1,temp1);
				um_vctmsc(delta2,t2,temp2);
				um_vcplvc(pt2[cvh2[j]],temp2,temp2);
				/*if (um_cceqcc(temp1,temp2))*/
					{
					if (t1 < ts) ts = t1;
					if (t1 > tl) tl = t1;
					intersect = UU_TRUE;
					segint = UU_TRUE;
					}
				}
			}
#if UM_CVHTRC
		if (parallel)
			{
			if (colinear)
				um_pscroll("parallel; colinear;");
			else
				um_pscroll("parallel; not colinear");
			}
		else
			{
			if (segint)
				{
				sprintf(UM_sbuf,"non-parallel; intersect t1=%f t2=%f",t1,t2);
				um_pscroll(UM_sbuf);
				}
			else
				um_pscroll("non-parallel; no intersect");
			}
#endif
		}
	*rintersect = intersect;
	*rts = ts;
	*rtl = tl;
	uu_dexit;
	}

#if	UM_CVHTRC || UM_LOCALTRC
um_debug_cvh(cvh)
	C2vh	*cvh;
	{
	int i;

	um_pscroll("debug convex hull:");
	sprintf(UM_sbuf, "\tno_pt %d, no_cv %d", cvh->no_pt, cvh->no_cv);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "\tmin %d, radius %f, shape %x", cvh->min, cvh->radius,
	cvh->shape);
	um_pscroll(UM_sbuf);
	for (i = 0; i < cvh->no_cv; ++i)
		{
		sprintf(UM_sbuf, "\t\tpoint %d: <%f, %f, %f>", i,
		cvh->pt[cvh->cv[i]][0], cvh->pt[cvh->cv[i]][1], cvh->pt[cvh->cv[i]][2]);
		um_pscroll(UM_sbuf);
		}
	um_pscroll(" ");
	}
#endif
