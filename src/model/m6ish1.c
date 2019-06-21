/*********************************************************************
**    NAME         :  m6ish1.c
**       CONTAINS: internal support routines for tessellation of faces
**			int umi_face_tessellate(f, fillflag)
**			int umi_uvparam_compare(a, b)
**			int umi_get_srf_vparams(sf, v_draw, vparam)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m6ish1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:07
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "go.h"
#include "ginq.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdebug.h"
#include "mdeval.h"
#include "mcrv.h"
#include "msol.h"
#include "modef.h"
#include "mderror.h"

#include "ag_incl.h"
#include "ag_global.h"

typedef struct
	{
	UU_REAL u;
	UU_REAL v;
	int cur;
	int prev;
	int next;
	} UM_uvparam;

/*********************************************************************
**    E_FUNCTION     : int umi_face_tessellate(f, fillflag)
**       Tessellate a trimmed surface (e.g. face F).
**    PARAMETERS   
**       INPUT  : 
**          f					pointer to AG face 
**				fillflag			UU_TRUE -> draw filled polygons
**									UU_FALSE -> draw polygon borders
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_face_tessellate(f, fillflag)
	AG_FACEP f;
	UU_LOGICAL fillflag;

	{
	int status = UU_SUCCESS;
	UM_uvparam uv;
	UM_param t0, t1, dt;
	UU_LIST scanlist;
	UU_LIST uvlist;
	UU_LIST ualisthead;
	UU_LIST *ualist;
	UU_LIST ublisthead;
	UU_LIST *ublist;
	UU_LIST *tlist;
	UU_REAL *vparam;
	UU_REAL *uaparam;
	UU_REAL *ubparam;
	UM_uvparam *uvparam;
	UM_uvparam *prevuv;
	UM_uvparam *nextuv;
	UU_LOGICAL prevon;
	UU_LOGICAL nexton;
	UU_REAL u;
	int umi_uvparam_compare();
	int um_param_compare();
	int i,j, k, l, ns;
	int markertype;
	UU_LOGICAL active, prevactive, nextactive;
	UU_LOGICAL found;
	UU_LOGICAL boundary_pts_on_a;
	UU_REAL va;
	int ia, ja, na;
	UU_REAL ua1, ua2;
	UU_REAL ua[100];
	UU_LOGICAL boundary_pts_on_b;
	UU_REAL vb;
	int ib, jb, nb;
	UU_REAL ub1, ub2;
	UU_REAL ub[100];
	UM_coord spt[4];
	UM_vector snorm[4];
	int *index_to_ptr;
	int start;

	AG_CNODEP node;						/* pointer to node */
	AG_BOUNDARYP b;						/* pointer to boundary curve */
	AG_TEDGEP te;							/* pointer to twin edge */
	AG_CURVEP pe;							/* pointer to parameter edge */
	AG_CURVEP cv;							/* pointer to curve */
	AG_SPLINEP bs;							/* pointer to bspline */
	AG_SURFACEP sf;						/* pointer to surface */
	AG_CPOINT uvpoint;					/* curve evaluation record */
	AG_CP_LISTP ag_x_bnd_rayv();
	AG_CPOINTP cp;
	AG_CPOINTP cpa;
	AG_CPOINTP cpb;
	AG_CPOINTP cpbnext;

	uu_denter(UU_MTRC, (us,"umi_face_tessellate(face=%x, fillflag=%d)",
		f, fillflag));

	/* debugging */
	gslinecolor(UM_MAGENTA);
	gsmarkcolor(UM_MAGENTA);
	markertype = gqmarktype();	/* get current marker type */

	/* initialization */
	sf = f->srf;
	uvpoint.prev = NULL;
	uvpoint.next = NULL;
	uvpoint.P = (double *) &uv;
	uu_list_init(&scanlist, sizeof(UU_REAL), 100, 100);
	uu_list_init(&uvlist, sizeof(UM_uvparam), 100, 100);

	/**********************************************************************
	STEP 1:	Traverse the boundary of the face to determine the critial uv
				parameters which must be present in the tessellation of the
				face.  For lines, these are simply the endpoints of the curve.
				For other curves, the curve is evaluated at interior points.
				The uv list is then sorted and the indices which indicate
				which edge the parameter came from is replace with pointers
				for faster access.
	**********************************************************************/
	b = f->ob;
	do
		{
		te =  b->te0;
		start = uvlist.cur_cnt;
		do
			{
			pe = te->pedge;
			t0 = *(pe->bs0->node0->t);
			t1 = *(pe->bs0->prev->noden->t);
			cv = te->edge;
			if (cv == NULL) cv = te->twin->edge;
			if (cv == NULL)
				ns = 1;
			else if ((cv->nbs == 1) && (cv->bs->ctype == AG_LINE))
				ns = 1;
			else ns = UM_srfattr.ptsperucrv;
			dt = (t1 - t0) / ns;
			sprintf(UM_sbuf," t0=%g, t1=%g, dt=%g, ns=%d", t0, t1, dt, ns);
			um_pscroll(UM_sbuf);
			for (i=0; i<ns; i++)
				{
				ag_eval_crv(t0, 0, pe, &uvpoint);
				uv.cur = uvlist.cur_cnt;
				uv.prev= uv.cur - 1;
				uv.next= uv.cur + 1;
				uu_list_push(&uvlist, &uv);
				uu_list_push(&scanlist, &uv.v);
				t0 += dt;
				}
			te = te->next;
			}
		while ((te != NULL) && (te != b->te0));
		uvparam = (UM_uvparam *) UU_LIST_ARRAY(&uvlist);
		uvparam[start].prev = uvlist.cur_cnt - 1;
		uvparam[uvlist.cur_cnt-1].next = start;
		b = b->next;
		}
	while((b != NULL) && (b != f->ob));

	/* sort uv parameter list by v then u (e.g (uv(i) < uv(j)
		iff v(i) < v(j) or v(i) = v(j) and u(i) < u(j))) */
	uu_list_sort(&uvlist, umi_uvparam_compare);
	uvparam = (UM_uvparam *) UU_LIST_ARRAY(&uvlist);
	um_pscroll("SORTED UVPARAM LIST (WITH INDICIES)");
	for (i=0; i<uvlist.cur_cnt; i++)
		{
		sprintf(UM_sbuf, "uvparam[%d] = (C=%d, P=%d ,N=%d, U=%g, V=%g)", i,
			 uvparam[i].cur, uvparam[i].prev, uvparam[i].next,
			 uvparam[i].u, uvparam[i].v);
		um_pscroll(UM_sbuf);
		}

	/* convert prev and next indicies for parameter list to pointers */
	uvparam = (UM_uvparam *) UU_LIST_ARRAY(&uvlist);
	index_to_ptr = (int *) uu_malloc(uvlist.cur_cnt * sizeof(int));
	for (i=0; i<uvlist.cur_cnt; i++)
		index_to_ptr[uvparam[i].cur] = (int) &uvparam[i];
	for (i=0; i<uvlist.cur_cnt; i++)
		{
		uvparam[i].prev = index_to_ptr[uvparam[i].prev];
		uvparam[i].next = index_to_ptr[uvparam[i].next];
		}
	uu_free(index_to_ptr);

	/**********************************************************************
	STEP 2:	Merge the v parameter values which specify the tessellation
				for the underlying surface with the parameter values
				determined in step 1.  The resulting list represents the v
				parameter "scan line" parameters which will be used to define
				the polygons which tessellate the face.
	**********************************************************************/
	/*umi_get_srf_vparams(sf, UM_srfattr.numvpaths, &scanlist);*/
	uu_list_sort(&scanlist, um_param_compare);
	vparam = (UU_REAL *) UU_LIST_ARRAY(&scanlist);
	for (i=(scanlist.cur_cnt-1); i>0; i--)
		{
		if (fabs(vparam[i] - vparam[i-1]) < AG_tol_knot)
			{
			for (j=i; j<scanlist.cur_cnt; j++) vparam[j-1] = vparam[j];
			scanlist.cur_cnt--;
			}
		}
	um_pscroll("SORTED SCANLIST WITH DUPLICATES REMOVED");
	for (i=0; i<scanlist.cur_cnt; i++)
		{
		sprintf(UM_sbuf, "scanlist[%d]=%g", i, vparam[i]);
		um_pscroll(UM_sbuf);
		}

	/**********************************************************************
	STEP 3:	Generate the polygons which cover the face:
					a.	SCANLIST is used to define the v parameter values
						of two isoparametric curves (A and B) which are used
						to define the top and bottom of the polygon in parameter 
						space
					b.	UVPARAM represents the vertices of points on the surface
						along the boundary 
					c. UALIST and UBLIST represent the intersections of rays
						along the A and B scan lines which define portions of the
						isoparameter line which clearly lie within the interior
						of the parameter space.
					d. UALIST + UVPARAM and UBLIST + UVPARAM together define
						a list of u parameters which taken pairwise define the
						polygons which cover the face between the A and B curves.
	**********************************************************************/
	uu_list_init(&ualisthead, sizeof(UU_REAL), 100, 100);
	uu_list_init(&ublisthead, sizeof(UU_REAL), 100, 100);
	ualist = &ualisthead;
	ublist = &ublisthead;
	ja = 0;
	b = f->ob;
	for (i=0; i<scanlist.cur_cnt; i++)
		{
		/* find all V parameters on boundary at next scanline V parameter */
		for (j=ja; j<uvlist.cur_cnt; j++)
			if (fabs(vparam[i] - uvparam[j].v) > AG_tol_knot)
				break;
		vb = vparam[i];
		ib = ja;
		jb = j;
		boundary_pts_on_b = (jb > ib);

		/* find all intersections on constant V parameter curve with boundary */
		umi_x_bnd_rayv(vb, &uvlist, ublist);

		/* just for debugging */
		gsmarktype(UM_ptattr.markertype);
		ubparam = (UU_REAL *) UU_LIST_ARRAY(ublist);
		if (ublist->cur_cnt > 0)
			{
			um_pscroll("");
			for (k=0; k<ublist->cur_cnt; k++)
				{
				sprintf(UM_sbuf,"uv=(%g,%g)", ubparam[k], vb);
				um_pscroll(UM_sbuf);
				ag_eval_srf_norm(ubparam[k], vb, sf, spt, snorm);
				/*gpolymarker3(1, spt);*/
				}
			}

		/* generate polygons by scanning both the ua and ub parameter lists */
		if (i > 0)
			{
			/* find active spans on A curve */
			um_pscroll("find A spans");
			sprintf(UM_sbuf, "ia=%d, ja=%d", ia, ja);
			um_pscroll(UM_sbuf);
			na = 0;
			if (ualist->cur_cnt > 0)
				{
				uaparam = (UU_REAL *) UU_LIST_ARRAY(ualist);
				for (j=0; j<(ualist->cur_cnt); j++)
					{
					ua[na] = uaparam[j];
					na++;
					}
				}
			if (na == 0)
				um_pscroll("..initial ua list is empty");
			else		
				{
				for (j=0; j<na; j++)
					{
					sprintf(UM_sbuf,"..initial ua[%d]=%g", j, ua[j]);
					um_pscroll(UM_sbuf);
					}
				}
			if (boundary_pts_on_a) for (j=ia; j<ja; j++)
				{
				prevuv = (UM_uvparam *) uvparam[j].prev;
				nextuv = (UM_uvparam *) uvparam[j].next;
				prevactive = (prevuv->v > (va + AG_tol_knot));
				nextactive = (nextuv->v > (va + AG_tol_knot));
				active = prevactive || nextactive;
				sprintf(UM_sbuf,"..uvparam[%d].u=%g prevactive=%d nextactive=%d", 
					j, uvparam[j].u, prevactive, nextactive);
				um_pscroll(UM_sbuf);
				found = UU_FALSE;
				if (na > 0)
					{
					for (k=0; (!found) && (k<na); k++)
						{
						sprintf(UM_sbuf,"..compare to ua[%d]=%g", k, ua[k]);
						um_pscroll(UM_sbuf);
						if (fabs(ua[k] - uvparam[j].u) < AG_tol_knot)
							{ 
							found = UU_TRUE;
							if (!active) 
								{
								um_pscroll("....remove from ua list");
								for (l=k; l<(na-1); l++) ua[l] = ua[l+1];
								na--;
								}
							}
						else if (uvparam[j].u < ua[k])
							{
							found = UU_TRUE;
							if (active) 
								{
								if (prevactive)
									{
									um_pscroll("....add to ua because of prev test");
									for (l=(na-1); l>=k; l--) ua[l+1] = ua[l];
									ua[k] = uvparam[j].u;
									na++;
									}
								if (nextactive)
									{
									um_pscroll("....add to ua because of next test");
									for (l=(na-1); l>=k; l--) ua[l+1] = ua[l];
									ua[k] = uvparam[j].u;
									na++;
									}
								}
							}
						}
					}
				if (! found)
					{
					um_pscroll("..uvparam not found in ua list");
					if (prevactive)
						{
						um_pscroll("....add to ua because of prev test");
						ua[na] = uvparam[j].u;
						na++;
						}
					if (nextactive)
						{
						um_pscroll("....add to ua because of next test");
						ua[na] = uvparam[j].u;
						na++;
						}
					}
				}
			for (j=0; j<na; j++)
				{
				sprintf(UM_sbuf, "..final ua[%d]=%g", j, ua[j]);
				um_pscroll(UM_sbuf);
				}

			/* find active spans on vb curve */
			um_pscroll("find B spans");
			sprintf(UM_sbuf, "ib=%d, jb=%d", ib, jb);
			um_pscroll(UM_sbuf);
			nb = 0;
			if (ublist->cur_cnt > 0)
				{
				for (j=0; j<(ublist->cur_cnt); j++)
					{
					ub[nb] = ubparam[j];
					nb++;
					}
				}
			if (nb == 0)
				um_pscroll("..initial ub list is empty");
			else		
				{
				for (j=0; j<nb; j++)
					{
					sprintf(UM_sbuf,"..initial ub[%d]=%g", j, ub[j]);
					um_pscroll(UM_sbuf);
					}
				}
			if (boundary_pts_on_b) for (j=ib; j<jb; j++)
				{
				prevuv = (UM_uvparam *) uvparam[j].prev;
				nextuv = (UM_uvparam *) uvparam[j].next;
				prevactive = (prevuv->v < (vb - AG_tol_knot));
				nextactive = (nextuv->v < (vb - AG_tol_knot));
				active = (prevactive || nextactive);
				sprintf(UM_sbuf,"..uvparam[%d].u=%g prevactive=%d nextactive=%d", 
					j, uvparam[j].u, prevactive, nextactive);
				um_pscroll(UM_sbuf);
				found = UU_FALSE;
				if (nb > 0)
					{
					for (k=0; (!found) && (k<nb); k++)
						{
						sprintf(UM_sbuf,"..compare to ub[%d]=%g", k, ub[k]);
						um_pscroll(UM_sbuf);
						if (fabs(ub[k] - uvparam[j].u) < AG_tol_knot)
							{ 
							found = UU_TRUE;
							if (!active) 
								{
								um_pscroll("....remove from ub list");
								for (l=k; l<(nb-1); l++) ub[l] = ub[l+1];
								nb--;
								}
							}
						else if (uvparam[j].u < ub[k])
							{
							found = UU_TRUE;
							if (active) 
								{
								if (prevactive)
									{
									um_pscroll("....add to ub because of prev test");
									for (l=(nb-1); l>=k; l--) ub[l+1] = ub[l];
									ub[k] = uvparam[j].u;
									nb++;
									}
								if (nextactive)
									{
									um_pscroll("....add to ub because of next test");
									for (l=(nb-1); l>=k; l--) ub[l+1] = ub[l];
									ub[k] = uvparam[j].u;
									nb++;
									}
								}
							}
						}
					}
				if (!found)
					{
					um_pscroll("..uvparam not found in ub list");
					if (prevactive)
						{
						um_pscroll("....add to ub because of prev test");
						ub[nb] = uvparam[j].u;
						nb++;
						}
					if (nextactive)
						{
						um_pscroll("....add to ub because of next test");
						ub[nb] = uvparam[j].u;
						nb++;
						}
					}
				}
			for (j=0; j<nb; j++)
				{
				sprintf(UM_sbuf, "..final ub[%d]=%g", j, ub[j]);
				um_pscroll(UM_sbuf);
				}

			for (k=0; k<na; k=k+2)
				{
				ua1 = ua[k];
				if (k+1 < na) ua2 = ua[k+1]; else ua2= ua1;
				ub1 = ub[k];
				if (k+1 < nb) ub2 = ub[k+1]; else ub2 = ub1;

				if (fabs(ua1 - ua2) > AG_tol_knot)
					{
					um_pscroll("triangle along va");
					ag_eval_srf_norm(ua1, va, sf, spt[0], snorm[0]);
					ag_eval_srf_norm(ua2, va, sf, spt[1], snorm[1]);
					ag_eval_srf_norm(ub1, vb, sf, spt[2], snorm[2]);
					um_vctovc(spt[0], spt[3]);
					um_vctovc(snorm[0], snorm[3]);
					if (f->flipnorm)
						for (j=0; j<4; j++) 
							um_vctmsc(snorm[j], (UU_REAL) -1.0, snorm[j]);
					if (fillflag)
						gfillareanorm3( 3, spt, snorm );
					else
						gpolyline3(4, spt);
						/*gfillarea3(4, spt);*/
					}
	
				if (fabs(ub1 - ub2) > AG_tol_knot)
					{
					um_pscroll("triangle along vb");
					ag_eval_srf_norm(ua2, va, sf, spt[0], snorm[0]);
					ag_eval_srf_norm(ub2, vb, sf, spt[1], snorm[1]);
					ag_eval_srf_norm(ub1, vb, sf, spt[2], snorm[2]);
					um_vctovc(spt[0], spt[3]);
					um_vctovc(snorm[0], snorm[3]);
					if (f->flipnorm)
						for (j=0; j<4; j++) 
							um_vctmsc(snorm[j], (UU_REAL) -1.0, snorm[j]);
					if (fillflag)
						gfillareanorm3( 3, spt, snorm );
					else
						gpolyline3(4, spt);
						/*gfillarea3(4, spt);*/
					}
				}

			}

		/* vb curve becomes va curve for next pass */
		tlist = ualist;
		ualist = ublist;
		ublist = tlist;
		boundary_pts_on_a = boundary_pts_on_b;
		va = vb;
		ia = ib;
		ja = jb;
		}
	uu_list_free(ualist);
	uu_list_free(ublist);
	uu_list_free(&uvlist);
	uu_list_free(&scanlist);
	gsmarktype(markertype);

	uu_dexitstatus("umi_face_tessellate", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int umi_uvparam_compare(a, b)
**       Compare a and b where a < b iff
**						1. a.v < b.v or
**					or
**						2. a.v = b.v and a.u < b.u
**    PARAMETERS   
**       INPUT  : 
**          a						(u,v) parameter value
**          b						(u,v) parameter value
**       OUTPUT :  
**          none
**    RETURNS      : 
**				-1		if a < b
**				 0		if a = b
**				 1		if a > b
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_uvparam_compare(a, b)
	UM_uvparam *a;
	UM_uvparam *b;

	{
	if (fabs(a->v - b->v) < AG_tol_knot)
		{
		if (fabs(a->u - b->u) < AG_tol_knot) return (0);
		else if (a->u < b->u) return (-1);
		else return (1);
		}
	else if (a->v < b->v) return (-1);
	else return (1);
	}

/*********************************************************************
**    E_FUNCTION     : int umi_get_srf_vparams(sf, v_draw, vparam)
**       Return the v parameters to be drawn on a surface
**    PARAMETERS   
**       INPUT  : 
**          sf					pointer to surface
**				v_draw			0 => v0 and vn only
**									1 => all v knot values
**									n => n-1 v values between knots
**       OUTPUT :  
**          vparam			list of v parameter values
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_get_srf_vparams(sf, v_draw, vparam)
	AG_SURFACEP sf;
	int v_draw;
	UU_LIST *vparam;

	{ 
	int				status = UU_SUCCESS;
	int				nv, i, j;
	UU_REAL			*v0, *v1, s, ds;
	AG_SNODEP		snd, snd_save;

	uu_denter(UU_MTRC,(us,"umi_get_srf_vparams(sf=%x, v_draw=%d)",
		sf, v_draw));

	snd = sf->node0;
	nv = sf->nv;
	v0 = snd->v;
	snd_save = sf->node;

	/* get v0 */
	s = *v0;
	uu_list_push(vparam, &s);

	for (i=1; i<=nv; i++)
		{
		snd = snd->vnext; v1 = snd->v;
		if (v0 != v1)
			{/* span has length */
			if (v_draw > 1)
				{ 
				s = *v0;
				ds = (*v1 - s)/v_draw;
				for (j=1; j<v_draw; j++)
					{/* extract and draw s = s + ds */
					s += ds;
					uu_list_push(vparam, &s);
					}
				}
			if (v_draw >= 1  &&  i != nv)
				{
				/* draw v1 */
				s = *v1;
				uu_list_push(vparam, &s);
				}
 			}
		v0 = v1;
		}

	uu_dexitstatus("umi_get_srf_vparams", status);
	return(status);
	} 

/*********************************************************************
**    E_FUNCTION     :
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
int
umi_x_bnd_rayv(v, uvlist, ublist)
	UU_REAL v;
	UU_LIST *uvlist;
	UU_LIST *ublist;

	{
	int i;
	int n;
	int um_param_compare();
	UU_REAL u, du, vm, vp, dv;
	UM_uvparam *nextuv;
	UM_uvparam *prevuv;
	UM_uvparam *uvparam;
	UU_REAL *ub;
	UU_LOGICAL x;

	uu_denter(UU_MTRC, (us,"umi_x_bnd_rayv(v=%g)", v));

	ublist->cur_cnt = 0;
	vm = v - AG_tol_knot;
	vp = v + AG_tol_knot;

	/* get addresses of various lists */
	uvparam = (UM_uvparam *) UU_LIST_ARRAY(uvlist);

	/* check next segments for boundary segments in list */
	sprintf(UM_sbuf, "umi_x_bnd_rayv: v=%g", v);
	um_pscroll(UM_sbuf);
	for (i=0; i<uvlist->cur_cnt; i++)
		{
		prevuv = (UM_uvparam *) uvparam[i].prev;
		nextuv = (UM_uvparam *) uvparam[i].next;
		x = UU_FALSE;
		if (fabs(uvparam[i].v - v) < AG_tol_knot)
			x = ((nextuv->v < vm && prevuv->v > vp) ||
				  (nextuv->v > vp && prevuv->v < vm));
		else if (uvparam[i].v < vm)
			x = (nextuv->v > vp);
		else if (uvparam[i].v > vp)
			x = (nextuv->v < vm);
		if (x)
			{
			dv = (nextuv->v - uvparam[i].v);
			if (fabs(dv) > AG_tol_knot)
				{
				du = (nextuv->u - uvparam[i].u) / dv;
				u = (v - uvparam[i].v) * du + uvparam[i].u;
				uu_list_push(ublist, &u);
				}
			sprintf(UM_sbuf,"..push x: i=%d, dv=%g, u=%g", i, dv, u);
			um_pscroll(UM_sbuf);
			}
		}

	/* sort intersection parameters */
	uu_list_sort(ublist, um_param_compare);

	/* remove duplicates */

	uu_dexit;
	}

