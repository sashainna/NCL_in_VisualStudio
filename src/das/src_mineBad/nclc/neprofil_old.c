#include "usysdef.h"

/*********************************************************************
**    NAME         :  neprofil.c
**       CONTAINS: Support routines used by the PROFIL command
**           ncl_profile_curve
**           ncl_profile_anote
**           ncl_profile_ps
**           ncl_profile_start
**           ncl_profile_offset
**           ncl_profile_project
**           ncl_profile_weed
**           ncl_profile_entry
**           ncl_profile_circle
**           ncl_profile_pt
**           ncl_profile_free
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       neprofil.c , 25.5
**    DATE AND TIME OF LAST MODIFICATION
**       11/22/17 , 10:55:13
*********************************************************************/

#include "nccs.h"
#include "nclfc.h"
#include "gsegop.h"
#include "mcrv.h"
#include "modef.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mgeom.h"
#include "msrf.h"
#include "nclvx.h"
#include "nclwaterln.h"
#include "atext.h"
#include "math.h"

#define DEBUG 0
#define NCVS 5
#define CVPT 0
#define OFPT 1
#define PJPT 2
#define WDPT 3
#define ENPT 4
#define CURVE 0
#define ANOTE 1

static void S_list_ary(),S_list_ptr(),S_print_curve(),S_free_offset();
static void S_pts_at_dis(),S_pts_at_arc(),S_print_time(),S_profile_maxdp();
static void S_calc_tilt_angles();
static UU_LOGICAL S_isect_prof();
void ncl_profile_entry();
void ncl_profile_pt();

static int Snpts[NCVS],Sldir,Sps_type,Sps_xproj,Sps_top,Sclose,Stype;
static int *Spoly_start=UU_NULL,*Spoly_npts=UU_NULL,Snpoly,Sps_tilt_flag[2];
static UU_LOGICAL Sps_tlaxfl;
static UU_LOGICAL Sinit[NCVS]={UU_FALSE,UU_FALSE,UU_FALSE,UU_FALSE,UU_FALSE};
static UU_REAL Sps_thick,Sps_depth,Stol,Sptdis,Sps_tilt_dis[2],Sps_tilt_ang[2];
static UM_vector Sps_vec;
static UM_plane Sps_plane;
static UU_LIST Scvpt[NCVS],Scvvc[NCVS];
static int Snofs,*Snofpts,Sofinc;
static UU_LIST *Sofpt,*Sofvc;
static NCL_w_arc *Scirc=UU_NULL;
static UM_transf Stfmat;
static UM_vector S_pv[2];
static UM_vector S_pp1[2];

#if DEBUG == 1
static int Stim[NCVS+1]={0,0,0,0,0,0},Stims,Stimm,Stems,Stemm;
#endif

/*********************************************************************
**    E_FUNCTION     : ncl_profile_curve (tol,npts,lrev)
**       Evolves an input curve and creates the evaluated points around
**       the curve within tolerance.  Also determines the direction for
**       the offset curves based on input coordinate.
**    PARAMETERS
**       INPUT  :
**          tol      = Tolerance to evaluate curve at.
**          lrev     = Reverse the curve iff TRUE.
**       OUTPUT :
**          npts     = Number of points generated.  0 means error.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_curve(tol,npts,lrev)
UM_int4 *npts;
UM_real8 *tol;
UM_int2 *lrev;
{
	int ix,i,nc,status;
	UM_coord *pp;
	UM_vector *pv;
	UU_KEY_ID delkey = NULLKEY;
/*
.....Initialize routine
*/
	*npts = 0;
	Stol = *tol;
	Stype = CURVE;
	ix = 2;
/*
.....Evolve the curve
*/
	if (Sinit[CVPT])
	{
		uu_list_free(&Scvpt[CVPT]);
		uu_list_free(&Scvvc[CVPT]);
	}
#if DEBUG == 1
	gtimx(&Stims,&Stimm);
#endif
			
	if (*lrev == 1)
	{
		status = ncl_cv_revers (ix,&delkey);
		if (status != 0)
		{
			Snpts[CVPT] = 0;
			goto done;
		}
	}


	Snpts[CVPT] = ncevolv(ix,Stol,&Scvpt[CVPT],&Scvvc[CVPT]);

#if DEBUG == 1
	gtimx(&Stems,&Stemm);
	S_print_time(CVPT);
#endif
	Sinit[CVPT] = UU_TRUE;
/*
.....Convert curve through MODSYS
*/
	if (Snpts[CVPT] > 0)
	{
		if (wcsact())
		{
			S_list_ary(CVPT,&pp,&pv,&nc);
			for (i=0;i<Snpts[CVPT];i++)
			{
				ncl_wcstomcs(0,pp[i],pp[i]);
				ncl_wcstomcs(1,pv[i],pv[i]);
			}
		}
		*npts = Snpts[CVPT];
	}

done:				
	if (delkey > NULLKEY) uc_delete (delkey);
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_profile_anote (nclkey,npts,npoly)
**       Calculates the polylines of an annotation to machine and stores
**       them in the curve array.  This curve array will actually contain
**       multiple curves (polylines), a separate array that points
**       to the beginning of each polyline, and an array that lists the
**       number of locations in each array.
**
**       This is the single array that will be used for each of the
**       different curve types (Curve, Offset, Projected, Weeded).
**    PARAMETERS
**       INPUT  :
**          nclkey    = Key of annotation.
**       OUTPUT :
**          npts     = Number of points generated.  0 means error.
**          npoly    = Number of polylines generated.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_anote(nclkey,npts,npoly)
UU_KEY_ID *nclkey;
UM_int4 *npts,*npoly;
{
	int i,j,inc,np,status;
	UM_coord *pp;
	UM_vector *pv,lstvec,tvec;
	UM_covec *ptv;
	UM_transf tfmat;
	struct UA_txt_rec note;
	struct UA_txtattr_rec txtattr;
/*
.....Initialize routine
*/
	*npts = 0;
	Stol = .001;
	Stype = ANOTE;
	um_vctovc(Sps_vec,lstvec);
	if (wcsact()) ncl_mcstowcs(1,lstvec,lstvec);
/*
.....Free the previous curve
*/
	if (Sinit[CVPT])
	{
		uu_list_free(&Scvpt[CVPT]);
		uu_list_free(&Scvvc[CVPT]);
		if (Spoly_start != UU_NULL) uu_free(Spoly_start);
		if (Spoly_npts != UU_NULL) uu_free(Spoly_npts);
		Spoly_start = Spoly_npts = UU_NULL;
	}
/*
.....Get the note
*/
	note.key = *nclkey;
	status = ua_get_text1(&note);

	if (status == UU_SUCCESS)
	{
		uc_retrieve_attr(note.key,&txtattr);
/*
.....Note does not have a projected display list
.....Generate the point list
*/
		if (note.no_displst == 0)
		{
			uc_retrieve_transf(note.key,tfmat);
			ug_text_capture(UU_TRUE,&Scvpt[CVPT]);
			ua_draw_text(&note,tfmat,&txtattr);
			ug_text_capture(UU_FALSE,UU_NULL);
/*
........Generate the vector list
*/
			np = UU_LIST_LENGTH(&Scvpt[CVPT]);
			uu_list_init(&Scvvc[CVPT],sizeof(UM_vector),np,50);
			um_vctovc(txtattr.plane,tvec);
			if (um_dot(tvec,lstvec) < 0.0) um_vctmsc(tvec,-1.,tvec);
			for (i=0;i<np;i++) uu_list_push(&Scvvc[CVPT],tvec);
		}
/*
.....Note has a projected display list
.....Store point-vectors in internal lists
*/
		else
		{
			ptv = (UM_covec *)note.displst;
			np = note.no_displst;
			uu_list_init(&Scvpt[CVPT],sizeof(UM_coord),np,50);
			uu_list_init(&Scvvc[CVPT],sizeof(UM_vector),np,50);
			for (i=0;i<np;i++)
			{
				uu_list_push(&Scvpt[CVPT],ptv[i]);
				if (um_mag(&ptv[i][3]) > UM_FUZZ)
				{
					if (um_dot(&ptv[i][3],lstvec) < 0.0)
						um_vctmsc(&ptv[i][3],-1.,&ptv[i][3]);
					um_vctovc(&ptv[i][3],lstvec);
				}
				uu_list_push(&Scvvc[CVPT],&ptv[i][3]);
			}
		}
/*
.....Calculate number of polylines,
.....Start of each polyline, and
.....Number of points in each polyline
*/
		pp = (UM_coord *)UU_LIST_ARRAY(&Scvpt[CVPT]);
		pv = (UM_vector *)UU_LIST_ARRAY(&Scvvc[CVPT]);
		np = pp[0][0];
		Spoly_start = (int *)uu_malloc(np*sizeof(int));
		Spoly_npts = (int *)uu_malloc(np*sizeof(int));
		Snpts[CVPT] = 0;
		inc = 1;
		Snpoly = 0;
		for (i=0;i<np;i++)
		{
			if (pp[inc][0] > 1)
			{
				Spoly_npts[Snpoly] = pp[inc++][0];
				Spoly_start[Snpoly] = Snpts[CVPT];
				for (j=0;j<Spoly_npts[Snpoly];j++)
				{
					um_vctovc(pp[inc+j],pp[Snpts[CVPT]]);
					um_vctovc(pv[inc+j],pv[Snpts[CVPT]]);
					Snpts[CVPT]++;
				}
				inc += Spoly_npts[Snpoly];
				Snpoly++;
			}
			else
				inc = inc + pp[inc][0] + 1;
		}
		Sinit[CVPT] = UU_TRUE;
/*
.....Convert points through MODSYS
*/
		if (Snpts[CVPT] > 0)
		{
			if (wcsact())
			{
				S_list_ary(CVPT,&pp,&pv,&np);
				for (i=0;i<Snpoly;i++)
				{
					for (j=0;j<Spoly_npts[i];j++)
					{
						ncl_wcstomcs(0,pp[Spoly_start[i]+j],pp[Spoly_start[i]+j]);
						ncl_wcstomcs(1,pv[Spoly_start[i]+j],pv[Spoly_start[i]+j]);
					}
				}
			}
			*npts = Snpts[CVPT];
			*npoly = Snpoly;
		}
	}
/*
.....End of routine
*/
done:				
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_profile_ps (nclkey,ietype,iptype,plane,tatyp,nvec,
**                                     thick,tiltfl,tiltval)
**       Stores the Profile Part surface for further use.
**    PARAMETERS
**       INPUT  :
**          nclkey   = Key of Profile Part surface when a surface is
**                     used.
**          ietype   = 0 = PS not provided, use curve points as generated,
**                     1 = Parallel Plane, 2 = Surface or Canted Plane.
**          iptype   = Top of part type.  1 = # of passes, 2 = Plane,
**                     3 = Distance.
**          plane    = Canonical form of plane.  If the Part surface
**                     is a surface, then its canonical data is
**                     retrieved using the 'nclkey'.
**          tatyp    = Tool axis calculation method.  0 = Fixed,
**                     1 = Normal to input part surface,
**                     2 = Normal to secondary part surface.
**          nvec     = Normal vector for projections.  Typically the
**                     tool axis vector.
**          thick    = Part surface thick.
**          tiltfl   = Apply Tilt Angle to beginning of move, etc
**          tiltval  = Tilted angle parameters array
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_ps(nclkey,ietype,iptype,plane,tatyp,nvec,thick,tiltfl,tiltval)
UM_int4 *nclkey;
UM_int2 *ietype,*tatyp,*iptype,tiltfl[2];
UU_REAL *plane,*nvec,*thick,tiltval[];
{
	int i;
/*
.....Initialize routine
*/
	Sps_tlaxfl = UU_FALSE;
	Sps_type = *ietype;
	Sps_xproj = *tatyp;
	Sps_top = *iptype;
	Sps_thick = *thick;
	Sps_tilt_flag[0] = tiltfl[0];
	Sps_tilt_flag[1] = tiltfl[1];
	Sps_tilt_dis[0] = tiltval[0];
	Sps_tilt_dis[1] = tiltval[1];
	Sps_tilt_ang[0] = tiltval[2];
	Sps_tilt_ang[1] = tiltval[3];
	Sps_depth = 0.;
	Snofs = 0;
	um_vctovc(nvec,Sps_vec);
/*
.....Store plane
*/
	if (Sps_type == 1 || Sps_top == 2)
	{
		um_vctovc(plane,Sps_plane.n);
		um_vctmsc(plane,plane[3],Sps_plane.p0);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_profile_start(pt,scpt,befl,vfl,vdir,nvec,dirfl,csfl,
**                                       ept,tol,iclose,npts)
**       Calculates the starting point of a profile based on a provided
**       point.  The starting point will be the closest point on the
**       curve to the provided point.  The profile points will be
**       re-ordered with the starting point first.
**
**       Also determines the direction for the offset curve.
**    PARAMETERS
**       INPUT  :
**          pt       = Point used to calculate starting point.
**          scpt     = Tool end - to help calculate offset direction.
**			befl	 = 0 = No BEGIN and No END, 1 = BEGIN, 2 = END
**          vfl      = 1 = Machine curve CLW, 2 = CCLW, 3 = Use 'vdir'.
**          vdir     = Direction to machine the curve in.  The curve may
**                     be reversed to honor this direction.
**          nvec     = Normal vector to compare the angle of the vectors
**                     between (typically the tool axis vector).
**          dirfl    = 0 = Calculate offset direction, -1 = Right, 1 = Left.
**          csfl     = 2 = An ending location is specified.
**          ept      = Ending location of curve.  The curve will be trimmed
**                     to this location.
**          tol      = Tolerance to use to determine if curve is closed.
**       OUTPUT :
**          iclose   = Returns 1 if curve is closed, 0 otherwise.
**          npts     = Updated number of points in case one was added.
**          nvec     = Normal vector recalculated as the average curve plane
**                     normal if 'PS,NORMAL' tlaxis
**    RETURNS      : none
**    SIDE EFFECTS : 
**       The starting point may have to be added to the list of profile
**       points if it does not exist within the profile already.
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_start(pt,scpt,befl,vfl,vdir,nvec,dirfl,csfl,ept,tol,iclose,npts)
UU_REAL *pt,*scpt,*tol,*vdir,*nvec,*ept;
UM_int2 *befl,*vfl,*csfl,*dirfl;
int *npts,*iclose;
{
	int i,inc,ix,nc,iw,ih,status,is1,is2;
	UU_LOGICAL iadd;
	UM_coord *pp,*px,ptadd;
	UM_vector *pv,*pxv,vcadd,vcf,vcr;
	UM_int2 ier;
	UU_REAL rdum;
/*
.....Get list pointers
*/
	S_list_ary(CVPT,&pp,&pv,&nc);
/*
.....Annotation
.....Simply return the first point
*/
	if (Stype == ANOTE)
	{
		*iclose = Sclose = UU_FALSE;
		um_vctovc(pv[Spoly_start[0]],nvec);
		*npts = Snpts[CVPT];
		return;
	}
/*
.....Initialize routine
*/
	iadd = UU_FALSE;
	inc = 0;
	px = UU_NULL;
	pxv = UU_NULL;
#if DEBUG == 1
	gtimx(&Stims,&Stimm);
#endif
/*
.....Intersect starting point with curve
*/
	if (*befl == 0)
		iadd = S_isect_prof(CVPT,pt,*tol,&inc,ptadd,vcadd);
	else if (*befl == 1)
	{
		if (*vfl == 1 || *vfl == 3)	
			um_vctovc(pp[0],pt);
		else if (*vfl == 2)
			um_vctovc(pp[Snpts[CVPT]-1],pt);
	}
/*
........Check if curve is closed
*/
	Sclose = UU_FALSE;
	if (um_cceqcc_tol(pp[0],pp[Snpts[CVPT]-1],*tol)) Sclose = UU_TRUE;
/*
.....Calculate forward and reverse
.....directional vectors
*/
	if (!iadd) um_vctovc(pp[inc],ptadd);
	is1 = inc + 1;
	is2 = inc - 1;
	if (Sclose)
	{
		if (is1 >= Snpts[CVPT]) is1 = 0;
		if (is2 < 0) is2 = Snpts[CVPT]-1;
		um_vcmnvc(pp[is1],ptadd,vcf); um_unitvc(vcf,vcf);
		um_vcmnvc(pp[is2],ptadd,vcr); um_unitvc(vcr,vcr);
	}
	else
	{
/*
.....Check if pt is at the begining or end of open profil
*/
		if (*befl == 0 && *vfl == 3)
		{
			if (um_cceqcc_tol(pt,pp[0],*tol))
				*vfl = 1;
			else if (um_cceqcc_tol(pt,pp[Snpts[CVPT]-1],*tol))
				*vfl = 2;
		}

		if (is1 < Snpts[CVPT]) 
		{
			um_vcmnvc(pp[is1],ptadd,vcf); um_unitvc(vcf,vcf);
		}
		else
		{
			vcf[0] = vcf[1] = vcf[2] = 0.;
		}
		if (is2 >= 0)
		{
			um_vcmnvc(pp[is2],ptadd,vcr); um_unitvc(vcr,vcr);
		}
		else
		{
			vcr[0] = vcr[1] = vcr[2] = 0.;
		}
		if (um_mag(vcf) < UM_FUZZ) um_vctmsc(vcr,-1.,vcf);
		if (um_mag(vcr) < UM_FUZZ) um_vctmsc(vcf,-1.,vcr);
	}
/*
.....Reverse the profile if necessary
*/
	if (S_reverse_profile(*vfl,vdir,vcf,vcr,nvec,Sclose))
	{	
		S_list_ary(CVPT,&pp,&pv,&nc);
		iadd = UU_FALSE;
		inc = 0;
		if (*befl == 1 && *vfl == 3)
			um_vctovc(pp[0],pt);
		iadd = S_isect_prof(CVPT,pt,*tol,&inc,ptadd,vcadd);
	}
/*
.....Print out the reversed curve
*/
	S_print_curve(CVPT,"Reversed Curve");
/*
.....Re-order the profile if necessary
*/
	if (inc != 0 || iadd)
	{
		px = (UM_coord *)uu_malloc(sizeof(UM_coord)*(Snpts[CVPT]+4));
		pxv = (UM_vector *)uu_malloc(sizeof(UM_vector)*(Snpts[CVPT]+4));
		if (px == UU_NULL || pxv == UU_NULL)
		{
			*npts = 0;
			goto done;
		}
		ix = 0;
/*
........Add new point to profile
*/
		if (iadd)
		{
			uu_list_push(&Scvpt[CVPT],ptadd);
			uu_list_push(&Scvvc[CVPT],vcadd);
			S_list_ary(CVPT,&pp,&pv,&nc);
			um_vctovc(ptadd,px[0]);
			um_vctovc(vcadd,pxv[0]);
			ix++;
		}
/*
........Copy profile point
*/
		for (i=0;i<Snpts[CVPT];i++)
		{
			if (inc >= Snpts[CVPT])
			{
				if (Sclose)
				{
					um_vctovc(pv[0],pxv[ix-1]);
					inc = 1;
					i++;
					if (i >= Snpts[CVPT]) break;
				}
				else
				{
					Snpts[CVPT] = ix;
					break;
				}
			}
			um_vctovc(pp[inc],px[ix]);
			um_vctovc(pv[inc],pxv[ix]);
			ix++; inc++;
		}
/*
........Add final point if curve is closed
*/
		if (Sclose)
		{
			um_vctovc(px[0],px[ix]);
			um_vcmnvc(px[ix],px[ix-1],pxv[ix]);
			um_unitvc(pxv[ix],pxv[ix]);
			Snpts[CVPT] = ix+1;
		}
/*
........Copy updated profile to global profile
*/
		UU_LIST_EMPTY(&Scvpt[CVPT]);
		UU_LIST_EMPTY(&Scvvc[CVPT]);
		uu_list_push_multiple(&Scvpt[CVPT],Snpts[CVPT],px);
		uu_list_push_multiple(&Scvvc[CVPT],Snpts[CVPT],pxv);
/*
.....Print out the shuffled curve
*/
		S_print_curve(CVPT,"Shuffled Curve");
	}

	if (Sps_xproj == 1 || Sps_xproj == 2)
	{
		S_list_ary(CVPT,&pp,&pv,&nc);
		if (Sclose) nc--;
		if (nc > 2)
		{
			if (px == UU_NULL)
				px = (UM_coord *)uu_malloc(sizeof(UM_coord)*nc);

			if (px != UU_NULL)
			{
				status = -1;
				iw = 1; ih = 2;
				rdum = 0;

				for (i = 0, ier = 0; i < nc; i++)
				{
/*
..... if there is a primary part  surface, use the projected curve points
..... for the average plane calculation, else use the curve points as is.
*/
					if (Sps_type == 2)
					{
						prfps(&iw,&ih,pp[i],nvec,&rdum,px[i],vcadd,&i,&ier);
						if (ier != 0) break;
					}
					else
						um_vctovc (pp[i],px[i]);
				}

				if (ier == 0)
					status = um_ptstopln (nc,px,ptadd,vcadd,Stol);

				if (status == 0)
				{
					if (UM_DOT (nvec,vcadd) < 0.)
						um_vctmsc (vcadd,-1.,nvec);
					else
						um_vctovc (vcadd,nvec);
				}
			}
		}
	}
/*
.....Calculate the offset direction
*/
	if (*dirfl == 0)
	{
		ncl_offset_lr(&Scvpt[CVPT],pt,nvec,*tol,&Sldir);
		if (Sldir == 0)
		{
			if (!um_cceqcc_tol(pt,scpt,*tol))
			ncl_offset_lr(&Scvpt[CVPT],scpt,nvec,*tol,&Sldir);
		}
	}
	else Sldir = *dirfl;
/*
.....Intersect ending point with curve
*/
	if (*csfl == 2)
	{
		iadd = S_isect_prof(CVPT,ept,*tol,&inc,ptadd,vcadd);
		if (inc < Snpts[CVPT]-1)
		{
			if (!iadd) inc++;
			uu_list_delete(&Scvpt[CVPT],inc,Snpts[CVPT]-inc);
			uu_list_delete(&Scvvc[CVPT],inc,Snpts[CVPT]-inc);
			Snpts[CVPT] = inc;
			Sclose = UU_FALSE;
		}
		if (iadd)
		{
			uu_list_push(&Scvpt[CVPT],ptadd);
			uu_list_push(&Scvvc[CVPT],vcadd);
			Sclose = UU_FALSE;
			Snpts[CVPT]++;
		}
/*
.....Print out the intersected curve
*/
		S_print_curve(CVPT,"CS Curve");
	}
/*
.....End of routine
*/
	*npts = Snpts[CVPT];
#if DEBUG == 1
	gtimx(&Stems,&Stemm);
	S_print_time(CVPT);
#endif
done:;
/*
........Free up local memory
*/
	UU_FREE(px);
	UU_FREE(pxv);
/*
.....End of routine
*/
	*iclose = Sclose;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_profile_offset(which,kinc,kncv,dis,nvec,tol,npts)
**       Creates an offset curve based on the evaluated profile curve
**       at a specified distance.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Offset Profile curve.
**                     2 = Offset Projected curve.
**          kinc     = Which offset curve to create (negative value) or
**                     activate (positive value).
**          kncv     = Total number of offset curves to create.
**          dis      = Offset distance.
**          nvec     = Normal vector to compare the angle of the vectors
**                     between (typically the tool axis vector).
**          tol      = Tolerance to evaluate curve at.
**       OUTPUT :
**          npts     = Number of points generated.  0 means error.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_offset(which,kinc,kncv,dis,nvec,tol,npts)
UM_int4 *npts,*which,*kinc,*kncv;
UU_REAL *dis,*tol,*nvec;
{
	int nc,ipt,inc,i,j,np,ishark;
	UU_REAL dtol,offdis,cvlen,d,alp0,rad0;
	UU_REAL um_getpolylen(),um_dcccc();
	UM_coord *pp,pto,pts;
	UM_vector *pv,zvec,pvo;
/*
.....Set an offset curve as active
*/
	inc = *kinc;
	if (inc >= 0)
	{
		Sofinc = inc;
		if (Stype == CURVE) *npts = Snofpts[inc];
		else *npts = Spoly_npts[Sofinc];
	}
/*
.....Initialize routine for
.....creating offset curve
*/
	else
	{
		inc = -inc - 1;
		Sofinc = inc;
		*npts = 0;
		ipt = *which;
		S_list_ary(*which,&pp,&pv,&nc);
/*
.....First time here
.....Allocate memory for offset curves and
.....Calculate length of curve
*/
		if (Sofinc == 0)
		{
			Snofs = *kncv;
			Sofpt = (UU_LIST *)uu_malloc(sizeof(UU_LIST)*Snofs);
			Sofvc = (UU_LIST *)uu_malloc(sizeof(UU_LIST)*Snofs);
			Snofpts = (int *)uu_malloc(sizeof(int)*Snofs);
			for (i=0;i<Snofs;i++)
			{
				uu_list_init(&Sofpt[i],sizeof(UM_coord),nc,25);
				uu_list_init(&Sofvc[i],sizeof(UM_vector),nc,25);
				Snofpts[i] = 0;
			}
			cvlen = um_getpolylen(nc,pp);
			Sptdis = cvlen / (UU_REAL)(nc-1);
		}
/*
.....Copy active curve into offset curve
*/
		uu_list_push_multiple(&Sofpt[Sofinc],nc,pp);
		uu_list_push_multiple(&Sofvc[Sofinc],nc,pv);
/*
.....Offset the curve
*/
#if DEBUG == 1
	gtimx(&Stims,&Stimm);
#endif
		dtol = *tol;
		offdis = *dis;
		if (Sldir == 0) offdis = 0;
		um_vctovc(nvec,zvec);
		ishark = 2;
		alp0 = 120;
		rad0 = 0;
		Snofpts[Sofinc] = ncl_cv_offset(NULLKEY,&Sofpt[Sofinc],&Sofvc[Sofinc],
			Snpts[ipt],Sldir,zvec,offdis,ishark,alp0,rad0,dtol,0,0);
#if DEBUG == 1
	gtimx(&Stems,&Stemm);
	S_print_time(OFPT);
#endif
/*
.....Print out the offset curve
*/
		S_print_curve(OFPT,"Offset Curve");
/*
.....Offsetting the curve can put gaps in the curve
.....Make sure there are enough points to output
.....when projecting the curve to a surface or plane
*/
		S_list_ary(OFPT,&pp,&pv,&nc);
		for (i=1;i<nc;i++)
		{
			d = um_dcccc(pp[i],pp[i-1]);
			if (d > Sptdis*1.2)
			{
				np = (d-.01) / Sptdis;
				d = d / (UU_REAL)(np+1);
				um_vctovc(pp[i-1],pto);
				um_vcmnvc(pp[i],pp[i-1],pvo);
				um_unitvc(pvo,pvo);
				for (j=1;j<=np;j++)
				{
					um_translate_point(pto,d*j,pvo,pts);
					uu_list_insert(&Sofpt[Sofinc],i+j-1,pts);
					uu_list_insert(&Sofvc[Sofinc],i+j-1,pvo);
				}
				i = i + np;
				Snofpts[Sofinc] = Snofpts[Sofinc] + np;
				S_list_ary(OFPT,&pp,&pv,&nc);
			}
		}
		Snofpts[Sofinc] = nc;
		*npts = Snofpts[Sofinc];
/*
.....Print out the offset adjusted curve
*/
		S_print_curve(OFPT,"Offset Adjusted Curve");
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_profile_project (which,dis,tlaxfl,npts,ier)
**       Projects the original profile curve onto a specified Z-level.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Project Profile curve.
**                     1 = Project Offset curve.
**          dis      = Offset distance when part surface type is simply
**                     offset value.
**          tlaxfl   = 1 = Calculate tool axis vectors.
**       OUTPUT :
**          npts     = Number of points generated.  0 means error.
**          ier      = Non-zero if an error occurs projecting curve.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_project(which,dis,tlaxfl,npts,ier)
UM_int4 *which;
UM_int4 *npts;
UM_int4 *tlaxfl;
UM_int2 *ier;
UU_REAL *dis;
{
	int i,nc,nc1,iw,ih,type;
	UM_coord *pp,*pp1,pt1;
	UM_vector *pv,vc1,svec;
	UU_REAL rdis;
	UM_plane mpl,*plpt;
/*
.....Annotation style projection
.....Just store distance for now
.....Points are adjusted in 'ncl_profile_pt'
*/
	*ier = 0;
	if (Stype == ANOTE)
	{
		Sps_tlaxfl = *tlaxfl;
		Sps_depth = *dis;
		goto done;
	}
/*
.....Initialize projection type
*/
	if (Sps_top == 2 &&
			(Sps_type == 0 || um_mag(Sps_plane.n) > UM_FUZZ) &&
			Sps_xproj == 0 && *dis != 0.)
		type = 1;
	else
		type = Sps_type;
#if DEBUG == 1
	gtimx(&Stims,&Stimm);
#endif
/*
.....Initialize projected curve
*/
	*npts = 0;
	S_list_ary(*which,&pp,&pv,&nc);
	if (Sinit[PJPT])
	{
		UU_LIST_EMPTY(&Scvpt[PJPT]);
		UU_LIST_EMPTY(&Scvvc[PJPT]);
	}
	else
	{
		uu_list_init(&Scvpt[PJPT],sizeof(UM_coord),nc+10,25);
		uu_list_init(&Scvvc[PJPT],sizeof(UM_vector),nc+10,25);
		Sinit[PJPT] = UU_TRUE;
	}
	uu_list_push_multiple(&Scvpt[PJPT],nc,pp);
	uu_list_push_multiple(&Scvvc[PJPT],nc,pv);
	S_list_ary(PJPT,&pp1,&pv,&nc1);
/*
.....Curve is projected at a distance
*/
	if (type == 0)
	{
		if (Sps_xproj == 2 && *tlaxfl == 1)
		{
			iw = 2; ih = 2;
			rdis = 0.;
			um_vctovc(Sps_vec,svec);
			for (i=0;i<nc;i++)
			{
				prfps(&iw,&ih,pp1[i],svec,&rdis,pt1,pv[i],&i,ier);
				if (*ier > 0) return;
				um_vctovc(pv[i],svec);
				if (*dis != 0.)
				{
					um_vctmsc(pv[i],*dis,vc1);
					um_vcplvc(pp[i],vc1,pp1[i]);
				}
			}
			Sps_tlaxfl = UU_TRUE;
		}
		else if (*dis != 0.)
		{
			um_vctmsc(Sps_vec,*dis,vc1);
			for (i=0;i<nc;i++) um_vcplvc(pp[i],vc1,pp1[i]);
		}
		Snpts[PJPT] = nc;
	}
/*
.....Curve is projected onto a parallel plane
*/
	else if (type == 1)
	{
		if (*dis == 0)
			plpt = &Sps_plane;
		else
		{
			um_vctovc(Sps_plane.n,mpl.n);
			um_vctmsc(Sps_plane.n,*dis,vc1);
			um_vcplvc(Sps_plane.p0,vc1,mpl.p0);
			plpt = &mpl;
		}
		um_proj_pt_on_plane(nc,pp,plpt,pp1);
/*
........This is a level projection, make sure that
........plane is not under surface
*/
		if (Sps_type == 2)
		{
			iw = 1; ih = 1;
			rdis = Sps_thick;
			for (i=0;i<nc;i++)
			{
				prfps(&iw,&ih,pp1[i],Sps_vec,&rdis,pt1,vc1,&i,ier);
				if (*ier > 0) return;
				if (um_dot(pt1,vc1)-um_dot(pp1[i],vc1) > 0)
					um_vctovc(pt1,pp1[i]);
			}
		}
/*
........And not under 3-D curve
*/
		else if (Sps_type == 0)
		{
			um_vctmsc(Sps_vec,Sps_thick,vc1);
			for (i=0;i<nc;i++)
			{
				um_vcplvc(pp[i],vc1,pt1);
				if (um_dot(pt1,Sps_vec)-um_dot(pp1[i],Sps_vec) > 0)
					um_vctovc(pt1,pp1[i]);
			}
		}
/*
........Project normal to secondary part surface
*/
		if (Sps_xproj == 2 && *tlaxfl == 1)
		{
			iw = 2; ih = 2;
			rdis = 0.;
			um_vctovc(Sps_vec,svec);
			for (i=0;i<nc;i++)
			{
				prfps(&iw,&ih,pp1[i],svec,&rdis,pt1,pv[i],&i,ier);
				if (*ier > 0) return;
				um_vctovc(pv[i],svec);
			}
			Sps_tlaxfl = UU_TRUE;
		}
		Snpts[PJPT] = nc;
	}
/*
.....Curve is projected onto surface or
.....canted plane
*/
	else
	{
/*
........Standard tool projection
........using tool axis
*/
		if (Sps_xproj == 0 || *tlaxfl == 0)
		{
			iw = 1; ih = 1;
			for (i=0;i<nc;i++)
			{
				prfps(&iw,&ih,pp[i],Sps_vec,dis,pp1[i],pv[i],&i,ier);
				if (*ier > 0) return;
			}
		}
/*
........Normal to Part Surface
*/
		else
		{
/*
...........The first projection obtains the normal vector
...........and occurs exactly on the defined surface
...........so that the various levels match up
*/
			iw = Sps_xproj; ih = 2;
			rdis = 0.;
			um_vctovc(Sps_vec,svec);
			for (i=0;i<nc;i++)
			{
				prfps(&iw,&ih,pp[i],svec,&rdis,pt1,pv[i],&i,ier);
				if (*ier > 0) return;
				um_vctovc(pv[i],svec);
			}
			iw = 1; ih = 1;
/*
............Now applied the ATANGL tilt, Calculate tilt angles at
............each point along the profile
*/	
			if (Sps_tilt_flag[0] == 3)
			{
				S_calc_tilt_angles(PJPT,&nc);
				S_list_ary(PJPT,&pp,&pv,&i);
				//Sps_tlaxfl = UU_TRUE;
			}
/*
...........Now, project the tool onto the (offset) surface
...........using the normal tool axis or modified ATANGLE Tool
...........axis obtained above
*/
			for (i=0;i<nc;i++)
			{
				prfps(&iw,&ih,pp[i],pv[i],dis,pp1[i],vc1,&i,ier);
				if (*ier > 0) return;
/*
.....Reproject the adjusted profile end point to surface to obtain 
.....the Normal vector
*/
					if (i==nc-1 && Sps_tilt_flag[0] == 3)
						{
							ih = 2;
							prfps(&iw,&ih,pp1[i],svec,dis,S_pp1[1],
							S_pv[1],&i,ier);
							ih = 1;
						}
			}
/*
...........If Tilt angles are applied to start and end of profile
...........Calculate tilt angles at each point along the profile
*/
		    if (Sps_tilt_flag[0] == 1 || Sps_tilt_flag[0] == 2 || Sps_tilt_flag[1] != 0)
		    {
			    S_calc_tilt_angles(PJPT,&nc);
			    S_list_ary(PJPT,&pp,&pv,&i);
			    //Sps_tlaxfl = UU_TRUE;	
/*
...........Now, project the tool onto the (offset) surface
...........using the tilted tool axis obtained above
*/
			    for (i=0;i<nc;i++)
			    {
				    prfps(&iw,&ih,pp[i],pv[i],dis,pp1[i],vc1,&i,ier);
				    if (*ier > 0) return;
/*
.....Reproject the adjusted profile start point to surface to obtain 
.....the Normal vector
*/
					if (i==0)
						{
							ih = 2;
							prfps(&iw,&ih,pp1[i],svec,dis,S_pp1[0],
							S_pv[0],&i,ier);
							ih = 1;
						}
/*
.....Reproject the adjusted profile end point to surface to obtain 
.....the Normal vector
*/
					if (i==nc-1)
						{
							ih = 2;
							prfps(&iw,&ih,pp1[i],svec,dis,S_pp1[1],
							S_pv[1],&i,ier);
							ih = 1;
						}
			    }
		    }

			Sps_tlaxfl = UU_TRUE;
	    }
		Snpts[PJPT] = nc;
	}
	*npts = Snpts[PJPT];
/*
.....Print out the projected curve
*/
#if DEBUG == 1
	gtimx(&Stems,&Stemm);
	S_print_time(PJPT);
#endif
	S_print_curve(PJPT,"Projected Curve");
done:;
	return;
}


/*********************************************************************
**    E_FUNCTION     : ncl_profile_weed (which,overlap,tol,maxdpf,maxdp,npts)
**       Weeds a profile curve, removing any points in a straight line.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Weed Profile curve.
**                     1 = Weed Offset curve.
**                     2 = Weed Projected curve.
**          overlap  = Overlap distances. 0 = Entry, 1 = Exit.
**          tol      = Tolerance to use in weeding procedure.
**          maxdpf   = 1 = Output points at maximum distance
**                   = 2 = Don't
**          maxdp    = Maximum step value when maxdpf = 1
**       OUTPUT :
**          npts     = Number of points generated.  0 means error.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_weed(which,overlap,tol,maxdpf,maxdp,npts)
UM_int4 *which;
UM_int4 *npts;
UM_int2 *maxdpf;
UU_REAL overlap[],*tol,*maxdp;
{
	int i,nc,nent,iw,np,icm;
	UM_int2 ids,ient,itype;
	UM_int4 twhich;
	UU_REAL dis[6],pts[6],ptpd[6];
	UM_coord *pp,pte,*pp1;
	UM_vector *pv,pvs,pve,*pv1;
	UU_LIST *lspt,*lsvc,mypt,myvc;
/*
.....Get pointers to work curve
*/
	*npts = 0;
	S_list_ptr(*which,&lspt,&lsvc,&nc);
	S_list_ary(*which,&pp,&pv,&nc);
#if DEBUG == 1
	gtimx(&Stims,&Stimm);
#endif
/*
.....Initialize tool axis vectors
*/
	if (*which != PJPT)
	{
		for (i=0;i<nc;i++) um_vctovc(Sps_vec,pv[i]);
	}
/*
.....Initialize weeded curve
*/
	if (nc > 0)
	{
		if (Sinit[WDPT])
		{
			UU_LIST_EMPTY(&Scvpt[WDPT]);
			UU_LIST_EMPTY(&Scvvc[WDPT]);
		}
		else
		{
			uu_list_init(&Scvpt[WDPT],sizeof(UM_coord),nc,25);
			uu_list_init(&Scvvc[WDPT],sizeof(UM_vector),nc,25);
			Sinit[WDPT] = UU_TRUE;
		}
/*
.....Weed out unnecessary points
*/
		um_vctovc(pp[0],pts); um_vctovc(pv[0],pvs);
		um_vctovc(pp[nc-1],pte); um_vctovc(pv[nc-1],pve);
		Snpts[WDPT] = ncl_mover_weed(nc,lspt,lsvc,&Scvpt[WDPT],&Scvvc[WDPT],*tol);
/*
.....Make sure first & last points weren't deleted
*/
/*
		if (Snpts[WDPT] > 0)
		{
			S_list_ary(WDPT,&pp,&pv,&nc);
			if (!um_cceqcc(pp[0],pts))
			{
				uu_list_insert(&Scvpt[WDPT],0,pts);
				uu_list_insert(&Scvvc[WDPT],0,pvs);
				Snpts[WDPT]++;
			}
			if (!um_cceqcc(pp[Snpts[WDPT]-1],pte))
			{
				uu_list_push(&Scvpt[WDPT],pte);
				uu_list_push(&Scvvc[WDPT],pve);
				Snpts[WDPT]++;
			}
		}
*/

/*
.....Add points back to curve if maxdpf = 1
*/
		if (*maxdpf == 1)
		{
			twhich = WDPT;
			S_profile_maxdp(&twhich,maxdp,&Snpts[WDPT]);
		}
/*
.....Add overlap distances
*/
		if (Snpts[WDPT] > 0)
		{
			iw = WDPT;
			ient = 2;
			ids = 1;
			icm = 0;
			nent = 0;
			dis[3] = 0.;
			dis[4] = 0.;
			dis[5] = 0.;
			if (overlap[0] != 0.)
			{
				itype = 1;
				dis[2] = overlap[0];
				ncl_profile_entry(&iw,&itype,&ient,&ids,&icm,dis,ptpd,pts,&nent);
				if (nent != 0)
				{
					uu_list_init(&mypt,sizeof(UM_vector),nent,25);
					uu_list_init(&myvc,sizeof(UM_vector),nent,25);
					uu_list_push_list(&mypt,&Scvpt[ENPT]);
					uu_list_push_list(&myvc,&Scvvc[ENPT]);
				}
			}
			if (overlap[1] != 0.)
			{
				itype = 2;
				dis[2] = overlap[1];
				ncl_profile_entry(&iw,&itype,&ient,&ids,&icm,dis,ptpd,pts,&np);
				if (np != 0)
				{
					if (overlap[1] > 0.)
					{
						uu_list_push_list(&Scvpt[WDPT],&Scvpt[ENPT]);
						uu_list_push_list(&Scvvc[WDPT],&Scvvc[ENPT]);
						Snpts[WDPT] += np;
					}
					else
					{
						uu_list_pop_list(&Scvpt[WDPT],np-1,&Scvpt[ENPT]);
						uu_list_pop_list(&Scvvc[WDPT],np-1,&Scvvc[ENPT]);
						pp1 = (UM_coord *)UU_LIST_ARRAY(&Scvpt[ENPT]);
						pv1 = (UM_vector *)UU_LIST_ARRAY(&Scvvc[ENPT]);
						uu_list_push(&Scvpt[WDPT],&pp1[0]);
						uu_list_push(&Scvvc[WDPT],&pv1[0]);
						Snpts[WDPT] -= np-2;
					}
				}
			}
			if (nent != 0)
			{
				if (overlap[0] > 0.)
				{
					uu_list_insert_list(&Scvpt[WDPT],0,&mypt);
					uu_list_insert_list(&Scvvc[WDPT],0,&myvc);
					Snpts[WDPT] += nent;
				}
				else
				{				
					uu_list_delete(&Scvpt[WDPT],0,nent-1);
					uu_list_delete(&Scvvc[WDPT],0,nent-1);
					pp1 = (UM_coord *)UU_LIST_ARRAY(&mypt);
					pv1 = (UM_vector *)UU_LIST_ARRAY(&myvc);
					uu_list_insert(&Scvpt[WDPT],0,&pp1[nent-1]);
					uu_list_insert(&Scvvc[WDPT],0,&pv1[nent-1]);
					Snpts[WDPT] -= nent-2;
				}				
				uu_list_free(&mypt);
				uu_list_free(&myvc);
			}
		}
/*
.....Print out the weeded curve
*/
		*npts = Snpts[WDPT];
		S_print_curve(WDPT,"Weeded Curve");
	}
#if DEBUG == 1
	gtimx(&Stems,&Stemm);
	S_print_time(WDPT);
#endif
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_profile_entry (which,ktype,kent,kds,kcm,
**                                        entval,pdpt,pt,npts)
**       Calculates the entry/exit points based on the entry/exit method.
**       Also returns the starting point of entry or ending point of exit.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Use the Profile curve.
**                     1 = Use the Offset curve.
**                     2 = Use the Projected curve.
**                     3 = Use the Weeded curve.
**          ktype    = 1 = Entry move, 2 = Exit move.
**          kent     = Entry method.  0 = Omit, 1 = Arc, 2 = Ramp, 3 = Comb
**          kds      = Drive surface condition.  1 = ON, 2 = OFF (to).
**			kcm		 = CUTCOM option. 1 = ON, 0= OFF
**          entval   = (0) = Arc radius,  (1) = Arc Rise distance,
**                     (2) = Ramp distance, (3) = Ramp angle,
**                     (4) = Ramp Rise distance
**					   (5) = CUTCOM Positional Distance.
**       OUTPUT :
**          pdpt     = positional distance start point
**          pt       = First point of entry.
**          npts     = Number of points created for entry.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_entry(which,ktype,kent,kds,kcm,entval,pdpt,pt,npts)
UM_int4 *which,*npts;
UM_int2 *kcm,*kds,*kent,*ktype;
UU_REAL *entval,*pt,*pdpt;
{
	int ist,ist2,nwd,np,nent,inc,ids,itype,ient,icm;
	UU_REAL pp[6],pp1[6],ang;
	UM_vector vc1,vc2,vc3,vc4,stan;
	UM_transf mtf;
/*
.....Initialize routine
*/
	ids = *kds;
	itype = *ktype;
	ient = *kent;
	icm = *kcm;
	ist = 1;
	if (itype == 2) ist = Snpts[*which];
	np = 1;
	nwd = 6;
	ncl_profile_pt(which,&ist,&nwd,&np,pp);
/*
.....Check comb entry
*/
	if (ient == 3 && fabs(entval[3]) < 0.1)
		ient = 2;
/*
.....Initialize the entry point list
*/
	if (Sinit[ENPT])
	{
		UU_LIST_EMPTY(&Scvpt[ENPT]);
		UU_LIST_EMPTY(&Scvvc[ENPT]);
	}
	else
	{
		uu_list_init(&Scvpt[ENPT],sizeof(UM_coord),25,25);
		uu_list_init(&Scvvc[ENPT],sizeof(UM_vector),25,25);
		Sinit[ENPT] = UU_TRUE;
	}
	Snpts[ENPT] = 0;
/*
.....Arc entry
*/
	if (ient == 1)
	{
		S_pts_at_arc(which,ENPT,itype,entval,ids,&nent,stan);
		inc = ENPT;
		ist = 1;
		if (itype == 2) ist = Snpts[ENPT];
		ncl_profile_pt(&inc,&ist,&nwd,&np,pt);

		if (itype == 1 && entval[5] > 0.0)
			um_vctovc(stan,vc2);
	}
/*
.....Ramp entry
*/
	else if (ient == 2)
	{
/*
.....TLON condition with closed curve
.....So follow the end of the curve to start
*/
		if (entval[2] > 0.)
		{
			if (ids == 1 && Sclose)
			{
				S_pts_at_dis(which,ENPT,itype,entval,&nent);
				inc = ENPT;
				ist = 1;
				if (itype == 2) ist = Snpts[ENPT];
				ncl_profile_pt(&inc,&ist,&nwd,&np,pt);
			}
/*
.....Enter off of curve
*/
			else
			{
				if (itype == 1) ist2 = 2;
				else ist2 = Snpts[*which] - 1;
				ncl_profile_pt(which,&ist2,&nwd,&np,pp1);
				if (ids == 1) ang = 0.;
				else
				{
					if (itype == 1)
						ang = entval[3] / UM_RADIAN * (0-Sldir);
					else
						ang = entval[3] / UM_RADIAN * Sldir;
				}
				um_rotlntf(pp,&pp[3],ang,mtf);
				um_vcmnvc(pp,pp1,vc1);
				um_unitvc(vc1,vc1);
				um_vctmtf(vc1,mtf,vc2);
				um_vctmsc(vc2,entval[2],vc1);
				um_vcplvc(pp,vc1,pt);
				um_vctmsc(&pp[3],entval[4],vc3);
				um_vcplvc(pt,vc3,pt);
				um_vctovc(&pp[3],&pt[3]);
				uu_list_push(&Scvpt[ENPT],pt);
				uu_list_push(&Scvvc[ENPT],&pt[3]);
				Snpts[ENPT]++;
			}
		}
		else
		{
/*
.....Negative overlap
*/
			S_pts_at_dis(which,ENPT,itype,entval,&nent);
			inc = ENPT;
			ist = 1;
			if (itype == 2) ist = Snpts[ENPT];
			ncl_profile_pt(&inc,&ist,&nwd,&np,pt);
		}
	}
/*
.....Comb entry
*/
	else if (ient == 3)
	{
/*
.....Arc entry
*/
		S_pts_at_arc(which,ENPT,itype,entval,ids,&nent,stan);
		inc = ENPT;
		ist = 1;
		if (itype == 2) ist = Snpts[ENPT];
		ncl_profile_pt(&inc,&ist,&nwd,&np,pp);
/*
.....ramp entry
*/
		um_vctovc(stan,vc2);
		um_vctmsc(vc2,entval[2],vc1);
		um_vcplvc(pp,vc1,pt);
		um_vctmsc(&pp[3],entval[4],vc3);
		um_vcplvc(pt,vc3,pt);
		um_vctovc(&pp[3],&pt[3]);
		if (itype == 1)	
		{
			uu_list_insert(&Scvpt[ENPT],0,pt);
			uu_list_insert(&Scvvc[ENPT],0,&pt[3]);
		}
		else
		{
			uu_list_push(&Scvpt[ENPT],pt);
			uu_list_push(&Scvvc[ENPT],&pt[3]);
		}
		Snpts[ENPT]++;
	}
/*
.....Entry omitted
*/
	else
	{
		um_vctovc(pp,pt);
		um_vctovc(&pp[3],&pt[3]);
		if (itype == 1 && entval[5] > 0.0)
		{
			ist2 = 2;
			ncl_profile_pt(which,&ist2,&nwd,&np,pp1);			
			um_vcmnvc(pp,pp1,vc2);
		}
	}

/*
.....Offset positional distance to calculate positional start point
*/
	if (itype == 1 && icm == 1 && entval[5] > 0.0)
	{
		ang = 90.0 / UM_RADIAN * (0-Sldir);
		um_rotlntf(pt,&pt[3],ang,mtf);
		um_unitvc(vc2,vc2);
		um_vctmtf(vc2,mtf,vc3);
		um_vctmsc(vc3,entval[5],vc4);
		um_vcplvc(pt,vc4,pdpt);
		um_vctovc(&pt[3],&pdpt[3]);
	}
	else
	{
		um_vctovc(&pt[0],&pdpt[0]);
		um_vctovc(&pt[3],&pdpt[3]);
	}

/*
.....End of routine
*/
	*npts = Snpts[ENPT];
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_profile_circle (which,kinc,kcirc,gcir,kst,
**                                         ken,gtol)
**       Calculates any circular arcs on the profile.  The profile must
**       be planar with no tool axis changes.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Use the Profile curve.
**                     1 = Use the Offset curve.
**                     2 = Use the Projected curve.
**                     3 = Use the Weeded curve.
**          kinc     = Starting point on profile to start checking for
**                     circular arcs.
**          kcirc    = 0 = Calculate all circular arcs.  Positive = Return
**                     next circular arc record.
**       OUTPUT :
**          kcirc    = Returns 1 when a circular record is returned.
**          gcir     = XYZIJKR circular record.
**          kst      = Starting point of circular record.
**          ken      = Ending point of circular record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_circle (which,kinc,kcirc,gcir,kst,ken,gtol)
UM_int4 *which,*kinc,*kcirc,*kst,*ken;
UU_REAL *gcir,*gtol;
{
	int npt,nc,i,ist;
	UU_LOGICAL planar;
	UU_REAL rnum,um_angle(),tol,rmax;
	UM_coord *pp,pt;
	UM_2Dcoord *pt2x;
	UM_vector *pv,xaxis,yaxis;
	UM_transf tfmat;
	NCL_w_arc *head0;
	static UU_REAL Splane[4];
	static UM_coord spx = {0.,0.,0.};
	static UM_vector sxax = {1.,0.,0.}, syax = {0.,1.,0.}, szax = {0.,0.,1.};

/*
.....First time here
.....Initialize routine
*/
	if (*kcirc == 0)
	{
		S_list_ary(*which,&pp,&pv,&nc);
		ist = *kinc;
		tol = *gtol;
		*kcirc = 0;
		*kst = 0;
		*ken = 0;
/*
........Determine if profile is planar
*/
		npt = nc - ist;
		planar = um_planar_curve(&pp[ist],npt,Splane,xaxis,yaxis);
/*
........Make sure tool axis matches plane
*/
		if (planar)
		{
			for (i=ist;i<nc;i++)
			{
				rnum = um_angle(Splane,pv[i]);
				if (rnum > .001 && rnum < UM_HALFPI-UM_FUZZ)
				{
					planar = UU_FALSE;
					break;
				}
			}
/*
........Rotate curve plane to XY-plane
*/
			if (planar)
			{
				um_chgcstf(pp[ist],xaxis,yaxis,Splane,spx,sxax,syax,szax,tfmat);
				pt2x = (UM_2Dcoord *)uu_malloc(sizeof(UM_2Dcoord)*nc);
				for (i=0;i<nc;i++)
				{
					um_cctmtf(&pp[i],tfmat,pt);
					pt2x[i][0] = pt[0]; pt2x[i][1] = pt[1];
				}

/*
........Calculate arcs in profile
*/
				rmax = 2500.;
				ncl_find_arcs(pt2x,npt,ist,nc-1,0,rmax,tol,2);
/*
........Free the allocated points memory
*/
				uu_free(pt2x);
/*
........Arcs have been defined
*/
				ncl_get_whead (0,&head0);
				if (head0 != UU_NULL)
				{
					um_inverttf(tfmat,Stfmat);
					Scirc = head0;
					*kcirc = 1;
				}
			}
		}
	}
/*
.....nth time here
.....See if there is another circle
*/
	else if (Scirc != UU_NULL)
	{
		Scirc = Scirc->next;
		if (Scirc == UU_NULL)
		{
			*kcirc = 0;
			ncl_free_circ(0);
		}
	}
/*
.....Return the circle data
*/
	if (*kcirc == 1)
	{
		pt[0] = Scirc->center[0]; pt[1] = Scirc->center[1]; pt[2] = 0.;
		um_cctmtf(pt,Stfmat,&gcir[0]);
		um_vctovc(Splane,&gcir[3]);
		gcir[6] = Scirc->rad;
		*kst = Scirc->j0;
		*ken = Scirc->j1;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_profile_pt (which,ist,ksz,npts,pts)
**       Returns point(s) from either the original Profile curve,
**       the Offset Curve, or the Projected Curve.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Return points from the Profile curve.
**                     1 = Return points from the Offset curve.
**                     2 = Return points from the Projected curve.
**                     3 = Return points from the Weeded curve.
**          ist      = Starting point to return (beginning at 1).
**          ksz      = 3 = Transfer point only, 6 = Transfer point and
**                     tool axis.
**          npts     = Number of points to return.
**       OUTPUT :
**          pts      = Output points.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_pt(which,ist,ksz,npts,pts)
UM_int4 *which,*ist,*npts,*ksz;
UU_REAL *pts;
{
	int nc,i,j,k,inc,ipt;
	UM_coord *pp;
	UM_vector *pv;
/*
.....Initialize routine
*/
	S_list_ary(*which,&pp,&pv,&nc);
	inc = *npts;
	if (*ist+*npts-1 > nc) inc = nc - *ist + 1;
/*
.....Transfer points
*/
	ipt = 0;
	j = *ist - 1;
	k = j + inc; 
	for (i=j;i<k;i++)
	{
/*
........Copy point
*/
		if (Stype == ANOTE && Sps_depth != 0. && *which >= PJPT)
			um_translate_point(pp[i],Sps_depth,pv[i],&pts[ipt]);
		else
			um_vctovc(pp[i],&pts[ipt]);
/*
........Copy vector
*/
		if (*ksz == 6)
		{
			if ((*which == PJPT || *which == WDPT || *which == ENPT ||
				(*which == OFPT && Stype == ANOTE)) && Sps_tlaxfl)
				um_vctovc(pv[i],&pts[ipt+3]);
			else um_vctovc(Sps_vec,&pts[ipt+3]);
		}
		ipt += *ksz;
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_profile_free ()
**       Frees the memory allocated by the Profile routines.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_profile_free()
{
	int i;
/*
.....Free the lists
*/
	for (i=NCVS-1;i>=0;i--)
	{
		if (Sinit[i])
		{
			uu_list_free(&Scvvc[i]);
			uu_list_free(&Scvpt[i]);
		}
		Sinit[i] = UU_FALSE;
	}
	S_free_offset();
	if (Spoly_start != UU_NULL) uu_free(Spoly_start);
	if (Spoly_npts != UU_NULL) uu_free(Spoly_npts);
	Spoly_start = Spoly_npts = UU_NULL;
#if DEBUG == 1
	S_print_time(NCVS);
#endif
}


/*********************************************************************
**    E_FUNCTION     : S_free_offset()
**       Frees the memory allocated for offset curves.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_free_offset()
{
	int i;
/*
.....Free the lists
*/
	if (Stype != ANOTE)
	{
		for (i=0;i<Snofs;i++)
		{
			uu_list_free(&Sofvc[i]);
			uu_list_free(&Sofpt[i]);
		}
		if (Snofs > 0)
		{
			uu_free(Sofvc);
			uu_free(Sofpt);
			uu_free(Snofpts);
			Snofs = 0;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : S_list_ptr (which,lspt,lsvc,nc)
**       Returns the requested Profile point and vector list pointers.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Return Profile curve pointers.
**                     1 = Return Offset curve pointers.
**                     2 = Return Projected curve pointers.
**                     3 = Return Weeded curve pointers.
**       OUTPUT :
**          pp       = Pointer to points array.
**          pv       = Pointer to vector array.
**          nc       = Size of array.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_list_ptr(which,lspt,lsvc,nc)
int which;
UU_LIST **lspt,**lsvc;
int *nc;
{
/*
.....Annotation curve
*/
	if (Stype == ANOTE)
	{
		*lspt = &Scvpt[CVPT];
		*lsvc = &Scvvc[CVPT];
		if (which == CVPT) *nc = Snpts[which];
		else *nc = Spoly_npts[Sofinc];
	}
/*
.....Return Offset curve
*/
	else if (which == OFPT)
	{
		*lspt = &Sofpt[Sofinc];
		*lsvc = &Sofvc[Sofinc];
		*nc = Snofpts[Sofinc];
	}
/*
.....Return other curve list pointers and list size
*/
	else
	{
		*lspt = &Scvpt[which];
		*lsvc = &Scvvc[which];
		*nc = Snpts[which];
	}
}

/*********************************************************************
**    E_FUNCTION     : S_list_ary (which,pp,pv,nc)
**       Returns the requested Profile point and vector array pointers
**       and array size.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Return Profile curve pointers.
**                     1 = Return Offset curve pointers.
**                     2 = Return Projected curve pointers.
**                     3 = Return Weeded curve pointers.
**       OUTPUT :
**          pp       = Pointer to points array.
**          pv       = Pointer to vector array.
**          nc       = Size of array.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_list_ary(which,pp,pv,nc)
int which;
UM_coord **pp;
UM_vector **pv;
int *nc;
{
	UU_LIST *lspt,*lsvc;
/*
.....Get list pointers
*/
	S_list_ptr(which,&lspt,&lsvc,nc);
/*
.....Return array pointers
*/
	*pp = (UM_coord *)UU_LIST_ARRAY(lspt);
	*pv = (UM_vector *)UU_LIST_ARRAY(lsvc);
/*
.....Return correct polyline
.....for Annotation
*/
	if (Stype == ANOTE && which != CVPT)
	{
		*pp += Spoly_start[Sofinc];
		*pv += Spoly_start[Sofinc];
		Snpts[which] = *nc;
	}
}

/*********************************************************************
**    E_FUNCTION     : S_isect_prof(which,pt,tol,kinc,ptadd,vcadd)
**       Calculates the nearest point on a curve as compared to a 
**       provided point.  It is possible that an intersection point on
**       the curve is returned if the closest point is on a segment
**       rather that at a point on the curve.
**    PARAMETERS
**       INPUT  :
**          which    = Which curve to analyze.
**          pt       = Point to intersect with curve.
**          tol      = Tolerance for checking intersections.
**       OUTPUT :
**          kinc     = Position in curve array where intersection occurs.
**          ptadd    = Added intersection point if required.
**          vcadd    = Added intersection vector if required.
**    RETURNS      : UU_TRUE if an intersection point was added,
**                   UU_FALSE a curve point is the closest point.
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_isect_prof(which,pt,tol,kinc,ptadd,vcadd)
int which,*kinc;
UU_REAL tol;
UM_coord ptadd,pt;
UM_vector vcadd;
{
	int i,inc,status,nc;
	UU_LOGICAL iadd;
	UU_REAL dln,dis,dis1,cdis,dd;
	UM_coord *pp,ptx;
	UM_vector vln,vln1,*pv;
/*
.....Initialize routine
*/
	S_list_ary(which,&pp,&pv,&nc);
	cdis = 100000.;
	iadd = UU_FALSE;
	inc = 0;
/*
.....Loop through curve points
*/
	for (i=0;i<Snpts[which]-1;i++)
	{
/*
........Build a line between two points on curve
*/
		um_vcmnvc(pp[i+1],pp[i],vln);
		dln = um_mag(vln);
		um_unitvc(vln,vln1);
/*
........Intersect the point and line
*/
		status = um_nptsg(pt,pp[i],vln1,dln,ptx,&dis1);
		dis = um_dcccc(pt,ptx);
		if (status != 1) dis = um_dcccc(pt,pp[i]);
/*
........Is this the closest distance
*/
		dd = dis - cdis;
		if (dd < 0. || (dd < tol && status == 1 && !iadd))
		{
			cdis = dis;
/*
...........Intersection is prior to segment
*/
			if (status == 0)
			{
				inc = i;
				iadd = UU_FALSE;
			}
/*
...........Intersection is in segment
*/
			else if (status == 1)
			{
				dis = um_dcccc(ptx,pp[i]);
				dis1 = um_dcccc(ptx,pp[i+1]);
				if (dis<dis1 && dis<=tol)
				{
					inc = i;
					iadd = UU_FALSE;
				}
				else if (dis1 <= tol)
				{
					inc = i + 1;
					iadd = UU_FALSE;
				}
				else
				{
					inc = i + 1;
					iadd = UU_TRUE;
					um_vctovc(ptx,ptadd);
					um_vctovc(vln1,vcadd);
				}
			}
/*
...........Intersection is past segment
*/
			else if (status == 2)
			{
				inc = i + 1;
				iadd = UU_FALSE;
			}
		}
	}
/*
.....End of routine
*/
	*kinc = inc;
	return(iadd);
}

/*********************************************************************
**    E_FUNCTION     : S_reverse_profile (vfl,vdir,vf,vr,nvec,iclose)
**       Determines the required direction of the original curve based
**       on the direction of the tangent vector of the starting location 
**       as compared to a provided direction vector.
**    PARAMETERS
**       INPUT  :
**          vfl      = 1 = Machine curve CLW, 2 = CCLW, 3 = Use 'vdir'.
**          vdir     = Direction to machine the curve in.  The curve may
**                     be reversed to honor this direction.
**          vf       = Curve tangent vector in curve direction.
**          vr       = Curve tangent vector opposite to curve direction.
**          nvec     = Normal vector to compare the angle of the vectors
**                     between.
**          iclose   = UU_TRUE means curve is closed.
**       OUTPUT :
**          none
**    RETURNS      : UU_TRUE if curve was reversed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL S_reverse_profile(vfl,vdir,vf,vr,nvec,iclose)
int vfl;
UM_vector vdir,vf,vr,nvec;
int iclose;
{
	UU_LOGICAL lrev = UU_FALSE;
	int idir;
	UU_REAL ang1,ang2,um_angle2p_acc();
	UM_coord *pp;
	UM_real8 tol8;
	UM_int4 npt4;
	UM_int2 lrv2;
/*
.....Get direction of curve
*/
	if (vfl != 3)
	{
		if (iclose)
		{
			pp = (UM_coord *)UU_LIST_ARRAY(&Scvpt[CVPT]);
			idir = ncl_polygon_orientation(pp,Snpts[CVPT],nvec,Stol);
			if (idir == -1 && vfl == 2) lrev = UU_TRUE;
			else if (idir == 1 && vfl == 1) lrev = UU_TRUE;
		}
		else
		{
			if (vfl == 2) lrev = UU_TRUE;
		}
	}
/*
.....Determine the angle between the vectors
*/
	else
	{
		ang1 = um_angle2p_acc(vdir,vf,nvec);
		ang2 = um_angle2p_acc(vdir,vr,nvec);
		if (ang1 > UM_PI) ang1 = UM_TWOPI - ang1;
		if (ang2 > UM_PI) ang2 = UM_TWOPI - ang2;
		if (ang1 > ang2) lrev = UU_TRUE;
	}
/*
.....The curve needs to be reversed
*/
	if (lrev)
	{
		lrv2 = 1;
		tol8 = Stol;
		ncl_profile_curve(&tol8,&npt4,&lrv2);
	}
	return(lrev);
}

/*********************************************************************
**    I_FUNCTION     : S_profile_maxdp (which,maxdp,npts)
**       Adds points to weeded curve when maxdpf = 1.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Weed Profile curve.
**                     1 = Weed Offset curve.
**                     2 = Weed Projected curve.
**          maxdp    = Maximum step value when maxdpf = 1
**       OUTPUT :
**          npts     = Number of points generated.  0 means error.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_profile_maxdp(which,maxdp,npts)
UM_int4 *which;
UM_int4 *npts;
UU_REAL *maxdp;
{
	int i,nc,listpt,maxpts,numpts;
	UU_REAL ppdist,vvdist,subdist,vsubdist;
	UM_coord *pp,*pp1;
	UM_vector *pv,vc1;
/*
.....Get pointers to work curve
*/
	S_list_ary(*which,&pp,&pv,&nc);
	*npts = 0;
/*
.....Add points to curve
*/
	maxpts = 0;
	listpt = 0;
	while (listpt < nc - 1) 
	{
		ppdist = um_dcccc(pp[listpt],pp[listpt+1]);
		vvdist = um_dcccc(pv[listpt],pv[listpt+1]);
		if (*maxdp < ppdist)
		{
			numpts = ceil(ppdist/ *maxdp);
			if (numpts > maxpts)
			{
				if (maxpts != 0) uu_free(pp1);
				pp1 = (UM_coord *)uu_malloc(sizeof(UM_coord)*numpts);
				maxpts = numpts;
			}
			subdist = ppdist/(numpts+1);
			vsubdist = vvdist/(numpts+1);
			um_vcmnvc(pp[listpt+1],pp[listpt],vc1);
			um_unitvc(vc1,vc1);
			for (i=0;i<numpts;i++)
			{
				um_translate_point(pp[listpt],(i+1)*subdist,vc1,pp1[i]);
			}
			uu_list_insert_multiple(&Scvpt[*which],listpt+1,numpts,pp1);
			
			um_vcmnvc(pv[listpt+1],pv[listpt],vc1);
			um_unitvc(vc1,vc1);

			for (i=0;i<numpts;i++)
			{
				um_translate_point(pv[listpt],(i+1)*vsubdist,vc1,pp1[i]);
			}
			uu_list_insert_multiple(&Scvvc[*which],listpt+1,numpts,pp1);
		
			S_list_ary(*which,&pp,&pv,&nc);
			nc = UU_LIST_LENGTH(&Scvpt[*which]);
			listpt += numpts + 1;
		}
		else
			listpt++;
	}
/*
.....End of routine
.....Free memory
*/
	*npts = UU_LIST_LENGTH(&Scvpt[*which]);
	if (maxpts != 0) uu_free(pp1); 
	return;
}

/*********************************************************************
**    E_FUNCTION     : S_pts_at_dis (which,wto,ktype,dis,npts)
**       Returns points within a distance of the end point along a curve.
**       A point may be added that is not on the curve to exactly match the
**       distance.  The end point will not be returned in the list.
**    PARAMETERS
**       INPUT  :
**          which    = Which curve list to use.
**          wto      = Which curve list to store the points in.
**          ktype    = 1 = Entry move, 2 = Exit move.
**          dis      = (2) = Ramp distance, (3) = Ramp angle,
**                     (4) = Ramp Rise distance.
**       OUTPUT :
**          npts     = Number of points generated.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_pts_at_dis(which,wto,ktype,dis,npts)
int *which,wto,*npts,ktype;
UU_REAL dis[];
{
	int i,j,ist,nwd,np,nc;
	UU_LOGICAL iadd;
	UU_REAL pp1[6],tdis,rdis,mdis;
	UM_coord pt,*pp,ptadd;
	UM_vector vc1,*pv;
/*
.....Initialize routine
*/
	S_list_ary(*which,&pp,&pv,&nc);
	nwd = 6;
	np = 1;
/*
.....Entry move or exit move negative exit
*/
	if ((ktype == 1 && dis[2] > 0.) || (ktype == 2 && dis[2] < 0.))
	{
		if (ktype == 1)	
			ist = 1;
		else if (ktype == 2)
			ist = nc;
		ncl_profile_pt(which,&ist,&nwd,&np,pp1);
/*
........Loop through curve points
*/
		tdis = 0.;
		for (i=nc-2;i>=0;i--)
		{
			rdis = um_dcccc(pp[i],pp[i+1]);
			mdis = tdis;
			tdis += rdis;
			rdis = fabs(tdis-fabs(dis[2]));
			if (rdis <= .001)
			{
				i--;
				break;
			}
			else if (tdis > fabs(dis[2]))
			{
				um_vcmnvc(pp[i+1],pp[i],vc1);
				um_unitvc(vc1,vc1);
/*				rdis = dis[0] - mdis;*/
				um_translate_point(pp[i],rdis,vc1,pt);
				um_translate_point(pt,dis[4],&pp1[3],pt);
				uu_list_push(&Scvpt[wto],pt);
				uu_list_push(&Scvvc[wto],&pp1[3]);
				Snpts[wto]++;
				break;
			}
		}
		if (i<0) i = 0;
/*
........Copy points to list
*/
		tdis = mdis;
		for (j=i+1;j<Snpts[*which]-1;j++)
		{
			rdis = tdis/dis[2] * dis[4];
			um_translate_point(pp[j],rdis,&pp1[3],pt);
			uu_list_push(&Scvpt[wto],pt);
			uu_list_push(&Scvvc[wto],&pp1[3]);
			Snpts[wto]++;
			rdis = um_dcccc(pp[j],pp[j+1]);
			tdis -= rdis;
		}

		if (ktype == 2 && dis[2] < 0.)
		{
			uu_list_push(&Scvpt[wto],&pp1[0]);
			uu_list_push(&Scvvc[wto],&pp1[3]);
			Snpts[wto]++;
		}
	}
/*
.....Exit move or entry move with negative overlap
*/
	else if ((ktype == 2 && dis[2] > 0.) || (ktype == 1 && dis[2] < 0.))
	{
		if (ktype == 2)
			ist = nc;
		else if (ktype == 1)
			ist = 1;
		iadd = UU_FALSE;
		ncl_profile_pt(which,&ist,&nwd,&np,pp1);
/*
........Loop through curve points
*/
		tdis = 0.;
		for (i=0;i<nc-1;i++)
		{
			rdis = um_dcccc(pp[i],pp[i+1]);
			mdis = tdis;
			tdis += rdis;
			rdis = fabs(tdis-fabs(dis[2]));
			if (rdis <= .001) break;
			else if (tdis > fabs(dis[2]))
			{
				um_vcmnvc(pp[i+1],pp[i],vc1);
				um_unitvc(vc1,vc1);
				rdis = fabs(dis[2]) - mdis;
				um_translate_point(pp[i],rdis,vc1,pt);
				um_translate_point(pt,dis[4],&pp1[3],ptadd);
				iadd = UU_TRUE;
				break;
			}
		}
		if (i<0) i = 0;
/*
........Copy points to list
*/
		if (ktype == 1 && dis[2] < 0.)
		{
			uu_list_push(&Scvpt[wto],&pp1[0]);
			uu_list_push(&Scvvc[wto],&pp1[3]);
			Snpts[wto]++;
		}

		tdis = 0.;
		for (j=1;j<=i;j++)
		{
			rdis = um_dcccc(pp[j],pp[j-1]);
			tdis += rdis;
			rdis = tdis/dis[2] * dis[4];
			um_translate_point(pp[j],rdis,&pp1[3],pt);
			uu_list_push(&Scvpt[wto],pt);
			uu_list_push(&Scvvc[wto],&pp1[3]);
			Snpts[wto]++;
		}
/*
........Copy final point to list
*/
		if (iadd)
		{
			uu_list_push(&Scvpt[wto],ptadd);
			uu_list_push(&Scvvc[wto],&pp1[3]);
			Snpts[wto]++;
		}
	}
/*
.....End of routine
*/
	*npts = Snpts[wto];
	return;
}

/*********************************************************************
**    E_FUNCTION     : S_pts_at_arc (which,wto,ktype,dis,kds,npts,stan)
**       Returns points along an arc based on the end point along a curve
**       and the radius.  The end point will not be returned in the list.
**    PARAMETERS
**       INPUT  :
**          which    = Which curve list to use.
**          wto      = Which curve list to store the points in.
**          ktype    = 1 = Entry move, 2 = Exit move.
**          dis      = (0) = Arc radius
**                     (1) = Arc Rise distance.
**          kds      = Drive surface condition.  1 = ON, 2 = OFF (to).
**       OUTPUT :
**          npts     = Number of points generated.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_pts_at_arc(which,wto,ktype,dis,kds,npts,stan)
int *which,wto,*npts,kds,ktype;
UU_REAL dis[];
UM_vector stan;
{
	int i,ist,nwd,np,nc,is0,is1;
	UU_REAL pp1[6],rdis,deltang,cosa,ang;
	UM_coord pt,cpt,*pp;
	UM_vector vc1,vc2,*pv,vc3,vc4,svec;
	struct UM_circle_rec c1;
	Gwpoint3 gpt[100];
	UM_transf mtf;
/*
.....Initialize routine
*/
	S_list_ary(*which,&pp,&pv,&nc);
	if (ktype == 1)
	{
		ist = 1;
		is0 = 0;
		is1 = 1;
	}
	else
	{
		ist = Snpts[*which];
		is0 = ist - 1;
		is1 = ist - 2;
	}
	nwd = 6;
	np = 1;
	ncl_profile_pt(which,&ist,&nwd,&np,pp1);
/*
.....Calculate entry circle
........Circle center
*/
	um_vcmnvc(pp[is0],pp[is1],vc1);
	um_unitvc(vc1,vc1);
	if ((ktype == 1 && Sldir == 1) || (ktype == 2 && Sldir == -1))
		um_cross(vc1,&pp1[3],vc2);
	else
		um_cross(&pp1[3],vc1,vc2);
	if (kds == 1) um_translate_point(pp[is0],dis[0],&pp1[3],cpt);
	else um_translate_point(pp[is0],dis[0],vc2,cpt);
/*
........Start point
*/
	if (fabs(dis[3]) > 0.0)
	{
/*
.....Comb Arc
*/
		if (ktype == 1)
			ang = (dis[3]) / UM_RADIAN * (0-Sldir);
		else
			ang = (dis[3]) / UM_RADIAN * Sldir;

		um_rotlntf(cpt,&pp1[3],ang,mtf);
		um_negvc(vc2,vc3);
		um_vctmtf(vc3,mtf,vc4);		
		um_translate_point(cpt,dis[0],vc4,pt);
	}
	else
		um_translate_point(cpt,dis[0],vc1,pt);
/*
........Actual circle
*/
	if (ktype == 1)
		ncl_cutr_arcc2p(cpt,pt,pp[is0],&c1);
	else
		ncl_cutr_arcc2p(cpt,pp[is0],pt,&c1);
/*
.....Arc start/End tangent
*/
	if (ktype == 1)
		um_cross(c1.svec,c1.nvec,stan);
	else
	{
		um_vcmnvc(cpt,pt,svec);
		um_unitvc(svec,svec);
		um_cross(svec,c1.nvec,stan);
	}
/*
.....Determine how many points to output on circle
*/
	cosa = (c1.radius-.002) / c1.radius;
	if (cosa > 1.) cosa = 1.;
	deltang = fabs(acos(cosa));
	if (deltang <= 0.) nc = 0;
	else nc = c1.dang / deltang;
	if (nc < 4) nc = 4;
	if (nc > 99) nc = 99;
/*
.....Calculate circle points
*/
	ncl_cutter_circle(&c1,gpt,nc);
/*
.....Place circle points in list
*/
	if (ktype == 1)
	{
		is0 = 0;
		if (fabs(dis[1]) < UM_FUZZ)
			is1 = nc;
		else	
			is1 = nc - 1;
	}
	else
	{
		if (fabs(dis[1]) < UM_FUZZ)
			is0 = 0;
		else
			is0 = 1;
		is1 = nc;
	}
	for (i=is0;i<is1;i++)
	{
		if (kds == 1)
			um_vctovc(&gpt[i],pt);
		else
		{
			if (ktype == 1)
				rdis = (1.-((UU_REAL)i/(UU_REAL)(nc-1))) * dis[1];
			else
				rdis = (((UU_REAL)i/(UU_REAL)(nc-1))) * dis[1];
			um_translate_point(&gpt[i],rdis,&pp1[3],pt);
		}
		uu_list_push(&Scvpt[wto],pt);
		uu_list_push(&Scvvc[wto],&pp1[3]);
		Snpts[wto]++;
	}
/*
.....End of routine
*/
	*npts = Snpts[wto];
	return;
}

/*********************************************************************
**    I_FUNCTION     : S_calc_tilt_angles(which,npt)
**       Calculates the tilt angles and updates the tool axis vectors
**       accordingly.
**    PARAMETERS
**       INPUT  :
**          which    = Which curve list to use.
**          npts     = Number of points in curve list.
**       OUTPUT :
**          npts     = Updated number of points generated.
**    RETURNS      : none
**    SIDE EFFECTS :
**          Points may be added to the list so that the tool axis
**          fanning is complete at the specified distance.
**    WARNINGS     : none
*********************************************************************/
static void S_calc_tilt_angles(which,npt)
int which,*npt;
{
	int nc,nc1,i,nind;
	UU_REAL len,mlen,rdis,tang,rper,dis[2],ang[2];
	UM_coord *pp,pt,pt0;
	UM_vector *pv,vc0,vc1,vc2,lastvc;
	struct UM_rotmatrix mx;
/*
.....Initialize routine
*/
	S_list_ary(which,&pp,&pv,&nc1);
	pt0[0] = pt0[1] = pt0[2] = 0.;
	nc =  *npt;
	dis[0] = Sps_tilt_dis[0]; dis[1] = Sps_tilt_dis[1];
	mlen = um_getpolylen(nc,pp);
	if (Sps_tilt_flag[0] == 2) dis[0] = mlen * (dis[0]/100.);
	if (Sps_tilt_flag[1] == 2) dis[1] = mlen * (dis[1]/100.);
	ang[0] = Sps_tilt_ang[0]/UM_RADIAN; ang[1] = Sps_tilt_ang[1]/UM_RADIAN;
/*
.....Tool axis is ATANGL,PS
.....Modify calculated tool axis vectors
*/
	if (Sps_tilt_flag[0] == 3)
	{
		tang = ang[0];
		for (i=0;i<nc;i++)
		{
			if (i != nc-1) um_vcmnvc(pp[i+1],pp[i],vc0); um_unitvc(vc0,vc0);
			if (um_mag(vc0) < UM_FUZZ) um_vctovc(lastvc,vc0);
			um_vctovc(vc0,lastvc);
			um_cross(pv[i],vc0,vc1); um_unitvc(vc1,vc1);
			um_rotatept(pv[i],vc1,pt0,tang,UU_TRUE,&mx);
		}
		goto done;
	}
/*
.....Shorten distances if longer than the curve length
*/
	if (dis[0]+dis[1] > mlen)
	{
		rper = dis[0] / (dis[0]+dis[1]);
		len = mlen * rper;
		dis[0] = len;
		rper = 1-rper;
		len = mlen * rper;
		dis[1] = len;
	}
/*
.....Calculate start distances and angles
*/
	if (ang[0] != 0.)
	{
		len = 0;
		for (i=1;i<nc;i++)
		{
			mlen = um_dcccc(pp[i-1],pp[i]);
			len += mlen;
			if (len >= dis[0]-Stol) break;
		}
		nind = i;
/*
........Another point needs to be added
*/
		if (len-Stol > dis[0])
		{
			rdis = len - dis[0];
			um_vcmnvc(pp[i-1],pp[i],vc1); um_unitvc(vc1,vc1);
			um_translate_point(pp[i],rdis,vc1,pt);
			um_vcmnvc(pp[i],pt,vc0);
			if (um_mag(vc0) > UM_FUZZ)
			{
				uu_list_insert((char *)&Scvpt[which],i,pt);
				S_list_ary(which,&pp,&pv,&nc1);

				rper = rdis / mlen;
				tang = um_angle(pv[i-1],pv[i]) * rper;

				if (i != nc-1) um_vcmnvc(pp[i],pp[i-1],vc0); um_unitvc(vc0,vc0);
				if (um_mag(vc0) < UM_FUZZ) um_vctovc(lastvc,vc0);
				um_vctovc(vc0,lastvc);

				um_cross(vc0,pv[i],vc1); um_unitvc(vc1,vc1);
				um_vctovc(pv[i],vc2);
				um_rotatept(vc2,vc1,pt0,-tang,UU_TRUE,&mx);
				uu_list_insert((char *)&Scvvc[which],i,vc2);

				*npt = ++nc;
				S_list_ary(which,&pp,&pv,&nc1);
			}
		}
/*
........Apply tilt angles to beginning of move
*/
		len = 0;
		for (i=0;i<nind;i++)
		{
			if (i != nc-1) um_vcmnvc(pp[i+1],pp[i],vc0); um_unitvc(vc0,vc0);
			if (um_mag(vc0) < UM_FUZZ) um_vctovc(lastvc,vc0);
			um_vctovc(vc0,lastvc);

			tang = -ang[0] * (1.-len/dis[0]);
			um_cross(vc0,pv[i+1],vc1); um_unitvc(vc1,vc1);
			um_rotatept(pv[i],vc1,pt0,tang,UU_TRUE,&mx);
			mlen = um_dcccc(pp[i],pp[i+1]);
			len += mlen;
		}
	}
/*
.....Calculate exit distances and angles
*/
	if (ang[1] != 0.)
	{
		len = 0;
		for (i=nc-1;i>=0;i--)
		{
			mlen = um_dcccc(pp[i-1],pp[i]);
			len += mlen;
			if (len >= dis[1]-Stol) break;
		}
		nind = i;
/*
........Another point needs to be added
*/
		if (len-Stol > dis[1])
		{
			rdis = len - dis[1];
			um_vcmnvc(pp[i],pp[i-1],vc1); um_unitvc(vc1,vc1);
			um_translate_point(pp[i-1],rdis,vc1,pt);
			um_vcmnvc(pp[i],pt,vc0);
			if (um_mag(vc0) > UM_FUZZ)
			{
				uu_list_insert((char *)&Scvpt[which],i,pt);
				S_list_ary(which,&pp,&pv,&nc1);

				if (i != nc-1) um_vcmnvc(pp[i],pp[i-1],vc0); um_unitvc(vc0,vc0);
				if (um_mag(vc0) < UM_FUZZ) um_vctovc(lastvc,vc0);
				um_vctovc(vc0,lastvc);

				rper = rdis / mlen;
				tang = um_angle(pv[i],pv[i-1]) * rper;
				um_cross(vc0,pv[i-1],vc1); um_unitvc(vc1,vc1);
				um_vctovc(pv[i-1],vc2);
				um_rotatept(vc2,vc1,pt0,-tang,UU_TRUE,&mx);
				uu_list_insert((char *)&Scvvc[which],i,vc2);

				*npt = ++nc;
				S_list_ary(which,&pp,&pv,&nc1);
			}
		}
/*
........Apply tilt angles to end of move
*/
		len = 0;
		for (i=nc-1;i>nind;i--)
		{
			if (i != 0) um_vcmnvc(pp[i],pp[i-1],vc0); um_unitvc(vc0,vc0);
			if (um_mag(vc0) < UM_FUZZ) um_vctovc(lastvc,vc0);
			um_vctovc(vc0,lastvc);
/*
........    Beginning ang and dis specified instead of exit move.
........		tang = ang[0] * (1.-len/dis[0]);
*/
                        tang = ang[1] * (1.-len/dis[1]);
			um_cross(vc0,pv[i-1],vc1); um_unitvc(vc1,vc1);
			um_rotatept(pv[i],vc1,pt0,tang,UU_TRUE,&mx);
			mlen = um_dcccc(pp[i],pp[i-1]);
			len += mlen;
		}
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : S_print_curve (which,title)
**       Prints out the requested Profile curve when DEBUG is set to 1
**       at the top of the file and the system variable UU_DEBUGL is
**       set to 1.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Print Profile curve.
**                     1 = Print Offset curve.
**                     2 = Print Projected curve.
**                     3 = Print Weeded curve.
**
**          title    = Text to print prior to curve.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_print_curve(which,title)
int which;
char *title;
{
#if DEBUG == 1
	int i,nc;
	UM_coord *pp;
	UM_vector *pv;
	char sbuf[80];
/*
.....Print out the curve
*/
	S_list_ary(which,&pp,&pv,&nc);
	sprintf(sbuf,"%s = %d",title,nc);
	NclxDbgPstr(sbuf);
	for(i=0;i<nc;i++)
	{
		sprintf(sbuf,"   %lf,%lf,%lf - %lf,%lf,%lf",pp[i][0],pp[i][1],pp[i][2],
			pv[i][0],pv[i][1],pv[i][2]);
		NclxDbgPstr(sbuf);
	}
#endif
}

/*********************************************************************
**    E_FUNCTION     : S_print_time (which)
**       Prints out the time for calculating the various curve types
**       when the system variable UU_DEBUGL is set to 1.
**    PARAMETERS
**       INPUT  :
**          which    = CVPT = Print Profile curve time.
**                     OFPT = Print Offset curve time.
**                     PJPT = Print Projected curve time.
**                     WDPT = Print Weeded curve time.
**                     NCVS = Print Total curve times.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_print_time(which)
int which;
{
#if DEBUG == 1
	int mtim,i;
	char sbuf[80];
	static char *pr[WDPT+1]={"Created curve","Offset curve","Projected curve",
		"Weeded curve"};
/*
.....Print out the total times
*/
	if (which == NCVS)
	{
		NclxDbgPstr(" ");
		for (i=0;i<=WDPT;i++)
		{
			sprintf(sbuf,"Total %s time = %d",pr[i],Stim[i]);
			NclxDbgPstr(sbuf);
		}
		sprintf(sbuf,"Total time = %d\n",Stim[NCVS]);
		NclxDbgPstr(sbuf);
		for (i=0;i<NCVS+1;i++) Stim[i] = 0;
	}
/*
.....Print out the curve time
*/
	else
	{
		mtim = (Stems-Stims)*1000 + (Stemm-Stimm);
		Stim[which] += mtim;
		Stim[NCVS] += mtim;
		sprintf(sbuf,"%s time = %d",pr[which],mtim);
		NclxDbgPstr(sbuf);
	}
#endif
}

/*********************************************************************
**    E_FUNCTION     : nclf_getpv_value(vec_first, vec_last)

**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclf_getpv_value(vec_first, vec_last)
UM_real8 vec_first[3], vec_last[3];
{
	int i;
	for (i=0; i<3;i++)
		vec_first[i] = S_pv[0][i];
	for (i=0; i<3;i++)
		vec_last[i] = S_pv[1][i];
}
