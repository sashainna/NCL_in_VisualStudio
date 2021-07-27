/*********************************************************************
**	 NAME:  necdraw2.c
**		 CONTAINS:
**			ncl_draw_lathe_cutter
**			ncl_display_lathe_cutter
**       ncl_tessel_polyline
**			ncl_get_lathe_center
**	 COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**	 MODULE NAME AND RELEASE LEVEL 
**       necdraw2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:26
*********************************************************************/

#include "usysdef.h"
#include "lipv.h"
#include "zsysdep.h"
#include "class.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdattr.h"
#include "m2dattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "mdcoord.h"
#include "modef.h"
#include "nccs.h"
#include "gomisc.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gviw.h"
#include "gsegop.h"
#include "gmat4.h"
#include "gconvert.h"
#include "mdcoord.h"
#include "mfort.h"
#include "mplot.h"
#include "mgeom.h"
#include "nclfc.h"
#include "nclmplay.h"

#define DTOR (UM_PI/180.0)

extern int DRAW_DISPLAY;
extern int dyn_view;

static void S_gen_circle();

/*********************************************************************
**    E_FUNCTION     : ncl_draw_lathe_cutter(cutseg,vp,ipv)
**		Calculate a cutter symbol for display in the specified
**		viewport.
**    PARAMETERS   
**       INPUT  : 
**          cutseg = Current cutter definition.
**          vp     = View port to calculate cutter for.
**          ipv    = 1 = Calculate cutter for NCLIPV.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_draw_lathe_cutter(cutseg,vp,ipv)
UN_motseg_cutter *cutseg;
int vp;
int ipv;
{
	int status,npts,n,i,nptx,ctype;
	UU_LOGICAL groove;
	UU_REAL scal,mntang,tangle,htan,hsin,hcos,msin,mcos,temp,px,py;
	UM_coord tend,ptx[8],ocen,hcen,scen1,scen2,*curve;
	UM_vector taxis,tmpvc,*vec,vc0;
	UM_length tradius,theight,hradius,twidth;
	UU_LIST *cvlist,*cnlist;
	struct UM_circle_rec c1;
/*
.....Store cutter parameters in local variables
*/
	ctype = cutdef[vp].cutr[8] - 10;
	tradius = cutdef[vp].cutr[0] / 2.0;
	hradius = cutdef[vp].cutr[6] / 2.0;
	theight = cutdef[vp].cutr[2];
	tangle = cutdef[vp].cutr[7] * DTOR;
	mntang = cutdef[vp].cutr[5] * DTOR;
	hcen[0] = hcen[1] = hcen[2] = 0.;
	tend[0] = 0.;
	tend[1] = 0.;
	tend[2] = 0.;
	taxis[0] = 0.;
	taxis[1] = 0.;
	taxis[2] = 1.;
	vc0[0] = vc0[1] = vc0[2] = 0.;
	if (fabs(tangle) > UM_FUZZ)
	{
		hsin = sin(tangle);
		hcos = cos(tangle);
		htan = tan(tangle/2.);
		msin = sin(mntang);
		mcos = cos(mntang);
	}
	if (ipv == 1) scal = 1.;
	else scal = cutdef[vp].view.scale;
/*
.....Initialize cutter storage
*/
   UU_LIST_EMPTY(&cutseg->cutsym->geo->curve);
   UU_LIST_EMPTY(&cutseg->cutsym->geo->cnorm);
   curve = (UM_coord *)UU_LIST_ARRAY(&cutseg->cutsym->geo->curve);
   cvlist = &cutseg->cutsym->geo->curve;
   cnlist = &cutseg->cutsym->geo->cnorm;
   npts = 0;
/*
.....Circular insert
*/
	if (ctype == 4)
	{
		um_vctovc(tend,hcen);
		um_vctovc(tend,c1.center);
		um_vctovc(taxis,c1.nvec);
		um_perpvc(taxis,c1.svec);
		c1.radius = tradius;
		c1.dang = UM_TWOPI;
		ncl_cutter_nsides(&c1,scal,&n);
		uu_list_expand(cvlist,n);
		curve = (UM_coord *)UU_LIST_ARRAY(cvlist);
		ncl_cutter_circle(&c1,&curve[npts],n);
/*
........Store the side normals
*/
		for (i=0;i<n;i++)
		{
			um_vcmnvc(curve[npts+i],c1.center,tmpvc);
			uu_list_push(cnlist,tmpvc);
		}
		npts = npts + n;
	}
/*
.....Standard Insert or Grooving tool
*/
	else
	{
		groove = UU_FALSE;
/*
........Calculate Grooving Tool parameters
*/
		if (ctype == 5)
		{
			groove = UU_TRUE;
			hsin = 1.;
			hcos = 0.;
			htan = 1.;
			twidth = cutdef[vp].cutr[5];
		}
/*
........Calculate center of inscribed circle
........For standard Insert
*/
		temp = hradius - tradius;
		hcen[0] = tend[0] + temp / htan;
		hcen[1] = tend[1] + temp;
		hcen[2] = tend[2];
/*
........Calculate center of opposite tool radius
*/
		ocen[0] = hcen[0] + temp / htan;
		ocen[1] = hcen[1] + temp;
		ocen[2] = tend[2];
/*
........Calculate center of side radii
*/
		if (ctype == 3)
			scen1[0] = hcen[0] + temp / htan;
		else
			scen1[0] = hcen[0] + temp * htan;
		scen1[1] = tend[1];
		scen1[2] = tend[2];
		if (ctype == 3)
		{
			scen2[0] = hcen[0];
			scen2[1] = hcen[1] + temp / hcos;
		}
		else
		{
			scen2[0] = hcen[0] - temp * htan;
			scen2[1] = ocen[1];
		}
		scen2[2] = tend[2];
/*
........Calculate points on diamond
*/
		ptx[0][0] = tend[0];
		ptx[0][1] = tend[1] - tradius;
		ptx[0][2] = tend[2];
		ptx[1][0] = scen1[0];
		ptx[1][1] = scen1[1] - tradius;
		ptx[1][2] = tend[2];
		if (ctype == 3)
		{
			ptx[2][0] = scen1[0] + tradius * hsin;
			ptx[2][1] = scen1[1] + tradius * hcos;
			ptx[2][2] = tend[2];
		}
		else
		{
			ptx[2][0] = scen1[0] + tradius * hsin;
			ptx[2][1] = scen1[1] - tradius * hcos;
			ptx[2][2] = tend[2];
		}
		if (groove)
		{
			ptx[3][0] = ptx[2][0];
			ptx[3][1] = ptx[1][0] + twidth;
			ptx[3][2] = tend[2];
			ptx[4][0] = tend[0] - tradius;
			ptx[4][1] = ptx[3][1];
			ptx[4][2] = tend[2];
			ptx[5][0] = ptx[4][0];
			ptx[5][1] = tend[1];
			ptx[5][2] = tend[2];
			nptx = 6;
			hcen[0] = ptx[5][0] + hradius;
			hcen[1] = ptx[3][1];
		}
		else
		{
			ptx[3][0] = ocen[0] + tradius * hsin;
			ptx[3][1] = ocen[1] - tradius * hcos;
			ptx[3][2] = tend[2];
			ptx[4][0] = ocen[0];
			ptx[4][1] = ocen[1] + tradius;
			ptx[4][2] = tend[2];
			if (ctype == 3)
			{
				ptx[5][0] = scen2[0] + tradius * hsin;
				ptx[5][1] = scen2[1] + tradius * hcos;
				ptx[5][2] = tend[2];
			}
			else
			{
				ptx[5][0] = scen2[0];
				ptx[5][1] = scen2[1] + tradius;
				ptx[5][2] = tend[2];
			}
			ptx[6][0] = scen2[0] - tradius * hsin;
			ptx[6][1] = scen2[1] + tradius * hcos;
			ptx[6][2] = tend[2];
			ptx[7][0] = tend[0] - tradius * hsin;
			ptx[7][1] = tend[1] + tradius * hcos;
			ptx[7][2] = tend[2];
			nptx = 8;
		}
/*
........Define cutter
*/
		S_gen_circle(tend,ptx[nptx-1],ptx[0],scal,&npts,cvlist,cnlist);
		S_gen_circle(scen1,ptx[1],ptx[2],scal,&npts,cvlist,cnlist);
		if (groove)
		{
			uu_list_push(cvlist,ptx[3]);
			uu_list_push(cvlist,ptx[4]);
			uu_list_push(cvlist,ptx[5]);
			uu_list_push(cnlist,vc0);
			uu_list_push(cnlist,vc0);
			vec = (UM_vector *)UU_LIST_ARRAY(cnlist);
			uu_list_push(cnlist,vec[0]);
			npts = npts + 3;
		}
		else
		{
			if (ctype != 3)
			{
				S_gen_circle(ocen,ptx[3],ptx[4],scal,&npts,cvlist,cnlist);
			}
			S_gen_circle(scen2,ptx[5],ptx[6],scal,&npts,cvlist,cnlist);
			uu_list_push(cvlist,ptx[7]);
			vec = (UM_vector *)UU_LIST_ARRAY(cnlist);
			uu_list_push(cnlist,vec[0]);
			npts++;
/*
........Rotate points for mount angle
*/
			if (fabs(mntang) > UM_FUZZ)
			{
				curve = (UM_coord *)UU_LIST_ARRAY(cvlist);
				vec = (UM_vector *)UU_LIST_ARRAY(cnlist);
				for (i=0;i<npts;i++)
				{
					px = curve[i][0] - tend[0];
					py = curve[i][1] - tend[1];
					curve[i][0] = px*mcos - py*msin + tend[0];
					curve[i][1] = py*mcos + px*msin + tend[1];
					px = vec[i][0];
					py = vec[i][1];
					vec[i][0] = px*mcos - py*msin;
					vec[i][1] = py*mcos + px*msin;
				}
				px = hcen[0] - tend[0];
				py = hcen[1] - tend[1];
				hcen[0] = px*mcos - py*msin + tend[0];
				hcen[1] = py*mcos + px*msin + tend[1];
			}
		}
	}
/*
.....Store attach point
*/
	uu_list_insert(cvlist,0,hcen);
	npts++;
/*
.....End of routine
*/
	status = UU_SUCCESS;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_gen_circle(cpt,spt,ept,scal,npts,cvlist,cnlist)
**		Generate circular points and normals for a lathe cutter.
**    PARAMETERS   
**       INPUT  : 
**          cpt      = Center point of circle.
**          spt      = Starting point of circle.
**          ept      = Ending point of circle.
**          scal     = Scale factor to use to generate circular points.
**          npts     = Number of points stored in list.
**       OUTPUT :
**          npts     = Updated number of points in list.
**          cvlist   = Updated list of points.
**          cnlist   = Updated list of normals.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_gen_circle(cpt,spt,ept,scal,npts,cvlist,cnlist)
UM_coord cpt,spt,ept;
UU_REAL scal;
int *npts;
UU_LIST *cvlist,*cnlist;
{
	int status,n,i;
	UM_coord *curve;
	UM_vector tmpvc;
	struct UM_circle_rec c1;
/*
.....Generate the circle
*/
	status = ncl_cutr_arcc2p(cpt,spt,ept,&c1);
	n = 0;
	if (status == UU_SUCCESS)
	{
		ncl_cutter_nsides(&c1,scal,&n);
		uu_list_expand(cvlist,n);
		curve = (UM_coord *)UU_LIST_ARRAY(cvlist);
		ncl_cutter_circle(&c1,&curve[*npts],n);
		for (i=0;i<n;i++)
		{
			um_vcmnvc(curve[*npts+i],c1.center,tmpvc);
			uu_list_push(cnlist,tmpvc);
		}
	}
	*npts = *npts + n;
}

/*********************************************************************
**    E_FUNCTION     : ncl_display_lathe_cutter(cutseg,cutsym,vp,shaded,
**		                                          ngpt,ofs)
**		Calculates the tool part display (cutter,shank,holder) for a lathe
**		tool in the specified viewport.
**    PARAMETERS   
**       INPUT  : 
**				cutseg = Current cutter definition.
**				cutsym = Current cutter symbol structure.
**				vp     = View port to calculate cutter for.
**				shaded = 1 = Calculate a shaded cutter.
**				ngpt   = Number of points currently in tool display storage.
**				ofs    = XYZ offsets for tool part.
**       OUTPUT :  
**				ngpt   = Updated number of tool display points.
**				ofs    = Updated XYZ offsets for tool part.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_display_lathe_cutter(cutseg,cutsym,vp,shaded,ngpt,ofs)
UN_motseg_cutter *cutseg;
UN_motseg_symbol *cutsym;
int vp;
int shaded;
int *ngpt;
UU_REAL ofs[];
{
	int status,npts,ngeo,isparl,isperp,i,j,nptx,segfl,nc,ist,ntess,ngeo_tess;
	int cnpt;
	UU_LOGICAL iswap,um_plane1();
	UU_REAL px,um_mag(),zmin,zmax;
	UM_coord tend,rpt,*curve,cpt[500],pt2;
	UM_vector taxis,vpn,cvc[500];
	UM_plane pln;
	UM_length theight;
	Gwpoint3 *vgpt,*vgnorm,*cdef,*cnorm;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	curve = (UM_coord *)UU_LIST_ARRAY(&cutsym->geo->curve);
	npts = UU_LIST_LENGTH(&cutsym->geo->curve);
	if (npts == 0) return(status);
	nc = *ngpt;
	ist = nc;
	ngeo = cutdef[vp].view.ngeo;
	nptx = 0;
	cnpt = 0.;
/*
.....Store cutter parameters in local variables
*/
	theight = cutdef[vp].cutr[2];
	um_vctovc(curve[0],tend);
	taxis[0] = 0.;
	taxis[1] = 0.;
	taxis[2] = 1.;
	segfl = cutdef[vp].segfl;
	vpn[0] = cutdef[vp].view.vpn[0];
	vpn[1] = cutdef[vp].view.vpn[1];
	vpn[2] = cutdef[vp].view.vpn[2];
	iswap = cutdef[vp].seguse;
/*
.....Determine if tlaxis is parallel or perpto view normal axis
.....Used for optimizing cutter display
*/
	if (cutdef[vp].seguse && segfl == 1) segfl = 2;
	if (shaded != 0)
	{
		isperp = 1;
		isparl = 0;
	}
   else if (segfl == 2)
	{
		isperp = 0;
		isparl = 0;
	}
	else
	{
		isparl = um_vcparall(taxis,vpn);
		isperp = um_vcperp(taxis,vpn);
	}
/*
.....Offset the profile if required
*/
	zmax = cutsym->zlim[0];
	if (cutsym->zlim[1] != 0.)
		zmin = zmax - cutsym->zlim[1] + ofs[2];
	else
		zmin = zmax - theight + ofs[2];
	cdef = (Gwpoint3 *)uu_malloc(sizeof(Gwpoint3)*(npts+1));
	cnorm = (Gwpoint3 *)uu_malloc(sizeof(Gwpoint3)*(npts+1));
	for (i=1;i<npts;i++)
	{
		cdef[i-1].x = curve[i][0] + ofs[0];
		cdef[i-1].y = curve[i][1] + ofs[1];
		cdef[i-1].z = zmax + ofs[2];
	}
	cnpt = npts - 1;
	if (um_dcccc(curve[1],curve[npts-1]) > UM_FUZZ)
	{
		um_vctovc(&cdef[0],&cdef[npts-1]);
		cnpt++;
	}
	nptx = cnpt;
/*
........Zero out side normal vectors
........They will be calculated further on
*/
	if (shaded)
	{
		for (i=0;i<nptx;i++)
			cnorm[i].x = cnorm[i].y = cnorm[i].z= 0.;
/*
........Tessellate the top of the cutter
*/
		ncl_tessel_polyline(cdef,cpt,cvc,nptx,&ntess);
		uu_list_push_multiple(&cutdef[vp].view.gpt,ntess,cpt);
		uu_list_push_multiple(&cutdef[vp].view.gnorm,ntess,cvc);
		ngeo_tess = 0;
		j = 3;
		for (i=0;i<ntess;i=i+3)
		{
			uu_list_push(&cutdef[vp].view.npt,&j);
			ngeo++; ngeo_tess++;
		}
		nc = nc + ntess;
	}
/*
........Store the profile in the cutter definition
*/
	else
	{
		for (i=0;i<nptx;i++) uu_list_push(&cutdef[vp].view.gpt,&cdef[i]);
		uu_list_push(&cutdef[vp].view.npt,&nptx);
		ngeo_tess = 1;
		ntess = nptx;
		nc = nc + nptx;
		ngeo++;
	}
/*
.....Wireframe cutter
........Extrude the cutter shape
*/
	if ((!isparl || segfl == 2) && !shaded)
	{
		nptx = ntess;
		for (i=0;i<nptx;i++)
		{
			vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gpt);
			pt2[0] = vgpt[ist+i].x;
			pt2[1] = vgpt[ist+i].y;
			pt2[2] = zmin;
			uu_list_push(&cutdef[vp].view.gpt,pt2);
			nc++;
		}
		uu_list_push(&cutdef[vp].view.npt,&nptx); ngeo++;
/*
........Calculate the extrusion lines
*/
		nptx = cnpt;
		j = 2;
		for (i=0;i<nptx-1;i++)
		{
			uu_list_push(&cutdef[vp].view.gpt,&cdef[i]);
			nc++;
			pt2[0] = cdef[i].x;
			pt2[1] = cdef[i].y;
			pt2[2] = zmin;
			uu_list_push(&cutdef[vp].view.gpt,pt2);
			nc++;
			uu_list_push(&cutdef[vp].view.npt,&j); ngeo++;
		}
	}
/*
.....Shaded cutter
*/
	else if (shaded)
	{
		nptx = ntess;
/*
........Copy top to bottom
*/
		for (i=0;i<nptx;i++)
		{
			vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gpt);
			pt2[0] = vgpt[ist+i].x; pt2[1] = vgpt[ist+i].y; pt2[2] = zmin;
			uu_list_push(&cutdef[vp].view.gpt,pt2);
			pt2[0] = 0.; pt2[1] = 0.; pt2[2] = 1.;
			uu_list_push(&cutdef[vp].view.gnorm,pt2);
			nc++;
		}
		if (ngeo_tess > 1)
		{
			j = 3;
			for (i=0;i<ngeo_tess;i++)
			{
				uu_list_push(&cutdef[vp].view.npt,&j); ngeo++;
			}
		}
		else
		{
			uu_list_push(&cutdef[vp].view.npt,&nptx); ngeo++;
		}
/*
........Calculate sides of cutter
*/
		nptx = cnpt;
		j = 4;
		for (i=0;i<nptx-1;i++)
		{
			uu_list_push(&cutdef[vp].view.gpt,&cdef[i]);

			pt2[0] = cdef[i].x;
			pt2[1] = cdef[i].y;
			pt2[2] = zmin;
			uu_list_push(&cutdef[vp].view.gpt,pt2);

			pt2[0] = cdef[i+1].x;
			pt2[1] = cdef[i+1].y;
			pt2[2] = zmin;
			uu_list_push(&cutdef[vp].view.gpt,pt2);

			uu_list_push(&cutdef[vp].view.gpt,&cdef[i+1]);
/*
........Calculate vectors for flat
........sides of cutter (grooving tool)
*/
			if (um_mag(&cnorm[i]) == 0.)
			{
				if (um_mag(&cnorm[i+1]) == 0.)
				{
					rpt[0] = cdef[i].x;
					rpt[1] = cdef[i].y;
					rpt[2] = zmin;
					if (um_plane1(&cdef[i],rpt,&cdef[i+1],&pln))
					{
						um_vctovc(pln.n,pt2);
					}
					else
					{
						pt2[0] = 1.;
						pt2[1] = 0.;
						pt2[2] = 0.;
					}
					uu_list_push(&cutdef[vp].view.gnorm,pt2);
					uu_list_push(&cutdef[vp].view.gnorm,pt2);
					uu_list_push(&cutdef[vp].view.gnorm,pt2);
					uu_list_push(&cutdef[vp].view.gnorm,pt2);
				}
				else
				{
					uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i+1]);
					uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i+1]);
					uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i+1]);
					uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i+1]);
				}
			}
			else if (um_mag(&cnorm[i+1]) == 0.)
			{
				uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i]);
				uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i]);
				uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i]);
				uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i]);
			}
			else
			{
				uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i]);
				uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i]);
				uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i+1]);
				uu_list_push(&cutdef[vp].view.gnorm,&cnorm[i+1]);
			}
			nc = nc + 4;
			uu_list_push(&cutdef[vp].view.npt,&j);
			ngeo++;
		}
	}
/*
.....Rotate points if drawing segmented or 
.....shaded cutter (tlaxis = 1,0,0)
*/
	if (iswap)
	{
		vgpt = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gpt);
		vgnorm = (Gwpoint3 *)UU_LIST_ARRAY(&cutdef[vp].view.gnorm);
		for (i=ist;i<nc;i++)
		{
			px = vgpt[i].z;
			vgpt[i].z = -vgpt[i].x;
			vgpt[i].x = px;

			px = vgnorm[i].z;
			vgnorm[i].z = -vgnorm[i].x;
			vgnorm[i].x = px;
		}
	}
/*
.....End of routine
*/
	ofs[0] += curve[0][0];
	ofs[1] += curve[0][1];
	ofs[2] = curve[0][2] + zmin;
	*ngpt = nc;
	cutdef[vp].view.ngeo = ngeo;
	uu_free(cdef);
	uu_free(cnorm);
	status = UU_SUCCESS;
	return(status);
}

/*********************************************************************
**   FUNCTION : int ncl_tessel_polyline (ipt,opt,norm,npti,npto)
**
**      Tessellates a flat 2D-polygon defined by an input polyline.
**
**    PARAMETERS
**       INPUT  :
**          ipt   = Input point array
**          npti  = Number of points in 'ipt'
**       OUTPUT :
**          opt   = Output point array that define tessellation triangles.
**          norm  = Array of normal vectors
**          npto  = Number of points in 'opt'
**    RETURNS :
**             UU_SUCCESS/UU_FAILURE (in case of failure ipt points are
**             copied directly into opt points)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tessel_polyline (ipt,opt,norm,npti,npto)
UM_coord *ipt,*opt;
UM_vector *norm;
int npti,*npto;
{
	UM_2Dcoord p1,p2,p3,w1,w3,w; 
	int k,np,itr, status,i,i1,i2,i3,imin,nver,*n;
	UU_REAL turn,dmin,d;
	UU_LOGICAL bad_triangle, check_angle;
	UU_LIST v_list;
	UM_vector ZAXIS;

	np = npti - 1;
	*npto = 0;
	if (np < 3) return (UU_FAILURE);
	ZAXIS[0] = ZAXIS[1] = 0.; ZAXIS[2] = 1.;
	if (np == 3)
	{
		for (i = 0; i < npti; i++)
		{
			um_vctovc (ipt[i],opt[i]);
			um_vctovc (ZAXIS,norm[i]);
		}
		*npto = 3;

		return (UU_SUCCESS);
	}
	nver = np;

	uu_list_init(&v_list, sizeof(int), np, np);
	if (!v_list.data) goto Done;

	for (i = 0; i < np; i++)
		uu_list_push (&v_list, &i);
/*
... During each pass of the while(nver>3) loop, the polygon is updated 
... as follows:
...
... 1. Find triplet of 3 successive vertices (i1,i2,i3) of the current
...    polygon such that:
...      a. angle (i1,i2,i3) is convex;
...      b. there are no vertices of the current polygon inside triangle
...         (i1,i2,i3);
...      c. angle i1,i2,i3 is not close to PI.
...         (this improves quality of tessellation)
...      d. chordal deviation of surface between i1 & i3 vertices 
...         is min of all triplets satisfying a,b,c 
...      e. if there are no triangles satisfying a+b+c, this pass will be 
...         repeated again without condition c.
... 2. When such triplet is found, cut the angle out:
...      a. make new edge (i1,i3);
...      b. delete vertex i2 from v_list (list of vertices of the current 
...         polygon);
...      c. go to step 1 with the updated polygon (without vertex i2);
...
... Repeat until the original polygon is reduced to triangle;
*/
	itr = 0;
	while (nver > 3)
	{
		check_angle = UU_TRUE;
/*
... cyclic enumeration of vertices: 0 1 2 ... (np-1) 0 1 2 ... (np-1) ...
... n[i] is the number of the vertex of the original polygon
... (from 0,1,2,... np-1 ) that is now vertex # i of the updated polygon 
... ( 0 <= i <= nver-1 )
*/
Again:;
		n = (int *) UU_LIST_ARRAY (&v_list);

		imin = -1;
		dmin = 1.e+20;
		for (i1 = 0; i1 < nver; i1++)
		{
			i2 = um_mod (i1+1, nver);
			i3 = um_mod (i2+1, nver);
			for (k = 0; k < 2; k++)
			{
				p1[k] = ipt[n[i1]][k]; 
				p2[k] = ipt[n[i2]][k]; 
				p3[k] = ipt[n[i3]][k];
			}
			d = UM_SQDIS_2D (p1,p3);

			if (d >= dmin) continue;
/*
... if turn <= 0 -> angle (i1,i2,i3) is concave; go to the next angle
... if turn > 0 -> angle (i1,i2,i3) is convex and is a candidate to be 
... cut out.
*/
			turn = um_triangle_signed_area (p1,p2,p3);
			if (check_angle && turn <= 0. || turn < -1.e-12) continue; 
/*
... first, try to avoid triangles with middle angle close to 180 degrees;
... if there is no triangle satisfying this (unnecessary) condition and 
... the turn (necessary) condition, go over this polygon again
... with check_angle = 0;
*/
			bad_triangle = UU_FALSE;
			um_vcmnvc_2d (p1,p2,w1);
			um_vcmnvc_2d (p3,p2,w3);
			um_unitvc_2d (w1,w1); um_unitvc_2d (w3,w3);

			if (check_angle)
				bad_triangle =  UM_DOT_2D (w1,w3) < -0.98; 
			if (bad_triangle) continue;
/*
... make sure that there are no points of the current polygon inside the
... triangle (i1,i2,i3):
*/
			for (i=0; i<nver && !bad_triangle; i++)
			{
				if ((i==i1) || (i==i2) || (i==i3)) continue; 
				um_vcmnvc_2d (ipt[n[i]],p2,w);
				bad_triangle =
					um_point_isin_triangle (ipt[n[i]],p1,p2,p3,UM_DFUZZ);
			}
/*
... if the triangle is empty, cut vertex i2 out.
*/
			if (!bad_triangle)
			{
				imin = i1;
				dmin = d;
			} 
		}

		if (imin < 0)
		{
			if (!check_angle) goto Done;
			else
			{ 
				check_angle = UU_FALSE;
				goto Again;
			}
		}

		i1 = imin;
		i2 = um_mod(i1+1, nver);
		i3 = um_mod(i2+1, nver);

		um_vctovc (ipt[n[i1]],opt[itr]);
		um_vctovc (ZAXIS, norm[itr]);
		itr++;
		um_vctovc (ipt[n[i2]],opt[itr]);
		um_vctovc (ZAXIS, norm[itr]);
		itr++;
		um_vctovc (ipt[n[i3]],opt[itr]);
		um_vctovc (ZAXIS, norm[itr]);
		itr++;
		
		uu_list_delete (&v_list,i2,1);

		nver = UU_LIST_LENGTH(&v_list);

	}  /* end of while-loop */

Done:;

	if (nver == 3)
	{
		n = (int *) UU_LIST_ARRAY (&v_list);
		um_vctovc (ipt[n[0]],opt[itr]);
		um_vctovc (ZAXIS, norm[itr]);
		itr++;
		um_vctovc (ipt[n[1]],opt[itr]);
		um_vctovc (ZAXIS, norm[itr]);
		itr++;
		um_vctovc (ipt[n[2]],opt[itr]);
		um_vctovc (ZAXIS, norm[itr]);
		itr++;

		status = UU_SUCCESS; *npto = itr;
	}
	else
	{
		status = UU_FAILURE;
		for (i = 0; i < npti; i++)
		{
			um_vctovc (ipt[i],opt[i]);
			um_vctovc (ZAXIS,norm[i]);
		}
		*npto = np;
	}

	uu_list_free (&v_list);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_lathe_center(cutr,hcen)
**			Returns the center of the inscribed circle for lathe cutters.
**    PARAMETERS   
**       INPUT  : 
**          cutr  = Cutter parameters.
**       OUTPUT :  
**			   hcen  = Center of inscribed circle.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_lathe_center(cutr,hcen)
UU_REAL cutr[];
UM_coord hcen;
{
	int ctype;
	UU_REAL tangle,mntang,tradius,hradius,htan,msin,mcos,temp,px,py;
/*
.....Calculate inscribed circle's center
........Circular & Groove tools
*/
	ctype = cutr[8] - 10;
	if (ctype == 4 || ctype == 5)
	{
		hcen[0] = hcen[1] = hcen[2] = 0.;
	}
/*
........Diamond, Square, & Triangle tools
*/
	else
	{
		tangle = cutr[7] * DTOR;
		mntang = cutr[5] * DTOR;
		tradius = cutr[0] / 2.0;
		hradius = cutr[6] / 2.0;
		htan = tan(tangle/2.);
		msin = sin(mntang);
		mcos = cos(mntang);
		temp = hradius - tradius;
		hcen[0] = temp / htan;
		hcen[1] = temp;
		hcen[2] = 0.;
		px = hcen[0];
		py = hcen[1];
		hcen[0] = px*mcos - py*msin;
		hcen[1] = py*mcos + px*msin;
		hcen[2] = 0.;
	}
}
