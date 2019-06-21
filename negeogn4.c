/*********************************************************************
**    NAME         :  negeogn4.c
**       CONTAINS:
**     int ncl_interp_rbsp1 (n, ptve,itsk, crv)
**     int ncl_slpset (n, ptve, imode)
**     int ncl_crvgen (n, ptve, s, pts)
**     int ncl_crvfit (n, ptve)
**     int ncl_segchk (xc,yc,zc,pi,pk,imode)
**     ncl_copy_seg (pi, pj)
**    COPYRIGHT 1991 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       negeogn4.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:35
*********************************************************************/

#include "udebug.h"
#include "modef.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "nccs.h"
#include "nclfc.h"
#include "ncl.h"

/*********************************************************************
**    E_FUNCTION     : ncl_interp_rbsp1 (n, ptve, itsk, nump, sp, ptsp)
**       Interpolate a rational B-spline curve thru a list of points &
**       optional slope vectors.
**    PARAMETERS   
**       INPUT  : 
**          n            number of points.
**          ptve         list of entities.
**          itsk         = 0 - interpolate all points
**                       = 1 - fit thru points.
**                       = 2 - use control points from ptve.
**       OUTPUT :  
**          nump         number of control points in B-spline curve
**          sp           array of knots
**          ptsp         array of control points
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_interp_rbsp1 (n, ptve, itsk, nump, sp, ptsp)
int n, *nump;
struct NCL_crvgen_rec *ptve;
int itsk;
UU_REAL **sp, **ptsp;
{
	int status;
	int npts, ns;
	UU_REAL *s, *pts;
	struct NCL_crvgen_rec *pi;
	char *uu_malloc();

	uu_denter(UU_MTRC,(us,"ncl_interp_rbsp1 ()"));

	s = 0;
	pts = 0;
	if (itsk == 0 || n < 3)
	{
		pi = ptve+1;
		if (n<3 && ptve->inv == 0 && pi->inv == 0)
		{
			ptve->a = pi->x-ptve->x;
			ptve->b = pi->y-ptve->y;
			ptve->c = pi->z-ptve->z;
			ptve->inv = 1;
		}
		status = ncl_slpset (n,ptve,0);
	}
	else if (itsk != 2)
		status = ncl_crvfit(&n, ptve);

	if (itsk == 2)
	{
		npts = n;
		ns = n+4;
		s = (UU_REAL *)uu_malloc(ns*sizeof(*s));
		pts = (UU_REAL *)uu_malloc(3*n*sizeof(*pts));
		status = ncl_crvctr(n, ptve, s, pts);
	}
	else
	{
		if (status == UU_SUCCESS)
		{
			npts = 3*n-2;
			ns = npts+4;
			s = (UU_REAL *)uu_malloc(ns*sizeof(*s));
			pts = (UU_REAL *)uu_malloc(3*npts*sizeof(*pts));
			if (s == 0 || pts == 0) status = UU_FAILURE;
		}

		if (status == UU_SUCCESS)
			status = ncl_crvgen (n, ptve, s, pts);
	}

	if (status == UU_SUCCESS)
	{
		*sp = s;
		*ptsp = pts;
		*nump = npts;
	}

   uu_dexitstatus("ncl_interp_rbsp1 ()", status);
   return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_slpset (n, ptve, imode)
**       Set slope vectors for array of curve generation points.
**    PARAMETERS   
**       INPUT  : 
**          n            number of points.
**          ptve         list of entities.
**          imode        mode - 1 = first pass for curve fit
**                              2 = 2nd    "    "    "    "
**                              0 = all others
**       OUTPUT :  
**          ptve         slope vectors calculated
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_slpset (n, ptve, imode)
   int n, imode;
   struct NCL_crvgen_rec *ptve;
   {
   int i,m,ismall, iknt, jknt, status;
   UU_REAL stol,ro,co,tl,hvchg,vchg,vdel,cal,adis,al,sa,dax,day,daz;
   UU_REAL cbe,bdis,bl,sb,dbx,dby,dbz;
   struct NCL_crvgen_rec *pi, *pj, *pk;
   UM_real8 ver;
   UM_int2 idx;

   uu_denter(UU_MTRC,(us,"ncl_slpset ()"));

   if (n<2) goto err;
   idx = 169;
   getsc (&idx, &ver);
   if (imode == 1) stol = 1.0e-4; else stol = 1.0e-5;
   ismall = n<3;
   m=n-1;
   pi=ptve;
   pj = &ptve[1];
   if (m==1 && pi->inv==0 && pj->inv==0)
     {
     pi->a = pj->a = pj->x - pi->x;
     pi->b = pj->b = pj->y - pi->y;
     pi->c = pj->c = pj->z - pi->z;
     pi->inv = pj->inv = 1;
     }
   for (i=0; i<m; i++,pi++,pj++)
     {
     pi->dx = pj->x - pi->x;
     pi->dy = pj->y - pi->y;
     pi->dz = pj->z - pi->z;
     pi->ch = sqrt(pi->dx*pi->dx+pi->dy*pi->dy+pi->dz*pi->dz);
     }
   pi = &ptve[m-1];
   pj = &ptve[m];
   pj->dx = pi->dx;
   pj->dy = pi->dy;
   pj->dz = pi->dz;
   pj->ch = pi->ch;
   if (imode < 2)
	{
		for (i=0,pi=ptve-1,pj=ptve;i<n;i++,pi++,pj++)
		{
			if (pj->inv == 0 && i>0 && i<n-1)
			{
				if (pj->ch < UM_DFUZZ) goto err;
				ro = pi->ch/pj->ch;
				ro = ro*ro;
				pj->a = pi->dx + ro*pj->dx;
				pj->b = pi->dy + ro*pj->dy;
				pj->c = pi->dz + ro*pj->dz;
			}
			if (pj->inv > 0 || i>0 && i<n-1)
			{
				tl = sqrt(pj->a*pj->a+pj->b*pj->b+pj->c*pj->c);
				if (ver < 8.25)
				if (!ismall && pj->a*pj->dx+pj->b*pj->dy+pj->c*pj->dz<0.)
				tl = -tl;
				if (tl < UM_DFUZZ) goto err;

				pj->a = pj->a/tl;
				pj->b = pj->b/tl;
				pj->c = pj->c/tl;
			}
		}
	}

   vchg = 1.0;
   hvchg = 1.0e8;
   for (jknt=0;jknt<3 && vchg > stol;jknt++,stol=hvchg*1.1)
   for (iknt=0;iknt<100 && vchg > stol;iknt++)
     {
     if (iknt>0)
       {
       vchg = 0.0;
       pj = ptve+1;
       for (i=1;i<m;i++,pj++)
         {
         if (pj->inv == 0)
           {
           pi = pj-1;
           pk = pj+1;
           cal = (pi->a*pi->dx+pi->b*pi->dy+pi->c*pi->dz)/pi->ch;
           adis = .531*pi->ch/(.593+cal);
           al = pi->ch*(1.104+.13*cal)/(.851+cal);
           sa = pi->ch*(3.565+.24*cal)/(2.805+cal);
           dax = pi->dx-pi->a*adis;
           day = pi->dy-pi->b*adis;
           daz = pi->dz-pi->c*adis;
           cbe = (pj->dx*pk->a+pj->dy*pk->b+pj->dz*pk->c)/pj->ch;
           bdis = .531*pj->ch/(.593+cbe);
           bl = pj->ch*(1.104+.13*cbe)/(.851+cbe);
           sb = pj->ch*(3.565+.24*cbe)/(2.805+cbe);
           ro = al*sa/(bl*sb);
           dbx = (pj->dx-bdis*pk->a)*ro+dax;
           dby = (pj->dy-bdis*pk->b)*ro+day;
           dbz = (pj->dz-bdis*pk->c)*ro+daz;
           tl = sqrt(dbx*dbx+dby*dby+dbz*dbz);
           dbx /= tl;
           dby /= tl;
           dbz /= tl;
           vdel = fabs(dbx-pj->a)+fabs(dby-pj->b)+fabs(dbz-pj->c);
           if (vdel > vchg) vchg = vdel;
           pj->a = dbx;
           pj->b = dby;
           pj->c = dbz;
           }
         }
       }
     if (ptve->inv == 0)
       {
       pi = ptve;
       pj = ptve+1;
       co = (pi->dx*pj->a+pi->dy*pj->b+pi->dz*pj->c)/pi->ch;
       pi->a = 2.0*co*pi->dx/pi->ch-pj->a;
       pi->b = 2.0*co*pi->dy/pi->ch-pj->b;
       pi->c = 2.0*co*pi->dz/pi->ch-pj->c;
       }
     pj = &ptve[m];
     if (pj->inv == 0)
       {
       pi = pj-1;
       co = (pi->dx*pi->a+pi->dy*pi->b+pi->dz*pi->c)/pi->ch;
       pj->a = 2.0*co*pi->dx/pi->ch-pi->a;
       pj->b = 2.0*co*pi->dy/pi->ch-pi->b;
       pj->c = 2.0*co*pi->dz/pi->ch-pi->c;
       }
     if (ismall) vchg = 0.0;
     if (hvchg>vchg) hvchg = vchg;
     }

   if (ver > 8.25)
     for (i=1,pj=ptve+1;i<n;i++,pj++)
       {
       pi=pj-1;
       co = (pj->a*pi->dx+pj->b*pi->dy+pj->c*pi->dz)/pi->ch;
       if (co < -0.9999)
         {
         co = pj->a*pi->a+pj->b*pi->b+pj->c*pi->c;
         if (co < -0.9999)
           {
           pj->a = -pj->a;
           pj->b = -pj->b;
           pj->c = -pj->c;
           }
         }
       }

   status = UU_SUCCESS;
   goto done;
err:
   status = UU_FAILURE;
done:

   uu_dexitstatus("ncl_slpset ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_crvctr(&n, ptve, s, pts)
**       Generate a rational B-spline curve.
**    PARAMETERS   
**       INPUT  : 
**          n            number of points.
**          ptve         list of entities.
**       OUTPUT :  
**          s            curve s values.
**          pts          curve control points.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_crvctr(n, ptve, s, pts)
   int n;
   struct NCL_crvgen_rec *ptve;
   UU_REAL *s, *pts;
   {
   int i, ix, m, status;
   struct NCL_crvgen_rec *pi;

   uu_denter(UU_MTRC,(us,"ncl_crvgen ()"));

   status = UU_SUCCESS;
   m = n-3;
   ix = 0;
   for (i=0, pi=ptve; i<n; i++,pi++)
     {
     pts[ix++] = pi->x;
     pts[ix++] = pi->y;
     pts[ix++] = pi->z;
     }
   ix = 4;
   m  = (n - 1)/3; 
   for (i=1; i<=m; i++) { s[ix++] = i; s[ix++] = i; s[ix++] = i; }

   s[0] = s[1] = s[2] = s[3] = 0.0;
   s[n+3] = s[n+2] = s[n+1] = s[n];

   uu_dexitstatus("ncl_crvgen ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_crvgen (n, ptve, s, pts)
**       Generate a rational B-spline curve.
**    PARAMETERS   
**       INPUT  : 
**          n            number of points.
**          ptve         list of entities.
**       OUTPUT :  
**          s            curve s values.
**          pts          curve control points.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_crvgen (n, ptve, s, pts)
   int n;
   struct NCL_crvgen_rec *ptve;
   UU_REAL *s, *pts;
   {
   int i, ix, m, status;
   UU_REAL arcsum, arcl, cal, cbe, adis, bdis, cdis, ro;
   UU_REAL dxr, dyr, dzr, dxc, dyc, dzc;
   struct NCL_crvgen_rec *pi, *pj;

   uu_denter(UU_MTRC,(us,"ncl_crvgen ()"));

   status = UU_SUCCESS;
   m = n-1;
   arcsum = 0.0;
   ix = 0;
   for (i=0,pi=ptve,pj = &ptve[1]; i<m; i++,pi++,pj++)
     {
		if (pi->ch < UM_DFUZZ)
			return (UU_FAILURE);
     cal = (pi->a*pi->dx+pi->b*pi->dy+pi->c*pi->dz)/pi->ch;
     cbe = (pj->a*pi->dx+pj->b*pi->dy+pj->c*pi->dz)/pi->ch;
		if ((1.0+cal < UM_DFUZZ) || (1.0+cbe < UM_DFUZZ))
	{
			return (UU_FAILURE);
	}
     adis = .666667*pi->ch/(1.0+cal);
     bdis = .666667*pi->ch/(1.0+cbe);
     if (adis > bdis) adis = bdis*(2.0-bdis/adis);
     if (bdis > adis) bdis = adis*(2.0-adis/bdis);
     pi->a *= adis;
     pi->b *= adis;
     pi->c *= adis;
     dxr = pi->dx-pj->a*bdis;
     dyr = pi->dy-pj->b*bdis;
     dzr = pi->dz-pj->c*bdis;
     dxc = dxr+pi->dx-pi->a;
     dyc = dyr+pi->dy-pi->b;
     dzc = dzr+pi->dz-pi->c;
     cdis = sqrt(dxc*dxc+dyc*dyc+dzc*dzc);
		if (cdis < UM_DFUZZ)
			return (UU_FAILURE);
     ro = 1.62*(adis+bdis)/cdis-.81;
     arcl = (.5-ro)*(adis+bdis)+(.5+.5*ro)*cdis;
     arcsum += arcl;
		if (arcsum < - UM_DFUZZ)
         return (UU_FAILURE);
     s[i] = arcsum;
     pts[ix++] = pi->x;
     pts[ix++] = pi->y;
     pts[ix++] = pi->z;
     pts[ix++] = pi->x+pi->a;
     pts[ix++] = pi->y+pi->b;
     pts[ix++] = pi->z+pi->c;
     pts[ix++] = pi->x+dxr;
     pts[ix++] = pi->y+dyr;
     pts[ix++] = pi->z+dzr;
     }
   pts[ix++] = pi->x;
   pts[ix++] = pi->y;
   pts[ix++] = pi->z;
   for (i=m-1; i>=0; i--)
     {
     ix = 3*i+4;
     s[ix+2] = s[ix+1] = s[ix] = s[i];
     }
   s[0] = s[1] = s[2] = s[3] = 0.0;
   s[3*n+1] = s[3*n];

   uu_dexitstatus("ncl_crvgen ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_crvfit (n, ptve)
**       Fit a rational B-spline curve through a set of points & optional
**       slope vectors.
**    PARAMETERS   
**       INPUT  : 
**          n            number of points.
**          ptve         list of entities.
**       OUTPUT :  
**          n            number of curve segments generated.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_crvfit (npts, ptve)
   int *npts;
   struct NCL_crvgen_rec *ptve;
   {
   int i, k, ih, m, n, imode, otol, status;
   UU_REAL xc, yc, zc, hmov, htol, ftol;
   UU_REAL ro;
   UM_real8 tol8;
   struct NCL_crvgen_rec *pi, *pj, *hldseg, *ph;
   char *uu_malloc();

   uu_denter(UU_MTRC,(us,"ncl_crvfit ()"));

   hldseg = 0;
   gettol (&tol8);
   ftol = tol8/2.0;
   htol = 1.e-6;
   n = *npts;
   m = n-1;
   imode = 1;
   status = ncl_slpset (n, ptve, imode);
   if (status == UU_SUCCESS)
     {
     for (i=1,pi=ptve; i<m; i++,pi++)
       {
       status = ncl_segchk(&xc,&yc,&zc,pi,pi+2,imode);
       pj = pi+1;
       pj->dx = xc-pj->x;
       pj->dy = yc-pj->y;
       pj->dz = zc-pj->z;
       }
     }
   if (status == UU_SUCCESS)
     {
     for (i=1,pi=ptve+1; i<m; i++,pi++)
       {
       if (pi->inv == 0)
         {
         hmov = sqrt (pi->dx*pi->dx+pi->dy*pi->dy+pi->dz*pi->dz);
         if (hmov > htol)
           {
           ro = ftol/hmov;
           if (ro > 1.0) ro = 1.0;
           pi->x = pi->x+pi->dx*ro;
           pi->y = pi->y+pi->dy*ro;
           pi->z = pi->z+pi->dz*ro;
           }
         }
       }
     imode = 2;
     status = ncl_slpset (n, ptve, imode);
     }

   if (status == UU_SUCCESS)
     {
     hldseg = (struct NCL_crvgen_rec *) uu_malloc(n*sizeof(*hldseg));
     ph = hldseg;
     pi = ptve;
     *ph = *pi;
     ph++;
     ih = 1;
     pj = pi+2;
     k = 2;
     if (hldseg == 0) status = UU_FAILURE;
     }
   while (status == UU_SUCCESS && k<n)
     {
     status = ncl_segchk (&xc,&yc,&zc,pi,pj,imode);
     if (status == UU_SUCCESS)
       {
       otol = fabs(xc)+fabs(yc)+fabs(zc) > ftol;
       if (otol || pj->inv == 1)
         {
         if (otol)
           {
           pj--;
           k--;
           }
         *ph = *pj;
         ph++;
         ih++;
         pi = pj;
         pj += 2;
         k += 2;
         }
       else
         {
         pj++;
         k++;
         }
       }
     }
   if (status == UU_SUCCESS)
     {
     if (k < n+1)
       {
       *ph = *(pj-1);
       ih++;
       }
     for (i=0,pi=ptve,ph=hldseg; i<ih; i++,pi++,ph++)
       {
       pi->x = ph->x;
       pi->y = ph->y;
       pi->z = ph->z;
       pi->a = ph->a;
       pi->b = ph->b;
       pi->c = ph->c;
       if (i<ih-1)
         {
         pj = ph+1;
         pi->dx = pj->x-ph->x;
         pi->dy = pj->y-ph->y;
         pi->dz = pj->z-ph->z;
         pi->ch = sqrt(pi->dx*pi->dx+pi->dy*pi->dy+pi->dz*pi->dz);
         }
       }
     }

   *npts = ih;
   if (hldseg != 0) uu_free (hldseg);

   uu_dexitstatus("ncl_crvfit ()", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_segchk (xc,yc,zc,pi,pk,imode)
**       Check points lying between 2 curve segment points for tolerance.
**    PARAMETERS   
**       INPUT  : 
**          pi           pointer to 1st curve segment.
**          pk           pointer to last curve segment
**          imode        =1 - return near point on seg
**                       =2 - return max delta between interior points & curve seg
**       OUTPUT :  
**          xc           x value of output
**          yc           y value of output
**          zc           z value of output
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_segchk (xc,yc,zc,pi,pk,imode)
   UU_REAL *xc, *yc, *zc;
   struct NCL_crvgen_rec *pi, *pk;
   int imode;
   {
   int itim, status;
   UU_REAL ctan, hdx, hdy, hdz, dlx,dly,dlz, cc,cal,cbe,adis,bdis;
   UU_REAL xq,yq,zq,xr,yr,zr,c1,c2,c3,xa,ya,za,xb,yb,zb;
   UU_REAL u, du, uerr,oerr, den;
   struct NCL_crvgen_rec *pj;

   uu_denter(UU_MTRC,(us,"ncl_segchk ()"));

   status = UU_SUCCESS;
   ctan = 1.0;
   hdx = 0.0;
   hdy = 0.0;
   hdz = 0.0;
   pj = pi+1;
   dlx = pk->x - pi->x;
   dly = pk->y - pi->y;
   dlz = pk->z - pi->z;
   cc = sqrt(dlx*dlx+dly*dly+dlz*dlz);
   cal = (dlx*pi->a+dly*pi->b+dlz*pi->c)/cc;
   cbe = (dlx*pk->a+dly*pk->b+dlz*pk->c)/cc;
   adis = .666667*cc/(1.0+cal);
   bdis = .666667*cc/(1.0+cbe);
   if (adis>bdis) adis = bdis*(2.0-bdis/adis);
   if (bdis>adis) bdis = adis*(2.0-adis/bdis);
   xq = pi->x+pi->a*adis;
   yq = pi->y+pi->b*adis;
   zq = pi->z+pi->c*adis;
   xr = pk->x-pk->a*bdis;
   yr = pk->y-pk->b*bdis;
   zr = pk->z-pk->c*bdis;

   while (pj < pk)
     {
     for (itim=0, u=0.5; itim<10; itim++)
       {
       c1 = 1.0-u;
       c1 = c1*c1;
       c2 = 2.0*(1.0-u)*u;
       c3 = u*u;
       xa = c1*pi->x+c2*xq+c3*xr;
       ya = c1*pi->y+c2*yq+c3*yr;
       za = c1*pi->z+c2*zq+c3*zr;
       xb = c1*xq+c2*xr+c3*pk->x-xa;
       yb = c1*yq+c2*yr+c3*pk->y-ya;
       zb = c1*zq+c2*zr+c3*pk->z-za;
       uerr = (xb*(pj->x-xa)+yb*(pj->y-ya)+zb*(pj->z-za))/(xb*xb+yb*yb+zb*zb)-u;
       uerr = uerr/3.0;
       if (fabs(uerr)<1.0e-5)
         itim = 10;
       else
         {
         if (itim > 0) 
           {
           den = oerr-uerr;
           if (fabs(den)>1.0e-5)
             {
             ctan = du/den;
             if (ctan<.1) ctan = 1.0;
             }
           }
         du = uerr*ctan;
         if (du+u > 1.0) du = 1.0-u;
         if (du+u < 0.0) du = -u;
         u = u+du;
         oerr = uerr;
         }
       }
     *xc = xa+u*xb;
     *yc = ya+u*yb;
     *zc = za+u*zb;
     if (imode == 2)
       {
       dlx = *xc-pj->x;
       dly = *yc-pj->y;
       dlz = *zc-pj->z;
       if (fabs(dlx) > fabs(hdx)) hdx = dlx;
       if (fabs(dly) > fabs(hdy)) hdy = dly;
       if (fabs(dlz) > fabs(hdz)) hdz = dlz;
       }
     pj++;
     } 
   if (imode == 2)
     {
     *xc = hdx;
     *yc = hdy;
     *zc = hdz;
     }

   uu_dexitstatus("ncl_segchk ()", status);
   return(status);
   }


/*********************************************************************
..... Local function - a constructor for struct NCL_crvgen_rec
*********************************************************************/
int ncl_init_seg (seg)
struct NCL_crvgen_rec *seg;
{
	seg->x = 0.;
	seg->y = 0.;
	seg->z = 0.;
	seg->a = 0.;
	seg->b = 0.;
	seg->c = 0.;
	seg->dx = 0.;
	seg->dy = 0.;
	seg->dz = 0.;
	seg->ch = 0.;
	seg->inv = 0;

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_copy_seg (pi, pj)
**       Copy crvgen segment.
**    PARAMETERS   
**       INPUT  : 
**          pi         - Pointer to source curve segment.
**       OUTPUT :  
**          pj         - Pointer to destination curve segment.
**    RETURNS      : none 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* void */ 
void ncl_copy_seg (pi, pj)
struct NCL_crvgen_rec *pi, *pj;
   {

   uu_denter(UU_MTRC,(us,"ncl_copy_seg ()"));

   pj->x = pi->x;
   pj->y = pi->y;
   pj->z = pi->z;
   pj->a = pi->a;
   pj->b = pi->b;
   pj->c = pi->c;
   pj->dx = pi->dx;
   pj->dy = pi->dy;
   pj->dz = pi->dz;
   pj->ch = pi->ch;
   pj->inv = pi->inv;

   uu_dexit;
   return;
   }

