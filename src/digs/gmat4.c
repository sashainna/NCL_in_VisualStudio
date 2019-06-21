/*********************************************************************
**    NAME         :  gmat4.c -- 4x4 matrix and vector routines.
**						Right handed coordinate system assumed. A positive 
**						rotation angle is such that, when looking
**						from a positive axis toward the origin, a 90 degree
**						counterclockwise rotation will transform one positive
**						axis into the other.
**       CONTAINS:
**		int ug_mident (a) -- 1 if a==identity, else 0.
**		ug_matmp(c,a,b) -- 4x4 matrix multiply. c=a*b.
**		ug_xform(x,y,z,npos,a) -- npos=(x,y,z,1)*a matrix.
**		ug_vecxform(newvec,vec,a) -- newvec=vec xformed by a.
**		int ug_invrt(ainv,a) -- ainv = inverse of a.
**		ug_efa(a,ipvt,info) -- factors a by gaussian elim.
**		ug_edi(a,ipvt) -- compute inverse of a.
**		ug_rot(angx,angy,angz,a) -- multiply a (on the right).
**		grotx(angx,b) -- calc an x-axis rotation matrix.
**		groty(angy,b) -- calc an y-axis rotation matrix.
**		grotz(angz,b) -- calc an z-axis rotation matrix.
**		ug_scale(sx,sy,sz,a) 
**		ug_trans(tx,ty,tz,a)
**		ug_ident(a) -- set a to identity matrix.
**		ug_matprt(a) -- print matrix a on debug file.
**		ug_matcmp(a,b) -- compare 2 matrices. return -1,0,1
**		Gfloat ug_atan2(num, den) -- returns arc tangent in correct quadrant.
**		ug_ztovec(a, vec) -- creates two rotations in a, which 
**									align z-axis with vec.
**		ug_vectoz(a, vec) -- creates two rotations in a, 
**									which align vec with z-axis.
**		ug_vecrotx(vec, phix) -- rotate vector around x axis.
**		ug_vcroty( vec, phiy ) -- rotate vector around y axis.
**		ug_vcrotz( vec, phiz ) -- rotate vector around z axis.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gmat4.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:21
*********************************************************************/
#include <stdio.h>
#include "umath.h"
#include "usysdef.h"
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "ginq.h"
#include "udebug.h"
#include "gmat4.h"
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gmat4.c 3.5 7/20/88 15:45:03 single"};
#else
static char uu_sccsident[]={"@(#) gmat4.c 3.5 7/20/88 15:45:03 double"};
#endif
#define true 1
#define false 0
#define logical int


/*********************************************************************
**    I_FUNCTION     :  int ug_mident (a) -- 1 if a==identity, else 0.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_mident (a)						/* 1 if a=identity, else 0 */
Gfloat a[4][4];
{
	static Gfloat b[4][4]={1.,0.,0.,0.,0.,1.,0.,0.,0.,0.,1.,0.,
								0.,0.,0.,1.};	/* identity */
	int i,j;
	for (i=0; i<4; i++) {
		for (j=0; j<4; j++) {
			if (a[i][j]!=b[i][j]) return(0);
		}
	}
	return(1);
}

/*********************************************************************
**    I_FUNCTION     :  ug_matmp(c,a,b) -- 4x4 matrix multiply. c=a*b.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_matmp(c,a,b)            /* 4x4 matrix multiply. c=a*b */
Gfloat c[4][4],a[4][4],b[4][4];
{
  int i,j;
	 uu_denter(UU_GITRC,(us,"ug_matmp(c,a,b)"));
	for (i=0; i<4; i++) {
    for (j=0; j<4; j++)
      c[i][j]=a[i][0]*b[0][j]+a[i][1]*b[1][j]+a[i][2]*b[2][j]+
              a[i][3]*b[3][j];
  }
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_xform(x,y,z,npos,a) -- npos=(x,y,z,1)*a matrix.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_xform(x,y,z,npos,a)           /* calc npos=(x,y,z,1)*a matrix */
Gfloat x,y,z,a[4][4];
Gnpoint3 *npos;
{ int i;
	char us[120];
	uu_denter2(UU_GITRC,(us,"ug_xform(%g %g %g,npos,a)",x,y,z));
	UG_XFORM(x,y,z,npos,a);
	uu_dprint(UU_GITRC,(us,"ug_xform npos=%g %g %g.\n",
		(*npos).x,(*npos).y,(*npos).z));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_vecxform(newvec,vec,a)
**       newvec = vec xformed by a.
**    PARAMETERS   
**       INPUT  : 	Gfloat vec[3]; -- vector to be xformed.
**							Gfloat a[4][4] -- xformation matrix.
**       OUTPUT :  	Gvloat newvec[3] -- resulting vector (vec * a)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_vecxform(newvec,vec,a)				/* xform a vector (not a point) */
Gfloat newvec[3],vec[3],a[4][4];
{
	Gfloat newtail[3];
	Gfloat newhead[3];
	int i;
	char us[140];
	
	ug_xform(vec[0],vec[1],vec[2],newhead,a);	/* xform head of vec */
	ug_xform((UU_REAL) 0.,(UU_REAL) 0.,(UU_REAL) 0.,newtail,a);					/* xform origin */
	for (i=0; i<3; i++) newvec[i]=newhead[i]-newtail[i];	/* subtract */
	uu_denter2(UU_GITRC,(us,"ug_vecxform(%g %g %g,a) gives %g %g %g",
		vec[0],vec[1],vec[2],newvec[0],newvec[1],newvec[2]));
	uu_dexit;
}
/*********************************************************************
**    I_FUNCTION     :  int ug_invrt(ainv,a) -- ainv = inverse of a.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_invrt(ainv,a)               /* ainv = inverse of a */
                       /* return 0 if inverse doesnt exist. else 1 */
Gfloat ainv[4][4],a[4][4];
{
  int i,j;
  int ipvt[4],info;
   uu_denter(UU_GITRC,(us,"ug_invrt(ainv,a)"));
  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) ainv[i][j]=a[i][j];
  }
  ug_efa(ainv,ipvt,&info);
  if (info!=0) { uu_dexit; return(0);}
  else ug_edi(ainv,ipvt);      /* ug_efa returned ok. */
  { uu_dexit; return(1);}
}

/*********************************************************************
**    I_FUNCTION     :  ug_efa(a,ipvt,info) -- factors a by gaussian elim.
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_efa(a,ipvt,info)            /* factors a by gaussian elim */
Gfloat a[4][4];                 /* matrix to be factored */
int ipvt[4];                   /* returned pivot indices */
int *info;                      /* returned =0 or =k if u[k][k]=0.0 */
{
/* this translated from the linpack routine sgefa */
/* gaussian elimination with partial pivoting */
  int i,j,k,l;
  Gfloat big,t;
   uu_denter(UU_GITRC,(us,"ug_efa(a,ipvt,info)"));
  (*info)=0;
  for (k=0; k<3; k++) {
    /* find l=pivot index */
    /* was l=isamax(n-k+1,a[k,k],1)+k-1  */
    big=fabs(a[k][k]);  l=k;
    for (i=k+1; i<4; i++) {
      if (fabs(a[i][k])>big) { l=i; big=fabs(a[i][k]); }
    }
    ipvt[k]=l;
    /* zero pivot implies this column already triangularized */
    if (a[l][k]!=0.0) {        /* column not already triang */
      /* interchange if necessary */
      if (l!=k) { t=a[l][k]; a[l][k]=a[k][k]; a[k][k]=t; }
      /* compute multipliers */
      t=(-1.0)/a[k][k];
      for (i=k+1; i<4; i++) {       /* was sscal(n-k,t,a[k+1][k],1) */
        a[i][k]=a[i][k]*t;
      }
      /* row elimination with column indexing */
      for (j=k+1; j<4; j++) {
        t=a[l][j];
        if (l!=k) { a[l][j]=a[k][j]; a[k][j]=t; }
        for (i=k+1; i<4; i++) {  /* was saxpy(n-k,t,a[k+1][k],1,a[k+1][j],1)*/
          a[i][j]=t*a[i][k]+a[i][j];
        }
      }                          /* for j */
    }                            /* if not already triang */
    else (*info)=k;                 /* already triang */
  }                              /* for k */
  ipvt[3]=3;
  if (a[3][3]==0.0) (*info)=3;
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_edi(a,ipvt) -- compute inverse of a.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_edi(a,ipvt)                   /* compute inverse of a */
Gfloat a[4][4];                 /* output from ug_efa */
int ipvt[4];                   /* pivot vector from ug_efa */
{ Gfloat tmp,work[4],t;
  int i,j,k,l;
   uu_denter(UU_GITRC,(us,"ug_edi(a,ipvt)"));
  /* compute inverse(u) */
  for (k=0; k<4; k++) {
    a[k][k]=1.0/a[k][k];
    t=(-a[k][k]);
    for (i=0; i<=k-1; i++) { /* sscal(k-1,t,a[1,k],1) */
      a[i][k]=a[i][k]*t;
    }
    for(j=k+1; j<4; j++) {      /* assume 0-trip loop is possible */
      t=a[k][j]; a[k][j]=0.0;
      for (i=0; i<=k; i++) {   /* was saxpy(k,t,a[1,k],1,a[1,j],1) */
        a[i][j]=t*a[i][k]+a[i][j];
      }
    }                                 /* for j */
  }                                   /* for k */
  /* form inverse(u)*inverse(l) */
  for (k=2; k>=0; k=k-1) {
    for (i=k+1; i<4; i++) {
      work[i]=a[i][k];
      a[i][k]=0.0;
    }
    for (j=k+1; j<4; j++) {
      t=work[j];
      for (i=0; i<4; i++) {  /* was saxpy(n,t,a[1,j],1,a[1,k],1) */
        a[i][k]=t*a[i][j]+a[i][k];
      }
    }
    l=ipvt[k];
    if (l!=k) {
      for (i=0; i<4; i++) {  /* was sswap(n,a[1,k],1,a[1,l],1) */
       tmp=a[i][l]; a[i][l]=a[i][k]; a[i][k]=tmp;
      }
    }
  }                                    /* for k */
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_rot(angx,angy,angz,a) -- multiply a (on the right).
**    PARAMETERS   
**       INPUT  :  Gfloat angx,angy,angz -- rotation angles (radians).
**						 Gfloat a[4][4] -- matrix to be updated.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_rot(angx,angy,angz,a)         /* multiply a (on the right)
                                by matrices for rotations along x,y,z axes */
Gfloat angx,angy,angz;
Gfloat a[4][4];
{
  Gfloat b1[4][4],b2[4][4],b3[4][4];		/* temporary matrices */
  uu_denter(UU_GITRC,(us,"ug_rot(%g,%g,%g,a)",angx,angy,angz));
  grotx(angx,b1);					/* first, rotate about x axis */
  ug_matmp(b2,a,b1);				/* b2 = a x b1 */
  groty(angy,b1);					/* now rotate about y axis */
  ug_matmp(b3,b2,b1);				/* b3 = b2 x b1 */
  grotz(angz,b1);					/* now rotate about z axis */
  ug_matmp(a,b3,b1);				/* a = b3 x b1 */
  uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  grotx(angx,b) -- rotate about x axis 
**    PARAMETERS   
**       INPUT  : 		Gfloat angx; -- angle (radians)
**       OUTPUT :  		Gfloat b[4][4]; -- rotation matrix.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
grotx(angx,b)					/* rotate about x axis */
/*$ INPUT */
Gfloat angx;
/*$ OUTPUT */
Gfloat b[4][4];					/* output matrix, a rotated by angx */
{
	int i,j;
	uu_denter(UU_GITRC,(us,"ug_rotx(%g)",angx));
	for (i=0; i<4; i++) {
		for (j=0; j<4; j++) {
			b[i][j]=0.0;
		}
	}
	b[3][3]=1.0;
	b[0][0]=1.0;
	b[1][1]=cos(angx);
	b[2][2]=b[1][1];
	b[1][2]=sin(angx);
	b[2][1]=(-b[1][2]);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  groty(angy,b) -- rotate about y axis 
**    PARAMETERS   
**       INPUT  : 		Gfloat angy; -- angle (radians)
**       OUTPUT :  		Gfloat b[4][4]; -- output matrix.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
groty(angy,b)					/* rotate about y axis */
/*$ INPUT */
Gfloat angy;
/*$ OUTPUT */
Gfloat b[4][4];					/* output rotation matrix */
{
	int i,j;
	uu_denter(UU_GITRC,(us,"groty(%g)",angy));
	for (i=0; i<4; i++) {
		for (j=0; j<4; j++) {
			b[i][j]=0.0;
		}
	}
	b[3][3]=1.0;
	b[1][1]=1.0;
	b[0][0]=cos(angy);
	b[2][2]=b[0][0];
	b[2][0]=sin(angy);
	b[0][2]=(-b[2][0]);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  grotz(angz,b) -- rotate about z axis 
**    PARAMETERS   
**       INPUT  : 		Gfloat angz; -- angle (radians)
**       OUTPUT :  		Gfloat b[4][4]; -- output matrix.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
grotz(angz,b)					/* rotate about z axis */
/*$ INPUT */
Gfloat angz;
/*$ OUTPUT */
Gfloat b[4][4];					/* output matrix, a rotated by angz */
{
	int i,j;
	Gfloat b1[4][4];
	uu_denter(UU_GITRC,(us,"grotz(%g)",angz));
	for (i=0; i<4; i++) {
		for (j=0; j<4; j++) {
			b[i][j]=0.0;
		}
	}
	b[2][2]=1.0; b[3][3]=1.0;
	b[0][0]=cos(angz);
	b[1][1]=b[0][0];
	b[0][1]=sin(angz);
	b[1][0]=(-b[0][1]);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_scale(sx,sy,sz,a) 
**			Compound matrix a by scale factors.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_scale(sx,sy,sz,a)              /* compound matrix a by scale factors */
Gfloat sx,sy,sz;
Gfloat a[4][4];
{
  int i;
	 uu_denter(UU_GITRC,(us,"ug_scale(%g,%g,%g,a)",sx,sy,sz));
  for (i=0; i<4; i++) {
    a[i][0]=a[i][0]*sx;
    a[i][1]=a[i][1]*sy;
    a[i][2]=a[i][2]*sz;
  }
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_trans(tx,ty,tz,a)
**			Compound matrix a by translations.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_trans(tx,ty,tz,a)              /* compound matrix a by translations */
Gfloat tx,ty,tz;
Gfloat a[4][4];
{
  int i;
	 uu_denter(UU_GITRC,(us,"ug_trans(%g,%g,%g,a)",tx,ty,tz));
  for (i=0; i<4; i++) {
    a[i][0]=a[i][0]+a[i][3]*tx;
    a[i][1]=a[i][1]+a[i][3]*ty;
    a[i][2]=a[i][2]+a[i][3]*tz;
  }
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_ident(a) -- set a to identity matrix.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_ident(a)                 /* set a to identity matrix */
Gfloat a[4][4];
{ 
	int i,j;
	 uu_denter(UU_GITRC,(us,"ug_ident(a)"));
	for (i=0; i<4; i++) {
    for (j=0; j<4; j++) a[i][j]=0.0;
  }
  for (i=0; i<4; i++) a[i][i]=1.0;
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_matprt(a) -- print matrix a on debug file.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_matprt(a)                         /* print matrix a on debug file */
Gfloat a[4][4];
{
  int i;
  char us[180];
  for (i=0; i<4; i++) {
    uu_denter2(UU_GITRC,(us,"     %g %g %g %g",a[i][0],a[i][1],a[i][2],a[i][3]));
	 uu_dexit;
  }
}

/*********************************************************************
**    I_FUNCTION :  ug_matcmp(a,b) -- compare 2 matrices.
**    PARAMETERS   
**       INPUT  : 	Gfloat a[4][4];
**							Gfloat b[4][4];
**       OUTPUT :  
**    RETURNS      :  0 if a=b. else 1.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_matcmp(a,b)
Gfloat a[4][4],b[4][4];
{
#define EPS (UU_REAL) .00001	
	int i,j,irtn;
	irtn=0;
	for (i=0; i<4; i++) {
		for (j=0; j<4; j++) {
			if (fabs(a[i][j]-b[i][j])>EPS)  return(1);
		}
	}
	return(0);
}

/*********************************************************************
**    I_FUNCTION     :  Gfloat ug_atan2( num, den )
**
**		Returns arctangent of (num/den) in correct quadrent.
**       
**    PARAMETERS   
**       INPUT  : 
**          num			opposite side of triangle
**				den			adjacent side of triangle
**       OUTPUT :  
**          none
**    RETURNS      : Arctangent of (num/den) in range -pi to pi. 
**							Zero if den is zero.
**    SIDE EFFECTS : none
**    WARNINGS     : Must declare this as Gloat 
**							in functions which use it.
*********************************************************************/
Gfloat ug_atan2( num, den )
Gfloat num, den;
{

	Gfloat value;								/* Value returned */
	static Gfloat eps = 1.0e-6;
	static Gfloat pi = 3.141592654;

	uu_denter(UU_GITRC,(us,"ug_atan2(%f %f)",num,den));

	/* First check for zero in denominator */
	if( fabs(den) < eps ) {

		/* If zero in numerator also, return zero */
		if(fabs(num) < eps) 	{
			value = 0.0;
		}
		else {	/* Only zero denominator */

			if( num > 0.0 ) value =  pi/2.0;
			else				 value = -pi/2.0;

		}

	}

	/* Non-zero numerator and denominator */
	else  {
		value = atan2(num, den);
	}

	uu_dprint(UU_GITRC,(us,"ug_atan2 returns %f",value));
	uu_dexit;
	return(value);

}

/*********************************************************************
**    I_FUNCTION     :  ug_vectoz( a, vec )
**
**		Applys two rotations to matrix "a", which rotate vec into the
**		z-axis.  
**
**		The first rotation is about y which rotates "vec" into the yz plane.  
**
**		The second rotation, rotates this projected vector about the x-axis 
**		onto the z-axis.
**
**    PARAMETERS   
**       INPUT  : 
**          a				Matrix to apply rotations on.
**				vec			Vector to rotate into z-axis.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_vectoz(a, vpn)
Gtran a;
Gwpoint3 *vpn;
{

Gfloat r;
Gfloat phix, phiy;
Gfloat ug_atan2();

uu_denter(UU_GITRC,(us,"ug_vectoz %f %f %f",vpn->x,vpn->y,vpn->z));

	/* Calculate angles phix, phiy... rotations to set vpn on z-axis */

	/* r = projected length in xz plane */
	r = sqrt(vpn->x*vpn->x + vpn->z*vpn->z);

	/* Calc y-axis rotation angle phix */
	phiy = -ug_atan2( vpn->x, vpn->z );

	/* Calc x-axis rotation angle */
	phix = ug_atan2( vpn->y, r );

	uu_denter2(UU_GITRC,(us,"ug_vectoz: phix %f, phiy %f (deg)",
		phix*180./3.1415,phiy*180./3.1415)); uu_dexit;

	/* Apply the rotations to a */
	ug_rot((UU_REAL) 0.0 , phiy, (UU_REAL) 0.0, a);
	ug_rot(phix,  (UU_REAL) 0.0, (UU_REAL) 0.0, a);

	uu_dexit;

}



/*********************************************************************
**    I_FUNCTION     :  ug_ztovec( a, vec )
**
**		Applys two rotations to matrix "a", which rotate the z-axis into
**		the vector vec.  These rotations are the opposite of the rotations
**		which would rotate "vec" onto the z-axis.  These opposite rotations
**		are calculated as follows:
**
**		The first rotation is about y which rotates "vec" into the yz plane.  
**
**		The second rotation, rotates this projected vector about the x-axis 
**		onto the z-axis.
**
**		Finally, these rotations are applied in reverse order and sign to
**		yield the desired effect.
**       
**    PARAMETERS   
**       INPUT  : 
**          a				Matrix to apply rotations on.
**				vec			Vector to rotate z-axis into.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_ztovec(a, vpn)
Gtran a;
Gwpoint3 *vpn;
{

Gfloat r;
Gfloat phix, phiy;
Gfloat ug_atan2();

uu_denter(UU_GITRC,(us,"ug_ztovec %f %f %f",vpn->x,vpn->y,vpn->z));
				
	/* r = projected length in xz plane */
	r = sqrt(vpn->x*vpn->x + vpn->z*vpn->z);

	/* Calc y-axis rotation angle */
	phiy = -ug_atan2( vpn->x, vpn->z );

	/* Calc x-axis rotation angle */
	phix = ug_atan2( vpn->y, r );

	uu_denter2(UU_GITRC,(us,"ug_ztovec: phix %f, phiy %f (deg)",
		-phix*180./3.1415,-phiy*180./3.1415)); uu_dexit;

	/* Apply the rotations to a in reverse order and sign */
	ug_rot(-phix , -phiy, (UU_REAL) 0.0, a);

	uu_dexit;

}

/*********************************************************************
**    I_FUNCTION     : ug_vcrotx( vec, phix )
**
** 	Rotate vec by phix around x-axis.
**       
**    PARAMETERS   
**       INPUT  : 
**				vec			Vector to rotate by angle phix.
**				phix			Angle of rotation.
**       OUTPUT :  
**				vec			Rotated vector.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_vcrotx( vec, phix )
Gfloat vec[3];
Gfloat phix;
{
	Gfloat c, s;		/* Cosine and sine of phix */

	uu_denter(UU_GITRC,(us,"ug_vcrotx: angle %f, vec %f %f %f",
		phix, vec[0], vec[1], vec[2]));

	c = cos(phix);
	s = sin(phix);

	vec[1] =  c * vec[1] - s * vec[2];
	vec[2] =  s * vec[1] + c * vec[2];

	uu_dprint(UU_GITRC,(us,"ug_vcrotx: returns vec %f %f %f",
		vec[0], vec[1], vec[2]));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     : ug_vcroty( vec, phiy )
**
** 	Rotate vec by phiy around y-axis.
**       
**    PARAMETERS   
**       INPUT  : 
**				vec			Vector to rotate by angle phiy.
**				phiy			Angle of rotation.
**       OUTPUT :  
**				vec			Rotated vector.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_vcroty( vec, phiy )
Gfloat vec[3];
Gfloat phiy;
{
	Gfloat c, s;		/* Cosine and sine of phiy */

	uu_denter(UU_GITRC,(us,"ug_vcroty: angle %f, vec %f %f %f",
		phiy, vec[0], vec[1], vec[2]));

	c = cos(phiy);
	s = sin(phiy);

	vec[0] =  c * vec[0] + s * vec[2];
	vec[2] = -s * vec[0] + c * vec[2];
		
	uu_dprint(UU_GITRC,(us,"ug_vcroty: returns vec %f %f %f",
		vec[0], vec[1], vec[2]));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     : ug_vcrotz( vec, phiz )
**
** 	Rotate vec by phiz around z-axis.
**       
**    PARAMETERS   
**       INPUT  : 
**				vec			Vector to rotate by angle phiz.
**				phiz			Angle of rotation.
**       OUTPUT :  
**				vec			Rotated vector.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_vcrotz( vec, phiz )
Gfloat vec[3];
Gfloat phiz;
{
	Gfloat c, s;		/* Cosine and sine of phiz */

	uu_denter(UU_GITRC,(us,"ug_vcrotz: angle %f, vec %f %f %f",
		phiz, vec[0], vec[1], vec[2]));

	c = cos(phiz);
	s = sin(phiz);

	vec[0] =  c * vec[0] - s * vec[1];
	vec[1] =  s * vec[0] + c * vec[1];
		
	uu_dprint(UU_GITRC,(us,"ug_vcrotz: returns vec %f %f %f",
		vec[0], vec[1], vec[2]));
	uu_dexit;
}
