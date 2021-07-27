
/*********************************************************************
**    NAME         :  m7mathtf.c
**       CONTAINS:
**			um_determinant(tfmat,det)
**			um_disptf( delta, tfr )
**			um_scaletf( scale, tfr )
**			um_rottf( axis, angle, tfr )
**			um_nodisptf( tfi, tfo )
**			um_tftotf( tfi, tfo )
**			um_tftmtf( tf1, tf2, tfr )
**			um_is_idmat(tfmat)
**			um_is_rottf(tfmat)
**			um_tfeqtf(tf1, tf2)
**			um_identtf( tfo )
**			um_cplntf(tfr)
**			um_chgcstf(o1, x1, y1, z1, o2, x2, y2, z2, tfmat)
**			um_rotlntf(pt, vec, ang, rottf)
**			int um_inverttf(a, ainv)
**			umi_efa(a,ipvt,info) 
**			umi_edi(a,ipvt) 
**			um_scale_in_tf(tfmat) 
**			um_distorting_tf(tfmat,scale)
**			um_ptxz_tf(tfmat)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m7mathtf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:10
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "modef.h"

/*********************************************************************
**    E_FUNCTION     :	um_disptf( delta, tfr )
**       Return the 4X3 translation transformation for a given 
**			delta = (dx,dy,dz).
**    PARAMETERS   
**       INPUT  : 
**				delta			delta vector
**       OUTPUT :  
**          tfr			resulting transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_disptf( delta, tfr )
	UM_coord		delta;
	UM_transf	tfr;
	{
	int i,j;

	for (i=0; i<3; i++)
		for (j=0; j<3; j++) tfr[i][j] = UM_idmat[i][j];
	for (j=0; j<3; j++) tfr[3][j] = delta[j];
	}

/*********************************************************************
**    E_FUNCTION     : um_scaletf( scale, tfr )
**       Return the 4X3 scaling transformation given a scale factor
**			for scale = (sx, sy, sz).
**    PARAMETERS   
**       INPUT  : 
**          scale			vector of scale factors
**       OUTPUT :  
**				tfr			resulting transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_scaletf( scale, tfr )
	UU_REAL		scale[3];
	UM_transf	tfr;

	{
	int i,j;

	for (i=0; i<4; i++)
		for (j=0; j<3; j++) tfr[i][j] = UM_idmat[i][j];
	for (j=0; j<3; j++) tfr[j][j] = scale[j];
	}

/*********************************************************************
**    E_FUNCTION     : um_rottf( axis, angle, tfr )
**       Return the 4X3 rotation transformation given an
**				axis vector and a rotation angle.
**    PARAMETERS   
**       INPUT  : 
**          axis			axis vector
**				angle			rotation angle
**       OUTPUT :  
**          tfr			resulting transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_rottf( axis, angle, tfr )
	UM_vector	axis;
	UM_angle		angle;
	UM_transf		tfr;

	{
   UM_vector unvec;       /* unit vector in direction of dir */
   UU_REAL   a, b, c;        /* Direction cosines of dir */
   UU_REAL   vsq;            /* b*b + c*c */
   UU_REAL   sine, cosine;   /* of angle */

   um_unitvc(axis,unvec);
   a = unvec[0];
   b = unvec[1];
   c = unvec[2];
   vsq = b*b + c*c;
   sine = sin(angle);
   cosine = cos(angle);
   if (vsq < UM_DFUZZ)    /* x-axis rotation */
		{
       tfr[0][0] = 1.0;
       tfr[0][1] = tfr[0][2] = tfr[1][0] = tfr[2][0] = 0.0;
       tfr[1][1] = tfr[2][2] = cosine;
       tfr[2][1] = -(tfr[1][2] = a*sine);
		}
   else
		{
       tfr[0][0] = vsq * cosine  +  a*a;
       tfr[1][1] = (a*a * b*b  +  c*c) * cosine / vsq  +  b*b; 
       tfr[2][2] = (a*a * c*c  +  b*b) * cosine / vsq  +  c*c;
       tfr[0][1] = (1.0 - cosine) * a * b   +   c * sine;
       tfr[1][0] = (1.0 - cosine) * a * b   -   c * sine; 
       tfr[0][2] = (1.0 - cosine) * a * c   -   b * sine;
       tfr[2][0] = (1.0 - cosine) * a * c   +   b * sine; 
       tfr[1][2] = (a*a - 1) * b * c * cosine / vsq  +  a*sine  +  b*c;
       tfr[2][1] = (a*a - 1) * b * c * cosine / vsq  -  a*sine  +  b*c; 
		}
   tfr[3][0] = tfr[3][1] = tfr[3][2] = 0.0;
	}

/*********************************************************************
**    E_FUNCTION     : um_nodisptf( tfi, tfo )
**			Get the transformation with no displacement
**    PARAMETERS   
**       INPUT  : 
**				tfi			transformation
**       OUTPUT :  
**          tfo			transformation with no displacement
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_nodisptf( tfi, tfo )
	UM_transf	tfi;
	UM_transf	tfo;

	{
	int i,j;

	/* if not the same transformation ... */
	if (tfi == UM_DEFAULT_TF)
		um_identtf(tfo);
	else if( tfi != tfo )
		{
		for (i=0; i<3; i++)
			for (j=0; j<3; j++) tfo[i][j] = tfi[i][j];
		}

	for (j=0; j<3; j++) tfo[3][j] = 0.0;
	}

/*********************************************************************
**    E_FUNCTION     : um_tftotf( tfi, tfo )
**			Copy transformation
**    PARAMETERS   
**       INPUT  : 
**				tfi			transformation
**       OUTPUT :  
**          tfo			copied transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_tftotf( tfi, tfo )
	UM_transf	tfi;
	UM_transf	tfo;

	{
	int i,j;

	if (tfi == UM_DEFAULT_TF)
		um_identtf(tfo);
	else
		for (i=0; i<4; i++)
			for (j=0; j<3; j++)
				tfo[i][j] = tfi[i][j];
	}

/*********************************************************************
**    E_FUNCTION     :	um_tftmtf( tf1, tf2, tfr )
**       Multiply two 4X3 transformations matrices and return the
**			result tfr = tf1 * tf2. The resultant matrix may be identical
**			to either of the input transformations.
**    PARAMETERS   
**       INPUT  : 
**          tf1					first transformation
**				tf2					second transformation
**       OUTPUT :  
**          tfr					resultant transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_tftmtf( tf1, tf2, tfr )
	UM_transf	tf1;
	UM_transf	tf2;
	UM_transf	tfr;

	{
	int i,j,k;
	UM_transf temp;

	if (tf1 == UM_DEFAULT_TF)
		{
		if (tf2 == UM_DEFAULT_TF)
			um_identtf(tfr);
		else
			um_tftotf(tf2, tfr);
		goto Done;
		}
	else if (tf2 == UM_DEFAULT_TF)
		{
		um_tftotf(tf1, tfr);
		goto Done;
		}

	for (i=0; i<3; i++)
		for (j=0; j<3; j++)
			{
			temp[i][j] = 0.0;
			for (k=0; k<3; k++) temp[i][j] = temp[i][j] + (tf1[i][k]*tf2[k][j]);
			}

	for (j=0; j<3; j++)
		{
		temp[3][j] = tf2[3][j];
		for (k=0; k<3; k++) temp[3][j] = temp[3][j] + (tf1[3][k]*tf2[k][j]);
		}

	for (i=0; i<4; i++)
		for (j=0; j<3; j++)
			tfr[i][j] = temp[i][j];
Done:
	return;
	}

/*********************************************************************
**    E_FUNCTION : UU_LOGICAL  um_scale_in_tf(tfmat) 
**       Determines whether the transformation contains a scale factor
**			other than 1 in one of the image directions.
**    PARAMETERS   
**       INPUT  : 
**          tfmat			transformation to determine whether there is a scalar
**								factor.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_TRUE iff a scale factor exists.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_scale_in_tf(tfmat)
UM_transf tfmat;
{
	UU_LOGICAL scales;
	int i;

	scales = UU_FALSE;
	if (tfmat != UM_DEFAULT_TF)
	{
		for (i=0; (i<3 && !scales); i++)
		if (fabs(1.0 - um_mag(tfmat[i])) >= UM_FUZZ)
			scales = UU_TRUE;
	}
	return(scales);
}

/*********************************************************************
**    E_FUNCTION : UU_LOGICAL  um_is_rottf(tfmat,flag) 
**       Determines whether the transformation contains a rotation.
**    PARAMETERS   
**       INPUT  : 
**          tfmat			Transformation to determine whether there is a
**								rotation.
**          flag        UU_TRUE = Consider 90 degree rotations a rotation
**                      matrix.  UU_FALSE = 90 degree rotations are not
**                      considered a rotation matrix.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_TRUE if a rotation exists.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_is_rottf(tfmat,flag)
UM_transf tfmat;
UU_LOGICAL flag;
{
	UU_LOGICAL rottf;
	int i;
	static UM_vector axis[3]={1.,0.,0., 0.,1.,0., 0.,0.,1.};

	rottf = UU_FALSE;
	if (tfmat != UM_DEFAULT_TF)
	{
		if (flag)
		{
			if (!um_cceqcc(axis[0],tfmat[0])) rottf = UU_TRUE;
			if (!um_cceqcc(axis[1],tfmat[1])) rottf = UU_TRUE;
			if (!um_cceqcc(axis[2],tfmat[2])) rottf = UU_TRUE;
		}
		else
		{
			
			for (i=0;i<3;i++)
			{
				if (!um_vcparall(axis[0],tfmat[i]) &&
					!um_vcparall(axis[1],tfmat[i]) &&
					!um_vcparall(axis[2],tfmat[i])) rottf = UU_TRUE;
			}
		}
	}
	return(rottf);
}

/*********************************************************************
**    E_FUNCTION : UU_LOGICAL  um_is_idmat(tfmat) 
**			Determines if the specified transformation is an identity
**			transformation
**    PARAMETERS   
**       INPUT  : 
**          tfmat			any transformation
**       OUTPUT :  
**          none.
**    RETURNS      : UU_TRUE iff the transformation is the identity
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_is_idmat(tfmat)
	UM_transf tfmat;

	{
	UU_LOGICAL identity;
	int i,j;

	identity = UU_TRUE;
	if (tfmat != UM_DEFAULT_TF)
		{
		i = 0;
		j = 0;
		while (identity && (i<4))
			{
			if (i==j)
				identity = (fabs(1.0 - tfmat[i][j]) < UM_EQPARM);
			else
				identity = fabs(tfmat[i][j]) < UM_EQPARM;
			j++;
			if (j == 3)
				{
				j = 0;
				i++;
				}
			}
		}
/*
	if (tfmat != UM_DEFAULT_TF) umi_print_transformation(tfmat);
	if (identity) um_pscroll("um_is_idmat returns true"); 
	else um_pscroll("um_is_idmat returns false");
*/
	return(identity);
	}

/*********************************************************************
**    E_FUNCTION: UU_LOGICAL um_tfeqtf(tf1, tf2)
**			This function determines whether the two transformations, tf1 and
**			tf2 are the same or not.
**    PARAMETERS   
**       INPUT: 
**				tf1	First transformation.
**				tf2	Second transformation.
**       OUTPUT: none. 
**    RETURNS: UU_TRUE iff the transformations are equal within UM_FUZZ of
**					each entry. 
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
UU_LOGICAL um_tfeqtf(tf1, tf2)
	UM_transf tf1, tf2;

	{
	int r,c;
	if (tf1 == tf2)
		return(UU_TRUE);
	for (r=0; r<4; r++)
		for (c=0; c<3; c++)
			if (fabs(tf1[r][c] - tf2[r][c]) > UM_EQPARM)
				return(UU_FALSE);
	return(UU_TRUE);
	}

/*********************************************************************
**    E_FUNCTION     : um_chgcstf(o1, x1, y1, z1, o1, x2, y2, z2, tfmat)
**       Calculate the transformation matrix (TFMAT) which will map
**			one coordinate system (o1,x1,y1,z1) into another (o2,x2,y2,z2).
**    PARAMETERS   
**       INPUT  : 
**          o1								origin of first coordinate system
**          x1								xaxis of first coordinate system
**          y1								yaxis of first coordinate system
**          z1								zaxis of first coordinate system
**          o2								origin of second coordinate system
**          x2								xaxis of second coordinate system
**          y2								yaxis of second coordinate system
**          z2								zaxis of second coordinate system
**       OUTPUT :  
**          tfmat							transformation matrix
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_chgcstf(o1, x1, y1, z1, o2, x2, y2, z2, tfmat)
	UM_coord o1;
	UM_vector x1;
	UM_vector y1;
	UM_vector z1;
	UM_coord o2;
	UM_vector x2;
	UM_vector y2;
	UM_vector z2;
	UM_transf tfmat;

	{
	UM_angle ang;
	UM_vector vec;
	UM_vector newy1;
	UM_transf tempmat;

	if (um_vcparall(z1, z2))
		um_vctovc(y2, vec);
	else
		um_cross(z1, z2, vec);
	ang = um_angle2p(z1, z2, vec);
	um_rottf(vec, ang, tfmat);
	um_cctmtf(y1, tfmat, newy1);
	ang = um_angle2p(newy1, y2, z2);
	um_rottf(z2, ang, tempmat);
	um_tftmtf(tfmat, tempmat, tfmat);
	um_vcmnvc(o2, o1, vec);
	um_vctmtf(vec, tfmat, tfmat[3]);
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : um_identtf( tfo )
**			Return the identity transformation
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          tfo			identity transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_identtf( tfo )
	UM_transf	tfo;

	{
	int i,j;

	for (i=0; i<4; i++)
		for (j=0; j<3; j++) tfo[i][j] = UM_idmat[i][j];
	return (0);
	}

/*********************************************************************
**    E_FUNCTION :  um_cplntf(tfmat)
**			Loads TFMAT with the transformation which will map 
**			construction space to model space.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          *tfmat	-	set to map from construction space to 
**								model space
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_cplntf(tfmat)
	UM_transf	tfmat;

	{
	um_vctovc(UM_cpln.xaxis, tfmat[0]);
	um_vctovc(UM_cpln.yaxis, tfmat[1]);
	um_vctovc(UM_cpln.zaxis, tfmat[2]);
	um_vctovc(UM_cpln.origin, tfmat[3]);
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : void um_rotlntf(pt, vec, ang, rottf)
**       Return a transformation (ROTTF) which rotates around a
**			line (PT, VEC) by an angle (ANG). The right hand rule
**			applies for the sign of the angle.
**    PARAMETERS   
**       INPUT  : 
**          pt						point defining line
**				vec					vector defining line
**				ang					rotation angle
**       OUTPUT :  
**          rottf					rotation transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_rotlntf(pt, vec, ang, rottf)
	UM_coord pt;
	UM_vector vec;
	UM_angle ang;
	UM_transf rottf;

	{
	UM_coord temp;
	UM_transf trantf;

	um_vctmsc(pt, (UU_REAL) -1.0, temp);
	um_disptf(temp, trantf);
	um_rottf(vec, ang, rottf);
	um_tftmtf(trantf, rottf, rottf);
	um_disptf(pt, trantf);
	um_tftmtf(rottf, trantf, rottf);
	}

/*********************************************************************
**    E_FUNCTION     :  int um_inverttf(a, ainv)
**					Invert the 4x3 transformation "a" and return the inverted
**					4x3 transformation thru "ainv".
**    PARAMETERS   
**       INPUT  : 
**          a					4x3 transformation to be inverted.
**					a == UM_DEFAULT_TF works OK now.
**       OUTPUT :  
**          ainv				4x3 inverted transformation.
**    RETURNS      : Returns 0 if inverse does not exist; else 1.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_inverttf(a, ainv)               
	UM_transf a;
	UM_transf ainv; /* ainv = inverse of a */

	{
	UU_REAL tempa[4][4];
	UU_REAL tempainv[4][4];
  int i,j;
  int ipvt[4],info;

	/* convert "a" to a 4x4 matrix */	

  if ( a == UM_DEFAULT_TF)
  	{
		/* inverse of identity is identity	*/
		um_identtf(ainv);
		goto  Done;
  	}
  for (i=0; i<4; i++)
	for (j=0; j<3; j++)
		tempa[i][j] = a[i][j];
  for (i=0; i<4; i++)
	tempa[i][3] = 0.0;
  tempa[3][3] = 1.0;

  for (i=0; i<4; i++) 
    for (j=0; j<4; j++) tempainv[i][j]=tempa[i][j];
  umi_efa(tempainv,ipvt,&info);
  if (info!=0) 
  	{ 
	 return(0);
  	}
  umi_edi(tempainv,ipvt);      /* umi_efa returned ok. */

  /* convert "tempainv" to a 4x3 matrix */	
  for (i=0; i<4; i++)
	for (j=0; j<3; j++)
		ainv[i][j] = tempainv[i][j];

Done:
  return(1);
	}

/*********************************************************************
**    I_FUNCTION     :  umi_efa(a,ipvt,info) 
**        Makes "a" an upper triangular matrix by gaussian elim.
**    PARAMETERS   
**       INPUT  : 
**          a					matrix to be factored.
**       OUTPUT :  
**          ipvt				returned pivot indices.
**				info				returns nonzero if successful.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umi_efa(a,ipvt,info)            /* factors a by gaussian elim */
UU_REAL a[4][4];                 /* matrix to be factored */
int ipvt[4];                   /* returned pivot indices */
int *info;                      /* returned =0 or =k if u[k][k]=0.0 */
{
/* this translated from the linpack routine sgefa */
/* gaussian elimination with partial pivoting */
  int i,j,k,l;
  UU_REAL big,t;

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
  return (0);
}

/*********************************************************************
**    I_FUNCTION     :  umi_edi(a,ipvt) 
**				 compute inverse of a.
**    PARAMETERS   
**       INPUT  : 
**          a					upper triangular matrix to find inverse of.
**				ipvt				pivot vector from umi_efa.
**       OUTPUT :  
**          a					inverse matrix.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umi_edi(a,ipvt)                   /* compute inverse of a */
	UU_REAL a[4][4];                 /* output from ug_efa */
	int ipvt[4];                   /* pivot vector from ug_efa */
{ 
	UU_REAL tmp,work[4],t;
   int i,j,k,l;

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
  return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_determinant(tfmat,det)
**      Calculate the nearest point on the plane to the specified point.
**    PARAMETERS   
**       INPUT  : 
**				pt             coordinates of given point
**          ppt            point on plane
**          unvc           unit normal to plane
**       OUTPUT :  
**				npt            coordinates of nearest point on plane
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_determinant(tfmat,det)
	UM_transf tfmat;
	UU_REAL *det;
 {
  *det = tfmat[0][0] * 
           (tfmat[1][1]*tfmat[2][2] - tfmat[2][1]*tfmat[1][2]) +
         tfmat[1][0] *
           (tfmat[2][1]*tfmat[0][2] - tfmat[0][1]*tfmat[2][2]) +
         tfmat[2][0] *
           (tfmat[0][1]*tfmat[1][2] - tfmat[1][1]*tfmat[0][2]);
 } 

/*********************************************************************
**    E_FUNCTION : UU_LOGICAL  um_distorting_tf(tfmat,scale)
**       Determines whether the transformation is distorting, i.e., whether
**			it scales differently along different directions.
**    PARAMETERS   
**       INPUT  : 
**          tfmat			transformation
**       OUTPUT :  
**          scale       scaling factor
**    RETURNS      : UU_TRUE iff distorting
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_distorting_tf(tfmat,scale)
UM_transf tfmat;
UU_REAL *scale;
{
	int i;
	UU_LOGICAL distort;

	*scale = 1.;
	distort = UU_FALSE;
	if (tfmat != UM_DEFAULT_TF)
	{
		*scale = um_mag(tfmat[0]);
		for (i=1; i<3 && !distort; i++)
		{
			if (fabs((*scale) - um_mag(tfmat[i])) >= UM_FUZZ)
				distort = UU_TRUE;
		}
	}
	return (distort);
}

/*********************************************************************
**    E_FUNCTION     : um_ptzx_tf(pt,vz,vx,tfr)
**       Return the 4X3 scaling transformation given an origin point,
**			Z-axis vector, and X-axis vector.
**    PARAMETERS   
**       INPUT  : 
**          pt   			Origin point.
**          vx   			X-axis vector.
**          vz   			Z-axis vector.
**       OUTPUT :  
**				tfr			resulting transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_ptzx_tf(pt,vz,vx,tfr)
UM_coord pt;
UM_vector vx,vz;
UM_transf tfr;
{
/*
.....Create rotation part
*/
	um_unitvc(vz,tfr[2]);
	um_cross(vz,vx,tfr[1]); um_unitvc(tfr[1],tfr[1]);
	um_cross(tfr[1],vz,tfr[0]); um_unitvc(tfr[2],tfr[2]);
/*
.....Create translation part
*/
	um_vctovc(pt,tfr[3]);
}
