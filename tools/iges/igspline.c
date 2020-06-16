/*********************************************************************
**    NAME         :  igspline.c
**       CONTAINS:
**					uig_map_spline_to_nclcrv - covert iges parametric spline to 
**												NCL curve.
**					init_fortran_data - initialize iges data into global areas
**					ncl_put_crvhead - initialize the header for NCL_curve_rec struct from rdr_buf
**					ncl_put_crvseg - initialize the segment data into NCL_curve_rec struct 
**					ncl_crvheadsize - return the size of the rdr_buff header info
**
**			FORTRAN CALLABLE:
**					igtseg - get the number of segments
**					igett -  get the T array 
**					igtxyz - get the x, y, z coefficients for the segment
**                  igvctmsc - scale the xyz coords of a point
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			igspline.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:43
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "tiges.h"
#include "tigdefs.h"
#include "mcrv.h"
#include "mfort.h"
#include "nccs.h"	
#include "ncl.h"	
#include "igesfc.h"

#include "mdattr.h"
#include "tigsupp.h"
#include "mdrel.h"
#include "mxxx.h"
#include "rbase.h"
#include "msrf.h"

#define FALSE 0
#define TRUE  1

#define UM_2PI (UU_REAL) 6.28318

extern UU_LOGICAL drw_flag;
extern int drw_ptr;
extern UU_REAL drw_t[4][3], drw_s[4][3], drw_v[4][3];

/* NCL curve fit tolerance */
extern double ctol;

/* Error flag set from FORTRAN */
int errflag = FALSE;

/**     SET UP ERROR MESSAGES FOR FORTRAN **/
char errtxt[20][60] = { "UNKNOWN ERROR",
	  "ERROR FITTING CURVE - MORE THAN 20 S-VALUES",
      "BAD ELLIPSE EQUATION",
      "MORE THAN 200 POINTS IN PARAMETRIC SPLINE",
      "START AND END POINTS INCONSISTENT",
      "HANDLING OF SPLINE TYPES 4,5 OR 6 NOT IMPLEMENTED",
      "HANDLING OF PARABOLA OR HYPERBOLA NOT IMPLEMENTED",
      "ERROR FITTING CURVE - NOT ENOUGH POINTS",
      "ERROR FITTING CURVE - POINTS ARE TOO CLOSE",
      "ERROR FITTING CURVE - BAD INPUT VECTOR",
      "ERROR FITTING CURVE - SLOPES DID NOT CONVERGE",
      "ERROR FITTING CURVE - CONDITIONS ARE TOO SEVERE",
      "CIRCLE RADIUS IS ZERO",
      "BAD PARAMETER RECORD - INCORRECT TYPE",
      "UNKNOWN ERROR",
      "UNKNOWN ERROR",
      "UNKNOWN ERROR",
      "UNKNOWN ERROR",
      "UNKNOWN ERROR",
      "UNKNOWN ERROR"};
  


/* global variables to hold data needed in FORTRAN routines 
   to convert parametric splines */
int NSEG, DREC;
UM_real8 *T;
struct IG_coef_rec *COEF;

/*********************************************************************
**    I_FUNCTION     :  uig_map_spline_to_nclcrv(dblk,igesin,t,key)
**				Map an IGES parametric bspline to an NCL curve.
**    PARAMETERS   
**       INPUT  : 
**				dblk							IGES directory block
**				igesin							IGES arc structure
**				t								associated matrix
**       OUTPUT :  
**				key							UNIBASE key_id
**    RETURNS      : -1 if error, key otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int
uig_map_spline_to_nclcrv(dblk,igesin,t,key)
struct dir_rec *dblk;				/* IGES parametric bspline dir record */
struct IG_igesplin_rec *igesin;		/* IGES parametric bspline para record */
UU_REAL	 t[12];						/* IGES transformation matrix */
UU_KEY_ID *key;						/* key to return */
	{
	int i;
	struct NCL_curve_rec nclcrv;
	struct NCLI_crvhead_rec *crvhead;
	struct NCLI_crvseg_rec *crvseg;
	UU_KEY_ID e_key;
	char tbuf[100];
	UM_real8 rdr_buf[600]; 		/* buffer to hold canonical form of NCL curve */
	int status = 0;

	ur_setup_data(NCL_CURVE_REL, &nclcrv, sizeof(struct NCL_curve_rec));

	if (igesin->n_seg > 200) 	/*jkd53: check number spline segments */
		{
		sprintf(tbuf, "too many spline segments (%d); reset to 200\n", 
			igesin->n_seg);
		uig_error(tbuf);
		igesin->n_seg = 200;
		}

/* initialize FORTRAN stuff: */
	status = init_fortran_data(t, igesin->n_seg, dblk->drec_num, igesin->t, 
		igesin->no_t, igesin->coef, igesin->no_coef);

	/* convert the parametric spline to an NCL curve */
	if (status == 0)
		{
/*		igspln(rdr_buf, &status);  */
		igspln(rdr_buf);
		if (errflag == TRUE)
			{
			*key = -1;
			return *key;
			}
	
		/* move header info from RDR to crvhead */
		crvhead = (struct NCLI_crvhead_rec *)rdr_buf;
		ncl_put_crvhead(crvhead, &nclcrv); 

		/* move each curve segment into nclcrv */
		crvseg = (struct NCLI_crvseg_rec *)(rdr_buf + ncl_crvheadsize(rdr_buf));
		for (i = 0; i < crvhead->nosegs; i++)
			{
			nclcrv.no_segment++;
			if (i == crvhead->nosegs - 1) /* zero out unused values in last seg */
				{
				crvseg->duds0 = 0.;
				crvseg->duds1 = 0.;
				crvseg->duds0 = 0.;
				}

			ncl_put_crvseg(crvseg, &nclcrv.segment[i]); 
			crvseg++;
			}

		uig_update_attr(dblk);

		create_label(dblk, igesin->no_prop, igesin->prop, nclcrv.label, &nclcrv.subscr);

		/* check if creating a drawing entity */

		if(drw_flag)
			{
			uig_transform_drafting(&nclcrv, drw_v);
			uig_transform_drafting(&nclcrv, drw_s);
			uig_transform_drafting(&nclcrv, drw_t);
			dblk->view_ptr = drw_ptr;
			}

		ncl_create_entity(&nclcrv, dblk->view_ptr);

		UIG_unibase_entities++;

		*key = nclcrv.key;
		}
	else
		*key = -1;

	return (*key);
	}


/*********************************************************************
**    I_FUNCTION     :  init_fortran_data(trans, nseg, drec, t, tsz, coef, coefsz)
**				Initialize data into global area so that FORTRAN can
**              get to it.
**    PARAMETERS   
**       INPUT  : 
**				trans			transformation matrix
**				nseg			number of segments in parametric spline
**				drec			directory record number
**				t				T array
**				tsz				size of T array
**				coef			array of coefficient data
**				tsz				size of coefficient data array
**       OUTPUT :  
**    RETURNS      : -1 if error, 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int
init_fortran_data(trans, nseg, drec, t, tsz, coef, coefsz)
UU_REAL trans[12];
int nseg, drec, tsz, coefsz;
UU_REAL *t;
struct IG_coef_rec *coef;
	{
	int i, j;
	UM_real8 tm[12], *Tptr;
	struct IG_coef_rec *Cptr;

	errflag = FALSE;

	/* initialize transformation matrix to FORTRAN, and curve fit toler */
	for (i = 0; i < 12; i++)
		tm[i] = trans[i];
	igmat(tm, &ctol);

	if (nseg > 0)
		NSEG = nseg;
	else return -1;

	DREC = drec;

	if (t != NULL)
		{
		T = (UM_real8 *)malloc(sizeof(UM_real8) * tsz);

		for (Tptr = T, i = 0; i < tsz; i++)
			*Tptr++ = *t++;
		}
	else return -1;

	if (coef != NULL)
		{
		COEF = (struct IG_coef_rec *)malloc(sizeof(struct IG_coef_rec) * coefsz);

		for (Cptr = COEF, i = 0; i < coefsz; i++)
			{
			for (j = 0; j < 4; j++)
				Cptr->cx[j] = coef->cx[j];

			for (j = 0; j < 4; j++)
				Cptr->cy[j] = coef->cy[j];

			for (j = 0; j < 4; j++)
				Cptr->cz[j] = coef->cz[j];

			Cptr++; coef++;
			}
		}
	else return -1;

	return 0;
	}


/* FORTRAN callable routines */
/* get the number of segments */
int
igtseg(nseg)
UM_int4 *nseg;
	{
	*nseg = NSEG;
	return;
	}

/* get the T array */
int
igett(t)
UM_real8 *t;
	{
	int i;
	UM_real8 *tptr, *Tptr;

	for (tptr = t, Tptr = T, i = 0; i < NSEG+1; i++)
		*tptr++ = *Tptr++;

	return;
	}

/* get coefficient data for the n'th point */
int
igtxyz(n, a)
UM_int2 *n;
UM_real8 *a;
	{
	int i;
	UM_real8 *aptr;
	struct IG_coef_rec *cptr;

	/* init the x vaules */
	for (cptr = &COEF[*n-1], aptr = a, i = 0; i < 4; i++)
		*aptr++ = cptr->cx[i];

	/* init the y vaules */
	for (i = 0; i < 4; i++)
		*aptr++ = cptr->cy[i];
		
	/* init the z vaules */
	for (i = 0; i < 4; i++)
		*aptr++ = cptr->cz[i];
		
	return;
	}

/** NOTE: the following routines were borrowed from /ncl502/src/nclc/necv.c **/
/*********************************************************************
**    E_FUNCTION     : ncl_put_crvhead(crvhead, nclcrv)
**       Move the internal NCL representation of a curve header into
**       a UNIBSE representation.
**    PARAMETERS   
**       INPUT  : 
**          crvhed               NCLI crvhead record
**       OUTPUT :  
**          e                    NCL curve record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_put_crvhead(crvhead, nclcrv)
struct NCLI_crvhead_rec *crvhead;
struct NCL_curve_rec *nclcrv;
   {
   int i;

   nclcrv->no_param = crvhead->nosegs-1;
   for (i=0; i<nclcrv->no_param; i++) nclcrv->param[i] = crvhead->param[i];
   }

/*********************************************************************
**    E_FUNCTION     : ncl_put_crvseg(crvseg, e)
**       Move the internal NCL representation of a curve segment into
**       a UNIBSE representation.
**    PARAMETERS   
**       INPUT  : 
**          crvseg               NCLI crvseg record
**       OUTPUT :  
**          segdata              NCL segment record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_put_crvseg(crvseg, segdata)
struct NCLI_crvseg_rec *crvseg;
struct NCL_segment_rec *segdata;
   {
   int i;

   for (i=0; i<3; i++) segdata->point[i] = crvseg->point[i];
   for (i=0; i<3; i++) segdata->delta[i] = crvseg->delta[i];
   segdata->duds0 = crvseg->duds0;
   segdata->duds1 = crvseg->duds1;
   segdata->rho = crvseg->rho;
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_crvheadsize(crvhead)
**       Calculate the size (in bytes) of a curve head record.
**    PARAMETERS   
**       INPUT  : 
**          crvhead              NCLI curve header record
**       OUTPUT :  
**          none
**    RETURNS      : 
**       actual size of curve header (in bytes)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_crvheadsize(crvhead)
struct NCLI_crvhead_rec *crvhead;
   {
   int crvheadsize;
   int nosegs;

   nosegs = crvhead->nosegs;
   crvheadsize = sizeof(UM_real4) + ((nosegs-1) * sizeof(UM_real4));
   crvheadsize = crvheadsize / sizeof(UM_real8);
   if (nosegs & 1) crvheadsize++;
   return (crvheadsize);
   }

/*********************************************************************
C **                                                                 **
C **     COPYRIGHT (C) 1984 MILLS DATA SYSTEMS CORPORATION           **
C **                                                                 **
C *********************************************************************
C **  PROGRAM NAME: IGEROR                                           **
C **                                                                 **
C **  WRITTEN BY: I.DONALDSON                  ON: 09 MAY 84         **
C **  LAST REVISION BY:                        ON:                   **
C **                                                                 **
C **  LAST REVISION:                                                 **
C **                                                                 **
C **  PURPOSE OF SUBROUTINE:                                         **
C **                                                                 ** 
C **    Modified FORTRAN routine 
C **
C **    REPORT ERRORS                                                **
C **                                                                 **
C *********************************************************************/
int
igeror(nerr)
UM_int2 *nerr;
	{
	char buf[100];

	errflag = TRUE;

	sprintf(buf,"ERROR mapping PARAMETRIC SPLINE to NCL curve: record #%d\n", 
		DREC);
	uig_error(buf);

	if (*nerr > 20) *nerr = 20;

	sprintf(buf,"** ERROR #%d %s\n", *nerr, errtxt[*nerr]);
	uig_error(buf);
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : igvctmsc(rdr)
**         To call MPE routine um_vctmsc() from FORTRAN            
**    PARAMETERS   
**       INPUT  : 
**          rdr               x,y,z coordinates of a point
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
igvctmsc(rdr)
UM_real8 *rdr;
	{
	UU_REAL pt[3];

	pt[0] = rdr[0];
	pt[1] = rdr[1];
	pt[2] = rdr[2];

	um_vctmsc(pt,unit_scale,pt);

	rdr[0] = pt[0];
	rdr[1] = pt[1];
	rdr[2] = pt[2];

	return 0;
	}
