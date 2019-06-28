/*********************************************************************
**    NAME         : m3multi.c
**       CONTAINS:
**         um_fill_arc(e, radius, last, c, err, npt, ipt)
**         umu_multi_fillet()
**         um_make_fillets(e,radius,last,nclkey,subs,label)
**         umu_m2_multi_chamfer()
**         um_get_id (key,eid)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3multi.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       10/13/15 , 11:08:35
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdcoord.h"
#include "mdeval.h"
#include "mdebug.h"
#include "modef.h"
#include "misect.h"
#include "zsysdep.h"
#include "mdclass.h"
#include "mdgenent.h"
#include "mdpick.h"
#include "mattr.h"
#include "dmark.h"
#include "nccs.h"
#include "dselmask.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclvx.h"

extern UU_LOGICAL   ud_gnxt();
extern int NCL_ubcopy;

/*********************************************************************
**    E_FUNCTION     : um_fill_arc
**       Calculate the arc used for the fillet operation
**    PARAMETERS   
**       INPUT  : 
**             e        two curves to fillet
**            radius    radius of fillet
**            last      is this the last pair of entities?
**            errflag   UU_TRUE = output error messages.
**       OUTPUT :  
**             c       record containing fillet arc
**            err      0 iff successful -1 otherwise
**            npt      points the fillet arc intersects the two input curves
**            ipt      intersection point of the two curves
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_fill_arc(e, radius, last, c, err, npt, ipt, errflag)
struct UM_crvdatabag e[2];
UM_length radius;
UU_LOGICAL last;
struct UM_circle_rec  *c;
UU_LOGICAL  *err;
UU_REAL   npt[2][3];
UM_coord   ipt;
UU_LOGICAL errflag;
{
   int       nint;                 /* number of intersections */
   UU_REAL   tpt[3];               /* temporary point */
   UU_REAL   pt[4][3];             /* end points of curve */
   UU_REAL   der[4][3];            /* tangent at the end points of curve */

   UU_LOGICAL status;

   UM_length rad;
   UM_length crad;

   UM_vector   v0, v1;            /* vectors of the two intersecting lines */
   UM_vector   u0, u1;            /* unitized v0 and v1 */
   UM_vector   nvec;               /* normal to plane defined by v0 and v1 */
   UM_vector   bvec;               /* bisector of u0 and u1 */
   UM_vector   pvec;               /* vector a distance rad away from v0 */

   UM_coord      center;            /* center of fillet arc */
   UM_coord      tanpt;            /* fillet arc start point */

   UM_transf tfmat;               /* transformation matrix */

   struct UM_line_rec pline;      /* line a distance rad away from v0 */
   struct UM_circle_rec tcircle, tcircle1;
   struct UM_evcrvout evout;      /* curve evaluator */
   struct UM_line_rec *lptr0;
   struct UM_line_rec *lptr1;
   struct UM_circle_rec *cptr1;   
   struct UM_circle_rec *cptr0;   /* circle with radius crad concentric with
                                    the input arc */
   struct UM_line_rec tl;         /* a line coincident with the input line that
                                    points away from the intersection point */
   struct UM_circle_rec tc, tc1;    /* the input arc */

   UM_isect ibuff[2];               /* buffer to hold intersection points */
   int i, j, ix;
   UM_vector vecc; 
   UM_coord dlt0, dlt1;

   uu_denter(UU_MTRC,(us,"um_fill_arc()"));
   *err = 0;

   if ((e[0].rel_num == UM_LINE_REL) && (e[1].rel_num == UM_LINE_REL))
      {
      lptr0 = (struct UM_line_rec *) &e[0];
      lptr1 = (struct UM_line_rec *) &e[1];

      if (um_cceqcc(lptr0->spt, lptr1->spt))
         {
         um_vcmnvc(lptr0->ept, lptr0->spt, v0);
         um_vcmnvc(lptr1->ept, lptr1->spt, v1);
         um_vctovc(lptr0->spt, ipt);
         rad = -radius;
         }
      else if (um_cceqcc(lptr0->spt, lptr1->ept))
         {
         um_vcmnvc(lptr0->ept, lptr0->spt, v0);
         um_vcmnvc(lptr1->spt, lptr1->ept, v1);
         um_vctovc(lptr0->spt, ipt);
         rad = -radius;
         }
      else if (um_cceqcc(lptr0->ept, lptr1->ept))
         {
         um_vcmnvc(lptr0->spt, lptr0->ept, v0);
         um_vcmnvc(lptr1->spt, lptr1->ept, v1);
         um_vctovc(lptr0->ept, ipt);
         rad = radius;
         }
      else if (um_cceqcc(lptr0->ept, lptr1->spt))
         {
         um_vcmnvc(lptr0->spt, lptr0->ept, v0);
         um_vcmnvc(lptr1->ept, lptr1->spt, v1);
         um_vctovc(lptr0->ept, ipt);
         rad = radius;
         }
      else
         {
         if (! last && errflag)
            uu_uerror0(UM_MODEL, 238); /* entities not connected */
         *err = -1;
         uu_dexit;
         return;
         }
      um_unitvc(v0, u0);
      um_unitvc(v1, u1);

   /* calculate the bisector of the two vectors defined by lines 1 and 2 */
      um_bisect(v0, v1, ipt, bvec);
      um_unitvc(bvec, bvec);

   /* calculate the normal vector of the plane defined by lines 1 and 2 */
      um_cross(v0, v1, nvec);
      um_unitvc(nvec, nvec);

   /* set up a line a distance radius away from line 1 (one that interects
      line 2)
   */
      um_lnatdist(nvec, rad, lptr0, &pline);
      um_vcmnvc(pline.ept, pline.spt, pvec);
      um_unitvc(pvec, pvec);

   /* find the intersection of the bisector and the parallel line */
      um_ilnln(ipt, bvec, pline.spt, pvec, &nint, center);

   /* calculate the arcs start vector */
      um_nptln(center, ipt, u0, npt[0]);
      um_nptln(center, ipt, u1, npt[1]);
      um_vctovc(npt[1], tanpt);

   /* check that the intersection of the fillet arc lies on the line
      between the start and end point   */
      status = um_ptinseg(lptr0->spt,npt[0],lptr0->ept);
      if (status == UU_TRUE)
      {
         status = um_ptinseg(lptr1->spt,npt[1],lptr1->ept);
         if (status == UU_FALSE)
         {
				if (errflag)
       	     uu_uerror0(UM_MODEL, 239); /* radius too large for fillet */
            *err = -1;
            uu_dexit;
            return;
         }
      }
      else
      {
			if (errflag)
  		       uu_uerror0(UM_MODEL, 239); /* radius too large for fillet */
         *err = -1;
         uu_dexit;
         return;
      }

   /* set up arc */
      c->rel_num = UM_CIRCLE_REL;
      c->radius = radius;
      um_vctovc(center, c->center);
      um_vcmnvc(tanpt, c->center, c->svec);
      um_unitvc(c->svec, c->svec);
      um_vctovc(nvec, c->nvec);
      um_vcmnvc(npt[0], center, v0);
      um_vcmnvc(npt[1], center, v1);
      c->dang = um_angle2p(v0, v1, nvec);
      if (c->dang > UM_PI)
         c->dang = UM_TWOPI - c->dang;

      }
   else if (e[0].rel_num == UM_CIRCLE_REL && e[1].rel_num == UM_CIRCLE_REL) 
      {
       cptr0 = (struct UM_circle_rec *) &e[0];
       cptr1 = (struct UM_circle_rec *) &e[1];

       uc_init_evcrvout(cptr0, &evout);
       um_get_transformation(cptr0->key, tfmat);
       uc_evcrv(UM_FRSTDERIV, (UU_REAL) 0.0, cptr0, tfmat, &evout);
       um_vctovc(evout.cp, pt[0]);
       um_vctovc(evout.dcdu, der[0]);
       uc_evcrv(UM_FRSTDERIV, (UU_REAL) 1.0, cptr0, tfmat, &evout);
       um_vctovc(evout.cp, pt[1]);
       um_vctovc(evout.dcdu, der[1]);

       uc_init_evcrvout(cptr1, &evout);
       um_get_transformation(cptr1->key, tfmat);
       uc_evcrv(UM_FRSTDERIV, (UU_REAL) 0.0, cptr1, tfmat, &evout);
       um_vctovc(evout.cp, pt[2]);
       um_vctovc(evout.dcdu, der[2]);
       uc_evcrv(UM_FRSTDERIV, (UU_REAL) 1.0, cptr1, tfmat, &evout);
       um_vctovc(evout.cp, pt[3]);
       um_vctovc(evout.dcdu, der[3]);

       zbytecp(tc, *cptr0);
       zbytecp(tc1, *cptr1);
       zbytecp(tcircle, tc);
       zbytecp(tcircle1, tc1);
    
       for (i=0; i<2; i++)
         {
          for (j=2; j<4; j++)
             if (um_cceqcc(pt[i], pt[j]))
                {
                 if (i == 0) um_vctovc(der[i], v0);
                 else um_vctmsc (der[i], (UU_REAL) -1.0, v0);
                 if (j == 2) um_vctovc(der[j], v1);
                 else um_vctmsc (der[j], (UU_REAL) -1.0, v1);
                 um_vctovc(pt[i], ipt);
                 goto next;
                }
         }       

       if (! last && errflag)
            uu_uerror0(UM_MODEL, 238); /* entities not connected */
       *err = -1;
       uu_dexit;
       return;

next:;
       um_unitvc(v0, u0);
       um_unitvc(v1, u1);
/* 
...determine if we need to use a circle with a larger or smaller radius 
*/
       um_vcmnvc (tc.center,ipt,vecc);
       um_unitvc(vecc, vecc);
       crad = (um_dot (vecc,u1) > 0.)? -radius: radius; 
       um_vcmnvc (tc1.center,ipt,vecc);
       um_unitvc(vecc, vecc);
       rad = (um_dot (vecc,u0) > 0.)? -radius: radius; 

      /* check to see if the vectors u0 and u1 are parallel */
      if (um_dot(u0, u1) < UM_DFUZZ - 1.0)
         {
			if (errflag)
         	uu_uerror0(UM_MODEL, 240); /* line and arc are tangent */
         *err = -1;
         uu_dexit;
         return;
         }
      else 
         if (um_dot(u0, u1) > 1.0 - UM_DFUZZ) rad = crad = radius;

   /* calculate the normal vector of the plane defined by lines 1 and 2 */
      um_cross(v0, v1, nvec);
      if (um_cceqcc(nvec, UM_zerovec)) um_vctovc (tc.nvec,nvec);
      um_unitvc(nvec, nvec);

   /* set up a circle with radius radius + crad */
      tcircle.radius = tcircle.radius + crad;
      tcircle1.radius = tcircle1.radius + rad;

   /* find the intersection of tcircle and pline (this is the center of the 
      fillet arc */
      um_isect(&tcircle1, UM_DEFAULT_TF, &tcircle, UM_DEFAULT_TF, &nint, UM_MAXISECT,
               ibuff);
      ix  = 0; 
      if (nint > 1)
         {
          um_vcmnvc (ibuff[0].pt,ipt,dlt0);
          um_vcmnvc (ibuff[1].pt,ipt,dlt1);
          if (um_mag (dlt1) < um_mag (dlt0)) ix = 1;
          if (ibuff[ix].u0 > 1.0 || ibuff[ix].u0 < 0.0 ||
              ibuff[ix].u1 > 1.0 || ibuff[ix].u1 < 0.0) ix = 1 - ix; 
         }
      um_vctovc(ibuff[ix].pt, center);

   /* calculate the arcs start vector */
      um_vcmnvc (center,cptr0->center,dlt0);
      um_unitvc (dlt0,dlt0);
      um_vctmsc (dlt0,cptr0->radius,dlt0);
      um_vcplvc (cptr0->center,dlt0,npt[0]);
      um_vctovc(npt[0], tanpt);
      um_vcmnvc (center,cptr1->center,dlt0);
      um_unitvc (dlt0,dlt0);
      um_vctmsc (dlt0,cptr1->radius,dlt0);
      um_vcplvc (cptr1->center,dlt0,npt[1]);

   /* set up arc */
      c->rel_num = UM_CIRCLE_REL;
      c->radius = radius;
      um_vctovc(center, c->center);
      um_vcmnvc(tanpt, c->center, c->svec);
      um_unitvc(c->svec, c->svec);
      um_vctovc(nvec, c->nvec);
      um_vcmnvc(npt[0], center, v0);
      um_vcmnvc(npt[1], center, v1);
      c->dang = um_angle2p(v0, v1, nvec);
      if (c->dang > UM_PI)
        {
         c->dang = UM_TWOPI - c->dang;
         um_vctmsc (c->nvec, (UU_REAL) -1.0, c->nvec);
        }
      }
   else if(((e[0].rel_num == UM_LINE_REL) && (e[1].rel_num == UM_CIRCLE_REL)) ||
           ((e[0].rel_num == UM_CIRCLE_REL) && (e[1].rel_num == UM_LINE_REL)))
      {
      if ((e[0].rel_num == UM_LINE_REL) && (e[1].rel_num == UM_CIRCLE_REL))
         {
         lptr0 = (struct UM_line_rec *) &e[0];
         cptr0 = (struct UM_circle_rec *) &e[1];
         }
      else if ((e[0].rel_num == UM_CIRCLE_REL) && (e[1].rel_num == UM_LINE_REL))
         {
         lptr0 = (struct UM_line_rec *) &e[1];
         cptr0 = (struct UM_circle_rec *) &e[0];
         }

         uc_init_evcrvout(cptr0, &evout);
         um_get_transformation(cptr0->key, tfmat);
         uc_evcrv(UM_FRSTDERIV, (UU_REAL) 0.0, cptr0, tfmat, &evout);
         um_vctovc(evout.cp, pt[0]);
         um_vctovc(evout.dcdu, der[0]);
         uc_evcrv(UM_FRSTDERIV, (UU_REAL) 1.0, cptr0, tfmat, &evout);
         um_vctovc(evout.cp, pt[1]);
         um_vctovc(evout.dcdu, der[1]);

         zbytecp(tl, *lptr0);
         zbytecp(tc, *cptr0);
         zbytecp(tcircle, tc);
         pline.rel_num = UM_LINE_REL;
   
      if (um_cceqcc(lptr0->spt, pt[0]))
         {
         um_vcmnvc(lptr0->ept, lptr0->spt, v0);
         um_vctovc(der[0], v1);
         um_vctovc(pt[0], ipt);
         }
      else if (um_cceqcc(lptr0->spt, pt[1]))
         {
         um_vcmnvc(lptr0->ept, lptr0->spt, v0);
         um_vctmsc(der[1], (UU_REAL) -1.0, v1);
         um_vctovc(pt[1], ipt);
         }
      else if (um_cceqcc(lptr0->ept, pt[0]))
         {
         um_vcmnvc(lptr0->spt, lptr0->ept, v0);
         um_vctovc(der[0], v1);
         um_vctovc(pt[0], ipt);
         um_vctovc(tl.spt, tpt);
         um_vctovc(tl.ept, tl.spt);
         um_vctovc(tpt, tl.ept);
         }
      else if (um_cceqcc(lptr0->ept, pt[1]))
         {
         um_vcmnvc(lptr0->spt, lptr0->ept, v0);
         um_vctmsc(der[1], (UU_REAL) -1.0, v1);
         um_vctovc(pt[1], ipt);
         um_vctovc(tl.spt, tpt);
         um_vctovc(tl.ept, tl.spt);
         um_vctovc(tpt, tl.ept);
         }
      else
         {
         if (! last && errflag)
            uu_uerror0(UM_MODEL, 238); /* entities not connected */
         *err = -1;
         uu_dexit;
         return;
         }
      um_unitvc(v0, u0);
      um_unitvc(v1, u1);
      um_vcmnvc (tc.center,ipt,vecc);
      um_unitvc(vecc, vecc);

   /* calculate the normal vector of the plane defined by lines 1 and 2 */
      um_cross (u1,vecc,nvec);
      um_unitvc(nvec, nvec); 


   /* determine if we need to use a circle with a larger or smaller radius */
      crad = (um_dot (vecc,v0) > 0.)? -radius: radius; 

      /* check to see if the vectors u0 and u1 are parallel */
      if (um_dot(u0, u1) < UM_DFUZZ -1.0)
         {
			if (errflag)
         	uu_uerror0(UM_MODEL, 240); /* line and arc are tangent */
         *err = -1;
         uu_dexit;
         return;
         }
      else 
         {
          um_cross (nvec,u0,pvec);
          um_unitvc (pvec,pvec);
          rad = radius;
/* 
...set up a line a distance radius away from line 1 (one that interects the arc)
*/
          if (um_dot(pvec,u1) < 0.0) rad = -radius;
          if (um_dot(u0, u1) > 1.0 - UM_DFUZZ) { crad = radius; rad = radius; }
          um_vctmsc (pvec,rad,pvec);
          um_vcplvc (tl.spt,pvec,pline.spt);
          um_vcplvc (tl.ept,pvec,pline.ept);
         }

   /* set up a circle with radius radius + crad */
      tcircle.radius = tcircle.radius + crad;

   /* find the intersection of tcircle and pline (this is the center of the 
      fillet arc */
      um_isect(&pline, UM_DEFAULT_TF, &tcircle, UM_DEFAULT_TF, &nint, UM_MAXISECT,
               ibuff);
      ix  = 0; 
      if (nint > 1)
         {
          um_vcmnvc (ibuff[0].pt,ipt,dlt0);
          um_vcmnvc (ibuff[1].pt,ipt,dlt1);
          if (um_mag (dlt1) < um_mag (dlt0)) ix = 1;
          if (ibuff[ix].u0 > 1.0 || ibuff[ix].u0 < 0.0 ||
              ibuff[ix].u1 > 1.0 || ibuff[ix].u1 < 0.0) ix = 1 - ix; 
         }
      um_vctovc(ibuff[ix].pt, center);

   /* calculate the arcs start vector */
      um_vcmnvc (center,cptr0->center,dlt0);
      um_unitvc (dlt0,dlt0);
      um_vctmsc (dlt0,cptr0->radius,dlt0);
      um_vcplvc (cptr0->center,dlt0,npt[1]);
      um_nptln(center, ipt, u0, npt[0]);

   /* set up arc */
      c->rel_num = UM_CIRCLE_REL;
      c->radius = radius;
      um_vctovc(center, c->center);
      um_vctovc(nvec, c->nvec);
      um_vcmnvc(npt[0], center, v0);
      um_vcmnvc(npt[1], center, v1);
      um_vctovc(v0,c->svec);
      um_unitvc(c->svec, c->svec);
      c->dang = um_angle2p(v0, v1, nvec);
      if (c->dang > UM_PI)
         {
          c->dang = UM_TWOPI - c->dang;
          um_vctmsc (c->nvec, (UU_REAL) -1.0, c->nvec);
         }

      if (e[0].rel_num == UM_CIRCLE_REL)
         {
         um_vctovc(npt[0], tpt);
         um_vctovc(npt[1], npt[0]);
         um_vctovc(tpt, npt[1]);
         }
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : umu_multi_fillet()
**      Fillet lines.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_multi_fillet()

   {
   UM_PLOCREC pick;                    /* pick information */
   UU_REAL radius;                     /* radius of tangent circle */
   struct UM_crvdatabag e[2];          /* two picked entities */
   struct UM_crvdatabag first_e;
   struct NCL_id_rec *eid;
   int i,k;                              /* index */
   int numint, nent, status;
   UU_LOGICAL gnxt_init;
   UU_LOGICAL cont;
   UU_LOGICAL last;                    /* last pair of entities? */
	UM_int2 i1,i2,sub;
	int jsub,cmdst;
	UM_f77_str fstr;
	char slab[NCL_MAX_LABEL+1];
	UM_int2 idx = 169;
	UM_real8 ver;
	UU_LOGICAL NCL_lv94;

   uu_denter( UU_MTRC,(us,"umu_multi_fillet()"));

	status = UU_SUCCESS;
	getsc(&idx, &ver);
	NCL_lv94 = (ver < 9.449);

   nent = 0;
	i2   = 7;
   while (UU_TRUE)
      {
      gnxt_init = UU_TRUE;
      last = UU_FALSE;

      /* limit DAS to editable lines & circles/arcs */
      ud_lgeo(UU_TRUE, UD_lncir);
      ud_leditable(UU_TRUE);

      ud_ldas(UD_DASSELECT, /* pick a connected sequence of lines/arcs 
         */UM_MODEL, 265, &pick, 1, &numint, 1);
      if (numint <= 1) goto done;
      nent = numint;

repeat:;
      ud_ldas(UD_DASDISTANCE, /*fillet radius*/UM_MODEL, 64, &radius, 
             1, &numint, UD_NODEFAULT);
      if (numint <= 0) goto done;
      if (radius < UM_FUZZ)
         {
         uu_uerror0(/*radius is too small*/UM_MODEL,19);
         e[0].key = e[1].key;
         goto repeat;
         }
      k    = 0;

      ud_gnxt(gnxt_init, UU_NULL, &first_e.key, 1);
      e[0].key = first_e.key;
      gnxt_init = UU_FALSE;
      cont = UU_TRUE;
      cmdst = 0;
		jsub = 0;
      if (UU_application == UU_NCLCAM)
		{ 
         eid  = (struct NCL_id_rec *) uu_malloc(nent*sizeof(struct NCL_id_rec));
         um_get_id (e[0].key,&eid[0]);
/*
.....Multiple fillets must be generated
.....with subscripted variable name
.....Bobby  -  5/3/96
*/
		if (NCL_lv94)
		{		
			UM_init_f77_str(fstr,slab,NCL_MAX_LABEL);
			namgen_s (&i2,UM_addr_of_f77_str(fstr),&sub);	
			for (i=0;i<NCL_MAX_LABEL;i++) if (slab[i] == ' ') break;
			slab[i] = '\0';
			if (sub == 0) sub = 1;
			jsub = sub;
		}
		}

      while (cont)
         {
         cont = ud_gnxt(gnxt_init, UU_NULL, &e[1].key, 1);
         if (!cont)
            {
            last = UU_TRUE;
            e[1].key = e[0].key;
            e[0].key = first_e.key;
            }
         else
            if (UU_application == UU_NCLCAM) um_get_id (e[1].key,&eid[++k]);

         if (NCL_lv94)
            status = um_make_fillets (e,radius,last,0,jsub,slab,UU_TRUE);
         if (status != UU_SUCCESS) e[0].key = e[1].key;
         else cmdst = 1;
			if (jsub != 0) jsub++;

         }   /* end of inner while */
       if (UU_application == UU_NCLCAM)
			{ 
          jsub = sub;
          if (cmdst == 1) 
            {
            nclu_fill_cmd (slab, jsub, eid, nent, radius);
            if (NCL_lv94)
            {
      		i1 = 1; 
            namgen(&i1,&i2,UM_addr_of_f77_str(fstr),&sub);	
   			for (i=0;i<NCL_MAX_LABEL;i++) if (slab[i] == ' ') break; 
				slab[i] = '\0';
            }
            }
          uu_free (eid);
			}
    }   /* end of outer while */

done:;
   ud_lgeo(UU_FALSE, UD_lncir);
   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : um_make_fillets(e,radius,last,nclkey,
**                                     subsc,label)
**      Fillet lines.
**    PARAMETERS   
**       INPUT  : 
**          e      - pointer to two conected curves to insert fillet arc
**          radius - fillet radius
**          last   - flag marking the last curve to check if the first
**                   curve in list is connected in closed contour.
**          nclkey - circle entity key_id if allredy used. 
**          subsc  - circle entity (fillet) subscript
**          label  - circle entity label
**          errflag - UU_TRUE = Output error messages.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_make_fillets(e,radius,last,nclkey,subsc,label,errflag)
struct UM_crvdatabag e[2];          /* two picked entities */
UU_REAL radius;                     /* radius of tangent circle */
UU_LOGICAL last;                    /* last pair of entities? */
int subsc;
char *label;
UU_KEY_ID nclkey;
UU_LOGICAL errflag;
{
   struct UM_circle_rec c;            /* new tangent circle */
   UM_coord npt[2];                   /* nearest point to center of tangent 
                                         circle on each picked entity */
   UM_vector tan;                     /* tangent on fillet circle */
   UM_vector vec;                     /* vector along line */
   UM_vector svec;                    /* vector along line */
   UM_coord    ipt;                   /* intersection point of two curves */
   UM_angle dang;
   UM_length arclen;                  /* arc length of curve */
   UU_LOGICAL err;                    /* error flag */
   int i, status;      

/*
.....Adding itype and lablst to determine if labels need to 
.....be displayed.
*/
	UM_int2 itype, lblst, istat;
	UM_int4 jlblst;
	UM_real8 ver;

   uu_denter( UU_MTRC,(us,"um_make_fillets()"));

	status = UU_SUCCESS;

	itype = 169;
	getsc(&itype,&ver);

   ur_retrieve_data_relnum(e[0].key, &e[0].rel_num);
   ur_retrieve_data_relnum(e[1].key, &e[1].rel_num);
   um_get_all_geom(&e[0], sizeof(struct UM_crvdatabag));
   um_get_all_geom(&e[1], sizeof(struct UM_crvdatabag));
   ur_setup_data(UM_CIRCLE_REL, &c, sizeof(struct UM_circle_rec));
   /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
   strcpy (c.label, "");
   c.subscr = 0;

   um_fill_arc(e, radius, last, &c, &err, npt, ipt, errflag);
   if (err) goto repeat;

	for (i = 0; i < 2; i++)
	{
           um_vcmnvc(npt[i],c.center,tan);
           um_cross(c.nvec,tan,tan);
           um_unitvc(tan,tan);
           switch (e[i].rel_num)
              {
               case  UM_LINE_REL:
                  {
                   struct UM_line_rec *ptr;
                   ptr = (struct UM_line_rec *) &e[i];
                   if (um_cceqcc(ptr->spt, ipt))
                      um_vctovc(npt[i], ptr->spt);
                   else
                      um_vctovc(npt[i], ptr->ept);
                   arclen = um_dcccc(ptr->spt, ptr->ept);
                   if (arclen < UM_FUZZ)
                      {
                       uu_uerror0(/*filleted line too small to be 
                           trimmed*/ UM_MODEL,106);
                       goto repeat;
                      }
                  }
                   break;
               case  UM_CIRCLE_REL:
                  {
                   struct UM_circle_rec *ptr;
                   ptr = (struct UM_circle_rec *) &e[i];
                   um_vctmsc(ptr->svec, ptr->radius, svec);
                   um_vcplvc(ptr->center, svec, svec);
                   um_vcmnvc(npt[i], ptr->center, vec);
                   um_unitvc(vec, vec);
                   um_vcmnvc(ipt, ptr->center, tan);
                   um_unitvc(tan, tan);
                   dang = um_angle (vec, tan);
                   if (fabs(dang) < fabs(ptr->dang))
                      {
                       ptr->dang = ptr->dang - dang;
                       if (um_cceqcc(svec, ipt))
                          {
                           um_vctovc(vec, ptr->svec);
                          }
                      }
                   else 
                      {
                       arclen = ptr->radius * fabs(ptr->dang - dang);
                       if (arclen < UM_FUZZ)
                          {
                           uu_uerror0(/*filleted arc too small to 
                                 be trimmed*/ UM_MODEL,108);
                           goto repeat;
                          }
                       else
                          {
                           uu_uerror0(/*can't auto trim entity*/UM_MODEL,109);
                           goto repeat;
                          }
                      }

                  }   /* case circle rel */
                  break;
               default:
                  uu_uerror0(/*wrong type picked; pick a line,
                  circle, or arc*/UM_MODEL,33);
                  goto repeat;
              }
	}
	for (i=0; i < 2; i++)
	{
		um_update_geom(&e[i], UM_DEFAULT_TF);
		uc_display(&e[i]);
	}
/*
.....Set label if not autoname generating
*/
	if (subsc > 0) 
	{
		strncpy (c.label,label, NCL_MAX_LABEL);
		c.subscr = subsc;
		NCL_ubcopy = 1;
	}
	else if (ver >= 9.449 && NCL_ubcopy == 0)
	{
		istat = 0;
		ncl_label_wf(c.rel_num, c.label, &c.subscr, c.key, &istat);
		if (istat != 0 && istat != 1) goto repeat;
		NCL_ubcopy = 1;
	}	

	if (nclkey > NULLKEY)
	{
		uc_delete (nclkey);
		ncl_randel (nclkey,UM_CIRCLE_REL);
	} 
	um_create_geom(&c, UM_DEFAULT_TF, UM_CURRENT_ATTR);
	NCL_ubcopy = 0;
	if (UU_application == UU_NCLCAM) ncl_def_color (c.key);

	itype = 7;
	lblchk(&itype,&lblst);

	if ( (lblst == 1)||(lblst == 5) && ver >= 9.349)
	{
		struct NCL_nclattr_rec attr;

		uc_retrieve_attr(c.key, &attr);
		attr.label_on = lblst;
		ur_update_attr(&attr);
	}

	uc_display(&c);
/* 
.....Checking to see if labels need to be displayed and if they
.....do, labset1 will display them.
*/
	if (lblst == 1 && ver < 9.349)
	{
		jlblst = lblst;
		labset1(&jlblst);
	}
	e[0].key = e[1].key;

	goto done;

repeat:;
	status = UU_FAILURE;

done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : umu_m2_multi_chamfer()
**         Same as umu_m2_chamfer(), except the user can select any
**         number of line pairs to chamfer, using single select or
**         chain select.  The distance and angle entered by the user
**         are used for every line pair.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m2_multi_chamfer()

   {
   int         numint;               /* the number of interactions input */
   struct         UM_line_rec  e0;  /* the first line entity */ 
   struct         UM_line_rec  e1;  /* the first line entity */ 
   struct         UM_line_rec  e2;  /* the second line entity */
   struct         UM_line_rec   lc;   /* the chamfer line entity */
   UM_vector      v1;               /* vector of the first line */
   UM_vector      v2;               /* vector of the second line */
   UM_vector      u1;               /* unit vector of the first line */
   UM_vector      u2;               /* unit vector of the second line */
   UM_coord         ipt;               /* the intersection between the
                                       first and second lines */
   UM_length      dist1;
   UM_length      dist2;
   UM_length      angc;
   UM_angle         ang1;
   UM_angle         ang2;
   UM_vector       vc1, vc2;
   UU_LOGICAL     um_ptbtwn();
   UU_LOGICAL      gnxt_init;
   UU_LOGICAL      cont;
   UU_LOGICAL      last_pair;

/*--------------------------------------------------------------------
**   Start of Executable Code
**--------------------------------------------------------------------
**
**   Prompt the user for input
*/
   uu_denter(UU_MTRC,(us,"umu_m2_multi_chamfer()"));

   while (UU_TRUE)
      {
      gnxt_init = UU_TRUE;
      last_pair = UU_FALSE;

      /* limit DAS to editable lines */
      ud_lgeo(UU_TRUE, UD_line);
      ud_leditable(UU_TRUE);
      ud_ldas(UD_DASSELECT,/* Pick a connected sequence of lines */
         UM_MODEL,264,UU_NULL,1,&numint,UD_NODEFAULT);
      if (numint <= 1) goto done;

      ud_ldas(UD_DASDISTANCE, /* distance (along 1st line) */UM_MODEL, 12, 
               &dist1, 1, &numint, UD_NODEFAULT);
      if (numint <= 0) goto done;

      /* limit angle input to +/- 180deg */
      ud_lpra(UU_TRUE, -(UM_PI), UM_PI);
      ud_ldas(UD_DASANGLE, /* angle */UM_MODEL, 13, &ang2, 1, 
               &numint, UD_NODEFAULT);
      ud_lpra(UU_FALSE, -(UM_PI), UM_PI);
      if (numint <= 0) goto done;

   /* ------------------------------------
   ** get entities out of select buffer **
      ------------------------------------ */
   
      ud_gnxt(gnxt_init, UU_NULL, &e0.key, 1);
      e1.key = e0.key;
      gnxt_init = UU_FALSE;
      cont = UU_TRUE;

      while (cont)
         {
         cont = ud_gnxt(gnxt_init, UU_NULL, &e2.key, 1);
         if (cont == UU_FALSE)
            {
            last_pair = UU_TRUE;
            e2.key = e1.key;
            e1.key = e0.key;
            }

         /*-------------------------------------------------------
         **
         **   Calculate the intersection point
         */

         ur_retrieve_data_relnum(e1.key, &e1.rel_num);
         ur_retrieve_data_relnum(e2.key, &e2.rel_num);
         if (e1.rel_num != UM_LINE_REL || e2.rel_num != UM_LINE_REL)
            {
            uu_uerror0(/* Selected entity not a line */UM_MODEL,14);
            goto repeat;
            }

         um_get_all_geom(&e1, sizeof(struct UM_line_rec));
         um_get_all_geom(&e2, sizeof(struct UM_line_rec));
         um_vcmnvc( e1.ept, e1.spt, v1 );
         um_unitvc( v1, u1 );
         um_vcmnvc( e2.ept, e2.spt, v2 );
         um_unitvc( v2, u2 );
         if(um_cceqcc(e1.spt,e2.spt) == UU_TRUE  ||
            um_cceqcc(e1.spt,e2.ept) == UU_TRUE) um_vctovc(e1.spt,ipt);
         else if(um_cceqcc(e1.ept,e2.spt) == UU_TRUE  ||
            um_cceqcc(e1.ept,e2.ept) == UU_TRUE) um_vctovc(e1.ept,ipt);
         else
               {
               /* it's not an error of the last pair of lines aren't
               ** connected - i.e. lines don't form a closed curve */

               if (! last_pair)
                  uu_uerror0( /* entities not connected */UM_MODEL,238);
               goto repeat;
               }
      
      /*-----------------------------------------------------------------------
      **
      **   Calculate the point on the first line a distance back from the
      **   intersection and update the endpoints of the first line and
      **   create the starting point for the chamfer line.
      */
         um_vctmsc( u1, dist1, v1 );
         if(um_cceqcc(e1.spt,ipt) == UU_TRUE)
            {
            um_vcplvc( ipt, v1, e1.spt );
            um_vctovc( e1.spt, lc.spt );
            if ( um_ptbtwn( e1.ept, e1.spt, ipt )==UU_FALSE )
               {
               uu_uerror0( /* chamfer distance entered is too large */
                  UM_MODEL,17);
               goto repeat;
               }
            }

         else if(um_cceqcc(e1.ept,ipt) == UU_TRUE)
            {
            um_vcmnvc( ipt, v1, e1.ept );
            um_vctovc( e1.ept, lc.spt );
            if ( um_ptbtwn( e1.spt, e1.ept, ipt )==UU_FALSE )
               {
               uu_uerror0( /* chamfer distance entered is too large */
                  UM_MODEL,17);
               goto repeat;
               }
            }
      
      /*-----------------------------------------------------------------------
      **
      **   Calculate the point on the second line at an angle from the first
      **   line and update the endpoints of the second line and create the
      **   ending point for the chamfer line.
      */

         if(um_dcccc(ipt,e1.spt) < um_dcccc(ipt,e1.ept) )
               {
            um_vcmnvc(e1.ept,ipt,vc1);
               }
         else
               {
            um_vcmnvc(e1.spt,ipt,vc1);
               }
         if(um_dcccc(ipt,e2.spt) < um_dcccc(ipt,e2.ept) )
               {
            um_vcmnvc(e2.ept,ipt,vc2);
               }
         else
               {
            um_vcmnvc(e2.spt,ipt,vc2);
               }
         angc = um_angle(vc1,vc2);
         if(ang2 < 0.0 ) ang2 = UM_PI + ang2;
         if( ang2 + angc >= UM_PI )
            {
            uu_uerror0( /* chamfer angle entered is too large*/
               UM_MODEL,18);
            goto repeat;
            }
         ang1 = UM_PI - ang2 - angc;
         if ( sin(ang1) < UM_FUZZ )
            {
            uu_uerror0(/*chamfer angle entered is too large*/
               UM_MODEL,18);
            goto repeat;
            }
         dist2 = sin(ang2) * dist1 / sin(ang1);
         um_vctmsc( u2, dist2, v2 );
         if(um_cceqcc(e2.spt,ipt) == UU_TRUE)
            {
            um_vcplvc( ipt, v2, e2.spt );
            um_vctovc( e2.spt, lc.ept );
            if ( um_ptbtwn( e2.ept, e2.spt, ipt )==UU_FALSE )
               {
               uu_uerror0( /* chamfer angle entered is too large*/
                  UM_MODEL,18);
               goto repeat;
               }
            }
         else if (um_cceqcc(e2.ept, ipt) == UU_TRUE)
            {
            um_vcmnvc( ipt, v2, e2.ept );
            um_vctovc( e2.ept, lc.ept );
            if ( um_ptbtwn( e2.spt, e2.ept, ipt )==UU_FALSE )
               {
               uu_uerror0( /*chamfer angle entered is too large*/
                  UM_MODEL,18);
               goto repeat;
               }
            }
      /*-----------------------------------------------------------------------
      **
      **   Update the lines in unibase and redisplay them
      */
         ur_setup_data(UM_LINE_REL, &lc, sizeof(struct UM_line_rec));
         /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
         strcpy (lc.label, "");
         lc.subscr = 0;
         um_create_geom(&lc , UM_DEFAULT_TF, UM_CURRENT_ATTR); 
         uc_display(&lc);
         um_update_geom( &e1 , UM_DEFAULT_TF); 
         uc_display(&e1);
      
         um_update_geom( &e2 , UM_DEFAULT_TF);
         uc_display(&e2);

         repeat:;
         e1.key = e2.key;
         }   /* end of inner while */
      }   /* end of outer while */
   done:;
      ud_lgeo(UU_FALSE, UD_line);
      uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : um_get_id (key,eid)
**       Gets identity data of the entity defined by its key 
**
**    PARAMETERS   
**       INPUT  : 
**          key   - entity key.
**       OUTPUT :  
**          eid   - identity data of entity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
 um_get_id (key,eid)
  UU_KEY_ID key;
  struct NCL_id_rec *eid;
 {
  struct NCL_fixed_databag e;
  if (key > NULLKEY)
    {
     eid->key = e.key = key;
     ur_retrieve_data_fixed (&e);
     eid->rel_num = e.rel_num;
     strncpy (eid->label,e.label, NCL_MAX_LABEL);
     eid->subscr = e.subscr;
    }
  return (UU_SUCCESS);
 }
