/*******************************************************************
**    NAME         :  m3etrim.c
**       CONTAINS:
**      int um_nearest_isect_to_ploc(ploc,npts,ibuff)
**      int um_extend_curve(ploc,pt,u,eptr)
**      int um_trim_extend_curve(ploc,pt,u,eptr)
**      int um_midtrim_curve(ploc, isect1, isect2, eptr)
**      log um_is_curve_closed (eptr,tfmat)
**      int um_join_midtrim_split (eptr3,eptr1,eptr)
**      int um_ploc_nr_curve(ploc, eptr)
**      int um_is_pt_nr_curve(pt, eptr)
**      int um_redef_curve (eptr,tfmat)
**      int um_isect_curves (crv1p, tf1, crv2p, tf2, ipt, npt, nint, no_ibuf,
**                           ibuf)
**      int um_isect_lnln_np (l1p,tf1,l2p,tf2,nint,ibuf)
**      int um_isect_lnci_np (l1p,tf1,c1p,tf2,nint,ibuf)
**      int um_isect_lncv_np (l1p,tf1,c1p,tf2,npt,nint,ibuf)
**      int um_isect_ciln_np (c1p,tf1,l1p,tf2,nint,ibuf)
**      int um_isect_cici_np (c1p,tf1,c2p,tf2,nint,ibuf)
**      int um_isect_cipt_np (c1p,tf1,p1p,tf2,nint,ibuf)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3etrim.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:55
*********************************************************************/
#include "zsysdep.h"
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "class.h"
#include "mcrv.h"
#include "mattr.h"
#include "modef.h"
#include "misect.h"
#include "mdeval.h"
#include "mdpick.h"
#include "mdebug.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclvx.h"
#include "nclmodals.h"

extern int NCL_ubcopy, UM_RD_subscr;
extern char UM_RD_name[32];

/*********************************************************************
**    E_FUNCTION     : int um_nearest_isect_to_ploc(ploc,npts,ibuff)
**      Given an array of intersection points (i.e. cartesian modeling
**      coordinates and parameter values) and a pick location (NDC space),
**      return the index of the closest point to the pick location.
**    PARAMETERS   
**       INPUT  : 
**        ploc           pick location
**          npts           number of points in array
**          ibuff          intersection buffer of points (world  coordinate)
**                  and parameter values
**       OUTPUT :  
**          none
**    RETURNS      : 
**      index of closest point
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_nearest_isect_to_ploc(ploc,npts,ibuff)
  UD_NDCLOCREC *ploc;
  int npts;
  UM_isect ibuff[];

  {
  int closest;            /* index of closest point */
  UU_REAL dist, mindist;      /* distance (minimum distance) to point */
  UU_REAL um_dploccc();
  int i;                /* index */

  uu_denter( UU_MTRC,(us,"um_nearest_isect_to_ploc(?,?,?)"));
  if (npts  ==  0) closest = -1;
  else
    {
    for (i = 0; i < npts; i++)
      {
      dist = um_dploccc(ploc, ibuff[i].pt);
      if ( (i == 0) || (dist < mindist) )
        {
        closest = i;
        mindist = dist;
        }
      }
    }
  uu_dexit;
  return (closest);
  }

/*********************************************************************
**    E_FUNCTION     : int um_extend_curve(cvend,pt,u,udel,eptr)
**      Given a point (PT) which lies outside of the parameter range
**      of a curve (EPTR) at a parameter value U, update the definition
**      of the curve (in UNIBASE and DIGS) to be defined over the
**      new parameter range. Note that some curves (e.g. rational
**      bsplines) can not be extended because they are only defined
**      over the interval 0.0 to 1.0.  The picked location (PLOC)
**      determines if the start point (or end point) of the curve
**      is to be replaced by the given point (PT).
**    PARAMETERS   
**       INPUT  : 
**        cvend       ? not used 
**        pt          new point on (possibly infinite) curve
**        u           parameter value of new point
**        udel        parameter value of pick point (used with closed CV)
**        eptr        curve entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**      UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**      Unpredictable results may occur if the curve is closed.
*********************************************************************/
int
um_extend_curve(cvend, pt,u,udel,eptr)
  UM_coord cvend, pt;
  UM_param u, *udel;
  struct UM_crvdatabag  *eptr;

  {
  int status, clsd;
  UM_coord spt, ept;
  struct UM_conic_rec  *cn;
  char label[NCL_MAX_LABEL+1];
  int subscr;
  UU_KEY_ID key;

  uu_denter(UU_MTRC,(us,"um_extend_curve(pt=%x,u=%f,key=%d)",
    pt, u, eptr->key));

  /*RAH: lets preserve the key, label and subscr */
  key    = eptr->key;
  strncpy(label, eptr->label, NCL_MAX_LABEL);
  subscr = eptr->subscr;
/*
...Assume all curves closed
*/
  clsd   = 1;
  status = UU_SUCCESS;
  switch (eptr->rel_num)
   {
	 case UM_COMPCRV_REL:
    case UM_CIRCLE_REL:
      break;
    case UM_CONIC_REL:
      cn     = (struct UM_conic_rec *) eptr;
      if (cn->type != UM_ELLIPSE) clsd = 0;
      break;
    case UM_LINE_REL:
    case UM_RBSPLCRV_REL:
    case NCL_CURVE_REL:
      clsd = 0;
      break;
    default:
      status = UU_FAILURE;
      break;
   }
   if (status != UU_SUCCESS) return (status);
   um_get_endpts(eptr, UM_idmat, spt, ept);
/*
...Define which direction extend curve using u parameter
...for open curves
*/
   if (clsd == 0)
      if (u > 0.5)
        um_vctovc(pt, ept);
      else
        um_vctovc(pt, spt);
/*
...and for closed curves
*/
   else
      if (*udel > .5)
        um_vctovc (pt,ept);
      else
        um_vctovc (pt,spt);
          
   switch (eptr->rel_num)
     {
      case UM_LINE_REL:
          status = um_c2_pp(spt, ept, eptr);
          break;
      case UM_CIRCLE_REL:
          status = um_c3_endpoints(eptr, spt, ept, UM_DEFAULT_TF);
          break;
      case UM_CONIC_REL:
          um_cn4_endpoints(eptr, spt, ept, UM_DEFAULT_TF);
          status = UU_SUCCESS;
          break;
      case UM_RBSPLCRV_REL:
          status = um_c7_endpoints(eptr, u, udel);
          break;
      case UM_COMPCRV_REL:
          status = um_c5_endpoints(eptr, u, pt, udel);
          break;
      case NCL_CURVE_REL:
          status = ncl_c7_endpoints(eptr, u, udel);
          break;
      default:
          status = UU_FAILURE;
          break;
     }
/* RAH: Now restore the key, label and subscr */
  eptr->key = key;
  strncpy(eptr->label, label, NCL_MAX_LABEL);
  eptr->subscr = subscr;

  uu_dexit;
  return (status);
  }
/*********************************************************************
**    E_FUNCTION     : int um_trim_extend_curve(crv, tfmat, pt,
**                                 uall, udel)
**      Trim or extend the first curve (CRV1, TFMAT1) to one of the 
**      intersection points with the second curve (CRV2, TFMAT2).
**      If more that one intersection point is found, the user will
**      be prompted to pick close to an intersection point to use.
**      The definition of the curve is updated in both UNIBASE and DIGS.
**    PARAMETERS   
**       INPUT  : 
**        crv       curve to trim/extend
**        tfmat     transformation matrix for curve (may be UM_DEFAULT_TF)
**        pt        new point where curve is extended (when extend)
**        uall      list of all IOs (u values)
**        udel      u parameter at pick point of CV
**       OUTPUT :  
**          none
**    RETURNS      : 
**      UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**      Some curves (e.g. rational bsplines) can not be extended.
*********************************************************************/
int
um_trim_extend_curve(crv, tfmat, pt, uall, udel)
  struct UC_entitydatabag *crv;
  UM_transf *tfmat;
  UM_param *uall, udel;
  UM_coord pt;

  {
  int status;
  int size;
  struct UM_crvdatabag *part1;  /* first portion of split curve */
  struct UM_crvdatabag *part2;  /* second portion of split curve */
  struct UM_crvdatabag *e;     /* portion to keep */
  struct UM_crvdatabag *crv1;
  struct UM_rbsplcrv_rec *rb;
  struct UM_compcrv_rec *ccrv;
  UM_coord cvend;
  UM_param u, delp;
  int i,rel;

  uu_denter(UU_MTRC,(us,"um_trim_extend_curve(trimkey=%x, trimtokey=%x)",
    crv1->key, crv2->key));

  crv1 = (struct UM_crvdatabag *) crv;
  part1 = part2 = UU_NULL;
/*
.....UV curves cannot be trimmed so composite curves with a UV curve 
.....will not work either.
*/
  if (crv->rel_num == UM_COMPCRV_REL)
  {
	  ccrv = (struct UM_compcrv_rec *)crv;
	  for (i=0;i<ccrv->no_cid;i++)
	  {
		  ur_retrieve_data_relnum(ccrv->cid[i].crvid,&rel);
		  if (rel == UM_UVCVONSF_REL)
		  {
			  status = UU_FAILURE;
			  goto done;
		  }
	  }
  }
/* 
...Split curve or extend
*/
  u   = uall[0];

  if ((0.0 < u) && (u < 1.0))
  {
    delp = udel;
    size = um_curve_size (crv1);
    um_allocate_curve (&part1,size);
    um_allocate_curve (&part2,size);
    status = um_splitcurve(crv1, uall, &delp, part1, part2);
    if (status != UU_SUCCESS) goto done;
/* 
...Determine which portion to keep, update the original curve 
...in UNIBASE, and display it 
*/
       if (delp < u) e = part2; else e = part1;
       e->key = crv1->key;
       strncpy(e->label, crv1->label, NCL_MAX_LABEL);
       e->subscr = crv1->subscr;
/*
... aak 06-nov-1997: if e is a rbspl copy of a uv-curve, restore the uv-curve; 
*/
       rb = (struct UM_rbsplcrv_rec *) e;
       if(ncl_itis_uvcv(rb) )
       {
          struct UM_uvcvonsf_rec uvcv;
			 status = ncl_conv_trim_rbcv_uvcv(rb,&uvcv);
          if (status != UU_SUCCESS) goto done;
          e = (struct UM_crvdatabag *)(&uvcv);
       }
  }
/*
...Extend curve
*/
  else
  {
    status = um_extend_curve(cvend, pt, u, &udel, crv1);
    if (status == UU_SUCCESS)
    {
/*
... aak 06-nov-1997: if e is a rbspl copy of a uv-curve, restore the uv-curve;
*/
       e = crv1;
       rb = (struct UM_rbsplcrv_rec *) e;
       if( ncl_itis_uvcv(rb) )
       {
          struct UM_uvcvonsf_rec uvcv;
			 status = ncl_conv_trim_rbcv_uvcv(rb,&uvcv);
          if (status != UU_SUCCESS) goto done;
          e = (struct UM_crvdatabag *)(&uvcv);
       }
    }
  }

  if (status == UU_SUCCESS) 
  {
     status = ur_update_data_fixed (e); 
     if (status == UU_SUCCESS) uc_display(e);
  }

done:;
  if (part1 != UU_NULL) uu_toolfree ((struct UM_crvdatabag *)part1);
  if (part2 != UU_NULL) uu_toolfree ((struct UM_crvdatabag *)part2);
  uu_dexitstatus("um_trim_extend_curve", status);
  return (status);
  }

/*********************************************************************
**    E_FUNCTION     : int um_midtrim_curve(eptr, tfmat, pt1, pt2, uall,
**                                          uend)
**      Use the intersection buffers (ISECT1 and ISECT2) to split the
**      curve entity (EPTR) into three pieces (note: the t0 parameter
**      value is assumed to be on the curve EPTR). Then use the picked
**      location (PLOC) to determine if the middle piece or the two
**      end pieces of the curve should be discarded (i.e. throw away
**      the middle piece if the user picked on that segment).
**    PARAMETERS   
**       INPUT  : 
**        eptr      curve entity
**        tfmat     transformation matrix for curve (may be UM_DEFAULT_TF)
**        pt1       IO point with first trimer (not used here)
**        pt2       IO point with second trimer (not used here)
**        uall      parameters u for IO points with trimmers
**        uend      u parameter at pick point of CV
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
um_midtrim_curve(eptr, tfmat, pt1, pt2, uall, uend)
  struct UM_crvdatabag *eptr;
  UM_transf *tfmat;
  UM_param *uall, uend;
  UM_coord pt1, pt2;

 {
  int status, midfl, size, join, a, b;
  struct UM_crvdatabag *eptr1, *eptr2, *eptr3, *eptr4;
  struct UM_crvdatabag *ecv1, *ecv2, *ecv3, *ecv4;
  struct UM_attrdata_rec attr;
  UM_param dumm, ufirst, usecond, uio[10];
  UU_LOGICAL um_is_curve_closed();
  UU_REAL arclen1, arclen2, arclen3;
  struct UM_rbsplcrv_rec *rb;
  struct UM_compcrv_rec *ccrv;
  int i,rel;

  uu_denter(UU_MTRC,(us,"um_midtrim_curve(ploc=%x, isect1=%x,isect2=%x,key=%d",
    ploc, isect1, isect2, eptr->key));

  status = UU_FAILURE;
  midfl  = 1;
  eptr1  = eptr2 = eptr3 = eptr4 = UU_NULL;
/*
.....Also initialize ecv1,ecv2,ecv3,ecv4 to UU_NULL, otherwise there could
.....be a problem if the program goes to done before there is an assignment of
.....information to ecv1 thru ecv4.  JLS 3/2/99
*/
	ecv1 = ecv2 = ecv3 = ecv4 = UU_NULL;
/*
.....UV curves cannot be trimmed so composite curves with a UV curve 
.....will not work either.
*/
  if (eptr->rel_num == UM_COMPCRV_REL)
  {
	  ccrv = (struct UM_compcrv_rec *)eptr;
	  for (i=0;i<ccrv->no_cid;i++)
	  {
		  ur_retrieve_data_relnum(ccrv->cid[i].crvid,&rel);
		  if (rel == UM_UVCVONSF_REL)
		  {
			  status = UU_FAILURE;
			  goto done;
		  }
	  }
  }
/*
...allow mid trim only if parameter values of intersection points lie
...in (0.0, 1.0) parameter range (for open curves) 
*/
  if (!um_is_curve_closed(eptr, UM_DEFAULT_TF))
    {
     if ((uall[0] <= 0.0) || (uall[0] >= 1.0)) goto done;
     if ((uall[1] <= 0.0) || (uall[1] >= 1.0)) goto done;
     join  = 0;
    }
  else
     join  = 1;
/* 
...determine the parameter values to split the orginal curve into three
...pieces; we assume linear parameterization in determining  the parameter
...value for splitting the first part of the first split of the orginal curve 
*/
  a = 1; b = 0;
  if (uall[1] < uall[0])
  {
	  a = 0; b = 1;
  }
  ufirst = uall[a];
  if (eptr->rel_num != UM_COMPCRV_REL)
	  usecond = uall[b] / uall[a];
  else
	  usecond = uall[b];
  if (uend < uall[b] || uend > uall[a]) midfl = 0;
  uio[0] = ufirst;
  uio[2] = usecond;
/*
...prevent spliting closed curves on second i/o
*/
  uio[1] = uio[3] = -1.0;

  size = um_curve_size (eptr);
  um_allocate_curve (&eptr1,size);
  um_allocate_curve (&eptr2,size);
  um_allocate_curve (&eptr3,size);
  um_allocate_curve (&eptr4,size);
  ecv1 = eptr1;
  ecv2 = eptr2;
  ecv3 = eptr3;
  ecv4 = eptr4;
/* 
...split the orginal curve (eptr) into three pieces eptr1, eptr2, eptr3 
...do not split 0 length curves
*/
  dumm   = uend;
  status = um_splitcurve(eptr, &uio[0], &dumm, ecv4, ecv3);
  if (status != UU_SUCCESS) goto done;
  if (uio[2] > 0.)
      status = um_splitcurve(ecv4, &uio[2], &dumm,  ecv1, ecv2);
  else
    {
     join  = 0;
     if (midfl == 1) 
       {
        eptr1 = ecv3; 
        eptr2 = ecv4; 
       }
     else
       {
        eptr1 = ecv4; 
        eptr2 = ecv3; 
       }
    }
  if (status != UU_SUCCESS) goto done;
/*
...now determine the portion of the original curve to keep 
*/
  status = UU_FAILURE;
/* 
...keep the interior portion 
*/
/*  if (um_ploc_nr_curve(ploc, eptr2) == 0)   */
  if (midfl == 1) 
  {
     eptr2->key = eptr->key;
     arclen2 = um_getarclen(eptr2, UM_idmat);
     if (arclen2 < UM_FUZZ)
     {
        uu_uerror0(/* trimed entity too small */ UM_MODEL, 71);
     }
     else
     {
       strncpy(eptr2->label, eptr->label, NCL_MAX_LABEL);
       eptr2->subscr = eptr->subscr;
/*
... aak 06-nov-1997: if it's a rbspl copy of a uv-curve, restore the uv-curve;
*/
       rb = (struct UM_rbsplcrv_rec *) eptr2;
       if( ncl_itis_uvcv(rb) )
       {
          struct UM_uvcvonsf_rec uvcv;
			 status = ncl_conv_trim_rbcv_uvcv(rb,&uvcv);
          if (status != UU_SUCCESS) goto done;
          eptr2 = (struct UM_crvdatabag *)(&uvcv);
       }

       if ((status = ur_update_data_fixed (eptr2)) == UU_SUCCESS)
       {
          uc_display(eptr2);
       }
     }
  }
  else
/* 
...keep the two exterior portions in one piece 
...join two exterior parts into single curve 
*/
  {
     if (um_is_curve_closed(eptr, UM_DEFAULT_TF) &&
		  eptr->rel_num != UM_COMPCRV_REL)
       {
       if (join == 1)
           status = um_join_midtrim_split(eptr3, eptr1, eptr2);
       else
           status = 0;
       if (status == 0)
       {
          eptr2->key = eptr->key;
          arclen2 = um_getarclen(eptr2, UM_idmat);
          if (arclen2 < UM_FUZZ)
          {
             uu_uerror0(/* trimed entity too small */ UM_MODEL, 71);
          }
          else
          {
             strncpy(eptr2->label, eptr->label, NCL_MAX_LABEL);
             eptr2->subscr = eptr->subscr;
/*
... aak 06-nov-1997: if eptr is a rbspl copy of a uv-curve, restore the uv-curve;
*/
		       rb = (struct UM_rbsplcrv_rec *) eptr2;
       		 if(ncl_itis_uvcv(rb) )
      		 {
         		struct UM_uvcvonsf_rec uvcv;
          		status = ncl_conv_trim_rbcv_uvcv(rb,&uvcv);
          		if (status != UU_SUCCESS) goto done;
          		eptr2 = (struct UM_crvdatabag *)(&uvcv);
				 }
             if ((status = ur_update_data_fixed (eptr2))
                        == UU_SUCCESS)
             {
                uc_display(eptr2);
             }
          }
       }
     }
    else
/* 
...create two parts separated 
*/
    {
       eptr1->key = eptr->key;
       arclen1 = um_getarclen(eptr1, UM_idmat);
       arclen3 = um_getarclen(eptr3, UM_idmat);
       if ((arclen1 < UM_FUZZ) || (arclen3 < UM_FUZZ))
       {
          uu_uerror0(/* trimed entity too small */ UM_MODEL, 71);
       }
       else
       {
          strncpy(eptr1->label, eptr->label, NCL_MAX_LABEL);
          eptr1->subscr = eptr->subscr;
/*
... aak 06-nov-1997: if eptr is a rbspl copy of a uv-curve, restore the uv-curve;
*/
	       rb = (struct UM_rbsplcrv_rec *) eptr1;
       	 if(ncl_itis_uvcv(rb) )
      	 {
         	struct UM_uvcvonsf_rec uvcv;
          	status = ncl_conv_trim_rbcv_uvcv(rb,&uvcv);
          	if (status != UU_SUCCESS) goto done;
          	eptr1 = (struct UM_crvdatabag *)(&uvcv);
			 }
          if ((status = ur_update_data_fixed (eptr1)) == UU_SUCCESS)
          {
             uc_display(eptr1);
/*
...get attributes from original curve but default label location
*/
             um_get_disp_attr(eptr1->key, &attr);
/*
.....if label is bit is set to altered reset it
*/
             if (ncl_get_label_alter(attr.label_on)) 
					attr.label_on = ncl_set_label_on(&attr.label_on,0);
/*
...if in CAM command mode, applay a label supplied by user
*/
             if (NCL_ubcopy == 1)
             {
                 strcpy (eptr3->label,UM_RD_name);
                 eptr3->subscr = UM_RD_subscr;
             }
/*
... aak 06-nov-1997: if eptr is a rbspl copy of a uv-curve, restore the uv-curve;
*/
				rb = (struct UM_rbsplcrv_rec *) eptr3;
				rb->key = eptr->key;
       	   if(ncl_itis_uvcv(rb) )
      	 	{
         		struct UM_uvcvonsf_rec uvcv;
          		status = ncl_conv_trim_rbcv_uvcv(rb,&uvcv);
          		if (status != UU_SUCCESS) goto done;
          		eptr3 = (struct UM_crvdatabag *)(&uvcv);
			 	}
             um_create_geom(eptr3, UM_DEFAULT_TF, &attr);
             if (NCL_ubcopy == 0)
             {
                 strncpy (UM_RD_name,eptr3->label, NCL_MAX_LABEL);
                 UM_RD_subscr = eptr3->subscr; 
             }
             uc_display(eptr3);
          }
       }
    }
  }

done:
/*
.....Changed 'eptr' to 'ecv' on following lines
.....Because 'eptr1' & 'eptr2' can be changed above
.....causing bad memory deallocation
.....Bobby  -  5/5/97
*/
  if (ecv1 != UU_NULL) uu_toolfree ((struct UM_crvdatabag *)ecv1);
  if (ecv2 != UU_NULL) uu_toolfree ((struct UM_crvdatabag *)ecv2);
  if (ecv3 != UU_NULL) uu_toolfree ((struct UM_crvdatabag *)ecv3);
  if (ecv4 != UU_NULL) uu_toolfree ((struct UM_crvdatabag *)ecv4);
  uu_dexitstatus("um_midtrim_curve", status);
  return (status);
 }

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_is_curve_closed(eptr, tfmat)
**       Determine if a curve is closed.
**    PARAMETERS   
**       INPUT  : 
**          eptr       pointer to curve
**          tfmat      entity transformation (may be UM_DEFAULT_TF)
**       OUTPUT :  
**          none
**    RETURNS      : 
**      UU_TRUE if curve is closed; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL 
um_is_curve_closed(eptr, tfmat)
  struct UM_crvdatabag *eptr;
  UM_transf tfmat;

  {
  UU_LOGICAL status;
  struct UM_evcrvout evcrv;
  UM_coord spt, ept;

  uu_denter(UU_MTRC,(us,"um_is_curve_closed(key=%d, tfmat=%x)",
    eptr->key,tfmat));

  uc_init_evcrvout(eptr, &evcrv);
  uc_evcrv(UM_POINT, (UU_REAL) 0.0, eptr, tfmat, &evcrv);
  um_vctovc(evcrv.cp, spt);
  uc_evcrv(UM_POINT, (UU_REAL) 1.0, eptr, tfmat, &evcrv);
  um_vctovc(evcrv.cp, ept);
  status = um_cceqcc(spt, ept);

  uu_dexit;
  return (status);
  }
/*********************************************************************
**    E_FUNCTION     : int um_join_midtrim_split(eptr3, eptr1, eptr)
**      Join curves EPTR3 and EPTR1 which are the two end pieces of
**      a closed curve which has been mid-trimed. Thus, the end
**      point of EPTR3 is the start point of EPTR1 and the new
**      curve goes from EPTR3 to EPTR1. (NEED SUPPORT FOR RBSPLINES!).
**    PARAMETERS   
**       INPUT  : 
**          eptr3          pointer to first part of curve
**          eptr1          pointer to second parr of curve
**       OUTPUT :  
**          eptr          pointer to resultant curve
**    RETURNS      : 
**      0 iff success; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_join_midtrim_split(eptr3, eptr1, eptr)
  struct UM_crvdatabag *eptr3;
  struct UM_crvdatabag *eptr1;
  struct UM_crvdatabag *eptr;

  {
  int status;
  struct UM_evcrvout evcrv;
  UM_coord spt, ept;

  struct UM_circle_rec *circle;
  UU_REAL ang;
  UM_vector svec, evec;

  struct UM_conic_rec *conic;
  struct UM_conic_rec *conic3;

  uu_denter(UU_MTRC,(us,"um_join_midtrim_split(key3=%d, key1=%d, eptr=%x)",
    eptr3->key, eptr1->key, eptr));

  status = -1;
  uc_init_evcrvout(eptr3, &evcrv);
  uc_evcrv(UM_POINT, (UU_REAL) 0.0, eptr3, UM_DEFAULT_TF, &evcrv);
  um_vctovc(evcrv.cp, spt);
  uc_init_evcrvout(eptr1, &evcrv);
  uc_evcrv(UM_POINT, (UU_REAL) 1.0, eptr1, UM_DEFAULT_TF, &evcrv);
  um_vctovc(evcrv.cp, ept);
  if (eptr3->rel_num == UM_CIRCLE_REL)
    {
    circle = (struct UM_circle_rec *) eptr3;
    um_vcmnvc(spt, circle->center, svec);
    um_unitvc(svec, svec);
    um_vcmnvc(ept, circle->center, evec);
    um_unitvc(evec, evec);
    ang = um_angle2p(svec, evec, circle->nvec);
    status = um_c3_arccnaarb(circle->center, circle->nvec, (UU_REAL) 0.0, ang,
      circle->svec, circle->radius, eptr);
    }
  else if (eptr3->rel_num == UM_CONIC_REL)
    {
    conic = (struct UM_conic_rec *) eptr;
    conic3 = (struct UM_conic_rec *) eptr3;
    zbytecp(*conic, *conic3);
    conic->key = -1;
    status = um_cn4_endpoints(conic, spt, ept, UM_DEFAULT_TF);
    }

  uu_dexit;
  return (status);
  }

/*********************************************************************
**    E_FUNCTION     : int um_ploc_nr_curve(ploc, eptr)
**       Determine if the picked location (PLOC) is close enough to
**      the given curve entity (EPTR) to indicate that this curve
**      was the one selected.
**    PARAMETERS   
**       INPUT  : 
**          ploc          pick location
**        eptr          pointer to curve entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**      0 iff entity is close enough to the picked location; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_ploc_nr_curve(ploc, eptr)
  UD_NDCLOCREC *ploc;
  struct UM_crvdatabag *eptr;

  {
  int status;

  uu_denter(UU_MTRC,(us,"um_ploc_nr_curve(ploc=%x,key=%d)",ploc,
    eptr->key));

  status = -1;
  switch (eptr->rel_num)
    {
    case UM_LINE_REL:
      {
      struct UM_line_rec *e;      /* recast curve to line */
      UM_vector vpnorm;          /* vector to project along */
      UM_coord cc;            /* cartesian coordinate form of ploc */
      UM_coord projploc;        /* project pick loc to plane */
      UM_coord projept;          /* project line ept to plane */
      int nint;              /* number of intersections */

      e = (struct UM_line_rec *) eptr;
      um_ploctocc(ploc, cc);
      um_vpnorm(ploc->transform, vpnorm);
      um_ilnpln(cc, vpnorm, e->spt, vpnorm, &nint, projploc);
      um_ilnpln(e->ept, vpnorm, e->spt, vpnorm, &nint, projept);
      if (nint == 1)
        {
        if (um_ptbtwn(e->spt, projploc, projept) == UU_TRUE) status = 0;
        }
      }
      break;
    case UM_CIRCLE_REL:
      {
      struct UM_circle_rec *e;
      UU_REAL ang;
      UM_vector evec;
      UM_coord projploc;

      e = (struct UM_circle_rec *) eptr;
      um_projploctopln(ploc, e->center, e->nvec, projploc);
      um_vcmnvc(projploc, e->center, evec);
      um_unitvc(evec, evec);
      ang = um_angle2p(e->svec, evec, e->nvec);
      if (e->dang < 0.0) ang = UM_TWOPI - ang;
      if (fabs(ang) < fabs(e->dang)) status = 0;
      }
      break;
    case UM_CONIC_REL:
      {
      struct UM_conic_rec *e;
      struct UM_circle_rec circ;
      struct UM_crvdatabag *ecirc;
      struct UM_evcrvout evcrv;
      UM_vector evec;
      UM_coord epts[2];
      UM_coord projploc;

      e = (struct UM_conic_rec *) eptr;
      if (e->type == UM_ELLIPSE)
        {
        uc_init_evcrvout(e, &evcrv);
        uc_evcrv(UM_POINT, (UU_REAL) 0.0, e, UM_DEFAULT_TF, &evcrv);
        um_vctovc(evcrv.cp, epts[0]);
        uc_evcrv(UM_POINT, (UU_REAL) 1.0, e, UM_DEFAULT_TF, &evcrv);
        um_vctovc(evcrv.cp, epts[1]);
    
        /** -- fill in circle rec (with "squished" ellipse)  --  **/
        circ.rel_num = UM_CIRCLE_REL;
        circ.radius  = e->invariants[1];
        um_vctovc(e->tfmat[3], circ.center);
        um_vctovc(e->tfmat[2], circ.nvec);
        um_vcmnvc(epts[0], circ.center, circ.svec);
        um_unitvc(circ.svec, circ.svec);
        um_vcmnvc(epts[1], circ.center, evec);
        um_unitvc(evec, evec);
        circ.dang = um_angle2p(circ.svec, evec, circ.nvec);
        if (fabs(circ.dang) < UM_FUZZ) circ.dang = UM_TWOPI;
        ecirc = (struct UM_crvdatabag *) &circ;
        status = um_ploc_nr_curve(ploc, ecirc);
        }
      else
        {
        um_projploctopln(ploc, e->tfmat[3], e->tfmat[2], projploc);
        status = um_is_pt_nr_curve(projploc, e);
        }
      }
      break;
    case UM_RBSPLCRV_REL:
      {
      struct UM_rbsplcrv_rec *projcrv;
      UM_coord vrefpt;
      UM_coord projploc;
      UM_vector vpnorm;
/*
.....vp 20-feb-97 allocate space for projected curve equal to 
.....input curve
*/
		projcrv = UU_NULL;
		projcrv = (struct UM_rbsplcrv_rec *) uu_toolmalloc (um_curve_size(eptr));
      um_vpnorm(ploc->transform, vpnorm);
      um_vrefpt(ploc->transform, vrefpt);
      um_projploctopln(ploc, vrefpt, vpnorm, projploc);
      um_proj_geom_to_plane(eptr, vrefpt, vpnorm, projcrv);
      status = um_is_pt_nr_curve(projploc, projcrv);
		if (projcrv != UU_NULL) uu_toolfree (projcrv);
      }
      break;
    default:
      status = -1;
      break;
    }
  uu_dexit;
  return (status);
  }

/*********************************************************************
**    E_FUNCTION     : int um_is_pt_nr_curve(pt, eptr)
**       Determine if the given point (PT in MCS) is near the specified
**      curve (EPTR).
**    PARAMETERS   
**       INPUT  : 
**          pt              point coordinates (MCS)
**        eptr            curve entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**      0 iff point is on curve; -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_is_pt_nr_curve(pt, eptr)
  UM_coord pt;
  struct UM_crvdatabag *eptr;

  {
  UU_REAL u, delta;
  struct UM_evcrvout evcrv;
  UM_coord crvpts[20];
  int npts, closest;
  int i;
  int status;

  uu_denter(UU_MTRC,(us,"um_is_pt_nr_curve(pt=(%f,%f,%f),key=%x)",
    pt[0], pt[1], pt[2], eptr->key));

  npts = 10;
  delta = 1.0/npts;
  for (i=0, u=0.0; i<=npts; i++, u=u+delta)
    {
    uc_evcrv(UM_POINT, u, eptr, UM_DEFAULT_TF, &evcrv);
    um_vctovc(evcrv.cp, crvpts[i]);
    }
  closest = um_ccnearcc(pt, npts+1, crvpts);
  if ((closest > 0) && (closest < npts)) status = 0; else status = -1;
  uu_dexit;
  return (status);
  }

/*********************************************************************
**    E_FUNCTION     : int um_redef_curve(eptr,tfmat)
**       Dispatching routine to redefine the general curve 'eptr'.
**    PARAMETERS   
**       INPUT  : 
**        eptr      curve entity
**        tfmat     transformation matrix for curve (may be UM_DEFAULT_TF)
**       OUTPUT :  
**          none
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_redef_curve(elst,tfmat)
struct UM_crvdatabag *elst[3];
UM_transf *tfmat;
{
  int status;
  struct UM_rbsplcrv_rec *rb;
  struct UM_crvdatabag *eptr;

  eptr = (struct UM_crvdatabag *) elst[0];
  switch (eptr->rel_num)
  {
    case UM_LINE_REL:
      status = um_redef_line(eptr);
      break;
    case UM_CIRCLE_REL:
      status = um_redef_circle(eptr);
      break;
    case UM_CONIC_REL:
      status = um_redef_conic(eptr,tfmat);
      break;
    case UM_RBSPLCRV_REL:
      status = um_redef_rbsplcrv(eptr);
/*
... aak 06-nov-1997: if this rbspl curve is a clone of a uv-curve on surface, 
... restore the original uv-curve;
*/
      rb = (struct UM_rbsplcrv_rec *) eptr;
      if(ncl_itis_uvcv(rb) )
      {
         struct UM_uvcvonsf_rec uvcv;
         status = ncl_conv_trim_rbcv_uvcv(rb,&uvcv);
         if (status != UU_SUCCESS) goto done;
         eptr = (struct UM_crvdatabag *)(&uvcv);
      }
      break;    
    case NCL_CURVE_REL:
      status = ncl_redef_curve(eptr);
      break;    
    case UM_COMPCRV_REL:
      status = um_redef_compcrv(eptr);
      break;

    default:
      status = UU_FAILURE;
      break;
  }

  if (status == UU_SUCCESS)
      status = ur_update_data_fixed (eptr);

  if (status == UU_SUCCESS) uc_display(eptr);

done:;
  uu_dexit;
  return (status);
}
/*********************************************************************
**    E_FUNCTION: um_isect_curves (crv1p, tf1, crv2p, tf2, ipt, nint, no_ibuf,
**                                  ibuf)
**      Intersect 2 possibly non-planar curves. For most cases, call a routine
**      which projects one or both of the entities onto an appropriate plane 
**      (usually the Z plane), then calls uc_crv_intersect(). If the
**      intersection fails, call uc_crv_intersect() using original entities
**      in case the projection failed but the entities are planar. BUT for
**      the intersection of a line with a curve, call uc_crv_intersect()
**      first. If this is successful it will find all intersections (and
**      create a REDEF a statement without a near point in it). If it is
**      not successful, call the non-planar line-curve intersection routine
**      which will find one intersection near the near point.
**    PARAMETERS
**       INPUT  :
**          crv1p   - First curve.
**          tf1     - Transformation of first curve.
**          crv2p   - Second curve.
**          tf2     - Transformation of second curve.
**          ipt     - =0, use planar routine to intersect line/curve
**                    =1, use non-planar routine to intersect line/curve
**                    =2, use planar routine, if it fails use non-planar
**          no_ibuf - Maximum number of intersections ibuf can hold.
**       OUTPUT :
**          ipt     - =0 if planar routine to intersect line/curve was used
**                    =1 if non-planar routine to intersect line/curve was used
**          npt     - near point used in non-planar line/curve intersection
**          nint    - Number of intersections.
**          ibuf    - Intersections.
**    RETURNS      : UU_SUCCESS for successful intersection; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_isect_curves (crv1p, tf1, crv2p, tf2, ipt, npt, nint, no_ibuf, ibuf)
struct UM_crvdatabag *crv1p, *crv2p;
UM_transf tf1, tf2;
int *ipt;
UM_coord npt;
int *nint, no_ibuf;
UM_isect ibuf[];
{
	int lpt, status = UU_FAILURE;
	UM_int2 i2type;

	*nint = 0;
	if (crv1p->rel_num == UM_LINE_REL)
	{
		if (crv2p->rel_num == UM_LINE_REL)
			status = um_isect_lnln_np (crv1p, tf1, crv2p, tf2, nint, ibuf);
		else if (crv2p->rel_num == UM_CIRCLE_REL)
			status = um_isect_lnci_np (crv1p, tf1, crv2p, tf2, nint, ibuf);
		else
		{
			lpt = *ipt;
			if (lpt != 1)
				status = uc_crv_intersect(crv1p, tf1, crv2p, tf2, nint, no_ibuf,
				                          ibuf);
			if (lpt == 2) *ipt = 0;
			if (lpt > 0 && status != UU_SUCCESS)
			{
				status = ncl_get_type (crv2p->rel_num, &i2type);
				if (i2type == NCLI_CURVE)
				{
				status = um_isect_lncv_np (crv1p, tf1, crv2p, tf2, npt, nint, ibuf);
				if (status == UU_SUCCESS) *ipt = 1;
				}
			}
		}
	}
	else if (crv1p->rel_num == UM_CIRCLE_REL)
	{
		if (crv2p->rel_num == UM_LINE_REL)
			status = um_isect_ciln_np (crv1p, tf1, crv2p, tf2, nint, ibuf);
		else if (crv2p->rel_num == UM_CIRCLE_REL)
			status = um_isect_cici_np (crv1p, tf1, crv2p, tf2, nint, ibuf);
		else if (crv2p->rel_num == UM_POINT_REL)
			status = um_isect_cipt_np (crv1p, tf1, crv2p, tf2, nint, ibuf);
	}
	if (*nint == 0)
		status = uc_crv_intersect(crv1p, tf1, crv2p, tf2, nint, no_ibuf, ibuf);

	return (status);
}
/*********************************************************************
**    E_FUNCTION: um_isect_lnln_np (l1p,tf1,l2p,tf2,nint,ibuf)
**      Intersect 2 non-planar lines in the Z direction.
**    PARAMETERS
**       INPUT  :
**          l1p   - First line.
**          tf1   - Transformation of first line.
**          l2p   - Second line.
**          tf2   - Transformation of second line.
**       OUTPUT :
**          nint  - Number of intersections (0 or 1).
**          ibuf  - Intersection.
**    RETURNS      : UU_SUCCESS for successful intersection; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_isect_lnln_np (l1p,tf1,l2p,tf2,nint,ibuf)
struct UM_line_rec *l1p, *l2p;
UM_transf tf1, tf2;
int *nint;
UM_isect ibuf[];
{
	int status = UU_FAILURE;
	UM_vector v1, v2, vz;
	UM_coord  p0;
	struct UM_line_rec ll1, ll2;
	UM_int2 ifl;
	UM_transf lref_tm, lmod_tm;

	*nint = 0;
	ll1 = *l1p;
	ll2 = *l2p;
/*
.....Transform zaxis thru refsys or modsys matrices, if necessary.
*/
	p0[0] = p0[1] = p0[2] = vz[0] = vz[1] = 0.0;
	vz[2] = 1.0;
	gtref (lref_tm,&ifl);
	if (ifl)
	{
		um_vctmtf (vz,lref_tm,vz);
	}
	gtmod (lmod_tm,&ifl);
	if (ifl)
	{
		um_vctmtf (vz,lmod_tm,vz);
	}
/*
.....Project lines to refsys/modsys xy-plane.
*/
	um_unitvc (vz, vz);
	um_nptpln (ll1.spt, p0, vz, ll1.spt);
	um_nptpln (ll1.ept, p0, vz, ll1.ept);
	um_nptpln (ll2.spt, p0, vz, ll2.spt);
	um_nptpln (ll2.ept, p0, vz, ll2.ept);
	um_vcmnvc (ll1.ept, ll1.spt, v1);
	um_vcmnvc (ll2.ept, ll2.spt, v2);
/*
.....If both projected lines have real length, intersect.
*/
	if (um_mag(v1) > UM_FUZZ && um_mag(v2) > UM_FUZZ)
	{
		status = uc_crv_intersect (&ll1, tf1, &ll2, tf2, nint, 1, ibuf);
/*
.....If intersection was successful, calculate intersection point on first
.....line.
*/
		if (*nint > 0)
		{
			um_vcmnvc (l1p->ept, l1p->spt, v1);
			um_vctmsc (v1,ibuf->u0,v1);
			um_vcplvc (l1p->spt, v1, ibuf->pt);
		}
	}
	return (status);
}
/*********************************************************************
**    E_FUNCTION: um_isect_lnci_np (l1p,tf1,c1p,tf2,nint,ibuf)
**      Intersect a non-planar line and circle in the direction
**      normal to the line and in the plane containing the line
**      and the circle axis.
**    PARAMETERS
**       INPUT  :
**          l1p   - Pointer to line.
**          tf1   - Transformation of line.
**          c1p   - Pointer to circle.
**          tf2   - Transformation of circle.
**       OUTPUT :
**          nint  - Number of intersections.
**          ibuf  - Intersections.
**    RETURNS      : UU_SUCCESS for successful intersection; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_isect_lnci_np (l1p,tf1,c1p,tf2,nint,ibuf)
struct UM_line_rec *l1p;
struct UM_circle_rec *c1p;
UM_transf tf1, tf2;
int *nint;
UM_isect ibuf[];
{
	int i, status = UU_FAILURE;
	UM_vector vln;
 	UM_vector vtmp;
 	UM_vector vn;
	UM_coord  spt, ept;
	struct UM_line_rec ll1;
	UU_REAL um_dcccc();

	*nint = 0;
	ll1 = *l1p;

	um_vcmnvc (ll1.ept, ll1.spt, vln);
	um_cross (vln,c1p->nvec, vtmp);
	um_cross (vtmp, vln, vn);
	um_unitvc (vn,vn);
/*
.....Project line into plane of circle
*/
	um_nptpln (ll1.spt, c1p->center, c1p->nvec, spt);
	um_nptpln (ll1.ept, c1p->center, c1p->nvec, ept);
/*
.....If projected line has real length, intersect it with the circle.
*/
	if (um_dcccc(spt, ept) > UM_FUZZ)
	{
		um_vctovc (spt, ll1.spt);
		um_vctovc (ept, ll1.ept);
		status = uc_crv_intersect (&ll1, tf1, c1p, tf2, nint, 2, ibuf);
/*
.....Calculate intersection point(s) on original line.
*/
		for (i=0;i<*nint;i++)
		{
			um_nptpln (ibuf[i].pt, l1p->spt, vn, ibuf[i].pt);
/* 			um_vctmsc (vln,ibuf[i].u0,vtmp); */
/* 			um_vcplvc (l1p->spt, vtmp, ibuf[i].pt); */
		}
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION: um_isect_lncv_np (l1p,tf1,c1p,tf2,npt,nint,ibuf)
**      Intersect a non-planar line and curve in the direction
**      of the z axis.
**    PARAMETERS
**       INPUT  :
**          l1p   - Pointer to line.
**          tf1   - Transformation of line.
**          c1p   - Pointer to curve.
**          tf2   - Transformation of curve.
**          npt   - Near point.
**       OUTPUT :
**          nint  - Number of intersections.
**          ibuf  - Intersections.
**    RETURNS      : UU_SUCCESS for successful intersection; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_isect_lncv_np (l1p,tf1,c1p,tf2,npt,nint,ibuf)
struct UM_line_rec *l1p;
struct UM_crvdatabag *c1p;
UM_transf tf1, tf2;
UM_coord npt;
int *nint;
UM_isect ibuf[];
{
	int i, j, status = UU_SUCCESS;
	UM_vector vln;
 	UM_vector vn;
 	UM_vector vz;
	UM_coord  nptt, cpt;
	UM_coord  p0;
	struct UM_evcrvout evcv;
	int done, itim;
	UM_param u, ust, und, uhld;
	UU_REAL dhld;
	UU_REAL du, uerr, oerr, den, utan, d1;
	UM_int2 ifl;
	UM_transf lref_tm, lmod_tm;
	UU_LOGICAL clsd, um_is_curve_closed();
	UU_REAL um_dcccc();

	*nint = 0;
/*
.....Transform zaxis thru refsys or modsys matrices, if necessary.
*/
	p0[0] = p0[1] = p0[2] = 0.0;
	vz[0] = vz[1] = 0.0;
	vz[2] = 1.0;
	gtref (lref_tm,&ifl);
	if (ifl)
	{
		um_vctmtf (vz,lref_tm,vz);
		um_cctmtf (p0,lref_tm,p0);
	}
	gtmod (lmod_tm,&ifl);
	if (ifl)
	{
		um_vctmtf (vz,lmod_tm,vz);
		um_cctmtf (p0,lmod_tm,p0);
	}
	um_unitvc (vz, vz);
/*
.....Get near point on curve.
*/
	um_nptpln (npt, p0, vz, nptt);
	uc_init_evcrvout(c1p, &evcv);
	ust = 0.0;
	und = 1.0;
	for (i=0; i<3; i++)
	{
		u = ust;
		du = (und-ust) / 16.0;
		for (j=0; j<17; j++)
		{
			if (u>1.0) u = 1.0;
			uc_evcrv (UM_POINT, u, c1p, tf2, &evcv);
			um_nptpln (evcv.cp, p0, vz, cpt);
			d1 = um_dcccc (nptt,cpt);
			if (j == 0 || d1 < dhld)
			{
				dhld = d1;
				uhld = u;
			}
			u += du;
		}
		ust = uhld-du;
		if (ust < 0.0) ust = 0.0;
		und = uhld+du;
		if (und > 1.0) und = 1.0;
	}

	u = uhld;
/*
.....Converge on plane containing line & perp to xy plane (vn,d1).
*/
	um_vcmnvc (l1p->ept, l1p->spt, vln);
	um_cross (vln, vz, vn);
	if (um_mag(vn) < UM_FUZZ) status = UU_FAILURE;
	um_unitvc (vn,vn);
	d1 = um_dot(l1p->spt, vn);

	utan = 1.0;
	done = itim = 0;
	clsd = um_is_curve_closed(c1p, tf2);
	while (!status && !done)
	{
		uc_evcrv (UM_POINT, u, c1p, tf2, &evcv);
		uerr = d1 - um_dot(vn,evcv.cp);
		if (fabs(uerr) < UM_FUZZ)
		{
		  done = 1;
		}
		else
		{
			if (itim == 0)
			{
				du = .01;
				if (u+du > 1.0) du = -.01;
			}
			else
			{
				den = oerr-uerr;
				if (fabs(den) > UM_FUZZ) utan = uerr/den;
				du = du*utan;
				if (u+du > 1.0) du = 1.0-u;
				if (u+du < 0.0) du = -u;
			}
			oerr = uerr;
			u += du;
			if (fabs(du) < 1.0e-8)
			{
				if (uerr < UM_FUZZ * 10.0)
				{
					done = 1;
				}
/*
.....If we are at the end of a closed curve, switch to the other end.
*/
				else if (clsd)
				{
					if (u > .999) u = 0.0;
					else if (u < .001) u = 1.0;
				}
			}
			itim++;
			if (itim > 32) status = UU_FAILURE;
		}
	}
/*
.....Calculate intersection point on original line.
*/
	if (status == UU_SUCCESS)
	{
		*nint = 1;
		um_cross (vn, vln, vn);
		um_unitvc (vn,vn);
		um_nptpln (evcv.cp, l1p->spt, vn, ibuf->pt);
		d1 = um_mag (vln);
		if (d1 > UM_FUZZ)
			ibuf->u0 = um_dcccc(ibuf->pt, l1p->spt) / d1;
		else
			ibuf->u0 = 0.0;
		ibuf->u1 = u;
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION: um_isect_ciln_np (c1p,tf1,l1p,tf2,nint,ibuf)
**      Intersect a circle with a non-planar line in the direction
**      of the circle axis.
**    PARAMETERS
**       INPUT  :
**          c1p   - Pointer to circle.
**          tf1   - Transformation of circle.
**          l1p   - Pointer to line.
**          tf2   - Transformation of line.
**       OUTPUT :
**          nint  - Number of intersections.
**          ibuf  - Intersection.
**    RETURNS      : UU_SUCCESS for successful intersection; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_isect_ciln_np (c1p,tf1,l1p,tf2,nint,ibuf)
struct UM_circle_rec *c1p;
struct UM_line_rec *l1p;
UM_transf tf1, tf2;
int *nint;
UM_isect ibuf[];
{
	int status = UU_FAILURE;
	UM_coord  spt, ept;
	struct UM_line_rec ll1;
	UU_REAL um_dcccc();

	*nint = 0;
	ll1 = *l1p;
/*
.....Project line into plane of circle
*/
	um_nptpln (ll1.spt, c1p->center, c1p->nvec, spt);
	um_nptpln (ll1.ept, c1p->center, c1p->nvec, ept);
/*
.....If projected line has real length, intersect it with the circle.
*/
	if (um_dcccc(spt, ept) > UM_FUZZ)
	{
		um_vctovc (spt, ll1.spt);
		um_vctovc (ept, ll1.ept);
		status = uc_crv_intersect (c1p, tf1, &ll1, tf2, nint, 2, ibuf);
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION: um_isect_cici_np (c1p,tf1,c2p,tf2,nint,ibuf)
**      Intersect a circle with a non-planar but parallel circle
**      in the direction of the circles axes.
**    PARAMETERS
**       INPUT  :
**          c1p   - Pointer to first circle.
**          tf1   - Transformation of first circle.
**          c2p   - Pointer to second circle.
**          tf2   - Transformation of second circle.
**       OUTPUT :
**          nint  - Number of intersections.
**          ibuf  - Intersections.
**    RETURNS      : UU_SUCCESS for successful intersection; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_isect_cici_np (c1p,tf1,c2p,tf2,nint,ibuf)
struct UM_circle_rec *c1p;
struct UM_circle_rec *c2p;
UM_transf tf1, tf2;
int *nint;
UM_isect ibuf[];
{
	int status = UU_FAILURE;
	struct UM_circle_rec lc2;
	UU_REAL um_dot();

	*nint = 0;

	if (fabs(um_dot(c1p->nvec,c2p->nvec)) > .99995)
	{
		lc2 = *c2p;
		um_nptpln (lc2.center, c1p->center, c1p->nvec, lc2.center);
		status = uc_crv_intersect (c1p, tf1, &lc2, tf2, nint, 2, ibuf);
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION: um_isect_cipt_np (c1p,tf1,p1p,tf2,nint,ibuf)
**      Intersect a circle with a point.
**    PARAMETERS
**       INPUT  :
**          c1p   - Pointer to circle.
**          tf1   - Transformation of circle.
**          p1p   - Pointer to point.
**          tf2   - Transformation of point.
**       OUTPUT :
**          nint  - Number of intersections.
**          ibuf  - Intersections.
**    RETURNS      : UU_SUCCESS for successful intersection; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_isect_cipt_np (c1p,tf1,p1p,tf2,nint,ibuf)
struct UM_circle_rec *c1p;
struct UM_point_rec *p1p;
UM_transf tf1, tf2;
int *nint;
UM_isect ibuf[];
{
	int status;
	struct UM_point_rec lp1;

	lp1 = *p1p;
	um_nptpln (lp1.pt, c1p->center, c1p->nvec, lp1.pt);
	status = uc_crv_intersect (c1p, tf1, &lp1, tf2, nint, 2, ibuf);

	return (status);
}
