/*********************************************************************
**    NAME         :  nept.c
**       CONTAINS: routines to handle NCL points
**
**       int ncl_point_to_nclpt(e, ncl)
**       int ncl_nclpt_to_point(ncl, e)
**       ncl_nclpt_to_unipt(nclpt, unipt)
**       ncl_p_nclpt(ncl)
**       ncl_p88_point(ptr)
**       drwpt(nclkey)
**       ncl_point_transl(e, offset)
**       ncl_isect_evcv
**       ncl_isect_refine2
**       ncl_isect_refine1
**       ncl_isect_refine
**       ncl_isect_3dc
**       ncl_isect_3db
**       ncl_isect_3da
**       ncl_isect_3d
**       ncl_isect_2d
**       ncl_pt_intof_cv(nkey1,nkey2,pti,pto,ierr)
**       void ncl_pt_on_cv
**       int ncl_percnt_on_cv
**		 ncl_is_pt_on_curve
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nept.c , 25.2
**     DATE AND TIME OF LAST  MODIFICATION
**       08/17/15 , 17:41:17
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "ginq.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdebug.h"
#include "misect.h"
#include "mderror.h"
#include "mdgenent.h"
#include "mdeval.h"
#include "modef.h"

#include "ncl.h"
#include "nccs.h"
#include "nclfc.h"
#include "uminmax.h"
#include "nclvx.h"

extern int NCL_ubcopy;

typedef struct
{
	struct NCL_fixed_databag *e1;
	UM_transf tf1;
	struct UM_evcrvout *evcv1;
	UM_real8 u2;
	UM_int4 key2;
	UM_int2 iwf2;
} S_2cvdat;

/*********************************************************************
**    E_FUNCTION     : int ncl_point_to_nclpt(e, ncl)
**       Convert a UNICAD point to an NCL  point.
**    PARAMETERS   
**       INPUT  : 
**          e						UNICAD point entity
**       OUTPUT :  
**          ncl					buffer to place NCL point
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_point_to_nclpt(e, ncl)
	struct NCL_nclpt_rec *e;
	struct NCLI_point_rec *ncl;

	{
	int status;

	uu_denter(UU_MTRC,(us,"ncl_point_to_nclpt(key=%x, ncl=%x)",
		e->key, ncl));

	status = UU_SUCCESS;
	ncl_uureal_to_real8(3, e->pt, ncl->pt);
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_nclpt_to_point(ncl, e)
**       Convert an NCL point to a UNICAD point.
**    PARAMETERS   
**       INPUT  : 
**          ncl						buffer holding NCL point
**       OUTPUT :  
**          e							UNICAD point entity
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_nclpt_to_point(ncl, e)
	struct NCLI_point_rec *ncl;
	struct NCL_nclpt_rec *e;

	{
	int status;

	uu_denter(UU_MTRC,(us,"ncl_nclpt_to_point(ncl=%x, e=%x)",
		ncl, e));
	status = UU_SUCCESS;
	e->rel_num = NCL_POINT_REL;

	/* If key exists, retrieve entity */
	if (e->key != 0)
		status = ur_retrieve_data(e, sizeof(struct NCL_nclpt_rec));
	else
		{
		e->markertype = UM_ptattr.markertype;
		e->snap_node = UM_ptattr.snap_node;
		}

	if (status != UU_SUCCESS)
		goto done;

	ncl_real8_to_uureal(3, ncl->pt, e->pt);

done:;
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_nclpt_to_unipt(nclpt, unipt)
**       Convert an NCL point to a UNICAD point.
**    PARAMETERS   
**       INPUT  : 
**          nclpt						NCL point representation
**       OUTPUT :  
**          unipt						UNICAD point representation
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_nclpt_to_unipt(nclpt, unipt)
	struct NCL_nclpt_rec *nclpt;
	struct UM_point_rec *unipt;

	{
	int status;
	char label[100];

	uu_denter(UU_MTRC,(us,"ncl_nclpt_to_unipt(key=%x)", nclpt->key));

	/* added  to correct the label on drawing.  kathy */
	ncl_get_label_with_key(nclpt->key, label);
	strcpy (unipt->label, label);

	status = UU_SUCCESS;
	unipt->rel_num = UM_POINT_REL;
	unipt->markertype = nclpt->markertype;
	unipt->snap_node = nclpt->snap_node;
	um_vctovc(nclpt->pt, unipt->pt);
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_unipt_to_nclpt(unipt, nclpt)
**       Convert a UNICAD point to an NCL point.
**    PARAMETERS   
**       INPUT  : 
**          unipt						UNICAD point representation
**       OUTPUT :  
**          nclpt						NCL internal point representation
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_unipt_to_nclpt(unipt, nclpt)
	struct UM_point_rec *unipt;
	struct NCLI_point_rec *nclpt;

	{
	int status;
   int i;

	uu_denter(UU_MTRC,(us,"ncl_unipt_to_nclpt(key=%x)", unipt->key));
	status = UU_SUCCESS;
   for (i=0; i<3; i++) nclpt->pt[i] = unipt->pt[i];
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : ncl_p_nclpt(ncl)
**       Print an NCL point.
**    PARAMETERS   
**       INPUT  : 
**          ncl					buffer holding NCL point.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p_nclpt(ncl)
	struct NCLI_point_rec *ncl;

	{
	uu_denter(UU_MTRC,(us,"ncl_p_nclpt(ncl=%x)",ncl));
	sprintf(UM_sbuf,"NCLPOINT:");
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf,"  pt=(%g,%g,%g)", ncl->pt[0], ncl->pt[1], ncl->pt[2]);
	um_pscroll(UM_sbuf);
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : ncl_p88_point(ptr)
**       Print the contents of an NCL point record.
**    PARAMETERS   
**       INPUT  : 
**				ptr 	            pointer to point record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_p88_point(ptr)
	struct  NCL_nclpt_rec  *ptr;

	{

	uu_denter(UU_MTRC,(us,"ncl_p88_point(key=%x)",ptr->key));

	sprintf(UM_sbuf, "POINT %d", ptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "label %s", ptr->label);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "subscr %d", ptr->subscr);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PFLOAT, "pt", 3, ptr -> pt);
	um_p_ary(UM_PINT,"markertype",1,&ptr->markertype);
	um_p_ary(UM_PINT,"snap_node",1,&ptr->snap_node);

	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : drwpt(nclkey)
**       Display a point in the current open segment.
**    PARAMETERS   
**       INPUT  : 
**				nclkey				UNIBASE key of point entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added parameter tranf
.....allowed pass in tranformation
.....which added in default tranformation
.....Yurong 9/30/97
*/
drwpt(nclkey, tranf)
	UU_KEY_ID *nclkey;
	UM_transf tranf;

	{
	struct  NCL_nclpt_rec  e;
	UM_transf tfmat;
	struct NCL_nclattr_rec attr;
	UM_coord pt;
	UM_coord ptout;
	Gwpoint3 gpt;					/* markers to send to GKS */
	int markertype;				/* type of DIGGS marker */
	int status;

	uu_denter(UU_MTRC,(us,"drwpt(key:%d)", *nclkey));

	e.key = *nclkey;
	status =	uc_retrieve_data(&e, sizeof(e));
	if (status == UU_SUCCESS) uc_retrieve_transf(e.key, tfmat);
	if (status == UU_SUCCESS) uc_retrieve_attr(e.key, &attr);
	if (status == UU_SUCCESS)
		{
	
		gsmarkcolor(attr.color);
		markertype = gqmarktype();	/* get current marker type */
		gsmarktype(e.markertype);
		um_cctmtf(e.pt, tfmat, pt);
/*
.....added by Yurong
.....9/30/97
*/
		um_cctmtf(pt, tranf, ptout);

		gpt.x = ptout[0];
		gpt.y = ptout[1];
		gpt.z = ptout[2];
		gpolymarker3(1,&gpt);
		gsmarktype(markertype);	/* reset DIGGS marker type */
		}
	uu_dexit;
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_point_transl(e, offset)
**       Translate an NCL point along a vector.
**    PARAMETERS   
**       INPUT  : 
**          e             NCL point entity
**          offset        vector
**       OUTPUT : 
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_point_transl(e, offset)
   struct NCL_nclpt_rec *e;
   UM_vector offset;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_point_transl(key=%x)",
      e->key));

   status = UU_SUCCESS;

   um_vcplvc(e->pt, offset, e->pt);
   um_update_geom(e, UM_DEFAULT_TF);

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_isect_evcv (u1,e1,tf1,evcrv,u2,key2,pt2,d)
**       Evaluate the first curve at u1, and project result onto second.
**    PARAMETERS   
**       INPUT  : 
**          u1            Parameter on first curve
**          e1            First curve struct pointer
**          tf1           First curve transformation
**          u2            Starting parameter for second curve
**          key2          Key of second curve.
**          iwf2          Wireframe flag of second curve.
**       OUTPUT : 
**          evcrv         First curve evaluated at u1
**          pt2           Point on second curve.
**          d             Squared distance between two curve points
**    RETURNS      : 
**       UU_SUCCESS iff all OK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_isect_evcv (u1,e1,tf1,evcrv,u2,key2,iwf2,pt2,d)
UU_REAL u1,u2,*d;
struct NCL_fixed_databag *e1;
UM_transf tf1;
struct UM_evcrvout *evcrv;
UU_KEY_ID key2;
UM_int2 iwf2;
UM_coord pt2;
{
	int status, iflg = 0, unflg = 1, ier;
	UM_coord pt1;
	UM_vector vc2;

	status = uc_evcrv(UM_POINT,u1,e1,tf1,evcrv);
	if (status != UU_SUCCESS) return (status);
	um_vctovc (evcrv->cp,pt1);

/* maybe call um_midtrim_curve on e2 */

	cvpv2(&key2,&iwf2,pt1,&iflg,&u2,&unflg,pt2,vc2,&ier);
	if (ier) return(ier);
	
	*d = um_sqdis (pt1,pt2);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int fdis (u,cvs,fu)
**       Evaluate the first curve at u1, project result onto second,
**       return the squared distance between the points on the two curves 
**    PARAMETERS   
**       INPUT  : 
**          u             Parameter on first curve
**          cvs           the two curves data
**       OUTPUT : 
**          fu            Squared distance between two curve points
**    RETURNS      : 
**       UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int fdis (u,cvs,fu)
UU_REAL u,*fu;
S_2cvdat *cvs;
{
	int istat;
	UM_coord pt2;

	istat = ncl_isect_evcv (u,cvs->e1,cvs->tf1,cvs->evcv1,cvs->u2,cvs->key2,
		cvs->iwf2,pt2,fu);
	return (istat);
}

/*********************************************************************
**    E_FUNCTION     : ncl_isect_refine2 (e1,tf1,a,u1,b,evcrv1,pt1,key2,u2,pt2,
**                                                             d0,EPS)
**       Find the closest pair of points on two curves by using the brent
**       method for the minimum of a one-parameter function.
**    PARAMETERS   
**       INPUT  : 
**          e1            First curve struct pointer
**          tf1           First curve transformation
**          u1            Starting parameter on first curve
**          (a,b)         Parameter range for first curve
**          evcrv1        Evaluator struct to reuse
**          key2          Key of second curve.
**          iwf2          Wireframe flag of second curve.
**          u2            Starting parameter for second curve
**          EPS           Epsilon value for success
**       OUTPUT : 
**          pt1           Point on second curve.
**          pt2           Point on second curve.
**          d0            Squared distance between two curve points
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_isect_refine2 (e1,tf1,a,u1,b,evcrv1,pt1,key2,iwf2,u2,pt2,dmin,eps)
struct NCL_fixed_databag *e1;
UM_transf tf1;
UU_REAL a,u1,b,u2,*dmin,eps;
struct UM_evcrvout *evcrv1;
UU_KEY_ID key2;
UM_int2 iwf2;
UM_coord pt1,pt2;
{
	int status;
	S_2cvdat cvs;
	UU_REAL fu,u1min;
	UM_coord qqu;

	cvs.e1 = e1;
	cvs.u2 = u2;
	cvs.evcv1 = evcrv1;
	cvs.key2 = key2;
	cvs.iwf2 = iwf2;
	um_tftotf (tf1,cvs.tf1);

	status = uu_brent (a,u1,b,&u1min,dmin,&fdis,&cvs,eps);
/*
..... get the curve points at the minimal distance
*/
	if (status == UU_SUCCESS)
	{
		status = ncl_isect_evcv (u1min,e1,tf1,evcrv1,u2,key2,iwf2,qqu,&fu);
		if (status == UU_SUCCESS)
		{
			um_vctovc (evcrv1->cp,pt1);
			um_vctovc (qqu,pt2);
		}
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION  : ncl_isect_refine1(e1,tf1,a1,b1,u1,evcrv1,pt1,key2,
**                                                  iwf2,u2,pt2,d,EPS)  
**       Refine the intersection point of two curves by projecting
**       points of the first curve onto the second.
**    PARAMETERS   
**       INPUT  : 
**          e1            First curve struct pointer
**          tf1           First curve transformation
**          u1            Starting parameter on first curve
**          (a1,b1)       Parameter range for first curve
**          evcrv1        Evaluator struct to reuse
**          key2          Key of second curve.
**          iwf2          Wireframe flag of second curve.
**          u2            Starting parameter for second curve
**          EPS           Epsilon value for success
**       OUTPUT : 
**          pt1           Point on second curve.
**          pt2           Point on second curve.
**          d            Squared distance between two curve points
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_isect_refine1 (e1,tf1,a1,b1,u1,evcrv1,pt1,key2,iwf2,u2,pt2,d,EPS)
struct NCL_fixed_databag *e1;
UM_transf tf1;
UU_REAL a1,b1,u1,u2,*d,EPS;
struct UM_evcrvout *evcrv1;
UU_KEY_ID key2;
UM_int2 iwf2;
UM_coord pt1,pt2;
{
	int status, iflg = 0, unflg = 1, ier;
	UM_vector vc;
	UM_coord ppi,ppa,ppb,qqi,qqa,qqb;
	UU_REAL da,db,dab,di;


	um_vctovc (evcrv1->cp,ppi);
	cvpv2(&key2,&iwf2,ppi,&iflg,&u2,&unflg,qqi,vc,&ier);
	if (ier != 0) return;
	di = um_sqdis (ppi,qqi);
	if (di < *d)
	{
		*d = di;
		um_vctovc (ppi,pt1);
		um_vctovc (qqi,pt2);
		if (di < EPS) return;
	}
/*
..... project the a,b endpoints to set up the brent minimum search
*/
	status = ncl_isect_evcv (a1,e1,tf1,evcrv1,u2,key2,iwf2,qqa,&da);
	if (status != UU_SUCCESS) return;
	um_vctovc (evcrv1->cp,ppa);
	status = ncl_isect_evcv (b1,e1,tf1,evcrv1,u2,key2,iwf2,qqb,&db);
	if (status != UU_SUCCESS) return;
	um_vctovc (evcrv1->cp,ppb);
/*
..... if the endpoints are farther from cv2 than the initial parameter u2 - 
..... use the brent method to find the point ov cv1 in (a1,b1), which is
..... closest to cv2.
*/
	dab = MIN2(da,db);
	if (dab < di)
	{
		if (dab < *d)
		{
			*d = dab;
			if (da <= db)
			{
				um_vctovc (ppa,pt1);
				um_vctovc (qqa,pt2);
			}
			else
			{
				um_vctovc (ppb,ppi);
				um_vctovc (qqb,pt2);
			}
		}
		return;		
	}

	status = 
		ncl_isect_refine2 (e1,tf1,a1,u1,b1,evcrv1,pt1,key2,iwf2,u2,pt2,&di,EPS);
	if (status == UU_SUCCESS && di < *d) *d = di;

	return;
}

/*********************************************************************
**    E_FUNCTION  : ncl_isect_refine(e1,tf1,iwf1,e2,tf2,iwf2,a1,b1,u1,
**                                              a2,b2,u2,tol,pint)
**       Refine the intersection point of two curves.
**    PARAMETERS   
**       INPUT  : 
**          e1            First curve struct pointer
**          tf1           First curve transformation
**          u1            Starting parameter on first curve
**          (a1,b1)       Parameter range for first curve
**          iwf1          Wireframe flag of second curve.
**          e2            Second curve struct pointer
**          tf2           Second curve transformation
**          u2            Starting parameter for second curve
**          (a2,b2)       Parameter range for second curve
**          iwf2          Wireframe flag of second curve.
**          pint          Intersection point to refine.
**          tol           Tolerance
**       OUTPUT : 
**          pint          Intersection point.
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_isect_refine(e1,tf1,iwf1,e2,tf2,iwf2,a1,b1,u1,a2,b2,u2,tol,pint)
struct NCL_fixed_databag *e1,*e2;
UM_transf tf1,tf2;
UM_int2 iwf1,iwf2;
UM_coord pint;
UU_REAL tol,a1,b1,u1,a2,b2,u2;
{
	UU_REAL EPS = 0.25*tol*tol;
	UU_REAL d,d0;
	int status;
	UM_coord pt1,pt2;
	struct UM_evcrvout evcrv1,evcrv2;


	status = uc_init_evcrvout (e1,&evcrv1);
	if (status == UU_SUCCESS)
		status = uc_evcrv(UM_POINT,u1,e1,tf1,&evcrv1);
	if (status != UU_SUCCESS) return;
	status = uc_init_evcrvout (e2,&evcrv2);
	if (status == UU_SUCCESS)
		status = uc_evcrv(UM_POINT,u2,e2,tf2,&evcrv2);
	if (status != UU_SUCCESS) return;
/*
..... evaluate two curves - take the middle point as the initial approximation
*/
	d = um_sqdis(evcrv1.cp,evcrv2.cp);
	d0 = d;
	if (d < 4*tol*tol)
	{
		um_middlept(evcrv1.cp,evcrv2.cp,pint);
		if (d < EPS) return;
	}
/*
..... refine by projecting points of the first curve onto the second
*/
	ncl_isect_refine1(e1,tf1,a1,b1,u1,&evcrv1,pt1,e2->key,iwf2,u2,pt2,&d,EPS);
	if (d < d0)
	{
		d0 = d;
		if (d < 4*tol*tol)	
			um_middlept(pt1,pt2,pint);
		if (d0 < EPS) return;
	}
/*
..... refine by projecting points of the second curve onto the first
*/
	d = d0;
	ncl_isect_refine1(e2,tf2,a2,b2,u2,&evcrv2,pt2,e1->key,iwf1,u1,pt1,&d,EPS);
	if (d < d0 && d < 4*tol*tol) um_middlept(pt1,pt2,pint);

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_isect_3dc (e1,rn1,tf1,e2,rn2,tf2,lnrpt,nearpt,
**                                                          pint,tol,dis)
**       Calculate the intersection point of two curves.
**    PARAMETERS   
**       INPUT  : 
**          e1            First curve struct pointer
**          tf1           First curve transformation
**          rn1           Curve type of first curve
**          e2            Second curve struct pointer
**          tf2           Second curve transformation
**          rn2           Curve type of second curve
**          tol           Tolerance
**          lnrpt         Near point flag
**          nearpt        Near point
**       OUTPUT : 
**          pint          Intersection point.
**          dis           Distance (squared) to near point
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_isect_3dc (e1,rn1,tf1,e2,rn2,tf2,lnrpt,nearpt,pint,tol,dis,nintp,ibuf)
struct NCL_fixed_databag *e1,*e2;
UM_int2 rn1,rn2;
UM_transf tf1,tf2;
UU_LOGICAL lnrpt;
UM_coord nearpt,pint;
UU_REAL tol,*dis;
int	*nintp;
UM_isect ibuf[];
{
	int i,j,n1,n2,i0,j0;
	UU_REAL tolsq,d,d0,a1,b1,a2,b2,u1,u2;
	UU_REAL ncl_lnlndis();
	UU_LIST pptr1,pptr2,uptr1,uptr2;
	UU_LIST i0lst,j0lst,pp1lst,pp2lst;
	UM_coord *pts1,*pts2,npt1,npt2,pp1,pp2;
	int ni0,*i0idx,*j0idx,ni1;
	UM_coord *pp1ts,*pp2ts;
	int status = UU_FAILURE;
	struct UM_rbsplcrv_rec *rcrv1,*rcrv2;
	struct NCL_fixed_databag *e11,*e21;
	int size1 = 0, size2 = 0;
	UM_int2 iwf1 = 0, iwf2 = 0;
	ni0 = 1;
	ni1 = 0;

	tolsq = 4*tol*tol;
/*
..... evolve two curves, find intersecting segments
*/
	uu_list_init(&pptr1,sizeof(UM_coord),100,100);
	uu_list_init(&uptr1,sizeof(UM_coord),100,100);
	uu_list_init(&pptr2,sizeof(UM_coord),100,100);
	uu_list_init(&uptr2,sizeof(UM_coord),100,100);

	n1 = ncl_evolve_curve(e1,tf1,tol,&pptr1,UU_NULL,&uptr1,0);
	if (n1 < 2) goto Done;
	n2 = ncl_evolve_curve(e2,tf2,tol,&pptr2,UU_NULL,&uptr2,0);
	if (n2 < 2) goto Done;

	pts1 = (UM_coord *)UU_LIST_ARRAY(&pptr1);
	pts2 = (UM_coord *)UU_LIST_ARRAY(&pptr2);
	d0 = 1.e10;
/*
.....Initilize all posssible intersection points
*/
	if(!lnrpt)
	{
		uu_list_init(&i0lst,sizeof(int),10,10);
		uu_list_init(&j0lst,sizeof(int),10,10);
		uu_list_init(&pp1lst,sizeof(UM_coord),10,10);
		uu_list_init(&pp2lst,sizeof(UM_coord),10,10);
	}

	for (i = 0; i < n1-1; i++)
	{
		for (j = 0; j < n2-1; j++)
		{
			d = ncl_lnlndis(pts1[i],pts1[i+1],pts2[j],pts2[j+1],npt1,npt2);
			if (d < tolsq)
			{
				status = UU_SUCCESS;
				if (!lnrpt)
				{
					i0 = i; j0 = j;
					um_vctovc (npt1,pp1); um_vctovc (npt2,pp2);

					uu_list_push(&i0lst,&i0);
					uu_list_push(&j0lst,&j0);
					uu_list_push(&pp1lst,&pp1);
					uu_list_push(&pp2lst,&pp2);
					j++;
					continue;
				}
/*
..... if there is near point, get the closest intersection
*/
				d = um_sqdis(npt1,nearpt) + um_sqdis(npt2,nearpt);
				if (d < d0)
				{
					d0 = d;
					i0 = i; j0 = j;
					um_vctovc (npt1,pp1); um_vctovc (npt2,pp2);
					if (d < tolsq)
						goto Refine;
				}
			}
		}
	}
	if (status != UU_SUCCESS) goto Done;

Refine:
	
	if (!lnrpt)
	{
		ni0 = UU_LIST_LENGTH(&i0lst);
		i0idx = (int *)UU_LIST_ARRAY(&i0lst);
		j0idx = (int *)UU_LIST_ARRAY(&j0lst);
		pp1ts = (UM_coord *)UU_LIST_ARRAY(&pp1lst);
		pp2ts = (UM_coord *)UU_LIST_ARRAY(&pp2lst);
		*nintp = ni0;

		if (ni0 > UM_MAXISECT)
		{
			status = UU_FAILURE;
			goto Done;
		}
	}

	for (i = 0 ; i < ni0; i++)
	{
		if (!lnrpt)
		{
			i0 = i0idx[i];
			j0 = j0idx[i];
			um_vctovc (pp1ts[i],pp1);
			um_vctovc (pp2ts[i],pp2);
		}
/*
..... get curve parameters for the segment intersections
*/
		d0 = UM_DCCCC(pts1[i0],pts1[i0+1]);
		d = UM_DCCCC(pts1[i0],pp1);

		pts1 = (UM_coord *)UU_LIST_ARRAY(&uptr1);
		a1 = pts1[i0][0]; b1 = pts1[i0+1][0];
		if ((i0 < n1-2) && (d0 < d + tol))
		{
			u1 = b1;
			b1 = pts1[i0+2][0];
		}
		else
			u1 = (d/d0)*(b1 - a1) + a1;

		d0 = UM_DCCCC(pts2[j0],pts2[j0+1]);
		d = UM_DCCCC(pts2[j0],pp2);

		pts2 = (UM_coord *)UU_LIST_ARRAY(&uptr2);
		a2 = pts2[j0][0]; b2 = pts2[j0+1][0];
		if ((j0 < n2-2) && (d0 < d + tol))
		{
			u2 = b2;
			b2 = pts2[j0+2][0];
		}
		else
			u2 = (d/d0)*(b2 - a2) + a2;

		um_middlept(pp1,pp2,pint);
/*
..... make lines/circles into splines
*/
		if (rn1 != NCLI_CURVE)
		{
			size1 = um_curve_size (e1);
			um_allocate_curve (&rcrv1,size1); 
			if (um_rbcrv_frmnclcrv (e1,rcrv1) != UU_SUCCESS) goto Done;
			e11 = (struct NCL_fixed_databag *) rcrv1;
			iwf1 = 1;
		}
		else
			e11 = e1;
		if (rn2 != NCLI_CURVE)
		{
			size2 = um_curve_size (e2);
			um_allocate_curve (&rcrv2,size2); 
			if (um_rbcrv_frmnclcrv (e2,rcrv2) != UU_SUCCESS) goto Done;
			e21 = (struct NCL_fixed_databag *) rcrv2;
			iwf2 = 1;
		}
		else
			e21 = e2;

		ncl_isect_refine(e11,tf1,iwf1,e21,tf2,iwf2,a1,b1,u1,a2,b2,u2,tol,pint);

		if (!lnrpt)
		{
			if (i == 0 || (i > 0 && UM_DCCCC(pint,ibuf[ni1-1].pt) > 0.5*tol))
			{
				um_vctovc (pint,ibuf[ni1].pt);
				ni1++;
			}
		}
		else	
			um_vctovc (pint,ibuf[i].pt);
	}

	if (lnrpt)
	{
		*dis = um_sqdis(pint,nearpt);
		*nintp = 1;
	}
	else
		*nintp = ni1;

Done:
	uu_list_free(&pptr1);
	uu_list_free(&pptr2);
	uu_list_free(&uptr1);
	uu_list_free(&uptr2);
	if (size1 > 0) uu_toolfree (rcrv1);
	if (size2 > 0) uu_toolfree (rcrv2);
	if (!lnrpt)
	{
		uu_list_free(&i0lst);
	    uu_list_free(&j0lst);
		uu_list_free(&pp1lst);
	    uu_list_free(&pp2lst);
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_isect_3db (e1,tf1,e2,tf2,lnrpt,nearpt,pint,tol,dis)
**       Calculate the intersection point of two non-composite curves in 3D.
**    PARAMETERS   
**       INPUT  : 
**          e1            First curve struct pointer
**          tf1           First curve transformation
**          e2            Second curve struct pointer
**          tf2           Second curve transformation
**          tol           Tolerance
**          lnrpt         Near point flag
**          nearpt        Near point
**       OUTPUT : 
**          pint          Intersection point.
**          dis           Distance (squared) to near point
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_isect_3db (e1,tf1,e2,tf2,lnrpt,nearpt,pint,tol,dis,nintp,ibuf)
struct NCL_fixed_databag *e1,*e2;
UM_transf tf1,tf2;
UU_LOGICAL lnrpt;
UM_coord nearpt,pint;
UU_REAL tol,*dis;
int	*nintp;
UM_isect ibuf[];
{
	UM_int2 rn1,rn2;

	ncl_get_type (e1->rel_num, &rn1);
	ncl_get_type (e2->rel_num, &rn2);
/*
..... two lines are a special case
*/
	if (rn1 == NCLI_LINE && rn2 == NCLI_LINE)
	{
		UM_coord npt1,npt2;
		struct UM_line_rec *l1,*l2;
		UU_REAL dist,ncl_lnlndis();

		l1 = (struct UM_line_rec *) e1;
		l2 = (struct UM_line_rec *) e2;

		dist = ncl_lnlndis(l1->spt,l1->ept,l2->spt,l2->ept,npt1,npt2);
		if (dist < tol*tol)
		{
			*dis = dist;
			um_middlept(npt1,npt2,pint);
			*nintp = 1;
			um_vctovc (pint,ibuf[0].pt);
			return (UU_SUCCESS);
		}
		else
			return (UU_FAILURE);
	}
	else
		return (ncl_isect_3dc (e1,rn1,tf1,e2,rn2,tf2,lnrpt,nearpt,pint,tol,dis,
						nintp,ibuf));
}

/*********************************************************************
**    E_FUNCTION     : ncl_isect_3da (e1,tf1,e2,tf2,lnrpt,nearpt,pint,tol,dis)
**       Calculate the intersection point of two (maybe composite) curves in 3D.
**    PARAMETERS   
**       INPUT  : 
**          e1            First curve struct pointer
**          tf1           First curve transformation
**          e2            Second curve struct pointer
**          tf2           Second curve transformation
**          tol           Tolerance
**          lnrpt         Near point flag
**          nearpt        Near point
**       OUTPUT : 
**          pint          Intersection point.
**          dis           Distance (squared) to near point
**          nintp		  number of intersection points found
**			ibuf		  array of point intersection records
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_isect_3da (e1,tf1,e2,tf2,lnrpt,nearpt,pint,tol,dis,nintp,ibuf)
struct NCL_fixed_databag *e1,*e2;
UM_transf tf1,tf2;
UU_LOGICAL lnrpt;
UM_coord nearpt,pint;
UU_REAL tol,*dis;
int	*nintp;
UM_isect ibuf[];
{
	int i,j,rev;
	int ncv,stat,status,ist;
	struct NCL_fixed_databag crv;
	UU_REAL di;
	UM_coord pti;

	status = UU_FAILURE;
	stat = UU_SUCCESS;

	if (ncl_itsa_compcrv (e1))
	{
		stat = ncl_compcrv_getnents (e1, &ncv);

		for (i=0; i<ncv && stat == UU_SUCCESS; i++)
		{
			stat = ncl_compcrv_getelm (e1, i, &crv, &rev);
			if (stat == UU_SUCCESS)
			{
				di = 1.e10;
				ist = ncl_isect_3da (&crv,tf1,e2,tf2,lnrpt,nearpt,pti,tol,&di,
							nintp,ibuf);
				if (ist == 0)
				{
					status = UU_SUCCESS;
					if (!lnrpt || di < *dis)
					{
						*dis = di;
						for (j = 0; j < 3; j++) pint[j] = pti[j];
						if (!lnrpt || di < tol*tol) return (status);
					}
				}
			}
		}
	}
	else if (ncl_itsa_compcrv (e2))
	{
		stat = ncl_compcrv_getnents (e2, &ncv);

		for (i=0; i<ncv && stat == UU_SUCCESS; i++)
		{
			stat = ncl_compcrv_getelm (e2, i, &crv, &rev);
			if (stat == UU_SUCCESS)
			{
				di = 1.e10;
				ist = ncl_isect_3da (e1,tf1,&crv,tf2,lnrpt,nearpt,pti,tol,&di,
							nintp,ibuf);
				if (ist == 0)
				{
					status = UU_SUCCESS;
					if (!lnrpt || di < *dis)
					{
						*dis = di;
						for (j = 0; j < 3; j++) pint[j] = pti[j];
						if (!lnrpt || di < tol*tol) return (status);
					}
				}
			}
		}
	}
	else
		status = ncl_isect_3db (e1,tf1,e2,tf2,lnrpt,nearpt,pint,tol,dis,
						nintp,ibuf);

	if (stat != UU_SUCCESS || status != UU_SUCCESS)
		return (-1);
	else
		return (0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_isect_3d(e1,tf1,e2,tf2,lnrpt,nearpt,pto,ierr)
**       Calculate the intersection point of two curves in 3D.
**    PARAMETERS   
**       INPUT  : 
**          e1            First curve struct pointer
**          tf1           First curve transformation
**          e2            Second curve struct pointer
**          tf2           Second curve transformation
**          lnrpt         Near point flag
**          nearpt        Near point
**       OUTPUT : 
**          nintp		  number of intersection points found
**			ibuf		  array of point intersection records
**          ierr          0 if OK, -1 else
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_isect_3d(e1,tf1,e2,tf2,lnrpt,nearpt,nintp,ibuf,ierr)
struct NCL_fixed_databag *e1,*e2;
UM_transf tf1,tf2;
UM_int2 *ierr;
UU_LOGICAL lnrpt;
UM_coord nearpt;
int *nintp;
UM_isect ibuf[];
{
	int j,status,nint;
	UM_real8 tol8;
	UU_REAL tol,dis;
	UM_coord pint;

	gettol (&tol8);
	tol = tol8;

	dis = 1.e10;
	status = ncl_isect_3da (e1,tf1,e2,tf2,lnrpt,nearpt,pint,tol,&dis,
				&nint, ibuf);

	if (status == UU_SUCCESS)
	{
		*ierr = 0;
		*nintp = nint;
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_isect_2d(e1,tf1,e2,tf2,lnrpt,nrpt,pto,ierr)
**       Calculate the intersection point of two curves in 2D.
**    PARAMETERS   
**       INPUT  : 
**          nkey1         Key of first curve.
**          nkey2         Key of second curve.
**          lnrpt         Near point flag
**          npt           Near point.
**       OUTPUT : 
**          nintp		  number of intersection points found
**			ibuf		  array of point intersection records
**          ierr          Non-zero if there is an error.
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_isect_2d(e1,tfmat1,e2,tfmat2,lnrpt,nrpt,nintp,ibuf,ierr)
struct NCL_fixed_databag *e1,*e2;
UM_transf tfmat1,tfmat2;
UU_LOGICAL lnrpt;
UM_coord nrpt;
int *nintp;
UM_isect ibuf[];
UM_int2 *ierr;
{
	int i,j,status,nint;
	UU_REAL dist,sdist;
	int ipt,istat;	

/*
.....Intersect the curves
*/
	ipt = 2;
	status = um_isect_curves(e1, tfmat1, e2, tfmat2, &ipt, nrpt, &nint,
		UM_MAXISECT, ibuf);
/*
........Reverse curve order if not successful
*/
	if ((status != UU_SUCCESS) || (nint == 0))
		status = um_isect_curves(e2, tfmat2, e1, tfmat1, &ipt, nrpt, &nint,
			UM_MAXISECT, ibuf);
	if ((status != UU_SUCCESS) || (nint == 0)) goto failed;

	*ierr = UU_SUCCESS;
	*nintp = nint;
	goto done;
/*
.....Return failure
*/
failed:;
	*ierr = -1;
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_pt_intof_cv (nkey1,nkey2,ifl,npt,ierr)
**       Fortran callable routine to calculate the intersection point
**			of two curves.
**    PARAMETERS   
**       INPUT  : 
**          nkey1         Key of first curve.
**          nkey2         Key of second curve.
**			ifl			  Near point flag
**			ifl3d		  3D point flag
**          npt           Near point.
**       OUTPUT : 
**          ierr          Non-zero if there is an error.
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pt_intof_cv (nkey1,nkey2,ifl,ifl3d,npt,fillnm,subsc,ierr)
UM_int4 *nkey1,*nkey2;
UM_int2 *ifl,*ifl3d,*ierr;
UM_real8 npt[];
UM_f77_str_ptr fillnm;
UM_int4 *subsc;
{
	int i,j,istat,status,nclkey,nclrel,nint,subs,nc; 
	UU_LOGICAL l3d,lnrpt;
	UM_coord nrpt;
	struct NCL_fixed_databag e1,e2;
	UM_transf tfmat1,tfmat2;

	UM_isect ibuff[UM_MAXISECT];
	UU_REAL dist,sdist;
	struct UM_point_rec p;

	UM_int2 cnon, ifnd, ier, idx;
	UU_KEY_ID delkey;
	char *ptid, label[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
    UM_int4 isub2;
	UM_int2 j2,ilab;
	char *lb0,buf[80];
	UM_f77_str_ptr str77;

/*
...set label to use supplied name for intof
*/
	cnon = 0;
	delkey = NULLKEY;
	subs  = isub2 = *subsc;
	if (*subsc > 0) 
	{
		ptid = UM_cstr_of_f77_str(fillnm);
		strcpy (label,ptid);
		for (i=strlen(label);i<NCL_MAX_LABEL-1;i++) label[i] = ' ';
		label[NCL_MAX_LABEL-1] = '\0';
	}

	UM_init_f77_str (str77, buf, 80);
	j2 = 296; getifl(&j2,&ilab);
	if (ilab > 0)
	{
		lb0 = UM_cstr_of_f77_str(fillnm);
		strncpy(label,lb0,NCL_MAX_LABEL);
		nc = ul_cut_string(label,NCL_MAX_LABEL);
		label[nc] = '\0';
	}

	idx = 41;
	getifl (&idx,&cnon);

/*
.....Get the curve data
*/
	*ierr = -1;
	e1.key = *nkey1;
	e2.key = *nkey2;
	status = ncl_retrieve_data_fixed(&e1);
	if (status != UU_SUCCESS) return;;
	status = uc_retrieve_transf(e1.key, tfmat1);
	if (status != UU_SUCCESS) return;;
	status = ncl_retrieve_data_fixed(&e2);
	if (status != UU_SUCCESS) return;;
	status = uc_retrieve_transf(e2.key, tfmat2);
	if (status != UU_SUCCESS) return;;

	l3d = (*ifl3d > 0);
	lnrpt = (*ifl == 0);
	if (!l3d || lnrpt)
	{
		nrpt[0] = npt[0]; nrpt[1] = npt[1]; nrpt[2] = npt[2];
	}
/*
.....if one of the entites is a point-vector, convert it to 
.....a line.  JLS 6/30/99
*/
	if (e1.rel_num == NCL_POINTVEC_REL) um_pv_to_line(&e1,&e1,UU_TRUE);
	if (e2.rel_num == NCL_POINTVEC_REL) um_pv_to_line(&e2,&e2,UU_TRUE);
/*
.....Convert plane to infinite line
*/
	if (!l3d && e2.rel_num == NCL_PLN_REL) 
	{
		UU_REAL buff[6];		
		int plnr,ier;
		int cvflg = 1;
		plcvsf (&e1.key,&cvflg,buff,&plnr,&ier);
		if (ier != 1)
			um_pl_to_line(&e2,buff,&e2);
	}

/*
...Check if label exists in unibase and canon is ON
*/
	if (isub2 > 0)
	{
		chklab (fillnm, &nclkey, &isub2, &ifnd, &cnon);
		if (ifnd == 1)
			if (cnon == 0)
			{
				ier = 384;
				return;
			}
			else if (cnon == 1)
			{
				ur_retrieve_data_relnum(nclkey,&nclrel);
				if (nclrel != UM_POINT_REL)
				{
					ier  = 89;
					return;
				}
				delkey = nclkey;
			}
			subs = isub2;
			isub2++;
	}
	else if (ilab > 0 && cnon == 0)
	{
		if (*ifl == 2)
			sprintf(buf,"%s%d",label,ilab);
		else
			strcpy(buf,label);
		chklab (str77, &nclkey, &isub2, &ifnd, &cnon);
		if (ifnd == 1)
		{
			if (cnon == 0)
			{
				ier = 384;
				return;
			}				
			else if (cnon == 1)
			{
				ur_retrieve_data_relnum(nclkey,&nclrel);
				if (nclrel != UM_POINT_REL)
				{
					ier  = 89;
					return;
				}
				delkey = nclkey;
			}
		}
	}		
	if (delkey != NULLKEY) uc_delete (delkey);

/*
.....Intersect the curves
*/
	if (l3d)
		ncl_isect_3d(&e1,tfmat1,&e2,tfmat2,lnrpt,nrpt,&nint,ibuff,ierr);
	else
		ncl_isect_2d(&e1,tfmat1,&e2,tfmat2,lnrpt,nrpt,&nint,ibuff,ierr);

	if (*ierr != UU_SUCCESS)
		return;
/*
.....Find the near intersection point if lnrpt
*/
	j = 0;
	if (nint > 1 && lnrpt)
	{
		sdist = 1.e10;
		for (i = 0; i < nint; i++)
		{	
			dist = um_sqdis (nrpt,ibuff[i].pt);
			if (dist < sdist)
			{
				j = i;
				sdist = dist;
			}
		}
	}
	*ierr = 0;

	ur_setup_data(UM_POINT_REL, &p, sizeof(struct UM_point_rec));
/*
.....Initialize the LABEL and SUBSCRIPT fields
*/
	strcpy (p.label, "");
	p.subscr = 0;

	for (i = 0; i < nint; i++)
	{
/*
.....Check if the point is on curves or not
*/
		if  (!lnrpt)
		{
			status = ncl_is_pt_on_curve(ibuff[i].pt, &e1, tfmat1);
			if (status != UU_SUCCESS)
				continue;
			status = ncl_is_pt_on_curve(ibuff[i].pt, &e2, tfmat2);
			if (status != UU_SUCCESS)
				continue;
		}

		if (lnrpt)
			i = j;
		else if (*ifl != 2)
			i = nint - 1;

		um_vctovc(ibuff[i].pt, p.pt);
		p.markertype = UM_ptattr.markertype;
		p.snap_node = UM_ptattr.snap_node;

		if (subs > 0) 
		{
			strncpy (p.label,label, NCL_MAX_LABEL);
			p.subscr = subs;		
			NCL_ubcopy = 1;
			subs++;
		}
		else if (ilab > 0)
		{
			if (*ifl == 2)
			{
				sprintf(p.label,"%s%d",label,ilab);					
				ilab++;
			}
			else
				strcpy(p.label,label);
			NCL_ubcopy = 1;
		}
		else
		{
			istat = 0;
			ncl_label_wf(UM_POINT_REL,p.label,&p.subscr,p.key,&istat);
			NCL_ubcopy = 1;
		}
		
		if (um_create_geom(&p, UM_DEFAULT_TF, UM_CURRENT_ATTR) != UU_SUCCESS)			
			return;

		NCL_ubcopy = 0;
		ncl_def_color(p.key);

		uc_display(&p);

		if (lnrpt || *ifl != 2)
			break;
	}
	
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_percnt_on_cv (uper,e1,tol)
**       Calculate a point along a curve at U=u: compute the parameter
**       from a percentage of length.
**    PARAMETERS   
**       INPUT  : 
**          e1            curve struct.
**          uper          position on curve, given as percentage.
**          tol           tolerance
**       OUTPUT : 
**          uper          U-parameter of curve
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_percnt_on_cv (uper,e1,tol)
struct NCL_fixed_databag *e1;
UU_REAL *uper,tol;
{
	int status,i;
	UU_REAL tol1,un,u0,u1,du,dtot,up,d0,d1,del,dn,DP;
	UU_REAL um_getarclen();

	up = (*uper)/100;
	if (ncl_itsa_line (e1) || ncl_itsa_circle (e1) || up < 0.0001 || up > 0.9999)
		goto done;

	tol1 = tol/10;

	dtot = um_getarclen (e1, UM_idmat);
	DP = up*dtot;

	d0 = 0;
	d1 = dtot;
	u0 = 0; u1 = 1;

	while (UU_TRUE)
	{
		del = d1 - d0;
		du = u1 - u0;
		un = (u0 + u1)/2;

		if (del < tol || du < 0.0001)
		{
			if (up < u0 || up > u1)
			{
				if (DP - d0 < tol1)
					up = u0;
				else if (d1 - DP < tol1)
					up = u1;
				else
					up = un;
			}
			goto done;
		}

		du = du/4;

		for (i = 0; i < 3; i++)
		{
			un = u0 + du;
			status = um_getarclen1 (u0, un, e1, UM_idmat, &del);
			if (status != UU_SUCCESS) return (status);
			dn = d0 + del;
			if (dn >= DP)
			{
				u1 = un;
				d1 = dn;
				break;
			}
			else
			{
				u0 = un;
				d0 = dn;
			}
		}
	}

done:
	*uper = up;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_pt_on_cv (nkey,u,pto,ierr)
**       Fortran callable routine to calculate a point or a point-vector
**       along a curve at U=u.
**    PARAMETERS   
**       INPUT  : 
**          itsk          point if 0, point-vector if 1
**          nkey          Key of curve.
**          u             U-parameter of curve.
**          lpercnt       flag to interpret u as percentage
**       OUTPUT : 
**          pto           Calculated point (point-vector).
**          ierr          Non-zero if there is an error.
**    RETURNS      : 
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pt_on_cv(itsk,nkey,lpercnt,u,pto,ierr)
UM_int2 *itsk,*lpercnt,*ierr;
UM_int4 *nkey;
UM_real8 *u,pto[];
{
	int status,evflg,i;
	struct NCL_fixed_databag e1;
	struct UM_evcrvout evcrv;
	UU_REAL up,tol;
	UM_real8 tol8;

	*ierr = -1;
/*
.....Get the curve data
*/
	e1.key = *nkey;
	status = ncl_retrieve_data_fixed(&e1);
	if (status != UU_SUCCESS) return;
/*
..... set the parameter
*/
	up = *u;
	if (*lpercnt == 1)
	{
		gettol (&tol8);
		tol = tol8;
		status = ncl_percnt_on_cv (&up,&e1,tol);
		if (status != UU_SUCCESS) return;
	}
/*
.....Evaluate the curve
*/
	status = uc_init_evcrvout (&e1,&evcrv);
	if (*itsk == 1)
		evflg = UM_FRSTDERIV;
	else
		evflg = UM_POINT;

	if (status == UU_SUCCESS)
		status = uc_evcrv(evflg, up, &e1, UM_idmat, &evcrv);

	
	if (status != UU_SUCCESS) return;

	*ierr = 0;
	for (i = 0; i < 3; i++)
	{
		pto[i] = evcrv.cp[i];
	}
	if (evflg == UM_FRSTDERIV)
	{
		um_unitvc (evcrv.dcdu,evcrv.dcdu);
		for (i = 0; i < 3; i++)
		{
			pto[i+3] = evcrv.dcdu[i];
		}
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_is_pt_on_curve(pt, eptr,tol)
**       Determine if the given point (PT in MCS) is on the specified
**      curve within tolerance
**    PARAMETERS   
**       INPUT  : 
**          pt			point coordinates (MCS)
**			eptr		curve entity
**			tfmat		transformation matrix for curve
**       OUTPUT :  
**          none
**    RETURNS      : 
**      0 iff point is on curve; -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_is_pt_on_curve(pt, eptr, tfmat)
UM_coord pt;
struct UM_crvdatabag *eptr;
UM_transf tfmat;
{
	int i,iret,npts,status;
	UU_LIST pptr,uptr;
	UM_coord *pts,pti;
	UM_vector vc;
	UU_REAL d,di,dist,dmin,tol;
	UM_real8 tol8;
	UU_REAL um_dcccc();

	gettol (&tol8);
	tol = tol8;
	status = UU_SUCCESS;

/*
..... evolve the curve
*/
	uu_list_init(&pptr,sizeof(UM_coord),100,100);
	uu_list_init(&uptr,sizeof(UM_coord),100,100);
	npts = ncl_evolve_curve(eptr,tfmat,tol,&pptr,UU_NULL,&uptr,0);

	pts = (UM_coord *)UU_LIST_ARRAY(&pptr);

	dmin = 1.e10;
	for (i = 0; i < npts -1 ; i++)
	{
		um_vcmnvc(pts[i+1],pts[i],vc);
		d = UM_MAG(vc);
		if (d < tol) continue;
		vc[0] /= d; vc[1] /= d; vc[2] = vc[2] /= d; 
		iret = um_nptsg1 (pt,pts[i],vc,d,pti,&di);

		dist = 	um_dcccc(pt, pti);
		if (dist < dmin)
			dmin = dist;
	}

	if (dmin > 10.0 * tol)
		status = UU_FAILURE;

	uu_list_free(&pptr);
	uu_list_free(&uptr);

	return status;
}
