
/*********************************************************************
**    NAME         :  m3eplin.c
**       CONTAINS: polyline routines
**			int um_drw42_polyline(eptr, tfmat, attrptr)
**			int um_p42_polyline(ptr)
**			int um_cp42_copypolyline(e1, e2, bagsize)
**			int um_tr42_tranpolyline(eptr,offset)
**			int um_tf42_tranfpolyline(eptr, tfmat, store)
**			int um_ev42_linepoly(evflag,u,eptr,tfmat,evout)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3eplin.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:54
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdebug.h"
#include "mdeval.h"
#include "class.h"


/*********************************************************************
**    E_FUNCTION :  int um_drw42_polyline(eptr, tfmat, attrptr)
**			Draw a polyline entity.
**    PARAMETERS   
**       INPUT  : 
**				eptr    			pointer to polyline record
**				tfmat				transformation matrix
**				attrptr			pointer to attribute record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_drw42_polyline(eptr,tfmat,attrptr)
	struct  UM_polyline_rec  *eptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;
	{
	int i,j;
	Gwpoint3 *gpt;
	int nvx;
	UM_coord	tmpt;

	uu_denter(UU_MTRC,(us,"um_drw42_polyline(key=%x,tfmat:%x,attrptr:%x)",
					eptr->key,tfmat,attrptr));

	um_set_disp_attr(attrptr);

	nvx = eptr->no_pt;
	gpt = (Gwpoint3 *) uu_malloc( (nvx + 1) * sizeof(Gwpoint3));

	for (i=0, j=0; i<nvx; i++, j=j+3 )
		{
		um_cctmtf(&eptr->pt[j], tfmat, tmpt);
		gpt[i].x = tmpt[0];
		gpt[i].y = tmpt[1];
		gpt[i].z = tmpt[2];
		}
	um_gpolyline3(attrptr, nvx, gpt);

	uu_free(gpt);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_p42_polyline(ptr)
**       Print the data defining a polyline.
**    PARAMETERS   
**       INPUT  : 
**				ptr					pointer to polyline
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_p42_polyline(ptr)
	struct  UM_polyline_rec  *ptr;

	{
	int		nvx;

	uu_denter( UU_MTRC,(us,"um_p42_polyline(key=%x)",ptr->key));

	nvx = ptr->no_pt;
	sprintf(UM_sbuf, "POLYLINE %d", ptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "label %7.7s", ptr->label);
	um_pscroll(UM_sbuf);

	um_p_ary(UM_PINT, "number of vertices", 1, &nvx);
	um_p_ary(UM_PFLOAT, "vertices", 3*nvx, ptr->pt);

	umi_print_transf(ptr->key);

	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION: int um_cp42_copypolyline(e1, e2, bagsize)
**      Copy a polyline.
**    PARAMETERS   
**       INPUT  : 
**				e1			pointer to entity to be copied
**				bagsize	size of storage for entity.
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cp42_copypolyline(e1, e2, bagsize)
	struct UM_polyline_rec *e1;        /* The entity to translate */
	struct UM_polyline_rec *e2;        /* New entity */
	int bagsize;

	{
	struct UM_attrdata_rec attrbag;
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"um_cp42_copypolyline(key=%x,bagsize=%d)",
		e1->key, bagsize));
                                            
	ur_setup_app_data(e1->rel_num, e2, bagsize);

	key = e1->key;
	um_get_disp_attr(e1->key, &attrbag);
	um_create_geom(e1, UM_DEFAULT_TF, &attrbag);
	e2->key = e1->key;
	e1->key = key;
	um_get_all_geom(e2, bagsize);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_tr42_tranpolyline(eptr,offset)
**      Translate the specified polyline by offset
**    PARAMETERS   
**       INPUT  : 
**				eptr		pointer to the  entity to be translated
**			   offset	vector by which to translate
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tr42_tranpolyline(eptr,offset)
	struct UM_polyline_rec *eptr;
	UM_vector   offset;

	{
	int	nvx;
	int	i,j;

	uu_denter( UU_MTRC,(us,"um_tr42_tranpolyline(?,?)"));
    
	nvx = eptr->no_pt;
	for (i=0, j=0; i<nvx; i++, j=j+3)
		{
		um_vcplvc(&eptr->pt[j], offset, &eptr->pt[j]);
		}

	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_tf42_tranfpolyline(eptr, tfmat, store)
**      Transform the specified polyline by the given 4x3 matrix, 
**			conjugated by translation to basepoint.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be scaled
**          tfmat	      the 4x3 scaling matrix
**				store			if true, update_geom()
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tf42_tranfpolyline(eptr, tfmat, store)
	struct UM_polyline_rec *eptr;
   UM_transf    tfmat;
	UU_LOGICAL	store;

	{                                     
	int	nvx;
	int	i,j;

	uu_denter(UU_MTRC,(us,"um_tf42_tranfpolyline(?,?,?)"));

	nvx = eptr->no_pt;
	for (i=0, j=0; i<nvx; i++, j=j+3)
		{
		um_cctmtf(&eptr->pt[j], tfmat, &eptr->pt[j]);
		}

	if (store)
		um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}

/********************************************************************* 
**  E_FUNCTION: int um_ev42_linepoly(evflag,u,eptr,tfmat,evout)
**			evaluate a polyline at a parameter
**  	PARAMETERS:   
**  		INPUT: 
**				evflag        UM_POINT= calculate point on polyline only;
**         	              UM_FRSTDERIV= calculate point and 1st 
**                        			derivative;
**                        UM_SECDERIV= calculate point, 1st and 
**                        			2nd derivative;
**                        UM_CURVATURE= calc point, 1st, 2nd deriv, 
**                                 and curvature;
**				u					the parameter value
**				eptr				pointer to the entity data
**				tfmat				transformation matrix.
**			OUTPUT:
**				evout				pointer to a curve evaluator
**                         record containing both the requested
**                         information, and (ultimately) a 
**                         status value indicating the validity
**                         of the requested information.
**  RETURNS : UU_SUCCESS/UU_FAILURE
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int
um_ev42_polyline(evflag,u,eptr,tfmat, evout)
   int evflag;
   UM_param u;
   struct UM_polyline_rec *eptr;
	UM_transf tfmat;
	struct UM_evcrvout *evout;

	{
   int i,j;
	UU_REAL magvec, lvec, ulen, partlen, len;
	UM_vector vec;

	uu_denter(UU_MTRC,(us,"um_ev42_linepoly(evflag:%d,u:%g,key:%d,tfmat:%x,evout:%x)",
							evflag, u, eptr->key, tfmat, evout));

	/* quick exit for start and end point evaluation */
	if (evflag == UM_POINT)
		{
		if (u < UM_FUZZ)
			{
			um_vctovc(&eptr->pt[0], evout->cp);
			goto done;
			}
		else if (fabs(1.0 - u) < UM_FUZZ)
			{
			j = (eptr->no_pt - 1) * 3;
			um_vctovc(&eptr->pt[j], evout->cp);
			goto done;
			}
		}
/*
..... calculate total length of polyline 
.....vp 12/9/96 fix index in for loop 
	for (i=0, j=0; i<eptr->no_pt; i++, j+=3)
*/
	len = 0.0;
	for (i=1, j=0; i<eptr->no_pt; i++, j+=3)
		{
		um_vcmnvc(&eptr->pt[j+3], &eptr->pt[j], vec);
		magvec = um_mag(vec);
		len += magvec;
		}
	um_p_ary(UM_PFLOAT,"len=",1, &len);

/* 
.....second derivatives and curvature not handled yet 
*/
	um_nullvc (evout->dcdu); um_nullvc (evout->d2cdu2);
	evout->curv = 0.0;

	/* find segment that parameter value lies in */
	ulen = u * len;
	partlen = 0.0;
	for (i=0, j=0; i<eptr->no_pt; i++, j+=3)
		{
		um_vcmnvc(&eptr->pt[j+3], &eptr->pt[j], vec);
		magvec = um_mag(vec);
		partlen += magvec;
		if (partlen >= ulen && magvec > 1.e-6)
			{
			lvec = 1.0 - (partlen - ulen)/magvec;
/*
.....vp 12/9/96 add 1-st derivative to output
*/
			um_vctovc(vec, evout->dcdu);
			um_vctmsc(vec, lvec, vec);
			um_vcplvc(&eptr->pt[j], vec, evout->cp);
			um_p_ary(UM_PFLOAT,"lvec=", 1, &lvec);
			um_p_ary(UM_PFLOAT,"cp=", 3, evout->cp);
			um_p_ary(UM_PFLOAT,"dcdu=", 3, evout->dcdu);
			goto done;
			}
		}
	return (UU_FAILURE);

done:
	/* position results in evaluator record according to the transform, tfmat */
	um_transform_evcrvout(evflag, eptr, tfmat, evout);
	uu_dexit;
	return (UU_SUCCESS);
	}

/********************************************************************* 
**  E_FUNCTION: int um_c42_genpoly(pts,no_pts,attr,key)
**			Create a polyline with the given pts and attribute
**  	PARAMETERS:   
**  		INPUT: 
**				pts            List of points for polyline.
**				no_pts         Number of points in list.
**				attr           Attribute record to assign to polyline.
**			OUTPUT:
**				key            Key of polyline,
**  RETURNS : UU_SUCCESS/UU_FAILURE
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int um_c42_genpoly(pts,no_pts,attr)
UM_coord *pts;
int no_pts;
struct UC_attributedatabag *attr;
{
	int status;
	struct UM_polyline_rec poly;

	ur_setup_app_data(UM_POLYLINE_REL,&poly,sizeof(poly));
	ur_update_app_data_varlist(&poly,1,pts,1,no_pts);
	status = um_create_geom(&poly,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	if (status == UU_SUCCESS)
	{
		attr->key = poly.key;
		ur_update_attr(attr);
		uc_display(&poly);
	}
	return(status);
}
