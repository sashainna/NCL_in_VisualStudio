/*********************************************************************
**    NAME         :  negfauto.c
**       CONTAINS:  C Routines to support Auto Gofwd logic.
**
**           ncl_reproj_on_lnci
**           ncl_reproj_on_lnci8
**           ncl_reproj_on_line
**           ncl_reproj_on_circle
**           ncl_2cv_tanto
**           ncl_get_cv_endpts_all
**           gfadbglbl
**           gfadbgdata
**           gfadbgline
**
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       negfauto.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:35
*********************************************************************/

#include "nccs.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mcrv.h"
#include "modef.h"
#include "mdcpln.h"
#include "zsysdep.h"
#include "uminmax.h"
#include "mfort.h"
#include "mgeom.h"
#include "nclfc.h"
#include "ycom.h"
#include "ncl.h"
#include "nclpsmult.h"
#include "misect.h"

extern int NCLX_internal_geom;
/*********************************************************************
**    FUNCTION     : int ncl_reproj_on_lnci (pext,key)
**
**      decides whether a point projects on or outof line segment
**    PARAMETERS
**       INPUT  :
**              pext - external point in question
**              lkey - key of the line segment
**       OUTPUT :
**    RETURNS      :
**               1 - if proj_pt is on the segment
**               0 - if proj_pt is on the out side of the line segment
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int ncl_reproj_on_lnci (pext,key)
UM_real4 *pext;
UM_int4 *key;
{
        int status, i;
        UM_coord pt;

        for (i=0;i<3;i++) pt[i] = pext[i];

        status = ncl_reproj_on_lnci8(pt,key);

        for (i=0;i<3;i++) pext[i] = (UM_real4) pt[i];

        return(status);
}
/*********************************************************************
**    FUNCTION     : int ncl_reproj_on_lnci8 (pext,key)
**
**      decides whether a point projects on or outof line segment
**    PARAMETERS
**       INPUT  :
**              pext - external point in question
**              lkey - key of the line segment
**       OUTPUT :
**    RETURNS      :
**               1 - if proj_pt is on the segment
**               0 - if proj_pt is on the out side of the line segment
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int ncl_reproj_on_lnci8 (pext,key)
UM_real8 *pext;
UM_int4 *key;
{
        int status, i;
        UM_coord pt;
        struct NCL_fixed_databag geom;

        geom.key = *key;
        status = ncl_retrieve_data_fixed (&geom);
        if (status != UU_SUCCESS) return(status);

        if (ncl_itsa_line(&geom))
                status = ncl_reproj_on_line(pext,geom);
        else if (ncl_itsa_circle(&geom))
                status = ncl_reproj_on_circle(pext,geom);
        else
                status = UU_FAILURE;

        return(status);
}
/*********************************************************************
**    FUNCTION     : int ncl_reproj_on_line (pext,line)
**
**      decides whether a point projects on or outof line segment
**    PARAMETERS
**       INPUT  :
**              pext - external point in question
**              lkey - key of the line segment
**       OUTPUT :
**    RETURNS      :
**               1 - if proj_pt is on the segment
**               0 - if proj_pt is on the out side of the line segment
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int ncl_reproj_on_line (pt,line)
UM_coord pt;
struct NCL_fixed_databag line;
{
        UM_vector vln,vln1;
        UU_REAL dln,t;
        int i,status;
        UM_coord spt,ept;
/*
... get line data.
*/
        if (NCLX_internal_geom)
        {
                NCLX_mdl_line *ln;
                ln = (NCLX_mdl_line *) &line;
                for (i=0;i<3;i++)
                {
                        spt[i] = (*ln).spt[i];
                        ept[i] = (*ln).ept[i];
                }
        }
        else
        {
                struct UM_line_rec *ln;
                ln = (struct UM_line_rec *) &line;
                um_vctovc (ln->spt,spt);
                um_vctovc (ln->ept,ept);
        }

        um_vcmnvc(ept,spt,vln);
        dln = um_mag (vln);
        um_unitvc(vln,vln1);
/*
... project the point on the line; 
... check if the projection point is within the line segment
*/
        status = um_nptsg (pt,spt,vln1,dln,pt,&t);
/*
... if proj not on line segment, get the closest end point.
*/
        if (status != 1)
        {
                if (status == 0)
                        um_vctovc(spt,pt);
                else
                        um_vctovc(ept,pt);
                status = 1;
        }
        else
                status = 0;
        return (status);
}
/*********************************************************************
**    FUNCTION     : int ncl_reproj_on_circle (pext,circle)
**
**      decides whether a point projects on or on extension of a curve.
**    PARAMETERS
**       INPUT  :
**              pext - external point in question
**              cvkey - key of the circle
**       OUTPUT :
**    RETURNS      :
**               1 - if proj_pt is on the arc
**               0 - if proj_pt is on the extension of the arc
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int ncl_reproj_on_circle (pt,circle)
UM_coord pt;
struct NCL_fixed_databag circle;
{
        int i, status;
        UM_coord endpt[2],pv[4],spt,ept,ci_center,ci_svec,ci_nvec;
        UM_vector ptvec,evec;
        UU_REAL ci_dang,sang,eang,tol,curv[2];
        extern UU_REAL um_angle2p_acc();
	UM_int2 ierr;

	ierr = 0;
/*
... get circle data
*/
        if (NCLX_internal_geom)
        {
                NCLX_mdl_circle *ci;
                ci = (NCLX_mdl_circle *) &circle;
                ci_dang = (*ci).dang;
                for (i=0;i<3;i++)
                {
                        ci_center[i] = (*ci).center[i];
                        ci_svec[i] = (*ci).svec[i];
                        ci_nvec[i] = (*ci).nvec[i];
                }
        }
        else
        {
                struct UM_circle_rec *ci;
                ci = (struct UM_circle_rec *) &circle;
                ci_dang = ci->dang;
                um_vctovc (ci->center,ci_center);
                um_vctovc (ci->svec,ci_svec);
                um_vctovc (ci->nvec,ci_nvec);
        }
/*
... get circle starting point and ending point.
*/
	status = ncl_get_cv_endpts_all(circle.key,UM_POINT,&ierr,endpt,pv,curv);
	if (ierr >0) return(UU_FAILURE);
	um_vctovc(endpt[0],spt);
	um_vctovc(endpt[1],ept);
/*
... calcuate the angles between (svec,ptvec) and (ptvec,evec)
*/
        um_vcmnvc(pt,ci_center,ptvec);
        um_vcmnvc(ept,ci_center,evec);
        sang = um_angle2p_acc(ci_svec,ptvec,ci_nvec);
        eang = um_angle2p_acc(ptvec,evec,ci_nvec);
/*
... if sang and eang add up to dang, pt is on circle, otherwise
... it's on circle exetension.
*/
        gettol(&tol);
        if (fabs(sang +eang-ci_dang) > tol)
        {
/*
... if on extension, get the closest end point.
*/
                if (um_dcccc(spt,pt)<um_dcccc(ept,pt))
                        um_vctovc(spt,pt);
                else
                        um_vctovc(ept,pt);
                status = 1;
        }

        return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_2cv_tanto (key1,key2,itanto,icnct,ptt,pte,ierr)
**
**       Determine if two curves/lines/circles are tangent to each
**       other at their intersection point.
**
**    PARAMETERS
**       INPUT  :
**              key1:   key to cv 1;
**              key2:   key to cv 2;
**
**       OUTPUT :
**              itanto: 1 - tangent
**                      0 - not tangent.
**              icnct:  1 - have intersection
**                      0 - no intersection
**              ptt    - point of tangency
**              pte    - other end of second curve
**
**    RETURNS      : UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
*********************************************************************/
int ncl_2cv_tanto (key1,key2,itanto,icnct,ptt,pte,ierr)
UM_int4 *key1,*key2;
UM_int2 *itanto,*icnct,*ierr;
UM_coord ptt,pte;
{
        int i,j,status;
	UU_KEY_ID cvkey;
        UU_REAL tol,curv[2];
        UM_coord pt1[2],pv1[4],pt2[2],pv2[4];
/*
... initilization.
*/
        *itanto = 0;
        *icnct = 0;
        *ierr = 0;
/*
... get cv1 starting point and ending point.
*/
	cvkey = *key1;
	status = ncl_get_cv_endpts_all(cvkey,UM_FRSTDERIV,ierr,pt1,pv1,curv);
	if (*ierr >0 ) return (0);
/*
... get cv2 starting point and ending point.
*/
	cvkey = *key2;
	status = ncl_get_cv_endpts_all(cvkey,UM_FRSTDERIV,ierr,pt2,pv2,curv);
	if (*ierr >0 ) return (0);
/*
... unitize tangent vector
*/
        for (i=0; i<2; i++)
        {
                um_unitvc(pv1[i],pv1[i]);
                um_unitvc(pv2[i],pv2[i]);
        }
/*
.. Check if the 2 cvs are tangent at connecting point
*/
        gettol(&tol);
        for (i =0; i<2; i++)
        {
                for (j = 0; j< 2; j++)
                {
                        if (um_dcccc(pt1[i],pt2[j]) < tol)
                        {
                                *icnct = 1;
                                if (um_vcparall(pv1[i],pv2[j])) *itanto = 1;
                                um_vctovc(pt1[i],ptt);
                                um_vctovc(pt1[1-i],pte);
                                return (status);
                        }
                }
        }

        return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_get_cv_endpts_all(key,evflag,ierr,pt,pv,cur)
**      Get the cartesian  coordinates of the starting & end point of 
**	a curve and its derivatives.
**      point vector.
**    PARAMETERS
**       INPUT  : 
**          key			Key of entity to evaluate.
**          evflag            	UM_POINT=>     point
**                            	UM_FRSTDERIV=> 1st deriv plus above
**                            	UM_SECDERIV=>  2nd deriv plus above
**                            	UM_CURVATURE=> curvature plus above
**       OUTPUT :
**          pt                  pt[0]            Starting point.
**                              pt[1]            ending point.
**	    pv			pv[0]		 first deriv for starting point
**                              pv[1]            first deriv for ending point.
**	    			pv[3]		 second deriv for starting point
**                              pv[4]            second deriv for ending point.
**          cur                 cur[0]		 curvature at starting point
**                              cur[1]		 curvature at ending point
**	    ierr                error
**    RETURNS      :
**                              none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_cv_endpts_all(key,evflag,ierr,pt,pv,curv)
UU_KEY_ID key;
int evflag;
UM_int2 *ierr;
UM_coord pt[2],pv[4];
UU_REAL curv[2];
{
        struct UM_entitydatabag e;
	struct UM_evcrvout evout;
        UM_transf tfmat;
        int status;
/*
.....Get the entity 
*/
        e.key = key;
        status = ncl_retrieve_data_fixed(&e);
        if (status != UU_SUCCESS) goto failed;
        status = uc_retrieve_transf(e.key, tfmat);
        if (status != UU_SUCCESS) goto failed;
/*
... Evaluate the starting point of the curve
*/
	status = uc_init_evcrvout(&e, &evout);
        if (status != UU_SUCCESS) return(status);

	status = uc_evcrv(evflag,(UU_REAL) 0.0,&e,tfmat,&evout);
        if (status != UU_SUCCESS) goto failed;
        um_vctovc(evout.cp,pt[0]);
        if (evflag > UM_POINT) um_vctovc(evout.dcdu,pv[0]);
        if (evflag > UM_FRSTDERIV) um_vctovc(evout.d2cdu2,pv[2]);
	if (evflag > UM_SECDERIV) curv[0] = evout.curv;
/*
... Evaluate the starting point of the curve
*/
        status = uc_evcrv(evflag,(UU_REAL) 1.0,&e,tfmat,&evout);
        if (status != UU_SUCCESS) goto failed;
        um_vctovc(evout.cp,pt[1]);
        if (evflag > UM_POINT) um_vctovc(evout.dcdu,pv[1]);
        if (evflag > UM_FRSTDERIV) um_vctovc(evout.d2cdu2,pv[3]);
	if (evflag > UM_SECDERIV) curv[1] = evout.curv;

        *ierr = 0;
        goto done;
/*
.....Failure
*/
failed:;
        *ierr = 1;
done:;
        return (0);
}
/*********************************************************************
**    E_FUNCTION     : gfadbglbl(buf, buflen)
**       Write a message (BUF of length BUFLEN).
**    PARAMETERS
**       INPUT  :
**          buf                     character buffer (no terminator)
**          buflen                  length of buffer
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gfadbglbl()
{
      	static char *cstr="  DS     CS     CS modif    err   numpts    tot dis        AVOID      SKIP";
	printf("%s \n",cstr);

	return;
}
/*********************************************************************
**    E_FUNCTION     : gfadbgdata(tb)
**       Write a message (BUF of length BUFLEN).
**    PARAMETERS
**       INPUT  :
**          buf                     character buffer (no terminator)
**          buflen                  length of buffer
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gfadbgdata(tb)
UM_real8 *tb;
{
	printf("%3d     %3d",(int)tb[0],(int)tb[1]);

	switch ((int)tb[2])
	{
		case 646:
			printf("     %-7s","TNTO");
			break;
		case 714:
			printf("     %-7s","CS_TO");
			break;
		case 715:
			printf("     %-7s","CS_PAST");
			break;
		case 71:
			printf("     %-7s","CS_ON");
			break;
	}
	printf("     %-3d",(int)tb[3]);
	printf("     %-3d",(int)tb[4]);
	printf("     %-10.4f",tb[5]);
	if ((int)tb[6] == 0) printf("     %-7s","---");
	else printf("     %-7s","AVOIDED");	
	if ((int)tb[7] == 0) printf("     %-7s","---");
	else printf("     %-7s","SKIPPED");	
        printf("\n");

        return;
}
/*********************************************************************
**    E_FUNCTION     : gfadbgline
**	print out a line.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gfadbgline()
{
	printf("---------------------------------------------------\n");
}

