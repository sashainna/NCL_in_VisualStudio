/*********************************************************************
**    NAME         :  nepv.c
**       CONTAINS: Routines to handle point vectors.
**       ncl_put_pointvec (ncl, e)
**       ncl_get_pointvec (e, ncl)
**			ncl_pntvec_revsf (sfkey,buff)
**    COPYRIGHT 1991 (c) NCCS  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nepv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:44
*********************************************************************/
#include "usysdef.h"
#include "umath.h" 
#include "udebug.h"
#include "uhep.h" 
/*#include "class.h" */
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mdcoord.h"
/*#include "mattr.h" */
/*#include "mdattr.h" */
#include "mdebug.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION :  ncl_put_pointvec (ncl, e)
**       Moves NCL point vec into unibase record.
**    PARAMETERS
**       INPUT  :
**          ncl    - NCL pointvec
**       OUTPUT :
**          e      - unibase pointvec record.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_put_pointvec (ncl, e)
   struct NCLI_pointvec_rec *ncl;
   struct NCL_nclpv_rec *e;
   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_put_pointvec(ncl=%x, e=%x)",ncl,e));

   status = UU_SUCCESS;
   e->rel_num = NCL_POINTVEC_REL;

/*   if (e->key != 0)
      status = ur_retrieve_data(e, sizeof(struct NCL_nclpv_rec));

   if (status != UU_SUCCESS)
      go to done;
*/
   ncl_real8_to_uureal(3, ncl->pt, e->pt);
   ncl_real8_to_uureal(3, ncl->ve, e->ve);

done:;
   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION :  ncl_get_pointvec (e, ncl)
**       Moves unibase point vec into NCL record.
**    PARAMETERS
**       INPUT  :
**          e      - unibase pointvec record.
**       OUTPUT :
**          ncl    - NCL pointvec
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_get_pointvec (e, ncl)
   struct NCL_nclpv_rec *e;
   struct NCLI_pointvec_rec *ncl;
   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_get_pointvec(e=%x, ncl=%x)",e,ncl));

   status = UU_SUCCESS;

   ncl_uureal_to_real8(3, e->pt, ncl->pt);
   ncl_uureal_to_real8(3, e->ve, ncl->ve);

done:;
   uu_dexit;
   return(status);
   }
/*********************************************************************
**    E_FUNCTION :  ncl_ev_pntvec (evflag,u,eptr,tfmat,evout)
**       Evalute a pntvec. 
**    PARAMETERS
**       INPUT  :
**          evflag  - evalution flag
**          u       - parameter at which to evalute
**          eptr    - pointer to pointvec entity
**          tfmat   - transformation matrix
**       OUTPUT :
**          evout   - evalution output record.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_ev_pntvec (evflag,u,eptr,tfmat, evout)
   int evflag;
   UM_param u;
   struct NCL_nclpv_rec *eptr;
   UM_transf tfmat;
   struct UM_evcrvout *evout;
   {
   UM_vector vec1;
   struct UM_line_rec unive;
   int status;

   uu_denter(UU_MTRC,(us,"ncl_ev_pntvec(e=%x, ncl=%x)",e,ncl));
   unive.rel_num = UM_LINE_REL;
   vec1[0] = eptr->pt[0] + eptr->ve[0];
   vec1[1] = eptr->pt[1] + eptr->ve[1];
   vec1[2] = eptr->pt[2] + eptr->ve[2];
   um_vctovc(eptr->pt, unive.spt);
   um_vctovc(vec1, unive.ept);
  	status = uc_evcrv(evflag, u, &unive, tfmat, evout);

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION :  int ncl_pntvec_revsf (sfkey,buff)
**       Fortran callable function to extract axis from surface as pointvector
**    PARAMETERS
**       INPUT  :
**          sfkey   - key id of surface
**       OUTPUT :
**          buff    - buffer that stores point vector
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_pntvec_revsf (sfkey,buff)
UM_int4 *sfkey;
UM_real8 buff[6];
{
	struct NCL_fixed_databag e;
	struct NCL_trimsf_rec surf;
	struct NCL_revsurf_rec rev;
	struct NCL_fixed_databag cv;
	struct UM_evcrvout evcrv;
	int i,status;
	UM_int2 primtyp;
	UM_real8 primdata[16];
	UM_transf tfmat;
	UM_int2 IPT = NCLI_POINT;
	UM_int2 IVE = NCLI_VECTOR;
	UM_coord pt0,ppt,npt;
	UM_vector unvc;
	UU_REAL tol;
	UM_real8 tol8;

	gettol (&tol8);
	tol = tol8;

	e.key = *sfkey;
	um_identtf(tfmat);
/*
.....consider base sf for trimmed surface
*/
	status = ncl_retrieve_data_fixed(&e);
	if (e.rel_num == NCL_TRIMSF_REL)
	{
		surf.key = *sfkey;
		status = ncl_retrieve_data_fixed(&surf);
		if (uc_retrieve_transf(surf.key, tfmat) != UU_SUCCESS)
			return (UU_FAILURE);
		e.key = surf.bs_key;
		status = ncl_retrieve_data_fixed(&e);
	}
	ncl_get_sf_primtyp(&e.key,&primtyp);
	if(primtyp!= NCLSF_CYLINDER && primtyp!=NCLSF_CONE
		&& e.rel_num != NCL_REVSURF_REL)
		return (UU_FAILURE);

/*
.....retrieve axis of revolution
*/
	if(e.rel_num == NCL_REVSURF_REL)
	{
		rev.key = e.key;
		status = ncl_retrieve_data_fixed(&rev);
		for(i=0;i<3;i++)
		{
			buff[i] = rev.pta[i];
			buff[i+3] = rev.vca[i];	
		}
/*
.....Shift Point vector to the plane of the revolved curve starting point
*/
		cv.key = rev.cvkey;
		status = ncl_retrieve_data_fixed(&cv);
		if (status != UU_SUCCESS) return(status);
		status = uc_init_evcrvout (&cv,&evcrv);
		if (status != UU_SUCCESS) return(status);
		status = uc_evcrv(UM_POINT, 0.0, &cv, UM_idmat, &evcrv);
		for (i = 0; i < 3; i++)
		{
			pt0[i] = evcrv.cp[i];
			ppt[i] = buff[i];
			unvc[i] = buff[i+3];
		}
		um_unitvc(unvc,unvc);
		um_nptpln(ppt,pt0,unvc,npt);
		for (i = 0; i < 3; i++)
			buff[i] = npt[i];
/*
.....Adjust for trim surface transformation matrix
*/
		if (!um_is_idmat(tfmat))
		{	
			um_cctmtf(buff,tfmat,buff);
			um_vctmtf(&buff[3],tfmat,&buff[3]);
			um_unitvc (&buff[3],&buff[3]);
		}
	}
	else if(primtyp == NCLSF_CYLINDER || primtyp == NCLSF_CONE)
	{
		status = ncl_get_sf_primdat(sfkey,&primtyp,primdata);
		if (primtyp == NCLSF_CYLINDER)
			for(i=0;i<3;i++)
			{
				buff[i] = primdata[i];
				buff[i+3] = primdata[i+3];	
			}
		else
		{
			um_translate_point (primdata,primdata[7]+primdata[8],&primdata[3],buff);
			for(i=0;i<3;i++)
				buff[i+3] = -primdata[i+3];	
		}
	}
/*
.....Adjust for modsys/refsys
*/
	from_unibase (buff,buff,&IPT);
	from_unibase (&buff[3],&buff[3],&IVE);
	
	return (status);
}
