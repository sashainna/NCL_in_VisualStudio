/*********************************************************************
**    NAME: n2query.c
**       CONTAINS:
**          int ncl_query(key, list) 
**          int ncl_closed_part(cldu, cldv)
**          int ncl_curve_query(key, list) 
**          int ncl_surface_query(key, list)
**          int ncl_meshsf_query(key, list)
**          int ncl_evalsf_query(key, list)
**          int ncl_rbcv_query(key, list)
**          int ncl_rbsf_query(key, list)
**          int ncl_polyln_query(key, list)
**  COPYRIGHT  1986  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       n2query.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:20
**********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mderror.h"
#include "mdebug.h"
#include "ulist.h"
#include "mdgenent.h"
#include "mdrel.h"
#include "mdeval.h"
#include "mcrv.h"
#include "msrf.h"
#include "mxxx.h"
#include "mattr.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "modef.h"
#include "nccs.h"
#include "class.h"  /* for UC_attributedatabag */
#include "jquery.h" /* query macros, etc. */
#include "nclfc.h"
#include "mfort.h"
#include "nclvx.h"
#include "ncl.h"

/*********************************************************************
**    I_FUNCTION : int ncl_query(key,list)
**       This function produces the query output for NCL entity's
**       part information. Cannibalized from model/m2query.c:um_query().
**    PARAMETERS
**       INPUT  :
**              key         Entity to display
**              list        The list package.
**       OUTPUT :
**              none.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_query(key, list) 
UU_KEY_ID  key;
UU_LIST 	  *list;
	{
	int  status = UU_SUCCESS;
	UM_transf tfmat;
	struct UM_entitydatabag e;
	struct UC_attributedatabag attr;
	char buf[300];
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT];

	e.key = key;
	if (uc_retrieve_data(&e, sizeof(struct UM_entitydatabag)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;

	uj_putstr(list, " ");
	ncl_get_label(&e,label);
	sprintf(buf,"- See *SHOW/%s for more information.", label);
	uj_putstr(list, buf);

	goto done;

failed: status = UU_FAILURE;
done:;
	return(status);
	}
	
/*********************************************************************
**    I_FUNCTION : int ncl_params_part(list,iswap,revnorm)
**			Print out closed in u and closed in v.
**    PARAMETERS   
**       INPUT  : 
**          iswap    - swap UV, reverse U, reverse V flag
**          revnorm  - reverse normal flag
**       OUTPUT :  
**          list
**    RETURNS: none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_params_part(list,iswap,revnorm)
UU_LIST	*list;
int iswap;
UU_LOGICAL revnorm;
{
	char buf[300];
	UU_LOGICAL urev,vrev,uvswap;

	if (iswap > 0 && iswap < 8)
	{
		urev = iswap & 4;
		vrev = iswap & 2;
		uvswap = iswap & 1;
	}
	else
		urev = vrev = uvswap = UU_FALSE;

	if (uvswap || urev || vrev || revnorm)
	{
		if (uvswap)
		{
			sprintf(buf,"   U,V parameters swapped");
			uj_putstr(list, buf);
		}
		if (urev)
		{
			sprintf(buf,"   U parameter is reversed");
			uj_putstr(list, buf);
		}
		if (vrev)
		{
			sprintf(buf,"   V parameter is reversed");
			uj_putstr(list, buf);
		}
		if (revnorm)
		{
			sprintf(buf,"   Normal direction is reversed");
			uj_putstr(list, buf);
		}
		uj_putstr(list, " ");
	}
}

/*********************************************************************
**    I_FUNCTION : int ncl_closed_part(cldu, cldv)
**			Print out closed in u and closed in v.
**    PARAMETERS   
**       INPUT  : 
**          list
**          int closdinu
**          int closdinv
**       OUTPUT :  
**				none
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_closed_part(list, cldu, cldv)
UU_LIST	*list;
int *cldu, *cldv;
	{
	char    ans[20];
	char	buf[300];

/*
.....Changed 'sprintf' to 'strcpy' when no
.....no variables are given.
.....Bobby  -  2/21/92
*/
	strcpy(buf,"- Additional Data -");
	uj_putstr(list, buf);

/*
.....This is a curve
*/
	if (cldv == NULL)
		{
		if (*cldu)
			strcpy(ans,"Yes");
		else
			strcpy(ans,"No");
		sprintf(buf,"   closed continuous.......... %s", ans);
		uj_putstr(list, buf);
		}
/*
.....We have a surface
*/
	else
		{
		if (*cldu)
			strcpy(ans,"Yes");
		else
			strcpy(ans,"No");
		sprintf(buf,"   closed continuous in U .... %s", ans);
		uj_putstr(list, buf);

		if (*cldv)
			strcpy(ans,"Yes");
		else
			strcpy(ans,"No");
		sprintf(buf,"   closed continuous in V .... %s", ans);
		uj_putstr(list, buf);
		}

	return (0);
	}
/*********************************************************************
**    I_FUNCTION : int ncl_curve_query(key, list)
**			This function produces the query output for an NCL curve
**    PARAMETERS   
**       INPUT  : 
**          key	   key to an NCL curve.
**       OUTPUT :  
**				list		list to contain the query informaiton.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_curve_query(key, list)
UU_KEY_ID	key;
UU_LIST		*list;
	{
	struct NCL_curve_rec  	nclent;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	int   status = UU_SUCCESS;
	char	buf[300];
	char mlabel[NCL_MAX_LABEL_AND_SUBSCRIPT];

	/* get the entity data */
	nclent.key = key;
	if (uc_retrieve_data(&nclent, sizeof(struct NCL_curve_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;

/*
.....For some reason, the "sprintf"'s in 'ncl_closed_part'
.....change the contents of 'label'.  Therefore, we must
.....save label in a local variable.
.....Bobby  -  2/21/92
*/
	strcpy (mlabel,nclent.label);
	ncl_closed_part(list, &nclent.closdinu, NULL);

/*	strncpy(label,nclent.label, 8); */
	uj_putstr(list, " ");
	sprintf(buf,"- See *SHOW/%s for more information.", mlabel);
	uj_putstr(list, buf);

	goto done;
failed: status = UU_FAILURE;
done:;
	return(status);
	}

#if 0
/*********************************************************************
**    I_FUNCTION : int ncl_surface_query(key, list)
**			This function produces the query output for an NCL curve
**    PARAMETERS   
**       INPUT  : 
**          key	   key to an NCL curve.
**       OUTPUT :  
**				list		list to contain the query informaiton.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_surface_query(key, list)
UU_KEY_ID	key;
UU_LIST		*list;
	{
	struct NCL_surface_rec  	nclent;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	int   status = UU_SUCCESS;
	char	buf[300];
	char mlabel[NCL_MAX_LABEL_AND_SUBSCRIPT];

	/* get the entity data */
	nclent.key = key;
	if (uc_retrieve_data(&nclent, sizeof(struct NCL_surface_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;

/*
.....For some reason, the "sprintf"'s in 'ncl_closed_part'
.....change the contents of 'label'.  Therefore, we must
.....save label in a local variable.
.....Bobby  -  2/21/92
*/
	strcpy (mlabel,nclent.label);
	ncl_closed_part(list, &nclent.closdinu, &nclent.closdinv);

   sprintf(buf,"- Curves on surface = %d", nclent.no_sskey);
   uj_putstr(list, buf);
   uj_putstr(list, " ");

/*	strncpy(label,nclent.label, 8); */
   uj_putstr(list, " ");
	sprintf(buf,"- See *SHOW/%s for more information.", mlabel);
	uj_putstr(list, buf);

	goto done;
failed: status = UU_FAILURE;
done:;
	return(status);
	}

/*********************************************************************
**    I_FUNCTION : int ncl_meshsf_query(key, list)
**			This function produces the query output for an NCL curve
**    PARAMETERS   
**       INPUT  : 
**          key	   key to an NCL curve.
**       OUTPUT :  
**				list		list to contain the query informaiton.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_meshsf_query(key, list)
UU_KEY_ID	key;
UU_LIST		*list;
	{
	struct NCL_meshsf_rec  	nclent;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	int   status = UU_SUCCESS;
	char	buf[300];
	char mlabel[NCL_MAX_LABEL_AND_SUBSCRIPT];

	/* get the entity data */
	nclent.key = key;
	if (uc_retrieve_data(&nclent, sizeof(struct NCL_meshsf_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;

/*
.....For some reason, the "sprintf"'s in 'ncl_closed_part'
.....change the contents of 'label'.  Therefore, we must
.....save label in a local variable.
.....Bobby  -  2/21/92
*/
	strcpy (mlabel,nclent.label);
	ncl_closed_part(list, &nclent.closdinu, &nclent.closdinv);

/*	strncpy(label,nclent.label, 8); */
	uj_putstr(list, " ");
	sprintf(buf,"- See *SHOW/%s for more information.", mlabel);
	uj_putstr(list, buf);

	goto done;
failed: status = UU_FAILURE;
done:;
	return(status);
	}
#endif

/*********************************************************************
**    I_FUNCTION : int ncl_evalsf_query(key, list)
**			This function produces the query output for an NCL curve
**    PARAMETERS   
**       INPUT  : 
**          key	   key to an NCL curve.
**       OUTPUT :  
**				list		list to contain the query informaiton.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_evalsf_query(key, list)
UU_KEY_ID	key;
UU_LIST		*list;
	{
	struct NCL_evalsf_rec  	nclent;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	int   status = UU_SUCCESS;
	char	buf[300];
	char mlabel[NCL_MAX_LABEL_AND_SUBSCRIPT];

	/* get the entity data */
	nclent.key = key;
	if (uc_retrieve_data(&nclent, sizeof(struct NCL_evalsf_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)	goto failed;

/*
.....For some reason, the "sprintf"'s in 'ncl_closed_part'
.....change the contents of 'label'.  Therefore, we must
.....save label in a local variable.
.....Bobby  -  2/21/92
*/
	strcpy (mlabel,nclent.label);
	ncl_closed_part(list, &nclent.closdinu, &nclent.closdinv);

/*	strncpy(label,nclent.label, 8); */
	uj_putstr(list, " ");
	sprintf(buf,"- See *SHOW/%s for more information.", mlabel);
	uj_putstr(list, buf);

	goto done;
failed: status = UU_FAILURE;
done:;
	return(status);
	}

/*********************************************************************
**    I_FUNCTION : int ncl_cvsf_query(key, list)
**         This function produces the query output for a Rational B-Spline curve.
**    PARAMETERS   
**       INPUT  : 
**          key      key to a Rational B-Spline curve on surface. 
**       OUTPUT :  
**          list     list to contain the query information.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_cvsf_query(key, list)
UU_KEY_ID key;
UU_LIST   *list;
{
   struct UM_uvcvonsf_rec nclent;
   struct UC_attributedatabag attr;
   UM_transf tfmat;
   int   status = UU_SUCCESS;
   int i;
   char  buf[300];
   UU_REAL *pt;
   UU_REAL *wt, weight,*t;
   char mlabel[NCL_MAX_LABEL_AND_SUBSCRIPT];

   /* get the entity data */
   nclent.key = key;
   if (ncl_retrieve_data(&nclent, sizeof(struct UM_rbsplcrv_rec)) != UU_SUCCESS)
      goto failed;
   if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
   if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)   goto failed;

/*
.....For some reason, the "sprintf"'s in 'ncl_closed_part'
.....change the contents of 'label'.  Therefore, we must
.....save label in a local variable.
.....Bobby  -  2/21/92
*/
	strcpy (mlabel,nclent.label);
   ncl_closed_part(list, &nclent.closdinu, 0);
   sprintf(buf,"- Base SF key = %d", nclent.bskey);
   uj_putstr(list, buf);
   uj_putstr(list, " ");

   sprintf(buf,"- Degree = %d", nclent.k-1);
   uj_putstr(list, buf);
   uj_putstr(list, " ");
   sprintf(buf,"- Number of spans = %d", nclent.n);
   uj_putstr(list, buf);
   uj_putstr(list, " ");

   pt = nclent.pt;
   wt = nclent.wt;
   t = &nclent.t[nclent.k-1];
/*
.....uv curve is units independant
.....print as it is
*/
   for (i=0,weight = 1.0; i<nclent.no_pt; i++, pt += 3)
	{		
		if (nclent.no_wt && wt)
			weight = wt[i];

		sprintf(buf,"  pt=<%11.6f,%11.6f,%11.6f>   wt=%10.5f   t=%10.5f",
			pt[0], pt[1], pt[2], weight, t[i]);
		uj_putstr(list, buf);
	}

	goto done;
failed: 
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ncl_rbcv_query(key, list)
**         This function produces the query output for a Rational B-Spline curve.
**    PARAMETERS   
**       INPUT  : 
**          key      key to a Rational B-Spline curve. 
**       OUTPUT :  
**          list     list to contain the query information.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_rbcv_query(key, list)
UU_KEY_ID key;
UU_LIST   *list;
   {
   UM_int2 idx, ival;
   struct UM_rbsplcrv_rec nclent;
   struct UC_attributedatabag attr;
   UM_transf tfmat;
   int   status = UU_SUCCESS;
   int i;
   char  buf[300];
   UU_REAL *pt;
   UU_REAL *wt,weight,*t;
   char mlabel[NCL_MAX_LABEL_AND_SUBSCRIPT];
   UM_coord spt;

   /* get the entity data */
   nclent.key = key;
   if (ncl_retrieve_data(&nclent, sizeof(struct UM_rbsplcrv_rec)) != UU_SUCCESS)
      goto failed;
   if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
   if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS)   goto failed;

/*
.....For some reason, the "sprintf"'s in 'ncl_closed_part'
.....change the contents of 'label'.  Therefore, we must
.....save label in a local variable.
.....Bobby  -  2/21/92
*/
	strcpy (mlabel,nclent.label);
   ncl_closed_part(list, &nclent.closdinu, 0);

   sprintf(buf,"- Degree = %d", nclent.k-1);
   uj_putstr(list, buf);
   uj_putstr(list, " ");
   sprintf(buf,"- Number of spans = %d", nclent.n);
   uj_putstr(list, buf);
   uj_putstr(list, " ");

   pt = nclent.pt;
   wt = nclent.wt;
   t = &nclent.t[nclent.k-1];

   idx = 264;
   getifl (&idx, &ival);

   uj_putstr(list, " ");
	UJ_PUT0(list,"***The following information is displayed in Model Space");
   uj_putstr(list, " ");

   for (i=0, weight = 1.; i<nclent.no_pt; i++, pt += 3)
	{
		if (nclent.no_wt && wt)
			weight = wt[i];
     if (ival == 0) /* inches */
            sprintf(buf,"  pt=<%11.6f,%11.6f,%11.6f>   wt=%10.5f   t=%10.5f",
            pt[0], pt[1], pt[2], weight, t[i]);
     else
     {
            UM_cc_inttoext(pt, spt);
            sprintf(buf,"  pt=<%11.5f,%11.5f,%11.5f>   wt=%10.4f   t=%10.4f",
            spt[0], spt[1], spt[2], weight, t[i]);
     }

     uj_putstr(list, buf);
	 
	}
/*
.....output start and end parameters for trimmed/extended curves
*/
	if (nclent.t0 != nclent.t[0] || nclent.t1 != nclent.t[nclent.no_t-1])
	{
		uj_putstr(list, " ");
		sprintf (buf, "Redefined from t0 = %6.4f to t1 = %6.4f",nclent.t0,nclent.t1); 
		uj_putstr(list, buf);
		uj_putstr(list, " ");
	}
   goto done;
failed: status = UU_FAILURE;
done:;
   return(status);
   }

/*********************************************************************
**    I_FUNCTION : int ncl_rbsf_query(key, list)
**         This function produces the query output for a Rational B-Spline surface.
**    PARAMETERS   
**       INPUT  : 
**          key      key to a Rational B-Spline surface. 
**       OUTPUT :  
**          list     list to contain the query information.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_rbsf_query(key, list)
UU_KEY_ID key;
UU_LIST   *list;
{
	UM_int2 idx, ival;
	struct UM_rbsplsrf_rec nclent;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	int   status = UU_SUCCESS;
	int i;
	char  buf[300];
	UU_REAL *pt;
	UU_REAL *wt,weight;
	char mlabel[NCL_MAX_LABEL_AND_SUBSCRIPT];
	UM_coord spt;
	UU_REAL param[8];

   /* get the entity data */
	nclent.key = key;
	if (ncl_retrieve_data(&nclent, sizeof(struct UM_rbsplsrf_rec)) != UU_SUCCESS)
		goto failed;
	if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 
	if (uc_retrieve_transf(key, tfmat) != UU_SUCCESS) goto failed;

/*
.....For some reason, the "sprintf"'s in 'ncl_closed_part'
.....change the contents of 'label'.  Therefore, we must
.....save label in a local variable.
.....Bobby  -  2/21/92
*/
	strcpy (mlabel,nclent.label);

	idx = 264;
	getifl (&idx, &ival);
	if (nclent.primitive > 2)
	{ 
		if (ival == 1)
		{
			for (i = 0; i < 8; i++)
				param[i] = nclent.prim_param[i]*25.4;
		}
		else
		{
			for (i = 0; i < 8; i++)
				param[i] = nclent.prim_param[i];
		}
	}

	uj_putstr(list," ");
	UJ_PUT0(list,
		"***The following information is displayed in Model Space");
	uj_putstr(list," ");
/*
......Primitive data
*/
	um_primitive_query(nclent.primitive,nclent.prim_param,list);
	
	ncl_closed_part(list, &nclent.closdinu, &nclent.closdinv);
	ncl_params_part(list, nclent.swapuv, nclent.rev_normal);

	sprintf(buf,"- Curves on surface = %d", nclent.no_sskey);
	uj_putstr(list, buf);
	uj_putstr(list," ");

	sprintf(buf,
		"- Degree in u = %d, degree in v = %d", nclent.ku-1, nclent.kv-1);
	uj_putstr(list, buf);

	uj_putstr(list," ");
	sprintf(buf,"- Spans in u = %d, Spans in v = %d", nclent.nu,nclent.nv);
	uj_putstr(list, buf);
	uj_putstr(list," ");
/*
... jingrong. do not show points if the entity is a primitive surface.
*/
	if (nclent.primitive >= NCLSF_PLANE) goto done;	

	pt=nclent.pt;
	wt=nclent.wt;
	for (i=0, weight = 1.; i<nclent.no_pt; i++, pt += 3)
	{
		if (nclent.no_wt && wt)
			weight = wt[i];
		if (ival == 0) /* inches */
			sprintf(buf,"  pt=<%11.6f,%11.6f,%11.6f>   wt=%10.5f",
				pt[0], pt[1], pt[2], weight);
		else
		{
			UM_cc_inttoext(pt, spt);
			sprintf(buf,"  pt=<%11.5f,%11.5f,%11.5f>   wt=%10.4f",
				spt[0], spt[1], spt[2], weight);
		}

		uj_putstr(list, buf);
	}

	goto done;
failed: status = UU_FAILURE;
done:;
	 return(status);
}

/*********************************************************************
**    I_FUNCTION : int ncl_polyln_query(key, list)
**         This function produces the query output for a polyline
**    PARAMETERS   
**       INPUT  : 
**          key      key to a polyline
**       OUTPUT :  
**          list     list to contain the query information.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_polyln_query(key, list)
UU_KEY_ID key;
UU_LIST   *list;
   {
   UM_int2 idx, ival;
   UM_real8 tol;
   struct UM_polyline_rec nclent;
   struct UC_attributedatabag attr;
   int   status = UU_SUCCESS;
   int i,npt;
   char  buf[300];
   UM_coord *pt,spt;

   /* get the entity data */
   nclent.key = key;
   if (ncl_retrieve_data(&nclent, sizeof(struct UM_polyline_rec)) != UU_SUCCESS)
      goto failed;
   if (uc_retrieve_attr(key, &attr) != UU_SUCCESS) goto failed; 

   npt = nclent.no_pt;
   uj_putstr(list, " ");
   sprintf(buf,"- Number of points = %d",npt);
   uj_putstr(list, buf);

   pt = (UM_coord *) nclent.pt;

   idx = 264;
   getifl (&idx, &ival);
   gettol (&tol);
   if (UM_SQDIS(pt[0],pt[npt-1]) < tol*tol)
   {
		UJ_PUT0(list,"- Closed");
   }
   else
   {
		UJ_PUT0(list,"- Open");
   }

   if (npt > 350) npt = 350;
	uj_putstr(list," ");
   UJ_PUT0(list,"***The following information is displayed in Model Space");
	uj_putstr(list," ");
   for (i = 0; i < npt; i++, pt++)
	{
     if (ival == 0) /* inches */
            sprintf(buf,"  pt=<%11.6f,%11.6f,%11.6f>",
            pt[0][0], pt[0][1], pt[0][2]);
     else
     {
            UM_cc_inttoext(pt[0], spt);
            sprintf(buf,"  pt=<%11.5f,%11.5f,%11.5f>",
            spt[0], spt[1], spt[2]);
     }

     uj_putstr(list, buf);
	}

   goto done;
failed: status = UU_FAILURE;
done:;
   return(status);
}
