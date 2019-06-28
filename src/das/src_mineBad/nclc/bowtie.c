/*********************************************************************
**    NAME         :  bowtie.c
**       CONTAINS:
**     int ncl_check_bnd (ncrvs,crvp,ktwist)
**     int chk_crv(cv1,cv2,ktwist)
**     int chk_4pts(ptx,pty,ptw,ptz, ktwist)
**
**    COPYRIGHT 1999 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bowtie.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:20 
*********************************************************************/

#include "udebug.h"
#include "mdeval.h"
#include "modef.h"
#include "nccs.h"
#include "nclfc.h"
#include "mdrel.h"

/*********************************************************************
**    E_FUNCTION     : ncl_check_bnd (ncrvs,crvp,ktwist)
**       Determines which of the boundary curves, encoded in crvp, 
**       causes twisting.
**    PARAMETERS   
**       INPUT  : 
**          crvp    - pointer to the first boundary curve    
**          ncrvs   - number of boundary curves
**          itsk    - flag: if itsk=1 slopes are not checked
**       OUTPUT :  
**          ktwist  - array of flags; ktwist[i]=1 iff the i-th curve
**                    should be reversed.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_check_bnd (itsk,ncrvs,crvp,ktwist)
int itsk, ncrvs;
struct NCL_fixed_databag *crvp;
int *ktwist;   
   {
   int i, rel_num, status, twist;
   struct NCL_fixed_databag bp1, bp2;
   char *uu_malloc();

   uu_denter(UU_MTRC,(us,"ncl_check_bnd (ncrvs,crvp,ktwist)"));

   status = UU_SUCCESS;

/*
..... Check which boundary curves cause twisting.
*/
   bp1=crvp[0];
   ktwist[0]=0;
   rel_num=bp1.rel_num;
/*
..... If the first entity is a point, this must be a ruled surface,
..... with no twisting
*/
   if (rel_num == UM_POINT_REL || rel_num == NCL_POINT_REL)
      {
      ktwist[1]=0;
      goto done; 
      }

   bp2=crvp[1];   
   rel_num=bp2.rel_num;
/*
..... If the second entity is a point, this must be a ruled surface,
..... with no twisting
*/
   if (rel_num == UM_POINT_REL || rel_num== NCL_POINT_REL)
       {
       ktwist[1]=0;
       goto done; 
       }

   for (i=1; i<ncrvs && status==UU_SUCCESS; i++)
      {
      status = chk_crv(&bp1,&bp2,&twist);

      if (ktwist[i-1]==0)
         ktwist[i]=twist;
      else
         ktwist[i]=1-twist;

      bp1=bp2;
      bp2=crvp[i+1];   
      }

   if (itsk) goto done;
/*
..... Check which slope curves cause twisting.
*/
   for (i=0; i<ncrvs && status==UU_SUCCESS; i++)
      {
      bp1=crvp[i];
      bp2=crvp[ncrvs+i];
      rel_num=bp2.rel_num;
/*
..... If the second entity is zero, point, vector, or point-vector, 
..... no twisting is checked. 
*/
      if (bp2.key==0 || rel_num == UM_POINT_REL || 
          rel_num== NCL_POINT_REL || rel_num == NCL_POINTVEC_REL ||
          rel_num == NCL_VECTOR_REL)
         {
         ktwist[ncrvs+i]=0;
         }
      else
         {
         status = chk_crv(&bp1,&bp2,&twist);
         if (ktwist[i]==0)
            ktwist[ncrvs+i]=twist;
         else
            ktwist[ncrvs+i]=1-twist;
         }
      }

done:;

   uu_dexitstatus("ncl_check_bnd (ncrvs,crvp,ktwist)", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : chk_crv(cv1,cv2,ktwist)
**       Determines whether the direction of the curve cv2 is "wrong"
**       with respect to the curve cv1.
**    PARAMETERS   
**       INPUT  : 
**          cv1     - pointer to the first curve    
**          cv2     - pointer to the second curve    
**       OUTPUT :  
**          ktwist  - the flag; ktwist=1 iff the second curve cv2
**                    should be reversed.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int chk_crv(cv1,cv2,ktwist)
struct NCL_fixed_databag *cv1, *cv2;
int *ktwist;   
   {
   int i,npat,status,itwist,itim,itwis[2];
   UU_REAL sa, sb, sb1, *sp;
   UM_transf tfmat1, tfmat2;
   UU_LIST slist;
   struct UM_evcrvout evout1, evout2;
   UM_param u;
   UM_coord ptx,pty,ptw,ptz;

   uu_denter(UU_MTRC,(us,"chk_crv(cv1,cv2,ktwist)"));

   status = UU_SUCCESS;
   uu_list_init (&slist, sizeof(UU_REAL), 20, 20);

   if (cv1->key > 0) 
	   status = uc_retrieve_transf (cv1->key, tfmat1);
   else 
	   um_identtf (tfmat1);
   if (status != UU_SUCCESS) goto done;
   if (cv2->key > 0) 
	   status = uc_retrieve_transf (cv2->key, tfmat2);
   else 
	   um_identtf (tfmat2);
   if (status != UU_SUCCESS) goto done;
/* 
..... Form a sequence of patches for these two curves.
*/
	sa = sb = 0.0;
	uu_list_push (&slist, &sb);
	while (sb < .99999 && status == UU_SUCCESS)
	{
		status = ncl_sbsolv (1, sa, &sb, cv1, tfmat1);
		if (status != UU_SUCCESS) goto done;
		sb1 = sb;
		status = ncl_sbsolv (2, sa, &sb, cv2, tfmat2);
		if (status != UU_SUCCESS) goto done;
		if (sb > sb1) sb = sb1;
		if (slist.cur_cnt < 4 && sb > sa+.25) sb = sa + .25;
		uu_list_push (&slist, &sb);
		sa = sb;
	}
	if (sb < 0.999)
	{
		sb = 1.;
		uu_list_push (&slist, &sb);
	}

	if (status == UU_SUCCESS)
	{
		npat = slist.cur_cnt - 1;
		sp = (UU_REAL *) UU_LIST_ARRAY (&slist);
	}
	else 
		goto done;
/*
..... check twisting
*/
	*ktwist = itwis[0] = itwis[1] = 0;

	itim = 0;
	status = uc_init_evcrvout (cv1, &evout1);
	if (status != UU_SUCCESS) goto done;
	status = uc_init_evcrvout (cv2, &evout2);
	if (status != UU_SUCCESS) goto done;
/*
..... Eduard 10/04/1999. First check endpoints for twisting.
*/
repeat:
	sa = 0.; sb = 1.;
	status = uc_evcrv (UM_POINT, sa, cv1, tfmat1, &evout1);
	if (status != UU_SUCCESS) goto done;
	um_vctovc (evout1.cp, ptx);
	if (itim == 0)
		status = uc_evcrv (UM_POINT, sa, cv2, tfmat2, &evout2);
	else	  
		status = uc_evcrv (UM_POINT, 1.-sa, cv2, tfmat2, &evout2);
	if (status != UU_SUCCESS) goto done;
	um_vctovc (evout2.cp, pty);

	status = uc_evcrv (UM_POINT, sb, cv1, tfmat1, &evout1);
	if (status != UU_SUCCESS) goto done;
	um_vctovc (evout1.cp, ptw);
	if (itim == 0)
		status = uc_evcrv (UM_POINT, sb, cv2, tfmat2, &evout2);
	else	  
		status = uc_evcrv (UM_POINT, 1.-sb, cv2, tfmat2, &evout2);
	if (status != UU_SUCCESS) goto done;
	um_vctovc (evout2.cp, ptz);

	status = chk_4pts(ptx,pty,ptw,ptz, &itwist);
    if (status != UU_SUCCESS) goto done;	
	itwis[itim] += itwist;
/* 
..... Consider consecutive patches. 
*/
	u = sp[0];
	status = uc_evcrv (UM_POINT, u, cv1, tfmat1, &evout1);
	if (status != UU_SUCCESS) goto done;
	um_vctovc (evout1.cp, ptx);
	if (itim == 0)
		status = uc_evcrv (UM_POINT, u, cv2, tfmat2, &evout2);
	else	  
		status = uc_evcrv (UM_POINT, 1.-u, cv2, tfmat2, &evout2);
	if (status != UU_SUCCESS) goto done;
	um_vctovc (evout2.cp, pty);

	for (i = 0; i < npat; i++)
	{
		u = sp[i+1];
		status=uc_evcrv (UM_POINT, u, cv1, tfmat1, &evout1);
		if (status != UU_SUCCESS) goto done;
		um_vctovc (evout1.cp, ptw);
		if (itim == 0)
			status = uc_evcrv (UM_POINT, u, cv2, tfmat2, &evout2);
		else	  
			status = uc_evcrv (UM_POINT, 1.-u, cv2, tfmat2, &evout2);
		if (status != UU_SUCCESS) goto done;
		um_vctovc (evout2.cp, ptz);

		status = chk_4pts(ptx,pty,ptw,ptz, &itwist);
		if (status != UU_SUCCESS) goto done;
		itwis[itim] += itwist;

		um_vctovc(ptw,ptx); um_vctovc(ptz,pty);
	}
	if (itim == 0 && itwis[0] <= npat)
	{
		itim = 1;
		goto repeat;
	}
	*ktwist = (itwis[0] > itwis[1])? 1: 0;

done:;
   uu_list_free (&slist);
   uu_dexitstatus("chk_crv(cv1,cv2,ktwist)", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : chk_4pts(ptx,pty,ptw,ptz, ktwist)
**       Determines whether the quadrilateral (x,w,z,y) is twisted.  
**       (Direct translation from the routine chkpat (file rldpat.f))            
**    PARAMETERS   
**       INPUT  : 
**          x,w,z,y    - vertices of a (3-D) quadrilateral       
**       OUTPUT :  
**          ktwist  - the flag; ktwist=1 iff (x,w,z,y) is twisted.
**    RETURNS      : 
**       none 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int chk_4pts(ptx,pty,ptw,ptz, ktwist)
UM_coord ptx,pty,ptw,ptz;
int *ktwist;   
   {
   int status;
   UU_REAL etol;
   UU_REAL dxy,dxz,dxw,dwy,dyz,dwz;         
   UU_REAL cos_ab,sin_ab,cos_ab1,sin_ab1;
   UU_REAL c_a,c_b,da,db,d,c_a1,c_b1,da1,db1,d1;     
   UM_real8 tol8;
   UM_coord x_da,x_da1,w_db,w_db1;
   UM_vector a,b,a1,b1,c;

   uu_denter(UU_MTRC,(us,"chk_4pts(ptx,pty,ptw,ptz, ktwist)"));

     status = UU_SUCCESS;

     gettol (&tol8);
     etol = tol8;
     *ktwist=0;

     dxw=um_dcccc(ptx,ptw);
     if (dxw<etol) goto done;
     dyz=um_dcccc(pty,ptz);
     if (dyz<etol) goto done;
     dxy=um_dcccc(ptx,pty);
     if (dxy<etol) goto done;
     dwz=um_dcccc(ptw,ptz);
     if (dwz<etol) goto done;

     dxz=um_dcccc(ptx,ptz);
     dwy=um_dcccc(ptw,pty);
/*
..... Calculate d - the distance between segments [x,y] and [w,z]
*/
     um_vcmnvc(pty, ptx, a);
     um_unitvc(a,a);
     um_vcmnvc(ptz, ptw, b);
     um_unitvc(b,b);

     cos_ab=um_dot(a,b);
     sin_ab=sqrt(1-cos_ab*cos_ab);
     if (sin_ab <= UM_FUZZ) goto done;

     um_vcmnvc(ptw,ptx,c);
     c_a=um_dot(c,a);
     c_b=um_dot(c,b);

     da=(c_a-c_b*cos_ab)/(sin_ab*sin_ab);
     if (da <= 0.0) da=0.0;
     if (da >= dxy) da=dxy;
     um_translate_point(ptx,da,a,x_da);

     db=(c_a*cos_ab-c_b)/(sin_ab*sin_ab);
     if (db <= 0.0) db=0.0;
     if (db >= dwz) db=dwz;
     um_translate_point(ptw,db,b,w_db);

     d=um_dcccc(x_da,w_db);
     if (d > dxw || d > dyz) goto done;
/*
..... Calculate d1 - the distance between segments [x,z] and [w,y]
*/
     um_vcmnvc(ptz, ptx, a1);
     um_unitvc(a1,a1);
     um_vcmnvc(pty, ptw, b1);
     um_unitvc(b1,b1);

     cos_ab1=um_dot(a1,b1);
     sin_ab1=sqrt(1-cos_ab1*cos_ab1);

     if (sin_ab1 <= UM_FUZZ) 
         {
         *ktwist=1;
         goto done;
         }  

     c_a1=um_dot(c,a1);
     c_b1=um_dot(c,b1);

     da1=(c_a1-c_b1*cos_ab1)/(sin_ab1*sin_ab1);
     if (da1 <= 0.0) da1=0.0;
     if (da1 >= dxz) da1=dxz;
     um_translate_point(ptx,da1,a1,x_da1);

     db1=(c_a1*cos_ab1-c_b1)/(sin_ab1*sin_ab1);
     if (db1 <= 0.0) db1=0.0;
     if (db1 >= dwy) db1=dwy;
     um_translate_point(ptw,db1,b1,w_db1);

     d1=um_dcccc(x_da1,w_db1);
/*
..... If d is less than d1 we consider the patch twisted
*/
     if (d1 > d) *ktwist=1;

done:;

   uu_dexitstatus("chk_4pts(ptx,pty,ptw,ptz, ktwist)", status);
   return(0);
   }
