#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**    NAME         : vdagetdata.c
**       CONTAINS:
**             vda_get_curve_data
**             vda_get_surf_data 
**             vda_get_circle_data
**             vda_get_point_data    
**             vda_check_header  
**             vda_check_trailer 
**             vda_check_beginset 
**             vda_check_endset  
**             vda_define_group  
**             vda_process_tmat  
**             vda_assign_tmat_to_group
**             vda_assign_tmat_to_element
**             vda_process_tlist
**             vda_process_tmat_tlist_group
**             vda_get_data      
**    COPYRIGHT 1989 (c) MILLS DATA SYSTEMS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       vdagetdata.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:54
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "vda.h"

extern struct vda_ent vda_dir[];
extern int beginset_drec;  /* directory index of last beginset record */
extern int header_drec;  /* directory index of header record (should be 1) */
extern int pchar;
extern int recnum;
extern int last_irec;    /* record index of last entity */

/*********************************************************************
**    I_FUNCTION     :  vda_get_curve_data(drec, dblk, pblk) 
**       Create an iges curve dblk and pblk from vda CURVE.
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of CURVE record.   
**       OUTPUT :  
**          dblk     iges directory record
**          pblk     iges parameter record        
**    RETURNS      : 0 if OK; -1 if error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_get_curve_data(drec, dblk, pblk)
int drec;
struct dir_rec *dblk;
struct IG_igesplin_rec *pblk;
	{
	int status;
	int last, i, j, n, iord;
	long loc_ptr;
	UU_REAL x, dt;

	recnum = vda_dir[drec].recnum;
	pchar  = vda_dir[drec].pchar; 

	status = vda_read_record();
	if (status) 
		return(status);

	status = vda_get_integer(&n, 1);
	if (status) 
		return(status);

	pblk->n_seg = n;
	pblk->no_t = n+1;

	uig_get_addr(&pblk->varlistbuf[0], &loc_ptr);
	pblk->t = (UU_REAL *)loc_ptr;

	pblk->no_coef = n+1;
	uig_get_addr(&pblk->t[n+1], &loc_ptr);
	pblk->coef = (struct IG_coef_rec *)loc_ptr; 

	for (i=0;i<=n;i++)  /* get n+1 parameter values */
		{
		status = vda_get_real(&x, 1);
		if (status) 
			return(status);
		pblk->t[i] = i; /* =i, not x; VDA cv segs range 0 - 1 in u */
		}

	for (i=0;i<n;i++)  /* get n sets of polynomial coefficients */
		{
		status = vda_get_integer(&iord, 1);
		if (status) 
			return(status);

		if (iord>4)
			{
			vda_error("degree cannot exceed 3");
			return(-1);
			}

		for (j=0;j<4;j++)  /* initialize all values to zero */
			{
			pblk->coef[i].cx[j] = 0.0;
			pblk->coef[i].cy[j] = 0.0;
			pblk->coef[i].cz[j] = 0.0;
			}

		for (j=0;j<iord;j++)  /* get iord ax values */
			{
			status = vda_get_real(&x, 1);
			if (status) 
				return(status);
			pblk->coef[i].cx[j] = x;
			}

		for (j=0;j<iord;j++)  /* get iord ay values */
			{
			status = vda_get_real(&x, 1);
			if (status) 
				return(status);
			pblk->coef[i].cy[j] = x;
			}

		for (j=0;j<iord;j++)  /* get iord az values */
			{
			/* jkd57 */
			/* fix for composite curves with more than 1 segment */
			last = ((i==n-1) && (j==iord-1)); 
			status = vda_get_real(&x, last+1);
			if (status) 
				return(status);
			pblk->coef[i].cz[j] = x;
			}
		}

	/* create (dummy) set of coefficient data at end */
	for (j=0;j<4;j++)  /* initialize all values to zero */
		{
		pblk->coef[n].cx[j] = 0.0;
		pblk->coef[n].cy[j] = 0.0;
		pblk->coef[n].cz[j] = 0.0;
		}

	for (j=0;j<4;j++)  /* get the point location */
		{
		pblk->coef[n].cx[0] += pblk->coef[n-1].cx[j];
		pblk->coef[n].cy[0] += pblk->coef[n-1].cy[j];
		pblk->coef[n].cz[0] += pblk->coef[n-1].cz[j];
		}


	/* get the slope */
	dt = pblk->t[n] - pblk->t[n-1];

	pblk->coef[n].cx[1] = pblk->coef[n-1].cx[j]+(2*pblk->coef[n-1].cx[j]*dt)+(3*pblk->coef[n-1].cx[j]*dt*dt);
	pblk->coef[n].cy[1] = pblk->coef[n-1].cy[j]+(2*pblk->coef[n-1].cy[j]*dt)+(3*pblk->coef[n-1].cy[j]*dt*dt);
	pblk->coef[n].cz[1] = pblk->coef[n-1].cz[j]+(2*pblk->coef[n-1].cz[j]*dt)+(3*pblk->coef[n-1].cz[j]*dt*dt);

	vda_create_dblk(drec,dblk,GSPLINE);

	return(0);
	}


/*********************************************************************
**    I_FUNCTION     :  vda_get_surf_data(drec, dblk, pblk) 
**       Create an iges surface dblk and pblk from vda SURF.   
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of SURF record.             
**       OUTPUT :  
**          dblk     iges directory record
**          pblk     iges parameter record        
**    RETURNS      : 0 if OK; -1 if error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_get_surf_data(drec, dblk, pblk)
int drec;
struct dir_rec *dblk;
struct IG_igesplsf_rec *pblk;
	{
	int status;
	int i, j, k, n, m, iordu, iordv;
	int u, v, inx, last;
	long loc_ptr;
	UU_REAL x;
	struct IG_patch_rec
		{
		UU_REAL cx[4][4];
		UU_REAL cy[4][4];
		UU_REAL cz[4][4];
		};
	struct IG_patch_rec *patch;

	recnum = vda_dir[drec].recnum;
	pchar  = vda_dir[drec].pchar; 

	status = vda_read_record();
	if (status) 
		return(status);

	status = vda_get_integer(&m, 1);
	if (status) 
		return(status);

	status = vda_get_integer(&n, 1);
	if (status) 
		return(status);

	pblk->no_u_seg = m;
	pblk->no_tu = m+1;
	pblk->no_v_seg = n;
	pblk->no_tv = n+1;

	uig_get_addr(&pblk->varlistbuf[0], &loc_ptr);
	pblk->tu = (UU_REAL *)loc_ptr;

	uig_get_addr(&pblk->tu[m+1], &loc_ptr);
	pblk->tv = (UU_REAL *)loc_ptr;

	pblk->no_patc = n*m;
	uig_get_addr(&pblk->tu[m+1+n+1], &loc_ptr);
	pblk->patc = (UU_REAL *)loc_ptr; 

	for (i=0;i<=m;i++)  /* get m+1 s parameter values */
		{
		status = vda_get_real(&x, 1);
		if (status) 
			return(status);
		pblk->tu[i] = i; /* =i, not x; VDA pats go 0 - 1 in u,v */
		}

	for (i=0;i<=n;i++)  /* get n+1 t parameter values */
		{
		status = vda_get_real(&x, 1);
		if (status) 
			return(status);
		pblk->tv[i] = i; /* =i, not x; VDA pats go 0 - 1 in u,v */
		}

	inx = 0;
	for (v=0;v<n;v++) 
		{
		for (u=0;u<m;u++)  /* get n*m sets of polynomial coefficients */
			{
			inx = u*(n+1)+v;
			inx = inx*48;

			uig_get_addr(&pblk->patc[inx], &loc_ptr);
			patch = (struct IG_patch_rec *)loc_ptr;

			status = vda_get_integer(&iordu, 1);
			if (status) 
				return(status);

			if (iordu>4)
				{
				vda_error("degree cannot exceed 3");
				return(-1);
				}

			status = vda_get_integer(&iordv, 1);
			if (status) 
				return(status);

			if (iordv>4)
				{
				vda_error("degree cannot exceed 3");
				return(-1);
				}

			for (j=0;j<4;j++)  /* initialize all values to zero */
				{
				for (k=0;k<4;k++)
					{
					patch->cx[j][k] = 0.0;
					patch->cy[j][k] = 0.0;
					patch->cz[j][k] = 0.0;
					}
				}

			for (j=0;j<iordu;j++)  /* get iord*iordv ax values */
				{
				for (k=0;k<iordv;k++)  
					{
					status = vda_get_real(&x, 1);
					if (status) 
						return(status);
					patch->cx[j][k] = x;
					}
				}

			for (j=0;j<iordu;j++)  /* get iord*iordv ay values */
				{
				for (k=0;k<iordv;k++)  
					{
					status = vda_get_real(&x, 1);
					if (status) 
						return(status);
					patch->cy[j][k] = x;
					}
				}
			for (j=0;j<iordu;j++)  /* get iord*iordv az values */
				{
				for (k=0;k<iordv;k++)  
					{
					last = (u==m-1 && v==n-1 && j==iordu-1 && k==iordv-1); 
					status = vda_get_real(&x, last+1); 
					if (status) 
						return(status);
					patch->cz[j][k] = x;
					}
				}
			/* inx = inx+48; */
			}
		/* inx = inx+48;  /* skip dummy patch */
		}
	vda_create_dblk(drec,dblk,GSPLSURF);
	return(0);
	}


/*********************************************************************
**    I_FUNCTION     :  vda_get_circle_data(drec, dblk, pblk) 
**       Create an iges arc dblk and pblk from vda CIRCLE.
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of CIRCLE record.       
**       OUTPUT :  
**          dblk     iges directory record
**          pblk     iges parameter record        
**    RETURNS      : 0 if OK; -1 if error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_get_circle_data(drec, dblk, pblk)
   int drec;
   struct dir_rec *dblk;
   struct IG_igesarc_rec *pblk;
   {
   int status, tptr, i;    
   UU_REAL x,y,z,r,vx,vy,vz,wx,wy,wz,alpha,beta;
   UU_REAL tmat[12];
   UU_REAL tfa[4][3], tfb[4][3], tfr[4][3];
   struct IG_igestran_rec t, total;
   recnum = vda_dir[drec].recnum;
   pchar  = vda_dir[drec].pchar; 
   status = vda_read_record();
   if (status) return(status);
   status = vda_get_real(&x, 1);
   if (status) return(status);
   status = vda_get_real(&y, 1);
   if (status) return(status);
   status = vda_get_real(&z, 1);
   if (status) return(status);
   status = vda_get_real(&r, 1);
   if (status) return(status);
   status = vda_get_real(&vx, 1);
   if (status) return(status);
   status = vda_get_real(&vy, 1);
   if (status) return(status);
   status = vda_get_real(&vz, 1);
   if (status) return(status);
   status = vda_get_real(&wx, 1);
   if (status) return(status);
   status = vda_get_real(&wy, 1);
   if (status) return(status);
   status = vda_get_real(&wz, 1);
   if (status) return(status);
   status = vda_get_real(&alpha, 1);
   if (status) return(status);
   status = vda_get_real(&beta, 2);
   if (status) return(status);
   pblk->zt = 0.0;
   pblk->cpt[0] = 0.0;
   pblk->cpt[1] = 0.0;
   pblk->spt[0] = r*cos(alpha);
   pblk->spt[1] = r*sin(alpha);
   pblk->ept[0] = r*cos(beta); 
   pblk->ept[1] = r*sin(beta); 
   t.trans[0] = vx;
   t.trans[1] = wx; 
   t.trans[2] = vy*wz-vz*wy; 
   t.trans[3] = x; 
   t.trans[4] = vy; 
   t.trans[5] = wy; 
   t.trans[6] = vz*wx-vx*wz; 
   t.trans[7] = y; 
   t.trans[8] = vz; 
   t.trans[9] = wz; 
   t.trans[10] = vx*wy-vy*wx; 
   t.trans[11] = z; 
   tptr = vda_dir[drec].tran;     
   if (tptr != 0)
	 {
	 /* combine the two transfoms into a single one */
	 uig_get_trans(tptr, tmat);
     for(i=0;i<4;i++)
       {
       tfa[i][0] = t.trans[i];
       tfa[i][1] = t.trans[i+4];
       tfa[i][2] = t.trans[i+8];
       tfb[i][0] = tmat[i];
       tfb[i][1] = tmat[i+4];
       tfb[i][2] = tmat[i+8];
       }
     um_tftmtf(tfa, tfb, tfr);
     for(i=0;i<4;i++)
       {
       t.trans[i] = tfr[i][0];
       t.trans[i+4] = tfr[i][1];
       t.trans[i+8] = tfr[i][2];
       }
	 }
   uig_update_trans(drec,&t);
   vda_create_dblk(drec,dblk,GARC);
   dblk->matrix_ptr = drec;
   return(0);
   }


/*********************************************************************
**    I_FUNCTION     :  vda_get_point_data(drec, dblk, pblk) 
**       Create an iges point dblk and pblk from vda POINT.
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of POINT record.      
**       OUTPUT :  
**          dblk     iges directory record
**          pblk     iges parameter record        
**    RETURNS      : 0 if OK; -1 if error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_get_point_data(drec, dblk, pblk)
   int drec;
   struct dir_rec *dblk;
   struct IG_igespt_rec *pblk;
   {
   int status;
   UU_REAL x, y, z;
   recnum = vda_dir[drec].recnum;
   pchar  = vda_dir[drec].pchar; 
   status = vda_read_record();
   if (status) return(status);
   status = vda_get_real(&x, 1);
   if (status) return(status);
   status = vda_get_real(&y, 1);
   if (status) return(status);
   status = vda_get_real(&z, 2);
   if (status) return(status);
   vda_create_dblk(drec,dblk,GPOINT);
   pblk->rel_num = 0;   /* UM_POINT_REL */
   pblk->pt[0] = x;
   pblk->pt[1] = y;
   pblk->pt[2] = z;
   return(0);
   }

/*********************************************************************
**    I_FUNCTION     :  vda_check_header(drec)            
**       Process a vda HEADER record.              
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of HEADER record.
**       OUTPUT :  
**    RETURNS      : 0 if OK; -1 if error. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_check_header(drec)
   int drec;
   {
   int status;
   recnum = vda_dir[drec].recnum;
   pchar  = 0; 
   status = vda_read_record();
   if (status) return(status);
   if (header_drec != 0)
	  {
      vda_error("only one HEADER permitted\n");
	  return(-1);
	  }
   header_drec = drec;
   beginset_drec = 0;
   }

/*********************************************************************
**    I_FUNCTION     :  vda_check_trailer(drec)           
**       Process a vda END record.              
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of END record.
**       OUTPUT :  
**    RETURNS      : 0 if OK; -1 if error. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_check_trailer(drec)
   int drec;
   {
   int status;
   recnum = vda_dir[drec].recnum;
   pchar  = 0; 
   status = vda_read_record();
   if (status) return(status);
   if (beginset_drec != 0)
	  {
      vda_error("previous data set not ended\n");
	  }
   if (header_drec == 0)
	  {
      vda_error("no matching HEADER\n");
	  return(-1);
	  }
   if (strcmp(vda_dir[header_drec].name, vda_dir[drec].name))
	  {
      vda_error("name does not match HEADER\n");
	  return(-1);
	  }
   header_drec = 0;
   }

/*********************************************************************
**    I_FUNCTION     :  vda_check_beginset(drec)          
**       Process a vda HEADER record.              
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of BEGINSET record.
**       OUTPUT :  
**    RETURNS      : 0 if OK; -1 if error. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_check_beginset(drec)
   int drec;
   {
   int status;
   recnum = vda_dir[drec].recnum;
   pchar  = 0; 
   status = vda_read_record();
   if (status) return(status);
   if (beginset_drec != 0)
	  {
      vda_error("previous data set not ended\n");
	  return(-1);
	  }
   beginset_drec = drec;
   }

/*********************************************************************
**    I_FUNCTION     :  vda_check_endset(drec)            
**       Process a vda ENDSET record.              
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of ENDSET record.
**       OUTPUT :  
**    RETURNS      : 0 if OK; -1 if error. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_check_endset(drec)
   int drec;
   {
   int status;
   recnum = vda_dir[drec].recnum;
   pchar  = 0; 
   status = vda_read_record();
   if (status) return(status);
   if (beginset_drec == 0)
	  {
      vda_error("no matching BEGINSET\n");
	  return(-1);
	  }
   if (strcmp(vda_dir[beginset_drec].name, vda_dir[drec].name))
	  {
      vda_error("name does not match previous BEGINSET\n");
	  return(-1);
	  }
   beginset_drec = 0;
   }

/*********************************************************************
**    I_FUNCTION     :  vda_define_group(drec)            
**       Process a vda GROUP record.              
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of GROUP record.
**       OUTPUT :  
**    RETURNS      : 0 if OK; -1 if error. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_define_group(drec)
   int drec;
   {
   int status;
   int elements, element_drec;
   int i, last;
   char element_name[9];
   recnum = vda_dir[drec].recnum;
   pchar  = vda_dir[drec].pchar;
   status = vda_read_record();
   if (status) return(status);
   status = vda_get_integer(&elements, 1);
   if (status) return(status);
   for (i=0;i<elements;i++)
	  {
	  last = (i==elements-1);
      status = vda_get_name(element_name, last+1);
      if (status) return(status);
	  status = vda_find_name(element_name, &element_drec);
	  if (status) return(status);
	  if (vda_dir[element_drec].group != 0)
		 {
         sprintf(p_buff, "element %s already belongs to group %s\n", 
		   vda_dir[element_drec].name, 
	       vda_dir[vda_dir[element_drec].group].name);
         vda_error(p_buff); 
		 }
	  switch (vda_dir[element_drec].type)
		 {
		 case vda_point_type:
		 case vda_pset_type:
		 case vda_mdi_type:
		 case vda_circle_type:
		 case_vda_curve_type:
		 case vda_surf_type:
		 case vda_cons_type:
		 case vda_face_type:
		 case vda_top_type:
		 case vda_group_type:
	        vda_dir[element_drec].group = drec;
			break;  
         default:
		    vda_error("not permitted in a GROUP\n");
         }
	  }
   }

/*********************************************************************
**    I_FUNCTION     :  vda_process_tmat(drec,dblk,pblk)  
**       Create an iges tran dblk and pblk from vda TMAT. 
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of TMAT record.
**       OUTPUT :  
**          dblk     iges directory record
**          pblk     iges parameter record        
**    RETURNS      : 0 if OK; -1 if error. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_process_tmat(drec, dblk, pblk)
   int drec;
   struct dir_rec *dblk;
   struct IG_igestran_rec *pblk;
   {
   int last, i, j, status;
   UU_REAL x;
   recnum = vda_dir[drec].recnum;
   pchar  = vda_dir[drec].pchar; 
   status = vda_read_record();
   if (status) return(status);
   for (i=0;i<3;i++)
	 {
     status = vda_get_real(&x, 1);
     if (status) return(status);
	 pblk->trans[i] = x;
	 }
   for (i=4;i<7;i++)
	 {
     status = vda_get_real(&x, 1);
     if (status) return(status);
	 pblk->trans[i] = x;
	 }
   for (i=8;i<11;i++)
	 {
     status = vda_get_real(&x, 1);
     if (status) return(status);
	 pblk->trans[i] = x;
	 }
   for (i=0,j=3;i<3;i++,j=j+4)
	 {
	 last = (i==2);
     status = vda_get_real(&x, last+1);
     if (status) return(status);
	 pblk->trans[j] = x;
	 }
   uig_update_trans(drec, pblk);
   return(0);
   }


/*********************************************************************
**    I_FUNCTION     :  vda_assign_tmat_to_group(tmat_drec,group_drec)
**       Assign a TMAT from a TLIST to each element of a GROUP.
**    PARAMETERS   
**       INPUT  : 
**			tmat_drec  directory index of TMAT
**			group_drec directory index of GROUP
**       OUTPUT :    none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : element may already have a TMAT.
*********************************************************************/

int 
vda_assign_tmat_to_group(tmat_drec, group_drec)
   int tmat_drec, group_drec;
   {
   int i;
   for (i=1;i<=last_irec;i++)
	 if (vda_dir[i].group == group_drec)
       vda_assign_tmat_to_element(tmat_drec, i);
   }


/*********************************************************************
**    I_FUNCTION     :  vda_assign_tmat_to_element(tmat_drec,elm_drec)
**       Assign a TMAT from a TLIST to an element.
**    PARAMETERS   
**       INPUT  : 
**			tmat_drec  directory index of TMAT
**			elm_drec   directory index of element
**       OUTPUT :    none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : element may already have a TMAT.
*********************************************************************/

int 
vda_assign_tmat_to_element(tmat_drec, elm_drec)
  int tmat_drec, elm_drec;
  {
  switch (vda_dir[elm_drec].type)
	 {
	 case vda_point_type:
	 case vda_pset_type:
	 case vda_mdi_type:
	 case vda_circle_type:
	 case_vda_curve_type:
	 case vda_surf_type:
	 case vda_cons_type:
	 case vda_face_type:
	 case vda_top_type:
        if (vda_dir[elm_drec].tran != 0)
		  {
          sprintf(p_buff, "element %s already assigned TMAT %s\n", 
		    vda_dir[elm_drec].name, 
	        vda_dir[vda_dir[elm_drec].tran].name);
          vda_error(p_buff); 
		  }
        vda_dir[elm_drec].tran = tmat_drec;
		break;
	 case vda_group_type:
		/* assign TMAT to each element in group */
		vda_assign_tmat_to_group(tmat_drec, elm_drec);
		break;
     default:
	    vda_error("not permitted in a TLIST\n");
        break;      
     }
  }


/*********************************************************************
**    I_FUNCTION     :  vda_process_tlist(drec)           
**       Process a vda TLIST record.              
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of TLIST record.
**       OUTPUT :  
**    RETURNS      : 0 if OK; -1 if error. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_process_tlist(drec)
   int drec;
   {
   int status;
   int elements, tmat_drec, element_drec;
   int i, last;
   char tmat_name[9], element_name[9];
   recnum = vda_dir[drec].recnum;
   pchar  = vda_dir[drec].pchar;
   status = vda_read_record();
   if (status) return(status);
   status = vda_get_name(tmat_name, 1);
   if (status) return(status);
   status = vda_find_name(tmat_name, &tmat_drec);
   if (status) return(status);
   status = vda_get_integer(&elements, 1);
   if (status) return(status);
   for (i=0;i<elements;i++)
	  {
	  last = (i==elements-1);
      status = vda_get_name(element_name, last+1);
      if (status) return(status);
	  status = vda_find_name(element_name, &element_drec);
	  if (status) return(status);
      vda_assign_tmat_to_element(tmat_drec, element_drec);
	  }
   }

/*********************************************************************
**    I_FUNCTION     :  vda_process_tmat_tlist_group(...)
**       Process these element types, ignoring others.     
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of element.
**       OUTPUT :  
**          dblk     iges directory record
**          pblk     iges parameter record        
**			vda_type type of element.
**    RETURNS      : 0 if processed OK; -1 if error; -3 if ignored.     
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_process_tmat_tlist_group(drec, dblk, pblk, vda_type)
   int drec;
   struct dir_rec *dblk;
   char pblk[];
   int *vda_type;
   {
   int status;
   *vda_type = vda_dir[drec].type;
   switch (*vda_type)
	 {
     case vda_tmat_type:     
	   status = vda_process_tmat(drec, dblk, pblk);
	   return(status);
     case vda_tlist_type:     
	   vda_process_tlist(drec);
	   return(0);
     case vda_group_type:    
	   vda_define_group(drec);
	   return(0); 
     }
   return(-3);  /* ignore this entity */
   }

/*********************************************************************
**    I_FUNCTION     :  vda_get_data(drec, dblk, pblk)
**       Create an iges directory and parameter from vda record.
**    PARAMETERS   
**       INPUT  : 
**			drec     directory index of vda record.
**       OUTPUT :  
**          dblk     iges directory record
**          pblk     iges parameter record        
**			vda_type type of element.
**    RETURNS      :  0 if OK; continue processing iges dblk and pblk;   
**                   -1 if error, or this element not supported;
**                   -2 if OK, but no further processing to be done;
**                   -3 if ignored (already processed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
vda_get_data(drec, dblk, pblk, vda_type)
   int drec;
   struct dir_rec *dblk;
   char pblk[];
   int *vda_type;
   {
   int status;
   *vda_type = vda_dir[drec].type;
   switch (*vda_type)
	 {
     case vda_header_type:   
	   vda_check_header(drec);
	   return(-2);
     case vda_end_type:       
	   vda_check_trailer(drec);
	   return(-2);
     case vda_beginset_type: 
	   vda_check_beginset(drec);
	   return(-2);
     case vda_endset_type:   
	   vda_check_endset(drec);
	   return(-2);
     case vda_group_type:    
	   return(-3);  /* already processed */
     case vda_tmat_type:     
	   return(-3);  /* already processed */    
     case vda_tlist_type:     
	   return(-3);  /* already processed */
     case vda_point_type:    
	   status = vda_get_point_data(drec, dblk, pblk);
	   return(status);
     case vda_pset_type:     
	   return(-1);
     case vda_mdi_type:      
	   return(-1);
     case vda_circle_type:   
	   status = vda_get_circle_data(drec, dblk, pblk);
	   return(status);
     case vda_curve_type:     
	   status = vda_get_curve_data(drec, dblk, pblk);
	   return(status);
     case vda_surf_type:      
	   status = vda_get_surf_data(drec, dblk, pblk);
	   return(status);
     case vda_cons_type:      
	   return(-1);
     case vda_face_type:     
	   return(-1);
     case vda_top_type:       
	   return(-1);
     }         
   }
#endif
