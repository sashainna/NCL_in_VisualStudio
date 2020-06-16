/*********************************************************************
**    NAME         :  tiomapd2.c
**    CONTAINS:
**			uio_radim
**    	 uio_angdim
**    	uio_anglead
**    	uio_anglds
**    	uio_centerline
**    	uio_crosshatch
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tiomapd2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:53
*********************************************************************/

#include		"usysdef.h"
#include "umath.h"
#include		"udebug.h"
#include		"tiges.h"
#include		"tioconv.h"
#include		"tigdefs.h"
#include		"mdcoord.h"
#include		"adrf.h"
#include		"adraft.h"
#include		"rbase.h"
#include		"mattr.h"
#include		"mdattr.h"

char	*uu_toolmalloc();
static struct UR_attr 	xhatch_attr;
extern int UIO_sub_swt;

/*********************************************************************
**    I_FUNCTION     :  uio_radim(fd1,fd2,es,attr,dcount,pcount)
**				Map the Unibase dadius dimension block to the iges structure. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_radim(fd1,fd2,es,attr,dcount,pcount)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */

	{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igesrad_rec radrec;
	UM_transf	trf, invtrf;
	int	mtrid, matrid;
	int	status, arrows;
	UU_REAL	t[12];
	UU_REAL	v1[3], v2[3];
	int  i, k;
	UU_LOGICAL	blanked;

	uu_denter(UU_MTRC,(us,"enter	uio_radim"));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GRADIM;
	dblk.use_flg = 1;			/* an annotation entity */
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"GRADIM");
   dblk.subsno = 0;       /*720vp*/

	radrec.key = GRADIM;
	uio_set_trf(es,trf);
	status = um_inverttf(trf,invtrf);
	if ((mtrid=uio_chk_matrix(trf,*dcount))<0)
  	  {
		uio_mtrtomtr(trf,t);	/* put two dimension matric to iges matrix */
		matrid = *dcount;
				/* set up the transformation matrix in the directory and parameter
						section */
		uio_tran(fd1,fd2,t,dcount,pcount);
  	  }
	else		/* same matrix exist before, put the pointer to it */
		{
		matrid = mtrid;
		}
	dblk.matrix_ptr = matrid;
	
			/* get text block */
	radrec.note_ptr = *dcount;
	uio_note(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf);
			/* get leader lines */
	arrows = 0;
	radrec.l_ptr = *dcount;
	uio_lead(fd1,fd2,es,attr,dcount,pcount,arrows,matrid,invtrf);

	UIO_CC_INTTOEXT(es->dim_origin,v1);		/* change location unit  */
	um_cctmtf(v1,invtrf,v2);						
	radrec.xyt[0] = v2[0];
	radrec.xyt[1] = v2[1];

	dblk.par_ptr = *pcount;
	radrec.no_bptr = radrec.no_prop = 0;

	uio_put_para(GRADIM,&radrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_dexit;
	}	

/*********************************************************************
**    I_FUNCTION     :  uio_angdim(fd1,fd2,es,attr,dcount,pcount)
**				Map the Unibase angular dimension block to the iges structure. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_angdim(fd1,fd2,es,attr,dcount,pcount)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	
	{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igesangd_rec angdrec;
	UM_transf	trf, invtrf;
	int	mtrid, matrid;
	int	status, arrows;
	UU_REAL	t[12];
	int  i, k;
	UU_LOGICAL	blanked;

	uu_denter(UU_MTRC,(us,"enter	uio_angdim"));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GANGDIM;
	dblk.use_flg = 1;				/* an annotation entity */
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"GANGDIM");
	dblk.subsno = 0;       /*720vp*/

	angdrec.key = GANGDIM;
	uio_set_trf(es,trf);
	status = um_inverttf(trf,invtrf);
	if ((mtrid=uio_chk_matrix(trf,*dcount))<0)
  	  {
		uio_mtrtomtr(trf,t);	/* put two dimension matric to iges matrix */
		matrid = *dcount;
				/* set up the transformation matrix in the directory and parameter
						section */
		uio_tran(fd1,fd2,t,dcount,pcount);
  	  }
	else		/* same matrix exist before, put the pointer to it */
		{
		matrid = mtrid;
		}
	dblk.matrix_ptr = matrid;
	
			/* get text block */
	angdrec.note_ptr = *dcount;
	uio_note(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf);

			/* get the extended lines */
	angdrec.w1_ptr = angdrec.w2_ptr = 0;
	if (es->line_blk_use > 0)			/* there is at lease a witness line */
	  {
		for (i=0; i<es->line_blk_use; i++)
		  if (es->line_blk[i].subtype==ext_line)
				break;
		if (i < es->line_blk_use)		/* found the witness lines' block */
		  {
			angdrec.w1_ptr = *dcount;		/* the first witness line */
			k = 0;
		   uio_poly2(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf,i,k);
			if (es->line_blk[i].num_pts > 2)		/* two witness line */
		  	  {
				k = 2;
				angdrec.w2_ptr = *dcount;		/* the sencond witness line */
				uio_poly2(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf,i,k);
			  }
		  }
	  }
			/* get leader lines */
	uio_anglead(fd1,fd2,es,attr,dcount,pcount,&angdrec,matrid,invtrf);

	dblk.par_ptr = *pcount;
	angdrec.no_bptr = angdrec.no_prop = 0;

	uio_put_para(GANGDIM,&angdrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION :  uio_anglead(fd1,fd2,es,attr,dcount,pcount,angdrec,
**						  matrid,invtrf)
**     Create the angular dimension leaders 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_anglead(fd1,fd2,es,attr,dcount,pcount,angdrec,matrid,invtrf)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	struct  IG_igesangd_rec *angdrec;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	int	matrid;
	UM_transf	invtrf;
	
	{
	int	i, j, k;
	int	linind, arset;
	struct UA_arc_blk	 *arcptr;
	struct UA_line_blk *linptr;
	UU_REAL	vc[3], v1[3], v2[3], arcloc[4][3];
	UU_REAL	vx[3], vy[3];
	UU_REAL	ang;
	int	arcpts[4];
	int	arrow1, arrow2;
        static int cont=0;
	struct {int	arc, aro;}	aropts[4];		/* arrow head point information    */

	uu_denter(UU_MTRC,(us,"enter uio_anglead"));
        cont++;

	arcptr = &es->arc_blk[0];		/* ? */
	UIO_CC_INTTOEXT(arcptr->center_pt,v1);		/* get the arc center point */
	um_cctmtf(v1,invtrf,v2);						
	angdrec->xyt[0] = v2[0];
	angdrec->xyt[1] = v2[1];
	UIO_LEN_INTTOEXT(arcptr->radius,angdrec->rad);	/* get the arc radius */
				/* Calculate the location of the arc leader end points */
	for (i=0; i<arcptr->num_pts; i++)
	  {
		ang = arcptr->angles[i];
		um_vctmsc(es->cpln.xaxis,cos(ang),vx);		/* x axis vector */
		um_vctmsc(es->cpln.yaxis,sin(ang),vy);		/* y axis vector */
		um_vcplvc(vx,vy,vc);								/* arc point vector */
		um_vctmsc(vc,arcptr->radius,v1);
		um_vcplvc(arcptr->center_pt,v1,arcloc[i]);
	  }

	linind = -1;
	for (i=0; i<es->line_blk_use; i++)
	  if (es->line_blk[i].subtype==dim_line)
		 {
		  linind = i;		/* there is extended lines for the leader */
		  break;
	    }
							/* find the arrow head points on the arc leader */
	for (k=0,i=0; i<es->arrow_blk_use; i++)
	  for (j=0; j<arcptr->num_pts; j++)
		 if (um_cceqcc(es->arrow_blk[i].location,arcloc[j]))
			{
			 aropts[k].arc = j;
			 aropts[k++].aro = i;
			 break;
			}
	if (k < es->arrow_blk_use)
	  {
		aropts[k].arc = (aropts[0].arc==0)? 1 : 0;
		aropts[k].aro = (aropts[0].aro==0)? 1 : 0;
	  }

   arset = 0;
	if (arcptr->num_pts == 4)			/* has two pieces of arc leader */
	  {
					/* arrow head at the both end */
		if (((aropts[0].arc==0)||(aropts[0].arc==3))&&
			 ((aropts[1].arc==0)||(aropts[1].arc==3)))
		  {					/* the second pair should be reversed */
			arcpts[0] = 0; 	arcpts[1] = 1;
			arcpts[2] = 3;		arcpts[3] = 2;
			arrow1 = (aropts[0].arc==0)? aropts[0].aro : aropts[1].aro;
			arrow2 = (aropts[0].arc==3)? aropts[0].aro : aropts[1].aro;
         arset = 1;
		  }
					/* arrow head in the middle */
		if (((aropts[0].arc==1)||(aropts[0].arc==2))&&
			 ((aropts[1].arc==1)||(aropts[1].arc==2)))
		  {
			arcpts[0] = 2;	arcpts[1] = 3;		/* the second pair should be first*/
			arcpts[2] = 1;	arcpts[3] = 0;    /* the reverse of the first pair 
															should be the second pair */
			arrow1 = (aropts[0].arc==2)? aropts[0].aro : aropts[1].aro;
			arrow2 = (aropts[0].arc==1)? aropts[0].aro : aropts[1].aro;
         arset = 1;
		  }
	  }
/* 18.12.92vp fix undefined arcpts by removing following 'else' 
	else	*/		/* has only one arc, arrow head should be at both end */
   if (arset == 0)
	  {
		arcpts[0] = 0;		arcpts[1] = 1;
		arcpts[2] = 1;		arcpts[3] = 0;
		arrow1 = (aropts[0].arc==0)? aropts[0].aro : aropts[1].aro;
		arrow2 = (aropts[0].arc==1)? aropts[0].aro : aropts[1].aro;
	  }

	angdrec->l1_ptr = *dcount;		/* process the first arc */
	uio_anglds(fd1,fd2,es,attr,dcount,pcount,arrow1,matrid,invtrf,linind,
					  arcloc[arcpts[1]]);

	angdrec->l2_ptr = *dcount;		/* process the second arc */
	uio_anglds(fd1,fd2,es,attr,dcount,pcount,arrow2,matrid,invtrf,linind,
					  arcloc[arcpts[3]]);
	uu_dexit;

	}



/*********************************************************************
**    I_FUNCTION :  uio_anglds(fd1,fd2,es,attr,dcount,pcount,arrows,
**										 matrid,invtrf,linind,arcloc)
**       Given the arrow's number, arc end points, creates an arc leader
**			entity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_anglds(fd1,fd2,es,attr,dcount,pcount,arrows,matrid,invtrf,linind,arcloc)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	int	arrows;
	int	matrid;
	UM_transf	invtrf;
	int	linind;
	UU_REAL	arcloc[3];
	
	{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igeslead_rec	 leadrec;
	struct  UA_arrow_blk  *arptr;
	struct  UA_line_blk 	 *linptr;
	UU_REAL	*ptptr;
	UU_REAL	origin[3], v1[3], v2[3];
	int	i, j, k, num, pre;
	int	num_seg, extind;
	UU_LOGICAL	got;
	int	try;
	UU_LOGICAL	blanked;

	uu_denter(UU_MTRC,(us,"enter uio_angld1,arrow=%d",arrows));
	arptr = &es->arrow_blk[arrows];

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GLEADER;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = 1;			/* physically dependent */
	dblk.use_flg = 1;			/* an annotation entity */
	dblk.matrix_ptr = matrid;
	uio_arrow(arptr->arrow_type,&dblk.form_no);
	strcpy(dblk.label,"GLEADER");
	dblk.subsno = 0;       /*720vp*/

	leadrec.key = GLEADER;
	UIO_CC_INTTOEXT(arptr->location,origin);		/* change location unit  */
	um_cctmtf(origin,invtrf,v1);						/* arrow head location */
	for (i=0; i<2; i++)		leadrec.xyh[i] = v1[i];
	leadrec.zt = v1[2];
	UIO_LEN_INTTOEXT(arptr->size,leadrec.ah);
	leadrec.aw = leadrec.ah / 3.0;
	num = 1;
	if (linind > -1)		/* there are possibly some extended lines to the arc 
									leader, allocte more memory  */
	  {
		linptr = &es->line_blk[linind];
		num = linptr->num_pts/2 + 2;
	  }
	ptptr = leadrec.pt2 = (UU_REAL *) uu_toolmalloc(2*num*sizeof(UU_REAL));
	UIO_CC_INTTOEXT(arcloc,v1);		/* change location unit  */
	um_cctmtf(v1,invtrf,v2);			/* arc tail location */		
	*ptptr++ = v2[0];
	*ptptr++ = v2[1];
	num_seg = 1;
				/* if there is extended line, check whether it is connected to this
					arc. if it is, put the leader record */
	if (linind > 0)
	  {
		extind = -1;
		for (i=0; i<linptr->num_pts; i++)
	  	if (um_cceqcc(arcloc,linptr->line_seg[i]))
		  {
		  	extind = i;
		  	break;
		  }
		if (extind > -1)			/* there is extended line connected to this arc */
	  	  {
			*ptptr++ = v2[0];		/* second arc end point */
			*ptptr++ = v2[1];
			num_seg++;
			pre = extind;
			j = ((extind%2)==0)? (extind+1) : (extind-1);	/* tail point */
			num = linptr->num_pts/2;
			for (i=0; i<num; i++)
		  	  {
				UIO_CC_INTTOEXT(linptr->line_seg[j],v1);
				um_cctmtf(v1,invtrf,v2);
				*ptptr++ = v2[0];
				*ptptr++ = v2[1];
				num_seg++;
						/* search for the continuous line's endpoint */
				got = UU_FALSE;
				for (k=0; k<linptr->num_pts; k++)
			  	  {
					if ((k==pre)||(k==j))
						continue;
					else
				  	  {
						if (um_cceqcc(linptr->line_seg[k],linptr->line_seg[j]))
					  	  {
							pre = j;
							j = ((k%2)==0)? (k+1) : (k-1);
							got = UU_TRUE;
							break;
					  	  }
				  	  }
			   	}
		   	if (!got)		break;			/* no more connected lines */
		  	  }
	  	  }
	  }
	leadrec.no_pt2 = leadrec.num_seg = num_seg;
	leadrec.no_bptr = leadrec.no_prop = 0;

	uio_put_para(GLEADER,&leadrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(leadrec.pt2);
	uu_dexit;
	}	

/*********************************************************************
**    I_FUNCTION     :  uio_centerline(fd1,fd2,es,attr,dcount,pcount)
**				Map the Unibase centerline dimension to the iges structure. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_centerline(fd1,fd2,es,attr,dcount,pcount)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	{

	struct  IG_poly2d_rec	polyrec;
	UU_REAL	*ptptr;
	UU_REAL	v1[3], v2[3];
	struct  dir_rec dblk;		/* directory record */
	UM_transf	trf, invtrf;
	int	mtrid, matrid;
	int	status;
	UU_REAL	t[12];
	int  i, k, num_pts;
	UU_LOGICAL	blanked;

	uu_denter(UU_MTRC,(us,"enter	uio_centerline dcnt %d pcnt %d", *dcount,
	*pcount));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GPOLY;
	dblk.sub_swt = UIO_sub_swt;
	dblk.use_flg = 1;				/* an annotation entity */
	dblk.form_no = 20;
	if(es->arc_blk_use > 0) dblk.form_no = 21;
	strcpy(dblk.label,"CNTLINE");
	dblk.subsno = 0;       /*720vp*/

	uio_set_trf(es,trf);
	status = um_inverttf(trf,invtrf);

	/* check transformation */

	if ((mtrid=uio_chk_matrix(trf,*dcount))<0)
     {
		uio_mtrtomtr(trf,t);
		matrid = *dcount;
		uio_tran(fd1,fd2,t,dcount,pcount);
  	  }
	else	
		{
		matrid = mtrid;
		}
	dblk.matrix_ptr = matrid;

	/* count number of points */
	num_pts = 0;
	for(i=0; i<es->line_blk_use; i++)
		{
		num_pts = num_pts + (es->line_blk[i].num_pts);
		}

	polyrec.key = GPOLY;
	polyrec.type = 1;
	polyrec.no_pt2 = polyrec.num = num_pts;
	ptptr = polyrec.pt2 = (UU_REAL *) uu_toolmalloc(2*polyrec.num*
																	sizeof(UU_REAL));

	/* fill point array */

	for(k=0; k<es->line_blk_use; k++)
		{
		for(i=0;i<es->line_blk[k].num_pts;i++, i++)
			{
			UIO_CC_INTTOEXT(es->line_blk[k].line_seg[i],v1);	
			um_cctmtf(v1,invtrf,v2);				
			if(k==0 && i==0)	polyrec.zt = v2[2];
			*ptptr++ = v2[0];		
			*ptptr++ = v2[1];
			UIO_CC_INTTOEXT(es->line_blk[k].line_seg[i+1],v1);	
			um_cctmtf(v1,invtrf,v2);						
			*ptptr++ = v2[0];		
			*ptptr++ = v2[1];
			}
		}

	polyrec.no_bptr = polyrec.no_prop = 0;
	dblk.par_ptr = *pcount;

	uio_put_para(GPOLY,&polyrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	if (polyrec.pt2 != 0) uu_toolfree(polyrec.pt2);
	uu_dexit;
	}	

/*********************************************************************
**    I_FUNCTION     :  uio_crosshatch(fd1,fd2,es,attr,dcount,pcount)
**				Map the Unibase crosshatch dimension to the iges structure. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_crosshatch(fd1,fd2,es,attr,dcount,pcount)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	{

	struct  IG_poly2d_rec	polyrec;
	struct UA_hatchlin_rec  hatchrec;
	UU_REAL	*ptptr,*p;
	UU_REAL	v[3], v1[3], v2[3];
	struct  dir_rec dblk;		/* directory record */
	UM_transf	trf, invtrf;
	int	mtrid, matrid;
	int	status;
	UU_REAL	t[12];
	int  i, k, num_pts;
	UU_LOGICAL	blanked;

	uu_denter(UU_MTRC,(us,"enter	uio_crosshatch dcnt %d pcnt %d", *dcount,
	*pcount));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GPOLY;
	dblk.use_flg = 1;				/* an annotation entity */
	dblk.sub_swt = UIO_sub_swt;
	dblk.form_no = 31;
	strcpy(dblk.label,"XHATCH");
	dblk.subsno = 0;       /*720vp*/

	uio_set_trf(es,trf);
	status = um_inverttf(trf,invtrf);

	/* check transformation */

	if ((mtrid=uio_chk_matrix(trf,*dcount))<0)
     {
		uio_mtrtomtr(trf,t);
		matrid = *dcount;
		uio_tran(fd1,fd2,t,dcount,pcount);
  	  }
	else	
		{
		matrid = mtrid;
		}
	dblk.matrix_ptr = matrid;

	/* get cross hatch line record */
	hatchrec.key = es->asso_blk[es->asso_blk_use - 1].key;
   if((status = uio_get_xhatch(&hatchrec, &xhatch_attr)) != UU_SUCCESS)
		{
		return;
		}

	num_pts = (hatchrec.no_wt/3);

	polyrec.key = GPOLY;
	polyrec.type = 1;
	polyrec.no_pt2 = polyrec.num = num_pts;
	ptptr = polyrec.pt2 = (UU_REAL *) uu_toolmalloc((2*polyrec.num + 10)*
																	sizeof(UU_REAL));
	/* fill point array */

	p = hatchrec.wt;
	for(i=0; i < hatchrec.no_wt; i+=6)
		{
		v[0] = p[i];
		v[1] = p[i+1];
		v[2] = p[i+2];
		UIO_CC_INTTOEXT(v,v1);	
		um_cctmtf(v1,invtrf,v2);				
		if(i == 0 )	polyrec.zt = v2[2];
		*ptptr++ = v2[0];		
		*ptptr++ = v2[1];
		v[0] = p[i+3];
		v[1] = p[i+4];
		v[2] = p[i+5];
		UIO_CC_INTTOEXT(v,v1);	
		um_cctmtf(v1,invtrf,v2);				
		*ptptr++ = v2[0];		
		*ptptr++ = v2[1];
		}

	polyrec.no_bptr = polyrec.no_prop = 0;
	dblk.par_ptr = *pcount;

	uio_put_para(GPOLY,&polyrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(polyrec.pt2);
	uu_dexit;
	}	
/*********************************************************************
**    I_FUNCTION     :  uio_get_xhatch(e, attr)
**				Get cross-hatch entity
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uio_get_xhatch(e, attr)
struct UA_hatchlin_rec *e;
struct UR_attr 	*attr;
{
	int status;

	uu_denter(UU_MTRC,(us,"entering uio_get_xhatch(%x)",
		e));

	/* get fixed part of hatchlin_rec out of Unibase */
	if ((status = ur_retrieve_data_fixed(e)) != UU_SUCCESS)
		{
		uu_denter2(UU_STRC,(us,"uio_get_xhatch: retrieve_data_fixed ERROR"));
		uu_dexit;
		goto fexit;
		}

	/* set up for coordinate array */
	e->wt = (UU_REAL*) uu_toolmalloc(e->no_wt * sizeof(UU_REAL));

	/* fill it up */
	if (ur_retrieve_data_varlist(e->key, 1, e->wt, 1, e->no_wt) == UU_FAILURE)
		{
		status = UU_FAILURE;
		uu_denter2(UU_STRC,(us,"uio_get_xhatch: retrieve_data_varlist ERROR"));
		uu_dexit;
		goto fexit;
		}

	attr->key_id = e->key;
	ur_retrieve_attr(attr);
fexit:;
	uu_dexit;
	return (status);
}
