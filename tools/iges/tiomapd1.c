
/*********************************************************************
**    NAME         :  tiomapd1.c
**       CONTAINS:
**    		uio_gnote
**    		uio_nextstr
**    		uio_lindim
**    		uio_note
**    		uio_chkfont
**    		uio_lead
**    		uio_arrow
**    		uio_poly2
**          uio_diadim(fd1,fd2,es,attr,dcount,pcount)
**          uio_glabel(fd1,fd2,es,attr,dcount,pcount)
**          uio_gsymbol(fd1,fd2,es,attr,dcount,pcount)
**          uio_gsymline(fd1,fd2,es,attr,dcount,pcount,spt,ept)
**          uio_gsymarc(fd1,fd2,es,attr,dcount,pcount,arc)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tiomapd1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:53
*********************************************************************/

#include		"usysdef.h"
#include		"udebug.h"
#include "umath.h"
#include		"tiges.h"
#include		"tioconv.h"
#include		"tigdefs.h"
#include		"mdcoord.h"
#include		"adrf.h"
#include		"adraft.h"


char *uu_toolmalloc();
extern int UIO_sub_swt;

/*********************************************************************
**    I_FUNCTION     :  uio_gnote(dblk,t,gnote,key)
**				Map a Unibase general note to a IGES note. Only the first
**			 text block is processed.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_gnote(fd1,fd2,es,attr,dcount,pcount)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	 
	{
	struct IG_igesnote_rec	noterec;
	struct IG_gnote_rec *p;
	struct UA_txt_blk	*tx;
	UM_transf	trf, invtrf;
	int	status, mtrid;
	UU_REAL	t[12], v1[3];
	UU_REAL	wid, hgt, ang, txsize;		/* box width and height	*/
	UM_vector	vc;
	UU_REAL	origin[3];
	char	*pstr;
	struct  dir_rec dblk;		/* directory record */
	int  i, j, m, num, sum, jprev, line_cnt[10];
	register char	ch;
	register	 n;
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter	uio_gnote"));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GNOTE;
	dblk.par_ptr = *pcount;
	dblk.use_flg = 1;			/* this is an annotation */
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"GNOTE");
   dblk.subsno = 0;       /*720vp*/
	noterec.key = GNOTE;

		/* set the trasformation matrix	*/
	uio_set_trf(es,trf);
	status = um_inverttf(trf,invtrf);
	m = es->txt_blk_use;
	for (i=0,sum=0; i<m; i++)	/* find out the total num of lines of string */
	  {
		tx = &es->txt_blk[i];
		n = strlen(tx->tstring);
		if (n < tx->char_cnt) n = tx->char_cnt;
		if(n < 3)
			{
			sum++;
			line_cnt[i] = 1;
			}
		else
			{
			jprev = 0;
			num = 0;
			for (j=0; j<=n; j++)
				{
				if (((ch=tx->tstring[j])=='\n')||(ch=='\0'))
					{
					if(j != jprev + 1)
						{
						sum++;
						num++;
						}
					jprev = j;
					}
				}
			if (sum == 0) sum = 1;
			if (num == 0) num = 1;
			line_cnt[i] = num;
			}
	  }
	noterec.num_str = noterec.no_gnote = sum;
	noterec.no_bptr = 0;
	noterec.no_prop = 0;
	p = noterec.gnote = (struct IG_gnote_rec *)uu_toolmalloc(sum*sizeof(struct IG_gnote_rec));

	for (j=0; j<m; j++)
	  {
		num = line_cnt[j];
		UIO_LEN_INTTOEXT(tx->dx,wid);
		hgt = tx->dy / num;
		ang = tx->tangle + 3.1416 + 1.57;
		um_xyztovc(cos(ang),sin(ang),(UU_REAL) 0.0,vc);	/* find the text vector */
		UIO_LEN_INTTOEXT(tx->txt_size,txsize);			/* character size */
		pstr = tx->tstring;
		for(i=0;i<num;i++)
	  	  {
			p->str_cnt = uio_nextstr(&pstr,p->str);
			if (p->str_cnt == 0)
			{
				strcpy (p->str, " ");
				p->str_cnt = 1;
			}
			p->wt = wid;		/* box width */
			p->ht = txsize;		/* text size  */
			p->fc = 14;			/* font characteristic - simplex roman*/
			p->sl = tx->slant+1.57;					/* slant angle */
			p->ang = tx->tangle;					/* rotation angle */
			p->mir = 0;			/* no mirror */
			p->vh = 0;			/* rotate internal text flag */
			um_vcplvc(es->dim_origin,tx->origin,origin);
			um_vctmsc(vc,i*hgt,v1);
			um_vcplvc(origin,v1,v1);
			um_cctmtf(v1,invtrf,origin);		/* text origin */
			UIO_CC_INTTOEXT(origin,p->xyzt);		/* change origin unit  */
			p++;
	  	  }
	 }

	if ((mtrid=uio_chk_matrix(trf,*dcount))<0)	
	  {
		uio_mtrtomtr(trf,t);	/* put two dimension matric to iges matrix */
		dblk.matrix_ptr = *dcount;
				/* set up the transformation matrix in the directory and parameter
					section */
		uio_tran(fd1,fd2,t,dcount,pcount);
	  }
	else		/* same matrix exist before, put the pointer to it */
		{
		dblk.matrix_ptr = mtrid;
		}

	dblk.par_ptr = *pcount;
	uio_put_para(GNOTE,&noterec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(noterec.gnote);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION :  uio_nextstr(pstr,str)
**       Retrieve a character string up to a '\n' char from a buffer.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_nextstr(pstr,str)
	char	**pstr, *str;

	{
	int	num;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter uio_nextstr,pstr=%s", *pstr));
	num = 0;
	while (((*str = **pstr) != '\n')&&(*str != '\0'))
	  {
		num++;
		str++;
		(*pstr)++;
	  }
	*str = '\0';
	(*pstr)++;
	uu_dexit;
	return(num);
	}

/*********************************************************************
**    I_FUNCTION     :  uio_lindim(fd1,fd2,es,attr,dcount,pcount)
**				Map the Unibase linear dimension block to the iges structure. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_lindim(fd1,fd2,es,attr,dcount,pcount)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	
	{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igeslind_rec lindrec;
	UM_transf	trf, invtrf;
	int	mtrid, matrid;
	int	status, arrows;
	UU_REAL	t[12];
	int  i, k;
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/


	uu_denter(UU_MTRC,(us,"enter	uio_lindim dcnt %d pcnt %d", *dcount,
	*pcount));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GLINDIM;
	dblk.use_flg = 1;				/* an annotation entity */
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"GLINDIM");
   dblk.subsno = 0;       /*720vp*/

	lindrec.key = GLINDIM;
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
	lindrec.note_ptr = *dcount;
	uio_note(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf);
			/* get leader lines */
	arrows = 0;
	lindrec.l1_ptr = *dcount;
	uio_lead(fd1,fd2,es,attr,dcount,pcount,arrows,matrid,invtrf);
	lindrec.l2_ptr = *dcount;
	arrows++;
	uio_lead(fd1,fd2,es,attr,dcount,pcount,arrows,matrid,invtrf);
			/* get the extended lines */
	lindrec.w1_ptr = lindrec.w2_ptr = 0;
	if (es->line_blk_use > 0)			/* there is at lease a witness line */
	  {
		for (i=0; i<es->line_blk_use; i++)
		  if (es->line_blk[i].subtype==ext_line)	/* find the witness line block*/
				break;
		if (i < es->line_blk_use)		/* found the witness lines' block */
		  {
			lindrec.w1_ptr = *dcount;		/* the first witness line */
			k = 0;
		   uio_poly2(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf,i,k);
			if (es->line_blk[i].num_pts > 2)		/* two witness line */
		  	  {
				k = 2;
				lindrec.w2_ptr = *dcount;		/* the sencond witness line */
				uio_poly2(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf,i,k);
			  }
		  }
	  }

	dblk.par_ptr = *pcount;
	lindrec.no_bptr = lindrec.no_prop = 0;

	uio_put_para(GLINDIM,&lindrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     :  uio_note(dblk,t,gnote,key,matrid,invtrf)
**				Map the Unibase text block to the iges structure. This
**			routine has to consider those different type of text in the 
**			darfting.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_note(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	int	matrid;	/* transfomation matrix index */
	UM_transf	invtrf;
	
	{
	struct  dir_rec dblk;		/* directory record */
	struct IG_igesnote_rec	noterec;
	struct IG_gnote_rec *p;
	struct UA_txt_blk	*tx;
	int	blknu;
	UU_REAL	t[12], v1[3];
	UU_REAL	wid, hgt, ang, txsize;		/* box width and height	*/
	UM_vector	vc;
	UU_REAL	origin[3];
	char	*pstr;
	int  i, j, num;
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter	uio_note"));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GNOTE;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = 1;			/* physically dependent */
	dblk.use_flg = 1;			/* an annotation entity */
	strcpy(dblk.label,"GNOTE");
   dblk.subsno = 0;       /*720vp*/

	noterec.key = GNOTE;

		/* set the trasformation matrix	*/
	blknu = es->txt_blk_use;
	tx = &es->txt_blk[0];			
		/* first find out how many text block needed in the iges note record */
	for (j=0,num=0; j<blknu; j++)
	  {
/* 		if (tx->subtype == app_edit_txt)	 */
		 {
		  for (i=0; i<tx->char_cnt-1; i++)
			if (tx->tstring[i] == '\n')
				num++;
		 }
/* 	   else */
		num++;
		tx++;
	  }
   if (num == 0) goto done;
	noterec.num_str = noterec.no_gnote = num;
	noterec.no_bptr = 0;
	noterec.no_prop = 0;
	p = noterec.gnote = (struct IG_gnote_rec *)uu_toolmalloc(num*sizeof(struct IG_gnote_rec));

	for (j=0; j<blknu; j++)
	  {
		tx = &es->txt_blk[j];			
		num = 0;
/*		if (tx->subtype == app_edit_txt)	   only app_edit_txt contains more than
														one text line in tstring  */
		  {
		   for (i=0; i<tx->char_cnt-1; i++)
			 if (tx->tstring[i] == '\n')
				num++;
		  }
/* 		else	*/
		num++;

		UIO_LEN_INTTOEXT(tx->dx,wid);
		hgt = tx->dy / num;
		UIO_LEN_INTTOEXT(hgt,hgt);
		ang = tx->tangle + 3.1416 + 1.57;
		um_xyztovc(cos(ang),sin(ang),(UU_REAL) 0.0,vc);	/* find the text vector */
		UIO_LEN_INTTOEXT(tx->txt_size,txsize);
		pstr = tx->tstring;
		for(i=0;i<num;i++)
	  	  {
/* 			if (tx->subtype == app_edit_txt)	 */
				uio_nextstr(&pstr,p->str);
/* 		   else */
/* 				strcpy(p->str,pstr); */
			p->wt = wid;		/* box width */
			p->ht = txsize;		/* text height */
			p->fc = uio_chkfont(p->str);			/* font characteristic */
			p->str_cnt = strlen(p->str);
			if (p->str_cnt == 0)
			{
				strcpy (p->str, " ");
				p->str_cnt = 1;
			}
			p->sl = tx->slant;					/* slant angle */
			p->ang = tx->tangle;					/* rotation angle */
			p->mir = 0;			/* no mirror */
			p->vh = 0;			/* rotate internal text flag */
			um_vcplvc(es->dim_origin,tx->origin,origin);		/* text origin */
			um_vctmsc(vc,i*hgt,v1);
			um_vcplvc(origin,v1,v1);
			um_cctmtf(v1,invtrf,origin);		/* text origin */
			UIO_CC_INTTOEXT(origin,p->xyzt);
			p++;
	  	  }
	  }

	dblk.matrix_ptr = matrid;
	dblk.par_ptr = *pcount;
	uio_put_para(GNOTE,&noterec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(noterec.gnote);

done:;
	uu_dexit;
	return(UU_SUCCESS);
	}


/*********************************************************************
**    I_FUNCTION :  uio_chkfont(pstr)
**       Given a string, check the special characters (from ansi font)
**			and then change them to iges font representations.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_chkfont(pstr)
	char	*pstr;

	{
	register char	c;
	char	*ptr, buf[200];
	int	font, i;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter uio_chkfont, str=%s",pstr));
	font = 14;
	i = 0;
	ptr = pstr;
	while ((c = *ptr++) != '\0')
	 {
	  if (c == '\\')	
	  {
		c = *ptr++;
		switch (c)
		{
		 case	'd':				/* degree */
			buf[i++] = '$';
			font = 1002;
			break;
		 case '+':				/* plus and minus sign */
			buf[i++] = '#';
			font = 1002;
			break;
		 case 'T':				/* diameter */
			buf[i++] = 'n';
			font = 1001;
			break;
		 case 'm':				/* minute */
			buf[i++] = '"';
			font = 1002;
			break;
		 case 's':				/* second */
			buf[i++] = '\'';
			font = 1002;
			break;
		 default:
			buf[i++] = '\\';
			buf[i++] = c;
			break;
	    }
	   }
	  else
		 buf[i++] = c;
	 }
	buf[i] = '\0';
	strcpy(pstr,buf);
	uu_dexit;
	return(font);

	}


/*********************************************************************
**    I_FUNCTION :  uio_lead(fd1,fd2,es,attr,dcount,pcount,arrows,matrid,invtrf)
**       Given the arrow's number, find the corresponding line segments
**			to form a leader, then pack the information to the iges leader
**			structure.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_lead(fd1,fd2,es,attr,dcount,pcount,arrows,matrid,invtrf)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	int	arrows;
	int	matrid;
	UM_transf	invtrf;
	
	{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igeslead_rec	 leadrec;
	struct  UA_arrow_blk  *arptr;
	struct  UA_line_blk 	 *linptr;
	UU_REAL	*ptptr;
	UU_REAL	origin[3], v1[3], v2[3], vc[3];
	UU_REAL	ang;
	int	i, j, k, num, pre;
	int	num_seg;
	register int  nupts;
	UU_LOGICAL	found, got;
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter uio_lead"));
	arptr = &es->arrow_blk[arrows];

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GLEADER;
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
		/* find the corresponding dimension line block */
	found = UU_FALSE;
	for (i=0; i<es->line_blk_use; i++)
	  {
		linptr = &es->line_blk[i];
/* 		if (linptr->subtype == dim_line) */
		  {
			nupts = linptr->num_pts;
		 	for (j=0; j<linptr->num_pts; j++)
		    {
			  if (um_cceqcc(arptr->location,linptr->line_seg[j]))
				 {
				  found = UU_TRUE;
				  break;
				 }
		    }
		  }
	   if (found)	break;
	  }
	num = nupts / 2;
	ptptr = leadrec.pt2 = (UU_REAL *) uu_toolmalloc(2*num*sizeof(UU_REAL));
	num_seg = 0;
	if (!found)		/* arrow head location not found, use the same point as tail*/
	  {
		ang = arptr->aangle + 3.1416;
		um_xyztovc(cos(ang),sin(ang),0.0,vc);	/* find the text vector */
		um_vcplvc(v1,vc,v1);
		*ptptr++ = v1[0];
		*ptptr++ = v1[1];
		num_seg++;
	  }
	else 		/* find the element which store the tail point */
	  {
		pre = j;
		j = ((j%2)==0)? (j+1) : (j-1);
		for (i=0; i<num; i++)
		  {
			UIO_CC_INTTOEXT(linptr->line_seg[j],v1);
			um_cctmtf(v1,invtrf,v2);						
			*ptptr++ = v2[0];
			*ptptr++ = v2[1];
			num_seg++;
					/* search for the continuous line's endpoint */
			got = UU_FALSE;
			for (k=0; k<nupts; k++)
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
			if (!got)		break;		/* no more connected lines */
		  }
	  }
	leadrec.no_pt2 = leadrec.num_seg = num_seg;
	leadrec.no_bptr = leadrec.no_prop = 0;
	dblk.par_ptr = *pcount;

	uio_put_para(GLEADER,&leadrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(leadrec.pt2);
	uu_dexit;
	return(UU_SUCCESS);
	}	

/*********************************************************************
**    I_FUNCTION     :  uio_arrow(type,form)
**				Map UNICAD arrow-head type to iges form number.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_arrow(type,form)
	int *form;
	int type;

	{

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	switch(type)
	{
		case UA_SINGLE_OPEN:
			*form = 1;
			break;
		case UA_SINGLE_CLOSED:
			*form = 2;
			break;
		case UA_SINGLE_FILLED:
			*form = 3;
			break;
		case UA_POINT:
			*form = 4;
			break;
		case UA_NODE:
			*form = 6;
			break;
		case UA_TILDE:
			*form = 9;
			break;
		default:
			*form = 1;
			break;
	 }
	return(UU_SUCCESS);
	}	

/*********************************************************************
**    I_FUNCTION :  uio_poly2(fd1,fd2,es,attr,dcount,pcount,matrid,
**										  invtrf,ind,kk)
**       Map Unicad's extended line to iges' witness line structure
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_poly2(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf,ind,kk)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;		/* parameter section sequence number */
	int	matrid;
	UM_transf	invtrf;
	int	ind, kk;
	
	{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_poly2d_rec	polyrec;
	struct  UA_line_blk 	 *linptr;
	UU_REAL	*ptptr;
	UU_REAL	v1[3], v2[3];
	int	i;
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter uio_poly2"));
	linptr = &es->line_blk[ind];

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GPOLY;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = 1;			/* physically dependent */
	dblk.use_flg = 1;			/* an annotation entity */
	dblk.matrix_ptr = matrid;
	dblk.form_no = 40;		/* witness line */
	strcpy(dblk.label,"GPOLY");
   dblk.subsno = 0;       /*720vp*/

	polyrec.key = GPOLY;
	polyrec.type = 1;
	polyrec.no_pt2 = polyrec.num = 2 + 1;
	ptptr = polyrec.pt2 = (UU_REAL *) uu_toolmalloc(2*polyrec.num*sizeof(UU_REAL));
	UIO_CC_INTTOEXT(linptr->line_seg[kk],v1);		/* change location unit  */
	um_cctmtf(v1,invtrf,v2);						/* first point location */
	polyrec.zt = v2[2];
	for (i=0; i<2; i++)			
	  {
		*ptptr++ = v2[0];		
		*ptptr++ = v2[1];
	  }
							/* get the end point	*/
	UIO_CC_INTTOEXT(linptr->line_seg[kk+1],v1);		/* change location unit  */
	um_cctmtf(v1,invtrf,v2);						
	*ptptr++ = v2[0];		
	*ptptr++ = v2[1];

	polyrec.no_bptr = polyrec.no_prop = 0;

	uio_put_para(GPOLY,&polyrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(polyrec.pt2);
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    I_FUNCTION     :  uio_diadim(fd1,fd2,es,attr,dcount,pcount)
**				Map the Unibase diameter dimension block to the iges structure. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_diadim(fd1,fd2,es,attr,dcount,pcount)
	int	fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	
	{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_igesdiad_rec diadrec;
	UM_transf	trf, invtrf;
	int	mtrid, matrid;
	int	status, arrows;
	UU_REAL	t[12];
	UU_REAL	v1[3], v2[3];
	int  i, k;
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter	uio_diadim"));

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GDIADIM;
	dblk.use_flg = 1;			/* an annotation entity */
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"GDIADIM");
   dblk.subsno = 0;       /*720vp*/

	diadrec.key = GDIADIM;
	uio_set_trf(es,trf);
	status = um_inverttf(trf,invtrf);
	/* check matrix */
	if ((mtrid=uio_chk_matrix(trf,*dcount))<0)
  	  {
		uio_mtrtomtr(trf,t);	/* put two dimension matric to iges matrix */
		matrid = *dcount;
				/* set up the transformation matrix in the directory and parameter
						section */
		uio_tran(fd1,fd2,t,dcount,pcount);
  	  }
	else		/* same matrix exist before, put the pointer to it */
		matrid = mtrid;
	dblk.matrix_ptr = matrid;
	
			/* get text block */
	diadrec.note_ptr = *dcount;
	uio_note(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf);
			/* get leader lines */
	arrows = 0;
	diadrec.l1_ptr = *dcount;
	diadrec.l2_ptr = 0;
	uio_lead(fd1,fd2,es,attr,dcount,pcount,arrows,matrid,invtrf);
	if (es->arrow_blk_use > 1)
	  {
		arrows++;
		diadrec.l2_ptr = *dcount;
		uio_lead(fd1,fd2,es,attr,dcount,pcount,arrows,matrid,invtrf);
	  }
	UIO_CC_INTTOEXT(es->dim_origin,v1);		/* change location unit  */
	um_cctmtf(v1,invtrf,v2);						
	diadrec.xyt[0] = v2[0];
	diadrec.xyt[1] = v2[1];

	dblk.par_ptr = *pcount;
	diadrec.no_bptr = diadrec.no_prop = 0;

	uio_put_para(GDIADIM,&diadrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     :  uio_glabel(fd1,fd2,es,attr,dcount,pcount)
**         Map the Unibase label to the iges label structure. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_glabel(fd1,fd2,es,attr,dcount,pcount)
	int    fd1, fd2;
	struct UA_generic_draft *es;
	struct UR_attr    *attr;
	int    *dcount;      /* directory section sequence number */
	int   *pcount;   /* parameter section sequence number */

{
	struct  dir_rec dblk;      /* directory record */
	struct  IG_igeslabl_rec lablrec;
	UM_transf   trf, invtrf;
	int   mtrid, matrid;
	int   status, arrows;
	UU_REAL   t[12];
	int  i, k;
	UU_LOGICAL   blanked;
	int num;
	UU_KEY_ID *ent_ptr, *p1;

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GLABEL;
	dblk.use_flg = 1;            /* an annotation entity */
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"GLABEL");
	dblk.subsno = 0;

	lablrec.key = GLABEL;
	uio_set_trf(es,trf);
	status = um_inverttf(trf,invtrf);
	if ((mtrid=uio_chk_matrix(trf,*dcount))<0)
  	  {
		uio_mtrtomtr(trf,t);   /* move two dimension matrix to iges matrix */
		matrid = *dcount;
	/* set up the transformation matrix in the directory and parameter section */
		uio_tran(fd1,fd2,t,dcount,pcount);
  	  }
	else      /* same matrix exists , point the pointer to it */
		{
		matrid = mtrid;
		}
	dblk.matrix_ptr = matrid;

			/* get text block */
	lablrec.note_ptr = *dcount;
	uio_note(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf);
			/* get leader lines */
	num = es->arrow_blk_use;
	lablrec.no_cid = num+1;
	ent_ptr = 0;
	if (num > 0)
	{
		ent_ptr = p1 = (UU_KEY_ID *) uu_toolmalloc((num+1)*sizeof(UU_KEY_ID));
		*p1++ = num;
		for (arrows=0; arrows<num; arrows++,p1++)
		{
			*p1 = *dcount;
			uio_lead(fd1,fd2,es,attr,dcount,pcount,arrows,matrid,invtrf);
		}
	}
	lablrec.cid = ent_ptr;

	dblk.par_ptr = *pcount;
	lablrec.no_bptr = lablrec.no_prop = 0;

	uio_put_para(GLABEL,&lablrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	if (ent_ptr) uu_toolfree(ent_ptr);
}
/*********************************************************************
**    I_FUNCTION     :  uio_gsymbol(fd1,fd2,es,attr,dcount,pcount)
**         Map the Unibase ballloon note to the iges general symbol structure. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_gsymbol(fd1,fd2,es,attr,dcount,pcount)
int    fd1, fd2;
struct UA_generic_draft *es;
struct UR_attr    *attr;
int    *dcount;      /* directory section sequence number */
int   *pcount;   /* parameter section sequence number */

{
	struct  dir_rec dblk;      /* directory record */
	struct  IG_igesgsym_rec gsymrec;
	UM_transf   trf, invtrf;
	int   mtrid, matrid;
	int   status, arrows;
	UU_REAL   t[12];
	int  i, k;
	UU_LOGICAL   blanked;
	int num;
	UU_KEY_ID *ent_ptr, *p1, kbal[5];

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GGENSYM;
	dblk.use_flg = 1;            /* an annotation entity */
	dblk.sub_swt = UIO_sub_swt;
	strcpy(dblk.label,"GGENSYM");
	dblk.subsno = 0;
	dblk.form_no = 1;

	gsymrec.key = GGENSYM;
	uio_set_trf(es,trf);
	status = um_inverttf(trf,invtrf);
	matrid = 0;
	if (!um_is_idmat(trf))
	{
		if ((mtrid=uio_chk_matrix(trf,*dcount))<0)
  	  	{
			uio_mtrtomtr(trf,t);   /* move two dimension matrix to iges matrix */
			matrid = *dcount;
	/* set up the transformation matrix in the directory and parameter section */
			uio_tran(fd1,fd2,t,dcount,pcount);
  	  	}
		else      /* same matrix exists , point the pointer to it */
		{
			matrid = mtrid;
		}
	}
	dblk.matrix_ptr = matrid;

			/* get text block */
	gsymrec.note_ptr = *dcount;
	uio_note(fd1,fd2,es,attr,dcount,pcount,matrid,invtrf);
	num = 0;
	if (es->line_blk_use > 0 && es->line_blk[0].num_pts > 1)
	{
		kbal[++num] = *dcount;
		uio_gsymline(fd1,fd2,es,attr,dcount,pcount,es->line_blk[0].line_seg[0],
		  es->line_blk[0].line_seg[1]);
		if (es->line_blk[0].num_pts > 3)
		{
			kbal[++num] = *dcount;
			uio_gsymline(fd1,fd2,es,attr,dcount,pcount,es->line_blk[0].line_seg[2],
			  es->line_blk[0].line_seg[3]);
		}
	}
	if (es->arc_blk_use > 0)
	{
		kbal[++num] = *dcount;
		uio_gsymarc(fd1,fd2,es,attr,dcount,pcount,es->arc_blk);
		if (es->arc_blk_use > 1)
		{
			kbal[++num] = *dcount;
			uio_gsymarc(fd1,fd2,es,attr,dcount,pcount,&es->arc_blk[1]);
		}
	}
	kbal[0] = num;
	gsymrec.no_cid = num+1;
	gsymrec.cid = kbal;
			/* get leader lines */
	num = es->arrow_blk_use;
	gsymrec.no_cid1 = num;
	ent_ptr = 0;
	if (num > 0)
	{
		ent_ptr = p1 = (UU_KEY_ID *) uu_toolmalloc((num+1)*sizeof(UU_KEY_ID));
		*p1 = num;
		for (arrows=0; arrows<num; arrows++)
		{
			p1++;
			*p1 = *dcount;
			uio_lead(fd1,fd2,es,attr,dcount,pcount,arrows,matrid,invtrf);
		}
	}
	gsymrec.cid1 = ent_ptr;

	dblk.par_ptr = *pcount;
	gsymrec.no_bptr = gsymrec.no_prop = 0;

	uio_put_para(GGENSYM,&gsymrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	if (ent_ptr) uu_toolfree(ent_ptr);
}
/*********************************************************************
**    I_FUNCTION     :  uio_gsymline(fd1,fd2,es,attr,dcount,pcount)
**         Output a balloon line for a general symbol entity.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_gsymline(fd1,fd2,es,attr,dcount,pcount,spt,ept)
int    fd1, fd2;
struct UA_generic_draft *es;
struct UR_attr    *attr;
int    *dcount;      /* directory section sequence number */
int   *pcount;   /* parameter section sequence number */
UU_REAL *spt, *ept;  /* start and end points of line  */
{
	struct  dir_rec dblk;      /* directory record */
	struct  IG_igesline_rec linerec;
	UU_LOGICAL   blanked;

	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GLINE;
	dblk.use_flg = 1;            /* an annotation entity */
	dblk.sub_swt = 1;
	strcpy(dblk.label,"LINE");
	dblk.subsno = 0;
	dblk.matrix_ptr = 0;

	linerec.key = GLINE;
	UIO_CC_INTTOEXT(spt,linerec.spt);
	UIO_CC_INTTOEXT(ept,linerec.ept);

	dblk.par_ptr = *pcount;
	linerec.no_bptr = linerec.no_prop = 0;

	uio_put_para(GLINE,&linerec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	return (UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION     :  uio_gsymarc(fd1,fd2,es,attr,dcount,pcount)
**         Output a balloon arc for a general symbol entity.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_gsymarc(fd1,fd2,es,attr,dcount,pcount,arc)
int    fd1, fd2;
struct UA_generic_draft *es;
struct UR_attr    *attr;
int    *dcount;      /* directory section sequence number */
int   *pcount;   /* parameter section sequence number */
struct UA_arc_blk *arc;    /* arc of balloon */
{
	struct  dir_rec dblk;      /* directory record */
	struct  IG_igesarc_rec arcrec;
	UU_LOGICAL   blanked;
	UU_REAL *cpt;
	UM_vector vtmp, spt, ept;
	UM_vector yvec;
	int i;

	yvec[0] = 0.; yvec[1] = 1.; yvec[2] = 0.;
	uio_init_dir(attr,&dblk);
	uio_check_view(fd1, fd2, es->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(es->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GARC;
	dblk.use_flg = 1;            /* an annotation entity */
	dblk.sub_swt = 1;
	strcpy(dblk.label,"ARC");
	dblk.subsno = 0;
	dblk.matrix_ptr = 0;

	arcrec.key = GARC;
	cpt = arc->center_pt;
	um_vctmsc (yvec, arc->radius, vtmp);
	if (arc->angles[0] > arc->angles[1]) um_vctmsc (vtmp, (UU_REAL) -1.0, vtmp);
	um_vcplvc (cpt, vtmp, spt);
	um_vcmnvc (cpt, vtmp, ept);
	UIO_CC_INTTOEXT(cpt,vtmp);
	UIO_CC_INTTOEXT(spt,spt);
	UIO_CC_INTTOEXT(ept,ept);
	for (i=0; i<2; i++)
	{
		arcrec.spt[i] = spt[i];
		arcrec.ept[i] = ept[i];
		arcrec.cpt[i] = vtmp[i];
	}
	arcrec.zt = 0.0;

	dblk.par_ptr = *pcount;
	arcrec.no_bptr = arcrec.no_prop = 0;

	uio_put_para(GARC,&arcrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	return (UU_SUCCESS);
}
