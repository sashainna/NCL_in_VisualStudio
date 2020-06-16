/*********************************************************************
**    NAME         :  tiomaps.c
**    CONTAINS:
**    	uio_symbol 	
**    	uio_symbol_text 	
**			uio_set_text_trf
**			uio_connector
**			uio_map_poly3
**    	uio_map_polygon
**    	uio_out_drawing 	
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tiomaps.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:53
*********************************************************************/

#include "umath.h"
#include		"mdeval.h"
#include		"udebug.h"
#include		"tiges.h"
#include		"tigdefs.h"
#include		"rbase.h"
#include		"mfort.h"
#include		"mdrel.h" 
#include		"mdattr.h"
#include		"mattrddl.h"
#include		"mxxx.h"
#include		"mcrv.h"
#include		"mdcoord.h"
#include		"tioconv.h"
#include		"modef.h"
#include		"bsym.h"
#include		"atext.h"
#include		"nclvx.h"

extern int UIO_sub_swt;
UU_LOGICAL UIO_drawing = UU_FALSE;
char *uu_toolmalloc();

static struct UR_data 	e;
static struct UR_attr 	attr;

void uio_symbol_text(),uio_set_text_trf(),uio_map_poly3(),uio_map_polygon();
void uio_default_view(),uio_label_dir();

/*********************************************************************
**    I_FUNCTION :  uio_symbol(fd1,fd2,e,attr,dcount,pcount)
**      Output the entities for the symbol entity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_symbol(fd1,fd2,e_symbol,attr_symbol,dcount,pcount)
	int	fd1, fd2;
	struct UB_symbol_rec	*e_symbol;
	struct UR_attr 	*attr_symbol;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */

	{
	struct UB_instance_rec	e_instance;
	struct UR_attr 	attr_instance;
	struct  dir_rec dblk;				/* directory record */
	struct  IG_igessfd_rec	sfdrec;	/* iges subfigure def record */
	struct  IG_igessfi_rec	sfirec;	/* iges subfigure instance record */
	UU_KEY_ID 			key;
	UU_KEY_ID         *ent_ptr;
	int 					i, j, k;
	int 					num_ents; 
	UU_LOGICAL			blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_symbol"));	

	/* loop thru all instances */

	for(k=0;k<e_symbol->no_inst;k++)
		{
		e_instance.key = e_symbol->inst[k].inst_key;
		ur_retrieve_data_relnum(e_instance.key, &e_instance.rel_num);
		if(uio_getdata(&e_instance, &attr_instance))
			{
			UIO_sub_swt = 2;
			num_ents = 0;
			ent_ptr = (UU_KEY_ID *) uu_toolmalloc((e_instance.no_geom +
									e_instance.no_text_nod + 10)*sizeof(UU_KEY_ID));

			for(i=0;i<e_instance.no_geom;i++)
				{
				e.key_id = e_instance.geom[i];
				ur_retrieve_data_relnum(e.key_id, &e.rel_num);
				if(uio_getdata(&e, &attr))
					{
					switch (e.rel_num)
						{
			 			 case UM_POINT_REL:		
							uio_point(fd1,fd2,&e,&attr,dcount,pcount);
							ent_ptr[num_ents] = *dcount-2;
							num_ents++;
							break;
					 	 case UM_LINE_REL:		
							uio_line(fd1,fd2,&e,&attr,dcount,pcount);
							ent_ptr[num_ents] = *dcount-2;
							num_ents++;
							break;
					 	 case UM_CIRCLE_REL:	
							uio_arc(fd1,fd2,&e,&attr,dcount,pcount);
							ent_ptr[num_ents] = *dcount-2;
							num_ents++;
							break;
					 	 case UM_CONIC_REL:
							uio_conic_arc(fd1,fd2,&e,&attr,dcount,pcount);
							ent_ptr[num_ents] = *dcount-2;
							num_ents++;
							break;
		 				 case UM_RBSPLCRV_REL:	
							uio_rbspl(fd1,fd2,&e,&attr,dcount,pcount);
							ent_ptr[num_ents] = *dcount-2;
							num_ents++;
							break;
 				 		case UM_AGCRV_REL:		/* AG bspline curve */
							uio_agcrv_rbspl(fd1,fd2,&e,&attr,dcount,pcount);
							ent_ptr[num_ents] = *dcount-2;
							num_ents++;
							break;
	 			 		case UM_POLY_REL:	
							uio_map_polygon(fd1,fd2,&e,&attr,dcount,pcount);
							ent_ptr[num_ents] = *dcount-2;
							num_ents++;
							break;
			 			 case UM_POLYLINE_REL:
							uio_map_poly3(fd1,fd2,&e,&attr,dcount,pcount);
							ent_ptr[num_ents] = *dcount-2;
							num_ents++;
							break;

						default:
							 break;
			  		 	}
	    			}
				}

			for(i=0;i<e_instance.no_text_nod;i++)
				{
				e.key_id = e_instance.text_nod[i].text_key;
				ur_retrieve_data_relnum(e.key_id, &e.rel_num);
				if(uio_getdata(&e, &attr))
					{
					uio_symbol_text(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					}
				}

			UIO_sub_swt = 0;

			uio_init_dir(attr_symbol,&dblk);
			ur_retrieve_blanked(e_symbol->key, &blanked);
			dblk.blank = blanked? 1 : 0;
			dblk.rel_type = SUB_FIG;
			dblk.par_ptr = *pcount;
			dblk.sub_swt = 2;
			dblk.use_flg = 2;
			dblk.hier = 1;
			dblk.form_no = 1;
			strcpy(dblk.label,"SUBFIG");
			dblk.subsno = 0;       /*720vp*/
			sfdrec.depth = 0;
			sprintf(sfdrec.name,"%s%d",e_symbol->label,k+1);
			sfdrec.num = num_ents;
			sfdrec.no_cid = num_ents;
			sfdrec.cid = ent_ptr;
		
			sfdrec.key = SUB_FIG;
			sfdrec.no_bptr = 0;
			sfdrec.no_prop = 0;
			uio_put_para(SUB_FIG,&sfdrec,fd2,pcount,*dcount);
			uio_attr_to_dir(fd1,&dblk,dcount,*pcount);


			uio_init_dir(&attr_instance,&dblk);
			ur_retrieve_blanked(e_instance.key, &blanked);
			dblk.blank = blanked? 1 : 0;
			dblk.rel_type = INST;
			dblk.par_ptr = *pcount;
			dblk.sub_swt = 3;
			dblk.use_flg = 2;
			dblk.hier = 0;
			dblk.form_no = 1;
			strcpy(dblk.label,"INST");
			dblk.subsno = 0;       /*720vp*/
		
			sfirec.key = INST;
			sfirec.xyz[0] = 0.0;
			sfirec.xyz[1] = 0.0;
			sfirec.xyz[2] = 0.0;
			sfirec.scale =  1.0;
			sfirec.sfd_ptr = *dcount - 2;
			sfirec.no_bptr = 0;
			sfirec.no_prop = 0;
			uio_put_para(INST,&sfirec,fd2,pcount,*dcount);
			uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
			uu_toolfree(ent_ptr);
			}
		}
fexit:;
	uu_dexit;	
	}	

/*********************************************************************
**    I_FUNCTION :  uio_symbol_text(fd1,fd2,e,attr,dcount,pcount)
**      Output symbol text entity
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uio_symbol_text(fd1,fd2,e_text,attr_text,dcount,pcount)
	int	fd1, fd2;
	struct UA_txt_rec			*e_text;
	struct UA_txtattr_rec 		*attr_text;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;		/* parameter section sequence number */

	{
	struct IG_igesnote_rec	noterec;
	struct IG_gnote_rec *p;
	UM_transf	trf;
	int	status, mtrid, i, j, m, sum, jprev;
	UU_REAL	wid, hgt, ang, txsize, t[12];		
	UM_coord	origin, vc, v1;
	char	*pstr;
	struct  dir_rec dblk;
	register char	ch;
	register	 n;
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter	uio_symbol_text"));

	if (e_text->no_tchar == 0) goto fexit;
	uio_init_dir(attr_text,&dblk);
	if(UIO_sub_swt == 0)
			uio_check_view(fd1, fd2, e_text->key, &dblk, dcount, pcount);
	ur_retrieve_blanked(e_text->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GNOTE;
	dblk.par_ptr = *pcount;
	dblk.use_flg = 1;			/* this is an annotation */
	dblk.sub_swt = UIO_sub_swt;			
	strcpy(dblk.label,"GNOTE");
	dblk.subsno = 0;       /*720vp*/

	noterec.key = GNOTE;

		/* set the trasformation matrix	*/
	uio_set_text_trf(e_text, attr_text, trf);

	n = e_text->no_tchar;
	sum = 0;
	if(n < 3) sum++;
	else
		{
		jprev = 0;
		for (j=0; j<=n; j++)
			{
			if (((ch=e_text->tchar[j])=='\n')||(ch=='\0'))
				{
				if(j != jprev + 1)
					{
					sum++;
					jprev = j;
					}
				else
					jprev = j;
				}
			}
		}
	noterec.num_str = noterec.no_gnote = sum;
	noterec.no_bptr = 0;
	noterec.no_prop = 0;
	p = noterec.gnote = (struct IG_gnote_rec *)
							uu_toolmalloc(sum*sizeof(struct IG_gnote_rec));

	hgt = e_text->dy / sum;
	ang = e_text->tangle + 3.1416 + 1.57;
	um_xyztovc(cos(ang),sin(ang),(UU_REAL) 0.0,vc);
	UIO_LEN_INTTOEXT(attr_text->height,txsize);
	UIO_LEN_INTTOEXT(e_text->dx,wid);

	pstr = e_text->tchar;
	for(i=0;i<3;i++) origin[i] = 0.0;

	for(i=0;i<sum;i++)
  	  {
		p->str_cnt = uio_nextstr(&pstr,p->str);
		p->wt = wid;		/* box width */
		p->ht = txsize;		/* text size  */
		p->fc = 14;			/* font characteristic - simplex roman*/
		p->sl = attr_text->slant+1.57;					/* slant angle */
		p->ang = e_text->tangle;					/* rotation angle */
		p->mir = 0;			/* no mirror */
		p->vh = 0;			/* rotate internal text flag */
		um_vctmsc(vc,i*hgt,v1);
		um_vcplvc(origin,v1,v1);
		UIO_CC_INTTOEXT(v1,p->xyzt);		/* change origin unit  */
		p++;
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
	if (noterec.gnote) uu_toolfree(noterec.gnote);
fexit:;
	uu_dexit;
	}	

/*********************************************************************
**    I_FUNCTION :  uio_set_text_trf(e, attr, trf)
**       Set the modeling transfomation from text data
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uio_set_text_trf(e, attr, trf)
struct UA_txt_rec *e;
struct UA_txtattr_rec *attr;
UM_transf	trf;

	{
	UU_REAL xaxis[3], yaxis[3], zaxis[3];
	UU_REAL vec[3];

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter uio_set_text_trf"));
	um_vctovc(attr->up, yaxis);
	um_vctovc(attr->plane, zaxis);
	um_cross(yaxis, zaxis, xaxis);
	um_unitvc(xaxis, xaxis);
	um_unitvc(yaxis, yaxis);
	um_unitvc(zaxis, zaxis);

	um_vctovc(xaxis, trf[0]);
	um_vctovc(yaxis, trf[1]);
	um_vctovc(zaxis, trf[2]);
	UIO_CC_INTTOEXT(e->position, vec);
	um_vctovc(vec, trf[3]);
	uu_dexit;
	}	

/*********************************************************************
**    I_FUNCTION :  uio_connector(fd1,fd2,e,attr,dcount,pcount)
**      Output a connector.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uio_connector(fd1,fd2,e_cont,attr_cont,dcount,pcount)
	int	fd1, fd2;
	struct UB_conector_rec			*e_cont;
	struct UM_attr_rec 				*attr_cont;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;		/* parameter section sequence number */

	{
	struct UM_polyline_rec         p_line;
	struct UR_attr						attr;
	UM_transf	trf;
	int	status;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter	uio_connector"));

	p_line.key = e_cont->pline;
	if(uio_getdata(&p_line, &attr))
		uio_map_poly3(fd1, fd2, &p_line, &attr, dcount, pcount);
	uu_dexit;
	}	
/*********************************************************************
**    I_FUNCTION :  uio_map_poly3(fd1,fd2,poly,attr,dcount,pcount)
**       Map Unicad's polyline entity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uio_map_poly3(fd1,fd2,poly,attr,dcount,pcount)
	int	fd1, fd2;
	struct UM_polyline_rec   *poly;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;		/* parameter section sequence number */
	{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_poly3d_rec	polyrec;
	UU_REAL	*ptptr;
	UU_REAL	v1[3], v2[3];
	int	i, num, prop[5];
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter uio_map_poly3"));

	uio_init_dir(attr,&dblk);
	ur_retrieve_blanked(poly->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GPOLY;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
	dblk.use_flg = 0;
	dblk.form_no = 12;
	strcpy(dblk.label,"POLY3");    
/*	strcpy(dblk.label,poly->label);    /*721vp*/
	dblk.subsno = poly->subscr;       /*721vp*/

	polyrec.key = GPOLY;
	polyrec.type = 2;
	polyrec.no_pt3 = poly->no_pt;
	ptptr = polyrec.pt3 = (UU_REAL *) uu_toolmalloc(3*polyrec.no_pt3
																		*sizeof(UU_REAL));
	num = 3*poly->no_pt;
	for(i=0;i<num;i++)
		{
		UIO_LEN_INTTOEXT(poly->pt[i],polyrec.pt3[i]);		
		}
/*
.....put label in properties
*/
	uio_label (fd1,fd2,poly->label,dcount,pcount);
	polyrec.no_bptr = 0;
	dblk.par_ptr = *pcount;
	polyrec.no_prop = 1;
	prop[0] = *dcount - 2;
	polyrec.prop = &prop[0];

	uio_put_para(GPOLY3D,&polyrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(polyrec.pt3);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION :  uio_map_polygon(fd1,fd2,polygon,attr,dcount,pcount)
**       Map Unicad's polyline entity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uio_map_polygon(fd1,fd2,polygon,attr,dcount,pcount)
	int	fd1, fd2;
	struct UM_poly_rec   *polygon;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;		/* parameter section sequence number */
	{
	struct  dir_rec dblk;		/* directory record */
	struct  IG_poly3d_rec	polyrec;
	UU_REAL	*ptptr;
	UU_REAL	v1[3], v2[3];
	int	i, j, k, num, prop[5];
	UU_LOGICAL	blanked, repeat, um_cceqcc();

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"enter uio_map_polygon"));

	uio_init_dir(attr,&dblk);
	repeat = UU_FALSE;
	ur_retrieve_blanked(polygon->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = GPOLY;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = UIO_sub_swt;
	dblk.use_flg = 0;
	dblk.form_no = 12;
	strcpy(dblk.label,"GPOLY"); 
/*	strcpy(dblk.label,polygon->label);    /*721vp*/
	dblk.subsno = polygon->subscr;       /*721vp*/

	polyrec.key = GPOLY;
	polyrec.type = 2;
	polyrec.no_pt3 = num = polygon->numvtx;
	if(um_cceqcc(polygon->vertex[0], polygon->vertex[num-1]) != UU_TRUE)
		{
		repeat = UU_TRUE;
		polyrec.no_pt3++;
		}
	ptptr = polyrec.pt3 = (UU_REAL *) uu_toolmalloc(3*polyrec.no_pt3
																		*sizeof(UU_REAL));
	j=0;
	for(i=0;i<num;i++)
		{
		for(k=0;k<3;k++)
			{
			UIO_LEN_INTTOEXT(polygon->vertex[i][k],polyrec.pt3[j]);		
			j++;
			}
		}

	if(repeat)
		{
		for(k=0;k<3;k++)
			{
			UIO_LEN_INTTOEXT(polygon->vertex[0][k],polyrec.pt3[j]);		
			j++;
			}
		}
/*
.....put label in properties
*/
	uio_label (fd1,fd2,polygon->label,dcount,pcount);
	polyrec.no_bptr = 0;
	dblk.par_ptr = *pcount;
	polyrec.no_prop = 1;
	prop[0] = *dcount - 2;
	polyrec.prop = &prop[0];

	uio_put_para(GPOLY3D,&polyrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_toolfree(polyrec.pt3);
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION :  uio_out_drawing(fd1,fd2,e,attr,dcount,pcount)
**      Output the entities for the drawing entity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/*MILL:renamed to resolve name conflict on VMS */ 
uio_out_drawing(fd1,fd2,e_draw,attr_draw,dcount,pcount)
	int	fd1, fd2;
	struct UM_drawing_rec	*e_draw;
	struct UR_attr 			*attr_draw;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */

	{
	struct  dir_rec dblk;				/* directory record */
	struct  IG_igesdrw_rec	drwrec;	/* iges drawing def record */
	UU_KEY_ID 	key, view_key, *ent_ptr;
	int 			i, j, k;
	int 			num_ents; 
	UU_LOGICAL	blanked;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_drawing"));	

	UIO_sub_swt = 2;
	num_ents = 0;
	UIO_drawing = UU_TRUE;

	/* ouput a view entity */

	uio_default_view(fd1,fd2,dcount,pcount);
	view_key = *dcount-2;
	ent_ptr = (UU_KEY_ID *) uu_toolmalloc((e_draw->no_member+10)
																		*sizeof(UU_KEY_ID));

	/*  output subordinate entities */

	for(i=0;i<e_draw->no_member;i++)
		{
		e.key_id = e_draw->member[i];
		ur_retrieve_data_relnum(e.key_id, &e.rel_num);
		if(uio_getdata(&e, &attr))
			{
			switch (e.rel_num)
				{
	 			 case UM_POINT_REL:			/* 1 point */
					uio_point(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					break;
			 	 case UM_LINE_REL	:			/* 2 line */
					uio_line(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					break;
			 	 case UM_CIRCLE_REL:			/* 3 circle */
					uio_arc(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					break;
			 	 case UM_CONIC_REL:			/* 4 conic curve*/
					uio_conic_arc(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					break;
 				 case UM_RBSPLCRV_REL:		/* 7 rational bspline curve */
					uio_rbspl(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					break;
 				 case UM_AGCRV_REL:		/* AG bspline curve */
					uio_agcrv_rbspl(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					break;
	 			 case UM_POLY_REL:				/* 40 polyfill region	*/
					uio_map_polygon(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					break;
			 	 case UM_POLYLINE_REL:		/* 42 polyline */
					uio_map_poly3(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					break;

			 	 case UA_TEXT_REL:		/* text */
					uio_symbol_text(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					break;

			/**		Drafting Subsystem	**/
			 	 case UA_LINEAR_DIMS_REL:	/* 48 linear dimension */
					uio_draft(fd1,fd2,&e,&attr,dcount,pcount);
					ent_ptr[num_ents] = *dcount-2;
					num_ents++;
					break;
				default:
					 break;
	  		 	}	
			}
		}

	UIO_sub_swt = 0;

	/* output drawing definition record */

	uio_init_dir(attr_draw,&dblk);
	ur_retrieve_blanked(e_draw->key, &blanked);
	dblk.blank = blanked? 1 : 0;
	dblk.rel_type = DRAW;
	dblk.par_ptr = *pcount;
	dblk.sub_swt = 0;
	dblk.use_flg = 2;
	dblk.hier = 0;
	dblk.form_no = 1;
	strcpy(dblk.label,"DRAW");
	dblk.subsno = 0;       /*720vp*/

	drwrec.key = DRAW;
	drwrec.no_v_tab = 1;
	drwrec.v_tab = (struct IG_v_tab_rec *) uu_toolmalloc(2 *sizeof(
												struct IG_v_tab_rec));
	drwrec.v_tab[0].v_key = view_key;
	drwrec.v_tab[0].x_org = 0.0;
	drwrec.v_tab[0].y_org = 0.0;
	drwrec.no_cid = num_ents;
	drwrec.cid = ent_ptr;

						/* no back pointers */
	drwrec.no_bptr = 0;
						/* no properties */
	drwrec.no_prop = 0;

	uio_put_para(DRAW,&drwrec,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);

	UIO_drawing = UU_FALSE;
	uu_toolfree(ent_ptr);
	uu_toolfree(drwrec.v_tab);
	uu_dexit;	
	}	

/*********************************************************************
**    I_FUNCTION :  uio_default_view(key, dblk)
**      OUTPUT default view
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uio_default_view(fd1, fd2, dcount, pcount)
	int	fd1, fd2;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	{
	struct IG_igesvie_rec iges_view;
	struct  dir_rec dblk_view;				/* directory record */
	int i,j,k;
	UU_REAL  t[12];

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"entering uio_default_view dcnt %d pcnt %d",
								*dcount, *pcount));	

	/* create an identity transformation */

	uio_tran_dir(&dblk_view);
	dblk_view.matrix_ptr = *dcount;

	for(i=0;i<12;i++) t[i] = 0.0;
	t[0] = t[5] = t[10] = 1.0;
		
	uio_tran(fd1,fd2,t,dcount,pcount);

	/* now complete view */

	dblk_view.rel_type = VIEW;
	dblk_view.par_ptr = *pcount;
	dblk_view.sub_swt = 2;
	dblk_view.par_cnt = 1;
	strcpy(dblk_view.label,"VIEW");
	dblk_view.subsno = 0;       /*721vp*/
	
	iges_view.key = VIEW;
	iges_view.view_id = 13;
	iges_view.scale = 1.0;
	iges_view.left = 0;
	iges_view.top = 0;
	iges_view.right = 0;
	iges_view.bottom = 0;
	iges_view.back = 0;
	iges_view.front = 0;
								/* no back pointers */
	iges_view.no_bptr = 0;
								/* no properties */
	iges_view.no_prop = 0;

	uio_put_para(VIEW,&iges_view,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk_view,dcount,*pcount);
	uu_dexit;	
	}
/*********************************************************************
**    I_FUNCTION :  uio_label(fd1,fd2,label,dcount,pcount)
**       Put the IGES property label information (form 15)
**       into the directory and parameter section
**    PARAMETERS
**       INPUT  :
**          fd1,fd2 - unibase temporary files
**				label   - entity label
**				dcount  - iges directory record #. 
**				pcount  - iges parameter record #. 
**       OUTPUT :
**				dcount  - iges directory record #. 
**				pcount  - iges parameter record #. 
**    RETURNS      : # of characters in label 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uio_label(fd1,fd2,label,dcount,pcount)
int   fd1, fd2;
char  label[];
int   *dcount;    /* directory section sequence number */
int   *pcount;    /* parameter section sequence number */

{
	struct  dir_rec dblk;      /* directory record */
	struct  IG_igesprop_rec prop15;      /* directory record */
	int n;

	n   = NCL_MAX_LABEL;
	ul_strip_blanks (label,&n);
	if (n <= 1) 
		strcpy (prop15.name,"@UN");
	else
		strcpy (prop15.name,label);
	uio_label_dir(&dblk);
	dblk.rel_type = PROPERTY;
	dblk.par_ptr = *pcount;
	strcpy(dblk.label,"PROP0015");
	dblk.subsno = 0; 

	prop15.key = PROPERTY;
	prop15.pptr = 1;
	prop15.no_bptr = 0;
	prop15.no_prop = 0;
	uio_put_para (PROPERTY,&prop15,fd2,pcount,*dcount);
	uio_attr_to_dir(fd1,&dblk,dcount,*pcount);
	uu_dexit;
	return(n);	
}
/*********************************************************************
**    I_FUNCTION :  uio_label_dir(dblk)
**       Initialize the property form 15 (label info) directory record 
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          dblk - iges directory record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uio_label_dir(dblk)
struct  dir_rec   *dblk;      /* directory record */
{

/*------------------------------------------------------------------------
** Start of executable code
**----------------------------------------------------------------------*/

	dblk->line_font = 1;
	dblk->level = 0;
	dblk->view_ptr = 0;
	dblk->matrix_ptr = 0;
	dblk->blank = 0;
	dblk->sub_swt = 0;
	dblk->use_flg = 0;
	dblk->hier = 0;
	dblk->line_wt = 0;
	dblk->pen_no = 0;
	dblk->par_cnt = 0;
	dblk->form_no = 15;
}

