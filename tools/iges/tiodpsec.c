/*********************************************************************
**    NAME         :  tiodpsec.c
**       CONTAINS:
**    		uio_dir_par(fd1,fd2)
**    		uio_draft(fd1,fd2,eu,attr,dcount,pcount)
**				uio_getdata(e,attr)
**				uio_init_dir(attr,dblk)
**				uio_tran_dir(dblk)
**				uio_attr_to_dir(fd1,dblk,dcount,pcount)
**				uio_term_sec(fd,fd1,fd2,scount,gcount,dcount,pcount)	
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tiodpsec.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:52
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"
#include "mdrel.h"
#include	"mdattr.h"
#include "mattrddl.h"
#include	"tiges.h"
#include "mcrv.h"
#include	"tioconv.h"
#include	"adraft.h"
#include	"tigtypes.h"
#include "xenv1.h"		/* used by "ux" calls: UX_PRTERRS value */
#include	"nccs.h"

#if (UU_COMP==UU_WIN2K)
#include <io.h>
#define write _write
#define read _read
#define close _close
#define open _open
#define creat _creat
#define lseek _lseek
#endif

/*********************************************************************
**    I_FUNCTION :  uio_dir_par(fd1,fd2)
**      Use Unibase to get ALL geometry except those un-needed then
**		  set those information to the directory and parameter section
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_dir_par(fd1,fd2,dcount,pcount)
	int	fd1;		/* file to store the directory information */
	int	fd2;		/* file to store the parameter information */
	int	*dcount;		/* directory sequence number */
	int	*pcount;	 	/* parameter sequence number */

	{
	struct UR_data 	e;
	struct UR_attr 	attr;
	struct NCL_fixed_databag *eptr;
	int next_tupleid;
	char	*cptr;
	FILE	/* *fopen(),*/ *ddl_fd;
	int	status, count;
	char	*charptr;
	UX_pathname	pathname;
	UU_LOGICAL	found;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
	*dcount = 1;
	*pcount = 1;
	next_tupleid = 1; 		/* the first master tupleid */

	charptr = UU_NULL;
	if (ux_get_syspath("IGES_DDL",&charptr,pathname,&found,
											UX_NPRTERRS|UX_NQUOTES)==UU_SUCCESS)
	{
		ddl_fd = fopen(pathname,"r");           /* open ddl file */
		status = ur_init_data_dict(ddl_fd);       /* initialize data dictionary */
	}
	else
		return;
	uu_lsdel(charptr);
	eptr = (struct NCL_fixed_databag *)&e;
									/* query Unibase for next key */
/*
......output GCOLOR information
*/
	uio_colortable(fd1,fd2,dcount,pcount);
	while (ur_get_next_key(&next_tupleid, &e.key_id) > -1)
	{
		next_tupleid++;
		ur_retrieve_data_relnum(e.key_id, &e.rel_num);
		if((e.rel_num != UM_DRAWING_REL) && UIG_drawing_only) continue;
		if (e.rel_num == NCL_LABTBL_REL) continue;
		if(uio_getdata(&e, &attr))
			{
			/**		check for SYMBOL, DRAWING  	**/

 	 		if( e.rel_num == UB_SYMBOL_REL || e.rel_num == UM_DRAWING_REL)	
				{
				switch (e.rel_num)
					{
		 			 case UB_SYMBOL_REL:			/* SYMBOL */
						if(!UIG_drawing_only)
										uio_symbol(fd1,fd2,&e,&attr,dcount,pcount);
						break;
		 			 case UM_DRAWING_REL:			/* DRAWING */
						if(UIG_drawing_only) 
							/*MILL:renamed to resolve name conflict on VMS */ 
							uio_out_drawing(fd1,fd2,&e,&attr,dcount,pcount);
						break;
					}
				}
			else if(attr.displayable != UM_NEVERDISPLAYABLE && 
					  attr.displayable != UM_UNDISPLAYABLE  &&
					  strncmp(eptr->label,"@UN",3) ||
					  e.rel_num == NCL_MATRIX_REL)
				{
				switch (e.rel_num)
					{
		 			 case UM_POINT_REL:			/* 1 point */
						if(!UIG_drawing_only)
										uio_point(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_LINE_REL	:			/* 2 line */
						if(!UIG_drawing_only)
										uio_line(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case NCL_PLN_REL	:			/* 91 plane */
						if(!UIG_drawing_only)
										uio_plane(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case NCL_POINTVEC_REL: 	/* 98 pointvector */
						if(!UIG_drawing_only)
										uio_nclvec(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case NCL_VECTOR_REL	:		/* 80 vector */
						if(!UIG_drawing_only)
										uio_nclvec(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_CIRCLE_REL:			/* 3 circle */
						if(!UIG_drawing_only)
										uio_arc(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_CONIC_REL:			/* 4 conic curve*/
						if(!UIG_drawing_only)
										uio_conic_arc(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_COMPCRV_REL:		/*	5 composite curve */
						if(!UIG_drawing_only)
										uio_comp_crv(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case NCL_CURVE_REL: 		/*	82 NCL Bezier curve */
						if(!UIG_drawing_only)
										uio_nclcrv(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case NCL_MATRIX_REL: 		/*	81 NCL matrix */
						if(!UIG_drawing_only)
										uio_ncltran(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case NCL_PATERN_REL: 		/*	92 NCL patern */
						if(!UIG_drawing_only)
										uio_nclpat(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_RBSPLCRV_REL:		/* 7 rational bspline curve */
						if(!UIG_drawing_only)
										uio_rbspl(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_UVCVONSF_REL:		/* 7 rational bspline curve */
						if(!UIG_drawing_only)
										uio_cvonsf1(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_AGCRV_REL:		/* 8 rational bspline curve */
						if(!UIG_drawing_only)
										uio_agcrv_rbspl(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_AGSRF_REL:		/* 10 AG rational bspline surface */
						if(!UIG_drawing_only)
										uio_agsrf_rbspl(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case NCL_SURF_REL:		/* 83 NCL surface */
						if(!UIG_drawing_only)
										uio_nclsrf (fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case NCL_MESHSURF_REL:		/* 85 NCL mesh surface */
						if(!UIG_drawing_only)
										uio_mshsrf (fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_RBSPLSRF_REL:		/* 11 rational bspline surface */
						if(!UIG_drawing_only)
										uio_rbsrf (fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case NCL_TRIMSF_REL:		/* 99 Trimmed surface */
						if(!UIG_drawing_only)
										uio_trimsrf (fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case NCL_REVSURF_REL:		/* 100 NCL surface of revolution */
						if(!UIG_drawing_only)
										uio_revsrf (fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_BODY_REL:			/*	31 solid body */
				 	 case UM_FEAT_REL:			/*	32 features "relation" */
		 			 case UM_TRANSFORM_REL:		/* 36 transformation */
				 	 case UM_ATTR_REL:			/*	37 point attribute */
						 break;
		 			 case UM_POLY_REL:				/* 40 polyfill region	*/
						if(!UIG_drawing_only)
										uio_map_polygon(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_POLYLINE_REL:		/* 42 polyline */
						if(!UIG_drawing_only)
										uio_map_poly3(fd1,fd2,&e,&attr,dcount,pcount);
						break;
				 	 case UM_COORDSYS_REL:		/* 43 coordinate system */
	 				 case UM_LAYER_REL:			/* 47 layer */
						 break;

		 			 case UM_GROUP_REL:			/* 44 group */
						if(!UIG_drawing_only)
										uio_group(fd1,fd2,&e,&attr,dcount,pcount);
						break;

		 			 case UB_CONECTOR_REL:			/*  connector */
						if(!UIG_drawing_only)
										uio_connector(fd1,fd2,&e,&attr,dcount,pcount);
						break;

		 			 case UA_TEXT_REL:			/*  text */
						if(!UIG_drawing_only)
										uio_symbol_text(fd1,fd2,&e,&attr,dcount,pcount);
						break;

				/**		Drafting Subsystem	**/
				 	 case UA_LINEAR_DIMS_REL:	/* 48 linear dimension */
						if(!UIG_drawing_only)
										uio_draft(fd1,fd2,&e,&attr,dcount,pcount);
						break;

				 	 default:
							 break;
					}	/* switch */
			  }
		 }

	}

	}	

/*********************************************************************
**    I_FUNCTION :  uio_draft(fd1,fd2,eu,attr,dcount,pcount)
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_draft(fd1,fd2,eu,attr,dcount,pcount)
	int	fd1, fd2;
	char	*eu;
	struct UR_attr 	*attr;
	int	*dcount;		/* directory section sequence number */
	int	*pcount;	/* parameter section sequence number */
	
	{
	struct UA_generic_draft es;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	ua_uni_to_sal(eu,&es);
	switch (es.etype)
		{
		 case	UA_LINEAR_DIM:
		 case UA_SYM_DIM:
			uio_lindim(fd1,fd2,&es,attr,dcount,pcount);
			break;
		 case UA_ARC_LEN_DIM:
			break;
		 case UA_ANGULAR_DIM:
			uio_angdim(fd1,fd2,&es,attr,dcount,pcount);
			break;
		 case UA_LABEL_DIM:
			uio_glabel(fd1,fd2,&es,attr,dcount,pcount);
			break;
		 case UA_BALLOON_DIM:
			uio_gsymbol(fd1,fd2,&es,attr,dcount,pcount);
			break;
		 case UA_NOTE_DIM:
			uio_gnote(fd1,fd2,&es,attr,dcount,pcount);
			break;
		 case UA_CENTERLINE:
			uio_centerline(fd1,fd2,&es,attr,dcount,pcount);
			break;
		 case UA_CROSSHATCH:
			uio_crosshatch(fd1,fd2,&es,attr,dcount,pcount);
			break;
		 case UA_DIA_IN_DIM:
		 case UA_DIA_OUT_DIM:
		 case UA_DIA_ONE_DIM:
			uio_diadim(fd1,fd2,&es,attr,dcount,pcount);
			break;
		 case UA_RAD_CEN_DIM:
		 case UA_RAD_OUT_DIM:
		 case UA_RAD_LRG_DIM:
			uio_radim(fd1,fd2,&es,attr,dcount,pcount);
			break;
		 default:
			break;
		}

	}

/*********************************************************************
**    I_FUNCTION :  uio_getdata(e,attr)
**      Retrieve the entity information from UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_getdata(e,attr)
	struct UR_data		*e;
	struct UR_attr 	*attr;

	{
	int	status;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
	status = UU_FALSE;

	if ((e->rel_num == UB_SYMBOL_REL) || (e->rel_num == UB_INSTANCE_REL))
		{
		status = ur_retrieve_app_data(e);
		}
	else if (e->rel_num == UM_AGCRV_REL || e->rel_num == UM_AGSRF_REL)
		{
		status = uc_retrieve_mtuple_data(e, sizeof(struct UR_data));
		}
	else
		{
		status = ncl_retrieve_data_fixed (e);
		}
	if(status == UU_SUCCESS)
	  {
		attr->key_id = e->key_id;
		if (ur_retrieve_attr(attr) == 0)	/* success in retrieving attributes */
		   {
			return(UU_TRUE);
		   }
		else
			{
			return(UU_FALSE);
			}
	   }
	else
		{
		return(UU_FALSE);
		}
	}

/*********************************************************************
**    I_FUNCTION :  uio_init_dir(attr,dblk,)
**       Initialize the directory record
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_init_dir(attr,dblk)
	struct UM_attrdata_rec 	*attr;
	struct  dir_rec   *dblk;		/* directory record */

	{
	char	buf[81];
	int i;
   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	switch (attr->line_style)
		{
		 case	UM_SOLID_LINE:
			dblk->line_font = 1;	break;
		 case UM_DASHED_LINE:
			dblk->line_font = 2;	break;
		 case UM_PHANTOM_LINE:
			dblk->line_font = 3;	break;
		 case UM_CENTER_LINE:
			dblk->line_font = 4;	break;
/*
.....Adding UM_DOT_LINE JLS 11/17/98
*/
		 case UM_DOT_LINE:
			dblk->line_font = 5; break;
		 default:
			dblk->line_font = 1;	break;
		}
	dblk->level = attr->layer;
	dblk->view_ptr = 0;
	dblk->matrix_ptr = 0;
	dblk->sub_swt = 0;
	dblk->use_flg = 0;
	dblk->hier = 0;
	dblk->line_wt = (attr->line_weight);
	if(attr->line_width != 0.0)
	{
		if(attr->line_width == 0.25)
		{
			dblk->line_wt = 2;
		}
		else
		{
			dblk->line_wt = 3;
		}
	}
	dblk->pen_no = 0;
	for(i=0;i<UIG_ncolor_iges;i++)
	{
		if (attr->color == UIG_color_array[i].color)
		{
			dblk->pen_no = (-1) * UIG_color_array[i].irec;
			break;
		}
	}
	dblk->par_cnt = 0;
	dblk->form_no = 0;
}	



/*********************************************************************
**    I_FUNCTION :  uio_tran_dir(dblk)
**       Initialize the transfer matrix directory record
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_tran_dir(dblk)
	struct  dir_rec   *dblk;		/* directory record */

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
	dblk->form_no = 0;
	}	


/*********************************************************************
**    I_FUNCTION :  uio_attr_to_dir(fd1,dblk,dcount,pcount)
**      Write the information in the directory record into a file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_attr_to_dir(fd1,dblk,dcount,pcount)
	int fd1;
	struct  dir_rec   *dblk;		/* directory record */
	int	*dcount;		/* directory section sequence number */
	int	 pcount;	/* parameter section sequence number */

	{
	char	buf[81];
	int	version, label_ptr;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	version = 0;
	label_ptr = 0;
	dblk->par_cnt = pcount - dblk->par_ptr;
	sprintf(buf,"%8d%8d%8d%8d%8d%8d%8d%8d0%d0%d0%d0%dD%7d",dblk->rel_type,
			  dblk->par_ptr,version,dblk->line_font,dblk->level,dblk->view_ptr,
			  dblk->matrix_ptr,label_ptr,dblk->blank,dblk->sub_swt,dblk->use_flg,
			  dblk->hier,(*dcount)++);
	uio_write(fd1,buf,80*sizeof(char));
	sprintf(buf,"%8d%8d%8d%8d%8d%8s%8s%-8s%8dD%7d",dblk->rel_type,
			  dblk->line_wt,dblk->pen_no,dblk->par_cnt,dblk->form_no,"","",
			  dblk->label,dblk->subsno,(*dcount)++);
	uio_write(fd1,buf,80*sizeof(char));
	}



/*********************************************************************
**    I_FUNCTION :  uio_term_sec(fd,fd1,fd2,scount,gcount,dcount,pcount)	
**       Process the terminate section
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_term_sec(fd,fd1,fd2,scount,gcount,dcount,pcount)	
	int	fd;			/* the final iges out file id */
	int	fd1, fd2;
	int	scount, gcount, dcount, pcount;

	{
	char	buf[513];
	int	n, i, len;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
	close(fd1);	close(fd2); 
	fd1 = open("iges1.tmp", 0);
	fd2 = open("iges2.tmp", 0);
	buf[80] = 10;
	while ((n=read(fd1,buf,80)) != 0) 
		n=write(fd,buf,81);
	while ((n=read(fd2,buf,80)) != 0)
		write(fd,buf,81);
	close(fd1);
	close(fd2);
	for (i=0; i<80; i++)
		buf[i] = ' ';
	sprintf(buf, "S%7dG%7dD%7dP%7d", scount-1,gcount-1,dcount-1,pcount-1);
	len = strlen(buf);
	for (i=len; i<73; i++)
		buf[i] = ' ';
	sprintf(&buf[72], "T%7d\n", 1);
	write(fd,buf,81);
	close(fd);
	}
