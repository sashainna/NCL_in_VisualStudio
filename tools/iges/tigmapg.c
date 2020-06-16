/*********************************************************************
**    NAME         :  tigmapg.c
**       CONTAINS:
**             check_color 
**             create_label
**             create_segment_label
**             uig_map_arc
**             uig_map_line
**             uig_map_pt
**             uig_map_conic
**             uig_map_poly2
**             uig_map_poly3
**             uig_map_poly6
**             uig_map_poly3crv
**             uig_map_poly6crv
**             uig_bs_data
**             uig_map_spline
**             uig_eval_cubic
**             uig_create_comp
**             uig_map_rbspl
**             uig_map_group
**             uig_map_plane
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       tigmapg.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:48
*********************************************************************/
#if UU_COMP == UU_WIN2K
#include <string.h>
#endif
#include "tiges.h"
#include "tigdefs.h"
#include "umath.h"
#include "mcrv.h"
#include "mdattr.h"
#include "modef.h"
#include "tigsupp.h"
#include "mdrel.h"
#include "udebug.h"
#include "usysdef.h"
#include "mxxx.h"
#include "mdeval.h"
#include "rbase.h"
#include "nccs.h"	/*jkd31a*/
#include "ag_incl.h"
#include "ag_global.h"

UU_REAL nv[3] =  {0.,0.,1.};

#define UM_MAX_CNTL_PTS 200

extern UU_LOGICAL drw_flag, uvcv_flag, bump_u, bump_v;
extern UU_REAL starting_ang_u, starting_ang_v, period_u, period_v;
extern int drw_ptr; 
extern UU_REAL drw_t[4][3], drw_v[4][3], drw_s[4][3];
extern int MAX_PARA_REC;

extern char lpt[];
extern char lln[];
extern char lpl[];
extern char lci[];
extern char lcv[];
extern char lsf[];
extern char lpv[];
extern char lpn[];
extern char lxx[];

/* extern int sense[20]; */
static struct	UR_data	cons;
static struct	UR_attr	attr;

extern uig_exit();				/*jkd44: div by zero */
extern UU_KEY_ID tig_uvcvonsf_bskey;

void uig_eval_cubic();

/*jkd34: default colors */
/*********************************************************************
**    I_FUNCTION     :  check_color(dblk)
**				Set default color for entity.  
**    PARAMETERS   
**       INPUT  : 
**				dblk 					directory record     
**       OUTPUT :  
**				none   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void check_color(dblk)     
struct dir_rec *dblk;
{
	int i;
/*
.....If the pen_no attribute  points to a GCOLOR (314) entity, then 
.....the color is
.....mapped accordingly.
*/	
	if( dblk->pen_no < 0 && UIG_color_iges != 0 && UIG_ncolor_iges != 0)
	{
		for(i=0;i<UIG_ncolor_iges;i++)
		{
			if(UIG_color_array[i].irec == abs(dblk->pen_no))
			{
				dblk->pen_no = UIG_color_array[i].color;
				return;
			}
		}
		dblk->pen_no = 0;
	}
	else if (UIG_color_iges == 0)
	{
		switch(dblk->rel_type)
		{
/*
.....Added GFACE (entity 510) Himani
*/
			case GFACE:
			case GSPLSURF:
/*
.....Added GBDSRF (entity 143)  JLS 12/7/98
*/
			case GBDSRF:
			case GRSPLSRF:
			case GRULEDSRF:
			case GTRIMSRF:
			case GREVSRF:
				dblk->pen_no = UM_MAGENTA;
				break;
			case GLINE:
				dblk->pen_no = UM_DARKGREEN;
				break;
			case GPOLY:
			case GPOLY6D:
/*
..... form 13 is a polyline curve hence CYAN
*/ 
				if(entity_mask[36] ==1 && dblk->form_no ==13)
					dblk->pen_no = UM_CYAN;
				else
					dblk->pen_no = UM_DARKRED;
				break;
			case GPOLY3D:
/*
..... form 12 is a polyline curve hence CYAN
*/
				if(entity_mask[36] ==1 && dblk->form_no ==12)
					dblk->pen_no = UM_CYAN;
				else
					dblk->pen_no = UM_YELLOW;  /* vp 5-may-93 copious data now */
				break;                     /* is converted to patern in NCL  */
			case GPOINT:
				dblk->pen_no = UM_DARKRED;
				break;
			case GPLANE:
				dblk->pen_no = UM_DARKGREEN;
				break;
			case GARC:
				dblk->pen_no = UM_DARKBLUE;    /* Really DARK BLUE */
				break;
			case GCONIC:
				dblk->pen_no = UM_YELLOW;
				break;
			case GSPLINE:
			case GRSPLINE:
				dblk->pen_no = UM_CYAN;
				break;
			default:
				dblk->pen_no = UM_YELLOW; 
				break;
		}
	}
	else 
	{
/*
.....used iges color but no color table defined UIG_color_iges!=0 but dblk->pen_no >= 0
*/
/*
......if there is no color table, IGES only have 8 standard colors
......if more than 8, we accept as default color
*/
/*
.....positive pen number, convert to NCL standard color
*/
		switch (dblk->pen_no)
		{
			case 1:
				dblk->pen_no = UM_BLACK;
				break;
			case 2:
				dblk->pen_no = UM_RED;
				break;
			case 3:
				dblk->pen_no = UM_GREEN;
				break;
			case 4:
				dblk->pen_no = UM_BLUE;
				break;
			case 5:
				dblk->pen_no = UM_YELLOW;
				break;
			case 6:
				dblk->pen_no = UM_MAGENTA;
				break;
			case 7:
				dblk->pen_no = UM_CYAN;
				break;
			case 8:
				dblk->pen_no = UM_WHITE;
				break;
			default:
				dblk->pen_no = -1; 
				break;
		}
	}
}
/*jkd15a: unique label names */
/*********************************************************************
**    I_FUNCTION     :  init_label(etype, drec)
**				Initialize a label for entity. (No longer performs a useful function)
**    PARAMETERS   
**       INPUT  : 
**				etype					entity type          
**				drec					directory record index
**       OUTPUT :  
**				none   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void init_label(etype, drec)
	int etype, drec;
	{
	}
/*********************************************************************
**    I_FUNCTION     :  create_label(dblk,nprops,props,label,isub)
**            Create a label for entity.
**    PARAMETERS   
**       INPUT  : 
**          dblk     - IGES directory block
**          nprops   - Number of properties.
**          props    - Directory record seq numbers of properties.
**       OUTPUT :  
**          label    - created label. 
**          isub     - subscript (zero for now)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

create_label(dblk,nprops,props,label,isub)
struct dir_rec *dblk;         /* IGES directory record */
int nprops, *props;
char label[];
int *isub;
{
	int i, j, status, *isubptr,inc;
	int rel_num;
	struct dir_rec ldir;
	struct IG_igesprop_rec prec;
	char *p1,buf[20];

	*isub = 0;
	isubptr = UU_NULL;
	if (label_comp_element == UU_TRUE) /* label composite elements */
	{
		strcpy(label, "@UN    ");
/*
.....We want to mark the property as being translated
.....if label_type=6 and there is a property for this
.....component.  Sometimes if it is a uv curve, nprops is
.....greater than 1 but there is bad information in
.....props, so check to see if it is a valid record.
.....JLS 1/27/99
*/
		if((label_type==6||label_type==10) && nprops>0)
		{
			if ((props[0]>0)&&(props[0]<150000))
			{
				update_counts(1,props[0]);
			}
		}
		return(UU_SUCCESS);
	}
/*
.....From file, from file with subscript or from file with max 6 chars,
.....from file label and subscript fields.
*/
	if (label_type == 3 || label_type == 4 || label_type == 7 ||
		label_type == 9)
	{
/*
.....If the composite curve is a boundary curve of a trimmed surface and it is 
.....translated independently and the labels are from the iges Label field
.....then we donot assign the iges label field to this curve but we assign
.....it to the trimmed surface.
*/
		if ((label_type == 3) && (dblk->rel_type == GCOMPOSITE) 
									 && (UIG_from_trimsrf))
			goto genlab;
		strcpy(label, "      ");	/*jkd15d*/
		j = 0;
		inc = 0;
		for (i=0;i<16;i++)
		{
			if (inc == 0 && i == 8) break;
			if (dblk->label[i] == '(') inc = i + 1;
			else if (dblk->label[i] == ')')
			{
				strncpy(buf,&dblk->label[inc],i-inc);
				buf[i-inc] = '\0';
				ul_to_number(buf,&dblk->seq_no);
				break;
			}
			else if (inc == 0 && dblk->label[i] != ' ' && dblk->label[i] != '*')
			{
				label[j] = dblk->label[i];
				j++;
			};
		}
/*
.....vp 2-mar-97 allow long labels
.....vp 5-aug-97 make sure label is in upper case
*/
		if (label_type == 7)
		{
			label[6] = '\0';		/*jkd15d*/
			label[7] = '\0';
		}
		else if (label_type == 9)
			sprintf(&label[j], "%d", dblk->seq_no);
		else
 			label[j] = '\0';
		if (label_type == 4) *isub = dblk->seq_no;
		ul_to_upper(label);
		if (uig_label_check(label,*isub)) goto genlab;
		return(UU_SUCCESS);
	};
/*
.....CV style labels.
*/
	if (label_type == 5)
	{
		sprintf(label, "L%d    ", dblk->seq_no);
		if (isalpha(dblk->label[7]))
		{
			label[0] = dblk->label[7];
		};
/*
.....vp 2-mar-97 allow long labels
.....vp 5-aug-97 make sure label is in upper case
*/
		label[8] = '\0'; 
		ul_to_upper(label);
		if (uig_label_check(label,*isub)) goto genlab;
		return(UU_SUCCESS);
	};
/*
.....Labels from IGES property entity.
*/
	if ((label_type == 6 || label_type == 10) && (nprops > 0))
	{
		for (i=0; i<nprops; i++)
		{
			uig_get_directory(props[i],&ldir);
			if (ldir.form_no == 15)
			{
				status = uig_get_para_rec(ldir.rel_type,props[i],ldir.par_ptr,&prec);
				if (status == 0)
				{
					p1 = strchr (prec.name, 'H');
					if (p1 != NULL)
					{
						strcpy (label, p1+1);
/*
.....vp 5-aug-97 make sure label is in upper case
.....and subscript is copied from dirctory record.
*/
						ul_to_upper(label);
						if (label_type == 6) *isub = dblk->seq_no;
/*
.....If the label property is being used we want to make
.....sure that it is counted as being translated.  The
.....1 is to indicate that the property has been translated.   
.....props[i] is the record of the property. JLS 1/22/99
*/
						update_counts(1,props[i]);
						if (uig_label_check(label,*isub)) goto genlab;
						return(UU_SUCCESS);
					}
				}
			}
		}
	}

	if (label_type == 2)
	{
		switch(dblk->rel_type)
		{
			case GSPLSURF:
			case GRSPLSRF:
			case GRULEDSRF:
			case GREVSRF:
			case GTBCYSRF:
/*
.....Added GBDSRF (entity 143)  JLS 12/7/98
*/
			case GBDSRF:
/*
.....Added GFACE (entity 510) Himani
*/
      		case GFACE:
			case GTRIMSRF:
				isf++;
				*isub = isf;
				strcpy (label,lsf);
				break;
			case GLINE:
				iln++;
				*isub = iln;
				strcpy (label,geo_lab[2]);
				break;
			case GPOLY:
			
			case GPOLY6D:
/*
..... form 13 is a polyline curve 
*/
				if(dblk->form_no == 13 && entity_mask[36] ==1)
				{
   				icv++;
   				*isub = icv;
   				strcpy (label,lcv);
   				break;
				}
				ipv++;
				*isub = ipv;
				strcpy (label,lpv);
				break;
			case GPOLY3D:
/*
..... form 12 is a polyline curve
*/
				if(dblk->form_no == 12 && entity_mask[36] ==1)
				{
					icv++;
					*isub = icv;
					strcpy (label,lcv);
					break;
				}
				ipn++;
				*isub = ipn;
				strcpy (label,lpn);
				break;
			case GPOINT:
				ipt++;
				*isub = ipt;
				strcpy (label,geo_lab[0]);
				break;
			case GPLANE:
				ipl++;
				*isub = ipl;
				strcpy (label,geo_lab[4]);
				break;
			case GARC:
				ici++;
				*isub = ici;
				strcpy (label,lci);
				break;
			case GCONIC:
			case GSPLINE:
			case GRSPLINE:
/*
.....Added GLOOP (entity 508) Himani
*/

			case GLOOP:
			case GCOMPOSITE:      
				icv++;
				*isub = icv;
				strcpy (label,lcv);
				break;
			default:
				ixx++;
				*isub = ixx;
				strcpy (label,lxx);
				break;
		}
		return(UU_SUCCESS);
	}
/*
.....If label_type is equal to 8 we are checking a secondary
.....unibase for matching entities and giving the translated entities
.....those labels. So do nothing with the labels now. JLS 12/16/99
*/
	if (label_type ==8)
	{
		switch(dblk->rel_type)
		{
			case GSPLSURF:
			case GRSPLSRF:
			case GRULEDSRF:
			case GREVSRF:
			case GTBCYSRF:
			case GBDSRF:
			case GFACE:
			case GTRIMSRF:
			case GLINE:
			case GPOLY:
			case GPOLY3D:
			case GPOLY6D:
			case GPOINT:
			case GARC:
			case GCONIC:
			case GSPLINE:
			case GRSPLINE:
			case GCOMPOSITE:
/*
.....Added GLOOP (entity 508) Himani
*/
			case GLOOP:
				break;
			default:
				rel_num = 1;
				if(lab_flag[0]==1)
				{
					ipt++;
					*isub = ipt;
				}
				um_auto_label(rel_num, label);
				break;
		}
		return(UU_SUCCESS);
	}
genlab:;
	switch(dblk->rel_type)
	{
		case GSPLSURF:
			rel_num = 85;	/* NCL_MESHSURF_REL */
			break;
		case GRSPLSRF:
		case GRULEDSRF:
		case GREVSRF:
		case GTBCYSRF:
/*
.....Added GBDSRF (entity 143)  JLS 12/7/98
*/
		case GBDSRF:
		case GFACE:	
		case GTRIMSRF:
			rel_num = 11;	/* UM_RBSPLSRF_REL */
			break;
		case GLINE:
			rel_num = 2;	/* UM_LINE_REL */
			break;
		case GPOLY:
		case GPOLY6D:
/*
..... form 13 is a polyline curve
*/
			if(dblk->form_no ==13 && entity_mask[36] ==1)
			{
   			rel_num = 7;   /* UM_RBSPLCRV_REL */
   			break;
			}
			rel_num = 98;	 /* NCL_POINTVECTOR_REL */
			break;
		case GPOLY3D:
/*
..... form 12 is a polyline curve
*/
			if(dblk->form_no ==12 && entity_mask[36] ==1)
			{
				rel_num = 7;   /* UM_RBSPLCRV_REL */
				break;
			}
			rel_num = 92;	 /* NCL_PATERN_REL */
			break;
		case GPOINT:
			rel_num = 1;	/* UM_POINT_REL */
			break;
		case GPLANE:
			rel_num = 91;	/* NCL_PLN_REL */
			break;
		case GARC:
			rel_num = 3;	/* UM_CIRCLE_REL */
			break;
		case GCONIC:
			rel_num = 4;	/* UM_CONIC_REL */
			break;
		case GSPLINE:
			rel_num = 82;	/* NCL_CURVE_REL */
			break;
		case GRSPLINE:
			rel_num = 7;	/* UM_RBSPLCRV_REL */
			break;
		case GCOMPOSITE:  
/*
.....Added GLOOP (entity 508) Himani
*/
		case GLOOP:
			rel_num = 5;	/* UM_COMPCRV_REL */
			break;
		default:
			rel_num = 1;	/* UM_POINT_REL */
			break;
	}

	if (rel_num == 4)	/* UM_CONIC_REL */
			rel_num = 82;	/* NCL_CURVE_REL */

	do
	{
		um_auto_label(rel_num, label);
		um_auto_subscr(rel_num, isub);
		if (*isub != 0) *isub = *isub - 1;
		j=uig_label_check(label, *isub);
	} while (j==1);
/*
.....If label contains invalid characters, return dummy label.
*/
	if (j==2)
	{
		ixx++;
		*isub = ixx;
		strcpy(label,"XX     ");
	}

	return(UU_SUCCESS);
}


/*jkd15c: labels for generated entities */
/*********************************************************************
**    I_FUNCTION     :  create_segment_label(dblk,label)
**				Create a label for a segment.
**    PARAMETERS   
**       INPUT  : 
**				dblk						IGES directory block
**       OUTPUT :  
**				label						created label. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void create_segment_label(dblk,label)
	struct dir_rec *dblk;			/* IGES directory record */
	char label[];
	{
	int lab;  /*jkd15a: unique label names */

	isg++;
	lab = isg;
	sprintf(label, "SG%d   ", lab);
	label[6] = '\0';
	label[7] = '\0';
	}

/*********************************************************************
**    I_FUNCTION     :  uig_map_arc(dblk,igesin,t,key)
**				Map an IGES arc to a unibase arc.
**    PARAMETERS   
**       INPUT  : 
**				dblk							IGES directory block
**				igesin						IGES arc structure
**				t								associated matrix
**       OUTPUT :  
**				key							UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_arc(dblk,igesin,t,key)
struct dir_rec *dblk;				/* IGES arc directory record */
struct IG_igesarc_rec *igesin;	/* IGES arc parameter record */
UU_REAL	 t[12];
UU_KEY_ID *key;
{

	struct UM_circle_rec uniout;

	UU_REAL vecn[3],vecs[3],vece[3];     /* temporary vectors */
	UU_REAL pts_cc[3], pte_cc[3];
	UU_REAL ptc_cc[3],ptc[3],pts[3],pte[3];				/* temporary points */
	UU_LOGICAL um_cceqcc();								/* test if to pts are = */
	UU_REAL um_angle2p();								/* calc angle btw two vectors */
	UU_REAL um_mag();										/* calc magnitude of a vector */
	int uig_match_circle();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	/* setup unibase storage area */

	ur_setup_data(UM_CIRCLE_REL,&uniout,sizeof(struct UM_circle_rec));

	/*MILLS: initialize LABEL and SUBSCRIPT */
	strcpy(uniout.label,"");

	/* transform normal */

	uig_tran_vec(nv,t,vecn);
	um_vctovc(vecn,&uniout.nvec[0]);	/* transfer to output structure */

	/* transform points */

	ptc_cc[0] = igesin->cpt[0];					/* center */
	ptc_cc[1] = igesin->cpt[1];
	ptc_cc[2] = igesin->zt;
	/* adjust u value if periodic */
	if (bump_u && (period_u > 0.0) && (ptc_cc[0] < starting_ang_u))	
	{
		while (ptc_cc[0] < starting_ang_u) ptc_cc[0] += period_u;
		while (ptc_cc[0] > (starting_ang_u+period_u)) ptc_cc[0] -= period_u;
	}
	/* adjust v value if periodic */
	if (bump_v && (period_v > 0.0) && (ptc_cc[1] < starting_ang_v))
	{
		while (ptc_cc[1] < starting_ang_v) ptc_cc[1] += period_v;
		while (ptc_cc[1] > (starting_ang_v+period_v)) ptc_cc[1] -= period_v;
	}

	uig_tran_coor(ptc_cc,t,ptc);				/* transform to model space */
	um_vctmsc(ptc,unit_scale,ptc);			/* scale */
	um_vctovc(ptc,&uniout.center[0]);		/* transfer to output structure */

	pts_cc[0] = igesin->spt[0];					/* start */
	pts_cc[1] = igesin->spt[1];
	pts_cc[2] = igesin->zt;
	/* adjust u value of start point if periodic */
	if (bump_u && (period_u > 0.0) && (pts_cc[0] < starting_ang_u))	
	{
		while (pts_cc[0] < starting_ang_u) pts_cc[0] += period_u;
		while (pts_cc[0] > (starting_ang_u+period_u)) pts_cc[0] -= period_u;
	}
	/* adjust v value of start point if periodic */
	if (bump_v && (period_v > 0.0) && (pts_cc[1] < starting_ang_v))
	{
		while (pts_cc[1] < starting_ang_v) pts_cc[1] += period_v;
		while (pts_cc[1] > (starting_ang_v+period_v)) pts_cc[1] -= period_v;
	}
	uig_tran_coor(pts_cc,t,pts);			/* transform to model space */
	um_vctmsc(pts,unit_scale,pts);			/* scale */

	pte_cc[0] = igesin->ept[0];					/* end */
	pte_cc[1] = igesin->ept[1];
	pte_cc[2] = igesin->zt;
	/* adjust u value of end point if periodic */
	if (bump_u && (period_u > 0.0) && (pte_cc[0] < starting_ang_u))	
	{
		while (pte_cc[0] < starting_ang_u) pte_cc[0] += period_u;
		while (pte_cc[0] > (starting_ang_u+period_u)) pte_cc[0] -= period_u;
	}
	/* adjust v value of end point if periodic */
	if (bump_v && (period_v > 0.0) && (pte_cc[1] < starting_ang_v))
	{
		while (pte_cc[1] < starting_ang_v) pte_cc[1] += period_v;
		while (pte_cc[1] > (starting_ang_v+period_v)) pte_cc[1] -= period_v;
	}
	uig_tran_coor(pte_cc,t,pte);					/* transform to model space */
	um_vctmsc(pte,unit_scale,pte);			/* scale */

	/* calculate radius */

	um_vcmnvc(pts,ptc,vecs);				/* subtract center from start pt */
	uniout.radius = um_mag(vecs);		/* calculate magnitude of difference */

	/* calculate delta angle */

	if(um_cceqcc_tol(pts,pte,UM_DFUZZ) == UU_TRUE)	/* check if full circle */
	{
		uniout.dang = UM_TWOPI;					/* yes */
	}
	else												/* no */
	{
		um_vcmnvc(pte,ptc,vece);    /* subtract center from end pt */
		uniout.dang = um_angle2p(vecs,vece,vecn);
	}
	
	/* store start vector */

	um_unitvc(vecs,vecs);						/* unitize vector */
	um_vctovc(vecs,&uniout.svec[0]);		/* transfer to output structure */

	/* check if creating a drawing entity */

	if(drw_flag)
	{
		uig_transform_circle(&uniout, drw_v);
		uig_transform_circle(&uniout, drw_s);
		uig_transform_circle(&uniout, drw_t);
		dblk->view_ptr = drw_ptr;
	}
	uig_update_attr(dblk);
	um_save_active_label(uniout.rel_num);
	create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
	uig_create_geom(&uniout,0,0, dblk->view_ptr);

	if (UIG_nodups && !label_comp_element &&
		ncl_retrieve_data_fixed(&uniout) == UU_SUCCESS &&
		uig_match_circle(&uniout,0) == UU_SUCCESS)
	{
		uig_remove_dup(&uniout,key);
	}
	else
	{
	/* now create unibase record */
		UIG_unibase_entities++;

		if(label_type ==8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&uniout,uig_match_circle);
		}
		*key = uniout.key;
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  uig_map_line(dblk,igesin,t,key)
**				Map a IGES line to a Unibase line.
**    PARAMETERS   
**       INPUT  : 
**				dblk							IGES directory block
**				igesin						parameter block
**				t								IGES transformation matrix
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_line(dblk,igesin,t,key)
struct dir_rec *dblk;				/* IGES line directory record */
struct IG_igesline_rec *igesin;	/* IGES line parameter record */
UU_REAL t[12];
UU_KEY_ID *key;
{

	struct UM_line_rec uniout;
	UU_REAL pt1[3];
	int uig_match_line();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	/* setup unibase storage area */

	ur_setup_data(UM_LINE_REL,&uniout,sizeof(struct UM_line_rec));

	/*MILLS: initialize LABEL and SUBSCRIPT */
	strcpy(uniout.label,"");

	uig_tran_coor(&igesin->spt[0],t,pt1);		/* transform to model space */
	um_vctmsc(pt1,unit_scale,pt1);				/* scale */
	/* adjust u value if periodic */
	if (bump_u && (period_u > 0.0) &&(pt1[0] < starting_ang_u))
	{
		while (pt1[0] < starting_ang_u) pt1[0] += period_u;
		while (pt1[0] > (starting_ang_u+period_u)) pt1[0] -= period_u;
	}
	/* adjust v value if periodic */
	if (bump_v && (period_v > 0.0) && (pt1[1] < starting_ang_v))
	{
		while (pt1[1] < starting_ang_v) pt1[1] += period_v;
		while (pt1[1] > (starting_ang_v+period_v)) pt1[1] -= period_v;
	}
	um_vctovc(pt1,&uniout.spt[0]);				/* transfer to output structure */
	uig_tran_coor(&igesin->ept[0],t,pt1);		/* transform to model space */
	um_vctmsc(pt1,unit_scale,pt1);				/* scale */
	/* adjust u value if periodic */
	if (bump_u && (period_u > 0.0) &&(pt1[0] < starting_ang_u))
	{
		while (pt1[0] < starting_ang_u) pt1[0] += period_u;
		while (pt1[0] > (starting_ang_u+period_u)) pt1[0] -= period_u;
	}
	/* adjust v value if periodic */
	if (bump_v && (period_v > 0.0) && (pt1[1] < starting_ang_v))
	{
		while (pt1[1] < starting_ang_v) pt1[1] += period_v;
		while (pt1[1] > (starting_ang_v+period_v)) pt1[1] -= period_v;
	}
	um_vctovc(pt1,&uniout.ept[0]);				/* transfer to output structure */

	/* check if creating a drawing entity */

	if(drw_flag)
	{
		uig_transform_line(&uniout, drw_v);
		uig_transform_line(&uniout, drw_s);
		uig_transform_line(&uniout, drw_t);
		dblk->view_ptr = drw_ptr;
	}

	if (UIG_nodups && !label_comp_element && uig_match_line(&uniout,0) == UU_SUCCESS)
	{
		*key = 0;
		UIG_dupcount++;
	}
	else
	{

	/* create unibase record */

		uig_update_attr(dblk);
		create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
		uig_create_geom(&uniout,0,0,dblk->view_ptr);
		UIG_unibase_entities++;
		if(label_type ==8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&uniout,uig_match_line);
		}
		*key = uniout.key;
	}
	return(UU_SUCCESS);
}

/*jkd31: implement planes */
/*********************************************************************
**    I_FUNCTION     :  uig_map_plane(dblk,igesin,t,key)
**				Map an IGES plane to a Unibase plane.    
**    PARAMETERS   
**       INPUT  : 
**				dblk							IGES directory block
**				igesin						parameter block
**				t								IGES transformation matrix
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_map_plane(dblk,igesin,t,key)
struct dir_rec *dblk;				/* IGES plane directory record */
struct IG_igespln_rec *igesin;		/* IGES plane parameter record */
UU_REAL t[12];
UU_KEY_ID *key;
{

	struct NCL_nclpl_rec uniout;		/*jkd31a: use NCL plane record*/

	UU_REAL pt1[3], pt2[3], d_pt[3], temp_pt[3];
	UU_REAL sum = 0., dot;
	UU_REAL um_dot();
	int uig_match_plane();
	int i;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	/* setup unibase storage area */

	ur_setup_data(NCL_PLN_REL,&uniout,sizeof(struct NCL_nclpl_rec));  /*jkd31a*/

/*
.....Check if the normal vector is unitized if not , then unitize it.
*/
	for (i=0; i<3; i++)
		sum = sum + (igesin->coef[i] * igesin->coef[i]);
	if (fabs(1-sqrt(sum)) >= UM_FUZZ)
		for (i=0; i<4; i++)
			igesin->coef[i] = igesin->coef[i] / sqrt(sum);
	um_vctovc(&igesin->coef[0], pt1);
	um_vctmsc(pt1, igesin->coef[3], pt1);
	uig_tran_coor(pt1,t,pt2);					/* transform to model space */
	um_vctmsc(pt2,unit_scale,pt2);				/* scale */
	um_vctovc(pt2,&uniout.pt[0]);				/* transfer to output structure */
	uig_tran_coor(&igesin->coef[0],t,pt1);		/* transform to model space */
	um_unitvc(pt1, pt1);
	um_vctovc(pt1,&uniout.nvec[0]);				/* transfer to output structure */

	/* project display point onto the plane */
	uig_tran_coor(&igesin->disp_pt[0],t,d_pt);
	um_vctmsc(d_pt,unit_scale,d_pt);		

	um_vcmnvc(pt2, d_pt, temp_pt);
	dot = um_dot(temp_pt, pt1);
	um_vctmsc(pt1, dot, temp_pt);
	um_vcplvc(d_pt, temp_pt, d_pt);
	um_vctovc(d_pt,&uniout.pt[0]);			

	uniout.radius = 0.0;

	/* create unibase record */

	uig_update_attr(dblk);
	create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
	ncl_create_entity(&uniout,dblk->view_ptr);
	UIG_unibase_entities++;
	if(label_type ==8)
	{
/*
.....Label matching. Determine if exact match.
*/
		uig_exact_match(&uniout,uig_match_plane);
	}

	*key = uniout.key;
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  uig_map_pt(dblk,igesin,t,key)
**				Map a IGES pt to a Unibase pt.
**    PARAMETERS   
**       INPUT  : 
**				dblk							directory block
**				igesin						parameter block
**				t								IGES transformation matrix
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_pt(dblk,igesin,t,key)
struct dir_rec *dblk;				/* IGES pt directory record */
struct IG_igespt_rec *igesin;		/* IGES pt parameter record */
UU_REAL t[12];
UU_KEY_ID *key;
{

	struct UM_point_rec uniout;
	UU_REAL pt1[3];
	int uig_match_point();

/* 
.....Setup unibase storage space 
*/
	ur_setup_data(UM_POINT_REL,&uniout,sizeof(struct UM_point_rec));

/*
.....Initialize LABEL and SUBSCRIPT 
*/
	strcpy(uniout.label,"");

/*
.....transform to model space
.....scale
.....transfer to output structure
*/
	uig_tran_coor(&igesin->pt[0],t,pt1);		
	um_vctmsc(pt1,unit_scale,pt1);
	um_vctovc(pt1,&uniout.pt[0]);	
	uniout.markertype = 2;
	uniout.snap_node = UU_FALSE;

/* 
.....Check if creating a drawing entity 
*/

	if(drw_flag)
	{
		uig_transform_point(&uniout, drw_v);
		uig_transform_point(&uniout, drw_s);
		uig_transform_point(&uniout, drw_t);
		dblk->view_ptr = drw_ptr;
	}

	if (UIG_nodups && !label_comp_element && uig_match_point(&uniout,0) == UU_SUCCESS)
	{
		*key = 0;
		UIG_dupcount++;
	}
	else
	{
/* 
.....Create unibase record 
*/
		uig_update_attr(dblk); 
		create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
		uig_create_geom(&uniout,0,0,dblk->view_ptr);
		UIG_unibase_entities++;
		if(label_type ==8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&uniout,uig_match_point);
		}

		*key = uniout.key;
	}
	return(UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION     :  uig_map_conic(dblk,igesin,t,key)
**				Map a IGES conic to a Unibase conic.
**    PARAMETERS   
**       INPUT  : 
**				dblk							directory block
**				igesin						parameter block
**				t								IGES transformation matrix
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_map_conic(dblk,igesin,t,key)
struct dir_rec *dblk;				/* IGES conic directory record */
struct IG_igescon_rec *igesin;		/* IGES conic parameter record */
UU_REAL t[12];
UU_KEY_ID *key;
{

	struct UM_conic_rec uniout;

	UU_REAL spt[3], ept[3], ccpt[3], coeffs[6], tfmat[4][3];
	UU_REAL temp;
	int status;
	int i, j;
	int uig_match_conic();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	/* setup unibase storage space */

	ur_setup_data(UM_CONIC_REL,&uniout,sizeof(struct UM_conic_rec));

	/*MILLS: initialize LABEL and SUBSCRIPT */
	strcpy(uniout.label,"");

	/* load IGES transformation into the standard conic record */

	for(i=0;i<3;i++)
	{
		uniout.tfmat[i][0] = t[i];
		uniout.tfmat[i][1] = t[i+4];
		uniout.tfmat[i][2] = t[i+8];
	}
	uniout.tfmat[3][0] = t[3];
	uniout.tfmat[3][1] = t[7] ;
	uniout.tfmat[3][2] = t[11];

	/* load coeffs into a local array */

	coeffs[0] = igesin->a;
	coeffs[1] = igesin->b;
	coeffs[2] = igesin->c;
	coeffs[3] = igesin->d;
	coeffs[4] = igesin->e;
	coeffs[5] = igesin->f;

	/* create full conic record */

	if(uig_normquad(coeffs, &uniout, uniout.tfmat) < 0)	/* cpp */
	{
		/*jkd48: print and display errors*/
		sprintf(p_buff,
			"(DREC = %d) Conic coefficients don't define ellipse, \n", 
			dblk->drec_num);
		uig_error(p_buff);
		sprintf (p_buff, "            hyperbola, or parabola.\n");
		uig_error(p_buff);
		*key = 0;
		goto fexit;
	}
	else
	{
		switch(uniout.type)
		{
			case UM_PARABOLA:
				uniout.t0 = 50;
				uniout.t1 = -50;
				break;
			case UM_HYPERBOLA:
				uniout.t0 = .9;
				uniout.t1 = -.9;
				break;
			case UM_ELLIPSE:
				uniout.t0 = -2;
				uniout.t1 = 2;
				break;
			default:
				*key = 0;
				goto fexit;
		}
	}

		/* find correct endpoints for the conic */

	ccpt[0] = igesin->spt[0];
	ccpt[1] = igesin->spt[1];
	ccpt[2] = 0.0;     /* igesin->zt; */
	/* adjust u value if periodic */
	if (bump_u && (period_u > 0.0) && (ccpt[0] < starting_ang_u))  
	{
		while (ccpt[0] < starting_ang_u) ccpt[0] += period_u;
		while (ccpt[0] > (starting_ang_u+period_u)) ccpt[0] -= period_u;
	}
	/* adjust v value if periodic */
	if (bump_v && (period_v > 0.0) && (ccpt[1] < starting_ang_v))  
	{
		while (ccpt[1] < starting_ang_v) ccpt[1] += period_v;
		while (ccpt[1] > (starting_ang_v+period_u)) ccpt[1] -= period_v;
	}
	uig_tran_coor(ccpt,t,spt);	

	ccpt[0] = igesin->ept[0];
	ccpt[1] = igesin->ept[1];
	ccpt[2] = 0.0;     /* igesin->zt; */
	/* adjust u value if periodic */
	if (bump_u && (period_u > 0.0) && (ccpt[0] < starting_ang_u))  
	{
		while (ccpt[0] < starting_ang_u) ccpt[0] += period_u;
		while (ccpt[0] > (starting_ang_u+period_u)) ccpt[0] -= period_u;
	}
	/* adjust v value if periodic */
	if (bump_v && (period_v > 0.0) && (ccpt[1] < starting_ang_v))  
	{
		while (ccpt[1] < starting_ang_v) ccpt[1] += period_v;
		while (ccpt[1] > (starting_ang_v+period_v)) ccpt[1] -= period_v;
	}
	uig_tran_coor(ccpt,t,ept);

	tfmat[0][0] = tfmat[0][1] = tfmat[0][2] = 0.0;
	tfmat[1][0] = tfmat[1][1] = tfmat[1][2] = 0.0;
	tfmat[2][0] = tfmat[2][1] = tfmat[2][2] = 0.0;
	tfmat[3][0] = tfmat[3][1] = tfmat[3][2] = 0.0;
	tfmat[0][0] = tfmat[1][1] = tfmat[2][2] = 1.0;

	status = um_cn4_endpoints(&uniout, spt, ept, tfmat);
	if(status == 0)
	{
  		if (igesin->zt != 0.0)	/* include zt value in transf */
  		{
  			tfmat[3][2] = igesin->zt;
  			um_tftmtf(tfmat,uniout.tfmat,uniout.tfmat);
  		}
			/* create unibase record */
		for (i=0; i<4;i++)
			for (j=0; j<3; j++) uniout.tfmat[i][j] *= unit_scale;
		if(fabs(uniout.t0 - uniout.t1) < 0.01)
		{
			if(uniout.t0 > uniout.t1)
			{
				temp = uniout.t0;
				uniout.t0 = uniout.t1;
				uniout.t1 = temp;
			}
		}
			/* check if creating a drawing entity */

		if(drw_flag)
		{
			uig_transform_conic(&uniout, drw_v);
			uig_transform_conic(&uniout, drw_s);
			uig_transform_conic(&uniout, drw_t);
			dblk->view_ptr = drw_ptr;
		}
 		uig_update_attr(dblk);
		um_save_active_label(uniout.rel_num);
		create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
   	uig_create_geom(&uniout, 0, 0,dblk->view_ptr);

		if (UIG_nodups && !label_comp_element && 
				ncl_retrieve_data_fixed(&uniout) == UU_SUCCESS &&
				uig_match_conic(&uniout,0) == UU_SUCCESS)
		{
			uig_remove_dup(&uniout,key);
		}
		else
		{
			UIG_unibase_entities++;
			if(label_type ==8)
			{
/*
.....Label matching. Determine if exact match.
*/
				uig_exact_match(&uniout,uig_match_conic);
			}
			*key = uniout.key;
		}
		goto fexit;
	}
	else
	{
			/*jkd48: print and display errors*/
		sprintf(p_buff,"(DREC = %d) Error %d in conic endpoint definitions.\n",
			dblk->drec_num, status);
		uig_error(p_buff);
		*key = 0;
		goto fexit;
	}
fexit:;
	return(UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION     :  uig_map_poly2(dblk,igesin,t,key)
**				Map a IGES poly line to a set of Unibase lines.
**          vp 5-may-93 changed to NCL patern type 1 instead of
**          Unibase lines.
**    PARAMETERS   
**       INPUT  : 
**				dblk							IGES directory block
**				igesin						parameter block
**				t								IGES transformation matrix
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_poly2(dblk,igesin,t,key)
struct dir_rec *dblk;				/* IGES line directory record */
struct IG_poly2d_rec *igesin;	/* IGES line parameter record */
UU_REAL t[12];
UU_KEY_ID *key;
{
	int i,j,num;
	UU_REAL pt2[3],pt2_cc[3];
	UU_REAL *p;
	int uig_match_polyline();
	int uig_match_patern();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	/* retrieve data */

	if( dblk->form_no > 3 )
	{
		struct UM_polyline_rec uniout;

		num = igesin->num;
		p = igesin->pt2;
		pt2_cc[2] = igesin->zt;

		/* setup unibase storage area */
		ur_setup_app_data(UM_POLYLINE_REL,&uniout,sizeof(struct UM_polyline_rec));

		/*MILLS: initialize LABEL and SUBSCRIPT */
		strcpy(uniout.label,"");

		uniout.no_pt = 0;

		/* create line segments */
		for(i=0,j=0;i<num;i++,j++,j++)
		{
			/* adjust u value if periodic */
			if (bump_u && (period_u > 0.0) && (p[j] < starting_ang_u))  
			{
				while (p[j] < starting_ang_u) p[j] += period_u;
				while (p[j] > (starting_ang_u+period_u)) p[j] -= period_u;
			}
			/* adjust v value if periodic */
			if (bump_v && (period_v > 0.0) && (p[j+1] < starting_ang_v))  
			{
				while (p[j+1] < starting_ang_v) p[j+1] += period_v;
				while (p[j+1] > (starting_ang_v+period_v)) p[j+1] -= period_v;
			}

			pt2_cc[0] = p[j];
			pt2_cc[1] = p[j+1];
			uig_tran_coor(pt2_cc,t,pt2);			/* transform to model space */
			um_vctmsc(pt2,unit_scale,pt2);		/* scale */
			ur_update_app_data_varlist(&uniout, 1, pt2, uniout.no_pt+1, 1);
		}

		/* check if creating a drawing entity */

		if(drw_flag)
		{
			uig_transform_polyline(&uniout, drw_v);
			uig_transform_polyline(&uniout, drw_s);
			uig_transform_polyline(&uniout, drw_t);
			dblk->view_ptr = drw_ptr;
		}

		/* create unibase record */

		uig_update_attr(dblk);
		um_save_active_label(uniout.rel_num);
		create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
		uig_create_geom(&uniout,0,0,dblk->view_ptr);
		if (UIG_nodups && !label_comp_element && uig_match_polyline(&uniout,0) == UU_SUCCESS)
		{
			uig_remove_dup(&uniout,key);
		}
		else
		{

			UIG_unibase_entities++;
			if(label_type ==8)
			{
/*
.....Label matching. Determine if exact match.
*/
				uig_exact_match(&uniout,uig_match_polyline);
			}
			*key = uniout.key;
		}
	}
	else if (dblk->form_no == 1)
/*
...vp 1-may-93 changed Unibase output from Polyline to Patern
...when converting Copious Data Form 1 (x,y pairs, common z)
*/
	{
		struct NCL_patern_rec uniout;

       /* setup unibase storage area */
		ur_setup_app_data(NCL_PATERN_REL,&uniout,sizeof(struct NCL_patern_rec));
	
		strcpy(uniout.label,"");
		uniout.markertype = 2;
		uniout.pntype = 1;
		uniout.no_patpnt = 0;

/* retrieve data */

		num = igesin->num / 2;
		p = igesin->pt2;
		pt2_cc[2] = igesin->zt;

/* create line segments */

		j   = 0;
		for(i=0; i<num; i++)
		{
			/* adjust u value if periodic */
			if (bump_u && (period_u > 0.0) && (p[j] < starting_ang_u))  
			{
				while (p[j] < starting_ang_u) p[j] += period_u;
				while (p[j] > (starting_ang_u+period_u)) p[j] -= period_u;
			}
			/* adjust v value if periodic */
			if (bump_v && (period_v > 0.0) && (p[j+1] < starting_ang_v))  
			{
				while (p[j+1] < starting_ang_v) p[j+1] += period_v;
				while (p[j+1] > (starting_ang_v+period_v)) p[j+1] -= period_v;
			}

			pt2_cc[0] = p[j];
			pt2_cc[1] = p[j+1];
			uig_tran_coor(pt2_cc,t,pt2);			/* transform to model space */
			um_vctmsc(pt2,unit_scale,pt2);		/* scale */
			ur_update_app_data_varlist(&uniout, 1, pt2, j+1, 3);
			j   = j + 3;
		}

/* check if creating a drawing entity */

		if(drw_flag)
		{
			uig_transform_patern(&uniout, drw_v);
			uig_transform_patern(&uniout, drw_s);
			uig_transform_patern(&uniout, drw_t);
			dblk->view_ptr = drw_ptr;
		}

		uig_update_attr(dblk);
		um_save_active_label(uniout.rel_num);
		create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
		uig_create_geom(&uniout,0,0,dblk->view_ptr);
		UIG_unibase_entities++;

		if (UIG_nodups && !label_comp_element && uig_match_patern(&uniout,0) == UU_SUCCESS)
		{
			uig_remove_dup(&uniout,key);
		}
		else
		{
			if(label_type ==8)
			{
/*
.....Label matching. Determine if exact match.
*/
				uig_exact_match(&uniout,uig_match_patern);
			}
		*key = uniout.key;
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  uig_map_poly3(dblk,igesin,t,key)
**				Map a IGES poly line to a set of Unibase lines.
**          vp 1-may-93 changed Unibase output from Polyline to Patern
**          when converting Copious Data Form 2 (x,y,z triples)
**    PARAMETERS   
**       INPUT  : 
**				dblk							IGES directory block
**				igesin						parameter block
**				t								IGES transformation matrix
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_poly3(dblk,igesin,t,key)
struct dir_rec *dblk;				/* IGES line directory record */
struct IG_poly3d_rec *igesin;	/* IGES line parameter record */
UU_REAL t[12];
UU_KEY_ID *key;
{
	struct NCL_patern_rec uniout;

	int i,j,num;
	UU_REAL *p;
	UM_coord *pnpt, *pnptr = UU_NULL;
	char *uu_toolmalloc();
	int uig_match_patern();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

/* setup unibase storage area */
   ur_setup_app_data(NCL_PATERN_REL,&uniout,sizeof(struct NCL_patern_rec));

/*MILLS: initialize LABEL and SUBSCRIPT */
	strcpy(uniout.label,"");
	uniout.markertype = 2;
   uniout.pntype = 1;
	uniout.no_patpnt = 0;

/* retrieve data */

	num = igesin->no_pt3;
	p = igesin->pt3;
	pnptr = (UM_coord *) uu_toolmalloc (num * sizeof(UM_coord));
	pnpt  = pnptr;

/* create line segments */

   j   = 0;
	for(i=0; i<num; i++)
	{
		/* adjust u value if periodic */
		if (bump_u && (period_u > 0.0) && (p[j] < starting_ang_u))  
		{
			while (p[j] < starting_ang_u) p[j] += period_u;
			while (p[j] > (starting_ang_u+period_u)) p[j] -= period_u;
		}
		/* adjust v value if periodic */
		if (bump_v && (period_v > 0.0) && (p[j+1] < starting_ang_v))  
		{
			while (p[j+1] < starting_ang_v) p[j+1] += period_v;
			while (p[j+1] > (starting_ang_v+period_v)) p[j+1] -= period_v;
		}

		uig_tran_coor(&p[j],t,pnpt);			/* transform to model space */
		um_vctmsc(pnpt,unit_scale,pnpt);		/* scale */
		pnpt++;
      j   = j + 3;
	}
	ur_update_app_data_varlist(&uniout, 1, pnptr, 1, 3*num);

/* check if creating a drawing entity */

   if(drw_flag)
	{
		uig_transform_patern(&uniout, drw_v);
		uig_transform_patern(&uniout, drw_s);
		uig_transform_patern(&uniout, drw_t);   
		dblk->view_ptr = drw_ptr;
	}

/* create unibase record */

	uig_update_attr(dblk);
	um_save_active_label(uniout.rel_num);
	create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
	uig_create_geom(&uniout,0,0,dblk->view_ptr);

	if (UIG_nodups && !label_comp_element && uig_match_patern(&uniout,0) == UU_SUCCESS)
	{
		uig_remove_dup(&uniout,key);
	}
	else
	{
		UIG_unibase_entities++;
		if(label_type ==8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&uniout,uig_match_patern);
		}
		*key = uniout.key;
	}

	if (pnptr != UU_NULL) uu_toolfree (pnptr);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  uig_map_poly3crv(dblk,igesin,t,key)
**          Map a IGES poly line to a set of Unibase lines.
**          ** changed Unibase output from  Patern to a Unibase
**          rational B-spline when converting Copious Data Form 12
**				(x,y,z triples representing a curve)
**    PARAMETERS
**       INPUT  :
**          dblk                    IGES directory block
**          igesin                  parameter block
**          t                       IGES transformation matrix
**       OUTPUT :
**          key                     Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_map_poly3crv(dblk,igesin,t,key)
struct dir_rec *dblk;            /* IGES line directory record */
struct IG_poly3d_rec *igesin; /* IGES line parameter record */
UU_REAL t[12];
UU_KEY_ID *key;
{
	int i, ns, num, npts, status;
	UU_REAL *p, *s, *pts;
	struct NCL_crvgen_rec seg, *segp;
	UU_LIST seglist;
	UM_coord opt, pt;
   struct UM_rbsplcrv_rec uniout;
	int uig_match_rbsplcrv();

	status = UU_SUCCESS;
	s = pts = UU_NULL;
	npts = 0;
   ur_setup_data(UM_RBSPLCRV_REL, &uniout, sizeof(struct UM_rbsplcrv_rec));

/*
.....initialize label and Subscript.
*/
	strcpy(uniout.label,"");
	
/* 
.....retrieve data
*/
   num = igesin->no_pt3;
   p = igesin->pt3;
	uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), 20, 20);
/*
..... rb-spline interpolator
*/
   for (i=0; i<num*3; i+=3)
   {
      pt[0] = p[i];
      pt[1] = p[i+1];
      pt[2] = p[i+2];
		seg.x = p[i];
		seg.y = p[i+1];
		seg.z = p[i+2];
      seg.inv = 0;
      if (i>0)
         if (um_dcccc(opt, pt) < UM_EQPARM)
            status = UU_FAILURE;
		if (status == UU_SUCCESS)
		{
   		uu_list_push (&seglist, &seg);
   		um_vctovc (pt, opt);
			npts++;
		}
		status = UU_SUCCESS;
	}
/*
.....interpolate rbspline
*/
   segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);
   if (status == UU_SUCCESS)
   {
      status = uig_ncl_interp_rbsp1 (npts, segp, 0, &ns, &s, &pts);
/*
.....store curve in unibase
*/
      if (status == UU_SUCCESS)
      {
			status = uig_create_rbsp (ns, s, pts, &uniout);
        	ncl_retrieve_data_fixed(&uniout);
      }
   }
/* 
.....check if creating a drawing entity 
*/
	if(drw_flag) dblk->view_ptr = drw_ptr;
	uig_update_attr(dblk);
	um_save_active_label(uniout.rel_num);
	create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
	uig_create_geom(&uniout, 0, 0,dblk->view_ptr);
	if (UIG_nodups && !label_comp_element &&
		ncl_retrieve_data_fixed(&uniout) == UU_SUCCESS &&
		uig_match_rbsplcrv(&uniout,0) == UU_SUCCESS)
	{
		uig_remove_dup(&uniout,key);
	}
	else
	{

		UIG_unibase_entities++;
		if (label_type == 8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&uniout,uig_match_rbsplcrv);
		}
		*key = uniout.key;
	}

/*
.....free memory used for interpolator
*/
   uu_list_free (&seglist);
   uig_ncl_free_uv();
   if (s != UU_NULL) uu_free(s);
	if (pts != 0) uu_free(pts);

   return (0);
}


/*********************************************************************
**    I_FUNCTION     :  uig_map_poly6(dblk,igesin,t,key)
**				Map a IGES Copious Data Form 3 (x,y,z,i,j,k's) to NCL
**          Patern of PVs (Patern type 2). Single sixtuple input
**          map to NCL Point-vector.
**    PARAMETERS   
**       INPUT  : 
**				dblk							IGES directory block
**				igesin						parameter block
**				t								IGES transformation matrix
**       OUTPUT :  
**				key							Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_poly6(dblk,igesin,t,key)
struct dir_rec *dblk;				/* IGES line directory record */
struct IG_poly6d_rec *igesin;	/* IGES line parameter record */
UU_REAL t[12];
UU_KEY_ID *key;
{

	struct IG_pt6_rec *ptr6;
	UM_coord *pnpt;
	int temp_rel_type,i,num;
	UU_REAL pt1[3],pt2[3];
	char *uu_toolmalloc();
	int uig_match_patern();
	int uig_match_pointvec();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

/* retrieve data */

	num = igesin->no_pt6;
	ptr6 = igesin->pt6;
/*
...Convert to ncl PointVector structure
*/ 
   if (num == 1) 
	{
		struct NCL_nclpv_rec uniout;

/* setup unibase storage area */

		ur_setup_app_data(NCL_POINTVEC_REL,&uniout,sizeof(struct NCL_nclpv_rec));

		strcpy(uniout.label,"");

		uig_tran_coor(&ptr6->pt[0],t,pt1);			/* transform to model space */
		um_vctmsc(pt1,unit_scale,pt1);		/* scale */
		uig_tran_coor(&ptr6->vec[0],t,pt2);			/* transform to model space */
		um_vctmsc(pt2,unit_scale,pt2);		/* scale */
		um_vctovc(pt1,&uniout.pt[0]);		/* point to output structure */
		um_vctovc(pt2,&uniout.ve[0]);		/* vector to output structure */

		if(drw_flag)
		{
			uig_transform_nclpv(&uniout, drw_v);
			uig_transform_nclpv(&uniout, drw_s);
			uig_transform_nclpv(&uniout, drw_t);
			dblk->view_ptr = drw_ptr;
		}
		if (UIG_nodups && !label_comp_element && uig_match_pointvec(&uniout,0) == UU_SUCCESS)
		{
			*key = 0;
			UIG_dupcount++;
		}
		else
		{

/* create unibase record */

			uig_update_attr(dblk);
			create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
			uig_create_geom(&uniout,0,0,dblk->view_ptr);
			UIG_unibase_entities++;
			if(label_type ==8)
			{
/*
.....Label matching. Determine if exact match.
*/
				uig_exact_match(&uniout,uig_match_pointvec);
			}
			*key = uniout.key;
		}
	}
/*
...Convert to ncl Patern type 2 (PVs structure)
*/ 
   else
	{
		struct NCL_patern_rec uniout;
		UM_coord *pnptr = UU_NULL;

/* setup unibase storage area */
		ur_setup_app_data(NCL_PATERN_REL,&uniout,sizeof(struct NCL_patern_rec));

		strcpy(uniout.label,"");
/* 
.....assigning the color as the default color for pattern
*/
		if( UIG_color_iges == 0)
			dblk->pen_no = UM_YELLOW;
		uniout.markertype = 2;
		uniout.pntype = 2;
		uniout.no_patpnt = 0;
		pnptr = (UM_coord *) uu_toolmalloc (2*num * sizeof(UM_coord));
		pnpt  = pnptr;
		for(i=0; i<num; i++)
		{
			uig_tran_coor(&ptr6->pt[0],t,pnpt);         /* transform to model space */
			um_vctmsc(pnpt,unit_scale,pnpt);       /* scale */
			pnpt++;
			uig_tran_coor(&ptr6->vec[0],t,pnpt);         /* transform to model space */
			um_vctmsc(pnpt,unit_scale,pnpt);       /* scale */
			pnpt++;
			ptr6++;
		}
		ur_update_app_data_varlist(&uniout, 1, pnptr, 1, 2*3*num);

/* check if creating a drawing entity */

		if (drw_flag)
		{
			uig_transform_patern(&uniout, drw_v);
			uig_transform_patern(&uniout, drw_s);
			uig_transform_patern(&uniout, drw_t);
			dblk->view_ptr = drw_ptr;
		}

/* create unibase record */

		uig_update_attr(dblk);
/*
.....Changing dblk->rel_type to GPOLY3D just to create a pattern PN label 
.....After the label is created it is assigned to its original GPOLY6D value.
*/ 
		temp_rel_type = dblk->rel_type;
		dblk->rel_type = 606;
		um_save_active_label(uniout.rel_num);
		create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
		dblk->rel_type = temp_rel_type;
		uig_create_geom(&uniout,0,0,dblk->view_ptr);

		if (UIG_nodups && !label_comp_element && uig_match_patern(&uniout,0) == UU_SUCCESS)
		{
			uig_remove_dup(&uniout,key);
		}
		else
		{
			UIG_unibase_entities++;
			if(label_type ==8)
			{
/*
.....Label matching. Determine if exact match.
*/
				uig_exact_match(&uniout,uig_match_patern);
			}
			*key = uniout.key;
		}
		if (pnptr != UU_NULL) uu_toolfree (pnptr);
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  uig_map_poly6crv(dblk,igesin,t,key)
**          Map a IGES poly line to a set of Unibase lines.
**          ** changed Unibase output from  Patern to a Unibase
**          rational B-spline when converting Copious Data Form 13
**				(x,y,z,i,j,k sextuples representing a curve)
**    PARAMETERS
**       INPUT  :
**          dblk                    IGES directory block
**          igesin                  parameter block
**          t                       IGES transformation matrix
**       OUTPUT :
**          key                     Unibase key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_map_poly6crv(dblk,igesin,t,key)
struct dir_rec *dblk;            /* IGES line directory record */
struct IG_poly6d_rec *igesin; /* IGES line parameter record */
UU_REAL t[12];
UU_KEY_ID *key;
{
	int i, ns, num, npts, status;
	UU_REAL *p, *s, *pts, *vec;
	struct NCL_crvgen_rec seg, *segp;
	UU_LIST seglist;
	UM_coord opt, pt;
   struct UM_rbsplcrv_rec uniout;
	int uig_match_rbsplcrv();

	status = UU_SUCCESS;
	s = pts = UU_NULL;
	npts = 0;
   ur_setup_data(UM_RBSPLCRV_REL, &uniout, sizeof(struct UM_rbsplcrv_rec));

/*
.....initialize label and Subscript.
*/
	strcpy(uniout.label,"");
	
/* 
.....retrieve data
*/
   num = igesin->no_pt6;
	uu_list_init (&seglist, sizeof(struct NCL_crvgen_rec), 20, 20);
/*
..... rb-spline interpolator
*/
   for (i=0; i<num; i++)
   {
      p = igesin->pt6[i].pt;
		vec = igesin->pt6[i].vec;
		seg.x = p[0];
		seg.y = p[1];
		seg.z = p[2];
		seg.dx = vec[0];
		seg.dy = vec[1];
		seg.dz = vec[2];

      seg.inv = 0;
		pt[0] = p[0];
		pt[1] = p[1];
		pt[2] = p[2];	
      if (i>0)
         if (um_dcccc(opt, pt) < UM_EQPARM)
            status = UU_FAILURE;
		if (status == UU_SUCCESS)
		{
   		uu_list_push (&seglist, &seg);
   		um_vctovc (pt, opt);
			npts++;
		}
		status = UU_SUCCESS;
	}
/*
.....interpolate rbspline
*/
   segp = (struct NCL_crvgen_rec *) UU_LIST_ARRAY (&seglist);
   if (status == UU_SUCCESS)
   {
      status = uig_ncl_interp_rbsp1 (npts, segp, 0, &ns, &s, &pts);
/*
.....store curve in unibase
*/
      if (status == UU_SUCCESS)
      {
			status = uig_create_rbsp (ns, s, pts, &uniout);
        	ncl_retrieve_data_fixed(&uniout);
      }
   }
/* 
.....check if creating a drawing entity 
*/
	if(drw_flag) dblk->view_ptr = drw_ptr;
	uig_update_attr(dblk);
	create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
	um_save_active_label(uniout.rel_num);
	uig_create_geom(&uniout, 0, 0,dblk->view_ptr);
	if (UIG_nodups && !label_comp_element &&
		ncl_retrieve_data_fixed(&uniout) == UU_SUCCESS &&
		uig_match_rbsplcrv(&uniout,0) == UU_SUCCESS)
	{
		uig_remove_dup(&uniout,key);
	}
	else
	{

		UIG_unibase_entities++;
		if (label_type == 8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&uniout,uig_match_rbsplcrv);
		}
		*key = uniout.key;
	}

/*
.....free memory used for interpolator
*/
   uu_list_free (&seglist);
   uig_ncl_free_uv();
   if (s != UU_NULL) uu_free(s);
	if (pts != 0) uu_free(pts);

   return (0);
}


/*********************************************************************
**    I_FUNCTION     :  uig_map_spline(dblk,igesin,t,key)
**       Map an IGES parametric bspline to a unibase rational bspline.
**    PARAMETERS 
**       INPUT  : 
**            dblk                    IGES directory block
**            igesin                  IGES P-spline structure
**            t                       associated matrix
**       OUTPUT :  
**            key                     UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_map_spline(dblk,igesin,t,key)
struct dir_rec *dblk;            /* IGES parametric spline dir record */
struct IG_igesplin_rec *igesin;  /* IGES parametric spline para record */
UU_REAL    t[12];
UU_KEY_ID *key;
{
	int status, i, j, n, num;
	struct UM_rbsplcrv_rec uniout;
	struct IG_coef_rec *p_coef;
	UU_REAL pt[4][3], spt[3], ept[3], sderv[3], ederv[3];
	UU_REAL one_third;
	double *trbs;
	double *pt1;
	int ix;
	int nsegs, npts;
	char *uu_toolmalloc();
	int uig_match_rbsplcrv();

	ur_setup_data(UM_RBSPLCRV_REL, &uniout, sizeof(struct UM_rbsplcrv_rec));

	if (igesin->ndim == 2)
		uniout.planar = UU_TRUE;
	else
		uniout.planar = UU_FALSE;

	nsegs = igesin->n_seg;
	npts  = nsegs*3+1;
	trbs = (double *) uu_toolmalloc ((npts+4)*sizeof(double));
	pt1 = (double *) uu_toolmalloc (npts*3*sizeof(double));

	trbs[0] = igesin->t[0];
	for (i=0,ix=1; i<nsegs+1; i++,ix=ix+3) 
		trbs[ix] = trbs[ix+1] = trbs[ix+2] =  igesin->t[i];
	trbs[ix] = trbs[ix-1];

	uniout.open = UU_FALSE;
	uniout.closdinu = 0;
	uniout.k = 4;
	uniout.n = npts-3;         /* number of spans */
	uniout.t0 = trbs[0];       /* starting value  */
	uniout.t1 = trbs[ix];      /* ending value    */

	p_coef = igesin->coef;
	ix = 0;
	for(n=0;n<nsegs;n++)
	{
		uig_eval_cubic(igesin->t[n], igesin->t[n+1], p_coef[n].cx,
					p_coef[n].cy, p_coef[n].cz, spt, sderv, ept, ederv);
		one_third = (igesin->t[n+1] - igesin->t[n]) / 3.0;

/* transform to model space */

		uig_tran_coor(spt,t,spt);
		um_vctmsc(spt,unit_scale,spt);
		uig_tran_coor(ept,t,ept);
		um_vctmsc(ept,unit_scale,ept);
		uig_tran_vec(sderv,t,sderv);
		um_vctmsc(sderv,unit_scale,sderv);
		uig_tran_vec(ederv,t,ederv);
		um_vctmsc(ederv,unit_scale,ederv);

      /* create nurb control points */

		um_vctovc(spt, pt[0]);
		um_vctmsc(sderv, one_third, pt[1]);
		um_vcplvc(spt, pt[1], pt[1]);
		um_vctmsc(ederv, one_third, pt[2]);
		um_vcmnvc(ept, pt[2], pt[2]);
		for(j=0;j<3;j++)
			for(i=0;i<3;i++,ix++) pt1[ix] = pt[j][i];
		if (n == nsegs - 1) um_vctovc (ept, &pt1[ix]);
	}

      /* check if creating a drawing entity */
	if(drw_flag) dblk->view_ptr = drw_ptr;
	uig_update_attr(dblk);
	um_save_active_label(uniout.rel_num);
	create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
	uig_create_geom(&uniout, 0, 0,dblk->view_ptr);

	num = npts+4;
	status = ur_update_data_varlist(uniout.key, 1, trbs, 1, num);
/*
	for (i=0;i<npts;i++) trbs[i] = 1.0;
	status = ur_update_data_varlist(uniout.key, 3, trbs, 1, npts);
*/
	uniout.no_wt = 0; uniout.wt = UU_NULL;
	if(drw_flag)
	{
		for (i = 0, j = 0; i < npts; i++, j = j+3)
		{
			uig_transform_array(&pt1[j], drw_v);
			uig_transform_array(&pt1[j], drw_s);
			uig_transform_array(&pt1[j], drw_t);
		}
	}
	status = ur_update_data_varlist(uniout.key, 2, pt1, 1, npts);

	if (UIG_nodups && !label_comp_element &&
		ncl_retrieve_data_fixed(&uniout) == UU_SUCCESS &&
		uig_match_rbsplcrv(&uniout,0) == UU_SUCCESS)
	{
		uig_remove_dup(&uniout,key);
	}
	else
	{

		UIG_unibase_entities++;
		if (label_type == 8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&uniout,uig_match_rbsplcrv);
		}
		*key = uniout.key;
	}

	uu_toolfree(trbs);
	uu_toolfree(pt1);
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  uig_eval_cubic(t0, t1, cx, cy, cz, spt,
**															sderv, ept, ederv)
**				Evaluate IGES parametric spline segment1
**
**    PARAMETERS   
**       INPUT  : 
**				t0								initial knot value
**				t1								final knot value 
**				cx, cy, cz					cubic coeficients for current segment
**       OUTPUT :  
**				spt							start point
**				sderv							derivative at start point
**				ept							end point
**				ederv							derivative at end point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_eval_cubic(t0, t1, cx, cy, cz, spt,
						sderv, ept, ederv)
	UU_REAL  t0, t1, cx[4], cy[4], cz[4], spt[3], sderv[3], ept[3], ederv[3];
	{
	UU_REAL  s1, s2, s3;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	s1 = t1 - t0;
	s2 = s1 * s1;
	s3 = s2 * s1;

	spt[0] = cx[0];
	spt[1] = cy[0];
	spt[2] = cz[0];

	ept[0] = cx[0] + cx[1] * s1 + cx[2] *s2 + cx[3] *s3;
	ept[1] = cy[0] + cy[1] * s1 + cy[2] *s2 + cy[3] *s3;
	ept[2] = cz[0] + cz[1] * s1 + cz[2] *s2 + cz[3] *s3;

	sderv[0] = cx[1];
	sderv[1] = cy[1];
	sderv[2] = cz[1];

	ederv[0] = cx[1] + 2.0 *cx[2] *s1 + 3.0 * cx[3] *s2;
	ederv[1] = cy[1] + 2.0 *cy[2] *s1 + 3.0 * cy[3] *s2;
	ederv[2] = cz[1] + 2.0 *cz[2] *s1 + 3.0 * cz[3] *s2;
	}

/*********************************************************************
**    I_FUNCTION     :  uig_create_comp(dblk,igesin,num, list,
**                                      comp_type,sense, key)
**       Create a composite curve from splines.
**
**    PARAMETERS
**       INPUT  :
**          dblk      - directory record
**          igesin    - parameter record
**          num       - number of curves
**          list      - list of spline key-id's
**          comp_type - planar flag.
**          sense     - array of reverse flags, 2=reverse, 1 or null don't
**       OUTPUT :
**          key    - comp curve key
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_create_comp(dblk, igesin, num, list, comp_type, sense, key)
struct dir_rec *dblk;				/* IGES directory record */
struct IG_igescomp_rec *igesin;
int			num;
UU_KEY_ID	list[50];
UU_LOGICAL  comp_type;
int *sense;
UU_KEY_ID	*key;
{
	int i, rev, test_flags[4];
	UU_REAL length;
    UM_coord fpt, lpt, start_pt, end_pt, temp_pt;
	struct UM_compcrv_rec	comptr;
	struct UM_cid_rec *lcid;
	char *uu_toolmalloc();
	UU_REAL um_getarclen(),arclen;
	int uc_init_evcrvout();
	int uig_match_compcrv();
	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	/* check if creating a drawing entity */
	if(drw_flag)
	{
		dblk->view_ptr = drw_ptr;
	}

	ur_setup_data(UM_COMPCRV_REL, &comptr, sizeof(struct UM_compcrv_rec));
	lcid = (struct UM_cid_rec *)uu_toolmalloc(num*sizeof(struct UM_cid_rec));

	/*MILLS: initialize LABEL and SUBSCRIPT */
	strcpy(comptr.label,"");

	comptr.arclen = 0;
	comptr.planar = comp_type;
	comptr.open   = UU_TRUE;
	comptr.continuity = 0;
	comptr.fcolor = UM_BACKGROUND;
	comptr.t0 = 0.;
	comptr.t1 = 1.;
	comptr.addflg = 0;

    for(i=0;i<num;i++)
    {
/*
..... For each subcurve, first, make sure it exists (list[i] > 0).
..... After that, make sure that the composite curve is continuous;
..... in other words, the last point on the previous subcurve should
..... be close to the first point on the current subcurve (subcurves
..... should be joined, end-to-end).  Finally, fill in the data
..... structure.
*/
previous:;
        if (list[i] > 0)
        {
            UU_REAL start_u, end_u, tol;
            struct NCL_fixed_databag crv;
            struct UM_evcrvout evout;
            UM_transf tfmat;
/* 
.....Use this value of tolerance stored in UIG_comp_tol only if this 
.....composite curve is a part of a trimmed surface.
.....Himani.
*/
    			if(UIG_from_trimsrf)
					tol = UIG_comp_tol;
				else 
					tol = 0.008;

            crv.key = list[i];
            ncl_retrieve_data_fixed (&crv);
            uc_retrieve_transf (list[i],tfmat);
            uc_init_evcrvout (&crv,&evout);

            start_u = 0.0;
            end_u   = 1.0;
            uc_evcrv (UM_POINT, start_u, &crv, tfmat, &evout);
            um_vctovc(evout.cp, start_pt);
            uc_evcrv (UM_POINT, end_u, &crv, tfmat, &evout);
            um_vctovc(evout.cp, end_pt);
/*
.....If the last component is a point ignore it and recalculate the orientation
.....of the previous component ,treating it as the last component.
.....
.....This logic was changed to use the arc length instead of pt distance
.....since the first curve could be a point and the second a valid
.....closed curve (FSR 61482)
.....Bobby - 9/6/12
*/ 
            cons.key_id = list[i];
            ur_retrieve_data_relnum(cons.key_id, &cons.rel_num);	/* cpp */
            uio_getdata(&cons, &attr);
            arclen = um_getarclen(&cons, UU_NULL);
				if((i>0) && (i==num-1) && arclen<=UM_FUZZ)
				{
					num--;
					i--;
					goto previous;
				}

            if (i == 0)
            {
/*
..... For first subcurve, don't care about its starting point
..... being close to anything.  Just get the endpoint.
*/
               um_vctovc(start_pt, fpt);
               um_vctovc(end_pt, lpt);

               lcid[i].reverse = UU_FALSE;
            }
            if (i > 0)
            {
               rev = uig_compcv_contin_check(fpt, lpt, start_pt, \
                                             end_pt, test_flags, tol);

               if (rev == -1)
               {
/*
..... Both endpoints of both 1st and 2nd curves are too far away.
..... The curves are separated (not continuous).  Can't make a
..... smooth composite curve using them.
*/
                  *key = 0;
                  return(UU_FAILURE);
               }
               if (i == 1)
               {
                  if ((rev == 3) || (rev == 4))
                  {
/*
..... The first subcurve is reversed.  Change the orientation of
..... composite curve (first point <-> last point).
*/
                     lcid[0].reverse = UU_TRUE;
                     um_vctovc(fpt, temp_pt);
                     um_vctovc(lpt, fpt);
                     um_vctovc(temp_pt, lpt);
                  }
               }
/*
..... Current subcurve will need to be reversed if rev = 2 or 4.
*/
               rev = rev % 2;
               if (rev == 1)
               {
/*
..... If current subcurve is the last subcurve and if rev = 3 and if End of
..... subcurve is within tol of end of comp then reverse the subcurve.
*/

					if((i == (num-1))&&(test_flags[0]== 0)&&(test_flags[1]==1)
						&&(test_flags[2]== 1)&&(test_flags[3]==0))
						lcid[i].reverse = UU_TRUE;
					else
					{
						lcid[i].reverse = UU_FALSE;
						um_vctovc(end_pt, lpt);
					}
               }
               else
               {
                  lcid[i].reverse = UU_TRUE;
                  um_vctovc(start_pt, lpt);
               }
/*
..... Last curve of composite.  Make sure composite is not just
..... 2 subcurves.  It's possible to say that this subcurve and
..... the rest of the composite should both be reversed (if it
..... is closed -> just go around the comp_cv backwards).  But
..... then only reverse this last subcurve.  Must check to see
..... if can also go in non-reversed direction.
*/
               if ((i > 1) && (i == (num-1)))
               {
                  if (test_flags[0] == 1)
                     lcid[i].reverse = UU_FALSE;
               }

            }

            lcid[i].crvid = list[i];
            lcid[i].endparam = arclen;
            comptr.arclen = comptr.arclen + lcid[i].endparam;
        }
    }
    if (comptr.arclen == 0.)
    {
        *key = 0;
    }
    else
    {
		length = 0.0;
		for(i=0;i<num;i++)
		{
			length = length + lcid[i].endparam;
			lcid[i].endparam = length/comptr.arclen;
		}

		um_save_active_label(comptr.rel_num);
		create_label(dblk, igesin->no_prop, igesin->prop, comptr.label, &comptr.subscr);
		uig_create_geom(&comptr, 0, 0,dblk->view_ptr);
		ur_update_data_varlist (comptr.key, 1, lcid, 1, num);

		if (UIG_nodups && !label_comp_element && 
			ncl_retrieve_data_fixed(&comptr) == UU_SUCCESS &&
			uig_match_compcrv(&comptr,0) == UU_SUCCESS)
		{
			uig_remove_dup(&comptr,key);
		}
		else
		{

			UIG_unibase_entities++;
			if (label_type == 8)
			{
/*
.....Label matching. Determine if exact match.
*/
				uig_exact_match(&comptr,uig_match_compcrv);
			}
			*key = comptr.key;
		}
	}

	if (lcid != 0) uu_toolfree(lcid);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  uig_map_rbspl(dblk,igesin,t,key)
**            Map an IGES rational bspline to a unibase rational bspline.
**    PARAMETERS   
**       INPUT  : 
**            dblk                     IGES directory block
**            igesin                   IGES arc structure
**            t                        associated matrix
**       OUTPUT :  
**            key                     UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_map_rbspl(dblk,igesin,t,key)
struct dir_rec *dblk;             /* IGES rational bspline directory record */
struct IG_igesrspl_rec *igesin;   /* IGES rational bspline parameter record */
UU_REAL    t[12];
UU_KEY_ID *key;
{
	if (uvcv_flag) 
		uig_map_uvrbspl (dblk,igesin,t,key);
	else
		uig_map_rbspl1 (dblk,igesin,t,key);
}
/*********************************************************************
**    I_FUNCTION     :  uig_map_rbspl1(dblk,igesin,t,key)
**            Map an IGES rational bspline to a unibase rational bspline.
**    PARAMETERS   
**       INPUT  : 
**            dblk                     IGES directory block
**            igesin                   IGES arc structure
**            t                        associated matrix
**       OUTPUT :  
**            key                     UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_rbspl1(dblk,igesin,t,key)
struct dir_rec *dblk;             /* IGES rational bspline directory record */
struct IG_igesrspl_rec *igesin;   /* IGES rational bspline parameter record */
UU_REAL    t[12];
UU_KEY_ID *key;
{

   int i, j, num;
   UU_REAL *p;
   struct UM_rbsplcrv_rec uniout;
	UU_REAL *tparm, *wght;
	int uig_match_rbsplcrv();

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   ur_setup_data(UM_RBSPLCRV_REL, &uniout, sizeof(struct UM_rbsplcrv_rec));

   uniout.key = 0;
   if (igesin->planar == 1)
      uniout.planar = UU_TRUE;
   else
      uniout.planar = UU_FALSE;

   if (igesin->open == 1)
      uniout.open = UU_FALSE;
   else
      uniout.open = UU_TRUE;

   uniout.closdinu = 0;
   uniout.k = igesin->degree + 1;		/* order of the bspline curve */
   uniout.n = igesin->no_pt3 - igesin->degree;			/* number of spans */
   uniout.t0 = igesin->rpara[0].t0;      /* starting value  */
   uniout.t1 = igesin->rpara[0].t1;      /* ending value    */

      /* check if creating a drawing entity */

   if(drw_flag) dblk->view_ptr = drw_ptr;

   tparm = igesin->t;
   wght = igesin->w;
   num = igesin->no_pt3;
   p = igesin->pt3;
   for (i = 1, j = 0; i <= num; i++, j = j+3)
   {
      uig_tran_coor(&p[j], t, &p[j]);
      um_vctmsc(&p[j], unit_scale, &p[j]);
      if(drw_flag)
      {
/*
.....Call uig_transform_array instead of uig_tran_coor, this
.....was the wrong tranformation routine to call here.
*/
         uig_transform_array(&p[j],drw_v);
         uig_transform_array(&p[j],drw_s);
         uig_transform_array(&p[j],drw_t);
       }
/*
..... if this is a boundary on a surface of revolution with a circle as
..... generatrix, the circle's angles give the surface U-range, and we 
..... translate the angles as positive - so add TWOPI to negative 
..... u-coordinates
*/
       /* adjust u value if periodic */
       if (bump_u && (period_u > 0.0) && (p[j] < starting_ang_u))  
       {
           while (p[j] < starting_ang_u) p[j] += period_u;
           while (p[j] > (starting_ang_u+period_u)) p[j] -= period_u;
       }
       /* adjust v value if periodic */
       if (bump_v && (period_v > 0.0) && (p[j+1] < starting_ang_v))  
       {
           while (p[j+1] < starting_ang_v) p[j+1] += period_v;
           while (p[j+1] > (starting_ang_v+period_v)) p[j+1] -= period_v;
       }
   }
   uig_update_attr(dblk);
	um_save_active_label(uniout.rel_num);
	create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
   uig_create_geom(&uniout, 0, 0,dblk->view_ptr);
	ur_update_data_varlist(uniout.key, 1, tparm, 1, igesin->no_t);
	ur_update_data_varlist(uniout.key, 2, p, 1, igesin->no_pt3);
	if (igesin->type == 0)
	{
		igesin->type = 1;
		for (i = 0; i < igesin->no_w && igesin->type == 1; i++)
			if (wght[i] != 1.) igesin->type = 0;
	}
	if (igesin->type == 0)
		ur_update_data_varlist(uniout.key, 3, wght, 1, igesin->no_w);

	if (UIG_nodups && !label_comp_element &&
		ncl_retrieve_data_fixed(&uniout) == UU_SUCCESS &&
		uig_match_rbsplcrv(&uniout,0) == UU_SUCCESS)
	{
		uig_remove_dup(&uniout,key);
	}
	else
	{
		UIG_unibase_entities++;
		if(label_type ==8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&uniout,uig_match_rbsplcrv);
		}
   *key = uniout.key;
	}
	return(UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION     :  uig_map_uvrbspl(dblk,igesin,t,key)
**            Map an IGES rational bspline to a unibase rational bspline.
**    PARAMETERS   
**       INPUT  : 
**            dblk                     IGES directory block
**            igesin                   IGES arc structure
**            t                        associated matrix
**       OUTPUT :  
**            key                     UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_map_uvrbspl(dblk,igesin,t,key)
struct dir_rec *dblk;             /* IGES rational bspline directory record */
struct IG_igesrspl_rec *igesin;   /* IGES rational bspline parameter record */
UU_REAL    t[12];
UU_KEY_ID *key;
{

   int i, j, num;
   UU_REAL *p = UU_NULL;
   UU_REAL *tparm = UU_NULL,*wght = UU_NULL;
   struct UM_uvcvonsf_rec uniout;
	int uig_match_uvcvonsf();

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   ur_setup_data (UM_UVCVONSF_REL, &uniout, sizeof(struct UM_uvcvonsf_rec));

   uniout.key = 0;
	uniout.labloc[0]=0.;
	uniout.labloc[1]=0.;
	uniout.labloc[2]=0.;
   if (igesin->planar == 1)
      uniout.planar = UU_TRUE;
   else
      uniout.planar = UU_FALSE;

   if (igesin->open == 1)
      uniout.open = UU_FALSE;
   else
      uniout.open = UU_TRUE;

   uniout.bskey = 0;
   uniout.bskey = tig_uvcvonsf_bskey;
   uniout.closdinu = 0;
   uniout.k = igesin->degree + 1;		/* order of the bspline curve */
   uniout.n = igesin->no_pt3 - igesin->degree;			/* number of spans */
   uniout.t0 = igesin->rpara[0].t0;      /* starting value  */
   uniout.t1 = igesin->rpara[0].t1;      /* ending value    */
   uniout.no_t = 0;
   uniout.no_pt = 0;
   uniout.no_wt = 0;
   uniout.no_displst = 0;

/* 
.....check if creating a drawing entity 
*/

   if(drw_flag) dblk->view_ptr = drw_ptr;
	tparm = igesin->t;
	wght = igesin->w;

   num = igesin->no_pt3;
   p = igesin->pt3;
   for (i = 1, j = 0; i <= num; i++, j = j+3)
   {
      uig_tran_coor(&p[j], t, &p[j]);
   }
   uig_update_attr(dblk);
	um_save_active_label(uniout.rel_num);
	create_label(dblk, igesin->no_prop, igesin->prop, uniout.label, &uniout.subscr);
   uig_create_geom(&uniout, 0, 0,dblk->view_ptr);
   ur_update_data_varlist(uniout.key, 1, tparm, 1, igesin->no_t);
   ur_update_data_varlist(uniout.key, 2, p, 1, num);
   ur_update_data_varlist(uniout.key, 3, wght, 1, igesin->no_w);
	if (UIG_nodups && !label_comp_element &&
		ncl_retrieve_data_fixed(&uniout) == UU_SUCCESS &&
		uig_match_uvcvonsf(&uniout,0) == UU_SUCCESS)
	{
		uig_remove_dup(&uniout,key);
	}
	else
	{

		UIG_unibase_entities++;
		if (label_type ==8)
		{
/*
.....Label matching. Determine if exact match.
*/
			uig_exact_match(&uniout,uig_match_uvcvonsf);
		}
		*key = uniout.key;
	}
	return(UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION     :  uig_map_group(no_ptr, ptr, dblk, key)
**   			Complete generation of group entity
**    PARAMETERS   
**       INPUT  : 
**				no_ptr						number of KEY_ID's in ptr
**				ptr							pointer array
**          dblk							directory blk for the group
**       OUTPUT :  
**				key							UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_group(no_ptr,ptr, dblk, key)
	int       no_ptr;
	UU_KEY_ID ptr[500];
	struct dir_rec *dblk;				/* IGES group directory record */
	UU_KEY_ID *key;
	{

	struct UM_grouper_rec uniout;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	/* check if creating a drawing entity */

	if(drw_flag)
		{
		dblk->view_ptr = drw_ptr;
		}


	/* setup unibase storage area */

	ur_setup_data(UM_GROUP_REL,&uniout,sizeof(struct UM_grouper_rec));

/*	uniout.no_member = no_ptr;*/
/*	for(i=0;i<no_ptr;i++)*/
/*		{*/
/*		uniout.member[i] = ptr[i];*/
/*		}*/
	/* now create unibase record */

	uig_update_attr(dblk);
	uig_create_geom(&uniout,0,0,dblk->view_ptr);
	ur_update_data_varlist (uniout.key, 1, ptr, 1, no_ptr);
	UIG_unibase_entities++;

	*key = uniout.key;
	return(UU_SUCCESS);
}


