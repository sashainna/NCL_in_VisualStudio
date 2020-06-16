/*********************************************************************
**    NAME         :  tigdisp.c
**       CONTAINS:
**					uig_in_dispat
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigdisp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:45
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "mcrv.h"
#include "udebug.h"

extern int process_normal;

extern int uig_in_splinesrf();
extern int uig_in_plane();
extern int uig_in_line();
extern int uig_in_point();
extern int uig_in_arc();
extern int uig_in_conic();
extern int uig_in_comp();
extern int uig_in_group();
extern int uig_in_viewvs();
extern int uig_in_plnassoc();
extern int uig_in_poly2();
extern int uig_in_poly3();
extern int uig_in_poly6();
extern int uig_in_spline();
extern int uig_in_rspline();
extern int uig_in_offstsrf();
extern int uig_in_crvonsrf();
extern int uig_in_trimsrf();
extern void uig_in_gnote();
extern int uig_in_angdim();
extern int uig_in_diadim();
extern int uig_in_leader();
extern int uig_in_lindim();
extern int uig_in_radim();
extern int uig_in_label();
extern int uig_in_gsymbol();
extern int uig_in_instance();
extern void uig_in_view();
extern void uig_in_drawing();
extern int uig_in_rsplsrf();
extern int uig_in_ruledsrf();
extern int uig_in_revsrf();
extern int uig_in_tbcysrf();
extern int uig_in_bdsrf();
extern int uig_in_solid();
extern int uig_in_verlist();
extern int uig_in_edglist();
extern int uig_in_loop();
extern int uig_in_face();
extern int uig_in_shell();
extern int uig_in_color();

/*********************************************************************
**    I_FUNCTION     :  uig_in_dispat(gblk,dblk,c,key)
**				Take the iges entity record and reformat to a neutral 
**				file record.
**    PARAMETERS   
**       INPUT  : 
**				gblk						 global structure 
**				dblk						 iges entities directory structure 
**				c							 iges entities parameter structure 
**       OUTPUT :  
**				key						 file pointer	
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uig_in_dispat(gblk,dblk,c,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record structure */
char c[];									/* parameter record */
UU_KEY_ID *key;							/* file pointer */
{
	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	*key = 0;
	switch(dblk->rel_type)
		{
		/*jkd14: p-spline surface */
		case GSPLSURF:
			uig_in_splinesrf(gblk,dblk,c,key);
			break;
		/*jkd31: plane */
		case GPLANE:
			uig_in_plane(gblk,dblk,c,key);
			break;
		case GLINE:
			uig_in_line(gblk,dblk,c,key);
			break;
		case GPOINT:
			uig_in_point(gblk,dblk,c,key);
			break;
		case GARC:
			uig_in_arc(gblk,dblk,c,key);
			break;
		case GCONIC:
			uig_in_conic(gblk,dblk,c,key);
			break;
		case GCOMPOSITE:
			/*
			if (process_normal)
			*/
			uig_in_comp(gblk,dblk,c,key);
			break;
		case GGROUP:
			/* cpp: remove test
			if (process_normal)	
			*/
			switch(dblk->form_no)
				{
				case 1:
				case 7:
/* 			case 13:    12.18.92vp temp added to check and removed*/
				case 14:
				case 15:
					uig_in_group(gblk,dblk,c,key);
					break;
				}
			break; 
		case VIEWVS:
			uig_in_viewvs(gblk, dblk, c, key);
			break;
		case PLNASSOC:
			uig_in_plnassoc(gblk, dblk, c, key);
			break;
		case GPOLY:
			uig_in_poly2(gblk,dblk,c,key);
			break;
		case GPOLY3D:
			uig_in_poly3(gblk,dblk,c,key);
			break;
		case GPOLY6D:
			uig_in_poly6(gblk,dblk,c,key);
			break;
		case GSPLINE:
			uig_in_spline(gblk,dblk,c,key);
			break;
		case GRSPLINE:
			uig_in_rspline(gblk,dblk,c,key);
			break;
		case GOFFSTSRF:
			uig_in_offstsrf(gblk,dblk,c,key);
			break;
		case GCRVONSRF:
			uig_in_crvonsrf(gblk,dblk,c,key);
			break;
		case GTRIMSRF:
			uig_in_trimsrf(gblk,dblk,c,key);
			break;
		case GNOTE:
			uig_in_gnote(gblk,dblk,c,key);
			break;
		case GANGDIM:
			uig_in_angdim(gblk,dblk,c,key);
			break;
		case GDIADIM:
			uig_in_diadim(gblk,dblk,c,key);
			break;
		case GLEADER:
			uig_in_leader(gblk,dblk,c,key);
			break;
		case GLINDIM:
			uig_in_lindim(gblk,dblk,c,key);
			break;
		case GRADIM:
			uig_in_radim(gblk,dblk,c,key);
			break;
		case GLABEL:
			uig_in_label(gblk,dblk,c,key);
			break;
		case GGENSYM:
			uig_in_gsymbol(gblk,dblk,c,key);
			break;
		case INST:
			/* remove test
			if (process_normal)	
			*/
			uig_in_instance(gblk,dblk,c,key);
			break;
		case VIEW:
			uig_in_view(gblk,dblk,c,key);
			break;
		case DRAW:
			uig_in_drawing(gblk,dblk,c,key);
			break;
		case GRSPLSRF:
			uig_in_rsplsrf(gblk,dblk,c,key);
			break;
		case GRULEDSRF:
			uig_in_ruledsrf(gblk,dblk,c,key);
			break;
		case GREVSRF:
			uig_in_revsrf(gblk,dblk,c,key);
			break;
		case GTBCYSRF:
			uig_in_tbcysrf(gblk,dblk,c,key);
			break;
/*
.....Adding for case GBDSRF for new entities 
.....141 and 143. JLS 10-29-98
*/
		case GBDSRF:
			uig_in_bdsrf(gblk,dblk,c,key);
			break;
/*
.....Adding for case GMSBO for new entities 186 and its individual entities
*/
		case GMSBO:
			uig_in_solid(gblk,dblk,c,key);
			break;

		case GVERLST:
         uig_in_verlist(gblk,dblk,c,key);
         break;

   	case GEDGLST:
         uig_in_edglist(gblk,dblk,c,key);
         break;

   	case GLOOP:
         uig_in_loop(gblk,dblk,c,key);
         break;

   	case GFACE:
         uig_in_face(gblk,dblk,c,key);
         break;

   	case GSHELL:
         uig_in_shell(gblk,dblk,c,key);
         break;


		}
	uu_dexit;
	}

