/*********************************************************************
**
**    NAME         :  nu5sel.c
**
**       CONTAINS:
**			  nclu_allentity_type
**		  	  nclu_allpoints	
**			  nclu_allsurfs
**			  nclu_get_geomtype
**			  nclu_set_geommask
**         ncl_sel_filter
**			  saveclip
**
**    MODULE NAME AND RELEASE LEVEL 
**       nu5sel.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:03
**
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "dselect.h"
#include "dselmask.h"
#include "drubber.h"
#include "view.h"
#include "umath.h"
#include "mdrel.h"
#include "mdpick.h"
#include "dmark.h"
#include "udebug.h"
#include "zsysdep.h"
#include "ginq.h"
#include "nclinp.h"
#include "udforms.h"
#include "udfdata.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"

#define UD_NULLPICKID -1			/* invalid pick id */
#define ENDSEG -1
#define UD_POLYMARKER 0
#define UD_POLYLINE 	 1
#define UD_TEXT 		 2
#define UD_FILLAREA 	 3
#define SEGNO  picker.ppath.pickpath[picker.ppath.depth - 2]

extern UU_LOGICAL UD_hilite;
extern UU_LOGICAL NCL_pick_feature;

void nclu_set_geommask();

/*********************************************************************
**
**    E_FUNCTION        :  nclu_allentity_type()
**       all entity type select handler in select subsystem
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/

nclu_allentity_type()
	{
	extern UD_FILTREC UD_filter;	/* attribute filter */
	UD_FILTREC save_filter;
	int i, status;
	static int types[28] = {0,0,0,0,0,0,0,
							0,0,0,0,0,0,0,
							0,0,0,0,0,0,0,
							0,0,0,0,0,0,0};

	uu_denter(UU_DTRC,(us,"entering nclu_allentity_type()"));

	/* save current filter */
	zbytecp(save_filter, UD_filter);

/* -- set filter to include only entity type selected -- */

	UD_filter.layer_state = UD_INACTIVE;
	UD_filter.color_state = UD_INACTIVE;
	UD_filter.linestyle_state = UD_INACTIVE;
	UD_filter.pen_state = UD_INACTIVE;
	UD_filter.entity_state = UD_INCLUDE;
	UD_filter.marker_state = UD_INACTIVE;

	status = nclu_get_geomtype(types);
	if (status==-1)
		return -1;
/*
.....Set the filter mask
*/
	nclu_set_geommask(types,UD_filter.f_entity);

/* -- now call all displayed with filtering and hiliting-- */

	ud_alldisp(UU_TRUE, UU_TRUE, UU_TRUE);

/* -- restore original filter -- */

	zbytecp(UD_filter, save_filter);
	uu_dexit;
	return 0 ;
}
/*********************************************************************
**
**    E_FUNCTION        :  nclu_allpoints()
**       all points select handler in select subsystem
**
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/

nclu_allpoints()
{
	extern UD_FILTREC UD_filter;	/* attribute filter */
	UD_FILTREC save_filter;
	int i;

	/* temporarily change select filter, all ud_alldisp() with filtering,
	**	and then restore the select filter to its original state */

	uu_denter(UU_DTRC,(us,"entering nclu_allpoints()"));

	/* save current filter */
	zbytecp(save_filter, UD_filter);

/* -- set filter to include only points -- */

	UD_filter.layer_state = UD_INACTIVE;
	UD_filter.color_state = UD_INACTIVE;
	UD_filter.linestyle_state = UD_INACTIVE;
	UD_filter.pen_state = UD_INACTIVE;
	UD_filter.entity_state = UD_INCLUDE;
	UD_filter.marker_state = UD_INACTIVE;

/* -- clear entity filter -- */

	for (i=0; i<UD_NENTITY_WORDS; i++)
		UD_filter.f_entity[i] = 0;

/* -- set the points bit -- */

	uu_set_bit(UD_filter.f_entity, UM_POINT_REL - 1);
	uu_set_bit(UD_filter.f_entity, NCL_POINT_REL - 1);

/* -- now call all displayed with filtering and no hiliting-- */

	ud_alldisp(UU_TRUE, UU_FALSE, UU_TRUE);

/* -- restore original filter -- */

	zbytecp(UD_filter, save_filter);
	uu_dexit;
	return 0 ;
}

/*********************************************************************
**
**    E_FUNCTION        :  nclu_allsurfs()
**       all surface entity type select handler in select subsystem
**
**    PARAMETERS
**       INPUT  :  none
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/
nclu_allsurfs()
{
   extern UD_FILTREC UD_filter;  /* attribute filter */
   UD_FILTREC save_filter;
   int i;

   /* temporarily change select filter, all ud_alldisp() with filtering,
   ** and then restore the select filter to its original state */

   uu_denter(UU_DTRC,(us,"entering nclu_allsurfs()"));

   /* save current filter */
   zbytecp(save_filter, UD_filter);

/* -- set filter to include only surfs -- */

   UD_filter.layer_state = UD_INACTIVE;
   UD_filter.color_state = UD_INACTIVE;
   UD_filter.linestyle_state = UD_INACTIVE;
   UD_filter.pen_state = UD_INACTIVE;
   UD_filter.entity_state = UD_INCLUDE;

/* -- clear entity filter -- */

   for (i=0; i<UD_NENTITY_WORDS; i++)
      UD_filter.f_entity[i] = 0;

/* -- set the surfaces bit -- */

   uu_set_bit(UD_filter.f_entity, NCL_SURF_REL - 1);
   uu_set_bit(UD_filter.f_entity, NCL_EVALSF_REL - 1);
   uu_set_bit(UD_filter.f_entity, UM_AGSRF_REL - 1);
   uu_set_bit(UD_filter.f_entity, UM_RBSPLSRF_REL - 1);
   uu_set_bit(UD_filter.f_entity, NCL_MESHSURF_REL - 1);
   uu_set_bit(UD_filter.f_entity, NCL_QUILTSURF_REL - 1);
   uu_set_bit(UD_filter.f_entity, NCL_NETSF_REL - 1);
   uu_set_bit(UD_filter.f_entity, NCL_TRIMSF_REL - 1);
   uu_set_bit(UD_filter.f_entity, NCL_REVSURF_REL - 1);

/* -- now call all displayed with filtering and no hiliting-- */

   ud_alldisp(UU_TRUE, UU_FALSE, UU_TRUE);

/* -- restore original filter -- */

   zbytecp(UD_filter, save_filter);
   uu_dexit;
   return 0 ;
}
/*********************************************************************
**    E_FUNCTION        :  OnClearAll()
**       Clear all geometry type selection
**
**    PARAMETERS
**       INPUT  :  
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClearAll(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i, type = 0;
	for (i=0; i<27;i++)
		ud_dispfrm_update_answer(0, i,(int *)&type);
	return(UD_FLDOK);
}

/*********************************************************************
**
**    E_FUNCTION        :  nclu_get_geomtype(types)
**       open a geometry type form and get the types inputs
**
**    PARAMETERS
**       INPUT  :  types
**       OUTPUT :  types
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
nclu_get_geomtype(types)
int *types;
{
	int i,status;
	static char traverse[]     = {1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
	static UD_METHOD methods[] = 
		{ UU_NULL,UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, 
			UU_NULL,UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, 
			UU_NULL,UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, 
			UU_NULL,UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, 
			OnClearAll};
	static char called[] = {6,6,6,6,6,6,6,6,6,6,6,6,6,6,
			6,6,6,6,6,6,6,6,6,6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
						1,1,1,1,1,1,1,1};
	static int *ans[28];
	for (i=0; i<27; i++)
	{
		ans[i] = (int *)&(types[i]);
	}
	ans[27] = UU_NULL;
	status = ud_form1("geotype.frm", ans, ans, methods, called,
		display, traverse);
	return status;
}

/*********************************************************************
**
**    E_FUNCTION        :  nclu_set_geommask(types,mask)
**       Sets the filter mask based on selected geometry from a
**       Geometry Types form.
**
**    PARAMETERS
**       INPUT  :
**          types    = Selected geometry types.
**       OUTPUT :
**          mask     = Filter mask for selected geometry types.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void nclu_set_geommask(types,mask)
int types[],mask[];
{
	int i;
/*
.....Initialize filter mask
*/
	for (i=0;i<UD_NMENTWD;i++) mask[i]=0;
/*
.....Points
*/
	if (types[0] == 1)
	{
		uu_set_bit(mask, UM_POINT_REL-1);
		uu_set_bit(mask, NCL_POINT_REL-1);
	}
/*
.....Lines
*/
	if (types[1] == 1)
	{
		uu_set_bit(mask, UM_LINE_REL - 1);
		uu_set_bit(mask, NCL_LINE_REL - 1);
	}

/*
.....Arcs/Circles
*/
	if (types[2] == 1)
	{
		uu_set_bit(mask, UM_CIRCLE_REL - 1);
		uu_set_bit(mask, NCL_CIRCLE_REL - 1);
	}
/*
.....Point-Vectors
*/
	if (types[3] == 1)
	{
		uu_set_bit(mask, NCL_POINTVEC_REL - 1);
	}
/*
.....Vectors
*/
	if (types[4] == 1)
	{
		uu_set_bit(mask, NCL_VECTOR_REL - 1);
	}
/*
.....Planes
*/
	if (types[5] == 1)
	{
 		uu_set_bit(mask, NCL_PLN_REL - 1);
	}
/*
.....Patterns
*/
	if (types[6] == 1)
	{
 		uu_set_bit(mask, NCL_PATERN_REL - 1);
	}
/*
......Matrixes
*/
	if (types[7] == 1)
	{
 		uu_set_bit(mask, NCL_MATRIX_REL - 1);
	}
/*
.....All Curves
*/
	if (types[8] == 1)
	{
		uu_set_bit(mask, NCL_CURVE_REL - 1);
		uu_set_bit(mask, NCL_EVALCV_REL - 1);
 		uu_set_bit(mask, UM_CONIC_REL - 1);
 		uu_set_bit(mask, UM_COMPCRV_REL - 1);
 		uu_set_bit(mask, UM_RBSPLCRV_REL - 1);
	}
/*
......B-Spline Curves
*/
	if (types[9] == 1)
	{
		uu_set_bit(mask, UM_RBSPLCRV_REL - 1);
		uu_set_bit(mask, UM_AGCRV_REL - 1);
	}
/*
.....NCL Curves
*/
	if (types[10] == 1)
	{
		uu_set_bit(mask, NCL_CURVE_REL - 1);
	}
/*
.....Composite Curves
*/
	if (types[11] == 1)
	{
		uu_set_bit(mask, UM_COMPCRV_REL - 1);
	}
/*
.....Surface-Splines
*/
	if (types[12] == 1)
	{
		uu_set_bit(mask, UM_UVCVONSF_REL - 1);
	}
/*
......Conics
*/
	if (types[13] == 1)
	{
		uu_set_bit(mask, UM_CONIC_REL - 1);
	}
/*
.....Shapes
*/
	if (types[14] == 1)
	{
		uu_set_bit(mask, NCL_SHAPE_REL - 1);
	}
/*
.....All Surfaces
*/
	if (types[15] == 1)
	{
		uu_set_bit(mask, NCL_SURF_REL - 1);
		uu_set_bit(mask, NCL_EVALSF_REL - 1);
		uu_set_bit(mask, UM_AGSRF_REL - 1);
		uu_set_bit(mask, UM_RBSPLSRF_REL - 1);
		uu_set_bit(mask, NCL_MESHSURF_REL - 1);
		uu_set_bit(mask, NCL_QUILTSURF_REL - 1);
		uu_set_bit(mask, NCL_NETSF_REL - 1);
		uu_set_bit(mask, NCL_TRIMSF_REL - 1);
		uu_set_bit(mask, NCL_REVSURF_REL - 1);
	}
/*
.....Nurb Surfaces
*/
	if (types[16] == 1)
	{
		uu_set_bit(mask, UM_RBSPLSRF_REL - 1);
	}
/*
.....NCL Surfaces
*/
	if (types[17] == 1)
	{
		uu_set_bit(mask, NCL_SURF_REL - 1);
	}
/*
.....Trimmed Surfaces
*/
	if (types[18] == 1)
	{
		uu_set_bit(mask, NCL_TRIMSF_REL - 1);
	}
/*
.....Revolved Surfaces
*/
	if (types[19] == 1)
	{
		uu_set_bit(mask, NCL_REVSURF_REL - 1);
	}
/*
.....Net Surfaces
*/
	if (types[20] == 1)
	{
		uu_set_bit(mask, NCL_NETSF_REL - 1);
	}
/*
.....Mesh Surfaces
*/
	if (types[21] == 1)
	{
		uu_set_bit(mask, NCL_MESHSURF_REL - 1);
	}
/*
.....Quilt Surfaces
*/
	if (types[22] == 1)
	{
		uu_set_bit(mask, NCL_QUILTSURF_REL - 1);
	}
/*
.....Solids
*/
	if (types[23] == 1)
	{
		uu_set_bit(mask, UM_SOLID_REL - 1);
	}
/*
.....Annotation
*/
	if (types[24] == 1)
	{
		uu_set_bit(mask, UA_TEXT_REL - 1);
	}
/*
.....Drafting entities
*/
	if (types[25] == 1)
	{
		for (i=UA_LINEAR_DIMS_REL;i<=UA_DRWBORDR_REL;i++)
			uu_set_bit(mask, i-1);
	}
/*
.....Features
*/
	if (types[26] == 1)
	{
		NCL_pick_feature = UU_TRUE;
		uu_set_bit(mask, UM_FEAT_REL - 1);
	}
}

/*********************************************************************
**   I_FUNCTION: OnPnChk(fieldno,val,stat)
**      Callback function Clip Plane form fields.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT  OnPnChk(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;

	ud_default_method(fieldno, val, stat);
/*
.....Enable correct fields
.....based on toggle field value
*/
	switch(*fieldno)
	{
	case 0:
		if (*(val->frmint) == 0 )
		{
			ud_set_traverse_mask(1,UU_FALSE);
			ud_set_traverse_mask(2,UU_FALSE);
			ud_set_traverse_mask(3,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(1,UU_TRUE);
			ud_set_traverse_mask(2,UU_TRUE);
			ud_set_traverse_mask(3,UU_TRUE);
		}
		break;
	case 4:
		if (*(val->frmint) == 0 )
		{
			ud_set_traverse_mask(5,UU_FALSE);
			ud_set_traverse_mask(6,UU_FALSE);
			ud_set_traverse_mask(7,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(5,UU_TRUE);
			ud_set_traverse_mask(6,UU_TRUE);
			ud_set_traverse_mask(7,UU_TRUE);
		}
		break;
	case 8:
		if (*(val->frmint) == 0 )
		{
			ud_set_traverse_mask(9,UU_FALSE);
			ud_set_traverse_mask(10,UU_FALSE);
			ud_set_traverse_mask(11,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(9,UU_TRUE);
			ud_set_traverse_mask(10,UU_TRUE);
			ud_set_traverse_mask(11,UU_TRUE);
		}
		break;
	case 12:
		if (*(val->frmint) == 0 )
		{
			ud_set_traverse_mask(13,UU_FALSE);
			ud_set_traverse_mask(14,UU_FALSE);
			ud_set_traverse_mask(15,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(13,UU_TRUE);
			ud_set_traverse_mask(14,UU_TRUE);
			ud_set_traverse_mask(15,UU_TRUE);
		}
		break;
	case 16:
		if (*(val->frmint) == 0 )
		{
			ud_set_traverse_mask(17,UU_FALSE);
			ud_set_traverse_mask(18,UU_FALSE);
			ud_set_traverse_mask(19,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(17,UU_TRUE);
			ud_set_traverse_mask(18,UU_TRUE);
			ud_set_traverse_mask(19,UU_TRUE);
		}
		break;
	case 20:
		if (*(val->frmint) == 0 )
		{
			ud_set_traverse_mask(21,UU_FALSE);
			ud_set_traverse_mask(22,UU_FALSE);
			ud_set_traverse_mask(23,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(21,UU_TRUE);
			ud_set_traverse_mask(22,UU_TRUE);
			ud_set_traverse_mask(23,UU_TRUE);
		}
		break;
	default:
		break;
    }
	return(UD_FLDOK);
}
/*********************************************************************
**    E_FUNCTION     : ncl_sel_filter()
**       Processes the Filter BOUNDING REGION form.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_sel_filter()
{
	int status;
/*
.....Set up form fields
*/
	static char tplane[6][64] = {"", "", "", "", "", ""};
	static int tside[6] = {0,0,0,0,0,0};
	static int tcross[6] = {0,0,0,0,0,0};
	UU_REAL rplane[6][4], um_mag();
	int inc,i,j,inum,ipl,nc,cside[6],lims[UD_NMENTWD];
	int *ans[24], flag, pl_sel[6];
	UU_LOGICAL limflg;
	char *tpp,lerr[80];
	struct NCL_nclpl_rec upl;
	struct NCLI_plane_rec npl;
	UM_f77_str fsym;
	static char traverse[]     = {1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0};
	static char display[]      = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1};
	static UD_METHOD methods[] = {OnPnChk, UU_NULL, UU_NULL, UU_NULL,
									OnPnChk, UU_NULL, UU_NULL, UU_NULL,
									OnPnChk, UU_NULL, UU_NULL, UU_NULL,
									OnPnChk, UU_NULL, UU_NULL, UU_NULL,
									OnPnChk, UU_NULL, UU_NULL, UU_NULL,
									OnPnChk, UU_NULL, UU_NULL, UU_NULL};
	static char called[]       = {6,6,6,6, 6,6,6,6, 6,6,6,6, 6,6,6,6, 6,6,6,6, 6,6,6,6};

/*
.....Initialize form
*/
	cside[0] = 0; cside[1] = 0;
	cside[2] = 1; cside[3] = 1;
	cside[4] = 2; cside[5] = 2;
	inc = 0;
	for (i=0;i<6;i++)
	{
		ans[inc] = (int *)&pl_sel[i];
		ans[inc+1] = (int *)&tplane[i][0];
		ans[inc+2] = &tside[i];
		ans[inc+3] = &tcross[i];
		inc = inc + 4;
	}
/*
.....Set up the field entries
*/
	for (i=0;i<6;i++)
	{
		pl_sel[i] = 0;
	}
/*
.....Limit the selection to Planes
*/
	ud_inqgeo(&limflg,lims);
	ud_lgeo(UU_TRUE,UD_ncl_pl);
/*
.....Get the Form input
*/
form:;
	status = ud_form1("selectclip.frm", ans, ans, methods, called, display, traverse);
	ud_lgeo(limflg,lims);
	if (status==-1)
		return -1;
/*
.....Verify planes are valid
*/
    ipl = 0;
	for (i=0;i<6;i++)
	{
		if (pl_sel[i]==1)
		{
			tpp = tplane[i];
			nc = strlen(tpp);
			ul_strip_blanks(tpp,&nc);
			if (nc != 0)
			{
				for (j=0;j<nc;j++)
					tpp[j] = islower(tpp[j]) ? toupper(tpp[j]) : tpp[j];
				if (ul_to_reals(rplane[ipl],&inum,4,tpp) != UU_SUCCESS)
				{
					UM_init_f77_str(fsym,tpp,nc);
					getkey(UM_addr_of_f77_str(fsym),&upl.key);
					if (upl.key == 0) goto invplane;
					ncl_retrieve_data_fixed(&upl);
					if (upl.rel_num != NCL_PLN_REL) goto invplane;
					ncl_plane_to_nclpln(&upl,&npl);
					rplane[ipl][0] = npl.ijk[0];
					rplane[ipl][1] = npl.ijk[1];
					rplane[ipl][2] = npl.ijk[2];
					rplane[ipl][3] = npl.dist;
					inum = 4;
				}
				if (inum != 4) goto invplane;
				um_unitvc(rplane[ipl],rplane[ipl]);
				if (um_mag(rplane[ipl]) == 0.) goto invplane;
				if (tside[i]<=5)
				{
					if (rplane[ipl][cside[tside[i]]] == 0.) goto invside;
				}
				for (j=0;j<4;j++)
				{
					UD_selclip.rplane[ipl][j] = rplane[ipl][j];
				}
				UD_selclip.side[ipl] = tside[i];
				UD_selclip.cross[ipl] = tcross[i];
				ipl++;
			}
		}
	}
/*
.....Save the form entries
*/
	UD_selclip_npl = ipl;
	flag = 0;
	ud_clip_region(&flag);
	goto done;
/*
.....Invalid plane plane
*/
invplane:;
	sprintf (lerr,"Invalid clip plane: %s",tplane[i]);
	ud_wrerr(lerr);
	goto form;
/*
.....Invalid clip side
*/
invside:;
	sprintf (lerr,"Clipping side parallel with plane: %s",tplane[i]);
	ud_wrerr(lerr);
	goto form;
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

void saveclip (rplane1, rplane2, rplane3, rplane4, side, cross, num)
UM_real8 *rplane1, *rplane2, *rplane3, *rplane4;
UM_int4 *side, *cross, *num;
{
	int i;
	UD_selclip_npl = *num;
	i = UD_selclip_npl;
	UD_selclip.rplane[i-1][0] = *rplane1;
	UD_selclip.rplane[i-1][1] = *rplane2;
	UD_selclip.rplane[i-1][2] = *rplane3;
	UD_selclip.rplane[i-1][3] = *rplane4;
	UD_selclip.side[i-1] = *side;
	UD_selclip.cross[i-1] = *cross;
}
