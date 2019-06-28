/****************************************************************
**     NAME       : reup9700.c
**     CONTAINS   :
**        ur_update_9700()
**        ur_update_9700_varl()
**        ur_update_9700_attr()
**        ur_up_970_text_fxd()
**        ur_up_970_textattr()
**        ur_up_970_symbol_fxd()
**        ur_up_970_instance_fxd()
**        ur_up_970_symattr()
**
**-------------------------  IMPORTANT NOTE  ---------------------
**
**  Be sure to change all standard Unibase structure names to ...
**
**              'xxxver'
**
**  ... in THIS FILE when updating to the next version.  The newly
**  created file should still have the correct structure names
**  (NCL_surface_rec) just in case the structure changes during the
**  release cycle.
**
**  Where:  xxx = symbolic abbreviation for entity, for example,
**                'NCL_surface_rec' becomes 'surf'.
**
**          ver = Version number that THIS FILE was created to update
**                the Unibase TO.  For example '94'.
**
**  This would create the structure name '93'.  These new structure names
**  will be added to the NEW include file created for the next version's update
**  routine, for example 'rver9400.h'.  This new include file will of course
**  have to be added to THIS FILE also.
**
**  The reason for this is that all of the routines used to use the actual
**  structure name (ex. NCL_surface_rec) to update to, even though the
**  structure was changed from release to release.  Needless to say this
**  caused the structure to be updated incorrectly (especially since it would
**  not be updated multiple releases due to the old logic in
**  'ur_update_entity').
**
**-------------------------  IMPORTANT NOTE  ---------------------
**
**     MODULE NAME AND RELEASE LEVEL
**       reup9700.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:40
**
*****************************************************************/
#include "mlabddl.h"
#include "class.h"
#include "udebug.h"
#include "uhep.h"
#include "ribase.h"
#include "ritrnerr.h"
#include "rerrdef.h"
#include "mdrel.h"
#include "atext.h"
#include "bsym.h"

#include "mattr.h"
#include "msrf.h"
#include "nccs.h"
#include "ncl.h"
#include "mdcoord.h"

#include "dtypes.h"
#include "nclver.h"
#define V97MAIN
#include "rver9700.h"
#undef V97MAIN
#include "rver9900.h"

/***************************************************************************
**    E_FUNCTION     :  ur_update_9700 (in_ptr,out_ptr)
**    Update fixed data of entity (Unibase ver < 9.700). 
**    PARAMETERS
**       INPUT  :
**          in_ptr    - Pointer to input structure.
**       OUTPUT :
**          out_ptr   - Pointer to output structure.
****************************************************************************/
int ur_update_9700 (in_ptr,out_ptr)
struct UR_data *in_ptr, *out_ptr;
{
	int status;
	status = 0; 
	switch (in_ptr->rel_num)
	{
		case UA_TEXT_REL:
			status = ur_up_970_text_fxd (in_ptr,out_ptr);
			break;
		case UA_TEXTATTR_REL:
			status = ur_up_970_textattr (in_ptr,out_ptr);
			break;
		case UB_SYMBOL_REL:
			status = ur_up_970_symbol_fxd (in_ptr,out_ptr);
			break;
		case UB_INSTANCE_REL:
			status = ur_up_970_instance_fxd (in_ptr,out_ptr);
			break;
		case UB_SYMATTR_REL:
			status = ur_up_970_symattr (in_ptr,out_ptr);
			break;
		case UM_RBSPLSRF_REL:
			status = ur_up_970_rbsplsrf_fxd (in_ptr,out_ptr);
			break;
		case NCL_SURF_REL:
			status = ur_up_970_surface_fxd (in_ptr,out_ptr);
			break;
		case NCL_MESHSURF_REL:
			status = ur_up_970_meshsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_REVSURF_REL:
			status = ur_up_970_revsurf_fxd (in_ptr,out_ptr);
			break;
		case NCL_NETSF_REL:
			status = ur_up_970_netsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_TRIMSF_REL:
			status = ur_up_970_trimsf_fxd (in_ptr,out_ptr);
			break;
		case NCL_SHAPE_REL:
			status = ur_up_970_shape_fxd (in_ptr,out_ptr);
			break;
		case NCL_SCALAR_REL:
			status = ur_up_970_scalar_fxd (in_ptr,out_ptr);
			break;
		default:
			status = 0;
			break;
	}
	return(status);
}
/***************************************************************************
**    E_FUNCTION     :  ur_update_9700_varl (eptr,list,vli_ptr,vlo_ptr)
**    Update variable list data of entity (Unibase ver < 9.700). 
**
**       WARNING: The 'update_varl' routines are all called after all
**                versions of 'updated_fxd' routines, so the output
**                structures should be the same as the current version
**                of NCL.
**    PARAMETERS
**       INPUT  :
**          eptr     - Pointer to input structure (updated already).
**          list     - Var-list number to update.
**          vli_ptr  - Pointer to input variable list data (#list).
**       OUTPUT :
**          vli_ptr  - Pointer to updated list data.
****************************************************************************/
int ur_update_9700_varl (eptr,list,vli_ptr,vlo_ptr)
struct UR_data *eptr;
int list;
char *vli_ptr, **vlo_ptr;
{
	int status,ntess,nxyzby;
	status = 0; 
	ntess = 0;
	nxyzby = 0;

	switch (eptr->rel_num)
	{
		case UM_RBSPLSRF_REL:
		{
			if (list == 7)
			{
				struct UM_rbsplsrf_rec *sf1;
				sf1 = (struct UM_rbsplsrf_rec *) eptr;
				ntess =  sf1->no_tesslst;

				status = ur_up_970_tesslst (ntess,vli_ptr,vlo_ptr);
			}			
			if (list == 9)
			{
				struct UM_rbsplsrf_rec *sf1;
				sf1 = (struct UM_rbsplsrf_rec *) eptr;
				nxyzby =  sf1->no_xyzbylst;

				status = ur_up_970_xyzbylst (nxyzby,vli_ptr,vlo_ptr);
			}
			break;
		}
		case NCL_SURF_REL:
		{
			if (list == 4)
			{
				struct NCL_surface_rec *sf1;
				sf1 = (struct NCL_surface_rec *) eptr;
				ntess =  sf1->no_tesslst;

				status = ur_up_970_tesslst (ntess,vli_ptr,vlo_ptr);
			}				
			if (list == 6)
			{
				struct NCL_surface_rec *sf1;
				sf1 = (struct NCL_surface_rec *) eptr;
				nxyzby =  sf1->no_xyzbylst;

				status = ur_up_970_xyzbylst (nxyzby,vli_ptr,vlo_ptr);
			}
			break;
		}
		case NCL_TRIMSF_REL:
		{
			if (list == 3)
			{
				struct NCL_trimsf_rec *sf1;
				sf1 = (struct NCL_trimsf_rec *) eptr;
				ntess =  sf1->no_tesslst;

				status = ur_up_970_tesslst (ntess,vli_ptr,vlo_ptr);
			}
			if (list == 5)
			{
				struct NCL_trimsf_rec *sf1;
				sf1 = (struct NCL_trimsf_rec *) eptr;
				nxyzby =  sf1->no_xyzbylst;

				status = ur_up_970_xyzbylst (nxyzby,vli_ptr,vlo_ptr);
			}
			break;
		}
		case NCL_MESHSURF_REL:
		{
			if (list == 3)
			{
				struct NCL_meshsf_rec *sf1;
				sf1 = (struct NCL_meshsf_rec *) eptr;
				ntess =  sf1->no_tesslst;

				status = ur_up_970_tesslst (ntess,vli_ptr,vlo_ptr);
			}
			if (list == 5)
			{
				struct NCL_meshsf_rec *sf1;
				sf1 = (struct NCL_meshsf_rec *) eptr;
				nxyzby =  sf1->no_xyzbylst;

				status = ur_up_970_xyzbylst (nxyzby,vli_ptr,vlo_ptr);
			}
			break;
		}
		case NCL_SHAPE_REL:
		{
			if (list == 3)
			{
				struct NCL_shape_rec *sf1;
				sf1 = (struct NCL_shape_rec *) eptr;
				ntess =  sf1->no_tesslst;

				status = ur_up_970_tesslst (ntess,vli_ptr,vlo_ptr);
			}
			break;
		}
		case NCL_REVSURF_REL:
		{
			if (list == 3)
			{
				struct NCL_revsurf_rec *sf1;
				sf1 = (struct NCL_revsurf_rec *) eptr;
				ntess =  sf1->no_tesslst;

				status = ur_up_970_tesslst (ntess,vli_ptr,vlo_ptr);
			}
			if (list == 5)
			{
				struct NCL_revsurf_rec *sf1;
				sf1 = (struct NCL_revsurf_rec *) eptr;
				nxyzby =  sf1->no_xyzbylst;

				status = ur_up_970_xyzbylst (nxyzby,vli_ptr,vlo_ptr);
			}
			break;
		}
	}

	uu_dexit;
	return(status);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_tesslst (ntess,dti,vlo)
**    Update tessellation list in a surface entity (Unibase ver < 9.700). 
**    PARAMETERS
**       INPUT  :
**          ntess   - number of stored UU_REAL numbers
**          dti     - pointer to input var-list.
**       OUTPUT :
**          vlo     - pointer to pointer of updated output var-list.
****************************************************************************/
int ur_up_970_tesslst (ntess,dti,vlo)
int ntess;
UU_REAL *dti;
char **vlo;
{
	UU_REAL tol;
	UU_REAL *dto;
	int np,ntri,nn,i,j;

	tol = dti[0];
	if (tol > 0.) return(0);
	np = (int) dti[1];
	ntri = (int) dti[2];

	nn = 3 + 6*np + 3*ntri;
	if (nn + 2*np > ntess) return(0);

	dto   = (UU_REAL *) uu_toolmalloc (ntess * sizeof(UU_REAL));
	*vlo  = (char *) dto;

	for (i = 0; i < nn; i++)
	{
		if (i < 6*np+3)
			j = i;
		else
			j = 2*np+i;
		dto[i] = dti[j];
	}

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_xyzbylst (ntess,dti,vlo)
**    Update xyzby list in a surface entity (Unibase ver < 9.700). 
**    PARAMETERS
**       INPUT  :
**          eptr    - Pointer to input structure (updated already).
**          dti     - pointer to input var-list.
**       OUTPUT :
**          vlo     - pointer to pointer of updated output var-list.
****************************************************************************/
int ur_up_970_xyzbylst (nxyzby,dti,vlo)
int nxyzby;
UU_REAL *dti;
char **vlo;
{
	UU_REAL dist;
	int i,j,nn,nb;
	UM_coord *pt,pt1;
	int *np;  

/*
...	Get number of boundary
*/
	nb = (int) dti[1];

/*
...	Initialize number of boundary points
*/
	np = (int *)uu_malloc((nb)*sizeof(int));

	*vlo  = (char *) dti;

	nn = 2;
	for (i=0; i<nb; i++) nn += 2;
	for (i=0; i<nb; i++) nn += 2;
	for (i=0; i<nb; i++)
	{
		np[i] = (int) *(dti+nn);
		nn++;
	}
	nn++;

	for (i = 0; i < nb; i++)
	{	
		pt = (UM_coord *) (dti+nn);
		nn += 3 * np[i];
/*
... upadte uvpts with t parameter
*/
		dist = 0.0;
		for (j = 1; j < np[i]; j++)
		{
			um_vctovc (pt[j-1],pt1);	
			pt1[2] = 0.0;
			dist += um_dcccc(pt[j],pt1);
			pt[j][2] = dist;
		}

		for (j = 0; j < np[i]; j++)
			pt[j][2] = pt[j][2] / dist;
	}
	
	uu_free(np);
	np=NULL;	

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_update_9700_attr (key,relnum)
**    Update attribute data of entity (Unibase ver < 9.700). 
**
**       WARNING: The 'update_attr' routines are all called after all
**                versions of 'updated_fxd' routines, so the output
**                structures should be the same as the current version
**                of NCL.
**    PARAMETERS
**       INPUT  :
**          key       - Key of entity to update.
**          relnum    - Relation number of entity to update.
**       OUTPUT :
**          none
****************************************************************************/
int ur_update_9700_attr (key,relnum)
UU_KEY_ID key;
int relnum;
{
	int status;
	struct NCL_nclattr_rec attr;
	struct UM_surfattr_rec sfattr;
	int material,lucency,ecolor,numu,numv,peru,perv;
	UU_LOGICAL shaded;

	status = 0; 

	switch (relnum)
	{
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_REVSURF_REL:
		case NCL_MESHSURF_REL:
		case NCL_NETSF_REL:
		case NCL_SHAPE_REL:
		case NCL_TRIMSF_REL:
			attr.key = key;
			status = ur_retrieve_attr(&attr);
			if (status == 0)
			status = S_get_surf_attr(key,&material,&shaded,&lucency,&ecolor,
				&numu,&numv,&peru,&perv);
			if (status == 0)
				status = ur_delete_attr(key);
			if (status == 0)
			{
				sfattr.key = key;
				sfattr.rel_num = UM_SURFATTR_REL;
				sfattr.use_count = 0;
				sfattr.color = attr.color;
				sfattr.layer = attr.layer;
				sfattr.pen = attr.pen;
				sfattr.line_style = attr.line_style;
				sfattr.line_weight = attr.line_weight;
				sfattr.line_width = attr.line_width;
				sfattr.displayable = attr.displayable;
				sfattr.selectable = attr.selectable;
				sfattr.label_on = attr.label_on;
				sfattr.material = material;
				sfattr.numupaths = numu;
				sfattr.numvpaths = numv;
				sfattr.ptsperucrv = peru;
				sfattr.ptspervcrv = perv;
				sfattr.ecolor = ecolor;
				sfattr.shaded = shaded;
				sfattr.lucency = lucency;

				ur_update_attr(&sfattr);
			}
			break;
		default:
			break;
	}
 
	return(status);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_text_fxd (e1,e2)
**    Update fixed data text entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) structure.
**       OUTPUT :
**          e2   - pointer to updated output structure.
**
****************************************************************************/
int ur_up_970_text_fxd (e1,e2)
struct UA_txt_rec96 *e1;
struct UA_txt_rec98 *e2;
{
	int i;
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	e2->label[0] = '\0';
	e2->subscr = 0;
	e2->subtype = e1->subtype;
	e2->arckey = e1->arckey;
	e2->dx = e1->dx;
	e2->dy = e1->dy;
	e2->tangle = e1->tangle;
	e2->no_tchar = e1->no_tchar;
	e2->no_displst = 0;
	for (i=0;i<3;i++)
	{
		e2->labloc[i] = e2->ldrloc[i] = 0.;
		e2->position[i] = e1->position[i];
	}
	return 1;
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_textattr (key)
**    Update fixed data text attribute entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) structure.
**       OUTPUT :
**          e2   - pointer to updated output structure.
**
****************************************************************************/
int ur_up_970_textattr (e1,e2)
struct UA_txtattr_rec96 *e1;
struct UA_txtattr_rec98 *e2;
{
	int i;
/*
.....Update the structure
*/
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	e2->use_count = e1->use_count;
	e2->color = e1->color;
	e2->layer = e1->layer;
	e2->pen = e1->pen;
	e2->line_style = e1->line_style;
	e2->line_weight = e1->line_weight;
	e2->line_width = e1->line_width;
	e2->displayable = e1->displayable;
	e2->selectable = e1->selectable;
	e2->label_on = 0;
	e2->font = e1->font;
	e2->prec = e1->prec;
	e2->expn = e1->expn;
	e2->spacing = e1->spacing;
	e2->height = e1->height;
	for (i=0;i<3;i++)
	{
		e2->up[i] = e1->up[i];
		e2->plane[i] = e1->plane[i];
	}
	e2->path = e1->path;
	e2->align_hor = e1->align_hor;
	e2->align_ver = e1->align_ver;
	e2->txt_dens = e1->txt_dens;
	e2->slant = e1->slant;
	e2->sub_sup = e1->sub_sup;
	e2->line_spacing = e1->line_spacing;
	e2->entity_site = e1->entity_site;
	return 1;
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_symbol_fxd (e1,e2)
**    Update fixed data text entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) structure.
**       OUTPUT :
**          e2   - pointer to updated output structure.
**
****************************************************************************/
int ur_up_970_symbol_fxd (e1,e2)
struct UB_symbol_rec96 *e1;
struct UB_symbol_rec98 *e2;
{
	int i;
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy(e2->label,e1->name);
	for (i=0;i<3;i++) e2->labloc[i] = e2->ldrloc[i] = 0.;
	e2->subscr = 0;
	e2->version = e1->version;
	strcpy(e2->path,e1->path);
	e2->no_masters = e1->no_masters;
	e2->no_inst = e1->no_inst;
	e2->no_geom = e1->no_geom;
	e2->no_text_nod = e1->no_text_nod;
	e2->no_snap_nod = e1->no_snap_nod;
	return 1;
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_instance_fxd (e1,e2)
**    Update fixed data text entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) structure.
**       OUTPUT :
**          e2   - pointer to updated output structure.
**
****************************************************************************/
int ur_up_970_instance_fxd (e1,e2)
struct UB_instance_rec96 *e1;
struct UB_instance_rec98 *e2;
{
	int i;
	static int isub=1;

	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy(e2->label,"SYM_INST");
	e2->subscr = isub++;
	for (i=0;i<3;i++) e2->labloc[i] = e2->ldrloc[i] = 0.;
	e2->no_geom = e1->no_geom;
	e2->no_text_nod = e1->no_text_nod;
	e2->no_snap_nod = e1->no_snap_nod;
	return 1;
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_symattr_fxd (e1,e2)
**    Update fixed data symbol attribute entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) structure.
**       OUTPUT :
**          e2   - pointer to updated output structure.
**
****************************************************************************/
int ur_up_970_symattr (e1,e2)
struct UB_symattr_rec96 *e1;
struct UB_symattr_rec98 *e2;
{
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	e2->use_count = e1->use_count;
	e2->color = e1->color;
	e2->layer = e1->layer;
	e2->pen = e1->pen;
	e2->line_style = e1->line_style;
	e2->line_weight = e1->line_weight;
	e2->line_width = e1->line_width;
	e2->displayable = e1->displayable;
	e2->selectable = e1->selectable;
	e2->label_on = 0;
	e2->see_snod = e1->see_snod;
	e2->see_tnod = e1->see_tnod;
	return 1;
}

/**************************************************************************
**    E_FUNCTION     :  ur_up_970_header (e1,e2)
**    copy surface header.
****************************************************************************/
void ur_up_970_header (e1,e2)
struct NCL_sfhead96 *e1;
struct NCL_sfhead98 *e2;
{
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy (e2->label,e1->label);
	e2->subscr = e1->subscr;
	um_vctovc (e1->labloc,e2->labloc);
	um_vctovc (e1->ldrloc,e2->ldrloc);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_rbsplsrf_fxd (e1,e2)
**    Update fixed data rbsplsrf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old)  rbsplsrf structure.
**       OUTPUT :
**          e2   - pointer to updated output  rbsplsrf structure.
**
****************************************************************************/
int ur_up_970_rbsplsrf_fxd (e1,e2)
struct UM_rbsplsrf_rec96 *e1;
struct UM_rbsplsrf_rec98 *e2;
{
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_970_header (e1,e2);
	e2->primitive = e1->primitive + e1->numupaths*100;
	e2->rldnu = e1->rldnu + e1->material*100;
	e2->swapuv = 0;
	e2->rev_normal = e1->rev_normal + e1->numvpaths*100;
	e2->closdinu = e1->closdinu + e1->shaded*100;
	e2->closdinv = e1->closdinv + e1->lucency*100;
	e2->offdist = e1->offdist;
	e2->ku = e1->ku;
	e2->kv = e1->kv;
	e2->nu = e1->nu;
	e2->nv = e1->nv;
	for (i = 0; i < 16; i++) e2->prim_param[i] = e1->prim_param[i];
	e2->no_tu = e1->no_tu;
	e2->no_tv = e1->no_tv;
	e2->no_pt = e1->no_pt;
	e2->no_wt = e1->no_wt;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_surface_fxd (e1,e2)
**    Update fixed data NCL surface entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL surface structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL surface structure.
**
****************************************************************************/
int ur_up_970_surface_fxd (e1,e2)
struct NCL_surface_rec96 *e1;
struct NCL_surface_rec98 *e2;
{	
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_970_header (e1,e2);
	e2->rldnu = e1->rldnu + e1->material*100;
	e2->swapuv = 0;
	e2->rev_normal = e1->rev_normal + e1->numvpaths*100;
	e2->closdinu = e1->closdinu + e1->shaded*100;
	e2->closdinv = e1->closdinv + e1->lucency*100;
	e2->offset = e1->offset;
	e2->offdist = e1->offdist;
	e2->surf_type = e1->surf_type;
	e2->primitive = e1->primitive + e1->numupaths*100;
	for (i = 0; i < 16; i++) e2->prim_param[i] = e1->prim_param[i];
	e2->no_panelkey = e1->no_panelkey;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_revsurf_fxd (e1,e2)
**    Update fixed data NCL revsurf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL revsurf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL revsurf structure.
**
****************************************************************************/
int ur_up_970_revsurf_fxd (e1,e2)
struct NCL_revsurf_rec96 *e1;
struct NCL_revsurf_rec98 *e2;
{	
	int i;
/*
...Copy unchanged fields of surface
*/
	ur_up_970_header (e1,e2);
	e2->rldnu = -1;
	e2->swapuv = 0 + e1->material*100;
	e2->rev_normal = e1->rev_normal + e1->numvpaths*100;
	e2->closdinu = e1->closdinu + e1->shaded*100;
	e2->closdinv = e1->closdinv + e1->lucency*100;
	e2->offdist = e1->offdist;
	e2->primitive = e1->primitive + e1->numupaths*100;
	for (i = 0; i < 16; i++) e2->prim_param[i] = e1->prim_param[i];
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->cvkey = e1->cvkey;
	for (i = 0; i < 3; i++)
	{
		e2->pta[i] = e1->pta[i]; e2->vca[i] = e1->vca[i];
	}
	e2->sa = e1->sa;
	e2->ta = e1->ta;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_meshsf_fxd (e1,e2)
**    Update fixed data NCL meshsf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL meshsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL meshsf structure.
**
****************************************************************************/
int ur_up_970_meshsf_fxd (e1,e2)
struct NCL_meshsf_rec96 *e1;
struct NCL_meshsf_rec98 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_970_header (e1,e2);
	e2->rldnu = e1->rldnu + e1->material*100;
	e2->swapuv = 0;
	e2->rev_normal = e1->rev_normal + e1->numvpaths*100;
	e2->closdinu = e1->closdinu + e1->shaded*100;
	e2->closdinv = e1->closdinv + e1->lucency*100;
	e2->offset = e1->offset;
	e2->offdist = e1->offdist;
	e2->surf_type = e1->surf_type + e1->numupaths*100;
	e2->m = e1->m;
	e2->n = e1->n;
	e2->no_mpatch = e1->no_mpatch;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_netsf_fxd (e1,e2)
**    Update fixed data NCL netsf entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL netsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL netsf structure.
**
****************************************************************************/
int ur_up_970_netsf_fxd (e1,e2)
struct NCL_netsf_rec96 *e1;
struct NCL_netsf_rec98 *e2;
{	
	int i,j;
/*
...Copy unchanged fields of surface
*/
	ur_up_970_header (e1,e2);
	e2->surf_type = e1->surf_type + e1->material*100;
	e2->ldrloc[0] = e1->ldrloc[0] + e1->shaded*10000;
	e2->ldrloc[1] = e1->ldrloc[1] + e1->lucency*10000;
	for (i = 0; i < 40; i++)
		for (j = 0; j < 4; j++)
			e2->bndsfs[i][j] = e1->bndsfs[i][j];
	e2->no_netkey = e1->no_netkey;
	e2->no_sskey = e1->no_sskey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_trimsf_fxd (e1,e2)
**    Update fixed data NCL trimsfentity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL trimsf structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL trimsf structure.
**
****************************************************************************/
int ur_up_970_trimsf_fxd (e1,e2)
struct NCL_trimsf_rec96 *e1;
struct NCL_trimsf_rec98 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_970_header (e1,e2);
	e2->ldrloc[0] = e1->ldrloc[0] + e1->numupaths*10000;
	e2->ldrloc[1] = e1->ldrloc[1] + e1->numvpaths*10000;
	e2->ldrloc[2] = e1->ldrloc[2] + e1->material*10000;
	e2->closdinu = e1->closdinu + e1->shaded*100;
	e2->closdinv = e1->closdinv + e1->lucency*100;
	e2->offdist = e1->offdist;
	e2->uv_key = e1->uv_key;
	e2->cv_key = e1->cv_key;
	e2->bs_key = e1->bs_key;
	e2->ub_min = e1->ub_min;
	e2->ub_max = e1->ub_max;
	e2->vb_min = e1->vb_min;
	e2->vb_max = e1->vb_max;
	e2->u_min = e1->u_min;
	e2->u_max = e1->u_max;
	e2->v_min = e1->v_min;
	e2->v_max = e1->v_max;
	e2->drive_type = e1->drive_type;
	e2->shaded  = e1->shaded;
	e2->lucency = e1->lucency;
	e2->no_ibndykey = e1->no_ibndykey;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;
	e2->no_boxlst = e1->no_boxlst;
	e2->no_xyzbylst = e1->no_xyzbylst;
	e2->no_uvbylst = e1->no_uvbylst;
	e2->no_uvboxlst = e1->no_uvboxlst;

	return(1);
}

/***************************************************************************
**    E_FUNCTION     :  ur_up_970_shape_fxd (e1,e2)
**    Update fixed data NCL shape entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) NCL shape structure.
**       OUTPUT :
**          e2   - pointer to updated output NCL shape structure.
**
****************************************************************************/
int ur_up_970_shape_fxd (e1,e2)
struct NCL_shape_rec96 *e1;
struct NCL_shape_rec98 *e2;
{	
/*
...Copy unchanged fields of surface
*/
	ur_up_970_header (e1,e2);
	e2->ldrloc[0] = e1->ldrloc[0] + e1->numupaths*10000;
	e2->ldrloc[1] = e1->ldrloc[1] + e1->numvpaths*10000;
	e2->ldrloc[2] = e1->ldrloc[2] + e1->material*10000;
	e2->f2d3d = e1->f2d3d + e1->shaded*100;
	e2->labloc[0] = e1->labloc[0] + e1->lucency*10000;
	e2->no_shapwd = e1->no_shapwd;
	e2->no_displst = e1->no_displst;
	e2->no_tesslst = e1->no_tesslst;

	return(1);
}
/***************************************************************************
**    E_FUNCTION     :  ur_up_970_scalar_fxd (e1,e2)
**    Update fixed data NCL surface entity.
**
**    PARAMETERS
**       INPUT  :
**          e1   - pointer to input (old) Layer structure.
**       OUTPUT :
**          e2   - pointer to updated output Layer structure.
**
****************************************************************************/
int ur_up_970_scalar_fxd (e1,e2)
struct NCL_scalar_rec96 *e1;
struct NCL_scalar_rec98 *e2;
{
/*
...copy data
*/
	e2->key = e1->key;
	e2->rel_num = e1->rel_num;
	strcpy (e2->label,e1->label);
	e2->subscr = e1->subscr;
	e2->scalar_value = e1->scalar_value;
	strcpy (e2->descript,e1->descript);
	strcpy (e2->classnm,e1->classnm);
/*
.....initial modified to 0
*/
	e2->modified = 0;
	return(1);
}

/*********************************************************************
**    E_FUNCTION     : S_get_surf_attr(key,material,shaded,lucency,ecolor,
**                                       numu,numv,peru,perv)
**       Get surface attributes stored in surface structures.
**    PARAMETERS   
**       INPUT  : 
**          key         Key of surface entity.
**       OUTPUT :  
**          material,shaded,lucency,ecolor,numu,numv,peru,perv
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_surf_attr (key,material,shaded,lucency,ecolor,numu,numv,peru,
	perv)
UU_KEY_ID key;
int *material,*lucency,*ecolor,*numu,*numv,*peru,*perv;
UU_LOGICAL *shaded;
{
	struct NCL_fixed_databag sf;
	int status,ecolor0;

	uu_denter(UU_MTRC,(us,"ncl_get_surf_attr()"));

	sf.key = key;
	status = ncl_retrieve_data_fixed(&sf);
	if (status != UU_SUCCESS) return (status);

	if (sf.rel_num == UM_RBSPLSRF_REL)
	{
		struct UM_rbsplsrf_rec *sfp;
		sfp = (struct UM_rbsplsrf_rec *)&sf;

		*numu = sfp->primitive/100;
		*numv = sfp->rev_normal/100;
		*peru = 0;
		*perv = 0;
		*material = sfp->rldnu/100;
		*shaded = sfp->closdinu/100;
		*lucency = sfp->closdinv/100;
		*ecolor = -1;

		sfp->primitive = sfp->primitive-(*numu*100);
		sfp->rev_normal = sfp->rev_normal-(*numv*100);
		sfp->rldnu = sfp->rldnu-(*material*100);
		sfp->closdinu = sfp->closdinu-(*shaded*100);
		sfp->closdinv = sfp->closdinv-(*lucency*100);
		status = ur_update_data_fixed(&sf);
	}
	else if (sf.rel_num == NCL_SURF_REL) 
	{
		struct NCL_surface_rec *sfp;
		sfp = (struct NCL_surface_rec *)&sf;

		*numu = sfp->primitive/100;
		*numv = sfp->rev_normal/100;
		*peru = 0;
		*perv = 0;
		*material = sfp->rldnu/100;
		*shaded = sfp->closdinu/100;
		*lucency = sfp->closdinv/100;
		*ecolor = -1;

		sfp->primitive = sfp->primitive-(*numu*100);
		sfp->rev_normal = sfp->rev_normal-(*numv*100);
		sfp->rldnu = sfp->rldnu-(*material*100);
		sfp->closdinu = sfp->closdinu-(*shaded*100);
		sfp->closdinv = sfp->closdinv-(*lucency*100);
		status = ur_update_data_fixed(&sf);
	}
	else if (sf.rel_num == NCL_MESHSURF_REL) 
	{
		struct NCL_meshsf_rec *sfp;
		sfp = (struct NCL_meshsf_rec *)&sf;

		*numu = sfp->surf_type/100;
		*numv = sfp->rev_normal/100;
		*peru = 0;
		*perv = 0;
		*material = sfp->rldnu/100;
		*shaded = sfp->closdinu/100;
		*lucency = sfp->closdinv/100;
		*ecolor = -1;

		sfp->surf_type = sfp->surf_type-(*numu*100);
		sfp->rev_normal = sfp->rev_normal-(*numv*100);
		sfp->rldnu = sfp->rldnu-(*material*100);
		sfp->closdinu = sfp->closdinu-(*shaded*100);
		sfp->closdinv = sfp->closdinv-(*lucency*100);
		status = ur_update_data_fixed(&sf);
	}
	else if (sf.rel_num == NCL_NETSF_REL)
	{
		struct NCL_netsf_rec *sfp;
		sfp = (struct NCL_netsf_rec *)&sf;

		*numu = UM_srfattr.numupaths; 
		*numv = UM_srfattr.numvpaths; 
		*peru = UM_srfattr.ptsperucrv;
		*perv = UM_srfattr.ptspervcrv;
		*material = sfp->surf_type/100;
		*shaded = sfp->ldrloc[0]/10000;
		*lucency = sfp->ldrloc[1]/10000;
		*ecolor = -1;
		if (UM_srfattr.edge_disp)
		{
			ecolor0 = UM_srfattr.edge_color;

			if (ecolor0 == 0)
				*ecolor = 16;
			else
				*ecolor = ecolor0 - 1;
		}

		sfp->surf_type = sfp->surf_type-(*material*100);
		sfp->ldrloc[0] = sfp->ldrloc[0]-(*shaded*100);
		sfp->ldrloc[1] = sfp->ldrloc[1]-(*lucency*100);
		status = ur_update_data_fixed(&sf);
	}
	else if (sf.rel_num == NCL_TRIMSF_REL)
	{
		struct NCL_trimsf_rec *sfp;
		sfp = (struct NCL_trimsf_rec *)&sf;

		*numu = sfp->ldrloc[0]/10000;
		*numv = sfp->ldrloc[1]/10000;
		*peru = 0;
		*perv = 0;
		*material = sfp->ldrloc[2]/10000;
		*shaded = sfp->closdinu/100;
		*lucency = sfp->closdinv/100;
		*ecolor = -1;

		sfp->ldrloc[0] = sfp->ldrloc[0]-(*numu*10000);
		sfp->ldrloc[1] = sfp->ldrloc[1]-(*numv*10000);
		sfp->ldrloc[2] = sfp->ldrloc[2]-(*material*10000);
		sfp->closdinu = sfp->closdinu-(*shaded*100);
		sfp->closdinv = sfp->closdinv-(*lucency*100);
		status = ur_update_data_fixed(&sf);
	}
	else if (sf.rel_num == NCL_REVSURF_REL)
	{
		struct NCL_revsurf_rec *sfp;
		sfp = (struct NCL_revsurf_rec *)&sf;

		*numu = sfp->primitive/100;
		*numv = sfp->rev_normal/100;
		*peru = 0;
		*perv = 0;
		*material = sfp->swapuv/100;
		*shaded = sfp->closdinu/100;
		*lucency = sfp->closdinv/100;
		*ecolor = -1;

		sfp->primitive = sfp->primitive-(*numu*100);
		sfp->rev_normal = sfp->rev_normal-(*numv*100);
		sfp->swapuv = sfp->rldnu-(*material*100);
		sfp->closdinu = sfp->closdinu-(*shaded*100);
		sfp->closdinv = sfp->closdinv-(*lucency*100);
		status = ur_update_data_fixed(&sf);
	}
	else if (sf.rel_num == NCL_SHAPE_REL)
	{
		struct NCL_shape_rec *sfp;
		sfp = (struct NCL_shape_rec *)&sf;

		*numu = sfp->ldrloc[0]/10000;
		*numv = sfp->ldrloc[1]/10000;
		*peru = 0;
		*perv = 0;
		*material = sfp->ldrloc[2]/10000;
		*shaded = sfp->f2d3d/100;
		*lucency = sfp->labloc[0]/10000;
		*ecolor = -1;

		sfp->ldrloc[0] = sfp->ldrloc[0]-(*numu*10000);
		sfp->ldrloc[1] = sfp->ldrloc[1]-(*numv*10000);
		sfp->ldrloc[2] = sfp->ldrloc[2]-(*material*10000);
		sfp->f2d3d = sfp->f2d3d-(*shaded*100);
		sfp->labloc[0] = sfp->labloc[0]-(*lucency*10000);
		status = ur_update_data_fixed(&sf);
	}

	uu_dexit;
	return(status);
}
