/*********************************************************************
**    NAME         :  neub2sup1.c
**       CONTAINS:
**       ncl_set_ubcopy(copy)
**       int ncl_get_ubcopy()
**       ncl_set_surfattr_type()
**       ncl_reset_surfattr_type()
**       UU_LOGICAL ncl_old_sflists (type)
**       UU_LOGICAL ncl_create_surfattr (rel)
**       UU_LOGICAL ncl_update_surfattr (rel)
**       UU_LOGICAL ncl_old_sflists (type)
**       int ncl_ubcopy_ent (eptr)
**       int ncl_ubcopy_compon (e1)
**       int ncl_ubget_compon (e1)
**			ncl_ranfilt(eptr,savstat)
**			ncl_fix_curve (eptr)
**		   ncl_get_type(rel_num, ncltype)
**			ncl_randel(key)
**			ncl_get_subscript(eptr)
**		   ncl_store_wf2(key,rel_num,label)
**
**    MODULE NAME AND RELEASE LEVEL
**			neub2sup1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:08:55
************************************************************************/
#include "usysdef.h"
#include "mfort.h"
#include "xenv1.h"
#include "mdrel.h"
#include "class.h"
#include "nclfc.h"
#include "mcrv.h"
#include "mxxx.h"
#include "msol.h"
#include "nccs.h"
#include "mattr.h"
#include "udebug.h"
#include "mdattr.h"
#include "mdcoord.h"
#include "msrfddl.h"
#include "nclmodals.h"
#include "mlab.h"
#include "ncl.h"
#include "nclvx.h"
#include "nclver.h"
#include "rienvtab.h"
#include "rver9400.h"
#include "view.h"
#include "mdunits.h"
#include "bsym.h"
#include "adrf.h"
#include "adraft.h"

int NCL_ubcopy = 0;
UM_transf *REFTM = UU_NULL, *MODTM = UU_NULL;
UM_transf REF_tm, MOD_tm;

extern UU_LOGICAL NCL_merge_overwrite, UR_EOF_stat;
extern char UBopen[100];
extern int lub2, UR_active;
/*
...
... Defined in ur_lp02b the lowest key of the geometries merged.
... Also used in ur_lp02c
...
 */
extern UU_KEY_ID LABTBL_KEY, UR_low_key;
UU_KEY_ID LABTBL2_KEY = 0;
extern UU_LOGICAL ncl_where, UR_load_env;
extern int UR_readenv;
extern UU_LOGICAL NCL_create_compcrv;
extern UU_LOGICAL NCL_copy_compcrv;
extern UX_pathname UR_exnam[2];
extern struct UR_env_table_rec   UR_environ_table[];

static UU_LOGICAL NCL_surfattr = UU_FALSE;

/*********************************************************************
**    E_FUNCTION     : ncl_set_ubcopy(copy)
**       Set NCL_ubcopy so external routines can call ncl_ubcopy_ent
**       with NCL_ubcopy set as needed.
**    PARAMETERS
**       INPUT  :   
**          copy  - New value for NCL_ubcopy.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/			
void ncl_set_ubcopy(copy)
int copy;
{
	NCL_ubcopy = copy;
}
/*********************************************************************
**    E_FUNCTION     : ncl_get_ubcopy(copy)
**       Get NCL_ubcopy.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : NCL_ubcopy
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/			
int ncl_get_ubcopy()
{
	return(NCL_ubcopy);
}
/*********************************************************************
*********************************************************************/			
void ncl_set_surfattr_type()
{
	NCL_surfattr = UU_TRUE;
}

/*********************************************************************
*********************************************************************/			
void ncl_reset_surfattr_type()
{
	NCL_surfattr = UU_FALSE;
}

/*********************************************************************
*********************************************************************/			
UU_LOGICAL ncl_surfattr_type (rel)
int rel;
{
	return (rel == NCL_TRIMSF_REL || rel == NCL_SURF_REL ||
		rel == NCL_MESHSURF_REL || rel == NCL_REVSURF_REL ||
		rel == UM_RBSPLSRF_REL || rel == NCL_SHAPE_REL || rel == NCL_NETSF_REL);
}

/*********************************************************************
*********************************************************************/			
UU_LOGICAL ncl_create_surfattr (rel)
int rel;
{
	return (ncl_surfattr_type (rel) && NCL_surfattr);
}

/*********************************************************************
*********************************************************************/			
UU_LOGICAL ncl_update_surfattr (rel)
int rel;
{
	return (ncl_surfattr_type (rel) && !NCL_surfattr);
}

/*********************************************************************
*********************************************************************/			
UU_LOGICAL ncl_old_sflists (type)
int type;
{
	if (NCL_infile_version < 9.649)
	{
		if (type == NCL_TRIMSF_REL || type == NCL_SURF_REL ||
			type == NCL_MESHSURF_REL || type == NCL_REVSURF_REL ||
			type == UM_RBSPLSRF_REL)
			return (UU_TRUE);
	}
	return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_ent (eptr,iren)
**       Copy entity from unibase 2 to unibase 1 (NCL unibase).
**    PARAMETERS
**       INPUT  :   eptr - pointer to the entity
**                  iren - 1 = Use NCL's autonaming feature to rename geo.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ubcopy_ent (e1,iren,offset,copied)
struct NCL_fixed_databag *e1;
int iren,offset;
UU_LOGICAL *copied;
{
	struct NCL_fixed_databag e2;
	UM_transf tran;
	struct UC_attributedatabag attr;
	struct  NCL_nclattr_rec *nattr;
	struct UM_transf_rec transpacket;
	int status,type,i,j,lbloc,layer_number,num_geom,rel_num;
	UM_int2 i2stat,idst;
	UU_LOGICAL savstat, lgeo, blank, outerr,lcomp;
	UM_coord pos;
	struct UM_rbsplsrf_rec *srf;
	struct UM_solid_rec *solid;
	short ist, ityp;
	struct UA_generic_draft draft;
	UU_KEY_ID key,keys[2];

	status = UU_SUCCESS;
	NCL_surfattr = UU_FALSE;
	*copied = UU_FALSE;
/*
.....save the parser 'ist' and 'ityp' because the transfer will change it
.....but we don't want it to be changed after transfer here
*/
	keys[0] = e1->key;
	getist(&ist);
	getityp(&ityp);

	type = e1->rel_num;
/*
.....vp 10/1/97 uv curves not supported in this release
.....temporary ignore all uv curves on surface
*/
	if (type == UM_UVCVONSF_REL) goto done;

/*
...Check if entity is not a part of the "composite" entity
*/
	if (!strncmp(e1->label,"@UN    ",strlen(e1->label))) 
	{
/*
.....set the entities with a @UN label to NEVER DISPLAYABLE.
*/
		status = ur_update_displayable(e1->key, UM_NEVERDISPLAYABLE);
		goto done; 
	}
	if (!strncmp(e1->label,"@",1)) goto done; 
	savstat = UU_FALSE;
	lgeo    = UU_FALSE;
	lbloc   = 0;
/*
.....Rename geometry if requested
*/
	if (iren == 1)
	{
		if (type != UB_SYMBOL_REL && type != UB_INSTANCE_REL)
		{
			ncl_get_type(e1->rel_num,&idst);
			if (idst != 0 && idst != NCLI_SCALAR)
				ncl_label_wf(e1->rel_num,e1->label,&e1->subscr,e1->key,&i2stat);
		}
		else
		{
			ncl_ubcheck_symnam(&e1->label,e1->subscr,type,&savstat,UU_FALSE);
			if (!savstat) ncl_ubchg_symnam(e1,type);
		}
	}
/*
...Get transformation matrix, attributes & label location 
...before switching to work unibase
*/
	status = ur_retrieve_blanked (e1->key,&blank);
	if (!status) status = uc_retrieve_transf (e1->key,tran);
	if (status == UU_SUCCESS)
	{
		status = uc_retrieve_attr (e1->key,&attr);
	}
	if (status == UU_SUCCESS && (wf_geom_type(type) == 0 ||
									  ncl_geom_type(type) == 0))
	{
		lgeo    = UU_TRUE;
		nattr  = (struct  NCL_nclattr_rec *) &(attr);
/*
......vp 2/12/98 removed label table support
*/
/*
.....check if label display is on and has been altered
*/
		if (ncl_get_label_on(nattr->label_on) && 
				ncl_get_label_alter(nattr->label_on)) 
		{
			lbloc = nattr->label_on;
			if (ncl_retrieve_labloc (e1,nattr->label_on,pos) != 0) 
			{ lbloc = 0; nattr->label_on = 0; }
		}
	}

	if (status == UU_SUCCESS && ncl_surfattr_type (e1->rel_num) && attr.rel_num == UM_SURFATTR_REL)
		NCL_surfattr = UU_TRUE;
/*
...Get components for "composite" entity
*/
	if (status == UU_SUCCESS)
		 status = ncl_ubget_compon (e1);
	if (status != UU_SUCCESS) goto done;
/*
...Check if label exists in ranfile
.....Draft entities have no label and so checking is unnecessary.
.......It is assumed that all draft entities are retrieved at the
.......same time so no check is made.
*/
	ur_getu_work();
	if (type != UA_LINEAR_DIMS_REL && type != UB_SYMBOL_REL &&
		type != UB_INSTANCE_REL)
		ncl_ranfilt (e1,&savstat,&key);  
	else if (type == UB_SYMBOL_REL || type == UB_INSTANCE_REL)
	{
		outerr = (iren == 0);
		ncl_ubcheck_symnam(&e1->label,e1->subscr,type,&savstat,outerr);
	}
	else
		savstat = UU_TRUE;
	*copied = savstat;
	if (savstat)
	{
/*
...Create entity in primary unibase and
...update ranfile
*/
		NCL_create_compcrv = UU_FALSE;
		NCL_copy_compcrv = UU_FALSE;
		NCL_ubcopy = 1; 
		ncl_ubcopy_set_ptrs();
/*
.....Momentarily set srf->no_sskey to 0, they will be
.....copied over in ncl_ubcopy_rbsf. JLS 11/8/99
*/ 
		if (type == UM_RBSPLSRF_REL)
		{
			srf = (struct UM_rbsplsrf_rec *) e1;
			i = srf->no_sskey;
			srf->no_sskey = 0;
		}
/*
.....Determine if this is a composite entity
*/
		lcomp = UU_FALSE;
		if (type == UM_COMPCRV_REL || type == NCL_NETSF_REL ||
			type == NCL_TRIMSF_REL || type == NCL_SURF_REL ||
			type == NCL_REVSURF_REL || type == UB_INSTANCE_REL ||
			type == UB_SYMBOL_REL || type == UA_LINEAR_DIMS_REL) lcomp = UU_TRUE;
		if (type == UM_SOLID_REL)
		{
			solid = (struct UM_solid_rec *)e1;
			if (solid->type == UM_COMPOS_SOLID) lcomp = UU_TRUE;
		}
/*
.....Create non-composite geometry
*/
		if (!lcomp)
			status = um_create_geom (e1,UM_DEFAULT_TF,&attr);
		if (type == UM_RBSPLSRF_REL) srf->no_sskey = i;
/*
.....Create composite geometry
*/
		status = ncl_ubcopy_compon (e1,iren,offset);
/*
...Get created entity to apply refsys & disply 
*/
		if (status == UU_SUCCESS)
		{
			keys[1] = key = e1->key;
			ncl_ubput_keys(1,&keys[0],&keys[1]);
			if (type != UA_LINEAR_DIMS_REL)
			{
				e2.key = key;
				status = ncl_retrieve_data_fixed(&e2);
				rel_num = e2.rel_num;
			}
			else
			{
				draft.key = key;
				draft.rel_num = e1->rel_num;
				ua_retrieve_data(&draft,sizeof(struct UA_generic_draft));
				rel_num = draft.rel_num;
			}
		}
/*
...Copy label location in work label table record, then
...update attributes and transf according to input entity
*/
	 	if (status == UU_SUCCESS)
		{
			attr.key = key;

			if (lbloc < 0 || lbloc > 1)
			{
				if (REFTM != UU_NULL) um_cctmtf (pos,REFTM,pos);
				if (MODTM != UU_NULL) um_cctmtf (pos,MODTM,pos);
				if (type != UA_LINEAR_DIMS_REL)
					ncl_add_labloc (&e2,&i,pos);
				nattr->label_on = lbloc;
			}

			status = ur_update_attr(&attr);

			if (ncl_update_surfattr(rel_num))
			{
				ur_update_attribut (key,rel_num);
			}

			if (!status) status = ur_update_blanked (key,blank);
		}
		if (status == UU_SUCCESS) 
		{
			transpacket.key = key;
			transpacket.rel_num = UM_TRANSFORM_REL;
			ur_retrieve_transf(&transpacket);
			um_tftotf(tran, transpacket.tfmat);
			status = ur_update_transf(&transpacket);
		}
		if (status == UU_SUCCESS)
		{
			if (type != UA_LINEAR_DIMS_REL)
			{
				if (REFTM != UU_NULL)
					status = uc_transform (&e2, REFTM, UU_TRUE);
				if (MODTM != UU_NULL)
					status = uc_transform (&e2, MODTM, UU_TRUE);
			}
			else
			{
				if (REFTM != UU_NULL)
					status = uc_transform (&draft, REFTM, UU_TRUE);
				if (MODTM != UU_NULL)
					status = uc_transform (&draft, MODTM, UU_TRUE);
			}
			if (REFTM || MODTM || ncl_old_sflists(type)) 
			{
				ncl_lst_delete(TESSELLATION_LIST,&key);
				ncl_lst_delete (DISPLAY_LIST,&key);
				ncl_lst_delete(WHOLE_BOUNDARY_LIST, &key);
			}
		}
		if (type != UA_LINEAR_DIMS_REL) uig_match_updatts(&e2,6);
		NCL_ubcopy = 0; 
		if (status == UU_SUCCESS)
			um_copy_layer_struct(attr.layer,offset, 1);
		if (status == UU_SUCCESS)
		{
			ur_retrieve_layer(e1->key, &layer_number);
			layer_number = layer_number + offset;
			ur_update_layer(e1->key, layer_number);
		}
/*
...Display entity
...  vp 12/4/97 load entity again in case if it is tranformed 
...  so display list has been reset
*/
		if (rel_num == NCL_CURVE_REL) ncl_fix_curve (&e2);
		if (lgeo || type==UB_INSTANCE_REL)// || type==UA_LINEAR_DIMS_REL)
		{
	 		if (type != UA_LINEAR_DIMS_REL) ncl_retrieve_data_fixed(&e2); 
			ur_update_view_key(key, ur_get_dispattr_view_in());
			if (lgeo) uc_display(&e2); 
			else ub_display_sym(&e2);
		}
		else if (type==UA_LINEAR_DIMS_REL) ua_display_drafting(&draft);
	}
done:;
	ur_getu_second();
	ncl_ublist_empty();
	setist(&ist);
	setityp(&ityp);
	NCL_surfattr = UU_FALSE;

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_compon(eptr)
**        Get entity from unibase and load into global structure to 
**        store in primary unibase using specific routines.  If it is
**        composite entity then all keyed components are processed.
**        If the entity contains a list pointer than all pointers are
**        updated. For all trivial entity bypass. 
**    PARAMETERS
**       INPUT  :
**          e1            entity pointer
**          iren          rename flag
**          offset        layer offset value
**       OUTPUT :
**          none          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ubcopy_compon (e1,iren,offset)
struct NCL_fixed_databag *e1;
int iren,offset;
{
	int type, status;

	type  = e1->rel_num;
	switch (type)
	{
		case NCL_SURF_REL:
			status = ncl_ubcopy_nsf (e1);
			break;
		case NCL_REVSURF_REL:
			status = ncl_ubcopy_revsf (e1);
			break;
		case UA_TEXT_REL: 
			status = ncl_ubcopy_anote (e1);
			break;
		case UM_COMPCRV_REL:
			status = ncl_ubcopy_ccv (e1); 
			break;
		case NCL_NETSF_REL:
			status = ncl_ubcopy_netsf (e1);
		 	break;
		case NCL_TRIMSF_REL:
			status = ncl_ubcopy_trimsf (e1);
			break;
		case UM_RBSPLSRF_REL:
			status = ncl_update_list_rbsf (e1); 
			break;
		case UM_RBSPLCRV_REL:
			status = ncl_update_list_rbcv (e1); 
			break;
		case UM_SOLID_REL:
			status = ncl_ubcopy_solid(e1); 
			break;
		case NCL_PATERN_REL:
			status = ncl_update_list_patrn (e1); 
			break;
		case NCL_CURVE_REL:
			status = ncl_update_list_nclcv (e1);
			break;
/*
...vp 2/18/98 missing link has been added
*/
		case NCL_MESHSURF_REL:
			status = ncl_update_list_mshsf (e1);
			break;
		case NCL_EVALSF_REL:
			status = ncl_update_list_evalsf (e1);
			break;
		case NCL_EVALCV_REL:
			status = ncl_update_list_evalcv (e1);
			break;
		case NCL_SHAPE_REL:
			status = ncl_update_list_shape (e1);
			break;
		case UB_SYMBOL_REL:
			status = ncl_ubcopy_symdata (e1,2);
			break;
		case UB_INSTANCE_REL:
			status = ncl_ubcopy_symdata (e1,0);
			break;
		case UA_LINEAR_DIMS_REL:
			status = ncl_ubcopy_draft (e1,iren,offset);
			break;
		default:
			status = UU_SUCCESS;
			break;
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubget_compon(eptr)
**        Get composite entity from unibase and load into global
**        structure each keyed component using specific routine. 
**    PARAMETERS
**       INPUT  :
**          eptr            entity pointer
**       OUTPUT :
**          none          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ubget_compon (e1)
struct NCL_fixed_databag *e1;
{
	int type, status;

	type  = e1->rel_num;
	switch (type)
	{
		case NCL_SURF_REL:
			status = ncl_ubget_nsf (e1);
			break;
		case UM_COMPCRV_REL:
			status = ncl_ubget_ccv (e1);
			break;
		case NCL_NETSF_REL:
			status = ncl_ubget_netsf (e1);
			break;
		case NCL_TRIMSF_REL: 
			status = ncl_ubget_trimsf (e1);
			break;
/*
.....Added rbsplsrf because we want to get the
.....crvonsrfs if there are any. JLS 11/10/99
*/
		case UM_RBSPLSRF_REL:
			status = ncl_ubget_sskeys (e1);
			break;
		case NCL_REVSURF_REL: 
			status = ncl_ubget_revsf (e1);
			break;
		case UA_TEXT_REL: 
			status = ncl_ubget_anote (e1);
			break;
		case UB_SYMBOL_REL:
			status = ncl_ubget_symdata (e1,2);
			break;
		case UB_INSTANCE_REL:
			status = ncl_ubget_symdata (e1,0);
			break;
		case UA_LINEAR_DIMS_REL:
			status = ncl_ubget_draft (e1);
			break;
		case UM_SOLID_REL:
			status = ncl_ubget_solid(e1);
			break;
		default:
			status = UU_SUCCESS;
			break;
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ranfilt(eptr,savstat)
**        Check the NCL ranfile with a new entity name. If geometry's
**        label exist and canon/on remove it from both, unibase
**        and ranfile, if ca/off disable storing. 
**    PARAMETERS
**       INPUT  :
**          eptr    - entity pointer
**          savstat - setting to false will check for label and output
**                    error if label exists and canon is off
**                  - setting to true will check for label but will
**                    skip the error display
**       OUTPUT :
**          savstat - status: .TRUE. if entity can be added
**                          to the unibase.
**          key     - key of entity if found
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ranfilt(eptr,savstat,rkey)
struct UC_entitydatabag *eptr;
UU_LOGICAL *savstat;
UU_KEY_ID *rkey;
{
	union {                  /* to force double word alignment of label */
		double junk;
		char label[NCL_MAX_LABEL];
		} ncl;
	UM_int2 ncltype;
	UM_int4 nclsubscript;
	UM_int4 nclkey,ipg,iel;
	UM_f77_str ncllabel;
	struct NCL_vector_rec *e;
	UU_KEY_ID key;
	int i, nclrel,nc;
	int status,ifnd;
	UU_LOGICAL nclg,errflg;
	UM_int2 nwds,ietype,ix,ifl;

	uu_denter(UU_MTRC,(us,"ncl_ranstr(key=%x relnum=%d)",
		eptr->key, eptr->rel_num));

	errflg = *savstat;
	*savstat = UU_TRUE;
	status  = ncl_get_type (eptr->rel_num, &ncltype);
	if (status == UU_SUCCESS)
	{
		nclg  = UU_FALSE;
		if ((ncl_geom_type(eptr->rel_num)==UU_SUCCESS)||
			 (wf_geom_type(eptr->rel_num)==UU_SUCCESS) ||
			 (ncltype == NCLI_SCALAR || ncltype == NCLI_MATRIX ||
			  ncltype == NCLI_DATAST || ncltype == NCLI_TEXTVAR ||
			  ncltype == NCLI_SYMBOL || ncltype == NCLI_INSTANCE)) 
		{
			nclg  = UU_TRUE;
			e = (struct NCL_vector_rec *) eptr;
			if (ncltype == 2 && strcmp (e->label, "       ") == 0)
			{
				goto done;
			}
/* this should be geometry which makes up net surface ... */
/* don't put in ranfile */
			else if (ncltype == 9 && (strcmp (e->label,"") == 0))
				goto done;

/* this should be geometry which makes up composite curves ... */
/* don't put in ranfile */
			else if (strcmp (e->label, "@UN    ") == 0)
				goto done;
/*
.....Labels starting with a '#' are drawing entities
.....Do not store in ranfile
.....Bobby  -  2/28/94
*/
			else if (e->label[0] == '#') goto done;

		for (i=0; i<NCL_MAX_LABEL; i++) ncl.label[i] = e->label[i];
			nclsubscript = ncl_get_subscript(e);

		UM_init_f77_str(ncllabel, ncl.label, NCL_MAX_LABEL);
/*
...  check to see if label has already been defined.
*/
		ifnd = vxchk(UM_addr_of_f77_str(ncllabel), &nclsubscript, &nclkey, &ipg,
			&iel,&nwds,&ietype);
		*rkey = nclkey;
		ix = 41;
		getifl(&ix,&ifl);
		if (ifnd == UU_SUCCESS)
/* 
...  Geometry label already defined and canon is off
*/
			if (ifl == 0 && e->rel_num != NCL_SCALAR_REL &&
				e->rel_num != NCL_TEXTVAR_REL)
			{
/*
.....Don't know why this check is made
.....but it causes subscripted geometry
.....to be overwritten (GET/TP(ALL) followed by another GET/TP(ALL))
.....Bobby  -  02/15/01
*/
/*				if (!(nclsubscript >0 && nclsubscript >= 
				 	UM_labelmdl.subscr[UM_labelmdl.rel[e->rel_num]]))*/
/*
.....Added error flag so entities could be searched for without writing
.....an error
*/
				if (!errflg)
				{
				 	wintst();
					nc = strlen(ncllabel);
					for (i=nc;i<NCL_MAX_LABEL;i++) ncllabel[i] = ' ';
				 	uerror(UM_addr_of_f77_str(ncllabel), &nclsubscript);
					ncllabel[nc] = '\0';
				}
				*savstat = UU_FALSE;
			}
/*
...   Geometry label already defined and canon is on 
*/
			else if (ifl == 1)
			{
				key = nclkey;
				ur_retrieve_data_relnum(key,&nclrel);
				uc_delete(nclkey);
				ncl_randel(nclkey, nclrel);
			}
		}
	}

done:;
	return(0);
}

/*********************************************************************
**    E_FUNCTION: ncl_fix_curve (eptr)
**       Add curve length to the NCL curve data structure.  
**       NOTE: The length calculated here is only in curve original 
**       definition space; Can not be called for any trimmed CV.
**    PARAMETERS   
**       INPUT  : 
**          eptr     pointer to NCL curve record
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_fix_curve (eptr)
struct NCL_curve_rec *eptr;
{
	int status = UU_SUCCESS;

	if (eptr->t_end < 0.)
	{
		eptr->t_end = um_getarclen (eptr,UM_idmat);
		status = ur_update_data_fixed (eptr);
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_type(rel_num, ncltype)
**       Get the ncl geometry type from rel_num
**    PARAMETERS   
**       INPUT  : 
**          rel_num
**          ncltype
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			The only data which must be initialized in the entity 
**			are the UNIBASE key and relation number.
*********************************************************************/
int
ncl_get_type(rel_num, ncltype)
int     rel_num;
UM_int2 *ncltype;

{
	int status;
	uu_denter(UU_MTRC,(us,"ncl_get_type(rel_num=%d)", rel_num));

	status = UU_SUCCESS;
	switch (rel_num)
	{
		case UM_POINT_REL:
		case NCL_POINT_REL:
			*ncltype = NCLI_POINT;
			break;
		case UM_LINE_REL:
		case NCL_LINE_REL:
			*ncltype = NCLI_LINE;
			break;
		case UM_CIRCLE_REL:
		case NCL_CIRCLE_REL:
			*ncltype = NCLI_CIRCLE;
			break;
/*      case UM_PLN_REL: */
		case NCL_PLN_REL:
			*ncltype = NCLI_PLANE;
			break;
		case NCL_VECTOR_REL:
			*ncltype = NCLI_VECTOR;
			break;
		case UM_CONIC_REL:
		case UM_COMPCRV_REL:
		case UM_RBSPLCRV_REL:
		case UM_AGCRV_REL:
		case UM_UVCVONSF_REL:
		case NCL_CURVE_REL:
		case NCL_EVALCV_REL:
		case UM_POLYLINE_REL:
			*ncltype = NCLI_CURVE;
			break;
		case UM_AGSRF_REL:
		case UM_AGSHELL_REL:
		case NCL_SURF_REL:
		case NCL_REVSURF_REL:
		case NCL_MESHSURF_REL:
		case NCL_QUILTSURF_REL:
		case NCL_NETSF_REL:
		case NCL_EVALSF_REL:
		case UM_RBSPLSRF_REL:
		case NCL_TRIMSF_REL:
			*ncltype = NCLI_SURF;
			break;
		case NCL_MATRIX_REL:
			*ncltype = NCLI_MATRIX;
			break;
		case NCL_PATERN_REL:
			*ncltype = NCLI_PATERN;
			break;
		case NCL_SHAPE_REL:
			*ncltype = NCLI_SHAPE;
			break;
		case NCL_SCALAR_REL:
			*ncltype = NCLI_SCALAR;
			break;
		case NCL_POINTVEC_REL:
			*ncltype = NCLI_POINTVEC;
			break;
		case NCL_DATAST_REL:
			*ncltype = NCLI_DATAST;
			break;
		case NCL_TEXTVAR_REL:
			*ncltype = NCLI_TEXTVAR;
			break;
		case UA_TEXT_REL:
			*ncltype = NCLI_NOTE;
			break;
		case UB_SYMBOL_REL:
			*ncltype = NCLI_SYMBOL;
			break;
		case UB_INSTANCE_REL:
			*ncltype = NCLI_INSTANCE;
			break;
		case UM_SOLID_REL:
			*ncltype = NCLI_SOLID;
			break;
		default:
			*ncltype = 0;
			status = UU_FAILURE;
			break;
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_randel(key)
**       If the specified entity (KEY) has an entry in the NCL
**         ranfile, it will be deleted.
**    PARAMETERS
**       INPUT  :
**          key               UNIBASE key
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff ranfile entry deleted; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_randel(key, rel_num)
   UU_KEY_ID key;
   int rel_num;

   {
   int status;
   UM_int4 nclkey;
   UM_int2 ncltype;
   UM_int2 nclstatus;
   UU_LOGICAL ncl_legal_relation();

   uu_denter(UU_MTRC,(us,"ncl_randel(key=%d)", key));

   status = UU_FAILURE;
   nclkey = key;
   ncl_get_type( rel_num, &ncltype);
   vxdlk(&nclkey, &ncltype, &nclstatus);
   if (nclstatus == 0) status = UU_SUCCESS;

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION :  int uc_retrieve_attr(key, attrptr)
**       This function retrieves entity data from UNIBASE.
**    PARAMETERS
**       INPUT  :
**          key            key of the entity for which a attribute bundle is
**                         to be retrieved.
**       OUTPUT :
**          attrptr        pointer to the retrieved attribute bundle.
**    RETURNS      : UU_SUCCESS ifno problems encountered.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*int uc_retrieve_attr(key, attrptr)
UU_KEY_ID key;
struct UC_attributedatabag *attrptr;
   {
   UC_METHOD function, ucu_validate();
   int rel_num;
   int status;
   int class;

   uu_denter(UU_MTRC,(us,"uc_retrieve_attr(key:%d,attrptr:%x)", key, attrptr));
   status = um_retrieve_data_relnum(key, &rel_num);
   class = CONVERT_REL_TO_CLASS(rel_num);
   if(status == 0)
      {
      function = ucu_validate(&UC_cot_descriptor, class, UC_RETRIEVE_ATTR);
      if(function == UC_UNDEFINED_METHOD)
         status = UU_FAILURE;
      else
         status = (*function)(key, attrptr);
      }
   uu_dexitstatus("uc_retrieve_attr", status);
   return(status);
   }
*/

/*********************************************************************
**    E_FUNCTION     : ncl_get_subscript(eptr)
**        Retrive entity subscript from unibase data structure.
**    PARAMETERS
**       INPUT  :
**          eptr            entity pointer
**       OUTPUT :
**          none
**    RETURNS      : entity subscript
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_subscript (e1)
struct NCL_fixed_databag *e1;
{
   struct NCL_scalar_rec *escl;
   if (e1->rel_num == NCL_SCALAR_REL || e1->rel_num == NCL_DATAST_REL ||
       e1->rel_num == NCL_TEXTVAR_REL)
   {
      escl = (struct NCL_scalar_rec *) e1;
      return (escl->subscr);
   }
      return (e1->subscr);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_store_wf2(key,rel_num,label)
**       Store a wf entity label in the ranfile.
**    PARAMETERS   
**       INPUT  : 
**          key           key of entity.
**          rel_num       rel_num of entity
**          label         label of entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_store_wf2(key,rel_num,label,sub)
   UU_KEY_ID key;
   int rel_num, sub;
   char *label;

{
   UM_f77_str ncllabel;
   char str[NCL_MAX_LABEL];
   UM_int2 idst;
   UM_int4 nclkey, isub;
   int i;
   int status;

   uu_denter(UU_MTRC, (us,"ncl_store_wf2(key=%d)", key));
   status = UU_SUCCESS;
/*
.....Don't store drawing entities in ranfile
.....nor @UN labeled geometry
*/
 if (UM_2d3d_mode == UM_3D && strncmp(label,"@UN",3) != 0)
 {
   UM_init_f77_str(ncllabel, str, NCL_MAX_LABEL);
   switch (rel_num)
      {
      case UM_POINT_REL:
         idst = NCLI_POINT;
         break;
      case NCL_POINTVEC_REL:
         idst = NCLI_POINTVEC;
         break;
      case UM_LINE_REL:
         idst = NCLI_LINE;
         break;
      case NCL_VECTOR_REL:
         idst = NCLI_VECTOR;
         break;
      case UM_CIRCLE_REL:
         idst = NCLI_CIRCLE;
         break;
      case NCL_PLN_REL:
         idst = NCLI_PLANE;
         break;
      case UM_CONIC_REL:
      case UM_COMPCRV_REL:
      case UM_RBSPLCRV_REL:
      case UM_AGCRV_REL:
      case UM_UVCVONSF_REL:
      case NCL_EVALCV_REL:
      case NCL_CURVE_REL:
         idst = NCLI_CURVE;
         break;
      case NCL_SURF_REL:
      case NCL_EVALSF_REL:
      case UM_RBSPLSRF_REL:
      case NCL_MESHSURF_REL:
      case NCL_QUILTSURF_REL:
      case NCL_NETSF_REL:
      case NCL_TRIMSF_REL:
      case NCL_REVSURF_REL:
         idst = NCLI_SURF;
         break;
      case NCL_MATRIX_REL:
         idst = NCLI_MATRIX;
         break;
      case NCL_PATERN_REL:
         idst = NCLI_PATERN;
         break;
      case NCL_SHAPE_REL:
         idst = NCLI_SHAPE;
         break;
      case NCL_DATAST_REL:
         idst = NCLI_DATAST;
         break;
      case NCL_TEXTVAR_REL:
         idst = NCLI_TEXTVAR;
         break;
      case UM_SOLID_REL:
         idst = NCLI_SOLID;
         break;
      case UA_TEXT_REL:
         idst = NCLI_NOTE;
         break;
      default:
         idst = NCLI_SCALAR;  /* rest are surfaces for now */
         break;
      }

   for (i=0;i<NCL_MAX_LABEL;i++) str[i] = label[i];
   nclkey = key;
   isub = sub;
   strwf2(&nclkey, &idst, UM_addr_of_f77_str(ncllabel),&isub);
 }
   uu_dexit;
   return(status);
}
