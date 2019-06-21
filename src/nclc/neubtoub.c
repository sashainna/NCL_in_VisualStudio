/*********************************************************************
**    NAME         :  neubtoub.c
**       CONTAINS:
**       ncl_ubget_nsf (e1)
**       ncl_ubcopy_nsf (e1)
**       ncl_ubget_sskeys(sf)
**       ncl_update_sskeys(sf,ncv,lst)
**       ncl_update_list_rbsf (sf)
**       ncl_ubget_trimsf (e1)
**       ncl_ubcopy_trimsf (e1)
**       ncl_ubget_netsf (e1)
**       ncl_ubcopy_netsf (e1)
**       ncl_ubget_revsf (e1)
**       ncl_ubcopy_revsf (e1)
**       ncl_update_list_mshsf (sf)
**       ncl_update_list_evalsf (sf)
**       ncl_ubget_ccv (e1)
**       ncl_ubcopy_ccv (e1)
**       ncl_ubget_solid (e1)
**       ncl_ubcopy_solid (e1)
**       ncl_ubget_symdata (e1,type)
**       ncl_ubcopy_symdata (e1,type)
**       ncl_ubget_draft (e1)
**       ncl_ubcopy_draft (e1,iren,offset)
**       ncl_ubget_anote (e1)
**       ncl_ubcopy_anote (e1)
**       ncl_update_list_evalcv (cv)
**       ncl_update_list_nclcv (cv)
**       ncl_update_list_rbcv (cv)
**       ncl_update_list_patrn (pn)
**       ncl_update_list_shape (sh)
**       ncl_ublist_init ()
**       ncl_ublist_free ()
**       ncl_ublist_empty ()
**       ncl_ubent_memfree ()
**       ncl_ubent_memealoc (n)
**       ncl_ubent_netsf_memfree ()
**       ncl_ubent_netsf_memaloc ()
**       ncl_ubent_solid_memfree ()
**       ncl_ubent_solid_memaloc ()
**       ncl_ubent_symbol_memaloc(geom,text,snap)
**       ncl_retrieve_ent (ept,attpt,tmatpt)
**       ncl_createub_ent (ept,attpt,trmat)
**       ncl_updateub_ent (ept,attpt,trmat)
**       ncl_ubget_rbsf (e1)
**       ncl_ubcopy_create_geom (e1,attptr,tfmat)
**       ncl_ubcopy_set_ptrs ()
**
**    MODULE NAME AND RELEASE LEVEL
**       neubtoub.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:56
***********************************************************************/

#include "usysdef.h"
#include "mfort.h"
#include "xenv1.h"
#include "mdrel.h"
#include "class.h"
#include "nclfc.h"
#include "ulist.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include "nccs.h"
#include "mattr.h"
#include "udebug.h"
#include "mdattr.h"
#include "mdcoord.h"
#include "mdgenent.h"
#include "nclmodals.h"
#include "mlab.h"
#include "atext.h"
#include "bsym.h"
#include "uhep.h"                /* for error system */
#include "adraft.h"
#include "adrf.h"

extern int UR_active;
extern  ATXT_FRM  UB_txtattr;
extern UU_LOGICAL NCL_merge_overwrite;
UU_LOGICAL wind_stat;
extern UU_LOGICAL ncl_where;
extern UU_LOGICAL NCL_create_compcrv;
extern UU_LOGICAL NCL_copy_compcrv;
extern UM_transf *REFTM,*MODTM,REF_tm,MOD_tm;
extern int NCL_ubcopy;

UU_LIST PNL_ls,DTB_ls,ATT_ls,TRN_ls,CID_ls,GEO_ls,TXT_ls,SYM_ls,
	 SATT_ls,STRN_ls,ASO_ls;
UU_LIST SNP_ls;
struct NCL_panel_rec *PNL_pt;
int PNL_cnt = 0, PNL_num = 0;

struct UM_cid_rec *CID_pt;
struct NCL_fixed_databag *DTB_pt;
struct NCL_fixed_databag *SYM_pt;
struct UC_attributedatabag *ATT_pt;
UM_transf *TRN_pt;
struct UC_attributedatabag *SATT_pt;
UM_transf *STRN_pt;
UU_KEY_ID *GEO_pt;
struct UB_text_nod_rec *TXT_pt;
struct UB_snap_nod_rec *SNP_pt;
struct UA_asso_blk *ASO_pt;

struct UM_cid_rec *CID = UU_NULL;
struct NCL_fixed_databag *DTBAG = UU_NULL;
struct UC_attributedatabag *ATTR = UU_NULL;
UM_transf *TRANSP = UU_NULL;
struct NCL_fixed_databag *DTBAG_netsf = UU_NULL;
struct UC_attributedatabag *ATTR_netsf = UU_NULL;
UM_transf *TRANSP_netsf = UU_NULL;
static struct NCL_fixed_databag DTBAG2_fixed;
static struct NCL_fixed_databag *DTBAG2_netsf = UU_NULL;
static struct NCL_fixed_databag *DTBAG2 = &DTBAG2_fixed;
static struct UC_attributedatabag ATTR2;

struct NCL_fixed_databag *DTBAG_solid = UU_NULL;
struct UC_attributedatabag *ATTR_solid = UU_NULL;
static struct NCL_fixed_databag *DTBAG2_solid = UU_NULL;
UM_transf *TRANSP_solid = UU_NULL;

static UM_transf TRANSP2;
struct NCL_fixed_databag *SYMBAG = UU_NULL;


/*********************************************************************
**    E_FUNCTION     : ncl_ubget_nsf (e1)
**       Gets an NCL surface from unibase and stores it in the
**       global structure.
**    PARAMETERS
**       INPUT  :  e1 - pointer to NCL surface structure header.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_ubget_nsf (eptr)
struct NCL_fixed_databag *eptr;
{
	int ipan, ispan, status;
	struct NCL_panel_rec temp;
	struct NCL_surface_rec *e1;

	status = ncl_ubget_sskeys(eptr);
	if (status != UU_SUCCESS) return (UU_FAILURE);
	e1 = (struct NCL_surface_rec *)eptr;
	ispan  = e1->no_panelkey; 
	status = UU_SUCCESS;
/*
...Get all panels into temp global struct
*/
	for (ipan=0; ipan<ispan && status == UU_SUCCESS; ipan++)
	{
		temp.key = e1->panelkey[ipan];
		status = ncl_retrieve_data_fixed(&temp); 
		if (status != UU_FAILURE) uu_list_push (&PNL_ls, &temp);
		PNL_cnt++;
	}
	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubget_sskeys(e1)
**       If there are ssplines associated with the
**       surface, retrieve those entities.
**    PARAMETERS
**       INPUT  :  e1 - pointer to a surface structure
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ncl_ubget_sskeys(e1)
struct NCL_fixed_databag *e1;
{
	int status,i,ncv,lst;
	UU_KEY_ID *keys;
	UM_transf tmat;
	struct NCL_fixed_databag temp2;
	struct UC_attributedatabag tatr;
/*
...Allocate memory for the cvonsf's.
*/
	status = ncl_retrieve_sskeys (e1,&lst,&ncv,&keys);
	if (status != UU_SUCCESS) return (UU_FAILURE);
	
	if (ncv > 0)
	{
/*
.....Retrieve each cvonsf.
*/
		for (i=0; i<ncv && status == UU_SUCCESS; i++)
		{
			temp2.key = tatr.key = keys[i];
			if (temp2.key != 0)
			{
				status = ncl_retrieve_ent (&temp2,&tatr,&tmat);
				if (status  == UU_SUCCESS)
				{
					uu_list_push (&DTB_ls, &temp2);
					uu_list_push (&ATT_ls, &tatr);
					uu_list_push (&TRN_ls, &tmat);
					if (temp2.rel_num == UM_COMPCRV_REL)
						status = ncl_ubget_ccv (&temp2); 
				}
			} 
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_update_sskeys(e1,ncv,lst)
**       If there are ssplines associated with the
**       surface, retrieve those entities.
**    PARAMETERS
**       INPUT  :  e1 - pointer to a surface structure
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ncl_update_sskeys(e1,ncv,lst)
struct NCL_fixed_databag *e1;
int ncv,lst;
{
	int status,i;
	UU_KEY_ID *sskeys;
	struct NCL_fixed_databag *temp2;
	struct UC_attributedatabag *tatr;
	struct UM_uvcvonsf_rec *cvonsf;

	status = UU_SUCCESS;
	sskeys = UU_NULL;

	if (ncv > 0)
	{
/*
.....Create the cvonsfs that go with this surface.
*/
		NCL_create_compcrv = UU_FALSE;
		NCL_copy_compcrv = UU_FALSE;

		sskeys  = (UU_KEY_ID *) uu_toolmalloc((ncv)* sizeof(UU_KEY_ID));
		if (sskeys == UU_NULL) status = UU_FAILURE;

		for (i=0; i<ncv && status == UU_SUCCESS; i++)
		{
			temp2 = DTB_pt;
			tatr = ATT_pt;
			temp2->key = 0;
			DTB_pt++; ATT_pt++; TRN_pt++;
			cvonsf = (struct UM_uvcvonsf_rec *) temp2;
/*
.....set base surface to 0 (we already have it (DTBAG)) a fatal
.....error will occur if it is not set to 0. JLS 11/8/99
*/
			cvonsf->bskey = 0;
/*
.....Create each cvonsf.
*/
			status = um_create_geom (temp2,UM_DEFAULT_TF,tatr);
			cvonsf->bskey = e1->key;
			sskeys[i] = temp2->key;
			ur_update_data_fixed(cvonsf);
/*
.....Apply REFSYS & MODSYS
.....Bobby  -  2/11/00
*/
		if (REFTM != UU_NULL)
			status = uc_transform (cvonsf, REFTM, UU_TRUE);
		if (MODTM != UU_NULL)
			status = uc_transform (cvonsf, MODTM, UU_TRUE);
/*
.....Display the cvonsf
*/
			if (NCL_ubcopy != 2) uc_display(cvonsf);
		}
/*
.....Update the sskey list
*/
		status = ur_update_data_varlist (e1->key, lst, sskeys, 1, ncv);
	}

	if (sskeys != UU_NULL) uu_toolfree(sskeys);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_nsf (e1)
**       Saves an NCL surface in unibase & deallocates memory used
**       to store this surface.
**    PARAMETERS
**       INPUT  :  e1 - pointer to NCL surface structure header.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_ubcopy_nsf (e1)
struct NCL_surface_rec *e1;
{
	int ipan, ispan, npats, status;
	struct NCL_panel_rec *temp;
	UU_KEY_ID *keys;
	int ncv;

	status = UU_SUCCESS;
/*
.....All panels are in PNL array of structures
.....Loop thru all panels 
*/
	ispan = e1->no_panelkey; 
	if (PNL_num == 0) PNL_pt = (struct NCL_panel_rec *) UU_LIST_ARRAY (&PNL_ls);
	keys  = (UU_KEY_ID *) uu_toolmalloc((ispan)* sizeof(UU_KEY_ID));
	for (ipan=1; ipan<=ispan && status == UU_SUCCESS; ipan++)
	{
		temp  = PNL_pt;
		npats = temp->no_patch;
/*
...Create new panel in unibase
*/
		temp->key = 0;
		temp->no_patch = 0;
		if (ur_create_data(temp) != 0) status = UU_FAILURE;
/*
...Store all patches in every panel
...and save panel key to update panelkey list
*/
		if (status == UU_SUCCESS)
		{
			keys[ipan-1] = temp->key;
			status = ur_update_data_varlist (temp->key,1,temp->patch,1, npats);
		}
		if (status == UU_SUCCESS)  PNL_pt++; 
		PNL_num++;
	}    
/*
...Update panelkey list in NCL surface 
*/
	if (status == UU_SUCCESS)
	{
		ncv = e1->no_sskey;
		e1->no_sskey = 0;
		e1->no_panelkey = 0;
		e1->panelkey = UU_NULL;
		status = um_create_geom (e1,UM_DEFAULT_TF,UM_CURRENT_ATTR);
		status = ur_update_data_varlist (e1->key, 1, keys, 1, ispan);
		e1->no_sskey = ncv;

		status = ncl_update_sskeys((struct NCL_fixed_databag *)e1,ncv,2);
/*
.....vp 2/18/98 update display list (3). 
*/
		if (e1->no_displst != 0) status += 
			ur_update_data_varlist (e1->key, 3, e1->displst, 1, e1->no_displst);
	}

	if (keys != UU_NULL) uu_toolfree(keys);

	if (ncv > 0)
	{
		ncl_ubent_memfree ();
	}

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubget_ccv (e1)
**       Gets composite curve from unibase and stores it in the
**       allocated general structure array.  Important note:
**       memory allocated here must deallocated in the associated
**       routine which uses this array (here:ncl_ubcopy_ccv).
**    PARAMETERS
**       INPUT  : e1 - pointer to the composite curve structure. 
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_ubget_ccv (eptr)
struct NCL_fixed_databag *eptr;
 {
  int i, ispan, status;
  struct NCL_fixed_databag temp;
  struct UC_attributedatabag tatr;
  UM_transf tmat;
  struct UM_cid_rec tcid;
  struct UM_compcrv_rec *e1;

  e1 = (struct UM_compcrv_rec *)eptr;

  ispan  = e1->no_cid; 
  status = UU_SUCCESS;

/*
...Get all components of the composite curve
*/
  for (i=0; i<ispan && status == UU_SUCCESS; i++)
	 {
	  tcid.reverse = e1->cid[i].reverse;
	  tcid.endparam = e1->cid[i].endparam;
	  temp.key = e1->cid[i].crvid;
	  status = ncl_retrieve_ent (&temp,&tatr,&tmat);
/*
...Make sure label is @UN
*/
	  if (status == UU_SUCCESS &&
			 strncmp(temp.label,"@UN    ",strlen(temp.label)))
		  {
			strcpy (temp.label,"@UN    ");
			status = ur_update_data_fixed(&temp);
		  }
/*
...Push component of curve on the list
*/
	  if (status == UU_SUCCESS) 
		  {
			uu_list_push (&DTB_ls, &temp);
			uu_list_push (&ATT_ls, &tatr);
			uu_list_push (&TRN_ls, &tmat);
			uu_list_push (&CID_ls, &tcid);
		  }
		status = ncl_ubget_compon (&temp);
	 }
  uu_dexit;
  return (status);
 }
/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_ccv (e1)
**       Saves a composite curve in unibase & deallocates memory used
**       to store this curve.
**
**    PARAMETERS
**       INPUT  :  e1 - initialized composite curve header record.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
 ncl_ubcopy_ccv (e1)
 struct UM_compcrv_rec *e1;
 {
  int i, ispan, status;
  struct NCL_fixed_databag *temp;
  struct UC_attributedatabag *tatr;
  struct UM_transf_rec tran;
  UM_transf *tmat;
  struct UM_cid_rec *tcid, *incid;
  UU_LOGICAL crcsv, cocsv;

  status = UU_SUCCESS;
  crcsv = NCL_create_compcrv;
  cocsv = NCL_copy_compcrv;
  NCL_create_compcrv = UU_TRUE;
  NCL_copy_compcrv = UU_TRUE;

  ispan = e1->no_cid; 
  incid = CID_pt;
/*
...Loop thru all component curves 
*/
  for (i=1; i<=ispan && status == UU_SUCCESS; i++)
	 {
	  temp = DTB_pt;
	  tatr = ATT_pt;
	  tmat = TRN_pt;
	  tcid = CID_pt;
/*
...Create component curve, make sure label is @UN
*/
	  temp->key = 0;
	  strcpy (temp->label,"@UN    ");
	  status = um_create_geom(temp,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	  if (status == UU_SUCCESS)
		 status = ncl_ubcopy_compon (temp);
/*
...Update attributes & transf, save curve key to update cid list
*/
	  if (status == UU_SUCCESS)
		 {
		  tatr->key = tran.key = temp->key;
		  ur_retrieve_transf(&tran);
		  um_tftotf(tmat, tran.tfmat);
		  status = ur_update_attr(tatr) + ur_update_transf(&tran);
		  tcid->crvid = temp->key;
		  DTB_pt++;  ATT_pt++; TRN_pt++; CID_pt++;
		 }
	 }    
  NCL_create_compcrv = crcsv;
  NCL_copy_compcrv = cocsv;
/*
...Create comp CV entry with new key list.  
*/
  if (status == UU_SUCCESS)
	{
	 e1->no_cid = 0;
	 e1->cid = UU_NULL;
	 status = um_create_geom (e1,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	 status = ur_update_data_varlist (e1->key, 1, incid, 1, ispan);
	}

  return(status);
 }

/*********************************************************************
**    E_FUNCTION     : ncl_ubget_solid(e1)
**       Gets composite solid from unibase and stores it in the
**       allocated general structure array.  Important note:
**       memory allocated here must deallocated in the associated
**       routine which uses this array (here:ncl_ubcopy_solid).
**    PARAMETERS
**       INPUT  :
**          e1      - pointer to the solid structure. 
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_ubget_solid(e1)
struct UM_solid_rec *e1;
{
	int i, ispan, status;
	UM_transf *tmat;
	struct NCL_fixed_databag *temp;
	struct UC_attributedatabag *tatr;
/*
.....Only Composite Solids require this routine
*/
	status = UU_SUCCESS;
	if (e1->type != UM_COMPOS_SOLID) goto done;
/*
...Make sure memory is not allocated and
...allocate it for all components & attributes
*/
	ncl_ubent_solid_memfree ();
	ispan  = e1->no_netkey; 
	ncl_ubent_solid_memaloc(ispan);

	temp   = DTBAG_solid;
	tatr   = ATTR_solid;
	tmat   = TRANSP_solid;
	DTBAG2 = DTBAG2_solid;
/*
...Get all components into DTBAG_solid global struct
...and attributes for every component
*/
	for (i=0; i<ispan && status == UU_SUCCESS; i++)
	{
		temp->key = e1->netkey[i];
		status = ncl_retrieve_ent (temp,tatr,tmat);
/*
...Make sure label is @UN
*/
		if (status == UU_SUCCESS && strncmp(temp->label,"@UN",3))
		{
			strcpy (temp->label,"@UN");
			status = ur_update_data_fixed(temp);
		}

		if (status == UU_SUCCESS )
			status = ncl_ubget_compon (temp);
		if (status == UU_SUCCESS) { temp++; tatr++; tmat++; DTBAG2++;}
	}
done:;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_solid (e1)
**       Saves a composite solid in unibase & deallocates memory used
**       to store this solid.
**
**    PARAMETERS
**       INPUT  :  e1 - initialized composite solid record.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubcopy_solid (e1)
struct UM_solid_rec *e1;
{
	int i, ispan, status;
	struct NCL_fixed_databag *temp;
	struct UC_attributedatabag *tatr;
	struct UM_attrdata_rec *umattr;
	UM_transf *tmat;
	UU_KEY_ID *keys;
	UU_LOGICAL crcsv, cocsv;
/*
.....Only Composite Solids require this routine
*/
	status = UU_SUCCESS;
	if (e1->type != UM_COMPOS_SOLID) goto done;
/*
...All components are in DTBAG_solid array of structures
...Get all components and pretend it is compcrv to force @UN labels 
*/
	crcsv = NCL_create_compcrv;
	cocsv = NCL_copy_compcrv;
	NCL_create_compcrv = UU_TRUE;
	NCL_copy_compcrv = UU_TRUE;
	ispan = e1->no_netkey; 
	temp  = DTBAG_solid;
	tatr  = ATTR_solid;
	tmat  = TRANSP_solid;
	DTBAG2 = DTBAG2_solid;
	keys  = (UU_KEY_ID *) uu_toolmalloc((ispan)* sizeof(UU_KEY_ID));
	for (i=1; i<=ispan && status == UU_SUCCESS; i++)
	{
/*
...Create new component in unibase
*/
		strcpy (temp->label,"@UN    ");
		umattr = (struct UM_attrdata_rec *) tatr;
		umattr->label_on = 0;
		status = ncl_ubcopy_create_geom (temp,tatr,tmat);
/*
...Store all components 
...and save component key to update key list
*/
		if (status == UU_SUCCESS)
		{
			keys[i-1] = temp->key;
			temp++;  tatr++; tmat++; DTBAG2++;
		}
	}    
	NCL_create_compcrv = crcsv;
	NCL_copy_compcrv = cocsv;
/*
...Create composite solid entry with new key list. 
*/
	if (status == UU_SUCCESS)
	{
		e1->no_netkey = 0;
		e1->netkey = UU_NULL;
		status = um_create_geom (e1,UM_DEFAULT_TF,UM_CURRENT_ATTR);
		status = ur_update_data_varlist (e1->key, 4, keys, 1, ispan);
	}
	if (keys != UU_NULL) uu_toolfree(keys);
	ncl_ubent_solid_memfree ();
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubget_symdata (e1,type)
**       Gets symbol or instance geometry, text and snap node data
**       from unibase and stores it in the allocated general databag
**       list.  Note that the memory allocated for the lists used
**       to store entity data is handled by the calling routine.
**    PARAMETERS
**       INPUT  : e1 - pointer to the symbol instance structure. 
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_ubget_symdata (eptr,type)
struct NCL_fixed_databag *eptr;
int type;
{
	int i, igeom, itext, isnap, status;
	struct NCL_fixed_databag temp;
	struct UC_attributedatabag tatr;
	UM_transf tmat;
	struct UB_instance_rec *e1;
	struct UB_symbol_rec *e2;

	if (type == 0)
	{
		e1 = (struct UB_instance_rec *)eptr;
		igeom = e1->no_geom;
		itext = igeom + e1->no_text_nod;
		isnap = itext + e1->no_snap_nod;
	}
	else 
	{
		e2 = (struct UB_symbol_rec *)eptr;
		igeom = e2->no_geom;
		itext = igeom + e2->no_text_nod;
		isnap = itext + e2->no_snap_nod;
	}
	status = UU_SUCCESS;
/*
...Get all components of the symbol/instance
*/
	for (i=0; i<isnap && status == UU_SUCCESS; i++)
	{
		if (i < igeom)
		{
			if (type == 0) temp.key = e1->geom[i];
			else temp.key = e2->geom[i];
		}
		else if (i < itext)
		{
			if (type == 0) temp.key = e1->text_nod[i-igeom].text_key;
			else temp.key = e2->text_nod[i-igeom].text_key;
		}
		else
		{
			if (type == 0) temp.key = e1->snap_nod[i-itext].snap_key;
			else temp.key = e2->snap_nod[i-itext].snap_key;
		}
		status = ncl_retrieve_ent (&temp,&tatr,&tmat);
/*
...Make sure label is @UN
*/
		if (status == UU_SUCCESS && strncmp(temp.label,"@UN",3) != 0)
		{
			strcpy (temp.label,"@UN   ");
			status = ur_update_data_fixed(&temp);
		}
/*
...Push component of instance on the list
*/
		if (status == UU_SUCCESS) 
		{
			uu_list_push (&SYM_ls, &temp);
			uu_list_push (&SATT_ls, &tatr);
			uu_list_push (&STRN_ls, &tmat);
		}
/*
.....Get components of geometry if it has any e.g. trim surface
*/
		if (i < igeom) status = ncl_ubget_compon (&temp);
	}
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_symdata (eptr)
**       Copies a symbol or symbol instance from one unibase to the
**       other.  Note that the memory allocated for the lists used
**       to store entity data is handled by the calling routine.
**
**    PARAMETERS
**       INPUT  :  eptr - symbol or instance pointer.
**                 type - symbol or instance flag
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubcopy_symdata (eptr,type)
struct NCL_fixed_databag *eptr;
int type;
{
	int i,ispan,status,igeom,itext,isnap,snap_no=0,
		text_no=0,typ,trel_num,tsubscr,i2stat,tmp;
	struct NCL_fixed_databag *temp;
	struct UC_attributedatabag *tatr,copyattr;
	struct UM_transf_rec tran;
	UM_transf *tmat;
	struct UB_instance_rec *e1;
	struct UB_symbol_rec *e2,sym;
	struct UB_snap_nod_rec snap_nod;
	struct UB_text_nod_rec textnod,*tmp_tnod;
	struct UA_txt_rec trec, *tmp_txt;
	struct UA_txtattr_rec tattr,*tmp_att;
	struct UM_point_rec pt,*tmp_pt;
	struct UM_attrdata_rec ptattr;
	struct UB_snap_nod_rec snapnod;
	UU_KEY_ID key,tkey;
	struct UB_inst_rec instdataptr;
	UU_LOGICAL notify,found,srfatt;
	char *pos,tlabel[NCL_MAX_LABEL];
/*
.....Label is filled with trailing blank spaces so an end of string
.....character is needed
.......Note: The first space found is considered the end
.......Note: instdataptr is filled with the correct tranformation
.......matrix and the key is updated when the instance tuple is
.......created
*/
	pos = (char *)strchr(eptr->label,' ');
	if (pos != UU_NULL) *pos = '\0';
/*
.....Copy attributes so that they can be copied into the unibase
.......The copy routines switch the active unibase so the switch
.......needs to be reversed when retrieving the attributes
*/
	if (UR_active == 1) ur_getu_second();
	else ur_getu_work();
	copyattr.key = eptr->key;
	ur_retrieve_attr(&copyattr);
	if (UR_active == 1) ur_getu_second();
	else ur_getu_work();
	
	if (type == 0)
	{
		e1 = (struct UB_instance_rec *)eptr;
		if (UR_active == 1) ur_getu_second();
		else ur_getu_work();
		ubi_retrieve_inst_data_in_msym(e1,&instdataptr,&key,&tmp);
		sym.key = key;
		ur_retrieve_data(&sym,sizeof(sym));
		if (UR_active == 2) ur_getu_work();
		else ur_getu_second();
		igeom = e1->no_geom;
		itext = igeom + e1->no_text_nod;
		isnap = itext + e1->no_snap_nod;
		if (itext > igeom)
			tmp_tnod = (struct UB_text_nod_rec *)e1->text_nod;
	}
	else 
	{
		e2 = (struct UB_symbol_rec *)eptr;
		igeom = e2->no_geom;
		itext = igeom + e2->no_text_nod;
		isnap = itext + e2->no_snap_nod;
		if (itext > igeom)
			tmp_tnod = (struct UB_text_nod_rec *)e2->text_nod;
	}
	status = UU_SUCCESS;
/*
...Loop thru all symbol geometry, text nodes & snap nodes
*/
	for (i=0; i<isnap && status == UU_SUCCESS; i++)
	{
		temp = SYM_pt; tatr = SATT_pt; tmat = STRN_pt;
		if (i < igeom) 
		{
			typ = temp->rel_num;
			if (typ != UM_COMPCRV_REL && typ != NCL_NETSF_REL &&
			typ != NCL_TRIMSF_REL && typ != NCL_SURF_REL && 
			typ != NCL_REVSURF_REL && typ != UB_INSTANCE_REL &&
			typ != UB_SYMBOL_REL)
			{
				temp->key = 0;
				strcpy (temp->label,"@UN    ");
				status = um_create_geom(temp,UM_DEFAULT_TF,UM_CURRENT_ATTR);
			}
/*
.....Need to set surface attribute flag so the correct type of attribute
.....record is created when ur_update_attr is called below
*/
			else if (ncl_surfattr_type(temp->rel_num))
			{
				ncl_set_surfattr_type();
				srfatt = UU_TRUE;
			}
			status = ncl_ubcopy_compon(temp);
			uu_list_push(&GEO_ls,&temp->key);
			tkey = temp->key;
		}
		else if (i < itext)
		{
			tmp_att = (struct UA_txtattr_rec *)tatr;
			tmp_txt = (struct UA_txt_rec *)temp;
			trec.key = temp->key;
			UB_txtattr.color = tmp_att->color;
			UB_txtattr.tangle = tmp_txt->tangle;
			uc_create_data(temp, UM_DEFAULT_TF, tatr);
			textnod.prompt = tmp_tnod[i-igeom].prompt;
			textnod.visibility = tmp_tnod[i-igeom].visibility;
			textnod.masterindx = tmp_tnod[i-igeom].masterindx;
			tkey = textnod.text_key = temp->key;
			uu_list_push(&TXT_ls,&textnod);
		}
		else
		{
			tmp_pt = (struct UM_point_rec *)temp;
			ur_setup_data(UM_POINT_REL,&pt,sizeof(struct UM_point_rec));
			pt.snap_node = UU_TRUE;
			pt.markertype = UB_snap_node_marker_type;
			um_current_default_attr(UM_POINT_REL, &ptattr);
			ptattr.color = UB_snap_node_color;
			pt.key = 0;
			strcpy (pt.label,"@UN    ");
			um_create_geom(&pt, UM_DEFAULT_TF, &ptattr);
			um_vctovc(tmp_pt->pt,pt.pt);
			ur_update_data(&pt);
			snapnod.snap_key = pt.key;
			snapnod.nbr = snap_no++;
			uu_list_push(&SNP_ls,&snapnod);
			tkey = pt.key;
		}
/*
...Update attributes & transf
*/
		if (status == UU_SUCCESS)
		{
			tatr->key = tran.key = tkey;
			ur_retrieve_transf(&tran);
			um_tftotf(tmat, tran.tfmat);
			status = ur_update_attr(tatr) + ur_update_transf(&tran);
			if (srfatt) ncl_reset_surfattr_type();
		}
		SYM_pt++; SATT_pt++; STRN_pt++;
	}
/*
...Create symbol or instance  
*/
	if (status == UU_SUCCESS)
	{
		if (type == 0)
		{
/*
.....Get master symbol so the association can be made between the instance
.....and master symbol
*/
			ub_get_symmaster_by_name(&sym, &found, 0,1);
			UB_SETUP_DATA(UB_INSTANCE_REL,e1,sizeof(struct UB_instance_rec), status);
			e1->no_geom = 0; e1->geom = UU_NULL;
			e1->no_text_nod = 0;	e1->text_nod = UU_NULL;
			e1->no_snap_nod = 0;	e1->snap_nod = UU_NULL;
			ub_create_instance_tuple(sym.key,e1, UB_DEFAULT_TF, 
				&copyattr, &instdataptr);
			tkey = e1->key;
			trel_num = e1->rel_num;
			tsubscr = e1->subscr;
			strcpy(tlabel,e1->label);
			typ = 32;
		}
		else
		{
			UB_SETUP_DATA(UB_SYMBOL_REL, e2, sizeof(struct UB_symbol_rec), status);
			e2->no_masters = 0; e2->masters = UU_NULL;
			e2->no_inst = 0; e2->inst = UU_NULL;
			e2->no_geom = 0; e2->geom = UU_NULL;
			e2->no_text_nod = 0;	e2->text_nod = UU_NULL;
			e2->no_snap_nod = 0;	e2->snap_nod = UU_NULL;
			ub_create_symbol_tuple(e2,UB_DEFAULT_TF,&copyattr);
			tkey = e2->key;
			trel_num = e2->rel_num;
			strcpy(tlabel,e2->label);
			tsubscr = e2->subscr;
			typ = 31;
		}
/*
.....Add type since symbol records have two lists prior to these ones
.....where instances do not
*/
		GEO_pt = (UU_KEY_ID *) UU_LIST_ARRAY (&GEO_ls);
		TXT_pt = (struct UB_text_nod_rec *) UU_LIST_ARRAY(&TXT_ls);
		SNP_pt = (struct UB_snap_nod_rec *) UU_LIST_ARRAY(&SNP_ls);
		status = ur_update_data_varlist (tkey, 1+type, GEO_pt, 1, igeom);
		status = ur_update_data_varlist (tkey, 2+type, TXT_pt, 1, itext-igeom);
		status = ur_update_data_varlist (tkey, 3+type, SNP_pt, 1, isnap-itext);
/*
.....Removed labeling routine calls so symbols and instances are not stored in
.....variable tables - Andrew 2/27/13
		ncl_label_wf(trel_num,tlabel,&tsubscr,tkey,&i2stat);
		if (UR_active == 1) ncl_store_name(&tlabel,tsubscr,0,0,0,typ,UU_FALSE,&tkey);
*/
	}

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubget_trimsf (e1)
**       Gets a trim surface from unibase and stores it in the
**       global structure.
**    PARAMETERS
**       INPUT  :  e1 - pointer to NCL surface structure header.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_ubget_trimsf (e1)
struct NCL_trimsf_rec *e1;
{
	int i, ncv, status;
	UM_transf tmat;
	struct NCL_fixed_databag temp;
	struct UC_attributedatabag tatr;
/*
...Get underlying surface
*/
	ncv = e1->no_ibndykey; 
	temp.key = e1->bs_key;
	status = ncl_retrieve_ent (&temp,&tatr,tmat);

	if (status  == UU_SUCCESS)
	{
		uu_list_push (&DTB_ls, &temp);
		uu_list_push (&ATT_ls, &tatr);
		uu_list_push (&TRN_ls, &tmat);
		status = ncl_ubget_compon (&temp);
	}

/*
...Get uv curve
*/
	if (status == UU_SUCCESS)
	{
		temp.key = tatr.key = e1->uv_key;
		if (temp.key != 0)
		{
			status = ncl_retrieve_ent (&temp,&tatr,&tmat);
			if (status  == UU_SUCCESS)
			{
				uu_list_push (&DTB_ls, &temp);
				uu_list_push (&ATT_ls, &tatr);
				uu_list_push (&TRN_ls, &tmat);
				if (temp.rel_num == UM_COMPCRV_REL)
					status = ncl_ubget_ccv (&temp); 
			}
		} 
	}
/*
...Get cv curve
*/
	if (status == UU_SUCCESS)
	{
		temp.key = tatr.key = e1->cv_key;
		if (temp.key != 0)
		{
			status = ncl_retrieve_ent (&temp,&tatr,&tmat);
			if (status  == UU_SUCCESS)
			{
				uu_list_push (&DTB_ls, &temp);
				uu_list_push (&ATT_ls, &tatr);
				uu_list_push (&TRN_ls, &tmat);
				if (temp.rel_num == UM_COMPCRV_REL)
					status = ncl_ubget_ccv (&temp); 
			}
		} 
	}
/*
...Get boundary curves
*/
	for (i=0; i<ncv && status == UU_SUCCESS; i++)
	{
		if (status == UU_SUCCESS)
		{
			temp.key = tatr.key = e1->ibndykey[i];
			if (temp.key != 0)
			{
				status = ncl_retrieve_ent (&temp,&tatr,&tmat);
				if (status  == UU_SUCCESS)
				{
					uu_list_push (&DTB_ls, &temp);
					uu_list_push (&ATT_ls, &tatr);
					uu_list_push (&TRN_ls, &tmat);
					if (temp.rel_num == UM_COMPCRV_REL)
						status = ncl_ubget_ccv (&temp); 
				}
			} 
		}
	}

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_trimsf (e1)
**       Saves trim surface in unibase & deallocates memory used
**       to store this surface.
**    PARAMETERS
**       INPUT  :  e1 - pointer to NCL surface structure header.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_ubcopy_trimsf (e1)
struct NCL_trimsf_rec *e1;
{
	int i, ncv, status;
	struct NCL_fixed_databag *temp;
	struct UC_attributedatabag *tatr;
	UM_transf *tmat;
	UU_KEY_ID *keys,uvsav;
	int num_sskeys,lst;
	UU_LOGICAL crcsv, cocsv;
/*
.....Removed calls to ncl_displst_fixkeys after unibase update routine
.....was added to reup10000.c that updates to how the display list is
.....used in 10.0.  Now indices are stored instead of curve keys so that
.....if the keys change the display list will still be valid.
.......Andrew 11/8/12
*/
/*
...underlying surfaces is in DTBAG array 
...pretend it is compcrv to force @UN labels 
*/
	crcsv = NCL_create_compcrv;
	cocsv = NCL_copy_compcrv;
	NCL_create_compcrv = UU_TRUE;
	NCL_copy_compcrv = UU_TRUE;

	status = ncl_retrieve_sskeys (DTB_pt,&lst,&num_sskeys,&keys);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	status = ncl_ubcopy_create_geom (DTB_pt,ATT_pt,TRN_pt);

	e1->bs_key = DTB_pt->key; 

	temp = DTB_pt;
	DTB_pt++; ATT_pt++; TRN_pt++;
/*
.....Process the sskeys to the base surface.
*/
	status = ncl_update_sskeys(temp,num_sskeys,5);

/*
...Copy uv curve of trimed sf
*/
	if (status == UU_SUCCESS)
	{
		if (e1->uv_key != 0)
		{
			uvsav = e1->uv_key;
			temp = DTB_pt;
			tatr = ATT_pt;
			tmat = TRN_pt;
			temp->key = 0;
			DTB_pt++; ATT_pt++; TRN_pt++;
			if (temp->rel_num != UM_COMPCRV_REL)
				status = um_create_geom (temp,UM_DEFAULT_TF,UM_CURRENT_ATTR);
			if (status + ncl_ubcopy_compon (temp) == 0)  
				status = ncl_updateub_ent (temp,tatr,tmat); 
			e1->uv_key = temp->key;
/*			if (uvsav != e1->uv_key && e1->no_displst != 0) 
				ncl_displst_fixkeys (e1->displst,e1->uv_key,uvsav,0);
*/
		}
	}
/*
...Copy cv curve of trimed sf
*/
	if (status == UU_SUCCESS)
	{
		if (e1->cv_key != 0)
		{
			temp = DTB_pt;
			tatr = ATT_pt;
			tmat = TRN_pt;
			temp->key = 0;
			DTB_pt++; ATT_pt++; TRN_pt++;
			if (temp->rel_num != UM_COMPCRV_REL)
				status = um_create_geom (temp,UM_DEFAULT_TF,UM_CURRENT_ATTR);
			if (status + ncl_ubcopy_compon (temp) == 0)  
				status = ncl_updateub_ent (temp,tatr,tmat); 
			e1->cv_key = temp->key; 
		}
	}
/*
...Copy boundary curves of trimmed sf
*/
	keys  = UU_NULL;
	ncv = e1->no_ibndykey; 
	if (ncv > 0)
	{
		keys  = (UU_KEY_ID *) uu_toolmalloc((ncv)* sizeof(UU_KEY_ID));
		if (keys == UU_NULL) status = UU_FAILURE;
	}

	for (i=0; i<ncv && status == UU_SUCCESS; i++)
	{
		if (e1->ibndykey[i] != 0)
		{
			uvsav = e1->ibndykey[i];
			temp = DTB_pt;
			tatr = ATT_pt;
			tmat = TRN_pt;
			temp->key = 0;
			DTB_pt++; ATT_pt++; TRN_pt++;
			if (temp->rel_num != UM_COMPCRV_REL)
				status = um_create_geom (temp,UM_DEFAULT_TF,UM_CURRENT_ATTR);
			if (status + ncl_ubcopy_compon (temp) == 0)  
				status = ncl_updateub_ent (temp,tatr,tmat); 
			keys[i] = temp->key;
/*			if (i != 2*(i/2) && uvsav != keys[i] && e1->no_displst != 0) 
				ncl_displst_fixkeys (e1->displst,keys[i],uvsav,i+1);
*/
		}
		else keys[i] = 0;
	}
/*
...Create trimed SF entry with new key list.
*/
	NCL_create_compcrv = crcsv;
	NCL_copy_compcrv = cocsv;

	if (status == UU_SUCCESS)
	{
		e1->no_ibndykey = 0;
		e1->ibndykey = UU_NULL;
		if (um_create_geom (e1,UM_DEFAULT_TF,UM_CURRENT_ATTR) == 0)
		if (ncv > 0) 
		{
			status = ur_update_data_varlist (e1->key, 1, keys, 1, ncv);
/*
.....vp 2/18/98 update display list if exists
*/
			if (e1->no_displst != 0) status += 
				ur_update_data_varlist (e1->key,2,e1->displst,1,e1->no_displst);
		}
	}
/*
...Deallocate memory
*/
	if (keys != UU_NULL) uu_toolfree(keys);
	
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubget_netsf (e1)
**       Gets an NCL surface from unibase and stores it in the
**       global structure.
**    PARAMETERS
**       INPUT  :  e1 - pointer to NCL surface structure header.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_ubget_netsf (e1)
 struct NCL_netsf_rec *e1;
 {
  int i, ispan, status;
  UM_transf *tmat;
  struct NCL_fixed_databag *temp;
  struct UC_attributedatabag *tatr;
/*
...Make sure memory is not allocated and
...allocate it for all surfaces & attributes
*/
  ncl_ubent_netsf_memfree ();
  ispan  = e1->no_netkey; 
  ncl_ubent_netsf_memaloc (ispan);

  temp   = DTBAG_netsf;
  tatr   = ATTR_netsf;
  tmat   = TRANSP_netsf;
  status = UU_SUCCESS;
	DTBAG2 = DTBAG2_netsf;
/*
...Get all surfaces into DTBAG_netsf global struct
...and attributes for every surface
*/
  for (i=0; i<ispan && status == UU_SUCCESS; i++)
	{
	  temp->key = e1->netkey[i];
	  status = ncl_retrieve_ent (temp,tatr,tmat);
/*
...Make sure label is @UN
*/
	  if (status == UU_SUCCESS &&
			 strncmp(temp->label,"@UN    ",4))
		{
			strcpy (temp->label,"@UN    ");
			status = ur_update_data_fixed(temp);
		}

	  if (status == UU_SUCCESS )
			status = ncl_ubget_compon (temp);
	  if (status == UU_SUCCESS) { temp++; tatr++; tmat++; DTBAG2++;}
	}

  return (status);
 }

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_netsf (e1)
**       Saves an NCL surface in unibase & deallocates memory used
**       to store this surface.
**    PARAMETERS
**       INPUT  :  e1 - pointer to NCL surface structure header.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_ubcopy_netsf (e1)
struct NCL_netsf_rec *e1;
{
	int i, ispan, status;
	struct NCL_fixed_databag *temp;
	struct UC_attributedatabag *tatr;
	struct UM_attrdata_rec *umattr;
	UM_transf *tmat;
	UU_KEY_ID *keys;
	UU_LOGICAL crcsv, cocsv;

	status = UU_SUCCESS;
/*
...All surfaces are in DTBAG_netsf array of structures
...Get all sf and pretend it is compcrv to force @UN labels 
*/
	crcsv = NCL_create_compcrv;
	cocsv = NCL_copy_compcrv;
	NCL_create_compcrv = UU_TRUE;
	NCL_copy_compcrv = UU_TRUE;
	ispan = e1->no_netkey; 
	temp  = DTBAG_netsf;
	tatr  = ATTR_netsf;
	tmat  = TRANSP_netsf;
	DTBAG2 = DTBAG2_netsf;
	keys  = (UU_KEY_ID *) uu_toolmalloc((ispan)* sizeof(UU_KEY_ID));
	for (i=1; i<=ispan && status == UU_SUCCESS; i++)
	{
/*
...Create new subsurface in unibase
*/
		strcpy (temp->label,"@UN    ");
		umattr = (struct UM_attrdata_rec *) tatr;
		umattr->label_on = 0;
		status = ncl_ubcopy_create_geom (temp,tatr,tmat);
/*
...Store all subsurface 
...and save subsurface key to update key list
.....vp 1.23.97 make sure label is not displayed at subsurface
*/
		if (status == UU_SUCCESS)
		{
			keys[i-1] = temp->key;
			temp++;  tatr++; tmat++; DTBAG2++;
		}
	}    
	NCL_create_compcrv = crcsv;
	NCL_copy_compcrv = cocsv;
/*
...Create net SF entry with new key list. 
*/
	if (status == UU_SUCCESS)
	{
/*
...vp 3/25/98 zero out sskey list for now. Remove this when sspline
...is supported in put/get
*/
		i = e1->no_sskey;
		e1->no_sskey = 0;

		e1->no_netkey = 0;
		e1->netkey = UU_NULL;
		status = um_create_geom (e1,UM_DEFAULT_TF,UM_CURRENT_ATTR);
		status = ur_update_data_varlist (e1->key, 1, keys, 1, ispan);
		e1->no_sskey = i;
/*
.....vp 2/18/98 update display list if exists
*/
		status +=
		ur_update_data_varlist (e1->key, 3, e1->displst, 1, e1->no_displst);
	}
	if (keys != UU_NULL) uu_toolfree(keys);
	ncl_ubent_netsf_memfree ();
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_update_list_rbsf (sf)
**       Updates data in rbspline surface referenced by pointer.
**    PARAMETERS
**       INPUT  :  sf - pointer to NCL surface structure.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_update_list_rbsf (sf)
struct UM_rbsplsrf_rec *sf;
{
	int status;
	int ncv;

	status = ur_update_data_varlist (sf->key, 1, sf->tu, 1, sf->no_tu) +
			ur_update_data_varlist (sf->key, 2, sf->tv, 1, sf->no_tv) +
			ur_update_data_varlist (sf->key, 3, sf->pt, 1, sf->no_pt) +
			ur_update_data_varlist (sf->key, 4, sf->wt, 1, sf->no_wt);

	ncv = sf->no_sskey; 
	status += ncl_update_sskeys((struct NCL_fixed_databag *)sf,ncv,5);

/*
.....vp 2/18/98 update display list if exists
*/
	if (sf->no_displst != 0) status += 
		ur_update_data_varlist (sf->key, 6, sf->displst, 1, sf->no_displst);

	if (ncv > 0)
	{
		ncl_ubent_memfree ();
	}

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_update_list_mshsf (sf)
**       Updates data in mesch surface referenced by pointer.
**    PARAMETERS
**       INPUT  :  sf - pointer to mesh surface structure.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_update_list_mshsf (sf)
struct NCL_meshsf_rec *sf;
{
	int status;
	status = ur_update_data_varlist 
		(sf->key, 1, sf->mpatch, 1, sf->no_mpatch);
/*
.....vp 2/18/98 update display list if exists
*/
	if (sf->no_displst != 0) status += 
		ur_update_data_varlist (sf->key, 2, sf->displst, 1, sf->no_displst);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_update_list_evalsf (sf)
**       Updates data in evaluated surface referenced by pointer.
**    PARAMETERS
**       INPUT  :  sf - pointer to evaluated surface structure.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_update_list_evalsf (sf)
struct NCL_evalsf_rec *sf;
{
	int status;
	status = ur_update_data_varlist 
			(sf->key, 1, sf->evwd, 1, sf->no_evwd);
/*
.....vp 2/18/98 update display list if exists
*/
	if (sf->no_displst != 0) status += 
		ur_update_data_varlist (sf->key, 2, sf->displst, 1, sf->no_displst);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_update_list_evalcv (cv)
**       Updates data in evaluated curve referenced by pointer.
**    PARAMETERS
**       INPUT  :  cv - pointer to evaluated curve structure.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_update_list_evalcv (cv)
 struct NCL_evalcv_rec *cv;
 {
  int status;
  status = ur_update_data_varlist 
			 (cv->key, 1, cv->evwd, 1, cv->no_evwd);
  uu_dexit;
  return(status);
 }

/*********************************************************************
**    E_FUNCTION     : ncl_update_list_nclcv (cv)
**       Updates data in NCL curve referenced by pointer.
**    PARAMETERS
**       INPUT  :  cv - pointer to NCL curve structure.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_update_list_nclcv (cv)
 struct NCL_curve_rec *cv;
 {
  int status;
  status = ur_update_data_varlist (cv->key, 1, cv->param, 1, cv->no_param) +
		  ur_update_data_varlist (cv->key, 2, cv->segment, 1, cv->no_segment);
  uu_dexit;
  return(status);
 }
 
/*********************************************************************
**    E_FUNCTION     : ncl_update_list_rbcv (cv)
**       Updates data in rbspline curve referenced by pointer.
**    PARAMETERS
**       INPUT  :  cv - pointer to rbspline curve structure.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_update_list_rbcv (cv)
 struct UM_rbsplcrv_rec *cv;
 {
  int status;
  status = ur_update_data_varlist (cv->key, 1, cv->t, 1, cv->no_t) +
			  ur_update_data_varlist (cv->key, 2, cv->pt, 1, cv->no_pt) +
			  ur_update_data_varlist (cv->key, 3, cv->wt, 1, cv->no_wt);
  uu_dexit;
  return(status);
 }

/*********************************************************************
**    E_FUNCTION     : ncl_update_list_patrn (pn)
**       Updates data in patern structure referenced by pointer.
**    PARAMETERS
**       INPUT  :  pn - pointer to patern structure.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_update_list_patrn (pn)
 struct NCL_patern_rec *pn;
 {
  int status;
  status = ur_update_data_varlist (pn->key, 1, pn->patpnt, 1, pn->no_patpnt); 
  uu_dexit;
  return(status);
 }

/*********************************************************************
**    E_FUNCTION     : ncl_update_list_shape (sh)
**       Updates data in shape structure referenced by pointer.
**    PARAMETERS
**       INPUT  :  sh - pointer to shape structure.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
ncl_update_list_shape (sh)
 struct NCL_shape_rec *sh;
 {
  int status;
  status = ur_update_data_varlist (sh->key, 1, sh->shapwd, 1, sh->no_shapwd); 
  uu_dexit;
  return(status);
 }

/*********************************************************************
**    E_FUNCTION     : ncl_ubget_revsf (e1)
**       Gets an NCL surface of revolution from unibase and stores it 
**       in the global structure and zeroes the cvkey.
**    PARAMETERS
**       INPUT  :  e1 - pointer to NCL surface of revolution structure
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ncl_ubget_revsf (e1)
struct NCL_revsurf_rec *e1;
 {
  int status;

	DTBAG2->key = e1->cvkey;
	status = ncl_retrieve_ent (DTBAG2,&ATTR2,TRANSP2);
	if (status == UU_SUCCESS && DTBAG2->rel_num == UM_COMPCRV_REL)
		status = ncl_ubget_ccv (DTBAG2); 
	e1->cvkey = 0;

	return (status);
 }

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_revsf (rsf)
**       Saves an NCL surface of revolution in unibase & deallocates 
**       memory used to store this surface.
**    PARAMETERS
**       INPUT  :  rsf - pointer to surface of revolution structure.
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_ubcopy_revsf (rsf)
struct NCL_revsurf_rec *rsf;
{
	int status;
	UU_KEY_ID *keys;
	int num_sskeys,lst;
	UU_LOGICAL crcsv, cocsv;
/*
...Underlying surfaces is in DTBAG array 
...pretend it is compcrv to force @UN labels 
*/
	crcsv = NCL_create_compcrv;
	cocsv = NCL_copy_compcrv;
	NCL_create_compcrv = UU_TRUE;
	NCL_copy_compcrv = UU_TRUE;

	status = ncl_retrieve_sskeys (rsf,&lst,&num_sskeys,&keys);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	if (DTBAG2->rel_num != UM_COMPCRV_REL)
		status = um_create_geom (DTBAG2,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	if (status + ncl_ubcopy_compon (DTBAG2) == 0)  
		status = ncl_updateub_ent (DTBAG2,&ATTR2,TRANSP2); 
/*
.....Process the sskeys to the base surface.
*/
	status = ncl_update_sskeys((struct NCL_fixed_databag *)rsf,num_sskeys,1);

	if (status == UU_SUCCESS)
	{
		rsf->cvkey = DTBAG2->key; 
	}
	NCL_create_compcrv = crcsv;
	NCL_copy_compcrv = cocsv;
/*
...Create SF entry
*/
	if (status == UU_SUCCESS)
	{
		if (um_create_geom (rsf,UM_DEFAULT_TF,UM_CURRENT_ATTR) == 0)
		if (rsf->no_displst != 0) status += 
			ur_update_data_varlist (rsf->key,2,rsf->displst,1,rsf->no_displst);
	}
/*
...Deallocate memory
*/
	if (keys != UU_NULL) uu_toolfree(keys);

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubget_draft (e1)
**       Gets a UA_generic_draft structure from the unibase and stores 
**       it in the list
**    PARAMETERS
**       INPUT  :  e1 - pointer to UA_txt_rec structure
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_ubget_draft (e1)
struct UA_generic_draft *e1;
{
	int status,i,j,index;
	struct UA_generic_draft e;
	struct UA_draft_rec t_draft;
	struct UC_attributedatabag att,assoc_att;
	UM_transf tran;
	struct NCL_fixed_databag assoc;
	UU_KEY_ID key;
	struct UA_asso_blk asso_blk;
	UU_LOGICAL found;

	uu_list_init (&ASO_ls,sizeof(struct UA_asso_blk),0,20);
	status = UU_SUCCESS;
	status = ua_retrieve_data(e1,sizeof(struct UA_generic_draft));
/*
.....The associate geometry is the only data not stored directly in
.....the draft structure and so must be stored and copied later
*/
	for(i=0;i<e1->asso_blk_use;i++)
   {
		asso_blk.asso_type = e1->asso_blk[i].asso_type;
		asso_blk.modifier = e1->asso_blk[i].modifier;
		asso_blk.key = e1->asso_blk[i].key;
		for(j=0;j<3;j++)
      {
			asso_blk.location[j] = e1->asso_blk[i].location[j];
      }
/*
.....Associate key equal to zero means the draft entity should be
.....skipped since its geometry is no longer valid in the unibase
*/
		if (asso_blk.key <= 0)
		{
			if (e1->etype != UA_BALLOON_DIM)// && e1->etype != UA_CROSSHATCH)
				return(UU_FAILURE);
			else continue;
		}
		assoc.key = asso_blk.key;
		status = ncl_retrieve_data_fixed(&assoc);
		ncl_ubkey_search(UR_active-1,assoc.key,&key,&found,&index);
		if (!found) ncl_ubget_compon(&assoc);
		uu_list_push(&ASO_ls,&asso_blk);
   }
	
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_draft (e1,iren,offset)
**       Gets a UA_generic_draft structure from the list and stores it 
**       in the unibase
**    PARAMETERS
**       INPUT  :
**          e1     - pointer to UA_txt_rec structure
**          iren   - rename flag
**          offset - layer offset value
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_ubcopy_draft (e1,iren,offset)
struct UA_generic_draft *e1;
int iren,offset;
{
	int status,i,j,ind,index,sv_copy;
	struct NCL_fixed_databag e2;
	struct UA_asso_blk *assoc;
	UU_KEY_ID key,*keys;
	UU_LOGICAL found,*is_new,copied;
	UU_LIST new_list,in_list;
	char sbuf[80];

	ind = 0;
	assoc = (struct UA_asso_blk *) UU_LIST_ARRAY (&ASO_ls);
	uu_list_init(&new_list,sizeof(UU_LOGICAL),e1->asso_blk_use,5);
	uu_list_init(&in_list,sizeof(UU_KEY_ID),e1->asso_blk_use,5);
	if (UR_active == 1) ur_getu_second();
	else ur_getu_work();
	for (i=0;i<e1->asso_blk_use;i++)
	{
		e2.key = assoc->key;
		if (e2.key <= 0)
		{
			if (e1->etype != UA_BALLOON_DIM)// && e1->etype != UA_CROSSHATCH)
				goto failed;
			else
				continue;
		}
		ncl_retrieve_data_fixed(&e2);
/*
.....Check if the associate geometry already exists and do not
.....recreate if it is already in the unibase
*/
		uu_list_push(&in_list,&(e2.key));
		ncl_ubkey_search(2-UR_active,e2.key,&key,&found,&index);
		uu_list_push(&new_list,&found);
		if (!found)
		{
			copied = UU_TRUE;
			sv_copy = ncl_get_ubcopy();
			ncl_set_ubcopy(0);
			if(UR_active == 2)
				status = ncl_ubcopy_ent(&e2,iren,offset,&copied);
			else
				status = ncl_ubput_ent1(&e2);
			if (status != UU_SUCCESS) goto done;
			if (!copied) goto failed;
			ncl_set_ubcopy(sv_copy);
			ncl_ubput_keys(2-UR_active,&key,&e2.key);
		}
		else
		{
			e2.key = key;
			if (UR_active == 1) ur_getu_second();
			else ur_getu_work();
			ncl_retrieve_data_fixed(&e2);
			if (UR_active == 1) ur_getu_second();
			else ur_getu_work();
		}
		assoc++;
		e1->asso_blk[ind++].key = e2.key;
	}
	if (UR_active == 1) ur_getu_second();
	else ur_getu_work();
	status = ua_create_entity(e1,&key);
	goto done;
failed:
	is_new = (UU_LOGICAL *)UU_LIST_ARRAY(&new_list);
	keys = (UU_KEY_ID *)UU_LIST_ARRAY(&in_list);
	for (j=0;j<i;j++)
	{
		if (!is_new[j] && keys[j] != 0)
			ncl_ubkey_delete(keys[j]);
	}
	status = UU_FAILURE;
done:
	uu_list_free(&ASO_ls);
	uu_list_free(&new_list);
	uu_list_free(&in_list);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubget_anote (e1)
**       Gets a UA_txt_rec structure's arc from unibase, stores it 
**       in the global structure, zeroes the arckey.
**    PARAMETERS
**       INPUT  :  e1 - pointer to UA_txt_rec structure
**       OUTPUT : none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubget_anote (e1)
struct UA_txt_rec *e1;
{
	int status;

	DTBAG2->key = e1->arckey;

	status = 0;
	if (e1->arckey != 0)
	status = ncl_retrieve_ent (DTBAG2,&ATTR2,TRANSP2);
	e1->arckey = 0;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_anote (e1)
**       Saves a UA_txt_rec structure in unibase & deallocates 
**       memory used to store it.
**    PARAMETERS
**       INPUT  :  e1 - pointer to UA_txt_rec structure
**       OUTPUT :
**          none
**    RETURNS      : status = UU_SUCCESS if OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_ubcopy_anote (e1)
struct UA_txt_rec *e1;
{
	int status;
	struct   UA_txt_rec			note;			/* text record */
	struct	UA_txtattr_rec		txtattr;		/* text attribute */
	UU_LOGICAL crcsv, cocsv;

	status = UU_SUCCESS;

	if (DTBAG2->key != 0)
	{
/*
...Underlying surfaces is in DTBAG array 
...pretend it is compcrv to force @UN labels 
*/
	crcsv = NCL_create_compcrv;
	cocsv = NCL_copy_compcrv;
	NCL_create_compcrv = UU_TRUE;
	NCL_copy_compcrv = UU_TRUE;
		status = um_create_geom (DTBAG2,UM_DEFAULT_TF,UM_CURRENT_ATTR);
	if (status == UU_SUCCESS)
		status = ncl_updateub_ent (DTBAG2,&ATTR2,TRANSP2); 

	if (status == UU_SUCCESS)
	{
		e1->arckey = DTBAG2->key; 
	}
	NCL_create_compcrv = crcsv;
	NCL_copy_compcrv = cocsv;
	}
/*
...Create SF entry
*/
	if (status == UU_SUCCESS)
	{
		ua_init_txtrec(&note,&txtattr,&UA_txtattr,UU_FALSE);

		status = uc_create_data(e1,UM_DEFAULT_TF,&txtattr);
	}
	if (status == UU_SUCCESS)
	{
		status = ur_update_data_varlist 
			(e1->key, 1, e1->tchar, 1, e1->no_tchar);
	}
	if (status == UU_SUCCESS)
	{
		if (e1->no_displst != 0) status += 
			ur_update_data_varlist (e1->key,2,e1->displst,1,e1->no_displst);
	}
/*
.....Store anote in ranfile
*/
	ncl_store_wf2(e1->key,e1->rel_num,e1->label,e1->subscr);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ublist_init ()
**       Initialize ubfn lists.
**    PARAMETERS
**       INPUT  :  
**          none
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ublist_init ()
{
	uu_list_init (&DTB_ls, sizeof(struct NCL_fixed_databag),0,20);
	uu_list_init (&ATT_ls, sizeof(struct UC_attributedatabag),0,20);
	uu_list_init (&TRN_ls, sizeof(UM_transf),0,20);
	uu_list_init (&CID_ls, sizeof(struct UM_cid_rec),0,20);
	uu_list_init (&PNL_ls, sizeof(struct NCL_panel_rec), 0, 20);
	uu_list_init (&GEO_ls,sizeof(UU_KEY_ID),0,20);
	uu_list_init (&SYM_ls,sizeof(struct NCL_fixed_databag),0,20);
	uu_list_init (&TXT_ls,sizeof(struct UB_text_nod_rec),0,20);
	uu_list_init (&SNP_ls,sizeof(struct UB_snap_nod_rec),0,20);
	uu_list_init (&SATT_ls,sizeof(struct UC_attributedatabag),0,20);
	uu_list_init (&STRN_ls,sizeof(UM_transf),0,20);

	return(0);
}
/*********************************************************************
**    E_FUNCTION     : ncl_ublist_free ()
**       Free ubfn lists.
**    PARAMETERS
**       INPUT  :  
**          none
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ublist_free ()
{
	uu_list_free(&DTB_ls);
	uu_list_free(&ATT_ls);
	uu_list_free(&TRN_ls);
	uu_list_free(&CID_ls);
	uu_list_free(&PNL_ls);
	uu_list_free(&SYM_ls);
	uu_list_free(&GEO_ls);
	uu_list_free(&TXT_ls);
	uu_list_free(&SNP_ls);
	uu_list_free(&SATT_ls);
	uu_list_free(&STRN_ls);
	PNL_cnt = PNL_num = 0;

	return(0);
}
/*********************************************************************
**    E_FUNCTION     : ncl_ublist_empty ()
**       Empty ubfn lists (set the current count to zero but do not
**       free memory).
**    PARAMETERS
**       INPUT  :  
**          none
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ublist_empty ()
{
	UU_LIST_EMPTY(&DTB_ls);
	UU_LIST_EMPTY(&ATT_ls);
	UU_LIST_EMPTY(&TRN_ls);
	UU_LIST_EMPTY(&CID_ls);
	UU_LIST_EMPTY(&PNL_ls);
	UU_LIST_EMPTY(&SYM_ls);
	UU_LIST_EMPTY(&GEO_ls);
	UU_LIST_EMPTY(&TXT_ls);
	UU_LIST_EMPTY(&SNP_ls);
	UU_LIST_EMPTY(&SATT_ls);
	UU_LIST_EMPTY(&STRN_ls);
	PNL_cnt = PNL_num = 0;

	return(0);
}
/*********************************************************************
**    E_FUNCTION     : ncl_ubent_memfree ()
**       Deallocates memory allocated for composite entity.
**    PARAMETERS
**       INPUT  :  
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  ncl_ubent_memfree ()
	{
	 if (DTBAG != UU_NULL) { uu_toolfree(DTBAG); DTBAG = UU_NULL; }
	 if (ATTR != UU_NULL)  { uu_toolfree(ATTR); ATTR = UU_NULL; }
	 if (TRANSP != UU_NULL) { uu_toolfree(TRANSP); TRANSP = UU_NULL; }
	 uu_dexit;
	 return(0);
	}
/*********************************************************************
**    E_FUNCTION     : ncl_ubent_memaloc (n)
**       Allocates memory for composite entity.
**    PARAMETERS
**       INPUT  :  n - number of simple entities used in composite ent.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS  
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  ncl_ubent_memaloc (n)
  int n;
	{
	 DTBAG  = (struct NCL_fixed_databag *) uu_toolmalloc((n)*
				  sizeof(struct NCL_fixed_databag));
	 ATTR   = (struct UC_attributedatabag *) uu_toolmalloc((n)*
				  sizeof(struct UC_attributedatabag));
	 TRANSP = (UM_transf *) uu_toolmalloc((n)*
				  sizeof(UM_transf));
	 uu_dexit;
	 return(0);
	}

/*********************************************************************
**    E_FUNCTION     : ncl_ubent_netsf_memfree ()
**       Deallocates memory allocated for net sf components.
**    PARAMETERS
**       INPUT  :  
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubent_netsf_memfree ()
{
	if (DTBAG_netsf) { uu_toolfree(DTBAG_netsf); DTBAG_netsf = UU_NULL; }
	if (DTBAG2_netsf) { uu_toolfree(DTBAG2_netsf); DTBAG2_netsf = UU_NULL; }
	if (ATTR_netsf)  { uu_toolfree(ATTR_netsf); ATTR_netsf = UU_NULL; }
	if (TRANSP_netsf) { uu_toolfree(TRANSP_netsf); TRANSP_netsf = UU_NULL; }
	DTBAG2 = &DTBAG2_fixed;
	return(0);
}
/*********************************************************************
**    E_FUNCTION     : ncl_ubent_netsf_memaloc (n)
**       Allocates memory for net sf components.
**    PARAMETERS
**       INPUT  :  n - number of surface in net surface.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS  
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubent_netsf_memaloc (n)
int n;
{
	DTBAG_netsf  = (struct NCL_fixed_databag *) uu_toolmalloc((n)*
				  sizeof(struct NCL_fixed_databag));
	DTBAG2_netsf  = (struct NCL_fixed_databag *) uu_toolmalloc((n)*
				  sizeof(struct NCL_fixed_databag));
	ATTR_netsf   = (struct UC_attributedatabag *) uu_toolmalloc((n)*
				  sizeof(struct UC_attributedatabag));
	TRANSP_netsf = (UM_transf *) uu_toolmalloc((n)*
				  sizeof(UM_transf));
	DTBAG2 = DTBAG2_netsf;
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubent_solid_memfree ()
**       Deallocates memory allocated for composite solid components.
**    PARAMETERS
**       INPUT  :  
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubent_solid_memfree ()
{
	if (DTBAG_solid) { uu_toolfree(DTBAG_solid); DTBAG_solid = UU_NULL; }
	if (DTBAG2_solid) { uu_toolfree(DTBAG2_solid); DTBAG2_solid = UU_NULL; }
	if (ATTR_solid)  { uu_toolfree(ATTR_solid); ATTR_solid = UU_NULL; }
	if (TRANSP_solid) { uu_toolfree(TRANSP_solid); TRANSP_solid = UU_NULL; }
	DTBAG2 = &DTBAG2_fixed;
	return(0);
}
/*********************************************************************
**    E_FUNCTION     : ncl_ubent_solid_memaloc (n)
**       Allocates memory for composite solid components.
**    PARAMETERS
**       INPUT  :  n - number of components in composite solid.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS  
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubent_solid_memaloc (n)
int n;
{
	DTBAG_solid  = (struct NCL_fixed_databag *) uu_toolmalloc((n)*
				  sizeof(struct NCL_fixed_databag));
	DTBAG2_solid  = (struct NCL_fixed_databag *) uu_toolmalloc((n)*
				  sizeof(struct NCL_fixed_databag));
	ATTR_solid   = (struct UC_attributedatabag *) uu_toolmalloc((n)*
				  sizeof(struct UC_attributedatabag));
	TRANSP_solid = (UM_transf *) uu_toolmalloc((n)*
				  sizeof(UM_transf));
	DTBAG2 = DTBAG2_solid;
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_retrieve_ent (ept,attpt,tmatpt)
**       Retrieves entity, its attributes & trnasformation matrix from
**       active unibase.
**    PARAMETERS
**       INPUT  :  ept->key - key number of requested entity.
**       OUTPUT :
**                 ept    - pointer to the entity structure.
**                 attpt  - pointer to the entity attribute structure.
**                 tmatpt - pointer to the entity transformation matrix.
**    RETURNS      : status = UU_SUCCESS if OK. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  ncl_retrieve_ent (ept,attpt,tmatpt)
  struct NCL_fixed_databag *ept;
  struct UC_attributedatabag *attpt;
  UM_transf *tmatpt;
	{
	 int status;
	 UU_KEY_ID key;
	 status = UU_FAILURE;
	 key = ept->key;
	 if (ncl_retrieve_data_fixed(ept) == 0)
		{
		 if (uc_retrieve_attr (key,attpt) == 0)
			{
			 status = uc_retrieve_transf (key,tmatpt);
			}
		}
	 uu_dexit;
	 return(status);
	} 

/*********************************************************************
**    E_FUNCTION     : ncl_createub_ent (ept,attpt,trmat)
**       Creates entity in active unibase using pointers to entity data
**       structure, its attributes & trnasformation matrix.
**    PARAMETERS
**       INPUT  :  ept    - pointer to the entity structure.
**                 attpt  - pointer to the entity attribute structure.
**                 tmatpt - pointer to the entity transformation matrix.
**       OUTPUT:
**         none
**    RETURNS      : status = UU_SUCCESS if OK. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  ncl_createub_ent (ept,attpt,trmat)
  struct NCL_fixed_databag *ept;
  struct UC_attributedatabag *attpt;
  UM_transf *trmat;
	{
	 int status;
	 status = UU_FAILURE;
	 if (um_create_geom (ept,UM_DEFAULT_TF,UM_CURRENT_ATTR) == 0)
		 status = ncl_updateub_ent (ept,attpt,trmat); 
	 uu_dexit;
	 return(status);
	} 

/*********************************************************************
**    E_FUNCTION     : ncl_updateub_ent (ept,attpt,trmat)
**       Updates entity attributes & trnasformation matrix with the
**       current data when it is possibility that existing data in 
**       unibase defaults or is not valid anymore.
**    PARAMETERS
**       INPUT  :  ept    - pointer to the entity structure.
**                 attpt  - pointer to the entity attribute structure.
**                 tmatpt - pointer to the entity transformation matrix.
**       OUTPUT:
**         none
**    RETURNS      : status = UU_SUCCESS if OK. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_updateub_ent (ept,attpt,trmat)
struct NCL_fixed_databag *ept;
struct UC_attributedatabag *attpt;
UM_transf *trmat;
{
	int status;
	struct UM_transf_rec trnpt;
	status = UU_SUCCESS;
	attpt->key = trnpt.key = ept->key;
	trnpt.rel_num = UM_TRANSFORM_REL;
	ur_retrieve_transf(&trnpt);
	um_tftotf(trmat, trnpt.tfmat);
	status = ur_update_attr(attpt) + ur_update_transf(&trnpt); 
		 
	uu_dexit;
	return(status);
} 
/*********************************************************************
**    E_FUNCTION     : ncl_ubget_rbsf(e1)
**       If there are ssplines associated with the
**       surface, retrieve those entities.
**    PARAMETERS
**       INPUT  :  e1 pointer to the surface structure.
**
**       OUTPUT:
**         none
**    RETURNS      : status = UU_SUCCESS if OK. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_ubget_rbsf (e1)
struct UM_rbsplsrf_rec *e1;
{
	int i, ncv, status;
	UM_transf tmat;
	struct NCL_fixed_databag temp;
	struct UC_attributedatabag tatr;

	status = UU_SUCCESS;
/*
...Allocate memory for the cvonsf's.
*/ 
	ncv    = e1->no_sskey; 
	if(ncv>0)
	{
		ncl_ubent_memfree ();
		ncl_ubent_memaloc (ncv);
/*
.....Retrieve each entity.
*/
		for (i=0; i<e1->no_sskey && status == UU_SUCCESS; i++)
		{
			temp.key = tatr.key = e1->sskey[i];
			if (temp.key != 0)
			{
				status = ncl_retrieve_ent (&temp,&tatr,&tmat);
				if (status  == UU_SUCCESS)
				{
					uu_list_push (&DTB_ls, &temp);
					uu_list_push (&ATT_ls, &tatr);
					uu_list_push (&TRN_ls, &tmat);
					if (temp.rel_num == UM_COMPCRV_REL)
						status = ncl_ubget_ccv (&temp); 
				}
			} 
		}
	}
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_create_geom(e1)
**       Save entity key fields, create geometry and restore key fields.
**    PARAMETERS
**       INPUT  :
**         e1     - pointer to entity.
**       OUTPUT:
**         none
**    RETURNS      : UU_SUCCESS if OK, otherwise UU_FAILURE. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubcopy_create_geom (e1,attptr,tfmat)
struct NCL_fixed_databag *e1;
struct UC_attributedatabag *attptr;
UM_transf tfmat;
{
	int n1, status;

	e1->key = 0;
	switch (e1->rel_num)
	{
	case NCL_SURF_REL:
	case NCL_TRIMSF_REL:
	case NCL_REVSURF_REL:
	case NCL_NETSF_REL:
	{
		status = ncl_ubcopy_compon (e1);
		if (status == UU_SUCCESS) status = ncl_updateub_ent (e1,attptr,tfmat); 
		break;
	}
	case UM_RBSPLSRF_REL:
	{
		struct UM_rbsplsrf_rec *rsf = (struct UM_rbsplsrf_rec *)e1;
		n1 = rsf->no_sskey;
		rsf->no_sskey = 0;
		status = ncl_createub_ent (e1,attptr,tfmat);
		if (status == UU_SUCCESS) status = ncl_ubcopy_compon (e1);
		rsf->no_sskey = n1;
		break;
	}
	default:
		status = ncl_createub_ent (e1,attptr,tfmat);
		if (status == UU_SUCCESS) status = ncl_ubcopy_compon (e1);
		break;
	}

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_set_ptrs()
**       Initialize pointers to component lists;
**    PARAMETERS
**       INPUT  :
**         e1     - pointer to entity.
**       OUTPUT:
**         none
**    RETURNS      : UU_SUCCESS if OK, otherwise UU_FAILURE. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubcopy_set_ptrs ()
{
	DTB_pt = (struct NCL_fixed_databag *) UU_LIST_ARRAY (&DTB_ls);
	ATT_pt = (struct UC_attributedatabag *) UU_LIST_ARRAY (&ATT_ls);
	TRN_pt = (UM_transf *) UU_LIST_ARRAY (&TRN_ls);
	CID_pt = (struct UM_cid_rec *) UU_LIST_ARRAY (&CID_ls);
	SYM_pt = (struct NCL_fixed_databag *) UU_LIST_ARRAY (&SYM_ls);
	SATT_pt = (struct UC_attributedatabag *) UU_LIST_ARRAY (&SATT_ls);
	STRN_pt = (UM_transf *) UU_LIST_ARRAY (&STRN_ls);
	return(UU_SUCCESS);
}
