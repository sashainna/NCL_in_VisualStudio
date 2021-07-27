/*********************************************************************
**    NAME         :  neub2sup.c
**       CONTAINS:
**       ubfind (prm,ubname,subnm,rfname,rsubnm,i2fnd,i2erok,i2cpd,
**                 nlay,layer)
**       ub2lod (itype,istat)
**       ncl_ubkey_delete (key)
**       ncl_ubput_keys (type,in_key,as_key)
**       ncl_ubkey_init ()
**       ncl_ubkey_free ()
**       ncl_ubkey_empty ()
**       ncl_ubkey_search (type,key)
**       ubput (ubname,usub,key,ierr)
**       ncl_ubget_draftent(ubname,prm,oldkey,isub)
**       void ncl_ubget_symbol(ubname,prm,oldkey)
**       int ncl_ubfiltr (prm,ubptr,subnm,rfptr,rsubnm)
**       int ubact (ibnum)
**       int ncl_ubput_ent (eptr)
**       int ncl_ubput_ent1 (eptr)
**       int ncl_ubsearch (e1)
**       int ncl_ubget_labloc (labkey,index,pos)
**       int ncl_ubcopy_labloc (labkey,index,pos)
**       f77_fdst ()
**       f77_resu ()
**       ncl_put_subscript (e1,isub)
**		   void chkwstr
**       void ncl_ubcheck_symnam(label,sub,type,outfl,outerr)
**       void ncl_ubchg_symnam(eptr,type)
**
**    MODULE NAME AND RELEASE LEVEL
**       neub2sup.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/10/18 , 15:30:22
************************************************************************/

#include "usysdef.h"
#include "mfort.h"
#include "xenv1.h"
#include "mdrel.h"
#include "class.h"
#include "nclfc.h"
#include "mcrv.h"
#include "mxxx.h"
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
#include "rver9700.h"
#include "adraft.h"
#include "adrf.h"

extern int NCL_ubcopy;
extern UM_transf *REFTM, *MODTM;
extern UM_transf REF_tm, MOD_tm;

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
extern UU_KEY_ID LABTBL2_KEY;
extern UU_LOGICAL ncl_where, UR_load_env;
extern int UR_readenv;
extern UU_LOGICAL NCL_create_compcrv;
extern UU_LOGICAL NCL_copy_compcrv;
extern UX_pathname UR_exnam[2];
extern struct UR_env_table_rec	UR_environ_table[];
void chkwstr(char*, int*, char*, int*, int*);

static int S_ncl_match();
static int S_match_after_star();
static void S_get_master(),S_put_master();

UU_LIST PUTIN_key, GETIN_key, PUTAS_key, GETAS_key;

/*********************************************************************
**    E_FUNCTION     : ubfind (prm,ubname,subnm,rfname,rsubnm,
**                             i2fnd,i2erok,i2cpd,nlay,layer)
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ubfind(prm,ubname,subnm,rfname,rsubnm,i2fnd,i2erok,i2cpd,nlay,layer,offset)
UM_int2 *prm,*i2fnd,*i2erok,*i2cpd,*nlay;
UM_int4 *subnm,*rsubnm,layer[],*offset;
UM_f77_str_ptr  ubname,rfname;
{
	char ubptr[NCL_MAX_LABEL_AND_SUBSCRIPT], rfptr[NCL_MAX_LABEL_AND_SUBSCRIPT], *p;
	int  i,iren;
	UM_int2 ifl;
/*
...Convert entity names for C processing
*/
	if (*nlay == 0)
	{
		i = NCL_MAX_LABEL;
		p = UM_cstr_of_f77_str(ubname);
		strncpy(ubptr,p,i);
		ul_strip_blanks(ubptr,&i);

		i = NCL_MAX_LABEL;
		p = UM_cstr_of_f77_str(rfname); 
		strncpy(rfptr,p,i); 
		ul_strip_blanks(rfptr,&i);
	}
/*
...Check if REFSYS (MODSYS) is active so we can get tfmat 
...once for multiple get.
*/
	gtref (REF_tm,&ifl);
	if (ifl) REFTM = (UM_transf *) REF_tm;
	else REFTM = UU_NULL;
	gtmod (MOD_tm,&ifl);
	if (ifl) MODTM = (UM_transf *) MOD_tm;
	else MODTM = UU_NULL;
/*
...Set flag OK since secondary unibase is loaded 
...when open succesfully
*/
	*i2erok = 1;
/*
.....Set automatic rename flag
*/
	iren = *i2cpd;
/*
...Search unibase for specified entries
*/
	*i2fnd = (UM_int2) ncl_ubfiltr (*prm,iren,ubptr,*subnm,rfptr,*rsubnm,
		*nlay,layer,*offset);
	*i2cpd = *i2fnd;
}
/*********************************************************************
**    E_FUNCTION     : ub2lod (istat)
**    PARAMETERS
**       INPUT  : istat  - 1 when loaded successfuly, otherwise
**                         unibase error number (URM_*) as specified
**                         in ur_lp02v.  
**                itype  - 0 = binary .u, 1 = ASCI .ud file
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ub2lod (itype,istat)
UM_int2 *itype,*istat;
{
	int status, ftype,isav;
	UV_view view;

	ftype = *itype;
/*
...
... Clear the save/load bit map (used here to mark newly loaded tuples)
...
*/
	uu_unireset2();
	ur_svld_clear();
/*
...
... Load Unibase file.
... vp 2/18/98 add ASCI unibase support
*/
	ux_file_rewind(lub2,UX_PRTERRS);
	status = ur_lp02c(lub2,ftype);
/*
...Load environment from unibase file
... vp 2/18/98 need environment to handle CPLN key 
*/
	isav = UR_readenv;
	if (status == UU_SUCCESS )
		status = (ftype == 1)? ur_rd_txt_env(lub2): ur_environ_in(lub2);
	UR_readenv = 0;
	if (status == 0) 
	{
		UU_LOGICAL init;
		struct NCL_fixed_databag e;
struct NCL_scalar_rec *s1;
		*istat = 1;
/*
... vp 2/18/98
...process all loaded tuples for special cases
......remove label table and adjust label flag in attribute struct.
......do not be confused with UR_low_key, it should be 1.
*/
		init = UU_TRUE;
		while (um_getallobjs(init, &e.key) == 0)
		{
			init = UU_FALSE;
			if (e.key >= UR_low_key)
			{
				ur_retrieve_data_relnum(e.key,&e.rel_num);
				if (e.rel_num == NCL_LABTBL_REL)
					ur_delete_all (e.key);
				else if (LABTBL_KEY > 0)
					ncl_fix_attr_label (e.key,e.rel_num);
/*
.....Update attributes
*/
				if (NCL_infile_version != NCL_version)
					ur_update_attribut(e.key,e.rel_num);
/*
......added routine to handle data with text string
*/
				if ((NCL_infile_version < 10.202)&&(e.rel_num == NCL_DATAST_REL))
				{
					um_retrieve_data_fixed(&e);
					ncl_fix_data (&e);
				}
/*
.....reset all the segment
*/
				ur_update_disp_segid(e.key, -1);
			}
		}
	}
	else *istat = status; 
/*
......if it is the old version < 9.4
......then update the old UM_mtrlmdl_rec_old strcutre UM_mtrlmdl_old to 
......the new UM_mtrlmdl_rec strcutre UM_mtrlmdl
*/
	if (NCL_infile_version<9.350) ur_restore_second_material();
	if (NCL_infile_version<9.651) ur_restore_second_label();
	ur_getu_work();
	if (status==0)
	{
		if (uv_getvnm("Extern Unibase", &view) == UU_SUCCESS)
		{
			uv_update_secondview(2);
		}
	}
	ur_getu_second();
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubput_key ()
**       Store keys in their appropraite list.
**    PARAMETERS
**       INPUT  :  
**          type   - 0: entities are being stored using PUT
**                   1: entities are being retrieved using GET
**          in_key - key of entity in unibase copied from
**          as_key - key of entity in unibase copied to
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ubkey_delete (key)
UU_KEY_ID key;
{
	int index;
	UU_KEY_ID okey;
	UU_LOGICAL found;

	ncl_ubkey_search(2-UR_active,key,&okey,&found,&index);
	if (UR_active == 1)
	{
		uu_list_delete(&GETIN_key,index,1);
		uu_list_delete(&GETAS_key,index,1);
	}
	else
	{
		uu_list_delete(&PUTIN_key,index,1);
		uu_list_delete(&PUTAS_key,index,1);
	}
	uc_delete(okey);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubput_keys ()
**       Store keys in their appropraite list.
**    PARAMETERS
**       INPUT  :  
**          type   - 0: entities are being stored using PUT
**                   1: entities are being retrieved using GET
**          in_key - key of entity in unibase copied from
**          as_key - key of entity in unibase copied to
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ubput_keys (type,in_key,as_key)
int type;
UU_KEY_ID *in_key,*as_key;
{
	if (type == 0) 
	{
		uu_list_push(&PUTIN_key,in_key);
		uu_list_push(&PUTAS_key,as_key);
	}
	else 
	{
		uu_list_push(&GETIN_key,in_key);
		uu_list_push(&GETAS_key,as_key);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubkey_init ()
**       Initialize key lists.  The key lists are used to check for
**       entities already retrieved when draft entities are being 
**       copied so duplicate geometry is not created/stored.
**    PARAMETERS
**       INPUT  :  
**          none
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubkey_init ()
{
	uu_list_init (&GETAS_key,sizeof(UU_KEY_ID),0,20);
	uu_list_init (&PUTAS_key,sizeof(UU_KEY_ID),0,20);
	uu_list_init (&GETIN_key,sizeof(UU_KEY_ID),0,20);
	uu_list_init (&PUTIN_key,sizeof(UU_KEY_ID),0,20);

	return(0);
}
/*********************************************************************
**    E_FUNCTION     : ncl_ubkey_free ()
**       Free key lists.
**    PARAMETERS
**       INPUT  :  
**          none
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubkey_free ()
{
	uu_list_free(&GETAS_key);
	uu_list_free(&PUTAS_key);
	uu_list_free(&GETIN_key);
	uu_list_free(&PUTIN_key);
	return(0);
}
/*********************************************************************
**    E_FUNCTION     : ncl_ubkey_empty ()
**       Empty key lists (set the current count to zero but do not
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
int ncl_ubkey_empty ()
{
	UU_LIST_EMPTY(&GETIN_key);
	UU_LIST_EMPTY(&PUTIN_key);
	UU_LIST_EMPTY(&GETAS_key);
	UU_LIST_EMPTY(&PUTAS_key);
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubkey_search (type,key)
**       Searches key list for given key.
**    PARAMETERS
**       INPUT  :  
**          type  - 0: check keys for entities stored from PUT
**                  1: check keys for entities retrieved from GET
**          ikey  - original entity key
**       OUTPUT :
**          okey  - key given to enity when copied into unibase
**          found - return true is entity with matching key is found
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ubkey_search (type,ikey,okey,found,index)
int type,*index;
UU_KEY_ID ikey,*okey;
UU_LOGICAL *found;
{
	UU_KEY_ID *in_key,*as_key;
	int i,count;

	*found = UU_FALSE;
	*index = -1;
	if (type == 0)
	{
		in_key = (UU_KEY_ID *)UU_LIST_ARRAY(&PUTIN_key);
		as_key = (UU_KEY_ID *)UU_LIST_ARRAY(&PUTAS_key);
		count = PUTIN_key.cur_cnt;
	}
	else
	{
		in_key = (UU_KEY_ID *)UU_LIST_ARRAY(&GETIN_key);
		as_key = (UU_KEY_ID *)UU_LIST_ARRAY(&GETAS_key);
		count = GETIN_key.cur_cnt;
	}
	*okey = ikey;
	for (i=0;i<count;i++)
	{
		if (in_key[i] == ikey)
		{
			*okey = as_key[i];
			*found = UU_TRUE;
			*index = i;
			break;
		}
	}
	return(0);
}
				
/*********************************************************************
**    E_FUNCTION     :ubput (ubname,usub,key,ierr)  
**       Put selected entity into file.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ubput (ubname,usub,key,prm,ierr)
UM_f77_str_ptr       ubname;
UM_int4              *key;
UM_int2              *ierr,*usub,*prm;
{
	char        uname[NCL_MAX_LABEL], *p, label[NCL_MAX_LABEL];
	int         i, subscr,rel_num;
	int         status;
	struct NCL_fixed_databag e,*te;
	struct UA_generic_draft d;
	UM_int2 ifl;
	UU_KEY_ID keys[2];
	*ierr = 0;
/*
...
...Convert Fortran Character strings to C strings
...
*/
	keys[0] = *key;
	i = NCL_MAX_LABEL;
	p = UM_cstr_of_f77_str(ubname);
	strncpy(uname,p,i);
	ul_strip_blanks(uname,&i);
	subscr = *usub;
/*
...Replace name if saved 'as'
*/
	if (*prm != 34)
	{
		e.key  = *key;
		status = ncl_retrieve_data_fixed(&e);
		rel_num = e.rel_num;
	}
	else
	{
		d.key = *key;
		ur_retrieve_data(&d,sizeof(struct UA_generic_draft));
		rel_num = d.rel_num;
	}
/*
.....vp 10/1/97 uv curves not supported
.....temporary ignore 
*/
	if (rel_num == UM_UVCVONSF_REL) return(0);

	strncpy (label,uname,i); 
	while (i<NCL_MAX_LABEL) label[i++] = ' ';
	if (*prm != 34) strcpy(e.label,label);
	else
	{
		te = (struct NCL_fixed_databag *)&d;
		strcpy(te->label,label);
	}
	REFTM = UU_NULL;
	MODTM = UU_NULL;
/*
.....vp 28-may-97. Process scalars: 
.....subscript is in different spot
*/
	if (*prm != 34) ncl_put_subscript (&e,subscr);
	else ncl_put_subscript (&d,subscr);
/*
...Check if REFSYS (MODSYS) is active so we can get tfmat
...once for multiple put.
*/
	if (rel_num != NCL_SCALAR_REL && rel_num != NCL_DATAST_REL &&
	    rel_num != NCL_TEXTVAR_REL)
	{
		gtref (REF_tm,&ifl);
		if (ifl) REFTM = (UM_transf *) REF_tm;
		gtmod (MOD_tm,&ifl);
		if (ifl) MODTM = (UM_transf *) MOD_tm;
	}
/*
...Copy entity from NCL unibase to output unibase
*/
	if (rel_num != UA_LINEAR_DIMS_REL)
	{
		status = ncl_ubput_ent (&e);
		if (status == UU_SUCCESS) keys[1] = e.key;
	}
	else
	{
		status = ncl_ubput_ent (&d);
		if (status == UU_SUCCESS) keys[1] = d.key;
	}
	if (status == UU_SUCCESS) ncl_ubput_keys(0,&keys[0],&keys[1]);
	if (status == 0) UR_EOF_stat = UU_TRUE;
	return(0);
}
/*********************************************************************
**    E_FUNCTION     :ncl_ubget_draftent(ubname,prm,oldkey) 
**       Search for the next available draft entity based on the
**       given parameter (prm) and the key of the last one found.
**    PARAMETERS
**       INPUT  :
**          oldkey - the key for the last found symbol or instance,
**                   which is used so there are no repeats
**       OUTPUT :
**          prm    - set to -1 if none found
**          ubname - label of draft entity retrieved
**          isub   - label subscript of draft entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ubget_draftent(ubname,prm,oldkey,isub)
UM_f77_str_ptr ubname;
UM_int2 *prm;
UM_int4 *oldkey,*isub;
{
	struct NCL_fixed_databag e;
	UU_LOGICAL init;
	char *cstr;
	int cmp_rel;
	UM_int2 tprm;
/*
.....Initialize routine
.......prm is set to -1 so that if none are found the calling routine
.......knows to stop
*/
	e.key = 0;
	tprm = *prm;
	*prm = -1;
	cstr = UM_cstr_of_f77_str(ubname);
	init = UU_TRUE;
/*
.....Search for next available symbol or instance
*/
	while (um_getallobjs(init, &e.key) == 0)
	{
		init = UU_FALSE;
		if (e.key <= *oldkey) continue;
		ur_retrieve_data_relnum(e.key,&e.rel_num);
		ncl_retrieve_data_fixed(&e);
		if (e.rel_num == UA_LINEAR_DIMS_REL)
		{
			*prm = tprm;
			strncpy(cstr,e.label,NCL_MAX_LABEL);
			*isub = e.subscr;
			*oldkey = e.key;
			break;
		}
	}
}
/*********************************************************************
**    E_FUNCTION     :ncl_ubget_symbol(ubname,prm,oldkey) 
**       Search for the next available symbol or instance based on the
**       given parameter (prm) and the key of the last one found.
**    PARAMETERS
**       INPUT  :
**          prm    - type: 31 = find the next symbol
**                         32 = find the next symbol instance
**          oldkey - the key for the last found symbol or instance,
**                   which is used so there are no repeats
**       OUTPUT :
**          ubname - label of symbol or instance retrieved
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ubget_symbol(ubname,prm,oldkey)
UM_f77_str_ptr ubname;
UM_int2 *prm;
UM_int4 *oldkey;
{
	struct NCL_fixed_databag e;
	UU_LOGICAL init;
	char *cstr;
	int cmp_rel;
	UM_int2 tprm;
/*
.....Initialize routine
.......prm is set to -1 so that if none are found the calling routine
.......knows to stop
*/
	e.key = 0;
	tprm = *prm;
	cmp_rel = (*prm == 31)? UB_SYMBOL_REL : UB_INSTANCE_REL;
	*prm = -1;
	cstr = UM_cstr_of_f77_str(ubname);
	init = UU_TRUE;
/*
.....Search for next available symbol or instance
*/
	while (um_getallobjs(init, &e.key) == 0)
	{
		init = UU_FALSE;
		if (e.key <= *oldkey) continue;
		ur_retrieve_data_relnum(e.key,&e.rel_num);
		ncl_retrieve_data_fixed(&e);
		if (e.rel_num == cmp_rel)
		{
			*prm = tprm;
			strncpy(cstr,e.label,NCL_MAX_LABEL);
			break;
		}
	}
}
/*********************************************************************
**    E_FUNCTION     : ncl_ubfiltr (prm,iren,ubptr,subnm,rfptr,rsubnm,
**                                  nlay,layer)
**       Search unibase for specified entity (type, name or all) to
**       copy to the active NCL unibase.
**    PARAMETERS
**       INPUT  : prm    - switch defining type of entry, see comments.
**                iren   - 1 = Use NCL autonaming feature to rename entities.
**                ubptr  - label name to search in unibase.
**                subnm  - subscript to search
**                rfptr  - label name to apply ('as' specified) 
**                rsubnm - subscript to apply ('as' specified) 
**                nlay   - Number of layers to retrieve.  0 = retrieve by name.
**                layer  - Array of layers to retrieve.
**               
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ubfiltr (prm,iren,ubptr,subnm,rfptr,rsubnm,nlay,layer,offset)
UM_int2   prm,nlay;
UM_int4 subnm,rsubnm,layer[],offset;
int iren;
char      ubptr[], rfptr[];
{
	UU_LOGICAL init,copied;
	UU_LOGICAL ncl_legal_relation();
	struct NCL_fixed_databag e,e2;
	int status,nc,nl,i,j,isub,subscr,inlay,match,nc1,nc2,subnum;
	char lb[NCL_MAX_LABEL], cmpstr[NCL_MAX_LABEL], tmpstr[NCL_MAX_LABEL];
	struct NCL_scalar_rec *s1;
	struct UA_generic_draft d;

	init = UU_TRUE;
	status = 0;
	nc   = strlen(ubptr);
	ul_strip_blanks(ubptr,&nc);
	subscr = rsubnm;
	while (um_getallobjs(init, &e.key) == 0)
	{
		init = UU_FALSE;
/*
...
... Update ranfile only for entity's being merged
......Exclude symbols, symbol instances and draft entities from GET unless
......prm is specifically for one of those types to maintain previous
......functionality of GET
*/
		if (e.key >= UR_low_key)
		{
			ur_retrieve_data_relnum(e.key,&e.rel_num);
			if (e.rel_num != UA_LINEAR_DIMS_REL)
				ncl_retrieve_data_fixed(&e);
			if (ncl_legal_relation(e.rel_num) || e.rel_num == UB_INSTANCE_REL
				||	e.rel_num == UB_SYMBOL_REL || e.rel_num == UA_LINEAR_DIMS_REL)
			{
				if (prm < 0)
				{
					nl = strlen (e.label);
					/*if (strncmp(e.label,ubptr,3)==0)
					{
						strncpy(tmpstr, ubptr, 3);;
					}*/
/*
.....Make sure the label isn't too long. JLS 11/23/99
*/
					if(nl > NCL_MAX_LABEL)
					{
						nl = NCL_MAX_LABEL;
						e.label[NCL_MAX_LABEL -1] = '\0';
					}
					strncpy(lb,e.label,nl);
					ul_strip_blanks(lb,&nl);
				}
				isub = ncl_get_subscript(&e);
				switch (prm)
				{
					case  0:         /*** GET/ALL[,OFFSET,n] ***/
						if (e.rel_num==UB_INSTANCE_REL || e.rel_num==UB_SYMBOL_REL ||
							 e.rel_num==UA_LINEAR_DIMS_REL)
							break;
						else
							ncl_ubcopy_ent(&e,iren,offset,&copied);
						status = 1;
						break;
					case -2:         /*** GET/string ***/
/*
......wildcard string could be '*', '*str', 'str1*str2', '*str1*str2*...*str'
......just call 'chkwstr' to see if token2 match the wild card string
*/
						if (e.rel_num==UB_INSTANCE_REL || e.rel_num==UB_SYMBOL_REL ||
							 e.rel_num==UA_LINEAR_DIMS_REL)
							break;
						nc2 = strlen (e.label);
						if(nc2 > NCL_MAX_LABEL)
						{
							nc2 = NCL_MAX_LABEL;
							e.label[NCL_MAX_LABEL -1] = '\0';
						}
						strncpy(cmpstr, e.label, nc2);
						ul_strip_blanks(cmpstr,&nc2);
/*
......get the subcript, it can't general get e.subscr but need treat differently
......for different type
*/
					   if (e.rel_num == NCL_SCALAR_REL || e.rel_num == NCL_DATAST_REL
							|| e.rel_num == NCL_TEXTVAR_REL)
					   {
							s1 = (struct NCL_scalar_rec *)&e;
							subnum = s1->subscr;
					   }
					   else
							subnum = e.subscr;
						if (subnum>0)
						{
							sprintf(cmpstr, "%s(%d)", cmpstr, subnum);
						}
						nc1 = strlen(cmpstr);
						nc2 = strlen(ubptr);
						chkwstr(cmpstr, &nc1, ubptr, &nc2, &match);
						if (match==1) 
						{ 
							ncl_ubcopy_ent(&e,iren,0,&copied); 
							status = 1; 
						}
						break;
					case -3:         /*** GET/name   ***/
						nc2 = strlen (e.label);
						if(nc2 > NCL_MAX_LABEL)
						{
							nc2 = NCL_MAX_LABEL;
							e.label[NCL_MAX_LABEL -1] = '\0';
						}
						strncpy(tmpstr, e.label, nc2);
						ul_strip_blanks(tmpstr,&nc2);
						if (e.rel_num==UB_INSTANCE_REL || e.rel_num==UB_SYMBOL_REL)
							ul_to_upper(&tmpstr);
						if (strncmp(tmpstr,ubptr,nc) == 0 && 
							nl == nc && isub == subnm)
						{
							if (e.rel_num == UB_INSTANCE_REL)
								S_get_master(&e);
							i = strlen(rfptr);
							if (e.rel_num != UB_INSTANCE_REL &&
								 e.rel_num != UB_SYMBOL_REL)
								strncpy(e.label,rfptr,i);
							while (i<NCL_MAX_LABEL) e.label[i++] = ' ';
							ncl_put_subscript(&e,subscr);
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;      
							goto ext;
						}
						break;
					case  -10:         /*** GET/LAYER ***/
						if (e.rel_num==UB_INSTANCE_REL || e.rel_num==UB_SYMBOL_REL ||
							 e.rel_num==UA_LINEAR_DIMS_REL)
							break;
						ur_retrieve_layer(e.key,&inlay);
						for (i=0;i<nlay;i++)
						{
							if (layer[i] < 0)
							{
								if (inlay >= (layer[i] * -1) && inlay <= layer[i+1])
								{
									ncl_ubcopy_ent(&e,iren,offset,&copied);
									break;
								}
                        else
									i++;
							}
							else if (inlay == layer[i])
							{
								ncl_ubcopy_ent(&e,iren,offset,&copied);
								break;
							}
						}
						status = 1;
						break;
					case  2:         /*** GET/SH     ***/
						if (e.rel_num == NCL_SHAPE_REL) 
						{ 
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  3:         /*** GET/PT     ***/
						if (e.rel_num == UM_POINT_REL ||
							e.rel_num == NCL_POINT_REL)  
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1; 
						}
						break;
					case  4:         /*** GET/VE     ***/
						if (e.rel_num == NCL_VECTOR_REL)   
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  5:         /*** GET/LN     ***/
						if (e.rel_num == UM_LINE_REL ||
							e.rel_num == NCL_LINE_REL)   
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  6:         /*** GET/PL     ***/
						if (e.rel_num == NCL_PLN_REL)   
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  7:         /*** GET/CI     ***/
						if (e.rel_num == UM_CIRCLE_REL || 
							e.rel_num == NCL_CIRCLE_REL )  
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  8:         /*** GET/CV     ***/
						if (e.rel_num == NCL_CURVE_REL || 
							e.rel_num == NCL_EVALCV_REL ||
							e.rel_num == UM_RBSPLCRV_REL ||
							e.rel_num == UM_CONIC_REL ||   
							e.rel_num == UM_COMPCRV_REL)
						{ 
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  9:         /*** GET/SF     ***/
						if (e.rel_num == NCL_SURF_REL ||
							e.rel_num == NCL_REVSURF_REL ||
							e.rel_num == NCL_EVALSF_REL ||
							e.rel_num == UM_RBSPLSRF_REL ||
							e.rel_num == NCL_MESHSURF_REL ||
							e.rel_num == NCL_QUILTSURF_REL ||
							e.rel_num == NCL_NETSF_REL ||
							e.rel_num == NCL_TRIMSF_REL)
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  10:        /*** GET/MX     ***/
						if (e.rel_num == NCL_MATRIX_REL)
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  21:        /*** GET/PV     ***/
						if (e.rel_num == NCL_POINTVEC_REL)  
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  20:        /*** GET/PN     ***/
						if (e.rel_num == NCL_PATERN_REL) 
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  NCLI_DATAST: /*** GET/DATA   ***/
						if (e.rel_num == NCL_DATAST_REL) 
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  NCLI_NOTE: /*** GET/ANOTE   ***/
						if (e.rel_num == UA_TEXT_REL) 
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case  NCLI_SOLID: /*** GET/SOLID   ***/
						if (e.rel_num == UM_SOLID_REL) 
						{
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;
						}
						break;
					case 31: /*** GET/SYMBOL   ***/
						if (e.rel_num == UB_SYMBOL_REL)
						{ 
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;   
						}  
						break;
					case 32: /*** GET/PLACE   ***/
						if (e.rel_num == UB_INSTANCE_REL)
						{ 
							S_get_master(&e,iren);
							ncl_ubcopy_ent(&e,iren,0,&copied);
							status = 1;   
						}  
						break;
					case 34: /*** GET/DRAFT   ***/
						if (e.rel_num == UA_LINEAR_DIMS_REL) 
						{
							d.key = e.key;
							d.rel_num = e.rel_num;
							ua_retrieve_data(&d,sizeof(struct UA_generic_draft));
							ncl_ubcopy_ent(&d,iren,0,&copied);
							status = 1;
						}
						break;
					default:;
				}
			}
		}
	}
ext:;
	return(status);
}
/*********************************************************************
**    E_FUNCTION     : ubact (ibnum)
**       Activate specified unibase as a working NCL unibase.
**    PARAMETERS
**       INPUT  : ibnum - 1 = NCL unibase, 2 = secondary unibase.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ubact (ibnum)
UM_int2 *ibnum;
{
	int status;
	status = UU_SUCCESS;
	if (*ibnum == 2)
	{
		ur_getu_second();
	}
	else if (*ibnum == 1)
	{
		ur_getu_work();
	}
	return(status);
} 

/*********************************************************************
**    E_FUNCTION     : ncl_ubput_ent (eptr)
**       Copy entity from NCL unibase 1 to 2 unibase.
**    PARAMETERS
**       INPUT  :   eptr - pointer to the entity
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ubput_ent (e1)
struct NCL_fixed_databag *e1;
{
	int status;

	if (e1->rel_num == UB_INSTANCE_REL)
		S_put_master(e1);
	status = ncl_ubput_ent1(e1);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubput_ent1 (eptr)
**       Copy entity from NCL unibase 1 to 2 unibase.
**    PARAMETERS
**       INPUT  :   eptr - pointer to the entity
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ubput_ent1 (e1)
struct NCL_fixed_databag *e1;
{
	struct NCL_fixed_databag e2;
	UM_transf tran;
	struct UC_attributedatabag attr;
	struct  NCL_nclattr_rec *nattr;
	struct UM_transf_rec transpacket;
	int status,type,i,lbloc,rel_num;
	UM_coord pos;
	UU_LOGICAL blank, lgeo;
	struct UM_rbsplsrf_rec *srf;
	struct UA_generic_draft draft;
	UU_KEY_ID key;

	ncl_reset_surfattr_type();
	status = UU_SUCCESS;
	lgeo    = UU_FALSE;
	type = e1->rel_num;
/*
...Check if entity is not a part of the "composite" entity
*/
	if (!strncmp(e1->label,"@UN    ",strlen(e1->label))) goto done; 
	if (!strncmp(e1->label,"@",1)) goto done; 
	lbloc   = 0;
/*
...Get transformation matrix, attributes & label location 
...before switching to work unibase
*/
	status = ur_retrieve_blanked (e1->key,&blank);
	if (!status) status = uc_retrieve_transf (e1->key,tran);
	if (status == UU_SUCCESS)
		 status = uc_retrieve_attr (e1->key,&attr);

	if (status == UU_SUCCESS && ncl_surfattr_type (e1->rel_num) && attr.rel_num == UM_SURFATTR_REL)
		ncl_set_surfattr_type();
/*
.....vp 2/12/98 romoved label table support
*/
	if (status == UU_SUCCESS && (wf_geom_type(type) == 0 ||
								  ncl_geom_type(type) == 0))
	{
		lgeo    = UU_TRUE;
		nattr  = (struct  NCL_nclattr_rec *) &(attr);
/*
.....check if label display is on and if the label has been altered
*/
		if (ncl_get_label_on(nattr->label_on) &&
			ncl_get_label_alter(nattr->label_on))
		{
			lbloc = nattr->label_on;
			if (ncl_retrieve_labloc (e1,nattr->label_on,pos) != 0) 
			{ lbloc = 0; nattr->label_on = 0; }
		}
	}
/*
...Get components for "composite" entity
*/
	if (status == UU_SUCCESS)
		status = ncl_ubget_compon (e1);
/*
...Check if label exists in output unibase to delete it 
...before moving entity from input unibase
......Draft entities do not have labels and so checking is unnecessary
*/
	ur_getu_second();
	if (type != UA_LINEAR_DIMS_REL)
		status = ncl_ubsearch (e1,UU_FALSE,&key);
/*
...Copy entity from primary unibase to output unibase 
*/
	NCL_create_compcrv = UU_FALSE;
	NCL_copy_compcrv = UU_FALSE;
	NCL_ubcopy = 2;
	ncl_ubcopy_set_ptrs();
/*
...vp 3/25/98 mask all uvcvonsf. Remove this code when
...support for it is provided in put/get
*/ 
	if (type == UM_RBSPLSRF_REL)
	{
		srf = (struct UM_rbsplsrf_rec *) e1;
		i = srf->no_sskey;
		srf->no_sskey = 0;
	}
	if (type != UM_COMPCRV_REL && type != NCL_NETSF_REL &&
		 type != NCL_TRIMSF_REL && type != NCL_SURF_REL &&
		 type != NCL_REVSURF_REL && type != UA_TEXT_REL &&
		 type != UB_SYMBOL_REL && type != UB_INSTANCE_REL &&
		 type != UA_LINEAR_DIMS_REL)
		status = um_create_geom (e1,UM_DEFAULT_TF,UM_CURRENT_ATTR);

	status = ncl_ubcopy_compon (e1);
	if (type == UM_RBSPLSRF_REL) srf->no_sskey = i;
/*
...Get created entity to apply refsys & disply 
*/
	if (status == UU_SUCCESS)
	{
		key = e1->key;
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
		if (REFTM != UU_NULL)
		{
			if (type != UA_LINEAR_DIMS_REL)
				status = uc_transform (&e2, REFTM, UU_TRUE);
			else
				status = uc_transform (&draft, REFTM, UU_TRUE);
		}
		if (MODTM != UU_NULL)
		{
			if (type != UA_LINEAR_DIMS_REL)
				status = uc_transform (&e2, MODTM, UU_TRUE);
			else
				status = uc_transform (&draft, MODTM, UU_TRUE);
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
		if (!status) status = ur_update_blanked (key,blank);
	}
	NCL_ubcopy = 0; 
	if (status == UU_SUCCESS)
		um_copy_layer_struct(attr.layer, 0, 2);
/*		um_set_layer_num(attr.layer);*/
	if (rel_num == NCL_CURVE_REL) ncl_fix_curve (&e2);
	if (lgeo || type==UB_INSTANCE_REL)
	{
	 	ncl_retrieve_data_fixed(&e2); 
		ur_update_view_key(e2.key, ur_get_dispattr_view_in());
		if (lgeo) uc_display(&e2);
		else ub_display_sym(&e2);
	}
	else if (type==UA_LINEAR_DIMS_REL) ua_display_drafting(&draft);
/*
...vp 13-may-97 return status!
*/
done:;
	ur_getu_work();
	ncl_ublist_empty();
	ncl_reset_surfattr_type();

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubsearch (eptr)
**        Checks the unibase if entity with this label & subscript 
**        exists then removes it to replace by new.
**    PARAMETERS
**       INPUT  :
**          eptr  - entity pointer
**          flag  - delete flag (delete if set to true)
**       OUTPUT :
**          key   - key of found entity 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ubsearch (e1,flag,key)
struct NCL_fixed_databag *e1;
UU_LOGICAL flag;
UU_KEY_ID *key;
{
	char label[NCL_MAX_LABEL];
	int status, isub, n1, ncl_get_subscript();
	UU_LOGICAL init,found;
	struct NCL_fixed_databag e2;

/*
...Extract name & subscript
*/
	n1    = NCL_MAX_LABEL;
	strncpy (&label[0],e1->label,n1);
	ul_strip_blanks(label,&n1);
	n1    = strlen(label);
	isub  = ncl_get_subscript (e1);
/*
...Check all keys in unibase
*/
	init   = UU_TRUE;
	status = UU_SUCCESS;
	while (um_getallobjs(init, &e2.key) == 0) 
	{
		init  = UU_FALSE;
		ur_retrieve_data_fixed(&e2);
/*
...Delete entity when name & subscript match
...vp 2/16/98 label can be terminated now at the last charater
...when it is .ud file.  uc_delete replaced by ur_delete
...because we are deleting from 2nd unibase and it is not the same set
...of keys as in main unibase (associate keys!).
*/
		if (strncmp(e2.label,label,n1) == 0 &&
			(*(e2.label+n1) == ' ' || *(e2.label+n1) == '\0')
			&& ncl_get_subscript(&e2) == isub) 
		{
/*			status = uc_delete(e2.key);   */
			*key = e2.key;
			if (!flag) status = ur_delete_all (e2.key);
			else return(2);
			break;
		}
	}
	return(status);
}
		
/*********************************************************************
**    E_FUNCTION     : ncl_ubget_labloc(lbakey,index,pos)
**        Get label position from unibase label table entity.
**                 Not used anymore vp 2/12/98
**    PARAMETERS
**       INPUT  :  labkey - label table key in unibase.
**                 index  - index of label location in the table record
**                          specified by 'attr.label_on' parameter.
**       OUTPUT :  pos[3] - label location coordinates.
**                    
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
int
ncl_ubget_labloc (labkey,index,pos)
UU_KEY_ID labkey;
int index;
UM_coord pos;
{
	int idnum, status;
	UU_KEY_ID savk;

	status = UU_SUCCESS;
	if (labkey == 0) status = UU_FAILURE;
	if (status == UU_SUCCESS)
	{
//
//...Save label table key since it is used with primary unibase
//
		savk  = LABTBL_KEY;
		LABTBL_KEY = labkey;
		idnum = index;
		status = ncl_retrieve_labloc(idnum,pos);
		LABTBL_KEY = savk;
	}
	return (status);
}
*/

/*********************************************************************
**    E_FUNCTION     : ncl_ubcopy_labloc (labkey,index,pos)
**        Save label location in label table of unibase if 
**        exists, create before saving if doesn't.
**                 Not used anymore vp 2/12/98
**    PARAMETERS
**       INPUT  :
**          eptr            entity pointer
**       OUTPUT :
**          none          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
int
ncl_ubcopy_labloc (labkey,index,pos)
UU_KEY_ID labkey;
int *index;
UM_coord pos;
{
	int idnum, status;
	UU_KEY_ID savk;
//
//...Save label table key since it is used with primary unibase
//
	savk  = LABTBL_KEY;
	LABTBL_KEY = labkey;
	idnum  = 1;
	if (labkey == NULL) 
	{ 
		ncl_init_labtbl (labkey);
		if (UR_active == 1) savk = LABTBL_KEY;
		else LABTBL2_KEY = LABTBL_KEY;
	}
	status = ncl_add_labloc (&idnum,pos);
	if (*index < 0) *index = 1 - idnum;
	else *index = idnum;

	LABTBL_KEY = savk;
	return (status);
} 
*/
				
/*********************************************************************
**    E_FUNCTION     : f77_* ()
**       Wraps for fortran call of:
**          ur_flush_del_stack
**          ncl_reset_unicad
**                             
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void f77_fdst ()
{
	ur_flush_del_stack();
}

void f77_resu ()
{
	ncl_reset_unicad();
}
/*********************************************************************
**    E_FUNCTION     : ncl_put_subscript(eptr, isub)
**        Retrieve entity subscript from unibase data structure.
**    PARAMETERS
**       INPUT  :
**          eptr     - entity pointer.
**       OUTPUT :
**          isub     - entity subscript.
**    RETURNS      : Zero
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_put_subscript (e1,isub)
struct NCL_fixed_databag *e1;
int isub;
{
	struct NCL_scalar_rec *escl;
	if (e1->rel_num == NCL_SCALAR_REL || e1->rel_num == NCL_DATAST_REL ||
	    e1->rel_num == NCL_TEXTVAR_REL)
	{
		escl = (struct NCL_scalar_rec *) e1;
		escl->subscr = isub;
	}
	else
		e1->subscr = isub;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : chkwstr(token, nc1, wstr, nc2, match)
**        check if a token is match with wildcard string
**    PARAMETERS
**       INPUT  :
**          token - token string to be checked
**			   nc1:  - length of the token
**          wstr  - wildcard string to be compared
**	         nc2:  - length of the wildcard string
**       OUTPUT :
**          match - 1: matched
**						  0: not matched
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void chkwstr(token, nc1, wstr, nc2, match)
char *token, *wstr;
int *nc1, *nc2, *match;
{
	char string1[65], string2[65];
	strncpy(string1, token, *nc1);
	string1[*nc1]='\0';
	strncpy(string2, wstr, *nc2);
	string2[*nc2]='\0';
	*match = 0;
	if ((*nc1==0) || (*nc2==0)) 
		return;
	if (strcmp(string2, "*")==0)
	{
		*match = 1;
		return;
	}
	*match = S_ncl_match(string2, string1);
	if (*match==-1)
		*match = 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubcheck_symnam(label,sub,type,outfl,outerr)
**        Check if a symbol name is already defined.
**    PARAMETERS
**       INPUT  :
**          label  - laebel to be checked
**          sub    - subscript for label
**			   type   - 61: Master Symbol
**                   62: Symbol Instance
**          outerr - output error flag
**       OUTPUT :
**          outfl  - UU_TRUE if label not used
**                   UU_FALSE if label used
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ubcheck_symnam(label,sub,type,outfl,outerr)
char *label;
int sub,type;
UU_LOGICAL *outfl,outerr;
{	
	UM_f77_str symlab;
	UM_int4 isub,ikey=0;
	UM_real8 origin[3];
	UM_int2 flag;
	UM_int2 nwds,ietype;
	UU_LOGICAL nclg,errflg;
	UM_int4 nclkey,ipg,iel;
	UM_f77_str ncllabel;
	UM_int4 nclsubscript;
	int ttype=type-60;
	int found,i,nc;

	*outfl = UU_TRUE;
	UM_init_f77_str(symlab, label, NCL_MAX_LABEL);
	isub = sub;
	ub_symbol_name(UM_addr_of_f77_str(symlab),&isub,&ikey,&origin,&flag);
	if (flag == ttype)
	{
		*outfl = UU_FALSE;
		goto done;
	}
	nclsubscript = sub;
	UM_init_f77_str(ncllabel, label, NCL_MAX_LABEL);
	found = vxchk(UM_addr_of_f77_str(ncllabel), &nclsubscript, &nclkey, &ipg,
		&iel,&nwds,&ietype);
	if (found == UU_SUCCESS) *outfl = UU_FALSE;
done:;
	if (!(*outfl) && outerr)
	{
		wintst();
		UM_init_f77_str(ncllabel, label, NCL_MAX_LABEL);
		nc = strlen(ncllabel);
		for (i=nc;i<NCL_MAX_LABEL;i++) ncllabel[i] = ' ';
		uerror(UM_addr_of_f77_str(ncllabel), &sub);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_ubchg_symnam(eptr,type)
**        Autoname a symbol or instance.
**    PARAMETERS
**       INPUT  :
**          eptr - entity data
**			   type - 61: Master Symbol
**                 62: Symbol Instance
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_ubchg_symnam(eptr,type)
struct NCL_fixed_databag *eptr;
int type;
{
	char label[NCL_MAX_LABEL],slabel[NCL_MAX_LABEL];
	static int symsub = 1;
	static int inssub = 1;
	UM_int2 idst,nclstatus,i1;
	UU_LOGICAL not_found;
	UU_KEY_ID tkey;
	char output[250]; /*set to match buffer length in ncl_windout */
	UM_f77_str ncllabel;
	int i,nc;

	ur_getu_work();
	strcpy(slabel,eptr->label);
repeat:;
	not_found = UU_TRUE;
	ncl_get_type(eptr->rel_num,&idst);
	i1 = 1;
	namgen(&i1,&idst,UM_addr_of_f77_str(label),&eptr->subscr);
	ncl_ubcheck_symnam(&label,eptr->subscr,type,&not_found,UU_FALSE);
	if (!not_found) goto repeat;
	ur_getu_second();
	strcpy(eptr->label,label);
}

/*********************************************************************
**    E_FUNCTION     : S_get_master (eptr)
**        Copies symbols master from secondary unibase to primary
**        unibase if it has not already been done.  The master is
**        found based on the instance passed in.
**    PARAMETERS
**       INPUT  :
**          eptr - pointer to symbol instance
**       OUTPUT : none
**       RETURNS: none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_get_master(eptr,iren)
struct NCL_fixed_databag *eptr;
int iren;
{
	struct NCL_fixed_databag e2;
	UU_KEY_ID key;
	UU_LOGICAL outerr,copied,not_found=UU_TRUE;
	char output[NCL_MAX_LABEL_AND_SUBSCRIPT + 20];

	ubi_get_master_key_for_inst(eptr,&key);
	e2.key = key;
	ncl_retrieve_data_fixed(&e2);
	ur_getu_work();
	outerr = (iren == 0);
	ncl_ubcheck_symnam(&e2.label,e2.subscr,UB_SYMBOL_REL,&not_found,outerr);
	ur_getu_second();
/*
.....Set to true if e2 is not found
*/
	if (not_found || iren == 1) ncl_ubcopy_ent(&e2,iren,0,&copied);
}

/*********************************************************************
**    E_FUNCTION     : S_put_master (eptr)
**        Copies symbols master from primary unibase to secondary
**        unibase if it has not already been done.  The master is
**        found based on the instance passed in.
**    PARAMETERS
**       INPUT  :
**          eptr - pointer to symbol instance
**       OUTPUT : none
**       RETURNS: none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_put_master(eptr)
struct NCL_fixed_databag *eptr;
{
	struct NCL_fixed_databag e2;
	UU_KEY_ID key;
	UU_LOGICAL skip_error;
	int status;

	skip_error = UU_TRUE;
	ubi_get_master_key_for_inst(eptr,&key);
	e2.key = key;
	ncl_retrieve_data_fixed(&e2);
	ur_getu_second();
	status = ncl_ubsearch(&e2,skip_error,&key);
	ur_getu_work();
/*
.....Set to UU_SUCCESS if e2 is not found
*/
	if (status == UU_SUCCESS)
	{
		ncl_ubput_ent1(&e2,0,0);
	}
}

/*********************************************************************
**    E_FUNCTION     : S_ncl_match (pattern, text)
**        check if a text is match with wildcard string
**    PARAMETERS
**       INPUT  :
**          text     - text string to be checked
**          pattern     - wildcard string to be compared
**       OUTPUT : none
**    RETURNS      : 
**          match     - 1: matched
**						0: not matched
**						-1: abort
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_ncl_match (pattern, text)
char *pattern, *text;
{
	for ( ; *pattern; pattern++, text++)
	{
		if (text[0]=='\0')
		{
			if (*pattern== '*'&& *++pattern == '\0')
				return 1;  /* matched */
			else
				return -1;
		}
		switch (*pattern)
		{
			case '*': 
				return S_match_after_star (pattern, text);
			default:
				if (*pattern != *text)
					return 0;
		}
    }
/* 
.....if end of text not reached then the pattern fails 
*/
	if (*text)
       return 0;
	else  
		return 1;
}

/*********************************************************************
**    E_FUNCTION     : S_match_after_star (pattern, text)
**        check if a text is match with wildcard string
**    PARAMETERS
**       INPUT  :
**          text     - text string to be checked
**          pattern     - wildcard string to be compared
**       OUTPUT : none
**    RETURNS      : 
**          match     - 1: matched
**						0: not matched
**						-1: abort
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_match_after_star (pattern, text)
char *pattern, *text;
{
	int match = 0;
	char nextp;

	while (*pattern=='*')
	{
		pattern++;
	}
	if (*pattern=='\0')
		return 1;

	nextp = *pattern;
	do
	{
		if (nextp == *text)
			match = S_ncl_match(pattern, text);
		if (*text==0)
			match = -1;
		text++;
	} while ( match != 1 && match != -1 );
	if (match==-1) match = 0;
	return match;
}
