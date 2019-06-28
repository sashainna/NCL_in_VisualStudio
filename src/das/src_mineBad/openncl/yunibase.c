/*********************************************************************
**    NAME         :  yunibase.c
**       CONTAINS:
**          NclxMdlUnibaseInit()
**          NclxMdlUnibaseClose()
**          NclxMdlUnibaseSave()
**          NclxMdlLoadSecondary()
**          NclxMdlMatchLabels()
**          NclxMdlLabelEntity()
**          NclxMdlExtractLabel()
**          NclxMdlUpdateUnistat()
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       yunibase.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:01
*********************************************************************/
#include "usysdef.h"
#include <string.h>
#include "mattr.h"
#include "mdattr.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "msrf.h"
#include "nccs.h"
#include "nclx.h"
#include "ulist.h"
#include "unserve.h"
#include "ribase.h"
#include "xfsys0.h"
#include "nclxunib.h"
#include "nclmodals.h"
#include "class.h"
#include "riddle.h"
/*
#define TIGMAIN
#include "tigglobal.h"
#undef TIGMAIN
*/
void NclxMdlLabelEntity();
void NclxMdlExtractLabel();

extern int UIG_from_sw;
extern int UIG_unmatch_sec;
extern int UIG_start_unmatch;
extern int UIG_regressive;
extern UU_KEY_ID *tig_unlabeled_keys;
extern int tig_max_cvlab;
extern int tig_max_sflab;
extern UU_REAL *UIG_regfactor;
extern UU_KEY_ID *UIG_matchkeys;
extern UU_KEY_ID *UIG_checkkeys;
extern UU_REAL UIG_match_tol;
extern int tig_unlabeled;
extern UU_LIST UIG_sflist_keys;
extern int UIG_regcount;

int curPtr=-1;
UU_LOGICAL NCLX_external_unibase=UU_FALSE;
/*
.....defined in tigglobal
UU_REAL UIG_match_tol;
int tig_unlabeled;
UU_KEY_ID *tig_unlabeled_keys = NULL;
UU_LIST UIG_sflist_keys;
int tig_max_cvlab =0 ;
int tig_max_sflab = 0 ;

int UIG_regcount;
UU_REAL *UIG_regfactor = NULL;
UU_KEY_ID *UIG_matchkeys = NULL;
UU_KEY_ID *UIG_checkkeys = NULL;
*/
/*********************************************************************
**    E_FUNCTION     : NclxMdlUnibaseInit()
**       Initializes the Unibase for the OpenNCL Geometry Library.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void NclxMdlUnibaseInit()
{
/*
.....Initialize routine
*/
	NCLX_external_unibase = UU_TRUE;
	curPtr = -1;
/*
....Initialize number server
*/
	uu_nserv_init(0,14,UU_XFORM_NM);
	uu_nserv_init(0,MAXT-1,UU_OT_NM);
/*
.....Initialize Unibase
*/
	ur_init_unibase();
	um_init_labels();
	uc_uni_init_all();
	ncl_uni_init_all();
	znu_init_machinetype();
/*	ua_init_drafting();*/
	ur_init_unibase_stat(UR_STAT_INIT);
/*
.....Initialize attributes
*/
	ncl_init_color();
	ur_put_attrmdl_color(UM_YELLOW);
	ur_put_attrmdl_layer(1);
	ur_put_attrmdl_pen(1);
	ur_put_attrmdl_line_style(1.0);
	ur_put_attrmdl_line_width(0.);
	ur_put_attrmdl_displayable(UM_DISPLAYABLE);
	ur_put_attrmdl_selectable(UU_TRUE);
	ur_put_attrmdl_label_on(UU_FALSE);
/*
.....Save the active Unibase structures
*/
	ur_saveu_active(1);
/*
.....Initialize curve/surface display attributes
*/
	UM_crvattr.relcirerr = .01;
	UM_crvattr.maxpts = 400;

	UM_srfattr.numupaths = 5;
	UM_srfattr.numvpaths = 5;
	UM_srfattr.ptsperucrv = 0.;
	UM_srfattr.ptspervcrv = 0.;
	UM_srfattr.maxpaths = 200;
	UM_srfattr.edge_disp = UU_FALSE;
	UM_srfattr.edge_color = 8;
	UM_srfattr.shaded = UU_TRUE;
	UM_srfattr.lucency = 100;
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlUnibaseClose()
**       Shuts down the currently open Unibase for the OpenNCL Geometry
**       Library.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void NclxMdlUnibaseClose()
{
/*
.....Reset the Unibase
*/
/*	ur_reset_unibase();*/
/*
....Terminate number server
*/
/*	uu_nserv_term(0,14,UU_XFORM_NM);
	uu_nserv_term(0,MAXT-1,UU_OT_NM);*/
/*
....Free all memory
*/
	uu_free_all();
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlUnibaseSave(file)
**       Saves the internal Unibase to an external file for the OpenNCL
**       Geometry Library.
**    PARAMETERS
**       INPUT  :
**          file              Filename to save.
**       OUTPUT : none
**    RETURNS      :
**       NCLX_SUCCESS if successful.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMdlUnibaseSave(file)
char *file;
{
	int status;
	status = ur_sp02(file);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlLoadSecondary(file)
**       Loads an external Unibase for label matching in the OpenNCL
**       Geometry Library.
**    PARAMETERS
**       INPUT  :
**          file              Filename to load.
**       OUTPUT : none
**    RETURNS      :
**       NCLX_SUCCESS if successful.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMdlLoadSecondary(file)
char *file;
{
	int status;
	char uname[1024];
/*
.....Quote filename
*/
	uname[0] = '"';
	strcpy(&uname[1],file);
	strcat(uname,"\"");
/*
.....Set the secondary Unibase as active
*/
	ur_getu_second();
/*
.....Initialize Secondary Unibase
*/
	ur_init_unibase();
//	uc_init_relations();
	uc_uni_init_all();
	ncl_uni_init_all();
/*
.....Load the secondary Unibase
*/
//	ur_saveu_active(1);				/* save acitve unibase in work unibase */
//	ur_getu_second();					/* switch to secondary unibase */
//	status = ur_reset_unibase();  /* clear secondary unibase */
//	if (status == UU_SUCCESS)
	{
		status = ur_lp02(uname,0); /* load unibase into secondary unibase */
		if (status == UU_SUCCESS)
		{
//			ur_saveu_active(2);
//			ur_saveu_active(1);
			ur_getu_work();			/* move work unibase into active unibase */
		}
	}
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlMatchLabels(nsf,ncv,nsk,tol,nlevel,func,label,
**                                        inter,layers,colors,list)
**       Compares geometry in the main Unibase to a secondary Unibase and
**       creates mathing labels where applicable.
**    PARAMETERS
**       INPUT  :
**          nsf     = Number of trimmed surfaces to match.
**          ncv     = Number of B-spline curves to match.
**          nsk     = Number of sketch entities to match.
**          tol     = Tolerance to use for matching.
**          nlevel  = Matching level to perform.  0-4, where 0 = Exact
**                    Match only.
**          func    = User supplied routine that is used to report the
**                    level of matching currently being worked on and the
**                    number of matches found.
**          label   = Structure that defines surface and curve labels.
**          inter   = User defined function to determine if user interrupted
**                    the matching.
**          layers  = Layers to assign enities based on matching level.
**          colors  = Colors to assign enities based on matching level.
**          list    = User defined function for outputing data to a list or
**                    file.  Not used at this time.
**       OUTPUT : none
**    RETURNS      :
**       NCLX_SUCCESS if successful.
**    SIDE EFFECTS :
**       Only works with trimmed surfaces, B-spline curves, composite
**       curves, circles and lines at this time.
**    WARNINGS     : none
*********************************************************************/
int NclxMdlMatchLabels(nsf,ncv,nsk,tol,nlevel,func,label,inter,layers,colors,
	list)
int nsf,ncv;
double tol;
int nlevel;
void (*func)();
void (*inter)();
void (*list)();
NCLX_label_struc *label;
int layers[],colors[];
{
/*	struct NCL_trimsf_rec surf;*/
	struct NCL_fixed_databag eptr,check,ent;
/*
.....Changed due to memory size problems - Andrew 5/2/13
	struct NCL_nclattr_rec attr;
*/
	struct UC_attributedatabag attrbag;
	struct UM_attrdata_rec *attr;
	int next_tupleid,ilev,nmatch,status,i,j,type,tsub,unlabeled,stopfl,relinc;
	int k,n,ie,nent,rel;
	static int relnum[6]={NCL_TRIMSF_REL,UM_RBSPLCRV_REL,UM_COMPCRV_REL,
	   UM_CIRCLE_REL,UM_LINE_REL,UM_POINT_REL};
	char tprefix[64];
	UU_KEY_ID *keyp;
	UU_LIST key_list;
/*
.....Initialize routine
*/
	stopfl = UU_FALSE;
	ilev = 0;
	nmatch = 0;
	nent = nsf + ncv + nsk;
	UIG_match_tol = tol;
	tig_unlabeled = 0;
	uu_list_init(&key_list,sizeof(UU_KEY_ID),nent,1000);
	(*func)(ilev,nmatch);
	attr = (struct UM_attrdata_rec *)&attrbag;
/*
.....Retrieve entities from the Unibase
*/
	ie = 1; if (ncv > 0) ie = 5; if (nsk > 0) ie = 6;
	relinc = 0;
	for (j=0;j<ie;j++)
	{
		next_tupleid = 1;
		rel = relnum[j];
		while (ur_get_next_data_key(rel,&next_tupleid,&ent.key)>-1)
		{
			next_tupleid++;
			ncl_retrieve_data_fixed(&ent);
			if (ent.label[0] == '@') continue;
/*
.....Match with existing entities
*/
			switch (rel)
			{
/*
.....pass labflg parameter along with the entity pointer, to avoid memory 
.....problems
*/
			case NCL_TRIMSF_REL:
				status = uig_match_trimsrf(&ent,1);
				break;
			case UM_RBSPLCRV_REL:
				status = uig_match_rbsplcrv(&ent,1);
				break;
			case UM_COMPCRV_REL:
				status = uig_match_compcrv(&ent,1);
				break;
			case UM_CIRCLE_REL:
				status = uig_match_circle(&ent,1);
				break;
			case UM_LINE_REL:
				status = uig_match_line(&ent,1);
				break;
			case UM_POINT_REL:
				status = uig_match_point(&ent,1);
				break;
			default:
				status = UU_FAILURE;
				break;
			}
/*
.....Store entity with new label
.....If there was a match
*/
			if (status == UU_SUCCESS)
			{
				ur_update_data_fixed(&ent);
				nmatch++;
				(*func)(ilev,nmatch);
				NclxMdlExtractLabel(ent.label,ent.subscr,&type,tprefix,&tsub);
				if (type == label[relinc].type &&
					strcmp(tprefix,label[relinc].prefix) == 0 &&
					tsub >= label[relinc].sub) label[relinc].sub = tsub + 1;
				attr->key = ent.key;
				ur_retrieve_attr(attr);
/*
.....if the color is set to default, set the color to that from the matching
.....unibase
*/
				if(colors[0])attr->color = colors[0];
				attr->layer = layers[0];
				ur_update_attr(attr);
			}
			else
			{
				uu_list_push(&key_list,&ent.key);
				tig_unlabeled++;
			}
			(*inter)(&stopfl);
			if (stopfl) break;
		}
		relinc = 1;
	}
/*
.....Perform remaining levels of matching
*/
	unlabeled = tig_unlabeled;
	tig_unlabeled_keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
	if (unlabeled && !stopfl && nlevel > 0)
	{
		uu_list_init(&UIG_sflist_keys,sizeof(UU_KEY_ID),unlabeled,unlabeled);
		for (ilev=1;ilev<=nlevel;ilev++)
		{
			(*func)(ilev,nmatch);
			if(UIG_regressive) status = uig_unused_sec();
			for (j=0;j<tig_unlabeled;j++)
			{
				if (tig_unlabeled_keys[j] != 0)
				{
					ent.key = tig_unlabeled_keys[j];
					ncl_retrieve_data_fixed(&ent);
					relinc = 1;
					rel = ent.rel_num;
					switch (rel)
					{
					case NCL_TRIMSF_REL:
						status = uig_comp_trimsrf(&ent,ilev);
						relinc = 0;
						break;
					case UM_RBSPLCRV_REL:
						status = uig_comp_rbsplcrv(&ent,ilev);
						break;
					case UM_COMPCRV_REL:
						status = uig_comp_compcrv(&ent,ilev);
						break;
					case UM_CIRCLE_REL:
						status = uig_comp_circle(&ent,ilev);
						break;
					case UM_LINE_REL:
						status = uig_comp_line(&ent,ilev);
						break;
					default:
						status = UU_FAILURE;
						break;
					}
					if (!UIG_regressive && status == UU_SUCCESS)
					{
						ur_update_data_fixed(&ent);
						nmatch++;
						unlabeled--;
						(*func)(ilev,nmatch);
						NclxMdlExtractLabel(ent.label,ent.subscr,&type,tprefix,
							&tsub);
						if (type == label[relinc].type &&
							strcmp(tprefix,label[relinc].prefix) == 0 &&
							tsub >= label[relinc].sub) label[relinc].sub = tsub + 1;
						attr->key = ent.key;
						ur_retrieve_attr(attr);
/*
.....if the color is set to default, set the color to that from the matching
.....unibase
*/
						if(colors[ilev])attr->color = colors[ilev];
						attr->layer = layers[ilev];
						ur_update_attr(attr);
						tig_unlabeled_keys[j] = 0;
					}
				}
			}
/*
.....Regressive matching,
.....for all unlabeled ent ,check if its key is in matchkeys,
.....delete the key from tig_unlabeled_keys decrease unlabeled by 1
.....decrease corresponding tig_unlabeled_ent by 1 
.....copy label of check to label of match
.....for all keys in checkkeys with matchkey not 0 make label of check key @UN
*/
			if(UIG_regressive)
			{	
				for (k = 0; k<tig_unlabeled; k++)
				{
					eptr.key = tig_unlabeled_keys[k];
					if (eptr.key !=0)
					{
						ncl_retrieve_data_fixed(&eptr);
						for (j = 0; j<UIG_regcount; j++)
						{
							if(UIG_matchkeys[j] == eptr.key)
							{
								ur_getu_second();
								check.key = UIG_checkkeys[j];
								ncl_retrieve_data_fixed(&check);						
								NclxMdlExtractLabel(check.label,
								check.subscr,&type,tprefix,&tsub);
								if (type == label[relinc].type &&
								strcmp(tprefix,label[relinc].prefix) == 0
								&& tsub >= label[relinc].sub) 
									label[relinc].sub = tsub + 1;
								strcpy(eptr.label,check.label);
								eptr.subscr = check.subscr;
/*
.....We have used this ent so mark it with
.....a label of @UN.
*/
								strcpy(check.label,"@UN");
								check.subscr = 0;
								ur_update_data_fixed(&check);
								nmatch++;
								(*func)(ilev,nmatch);
								ur_getu_work();
/*
.....put 0 for the key in the tig_unlabeled_keys
.....array decrease nlab by one and update color and layer
.....if requested.
*/
								ur_update_data_fixed(&eptr);
								attr->key = eptr.key;
								ur_retrieve_attr(attr);
/*
.....if the color is set to default, set the color to that from the matching
.....unibase
*/
								if(colors[ilev])attr->color = colors[ilev];
								attr->layer = layers[ilev];
								ur_update_attr(attr);
								tig_unlabeled_keys[k] = 0;
								unlabeled--;
								break;
							}
						}
					}
				}
/*
.....empty lists
*/
				UIG_regcount = 0 ;
				if (UIG_checkkeys)
				{
					uu_free(UIG_checkkeys);
					UIG_checkkeys = NULL;
				} 
				if (UIG_matchkeys)
				{
					uu_free(UIG_matchkeys);
					UIG_matchkeys = NULL;
				} 
				if (UIG_regfactor)
				{
					uu_free(UIG_regfactor);
					UIG_regfactor = NULL;
				} 
			}
			if (unlabeled == 0) break;
			(*inter)(&stopfl);
			if (stopfl) break;
		}
/*
........Free surface data
*/
		keyp = (UU_KEY_ID *)UU_LIST_ARRAY(&UIG_sflist_keys);
		if (keyp)
		{
			n = UU_LIST_LENGTH(&UIG_sflist_keys);
			for (i=0;i<n;i++)
			{
				ncl_lst_delete (BOX_LIST, &keyp[i]);
				ncl_lst_delete (WHOLE_BOUNDARY_LIST, &keyp[i]);
			}
			uu_list_free (&UIG_sflist_keys);
		}
	}
/*
.....Create unmatched entities from secondary unibase
*/
	if(UIG_unmatch_sec)
	{
		UIG_from_sw =1;
		uig_create_sec_unmatch();
	
		UIG_from_sw =0;
	}
/*
.....updates the highest label number for each of the standard labels so that
.....unmatched entities will not accidentally be given the same name as an 
.....unused entity if the unmatched entitites are to be labeled from the next
..... higest matched entity.
*/
	uig_secondary_unmatched();
	if(label[0].sub < tig_max_sflab +1) label[0].sub = tig_max_sflab +1;
	if(label[1].sub < tig_max_cvlab +1) label[1].sub = tig_max_cvlab;
/*
.....Label remaining entities
*/
	if (unlabeled && !stopfl)
	{
		(*func)(-1,nmatch);
		for (i=0;i<tig_unlabeled;i++)
		{
			ent.key = tig_unlabeled_keys[i];
			if (ent.key != 0)
			{
				ncl_retrieve_data_fixed(&ent);
				if (ent.rel_num == relnum[0]) relinc = 0;
				else relinc = 1;
				NclxMdlLabelEntity(ent.label,&(label[relinc].sub),
					label[relinc].type, label[relinc].prefix);
				if (label[relinc].type == 0)
					ent.subscr = 0;
				else
					ent.subscr = label[relinc].sub;
				label[relinc].sub++;
				ur_update_data_fixed(&ent);
				attr->key = ent.key;
				ur_retrieve_attr(attr);
/*
.....if the color is set to default, set the color to that from the matching
.....unibase
*/
				if(colors[5])attr->color = colors[5];
				attr->layer = layers[5];
				ur_update_attr(attr);
				nmatch++;
				(*func)(-1,nmatch);
			}
		}
/*
.....Free matching keys
*/
		uu_list_free(&key_list);
		tig_unlabeled_keys = NULL;
	}
/*
.....End of routine
*/
	if (stopfl) return(NCLX_FAILURE);
	else return(NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlLabelEntity(label,sub,type,prefix)
**
**       Constructs a label based on the labelling parameters.
**    PARAMETERS
**       INPUT  :
**          sub     = Current subscript number.
**          type    = 0 = Prefix labeling.  1 = Subscript labeling.
**          prefix  = Label prefix.
**       OUTPUT :
**          label   = Prepared geometry label.
**          sub     = Updated subscript number.
**          prefix  = Updated label prefix.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void NclxMdlLabelEntity(label,sub,type,prefix)
int *sub,type;
char *label,*prefix;
{
/*
.....Prefix labeling
*/
	if (type == 0)
	{
		if (*sub > 9999)
		{
			strcat(prefix,"A");
			*sub = 1;
		}
		sprintf(label,"%s%d",prefix,*sub);
	}
/*
.....Subscript labeling
*/
	else
	{
		if (*sub > 32767)
		{
			strcat(prefix,"A");
			*sub = 1;
		}
		strcpy(label,prefix);
	}
}

/*********************************************************************
**    E_FUNCTION     : NclxMdlExtractLabel(labin,subin,type,prefix,sub)
**
**       Parses a label and returns the label prefix, type, and subscript.
**    PARAMETERS
**       INPUT  :
**          labin   = Label string.
**          subin   = Subscript value.
**       OUTPUT :
**          type    = 0 = Prefixed label (SF10), 1 = Subscripted label
**                    (SFX(10)), 2 = Unknown label type (SFX).
**          prefix  = Label prefix.
**          sub     = Label subscript.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void NclxMdlExtractLabel(labin,subin,type,prefix,sub)
char *labin,*prefix;
int subin,*sub,*type;
{
	int i,nc;
	char *p;
/*
.....Initialize routine
*/
	strcpy(prefix,labin);
	*sub = subin;
	*type = 2;
/*
.....Subscripted entity
*/
	if (subin != 0)
	{
		*type = 1;
	}
/*
.....Prefixed entity
*/
	else
	{
		p = strchr(prefix,' ');
		if (p != UU_NULL) *p = '\0';
		nc = strlen(prefix);
		for (i=nc-1;i>=0;i--)
		{
			if (!isdigit(prefix[i]))
			{
				if (i == nc-1) break;
				sscanf(&prefix[i+1],"%d",sub);
				prefix[i+1] = '\0';
				*type = 0;
				break;
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION :  NclxMdlUpdateUnistat(unistat)
**       Stores the Unibase Statistics record in the Unibase.
**    PARAMETERS
**       INPUT  :
**          unistat - Unibase Statistics record.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMdlUpdateUnistat(unistat)
struct UR_unistat_rec *unistat;
{
	int status;
/*
.....Store fixed portion of record
*/
	if (unistat->key == 0) return(UU_FAILURE);
	status = ur_update_data(unistat);
/*
.....Update notes if any
*/
/*
	if (unistat->no_notes != 0)
	{
		status = ur_update_data_varlist(unistat->key,1,unistat->notes,1,
			unistat->no_notes);
		uu_free(unistat->notes);
	}
*/
/*
.....End of routine
*/
	return(status);
}
