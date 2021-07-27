/*********************************************************************
**    NAME         :  neskylst.c
**       CONTAINS:  Routines for key lists
**
**        netsky
**        delsky
**        addsky
**        gtsky
**        delskg
**        addskg
**        gtskg
**        delskx
**        addskx
**        gtskx
**        delskw
**        addskw
**        gtnskw
**        gtskw
**        addskj
**        getskj
**        ncl_free_keylist
**        ncl_addsky
**        ncl_add_listkey1
**        ncl_getnum_listkey
**        ncl_get_listkeys
**        ncl_get_listkey
**        delskf
**			addskf
**			gtskf
**			copyskft
**			copyskftm
**			isin_sklst
**			andskm_skfc
**        delsclp
**        delsflt
**        copyskf
**        copysflt
**        copyskfc
**        orskf_sclp
**        ncl_add_geolst
**        ncl_add_clrlst
**        ncl_add_laylst
**        ncl_add_mkrlst
**        ncl_add_clplst
**        ncl_get_clplst
**        ncl_add_lablst 
**        ncl_chk_layf
**        ncl_filter_keep
**        ncl_delete_filtlsts
**
**    COPYRIGHT 2004 (c) Numerical Control Computer Sciences Inc.
**    All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       neskylst.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       08/17/15 , 17:52:10
*********************************************************************/
#include "nclfc.h"
#include "nccs.h"
#include "ncl.h"
#include "nconst.h"
#include "mdrel.h"

static UU_LIST skylst,skxlst,skwlst,skglst,skmlst;
static UU_LIST skflst, skfclst;
static UU_LOGICAL skylst_init = UU_FALSE;
static UU_LOGICAL skxlst_init = UU_FALSE;
static UU_LOGICAL skwlst_init = UU_FALSE;
static UU_LOGICAL skglst_init = UU_FALSE;
static UU_LOGICAL skmlst_init = UU_FALSE;
static UU_LOGICAL skflst_init = UU_FALSE;
static UU_LOGICAL skfclst_init = UU_FALSE;

static UU_LIST geolst,clrlst,laylst,lablst,mkrlst,clplst;
static UU_LIST sfltlst,sclplst;
static UU_LOGICAL geolst_init = UU_FALSE;
static UU_LOGICAL clrlst_init = UU_FALSE;
static UU_LOGICAL laylst_init = UU_FALSE;
static UU_LOGICAL lablst_init = UU_FALSE;
static UU_LOGICAL mkrlst_init = UU_FALSE;
static UU_LOGICAL clplst_init = UU_FALSE;
static UU_LOGICAL sfltlst_init = UU_FALSE;
static UU_LOGICAL sclplst_init = UU_FALSE;

static UU_LOGICAL Snonet = UU_TRUE;

#define LIST_Y 0
#define LIST_X 1
#define LIST_W 2
#define LIST_G 3

struct UG_geoinfo
{
  UU_REAL geonam;
  UM_int2 gtyp;
};

/*********************************************************************
**    E_FUNCTION     : netsky (flag)
**       Sets the flag determining if NET surfaces will be broken up
**       and added as single surfaces to the surface list.
**    PARAMETERS
**       INPUT  :
**          flag      1 = Break up net surfaces into individual
**                    surfaces.  0 = Add net surface key to the
**                    surface list.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void netsky(flag)
UM_int2 *flag;
{
	Snonet = UU_TRUE;
	if (*flag == 0) Snonet = UU_FALSE;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_free_keylist(ilst)
**       Free the appropriate key list
**          ilst       0 - skylst, 1 - skxlst, 2 - skwlst, 3 - skylst
*********************************************************************/
void ncl_free_keylist(ilst)
int ilst;
{
	if (ilst == LIST_Y && skylst_init)
	{
		skylst_init = UU_FALSE;
		uu_list_free(&skylst);
	}
	else if (ilst == LIST_X && skxlst_init)
	{
		skxlst_init = UU_FALSE;
		uu_list_free(&skxlst);
	}
	else if (ilst == LIST_W && skwlst_init)
	{
		skwlst_init = UU_FALSE;
		uu_list_free(&skwlst);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void delsky()
**       Free the list skylst.
*********************************************************************/
void delsky()
{
	ncl_free_keylist(LIST_Y);
	return;
}
/*********************************************************************
**    E_FUNCTION     : void delsky()
**       Free the list skylst.
*********************************************************************/
void delskg()
{
	if (skglst_init)
	{
		skglst_init = UU_FALSE;
		uu_list_free(&skglst);
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void delskm()
**       Free the list skmlst.
*********************************************************************/
void delskm()
{
	if (skmlst_init)
	{
		skmlst_init = UU_FALSE;
		uu_list_free(&skmlst);
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void delskf()
**       Free the list skmlst.
*********************************************************************/
void delskf()
{
	if (skflst_init)
	{
		skflst_init = UU_FALSE;
		uu_list_free(&skflst);
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void delskfc()
**       Free the list skfclst.
*********************************************************************/
void delskfc()
{
	if (skfclst_init)
	{
		skfclst_init = UU_FALSE;
		uu_list_free(&skfclst);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void delskx()
**       Free the list skxlst.
*********************************************************************/
void delskx()
{
	ncl_free_keylist(LIST_X);
	return;
}

/*********************************************************************
**    E_FUNCTION     : void delskw()
**       Free the list skxlst.
*********************************************************************/
void delskw()
{
	ncl_free_keylist(LIST_W);
	return;
}

/*********************************************************************
**    E_FUNCTION     : void delsclp()
**       Free the list sclplst.
*********************************************************************/
void delsclp()
{
	if (sclplst_init)
	{
		sclplst_init = UU_FALSE;
		uu_list_free(&sclplst);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void delsflt()
**       Free the list sfltlst.
*********************************************************************/
void delsflt()
{
	if (sfltlst_init)
	{
		sfltlst_init = UU_FALSE;
		uu_list_free(&sfltlst);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_add_listkey1 (ilst,skey)
**       Add a key to the list if not already there.
**    PARAMETERS
**       INPUT  :
**          ilst              0 - skylst, 1 - skxlst, 2 - skwlst, 3 - skylst
**          skey              surface key number
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_add_listkey1 (ilst,skey)
int ilst;
UU_KEY_ID skey;
{
	UU_LIST *lst;
	UU_LOGICAL *lsinit;
	int i,status;
	UU_KEY_ID *skeys;

	if (ilst == LIST_Y)
	{
		lst = &skylst;
		lsinit = &skylst_init;
	}
	else if (ilst == LIST_X)
	{
		lst = &skxlst;
		lsinit = &skxlst_init;
	}
	else if (ilst == LIST_W)
	{
		lst = &skwlst;
		lsinit = &skwlst_init;
	}
	else if (ilst == LIST_G)
	{
		lst = &skglst;
		lsinit = &skglst_init;
	}
	else
	{
		return;
	}

	if (*lsinit == UU_FALSE)
	{
		status = uu_list_init1 (lst, sizeof(UU_KEY_ID),100,100);
		*lsinit = (status == UU_SUCCESS);
	}
	else
	{
		skeys = (UU_KEY_ID *) UU_LIST_ARRAY(lst);
		for (i = 0; i < lst->cur_cnt; i++)
		{
			if (skey == skeys[i]) return;
		}
	}
	if (*lsinit) uu_list_push(lst,&skey);

	return;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_add_listkey(ilst,skey)
**       Add key(s) to the list if not already there.
**    PARAMETERS
**       INPUT  :
**          ilst              0 - skylst, 1 - skxlst, 2 - skwlst, 3 - skylst
**          skey              surface key number
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_add_listkey(ilst,skey)
int ilst;
UU_KEY_ID skey;
{
	int rel;

	if (ilst != LIST_Y && ilst != LIST_X && ilst != LIST_W && ilst != LIST_G)
		return;
	if (ur_retrieve_data_relnum (skey, &rel) != UU_SUCCESS)
		return;
	if (rel == NCL_NETSF_REL && Snonet)
	{
		struct NCL_fixed_databag srf;
		struct NCL_netsf_rec *tsf;
		int i,status;

		srf.key = skey;
		status = ncl_retrieve_data_fixed (&srf);
		if (status != UU_SUCCESS) return;
		tsf = (struct NCL_netsf_rec *)&srf;
		for (i = 0; i < tsf->no_netkey; i++)
			ncl_add_listkey1(ilst,tsf->netkey[i]);
	}
	else
		ncl_add_listkey1(ilst,skey);

	return;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_addsky(ilst,ityp,knum,intype)
**       Add key(s) to the list if not already there.
**    PARAMETERS
**       INPUT  :
**          ilst              0 - skylst, 1 - skxlst, 2 - skwlst, 3 - skylst
**          ityp              0 - Entity key, 1 - layer number,
**                            2 - Push a NULL key onto list,
**                            3 - All surfaces
**          knum              Entity key or layer number
**          intype            0 - Add all wireframe geometry to list,
**                            1 = Add surfaces, 2 = Solids.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_addsky(ilst,ityp,knum,intype)
int ilst,ityp,intype,knum;
{
	int layer,dum,relnum;
	int ietype,mytype;
	UU_KEY_ID skey;

/*
.....Initialize routine
*/
	mytype = NCLI_SURF;
	if (intype == 2) mytype = NCLI_SOLID;
	else if (intype == 0) mytype = 0;
/*
.....Add by layer or all defined entities
*/
	if (ityp == 1 || ityp == 3)
	{
		vxlfst();
		while (UU_TRUE)
		{
			if (!ncl_vxlnxt (&skey,&ietype)) break;
			else if (ietype == NCLI_MPARM || ietype == NCLI_LABEL || skey == 0) continue;

			if (mytype == 0)
			{
				ur_retrieve_data_relnum(skey,&relnum);
				if (ncl_label_type(relnum) != UU_SUCCESS) continue;
			}
			else
			{
				if (ietype != mytype) continue;
			}

			if (ityp == 1)
			{
				um_get_attrib (&skey,&dum,&dum,&dum,&dum,&layer);
				if (layer != knum) continue;
			}
			ncl_add_listkey (ilst,skey);
		}
	}
/*
.....Push a null key onto stack
*/
	else if (ityp == 2)
	{
		skey = NULLKEY;
		ncl_add_listkey1 (ilst,skey);
	}
/*
.....Push single entity key onto stack
*/
	else
	{
		skey = knum;
		ncl_add_listkey (ilst,skey);
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     : void addsky(ityp,knum,numsf)
**       Add key(s) to the list if not already there.
**    PARAMETERS
**       INPUT  :
**          ityp              0 - Entity key, 1 - layer number,
**                            2 - Push a NULL key onto list,
**                            3 - All surfaces
**          knum              Entity key or layer number
**          intype            0 - Add all wireframe geometry to list,
**                            1 = Add surfaces, 2 = Solids.
**       OUTPUT :
**          numsf             current number of surface keys
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void addsky(ityp2,knum4,intype2,numsf2)
UM_int2 *ityp2,*numsf2,*intype2;
UM_int4 *knum4;
{
	int ityp,intype,knum;

	ityp = *ityp2;
	intype = *intype2;
	knum = *knum4;
	ncl_addsky (LIST_Y,ityp,knum,intype);

	if (skylst_init)
		*numsf2 = skylst.cur_cnt;
	else
		*numsf2 = 0;

	return;
}
/*********************************************************************
**    E_FUNCTION     : void addskx(ityp,knum,numsf)
**       Add key(s) to the list if not already there.
**    PARAMETERS
**       INPUT  :
**          ityp              0 - Entity key, 1 - layer number,
**                            2 - Push a NULL key onto list,
**                            3 - All surfaces
**          knum              Entity key or layer number
**          intype            1 = Add surfaces to list, 2 = Solids.
**       OUTPUT :
**          numsf             current number of surface keys
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void addskx(ityp2,knum4,intype2,numsf2)
UM_int2 *ityp2,*numsf2,*intype2;
UM_int4 *knum4;
{
	int ityp,intype,knum;

	ityp = *ityp2;
	intype = *intype2;
	knum = *knum4;
	ncl_addsky (LIST_X,ityp,knum,intype);

	if (skxlst_init)
		*numsf2 = skxlst.cur_cnt;
	else
		*numsf2 = 0;

	return;
}

/*********************************************************************
**    E_FUNCTION     : void addskw(ityp,knum,numsf)
**       Add key(s) to the list if not already there.
**    PARAMETERS
**       INPUT  :
**          ityp              0 - Entity key, 1 - layer number,
**                            2 - Push a NULL key onto list,
**                            3 - All surfaces
**          knum              Entity key or layer number
**          intype            1 = Add surfaces to list, 2 = Solids.
**       OUTPUT :
**          numsf             current number of surface keys
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void addskw(ityp2,knum4,intype2,numsf2)
UM_int2 *ityp2,*numsf2,*intype2;
UM_int4 *knum4;
{
	int ityp,intype,knum;

	ityp = *ityp2;
	intype = *intype2;
	knum = *knum4;
	ncl_addsky (LIST_W,ityp,knum,intype);

	if (skwlst_init)
		*numsf2 = skwlst.cur_cnt;
	else
		*numsf2 = 0;

	return;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_getnum_listkey (ilst,num)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          ilst              0 - skylst, 1 - skxlst, 2 - skwlst, 3 - skylst
**          inum              the element's number in the list
**       OUTPUT :
**          jnum              the element (positive integer)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getnum_listkey (ilst,num)
int ilst,*num;
{
	*num = 0;
	if (ilst == LIST_Y && skylst_init)
	{
		*num = skylst.cur_cnt;
	}
	else if (ilst == LIST_X && skxlst_init)
	{
		*num = skxlst.cur_cnt;
	}
	else if (ilst == LIST_W && skwlst_init)
	{
		*num = skwlst.cur_cnt;
	}
	else if (ilst == LIST_G && skglst_init)
	{
		*num = skglst.cur_cnt;
	}

	return;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_get_listkeys (ilst,keys)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          ilst              0 - skylst, 1 - skxlst, 2 - skwlst, 3 - skylst
**          inum              the element's number in the list
**       OUTPUT :
**          jnum              the element (positive integer)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_listkeys (ilst,keys)
int ilst;
UU_KEY_ID **keys;
{
	*keys = UU_NULL;

	if (ilst == LIST_Y && skylst_init)
	{
		*keys = (UU_KEY_ID *) UU_LIST_ARRAY(&skylst);
	}
	else if (ilst == LIST_X && skxlst_init)
	{
		*keys = (UU_KEY_ID *) UU_LIST_ARRAY(&skxlst);
	}
	else if (ilst == LIST_W && skwlst_init)
	{
		*keys = (UU_KEY_ID *) UU_LIST_ARRAY(&skwlst);
	}
	else if (ilst == LIST_G && skglst_init)
	{
		*keys = (UU_KEY_ID *) UU_LIST_ARRAY(&skglst);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void ncl_get_listkey (ilst,i,keyi)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          ilst              0 - skylst, 1 - skxlst, 2 - skwlst, 3 - skylst
**          inum              the element's number in the list
**       OUTPUT :
**          keyi              the element (positive integer), Returns -1
**                            if the element does not exist.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_listkey (ilst,i,keyi)
int ilst,i;
UU_KEY_ID *keyi;
{
	UU_KEY_ID *js;
	int status = UU_FAILURE;

	*keyi = -1;
	if (ilst == LIST_Y && skylst_init)
	{
		js = (UU_KEY_ID *) UU_LIST_ARRAY(&skylst);
		if (i >= 0 && i < skylst.cur_cnt)
		{
			*keyi = js[i];
			status = UU_SUCCESS;
		}
	}
	else if (ilst == LIST_X && skxlst_init)
	{
		js = (UU_KEY_ID *) UU_LIST_ARRAY(&skxlst);
		if (i >= 0 && i < skxlst.cur_cnt)
		{
			*keyi = js[i];
			status = UU_SUCCESS;
		}
	}
 	else if (ilst == LIST_W && skwlst_init)
	{
		js = (UU_KEY_ID *) UU_LIST_ARRAY(&skwlst);
		if (i >= 0 && i < skwlst.cur_cnt)
		{
			*keyi = js[i];
			status = UU_SUCCESS;
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : void gtsky(inum,skey)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          inum              the element's number in the list
**       OUTPUT :
**          jnum              the element (positive integer)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtsky(inum,skey)
UM_int2 *inum;
UM_int4 *skey;
{
	int i;
	UU_KEY_ID keyi;

	i = *inum - 1;
	ncl_get_listkey (LIST_Y,i,&keyi);
	*skey = keyi;

	return;
}
/*********************************************************************
**    E_FUNCTION     : void gtskx(inum,skey)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          inum              the element's number in the list
**       OUTPUT :
**          jnum              the element (positive integer)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtskx(inum,skey)
UM_int2 *inum;
UM_int4 *skey;
{
	int i;
	UU_KEY_ID keyi;

	i = *inum - 1;
	ncl_get_listkey (LIST_X,i,&keyi);
	*skey = keyi;
	return;
}

/*********************************************************************
**    E_FUNCTION     : void gtnskw(nkeys)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          nkeys             Number of keys stored in W-list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtnskw(nkeys)
UM_int4 *nkeys;
{
	int i;
	UU_KEY_ID keyi;

	*nkeys = 0;
	if (skwlst_init) *nkeys = UU_LIST_LENGTH(&skwlst);
	return;
}

/*********************************************************************
**    E_FUNCTION     : void gtskw(inum,skey)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          inum              the element's number in the list
**       OUTPUT :
**          jnum              the element (positive integer)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtskw(inum,skey)
UM_int2 *inum;
UM_int4 *skey;
{
	int i;
	UU_KEY_ID keyi;

	i = *inum - 1;
	ncl_get_listkey (LIST_W,i,&keyi);
	*skey = keyi;

	return;
}

/*********************************************************************
**    E_FUNCTION     : void addskj(knum,numsf)
**       Add a (positive integer) number to the list, if not already there
**    PARAMETERS
**       INPUT  :
**          knum              number to add
**       OUTPUT :
**          numsf             current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void addskj(knum,numsf)
UM_int2 *knum,*numsf;
{
	int i,jn,status;
	int *js;

	if (skylst_init)
		*numsf = skylst.cur_cnt;
	else
		*numsf = 0;

	jn = *knum;
	if (jn <= 0) return;

	if (!skylst_init)
	{
		status = uu_list_init1 (&skylst, sizeof(int),10,10);
		skylst_init = (status == UU_SUCCESS);
	}
	else
	{
		js = (int *) UU_LIST_ARRAY(&skylst);
		for (i = 0; i < skylst.cur_cnt; i++)
		{
			if (jn == js[i]) return;
		}
	}

	if (skylst_init) uu_list_push(&skylst,&jn);

	*numsf = skylst.cur_cnt;

	return;
}

/*********************************************************************
**    E_FUNCTION     : void getskj(inum,jnum)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          inum              the element's number in the list
**       OUTPUT :
**          jnum              the element (positive integer)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void getskj(inum,jnum)
UM_int2 *inum,*jnum;
{
	int i;
	int *js;

	*jnum = -1;

	if (!skylst_init) return;

	i = *inum - 1;
	if (i >= 0 && i < skylst.cur_cnt)
	{
		js = (int *) UU_LIST_ARRAY(&skylst);
		*jnum = js[i];
	}

	return;
}
/*********************************************************************
**    E_FUNCTION     : void addskg(geonam, gtyp, numsf)
**       Add a 'struct UG_geoinfo' to the list, if not already there
**    PARAMETERS
**       INPUT  :
**          item:           item to add
**       OUTPUT :
**          numsf           current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void addskg(geonam, gtyp, numsf)
UU_REAL *geonam;
UM_int2 *gtyp;
UM_int4 *numsf;
{
	int status;
	struct UG_geoinfo item;

	if (skglst_init)
		*numsf = skglst.cur_cnt;
	else
		*numsf = 0;

	item.geonam = *geonam;
	item.gtyp = *gtyp;
	if (!skglst_init)
	{
		status = uu_list_init1 (&skglst, sizeof(struct UG_geoinfo), 100, 100);
		skglst_init = (status == UU_SUCCESS);
	}
	if (skglst_init) 
	{
		uu_list_push(&skglst, &item);
		*numsf = skglst.cur_cnt;
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void gtskg((inum, geonam, gtyp)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          inum              the element's number in the list
**       OUTPUT :
**          geonam             
**          gtyp             
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtskg(inum, geonam, gtyp)
UM_int4 *inum;
UU_REAL *geonam;
UM_int2 *gtyp;
{
	int i;
	struct UG_geoinfo *item;

	i = *inum - 1;
 	if (skglst_init)
	{
		item = (struct UG_geoinfo *) UU_LIST_ARRAY(&skglst);
		if (i >= 0 && i < skglst.cur_cnt)
		{
			*gtyp = item[i].gtyp;
			*geonam = item[i].geonam;
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void chgskg(inum, geonam, gtyp)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          inum              the element's number in the list
**       OUTPUT :
**          geonam             
**          gtyp             
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void chgskg(inum, geonam, gtyp)
UM_int4 *inum;
UU_REAL *geonam;
UM_int2 *gtyp;
{
	int i;
	struct UG_geoinfo *item;

	i = *inum - 1;
 	if (skglst_init)
	{
		item = (struct UG_geoinfo *) UU_LIST_ARRAY(&skglst);
		if (i >= 0 && i < skglst.cur_cnt)
		{
			item[i].gtyp = *gtyp;
			item[i].geonam = *geonam;
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void addskm(geonam, numsf)
**       Add a UU_REAL to the list
**    PARAMETERS
**       INPUT  :
**          item:           item to add
**       OUTPUT :
**          numsf           current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void addskm(geonam,numsf)
UU_REAL *geonam;
UM_int4 *numsf;
{
	struct UG_geoinfo item;
	int status;

	if (skmlst_init)
		*numsf = skmlst.cur_cnt;
	else
		*numsf = 0;

	if (!skmlst_init)
	{
		status = uu_list_init1 (&skmlst, sizeof(UU_REAL), 100, 100);
		skmlst_init = (status == UU_SUCCESS);
	}
	if (skmlst_init) 
	{
		uu_list_push(&skmlst, geonam);
		*numsf = skmlst.cur_cnt;
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void gtskm((inum, geonam)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          inum              the element's number in the list
**       OUTPUT :
**          geonam             
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtskm(inum, geonam)
UM_int4 *inum;
UU_REAL *geonam;
{
	int i;
	UU_REAL *item;

	i = *inum - 1;
 	if (skmlst_init)
	{
		item = (UU_REAL *) UU_LIST_ARRAY(&skmlst);
		if (i >= 0 && i < skmlst.cur_cnt)
		{
			*geonam = item[i];
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void addskfc(geonam, numsf)
**       Add a UU_REAL to the list
**    PARAMETERS
**       INPUT  :
**          item:           item to add
**       OUTPUT :
**          numsf           current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void addskfc(geonam,numsf)
UU_REAL *geonam;
UM_int4 *numsf;
{
	struct UG_geoinfo item;
	int status;

	if (skfclst_init)
		*numsf = skfclst.cur_cnt;
	else
		*numsf = 0;

	if (!skfclst_init)
	{
		status = uu_list_init1 (&skfclst, sizeof(UU_REAL), 100, 100);
		skfclst_init = (status == UU_SUCCESS);
	}
	if (skfclst_init) 
	{
		uu_list_push(&skfclst, geonam);
		*numsf = skfclst.cur_cnt;
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void gtskfc((inum, geonam)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          inum              the element's number in the list
**       OUTPUT :
**          geonam             
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtskfc(inum, geonam)
UM_int4 *inum;
UU_REAL *geonam;
{
	int i;
	UU_REAL *item;

	i = *inum - 1;
 	if (skfclst_init)
	{
		item = (UU_REAL *) UU_LIST_ARRAY(&skfclst);
		if (i >= 0 && i < skfclst.cur_cnt)
		{
			*geonam = item[i];
		}
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void addskf(geonam, numsf)
**       Add a UU_REAL to the list
**    PARAMETERS
**       INPUT  :
**          item:           item to add
**       OUTPUT :
**          numsf           current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void addskf(geonam,numsf)
UU_REAL *geonam;
UM_int4 *numsf;
{
	struct UG_geoinfo item;
	int status;

	if (skflst_init)
		*numsf = skflst.cur_cnt;
	else
		*numsf = 0;

	if (!skflst_init)
	{
		status = uu_list_init1 (&skflst, sizeof(UU_REAL), 100, 100);
		skflst_init = (status == UU_SUCCESS);
	}
	if (skflst_init) 
	{
		uu_list_push(&skflst, geonam);
		*numsf = skflst.cur_cnt;
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void gtskf((inum, geonam)
**       Get the numbered element from the list.
**    PARAMETERS
**       INPUT  :
**          inum              the element's number in the list
**       OUTPUT :
**          geonam             
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtskf(inum, geonam)
UM_int4 *inum;
UU_REAL *geonam;
{
	int i;
	UU_REAL *item;

	i = *inum - 1;
 	if (skflst_init)
	{
		item = (UU_REAL *) UU_LIST_ARRAY(&skflst);
		if (i >= 0 && i < skflst.cur_cnt)
		{
			*geonam = item[i];
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void copyskft(numsf)
**       copy list skflst to skfclst
**    PARAMETERS
**       INPUT  :
**				numsf: total number to be copied
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void copyskft(numsf)
UM_int4 *numsf;
{
	int i, num, status;
	UU_REAL *item, geonam;

	if (skfclst_init)
	{
		uu_list_free(&skfclst);
		skfclst_init = UU_FALSE;
	}
	if (!skflst_init)
	{
		*numsf = 0;
		return;
	}
	status = uu_list_init1 (&skfclst, sizeof(UU_REAL), 100, 100);
	skfclst_init = (status == UU_SUCCESS);

	if (skfclst_init) 
	{
		for (i=0; i<*numsf; i++)
		{
			item = (UU_REAL *) UU_LIST_ARRAY(&skflst);
			if (item==NULL) break;
			if (i < skflst.cur_cnt)
			{
				geonam = item[i];
				uu_list_push(&skfclst, &geonam);
			}
		}
	}
	return;
}


/*********************************************************************
**    E_FUNCTION     : void copyskftm(numsf)
**       copy list skfclst to skmlst
**    PARAMETERS
**       INPUT  :
**				numsf: total number to be copied
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void copyskftm(numsf)
UM_int4 *numsf;
{
	int i, num, status;
	UU_REAL *item, geonam;

	if (skmlst_init)
	{
		uu_list_free(&skmlst);
		skmlst_init = UU_FALSE;
	}
	if (!skfclst_init)
	{
		*numsf = 0;
		return;
	}
	status = uu_list_init1 (&skmlst, sizeof(UU_REAL), 100, 100);
	skmlst_init = (status == UU_SUCCESS);

	if (skmlst_init) 
	{
		for (i=0; i<*numsf; i++)
		{
			item = (UU_REAL *) UU_LIST_ARRAY(&skfclst);
			if (item==NULL) break;
			if (i < skfclst.cur_cnt)
			{
				geonam = item[i];
				uu_list_push(&skmlst, &geonam);
			}
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void copyskf()
**       copy list skfclst to sfltlst
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void copyskf()
{
	int i, num, status;
	UU_REAL *item, geonam;

	if (sfltlst_init)
	{
		uu_list_free(&sfltlst);
		sfltlst_init = UU_FALSE;
	}
	if (!skfclst_init)
	{
		return;
	}
	status = uu_list_init1 (&sfltlst, sizeof(UU_REAL), 100, 100);
	sfltlst_init = (status == UU_SUCCESS);

	if (sfltlst_init) 
	{
		for (i=0; i<skfclst.cur_cnt; i++)
		{
			item = (UU_REAL *) UU_LIST_ARRAY(&skfclst);
			if (item==NULL) break;
			geonam = item[i];
			uu_list_push(&sfltlst, &geonam);
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void copysflt()
**       copy list sfltlst to skfclst
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void copysflt()
{
	int i, num, status;
	UU_REAL *item, geonam;

	if (skfclst_init)
	{
		uu_list_free(&skfclst);
		skfclst_init = UU_FALSE;
	}
	if (!sfltlst_init)
	{
		return;
	}
	status = uu_list_init1 (&skfclst, sizeof(UU_REAL), 100, 100);
	skfclst_init = (status == UU_SUCCESS);

	if (skfclst_init) 
	{
		for (i=0; i<sfltlst.cur_cnt; i++)
		{
			item = (UU_REAL *) UU_LIST_ARRAY(&sfltlst);
			if (item==NULL) break;
			geonam = item[i];
			uu_list_push(&skfclst, &geonam);
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : void copyskfc()
**       copy list sclplst to skfclst
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**          onum - number of entities in list
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void copyskfc(onum)
UM_int4 *onum;
{
	int i, num, status;
	UU_REAL *item, geonam;

	if (skfclst_init)
	{
		uu_list_free(&skfclst);
		skfclst_init = UU_FALSE;
	}
	if (!sclplst_init)
	{
		*onum = 0;
		return;
	}
	status = uu_list_init1 (&skfclst, sizeof(UU_REAL), 100, 100);
	skfclst_init = (status == UU_SUCCESS);

	if (skfclst_init) 
	{
		*onum = 0;
		for (i=0; i<sclplst.cur_cnt; i++)
		{
			item = (UU_REAL *) UU_LIST_ARRAY(&sclplst);
			if (item==NULL) break;
			geonam = item[i];
			uu_list_push(&skfclst, &geonam);
			(*onum)++;
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : isin_sklst(geonam, list, fnum)
**     check if geonam is in list
**    PARAMETERS
**       INPUT  :
**				geonam: item to be checked
**				list: list to be checked
**				fnum: total number in list
**       OUTPUT :
**				none
**    RETURNS      : 1: it's in
**					0: not in
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
isin_sklst(geonam, list, fnum)
UU_REAL geonam;
UU_LIST *list;
int fnum;
{
	int i;
	UM_int4 key1, key2;
	UM_int2 nwds, ietype;
	UM_real8 asw;
	UU_REAL *item;
	for (i=0; i<fnum; i++)
	{
		item = (UU_REAL *) UU_LIST_ARRAY(list);
		if (item==NULL) return 0;
		if (i < list->cur_cnt)
		{
			asw = geonam;
			gtdesc (&asw, &key1, &nwds, &ietype);
			asw = item[i];
			gtdesc (&asw, &key2, &nwds, &ietype);
			if (key1 == key2)
				return 1;
		}
	}
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : void andskm_skfc(geomnum, filtnum)
**     and skmlst / skfclst and put result list into skfclst
**    PARAMETERS
**       INPUT  :
**				geomnum: total number in skmlst
**				filtnum: total number in skfclst
**       OUTPUT :
**				filtnum: total number in skfclst
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void andskm_skfc(geomnum, filtnum)
UM_int4 *geomnum, *filtnum;
{
	int i, num, status;
	UU_REAL *item, geonam;
	UU_LIST tmplst;
	int fnum = *filtnum;

	if (!skmlst_init || !skfclst_init)
	{
		delskm();
		delskfc();
		*filtnum = 0;
		return;
	}
/*
......copy skfclst item into temp list first
*/
	status = uu_list_init1 (&tmplst, sizeof(UU_REAL), 100, 100);
	if (status != UU_SUCCESS)
	{
		*filtnum =  0;
		return;
	}
	for (i=0; i<*filtnum; i++)
	{
		item = (UU_REAL *) UU_LIST_ARRAY(&skfclst);
		if (item==NULL) break;
		if (i < skfclst.cur_cnt)
		{
			geonam = item[i];
			uu_list_push(&tmplst, &geonam);
		}
	}
/*
.....delete old skfclst and recreate a new one
*/
	if (skfclst_init)
	{
		uu_list_free(&skfclst);
	}
	status = uu_list_init1 (&skfclst, sizeof(UU_REAL), 100, 100);
	skfclst_init = (status == UU_SUCCESS);

	if (skfclst_init) 
	{
		*filtnum = 0;
		for (i=0; i<*geomnum; i++)
		{
			item = (UU_REAL *) UU_LIST_ARRAY(&skmlst);
			if (item==NULL) return;
			if (i < skmlst.cur_cnt)
			{
				geonam = item[i];
/*
......check if geonam is in the skflst, if it is in, added into skfclst
*/
				if (isin_sklst(geonam, &tmplst, fnum))
				{
					uu_list_push(&skfclst, &geonam);
					(*filtnum)++;
				}
			}
		}
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void orskf_sclp()
**     OR skfclst with sclplst and put result list into sclplst
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void orskf_sclp()
{
	int i, num, status;
	UU_REAL *item, geonam;
	UU_LIST tmplst;
	int fnum;

	if (!skfclst_init)
	{
		delsclp();
		delskfc();
		return;
	}
	if (!sclplst_init)
	{
		status = uu_list_init1 (&sclplst, sizeof(UU_REAL), 100, 100);
		sclplst_init = (status == UU_SUCCESS);
	}
	fnum = sclplst.cur_cnt;
	if (sclplst_init) 
	{
		for (i=0; i<skfclst.cur_cnt; i++)
		{
			item = (UU_REAL *) UU_LIST_ARRAY(&skfclst);
			if (item==NULL) return;
			geonam = item[i];
			if (isin_sklst(geonam, &sclplst, fnum))
				continue;
			else
				uu_list_push(&sclplst, &geonam);
		}
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void ncl_add_geolst(type, numgeo)
**       Add a geometry type to the list
**    PARAMETERS
**       INPUT  :
**          type   - ietype of geometry to add
**       OUTPUT :
**          numgeo - current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_add_geolst(type,numgeo)
UM_int2 *type,*numgeo;
{
	int status;

	if (geolst_init)
		*numgeo = geolst.cur_cnt;
	else
		*numgeo = 0;
	if (!geolst_init)
	{
		status = uu_list_init1 (&geolst, sizeof(UM_int2), 10, 10);
		geolst_init = (status == UU_SUCCESS);
	}
	if (geolst_init) 
	{
		uu_list_push(&geolst, type);
		*numgeo = geolst.cur_cnt;
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void ncl_add_clrlst(color, numclr)
**       Add a color type to the list
**    PARAMETERS
**       INPUT  :
**          color  - type of color to add
**       OUTPUT :
**          numclr - current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_add_clrlst(color,numclr)
UM_int2 *color,*numclr;
{
	int status;

	if (clrlst_init)
		*numclr = clrlst.cur_cnt;
	else
		*numclr = 0;
	if (!clrlst_init)
	{
		status = uu_list_init1 (&clrlst, sizeof(UM_int2), 10, 10);
		clrlst_init = (status == UU_SUCCESS);
	}
	if (clrlst_init) 
	{
		uu_list_push(&clrlst, color);
		*numclr = clrlst.cur_cnt;
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void ncl_add_laylst(layer, numlay)
**       Add a layer number to the list
**    PARAMETERS
**       INPUT  :
**          layer  - type of layer to add
**       OUTPUT :
**          numlay - current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_add_laylst(layer,numlay)
UM_int2 *layer,*numlay;
{
	int status;

	if (laylst_init)
		*numlay = laylst.cur_cnt;
	else
		*numlay = 0;
	if (!laylst_init)
	{
		status = uu_list_init1 (&laylst, sizeof(UM_int2), 10, 10);
		laylst_init = (status == UU_SUCCESS);
	}
	if (laylst_init) 
	{
		uu_list_push(&laylst, layer);
		*numlay = laylst.cur_cnt;
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void ncl_add_mkrlst(marker, nummkr)
**       Add a marker type to the list
**    PARAMETERS
**       INPUT  :
**          marker - type of marker to add
**       OUTPUT :
**          nummkr - current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_add_mkrlst(marker,nummkr)
UM_int2 *marker,*nummkr;
{
	int status;

	if (mkrlst_init)
		*nummkr = mkrlst.cur_cnt;
	else
		*nummkr = 0;
	if (!mkrlst_init)
	{
		status = uu_list_init1 (&mkrlst, sizeof(UM_int2), 10, 10);
		mkrlst_init = (status == UU_SUCCESS);
	}
	if (mkrlst_init) 
	{
		uu_list_push(&mkrlst, marker);
		*nummkr = mkrlst.cur_cnt;
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void ncl_add_clplst(index, numclp)
**       Add index for CLIPF call to the list
**    PARAMETERS
**       INPUT  :
**          index  - index for parsing where CLIPF call was made
**       OUTPUT :
**          numclp - current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_add_clplst(index,numclp)
UM_int2 *index,*numclp;
{
	int status;

	if (clplst_init)
		*numclp = clplst.cur_cnt;
	else
		*numclp = 0;
	if (!clplst_init)
	{
		status = uu_list_init1(&clplst, sizeof(int), 10, 10);
		clplst_init = (status == UU_SUCCESS);
	}
	if (clplst_init) 
	{
		uu_list_push(&clplst, index);
		*numclp = clplst.cur_cnt;
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void ncl_get_clplst(index, numclp)
**       Get inx frm CLIPF list at index
**    PARAMETERS
**       INPUT  :
**          index - index for parsing where CLIPF call was made
**       OUTPUT :
**          inx   - parser index for clipf statement
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_clplst(index,inx)
UM_int2 *index,*inx;
{
	int *lst;

	if (*index >= 0 && *index < clplst.cur_cnt) 
	{
		lst = (int *)UU_LIST_ARRAY(&clplst);
		*inx = lst[*index];
	}
	return;
}
/*********************************************************************
**    E_FUNCTION     : void ncl_add_lablst(label, numlab)
**       Add a wildcard to the list
**    PARAMETERS
**       INPUT  :
**          label  - type of marker to add
**       OUTPUT :
**          numlab - current list length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_add_lablst(label,numlab)
char *label;
UM_int2 *numlab;
{
	int status;

	if (lablst_init)
		*numlab = lablst.cur_cnt;
	else
		*numlab = 0;
	if (!lablst_init)
	{
		status=uu_list_init1(&lablst,NCL_MAX_LABEL*sizeof(char),10,10);
		lablst_init = (status == UU_SUCCESS);
	}
	if (lablst_init) 
	{
		uu_list_push(&lablst, label);
		*numlab = lablst.cur_cnt;
	}
	return;
}
/*********************************************************************
**    S_FUNCTION     : int S_get_length(str,maxlen)
**       Get string length
**    PARAMETERS
**       INPUT  :
**          maxlen - size of input string
**       OUTPUT :
**          none
**    RETURNS      : position of first blank space or maxlen if none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_get_length(str,maxlen)
char *str;
int maxlen;
{
	int i;

	for (i=0;i<maxlen;i++)
		if (str[i]==' ') return i;
	return maxlen;
}
/*********************************************************************
**    S_FUNCTION     : int S_isin_list(chkval,list,len)
**       Check if a value is in one of the UM_int2 lists
**    PARAMETERS
**       INPUT  :
**          maxlen - size of input string
**       OUTPUT :
**          none
**    RETURNS      : position of first blank space or maxlen if none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int S_isin_list(chkval,list,len)
UM_int2 chkval;
UU_LIST *list;
int len;
{
	int i;
	UM_int2 *lst;

	lst = (UM_int2 *)UU_LIST_ARRAY(list);
	for (i=0;i<len;i++)
		if (lst[i] == chkval) return 1;
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : void ncl_chk_layf(layer,found)
**       Check if a layer is in laylst
**    PARAMETERS
**       INPUT  :
**          layer - layer to check
**       OUTPUT :
**          found - set to true if layer is in the list
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_chk_layf(layer,found)
UM_int2 *layer;
UU_LOGICAL *found;
{
	int i;
	UM_int2 *lay;

	*found = S_isin_list(*layer,&laylst,laylst.cur_cnt);
}
/*********************************************************************
**    E_FUNCTION     : void ncl_filter_keep(key,ietype,label,keep)
**       Determines whether given entity should be kept in geometry
**       list for filter function
**    PARAMETERS
**       INPUT  :
**          key    - key of entity
**          ietype - ietype of entity
**          label  - entity label used for wildcard matching
**       OUTPUT :
**          keep   - flag set to true if entity matches filters
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_filter_keep(key,ietype,label,keep)
UU_KEY_ID *key;
UM_int2 *ietype;
char *label;
UU_LOGICAL *keep;
{
	int status,i,isub,pos;
	UM_int4 wlen,slen;
	char *labs;
	UM_int2 *lst,color,pen,lintyp,linwgt,layer,marker;
	UU_LOGICAL geomatch,mkrmatch,laymatch,clrmatch,labmatch;

	geomatch = mkrmatch = laymatch = clrmatch = labmatch = UU_TRUE;
	if (geolst_init)
		geomatch = S_isin_list(*ietype,&geolst,geolst.cur_cnt);
	if (clrlst_init || laylst_init)
	{
		umf_get_attrib(key,&color,&pen,&lintyp,&linwgt,&layer);
		if (clrlst_init)
			clrmatch = S_isin_list(color,&clrlst,clrlst.cur_cnt);
		if (laylst_init)
			laymatch = S_isin_list(layer,&laylst,laylst.cur_cnt);
	}
	if (mkrlst_init && (*ietype==3 || *ietype==20))
	{
		umf_get_marker_type (key,&marker);
		mkrmatch = S_isin_list(marker,&mkrlst,mkrlst.cur_cnt);
	}
	if (lablst_init)
	{
		labmatch = UU_FALSE;
		slen = S_get_length(label,NCL_MAX_LABEL);
		labs = (char *)UU_LIST_ARRAY(&lablst);
		for (i=0;i<lablst.cur_cnt;i++)
		{
			wlen = S_get_length(labs,NCL_MAX_LABEL);
			chkwstr(label,&slen,labs,&wlen,&labmatch);
			if (labmatch) break;
			labs += NCL_MAX_LABEL;
		}
	}
	if (geomatch && laymatch && clrmatch && mkrmatch && labmatch)
		*keep = UU_TRUE;
	else
		*keep = UU_FALSE;

	return;
}
/*********************************************************************
**    E_FUNCTION     : void ncl_delete_filtlsts()
**       Delete filter lists
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_delete_filtlsts()
{
	if (mkrlst_init)
	{
		mkrlst_init = UU_FALSE; uu_list_free(&mkrlst);
	}
	if (clrlst_init)
	{
		clrlst_init = UU_FALSE; uu_list_free(&clrlst);
	}
	if (laylst_init)
	{
		laylst_init = UU_FALSE; uu_list_free(&laylst);
	}
	if (geolst_init)
	{
		geolst_init = UU_FALSE; uu_list_free(&geolst);
	}
	if (lablst_init)
	{
		lablst_init = UU_FALSE; uu_list_free(&lablst);
	}
	if (clplst_init)
	{
		clplst_init = UU_FALSE; uu_list_free(&clplst);
	}
}

