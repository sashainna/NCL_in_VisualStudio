/*********************************************************************
**    NAME         :  nevx.c
**       CONTAINS:  Routines to interface to Varimetrix.
**           ncl_vxchk()
**           int vxchk (f77label, isub, nclkey, ist)
**           int vxstor (f77label, isub, nclkey, ist)
**           int ncl_store_name (label, isub, ietype, key)
**           int vxdel (f77label, isub, ietype, irslt)
**           int vxdlk (nclkey, irslt)
**           int vxlini ()
**           int vxlfin ()
**           gtsize (ietype, nwds)
**           vxlfst (f77label, isub, nclkey, nwds, ietype)
**           vxlnxt (f77label, isub, nclkey, nwds, ietype, ipg, iel)
**           ncl_vxlnxt (nclkey,ietype)
**           int ncl_vx_geotype (rel_num)
**           int ncl_vx_delall()
**           int ncl_entnam_list (ietype, lst, icnt)
**           int vxnupd (f77label, isub, maxix)
**           int vxtest (iflg)
**           ncl_get_entity_ptr (f77label, isub, indx)
**           void ncl_setnext_curptr()
**           nclf_set_curmac()
**           ncl_set_curmac()
**           ncl_get_curmac()
**           setvxidx()
**           getvxinx()
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nevx.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:58
*********************************************************************/

#include "usysdef.h"
#include "driver.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdeval.h"
#include "lcom.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclvx.h"

/* This define was used to add code to call vx inq functions twice when */
/* an error was returned to detour a VX bug. */
/* #define INQBUG */

char *uu_lsinsrt(), *uu_lsend(), *uu_lsnext(), *uu_lsnew(), *uu_lsdele();
char *uu_lsempty();

#define NCL_NAME_HASH 37

static char *NCL_name_list[NCL_NAME_HASH] = {UU_NULL};
struct NCL_name_list_data
  {
  UU_KEY_ID key;
  char *macptr;
  UM_int4 isub;
  UM_int4 ipg;
  UM_int4 iel;
  UM_int2 ipgo;
  UM_int2 ielo;
  UM_int2 nwds;
  UM_int2 ietype;
  UM_int2 ivx;
  char label[NCL_MAX_LABEL+1];
  };
static struct NCL_name_list_data *NCL_name_list_curptr = UU_NULL;
static int NCL_name_list_idx = 0;
int NCL_vx_flag = 0; /* 0=native NCL, 1=run native under VX, 2=run VX NCL */

int NCL_vx_createf = 0;

/*********************************************************************
**    E_FUNCTION     : ncl_vxchk (label,isub,nclkey,relnum)
**       Looks for an entity label in NCL_name_list. If found returns
**       the key and relation number of the entity.
**    PARAMETERS   
**       INPUT  : 
**          label    - Label if entity.
**          isub     - Subscript of entity.
**       OUTPUT :  
**          nclkey   - Key of entity.
**          relnum   - Relation of entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_vxchk(label,isub,nclkey,relnum)
char *label;
int isub;
UU_KEY_ID *nclkey;
int *relnum;
{
	int status,i,sub;
	char ncllabel[NCL_MAX_LABEL];
	UM_f77_str_ptr flab;
	UM_int4 ipg, iel;
	UM_int2 nwds, ietype;
/*
.....Initialize label
*/
	strcpy(ncllabel,label);
	for (i=strlen(ncllabel);i<NCL_MAX_LABEL;i++) ncllabel[i] = ' ';
	UM_init_f77_str(flab,ncllabel,NCL_MAX_LABEL);
	sub = isub;
/*
......Get key of entity
*/
	status = vxchk(UM_addr_of_f77_str(flab),&sub,nclkey,&ipg,&iel,&nwds,&ietype);
/*
.....Get relation number
*/
	if (status == UU_SUCCESS && *nclkey != 0)
		ur_retrieve_data_relnum(*nclkey,relnum);
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int vxchk (f77label, isub, nclkey, nwds, ietype)
**       Look entity label in NCL_name_list. If not found, ask VX about it.
**    PARAMETERS   
**       INPUT  : 
**          f77label - Label if entity.
**          isub     - Subscript of entity.
**       OUTPUT :  
**          nclkey   - Key of entity.
**          ietype   - Subclass of entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
vxchk (f77label, isub, nclkey, ipg, iel, nwds, ietype)
UM_f77_str_ptr f77label;
UM_int4 *isub;
UM_int4 *nclkey, *ipg, *iel;
UM_int2 *nwds, *ietype;
{
	int status, num,i;
	char *ncl_get_macptr();
	struct NCL_name_list_data *p1, *ncl_get_entity_ptr();
	UM_int2 ifl38, ifl45, val38, val45;

/*   ------   FIX   ------ temp timing code   */
#ifdef TMP_TIMING
	int itim;
	itim = 0;
	status = ncl_time (&itim, "ncl_vxchk");
#endif
/*   ------ END FIX ------ temp timing code   */

	status = UU_FAILURE;
	*ietype  = 1;  /* NCL_UNKNOWNV; */
	*nclkey  = 0;
	num = 0;
get:;
	p1 = ncl_get_entity_ptr(f77label, *isub, num);
	if (p1)
	{
		*nclkey = p1->key;
		*ipg = p1->ipg;
		*iel = p1->iel;
		*nwds = p1->nwds;
		*ietype = p1->ietype;
		status = UU_SUCCESS;
		if (*ietype == NCLI_MACRO) 
/*
.....only if they are not inside macro or loop define mode (which is not execute 
.....the macro/loop yet, so do not set the current macro pointer)
*/
		{
			ifl38 = 38; ifl45 = 45;
			getifl(&ifl38, &val38);
			getifl(&ifl45, &val45);
			if ((val45!=1) && (val38!=1))
				status = ncl_set_macptr (p1->macptr);
		}
		else if (*ietype == NCLI_MPARM)
		{
/*
.....only if the paramters under this macro is the active macro, it is found
*/
			if (ncl_get_macptr()!=p1->macptr)
			{
/*
.....we need get the label again because it could have same label with 
.....diffferent MACRO, but set the indx++
*/
				num++;
				goto get;
			}
		}
	}
/*   ------   FIX   ------ Restore this code & add flag for remove, get etc */
/*                         to prevent VX entity from being created twice.   */
#ifdef TEMP
/* #ifdef NCL_VX */
   if (*ietype == 1)
     {
     cstr = UM_cstr_of_f77_str(f77label);
     i = 0;
     while (*cstr != ' ' && *cstr != '\0' && i<NCL_MAX_LABEL) label[i++] = *cstr++;
     label[i] = '\0';
     status = ncl_vx_find (label, *isub, nclkey, nwds, ietype);
     if (*ietype>1)
       {
       *ipg = 0;
       *iel = 0;
       }
     }
#endif
/*   ------ END FIX ------ Restore this code & add flag for remove, get etc */

/*   ------   FIX   ------ temp timing code   */
#ifdef TMP_TIMING
   status = ncl_time (&itim, "ncl_vxchk");
#endif
/*   ------ END FIX ------ temp timing code   */
   return (status);

   }
/*********************************************************************
**    E_FUNCTION     : int vxstor (f77label, isub, ipg, iel, nclkey, ist)
**       Store entity label in NCL_name_list.
**    PARAMETERS   
**       INPUT  : 
**          f77label - Label if entity.
**          isub     - Subscript of entity.
**       OUTPUT :  
**          nclkey   - Key of entity.
**          ist      - Subclass of entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
vxstor (f77label, isub, nclkey, ipg, iel, nwds, ietype, novxcreat)
UM_f77_str_ptr f77label;
UM_int4 *ipg, *iel, *isub, *nclkey;
UM_int2 *nwds, *ietype, *novxcreat;
   {
   int status;
   char *cstr;
   UU_KEY_ID key;

   cstr = UM_cstr_of_f77_str(f77label);
   key = *nclkey;
   status = ncl_store_name(cstr, *isub, *ipg, *iel, *nwds, *ietype, 
            *novxcreat, &key);

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_store_name (label,isub,ipg,iel,nwds,ietype,key)
**       Store a NCL entity name in the NCL_name_list.
**    PARAMETERS   
**       INPUT  : 
**          label      - Label if entity.
**          isub       - Subscript of entity.
**          nwds       - Size of geometry.
**          ietype     - Subclass of entity.
**          novxcreat  - 0 = create vx entity, else don't create.
**          key        - Key of entity.
**       OUTPUT :  
**          key        - New key of entity if it is a VX type.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_store_name (label, isub, ipg, iel, nwds, ietype, novxcreat, key)
char *label;
UM_int4 isub,ipg, iel;
UM_int2 nwds, ietype, novxcreat;
UU_KEY_ID *key;
{
	int status, i, n1, idx, err;
	struct NCL_name_list_data *p1;
	struct NCL_fixed_databag e1;
	struct NCL_nclattr_rec attr;
	char *ncl_get_macptr();
	int ix,maxchr;
	char lab[NCL_MAX_LABEL+1];

	status = UU_SUCCESS;
	ix = 0;
	i = 0;
/*
......if it is a macro, we only allow 6 chars to store (when we save
......the macro name as 6 chars because we use 'token' and it will cut
......up to 6 chars. So here should be consistent. we don't need this in V97
*/
/*	if (ietype==NCLI_MACRO)
		maxchr = 6;
	else
*/
		maxchr = NCL_MAX_LABEL;
	while (*label != ' ' && *label  != '\0' && i<maxchr)
	{
		ix += *label;
		lab[i++] = *label++;
	}
	lab[i] = '\0';
	ix = (ix + isub) % NCL_NAME_HASH;
	if (NCL_name_list[ix] == UU_NULL)
	{
		NCL_name_list [ix]= uu_lsnew();
		if (NCL_name_list [ix]== UU_NULL) status = UU_FAILURE;
	}

	if (status == UU_SUCCESS)
	{
		n1 = sizeof(struct NCL_name_list_data);
		p1 = (struct NCL_name_list_data *) NCL_name_list[ix];
		if (p1 != UU_NULL) p1 = (struct NCL_name_list_data *) uu_lsinsrt(p1,n1);
		if (p1 == UU_NULL)
			status = UU_FAILURE;
		else
		{
			p1->ietype = ietype;
			if (ietype == NCLI_MACRO)
			{
				p1->macptr = ncl_get_macptr();
				p1->ipgo = ipg;
				p1->ielo = iel;
			}
			else
			{
/*
.....if it is macro parameter, we need store the macro pointer too
.....because this macro parameter is only under those macro name
*/
				if (ietype == NCLI_MPARM)
					p1->macptr = ncl_get_macptr();
				else
					p1->macptr = UU_NULL;
				p1->ipgo = 0;
				p1->ielo = 0;
			}
			if (ietype == NCLI_RESERVED_ID)
				p1->ipg = 0;
			else
				p1->ipg = ipg;
			p1->iel  = iel;
			p1->nwds = nwds;
			p1->isub = isub;
			strcpy (p1->label, lab);
			if (*key == 0 && novxcreat == 0 && NCL_vx_flag)
			{
				err = vx_mdl_inq_index (p1->label, &idx);

#ifdef INQBUG
				if (err !=0) err = vx_mdl_inq_index (p1->label, &idx);
#endif
				if (err == 0 && idx > 0)
				{
					p1->ivx = 1;
					status = ncl_vx_create (idx, ietype, p1->label, &e1);

					if (status == UU_SUCCESS)
					{
						p1->key = *key = e1.key;
						ncl_vx_set_attr (idx);
						ncl_init_attr_rec (e1.key, &attr, i);
						ncl_vx_set_blank (idx, e1.key);
					}
/*
.....If Failed, delete list that already created
.....Yurong 11/2/98
*/
					else
					{
						if (p1!=NULL)
						uu_lsdele(p1);
						return status;
					}		
					if ( NCL_vx_flag == 1)
					{
						if (status == UU_SUCCESS)
							status = ur_update_view_key(e1.key, 0);

						if (status == UU_SUCCESS)
							status = uv_disp_entity(&e1);
					}
				}
				else
				{
					p1->ivx = NCL_vx_createf;
					p1->key = *key;
				}
			}
			else
			{
				p1->ivx = NCL_vx_createf;
				p1->key = *key;
			}
		}
	}
	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int vxdel (f77label, isub, irslt)
**       Delete entity label from NCL_name_list by label.
**    PARAMETERS 
**       INPUT  :
**          f77label - Label of entity.
**          isub     - Subscript of entity.
**          irslt    - = 0, delete macro record.
**       OUTPUT :  
**          irslt    - = 1 iff not successful; else 0
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
vxdel (f77label, isub, irslt)
UM_f77_str_ptr f77label;
UM_int4 *isub;
UM_int2 *irslt;
{
	int status = UU_FAILURE;
	struct NCL_name_list_data *p1, *ncl_get_entity_ptr();
	UU_LOGICAL ldelmac;

	ldelmac = *irslt == 0;
	*irslt = 1;

	p1 = ncl_get_entity_ptr(f77label, *isub, 0);
	if (p1)
	{
		status = UU_SUCCESS;
		if (ldelmac && p1->macptr != UU_NULL)
		{
/*
.....don't delete the macptr when it is parameter type because
.....it deleted or will delete when p1->ietype== NCLI_MACRO, otherwise
.....it will hit error because it try to delete this macptr more than once
.....yurong 6/16/05
*/
			if (p1->ietype!=NCLI_MPARM)
				status = ncl_del_maclist (p1->macptr);
		}
		p1 = (struct NCL_name_list_data *)uu_lsdele(p1);
		*irslt = 0;
	}
	return (status);
}
           
/*********************************************************************
**    E_FUNCTION     : void ncl_setnext_curptr()
**       Move forward the pointer NCL_name_list_curptr.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_setnext_curptr()
{
	NCL_name_list_curptr = (struct NCL_name_list_data *)
		uu_lsnext(NCL_name_list_curptr);
	while (NCL_name_list_curptr == UU_NULL &&
				NCL_name_list_idx < NCL_NAME_HASH)
	{
		NCL_name_list_idx++;
		if (NCL_name_list_idx < NCL_NAME_HASH )
		{
			NCL_name_list_curptr =
			(struct NCL_name_list_data *) NCL_name_list[NCL_name_list_idx];
			if (NCL_name_list_curptr != UU_NULL )
				NCL_name_list_curptr =
				(struct NCL_name_list_data *) uu_lsnext(NCL_name_list_curptr);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : int vxdlk (nclkey, irslt)
**       Delete entity label from NCL_name_list by key.
**    PARAMETERS 
**       INPUT  :
**          nclkey   - Key of entity.
**       OUTPUT :  
**          irslt    - = 1 iff not succesful; else 0
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
vxdlk (nclkey, ietype, irslt)
UM_int4 *nclkey;
UM_int2 *ietype, *irslt;
{
	int i, status;
	struct NCL_name_list_data *p1;

	status = UU_FAILURE;

	*irslt  = 1;
	for (i=0; i<NCL_NAME_HASH && *irslt==1; i++)
	{
		p1 = (struct NCL_name_list_data *) NCL_name_list[i];
		if (p1 != UU_NULL) p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		while (p1 != UU_NULL)
		{
			if (p1->key == (UU_KEY_ID)*nclkey && p1->ietype == *ietype)
			{
				status = UU_SUCCESS;
				if (p1 == NCL_name_list_curptr)
					ncl_setnext_curptr(); 
				if (p1->macptr != UU_NULL) 
				{
/*
.....don't delete the macptr when it is parameter type because
.....it deleted or will delete when p1->ietype== NCLI_MACRO, otherwise
.....it will hit error because it try to delete this macptr more than once
.....yurong 6/16/05
*/
					if (p1->ietype!=NCLI_MPARM)
						status = ncl_del_maclist (p1->macptr);
				}
				p1 = (struct NCL_name_list_data *)uu_lsdele(p1);
				*irslt = 0;
				break;
			}
			else
				p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		}
	}
	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int vxlini ()
**       Initiialize the NCL_name_list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
vxlini ()
   {
   int i, status;

   status = UU_SUCCESS;

   for (i=0; i<NCL_NAME_HASH && status == UU_SUCCESS; i++)
     {
     if (NCL_name_list[i] == UU_NULL)
       {
       NCL_name_list[i] = uu_lsnew();
       if (NCL_name_list[i] == UU_NULL) status = UU_FAILURE;
       }
     }

   if (status == UU_SUCCESS)
     status = ncl_maclist_init();

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int vxlfin ()
**       Delete all entries from the NCL_name_list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
vxlfin ()
{
	int i, ix, status = UU_SUCCESS;
	struct NCL_name_list_data *p1;
	char *mnptr, *lptr;

	while (ncl_get_next_called_macro (&mnptr))
	{
		ix = 0;
		lptr = mnptr;
		do { ix += *lptr; } while (*lptr++);
		ix = ix % NCL_NAME_HASH;
		p1 = (struct NCL_name_list_data *) NCL_name_list[ix];
		if (p1 != UU_NULL) p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		while (p1 != UU_NULL)
		{
			if (!strcmp(mnptr, p1->label) && p1->isub == 0)
			{
				p1 = (struct NCL_name_list_data *)uu_lsdele(p1);
				break;
			}
			p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		}
		ncl_del_first_called_macro();
	}

	for (i=0; i<NCL_NAME_HASH && status == UU_SUCCESS; i++)
	{
		p1 = (struct NCL_name_list_data *) NCL_name_list[i];
		if (p1 != UU_NULL ) p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		while (p1 != UU_NULL &&
			p1 != (struct NCL_name_list_data *) NCL_name_list[i])
		{
			if (p1->macptr != UU_NULL)
			{
/*
.....don't delete the macptr when it is parameter type because
.....it deleted or will delete when p1->ietype== NCLI_MACRO, otherwise
.....it will hit error because it try to delete this macptr more than once
.....yurong 6/16/05
*/
				if (p1->ietype!=NCLI_MPARM)
					status = ncl_del_maclist (p1->macptr);
			}
			p1 = (struct NCL_name_list_data *)uu_lsdele(p1);
		}
		if (NCL_name_list[i]) uu_lsdel(NCL_name_list[i]);
		NCL_name_list[i] = UU_NULL;
	}

	status = ncl_maclist_done ();

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : gtsize (ietype, nwds)
**       Return the number of words for a geometry type.
**    PARAMETERS
**       INPUT  :
**          ietype     - Type of geometry.
**       OUTPUT :
**          nwds       - Number of real*8 words.
**    RETURNS      :
**         none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtsize (ietype, nwds)
UM_int2 *ietype, *nwds;
   {
   switch (*ietype)
     {
     case NCLI_POINT:
     case NCLI_VECTOR:
       *nwds = 3;
       break;
     case NCLI_LINE:
       *nwds = 6;
       break;
     case NCLI_PLANE:
       *nwds = 4;
       break;
     case NCLI_CIRCLE:
       *nwds = 11;
       break;
     default:
       *nwds = 1;
       break;
     }

   }
/*********************************************************************
**    E_FUNCTION     : vxlfst ()
**       Set the NCL name list pointer to the first entry in NCL_name_list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Sets NCL_name_list_curptr
**    WARNINGS     : none
*********************************************************************/
void vxlfst ()
   {

   NCL_name_list_curptr = (struct NCL_name_list_data *) NCL_name_list[0];
   NCL_name_list_idx = 0;
   if (NCL_name_list_curptr != UU_NULL )
      ncl_setnext_curptr(); 
   }
/*********************************************************************
**    E_FUNCTION     : vxlnxt (f77label, isub, nclkey, nwds, ietype, ipg, iel)
**       Return the next entity in the NCL name list.
**    PARAMETERS
**       INPUT  :
**         Uses NCL_name_list_curptr.
**       OUTPUT :
**          f77label   - Label of entity
**          isub       - Subscript of entity
**          nclkey     - Key of entity.
**          nwds       - Number of real*8 words.
**          ietype     - Type of geometry or 1 if none found.
**    RETURNS      :
**         none.
**    SIDE EFFECTS : Sets NCL_name_list_curptr to next entry in NCL_name_list,
**    WARNINGS     : none
*********************************************************************/
void vxlnxt (f77label, isub, nclkey, nwds, ietype, ipg, iel)
UM_f77_str_ptr f77label;
UM_int4 *isub, *nclkey, *ipg, *iel;
UM_int2 *ietype, *nwds;
{
   int i;
   char *cstr;
   char *p1;

   cstr = UM_cstr_of_f77_str(f77label);

   *ietype = 1;
	if (NCL_name_list_curptr == UU_NULL )
		return;
   if (NCL_name_list_idx < NCL_NAME_HASH )
     {
     for (i=0,p1=NCL_name_list_curptr->label; i<NCL_MAX_LABEL && *p1!='\0'; i++)
       *cstr++ = *p1++;
     while (i<NCL_MAX_LABEL)
       {
       *cstr++ = ' ';
       i++;
       }
     *isub = NCL_name_list_curptr->isub;
     *nclkey = NCL_name_list_curptr->key;
     *nwds = NCL_name_list_curptr->nwds;
     *ietype = NCL_name_list_curptr->ietype;
     *ipg = NCL_name_list_curptr->ipg;
     *iel = NCL_name_list_curptr->iel;
     ncl_setnext_curptr(); 
     }

   }
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_geotype (rel_num)
**       Determine if an entity is a VX type.
**    PARAMETERS
**       INPUT  :
**          rel_num  - Relation number of entity.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_TRUE iff entity is VX; else UU_FALSE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_geotype (rel_num)
int rel_num;
   {
   int status;

   if (rel_num >= 100 && rel_num <= 104)
     status = UU_TRUE;
   else
     status = UU_FALSE;

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_vx_delall()
**       Delete all VX entities from the name list & unibase.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vx_delall()
   {
   int i, status;
   int dsegid;
   struct NCL_name_list_data *p1;

   status = UU_SUCCESS;

   for (i=0; i<NCL_NAME_HASH; i++)
     {
     p1 = (struct NCL_name_list_data *) NCL_name_list[i];
     if (p1 != UU_NULL ) p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		while (p1 != UU_NULL &&
			p1 != (struct NCL_name_list_data *) NCL_name_list[i])
       {
       if (p1->ivx == 1)
         {
         ur_retrieve_disp_segid (p1->key, &dsegid);
         ur_delete_all (p1->key);
         if (dsegid >= 0) uv_delsegs (dsegid);
         p1 = (struct NCL_name_list_data *)uu_lsdele(p1);
         }
       else
         p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
       }
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_entnam_list (ietype, lst, icnt)
**       Push the names of all entities of a certain type onto a list in
**       alphabetical order.
**    PARAMETERS
**       INPUT  :
**          ietype     - Type of entity to search for.
**       OUTPUT :
**          lst        - List of entities of type ietype.
**          icnt       - Number of entities found.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : None
**    WARNINGS     : Ignores subscript.
*********************************************************************/
int
ncl_entnam_list (ietype, lst, icnt)
int ietype;
char *lst;
int *icnt;
   {
   int i, status = UU_SUCCESS;
   char *l1, *l2;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT];
   struct NCL_name_list_data *p1;

   *icnt = 0;
   for (i=0; i<NCL_NAME_HASH; i++)
     {
     p1 = (struct NCL_name_list_data *) NCL_name_list[i];

     if (p1 != UU_NULL )
        p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
     while (p1 != UU_NULL)
       {
       if (p1->ietype == ietype)
         {
         l1 = lst;
         l2 = uu_lsnext (lst);
			if (p1->isub == 0)
				strcpy(label,p1->label);
			else
				sprintf(label,"%s(%d)",p1->label,p1->isub);
         while (l2 != UU_NULL && strcmp(label, l2) > 0)
           {
           l1=l2;
           l2=uu_lsnext(l2);
           }
			l1 = uu_lsinsrt(l1, strlen(label)+1);
			strcpy (l1,label);
         (*icnt)++;
         }
       p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
       }
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int vxnupd (f77label, isub, maxix)
**       Set the ipg field of an entity.
**       Used to set maximum index of reserved variable.
**    PARAMETERS
**       INPUT  :
**          f77label  - Label of entity.
**          isub      - subscript of entity.
**          maxix     - Value words to set.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
vxnupd (f77label, isub, maxix)
UM_f77_str_ptr f77label;
UM_int4 *isub;
UM_int4 *maxix;
   {
   int status;
   struct NCL_name_list_data *p1, *ncl_get_entity_ptr();

   status = UU_FAILURE;

   p1 = ncl_get_entity_ptr(f77label, *isub, 0);
   if (p1)
   {
     if (p1->ipg < *maxix) p1->ipg = *maxix;
     status = UU_SUCCESS;
   }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int vxtest (iflg)
**       Fortran callable routine to determine whether NCL is running under
**       VX.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          iflg      - =1 iff NCL is running under VX.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
vxtest (iflg)
UM_int2 *iflg;
   {
   int status = UU_SUCCESS;

   *iflg = NCL_vx_flag;

   return (status);
   }
/***************************************************************************
**
**     FUNCTION: subchk
**
**     PURPOSE:  Searches for the lowest subscript for a particular 
**               identifier.
**
*************************************************************************/
int
subchk(f77label, isub)
UM_f77_str_ptr f77label;
UM_int4 *isub;
{
/*	UM_int4 nclkey;*/
/*	UM_int2 ietype;*/
   int status, i;
/*	char label[NCL_MAX_LABEL+1];*/
/*	char *cstr;*/
   struct NCL_name_list_data *p1,*ncl_get_entity_ptr();
   char *ncl_get_macptr();
/*	int ix;*/
	int lowest;
	/*int list_elem;
	int counter, savix;
	int init_ix;
	counter =1;
	init_ix =0;
	list_elem = 0;

	lowest = 9999999;
*/
   status = UU_FAILURE;

/*
.....This for loop will deadlock NCL if we can't find a match for f77label.
.....made the change to setuo highest limit
.....since the highest subscript of entity will not excess 
	for(i=1; ;i++)
*/
	for(i=1; i<1000000;i++)
	{
		p1 = ncl_get_entity_ptr(f77label, i, 0);	
		if(p1)
		{
			status = UU_SUCCESS;
			lowest = i;
			goto done;
		}
	}	
/*
.....if goes here, not found any
*/
	lowest = 0;
	return status;
   /*cstr = UM_cstr_of_f77_str(f77label);
   ix = 0;
   i = 0;
   while (*cstr != ' ' && *cstr != '\0' && i<NCL_MAX_LABEL)
   {
     ix += *cstr;
     label[i++] = *cstr++;
   }
   ix++;
	savix = ix;
try_again:;
	list_elem++;
	if (list_elem >40) goto done;
   label[i] = '\0';
   ix = (ix) % NCL_NAME_HASH;
	if (init_ix ==0) init_ix = ix;
	if (ix < init_ix) goto done;
   ietype  = 1;*/  /* NCL_UNKNOWNV; */
   /*nclkey  = 0;
   p1 = (struct NCL_name_list_data *) NCL_name_list[ix];

	if (p1 == UU_NULL)
	{
		ix = savix + counter;
		counter++;
		goto try_again;
	}
	while (p1 !=UU_NULL)
	{
		p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		if ((p1 == UU_NULL))
		{
			ix = savix + counter;
			counter++;
			goto try_again;
		}
		if ((p1->isub !=0) && (p1->isub < lowest)) lowest=p1->isub;
      if (lowest ==1) goto done;
	}
done:;
	if (lowest != 9999999) *isub = lowest;
	*/
done:	
	*isub = lowest;
	return(UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_get_entity_ptr (f77label, isub, indx)
**       Return name list entity pointer.
**		I added the 'indx' because for MACRO parameter, it could have same
**			lable but under different macro pointer, so sometime, we are not
**			only just compare the label but need compare other paramter, so
**			set the indx here to allow access to same label under different
**			macro pointer. For most label, just set indx = 0
**		Yurong changed.
**    PARAMETERS
**       INPUT  :
**          f77label  - Label of entity.
**          isub      - subscript of entity.
**			indx:		- indx number of the same label number
**       OUTPUT :
**          none
**    RETURNS      :
**         pointer to name list structure or NULL if not found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
struct NCL_name_list_data *
ncl_get_entity_ptr (f77label, isub, indx)
UM_f77_str_ptr f77label;
UM_int4 isub;
int indx;
{
	int i, ix, num;
	struct NCL_name_list_data *p1;
	char *cstr;
	char lab[NCL_MAX_LABEL+1];

	cstr = UM_cstr_of_f77_str(f77label);
	ix = 0;
	i = 0;
	while (*cstr != ' ' && *cstr != '\0' && i<NCL_MAX_LABEL)
	{
		ix += *cstr;
		lab[i++] = *cstr++;
	}
	lab[i] = '\0';
	ix = (ix + isub) % NCL_NAME_HASH;
	p1 = (struct NCL_name_list_data *) NCL_name_list[ix];
	if (p1) p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
 
	num = 0;
	while (p1)
	{
		if (!strcmp(lab, p1->label) && p1->isub == isub)
		{
			if (num==indx)
			{
				uu_lsmovel (p1, NCL_name_list[ix]);
				break;
			}
			num++;
		}
		else if (num>0)
		{
			p1 = UU_NULL;
			break;
		}
		p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
	}
	return (p1);
}
	
/*********************************************************************
**    E_FUNCTION     : nclf_set_curmac (f77mnam)
**       Fortran callable routine to set the current macro as name.
**    PARAMETERS
**       INPUT  :
**          f77mnam  - macro name.
**       OUTPUT :
**          none
**    RETURNS      :
**         0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclf_set_curmac (f77mnam, nc)
UM_f77_str_ptr f77mnam;
UM_int4 *nc;
{
	char *mcname;

	mcname = UM_cstr_of_f77_str(f77mnam);
	return(ncl_set_curmac(mcname));
}

/*********************************************************************
**    E_FUNCTION     : int ncl_set_curmac (name)
**       Set the current macro as name.
**    PARAMETERS
**       INPUT  :
**          name  - macro name.
**       OUTPUT :
**          none
**    RETURNS      :
**         0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_set_curmac (name)
char *name;
{
	int i, ix, isub;
	struct NCL_name_list_data *p1;
	char lab[NCL_MAX_LABEL+1];

	ix = 0;
	i = 0;
	while (*name != ' ' && *name != '\0' && i<NCL_MAX_LABEL)
	{
		ix += *name;
		lab[i++] = *name++;
	}
	lab[i] = '\0';
/*
.....isub = 0 for macro name
*/
	isub = 0;
	ix = (ix + isub) % NCL_NAME_HASH;
	p1 = (struct NCL_name_list_data *) NCL_name_list[ix];
	if (p1) p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
 
	while (p1)
	{
		if (!strcmp(lab, p1->label))
		{
			if (p1->ietype == NCLI_MACRO) 
			{
				ncl_set_macptr (p1->macptr);
				return 0;
			}
		}
		p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
	}
	return (-1);
}
/*********************************************************************
**    E_FUNCTION     : ncl_get_curmac (f77mnam, nc)
**       Set the current macro as name.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          f77mnam  - macro name.
**          nc  - macro name length
**    RETURNS      :
**         0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_curmac (f77mnam, nc)
UM_f77_str_ptr f77mnam;
UM_int4 *nc;
{
	int i;
	struct NCL_name_list_data *p1;
	char *mcname;
   char *ncl_get_macptr();

	mcname = UM_cstr_of_f77_str(f77mnam);

	for (i=0; i<NCL_NAME_HASH; i++)
	{
		p1 = (struct NCL_name_list_data *) NCL_name_list[i];

		if (p1 != UU_NULL )
			p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		while (p1 != UU_NULL)
		{
			if (p1->ietype == 11)
			{
				if (p1->macptr==ncl_get_macptr())
				{
					strcpy(mcname, p1->label);
					*nc = strlen (mcname);
					return 0;
				}
			}
			p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		}
	}
	*nc = 0;
	return -1;
}

/*********************************************************************
**    E_FUNCTION     : ncl_ifmac_local(label, mcname)
**       check if the variable label belong to a macro
**    PARAMETERS
**       INPUT  :
**          label: label to be checked
**			macro: macro to be checked
**       OUTPUT :
**          none
**    RETURNS   :
**		   1: Yes
**         0: No
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_ifmac_local(label, mcname)
char *label, *mcname;
{
	char *tok, tmpstr[256];

	strcpy(tmpstr, label);
	tok = (char*)strtok (tmpstr, "#");
	if (tok==NULL)
		return 0;
	else
	{
		if (strcmp (tok, mcname)==0)
			return 1;
		else
			return 0;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_delcur_macvar ()
**       Delete the current macro local variables
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_delcur_macvar ()
{
	int i, nc;
	struct NCL_name_list_data *p1;
	char mcname[64];
	int dsegid;

	ncl_get_curmac (mcname, &nc);
	if (nc==0)
		return;

	for (i=0; i<NCL_NAME_HASH; i++)
	{
		p1 = (struct NCL_name_list_data *) NCL_name_list[i];

		if (p1 != UU_NULL )
			p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		while (p1 != UU_NULL &&
			p1 != (struct NCL_name_list_data *) NCL_name_list[i])
		{
			if ((p1->ietype!=NCLI_MACRO) && (p1->ietype!=NCLI_MPARM))
			{
				if (ncl_ifmac_local(p1->label, mcname))
				{
/*
......delete this label, if it is 'B(1)', it have value and key (isub=1,key>0), 
......but B itself does not have value and key (isub=0,key=0), so do nothing
*/
					if (p1->key>0)
					{
						ur_retrieve_disp_segid (p1->key, &dsegid);
						ur_delete_all (p1->key);
						if (dsegid >= 0) uv_delsegs (dsegid);
					}
					p1 = (struct NCL_name_list_data *)uu_lsdele(p1);
				}
				else
					p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
			}
			else
				p1 = (struct NCL_name_list_data *) uu_lsnext(p1);
		}
	}
}


/*********************************************************************
**    E_FUNCTION     : rmlab_prefix(in, out)
**       remove label's macro profix before #
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         0: success
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void rmlab_prefix(in, out, nc)
UM_f77_str_ptr in, out;
UM_int2 *nc;
{
	char *tok, tmpstr[256];
	char *cstr1, *cstr2;

	if (*nc<=0) return;
	
	cstr1 = UM_cstr_of_f77_str(in);
	cstr2 = UM_cstr_of_f77_str(out);

	strncpy(tmpstr, cstr1, *nc);
	tmpstr[*nc] = '\0';
	tok = (char*)strtok (tmpstr, "#");
	if (tok==NULL)
		return;
	else
	{
		tok = (char*)strtok (NULL, " ");
		if (tok!=NULL)
		{
			strcpy(cstr2, "#");
			strcat(cstr2, tok);
		}
		else
		{
			cstr2[0] = '\0';
			strcat(cstr2, tmpstr);
		}
	}
	*nc = strlen (cstr2);
} 

/*********************************************************************
**    E_FUNCTION     : ncl_vxlnxt (nclkey,ietype)
**       Return the next entity in the NCL name list.
**    PARAMETERS
**       INPUT  :
**         Uses NCL_name_list_curptr.
**       OUTPUT :
**          nclkey     - Key of entity.
**          ietype     - Type of geometry or 1 if none found.
**    RETURNS      :
**         UU_TRUE if a valid entity found, else UU_FALSE.
**    SIDE EFFECTS : Sets NCL_name_list_curptr to next entry in NCL_name_list,
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_vxlnxt (nclkey,ietype)
int *ietype;
UU_KEY_ID *nclkey;
{
	if (NCL_name_list_curptr == UU_NULL)
		return (UU_FALSE);
	if (NCL_name_list_idx >= NCL_NAME_HASH)
		return (UU_FALSE);

	*nclkey = NCL_name_list_curptr->key;
	*ietype = NCL_name_list_curptr->ietype;
	ncl_setnext_curptr(); 

	return (UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION     : setvxidx(nextnum, nextptr)
**       Set the current NCL_name_list_idx and NCL_name_list_curptr.
**    PARAMETERS
**          nextnum     - current NCL_name_list_idx.
**          nextptr     - NCL_name_list_curptr.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void setvxidx(nextnum, nextptr)
int *nextnum;
char **nextptr;
{
	NCL_name_list_idx = *nextnum;
	NCL_name_list_curptr = (struct NCL_name_list_data *)*nextptr;
	while (NCL_name_list_curptr == UU_NULL &&
				NCL_name_list_idx < NCL_NAME_HASH)
	{
		NCL_name_list_idx++;
		if (NCL_name_list_idx < NCL_NAME_HASH )
		{
			NCL_name_list_curptr =
			(struct NCL_name_list_data *) NCL_name_list[NCL_name_list_idx];
			if (NCL_name_list_curptr != UU_NULL )
				NCL_name_list_curptr =
				(struct NCL_name_list_data *) uu_lsnext(NCL_name_list_curptr);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : getvxinx(nextnum, nextptr)
**       get the current NCL_name_list_idx and NCL_name_list_curptr.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          nextnum     - current NCL_name_list_idx.
**          nextptr     - NCL_name_list_curptr.
**    RETURNS      :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void getvxinx(nextnum, nextptr)
int *nextnum;
char **nextptr;
{
	*nextnum = NCL_name_list_idx;
	*nextptr = (char *)NCL_name_list_curptr;
}
