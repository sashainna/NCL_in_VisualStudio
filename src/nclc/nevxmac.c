/*********************************************************************
**    NAME         :  nevxmac.c
**       CONTAINS:  Routines to handle storage of macro variables.
**           int mclstr (iflg, f77plab, psub, f77dlab, dsub, dvalue)
**           int mclini ()
**           int mclcll (f77mlab)
**           int mcldel ()
**           int ncl_del_maclist (ptr)
**           int ncl_set_macptr (ptr)
**           char *ncl_get_macptr ()
**           void mclspt(ptr)
**           void mclrpt(ptr)
**           int mclfnd (f77plab, psub, ifnd, iclas, ietype, aval, nclkey)
**           int mclsav (f77alab, asub, val, iclas, ietype)
**           int mclchk (iflg)
**           int mclrst (iflg)
**           int mclprv (f77plab, psub, ifnd, iclas, ietype, aval,
**                       f77mtok, msub)
**           int ncl_maclist_init ()
**           int ncl_maclist_done ()
**           int mclpst ()
**           int ncl_get_next_called_macro (nam)
**           int ncl_del_first_called_macro ()
**           int ncl_create_mparm_scalar (p1)
**           ncl_get_next_maclist()
**           ncl_store_maclist_data()
**           ncl_get_next_macall()
**           ncl_get_maclist_ptrs()
**           ncl_get_macinfo()
**           ncl_store_macinfo()
**           ncl_storehd()
**           ncl_getmc_hd()
**           ncl_getmac_parms0()
**           ncl_getmac_parms()
**           ncl_putmac_parms0()
**           ncl_putmac_parms()
**           ncl_getmac_plabel()
**           ncl_getmac_values()
**           ncl_getmac_desp()
**           ncl_setmac_desp()
**           ncl_setmac_rvalue()
**           ncl_getmac_rvalue()
**           nclf_getmac_callin()
**
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nevxmac.c , 25.4
**    DATE AND TIME OF LAST MODIFICATION
**       10/27/16 , 13:10:47
*********************************************************************/

#include "usysdef.h"
#include "mfort.h"
#include "mdrel.h"
#include "ncl.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclvx.h"
#include "nclmodals.h"

char *uu_lsinsrt(), *uu_lsend(), *uu_lsnext(), *uu_lsnew(), *uu_lsdele();
char *uu_lsempty();

static struct NCL_macro_list_data *NCL_hld_macro_list_ptr = UU_NULL;
static struct NCL_macro_list_data *NCL_cur_macro_list_ptr = UU_NULL;
static struct NCL_macro_list_data *NCL_hld_mac_param_ptr = UU_NULL;
static struct NCL_macro_list_data *NCL_cur_mac_param_ptr = UU_NULL;
static struct NCL_macro_call_data *NCL_macro_call_list = UU_NULL;
static char *NCL_macro_tmpdata = UU_NULL;

/*********************************************************************
**    E_FUNCTION     : int mclstr (iflg, f77plab, psub, f77dlab, dsub, dvalue,
**                                 dclas, detype)
**       Store the next macro parameter & its default in the current
**       macro parameter list.
**    PARAMETERS
**       INPUT  :
**          iflg     - =1 no default.
**                   - =2 default value is name.
**                     =3 default value is scalar value.
**          f77plab  - Macro parameter label.
**          psub     - Macro parameter subscript.
**          f77dlab  - Macro parameter value label.
**          dsub     - Macro parameter value subscript.
**          dvalue   - Scalar value if parameter is a scalar.
**          dclas    - Class of default value.
**          detype   - Subclass of default value.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
mclstr (iflg, f77plab, psub, f77dlab, dsub, dvalue, dclas, detype)
UM_int2 *iflg;
UM_f77_str_ptr f77plab;
UM_int4 *psub;
UM_f77_str_ptr f77dlab;
UM_int4 *dsub;
UM_real8 *dvalue;
UM_int2 *dclas;
UM_int2 *detype;
{
	int status = UU_SUCCESS;
	int i, n1;
	char *cstr;
	struct NCL_macro_list_data *p1;

	n1 = sizeof(struct NCL_macro_list_data);
	p1 = NCL_hld_macro_list_ptr;
	if (p1 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsend ((char*)p1);
	if (p1 == UU_NULL) p1 = NCL_hld_macro_list_ptr;
	if (p1 != UU_NULL) 
		p1 = (struct NCL_macro_list_data *) uu_lsinsrt((char*)p1,n1);
	if (p1 == UU_NULL)
		status = UU_FAILURE;
	else
	{
		p1->iflg = *iflg;
		cstr = UM_cstr_of_f77_str(f77plab);
		i = 0;
		while (*cstr != ' ' && i<NCL_MAX_LABEL) p1->plab[i++] = *cstr++;
		p1->plab[i] = '\0';
		p1->psub = *psub;
		cstr = UM_cstr_of_f77_str(f77dlab);
		i = 0;
		if (*iflg > 1)
		{
			p1->dsub = *dsub;
			p1->dvalue = *dvalue;
			while (*cstr != ' ' && i<NCL_MAX_LABEL) p1->dlab[i++] = *cstr++;
			p1->dclas  = *dclas;
			p1->detype = *detype;
		}
		else
		{
			p1->dvalue = 0.;
			p1->dsub = 0;
			p1->dclas = 0;
			p1->detype = 0;
		}
		p1->dlab[i] = '\0';
		p1->iclas = 0;
		p1->keyscl = 0;
		p1->avalue = 0;
		p1->asub = 0;
		p1->alab[0] = '\0';
		p1->prompt[0] = '\0';
		p1->wrdlist[0] = '\0';
		p1->count = 0;
		p1->substr = 0;
		p1->pflag = 1;
		p1->prsvalue = 4;
		p1->lenvalue = 9;
		p1->min = 0;
		p1->max = 0;
		p1->rvalue = p1->dvalue;
		strcpy(p1->rlab, p1->dlab);
		p1->rsub = p1->dsub;
		p1->ietype = NCLI_MPARM;
		p1->pclas = 0;
		p1->geomflag = 0;
		p1->psel = 0;
		p1->pclr = -1;
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int mclini ()
**       Start a new macro list.
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
mclini ()
{
	int n1;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	int status = UU_SUCCESS;

	NCL_hld_macro_list_ptr =
		NCL_cur_macro_list_ptr = (struct NCL_macro_list_data *) uu_lsnew();
	if (NCL_cur_macro_list_ptr == UU_NULL) 
	{
		status = UU_FAILURE;
		return status;
	}
/*
.....use insert to create a NCL_macro_define_data space
*/
/*
.....Allocate next block
*/
	n1 = sizeof(struct NCL_macro_define_data);
	p1 = NCL_hld_macro_list_ptr;
	p2 = UU_NULL;
	if (p1 != UU_NULL) 
		p2 = (struct NCL_macro_define_data *) uu_lsinsrt((char*)p1,n1);
	if (p2 != UU_NULL)
	{
		strcpy(p2->classname, "DEFAULT");
		p2->prompt[0] = '\0';
		p2->outflag = NCL_macro_outflag;
		p2->dispflag = NCL_macro_remval;
		p2->rvsaved = 0;
	}
	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int mclcll (f77mlab)
**       Push a new macro onto the macro call list.
**    PARAMETERS
**       INPUT  : 
**          f77mlab   - Name of macro
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
mclcll (f77mlab)
UM_f77_str_ptr f77mlab;
{
	int status = UU_SUCCESS;
	int i, n1;
	char *cstr;
	struct NCL_macro_call_data *p1;

	n1 = sizeof(struct NCL_macro_call_data);
	if (!NCL_macro_call_list) status = ncl_maclist_init ();
	p1 = NCL_macro_call_list;
	if (!p1) status = ncl_maclist_init ();
	if (p1 != UU_NULL) 
		p1 = (struct NCL_macro_call_data *) uu_lsinsrt((char*)p1,n1);
	if (p1 != UU_NULL)
	{
		cstr = UM_cstr_of_f77_str(f77mlab);
		i = 0;
		while (*cstr != ' ' && i<NCL_MAX_LABEL-1) p1->macnam[i++] = *cstr++;
		p1->macnam[i] = '\0';
		p1->macptr = (char *)NCL_cur_macro_list_ptr;
		p1->tmpdkey = (struct NCL_macro_datakey *)UU_NULL;
		NCL_hld_macro_list_ptr = NCL_cur_macro_list_ptr;
	}
	return (status);
}

void addtmpdata(indx, key)
short *indx;
int *key;
{
	int n1,n2;
	struct NCL_macro_call_data *p1;
	struct NCL_macro_datakey *datapt;
	int status = UU_SUCCESS;

	p1 = NCL_macro_call_list;
	if (p1==NULL)
		return;
	if (p1 != UU_NULL)
		p1 = (struct NCL_macro_call_data *) uu_lsnext((char*)p1);
	if (p1==NULL)
		return;
	datapt = (struct NCL_macro_datakey*) p1->tmpdkey;
	if (datapt==UU_NULL)
	{
		p1->tmpdkey = (struct NCL_macro_datakey *) uu_lsnew();
		datapt = (struct NCL_macro_datakey*) p1->tmpdkey;
	}
//	datapt->key = *key;
//	datapt->indx = *indx;
// 	datapt = (struct NCL_macro_datakey*)uu_lsinsrt((char*)datapt, sizeof(struct NCL_macro_datakey));
	if (datapt != UU_NULL)
	{
		n1 = sizeof(struct NCL_macro_datakey);
		n2 = sizeof(struct NCL_macro_datakey *);
		datapt = (struct NCL_macro_datakey *)uu_lsend(datapt);
		if (datapt == UU_NULL) datapt = (struct NCL_macro_datakey*) p1->tmpdkey;
		if (datapt != UU_NULL) 
		{
			datapt = (struct NCL_macro_call_data *)uu_lsinsrt((char*)datapt,n1);
			datapt->key = *key;
			datapt->indx = *indx;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : int mcldel ()
**       Delete the current macro parameter list.
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
mcldel ()
{
	int status = UU_SUCCESS;

	if (NCL_hld_macro_list_ptr != UU_NULL) uu_lsdel(NCL_hld_macro_list_ptr);
	NCL_hld_macro_list_ptr = NCL_cur_macro_list_ptr = UU_NULL;

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_del_maclist (ptr)
**       Delete a macro parameter list.
**    PARAMETERS
**       INPUT  :
**          ptr      - Pointer to macro list.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_del_maclist (ptr)
char *ptr;
{
	int status = UU_SUCCESS;

	if (ptr != UU_NULL) uu_lsdel(ptr);

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_set_macptr (ptr)
**       Set the current macro parameter list pointer NCL_cur_macro_list_ptr.
**    PARAMETERS
**       INPUT  :
**          ptr      - Pointer to macro list.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_set_macptr (ptr)
char *ptr;
{
	int status = UU_SUCCESS;

	NCL_cur_macro_list_ptr = (struct NCL_macro_list_data *) ptr;

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : char *ncl_get_macptr ()
**       Return the current macro list pointer NCL_cur_macro_list_ptr.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         NCL_cur_macro_list_ptr
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char *ncl_get_macptr ()
{
	return ((char *)NCL_cur_macro_list_ptr);
}
/*********************************************************************
**    E_FUNCTION     : void mclspt ()
**       Returns the current macro list pointer NCL_cur_macro_list_ptr.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          ptr    = Variable to receive NCL_cur_macro_list_ptr.
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void mclspt(ptr)
char **ptr;
{
	*ptr = ncl_get_macptr();
}

/*********************************************************************
**    E_FUNCTION     : void mclspt ()
**       Sets the current macro list pointer NCL_cur_macro_list_ptr.
**    PARAMETERS
**       INPUT  :
**          ptr    = New NCL_cur_macro_list_ptr.
**       OUTPUT :
**          none
**    RETURNS      :
**         none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void mclrpt(ptr)
char **ptr;
{
	ncl_set_macptr(*ptr);
}

/*********************************************************************
**    E_FUNCTION     : int mclfnd (f77plab, psub, ifnd, iclas, ietype, aval,
**                                  nclkey)
**       Find a macro parameter in the current macro parameter list.
**    PARAMETERS
**       INPUT  :
**          f77plab  - Macro parameter label.
**          psub     - Macro parameter subscript.
**       OUTPUT :
**          ifnd     - =0 not found
**                   - =1 no default.
**                   - =2 default value is name.
**                     =3 default value is scalar value.
**          iclas    - NCL type of parameter value.
**          ietype   - NCL sub type of parameter value.
**          aval     - Macro parameter associated word (or value if scalar).
**          f77mtok  - Macro parameter token.
**          msub     - Macro parameter subscript.
**          nclkey   - Key of dummy scalar created when mac param
*                      is assigned to a scalar variable.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
mclfnd (f77plab, psub, ifnd, iclas, ietype, aval, f77mtok, msub, nclkey)
UM_f77_str_ptr f77plab;
UM_int4 *psub;
UM_int2 *ifnd;
UM_int2 *iclas, *ietype;
UM_real8 *aval;
UM_f77_str_ptr f77mtok;
UM_int4 *msub;
UM_int4 *nclkey;
{
	int status = UU_FAILURE;
	int i;
	char label[NCL_MAX_LABEL+1];
	char *cstr;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_call_data *p2;
	struct NCL_macro_define_data *p3;
	struct NCL_scalar_rec sclr;

	*ifnd = 0;
	cstr = UM_cstr_of_f77_str(f77plab);
	i = 0;
	while (*cstr != ' ' && i<NCL_MAX_LABEL) label[i++] = *cstr++;
	label[i] = '\0';

	*nclkey = 0;
	p1 = UU_NULL;
	p2 = NCL_macro_call_list;
	p3 = NULL;
	if (p2 != UU_NULL)
		p2 = (struct NCL_macro_call_data *) uu_lsnext(p2);
	if (p2 != UU_NULL)
		p1 = (struct NCL_macro_list_data *) p2->macptr;
	if (p1 == UU_NULL) p1 = NCL_cur_macro_list_ptr;
/*
......the first in macro list linklist is macro definantion, 
......then followed by macro pararmeter data
*/
	if (p1 != UU_NULL) p3 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p3!=NULL) 
	{
		p1 = (struct NCL_macro_list_data *) uu_lsnext(p3);
		while (p1 != UU_NULL)
		{
			if ((!strcmp(label, p1->plab) && p1->psub == *psub))
			{
				*ifnd = p1->iflg;
				cstr = UM_cstr_of_f77_str(f77mtok);
				if (p1->iclas == 0)
				{
					*iclas = NCLI_IDENT;
					*ietype = NCLI_MPARM;
					*cstr = ' ';
					*nclkey = 0;
				}
				else
				{
					*iclas = p1->iclas;
					*ietype = p1->ietype;
					*aval = p1->avalue;
					i = 0;
					while (p1->alab[i] != '\0' && i<NCL_MAX_LABEL)
						*cstr++ = p1->alab[i++];
					while (i<NCL_MAX_LABEL)
					{
						*cstr++ = ' ';
						i++;
					}
					*msub = p1->asub;
					*nclkey = p1->keyscl;
					if (p1->keyscl)
					{
						sclr.key = p1->keyscl;
						ur_retrieve_data_fixed(&sclr);
						*aval = sclr.scalar_value;
					}
				}
				NCL_hld_mac_param_ptr = p1;
				status = UU_SUCCESS;
				break;
			}
			else
			{
				p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
				if (p1 == UU_NULL && p2 != UU_NULL)
				{
					p2 = (struct NCL_macro_call_data *) uu_lsnext(p2);
					if (p2 != UU_NULL)
					{
						p1 = (struct NCL_macro_list_data *) p2->macptr;
						if (p1 != UU_NULL)
						{
/*
.....replaced
.....p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
.....to get next p1  from the macro list data from the macro define data
*/
						 p3 = (struct NCL_macro_define_data *) uu_lsnext(p1);					
						if(p3 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p3);
						}
					}
				}
			}
		}
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int mclsav (f77alab, asub, val, iclas, ietype)
**       Save macro parameters for the current macro parameter pointer
**       NCL_cur_mac_param_ptr.
**    PARAMETERS
**       INPUT  :
**          f77alab  - Macro parameter label.
**          asub     - Macro parameter subscript.
**          val      - Macro parameter associated word (value if scalar).
**          iclas    - NCL type of parameter value.
**          ietype   - NCL sub type of parameter value.
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
mclsav (f77alab, asub, val, iclas, ietype)
UM_f77_str_ptr f77alab;
UM_int4 *asub;
UM_real8 *val;
UM_int2 *iclas, *ietype;
{
	int status = UU_SUCCESS;
	int i;
	char *cstr;
	struct NCL_macro_list_data *p1;

	p1 = NCL_cur_mac_param_ptr;
	cstr = UM_cstr_of_f77_str(f77alab);
	i = 0;
	while (*cstr != ' ' && i<NCL_MAX_LABEL) p1->alab[i++] = *cstr++;
	p1->alab[i] = '\0';
	p1->asub   = *asub;
	p1->avalue = *val;
	p1->keyscl = 0;
	if (*iclas == 3 || *iclas == 4)
	{
		status = ncl_create_mparm_scalar (p1);
	}
	else
	{
		p1->iclas  = *iclas;
		p1->ietype = *ietype;
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int mclchk (iflg)
**       Check if all macro parameters have been set. Set values to default
**       if any, or return error if no default value.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          irslt    - =0 All set.
**                   - =1 All not set.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
mclchk (irslt)
UM_int2 *irslt;
{
	int status = UU_SUCCESS;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;

	*irslt = 0;
	p1 = NCL_hld_macro_list_ptr;
/*
......the first in macro list linklist is macro definantion, 
......then followed by macro pararmeter data
*/
	p2 = UU_NULL;
	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p2);
	while (p1 != UU_NULL)
	{
		if (!p1->iclas)
		{
			if (p1->iflg > 1)
			{
				strcpy (p1->alab, p1->dlab);
				p1->asub = p1->dsub;
				p1->avalue = p1->dvalue;
				p1->iclas  = p1->dclas;
				p1->ietype = p1->detype;
				if (p1->dclas == 3 || p1->dclas == 4)
				{
					status = ncl_create_mparm_scalar (p1);
				}
			}
			else
			{
				*irslt = 1;
				status = UU_FAILURE;
				break;
			}
		}
		p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
	}
/*
..... Set the current macro list pointer to the macro just being called in case it was
..... changed because one of the macro parameters was a macro.
*/
	NCL_cur_macro_list_ptr = NCL_hld_macro_list_ptr;

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int mclrst ()
**       Set all parameters in the current list to unused and delete
**       the last macro from the macro call list.
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
mclrst ()
{
	int status = UU_SUCCESS;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_call_data *p2;
	struct NCL_macro_define_data *p3;

	p1 = NCL_cur_macro_list_ptr;
/*
.....delete local  macro variable
*/
	ncl_delcur_macvar();
/*
......the first in macro list linklist is macro definantion, 
......then followed by macro pararmeter data
*/
	if (p1 != UU_NULL) p3 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p3) 
	{
		p1 = (struct NCL_macro_list_data *) uu_lsnext(p3);
		while (p1)
		{
			p1->iclas = 0;
			if (p1->keyscl)
			{
				uc_delete ((UU_KEY_ID)p1->keyscl);
				p1->keyscl = 0;
			}
			p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
		}
	}

	p2 = NCL_macro_call_list;
	if (p2 != UU_NULL)
		p2 = (struct NCL_macro_call_data *) uu_lsnext(p2);
	if (p2 != UU_NULL)
		p2 = (struct NCL_macro_call_data *) uu_lsdele(p2);
/*
.....reset back the current mac list pointer
.....Yurong 6/30/05
*/
	if (NCL_macro_call_list != UU_NULL)
	{
		p2 = (struct NCL_macro_call_data *) uu_lsnext(NCL_macro_call_list);
		if (p2!=NULL)
			NCL_cur_macro_list_ptr = (struct NCL_macro_list_data *) (p2->macptr);
/*
.....if the p2==NULL, we need set NCL_cur_macro_list_ptr = NULL;
*/
		else
			NCL_cur_macro_list_ptr = UU_NULL;
	}
	return (status); 
}
/*********************************************************************
**    E_FUNCTION     : int mclprv (f77plab, psub, ifnd, iclas, ietype, aval)
**       Find a macro parameter in the previous (calling)  macro parameter list.
**    PARAMETERS
**       INPUT  :
**          f77plab  - Macro parameter label.
**          psub     - Macro parameter subscript.
**       OUTPUT :
**          ifnd     - =0 not found
**                   - =1 no default.
**                   - =2 default value is name.
**                     =3 default value is scalar value.
**          iclas    - NCL type of parameter value.
**          ietype   - NCL sub type of parameter value.
**          aval     - Macro parameter associated word (or value if scalar).
**          f77mtok  - Macro parameter token.
**          msub     - Macro parameter subsrcipt.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
mclprv (f77plab, psub, ifnd, iclas, ietype, aval, f77mtok, msub)
UM_f77_str_ptr f77plab;
UM_int4 *psub;
UM_int2 *ifnd;
UM_int2 *iclas, *ietype;
UM_real8 *aval;
UM_f77_str_ptr f77mtok;
UM_int4 *msub;
{
	int status = UU_FAILURE;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_list_data *prmptr;
	struct NCL_macro_call_data *p2;
	UM_int4 nclkey;

	*ifnd = 0;

	p2 = NCL_macro_call_list;
	if (p2 != UU_NULL)
		p2 = (struct NCL_macro_call_data *) uu_lsnext(p2);
	if (p2 != UU_NULL)
		p2 = (struct NCL_macro_call_data *) uu_lsnext(p2);
	if (p2 != UU_NULL)
	{
		prmptr = NCL_cur_mac_param_ptr;
		p1 = NCL_cur_macro_list_ptr;
		NCL_cur_macro_list_ptr = (struct NCL_macro_list_data *) p2->macptr;
/*
.....Added missing parameter nclkey - ASF 12/17/13.
*/
		status = mclfnd (f77plab, psub, ifnd, iclas, ietype, aval, f77mtok, msub,
								&nclkey);

		NCL_cur_macro_list_ptr = p1;
		NCL_cur_mac_param_ptr = prmptr;
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_maclist_init ()
**       Initialize the macro call list.
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
ncl_maclist_init ()
{
	int status = UU_SUCCESS;

	if (NCL_macro_call_list == UU_NULL)
	{
 		NCL_macro_call_list = (struct NCL_macro_call_data *) uu_lsnew();
		if (NCL_macro_call_list == UU_NULL) status = UU_FAILURE;
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_maclist_done ()
**       Delete the macro call list.
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
ncl_maclist_done ()
{
	void delm_tdata();
	int status = UU_SUCCESS;

	if (NCL_macro_call_list)
	{
		delm_tdata();
		uu_lsdel (NCL_macro_call_list);
		NCL_macro_call_list = UU_NULL;
	}
	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int mclpst ()
**       Set the cur macro parameter pointer.
**    PARAMETERS
**       INPUT  : 
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
mclpst ()
{
	int status = UU_SUCCESS;

	NCL_cur_mac_param_ptr = NCL_hld_mac_param_ptr;

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_get_next_called_macro (nam)
**       Return the name of the first macro on the macro call list.
**    PARAMETERS
**       INPUT  : 
**          none
**       OUTPUT :
**          nam     : Name of macro.
**    RETURNS      :
**         UU_TRUE iff name returned; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_next_called_macro (nam)
char **nam;
{
	int status = UU_FALSE;
	struct NCL_macro_call_data *p2;

	p2 = NCL_macro_call_list;
	if (p2 != UU_NULL)
		p2 = (struct NCL_macro_call_data *) uu_lsnext(p2);
	if (p2 != UU_NULL)
	{
		*nam = p2->macnam;
		status = UU_TRUE;
	}

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_del_first_called_macro ()
**       Delete the first macro on the macro call list.
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
ncl_del_first_called_macro ()
{
	int status = UU_SUCCESS;
	struct NCL_macro_call_data *p2;

	p2 = NCL_macro_call_list;
	if (p2 != UU_NULL)
		p2 = (struct NCL_macro_call_data *) uu_lsnext(p2);
	if (p2 != UU_NULL)
		p2 = (struct NCL_macro_call_data *) uu_lsdele(p2);

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_create_mparm_scalar (p1)
**       Create a dummy scalar for a scalar macro parameter.
**    PARAMETERS
**       INPUT  : 
**          p1     - macro parameter pointer.
**       OUTPUT :
**          p1     - macro parameter pointer updated.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_create_mparm_scalar (p1)
struct NCL_macro_list_data *p1;
{
	int status;
	UM_int2 i2flg;
	struct NCL_scalar_rec sclr;

	sclr.key = 0;
	sclr.rel_num = NCL_SCALAR_REL;
	sclr.scalar_value = p1->avalue;
	strcpy(sclr.descript, p1->prompt);
	i2flg = 1;
	stunlb(&i2flg);
	status = ncl_create_entity (&sclr, (int)NCLI_SCALAR);
	stunlb(&i2flg);
	if (status == UU_SUCCESS)
	{
		p1->iclas  = 2;
		p1->ietype = NCLI_SCALAR;
		p1->keyscl = sclr.key;
		strcpy (p1->alab, "@UN ");
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_next_maclist(p1, indx)
**       Returns the next Macro argument in the Macro parameter list.
**    PARAMETERS
**       INPUT  :
**          p1       - Pointer to current list structure.  Should be
**                     set to UU_NULL on first call.
**			indx:	- indx of parameter
**       OUTPUT :
**          p1       - Pointer to current macro list structure.
**                     Returns UU_NULL if there are no more Macro arguments
**                     on the list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_next_maclist(p1, indx)
struct NCL_macro_list_data **p1;
int indx;
{
	struct NCL_macro_define_data *p2;
/*
.....First time here
.....Get first list item
*/
	if (indx<1) 
	{
		*p1 = NULL;
		return;
	}
	if (indx==1)
	{
  		p2 = (struct NCL_macro_define_data *)uu_lsnext(NCL_cur_macro_list_ptr);
		if (p2)
			*p1 = (struct NCL_macro_list_data *)uu_lsnext(p2);
	}
/*
.....Return next list structure
*/
	else
	{
  		*p1 = (struct NCL_macro_list_data *)uu_lsnext(*p1);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_store_maclist_data(p1)
**       Stores the next Macro argument in the Macro parameter list.
**    PARAMETERS
**       INPUT  :
**          p1       - Pointer to current list structure.
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_store_maclist_data(p1)
struct NCL_macro_list_data *p1;
{
	int n1;
	int status = UU_SUCCESS;
	struct NCL_macro_list_data *p2;
/*
.....Allocate next block
*/
	n1 = sizeof(struct NCL_macro_list_data);
	p2 = NCL_hld_macro_list_ptr;
	if (p2 != UU_NULL) p2 = (struct NCL_macro_list_data *) uu_lsend (p2);
	if (p2 == UU_NULL) p2 = NCL_hld_macro_list_ptr;
	if (p2 != UU_NULL) p2 = (struct NCL_macro_list_data *) uu_lsinsrt((char*)p2,n1);
	if (p2 == UU_NULL)
		status = UU_FAILURE;
/*
.....Store structure
*/
	else
		*p2 = *p1;
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_macinfo(p1)
**       Returns the current Macro info NCL_macro_define_data.
**    PARAMETERS
**       INPUT  :
**          p1       - Pointer to current NCL_macro_define_data structure.  
**                     
**       OUTPUT :
**          p1       - Pointer to current NCL_macro_define_data structure.  
**                     
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_macinfo(p1)
struct NCL_macro_define_data **p1;
{
	if (NCL_cur_macro_list_ptr==NULL)
		*p1 = NULL;
	else
  		*p1 = (struct NCL_macro_define_data *)uu_lsnext(NCL_cur_macro_list_ptr);
}

/*********************************************************************
**    E_FUNCTION     : ncl_store_macinfo(p1)
**       Stores the current Macro info
**    PARAMETERS
**       INPUT  :
**          p1       - current NCL_macro_define_data structure.
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_store_macinfo(p1)
struct NCL_macro_define_data *p1;
{
	struct NCL_macro_define_data *p2;
	if (NCL_cur_macro_list_ptr==NULL)
		return UU_FAILURE;
	else
	{
  		p2 = (struct NCL_macro_define_data *)uu_lsnext(NCL_cur_macro_list_ptr);
		if (p2==NULL)
			return UU_FAILURE;
/*
.....Store structure
*/
		else
			*p2 = *p1;
	}
	return UU_SUCCESS;
}
/*********************************************************************
**    E_FUNCTION     : ncl_get_next_macall(p1)
**       Returns the next entry in the Macro call list.
**    PARAMETERS
**       INPUT  :
**          p1       - Pointer to current list structure.  Should be
**                     set to UU_NULL on first call.
**       OUTPUT :
**          p1       - Pointer to current macro list structure.
**                     Returns UU_NULL if there are no more Macro on the list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_next_macall(p1)
struct NCL_macro_call_data **p1;
{
/*
.....First time here
.....Get first list item
*/
	if (*p1 == UU_NULL) *p1 = NCL_macro_call_list;
/*
.....Return next list structure
*/
  	if (*p1 != UU_NULL) *p1 = (struct NCL_macro_call_data *)uu_lsnext(*p1);
}

/*********************************************************************
**    E_FUNCTION     : ncl_store_macall_data(p1)
**       Stores the next Macro call in the Macro call list.
**    PARAMETERS
**       INPUT  :
**          p1       - Pointer to current list structure.
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_store_macall_data(p1)
struct NCL_macro_call_data *p1;
{
	int status = UU_SUCCESS;
	int n1;
	struct NCL_macro_call_data *p2;
/*
.....Allocate next block
*/
	n1 = sizeof(struct NCL_macro_call_data);
	if (NCL_macro_call_list == UU_NULL) status = ncl_maclist_init();
	p2 = NCL_macro_call_list;
	if (p2 != UU_NULL)
	{
		p2 = (struct NCL_macro_call_data *)uu_lsend(p2);
		if (p2 != UU_NULL) p2 = (struct NCL_macro_call_data *)uu_lsinsrt((char*)p2,n1);
	}
	if (p2 == UU_NULL)
		status = UU_FAILURE;
/*
.....Store structure
*/
	else
		*p2 = *p1;
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_maclist_ptrs(p1)
**       Returns the active Macro list pointers.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          p1       - Active Macro list Pointers.$
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_maclist_ptrs(p1)
struct NCL_macro_list_data *p1[];
{
/*
.....Return pointers
*/
	p1[0] = NCL_hld_macro_list_ptr;
	p1[1] = NCL_cur_macro_list_ptr;
	p1[2] = NCL_hld_mac_param_ptr;
	p1[3] = NCL_cur_mac_param_ptr;
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_maclist_ptrs(p1)
**       Sets the active Macro list pointers.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          p1       - Active Macro list Pointers.$
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_maclist_ptrs(p1)
struct NCL_macro_list_data *p1[];
{
/*
.....Return pointers
*/
	NCL_hld_macro_list_ptr = p1[0];
	NCL_cur_macro_list_ptr = p1[1];
	NCL_hld_mac_param_ptr = p1[2];
	NCL_cur_mac_param_ptr = p1[3];
}

/*********************************************************************
**    E_FUNCTION     : ncl_storehd (maclin, next, termln, mode, pr_rec,  pr_ele, callin)
**       Store header informtion in the current Macro definition structure.
**    PARAMETERS
**       INPUT  : 
**				maclin:  macro start line
**				next:	this will contain the line of the next line after
**					      the macro statement
**				termln: line number of termac statement
**				mode:	program state at time of call
**				pr_rec: Record number of Macro description
**				pr_ele: Element within record of Macro description
**				callin: callin line
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_storehd (maclin, next, termln, mode, pr_rec,  pr_ele, callin)
UM_int4 *maclin, *next, *termln;
UM_int2 *mode, *pr_rec,  *pr_ele;
UM_int4 *callin;
{
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;
	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL)
	{
		p2->maclin = *maclin;
		p2->next = *next;
		p2->termln = *termln;
		p2->mode = *mode;
		p2->pr_rec = *pr_rec;
		p2->pr_ele = *pr_ele;
		p2->callin = *callin;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_getmc_hd (maclin, next, termln, mode, pr_rec,  pr_ele, callin)
**       Get header informtion from the current Macro definition structure.
**    PARAMETERS
**       INPUT  :  None
**				
**       OUTPUT :
**				maclin:  macro start line
**				next:	this will contain the line of the next line after
**					      the macro statement
**				termln: line number of termac statement
**				mode:	program state at time of call
**				pr_rec: Record number of Macro description
**				pr_ele: Element within record of Macro description
**				callin: callin line
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getmc_hd (maclin, next, termln, mode, pr_rec,  pr_ele, callin)
UM_int4 *maclin, *next, *termln;
UM_int2 *mode, *pr_rec,  *pr_ele;
UM_int4 *callin;
{
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;
	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL)
	{
		*maclin = p2->maclin;
		*next = p2->next;
		*termln = p2->termln;
		*mode = p2->mode;
		*pr_rec = p2->pr_rec;
		*pr_ele= p2->pr_ele;
		*callin = p2->callin;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_getmac_parms0(classname, outflag, dispflag, prmpt)
**       Get macro definition informtion from the current Macro definition structure.
**    PARAMETERS
**       INPUT  :  None
**				
**       OUTPUT :
**				classname:  macro class name
**				outflag:	output flag
**					    0: OUT		1: OMIT
**				dispflag: display flag
**						0: DEFALT	1: RETAIN
**				prmpt:	Macro description
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getmac_parms0(classname, outflag, dispflag, prmpt)
char *classname, *prmpt;
int *outflag, *dispflag;
{
	int i;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;
	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL)
	{
		strncpy(classname, p2->classname, 20);
		classname[20] = '\0';
/*
.....remove trailing space
*/
		i = strlen(classname)-1;
		while ((i>=0)&&(classname[i]==' ')) 
			i--;
		classname[i+1] = '\0';
		*outflag = p2->outflag;
		*dispflag = p2->dispflag;
		strncpy(prmpt, p2->prompt, 40);
		prmpt[40] = '\0';
/*
.....remove trailing space
*/
		i = strlen(prmpt)-1;
		while ((i>=0)&&(prmpt[i]==' ')) 
			i--;
		prmpt[i+1] = '\0';
		return;
	}
}
/*********************************************************************
**    E_FUNCTION     : ncl_getmac_parms(k, label, clas, prmpt, counter, wrdlist, geomflag, min, max, prsvalue, 
**		lenvalue, substr, pflag)
**		Get parameter informtion from the current Macro list data structure.
**
**    PARAMETERS
**       INPUT  :  k: index of the paramter
**				
**       OUTPUT :
**				label:  macro paramter label
**				clas:	type of the parameter
**				prmpt:  paramter prompt
**				counter:	number of word/geom in word/geom list
**				wrdlist: word/geom list
**				geomflag: 1: wrdlist contain all geometry
**							0: wrdlist contain word but not all geometry
**				min, max: min/max value of the paramter
**				prsvalue, lenvalue:
**				substr:		sub-string format:
**						0: RETAIN	1: LABEL	2: NUM
**				pflag:		pick flag:
**						0: NOW		1: NEXT
**				pclr:   color of select color
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getmac_parms(k, label, clas, prmpt, counter, wrdlist, geomflag, min, max, prsvalue, 
		lenvalue, substr, pflag, psel, pclr)
char *label, *prmpt, *wrdlist;
UM_real8 *min, *max;
UM_int2 *clas, *prsvalue, *lenvalue, *geomflag;
int *k, *counter, *substr, *pflag, *psel, *pclr;
{
	int count, i;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;
	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p2);
	count = *k;
	while (count>1)
	{
		if (p1 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
		count--;
	}
	if (p1 != UU_NULL)
	{
		if (p1->psub!=0)
			sprintf(label, "%s(%d)", p1->plab, p1->psub);
		else
			strcpy(label, p1->plab);

		strncpy(prmpt, p1->prompt, 40);
		prmpt[40] = '\0';
/*
.....remove trailing space
*/
		i = strlen(prmpt)-1;
		while ((i>=0)&&(prmpt[i]==' ')) 
			i--;
		prmpt[i+1] = '\0';
		*counter = p1->count;
		strncpy(wrdlist, p1->wrdlist,480);
		wrdlist[481] = '\0';
/*
.....remove trailing space
*/
		i = strlen(wrdlist)-1;
		while ((i>=0)&&(wrdlist[i]==' ')) 
			i--;
		wrdlist[i+1] = '\0';
		*min = p1->min;
		*max = p1->max;
		*prsvalue = p1->prsvalue;
		*lenvalue = p1->lenvalue;
		*substr = p1->substr;
		*pflag = p1->pflag;
		*clas = p1->dclas;
		*geomflag = p1->geomflag;
		*psel = p1->psel;
		*pclr = p1->pclr;
	}
}


/*********************************************************************
**    E_FUNCTION     : ncl_putmac_parms0(classname, len1, outflag, dispflag, prmpt, len2)
**       Get macro definition informtion from the current Macro definition structure.
**    PARAMETERS
**       INPUT  :  
**				classname:  macro class name
**				len1:		len of classname
**				outflag:	output flag
**					    0: OUT		1: OMIT
**				dispflag: display flag
**						0: DEFALT	1: RETAIN
**				prmpt:	Macro description
**				len2:		len of prmpt
**				
**       OUTPUT :None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_putmac_parms0(classname, len1, outflag, dispflag,
		prmpt, len2)
char *classname, *prmpt;
UM_int2 *len1, *len2;
int *outflag, *dispflag;
{
	int nc;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;
	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL)
	{
		classname[*len1] = '\0';
		if (*len1<=20)
			nc = *len1;
		else
			nc = 20;
		strncpy(p2->classname, classname, nc);
		p2->classname[nc] = '\0';
/*
.....save all class as upper case
*/
		ul_to_upper(p2->classname);
		p2->outflag = *outflag;
		p2->dispflag = *dispflag;
		prmpt[*len2] = '\0';
		if (*len2<=40)
			nc = *len2;
		else
			nc = 40;
		strncpy(p2->prompt, prmpt, nc);
		p2->prompt[nc] = '\0';
		p2->outflag = *outflag;
		return;
	}
}
/*********************************************************************
**    E_FUNCTION     : ncl_putmac_parms(k, label, clas, prmpt, counter, wrdlist, geomflag, min, max, prsvalue, 
**		lenvalue, substr, pflag)
**		Put parameter informtion into the current Macro list data structure.
**
**    PARAMETERS
**       INPUT  :  k: index of the paramter
**				label:  macro paramter label
**				clas:	type of the parameter
**				prmpt:  paramter prompt
**				counter:	number of word/geom in word/geom list
**				wrdlist: word/geom list
**				geomflag: 1: wrdlist contain all geometry
**							0: wrdlist contain word but not all geometry
**				min, max: min/max value of the paramter
**				prsvalue, lenvalue:
**				substr:		sub-string format:
**						0: RETAIN	1: LABEL	2: NUM
**				pflag:		pick flag:
**						0: NOW		1: NEXT
**				
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_putmac_parms(k, clas, prmpt, len1, counter, wrdlist, len2, geomflag, min, max, prsvalue, 
		lenvalue,substr, pflag, psel, pclr)
char *prmpt, *wrdlist;
UM_real8 *min, *max;
UM_int2 *clas, *prsvalue, *lenvalue, *len1, *len2, *geomflag;
int *k, *counter, *substr, *pflag, *psel, *pclr;
{
	int count, nc, spc;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;
	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p2);
	count = *k;
	while (count>1)
	{
		if (p1 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
		count--;
	}
	if (p1 != UU_NULL)
	{
		if (*len1<=40)
			nc = *len1;
		else
			nc = 40;
		strncpy(p1->prompt, prmpt, 40);
		p1->prompt[nc] = '\0';
		p1->count = *counter;
		if (*len2<=480)
			nc = *len2;
		else
			nc = 480;
		strncpy(p1->wrdlist, wrdlist, nc);	
/*		p1->wrdlist[481] = '\0'; */
/*
.....In the wordlist after the last non space character of the last word
.....pad the remainder of the 24 charactes with spaces.
*/
		spc = (24 * p1->count) - nc;
		while(spc>0) 
		{
			p1->wrdlist[nc+spc-1]=' ';
			spc--;
		}
/*
..... If the number of words are less than 20 add a null character after
..... the 24 characters of the last word.
*/
		if(p1->count!=20)
			p1->wrdlist[24 * p1->count]='\0';

		p1->min = *min;
		p1->max = *max;
		p1->prsvalue = *prsvalue;
		p1->lenvalue = *lenvalue;
		p1->substr = *substr;
		p1->pflag = *pflag;
		p1->pclas = *clas;
		p1->geomflag = *geomflag;
		p1->psel = *psel;
		p1->pclr = *pclr;
	}
}
/*********************************************************************
**    E_FUNCTION     : ncl_getmac_plabel(k, label, len)
**		Get parameter label from the current Macro list data structure.
**
**    PARAMETERS
**       INPUT  :  k: index of the paramter
**				
**       OUTPUT :
**				label:  macro paramter label
**				len:	Length of the label.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getmac_plabel(k, label, len)
char *label;
int *k, *len;
{
	int count;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;

	*len = 0;
	if (*k<1) return;

	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p2);
	count = *k;
	while (count>1)
	{
		if (p1 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
		count--;
	}
	if (p1 != UU_NULL)
	{
		if (p1->psub>0)
			sprintf(label, "%s(%d)", p1->plab, p1->psub);
		else
			sprintf(label, "%s", p1->plab);
		*len = strlen(label);
	}
}
/*********************************************************************
**    E_FUNCTION     : ncl_getmac_pnum(num)
**		Get parameter number of the current macro.
**
**    PARAMETERS
**       INPUT  :  None.
**				
**       OUTPUT :
**				num:  parameter number of the current macro
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getmac_pnum(num)
int *num;
{
	int count;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;

	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p2);
	count = 10000;
	if (p1==NULL)
	{
		*num = 0;
		return;
	}
	*num = 1;
	while (count!=0)
	{
		if (p1 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
		count--;
		if (p1 != UU_NULL) 
			(*num)++;
		else
			break;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_getmac_values(k, iflg, psub, dlab, dsub, dvalue, pclas, detype)
**		Get parameter value data of the current macro.
**
**    PARAMETERS
**       INPUT  :  k: index of the paramter
**				
**       OUTPUT :
**				iflg:
**				psub:
**				dlab:
**				dsub:
**				dvalue:
**				pclas:
**				detype:
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getmac_values(k, iflg, psub, dlab, dsub, dvalue, pclas, detype)		
int *k;
UM_int2 *iflg, *detype, *pclas;
UM_int4 *dsub, *psub;
UM_real8 *dvalue;
char *dlab;
{
	int count;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;
	if (*k<1) return;
	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p2);
	count = *k;
	while (count>1)
	{
		if (p1 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
		count--;
	}
	if (p1 != UU_NULL)
	{
		*iflg = p1->iflg;
		*psub = p1->psub;
		*dsub = p1->dsub;
		*dvalue = p1->dvalue;
		*pclas = p1->pclas;
		*detype = p1->detype;
		strcpy(dlab, p1->dlab);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_getmac_desp(macname, classname, outflag, dispflag, prmpt)
**       Get macro definition informtion.
**    PARAMETERS
**       INPUT  :  macname: macro name
**				
**       OUTPUT :
**				classname:  macro class name
**				outflag:	output flag
**					    0: OUT		1: OMIT
**				dispflag: display flag
**						0: DEFALT	1: RETAIN
**				prmpt:	Macro description
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getmac_desp(macname, classname, outflag, dispflag, prmpt)
char *macname, *classname, *prmpt;
int *outflag, *dispflag;
{
	char *savptr;
	savptr = ncl_get_macptr();
	ncl_set_curmac (macname);
	ncl_getmac_parms0(classname, outflag, dispflag, prmpt);
	ncl_set_macptr(savptr);
}
/*********************************************************************
**    E_FUNCTION     : ncl_setmac_desp(macname, classname, outflag, dispflag, prmpt)
**       Set macro definition informtion.
**    PARAMETERS
**       INPUT  :  macname: macro name
**				
**       OUTPUT :
**				classname:  macro class name
**				outflag:	output flag
**					    0: OUT		1: OMIT
**				dispflag: display flag
**						0: DEFALT	1: RETAIN
**				prmpt:	Macro description
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_setmac_desp(macname, classname, outflag, dispflag, prmpt)
char *macname, *classname, *prmpt;
int *outflag, *dispflag;
{
	char *savptr;
	UM_int2 len1, len2;
	savptr = ncl_get_macptr();
	ncl_set_curmac (macname);
	len1 = strlen (classname);
	len2 = strlen(prmpt);
	ncl_putmac_parms0(classname, &len1, outflag, dispflag,
		prmpt, &len2);
	ncl_set_macptr(savptr);
}

/*********************************************************************
**    E_FUNCTION     : ncl_setmac_rvalue(k, rvalue, rlab, rsub, rtype)
**       Set macro paramter reserve value.
**    PARAMETERS
**       INPUT  :  k: index of the macro parameter
**				rvalue:  reserve value
**				rtype  = 0 reserve value type is the same as default value type
**                     =1 reserve value is name.
**                     =2 reserve value is scalar value.
**				
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_setmac_rvalue(k, rvalue, rlab, rsub, rtype)		
int k, rsub, rtype;
UM_real8 rvalue;
char *rlab;
{
	int count;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;
	if (k<1) return;
	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL) 
	{
/*
.....make the macro as retain value saved
*/
		p2->rvsaved = 1;
		p1 = (struct NCL_macro_list_data *) uu_lsnext(p2);
	}
	else
		return;
	count = k;
	while (count>1)
	{
		if (p1 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
		count--;
	}
	if (p1 != UU_NULL)
	{
		if (rlab!=NULL)
		{
			strcpy(p1->rlab, rlab);
			p1->rsub = rsub;
		}
		else
			p1->rvalue = rvalue;
		p1->rtype = rtype;
	}
}
/*********************************************************************
**    E_FUNCTION     : ncl_getmac_rvalue(k, rvalue, rlab, rsub, rtype)
**       Get macro paramter reserve value.
**		if there is no retain value saved, return failed
**    PARAMETERS
**       INPUT  :  k: index of the macro parameter
**				
**       OUTPUT :
**				rvalue:  reserve value
**				rtype  = 0 reserve value type is the same as default value type
**                     =1 reserve value is name.
**                     =2 reserve value is scalar value.
**    RETURNS      : if there is no retain value saved, return failed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_getmac_rvalue(k, rvalue, rlab, rsub, rtype)		
int k, *rsub, *rtype;
UM_real8 *rvalue;
char *rlab;
{
	int count;
	struct NCL_macro_list_data *p1;
	struct NCL_macro_define_data *p2;
	p1 = NCL_cur_macro_list_ptr;
	p2 = UU_NULL;
	if (k<1) return -1;
	if (p1 != UU_NULL) p2 = (struct NCL_macro_define_data *) uu_lsnext(p1);
	if (p2 != UU_NULL) 
	{
/*
.....make the macro as retain value saved
*/
		if (p2->rvsaved != 1)
			return -1;
		p1 = (struct NCL_macro_list_data *) uu_lsnext(p2);
	}
	else
		return -1;
	count = k;
	while (count>1)
	{
		if (p1 != UU_NULL) p1 = (struct NCL_macro_list_data *) uu_lsnext(p1);
		count--;
	}
	if (p1 != UU_NULL)
	{
		*rvalue = p1->rvalue;
		strcpy(rlab, p1->rlab);
		*rsub = p1->rsub;
		*rtype = p1->rtype;
	}
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : mclsetprv
**       set the current macro list pointer to the the previous (calling)  macro one
**		if there is no previous one do nothing
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void mclsetprv()
{
	struct NCL_macro_call_data *p2;

	p2 = NCL_macro_call_list;
	if (p2 != UU_NULL)
		p2 = (struct NCL_macro_call_data *) uu_lsnext(p2);
	if (p2 != UU_NULL)
		p2 = (struct NCL_macro_call_data *) uu_lsnext(p2);
	if (p2 != UU_NULL)
	{
		NCL_cur_macro_list_ptr = (struct NCL_macro_list_data *) p2->macptr;
	}
}

/*********************************************************************
**    E_FUNCTION     : nclf_getmac_callin(macnam,nc,callin)
**       Returns the line after the calling line for a Macro on the
**       call stack.
**    PARAMETERS
**       INPUT  :
**				macnam  = Name of Macro to get information about.
**				nc      = # of chars in 'macnam'.
**       OUTPUT :
**				callin  = Line number after CALL/macnam command.  Returns -1
**                    if the Macro is not found.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_getmac_callin(macnam,nc,callin)
char *macnam;
int *nc,*callin;
{
	char label[NCL_MAX_LABEL+1];
	struct NCL_macro_list_data *p1;
	struct NCL_macro_call_data *p2;
	struct NCL_macro_define_data *p3;
/*
.....Initialize routine
*/
	*callin = -1;
	strncpy(label,macnam,*nc);
	label[*nc] = '\0';
/*
.....Find requested Macro
*/
	p1 = UU_NULL;
	p2 = NCL_macro_call_list;
	p3 = UU_NULL;
	if (p2 == UU_NULL) goto done;
	do
	{
		p2 = (struct NCL_macro_call_data *)uu_lsnext(p2);
		if (p2 == UU_NULL) break;
		if (strcmp(p2->macnam,label) == 0)
		{
			p1 = (struct NCL_macro_list_data *)p2->macptr;
			if (p1 == UU_NULL) p1 = NCL_cur_macro_list_ptr;
			if (p1 != UU_NULL) p3 = (struct NCL_macro_define_data *)uu_lsnext(p1);
			if (p3 != UU_NULL) *callin = p3->callin;
			break;
		}
	} while (p2 != UU_NULL);
/*
.....End of routine
*/
done:;
	return;
}
void delm_tdata()
{
	int key;
	struct NCL_macro_call_data *p1;
	struct NCL_macro_datakey *datapt;

	if (NCL_macro_call_list==NULL)
	{
		return;
	}
	p1 = NCL_macro_call_list;
	if (p1 != UU_NULL)
		p1 = (struct NCL_macro_call_data *) uu_lsnext(p1);
	if (p1==NULL)
		return;
	datapt = (struct NCL_macro_datakey *) p1->tmpdkey;
	if (datapt != UU_NULL)
		datapt = (struct NCL_macro_datakey *) uu_lsnext(datapt);
	while (datapt != UU_NULL)
	{
		key = datapt->key;
		if (key!=-1)
		{
			dtdele(&key);
		}
		datapt = (struct NCL_macro_datakey *) uu_lsnext(datapt);
	}
}
