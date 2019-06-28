/*********************************************************************
**    NAME         :  nedatastmt.c
**       CONTAINS:  Routines to handle data statement.
**           int dtinit (nclkey)
**           int dtstor (nclkey, idx, ityp, ist, tv, nextyp)
**           int dtdele (nclkey)
**           int dtgetv (nclkey, ix, val, ivoc, nextyp)
**           int dtgtnw (nclkey, len)
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nedatastmt.c , 25.3
**    DATE AND TIME OF LAST MODIFICATION
**       08/01/17 , 13:42:12
*********************************************************************/

#include "usysdef.h"
#include "mfort.h"
#include "mdrel.h"
#include "nclfc.h"
#include "ncl.h"
#include "nccs.h"
#include "ulist.h"
#include "nclvx.h"

static struct NCL_datast_rec NCL_ds_curr_rec;

/*********************************************************************
**    E_FUNCTION     : int dtinit (nclkey)
**       Store the fixed part of a data statement entity.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          nclkey      - Unibase key of data statement.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
dtinit (nclkey)
UM_int4 *nclkey;
   {
   
   int status, ityp;
   struct NCL_datast_rec e1;

   ur_setup_data(NCL_DATAST_REL, &e1, sizeof(e1));
   ityp = NCLI_DATAST;
/*   e1.key = *nclkey;*/
   e1.rel_num = NCL_DATAST_REL;
   e1.nargs = 0;
   status = ncl_create_entity (&e1, ityp);
   NCL_ds_curr_rec.key = *nclkey = e1.key;

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int dtstor (nclkey, idx, ityp, ist, tv, nextyp,label, sub)
**       Store an element in a data statement list.
**    PARAMETERS   
**       INPUT  : 
**          nclkey   - key of data statement entity.
**          idx      - index into data statement elements.
**          ityp     - type of element.
**          ist      - subtype of element.
**          tv       - value of scalar
**         label      - label of element
**         sub      - sub indx of element
**       OUTPUT :  
**         none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
dtstor (nclkey, idx, ityp, ist, tv, nextyp,label, sub)
UM_int4 *nclkey,*sub;
UM_int2 *idx;
UM_int2 *ityp;
UM_int2 *ist;
UM_real8 *tv;
UM_int2 *nextyp;
char *label;
{
    
	int status, ix;
	UU_KEY_ID key;
	struct NCL_datael_rec ds;

	if (*ityp == 1)
	{
		ds.type = 1;
		ds.value = *ist;
	}
	else
	{
		if (*ityp == 2 && (*ist > 2)&&(*ist!=24))
			ds.type = 2;
		else if ((*ityp == 2) && (*ist==24))
		{
/*
......text variable treats differently
*/
			ds.type = *ist;
		}
		else
			ds.type = 0;
		ds.value = *tv;
	}
	ds.delim = *nextyp;
	if (label!=NULL)
	{
		strncpy(ds.label, label, 63);
		ds.label[63] = '\0';
		ds.isub = *sub;
	}
	else
	{
		ds.label[0] = '\0';
		ds.isub = 0;
	}
	key = *nclkey;
	ix = *idx;
	status = ur_update_data_varlist (key,1,&ds,ix,1);

	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int dtdele (nclkey)
**       Delete a data statement entity.
**    PARAMETERS   
**       INPUT  : 
**          nclkey        - Key of data statement entity to delete.
**       OUTPUT :  
**         none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
dtdele (nclkey)
UM_int4 *nclkey;
   {
   int status;

   status = dlgeom(nclkey);

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int dtgetv (nclkey, ix, val, ivoc, nextyp,label, sub)
**       Retrieve an element of a data statement.
**    PARAMETERS   
**       INPUT  :
**          nclkey  - index into data statement list
**          ix      - index of element in list
**       OUTPUT :  
**         val      - value of element
**         ivoc     - 0 iff element is scalar.
**                    1 iff element is vocabulary word.
**                    2 iff element is variable.
**                    24 iff element is text string.
**         nextyp   - delimeter following this element.
**         label      - label of element
**         sub      - sub indx of element
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
dtgetv (nclkey, ix, val, ivoc, nextyp,label, sub)
UM_int4 *nclkey, *sub;
UM_int2 *ix;
UM_real8 *val;
UM_int2 *ivoc;
UM_int2 *nextyp;
char *label;
   {
   
   int status = UU_SUCCESS;
   int lix,i,nc;
	UU_REAL rval;
   struct NCL_datael_rec *dsp;

   lix = *ix-1;
   *val = 0.0;
   *ivoc = 0;
   NCL_ds_curr_rec.key = *nclkey;
   status = ncl_retrieve_data_fixed (&NCL_ds_curr_rec);
   if (status == UU_SUCCESS && lix>=0 && lix<NCL_ds_curr_rec.no_datael)
     {
     dsp = NCL_ds_curr_rec.datael;
     *val = dsp[lix].value;
     *ivoc = dsp[lix].type;
     *nextyp = dsp[lix].delim;
     *sub = dsp[lix].isub;
/*
.....Store scalar value at time of definition
.....so that if the scalar is changed later
.....the value in the data statement does not change
.....This matches the way it worked in previous versions
.....Bobby - 10/24/14
*/
		if (dsp[lix].type == 0)
		{
			if (ul_to_reals(&rval,&nc,1,dsp[lix].label) == UU_SUCCESS)
			{
     			strncpy(label, dsp[lix].label, NCL_MAX_LABEL);
			}
			else
			{
				ncl_sprintf(label,&dsp[lix].value,1,8);
				for (i=strlen(label);i<NCL_MAX_LABEL;i++) label[i] = ' ';
			}
		}
/*
.....Copy variable/word label
*/
		else
			strncpy(label, dsp[lix].label, NCL_MAX_LABEL);
     }

   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int dtgtnw (nclkey, len)
**       Return the number of elements in a data statement entity.
**    PARAMETERS   
**       INPUT  :
**          nclkey  - Key of data statement entity.
**       OUTPUT :  
**         len      - Number of elements in the data statement entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
dtgtnw (nclkey, len)
UM_int4 *nclkey;
UM_int2 *len;
{

	int status = UU_SUCCESS;
	struct NCL_datast_rec e1;

	*len = 0;
	e1.key = *nclkey;
	status = ncl_retrieve_data_fixed (&e1);
	if (status == UU_SUCCESS)
		*len = e1.no_datael;

	return (status);
}
/*
.....compared data1 and data2, if it is the same, remove data2
*/
void removdup_data(nclkey1, nclkey2, count, label, nc)
UM_int4 *nclkey1, *nclkey2, *nc;
UM_int2 *count; 
UM_f77_str_ptr label;
{
	int i;
	UM_int4 nclkey;
	UM_int4 sub1, sub2;
	UM_int2 ix;
	UM_real8 val1,  val2;
	UM_int2 ivoc1, ivoc2;
	UM_int2 nextyp1, nextyp2;
	char label1[65], label2[65];
	int status = UU_SUCCESS;
	struct NCL_datast_rec e1, e2;

	e1.key = *nclkey1;
	status = ncl_retrieve_data_fixed (&e1);
	if (status != UU_SUCCESS)
		return;
/*	e2.key = *nclkey2;
	status = ncl_retrieve_data_fixed (&e2);
	if (status != UU_SUCCESS)
		return;
	if (stricmp(e1.
*/
	for (i=0; i<*count;i++)
	{
		nclkey = *nclkey1;
		ix = i+1;
		dtgetv(&nclkey, &ix, &val1, &ivoc1, &nextyp1,label1, &sub1);
		label1[64] = '\0';
		nclkey = *nclkey2;
		ix = i+1;
		dtgetv(&nclkey, &ix, &val2, &ivoc2, &nextyp2,label2, &sub2);
		label2[64] = '\0';
		if (val1!=val2)
			return;
		if (ivoc1!=ivoc2)
			return;
		if (nextyp1!=nextyp2)
			return;
		if (sub1!=sub2)
			return;
		if (stricmp(label1, label2)!=0)
			return;
	}
	if (i==*count)
	{
/*
.....delete nclkey2
*/
		dtdele(nclkey2);
/*
.....assign the nclkey1 label
*/
		strncpy(label, e1.label, 64);
		strncpy(label1, e1.label, 64);
		label1[64] = '\0';
		i = 63;
		while (label1[i]==' ') i--;
		*nc = i+2;
	}
}
