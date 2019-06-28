/*********************************************************************
**    NAME         :  nerd1.c
**       CONTAINS:
**
**				ncl_retrieve_data1
**				ncl_retrieve_tuple_and_var_info
**				ncl_store_surflist1
**				ncl_store_bndrlist
**				ncl_have_tesslst
**				ncl_surflist_number
**				ncl_get_surflist_num
**				ncl_wf_displst_num
**				ncl_surflist_data
**				ncl_retrieve_shaded
**				ncl_setent_shaded
**				ncl_setent_lucency
**				ncl_retrieve_lucency
**				ncl_setent_material
**				ncl_retrieve_material
**          ncl_setent_edge
**				ncl_lst_delete
**				ncl_sfdisp_delete
**				ncl_wfdisp_delete
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nerd1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:45
*********************************************************************/

#include "usysdef.h"
#include "rbase.h"
#include "ribase.h"
#include "rmtuple.h"
#include "udebug.h"
#include "mgeom.h"
#include "nccs.h"
#include "mdrel.h"
#include "msrfddl.h"
#include "mcrv.h"
#include "msol.h"
#include "atext.h"
static int ncl_get_surflist_num ();
static int ncl_retrieve_tuple_and_var_info ();

/*********************************************************************
**    FUNCTION     : int ncl_retrieve_data_fixed1 (eptr)
**       Retrieve fixed data & fill in variable list pointers.
**    PARAMETERS
**       INPUT  :
**          eptr       - ptr to entity.
**          eptr->key  - key of entity.
**       OUTPUT :
**          eptr       - Fixed data & variable list pointers.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    NOTE         : copy of ur_retrieve_data_fixed() except that it
**                   fills in variable list pointers to point directly
*                    to data in unibase.
*********************************************************************/
int ncl_retrieve_data_fixed1 (data_packet)
struct UR_data *data_packet;   /* address of the data packet   */
{
	int         status;  /* status, -1 if error, 0 otherwise    */
	UU_KEY_ID   key;     /* the key_id to retrieve from         */
	UU_REL_ID   rel_key; /* the geometric entity tuple from MTID*/
	int         rel;     /* the relation number                 */
	int         tuple;   /* the index into the relation         */

	uu_denter(UU_RTRC,(us,"ncl_retrieve_data_fixed, key = 0x%x",
			   data_packet->key_id));

	if (data_packet->key_id < 1) return (UU_FAILURE);

	status = 0 ;

	/* get the data tuple */
	key =  data_packet->key_id;
	ur_k2rt(key, &rel, &tuple);
	if(rel == UR_MTUPLE_REL)
	{
		//if (tuple == 1378897)
		//		{
		//			sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
		//			//return -1;
		//		}
		status = uri_retrieve_master_attr(rel, tuple, UR_DATA_INDX, &rel_key);
	}
	else
	{
		rel_key = key;
	}
	ur_k2rt(rel_key, &rel, &tuple);
	if(status == 0)
	{
		/* got a valid data key, get the fixed data and variable info for it */
		status = ncl_retrieve_tuple_and_var_info(rel, tuple, data_packet);
	}
	if(data_packet->key_id == 0)
	{
		data_packet->key_id = rel_key;   /* make key valid in no mtuple */
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**   E_FUNCTION  :  ncl_retrieve_tuple_and_var_info(rel_num, tuple_indx,
**                                              &data_packet)
**      retrieve fixed data and variable length list information
**    PARAMETERS
**       INPUT  :
**       rel_num,       relation number to retrieve from
**       tuple_indx,    index of tuple to retrieve
**       data_packet,   a pointer to a data packet where the fixed data
**                      and var info that is retrieved is to be put
**       OUTPUT :
**       data_packet.key_id,  still requested key_id
**       data_packet.rel_num, the relation number for the retrieved data
**       data_packet.buff,    the retrieved fixed data and var info
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    NOTE         : copy of ur_retrieve_tuple_and_var_info() except
**                   that it fills in var list pointers.
*********************************************************************/
static int ncl_retrieve_tuple_and_var_info(rel_num, tuple_indx, data_packet)
UR_REL_NUM     rel_num;       /* relation number to retrieve from */
UR_TUPLE_INDX  tuple_indx;    /* index of tuple to retrieve */
struct UR_data *data_packet;  /* address of the data packet */
{
	int      status;     /* status, -1 if error, 0 otherwise    */
	int      i;          /* an index                            */
	char     *lst_ptr;   /* pointer to a varlist, we don't use  */
	struct UR_lpacket *lpack_ptr; /* pointer to a list_pack  */

/*----------------------------------------------------------------------
** Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC,(us,"ncl_retrieve_tuple_and_var_info(rel=%d, tuple=%d)",
			   rel_num, tuple_indx));

	/* get the data tuple */
	status = ur_retrieve_tuple(rel_num, tuple_indx, data_packet);
	if(!status)
	{
		/* now insert the variable length info after the fixed data */
		for(i = 1; i <= UR_rcb[rel_num].n_varl; i++)
		{
			/* get a pointer to the list packet data after the fixed data */
			ur_get_list_packet_ptr(rel_num, i, data_packet, &lpack_ptr);
			ur_get_varlist_ptr(rel_num, tuple_indx, i, &lst_ptr,
				&(lpack_ptr->atom_cnt));
			lpack_ptr->list_ptr = lst_ptr;
			lpack_ptr++;
		}
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    FUNCTION     : int ncl_store_surflist1 (type, sf, ptr)
**    Stores a surface structure (box,boundary,..) in the Unibase
**    PARAMETERS
**       INPUT  :
**           type - type of list to store (BOX_LIST, BOUNDARY_LIST,...)
**           sf  - surface
**           ptr - pointer to the surface structure
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_store_surflist1 (type, sf, ptr)
NCL_surflist_type type;
struct NCL_fixed_databag *sf;
char *ptr;
{
	int status, num, pos, lst, np, nv, nt;
	UU_KEY_ID key;
	UU_REAL *dat, d;

	lst = ncl_surflist_number (type, sf->rel_num);
	if (!lst) return (UU_FAILURE);

	key = sf->key;

	switch (type)
	{
		case TESSELLATION_LIST:
		{
			UM_tessellation *t;

			t = (UM_tessellation *)ptr;
			pos = num = 1;

			np = t->np;
			nv = t->np;
			nt = t->ntri;

			if (UU_LIST_LENGTH(&t->normals) != np)
			{
				nv = nt;
				np = np * -1;
			}

			d = - t->toler;
			status = ur_update_data_varlist (key,lst,&d,pos,num);
			if(status != UU_SUCCESS) return (status);
			pos += num; d = np;
			status = ur_update_data_varlist (key,lst,&d,pos,num);
			if(status != UU_SUCCESS) return (status);
			pos += num; d = t->ntri;
			status = ur_update_data_varlist (key,lst,&d,pos,num);
			if(status != UU_SUCCESS) return (status);

			pos += num;
			dat = (UU_REAL *) UU_LIST_ARRAY (&t->vertices);
			num = (sizeof(UM_coord)/sizeof(UU_REAL)) * t->np;
			status = ur_update_data_varlist (key,lst,dat,pos,num);
			pos += num;
			dat = (UU_REAL *) UU_LIST_ARRAY (&t->normals);
			num = (sizeof(UM_vector)/sizeof(UU_REAL)) * nv;
			status = ur_update_data_varlist (key,lst,dat,pos,num);
			pos += num;
/*
			if (sf->rel_num != UM_SOLID_REL)
			{
				dat = (UU_REAL *) UU_LIST_ARRAY (&t->uv);
				num = (sizeof(UM_2Dcoord)/sizeof(UU_REAL)) * t->np;
				status = ur_update_data_varlist (key,lst,dat,pos,num);
				pos += num;
			}
*/
			dat = (UU_REAL *) UU_LIST_ARRAY (&t->tri);
			num = (sizeof(UM_tript)/sizeof(UU_REAL)) * t->ntri;
			status = ur_update_data_varlist (key,lst,dat,pos,num);

			break;
		}

		case BOX_LIST:
		{
			UM_3D_box *box;
/*
... save box vertices in the Unibase
*/
			box = (UM_3D_box *) ptr;
			num = 3*UM_3DBOX_NUM_VERT;
			status = ur_update_data_varlist(key, lst, box->ver, 1, num);
			break;
		}
		case UV_BOX_LIST:
		case WHOLE_BOUNDARY_LIST:
		{
			int i, nb;
			UM_srf_boundary *p;

			p = (UM_srf_boundary *) ptr;
/*
... save boundary data in the Unibase record of this surface
... in the following order:
... toler, nb, np[0],...,np[nb-1],np[nb], ummx,vmmx,uvpts,cvpts
*/
			pos = num = 1;
			if (type == UV_BOX_LIST) p->toler = -100;
			status = ur_update_data_varlist (key,lst,&p->toler,pos,num);
			if(status != UU_SUCCESS) return (status);

			nb = d = p->nb;
			pos += num;

			status = ur_update_data_varlist (key,lst,&d,pos,num);
			if(status != UU_SUCCESS) return (status);

			pos += num; num = 2*nb;
			status = ur_update_data_varlist (key,lst,p->ummx,pos,num);
			if(status != UU_SUCCESS) return (status);
			pos += num;
			status = ur_update_data_varlist (key,lst,p->vmmx,pos,num);
			if(status != UU_SUCCESS) return (status);

			if (type == WHOLE_BOUNDARY_LIST)
			{
				dat = (UU_REAL *) uu_malloc ( (nb+1)*sizeof(UU_REAL) );

				for (i=0; i<= nb; i++) dat[i] = p->np[i];
				pos += num; num = nb + 1;
				status = ur_update_data_varlist (key,lst,dat,pos,num);
				uu_free (dat);
				if(status != UU_SUCCESS) return (status);

				dat = (UU_REAL *) UU_LIST_ARRAY (p->uvpts);
				pos += num; num = 3*UU_LIST_LENGTH (p->uvpts);
				status = ur_update_data_varlist (key,lst,dat,pos,num);
				if(status != UU_SUCCESS) return (status);

				dat = (UU_REAL *) UU_LIST_ARRAY (p->cvpts);
				pos += num; num = 3*UU_LIST_LENGTH (p->cvpts);
				status = ur_update_data_varlist (key,lst,dat,pos,num);
			}
			break;
		}
		default:
			status = UU_FAILURE;
	}

	return (status);
}

/*********************************************************************
**    FUNCTION     : int ncl_store_bndrlist (sf,p,flag)
**      Stores a surface boundary in the Unibase, preceded by a flag
**    PARAMETERS
**       INPUT  :
**           flag - 2 iff boundary matches shading and wireframe display,
**                  1 iff boundary matches wirefarme only
**                  0 iff boundary may not match any display mode
**           sf   - surface
**           p    - boundary
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_store_bndrlist (sf,p,flag)
struct NCL_fixed_databag *sf;
UM_srf_boundary *p;
int flag;
{
	int status, num, pos, lst;
	UU_KEY_ID key;
	int i, nb;
	UU_REAL *dat, d;

	lst = ncl_surflist_number (WHOLE_BOUNDARY_LIST, sf->rel_num);
	if (!lst) return (UU_FAILURE);

	key = sf->key;
/*
... save boundary data in the Unibase record of this surface
... in the following order:
... flag, toler, nb, np[0],...,np[nb-1],np[nb], ummx,vmmx,uvpts,cvpts
*/
	pos = num = 1;
	d = flag;

	status = ur_update_data_varlist (key,lst,&d,pos,num);
	if (status != UU_SUCCESS) return (status);

	pos += num;
	status = ur_update_data_varlist (key,lst,&p->toler,pos,num);
	if (status != UU_SUCCESS) return (status);

	nb = d = p->nb;
	pos += num;
	status = ur_update_data_varlist (key,lst,&d,pos,num);
	if (status != UU_SUCCESS) return (status);

	pos += num; num = 2*nb;
	status = ur_update_data_varlist (key,lst,p->ummx,pos,num);
	if (status != UU_SUCCESS) return (status);
	pos += num;
	status = ur_update_data_varlist (key,lst,p->vmmx,pos,num);
	if (status != UU_SUCCESS) return (status);

	for (i = 0; i < nb; i++)
	{
		d = p->np[i];
		pos += num; num = 1;
		status = ur_update_data_varlist (key,lst,&d,pos,num);
		if (status != UU_SUCCESS) return (status);
	}

	dat = (UU_REAL *) UU_LIST_ARRAY (p->uvpts);
	pos += num; num = 3*UU_LIST_LENGTH (p->uvpts);
	status = ur_update_data_varlist (key,lst,dat,pos,num);
	if (status != UU_SUCCESS) return (status);

	dat = (UU_REAL *) UU_LIST_ARRAY (p->cvpts);
	pos += num; num = 3*UU_LIST_LENGTH (p->cvpts);
	status = ur_update_data_varlist (key,lst,dat,pos,num);

	return (status);
}

/*********************************************************************
**    FUNCTION : ncl_surflist_number (type,rel_num)
**      Gets the number of certain types of lists in a UNIBASE surface entity.
**    PARAMETERS
**       INPUT  :
**             type     - type of list: {BOX_LIST,BOUNDARY_LIST}
**             rel_num  - relation number of an entity
**       OUTPUT :
**          none
**    RETURNS      : number of the corresponding list for this surface type
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_surflist_number (type,rel_num)
NCL_surflist_type type;
int rel_num;
{
	int lst = 0;

	switch (rel_num)
	{
		case UM_RBSPLSRF_REL:
			switch (type)
			{
				case SSKEY_LIST:
					lst = 5;
					break;
				case DISPLAY_LIST:
					lst = 6;
					break;
				case TESSELLATION_LIST:
					lst = 7;
					break;
				case BOX_LIST:
					lst = 8;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					lst = 9;
					break;
				default:
					break;
			}
			break;
		case NCL_SURF_REL:
			switch (type)
			{
				case SSKEY_LIST:
					lst = 2;
					break;
				case DISPLAY_LIST:
					lst = 3;
					break;
				case TESSELLATION_LIST:
					lst = 4;
					break;
				case BOX_LIST:
					lst = 5;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					lst = 6;
					break;
				default:
					break;
			}
			break;
		case NCL_TRIMSF_REL:
		case NCL_MESHSURF_REL:
		case NCL_QUILTSURF_REL:
			switch (type)
			{
				case DISPLAY_LIST:
					lst = 2;
					break;
				case TESSELLATION_LIST:
					lst = 3;
					break;
				case BOX_LIST:
					lst = 4;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					lst = 5;
					break;
				default:
					break;
			}
			break;
		case NCL_NETSF_REL:
			switch (type)
			{
				case SSKEY_LIST:
					lst = 2;
					break;
				case DISPLAY_LIST:
					lst = 3;
					break;
				case TESSELLATION_LIST:
					lst = 4;
					break;
				default:
					break;
			}
			break;
		case NCL_SHAPE_REL:
			switch (type)
			{
				case DISPLAY_LIST:
					lst = 2;
					break;
				case TESSELLATION_LIST:
					lst = 3;
					break;
				default:
					break;
			}
			break;
		case NCL_REVSURF_REL:
			switch (type)
			{
				case SSKEY_LIST:
					lst = 1;
					break;
				case DISPLAY_LIST:
					lst = 2;
					break;
				case TESSELLATION_LIST:
					lst = 3;
					break;
				case BOX_LIST:
					lst = 4;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					lst = 5;
					break;
				default:
					break;
			}
			break;
		case UM_SOLID_REL:
			switch (type)
			{
				case DISPLAY_LIST:
					lst = 2;
					break;
				case TESSELLATION_LIST:
					lst = 3;
					break;
				default:
					break;
			}
			break;
	}

	return (lst);
}

/*********************************************************************
**    FUNCTION : ncl_get_surflist_num (sf,type)
**      Gets the number of certain types of lists in a UNIBASE surface entity.
**    PARAMETERS
**       INPUT  :
**             type     - type of list: {BOX_LIST,BOUNDARY_LIST}
**             sf       - surface entity
**       OUTPUT : none
**    RETURNS      : number of the corresponding list for this surface type,
**                   if the data is present, else 0
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_get_surflist_num (sf,type)
struct NCL_fixed_databag *sf;
NCL_surflist_type type;
{
	int lst = 0;

	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		{
			struct UM_rbsplsrf_rec *sf1;
			sf1 = (struct UM_rbsplsrf_rec *) sf;
			switch (type)
			{
				case SSKEY_LIST:
					if (sf1->no_sskey > 0) lst = 5;
					break;
				case DISPLAY_LIST:
					if (sf1->no_displst > 0) lst = 6;
					break;
				case TESSELLATION_LIST:
					if (sf1->no_tesslst > 0) lst = 7;
					break;
				case BOX_LIST:
					if (sf1->no_boxlst) lst = 8;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					if (sf1->no_xyzbylst) lst = 9;
					break;
				default:
					break;
			}
			break;
		}
		case NCL_SURF_REL:
		{
			struct NCL_surface_rec *sf1;
			sf1 = (struct NCL_surface_rec *) sf;
			switch (type)
			{
				case SSKEY_LIST:
					if (sf1->no_sskey > 0) lst = 2;
					break;
				case DISPLAY_LIST:
					if (sf1->no_displst > 0) lst = 3;
					break;
				case TESSELLATION_LIST:
					if (sf1->no_tesslst > 0) lst = 4;
					break;
				case BOX_LIST:
					if (sf1->no_boxlst) lst = 5;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					if (sf1->no_xyzbylst) lst = 6;
					break;
				default:
					break;
			}
			break;
		}
		case NCL_TRIMSF_REL:
		{
			struct NCL_trimsf_rec *sf1;
			sf1 = (struct NCL_trimsf_rec *) sf;
			switch (type)
			{
				case DISPLAY_LIST:
					if (sf1->no_displst > 0) lst = 2;
					break;
				case TESSELLATION_LIST:
					if (sf1->no_tesslst > 0) lst = 3;
					break;
				case BOX_LIST:
					if (sf1->no_boxlst) lst = 4;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					if (sf1->no_xyzbylst) lst = 5;
					break;
				default:
					break;
			}
			break;
		}
		case NCL_MESHSURF_REL:
		{
			struct NCL_meshsf_rec *sf1;
			sf1 = (struct NCL_meshsf_rec *) sf;
			switch (type)
			{
				case DISPLAY_LIST:
					if (sf1->no_displst > 0) lst = 2;
					break;
				case TESSELLATION_LIST:
					if (sf1->no_tesslst > 0) lst = 3;
					break;
				case BOX_LIST:
					if (sf1->no_boxlst) lst = 4;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					if (sf1->no_xyzbylst) lst = 5;
					break;
				default:
					break;
			}
			break;
		}
		case NCL_QUILTSURF_REL:
		{
			struct NCL_quiltsf_rec *sf1;
			sf1 = (struct NCL_quiltsf_rec *) sf;
			switch (type)
			{
				case DISPLAY_LIST:
					if (sf1->no_displst > 0) lst = 2;
					break;
				case TESSELLATION_LIST:
					if (sf1->no_tesslst > 0) lst = 3;
					break;
				case BOX_LIST:
					if (sf1->no_boxlst) lst = 4;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					if (sf1->no_xyzbylst) lst = 5;
					break;
				default:
					break;
			}
			break;
		}
		case NCL_NETSF_REL:
		{
			struct NCL_netsf_rec *sf1;
			sf1 = (struct NCL_netsf_rec *) sf;
			switch (type)
			{
				case SSKEY_LIST:
					if (sf1->no_sskey > 0) lst = 2;
					break;
				case DISPLAY_LIST:
					if (sf1->no_displst > 0) lst = 3;
					break;
				case TESSELLATION_LIST:
					if (sf1->no_tesslst > 0) lst = 4;
					break;
				default:
					break;
			}
			break;
		}
		case NCL_SHAPE_REL:
		{
			struct NCL_shape_rec *sf1;
			sf1 = (struct NCL_shape_rec *) sf;
			switch (type)
			{
				case DISPLAY_LIST:
					if (sf1->no_displst > 0) lst = 2;
					break;
				case TESSELLATION_LIST:
					if (sf1->no_tesslst > 0) lst = 3;
					break;
				default:
					break;
			}
			break;
		}
		case NCL_REVSURF_REL:
		{
			struct NCL_revsurf_rec *sf1;
			sf1 = (struct NCL_revsurf_rec *) sf;
			switch (type)
			{
				case SSKEY_LIST:
					if (sf1->no_sskey > 0) lst = 1;
					break;
				case DISPLAY_LIST:
					if (sf1->no_displst > 0) lst = 2;
					break;
				case TESSELLATION_LIST:
					if (sf1->no_tesslst > 0) lst = 3;
					break;
				case BOX_LIST:
					if (sf1->no_boxlst) lst = 4;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					if (sf1->no_xyzbylst) lst = 5;
					break;
				default:
					break;
			}
			break;
		}
		case UM_SOLID_REL:
		{
			struct UM_solid_rec *sf1;
			sf1 = (struct UM_solid_rec *) sf;
			switch (type)
			{
				case DISPLAY_LIST:
					if (sf1->no_displst > 0) lst = 2;
					break;
				case TESSELLATION_LIST:
					if (sf1->no_tesslst > 0) lst = 3;
					break;
				default:
					break;
			}
			break;
		}
	}

	return (lst);
}

/*********************************************************************
**    FUNCTION     : ncl_surflist_data (type,sf,data)
**      Gets pointer to certain types of lists in a UNIBASE surface entity.
**    PARAMETERS
**       INPUT  :
**             type     - type of list: {BOX_LIST,BOUNDARY_LIST}
**             sf       - a surface
**       OUTPUT :
**             *data     - pointer to the corresponding list
**                         in this surface structure
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_surflist_data (type, sf, data)
NCL_surflist_type type;
struct NCL_fixed_databag *sf;
UU_REAL **data;
{
	int status = UU_SUCCESS;

	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		{
			struct UM_rbsplsrf_rec *sf1;
			sf1 = (struct UM_rbsplsrf_rec *) sf;
			switch (type)
			{
				case TESSELLATION_LIST:
					*data = sf1->tesslst;
					break;
				case BOX_LIST:
					*data = sf1->boxlst;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					*data = sf1->xyzbylst;
					break;
			}
			break;
		}
		case NCL_SURF_REL:
		{
			struct NCL_surface_rec *sf1;
			sf1 = (struct NCL_surface_rec *) sf;
			switch (type)
			{
				case TESSELLATION_LIST:
					*data = sf1->tesslst;
					break;
				case BOX_LIST:
					*data = sf1->boxlst;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					*data = sf1->xyzbylst;
					break;
			}
			break;
		}
		case NCL_MESHSURF_REL:
		{
			struct NCL_meshsf_rec *sf1;
			sf1 = (struct NCL_meshsf_rec *) sf;
			switch (type)
			{
				case TESSELLATION_LIST:
					*data = sf1->tesslst;
					break;
				case BOX_LIST:
					*data = sf1->boxlst;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					*data = sf1->xyzbylst;
					break;
			}
			break;
		}
		case NCL_QUILTSURF_REL:
		{
			struct NCL_quiltsf_rec *sf1;
			sf1 = (struct NCL_quiltsf_rec *) sf;
			switch (type)
			{
				case TESSELLATION_LIST:
					*data = sf1->tesslst;
					break;
				case BOX_LIST:
					*data = sf1->boxlst;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					*data = sf1->xyzbylst;
					break;
			}
			break;
		}
		case NCL_TRIMSF_REL:
		{
			struct NCL_trimsf_rec *sf1;
			sf1 = (struct NCL_trimsf_rec *) sf;
			switch (type)
			{
				case TESSELLATION_LIST:
					*data = sf1->tesslst;
					break;
				case BOX_LIST:
					*data = sf1->boxlst;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					*data = sf1->xyzbylst;
					break;
			}
			break;
		}
/*
.....added for shape
.....Yurong 5/5/99
*/
		case NCL_SHAPE_REL:
		{
			struct NCL_shape_rec *shape;
			shape = (struct NCL_shape_rec *)sf;
			switch (type)
			{
				case TESSELLATION_LIST:
					*data = shape->tesslst;
					break;
			}
			break;
		}
		case NCL_REVSURF_REL:
		{
			struct NCL_revsurf_rec *sf1;
			sf1 = (struct NCL_revsurf_rec *) sf;
			switch (type)
			{
				case TESSELLATION_LIST:
					*data = sf1->tesslst;
					break;
				case BOX_LIST:
					*data = sf1->boxlst;
					break;
				case UV_BOX_LIST:
				case WHOLE_BOUNDARY_LIST:
					*data = sf1->xyzbylst;
					break;
			}
			break;
		}
		case UM_SOLID_REL:
		{
			struct UM_solid_rec *sf1;
			sf1 = (struct UM_solid_rec *)sf;
			switch (type)
			{
				case TESSELLATION_LIST:
					*data = sf1->tesslst;
					break;
			}
			break;
		}
		default:
			*data = 0;
	}

	if ( !(*data)) status = UU_FAILURE;

	return (status);
}

/*********************************************************************
**    FUNCTION     : ncl_have_tesslst (sf)
**      Check if a Unibase surface struct currently stores a tessellation list.
**    PARAMETERS
**       INPUT  :
**             sf       - a surface
**       OUTPUT : none
**    RETURNS      : UU_TRUE / UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_have_tesslst (sf)
struct NCL_fixed_databag *sf;
{
	int ntess;
	UU_REAL *data;

	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		{
			struct UM_rbsplsrf_rec *sf1;
			sf1 = (struct UM_rbsplsrf_rec *) sf;
			ntess =  sf1->no_tesslst;
			data = sf1->tesslst;
			break;
		}
		case NCL_SURF_REL:
		{
			struct NCL_surface_rec *sf1;
			sf1 = (struct NCL_surface_rec *) sf;
			ntess =  sf1->no_tesslst;
			data = sf1->tesslst;
			break;
		}
		case NCL_MESHSURF_REL:
		{
			struct NCL_meshsf_rec *sf1;
			sf1 = (struct NCL_meshsf_rec *) sf;
			ntess =  sf1->no_tesslst;
			data = sf1->tesslst;
			break;
		}
		case NCL_QUILTSURF_REL:
		{
			struct NCL_quiltsf_rec *sf1;
			sf1 = (struct NCL_quiltsf_rec *) sf;
			ntess =  sf1->no_tesslst;
			data = sf1->tesslst;
			break;
		}
		case NCL_TRIMSF_REL:
		{
			struct NCL_trimsf_rec *sf1;
			sf1 = (struct NCL_trimsf_rec *) sf;
			ntess =  sf1->no_tesslst;
			data = sf1->tesslst;
			break;
		}
/*
.....added for shape
.....Yurong 5/5/99
*/
		case NCL_SHAPE_REL:
		{
			struct NCL_shape_rec *shape;
			shape = (struct NCL_shape_rec *)sf;
			ntess =  shape->no_tesslst;
			data = shape->tesslst;
			break;
		}
		case NCL_REVSURF_REL:
		{
			struct NCL_revsurf_rec *sf1;
			sf1 = (struct NCL_revsurf_rec *) sf;
			ntess =  sf1->no_tesslst;
			data = sf1->tesslst;
			break;
		}
		default:
			ntess = 0;
			data = UU_NULL;
	}

	return (ntess > 0 && data != UU_NULL);
}

/*********************************************************************
**    FUNCTION     : ncl_retrieve_shaded(sf, shaded)
**      Gets shaded attributes in a UNIBASE surface entity.
**    PARAMETERS
**       INPUT  :
**             sf       - a surface
**       OUTPUT :
**             shaded
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_retrieve_shaded(sf, shaded)
struct NCL_fixed_databag *sf;
int *shaded;
{
	int status = UU_SUCCESS;

	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_NETSF_REL:
		case NCL_MESHSURF_REL:
		case NCL_TRIMSF_REL:
		case NCL_SHAPE_REL:
		case NCL_REVSURF_REL:
		case UM_SOLID_REL:
		{
			struct UM_surfattr_rec attr;
			uc_retrieve_attr(sf->key,&attr);
			*shaded = attr.shaded;
			break;
		}
		case NCL_QUILTSURF_REL:
		{
			struct NCL_quiltsf_rec *sf1;
			sf1 = (struct NCL_quiltsf_rec *) sf;
			*shaded = sf1->shaded;
			break;
		}
		default:
			*shaded = 0;
	}
	return (status);
}

/*********************************************************************
**    FUNCTION     : ncl_setent_shaded(sf, shaded)
**      Sets shaded attributes in a UNIBASE surface entity.
**    PARAMETERS
**       INPUT  :
**             sf       - a surface
**       OUTPUT :
**             shaded
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added for set shaded attributes
.....Yurong
*/
int ncl_setent_shaded(sf, shaded)
struct NCL_fixed_databag *sf;
int shaded;
{
	int changed;
	int status = UU_SUCCESS;

	changed = 0;
	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_NETSF_REL:
		case NCL_MESHSURF_REL:
		case NCL_TRIMSF_REL:
		case NCL_SHAPE_REL:
		case NCL_REVSURF_REL:
		case UM_SOLID_REL:
		{
			struct UM_surfattr_rec attr;
			uc_retrieve_attr(sf->key,&attr);
			attr.shaded = shaded;
			ur_update_attr(&attr);
			break;
		}
		case NCL_QUILTSURF_REL:
		{
			struct NCL_quiltsf_rec *sf1;
			sf1 = (struct NCL_quiltsf_rec *) sf;
			sf1->shaded = shaded;
			changed = 1;
			break;
		}		
		default:
			break;
	}
	if (changed)
		ur_update_data_fixed(sf);

	return (status);
}

/*********************************************************************
**    FUNCTION     : ncl_setent_lucency(sf, lucency)
**      retrieve translucency attributes in a UNIBASE surface entity.
**    PARAMETERS
**       INPUT  :
**             sf       - a surface
**					lucency
**       OUTPUT :
**              none
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added for set entity translucency
.....Yurong 4/15/99
*/
int ncl_setent_lucency(sf, lucency)
struct NCL_fixed_databag *sf;
int lucency;
{
	int changed;
	int status = UU_SUCCESS;

	changed = 0;
	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_NETSF_REL:
		case NCL_MESHSURF_REL:
		case NCL_TRIMSF_REL:
		case NCL_SHAPE_REL:
		case NCL_REVSURF_REL:
		case UM_SOLID_REL:
		{
			struct UM_surfattr_rec attr;
			uc_retrieve_attr(sf->key,&attr);
			attr.lucency = lucency;
			ur_update_attr(&attr);
			break;
		}
		case NCL_QUILTSURF_REL:
		{
			struct NCL_quiltsf_rec *sf1;
			sf1 = (struct NCL_quiltsf_rec *) sf;
			sf1->lucency = lucency;
			changed = 1;
			break;
		}
		default:
			break;
	}
	if (changed)
		ur_update_data_fixed(sf);
	return (status);
}

/*********************************************************************
**    FUNCTION     : ncl_retrieve_lucency(sf, lucency)
**      retrieve translucency attributes in a UNIBASE surface entity.
**    PARAMETERS
**       INPUT  :
**             sf       - a surface
**					lucency
**       OUTPUT :
**              none
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_retrieve_lucency(sf, lucency)
struct NCL_fixed_databag *sf;
int *lucency;
{
	int status = UU_SUCCESS;

	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_NETSF_REL:
		case NCL_MESHSURF_REL:
		case NCL_TRIMSF_REL:
		case NCL_SHAPE_REL:
		case NCL_REVSURF_REL:
		case UM_SOLID_REL:
		{
			struct UM_surfattr_rec attr;
			uc_retrieve_attr(sf->key,&attr);
			*lucency = attr.lucency;
			break;
		}
		case NCL_QUILTSURF_REL:
		{
			struct NCL_quiltsf_rec *sf1;
			sf1 = (struct NCL_quiltsf_rec *) sf;
			*lucency = sf1->lucency;
			break;
		}		
		default:
			*lucency = 100;
	}
	return (status);
}
/*********************************************************************
**    FUNCTION     : ncl_lst_delete1 (type,e)
**      delete a list in a UNIBASE  surface entity.
**    PARAMETERS
**       INPUT  :
**             e    - surface entity
**             type - type of list to store (BOX_LIST, BOUNDARY_LIST,...)
**       OUTPUT : none
**    RETURNS      :
**         UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int ncl_lst_delete1 (type,e)
NCL_surflist_type type;
struct NCL_fixed_databag *e;
{
	int lst,status;

	lst = ncl_get_surflist_num (e,type);
	if (lst <= 0)
		status = UU_FAILURE;
	else
		status = ur_delete_data_varlist (e->key, lst);

	return (status);
}

/*********************************************************************
**    FUNCTION     : ncl_lst_delete(type, nclkey)
**      delete a list in a UNIBASE  surface entity.
**    PARAMETERS
**       INPUT  :
**             nclkey: key to the surface
**             type - type of list to store (BOX_LIST, BOUNDARY_LIST,...)
**       OUTPUT : none
**    RETURNS      :
**         UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
ncl_lst_delete(type, nclkey)
NCL_surflist_type type;
UM_int4 *nclkey;
{
	int status;
	struct NCL_fixed_databag sf;

	sf.key = *nclkey;
	status = ncl_retrieve_data_fixed (&sf);

	if (status == UU_SUCCESS)
		status = ncl_lst_delete1 (type,&sf);

	return (status);
}

/*********************************************************************
**    FUNCTION     : ncl_sfdisp_delete (e)
**********************************************************************/
void ncl_sfdisp_delete (e)
struct NCL_fixed_databag *e;
{
	ncl_lst_delete1 (DISPLAY_LIST,e);
	ncl_lst_delete1 (TESSELLATION_LIST,e);
}

/*********************************************************************
**    FUNCTION : ncl_wf_displst_num (e)
**      Gets the display list number in a UNIBASE non-surface entity.
**    PARAMETERS
**       INPUT  :
**             e        - entity
**       OUTPUT : none
**    RETURNS      : number of the corresponding list for this entity type,
**                   if the data is present, else 0
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_wf_displst_num (e)
struct NCL_fixed_databag *e;
{
	int lst = 0;

	switch (e->rel_num)
	{
		case NCL_POINT_REL:
		case UM_POINT_REL:
		{
			struct UM_point_rec *e1;
			e1 = (struct UM_point_rec *) e;
			if (e1->no_displst > 0) lst = 1;
			break;
		}
		case NCL_LINE_REL:
		case UM_LINE_REL:
		{
			struct UM_line_rec *e1;
			e1 = (struct UM_line_rec *) e;
			if (e1->no_displst > 0) lst = 1;
			break;
		}
		case NCL_CIRCLE_REL:
		case UM_CIRCLE_REL:
		{
			struct UM_circle_rec *e1;
			e1 = (struct UM_circle_rec *) e;
			if (e1->no_displst > 0) lst = 1;
			break;
		}
		case UM_CONIC_REL:
		{
			struct UM_conic_rec *e1;
			e1 = (struct UM_conic_rec *) e;
			if (e1->no_displst > 0) lst = 1;
			break;
		}
		case NCL_VECTOR_REL:
		{
			struct NCL_vector_rec *e1;
			e1 = (struct NCL_vector_rec *) e;
			if (e1->no_displst > 0) lst = 1;
			break;
		}
		case NCL_PLN_REL:
		{
			struct NCL_nclpl_rec *e1;
			e1 = (struct NCL_nclpl_rec *) e;
			if (e1->no_displst > 0) lst = 1;
			break;
		}
		case NCL_MATRIX_REL:
		{
			struct NCL_matrix_rec *e1;
			e1 = (struct NCL_matrix_rec *) e;
			if (e1->no_displst > 0) lst = 1;
			break;
		}
		case NCL_CURVE_REL:
		{
			struct NCL_curve_rec *e1;
			e1 = (struct NCL_curve_rec *) e;
			if (e1->no_displst > 0) lst = 3;
			break;
		}
		case NCL_PATERN_REL:
		{
			struct NCL_patern_rec *e1;
			e1 = (struct NCL_patern_rec *) e;
			if (e1->no_displst > 0) lst = 2;
			break;
		}
		case NCL_POINTVEC_REL:
		{
			struct NCL_nclpv_rec *e1;
			e1 = (struct NCL_nclpv_rec *) e;
			if (e1->no_displst > 0) lst = 1;
			break;
		}
		case UM_COMPCRV_REL:
		{
			struct UM_compcrv_rec *e1;
			e1 = (struct UM_compcrv_rec *) e;
			if (e1->no_displst > 0) lst = 2;
			break;
		}
		case UM_RBSPLCRV_REL:
		{
			struct UM_rbsplcrv_rec *e1;
			e1 = (struct UM_rbsplcrv_rec *) e;
			if (e1->no_displst > 0) lst = 4;
			break;
		}
		case UM_UVCVONSF_REL:
		{
			struct UM_uvcvonsf_rec *e1;
			e1 = (struct UM_uvcvonsf_rec *) e;
			if (e1->no_displst > 0) lst = 4;
			break;
		}
		case UM_POLYLINE_REL:
		{
			struct UM_polyline_rec *e1;
			e1 = (struct UM_polyline_rec *) e;
			if (e1->no_displst > 0) lst = 2;
			break;
		}
		case UA_TEXT_REL:
		{
			struct UA_txt_rec *e1;
			e1 = (struct UA_txt_rec *) e;
			if (e1->no_displst > 0) lst = 2;
			break;
		}
		case UM_SOLID_REL:
		{
			struct UM_solid_rec *e1;
			e1 = (struct UM_solid_rec *) e;
			if (e1->no_displst > 0) lst = 2;
			break;
		}
	}

	return (lst);
}

/*********************************************************************
**    FUNCTION     : ncl_wfdisp_delete (e)
**      Delete a display list if not empty for a UNIBASE dispalyable
**      non-surface entity.
**********************************************************************/
int ncl_wfdisp_delete (e)
struct NCL_fixed_databag *e;
{
	int lst,status;

	lst = ncl_wf_displst_num (e);
	if (lst <= 0)
		status = UU_FAILURE;
	else
		status = ur_delete_data_varlist (e->key, lst);

	return (status);
}

/*********************************************************************
**    FUNCTION     : ncl_setent_material(sf, material)
**      retrieve material attributes in a UNIBASE surface entity.
**    PARAMETERS
**       INPUT  :
**             sf       - a surface
**				material
**       OUTPUT :
**              none
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_setent_material(sf, material)
struct NCL_fixed_databag *sf;
int material;
{
	int changed;
	int status = UU_SUCCESS;

	changed = 0;
	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_NETSF_REL:
		case NCL_MESHSURF_REL:
		case NCL_TRIMSF_REL:
		case NCL_SHAPE_REL:
		case NCL_REVSURF_REL:
		case UM_SOLID_REL:
		{
			struct UM_surfattr_rec attr;
			uc_retrieve_attr(sf->key,&attr);
			attr.material = material;
			ur_update_attr(&attr);
			break;
		}
		case NCL_QUILTSURF_REL:
		{
			struct NCL_quiltsf_rec *sf1;
			sf1 = (struct NCL_quiltsf_rec *) sf;
			sf1->material = material;
			changed = 1;
			break;
		}
		default:
			break;
	}
	if (changed)
		ur_update_data_fixed(sf);
	return (status);
}

/*********************************************************************
**    FUNCTION     : ncl_retrieve_material(sf, material)
**      retrieve material attributes in a UNIBASE surface entity.
**    PARAMETERS
**       INPUT  :
**             sf       - a surface
**					material
**       OUTPUT :
**              none
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_retrieve_material(sf, material)
struct NCL_fixed_databag *sf;
int *material;
{
	int status = UU_SUCCESS;

	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_NETSF_REL:
		case NCL_MESHSURF_REL:
		case NCL_TRIMSF_REL:
		case NCL_SHAPE_REL:
		case NCL_REVSURF_REL:
		case UM_SOLID_REL:
		{
			struct UM_surfattr_rec attr;
			uc_retrieve_attr(sf->key,&attr);
			*material = attr.material;
			break;
		}
		case NCL_QUILTSURF_REL:
		{
			struct NCL_quiltsf_rec *sf1;
			sf1 = (struct NCL_quiltsf_rec *) sf;
			*material = sf1->material;
			break;
		}		
		default:
			*material = 0;
	}
	return (status);
}

/*********************************************************************
**    E-FUNCTION     : ncl_setent_edge(sf,disp,color)
**      Sets the edge display attributes for a surface entity.
**    PARAMETERS
**       INPUT  :
**             sf     - a surface
**             disp   - 1 = Display surface edges, 0 = Don't.
**             color  - Edge color, -1 if not specified.
**				material
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_setent_edge(sf,disp,color)
struct NCL_fixed_databag *sf;
int disp,color;
{
	int changed;
	int status = UU_SUCCESS;

	changed = 0;
	switch (sf->rel_num)
	{
		case UM_RBSPLSRF_REL:
		case NCL_SURF_REL:
		case NCL_NETSF_REL:
		case NCL_MESHSURF_REL:
		case NCL_TRIMSF_REL:
		case NCL_SHAPE_REL:
		case NCL_REVSURF_REL:
		case UM_SOLID_REL:
		{
			struct UM_surfattr_rec attr;
			uc_retrieve_attr(sf->key,&attr);
			if (disp == 0)
				attr.ecolor = -1;
			else
				attr.ecolor = color == -1 ? attr.ecolor : color;
			ur_update_attr(&attr);
			break;
		}
		default:
			break;
	}
	if (changed)
		ur_update_data_fixed(sf);
	return (status);
}

