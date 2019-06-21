/*********************************************************************
**    NAME         :  nerd.c
**       CONTAINS:
**
**           ncl_retrieve_data_fixed
**           ncl_store_surflist
**           ncl_get_surflist
**           ncl_get_tesslst
**           ncl_get_tesslst0
**           ncl_get_bndrlist
**
**    COPYRIGHT 1996 (c) Numerical Control Computer Sciences Inc.
**                       All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nerd.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:45
*********************************************************************/

#include "usysdef.h"
#include "rbase.h"
#include "ribase.h"
#include "rmtuple.h"
#include "udebug.h"
#include "nccs.h"
#include "nclx.h"
#include "ycom.h"
#include "mdrel.h"
#include "mgeom.h"
#include "nclxmdl.h"
#include "nclfc.h"
#include "uminmax.h"

extern int NCLX_internal_geom;
#define TOL_CHANGED(t1,t2) t1 >= UM_BOUNDARY_TOLER && t2 > 1.2*t1

char *uu_malloc();

/*********************************************************************
**    FUNCTION     : int ncl_retrieve_data_fixed (eptr)
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
**                   to data in unibase.
*********************************************************************/
int ncl_retrieve_data_fixed (eptr)
struct NCL_fixed_databag *eptr;
{
	int status = UU_SUCCESS;
	NCLX_mdl_struct *geo = UU_NULL;
	UU_KEY_ID key;
	NCLX_mdl_surf *nsf1, *nsf2;
	NCLX_mdl_trimsf *tsf1, *tsf2;
	NCLX_mdl_curve  *cv1, *cv2;
	NCLX_mdl_composite *ccv1, *ccv2;
	NCLX_mdl_line *ln1, *ln2;
	NCLX_mdl_circle *ci1, *ci2;

	if (!NCLX_internal_geom)
		status = ncl_retrieve_data_fixed1(eptr);
	else
	{
/*
.....Search for geometry in PS,DS,CS
*/
	  key = eptr->key;
	  if (key < 1) return (NCLX_FAILURE);

	  status = NclxMdlFindGeo (key, &geo);

	  if (status == NCLX_SUCCESS)
	  {
		  switch (geo->relnum)
		  {
		  case NCLX_MDL_LINE:
			  ln1 = (NCLX_mdl_line *)eptr;
			  ln2 = (NCLX_mdl_line *)geo;
			  *ln1 = *ln2;
			  break;
		  case NCLX_MDL_CIRCLE:
			  ci1 = (NCLX_mdl_circle *)eptr;
			  ci2 = (NCLX_mdl_circle *)geo;
			  *ci1 = *ci2;
			  break;

		  case NCLX_MDL_CURVE:
		  case NCLX_MDL_BSPLINE:
			  cv1 = (NCLX_mdl_curve *)eptr;
			  cv2 = (NCLX_mdl_curve *)geo;
			  *cv1 = *cv2;
			  break;
		  case NCLX_MDL_COMPOSITE:
			  ccv1 = (NCLX_mdl_composite *)eptr;
			  ccv2 = (NCLX_mdl_composite *)geo;
			  *ccv1 = *ccv2;
			  break;
		  case NCLX_MDL_SURF:
		  case NCLX_MDL_NSURF:
			  nsf1 = (NCLX_mdl_surf *)eptr;
			  nsf2 = (NCLX_mdl_surf *)geo;
			  *nsf1 = *nsf2;
			  break;
		  case NCLX_MDL_TRIMSF:
			  tsf1 = (NCLX_mdl_trimsf *)eptr;
			  tsf2 = (NCLX_mdl_trimsf *)geo;
			  *tsf1 = *tsf2;
			  break;
		  default:
			  status = NCLX_FAILURE;
			  break;
			}
		}
	}
	return (status);
}

/*********************************************************************
**    FUNCTION     : int ncl_store_surflist (type, sf, p)
**    Stores given surface structure (box, boundary,...) in the Unibase
**    PARAMETERS
**       INPUT  :
**           type - type of surface list to store (BOX_LIST, BOUNDARY_LIST,...)
**           sf - surface
**           ptr  - pointer to the structure to store;
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_store_surflist (type, sf, ptr)
NCL_surflist_type type;
struct NCL_fixed_databag *sf;
char *ptr;
{
	int status;

	if (!NCLX_internal_geom)
		status = ncl_store_surflist1 (type, sf, ptr);
	else
	{
		int i,size;
		double *dat, *newptr;
		NCLX_mdl_struct *geo = UU_NULL;
		NCLX_mdl_surf *sf1;
		NCLX_mdl_trimsf *sf2;

		status = NclxMdlFindGeo (sf->key, &geo);
		if (status != NCLX_SUCCESS) return (UU_FAILURE);

		sf1 = (NCLX_mdl_surf *)   geo;
		sf2 = (NCLX_mdl_trimsf *) geo;

		switch (type)
		{
			case BOX_LIST:
			{
				UM_3D_box *box;
				box = (UM_3D_box *) ptr;
				size = 3*UM_3DBOX_NUM_VERT;
				dat = (double *) box->ver;
				newptr = (double *) uu_malloc (sizeof(double)*size);
				for (i=0; i<size; i++) newptr[i] = dat[i];

				status = UU_SUCCESS;

				switch (sf->rel_num)
				{
/*
.....Include NSURF here. JLS 6/15/99
*/
					case NCLX_MDL_NSURF:
					case NCLX_MDL_SURF:
						sf1->no_boxlst = size;
						sf1->boxlst    = newptr;
						break;
					case NCLX_MDL_TRIMSF:
						sf2->no_boxlst = size;
						sf2->boxlst    = newptr;
						break;
					default:
						if (newptr) uu_free (newptr);
						status = UU_FAILURE;
				}
				break;
			}
			case UV_BOX_LIST:
			case WHOLE_BOUNDARY_LIST:
			{
/*
... save boundary data in sf->bndrylst in the following order:
... toler, nb, ummx,vmmx, np[0],...,np[nb-1],np[nb] (=0),uvpts,cvpts
*/
				int np,nuv,nb,j;
				UM_srf_boundary *p;

				p = (UM_srf_boundary *) ptr;
				nb = p->nb;
				if (type == UV_BOX_LIST)
				{
					p->toler = -100.;
					size = 2 + 4*nb;
				}
				else
				{
					np = UU_LIST_LENGTH (p->uvpts);
					nuv = UU_LIST_LENGTH (p->cvpts);
					size = 3 + 5*nb + 3*np + 3*nuv;
				}

				if (size <= 0 ) return (UU_FAILURE);

				newptr = (double *) uu_malloc (sizeof(double)*size);
				newptr[0] = p->toler;
				newptr[1] = p->nb;
				j = 2; dat  = (double *) p->ummx;
				for (i=0; i< 2*nb;  i++, j++) newptr[j] = dat[i];
					dat = (double *) p->vmmx;
				for (i=0; i< 2*nb;  i++, j++) newptr[j] = dat[i];

				if (type == WHOLE_BOUNDARY_LIST)
				{
					for (i=0; i<= nb;   i++, j++) newptr[j] = p->np[i];
					dat = (double *) UU_LIST_ARRAY (p->uvpts);
					for (i=0; i< 3*nuv; i++, j++) newptr[j] = dat[i];
					dat = (double *) UU_LIST_ARRAY (p->cvpts);
					for (i=0; i< 3*np;  i++, j++) newptr[j] = dat[i];
				}

				status = UU_SUCCESS;

				switch (sf->rel_num)
				{
/*
.....NSURF was left out, including it. JLS 6/15/99
*/
					case NCLX_MDL_NSURF:
					case NCLX_MDL_SURF:
						sf1->no_bndrylst = size;
						if (!sf1->bndrylst) uu_free (sf1->bndrylst);
						sf1->bndrylst    = newptr;
						break;
					case NCLX_MDL_TRIMSF:
						sf2->no_bndrylst = size;
						if (!sf2->bndrylst) uu_free (sf2->bndrylst);
						sf2->bndrylst    = newptr;
						break;
					default:
						if (newptr) uu_free (newptr);
						return (UU_FAILURE);
				}
				break;
			}
			default:
				status = UU_FAILURE;
		}
	}

	return (status);
}

/*********************************************************************
**    FUNCTION     : int ncl_get_tesslst0 (t,data,relnum)
**      Retrieves the tessellation from a surface Unibase-stored data
**    PARAMETERS
**       INPUT  :
**         data   - Unibase-stored tessellation data
**         relnum - Unibase relation number to retrieve data for.
**       OUTPUT :
**         t    - tessellation structure
**    RETURNS :
**       UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_get_tesslst0 (t,data,relnum)
UU_REAL *data;
UM_tessellation *t;
int relnum;
{
	UU_REAL tol = data[0];
	int n,status,nv;

	if (tol > 0.)
	{
		UM_edge *edge;
		UM_coord *p;
		UM_2Dcoord *uv;
		UM_vector *norm;
		int np;

		t->toler = tol; data+=2;
		np = (int) *data; data++;
		n = (int) *data; data++; /* nedges */
		p = (UM_coord *) data; data += 3*np;
		norm = (UM_vector *) data; data += 3*np;
		uv = (UM_2Dcoord *) data; data += 2*np;
		edge = (UM_edge *) data;

		status = ncl_edges_to_tess (edge,n,p,norm,uv,np,t,tol*tol);
	}
	else
	{
		t->toler = -tol; data++;
		t->np = (int) *data;  data++;
		t->ntri = (int) *data; data++;
		nv = t->np;
		if (t->np < 0)
		{
			t->np = t->np * -1;
			nv = t->ntri;
		}

		status = UU_SUCCESS;

		n = t->np + 1;
		if (UU_LIST_NULLPTR (&t->vertices))
		{
			status = uu_list_init1 (&t->vertices, sizeof (UM_coord), n, n);
			if (status != UU_SUCCESS) return (status);
		}
		if (UU_LIST_NULLPTR (&t->normals))
		{
			status = uu_list_init1 (&t->normals, sizeof (UM_vector), nv+1, nv+1);
			if (status != UU_SUCCESS) return (status);
		}
/*
		if (UU_LIST_NULLPTR (&t->uv))
		{
			if (relnum == UM_SOLID_REL)
				uu_list_init0(&t->uv);
			else
			{
				status = uu_list_init1 (&t->uv, sizeof (UM_2Dcoord), n, n);
				if (status != UU_SUCCESS) return (status);
			}
		}
*/

		n = t->np;
		uu_list_push_multiple (&t->vertices,t->np,data);
		data += 3*n;
		uu_list_push_multiple (&t->normals,nv,data);
		data += 3*nv;
/*
		if (relnum != UM_SOLID_REL)
		{
			uu_list_push_multiple (&t->uv,t->np,data);
			data += 2*n;
		}
*/

		n = t->ntri + 1;

		if (UU_LIST_NULLPTR (&t->tri))
		{
			status = uu_list_init1 (&t->tri, sizeof (UM_tript), n, n);
			if (status != UU_SUCCESS) return (status);
		}

		uu_list_push_multiple (&t->tri,t->ntri,data);
	}

	return (status);
}

/*********************************************************************
*********************************************************************/
static void S_get_uvmmx (nb,p,pdata)
int nb;
UM_srf_boundary *p;
UU_REAL **pdata;
{
	int i;
	UU_REAL *data;

	data = *pdata;
	for (i=0; i<nb; i++, data +=2) um_vctovc_2d (data, p->ummx[i]);
	for (i=0; i<nb; i++, data +=2) um_vctovc_2d (data, p->vmmx[i]);
	*pdata = data;
}

/*********************************************************************
*********************************************************************/
static void S_get_bndrys (nb,p,data)
int nb;
UM_srf_boundary *p;
UU_REAL *data;
{
	int i,n;
	UM_coord *pt;

	for (i=0; i<=nb; i++, data++) p->np[i] = (int) *data;

	pt = (UM_coord *) data;
	for (i=0, n=0; i<nb; i++) n += p->np[i];

	uu_list_push_multiple (p->uvpts,n,pt);
	pt += n;
	uu_list_push_multiple (p->cvpts,n,pt);
}

/*********************************************************************
**   FUNCTION : int S_get_bndrbox (p,data)
**      Retrieves a boundary box from a surface Unibase boundary record.
**    PARAMETERS
**       INPUT  :
**         data   - stored surface list
**       OUTPUT :
**         p     - boundary box data
**    RETURNS : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_get_bndrbox (p,pdata)
UM_srf_boundary *p;
UU_REAL **pdata;
{
	int nb;
	UU_REAL *data;

	data = *pdata;

	p->toler    = *data; data++;
	nb = p->nb  = abs((int) *data); data++;
	p->ummx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));
	p->vmmx = (UM_2Dcoord *) uu_malloc(nb*sizeof(UM_2Dcoord));

	*pdata = data;

	S_get_uvmmx (nb,p,pdata);
}

/*********************************************************************
**   FUNCTION : int S_get_bndrlist0 (p,data)
**      Retrieves a boundary list from a surface Unibase record.
**    PARAMETERS
**       INPUT  :
**         data   - stored surface list
**       OUTPUT :
**         p     - boundary data
**    RETURNS :
**       UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_get_bndrlist0 (p,data)
UM_srf_boundary *p;
UU_REAL *data;
{
	int status,nb;
	UU_REAL tol;

	if (!data) return (UU_FAILURE);
/*
..... check if it's UV_BOX_LIST (*data <= 0);
..... if yes, free the data pointer & get out.
..... check tolerance; if database toler. is 20% bigger than the current one,
..... free the data pointer, get out and recalculate the boundary
*/
	if (*data <= 0.) return (UU_FAILURE);

	tol = ncl_get_boundary_toler ();
	if (TOL_CHANGED(tol,*data)) return (UU_FAILURE);

	status = UU_SUCCESS;

	S_get_bndrbox (p,&data);
	nb = p->nb;

	p->np = (int *) uu_malloc((nb+1)*sizeof(int));

	S_get_bndrys (nb,p,data);

	return (status);
}

/*********************************************************************
**   FUNCTION : int ncl_get_surflist (type,sf,ptr)
**      Retrieves a list of given type (box,boundary,...)
**      from a surface Unibase record.
**    PARAMETERS
**       INPUT  :
**         type   - specifies the list to get (BOX_LIST, BOUNDARY_LIST, ...)
**         sf     - surface
**       OUTPUT :
**         ptr    - pointer to a structure to put the list data into.
**    RETURNS :
**       UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS :
**             If a BOUNDARY LIST, always intializes ptr->cvlst, ptr->uvlst;
**             Also, allocates all other pointers in the boundary structure
**             only if the data are in the Unibase
**    WARNINGS     : none
*********************************************************************/
int ncl_get_surflist (type,sf,ptr)
NCL_surflist_type type;
struct NCL_fixed_databag *sf;
char *ptr;
{
	int status,i;
	UU_REAL *data = UU_NULL;

	if (NCLX_internal_geom)
	{
		NCLX_mdl_struct *geo = UU_NULL;
		NCLX_mdl_surf *sf1;
		NCLX_mdl_trimsf *sf2;

		status = NclxMdlFindGeo (sf->key, &geo);
		if (status != NCLX_SUCCESS) goto Cont;

		data = UU_NULL;

		switch (sf->rel_num)
		{
			case NCLX_MDL_NSURF:
			case NCLX_MDL_SURF:
			{
				sf1 = (NCLX_mdl_surf *)geo;
				switch (type)
				{
					case BOX_LIST:
						data = sf1->boxlst;
						break;
					case UV_BOX_LIST:
					case WHOLE_BOUNDARY_LIST:
						data = sf1->bndrylst;
						break;
				}
				break;
			}
			case NCLX_MDL_TRIMSF:
			{
				sf2 = (NCLX_mdl_trimsf *)geo;
				switch (type)
				{
					case BOX_LIST:
						data = sf2->boxlst;
						break;
					case UV_BOX_LIST:
					case WHOLE_BOUNDARY_LIST:
						data = sf2->bndrylst;
						break;
				}
				break;
			}
			default:
				data   = UU_NULL;
				status = UU_FAILURE;
		}
	}
/*
... NCL own geometry
*/
	else
		status = ncl_surflist_data (type, sf, &data);
Cont:;
	switch (type)
	{
		case TESSELLATION_LIST:
		{
			UM_tessellation *t;

			if (!data) return (UU_FAILURE);

			t = (UM_tessellation *) ptr;
			status = ncl_get_tesslst0 (t,data,sf->rel_num);

			break;
		}
		case BOX_LIST:
		{
			UM_3D_box *box;

			if (!data) return (UU_FAILURE);

			status = UU_SUCCESS;

			box = (UM_3D_box *) ptr;
			for (i=0; i<UM_3DBOX_NUM_VERT; i++, data += 3)
				um_vctovc (data, box->ver[i]);

			break;
		}
		case UV_BOX_LIST:
		case WHOLE_BOUNDARY_LIST:
		{
			UM_srf_boundary *p;
			p = (UM_srf_boundary *) ptr;

			p->cvpts = p->uvpts = UU_NULL;
			p->ummx = p->vmmx = UU_NULL;
			p->np = UU_NULL;

			if (type == UV_BOX_LIST)
			{
				if (!data) return (UU_FAILURE);
				status = UU_SUCCESS;
				S_get_bndrbox (p,&data);
			}
			else
			{
				p->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
				p->uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
				uu_list_init (p->cvpts, sizeof(UM_coord), 100, 100);
				uu_list_init (p->uvpts, sizeof(UM_coord), 100, 100);

				status = S_get_bndrlist0 (p,data);
			}

			break;
		}

		default:
			status = UU_FAILURE;
	}

	return (status);
}

/*********************************************************************
**   FUNCTION : int ncl_get_bndrlist96 (sf,p)
**      Retrieves a boundary list from a surface Unibase record.
**    PARAMETERS
**       INPUT  :
**         sf     - surface
**       OUTPUT :
**         p     - boundary data
**    RETURNS :
**       UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_bndrlist96 (sf,p)
struct NCL_fixed_databag *sf;
UM_srf_boundary *p;
{
	int status;
	UU_REAL *data = UU_NULL;

	ncl_surflist_data (WHOLE_BOUNDARY_LIST, sf, &data);
	status = S_get_bndrlist0 (p,data);

	return (status);
}

/*********************************************************************
**   FUNCTION : int ncl_get_bndrlist (sf,p)
**      Retrieves a boundary list from a surface Unibase record.
**    PARAMETERS
**       INPUT  :
**         sf     - surface
**       OUTPUT :
**         p     - boundary data
**    RETURNS :
**       UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_bndrlist (sf,p,flag)
struct NCL_fixed_databag *sf;
UM_srf_boundary *p;
int flag;
{
	int status,nb,dfl;
	UU_REAL *data = UU_NULL;
	UU_REAL tol,dtol;

	ncl_surflist_data (WHOLE_BOUNDARY_LIST, sf, &data);

	if (!data) return (UU_FAILURE);
/*
..... check if it's UV_BOX_LIST (*data <= 0);
..... if yes, free the data pointer & get out.
..... check tolerance; if database toler. is 20% bigger than the current one,
..... free the data pointer, get out and recalculate the boundary
*/
	dfl = 0;
	if (data[0] <= 0.)
	{
		dfl = (int) data[0];
		dfl = -dfl;
		if (dfl > 99) return (UU_FAILURE);
		data++;
	}
	if (dfl < flag) return (UU_FAILURE);

	dtol = data[0];
	tol = ncl_get_boundary_toler ();
	if (TOL_CHANGED(tol,dtol)) return (UU_FAILURE);
	nb = (int) data[1];

	status = UU_SUCCESS;

	p->toler = dtol;
	p->nb = nb;

	data += 2;

	S_get_uvmmx (nb,p,&data);
	S_get_bndrys (nb,p,data);

	return (status);
}

/*********************************************************************
**   FUNCTION : int ncl_get_tesslst (sf,ptr)
**      Retrieves a tessellation list from a surface Unibase record,
**      does not accept if TOL_CHANGED.
**    PARAMETERS
**       INPUT  :
**         sf     - surface
**       OUTPUT :
**         ptr    - pointer to a structure to put the list data into.
**    RETURNS :
**       UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_tesslst (sf,ptr)
struct NCL_fixed_databag *sf;
char *ptr;
{
	int status;
	UU_REAL *data = UU_NULL,tol,dtol;
	UM_tessellation *t;

	status = ncl_surflist_data (TESSELLATION_LIST, sf, &data);

	if (!data) return (UU_FAILURE);

	t = (UM_tessellation *) ptr;
	tol = um_get_tess_toler();
	dtol = fabs(*data);
	if (TOL_CHANGED(tol,dtol)) return (UU_FAILURE);

	status = ncl_get_tesslst0 (t,data,sf->rel_num);

	return (status);
}
