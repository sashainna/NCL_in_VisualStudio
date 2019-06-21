
/*********************************************************************
**    NAME         :  nesolid.c
**       CONTAINS:
**					nclf_define_solid()
**					nclf_load_solid()
**					nclf_save_solid()
**					nclf_get_solid()
**					nclf_get_solid_comp()
**					ncl_init_solidrec()
**             ncl_create_solid()
**             ncl_draw_solid()
**             ncl_solid_labloc()
**             ncl_solid_to_stock()
**             ncl_solid_store_components()
**    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nesolid.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       08/17/15 , 17:51:22
*********************************************************************/

#include "usysdef.h"
#include "lipv.h"
#include "umath.h"
#include "ulist.h"
#include "uhep.h"
#include "udebug.h"
#include "umoveb.h"
#include "class.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdcoord.h"
#include "mdgenent.h"
#include "mdpick.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include	"mfort.h"
#include	"mdrel.h"
#include	"mdcpln.h"
#include	"mgeom.h"
#include	"nccs.h"
#include	"ncl.h"
#include	"nclfc.h"
#include	"nclmplay.h"

#define CO25 .906308

extern UU_LOGICAL UM_set_constituent_pickids;

void ncl_init_solidrec();
static void S_copy_curve_pts();

/*********************************************************************
**    E_FUNCTION :  nclf_define_solid(styp,label,sub,params,nparms,
**                                    calcfl,nclkey,ierr)
**       Create a visual solid.  Fortran callable routine.
**    PARAMETERS   
**       INPUT  : 
**          styp     Type of solid.
**          label    Entity label.
**          sub      Entity subscript.
**          params   Solid canonical data.
**          nparms   Number of values in 'params'.
**          calcfl   1 = Curve points for extruded, revolved, or
**                   contour solid need to be calculated.
**       OUTPUT :  
**          nclkey   Key of created solid.
**          ierr     Returns non-zero if solid could not be created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_define_solid(styp,label,sub,params,nparms,calcfl,nclkey,ierr)
int *styp;
char *label;
int *sub;
UU_REAL params[];
int *nparms;
int *calcfl;
UU_KEY_ID *nclkey;
UM_int2 *ierr;
{
	int status,nc,np,nq,i;
	UU_LOGICAL alloc,xfrm;
	UU_REAL *oparms,*pto;
	UU_LIST dlist;
	UM_tessellation tess;
	UM_coord *pts;
	struct UM_solid_rec solid;
	struct UM_surfattr_rec attr;
/*
.....Initialize routine
*/
	*ierr = 0;
	*nclkey = 0;
	uu_list_init0(&dlist);
	tess.np = 0;
	alloc = UU_FALSE;
	xfrm = UU_TRUE;
/*
.....Initialize solid record
*/
	i = 1;
	if (*styp == UM_COMPOS_SOLID) i = params[0];
	ncl_init_solidrec(&solid,&attr,*styp,i,label,*sub);
/*
.....Create the solid entity
*/
	status = uc_create_data(&solid,UM_DEFAULT_TF,&attr);
	if (status != UU_SUCCESS) goto failed;
	*nclkey = solid.key;
/*
.....Calculate the generating curve
*/
	oparms = params;
	np = *nparms;
	if (*calcfl == 1)
	{
		if (*styp == UM_EXTRUDED_SOLID || *styp == UM_REVOLVED_SOLID ||
			*styp == UM_CONTOUR_SOLID)
		{
			status = S_calc_curve(*styp,params,*nparms,&oparms,&np);
			if (status != UU_SUCCESS) goto failed;
			alloc = UU_TRUE;
			xfrm = UU_FALSE;
		}
/*
.....Calculate the bounding box
*/
      else if (*styp == UM_BOX_SOLID && np < 12)
		{
			status = S_calc_box(params,*nparms,params,&np);
			if (status != UU_SUCCESS) goto failed;
		}
	}
/*
.....The generating curve is provided
*/
	else
	{
/*
........Solid of revolution is being generated
........without associated tangent vectors
........So these must be generated here
*/
		if (*styp == UM_REVOLVED_SOLID)
		{
			nq = oparms[8]*6 + 9;
			pto = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*nq*2);
			if (pto == UU_NULL) goto failed;
			for (i=0;i<9;i++) pto[i] = oparms[i];
			pts = (UM_coord *)&pto[9];
			i = pto[8];
			S_copy_curve_pts(&oparms[9],i,pts,&nc);
			pto[8] = nc;
			if (np<nq) ncl_calc_tanvecs(pts,nc,&pts[nc]);
			oparms = pto;
			np = 9 + nc*6;
			alloc = UU_TRUE;
		}
	}
/*
.....Run solid through MODSYS
.....To get modelling system coordinates
*/
	ncl_solid_mcstowcs(solid.type,oparms,xfrm);
/*
.....Calculate bounding box, display list, & tessellation list
*/
	status = ncl_solid_calc_lists(&solid,oparms,np,&dlist,&tess);
	if (status != UU_SUCCESS) goto failed;
/*
.....Update solid
*/
	ur_update_data_fixed(&solid);
/*
.....Store component surface keys
.....for composite solid
*/
	if (*styp == UM_COMPOS_SOLID)
	{
		status = ncl_solid_store_components(&solid,oparms);
		if (status != UU_SUCCESS) goto failed;
	}
	else
	{
/*
.....Store canonical data
*/
		status = ur_update_data_varlist(solid.key,1,oparms,1,np);
		if (status != UU_SUCCESS) goto failed;
		status = ncl_retrieve_data(&solid,sizeof(struct UM_solid_rec));
/*
.....Store display & tessellation lists
*/
		pts = (UM_coord *)UU_LIST_ARRAY(&dlist);
		nc = UU_LIST_LENGTH(&dlist);
		status = ur_update_data_varlist(solid.key,2,pts,1,nc);
		if (status != UU_SUCCESS) goto failed;
		status = ncl_retrieve_data(&solid,sizeof(struct UM_solid_rec));

		status = ncl_store_surflist1(TESSELLATION_LIST,&solid,&tess);
		if (status != UU_SUCCESS) goto failed;
		status = ncl_retrieve_data(&solid,sizeof(struct UM_solid_rec));
	}
	goto done;
/*
.....Failed
*/
failed:;
	if (status != UX_NO_ACCESS) status = UU_FAILURE;
	if (*nclkey != 0) uc_delete(*nclkey);
/*
.....End of routine
.....Free the memory
*/
done:;
	if (!UU_LIST_NULLPTR(&dlist)) uu_list_free(&dlist);
	if (tess.np != 0) um_free_tess(&tess);
	if (alloc) uu_free(oparms);
	if (status == UX_NO_ACCESS) *ierr = 2;
	else if (status != UU_SUCCESS) *ierr = 1;
	return;
}

/*********************************************************************
**    E_FUNCTION :  nclf_load_solid(fnam,label,sub,nent,ierr)
**       Create a visual solid from a stored stock file.
**       Fortran callable routine.
**    PARAMETERS   
**       INPUT  : 
**          fnam     Stock file to load.
**          label    Entity label.
**          sub      Entity subscript.
**          trfl     1 = Transform through matrix when loading solid.
**          mx       Transformation matrix when 'trfl' = 1.
**       OUTPUT :  
**          nent     Number of solids created.
**          ierr     Returns non-zero if solid could not be created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_load_solid(fnam,label,sub,nent,trfl,mx,ierr)
char *fnam;
char *label;
int *sub;
int *nent;
UM_int2 *trfl;
UU_REAL mx[];
UM_int2 *ierr;
{
	int status,j,k,styp,nstk[2],nparms,mode,fstat,calcfl,tsub,sub1,nc,ncomp;
	int ncids,idn[2];
	UM_int2 idx;
	UU_LOGICAL lcomp;
	UU_KEY_ID nclkey,cids[50];
	UU_REAL *ptr,parms[12],nclver;
	char tlab[NCL_MAX_LABEL],lab1[NCL_MAX_LABEL],*cptr1,*cptr2;
	UM_coord *pts;
	UM_transf tf;
	UX_pathname fullname,tnam;
	LW_stock_struc *sp[2],*sd;
	FILE *fd;
	struct UM_solid_rec solid;
	struct UM_surfattr_rec attr;
	union {UU_REAL rval; int ival[2];} tprm;
/*
.....Initialize routine
*/
	*ierr = 0;
	*nent = 0;
	strcpy(tlab,label);
	tsub = *sub;
	sp[0] = sp[1] = 0;
	lcomp = UU_FALSE;
	ncids = 0;
	idx = 169; getsc(&idx,&nclver);
/*
.....Make sure the file exists
*/
	mode = UX_EXISTS|UX_READ;
	status = ux_file_inquire(UU_NULL,UU_NULL,fnam,UU_NULL,UU_NULL,
		&mode,&fstat,tnam,UX_NPRTERRS);
/*
.....Open the file for reading
*/
	if (status == UU_SUCCESS) status = ux_fopen0(fnam,"r",&fd);
/*
.....Open failed
.....try the NCL_INCDIR directory
*/
	if (status != UU_SUCCESS)
	{
		ul_build_full_fname("NCL_INCDIR",fnam,".stk",fullname);
		mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
		status = ux_file_inquire(UU_NULL,UU_NULL,fullname,UU_NULL,UU_NULL,
			&mode,&fstat,tnam,UX_NPRTERRS);
		if (status != UU_SUCCESS) goto openerr;
		status = ux_fopen0(fullname,"r",&fd);
		if (status != UU_SUCCESS) goto openerr;
	}
/*
.....Load stock commands
*/
	ul_ipv_count_stock_cmds(fd,nstk);
	if (nstk[0] != 0)
	{
		sp[0] = (LW_stock_struc *)uu_malloc(sizeof(LW_stock_struc)*nstk[0]);
		if (sp[0] == UU_NULL) goto failed;
	}
	if (nstk[1] != 0)
	{
		sp[1] = (LW_stock_struc *)uu_malloc(sizeof(LW_stock_struc)*nstk[1]);
		if (sp[1] == UU_NULL) goto failed;
	}
	status = ul_ipv_load_stock_cmds(fd,UU_FALSE,idn,sp,nstk,UU_TRUE);
	if (status != UU_SUCCESS) goto failed;
/*
.....Define the solid
*/
	for (k=0;k<2;k++)
	{
		for (j=0;j<nstk[k];j++)
		{
			sd = &sp[k][j];
			if (sd->type == LW_STOCK_BOX)
			{
				styp = UM_BOX_SOLID;
				pts = (UM_coord *)uu_malloc(sizeof(UM_coord)*4);
				if (pts == UU_NULL) goto failed;
				boxcnv(&sd->data[0],&sd->data[3],pts);
				uu_free(sd->data); sd->data = (UU_REAL *)pts;
			}
			else if (sd->type == LW_STOCK_CONE) styp = UM_CONE_SOLID;
			else if (sd->type == LW_STOCK_CYLINDER) styp = UM_CYLINDER_SOLID;
			else if (sd->type == LW_STOCK_SPHERE) styp = UM_SPHERE_SOLID;
			else if (sd->type == LW_STOCK_TORUS) styp = UM_TORUS_SOLID;
			else if (sd->type == LW_STOCK_SWEEP) styp = UM_EXTRUDED_SOLID;
			else if (sd->type == LW_STOCK_REVOLVE) styp = UM_REVOLVED_SOLID;
			else if (sd->type == LW_STOCK_FILE) styp = UM_STL_SOLID;
			else if (sd->type == LW_STOCK_COMPOS) styp = UM_COMPOS_SOLID;
			else goto failed;
/*
........Transform the solid
*/
			if (*trfl == 1 && styp != UM_COMPOS_SOLID)
			{
				ncl_34mx_to_43mx(mx,tf);
				ncl_xform_solid(styp,&sd->data,tf,UU_TRUE);
			}
/*
........Box solid
*/
			switch (styp)
			{
			case UM_BOX_SOLID:
				nparms = 12;
				break;
/*
........Cone, Torus solid
*/
			case UM_CONE_SOLID:
			case UM_TORUS_SOLID:
				nparms = 8;
				break;
/*
........Cylinder solid
*/
			case UM_CYLINDER_SOLID:
				nparms = 7;
				break;
/*
........Sphere solid
*/
			case UM_SPHERE_SOLID:
				nparms = 4;
				break;
/*
........Extruded solid
*/
			case UM_EXTRUDED_SOLID:
				nparms = sd->data[3]*3 + 4;
				break;
/*
........Revolved solid
*/
			case UM_REVOLVED_SOLID:
				nparms = sd->data[8]*3 + 9;
				break;
/*
........STL solid
*/
			case UM_STL_SOLID:
				cptr1 = (char *)sd->data;
				nc = strlen(cptr1);
				nparms = nc/4+1 + 14;
				ptr = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*nparms);
				if (ptr == UU_NULL) goto failed;
				tprm.ival[0] = sd->units;
				tprm.ival[1] = sd->bin;
				ptr[0] = tprm.rval;
				tprm.ival[0] = *trfl;
				tprm.ival[1] = nc;
				ptr[1] = tprm.rval;
				ncl_34mx_to_43mx(mx,&ptr[2]);
				cptr2 = (char *)&ptr[14];
				strcpy(cptr2,cptr1);
				uu_free(sd->data); sd->data = (UU_REAL *)ptr;
				break;
/*
........Composite solid
*/
			case UM_COMPOS_SOLID:
				lcomp = UU_TRUE;
				ncomp = sd->bin;
				if (ncomp == 0) ncomp = 10000;
				ncids = 0;
				continue;
/*
........Unrecognized solid
*/
			default:
				if (sd->data != UU_NULL) uu_free(sd->data);
				goto failed;
			}
/*
........Label the solid
*/
			if (lcomp)
			{
				strcpy(lab1,"@UN");
				sub1 = 0;
			}
			else if (nstk[0]+nstk[1] == 1 && nclver > 10.149)
			{
				strcpy(lab1,tlab);
				sub1 = tsub;
			}
			else
				ncl_label_multiple(tlab,&tsub,lab1,&sub1);
/*
........Create the solid
*/
			calcfl = 0;
			nclf_define_solid(&styp,lab1,&sub1,sd->data,&nparms,&calcfl,&nclkey,
				ierr);
			uu_free(sd->data);
			if (*ierr != 0) goto failed;
/*
........Update the solid color
*/
			if (sd->color != 0)
				ncl_update_color(nclkey,sd->color);
/*
........A composite solid is being created
...........Increment Composite counts
*/
			if (lcomp)
			{
				ncomp--;
				cids[ncids++] = nclkey;
				ur_update_displayable(nclkey,UM_NEVERDISPLAYABLE);
/*
...........Create composite stock
*/
				if (ncomp == 0)
				{
					parms[0] = 0;
					parms[1] = 2;
					parms[2] = 3;
					if (nstk[0]+nstk[1]-1 == ncids && nclver > 10.149)
					{
						strcpy(lab1,tlab);
						sub1 = tsub;
					}
					else
						ncl_label_multiple(tlab,&tsub,lab1,&sub1);
					ncl_init_solidrec(&solid,&attr,UM_COMPOS_SOLID,UU_FALSE,lab1,
						sub1);
					status = ncl_create_solid(&solid,UM_DEFAULT_TF,&attr);
					if (status != UU_SUCCESS) goto failed;
					status = ur_update_data_varlist(solid.key,1,parms,1,3);
					if (status != UU_SUCCESS) goto failed;
					status = ur_update_data_varlist(solid.key,4,cids,1,ncids);
					if (status != UU_SUCCESS) goto failed;
					nclkey = solid.key;
					lcomp = UU_FALSE;
				}
				else
					continue;
			}
/*
........Store the label
*/
			status = ncl_store_wf2(nclkey,UM_SOLID_REL,lab1,sub1);
			if (status != UU_SUCCESS) goto failed;
/*
........Display the entity
*/
			solid.key = nclkey;
			status = ncl_retrieve_data(&solid,sizeof(struct UM_solid_rec));
			if (status != UU_SUCCESS) goto failed;
			status = uc_display(&solid);
			if (status != UU_SUCCESS) goto failed;
			*nent += 1;
		}
	}
	goto done;
/*
.....Could not open file
*/
openerr:
	*ierr = 2;
	goto done;
/*
.....Failed to create solid
*/
failed:
	if (*ierr == 0) *ierr = 1;
/*
.....End of routine
*/
done:
	if (sp[0] != UU_NULL) uu_free(sp[0]);
	if (sp[1] != UU_NULL) uu_free(sp[1]);
	return;
}

/*********************************************************************
**    E_FUNCTION :  nclf_save_solid(istk,fnam,ierr)
**       Save visual solids to an external stock file.
**       Fortran callable routine.
**    PARAMETERS   
**       INPUT  : 
**          istk     1 = Save as stocks, 2 = Fixtures.
**          fnam     Stock file to save.
**          nso      Number of stocks to save.
**       OUTPUT :  
**          ierr     Returns non-zero if file could not be not be created.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_save_solid(istk,fnam,nso,ierr)
UM_int2 *istk;
char *fnam;
UM_int2 *nso;
UM_int2 *ierr;
{
	int status,i,nstk[2],nent,np,nkeys,inc,rel,nsol,nsrf;
	UM_int2 idx;
	UU_LOGICAL compfl;
	UU_REAL *p;
	LW_stock_struc *sp[2],*sd,stock;
	UU_LIST elist;
	FILE *fd;
	struct UM_solid_rec solid,csol;
/*
.....Initialize routine
*/
	*ierr = 0;
	nent = *nso;
/*
.....Retrieve solid keys
.....and store in stock structures
*/
	uu_list_init(&elist,sizeof(LW_stock_struc),nent,10);
	if (UU_LIST_NULLPTR(&elist)) goto failed;
	for (i=0;i<nent;i++)
	{
		idx = i + 1;
		gtsky(&idx,&solid.key);
		status = ncl_retrieve_data(&solid,sizeof(struct UM_solid_rec));
		if (status != UU_SUCCESS) goto failed;
/*
.....Composite solid
*/
		compfl = UU_FALSE;
		nkeys = 0;
		if (solid.type == UM_COMPOS_SOLID)
		{
			ncl_solid_count_components(&solid,&nsol,&nsrf);
			if (nsol == 0) continue;
			compfl = UU_TRUE;
			csol = solid;
			nkeys = csol.no_netkey;
			solid.no_netkey = nsol;
		}
/*
.....Loop through solids
*/
		for (inc=-1;inc<nkeys;inc++)
		{
			if (compfl && inc != -1)
			{
				solid.key = csol.netkey[inc];
				ur_retrieve_data_relnum(solid.key,&rel);
				if (rel != UM_SOLID_REL) continue;
				status = ncl_retrieve_data(&solid,sizeof(struct UM_solid_rec));
				if (status != UU_SUCCESS) goto failed;
			}
/*
........Apply MODSYS
*/
			np = solid.no_sdata;
			if (np != 0)
			{
				p = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*np);
				if (p == UU_NULL) goto failed;
				status = ur_retrieve_data_varlist(solid.key,1,p,1,np);
				ncl_solid_wcstomcs(solid.type,p,UU_TRUE);
			}
			else
				p = UU_NULL;
			solid.sdata = p;
			status = ncl_solid_to_stock(&solid,&stock);
			if (status != UU_SUCCESS) goto failed;
			uu_list_push(&elist,&stock);
			if (p != UU_NULL) uu_free(p);
		}
	}
/*
.....Open file for writing
*/
	status = ux_fopen0(fnam,"w",&fd);
	if (status != UU_SUCCESS)
	{
		*ierr = 2;
		goto failed;
	}
/*
.....Save solids as stocks
*/
	if (*istk == 1)
	{
		sp[0] = (LW_stock_struc *)UU_LIST_ARRAY(&elist);
		sp[1] = UU_NULL;
		nstk[0] = UU_LIST_LENGTH(&elist);
		nstk[1] = 0;
	}
/*
.....Save solids as fixtures
*/
	else if (*istk == 2)
	{
		sp[0] = UU_NULL;
		sp[1] = (LW_stock_struc *)UU_LIST_ARRAY(&elist);
		nstk[0] = 0;
		nstk[1] = UU_LIST_LENGTH(&elist);
	}
/*
.....Unrecognized stock type
*/
	else
		goto failed;
/*
.....Save stock/fixture commands
*/
	status = ul_ipv_save_stock_cmds(fd,UU_TRUE,UU_FALSE,sp,nstk);
	ux_fclose0(fd);
	if (status != UU_SUCCESS) goto failed;
	goto done;
/*
.....Failed to create solid
*/
failed:;
	if (*ierr == 0) *ierr = 1;
/*
.....End of routine
*/
done:;
	if (!UU_LIST_NULLPTR(&elist))
	{
		sd = (LW_stock_struc *)UU_LIST_ARRAY(&elist);
		nent = UU_LIST_LENGTH(&elist);
		for (i=0;i<nent;i++)
			if (sd[i].data != UU_NULL) uu_free(sd[i].data);
		uu_list_free(&elist);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION :  nclf_get_solid(nclkey,styp,params,nparms)
**       Return the canonical data of a visual solid.  The canonical
**       data will be formatted for output and for the OBTAIN
**       statement, meaning units conversion will be applied and all
**       length vectors will be converted to a unit vector and distance.
**       Fortran callable routine.
**    PARAMETERS   
**       INPUT  : 
**          nclkey   Key of solid.
**       OUTPUT :  
**          styp     Type of solid.
**          params   Solid canonical data.
**          nparms   Number of values in 'params'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_get_solid(nclkey,styp,params,nparms)
UU_KEY_ID *nclkey;
int *styp;
UU_REAL params[];
int *nparms;
{
	int i,status,numsf,numso,rel;
	UM_int2 i3,i4;
	UU_REAL *ptr,*ptx;
	UM_coord *pts,*pto;
	struct UM_solid_rec solid;
/*
.....Initialize routine
*/
	*styp = 0;
	*nparms = 0;
	i3 = 3; i4 = 4;
/*
.....Retrieve the solid data
*/
	solid.key = *nclkey;
	status = ncl_retrieve_data(&solid,sizeof(struct UM_solid_rec));
/*
.....Copy the canonical data
*/
	ptr = UU_NULL;
	if (status == UU_SUCCESS)
	{
		*styp = solid.type;
		if (solid.type != UM_COMPOS_SOLID && solid.type != UM_STL_SOLID)
		{
			ptr = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*solid.no_sdata);
			if (ptr == UU_NULL) goto done;
			for (i=0;i<solid.no_sdata;i++) ptr[i] = solid.sdata[i];
			pts = (UM_coord *)ptr;
			pto = (UM_coord *)&params[0];
			ptx = (UU_REAL *)&params[0];
		}
/*		ncl_solid_wcstomcs(solid.type,ptr,UU_TRUE);*/
		switch (solid.type)
		{
/*
........Box
*/
		case UM_BOX_SOLID:
			um_vctovc(pts[0],pto[0]);
			um_vcplvc(pts[0],pts[1],pto[1]);
			um_vcplvc(pto[1],pts[2],pto[1]);
			um_vcplvc(pto[1],pts[3],pto[1]);
			conpt(pto[0],&i3);
			conpt(pto[1],&i3);
			um_vctovc(pts[1],pto[2]);
			conpt(pto[2],&i3);
			um_vctovc(pts[2],pto[3]);
			conpt(pto[3],&i3);
			um_vctovc(pts[3],pto[4]);
			conpt(pto[4],&i3);
			*nparms = 15;
			break;
/*
........Cone, Cylinder
*/
		case UM_CONE_SOLID:
		case UM_CYLINDER_SOLID:
			um_vctovc(pts[0],pto[0]);
			conpt(pto[0],&i3);
			conpt(pts[1],&i4);
			ptx[6] = um_mag(pts[1]);
			um_unitvc(pts[1],pto[1]);
			UM_len_inttoext(ptr[6],ptx[7]);
			*nparms = 8;
			if (solid.type == UM_CONE_SOLID)
			{
				UM_len_inttoext(ptr[7],ptx[8]);
				*nparms = 9;
			}
			break;
/*
........Sphere
*/
		case UM_SPHERE_SOLID:
			um_vctovc(pts[0],pto[0]);
			conpt(pto[0],&i3);
			UM_len_inttoext(ptr[3],ptx[3]);
			*nparms = 4;
			break;
/*
........Torus
*/
		case UM_TORUS_SOLID:
			um_vctovc(pts[0],pto[0]);
			conpt(pto[0],&i3);
			conpt(pts[1],&i4);
			um_unitvc(pts[1],pto[1]);
			UM_len_inttoext(ptr[6],ptx[6]);
			UM_len_inttoext(ptr[7],ptx[7]);
			*nparms = 8;
			break;
/*
........Extruded
*/
		case UM_EXTRUDED_SOLID:
			conpt(pts[0],&i4);
			ptx[3] = um_mag(pts[0]);
			um_unitvc(pts[0],pto[0]);
			UM_len_inttoext(ptx[3],ptx[3]);
			ptx[4] = ptr[3];
			pts = (UM_coord *)&solid.sdata[4];
			pto = (UM_coord *)&params[5];
			um_vctovc(pts[0],pto[0]);
			conpt(pto[0],&i3);
			*nparms = 8;
			break;
/*
........Contour
*/
		case UM_CONTOUR_SOLID:
			UM_len_inttoext(ptr[0],ptx[0]);
			ptx[1] = ptr[1];
			ptx[5] = um_mag(&ptr[2]);
			pts = (UM_coord *)&solid.sdata[2];
			pto = (UM_coord *)&params[2];
			um_unitvc(pts[0],pto[0]);
			conpt(pto[0],&i4);
			UM_len_inttoext(ptx[5],ptx[5]);
			ptx[6] = ptr[5];
			pts = (UM_coord *)&solid.sdata[6];
			pto = (UM_coord *)&params[7];
			um_vctovc(pts[0],pto[0]);
			conpt(pto[0],&i3);
			*nparms = 10;
			break;
/*
........Revolved
*/
		case UM_REVOLVED_SOLID:
			um_vctovc(pts[0],pto[0]);
			conpt(pto[0],&i3);
			um_unitvc(pts[1],pto[1]);
			conpt(pto[1],&i4);
			ptx[6] = ptr[6];
			ptx[7] = ptr[7];
			ptx[8] = ptr[8];
			um_vctovc(pts[3],pto[3]);
			conpt(pto[3],&i3);
			*nparms = 12;
			break;
/*
........STL
*/
		case UM_STL_SOLID:
			*nparms = 0;
			for (i=0;i<solid.no_sdata;i++) params[i] = solid.sdata[i];
/*
........Composite
*/
		case UM_COMPOS_SOLID:
			numsf = 0;
			numso = 0;
			for (i=0;i<solid.no_netkey;i++)
			{
				um_retrieve_data_relnum(solid.netkey[i],&rel);
				if (rel == UM_SOLID_REL) numso++;
				else numsf++;
			}
			*nparms = 2;
			params[0] = numsf;
			params[1] = numso;
		}
	}
done:;
	if (ptr != UU_NULL) uu_free(ptr);
	return;
}

/*********************************************************************
**    E_FUNCTION :  nclf_get_solid_comp(nclkey,ckey,kpt,knsf)
**       Returns the component entities of a composite solid.
**       Fortran callable routine.
**    PARAMETERS   
**       INPUT  : 
**          nclkey   Key of composite solid.
**          kpt      Which to component to return (1:knsf).
**       OUTPUT :  
**          ckey     Key of compenent entity.
**          knsf     Number of components in solid.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_get_solid_comp(nclkey,ckey,kpt,knsf)
UM_int4 *nclkey,*ckey;
UM_int2 *kpt,*knsf;
{
	struct UM_solid_rec sol;
	int i,nsf,status;
/*
.....Initialize routine
*/
	*ckey = 0;
/*
.....Get composite solid
*/
	sol.key = *nclkey;
	status = ncl_retrieve_data(&sol,sizeof(sol));
	if (status != UU_SUCCESS) return;
	if (sol.type != UM_COMPOS_SOLID) return;
/*
.....Verify the requested entity is within range
*/
	i = *kpt - 1;
	nsf = sol.no_netkey;
	if (i < 0 || i >= sol.no_netkey) return;
/*
.....Return the component key
*/
	*ckey = sol.netkey[i];
	*knsf = nsf;
	return;
}

/*********************************************************************
**    E_FUNCTION :  ncl_init_solidrec(solid,attr,styp,closed,label,sub)
**       Initializes a solid record and its attribute record for
**       storage in the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          styp     Type of solid.  1 = Box.
**          closed   1 = Solid is closed, 0 = Open.
**          label    Entity label.
**          sub      Entity subscript.
**       OUTPUT :  
**          solid    Initialized Solid record.
**          attr     Initialized Attribute record.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_init_solidrec(solid,attr,styp,closed,label,sub)
struct UM_solid_rec *solid;
struct UM_surfattr_rec *attr;
int styp,closed;
char *label;
int sub;
{
	int i;
/*
.....Setup solid record
*/
	ur_setup_data(UM_SOLID_REL,solid,sizeof(struct UM_solid_rec));
/*
.....Initialize solid structure
*/
	strcpy(solid->label,label);
	um_init_lablocal(&solid);
	solid->subscr = sub;
	solid->type = styp;
	solid->closed = closed;
	for (i=0;i<6;i++) solid->box[i] = 0.;
}

/*********************************************************************
**    E_FUNCTION :  ncl_create_solid(eptr,tfmat,attrptr)
**       Stores a Visual Solid in the Unibase and updates the
**       transformation matrix and attribute bundle.
**    PARAMETERS   
**       INPUT  : 
**          eptr     Solid entity to stores in Unibase.
**          tfmat    Associated transformation matrix.
**          attrptr  Associated attribute bundle.
**       OUTPUT :  none
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_solid(eptr,tfmat,attrptr)
struct UM_solid_rec *eptr;
UM_transf tfmat;
struct UM_surfattr_rec *attrptr;
{
	struct UM_transf_rec transpacket;
	int status;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Create  master tuple in Unibase
*/
	if (ncl_create_data(eptr) != 0)
	{
		status = UU_FAILURE;
		goto done;
	}
/*
.....Initialize transformation
*/
	if (tfmat != UM_DEFAULT_TF)
	{
		transpacket.key = eptr->key;
		transpacket.rel_num = UM_TRANSFORM_REL;
		um_tftotf(tfmat,transpacket.tfmat);
		if (ur_update_transf(&transpacket) != 0)
		{
			status = UU_FAILURE;
			goto done;
		}
	}
/*
.....Initialize attribute structure
.....Associate the attributes with the entity
*/
	status = ncl_init_surf_attr(eptr->key,attrptr,NCLI_SOLID);
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_draw_solid(eptr,tfmat,attrptr)
**       Displays a Visual Solid.
**    PARAMETERS   
**       INPUT  : 
**          eptr     Solid entity to draw.
**          tfmat    Associated transformation matrix.
**          attrptr  Associated attribute bundle.
**       OUTPUT :  none
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_draw_solid(eptr,tfmat,attrptr)
struct UM_solid_rec *eptr;
UM_transf tfmat;
struct UM_surfattr_rec *attrptr;
{
	int i,lst,nc,status,style,color,ecolor;
	UU_LOGICAL alloc,shaded,savpik,highlighting;
	UM_coord *pts;
	UU_LIST dlist;
	UM_tessellation tess,tess1;
	UM_transf tf;
	struct UM_srfdatabag e1;
	struct UM_surfattr_rec attr;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	alloc = UU_FALSE;
	shaded = attrptr->shaded && ncl_shading_mode();
	savpik = UM_set_constituent_pickids;
/*
.....Display Composite solid by
.....displaying each individual surface
*/
	if (eptr->type == UM_COMPOS_SOLID)
	{
		highlighting = ncl_highlighting_geo();
		UM_set_constituent_pickids = UU_FALSE;
		for (i=0;i<eptr->no_netkey;i++)
		{
			e1.key = eptr->netkey[i];
			status = uc_retrieve_data(&e1,sizeof(e1));
			if (status != UU_SUCCESS) goto done;
			uc_retrieve_transf(e1.key,tf);
			uc_retrieve_attr(e1.key,&attr);
			e1.label[0] = '\0';
			if (savpik) gspickid(e1.key);
			if (highlighting)
				uc_draw(&e1,tf,attrptr);
			else
				uc_draw(&e1,tf,&attr);
		}
		goto done;
	}
/*
.....Calculate display lists if necessary
*/
	if (eptr->no_displst == 0)
	{
		status = ncl_solid_calc_lists(eptr,eptr->sdata,eptr->no_sdata,&dlist,
			&tess);
		if (status != UU_SUCCESS) goto done;
		alloc = UU_TRUE;
		pts = (UM_coord *)UU_LIST_ARRAY(&dlist);
		nc = UU_LIST_LENGTH(&dlist);
		lst = ncl_surflist_number(DISPLAY_LIST,eptr->rel_num);
		status = ur_update_data_varlist(eptr->key,lst,pts,1,nc);
		if (status != UU_SUCCESS) goto done;

		status = ncl_store_surflist1(TESSELLATION_LIST,eptr,&tess);
		if (status != UU_SUCCESS) goto done;
		status = ncl_retrieve_data(eptr,sizeof(struct UM_solid_rec));
		if (status != UU_SUCCESS) goto done;
	}
/*
.....Display as shaded
*/
	if (shaded)
	{
/*
........Save colors and line style
*/
		color = attrptr->color;
		ecolor = attrptr->ecolor;
		if (ecolor == 16) ecolor = color;
		style = attrptr->line_style;
/*
........Setup for edge color display
*/
		if (ecolor >= 0)
		{
			gsedgeflag(UG_ON);
			attrptr->color = ecolor;
		}
		else
			attrptr->line_style = 9;
/*
........Set display attributes
*/
		um_set_disp_attr(attrptr);
		ncl_setcrt_lucency(attrptr->lucency);
		ncl_setcrt_material(attrptr->material);

		pts = (UM_coord *)eptr->displst;
		ncl_bndr_display1(pts,tfmat);
/*
........Reset display attributes
*/
		attrptr->line_style = style;
		attrptr->color = color;
		um_set_disp_attr(attrptr);

		um_init_tess(&tess1);
		status = ncl_get_tessellation(eptr,&tess1);
		if (status == UU_SUCCESS)
		{
			status = ncl_shade_tessellation(&tess1,1);
			if (status != UU_SUCCESS)
				ncl_lst_delete(TESSELLATION_LIST,&eptr->key);
		}
		um_free_tess(&tess1);
	}
/*
.....Display as wireframe
*/
	else
	{
		um_set_disp_attr(attrptr);
		pts = (UM_coord *)eptr->displst;
		ncl_displst_display(pts,tfmat);
		if (ncl_is_hidline())
		{
			um_init_tess(&tess1);
			status = ncl_get_tessellation(eptr,&tess1);
			if (status == UU_SUCCESS)
			{
				status = ncl_shade_tessellation(&tess1,1);
				if (status != UU_SUCCESS)
					ncl_lst_delete(TESSELLATION_LIST,&eptr->key);
			}
			um_free_tess(&tess1);
		}
	}
/*
.....End of routine
*/
done:;
	if (alloc)
	{
		uu_list_free(&dlist);
		um_free_tess(&tess);
	}
	UM_set_constituent_pickids = savpik;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_calc_labloc(solid,labloc)
**       Calculates the default label location for a Visual Solid.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid entity to calculate label location for.
**       OUTPUT :
**          labloc   Default label location.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_solid_labloc(solid,labloc)
struct UM_solid_rec *solid;
UM_coord labloc;
{
	int inc;
	UU_REAL r1,box[6];
	UM_vector nvec,svec;
/*
.....The default label location is based
.....on the type of solid
*/
	switch (solid->type)
	{
/*
.....Box solid
*/
	case UM_BOX_SOLID:
		um_vcplvc(&solid->sdata[0],&solid->sdata[3],labloc);
		um_vcplvc(labloc,&solid->sdata[6],labloc);
		um_vcplvc(labloc,&solid->sdata[9],labloc);
		break;
/*
.....Cone, Cylinder solid
*/
	case UM_CONE_SOLID:
	case UM_CYLINDER_SOLID:
		um_unitvc(&solid->sdata[3],nvec);
		um_perpvc(nvec,svec);
		um_translate_point(&solid->sdata[0],1.,&solid->sdata[3],labloc);
		r1 = solid->sdata[6];
		if (solid->type == UM_CONE_SOLID) r1 = solid->sdata[7];
		um_translate_point(labloc,r1,svec,labloc);
		break;
/*
.....Torus solid
*/
	case UM_TORUS_SOLID:
		um_unitvc(&solid->sdata[3],nvec);
		um_perpvc(nvec,svec);
		um_translate_point(&solid->sdata[0],solid->sdata[6]+solid->sdata[7],
			svec,labloc);
		break;
/*
.....Sphere solid
*/
	case UM_SPHERE_SOLID:
		svec[0] = 0.; svec[1] = 0.; svec[2] = 1.;
		um_translate_point(&solid->sdata[0],solid->sdata[3],svec,labloc);
		break;
/*
.....Extruded solid
*/
	case UM_EXTRUDED_SOLID:
		um_translate_point(&solid->sdata[4],1.,&solid->sdata[0],labloc);
		break;
/*
.....Contour solid
*/
	case UM_CONTOUR_SOLID:
		um_translate_point(&solid->sdata[6],1.,&solid->sdata[2],labloc);
		break;
/*
.....Revolved solid
*/
	case UM_REVOLVED_SOLID:
		inc = 9 /*8 + (solid->sdata[8]-1)*3*/;
		um_vctovc(&solid->sdata[inc],labloc);
		break;
/*
.....STL solid
*/
	case UM_STL_SOLID:
		um_vctovc(&solid->displst[6],labloc);
		break;
/*
.....Composite solid
*/
	case UM_COMPOS_SOLID:
		ncl_geo_box(solid->key,box);
		um_vctovc(&box[3],labloc);
		break;
/*
.....Unrecognized solid
*/
	default:
		labloc[0] = labloc[1] = labloc[2] = 0.;
		break;
	}
}

/*********************************************************************
**    E_FUNCTION :  ncl_solid_to_stock(solid,stock)
**       Converts solid primitive data to NCLIPV stock primitive data.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to convert.
**       OUTPUT :  
**          stock    NCLIPV stock primitive data.
**    RETURNS      : UU_FAILURE if could not create stock.
**    SIDE EFFECTS :
**          Memory for 'stock->data' is allocated in this routine and must
**          be deallocated in the calling routine.
**    WARNINGS     : none
*********************************************************************/
int ncl_solid_to_stock(solid,stock)
struct UM_solid_rec *solid;
LW_stock_struc *stock;
{
	int i,status,inc,nsav;
	UU_REAL sdata[20],*ssav;
	UM_coord *pts,pt1,pt2;
	UM_vector vc1,vc2,vc3;
	struct UM_surfattr_rec attr;
	union
	{
		UU_REAL rval;
		int ival[2];
	} tprm;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	ssav = solid->sdata;
	nsav = solid->no_sdata;
	inc = 0;
/*
.....Get stock color
*/
	if (uc_retrieve_attr(solid->key,&attr) != UU_SUCCESS) goto failed;
	stock->color = attr.color;
/*
.....Set stock type
*/
	switch (solid->type)
	{
	case UM_BOX_SOLID:
		um_unitvc(&solid->sdata[3],vc1);
		um_unitvc(&solid->sdata[6],vc2);
		um_unitvc(&solid->sdata[9],vc3);
/*
........Convert box out of XY-plane to extrusion
*/
		if (fabs(vc1[0])+UM_FUZZ < 1. || fabs(vc2[1])+UM_FUZZ < 1. ||
			fabs(vc3[2])+UM_FUZZ < 1.)
		{
			stock->type = LW_STOCK_SWEEP;
			um_vctovc(&solid->sdata[9],&sdata[0]);
			sdata[3] = 5;
			pts = (UM_coord *)&sdata[4];
			um_vctovc(&solid->sdata[0],pts[0]);
			um_vcplvc(pts[0],&solid->sdata[3],pts[1]);
			um_vcplvc(pts[1],&solid->sdata[6],pts[2]);
			um_vcmnvc(pts[2],&solid->sdata[3],pts[3]);
			um_vctovc(pts[0],pts[4]);
			solid->sdata = sdata;
			solid->no_sdata = 3 + 1 + 5*3;
		}
/*
........Convert box to stock style box
*/
		else
		{
			stock->type = LW_STOCK_BOX;
			pts = (UM_coord *)&sdata[0];
			um_vctovc(&solid->sdata[0],pt1);
			um_vcplvc(pt1,&solid->sdata[3],pt2);
			um_vcplvc(pt2,&solid->sdata[6],pt2);
			um_vcplvc(pt2,&solid->sdata[9],pt2);
			ncl_init_box(pt1,sdata);
			ncl_update_box(pt2,sdata);
			solid->sdata = sdata;
			solid->no_sdata = 6;
		}
		break;
	case UM_CONE_SOLID:
		stock->type = LW_STOCK_CONE;
		break;
	case UM_CYLINDER_SOLID:
		stock->type = LW_STOCK_CYLINDER;
		break;
	case UM_SPHERE_SOLID:
		stock->type = LW_STOCK_SPHERE;
		break;
	case UM_TORUS_SOLID:
		stock->type = LW_STOCK_TORUS;
		break;
	case UM_EXTRUDED_SOLID:
		stock->type = LW_STOCK_SWEEP;
		break;
	case UM_CONTOUR_SOLID:
		stock->type = LW_STOCK_SWEEP;
		inc = 2;
		break;
	case UM_REVOLVED_SOLID:
		stock->type = LW_STOCK_REVOLVE;
		break;
	case UM_STL_SOLID:
		stock->type = LW_STOCK_FILE;
		tprm.rval = solid->sdata[0];
		stock->units = tprm.ival[0];
		stock->bin = tprm.ival[1];
		tprm.rval = solid->sdata[1];
		stock->mxflag = tprm.ival[0];
		if (stock->mxflag)
		{
			stock->mxchg = UU_TRUE;
			um_tftotf(&solid->sdata[2],stock->matrix);
			stock->invflag = UU_TRUE;
			um_inverttf(stock->matrix,stock->invmx);
		}
		inc = 14;
		break;
	case UM_COMPOS_SOLID:
		stock->type = LW_STOCK_COMPOS;
		stock->bin = solid->no_netkey;
		solid->no_sdata = 0;
		break;
	}
/*
.....Store stock parameters
*/
	if (solid->no_sdata != 0)
	{
		stock->data = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*solid->no_sdata);
		if (stock->data == UU_NULL) goto failed;
		for (i=inc;i<solid->no_sdata;i++) stock->data[i-inc] = solid->sdata[i];
	}
	else
		stock->data = UU_NULL;
	goto done;
/*
.....Could not create stock
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	solid->sdata = ssav;
	solid->no_sdata = nsav;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_solid_store_components(solid,nsrf)
**       Copies surfaces defined for the COMPOS Solid and updates the
**       solid structure with the keys of the copied surfaces.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid being defined.
**          params   [1] = Existing surface flag, [2] = # of surfaces.
**       OUTPUT :  
**          solid    List of surface keys are stored.
**    RETURNS      : UU_FAILURE if problems arise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_solid_store_components(solid,params)
struct UM_solid_rec *solid;
UU_REAL params[];
{
	int i,nsrf,ifl,status;
	UU_LOGICAL blanked;
	UM_int2 idx,lfl_77;
	Gseg dsegid;
	UU_KEY_ID *keys;
	struct UM_srfdatabag e1,e2;
	struct UM_surfattr_rec attr;
/*
.....Initialize routine
*/
	nsrf = params[2];
	ifl = params[1];
	keys = UU_NULL;
	if (uc_retrieve_attr(solid->key,&attr) != UU_SUCCESS) goto failed;
	status = UU_SUCCESS;
/*
.....Allocate memory for surface keys
*/
	keys = (UU_KEY_ID *)uu_malloc(sizeof(UU_KEY_ID)*nsrf);
	if (keys == UU_NULL) goto failed;
/*
.....Copy surfaces 
*/
	lfl_77 = 1;
	stunlb(&lfl_77);
	for (i=0;i<nsrf;i++)
	{
		idx = i + 1;
		gtsky(&idx,&e1.key);
		ncl_retrieve_data_fixed(&e1,sizeof(e1));
		status = uc_copy(&e1,&e2,sizeof(struct NCL_fixed_databag));
		if (status != UU_SUCCESS) goto done;
		strcpy(e2.label,"@UN");
		status = ur_update_data_fixed(&e2);
		keys[i] = e2.key;
/*
........Copy surface attributes
*/
		if (e2.rel_num != UM_SOLID_REL)
		{
			if (uc_retrieve_attr(solid->key,&attr) != UU_SUCCESS) goto failed;
			attr.key = e2.key;
			if (ur_update_attr(&attr) != 0) goto failed;
		}
		ur_update_displayable(e2.key,UM_NEVERDISPLAYABLE);
/*
........Invisible input surface
*/
		if (ifl == 0)
		{
			ur_retrieve_blanked(e1.key,&blanked);
			if (!blanked)
			{
				ur_update_blanked(e1.key,UU_TRUE);
				ur_retrieve_disp_segid(e1.key,&dsegid);
				uv_blanksegs(dsegid, e1.key);
			}
		}
/*
........Remove input surface
*/
		else if (ifl == 2)
			uc_delete(e1.key);
	}
/*
.....Store surface keys in solid
*/
	stunlb(&lfl_77);
	status = ur_update_data_varlist(solid->key,4,keys,1,nsrf);
/*
........Invisible Net/Composite entities
*/
	if (ifl == 0 || ifl == 2)
	{
		gtnskw(&nsrf);
		for (i=0;i<nsrf;i++)
		{
			idx = i + 1;
			gtskw(&idx,&e1.key);
			if (ifl == 0)
			{
				ur_retrieve_blanked(e1.key,&blanked);
				if (!blanked)
				{
					ur_update_blanked(e1.key,UU_TRUE);
					ur_retrieve_disp_segid(e1.key,&dsegid);
					uv_blanksegs(dsegid, e1.key);
				}
			}
/*
........Remove input surface
*/
			else if (ifl == 2)
				uc_delete(e1.key);
		}
	}
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (keys != UU_NULL) uu_free(keys);
	delsky();
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_calc_box(params,nparms,oparms,nprmo)
**       Calculates the bounding box for the motion display or a list
**       of geometry.
**    PARAMETERS   
**       INPUT  : 
**          params   Solid canonical data.
**          nparms   Number of values in 'params'.
**       OUTPUT :  
**          oparms   Solid canonical data containing bounding box.
**          nprmo    Number of values in 'oparms'.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_calc_box(params,nparms,oparms,nprmo)
UU_REAL *params;
int nparms;
UU_REAL *oparms;
int *nprmo;
{
	int status,nt,modals[20],icurpt,nsf,i;
	UM_int2 idx;
	UU_REAL exp[3],box[6],tbox[6];
	UU_KEY_ID nclkey;
	UN_clstruc *iclpt[4];
/*
.....Store expansion values
*/
	um_vctovc(params,exp);
/*
.....Calculate motion bounding box
*/
	if (params[3] == -1)
	{
		ncl_play_initscan(modals,iclpt,&icurpt,UU_FALSE);
		nt = 0;
		ncl_motion_playback(modals,2,tbox,&nt);
		ncl_play_resetscan(iclpt,icurpt);
		status = UU_SUCCESS;
	}
/*
.....Calculate bounding box using geometry list
*/
	else
	{
		nsf = params[3];
		tbox[0] = tbox[1] = tbox[2] = 100000.;
		tbox[3] = tbox[4] = tbox[5] = -100000.;
		for (i=0;i<nsf;i++)
		{
/*
........Get the geometry
*/
			idx = i + 1;
			gtsky(&idx,&nclkey);
/*
........Calculate box
*/
			status = ncl_geo_box(nclkey,box);
/*
........Merge boxes
*/
			if (status == UU_SUCCESS)
			{
				ncl_update_box(&box[0],tbox);
				ncl_update_box(&box[3],tbox);
			}
		}
		status = UU_SUCCESS;
		if (tbox[3] < tbox[0]) status = UU_FAILURE;
	}
/*
.....Apply expansion values
*/
	if (status == UU_SUCCESS)
	{
		*nprmo = 12;
		tbox[0] = tbox[0] - exp[0]/2.;
		tbox[1] = tbox[1] - exp[1]/2.;
		tbox[2] = tbox[2] - exp[2]/2.;
		tbox[3] = tbox[3] + exp[0]/2.;
		tbox[4] = tbox[4] + exp[1]/2.;
		tbox[5] = tbox[5] + exp[2]/2.;
/*
.....Convert box to solid box format
*/
		boxcnv(&tbox[0],&tbox[3],oparms);
	}
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_calc_curve(styp,params,nparms,oparms,nprmo)
**       Calculates the curve points for extruded, revolved, and
**       contour solids.
**    PARAMETERS   
**       INPUT  : 
**          styp     Type of solid.
**          params   Solid canonical data.
**          nparms   Number of values in 'params'.
**       OUTPUT :  
**          oparms   Solid canonical data containing curve points and
**                   optional tangent vectors.
**          nprmo    Number of values in 'oparms'.
**    RETURNS      : UU_FAILURE on failure, otherwise UU_SUCCESS.
**    SIDE EFFECTS :
**          Memory for 'oparms' is allocated in this routine and must
**          be deallocated in the calling routine.
**    WARNINGS     : none
*********************************************************************/
static int S_calc_curve(styp,params,nparms,oparms,nprmo)
int styp;
UU_REAL *params;
int nparms;
UU_REAL **oparms;
int *nprmo;
{
	int i,n,status,nsf,np,ilev,ibox;
	UM_int2 idx;
	UU_LOGICAL dupfl;
	UU_REAL told,len,bofs,tofs,exp,*optr;
	UM_coord *pts,*pto,bxpt[5];
	UM_vector nvec,svec,*vco;
	UM_plane plane;
	UM_transf tfmat;
	UM_sgeo sgeo;
	UU_LIST plist,vlist,*tvc,*clist,sflist;
	struct UC_entitydatabag e;
/*
.....Initialize routine
*/
	idx = 175; getsc(&idx,&told);
	uu_list_init0(&plist);
	uu_list_init0(&vlist);
	clist = UU_NULL;
	uu_list_init0(&sflist);
/*
.....Extruded or Revolved Solid
*/
	if (styp == UM_EXTRUDED_SOLID || styp == UM_REVOLVED_SOLID)
	{
/*
........Prepare curve for evaluation
*/
		e.key = params[0];
		status = ncl_retrieve_data(&e,sizeof(struct UC_entitydatabag));
		if (status == UU_SUCCESS) status = uc_retrieve_transf(e.key,tfmat);
		if (status != UU_SUCCESS) goto done;
/*
........Initialize lists
*/
		uu_list_init(&plist,sizeof(UM_coord),50,20);
		if (UU_LIST_NULLPTR(&plist)) goto failed;
		if (styp == UM_REVOLVED_SOLID)
		{
			uu_list_init(&vlist,sizeof(UM_coord),50,20);
			if (UU_LIST_NULLPTR(&vlist)) goto failed;
			tvc = &vlist;
			dupfl = UU_TRUE;
		}
		else
		{
			tvc = UU_NULL;
			dupfl = UU_FALSE;
		}
/*
........Evolve curve
*/
		n = ncl_evolve_all_curves(&e,tfmat,told,&plist,tvc,dupfl);
		if (n == 0) goto failed;
/*
........Allocate memory for output array
*/
		np = 4 + n*3;
		if (styp == UM_REVOLVED_SOLID) np += n*3 + 5;
		optr = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*np);
		if (optr == UU_NULL) goto failed;
/*
........Setup parameters for solid definition
*/
		um_vctovc(&params[1],&optr[0]);
		np = 3;
		if (styp == UM_REVOLVED_SOLID)
		{
			um_vctovc(&params[4],&optr[3]);
			optr[6] = params[7];
			optr[7] = params[8];
			np = 8;
		}
		pto = (UM_coord *)UU_LIST_ARRAY(&plist);
		vco = (UM_vector *)UU_LIST_ARRAY(&vlist);
	}
/*
.....Contour solid
*/
	else if (styp == UM_CONTOUR_SOLID)
	{
/*
........Get the surface keys
*/
		nsf = params[0];
		uu_list_init(&sflist,sizeof(UU_KEY_ID),nsf,10);
		if (UU_LIST_NULLPTR(&sflist)) goto failed;
		for (i=0;i<nsf;i++)
		{
			idx = i + 1;
			gtsky(&idx,&sgeo.key);
			nclu_push_sfkey(&sflist,&sgeo);
		}
/*
........Prepare for creation of contour curve
*/
		um_unitvc(&params[3],nvec);
		ncl_mcstowcs(1,nvec,nvec);
		um_unitvc(&params[7],svec);
		exp = params[1];
		ilev = params[2];
		ibox = params[12];
		if (ilev == 0)
		{
			bofs = 0.;
			tofs = params[11];
			len  = params[11];
		}
		else
		{
			bofs = params[10];
			tofs = params[11];
			len  = 0.;
		}
/*
........Create contour curve
*/
		status = ncl_ipv_contour_stock(&sflist,nvec,exp,bofs,tofs,&clist,
			UU_FALSE);
		if (status != UU_SUCCESS || clist == UU_NULL) goto failed;
		n = UU_LIST_LENGTH(clist);
		if (n == 0) goto failed;
		if (len == 0.) len = um_mag(nvec);
		um_unitvc(nvec,nvec);
/*
........Place contour curve on requested plane
*/
		pts = (UM_coord *)UU_LIST_ARRAY(clist);
		if (ilev == 0)
		{
			um_vctovc(nvec,plane.n);
			um_vctmsc(nvec,params[6],plane.p0);
			um_proj_pt_on_plane(n,pts,&plane,pts);
		}
/*
........Calculate bounding box of contour curve
*/
		pto = pts;
		if (ibox == 1)
		{
			if (ilev == 0)
				um_vctmsc(nvec,params[6],plane.p0);
			else
				um_vctovc(pts[0],plane.p0);
			um_box_on_plane(pts,n,plane.p0,nvec,bxpt,UU_FALSE);
			um_vctovc(bxpt[0],bxpt[4]);
			pto = bxpt;
			n = 5;
		}
/*
........Allocate memory for output array
*/
		np = 6 + n*3;
		optr = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*np);
		if (optr == UU_NULL) goto failed;
/*
........Setup parameters for solid definition
*/
		optr[0] = exp;
		optr[1] = ibox;
		um_vctmsc(svec,len,&optr[2]);
		np = 5;
/*		pto = (UM_coord *)UU_LIST_ARRAY(clist);*/
	}
/*
.....Store points in output parameters
*/
	optr[np] = n;
	np++;
	for (i=0;i<n;i++)
	{
		um_vctovc(&pto[i],&optr[np]);
		np += 3;
	}
	if (styp == UM_REVOLVED_SOLID)
	{
		for (i=0;i<n;i++)
		{
			um_vctovc(&vco[i],&optr[np]);
			np += 3;
		}
	}
	*oparms = optr;
	*nprmo = np;
	goto done;
/*
.....Could not generate curve
*/
failed:;
	status = UU_FAILURE;
/*
.....End of routine
.....Free lists
*/
done:;
	if (!UU_LIST_NULLPTR(&plist)) uu_list_free(&plist);
	if (!UU_LIST_NULLPTR(&vlist)) uu_list_free(&vlist);
	if (!UU_LIST_NULLPTR(&sflist)) uu_list_free(&sflist);
	if (clist != UU_NULL) uu_list_free(clist);
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_copy_curve_pts(pti,nci,pto,nco)
**       Copies a set of curve points from one array to another,
**       adding duplicate points at sharp corners.
**    PARAMETERS   
**       INPUT  : 
**          pti      Input point array.
**          nci      Number of points in 'pti'.
**       OUTPUT :  
**          pti      Output point array.
**          nco      Number of points in 'pto'.
**    RETURNS      : none
**    SIDE EFFECTS :
**          The 'pto' array should be large enough to hold 2x the size
**          of the input 'pti' array.
**    WARNINGS     : none
*********************************************************************/
static void S_copy_curve_pts(pti,nci,pto,nco)
UM_coord *pti;
int nci;
UM_coord *pto;
int *nco;
{
	int i;
	UM_vector vc0,vc1;
	UU_REAL cco;
/*
.....Store first point
*/
	um_vctovc(pti[0],pto[0]);
	um_vcmnvc(pti[1],pti[0],vc0);
	um_unitvc(vc0,vc0);
	*nco = 1;
/*
.....Loop through points
*/
	for (i=1;i<nci-1;i++)
	{
		um_vcmnvc(pti[i+1],pti[i],vc1);
		if (um_mag(vc1) > UM_FUZZ)
		{
			um_unitvc(vc1,vc1);
			cco = um_dot(vc0,vc1);
/*
........Points of less than 25 degrees
........signify a sharp corner
*/
			if (cco < CO25)
			{
				um_vctovc(pti[i],pto[*nco]);
				*nco += 1;
			}
			um_vctovc(vc1,vc0);
			um_vctovc(pti[i],pto[*nco]);
			*nco += 1;
		}
	}
/*
.....Store last point
*/
	um_vctovc(pti[nci-1],pto[*nco]);
	*nco += 1;
}
