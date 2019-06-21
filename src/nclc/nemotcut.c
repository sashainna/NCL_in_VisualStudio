/*********************************************************************
**    NAME         :  nemotcut.c
**       CONTAINS:
**          nclf_cutter_set
**          ncl_cutter_set
**          ncl_cutter_set_attr
**				ncl_cutter_get_defattr
**				ncl_cutter_store_geo
**				ncl_cutter_get
**				ncl_cutter_get_struc
**				ncl_cutter_get_active
**				ncl_cutter_get_ptr
**				ncl_cutter_get_attr
**				ncl_cutter_get_parms
**				ncl_cutter_get_symbol
**				ncl_cutter_get_symgeo
**				nclf_cutter_get_cutsym
**				nclf_cutter_get_bounds
**				ncl_cutter_is_blade
**				ncl_cutter_reset
**				nclf_get_tool_symlib
**				ncl_get_tool_symlib
**				ncl_cutter_struc_size
**				ncl_cutter_attr_size
**				ncl_cutter_parms_size
**				ncl_cutter_symbol_size
**				ncl_cutter_symgeo_size
**				ncl_cutter_get_ptrs
**				ncl_cutter_set_ptrs
**				ncl_cutter_store_struc
**				ncl_cutter_store_attr
**				ncl_cutter_store_parms
**				ncl_cutter_store_symbol
**				ncl_cutter_store_symgeo
**          ncl_format_tool
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nemotcut.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       12/01/15 , 08:50:01
*********************************************************************/
#include "usysdef.h"
#include <math.h>
#include "nclfc.h"
#include "nclx.h"
#include "view.h"
#include "lipv.h"
#include "mdcpln.h"
#include "mfort.h"
#include "modef.h"
#include "nclmplay.h"
#include "ulist.h"
#include "umath.h"
#include "xenv1.h"
#include "mcrv.h"

void ncl_cutter_set();
void ncl_cutter_get_active();
void ncl_cutter_get_attr();
void ncl_cutter_get_parms();
void ncl_cutter_get_symbol();
void ncl_get_tool_symlib();

static int S_list_init();
static int S_init_geolist();
static UU_LOGICAL S_match_cutter();
static UU_LOGICAL S_match_parms();
static UU_LOGICAL S_match_attr();
static UU_LOGICAL S_match_symbol();
static UU_LOGICAL S_find_symgeo();

static int Sactive_cutter = -1;
static int Ssymgeo_ptr = 0;
static UU_LOGICAL Sis_blade = UU_FALSE;
static UU_LOGICAL Sinit_geo = UU_FALSE;
static UN_motseg_cutattr
	Scattr_def = {1,0,0,0,0,NCLX_GREEN,NCLX_ORANGE,NCLX_PURPLE,1,1,1,
		100,100,100};
static UU_LIST Scutter_list;
static UU_LIST Scutparm_list;
static UU_LIST Scutattr_list;
static UU_LIST Ssymbol_list;
static UU_LIST Ssymgeo_list;

/*********************************************************************
**    E_FUNCTION     : nclf_cutter_set(cutr,cfl,flags,cutsyb,shksyb,hldsyb,
**                                     fkey)
**			FORTRAN callable routine to store the cutter definition in the
**			motion display list.
**    PARAMETERS   
**       INPUT  : (see ncl_cutter_set)
**       OUTPUT : none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_cutter_set(cutr,cfl,flags,cutsyb,shksyb,hldsyb,fkey)
UM_int4 flags[],cfl[],fkey[];
UM_real8 cutr[];
UM_f77_str_ptr cutsyb,shksyb,hldsyb;
{
	int nc;
	char *p,cutsym[MAXSYMLEN],shksym[MAXSYMLEN],hldsym[MAXSYMLEN];
	UU_KEY_ID symkey[3];
/*
.....Setup Fortran character strings
*/
	p = UM_cstr_of_f77_str(cutsyb);
	nc = MAXSYMLEN;
	strncpy(cutsym,p,nc);
	ul_strip_blanks(cutsym,&nc);
	p = UM_cstr_of_f77_str(shksyb);
	nc = MAXSYMLEN;
	strncpy(shksym,p,nc);
	ul_strip_blanks(shksym,&nc);
	p = UM_cstr_of_f77_str(hldsyb);
	nc = MAXSYMLEN;
	strncpy(hldsym,p,nc);
	ul_strip_blanks(hldsym,&nc);
	symkey[0] = fkey[0]; symkey[1] = fkey[1]; symkey[2] = fkey[2];
/*
.....Call cutter set routine
*/
	ncl_cutter_set(cutr,cfl,flags,cutsym,shksym,hldsym,symkey);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_set(cutr,cfl,flags,cutsyb,shksyb,hldsyb,
**                                    symkey)
**			Store the cutter definition in the motion display list.
**    PARAMETERS   
**       INPUT  : 
**          cutr     = Cutter parameters.
**          cfl(1)   = 0   = Don't display cutter.
**                     1   = Display NCL calculated cutter.
**                     2   = Display CADD symbol as cutter.
**                     3   = Display Curve cutter.
**                     4   = Display Tool file cutter.
**          cfl(2)   = Same as 'cfl' except for shank.
**          cfl(3)   = Same as 'cfl' except for holder.
**          flags(0) = 1 = Display cutter in greater detail.
**          flags(1) = -1 = Mark as moving cutter displayed, but don't
**                          display it.
**                      1 = Display moving cutter.
**                      2 = Display moving part.
**          flags(2) = 1 = Display shaded cutter.
**          flags(5) = 1 = Display shaded shank.
**          flags(6) = 1 = Display shaded holder.
**          cutsyb   = CADD symbol to use for cutter display.
**          shksyb   = CADD symbol to use for shank display.
**          hldsyb   = CADD symbol to use for holder display.
**          symkey   = Unibase keys of symbol geometry, [0] = Cutter,
**                     [1] = Shank, [2] = Holder.
**
**       OUTPUT : 
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_set(cutr,cfl,flags,cutsyb,shksyb,hldsyb,symkey)
int flags[],cfl[];
UU_REAL cutr[];
char *cutsyb,*shksyb,*hldsyb;
UU_KEY_ID symkey[];
{
	UU_REAL ary[4];
	UN_motseg_cutter_struc mycut,cutseg;
	UN_motseg_cutattr cattr;
/*
.....Get active cutter structure
*/
	ncl_cutter_get_active(&mycut);
	cutseg = mycut;
/*
.....Find matching cutter parameters
*/
	S_match_parms(&mycut,cutr);
	cattr = Scattr_def;
	cattr.mov = flags[1]; cattr.segfl  = flags[0];
	cattr.shaded[0] = flags[2]; cattr.shaded[1] = flags[5];
	cattr.shaded[2] = flags[6];
	if (cattr.shaded[0] || cattr.shaded[1] || cattr.shaded[2])
		cattr.segfl = UU_TRUE;
	S_match_attr(&mycut,&cattr);
	ary[0] = ary[1] = 0.;
	ary[2] = cutr[18];
	ary[3] = cutr[19];
	S_match_symbol(&mycut,0,cutsyb,symkey[0],cfl[0],ary,flags[2]);
/*
.....Find matching shank paramters
*/
	S_match_symbol(&mycut,1,shksyb,symkey[1],cfl[1],&cutr[13],flags[5]);
/*
.....Find matching holder paramters
*/
	S_match_symbol(&mycut,2,hldsyb,symkey[2],cfl[2],&cutr[9],flags[6]);
/*
.....Find matching cutter structure
*/
	S_match_cutter(&cutseg,&mycut);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_set_attr(cattr)
**			Sets the default Cutter attribute bundle.  A value of -1 in
**       any of the attribute values will cause that value not to be
**       changed.
**    PARAMETERS   
**       INPUT  : 
**          cattr    = Cutter attributes to set.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_set_attr(cattr)
UN_motseg_cutattr *cattr;
{
/*
.....Set attributes
*/
	if (cattr->segfl != -1) Scattr_def.segfl = cattr->segfl;
	if (cattr->mov != -1) Scattr_def.mov = cattr->mov;
	if (cattr->shaded[0] != -1) Scattr_def.shaded[0] = cattr->shaded[0];
	if (cattr->shaded[1] != -1) Scattr_def.shaded[1] = cattr->shaded[1];
	if (cattr->shaded[2] != -1) Scattr_def.shaded[2] = cattr->shaded[2];
	if (cattr->color[0] != -1) Scattr_def.color[0] = cattr->color[0];
	if (cattr->color[1] != -1) Scattr_def.color[1] = cattr->color[1];
	if (cattr->color[2] != -1) Scattr_def.color[2] = cattr->color[2];
	if (cattr->pen[0] != -1) Scattr_def.pen[0] = cattr->pen[0];
	if (cattr->pen[1] != -1) Scattr_def.pen[1] = cattr->pen[1];
	if (cattr->pen[2] != -1) Scattr_def.pen[2] = cattr->pen[2];
	if (cattr->trans[0] != -1) Scattr_def.trans[0] = cattr->trans[0];
	if (cattr->trans[1] != -1) Scattr_def.trans[1] = cattr->trans[1];
	if (cattr->trans[2] != -1) Scattr_def.trans[2] = cattr->trans[2];
	if (Scattr_def.shaded[0] || Scattr_def.shaded[1] || Scattr_def.shaded[2])
		Scattr_def.segfl = UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_get_defattr(cattr)
**			Returns the default Cutter attribute bundle.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          cattr    = Default cutter attributes.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_get_defattr(cattr)
UN_motseg_cutattr *cattr;
{
/*
.....Set attributes
*/
	cattr->segfl = Scattr_def.segfl;
	cattr->mov = Scattr_def.mov;
	cattr->shaded[0] = Scattr_def.shaded[0];
	cattr->shaded[1] = Scattr_def.shaded[1];
	cattr->shaded[2] = Scattr_def.shaded[2];
	cattr->color[0] = Scattr_def.color[0];
	cattr->color[1] = Scattr_def.color[1];
	cattr->color[2] = Scattr_def.color[2];
	cattr->pen[0] = Scattr_def.pen[0];
	cattr->pen[1] = Scattr_def.pen[1];
	cattr->pen[2] = Scattr_def.pen[2];
	cattr->trans[0] = Scattr_def.trans[0];
	cattr->trans[1] = Scattr_def.trans[1];
	cattr->trans[2] = Scattr_def.trans[2];
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_store_geo(sym,pts,vcs,ctype,npt)
**			Stores cutter geometry obtained from an external source, such
**			as the clfile.  This geometry will always be stored with a type
**			equal to 3 (Geometry) and a key equal to 0.  It replaces any
**			geometry in the list that is defined with the same label and a
**			key of 0.
**    PARAMETERS   
**       INPUT  : 
**          sym      = Geometry symbol.
**          pts      = Geometry points.
**          vcs      = Geometry normal vectors.
**          ctype    = 1 = Mill geometry, 2 = Lathe/Blade geometry.
**          npt      = Number of points to store.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_store_geo(sym,pts,vcs,ctype,npt)
char *sym;
UM_coord *pts;
UM_vector *vcs;
int npt;
{
	int sympt,type;
	UU_KEY_ID key;
	UN_motseg_symgeo *symgeo;
/*
.....Find a matching symbol
*/
	key = 0;
	type = -3;
	S_find_symgeo(sym,0,&type,ctype,0,&sympt,&symgeo);
/*
.....Store points and vectors
*/
	UU_LIST_EMPTY(&symgeo->curve);
	UU_LIST_EMPTY(&symgeo->cnorm);
	uu_list_push_multiple(&symgeo->curve,npt,pts);
	uu_list_push_multiple(&symgeo->cnorm,npt,vcs);
}

/*********************************************************************
**    I_FUNCTION     : ncl_cutter_get(cutseg,which)
**			Returns the requested full cutter structure.
**    PARAMETERS   
**       INPUT  : 
**          which    = Which cutter structure to return.
**                     UN_MOTSEG_ACTIVE returns the active structure.
**
**       OUTPUT : 
**          cutseg   = Requested cutter structure.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_get(cutseg,which)
UN_motseg_cutter *cutseg;
int which;
{
	int inc;
	UN_motseg_cutter_struc *mycut;
/*
.....Initialize routine
*/
	if (which == UN_MOTSEG_ACTIVE) inc = Sactive_cutter;
	else inc = which;
/*
.....No cutter has been defined
*/
	if (inc == -1)
	{
		cutseg->cutter = UU_NULL;
		cutseg->cattr = UU_NULL;
		cutseg->cutsym = UU_NULL;
		cutseg->shank = UU_NULL;
		cutseg->holder = UU_NULL;
		Sis_blade = UU_FALSE;
	}
/*
.....Return full cutter structure
*/
	else
	{
		mycut = (UN_motseg_cutter_struc *)UU_LIST_ARRAY(&Scutter_list);
		ncl_cutter_get_parms(&(cutseg->cutter),mycut[inc].cutter);
		ncl_cutter_get_attr(&(cutseg->cattr),mycut[inc].cattr);
		ncl_cutter_get_symbol(&(cutseg->cutsym),mycut[inc].cutsym);
		ncl_cutter_get_symbol(&(cutseg->shank),mycut[inc].shank);
		ncl_cutter_get_symbol(&(cutseg->holder),mycut[inc].holder);
		if (cutseg->cutter->parms[0] < 0.) Sis_blade = UU_TRUE;
		else Sis_blade = UU_FALSE;
	}
}

/*********************************************************************
**    I_FUNCTION     : ncl_cutter_get_active(cutseg)
**			Returns the currently active cutter structure.  If the cutter
**			lists have not been initialized yet, they will be from this
**			routine and the returned cutter structure will have -1 values
**			for each of it pointers.
**    PARAMETERS   
**       INPUT  : 
**          cutseg   = Active cutter structure.
**
**       OUTPUT : 
**          cutseg   = Active cutter structure.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_get_active(cutseg)
UN_motseg_cutter_struc *cutseg;
{
	UN_motseg_cutter_struc *mycut;
/*
.....Cutter list not used yet
.....initialize it
*/
	if (Sactive_cutter == -1) S_list_init(cutseg);
/*
.....Return requested address
*/
	else
	{
		mycut = (UN_motseg_cutter_struc *)UU_LIST_ARRAY(&Scutter_list);
		uu_move_byte(&mycut[Sactive_cutter],cutseg,
			sizeof(UN_motseg_cutter_struc));
	}
}

/*********************************************************************
**    I_FUNCTION     : ncl_cutter_get_struc(cutseg,which)
**			Returns the actual cutter structure.  The parameters of the
**			cutter structure returned will point to the arrays of the
**			cutter sub-structures rather than be address pointers to these
**			structures.
**    PARAMETERS   
**       INPUT  : 
**          which    = Which cutter structure from the list to return.
**
**       OUTPUT : 
**          cutseg   = Active cutter structure.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_get_struc(cutseg,which)
UN_motseg_cutter_struc **cutseg;
int which;
{
	UN_motseg_cutter_struc *mycut;
/*
.....Cutter list not used yet
.....initialize it
*/
	if (which == -1) S_list_init(cutseg);
/*
.....Return requested address
*/
	else
	{
		mycut = (UN_motseg_cutter_struc *)UU_LIST_ARRAY(&Scutter_list);
		*cutseg = &mycut[which];
	}
}

/*********************************************************************
**    I_FUNCTION     : ncl_cutter_get_ptr()
**			Returns the pointer to the currently active cutter structure.
**    PARAMETERS   
**       INPUT  : none
**
**       OUTPUT : none
**
**    RETURNS      : Pointer to active cutter structure.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cutter_get_ptr()
{
	return(Sactive_cutter);
}

/*********************************************************************
**    I_FUNCTION     : ncl_cutter_get_attr(parms,which)
**			Returns the address of the cutter attributes structure referenced
**       by 'which'.  Returns UU_NULL if 'which' does not point to a
**       structure.
**    PARAMETERS   
**       INPUT  : 
**          which    = Index of Cutter attributes to return.
**
**       OUTPUT : 
**          parms    = Address of cutter attribute structure or UU_NULL.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_get_attr(parms,which)
UN_motseg_cutattr **parms;
int which;
{
	UN_motseg_cutattr *mycut;
/*
.....Return requested address
*/
	if (which == -1) *parms = UU_NULL;
	else
	{
		mycut = (UN_motseg_cutattr *)UU_LIST_ARRAY(&Scutattr_list);
		*parms = &mycut[which];
	}
}

/*********************************************************************
**    I_FUNCTION     : ncl_cutter_get_parms(parms,which)
**			Returns the address of the cutter parameter structure referenced
**       by 'which'.  Returns UU_NULL if 'which' does not point to a
**       structure.
**    PARAMETERS   
**       INPUT  : 
**          which    = Index of Cutter parameters to return.
**
**       OUTPUT : 
**          parms    = Address of cutter parameter structure or UU_NULL.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_get_parms(parms,which)
int which;
UN_motseg_cutparm **parms;
{
	UN_motseg_cutparm *mycut;
/*
.....Return requested address
*/
	if (which == -1) *parms = UU_NULL;
	else
	{
		mycut = (UN_motseg_cutparm *)UU_LIST_ARRAY(&Scutparm_list);
		*parms = &mycut[which];
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_get_symbol(parms,which)
**			Returns the address of a cutter symbol structure referenced
**       by 'which'.  Returns UU_NULL if 'which' does not point to a
**       structure.
**    PARAMETERS   
**       INPUT  : 
**          which    = Index of Cutter symbol to return.
**
**       OUTPUT : 
**          parms    = Address of cutter symbol structure or UU_NULL.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_get_symbol(parms,which)
UN_motseg_symbol **parms;
int which;
{
	UN_motseg_symbol *mycut;
	UN_motseg_symgeo *mygeo;
/*
.....Return requested address
*/
	if (which == -1) *parms = UU_NULL;
	else
	{
		mycut = (UN_motseg_symbol *)UU_LIST_ARRAY(&Ssymbol_list);
		*parms = &mycut[which];
		mygeo = (UN_motseg_symgeo *)UU_LIST_ARRAY(&Ssymgeo_list);
		mycut[which].geo = &mygeo[mycut[which].geoptr];
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_get_symgeo(parms,which)
**			Returns the address of a cutter symbol geometry structure
**       Returns UU_NULL if list does not exist.
**    PARAMETERS   
**       INPUT  : 
**          which    = Index of Cutter parameters to return.
**
**       OUTPUT : 
**          parms    = Address of cutter symbol geometry structure or UU_NULL.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_get_symgeo(parms,which)
int which;
UN_motseg_symgeo **parms;
{
	UN_motseg_symgeo *mygeo;
/*
.....Return requested address
*/
	if (which == -1) *parms = UU_NULL;
	else
	{
		mygeo = (UN_motseg_symgeo *)UU_LIST_ARRAY(&Ssymgeo_list);
		*parms = &mygeo[which];
	}
}

/*********************************************************************
**    E_FUNCTION     : nclf_cutter_get_cutsym(fsym,fkey,fshade,ftype,ctype,
**		                                        pts,vecs,npts)
**			Loads the requested symbol/geo into the UN_motseg_symbol list
**			if not already loaded and returns the point array that makes
**			up the cutter shape if it is represented by geo.
**    PARAMETERS   
**       INPUT  : 
**          fsym     = Symbol to load.
**          fkey     = Unibase key of symbol.
**          fshade   = 1 = Symbol is shaded.
**          ftype    = Type of cutter geometry to search for, 2 = Symbol,
**                     3 = Geometry, 4 = Point-List file.
**          ctype    = Type of cutter being defined, 1 = Mill, 2 = Lathe/Blade.
**
**       OUTPUT : 
**          ftype    = Updated geometry type.
**          pts      = Point array for cutter shape.
**          vecs     = Normal vector array.
**          npts     = Number of points returned.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_cutter_get_cutsym(fsym,fkey,fshade,ftype,ctype,pts,vecs,npts)
UM_f77_str_ptr fsym;
UM_int4 *fkey,*fshade,*ftype,*ctype;
UM_real8 pts[],vecs[];
UM_int4 *npts;
{
	int nc,geopt,inc,i;
	UU_LOGICAL shaded,found;
	UU_KEY_ID key;
	char *p,symbol[MAXSYMLEN];
	UM_coord *ptx;
	UM_vector *vcx;
	UN_motseg_symgeo *symgeo;
/*
.....Setup Fortran character string
*/
	p = UM_cstr_of_f77_str(fsym);
	nc = MAXSYMLEN;
	strncpy(symbol,p,nc);
	ul_strip_blanks(symbol,&nc);
/*
.....Initialize routine
*/
	*npts = 0;
	found = UU_FALSE;
/*
.....Initialize symbol geometry list
*/
	if (S_init_geolist() != UU_SUCCESS) return;
/*
.....Find matching symbol
*/
	key = *fkey;
	shaded = *fshade;
	found = S_find_symgeo(symbol,key,ftype,*ctype,shaded,&geopt,&symgeo);
	*fkey = symgeo->key;
/*
.....Return point array if new symbol was created
*/
	if (!found && *ftype == 3)
	{
		nc = UU_LIST_LENGTH(&symgeo->curve);
		if (nc > 500) nc = 500;
		if (nc != 0)
		{
			ptx = (UM_coord *)UU_LIST_ARRAY(&symgeo->curve);
			vcx = (UM_coord *)UU_LIST_ARRAY(&symgeo->cnorm);
			inc = 0;
			for (i=0;i<nc;i++)
			{
				um_vctovc(ptx[i],&pts[inc]);
				um_vctovc(vcx[i],&vecs[inc]);
				inc += 3;
			}
			*npts = nc;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : nclf_cutter_get_bounds(fsym,fkey,ftype,ctype,bounds)
**			Calculates the boundary extents of a cutter profile/symbol.
**    PARAMETERS   
**       INPUT  : 
**          fsym     = Cutter symbol.
**          fkey     = Unibase key of symbol.
**          ftype    = Type of cutter geometry to search for, 2 = Symbol,
**                     3 = Geometry, 4 = Point-List file.
**          ctype    = Type of cutter being defined, 1 = Mill, 2 = Lathe/Blade.
**
**       OUTPUT : 
**          bounds   = XYZ boundary size of cutter geometry.
**
**    RETURNS      : none
**    SIDE EFFECTS : Returns the boundary in the active units.
**    WARNINGS     : none
*********************************************************************/
void nclf_cutter_get_bounds(fsym,fkey,ftype,ctype,bounds)
UM_f77_str_ptr fsym;
UM_int4 *fkey,*ftype,*ctype;
UM_real8 bounds[];
{
	int nc,geopt,i;
	UU_LOGICAL shaded,found;
	UU_KEY_ID key;
	char *p,symbol[MAXSYMLEN];
	UM_coord *ptx;
	UN_motseg_symgeo *symgeo;
	UN_mot_vpbox_struc box;
/*
.....Setup Fortran character string
*/
	p = UM_cstr_of_f77_str(fsym);
	nc = MAXSYMLEN;
	strncpy(symbol,p,nc);
	ul_strip_blanks(symbol,&nc);
/*
.....Initialize routine
*/
	bounds[0] = bounds[1] = bounds[2] = 0.;
	found = UU_FALSE;
	shaded = -1;
	key = *fkey;
/*
.....Find matching symbol
*/
	found = S_find_symgeo(symbol,key,ftype,*ctype,shaded,&geopt,&symgeo);
/*
.....Found entity
.....Calculate bounding box
*/
	if (found)
	{
/*
........Cutter profile
*/
		if (*ftype == 3 || *ftype == 4)
		{
			ncl_cutter_box_init(&box);
			ptx = (UM_coord *)UU_LIST_ARRAY(&symgeo->curve);
			nc = UU_LIST_LENGTH(&symgeo->curve);
			for (i=0;i<nc;i++) ncl_cutter_box_add(&box,ptx[i]);
		}
/*
........Cutter symbol
*/
		else if (*ftype == 2)
		{
			ncl_cutsym_box(symgeo->segno,&box);
		}
/*
.....Cutter symbol not defined
*/
		if (box.ll[0] > box.ur[0])
		{
			bounds[0] = 0.;
			bounds[1] = 0.;
			bounds[2] = 0.;
		}
/*
.....Store X,Y,Z dimensions
*/
		else
		{
/*
........Mill cutter
*/
			if (*ctype == 1)
			{
				if (box.ll[0] == box.ur[0]) box.ll[0] = 0.;
				bounds[0] = fabs(box.ur[0]-box.ll[0]);
				if (*ftype != 2) bounds[0] = bounds[0] * 2.;
				bounds[1] = 0.;
				bounds[2] = fabs(box.ur[2]-box.ll[2]);
			}
/*
........Lathe cutter
*/
			else
			{
				bounds[0] = fabs(box.ur[0]-box.ll[0]);
				bounds[1] = fabs(box.ur[1]-box.ll[1]);
				bounds[2] = 0.;
			}
		}
	}
/*
.....Cutter symbol not defined
*/
	else
	{
		bounds[0] = 0.;
		bounds[1] = 0.;
		bounds[2] = 0.;
	}
/*
.....Convert to correct units
*/
	for (i=0;i<6;i++) UM_len_inttoext(bounds[i],bounds[i]);
}

/*********************************************************************
**    I_FUNCTION     : ncl_cutter_is_blade()
**			Determines if the current cutter definition is a blade cutter.
**    PARAMETERS   
**       INPUT  : none
**
**       OUTPUT : none
**
**    RETURNS      : UU_TRUE if cutter is a blade.
**    SIDE EFFECTS :
**         Returns the blade condition based on the active cutter as
**         set by 'ncl_cutter_get'.
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_cutter_is_blade()
{
	return(Sis_blade);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_reset()
**			Frees the cutter storage lists.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_reset()
{
	int i;
	UN_motseg_symgeo *lcut;
/*
.....Free cutter list
*/
	if (Sactive_cutter != -1)
	{
		Sactive_cutter = -1;
		uu_list_free(&Scutter_list);
/*
.....Cutter parameter list
*/
		uu_list_free(&Scutparm_list);
/*
.....Cutter attribute list
*/
		uu_list_free(&Scutattr_list);
/*
.....Cutter symbol list
*/
		uu_list_free(&Ssymbol_list);
/*
.....Symbol geometry list
*/
		if (Sinit_geo)
		{
			lcut = (UN_motseg_symgeo *)UU_LIST_ARRAY(&Ssymgeo_list);
			for (i=0;i<UU_LIST_LENGTH(&Ssymgeo_list);i++)
			{
				uu_list_free(&lcut[i].curve);
				uu_list_free(&lcut[i].cnorm);
			}
			uu_list_free(&Ssymgeo_list);
			Sinit_geo = UU_FALSE;
		}
	}
/*
.....Motion attribute list
*/
	ncl_motattr_reset();
/*
.....MCD file list
*/
	ncl_mcd_free();
}

/*********************************************************************
**    E_FUNCTION     : nclf_get_tool_symlib(symlib)
**			Returns the default symbol library for cutters.
**    PARAMETERS   
**       INPUT  : none
**          
**       OUTPUT : 
**          symlib   = Default cutter symbol library as specified by
**				           NCL_TOOL_SYMLIB.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_get_tool_symlib(fsymlib)
UM_f77_str_ptr fsymlib;
{
	int nc,i;
	char *cstr;
/*
.....Get default symbol library
*/
	cstr = UM_cstr_of_f77_str(fsymlib);
	ncl_get_tool_symlib(cstr);
/*
.....Pad the string with spaces
*/
	nc = strlen(cstr);
	for (i=nc;i<20;i++) cstr[i] = ' ';
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_tool_symlib(symlib)
**			Returns the default symbol library for cutters.
**    PARAMETERS   
**       INPUT  : none
**          
**       OUTPUT : 
**          symlib   = Default cutter symbol library as specified by
**				           NCL_TOOL_SYMLIB.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_tool_symlib(symlib)
char *symlib;
{
	char *p,*ux_getenv();
	char clib[256];
	int i,len;
/*
.....Get default symbol library
*/
	p = ux_getenv("NCL_TOOL_SYMLIB",UX_NPRTERRS);
	if (p != UU_NULL)
		strcpy(symlib,p);
	else
		strcpy(symlib,"symlib");
	len = strlen (symlib);
	strncpy(clib, symlib, len);
	for (i=len;i<256;i++)
		clib[i] = ' ';
	scutlb (clib);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_struc_size()
**			Returns the size of the cutter list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : Size of list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cutter_struc_size()
{
/*
.....Return size of list
*/
	return(UU_LIST_LENGTH(&Scutter_list));
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_attr_size()
**			Returns the size of the cutter attribute list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : Size of list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cutter_attr_size()
{
/*
.....Return size of list
*/
	return(UU_LIST_LENGTH(&Scutattr_list));
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_parms_size()
**			Returns the size of the cutter parameter list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : Size of list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cutter_parms_size()
{
/*
.....Return size of list
*/
	return(UU_LIST_LENGTH(&Scutparm_list));
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_symbol_size()
**			Returns the size of the cutter symbol list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : Size of list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cutter_symbol_size()
{
/*
.....Return size of list
*/
	return(UU_LIST_LENGTH(&Ssymbol_list));
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_symgeo_size()
**			Returns the size of the cutter geometry list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : Size of list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cutter_symgeo_size()
{
/*
.....Return size of list
*/
	return(UU_LIST_LENGTH(&Ssymgeo_list));
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_get_ptrs(ptrs)
**			Gets the active cutter list pointers.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          ptrs     = [0] = Cutter, [1] = Geometry, [2] = IsBlade.
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_get_ptrs(ptrs)
int ptrs[];
{
	ptrs[0] = Sactive_cutter;
	ptrs[1] = Ssymgeo_ptr;
	ptrs[2] = Sis_blade;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_set_ptrs(ptrs)
**			Sets the active cutter list pointers.
**    PARAMETERS   
**       INPUT  :
**          ptrs     = [0] = Cutter, [1] = Geometry, [2] = IsBlade.
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_set_ptrs(ptrs)
int ptrs[];
{
	Sactive_cutter = ptrs[0];
	Ssymgeo_ptr = ptrs[1];
	Sis_blade = ptrs[2];
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_store_struc(cutseg)
**			Pushes a cutter structure onto the list.
**    PARAMETERS   
**       INPUT  : 
**          cutseg   = Cutter structure.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_store_struc(cutseg)
UN_motseg_cutter_struc *cutseg;
{
	UN_motseg_cutter_struc temp;
	if (Sactive_cutter == -1) S_list_init(&temp);
	uu_list_push(&Scutter_list,cutseg);
	Sactive_cutter = 0;
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_store_parms(cparms)
**			Pushes a cutter parameters structure onto the list.
**    PARAMETERS   
**       INPUT  : 
**          cparms   = Cutter parameters structure.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_store_parms(cparms)
UN_motseg_cutparm *cparms;
{
	uu_list_push(&Scutparm_list,cparms);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_store_attr(cattr)
**			Pushes a cutter attribute structure onto the list.
**    PARAMETERS   
**       INPUT  : 
**          cattr   = Cutter attribute structure.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_store_attr(cattr)
UN_motseg_cutattr *cattr;
{
	uu_list_push(&Scutattr_list,cattr);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_store_symgeo(symgeo)
**			Pushes a cutter symbol geometry structure onto the list.
**    PARAMETERS   
**       INPUT  : 
**          symgeo   = Cutter geometry structure.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_store_symgeo(symgeo)
UN_motseg_symgeo *symgeo;
{
	uu_list_push(&Ssymgeo_list,symgeo);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cutter_store_symbol(symbol)
**			Pushes a cutter symbol structure onto the list.
**    PARAMETERS   
**       INPUT  : 
**          symbol   = Cutter symbol structure.
**
**       OUTPUT : none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutter_store_symbol(symbol)
UN_motseg_symbol *symbol;
{
	uu_list_push(&Ssymbol_list,symbol);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_format_tool(sbuf,cutseg)
**       Formats the requested cutter for output to a form list.
**    PARAMETERS
**       INPUT  :
**          tool     Tool number to format.
**       OUTPUT :
**          sbuf     Output buffer to hold formatted tool.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_format_tool(sbuf,cutseg)
char *sbuf;
UN_motseg_cutter *cutseg;
{
	int i,ncparm;
	UU_REAL rnum,cparm[6];
	char lnum[40];
	NCL_cutter_type type;
/*
.....Initialize routine
*/
	type = NCL_CUTTER_MILL;
	if (cutseg->cutter->parms[8] > 10) type = NCL_CUTTER_LATHE;
	else if (cutseg->cutter->parms[0] < 0.) type = NCL_CUTTER_BLADE;
/*
.....Format tool
*/
	strcpy(sbuf,"CUTTER/");
/*
........Lathe cutter
*/
	if (type == NCL_CUTTER_LATHE)
	{
		strcat(sbuf,"LATHE,");
		UM_len_inttoext(cutseg->cutter->parms[0],cparm[0]);
		cparm[0] = cparm[0] / 2.;
		UM_len_inttoext(cutseg->cutter->parms[6],cparm[1]);
		UM_len_inttoext(cutseg->cutter->parms[2],cparm[2]);
		cparm[3] = cutseg->cutter->parms[7];
		if (cparm[3] == 0.)
		{
			UM_len_inttoext(cutseg->cutter->parms[5],cparm[4]);
		}
		else
			cparm[4] = cutseg->cutter->parms[5];
		ncparm = 5;
	}
/*
........Blade cutter
*/
	else if (type == NCL_CUTTER_BLADE)
	{
		strcat(sbuf,"BLADE,");
		UM_len_inttoext(cutseg->cutter->parms[5],cparm[0]);
		UM_len_inttoext(cutseg->cutter->parms[6],cparm[1]);
		UM_len_inttoext(cutseg->cutter->parms[2],cparm[2]);
		cparm[3] = asin(cutseg->cutter->parms[7]) * UM_RADIAN;
		ncparm = 4;
	}
/*
........Mill cutter
*/
	else
	{
		ncparm = 0;
		for (i=0;i<6;i++)
		{
			if (i <= 2 || i == 4)
			{
				UM_len_inttoext(cutseg->cutter->parms[i],cparm[i]);
			}
			else
				cparm[i] = cutseg->cutter->parms[i];
			if (cparm[i] != 0) ncparm = i + 1;
		}
	}
/*
........Cutter parameters
*/
	if (ncparm > 0)
	{
		sprintf(lnum,"%g",cparm[0]);
		strcat(sbuf,lnum);
		for (i=1;i<ncparm;i++)
		{
			sprintf(lnum,", %g",cparm[i]);
			strcat(sbuf,lnum);
		}
/*
........Cutter symbol
*/
		if (cutseg->cutsym->geo->type > 1)
		{
			strcat(sbuf,",");
			strcat(sbuf,cutseg->cutsym->geo->symbol);
		}
	}
	else if (cutseg->cutsym->geo->symbol[0] != '\0')
		strcat(sbuf,cutseg->cutsym->geo->symbol);
	else
		strcat(sbuf,"0");
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    I_FUNCTION     : S_list_init(cutseg)
**			Initializes the cutter storage lists.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          cutseg   = Initialized Cutter definition structure.
**    RETURNS      : UU_FAILURE if memory could not be allocated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_list_init(cutseg)
UN_motseg_cutter_struc *cutseg;
{
/*
.....Initialize cutter list
*/
	uu_list_init(&Scutter_list,sizeof(UN_motseg_cutter_struc),60,20);
	if (Scutter_list.data == UU_NULL) goto failed;
/*
.....Initialize cutter parameter list
*/
	uu_list_init(&Scutparm_list,sizeof(UN_motseg_cutparm),20,20);
	if (Scutparm_list.data == UU_NULL) goto failed;
/*
.....Initialize cutter attribute list
*/
	uu_list_init(&Scutattr_list,sizeof(UN_motseg_cutattr),20,20);
	if (Scutattr_list.data == UU_NULL) goto failed;
/*
.....Initialize cutter symbol list
*/
	uu_list_init(&Ssymbol_list,sizeof(UN_motseg_symbol),20,20);
	if (Ssymbol_list.data == UU_NULL) goto failed;
/*
.....Initialize symbol geometry list
*/
	if (S_init_geolist() != UU_SUCCESS) goto failed;
/*
.....Initialize provided cutter structure
*/
	cutseg->cutter = -1;
	cutseg->cattr = -1;
	cutseg->cutsym = -1;
	cutseg->shank = -1;
	cutseg->holder = -1;
	return(UU_SUCCESS);
/*
.....Failed trying to allocate lists
*/
failed:;
	return(UU_FAILURE);
}

/*********************************************************************
**    I_FUNCTION     : S_init_geolist()
**			Initializes the cutter geometry storage list.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_FAILURE if memory could not be allocated.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_init_geolist()
{
/*
.....Initialize symbol geometry list
*/
	if (!Sinit_geo)
	{
		uu_list_init(&Ssymgeo_list,sizeof(UN_motseg_symgeo),20,20);
		if (Ssymgeo_list.data == UU_NULL) return(UU_FAILURE);
		Sinit_geo = UU_TRUE;
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_match_parms(cutseg,cutter)
**			Determines if the cutter parameters match an existing cutter
**       definition.  Returns a pointer to the cutter parameter structure
**       that matches the input cutter.
**    PARAMETERS   
**       INPUT  : 
**          cutseg   = Active cutter structure.
**          cutter   = Cutter parameters.
**
**       OUTPUT : 
**          cutseg   = Updated cutter structure.
**
**    RETURNS      : UU_TRUE if parameters match active cutter.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_match_parms(cutseg,cutter)
UN_motseg_cutter_struc *cutseg;
UU_REAL *cutter;
{
	int i,j,nc;
	UU_LOGICAL matched;
	UN_motseg_cutparm *mycut,lcut;
/*
.....Initialize routine
*/
	ncl_cutter_get_parms(&mycut,cutseg->cutter);
/*
.....See if parameters match active cutter
*/
	matched = UU_FALSE;
	if (mycut != UU_NULL)
	{
		matched = UU_TRUE;
		for (i=0;i<9;i++)
		{
			if (cutter[i] != mycut->parms[i])
			{
				matched = UU_FALSE;
				break;
			}
		}
	}
/*
.....No match
.....Search cutter list
*/
	if (!matched)
	{
		nc = UU_LIST_LENGTH(&Scutparm_list);
		mycut = (UN_motseg_cutparm *)UU_LIST_ARRAY(&Scutparm_list);
		for (j=0;j<nc;j++)
		{
			matched = UU_TRUE;
			for (i=0;i<9;i++)
			{
				if (cutter[i] != mycut[j].parms[i])
				{
					matched = UU_FALSE;
					break;
				}
			}
			if (matched)
			{
				cutseg->cutter = j;
				break;
			}
		}
/*
........No match found
........add new record
*/
		if (!matched)
		{
			for (i=0;i<9;i++) lcut.parms[i] = cutter[i];
			uu_list_push(&Scutparm_list,&lcut);
			cutseg->cutter = nc;
		}
	}
/*
.....End of routine
*/
	return(matched);
}

/*********************************************************************
**    I_FUNCTION     : S_match_attr(cutseg,cattr)
**			Determines if the cutter parameters match an existing cutter
**       definition.  Returns a pointer to the cutter parameter structure
**       that matches the input cutter.
**    PARAMETERS   
**       INPUT  : 
**          cutseg   = Active cutter structure.
**          cattr    = Cutter attributes to match.
**
**       OUTPUT : 
**          cutseg   = Updated cutter structure.
**
**    RETURNS      : UU_TRUE if parameters match active cutter.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_match_attr(cutseg,cattr)
UN_motseg_cutter_struc *cutseg;
UN_motseg_cutattr *cattr;
{
	int j,nc;
	UU_LOGICAL matched;
	UN_motseg_cutattr *mycut,lcut;
/*
.....Initialize routine
*/
	ncl_cutter_get_attr(&mycut,cutseg->cattr);
	if (cattr->shaded[0] || cattr->shaded[1] || cattr->shaded[2])
		cattr->segfl = UU_TRUE;
/*
.....See if parameters match active cutter
*/
	matched = UU_FALSE;
	if (mycut != UU_NULL)
	{
		if (cattr->segfl == mycut->segfl && cattr->mov == mycut->mov &&
			cattr->shaded[0] == mycut->shaded[0] &&
			cattr->shaded[1] == mycut->shaded[1] &&
			cattr->shaded[2] == mycut->shaded[2] &&
			cattr->color[0] == mycut->color[0] &&
			cattr->color[1] == mycut->color[1] &&
			cattr->color[2] == mycut->color[2] &&
			cattr->pen[0] == mycut->pen[0] && cattr->pen[1] == mycut->pen[1] &&
			cattr->pen[2] == mycut->pen[2] &&
			cattr->trans[0] == mycut->trans[0] &&
			cattr->trans[1] == mycut->trans[1] &&
			cattr->trans[2] == mycut->trans[2])
				matched = UU_TRUE;
	}
/*
.....No match
.....Search cutter list
*/
	if (!matched)
	{
		nc = UU_LIST_LENGTH(&Scutattr_list);
		mycut = (UN_motseg_cutattr *)UU_LIST_ARRAY(&Scutattr_list);
		matched = UU_FALSE;
		for (j=0;j<nc;j++)
		{
		if (cattr->segfl == mycut[j].segfl && cattr->mov == mycut[j].mov &&
			cattr->shaded[0] == mycut[j].shaded[0] &&
			cattr->shaded[1] == mycut[j].shaded[1] &&
			cattr->shaded[2] == mycut[j].shaded[2] &&
			cattr->color[0] == mycut[j].color[0] &&
			cattr->color[1] == mycut[j].color[1] &&
			cattr->color[2] == mycut[j].color[2] &&
			cattr->pen[0] == mycut[j].pen[0] && cattr->pen[1] == mycut[j].pen[1] &&
			cattr->pen[2] == mycut[j].pen[2] &&
			cattr->trans[0] == mycut[j].trans[0] &&
			cattr->trans[1] == mycut[j].trans[1] &&
			cattr->trans[2] == mycut[j].trans[2])
			{
				matched = UU_TRUE;
				cutseg->cattr = j;
				break;
			}
		}
/*
........No match found
........add new record
*/
		if (!matched)
		{
			lcut = *cattr;
			uu_list_push(&Scutattr_list,&lcut);
			cutseg->cattr = nc;
		}
	}
/*
.....End of routine
*/
	return(matched);
}

/*********************************************************************
**    I_FUNCTION     : S_match_symbol(cutseg,which,symbol,symkey,type,parms,
**                                    shaded)
**			Determines if the cutter symbol matches an existing cutter
**       definition.  Returns a pointer to the cutter symbol structure
**       that matches the input cutter.
**    PARAMETERS   
**       INPUT  : 
**          cutseg   = Active cutter structure.
**          which    = 0 = Cutter symbol, 1 = Shank, 2 = Holder.
**          symbol   = Symbol to use for display.
**          symkey   = Unibase key of geometry to use for display.
**          intype   = Symbol defined by - 1 = Parameters, 2 = Symbol,
**                     3 = Curve, 4 = From tool library.
**          parms    = Symbol parameters or attach point.
**          shaded   = 1 = This symbol should be shaded.
**
**       OUTPUT : 
**          cutseg   = Updated cutter structure.
**
**    RETURNS      : UU_TRUE if parameters match active cutter.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_match_symbol(cutseg,which,symbol,symkey,type,parms,shaded)
UN_motseg_cutter_struc *cutseg;
int which,type,shaded;
UU_REAL parms[];
char *symbol;
UU_KEY_ID symkey;
{
	static int Scn=0;
	int i,nc,*symptr,it,geopt;
	UU_LOGICAL matched;
	char sym[MAXSYMLEN],csym[MAXSYMLEN];
	UN_motseg_symbol *mycut,*mysym,lcut;
	UN_motseg_symgeo *symgeo,*mygeo;
	UN_motseg_cutparm *myparm;
/*
.....Initialize routine
*/
	strcpy(csym,symbol);
	mygeo = (UN_motseg_symgeo *)UU_LIST_ARRAY(&Ssymgeo_list);
	switch (which)
	{
	case 0:
		if (LW_active) sprintf(sym,"CUTTER%d",Scn++);
		else strcpy(sym,"CUTTER");
		ncl_cutter_get_symbol(&mysym,cutseg->cutsym);
		symptr = &(cutseg->cutsym);
		break;
	case 1:
		if (LW_active) sprintf(sym,"SHANK%d",Scn++);
		else strcpy(sym,"SHANK");
		ncl_cutter_get_symbol(&mysym,cutseg->shank);
		symptr = &(cutseg->shank);
		break;
	case 2:
		if (LW_active) sprintf(sym,"HOLDER%d",Scn++);
		else strcpy(sym,"HOLDER");
		ncl_cutter_get_symbol(&mysym,cutseg->holder);
		symptr = &(cutseg->holder);
		break;
	}
/*
.....Nondisplayed cutter
*/
	if (type == 0) parms[0] = parms[1] = parms[2] = parms[3] = 0.;
/*
.....Get symbol geometry
........Cutter defined by parameters
*/
	matched = UU_FALSE;
	if (type <= 1)
	{
		if (mysym != UU_NULL)
		{
			if ((type == 0 && mysym->geo->type == 0) ||
				(type == mysym->geo->type && strcmp(sym,mysym->geo->symbol) == 0))
			{
					matched = UU_TRUE;
					geopt = mysym->geoptr;
					symgeo = &mygeo[geopt];
			}
		}
		if (!matched)
		{
			nc = UU_LIST_LENGTH(&Ssymbol_list);
			mycut = (UN_motseg_symbol *)UU_LIST_ARRAY(&Ssymbol_list);
			for (i=0;i<nc;i++)
			{
				if ((type == 0 && mygeo[mycut[i].geoptr].type == 0) ||
					(type == mygeo[mycut[i].geoptr].type &&
					strcmp(sym,mygeo[mycut[i].geoptr].symbol) == 0))
				{
					matched = UU_TRUE;
					geopt = mycut[i].geoptr;
					symgeo = &mygeo[geopt];
					break;
				}
			}
		}
	}
/*
.....Cutter defined by geo/symbol
.....Or parameter cutter not yet defined
*/
	if (!matched)
	{
		ncl_cutter_get_parms(&myparm,cutseg->cutter);
		it = 1;
		if (myparm->parms[8] > 10 || myparm->parms[0] < 0.) it = 2;
		if (type == 1) strcpy(csym,sym);
		S_find_symgeo(csym,symkey,&type,it,shaded,&geopt,&symgeo);
	}
/*
.....See if parameters match active cutter
*/
	matched = UU_FALSE;
	if (mysym != UU_NULL)
	{
		if (geopt == mysym->geoptr &&
			parms[0] == mysym->parms[0] && parms[1] == mysym->parms[1] &&
			parms[2] == mysym->parms[2] && parms[3] == mysym->parms[3])
				matched = UU_TRUE;
	}
/*
.....No match
.....Search cutter list
*/
	if (!matched)
	{
		nc = UU_LIST_LENGTH(&Ssymbol_list);
		mycut = (UN_motseg_symbol *)UU_LIST_ARRAY(&Ssymbol_list);
		matched = UU_FALSE;
		for (i=0;i<nc;i++)
		{
			if (geopt == mycut[i].geoptr &&
				parms[0] == mycut[i].parms[0] && parms[1] == mycut[i].parms[1] &&
				parms[2] == mycut[i].parms[2] && parms[3] == mycut[i].parms[3])
			{
				matched = UU_TRUE;
				*symptr = i;
				break;
			}
		}
/*
.....No match found
.....add new record
*/
		if (!matched)
		{
			for (i=0;i<4;i++) lcut.parms[i] = parms[i];
			lcut.geoptr = geopt;
			lcut.geo = symgeo;
			if (type >= 2)
			{
				lcut.zlim[0] = parms[2];
				lcut.zlim[1] = parms[3];
			}
			else
			{
				lcut.zlim[0] = 0.;
				lcut.zlim[1] = parms[2];
			}
			uu_list_push(&Ssymbol_list,&lcut);
			*symptr = nc;
		}
	}
/*
.....End of routine
*/
	return(matched);
}

/*********************************************************************
**    I_FUNCTION     : S_find_symgeo(symbol,inkey,geo_type,ctype,shaded,sympt,
**		                               symgeo)
**			Determines if the cutter symbol matches an existing symbol.
**       Returns a pointer to the cutter symbol geometry structure
**       that matches the input symbol.
**    PARAMETERS   
**       INPUT  : 
**          symbol   = Symbol to find.
**          inkey    = Unibase key of symbol geometry.
**          geo_type = Type of cutter geometry to search for, 2 = Symbol,
**                     3 = Geometry, 4 = Point-List file.
**          ctype    = Type of cutter beind defined, 1 = Mill, 2 = Lathe/Blade.
**          ishade   = 1 = Cutter is shaded.  -1 = Don't care if shaded.
**
**       OUTPUT : 
**          geo_type = Updated geometry type.
**          sympt    = Pointer into symbol geometry array of matching symbol.
**          symgeo   = Pointer to actual symbol geometry structure.
**
**    RETURNS      : UU_TRUE if symbol geometry already existed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_find_symgeo(symbol,inkey,geo_type,ctype,ishade,sympt,
	symgeo)
char *symbol;
UU_KEY_ID inkey;
int *geo_type,ctype;
UU_LOGICAL ishade;
int *sympt;
UN_motseg_symgeo **symgeo;
{
	int i,nc,status,type,inc,stat,npts;
	UU_LOGICAL matched,shaded;
	UU_KEY_ID symkey;
	UM_coord *pts;
	UN_motseg_symgeo *mycut,lcut;
	double length;
/*
.....Initialize symbol list
*/
	if (S_init_geolist() != UU_SUCCESS) return(UU_FALSE);
/*
.....Initialize routine
*/
	matched = UU_FALSE;
	shaded = ishade; if (shaded == -1) shaded = 0;
	nc = UU_LIST_LENGTH(&Ssymgeo_list);
	mycut = (UN_motseg_symgeo *)UU_LIST_ARRAY(&Ssymgeo_list);
	inc = Ssymgeo_ptr;
	type = *geo_type;
	if (type == -3) type = 3;
	symkey = inkey;
/*
.....If symbol
.....Determine if it can be simple geometry
*/
	if (type == 2)
	{
		strcpy(lcut.symbol,symbol);
		lcut.shaded = shaded;
		stat = ncl_get_cutsym(&lcut,0,ctype);
		if (stat == UU_SUCCESS)
		{
			type = lcut.type;
			symkey = lcut.key;
		}
		else
		{
			stat = ncl_load_cutprof(symbol,&pts,&npts, &length);
			if (stat == UU_SUCCESS) type = 4;
		}
	}
		
/*
.....Search for existing symbol
*/
	if (type >= 2)
	{
		for (i=0;i<nc;i++,inc++)
		{
			if (inc == nc) inc = 0;
			if (((strcmp(symbol,mycut[inc].symbol) == 0 &&
				(symkey == mycut[inc].key || symkey == 0) &&
				type == mycut[inc].type) &&
				((mycut[inc].type != 2 || (ishade==-1) ||(shaded == mycut[inc].shaded)))))
			{
				matched = UU_TRUE;
				*sympt = inc;
				*symgeo = &mycut[inc];
				Ssymgeo_ptr = inc;
				break;
			}
		}
	}
/*
.....No match found
.....add new record
*/
	if (!matched)
	{
		if (type != 0 && type != 2)
		{
			uu_list_init(&lcut.curve,sizeof(UM_coord),50,20);
			uu_list_init(&lcut.cnorm,sizeof(UM_vector),50,20);
		}
		else
		{
			uu_list_init0(&lcut.curve);
			uu_list_init0(&lcut.cnorm);
		}
		strcpy(lcut.symbol,symbol);
		lcut.key = symkey;
		lcut.shaded = shaded;
		lcut.type = type;
		lcut.axis[0] = lcut.axis[1] = lcut.axis[2] = 0.;
		status = UU_SUCCESS;
		lcut.segno = 0;
		if (type == 2)
			status = ncl_get_cutsym(&lcut,1,ctype);
		else if (type == 3 && *geo_type != -3)
			status = ncl_get_cutgeo(&lcut,symkey,ctype);
		else if (type == 4)
			status = ncl_get_cutprof(&lcut,ctype);
		else if (type == 5)
			status = ncl_get_cutgeo(&lcut,symkey,1);
		if (status != UU_SUCCESS)
		{
			lcut.type = 1;
			type = 1;
		}
		uu_list_push(&Ssymgeo_list,&lcut);
		mycut = (UN_motseg_symgeo *)UU_LIST_ARRAY(&Ssymgeo_list);
		*symgeo = &mycut[nc];
		*sympt = nc;
		Ssymgeo_ptr = nc;
	}
/*
.....End of routine
*/
	*geo_type = type;
	return(matched);
}

/*********************************************************************
**    I_FUNCTION     : S_match_cutter(cutseg,cutin)
**			Determines if a cutter structure matches the active cutter
**       structure.
**    PARAMETERS   
**       INPUT  : 
**          cutseg   = Active cutter structure.
**          cutin    = New cutter structure.
**
**       OUTPUT : 
**          cutseg   = Updated cutter structure.
**
**    RETURNS      : UU_TRUE if parameters match active cutter.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_match_cutter(cutseg,cutin)
UN_motseg_cutter_struc *cutseg,*cutin;
{
	int j,nc;
	UU_LOGICAL matched;
	UN_motseg_cutter_struc *mycut;
/*
.....See if parameters match active cutter
*/
	matched = UU_FALSE;
	if (cutseg != UU_NULL)
	{
		if (cutseg->cutter == cutin->cutter && 
			cutseg->cattr == cutin->cattr &&
			cutseg->cutsym == cutin->cutsym &&
			cutseg->shank == cutin->shank &&
			cutseg->holder == cutin->holder) matched = UU_TRUE;
	}
/*
.....No match
.....Search cutter list
*/
	if (!matched)
	{
		nc = UU_LIST_LENGTH(&Scutter_list);
		mycut = (UN_motseg_cutter_struc *)UU_LIST_ARRAY(&Scutter_list);
		matched = UU_FALSE;
		for (j=0;j<nc;j++)
		{
			if (mycut[j].cutter == cutin->cutter && 
				mycut[j].cattr == cutin->cattr &&
				mycut[j].cutsym == cutin->cutsym &&
				mycut[j].shank == cutin->shank &&
				mycut[j].holder == cutin->holder)
			{
				matched = UU_TRUE;
				Sactive_cutter = j;
				*cutseg = mycut[j];
				break;
			}
		}
/*
........No match found
........add new record
*/
		if (!matched)
		{
			uu_list_push(&Scutter_list,cutin);
			mycut = (UN_motseg_cutter_struc *)UU_LIST_ARRAY(&Scutter_list);
			Sactive_cutter = nc;
			*cutseg = mycut[nc];
		}
	}
/*
.....End of routine
*/
	return(matched);
}

void getcut_hgt(fsym, length)
UM_f77_str_ptr fsym;
UM_real8 *length;
{
	int nc,geopt,i,inc;
	UU_LOGICAL shaded,found;
	UU_KEY_ID key;
	char *p, symbol[MAXSYMLEN];
	int stat,npts;
	UM_coord *pts;
	UN_motseg_symgeo *mycut;
	struct UB_symbol_rec symrec;
	struct UM_point_rec ent;
	double dlen;
/*
.....Setup Fortran character string
*/
	p = UM_cstr_of_f77_str(fsym);
	nc = MAXSYMLEN;
	strncpy(symbol,p,nc);
	ul_strip_blanks(symbol,&nc);
/*
.....Initialize routine
*/
	npts = 0;
/*
.....Initialize symbol list
*/
	if (S_init_geolist() != UU_SUCCESS) return(UU_FALSE);

	nc = UU_LIST_LENGTH(&Ssymgeo_list);
	mycut = (UN_motseg_symgeo *)UU_LIST_ARRAY(&Ssymgeo_list);
	inc = Ssymgeo_ptr;
/*
.....Search for existing symbol
*/
	for (i=0;i<nc;i++,inc++)
	{
		if (strcmp(symbol, mycut[inc].symbol) == 0)
		{
			break;
		}
	}
	*length = 0.0;
	if (i>=nc)
		return;
/*
.....Get the symbol structure
*/
	ncl_parse_label(symbol, symrec.label,&symrec.subscr);
	if (ub_get_symmaster_by_name(&symrec,&found,1,1) == UU_SUCCESS)
	{
		if (found)
		{
			if (symrec.no_snap_nod>0)
			{
				ent.key = symrec.snap_nod[0].snap_key;
				if (uc_retrieve_data(&ent, sizeof(ent)) != UU_SUCCESS) 
					goto next;
				*length = ent.pt[2];
				return;
			}
		}
	}
next:;
	stat = ncl_load_cutprof(symbol,&pts,&npts, &dlen);
	*length = dlen;
}
