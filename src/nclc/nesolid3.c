/*********************************************************************
**    NAME         :  nesolid3.c
**       CONTAINS:
**             ncl_load_stl()
**             nclf_save_stl()
**				ncl_solid_conv()
**    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nesolid3.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       11/22/17 , 12:31:58
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
#include "mdcoord.h"
#include "mdpick.h"
#include "mdunits.h"
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
#include	"nclver.h"

#define CO05 .99999962
/*#define DEBUG_T 1*/

static int Snseg=0;
static UM_coord *Sseg;

#ifdef DEBUG_T
static int Tload,Ttri,Tpoly,Tdisp,Tsort;
#endif

void ncl_push_triangle(),ncl_sort_triangles();

/*********************************************************************
**    E_FUNCTION :  ncl_load_stl(solid,file,units,bin,xf,optimize,tf,dlist,tess)
**       Imports an STL file as a Visual Solid.
**    PARAMETERS   
**       INPUT  : 
**          solid    Solid to calculate lists for.
**          file     STL file to load.
**          units    STL file units, 0 = Inches, 1 = Millimeters.
**          xf       1 = Apply xform to display/tessellation lists,
**                   2 = Aply xform and force lists to be recalculated.
**          optimize 0 = Don't optimize edge display.  1 = Merge planar
**                   edges in display.
**          tf       Transformation matrix.
**       OUTPUT :
**          solid    Contains the bounding box of the solid.
**          bin      0 = Ascii STL file, 1 = Binary.
**          xf       Reset to 1 if it equaled 2 on input.
**          dlist    Display list.
**          tess     Tessellation list.
**    RETURNS      :
**          UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS :  none
**    WARNINGS     : none
*********************************************************************/
int ncl_load_stl(solid,file,units,bin,xf,optimize,tf,dlist,tess)
struct UM_solid_rec *solid;
char *file;
int units,*bin,*xf,optimize;
UM_transf tf;
UU_LIST *dlist;
UM_tessellation *tess;
{
	int stat,status,i,fstat,mode;
	UU_LOGICAL opened;
	UU_REAL *pts;
	UM_coord ptx;
	UM_tessellation tesstmp;
	UX_pathname fullname,lbuf;
	FILE *fd;
	char sbuf[80];
#ifdef DEBUG_T
	int stims,stimm,etims,etimm;
#endif
/*
.....Initialize routine
*/
	opened = UU_FALSE;
	um_init_tess(tess);
	um_init_tess(&tesstmp);
#ifdef DEBUG_T
	Tload = Ttri = Tpoly = Tdisp = Tsort = 0;
#endif
/*
.....Use existing display and tessellation lists
.....if they already exist
*/
	if (solid->no_displst != 0)
	{
		uu_list_init(dlist,sizeof(UM_coord),solid->no_displst,10);
		for (i=0;i<solid->no_displst*3;i+=3)
			uu_list_push(dlist,&solid->displst[i]);
		status = ncl_get_tessellation(solid,tess);
		if (*xf == 2)
		{
			pts = (UU_REAL *)UU_LIST_ARRAY(dlist);
			ncl_displst_xform(pts,tf);
			ncl_xform_tessellation(tess,tf);
			*xf = 1;
		}
		goto done;
	}
/*
.....Make sure the file exists
*/
	mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
	strcpy(fullname,file);
	stat = ux_file_inquire(UU_NULL,UU_NULL,fullname,UU_NULL,UU_NULL,
		&mode,&fstat,lbuf,UX_NPRTERRS);
/*
.....File does not exist
.....try the NCL_INCDIR directory
*/
	if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		ul_build_full_fname("NCL_INCDIR",file,".stl",fullname);
		mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
		stat = ux_file_inquire(UU_NULL,UU_NULL,fullname,UU_NULL,UU_NULL,
			&mode,&fstat,lbuf,UX_NPRTERRS);
		if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS)) goto failed;
	}
/*
.....Determine if Binary or Ascii file
*/
	stat = ul_ipv_stl_binary(&fd,fullname,bin);
	if (stat != UU_SUCCESS) goto openerr;
/*
.....Open STL file
*/
	if (*bin)
		stat = ux_fopen0(fullname,"rb",&fd);
	else
		stat = ux_fopen0(fullname,"r",&fd);
	if (stat != UU_SUCCESS) goto openerr;
	opened = UU_TRUE;
/*
.....Initialize display and tessellation lists
*/
	uu_list_init(dlist,sizeof(UM_coord),30000,3000);
	if (UU_LIST_NULLPTR(dlist)) goto failed;
	ptx[0] = ptx[1] = ptx[2] = 0.;
	uu_list_push(dlist,ptx);

	stat = uu_list_init(&tess->vertices,sizeof(UM_coord),10000,2000);
	if (UU_LIST_NULLPTR(&tess->vertices)) goto failed;
	uu_list_init0(&tess->normals);
	uu_list_init0(&tess->tri);

	uu_list_init0(&tesstmp.vertices);
	if (optimize)
	{
		stat = uu_list_init(&tesstmp.normals,sizeof(UM_vector),10000,2000);
		if (UU_LIST_NULLPTR(&tesstmp.normals)) goto failed;
		stat = uu_list_init(&tesstmp.tri,sizeof(UM_tript),10000,2000);
		if (UU_LIST_NULLPTR(&tesstmp.tri)) goto failed;
	}
	else
	{
		stat = uu_list_init(&tess->normals,sizeof(UM_vector),10000,2000);
		if (UU_LIST_NULLPTR(&tess->normals)) goto failed;
		stat = uu_list_init(&tess->tri,sizeof(UM_tript),10000,2000);
		if (UU_LIST_NULLPTR(&tess->tri)) goto failed;
	}
/*
.....Load binary file
*/
#ifdef DEBUG_T
	gtimx(&stims,&stimm);
#endif
	if (*bin) stat = S_load_binary_stl(fd,units,*xf,optimize,tf,tess,&tesstmp,
		solid->box);
/*
....Load ascii file
*/
	else stat = S_load_ascii_stl(fd,units,*xf,optimize,tf,tess,&tesstmp,
		solid->box);
	if (stat != UU_SUCCESS) goto failed;
#ifdef DEBUG_T
	gtimx(&etims,&etimm);
	Tload = Tload + ((etims-stims)*1000 + (etimm-stimm));
#endif
/*
.....Sort triangles based on normals
*/
	if (optimize)
	{
		stat = uu_list_init(&tess->normals,sizeof(UM_vector),tesstmp.ntri,100);
		if (UU_LIST_NULLPTR(&tess->normals)) goto failed;
		stat = uu_list_init(&tess->tri,sizeof(UM_tript),tesstmp.ntri,100);
		if (UU_LIST_NULLPTR(&tess->tri)) goto failed;
		ncl_sort_triangles(tess,&tesstmp);
	}
/*
.....Convert triangle display to
.....polyline display list
*/
	status = UU_SUCCESS;
	status = S_create_display_list(dlist,tess);
	if (*xf == 2) *xf = 1;
	goto done;
/*
.....Could not read STL file
*/
openerr:
	status = UX_NO_ACCESS;
	goto done;
/*
.....Could not read STL file
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (optimize && tesstmp.ntri != 0) um_free_tess(&tesstmp);
	if (opened) ux_fclose0(fd);
	if (status == UU_FAILURE && tess->np != 0) um_free_tess(tess);
#ifdef DEBUG_T
	sprintf(sbuf,"Load STL = %d",Tload);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Push Triangles = %d",Ttri);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Sort Triangles = %d",Tsort);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Display List = %d",Tdisp);
	NclxDbgPstr(sbuf);
	sprintf(sbuf,"Push Polygon = %d",Tpoly);
	NclxDbgPstr(sbuf);
#endif
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  nclf_save_stl(file,nsf,units,inbin,dtol,ierr)
**       Saves a list of solids/surfaces as an STL file.
**    PARAMETERS   
**       INPUT  : 
**          file     STL file to create.
**          nsf      Number of solids/surfaces to save.
**          units    STL file units, 0 = Inches, 1 = Millimeters.
**          inbin    0 = Ascii STL file, 1 = Binary.
**          dtol     Tolerance to tessellate entities at.
**       OUTPUT :
**          ierr     Non-zero when an error is encountered.
**    RETURNS      :
**          UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS :  none
**    WARNINGS     : none
*********************************************************************/
void nclf_save_stl(file,nsf,units,inbin,dtol,ierr)
char *file;
UM_int2 *nsf,*units,*inbin,*ierr;
UU_REAL *dtol;
{
	int status,i,j,nent,nc,inum,ntri,ntess;
	UM_int2 idx,sc175;
	UU_LOGICAL opened,bin;
	char ustr[20],dstr[20],tstr[20],sbuf[80];
	UU_REAL tsav,rval,tol;
	UM_coord pts[3],*verts;
	UM_plane plane;
	UM_tessellation *tess;
	UM_tript *tript;
	UU_LIST dlist;
	FILE *fd;
	struct UC_entitydatabag eptr;
	struct UM_solid_rec *solid;
/*
.....Initialize routine
*/
	*ierr = 0;
	nent = *nsf;
	bin = *inbin;
	tol = *dtol;
	tess = UU_NULL;
	ntess = 0;
	opened = UU_FALSE;
/*
.....Open file for writing
*/
	if (bin)
		status = ux_fopen0(file,"wb",&fd);
	else
		status = ux_fopen0(file,"w",&fd);
	if (status != UU_SUCCESS) goto openerr;
	opened = UU_TRUE;
/*
.....Create header
*/
	if (*units == 1) strcpy(ustr,"MM");
	else strcpy(ustr,"INCH");
	ux_get_date(dstr,tstr);
	if (bin)
	{
		sprintf(sbuf,"NCL V%.3f   Units = %s  Created: %s %s",NCL_version,
			ustr,dstr,tstr);
		nc = strlen(sbuf);
		for (i=nc;i<80;i++) sbuf[i] = ' ';
		nc = 80;
	}
	else
	{
		sprintf(sbuf,"solid NCL V%.3f   Units = %s  Created: %s %s\n",NCL_version,
			ustr,dstr,tstr);
		nc = strlen(sbuf);
	}
	ux_fwrite0(sbuf,nc,1,fd,&inum);
	if (status != UU_SUCCESS) goto openerr;
/*
.....Allocate memory for tessellation lists
*/
	tess = (UM_tessellation *)uu_malloc(sizeof(UM_tessellation)*nent);
	if (tess == UU_NULL) goto failed;
/*
.....Get tessellation lists
*/
	sc175 = 175; getsc(&sc175,&tsav); rval = tol; setscv(&sc175,&rval);
	for (i=0;i<nent;i++)
	{
		idx = i + 1;
		gtsky(&idx,&eptr.key);
		status = ncl_retrieve_data(&eptr,sizeof(eptr));
		if (status != UU_SUCCESS) goto failed;
/*
........Tessellate solid within tolerance
*/
		if (eptr.rel_num == UM_SOLID_REL)
		{
			solid = (struct UM_solid_rec *)&eptr;
			status = ncl_solid_calc_lists(solid,solid->sdata,solid->no_sdata,
				&dlist,&tess[i]);
			setscv(&sc175,&tsav);
			if (status == UU_SUCCESS) ntess++;
			else um_free_tess(&tess[i]);
			uu_list_free(&dlist);
		}
/*
.....Tessellate surface within tolerance
*/
		else
		{
			ncl_set_boundary_toler(tol);
			ncl_set_tess_parms(UM_TESS_TOLER,tol,0,0);
			um_init_tess(&tess[i]);
			status = ncl_tessellate_surf(&eptr,&tess[ntess]);
			if (status == UU_SUCCESS) ntess++;
			else um_free_tess(&tess[i]);
		}
	}
	if (ntess == 0) goto failed;
/*
.....Binary files require a count of the triangles
*/
	if (bin)
	{
		ntri = 0;
		for (i=0;i<nent;i++) ntri += tess[i].ntri;
		ux_fwrite0(&ntri,sizeof(int),1,fd,&inum);
		if (status != UU_SUCCESS) goto openerr;
	}
/*
.....Loop through geometry
*/
	for (i=0;i<nent;i++)
	{
/*
........Loop through triangles
*/
		tript = (UM_tript *)UU_LIST_ARRAY(&tess[i].tri);
		verts = (UM_coord *)UU_LIST_ARRAY(&tess[i].vertices);
		for (j=0;j<tess[i].ntri;j++)
		{
/*
............Get triangle points
..............Changed input flag to '0' instead of '1' so the points
..............are not treated like vectors when moved to model
..............coordinate system - ASF 12/16/13.
*/
			ncl_wcstomcs(0,verts[(int)tript[j].n1],pts[0]);
			ncl_wcstomcs(0,verts[(int)tript[j].n2],pts[1]);
			ncl_wcstomcs(0,verts[(int)tript[j].n3],pts[2]);
			if (*units == 1)
			{
				um_vctmsc(pts[0],25.4,pts[0]);
				um_vctmsc(pts[1],25.4,pts[1]);
				um_vctmsc(pts[2],25.4,pts[2]);
			}
/*
............Calculate normal vector
*/
			um_plane1(pts[0],pts[1],pts[2],&plane);
/*
...........Create binary STL triangle
*/
			if (bin) S_save_binary_tri(fd,pts,plane.n);
/*
...........Create Ascii STL triangle
*/
			else S_save_ascii_tri(fd,pts,plane.n);
		}
	}
/*
.....Write out End-of-File
*/
	if (!bin)
	{
		strcpy(sbuf,"endsolid\n");
		nc = strlen(sbuf);
		ux_fwrite0(sbuf,nc,1,fd,&inum);
		if (status != UU_SUCCESS) goto openerr;
	}
	goto done;
/*
.....Could not access STL file
*/
openerr:
	*ierr = 2;
	goto done;
/*
.....Failed to create STL file
*/
failed:
	if (*ierr == 0) *ierr = 1;
/*
.....End of routine
.........Free tessellation lists
*/
done:
	if (tess != UU_NULL)
	{
		for (i=0;i<ntess;i++) um_free_tess(&tess[i]);
		uu_free(tess);
	}
	if (opened)
	{
		ux_fclose0(fd);
		if (*ierr != 0) ux_delete(file,UX_NPRTERRS);
	}
	return;
}

/*********************************************************************
**    I_FUNCTION :  S_load_ascii_stl(fd,units,xf,optimize,tf,tess1,tess2,box)
**       Creates the display and tessellation lists from an Ascii STL file.
**    PARAMETERS   
**       INPUT  : 
**          fd       File descriptor for STL file.
**          units    0 = File is in Inches, 1 = Millimeters.
**          xf       1 = Apply xform to display/tessellation lists,
**                   2 = Aply xform and force lists to be recalculated.
**          optimize 0 = Don't optimize edge display.  1 = Merget planar
**                   edges in display.
**          tf       Transformation matrix.
**       OUTPUT :
**          tess1    Main tessellation list, receives vertices from
**                   this routine.
**          tess2    Secondary tessellation list.  Receives normals and
**                   triangles from this routine, to be sorted into
**                   'tess1' after all triangles are added.
**          box      Bounding box of solid.
**    RETURNS      :
**          UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS :  none
**    WARNINGS     : none
*********************************************************************/
static int S_load_ascii_stl(fd,units,xf,optimize,tf,tess1,tess2,box)
FILE *fd;
int units,xf;
UM_transf tf;
UM_tessellation *tess1,*tess2;
UU_REAL *box;
int optimize;
{
	int inc,nc,stat,status,j;
	UU_LOGICAL first=UU_TRUE;
	char sbuf[256],styp[80],ssub[80];
	UM_coord pt[3];
	UM_vector nvec;
	UM_plane plane;
/*
.....Read STL file
*/
	inc = 0;
	do
	{
		stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
		if (stat == UX_EOF) break;
		if (stat != UU_SUCCESS) goto failed;
		if (nc == 0) continue;
/*
.....Parse input record
*/
		nc = sscanf(sbuf,"%s",styp);
		if (nc == 0) continue;
/*
.....Facet normal
*/
		if (strcmp(styp,"facet") == 0)
		{
			nc = sscanf(sbuf,"%s %s %lf %lf %lf",styp,ssub,&nvec[0],&nvec[1],
				&nvec[2]);
			if (nc != 5) goto failed;
			um_unitvc(nvec,nvec);
		}
/*
.....Vertex
*/
		else if (strcmp(styp,"vertex") == 0)
		{
			if (inc > 2) goto failed;
			nc = sscanf(sbuf,"%s %lf %lf %lf",styp,&pt[inc][0],&pt[inc][1],
				&pt[inc][2]);
			if (nc != 4) goto failed;
			inc++;
		}
/*
.....End loop
*/
		else if (strcmp(styp,"endloop") == 0)
		{
			if (inc != 3) goto failed;
			for (j=0;j<3;j++)
			{
				if (units == 1) um_vctmsc(pt[j],1./25.4,pt[j]);
				if (xf) um_cctmtf(pt[j],tf,pt[j]);
				if (first)
				{
					ncl_init_box(pt[j],box);
					first = UU_FALSE;
				}
				else ncl_update_box(pt[j],box);
			}
			um_plane1(pt[0],pt[1],pt[2],&plane);
			ncl_push_triangle(tess1,tess2,pt,plane.n,optimize);
			inc = 0;
		}
	} while (stat == UU_SUCCESS);
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to load file
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_load_binary_stl(fd,units,xf,optimize,tf,tess1,tess2,box)
**       Creates the display and tessellation lists from a Binary STL file.
**    PARAMETERS   
**       INPUT  : 
**          fd       File descriptor for STL file.
**          units    0 = File is in Inches, 1 = Millimeters.
**          xf       1 = Apply xform to display/tessellation lists,
**                   2 = Aply xform and force lists to be recalculated.
**          tf       Transformation matrix.
**          optimize 0 = Don't optimize edge display.  1 = Merget planar
**                   edges in display.
**       OUTPUT :
**          tess1    Main tessellation list, receives vertices from
**                   this routine.
**          tess2    Secondary tessellation list.  Receives normals and
**                   triangles from this routine, to be sorted into
**                   'tess1' after all triangles are added.
**          box      Bounding box of solid.
**    RETURNS      :
**          UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS :  none
**    WARNINGS     : none
*********************************************************************/
static int S_load_binary_stl(fd,units,xf,optimize,tf,tess1,tess2,box)
FILE *fd;
int units,xf;
UM_transf tf;
UM_tessellation *tess1,*tess2;
UU_REAL *box;
int optimize;
{
	int nc,stat,status,ntri,i,j,inc;
	UU_LOGICAL first=UU_TRUE;
	float fnum[12];
	char sbuf[82];
	UM_coord pt[3];
	UM_coord nvec;
	UM_plane plane;
/*
.....Read header
*/
	stat = ux_fread0(sbuf,sizeof(char),80,fd,&nc);
	if (stat != UU_SUCCESS || nc == 0) goto failed;
	stat = ux_fread0(&ntri,sizeof(int),1,fd,&nc);
	if (stat != UU_SUCCESS || nc == 0) goto failed;
	for (i=0;i<ntri;i++)
	{
/*
.....Read normal vector & vertices
*/
		stat = ux_fread0(fnum,sizeof(float),12,fd,&nc);
		if (stat != UU_SUCCESS || nc < 3) goto failed;
/*
.....Read attribute data
*/
		stat = ux_fread0(sbuf,sizeof(char),2,fd,&nc);
		if (stat != UU_SUCCESS || nc < 2) goto failed;
/*
.....Convert input data to UU_REALs
*/
		inc = 3;
		for (j=0;j<3;j++)
		{
			nvec[j] = fnum[j];
			pt[j][0] = fnum[inc];
			pt[j][1] = fnum[inc+1];
			pt[j][2] = fnum[inc+2];
			inc += 3;
		}
/*
.....Store triangle
*/
		for (j=0;j<3;j++)
		{
			if (units == 1) um_vctmsc(pt[j],1./25.4,pt[j]);
			if (xf) um_cctmtf(pt[j],tf,pt[j]);
			if (first)
			{
				ncl_init_box(pt[j],box);
				first = UU_FALSE;
			}
			else ncl_update_box(pt[j],box);
		}
		um_plane1(pt[0],pt[1],pt[2],&plane);
		ncl_push_triangle(tess1,tess2,pt,plane.n,optimize);
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to load file
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  ncl_push_triangle(tess1,tess2,invert,invec,optimize)
**       Pushes the triangle vertices and normal vector onto the
**       tessellation list.  Duplicate vertices will be removed from
**       the tessellation list.
**    PARAMETERS   
**       INPUT  : 
**          invert   Vertices of triangle.
**          invec    Triangle normal vector.
**          optimize 0 = Don't optimize edge display.  1 = Merge planar
**                   edges in display.
**       OUTPUT :
**          tess1    Main tessellation list, receives vertices from
**                   this routine.  Receives normals and triangles also
**                   if optimization is disabled.
**          tess2    Secondary tessellation list.  Receives normals and
**                   triangles from this routine, to be sorted into
**                   'tess1' after all triangles are added.  Remains
**                   empty if optimization is disabled.
**    RETURNS      :
**          UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS :
**          Only one normal is stored with each triangle, rather than
**          one with each triangle vertex in normal tessellation.
**    WARNINGS     : none
*********************************************************************/
void ncl_push_triangle(tess1,tess2,invert,invec,optimize)
UM_tessellation *tess1,*tess2;
UM_coord *invert;
UM_vector invec;
int optimize;
{
	int i,np;
	UU_REAL um_sqdis(),tol;
	UM_coord *pts,verts[3];
	UM_vector nvec;
	UM_tript tript;
	UM_tessellation *tess1a;
#ifdef DEBUG_T
	int stims,stimm,etims,etimm;
#endif
/*
......Initialize routine
*/
	if (optimize) tess1a = tess2;
	else tess1a = tess1;
#ifdef DEBUG_T
	gtimx(&stims,&stimm);
#endif
/*
.....Initialize triangle
*/
	tript.n1 = -1;
	tript.n2 = -1;
	tript.n3 = -1;
	pts = (UM_coord *)UU_LIST_ARRAY(&tess1->vertices);
	np = UU_LIST_LENGTH(&tess1->vertices);
/*
.....Convert triangle to modelling plane
*/
	ncl_mcstowcs(0,invert[0],verts[0]);
	ncl_mcstowcs(0,invert[1],verts[1]);
	ncl_mcstowcs(0,invert[2],verts[2]);
	ncl_mcstowcs(1,invec,nvec);
/*
.....Remove duplicate vertices from triangle
.....as compared to existing tessellation list
*/
	if (optimize)
	{
		tol = .002 * .002;
		for (i=np-1;i>=0;i--)
		{
			if (tript.n1 == -1)
				if (UM_SQDIS(verts[0],pts[i]) < tol) tript.n1 = i;
			if (tript.n2 == -1)
				if (UM_SQDIS(verts[1],pts[i]) < tol) tript.n2 = i;
			if (tript.n3 == -1)
				if (UM_SQDIS(verts[2],pts[i]) < tol) tript.n3 = i;
			if (tript.n1 != -1 && tript.n2 != -1 && tript.n3 != -1) break;
		}
	}
/*
.....Push unmatched vertices
*/
	if (tript.n1 == -1)
	{
		uu_list_push(&tess1->vertices,verts[0]);
		tript.n1 = np;
		np++;
	}
	if (tript.n2 == -1)
	{
		uu_list_push(&tess1->vertices,verts[1]);
		tript.n2 = np;
		np++;
	}
	if (tript.n3 == -1)
	{
		uu_list_push(&tess1->vertices,verts[2]);
		tript.n3 = np;
		np++;
	}
	tess1->np = np;
/*
.....Push triangle onto tessellation list
*/
	uu_list_push(&tess1a->tri,&tript);
	tess1a->ntri++;
/*
.....Push normal vector onto list
.....One normal per triangle
*/
	uu_list_push(&tess1a->normals,nvec);
#ifdef DEBUG_T
	gtimx(&etims,&etimm);
	Ttri = Ttri + ((etims-stims)*1000 + (etimm-stimm));
#endif
}

/*********************************************************************
**    I_FUNCTION :  ncl_sort_triangles(tess1,tess2)
**       Sorts the tessellation triangles from 'tess2' and places them
**       in order based their normals into 'tess1'.
**    PARAMETERS   
**       INPUT  : 
**          tess2    Tessellation list containing list of triangles
**                   and normals to be sorted.
**       OUTPUT :
**          tess1    Tessellation list.
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void ncl_sort_triangles(tess1,tess2)
UM_tessellation *tess1,*tess2;
{
	int i,ist,ien,ntri1,ntri2;
	UM_vector *vc1,*vc2;
	UM_tript *tri1,*tri2;
#ifdef DEBUG_T
	int stims,stimm,etims,etimm;
#endif
/*
.....Initialize routine
*/
	tri1 = (UM_tript *)UU_LIST_ARRAY(&tess1->tri);
	tri2 = (UM_tript *)UU_LIST_ARRAY(&tess2->tri);
	vc1 = (UM_vector *)UU_LIST_ARRAY(&tess1->normals);
	vc2 = (UM_vector *)UU_LIST_ARRAY(&tess2->normals);
	ntri1 = 0;
	ntri2 = UU_LIST_LENGTH(&tess2->tri);
#ifdef DEBUG_T
	gtimx(&stims,&stimm);
#endif
/*
.....Sort triangles based on normal vectors
*/
	ist = 0; ien = ntri2;
	do
	{
/*
........Store first triangle
*/
		tri1[ntri1].n1 = tri2[ist].n1;
		tri1[ntri1].n2 = tri2[ist].n2;
		tri1[ntri1].n3 = tri2[ist].n3;
		um_vctovc(vc2[ist],vc1[ntri1]);
		ntri1++;
		ntri2--;
		tri2[ist].n1 = -1;
		if (ntri2 == 0) break;
/*
........Match rest of triangles
*/
		for (i=ist+1;i<ien;i++)
		{
			if (tri2[i].n1 != -1)
			{
				if (UM_DOT(vc2[ist],vc2[i]) > CO05)
				{
					tri1[ntri1].n1 = tri2[i].n1;
					tri1[ntri1].n2 = tri2[i].n2;
					tri1[ntri1].n3 = tri2[i].n3;
					um_vctovc(vc2[i],vc1[ntri1]);
					ntri1++;
					ntri2--;
					tri2[i].n1 = -1;
					if (ntri2 == 0) break;
				}
			}
		}
		for (ist=ist+1;ist<ien;ist++) if (tri2[ist].n1 != -1) break;
		for (ien=ien-1;ien>=ist;ien--) if (tri2[ien].n1 != -1) break;
		ien++;
	} while (ntri2 != 0);
/*
.....Set list values
*/
	tess1->ntri = ntri1;
	tess1->normals.cur_cnt = ntri1;
	tess1->tri.cur_cnt = ntri1;
#ifdef DEBUG_T
	gtimx(&etims,&etimm);
	Tsort = Tsort + ((etims-stims)*1000 + (etimm-stimm));
#endif
}

/*********************************************************************
**    I_FUNCTION :  S_create_display_list(tess,dlist)
**       Converts the tessellation list to a polygon display list.
**       Duplicate line segments will be removed from the display
**       list and vertices reordered on coplanar faces to optimize
**       the display.
**    PARAMETERS   
**       INPUT  : 
**          tess     Tessellation list.
**       OUTPUT :
**          dlist    Polygon display list.
**    RETURNS      :
**          UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS :
**          Only one normal is stored with each triangle, rather than
**          one with each triangle vertex in normal tessellation.
**    WARNINGS     : none
*********************************************************************/
static int S_create_display_list(dlist,tess)
UU_LIST *dlist;
UM_tessellation *tess;
{
	int i,status,np,ntri,pary[6];
	UM_coord *pts;
	UM_vector *vcs,nvec;
	UU_LIST plist;
	UM_tript *tript;
#ifdef DEBUG_T
	int stims,stimm,etims,etimm;
#endif
/*char sbuf[80];*/
/*
.....Initialize list pointers
*/
	pts = (UM_coord *)UU_LIST_ARRAY(&tess->vertices);
	np = tess->np;
	vcs = (UM_vector *)UU_LIST_ARRAY(&tess->normals);
	tript = (UM_tript *)UU_LIST_ARRAY(&tess->tri);
	ntri = tess->ntri;
#ifdef DEBUG_T
	gtimx(&stims,&stimm);
#endif
/*
.....Initialize working list
*/
	uu_list_init(&plist,sizeof(int)*6,ntri*6,500);
	if (UU_LIST_NULLPTR(&plist)) goto failed;
/*
.....Work within coplanar triangles only
*/
	um_vctovc(vcs[0],nvec);
	for (i=0;i<ntri;i++)
	{
/*
........Normal vectors differ
........Create polygon and
........Add to display list
*/
		if (UM_DOT(vcs[i],nvec) < CO05)
		{
/*sprintf(sbuf,"Nvec = %lf,%lf,%lf",nvec[0],nvec[1],nvec[2]);
NclxDbgPstr(sbuf);*/
			status = S_push_polygon(dlist,&plist,pts);
			if (status != UU_SUCCESS) goto failed;
			UU_LIST_EMPTY(&plist);
			um_vctovc(vcs[i],nvec);
		}
/*
........Add triangle to working list
*/
		pary[0] = tript[i].n1; pary[1] = tript[i].n2; pary[2] = tript[i].n2;
		pary[3] = tript[i].n3; pary[4] = tript[i].n3; pary[5] = tript[i].n1;
		uu_list_push(&plist,pary);
	}
/*
.....Push last polygon onto display list
*/
	status = S_push_polygon(dlist,&plist,pts);
	uu_list_free(&plist);
	goto done;
/*
.....Failed to create display list
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	if (Sseg != UU_NULL) uu_free(Sseg);
	Sseg = UU_NULL;
	Snseg = 0;
#ifdef DEBUG_T
	gtimx(&etims,&etimm);
	Tdisp = Tdisp + ((etims-stims)*1000 + (etimm-stimm));
#endif
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_push_polygon(dlist,plist,verts)
**       Pushes the line segments made from a list of triangle vertices
**       onto a display list.  The display list is optimized to remove
**       duplicate segments and reordered in hopes of creating a single
**       polygon.
**    PARAMETERS   
**       INPUT  : 
**          plist    Triangle vertices pointers.
**          verts    Vertices of triangle.
**          optimize 0 = Don't optimize edge display.  1 = Merget planar
**                   edges in display.
**       OUTPUT :
**          dlist    Updated display list.
**    RETURNS      :
**          UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS :
**          Only one normal is stored with each triangle, rather than
**          one with each triangle vertex in normal tessellation.
**    WARNINGS     : none
*********************************************************************/
static int S_push_polygon(dlist,plist,verts)
UU_LIST *dlist,*plist;
UM_coord *verts;
{
	int i,j,*ipt,np,npts,ngeo,inc,jinc,nseg,status,isiz;
	UM_coord *dpt,ptx;
#ifdef DEBUG_T
	int stims,stimm,etims,etimm;
#endif
/*char sbuf[80];*/
/*
.....Initialize routine
*/
#ifdef DEBUG_T
	gtimx(&stims,&stimm);
#endif
	ipt = (int *)UU_LIST_ARRAY(plist);
	np = UU_LIST_LENGTH(plist) * 3;
	npts = np;
	dpt = (UM_coord *)UU_LIST_ARRAY(dlist);
	ngeo = dpt[0][0];
/*sprintf(sbuf,"Npts = %d",npts);
NclxDbgPstr(sbuf);*/
/*
.....Remove duplicate line segments
*/
{
	inc = 0;
	for (i=0;i<np-1;i++)
	{
		jinc = inc + 2;
		if (ipt[inc] != -1)
		{
			for (j=i+1;j<np;j++)
			{
/*
........Found zero length line segment
........Remove it
*/
				if (ipt[inc] == ipt[inc+1])
				{
					ipt[inc] = -1;
					npts--;
					break;
				}
/*
........Found duplicate line segments
........Remove them both
*/
				if ((ipt[inc] == ipt[jinc] && ipt[inc+1] == ipt[jinc+1]) || 
					(ipt[inc+1] == ipt[jinc] && ipt[inc] == ipt[jinc+1]))
				{
					ipt[inc] = ipt[jinc] = -1;
					npts -= 2;
					break;
				}
				jinc += 2;
			}
		}
		inc += 2;
	}
/*
.....Store polyline(s) in display list
.....Reorder segments to form as least amount of
.....polylines as possible
*/
	isiz = sizeof(UM_coord) * npts * 2;
	if (isiz > Snseg)
	{
		if (Sseg != UU_NULL) uu_free(Sseg);
		Sseg = (UM_coord *)uu_malloc(isiz);
		if (Sseg == UU_NULL) goto failed;
		Snseg = isiz;
	}
	while (npts > 0)
	{
		inc = 0;
		while (inc < np*2 && npts > 0)
		{
			nseg = 0;
			if (ipt[inc] != -1)
			{
/*
........Store first segment of polyline
*/
				um_vctovc(verts[ipt[inc]],Sseg[nseg++]);
				um_vctovc(verts[ipt[inc+1]],Sseg[nseg++]);
				ipt[inc] = -1;
				jinc = inc;
				npts--;
				for (i=0;i<np*2 && npts>0;i+=2)
				{
					if (ipt[i] == -1) continue;
/*
........Found adjoining segment
........Add to polyline
*/
					if (ipt[i] == ipt[jinc+1] || ipt[i+1] == ipt[jinc+1])
					{
						if (ipt[i+1] == ipt[jinc+1]) ipt[i+1] = ipt[i];
							um_vctovc(verts[ipt[i+1]],Sseg[nseg++]);
						ipt[i] = -1;
						npts--;
						jinc = i;
						i = 0;
					}
				}
/*
........Store polyline in display list
*/
				if (nseg != 0)
				{
/*int k;*/
					ptx[0] = nseg; ptx[1] = 1; ptx[2] = 0;
					uu_list_push(dlist,ptx);
					uu_list_push_multiple(dlist,nseg,Sseg);
/*
sprintf(sbuf,"   Nseg = %d",nseg);
NclxDbgPstr(sbuf);
for(k=0;k<nseg;k++)
{
sprintf(sbuf,"   Pt[%d] = %lf,%lf,%lf",k,Sseg[k][0],Sseg[k][1],Sseg[k][2]);
NclxDbgPstr(sbuf);
}
*/
					ngeo++;
				}
			}
			inc += 2;
		}
	}
}
/*
.....Set number of polylines stored in display list
*/
	dpt = (UM_coord *)UU_LIST_ARRAY(dlist);
	dpt[0][0] = ngeo;
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to create display list
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
#ifdef DEBUG_T
	gtimx(&etims,&etimm);
	Tpoly = Tpoly + ((etims-stims)*1000 + (etimm-stimm));
#endif
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_save_ascii_tri(fd,pts,nvec)
**       Writes to a file the Ascii version of an STL triangle.
**    PARAMETERS   
**       INPUT  : 
**          fd       File descriptor to write to.
**          pts      Corner points of triangle.
**          nvec     Triangle normal vector.
**       OUTPUT : none
**    RETURNS      :
**          UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS :  none
**    WARNINGS     : none
*********************************************************************/
static int S_save_ascii_tri(fd,pts,nvec)
UM_coord *pts;
UM_vector nvec;
FILE *fd;
{
	int i,nc,status,inum;
	char sbuf[80];
/*
.....Initialize routine
*/
	status = UU_FAILURE;
/*
.....Output facet normal
*/
	sprintf(sbuf,"   facet normal %f %f %f\n",nvec[0],nvec[1],nvec[2]);
	nc = strlen(sbuf);
	status = ux_fwrite0(sbuf,nc,1,fd,&inum);
	if (status != UU_SUCCESS) goto done;
/*
.....Ouput triangle vertices
*/
	strcpy(sbuf,"      outer loop\n");
	nc = strlen(sbuf);
	status = ux_fwrite0(sbuf,nc,1,fd,&inum);
	if (status != UU_SUCCESS) goto done;

	for (i=0;i<3;i++)
	{
		sprintf(sbuf,"         vertex %f %f %f\n",pts[i][0],pts[i][1],pts[i][2]);
		nc = strlen(sbuf);
		status = ux_fwrite0(sbuf,nc,1,fd,&inum);
		if (status != UU_SUCCESS) goto done;
	}

	strcpy(sbuf,"      endloop\n");
	nc = strlen(sbuf);
	status = ux_fwrite0(sbuf,nc,1,fd,&inum);
	if (status != UU_SUCCESS) goto done;

	strcpy(sbuf,"   endfacet\n");
	nc = strlen(sbuf);
	status = ux_fwrite0(sbuf,nc,1,fd,&inum);
	if (status != UU_SUCCESS) goto done;
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_save_binary_tri(fd,pts,nvec)
**       Writes to a file the Ascii version of an STL triangle.
**    PARAMETERS   
**       INPUT  : 
**          fd       File descriptor to write to.
**          pts      Corner points of triangle.
**          nvec     Triangle normal vector.
**       OUTPUT : none
**    RETURNS      :
**          UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS :  none
**    WARNINGS     : none
*********************************************************************/
static int S_save_binary_tri(fd,pts,nvec)
UM_coord *pts;
UM_vector nvec;
FILE *fd;
{
	int i,status,inum;
	short i2;
	float fpt[3];
/*
.....Initialize routine
*/
	status = UU_FAILURE;
/*
.....Output facet normal
*/
	fpt[0] = nvec[0]; fpt[1] = nvec[1]; fpt[2] = nvec[2];
	status = ux_fwrite0(fpt,sizeof(float)*3,1,fd,&inum);
	if (status != UU_SUCCESS) goto done;
/*
.....Output triangle vertices
*/
	for (i=0;i<3;i++)
	{
		fpt[0] = pts[i][0]; fpt[1] = pts[i][1]; fpt[2] = pts[i][2];
		status = ux_fwrite0(fpt,sizeof(float)*3,1,fd,&inum);
		if (status != UU_SUCCESS) goto done;
	}
/*
.....Output attribute bit
*/
	i2 = 0;
	status = ux_fwrite0(&i2,sizeof(i2),1,fd,&inum);
	if (status != UU_SUCCESS) goto done;
/*
.....End of routine
*/
done:
	return(status);
}
/*********************************************************************
**    E_FUNCTION : ncl_solid_conv(int nclkey)
**       convert solid current display list and tess list
**			to display solid with right light
**    PARAMETERS   
**       INPUT  : 
**          nclkey:    Solid key
**       OUTPUT : None
**    RETURNS      : -1: fail
**          UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS :  none
**    WARNINGS     : none
*********************************************************************/
int ncl_solid_conv(int nclkey)
{
	UU_REAL tsav,rval,tol;
	UU_REAL *oparms;

	UM_coord pts[3],*verts,*pt;
	UM_coord ptx;
	UM_plane plane;
	UM_tript *tript;
	int status,j,nent,nc,inum,ntri,ntess,stat;
	struct UC_entitydatabag eptr;
	struct UM_solid_rec *solid;
	UM_tessellation tess_orig;
	UU_LIST dlist, dlist_orig;
	UM_tessellation tess, tesstmp;

	eptr.key = nclkey;
	status = ncl_retrieve_data(&eptr, sizeof(eptr));
	if (status != UU_SUCCESS) goto failed;
/*
........Tessellate solid within tolerance
*/
	if (eptr.rel_num == UM_SOLID_REL)
	{
		solid = (struct UM_solid_rec *)&eptr;
		status = ncl_solid_calc_lists(solid, solid->sdata, solid->no_sdata,
					&dlist_orig, &tess_orig);
		if (status == UU_SUCCESS) ntess++;
		else um_free_tess(&tess_orig);
		uu_list_free(&dlist_orig);
	}
/*
.....Initialize display and tessellation lists
*/
	um_init_tess(&tess);
	um_init_tess(&tesstmp);
	uu_list_init(&dlist, sizeof(UM_coord),30000,3000);
	if (UU_LIST_NULLPTR(&dlist)) goto failed;
	ptx[0] = ptx[1] = ptx[2] = 0.;
	uu_list_push(&dlist,ptx);

	stat = uu_list_init(&tess.vertices,sizeof(UM_coord),10000,2000);
	if (UU_LIST_NULLPTR(&tess.vertices)) goto failed;

	uu_list_init0(&tess.normals);
	uu_list_init0(&tess.tri);

	uu_list_init0(&tesstmp.vertices);
	stat = uu_list_init(&tesstmp.normals,sizeof(UM_vector),10000,2000);
	if (UU_LIST_NULLPTR(&tesstmp.normals)) goto failed;
	stat = uu_list_init(&tesstmp.tri,sizeof(UM_tript),10000,2000);
	if (UU_LIST_NULLPTR(&tesstmp.tri)) goto failed;


	tript = (UM_tript *)UU_LIST_ARRAY(&tess_orig.tri);
	verts = (UM_coord *)UU_LIST_ARRAY(&tess_orig.vertices);
	for (j=0;j<tess_orig.ntri;j++)
	{
		ncl_wcstomcs(0,verts[(int)tript[j].n1],pts[0]);
		ncl_wcstomcs(0,verts[(int)tript[j].n2],pts[1]);
		ncl_wcstomcs(0,verts[(int)tript[j].n3],pts[2]);
/*
............Calculate normal vector
*/
		um_plane1(pts[0],pts[1],pts[2],&plane);
		ncl_push_triangle(&tess, &tesstmp, pts,plane.n,1);
	}
/*
.....Sort triangles based on normals
*/
	stat = uu_list_init(&tess.normals,sizeof(UM_vector),tesstmp.ntri,100);
	if (UU_LIST_NULLPTR(&tess.normals)) goto failed;
	stat = uu_list_init(&tess.tri,sizeof(UM_tript),tesstmp.ntri,100);
	if (UU_LIST_NULLPTR(&tess.tri)) goto failed;
	ncl_sort_triangles(&tess, &tesstmp);
/*
.....Convert triangle display to
.....polyline display list
*/
	status = UU_SUCCESS;
	status = S_create_display_list(&dlist, &tess);
/*
.....Update solid
*/
/*
.....Store display & tessellation lists
*/
	pt = (UM_coord *)UU_LIST_ARRAY(&dlist);
	nc = UU_LIST_LENGTH(&dlist);
	status = ur_update_data_varlist(solid->key,2,pt,1,nc);
	if (status != UU_SUCCESS) goto failed;
	status = ncl_store_surflist1(TESSELLATION_LIST, solid, &tess);
	if (status != UU_SUCCESS) goto failed;
	status = ncl_retrieve_data(solid,sizeof(struct UM_solid_rec));
	goto done;
failed:
	status = UU_FAILURE;
done:;
	if (tess_orig.ntri != 0) um_free_tess(&tess_orig);
	if (tesstmp.ntri != 0) um_free_tess(&tesstmp);
	if (status == UU_FAILURE && tess.np != 0) 
	{
		uu_list_free(&dlist);
		um_free_tess(tess);
	}
	return(status);
}

