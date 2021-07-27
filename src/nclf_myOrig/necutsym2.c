/*********************************************************************
**	 NAME:  necutsym2.c
**		 CONTAINS:
**			ncl_cutsym_box
**			ncl_draw_cutsym
**			ncl_get_cutgeo
**			ncl_get_cutsym
**			ncl_move_cutsym
**
**	 COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       necutsym2.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       12/01/15 , 08:27:30
*********************************************************************/

#include "usysdef.h"
#include "zsysdep.h"
#include "class.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "mdcoord.h"
#include "msol.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "bsym.h"
#include "gsegac.h"
#include "gomisc.h"
#include "gtbl.h"
#include "g.h"
#include "ginq.h"
#include "gdidd.h"
#include "gviw.h"
#include "gsegop.h"
#include "gmat4.h"
#include "gconvert.h"
#include "mdcoord.h"
#include "mfort.h"
#include "modef.h"
#include "mplot.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclvx.h"
#include "nccs.h"

#define DTOR (UM_PI/180.0)

struct S_tools_struc
{
	char label[21];
	char class[21];
	UU_LIST pts;
};

static int NCL_shade_symbol = 0;

void ncl_cutsym_tf();

static UU_LOGICAL S_lib_loaded=UU_FALSE;
extern UU_LIST NCL_plib_tools;

/*********************************************************************
**	 E_FUNCTION : ncl_draw_cutsym(cutsym,shadfl)
**			This function draws the requested cutter symbol in an
**			invisible segment.  'ncl_move_cutsym' is used to display
**			this segment.
**	 PARAMETERS	
**		 INPUT  : 
**		 	cutsym  = Name of cutter symbol to draw.
**			shadfl  = >0 = Symbol should be shaded.
**		 OUTPUT :
**			none.
**	 RETURNS: UU_SUCCESS if no problems encoutered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none
*********************************************************************/
int ncl_draw_cutsym(cutsym,shadfl)
char cutsym[];
int shadfl;
{
	int i,ish;
	int status = UU_SUCCESS;
	UU_REAL etfmat[4][3];
	struct UC_entitydatabag ent;
	struct UC_attributedatabag subent_attr;
	struct UB_symbol_rec sym_rec;
	UU_LOGICAL found;
/*
.....Get storage for master symbol instance
*/
	ncl_parse_label(cutsym,sym_rec.label,&sym_rec.subscr);
/*
.....Set shading mode dependant on
.....if cutter is shaded or not
*/
	ish = ncl_shading_mode();
	ncl_set_shademode(shadfl);
	NCL_shade_symbol = shadfl;
/*
.....Is master symbol already loaded ?
*/
	if (ub_get_symmaster_by_name(&sym_rec, &found,1,1) != UU_SUCCESS)
		goto failed;
/*
.....Store the cutter symbol in
.....an invisible digs segment
*/
	for (i=0; i<sym_rec.no_geom; i++) 
	{
/*
........Retrieve symbol subentity
*/
		ent.key = sym_rec.geom[i];
		if (uc_retrieve_data(&ent, sizeof(struct UC_entitydatabag))
			!= UU_SUCCESS)	goto failed;
/*
........Orient subentity in symbol space
*/
		if (uc_retrieve_transf(ent.key, etfmat) != UU_SUCCESS)
			goto failed;
/*
........Get subentity display attributes
*/
		if (uc_retrieve_attr(ent.key, &subent_attr) != UU_SUCCESS)
			goto failed;
/*
........Draw symbol subentity
*/
		if (uc_draw(&ent, etfmat, &subent_attr) != UU_SUCCESS)
			goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
done:;
	ncl_set_shademode(ish);
	NCL_shade_symbol = 0;
	return(status);
}  
/*********************************************************************
**	 E_FUNCTION : int ncl_get_cutgeo(cutsym,key,type)
**			This function evaluates a curve/surface and stores the evaluated
**			points and normals in the curve list.
**	 PARAMETERS	
**		 INPUT  :
**			cutsym->symbol = Label of geometry to load.  May be in the format
**                        'CV&PV' when both a curve and associated
**                        point-vector are provided.
**       key     = Surface of revolution key if provided.  This routine
**                 will load the geometry when 'key' is set to 0.
**       type    = 1 = Surface of revolution, 2 = Composite curve.
**		 OUTPUT :
**			cutsym  = Symbol structure to receive point list.
**	 RETURNS: UU_SUCCESS of no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ncl_get_cutgeo(cutsym,key,type)
UN_motseg_symgeo *cutsym;
UU_KEY_ID key;
int type;
{
	int i,status,nc,npts,ifree;
	char lab1[MAXSYMLEN],lab2[MAXSYMLEN],*p,*strchr();
	UU_REAL tol,ptlwr[2],ptupr[2];
	UM_coord *pts,origpt,*opts;
	UM_vector *vcs,vctmp,vcn,axisvc,*ovcs,vx;
	UM_covec pva;
	UM_transf tfmat;
	UU_LIST *cvlist,*cnlist,ptlist,vclist;
	UM_f77_str f77_str;
	struct NCL_nclpv_rec pv;
	struct NCL_revsurf_rec *revp;
	struct NCL_fixed_databag cv;
	struct UM_solid_rec *solid;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	ifree = 0;
	cvlist = &cutsym->curve;
	cnlist = &cutsym->cnorm;
	origpt[0] = origpt[1] = origpt[2] = 0.;
	vcn[0] = 0.; vcn[1] = 0.; vcn[2] = 1.;
	axisvc[0] = 0.; axisvc[1] = 1.; axisvc[2] = 0.;
	um_vctovc(axisvc,&pva[3]);
/*
.....Geometry key is provided
*/
	cv.key = key;
/*
.....Break out point-vector if specified
.....as CV&PV
*/
	strcpy(lab1,cutsym->symbol);
	p = strchr(lab1,'&');
	if (p != 0 && type == 1)
	{
		*p = '\0'; p++;
		strcpy(lab2,p);
		nc = strlen(lab2);
		UM_init_f77_str(f77_str,lab2,nc);
		getkey(UM_addr_of_f77_str(f77_str),&pv.key);
		if (ncl_retrieve_data(&pv,sizeof(struct NCL_nclpv_rec)) != UU_SUCCESS)
			goto failed;
		um_vctovc(pv.pt,origpt);
		um_vctovc(pv.ve,axisvc);
		um_vctovc(axisvc,&pva[3]);
	}
/*
.....Retrieve the curve/surf
*/
	if (cv.key == 0)
	{
		nc = strlen(lab1);
		UM_init_f77_str(f77_str,lab1,nc);
		getkey(UM_addr_of_f77_str(f77_str),&cv.key);
	}
	if (ncl_retrieve_data_fixed(&cv) != UU_SUCCESS) goto failed;
	if (uc_retrieve_transf(cv.key,tfmat) != UU_SUCCESS) goto failed;
/*
.....Revolved surface
*/
	if (cv.rel_num == NCL_REVSURF_REL)
	{
		revp = (struct NCL_revsurf_rec *)&cv;
		um_vctovc(revp->pta,origpt);
		um_vctovc(revp->vca,axisvc);
		um_vctovc(axisvc,&pva[3]);
		cv.key = revp->cvkey;
		if (ncl_retrieve_data_fixed(&cv) != UU_SUCCESS) goto failed;
		if (uc_retrieve_transf(cv.key,tfmat) != UU_SUCCESS) goto failed;
	}
/*
.....Get the solid data
*/
	if (cv.rel_num == UM_SOLID_REL)
	{
		solid = (struct UM_solid_rec *)&cv;
		ncl_solid_cutter_lists(solid,pva,&ptlist,&vclist,&npts);
		if (npts <= 1) goto failed;
		um_vctovc(&pva[0],origpt);
		um_vctovc(&pva[3],axisvc);
		pts = (UM_coord *)UU_LIST_ARRAY(&ptlist);
		vcs = (UM_vector *)UU_LIST_ARRAY(&vclist);
	}
/*
.....Evolve the curve
*/
	else
	{
		gettol(&tol);
		uu_list_init(&ptlist,sizeof(UM_coord),50,20);
		uu_list_init(&vclist,sizeof(UM_vector),50,20);
		ifree = 1;
		npts = ncl_evolve_all_curves(&cv,tfmat,tol,&ptlist,&vclist,UU_TRUE);
		if (npts <= 1) goto failed;
		pts = (UM_coord *)UU_LIST_ARRAY(&ptlist);
		vcs = (UM_vector *)UU_LIST_ARRAY(&vclist);
	}
/*
.....Generate the revolved curve
*/
	if (type == 1)
	{
		opts = (UM_coord *)uu_malloc(sizeof(UM_coord)*(npts+2));
		if (opts == UU_NULL) goto failed;
		ifree = 2;
		ovcs = (UM_coord *)uu_malloc(sizeof(UM_vector)*(npts+2));
		if (ovcs == UU_NULL) goto failed;
		ifree = 3;
		ul_ipv_gen_revsf(origpt,axisvc,pts,vcs,&npts,opts,ovcs,vx,UU_TRUE,UU_TRUE,
			UU_TRUE);
		um_vctovc(&pva[3],cutsym->axis);
	}
/*
.....Generate the composite curve pointers
*/
	else
	{
		ptlwr[0] = ptupr[0] = pts[0][0]; ptlwr[1] = ptupr[1] = pts[0][1];
		for (i=1;i<npts;i++)
		{
			if (pts[i][0] < ptlwr[0]) ptlwr[0] = pts[i][0];
			if (pts[i][0] > ptupr[0]) ptupr[0] = pts[i][0];
			if (pts[i][1] < ptlwr[1]) ptlwr[1] = pts[i][1];
			if (pts[i][1] > ptupr[1]) ptupr[1] = pts[i][1];
		}
		if (ptupr[0]-ptlwr[0] > ptupr[1]-ptlwr[1])
		{
			cutsym->axis[0] = 1.; cutsym->axis[1] = 0.; cutsym->axis[2] = 0.;
		}
		else
		{
			cutsym->axis[0] = 0.; cutsym->axis[1] = 1.; cutsym->axis[2] = 0.;
		}
		opts = pts;
		ovcs = vcs;
	}
/*
.....Calculate and store vectors
*/
	for (i=0;i<npts;i++)
	{
		um_unitvc(ovcs[i],ovcs[i]);
		um_cross(ovcs[i],vcn,vctmp);
		um_unitvc(vctmp,ovcs[i]);
	}
/*
.....Store points
*/
	ncl_store_cutgeo(cutsym,opts,ovcs,npts,type);
	goto done;
/*
.....Failed to load curve points
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	if (ifree >= 1)
	{
		uu_list_free(&ptlist);
		uu_list_free(&vclist);
		if (ifree >= 2) uu_free(opts);
		if (ifree >= 3) uu_free(ovcs);
	}
	return(status);
}
/*********************************************************************
**    E_FUNCTION     :  ncl_cutsym_box(segno,box)
**			Calculates the bounding box of a cutter symbol.
**    PARAMETERS   
**       INPUT  :
**				segno   = Segment number of CADD cutter symbol.
**       OUTPUT :  
**				box	  = Bounding 3-D box for symbol.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cutsym_box(segno,box)
int segno;
UN_mot_vpbox_struc *box;
{
	int ncmd,icmd,cmdlen,opcode,i,len;
	char *ug_lsielt();
	Gwpoint3 *pta;
	UG_segstli *segptr;
	UG_plylna3op *cmd;
	UG_LSI *listp;
/*
.....Initialize routine
*/
	ncl_cutter_box_init(box);
/*
.....Get pointer to segment data
*/
	segptr=ug_segac(segno);
	if (segptr == NULL) return;
/*
.....Get segment list
*/
	listp=(UG_LSI *)segptr->seglist;
	ncmd = ug_lsinelt((*listp));
/*
.....For each command in segment
*/
	for (icmd=0; icmd < ncmd; ++icmd)
	{
/*
........Get next command
*/
		cmd = (UG_plylna3op *)ug_lsielt(listp,icmd);
		cmdlen = ug_lsilen(listp,icmd)/sizeof(int);
/*
........Get type of command
*/
		opcode=(*cmd).elttype;
		if (opcode<=UG_OPPTR)
		{
			opcode=opcode&(UG_OPPTR-1);
		}
		switch (opcode)
		{
/*
...........3-D Polyline
*/
		case UG_PLYLNA3OP: 
			len=(*(UG_plylna3op *)cmd).len;
			pta = (*(UG_plylna3op *)cmd).pts;
			for (i=0;i<len;i++) ncl_cutter_box_add(box,&pta[i]);
			break;
/*
............3-D Marker
*/
		case UG_PLYMKA3OP: 
			len=(*(UG_plymka3op *)cmd).len;
			pta = (*(UG_plymka3op *)cmd).pts;
			for (i=0;i<len;i++) ncl_cutter_box_add(box,&pta[i]);
			break;
/*
...........Shaded triangles
*/
		case UG_SHADEAREAOP: 
			len=(*(UG_shadearea *)cmd).len;
			pta = (*(UG_shadearea *)cmd).pts;
			for (i=0;i<len;i++) ncl_cutter_box_add(box,&pta[i]);
			break;
		}		/* End Normal Command */
	}			/* For each command in seg */
}
/*********************************************************************
**	 E_FUNCTION : ncl_get_cutsym(symgeo,cre,ictype)
**			This function loads a cutter symbol from a library for use
**			with cutter display.  It stores this symbol in an invisible
**			segment which can be displayed using the 'ncl_move_cutsym'
**			routine.
**	 PARAMETERS	
**		 INPUT  :
**			symgeo  = Cutter symbol structure.
**			cre     = 1 = Symbol graphics segment should be created.
**			ictype  = 1 = Mill cutter, 2 = Lathe cutter.
**		 OUTPUT : none
**	 RETURNS: UU_SUCCESS of no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ncl_get_cutsym(symgeo,cre,ictype)
UN_motseg_symgeo *symgeo;
int cre,ictype;
{
	int i,nc,ish,vmode,vp,type;
	UM_int2 idx,ibat;
	UU_KEY_ID key;
	char slib[256];
	UM_f77_str flib;
	UU_REAL vscale;
	UM_vector vpn,vup;
/*
.....Assume success
*/
	int status = UU_SUCCESS;
/*
.....Get batch setting
*/
	idx = 35; getifl(&idx,&ibat);
/*
.....Load symbol
*/
	UM_init_f77_str(flib,slib,256);
	gcutlb(UM_addr_of_f77_str(flib));
/*
.....Remove trailing spaces
*/
	nc = strlen (slib);
	if (nc>256)
		nc = 256;
	for (i=nc; i>0; i--)
	{
		if (slib[i-1]==' ')
			slib[i-1] = '\0';
		else
			break;
	}
	slib[255] = '\0';
	ul_to_lower(slib);
	if (ncl_load_cutter_symbol(slib,symgeo->symbol,&key,0,&type) != UU_SUCCESS)
		goto failed;
/*
.....Set the cutter type
.....if it was changed to standard geometry
*/
	if (type == 1 && ictype == 2) symgeo->type = 5;
	else if (type == 1 || type == 2) symgeo->type = 3;
	else symgeo->type = 2;
	symgeo->key = key;
/*
.....Now create and store symbol
.....in an invisible segment
*/
	if (cre == 1)
	{
/*
........Cutter is surface of revolution
........Display it as one
*/
		if (type == 1 || type == 2)
		{
			if (ncl_get_cutgeo(symgeo,key,type) != UU_SUCCESS) goto failed;
			symgeo->segno = 0;
		}
/*
........Determine if cutter is shaded
*/
		else if (ibat == 0)
		{
			ncl_motvwp(&vp,vpn,vup,&vscale,&vmode);
			if (symgeo->shaded > 0 && (vmode ==2 || vmode == 3)) ish = 1;
			else ish = 0;
/*
.....Create and store symbol
.....in an invisible segment
*/
			symgeo->segno = gnseg();
			gcreateseg(symgeo->segno);
			gssegdet(symgeo->segno,UG_UNDETECTABLE);
			gssegvis(symgeo->segno,UG_INVISIBLE);
			ncl_draw_cutsym(symgeo->symbol,ish);
			gcloseseg();
		}
	}
	goto done;

failed:;
	symgeo->segno = -1;
	status = UB_FAILURE;
done:;
	return(status);
}  
/*********************************************************************
**    I_FUNCTION     :  ncl_move_cutsym(segno,cpt,tax,blade,box,zhgt,seguse,
**		                                  erase)
**			Displays the CADD cutter symbol at the requested location.
**    PARAMETERS   
**       INPUT  :
**			segno   = Segment number of CADD cutter symbol.
**			cpt     = XYZ position to display symbol at.
**			tax     = Tool axis orientation for symbol.
**			blade   = Directional axis for Ultrasonic blade.
**			seguse  = UU_TRUE = Moving segment is used (1,0,0 tool axis).
**			erase   = 1 = Erase symbol instead of drawing it.
**                 -1 = Don't draw symbol.  Calculate bounding box only.
**       OUTPUT :  
**			box	  = Bounding 3-D box for symbol. (when erase = -1)
**       zhgt    = Maximum extents of cutter symbol.  Used to attach shank.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_move_cutsym(segno,cpt,tax,blade,box,zhgt,seguse,erase)
int erase,segno;
Gfloat cpt[3],tax[3];
UN_motseg_blade *blade;
UN_mot_vpbox_struc *box;
UU_LOGICAL seguse;
UU_REAL zhgt[];
{
	int ncmd,icmd,cmdlen,opcode,i,len,ifl,type,save_lucency;
	char *ug_lsielt();
	Gos systate;
	Gstate state;
	Gwpoint3 *pta,*ptb,*ptc,ptb1[200];
	Gfloat ptpt[3],vcn[3],vco[3];
	UG_segstli *segptr;
	UG_plylna3op *cmd;
	UG_LSI *listp;
	UM_angle um_angle(),ang;
	UM_transf mtf;
	UU_REAL um_dot(),um_mag(),aaxis;
	UM_coord pt0;
	UM_vector vfd,vr;
	UU_LOGICAL isblade;
/*
.....Initialize routine
*/
	if (erase == -1) ncl_cutter_box_init(box);
/*
.....Get pointer to segment data
*/
	zhgt[0] = zhgt[1] = zhgt[2] = 0.;
	segptr=ug_segac(segno);
	if (segptr == NULL) return;
/*
.....Push current transformations if neccessary.
.....Only required if this segment changes modelling xform. 
*/
	if (segptr->xforms & (UG_SEGMODXF)) ug_savestate(&state);
/*
.....Save GKS system stat
*/
	systate=ug_gksos.sysstate;
/*
.....Get segment list
*/
	listp=(UG_LSI *)segptr->seglist;
	ncmd = ug_lsinelt((*listp));
/*
.....Set rotation angle for tool axis
*/
	isblade = ncl_cutter_is_blade();
/*
........Get adjusted forward and right vectors
........for blade cutter
*/
	if (isblade)
	{
		ifl = 0;
		ncl_blade_get_fwd(0,vfd,vr);
		um_vctmsc(vr,-1.,vr);
	}
/*
........Calculate rotation matrix for
........standard cutter
*/
	else
	{
		pt0[0] = pt0[1] = pt0[2] = 0.;
		vcn[0] = 0.; vcn[1] = 0.; vcn[2] = 1.;
		if (seguse)
		{
			ncl_cutsym_tf(pt0,vcn,mtf,&ang,&aaxis,1);
			ifl = 1;
		}
		else
		{
			um_cross(vcn,tax,vco);
			ang = um_mag(vco);
			ifl = 1;
			if (ang < UM_FUZZ && tax[2] > 0.) ifl = 0;
			if (ifl == 1)
			{
				ang = um_angle(vcn,tax);
				um_rotlntf(pt0,vco,ang,mtf);
			}
		}
	}
/*
.....For each command in segment
*/
	for (icmd=0; icmd < ncmd; ++icmd)
	{
/*
........Get next command
*/
		cmd = (UG_plylna3op *)ug_lsielt(listp,icmd);
		cmdlen = ug_lsilen(listp,icmd)/sizeof(int);
/*
........Get type of command
*/
		opcode=(*cmd).elttype;
		if (opcode<=UG_OPPTR)
		{
			opcode=opcode&(UG_OPPTR-1);
		}
/*
........Library command
*/
		if (opcode > UG_OPPTR)
		{
			break;
		}
		else
/*
........Normal command
*/
		{
			switch (opcode)
			{
/*
...........Ignored commands
*/
			case UG_NOOP: break;
			case UG_PAGEOP: break;
			case UG_PLYLNA2OP: break;
			case UG_PLYLNRASOP: break;
			case UG_PLYMKA2OP: break;
			case UG_TEXTOP: break;
			case UG_TEXTRASOP: break;
			case UG_FLAREA3OP: break;
			case UG_FLAREAOP: break;
			case UG_FLAREARASOP: break;
			case UG_CELLOP: break;
			case UG_CELLRUNOP: break;
			case UG_CELLRASOP: break;
			case UG_CELLRUNRASOP: break;
			case UG_PROCOP: break;
			case UG_CALLOP: break;
			case UG_FONTOP: break;
			case UG_CHHGTOP: break;
			case UG_CHEXPOP: break;
			case UG_CHPLANEOP: break;
			case UG_CHUP3OP: break;
			case UG_CHUP2OP: break;
			case UG_CHPATHOP: break;
			case UG_CHSPACEOP: break;
			case UG_CHJUSTOP: break;
			case UG_SYMBOLOP: break;
			case UG_PICKIDOP: break;
			case UG_EDGEFLAGOP: break;
			case UG_TXCOLROP: break;
			case UG_FACOLROP: break;
/*
...........3-D Polyline
*/
			case UG_PLYLNA3OP: 
				len=(*(UG_plylna3op *)cmd).len;
				pta = (*(UG_plylna3op *)cmd).pts;
				if (len <= 200) ptb = ptb1;
				else ptb = (Gwpoint3 *)uu_malloc(sizeof(Gwpoint3)*len);
				for (i=0;i<len;i++)
				{
					ptpt[0] = pta[i].x;
					ptpt[1] = pta[i].y;
					ptpt[2] = pta[i].z;
					if (pta[i].x > zhgt[0]) zhgt[0] = pta[i].x;
					if (pta[i].y > zhgt[1]) zhgt[1] = pta[i].y;
					if (pta[i].z > zhgt[2]) zhgt[2] = pta[i].z;
					if (ifl == 1) um_cctmtf(ptpt,mtf,ptpt);
					if (isblade) ncl_blade_orient(ptpt,vfd,vr,tax,ptpt);
					um_vcplvc(ptpt,cpt,&ptb[i]);
					if (erase == -1) ncl_cutter_box_add(box,&ptb[i]);
				}
				if (erase != -1) gpolyline3(len,ptb);
				if (len > 200) uu_free(ptb);
				break;
/*
............3-D Marker
*/
			case UG_PLYMKA3OP: 
				len=(*(UG_plymka3op *)cmd).len;
				pta = (*(UG_plymka3op *)cmd).pts;
				if (len <= 200) ptb = ptb1;
				else ptb = (Gwpoint3 *)uu_malloc(sizeof(Gwpoint3)*len);
				for (i=0;i<len;i++)
				{
					ptpt[0] = pta[i].x;
					ptpt[1] = pta[i].y;
					ptpt[2] = pta[i].z;
					if (pta[i].x > zhgt[0]) zhgt[0] = pta[i].x;
					if (pta[i].y > zhgt[1]) zhgt[1] = pta[i].y;
					if (pta[i].z > zhgt[2]) zhgt[2] = pta[i].z;
					if (ifl == 1) um_cctmtf(ptpt,mtf,ptpt);
					if (isblade) ncl_blade_orient(ptpt,vfd,vr,tax,ptpt);
					um_vcplvc(ptpt,cpt,&ptb[i]);
					if (erase == -1) ncl_cutter_box_add(box,&ptb[i]);
				}
				if (erase != -1) gpolymarker3(len,ptb);
				if (len > 200) uu_free(ptb);
				break;
/*
...........Shaded triangles
*/
			case UG_SHADEAREAOP: 
				len=(*(UG_shadearea *)cmd).len;
				type=(*(UG_shadearea *)cmd).type;
				pta = (*(UG_shadearea *)cmd).pts;
				ptc = (Gwpoint3 *)uu_malloc(len*sizeof(Gwpoint3)*2);
				if (ptc == UU_NULL) break;
				for (i=0;i<len;i++)
				{
					ptpt[0] = pta[i].x;
					ptpt[1] = pta[i].y;
					ptpt[2] = pta[i].z;
					vcn[0] = pta[len+i].x;
					vcn[1] = pta[len+i].y;
					vcn[2] = pta[len+i].z;
					if (pta[i].x > zhgt[0]) zhgt[0] = pta[i].x;
					if (pta[i].y > zhgt[1]) zhgt[1] = pta[i].y;
					if (pta[i].z > zhgt[2]) zhgt[2] = pta[i].z;
					if (ifl == 1)
					{
						um_cctmtf(ptpt,mtf,ptpt);
						um_vctmtf(vcn,mtf,vcn);
					}
					if (isblade)
					{
						ncl_blade_orient(ptpt,vfd,vr,tax,ptpt);
						ncl_blade_orient(vcn,vfd,vr,tax,vcn);
					}
					um_vcplvc(ptpt,cpt,&ptc[i]);
					um_vctovc(vcn,&ptc[len+i]);
					if (erase == -1) ncl_cutter_box_add(box,&ptc[i]);
				}
				if (erase != -1) 
				{
					save_lucency = ug_get_lucency();
					ug_set_lucency(100);
					gshadearea(len,ptc,&ptc[len],type);
					ug_set_lucency(save_lucency);
				}
				uu_free(ptc);
				break;
/*
...........Transformation matrices
*/
			case UG_SNTRANOP: 
				ug_sntran((*(UG_sntranop *)cmd).xform); 
				break;
			case UG_MTRANOP:
				ug_smodxf((*(UG_mtranop *)cmd).xf,(*(UG_mtranop *)cmd).type); 
				break;
			case UG_LMTRANOP:
				ug_slmodxf((*(UG_lmtranop *)cmd).xf, (*(UG_lmtranop *)cmd).type); 
				break;
/*
...........Attribute settings
*/
			case UG_DFATSOP: ug_dfats(); break;
			case UG_LSTYLOP: ug_linetype(&(*(UG_lstylop *)cmd).ls); break;
			case UG_LWIDOP: ug_linewidth((Gscale)(*(UG_lwidop *)cmd).width); break;
			case UG_LNCOLROP:
				if (erase == 0) 
				{
					ug_linecolor((*(UG_lncolrop *)cmd).color); break;
				}
				else if (erase > 0)
				{
					ug_linecolor(ug_segerasecolor);
				}
			case UG_MKCOLROP: 
				if (erase == 0) 
				{
					ug_markcolor((*(UG_mkcolorop *)cmd).color); break;
				}
				else if (erase > 0)
				{
					ug_markcolor(ug_segerasecolor);
				}
			};	/* End Case */
		}		/* End Normal Command */
	}			/* For each command in seg */
/*
.....Restore digs state (xforms)
*/
	if (segptr->xforms & (UG_SEGMODXF)) ug_resstate(&state);
/*
.....Restore GKS state
*/
	ug_gksos.sysstate=systate;		/* restore GKS state */
}
