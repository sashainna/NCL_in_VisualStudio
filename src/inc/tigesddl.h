/*********************************************************************
**    NAME         :  tigesddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 15:10:27  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct IG_pt6_rec
{
	UU_REAL	pt[3];
	UU_REAL	vec[3];
};

struct IG_igespt_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	pt[3];
	int	fig_ptr;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESPT_BUFSZ];
};

struct IG_igesline_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	spt[3];
	UU_REAL	ept[3];
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESLINE_BUFSZ];
};

struct IG_igespln_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	coef[4];
	UU_KEY_ID	b_crv;
	UU_REAL	disp_pt[3];
	UU_REAL	size_par;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESPLN_BUFSZ];
};

struct IG_igesarc_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	zt;
	UU_REAL	cpt[2];
	UU_REAL	spt[2];
	UU_REAL	ept[2];
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESARC_BUFSZ];
};

struct IG_igescomp_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	num;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESCOMP_BUFSZ];
};

struct IG_igesgrp_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	num;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESGRP_BUFSZ];
};

struct IG_viewvs_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	num_views;
	int	num_entities;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	UU_KEY_ID	*cid1;
	int	no_cid1;
	int	pad_cid1;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_VIEWVS_BUFSZ];
};

struct IG_plnassoc_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	tran_num;
	int	num;
	UU_KEY_ID	trans_id;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_PLNASSOC_BUFSZ];
};

struct IG_poly2d_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	type;
	int	num;
	UU_REAL	zt;
	UU_REAL	*pt2;
	int	no_pt2;
	int	pad_pt2;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_POLY2D_BUFSZ];
};

struct IG_poly3d_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	type;
	UU_REAL	*pt3;
	int	no_pt3;
	int	pad_pt3;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_POLY3D_BUFSZ];
};

struct IG_poly6d_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	type;
	struct IG_pt6_rec	*pt6;
	int	no_pt6;
	int	pad_pt6;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_POLY6D_BUFSZ];
};

struct IG_igescon_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	a;
	UU_REAL	b;
	UU_REAL	c;
	UU_REAL	d;
	UU_REAL	e;
	UU_REAL	f;
	UU_REAL	zt;
	UU_REAL	spt[2];
	UU_REAL	ept[2];
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESCON_BUFSZ];
};

struct IG_igestran_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	trans[12];
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESTRAN_BUFSZ];
};

struct IG_coef_rec
{
	UU_REAL	cx[4];
	UU_REAL	cy[4];
	UU_REAL	cz[4];
};

struct IG_rpara_rec
{
	UU_REAL	t0;
	UU_REAL	t1;
	UU_REAL	norm[3];
};

struct IG_rspara_rec
{
	UU_REAL	u0;
	UU_REAL	u1;
	UU_REAL	v0;
	UU_REAL	v1;
};

struct IG_igesplin_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	sub_type;
	int	cont;
	int	ndim;
	int	n_seg;
	UU_REAL	*t;
	int	no_t;
	int	pad_t;
	struct IG_coef_rec	*coef;
	int	no_coef;
	int	pad_coef;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESPLIN_BUFSZ];
};

struct IG_igesrspl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	indx;
	int	degree;
	int	planar;
	int	open;
	int	type;
	int	period;
	UU_REAL	*t;
	int	no_t;
	int	pad_t;
	UU_REAL	*w;
	int	no_w;
	int	pad_w;
	UU_REAL	*pt3;
	int	no_pt3;
	int	pad_pt3;
	struct IG_rpara_rec	*rpara;
	int	no_rpara;
	int	pad_rpara;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESRSPL_BUFSZ];
};

struct IG_igesrssf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	indx1;
	int	indx2;
	int	degree1;
	int	degree2;
	int	open1;
	int	open2;
	int	type;
	int	period1;
	int	period2;
	UU_REAL	*t1;
	int	no_t1;
	int	pad_t1;
	UU_REAL	*t2;
	int	no_t2;
	int	pad_t2;
	UU_REAL	*w;
	int	no_w;
	int	pad_w;
	UU_REAL	*pt3;
	int	no_pt3;
	int	pad_pt3;
	struct IG_rspara_rec	*rspara;
	int	no_rspara;
	int	pad_rspara;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESRSSF_BUFSZ];
};

struct IG_igesrlsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	crv1;
	UU_KEY_ID	crv2;
	int	dirflg;
	int	devflg;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESRLSF_BUFSZ];
};

struct IG_igesrvsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	axis;
	UU_KEY_ID	crv;
	UU_REAL	sa;
	UU_REAL	ea;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESRVSF_BUFSZ];
};

struct IG_igestbcy_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	crv;
	UU_REAL	d_pt[3];
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESTBCY_BUFSZ];
};

struct IG_igesofsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	of_vec[3];
	UU_REAL	of_dist;
	UU_KEY_ID	srf;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESOFSF_BUFSZ];
};

struct IG_crvptr_rec
{
	UU_KEY_ID	crvptr;
	int	sense;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	char	varlistbuf[IG_CRVPTR_BUFSZ];
};

struct IG_igesbndy_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	type;
	int	pref;
	UU_KEY_ID	srf;
	struct IG_crvptr_rec	*crvptr;
	int	no_crvptr;
	int	pad_crvptr;
	char	varlistbuf[IG_IGESBNDY_BUFSZ];
};

struct IG_igescvsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	crtn;
	UU_KEY_ID	srf;
	UU_KEY_ID	b_ptr;
	UU_KEY_ID	crv;
	int	pref;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESCVSF_BUFSZ];
};

struct IG_igesbdsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	type;
	UU_KEY_ID	srf;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESBDSF_BUFSZ];
};

struct IG_igestrsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	srf;
	int	n1;
	int	n2;
	UU_KEY_ID	crv;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESTRSF_BUFSZ];
};

struct IG_igesplsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	sub_type;
	int	patch_type;
	int	no_u_seg;
	int	no_v_seg;
	UU_REAL	*tu;
	int	no_tu;
	int	pad_tu;
	UU_REAL	*tv;
	int	no_tv;
	int	pad_tv;
	UU_REAL	*patc;
	int	no_patc;
	int	pad_patc;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESPLSF_BUFSZ];
};

struct IG_gnote_rec
{
	int	str_cnt;
	UU_REAL	wt;
	UU_REAL	ht;
	int	fc;
	UU_REAL	sl;
	UU_REAL	ang;
	int	mir;
	int	vh;
	UU_REAL	xyzt[3];
	char	str[96];
};

struct IG_igesnote_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	num_str;
	struct IG_gnote_rec	*gnote;
	int	no_gnote;
	int	pad_gnote;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESNOTE_BUFSZ];
};

struct IG_igeslead_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	num_seg;
	UU_REAL	ah;
	UU_REAL	aw;
	UU_REAL	zt;
	UU_REAL	xyh[2];
	UU_REAL	*pt2;
	int	no_pt2;
	int	pad_pt2;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESLEAD_BUFSZ];
};

struct IG_igesangd_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	note_ptr;
	UU_KEY_ID	w1_ptr;
	UU_KEY_ID	w2_ptr;
	UU_REAL	xyt[2];
	UU_REAL	rad;
	UU_KEY_ID	l1_ptr;
	UU_KEY_ID	l2_ptr;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESANGD_BUFSZ];
};

struct IG_igesdiad_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	note_ptr;
	UU_KEY_ID	l1_ptr;
	UU_KEY_ID	l2_ptr;
	UU_REAL	xyt[2];
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESDIAD_BUFSZ];
};

struct IG_igeslind_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	note_ptr;
	UU_KEY_ID	l1_ptr;
	UU_KEY_ID	l2_ptr;
	UU_KEY_ID	w1_ptr;
	UU_KEY_ID	w2_ptr;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESLIND_BUFSZ];
};

struct IG_igesrad_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	note_ptr;
	UU_KEY_ID	l_ptr;
	UU_REAL	xyt[2];
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESRAD_BUFSZ];
};

struct IG_igeslabl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	note_ptr;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESLABL_BUFSZ];
};

struct IG_igesgsym_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	note_ptr;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	UU_KEY_ID	*cid1;
	int	no_cid1;
	int	pad_cid1;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESGSYM_BUFSZ];
};

struct IG_igessfd_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	depth;
	char	name[80];
	int	num;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESSFD_BUFSZ];
};

struct IG_igessfi_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	sfd_ptr;
	UU_REAL	xyz[3];
	UU_REAL	scale;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESSFI_BUFSZ];
};

struct IG_igesvie_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	view_id;
	UU_REAL	scale;
	UU_KEY_ID	left;
	UU_KEY_ID	top;
	UU_KEY_ID	right;
	UU_KEY_ID	bottom;
	UU_KEY_ID	back;
	UU_KEY_ID	front;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESVIE_BUFSZ];
};

struct IG_v_tab_rec
{
	UU_KEY_ID	v_key;
	UU_REAL	x_org;
	UU_REAL	y_org;
};

struct IG_igesdrw_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	struct IG_v_tab_rec	*v_tab;
	int	no_v_tab;
	int	pad_v_tab;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESDRW_BUFSZ];
};

struct IG_igesprop_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	pptr;
	char	name[80];
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESPROP_BUFSZ];
};

struct IG_igesvlst_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	n;
	UU_REAL	*pt3;
	int	no_pt3;
	int	pad_pt3;
	char	varlistbuf[IG_IGESVLST_BUFSZ];
};

struct IG_sp_crv_rec
{
	UU_KEY_ID	crv;
	UU_KEY_ID	vlist1;
	int	index1;
	UU_KEY_ID	vlist2;
	int	index2;
};

struct IG_igeselst_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	n;
	struct IG_sp_crv_rec	*sp_crv;
	int	no_sp_crv;
	int	pad_sp_crv;
	char	varlistbuf[IG_IGESELST_BUFSZ];
};

struct IG_par_sp_rec
{
	int	isopara;
	UU_KEY_ID	par_crv;
};

struct IG_edge_rec
{
	int	type;
	UU_KEY_ID	list;
	int	index;
	int	e_orn;
	int	num_par;
	struct IG_par_sp_rec	*par_sp;
	int	no_par_sp;
	int	pad_par_sp;
	char	varlistbuf[IG_EDGE_BUFSZ];
};

struct IG_igesloop_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	n;
	struct IG_edge_rec	*edge;
	int	no_edge;
	int	pad_edge;
	char	varlistbuf[IG_IGESLOOP_BUFSZ];
};

struct IG_igesface_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	srf;
	int	n;
	int	out;
	UU_KEY_ID	*cid;
	int	no_cid;
	int	pad_cid;
	char	varlistbuf[IG_IGESFACE_BUFSZ];
};

struct IG_ornface_rec
{
	UU_KEY_ID	igesface;
	int	f_orn;
};

struct IG_igeshell_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	n;
	struct IG_ornface_rec	*ornface;
	int	no_ornface;
	int	pad_ornface;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESHELL_BUFSZ];
};

struct IG_ovshell_rec
{
	UU_KEY_ID	igeshell;
	int	vs_orn;
};

struct IG_igesolid_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	igeshell;
	int	s_orn;
	int	num;
	struct IG_ovshell_rec	*ovshell;
	int	no_ovshell;
	int	pad_ovshell;
	int	*bptr;
	int	no_bptr;
	int	pad_bptr;
	int	*prop;
	int	no_prop;
	int	pad_prop;
	char	varlistbuf[IG_IGESOLID_BUFSZ];
};

struct IG_igesclr_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	red;
	UU_REAL	green;
	UU_REAL	blue;
	char	name[20];
};
