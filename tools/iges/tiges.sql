
create table bptr
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	bptr	number(12)
);

create table prop
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	prop	number(12)
);

create table pt2
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	pt_0	number(80,40),
	pt_1	number(80,40)
);

create table pt3
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	pt_0	number(80,40),
	pt_1	number(80,40),
	pt_2	number(80,40)
);

create table pt6
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	pt_0	number(80,40),
	pt_1	number(80,40),
	pt_2	number(80,40),
	vec_0	number(80,40),
	vec_1	number(80,40),
	vec_2	number(80,40)
);

create table igespt
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	pt_0	number(80,40),
	pt_1	number(80,40),
	pt_2	number(80,40),
	fig_ptr	number(12)
);

create table igesline
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	spt_0	number(80,40),
	spt_1	number(80,40),
	spt_2	number(80,40),
	ept_0	number(80,40),
	ept_1	number(80,40),
	ept_2	number(80,40)
);

create table igespln
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	coef_0	number(80,40),
	coef_1	number(80,40),
	coef_2	number(80,40),
	coef_3	number(80,40),
	b_crv_rel	number(8) not null,
	b_crv_key	number(8) not null,
	disp_pt_0	number(80,40),
	disp_pt_1	number(80,40),
	disp_pt_2	number(80,40),
	size_par	number(80,40)
);

create table igesarc
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	zt	number(80,40),
	cpt_0	number(80,40),
	cpt_1	number(80,40),
	spt_0	number(80,40),
	spt_1	number(80,40),
	ept_0	number(80,40),
	ept_1	number(80,40)
);

create table cid
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	cid_rel	number(8) not null,
	cid_key	number(8) not null
);

create table cid1
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	cid_rel	number(8) not null,
	cid_key	number(8) not null
);

create table igescomp
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	num	number(12)
);

create table igesgrp
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	num	number(12)
);

create table viewvs
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	num_views	number(12),
	num_entities	number(12)
);

create table plnassoc
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	tran_num	number(12),
	num	number(12),
	trans_id_rel	number(8) not null,
	trans_id_key	number(8) not null
);

create table poly2d
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	type	number(12),
	num	number(12),
	zt	number(80,40)
);

create table poly3d
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	type	number(12)
);

create table poly6d
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	type	number(12)
);

create table igescon
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	a	number(80,40),
	b	number(80,40),
	c	number(80,40),
	d	number(80,40),
	e	number(80,40),
	f	number(80,40),
	zt	number(80,40),
	spt_0	number(80,40),
	spt_1	number(80,40),
	ept_0	number(80,40),
	ept_1	number(80,40)
);

create table igestran
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	trans_0	number(80,40),
	trans_1	number(80,40),
	trans_2	number(80,40),
	trans_3	number(80,40),
	trans_4	number(80,40),
	trans_5	number(80,40),
	trans_6	number(80,40),
	trans_7	number(80,40),
	trans_8	number(80,40),
	trans_9	number(80,40),
	trans_10	number(80,40),
	trans_11	number(80,40)
);

create table t
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	t	number(80,40)
);

create table t1
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	t	number(80,40)
);

create table t2
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	t	number(80,40)
);

create table w
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	w	number(80,40)
);

create table coef
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	cx_0	number(80,40),
	cx_1	number(80,40),
	cx_2	number(80,40),
	cx_3	number(80,40),
	cy_0	number(80,40),
	cy_1	number(80,40),
	cy_2	number(80,40),
	cy_3	number(80,40),
	cz_0	number(80,40),
	cz_1	number(80,40),
	cz_2	number(80,40),
	cz_3	number(80,40)
);

create table rpara
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	t0	number(80,40),
	t1	number(80,40),
	norm_0	number(80,40),
	norm_1	number(80,40),
	norm_2	number(80,40)
);

create table rspara
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	u0	number(80,40),
	u1	number(80,40),
	v0	number(80,40),
	v1	number(80,40)
);

create table igesplin
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	sub_type	number(12),
	cont	number(12),
	ndim	number(12),
	n_seg	number(12)
);

create table igesrspl
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	indx	number(12),
	degree	number(12),
	planar	number(12),
	open	number(12),
	type	number(12),
	period	number(12)
);

create table igesrssf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	indx1	number(12),
	indx2	number(12),
	degree1	number(12),
	degree2	number(12),
	open1	number(12),
	open2	number(12),
	type	number(12),
	period1	number(12),
	period2	number(12)
);

create table igesrlsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	crv1_rel	number(8) not null,
	crv1_key	number(8) not null,
	crv2_rel	number(8) not null,
	crv2_key	number(8) not null,
	dirflg	number(12),
	devflg	number(12)
);

create table igesrvsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	axis_rel	number(8) not null,
	axis_key	number(8) not null,
	crv_rel	number(8) not null,
	crv_key	number(8) not null,
	sa	number(80,40),
	ea	number(80,40)
);

create table igestbcy
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	crv_rel	number(8) not null,
	crv_key	number(8) not null,
	d_pt_0	number(80,40),
	d_pt_1	number(80,40),
	d_pt_2	number(80,40)
);

create table igesofsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	of_vec_0	number(80,40),
	of_vec_1	number(80,40),
	of_vec_2	number(80,40),
	of_dist	number(80,40),
	srf_rel	number(8) not null,
	srf_key	number(8) not null
);

create table crvptr
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	crvptr_rel	number(8) not null,
	crvptr_key	number(8) not null,
	sense	number(12)
);

create table igesbndy
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	type	number(12),
	pref	number(12),
	srf_rel	number(8) not null,
	srf_key	number(8) not null
);

create table igescvsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	crtn	number(12),
	srf_rel	number(8) not null,
	srf_key	number(8) not null,
	b_ptr_rel	number(8) not null,
	b_ptr_key	number(8) not null,
	crv_rel	number(8) not null,
	crv_key	number(8) not null,
	pref	number(12)
);

create table igesbdsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	type	number(12),
	srf_rel	number(8) not null,
	srf_key	number(8) not null
);

create table igestrsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	srf_rel	number(8) not null,
	srf_key	number(8) not null,
	n1	number(12),
	n2	number(12),
	crv_rel	number(8) not null,
	crv_key	number(8) not null
);

create table tu
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	tu	number(80,40)
);

create table tv
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	tv	number(80,40)
);

create table patc
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	coef	number(80,40)
);

create table igesplsf
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	sub_type	number(12),
	patch_type	number(12),
	no_u_seg	number(12),
	no_v_seg	number(12)
);

create table gnote
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	str_cnt	number(12),
	wt	number(80,40),
	ht	number(80,40),
	fc	number(12),
	sl	number(80,40),
	ang	number(80,40),
	mir	number(12),
	vh	number(12),
	xyzt_0	number(80,40),
	xyzt_1	number(80,40),
	xyzt_2	number(80,40),
	str	char(96)
);

create table igesnote
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	num_str	number(12)
);

create table igeslead
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	num_seg	number(12),
	ah	number(80,40),
	aw	number(80,40),
	zt	number(80,40),
	xyh_0	number(80,40),
	xyh_1	number(80,40)
);

create table igesangd
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	note_ptr_rel	number(8) not null,
	note_ptr_key	number(8) not null,
	w1_ptr_rel	number(8) not null,
	w1_ptr_key	number(8) not null,
	w2_ptr_rel	number(8) not null,
	w2_ptr_key	number(8) not null,
	xyt_0	number(80,40),
	xyt_1	number(80,40),
	rad	number(80,40),
	l1_ptr_rel	number(8) not null,
	l1_ptr_key	number(8) not null,
	l2_ptr_rel	number(8) not null,
	l2_ptr_key	number(8) not null
);

create table igesdiad
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	note_ptr_rel	number(8) not null,
	note_ptr_key	number(8) not null,
	l1_ptr_rel	number(8) not null,
	l1_ptr_key	number(8) not null,
	l2_ptr_rel	number(8) not null,
	l2_ptr_key	number(8) not null,
	xyt_0	number(80,40),
	xyt_1	number(80,40)
);

create table igeslind
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	note_ptr_rel	number(8) not null,
	note_ptr_key	number(8) not null,
	l1_ptr_rel	number(8) not null,
	l1_ptr_key	number(8) not null,
	l2_ptr_rel	number(8) not null,
	l2_ptr_key	number(8) not null,
	w1_ptr_rel	number(8) not null,
	w1_ptr_key	number(8) not null,
	w2_ptr_rel	number(8) not null,
	w2_ptr_key	number(8) not null
);

create table igesrad
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	note_ptr_rel	number(8) not null,
	note_ptr_key	number(8) not null,
	l_ptr_rel	number(8) not null,
	l_ptr_key	number(8) not null,
	xyt_0	number(80,40),
	xyt_1	number(80,40)
);

create table igeslabl
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	note_ptr_rel	number(8) not null,
	note_ptr_key	number(8) not null
);

create table igesgsym
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	note_ptr_rel	number(8) not null,
	note_ptr_key	number(8) not null
);

create table igessfd
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	depth	number(12),
	name	char(80),
	num	number(12)
);

create table igessfi
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	sfd_ptr_rel	number(8) not null,
	sfd_ptr_key	number(8) not null,
	xyz_0	number(80,40),
	xyz_1	number(80,40),
	xyz_2	number(80,40),
	scale	number(80,40)
);

create table igesvie
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	view_id	number(12),
	scale	number(80,40),
	left_rel	number(8) not null,
	left_key	number(8) not null,
	top_rel	number(8) not null,
	top_key	number(8) not null,
	right_rel	number(8) not null,
	right_key	number(8) not null,
	bottom_rel	number(8) not null,
	bottom_key	number(8) not null,
	back_rel	number(8) not null,
	back_key	number(8) not null,
	front_rel	number(8) not null,
	front_key	number(8) not null
);

create table v_tab
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	v_key_rel	number(8) not null,
	v_key_key	number(8) not null,
	x_org	number(80,40),
	y_org	number(80,40)
);

create table igesdrw
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null
);

create table igesprop
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	pptr	number(12),
	name	char(80)
);

create table igesvlst
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	n	number(12)
);

create table sp_crv
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	crv_rel	number(8) not null,
	crv_key	number(8) not null,
	vlist1_rel	number(8) not null,
	vlist1_key	number(8) not null,
	index1	number(12),
	vlist2_rel	number(8) not null,
	vlist2_key	number(8) not null,
	index2	number(12)
);

create table igeselst
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	n	number(12)
);

create table par_sp
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	isopara	number(12),
	par_crv_rel	number(8) not null,
	par_crv_key	number(8) not null
);

create table edge
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	type	number(12),
	list_rel	number(8) not null,
	list_key	number(8) not null,
	index	number(12),
	e_orn	number(12),
	num_par	number(12)
);

create table igesloop
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	n	number(12)
);

create table igesface
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	srf_rel	number(8) not null,
	srf_key	number(8) not null,
	n	number(12),
	out	number(12)
);

create table ornface
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	igesface_rel	number(8) not null,
	igesface_key	number(8) not null,
	f_orn	number(12)
);

create table igeshell
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	n	number(12)
);

create table ovshell
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	igeshell_rel	number(8) not null,
	igeshell_key	number(8) not null,
	vs_orn	number(12)
);

create table igesolid
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	igeshell_rel	number(8) not null,
	igeshell_key	number(8) not null,
	s_orn	number(12),
	num	number(12)
);

create table igesclr
(
	part_id	number(8),
	key_rel	number(8) not null,
	key_key	number(8) not null,
	red	number(80,40),
	green	number(80,40),
	blue	number(80,40),
	name	char(20)
);
insert into geometry values(0, 'bptr', -1, 0);
insert into geometry values(0, 'prop', -1, 0);
insert into geometry values(0, 'pt2', -1, 0);
insert into geometry values(0, 'pt3', -1, 0);
insert into geometry values(0, 'pt6', -1, 0);
insert into geometry values(0, 'igespt', 0, 2);
insert into geometry values(0, 'igesline', 1, 2);
insert into geometry values(0, 'igespln', 2, 2);
insert into geometry values(0, 'igesarc', 3, 2);
insert into geometry values(0, 'cid', -1, 0);
insert into geometry values(0, 'cid1', -1, 0);
insert into geometry values(0, 'igescomp', 4, 3);
insert into geometry values(0, 'igesgrp', 5, 3);
insert into geometry values(0, 'viewvs', 6, 4);
insert into geometry values(0, 'plnassoc', 7, 3);
insert into geometry values(0, 'poly2d', 8, 3);
insert into geometry values(0, 'poly3d', 9, 3);
insert into geometry values(0, 'poly6d', 10, 3);
insert into geometry values(0, 'igescon', 11, 2);
insert into geometry values(0, 'igestran', 12, 2);
insert into geometry values(0, 't', -1, 0);
insert into geometry values(0, 't1', -1, 0);
insert into geometry values(0, 't2', -1, 0);
insert into geometry values(0, 'w', -1, 0);
insert into geometry values(0, 'coef', -1, 0);
insert into geometry values(0, 'rpara', -1, 0);
insert into geometry values(0, 'rspara', -1, 0);
insert into geometry values(0, 'igesplin', 13, 4);
insert into geometry values(0, 'igesrspl', 14, 6);
insert into geometry values(0, 'igesrssf', 15, 7);
insert into geometry values(0, 'igesrlsf', 16, 2);
insert into geometry values(0, 'igesrvsf', 17, 2);
insert into geometry values(0, 'igestbcy', 18, 2);
insert into geometry values(0, 'igesofsf', 19, 2);
insert into geometry values(0, 'crvptr', -1, 1);
insert into geometry values(0, 'igesbndy', 20, 1);
insert into geometry values(0, 'igescvsf', 21, 2);
insert into geometry values(0, 'igesbdsf', 22, 2);
insert into geometry values(0, 'igestrsf', 23, 3);
insert into geometry values(0, 'tu', -1, 0);
insert into geometry values(0, 'tv', -1, 0);
insert into geometry values(0, 'patc', -1, 0);
insert into geometry values(0, 'igesplsf', 24, 5);
insert into geometry values(0, 'gnote', -1, 0);
insert into geometry values(0, 'igesnote', 25, 3);
insert into geometry values(0, 'igeslead', 26, 3);
insert into geometry values(0, 'igesangd', 27, 2);
insert into geometry values(0, 'igesdiad', 28, 2);
insert into geometry values(0, 'igeslind', 29, 2);
insert into geometry values(0, 'igesrad', 30, 2);
insert into geometry values(0, 'igeslabl', 31, 3);
insert into geometry values(0, 'igesgsym', 32, 4);
insert into geometry values(0, 'igessfd', 33, 3);
insert into geometry values(0, 'igessfi', 34, 2);
insert into geometry values(0, 'igesvie', 35, 2);
insert into geometry values(0, 'v_tab', -1, 0);
insert into geometry values(0, 'igesdrw', 36, 4);
insert into geometry values(0, 'igesprop', 37, 2);
insert into geometry values(0, 'igesvlst', 38, 1);
insert into geometry values(0, 'sp_crv', -1, 0);
insert into geometry values(0, 'igeselst', 39, 1);
insert into geometry values(0, 'par_sp', -1, 0);
insert into geometry values(0, 'edge', -1, 1);
insert into geometry values(0, 'igesloop', 40, 1);
insert into geometry values(0, 'igesface', 41, 1);
insert into geometry values(0, 'ornface', -1, 0);
insert into geometry values(0, 'igeshell', 42, 3);
insert into geometry values(0, 'ovshell', -1, 0);
insert into geometry values(0, 'igesolid', 43, 3);
insert into geometry values(0, 'igesclr', 44, 0);
