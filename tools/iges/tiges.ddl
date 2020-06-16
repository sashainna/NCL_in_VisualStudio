/*********************************************************************
**    NAME         :  tiges.ddl
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tiges.ddl , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:42
*********************************************************************/

UNIBASE
	prefix IG_
	suffix _rec
	output tigesddl.h

create table bptr(
	key_id	key,
	int		bptr);

create table prop(
	key_id	key,
	int		prop);

create table pt2(
	key_id	key,
	real		pt[2]);

create table pt3(
	key_id	key,
	real		pt[3]);

create table pt6(
	key_id	key,
	real		pt[3],
	real		vec[3]);

create table igespt(
	key_id   key, 
	real 		pt[3],
	int 		fig_ptr,
	join     bptr,
	join 		prop);

create table igesline(
	key_id	key,
	real 		spt[3],
	real 		ept[3],
	join     bptr,
	join 		prop);

create table igespln(
	key_id	key,
	real 		coef[4],
	key_id   	b_crv,			/* cpp: enhanced PLANE in IGES to NCL502 mods */
	real 		disp_pt[3],		/* cpp: enhanced PLANE in IGES to NCL502 mods */
	real		size_par,		/* cpp: enhanced PLANE in IGES to NCL502 mods */
	join     bptr,
	join 		prop);

create table igesarc(
	key_id	key,
	real 		zt,
	real 		cpt[2],
	real 		spt[2],
	real 		ept[2],
	join     bptr,
	join 		prop);

create table cid(
	key_id 	key,
	key_id 	cid);

create table cid1(
	key_id 	key,
	key_id 	cid);

create table igescomp(
	key_id	key,
	int		num,		/*jkd36*/
	join		cid,
	join     bptr,
	join 		prop);

create table igesgrp(
	key_id	key,
	int      num,
	join		cid,
	join     bptr,
	join 		prop);

create table viewvs(
	key_id	key,
	int      num_views,
	int      num_entities,
	join		cid,
	join		cid1,
	join     bptr,
	join 		prop);

create table plnassoc(
	key_id	key,
	int      tran_num,
	int      num,
	key_id   trans_id,
	join		cid,
	join     bptr,
	join 		prop);

create table poly2d(
	key_id	key,
	int		type,
	int		num,
	real		zt,
	join		pt2,
	join     bptr,
	join 		prop);

create table poly3d(
	key_id	key,
	int		type,
	join		pt3,
	join     bptr,
	join 		prop);

create table poly6d(
	key_id	key,
	int		type,
	join		pt6,
	join     bptr,
	join 		prop);

create table igescon(
	key_id	key,
	real		a,
	real		b,
	real		c,
	real		d,
	real		e,
	real		f,
	real		zt,
	real		spt[2],
	real		ept[2],
	join		bptr,
	join		prop);

create table igestran(
	key_id	key,
	real		trans[12],
	join     bptr,
	join 		prop);

create table t(
	key_id	key,
	real		t);

create table t1(
	key_id	key,
	real		t);

create table t2(
	key_id	key,
	real		t);

create table w(
	key_id	key,
	real		w);

create table coef(
	key_id	key,
	real		cx[4],
	real		cy[4],
	real		cz[4]);

create table rpara(
	key_id	key,
	real		t0,
	real		t1,
	real		norm[3]);

create table rspara(
	key_id	key,
	real		u0,
	real		u1,
	real		v0,
	real		v1);

create table igesplin(
	key_id	key,
	int 		sub_type,
	int 		cont,
	int		ndim,
	int		n_seg,
	join		t,
	join		coef,
	join     bptr,
	join 		prop);

create table igesrspl(
	key_id	key,
	int 		indx,
	int 		degree,
	int 		planar,
	int 		open,
	int 		type,
	int 		period,
	join		t,
	join		w,
	join		pt3,
	join		rpara,
	join     bptr,
	join 		prop);

create table igesrssf(
	key_id	key,
	int 		indx1,
	int 		indx2,
	int 		degree1,
	int 		degree2,
	int 		open1,
	int 		open2,
	int 		type,
	int 		period1,
	int 		period2,
	join		t1,
	join		t2,
	join		w,
	join		pt3,
	join		rspara,
	join     bptr,
	join 		prop);

create table igesrlsf(
	key_id	key,
	key_id	crv1,
	key_id	crv2,
	int 		dirflg,
	int 		devflg,
	join     bptr,
	join 		prop);

create table igesrvsf(
	key_id	key,
	key_id	axis,
	key_id	crv,
	real 		sa,
	real 		ea,
	join     bptr,
	join 		prop);

create table igestbcy(
	key_id	key,
	key_id	crv,
	real 		d_pt[3],
	join     bptr,
	join 		prop);

create table igesofsf(		/* cpp: OFFSET SURFACE IMPLEMENTATION */
	key_id	key,
	real 	of_vec[3],
	real	of_dist,
	key_id	srf,
	join     bptr,
	join 		prop);


create table crvptr(       /* Join to Boundary entity 7/7/99 */
	key_id	key,
	key_id	crvptr,
	int	sense,
	join	cid);

create table igesbndy(		/* Boundary entity JLS 10/30/98 */
	key_id	key,
	int	type,
	int 	pref,
	key_id   srf,
	join crvptr);

create table igescvsf(		/* ijd: curve on surface */
	key_id	key,
	int		crtn,
	key_id	srf,
	key_id  b_ptr,
	key_id	crv,
	int		pref,
	join    bptr,
	join 	prop);

create table igesbdsf(     /*Bounded surface entity JLS 10/30/98 */
	key_id	key,
	int	type,
	key_id	srf,
	join	cid,
	join 	prop);

create table igestrsf(		/* ijd: trimmed surface */
	key_id	key,
	key_id	srf,
	int 	n1,
	int		n2,
	key_id	crv,
	join	cid,
	join    bptr,
	join 	prop);

create table tu(			/*jkd14: p-spline surface */
	key_id		key,
	real		tu);

create table tv(
	key_id		key,
	real		tv);

create table patc(
	key_id		key,
	real		coef);

create table igesplsf(
	key_id		key,
	int			sub_type,
	int			patch_type,
	int			no_u_seg,    /* M */
	int			no_v_seg,    /* N */
	join		tu,          /* M+1 real values */
	join		tv,          /* N+1 real values */
	join		patc,        /* (M+1)*(N+1)*48 values */
	join		bptr,
	join		prop);

create table gnote(
	key_id		key,
	int			str_cnt,
	real			wt,
	real			ht,
	int			fc,
	real			sl,
	real			ang,
	int			mir,
	int			vh,
	real			xyzt[3],
	character	str[96]);

create table igesnote(
	key_id	key,
	int		num_str,
	join		gnote,
	join		bptr,
	join		prop);

create table igeslead(
	key_id	key,
	int		num_seg,
	real		ah,
	real		aw,
	real		zt,
	real		xyh[2],
	join		pt2,
	join		bptr,
	join		prop);

create table igesangd(
	key_id	key,
	key_id	note_ptr,
	key_id	w1_ptr,
	key_id	w2_ptr,
	real		xyt[2],
	real		rad,
	key_id	l1_ptr,
	key_id	l2_ptr,
	join		bptr,
	join		prop);

create table igesdiad(
	key_id	key,
	key_id	note_ptr,
	key_id	l1_ptr,
	key_id	l2_ptr,
	real		xyt[2],
	join		bptr,
	join		prop);

create table igeslind(
	key_id	key,
	key_id	note_ptr,
	key_id	l1_ptr,
	key_id	l2_ptr,
	key_id	w1_ptr,
	key_id	w2_ptr,
	join		bptr,
	join		prop);

create table igesrad(
	key_id	key,
	key_id	note_ptr,
	key_id	l_ptr,
	real		xyt[2],
	join		bptr,
	join		prop);

create table igeslabl(
	key_id	key,
	key_id	note_ptr,
	join		cid,
	join		bptr,
	join		prop);

create table igesgsym(
	key_id	key,
	key_id	note_ptr,
	join		cid,
	join		cid1,
	join		bptr,
	join		prop);

create table igessfd(
	key_id		key,
	int      	depth,
	character	name[80],
	int      	num,
	join			cid,
	join     	bptr,
	join 			prop);

create table igessfi(
	key_id		key,
	key_id     	sfd_ptr,
	real			xyz[3],
	real			scale,
	join     	bptr,
	join 			prop);

create table igesvie(
	key_id		key,
	int         view_id,
	real			scale,
	key_id     	left,
	key_id     	top,
	key_id     	right,
	key_id     	bottom,
	key_id     	back,
	key_id     	front,
	join     	bptr,
	join 			prop);

create table v_tab(
	key_id 	key,
	key_id 	v_key,
	real     x_org,
	real     y_org);

create table igesdrw(
	key_id		key,
	join        v_tab,
	join			cid,
	join     	bptr,
	join 			prop);

create table igesprop(
   key_id     key, 
   int        pptr,
   character  name[80],
   join       bptr,
   join       prop);

create table igesvlst(    /* vertex list entity -06/21/05 */
	key_id	  key,
	int		  n,
	join		  pt3);

create table sp_crv(    /* join to edge list entity -06/21/05 */
   key_id     key,
	key_id     crv,
	key_id     vlist1,
   int        index1,
	key_id     vlist2,
	int        index2);

create table igeselst(    /* edge list entity -06/21/05 */
   key_id     key,
   int        n,
   join       sp_crv);

create table par_sp(   /* join to edge- 06/21/05 */
	key_id	  key,
	int		  isopara,
	key_id	  par_crv);

create table edge(    /* join to loop entity -06/21/05 */
   key_id     key,
   int        type,
	key_id     list,
   int		  index,
	int 		  e_orn,
	int 		  num_par,
	join 		  par_sp);

create table igesloop(    /* loop list -06/21/05 */
	key_id	  key,
   int        n,
   join       edge);

create table igesface(    /* face entity - 06/21/05 */
	key_id 	  key,
	key_id	  srf,
	int		  n,
	int		  out,
	join		  cid);

create table ornface(   /* join to shell entity -06/21/05*/
	key_id	  key,
	key_id	  igesface,
	int 		  f_orn);

create table igeshell(  /* shell entity - 06/21/05 */
	key_id	  key,
   int		  n,
	join 		  ornface,
	join       bptr,
	join       prop);

create table ovshell(   /* join to solid entity -06/21/05*/
   key_id     key,
   key_id     igeshell,
   int        vs_orn);

create table igesolid(  /* solid entity - 06/21/05 */
   key_id     key,
   key_id     igeshell,
   int        s_orn,
   int        num,
	join		  ovshell,
   join       bptr,
   join       prop);

create table igesclr( /* Color entity */
	key_id	  key,
	real 		  red,
	real		  green,
	real       blue,
	character  name[20]);

