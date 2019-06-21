/***************************************************************************
**    NAME         : isysddl.ddl
**       CONTAINS:
**
**    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**    NOTE: The module unibase/redatdct.c must be recompiled
**    to make changed here effective.
**    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**
**    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**    !                 I M P O R T A N T                   !                   
**    ! NOTE: In the function 'ur_reset_lists' (resp04c.c)  !
**    !       there are hardcoded pointers to the 'displst' !
**    !       and 'tesslst' variable lists for all surface  !
**    !       and shape structures.  If these lists change  !
**    !       locations within any of these structures,     !
**    !       then you MUST also change this routine.       !
**    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       isysddl.ddl , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:04
***************************************************************************/

/***************************************************************************
**
**								CURVES
**
***************************************************************************/

output   mcrvddl.h

UNIBASE

	prefix UM_
	suffix _rec

/* -- display list -- */

	create table displst
	(
		key_id		key,
		real			dpt[3]
	);

/* -- tesselation list -- */

	create table tesslst
	(
		key_id		key,
		real			tpt
	);

/* -- Surface limit box -- */

	create table boxlst
	(
		key_id      key,
		real        bx
	);

/* -- Surface xyz boundary -- */

	create table xyzbylst
	(
		key_id      key,
		real        xyzbndy
	);

/* -- Surface uv boundary -- */

	create table uvbylst
	(
		key_id      key,
		real        uvbndy
	);

/* -- Surface uv limit box -- */

	create table uvboxlst
	(
		key_id      key,
		real        uvbx
	);

/*	-- point -- */

	create table point
	(
		key_id		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		int			markertype,			/* type of DIGGS marker to display */
		logical		snap_node,			/* TRUE iff the point is a snap node */
		real			pt[3],				/* coordinates of point */
		join			displst				/* display list */
	);

/*	-- line -- */

	create table line
	(
		key_id 		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		real 			spt[3],				/* start point */
		real 			ept[3],				/* end point */
		join			displst				/* display list */
	);

/*	-- circle and circular arc -- */

	create table circle
	(
		key_id 		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		real 			radius,				/* radius */
		real 			dang,					/* delta angle */
		real 			center[3],			/* center point */
		real 			svec[3],				/* unit vector to start point */
		real 			nvec[3],				/* unit normal */
		join			displst				/* display list */
	);

/* general conic curve */

	create table  conic 
	(
		key_id		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		int			type,					/* type of conic section			*/
		real			invariants[2],		/* specific to type of conic		*/
		real			tfmat[4][3],		/* defn. space to model space		*/
		real			t0,					/* endpoints parameterized			*/
		real			t1,
		join			displst				/* display list */
	);

/* record structure for compcrv var list */

	create table cid 
	(
		key_id 		key,
		key_id 		crvid,				/* key id of this element */
		logical 		reverse,				/* reverse direction of component */
		real 			endparam				/* parameter value at end of this component */
	);

/* composite curve */

	create table  compcrv 
	(
		key_id 		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		int			closdinu, 			/* =1 iff closed in u */
		real			arclen,				/* arc length */
		logical 		planar,				/* t = planar; f = non-planar */
		logical 		open,					/* t = open; f = closed */
		int 			continuity,			/* 0 = C0 (piecewise)
													1 = C1 (tangent)
													2 = C2 (curvature) */
		int 			fcolor,				/* fill color (replaces 'n')	*/
		real		t0,						/* starting u parameter (trimming) */
		real		t1,						/* ending u parameter (trimming) */
		int				addflg,				/* extension lines added flag
												0 = none added
												1 = line added on u = 0 end
												2 = line added on u = 1 end
												3 = line added on both ends */
      join 			cid,					/* composite curve elements */
		join			displst				/* display list */
	);

/*	-- definition for triplets -- */

	create table pt
	(
		key_id 		key,
		real	 		pt[3]					/* coordinate */
	);

	create table wt
	(
		key_id 		key,
		real	 		wt						/* weighting factor */
	);

	create table t
	(
		key_id 		key,
		real	 		t						/* knot vector */
	);

/* bspline curve */

	create table  bsplcrv 
	(
		key_id 		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		logical 		inverted,			/* t = inverted bspline; f = not inverted */
		logical 		planar,				/* t = planar; f = non-planar */
		logical 		open,					/* t = open curve; f = closed curve */
		int	 		k,						/* order of spline basis max = 3 */
		int 			n,						/* number of spans */
												/* note, n+k-1=length of coeff list */
		real			t0,					/* starting parameter */
		real			t1,					/* ending parameter */
      join			pt,					/* bspline coefficent variable list */
		join			displst				/* display list */
	);

/* rational bspline curve */

   create table  rbsplcrv 
   (
      key_id      key,
      character   label[64],        /* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
      int         subscr,           /* subscript */
      logical     planar,           /* t = planar; f = non-planar */
      logical     open,             /* t = open; f = closed */
      int         closdinu,         /* =1 iff closed in u */
      int         k,                /* order of bspline basis */
      int         n,                /* number of spans */
      real        t0,               /* starting parameter */
      real        t1,               /* ending parameter */
      join        t,                /* knot vector list */
      join        pt,               /* control polygon coefficients list */
      join        wt,               /* control polygon weight values list */
		join			displst				/* display list */
   );

/* uv rbspline curve on surface */

   create table  uvcvonsf 
   (
      key_id      key,
      character   label[64],        /* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
      int         subscr,           /* subscript */
		key_ref     bskey,            /* base surface key */
		int         dummy,				/* byte alignment dummy*/
      logical     planar,           /* t = planar; f = non-planar */
      logical     open,             /* t = open; f = closed */
      int         closdinu,         /* =1 iff closed in u */
      int         k,                /* order of bspline basis */
      int         n,                /* number of spans */
      real        t0,               /* starting parameter */
      real        t1,               /* ending parameter */
      join        t,                /* knot vector list */
      join        pt,               /* control polygon coefficients list */
      join        wt,               /* control polygon weight values list */
		join			displst				/* display list */
   );

/* -- AG curve */

	create table agcrv
	(
		key_id		key,
		character	label[8],			/* label */
		int			subscr,				/* subscript */
		int			crvaddr,				/* address of curve in dynamic memory*/
		int			closdinu 			/* =1 iff closed in u */
	);

/*	filled polygon	*/

	create table poly
	(
		key_id		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		int			fcolor,				/* fill color */
		int			numvtx,				/* number of valid points in vertex array	*/
		real			vertex[200][3],	/* note: don't repeat startpoint	*/
		join			displst				/* display list */
	);

/*	polyline	*/

	create table polyline
	(
		key_id		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
      int         dummy,            /* dummy to comp. pad at the struct end */
		join			pt,					/* coordinates of polyline */
		join			displst				/* display list */
	);


/**********************************************************************/
/*                                                                    */
/*                          SURFACES											 */
/*																						    */
/**********************************************************************/

output   msrfddl.h

UNIBASE

	prefix UM_
	suffix _rec

	create table surfattr
	(
		key_id key,
		int	use_count,
		int	color,					/* current color */
		int	layer,					/* current layer */
		int	pen,						/* current pen type */
		int	line_style,				/* current line style */
		real  line_weight,			/* current line weight */
		real	line_width,				/* current line width */
		int	displayable,			/* current displayable */
		logical	selectable,			/* current selectable */
		int	label_on,				/* current label display off/on/leaderline*/
      int	material,   	      /* index of material properties */
      int	numupaths,  	      /* number of u paths to draw */
      int	numvpaths,  	      /* number of v paths to draw */
      int	ptsperucrv, 	      /* number of points per u path */
      int	ptspervcrv, 	      /* number of points per v path */
      int   ecolor,              /* edge color */
      logical	shaded,           /* shaded flag UU_TRUE = shaded */
      int	lucency     	      /* translucency value */
	);

   create table tu
   (
      key_id      key,
      real        t                 /* knot vector */
   );

   create table tv
   (
      key_id      key,
      real        t                 /* knot vector */
   );

/* uv curve on surface associations*/

   create table  sskey 
   (
      key_id      key,
      key_id      ssid              /* SS key on SF */
   );

/* rational bspline surface */

	create table  rbsplsrf 
	(
		key_id      key,
		character   label[64],        /* label */
		real        labloc[3],        /* label display location */
		real        ldrloc[3],        /* default label display location */
		int         subscr,           /* subscript */
		int         rldnu,            /* =1 iff ruled in u */
		int         swapuv,           /* swap and/or reverse u,v flag */
		logical     rev_normal,       /* UU_TRUE => reverse sense of normal */
		int         closdinu,         /* =1 iff closed in u */
		int         closdinv,         /* =1 iff closed in v */
		float       offdist,          /* offset distance */
		int         ku,               /* order of bspline basis */
		int         kv,               /* order of bspline basis */
		int         nu,               /* number of spans */
		int         nv,               /* number of spans */
		int         primitive,        /* special primitives */
		real        prim_param[16],   /* parameters defining the primitives*/
		join        tu,               /* knot vector list */
		join        tv,               /* knot vector list */
		join        pt,               /* control polygon coefficients list */
		join        wt,               /* control polygon weight values list */
		join        sskey,            /* cv on sf key list */
		join        displst,          /* display list */
		join        tesslst,          /* tesselation list */
		join        boxlst,           /* surface limit box */
		join        xyzbylst          /* surface xyz boundary curve */
	);

/* -- AG surface -- */

	create table agsrf
	(
		key_id		key,
		character	label[8],			/* label */
		int			subscr,				/* subscript */
		int			material,			/* index of material properties */
		int	 		numupaths,			/* number of u paths to draw */
		int	 		numvpaths,			/* number of v paths to draw */
		int	 		ptsperucrv,			/* number of points per u path */
		int	 		ptspervcrv,			/* number of points per v path */
		int	 		rldnu,				/* =1 iff ruled in u */
		int	 		rldnv,				/* =1 iff ruled in v */
		logical		rev_normal,			/* UU_TRUE => reverse sense of normal */
		int			srfaddr,				/* address of surface in dynamic memory */
	  int	 	  closdinu,			/* =1 iff closed in u */
	  int	 	  closdinv 			/* =1 iff closed in v */
	);


/**********************************************************************/
/*																							 */
/*        					Solids        											 */
/*										   												 */
/**********************************************************************/

output   msolddl.h

UNIBASE

	prefix UM_
	suffix _rec

	create table sdata
	(
		key_id key,
		real	 sdata				/* Solid canonical data */
	);

/* Net surface key list; UNIBASE relation */

   create table netkey
   (
      key_id      key,
      key_id      id
   );

/* NCL solids */

   create table  solid 
   (
      key_id      key,
      character   label[64],        /* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
      int         subscr,           /* subscript */
		int			type,					/* Type of solid definition */
		int			closed,				/* Closed solid flag */
		real        box[6],				/* solid limit box */
		join			sdata,				/* Solid canonical data */
		join			displst,				/* display list */
		join			tesslst,				/* tesselation list */
		join			netkey,				/* surface key list */
   );

/* -- AG shell (solid and trimmed surface ) -- */

	create table agshell
	(
		key_id		key,
		character	label[8],			/* label */
		int			subscr,				/* subscript */
		int			material,			/* surface material */
		int			shelladdr			/* address of shell in dynamic memory */
	);


/*	-- edge identifier -- */

	create table edge
	(
		key_id key,
		int	 edge					/* ROMULUS edge number */
	);

/* -- ROMULUS solid body -- */

	create table body 
	(
		key_id		key,
		character	label[8],		/* label */
		int			subscr,				/* subscript */
		real			pitch,			/* hatching pitch for faces */
		character	name[16],		/* name of solid */
		int 			id,				/* ROMULUS body identifier */
		int 			body_number,	/* body number, to be matched with a
												number in the ROMULUS data base, so 
												ROMULUS database and unibase entities
												can be matched after save/load/merge. */
		int 			color,		  /* color TEMPORARY */
		join			edge	        /* edge identifiers in ROMULUS */
	);	


/***************************************************************************
**
**								GENERAL MODELING ENTITIES
**
***************************************************************************/

output	mxxxddl.h


UNIBASE

	prefix UM_
	suffix _rec


/*	-- coordinate system -- */

	create table coordsys
	(
		key_id key,
		int	type,					/* 0 => local coordinate system
											1 => who knows what */
		real	xaxis[3],			/* xaxis of coordinate system */
		real	yaxis[3],			/* yaxis of coordinate system */
		real	zaxis[3],			/* zaxis of coordinate system */
		real	origin[3],			/* origin of coordinate system */
		real	z_depth,				/* z depth value */
		character name[16]	  	/* name of coordinate system */
	);

/* -- list of members of a group -- */

	create table member
	(
		key_id  key,
		key_id  id					/* key of member of group */
	);

/*	-- group -- */

	create table grouper
	(
		key_id key,
		character name[16],	  	/* name of group */
		join member					/* members of group */
	);

/* -- drawing -- */

	create table drawing
	(
		key_id key,
		character name[16],		/* name of drawing */
		int drwsize,				/* size A-E ANSI; A4-A0 DIN */
		real drwscale,				/* drawing scale */
		int drwunits,				/* units of drawing scale */
		real modscale,				/* model scale */
		int modunits,				/* units of model scale */
		real plotprec,				/* plotting precision */
		join member					/* geometry contained in drawing */
	);

/* -- layer -- */

	create table layers
	(
		key_id key,
		int layer
	);

	create table layer
	(
		key_id key,
		character name[80],		/* layer name */
		int num,						/* layer number */
		logical displayable,		/* layer visibility */
		logical selectable,		/* layer entities pickability */
		join layers					/* sub-layers */
	);


output   mrenddl.h

UNIBASE

	prefix UM_
	suffix _rec

	create table light
	(
		key_id key,
		int index,
		int type,
		int intens,
		real position[3],
		real direction[3],
		real attenuation[3],
		real cone_angle,
 		real scale,
		int space,
		real exp,
		real ambient[4]
	);

/**************************************************************************
**
**							ATTRIBUTE BUNDLES
**
**************************************************************************/

output   mattrddl.h

UNIBASE

	prefix UM_
	suffix _rec

/*	--  general attribute bundle applicable to ALL relations -- */

	create table attrdata
	(
		key_id key,
		int	use_count,
		int	color,					/* current color */
		int	layer,					/* current layer */
		int	pen,						/* current pen type */
		int	line_style,				/* current line style */
		real  line_weight,			/* current line weight */
		real	line_width,				/* current line width */
		int displayable,			/* current displayable */
		logical selectable,			/* current selectable */
		int label_on			/* current label display off/on/leaderline*/
	);

/*	-- transform bundle applicable to ALL relations -- */

	create table transf
	(
		key_id key,
		int	use_count,
		real	tfmat[4][3],
	);


	/* create the current global attribute modals */
	create modal attrmdl
	(
		key_id key,
		int	use_count,
		int	color,					/* current color */
		int	layer,					/* current layer */
		int	pen,						/* current pen type */
		int	line_style,				/* current line style */
		real	line_weight,		/* current line weight */
		real	line_width,				/* current line width */
		int displayable,				/* current displayable */
		logical selectable,			/* current selectable */
		int label_on			/* current label display off/on/leaderline*/
	);

	/* create the current global material modals */
	create modal mtrlmdl
	(
		int  index,						/* index of current material */
		character name[64][20],
		real ka[64],
		real kd[64],					/* diffuse light coefficient */
		real ks[64],					/* specular light coefficient */
		real ks_r[64],
		real ks_g[64],
		real ks_b[64],
		real spec_exp[64]				/* specular light expoenent */
	);

	/* create the current global display attribute modals */
	create modal dispattr
	(
		int consclr,					/* current construction color */
		int conslnstyl,				/* current construction linestyle */
		real conswght,					/* current construction line weight	*/
		int featclr,					/* current feature color */
		int fea_decpl,					/* number of decimal places to display */
		int fillclr,					/* current fill area colors	*/
		logical hidn_lines,			/* TRUE iff hidden lines drawn */
		key_id view_in,				/* 0 => visible in all views; else this
												is the key of the view tuple that the
												entity is in */
		key_id cpln_key				/* key of construction plane */

	);

/******************************************************************************
**
**								DRAWING MODALS
**
******************************************************************************/

output	mdraw.h

UNIBASE

	prefix UM_
	suffix _rec

	create modal drwmdl
	(
		key_id	key,
		int		use_count,
		key_id	drwsn,					/* key of drawing screen entity */
		key_id	drwvp,					/* key of drawing view port entity */
		key_id	drwvw,					/* key of drawing view entity */

		key_id	curdrw,					/* key of current active drawing
													(if in model mode) */
		character modsname[16],			/* name of screen in model mode */
		real	aspect,						/* aspect ratio of drawing */

		character unitsstr[100],		/* string for status area */
		int	drwsize,						/* current drawing size (e.g. "A") */
		real	drwscale,					/* current drawing scale (e.g. 1.0) */
		int	drwunits,					/* current drawing units (e.g. IN) */
		real	modscale,					/* current model scale (e.g. 40.0) */
		int	modunits,					/* current model units (e.g. CM) */
		real	plotprec						/* current plot precision */

	);

/******************************************************************************
**
**										LABEL MODAL TABLE
**
******************************************************************************/

output	mlabddl.h

UNIBASE

	prefix UM_
	suffix _rec
	create modal labelmdl
	(
		key_id		key,
		int			use_count,
		int			max,						/* max number of unique labels */
		int			num,						/* actual number of unique labels */
		character	pf[40][21],				/* label prefix */
		int			next[40],				/* next number to assign to label*/
		character	pfs[40][21],			/*NCL-  label prefix when there is subscript*/
		int			subscr[40],			/*NCL-  Array of subscripts to assign to label*/
		int			issub[40],			/*NCL-  Is this a  subscripted label*/
		int			rel[256]			/* index into above tables for relation label */
	);

/******************************************************************************
**
**										VIEWING
**
******************************************************************************/

output	viewddl.h

UNIBASE

	prefix UV_
	suffix _rec

	create table viewdef
	(
		key_id		key,					/* UNIBASE key */

		character	name[15],			/* view name (unique) */

		int			vtype,				/* 0 => planar
													1 => 3D */

		logical		can_save,			/* UU_TRUE => can save view using
													this view name
													UU_FALSE => cannot save view
													using this view name */

		logical		modified,			/* UU_TRUE => saved != current
													UU_FALSE => saved == current */

		int			projection,			/* 0 => parallel
													1 => perspective */

		/*	saved viewing parameters */
		real			sav_ref_pt[3],		/* view plane center */
		real			sav_pln_norm[3],	/* view plane normal */
		real			sav_up_vect[3],	/* view plane up vector */
		real			sav_eye_dist,		/* distance to eye point (along normal)
													= 0.0 for parallel */

		real			sav_aperture,		/* length of window in "x" axis */
		real			sav_front_clip,	/* distance to front clip plane */
		real			sav_back_clip,		/* distance to back clip plane */
		logical		sav_do_clip,		/* TRUE => apply hither/yon clipping */

		/*	current viewing parameters */
		real			cur_ref_pt[3],		/* view plane center */
		real			cur_pln_norm[3],	/* view plane normal */
		real			cur_up_vect[3],	/* view plane up vector */
		real			cur_eye_dist,		/* distance to eye point (along normal)
													= 0.0 for parallel */

		real			cur_aperture,		/* length of window in "x" axis */
		real			cur_front_clip,	/* distance to front clip plane */
		real			cur_back_clip,		/* distance to back clip plane */
		logical		cur_do_clip,		/* TRUE => apply hither/yon clipping */

	);

	create table vport
	(
		key_id		key,			/* UNIBASE key */

		character	name[15],	/* viewport name */

		int			xform,			/* transform number */

		real			llf[3],		/* lower left front of viewport*/
		real			urb[3],		/* upper right back of viewport*/

		key_id		cur_view,	/* view associated with viewport */

		int			disp_prio,
		int			input_prio,

		logical		disp_all,

		int			bord_seg,	/* DIGS display segment of view status */
		logical		aperture_on,/* TRUE => show aperture size */
		logical		v_axis_on,	/* TRUE => show view axis */
		logical		name_on,		/* TRUE => show view name */
		logical		bord_on,		/* TRUE => show border */
		int			nverts,		/* number of vertices in border */
		real			vertices[60],/* vertex coordinates */

		int			grid_seg,	/* DIGS display segment for grid */
		logical		grid_on,		/* TRUE => show grid */
		logical		motion,		/* TRUE >= Display motion */
		int			disp_mode,	/* 1 = Wireframe, 2 = Shaded, 3 = Hidden line */
		logical		wireframe,	/* TRUE >= Display wireframe */
	);

	create table screen
	(
		key_id		key,

		character	name[15],	/* name of screen */

		int			active,		/* UV_ACTIVE => active screen
											UV_INACTIVE => not active screen */

		int			wstation,	/* workstation active screen is on */

		int			nvports,		/* number of viewports on screen */
		key_id		vports[20],	/* UNIBASE keys of viewports on screen */
	);

/******************************************************************************
**
**										UNIBASE
**
******************************************************************************/
output	rbaseddl.h

UNIBASE

	prefix UU_
	suffix _rec

/*	-- system attribute bundle -- */

	create table sysattr
	(
		key_id key,
		int	 use_count,		/* counter of times used */
		int	 color,			/* color */
		int	 layer,			/* layer number */
		int	 pen,				/* pen number */
		int	 line_style,	/* line style */
		real	line_weight,		/* current line weight */
		real	 line_width,	/* line weight */
		int displayable,			/* current displayable */
		logical selectable,			/* current selectable */
		int label_on			/* current label display off/on/leaderline */
	);

/******************************************************************************
**
**										SYMBOLS 
**
******************************************************************************/

output   bsymddl.h

UNIBASE

	prefix UB_
	suffix _rec

/*		-- symbol geometry --		*/

	create table geom
	(
		key_id	key,
		key_id	dummy
	);

/*		-- text node --		*/

	create table text_nod
	(
		key_id	key,
 		int	prompt,	/* 0=prompt response optional; 1=prompt response required;
 							 * 2=label */
 		int	visibility,/* either UB_GRAPHIC_TEXT_NODE or UB_NONGRAPHIC_TEXT_NODE
 								*/
		int 	masterindx,/* index indicating which text node this is */
		key_id	text_key
	);

/*		-- snap node --		*/
	create table snap_nod
	(
		key_id	key,
		key_id	snap_key,
		int 		nbr
	);

/*		-- master symbol variable list for containing master symbols */
	create table masters
	(
		key_id	key,
		key_id	mastr_key,
		real 		tfmat[4][3]
	);

/*		-- instances variable list */
	create table inst
	(
		key_id	key,
		key_id	inst_key,
		real 		tfmat[4][3]
	);

/*		-- connectors associated with instances --		*/
	create table acon
	(
		key_id	key,
		key_id	con_key
	);

	

/*		-- (master) symbol --		*/
	create table symbol
	(
		key_id		key,
		character	label[64],	/* master symbol name */
		real 			labloc[3],	/* label display location */
		real 			ldrloc[3],	/* default label display location */
		int			subscr,     /* subscript */
		int 			version,		/* modification time stamp */
		character	path[200],	/* full system dependent path name to sym lib */
		join			masters,		/* keys of master symbols contained in this sym */
		join			inst,			/* keys of instances of this master */
		join			geom,			/* list of keys to the symbol geometry */
		join			text_nod,	/* list of records for text nodes */
		join			snap_nod
	);

/*		-- (instance) symbol --		*/

	create table instance
	(
		key_id		key,
		character	label[64],	/* symbol instance name */
		real 			labloc[3],	/* label display location */
		real 			ldrloc[3],	/* default label display location */
		int			subscr,     /* subscript */
		join			geom,			/* list of keys to the symbol geometry */
		join			text_nod,	/* list of records for text nodes */
		join			snap_nod
	);

	create table symattr
	(
		key_id key,
		int	use_count,
		int	color,					/* current color */
		int	layer,					/* current layer */
		int	pen,						/* current pen type */
		int	line_style,				/* current line style */
		real	line_weight,			/* current line weight */
		real	line_width,				/* current line width */
		int displayable,				/* current displayable */
		logical selectable,			/* current selectable */
		int label_on,					/* current label display off/on/leaderline */
		logical see_snod,				/* TRUE iff snap nodes are visible */
		int see_tnod					/* either UB_ALL_TEXT_NODES_VISIBLE,
											 * 		 UB_ONLY_GRAPHIC_TEXT_NODES, or
											 *			 UB_NO_TEXT_NODES_VISIBLE 
											 */
	);

/*		-- instances associated with connectors --		*/
	create table ainst
	(
		key_id	key,
		key_id	inst_key
	);

/*		-- connector symbol --		*/

	create table conector
	(
		key_id		key,
		key_id		pline,		/* polyline for connector */
		join			ainst			/* list of associated instances */
	);


/******************************************************************************
**
**										UNIBASE 
**
******************************************************************************/

output   riddl.h

UNIBASE

	prefix UR_
	suffix _rec



/* part list */
create table part_lis
(				/* part_id by magic */
	character	part_name[16],
	character	part_date[16]
);

/* geometry table */
create table geometry
(				/* don't really want part_id on this one */
	character	geom_type[16],
	int			geom_id,
	int			num_varl
);

/* master tuple table */
create table assocs	/* support relation for mtuple key list */
(
	key_id	key,
	key_id	assoc_key		/* list of associations */
);

create table MTID
(
	int		dsegid,
	int		save1,
	key_id	view_key,
	int		bit_tbl,
	join		assocs
);

/* unibase modals */
create modal unimod
(
	logical	dflt_editable,
	logical	dflt_blanked
);

/* unibase statistics */
create table unistat
(
		key_id		key,
		character	system[80],			/* originating system */
		character	processor[80],		/* conversion pre-processor */
		character	author[80],			/* original author */
		character	company[80],		/* organization */
		character	fname[256],			/* originating file */
		character	date[40],			/* time stamp */
		character	translator[80],	/* translator program */
		character	mod_system[80],	/* modifying system */
		character	mod_author[80],	/* modified by author */
		character	mod_company[80],	/* modified by company */
		character	mod_date[40],		/* time modified */
		string		notes 				/* text string list */
);

/******************************************************************************
**
**										DRAFTING 
**
******************************************************************************/

/* Uni-ddl file for drafting entities */

output		adrfddl.h

UNIBASE

	prefix	UA_
	suffix	_rec

create table txtblk
(
key_id			key,
int 				subtype,	/* subtype */
int				txt_font,	/* txt_font */
real				txt_dens,	/* txt_density */
int				color,	/* color */
int				char_cnt,	/* char_cnt */
int				txt_just,	/* txt_just */
character		fontname[17],	/* fontname */
character		tstring[1025],	/* tstring */
real				origin[3],	/* origin */
real				dx,	/* dx */
real				dy,	/* dy */
real				slant,	/* slant */
real				tangle,	/* tangle */
real				txt_size,	/* txt_size */
real				sub_sup,	/* sub_super */
real				char_exp,	/* character_expansion */
real				char_spa,	/* character_space */
real				line_spa	/* line_spacing */
);


create table arcblk
(
key_id			key,
int				subtype,	/* subtype */
int				arc_font,	/* arc_font */
real				arc_dens,	/* arc_density */
int				color,	/* color */
int				num_pts,	/* num_pts */
real				cent_pt[3],	/* center_pt */
real				radius,	/* radius */
real				angles[50],	/* angles */
);


create table lineblk
(
key_id			key,
int 				subtype,	/* subtype */
int				line_fon,	/* line_font */
real				line_den,	/* line_density */
int				color,	/* color */
int				num_pts,	/* num_pts */
real				line_seg[50][3],	/* line_seg */
);

	
create table arrowblk
(
key_id			key,
int				arr_type,	/* arrow_type */
int				arr_font,	/* line_font */
real				arr_dens,	/* line_density */
int				color,	/* color */
real				location[3],	/* location */
real				aangle,	/* aangle */
real				asize	/* size */
);


create table assoblk
(
key_id			key,
int				asso_typ,	/* asso_type */
int				modifier,	/* modifier */
key_id			asso_key,	/* key */
real				location[3]	/* location */
);


create table draft
(
key_id			key,	/* key */
int				draf_ati[44],
real				draf_atr[25],
real				cpln[12],
join 				txtblk,	/* txt_blk */
join 				arcblk,	/* arc_blk */
join 				lineblk,	/* line_blk */
join 				arrowblk,	/* arrow_blk */
join 				assoblk	/* asso_blk */
);


create table hatchlin
(
key_id		key,
join 		wt			/* defined above in CURVES */
);


/***************		CALC		****************/

output	qcddl.h
UNIBASE

prefix	UQ_

create table func
(
	key_id   key,
	int	arg[5],
	character	funcbuf[200]
);


create table qstb
(
	key_id	key,
	character	symbol[12],
	int	ttype,
	real	val[3],
	join  func
);

/***********************************************************************
**
**										TEXT
**
************************************************************************/

output   atxtddl.h

UNIBASE

	prefix UA_
	suffix _rec

/*--- text record 	---*/
	create table txt 
		(
			key_id 		key,
			character	label[64],			/* label */
			real 			labloc[3],			/* label display location */
			real 			ldrloc[3],			/* default label display location */
			int			subscr,				/* subscript */
			int 			subtype,				/* subtype */		
			key_id		arckey,				/* key of the associated arc */
			real			dx,		 			/* dx */			
			real			dy,		 			/* dy */				
			real			tangle,	 			/* tangle */	
			real			position[3],  		/* text position */
			string		tchar, 				/* text string list */
			join			displst, 			/* text string list */
    	);

/* text attribute boundle record */
	create table txtattr 
		(
			key_id key,
			int	use_count,
			int	color,			/* current color */
			int	layer,			/* current layer */
			int	pen,				/* current pen type */
			int	line_style,		/* current line style */
			real	line_weight,		/* current line weight */
			real	line_width,		/* current line width */
			int 	displayable,	/* current displayable */
			logical selectable,	/* current selectable */
			int label_on,			/* current label display off/on/leaderline */
			int   font,     		/* font number */
			int   prec,      		/* text precision  */
			real  expn,        	/* text character expansion */
			real  spacing,    	/* text character spacing  */
			real  height,     	/* text character height */
			real  up[3],      	/* text character up vector 2D only
		                      	    		relative to text plane */
			real  plane[3],   	/* text plane normal vector */
			int   path,       	/* text path - right,left,up,down */
			int   align_hor,  	/* text alignment - horizontal */
			int   align_ver,   	/* text alignment - vertical */
			int	txt_dens,		/* txt_density */ 				
			real	slant,	 		/* slant */					
			real	sub_sup,	 		/* sub_super */		
			real	line_spacing, 		/* line_spacing */
			int	entity_site		/* text site    */
		);


/***************************************************************************
**
**			NCL geometry definitions
**
***************************************************************************/

output   nccsddl.h

UNIBASE

	prefix NCL_
	suffix _rec

/*	NCL attribute bundle; UNIBASE relation */

	create table nclattr
	(
		key_id key,
		int	use_count,
		int	color,					/* current color */
		int	layer,					/* current layer */
		int	pen,						/* current pen type */
		int	line_style,				/* current line style */
		real  line_weight,			/* current line weight */
		real	line_width,				/* current line width */
		int displayable,				/* current displayable */
		logical selectable,			/* current selectable */
		int label_on					/* current label display off/on/leaderline*/
	);

/* NCCS point definition; UNIBASE relation */

	create table nclpt
	(
		key_id		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		int			markertype,			/* type of DIGGS marker to display */
		logical		snap_node,			/* TRUE iff the point is a snap node */
		real			pt[3],				/* coordinates of point */
		join			displst				/* display list */
	);

/* NCCS line definition; UNIBASE relation */

	create table nclln
	(
		key_id 		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		real 			spt[3],				/* start point */
		real 			ept[3],				/* end point */
		join			displst				/* display list */
	);

/* NCCS circle/arc definition; UNIBASE relation */

	create table nclci
	(
		key_id 		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		real 			radius,				/* radius */
		real 			dang,					/* delta angle */
		real 			center[3],			/* center point */
		real 			svec[3],				/* unit vector to start point */
		real 			nvec[3],				/* unit normal */
		join			displst				/* display list */
	);

/* NCCS vector definition; UNIBASE relation */

	create table vector
	(
		key_id		key,
		character	label[64],
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		real			vec[3],
		join			displst				/* display list */
	);

/* NCCS plane definition; UNIBASE relation */

	create table nclpl 
	(
		key_id 		key,
		character	label[64],			/* label */
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
		real		   radius,				/* display extent radius */
		real   		pt[3],				/* point of plane */
		real   		nvec[3],		   	/* unit normal vector of plane */
		join			displst				/* display list */
	);


/* NCCS matrix definition; mapped from UNICAD coordinate system */

	create table matrix
	(
		key_id		key,
		character	label[64],
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int	subscr,	      			/* subscript */
      real   dalen,                 /* display parameter: axis length */ 
      real   dbox[3],               /* display parameter: box size */ 
		real   mat[3][4],		      	/* transformation matrix */
		join	 displst						/* display list */
	);


/* record structure for parameter values along curves and surfaces */

	create table param
	(
		key_id		key,
		float			s						/* parameter value */
	);


/* record structure for segments for Bezier curve variable list */

	create table segment
	(
		key_id		key,
		real			point[3],			/* coordinate of start point of segment */
		float			delta[3],			/* vector from start point to next point
													of Bezier polygon */
		float			duds0,				/* slope at beginning of curve segment */
		float			duds1,				/* slope at end of curve segment */
		float			rho					/* ratio of length of 3rd leg of Bezier
													polygon to the length of 1st leg of the
													next segment Bezier polygon */
	);


/* NCCS Bezier curve; UNIBASE relation */

	create table curve 
	(
		key_id 		key,
		character	label[64],
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int			subscr,				/* subscript */
	   int	 	   closdinu,			/* =1 iff closed */
      real        t0,               /* starting parameter */
      real        t1,               /* ending parameter */
      real        t_end,            /* eq. to t[n] of the RBspl */
		join	   	param,				/* parameter value of segment along curve */
      join 		   segment,				/* variable length list of segments in this
													curve */
		join			displst				/* display list */
	);

/* record structure for patches on surface panel */

   create table patch
   (
      key_id      key,
      real        pt[3],            /* start point of patch */
      float       delta[7][3],      /* delta values of patch */
      float       rho               /* used to reconstruct points 9-12 of
                                       patch from points 1-8 of next patch */
   );



/* NCCS surface panel; UNIBASE relation */

   create table panel 
   (
      key_id      key,
      character   label[8],
      int         subscr,           /* subscript */
      int         type,             /* 0 => full 16 point patches
                                       1 => ruled surface 8 point patches  */
      int         no_param,
      float       param[50],
      join        patch             /* variable length list of patches in this
                                       panel; order is significant */
   );


/* record structure for panel keys for surface variable list */

   create table panelkey 
   (
      key_id      key,
      key_id      id                /* MTID of panel */
   );

/* surface; UNIBASE relation */

	create table  surface 
	(
		key_id      key,
		character   label[64],
		real        labloc[3],        /* label display location */
		real        ldrloc[3],        /* default label display location */
		int         subscr,           /* subscript */
		int         rldnu,            /* =1 iff ruled in u */
		int         swapuv,           /* swap and/or reverse u,v flag */
		logical     rev_normal,       /* UU_TRUE => reverse sense of normal */
		int         closdinu,         /* =1 iff closed in u */
		int         closdinv,         /* =1 iff closed in v */
		int         offset,           /* 0 => not offset; 7 => offset */
		float       offdist,          /* offset distance */
		int         surf_type,        /* type of surface */
		int         primitive,        /* special primitives */
		real        prim_param[16],   /* parameters defining the primitives*/
		join        panelkey,         /* variable length list of panels in this
		                                 surface */
		join        sskey,            /* cv on sf key list */
		join        displst,          /* display list */
		join        tesslst,          /* tesselation list */
		join        boxlst,           /* surface limit box */
		join        xyzbylst          /* surface xyz boundary curve */
	);

/* surface of revolution; UNIBASE relation */

	create table  revsurf 
	(
		key_id      key,
		character   label[64],
		real        labloc[3],        /* label display location */
		real        ldrloc[3],        /* default label display location */
		int         subscr,           /* subscript */
		int         dummy,            /* dummy to comp. pad at the struct end */
		int         rldnu,            /* =1 iff ruled in u */
		int         swapuv,           /* swap and/or reverse u,v flag */
		logical     rev_normal,       /* UU_TRUE => reverse sense of normal */
		int         closdinu,         /* =1 iff closed in u */
		int         closdinv,         /* =1 iff closed in v */
		float       offdist,          /* offset distance */
		key_id      cvkey,            /* key of generatrix curve */
		int         primitive,        /* special primitives */
		real        prim_param[16],   /* parameters defining the primitives*/
		real        pta[3],           /* axis point */
		real        vca[3],           /* axis vector */
		real        sa,               /* start angle */
		real        ta,               /* terminate angle */
		join        sskey,            /* cv on sf key list */
		join        displst,          /* display list */
		join        tesslst,          /* tesselation list */
		join        boxlst,           /* surface limit box */
		join        xyzbylst          /* surface xyz boundary curve */
	);

/* mesh patch; UNIBASE relation */

   create table mpatch
   (
      key_id      key,
      real        pt[3],            /* point of mesh surface patch */
      real        delta[15][3]      /* delta vectors */
   );

/* mesh surface; UNIBASE relation */

	create table meshsf
	(
		key_id      key,
		character   label[64],
		real        labloc[3],        /* label display location */
		real        ldrloc[3],        /* default label display location */
		int         subscr,           /* subscript */
		int         dummy,            /* dummy to comp. pad at the struct end */
		int         rldnu,            /* =1 iff ruled in u */
		int         swapuv,           /* swap and/or reverse u,v flag */
		logical     rev_normal,       /* UU_TRUE => reverse sense of normal */
		int         closdinu,         /* =1 iff closed in u */
		int         closdinv,         /* =1 iff closed in v */
		int         offset,           /* 0 => not offset; 7 => offset */
		float       offdist,          /* offset distance */
		int         surf_type,        /* type of surface */
		int         m,                /* number of patches in U direction */
		int         n,                /* number of patches in V direction */
		join        mpatch,           /* mesh surface patches */
		join        displst,          /* display list */
		join        tesslst,          /* tesselation list */
		join        boxlst,           /* surface limit box */
		join        xyzbylst          /* surface xyz boundary curve */
   );

/* quilt patch; UNIBASE relation */

   create table qpatch
   (
      key_id      key,
      int         bpatchnum[4],        /* boundary patch numbers */
      float       origin[3],           /* origin of quilt surface patch */
      float       xvalues[25],         /* */
      float       yvalues[25],         /* */
      float       zvalues[25]          /* */
   );

/* quilt surface; UNIBASE relation */

   create table quiltsf
   (
      key_id      key,
      character   label[64],
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
      int         subscr,           /* subscript */
	  int	material,
      int         surf_type,        /* type of surface */
      int         numpatches,       /* number of patches */
	  int	 	  rldnu,			/* =1 iff ruled in u */
	  int	 	  rldnv,			/* =1 iff ruled in v */
      int         offset,           /* 0 => not offset; 7 => offset */
      float       offdist,          /* offset distance */
      float       midpt[12][3],     /* */
      logical     shaded,           /* shaded flag UU_TRUE = shaded */
		int	      lucency,          /* translucency value */
      int         dummy,            /* dummy to comp. pad at the struct end */
      join        qpatch,           /* quilt surface patches */
		join			displst,				/* display list */
		join			tesslst,				/* tesselation list */
		join        boxlst,           /* surface limit box */
      join        xyzbylst          /* surface xyz boundary curve */
   );

/* patern point; UNIBASE relation */

   create table patpnt
   (
      key_id      key,
      real        pnpt            /* point/pointvector of patern */
   );

/* patern header; UNIBASE relation */

   create table patern
   (
      key_id      key,
      character   label[64],
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
      int         subscr,           /* subscript */
      int         markertype,       /* type of DIGGS marker to display */
      int         pntype,           /* type of patern (PTs or PVs) */
      int         dummy,            /* dummy to comp. pad at the struct end */
      join        patpnt,           /* patern points */
		join			displst				/* display list */
   );


/* Net surface header; UNIBASE relation */

	create table netsf
	(
		key_id      key,
		character   label[64],
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
		int         subscr,           /* subscript */
		int         surf_type,        /* type of surface */
		int         bndsfs [40][4],   /* boundary surface numbers */
		
		join        netkey,           /* keys of component surfaces */
		join        sskey,            /* cv on sf key list */
		join			displst,				/* display list */
		join			tesslst				/* tesselation list */
);

/* shape element; UNIBASE relation */

   create table shapwd
   (
      key_id      key,
      real        wd,               /* shape word */
   );

/* shape header; UNIBASE relation */

   create table shape
   (
      key_id      key,
      character   label[64],
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
      int         subscr,           /* subscript */
      int         f2d3d,           /* Indicates the key to the subcrvs */
		int			dummy,				/* Alignment variable */
      join        shapwd,           /* patern points */
		join			displst,				/* display list */
		join			tesslst				/* tessalation list */
   );
/* NCL scalar definition */

	create table scalar
	(
		key_id		key,
		character	label[64],
		int			subscr,				/* subscript */
		real		scalar_value,    	/* scalar value */
		character	classnm[21],		/* scalar class name*/
		character	descript[64],		/* scalar descript */
		real		modified
	);


/* NCL extended geo defs - altered label locations */

	create table labloc
	(
		key_id		key,
		real		del[3],    	/* xyz delta values */
        int         index    /* index of this entry in labtbl */
	);

	create table labtbl
	(
		key_id		key,
		join labloc
	);

/* NCL evaluated entity element relation */

   create table evwd
   (
      key_id      key,
      real        wd               /* evaluated entity word */
   );

/* NCL Evaluated Curve Relation */

   create table evalcv
   (
      key_id      key,
      character   label[64],
		real 			labloc[3],			/* label display location */
		real 			ldrloc[3],			/* default label display location */
      int         subscr,           /* subscript */
      int         curve_type,       /* type of curve */
      join        evwd,          /* curve data */
		join			displst				/* display list */
   );

/* NCL Evaluated Surface Relation */

   create table evalsf
   (
      key_id      key,
      character   label[64],
      real        labloc[3],        /* label display location */
      real        ldrloc[3],        /* default label display location */
      int         subscr,           /* subscript */
      int material,
      int         numupaths,        /* number of u paths to draw */
      int         numvpaths,        /* number of v paths to draw */
      int         ptsperucrv,       /* number of points per u path */
      int         ptspervcrv,       /* number of points per v path */
      int      rldnu,       /* =1 iff ruled in u */
      int      rldnv,       /* =1 iff ruled in v */
      int      closdinu,       /* =1 iff closed in u */
      int      closdinv,       /* =1 iff closed in v */
      int         surf_type,        /* type of surface */
      int         offset,           /* 0 => not offset; 7 => offset */
      logical     shaded,           /* shaded flag UU_TRUE = shaded */
      int         lucency,          /* translucency value */
      float       offdist,          /* offset distance */
      join        evwd,         /* surface data */
      join        displst,       /* display list */
      join        tesslst        /* tesselation list */
   );

/* NCL point vector definition; UNIBASE relation */

    create table nclpv
    (
        key_id         key,
        character      label[64], /* label */
        real           labloc[3], /* label display location */
		real 			ldrloc[3],			/* default label display location */
        int            subscr,    /* subscript */
        real           pt[3],     /* start point */
        real           ve[3],     /* vector */
        join           displst    /* display list */
    );

/* NCL trimmed surface relation */

   create table ibndykey 
   (
      key_id      key,
      key_id      id                /* Keys of innner boundaries */
   );

	create table trimsf
	(
		key_id      key,      
		character   label[64],        /* label */
		real        labloc[3],        /* label display location */
		real        ldrloc[3],        /* default label display location */
		int         subscr,           /* subscript */
		int         closdinu,         /* =1 iff closed in u */
		int         closdinv,         /* =1 iff closed in v */
		float       offdist,          /* offset distance */
		key_id      uv_key,           /* uv boundary curve key */
		key_id      cv_key,           /* boundary curve key */
		key_id      bs_key,           /* base surface key */
		float       ub_min,           /* minium u boundary of base */
		float       ub_max,           /* maxium u boundary of base */
		float       vb_min,           /* minium v boundary of base */
		float       vb_max,           /* maxium v boundary of base */
		float       u_min,            /* minium u value of base sf */
		float       u_max,            /* maxium u value of base sf */
		float       v_min,            /* minium v value of base sf */
		float       v_max,            /* maxium v value of base sf */
		int         drive_type,       /* 0 - drive underlying sf,
		                                 1 - drive as trimmed */
		join        ibndykey,         /* inner boundaries */
		join        displst,          /* display list */
		join        tesslst,          /* tesselation list */
		join        boxlst,           /* surface limit box */
		join        xyzbylst,         /* surface xyz boundary curve */
		join        uvbylst,          /* surface uv boundary curve */
		join        uvboxlst          /* surface uv limit box */
	);

/* NCL Data Statement relation */

   create table datael
   (
      key_id      key,
      real        value,      /* Value of data statement element */
      character   label[64],  /* Label of element */
      int         isub,       /* Subscript of element */
      int         type,       /* Type of data statement element */
      int         delim,      /* Delimeter following data statement element */
      int         dum         /* dummy field to align on 8 byte boundary */
  );

   create table datast
   (
      key_id      key,
      character   label[64],  /* label */
      int         subscr,     /* subscript */
      int         nargs,      /* number of arguments */
      join        datael
   );

	create table textvar
	(
		key_id      key,
		character   label[64],  /* label */
		int         subscr,     /* subscript */
		character   text[256]   /* label */
	);

output   ncolorddl.h

UNIBASE

	prefix NCL_
	suffix _rec
	create table color
	(
		key_id key,
		character color_name[64][96],
		int color_value[64][3]
	);
