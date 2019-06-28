
/*********************************************************************
**    NAME         :  m3upoly.c
**       CONTAINS: user interface for polygon filled area entities
**			umu_c40_poly_popmenu()
**			umu_c40_poly()
**			umu_c40_rectangle()
**			umu_c40_rightangle()
**			umu_m40_modpolygon(key, option)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m3upoly.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:59
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "dasnog.h"
#include "mdclass.h"
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mattr.h"
#include "mdcpln.h"
#include "mdpick.h"
#include "mdebug.h"
#include "mpopmenu.h"
#include "nclcmd.h"
#include "nclinp.h"

void umu_c40_poly();
void umu_c40_rectangle();
void umu_c40_rightangle();

extern char uw_color_name[64][96];

/*********************************************************************
**    E_FUNCTION :  umu_c40_poly_popmenu()
**       Put up a popup menu for all of the shape creation techniques.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_c40_poly_popmenu()

	{
	int option;
	int status;

	uu_denter(UU_MTRC,(us,"umu_c40_poly_popmenu()"));

	status = um_popupmenu(UM_DEFINE_SHAPE, &option);
	if (option == 1)
		umu_c40_poly();
	else if (option == 2)
		umu_c40_rectangle();
	else if (option == 3)
		umu_c40_rightangle();

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c40_poly()
**       create polyfill region
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_c40_poly()

	{
	struct UM_poly_rec e;
	int numint;
	int	i,j;
	UM_coord	vx[3];
    UD_NDCLOCREC tmp[3];

	Gwpoint3	tempseg[2];

	uu_denter(UU_MTRC,(us,"umu_c40_poly()"));

	while (UU_TRUE)		/* one iteration for each polygon	*/
		{
		ur_setup_data(UM_POLY_REL, &e, sizeof(struct UM_poly_rec));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (e.label, "");
		e.subscr = 0;
		e.numvtx = 0;
		for (i=0; i<200; i++) um_vctovc(UM_zerovec, e.vertex[i]);
		i = 0;

		do		/* until user 'done' before picking location	*/
			{
			ud_ldas(UD_DASCART, /* Vertex of polygon */UM_MODEL, 194,
				tmp, 1, &numint, UD_NODEFAULT);
            for(j=0; j<3; j++) vx[0][i] = tmp[0].cord[i];
            for(j=0; j<3; j++) vx[1][i] = tmp[1].cord[i];
            for(j=0; j<3; j++) vx[2][i] = tmp[2].cord[i];

			if (numint > 0)
				{
				um_vctovc(vx, e.vertex[i]);
				i++;	/* inc.vertex index	*/
				e.numvtx++;

				/** a little visual feedback, if you please	**/
				if (e.numvtx > 1)
					{

					tempseg[0].x = e.vertex[i-2][0];
					tempseg[0].y = e.vertex[i-2][1];
					tempseg[0].z = e.vertex[i-2][2];

					tempseg[1].x = e.vertex[i-1][0];
					tempseg[1].y = e.vertex[i-1][1];
					tempseg[1].z = e.vertex[i-1][2];

					gpolyline3( 2, tempseg);	/* drawn in no segment	*/
					}
				}
			}	while ((numint > 0) && (i < 200));

		if (e.numvtx < 3)	/* too few vertices: exit	*/
			{
			if (e.numvtx > 0)
				uu_uerror0(UM_MODEL/* must have more than 2 vertices */, 174);
			goto done;
			}

		/* FIX: test for coplanar */

		e.fcolor = ur_get_dispattr_fillclr();

		um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uc_display(&e);
		}

done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c40_rectangle()
**       Create a rectangular polygon filled region from two points
**			input by the user. The two points are projected onto the
**			construction plane, and the rectangle is defined with
**			sides parallel to the construction plane axes.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_c40_rectangle()

	{
	struct UM_poly_rec e;
	UM_coord  cpln1;
	UM_coord  cpln2;
    UD_NDCLOCREC pt1, pt2;

	UM_vector vec, xproj, yproj;
	UM_length xlen, ylen;
	int numint;
	int i;

	uu_denter(UU_MTRC,(us,"umu_c40_rectangle()"));

	while (UU_TRUE)
		{
		
		/* initialze the polygon entity */
		ur_setup_data(UM_POLY_REL, &e, sizeof(e));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (e.label, "");
		e.subscr = 0;
		e.numvtx = 0;
		for (i=0; i<200; i++) um_vctovc(UM_zerovec, e.vertex[i]);

		/* get the first point from the user */
		ud_ldas(UD_DASCART, /* Vertex of polygon */UM_MODEL, 194,
			&pt1, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		/* get the second point from the user */
		ud_ldas(UD_DASCART, /* Vertex of polygon */UM_MODEL, 194,
			&pt2, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		/* project the two points to the construction plane */
		um_nptpln(&pt1, UM_cpln.origin, UM_cpln.zaxis, cpln1);
		um_nptpln(&pt2, UM_cpln.origin, UM_cpln.zaxis, cpln2);
		um_p_ary(UM_PFLOAT,"pt1",3,&pt1);
		um_p_ary(UM_PFLOAT,"cpln1",3,cpln1);
		um_p_ary(UM_PFLOAT,"pt2",3,&pt2);
		um_p_ary(UM_PFLOAT,"cpln2",3,cpln2);

		/* define the polygon having the two points on the 
			construction plane with sides parallel to the
			xy axes */
		um_vcmnvc(cpln2, cpln1, vec);
		xlen = um_dot(vec, UM_cpln.xaxis);
		um_vctmsc(UM_cpln.xaxis, xlen, xproj);
		ylen = um_dot(vec, UM_cpln.yaxis);
		um_vctmsc(UM_cpln.yaxis, ylen, yproj);
		um_p_ary(UM_PFLOAT,"vec",3,vec);
		um_p_ary(UM_PFLOAT,"xproj",3,xproj);
		um_p_ary(UM_PFLOAT,"yproj",3,yproj);

		um_vctovc(cpln1, e.vertex[0]);
		um_vcplvc(e.vertex[0], xproj, e.vertex[1]);
		um_vcplvc(e.vertex[1], yproj, e.vertex[2]);
		um_vcmnvc(e.vertex[2], xproj, e.vertex[3]);
		um_vcmnvc(e.vertex[3], yproj, e.vertex[4]);
		e.numvtx = 5;

		e.fcolor = ur_get_dispattr_fillclr();

		um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uc_display(&e);
repeat:;
		}

done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c40_rightangle()
**		Create a polygon filled area by the following method:
**				1. Prompt the user for two points defining a line
**				2. Each subsequent point will determine the direction
**					and distance of the next leg of the polygon which
**					starts at the end of the previous leg and is at a
**					right angle
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_c40_rightangle()

	{
	struct UM_poly_rec e;
	int numint;
	int	i;
	UM_coord nvx;
    UD_NDCLOCREC vx;

	UM_coord spt;
	UM_vector lnvc;
	Gwpoint3	tempseg[2];

	uu_denter(UU_MTRC,(us,"umu_c40_rightangle()"));

	while (UU_TRUE)
		{
		/* initialize the polygon record */
		ur_setup_data(UM_POLY_REL, &e, sizeof(struct UM_poly_rec));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (e.label, "");
		e.subscr = 0;
		e.numvtx = 0;
		for (i=0; i<200; i++) um_vctovc(UM_zerovec, e.vertex[i]);

		i = 0;
		do		/* until user 'done' before picking location	*/
			{
			ud_ldas(UD_DASCART, /* Vertex of polygon */UM_MODEL, 194,
				&vx, 1, &numint, UD_NODEFAULT);
			if (numint > 0)
				{

				/* save the first point for stopping criteria */
				if (i == 0 ) um_vctovc(&vx, spt);
	
				/* modify point 2,... to maintain the right angle constraint */
				if (i > 1)
					{
					um_vcmnvc(e.vertex[i-2], e.vertex[i-1], lnvc);
					um_unitvc(lnvc, lnvc);
					um_nptln(&vx, e.vertex[i-1], lnvc, nvx);
					um_vcmnvc(&vx, nvx, lnvc);
					um_vcplvc(e.vertex[i-1], lnvc, &vx);
					}
	
				/* save the next point of the polygon */
				um_vctovc(&vx, e.vertex[i]);
				i++;
	
				/** a little visual feedback, if you please	**/
				if (i > 1)
					{
	
					tempseg[0].x = e.vertex[i-2][0];
					tempseg[0].y = e.vertex[i-2][1];
					tempseg[0].z = e.vertex[i-2][2];
	
					tempseg[1].x = e.vertex[i-1][0];
					tempseg[1].y = e.vertex[i-1][1];
					tempseg[1].z = e.vertex[i-1][2];
	
					gpolyline3( 2, tempseg);	/* drawn in no segment	*/
					}
				}
			}	while ((numint > 0) && (i < 200));

		e.numvtx = i;
		if (e.numvtx < 3)	/* too few vertices: exit	*/
			{
			if (e.numvtx > 0)
				uu_uerror0(UM_MODEL/* must have more than 2 vertices */, 174);
			goto done;
			}

		/* fix up start ppoint of polygon */
		if (((i/2)*2) != i)
			{
			um_vcmnvc(e.vertex[i-2], e.vertex[i-1], lnvc);
			um_unitvc(lnvc, lnvc);
			um_nptln(e.vertex[0], e.vertex[i-1], lnvc, nvx);
			um_vctovc(nvx, e.vertex[0]);
			um_vctovc(nvx, e.vertex[i-1]);
			}
		else
			{
			um_vcmnvc(e.vertex[0], e.vertex[1], lnvc);
			um_unitvc(lnvc, lnvc);
			um_nptln(e.vertex[i-1], e.vertex[0], lnvc, nvx);
			um_vctovc(nvx, e.vertex[0]);
			um_vctovc(nvx, e.vertex[i]);
			e.numvtx++;
			}

		e.fcolor = ur_get_dispattr_fillclr();

		um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uc_display(&e);
		}

done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_m40_modpolygon(key, option)
**       modify polyfill region
**    PARAMETERS   
**       INPUT  : 
**				key							UNIBASE key of polygon
**          option						1 => move polygon vertex
**												2 => change fill color
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int umu_m40_modpolygon(key, option)
	UU_KEY_ID key;
	int option;

	{
	int i, status;
	UM_PLOCREC	pick;						/* the pick id and location */
	int			numint;					/* the number user interactions	*/
	struct		UM_poly_rec	e;			/* polyfill region	*/
	int			index;					/* index of nearest point to 
												 * pick in e.vertex array
												 */
/**	UU_REAL		vx[3]; **/
    UD_NDCLOCREC vx;

	UM_coord		npt;
	UM_vector	vnorm;
	UU_REAL		dist;
	UU_REAL		space[2][3];
	int			dim, badent;
	NCL_cmdbuf cmdbuf;
	char buf[256];
	int output, modify;
	char Scolor[65][96];
	static char *lines[] = {"SOLID","DASH","DOTTED","CENTER","PHANTM",
									"DASHLN","DASHDT","DASHSP"};
	static char *wghts[] = {"STD","MEDIUM","HEAVY","EXHVY"};

	uu_denter(UU_MTRC,(us,"umu_m40_modpolygon(key=%x, option=%x)",key,option));

	strcpy(Scolor[0], "DEFALT");
	for (i=0; i<64;i++)
	{
		sprintf(Scolor[i+1], "%s", uw_color_name[i]);
	}
	switch  (option)
		{
		case 1:
			{
			um_dl_pldas( UD_DASPCKLOC, /* Pick a polyfill near vertex*/UM_MODEL,
					193, &pick, 1, &numint, 1);
			if (numint < 1) goto done;
	
			e.key = um_get_pickkey(&pick.pent, 1);
	
			um_get_all_geom(&e, sizeof(struct UM_poly_rec));
	
			/* -- find vertex nearest to pick	-- */
			index = um_nearest_to_ploc(&pick.ploc, e.numvtx, e.vertex);
			if (index < 0)
				goto done;
	
			/*	--	get new location for picked point	--	*/
	
			ud_ldas(UD_DASCART, /* New vertex of polygon */UM_MODEL, 192,
				&vx, 1, &numint, UD_NODEFAULT);
	
			if (numint < 1)
				goto done;
	
			/* FIX: check that points lie in plane?? */
			um_span_ptlist(e.numvtx, e.vertex, &dim, space, &badent);
			um_unitvc(space[1], vnorm);
			um_nptpln(&vx, e.vertex[0], vnorm, npt);
			dist = um_dcccc(&vx, npt);
			if (dist >= UM_FUZZ)
				{
				uu_uerror0(UM_MODEL, 59); /* point not in plane defined by polygon */
				goto done;
				}
			um_vctovc(&vx, e.vertex[index]);
	
			um_update_geom(&e, UM_DEFAULT_TF);
			uc_display(&e);
			}
			break;
		case 2:
			{
			int layer, color, line_styl, pen;
			UU_REAL line_width, line_weight;
			UU_LOGICAL modfield[5];

			e.key = key;
			um_get_all_geom(&e, sizeof(struct UM_poly_rec));
			output = 0;
			status = umu_attr_form(&layer, &color, &line_styl, &pen, &line_width,
						&line_weight, modfield, &output);
			if (status==-1)
				return -1;
			modify = 0;	
			for (i=0; i<5;i++)
			{
				if (modfield[i]) modify = 1;
			}
			if ((output) && (modify))
			{
				ncl_init_cmdbuf(&cmdbuf);
				strcpy(buf,"DRAFT/MODIFY=");	
				ncl_add_token(&cmdbuf, buf, NCL_nocomma);
				ncl_get_label(&e, buf);
				ncl_add_token(&cmdbuf, buf, NCL_comma);
			}

			if (modfield[0])
			{
				ur_update_layer(e.key,layer);
				if (output)
				{
					sprintf (buf, "LAYER=%d", layer);
					ncl_add_token(&cmdbuf, buf, NCL_comma);
				}
			}
			if (modfield[1])
			{
				e.fcolor = color;
				if (output)
				{
					sprintf (buf, "COLOR=%s", Scolor[color+1]);
					ncl_add_token(&cmdbuf, buf, NCL_comma);
				}
			}
			if (modfield[2])
			{
				ur_update_line_style(e.key,line_styl);
				if (output)
				{
					sprintf (buf, "LINTYP=%s", lines[line_styl]);
					ncl_add_token(&cmdbuf, buf, NCL_comma);
				}
			}
			if (modfield[3])
			{
				ur_update_line_weight(e.key,line_weight);
				if (output)
				{
					i = line_weight;
					sprintf (buf, "LINWGT=%s", wghts[i]);
					ncl_add_token(&cmdbuf, buf, NCL_comma);
				}
			}
			if (modfield[4])
			{
				ur_update_pen(e.key,pen);
				if (output)
				{
					sprintf (buf, "PEN=%d", pen);
					ncl_add_token(&cmdbuf, buf, NCL_comma);
				}
			}
			um_update_geom(&e, UM_DEFAULT_TF);
			uc_display(&e);
			if ((output) && (modify))
			{
				ncl_del_token(&cmdbuf,"", UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_set_cmdmode(UU_TRUE);
				ncl_call(&cmdbuf);
			}
			}
			break;
		default:
			break;
		}

done:
	uu_dexit;
	return(UU_SUCCESS);
	}
 
/*********************************************************************
**    E_FUNCTION :  umu_c40_inside_circle()
**       Create a regular N sided polygon inscribed within a circle.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_c40_inside_circle()

	{
	struct UM_poly_rec e;
	int numint;
	int i;
	int n;
	int status;
	UM_angle ang;
/**	UM_coord center; **/
    UD_NDCLOCREC center;

	UM_length radius;
	UM_transf rottf;

	uu_denter(UU_MTRC,(us,"umu_c40_inside_circle()"));

	ur_setup_data(UM_POLY_REL, &e, sizeof(struct UM_poly_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;
	while (UU_TRUE)		/* one iteration for each polygon	*/
		{
		ud_ldas(UD_DASCART, /* Center of circle */UM_MODEL, 319,
			&center, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		ud_ldas(UD_DASDISTANCE, /* Radius of circle */UM_MODEL, 15,
			&radius, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		ud_ldas(UD_DASINT, /* Number of sides of polygon */UM_MODEL, 320,
			&n, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		if (n < 3)	/* too few vertices: exit	*/
			{
			uu_uerror0(UM_MODEL/* must have more than 2 vertices */, 174);
			goto repeat;
			}

		um_vctmsc(UM_cpln.xaxis, radius, e.vertex[0]);
		um_vcplvc(&center, e.vertex[0], e.vertex[0]);

		ang = UM_TWOPI / n;
		if (((n/2)*2) == n)
			{
			um_rotlntf(&center, UM_cpln.zaxis, -ang/2.0, rottf);
			um_cctmtf(e.vertex[0], rottf, e.vertex[0]);
			}
		um_rotlntf(&center, UM_cpln.zaxis, ang, rottf);

		e.numvtx = 1;
		for (i=1; i<n; i++)
			{
			um_cctmtf(e.vertex[i-1], rottf, e.vertex[i]);
			e.numvtx++;
			}
		um_vctovc(e.vertex[0], e.vertex[e.numvtx]);
		e.numvtx++;
		for (i=e.numvtx; i<200; i++) um_vctovc(UM_zerovec, e.vertex[i]);

		e.fcolor = ur_get_dispattr_fillclr();

		status = uc_create_data(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		if (status == UU_SUCCESS)
			 uc_display(&e);

repeat:;
		}

done:
	uu_dexit;
	}
