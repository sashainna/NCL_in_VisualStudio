

/*********************************************************************
**    NAME         :  m3edash.c
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3edash.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:53
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdattr.h"
#include "mcrv.h"
#include "mdeval.h"
#include "modef.h"
#include "mdebug.h"
#include "mplot.h"


/*********************************************************************
**    E_FUNCTION     : um_get_dash_line_params(line_style, tfmat, dash, gap)
**       Determine the DASH and GAP values for a dashed line.
**    PARAMETERS   
**       INPUT  : 
**          line_style				dashed line style
**				tfmat						transformation matrix 
**       OUTPUT :  
**          dash						dash length
**          gap						gap length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_get_dash_line_params(line_style, tfmat, dash, gap)
	int line_style;
	UM_transf tfmat;
	UU_REAL *dash;
	UU_REAL *gap;

	{
	*dash = UM_long_dash;
	*gap = UM_long_gap;
	}

/*********************************************************************
**    E_FUNCTION     : um_dash2_line(ptr,tfmat,attrptr)
**			Draw a dashed line (PTR, TFMAT) using the dash pattern
**			specified in the attribute record.
**    PARAMETERS   
**       INPUT  : 
**				ptr     			pointer to line record
**				tfmat				transformation matrix
**				attrptr			pointer to attribute record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_dash2_line(ptr,tfmat,attrptr)
	struct  UM_line_rec  *ptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;

	{
	int line_style;
	UM_coord spt;
	UM_coord ept;
	UM_vector vec;
	UM_vector lvec;
	UU_REAL dash;
	UU_REAL hdash;
	UU_REAL gap;
	UM_vector dash_vec;
	UM_vector gap_vec;
	UU_REAL min_pattern;
	UU_REAL max_pattern;
	UU_REAL arc_length;
	UU_REAL fraction;
	int num_pattern;
	int i;
	UM_coord line[2];

	uu_denter(UU_MTRC,(us,"um_dash2_line(key=%x,tfmat=%x,attrptr=%x)",
					ptr->key, tfmat, attrptr));

	/* set all of the display attribute for the line making sure that 
		the line style is solid */
	line_style = attrptr->line_style;
	attrptr->line_style = UM_SOLID_LINE;
	um_set_disp_attr(attrptr);
	attrptr->line_style = line_style;

	/* calculate the start and end points of the line in the resultant
		space */
	um_cctmtf(ptr->spt, tfmat, spt);
	um_cctmtf(ptr->ept, tfmat, ept);

	/* determine the length of the dash pattern in the resultant space */
	um_get_dash_line_params(line_style, tfmat, &dash, &gap);
	min_pattern = gap + dash;

	/* determine the integral number of dash patterns for the line */
	um_vcmnvc(ept, spt, vec);
	um_unitvc(vec, lvec);
	arc_length = um_mag(vec);
	num_pattern = (arc_length / min_pattern) + .5;
	um_vctovc(spt, line[0]);
	if (num_pattern >= 1)
		{
/*
		fraction = arc_length/(num_pattern * min_pattern) - 1.0;
		dash *= (1.0 + fraction);
		gap *= (1.0 + fraction);
		hdash = dash/2.0;
*/
		fraction = arc_length - (num_pattern * min_pattern);
		hdash = (dash + fraction)/2.0;
		um_vctmsc(lvec, hdash, dash_vec);
		um_vctmsc(lvec, gap, gap_vec);
		for (i=0; i<num_pattern; i++)
			{
			um_vcplvc(line[0], dash_vec, line[1]);
			gpolyline3(2, line);
			um_vcplvc(line[1], gap_vec, line[0]);
			if (i==0) um_vctmsc(lvec, dash, dash_vec);
			}
		}
	um_vctovc(ept, line[1]);
	gpolyline3(2, line);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_dash3_circle(ptr,tfmat,attrptr)
**			Draw a dashed circle (PTR, TFMAT) using the dash pattern
**			specified in the attribute record.
**    PARAMETERS   
**       INPUT  : 
**				ptr     			pointer to circle record
**				tfmat				transformation matrix
**				attrptr			pointer to attribute record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_dash3_circle(ptr,tfmat,attrptr)
	struct  UM_circle_rec  *ptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;

	{
	struct UM_circle_rec c;
	int line_style;
	UM_coord spt;
	UM_coord ept;
	UM_vector vec;
	UM_vector lvec;
	UU_REAL dash, dash_angle;
	UU_REAL hdash, hdash_angle;
	UU_REAL gap, gap_angle;
	UU_REAL total_angle;
	UM_transf rotmat;
	UU_REAL min_pattern;
	UU_REAL max_pattern;
	UU_REAL arc_length;
	UU_REAL fraction;
	int num_pattern;
	int i;
	UM_coord line[2];
	UU_LOGICAL  circle;

	uu_denter(UU_MTRC,(us,"um_dash3_circle(key=%x,tfmat=%x,attrptr=%x)",
					ptr->key, tfmat, attrptr));

	/* set all of the display attributes making sure that the line style
		is solid */
	line_style = attrptr->line_style;
	attrptr->line_style = UM_SOLID_LINE;

	/* copy the circle and transform the copy into the resultant space */
	uu_move_byte(ptr, &c, sizeof(c));
	um_tf3_tranfcirc(&c, tfmat, UU_FALSE);
	circle = fabs(fabs(c.dang) - UM_TWOPI) < UM_FUZZ;

	/* determine the length of the dash pattern (dash and gap), the minimum
		pattern size, and the intergral number of patterns which will be 
		displayed around the arc/circle */
	um_get_dash_line_params(line_style, tfmat, &dash, &gap);
	min_pattern = gap + dash;
	arc_length = fabs(c.dang) * c.radius;
	num_pattern = arc_length / min_pattern;

	/* determine the remainder and distribute this to the first and 
		last dash */
	if (!circle)
		{
		fraction = arc_length - (num_pattern * min_pattern);
		hdash = (dash + fraction)/2.0;
		}
	else
		{
		fraction = arc_length/(num_pattern * min_pattern) - 1.0;
		dash *= (1.0 + fraction);
		gap *= (1.0 + fraction);
		hdash = dash/2.0;
		}

	/* now, convert the arc length dash and gap values to an equivalent
		angle */
	dash_angle = dash / c.radius;
	hdash_angle = hdash / c.radius;
	gap_angle = gap / c.radius;
	if (c.dang < 0.0)
		{
		dash_angle = -dash_angle;
		hdash_angle = -hdash_angle;
		gap_angle = -gap_angle;
		}

	total_angle = 0.0;
	c.dang = hdash_angle;
	um_rottf(c.nvec, c.dang + gap_angle, rotmat);
	if ((circle && (num_pattern > 3)) || (!circle &&(num_pattern >= 1)))
		{
		for (i=0; i<num_pattern; i++)
			{
			um_drw3_circle(&c, UM_DEFAULT_TF, attrptr);
			total_angle += (c.dang + gap_angle);
			um_vctmtf(c.svec, rotmat, c.svec);
			if (i==0)
				{
				c.dang = dash_angle;
				um_rottf(c.nvec, c.dang + gap_angle, rotmat);
				}
			}
		}
	c.dang = ptr->dang - total_angle;
	um_drw3_circle(&c, UM_DEFAULT_TF, attrptr);

	/* restore dashed line style */
	attrptr->line_style = line_style;

	uu_dexit;
	}
