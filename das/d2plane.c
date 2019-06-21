/*********************************************************************
**
**    NAME         :  d2plane.c
**       CONTAINS:
**				int ud_get_plane(subnum, errno1, plane, size, numint, flag)
**  			int ud_disp_arrow(origin, normal, transform)
**  			void ud_arrow(origin, normal_vec)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d2plane.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:05
*********************************************************************/

#include "usysdef.h"
#include "dtypes.h"
#include "dasnog.h"
#include "dmark.h"
#include "umath.h"
#include "mdcoord.h"
#include "mfeatcom.h"
#include "mdeval.h"
#include "modef.h"
#include "mdattr.h"
#include "udebug.h"
#include "uhep.h"
/*
.....Support picking an NCL plane - RAZ
*/
#include "mdpick.h"
#include "dselmask.h"
/*
.....Fix size of displayed arrow - RAZ
*/
#include "mdcpln.h"

/********************************************************************* 
**
**  E_FUNCTION	:  ud_get_plane(subnum, errno1, plane, size, numint, flag)
**      generalized front end to the DAS subystem to input planes
**
**  PARAMETERS   
**      INPUT:  subnum = prompt subsystem number
**					 errno1 = prompt number
**					 plane = input buffer
**					 size   = size of buffer in number of inputs of request
**								  type (e.g., 5 coordinates) except for choice
**								  as number of choices and string which is
**								  number of characters
**					 flag = if UD_DEFAULT then one exists
**      OUTPUT: numint = number of interactions actually input
**
**  RETURNS      :  status of operation
**							UU_TRUE if terminated with a "done"
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

int ud_get_plane(subnum, errno1, plane, size, numint, flag)
int subnum;							/* subsystem number */
int errno1;							/* prompt number */
UD_PLANE_REC *plane;				/* data return buffer */
int size ;							/* size of data buffer */
int *numint ;						/* actual number of interactions */
int flag;							/* if UD_DEFAULT then default exists */
{
	char *uu_uprompt0(), msg[100];
	UU_LOGICAL status, stat, ud_yesno();
	UU_LOGICAL arrow_disp;		/* arrow displayed flag */
	int segment;					/* segment number of arrow */
/**	UU_REAL coord[3];**/			/* local coordinate storage */
    UD_NDCLOCREC coord,tmp;

	int markval;
	int type;
	UM_PLOCREC pick;
	UU_KEY_ID key;
	UU_KEY_ID um_get_pickkey();

	uu_denter(UU_DTRC,(us,"enter ud_get_plane, subsys=%d, number=%d", 
							subnum, errno1));

/*	-- get the prompt -- */

	strcpy(msg, uu_uprompt0(subnum, errno1));
	ud_rpwrcom(msg);

/*	-- display the default vector -- */

	if(flag == UU_TRUE)
	{
		segment = ud_disp_arrow((*plane).org, (*plane).normal_vector,
										(*plane).transform);
		arrow_disp = UU_TRUE;
	}
	else
		arrow_disp = UU_FALSE;

	UD_MARK(markval, UU_FALSE);
	if(markval == 0)
	{

/*		-- prompt the operator for the interaction technique -- */

		ud_popupmenu(0, &type);

/*		-- delete the default vector -- */

		if(arrow_disp == UU_TRUE)
		{
			gdeleteseg(segment);
			arrow_disp = UU_FALSE;
		}

		switch(type)
		{

/*
.............Pick an NCL plane - RAZ
*/
			case 1:
				ud_lgeo(UU_TRUE, UD_ncl_pl);
				status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, 219, &pick, 1,
														numint, 1);
				if(*numint == 0)
					goto done;
				key = um_get_pickkey(&(pick.pent), 1);
				ncl_nclpln_to_pln(key, plane);
				(*plane).transform = 1;

				break;

/*				-- pick the three entities defining the plane -- */

			case 2:
				status = ud_ddas(UD_DASPLANE, msg, plane, size, numint, flag);
				if(*numint == 0)
					goto done;
				break;

/*				-- input point and vector -- */

			case 3:
				status = ud_ldas(UD_DASCART, UD_DASHEP, 68, &tmp,
						1, numint, flag);
                (*plane).org[0] = tmp.cord[0];
                (*plane).org[1] = tmp.cord[1];
                (*plane).org[2] = tmp.cord[2];

				if(*numint == 0)
					goto done;
				status = ud_ldas(UD_DASVEC, UD_DASHEP, 69, (*plane).normal_vector,
						1, numint, flag);
				if(*numint == 0)
					goto done;
				break;

/*			-- input plane origin by coordinate input, define vector normal 
				to plane -- */

			case 4:

/*				-- get the origin -- */

        coord.cord[0] = (*plane).org[0];  /* define coord because */
        coord.cord[1] = (*plane).org[1];  /* it is used as default */
        coord.cord[2] = (*plane).org[2];  /* when prompting user */

				status = ud_ldas(UD_DASCART, UD_DASHEP, 68, &coord, 
						1, numint, flag);
				if(*numint == 0)
					goto done;

/*				-- get the plane -- */

				status = ud_ddas(UD_DASPLANE, msg, plane, size, numint, flag);
				if(*numint == 0)
					goto done;

/*				-- move in the origin previously specified -- */

				(*plane).org[0] = coord.cord[0];
				(*plane).org[1] = coord.cord[1];
				(*plane).org[2] = coord.cord[2];
				break;

/*				-- use default -- */

			case 5:
				if(flag == UU_FALSE)
				{
					
/*					-- no default supplied -- */

					uu_uerror0(UD_DASHEP, 3);
					status = UU_TRUE;
					*numint = 0;
					goto done;
				}
				break;
			
			default:
				status = UU_TRUE;
				*numint = 0;
				goto done;
				break;
		}

/*		-- display the plane and ask if z axis needs to be flipped --*/

		do
		{

/*			-- display the arrow representing the vector -- */

			segment = ud_disp_arrow((*plane).org, (*plane).normal_vector,
										(*plane).transform);
			arrow_disp = UU_TRUE;

/*			-- ask operator if direction needs to be reversed -- */

			stat = ud_lyesno(UD_DASHEP, 67);
			if(stat == UU_TRUE)
				um_vctmsc((*plane).normal_vector, (UU_REAL) -1.0, (*plane).normal_vector);
	
			gdeleteseg(segment);
			arrow_disp = UU_FALSE;
		} while(stat == UU_TRUE);
	}

/*	-- delete the arrow if we are doing a longjump and it is displayed -- */

done:
	if(arrow_disp == UU_TRUE)
		gdeleteseg(segment);

	UD_promptsys = 0;
	UD_promptnum = 0;
	UD_UNMARK(markval);
	uu_dexit;
	return(status);
}

/********************************************************************* 
**  E_FUNCTION	:  int ud_disp_arrow(origin, normal, transform)
**      display an arrow normal to a plane at the desired position
**  PARAMETERS   
**      input:  origin - real array containing the start point
**              normal - real array to hold the three 
**                     components of the normal vector feature
**					 transform = normalization transform
**      output: none
**  RETURNS      :  segment id of arrow
**  SIDE EFFECTS :  displays the arrow(normal vector) at the desired
**                  screen position
**  WARNINGS     :  none
*********************************************************************/

int ud_disp_arrow(origin, normal, transform)
UU_REAL origin[]; 			/* real array containing the start point */
UU_REAL normal[]; 			/* real array to hold the normal vector components */
int transform;					/* normtran */
{
	int segment;					/* segment number arrow is in */
	void ud_arrow();

	uu_denter(UU_DTRC, (us, "enter ud_disp_arrow")); 

	segment = gnseg();
	gcreateseg(segment);
	gsnormtran(transform);

/*	-- set the color -- */

	gslinecolor(UM_WHITE);

/*	-- display the arrow in the segment -- */

	ud_arrow(origin, normal);

	gcloseseg();
	uu_dexit;
	return(segment);
}

/********************************************************************* 
**  E_FUNCTION:  void ud_arrow(origin, normal_vec)
**      display an arrow at the desired position
**  PARAMETERS   
**      input:  origin - real array containing the start point
**              normal_vec - real array to hold the three 
**                     components of the normal vector feature
**      output: none
**  RETURNS      : none
**  SIDE EFFECTS :  displays the arrow(normal vector) at the desired
**                  screen position
**  WARNINGS     :  none
*********************************************************************/

void ud_arrow(origin, normal_vec)
UU_REAL origin[]; 			/* real array containing the start point */
UU_REAL normal_vec[]; 		/* real array to hold the normal vector components */
{
	struct UM_rotmatrix matrix;
	UU_REAL ln_pt[6]; 		/* array to hold line points */
	UU_REAL theta; 			/* the yaw angle for the arrowhead calculations */
	UU_REAL tip[3];
	UU_REAL normal[3]; 
	UU_REAL unormal[3]; 
	UU_REAL unnormal[3];
	UU_REAL nnormal[3];
	UU_REAL mag_normal;
	UU_REAL del;
	UU_REAL pt[3];
	UU_REAL mvec1;
	UU_REAL mvec2;
	UU_REAL vptan[3];
	UU_REAL dvec1[3];
	UU_REAL dvec2[3];
	UU_REAL ref_normal[3];
	UU_LOGICAL flag;
	int i, j;
	UU_REAL UM_vpup[3];
	UU_REAL UM_vpnorm[3];
	UU_REAL len;

	uu_denter(UU_DTRC, (us, "enter ud_arrow")); 

	um_vctovc(UM_zaxis, UM_vpnorm);
	um_vctovc(UM_yaxis, UM_vpup);
	
/* -- unitize the normal vector -- */
	
	UM_len_inttoext((UU_REAL)1.0, len);
	um_unitvc(normal_vec, unormal);
	um_vctmsc(unormal, len, normal);
	
/* -- make the tail -- */
	
	um_vctovc(origin, ln_pt);
	um_vcplvc(origin, normal, &ln_pt[3]);
	
/* -- make the head, define the position of the arrowhead tip -- */

	um_vctovc(&ln_pt[3], tip);
	
/* -- define the third view plane normal vector -- */

	um_cross(UM_vpup, UM_vpnorm, vptan);
	
/* -- determine which view plane normal vector is nearest to 90
		degrees from the line normal vector -- */

	um_cross(unormal, UM_vpup, dvec1);
	um_cross(unormal, vptan, dvec2);
	mvec1 = um_mag(dvec1);
	mvec2 = um_mag(dvec2);
	
/* -- use view plane up normal vector -- */

	if(mvec1 > mvec2)
		um_vctovc(UM_vpup, ref_normal);

/* -- use view plane tangent normal vector -- */

	else
		um_vctovc(vptan, ref_normal);
	
/* -- define the normal to the viewing plane and the line -- */

	um_cross(unormal, ref_normal, nnormal);
	
/* -- calculate the normal vector normal to the line in the plane -- */

	um_cross(nnormal, unormal, unnormal);
	
/* -- get the magnitude of the normal vector -- */

	mag_normal = um_mag(normal);
	
/* -- define the delta distance for the offset -- */

	del = mag_normal / 3.0;
	
/* -- multiply the normal vector by the scalar delta -- */

	um_vctmsc(unormal, del, unormal);
	
/* -- subtract the scaled unit normal vector from the tip -- */

	um_vcmnvc(tip, unormal, pt);
	
/* -- redefine delta -- */

	del = 0.5 * del;
	
/* -- multiply the normal normal vector by the new delta -- */

	um_vctmsc(unnormal, del, nnormal);
	
/* -- get the first point for the arrowhead -- */

	um_vcplvc(pt, nnormal, pt);
	
/* -- calculate the x, y, z for other points around the line and set flag -- */

	flag = UU_TRUE;

/* -- set theta -- */

	theta = 0.;
	
/* -- display the tail segment -- */

	uu_dprint(UU_DTRC, (us, "ud_arrow: line=[%g, %g, %g], [%g, %g, %g]",
			ln_pt[0], ln_pt[1], ln_pt[2], ln_pt[3], ln_pt[4], ln_pt[5])); 
	gpolyline3(2, ln_pt);
	
/* -- display the arrowhead line segments.
		put the tip in as one end of all segments -- */

	um_vctovc(tip, ln_pt);
	
/* -- define the second end of the segment -- */

	for(i=0; i<3; i++)
	{
		um_vctovc(pt, &ln_pt[3]);
	
/* 	-- display the point -- */

		uu_dprint(UU_DTRC, (us, "ud_arrow: head=[%g, %g, %g], [%g, %g, %g]",
			ln_pt[0], ln_pt[1], ln_pt[2], ln_pt[3], ln_pt[4], ln_pt[5])); 

		gpolyline3(2, ln_pt);
	
/* 	-- calculate a new point rotated 120 degrees
			around the line. increment theta in radians -- */
	
		theta = theta + UM_PI/3;
	
/* 	-- rotate the point -- */
	
		um_rotatept(pt, normal, tip, theta, flag, &matrix);
	}
	uu_dexit;
}
