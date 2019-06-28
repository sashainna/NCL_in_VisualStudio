/*********************************************************************
**
**    NAME         :  d2hldcd.c
**
**       CONTAINS:
**  			ud_cart
**  			ud_cart1
**  			ud_cart2
**				ud_nrendpt
**          int ud_ddas_nearpt
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d2hldcd.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:04
**
*********************************************************************/

#include "usysdef.h"
#include "nclfc.h"
#include "dinput.h"      /* added for use define PLAYBACK etc. */  
#include "calcom.h"
#include "ddef.h"
#include "dasnog.h"
#include "dasg.h"
#include "mdcpln.h"
#include "mdpick.h"
#include "usysg.h"
#include "uims.h"
#include "mdunits.h"
#include "modef.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "gmat4.h"
#include "ginqxf.h"
#include "gtblvar6.h"
#include "mfort.h"
#include "dtypes.h"
#include "dselmask.h"
#include "nclvx.h"
#include "dselmask.h"
/*
.....Added by Paul to implement the "text" and "location" input. 08/20/92
*/
#include <ctype.h>
#include "class.h"
#include "uims.h"
#include "mdcoord.h"
#include "vsegbf.h"
#include "mcrv.h"
#include "driver.h" /*add for use UD_motif */
#define LOCREC (*event).indata.locdata
#define ZILCH(number) (fabs(number)<UM_FUZZ)
#define ENDSEG 		-1
#define UD_POLYMARKER 0
#define UD_POLYLINE   1
#define UD_FILLAREA 	 3


/* To set the RETURN key like use default. kathy */
extern UU_LOGICAL     dflag;
/*
.....For verify mode, reset to zero first time through picking routines
.....In case of previous reject op. - RAZ
*/
extern int NCL_nopick_cnt;
extern int UD_select_key;
static int NCL_pick_nearpt;

char *ud_uncord();
#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3
extern int NCL_mark_method;
static int UD_cart_part = 0;
extern int UR_active;
/********************************************************************* 
**
**  I_FUNCTION		:  ud_cart(prompt, ret_cord, defflag, def_cord, xflag)
**      coordinate high level DAS routine
**
**  PARAMETERS   
**      INPUT:  prompt = operator prompt string
**					 defflag = default exists flag
**					 def_cord = default coordinate
**					 xflag = NDC space flag if UU_FALSE
**      OUTPUT: ret_cord =  coordinate to return
**
**  RETURNS      : status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_cart(prompt, ret_cord, defflag, def_cord, xflag)
char *prompt;	 					/* operator prompt string  */
UD_NDCLOCREC *ret_cord;			/* coordinate to return  */
UU_LOGICAL defflag;				/* default exists flag */
UD_NDCLOCREC *def_cord;			/* default coordinate */
UU_LOGICAL xflag;					/* NDC flag (UU_TRUE=W.C, UU_FALSE=NDC) */
{

	UD_DASTAT status;						/* status return cell */
	UD_DASTAT ud_cart1();
	UD_DEVENT event;						/* event buffer */
	UU_REAL cord[3];						/* coordinate buffer */
	char pbuf[100];						/* character buffer */
	UD_EVENTDEF inparm;					/* string input parameters */
	char instring[256];

/* --- start of executable code --- */

	uu_denter(UU_DTRC,(us,"entering ud_cart, UD_locint=%d", UD_locint));

	/* To set up RETURN like use default key.  kathy */
	dflag = defflag;
/*
.....Reset verify list if have any
*/
	ud_reset_verify_list();
/*	-- set up the next prompt -- */

	if(defflag == UU_TRUE)
	{

/*		-- convert to construction coordinate system */

		cord[0] = (*def_cord).cord[0];
		cord[1] = (*def_cord).cord[1];
		cord[2] = (*def_cord).cord[2];

		if(xflag == UU_TRUE)
		{
			sprintf(pbuf, "%s %s [%s]",
				prompt, UD_synwc, ud_uncord(3, 8, cord));
		}
		else
		{
			sprintf(pbuf, "%s %s [<%g,%g,%g>]", 
				prompt, UD_synndc, cord[0], cord[1], cord[2]);
		}
	}
	else
	{
		if(xflag == UU_TRUE)
			sprintf(pbuf, "%s %s", prompt, UD_synwc);
		else
			sprintf(pbuf, "%s %s", prompt, UD_synndc);
	}

	do
	{
		if ((UD_cart_part) && (UD_locint==UD_STRING) && (event.indata.stringdata!=NULL))
		{
			strcpy(instring, event.indata.stringdata);
			inparm.defstr = instring;
			inparm.strbfsz = 256;
			ud_gevt(&event, UD_locint, pbuf, 1, UD_locdev, UD_locech, &inparm);
		}
		else
			ud_gevt(&event, UD_locint, pbuf, 1, UD_locdev, UD_locech, NULL);
		UD_cart_part = 0;
		status = ud_cart1(&event, ret_cord, defflag, def_cord, xflag);

/*		-- filter out coordinates that have a non zero z coordinate 
			if in drawing mode -- */

		if(UM_2d3d_mode == UM_2D && status == DE_TRUE)
			if(xflag == UU_TRUE)
				if(!ZILCH((*ret_cord).cord[2])) 
				{
					uu_uerror0(UD_DASHEP, 89);
					status = DE_AGAIN;
				}

	/* Test for number of coordinates where two are required. kathy
		if(NCL_2d3d_mode == UM_2D)
			if(xflag == UU_TRUE)
				if(!ZILCH((*ret_cord).cord[2])) 
				{
					uu_uerror0(UA_NCL, 8); NCL: changed to our system , more stable 
					status = DE_AGAIN;
				}*/
	}
	while(status == DE_AGAIN);

	UD_cart_part = 0;
	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION		:  ud_cart1(event, ret_cord, defflag, def_cord, xflag)
**      coordinate high level DAS routine
**
**  PARAMETERS   
**      INPUT:  event = event structure
**					 defflag = default exists flag
**					 def_cord = default coordinate
**					 xflag = NDC space flag if UU_TRUE
**      OUTPUT: ret_cord =  coordinate to return
**
**  RETURNS      : status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_cart1(event, ret_cord, defflag, def_cord, xflag)
UD_DEVENT *event;					/* event structure */
UD_NDCLOCREC *ret_cord;		/* coordinate to return  */
UU_LOGICAL defflag;				/* default exists flag */
UD_NDCLOCREC *def_cord;		/* default coordinate */
UU_LOGICAL xflag;					/* NDC flag (UU_TRUE=W.C., UU_FALSE=NDC) */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_auxm();
	int i;								/* temp variable */
	int ix, iy;							/* temp integers */
	UU_REAL dxx, dyy;					/* temp gridding */
	UU_REAL x, y, z;					/* coordinate storage */
	Gwpoint3 markpnt;					/* GKS structure for point echo on screen */
	UU_REAL cord[3], cord1[3];		/* Model coordinate structures */
	UD_NDCLOCREC ploc;				/* pickloc record for nearest endpoint */
	UD_PPICKREC picker;				/* pickloc record for nearest endpoint */
	UD_DASDATA dsda;					/* return buffer from kb coordinate parser */
	int stat;							/* status */
	UD_AREA *grafarea;				/* UIMS layout pointer */
	CRSLT stbptr;						/* symbol table entry pointer */
	UD_DASTAT ncl_verify_pick();
	char tempstr[80];	
	int assoc_buf,strln, len;
	UU_KEY_ID j, key;
	uv_segbuff(udata);
	struct UM_point_rec  e;
 
	UM_f77_str f77_str; 
 
	char strbuf[100], erms[256];


/* --- start of executable code --- */
/*
.....
..... Added by Paul for "Nested Command". We check this "label" field
..... In the "ncl_add_cord" routine. If it is not "      " - this is
..... the "Nested command". Paul. 08/26/92.
.....
*/
              strcpy(ret_cord->label,"      ");

	uu_denter(UU_DTRC,(us,"entering ud_cart1 xflag=%d, UD_locint=%d", 
								xflag, UD_locint));

	status = DE_TRUE ;
	(*ret_cord).choice = (*event).evclass;
	switch((*event).evclass)
	{
		case UD_LOCATOR:

/*			-- convert to world coordinates and echo the screen location -- */

/*----------- note that the GKS standard says coordinates are
				returned in world and not NDC coordinates------------*/

			givref3(LOCREC.transform, cord);
			gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
			x = LOCREC.position[0];
			y = LOCREC.position[1];

/*			-- make sure point is in graphics window if world coordinate -- */

			if(xflag == UU_TRUE)
			{
				grafarea = UD_duimsdeflt.screen[UD_curlayout.curr_screen].
								areas[UD_GRAF];
				if(x < (*grafarea).posn.ll.x ||
					x > (*grafarea).posn.ur.x ||
					y < (*grafarea).posn.ll.y ||
					y > (*grafarea).posn.ur.y)
				{
					uu_uerror0(UD_DASHEP, 84);
					goto retagain;
				}
			}
				
/*			-- convert from NDC on view surface to world coordinates -- */

			gsnormtran(LOCREC.transform);
			gndcw3(&x, &y, &z, x, y, z);

			uu_dprint(UU_DTRC,(us,"in ud_cart1, gndcw3 pt=%g,%g,%g", x,y,z));

			cord1[0] = x;
			cord1[1] = y;
			cord1[2] = z;

/*			-- project to construction plane only for model coordinates -- */

			if(xflag == UU_TRUE)
			{

/*			-- project to construction plane -- */

				if(uv_projcpln(cord1, cord1, LOCREC.transform) == UU_FAILURE)
				{

/*					-- contruction plane normal in this view -- */

					uu_uerror0(UD_DASHEP, 90);
					goto retagain;
				}

/*			-- convert back to construction coordinates and substitute active
					Z depth factor -- */

				um_vctmsc(UM_cpln.zaxis, UM_cpln.zdepth, cord);
				um_vcplvc(cord, cord1, cord1);
				x = cord1[0];
				y = cord1[1];
				z = cord1[2];

/*			-- if grid on then round coordinates -- */

				uu_dprint(UU_DTRC,(us,"ud_cart1 at grid, flag=%d, x=%g, y=%g, z=%g",
						UM_cpln.grid.snap,x,y,z));

				if(UM_cpln.grid.snap == UU_TRUE)
				{
					um_mcstoccs(0, cord1, cord);
					if(cord[0] < 0.)
						dxx = -UM_cpln.grid.dx;
					else
						dxx = UM_cpln.grid.dx;
					if(cord[1] < 0.)
						dyy = -UM_cpln.grid.dy;
					else
						dyy = UM_cpln.grid.dy;

/*
.....check for x,y grid user-input = 0.0
.....disallow snapping of axes to 0
.....Sharon - 6/12/91
*/
					if (UM_cpln.grid.dx != 0.0)
					{
						ix = (int)((cord[0] + dxx/2) / UM_cpln.grid.dx);
						cord[0] = ix * UM_cpln.grid.dx;
					}
					if (UM_cpln.grid.dy != 0.0)
					{
						iy = (int)((cord[1] + dyy/2) / UM_cpln.grid.dy);
						cord[1] = iy * UM_cpln.grid.dy;
					}
					um_ccstomcs(0, cord, cord1);

					x = cord1[0];
					y = cord1[1];
					z = cord1[2];

		uu_dprint(UU_DTRC,(us,"ud_cart1 at grid 1, x=%g, y=%g, dx=%g, dy=%g",
					x,y,UM_cpln.grid.dx,UM_cpln.grid.dy));
				}
			}

/*			-- echo point on the screen -- */

			markpnt.x = x;
			markpnt.y = y;
			markpnt.z = z;
			gsmarktype(1);
			gsmarkcolor(1);
			gpolymarker3(1, &markpnt);

			UD_wxhair.transform = LOCREC.transform;
			UD_wxhair.position.x = x;
			UD_wxhair.position.y = y;
			UD_wxhair.position.z = z;
			UD_GLOC3_W2NDC(&UD_nxhair, &UD_wxhair);
			gchglocinit(UD_ksws, UD_locdev, &UD_nxhair);

/*			-- set up default interactions -- */

			if((*event).evclass != UD_locint)
			{
				UD_locint = UD_LOCATOR;
				UD_locech = 1;
				UD_locdev = 1;
			}

			break ;

		case UD_VALUATOR:

/*---		"valuator input not allowed in coordinate DAS"  ----*/

			uu_uerror0(UD_DASHEP, 1);
			goto retagain;
			break ;

		case UD_PICK:

/*			-- read in picked geometry and get existing point or
				end point.  Set x, y, z -- */


/*			-- convert the location to world coordinates -- */

			ploc.transform = (*event).indata.pickdata.transform;
			givref3(ploc.transform, cord);
			gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
			ploc.cord[0] = (*event).indata.pickdata.position[0];
			ploc.cord[1] = (*event).indata.pickdata.position[1];
			ploc.cord[2] = z;
			ug_mcopy(ploc.wndc3_mat, gqnormmat(ploc.transform));
			ug_invrt(ploc.ndcw3_mat, ploc.wndc3_mat);

/*			-- get the closest endpoint to the pick location -- */

			picker.depth = (*event).indata.pickdata.depth;
			for(i=0; i<picker.depth; i++)
				picker.pickpath[i] = (*event).indata.pickdata.pickpath[i];

/*
.....restore picking area and ready to pick again or accept
*/
			ud_restore_pickarea();
/*
............VERIFY MODE: verify selected geometry before continuing
............We got here by toggling to 'by pick' from 'by loc'.
*/
/*
.....this changed to use other key function to deal with it
*/
			if (NCL_mark_method==DYNAMIC_MARK)
			{
				if (UD_select_key)
				{
					ncl_verify_pick2(&picker);
					ud_post_msg(2);
					UD_select_key = 0;
					status = DE_AGAIN;
				}
			}
			else
				status = ncl_verify_pick(&picker);
/*
			if ((ncl_verify_pick(&picker)) == DE_AGAIN)
				goto retagain;
*/
			if (status == DE_AGAIN)
				goto retagain;
			if(xflag == UU_TRUE)
				if (NCL_pick_nearpt)
					stat = ncl_ploc_to_coord(&picker, &ploc, cord);
				else
					stat = uc_ploc_to_coord(2, &picker, &ploc, cord);
			else
				stat = ud_nrendpt(2, &picker, &ploc, cord);

			if (stat == UU_SUCCESS)
			{
				uu_dprint(UU_DTRC,(us,"in ud_cart1 at nredpt, x=%g, y=%g, z=%g",
							cord[0],cord[1],cord[2]));
		
				if((*event).indata.pickdata.choice == 'd')
				{

/*					-- get delta vector -- */

					ud_ldas(UD_DASVEC, UD_DASHEP, 2, cord1, 1, &i, UD_NODEFAULT);

/*					-- add delta vector to base coordinate -- */

					um_vcplvc(cord, cord1, cord);
  				}

				x = cord[0] ;
				y = cord[1] ;
				z = cord[2] ;

/*				-- set up default interactions -- */

				if((*event).evclass != UD_locint)
				{
					UD_locint = UD_PICK;
					UD_locech = 1;
					UD_locdev = 1;
				}
			}
			else
				goto 	retagain;

			break ;
		case UD_CHOICE:

			if((*event).evdev == UD_AUXMENU)
			{
				status = ud_auxm(&(*event));

/*			-- check for default -- */

				if(status == DE_DEFAULT)
				{
					if(defflag == UU_TRUE)
					{

/*						-- set UD_nxhair and UD_wxhair if rubber band line 
							is active -- */

						if(xflag == UU_TRUE)
						{
							UD_wxhair.transform = 1;
							UD_wxhair.position.x = (*def_cord).cord[0];
							UD_wxhair.position.y = (*def_cord).cord[1];
							UD_wxhair.position.z = (*def_cord).cord[2];
							UD_GLOC3_W2NDC(&UD_nxhair, &UD_wxhair);
						}
						else
						{
							UD_nxhair.transform = 1;
							UD_nxhair.position.x = (*def_cord).cord[0];
							UD_nxhair.position.y = (*def_cord).cord[1];
							UD_nxhair.position.z = (*def_cord).cord[2];
							UD_GLOC3_NDC2W(&UD_wxhair, &UD_nxhair);
						}
						gchglocinit(UD_ksws, UD_locdev, &UD_nxhair);

						status = DE_TRUE;
						x = (*def_cord).cord[0];
						y = (*def_cord).cord[1];
						z = (*def_cord).cord[2];
					}
					else
					{

/*---					"no default in effect"  ----*/

						uu_uerror0(UD_DASHEP, 3);
						goto retagain;
					}
				}
			}
			else
			{

/*---			"inconsistent choice event in UD_DASCART"  ----*/
				uu_uerror0(UD_DASHEP, 4);
				goto retagain;
			}
			break ;

		case UD_STRING:
			len = strlen((*event).indata.stringdata);
			if ((len>0)&&((*event).indata.stringdata[len-1] == '\\'))
			{
/*
......remove "\" if partial string and goto do again to get next
*/
				(*event).indata.stringdata[len-1] = '\0';
				UD_cart_part = 1;
/*	
......set up default interactions
*/
				if((*event).evclass != UD_locint)
				{
					UD_locint = UD_STRING;
					UD_locech = 1;
					UD_locdev = 1;
				}
				goto retagain;
			}
/*
.....
..... If we are in CADD, string input like PTn is allowed,
..... but "Nested command" - not. If we are in CAM both PTn 
..... and "Nested command" are allowed. Paul. 08/25/92
.....
*/
			if (UU_application == UU_NCLCAM)
			{ 
				strln = strlen((*event).indata.stringdata);
/*
.....
..... If it is a "Nested point" do not check anything, only copy the
..... string to out buffer.
.....   
*/
				if((*event).indata.stringdata[0] == '(' &&
					(*event).indata.stringdata[strln-1] == ')' && strln <= 80)
				{
					strcpy(ret_cord->label,(*event).indata.stringdata);
					break;
				}
			}
/*
.....do not change (*event).indata.stringdata value
.....also we need check 6 chars of the keys
.....Yurong
*/
/***
			for (j=0; j<5; j++)
				(*event).indata.stringdata[j] =
				islower((*event).indata.stringdata[j]) ? toupper((*event).indata.stringdata[j])
				: (*event).indata.stringdata[j];
 
			UM_init_f77_str(f77_str,(*event).indata.stringdata,5);
*/
			strncpy(tempstr, (*event).indata.stringdata, 79);
			tempstr[79] = '\0';
	
			for (j=0; j<64; j++)
				tempstr[j] =
				islower(tempstr[j]) ? toupper(tempstr[j]) : tempstr[j];

			UM_init_f77_str(f77_str, tempstr, 64);
			getkey(UM_addr_of_f77_str(f77_str), &key);
 
			if(key == 0)
			{
				goto cont;
			}

			ur_retrieve_disp_segid(key, &assoc_buf);
			gsegrud(assoc_buf, udata);
			if(uv_getrelnum(udata) != 1) /* Not a point. See mdrel.h for defenitions. */
			{
				uu_uerror0(UD_DASHEP, 76);
				goto retagain;
			}
			e.key = key;
			ur_retrieve_data_fixed(&e);
			x = e.pt[0];
			y = e.pt[1];
			z = e.pt[2];
			UD_locint = UD_PICK;
			UD_locech = 1;
			UD_locdev = 1;
/*
.....Copy the GEO name into the label field
.....If it is a point.
.....Added for trimming operations.
.....Bobby  -  7/17/96
*/
			strncpy(ret_cord->label, e.label, NCL_MAX_LABEL);
			break;
cont:;
 /*
.....Get the scalar and convert to value
*/
			stat = ncl_parse_scalar_values((*event).indata.stringdata, strbuf, 0);
			strcpy(ret_cord->label, (*event).indata.stringdata);
			if (stat==-1)
			{
				sprintf(erms,"\"%s\" is not a valid scalar value",(*event).indata.stringdata);
				ud_wrerr(erms);
				goto retagain;
			}
 			if(xflag == UU_TRUE)
			{
 				if(ud_dasin(strbuf, &dsda, UD_DISTANCE)
 								==UU_FALSE)
 								goto retagain;
			}
			else
			{
 				if(ud_dasin(strbuf, &dsda, UD_UNITLESS)
 								==UU_FALSE)
 								goto retagain;
			}

			switch(dsda.dtype)
			{

/*				-- regular coordinate input; 
					convert to cartesian and return normally -- */

				case UD_DCOORDINATE:

					um_cotovc(dsda.stval.stcord.coord, dsda.stval.stcord.cordtype,
							cord);
					if(xflag == UU_TRUE)
					{
						um_ccstomcs(0, cord, cord1);
						x = cord1[0];
						y = cord1[1];
						z = cord1[2];
					}
					else
					{
						x = cord[0];
						y = cord[1];
						z = cord[2];

/*						-- check for unormalized coordinates if getting NDC -- */

						if( 	(x<0. || x>1.0) ||
								(y<0. || y>1.0) ||
								(z<0. || z>1.0) )
						{
							uu_uerror0(UD_DASHEP, 99);
							goto retagain;
						}
					}
					break;

/*				-- scalar input; error -- */

				case UD_DSCALAR:

/*					--	"scalar input not allowed in coordinate form" --*/

					uu_uerror0(UD_DASHEP, 5);
					goto retagain;
					break;

/*				-- delta point input -- */

				case UD_DELTAPOINT:

/*					-- convert to cartesion c.s. -- */

					um_cotovc(dsda.stval.stcord.coord, dsda.stval.stcord.cordtype,
							cord);

					if(xflag == UU_TRUE)
						um_ccstomcs(0, cord, cord);

					uu_dprint(UU_DTRC,(us,"ud_cart1 deltapt=%g,%g,%g",
						cord[0],cord[1],cord[2]));

/*					-- get the reference point -- */

					uq_gsymval("rp", &stbptr);
					cord1[0] = stbptr.val.cval[0];
					cord1[1] = stbptr.val.cval[1];
					cord1[2] = stbptr.val.cval[2];

					if(xflag == UU_TRUE)
					{
						um_ccstomcs(0, cord1, cord1);
						UM_cc_exttoint(cord1, cord1);
					}

					uu_dprint(UU_DTRC,(us,"ud_cart1 refpt=%g,%g,%g",
						cord1[0],cord1[1],cord1[2]));

					x = cord[0] + cord1[0];
					y = cord[1] + cord1[1];
					z = cord[2] + cord1[2];
					break;

/*				-- garbage input -- */

				default:

					if(((UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)||
						(UD_Rpstate[UD_Rpstate_ptr].flag==PLAYSUSP)))
					{	
						goto retagain;	
					}
					else
					{
/*---				"unrecognizable input in coordinate form"  ----*/

						uu_uerror0(UD_DASHEP, 6);
						goto retagain;
					}
			}

/*			-- set up default interactions -- */

			if((*event).evclass != UD_locint)
			{
				UD_locint = UD_STRING;
				UD_locech = 1;
				UD_locdev = 1;
			}
			break ;

		case UD_VECTOR:

			UM_cc_exttoint((*event).indata.vecdata, cord);
			um_ccstomcs(0, cord, cord);

			x = cord[0];
			y = cord[1];
			z = cord[2];

/*			-- set up default interactions -- */

			if((*event).evclass != UD_locint)
			{
				UD_locint = UD_PICK;
				UD_locech = 1;
				UD_locdev = 1;
			}

			break ;

		default:
/* 
... added for remove the error msg because it can't recognize the MENU
... input in playback file
... Yurong
*/
			if(((UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)||
				(UD_Rpstate[UD_Rpstate_ptr].flag==PLAYSUSP)))
			{	
				goto retagain;	
			}
			else
/*---		" invalid event in UD_DASCART"  ----*/
			{	uu_uerror0(UD_DASHEP, 7);
				goto retagain;
			}
			break ;
	}

/* -- return coordinate -- */

	if(status == DE_TRUE)
	{
		cord[0] = x;
		cord[1] = y;
		cord[2] = z;
		um_mcstoccs(0, cord, cord);
		UM_cc_inttoext(cord, cord1);
		uu_dprint(UU_DTRC,(us,"in ud_cart1 at rp, mcs = %g %g %g", x, y, z));
		uu_dprint(UU_DTRC,(us,"in ud_cart1 at rp, ccs = %g %g %g", 
					cord[0], cord[1], cord[2]));
		uu_dprint(UU_DTRC,(us,"in ud_cart1 at rp, rp = %g %g %g", 
					cord1[0], cord1[1], cord1[2]));

/*		-- set current point -- */
/*
.....seem there is no use for this one, it cause problem when 
.....in second unibase
.....Yurong
*/
		if (UR_active!=2)
			uq_addstbval("rp", UM_CARTESIAN, cord1);

		if(xflag == UU_TRUE)
		{
			(*ret_cord).cord[0] = x;
			(*ret_cord).cord[1] = y;
			(*ret_cord).cord[2] = z;
		}
		else
		{
/*			-- if the NDC coordinate was entered as a locator then go back to
				the original event record to get the coordinate, otherwise assume
				it was input as a string and converted to the wcs using
				normtran 0 -- */

			if((*event).evclass == UD_LOCATOR)
			{
				(*ret_cord).transform = LOCREC.transform;
				(*ret_cord).choice = LOCREC.choice;
				(*ret_cord).cord[0] = LOCREC.position[0];
				(*ret_cord).cord[1] = LOCREC.position[1];
				givref3(LOCREC.transform, cord);
				gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
				(*ret_cord).cord[2] = z;
				ug_mcopy((*ret_cord).wndc3_mat, gqnormmat((*ret_cord).transform));
				ug_invrt((*ret_cord).ndcw3_mat, (*ret_cord).wndc3_mat);
			}
			else if((*event).evclass == UD_PICK)
			{
				(*ret_cord).transform = (*event).indata.pickdata.transform;
				(*ret_cord).choice = (*event).indata.pickdata.choice;
				gwndc3(&(*ret_cord).cord[0], &(*ret_cord).cord[1], 
										&(*ret_cord).cord[2], x, y, z);
				ug_mcopy((*ret_cord).wndc3_mat, gqnormmat((*ret_cord).transform));
				ug_invrt((*ret_cord).ndcw3_mat, (*ret_cord).wndc3_mat);
			}
			else
			{
				(*ret_cord).transform = 1;
				(*ret_cord).choice = -1;
				(*ret_cord).cord[0] = x;
				(*ret_cord).cord[1] = y;
				(*ret_cord).cord[2] = 0.0;
				ug_mcopy((*ret_cord).wndc3_mat, gqnormmat((*ret_cord).transform));
				ug_invrt((*ret_cord).ndcw3_mat, (*ret_cord).wndc3_mat);
			}
		}
	}

	uu_dexit;
	return(status);

retagain:
	uu_dexit;
	return(DE_AGAIN);
}

/********************************************************************* 
**
**  I_FUNCTION		:  ud_cart2(prompt, ret_cord, defflag, def_cord, xflag)
**      coordinate high level DAS routine
**
**  PARAMETERS   
**      INPUT:  prompt = operator prompt string
**					 defflag = default exists flag
**					 def_cord = default coordinate
**					 xflag = NDC space flag if UU_FALSE
**      OUTPUT: ret_cord =  coordinate to return
**
**  RETURNS      : status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/
UD_DASTAT ud_cart2(prompt, ret_cord, defflag, def_cord, xflag)
char *prompt;	 					/* operator prompt string  */
UD_NDCLOCREC *ret_cord;			/* coordinate to return  */
UU_LOGICAL defflag;				/* default exists flag */
UD_NDCLOCREC *def_cord;			/* default coordinate */
UU_LOGICAL xflag;					/* NDC flag (UU_TRUE=W.C, UU_FALSE=NDC) */
{

	UD_DASTAT status;						/* status return cell */
	UD_DASTAT ud_cart1(),ud_pick1();
	UD_DEVENT event;						/* event buffer */
	UU_REAL cord[3];						/* coordinate buffer */
	char pbuf[100];						/* character buffer */
	UD_EVENTDEF inparm;					/* string input parameters */
	char instring[256];
	UD_PPICKREC ret_pck;
	struct UC_entitydatabag e;
    int dsegid;
    uv_segbuff(buffer);

/* --- start of executable code --- */

	uu_denter(UU_DTRC,(us,"entering ud_cart, UD_locint=%d", UD_locint));

	/* To set up RETURN like use default key.  kathy */
	dflag = defflag;
/*
.....Reset verify list if have any
*/
	ud_reset_verify_list();
/*	-- set up the next prompt -- */

	if(defflag == UU_TRUE)
	{

/*		-- convert to construction coordinate system */

		cord[0] = (*def_cord).cord[0];
		cord[1] = (*def_cord).cord[1];
		cord[2] = (*def_cord).cord[2];

		if(xflag == UU_TRUE)
		{
			sprintf(pbuf, "%s %s [%s]",
				prompt, UD_synwc, ud_uncord(3, 8, cord));
		}
		else
		{
			sprintf(pbuf, "%s %s [<%g,%g,%g>]", 
				prompt, UD_synndc, cord[0], cord[1], cord[2]);
		}
	}
	else
	{
		if(xflag == UU_TRUE)
			sprintf(pbuf, "%s %s", prompt, UD_synwc);
		else
			sprintf(pbuf, "%s %s", prompt, UD_synndc);
	}

	do
	{
		ud_lgeo (UU_TRUE, UD_ncl_pt);
		if ((UD_cart_part) && (UD_locint==UD_STRING) && (event.indata.stringdata!=NULL))
		{
			strcpy(instring, event.indata.stringdata);
			inparm.defstr = instring;
			inparm.strbfsz = 256;
			ud_gevt(&event, UD_locint, pbuf, 1, UD_locdev, UD_locech, &inparm);
		}
		else
			ud_gevt(&event, UD_locint, pbuf, 1, UD_locdev, UD_locech, NULL);
		UD_cart_part = 0;
		status = ud_cart1(&event, ret_cord, defflag, def_cord, xflag);
		if (event.evclass==UD_PICK)
		{
			status = ud_pick1(&event, &ret_pck);
			dsegid = ret_pck.pickpath[0];
			if (ud_isassist_seg(dsegid)==0)
			{
				gsegrud(dsegid,buffer);
				e.key = uv_getkey(buffer);
    			ur_retrieve_data_fixed(&e);
    			ncl_get_label(&e, ret_cord->label);
			}
			else
				ret_cord->label[0] = '\0';
		}
		if(UM_2d3d_mode == UM_2D && status == DE_TRUE)
		{
			if(xflag == UU_TRUE)
			{
				if(!ZILCH((*ret_cord).cord[2])) 
				{
					uu_uerror0(UD_DASHEP, 89);
					status = DE_AGAIN;
				}
			}
		}
		ud_lgeo (UU_TRUE, UD_ncl_pt);
	}
	while(status == DE_AGAIN);

	UD_cart_part = 0;
	uu_dexit;
	return(status);
}

/*********************************************************************
**
**    E_FUNCTION    : int ud_nrendpt(level, pickpath, pickloc, pt)
**      Get the cartesian coordinates of the nearest end point of a
**      DIGS segment to a picked location in NDC space.
**
**    PARAMETERS   
**       INPUT  : 
**				level							level of entity picked
**				picker						DAS pick path
**				pickloc						DAS pickloc record
**       OUTPUT :  
**				point							closest end point to entity
**
**    RETURNS      : 0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

int ud_nrendpt(level, picker, pickloc, point)
int level;
UD_PPICKREC *picker;
UD_NDCLOCREC *pickloc;
Gnpoint3 *point;
{
	Gnpoint3 best;					/* best point */
	UU_REAL (*coord)[3];
	int status;						/* status cell */
	UU_REAL um_dcccc();
	UU_REAL distance;				/* distance cell */
	UU_REAL bestdist;				/* best distance cell */
	int i;							/* for loop variable */
	int travpp[5], travdepth;	/* gtravnxt pickpath, depth */
	int type, count;				/* gtrav type and count parameters */
	UU_REAL locploc[3];			/* local pick location in model space */

	uu_denter(UU_DTRC, (us, "ud_nrendpoint(level=%d, pickpath=%x, pickloc=%x)", 
		level, picker, pickloc));

/*	-- initialize -- */

	best.x = 1., best.y = 1., best.z = 1.;
	bestdist = 10000000.;
	gsnormtran((*pickloc).transform);
	gndcw3(&locploc[0], &locploc[1], &locploc[2],
				(*pickloc).cord[0], (*pickloc).cord[1], (*pickloc).cord[2]);

/*	-- get the segment -- */

	gtrav((*picker).pickpath[(*picker).depth-2]);

	if(level == 1)
	{
		type = UD_POLYMARKER;
		while(type == UD_POLYMARKER)
			gtravnxt(&type, &count, &coord, travpp, &travdepth);

		status = UU_FAILURE;

		while(type == UD_POLYLINE || type == UD_FILLAREA)
		{
			status = UU_SUCCESS;

			distance = um_dcccc(coord[0], locploc);
			if(distance < bestdist)
			{
				best.x = coord[0][0], best.y = coord[0][1], best.z = coord[0][2];
				bestdist = distance;
			}
			distance = um_dcccc(coord[count-1], locploc);
			if(distance < bestdist)
			{
				best.x = coord[count-1][0], best.y = coord[count-1][1],
							best.z = coord[count-1][2];
				bestdist = distance;
			}
			gtravnxt(&type, &count, &coord, travpp, &travdepth);
		}
	}
	else if(level == 2)
	{
		type = UD_POLYMARKER;
		while(type == UD_POLYMARKER)
			gtravnxt(&type, &count, &coord, travpp, &travdepth);

		status = UU_FAILURE;

		while(type == UD_POLYLINE || type == UD_FILLAREA)
		{
			status = UU_SUCCESS;
			for(i=0; i<count; i++)
			{
				distance = um_dcccc(coord[i], locploc);
				if(distance < bestdist)
				{
					best.x = coord[i][0],best.y = coord[i][1],best.z = coord[i][2];
					bestdist = distance;
				}
			}
			gtravnxt(&type, &count, &coord, travpp, &travdepth);
		}
	}
	else
		status = UU_FAILURE;

	(*point).x = best.x, (*point).y = best.y, (*point).z = best.z;
	uu_dprint(UU_DTRC, (us, "ud_nrendpoint(status = %d, point=%g, %g, %g)", 
		status, (*point).x, (*point).y, (*point).z));

	uu_dexit;
	return(status);
}

/*********************************************************************
**
**    E_FUNCTION    : int ud_ddas_nearpt(prompt,nearpt,numint);
**      Call ud_ddas to get cartesian coordinate with global flag 
**      NCL_pick_nearpt set so picking an entity will return the 
**      point nearest to the pick instead of an end or corner point.
**
**    PARAMETERS   
**       INPUT  : 
**          prompt     - prompt.
**       OUTPUT :  
**          nearpt     - point selected.
**          numint     - Number of picks (0 or 1)
**
**    RETURNS      : 0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

int ud_ddas_nearpt(prompt,nearpt,numint)
char *prompt;
UM_coord nearpt;
int *numint;
{
	int status, hldflg;

	hldflg = NCL_pick_nearpt;
	NCL_pick_nearpt = 1;
	status = ud_ddas(UD_DASCART,prompt,nearpt,1,numint);
	NCL_pick_nearpt = hldflg;

	return (status);
}
