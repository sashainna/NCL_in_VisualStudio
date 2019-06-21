/*********************************************************************
**
**    NAME         :  d5filt.c
**
**       CONTAINS:
**    		  ud_filter_entity
**  		  ss_pickit
**    		  gotone
**    		  pick_select_verify - called from DIGS pick routines at low level
**....Added for enhanced picking.  -Roberta Zorzynski
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**			d5filt.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**			04/29/15 , 15:05:11
**
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "dselect.h"
#include "dinput.h"
#include "dselmask.h"
#include "gtbluni.h"
#include "gviw.h"
#include "gmat4.h"
#include "ginqxf.h"
#include "gtblvar6.h"
#include "vsegbf.h"
#include "umath.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mdcpln.h"
#include "udebug.h"
#include "mfort.h"
#include "nclfc.h"

#define FEATURE -1
#define NOTHING -2
#define VECTOR	 -3
#define PICKARPETURE (UU_REAL) .01
#define FMODE 0
#define PICKPTR (*event).indata.pickdata

static int Depthsave, Ppickpsave[5], Status, Type;

/*
.....Enhanced picking. User now specifies size of aperture...
.....Replaced PICKARPETURE with NCL_pick_aper.
.....Set initially in znuinit.c: znu_init_runvars().
.....Modified interactively through MODEL ENV->PICK APER.
.....Roberta Zorzynski.
*/
static UU_REAL Pdist;
extern UU_REAL NCL_pick_aper;
extern UU_KEY_ID NCL_verify_list[];
extern int NCL_pick_verify, NCL_nopick_cnt;
/*
.....Added new feature mask
.....Bobby  -  6/4/92
*/
extern UU_LOGICAL NCL_pick_feature;

/*********************************************************************
**
**    I_FUNCTION     :  gotone(len, segs, find)
**       procedure DIGS calls when item is picked
**
**    PARAMETERS   
**       INPUT  : 	len = length of segs
**							segs = segment numbers
**       OUTPUT :  	find = picked item record
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

static int 
gotone(len, segs, find)
int len;								/* length of segs */
int segs[];							/* segment nos. */
UG_findit *find;					/* pick item return buffer */
{
	int i;
	UD_PPICKREC picker;
	UU_KEY_ID mtid;
	int enttype;							/* entity type */
	uv_segbuff(udata);					/* user segment data buffer */
	UU_LOGICAL locstat;					/* local status cell */
	int um_d_pickresolve();
	UU_KEY_ID um_get_pickkey();
	UM_PICKENT pent;
	int stat;								/* status of um_d_pickresolve */

	uu_denter(UU_DTRC,(us,"gotone(len=%d, segs[0..1]=%d %d),pickid=%d",
				len,segs[0],segs[1],(*find).pickid));

	locstat = UU_FALSE;

/*	-- select a feature is a special case -- */

/*
.....Changed to use Limit Geo menu
.....instead of 'f' key
.....Bobby  -  6/4/92
*/
/*	if(Type == UM_FEAT_REL)*/
	if (NCL_pick_feature == UU_TRUE)
	{

/*		-- see if a feature was picked by checking segment name -- */

		gsegrud(segs[0], udata);
		if(uv_getrelnum(udata) == UM_FEAT_REL)
			locstat = UU_TRUE;
	}
	else
	{

/*		-- see if a feature was picked by checking segment name -- */

		gsegrud(segs[0], udata);
		if(uv_getrelnum(udata) == UM_FEAT_REL)
			locstat = UU_FALSE;
		else
		{

/*			-- move pertinent stuff in to a UD_PPICKREC record -- */

			picker.depth = len + 1;
			picker.pickpath[len] = (*find).pickid;
			for(i=0; i<len; i++)
				picker.pickpath[i] = segs[i];

			uu_dprint(UU_DTRC,(us,"in gotone, depth=%d, pickpath=%d,%d",
					picker.depth, picker.pickpath[0],picker.pickpath[1]));

/*			-- get the resolve mtid and access unibase for the entity type -- */

			stat = um_d_pickresolve(&picker, 1, &pent);
			if (stat == 0)
			{
				mtid = um_get_pickkey(&pent, 1);
				ur_retrieve_data_relnum(mtid, &enttype);
	uu_dprint(UU_DTRC,(us,"in gotone after unibase, mtid=%d, kind=%d, Type=%d",
		mtid,enttype,Type));
			}
			if(enttype == Type)
				locstat = UU_TRUE;
		}
	}

	if(locstat == UU_TRUE)
	{

/*		-- found the entity - save depth and pickpath -- */
/*
........If this found entity is closer than the last, store it.
........Improved picking.  RAZ.
*/
		if ((*find).dist < Pdist)
			{
			Status = UU_TRUE;
			Depthsave = len + 1;
			Ppickpsave[len] = (*find).pickid;
			for (i=0; i<len; i++)
				Ppickpsave[i] = segs[i];
			Pdist = (*find).dist;
			}
	}
/*	-- reset pick arpeture for next iteration -- */
/*
.....Using new user defined pick aperture. RAZ
*/

	(*find).epsx = NCL_pick_aper;
	(*find).epsy = NCL_pick_aper;

	uu_dexit;
	return 0;
}

/********************************************************************* 
**
**  I_FUNCTION:  UU_LOGICAL ss_pickit(loc, choice)
**      Find all graphics primitives within or without a
**			rectangle.
**
**  PARAMETERS   
**      INPUT:
**					 Gnpoint *loc -- center of search rectangle.
**					 int choice -- choice number used in pick
**      OUTPUT:  none
**
**  RETURNS      :  UU_TRUE if all went OK, UU_FALSE if nothing picked
**  SIDE EFFECTS : 	Depthsave - depth of pickid (0 means no pick)
**					 		Ppickpsave[] -- pickpath
**  WARNINGS     :  none
**
*********************************************************************/

static UU_LOGICAL ss_pickit(loc, choice)
UU_REAL *loc;				/* center of search rectangle */
int choice;					/* choice number used in select */
{

	Type = choice;
	Status = UU_FALSE;
	Depthsave = 0;
	Pdist = NCL_pick_aper;

/*	-- go initiate search -- */

/*
.....Use NCL_pick_aper as size of area to search when using momentary pick.
*/
	gfindndc(FMODE, loc, NCL_pick_aper, NCL_pick_aper, gotone, 0);
	return(Status);
}

/******************************************************************
**
**    I_FUNCTION     :  pick_select_verify(len, segs, find)
**       Called from DIGS (ug_findsgpk) when called from ug_dfindpk().
**       Checks a 'pick'd entity against current DAS geometry filter.
**       Modified version of gotone() routine above.
**.......New routine written by Roberta Zorzynski.
**
**    PARAMETERS   
**       INPUT  : 	len = length of segs
**							segs = segment numbers
**       OUTPUT :  	find = picked item record
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

int 
pick_select_verify(len, segs, find)
int len;								/* length of segs */
int segs[];							/* segment nos. */
UG_findit *find;					/* pick item return buffer */
	{
	int i;
	UD_PPICKREC picker;
	UU_KEY_ID mtid;
	int enttype;							/* entity type */
	uv_segbuff(udata);					/* user segment data buffer */
	UU_LOGICAL locstat;					/* local status cell */
	int um_d_pickresolve();
	UU_KEY_ID um_get_pickkey();
	UM_PICKENT pent;
	int stat;								/* status of um_d_pickresolve */

	locstat = UU_FALSE;

/*	-- select a feature is a special case -- */

/*
.....Changed to use the Geometry Limit menu
.....instead of the 'f' key, because we do not
.....know if the Feature Select Mask key was
.....pressed, until 'ud_filter_entity' is called.
.....Bobby  -  6/4/92
*/
/*	if(Type == UM_FEAT_REL)*/
	if (NCL_pick_feature == UU_TRUE)
		{

/*		-- see if a feature was picked by checking segment name -- */

		gsegrud(segs[0], udata);
		if(uv_getrelnum(udata) == UM_FEAT_REL)
			locstat = UU_TRUE;
		}
	else
		{
/*		-- see if a feature was picked by checking segment name -- */

		gsegrud(segs[0], udata);
		if(uv_getrelnum(udata) == UM_FEAT_REL)
			locstat = UU_FALSE;
		else
			{

/*			-- move pertinent stuff in to a UD_PPICKREC record -- */

			picker.depth = len + 1;
			picker.pickpath[len] = (*find).pickid;
			for(i=0; i<len; i++)
				picker.pickpath[i] = segs[i];

/*			-- get the resolve mtid and access unibase for the entity type -- */
			stat = um_d_pickresolve(&picker, 1, &pent);
			if (stat == 0)
				{
				mtid = um_get_pickkey(&pent, 1);
				ur_retrieve_data_relnum(mtid, &enttype);
				}
/*
............Check selection filter for valid types.
*/
			if ((UD_LIMIT.lsel == UU_TRUE) && (uu_tst_bit(UD_LIMIT.lselbf,enttype-1)))
				locstat = UU_TRUE;
/*
............If no filter in effect, all are valid.  Just pick the closest.
*/
			else if (UD_LIMIT.lsel == UU_FALSE)
				locstat = UU_TRUE;
			}
		}

/*
.....Explicitly state that the segment
.....did not pass the filter mask
.....Bobby  -  3/10/92
*/
	if (locstat != UU_TRUE)
	{
		find->found = 0;
	}
	else
	{
/*
........If user in verify mode, validate pick
*/
		if (NCL_pick_verify)
		{
			for (i = 0; i < NCL_nopick_cnt; i++)
			{
/*
................Ignore this one, previously picked.
*/
				if (mtid == NCL_verify_list[i])
				{
/*
.....Show that this segment cannot be picked
.....Bobby  -  2/25/92
*/
					find->found = 0;
					break;
				}
			}
/* 
............We have found something not to be ignored.
*/
			if (i == NCL_nopick_cnt)
			{
				find->found = 1;
				find->find = 0;
			}
		}
		else
		{
/*
........Tells digs to save this one as valid entity type.
*/
			find->found = 1;
			find->find = 0;
		}
/*
........If we did this, find->find = 0;, then digs would stop looking....
*/
	}
/*	-- reset pick arpeture for next iteration -- */
/*
.....Decrease the search area so that we only find things closer than this
.....one.
	(*find).epsx = (*find).dist;
	(*find).epsy = (*find).dist;
.....Use large search area....
*/
	(*find).epsx = NCL_pick_aper;
	(*find).epsy = NCL_pick_aper;

	return 0;
	}

/******************************************************************
**
**    I_FUNCTION     :  pick_select_verify(len, segs, find)
**       Checks a 'pick'd entity against current DAS geometry filter.
**       at same time check the verify mode. This routine used by 
**			openGL only cause it have different picking mothed
**       Create by Yurong
**
**    PARAMETERS   
**       INPUT  : 	
**							id: pick id
**							segno = segment numbers
**       OUTPUT :  	
**
**    RETURNS      : if this segment picked return 1
**							otherwise return 0;
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int 
ud_glpick_filter(depth, path, segno, id)
int *depth, segno, id;
int *path;
{
	UU_LOGICAL locstat;
	int i, tid, enttype;
	uv_segbuff(udata);
	UM_int2 primtyp;

	locstat = UU_FALSE;
	if (NCL_pick_feature == UU_TRUE)
	{
		gsegrud(segno, udata);
		if(uv_getrelnum(udata) == UM_FEAT_REL)
			locstat = UU_TRUE;
	}
	else if (ud_getpick_type() == UD_PICK_ASSIST)
		locstat = UU_TRUE;
	else
	{
		gsegrud(segno, udata);
		tid = uv_getkey(udata);
		enttype = uv_getrelnum(udata);
		if (enttype != UM_FEAT_REL)
		{
			if (UD_LIMIT.lsel == UU_FALSE) 
				locstat = UU_TRUE;
			else if (uu_tst_bit(UD_LIMIT.lselbf,enttype-1))
				locstat = UU_TRUE;
			if (locstat == UU_FALSE &&
			    uu_tst_bit(UD_LIMIT.lselbf,NCL_PLN_REL-1) &&
				(enttype==UM_RBSPLSRF_REL||enttype==NCL_SURF_REL||enttype==NCL_TRIMSF_REL))
			{
				ncl_get_sf_primtyp(&tid,&primtyp);
				if (primtyp == 3) locstat = UU_TRUE;
			}
/*
......check id the key limit
*/
			if (ud_islimit_entity(tid)==0)
				locstat = UU_FALSE;
		}
	}
	if (locstat != UU_TRUE)
		return locstat;
	else
	{
		if (NCL_pick_verify)
		{
			for (i = 0; i < NCL_nopick_cnt; i++)
			{
				if (tid== NCL_verify_list[i])
					return UU_FALSE;
			}
		}
	}
/*
....we put valid picking into path
*/
	if (locstat == UU_TRUE)
	{
		if (*depth==0)
		{
			*depth = *depth + 2;
			path[*depth-1] = id;
			path[*depth-2] = segno;
		}
		else if ((!NCL_pick_verify)&&(path[*depth-1]!=id)&&(path[*depth-2]!=segno))
		{
			*depth = *depth + 2;
			path[*depth-1] = id;
			path[*depth-2] = segno;
		}
	}
	return locstat;
}

/******************************************************************
**
**    I_FUNCTION     :  ud_glpick_alldisp(segneo,key)
**				Returns the key id of the segment.
**
**    PARAMETERS   
**       INPUT  : 	
**							segno = segment numbers
**       OUTPUT :  	
**							key   = key id of the segment
**    RETURNS      : if this is a valid pick  return 0
**							otherwise return 1;
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int ud_glpick_alldisp (segno, key)
int segno;
UU_KEY_ID *key;
{
	UU_LOGICAL locstat;
	int tid, enttype;
	uv_segbuff(udata);

	locstat = UU_FALSE;

		gsegrud(segno, udata);
		tid = uv_getkey(udata);
		enttype = uv_getrelnum(udata);

			if (UD_LIMIT.lsel == UU_FALSE) 
				locstat = UU_TRUE;
			else if (uu_tst_bit(UD_LIMIT.lselbf,enttype-1))
				locstat = UU_TRUE;


/*
....we put valid picking into path
*/
	if (locstat == UU_TRUE)
	{
		*key = tid;
		return (0);
	}
	else
		return (-1);
}

/*********************************************************************
**
**    E_FUNCTION     :  ud_filter_entity(event)
**       routine to filter pick events
**
**    PARAMETERS   
**
**       INPUT  : 
**          event = input pipe event buffer
**       OUTPUT :  
**          none
**
**    RETURNS      : filtered event in event buffer
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_filter_entity(event)
UD_DEVENT *event;
{
	int i;
	char pickchar;							/* character used to pick entity */
	UU_LOGICAL locstat;					/* local status cell */
	UU_KEY_ID mtid;
	int enttype;							/* entity type */
	uv_segbuff(udata);					/* user segment data buffer */
	UD_PPICKREC picker;
	UD_NDCLOCREC pndc;
	UU_REAL x, y, z, cord[3], cord1[3]; /* coordinate buffers */
	int um_d_pickresolve();
	UU_KEY_ID um_get_pickkey();
	UM_PICKENT pent;
	int stat;								/* status of um_d_pickresolve */
	int udata_type;						/* relation number from user data */

/*	-- see if pick was successfull -- */

	if(PICKPTR.depth > 0)
	{

/*		-- change a tablet pick to a blank character for now -- */

		if((*event).evdev != 1)
			PICKPTR.choice = ' ';

/*	-- locate momentary select character -- */

		pickchar = PICKPTR.choice;
		for(i=0; i<UD_num_momselect; i++)
		{
			if(pickchar == UD_momselect[i].selectchar)
			{
				Type = UD_momselect[i].relnum;
				break;
			}
		}

		if(i >= UD_num_momselect)
		{

/*			-- filter vector select -- */

			if(pickchar == UD_selvector)
				Type = VECTOR;
			else
				Type = NOTHING;
		}

		if(Type != NOTHING)
		{

/*		-- make the choice character a blank for ud_gevt -- */

			PICKPTR.choice = ' ';
			locstat = UU_FALSE;
			gsegrud(PICKPTR.pickpath[0], udata);
			udata_type = uv_getrelnum(udata);
/*
.....Changed to use Limit Geo menu
.....instead of 'f' key
.....Bobby  -  6/4/92
*/
/*			if(Type == UM_FEAT_REL)*/
			if (NCL_pick_feature == UU_TRUE)
			{

/*				-- see if a feature was picked by checking segment name -- */

				if(udata_type == UM_FEAT_REL)
					locstat = UU_TRUE;
			}
			else
			{

/*				-- see if a feature was picked by checking segment name -- */

				if(udata_type == UM_FEAT_REL)
					locstat = UU_FALSE;
				else
				{

/*	-- move pertinent stuff in to a UD_PPICKREC record -- */

					picker.depth = PICKPTR.depth;
					for(i=0; i<picker.depth; i++)
						picker.pickpath[i] = PICKPTR.pickpath[i];

uu_dprint(UU_DTRC,(us,"in ud_filter, depth=%d, pickpath=%d,%d",
			picker.depth, picker.pickpath[0],picker.pickpath[1]));

/*	-- get the resolve mtid and access unibase for the entity type -- */

					stat = um_d_pickresolve(&picker, 1, &pent);
					if (stat == 0) 
					{
						mtid = um_get_pickkey(&pent, 1);
						ur_retrieve_data_relnum(mtid, &enttype);
					}

uu_dprint(UU_DTRC,(us,"in ud_filter after unibase, mtid=%d, kind=%d, Type=%d",
			mtid,enttype,Type));

					if(Type != VECTOR)
					{
						if(Type == udata_type)
							locstat = UU_TRUE;
					}
					else

/*					-- if vector momentary select, make sure a line relation
						was picked, then convert to vector form. If some other
						entity was picked, leave the pick event alone. -- */

					{
						locstat = UU_TRUE;
/*-----
						if(enttype == UM_LINE_REL)
-----*/
						{
							pndc.transform = PICKPTR.transform;
							ug_mcopy(pndc.wndc3_mat, gqnormmat(pndc.transform));
							ug_invrt(pndc.ndcw3_mat, pndc.wndc3_mat);
							pndc.cord[0] = PICKPTR.position[0];
							pndc.cord[1] = PICKPTR.position[1];
							givref3(pndc.transform, cord);
							gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
							pndc.cord[2] = z;

/*				-- get the vector representation -- */

							uc_ploc_to_vector(2, &picker, &pndc, cord1);

/*							-- change the event record to vector type, and 
								convert to user coordinates system -- */

							(*event).evclass = UD_VECTOR;
							UM_cc_inttoext(cord1, (*event).indata.vecdata);
						}
					}
				}
			}

			if(locstat == UU_FALSE)
			{

/*				-- unhighlight this entity -- */

				gsseghilite(PICKPTR.pickpath[0], UG_NORMAL);

/*				-- go search the segment file for the entity -- */

				if(ss_pickit(PICKPTR.position, Type) == UU_TRUE)
				{

/*			-- go move in the new pick record -- */

					PICKPTR.status = UG_OK;
					PICKPTR.depth = Depthsave;
					PICKPTR.pickpath = Ppickpsave;
				}
				else
				{

/*					-- indicate nothing picked in event record -- */

					PICKPTR.status = UG_NONE;
					PICKPTR.depth = 0;
				}
			}
		}
	}
return 0;
}
