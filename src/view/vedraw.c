/*********************************************************************
**    NAME         :  vedraw.c
**       CONTAINS: Routines to interface viewing to DIGS
**			uv_set_defered_mode()
**			uv_set_immediate_mode()
**			uv_delsegs (first_dsegid)
**			uv_blanksegs (dsegid)
**			uv_unblanksegs (dsegid)
**			uv_setdetectable (dsegid,detect)
**			uv_dispobjs()
**			Gseg uv_drw(drw_routine, eptr, tfmat, attr,
**			uv_clear()
**			int uv_delvp(vport)
**			int uv_blankvp(vport, vis)
**			int uv_dispvp(vport)
**			int uv_autofact7_redrawvp(vport, view, drw_vp_info)
**			int uv_disp_entity(eprt)
**			uv_delete_hidden(vport)
**			uv_clear_segs_secondary()
**			uv_clear_segs()
**			uv_viewnum_key(viewnum, viewkey)
**			uv_drw_active_vp()
**			uv_redisp_label0(box,colorfg,colorbg,sizew,sizeh,ovrlpdis,ldrclr,arrow)
**			uv_redisp_label(box,colorfg,colorbg,sizew,sizeh,ovrlpdis,ldrclr,arrow)
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       vedraw.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:57
**************************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "zsysdep.h"
#include "usysg.h"
#include "vsegbf.h"
#include "g.h"
#include "class.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mdattr.h"
#include "mdebug.h"
#include "mderror.h"
#include "mdcpln.h"
#include "mattr.h"
#include "view.h"
#include "mromcom.h"
#include "unserve.h"
#include	"gerrorst.h"
#include	"uhep.h"
#include "gsegac.h"
#include "nclfc.h"
#include "nccs.h"
#include "lcom.h"
#include "nclmodals.h"
#include "driver.h"
#include "mpocket.h"

extern Gseg NCL_mot_seg;
extern int UZ_nclipv_view;
extern int UR_active;

void uv_delete_hidden();


#define TRACE UU_TRUE	/* for debugging only */

void uv_clear_segs_secondary();
void uv_clear_segs();
extern char UBopen[100];
extern int lub2;
extern int NCL_ldr_vp;
extern UU_KEY_ID NCL_ldr_vw;
static int Sactive_vp =-1;
extern int NCL_clipf;
/*********************************************************************
**    E_FUNCTION     : uv_set_defered_mode()
**       Set DIGS modification mode to NIVE (no immediate visual effect).
**			This causes some workstations to use block segment deleting.
**			It will prohibit workstations from redrawing deleted segments
**			in the background color.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_set_defered_mode()
{
	uu_denter(UU_MTRC,(us,"uv_set_defered_mode()"));
	gsdus(UD_ksws, UG_BNIG, UG_NIVE);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : uv_set_immediate_mode()
**       Set DIGS modification mode to UQUM (use quick update methods).
**			This has the effect of making some workstations delete segments
**			by redrawing them in the background color.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_set_immediate_mode()
{
	uu_denter(UU_MTRC,(us,"uv_set_immediate_mode()"));
	gsdus(UD_ksws, UG_BNIG, UG_UQUM);
	uu_dexit;
}

/**************************************************************************
**  E_FUNCTION:  uv_delsegs (first_dsegid)
**      delete all segs associated with an entity
**  PARAMETERS   
**      INPUT  : 
**				first_dsegid	: segment id that is stored with object in Unibase
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_delsegs (first_dsegid)
	Gseg first_dsegid;
{
	uv_segbuff(buffer);
	uv_segbuff(oldbuffer);
	int flg =0,rel_num,status;
	Gseg dsegid;
	UU_LOGICAL first;
	UU_KEY_ID view_key;
	Gseg predsegid,nextdsegid;

	uu_denter(UU_MTRC,(us,"uv_delsegs(%d)",first_dsegid));

	status = 0;
	dsegid = first_dsegid;
	first_dsegid = -1;
	first = UU_TRUE;
	while ((dsegid != -1) && (dsegid != 0) && (status == 0))
	{
		status = gsegrud(dsegid, buffer);
/*
.....for recalculation of leader lines, redisplay the entities in the 
.....current view only
*/
		if(Sactive_vp >=0)
		{
				view_key = uv_getviewkey(buffer);
				nextdsegid = uv_getsegid(buffer);
				if (NCL_ldr_vw == view_key)
				{
					if (first)
						ur_update_disp_segid(buffer[1], nextdsegid);
					else
					{
						gsegrud(predsegid, oldbuffer);
						uv_setsegid(nextdsegid, oldbuffer);
						gsegwud(predsegid, oldbuffer);
					}
					gdeleteseg(dsegid);
					break;
				}
				first = UU_FALSE;
				predsegid = dsegid;
				dsegid =  nextdsegid;
				continue;
		}
/*
.....if we are in plotting a drawing to plotter
.....we don't need erase segment in the screen.
.....only erase storage
.....Yurong 9/8/97
*/
		if (status == 0)
		{
			if (uj_miplotting() != UU_TRUE && um_is_pocket_graphics() != UU_TRUE) 
			{
/*
.....SET flags to blank out the label box
*/
				if(!NCL_toggle_off)
				{
					ur_retrieve_data_relnum(buffer[1], &rel_num);
					if(um_islabel_relation(rel_num)) 
					{
						NCL_labeldisp = UU_TRUE;
						NCL_toggle_off = UU_TRUE;
						flg =1;
					}
				}
				status = gdeleteseg(dsegid);
				if(flg)
				{
					flg =0;
					NCL_labeldisp = UU_FALSE;
					NCL_toggle_off = UU_FALSE;
				}
			}
			else
			{
				ug_segdel(dsegid);
				status = 0;
			} 
			if (status != 0)
			{
				sprintf(UM_sbuf,
					"uv_delsegs: error deleting display segment %d",dsegid);
				um_pscroll(UM_sbuf);
			}
			else
				dsegid = uv_getsegid(buffer);
		}
	}
	uu_dexit;
}

/**************************************************************************
**  E_FUNCTION:  uv_blanksegs (dsegid, ekey)
**      blank all segs associated with an entity
**  PARAMETERS   
**      INPUT  :  dsegid	: segment id that is stored with object in Unibase
**					ekey:	entity key
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_blanksegs (dsegid, ekey)
Gseg dsegid;
UU_KEY_ID ekey;
{
	UU_KEY_ID viewkey;
	UV_view view;
	int uc_draw();
/*
	int i;
	UV_vport vport;
	struct UC_entitydatabag e;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	int newdsegid;
	UU_LOGICAL	blanked;
	*/
	int rel_num;


	uv_segbuff(buffer);

	uu_denter(UU_MTRC,(us,"uv_blanksegs(%d)",dsegid));

	while (dsegid != -1)
	{
		gsegrud(dsegid, buffer);
		viewkey = uv_getviewkey(buffer);
		uv_getvid(viewkey, &view);
		if (view.vtype!=UV_INVISIBLE_VIEW)
		{
/*
.....SET flags to blank out the label box
*/
			ur_retrieve_data_relnum(ekey, &rel_num);
			if(um_islabel_relation(rel_num))
			{
				NCL_labeldisp = UU_TRUE;
				NCL_toggle_off = UU_TRUE;
			}
			gssegvis(dsegid,UG_INVISIBLE);
			NCL_labeldisp = UU_FALSE;
			NCL_toggle_off = UU_FALSE;
		}
		else
			gssegvis(dsegid,UG_VISIBLE);
		dsegid = uv_getsegid(buffer);
	}
/*
......above only invisble segment in visible view because the the segment 
......(visible segment before) in "invisible view" does not store in segment link 
*/
/*
.....update invisible view with new segments
*/
/*
......changed the drawing routine now to create all segment even though the entity is
......invisible in that view, so the blank/unblank just change the attrubute and don't
......need recreate the segment
......Yurong 10/28/05
*/
/********************************************************
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		if (view.vtype==UV_INVISIBLE_VIEW)
		{
			e.key = ekey;
			ur_retrieve_disp_segid(ekey, &dsegid);
			uc_retrieve_data(&e, sizeof(struct UC_entitydatabag));
			uc_retrieve_attr(ekey, &attr);
			ur_retrieve_blanked(ekey, &blanked);
			uc_retrieve_transf(ekey, tfmat);
			if (blanked==UG_INVISIBLE)
			{	
				ur_retrieve_data_relnum(e.key, &rel_num);
				if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
				{
					blanked = UG_VISIBLE;
					newdsegid = uv_drw(uc_draw, &e, tfmat, &attr, vport.xform,
											vport.cur_view, dsegid, blanked);
					ur_update_disp_segid(e.key, newdsegid);
					blanked = UG_INVISIBLE;
				}
			}
		}
	}
***************************************************************************/
	uu_dexit;
}

/**************************************************************************
**  E_FUNCTION:  uv_unblanksegs (dsegid, ekey)
**      unblank all segs associated with an entity
**  PARAMETERS   
**      INPUT  :  dsegid	: segment id that is stored with object in Unibase
**					ekey:	entity key
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_unblanksegs (dsegid, ekey)
Gseg dsegid;
UU_KEY_ID ekey;
{
	UU_KEY_ID viewkey;
	UV_view view;
	int uc_draw();
/*
	int i;
	UV_vport vport;
	struct UC_entitydatabag e;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	int newdsegid;
	UU_LOGICAL	blanked;
	int rel_num, layer, display;
	char tmpstr[20];
*/
	uv_segbuff(buffer);

	uu_denter(UU_MTRC,(us,"uv_unblanksegs(%d)",dsegid));

	while (dsegid != -1)
	{
		gsegrud(dsegid, buffer);
		viewkey = uv_getviewkey(buffer);
		uv_getvid(viewkey, &view);
		if (view.vtype!=UV_INVISIBLE_VIEW)
			gssegvis(dsegid,UG_VISIBLE);
		else
			gssegvis(dsegid,UG_INVISIBLE);
		dsegid = uv_getsegid(buffer);
	}
/*
......above only visible segment in invisible view 
*/
/*
.....update visible view with new segments
*/
/*
......changed the drawing routine now to create all segment even though the entity is
......invisible in that view, so the blank/unblank just change the attrubute and don't
......need recreate the segment
......Yurong 10/28/05
*/
/**********************************************************************************
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		display = 1;
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		if ((view.vtype!=UV_INVISIBLE_VIEW) &&
			(view.vtype!=UV_SECONDARY_VIEW))
		{
			if (view.vtype==UV_LAYER_VIEW)
			{
				strcpy(tmpstr, &(view.name[5]));
				layer = atoi(tmpstr);
				display = 2;
			}
			e.key = ekey;
			ur_retrieve_disp_segid(ekey, &dsegid);
			uc_retrieve_data(&e, sizeof(struct UC_entitydatabag));
			uc_retrieve_attr(ekey, &attr);
			ur_retrieve_blanked(ekey, &blanked);
			uc_retrieve_transf(ekey, tfmat);
			if (((display==2) && (attr.layer==layer) && (blanked==UG_VISIBLE))
				|| ((display==1) && (blanked==UG_VISIBLE)))
			{	
				ur_retrieve_data_relnum(e.key, &rel_num);
				newdsegid = uv_drw(uc_draw, &e, tfmat, &attr, vport.xform,
											vport.cur_view, dsegid, blanked);
				ur_update_disp_segid(e.key, newdsegid);
			}
		}
	}
*******************************************************************************/
	uu_dexit;
}
/**************************************************************************
**  E_FUNCTION:  uv_setdetectable (dsegid, detect)
**      Set detectable flag with all segs associated with an entity
**  PARAMETERS   
**      INPUT  :  dsegid	: segment id that is stored with object in Unibase
**                detect   : UU_TRUE if segment is detectable,
**                           UU_FALSE otherwise.
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_setdetectable (dsegid,detect)
Gseg dsegid;
UU_LOGICAL detect;
{
	uv_segbuff(buffer);
	while (dsegid != -1)
	{
		gsegrud(dsegid, buffer);
		gssegdet(dsegid,detect);
		dsegid = uv_getsegid(buffer);
	}
}

/**************************************************************************
**  E_FUNCTION:  uv_dispobjs()
**      Display all objects on the screen by querying Unibase for
**			all keys and then drawing them
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_dispobjs()
{
	struct UC_entitydatabag ent;
	UU_KEY_ID key;
	UU_LOGICAL init;
	UV_vport vport;
	UV_view view;
	int	i, second_view, work_view, uc_draw(), save_active=1; 

	uu_denter(UU_MTRC,(us,"uv_dispobjs()"));
/*
.....to see if the secondary view is active, if yes, we need display
.....objects in the secondary unibase too
*/
	work_view = 0;
	second_view = 0;
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		if (view.vtype==UV_SECONDARY_VIEW)
		{
			second_view = 1;
		}
		else
			work_view = 1;
	}
	save_active = UR_active;

	if (work_view)
	{
		if (UR_active == 2)
		{
			ur_getu_work();
		}
		init = UU_TRUE;
		while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
		{
			init = UU_FALSE;

			uu_dprint(UU_MTRC,(us,"uv_dispobj() creating segs for object %d",
							key));
			/* retrieve the object to be drawn */
			ent.key = key;
/*
.....vp 2/24/98 use ncl_retrieve_data_fixed for model
..... and uc_retrieve_data for drafting (sal convertion)
*/
			um_retrieve_data_fixed (&ent);

			/* draw the object */
			uc_display(&ent);
		}
	}
	if (second_view==0)
		goto done;
	if (!((lub2>0) && (UBopen[0]!='\0')))
		return;
	if (UR_active == 1)
	{
		ur_getu_second();
	}
	init = UU_TRUE;
	while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
	{
		init = UU_FALSE;
		ent.key = key;
		um_retrieve_data_fixed (&ent);
		uc_display(&ent);
	}
done:;
	if (save_active==2)
	{
		ur_getu_second();
	}
	else
		ur_getu_work();
	uu_dexit;
}
/**************************************************************************
**  E_FUNCTION:  uv_dispobjs2()
**      Display all objects on the secondary unibase by querying secondary Unibase for
**			all keys and then drawing them
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_dispobjs2()
{
	struct UC_entitydatabag ent;
	UU_KEY_ID key;
	UU_LOGICAL init;
	UV_vport vport;
	UV_view view;
	int	i, second_view, uc_draw(), save_active=1; 
/*
.....to see if the secondary view is active, if yes, we need display
.....objects in the secondary unibase too
*/
	second_view = 0;
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		if (view.vtype==UV_SECONDARY_VIEW)
		{
			second_view = 1;
			break;
		}
	}
	save_active = UR_active;
	if (second_view==0)
		goto done;
	if (!((lub2>0) && (UBopen[0]!='\0')))
		return;
	if (UR_active == 1)
	{
		ur_getu_second();
	}
	init = UU_TRUE;
	while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
	{
		init = UU_FALSE;
		ent.key = key;
		um_retrieve_data_fixed (&ent);
		uc_display(&ent);
	}
done:;
	if (save_active==2)
	{
		ur_getu_second();
	}
	else
		ur_getu_work();
	uu_dexit;
}


/**************************************************************************
**  I_FUNCTION:  Gseg uv_drw(drw_routine, eptr, tfmat, attr,
**							xform, viewkey, tempseg, blanked)
**			Create a DIGS display segment; set the current line color,
**			style and width, the current visibility, and the current marker
**			color using the specified attributes (ATTR); set the DIGS
**			normalization transformation (XFORM) and the segment pick
**			identifier (EPTR->key); then call the given drawing
**			routine (DRW_ROUTINE) to stroke out the graphical data
**			for the entity (EPTR, TFMAT, ATTR); finally set the segment
**			selectability, close the segment, and  update the user data.
**  PARAMETERS   
**      INPUT  :  eptr		: pointer to entity databag containing 
**									  entity to be drawn
**						tfmat		: transformation matrix for object
**						attr		: display attributes for object
**						xform		: norm tran used for segment
**						viewkey	: viewkey for segment
**						tempseg 	: previous segment in chain
**						blanked	: segment visibility
**      OUTPUT :  none
**  RETURNS      :  segment id used to draw this entity in this instance
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
Gseg uv_drw(drw_routine, eptr, tfmat, attr, xform, viewkey, tempseg, blanked)
	struct UC_entitydatabag *eptr;
	int (*drw_routine) ();
	UM_transf tfmat;						/* transformation matrix */
	struct UC_attributedatabag *attr;		/* display attributes */
	int xform;
	UU_KEY_ID viewkey;
	Gseg tempseg;
	UU_LOGICAL	blanked;
{
	int bsav;
	Gseg segid;
	Gseg gnseg();
 	uv_segbuff(buffer);					/* user segment data buffer */

	uu_denter(UU_MTRC, (us,
	"uv_drw(drw_routine=%x,ekey=%x,tf=%x,attr=%x,xform=%d,vkey=%x,tempseg=%d)",
		drw_routine,eptr->key,tfmat,attr,xform,viewkey,tempseg));

	/* get the next segment identifier and create a display segment with this 
		identifier */
/*
.....Disable background for Matrix text
*/
	bsav = UW_bkg_on; if (eptr->rel_num == 81) UW_bkg_on = 2;

	segid = gnseg();
	if( gcreateseg(segid) != NCL_NO_ERROR )
		uu_sys_err_recovery(/* Out of digs segments */ -1, UM_MODEL, 273, 0, 0);

	/* set segment visibility */
	gssegvis(segid,blanked);

	/* set the normalization transformation and pick id */
	gsnormtran(xform);

	gspickid(eptr->key);
	uu_dprint(UU_MTRC,(us,"before draw attr->selectable:%d",attr->selectable));

	/* call the drawing routine to display the graphical data in the current 
		open segment */
	(*drw_routine)(eptr,tfmat,attr);

	/* finally, close the segment, sets its selectability, and save the 
		system defined data in the "user" area */
/*
.....enable the label background 
*/
	if (um_islabel_relation(eptr->rel_num)) NCL_labeldisp = UU_TRUE;
	gcloseseg();
	NCL_labeldisp=UU_FALSE;
	UW_bkg_on = bsav;
	uu_dprint(UU_MTRC,(us,"attr->selectable:%d",attr->selectable));
	gssegdet(segid,attr->selectable);
	uv_setsegbuf(buffer, tempseg, eptr, attr, viewkey);
	gsegwud(segid,buffer);

	uu_dexit;
	return(segid);
}
/**************************************************************************
**  E_FUNCTION:  uv_clear()
**      Clear all segs used to display objects off the screen
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_clear()
{
	uv_clear_segs();
	uv_clear_segs_secondary();
}
/**************************************************************************
**  E_FUNCTION:  uv_clear_segs()
**      Clear all segs used to display objects off the screen
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_clear_segs()
{
	UU_KEY_ID key;
	int segid;
	UU_LOGICAL init;
	UG_segstli *segptr;

	uu_denter(UU_MTRC,(us,"uv_clear()"));

	/* set DIGS into defered update mode */
	uv_set_defered_mode();

	/* clear grid */
	um_inactgrid(UU_FALSE);

	/* clear features */
	um_feareset();

/*
.....Clear motion display
.....Bobby  -  9/1/92
*/
	segptr = ug_segac(NCL_mot_seg);
	if (segptr != NULL) gssegvis(NCL_mot_seg,UG_INVISIBLE);

	/* cycle through Unibase to find all objects */
	init = UU_TRUE;
	while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
	{
		init = UU_FALSE;

		/* get the first segment in the chain */
		ur_retrieve_disp_segid(key, &segid);
		if (segid != -1)
		{
			/* delete the chain of segments */
			uv_delsegs(segid);
			ur_update_disp_segid(key, -1);
		}
	}


	/* reset DIGS into immediate update mode */
	uv_set_immediate_mode();

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : int uv_delvp(vport)
**       Delete all of the display segments in the specified viewport
**			record.
**    PARAMETERS   
**       INPUT  : 
**          vport						view port entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_delvp(vport)
	UV_vport *vport;

{
	UU_LOGICAL init;
	UU_LOGICAL first;
	UU_KEY_ID key;
	UU_KEY_ID viewed_in;
	UU_KEY_ID view_key;
	UV_view view;
	Gseg dsegid;
	Gseg predsegid;
	Gseg nextdsegid;
	uv_segbuff(oldbuffer);
	uv_segbuff(newbuffer);
	int save_active = 1;

	uu_denter(UU_MTRC,(us,"uv_delvp(vpkey=%x)",vport->key));
/*
.....if the viewport we want delete is a secondary view, then we need
.....access secondary unibase to delete it
*/
	uv_getvid(vport->cur_view, &view);
	if (view.vtype==UV_SECONDARY_VIEW)
	{
		save_active = UR_active;
		ur_getu_second();
	}
	/* set DIGS into defered update mode */
	uv_set_defered_mode();

	/* delete all hidden lines */
	uv_delete_hidden(vport);

	/* clear grid */
	if (vport->key == UM_cpln.grid.vpid) um_inactgrid(UU_FALSE);

	/* clear features */
	um_feareset();

	init = UU_TRUE;
	while (uv_getobjs(init, &key, UU_FALSE) == 0)
	{
		init = UU_FALSE;
		ur_retrieve_view_key(key, &viewed_in);
		if ((viewed_in == 0) || (viewed_in == vport->cur_view))
		{
			ur_retrieve_disp_segid(key, &dsegid);
			first = UU_TRUE;
			while (dsegid != -1)
			{
				gsegrud(dsegid, newbuffer);
				view_key = uv_getviewkey(newbuffer);
				nextdsegid = uv_getsegid(newbuffer);
				if (vport->cur_view == view_key)
				{
					if (first)
						ur_update_disp_segid(key, nextdsegid);
					else
					{
						gsegrud(predsegid, oldbuffer);
						uv_setsegid(nextdsegid, oldbuffer);
						gsegwud(predsegid, oldbuffer);
					}
					gdeleteseg(dsegid);
					break;
				}
				first = UU_FALSE;
				predsegid = dsegid;
				dsegid =  nextdsegid;
			}
		}
	}

	/* reset DIGS into immediate update mode */
	uv_set_immediate_mode();
	 if (view.vtype==UV_SECONDARY_VIEW)
	 {
		if (save_active==2)
		{
			ur_getu_second();
		}
		else
			ur_getu_work();
	 }
	uu_dexit;
	return(UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int uv_blankvp(vport, vis)
**       Blank/unblank all of the display segments in the specified viewport
**			record.
**    PARAMETERS   
**       INPUT  : 
**          vport						view port entity
**				vis						UG_VISIBLE or UG_INVISIBLE	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_blankvp(vport, vis)
	UV_vport *vport;
	Gsegvis vis;

{
	UU_LOGICAL init;
	UU_KEY_ID key;
	UU_KEY_ID viewed_in;
	UU_KEY_ID view_key;
	UV_view view;
	Gseg dsegid;
	Gseg nextdsegid;
	uv_segbuff(newbuffer);
	UU_LOGICAL blanked;
	int save_active = 1;

	uu_denter(UU_MTRC,(us,"uv_blankvp(vpkey=%x,vis=%d)",vport->key,vis));

	uv_getvid(vport->cur_view, &view);
	if (view.vtype==UV_SECONDARY_VIEW)
	{
		save_active = UR_active;
		ur_getu_second();
	}

	/* set DIGS into defered update mode */
	uv_set_defered_mode();

	init = UU_TRUE;
	while (uv_getobjs(init, &key, UU_FALSE) == 0)
	{
		init = UU_FALSE;
		ur_retrieve_view_key(key, &viewed_in);
		if ((viewed_in == 0) || (viewed_in == vport->cur_view))
		{
			ur_retrieve_disp_segid(key, &dsegid);
			while (dsegid != -1)
			{
				gsegrud(dsegid, newbuffer);
				view_key = uv_getviewkey(newbuffer);
				nextdsegid = uv_getsegid(newbuffer);
				uv_getvid(view_key, &view);
				if (vport->cur_view == view_key)
				{
					ur_retrieve_blanked(key, &blanked);
					if (view.vtype!=UV_INVISIBLE_VIEW)
					{
						if (((vis == UG_VISIBLE) && (!blanked)) || (vis == UG_INVISIBLE))
							gssegvis(dsegid, vis);
					}
					else
					{
						if ((vis == UG_INVISIBLE) && (blanked))
						{
							vis = UG_VISIBLE;
							gssegvis(dsegid, vis);
						}
						else if (vis == UG_VISIBLE)
						{
							vis = UG_INVISIBLE;
							gssegvis(dsegid, vis);
						}
					}
				}
				dsegid =  nextdsegid;
			}
		}
	}

	/* reset DIGS into immediate update mode */
	uv_set_immediate_mode();
	 if (view.vtype==UV_SECONDARY_VIEW)
	 {
		if (save_active==2)
		{
			ur_getu_second();
		}
		else
			ur_getu_work();
	 }

	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int uv_dispvp(vport)
**       Display all of the displayable objects in the specified
**			viewport.
**    PARAMETERS   
**       INPUT  : 
**          vport						view port entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_dispvp(vport)
	UV_vport *vport;

{
	struct UC_entitydatabag e;
	struct UC_attributedatabag attr;
	UM_transf tfmat;
	UU_LOGICAL init;
	UU_KEY_ID viewed_in;
	int dsegid, display, layer;
	int newdsegid;
	int uc_draw();
	UV_view view;
	UU_LOGICAL	blanked;
	char tmpstr[20];
	int rel_num, save_active=1;

	uu_denter(UU_MTRC,(us,"uv_dispvp(vpkey=%x)",vport->key));

	/* display grid */
	if (vport->cur_view == UM_cpln.grid.viewid)
		um_actgrid(vport->key, vport->cur_view);

	uv_getvid(vport->cur_view, &view);
	if ((view.vtype==UV_SYSDEF_VIEW) || (view.vtype==UV_USERDEF_VIEW))
		display = 1;
	else if (view.vtype==UV_INVISIBLE_VIEW)
		display = 0;
	else if (view.vtype==UV_LAYER_VIEW)
	{
		strcpy(tmpstr, &(view.name[5]));
		layer = atoi(tmpstr);
		display = 2;
	}
	else
	{
		display = 3;
		if (!((lub2>0) && (UBopen[0]!='\0')))
			return UU_SUCCESS;
		if (UR_active == 1)
		{
			save_active = UR_active;
			ur_getu_second();
		}
	}

	init = UU_TRUE;
	while (uv_getobjs(init, &e.key, UU_FALSE) == 0)
	{
		init = UU_FALSE;
		ur_retrieve_view_key(e.key, &viewed_in);
		if ((viewed_in == 0) || (viewed_in == vport->cur_view))
		{
			ur_retrieve_disp_segid(e.key, &dsegid);
			uc_retrieve_data(&e, sizeof(struct UC_entitydatabag));
			uc_retrieve_attr(e.key, &attr);
			ur_retrieve_blanked(e.key, &blanked);
			uc_retrieve_transf(e.key, tfmat);
			if (display==1)
			{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*				if (blanked==UG_VISIBLE)            */
				{	
					newdsegid = uv_drw(uc_draw, &e, tfmat, &attr, vport->xform,
									vport->cur_view, dsegid, blanked);
					ur_update_disp_segid(e.key, newdsegid);
				}
			}
			else if (display==0)
			{
				if (blanked==UG_INVISIBLE)
				{	
					ur_retrieve_data_relnum(e.key, &rel_num);
					if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
					{
						blanked = UG_VISIBLE;
						newdsegid = uv_drw(uc_draw, &e, tfmat, &attr, vport->xform,
									vport->cur_view, dsegid, blanked);
						ur_update_disp_segid(e.key, newdsegid);
						blanked = UG_INVISIBLE;
					}
				}
				else
/*
......create the segment even though it is visible (not show in invisble view), 
......so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
				{
					ur_retrieve_data_relnum(e.key, &rel_num);
					if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
					{
						blanked = UG_INVISIBLE;
						newdsegid = uv_drw(uc_draw, &e, tfmat, &attr, vport->xform,
									vport->cur_view, dsegid, blanked);
						ur_update_disp_segid(e.key, newdsegid);
						blanked = UG_VISIBLE;
					}
				}
			}
			else if (display==2)
			{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*				if ((attr.layer==layer) && (blanked==UG_VISIBLE)) */
				if (attr.layer==layer)
				{
					newdsegid = uv_drw(uc_draw, &e, tfmat, &attr, vport->xform,
									vport->cur_view, dsegid, blanked);
					ur_update_disp_segid(e.key, newdsegid);
				}
			}
			else
			{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*				if (blanked==UG_VISIBLE) */
				{	
					newdsegid = uv_drw(uc_draw, &e, tfmat, &attr, vport->xform,
									vport->cur_view, dsegid, blanked);
					ur_update_disp_segid(e.key, newdsegid);
				}
			}
		}
	}
	if ((display==3) && (save_active==1))
		ur_getu_work();

	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int uv_autofact7_redrawvp(vport, view,
**										drw_vp_info)
**       Update the VIEW in UNIBASE and the viewport (VPORT) in
**			DIGS.
**    PARAMETERS   
**       INPUT  : 
**          vport							viewport entity
**          view							view entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uv_autofact7_redrawvp(vport, view, drw_vp_info)
	UV_vport *vport;
	UV_view *view;
	UU_LOGICAL drw_vp_info;

{

	uu_denter(UU_MTRC,(us,"uv_autofact7_redrawvp(vpkey=%x,vkey=%x,drw_vp=%d)",
		vport->key, view->key, drw_vp_info));
/*
.....Redraw IPV viewport
*/
	if (UZ_nclipv_view == 1)
	{
		um_set_screen_area(UM_IPV_WINDOW);
		uv_putv(view);
		uv_setxform(vport);
		ul_ipv_view_same(vport->xform);
	}
/*
.....Redraw NCL viewport
*/
	else
	{
		if (ur_get_dispattr_hidn_lines() == UU_TRUE)
			uv_blanksolinvp(vport, UG_VISIBLE);
		uv_putv(view);
		uv_updatevp(vport, drw_vp_info);

	}

	uu_dexit;
	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int uv_blanksolinvp(vport, vis)
**       Blank/unblank all of the display segments corresponding to 
**			solids in the specified viewport.
**			record.
**    PARAMETERS   
**       INPUT  : 
**          vport						view port entity
**				vis						UG_VISIBLE or UG_INVISIBLE	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uv_blanksolinvp(vport, vis)
	UV_vport *vport;
	Gsegvis vis;

{
	UU_LOGICAL init;
	UU_KEY_ID key;
	UU_KEY_ID viewed_in;
	UU_KEY_ID view_key;
	Gseg dsegid;
	Gseg nextdsegid;
	uv_segbuff(newbuffer);
	UU_LOGICAL blanked;
	UV_view view;

	uu_denter(UU_MTRC,(us,"uv_blanksolinvp(vpkey=%x,vis=%d)",vport->key,vis));

	/* set DIGS into defered update mode */
	uv_set_defered_mode();

	init = UU_TRUE;
	while ( uv_getalldispinrelation(init,UM_BODY_REL,&key) == UU_SUCCESS)
	{
		init = UU_FALSE;
		ur_retrieve_view_key(key, &viewed_in);
		if ((viewed_in == 0) || (viewed_in == vport->cur_view))
		{
			ur_retrieve_disp_segid(key, &dsegid);
			while (dsegid != -1)
			{
				gsegrud(dsegid, newbuffer);
				view_key = uv_getviewkey(newbuffer);
				nextdsegid = uv_getsegid(newbuffer);
				uv_getvid(view_key, &view);
				if (vport->cur_view == view_key)
				{
					ur_retrieve_blanked(key, &blanked);
					if (view.vtype!=UV_INVISIBLE_VIEW)
					{
						if (((vis == UG_VISIBLE) && (!blanked)) || (vis == UG_INVISIBLE))
							gssegvis(dsegid, vis);
					}
					else
					{
						if ((vis == UG_INVISIBLE) && (blanked))
						{
							vis = UG_VISIBLE;
							gssegvis(dsegid, vis);
						}
						else if (vis == UG_VISIBLE)
						{
							vis = UG_INVISIBLE;
							gssegvis(dsegid, vis);
						}
					}
				}
				dsegid =  nextdsegid;
			}
		}
	}

	/* reset DIGS into immediate update mode */
	uv_set_immediate_mode();

	uu_dexit;
	return(UU_SUCCESS);
}
/**************************************************************************
**  E_FUNCTION:  int uv_disp_entity(eptr)
**      Draw as many copies of the objects as are needed for the viewports
**			on the screen(s)
**  PARAMETERS   
**      INPUT  :  eptr	: pointer to an entity databage containing 
**								  entity to be drawn
**      OUTPUT :  none
**  RETURNS:  UU_SUCCESS if no problems encountered, UM_FAILURE otherwise.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int uv_disp_entity(eptr)
	struct UC_entitydatabag *eptr;
{
	Gseg tempseg;
	int i;
	int j;
	Gseg uv_drw();
	UU_KEY_ID onlyview;
	struct  UC_attributedatabag attr;	/* display attributes */
	UM_transf tfmat;							/* transformation matrix */
	UM_transf *tfp;
	int status, display, layer;
	int uc_draw();
	UU_LOGICAL	blanked;
	UV_vport    curvport;
	UV_view view;
	char tmpstr[20];
	int rel_num;
	

	uu_denter(UU_MTRC,(us,"uv_disp_entity(eptr->key:%x)", 
					 eptr->key));

	if (ur_retrieve_disp_segid(eptr->key, &tempseg) != 0)
		goto failed;
	/* delete any old segments containing this object */
	if (tempseg != -1)
	{
		uv_delsegs(tempseg);
/*
.....if in dynamic mouse is enabled get the updated seg id 
*/
		if(Sactive_vp<0)
			tempseg = -1;
		else
			ur_retrieve_disp_segid(eptr->key, &tempseg);
	}

	/* get display attributes */
	tfp = (UM_transf *)tfmat;
	if (uc_retrieve_transf(eptr->key, tfmat) != UU_SUCCESS)
		tfp = UU_NULL;
	if (uc_retrieve_attr(eptr->key, &attr) != UU_SUCCESS)
		goto failed;
	ur_retrieve_blanked(eptr->key, &blanked);
	if (attr.displayable != UM_DISPLAYABLE) goto failed;

	/* get the view that this entity is to be displayed in */
	if (ur_retrieve_view_key(eptr->key, &onlyview) != 0)
		goto failed;

	if (NCL_clipf)
	{
		tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
						0, 0, tempseg, blanked);
		ur_update_disp_segid(eptr->key, tempseg);
		return 0;
	}
	/* if only one workstation */
	if (UV_no_act_screens == 1)
	{

		/* if this object is only to be displayed in some views find
			the viewports that contain that view */
		if (onlyview != 0)
		{
			for (i = 0; i < UV_act_screen[0].nvports; i++)
/*
.....added shaded attribute here
.....yurong 12/30/98
*/
			{
/*
.....for recalculation of leader lines, redisplay the entities in the 
.....current view only
*/
				if(Sactive_vp>=0 && NCL_ldr_vp != UV_act_vports[0][i].xform)
					continue;
        		uv_getvpid(UV_act_screen[0].vports[i], &curvport);

				if ((curvport.disp_mode>3)||(curvport.disp_mode<=0))
					curvport.disp_mode = 2;

/*
.....if disp_mode = shaded
.....setup display mode 
*/
        		if (curvport.disp_mode==2)
				{
					ncl_set_shademode(1);
				}
				else
				{
					ncl_set_shademode(0);
				}
				if (curvport.disp_mode==3)
					ncl_set_hidline(1);
				else
					ncl_set_hidline(0);
				ncl_set_wireframe(curvport.wireframe);
				if (UV_act_vports[0][i].view == onlyview)
				{
					uv_getvid(onlyview, &view);
					if ((view.vtype==UV_SYSDEF_VIEW) || (view.vtype==UV_USERDEF_VIEW))
						display = 1;
					else if (view.vtype==UV_INVISIBLE_VIEW)
						display = 0;
					else if (view.vtype==UV_LAYER_VIEW)
					{
						strcpy(tmpstr, &(view.name[5]));
						layer = atoi(tmpstr);
						display = 2;
					}
					else
						display = 3;

					if ((display==1)&&(UR_active==1))
					{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*						if (blanked==UG_VISIBLE)   */
						{	
							tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
								UV_act_vports[0][i].xform, UV_act_vports[0][i].view,
								tempseg, blanked);
							ur_update_disp_segid(eptr->key, tempseg);
						}
					}
					else if ((display==0)&&(UR_active==1))
					{
						if (blanked==UG_INVISIBLE)
						{	
							ur_retrieve_data_relnum(eptr->key, &rel_num);
							if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
							{
								blanked = UG_VISIBLE;
								tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
									UV_act_vports[0][i].xform, UV_act_vports[0][i].view,
									tempseg, blanked);
								ur_update_disp_segid(eptr->key, tempseg);
								blanked = UG_INVISIBLE;
							}
						}
						else
/*
......create the segment even though it is visible (not show in invisble view), 
......so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
						{
							blanked = UG_INVISIBLE;
							tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
								UV_act_vports[0][i].xform, UV_act_vports[0][i].view,
								tempseg, blanked);
							ur_update_disp_segid(eptr->key, tempseg);
							blanked = UG_VISIBLE;
						}
					}
					else if ((display==2)&&(UR_active==1))
					{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
						if (attr.layer==layer)
						{
							tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
									UV_act_vports[0][i].xform, UV_act_vports[0][i].view,
									tempseg, blanked);
							ur_update_disp_segid(eptr->key, tempseg);
						}
					}
/*
......extern unibase view
*/
					else if ((display==3)&&(UR_active==2))
					{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*						if (blanked==UG_VISIBLE)           */
						{	
							tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
								UV_act_vports[0][i].xform, UV_act_vports[0][i].view,
								tempseg, blanked);
							ur_update_disp_segid(eptr->key, tempseg);
						}
					}
				}
			}
		}
		else
		{

		/* else display in all viewports that are marked for all display */
			for (i = 0; i < UV_act_screen[0].nvports; i++)
/*
.....added shaded attribute here
.....yurong 12/30/98
*/
			{
/*
.....for recalculation of leader lines, redisplay the entities in the 
.....current view only
*/
				if(Sactive_vp>=0 && NCL_ldr_vp != UV_act_vports[0][i].xform)
					continue;
        		uv_getvpid(UV_act_screen[0].vports[i], &curvport);

				if ((curvport.disp_mode>3)||(curvport.disp_mode<=0))
					curvport.disp_mode = 2;
/*
.....if disp_mode = shaded
.....setup display mode 
*/
        		if (curvport.disp_mode==2)
				{
					ncl_set_shademode(1);
				}
				else
				{
					ncl_set_shademode(0);
				}
				if (curvport.disp_mode==3)
					ncl_set_hidline(1);
				else
					ncl_set_hidline(0);
				ncl_set_wireframe(curvport.wireframe);
				if (UV_act_vports[0][i].disp_all == UU_TRUE)
				{
					uv_getvid(curvport.cur_view, &view);
					if ((view.vtype==UV_SYSDEF_VIEW) || (view.vtype==UV_USERDEF_VIEW))
						display = 1;
					else if (view.vtype==UV_INVISIBLE_VIEW)
						display = 0;
					else if (view.vtype==UV_LAYER_VIEW)
					{
						strcpy(tmpstr, &(view.name[5]));
						layer = atoi(tmpstr);
						display = 2;
					}
					else
						display = 3;

					if ((display==1)&&(UR_active==1))
					{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*						if (blanked==UG_VISIBLE)  */
						{	
							tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
								UV_act_vports[0][i].xform, UV_act_vports[0][i].view,
								tempseg, blanked);
							ur_update_disp_segid(eptr->key, tempseg);
						}
					}
					else if ((display==0)&&(UR_active==1))
					{
						if (blanked==UG_INVISIBLE)
						{	
							ur_retrieve_data_relnum(eptr->key, &rel_num);
							if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
							{
								blanked = UG_VISIBLE;
								tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
									UV_act_vports[0][i].xform, UV_act_vports[0][i].view,
									tempseg, blanked);
								ur_update_disp_segid(eptr->key, tempseg);
								blanked = UG_INVISIBLE;
							}
						}
						else
/*
......create the segment even though it is visible (not show in invisble view), 
......so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
						{
							ur_retrieve_data_relnum(eptr->key, &rel_num);
							blanked = UG_INVISIBLE;
							tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
									UV_act_vports[0][i].xform, UV_act_vports[0][i].view,
									tempseg, blanked);
							ur_update_disp_segid(eptr->key, tempseg);
							blanked = UG_VISIBLE;
						}
					}
					else if ((display==2)&&(UR_active==1))
					{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
						if (attr.layer==layer)
						{
							tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
								UV_act_vports[0][i].xform, UV_act_vports[0][i].view,
								tempseg, blanked);
							ur_update_disp_segid(eptr->key, tempseg);
						}
					}
/*
......extern unibase view
*/
					else if ((display==3)&&(UR_active==2))
					{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*						if (blanked==UG_VISIBLE) */
						{
							tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
								UV_act_vports[0][i].xform, UV_act_vports[0][i].view,
								tempseg, blanked);
							ur_update_disp_segid(eptr->key, tempseg);
						}
					}
				}
			}
		}
	}

	/* else there are multiple workstations */
	else
	{
		/* deactivate all workstations so output primitives go only
			to activated workstation */
		for (i = 0; i < UV_no_act_screens; i++)
			gdeactivatews(UV_act_screen[i].wstation);

		if (onlyview != 0)
		{
			for (j = 0; j < UV_no_act_screens; j++)
			{
				/* activate each workstation in order */
				gactivatews(UV_act_screen[j].wstation);

				/* if this object is only to be displayed in some views find
					the viewports that contain that view */
				for (i = 0; i < UV_act_screen[j].nvports; i++)
/*
.....added shaded attribute here
.....yurong 12/30/98
*/
				{
/*
.....for recalculation of leader lines, redisplay the entities in the 
.....current view only
*/
					if(Sactive_vp>=0 && NCL_ldr_vp != UV_act_vports[j][i].xform)
					continue;
      	  			uv_getvpid(UV_act_screen[j].vports[i], &curvport);

					if ((curvport.disp_mode>3)||(curvport.disp_mode<=0))
						curvport.disp_mode = 2;
/*
.....if disp_mode = shaded
.....setup display mode 
*/
   	     			if (curvport.disp_mode==2)
					{
						ncl_set_shademode(1);
					}
					else
					{
						ncl_set_shademode(0);
					}
					if (curvport.disp_mode==3)
						ncl_set_hidline(1);
					else
						ncl_set_hidline(0);
					ncl_set_wireframe(curvport.wireframe);
					if (UV_act_vports[j][i].view == onlyview)
					{
						uv_getvid(onlyview, &view);
						if ((view.vtype==UV_SYSDEF_VIEW) || (view.vtype==UV_USERDEF_VIEW))
							display = 1;
						else if (view.vtype==UV_INVISIBLE_VIEW)
							display = 0;
						else if (view.vtype==UV_LAYER_VIEW)
						{
							strcpy(tmpstr, &(view.name[5]));
							layer = atoi(tmpstr);
							display = 2;
						}
						else
							display = 3;

						if ((display==1)&&(UR_active==1))
						{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*							if (blanked==UG_VISIBLE)      */
							{	
								tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
									UV_act_vports[j][i].xform, UV_act_vports[j][i].view,
									tempseg, blanked);
								ur_update_disp_segid(eptr->key, tempseg);
							}
						}
						else if ((display==0)&&(UR_active==1))
						{
							if (blanked==UG_INVISIBLE)
							{	
								ur_retrieve_data_relnum(eptr->key, &rel_num);
								if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
								{
									blanked = UG_VISIBLE;
									tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
										UV_act_vports[j][i].xform, UV_act_vports[j][i].view,
										tempseg, blanked);
									blanked = UG_INVISIBLE;
									ur_update_disp_segid(eptr->key, tempseg);
								}
							}
							else
/*
......create the segment even though it is visible (not show in invisble view), 
......so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
							{
								ur_retrieve_data_relnum(eptr->key, &rel_num);
								blanked = UG_INVISIBLE;
								tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
										UV_act_vports[j][i].xform, UV_act_vports[j][i].view,
										tempseg, blanked);
								blanked = UG_VISIBLE;
								ur_update_disp_segid(eptr->key, tempseg);
							}
						}
						else if ((display==2)&&(UR_active==1))
						{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
							if (attr.layer==layer)
							{
								tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
									UV_act_vports[j][i].xform, UV_act_vports[j][i].view,
									tempseg, blanked);
								ur_update_disp_segid(eptr->key, tempseg);
							}
						}
/*
......extern unibase view
*/
						else if ((display==3)&&(UR_active==2))
						{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*							if (blanked==UG_VISIBLE)         */
							{
								tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
									UV_act_vports[j][i].xform, UV_act_vports[j][i].view,
									tempseg, blanked);
								ur_update_disp_segid(eptr->key, tempseg);
							}
						}
					}
				}

				/* deactivate the workstation */
				gdeactivatews(UV_act_screen[j].wstation);
			}
		}
		else
		{

			for (j = 0; j < UV_no_act_screens; j++)
			{
				/* activate each workstation in order */
				gactivatews(UV_act_screen[j].wstation);

				/* else display in all viewports that are marked for all display */
				for (i = 0; i < UV_act_screen[j].nvports; i++)
/*
.....added shaded attribute here
.....yurong 12/30/98
*/
				{
/*
.....for recalculation of leader lines, redisplay the entities in the 
.....current view only
*/
					if(Sactive_vp>=0 && NCL_ldr_vp != UV_act_vports[j][i].xform)
					continue;
      	  			uv_getvpid(UV_act_screen[j].vports[i], &curvport);

					if ((curvport.disp_mode>3)||(curvport.disp_mode<=0))
						curvport.disp_mode = 2;
/*
.....if disp_mode = shaded
.....setup display mode 
*/
   	     			if (curvport.disp_mode==2)
					{
						ncl_set_shademode(1);
					}
					else
					{
						ncl_set_shademode(0);
					}
					if (curvport.disp_mode==3)
						ncl_set_hidline(1);
					else
						ncl_set_hidline(0);
					ncl_set_wireframe(curvport.wireframe);
					if (UV_act_vports[j][i].disp_all == UU_TRUE)
					{
						uv_getvid(curvport.cur_view, &view);
						if ((view.vtype==UV_SYSDEF_VIEW) || (view.vtype==UV_USERDEF_VIEW))
							display = 1;
						else if (view.vtype==UV_INVISIBLE_VIEW)
							display = 0;
						else if (view.vtype==UV_LAYER_VIEW)
						{
							strcpy(tmpstr, &(view.name[5]));
							layer = atoi(tmpstr);
							display = 2;
						}
						else
							display = 3;

						if ((display==1)&&(UR_active==1))
						{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*							if (blanked==UG_VISIBLE)     */
							{	
								tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
									UV_act_vports[j][i].xform, UV_act_vports[j][i].view,
									tempseg, blanked);
								ur_update_disp_segid(eptr->key, tempseg);
							}
						}
						else if ((display==0)&&(UR_active==1))
						{
							if (blanked==UG_INVISIBLE)
							{	
								ur_retrieve_data_relnum(eptr->key, &rel_num);
								if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
								{
									blanked = UG_VISIBLE;
									tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
										UV_act_vports[j][i].xform, UV_act_vports[j][i].view,
										tempseg, blanked);
									ur_update_disp_segid(eptr->key, tempseg);
									blanked = UG_INVISIBLE;
								}
							}
							else
							{	
								ur_retrieve_data_relnum(eptr->key, &rel_num);
								if (rel_num != UM_COORDSYS_REL && rel_num != UM_LIGHT_REL)
								{
									blanked = UG_INVISIBLE;
									tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
										UV_act_vports[j][i].xform, UV_act_vports[j][i].view,
										tempseg, blanked);
									ur_update_disp_segid(eptr->key, tempseg);
									blanked = UG_VISIBLE;
								}
							}
						}
						else if ((display==2)&&(UR_active==1))
						{
							if (attr.layer==layer)
							{
								tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
										UV_act_vports[j][i].xform, UV_act_vports[j][i].view,
										tempseg, blanked);
								ur_update_disp_segid(eptr->key, tempseg);
							}
						}
/*
......extern unibase view
*/
						else if ((display==3)&&(UR_active==2))
						{
/*
......create the segment even though it is not visible, so when we unblank/blank
......segment, we don't need recreate the segments
......Yurong 10/28/05
*/
/*							if (blanked==UG_VISIBLE) */
							{
								tempseg = uv_drw(uc_draw, eptr, tfp, &attr,
									UV_act_vports[j][i].xform, UV_act_vports[j][i].view,
									tempseg, blanked);
								ur_update_disp_segid(eptr->key, tempseg);
							}
						}

					}
				}
				/* deactivate the workstation */
				gdeactivatews(UV_act_screen[j].wstation);
			}
		}

		for (i = 0; i < UV_no_act_screens; i++)
			gactivatews(UV_act_screen[i].wstation);
	}

/*	if (ur_update_disp_segid(eptr->key, tempseg) != 0)
		goto failed;
*/
	status = UU_SUCCESS;
	goto done;
failed: status = UM_FAILURE;
#if (TRACE) 
	UM_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION     : uv_delete_hidden(vport)
**       Delete the hidden lines segements in this view port, and return
**			the corresponding transform number to the number server.
**    PARAMETERS   
**       INPUT  : 
**          vport						view port entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_delete_hidden(vport)
UV_vport	*vport;
{

	int i;

	uu_denter(UU_MTRC,(us,"uv_delete_hidden(vpkey=%x)",vport->key));
	for (i=0; i<UM_MAX_HIDDEN_XFORMS; i++)
	{
		if ( UM_hidden.key[i] == (int)vport->key)
		{
			UM_hidden.key[i] = -1;
			uu_nserv_ret(UU_XFORM_NM, UM_hidden.xform[i]);
			UM_hidden.xform[i] = -1;
			gdeleteseg(UM_hidden.seg[i]);
			uu_dprint(UU_MTRC,(us,
					"uv_delete_hidden segement=%d",UM_hidden.seg[i]));
			UM_hidden.seg[i] = -1;
		}
	}
	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION     : uv_repaint(flag,viekey)
**       Repaints all screen or specified viewports only depending on
**       value of the first argument - "flag"                           
**    PARAMETERS
**       INPUT  :
**          flag:    -1 - Repaints all screen
**                    0 - repaints view specified in the viewkey param.
**                    1 - repaints view which number is specified in the  
**                        viewkey parameter.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uv_repaint(flag,viewkey)
int *flag;
int *viewkey;
{
int i;
UV_vport    curvport;           /* view port structure for the screen */

   if (*flag == -1) 
   {
      um_repaint();
      return(0);
   }
   if (*flag == 0)
   {
      for (i = 0; i < UV_act_screen[0].nvports; i++)
      {
        uv_getvpid(UV_act_screen[0].vports[i], &curvport);
        if(*viewkey  == (int)curvport.cur_view)
        {
          uv_updatevp(&curvport, UU_TRUE);
        }
      }
      return(0);
   }
   if (*flag == 1)
   {
      if (UV_act_screen[0].nvports < *viewkey ) return(0);
      uv_getvpid(UV_act_screen[0].vports[*viewkey-1], &curvport);
      uv_updatevp(&curvport, UU_TRUE);
      return(0);
   }
	return(0);
}
/*********************************************************************
**    E_FUNCTION     : uv_fit(flag,viekey, num, om_viewkey, onum, fsame)
**       Extrema zooms all screen or specified viewports depending on
**       value of the other arguments                           
**    PARAMETERS
**       INPUT  :
**          flag:    -1 - Zooms all screen
**                    0 - Zooms views specified in the viewkey params.
**			viekey:		Zooms views (array of views) to be zoomed
**			num:		number of views  to be zoomed
**			om_viewkey:		ignored views (array of views to not be changed
**			onum:		number of views  to not be zoomed
**			fsame:	=1:	setting all named views' scale to be 
**						same as the smallest one
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uv_fit(flag,viewkey,num, om_viewkey, onum, fsame)
int *flag, *num, *onum, *fsame;
UU_KEY_ID  *viewkey;
UU_KEY_ID  *om_viewkey;
{
	int i,j,k,modify;
	UV_vport    curvport;           /* view port structure for the screen */
	UV_vport new_vport[20];
	int new_num;
	UU_REAL aperture;

	if ((*num<=0) && (*flag != -1))
		return 0;
	new_num = 0;
	if (*flag == -1) 
	{
		for (i = 0; i < UV_act_screen[0].nvports; i++)
		{
			uv_getvpid(UV_act_screen[0].vports[i], &curvport);
			modify = 1;
			for (j=0; j<*onum;j++)
			{
				if(om_viewkey[j]  == curvport.cur_view)
				{
					modify = 0;
					break;
				}
			}
			if (modify)
			{
				uu_move_byte((char*)&curvport, (char*)&(new_vport[new_num]),
									sizeof(UV_vport));
				new_num++;
			}
		}
	}
	if (*flag == 0)
	{
		for (i = 0; i < UV_act_screen[0].nvports; i++)
		{
			uv_getvpid(UV_act_screen[0].vports[i], &curvport);
			for (j=0; j<*num;j++)
			{
				if(viewkey[j]  == curvport.cur_view)
					break;
			}
			if (j<*num)
			{
				modify = 1;
				for (k=0; k<*onum;k++)
				{
					if(om_viewkey[k]  == curvport.cur_view)
					{
						modify = 0;
						break;
					}
				}
				if (modify)
				{
					uu_move_byte((char*)&curvport, (char*)&(new_vport[new_num]),
									sizeof(UV_vport));
					new_num++;
				}
			}
		}
	}
	if (new_num<=0)
		return 0;
	if (*fsame)
	{
		uv_getzoom_large_apert(new_vport, new_num, &aperture);
	}
	for (j=0; j<new_num;j++)
	{
		uvu_curvpzoomextrema(&(new_vport[j]), *fsame, aperture);
	}
	return(0);
}
/*********************************************************************
**    E_FUNCTION     : uv_redisp_label0(box,colorfg,colorbg,
**												sizew,sizeh,ovrlpdis,ldrclr,arrow)
**       Set the label attributes according to the draft command.
**                      command *DRAFT/LABEL,BOX
**    PARAMETERS   
**       INPUT  : 
**                   box	:	backgorund box flag 1 - on, 2 - off
**					colorfg	:	label text color 0-17
**					colorbg	:	label background box color 0-17
**					sizew,sizeh:font size for label 
**					ovrlpdis	:	minimal overlap distance
**					ldrclr	:	leader line color 0-17
*					arrow	:	leader line arrow flag 1 - on, 2 - off 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_redisp_label0(box,colorfg,colorbg,sizew,sizeh,ovrlpdis,ldrclr,arrow)
int *box,*colorfg,*colorbg,*sizew,*sizeh,*ovrlpdis,*ldrclr,*arrow;
{
	struct UC_entitydatabag ent;
	UU_KEY_ID key;
	UU_LOGICAL init;
	int	uc_draw(); 
	struct  UC_attributedatabag attr;  
	int status, save_mask;

	init = UU_TRUE;

/*
......erase segment's label (with depth buffer set to off)
......and redraw
*/
#ifdef UU_OPENGL
	save_mask = uw_glget_depthmask();
	uw_gldepth_mask(0);
#endif
	while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
	{
		init = UU_FALSE;

		ent.key = key;
		um_retrieve_data_fixed (&ent);
/*
..... If label display is on
*/
		attr.key = ent.key;
		status = ur_retrieve_attr(&attr);
		if (status == UU_SUCCESS && ncl_get_label_on(attr.label_on))
		{
			NCL_display_label = UU_FALSE;
			attr.label_on = ncl_label_off(attr.label_on);
			ur_update_attr(&attr);
			uc_display(&ent);
/*
.....set the label on attributes but not display yet
.....display later after we reset label font size and color
*/
			attr.label_on = ncl_label_on(attr.label_on);
			ur_update_attr(&attr);
		}
	}
#ifdef UU_OPENGL
	uw_gldepth_mask(save_mask);
#endif
	if ((*sizeh>=0 && UW_label_size[0] != *sizeh) || (*sizew>=0 && UW_label_size[1] != *sizew))
	{
		UW_label_size[0] = *sizeh;
		UW_label_size[1] = *sizew;
#if UU_COMP == UU_WIN2K
		uw_ntcreate_font(-1);
#else
		uw_glupd_font();
#endif
	}
/*
.....set all the label attibutes
*/
	if (*colorfg>=0 && *colorfg!=UW_label_clr)
	{
		UW_label_clr = *colorfg;
		gstextcolor(UW_label_clr);
	}
	if(*box>=0 && *box!=UW_bkg_on)
		UW_bkg_on = *box;
	if(*colorbg>=0 && *colorbg!=UW_bkg_clr)
		UW_bkg_clr = *colorbg;
	if(*ovrlpdis>=0 && *ovrlpdis!=UW_overlap_dis)
		UW_overlap_dis = *ovrlpdis;
	if(*ldrclr>=0 && *ldrclr!=UW_ldr_clr)
		UW_ldr_clr = *ldrclr;
	if(*arrow>=0 && *arrow && *arrow!=UW_ldr_arrow)
		UW_ldr_arrow = *arrow;

/*
.....draw new segment with label
*/
	init = UU_TRUE;
	while (uv_getobjs(init, &key, UU_FALSE) == UU_SUCCESS)
	{
		init = UU_FALSE;

		ent.key = key;
		um_retrieve_data_fixed (&ent);
/*
..... If label display is on
*/
		attr.key = ent.key;
		status = ur_retrieve_attr(&attr);
		if (status == UU_SUCCESS && ncl_get_label_on(attr.label_on))
		{
			NCL_display_label = UU_TRUE;
			if(*colorfg >=0)
				UW_label_clr  = *colorfg;
			gstextcolor(UW_label_clr);
			uc_display(&ent);
		}
	}
/*
.....set UW_label_clr to labelclr because if labelclr = 0
.....UW_label_clr diff from entities and we only want save one color
*/
	if(*colorfg >=0)
		UW_label_clr  = *colorfg;
	if(*colorbg >=0)
		UW_bkg_clr  = *colorbg;
}
/**************************************************************************
**  E_FUNCTION:  uv_clear_segs_secondary()
**      Clear all segs used to display objects in the secondary unibase off the screen
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_clear_segs_secondary()
{
	int save_active =  UR_active;

	if (UR_active==1)
		ur_getu_second();
	uv_clear_segs();

	if (save_active==2)
	{
		ur_getu_second();
	}
	else
		ur_getu_work();
}
/*********************************************************************
**    E_FUNCTION     : uv_redisp_label(box,colorfg,colorbg,
**												sizew,sizeh,ldrclr,arrow)
**       Set the label attributes according to the draft command.
**                      command *DRAFT/LABEL,BOX
**    PARAMETERS   
**       INPUT  : 
**                   box	:	backgorund box flag 1 - on, 2 - off
**					colorfg	:	label text color 0-17
**					colorbg	:	label background box color 0-17
**					sizew,sizeh:font size for label 
**					ldrclr	:	leader line color 0-17
*					arrow	:	leader line arrow flag 1 - on, 2 - off 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uv_redisp_label(box,colorfg,colorbg,sizew,sizeh,ldrclr,arrow)
int *box,*colorfg,*colorbg,*sizew,*sizeh,*ldrclr,*arrow;
{
	UV_vport vport;
	UV_view view;
	int	i, second_view, work_view, save_active=1; 
/*
.....to see if the secondary view is active, if yes, we need display
.....label in the secondary unibase too
*/
	work_view = 0;
	second_view = 0;
	for (i=0; i<UV_act_screen[0].nvports; i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i], &vport);
		uv_getvid(vport.cur_view, &view);
		if (view.vtype==UV_SECONDARY_VIEW)
		{
			second_view = 1;
		}
		else
			work_view = 1;
	}
	save_active = UR_active;
	if (work_view)
	{
		if (UR_active == 2)
		{
			ur_getu_work();
		}
		uv_redisp_label0(box,colorfg,colorbg,sizew,sizeh,ldrclr,arrow);
	}
	if (second_view)
	{
		if (!((lub2>0) && (UBopen[0]!='\0')))
			return;
		if (UR_active == 1)
		{
			ur_getu_second();
		}
		uv_redisp_label0(box,colorfg,colorbg,sizew,sizeh,ldrclr,arrow);
	}
	if (save_active==2)
	{
		ur_getu_second();
	}
	else
		ur_getu_work();
}
/**************************************************************************
**  E_FUNCTION:  uv_viewnum_key(viewnum, viewkey)
**      Get the view key from the view number
**  PARAMETERS   
**      INPUT  :  viewnum: view number
**      OUTPUT :  viewkey: view key
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_viewnum_key(viewnum, viewkey)
int *viewkey, *viewnum;
{
	UV_vport    curvport;
	uv_getvpid(UV_act_screen[0].vports[*viewnum-1], &curvport);
	*viewkey = (int)curvport.cur_view;
}

/**************************************************************************
**  E_FUNCTION:  uv_drw_active_vp
**      Toggle static varialbe to draw entity only in active viewport
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
 uv_drw_active_vp()
{
	NCL_ldr_vp = 0;
	NCL_ldr_vw = 0;
	if(Sactive_vp <0) Sactive_vp=0;
	else Sactive_vp=-1;
	return 0;
}
