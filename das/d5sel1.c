/*********************************************************************
**
**    NAME         :  d5sel1.c
**
**       CONTAINS:
**    		  ud_reject
**    		  ud_single
**    		  ud_spstat
**    		  ud_limsss
**    		  ud_limsel
**            ud_setpick_type
**            ud_getpick_type
**            ud_verifypick_type
**            ud_motion_pick_type
**            ud_unhilight_pickseg
**            ud_restore_hilgt_seg
**            ud_get_pickstr
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d5sel1.c , 25.4
**    DATE AND TIME OF LAST MODIFICATION
**       03/02/17 , 10:32:43
**
*********************************************************************/

#include "usysdef.h"
#include "vsegbf.h"
#include "dasnog.h"
#include "dinput.h"
#include "dselect.h"
#include "dselmask.h"
#include "diconm.h"
#include "dselect.h"
#include "nclicons.h"
#include "nccs.h"
#include "view.h"
#include "uhep.h"
#include "udebug.h"
#include "mfort.h"
#include "nclfc.h"
#include "class.h" 
#include "udforms.h" 



#define UD_NULLPICKID -1					/* invalid pick id */
#define DYNAMIC_MARK 3

int UD_noselerr = 0;
UU_LOGICAL UD_hilite = UU_TRUE;			/* put up highlites? */

UU_LIST UD_limit_pikkey;
int UD_limit_pikkey_num = 0;

void ud_limsss(),ud_spstat();
extern char *UM_pocket_hwnd;
extern int UD_select_key;		
extern int UR_active;
extern int NCL_mark_method;
extern int UD_form_bypick;
extern int uw_glhicolor;

static int S_pick_type = UD_PICK_NORMAL;

/**************************************************************************
**
**  E_FUNCTION         : ud_limit_entity(flag, id) 
**      limit the geometry entity from picking
**
**  PARAMETERS   
**      input:  flag = if true then add this key into the limit picking list
**						false: remove this key from the limit picking list
**
**      output: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
void ud_limit_entity(flag, id)
UU_LOGICAL flag;
int id;
{
	int i, *pik_array;

	if (flag==UU_TRUE)
	{
		if (UD_limit_pikkey_num==0)
		{
			uu_list_init (&UD_limit_pikkey, sizeof(int), 25, 25);
			if (UD_limit_pikkey.data == UU_NULL) 
				return;
		}	
		uu_list_push(&UD_limit_pikkey,&id);
		UD_limit_pikkey_num++;
		return;
	}
	if (flag==UU_FALSE)
	{
		if (UD_limit_pikkey_num<=0)
			return;
		pik_array = (int *)UU_LIST_ARRAY(&UD_limit_pikkey);
/*
.....Delete the key from the list
*/
		for (i=0;i<UD_limit_pikkey_num;i++)
		{
			if (pik_array[i] == id)
			{
				uu_list_delete(&UD_limit_pikkey,i,1);
				UD_limit_pikkey_num--;
				break;
			}
		}
	}
}

/**************************************************************************
**
**  E_FUNCTION         : ud_islimit_entity(id) 
**     Check a entity to see if it is a limit entity.
** 
**  PARAMETERS
**      INPUT  :
**          id: id of entity to be checked
**      OUTPUT :
**          none
**
**  RETURNS      :  1: Yes; 0: No
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_islimit_entity(id) 
int id;
{
	int i;
	int *pik_array;
	if (UD_limit_pikkey_num==0)
		return 1;
	pik_array = (int *)UU_LIST_ARRAY(&UD_limit_pikkey);
	for (i=0; i<UD_limit_pikkey_num;i++)
	{
		if (pik_array[i]==id)
			return 1;
	}
	return 0;
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_reject
**       reject select handler in select subsystem
**
**    PARAMETERS   
**       INPUT  : 
**          ptr = select stack pointer to reject from
**          cnt = select counter to reject from
**       OUTPUT :  
**          none
**
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_reject(ptr, cnt)
int ptr;									/* ptr to reject from */
int cnt;									/* count to reject from */
{
	static int start = 0;
func_start:;
/*
.....add flag incase the UD_Select_ptr sets wrong
.....also avoid get into a loop
*/
	if ((start==0)||(start>1000))
	{
		start = 1;
/*
.....deselect for highlight picking surface
*/
		ud_desel_vpik();
/* --	set all entities selectable and not picked -- */

		ud_spstat(UU_TRUE, ptr);

/*		-- reset pick buffer -- */

		UD_Select_ptr = ptr;
		UD_Select_cnt = cnt;
		UM_pocket_hwnd = NULL;
		UD_select_key = 0;
		start = 0;
		ud_delete_assist_segs();
	}
	else
	{
		start++;
		goto func_start;
	}
}

/*********************************************************************
**
**    I_FUNCTION         :  ud_single(prompt,filter_on)
**       single select handler in select subsystem
**
**    PARAMETERS   
**       INPUT  : 
**          prompt = prompt string
**				filter_on = UU_TRUE, UU_FALSE
**       OUTPUT :  
**          none
**
**    RETURNS      : status
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/

UD_DASTAT ud_single(prompt, filter_on)
char *prompt;							/* prompt string */
UU_LOGICAL filter_on;				/* attribute filtering? */
{
	UD_DASTAT ud_pick();
	UD_DASTAT status;
	int baseptr,i;
	UD_PPICKREC prec;

	uu_denter(UU_DTRC,(us,"entering ud_single(%s, flag=%d)", prompt, filter_on));

/*
.....Allow for Reentrant call during selection process
.....Bobby  -  9/26/97
*/
/*	while((status = ud_pick(prompt, &UD_Select_buf[UD_Select_ptr])) == DE_TRUE)*/
	while((status = ud_pick(prompt, &prec)) == DE_TRUE)
	{
		uu_dprint(UU_DTRC,(us,"in ud_single, cnt=%d, ptr=%d,n=%d,pick=%d %d",
			UD_Select_cnt,UD_Select_ptr,UD_Select_buf[UD_Select_ptr],
			UD_Select_buf[UD_Select_ptr+1], UD_Select_buf[UD_Select_ptr+2]));

		UD_Select_buf[UD_Select_ptr] = prec.depth;
		for (i=1;i<=prec.depth;i++)
			UD_Select_buf[UD_Select_ptr+i] = prec.pickpath[i-1];

		baseptr = UD_Select_ptr;

/*	-- set the reject unselect pointer -- */

		UD_Selrej_ptr = UD_Select_ptr;
		UD_Selrej_cnt = UD_Select_cnt;

		UD_Select_ptr = UD_Select_ptr + UD_Select_buf[UD_Select_ptr] + 1;
		UD_Select_cnt = UD_Select_cnt + 1;

/*		-- Filter entities excluded by DAS limit or attribute filter -- */

		ud_limsss(baseptr, filter_on, UU_NULL);
		ud_spstat(UU_FALSE, baseptr);
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_spstat(flag, baseptr)
**       set or reset pickablity status of selected entities, and
**			highlight entities to feedback to operator
**
**    PARAMETERS   
**       INPUT  : 
**          flag = UU_TRUE for set pickable, UU_FALSE for unpickable
**				baseptr = start location into select buffer
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : adds entities to global select list
**    WARNINGS     : none
**
*********************************************************************/

void ud_spstat(flag, baseptr)
UU_LOGICAL flag;								/*  set / reset flag */
int baseptr;									/* buffer pointer */
{
	UU_KEY_ID key_id;
	uv_segbuff(buff);
	int locbaseptr,save_active=1;
	int n, segno, stat;

	uu_denter(UU_DTRC,(us,"entering ud_spstat(%d %d %d)", 
			flag, baseptr, UD_Select_ptr));

	locbaseptr = baseptr;
	while(locbaseptr < UD_Select_ptr)
	{
		n = UD_Select_buf[locbaseptr];
		segno = UD_Select_buf[locbaseptr + n - 1];

		if(segno != UD_NULLPICKID)
		{
/*			-- get first seg in chain -- */

			stat = gsegrud(segno, buff);
			if (stat!=0)
				break;
			key_id = uv_getkey(buff);
			save_active =  UR_active;
			if (ud_getpick_type()==UD_PICK_SECONDARY)
				ur_getu_second();
			ur_retrieve_disp_segid(key_id, &segno);
			if (save_active==1)
				ur_getu_work();

			while (segno != UD_NULLPICKID)
			{
				uu_dprint(UU_DTRC,(us,"ud_spstat: while loop segno = %d", segno));
				if(flag == UU_TRUE)
				{
					gssegdet(segno, UG_DETECTABLE);
					gsseghilite(segno, UG_NORMAL);
				}
				else
				{
/*					-- set undetectable only if multiple picks not allowed -- */
/* 				-- hilite only if UD_hilite is true -- */
	
					if(UD_Sellim_mp == UU_TRUE)
						gssegdet(segno, UG_UNDETECTABLE);
					if(UD_hilite == UU_TRUE)
						gsseghilite(segno, UG_HIGHLIGHTED);
				}

/*				--	Get next seg in chain. Segno will be UD_NULLPICKID at 
					end of chain -- */

				stat = gsegrud(segno, buff);
				if (stat!=0)
				{
					segno = UD_NULLPICKID;
					break;
				}
				segno = uv_getsegid(buff);

			} 	/* end of while segno != UD_NULLPICKID */
		}	/* end of if segno != UD_NULLPICKID */

/*		-- bump to next item -- */

		locbaseptr = locbaseptr + n + 1;
	}
	uu_dexit;
	return;
}

/*********************************************************************
**
**    E_FUNCTION :  ud_limsss(basptr, filter_on, viewkey)
**			filter select buffer of entities excluded by a DAS limit 
**			or by the attribute filter
**
**    PARAMETERS   
**       INPUT  : 
**          baseptr = base pointer into select buffer
**				filter_on = UU_TRUE, UU_FALSE
**				viewkey = viewkey to filter on; UU_NULL for no filtering
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

void ud_limsss(baseptr, filter_on, viewkey)
int baseptr;					/* base pointer into select buffer */
UU_LOGICAL filter_on;		/* attribute filtering? */
UU_KEY_ID viewkey;			/* view key to filter on */
{

	UU_LOGICAL	ud_limsel();
	int locbaseptr;
	int i, n, segno, got_one = UU_FALSE;

	uu_denter(UU_DTRC,(us,"in limsss = %d", UD_LIMIT.lsel));
	locbaseptr = baseptr;
	while(locbaseptr < UD_Select_ptr)
	{
		n = UD_Select_buf[locbaseptr];
		segno = UD_Select_buf[locbaseptr + n - 1];
	
		if(ud_limsel(segno, filter_on, viewkey) == UU_FALSE)
		{
			for(i=1; i<=n; i++)
				UD_Select_buf[locbaseptr+i] = UD_NULLPICKID;
		}
		else
/* 		-- at least one entity has passed thru ud_limsel */
			got_one = UU_TRUE;
	
/*		-- bump to next item -- */
	
		locbaseptr = locbaseptr + n + 1;
	}

	if((got_one == UU_FALSE) && (UD_noselerr == 0))
/* 	-- No entity picked -- */
		uu_uerror0(UD_DASHEP, 92);

	uu_dexit;
	return;
}

/*********************************************************************
**
**    I_FUNCTION :  UU_LOGICAL ud_limsel(segno, filter_on, viewkey)
**       detect if a segment is pickable or not
**
**    PARAMETERS   
**       INPUT  : 
**          segno = segment number to interrogate
**				filter_on = UU_TRUE, UU_FALSE
**				viewkey = viewkey to filter on; UU_NULL for no filtering
**       OUTPUT :  
**          none
**
**    RETURNS      : UU_TRUE if this is a valid entity (not excluded)
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

UU_LOGICAL ud_limsel(segno, filter_on, viewkey)
int segno;								/* segment number */
UU_LOGICAL filter_on;				/* attribute filtering? */
UU_KEY_ID viewkey;					/* viewkey to filter on */
{
	uv_segbuff(udata);				/* user segment id data buffer */
	extern UD_FILTREC UD_filter;	/* attribute filter */
	UU_KEY_ID segkey;					/* viewkey of segment */
	int rel_num;
	UM_int2 primtyp,marker;
	struct UC_entitydatabag e;
	struct UC_attributedatabag attr;
	uu_denter(UU_DTRC,(us,
		"enter ud_limsel:segno=%d, filter_on=%d) UD_LIMIT.lsel %d",
			segno, filter_on, UD_LIMIT.lsel));

	ud_prntlim("limsel");

/* -- always set features unpickable -- */

	if (ud_getpick_type()==UD_PICK_ASSIST && ud_isassist_seg(segno))
		goto rettrue;
	gsegrud(segno, udata);
	rel_num = uv_getrelnum(udata);
	if(rel_num == UM_FEAT_REL)
		goto retfalse;

/* -- filter out segs in wrong view -- */

	if(viewkey != 0)
	{
		segkey = uv_getviewkey(udata);
		uu_dprint(UU_DTRC,(us,"ud_limsel: viewkey %d segkey %d",
			viewkey, segkey));
		if(segkey != viewkey)
			goto retfalse;
	}

	segkey = uv_getkey(udata);
	if (ud_islimit_entity(segkey)==0)
		goto retfalse;

  	if(UD_LIMIT.lsel)
  	{

/* 	-- limiting the DAS -- */

		if(!uu_tst_bit(UD_LIMIT.lselbf, rel_num-1))
		{
			primtyp = 0;
			if ((rel_num==UM_RBSPLSRF_REL || rel_num==NCL_SURF_REL ||
				rel_num==NCL_TRIMSF_REL) &&
				uu_tst_bit(UD_LIMIT.lselbf, NCL_PLN_REL-1))
			{
				segkey = uv_getkey(udata);
				ncl_get_sf_primtyp(&segkey, &primtyp);
			}
/*			--	if bit not set, invalid entity picked -- */

			uu_dprint(UU_DTRC,(us,"ud_limsel: entity filter failed"));
			if (primtyp!=3) goto retfalse;
		}
  	}

  	if(UD_LIMIT.editable)
  	{

/* 	-- limiting the DAS to editable entities -- */

		if(UD_LIMIT.editable)
		{
/*			--	if non-editable entity picked -- */

			uu_dprint(UU_DTRC,(us,"ud_limsel: editable=%x, buf[3]=%x",
				(uv_geteditable(udata)), udata[3]));

			if(!(uv_geteditable(udata)))
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: editable filter failed"));
				goto retfalse;
			}
		}
  	}

  	if(filter_on)
   {

/* 	-- attribute filtering is on -- */	

		if(UD_filter.layer_state != UD_INACTIVE)
		{
/* 		-- make sure layer is valid -- */
/*
.....when we change the geometry's layer, we only updated the unibase without updated
.....the layer attributer in display segment (to avoid repaint to save time)
.....so here we cannot use segment's layer data but use geometry entity data in unibase
.....Yurong
*/
/*			if((uv_getlayer(udata) < 0) || (uv_getlayer(udata) > UM_MAX_LAYERS))
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: layer %d out of range",
					uv_getlayer(udata)));
				goto retfalse;
			}
/* 		-- filtering for layer -- */
/*			if(!uu_tst_bit(UD_filter.f_layer, uv_getlayer(udata))) 
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: attribute layer filter failed"));
				goto retfalse;
			}
*/
			e.key = uv_getkey (udata);
			if (uc_retrieve_data(&e, sizeof(struct UC_entitydatabag)) != UU_SUCCESS)
				goto retfalse;
			if (uc_retrieve_attr(e.key, &attr) != UU_SUCCESS)
				goto retfalse;
			if(!uu_tst_bit(UD_filter.f_layer, attr.layer)) 
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: attribute layer filter failed"));
				goto retfalse;
			}			
		}

		if(UD_filter.color_state != UD_INACTIVE)
		{
/* 		-- make sure color is valid -- */

			if((uv_getcolor(udata) < 0) || (uv_getcolor(udata) > 
														(UM_MAX_COLORS - 1)))
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: color %d out of range",
					uv_getcolor(udata)));
				goto retfalse;
			}

/* 		-- filtering for color -- */

			if(!uu_tst_bit(UD_filter.f_color, uv_getcolor(udata)))
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: attribute color filter failed"));
				goto retfalse;
			}
		}

		if(UD_filter.linestyle_state != UD_INACTIVE)
		{
/* 		-- make sure linestyle is valid -- */

			if((uv_getlstyle(udata) < 1) || (uv_getlstyle(udata) > 
														UM_MAX_LINESTYLES))
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: linestyle %d out of range",
					uv_getlstyle(udata)));
				goto retfalse;
			}

/* 		-- filtering for linestyle -- */

			if(!uu_tst_bit(UD_filter.f_linestyle, uv_getlstyle(udata)-1))
			{
				uu_dprint(UU_DTRC,(us,
					"ud_limsel: attribute line style filter failed"));
				goto retfalse;
			}
		}

		if(UD_filter.entity_state != UD_INACTIVE)
		{
/* 		-- make sure entity type is valid -- */

			if((uv_getrelnum(udata) < 1) || (uv_getrelnum(udata) > 
														UM_MAX_RELATION))
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: entity type %d out of range",
					uv_getrelnum(udata)));
				goto retfalse;
			}

/* 		-- filtering for entity type -- */

			if(!uu_tst_bit(UD_filter.f_entity, uv_getrelnum(udata)-1))
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: attribute entity filter failed"));
				goto retfalse;
			}
		}

/* 	-- filtering for pen number -- */

		if(UD_filter.pen_state != UD_INACTIVE)
		{
/* 		-- make sure pen number is valid -- */
/*
.....when we change the geometry's pen, we only updated the unibase without updated
.....the pen attributer in display segment (to avoid repaint to save time)
.....so here we cannot use segment's pen data but use geometry entity data in unibase
.....Yurong
*/
/*			if((uv_getpen(udata) < 1) || (uv_getpen(udata) > UM_MAX_PENS))
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: pen number %d out of range",
					uv_getpen(udata)));
				goto retfalse;
			}

			if(!uu_tst_bit(UD_filter.f_pen, uv_getpen(udata)-1))
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: attribute pen filter failed"));
				goto retfalse;
			}
*/
			e.key = uv_getkey (udata);
			if (uc_retrieve_data(&e, sizeof(struct UC_entitydatabag)) != UU_SUCCESS)
				goto retfalse;
			if (uc_retrieve_attr(e.key, &attr) != UU_SUCCESS)
				goto retfalse;
			if(!uu_tst_bit(UD_filter.f_pen, attr.pen-1)) 
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: attribute pen filter failed"));
				goto retfalse;
			}
		}

/* 	-- filtering for marker type -- */

		if((rel_num == UM_POINT_REL || rel_num == NCL_PATERN_REL) &&
			UD_filter.marker_state != UD_INACTIVE)
		{
/* 		-- make sure marker type is valid -- */
			segkey = uv_getkey(udata);
			umf_get_marker_type(&segkey,&marker);
			if(marker < 1 || marker > UD_MAX_MARKERTYPES)
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: marker type %d out of range",
					marker));
				goto retfalse;
			}

			if(!uu_tst_bit(UD_filter.f_marker, marker-1))
			{
				uu_dprint(UU_DTRC,(us,"ud_limsel: marker type filter failed"));
				goto retfalse;
			}
		}
	} /* -- end of iffilter_on -- */

/* -- entity has passed all active filters -- */

rettrue:
	uu_dprint(UU_DTRC,(us,"ud_limsel returns TRUE"));
	uu_dexit;
	return(UU_TRUE);

retfalse:
	uu_dprint(UU_DTRC,(us,"ud_limsel returns FALSE"));
	uu_dexit;
	return(UU_FALSE);
}
	
/*********************************************************************
**
**    I_FUNCTION :  ud_setpick_type(int type)
**      Set the pick type
**
**    PARAMETERS   
**       INPUT  : 
**          type = pick type to be set
**       OUTPUT :  
**          none
**
**    RETURNS      :none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_setpick_type(type)
int type;
{
	if (type==UD_PICK_SECONDARY)
		ur_getu_second();
	else
		ur_getu_work();
	S_pick_type = type;
}

/*********************************************************************
**
**    E_FUNCTION :  ud_getpick_type()
**      Returns the current pick type.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**
**    RETURNS      :none
**          Active pick type.
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int ud_getpick_type()
{
	return S_pick_type;
}

/*********************************************************************
**
**    E_FUNCTION :  ud_motion_pick_type()
**      Returns the current pick type.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**
**    RETURNS      :none
**          Active pick type.
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
UU_LOGICAL ud_motion_pick_type()
{
	if (S_pick_type == UD_PICK_MOTION || S_pick_type == UD_PICK_MOTSEG)
		return(UU_TRUE);
	else
		return(UU_FALSE);
}

/*********************************************************************
**
**    E_FUNCTION :  ud_verifypick_type(vport,printerr)
**      Verify the pick type is correct for the current view type.
**
**    PARAMETERS   
**       INPUT  :
**          vport     = Active viewport.
**          printerr  = UU_TRUE = output error message, UU_FALSE otherwise.
**       OUTPUT : none
**
**    RETURNS      :none
**          UU_TRUE if pick type is valid, UU_FALSE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int ud_verifypick_type(vport,printerr)
UV_vport *vport;
UU_LOGICAL printerr;
{
	UV_view view;

	uv_getvid(vport->cur_view, &view);
	if ((S_pick_type==UD_PICK_SECONDARY && view.vtype!=UV_SECONDARY_VIEW)
		|| (S_pick_type==UD_PICK_INVISIBLE && view.vtype!=UV_INVISIBLE_VIEW)
		/*|| (S_pick_type==UD_PICK_NORMAL && view.vtype==UV_INVISIBLE_VIEW)*/
		|| (S_pick_type==UD_PICK_NORMAL && view.vtype==UV_SECONDARY_VIEW))
	{
		if (printerr)
			ud_winerror ("Error - An invalid view for picking was selected!");
		return(UU_FALSE);
	}
	return(UU_TRUE);
}

/*********************************************************************
**
**    I_FUNCTION :  ud_unhilight_pickseg()
**      Unhighlight the curent picking segments
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      :none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_unhilight_pickseg()
{
	int locbaseptr;
	int n, segno, stat;
	uv_segbuff(buff);

	locbaseptr = 0;
	while(locbaseptr < UD_Select_ptr)
	{
		n = UD_Select_buf[locbaseptr];
		segno = UD_Select_buf[locbaseptr + n - 1];

		if(segno != UD_NULLPICKID)
		{
			stat = gsegrud(segno, buff);
			if (stat!=0)
				break;
			while (segno != UD_NULLPICKID)
			{
				gsseghilite(segno, UG_NORMAL);
				stat = gsegrud(segno, buff);
				if (stat!=0)
				{
					segno = UD_NULLPICKID;
					break;
				}
				segno = uv_getsegid(buff);
			}
		}
		locbaseptr = locbaseptr + n + 1;
	}
}

/*********************************************************************
**
**    I_FUNCTION :  ud_restore_hilgt_seg()
**      Restore/Highlight the picking segment
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      :none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_restore_hilgt_seg()
{
	int locbaseptr;
	int n, segno, stat;
	uv_segbuff(buff);

	locbaseptr = 0;
	while(locbaseptr < UD_Select_ptr)
	{
		n = UD_Select_buf[locbaseptr];
		segno = UD_Select_buf[locbaseptr + n - 1];

		if(segno != UD_NULLPICKID)
		{
			stat = gsegrud(segno, buff);
			if (stat!=0)
				break;
			while (segno != UD_NULLPICKID)
			{
				gsseghilite(segno, UG_HIGHLIGHTED);
				stat = gsegrud(segno, buff);
				if (stat!=0)
				{
					segno = UD_NULLPICKID;
					break;
				}
				segno = uv_getsegid(buff);
			}
		}
		locbaseptr = locbaseptr + n + 1;
	}
}

/*********************************************************************
**    E_FUNCTION : ud_get_pickstr(prompt, datatype, lstr, prec, len, flag)
**			Get  "Pick" or "Locate" string
**    PARAMETERS   
**       INPUT  :  prompt: prompt for "Pick" or "Locate"
**						datatype: data type of picking or locate
**						prec    = Number of digits to right of decimal for real
**						          numbers.
**						len     = Size of field.
**						flag: 1: pick, just reurn label
**								2: locate
**								4: pick but return label as "label(subscr)" format
**								5: pick but return label as "subscr" format
**       OUTPUT : lstr
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_get_pickstr(prompt, datatype, lstr, prec, len, flag)
char *prompt, *lstr;
int prec,len;
int datatype, flag;
{
	int evtype,evecho,evdev,nc,type,device,pet, sub;
	UD_DASTAT status,ud_pick1(),ncl_verify_pick(),ud_cart1(),ud_vec1(),ud_str1();
	UD_PPICKREC ret_pck;
	UD_DEVENT event;
	UD_EVENTDEF ud_inparm;
	UD_NDCLOCREC ret_cord[3],def_cord[3];
	char *ud_uncord(),*ud_unvec();
	int save_active;
	struct NCL_fixed_databag e;
    uv_segbuff(buf);       
/*
.....Set default prompt
*/
	ud_inparm.strbfsz = 80;
	ud_inparm.prompt = prompt;
	ud_inparm.number = 1;
/*
.....Setup default input type
*/
	status = DE_AGAIN;
/*	NCL_nopick_cnt = 0;*/
	ud_reset_verify_list();
	if (flag == 2)
	{
		evtype = UD_LOCATOR;
		evdev = UD_locdev;
		evecho = UD_locech;
	}
	else
	{
		evtype = UD_PICK;
		evdev = UD_pckdev;
		evecho = UD_pckech;
	}
	ud_inparm.evtype = evtype;
	ud_inparm.device = evdev;
	ud_inparm.pet = evecho;
/*
.....Get DAS input
*/
	while (status == DE_AGAIN)
	{
		ud_gevt(&event, evtype, ud_inparm.prompt, 1, evdev, evecho, &ud_inparm);
/*
......if UD_form_bypick==0 and it is in pick mode
......return to form
*/
		if (((flag==1)||(flag==4) || (flag==5))&&(UD_form_bypick==0))
			goto done;	
		
		if (event.evclass == UD_PICK)
		{
			status = ud_pick1(&event,&ret_pck);
/*
.....restore picking area and ready to pick again or accept
*/
			ud_restore_pickarea();

			if (status == DE_TRUE && 
				(datatype != UD_DASCART &&
					datatype != UD_DASVEC))
/*
........VERIFY MODE:
*/
			{
/*
.....this changed to use other key function to deal with it
*/
				if (NCL_mark_method==DYNAMIC_MARK)
				{
					if (UD_select_key)
					{
						ncl_verify_pick2(&ret_pck);
						ud_post_msg(2);
						UD_select_key = 0;
						status = DE_AGAIN;
					}
				}
				else
				{
					status = ncl_verify_pick(&ret_pck);
				}
			}
			evtype = UD_PICK;
			evdev = UD_pckdev;
			evecho = UD_pckech;
		}
		else if (event.evclass == UD_CHOICE &&
			event.evdev == UD_AUXMENU &&
			event.indata.choicedata == UD_CDONE) 
		{
			status = DE_DONE;
			goto done;
		}
		else status = DE_TRUE;
/*
.....Format the text string
*/
		switch (datatype)
		{
		case UD_DASCART:
			ud_qcord(&type, &device, &pet);
			status = ud_cart1(&event, ret_cord, UU_FALSE, def_cord, UU_TRUE);
			if (status == DE_TRUE)
			{
				ud_dtcord(type,device,pet);
				strcpy(lstr,ud_uncord(prec,len,ret_cord));
			}
			break;
		case UD_DASVEC:
			status = ud_vec1(&event, ret_cord, UU_FALSE, def_cord);
			if (status == DE_TRUE)
			{
				strcpy(lstr,ud_unvec(prec,len,ret_cord));
			}
			break;
		default:
			if (status == DE_TRUE)
			{
				if ((flag!=4) && (flag!=5))
					status = ud_str1(&event,lstr,UU_FALSE,NULL,len,&nc);
				else
				{
					save_active =  UR_active; 
					gsegrud (event.indata.pickdata.pickpath[0],buf);
					e.key = uv_getkey (buf);
/*
......if pick is for Secondary unibase, we need use Secondary unibase
......to retrieve data
*/
					if (ud_getpick_type()==UD_PICK_SECONDARY)
						ur_getu_second();
					ur_retrieve_data_fixed(&e);
					ncl_get_label_and_subscr(&e, lstr, &sub);
					if (sub>0)
					{
						if (flag==5)
							sprintf(lstr, "%d", sub);
					}
					if (ud_getpick_type()==UD_PICK_SECONDARY)
					{
						if (save_active==1)
							ur_getu_work();
						else
							ur_getu_second();
					}
				}
			}
			break;
		}
	}
done:;
	 return status;
}

/*********************************************************************
**    S_FUNCTION     :  S_hilite_entity(sfpt,color)
**       Highlights the selected geometry.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_hilite_entity (sfpt, color)
UD_sel_geo *sfpt;
int color;
{
	struct NCL_fixed_databag e;

	if (color != -1)
	{
		if (sfpt->key != 0)
		{
			e.key = sfpt->key;
			if (ncl_retrieve_data_fixed(&e) != 0) return;
			ncl_get_geo_color (e.key, &sfpt->color);
			if (color==-1) color = sfpt->color;
			ncl_update_geo_color(e.key, color, UU_TRUE);
			uc_display(&e);
		}
	}
}

/*********************************************************************
**    S_FUNCTION     :  S_unhilite_entity(sfpt)
**       Unhighlights the selected geometry.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_unhilite_entity(sfpt)
UD_sel_geo *sfpt;
{
	struct NCL_fixed_databag e;
/*
.....Save geometry color
*/
	if ((sfpt != 0)&&(sfpt->key!=0))
	{
		e.key = sfpt->key;
		if (ncl_retrieve_data_fixed(&e) != 0) return;
		if (sfpt->color == -1) ncl_get_geo_color (e.key,&sfpt->color);
		ncl_update_geo_color(e.key,sfpt->color,UU_FALSE);
		uc_display(&e);
	}
}

void ud_unhilite_sellist(select_list)
UU_LIST *select_list;
{
	int i;
	UD_sel_geo *geo;  
	geo = (UD_sel_geo*)UU_LIST_ARRAY(select_list);
	for (i=0;i<UU_LIST_LENGTH(select_list);i++) 
		S_unhilite_entity(&geo[i]);
}

/********************************************************************* 
**
**  I_FUNCTION			:  ud_get_pick_selstr(prompt, sflst, color)
**    Get  multiple "Pick" geom list and highlite
**
**  PARAMETERS   
**       INPUT  :  prompt: prompt for "Pick" 
**					color: hilite color
**	
**		OUTPUT: sflst: pick geometry list
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/
int ud_get_pick_selstr(prompt, sflst, color)
char *prompt;
UU_LIST *sflst;
int color;
{
	int i, numint,iret,nc;
	unsigned int *imask;
	UU_LOGICAL init;
	struct NCL_fixed_databag e;
	UU_LOGICAL cmdreject,doend;
	UM_int4 sfkey;
	UM_int2 primtyp;
	UD_sel_geo *geo, sfpt;  

	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0)
	{
		iret = DE_DONE;
		goto done;
	}
/*
.....un-hilite
*/
	geo = (UD_sel_geo*)UU_LIST_ARRAY(sflst);
	for (i=0;i<UU_LIST_LENGTH(sflst);i++) 
		S_unhilite_entity(&geo[i]);
	uu_list_free(sflst);
repeat:;
	ud_rpwrcom(prompt);
	ud_ddas(UD_DASSELECT, prompt, NULL, 1, &numint, UD_NODEFAULT);
	UD_promptsys = 0;
	UD_promptnum = 0;

	if (numint == 0)
	{
		iret = DE_DONE;
		goto done;
	}
	else
		iret = 0;

	uu_list_init(sflst, sizeof(UD_sel_geo), numint,10);
	
	init = UU_TRUE;
	while (ud_gnxt(init,UU_NULL,&e.key,1))
	{
		init = UU_FALSE;
		if (e.key != 0)
		{
			if (ncl_retrieve_data_fixed(&e) != 0) goto done;
			sfpt.key = e.key;
			if (color>=0)
				S_hilite_entity(&sfpt, color);
			else
				S_hilite_entity(&sfpt, uw_glhicolor);
/*
........Push the entity onto the stack
*/
			uu_list_push(sflst, &sfpt);
		}
	}
/*
........No entities selected
*/
	if (UU_LIST_LENGTH(sflst) == 0)
	{
		ud_wrerr("No CAM entities selected.");
		uu_list_free(sflst);
		goto repeat;
	} 
done:;
	UD_UNMARK(cmdreject);
	return(iret);
/*
.....User did not select anything
*/
failed:
	iret = UU_FAILURE;
	goto done;
}


