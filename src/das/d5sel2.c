/*********************************************************************
**
**    NAME         :  d5sel2.c
**
**       CONTAINS:
**    		  	ud_gnxt
**			  	ud_gnxtsegid
**    			ud_gextrema(bufadr, extrema)
**				ud_clip_region
**				gtpknxt
**				ud_clip_seg
**				ud_close_clipseg
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d5sel2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:13
**
*********************************************************************/

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) d5sel.c 2.2 9/30/85 14:49:55 single"};
#else
static char uu_sccsident[]={"@(#) d5sel.c 2.2 9/30/85 14:49:55 double"};
#endif


#include "usysdef.h"
#include "ustdio.h"
#include "dselect.h"
#include "mdpick.h"
#include "udebug.h"
#include "gsegac.h"
#include "mfort.h"
#include "ncl.h"
#include "nccs.h"
#include "gtbl.h"
#include "nclfc.h"



#define UD_NULLPICKID -1			/* invalid pick id */
int NCL_clipf = 0;
/*********************************************************************
**
**    E_FUNCTION         :  ud_gnxt(initflag, bufadr, mtid, level)
**       returns next picked item in global select buffer
**
**    PARAMETERS   
**       INPUT  : 
**          initflag = UU_TRUE for first time, UU_FALSE for subsequent calls
**				bufadr   = buffer of selected entities (NULL for global select
**									buffer)
**				level    = level number
**       OUTPUT :  
**          mtid = pickid of this entity
**
**    RETURNS      : UU_TRUE if a valid entry was found, UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

UU_LOGICAL ud_gnxt(initflag, bufadr, mtid, level)
UU_LOGICAL initflag;							/* if UU_TRUE, then initialize */
int bufadr[];									/* address of buffer */
UU_KEY_ID *mtid;								/* return mtid number */
int level;										/*  level number */
{
	static int buf_ptr;					/* pointer into the buffer */
	static int oldbuf_ptr;				/* trailer pointer into the buffer */
	int length;
	int oldlen;
	UU_LOGICAL status;
	int i;									/* temp integer */
	int um_d_pickresolve();
	UM_PICKENT pent;
	int stat;								/* status of um_d_pickresolve */
	char us[150];
	char p_buff[80];


	if(initflag == UU_TRUE)
	{
		buf_ptr = 0;
		oldbuf_ptr = 0;
	}

	sleep(1);

	
	if(bufadr == NULL)

/*	-- system select buffer -- */

	{
		length = UD_Select_buf[buf_ptr];
		if(buf_ptr > 0)
		{
			if(buf_ptr < UD_Select_ptr)
			{

				/*	-- cycle thru NULL pickid's -- */

				while(UD_Select_buf[buf_ptr+1] == UD_NULLPICKID
						&& buf_ptr < UD_Select_ptr)
				{
					buf_ptr = buf_ptr + length + 1;
					length = UD_Select_buf[buf_ptr];
				}

				/*	-- do not return selected entities with the same segment number
				if prohibited -- */

				if(UD_Sellim_mp == UU_TRUE && buf_ptr < UD_Select_ptr)
				{
					i = 0;
					while (i<buf_ptr)
					{
						if (UD_Select_buf[buf_ptr+1] == UD_Select_buf[i+1])
						{
							buf_ptr = buf_ptr + length + 1;
							length = UD_Select_buf[buf_ptr];
							i = 0;
						}
						else
							i += UD_Select_buf[i] + 1;
					}
				}
			}
		}
		else
		{

			/*	-- if any of the first items are null then flush them -- */

			while(UD_Select_buf[buf_ptr+1] == UD_NULLPICKID
					&& buf_ptr < UD_Select_ptr)
			{
				buf_ptr = buf_ptr + length + 1;
				length = UD_Select_buf[buf_ptr];
			}
		}
		
		/*	-- convert to higher level entity if level requires it -- */
		if(buf_ptr < UD_Select_ptr)
		{
			status = UU_TRUE;
			stat = um_d_pickresolve(&UD_Select_buf[buf_ptr], level, &pent);
			
			if (stat == 0) *mtid = um_get_pickkey(&pent, level);
		}
		else
			status = UU_FALSE;
	}
	else
	/*	-- user supplied buffer -- */
	{
		length = bufadr[buf_ptr];
		*mtid = bufadr[buf_ptr + length];
		status = UU_TRUE;
	}

	oldbuf_ptr = buf_ptr;
	buf_ptr = buf_ptr + length + 1;

	uu_dprint(UU_DTRC,(us,"leave gnxt, mtid=%d, b_ptr=%d, old_ptr=%d, len=%d",
		*mtid, buf_ptr, oldbuf_ptr, length));

	return(status);
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_gnxtsegid (initflag, bufadr, depth, pickpath)
**       returns next picked item in global select buffer as a seg id
**
**    PARAMETERS   
**       INPUT  : 
**          initflag = UU_TRUE for first time, UU_FALSE for subsequent calls
**				bufadr   = buffer of selected entities (NULL for global select
**									buffer)
**       OUTPUT :  
**          depth = length of pickpath of this entity
**				pickpath = pickpath of this entity
**
**    RETURNS      : UU_TRUE if a valid entry was found, UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

UU_LOGICAL ud_gnxtsegid(initflag, bufadr, depth, pickpath)
UU_LOGICAL initflag;							/* if UU_TRUE, then initialize */
int bufadr[];									/* address of buffer */
int *depth;										/* return depth */
int pickpath[];								/* pickpath */
{
	static int buf_ptr;					/* pointer into the buffer */
	static int oldbuf_ptr;				/* trailer pointer into the buffer */
	int length;
	int oldlen;
	UU_LOGICAL status;
	int i;									/* temp integer */
	char us[150];

	if(initflag == UU_TRUE)
	{
		buf_ptr = 0;
		oldbuf_ptr = 0;
	}
	uu_denter2(UU_DTRC,(us,"enter gnxtsegid, buf_ptr=%d, old=%d, buffer[ptr]=%d %d %d",
		buf_ptr,oldbuf_ptr, UD_Select_buf[buf_ptr], UD_Select_buf[buf_ptr+1],
		UD_Select_buf[buf_ptr+2]));

	if(bufadr == NULL)

	/*	-- system select buffer -- */

	{
		length = UD_Select_buf[buf_ptr];
		if(buf_ptr > 0)
		{
			if(buf_ptr < UD_Select_ptr)
			{

				/*	-- cycle thru NULL pickid's -- */

				while(UD_Select_buf[buf_ptr+1] == UD_NULLPICKID
						&& buf_ptr < UD_Select_ptr)
				{
					buf_ptr = buf_ptr + length + 1;
					length = UD_Select_buf[buf_ptr];
				}

				/*	-- do not return selected entities with the same segment number
				if prohibited -- */

				if(UD_Sellim_mp == UU_TRUE && buf_ptr < UD_Select_ptr)
				{
					i = 0;
					while (i<buf_ptr)
					{
						if (UD_Select_buf[buf_ptr+1] == UD_Select_buf[i+1])
						{
							buf_ptr = buf_ptr + length + 1;
							length = UD_Select_buf[buf_ptr];
							i = 0;
						}
						else
							i += UD_Select_buf[i] + 1;
					}
				}
			}
		}
		else
		{

			/*	-- if any of the first items are null then flush them -- */

			while(UD_Select_buf[buf_ptr+1] == UD_NULLPICKID
					&& buf_ptr < UD_Select_ptr)
			{
				buf_ptr = buf_ptr + length + 1;
				length = UD_Select_buf[buf_ptr];
			}
		}

		/*	-- move in depth and pickpath -- */

		if(buf_ptr < UD_Select_ptr)
		{
			status = UU_TRUE;
			*depth = length;
			for(i=0; i<length; i++) {
				pickpath[i] = UD_Select_buf[buf_ptr+i+1];
				uu_dprint(UU_DTRC,(us,"pickpath[%d] = %d",i, pickpath[i]));
			}
		}
		else
			status = UU_FALSE;
	}
	else

	/*	-- user supplied buffer -- */

	{
		length = bufadr[buf_ptr];
		*depth = length;
		for(i=0; i<length; i++)
			pickpath[i] = bufadr[buf_ptr+i+1];
		status = UU_TRUE;
	}

	oldbuf_ptr = buf_ptr;
	buf_ptr = buf_ptr + length + 1;

	uu_dprint(UU_DTRC,(us,
		"leave gnxtsegid, depth=%d, b_ptr=%d, old_ptr=%d, len=%d",
			*depth, buf_ptr, oldbuf_ptr, length));
	uu_dexit;
	return(status);
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_gextrema(bufadr, extrema)
**       returns extrema of items in global select buffer
**
**    PARAMETERS
**       INPUT  :
**          bufadr   = buffer of selected entities (NULL for global select
**                         buffer)
**       OUTPUT :
**				extrema: Max and min coordinates of selected segs
**								in world coordinates
**
**    RETURNS      : none
**    SIDE EFFECTS : none       
**    WARNINGS     : none       
**                              
*********************************************************************/

void ud_gextrema(bufadr,extrema)
int bufadr[];									/* address of buffer */
Gwrect3 *extrema;	          				/* Extrema  */
{
	UU_LOGICAL	ud_gnxtsegid();
	int depth,pickpath[2];
	Gwrect3 *rect;

	uu_denter(UU_DTRC,(us,"ud_gextrema(%x)",bufadr));

	if(ud_gnxtsegid(UU_TRUE, bufadr, &depth, pickpath)!= UU_TRUE)
		{
		uu_dprint(UU_DTRC,(us,"ud_gextrema:Error - no entities in bufadr"));
		uu_dexit;
		return;
		}
	extrema->llf.x = 1.0e10;
	extrema->llf.y = 1.0e10;
	extrema->llf.z = -1.0e10;
	extrema->urb.x = -1.0e10;
	extrema->urb.y = -1.0e10;
	extrema->urb.z = 1.0e10;
	ug_segextrema(ug_segac(pickpath[0]),extrema);
	while(ud_gnxtsegid(UU_FALSE, bufadr, &depth, pickpath) == UU_TRUE)
		ug_segextrema(ug_segac(pickpath[0]),extrema);
	uu_dprint(UU_DTRC,(us,"ud_gextrema:extrema = %f %f %f, %f %f %f",
			extrema->llf.x,extrema->llf.y,extrema->llf.z,
			extrema->urb.x,extrema->urb.y,extrema->urb.z));
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : ud_clip_region(flag)
**			region select handler in select subsystem.
**    PARAMETERS   
**       INPUT  : flag: 1: call from fortran, don't highlight segments
**
**       OUTPUT :  none
**
**    RETURNS      :
**			UU_TRUE if there are selection.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ud_clip_region(flag)
UM_int4 *flag;
{
	int retcount, count, wdcount;
	int baseptr;
	int stat;
/*
.....Initialize routine
*/
	if (UD_selclip_npl<=0) return UU_FALSE;

	UD_Selrej_ptr = UD_Select_ptr;
	UD_Selrej_cnt = UD_Select_cnt;

	count = UD_Selbuf_size - UD_Select_ptr;
	baseptr = UD_Select_ptr;

	gfindclip(&UD_Select_buf[UD_Select_ptr], count, &retcount, &wdcount);

	UD_Select_ptr = UD_Select_ptr + wdcount;
	UD_Select_cnt = UD_Select_cnt + retcount;
/*
......filter entities excluded by DAS limit or attribute filter
*/
	ud_limsss(baseptr, 0, 0);
/*
......set the entities as unpickable
*/
	if (*flag==0)
		ud_spstat(UU_FALSE, baseptr);	
	return UU_TRUE;
}	
	
/*********************************************************************
**
**    E_FUNCTION         :  gtpknxt(initflag, mtid, level, ret)
**       returns next picked item in global select buffer (fortran callable
**			of ud_gnxt)
**
**    PARAMETERS   
**       INPUT  : 
**          initflag = UU_TRUE for first time, UU_FALSE for subsequent calls
**			level    = level number
**       OUTPUT :  
**          mtid = pick key of this entity
**			ret:   1: if a valid entry was found, 0 otherwise
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void gtpknxt(initflag, mtid, level, ret)
UU_KEY_ID *mtid;
UM_int4 *initflag, *level, *ret;
{
	UU_LOGICAL init;
	int ilevel = *level;
	if (*initflag==0)
		init = UU_FALSE;
	else
		init = UU_TRUE;
	*ret = ud_gnxt(init, UU_NULL, mtid, ilevel);
}

/*********************************************************************
**
**    E_FUNCTION         :  ud_clip_seg
**       This function initialize the segment storage for the clipf function
**			when run batch
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_clip_seg()
{
	int status,ietype;
	struct NCL_fixed_databag ent;
		
	ug_gksos.sysstate=UG_WSAC;
/* 
......initialize segment storage 
*/
	ug_segacinit();
/*
.....Loop through Unibase for all entities
*/
	vxlfst();
	NCL_clipf = 1;
	while (UU_TRUE)
	{
		if (!ncl_vxlnxt(&ent.key,&ietype)) break;
		if (ietype==1) break;
		if (ent.key==0) continue;
/*
...only for geometries
*/
		if (ietype ==NCLI_POINT || ietype ==NCLI_VECTOR || ietype ==NCLI_LINE 
			||ietype == NCLI_CIRCLE || ietype == NCLI_CURVE || ietype == NCLI_SURF
			|| ietype == NCLI_MATRIX || ietype ==NCLI_PLANE || ietype ==NCLI_SHAPE 
			|| ietype ==NCLI_PATERN || ietype ==NCLI_POINTVEC || ietype == NCLI_MESHSURF
			|| ietype ==NCLI_QUILTSURF || ietype ==NCLI_NETSF || ietype == NCLI_EVALSF
			|| ietype ==NCLI_RBSF || ietype ==NCLI_REGSURF || ietype == NCLI_REVSURF
			|| ietype ==NCLI_SYMBOL || ietype ==NCLI_SOLID)
		{
			status = ncl_retrieve_data_fixed(&ent);
			if (status == UU_SUCCESS)
			{
				uc_display(&ent);
			}
		}
	}
	NCL_clipf = 0;
}
/*********************************************************************
**
**    E_FUNCTION         :  ud_close_clipseg
**       This function delete all segments and free the segment storage 
**			for the clipf function when run batch
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ud_close_clipseg()
{
	int status;
	int key;
	int displayability;
	int dsegid;
	UU_LOGICAL initialize;
/*
.....update all segment to -1 for all keys
*/
	status = 0;
	initialize = UU_TRUE;
	while (um_getallobjs(initialize, &key) == 0)
	{
		initialize = UU_FALSE;
		ur_retrieve_displayable(key, &displayability);
		if (displayability != UM_NEVERDISPLAYABLE)
		{
			ur_retrieve_disp_segid(key, &dsegid);
			if (dsegid != -1)
			{
				uv_delsegs(dsegid);
				ur_update_disp_segid(key, -1);
			}
		}
	}
	ug_gksos.sysstate=UG_GKCL;
	ug_segacterm();
}
