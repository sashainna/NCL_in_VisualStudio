/*********************************************************************
**
**    NAME         :  daseg.c
**
**       CONTAINS:
**            ud_create_assist_seg()
**            ud_close_assist_seg()
**            ud_pick_assist_seg()
**            ud_delete_assist_segs()
**            ud_assist_point()
**            ud_assist_vector()
**            ud_isassist_seg()
**            ud_pick_assist_seg()
**            ud_assist_point()
**            ud_assist_point1()
**            ud_assist_vector()
**            ud_assist_vector1()
**            ud_getn_assist_segs()
**            ud_get_assist_seg()
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       daseg.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:14
**
*********************************************************************/
#include "usysdef.h"
#include <math.h>
#include "nclfc.h"
#include "usysg.h"
#include "uhep.h"
#include "dmark.h"
#include "dinput.h"
#include "dasnog.h"
#include "diconm.h"
#include "dselect.h"
#include "udebug.h"
#include "xenv1.h"
#include "gmat4.h"
#include "ginqxf.h"
#include "gtblvar6.h"
#include "mdebug.h"
#include "nclicons.h"
#include "view.h"
#include "gtbl.h"
#include "ulist.h"
/*
.....Added by Paul to implement the "text" and "location" input. 07/10/92
*/
#include <ctype.h>
#include "class.h" 
#include "uims.h"
#include "mdcoord.h"
#include "mfort.h"  
#include "mdpick.h"
#include "driver.h"
#include "nclfc.h"
#include "gsegac.h"
#include "gviw.h"
#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3
#define INTSIZ(x) ((x+sizeof(int)-1)/sizeof(int))
#define INTSIZEOF(x) ((sizeof(x)+sizeof(int)-1)/sizeof(int))
#define MAX_ASSIST_SEG 10000

extern int NCL_mark_method;
extern int UD_select_key;		
extern int ug_viwseg;
extern struct {
	int mod;
	int ntran;
}	ug_xfseg;	

static int Sassist_seg_num = 0;
static UU_LIST Sassist_segs;

static int S_getseg_num();
static void S_get_screen_wc();

char *ug_lsielt();

/**************************************************************************
**
**  E_FUNCTION         : ud_create_assist_seg() 
**     Open a new assist display segment and return the number of
**			assist segment that is created 
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          none
**
**  RETURNS      :  assist segment number /0 if no segment created
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_create_assist_seg() 
{
	int segment;
	if (Sassist_seg_num==0)
	{
		uu_list_init (&Sassist_segs, sizeof(int), 25, 25);
		if (Sassist_segs.data == UU_NULL) return 0;
	}
	if (Sassist_seg_num+1>MAX_ASSIST_SEG)
		return 0;
	segment = gnseg();
	gcreateseg(segment);
		
	if (Sassist_seg_num+1<MAX_ASSIST_SEG)
	{
		uu_list_push(&Sassist_segs,&segment);
		Sassist_seg_num++;
	}
	return Sassist_seg_num;
}
/**************************************************************************
**
**  E_FUNCTION         : ud_close_assist_seg() 
**     Close a the current assist display segment
**		
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
void ud_close_assist_seg() 
{
	int i, base_seg, icmd, ncmd, cmdlen, siz;
	UG_segstli *bsegptr, *segptr;
	UG_LSI *listp;
	UG_plylna3op *cmd;
	UG_sntranop tran_cmd;

	base_seg = ug_viwseg;
	gcloseseg();
/*
.....we generally create the assist segment in the first viewport, but
.....if we have multiple views, we can't display assist segment in multiple view
.....other than xform = 1, because we didn't save anything in unibase for assist
.....segments, we can't do it like normally segment such dspent in craete segment
.....in diff viewport. What we can't do is in here, we check if we have more than 
.....one view active, if yes, copy the segment (in xform=1) into a new segment 
.....but xform= n.
*/
	bsegptr = ug_segac(base_seg);
	for (i = 1; i < UV_act_screen[0].nvports; i++)
	{
		ud_create_assist_seg();
		segptr = ug_segac(ug_viwseg);
/*		savid = segptr->segid;*/
/*		uu_move_byte((char*)bsegptr, (char*)segptr, sizeof(UG_segstli));*/
/*		segptr->segid = savid;*/
		listp = (UG_LSI *) bsegptr->seglist; 
		ncmd  = ug_lsinelt((*listp));
		for( icmd=0; icmd < ncmd; ++icmd )
		{
			cmd    = (UG_plylna3op *)ug_lsielt(listp,icmd);
			cmdlen = ug_lsilen(listp,icmd)/sizeof(int);
			switch (cmd->elttype)
			{
			case UG_SNTRANOP:
				tran_cmd.elttype = UG_SNTRANOP;
				tran_cmd.xform = i+1;
				ug_lsins(segptr->seglist,&tran_cmd,INTSIZEOF(tran_cmd));
				ug_gksstli.curvwindex = i+1;
				ug_xfseg.ntran=1;
				break;
			case UG_PLYLNA3OP: 
			case UG_PLYMKA3OP: 
				siz = sizeof(UG_plylna3op) - sizeof((*cmd).pts) + (cmd->len)*sizeof(Segpoint3);
				ug_lsins(segptr->seglist, cmd, INTSIZ(siz));
				break;
			case UG_PLYMKA2OP: 
			case UG_PLYLNA2OP: 
				siz=sizeof(UG_plylna2op) - sizeof((*cmd).pts) + (cmd->len)*sizeof(Segpoint);
				ug_lsins(segptr->seglist, cmd, INTSIZ(siz));
				break;
			case UG_LSTYLOP:
				siz=sizeof(cmd) - sizeof(((UG_lstylop*)cmd)->ls) + sizeof((((UG_lstylop*)cmd)->ls).typeno);
				ug_lsins(segptr->seglist, cmd, INTSIZ(siz));
				break;
			case UG_LNCOLROP:
			case UG_MKCOLROP:
			case UG_LWIDOP:
				ug_lsins(segptr->seglist, cmd, cmdlen);
				break;
			default: break;
			}
		}
		ug_updxforms(segptr);
		gcloseseg();
	}
}
/**************************************************************************
**
**  E_FUNCTION         : ud_delete_assist_segs() 
**     Delete all assist display segment
** 
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          none
**
**  RETURNS      :  assist segment number
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
void ud_delete_assist_segs() 
{
	int i, *seg_array;
/*
.....Initialize routine
*/
	if (Sassist_seg_num<=0) return;
	seg_array = (int *)UU_LIST_ARRAY(&Sassist_segs);
	if (seg_array==NULL) return;

	for (i=0; i<Sassist_seg_num;i++)
		ug_deleseg(seg_array[i]);
	Sassist_seg_num = 0;
	uu_list_free(&Sassist_segs);
}

/**************************************************************************
**
**  E_FUNCTION         : ud_isassist_seg(n) 
**     Check a segment to see if it is assist segment.
** 
**  PARAMETERS
**      INPUT  :
**          n: segment to be checked
**      OUTPUT :
**          none
**
**  RETURNS      :  1: Yes; 0: No
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_isassist_seg(n)
int n;
{
	int i;
	int *seg_array;
	seg_array = (int *)UU_LIST_ARRAY(&Sassist_segs);
	for (i=0; i<Sassist_seg_num;i++)
	{
		if (seg_array[i]==n)
			return 1;
	}
	return 0;
}
/**************************************************************************
**
**  E_FUNCTION         : ud_pick_assist_seg() 
**     Pick one assist segment. This Function is similar, maybe can merge
**		with ud_pick?
** 
**  PARAMETERS
**      INPUT  :
**          prompt: picking prompt
**      OUTPUT :
**          none
**
**  RETURNS      :  picked assist segment number / 0 if not picked
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_pick_assist_seg(prompt) 
char *prompt;
{
	UD_DASTAT status,ud_pick1(),ncl_verify_pick();
	UD_DEVENT event;
	char pbuf[100];
	int save_ptype, id, num;
	UD_PPICKREC ret_pck;

	save_ptype = ud_getpick_type();
	ud_setpick_type(UD_PICK_ASSIST);

	sprintf(pbuf, "%s %s", prompt, UD_synsel);

	status = DE_AGAIN;
/*
.....Reset verify list if have any
*/
/*
.....Initialize verify counter
*/
	ud_reset_verify_list();
	while(status == DE_AGAIN)
	{
		ud_gevt(&event, UD_pckint, pbuf, 1, UD_pckdev, UD_pckech, NULL);
		status = ud_pick1(&event, &ret_pck);
/*
.....restore picking area and ready to pick again or accept
*/
		ud_restore_pickarea();
/*
........VERIFY MODE:
*/
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
			if (status == DE_TRUE && (!UD_pckstr || event.evclass != UD_STRING))
				status = ncl_verify_pick(&ret_pck);
		}
	}
	id = ret_pck.pickpath[0];
	num = S_getseg_num (id);
	UD_pckstr = UU_FALSE;
	ud_setpick_type(save_ptype);
	ud_delete_assist_segs();
	return(num);
}

/**************************************************************************
**
**  E_FUNCTION         : ud_assist_point(gpt) 
**     Create one point (plus sign) assist segment.
** 
**  PARAMETERS
**      INPUT  :
**          gpt: point location
**      OUTPUT :
**          none
**
**  RETURNS      :  picked assist segment number / 0 if not picked
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_assist_point(gpt)
Gwpoint3 gpt;
{
	int num, color = 1;
	num = ud_assist_point1(color);
	return num;
}

/**************************************************************************
**
**  E_FUNCTION         : ud_assist_point1(gpt,clr) 
**     Create one point (plus sign) assist segment.
** 
**  PARAMETERS
**      INPUT  :
**          gpt: point location
**          clr: desired marker color
**      OUTPUT :
**          none
**
**  RETURNS      :  picked assist segment number / 0 if not picked
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_assist_point1(gpt,clr)
Gwpoint3 gpt;
int clr;
{
	int num;
	int markertype = ug_gksstli.curprats.mkbundl.type;

	num = ud_create_assist_seg();
	gsmarkcolor(clr);
	markertype = 3;
	gsmarktype(markertype);
	gsmarkcolor(clr);
	gsnormtran(1);

	gpolymarker3(1, &gpt);
	gsmarktype(markertype);
	ud_close_assist_seg();
	return num;
}

/**************************************************************************
**
**  E_FUNCTION         : ud_assist_vector1(startpt, vector, color)
**     Create one vector assist segment.
** 
**  PARAMETERS
**      INPUT  :
**         startpt: start point of the vector
**         vector: i,j,k of the vector: not unitized
**         color: vector color
**      OUTPUT :
**         none
**
**  RETURNS      :  picked assist segment number / 0 if not picked
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_assist_vector1(startpt, vector, color)
Gwpoint3 startpt;
Gwpoint3 vector; /* vector: not unitized */
int color;
{
	int num;
	double tfmat[12], temp1[3], temp2[3], unit, unit_x, unit_y, unit_z;
	num = ud_create_assist_seg();

	gslinecolor(color);
	gstextcolor(1);
	gsmarkcolor(1);
	gsnormtran(1);

	tfmat[1] = tfmat[2] = tfmat[3] = 0;
	tfmat[5] = tfmat[6] = tfmat[7] = 0;
	tfmat[9] = tfmat[10] = tfmat[11] = 0;
	tfmat[0] = tfmat[4] = tfmat[8] = 1;
/*
.....dstptv is using WC, so convert 1/20 screen size
.....to WC
*/
	S_get_screen_wc(&unit_x, &unit_y, &unit_z);
	temp1[0] = startpt.x;
	temp1[1] = startpt.y;
	temp1[2] = startpt.z;

	unit = unit_x * unit_x + unit_y * unit_y + unit_z * unit_z;
	unit = pow (unit, 0.5);
	temp2[0] = 0.05*unit*vector.x;
	temp2[1] = 0.05*unit*vector.y;
	temp2[2] = 0.05*unit*vector.z;

	dstptv (temp1, temp2, tfmat);
	ud_close_assist_seg();
	return num;
}

/**************************************************************************
**
**  E_FUNCTION         : ud_assist_vector(startpt, vector) 
**     Create one vector assist segment.
** 
**  PARAMETERS
**      INPUT  :
**         startpt: start point of the vector
**			vector: i,j,k of the vector: not unitized
**      OUTPUT :
**          none
**
**  RETURNS      :  picked assist segment number / 0 if not picked
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_assist_vector(startpt, vector)
Gwpoint3 startpt;
Gwpoint3 vector; /* vector: not unitized */
{
	int color,num;

	color = 1;
	num = ud_assist_vector1(startpt, vector, color);

	return num;
}

/**************************************************************************
**
**  E_FUNCTION         : ud_getn_assist_segs()
**     Returns the number of active assist segments.
** 
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          none
**
**  RETURNS      :  Number of active assist segments.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
int ud_getn_assist_segs()
{
	return(Sassist_seg_num);
}

/**************************************************************************
**
**  E_FUNCTION         : ud_get_assist_seg()
**     Returns the requested assist segment number.
** 
**  PARAMETERS
**      INPUT  :
**          seg    = Assist segment index to return segment number of.
**      OUTPUT :
**          none
**
**  RETURNS      :  Segment id of requested assist segment.  Returns
**                  -1 if there are no more assist segments.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
int ud_get_assist_seg(seg)
int seg;
{
	int *seg_array,iseg;
	if (seg < Sassist_seg_num)
	{
		seg_array = (int *)UU_LIST_ARRAY(&Sassist_segs);
		iseg = seg_array[seg];
	}
	else
		iseg = -1;
	return(iseg);
}

/**************************************************************************
**
**  I_FUNCTION         : S_getseg_num(id) 
**     Get a assist segment number from its segment id.
** 
**  PARAMETERS
**      INPUT  :
**          id: segment id
**      OUTPUT :
**          none
**
**  RETURNS      :  segment number
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
static int S_getseg_num(id)
int id;
{
	int i;
	int *seg_array;
	seg_array = (int *)UU_LIST_ARRAY(&Sassist_segs);
	for (i=0; i<Sassist_seg_num;i++)
	{
		if (seg_array[i]==id)
			return i+1;
	}
	return 0;
}

/**************************************************************************
**
**  I_FUNCTION         : S_get_screen_wc(id) 
**     Returns the size of the screen X in world coordinates.
** 
**  PARAMETERS
**      INPUT  :
**          id: segment id
**      OUTPUT :
**          none
**
**  RETURNS      :  segment number
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
static void S_get_screen_wc(x,y,z)
double *x,*y,*z;
{
	double x1, x2, y1, y2,z1,z2;

	x1 = 0.0;
	y1 = 0.0;
	z1 = 0.0;
	x2 = 1.0;
	y2 = 0.0;
	z2 = 0.0;
 	
	gndcw3(&x1, &y1, &z1, x1, y1, z1);
	gndcw3(&x2, &y2, &z2, x2, y2, z2);
	*x = fabs(x1-x2);
	*y = fabs(y1-y2);
	*z = fabs(z1-z2);
	return;
}
