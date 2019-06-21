/*********************************************************************
**    NAME         :  reeas.c
**       CONTAINS:
**       ur_cond_auto_save()
**       ur_enable_auto_save()
**			ur_disable_auto_save()
**			ur_snap_save()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reeas.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:29
*********************************************************************/

#include "udebug.h"
#include "dasnog.h"
#include "ribase.h"
#include "xfsys1.h"
#include "xenv1.h"		/* UX_pathname and UX_PRTERRS option definitions */
#include "uhep.h"

#define UR_BLKRCL	1024	/* block record length				*/
extern UU_LOGICAL	UR_ason;	/* could be Unibase modal? */
extern int UR_asfile;
long	UR_last_autosave = 0;	/* last autosave time */
int	UR_switch = 0;		/* default is time */
int	UR_time = 300;		/* default is 5 min. (300 sec) */
int	UR_num_chg = 50;	/* default is 50 changes? */
extern int	UR_chg_cnt;	/* Unibase change counter - initially 0 */
/*********************************************************************
**    E_FUNCTION     :  ur_cond_auto_save()
**       check to see if we should perform an auto save
**			and if conditions are appropriate then do so.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_cond_auto_save()
{
/*--------------------------------------------------------------------
**	Start of Executable Code
**--------------------------------------------------------------------
**
*/
	char buf[120];
	uu_denter(UU_RTRC,(us,"ur_cond_auto_save"));
	/* check if autosave is enabled and if the elapsed time since last */
	/* autosave exceeds the autosave threshold */
	if(UR_ason)
	{
		if((UR_switch == 0 && (time((long *) 0) - UR_last_autosave > UR_time)) ||
			(UR_switch == 1 && (UR_chg_cnt >= UR_num_chg)))
		{
/*
.....we don't want display a error box
.....Yurong changed 9/14/98
*/
/*			uu_uerror0(UU_UBASE,21);		/* show user autosave happening */
			uu_ugerror0(UU_UBASE,21,buf);		/* show user autosave happening */
			ud_prmerr(buf);
			ur_auto_save_part();
			UR_last_autosave = time((long *) 0);/* capture time of save */
			UR_chg_cnt = 0;		/* reset number of changes */
		}
	}
	uu_dexit ;
}


/*********************************************************************
**    E_FUNCTION     :  ur_enable_auto_save()
**       turns on the autosave feature
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : sets UR_ason to true and opens the autosave file
**    WARNINGS     : none
*********************************************************************/
ur_enable_auto_save()
{
	int status;
/*
.....No need to do anything if the auto save unibase has
.....already been created - ASF 2/19/14.
*/
	if (UR_ason) return;
	uu_denter(UU_RTRC,(us,"ur_enable_auto_save"));
	UR_ason = UU_TRUE;
	UR_last_autosave = time((long *) 0);/* capture time of save */
	UR_chg_cnt = 0;
}


/*********************************************************************
**    E_FUNCTION     :  ur_disable_auto_save()
**       turns off the autosave feature
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : sets UR_ason to false and closes the autosave file
**    WARNINGS     : none
*********************************************************************/
ur_disable_auto_save()
{
	uu_denter(UU_RTRC,(us,"ur_disable_auto_save"));
	UR_ason = UU_FALSE;
	uu_dexit ;
}


/*********************************************************************
**    E_FUNCTION     :  ur_snap_save()
**       force an autosave whether enabled or not and regardless
**			of whether the normal conditions are met.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_snap_save()
{
	int status;

	uu_denter(UU_RTRC,(us,"ur_snap_save()"));
	status = UU_SUCCESS;
	ur_auto_save_part();
	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     :  ur_set_auto_save_condition()
** 		set switch between autosave by time or by number of database
**			 changes since the last autosave and get the time interval or
**			 number of changes needed to require an autosave
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : set globals UR_switch, UR_time, UR_num_chg used
**							to determine if conditions for autosaving are met
**    WARNINGS     : none
*********************************************************************/
ur_set_auto_save_condition()
{
	char	swtch[80];	/* buffer for answer to switch setting */
	int	length;		/* length of buffer to and from das */
	int	thyme;			/* buffer to read time into */
	int	num_chg;		/* buffer to read number of database changes */

	uu_denter(UU_RTRC,(us,"ur_set_auto_save_condition()"));
	/* get the switch setting, save by time or # of changes? */

reject:
	length = sizeof(swtch);
	ud_ldas(UD_DASSTRING,UU_UBASE,7,swtch,length,&length,UD_NODEFAULT);

	uu_dprint(UU_RTRC,(us,"ur_set_auto_save_condition:swtch = %s",swtch));
	if(swtch[0] == 't')
	{
		UR_switch = 0;		/* set condition to test in cond_save */

		/* condition is time so get the interval */
		ud_ldas(UD_DASINT,UU_UBASE,8,&thyme,1,&length,UD_NODEFAULT);
		uu_dprint(UU_RTRC,(us,"ur_set_auto_save_condition:time interval = %d",thyme));

		/* check for reasonable time? */
		UR_time = thyme * 60;		/* convert to seconds */
	}
	else if(swtch[0] == 'n')
	{
		UR_switch = 1;		/* set condition to test in cond_save */

		/* condition is number of database changes so get the threshold num */
		ud_ldas(UD_DASINT,UU_UBASE,9,&num_chg,1,&length,UD_NODEFAULT);

		/* check for reasonable number of changes? */
		UR_num_chg = num_chg;
	}
	else		/* not either t or n -- reject */
	{
		uu_uerror0(UU_UBASE,22);		/* demand correct input. */
		goto reject;
	}
	uu_dexit;
}
