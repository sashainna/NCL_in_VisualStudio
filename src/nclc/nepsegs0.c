/*********************************************************************
**    NAME         :  nepsegs0.c
**       CONTAINS: routines to handle the display segment list Spseglist
**          ncl_psegs_init0
**          ncl_psegs_init
**          ncl_free_segs
**          ncl_get_psegs
**          ncl_getnum_psegs
**          ncl_psegs_push
**    COPYRIGHT 2007 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nepsegs0.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:43
*********************************************************************/
#include "ulist.h"
#include "goseg.h"

static UU_LIST Spseglst;
static int Spseglst_init = 0;

/*********************************************************************
**    E_FUNCTION     :  void ncl_psegs_init0()
**       NULL-initialize the list
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_psegs_init0()
{
	uu_list_init0 (&Spseglst);
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_psegs_init (n0)
**       Initialize the list
**    PARAMETERS   
**       INPUT  : 
**          n0    - init parameter
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_psegs_init(n0)
int n0;
{
	if (Spseglst_init == 0 && n0 > 0)
	{
		uu_list_init (&Spseglst,sizeof(Gseg),n0,n0);
		Spseglst_init = 1;
	}
}

/*********************************************************************
**    E_FUNCTION     :  ncl_free_segs()
**       Free the list, after erasing the display segments
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_free_segs ()
{
	int i,nseg;
	Gseg *psg;

	if (Spseglst_init == 1)
	{
		nseg = Spseglst.cur_cnt;
		psg = (Gseg *) UU_LIST_ARRAY (&Spseglst);

		for (i = 0; i < nseg; i++, psg++)
		{
			gdeleteseg(psg[0]);
		}
		uu_list_free (&Spseglst);

		Spseglst_init = 0;
	}
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_get_psegs (nseg,psg)
**       Get the list data
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          nseg   - list current counter
**          psg    - list array pointer
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_psegs (nseg,psg)
int *nseg;
Gseg **psg;
{
	if (Spseglst_init == 1)
	{
		*nseg = Spseglst.cur_cnt;
		*psg = (Gseg *) UU_LIST_ARRAY (&Spseglst);
	}
	else
	{
		*nseg = 0;
		*psg = UU_NULL;
	}
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_getnum_psegs (nseg)
**       Get the number of elements stored in the list
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          nseg   - list current counter
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_getnum_psegs (nseg)
int *nseg;
{
	if (Spseglst_init == 1)
	{
		*nseg = Spseglst.cur_cnt;
	}
	else
	{
		*nseg = 0;
	}
}

/*********************************************************************
**    E_FUNCTION     :  void ncl_psegs_push (segid)
**       Push a new display segment on the list
**    PARAMETERS   
**       INPUT  : 
**          segid  - segment id
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_psegs_push (segid)
Gseg *segid;
{
	if (Spseglst_init == 0) ncl_psegs_init (10);
	uu_list_push (&Spseglst,segid);
}
