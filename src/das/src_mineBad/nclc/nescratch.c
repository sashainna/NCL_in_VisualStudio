/*********************************************************************
**    NAME         :  nescratch.c
**       CONTAINS:
**       routines for "scratch file" input/output
**    MODULE NAME AND RELEASE LEVEL
**       nescratch.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:47
*********************************************************************/
#include "usysdef.h"
#include "nclfc.h"
#include "mfort.h"
#include "ulist.h"

static UU_LIST scratchlist[4];
static int scratchlistinit[4] = {0,0,0,0};
static int LEN = 0;

/**********************************************************************
**    E_FUNCTION     : scrfre(kfilp)
**      Free the scratch list.
**    PARAMETERS
**       INPUT  :
**          kfilp  - Index of scratch file list to deallocate.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int scrfre (kfilp)
UM_int4 *kfilp;
{
	int i = *kfilp - 1;
	if (scratchlistinit[i] == 1)
	{
		uu_list_free (&scratchlist[i]);
		scratchlistinit[i] = 0;
	}
	return (0);
}

/**********************************************************************
**    E_FUNCTION     : scropn (kfilp,length,kerr)
**       Initialize the scratch list.
**    PARAMETERS
**       INPUT  :
**          kfilp   -  Index of scratch file list to init.
**          length  -  the data unit size (number of UM_int4 pieces)
**       OUTPUT :
**          kerr    -  1 if error, 0 if no error
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int scropn (kfilp,length,kerr)
UM_int4 *kfilp,*length,*kerr;
{
	int nchunk;
	int i = *kfilp - 1;

	if (scratchlistinit[i] == 1)
		uu_list_free (&scratchlist[i]);

	LEN = *length;
	
	nchunk = LEN * sizeof (UM_int4);
	uu_list_init (&scratchlist[i],nchunk,50,50);

	*kerr = (scratchlist[i].data)? 0: 1;
	scratchlistinit[i] = 1;
	return (0);
}

/**********************************************************************
**    E_FUNCTION     : scrput (kfilp,irec,kerr)
**      Store records into a list.
**    PARAMETERS
**       INPUT  :
**          kfilp   -  Index of scratch file list
**          irec  -  pointer to the data
**       OUTPUT :
**          kerr    -  1 if error, 0 if no error
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int scrput (kfilp,irec,kerr)
UM_int4 *kfilp,*irec,*kerr;
{
	int ncur,i,j;
	int ind = *kfilp - 1;

	*kerr = 1;
	if (scratchlistinit[ind] != 1) return (0);

	i = irec[0];	
	ncur = scratchlist[ind].cur_cnt;
	
	if (i <= ncur)
	{
		UM_int4 *currec;

		currec = (UM_int4 *) UU_LIST_ARRAY (&scratchlist[ind]);
		currec += (i-1)*LEN;
		for (j = 0; j < LEN; j++)
			currec[j] = irec[j];
	}
	else
	{
		for (j = 0; j < (i-ncur); j++)
			uu_list_push (&scratchlist[ind],irec);
	}

	*kerr = 0;
	return (0);
}

/**********************************************************************
**    E_FUNCTION     : scrget (kfilp,krec,irec,kerr)
**       Retrieve data from the list
**    PARAMETERS
**       INPUT  :
**          kfilp -  Index of scratch file list
**          krec  -  index of a data chunk to access
**       OUTPUT :
**          irec  -  retrieved data
**          kerr    -  1 if error, 0 if no error
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
**********************************************************************/
int scrget (kfilp,krec,irec,kerr)
UM_int4 *kfilp,*krec,*irec,*kerr;
{
	int i,j;
	UM_int4 *currec;
	int ind = *kfilp - 1;

	*kerr = 1;
	if (scratchlistinit[ind] != 1) return (0);

	i = *krec;	
	if (scratchlist[ind].cur_cnt < i) return (0);

	currec = (UM_int4 *) UU_LIST_ARRAY (&scratchlist[ind]);
	currec += (i-1)*LEN;
	for (j = 0; j < LEN; j++)
		irec[j] = currec[j];

	*kerr = 0;
	return (0);
}
