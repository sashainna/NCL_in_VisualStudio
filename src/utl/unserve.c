/*********************************************************************
**    NAME    :  unserve.c -- generic name server
**
**			Generic number server, after being initialized by a call
**			to uu_nserv_init, calls to uu_nserve_req will return an
**			unused number between max and min, inclusive.  Numbers
**			that are no longer needed are returned with uu_nserv_ret.
**			The number server can be reset by a call to uu_nserv_reset
**			The number server can be deleted by a call to uu_nserv_del.
**			A specific number can be reserved by a call to uu_nserv_resv.
**
**			The number server is implemented as a bit table as is therefore
**			unsuitable for huge spans of numbers.

**			uu_nserv_reinit and uu_nserv_reterm are for use by SAL, exception
**			handler, etc., which may need to re-initialize things.
**
**       CONTAINS:
**				int uu_nserv_init(min, max, id);	initialize a number server
**				int uu_nserv_reinit(min, max, id);	really init a number server
**				int uu_nserv_term(id);				delete a number server
**				int uu_nserv_reterm(id);			really terminate a number server
**				int uu_nserv_req(id);				request a number from a server
**				int uu_nserv_ret(id, number);		return a number to a server
**				int uu_nserv_resv(id, number);	reserve a specific number from a
**																server
**				int uu_nserv_reset(id);				reset a number server
**
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       unserve.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:55
**
**************************************************************************/
#include "umath.h"
#include "udebug.h"
#include "usysdef.h"

#define UU_MAX_NS_IDS 12			/* number of number servers allocated		*/
char *uu_malloc(),*uu_free();

/* use counters for each name server, used by init,term */
static int uu_nserv_usecnt[UU_MAX_NS_IDS]={0,0,0,0,0,0,0,0,0,0,0,0};

	struct {								/* number server structure						*/
		int min;							/* minimun number to be given out			*/
		int max;							/* maximum number to be given out			*/
		int *bit_map;					/* bit map of numbers to be allocated, each
												bit set to one if number in use			*/
		int curr_bit;					/* last bit changed in bit map				*/
	} UU_nserver[UU_MAX_NS_IDS];

/*********************************************************************
**    E_FUNCTION     :  uu_nserv_init(min, max, id)
**      initialize a number server
**			numbers will be served out between min and max inclusive
**
**    PARAMETERS   
**       INPUT  : 
**				min	: minimum number to be returned
**				max   : maximum number to be returned
**				id		: number of number server to init
**
**       OUTPUT :  none
**
**    RETURNS      :  1 if successful, -1 if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_nserv_init(min, max, id)
int min;					/* minimum number to be returned				*/
int max;					/* maximum number to be returned				*/
int id;					/* number of number server to init			*/
{
	int irtn;			/* return parameter								*/
/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**	set ranges, and allocate bit map
*/

	uu_denter(UU_UITRC,(us,"uu_nserv_init(min=%d max=%d id=%d)",min, max, id));

	/* check id to see if in range */
	if ((id >= 0) && (id < UU_MAX_NS_IDS))
		if ((max - min) > 0)
		{
			if (uu_nserv_usecnt[id]==0) 
				irtn=uu_nserv_reinit(min,max,id);	/* initialize */
			else { 
				uu_nserv_usecnt[id]++;
				irtn=1;
			}
		}
	else
	{
		uu_denter2(UU_UITRC,(us,"ERROR uu_nserv_init id > max id, max = %d)",
					UU_MAX_NS_IDS));
		uu_dexit;
		irtn = -1;
	}
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  int uu_nserv_reinit(min,max,id)
**       Really initialize a number server.
**    PARAMETERS   
**       INPUT  : 
**				int min	: minimum number to be returned
**				int max  : maximum number to be returned
**				int id	: number of number server to init
**
**       OUTPUT :  none
**
**    RETURNS      :  1 if successful, -1 if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_nserv_reinit(min,max,id)
int min,max,id;
{
	int range;			/* span of numbers for number server 		*/
	int nwds;			/* number of words in bit map					*/
	int *ptr;			/* used to clear bit map						*/
	int irtn;

	uu_denter(UU_UITRC,(us,"uu_nserv_reinit(%d %d %d)",min, max, id));

	/* check id to see if in range */
	if ((id >= 0) && (id < UU_MAX_NS_IDS))
		if ((max - min) > 0) {
			uu_nserv_reterm(id);
			uu_nserv_usecnt[id]=1;				/* remember this id in use */
			/* set bounds */
			UU_nserver[id].min = min;
			UU_nserver[id].max = max;
			UU_nserver[id].curr_bit = 0;
		
			/* allocate bit map, UU_BITPW is bits per word,
				defined for each system in usysdef.h */
			range = max - min;
			nwds = ceil((UU_REAL)range / (UU_REAL) UU_BITPW);
uu_dprint(UU_UITRC,(us,"max,min = %d %d",max,min));
uu_dprint(UU_UITRC,(us,"range,UU_BITPW,nwds = %d %d %d",range,UU_BITPW,nwds));
			ptr = (int *)uu_malloc(nwds * sizeof(int));
			UU_nserver[id].bit_map = ptr;
		
			/* clear bit map */
			for (; nwds > 0; nwds--)
				*ptr++ = 0;
			irtn = 1;
		}
	else {
		uu_denter2(UU_UITRC,(us,"ERROR uu_nserv_reinit id > max id, max = %d)",
					UU_MAX_NS_IDS));
		uu_dexit;
		irtn = -1;
	}
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     :  uu_nserv_req(id)
**      get an unused number from a number server
**
**    PARAMETERS   
**       INPUT  : 
**				id		: number of number server to get number from
**
**       OUTPUT :  none
**
**    RETURNS      : an unused number if there is one, else min-1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_nserv_req(id) 
int id;				/* number of number server to get number from	*/
{
	int irtn;			/* return parameter										*/
	int range;			/* span of numbers used for number server			*/
/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
** search for an unused number
*/

	uu_denter(UU_UITRC,(us,"uu_nserv_req(%d)", id));
	irtn = UU_nserver[id].min-1;

	/* check id to see if in range */
	if ((id >= 0) && (id < UU_MAX_NS_IDS))
	{
		range = UU_nserver[id].max - UU_nserver[id].min;

		/* look from current bit forward looking for an unset bit */
		for (; range > 0; range--)
		{
			if((UU_nserver[id].curr_bit + UU_nserver[id].min) > UU_nserver[id].max)
				UU_nserver[id].curr_bit = 0;
			if (uu_tst_bit(UU_nserver[id].bit_map, UU_nserver[id].curr_bit) == 0)
			{
				uu_set_bit(UU_nserver[id].bit_map, UU_nserver[id].curr_bit);
				irtn = UU_nserver[id].curr_bit++ + UU_nserver[id].min;
				break;
			}
			else
				UU_nserver[id].curr_bit++;
		}
	}
	else
	{
		uu_denter2(UU_UITRC,(us,"ERROR uu_nserv_req id > max id, max = %d)",
					UU_MAX_NS_IDS));
		uu_dexit;
	}
	uu_denter2(UU_UITRC,(us,"%d=uu_nserv_req(%d)", id,irtn));
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     :  uu_nserv_ret(id, number)
**      return a number to a number server
**
**    PARAMETERS   
**       INPUT  : 
**				id		 : number of number server to return number to
**				number : number to be returned
**
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_nserv_ret(id, number)
int id;				/* number of number server 	*/
int number;			/* number to be returned		*/
{
/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
** clear bit for this number
*/
	uu_denter(UU_UITRC,(us,"uu_nserv_ret(%d %d)", id, number));

	/* check for valid id and bit number */
	if ((id >= 0) && (id < UU_MAX_NS_IDS))
		if ((number >= UU_nserver[id].min) && (number <= UU_nserver[id].max))

			/* clear bit so number is marked as unused */
			uu_clr_bit(UU_nserver[id].bit_map, number - UU_nserver[id].min);
		else
		{
			uu_denter2(UU_UITRC,(us,"ERROR uu_nserv_ret id > max id, max = %d)",
					UU_MAX_NS_IDS));
			uu_dexit;
		}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  uu_nserv_resv(id, number)
**      reserve a specific number from a number server
**
**    PARAMETERS   
**       INPUT  : 
**				id		 : number of number server to use
**				number : number to be reserved
**
**       OUTPUT :  none
**
**    RETURNS      : 1 if number can be reserved, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_nserv_resv(id, number)
int id;				/* number of number server to use				*/
int number;			/* number to be reserved							*/
{
	int irtn;			/* return parameter									*/
	int bit;				/* bit to be checked									*/
/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
** check bit map to see if this number is unused
*/
	uu_denter(UU_UITRC,(us,"uu_nserv_resv(%d %d)", id, number));
	irtn = -1;

	/* check for valid id and bit number */
	if ((id >= 0) && (id < UU_MAX_NS_IDS))
		if ((number >= UU_nserver[id].min) && (number <= UU_nserver[id].max))
		{
			bit = number - UU_nserver[id].min;

			/* check if bit clear */
			if (uu_tst_bit(UU_nserver[id].bit_map, bit) == 0)
			{
				/* set bit so number is reserved */
				uu_set_bit(UU_nserver[id].bit_map, bit);
				irtn = 1;
			}
		}
	if (irtn < 0)
	{
		uu_denter2(UU_UITRC,(us,"ERROR uu_nserv_resv number %d not reserved for %d",
					number, id));
		uu_dexit;
	}
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     :  uu_nserv_term(id)
**      delete a number server.
**
**    PARAMETERS   
**       INPUT  : 
**				id		: number of number server to delete
**
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_nserv_term(id)
int id;
{
	uu_denter(UU_UITRC,(us,"uu_nserv_term(%d)", id));
	if ((id>=0)&&(id<UU_MAX_NS_IDS)) {
		if (uu_nserv_usecnt[id]==1) 
			uu_nserv_reterm(id);
		else uu_nserv_usecnt[id]--;
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  uu_nserv_reterm(id)
**      really delete a number server
**
**    PARAMETERS   
**       INPUT  : 
**				id		: number of number server to delete
**
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_nserv_reterm(id)
int id;				/* number of number server to delete			*/
{
/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**	free bit map
*/
	uu_denter(UU_UITRC,(us,"uu_nserv_reterm(%d)", id));
	
	/* check id to see if in range, free bit map */
	if ((id >= 0) && (id < UU_MAX_NS_IDS)) {
		if (uu_nserv_usecnt[id]!=0) {
			uu_nserv_usecnt[id]=0;
			uu_free(UU_nserver[id].bit_map);
		}
	}
	else {
		uu_denter2(UU_UITRC,(us,"ERROR uu_nserv_del id > max id, max = %d)",
					UU_MAX_NS_IDS));
		uu_dexit;
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  uu_nserv_reset(id)
**			make all numbers available again
**
**    PARAMETERS   
**       INPUT  : 
**				id		: number of number server to reset
**
**       OUTPUT :  none
**
**    RETURNS      :  1 if successful, -1 if not
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_nserv_reset(id)
int id;				/* number of number server to reset			*/
{
	int *ptr;			/* used to clear bit map						*/
	int range;			/* span of numbers in number server			*/
	int nwds;			/* number of words in bit map					*/
	int irtn;			/* return parameter								*/
/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**	clear bit map
*/
	uu_denter(UU_UITRC,(us,"uu_nserv_reset(%d)", id));

	/* check id to see if in range */
	if ((id >= 0) && (id < UU_MAX_NS_IDS))
	{
		/* find size of bit map */
		range = UU_nserver[id].max - UU_nserver[id].min;
		nwds = ceil((UU_REAL)range / (UU_REAL) UU_BITPW);

		/* clear bit map */
		ptr = UU_nserver[id].bit_map;
		for (; nwds > 0; nwds--)
			*ptr++ = 0;
		UU_nserver[id].curr_bit = 0;
		irtn = 1;
	}
	else
	{
		uu_denter2(UU_UITRC,(us,"ERROR uu_nserv_reset id > max id, max = %d)",
					UU_MAX_NS_IDS));
		uu_dexit;
		irtn = -1;
	}
	uu_dexit;
	return(irtn);
}
