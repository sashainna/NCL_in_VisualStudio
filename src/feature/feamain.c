/*********************************************************************
**    NAME         :  feamain
**       CONTAINS:
**       feamain
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       feamain.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:45
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include	"gtbl.h"
#include "g.h"
#include "goatt.h"
#include "mfeatcom.h"

/*********************************************************************
**    E_FUNCTION     :  int um_feamain()
**      control the definition and display of features
**       and the construction of the features tables 
**  PARAMETERS   
**      input:  none 
**      output: none
**  RETURNS      :  
			UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int
um_feamain()
	{
	Gtxfp *textptr;	/* pointer to the type of text the system is currently
							 * using.
							 */
	Gtxfp oldtextype, /* storage for the old (current) text type */
			newtextype;	/* storage for the text type we will use here */
   int stat;
	int i;

	uu_denter(UU_MTRC,(us,"um_feamain()"));

	/* pick the entities and their associated feature orders */
	stat = um_feapick(); 

   if (stat == UU_SUCCESS)
  	 	{
		/* save current text type */
		textptr = gqtextfp();
		oldtextype.font = textptr->font;
		oldtextype.prec = textptr->prec;

		/* set new text type */
		newtextype.font = 1;
		newtextype.prec = UG_STRING; 
		gstextfp(&newtextype);

		/* calculate the new feature orders, store the results in the
			feature tables, and display in DIGS */
		stat = um_feacalc(); 

		/* reset old text type */
		gstextfp(&oldtextype);
  	 	}

	uu_dexit;
	return(stat);
	}
