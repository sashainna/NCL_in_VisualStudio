/*********************************************************************
**    NAME         :  garchall.c --  subroutines for archiving atructures.
**       CONTAINS:
**			garchall() - archive all currently defined structures.
**			gretrall() - retrieve all structures.
**      	gqarchids(num_strs, str_ids) -- inquire archive ids.
**
**		An archive file has the following layout:
**			byte offset			contents
**			from file start
**			0						file length
**			4						Size of table of contents (Multiple of TOFCINC
**									not usually full).
**			8						Number of structures in the archive file.
**			12						id of first stucture
**			16						(word) length of first structure. 0=not in file.
**			20						file (word) offset of first structure's data.
**			12*(i)+12			id of ith stucture.
**			12*(i)+16			(word) length of ith structure. 0=not in file.
**			12*(i)+20			(word) offset of ith structure(i=0..N-1)
**
**		Within each structure, the layout is:
**			byte offset from			
**			start of structure	contents
**			0							M=number elements in structure.
**			4							L=word length of 1st element.
**			8 to 8+4*L				1st element data.
**			etc for rest of elements.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
** 
**    MODULE NAME AND RELEASE LEVEL
**       garchall.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:17
*********************************************************************/

#include <stdio.h>
#include "garch.h"
#include "gerrorst.h"
#include "gerrorid.h"
#include "gerrormet.h"
#include "gomisc.h"
#include "gtbldef.h"
#include "udebug.h"

char *ug_lsielt();

/*********************************************************************
**    E_FUNCTION     :  Gerror garchall() -- archive all structures.
**			 Archive all currently defined structures into an archive file.
**
**				*** NOT IMPLEMENTED YET ***
**
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror garchall()
{
	Gint str_count;
	Gint str_ids[1];
	Gerror irtn=NCL_NO_ERROR;

	uu_denter(UU_GTRC,(us,"garchall()"));

	/* archive them all */
	str_count=0;	/* Here need to  traverse all structures and find count */
	garchstr(str_count, str_ids);

	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     :  Gerror gretrall() --  retrieve all structures.
**				Retrieve all structures in the open archive file.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gretrall()
{
	Gint num_strs;
	Gint *str_ids;

	uu_denter(UU_GTRC,(us,"gretrall()."));

	/* allocate memory for the str_ids table 
	* 	an automatic array (ie str_ids[UG_MAXSEGNO]) causes local variable
	*	overflow on the Silicon Graphics.
	*/
	str_ids = (Gint *)uu_toolmalloc( UG_MAXSEGNO * sizeof(Gint) );
	if (str_ids==NULL) {
		ug_errorhand(EMEMSPAC,"gretrall",NULL);
		return(EMEMSPAC);
	}

	/* inquire archived structure count and their ids */
  	gqarchids (&num_strs, str_ids);
	uu_dprint(UU_GTRC,(us,"gretrall. no structures=%d",num_strs));
	uu_dprint(UU_GTRC,(us,"structure ids[0..1]=%d,%d",
			str_ids[0],str_ids[1]));

  	/* no structures read -- post error */
	if (num_strs == 0) {
	 	fprintf(stderr,"no segments in the archive file");
	 	uu_dprint(-1,(us,"no segments in the archive file"));
	}

	/* retrieve them all */
  	gretrstr(num_strs, str_ids);

	/* free the locally allocated storage */
	uu_toolfree( str_ids );

	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    E_FUNCTION     :  gqarchids(num_strs, str_ids) -- inquire archive ids.
**			Inquire number and ids of all structures in an archive file.
**    PARAMETERS   
**       INPUT  : 	none
**       OUTPUT :  Gint *num_strs -- number of structures in archive file.
**						 Gint str_ids[] -- array of structure ids.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gqarchids(num_strs, str_ids)
/*$ OUTPUT */
 Gint *num_strs;
 Gint str_ids[];
{
	Gint i;
	UG_Tofc *h;			/* A table of contents entry */
	Gint nstructs;		/* Length of table of contents */

	uu_denter(UU_GTRC,(us,"gqarchids()"));

	/* initialize the number of structures found */
	*num_strs = 0;

	nstructs = ug_lsinelt(ug_tofc);
	uu_dprint(UU_GTRC,(us,"gqarchids narch=%d",nstructs));

 	/* For each entry in the table of contents */
	for (i=0; i<nstructs; i++) {

      /* If structure length > 0, the structure exists */
		h = (UG_Tofc *) ug_lsielt(ug_tofc, i);
      if (h->len > 0) {
			str_ids[*num_strs] = h->id; /* count it */
			(*num_strs)++;
		}
	}

	uu_dprint(UU_GTRC,(us,"gqarchids(%d, ids=",*num_strs));

	for (i=0; i < *num_strs; i++)	{
		uu_dprint(UU_GTRC,(us,"%d",str_ids[i]));
	}

	uu_dexit;
}
