/*********************************************************************
**    NAME         :  feacalc.c
**       CONTAINS:
**			int um_feacalc( )
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       feacalc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:45
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "usysg.h"
#include	"gtbl.h"
#include "g.h"
#include "class.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mdattr.h"
#include "mattr.h"
#include "mfeatcom.h"
#include "mdebug.h"
#include "mdpick.h"
#include "view.h"
#include "zsysdep.h"
#include "uhep.h"
#include "gerrorst.h"

/*********************************************************************
**    E_FUNCTION     :  int um_feacalc( )
**			Call the class routine to calculate various orders of 
**			features after setting up the DIGS display segment to
**			display the features in.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_feacalc()

	{     
   struct UC_entitydatabag ent;
   int feature;
   int i;
	int status;
	UM_PICKENT pent;
	UM_transf tfmat;
	uv_segbuff(seg_buf);
   int dsegid;
   int cur_mclr;
   int cur_lclr;
   int cur_tclr;
	UU_REAL cur_ch;
	Glntype cur_line, *style_ptr;
	Glntype new_line;
	UU_KEY_ID viewed_in;

	uu_denter(UU_MTRC,(us,"um_feacalc()"));

	status = UU_SUCCESS;
	for (i=0 ; i < UM_MAXFPICK ; i++ )
	{
		status = 0;
		if (UM_Features[i].order[0] != 0) 
		{
			/* get geometric information from UNIBASE */
			status = um_d_pickresolve(&UM_Features[i].ploc.ppath, 1, &pent);
			if (status != UU_SUCCESS) break;

			ent.key = um_get_pickkey(&pent, 1);
			status = uc_retrieve_data(&ent, sizeof(ent));
			if (status != UU_SUCCESS) break;

			status = uc_retrieve_transf(ent.key, tfmat);
			if (status != UU_SUCCESS) break;
	      /* get the feature order for the selected geometry */
			feature = UM_Features[i].order[0];
			UM_Features[i].order[feature] = 1;
			UM_Features[i].view[feature] = UM_Features[i].view[0];
	
			/* delete current feature segment for entity */
			if (UM_Features[i].seg != -1) 
				gdeleteseg(UM_Features[i].seg);

			/* create a new GKS segment and get its id */
			dsegid = gnseg();
  	   		seg_buf[0] = UM_FEAT_REL;
/*
.....save entity key and the view key also
.....if we don't di it, it will cause picking in view
.....and segmentation vilation error when try to access
.....the feature entity by using segment
*/
			ur_retrieve_view_key(ent.key, &viewed_in);
  	   		seg_buf[4] = viewed_in;
			seg_buf[1] = ent.key;

  	    	if( gcreateseg(dsegid) != NCL_NO_ERROR )
				uu_sys_err_recovery(/* Too many segments */ -1, UM_MODEL, 98, 0, 0);
      		gsegwud(dsegid, seg_buf);
			UM_Features[i].seg = dsegid;
	
	      /* save the color indices for markers, lines and text */
			cur_mclr = gqmarkcolor();
			cur_lclr = gqlinecolor();
			cur_tclr = gqtextcolor();
			cur_ch   = gqcharheight();
			style_ptr= gqlinetype();
			zbytecp(cur_line, *style_ptr);

	      /* set the marker, line and text colors to the features color */
			gsmarkcolor (UM_dispattr.featclr);
			gslinecolor (UM_dispattr.featclr);
			gstextcolor (UM_dispattr.featclr);
			new_line.typeno = UM_SOLID_LINE;
			new_line.npatn = 0;
			gslinetype(&new_line);

			/* set normalization transformation */
			gsnormtran(UM_Features[i].view[0]);

			/* place the desired features into the currently opened GKS segment */
			for (feature=1; feature<UM_NORDER; feature++)
			{
				if (UM_Features[i].order[feature] != 0)
				{
					status = uc_feature(&ent, tfmat, feature, &UM_Features[i].ploc);
					if (status==-1) break;
				}
			}
			/* restore the marker, line and text colors */
			gsmarkcolor (cur_mclr);
			gslinecolor (cur_lclr);
			gstextcolor (cur_tclr);
			gscharheight(cur_ch);
			gslinetype  (&cur_line);
	
	      /* close the GKS segment */
			gcloseseg();
			if (status==-1) break;
		}
   	}
	uu_dexit;
	return(status);
	}
