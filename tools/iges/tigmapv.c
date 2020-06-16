/*********************************************************************
**    NAME         :  tigmapv.c
**       CONTAINS:
**             uig_map_view
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigmapv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:48
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "view.h"
#include "tigtypes.h"
#include "udebug.h"

/* UU_REAL UM_model_size = 203.20; */
UU_REAL UM_model_size = 11.00;

/*********************************************************************
**    I_FUNCTION     :  uig_map_view(dblk,igesin,t,key)
**				Map an IGES view to a unibase view.
**    PARAMETERS   
**       INPUT  : 
**				dblk							IGES directory block
**				igesin							IGES view structure
**				t								associated matrix
**       OUTPUT :  
**				key							UNIBASE key_id
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_map_view(dblk,igesin,t,key)
	struct dir_rec *dblk;				/* IGES arc directory record */
	struct IG_igesvie_rec *igesin;	/* IGES arc parameter record */
	UU_REAL	 t[12];
	UU_KEY_ID *key;
	{
	struct dir_rec lblk;				/* IGES local directory record */
	                              /* IGES plane parameter record */
   struct IG_igespln_rec leftcl, rightcl, topcl, botomcl;
	UV_view view;
	UU_KEY_ID e_key;
	UU_REAL vec[3], vecx[3] ,vecy[3] ,vecz[3], origin[3];
	UU_REAL xmin, xmax, apr, fcl, bcl;
	int i,j,status,irec;
	char p_buff[80];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_map_view"));

	for(i=1;i<5;i++)
		{
		uig_trans_comp(t,i,vec);
		switch(i)
			{
			case 1:
				uig_vctovc(vecx, vec);
				break;
			case 2:
				uig_vctovc(vecy, vec);
				break;
			case 3:
				uig_vctovc(vecz, vec);
				break;
			case 4:
				uig_vctovc(origin, vec);
				um_vctmsc(origin, unit_scale, origin);
				break;
			}
		}
		sprintf(p_buff,"IGES%d", igesin->view_id);

      fcl  = 10000.0;
      bcl  = -10000.0;
      apr  = UM_model_size;
      xmin = -apr * .5;
      xmax = apr * .5;

		/* Mark associated planes as translated */
		if(igesin->left != 0)
/*         irec = igesin->left; */
         uig_get_data (igesin->left,0,&lblk,&leftcl);
			xmin = -leftcl.coef[3];
			update_counts(10, igesin->left);
		if(igesin->top != 0)
			update_counts(10, igesin->top);
		if(igesin->right != 0)
         uig_get_data (igesin->right,0,&lblk,&rightcl);
			xmax = rightcl.coef[3];
			update_counts(10, igesin->right);
		if(igesin->bottom != 0)
			update_counts(10, igesin->bottom);
		if(igesin->back != 0)
			update_counts(10, igesin->back);
		if(igesin->front != 0)
			update_counts(10, igesin->front);

      apr    = xmax - xmin;

		status = uv_create_view(p_buff, UV_USERDEF_VIEW, UV_PARALLEL,
					(UU_REAL) 0.0, origin, vecy, vecz, apr, UV_NOCLIP,
					fcl, bcl, &e_key);

	*key = e_key;

	uu_dexit;
	}
