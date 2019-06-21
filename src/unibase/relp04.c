/*********************************************************************
**    NAME         :  relp04
**       CONTAINS:
**			ur_lp04
**       
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       relp04.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:32
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include "rmtuple.h"
#include	"udebug.h"
#include	"nccs.h"
#include	"mxxx.h"
#include	"nclver.h"
#include	"mdrel.h"
#include	"class.h"

extern UU_KEY_ID LABTBL_KEY;
extern int UR_verindex;

/*********************************************************************
**    E_FUNCTION     :  status = ur_lp04() 
**  display newly loaded Unibase data
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful
**				<0,	Error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/* display newly loaded(restore) data */
ur_lp04()
{
	int				status;		/* holds status of unibase calls */
	int isav;
	UU_KEY_ID		key_id;		/* a key_id tuple */
	long				tuple_indx;		/* an entry id */
	UU_REAL rsav;
	struct UC_entitydatabag	geom_entity;	/* a entity to draw */
	struct NCL_fixed_databag *e1;

	uu_denter(UU_RTRC,(us,"ur_lp04"));
	status = 0 ;
	e1 = (struct NCL_fixed_databag *) &geom_entity;
/*
.....Define newly loaded lights
*/
	uw_gllight_define();

	/* now display the data we just loaded */
	tuple_indx = 1 ;
	while(tuple_indx > 0)
	{
		ur_get_next_new_tuple_index(UR_MTUPLE_REL, &tuple_indx);
		if(tuple_indx > 0)
		{
			int rel;
			ur_rt2k(UR_MTUPLE_REL, tuple_indx, &key_id);
			ur_update_disp_segid(key_id, -1);	/* force disp seg to none */
			geom_entity.key = key_id;
			ur_retrieve_data_relnum(key_id,&rel);
/*
...vp 3/3/98 label table removed from unibase
...in ncl_post_load
			if (rel == NCL_LABTBL_REL)
			{
				ur_delete_all (key_id);
			}
			else
*/
			{
/*
.....vp 2/22/98 use of ncl_retrieve_data_fixed is bad idea here
.....bacause drafting entity must be converted to sal format which
.....is done only thru uc_retrieve_data (we need to address this in 
.....future)
*/
				status = um_retrieve_data_fixed (e1);
				if (LABTBL_KEY > 0) ncl_fix_attr_label (key_id,e1->rel_num);
/*
.....Update attibutes from older unibase to support new features
*/
				if (NCL_infile_version != NCL_version)
					ur_update_attribut (e1->key,e1->rel_num);

				if(status == UU_SUCCESS)
				{
/*
.....Save and restore input file version
.....it may be changed by text command
.....when loading in a font file
.....Bobby  -  10/1/09
*/
					isav = UR_verindex;
					rsav = NCL_infile_version;
					if (!ncl_tst_un_label(e1)) uc_display(&geom_entity);
					UR_verindex = isav;
					NCL_infile_version = rsav;
				}
			}
			tuple_indx++;
		}
	}
	uu_dexit;
	return(status);
}

