/*********************************************************************
**    NAME         :  rirdenv.c
**       CONTAINS:
**       ur_rd_txt_env()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rirdenv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:46
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) rirdenv.c 3.2 11/2/87 15:17:30 single"};
#else
static char uu_sccsident[]={"@(#) rirdenv.c 3.2 11/2/87 15:17:30 double"};
#endif
#endif

#include "udebug.h"
#include "usysdef.h"
#include "rienvtab.h"
#include "rerrdef.h"
#include "rbase.h"
#include "ribase.h"
#include "rmtuple.h"
#include "riddldef.h"
#include "nclver.h"
#include "rver9400.h"
#include "rver9700.h"
extern int UR_readenv;
extern int UR_active, UR_dctindex;

/*********************************************************************
**    E_FUNCTION     :  ur_rd_txt_env(envfd)
**       load the new (buffer) environment from file envfd
**    PARAMETERS   
**       INPUT  : 
**				envfd	int			file descriptor for the file to load from
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful else error code
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_rd_txt_env(envfd)
int			envfd;				/* file descriptor to load from */
{
	extern struct UR_env_table_rec UR_environ_table[];

#define	atmax	64
	struct attr_def	atdefs[atmax];	/* attribute definitions */
	int			num_attr;				/* number of attributes parsed */
	struct UR_data	*ent_ptr;			/* a pointer to an entry				*/
	int			rel_typ;
	int			status;
	int			k,vn;
	char			*name;	/* pointer to fix name */

	uu_denter(UU_RTRC,(us,"ur_rd_txt_env(%d)", envfd));
	status = ur_chk_data_dict() ;
	if (status != 0)
	{
		uu_dprint(-1,(us,"ERROR:ur_rd_txt_env can't init data dict."));
		uu_dexit;
		return(-1);				/* return error code */
	}

	/* read env file into new (buffer) copy of environment */
	UR_readenv = 1;
/*
......if it is the old version < 9.4
......then use the old UM_mtrlmdl_rec_old strcutre UM_mtrlmdl_old to read
*/
	if (NCL_infile_version<9.350)
	{
		UR_environ_table[1].adrs = (char *)&UM_mtrlmdl_old;
		UR_environ_table[1].new_adrs = (char *)&UM_new_mtrlmdl_old; 
		UR_environ_table[1].length = sizeof(struct UM_mtrlmdl_oldrec);
	}
/*
......if it is the old version < 9.7
......then use the old UM_labelmdl_rec96 strcutre UM_labelmdl to read
*/
	if (NCL_infile_version<9.651)
	{
		UR_environ_table[4].adrs = (char *)&UM_labelmdl96;
		UR_environ_table[4].new_adrs = (char *)&UM_new_labelmdl96; 
		UR_environ_table[4].length = sizeof(struct UM_labelmdl_rec96);
	}
	for(k = 0; UR_environ_table[k].name[0] != '\0'; k++)
	{
		uu_dprint(UU_RITRC,(us,"reading %s",UR_environ_table[k].name));

		/* get the data dictionary definition */
		name = UR_environ_table[k].name + 3;	/* skip prefix ---kludge--- */
		
		UR_dctindex = ux_get_dct_index();
		vn   = UR_dctindex - 1;
		if ((UR_dctindex == 0) && (NCL_infile_version >= 9.350)) 
		{
			num_attr = ur_data_dict(atdefs, atmax, name,&rel_typ);
		}
		else
		{
			num_attr = ur_data_dict_o(atdefs,atmax, name,&rel_typ, vn);
		}		

		uu_dprint(UU_RITRC,(us,"get:%s - %d attributes",UR_environ_table[k].name,
				num_attr));
		if(num_attr <= 0)
		{
			uu_dprint(-1,(us,"ERROR: unable to find rel %s definition for load",
					name));
			status = URM_RELNTFND;	/* relation not in data dictionary error */
			goto exit;
		}
		ent_ptr = (struct UR_data *)UR_environ_table[k].new_adrs;
		ur_rd1_txt_ent(envfd, ent_ptr, num_attr, atdefs);
	}
exit:	/* error bailout */
	uu_dexit;
	return(0);
}
