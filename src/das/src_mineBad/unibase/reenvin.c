/*********************************************************************
**    NAME         :  reenvin.c
**       CONTAINS:
**       ur_environ_in()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			reenvin.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:11:29
*********************************************************************/

#include "udebug.h"
#include "usysdef.h"
#include "rienvtab.h"
#include "rerrdef.h"
#include "rbase.h"
#include "ribase.h"
#include "riddldef.h"
#include "xenv1.h"
#include "nclver.h"
#include "rver9400.h"
#include "rver9700.h"

int UR_readenv = 0;
/*********************************************************************
**    E_FUNCTION     :  ur_environ_in(envfd)
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
ur_environ_in(envfd)
int			envfd;				/* file descriptor to load from */
{
	extern struct UR_env_table_rec UR_environ_table[];

	char			*attr_ptr;				/* ptr to attributes within tuple*/
#define	atmax	64
	struct attr_def	atdefs[atmax];	/* attribute definitions */
	char			*data_ptr;				/* ptr to data within attribute	*/
	int			data_offset;			/* offset to next data item	*/
	long			rel_tuple_indx;		/* relative entry id					*/
	long			rel_num;					/* a relation number					*/
	int			atype;					/* the data type of the attribute */
	int			rndx;						/* row index of attribute array	*/
	int			cndx;						/* col index of attribute array	*/
	UU_KEY_ID	key_id;					/* a key_id tuple						*/
	int			atndx;					/* index into atdefs */
	UU_LOGICAL	relocate_keys;			/* true if need to relocate key's	*/
	int			num_attr;				/* number of attributes parsed */
	struct UR_data	*ent_ptr;			/* a pointer to an entry				*/
	int			rel_typ;
	int			i,j;
	int	status;
	int	k;
	int	lrecl;
	char	*name;	/* pointer to fix name */
   int   nb;
#ifdef UU_DEBUGON
	int	m;
	char	prfx[4];
#endif

	uu_denter(UU_RTRC,(us,"ur_environ_in(%d)", envfd));
	relocate_keys = UU_TRUE;
	status = ur_chk_data_dict() ;
	if (status != 0)
	{
		uu_dprint(-1,(us,"ERROR:ur_environ_in can't init data dict."));
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
		/* do blocked read directly into new area */
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
		lrecl =  UR_environ_table[k].length;
		nb = 1;
		ux_read_block(envfd,UR_environ_table[k].new_adrs,&lrecl,&nb,UX_PRTERRS);

		/* check that the read length matches the modal length */
		if(lrecl != UR_environ_table[k].length)
		{
			/* return error message - bad environment file */
			uu_dprint(-1,(us,
					"ERROR:ur_environ_in bad block length %d should be %d",
					lrecl,UR_environ_table[k].length));
			uu_dexit;
			return(UR_BAD_ENV_FILE);
		}
		/* now get the data dictionary definition and relocate any keys */
		name = UR_environ_table[k].name + 3;	/* skip prefix ---kludge--- */
#ifdef UU_DEBUGON
		for (m = 0; m < 3; m++) prfx[m] = UR_environ_table[k].name[m];
		prfx[3] = '\0';
		ur_dump_modal(name, prfx, UU_TRUE);
#endif
		num_attr = ur_data_dict(atdefs, atmax, name, &rel_typ);
		uu_dprint(UU_RITRC,(us,"get:%s - %d attributes",UR_environ_table[k].name,
				num_attr));
		if(num_attr <= 0)
		{
			uu_dprint(-1,(us,"ERROR: unable to find rel %s definition for load",
						name));
			status = URM_RELNTFND;	/* relation not in data dictionary error */
			goto l_p90 ;
		}
		ent_ptr = (struct UR_data *)UR_environ_table[k].new_adrs;

		/* convert the key_id to rel,index */
		key_id = ent_ptr->key_id;
		ur_k2rt(key_id,&rel_num,&rel_tuple_indx);
		if (atdefs[0].attr_type == KEY_ID || atdefs[0].attr_type == KEY_REF)
		{
			/* adjust geometry with key_id's imbedded in the data */
			if(relocate_keys)	/* only relocate if needed	*/
			{
				uri_lp02a(&(ent_ptr->key_id),
							&(UR_rcb[rel_num].bmap_ptr[UR_CALLOC_MAP*UR_rcb[rel_num].bmap_size]));
			}
			attr_ptr = (char *)ent_ptr+sizeof(UU_KEY_ID)+sizeof(long);
			atndx = 1;
		}
		else
		{
			attr_ptr = (char *)ent_ptr;
			atndx = 0;
		}
		for (; atndx<num_attr; atndx++)
		{
			atype = atdefs[atndx].attr_type;
			/* set array indexs, non array has 1 row, 1 col */
			rndx = atdefs[atndx].num_rows;
			cndx = atdefs[atndx].num_cols;
			data_ptr = attr_ptr;
			if(atype == CHARACTER)
			{
				data_offset = atdefs[atndx].attr_off / rndx;
				cndx = 1;			/* no names for each column */
			}
			else
			{
				data_offset = atdefs[atndx].attr_off / (rndx * cndx);
			}
			for(i = 1; i <= cndx; i++)
			{
				for(j = 1; j <= rndx; j++)
				{
					if((atype == KEY_ID) || (atype == REL_ID) || (atype == KEY_REF))
					{
						/* perform key jiggery-pokery */
						ur_k2rt(*((long *)data_ptr), &rel_num,
								&rel_tuple_indx);
						uri_lp02a(data_ptr,
									&(UR_rcb[rel_num].bmap_ptr[UR_CALLOC_MAP*UR_rcb[rel_num].bmap_size])) ;
					}
					data_ptr += data_offset;
				}	/* i loop */
			}	/* j loop */
			attr_ptr += atdefs[atndx].attr_off;
		} /* end massage keys */
	}
l_p90:	/* error bailout */
	uu_dexit;
	return(0);
}
