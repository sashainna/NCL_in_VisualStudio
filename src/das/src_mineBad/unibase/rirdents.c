/*********************************************************************
**    NAME         :  rirdents.c
**       CONTAINS:
**			ur_rd_txt_ents()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rirdents.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:46
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) rirdents.c 3.2 11/2/87 15:17:07 single"};
#else
static char uu_sccsident[]={"@(#) rirdents.c 3.2 11/2/87 15:17:07 double"};
#endif
#endif

#include "usysdef.h"
#include	"udebug.h"
#define	UR_BLKRCL	1024	/* block record length					*/
#include "umoveb.h"
#include "uhep.h"
#include "ribase.h"
#include "ritrnerr.h"
#include "riddldef.h"
#include "rerrdef.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_rd_txt_ents(lu, rel) 
**  read a Universal Part File into Unibase
**    PARAMETERS   
**       INPUT  : 
**          lu		file to read from
**				rel	relation to read
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful (all other error)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ur_rd_txt_ents(lu, rel)
int			lu;
UR_REL_NUM	rel;
{
	char				*uu_toolmalloc();	/*  external utility to get memory	*/
	extern int		UR_big_atom; 		/* size of the largest atom			*/
	extern int		UR_big_entry; 		/* size of the largest entry			*/

	int			iostat;					/* holds status of i/o calls			*/
	int			status;					/* holds status of unibase calls		*/
	UU_KEY_ID	data_key;				/* a data key							*/
	long			tuple_indx;				/* an entry id							*/
	int			atom_size;				/* atom size for var length data 	*/
	int			atom_ct;					/* atom count for var length data 	*/
	char			*atom_ptr;				/* a pointer to a byte					*/
	struct UR_data	*ent_ptr;			/* a pointer to an entry				*/
	char			*b_ptr;					/* pointer to a byte 					*/
	int			i,j,k,m,n;
	struct UR_lpacket	*lp_ptr;			/* pointer to a variable list packet*/
#define	atmax	64
	struct attr_def	atdefs[atmax];	/* attribute definitions */
	int			num_attr;				/* number of attributes parsed */
	int			atndx;					/* index into atdefs */
	int			atype;					/* the data type of the attribute */
	int			rndx;						/* row index of attribute array	*/
	int			cndx;						/* col index of attribute array	*/
	struct attr_def	v_adefs[atmax];/* attribute definitions - varlist */
	int			v_nattr;					/* number of attributes parsed	*/
	int			v_andx;					/* index into atdefs	*/
	int			v_atype;					/* the data type of the attribute	*/
	int			rel_typ;
	char			*buf_ptr;
	int			lst_len;					/* length of a list in bytes			*/
	int			act_cnt;

	uu_denter(UU_RTRC,(us,"ur_rd_txt_ents(%d, %d)", lu, rel));
	status = ur_chk_data_dict();
	if (status != 0)
	{
		uu_dexit;
		return(-1);				/* return error code */
	}

	/* find largest possible entry and atom size, and allocate space for them */
	ent_ptr = (struct UR_data *) uu_toolmalloc(UR_big_entry);
	if(ent_ptr == 0)
	{
		/* " unable to allocate necessary memory" */
		uu_dprint(-1,(us,"ERROR: unable to allocate buffer in ur_rd_txt_ents"));
		status = URM_NO_MEM_LD;
		goto exit;
	}
	status = 0;

	if(rel >= 0 && rel <= UR_MAX_REL)
	{
		/* get the data definition for the relation */
		num_attr = ur_data_dict(atdefs, atmax, UR_rcb[rel].relname, &rel_typ);
		uu_dprint(UU_RITRC,(us,"get:%s - %d attributes",UR_rcb[rel].relname,
				num_attr));
		if(num_attr <= 0)
		{
			uu_dprint(-1,(us,"ERROR: unable to find rel %s definition for load",
					UR_rcb[rel].relname));
			status = URM_RELNTFND;	/* relation not in data dictionary error */
			goto exit;
		}
		act_cnt = UR_rcb[rel].active_tuple_cnt;
		UR_rcb[rel].active_tuple_cnt = 0;
		for (n = 0; n < act_cnt; n++)
		{
			iostat = ur_rd1_txt_ent(lu, ent_ptr, num_attr, atdefs);
			if(iostat == 0)
			{
				/* put the tuple in Unibase */
				ur_create_tuple(rel, &tuple_indx, ent_ptr);
				ur_rt2k(rel, tuple_indx, &data_key);

				/* mark as a newly loaded tuple */
				ur_load_set(data_key);

				/* now read the variable lengths lists, if any */
				for(i = 1 ; i <= UR_rcb[rel].n_varl; i++)
				{
					ur_get_atom_size(rel, i,&atom_size);
					ur_get_list_packet_ptr(rel, i,ent_ptr,&lp_ptr);
					atom_ct = lp_ptr->atom_cnt;
					if(atom_ct > 0)
					{
						/* get the data definition for the relation */
						atndx = num_attr - UR_rcb[rel].n_varl + i -1;
						if(atdefs[atndx].attr_type == STRING)
						{
							v_nattr = 2;	/* one attr */
							strcpy(v_adefs[1].attr_name, "string");
							v_adefs[1].attr_type = CHARACTER;
							v_adefs[1].num_rows = 1;
							v_adefs[1].num_cols = 1;
							v_adefs[1].attr_off = 1;
						}
						else
						{
							v_nattr = ur_data_dict(v_adefs, atmax,
									atdefs[atndx].attr_name, &rel_typ);
						}
						uu_dprint(UU_RITRC,(us,"get:%s - %d attributes",
								atdefs[atndx].attr_name, v_nattr));
						if(v_nattr <= 0)
						{
							uu_dprint(-1,(us,
								"ERROR: unable to find varlist %s definition for load",
								atdefs[atndx].attr_name));
							status = URM_RELNTFND; /* rel not in data dict */
							goto exit;
						}
						lst_len = atom_ct * atom_size;
						buf_ptr = uu_toolmalloc(lst_len);	/* get space */
						if (buf_ptr != UU_NULL)
						{
							iostat = ur_rd_txt_atoms(lu, buf_ptr, atom_ct, v_adefs,
											v_nattr);
							if(iostat != 0)
							{
								uu_dprint(-1,(us,"ERROR reading varlist"));
								uu_dprint(-1,(us,"iostat = %d, )", iostat));
								status = URM_VARL_RDERR ;
								goto exit;
							}
							ur_update_varlist_info(rel, tuple_indx,
										i,buf_ptr,atom_ct);	/* fix the pointer */
						}
						else
						{
							/* " unable to allocate necessary memory" */
							uu_dprint(-1,(us,
								"ERROR: unable to allocate buffer in ur_rd_txt_ents"));
							status = URM_NO_MEM_LD;
							goto exit;
						}
					}
					else	/* atom count <= 0 */
					{
						ur_update_varlist_info(rel, tuple_indx,
									i,0,0);	/* set zero length */
					}
					lp_ptr++;
				}
			}
			else
			{
				/* " error reading data tuples" */
				uu_dprint(-1,(us,"ERROR reading data tuples"));
				uu_dprint(-1,(us,"iostat = %d", iostat));
				status = URM_MTID_ERR;
				goto exit;
			}
		}
	}
exit:
	uu_dprint(UU_RTRC,(us,"ur_rd_txt_ents exit status = %d",status));
	uu_dexit;
	return(status);
}
