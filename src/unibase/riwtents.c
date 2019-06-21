/*********************************************************************
**    NAME         :  riwtents.c
**       CONTAINS:
**       ur_wrt_txt_ents()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riwtents.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:49
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) riwtents.c 3.2 11/2/87 15:20:15 single"};
#else
static char uu_sccsident[]={"@(#) riwtents.c 3.2 11/2/87 15:20:15 double"};
#endif
#endif

#define	UR_BLKRCL	1024	/* block record length				*/
#include "udebug.h"
#include "usysdef.h"
#include "umoveb.h"
#include "uhep.h"
#include "ribase.h"
#include "ritrnerr.h"
#include "riddldef.h"
#include "rerrdef.h"

/*********************************************************************
**    E_FUNCTION     :  ur_wrt_txt_ents(lu, rel)
**       save entities from a relation as a text file records
**    PARAMETERS   
**       INPUT  : 
**          lu		logical unit (file) to write to
**				rel	relation to be written
**       OUTPUT :  
**          none
**    RETURNS      : 0 if all OK <0 otherwise
**    SIDE EFFECTS : writes to file
**    WARNINGS     : none
*********************************************************************/

ur_wrt_txt_ents(lu, rel)
int			lu;	/* file descriptor for xio file */
UR_REL_NUM	rel;	/* relation to be written */
{
	char		*uu_toolmalloc();		/* memory allocation function				*/
	extern int		UR_big_atom; /* size of the largest atom			*/
	extern int		UR_big_entry; /* size of the largest entry			*/
	extern UU_LOGICAL	UR_sav_all; /* boolean, UU_TRUE if save all		*/

	int		iostat;				/* holds status of i/o calls				*/
	int		status;				/* holds status of unibase calls			*/
	int		get_nxt_status;	/* holds status of get next key_id calls	*/
	int		length;				/* returned lenof filename	*/
	char		*atom_ptr;			/* a pointer to an atom 				*/
	struct UR_data	*ent_ptr;	/* a pointer to an entry 				*/
	long		nxt_ent;				/* next entry in relation				*/
	UU_KEY_ID	rel_key;			/* a relation,tuple index key			*/
	long		abs_tuple_indx;	/* absolute entry id						*/
	long		rel_num;				/* relation number						*/
	int		i,j,k,m;				/* indexes 									*/
	unsigned int	*w_ptr;		/* a pointer to an integer				*/
	int		atom_size;			/* atom size for var length data 	*/
	int		atom_cnt;			/* num of atoms in a particular list*/
	int		lst_len;				/* length of a list in bytes			*/
	UU_LOGICAL data_rel,attr_rel,transf_rel;/* what type rel		*/
	unsigned	long	word		;	/* word to be shifted						*/
	UU_LOGICAL	hit			;	/* whether a bit was set					*/
	long		save_map_disp	;	/* displacement in words to save map	*/
	static	char eofstr[]	= "EOF\n" ; /* eof str for last rcb 		*/
	char		*attr_ptr;			/* ptr to attributes within tuple		*/
	char		*data_ptr;			/* ptr to data within attribute			*/
	int		data_offset;		/* offset to next data item				*/

#define	atmax	64
	struct attr_def	atdefs[atmax];	/* attribute definitions */
	int		num_attr;			/* number of attributes parsed */
	int		atndx;				/* index into atdefs */
	int		atype;				/* the data type of the attribute */
	int		rndx;					/* row index of attribute array	*/
	int		cndx;					/* col index of attribute array	*/
	struct attr_def	v_adefs[atmax];/* attribute definitions - varlist */
	int		v_nattr;				/* number of attributes parsed	*/
	int		v_andx;				/* index into atdefs	*/
	int		v_atype;				/* the data type of the attribute	*/
	int		rel_typ;

	uu_denter(UU_RTRC,(us,"ur_wrt_txt_ents(%d, %d)", lu, rel));
	status = ur_chk_data_dict();
	if (status)
	{
		goto exit;		/* bailout - still no data dictionary */
	}

	/* allocate space for the largest possible entry and atom.  */
	ent_ptr = (struct UR_data *) uu_toolmalloc(UR_big_entry);
	if(ent_ptr == 0)
	{
		status = URM_NO_MEM_SV;
		goto exit;
	}
	uu_dprint(UU_RITRC,(us,"entry buffer allocated at 0x%x", ent_ptr));
	status = 0;

	/* first determine if initialized relation with active tuples */
	if(UR_rcb[rel].status >= 0 && UR_rcb[rel].active_tuple_cnt > 0)
	{
		/* get the data definition for the relation */
		if(UR_rcb[rel].rel_num >= 0 && UR_rcb[rel].rel_num <= UR_MAX_REL)
		{
			num_attr = ur_data_dict(atdefs, atmax,UR_rcb[rel].relname,&rel_typ);
			if(num_attr <= 0)
			{
				status = URM_RELNTFND; /* rel not in data dictionary error */
				goto fail;
			}
		}
		/* determine what type of relation it is, or isn't */
		data_rel		= UU_FALSE ;
		if(uu_tst_bit(&UR_rcb[rel].rel_flags,UR_DATA_REL))
		{
			data_rel = UU_TRUE ;
		}
		attr_rel		= UU_FALSE ;
		if(uu_tst_bit(&UR_rcb[rel].rel_flags,UR_ATTR_REL))
		{
			attr_rel = UU_TRUE ;
		}
		transf_rel	= UU_FALSE ;
		if(uu_tst_bit(&UR_rcb[rel].rel_flags,UR_TRANSF_REL))
		{
			transf_rel = UU_TRUE ;
		}
		/* now write active tuples of the relation */
		nxt_ent = 1;
		status = 0;
		while(!ur_get_next_tuple_index(rel,&nxt_ent))
		{
			ur_rt2k(rel, nxt_ent, &rel_key);

			/* retrieve the fixed data and atom counts */
			status=ur_retrieve_tuple_and_var_info(rel,nxt_ent,ent_ptr);

			/* if a attribute or transf, clear the key_id ?need this? */
			if(attr_rel || transf_rel)
			{
				ent_ptr->key_id = 0;		/* clear the key_id, it isn't used */
			}
			if(!status)		/* check for good status */
			{
				/* write the relation entry, and variable length lists */
				iostat = ur_wrt1_txt_ent(lu, ent_ptr, num_attr, atdefs);	
				if(iostat)
				{
					goto fail;
				}
				/* now output the variable lengths lists, if any */
				for(i = 1 ; i <= UR_rcb[rel].n_varl; i++)
				{
					status = ur_get_varlist_ptr(rel,nxt_ent,i,&w_ptr,
									&atom_cnt);
					ur_get_atom_size(rel,i,&atom_size);
					lst_len = atom_cnt * atom_size;
					if(lst_len > 0 && status == 0)
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
						/* write out the varlist */
						iostat = ur_wrt_txt_atoms(lu, w_ptr, atom_cnt, v_adefs,
									v_nattr);
						if(iostat)		/* check status of write */
						{
							status = -1;
							uu_dprint(-1,(us,
								"ERROR ur_wrt_txt_ents: bad write status %d", iostat));
							goto fail;
						}
					}	/* if list not 0 length */
				}	/* for each varlist */
			}	/* good status (found tuple?) */
			nxt_ent++ ;
		}	/* while got active tuple */
	}	/* if initialized relation with active tuples */

fail:
	if(iostat != 0) status = iostat;	/* if failure on write, return it*/
	uu_toolfree((char *)ent_ptr);

exit:
	uu_dexit;
	return(status);
}
