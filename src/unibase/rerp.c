/*********************************************************************
**    NAME         :  rerp.c
**       CONTAINS:
**       uri_rec_part()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       rerp.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:34
*********************************************************************/

#define	UR_BLKRCL	1024	/* block record length					*/
#include "udebug.h"
#include "umoveb.h"
#include "ribase.h"
#include "xenv1.h"

UU_LOGICAL UR_ason;   /* autosave on flag */
int        UR_asfile = -1; /* autosave file descriptor */

/*********************************************************************
**    E_FUNCTION     :  uri_rec_part()
**			recover a part saved by autosave
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uri_rec_part()
{
	char		*uu_toolmalloc();
	int		iostat;				/* holds status of i/o calls */
	int		status;				/* holds status of unibase calls */
	int		rpfd;					/* autosave file descriptor */
	int		length;				/* returned length from blocked reads */
	long		rel;					/* relationship counter */
	long		nxt_ent;				/* next entry in relation */
	int		i, j;						/* indexes */
	unsigned int	*w_ptr;		/* a pointer to an integer */
	int		lst_len;				/* length of a list in bytes */
	int		atom_size;			/* size of each list entry */
	int		atom_cnt;			/* number of atoms in the list */
	UR_list_parms	*save_parms[UR_MAX_REL];
	int		page;					/* page number of entries */
	int		page_start_ndx;	/* start entry index on the page */
   UX_pathname fullpath;
   int   mode;
   int   nb;

	uu_denter(UU_RTRC,(us,"uri_rec_part, recover Unibase"));
	status = 0;
	for(rel = 0; rel <= UR_MAX_REL; rel++)
	{
		save_parms[rel] = UR_rcb[rel].lparms;	/* save the existing list parms */
	}

	/* now open the autosaved part file and */
	/* read the rcb array for both used and unused relations.  */
	if(UR_ason)
	{
		rpfd = UR_asfile;		/* autosave has the file open and rewound */
	}
	else
	{
      /* open the old autosave file: */
      mode= 0;
      ux_mk_chk_syspath(UU_NULL, ".", "autosave", UU_NULL, UU_NULL,
         &mode, fullpath, UX_PRTERRS);
#if UU_COMP == UU_WIN2K
      ux_open(fullpath,"rb+" ,"BLOCKED", "BINARY", &rpfd, UX_PRTERRS);
#else
      ux_open(fullpath,"r+" ,"BLOCKED", "BINARY", &rpfd, UX_PRTERRS);
#endif
      ux_file_rewind(rpfd, UX_PRTERRS);  /* set pointer to after file header */
	}
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
	length =  UR_NUM_REL*sizeof(struct UR_rcb_rec);
	nb = 1;
   iostat = ux_read_block(rpfd, UR_rcb, &length, &nb, UX_PRTERRS);
	uu_dprint(UU_RITRC,(us,"uri_rec_part: %d bytes RCBs.",length));
	if (length != UR_NUM_REL*sizeof(struct UR_rcb_rec))
	{
		uu_dprint(-1,(us,"ERROR:uri_rec_part improper length for RCB read."));
		status = -1;
		goto r_p99;
	}
	for(rel = 0; rel <= UR_MAX_REL; rel++)
	{
		UR_rcb[rel].lparms = save_parms[rel];	/* restore list parms */
	}

	/* now read the relation info for each relation which is initialized and */
	/* contains active tuples */
	status = 0 ;
	for (rel = 0; rel <= UR_MAX_REL; rel++)
	{
		/* first determine if initialized relation with active tuples */
		if(UR_rcb[rel].status >= 0 && UR_rcb[rel].active_tuple_cnt > 0)
		{
			/* we got a hot one - first read the list parms if any */
			uu_dprint(UU_RITRC,(us,"uri_rec_part: Rel %d '%s'.",rel,
							UR_rcb[rel].relname));
			if (UR_rcb[rel].n_varl > 0)
			{
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
				length =  UR_rcb[rel].n_varl * 2 * sizeof(int);
				nb = 1;
            iostat = ux_read_block(rpfd,UR_rcb[rel].lparms,
               &length, &nb, UX_PRTERRS);
				uu_dprint(UU_RITRC,(us,"uri_rec_part: %d bytes lparms.",length));
				if (length != UR_rcb[rel].n_varl * 2 * sizeof(int))
				{
					uu_dprint(-1,(us,
							"ERROR:uri_rec_part improper length for list parms read."));
					status = -1;
					goto r_p99;
				}
			}

			/* next read the actual entries */
			uu_dprint(UU_RITRC, (us,"uri_rec_part:#entries %d",UR_rcb[rel].n_ent));

			/* allocate the page table */
			UR_rcb[rel].ent_ptr = (UR_page *)uu_toolmalloc(sizeof(UR_page) *
							( (int)((UR_rcb[rel].n_ent + UR_rcb[rel].page_size - 1) /
							UR_rcb[rel].page_size)));
			page = 0;
			page_start_ndx = 1;
			while (page_start_ndx <= UR_rcb[rel].n_ent)
			{
				UR_rcb[rel].ent_ptr[page].pg_ptr = uu_toolmalloc(
						( (UR_rcb[rel].n_ent < page_start_ndx + UR_rcb[rel].page_size)
 							? UR_rcb[rel].n_ent - page_start_ndx + 1
							: UR_rcb[rel].page_size )
						* (UR_rcb[rel].tuple_size + UR_rcb[rel].var_info_size));
				if(UR_rcb[rel].ent_ptr[page].pg_ptr == 0)
				{
					uu_dprint(-1,(us,"ERROR:uri_rec_part malloc error."));
					status = -1;
					goto r_p99;
				}
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
				if ( UR_rcb[rel].last_active_index > 
							page_start_ndx + UR_rcb[rel].page_size)
					length =  UR_rcb[rel].page_size *
								(UR_rcb[rel].tuple_size+UR_rcb[rel].var_info_size);
				else
					length =  (UR_rcb[rel].last_active_index - page_start_ndx + 1) *
								(UR_rcb[rel].tuple_size+UR_rcb[rel].var_info_size);
				nb = 1;
				iostat = ux_read_block(rpfd, UR_rcb[rel].ent_ptr[page].pg_ptr,
            		&length, &nb, UX_PRTERRS);
				uu_dprint(UU_RTRC,(us,"uri_rec_part: %d bytes entries.",length));
				if (length != ( (UR_rcb[rel].last_active_index > page_start_ndx +
						UR_rcb[rel].page_size)
 							? UR_rcb[rel].page_size
 							: UR_rcb[rel].last_active_index - page_start_ndx + 1) *
						(UR_rcb[rel].tuple_size+UR_rcb[rel].var_info_size))
				{
					uu_dprint(-1,(us,"uri_rec_part: improper length."));
					uu_dprint(-1,(us,"should be:%d",
						( (UR_rcb[rel].last_active_index > page_start_ndx +
							UR_rcb[rel].page_size)
 							? UR_rcb[rel].page_size
 							: UR_rcb[rel].last_active_index - page_start_ndx - 1) *
								(UR_rcb[rel].tuple_size+UR_rcb[rel].var_info_size)));
					status = -1;
					goto r_p99;
				}
				page_start_ndx += UR_rcb[rel].page_size;
				page++;
			}

			/* and finally the bitmaps for the entry allocation */
			UR_rcb[rel].bmap_ptr = (unsigned int *)uu_toolmalloc(
								(UR_rcb[rel].bmap_size)*UR_NUM_BMAPS*sizeof(int));

			if(UR_rcb[rel].bmap_ptr == 0)
			{
				uu_dprint(-1,(us,"ERROR:uri_rec_part malloc error."));
				status = -1;
				goto r_p99;
			}
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
			length =  UR_rcb[rel].bmap_size * UR_NUM_BMAPS * sizeof(int);
			nb = 1;
         iostat = ux_read_block(rpfd, UR_rcb[rel].bmap_ptr,
            &length, &nb, UX_PRTERRS);
			uu_dprint(UU_RTRC,(us,"uri_rec_part: %d bytes bitmaps.",length));
			if (length != UR_rcb[rel].bmap_size * UR_NUM_BMAPS * sizeof(int))
			{
				uu_dprint(UU_RTRC,(us,"uri_rec_part: improper length."));
				status = -1;
				goto r_p99;
			}
			/* copy the allocation bitmap to the save/load bitmap to mark */
			/* the tuples as newly loaded */
			uu_move_byte(UR_rcb[rel].bmap_ptr,
						&(UR_rcb[rel].bmap_ptr[UR_SVLD_MAP*UR_rcb[rel].bmap_size]),
						UR_rcb[rel].bmap_size*sizeof(unsigned long));

			/* now read the variable length lists, if any */
			for(i = 1 ; i <= UR_rcb[rel].n_varl; i++)
			{
				/* step through the active entries and read the list */
				nxt_ent	= 1	;
				status = 0 ;
				ur_get_atom_size(rel, i, &atom_size);
				while(!ur_get_next_tuple_index(rel,&nxt_ent))
				{
					status = ur_get_varlist_ptr(rel, nxt_ent,i, &w_ptr, &atom_cnt);
					lst_len = atom_cnt * atom_size;
					if(lst_len > 0 && status == 0)
					{
						w_ptr = (unsigned int *)uu_toolmalloc(lst_len);	/* get space */
						if(w_ptr == 0)
						{
							uu_dprint(-1,(us,"ERROR:uri_rec_part malloc error."));
							status = -1;
							goto r_p99;
						}
						ur_update_varlist_info(rel,nxt_ent,i,w_ptr,atom_cnt);
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
						length =  lst_len;
						nb = 1;
                  iostat = ux_read_block(rpfd, w_ptr, &length, &nb, UX_PRTERRS);
						uu_dprint(UU_RTRC,(us,"uri_rec_part: %d bytes list %d.",
									length,i));
						if (length != lst_len)
						{
							uu_dprint(UU_RTRC,(us,"uri_rec_part: improper length."));
							status = -1;
							goto r_p99;
						}
					}	/* if list not 0 length */
					nxt_ent++ ;
				}	/* while got active tuple */
			} /* for each variable list */
		}	/* if initialized relation with active tuples */
		else if(UR_rcb[rel].status >= 0)	/* active but no tuples - just clr bmap*/
		{
			uu_dprint(UU_RITRC,(us,"uri_rec_part: Rel %d '%s'.",rel,
							UR_rcb[rel].relname));
			UR_rcb[rel].bmap_ptr = 0;
			UR_rcb[rel].bmap_size = 0;
			UR_rcb[rel].ent_ptr = 0;
		}	/* if active(no tuples) */
	}	/* for each relation */
	status = ur_recover_environ(rpfd);

r_p99:
	if(UR_ason)
	{
      ux_file_rewind(rpfd,UX_PRTERRS);    /* rewind ready for next time */
	}
	else
	{
      iostat = ux_close(rpfd, UX_PRTERRS);
	}
	uu_dexit ;
	return(status)	;
}
