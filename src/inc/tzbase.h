/*********************************************************************
**    NAME         :  ribase.h
**       CONTAINS:
**       include file
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tzbase.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:59
*********************************************************************/

#ifndef RIBASEH

#include "usysdef.h"
#include "tzrbase.h"

#define TZ_RSHFT 24	/* bit positions to shift relation id into tid */
#define TZ_EMASK 0x0FFFFFF	/* mask to mask out entry id from a tid */

#define	TZ_NUM_REL			256			/* the  number of relations */
#define	TZ_MAX_REL			TZ_NUM_REL-1/* the maximum relation number */
#define	TZ_MAX_ATTR			650			/* the max number of fields */
#define	TZ_MAX_CHARS		8				/* max chars in rel name */
#define	TZ_MAX_VARL			10				/* max number of varlists */
#define	TZ_MAX_PAGE_SIZE	70000			/* max alloc page size for relations */
#define	TZ_HIGH_BIT			0x80000000	/* set high order bit */
#define	TZ_PATH_LENGTH		200			/* max filename path length */

/* the following are bit defintions within the rel_flags word of the */
/* relation control block */
#define	TZ_DATA_REL			0				/* data type relation */
#define	TZ_ATTR_REL			1				/* attribute type relation */
#define	TZ_TRANSF_REL		2				/* transformation matrix rel */
#define	TZ_MTUPLE_REQD		8				/* master tuple required */

/* the following are defintions for the various bit maps used with each */
/* relation table	*/
#define	TZ_NUM_BMAPS	3	/* the actual number of bit maps per rel */
#define	TZ_ALLOC_MAP	0	/* map for indicating allocated tuples */
#define	TZ_SVLD_MAP		1	/* map for indicating whether to save and	*/
										/* whether tuple was just loaded */
#define	TZ_DEL_MAP		2	/* map for indicating whether deleted */
#define	TZ_CALLOC_MAP	2	/* copy of the ALLOC_MAP used during load */

/* predicate to question if a tuple is allocated (in use) */
#define ur_allocatedp(rel, tuple) \
(uu_tst_bit(&(TZ_rcb[rel].bmap_ptr[TZ_ALLOC_MAP*TZ_rcb[rel].bmap_size]),tuple-1)!=0)

/*********************************************************************/
/*																							*/
/*					structure definitions for entity dababase					*/
/*																							*/
/*********************************************************************/

typedef struct TZ_list_parms 	/* list info that is kept in thr rcb	*/
{
	int	atom_size;				/* size of each atom in bytes							*/
	int	list_size;				/* number of atoms to use for setup and expand	*/
}TZ_list_parms;

typedef struct TZ_lpacket		/* a list packet with a data tuple	*/
{
	int	atom_cnt;				/* number of atoms in a list			*/
	char	*list_ptr;				/* pointer to the atom list			*/
}TZ_lpacket;

typedef struct TZ_page			/* page table entry format */
{
	char	*pg_ptr;					/* page pointer */
}TZ_page;

typedef struct TZ_rcb_rec		/* relation control block record structure */
{
	int				rel_num;					/* the relation num of this relation */
	char				relname[TZ_MAX_CHARS+1];	/* relation name */
	int				status;					/* -1 if relation uninitialized */
													/* 0 otherwise */
	int				n_ent;					/* current number of entries */
	int				init_ent;				/* number of initial entries */
	int				n_varl;					/* number of allowed variable lists */
	TZ_list_parms	*lparms;					/* ptr to array of list parms */
	int				var_info_size;			/* length of ptr & bytect for var lsts */
	int				tuple_size;				/* tuple size (in bytes) */
	long				rel_flags;				/* relation control flags */
										 			/* bit0, least significant, rightmost */
													/* bit0 = 1, defined as data relation */
													/* bit1 = 1, defined as attr relation */
													/* bit2 = 1, defined as trans relation */
													/* bit8 = 1, master tuple required */
	int				last_accessed_index;	/* index to last accessed tuple */
	int				last_active_index;	/* index to the last active tuple */
	int				active_tuple_cnt;		/* number of active tuples within rel */
	TZ_page			*ent_ptr;				/* address of page of buffer array */
	int				bmap_size;				/* bitmap size (in words) */
	unsigned	int	*bmap_ptr;				/* first bitmap entry in bitmap file */
	int				page_size;			/* alloc page size in # entries-10/7/86 */
	int				seg_shift;			/* shift count used to seperate tuple */
												/* index into page and page index parts */
}TZ_rcb_rec;

extern TZ_rcb_rec	TZ_rcb[TZ_NUM_REL];	/* the rel directory	*/
extern UU_KEY_ID	TZ_last_mod_mkey;	/* last modified master key*/
extern UU_REL_ID	TZ_last_mod_rkey;	/* last modified relation,tuple key	*/
extern TZ_REL_NUM	TZ_transf_relnum;	/* holds the relation number of the	*/
												/* active transformation relation	*/
extern	long	TZ_default_transf	;	/* signal whether default defined	*/
extern UU_LOGICAL	TZ_del_mark;		/* TRUE if to mark del stack			*/
extern UU_LOGICAL	TZ_del_stack_enabled; /* TRUE if del stack enabled		*/
extern UU_LOGICAL	TZ_del_started;	/* TRUE if a logical delete transaction */
												/* is under way */
#define RIBASEH
#endif
