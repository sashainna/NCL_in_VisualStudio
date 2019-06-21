/*********************************************************************
**    NAME         :  ribase.h
**       CONTAINS:
**       include file
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       ribase.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:42
*********************************************************************/

#ifndef RIBASEH

#include "usysdef.h"
#include "xenv1.h"
#include "rbase.h"
#include "rireldef.h"
#include "riddldef.h"

#define UR_RSHFT 24	/* bit positions to shift relation id into tid */
#define UR_EMASK 0x0FFFFFF	/* mask to mask out entry id from a tid */

#define	UR_NUM_REL			256			/* the  number of relations */
#define	UR_MAX_REL			UR_NUM_REL-1/* the maximum relation number */
#define	UR_MAX_ATTR			650			/* the max number of fields */
#define	UR_MAX_VARL			10				/* max number of varlists */
#define	UR_MAX_PAGE_SIZE	70000			/* max alloc page size for relations */
#define	UR_HIGH_BIT			0x80000000	/* set high order bit */
#define	UR_PATH_LENGTH		UX_MAX_PATH_LEN	/* max filename path length */

/* the following are bit defintions within the rel_flags word of the */
/* relation control block */
#define	UR_DATA_REL			0				/* data type relation */
#define	UR_ATTR_REL			1				/* attribute type relation */
#define	UR_TRANSF_REL		2				/* transformation matrix rel */
#define	UR_ASSOC_REL		3				/* is an association */
#define	UR_MTUPLE_REQD		8				/* master tuple required */

/* the following are defintions for the various bit maps used with each */
/* relation table	*/
#define	UR_NUM_BMAPS	4	/* the actual number of bit maps per rel */
#define	UR_ALLOC_MAP	0	/* map for indicating allocated tuples */
#define	UR_SVLD_MAP		1	/* map for indicating whether to save and	*/
										/* whether tuple was just loaded */
#define	UR_DEL_MAP		2	/* map for indicating whether deleted */
#define	UR_CALLOC_MAP	2	/* copy of the ALLOC_MAP used during load */
#define	UR_ASSOC_MAP	3	/* associativity auto update eligibility map */

/*
.....The following are Unibase Statistics generation flags
*/
#define	UR_STAT_INIT	0	/* Initialize stats for initial Unibase */
#define	UR_STAT_UPDATE	1	/* Update stats when loading older Unibase */
#define	UR_STAT_MODIFY	2	/* Update modified stats when saving Unibase */

/* predicate to question if a tuple is allocated (in use) */
#define ur_allocatedp(rel, tuple) \
(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_ALLOC_MAP*UR_rcb[rel].bmap_size]),tuple-1)!=0)

/* predicate to question if notification of associated updates is allowed */
#define uri_notify_allowedp(rel,tuple) \
(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_ASSOC_MAP*UR_rcb[rel].bmap_size]),tuple-1)==0)

/* predicate to ask if a relation is also an association */
#define uri_assocp(rel) (uu_tst_bit(&(UR_rcb[rel].rel_flags),UR_ASSOC_REL))

/*********************************************************************/
/*																							*/
/*					structure definitions for entity database					*/
/*																							*/
/*********************************************************************/

typedef struct UR_list_parms 	/* list info that is kept in thr rcb	*/
{
	int				atom_size;	/* size of each atom in bytes */
	int				list_size;	/* number of atoms for setup and expand */
	struct relblk	*atom_def;	/* dictionary definition of atom */
	struct attr_def	*atomattr;
}UR_list_parms;

typedef struct UR_lpacket		/* a list packet with a data tuple	*/
{
	char	*list_ptr;				/* pointer to the atom list			*/
	int	atom_cnt;				/* number of atoms in a list			*/
	int	padding;					/* padding for 64-bit pointer 		*/
}UR_lpacket;

typedef struct UR_lpacket_99	/* Pre NCL V10.0 lpacket            */
{
	int	atom_cnt;				/* number of atoms in a list			*/
	char	*list_ptr;				/* pointer to the atom list			*/
}UR_lpacket_99;

typedef struct UR_page			/* page table entry format */
{
	char	*pg_ptr;					/* page pointer */
}UR_page;

typedef struct UR_rcb_rec		/* relation control block record structure */
{
	int				rel_num;					/* the relation num of this relation */
	UR_REL_NAME		relname;					/* relation name */
	int				status;					/* -1 if relation uninitialized */
													/* 0 otherwise */
	int				n_ent;					/* current number of entries */
	int				init_ent;				/* number of initial entries */
	int				n_varl;					/* number of allowed variable lists */
	UR_list_parms	*lparms;					/* ptr to array of list parms */
	int				var_info_size;			/* length of ptr & bytect for var lsts */
	int				tuple_size;				/* tuple size (in bytes) */
	long				rel_flags;				/* relation control flags */
										 			/* bit0, least significant, rightmost */
													/* bit0 = 1, defined as data relation */
													/* bit1 = 1, defined as attr relation */
													/* bit2 = 1, defined as trans relation */
													/* bit3 = 1, is an association */
													/* bit8 = 1, master tuple required */
	int				last_accessed_index;	/* index to last accessed tuple */
	int				last_active_index;	/* index to the last active tuple */
	int				active_tuple_cnt;	/* number of active tuples within rel */
	UR_page			*ent_ptr;			/* address of page of buffer array */
	int				bmap_size;			/* bitmap size (in words) */
	unsigned	int	*bmap_ptr;			/* first bitmap entry in bitmap file */
	int				page_size;			/* alloc page size in # entries-10/7/86 */
	int				seg_shift;			/* shift count used to seperate tuple */
												/* index into page and page index parts */
	int				priority;			/* scheduling pri. for associations */
	struct relblk	*rel_def;			/* dictionary definition of tuple */
	struct attr_def	*relattr;
}UR_rcb_rec;

typedef struct UR_rcb_rec_32		/* relation control block record structure */
{
	int				rel_num;					/* the relation num of this relation */
	UR_REL_NAME		relname;					/* relation name */
	int				status;					/* -1 if relation uninitialized */
													/* 0 otherwise */
	int				n_ent;					/* current number of entries */
	int				init_ent;				/* number of initial entries */
	int				n_varl;					/* number of allowed variable lists */
	int				lparms;					/* ptr to array of list parms */
	int				var_info_size;			/* length of ptr & bytect for var lsts */
	int				tuple_size;				/* tuple size (in bytes) */
	long				rel_flags;				/* relation control flags */
										 			/* bit0, least significant, rightmost */
													/* bit0 = 1, defined as data relation */
													/* bit1 = 1, defined as attr relation */
													/* bit2 = 1, defined as trans relation */
													/* bit3 = 1, is an association */
													/* bit8 = 1, master tuple required */
	int				last_accessed_index;	/* index to last accessed tuple */
	int				last_active_index;	/* index to the last active tuple */
	int				active_tuple_cnt;	/* number of active tuples within rel */
	int				ent_ptr;				/* address of page of buffer array */
	int				bmap_size;			/* bitmap size (in words) */
	int				bmap_ptr;			/* first bitmap entry in bitmap file */
	int				page_size;			/* alloc page size in # entries-10/7/86 */
	int				seg_shift;			/* shift count used to seperate tuple */
												/* index into page and page index parts */
	int				priority;			/* scheduling pri. for associations */
	int				rel_def;				/* dictionary definition of tuple */
	int				relattr;
}UR_rcb_rec_32;

typedef struct UR_rcb_rec_64		/* relation control block record structure */
{
	int				rel_num;					/* the relation num of this relation */
	UR_REL_NAME		relname;					/* relation name */
	int				status;					/* -1 if relation uninitialized */
													/* 0 otherwise */
	int				n_ent;					/* current number of entries */
	int				init_ent;				/* number of initial entries */
	int				n_varl;					/* number of allowed variable lists */
	int				lparms[2];				/* ptr to array of list parms */
	int				var_info_size;			/* length of ptr & bytect for var lsts */
	int				tuple_size;				/* tuple size (in bytes) */
	long				rel_flags;				/* relation control flags */
										 			/* bit0, least significant, rightmost */
													/* bit0 = 1, defined as data relation */
													/* bit1 = 1, defined as attr relation */
													/* bit2 = 1, defined as trans relation */
													/* bit3 = 1, is an association */
													/* bit8 = 1, master tuple required */
	int				last_accessed_index;	/* index to last accessed tuple */
	int				last_active_index;	/* index to the last active tuple */
	int				active_tuple_cnt;	/* number of active tuples within rel */
	int				ent_ptr[2];			/* address of page of buffer array */
	int				bmap_size;			/* bitmap size (in words) */
	int				bmap_ptr_padding;
	int				bmap_ptr[2];		/* first bitmap entry in bitmap file */
	int				page_size;			/* alloc page size in # entries-10/7/86 */
	int				seg_shift;			/* shift count used to seperate tuple */
												/* index into page and page index parts */
	int				priority;			/* scheduling pri. for associations */
	int 				rel_def_padding;	/* dictionary definition of tuple */
	int 				rel_def[2];			/* dictionary definition of tuple */
	int 				relattr[2];
}UR_rcb_rec_64;

#define uri_get_priority(rel) UR_rcb[rel].priority

extern UR_rcb_rec	UR_rcb[UR_NUM_REL];	/* the rel directory	*/
extern UU_KEY_ID	UR_last_mod_mkey;	/* last modified master key*/
extern UU_REL_ID	UR_last_mod_rkey;	/* last modified relation,tuple key	*/
extern UR_REL_NUM	UR_transf_relnum;	/* holds the relation number of the	*/
												/* active transformation relation	*/
extern	long	UR_default_transf	;	/* signal whether default defined	*/
extern UU_LOGICAL	UR_del_mark;		/* TRUE if to mark del stack			*/
extern UU_LOGICAL	UR_del_stack_enabled; /* TRUE if del stack enabled		*/
extern UU_LOGICAL	UR_del_started;	/* TRUE if a logical delete transaction */
												/* is under way */
#define RIBASEH
#endif
