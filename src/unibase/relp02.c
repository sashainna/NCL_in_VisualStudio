/*********************************************************************
**    NAME         :  relp02
**       CONTAINS:
**				ur_lp02
**				ur_lp02b
**				ur_lp02c
**				ur_varl_data_def
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       relp02.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:32
*********************************************************************/
#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define rindex strrchr
#endif

#include "usysdef.h"
#include "umoveb.h"
#include "uhep.h"
#include "udebug.h"
#include "class.h"
#include "mlabddl.h"
#include "riddle.h"
#include "rmtuple.h"
#include "ribase.h"
#include "ritrnerr.h"
#include "riddldef.h"
#include "rerrdef.h"
#include "xenv1.h"
#include "nclver.h"
#include "rienvtab.h"
#include "rver9400.h"
#include "rver9700.h"
#include "xfsys0.h"
#include "xfsys1.h"
#include "nclfc.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdattr.h"
#include "nccs.h"
#include "dtypes.h"
#include "lcom.h"
#include "adraft.h"
#include "view.h"

#define UR_BLKRCL 1024	/* block record length					*/
#define MAX_NUM   100
#define ATMAX 64

/* NCL: special flag to handle loading of Unibase without viewing rels */
UU_LOGICAL NCL_view_load = UU_FALSE;
/* NCL: special flag to save the lowest key for merged CAM/CAD geometry. */
UU_KEY_ID  UR_low_key;
UX_pathname UR_exnam[2] = {"UR_UNB_FILE","UR_ASCII_PART"};


/* NCL: special flag to handle loading of Unibase without viewing rels */
extern UU_LOGICAL NCL_view_load, UR_changed;
UU_LOGICAL NCL_cpln_load;
extern UU_LOGICAL UR_load_env;
/* NCL: special flag to save the lowest key for merged CAM/CAD geometry. */
extern int UR_active, UR_dctindex;
extern struct attr_def  V_Ssadef[3];
UU_KEY_ID LABTBL_KEY;

static int S_xfer_rcb();
static void S_xfer_varl();


struct UM_mtrlmdl_rec
{
	int	index;
	char	name[64][20];
	UU_REAL	ka[64];
	UU_REAL	kd[64];
	UU_REAL	ks[64];
	UU_REAL	ks_r[64];
	UU_REAL	ks_g[64];
	UU_REAL	ks_b[64];
	UU_REAL	spec_exp[64];
};
extern struct UM_mtrlmdl_rec UM_mtrlmdl;
extern struct UM_mtrlmdl_rec UM_new_mtrlmdl;


/*********************************************************************
**    E_FUNCTION     :  status = ur_lp02(fnameu) 
**  load a part into unibase which has previously been saved 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful (all other error)
**				URM_CANTOPEN_LD,	Cannot open specified file
**				URM_INCOMPAT,	non-compatible file
**				URM_ILL_REL,	illegal relation number in master tuple
**				URM_MTID_RDERR,	error reading master tuples
**				URM_NO_MEM_LD,	unable to allocate memory
**				URM_ILL_RCB,	error reading relation control block
**				URM_VARL_RDERR,	error reading variable length list
**				URM_NEXIST_VARL,	attempted to load non-existent variable list
**				URM_ILL_MTID,	illegal master key found in relation tuple
**				URM_MTID_ERR,	error reading relation tuples
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ur_lp02(fnameu, load)
char		fnameu[];		/* unibase filename to use	*/
UU_LOGICAL   load; 			/* flag true if load and false if merge */
{
	int			status;					/* holds status of unibase calls		*/

	uu_denter(UU_RTRC,(us,"ur_lp02, load Unibase part %s", fnameu));

	/* Clear the save/load bit map (used here to mark newly loaded tuples) */
	ur_svld_clear();
	/* MILLS- Pass load for merge with rename option. */
   status = ur_lp02b(fnameu,load); /* really do the load.  This was factored out */
										/* so that symbol masters can be reloaded */
										/* during a part load. */
	uu_dprint(UU_RTRC,(us,"ur_lp02, exit status = %d",status));
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_lp02b(fnameu) 
**  load a part into unibase which has previously been saved 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful (all other error)
**				URM_CANTOPEN_LD,	Cannot open specified file
**				URM_INCOMPAT,	non-compatible file
**				URM_ILL_REL,	illegal relation number in master tuple
**				URM_MTID_RDERR,	error reading master tuples
**				URM_NO_MEM_LD,	unable to allocate memory
**				URM_ILL_RCB,	error reading relation control block
**				URM_VARL_RDERR,	error reading variable length list
**				URM_NEXIST_VARL,	attempted to load non-existent variable list
**				URM_ILL_MTID,	illegal master key found in relation tuple
**				URM_MTID_ERR,	error reading relation tuples
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ur_lp02b(fnameu, load)
char		fnameu[];		/* unibase filename to use	*/
UU_LOGICAL   load; 			/* flag true if load and false if merge */
{

	int			iostat;				/* holds status of i/o calls   			*/
	int			status;				/* holds status of unibase calls		*/
	int			lu;   				/* logical device number for xio 	*/
   char       fext[20]; 
#ifndef UU_RS6000
   char *rindex();
#endif
   int  ftype;
   UX_pathname extnam[2]; 

	uu_denter(UU_RTRC,(us,"ur_lp02b, load Unibase part %s", fnameu));

	status = ur_chk_data_dict() ;
	if (status != 0)
   	{
       status = -1;
       goto l_p99;
	   }

    if (ux_getsys_ext (2,UR_exnam,extnam) != 0)
      {
       status = UX_BAD_ENV;
       goto l_p99;
      }

   strcpy (fext,(rindex(fnameu,'.')));
   fext[0] = UX_QUOTE_CHAR;
   ux_strip_quotes(fext);
#if UU_COMP == UU_VAXVMS || UU_COMP == UU_WIN2K
   ul_to_upper (fext);
#endif
   ftype = 0;
   for (ftype=0; ftype<2; ftype++)
     {
#if UU_COMP == UU_WIN2K
      ul_to_upper (extnam[ftype]);
#endif
      if (strcmp(fext,extnam[ftype]) == 0) break;
     }

   if (ftype == 1)
      {
	    iostat = ux_open_to_data(fnameu,"r","STREAM","ASCII",&lu,UX_PRTERRS);
      }
   else
      {
	/* open blocked file with logical record length = UR_BLKRCL */
/*#ifndef WNT */
#if (UU_COMP != UU_WIN2K)
		iostat = ux_open_to_data(fnameu,"r","BLOCKED","BINARY",&lu,UX_PRTERRS);
#else
       iostat = ux_open_to_data(fnameu,"rb","BLOCKED","BINARY",&lu,UX_PRTERRS);
#endif 
	}

	if(iostat != 0)
	{
		/* " cannot open file, try again "*/
		status = URM_CANTOPEN_LD;
		goto l_p99;
	}
   status = ur_lp02c(lu,ftype);

	if (status == UU_SUCCESS && UR_load_env)
	{
	/* read the environment info */
		status = (ftype == 1)? ur_rd_txt_env(lu): ur_environ_in(lu);
		if (status != 0)
		{
			status = 0;		/* for upward compat... can't really croak */
		}
		/* NCL: fix label structure if this was an old IGES unibase 
		   OR, the loading of the environment info croaked          */

/*		if (ur_get_labelmdl_num() > ur_get_new_labelmdl_num())
			ur_put_new_labelmdl(&UM_labelmdl);*/
	}
l_p90:
	/* close the file, and clear the temporary reloaction bit maps */
	iostat =	ux_close(lu,UX_PRTERRS) ;
	ur_del_clear() ; /* clearing bit maps */
/*
.....we need load old env into new structure
*/ 
	ur_install_new_modals();	
 l_p99:
#if MODULEDBG != 0
	uu_dprint(UU_RTRC,(us,"ur_lp02b, exit status = %d",status)) ;
#endif
	uu_dexit ;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_lp02c(lu,ftype)
**  load a part into unibase which has previously been saved in binary
**  format.
**    PARAMETERS   
**       INPUT  : 
**          lu    - logical unit number associated with the output file.
**          ftype - logical unit number associated with the output file.
**       OUTPUT :  
**          none
**    RETURNS      : 
**        0,    load successful (all other error)
**        URM_CANTOPEN_LD,  Cannot open specified file
**        URM_INCOMPAT,  non-compatible file
**        URM_ILL_REL,  illegal relation number in master tuple
**        URM_MTID_RDERR,  error reading master tuples
**        URM_NO_MEM_LD,  unable to allocate memory
**        URM_ILL_RCB,  error reading relation control block
**        URM_VARL_RDERR,  error reading variable length list
**        URM_NEXIST_VARL,  attempted to load non-existent variable list
**        URM_ILL_MTID,  illegal master key found in relation tuple
**        URM_MTID_ERR,  error reading relation tuples
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ur_lp02c(lu,ftype)
int lu,ftype;
{
	char      *uu_toolmalloc();   /*  external utility to get memory  */
	extern int    UR_big_atom;    /* size of the largest atom      */
	extern int    UR_big_entry;           /* size of the largest entry      */

	int      iostat;                      /* holds status of i/o calls      */
	int      status;                      /* holds status of unibase calls  */
	int      lrecl;                       /* length of record read        */
	struct UR_rcb_rec   temp_rcb;         /* a temporary relation ctrl block*/
	union
	{
		struct UR_rcb_rec_32 rcb_32;
		struct UR_rcb_rec_64 rcb_64;
	} trcb;
	UU_KEY_ID  key_id;                    /* a key_id tuple            */
	UU_KEY_ID  data_key;                  /* a data key              */
	long    rel_num, r_n;                  /* a relation number      */
	long    rel_tuple_indx;               /* relative entry id          */
	long    tuple_indx;                   /* an entry id          */
	int     atom_size, inat_size;         /* atom size for var length data   */
	int     atom_ct, inat_ct;             /* atom count for var length data */
	char   *atom_ptr;                     /* a pointer to a byte      */
	struct UR_data *ent_ptr, *ent_ptro, *sav_ptr;
                                        /* a pointer to an entry      */
	char      *b_ptr, *b_ptro;            /* pointer to a byte       */
	UU_LOGICAL  relocate_keys;            /* true if need to relocate key's */
	int      i,j,k,m,n;
	struct UR_lpacket  *lp_ptr, *lp_ptro; /* pointer to a variable list pckt*/
	char      *attr_ptr;                  /* ptr to attributes within tuple*/
	char      *data_ptr;                  /* ptr to data within attribute  */
	int      data_offset;                 /* offset to next data item  */
	struct attr_def V_Adefs[ATMAX], V_Adef1[ATMAX];
	struct attr_def ATdef1[ATMAX];
	struct attr_def  *atdefs,*atdef1;     /* attribute definitions */
	int      num_attr;                    /* number of attributes parsed */
	int      atndx,atnd1;                 /* index into atdefs */
	int      atype;                       /* the data type of the attribute */
	int      rndx;                        /* row index of attribute array  */
	int      cndx;                        /* col index of attribute array  */
	struct attr_def  *v_adefs,*v_adef1;   /* attribute definitions - varlist*/
	int      v_nattr,v_nattr1;            /* number of attributes parsed  */
	int      v_andx;                      /* index into atdefs  */
	int      v_atype;                     /* the data type of the attribute */
	char    *buf_ptr, *buf_ptro;
	int      lst_len, lst_leno, lst_mem;  /* length of a list in bytes    */
	UU_LOGICAL  has_key;
	int      nb;
	UR_TUPLE_INDX  nxt_tid;               /* tuple to start looking from */

	struct relblk *relb;
	struct attr_def *ratb;
	UR_rcb_rec  *rcbr;         /* a temporary relation ctrl block  */
	int lpsiz,v_xfl;
	UR_lpacket_99 *lptr;
	char filetyp[20];
	FILE *fd;
	int vn,num_attr1,rel_typ;

/*
.....Initialize attr_here to 0 to avoid UMR in purify
*/
	if (ftype == 1)
	{
		for(i=0;i<ATMAX;i++) ATdef1[i].attr_here = 0;
	}
/*
.....Rewind the file and
.....position the pointer to the first block of data
*/
	if (ftype == 1)
	{
		ux_get_os_filedesc(lu, &fd, UX_PRTERRS);
		UX_FSCANF0((fd, "%s", filetyp), status);
	}
	else
	{
		iostat = ux_file_rewind(lu,UX_PRTERRS);
		if(iostat != 0) goto l_p90 ;
	}

/*
.....When merging keep the lowest key number used to be used later
.....when saving the geometry in the ranfile. 
*/
  UR_dctindex = ux_get_dct_index();
  ent_ptro = 0;
  sav_ptr = 0;
  UR_low_key = 10000;
  LABTBL_KEY = 0;
/* 
.....Just in case unibase was not reset, go flush the delete stack 
*/
  ur_flush_del_stack();
/* 
.....Check if anything in the database so that we need to relocate keys 
*/
  nxt_tid = 1;
  ur_get_next_key(&nxt_tid, &key_id);  /* = 0 if no active entries */
  relocate_keys = nxt_tid != 0;
/*
.....Always relocate keys for secondary unibase (why?)
.....vp 18-may-94 always relocate keys for secondary unibase (why?)
*/
	if (UR_active == 2) relocate_keys = UU_TRUE;

/*
.....Must make a copy of each relation's allocation bit map to use for 
.....relocating keys.  The delete bitmap (which is unused during save 
.....and load operations) is used to hold the copy. 
*/
	for(i=0; i<=UR_MAX_REL; i++)
	{
		if (UR_rcb[i].bmap_size!=0)
		{
			uu_move_byte(UR_rcb[i].bmap_ptr,
				&(UR_rcb[i].bmap_ptr[UR_CALLOC_MAP*UR_rcb[i].bmap_size]),
				UR_rcb[i].bmap_size*sizeof(unsigned long));
		}
	}

/* 
.....Find largest possible entry and atom size
.....and allocate space for them 
*/
	ent_ptr = (struct UR_data *) uu_toolmalloc(UR_big_entry);
	if (NCL_infile_version != NCL_version)
	{ 
		ent_ptro = (struct UR_data *) uu_toolmalloc(UR_big_entry); 
		sav_ptr  = ent_ptro;
	}
	if(ent_ptr == 0)
	{
		uu_dprint(-1,(us,"ERROR: unable to allocate buffer in ur_lp02b"));
		status = URM_NO_MEM_LD;
		goto l_p90;
	}
	status = 0 ;
	temp_rcb.rel_num = 1 ;  /* set for first time */
	NCL_view_load = UU_FALSE; /* NCL: assume no viewing in Unibase */
	NCL_cpln_load = UU_FALSE; 
/* 
.....Read an rcb, and check it 
*/
	while	(temp_rcb.rel_num >= 0)
	{
		if (ftype == 1)
		{
	   	iostat = ur_rd_txt_rcb(lu, &temp_rcb);
			lrecl = sizeof(struct UR_rcb_rec);
		}
/*
.....We need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
		else
		{
			lrecl =  sizeof (struct UR_rcb_rec_64);
			nb = 1;
   		iostat = ux_read_block(lu,&trcb,&lrecl,&nb,UX_PRTERRS);
   		if (iostat == 0)
			{
				if (lrecl != sizeof(temp_rcb))
					iostat = S_xfer_rcb(&trcb,&temp_rcb,&lrecl,&v_xfl);
				else
				{
					uu_move_byte(&trcb,&temp_rcb,sizeof(struct UR_rcb_rec));
					v_xfl = 0;
				}
			}
		}
		r_n  = temp_rcb.rel_num;
/* 
.....Encountered illegal rcb 
*/
		if (iostat != 0 || sizeof(struct UR_rcb_rec) != lrecl ||
			(r_n >= 0 && UR_rcb[r_n].status < 0))
		{
			uu_dprint(-1,(us,"ERROR reading a relation's rcb"));
			uu_dprint(-1,(us,"iostat %d, lrecl %d(should be %d)",iostat,lrecl,
				sizeof(struct UR_rcb_rec)));
			uu_dprint(-1,(us,"rcb status %d",UR_rcb[temp_rcb.rel_num].status));
			status = URM_ILL_RCB ;
			goto l_p90 ;
		}
/*
.....Process valid relation entity
*/
		if (r_n >= 0 && r_n <= UR_MAX_REL)
		{
/*			urc = &UR_rcb[r_n]; */ /* debugging conector */
/*
.....Label table is not supported any more, but if
.....if it is old file mark this event to remove record and
.....adjust attributes in post load processing
.....vp 2/14/98
*/
      	if (r_n == NCL_LABTBL_REL) LABTBL_KEY = 1;
/* 
.....If viewing relation set NCL_view_load to UU_TRUE 
*/
			if (r_n == UM_COORDSYS_REL) NCL_cpln_load = 1;
			if (!NCL_view_load && (r_n >= 74 && r_n <= 75))
				NCL_view_load = UU_TRUE;

/* 
.....Get the data definition for the relation 
*/
			num_attr = UR_rcb[r_n].rel_def->rellen;
			atdefs = UR_rcb[r_n].relattr;
			if(num_attr <= 0)
			{
				status = URM_RELNTFND;  /* relation not in data dictionary */
				goto l_p90 ;
			}
/*
.....Textual Unibase (.ud) files have only the pertinent
.....data stored in them, without the inherent operating system
.....padding found in the binary Unibase structures.
.....So each value is read directly into its slot within the structure
.....
.....Therefore we need to use the dictionary for the Unibase version
.....being read in for (.ud) files.
*/
			if (ftype == 1)
			{
				vn   = UR_dctindex - 1;
				if (UR_dctindex == 0)
				{
					num_attr1 = num_attr;
					atdef1 = atdefs;
				}
				else
				{
					num_attr1 = ur_data_dict_o(ATdef1,ATMAX,UR_rcb[r_n].relname,
						&rel_typ,vn,UU_TRUE);
					atdef1 = ATdef1;
				}
			}
/*
.....Now add the new relation entries to the relation, fetching number 
.....of entries from a value stored in the saved rcb 
*/
			for (n=0; n<temp_rcb.active_tuple_cnt; n++)
			{
/* 
.....Read relation entry 
*/
				if (ftype == 1)
				{
					lrecl = temp_rcb.tuple_size;
					iostat = ur_rd1_txt_ent(lu, ent_ptr, num_attr1, atdef1);
				}
				else
				{
					lrecl =  sizeof(struct UR_data)*UR_big_entry;
					nb = 1;
					iostat = ux_read_block(lu,ent_ptr,&lrecl,&nb,UX_PRTERRS);
/*
........Need to convert from 32-bit to 64-bit
........or vice-versa operating system
........When reading a binary file
*/
					if (iostat == 0 && v_xfl != 0 && temp_rcb.n_varl != 0)
						S_xfer_varl(ent_ptr,v_xfl);
				}
/*
.....Update entity if old Unibase format
*/
				if (iostat == 0 && lrecl == temp_rcb.tuple_size)   
				{
					if (NCL_infile_version != NCL_version) 
						ur_update_entity (ent_ptr,&ent_ptro);
					else 
						ent_ptro = ent_ptr;
/*
.....Store source line of entity
*/
  					if (UR_active != 2) ncl_srcctl_put(ent_ptro->key_id);
/* 
.....Adjust imbedded key_ids 
*/
					for (atndx=0; atndx<num_attr-UR_rcb[r_n].n_varl; atndx++)
					{
						attr_ptr = (char *)ent_ptro + atdefs[atndx].attr_here;
						atype = atdefs[atndx].attr_type;
/* 
.....Set array indexes
.....non array has 1 row, 1 col 
*/
						rndx = atdefs[atndx].num_rows;
						cndx = atdefs[atndx].num_cols;
						data_ptr = attr_ptr;
						if (atype == CHARACTER)
						{
							data_offset = atdefs[atndx].attr_off / rndx;
							cndx = 1;      /* no names for each column */
						}
						else
						{
							data_offset = atdefs[atndx].attr_off / (rndx * cndx);
						}
						for(i = 1; i <= cndx; i++)
						{
							for(j = 1; j <= rndx; j++)
							{
/* 
.....Perform key jiggery-pokery 
*/
								if ((atype == KEY_ID || atype == REL_ID ||
									atype == KEY_REF) && relocate_keys &&
									r_n != NCL_LABTBL_REL)
								{
									ur_k2rt(*((long *)data_ptr), &rel_num,
										&rel_tuple_indx);
									uri_lp02a(data_ptr,
										&(UR_rcb[rel_num].bmap_ptr[UR_CALLOC_MAP*
										UR_rcb[rel_num].bmap_size]));
								}
								data_ptr += data_offset;
							}  /* i loop */
						}  /* j loop */
					} /* end adjust keys */
/* 
.....Check for CAD or CAM geometry.  And save the lowest key
.....used for later when the geometry is being saved in ranfile. 
*/

					if (ncl_geom_type(ent_ptr->rel_num) == UU_SUCCESS ||
						wf_geom_type(ent_ptr->rel_num) == UU_SUCCESS ||
						ent_ptr->rel_num == NCL_MATRIX_REL ||
						ent_ptr->rel_num == NCL_DATAST_REL ||
						ent_ptr->rel_num == NCL_TEXTVAR_REL ||
						ent_ptr->rel_num == NCL_SCALAR_REL)
					{
						if (UR_low_key > ent_ptr->key_id)
							UR_low_key = ent_ptr->key_id;
					}
/* 
.....Add the new tuple to Unibase
*/
					ur_create_tuple(r_n,&tuple_indx,ent_ptro);
					ur_rt2k(r_n,tuple_indx,&data_key);
/*
.....Update attribute bundle (note that only MX is allowed
.....so remove this if considering other entity.
.....Attributes for other entities are updated in another routine
*/
					if (NCL_infile_version != NCL_version) 
						if (r_n == NCL_MATRIX_REL)
							ur_update_attribut (ent_ptro->key_id,r_n);
/* 
.....Mark as a newly loaded tuple 
*/
					ur_load_set(data_key);
/* 
.....Build a pointer to the list input packets 
.....(variable lists)
*/
					rcbr = (UR_rcb_rec *)&UR_rcb[r_n];
					relb = rcbr->rel_def;
					i = relb->rellen - rcbr->n_varl;
					ratb = &rcbr->relattr[i];
					b_ptro = (char *)ent_ptro + ratb->attr_here; 
					if (ftype == 1)
						b_ptr  = (char *)ent_ptr + 
							atdef1[num_attr1-temp_rcb.n_varl].attr_here;
					else
					{
						if (NCL_infile_version < 9.950)
							b_ptr  = (char *)ent_ptr + temp_rcb.tuple_size -
								(sizeof(UR_lpacket_99) * temp_rcb.n_varl);
						else
							b_ptr  = (char *)ent_ptr +
								atdefs[num_attr-temp_rcb.n_varl].attr_here;
/*						b_ptr  = (char *)ent_ptr + temp_rcb.tuple_size -
							(sizeof(int) * temp_rcb.n_varl * 2);*/
					}
					if (NCL_infile_version < 9.950)
						lpsiz = sizeof(struct UR_lpacket_99);
					else
						lpsiz = sizeof(struct UR_lpacket);
					lp_ptro = (struct UR_lpacket *)b_ptro;  
/* 
.....Now output the variable lengths lists, if any 
*/
					r_n    = ent_ptro->rel_num;
					for (i=1 ; i<=UR_rcb[r_n].n_varl; i++)
					{
						lst_len = 0;
						atom_ct = lp_ptro->atom_cnt;
						if (NCL_infile_version < 9.950)
						{
							lptr = (struct UR_lpacket_99 *)b_ptr;
							inat_ct = lptr->atom_cnt;
						}
						else
						{
							lp_ptr = (struct UR_lpacket *) b_ptr;
							inat_ct = lp_ptr->atom_cnt;
						}
						if (ftype != 1)
						{
							if (i <= temp_rcb.n_varl)
							{ 
								status  = ur_get_atom_size(temp_rcb.rel_num,i,
									&atom_size);
								if (NCL_infile_version != NCL_version)
								{ 
									ur_get_input_asize (r_n,i,&inat_size);
									lst_len = inat_ct * inat_size;
									lst_mem = lst_len;
								}
	
								if (status == 0)
								{
									atom_ct = lp_ptro->atom_cnt;
									lst_leno = atom_ct * atom_size;
									if (lst_len < lst_leno) 
									{ 
										if (lst_len == 0) lst_len = lst_leno;
										lst_mem = lst_leno;
									}
								}
/* 
........Attempt to load non-existent var list
*/
								else
								{
									uu_dprint(-1,(us,
										"bad status from ur_get_atom_size"));
									status = URM_NEXIST_VARL;
									goto l_p90 ;
								}
							}
						}
/* 
.....Get the data definition for the relation 
*/
						if (atom_ct > 0)
            		{
							atndx = num_attr - UR_rcb[temp_rcb.rel_num].n_varl +
								 i - 1;
/*
........Special logic for reading Textual variable lists
*/
							if (ftype == 1)
							{
								status = ur_varl_data_def (UR_dctindex,r_n,atndx,
									atdefs,V_Adefs,&v_nattr);
								if (status != 0) goto l_p90 ;
								v_adefs = V_Adefs;
								if (UR_dctindex != 0 && i <= temp_rcb.n_varl)
								{
									atnd1  = num_attr1 - temp_rcb.n_varl + i -1;
									status = ur_varl_data_def (UR_dctindex,r_n,atnd1,
										atdef1,V_Adef1,&v_nattr1);
									if (status != 0) goto l_p90 ;
									v_adef1 = V_Adef1;
								}
								else
								{
									v_adef1 = v_adefs;
									v_nattr1 = v_nattr;
								}
/*
...........This calculation of atom_size (for the version of the
...........unibase being loaded) is not correct for the txtblk list
...........of a drafting entity because it does not include the
...........padding after the tstring field. However, the call to
...........ur_get_atom_size() below to set inat_size (the atom size
...........for the current version of the unibase) does include the
...........padding and hopefully inat_size will never be smaller than
...........atom_size. This would also apply to any other entity with
...........padding - none known at this time except labtbl which is
...........not used.
*/
								atom_size = v_adefs[v_nattr-1].attr_off +
									v_adefs[v_nattr-1].attr_here;
								if (r_n == NCL_LABTBL_REL)
									ur_get_atom_size(r_n,i,&atom_size);
								lst_leno = atom_ct * atom_size;
								lst_mem = lst_leno;
								if (i <= temp_rcb.n_varl)
								{
									ur_get_atom_size(r_n,i,&inat_size);
									lst_len = inat_ct * inat_size;
									if (lst_len > lst_leno)
									{
										lst_mem = lst_len;
									}
								}
								buf_ptr = uu_toolmalloc(lst_mem);  /* get space */
								if (buf_ptr != UU_NULL)
								{
									iostat = 0;
									if (i <= temp_rcb.n_varl)
									iostat = ur_rd_txt_atoms(lu, buf_ptr, inat_ct,
										v_adef1, v_nattr1);
								}
								lrecl = lst_len;
							}
/*
........Special logic for reading Binary variable lists
*/
							else
							{
								if (atdefs[atndx].attr_type == STRING)
								{
									v_nattr = 1;  /* one attr */
									v_adefs = &V_Ssadef[2];
								}
								else
								{
									v_nattr = UR_rcb[temp_rcb.rel_num].
										lparms[i-1].atom_def->rellen;
									v_adefs = UR_rcb[temp_rcb.rel_num].
										lparms[i-1].atomattr;  
								}
/*
...........Invalid variable list definition
*/
								if (v_nattr <= 0)
								{
									uu_dprint(-1,(us,
									"ERROR:can't find varlist %s definition for load"
											,atdefs[atndx].attr_name));
									status = URM_RELNTFND; /* rel not in data dict */
									goto l_p90 ;
								}
								buf_ptr = uu_toolmalloc(lst_mem);  /* get space */
								if (buf_ptr != UU_NULL)
								{
									lrecl = lst_mem; 
									nb = 1;
									iostat = ux_read_block(lu,buf_ptr,&lrecl,&nb,
										UX_PRTERRS);
								}
							}
/* 
.....Unable to allocate necessary memory"
*/
							if (buf_ptr == UU_NULL)
							{
								uu_dprint(-1,
								(us,"ERROR:unable to allocate buffer in ur_lp02c"));
								status = URM_NO_MEM_LD;
								goto l_p90;
							}
/*
.....Update variable list buffer if old Unibase format
*/
							if (iostat == 0 && lrecl == lst_len) 
							{
								if (r_n != NCL_LABTBL_REL)
								{
									if (NCL_infile_version != NCL_version)
									{
										if (ur_update_ent_varlist
											(ent_ptro,i,buf_ptr,&buf_ptro) == 0)
												buf_ptro = buf_ptr; 
									}
									else
										buf_ptro = buf_ptr; 
/*
.....Memory now managed in ur_update_ent_varlist. Andrew - 1/10/12
									if (buf_ptro != buf_ptr)
										uu_toolfree((char *)buf_ptr);
*/
									has_key = UU_FALSE;
									for (v_andx=1; v_andx<v_nattr; v_andx++)
									{
										v_atype = v_adefs[v_andx].attr_type;
										if (v_atype == KEY_ID || v_atype == REL_ID ||
											atype == KEY_REF)
										{
											has_key = UU_TRUE;
										}
									}
									if (has_key)
									{
										atom_ptr = buf_ptro;
/* 
........Look through varlist def & convert keys 
*/
										for (m=1; m<=atom_ct; m++)
										{
											attr_ptr = (char *)atom_ptr;
/* 
...........Set array indexs, non array has 1,1 
*/
											for (v_andx=1; v_andx<v_nattr; v_andx++)
											{
												v_atype = v_adefs[v_andx].attr_type;
												rndx = v_adefs[v_andx].num_rows;
												cndx = v_adefs[v_andx].num_cols;
												data_ptr = attr_ptr;
												if(v_atype == CHARACTER)
												{
													data_offset =
														v_adefs[v_andx].attr_off / rndx;
													cndx = 1;  /* no name for each col */
												}
												else
												{
													data_offset =
														v_adefs[v_andx].attr_off /
														(rndx*cndx);
												}
												for (k=1; k<=cndx; k++)
												{
													for (j=1; j<=rndx; j++)
													{
														if ((v_atype == KEY_ID ||
															v_atype == REL_ID ||
															atype == KEY_REF) &&
															relocate_keys)
														{
															ur_k2rt(*(UU_KEY_ID *)data_ptr,
																&rel_num,&rel_tuple_indx);
															uri_lp02a(data_ptr,
																&(UR_rcb[rel_num].bmap_ptr
																[UR_CALLOC_MAP*UR_rcb
																[rel_num].bmap_size])) ;
														}
														data_ptr += data_offset;
													}  /* k loop */
												}  /* j loop */
												attr_ptr += v_adefs[v_andx].attr_off;
											} /* end massage keys */
											atom_ptr += atom_size;
										}
									}
								}
								else
								{
									if (buf_ptr != UU_NULL)
										uu_toolfree((char *)buf_ptr);
									ur_update_varlist_info(temp_rcb.rel_num,
										tuple_indx,i,0,0);  /* set zero length */
								}
							}
/* 
.....Error reading atom in var list
*/
							else
							{
								uu_dprint(-1,(us,"ERROR reading varlist"));
								uu_dprint(-1,(us,"iostat=%d,lrecl=%d(should be %d)",
								iostat, lrecl,lst_len));
								status = URM_VARL_RDERR ;
							}
/*
.....Update variable list
*/
							if (r_n != NCL_LABTBL_REL)
							{
								ur_update_varlist_info(temp_rcb.rel_num,tuple_indx,
									i,buf_ptro,atom_ct);  /* fix the pointer */
							}
						}
/*
.....Variable list is empty
.....Set zero length
*/
						else
						{
							ur_update_varlist_info(temp_rcb.rel_num,tuple_indx,
								i,0,0);
						}
						b_ptr = b_ptr + lpsiz;
						lp_ptro++; 
					}
				}
/* 
.....Error reading data tuples
*/
				else
				{
					uu_dprint(-1,(us,"ERROR reading data tuples"));
					uu_dprint(-1,(us,"iostat = %d, lrecl = %d(should be %d)",
						iostat, lrecl, temp_rcb.tuple_size));
					status = URM_MTID_ERR ;
					goto l_p90 ;
				}
			}
		}
		ent_ptro = sav_ptr;
	}
/*
.....Create Unibase Statistics record
.....for older Unibases
*/
	if (NCL_infile_version < 10.055)
		ur_update_10100_unistat();
/* 
.....Clear the temporary reloaction bit maps 
*/
l_p90:
	ur_del_clear() ; /* clearing bit maps */

l_p99:
	uu_toolfree((struct UR_data *)ent_ptr);
	if (ent_ptro != 0 && ent_ptro != ent_ptr)
		uu_toolfree((struct UR_data *)ent_ptro);
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  ur_varl_data_def(dct_num,rel_num,lst_num,
**                              atdefs,v_adefs,v_nattr)
**  Set variable list definition using relation dictionary.  This
**  routine MAKES COPY of selected definition attributes to v_adefs,
**  not just pointer to dictionary.
**    PARAMETERS   
**       INPUT  : 
**          dct_num  - 0 = current dictionary is valid, n = use
**                         old dictionary.
**          rel_num  - relation number
**          lst_num  - variable list number in relation
**          atdefs   - relation definitions (all) from dictionary
**       OUTPUT :  
**          v_adefs  - variable list definitions
**          v_nattr  - number of attributes in v_adefs.
**    RETURNS      : 
**        0,            variable list definition in dictionary 
**        URM_RELNTFND  var list definition not in dictionary 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ur_varl_data_def (dct_num,rel_num,lst_num,atdefs,v_adefs,v_nattr)
struct attr_def atdefs[], v_adefs[];
int *v_nattr, dct_num, rel_num, lst_num;
{
  int vn, status, rel_typ;

  status = 0;
/*
...string list is not in dictionary, set it up
*/
  if(atdefs[lst_num].attr_type == STRING)
    {
     *v_nattr = 2;  /* one attr */
     strcpy(v_adefs[0].attr_name, "string");
     v_adefs[1].attr_type = CHARACTER;
     v_adefs[1].num_rows = 1;
     v_adefs[1].num_cols = 1;
     v_adefs[1].attr_off = 1;
    }
    else
    {
     vn    = UR_dctindex - 1;
     if (dct_num == 0)
        *v_nattr = ur_data_dict (v_adefs,ATMAX,atdefs[lst_num].attr_name,&rel_typ);
     else
        *v_nattr = ur_data_dict_o (v_adefs,ATMAX,atdefs[lst_num].attr_name,
				&rel_typ,vn,UU_TRUE);
    }
  if((int)*v_nattr <= 0)
    {
     uu_dprint(-1,(us,"ERROR:can't find varlist %s definition for load",
               atdefs[lst_num].attr_name));
     status = URM_RELNTFND; /* rel not in data dict */
    }

  return(status);
}

/*********************************************************************
**    E_FUNCTION     :  S_xfer_rcb(trcb,rcb,lrecl);
**  Copies the external file RCB into the internal RCB when the sizes
**  do not match (32-bit to 64-bit and vice versa).
**    PARAMETERS   
**       INPUT  : 
**          trcb     - RCB read in from external file.
**          rcb      - Internal RCB.
**          lrecl    - Size of RCB read in from external file.
**       OUTPUT :  
**          lrecl    - Size of internal RCB or 0 if external RCB is
**                     invalid.
**          xfl      - 0 = No conversion of JOIN structures required.
**                     1 = Convert from 32-bit to 64-bit structure.
**                     2 = Convert from 64-bit to 32-bit structure.
**    RETURNS      : 
**          0        - RCB transferred OK.
**          1        - External RCB is invalid.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_xfer_rcb(trcb,rcb,lrecl,xfl)
union { struct UR_rcb_rec_32 rcb_32; struct UR_rcb_rec_64 rcb_64;} *trcb;
UR_rcb_rec *rcb;
int *lrecl,*xfl;
{
	int status;
/*
......From 32-bit to 64-bit
*/
	if (*lrecl == sizeof(struct UR_rcb_rec_32) &&
		sizeof(struct UR_rcb_rec_64) == sizeof(struct UR_rcb_rec))
	{
		rcb->rel_num = trcb->rcb_32.rel_num;
		strcpy(rcb->relname,trcb->rcb_32.relname);
		rcb->status = trcb->rcb_32.status;
		rcb->n_ent = trcb->rcb_32.n_ent;
		rcb->init_ent = trcb->rcb_32.init_ent;
		rcb->n_varl = trcb->rcb_32.n_varl;
		rcb->lparms = (UR_list_parms *)trcb->rcb_32.lparms;
		rcb->var_info_size = trcb->rcb_32.var_info_size;
		rcb->tuple_size = trcb->rcb_32.tuple_size;
		rcb->rel_flags = trcb->rcb_32.rel_flags;
		rcb->last_accessed_index = trcb->rcb_32.last_accessed_index;
		rcb->last_active_index = trcb->rcb_32.last_active_index;
		rcb->active_tuple_cnt = trcb->rcb_32.active_tuple_cnt;
		rcb->ent_ptr = (UR_page *)trcb->rcb_32.ent_ptr;
		rcb->bmap_size = trcb->rcb_32.bmap_size;
		rcb->bmap_ptr = (unsigned int *)trcb->rcb_32.bmap_ptr;
		rcb->page_size = trcb->rcb_32.page_size;
		rcb->seg_shift = trcb->rcb_32.seg_shift;
		rcb->priority = trcb->rcb_32.priority;
		rcb->rel_def = (struct relblk *)trcb->rcb_32.rel_def;
		rcb->relattr = (struct attr_def *)trcb->rcb_32.relattr;
		*lrecl = sizeof(struct UR_rcb_rec);
		*xfl = 1;
		status = 0;
	}
/*
......From 64-bit to 32-bit
*/
	else if (*lrecl == sizeof(struct UR_rcb_rec_64) &&
		sizeof(struct UR_rcb_rec_32) == sizeof(struct UR_rcb_rec))
	{
		rcb->rel_num = trcb->rcb_64.rel_num;
		strcpy(rcb->relname,trcb->rcb_64.relname);
		rcb->status = trcb->rcb_64.status;
		rcb->n_ent = trcb->rcb_64.n_ent;
		rcb->init_ent = trcb->rcb_64.init_ent;
		rcb->n_varl = trcb->rcb_64.n_varl;
		rcb->lparms = (UR_list_parms *)trcb->rcb_64.lparms;
		rcb->var_info_size = trcb->rcb_64.var_info_size;
		rcb->tuple_size = trcb->rcb_64.tuple_size;
		rcb->rel_flags = trcb->rcb_64.rel_flags;
		rcb->last_accessed_index = trcb->rcb_64.last_accessed_index;
		rcb->last_active_index = trcb->rcb_64.last_active_index;
		rcb->active_tuple_cnt = trcb->rcb_64.active_tuple_cnt;
		rcb->ent_ptr = (UR_page *)trcb->rcb_64.ent_ptr;
		rcb->bmap_size = trcb->rcb_64.bmap_size;
		rcb->bmap_ptr = (unsigned int *)trcb->rcb_64.bmap_ptr;
		rcb->page_size = trcb->rcb_64.page_size;
		rcb->seg_shift = trcb->rcb_64.seg_shift;
		rcb->priority = trcb->rcb_64.priority;
		rcb->rel_def = (struct relblk *)trcb->rcb_64.rel_def;
		rcb->relattr = (struct attr_def *)trcb->rcb_64.relattr;
		*lrecl = sizeof(struct UR_rcb_rec);
		*xfl = 2;
		status = 0;
	}
/*
.....Invalid RCB
*/
	else
	{
		*lrecl = 0;
		*xfl = 0;
		status = -1;
	}
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  S_xfer_varl(ent_ptr,xfl)
**  Converts the external file variable lists into the internal variable
**  list format (32-bit OS to 64-bit OS and vice versa).
**    PARAMETERS   
**       INPUT  : 
**          ent_ptr  - Entity structure.
**          xfl      - 1 = Convert from 32-bit to 64-bit structure.
**                     2 = Convert from 64-bit to 32-bit structure.
**       OUTPUT :  
**          ent_ptr   - Updated entity structure.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_xfer_varl(ent_ptr,xfl)
struct UR_data *ent_ptr;
int xfl;
{
	int nattr1,rel_typ,atom_cnt[25],i,inc,ist,rel,isiz,over,ix;
	char *ptr;
	struct attr_def atdef1[ATMAX];
	struct UR_lpacket *lptr2;
	struct UR_lpacket_99 *lptr1;
	struct lpack99 {int atom_cnt; int ptr;} *optr1;
	struct lpack10 {int ptr; int atom_cnt; int padding;} *optr2;
	struct lpack64 {int ptr[2]; int atom_cnt; int padding;} *optr3;
struct UM_layer_rec *sptr;
/*
.....Initialize routine
*/
	rel = ent_ptr->rel_num;
/*
	if (rel == UM_LAYER_REL)
	{
		sptr = (struct UM_layer_rec *)ent_ptr;
	}
*/
/*
.....Convert from 32-bit to 64-bit
*/
	if (xfl == 1)
	{
/*
........Unibase is same version as NCL version
*/
		inc = 0;
		if (UR_dctindex == 0)
		{
			nattr1 = ur_data_dict(atdef1,ATMAX,UR_rcb[rel].relname,
				&rel_typ);
		}
/*
........Unibase is an older version
........All older dictionaries are 32-bit dictionaries
*/
		else
		{
			nattr1 = ur_data_dict_o(atdef1,ATMAX,UR_rcb[rel].relname,
				&rel_typ,UR_dctindex-1,UU_FALSE);
		}
		if (UR_rcb[rel].n_varl == 0) goto done;
/*
........Build 32-bit attribute dictionary
........(local version defaults to 64-bit)
........Pre NCL V10.0 Unibases are all 32-bit
........and don't need conversion to 64-bit
*/
		if (NCL_infile_version >= 9.950)
		{
			i = nattr1 - UR_rcb[rel].n_varl;
			isiz = atdef1[i-1].attr_size * atdef1[i-1].num_rows *
				atdef1[i-1].num_cols;
			over = atdef1[i-1].attr_here % sizeof(int);
			atdef1[i-1].attr_off = isiz + over;
			atdef1[i].attr_here = atdef1[i-1].attr_here + isiz + over;
			isiz = sizeof(struct lpack10);
			atdef1[i].attr_size = isiz;
			atdef1[i].attr_off = isiz;
			inc = i;
			for (i=i+1;i<nattr1;i++)
			{
				atdef1[i].attr_here = atdef1[i-1].attr_here + isiz;
				atdef1[i].attr_size = isiz;
				atdef1[i].attr_off = isiz;
			}
		}
/*
........Convert 32-bit to 64-bit variable list packets
...........First get all variable list counts
*/
		ix = 0;
		ist = nattr1;
		for (i=inc;i<nattr1;i++)
		{
			if (atdef1[i].attr_type == JOIN || atdef1[i].attr_type == STRING)
			{
				if (ix == 0) ist = i;
				ptr = (char *)ent_ptr + atdef1[i].attr_here;
				if (NCL_infile_version < 9.950)
				{
					optr1 = (struct lpack99 *)ptr;
					atom_cnt[ix++] = optr1->atom_cnt;
				}
				else
				{
					optr2 = (struct lpack10 *)ptr;
					atom_cnt[ix++] = optr2->atom_cnt;
				}
			}
		}
/*
...........Now store the counts in 64-bit format
*/
		ix = 0;
		if (UR_dctindex == 0)
			nattr1 = ur_data_dict(atdef1,ATMAX,UR_rcb[rel].relname,&rel_typ);
		else if (NCL_infile_version >= 9.950)
			nattr1 = ur_data_dict_o(atdef1,ATMAX,UR_rcb[rel].relname,
				&rel_typ,UR_dctindex-1,UU_FALSE);
		ptr = (char *)ent_ptr + atdef1[ist].attr_here;
		for (i=ist;i<nattr1;i++)
		{
			if (NCL_infile_version < 9.950)
			{
				lptr1 = (struct UR_lpacket_99 *)ptr;
				lptr1->atom_cnt = atom_cnt[ix++];
				ptr = ptr + sizeof(struct UR_lpacket_99);
/*
.....This logic must match the logic
.....in the 'ur_data_dict_o' routine
.....in 'redatdct.c'
*/
				if (i == ist)
				{
					over = atdef1[i].attr_here % sizeof(char *);
					ptr -= over;
				}
			}
			else
			{
				lptr2 = (struct UR_lpacket *)ptr;
				lptr2->atom_cnt = atom_cnt[ix++];
				ptr = ptr + sizeof(struct UR_lpacket);
			}
		}
	}
/*
.....Convert from 64-bit to 32-bit
*/
	else if (xfl == 2)
	{
/*
........Unibase is same version as NCL version
*/
		if (UR_dctindex == 0)
		{
			nattr1 = ur_data_dict(atdef1,ATMAX,UR_rcb[rel].relname,
				&rel_typ);
		}
/*
........Unibase is an older version
........All older dictionaries are 32-bit dictionaries
*/
		else
		{
			nattr1 = ur_data_dict_o(atdef1,ATMAX,UR_rcb[rel].relname,
				&rel_typ,UR_dctindex-1,UU_FALSE);
		}
/*
........Build 64-bit attribute dictionary
*/
		for (i=0;i<nattr1;i++)
		{
			if (atdef1[i].attr_type == JOIN || atdef1[i].attr_type == STRING)
			{
				over = atdef1[i].attr_here % sizeof(char *);
				atdef1[i-1].attr_off += over;
				isiz = sizeof(int) * 4;
				for (i=i;i<nattr1;i++)
				{
					atdef1[i].attr_here = atdef1[i-1].attr_here +
						atdef1[i-1].attr_off;
					atdef1[i].attr_size = isiz;
					atdef1[i].attr_off = isiz;
				}
			}
		}
/*
........Convert 64-bit to 32-bit variable list packets
...........First get all variable list counts
*/
		inc = 0;
		ist = nattr1;
		for (i=0;i<nattr1;i++)
		{
			if (atdef1[i].attr_type == JOIN || atdef1[i].attr_type == STRING)
			{
				if (inc == 0) ist = i;
				ptr = (char *)ent_ptr + atdef1[i].attr_here;
/*				if (i == ist)*/
				{
					over = atdef1[i].attr_here % (sizeof(char *)*2);
					ptr += over;
				}
				optr3 = (struct lpack64 *)ptr;
				atom_cnt[inc++] = optr3->atom_cnt;
			}
		}
/*
...........Now store the counts in 32-bit format
*/
		inc = 0;
		if (UR_dctindex == 0)
			nattr1 = ur_data_dict(atdef1,ATMAX,UR_rcb[rel].relname,&rel_typ);
		ptr = (char *)ent_ptr + atdef1[ist].attr_here;
		for (i=ist;i<nattr1;i++)
		{
			lptr2 = (struct UR_lpacket *)ptr;
			lptr2->atom_cnt = atom_cnt[inc++];
			ptr = ptr + sizeof(struct UR_lpacket);
		}
	}
done:;
}
