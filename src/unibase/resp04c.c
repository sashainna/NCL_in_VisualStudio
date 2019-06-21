/****************************************************
**     NAME       : resp04c.c
**     CONTAINS   :
**        ur_sv02 (lu)
**        ur_txt_sv02 (lu,filetyp)
**        ur_reset_lists(relnum,listno,size)
**        ur_reset_list_sizes(eptr)
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       resp04c.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:36
**
*****************************************************/

#define UR_BLKRCL   1024    /* block record length              */
#define MAX_NUM     100
#include "mlabddl.h"
#include "class.h"
#include "udebug.h"
#include "usysdef.h"
#include "umoveb.h"
#include "uhep.h"
#include "rmtuple.h"
#include "ribase.h"
#include "ritrnerr.h"
#include "riddldef.h"
#include "rerrdef.h"
#include "xenv1.h"
#include "xfsys0.h"
#include "xfsys1.h"
#include "nclfc.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdattr.h"
#include "nccs.h"
#include "dtypes.h"
#include "lcom.h"
#include "nclver.h"
#include "msrfddl.h"
#include "msol.h"
#include "driver.h"

#define atmax 64

extern UU_LOGICAL NCL_view_load, UR_changed;
/* string attribute definition - varlist */
struct attr_def  V_Ssadef[3] = {{"key",KEY_ID,4,4,0,0,1,1},
                               {"string",CHARACTER,1,1,1,0,1,1},
                               {"string",CHARACTER,1,1,4,0,1,1}};
int UR_restore_lights=0;
int UR_restore_mtrl=0;
int UR_restore_clr=0;
UU_LOGICAL UR_restore_units=UU_TRUE;

/*********************************************************************
**    E_FUNCTION     :  ur_sv02(lu)
**       Save unibase entities for later load (GET/PUT interface) in
**       binary format.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_sv02(lu)
 int lu;
{
  char    *uu_toolmalloc();    /* memory allocation function */
  extern int    UR_big_atom; /* size of the largest atom */
  extern int    UR_big_entry; /* size of the largest entry */
  extern UU_LOGICAL  UR_sav_all; /* boolean, UU_TRUE if save all */
  extern UU_LOGICAL  UR_save_modals;  /* boolean, UU_TRUE if save modals */
  extern UU_LOGICAL  UR_changed;  /* Unibase change flag */

  int    iostat;        /* holds status of i/o calls */
  int    status;        /* holds status of unibase calls */
  long    rel;          /* relation counter */
  struct UR_rcb_rec  temp_rcb;/* a temporary relation ctrl block */
  char    *atom_ptr;      /* a pointer to an atom */
  struct UR_data  *ent_ptr;  /* a pointer to an entry */
  long    nxt_ent;        /* next entry in relation */
  UU_KEY_ID  rel_key;      /* a relation,tuple index key */
  int    i,j,k,m;        /* indexes */
  unsigned int  *w_ptr;    /* a pointer to an integer */
  int    atom_size;      /* atom size for var length data */
  int    atom_cnt;      /* num of atoms in a particular list */
  int    lst_len;        /* length of a list in bytes */
  unsigned  long  word;      /* word to be shifted */
  UU_LOGICAL  hit;        /* whether a bit was set */
  long    save_map_disp;    /* displacement in words to save map */
  static  char eofstr[]  = "EOF\n" ; /* eof str for last rcb */
  char    *attr_ptr;      /* ptr to attributes within tuple */
  char    *data_ptr;      /* ptr to data within attribute */
  int    data_offset;    /* offset to next data item */
  int    nb;
  struct attr_def  *atdefs;  /* attribute definitions */
  int    num_attr;      /* number of attributes parsed */
  int    atndx;        /* index into atdefs */
  int    atype;        /* the data type of the attribute */
  int    rndx;          /* row index of attribute array  */
  int    cndx;          /* col index of attribute array  */
  struct attr_def  *v_adefs;/* attribute definitions - varlist */
  int    v_nattr;        /* number of attributes parsed  */
  int    v_andx;        /* index into atdefs  */
  int    v_atype;        /* the data type of the attribute  */
  UU_LOGICAL has_key;
  char    *buf_ptr;

  uu_denter(UU_RTRC,(us,"ur_sp02(name=%s)", fnameu));
  status = ur_chk_data_dict();
  if (status)
  {
    goto s_p99;    /* bailout - still no data dictionary */
  }
  /* flush(clear) out any phantom tuples on the delete stack */
  ur_flush_del_stack();

  /* if this is a save of all of Unibase then clear the flag we */
  /* use to know whether to prompt at close */
  if(UR_sav_all)
  {
    UR_changed = UU_FALSE;  /* not changed since last save */
  }

/*
.....Save modified Unibase statistics
*/
	ur_init_unibase_stat(UR_STAT_MODIFY);

  /* allocate space for the largest possible entry and atom. */
  ent_ptr = (struct UR_data *) uu_toolmalloc(UR_big_entry) ;
  if(ent_ptr == 0)
  {
    status = URM_NO_MEM_SV;
    goto s_p99 ;
  }
/*
...Now iterate through all the relations 
*/
  for (rel = 0; rel <= UR_MAX_REL; rel++)
  {
    /* first determine if initialized relation with active tuples */
    if(UR_rcb[rel].status >= 0 && UR_rcb[rel].active_tuple_cnt > 0)
    {
      /* got an active relation, now get a copy of the rcb and */
      /* determine whether there are tuples to save */
      uu_move_byte(&UR_rcb[rel],&temp_rcb, sizeof(struct UR_rcb_rec)) ;
      if(!UR_sav_all) /* if not a save all , set count  */
      {
        temp_rcb.active_tuple_cnt = 0 ;
        save_map_disp = UR_SVLD_MAP*UR_rcb[rel].bmap_size ;
        for(i=0; i<temp_rcb.bmap_size;i++)
        {
          word = UR_rcb[rel].bmap_ptr[save_map_disp+i] ;
          j = 0 ;
          while(word != 0 && j < UU_BITPW)
          {
            hit = word & 1 ;
            word >>= 1 ;
            j++ ;
            if(hit) temp_rcb.active_tuple_cnt ++ ;
          }  /* while bits in bitmap word */
        }  /* for each word of bitmap */
      }  /* if not save all */
      if(temp_rcb.active_tuple_cnt > 0)
      {

        /* get the data definition for the relation */
        if(temp_rcb.rel_num >= 0 && temp_rcb.rel_num <= UR_MAX_REL)
        {
          num_attr = UR_rcb[temp_rcb.rel_num].rel_def->rellen;
          atdefs = UR_rcb[temp_rcb.rel_num].relattr;
          if(num_attr <= 0)
          {
            status = URM_RELNTFND; /* rel not in data dictionary error */
            goto s_p90;
          }
        }

        /* we have tuples to save for this relation, so write its rcb */
        iostat = ux_write_block(lu,&temp_rcb,sizeof(struct UR_rcb_rec),
                                &nb, UX_PRTERRS);
        if(iostat)
        {
          status = URM_WRT_ERR;
          goto s_p90 ;
        }

        /* now output active tuples of this relation */
        nxt_ent  = 1;
        while(!ur_get_next_tuple_index(rel, &nxt_ent))
        {
          ur_rt2k(rel, nxt_ent, &rel_key);

          /* check if this tuple is to be saved */
          if(UR_sav_all || ur_svld_test(rel_key))
          {
            /* got the index for the next data tuple in this */
            /* relation, go get the data and varlist info */
            iostat=ur_retrieve_tuple_and_var_info(rel, nxt_ent, ent_ptr);
            if(!iostat)    /* check for good status */
            {
              /* adjust geometry with key_id's imbedded in the data */
              for (atndx=0; atndx<num_attr-temp_rcb.n_varl; atndx++)
              {
                attr_ptr = (char *)ent_ptr + atdefs[atndx].attr_here;
                atype = atdefs[atndx].attr_type;
                /* set array indexs, non array has 1 row, 1 col */
                rndx = atdefs[atndx].num_rows;
                cndx = atdefs[atndx].num_cols;
                data_ptr = attr_ptr;
                if((atype == KEY_ID) || (atype == REL_ID) || (atype == KEY_REF))
                {
                  data_offset = atdefs[atndx].attr_off / (rndx * cndx);
                  for(i = 1; i <= cndx; i++)
                  {
                    for(j = 1; j <= rndx; j++)
                    {
                      /* perform key jiggery-pokery */
                      uri_sp02a(data_ptr); 
                      if (atndx == 0)
                      {
                        data_ptr = data_ptr + data_offset
                              + sizeof(long);
                      }
                      else
                      {
                        data_ptr = data_ptr + data_offset;
                      }
                    }  /* for each row */
                  }  /* for each column */
                }  /* if type is key */
              } /* for each definition field */

/*
.....Reset Display and Tessalation List Sizes
.....for Surfaces and Shapes if Requested
.....Bobby  -  1/6/98
*/
				ur_reset_list_sizes(ent_ptr);

              /* output the modified relation entry, along with */
              /* the variable length lists, atom size, byte count, */
              /* and null pointer.  If it is a master tuple then delay */
              /* until the backlinks are examined if selective save */
              if ((rel != UR_MTUPLE_REL) || (UR_sav_all))
              {
                iostat=ux_write_block(lu,ent_ptr,UR_rcb[rel].tuple_size,
                                      &nb, UX_PRTERRS);
                if(iostat)
                {
                  status = URM_WRT_ERR;
                  goto s_p90;
                }
              }
              /* now output the variable lengths lists, if any */
              for(i = 1 ; i <= UR_rcb[rel].n_varl; i++)
              {
                status = ur_get_varlist_ptr(rel,nxt_ent,i,&w_ptr,
                        &atom_cnt);
                ur_get_atom_size(rel,i,&atom_size);
/*
.....Reset Display and Tessalation Lists
.....for Surfaces and Shapes if Requested
.....Bobby  -  1/6/98
*/
					ur_reset_lists(ent_ptr->rel_num,i,&atom_size);

                lst_len = atom_cnt * atom_size;
                if(lst_len > 0 && status == 0)
                {
                  /* get the data definition for the relation */
                  atndx = num_attr - temp_rcb.n_varl + i -1;
                  if(atdefs[atndx].attr_type == STRING)
                  {
                    v_nattr = 1;  /* one attr */
                    v_adefs = &V_Ssadef[2];
                  }
                  else
                  {
                    v_nattr = UR_rcb[rel].lparms[i-1].atom_def->rellen;
                    v_adefs = UR_rcb[rel].lparms[i-1].atomattr;
                  }
                  if(v_nattr <= 0)
                  {
                    status = URM_RELNTFND;
                    goto s_p90;
                  }
                  has_key = UU_FALSE;
                  for(v_andx = 1; v_andx < v_nattr; v_andx++)
                  {
                    v_atype = v_adefs[v_andx].attr_type;
                    if((v_atype == KEY_ID) || (v_atype == REL_ID) || (atype == KEY_REF))
                    {
                      has_key = UU_TRUE;
                    }  /* if key */
                  } /* for each definition field */
                  if(has_key)
                  {
                    buf_ptr = uu_toolmalloc(lst_len);/* get space */
                    if(buf_ptr == 0)
                    {
                      status = URM_NO_MEM_SV;
                      goto s_p90;
                    }
                    uu_move_byte(w_ptr,buf_ptr,lst_len);/* copy list*/

                    /* prepare 2 step thru lst */
                    atom_ptr = buf_ptr
                          + (UR_MAX_INDX + 1) * sizeof(UU_KEY_ID);

                    /* if this is the master tuples' assoc list */
                    /* then we need to trim the list to objects */
                    /* being saved. */
                    if ((rel == UR_MTUPLE_REL) && (!UR_sav_all))
                    {
                      for(m = UR_MAX_INDX + 1; m <= atom_cnt; m++)
                      {
                        if (!ur_svld_test(*(UU_KEY_ID *)atom_ptr))
                        {
                          /* this key not in list as saved */
                          if (atom_cnt > m)
                          {
                            uu_move_byte(
                              atom_ptr+sizeof(UU_KEY_ID),
                              atom_ptr,
                              (atom_cnt-m)*sizeof(UU_KEY_ID));
                            --atom_cnt;
                          }
                        }
                        atom_ptr += sizeof(UU_KEY_ID);
                      }
                      lst_len = atom_cnt * atom_size;
                      ((struct UR_MTID_rec *)ent_ptr)->no_assocs = atom_cnt;
                        iostat=ux_write_block(lu,ent_ptr,
                               UR_rcb[rel].tuple_size,&nb, UX_PRTERRS);
                      if(iostat)
                      {
                        status = URM_WRT_ERR;
                        goto s_p90;
                      }
                    }

                    atom_ptr = buf_ptr;  /* prepare 2 step thru lst */
                    for(m = 1; m <= atom_cnt; m++)
                    {
                      /* look through varlist def & convert keys */
                      attr_ptr = (char *)atom_ptr;
                      for(v_andx = 1; v_andx < v_nattr; v_andx++)
                      {
                        v_atype = v_adefs[v_andx].attr_type;
                        /* set array indexs, non array has 1,1 */
                        rndx = v_adefs[v_andx].num_rows;
                        cndx = v_adefs[v_andx].num_cols;
                        data_ptr = attr_ptr;
                        if((v_atype==KEY_ID) || (v_atype==REL_ID) || (atype == KEY_REF))
                        {
                          data_offset = v_adefs[v_andx].attr_off
                              / (rndx*cndx);
                          for(k = 1; k <= cndx; k++)
                          {
                            for(j = 1; j <= rndx; j++)
                            {
                              uri_sp02a(data_ptr); 
                              data_ptr += data_offset;
                            }  /* for each row */
                          }  /* for each column */
                        }  /* if key */
                        attr_ptr += v_adefs[v_andx].attr_off;
                      } /* for each definition field */
                      atom_ptr += atom_size;  /* step 2 nxt atom */
                    }  /* for each atom */
                    /* write out the varlist */
                    iostat = ux_write_block(lu, buf_ptr, lst_len, &nb,
                      UX_PRTERRS);
                    uu_toolfree(buf_ptr);  /* return the copy */
                  }
                  else  /* no keys in varlist */
                  {
                    /* write out the varlist */
                    iostat = ux_write_block(lu, w_ptr, lst_len, &nb,
                      UX_PRTERRS);
                  }
                  if(iostat)    /* check status of write */
                  {
                    status = URM_WRT_ERR;
                    uu_dprint(-1,(us,"ur_sp02: bad write status %d",
                        iostat));
                    goto s_p90;
                  }
                }  /* if list not 0 length */
              }  /* for each varlist */
            }  /* good status (found tuple?) */
          }  /* if save all or save this tuple */
          nxt_ent++ ;
        }  /* while got active tuple */
      } /* if temp_rcb.active_tuple_cnt > 0 */
    }  /* if initialized relation with active tuples */
  }  /* for each relation */

/* output a EOF rcb by outputing an rcb with relation type -1 */

 temp_rcb.rel_num = -1  ;
 temp_rcb.n_ent  = 0  ;
 strcpy(temp_rcb.relname,eofstr) ;
 iostat = ux_write_block(lu,&temp_rcb, sizeof(struct UR_rcb_rec),
                         &nb, UX_PRTERRS);

s_p90:
  uu_toolfree((char *)ent_ptr) ;
/*
...end
*/
s_p99:
  uu_dexit ;
  return(status)  ;
}

/*********************************************************************
**    E_FUNCTION     :  ur_txt_sv02(lu,filetyp)
**       Save unibase entities for later load (GET/PUT interface) in
**       ASCII format for later load.
**       Works almost the same way as "ur_sv02".
**    PARAMETERS   
**       INPUT  : 
**          lu    - logical file number
**          filetyp - string defining type of file (part, symbol, etc).
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_txt_sv02(lu,filetyp)
 int lu;
 char *filetyp;
{
  char    *uu_toolmalloc();    /* memory allocation function */
  extern int    UR_big_atom; /* size of the largest atom */
  extern int    UR_big_entry; /* size of the largest entry */
  extern UU_LOGICAL  UR_sav_all; /* boolean, UU_TRUE if save all */
  extern UU_LOGICAL  UR_save_modals;  /* boolean, UU_TRUE if save modals */
  extern UU_LOGICAL  UR_changed;  /* Unibase change flag */

  int    iostat;        /* holds status of i/o calls */
  int    status;        /* holds status of unibase calls */
  long    rel;          /* relation counter */
  struct UR_rcb_rec  temp_rcb;/* a temporary relation ctrl block */
  char    *atom_ptr;      /* a pointer to an atom */
  struct UR_data  *ent_ptr;  /* a pointer to an entry */
  long    nxt_ent;        /* next entry in relation */
  UU_KEY_ID  rel_key;      /* a relation,tuple index key */
  int    i,j,k,m;        /* indexes */
  unsigned int  *w_ptr;    /* a pointer to an integer */
  int    atom_size;      /* atom size for var length data */
  int    atom_cnt;      /* num of atoms in a particular list */
  int    lst_len;        /* length of a list in bytes */
  unsigned  long  word;      /* word to be shifted */
  UU_LOGICAL  hit;        /* whether a bit was set */
  long    save_map_disp;    /* displacement in words to save map */
  static  char eofstr[]  = "EOF\n" ; /* eof str for last rcb */
  char    *attr_ptr;      /* ptr to attributes within tuple */
  char    *data_ptr;      /* ptr to data within attribute */
  int    data_offset;    /* offset to next data item */
  struct attr_def  *atdefs;  /* attribute definitions */
  int    num_attr;      /* number of attributes parsed */
  int    atndx;        /* index into atdefs */
  int    atype;        /* the data type of the attribute */
  int    rndx;          /* row index of attribute array  */
  int    cndx;          /* col index of attribute array  */
  struct attr_def  *v_adefs;/* attribute definitions - varlist */
  int    v_nattr;        /* number of attributes parsed  */
  int    v_andx;        /* index into atdefs  */
  int    v_atype;        /* the data type of the attribute  */
  UU_LOGICAL has_key;
  char    *buf_ptr, buf[80];
  int    length;

  uu_denter(UU_RTRC,(us,"ur_sp02(name=%s)", fnameu));
  status = ur_chk_data_dict();
  if (status)
  {
    goto s_p99;    /* bailout - still no data dictionary */
  }
  /* flush(clear) out any phantom tuples on the delete stack */
  ur_flush_del_stack();

  sprintf(buf, "%s\n", filetyp);
  length = strlen(buf);
  if (status = ux_write(lu, buf, 1, &length, UX_PRTERRS)) goto s_p99;

  /* if this is a save of all of Unibase then clear the flag we */
  /* use to know whether to prompt at close */
  if(UR_sav_all)
  {
    UR_changed = UU_FALSE;  /* not changed since last save */
  }

/*
.....Save modified Unibase statistics
*/
	ur_init_unibase_stat(UR_STAT_MODIFY);

  /* allocate space for the largest possible entry and atom. */
  ent_ptr = (struct UR_data *) uu_toolmalloc(UR_big_entry) ;
  if(ent_ptr == 0)
  {
    status = URM_NO_MEM_SV;
    goto s_p99 ;
  }
/*
...Now iterate through all the relations 
*/
  for (rel = 0; rel <= UR_MAX_REL; rel++)
  {
    /* first determine if initialized relation with active tuples */
    if(UR_rcb[rel].status >= 0 && UR_rcb[rel].active_tuple_cnt > 0)
    {
      /* got an active relation, now get a copy of the rcb and */
      /* determine whether there are tuples to save */
      uu_move_byte(&UR_rcb[rel],&temp_rcb, sizeof(struct UR_rcb_rec)) ;
      if(!UR_sav_all) /* if not a save all , set count  */
      {
        temp_rcb.active_tuple_cnt = 0 ;
        save_map_disp = UR_SVLD_MAP*UR_rcb[rel].bmap_size ;
        for(i=0; i<temp_rcb.bmap_size;i++)
        {
          word = UR_rcb[rel].bmap_ptr[save_map_disp+i] ;
          j = 0 ;
          while(word != 0 && j < UU_BITPW)
          {
            hit = word & 1 ;
            word >>= 1 ;
            j++ ;
            if(hit) temp_rcb.active_tuple_cnt ++ ;
          }  /* while bits in bitmap word */
        }  /* for each word of bitmap */
      }  /* if not save all */
      if(temp_rcb.active_tuple_cnt > 0)
      {

        /* get the data definition for the relation */
        if(temp_rcb.rel_num >= 0 && temp_rcb.rel_num <= UR_MAX_REL)
        {
          num_attr = UR_rcb[temp_rcb.rel_num].rel_def->rellen;
          atdefs = UR_rcb[temp_rcb.rel_num].relattr;
          if(num_attr <= 0)
          {
            status = URM_RELNTFND; /* rel not in data dictionary error */
            goto s_p90;
          }
        }

        /* we have tuples to save for this relation, so write its rcb */
        iostat = ur_wrt_txt_rcb(lu,&temp_rcb);
        if(iostat)
        {
          status = URM_WRT_ERR;
          goto s_p90 ;
        }

        /* now output active tuples of this relation */
        nxt_ent  = 1;
        while(!ur_get_next_tuple_index(rel, &nxt_ent))
        {
          ur_rt2k(rel, nxt_ent, &rel_key);

          /* check if this tuple is to be saved */
          if(UR_sav_all || ur_svld_test(rel_key))
          {
            /* got the index for the next data tuple in this */
            /* relation, go get the data and varlist info */
            iostat=ur_retrieve_tuple_and_var_info(rel, nxt_ent, ent_ptr);
            if(!iostat)    /* check for good status */
            {
              /* adjust geometry with key_id's imbedded in the data */
              for (atndx=0; atndx<num_attr-temp_rcb.n_varl; atndx++)
              {
                attr_ptr = (char *)ent_ptr + atdefs[atndx].attr_here;
                atype = atdefs[atndx].attr_type;
                /* set array indexs, non array has 1 row, 1 col */
                rndx = atdefs[atndx].num_rows;
                cndx = atdefs[atndx].num_cols;
                data_ptr = attr_ptr;
                if((atype == KEY_ID) || (atype == REL_ID) || (atype == KEY_REF))
                {
                  data_offset = atdefs[atndx].attr_off / (rndx * cndx);
                  for(i = 1; i <= cndx; i++)
                  {
                    for(j = 1; j <= rndx; j++)
                    {
                      /* perform key jiggery-pokery */
                      uri_sp02a(data_ptr); 
                      if (atndx == 0)
                      {
                        data_ptr = data_ptr + data_offset
                              + sizeof(long);
                      }
                      else
                      {
                        data_ptr = data_ptr + data_offset;
                      }
                    }  /* for each row */
                  }  /* for each column */
                }  /* if type is key */
              } /* for each definition field */

/*
.....Reset Display and Tessalation List Sizes
.....for Surfaces and Shapes if Requested
.....Bobby  -  1/6/98
*/
				ur_reset_list_sizes(ent_ptr);

              /* output the modified relation entry, along with */
              /* the variable length lists, atom size, byte count, */
              /* and null pointer.  If it is a master tuple then delay */
              /* until the backlinks are examined if selective save */
              if ((rel != UR_MTUPLE_REL) || (UR_sav_all))
              {
                iostat=ur_wrt1_txt_ent(lu,ent_ptr,num_attr,atdefs);
                if(iostat)
                {
                  status = URM_WRT_ERR;
                  goto s_p90;
                }
              }
              /* now output the variable lengths lists, if any */
              for(i = 1 ; i <= UR_rcb[rel].n_varl; i++)
              {
                status = ur_get_varlist_ptr(rel,nxt_ent,i,&w_ptr,
                        &atom_cnt);
                ur_get_atom_size(rel,i,&atom_size);
/*
.....Reset Display and Tessalation Lists
.....for Surfaces and Shapes if Requested
.....Bobby  -  1/6/98
*/
					ur_reset_lists(ent_ptr->rel_num,i,&atom_size);

                lst_len = atom_cnt * atom_size;
                if(lst_len > 0 && status == 0)
                {
                  /* get the data definition for the relation */
                  atndx = num_attr - temp_rcb.n_varl + i -1;
                  if(atdefs[atndx].attr_type == STRING)
                  {
                    v_nattr = 2;  /* one attr */
                    v_adefs = &V_Ssadef[0];
                  }
                  else
                  {
                    v_nattr = UR_rcb[rel].lparms[i-1].atom_def->rellen;
                    v_adefs = UR_rcb[rel].lparms[i-1].atomattr;
                  }
                  if(v_nattr <= 0)
                  {
                    status = URM_RELNTFND;
                    goto s_p90;
                  }
                  has_key = UU_FALSE;
                  for(v_andx = 1; v_andx < v_nattr; v_andx++)
                  {
                    v_atype = v_adefs[v_andx].attr_type;
                    if((v_atype == KEY_ID) || (v_atype == REL_ID) || (atype == KEY_REF))
                    {
                      has_key = UU_TRUE;
                    }  /* if key */
                  } /* for each definition field */
                  if(has_key)
                  {
                    buf_ptr = uu_toolmalloc(lst_len);/* get space */
                    if(buf_ptr == 0)
                    {
                      status = URM_NO_MEM_SV;
                      goto s_p90;
                    }
                    uu_move_byte(w_ptr,buf_ptr,lst_len);/* copy list*/

                    /* prepare 2 step thru lst */
                    atom_ptr = buf_ptr
                          + (UR_MAX_INDX + 1) * sizeof(UU_KEY_ID);

                    /* if this is the master tuples' assoc list */
                    /* then we need to trim the list to objects */
                    /* being saved. */
                    if ((rel == UR_MTUPLE_REL) && (!UR_sav_all))
                    {
                      for(m = UR_MAX_INDX + 1; m <= atom_cnt; m++)
                      {
                        if (!ur_svld_test(*(UU_KEY_ID *)atom_ptr))
                        {
                          /* this key not in list as saved */
                          if (atom_cnt > m)
                          {
                            uu_move_byte(
                              atom_ptr+sizeof(UU_KEY_ID),
                              atom_ptr,
                              (atom_cnt-m)*sizeof(UU_KEY_ID));
                            --atom_cnt;
                          }
                        }
                        atom_ptr += sizeof(UU_KEY_ID);
                      }
                      lst_len = atom_cnt * atom_size;
                      ((struct UR_MTID_rec *)ent_ptr)->no_assocs = atom_cnt;
                        iostat=ur_wrt1_txt_ent(lu,ent_ptr,num_attr,atdefs);
                      if(iostat)
                      {
                        status = URM_WRT_ERR;
                        goto s_p90;
                      }
                    }

                    atom_ptr = buf_ptr;  /* prepare 2 step thru lst */
                    for(m = 1; m <= atom_cnt; m++)
                    {
                      /* look through varlist def & convert keys */
                      attr_ptr = (char *)atom_ptr;
                      for(v_andx = 1; v_andx < v_nattr; v_andx++)
                      {
                        v_atype = v_adefs[v_andx].attr_type;
                        /* set array indexs, non array has 1,1 */
                        rndx = v_adefs[v_andx].num_rows;
                        cndx = v_adefs[v_andx].num_cols;
                        data_ptr = attr_ptr;
                        if((v_atype==KEY_ID) || (v_atype==REL_ID) || (atype == KEY_REF))
                        {
                          data_offset = v_adefs[v_andx].attr_off
                              / (rndx*cndx);
                          for(k = 1; k <= cndx; k++)
                          {
                            for(j = 1; j <= rndx; j++)
                            {
                              uri_sp02a(data_ptr); 
                              data_ptr += data_offset;
                            }  /* for each row */
                          }  /* for each column */
                        }  /* if key */
                        attr_ptr += v_adefs[v_andx].attr_off;
                      } /* for each definition field */
                      atom_ptr += atom_size;  /* step 2 nxt atom */
                    }  /* for each atom */
                    /* write out the varlist */
                    iostat = ur_wrt_txt_atoms1(lu, buf_ptr, atom_cnt, v_adefs, v_nattr);
                    uu_toolfree(buf_ptr);  /* return the copy */
                  }
                  else  /* no keys in varlist */
                  {
                    /* write out the varlist */
                    iostat = ur_wrt_txt_atoms1(lu, w_ptr, atom_cnt, v_adefs, v_nattr);
                  }
                  if(iostat)    /* check status of write */
                  {
                    status = URM_WRT_ERR;
                    uu_dprint(-1,(us,"ur_sp02: bad write status %d",
                        iostat));
                    goto s_p90;
                  }
                }  /* if list not 0 length */
              }  /* for each varlist */
            }  /* good status (found tuple?) */
          }  /* if save all or save this tuple */
          nxt_ent++ ;
        }  /* while got active tuple */
      } /* if temp_rcb.active_tuple_cnt > 0 */
    }  /* if initialized relation with active tuples */
  }  /* for each relation */

/* output a EOF rcb by outputing an rcb with relation type -1 */

 temp_rcb.rel_num = -1  ;
 temp_rcb.n_ent  = 0  ;
 strcpy(temp_rcb.relname,eofstr) ;
 iostat = ur_wrt_txt_rcb(lu,&temp_rcb);

s_p90:
  uu_toolfree((char *)ent_ptr) ;
/*
...end
*/
s_p99:
  uu_dexit ;
  return(status)  ;
}

/*********************************************************************
**    E_FUNCTION     :  ur_reset_lists(relnum,listno,size)
**       Reset to 0 the display and tessalation lists for surfaces and
**       shapes dependant on the #UNIBASE# variables /SAVE_DISPLAY/ and
**       /SAVE_TESSEL/.
**    PARAMETERS   
**       INPUT  : 
**          relnum   = Relation number of entity.
**          listno   = Number of current variable list.
**       OUTPUT :  
**          size     = Reset to 0 if requested.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_reset_lists(relnum,listno,size)
int relnum,listno,*size;
{
/*
.....Check for Display List
*/
	if ((relnum == NCL_SURF_REL && listno == 3) ||
	    (relnum == NCL_MESHSURF_REL && listno == 2) ||
	    (relnum == NCL_QUILTSURF_REL && listno == 2) ||
	    (relnum == NCL_NETSF_REL && listno == 3) ||
	    (relnum == NCL_TRIMSF_REL && listno == 2) ||
	    (relnum == UM_RBSPLSRF_REL && listno == 6) ||
	    (relnum == NCL_SHAPE_REL && listno == 2) ||
	    (relnum == UM_SOLID_REL && listno == 2))
	{
		if (UR_save_display == 0) *size = 0;
	}
/*
.....Check for Tessalation List
*/
	if ((relnum == NCL_SURF_REL && listno == 4) ||
	    (relnum == NCL_MESHSURF_REL && listno == 3) ||
	    (relnum == NCL_QUILTSURF_REL && listno == 3) ||
	    (relnum == NCL_NETSF_REL && listno == 4) ||
	    (relnum == NCL_TRIMSF_REL && listno == 3) ||
	    (relnum == UM_RBSPLSRF_REL && listno == 7) ||
	    (relnum == NCL_SHAPE_REL && listno == 3) ||
	    (relnum == UM_SOLID_REL && listno == 3))
	{
		if (UR_save_tessel == 0) *size = 0;
	}
	return 0;
}

/*********************************************************************
**    E_FUNCTION     :  ur_reset_list_sizes(eptr)
**       Reset to 0 the display list and tessalation list sizes for
**       surfaces and shapes dependant on the #UNIBASE# variables
**       /SAVE_DISPLAY/ and /SAVE_TESSEL/.
**    PARAMETERS   
**       INPUT  : 
**          eptr   = Pointer to Unibase data structure.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_reset_list_sizes(eptr)
struct UR_data *eptr;
{
	struct UM_rbsplsrf_rec *bsf;
	struct NCL_surface_rec *sf;
	struct NCL_meshsf_rec *msf;
	struct NCL_quiltsf_rec *qsf;
	struct NCL_netsf_rec *nsf;
	struct NCL_evalsf_rec *esf;
	struct NCL_trimsf_rec *tsf;
	struct NCL_shape_rec *shp;
	struct UM_solid_rec *solid;
/*
.....B-spline Surface
*/
	if (eptr->rel_num == UM_RBSPLSRF_REL)
	{
		bsf = (struct UM_rbsplsrf_rec *)eptr;
		if (UR_save_display == 0) bsf->no_displst = 0;
		if (UR_save_tessel == 0) bsf->no_tesslst = 0;
	}
/*
.....NCL Surface
*/
	if (eptr->rel_num == NCL_SURF_REL)
	{
		sf = (struct NCL_surface_rec *)eptr;
		if (UR_save_display == 0) sf->no_displst = 0;
		if (UR_save_tessel == 0) sf->no_tesslst = 0;
	}
/*
.....Mesh Surface
*/
	if (eptr->rel_num == NCL_MESHSURF_REL)
	{
		msf = (struct NCL_meshsf_rec *)eptr;
		if (UR_save_display == 0) msf->no_displst = 0;
		if (UR_save_tessel == 0) msf->no_tesslst = 0;
	}
/*
.....Quilt Surface
*/
	if (eptr->rel_num == NCL_QUILTSURF_REL)
	{
		qsf = (struct NCL_quiltsf_rec *)eptr;
		if (UR_save_display == 0) qsf->no_displst = 0;
		if (UR_save_tessel == 0) qsf->no_tesslst = 0;
	}
/*
.....Net Surface
*/
	if (eptr->rel_num == NCL_NETSF_REL)
	{
		nsf = (struct NCL_netsf_rec *)eptr;
		if (UR_save_display == 0) nsf->no_displst = 0;
		if (UR_save_tessel == 0) nsf->no_tesslst = 0;
	}
/*
.....Evaluated Surface
*/
	if (eptr->rel_num == NCL_EVALSF_REL)
	{
		esf = (struct NCL_evalsf_rec *)eptr;
		if (UR_save_display == 0) esf->no_displst = 0;
		if (UR_save_tessel == 0) esf->no_tesslst = 0;
	}
/*
.....Trimmed Surface
*/
	if (eptr->rel_num == NCL_TRIMSF_REL)
	{
		tsf = (struct NCL_trimsf_rec *)eptr;
		if (UR_save_display == 0) tsf->no_displst = 0;
		if (UR_save_tessel == 0) tsf->no_tesslst = 0;
	}
/*
.....Lathe Shape
*/
	if (eptr->rel_num == NCL_SHAPE_REL)
	{
		shp = (struct NCL_shape_rec *)eptr;
		if (UR_save_display == 0) shp->no_displst = 0;
		if (UR_save_tessel == 0) shp->no_tesslst = 0;
	}
/*
.....Solid
*/
	if (eptr->rel_num == UM_SOLID_REL)
	{
		solid = (struct UM_solid_rec *)eptr;
		if (UR_save_display == 0) solid->no_displst = 0;
		if (UR_save_tessel == 0) solid->no_tesslst = 0;
	}
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : S_save_unibasemod
**       Save the unibase mode into modals file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_unibasemod()
{
	int i,stat;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy(fname,"ncl_unibase.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
/*
.....Store unibase modals
*/	
	ux_fputs0("#UNIBASE#\n", fptr);
	if (UR_save_display)
		ux_fputs0("/SAVE_DISPLAY/ *YES\n", fptr);
	else
		ux_fputs0("/SAVE_DISPLAY/ *NO\n", fptr);
	if (UR_save_tessel)
		ux_fputs0("/SAVE_TESSEL/ *YES\n", fptr);
	else
		ux_fputs0("/SAVE_TESSEL/ *NO\n", fptr);
	if (UR_restore_lights)
		ux_fputs0("/RESTORE_LIGHT/ *YES\n", fptr);
	else
		ux_fputs0("/RESTORE_LIGHT/ *NO\n", fptr);
	if (UR_restore_mtrl)
		ux_fputs0("/RESTORE_MATERIAL/ *YES\n", fptr);
	else
		ux_fputs0("/RESTORE_MATERIAL/ *NO\n", fptr);
	if (UR_restore_clr==2)
		ux_fputs0("/RESTORE_COLOR/ *Merge\n", fptr);
	else if (UR_restore_clr==1)
		ux_fputs0("/RESTORE_COLOR/ *YES\n", fptr);
	else if (UR_restore_clr==0)
		ux_fputs0("/RESTORE_COLOR/ *NO\n", fptr);
	if (UR_restore_units)
		ux_fputs0("/RESTORE_UNITS/ *YES\n", fptr);
	else
		ux_fputs0("/RESTORE_UNITS/ *NO\n", fptr);
/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     :  ur_unibase_modals()
**       Displays Unibase Modals Form
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_unibase_modals()
{
	int *ans[6];
	int status, disp[5], tessel[5], light[5], mtrl[5], color[5], units;
/*
.....Set default value
*/
	disp[0] = UR_save_display;
	tessel[0] = UR_save_tessel;
	light[0] = UR_restore_lights;
	mtrl[0] = UR_restore_mtrl;
	color[0] = UR_restore_clr;
	units = UR_restore_units;
	ans[0] = (int *)disp;
	ans[1] = (int *)tessel;
	ans[2] = (int *)light;
	ans[3] = (int *)mtrl;
	ans[4] = (int *)color;
	ans[5] = &units;
/*
.....Get input from user
*/
	status = ud_form("unibasem.frm", ans, ans, ans, ans);
	if (status==-1)
	   return -1;
	UR_save_display = disp[0];
	UR_save_tessel = tessel[0];
	UR_restore_lights = light[0];
	UR_restore_mtrl = mtrl[0];
	UR_restore_clr = color[0];
	UR_restore_units = units;
	S_save_unibasemod();
	return 0;
}
