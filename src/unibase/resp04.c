/****************************************************
**     NAME       : resp04.c
**     CONTAINS   :
**        ur_op04 (fnameu,ierr)
**        ur_op04_session (fnameu,ierr)
**        ur_cl04 (ierr)
**        ur_st02c (ptr)
**        ur_skip_ent (unifile)
**
**  MODULE NAME AND RELEASE LEVEL
**       resp04.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:36
**
*****************************************************/

#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define index strchr
#define rindex strrchr
#endif

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
#include "adraft.h"
#include "view.h"

int lub2 = -1;                /* xio logical device number */
UX_pathname UBopen = "\0";
static int list[MAX_NUM] =
           {1,2,3,4,5,6,7,11,80,81,82,83,85,86,87,88,89,90,91,92,93,94,98,99};

/* NCL: flag to control output of EOF block to unibase file when closing. */
UU_LOGICAL UR_EOF_stat = UU_FALSE;
UU_LOGICAL NCL_ubnew;
extern struct attr_def  V_Ssadef[3];
extern UX_pathname UR_exnam[2];
extern char UR_dpn[UX_MAX_PATH_LEN+1];
int UB_ftype = 0;

/*********************************************************************
**    E_FUNCTION     :  ur_op04(fnameu,ierr)
**       Open a Unibase file for GET/PUT commands.   
**    PARAMETERS
**       INPUT  :
**         fnameu  - unibase file name
**         nci     - Number of chars in 'fnameu'.
**       OUTPUT :
**         itype   - unibase file type (0 = .u, 1 = .ud)
**         ierr    - error status (0 = OK) for f77 caller
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ur_op04(fnameu,nci,itype,ierr)
UM_int2 *ierr;
UM_int2 *itype;
UM_int4 *nci;
UM_f77_str_ptr fnameu;
{
	int     status;
	char    *p;
	int     ftype;
	UX_pathname fname;

	*ierr = 0;
/*
...
...Check if the file is already open
...
*/
	if (lub2 >= 0)  
	{
		*ierr = 1;
		goto s_p99;
	}
/*
...
...Convert Fortran Character string to C string
...
*/
	p = UM_cstr_of_f77_str(fnameu);
	strncpy(fname,p,*nci);
	fname[*nci] = '\0';
	if(fname[0] == '\0')
	{
		*ierr = 2;
		goto s_p99;       
	}
/*
.....Open secondary Unibase
*/
	status = ur_op04_session(fname,&ftype,UU_TRUE);
	if (status == UX_NFOUND) *ierr = -1;
	else if (status != UU_SUCCESS) *ierr = 3;
	*itype = ftype;
/*
.....End of routine
*/
s_p99:;
	return;
}

/*********************************************************************
**    E_FUNCTION     :  ur_op04_session(fnameu,itype,crefl)
**       Performs the actual open of a Secondary Unibase file.
**    PARAMETERS
**       INPUT  :
**         fname   - Unibase file name
**         crefl   - UU_TRUE create a Unibase if it does not exist.
**       OUTPUT :
**         itype   - Unibase file type (0 = .u, 1 = .ud)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ur_op04_session(fname,itype,crefl)
char *fname;
int *itype;
UU_LOGICAL crefl;
{
	int     status;
	char    *p;
	int     ftype;
	char fext[20];
	UX_pathname extnam[2],fnam;
#ifndef UU_RS6000
	char *rindex();
#endif
	UV_view view;

/*
.....Get valid Unibase file extensions
*/
	if (ux_getsys_ext (2,UR_exnam,extnam) != 0)
	{
		status = UU_FAILURE;
		goto done;
	}
/*
.....Determine file type of Unibase (Binary or ASCII)
*/
	p = rindex(fname,'.'); p++;
	strcpy (fext,p);
#if UU_COMP == UU_WIN2K
	ul_to_upper (fext);
	ul_to_upper (extnam[0]);
	ul_to_upper (extnam[1]);
#endif
   ftype = 0;
   for (ftype=0; ftype<2; ftype++)
      if (strcmp(fext,extnam[ftype]) == 0) break;
	if (ftype > 1) ftype = 0;
/*
.....Try to create file
*/
	if (crefl)
	{
		if (ftype == 1)
			status = ux_create_file(fname, 0640, UU_NULL, "STREAM", "ASCII",
				"UX_NOEXTRA", &lub2, UX_NPRTERRS);
		else
			status = ux_create_file(fname, 0640, UU_NULL, "BLOCKED", "BINARY",
				"UX_NOEXTRA", &lub2, UX_NPRTERRS);
		if (status == 0)
		{
			status = UX_NFOUND;
			NCL_ubnew = UU_TRUE;
			strcpy(UBopen,fname);
			*itype = UB_ftype = ftype;
			goto done;       
		}
	}
/*
.....Open existing Unibase
*/
		if (status == UX_FOUND || !crefl)
		{   
/*
.....The filename is required to be in quotes
.....by 'ux_open_to_data', or else it removes
.....the first "/" from a directory specification
.....Bobby  -  3/13/97
*/
		ul_add_quotes(fname,fnam);
		if (ftype == 1)
			status = ux_open_to_data(fnam,"r+","STREAM","ASCII",&lub2,UX_PRTERRS);
		else
#if (UU_COMP != UU_WIN2K)
			status = ux_open_to_data(fnam,"r+","BLOCKED","BINARY",&lub2,UX_PRTERRS);
#else
			status = ux_open_to_data(fnam,"rb+","BLOCKED","BINARY",&lub2,UX_PRTERRS);
#endif
		NCL_ubnew = UU_FALSE;
	}
/*
.....store info about opened file
*/
	if (!status)
	{
		strcpy(UBopen,fname); /* Save Unibase filename */
		*itype = UB_ftype = ftype;
		status = UU_SUCCESS;
	}
	else         
	{
		status = UU_FAILURE;
	}
	ux_file_rewind(lub2,UX_PRTERRS);
	UR_EOF_stat = UU_FALSE;
/*
.....Store unibase name in viewport
*/
done:;
	if (status == UU_SUCCESS)
	{
		ur_getu_work();
		if (uv_getvnm("Extern Unibase", &view) == UU_SUCCESS)
		{
			uv_update_secondview(3);
		}
	}	
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  ur_cl04(ierr)
**       close the Unibase file used for GET/PUT commands.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
 ur_cl04(ierr)
 UM_int2              *ierr;
{
	int     iostat;                     /* holds status of i/o calls */
	int     saver;
	UV_view view;
	UU_LOGICAL ls;
	UX_pathname savnam;
	extern UU_LOGICAL  UR_save_modals;  /* boolean, UU_TRUE if save modals */
	extern UU_LOGICAL  UR_sav_all;

	*ierr = 0;
	iostat = 0;
	if (lub2 < 0) 
	{
		*ierr = 1;
	}
	else
	{
		ur_getu_work();
		if (uv_getvnm("Extern Unibase", &view) == UU_SUCCESS)
		{
			uv_update_secondview(1);
/*			uv_del_secondview(); */
		}
		ur_getu_second();
/*
...The trick! - check if file saved pointer is 1 which means that
...it is old header format without units in it, so rewrite the header
...adding unit field 
*/
		ux_get_saver (&saver);
/*
...Dump the unibase into file if any changes has been made
*/
		if (UR_EOF_stat)
		{
			if (saver < 2) 
			{
				ux_restore_ptr (lub2);
				uxi_put_file_hdr (lub2,"UX_NOEXTRA",UX_NPRTERRS);
				fflush (otable[lub2].fileptr);
			}
			iostat = ux_file_rewind(lub2,UX_PRTERRS);
			ls    = UR_save_modals;
			UR_sav_all = UR_save_modals = UU_TRUE;
/*
.....vp 2/14/98 add support for ASCI unibase file
*/
			strcpy(savnam,UR_dpn); strcpy(UR_dpn,UBopen);
			iostat = (UB_ftype == 1)? ur_txt_sv02 (lub2,"part"): ur_sv02 (lub2);
			strcpy(UR_dpn,savnam);
			if (iostat == 0) 
				iostat = (UB_ftype == 1)? ur_wrt_txt_env(lub2): ur_environ_out(lub2);
			UR_save_modals = ls;
		}
		if (iostat == 0)
			iostat = ux_close(lub2, UX_NPRTERRS);
		if (iostat) *ierr = 2;
		else lub2 = -1;
	}
	strcpy(UBopen,"");
	iostat = *ierr;
	memrel();
	ur_getu_work();
	if (uv_getvnm("Extern Unibase", &view) == UU_SUCCESS)
	{
		uv_update_secondview(3);
	}
	return(iostat);
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_st02c(ptr) 
**  shows ststistics about open unibase file.
**    PARAMETERS   
**       INPUT  : 
**          ptr = 1 - LIST
**              = 2 - STAT
**       OUTPUT :  
**          none
**    RETURNS      : 
**        0,    read successful (all other error)
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

int ur_st02c(ptr)
UM_int2     *ptr;
{
  char      *uu_toolmalloc();       /*  external utility to get memory  */
  extern int    UR_big_entry;       /* size of the largest entry      */

  int      iostat;                  /* holds status of i/o calls      */
  int      status;                  /* holds status of unibase calls    */
  int      lrecl;                   /* length of record read        */
  struct UR_rcb_rec  temp_rcb;      /* a temporary relation ctrl block  */
  int      atom_size;               /* atom size for var length data   */
  int      atom_ct;                 /* atom count for var length data   */
  struct UR_data  *ent_ptr;         /* a pointer to an entry      */
  char      *b_ptr;                 /* pointer to a byte       */
  int      i,j,n;
  struct attr_def  *atdefs;         /* attribute definitions */
  int      num_attr;                /* number of attributes parsed */
  int      atndx;                   /* index into atdefs */
  struct attr_def  *v_adefs;        /* attribute definitions - varlist */
  int      v_nattr;                 /* number of attributes parsed  */
  char    *buf_ptr;
  int      lst_len;                 /* length of a list in bytes    */
  int      nb;
  struct UR_lpacket  *lp_ptr;       /* pointer to a variable list packet*/

  struct 
  {
    long count;
    char name[8];
  } stat[UR_NUM_REL];

  UU_LOGICAL cmdreject;
  int jmpsave, count;
  int wargs[3];
  char buff[100],buf[35],tmpbuf[2];
  char *ptr1, *ptr1_first;
  char ch;
/*
...
...Prepare storage for entity names
...
*/
    ptr1 = (char *)uu_lsnew();
    ptr1_first = ptr1;
    if (ptr1 == 0)
    {
        ud_wrerr("Could not allocate enough memory for internal usage.");
        status = -1;
        goto l_p99;
    }

/*
...
... clear the array
...
*/
    for (i=0; i<UR_NUM_REL; i++)
    {
       stat[i].count = 0;
       strcpy(stat[i].name,"        ");
    }
/*
...
...Rewind the file and 
...position the pointer to the first block of data
...
*/
  iostat = ux_file_rewind(lub2,UX_PRTERRS);
  if(iostat != 0)
  {
      goto l_p90 ;
  }

/*
...
... find largest possible entry and atom size,
... and allocate space for them 
...
*/
  ent_ptr = (struct UR_data *) uu_toolmalloc(UR_big_entry);
  if(ent_ptr == 0)
  {
    /* " unable to allocate necessary memory" */
    status = URM_NO_MEM_LD;
    goto l_p90;
  }
  status = 0 ;
  temp_rcb.rel_num = 1 ;  /* set for first time */

  while(temp_rcb.rel_num >= 0 )
  {
    /* read an rcb, and check it */
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
		lrecl =  sizeof (temp_rcb);
		nb = 1;
    iostat = ux_read_block(lub2,&temp_rcb,&lrecl,&nb,UX_PRTERRS);

/****/
    if  (iostat == UX_EOF || lrecl == 0) break;
/****/

    if(iostat != 0 || sizeof(struct UR_rcb_rec) != lrecl ||
      (temp_rcb.rel_num >= 0 && UR_rcb[temp_rcb.rel_num].status < 0))
    {
      /* " encountered illegal rcb" */
      status = URM_ILL_RCB ;
      goto l_p90 ;
    }
/*
...
...Realy usefull part.
...
*/
    stat[temp_rcb.rel_num].count += temp_rcb.active_tuple_cnt;
    strcpy(stat[temp_rcb.rel_num].name,temp_rcb.relname);
/***/

    if(temp_rcb.rel_num >= 0 && temp_rcb.rel_num <= UR_MAX_REL)
    {

      /* get the data definition for the relation */
      num_attr = UR_rcb[temp_rcb.rel_num].rel_def->rellen;
      atdefs = UR_rcb[temp_rcb.rel_num].relattr;
      if(num_attr <= 0)
      {
        status = URM_RELNTFND;  /* relation not in data dictionary error */
        goto l_p90 ;
      }

      /* now add the new relation entries to the relation, fetching number */
      /* of entries from a value stored in the saved rcb */
      for ( n = 0; n < temp_rcb.active_tuple_cnt; n++)
      {
        /* read rel. entry */
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
			lrecl =  sizeof (struct UR_data)*UR_big_entry;
			nb = 1;
        iostat = ux_read_block(lub2,ent_ptr,&lrecl,&nb,UX_PRTERRS);

        if(iostat == 0 && lrecl == temp_rcb.tuple_size)
        {
/*
...
...Another usefull part.
...
*/ 
          if((strncmp(ent_ptr->data,"A",1)) >= 0 && (strncmp(ent_ptr->data,"Z",1)) <= 0 )
          {
             for (i=0; i<MAX_NUM; i++)
             {
                if (list[i] == ent_ptr->rel_num) 
                {
                   ptr1 = (char *)uu_lsinsrt(ptr1,10);
                   strncpy(ptr1,"         ",10);
                   strncpy(ptr1,ent_ptr->data,strlen(ent_ptr->data));
                   break;
                }
             }
          }
/***/

          /* build a pointer to the list packets */
          b_ptr = (char *)ent_ptr + temp_rcb.tuple_size -
                (sizeof(int) * temp_rcb.n_varl * 2);
          lp_ptr= (struct UR_lpacket *) b_ptr;

          /* now output the variable lengths lists, if any */
          for(i = 1 ; i <= temp_rcb.n_varl; i++)
          {
            status =ur_get_atom_size(temp_rcb.rel_num,i,&atom_size);
            if(status == 0)
            {
              atom_ct = lp_ptr->atom_cnt;
            }
            else
            {
              status = URM_NEXIST_VARL;
              goto l_p90 ;
            }
            if(atom_ct > 0)
            {
              /* get the data definition for the relation */
/*              atndx = num_attr - temp_rcb.n_varl + i -1;*/
              atndx = num_attr - UR_rcb[temp_rcb.rel_num].n_varl + i -1;
              if(atdefs[atndx].attr_type == STRING)
              {
                v_nattr = 1;  /* one attr */
                v_adefs = &V_Ssadef[2];
              }
              else
              {
                v_nattr = UR_rcb[temp_rcb.rel_num].lparms[i-1].atom_def->rellen;
                v_adefs = UR_rcb[temp_rcb.rel_num].lparms[i-1].atomattr;
              }
              if(v_nattr <= 0)
              {
                status = URM_RELNTFND; /* rel not in data dict */
                goto l_p90 ;
              }
              lst_len = atom_ct * atom_size;
              buf_ptr = uu_toolmalloc(lst_len);  /* get space */
              if (buf_ptr != UU_NULL)
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
						lrecl = lst_len; 
						nb = 1;
                iostat = ux_read_block(lub2,buf_ptr,&lrecl,&nb,UX_PRTERRS);
                if(iostat != 0 || lrecl != lst_len)
                {
                  status = URM_VARL_RDERR ;
                }
              }
              else
              {
                status = URM_NO_MEM_LD;
                goto l_p90;
              }
              uu_toolfree(buf_ptr);
            }
            lp_ptr++ ;
          }

        }
        else
        {
          status = URM_MTID_ERR ;
          goto l_p90 ;
        }
      }
    }
  }
  uu_toolfree((char *)ent_ptr);
/*
...
...Display collected information.
...
*/
/*
.....Disable command reject
*/
    jmpsave = UD_enablejmp;
    UD_enablejmp = UU_FALSE;
    UD_MARK (cmdreject, UU_FALSE);
/*
.....
.....Open the scrolling window
.....
*/
    wargs[1] = 1;
    if(ul_open_window(UL_winrow,UL_wincol,wargs) != UU_SUCCESS)
    {
         return(0);
    }
/*
...
...Which option ? (STAT or LIST)
...
*/
    if (*ptr == 1)
    {
/*
.....Write out the window header (LIST option)
*/
       sprintf(buff,"    *** List of the entities in alphabetical order ***\n");
       ul_win_out(buff,0);
       buff[0] = 0;
       
       for (count = 0, ch='A'; ch<='Z'; ch++)
       {
          ptr1 = ptr1_first;
          for (j=1;;j++)
          {
             if(strlen(buff) > UL_wincol-14)
             {
                if (count >= UL_winrow-2)
                {
                   strcpy (tmpbuf,"");
                   ud_das (UD_DASSTRING,"Press any key to continue...",tmpbuf,1,&i);
                   count = 0;
                }
                ul_win_out(buff,0);
                count++;
                buff[0] = 0;
             }
             ptr1 = (char *)uu_lsnext(ptr1);
             if (!ptr1 ) break;
             if (strncmp(ptr1,&ch,1)) continue;
             sprintf(buf,"%-8s",ptr1);
             strcat(buff,buf);
          }
       }
       if(strlen(buff) > 0) 
       {
          ul_win_out(buff,0);
          buff[0] = 0;
       }
    }
    else if (*ptr == 2)
    {
/*
.....Write out the window header (STAT option)
*/
       sprintf(buff,"    *** General Information ***\n");
       ul_win_out(buff,0);
       buff[0] = 0;
       sprintf(buff,"    Unibase File name: %s\n",UBopen);
       ul_win_out(buff,0);
       buff[0] = 0;

       for (i=0; i<MAX_NUM; i++)
       {
          if (list[i] != 0 && stat[list[i]].count != 0)
          { 
             if(strlen(buff) > UL_wincol-14)
             {
                ul_win_out(buff,0);
                buff[0] = 0;
             }
             sprintf(buf,"    %-8s %5d",stat[list[i]].name,stat[list[i]].count);
             strcat(buff,buf);
          }
       }
       if(strlen(buff) > 0)
          ul_win_out(buff,0);
    } /* End of LIST/STAT */
/*
...
... Press any key to continue...
...
*/
    strcpy (buf,"Press any key to continue...");
    strcpy (buff,"");
    ud_das (UD_DASSTRING,buf,buff,1,&i);
    uu_lsdel(ptr1_first);
    ul_close_window();
/*
.....Enable Command Reject
*/
    UD_UNMARK (cmdreject);
    UD_enablejmp = jmpsave;

l_p90:

l_p99:
  uu_dexit ;
  return(status) ;
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_skip_ent(unifile)
**  skips an entity in open Unibase file.       
**    PARAMETERS   
**       INPUT  : 
**          unifile - file descriptor.
**       OUTPUT :  
**          none
**    RETURNS      : 
**        0,    read successful (all other error)
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

int ur_skip_ent(unifile)
int        unifile;
{
  char      *uu_toolmalloc();       /*  external utility to get memory  */
  extern int    UR_big_entry;       /* size of the largest entry      */

  int      iostat;                  /* holds status of i/o calls      */
  int      status;                  /* holds status of unibase calls    */
  int      lrecl;                   /* length of record read        */
  struct UR_rcb_rec  temp_rcb;      /* a temporary relation ctrl block  */
  int      atom_size;               /* atom size for var length data   */
  int      atom_ct;                 /* atom count for var length data   */
  struct UR_data  *ent_ptr;         /* a pointer to an entry      */
  char      *b_ptr;                 /* pointer to a byte       */
  int      i,n;
  struct attr_def  *atdefs;         /* attribute definitions */
  int      num_attr;                /* number of attributes parsed */
  int      atndx;                   /* index into atdefs */
  struct attr_def  *v_adefs;        /* attribute definitions - varlist */
  int      v_nattr;                 /* number of attributes parsed  */
  char    *buf_ptr;
  int      lst_len;                 /* length of a list in bytes    */
  int      nb;
  struct UR_lpacket  *lp_ptr;       /* pointer to a variable list packet*/

/*
...
... find largest possible entry and atom size,
... and allocate space for them 
...
*/
  ent_ptr = (struct UR_data *) uu_toolmalloc(UR_big_entry);
  if(ent_ptr == 0)
  {
    /* " unable to allocate necessary memory" */
    status = URM_NO_MEM_LD;
    goto l_p90;
  }
  status = 0 ;

    /* read an rcb, and check it */
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
	lrecl =  sizeof (temp_rcb);
	nb = 1;
    iostat = ux_read_block(unifile,&temp_rcb,&lrecl,&nb,UX_PRTERRS);

    if(iostat != 0 || sizeof(struct UR_rcb_rec) != lrecl ||
      (temp_rcb.rel_num >= 0 && UR_rcb[temp_rcb.rel_num].status < 0) ||
       temp_rcb.rel_num == -1)
    {
      status = URM_ILL_RCB ;
      goto l_p90 ;
    }

    if(temp_rcb.rel_num >= 0 && temp_rcb.rel_num <= UR_MAX_REL)
    {

      /* get the data definition for the relation */
      num_attr = UR_rcb[temp_rcb.rel_num].rel_def->rellen;
      atdefs = UR_rcb[temp_rcb.rel_num].relattr;
      if(num_attr <= 0)
      {
        status = URM_RELNTFND;  /* relation not in data dictionary error */
        goto l_p90 ;
      }

      /* now add the new relation entries to the relation, fetching number */
      /* of entries from a value stored in the saved rcb */
      for ( n = 0; n < temp_rcb.active_tuple_cnt; n++)
      {

/*        uri_sp04a(&(temp_rcb.rel_num),-3);  */

        /* read rel. entry */
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
			lrecl =  sizeof (struct UR_data)*UR_big_entry;
			nb = 1;
        iostat = ux_read_block(unifile,ent_ptr,&lrecl,&nb,UX_PRTERRS);

        if(iostat == 0 && lrecl == temp_rcb.tuple_size)
        {
          /* build a pointer to the list packets */
          b_ptr = (char *)ent_ptr + temp_rcb.tuple_size -
                (sizeof(int) * temp_rcb.n_varl * 2);
          lp_ptr= (struct UR_lpacket *) b_ptr;

          /* now output the variable lengths lists, if any */
          for(i = 1 ; i <= temp_rcb.n_varl; i++)
          {
            status =ur_get_atom_size(temp_rcb.rel_num,i,&atom_size);
            if(status == 0)
            {
              atom_ct = lp_ptr->atom_cnt;
            }
            else
            {
              status = URM_NEXIST_VARL;
              goto l_p90 ;
            }
            if(atom_ct > 0)
            {
              /* get the data definition for the relation */
/*              atndx = num_attr - temp_rcb.n_varl + i -1;*/
              atndx = num_attr - UR_rcb[temp_rcb.rel_num].n_varl + i -1;
              if(atdefs[atndx].attr_type == STRING)
              {
                v_nattr = 1;  /* one attr */
                v_adefs = &V_Ssadef[2];
              }
              else
              {
                v_nattr = UR_rcb[temp_rcb.rel_num].lparms[i-1].atom_def->rellen;
                v_adefs = UR_rcb[temp_rcb.rel_num].lparms[i-1].atomattr;
              }
              if(v_nattr <= 0)
              {
                status = URM_RELNTFND; /* rel not in data dict */
                goto l_p90 ;
              }
              lst_len = atom_ct * atom_size;
              buf_ptr = uu_toolmalloc(lst_len);  /* get space */
              if (buf_ptr != UU_NULL)
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
						lrecl = lst_len; 
						nb = 1;
                iostat = ux_read_block(unifile,buf_ptr,&lrecl,&nb,UX_PRTERRS);
                if(iostat != 0 || lrecl != lst_len)
                {
                  status = URM_VARL_RDERR ;
                }
              }
              else
              {
                status = URM_NO_MEM_LD;
                goto l_p90;
              }
              uu_toolfree(buf_ptr);
            }
            lp_ptr++ ;
          }

        }
        else
        {
          status = URM_MTID_ERR ;
          goto l_p90 ;
        }
      }
    }
  uu_toolfree((char *)ent_ptr);
l_p90:
  uu_dexit ;
  return(status) ;
}
