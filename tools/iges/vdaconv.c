#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**    NAME         : vdaconv.c  
**       CONTAINS:
**             vda_create_global
**             vda_create_dblk      
**             vda_create_directory
**             vda_read_record      
**             vda_error            
**             vda_deblank             
**             vda_find_next_char  
**             vda_get_command      
**             vda_find_name     
**             vda_get_name         
**             vda_name_and_type 
**             vda_get_next_char    
**             vda_get_real     
**             vda_get_integer      
**             vda_last_char     
**             vda_skip_parameters  
**    COPYRIGHT 1989 (c) MILLS DATA SYSTEMS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       vdaconv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:54
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "vda.h"
#include "xenv1.h" 	/* used by ux_() calls */

extern int vda_fd;
extern int num_entities;

struct vda_ent vda_dir[MAX_ENTITIES];
int header_drec;   /* directory record index of header record */
int beginset_drec; /* directory record index of last beginset */

/* several routines manipulate recnum, pchar and v_buff as side-effects;  
   the "current character" is v_buff[pchar] */
char v_buff[81];   /* current record - extra character to hold the '\0' */
int recnum;        /* record number of current record */
int pchar;         /* character index into current record */

extern int read_count;

char *vda_command[] = {
   "        ",
   "HEADER  ",
   "BEGINSET",
   "ENDSET  ",
   "GROUP   ",
   "TMAT    ",
   "TLIST   ",
   "END     ",
   "POINT   ",
   "PSET    ",
   "MDI     ",
   "CIRCLE  ",
   "CURVE   ",
   "SURF    ",
   "CONS    ",
   "FACE    ",
   "TOP     ",
   };

/*********************************************************************
**    I_FUNCTION     :  vda_in_convert()
**          Convert VDA input file to neutral file format
**    PARAMETERS   
**       INPUT  : 
**          input    
**       OUTPUT :  
**          output   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vda_in_convert()
   {
   struct global_rec gblk;          /* global record sturcture */
   struct dir_rec dblk;             /* directory record */
   int i,j, vda_type;
   int status;
   int lu;
   UU_KEY_ID key;
   int drec;                        /* directory record pointer */
   int ent_cnt[VDA_NUM+1];				
   int ent_not_xlated_cnt[VDA_NUM+1];  /* count untranslated entities */
   int not_xlated_flag;             /* some entities not translated */
   char c[150000];                  /* parameter data storage area */
	UX_pathname fnameu,fnames,fnamea;
   char *cptr;
   FILE  /* *fopen(),*/ *ddl_fd;
   char *ux_getenv();
   int uig_setup_save();
	char	*charptr;
	UX_pathname	pathname;
	UU_LOGICAL	found;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   vda_create_global(&gblk);                    /* get global block */

   vda_get_label_type();					 

   /*NCL: to handle writes to screen correctly on VAX */
   uig_tty_mode(SCROLL);

   status = uig_setup_save(&lu, fnameu, fnames, fnamea);     /* setup intermediate file */
   if(status != 0) 
		{
		/*NCL: reset */
		uig_tty_mode(RESET);
		return;
		}

   /*NCL: get curve tolerance for parametric splines */
   uig_get_curve_toler(gblk.units);	

   for(i=0;i<=VDA_NUM;i++)                     /* clear statistics arrays */
      {                                      /* clear all counters  */
      ent_cnt[i] = 0;
      ent_not_xlated_cnt[i] = 0;
      }
   for (i=0;i<MAX_ENTITIES;i++)				 /* clear flags for all entities */
	  xlated_flag[i] = NULL;
   not_xlated_flag = 0;
   read_count = 0;
   number_of_masters = 0;
   charptr = UU_NULL;
   if (ux_get_syspath("IGES_DDL",&charptr,pathname,&found,
					  UX_NPRTERRS|UX_NQUOTES)==UU_SUCCESS)
	  {
   	/* cptr = ux_getenv("IGES_DDL");	
   	if(cptr == 0) return;	*/
   	ddl_fd = fopen(pathname,"r");           /* open ddl file */
   	status = ur_init_data_dict(ddl_fd);       /* initialize data dictionary */
	  }
	else
		return;
	uu_lsdel(charptr);

   /*  set up standard output mode on tty */

   uig_tty_mode(SCROLL);
   uig_str_out("Converting VDA input file to internal format\n",UU_TRUE);

/***************************
**                        **
** primary directory loop **
**                        **
***************************/

   /* process directory twice */
   /* first pass: process all tmat, tlist and group entities */
   for(drec = 1;drec <= num_entities;drec++)
      {
	  status = vda_process_tmat_tlist_group(drec,&dblk,c,&vda_type);
	  /* status =
          0: processed OK        
		 -3: ignore completely (to be processed next pass)
		 -1: error
      */
	  if (status != -3)
		{
	    if (status == 0)
	  	  ent_cnt[vda_type]++;
        else
		  {
		  not_xlated_flag = 1;
		  ent_not_xlated_cnt[vda_type]++;
		  }
	    update_counts(key, drec);    /* update translated counts */
		}
	  }

   /* second pass: process all entities except tmat, tlist and group */
   for(drec = 1;drec <= num_entities;drec++)
      {
	  status = vda_get_data(drec,&dblk,c,&vda_type); 
	  /* status =
          0: process this entity;
		 -3: ignore completely (processed and counted in first pass);
		 -2: count as processed, but no more processing required;
		 -1: error, or cannot process for some reason.
      */
	  if (status == 0)							/* process this entity */
		{
	    init_label(dblk.rel_type, drec);	/* unique label names */
	    check_color(&dblk);				/* set default colors */
	    current_dir_number = drec;
	    key = -1;
        uig_in_dispat(&gblk,&dblk,c,&key);     /* handle individual types */
		if (key == -1)
		  status = -1;   /* error */
        }
	  if (status != -3)
		{
	    if (status != -1)
	  	  ent_cnt[vda_type]++;
        else
		  {
		  not_xlated_flag = 1;
		  ent_not_xlated_cnt[vda_type]++;
		  }
	    update_counts(key, drec);    /* update translated counts */
		}
      }
/****************************
**                         **
**  now save file          **
**                         **
****************************/

   uig_str_out("Writing Unibase file to disk\n",UU_TRUE);	
   uig_save(lu, fnameu, fnames, fnamea);

/****************************
**                         **
**  now print statistics   **
**                         **
****************************/

   uig_tty_mode(RESET);

   uig_list_out(" \n",UU_FALSE);
   uig_list_out("Load Statistics:\n",UU_TRUE);
   uig_list_out("   ENTITY TYPE        COUNT\n",UU_TRUE);
   for(i=1;i<=VDA_NUM;i++)		
      {
      if(ent_cnt[i] != 0)
         {
         sprintf(p_buff,"    %s          %d\n",
			vda_command[i],ent_cnt[i]);
         uig_list_out(p_buff,UU_TRUE);
         }
      }

	if (not_xlated_flag)
	{
	uig_list_out("   ENTITIES NOT TRANSLATED\n",UU_TRUE);
	for(i=1;i<=VDA_NUM;i++)
		{
		if(ent_not_xlated_cnt[i] != 0)
			{
			sprintf(p_buff,"    %s          %d\n",
				vda_command[i], ent_not_xlated_cnt[i]);
			uig_list_out(p_buff,UU_TRUE);
			}
		}
	}
   uig_list_out(" \n", UU_FALSE);
   uig_prompt(2,i);
   }
/*********************************************************************
**    I_FUNCTION     :  vda_create_global(blk)
**       Create the global section information.
**    PARAMETERS   
**       INPUT  :    none 
**       OUTPUT :  
**          blk      structure which contains global info
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vda_create_global(blk)
	struct global_rec *blk;
	{
	/* set defaults */
	blk->delim = ',';
	blk->term  = ';';    
	blk->sing_pr_int = 16;
	blk->sing_pr_exp = 8;
	blk->sing_pr_mant = 24;
	blk->doub_pr_exp = 8;
	blk->doub_pr_mant = 56;
	blk->scale = 1.0;
	blk->units = 2;  /* millimeters */ 
	strcpy(blk->unitstr, "2HMM");   
	strcpy(blk->datestr, "13H000000.000000");
	blk->granularity = 1.0e-5;
	blk->coor_max = 100;
	unit_scale = 0.1 / 2.54;
	line_scale = 1.0; 
   }

/*********************************************************************
**    I_FUNCTION     :  vda_create_dblk(drec,dblk,reltype)
**       Create an iges directory block for one entity 
**    PARAMETERS   
**       INPUT  : 
**			drec     record number of entity 
**			reltype  iges entity type 
**       OUTPUT :  
**          blk      structure which contains global info
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vda_create_dblk(drec,dblk,reltype)
   int drec;
   struct dir_rec *dblk;
   int reltype;
   {
   dblk->drec_num = drec;
   dblk->rel_type = reltype; 
   dblk->par_ptr  = 0;
   dblk->line_font= 1; 
   dblk->level    = 0; 
   dblk->view_ptr = 0; 
   dblk->matrix_ptr = vda_dir[drec].tran;
   dblk->blank    = 0; 
   dblk->sub_swt  = 2; 
   dblk->use_flg  = 0; 
   dblk->hier     = 1; 
   dblk->line_wt  = 0; 
   dblk->pen_no   = 2; 
   dblk->par_cnt  = 0; 
   dblk->form_no  = 0; 
   dblk->seq_no   = 0; 
   strncpy(dblk->label, vda_dir[drec].name, 8);
   }


/*********************************************************************
**    I_FUNCTION     :  vda_create_directory()
**       Create a directory of all entities in vda file.
**    PARAMETERS   
**       INPUT  :    none 
**       OUTPUT :    none 
**    RETURNS      : none
**    SIDE EFFECTS : sets num_entities
**    WARNINGS     : none
*********************************************************************/

vda_create_directory()
   {
   int drec, status, type;        
   int i;
   char name[9];
   header_drec = 0;
   beginset_drec = 0;
   recnum = 0;
   drec = 0;
   status = vda_read_record();  /* skips comment cards */
   while (status != -2)  /* stop only at end-of-file */
	 {     
     if (drec >= MAX_ENTITIES)
	   {
	   uig_list_out("Input file too large\n", UU_TRUE);
	   sprintf(p_buff,"Only %d entities processed\n", drec);
	   uig_list_out(p_buff, UU_TRUE);
	   num_entities = drec;
       return;
	   }
	 pchar = 0;
	 status = vda_name_and_type(name,&type);
	 if (status == 0)
	   {     
	   drec++;
	   strcpy(vda_dir[drec].name, name);
	   vda_dir[drec].type   = type;
 	   vda_dir[drec].recnum = recnum;
	   vda_dir[drec].tran   = 0;
	   vda_dir[drec].group  = 0;
	   vda_dir[drec].pchar  = pchar;  
	   status = vda_skip_parameters(type);
	   }    
	 else
	 if (status == -1)
	   {
	   /* syntax error; try to skip to next entity */  
       status = vda_skip_parameters(0);
	   }
     }    
   if (vda_dir[1].type != vda_header_type)
	 {
	 recnum = vda_dir[1].recnum;
	 pchar = 0;
	 vda_read_record();
     vda_error("first record must be HEADER\n");
	 }
   if (vda_dir[drec].type != vda_end_type)
	 {
	 recnum = vda_dir[drec].recnum;
	 pchar = 0;
	 vda_read_record();
     vda_error("last record must be END\n");
	 }
   num_entities = drec;
   }

/*********************************************************************
**    I_FUNCTION     :  vda_read_record();
**       Read a record from vda file.             
**    PARAMETERS   
**       INPUT  :    none 
**       OUTPUT :    none 
**    RETURNS      : 0 if record read successfully
**    SIDE EFFECTS : skip vda comment records;
**                   set recnum of record read.
**    WARNINGS     : none
*********************************************************************/

vda_read_record()
	{
	int loc, offset, bytes;

	offset = recnum * 80;  /* byte offset from start of file */
	loc = lseek(vda_fd, offset, 0);

	if (loc != offset)
		return(-1);

	bytes = read(vda_fd, v_buff, 80);

	while (bytes>0)
		{
		/* Null terminate in case we need it ... */
		v_buff[80] = '\0';  /* end of string */

		if (v_buff[0] != '$' || v_buff[1] != '$')
			/* found non-comment record: return success */
			return(0);

		/* comment record: read next record */
		recnum++;         
		bytes = read(vda_fd, v_buff, 80);
		}

	return(-2);   /* -2 indicates end-of-file, not error */
	}

/*********************************************************************
**    I_FUNCTION     :  vda_error(msg)    
**       Display syntax error or other error in vda input         
**    PARAMETERS   
**       INPUT  :    none 
**       OUTPUT :    none 
**    RETURNS      : none                           
**    SIDE EFFECTS : write record and error message to listing file.
**    WARNINGS     : none
*********************************************************************/

vda_error(msg)
   char msg[];
   {
   char buff2[80],tmpstr[256];
   char line[9];
   int i,j;
   /* print 3 lines in listing file only */
   uig_list_out(v_buff, UU_FALSE);
   if (pchar > 0)
	 {
     for (i=0;i<80;i++)
	   buff2[i] = ' ';
     buff2[pchar] = '*';
	 buff2[79] = '\0';
     uig_list_out(buff2, UU_FALSE);
	 }
   uig_list_out(msg, UU_FALSE);

   /* print next line on screen only */
   for (i=0,j=72;i<8;i++,j++)
	  line[i] = v_buff[j];
   line[8] = '\0';
   sprintf(tmpstr,"error at line %s: %s\n", line, msg);
   uig_str_out(tmpstr, UU_TRUE);
   }

/*********************************************************************
**    I_FUNCTION     :  vda_deblank                   
**       Skip blank characters, starting at current character.       
**    PARAMETERS   
**       INPUT  :    none 
**       OUTPUT :    none 
**    RETURNS      : none                           
**    SIDE EFFECTS : sets current character to non-blank char, if any;
**                   sets pchar to 72 if none. 
**    WARNINGS     : none
*********************************************************************/

vda_deblank()
   {
   int i;
   for (i=pchar;i<72;i++)
	 if (v_buff[i] != ' ')
	   {
	   pchar = i;
	   return;
	   }
   pchar = i;
   }

/*********************************************************************
**    I_FUNCTION     :  vda_find_next_char     
**       Find next non-blank character in this or next record.
**    PARAMETERS   
**       INPUT  :    none
**       OUTPUT :    none 
**    RETURNS      : 0 if OK; -1 if no non-blank char on this or next rec.
**    SIDE EFFECTS : sets recnum and pchar to non-blank char.
**    WARNINGS     : none
*********************************************************************/

vda_find_next_char()
   {
   int status;
   vda_deblank();
   if (pchar>=72)
	 {
	 recnum++;         
	 status = vda_read_record();
	 if (status) return(status);
	 pchar = 0;
	 vda_deblank();
	 if (pchar>=72)
		{
		vda_error("non-blank character expected\n");
		return(-1);
		}
	 }
   return(0);
   }

/*********************************************************************
**    I_FUNCTION     :  vda_get_command(command)
**       Return command name.                   
**    PARAMETERS   
**       INPUT  :    none
**       OUTPUT :  
**          command  character string containing command identifier.     
**    RETURNS      : 0 if OK; -1 if error.
**    SIDE EFFECTS : set pchar to index of first character after command.
**    WARNINGS     : none
*********************************************************************/

vda_get_command(command)
   char command[];
   {
   int ch, i, status;
   vda_deblank();
   if (pchar >= 72)
	 {
     vda_error("command expected\n");
	 return(-1);
	 };
   ch = v_buff[pchar];
   i = 0;
   while (ch!=' ' && ch!=',' && 
		  ch!='=' && ch!='/'   )
	 {
	 if (i>7)
	   {
	   vda_error("command must not exceed 8 characters\n");
	   return(-1);
	   };
	 command[i] = ch;
	 i++;
	 pchar++;
	 ch = v_buff[pchar];
	 }
   return (0);
   }

/*********************************************************************
**    I_FUNCTION     :  vda_find_name(name,drec)
**       Find record which defined the given name.
**    PARAMETERS   
**       INPUT  : 
**		    name     character string to find. 
**       OUTPUT :  
**          drec     directory index of entity with this name.
**    RETURNS      : 0 if OK; -1 if error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vda_find_name(name, drec)
   char name[];
   int  *drec;
   {
   int i;
   *drec = 0;
   for (i=0;i<num_entities;i++)
	  if (strcmp(name, vda_dir[i].name) == 0)
		 {
		 *drec = i;
		 break;
		 }
   if (*drec == 0)
	  {
      vda_error("unknown identifier\n");
	  return(-1);
	  }
   return (0);
   }

/*********************************************************************
**    I_FUNCTION     :  vda_get_name(name,check)
**       Return identifier name.                   
**    PARAMETERS   
**       INPUT  : 
**	        check    specifies type of check for next character. 
**       OUTPUT :  
**          name     character string containing name. 
**    RETURNS      : 0 if OK; -1 if error.
**    SIDE EFFECTS : sets pchar to next character after name.
**    WARNINGS     : none
*********************************************************************/

vda_get_name(name,check)
   char name[]; 
   int check; /* 1 if must be followed by comma */
   {
   int ch, i, j, status;
   strcpy(name, "        ");
   /* jkd 58 */
   /* error "name expected" occured when first name was not on the same 
	  record as the beginning of the group. */
   /*
   vda_deblank();
   if (pchar >= 72)
	 {
     vda_error("name expected\n");
	 return(-1);
	 };
	*/
	status = vda_find_next_char();
	if (status) return (status);
   ch = v_buff[pchar];
   i = 0;
   j = 0;
   while (ch!=',' && 
		  ch!='=' && ch!='/'   )
	 {
	 /* temporarily, ignore embedded blanks */
	 if (ch!=' ')
	   {
	   if (i>7)
	     {
	     vda_error("name must not exceed 8 characters\n");
	     return(-1);
	     };
	   name[i] = ch;
	   i++;
	   }	
	 j++;
	 pchar++;
	 if (pchar>=72 || j>=9)
		break;
	 ch = v_buff[pchar];
	 }
   vda_deblank();
   return(check_curr_char(check));
   }

/*********************************************************************
**    I_FUNCTION     :  vda_name_and_type(name,type)
**       Return name and command type of current entity.
**    PARAMETERS   
**       INPUT  :    none 
**       OUTPUT :  
**          name     character string containing name.
**          type     command type of current entity.   
**    RETURNS      : none
**    SIDE EFFECTS : sets current character to first char. after "/".
**    WARNINGS     : none
*********************************************************************/

vda_name_and_type(name,type)
   char name[];
   int *type;
   {
   int i, status;
   char command[9];
   strcpy(name, "        ");
   status = vda_get_name(name, 0);
   if (status) return(status);
   vda_deblank();
   if (v_buff[pchar] != '=')
	 {
     vda_error("'=' expected after vda name\n");
	 return(-1);
	 };
   pchar++;        
   strcpy(command, "        ");
   status = vda_get_command(command);
   if (status) return(status);
   *type = 0;
   for (i=1;i<VDA_NUM;i++)
	 {
	 if (strcmp(command,vda_command[i]) == 0)
	   {
	   *type = i;
	   break;
	   }
	 }
   if (*type == 0)
	 {
     vda_error("unknown vda command word\n");
	 return(-1);
	 };
   vda_deblank();
   if (pchar < 72)
     if (v_buff[pchar] == '/')
	   {
	   pchar++;        
	   }
     else
	   {
	   vda_error("'/' expected after command word\n");
	   return(-1);
	   }
   return (0);
   }


/*********************************************************************
**    I_FUNCTION     :  check_curr_char(check)          
**       Perform specified check on current character. 
**    PARAMETERS   
**       INPUT  : 
**          check    specifies type of check:                   
**                     0: no check;                             
**                     1: current character must be comma;      
**                     2: current character must be end of line.
**       OUTPUT :  
**    RETURNS      : 0 if OK; -1 if current character fails check.
**    SIDE EFFECTS : if comma, sets current character to next.
**    WARNINGS     : none
*********************************************************************/

int check_curr_char(check)
   int check;
   {
	switch (check)
	   {
	   case 0: /* no check */
		  return(0);
	   case 1: /* must be comma */
          if (v_buff[pchar] != ',')
	         {
		     vda_error("',' expected\n");
	         return(-1);
	         }
	      pchar++;        
		  return(0);
       case 2: /* must be end of line */
	      if (pchar != 72)
	         {
		     vda_error("end of record expected\n");
	         return(-1);
	         }
		  return(0);
	   default:
		return (-1);
       }
   }
 
/*********************************************************************
**    I_FUNCTION     :  vda_get_real(val,check)         
**       Return value of real number on current record.
**    PARAMETERS   
**       INPUT  : 
**          check    specifies type of check for next character.
**       OUTPUT :  
**          val      value of real number.
**    RETURNS      : 0 if OK; -1 if error.           	
**    SIDE EFFECTS : sets current character to character after real.
**    WARNINGS     : none
*********************************************************************/

vda_get_real(val,check)
  UU_REAL *val;
  int check; /* 1 if must be followed by comma */
  {
  int status, ctout;        
  char outbuff[81];
  double uig_atof();
  status = vda_find_next_char();
  if (status) return(status);
  status = uig_nxtarg(1,v_buff,pchar,&pchar,&ctout,outbuff);
  if (status) return(status);
  /* uig_nxtarg returns pchar at first char after comma,
	 or -72 to indicate end of record */
  if (pchar>0)
    pchar = pchar-1;
  else
	pchar = 72;
  outbuff[ctout] = '\0';
  *val = uig_atof(outbuff);
  return(check_curr_char(check));
  }

/*********************************************************************
**    I_FUNCTION     :  vda_get_integer(val, comma)     
**       Return value of integer number on current record.
**    PARAMETERS   
**       INPUT  : 
**          check    specifies type of check for next character.
**       OUTPUT :  
**          val      value of integer number.                      
**    RETURNS      : 0 if OK; -1 if error.           	
**    SIDE EFFECTS : sets current character to character after integer.
**    WARNINGS     : none
*********************************************************************/

vda_get_integer(val,check)
  int *val;
  int check;
  {
  int status, ch;
  status = vda_find_next_char();
  if (status) return(status);
  vda_deblank();
  ch = v_buff[pchar];
  if (pchar >= 72 || ch < '0' || ch > '9')
	{
	vda_error("integer constant expected\n");
	return(-1);
	};
  *val = 0;
  ch = v_buff[pchar];
  while (ch >= '0' && ch <= '9')
	{
	*val = *val*10 + (ch-'0');
	pchar++;        
    ch = v_buff[pchar];
	}
  vda_deblank();
  return(check_curr_char(check));
  }


/*********************************************************************
**    I_FUNCTION     :  vda_last_char              
**       Return last non-blank character on this record.
**    PARAMETERS   
**       INPUT  :    none 
**       OUTPUT :    none 
**    RETURNS      : last non-blank character.       	
**    SIDE EFFECTS : none                       
**    WARNINGS     : none
*********************************************************************/

vda_last_char()
   {
   int i;
   for (i=71;i>=0;i--)
	 {
	 if (v_buff[i] != ' ')
	   return(v_buff[i]);
	 }
   return (0);
   }

/*********************************************************************
**    I_FUNCTION     :  vda_skip_parameters(type)       
**       Skip all parameters of current entity.     
**    PARAMETERS   
**       INPUT  : 
**          type     entity type; used to decide how to skip parameters.
**       OUTPUT :    none 
**    RETURNS      : 0 if OK; -1 if error.           	
**    SIDE EFFECTS : sets current character to start of next entity.
**    WARNINGS     : none
*********************************************************************/

vda_skip_parameters(type)
   int type;
   {
   int status;
   int count;
   if (type == vda_header_type)
	 {
	 status = vda_get_integer(&count, 0);
	 if (status) return(status);
	 while (count != 0)
	   {
	   recnum++;         
	   status = vda_read_record();
	   if (status) return(status);
	   count--;
	   }
     }
   else
	 {
     while (vda_last_char() == ',')
	   {
	   recnum++;          
	   status = vda_read_record();
	   if (status) return(status);
	   }
     }
   /* now read next record, if any */
   recnum++;          
   status = vda_read_record();
   return(status);
   }

#endif
