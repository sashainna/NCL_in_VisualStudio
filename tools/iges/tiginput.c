/*********************************************************************
**    NAME         :  tiginput.c
**       CONTAINS:
**                  uig_terminal_rec
**                  uig_summary
**                  uig_get_directory
**                  uig_list_start
**                  uig_get_global
**                  uig_stor_global
**                  uig_insert_sort_two_arrays
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**			tiginput.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:47
*********************************************************************/

#include "tiges.h"
#include "umath.h"
#include <stdio.h>
#include "udebug.h"
#include "ulist.h"
#include "nclver.h"
#include "ribase.h"
#include "riddle.h"
#if UU_COMP != UU_WIN2K
#include "tigmf.h"
#endif
#if UU_COMP == UU_WIN2K
#include <io.h>
#define write _write
#define read _read
#define close _close
#define open _open
#define creat _creat
#define lseek _lseek
#endif


#if (UU_COMP==UU_IRIS) || (UU_COMP==UU_IRIS4D)
#include <fcntl.h>
#endif
#if UU_COMP==UU_RIDGE
#include <fcntl.h>
#endif
#if UU_COMP==UU_APOLLO
#include <fcntl.h>
#endif

#if UU_COMP!=UU_RIDGE
#if UU_COMP!=UU_APOLLO
#if UU_COMP!=UU_VAXVMS
#if UU_COMP!=UU_IRIS
#if UU_COMP!=UU_IRIS4D
#if UU_COMP!=UU_WIN2K
#include <sys/file.h>
#endif
#endif
#endif
#endif
#endif
#endif

#if UU_COMP!=UU_VAXVMS
#include <sys/types.h>
#endif

static char *deft_str[] ={
	"4HINCH",
	"13H000000.000000",
};

UU_REAL max_line;			/*jkd33: used for line_scale */
int line_wt_grads;			/*jkd33: number of line gradations */

/*********************************************************************
**    I_FUNCTION     :  int uig_terminal_rec()
**       Decode IGES terminal record for the sector pointers
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_terminal_rec()   

{       
	char f_buff[80];			/* file read buffer */
	char l_buf[9];				/* decode buffer */
	int f_bytes = 80;			/* num of bytes to read */
	int i,j,k;
	int atoi();
	UU_LOGICAL ok;
	int x = 0;				/*jkd45: inx of "S" */

	/*----------------------------------------------------------------------
	** Start of executable code
	**--------------------------------------------------------------------*/
	for (k = 1, ok = UU_FALSE; k<10 && !ok; k++)
	{
		j = k*80;
	 	lseek(iges_fd,-j,2);			/* set file pointer */
 		i = read(iges_fd,f_buff,f_bytes);				/* read record */

		if (f_buff[1] == 'S')	/*jkd45: S must be in char 0 or char 1*/
			x = 1;

		/*jkd7: make sure term rec valid */
		if (f_buff[x+ 0] != 'S' || f_buff[x+ 8] != 'G' ||   	/*jkd45*/
			f_buff[x+16] != 'D' || f_buff[x+24] != 'P')			/*jkd45*/
		{
			if (k < 9) continue;
			for (i=0;i<4;i++)
				sect_ptr[i] = 0;
			return(0);
		}
/*
..... set the last, 8-th, bit to zero
*/
		for(i=0;i<80;i++)
			f_buff[i] &= 0x7f;

		if (f_buff[72] == 'T')
			ok = UU_TRUE;
	}

																/* decode record */
	for (i=0;i<4;i++)
	{
		for (j=i*8,k=0;j<i*8+8;j++,k++)
		  l_buf[k] = f_buff[j];  	/* transfer to local array */
		l_buf[8] = '\0';
		l_buf[0] = ' ';
		sect_ptr[i] = atoi(l_buf);
	}
	return(0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_summary()
**       Generate a summary table of the IGES file data
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_summary() 
{
	struct dir_rec blk;						/* directory record structure */
	struct global_rec gblk;					/* directory record structure */
	int irec;									/* directory record number */
	int ent_cnt[IG_NUM];						/* entity count array */
	int i,j,layer, num_layers, layer_found;
	int *layer_array, *entity_count_array, num_entities;
	char c[9];
	UU_LIST layer_list, layer_entity_count_list;
	void uig_insert_sort_two_arrays();

	/*----------------------------------------------------------------------
	** Start of executable code
	**--------------------------------------------------------------------*/
	for(irec=0;irec<IG_NUM;irec++) ent_cnt[irec] = 0;    /* arrays          */

	uu_list_init(&layer_list, sizeof(int), 20, 10);
	uu_list_init(&layer_entity_count_list, sizeof(int), 20, 10);

	for(irec=1;irec<(sect_ptr[2]+1);irec++,irec++)		/* loop over records */
	{
		uig_get_directory(irec,&blk);				/* get data */

		if(irec > 0)
		{
			for(i=0;i<IG_NUM;i++)						/* find appropriate count box */
			{
				if(blk.rel_type == iges_type[i])
				{
					ent_cnt[i]++;
					j=blk.level;

					layer_found = -1;
					num_layers = UU_LIST_LENGTH (&layer_list);
					layer_array = (int *) UU_LIST_ARRAY(&layer_list);
					for (i = 0 ; i < num_layers ; i++)
					{
						if (j == layer_array[i])
						{
							layer_found = i;
							break;
						}
					}
					if (layer_found == -1)
					{
						i = 1;
						uu_list_push(&layer_entity_count_list, &i);
						uu_list_push(&layer_list, &blk.level);
					}
					else
					{
						entity_count_array = (int *) UU_LIST_ARRAY (&layer_entity_count_list);
						entity_count_array[layer_found]++;
					}
					break;
				}
			}
		}
	}
	uig_clear();
	uig_str_out("File Summary:\n\n",  UU_TRUE);
	uig_list_start();									/* list start records */
	uig_get_global(&gblk);								/* get global data */

	/* cpp: add new fields to the listing */

	uig_list_out("Product Information:\n", UU_TRUE);
	uig_list_out("   Product Id (sending): ", UU_TRUE);
	sprintf(p_buff,"%s\n" ,&gblk.prod_id1[0]);
	uig_list_out(p_buff, UU_TRUE);
	uig_list_out("   Input File Name: ", UU_TRUE);
	sprintf(p_buff,"%s\n", &gblk.file_id[0]);
	uig_list_out(p_buff, UU_TRUE);
	uig_list_out("   Source System Id: ", UU_TRUE);
	sprintf(p_buff,"%s\n", &gblk.sys_id[0]);
	uig_list_out(p_buff, UU_TRUE);
	uig_list_out("   Preprocessor Version: ", UU_TRUE);
	sprintf(p_buff,"%s\n", &gblk.process_id[0]);
	uig_list_out(p_buff, UU_TRUE);
	uig_list_out("   Product Id (receiving): ", UU_TRUE);
	sprintf(p_buff,"%s\n" ,&gblk.prod_id2[0]);
	uig_list_out(p_buff, UU_TRUE);
	uig_list_out("   Author: ", UU_TRUE);
	sprintf(p_buff,"%s\n" ,&gblk.author[0]);
	uig_list_out(p_buff, UU_TRUE);
	uig_list_out("   Author's Organization: ", UU_TRUE);
	sprintf(p_buff,"%s\n" ,&gblk.author_org[0]);
	uig_list_out(p_buff, UU_TRUE);
	sprintf(p_buff,"   Global Scale: %f\n", gblk.scale);
	uig_list_out(p_buff, UU_TRUE);
	uig_list_out("Data Information:\n",UU_TRUE);
	c[0] = gblk.datestr[2];
	c[1] = gblk.datestr[3];
	c[2] = '/';
	c[3] = gblk.datestr[4];
	c[4] = gblk.datestr[5];
	c[5] = '/';
	c[6] = gblk.datestr[0];
	c[7] = gblk.datestr[1];
	c[8] = '\0';
	sprintf(p_buff,"   Creation Date: %s\n   Data Units: %s\n"
							,c,&gblk.unitstr[0]);
	uig_list_out(p_buff,UU_TRUE);
	uig_list_out("Entity Information:\n",UU_TRUE);
	uig_list_out("   ENTITY TYPE        COUNT\n",UU_TRUE);
	for(i=0;i<IG_NUM;i++)
	{
		if(ent_cnt[i] != 0)
		{
			sprintf(p_buff,"    %s        %d\n",iges_name[i],ent_cnt[i]);
			uig_list_out(p_buff,UU_TRUE);
		}
	}
#if UU_COMP != UU_WIN2K
	if (UW_MOTIF!=1)
		uig_prompt(2,i);
#endif
	uig_list_out("LAYER NUMBER      COUNT\n",UU_TRUE);

	num_layers = UU_LIST_LENGTH(&layer_list);
	layer_array = (int *) UU_LIST_ARRAY(&layer_list);
	entity_count_array = (int *) UU_LIST_ARRAY (&layer_entity_count_list);
	uig_insert_sort_two_arrays (num_layers, layer_array, entity_count_array);

	for (i = 0 ; i < num_layers ; i++)
	{
		layer = layer_array[i];
		num_entities = entity_count_array[i];
		sprintf (p_buff, " %4d             %d\n", layer, num_entities);
		uig_list_out(p_buff, UU_TRUE);
	}

	uig_list_out(" \n",UU_FALSE);
#if UU_COMP != UU_WIN2K
	if (UW_MOTIF!=1)
	uig_prompt(2,i);		/* pause for user respnose */
#endif
	return 0;
}
/*********************************************************************
**    I_FUNCTION     :  uig_get_directory(irec,blk)
**       Get the uig directory record irec and stuff data in structure
**       blk.
**    PARAMETERS   
**       INPUT  : 
**				irec							directory record number
**       OUTPUT :  
**          blk							 directory data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_get_directory(irec,blk)

int irec;
struct dir_rec *blk; 
{
  	int i,j,k,itemp;
	int type;											/* record type */
	int bytes;											/* num of bytes to read */
  	char buf[9],c[3];									/* temporary character buffers */
	char f_buff[170];									/* read buffer */
	int uig_rd_direct();
	int atoi();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	bytes = 160;											/* number of bytes to read */
	type = 2;														/* directory type */
	uig_rd_direct(type,irec,bytes,f_buff);		/* read ascii data */

	buf[8] = '\0';
																		/* start parsing loop */
	for(i=0;i<19;i++)
	{                                                     
																/* skip un-used fields */

		if(i==2 || i==7 || i==9 || i==15 || i==16) continue;
		for(j=i*8,k=0;j<i*8+8;k++,j++)
		{
			buf[k] = f_buff[j];    						/* transfer char's */
		}
     
		if(i != 8 || i != 17)  						/* test against charactors */
				itemp = atoi(buf);  					/* change ASCII value to integer */  
		switch(i)    				 /* store data in appropriate struc position */
		{
			case 0:
				blk->rel_type = itemp;            /* entity type number */
				break;
			case 1:
				blk->par_ptr = itemp;             /* parameter pointer */
				break;
			case 3:
				blk->line_font = itemp;           /* line font */
				break;
			case 4:
				blk->level = itemp;               /* level number */
				break;
			case 5:
				blk->view_ptr = itemp;            /* view pointer */
				break;
			case 6:
				blk->matrix_ptr = itemp;          /* matrix pointer */
				break;
			case 8:                              /* decode status fields */
				c[2] = '\0';
				for(j=0;j<8;j=j+2)
				{
					c[0] = buf[j];
					c[1] = buf[j+1];
					itemp = atoi(c);  /* change ASCII value to integer */  
					switch(j)
					{
						case 0:
							blk->blank = itemp;         /* visibility flag */
							break;
						case 2:
							blk->sub_swt = itemp;       /* use flag */
							break; 
						case 4:
							blk->use_flg = itemp;       /* use flag */
							break;
						case 6:
							blk->hier = itemp;          /* hierarchy flag */
							break;
					}
				}
				break;
			case 11:
				/* NCL - commented these lines out - gave us fat lines ...  */
				/**
				if(itemp == 0) itemp = 1;
				if(itemp > 3) itemp = 3;
				**/
				/* MPE 2.6 */
				if(itemp > 3) itemp = 4;

				blk->line_wt = (int)itemp * line_scale;   /* line weight */
				break;
			case 12:
				blk->pen_no = itemp;        /* pen number */
				break;
			case 13:
				blk->par_cnt = itemp;       /* number of parameter records */
				break;
			case 14:
				blk->form_no = itemp;       /* form number */
				break;
			case 17:
				strcpy(blk->label,buf);     /* entity label */ 
				break;
			case 18:
				blk->seq_no = itemp;       /* entity subscipt */
				break;
		}
	}
	return 0;
}
/*********************************************************************
**    I_FUNCTION     :  uig_list_start()
**       List IGES start section.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_list_start()
{
	char f_buff[80];							/* read buffer */
	int f_bytes = 80;							/* number of bytes to read */
	int type = 0;								/* record type */
	int i;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	uig_clear();										/* clear screen */
	uig_list_out("Identification Information:\n",UU_TRUE);
	for(i=1;i<sect_ptr[0]+1;i++)
	{
		uig_rd_direct(type,i,f_bytes,f_buff);  /* get next record */
		f_buff[72] = '\0';									/* terminate at col 72 */
		uig_list_out(f_buff,UU_TRUE);						/* output to the screen */
		uig_list_out("\n",UU_TRUE);
	}
	uig_list_out("  \n", UU_TRUE);
	return 0;
}
/*********************************************************************
**    I_FUNCTION     :  uig_get_global(blk)
**       Get the IGES global section information.
**    PARAMETERS   
**       INPUT  : 
**				input
**       OUTPUT :  
**          blk						structure which contains global info
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_get_global(blk)
struct global_rec *blk;
{
	char inbuf[80];							/* read buffer */
	char outbuf[80];							/* output buffer */
	char *ptrbuf;							   /* string output buffer */
	int bytes = 80;							/* number of bytes to read */
	int type = 1;								/* record type */
	int i,start,irec,outnum,strfl;
	int uig_nxtarg();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	irec = 1;
											/* set defaults */
	blk->delim = delim;
	blk->term = term;
	blk->sing_pr_int = 16;
	blk->sing_pr_exp = 8;
	blk->sing_pr_mant = 24;
	blk->doub_pr_exp = 8;
	blk->doub_pr_mant = 56;
	blk->scale = 1.0;
	blk->units = 1;
	blk->prod_id1[0] = '\0';
	blk->prod_id2[0] = '\0';
	blk->process_id[0] = '\0';
	blk->author[0] = '\0';
	blk->author_org[0] = '\0';
	blk->file_id[0] = '\0';
	blk->sys_id[0] = '\0';
	strcpy(blk->unitstr, deft_str[0]);
	strcpy(blk->datestr, deft_str[1]);
	blk->granularity = 1.0e-5;
	blk->coor_max = 100;
	unit_scale = 1.0; 		/*jkd17: internal units = inch */	
	line_scale = 1.0; 		/*jkd33: line width scale = 0.01 incl */

	uig_rd_direct(type,irec,bytes,inbuf);  /* get first record */
	outnum = start = 0;

	if(inbuf[start] == ' ')
	{
		while(inbuf[start] == ' ')start++;
	}
/*
.....vp 6/30/97 
.....set delimiters
*/
	
	uig_get_holerith(type,&irec,&start,&outnum,inbuf,&ptrbuf);
	if (outnum > 0) 
	{
		uig_stor_global(1, outnum, ptrbuf,blk);
		uu_toolfree (ptrbuf);
	}
	uig_get_holerith(type,&irec,&start,&outnum,inbuf,&ptrbuf);
	if (outnum > 0) 
	{
		uig_stor_global(2, outnum, ptrbuf,blk);
		uu_toolfree (ptrbuf);
	}
			
/*
.....vp 6/30/97 
.....this is not used anymore, see above

	if(inbuf[start+1] == 'H')
	{
		uu_dprint(UU_MTRC,(us,"start = %d", start));
		outnum = 3;
		outbuf[2] = inbuf[start+2];
		uu_dprint(UU_MTRC,(us,"output = %c", outbuf[2]));
		uig_stor_global(1, outnum, outbuf,blk);
		start+= 4;
		uig_nxt_string(type,&irec,&start,&outnum,inbuf,outbuf);
		uu_dprint(UU_MTRC,(us,"global  i = 2 str = %s", outbuf));
		uu_dprint(UU_MTRC,(us,"output = %c", outbuf[2]));
		uig_stor_global(2, outnum, outbuf, blk);
	}
	else
	{
		start += 2;
	}
*/

	for(i=3;i<23;i++)
	{
		/*jkd41: fix scanning string fields in global record */
		switch (i)
		{
			case 3:
			case 4:
			case 5:
			case 6:
			case 12:
			case 15:
			case 18:
			case 21:
			case 22:
				/* string arguments */
				uig_get_holerith(type,&irec,&start,&outnum,inbuf,&ptrbuf);
/*
.....vp 6/30/97
.....changed to uig_get_holerith which has same parameters but can process
.....any length string and H format sets its length as specified in IGES 5.0
.....nH is not in output buffer so do not call uig_strip_holerith.
.....
				uig_nxt_string(type,&irec,&start,&outnum,inbuf,outbuf);
				uig_strip_holerith(&outnum, outbuf);   
*/
				strfl = 1;
				break;
			default:
				/* arithmetic arguments */
				uig_nxt_field (type,&irec,&start,&outnum,inbuf,outbuf);
				ptrbuf = outbuf;
				strfl = 0;
				break;
		}
		if(outnum > 0)
		{ 
			uig_stor_global(i,outnum,ptrbuf,blk);
			if (strfl) uu_toolfree (ptrbuf);
		}
	}
	return 0;
}
/*********************************************************************
**    I_FUNCTION     :  uig_stor_global(indx,count,buff,blk)
**       Store global data in proper location in the structure.
**    PARAMETERS   
**       INPUT  : 
**          indx						parameter index
**				count						character count in buff
**				buff						character buffer	
**       OUTPUT :  
**          blk						global structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_stor_global(indx,count,buff,blk)
int indx;									/* parameter index */
int count;									/* character count */
char buff[];								/* character buffer */
struct global_rec *blk;
{
	UU_TRUEDOUBLE ff,uig_atof();

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	buff[count] = '\0';
	switch(indx)
	{
		case 1:									/* delimiter */
			blk->delim = buff[0];
			delim = blk->delim;
			break;
		case 2:									/* EOR delimiter */
			blk->term = buff[0];
			term = blk->term;
			break;

		/* cpp: store additional fields */

		case 3:
			strcpy(blk->prod_id1, buff);
			break;
		case 4:
			strcpy(blk->file_id, buff);
			break;
		case 5:
			strcpy(blk->sys_id, buff);
			break;
		case 6:
			strcpy(blk->process_id, buff);
			break;

		case 7:									/* bits per single precision integer */
			blk->sing_pr_int = atoi(buff);
			break;
		case 8:									/* bits per floating exponent */
			blk->sing_pr_exp = atoi(buff);
			break;
		case 9:									/* bits per floating mantissa */
			blk->sing_pr_mant = atoi(buff);
			break;
		case 10:									/* bits per double exp */
			blk->doub_pr_exp = atoi(buff);
			break;
		case 11:									/* bits per double mantissa */
			blk->doub_pr_mant = atoi(buff);
			break;
		case 12:
			strcpy(blk->prod_id2, buff);
			break;
		case 13:									/* model space scale factor */
			ff = uig_atof(buff);
			blk->scale = ff;
			break;
		case 14:									/* data units 1 = inches 2 = MM */ 
			blk->units = atoi(buff);
			switch(blk->units)
			{
				case 1:
					unit_scale = 2.54;
					unit_scale = 1.0;   			/*jkd17  */
					break;
				case 2:
					unit_scale = 0.1;
					unit_scale = 0.1 / 2.54;   		/*jkd17  */
					break;
				case 3:
					unit_scale = 2.54;
					unit_scale = 1.0;  				/*jkd17  */
					break;
				case 4:
					unit_scale = 12.0 * 2.54;
					unit_scale = 12.0;   			/*jkd17  */
					break;
				case 5:
					unit_scale = 5280.0 * 12.0 * 2.54;
					unit_scale = 5280.0 * 12.0; 	/*jkd17  */
					break;
				case 6:
					unit_scale = 100.0;
					unit_scale = 100.0 / 2.54;  	/*jkd17  */
					break;
				case 7:
					unit_scale = 100000.0;
					unit_scale = 100000.0 / 2.54;	/*jkd17  */
					break;
				case 8:
					unit_scale = 0.001 * 2.54;
					unit_scale = 0.001;   			/*jkd17  */
					break;
				case 9:
					unit_scale = 0.000001;
					unit_scale = 0.000001 / 2.54;   /*jkd17  */
					break;
				case 10:
					unit_scale = 1.0;
					unit_scale = 1.0 / 2.54;   		/*jkd17  */
					break;
				case 11:
					unit_scale = 0.000001 * 2.54;
					unit_scale = 0.000001;   		/*jkd17  */
					break;
			}
			break;
		case 15:									/* Units string  */
			strcpy(blk->unitstr,buff);
			break;
		case 16:									/*jkd33: line wt grads  */
			line_wt_grads = (int)uig_atof(buff);
			break;
		case 17:									/*jkd33: line wt grads  */
			max_line = uig_atof(buff) * unit_scale; /*jkd55: do not ignore units */
			if (line_wt_grads * max_line != 0)		/*jkd33: neither is zero */
/*
..... Removed the multiplication by 100
..... Himani
*/
				line_scale = max_line / line_wt_grads ; 
			break;
		case 18:									/* creation date  */
			strcpy(blk->datestr,buff);
			break;
		case 19:									/* model space granularity  */
			ff = uig_atof(buff);
			blk->granularity = ff;
			break;
		case 20:									/* maximum coordinate value */ 
			ff = uig_atof(buff);
			blk->coor_max = ff;
			break;
		case 21:
			strcpy(blk->author, buff);
			break;
		case 22:
			strcpy(blk->author_org, buff);
			break;
	}
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  uig_strip_holerith
**       List IGES start section.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_strip_holerith(num, buff)
int  *num;
char buff[120];							
{

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	int i, count;
	char internal_buff[120];
	count = 0;
	for(i=0; i< *num; i++)
	{
		if(buff[i] == 'H')
		{
			strcpy(internal_buff, &buff[count+1]);
			strcpy(buff, internal_buff);
			*num = *num - count+1;
			break;
		}
		count++;
	}
	return 0;
}
/*********************************************************************
**    I_FUNCTION     :  uig_get_holerith
**       Extract character string from buffer defined by hollerith format.
**       Input buffer pointer is set to the very first character after
**       last parameter delimiter and we know that this parameter is string.
**       Length of string is unlimited; deallocate memory after this
**       call if out>0.  This function replaces uig_nxt_string.
**    PARAMETERS   
**       INPUT  : 
**         type    - limits max number of character in input buffer
**         prec    - rec number in input iges file
**         in      - pointer in inbuf where string parameter begins
**         inbuf   - input buffer to analize. 
**       OUTPUT :  
**         in      - pointer in inbuf where next parameter will begin
**         out     - number of characters in extracted string
**         outbuf  - extracted string
**         inbuf   - last used input buffer, can be different if next
**                   record was read in.. 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uig_get_holerith(type,prec,in,out,inbuf,outbuf)
int  type, *prec, *in, *out;
char inbuf[120], **outbuf;							
{

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	int i, status, imax, j, k, fi, ifl;
	int locrec, bytes=80;
	char numstr[10], *tmpbuf;

	status = 0;
	i = *in;
	j = i;
	*out = 0;
	locrec = *prec;
	if (type == 1) /*jkd41*/
		imax = 72;
	else
		imax = 64;
/*
.....find first none space character in buffer
*/
	k = 0;
loop:
	while (inbuf[i] == ' ' && i < imax) i++;
	ifl = 0;
/*
.....read in next record if buffer processed
*/
nextrec:
	if (i == imax || j == imax)
	{
		locrec++;
		uig_rd_direct(type, locrec, bytes, inbuf);
		i = j = 0;
		if (ifl == 0) goto loop;
	}
/*
.....get hollerith mark and string length
*/
scanner:
	ifl = 1;
	for (j=i; j<imax; j++)
	{
		if (inbuf[j] == 'H') 
		{
			if (k == 10) goto failed;
			numstr[k] = '\0';
			fi = atoi (numstr);
			goto getholl;
		}
		else
		{
			if (inbuf[j] == delim || inbuf[j] == term)
				goto exit;
			else if (inbuf[j] != ' ')
			{
				if (k == 10) goto failed;
				numstr[k] = inbuf[j];
				k++;
			}
		}
	}	
	if (j == imax) goto nextrec;
/*
.....found H - check string length
*/
getholl:
	if (fi > 32767) goto failed;
	tmpbuf = (char *) uu_toolmalloc ((fi+k+2));
/*
.....added H specifier to be compatible with old function uig_nxt_string.
.....
.....Remove this in future release and adjust following routines to not 
.....process H specifier: 
..........tigmapg.c - create_label lines ~ 329 (see prec.name),
..........in tigmaps.c - uig_sym_name lines ~ 332,
..........in tigmapd.c - uig_map_lindim ~147, uig_map_gnote ~518,
..........uig_map_diadim ~665, uig_map_radim ~960 uig_map_ang_dim ~1396,
..........uig_map_label ~1875 -> do not need check string length and start
..........pointer to remove H specifier since it is not in string!!!!!
*/
	if (type == 3)
	{
		strncpy (tmpbuf,numstr,k);
		tmpbuf[k++] = 'H';
	}
	else k = 0;
/*
.....get string
*/
	j++;
	for (i=0; i<fi; i++)
	{
		if (j == imax)
		{
			locrec++;
			uig_rd_direct(type, locrec, bytes, inbuf);
			j = 0;
		}
		tmpbuf[i+k] = inbuf[j++];
	}
	tmpbuf[fi+k] = '\0';
	*outbuf = tmpbuf;
	*out = fi + k;
	k  = 0;
/*
.....go to start of process to find end of parameter mark
*/
	i    = j;
	goto loop;
/*
.....Syntax error in header
*/
failed:;
	sprintf(p_buff,"Character string in the Global block is formatted incorrectly.\n");
	uig_list_out(p_buff,UU_TRUE);
	goto exit;

exit:
	if (j < imax && inbuf[j] == term) status = 1;
	*in = j + 1;
	*prec = locrec;
	return (status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_insert_sort_two_arrays
**       Sort two associated arrays at the same time.
**       Sorts an array arr[0..n-1] into ascending order using insert
**       sort, while making the corresponding rearrangement of the
**       array brr[0..n-1].
**
**       This is a very inefficient algorithm (order n^2).  It should
**       be Quicksort; however, I am too lazy to do it now.
**       This was stolen, verbatim, from "Numerical Recipes in C" by
**       Press, Teukolsky, Vetterling, and Flannery.
**    PARAMETERS
**       INPUT  :
**         n       - number of elements in the array
**         arr     - principle array (both arrays will be sorted in
**                   order of this array)
**                   (if arr[i] -> arr[j] then brr[i] -> brr[j] also)
**         brr     - associated array
**       OUTPUT :
**         arr     - sorted array
**         brr     - associated array with corresponding rearrangement
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_insert_sort_two_arrays(n, arr, brr)
int n, arr[], brr[];
{
	int i, j, a, b;

	for (j = 1; j < n; j++)
	{
/*
..... Pick out each element in turn.
*/
		a = arr[j];
		b = brr[j];
		i = j - 1;
/*
..... Look for the place to insert it.
*/
		while ((i >= 0) && (arr[i] > a))
		{
			arr[i+1] = arr[i];
			brr[i+1] = brr[i];
			i--;
		}
/*
..... Insert it.
*/
		arr[i+1] = a;
		brr[i+1] = b;
	}
}

/*********************************************************************
**    E_FUNCTION     :  uig_get_unistat(gblk,unistat)
**       Return the appropriate IGES file Header parameters in the
**       Unibase Statistice Record format.
**    PARAMETERS
**       INPUT  :
**         gblk    - Global record from IGES file.
**       OUTPUT :
**         unistat - Unibase Statistics record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uig_get_unistat(gblk,unistat)
struct global_rec *gblk;
struct UR_unistat_rec *unistat;
{
	int nc,stat,s,t,m,d,y,hh,mm,ss;
/*
.....Get Unibase Statistics Record
*/
	stat = ur_retrieve_unibase_stat(unistat);
	if (stat != UU_SUCCESS)
	{
		ur_init_unibase_stat(UR_STAT_INIT);
		stat = ur_retrieve_unibase_stat(unistat);
		if (stat != UU_SUCCESS) return;
	}
/*
.....Copy appropriate fields
*/
	strcpy(unistat->system,gblk->sys_id);
	strcpy(unistat->processor,gblk->process_id);
	strcpy(unistat->author,gblk->author);
	strcpy(unistat->company,gblk->author_org);
	strcpy(unistat->fname,gblk->file_id);
	nc = sscanf(gblk->datestr,"%d.%d",&s,&t);
	if (nc == 2 && s > 9999 && t > 9999)
	{
		y = s / 10000;
		s = s - y*10000;
		if (y < 70) y += 2000; else if (y < 1900) y += 1900;
		m = s / 100; d = s - m*100;
		hh = t / 10000; t -= hh*10000;
		mm = t / 100; ss = t - mm*100;
		ul_format_date(m,d,y,hh,mm,ss,unistat->date);
	}
	else
		strcpy(unistat->date,gblk->datestr);
	sprintf(unistat->translator,"NCL/IGES V%.3f",NCL_version);
	nc = strlen(gblk->prod_id1);
	if (nc > 0)
	{
		unistat->notes = uu_malloc(sizeof(char)*nc+1);
		strcpy(unistat->notes,gblk->prod_id1);
		unistat->no_notes = nc;
	}
}
