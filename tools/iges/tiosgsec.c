/*********************************************************************
**    NAME         :  tiosgsec.c
**       CONTAINS:
**			  uio_start_sec
**			  uio_global_sec
**			  uio_pack_str
**			  uio_pack_num
**			  uio_numlen
**			  uio_sec_term
**			  uio_set_factor
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tiosgsec.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:54
*********************************************************************/

#include		"ustdio.h"
#include		"usysdef.h"
#include		"udebug.h"
#include		"tioglb.h"
#include		"tiges.h"
#include		"tioconv.h"
#include		"nclver.h"
#include		"xenv1.h"
#if (UU_COMP!=UU_WIN2K)
#include		"tigmf.h"
#endif

char	us[132];
/*#define IGES_VERSION "2.0"*/  /*vp80392*/

extern UM_int2 NCL_ubas_unit;
/*********************************************************************
**    I_FUNCTION :  uio_start_sec(iges_fd1,scount)
**			Get the start section information, pack it with the necessary
**			start section information into the file.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_start_sec(iges_fd1,scount)		
	int	iges_fd1;
	int	*scount;			/* start section sequence count */
	
	{
	char	buf[81];
	char	longbuf[1000];
	char	inbuf[81];
	int	count, linenum;
	int	len, i, j, k;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
	*scount = 1;
	longbuf[0] = '\0';
	if (Iges_batch==0)
	{
#if (UU_COMP!=UU_WIN2K)
		uig_mfprompt(NULL, "INPUT", "Enter the identification information (less than 72 characters a line): ", 3, 72, longbuf); 
#else
		iges_wntprompt(NULL, "INPUT", "Enter the identification information \r\n(less than 72 characters a line): ", 3, 72, longbuf);
#endif
	}
/*
......if batch run, use filename as identification information
*/
	else
		strcpy(longbuf, iges_outfile);
	len = strlen(longbuf)+1;
	for (i=0, k=0 ; i<len; i++, k++)
	{
		if ((longbuf[i]=='\n')||(i==(len-1)))
		{
			for (j=k; j<81; j++)
				inbuf[j] = ' '; 
	   		sprintf(&inbuf[72], "%s%7d", "S", (*scount)++);
			inbuf[80] = '\0';
			uio_write(iges_fd1,inbuf,80*sizeof(char));
			k = -1;
		}
		else
			inbuf[k] = longbuf[i];
	}
	return;
#if (UU_COMP!=UU_WIN2K)
  	uig_str_out("Enter the identification information (less than 72 characters a line)\n: ",UU_TRUE);
	while ((len=uig_line_in(inbuf)) > 0 )
	  {
		for (i=len; i<81; i++)
			inbuf[i] = ' ';			/* clear all the non character space */
	   sprintf(&inbuf[72], "%s%7d", "S", (*scount)++);
	   inbuf[80] = '\0';
	   uio_write(iges_fd1,inbuf,80*sizeof(char));
#if UU_COMP==UU_VAXVMS
  	   uig_str_out("\nnext line (hit return key to stop) ...\n",UU_TRUE);
#else
  	   uig_str_out("next line (hit return key to stop) ...\n",UU_TRUE);
#endif
	  }
/*
	uio_sec_term(iges_fd1,72,inbuf,0,scount,"S",0);
*/
	uu_dexit;
#endif
	}	

/*********************************************************************
**    I_FUNCTION :  uio_global_sec(iges_fd1,fname,gcount)		
**			Most of the global section informations are set in the tioglb.h.
**       This routine gets them and packs them into the iges output file.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_global_sec(iges_fd1,fname,gcount)	
	int	iges_fd1;
	char	fname[];				/* iges out file name */
	int	*gcount;				/* global section sequence count */
	
	{
	int	ind, iunit;
	char	buf[81];
	char	sdatime[80],idbuf[80], unitbuf[5], unitflg[2];
   char *punit, *ux_getenv();

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
/*
...Set default to INCH
*/
   strcpy(unitbuf,"INCH");
   iunit = 1;
   strcpy(unitflg,"1");
/*
.....Neet to make sure that the proper choice is displayed in the 
.....header of the file, if output_units is equal to 1 then we are
.....outputting in MM otherwise we are outputting in INCHES.
.....JLS 1/19/99
*/
	if(output_units==1)
	{
   	strcpy(unitbuf,"MM");
   	iunit = 2;
   	strcpy(unitflg,"2");
	}

/*
...Check environtment variable U_UNITS first
*/
   if ((punit=ux_getenv("U_UNITS")) != UU_NULL)
     {
      if (strcmp(punit,"MM") == 0)
        {
         strcpy(unitflg,"2");
         iunit = 2;
        }
      else
        {
         punit = unitbuf;
        }  
     }
   else
/*
...U_UNITS not specified, check if unibase file can set units 
*/
     {
      if (NCL_ubas_unit == 1&&output_units==1)
        {
         strcpy(unitbuf,"MM");
         strcpy(unitflg,"2");
         iunit = 2;
        }
      punit = unitbuf;
     }

#define  SP_INT_BITS    "16"
#define  SP_MAX_EXP     "38"
#define  SP_MAX_PREC    "7"
#define  DP_MAX_EXP     "308"
#define  DP_MAX_PREC    "15"

	ind = 0;
	*gcount = 1;				/* global record count */
	buf[0] = '\0';
	uio_pack_str(iges_fd1,72,buf,&ind,gcount,DELIMITER,"G",0);			/*1*/
	uio_pack_str(iges_fd1,72,buf,&ind,gcount,END_DELIMITER,"G",0); 	/*2*/
	uio_pack_str(iges_fd1,72,buf,&ind,gcount,PRODUCT_ID,"G",0);	   	/*3*/
	uio_pack_str(iges_fd1,72,buf,&ind,gcount,fname,"G",0);				/*4*/
	uio_pack_str(iges_fd1,72,buf,&ind,gcount,SYSTEM_INFOR,"G",0);		/*5*/
	sprintf (idbuf,"%s %.3f",IGES_VERSION,NCL_version);
/*uio_pack_str(iges_fd1,72,buf,&ind,gcount,IGES_VERSION,"G",0); */   /*6*/
   uio_pack_str(iges_fd1,72,buf,&ind,gcount,idbuf,"G",0);            /*6*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,SP_INT_BITS,"G",0);		/*7*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,SP_MAX_EXP,"G",0);       /*8*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,SP_MAX_PREC,"G",0);      /*9*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,DP_MAX_EXP,"G",0);       /*10*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,DP_MAX_PREC,"G",0);      /*11*/
	uio_pack_str(iges_fd1,72,buf,&ind,gcount,TO_PRODUCT_ID,"G",0);	/*12*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,SCALE,"G",0);				/*13*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,unitflg,"G",0);			/*14*/
	uio_pack_str(iges_fd1,72,buf,&ind,gcount,punit,"G",0);				/*15*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,LINE_WEIGHT,"G",0);		/*16*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,LINE_WIDTH,"G",0);		/*17*/
	uig_time(sdatime);					/* find the current local time */
	uio_pack_str(iges_fd1,72,buf,&ind,gcount,sdatime,"G",0);			/*18*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,GRANULARITY,"G",0);		/*19*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,COOR_MAX,"G",0);			/*20*/
	uio_pack_str(iges_fd1,72,buf,&ind,gcount,AUTHOR,"G",0);				/*21*/
	uio_pack_str(iges_fd1,72,buf,&ind,gcount,ORGANIZATION,"G",0);		/*22*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,ANSI_STD_VERSION,"G",0); /*23*/
	uio_pack_num(iges_fd1,72,buf,&ind,gcount,DRAFTING_VERSION,"G",0); /*24*/
	uio_sec_term(iges_fd1,72,buf,ind,gcount,"G",0);	
	uio_set_factor(iunit);
	}

/*********************************************************************
**    I_FUNCTION :  uio_pack_str(fd1,size,buf,ind,gcount,str,sect,dind)
**       Given a string with a string type, pack the string into a buffer,
**			which is then written to the output file when it is full.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_pack_str(fd1,size,buf,ind,gcount,str,sect,dind)
	int	fd1;
	int	size;				/* max size of buffer to contain the data */
	char	buf[];			/* buffer to fill in the string */
	int	*ind;				/* index to the buffer */
	int	*gcount;			/* number of records in the current section */
	char	*str;				/* string to be written to the buffer */
	char	*sect;			/* symbols for the current processing section */
	int	dind;				/* pointer to the directory - for parameter
									section use only */
	
	{
	int	i, len, lensum;
	int	ptr, tmpind;
	char	tmpbuf[300];
	char	ch;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
	len = strlen(str);
	if (len == 0)			/* a empty string, put only a comma */
	  {
		if (*ind < size)
			sprintf(&buf[(*ind)++],",");
		else
		  {
			if (size == 64)		/* a parameter record */
				sprintf(&buf[64], " %7d", dind);		/* pointer to the directory */
			sprintf(&buf[72],"%s%7d", sect, (*gcount)++);
   			uio_write(fd1,buf,80*sizeof(char));
			*ind = 0;
			buf[(*ind)++] = ',';
		  }
	  }
	else			/* not a empty string, character count has to be added */
	  {
		lensum = len + uio_numlen(len) + 2;
		if (((*ind)+lensum) <= size)
		  {
			sprintf(&buf[*ind],"%dH%s,", len, str);
			*ind = *ind + lensum;
			if (*ind == size)
			  {
				for (i=*ind; i<81; i++)
					buf[i] = ' ';
				if (size == 64)		/* a parameter record */
					sprintf(&buf[64], " %7d", dind);	/* pointer to the directory */
			   sprintf(&buf[72],"%s%7d", sect, (*gcount)++);
   				uio_write(fd1,buf,80*sizeof(char));
				buf[80] ='\0';
				*ind = 0;
			  }
		  }
		else		/* can't fit the string into one line */
		  {
			sprintf(tmpbuf,"%dH%s,",len,str);
			ptr = tmpind = 0;
			do 
			{
			 tmpind = size - *ind + tmpind;
			 ch = tmpbuf[tmpind];
			 tmpbuf[tmpind] = '\0';
			 sprintf(&buf[*ind],"%s",&tmpbuf[ptr]);
				for (i=strlen(buf); i<81; i++)
					buf[i] = ' ';
			 if (size == 64)		/* a parameter record */
				sprintf(&buf[64], " %7d", dind);	/* pointer to the directory */
			 sprintf(&buf[72],"%s%7d", sect,(*gcount)++);
			 buf[80] = '\0';
			uio_write(fd1,buf,80*sizeof(char));
			 tmpbuf[tmpind] = ch;
			 ptr = tmpind;
			 *ind = 0;
			} while ((lensum-ptr)>size);
			sprintf(&buf[*ind],"%s",&tmpbuf[ptr]);
			*ind = *ind + (lensum-ptr);
		  }
	  }
	}

/*********************************************************************
**    I_FUNCTION :  uio_pack_num(fd1,size,buf,ind,gcount,str,sect,dind)
**       Given a numerial string packit into a character buffer, if the
**			buffer is full write to the output file.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_pack_num(fd1,size,buf,ind,gcount,str,sect,dind)
	int	fd1;
	int	size;				/* max size of buffer to contain the data */
	char	buf[];			/* buffer to fill in the string */
	int	*ind;				/* index to the buffer */
	int	*gcount;			/* number of records in the current section */
	char	*str;				/* string to be written to the buffer */
	char	*sect;			/* symbols for the current processing section */
	int	dind;				/* pointer to the directory - for parameter
									section only */
	
	{
	int	len, i;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
	len = strlen(str);
	if (len == 0)			/* a empty string, put only a comma */
	  {
		if (*ind < size)
			sprintf(&buf[(*ind)++],",");
		else
		  {
			for (i=*ind; i<81; i++)
				buf[i] = ' ';
			if (size == 64)		/* a parameter record */
				sprintf(&buf[64], " %7d", dind);	/* pointer to the directory */
			sprintf(&buf[72],"%s%7d", sect, (*gcount)++);
   			uio_write(fd1,buf,80*sizeof(char));
			buf[80] = '\0';
			*ind = 0;
			buf[(*ind)++] = ',';
		  }
	  }
	 else			/* not a empty string, character count has to be added */
	  {
		len = len + 1;
		if (((*ind)+len) > size)	/* buffer is not big enough to hold the whole
												string, write to the second line */
		  {
			for (i = *ind; i<81; i++)
				buf[i] = ' ';
			if (size == 64)		/* a parameter record */
			 {
				sprintf(&buf[64], " %7d", dind);	/* pointer to the directory */
			}				 
			sprintf(&buf[72],"%s%7d", sect, (*gcount)++);
			buf[80] = '\0';
   			uio_write(fd1,buf,80*sizeof(char));
			*ind = 0;
		  }
		sprintf(&buf[*ind],"%s,",str);
		*ind = *ind + len;
		if (*ind == size)
		  {
			for (i = strlen(buf); i<81; i++)
				buf[i] = ' ';
			if (size == 64)		/* a parameter record */
			  {
				sprintf(&buf[64], " %7d", dind);	/* pointer to the directory */
			}
			sprintf(&buf[72],"%s%7d", sect, (*gcount)++);
  			uio_write(fd1,buf,80*sizeof(char));
			buf[80] = '\0';
			*ind = 0;
		  }
	  }
	}

/*********************************************************************
**    I_FUNCTION :  uio_numlen(num)
**       Given an integer, return the number of digits
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_numlen(num)
	int	num;

	{
	int	i, k;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	i = 1;
	k = num;
	while ((k=k/10) != 0)
		i++;
	return(i);
	}

/*********************************************************************
**    I_FUNCTION :  uio_sec_term(fd,size,buf,ind,sect_count,sect,dind)
**       Write the section terminal deliminator out to the file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_sec_term(fd,size,buf,ind,sect_count,sect,dind)
	int	fd;
	int	size;				/* available size of buffer for data */
	char	buf[], sect[];
	int	ind, *sect_count;
	int	dind;				/* pointer to the corresponding direstory,
									-- for parameter directory use only */
	
	{
	int	i;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
	if (ind > size)
  		uig_error("ERROR: write size error\n: ");
	else 
	  {
		if (ind == size)
		  {
			for (i = strlen(buf); i<81; i++)
				buf[i] = ' ';
			if (size == 64)		/* a parameter record */
				sprintf(&buf[64], " %7d", dind);		/* pointer to the directory */
			sprintf(&buf[72],"%s%7d", sect, (*sect_count)++);
			buf[80] = '\0';
  			uio_write(fd,buf,80*sizeof(char));
			ind = 0;
		  }
		if(buf[ind-1] == ',') 
			sprintf(&buf[ind-1], "%s", ";");
		else
			sprintf(&buf[ind++],"%s", ";");
		for (i=ind; i<80; i++)
			buf[i] = ' ';
		if (size == 64)		/* a parameter record */
			sprintf(&buf[64], " %7d", dind);		/* pointer to the directory */
		sprintf(&buf[72],"%s%7d",sect,(*sect_count)++);
		uio_write(fd,buf,80*sizeof(char));
	  }
	}
		  
/*********************************************************************
**    I_FUNCTION :  uio_set_factor()
**       Set the conversion factor between unicad system and iges out-
**			put file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_set_factor(num)
	int	num;
	{

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/
/*
.....If output_units is equal to 0 then user wants the iges file to be
.....in INCHES, if it is not equal to 0 then the user wants the iges
.....file to be in MM. JLS 1/19/99
*/

	switch (num)
		{
		 case 1:			/* inches */
			if (output_units==0)
				uio_units = 1.0;
			else
				uio_units = 1.0/25.4;
			break;
		 case 2:			/* milimeters */
			if (output_units==0)
				uio_units = 1.0;
			else
				uio_units = 1.0/25.4;
			break;
		 default:		
  			uig_error("ERROR - unit conversion not yet implemented");
			break;
		}
	}	
