
/*********************************************************************
**    NAME         :  tlangio.c
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d5lgio.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:11
*********************************************************************/

/* file langio.c -- I/O routines for langpak */

#include "usysdef.h"
#include "ustdio.h"
#include "uctype.h"

#define CHECK 1

FILE *l_fd;
int fromfile=UU_FALSE;  
int fromtool=UU_FALSE;

/*********************************************************************
**    I_FUNCTION :  uti_tread()
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uti_tread(in,kf,len)		/* read terminal */
							/* Recognize %filename at beginning of line */
							/*	and change to the specified file for input*/
int in[];				/* array to receive data */
int kf;					/* index of where to start placing data*/
int *len;				/* on return, length of data read in */
{
	char *fgets();
	char  ibuf[600];
	char *p;
	int i,j;
	int gotlin;
	int ix;

	gotlin=0;
	*len = 0;
	while (gotlin==0)
	{
		if (fromfile==UU_TRUE)
		{
			p=fgets(&ibuf[*len],133,l_fd);
			if (p==NULL)				/* end of file */
			{
				fclose(l_fd);
				fromfile=UU_FALSE;
			}
			else 							/* not end of file */
			{
				i = *len;
				*len = strlen(ibuf);
				ibuf[--(*len)]='\0';		/* overwrite the newline with null*/
				printf("%s\n",&ibuf[i]);	/* echo file input */
			}
		}									/* if (fromfile==UU_TRUE) */ 
		if (fromfile==UU_FALSE)
		{
			if (fromtool)
			  {
            ibuf[*len] = 's';
            ibuf[*len+1] = '\0';
           }
         else
           {
				printf(": ");
				/* gets(&ibuf[*len]);	*/
				if ((gets(&ibuf[*len]))==NULL)
					ibuf[*len] = '\0';
           }
			*len = strlen (ibuf);
#ifdef debug
	printf("uti_tread got: %s\n",ibuf);
#endif
		}
		if (ibuf[*len-1] != '\\')
			gotlin=1;
		else		*len = *len - 1;
		if (ibuf[0]=='%') 
		{
			fromfile=UU_TRUE;
			l_fd=fopen(&ibuf[1],"r");
			*len = 0;
			gotlin=0;
		}
	}						/* end while */
	while (ibuf[*len-1] == ' ')		/* delete all the end blanks */
		(*len)--;
	for(ix=0;ix<*len;ix++)	{			/* copy to return buffer */
		in[ix+kf]=ibuf[ix];
		if (in[ix+kf]<' ') in[ix+kf]=' ';	/* change ctrl chars to blanks */
	}
	ibuf[*len] = '\0';					/* and terminate it */
}

/*********************************************************************
**    I_FUNCTION :  uti_twrite()
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uti_twrite(inbuf,kf,len)		/* write to terminal the data in inbuf. */
int inbuf[];				/* buffer containing data to write */
int kf;						/* beginning index of 1st elt of inbuf */
int len;					/* number characters to write */
{
	int i;
	char ch;
#ifdef CHECK
	if ((kf<1)||(kf>20000)||(len<1)||(len>20000)) {
		printf("uti_twrite ERROR. kf=%d, len=%d\n",kf,len);
		return;
	}
#endif
#ifdef debug
	printf("uti_twrite(inbuf,%d,%d)\n",kf,len);
#endif
	for (i=0; i< len; i++) 
	{
		ch=inbuf[kf+i];
		printf("%c",ch);
	}
	printf("\n");
}

/*********************************************************************
**    I_FUNCTION :  uti_lxrite()
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uti_lxrite(ln,len,lx)			/* write lexicon */
int  ln[];					/* lexicon file name */
int len;						/* length of lexicon file name */
int lx[];					/* array contains data to write into lexicon */
{
	char lnname[40];		/* will hold lexicon filename */
	int i,lxlen,fd;
	char msg[256];
	/* move lexicon filename in */
	for (i=1; i<= len; i++)
	{
		lnname[i-1]=ln[i];
	}
	lnname[len]='\0';		/* null string terminator */
	lxlen=lx[3];			/* number of words of lx to write */
/*
	printf("%d,",lx[0]);
	for (i=1; i<lxlen; i++)
		printf(((i%15)!=0)? "%4d,":"%4d\n",lx[i]);
	printf("\n");
*/
	fd=creat(lnname,0644);	/* open file */
	i=write(fd,&lxlen,sizeof(int));	/* write length(in words) */
	if (i!=sizeof(int))
	{
		sprintf (msg, "uti_lxrite(ln,%d,%s).  error writing length. %d\n",
						lxlen,lnname,i);
		ud_printmsg(msg);
	}
	i=write(fd,&lx[1],lxlen*sizeof(int));	/* write lxlen words */
	if (i!= lxlen*sizeof(int)) 
	{
		sprintf(msg, " uti_lxrite(ln,%d,%s) error writing data. %d\n",
						lxlen,lnname,i);
		ud_printmsg(msg);
	}
	close(fd);
	sprintf(msg, " lexicon file %s written. Length %d words\n",lnname,lxlen);
	ud_printmsg(msg);
}

/*********************************************************************
**    I_FUNCTION :  uti_lxread()
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uti_lxread(ln,len,lx,lflag)		/* read lexicon file */
int ln[];						/* lexicon file name */
int len;							/* length of filename */
int lx[];						/* array to contain the lexicon */
int *lflag;						/* returned 1 if no errors. else 0 */
{
	int i,fd,j;
	char lnname[40], msg[256];
	/* move lexicon name in */
	for (i=1; i<= len; i++)
	{
		lnname[i-1]=ln[i];
	}
	lnname[len]='\0';		/* null string terminator */
	fd=open(lnname,0);
	if (fd<0)
	{
		sprintf(msg, "uti_lxread cant open lexicon file %s\n",lnname);
		ud_printmsg(msg);
		*lflag=0; return;
	}
	i=read(fd,&j,sizeof(int));	/* read length of file */
	if (i!=sizeof(int))
	{
		sprintf(msg, "uti_lxread(%s,len,lx,lflag) cant read file length. i=%d\n",lnname,i);
		ud_printmsg(msg);
		*lflag=0; return;
	}
	i=read(fd,&lx[1],j*sizeof(int));	/* read lexicon */
	if (i!=j*sizeof(int))
	{
		sprintf(msg, "uti_lxread(%s) cant read lexicon data. length=%d\n",lnname,j);
		ud_printmsg(msg);
		*lflag=0; return;
	}
	close(fd);
	sprintf(msg, "read lexicon file %s of length %d words.\n",lnname,j);
	ud_printmsg(msg);
	*lflag=1;
}
