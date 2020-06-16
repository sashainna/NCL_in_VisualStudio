/*********************************************************************
**    NAME         :  tioconv.c
**       CONTAINS:
**    		int uio_put_para(ent_type,c,fd2,pcount,dcount)
**    		uio_ddl_field(type,c,next,cnt,rtn,fd2,bind,pcount,dcount,isjoint)
**    		uio_convert(c,type,fd2,buf,bind,pcount,dcount)
**    		uio_num_ptr(c)
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tioconv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:52
*********************************************************************/
#if (UU_COMP==UU_WIN2K)
#include <string.h>
#define index strchr
#include "usdbldef.h"
/*
.....no definition of those vars
.....but it will use it when link with ncl lib
.....Yurong 3/14/00
*/
UU_LOGICAL	UR_ason;
int			UR_asfile;
#endif

#include	"stdio.h"
#include "tiges.h"
#include "riddldef.h"
#include	"udebug.h"


/*********************************************************************
**    I_FUNCTION     :  int uio_put_para(ent_type,c,fd2,pcount,dcount)
**          Pass an iges entity record, use the data dictionary to 
**				find out the field type to write to a file
**    PARAMETERS   
**       INPUT  : 
**          ent_type                entity type
**          c                  an iges record
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uio_put_para(ent_type,c,fd2,pcount,dcount)
int ent_type;
char c[];
int	fd2;
int	*pcount;
int	dcount;
	
{
	int	cnt1;
	struct attr_def rtn1[20];
	char	p_buf[81], buf[81];
	int	dummy;
	int	bind, next;
	int	i;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	bind = 0;
	next = 0;
				/* find relation number */

	for(i=0;i<IG_NUM;i++)
	{
		if(ent_type == iges_type[i])
		{
			cnt1 = uig_data_dict(rtn1,20,ddl_nam[i],&dummy);
			if(cnt1 == 0)
			{
				sprintf(buf,"Unknown relation type %s\n",ddl_nam[i]);
				uig_error(buf);
				return(99);
			}
			uio_ddl_field(ent_type,c,&next,p_buf,cnt1,rtn1,fd2,
															&bind,pcount,dcount,0);
			uio_sec_term(fd2,64,p_buf,bind,pcount,"P",dcount);
		}
	}
	return(0);
}


/*********************************************************************
**    I_FUNCTION :  uio_ddl_field(type,c,next,cnt,rtn,fd2,bind
**																	,pcount,dcount,isjoint)
**       according to the data dictionary, get the iges data from the 
**			the data bag.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_ddl_field(type,c,next,p_buf,cnt,rtn,fd2,bind,pcount,dcount,isjoint)
int	type;			/* entity type */
char	c[], p_buf[];
int	*next;	/* index to the 'c' buffer to get data from */
int	cnt;		/* number of structure fields */
struct attr_def	rtn[];	/* data dictionary information  */
int	fd2;
int	*bind, *pcount, dcount;
int	isjoint;			/* is this an joint field? */
	
{
	int  cnt2;
	struct attr_def rtn2[20];
	char	buf[81], str[100];
	char	**addr;
	int	ptr,nptr;
	int	j, l;
	int	dummy;
	int	num, k;
	int	jcnt, joinum;
	int 	start;
	UU_LOGICAL	first, polys, no_inner_cvs;

/*------------------------------------------------------------------------
** Start of executable code
**----------------------------------------------------------------------*/
	joinum = 0;
	first = UU_TRUE;
	no_inner_cvs = UU_FALSE;
/*
.....vp 12/10/96 poly flag set for type 3 or 6 only, regular GPOLY (2D)
.....has its own field (this.num) with number of 2D points and is not output
.....as joint reference.
*/
	polys = (type == GPOLY3D || type == GPOLY6D || type == DRAW);
	for(j=isjoint;j<cnt;j++)              /* process fields in dictionary */
	{
		switch(rtn[j].attr_type)     /* switch on type */
		{
			case FLOAT:
			case DOUBLE:
			case REAL:
			case INT:
				num = rtn[j].num_rows * rtn[j].num_cols;
				start = *next;
				for(k=0;k<num;k++)
				{
					uio_convert(&c[*next],rtn[j].attr_type,fd2,p_buf,
																	bind,pcount,dcount);
					*next = *next + rtn[j].attr_size;
				}
				if (type == GTRIMSRF && j == 3)
				{
					no_inner_cvs = (int)c[start] == 0;
				}
				*next = start + rtn[j].attr_off;
			break;

			case CHARACTER:
				num = rtn[j].num_rows * rtn[j].num_cols;
				for (l=0;l<num;l++)
					str[l] = c[*next + l];
				str[l] = '\0';
				uio_pack_str(fd2,64,p_buf,bind,pcount,str,"P",dcount);
				*next = *next + rtn[j].attr_off;	/* get to the next field */
				break;

			case KEY_ID:
				uio_convert(&c[*next],rtn[j].attr_type,fd2,p_buf,bind,pcount,dcount);
				*next = *next + rtn[j].attr_off;
				if ((first)&&(!isjoint))
				{
					*next = *next + sizeof(int);
					first = UU_FALSE;
				}
				break;
/* 
.....special case of a join field - must process
.....the fields in the join table
*/
			case JOIN:
				joinum++;
				nptr = *next + sizeof(char *);
				if ((jcnt=uio_num_ptr(&c[nptr])) == 0)		/* no back pointer */
				{
/*
.....If a trimmed sf has no inner curves, don't output a zero or
.....names from the property will not be used by igesin.
*/
					if (j != 5 || !no_inner_cvs)
					{
						sprintf(str, "%d", 0);
						uio_pack_num(fd2,64,p_buf,bind,pcount,str,"P",dcount);
					}
					*next = *next + sizeof(int)*2 + sizeof(char *);	
				}
				else				
/*
.....process the joint structure 
.....vp 11/26/96 process 'no_prop' same way as GPOLYx i.e. output if >0 dispite
.....of number of join is not supported in general.  NOTE: we assume that all
.....geometry entries have property pointer specified as the last field.
*/
				{
					if(polys || j == cnt-1)
					{
						sprintf(str, "%d", jcnt);
						uio_pack_num(fd2,64,p_buf,bind,pcount,str,"P",dcount);
					}
/*					*next = *next + sizeof(int); */
					addr = (char **) &c[*next];		/* find the new address */
					cnt2 = uig_data_dict(rtn2,20,rtn[j].attr_name,&dummy);
					if (cnt2 == 0)
					{
						sprintf(buf,"Unknown relation type %s\n",ddl_nam[j]);
						uig_error(buf);
						return(99);
					}

					ptr = 0;
					do
					{
						uio_ddl_field(type,*addr,&ptr,p_buf,cnt2,rtn2,fd2,
										bind,pcount,dcount,1);
					}	while ((--jcnt) > 0);
					*next = *next + sizeof(int)*2 + sizeof(char *);	
				}
			break;

			default:
				uig_error("Unknown field type");
				break;
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION :  uio_convert(c,type,fd2,buf,bind,pcount,dcount)
**       According to the type given, extract the information in the
**			character array into a string, then write to the iges out file.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_convert(c,type,fd2,buf,bind,pcount,dcount)
int	*c;
int	type;
int	fd2;
char	*buf;					/* temporary buffer */
int	*bind;				/* index to the buf */
int	*pcount, dcount;	/* parameter and directory section counter */
	
{
	char	str[80];

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/
	switch (type)
	{
		case	KEY_ID:
			sprintf(str, "%d", *((long *)c));
			break;

		case	INT:
			sprintf(str, "%d", *((int *)c));
			break;

		case FLOAT:
			sprintf(str, "%-1.7e", *((float *)c));   
/*			sprintf(str, "%-1.8g", *((float *)c));   */
			cnv_format (str);
			break;

		case REAL: case DOUBLE:
/*
.....vp 11/27/96 incresed precision to 10 digits under influence
.....of Unigraphics iges file with 15 digits!!!
*/
			sprintf(str, "%-1.10G", *((double *)c));   
/*			sprintf(str, "%-1.8g", *((double *)c)); */
			cnv_format (str);
			break;
	}
	uio_pack_num(fd2,64,buf,bind,pcount,str,"P",dcount);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION :  cnv_format (str)
**       extract an integer number from a character string
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int cnv_format (str)
char str[80];
{
	char exp[10];
	char *ix,*ie;
	ix = str;
	if (*ix == '-') ix++;
	ul_to_upper (ix);
	if (*ix == 'N' || *ix == '?' || *ix == 'I')
	{
		strcpy (str, "0.0");
	}
	else
	{
		ix = index (str,'.');
		if (ix==0) 
		{
			ie = index (str,'e');
/*
.....vp 12/10/96 make sure that E is also valid exponent mark
*/
			if (ie==0) ie = index (str,'E');
			if (ie==0)
				strcat (str,".0");
			else
			{
				strcpy (exp,".0");
				strcat (exp,ie);
				strcpy (ie,exp);
			}
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION :  uio_num_ptr(c)
**       extract an integer number from a character string
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uio_num_ptr(c)
int	*c;

{
	char	str[80];
	int	num;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	sprintf(str, "%d", *c);
	sscanf(str, "%d", &num);
	return(num);
}
