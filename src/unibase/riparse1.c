#define MODULEDBG 0
/*********************************************************************
**    NAME         :  riparse1.c
**       CONTAINS:
**       ur_parse1()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       riparse1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:46
*********************************************************************/

#if MODULEDBG != 0
#include "udebug.h"
#endif
#include "ritokdef.h"
#include "riddldef.h"
#include "reddlerr.h"

#define	STRLEN	32

/*********************************************************************
**    I_FUNCTION     :  int ur_parse1(fd, attrbuf, buflimit, tblname, tbltype)
**       parse a single table def. from the file.
**    PARAMETERS   
**       INPUT  : 
**          fd			int	file descriptor returned by ur_popen()
**          buflimit	int	max. number of fields (array size)
**       OUTPUT :  
**          attrbuf[] struct array containing the field definitions
**          tblname	char*	name of the table parsed.
**				tbltype	int*	type of the table
**    RETURNS      : # of fields, 0 for EOF, negative for errors.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_parse1(fd, attrbuf, buflimit, tblname, tbltype)
int					fd;
struct attr_def	attrbuf[];
int					buflimit;
char					*tblname;
int					*tbltype;
{
	int	token, numdims, ind;
	char	tokn_str[80], msg[256];					/* string which lex'd as token */
	UU_LOGICAL	joinflag;
	int	i, len;
	char	* strcpy();
	int	cumoff;							/* cumulative offset for struct */
	int	over;								/* amount over alignment boundary */

#if MODULEDBG != 0
	uu_denter(UU_RTRC,(us,"ur_parse1"));
#endif
	ind = 0;											/* starting index */
	cumoff = 0;										/* struct offsets start at zero */
	token = ur_get_tokn(fd, tokn_str);		/* first token from the uddl stream */
	if ((token != ALPHNUM) || (strcmp(tokn_str, "create")))
	{
		/* not a table definition */
		strcpy(tblname, "");					/* null table name */
		if (token == ALPHNUM)
		{
			/* check for recognized controls. */
			if (strcmp(tokn_str, "output") == 0)
				attrbuf[ind].attr_type = UR_OUTPUT;
			else if (strcmp(tokn_str, "prefix") == 0)
				attrbuf[ind].attr_type = UR_PREFIX;
			else if (strcmp(tokn_str, "suffix") == 0)
				attrbuf[ind].attr_type = UR_SUFFIX;
			else if (strcmp(tokn_str, "UNIBASE") == 0)
			{
				attrbuf[ind].attr_type = UR_UNIBASE;
				/* set ctrl stmt. flag */
				uu_set_bit(&attrbuf[ind].attr_flags, UR_CTRLLINE);
#if MODULEDBG != 0
				uu_dexit;
#endif
				return(1);		/* lie... no arguments really */
			}
			else
			{
#if MODULEDBG != 0
				uu_dexit;
#endif
				return(token == EOFTOK ? 0 : UR_UNREC);	/* unrecognized */
			}
		}
		else /* not ALPHNUM */
		{
#if MODULEDBG != 0
			uu_dexit;
#endif
			return(token == EOFTOK ? 0 : UR_UNREC);	/* unrecognized */
		}

		/* get control argument */
		token = ur_get_tokn(fd, tokn_str);
		if (token != ALPHNUM)
		{
#if MODULEDBG != 0
			uu_dexit;
#endif
			return(token == EOFTOK ? 0 : UR_UNREC);
		}

		/* control stmt. squish into def. struct */
		if ((len=strlen(tokn_str)) > STRLEN)
			{
			 sprintf(msg, "WARNING - truncating string '%s' to %d characters\n", tokn_str,STRLEN);
			 tokn_str[33] = '\0';
			 ud_printmsg(msg);
			}
		strcpy(attrbuf[ind].attr_name, tokn_str);	/* control argument */

		/* set ctrl stmt. flag */
		uu_set_bit(&attrbuf[ind].attr_flags, UR_CTRLLINE);
#if MODULEDBG != 0
		uu_dexit;
#endif
		return(1);								/* one def entry used */
	}

	token = ur_get_tokn(fd, tokn_str);
	if (	(token != ALPHNUM) ||
			(strcmp(tokn_str, "table") && strcmp(tokn_str, "modal")))
	{
#if MODULEDBG != 0
		uu_dexit;
#endif
		return(token == EOFTOK ? UR_UEOI : UR_EXTAB);	/* expecting table */
	}
	if (strcmp(tokn_str, "table")==0)
	{
		*tbltype = UR_TABLE;
	}
	else
	{
		*tbltype = UR_MODAL;
	}

	token = ur_get_tokn(fd, tokn_str);
	if (token != ALPHNUM)
	{
#if MODULEDBG != 0
		uu_dexit;
#endif
		return(token == EOFTOK ? UR_UEOI : UR_EXNAM);	/* expecting a name */
	}
	if ((len=strlen(tokn_str)) > STRLEN)
		{
		/* printf("WARNING - truncating string '%s' to %d characters\n", tokn_str,STRLEN);	*/
		 tokn_str[33] = '\0';
		}
	strcpy(tblname, tokn_str);					/* copy to the table name string */

	token = ur_get_tokn(fd, tokn_str);
	if (token != OPENPAREN)
	{
#if MODULEDBG != 0
		uu_dexit;
#endif
		return(token == EOFTOK ? UR_UEOI : UR_EXOP);	/* ( expected */
	}

	/* process the body of the definition */
	token = ur_get_tokn(fd, tokn_str);			/* prime the loop - get a token */
	
	do					/* loop expects the current token to be a type field. */
	{
		attrbuf[ind].attr_flags = 0;			/* clear special flags */
		if (token != ALPHNUM)
		{
#if MODULEDBG != 0
			uu_dexit;
#endif
			return(token == EOFTOK ? UR_UEOI : UR_EXTYP); /* Expecting type */
		}
												/* Get the type and size of this field. */
		attrbuf[ind].attr_type = ur_totype(tokn_str);
		if (attrbuf[ind].attr_type == UNKNOWN_TYPE)
			return(UR_UNDEFTYPE);

#if (UU_COMP==UU_IRIS4D || UU_COMP == UU_DECUNIX || (UU_COMP==UU_SUN && UU_SUNTYPE==UU_SUN_SUN4)) || (UU_COMP == UU_HPUX) || (UU_COMP == UU_WINNT) || (UU_COMP == UU_WIN2K)
		if (attrbuf[ind].attr_type == REAL || attrbuf[ind].attr_type == DOUBLE)
		{
			if (ind > 0)
			{
#ifdef UU_RS6000
#define DBL_ALIGN 4
#else
#define DBL_ALIGN 8
#endif
				over = cumoff % DBL_ALIGN;
				if (over)
				{
					attrbuf[ind-1].attr_off += DBL_ALIGN - over;
					cumoff += DBL_ALIGN - over;
				}
			}
		}
		if (attrbuf[ind].attr_type == INT
			|| attrbuf[ind].attr_type == KEY_ID
			|| attrbuf[ind].attr_type == REL_ID
			|| attrbuf[ind].attr_type == KEY_REF)
		{
			if (ind > 0)
			{
#define INT_ALIGN 4
				over = cumoff % INT_ALIGN;
				if (over)
				{
					attrbuf[ind-1].attr_off += INT_ALIGN - over;
					cumoff += INT_ALIGN - over;
				}
			}
		}
		if (attrbuf[ind].attr_type == JOIN || attrbuf[ind].attr_type == STRING)
		{
			if (ind > 0)
			{
				over = cumoff % sizeof(char *);
				if (over)
				{
					attrbuf[ind-1].attr_off += sizeof(char *) - over;
					cumoff += sizeof(char *) - over;
				}
			}
		}
#endif

		attrbuf[ind].attr_size = ur_findsiz(attrbuf[ind].attr_type);
		token = ur_get_tokn(fd, tokn_str);		/* now get the field name */
		/* are there pointers except generated by join? */
		if (token == ASTERISK)					/* check if really a pointer */
		{
			/* should return size = ptr size or base size????? */
			uu_set_bit(&attrbuf[ind].attr_flags, UR_PTR); /* set pointer flag */
			token = ur_get_tokn(fd, tokn_str);	/* get real name */
		}
		if (token != ALPHNUM)
		{
#if MODULEDBG != 0
			uu_dexit;
#endif
			return(token == EOFTOK ? UR_UEOI : UR_EXNAM); /* expecting a name */
		}
		if ((len=strlen(tokn_str)) > STRLEN)
			{
		 	 sprintf(msg, "WARNING - truncating variable name '%s' to %d characters\n", tokn_str,STRLEN);
		 	 ud_printmsg(msg);
			 tokn_str[33] = '\0';
			}
		strcpy(attrbuf[ind].attr_name, tokn_str); /* add name to def. */
		attrbuf[ind].num_rows = attrbuf[ind].num_cols = 1;
		numdims = 0;
		
		do												/* handling for arrays */
		{
			token = ur_get_tokn(fd, tokn_str);	/* now see what follows */
			if (token == OPENBRAK)
			{
				token = ur_get_tokn(fd, tokn_str);
				if (token == NUMB)
				{
					if (numdims == 0)
					{
						attrbuf[ind].num_cols = atoi(tokn_str);
						numdims = 1;
						uu_set_bit(&attrbuf[ind].attr_flags, UR_ARRAY);
					}
					else if (numdims == 1)
					{
						attrbuf[ind].num_rows = attrbuf[ind].num_cols;
						attrbuf[ind].num_cols = atoi(tokn_str);
						numdims = 2;
					}
					else
					{
#if MODULEDBG != 0
						uu_dexit;
#endif
						return(token == EOFTOK ? UR_UEOI : UR_TMD); /*Too many dims */
					}
					token = ur_get_tokn(fd, tokn_str);
					if (token != CLOSBRAK)
					{
#if MODULEDBG != 0
						uu_dexit;
#endif
						return(token == EOFTOK ? UR_UEOI : UR_EXCB); /* ] expected */
					}
				}
				else
				{
#if MODULEDBG != 0
					uu_dexit;
#endif
					return(token == EOFTOK ? UR_UEOI : UR_EXNUM); /* number exp */
				}
			}
		} while (token == CLOSBRAK);
		ur_findoff(&attrbuf[ind]);			/* do next element offset */
		cumoff += attrbuf[ind].attr_off;	/* accumulate struct offset */
		if ((ind == 0) && (attrbuf[ind].attr_type == KEY_ID) )
			cumoff += sizeof(long); /* allow for rel_num field */
		if (token == COMMA)
		{
			token = ur_get_tokn(fd, tokn_str);	/* comma ends field defs */
		}
		else if (token != CLOSPAREN)
					return(token == EOFTOK ? UR_UEOI : UR_EXCM); /* comma exp */
		ind++;										/* bump output array */
	} while ((token != CLOSPAREN) && (ind < buflimit));
	token = ur_get_tokn(fd, tokn_str); 			/* formality-';' */
	if (token != SEMICOLON)
	{
#if MODULEDBG != 0
		uu_dexit;
#endif
		return(token == EOFTOK ? UR_UEOI : UR_EXSC);	/* ; expected */
	}
#if MODULEDBG != 0
	uu_dexit;
#endif
			/* check whether the JOIN declarations are the last */
	joinflag = UU_FALSE;		
	for (i=0; ((i<ind)&&(!joinflag)); i++)
		if ((attrbuf[i].attr_type==JOIN) || (attrbuf[i].attr_type==STRING))
			joinflag = UU_TRUE;
	if (joinflag)
		for (; i<ind; i++)
			if ((attrbuf[i].attr_type!=JOIN) && (attrbuf[i].attr_type!=STRING))
			  {
				sprintf(msg, "The join and string declarations must be at the end of the table %s\n",tblname);
				ud_printmsg(msg);
				return(UR_UNDEFTYPE);
				break;
			  }
	return(ind);									/* return # of fields */
} /* uddl_parser */

