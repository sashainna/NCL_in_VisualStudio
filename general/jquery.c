/*********************************************************************
**    NAME         :  jquery.c
**       CONTAINS:
**				uj_init_query(list, cnt)
**				uj_putstr(list, str)
**				uj_free_list(list)
**       
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       jquery.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:47
*********************************************************************/


#include		"ustdio.h"
#include		"usysdef.h"
#ifdef UU_RS6000
#include <string.h>
#else
#if UU_OPSYS==UU_SYS53
#	include <bsd/strings.h>
#else
/*
.....Added compile directive for VMS compiles.  Roberta Z.
*/
#if UU_COMP != UU_VAXVMS
#	include		<string.h>
#endif
#endif
#endif
#include		"ulist.h"
#include		"udebug.h"


static 	int	UJ_query_ind;

/*********************************************************************
**    I_FUNCTION :  uj_init_query(list, cnt)
**       Initialize a list and the index.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_init_query(list, cnt)
UU_LIST	*list;
int	cnt;

{
	uu_denter(UU_MTRC,(us,"enter uj_init_query"));

	UJ_query_ind = 1;
	uu_list_init(list, sizeof(char *), cnt, cnt);

	uu_dexit;
	return 0;

}	/* uj_init_query */

/*********************************************************************
**    I_FUNCTION :  uj_putstr(list, str)
**       Put the input string on to the list. Rearange it, if nessaray.
**			The basic idea here is to have the list keeps a list of string,
**			each string has only one line.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_putstr(list, str)
UU_LIST	*list;
char	*str;

{
	char	buf[300];
	char	*bufptr, *strptr;
	char	*ptr, **chptr;
	char	*uu_malloc();
#ifndef UU_RS6000
	char	*index();
#endif
	register	int i, len, len1;

	uu_denter(UU_MTRC,(us,"enter uj_putstr"));
	len = strlen(str);
	ptr = index(str, '\n');		/* try to find a newline in the string */
	if (ptr == UU_NULL)			/* no newline */
	  { 
		len1 = len + 1;
	 	ptr = (char *)uu_malloc(len1*sizeof(char));
	 	strcpy(ptr, str); 
	 	uu_list_push(list, &ptr);
	  }
	else
	  {
		buf[0] = '\0';
		if (UJ_query_ind <= list->cur_cnt)	
		  {
			chptr = (char **)&(list->data[(UJ_query_ind-1)*list->item_size]); 
			for (i=UJ_query_ind; i<=list->cur_cnt; i++)
			  {
				strcat(buf,*chptr);
				uu_free(*chptr++);
				uu_dprint(UU_MTRC,(us,"catstr, buf=%s", buf));
			  }
			uu_dprint(UU_MTRC,(us,"after catstr, buf=%s", buf));
			list->cur_cnt = UJ_query_ind - 1;
			bufptr = &buf[strlen(buf)];
		  }
	   else
			bufptr = buf;
		strptr = str;
		for (i=0; i<len; i++)
		  {
			*bufptr++ = *strptr;
			if (*strptr == '\n')
			  {
				*bufptr = '\0';
				len1 = strlen(buf) + 1;
	 			ptr = (char *)uu_malloc(len1*sizeof(char));
	 			strcpy(ptr, buf); 
	 			uu_list_push(list, &ptr);
				UJ_query_ind++;
				bufptr = buf;
			  }
			strptr++;
		  }
		if (bufptr != buf)		/* There is a string not ended with newline */
		  {
			*bufptr = '\0';
	 		ptr = (char *)uu_malloc(strlen(buf)*sizeof(char));
	 		strcpy(ptr, buf); 
	 		uu_list_push(list, &ptr);
	     }
	  }
	uu_dexit;
	return 0;

}	/* uj_putstr */

/*********************************************************************
**    I_FUNCTION :  uj_free_list(list)
**       Free up the query information list.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_free_list(list)
UU_LIST	*list;

{
	register int i, num;
	char	**chptr;

	uu_denter(UU_MTRC,(us,"enter uj_free_list"));

	num = list->cur_cnt;
	chptr = (char **)list->data;
	for (i=0; i<num; i++)
		uu_free(*chptr++);
	uu_list_free(list);
	UJ_query_ind = 1;

	uu_dexit;
	return 0;

}	/* uj_free_list */
