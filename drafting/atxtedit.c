
/*********************************************************************
**    NAME         :  name
**       CONTAINS:
**			ua_edit_txt(text)
**			uai_edit_line(listhd,lines);
**			uai_delete_line(listhd,lines);
**			uai_add_line(listhd,lines);
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       atxtedit.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:41
*********************************************************************/

#include		"usysdef.h"
#include		"ustrings.h"
#include		"udebug.h"
#include		"uhep.h"
#include		"dasnog.h"
#include 	"atext.h"
#include 	"mdcoord.h"

/*********************************************************************
**    I_FUNCTION :  ua_edit_txt(text,txctattr)
**       Put put the popup menum to let user edit the text.
**			Note: A text segment will be created when "uc_display" is called
**					in this routine.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_edit_txt(text)
struct UA_txt_rec 	*text;

{
	register	i, chcount;
	char	ch,line_text[257];
	int	len;
	TXTLIST	*listhd, *pre, *ptr;
	int	lines;
	UU_LOGICAL	done;
	int	choice;
	int	status;
	UU_LOGICAL	before, first;

	uu_denter(UU_STRC,(us,"ua_edit_txt(str=%s)",text->tchar));
	lines = 0;
	listhd = UU_NULL;
	len = strlen(text->tchar);
	if (len > 1)
	  {
			/* construct a text list, each node contains a line */
		i = chcount = 0;
		listhd = pre = ptr = (TXTLIST *)uu_malloc(sizeof(TXTLIST));
		ptr->prev = ptr->next = UU_NULL;
		while (i<len)
	    {
			ch = text->tchar[i++];
			if ((ch=='\n')||(i==len))
		    {
				if (ch!='\n')
					ptr->str[chcount++] = ch;
				ptr->str[chcount] = '\0';
				lines++;
				uu_dprint(UU_STRC,(us,"str=%s, lines=%d", ptr->str, lines));
				chcount = 0;
				if (i < len)
			  	  {
					ptr = (TXTLIST *)uu_malloc(sizeof(TXTLIST));
		   		pre->next = (int *)ptr;
		   		ptr->prev = (int *)pre;
					ptr->next = UU_NULL;
					pre = ptr;
			  	  }
			  }
			 else
				ptr->str[chcount++] = ch;
		 }	/* for */
	 }
	uai_prnt_list(listhd);

	done = UU_FALSE;
	do
	{
	 uc_display(text);
	 status = ua_popmenu(23,&choice);	/* pop up the edit text menu */
	 uu_dprint(UU_STRC,(us,"status=%d, choice=%d", status, choice));
			/* choice 0 == no choice, choic 5 == exit */
	 if ((status!=UU_TRUE)||((status==UU_TRUE)&&((choice==0)||(choice==5))))
		 done = UU_TRUE;
	 else
		{
		 switch (choice)
		 {
		  case  1:			/* edit a line */
			uai_edit_line(&listhd,&lines);
			break;

		  case  2:			/* delete a line */
			if(lines > 1)
				uai_delete_line(&listhd,&lines);
			else
				{
				uu_uerror0(UA_DRAFTING,42);
				}
			break;

		  case 3:			/* add a line before */
			before = UU_TRUE;
			uai_add_line(&listhd,&lines,before);
			break;

		  case 4:			/* add a line after  */
			before = UU_FALSE;
			uai_add_line(&listhd,&lines,before);
			break;
		 }
		ptr = listhd;
		line_text[0] = '\0';
		while (ptr != UU_NULL)
		  {
			strcat(line_text,ptr->str);
			strcat(line_text,"\n");
			ptr = (TXTLIST *)ptr->next;
		  }
		text->no_tchar = strlen(line_text) + 1;
		ur_update_data_varlist(text->key,1,line_text,1,text->no_tchar);
		uc_retrieve_data(text,sizeof(struct UA_txt_rec));
/*		status = ur_retrieve_data_varlist(text->key,1,text->tchar,1,
			text->no_tchar);*/
	   uu_dprint(UU_STRC,(us,"textstr=%s",text->tchar));
	  }
/*	 status = um_update_geom(text,UM_DEFAULT_TF);*/
	}while (!done);

	ptr = listhd;
	while (ptr != UU_NULL)
	  {
		pre = ptr;
		ptr = (TXTLIST *)ptr->next;
		uu_free(pre);
	  }
	uu_dexit;
}	/* ua_edit_txt */



/*********************************************************************
**    I_FUNCTION :  uai_edit_line(text,txtary,&line)
**       Let the user edit a specific line.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uai_edit_line(listhd,lines)
TXTLIST	**listhd;
int		*lines;

{
	int	line_no, outints;
	int	numint;
	int	status;
	TXTLIST	*ptr;
	UD_STRREC	retstr;

	uu_denter(UU_STRC,(us,"ua_edit_line,lines=%d,listhd=%o",*lines,*listhd));
						/* Enter the number of the line to be modified */
	if(*lines != 1)
		{
		status = ud_ldas(UD_DASINT,UA_DRAFTING,121,&line_no,1,
													&outints,UD_NODEFAULT);
		}
	else
		{
		outints = line_no = 1;
		}
	if (outints != 0)		
	  {
		if ((line_no<1)||(line_no>*lines))
			uu_uerror0(UA_DRAFTING,38/*Invalid line number*/);
		else
		  {
			ptr = *listhd;
			while (line_no-- > 1)	ptr = (TXTLIST *) ptr->next;
			retstr.instring = ptr->str;
			ud_ddas(UD_DASSTRINGDEF,"",&retstr,UA_MAX_TEXT_LEN-1,&numint,
					  UD_DEFAULT);
			/* strcpy(ptr->str,retstr.instring); */
		  }
	  }
	uu_dprint(UU_STRC,(us,"after edit_line"));
	uai_prnt_list(*listhd);
	uu_dexit;
}	/* uai_edit_line */

/*********************************************************************
**    I_FUNCTION :  uai_delete_line(listhd,&line)
**       Let the user delete a specific line.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uai_delete_line(listhd,lines)
TXTLIST	**listhd;
int		*lines;

{
	int	line_no, outints;
	int	status;
	TXTLIST	*ptr, *tmptr;

	uu_denter(UU_STRC,(us,"ua_delete_line"));

						/* Enter the number of the line to be deleted  */
	status = ud_ldas(UD_DASINT,UA_DRAFTING,122,&line_no,1,&outints,UD_NODEFAULT);
	if (outints != 0)		
	  {
		if ((line_no<1)||(line_no>*lines))
			uu_uerror0(UA_DRAFTING,38/*Invalid line number*/);
		else
		  {
			ptr = *listhd;
			if (line_no==1)		/* delete first line, has to take care list hd */
			  {
				*listhd = (TXTLIST *) ptr->next;
				(*listhd)->prev = UU_NULL;
			  }
			else
			  {
				while (line_no-- > 1)	ptr = (TXTLIST *) ptr->next;
				tmptr = (TXTLIST *)ptr->prev;
				tmptr->next = ptr->next;
				tmptr = (TXTLIST *)ptr->next;
				if (tmptr!=UU_NULL)
					tmptr->prev = ptr->prev;
			  }
			(*lines)--;
			uu_free(ptr);
		  }
	  }
	uu_dexit;
}	/* uai_delete_line */

/*********************************************************************
**    I_FUNCTION :  uai_add_line(listhd,&line)
**       Let the user add a specific line.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uai_add_line(listhd,lines,before)
TXTLIST	**listhd;
int		*lines;
UU_LOGICAL	before;

{
	int	line_no, outints;
	int	msg;
	int	status;
	TXTLIST	*ptr, *pre, *tmptr;
	char	buf[257];

	uu_denter(UU_STRC,(us,"ua_add_line"));

	msg = (before)? 124 : 123;

				/* 124 - Enter the line number before which text is to be added */
				/* 123 - Enter the line number after which text is to be added */
	if(*lines != 1)
		{
		status = ud_ldas(UD_DASINT,UA_DRAFTING,msg,&line_no,1,
																&outints,UD_NODEFAULT);
		}
	else
		{
		line_no = outints = 1;
		}
	if (outints != 0)		
	  {
		if ((line_no<1)||(line_no>*lines))
			uu_uerror0(UA_DRAFTING,38/*Invalid line number*/);
		else
		  {
													/* enter note text */
			ud_ldas(UD_DASSTRING,UA_DRAFTING,52,buf,256,&outints,UD_NODEFAULT);
			if (outints != 0)
			  {
				if (before)	/* adjust the line number for doing only adding after */
					line_no = line_no - 1;
				ptr = (TXTLIST *)uu_malloc(sizeof(TXTLIST));
				strcpy(ptr->str,buf);
				if (line_no==0)/* special case for adding a line at the begining */
			  	  {
					ptr->next = (int *)*listhd;
					ptr->prev = UU_NULL;
					(*listhd)->prev = (int *)ptr;
					*listhd = ptr;
			  	  }
				else
			  	  {
					pre = *listhd;
					while (line_no-- > 1)	pre = (TXTLIST *) pre->next;
					ptr->prev = (int *)pre;
					tmptr = (TXTLIST *)pre->next;
					if (tmptr!=UU_NULL)		/* not adding a node at the end */
						tmptr->prev = (int *)ptr;
					ptr->next = pre->next;
					pre->next = (int *)ptr;
		  	  	  }
			 	(*lines)++;
		     }
			}
	    }
	uu_dexit;
}	/* uai_edit_line */


/*********************************************************************
**    I_FUNCTION :  uai_prnt_list(listhd)
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

uai_prnt_list(listhd)
TXTLIST	*listhd;

{
	TXTLIST	*ptr;

	uu_denter(UU_STRC,(us,"uai_prnt_list"));
	ptr = listhd;
	while (ptr != UU_NULL)
	  {
		uu_dprint(UU_STRC,(us,"Text=%s,ptr=%o",ptr->str,ptr));
		ptr = (TXTLIST *)ptr->next;
	  }
	uu_dexit;
}	/* uai_prnt_list */
