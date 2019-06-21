/**************************************************************************
**
**  NAME:       xe1paths.c 
**      contains:               
**           ux_append_path
**           ux_get_syspath
**           uxi_nxt_path_identifier
**           ux_expand_sym
**           ux_expand_string
**           uxi_append_to_strings
**           uxi_getenv_paths
**           ux_cat_paths
**           ux_is_opsys_dep
**           ux_chk_path_syntax
**           ux_search_for_path
**           ux_add_farea_exten
**           ux_add_ftype
**           ux_decompose_path
**           ux_get_base_fname
**           ux_get_base_farea
**           ux_is_type_legal
**           uxi_get_suffixes
**           ux_strip_quotes
**           uxi_get_legal_name
**
**  COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**   MODULE NAME AND RELEASE LEVEL 
**       xe1paths.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:32
**
**************************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "udebug.h"                     /* used by the debug system */
#include "umoveb.h"
#include "derror.h"                     /* needed for error system resolution */
#include "uhep.h"                               /* used by the debug system */
#include "xfsys0.h"
#include "xenv1.h"                      /* include file for level 1 env system */
#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define index strchr
#define rindex strrchr
#endif
#define TRACE UU_TRUE

extern UU_LOGICAL NCLX_external_unibase;

/**************************************************************************
**    E_FUNCTION: int ux_append_path(path, append, options)
**                      Attempts to append the path name piece designated by, 
**                      "append", to the system dependent path piece, "path".
**              PARAMETERS:
**                      "path": input: A quoted system dependent path name piece to 
**                              be appended to.
**                              output: Appended path.
**                      "append": A representation of the path piece to be added
**                              to "path". This can be an environmental variable, a
**                              UNICAD path specification, or a quoted system dependent path
**                              specification.
**                      "options": A bitwise "OR" of one choice in each of the
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2. UX_QUOTES: any output system dependent path is returned 
**                                              with UX_QUOTE_CHAR surrounding it.
**                                      UX_NQUOTES: any output system dependent path name is returned
**                                              with UX_QUOTE_CHAR surrounding it.
**                                      Incorrectly specified is the same as UX_PRTERRS | UX_QUOTES.
**              RETURNS:
**                      UU_SUCCESS is returned if "path" is appended to; otherwise,
**                      one of the following values is returned:
**                              UX_FAILURE: something went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_append_path(path, append, options)
	UX_pathname path;
	char *append;
	int options;
{
	char *pathlistptr = UU_NULL;    
	UU_LOGICAL found, printit;
	UX_pathname addpath;
	int status;

	uu_denter(UU_XTRC,(us,"ux_append_path(path:%s,append:%x,options:%d)",
					path, append, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (append!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"append:%s", append));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	if (ux_get_syspath(append,&pathlistptr,addpath,&found,options) != UU_SUCCESS)
		goto failed;
	ux_cat_paths(path,addpath,path,options);
	uu_lsdel(pathlistptr);   /* delete the list created by get_syspath */

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT;
done:;
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION: int ux_get_syspath(inpath, pathlistptrptr, outpath, 
**                                                                              foundptr, options)
**              This function takes the path name in "inpath" and returns the
**              next path name with which it corresponds. We assume "inpath" is not
**              UU_NULL.
**    PARAMETERS   
**       INPUT: 
**                      inpath          Path name from which to get the next system dependent
**                                                      path name.
**                                                      Note, "inpath" can be a UNICAD path name specification, 
**                                                      or, a quoted system dependent specification.
**                      pathlistptrptr This is a pointer to a pointer, p, such that p points 
**                                                      to a list of data elements used in finding the next
**                                                      path name corresponding to "inpath". If "*pathlistptrptr"
**                                                      is UU_NULL, then we return the first path corresponding
**                                                      to "inpath". If "*pathlistptrptr" != UU_NULL, then
**                                                      the data pointed to by "*pathlistptrptr" indicates how
**                                                      to get the next path name.
**                      options One of the following values:
**                                                              UX_QUOTES: return quotes sys. dep. pathname
**                                                              UX_NQUOTES: don't quote sys. dep. pathname
**                                                              UX_PRTERRS: print any errors as specified by
**                                                                      uu_uerror functions.
**                                                              UX_NPRTERRS: don't print any errors as 
**                                                                      specified by uu_uerror functions.
**                                                      If incorrectly specified we will print errors.
**       OUTPUT:  
**                      pathlistptrptr Returns with "*pathlistptrptr" pointing to the new
**                                                      list indicating how to get to the next path name 
**                                                      (if any).
**                      outpath         Output buffer containing the next system dependent path 
**                                                      name corresponding to "inpath".
**                      foundptr                Pointer to UU_TRUE iff a new path name is returned in
**                                                      "outpath".
**    RETURNS: UU_SUCCESS if no problems encountered; otherwise, UX_FAILURE
**                      is returned. UX_NFOUND is returned  along with outpath = UU_NULL 
**                      when we have finished the list and there are no more paths.
**    SIDE EFFECTS: none
**    WARNINGS: Note, after all paths desired from "inpath" have been obtained
**              but the list wasn't exhausted, the list must be destoryed in the 
**                      calling function. Continuing to call ux_get_syspath after the list
**                      was exhausted (test foundptr and status ) will start the process again.
*********************************************************************/
int ux_get_syspath(oinpath, pathlistptrptr, outpath, foundptr, options)
	char *oinpath;
	char **pathlistptrptr;
	UX_pathname outpath;
	UU_LOGICAL *foundptr;
	int options;
{
	UX_pathname inpath;
	char *nxtelt;
	UU_LOGICAL printit;
	char *uu_lsnext();
	char *index();
	char *rindex();
	char *first;
	char *last;
	int status;
	int uxi_getenv_paths();
	int ux_is_opsys_dep();
	int uxi_nxt_path_identifier();
	int ux_cat_paths();

	uu_denter(UU_XTRC,(us,"ux_get_syspath(%x,pathlistptrptr,?,?,options:%d)",
							inpath, options));
/*
.....the inpath could be env value or a real system path. We added '%' in front of
.....an env value to pass into ux_expand_string. so if it is a system path such as 'c:\tmp' 
.....it become '%c:\tmp', when pass into ux_expand_string function,
.....it will become 'c:\tmp'. If it is a enviorment value, such as 
.....'NCLPATH', it will get value define first. So just add '%' in front of inpath
*/
	if ((oinpath[0]!='%')&&(oinpath[0]!='\\'))
		strcpy(inpath, "%");
	else
		inpath[0] = '\0';
	strcat(inpath, oinpath);
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (inpath!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"inpath:%s", inpath));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	strcpy(outpath, "");

	if (*pathlistptrptr == UU_NULL) /* first time through, build entire list */
	{
#ifdef UU_NOERROR
		uu_toolmalloc_init();           /* initialization needed by list programs */
#endif
		if (ux_expand_string(inpath, UX_ALL, uxi_getenv_paths, ux_is_opsys_dep,
			uxi_nxt_path_identifier, ux_cat_paths, pathlistptrptr, options)
			!= UU_SUCCESS)
			goto failed;
	}
	/* get next on list or first on list if first time through */
	nxtelt = uu_lsnext(*pathlistptrptr);
	if (nxtelt == UU_NULL)
	{
		/* case of being at end of list, delete the list now */
		uu_lsdel(*pathlistptrptr);      
		/* note: use these flags or else next call will start the process
		over and give a new list and first on that list */
		*foundptr = UU_FALSE;           /* next value not found */
		status = UX_NFOUND;
	}
	else
	{
		*pathlistptrptr = nxtelt;
		strcpy(outpath, nxtelt);
		*foundptr = UU_TRUE;
		if ( options == (options | UX_NQUOTES) )
		{
		/* remove the quotes from outpath */
		first = index(outpath,UX_QUOTE_CHAR);
		strcpy(outpath,(first+1));
		last = rindex(outpath,UX_QUOTE_CHAR);
		*last = '\0';
		}
	}
	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT;
done:;
	uu_dexit;
	return(status);
}
/*********************************************************************
**    I_FUNCTION: int uxi_nxt_path_identifier(path, ident, options)
**              This function returns the first piece of the path, "path", and
**              deletes the returned piece from "path".
**    PARAMETERS   
**       INPUT: 
**                      path            The path name from which the identifier is to be returned.
**                                              We assume "path" is a UNICAD path name specification
**                                              and "path" is not UU_NULL.
**                      options                 A bitwise "OR" of one choice in each of the
**                                                              following categories:
**                                                              1.      UX_PRTERRS: print any errors as specified by
**                                                                              uu_uerror functions.
**                                                                      UX_NPRTERRS: don't print any errors as 
**                                                                              specified by uu_uerror functions.
**                                                              2. UX_QUOTES: any output system dependent path is 
**                                                                              returned with UX_QUOTE_CHAR surrounding it.
**                                                                      UX_NQUOTES: any output system dependent path name 
**                                                                              is returned with UX_QUOTE_CHAR surrounding it.
**                                                              Incorrectly specified is the same as 
**                                                                      UX_PRTERRS | UX_QUOTES.
**       OUTPUT:  
**                      path            Returns without its original first identifier.
**                      ident           Returns with the first identifier in the path
**    RETURNS: UU_SUCCESS if "ident" is returned; otherwise, one of the
**                               following values is returned:
**                                              (the following is not an error)
**                                      UX_NFOUND: next identifier not found; "path" exhausted;
**                                              (the following are errors)
**                                      UX_BAD_SUBJECT: "path" is bad;
**                                      UX_FAILURE: something went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int uxi_nxt_path_identifier(path, ident, options)
	UX_pathname path;
	UX_pathname ident;      
	int options;
{
	char *marker;
	char *tempm;
	char *index();
	UX_pathname temppath;
	int status;
	UU_LOGICAL printit;

	uu_denter(UU_XTRC,
		(us,"uxi_nxt_path_identifier(path:%s,?,option:%d)",path,options));
	status = UU_SUCCESS;
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	strcpy(ident,"");
	if ( strcmp(path,"") == 0)
		{
			status = UX_NFOUND;
			goto done;
		}
	tempm = path;
/*
.....here does not consider the networking path such as "//computer_name/c/nclnt/...",
.....so if the beginning is "//", don't remove
*/
	while ( (*tempm == ' ') || (*tempm == '\t') || 
		(*tempm == UX_PATH_SEP)&&(*(tempm+1)!=UX_PATH_SEP))
		/* front of string is white or meaningless seperator - remove */
		++tempm;
	strcpy(temppath,tempm);
	marker = (index(temppath,UX_PATH_SEP));
	if (marker != UU_NULL)
	{
		if ((marker[0]==UX_PATH_SEP)&&(marker[1]==UX_PATH_SEP))
		{
			marker++;
		}
		strcpy(path,++marker);
		*(--marker) = '\0';
		strcpy(ident,temppath);
	}
	else
	{
		strcpy(ident,temppath);
		strcpy(path,"");
	}
	goto done;
done:;
	uu_dexit;
	return(status);
}
/*********************************************************************
**    I_FUNCTION: int ux_expand_sym(sym, extent, symlistptr, options)
**              This function returns a list of operating system dependent path
**              names that are the expansion of "sym". Note, no attempt to 
**              check whether the validity of the path names is made.
**    PARAMETERS   
**       INPUT: 
**                      sym                             Symbol (string) used as the key by which to retrieve 
**                                                              the requested string to expand (parse).
**                      extent                  Currently either UX_FIRSTONLY, or UX_ALL.
**                                                                      UX_FIRSTONLY: get first path only;
**                                                                      UX_ALL: get all paths.
**                      get_parse_ls   Function which returns the list to parse that is 
**                                                              the value of "sym". This function is called as 
**                                                              follows: 
**                                                              if (get_parse_ls(sym, extent, parselist, options)
**                                                                      != UU_SUCCESS) goto ??
**                      is_terminal             Function which termines if a string is a terminal 
**                                                              string or not. This function is called as follows:
**                                                              if (is_terminal(string, options) == UU_SUCCESS)
**                                                                      <then "string" is termial>
**                      nxt_token               Function to get the next token to expand from a 
**                                                              string. This function is called as follows:
**                                                              if (nxt_token(string, returntoken, options)
**                                                                                      == UU_SUCCESS)
**                                                                      <then a token was found and returned in
**                                                                              "returntoken">
**                      cat_strings             Function to put terminal strings together. This may
**                                                              merely be a concatenation function in many contexts.
**                                                              This function is called as follows:
**                                                              if (cat_strings(frontstrg, backstrg, returnstrg,
**                                                                              options) != UU_SUCCESS) <then function failed>
**                      options                 A bitwise "OR" of one choice in each of the
**                                                              following categories:
**                                                              1.      UX_PRTERRS: print any errors as specified by
**                                                                              uu_uerror functions.
**                                                                      UX_NPRTERRS: don't print any errors as 
**                                                                              specified by uu_uerror functions.
**                                                              2. UX_QUOTES: any output system dependent path is 
**                                                                              returned with UX_QUOTE_CHAR surrounding it.
**                                                                      UX_NQUOTES: any output system dependent path name 
**                                                                              is returned with UX_QUOTE_CHAR surrounding it.
**                                                              Incorrectly specified is the same as 
**                                                                      UX_PRTERRS | UX_QUOTES.
**       OUTPUT:  
**                      symlistptr      A pointer to a list of operating system dependent 
**                                                      path names corresponding to "sym".
**    RETURNS: UU_SUCCESS if the expansion can be done and returned in
**                      "symlist"; otherwise, UX_FAILURE is returned.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_expand_sym(sym, extent, get_parse_ls, is_terminal, nxt_token, 
								cat_strings, symlistptr, options)
	char *sym;
	char *extent;
	int (*get_parse_ls)();
	int (*is_terminal)();
	int (*nxt_token)();
	int (*cat_strings)();
	char **symlistptr;
	int options;
{
	char *symlist = UU_NULL;
	char *parselist = UU_NULL; /* pointer to the list of paths that are obtained
									   * by a "getenv" on "sym" */
	char *sysdeplist = UU_NULL;     /* pointer the list of system dependent paths 
										  * associated with one of "sym"'s paths */
	UU_LOGICAL printit;
	char *tempsptr;
	char *temppptr;
	char *uu_lsnext();
	char *uu_lsnew();
	char *uu_lsinsrt();
	int status;

	uu_denter(UU_XTRC,(us,
		"ux_expand_sym(sym:%s,extent:%s,?,options:%d)",sym,extent,options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	parselist = uu_lsnew();
	temppptr = parselist;           /* save copy of pointer to head of list */
	if ((status = (*get_parse_ls)(sym, parselist, options)) != UU_SUCCESS)
		goto done;               /* uxi_getenv_paths(sym, parselist, options) */

	symlist = uu_lsnew();            /* create a list for "sym"'s paths */
	if (strcmp(extent, UX_FIRSTONLY) == 0)
	{
		parselist = uu_lsnext(parselist);       /* must come back non-UU_NULL here */
		if (ux_expand_string(parselist, UX_FIRSTONLY, get_parse_ls, is_terminal,
							nxt_token, cat_strings, &symlist, options) != UU_SUCCESS)
									goto failed;
		*symlistptr = symlist;
	}
	else /* get all paths */
	{
		*symlistptr = symlist;
		/* expand each element of "parselist"; put results on a "sysdeplist" */
		while ((parselist = uu_lsnext(parselist)) != UU_NULL)
		{       
			/* create list for system paths associated with one of "sym"'s paths */
			if ((status = ux_expand_string(parselist, UX_ALL, get_parse_ls,
		is_terminal, nxt_token, cat_strings, &sysdeplist, options)) != UU_SUCCESS)
					goto failed;
			/* now fill in "symlist" with the op sys dependent lists found */
			tempsptr = sysdeplist;
			while ((sysdeplist = uu_lsnext(sysdeplist)) != UU_NULL)
			{
			/* put head of sysdeplist on symlist, is sys dep expansion of "sym"*/
				symlist = uu_lsinsrt(symlist, (UX_MAX_PATH_LEN));
				uu_move_byte(sysdeplist, symlist, (UX_MAX_PATH_LEN));
			}
			uu_lsdel(tempsptr);
		}
	}
	uu_lsdel(temppptr);             /* delete the parselist */
	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT;
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ux_expand_string(string, extent, get_parse_ls, 
**                                              is_terminal, nxt_token, cat_strings, termlistptr, options)
**              This function expands "string".
**    PARAMETERS   
**       INPUT: 
**                      string                  String to expand.
**                      extent                  Currently either UX_FIRSTONLY, or UX_ALL.
**                                                                      UX_FIRSTONLY: get first string only;
**                                                                      UX_ALL: get all strings.
**                      get_parse_ls   Function which returns the list to parse that is 
**                                                              the value of "sym". This function is called as 
**                                                              follows: 
**                                                              if (get_parse_ls(sym, parselist, options)
**                                                                      != UU_SUCCESS) goto ??
**                      is_terminal             Function which termines if a string is a terminal 
**                                                              string or not. This function is called as follows:
**                                                              if (is_terminal(string, options) == UU_SUCCESS)
**                                                                      <then "string" is termial>
**                      nxt_token               Function to get the next token to expand from a 
**                                                              string. This function is called as follows:
**                                                              if (nxt_token(string,returntoken,options) == UU_SUCCESS)
**                                                                      <then a token was found and returned in
**                                                                              "returntoken">
**                      cat_strings             Function to put terminal strings together. This may
**                                                              merely be a concatenation function in many contexts.
**                                                              This function is called as follows:
**                                                              if (cat_strings(frontstrg, backstrg, returnstrg,
**                                                                              options) != UU_SUCCESS) <then function failed>
**                      options                 A bitwise "OR" of one choice in each of the
**                                                              following categories:
**                                                              1.      UX_PRTERRS: print any errors as specified by
**                                                                              uu_uerror functions.
**                                                                      UX_NPRTERRS: don't print any errors as 
**                                                                              specified by uu_uerror functions.
**                                                              2. UX_QUOTES: any output system dependent path is 
**                                                                              returned with UX_QUOTE_CHAR surrounding it.
**                                                                      UX_NQUOTES: any output system dependent path name 
**                                                                              is returned with UX_QUOTE_CHAR surrounding it.
**                                                              Incorrectly specified is the same as 
**                                                                      UX_PRTERRS | UX_QUOTES.
**       OUTPUT:  
**                      termlistptr             A pointer to a list of terminal strings
**                                                              to return.
**    RETURNS: UU_SUCCESS if no problems encountered; otherwise, one of the
**                               following values is returned:
**                                      UX_BAD_SUBJECT: "path" is bad;
**                                      UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_expand_string(string, extent, list_to_parse, is_terminal, nxt_token,
							  cat_strings, termlistptr, options)
	char *string;
	char *extent;
	int (*list_to_parse)();
	int (*is_terminal)();
	int (*nxt_token)();
	int (*cat_strings)();
	char **termlistptr;
	int options;
{
	char *termlist = UU_NULL;
	UX_pathname tempstring;
	UX_pathname ident;
	char *idstringlist = UU_NULL;
	char *tlist;
	char *uu_lsnew();
	char *uu_lsnext();
	char *uu_lsinsrt();
	UU_LOGICAL printit;
	int status;

	uu_denter(UU_XTRC,(us,
		"ux_expand_string(string:%s,extent:%s,?,options:%d)",
		string, extent, options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	termlist = uu_lsnew();          /* set up head of a list */
	if ((status = (*is_terminal)(string,options)) == UU_SUCCESS)  /* is opsys dep */
				/* ux_is_opsys_dep(string, options) */
	{
		/* "string" is already terminal; so put "string" on list */
		tlist = uu_lsinsrt(termlist, (UX_MAX_PATH_LEN));
		uu_move_byte(string,tlist,(strlen(string)+1 < UX_MAX_PATH_LEN)?strlen(string)+1:UX_MAX_PATH_LEN);
		*termlistptr = termlist;
		goto done;
	}
	if (status != UX_NOPSYS)
		goto done;

	/* else "string" is not operating system dependent and must be expanded */
	strcpy(tempstring, string);                     /* make a copy we can chop up */
	while ((status = (*nxt_token)(tempstring,ident,options)) == UU_SUCCESS)
					/* uxi_nxt_path_identifier(tempstring, ident, options) */
	{
	/* append each expansion string of "ident" to the strings in "termlist" */

		if ((status = ux_expand_sym(ident, extent, list_to_parse, is_terminal, 
								nxt_token, cat_strings, &idstringlist, options))
				!= UU_SUCCESS) 
		{
			UX_ERROR1(7, ident, printit);
			/* Unable to expand identifier %s in ux_expand_sttring */
			goto failed;
		}

		/* "idstringlist" has terminal strings in it corresponding to "ident";
		 * so each in "idstringlist" must be added to the end of each string
		 * in a copy of "termlist".  */
		if (uxi_append_to_strings(&termlist, idstringlist, cat_strings, options)
				!= UU_SUCCESS) goto failed;
	}                                                                                                               /* end while */

	if (status != UX_NFOUND) /* something went wrong */
		goto done;
	*termlistptr = termlist; /* get the list to return */
	status = UU_SUCCESS;

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT;
done:;
	uu_dexit;
	return(status);
}
/*********************************************************************
**    I_FUNCTION: int uxi_append_to_strings(listallptr, appendlist, 
**                                                                              cat_strings, options)
**              This function appends the terminal string, "append", to each string
**              on "listallptr".
**    PARAMETERS   
**       INPUT: 
**                      listallptr      A pointer to a list of path pieces, each 
**                                                                      of which is to be appended to. We assume we
**                                                                      are at the head of the list.
**                      appendlist                      List of path pieces to append to copies of paths
**                                                                      in "listallptr".
**                      cat_strings                     Function to put terminal strings together. This 
**                                                                      may merely be a concatenation function in many 
**                                                                      contexts.
**                                                                      This function is called as follows:
**                                                                      if (cat_strings(frontstrg, backstrg, returnstrg,
**                                                                              options) != UU_SUCCESS) <then function failed>
**                      options                         A bitwise "OR" of one choice in each of the
**                                                                      following categories:
**                                                                      1.      UX_PRTERRS: print any errors as specified by
**                                                                                      uu_uerror functions.
**                                                                              UX_NPRTERRS: don't print any errors as 
**                                                                                      specified by uu_uerror functions.
**                                                                      2. UX_QUOTES: any output system dependent path is 
**                                                                                      returned with UX_QUOTE_CHAR surrounding it.
**                                                                              UX_NQUOTES: any output system dependent path 
**                                                                                      name is returned with UX_QUOTE_CHAR 
**                                                                                      surrounding it.
**                                                                      Incorrectly specified is the same as 
**                                                                      UX_PRTERRS | UX_QUOTES.
**       OUTPUT:  
**                      listallptr      A pointer to the list of newly appended path 
**                                                                      pieces.
**    RETURNS: UU_SUCCESS if all path pieces in "pathlist" have been 
**                               appended to; otherwise, UX_FAILURE and no paths will have
**                               been appended to.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int uxi_append_to_strings(listallptr, appendlist, cat_strings, options)
	char **listallptr;
	char *appendlist;       /* head of list of items to append */
	int (*cat_strings)();
	int options;
{
	char *listall = UU_NULL;
	char *listoflists;
	char *nxtlst = UU_NULL;
	char *appendptr;
	char *uu_lsnext();
	char *uu_lsinsrt();
	char *uu_lsnew();
	UU_LOGICAL printit;
	int status;
	
	uu_denter(UU_XTRC,(us,
		"uxi_append_to_strings(listall:%x,appendlist:%x,cat_strings,options:%d)",
		*listallptr, appendlist, options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	listall = *listallptr;          /* save pointer to the head of list */
	appendptr = appendlist;         /* save pointer to the head of list */
	if (listall == UU_NULL)
	{       /* then there is currently no list to append to; so put all of
		 * "appendlist" on a list to append to in subsequent calls.  */
		listall = uu_lsnew();
		while ((appendlist=uu_lsnext(appendlist)) != UU_NULL)
		{ 
			listall = uu_lsinsrt(listall, (UX_MAX_PATH_LEN));
			uu_move_byte(appendlist, listall, (UX_MAX_PATH_LEN));
		} 
	}
	else if ((uu_lsnext(listall)) == UU_NULL)
	{       /* the list was created but nothing is on it */
		while ((appendlist = uu_lsnext(appendlist)) != UU_NULL)
		{ 
			listall = uu_lsinsrt(listall,(UX_MAX_PATH_LEN));
			uu_move_byte(appendlist, listall, (UX_MAX_PATH_LEN));
		} 
	}
	else     /* "listall" is not empty; append to each path */
	{
		listoflists = uu_lsnew();
		/* we build up listoflists and it replaces listall, old master list */
		nxtlst = listoflists;   /* initialize to start at head of listoflists */
		while ((listall = uu_lsnext(listall)) != UU_NULL) 
		{
			appendlist = appendptr;
			while ((appendlist = uu_lsnext(appendlist)) != UU_NULL )
			{
				/*  allocate a spot in listoflists for the new element */
				nxtlst = uu_lsinsrt(nxtlst,(UX_MAX_PATH_LEN));
				/* ux_cat_paths(listall,appendlist,nxtlst,options) */
				if ((*cat_strings)(listall,appendlist,nxtlst,options) != UU_SUCCESS)
						goto failed;
			}
		}       /* end while; no more to append */
		uu_lsdel(*listallptr);  /* delete old master list now obsolete */
		*listallptr = listoflists;      /* return pointer to new list */
	}
	uu_lsdel(appendptr);    /* delete the appendlist now that task is done */       

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT;
done:;
	uu_dexit;
	return(status);
}
/*********************************************************************
**    I_FUNCTION: int uxi_getenv_paths(sym,parselist,options)
**                      Attempts to create a list of alternative values found in
**                      the symbol table for the symbol name "sym", in the list
**                      "parselist", which isn't expanded into sys dependent paths.
**		This function is only called inside function 'ux_expand_string' from function 
**		'ux_get_syspath', if the 'sym' start with "%" then we will treat it as envoirment
**		value, if not, just add 'sym' into parselist.
**    PARAMETERS   
**       INPUT: 
**                              "sym" string which is the name of environmental variable
**                              "parselist" pointer to head of a list
**                              "options"       A bitwise "OR" of one choice in each of the
**                                                                      following categories:
**                                                                      1.      UX_PRTERRS: print any errors as specified by
**                                                                                      uu_uerror functions.
**                                                                              UX_NPRTERRS: don't print any errors as 
**                                                                                      specified by uu_uerror functions.
**                                                                      2. UX_QUOTES: any output system dependent path is 
**                                                                                      returned with UX_QUOTE_CHAR surrounding it.
**                                                                              UX_NQUOTES: any output system dependent path 
**                                                                                      name is returned with UX_QUOTE_CHAR 
**                                                                                      surrounding it.
**                                                                      Incorrectly specified is the same as 
**                                                                      UX_PRTERRS | UX_QUOTES.
**    RETURNS:  UU_SUCCESS if symbol value is found, or if not and the symbol
**                              name "sym" is put itself on the list. UX_FAILURE returned for
**                              other errors in execution.
**              SIDE EFFECTS: none
**              WARNINGS: none
*********************************************************************/
int uxi_getenv_paths(sym,parselist,options)
	UX_pathname sym;
	char *parselist;
	int options;
{
	int status, env;
	char *svalue;
	char *uu_lsnext();
	char *uu_lsinsrt();
	char *ux_getenv();
	char *smarker;
	char *plisthead;
	char *index();
	UX_pathname tempsv, sval;
	UX_pathname select;
	UU_LOGICAL printit;

	uu_denter(UU_XTRC,(us,"uxi_getenv_paths(sym:%s,??,options:%d)",sym,options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	plisthead = parselist;

	/* look for sym in symbol table and get its value */
	env = 0;
	if (sym[0]=='%')
	{
		env = 1;
		strcpy(sym, &sym[1]);
		svalue = ux_getenv(sym, options|UX_QUOTES);
		uu_dprint(UU_XTRC,(us,"sym is:%s", sym));
		if (svalue != UU_NULL)
		{
			strcpy(sval, svalue);
			uu_dprint(UU_XTRC,(us,"svalue is:%s", svalue));
		}
		else
			sval[0] = '\0';
	}
	else
		strcpy(sval, sym);
	if ((sval[0] == '\0')||(env==0))
	{
		/* not found as a symbol, return sym itself in the list */
		/* is symbol table init'd first  ???? */
		strcpy(tempsv,UX_QUOTE_STR);
		strcat(tempsv, sym);
		strcat(tempsv,UX_QUOTE_STR);
		parselist = uu_lsinsrt(parselist,(UX_MAX_PATH_LEN));
		uu_move_byte(tempsv, parselist, (UX_MAX_PATH_LEN));
	}
	else    /* see if alt. values given, break svalue up into its parts */
	{
		strcpy(tempsv,sval);
		strcpy(select,tempsv);
		smarker = index(tempsv, UX_SEP_CHAR);
		while (smarker!=UU_NULL)
		{
			strcpy(select,++smarker);
			strcpy(--smarker,"\0");
			/* for each alt. value found, put it on the list */
			parselist = uu_lsinsrt(parselist,(UX_MAX_PATH_LEN));
			uu_move_byte(tempsv, parselist, (UX_MAX_PATH_LEN));
			smarker = index(select, UX_SEP_CHAR);   /* get next seperator */
		}
		/* no alternative values found in original svalue or else no more
		alt. values were found --  finish up, put svalue on the list: */
		/* if (strcmp(select,"\0")!=0)   not null or exhausted */
		if ( strlen(select) != 0)               /* a better test */
		{
			parselist = uu_lsinsrt(parselist,(UX_MAX_PATH_LEN));
			uu_move_byte(select, parselist, (UX_MAX_PATH_LEN));
		}       
	}

	goto done;
done:;
	uu_dexit;
	return(status);
}       
/*********************************************************************
**    E_FUNCTION :  ux_cat_paths(front, back, outpath, options)
**                      
**                      Attempts to concatenate the path names, "front" and
**                      "back" to obtain a syntactically correct path name for
**                      the operating system represented by "front".
**              PARAMETERS:
**                      "front": A quoted system dependent portion of a file path 
**                              name (with possible specification of the operating 
**                              system).
**                      "back": A quoted system dependent portion of a file path 
**                              name (with possible specification of the operating 
**                              system).
**                      "outpath": A system dependent portion of a file path
**                              file (with possible specification of the operating 
**                              system ). The argument for this parameter can be the
**                              same as "front" or "back".
**                      "options": A bitwise "OR" of one choice in each of the 
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2.      UX_CHK: check the syntax of the path name.
**                                      UX_NCHK: don't check the syntax of the path
**                                              name.
**                              3. UX_QUOTES: any output system dependent path is returned 
**                                              with UX_QUOTE_CHAR surrounding it.
**                                      UX_NQUOTES: any output system dependent path name is returned
**                                              with UX_QUOTE_CHAR surrounding it.
**                      Incorrectly specified is the same as UX_PRTERRS | UX_NCHK | UX_QUOTES.
**              RETURNS:
**                      UU_SUCCESS if "outpath" is created; otherwise, one
**                      of the following values is returned:
**                                      (the following are all considered errors)
**                              UX_BAD_SUBJECT: "front" is bad;
**                              UX_BAD_TARGET: "back" is bad;
**                              UX_NOPSYS: the operating system specifications
**                                      for "front" and/or "back" is bad;
**                              UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_cat_paths(frontpath, backpath, outpath, options)
	char *frontpath;
	char *backpath;
	UX_pathname outpath;
	int options;
{
	int status,back_quoted; 
	UU_LOGICAL printit;
	UX_pathname front, back, tmpstr;
	char *firstb, *firstf;
	char *lastb, *lastf;
#if UU_COMP == UU_VAXVMS
	char *left, *right;
#endif
	char *rindex();
	char *index();

	uu_denter(UU_XTRC,(us,"ux_cat_paths(frontpath:%x,backpath:%x,??,options:%d)",
		frontpath,backpath,options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (frontpath!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"frontpath:%s", frontpath));
		if (backpath!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"backpath:%s", backpath));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	UX_CHK_PATHERRS(frontpath, printit, options);
	UX_CHK_PATHERRS(backpath, printit, options);

	strcpy(front,frontpath);
	strcpy(back,backpath);
	if (strcmp(front,"") == 0)
	{
		strcpy(outpath,back);
		goto done;
	}
	if (strcmp(back,"") == 0)
	{
		strcpy(outpath,front);
		goto done;
	}

#if UU_COMP == UU_VAXVMS
/* fix peices to be all one style of dir specs */
/* if ] followed by .dir, replace ] with . and replace .dir with ] */
	if ((right=index(front,']'))!=UU_NULL)
		if ((left=index(right+1,'.'))!=UU_NULL)
			if ((strncmp(left,".dir",4)==0) || (strncmp(left,".DIR",4)==0))
			{
				*right = '.';
				*left = ']';
				*++left = '\0';
			}
	if ((right=index(back,']'))!=UU_NULL)
		if ((left=index(right+1,'.'))!=UU_NULL)
			if ((strcmp(left,".dir")==0) || (strcmp(left,".DIR")==0))
			{
				*right = '.';
				*left = ']';
				*++left = '\0';
			}
#endif

	back_quoted = 0;
	/* remove part of the quotes for each peice front, back: */
	if ( (firstb = index(back,UX_QUOTE_CHAR)) != UU_NULL)
	{
		if ( (lastb = rindex(firstb+1,UX_QUOTE_CHAR)) == UU_NULL)
			/* error message about lack of quote match */
			goto failed ;
		*++lastb = '\0';
		strcpy(back,++firstb);
		back_quoted = 1;
	}
	if ( (firstf = index(front,UX_QUOTE_CHAR)) != UU_NULL)
	{
		if ( (lastf = rindex(firstf+1,UX_QUOTE_CHAR)) == UU_NULL)
			/* error message about lack of quote match */
			goto failed;
		*lastf = '\0';
		strcpy(front,firstf);
	}
	else if (back_quoted==1)
	{
/*
.....we need add a quote_char in the front part in order to make the back part
*/
		tmpstr[0] = UX_QUOTE_CHAR;
		tmpstr[1] = '\0';
		strcat (tmpstr, front);
		strcpy(front, tmpstr);
	}
	 
	 /* add front and back: */
#if UU_COMP != UU_VAXVMS
/*
......WNT use "\\" as path seperator
......Yurong 1/13/00
*/
/*#ifndef WNT  */
#if (UU_COMP != UU_WIN2K)
	 strcat(front, "/");    /* separator for UNIX systems only */
#else
	 strcat(front, "\\");
#endif
#endif
	 strcat(front, back);
	 strcpy(outpath,front); 
#if UU_COMP == UU_VAXVMS 
	if ((right = index(outpath,']')) != UU_NULL)
		if ( *(right+1) == '[' )
		{
			/* case of ][ form, replace by . if not already ][.     */
			if ( *(right+2) == '.' )
				strcpy(right,(right+2));
			else
			{
				*right = '.';
				strcpy((right+1),(right+2));
			}
		}
#endif

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}       
/*********************************************************************
**    E_FUNCTION :  ux_is_opsys_dep(path,options)
**                      
**                      Attempts to determine if "path" is an operating system
**                      dependent path name.
**              PARAMTERS:
**                      "path": A path name to determine if it is operating
**                              system dependent. On output, "pathname" may be the quoted
**                              system dependent pathname replacing a single quote
**                              specifications or a "." (current directory).
**                      "options": A bitwise "OR" of one choice in each of the 
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2.      UX_CHK: check the syntax of the path name.
**                                      UX_NCHK: don't check the syntax of the path
**                                              name.
**                              3. UX_QUOTES: any output system dependent path is returned 
**                                              with UX_QUOTE_CHAR surrounding it.
**                                      UX_NQUOTES: any output system dependent path name is returned
**                                              with UX_QUOTE_CHAR surrounding it.
**                      Incorrectly specified is the same as UX_PRTERRS | UX_NCHK | UX_QUOTES.
**              RETURNS:
**                              UU_SUCCESS if "path" is operating system dependent;
**                              otherwise, one of the following values is returned:
**                                      UX_NOPSYS: not operating system dependent;
**                                              (the following are considered errors)
**                                      UX_BAD_SUBJECT: "path" is syntactically incorrect for a
**                                                      path name;
**                                      UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_is_opsys_dep(path,options)
	UX_pathname path;
	int options;
{
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	UX_pathname dirname, curdir;
#if UU_COMP == UU_VAXVMS
	char *getenv();
	char *chrs;
#else
#if (UU_COMP == UU_RIDGE) || (UU_COMP == UU_WIN2K)
	char *getcwd();
	char *chrs;
#else
	char *getwd();
#endif
#endif
	char *nwhit;    /* temporary pointer for removing initial whitespace */

	uu_denter(UU_XTRC,(us,"ux_is_opsys_dep(path:%s,options:%d)",path,options));
	status = UU_SUCCESS; /* assume success */
	UX_CHK_PATHERRS(path, printit, options);

	nwhit = path;           /* initialize pointer to beginning to array path */
	while ((*nwhit == ' ') || (*nwhit == '\t'))
	/* pointing to initial whitespaces */
		++nwhit;                        /* bump pointer to point to next character */
	strcpy(path,nwhit);             /* replace pathname with initial white removed */
		
	if (*path == '\'')      /* single quote before string makes it literal */
	{
		/* remove single quote and quote the value  */
		path[0] = UX_QUOTE_CHAR;                 /* overwrite sigle quote */
		strcat(path,UX_QUOTE_STR);
	}
/*
......need consider "."
*/
/*
	else if ( strcmp(path,".") == 0 )
*/
	else if (( strcmp(path,".") == 0 )||( strcmp(path,"\".\"")== 0 ))
	{
#if UU_COMP == UU_VAXVMS
		chrs = getenv("PATH");
		strcpy(dirname,chrs);
#else
#if (UU_COMP == UU_RIDGE) || (UU_COMP == UU_WIN2K)
		chrs = getcwd(dirname,UX_MAX_PATH_LEN);
		strcpy(dirname,chrs);
#else
		getwd(dirname);
#endif
#endif
		/* replace with current directory and quote it */
		path[0] = UX_QUOTE_CHAR;
		path[1] = '\0';
		strcat(path,dirname);
		strcat(path,UX_QUOTE_STR);
	}
/*
......need consider ".."
*/
	else if (( strcmp(path,"..") == 0 )||( strcmp(path,"\"..\"")== 0 ))
	{
/*
......if it is "..", we can't return the current path, it maybe c:\nclnt\..\files, but
......if we return the current path, then it will be c:\nclnt\c:\user\xxx\files, that's wrong
......we just accept as it is and not return error
*/
/*********
#if UU_COMP == UU_VAXVMS
		chrs = getenv("PATH");
		strcpy(curdir,chrs);
#else
#if (UU_COMP == UU_RIDGE) || (UU_COMP == UU_WIN2K)
		chrs = getcwd(curdir,UX_MAX_PATH_LEN);
		strcpy(curdir,chrs);
#else
		getwd(curdir);
#endif
#endif
		chdir ("..");
#if UU_COMP == UU_VAXVMS
		chrs = getenv("PATH");
		strcpy(dirname,chrs);
#else
#if (UU_COMP == UU_RIDGE) || (UU_COMP == UU_WIN2K)
		chrs = getcwd(dirname,UX_MAX_PATH_LEN);
		strcpy(dirname,chrs);
#else
		getwd(dirname);
#endif
#endif
		chdir (curdir);
		path[0] = UX_QUOTE_CHAR;
		path[1] = '\0';
		strcat(path,dirname);
		strcat(path,UX_QUOTE_STR);
*/
	}
	else if (*path == UX_QUOTE_CHAR)
	{
	}
	else    /* none of the special cases, and not quoted */
		status = UX_NOPSYS;             /* and path isn't changed */
done:;
	uu_dexit;
	return(status);
}       
/*********************************************************************
**    E_FUNCTION : ux_chk_path_syntax(path,options) 
**                      If "pathname" is a quoted system dependent path name, then
**                      this function attempts to check if it is syntactically correct.
**                      If "pathname" is not system dependent, then its UNICAD file syntax 
**                      is checked. If the path name can be modified to produce a correct 
**                      path by minor modifications such as changing from lower
**                      case to upper case, then this is done. This function 
**                      can be used to check user input file names for 
**                      syntactic validity for the given host operating system.
**              PARAMTERS:
**                      "pathname": input:  path name to be checked;
**                              output: a possibly modified path name that is 
**                              syntactically correct.
**                      "options": A bitwise "OR" of one choice in each of the
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2. UX_QUOTES: any output system dependent path is returned 
**                                              with UX_QUOTE_CHAR surrounding it.
**                                      UX_NQUOTES: any output system dependent path name is returned
**                                              with UX_QUOTE_CHAR surrounding it.
**                                      Incorrectly specified is the same as UX_PRTERRS | UX_QUOTES.a
**              RETURNS:
**                      UU_SUCCESS if "pathname" is syntactically correct for the
**                      host operating system; otherwise, one of the following
**                      values is returned:
**                              UX_MODIFIED: "pathname" was modified and is now 
**                                      syntactically correct;
**                              UX_CANTFIX: "pathname" is still bad, not modified;
**                              UX_FAILURE: something else went wrong.
**                      
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_chk_path_syntax(path, options)
	UX_pathname path;
	int options;
{
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	UX_pathname name;
	int len;
#if UU_COMP!=UU_WIN2K
	int i;
#endif
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	strcpy(name,path);
	len = strlen(path);
/*
.....WinNT allow special characters as filename
*/
#if UU_COMP!=UU_WIN2K
	for (i=0; i<len; i++)
		if ( (name[i]=='!') || (name[i]=='@') || (name[i]==' '))
			goto failed;
#endif
#if UU_COMP!=UU_WIN2K
	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
#endif
	return(status);
}       
/*********************************************************************
**    E_FUNCTION :  ux_search_for_path(sympath,pathname,options)
**                      Attempts to return the first path name corresponding to sympath
**                      that is successfully acessed. This function for "sympath"s that 
**                      expand into multiple path names where the file must be found.
**              PARAMETERS:
**                      "sympath": A path name, can be either a UNICAD specification
**                              or a quoted system dependent path name.
**                      "pathname": returns the path name to the first file that exists.
**                      "options": A bitwise "OR" of one choice in each of the 
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2.      UX_CHK: check the syntax of the path name.
**                                      UX_NCHK: don't check the syntax of the path
**                                              name.
**                              3. UX_QUOTES: any output system dependent path is returned 
**                                              with UX_QUOTE_CHAR surrounding it.
**                                      UX_NQUOTES: any output system dependent path name is returned
**                                              with UX_QUOTE_CHAR surrounding it.
**                      Incorrectly specified is the same as UX_PRTERRS | UX_NCHK | UX_QUOTES.
**              RETURNS:
**                      UU_SUCCESS is returned if a file is found; otherwise,
**                      one of the following values is returned:
**                              UX_BAD_SUBJECT: "sympath" is bad;
**                              UX_NFOUND: can't find a file;
**                              UX_FAILURE: some thing else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_search_for_path(sympath,pathname,options)
	char *sympath;
	UX_pathname pathname;
	int options;
{
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL found, printit;
	char *pathlistptr = UU_NULL;
	char *listhead;
	UX_pathname noquote,tmpstr;

	uu_denter(UU_XTRC,(us,
			"ux_search_for_path(sympath:%s,?,options:%d",sympath,options));
	status = UU_SUCCESS; /* assume success */
	printit = (options != (options | UX_NPRTERRS)); /* default=print errors */

	if (ux_get_syspath(sympath,&pathlistptr,pathname,&found,options)!=UU_SUCCESS)
		goto failed; 
	listhead = pathlistptr;
	while (found != UU_FALSE)
	{
		strcpy(noquote,pathname);
		ux_strip_quotes(noquote);
		if (ux_access0(noquote,0) == UU_SUCCESS)
		{
			uu_lsdel(listhead);
			goto done;
		}
		else
		{
/*
......if the file path is '\nclnt\nclfiles' with diskpath, the above routine 
......will change it to '\nclnt\nclfiles', so try to add in '\' and try again
*/
			tmpstr[0] = UX_PATH_SEP;
			tmpstr[1] = '\0';
			strcat (tmpstr, noquote);
			if (ux_access0(tmpstr,0) == UU_SUCCESS)
			{
				if (options == (options | UX_NQUOTES)) 
					strcpy (pathname, tmpstr);
				else
				{
					strcpy(pathname, UX_QUOTE_STR);
					strcat(pathname, tmpstr);
					strcat(pathname, UX_QUOTE_STR);
				}
				uu_lsdel(listhead);
				goto done;
			}
			if (ux_get_syspath(sympath,&pathlistptr,pathname,&found,options)
				!=UU_SUCCESS)
				goto failed; 
		}
	}

failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}       
/*********************************************************************
**    E_FUNCTION : ux_add_farea_exten(fenv, pathname, options) 
**                      Attempts to add any file area extensions associated
**              with the environmental variable, "fenv", to "pathname".
**                      Such extensions are only added if there is no file area
**                      extension on the final file area identifier of "pathname". 
**                      We assume that "pathname" is a file area; no checking
**                      will be done to assure this.
**                      Note, if there is more than 1 file area extension given, 
**                      in "fenv", then only the first one is added.
**              PARAMETERS:
**                      "fenv": Environmental variable giving the file 
**                              area extension to be added to "pathname".
**                      "pathname": 
**                              Input: A full quoted system dependent path name to which a
**                                       file area is to be added.
**                              Output: The original full path name with a file area
**                                      extension.
**                              Note, the file area extension may not be at the 
**                              very end on some operating systems.
**                      "options": A bitwise "OR" of one choice in each of the 
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2.      UX_CHK: check the syntax of the path name.
**                                      UX_NCHK: don't check the syntax of the path
**                                              name.
**                      Incorrectly specified is the same as UX_PRTERRS | UX_NCHK | UX_QUOTES.
**              RETURNS:
**                      UU_SUCCESS is returned if a file area extension was 
**                      added; otherwise, one of the following is returned:
**                                      (the following are considered errors)
**                              UX_BAD_ENV: environmental variable not found.
**                              UX_BAD_SUBJECT: syntax of "fullpath" is bad;
**                              UX_FAILURE: something else went wrong.
**                      
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_add_farea_exten(fenv, pathname, options)
	char *fenv;
	UX_pathname pathname;
	int options;
{
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	UX_ftype farea;
	UX_ftype ftype;
	char *lq;
	char *rindex();
	char *index();
	UX_pathname alts;
#if UU_COMP == UU_VAXVMS
	char *fr;
	UX_pathname tempend;
	UX_pathname noquote;
#endif
	UU_LOGICAL printit;

	uu_denter(UU_XTRC,(us,"ux_add_farea_exten(fenv:%s,pathname:%s,options:%d)",
		fenv,pathname,options));
	status = UU_SUCCESS; /* assume success */
	UX_CHK_PATHERRS(pathname, printit, options);

	if ( uxi_get_suffixes(fenv, farea, ftype, alts, options) == UX_FAILURE)
	{
		status = UX_BAD_ENV;
		goto done;
	}
	
	/* intended to create /users/deb/subdir_ext and [users.deb.subdir_ext] */
	/* case of farea.dir should be changed to [] syntax first */

#if UU_COMP == UU_VAXVMS
	fr = rindex(pathname,']');
	if (fr != UU_NULL)
	{
		strcpy(tempend,"_");
		strcat(tempend,farea);
		strcat(tempend,fr);
		*fr = '\0';
		strcat(pathname,tempend);
	}
	else
	{
		strcpy(noquote,pathname);
		ux_strip_quotes(noquote);
		if ((fr = rindex(noquote,'.')) != UU_NULL)
		{
			/* then check for and remove .DIR on the filearea name */
			if ((strncmp(fr,".dir",4)==0) || (strncmp(fr,".DIR",4)==0))
				*fr = '\0';
		}
		/* pathname must be something simple, "symlib" */
		strcpy(tempend,"\"[");
		strcat(tempend,noquote);
		strcat(tempend,"_");
		strcat(tempend,farea);
		strcat(tempend,"]\"");
		strcpy(pathname,tempend);
	}
#else
	lq = rindex(pathname,UX_QUOTE_CHAR);
	if (lq != UU_NULL)
	{
		*lq = '\0';
		strcat(pathname,"_");
		strcat(pathname,farea);
		strcat(pathname,UX_QUOTE_STR);
	}
	else
	{
		strcat(pathname,"_");
		strcat(pathname,farea);
	}
#endif
done:;
	uu_dexit;
	return(status);
}       
/********************************************************************
**    E_FUNCTION : ux_add_ftype(ftype, pathname, options) 
**                      Attempts to add any file type extensions associated
**              with the environmental variable, "ftype", to "pathname".
**                      Such extensions are only added if there is no file type
**                      extension on the final file identifier of "pathname". 
**                      We assume "pathname" specifies a file not a file area;
**                      no checking will be done to assure this.
**                      Note, if there is more than 1 file area extension given, 
**                      in "ftype", then only the first one is added.
**              PARAMETERS:
**                      "ftype": Variable giving the file type extension to add to pathname.
**                      "pathname": 
**                              Input: A full quoted system dependent path name to which a
**                                       file type is to be added.
**                              Output: The original full path name with a file type
**                                      extension.
**                              Note, the file type extension may not be at the 
**                              very end on some operating systems.
**                      "options": A bitwise "OR" of one choice in each of the 
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2.      UX_CHK: check the syntax of the path name.
**                                      UX_NCHK: don't check the syntax of the path
**                                              name.
**                      Incorrectly specified is the same as UX_PRTERRS | UX_NCHK.
**              RETURNS:
**                      UU_SUCCESS is returned if a file area extension was 
**                      added; otherwise, one of the following is returned:
**                                      (the following are considered errors)
**                              UX_BAD_ENV: environmental variable not found
**                              UX_FIXED_EXT: path already has extension that was overwritten
**                              UX_FAILURE: something else went wrong.
**                              also UX_BAD_SUBJECT by the macro UX_CHK_PATHERRS
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_add_ftype(ftype, pathname, options)
	char *ftype;
	UX_pathname pathname;
	int options;
{
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	UX_pathname tempfv;
	char *fv, *fe, *fi;
	char *index();
	char *rindex();
	UU_LOGICAL printit;
	char *pathlistptr = UU_NULL;
	UU_LOGICAL found;
	UX_pathname outtype;

	uu_denter(UU_XTRC,(us,"ux_add_ftype(ftype:%s,pathname:%s,optns:%d)",
		ftype,pathname,options));
	status = UU_SUCCESS; /* assume success */
	UX_CHK_PATHERRS(pathname, printit, options);

	/* expand if needed, the variable "ftype" */
	if (ux_get_syspath(ftype,&pathlistptr,outtype,&found,(options|UX_NQUOTES))
		!= UU_SUCCESS)
	{
		status = UX_BAD_ENV;
		goto done;
	}
	uu_lsdel(pathlistptr);

	/* check for existing ftype extension, after last directory seperator */
#if UU_COMP != UU_VAXVMS
	fe = rindex(pathname,'/');
#else
	fe = rindex(pathname,']');
#endif
	if ( fe != UU_NULL )
	{
		if ( (fi = rindex(fe,'.')) != UU_NULL)
			/* there is already an extension on the filename */
			status = UX_FIXED_EXT;
	}
	else    /* no subdirectory in pathname */
	{
		if ( (fi = rindex(pathname,'.')) != UU_NULL)
			/* there is already an extension on the filename */
			status = UX_FIXED_EXT;
	}

	strcpy(tempfv,"\0");
	/* check for version number  or final quote character in file spec: */
	if ( (fv = index(pathname,';')) != UU_NULL)     /* designates version number */
	{
		strcpy(tempfv,fv);              /* move last part into temp buffer */
		*fv = '\0';             /* and truncate the pathname up to this part */
	}
	else
	{
		if ( (fv = rindex(pathname,UX_QUOTE_CHAR)) != UU_NULL) 
		{
			strcpy(tempfv,fv);              /* move last part into temp buffer */
			*fv = '\0';             /* and truncate the pathname up to this part */
		}
	}

	/* test for case of extension is already on the pathname */
	if (status == UX_FIXED_EXT)
	{
		if (strncmp((fi+1),outtype,(strlen(outtype))) ==0)
			status = UU_SUCCESS;
		else
		{
			/* fix the extension to be the one requested */
			*fi = '\0';
			strcat(pathname,".");           /* append the extension */
			strcat(pathname,outtype);
		}
	}
	else
	{
		strcat(pathname,".");           /* append the extension */
		strcat(pathname,outtype);
	}

	if ((*tempfv == ';') || (*tempfv == UX_QUOTE_CHAR)) 
		strcat(pathname,tempfv);                /* add back the version number or quote */

	goto done;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}       
/*********************************************************************
**    E_FUNCTION :  ux_decompose_path(name, farea,fname,options)
**                      Attempts to decompose the path name, "name" into the
**                      path name to the last file area in "name", and the
**                      file name proper. At this time it works by searching for
**                      the last seperator and chopping apart the string at this.
**              PARAMETERS:
**                      "name": A quoted system dependent path name to decompose.
**                      "farea": returns with the full path up to but not 
**                              including the actual file name.
**                      "fname": returns the file name.
**                      "options": A bitwise "OR" of one choice in each of the 
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2.      UX_CHK: check the syntax of the path name.
**                                      UX_NCHK: don't check the syntax of the path
**                                              name.
**                              3. UX_QUOTES: any output system dependent path is returned 
**                                              with UX_QUOTE_CHAR surrounding it.
**                                      UX_NQUOTES: any output system dependent path name is returned
**                                              with UX_QUOTE_CHAR surrounding it.
**                      Incorrectly specified is the same as UX_PRTERRS | UX_NCHK | UX_QUOTES.
**              RETURNS:
**                      UU_SUCCESS if all three values obtained; otherwise,
**                      one of the following values is returned:
**                              (the following are considered errors)
**                              UX_BAD_SUBJECT: bad syntax for "name";
**                              UX_FAILURE: something else went wrong.
**
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_decompose_path(name, farea, fname, options)
	char *name;
	UX_pathname farea;
	UX_pathname fname;
	int options;
{
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	UX_pathname pathname;
#if UU_COMP == UU_VAXVMS
	UX_pathname dirname;
#endif
	char *rindex();
	char *index();
	char *sepmark;
	char *lq;
	UX_pathname tempname;

	uu_denter(UU_XTRC,(us,"ux_decompose_path(name:%x,?,?,options:%d)",
			name,options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (name!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"name:%s", name));
	}
#endif
	status = UU_SUCCESS; /* assume success */
	UX_CHK_PATHERRS(name, printit, options);

	strcpy(pathname,name);

#if UU_COMP != UU_VAXVMS
	sepmark = rindex(pathname,UX_PATH_SEP);         /* get last seperator */
	if (sepmark != UU_NULL)
	{
		*sepmark = '\0';                /* place string terminator at path seperator */
		strcpy(farea,pathname);
		strcpy(fname,++sepmark);
	}
	else
	{
		strcpy(farea,"");
		strcpy(fname,"");
		goto failed;
	}
#else
	if ( ux_vaxdir0(pathname,dirname) != UU_SUCCESS)
		goto failed;
	if ((sepmark = rindex(dirname,']')) != UU_NULL)
	{
		if ( (*(sepmark+1)!='\0') && (*(sepmark+1)!=UX_QUOTE_CHAR) )
		{
			strcpy(fname,(sepmark+1));
			*(sepmark+1) = '\0';
			strcpy(farea,dirname);
		}
		else
		{
			strcpy(farea,"");
			strcpy(fname,"");
			goto failed;
		}
	}
	else
	{
		strcpy(farea,"");
		strcpy(fname,"");
		goto failed;
	}
#endif

	/* strings may be "farea and fname" after the split or, if not quoted to */
	/* begin with, farea and fname. Make "farea" & "fname" or farea & fname */
	if (options == (options|UX_NQUOTES))
	{
		if (*farea == UX_QUOTE_CHAR)
			strcpy(farea,(farea+1));
		lq = rindex(fname,UX_QUOTE_CHAR);
		if (lq != UU_NULL)
			*lq = '\0';
	}
	else
	{
		if (*farea != UX_QUOTE_CHAR)
		{
			strcpy(tempname,UX_QUOTE_STR);
			strcat(tempname,farea);
			strcpy(farea,tempname);
		}
		strcat(farea,UX_QUOTE_STR);

		lq = rindex(fname,UX_QUOTE_CHAR);
		if (lq == UU_NULL)
			strcat(fname,UX_QUOTE_STR);
		strcpy(tempname,UX_QUOTE_STR);
		strcat(tempname,fname);
		strcpy(fname,tempname);
	}

	goto done;
failed: status = UX_FAILURE;
done:;
	uu_dexit;
	return(status);
}       
/*********************************************************************
**    E_FUNCTION : int ux_get_base_fname(pathname, basename, options) 
**                      This function returns strips off all directory sequences
**                      of "pathname", and also strips off the suffix of the remaining
**                      portion of "pathname"; e.g. if pathname is: /this/is/fun.sy
**                      then fun would be returned in "basename".
**    PARAMETERS   
**       INPUT  : 
**       pathname:              a quoted system dependent pathname (no symbols)
**                      options: A bitwise "OR" of one choice in each of the 
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2.      UX_CHK: check the syntax of the path name.
**                                      UX_NCHK: don't check the syntax of the path
**                                              name.
**                      Incorrectly specified is the same as UX_PRTERRS | UX_NCHK | UX_QUOTES.
**       OUTPUT :  
**          basename            File base name.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_get_base_fname(pathname, basename, options)
	char *pathname;
	char *basename;
	int options;
{
	UX_pathname farea, fname;
	UX_pathname noquote;
	char *rindex();
	char *position;
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;

	uu_denter(UU_XTRC,(us,"ux_get_base_fname(%s,?,op:%d)",pathname,options));
	UX_CHK_PATHERRS(pathname, printit, options);
	status = UU_SUCCESS; /* assume success */

	if (ux_decompose_path(pathname,farea,fname,options|UX_NQUOTES|UX_NCHK)
		!= UU_SUCCESS)
	{
		strcpy(noquote, pathname);
		ux_strip_quotes(noquote);
		strcpy(basename, noquote);
	}
	else
	{
		strcpy(basename, fname);
	}

	/* take off any suffix */
	position = rindex(basename, '.');
	if (position != UU_NULL)        /* strip everything following */
		*position = '\0';
		
	/* check length, if the base name is too long, some machines truncatate */
	if (strlen(basename) > (UX_MAX_FILE_LEN - UX_SUFFIX_LEN))
		{
		goto failed;
		}
goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}       
/*********************************************************************
**    E_FUNCTION : int ux_get_base_farea(pathname, basename, options) 
**                      This function returns strips off all directory sequences
**                      of "pathname", and also strips off the farea suffix of the remaining
**                      portion of "pathname"; e.g. if pathname is: /this/is/fun_sy
**                      then fun would be returned in "basename".
**    PARAMETERS   
**       INPUT  : 
**       pathname:              a quoted system dependent pathname (no symbols)
**                      options: A bitwise "OR" of one choice in each of the 
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2.      UX_CHK: check the syntax of the path name.
**                                      UX_NCHK: don't check the syntax of the path
**                                              name.
**                              3. UX_QUOTES: any output system dependent path is returned 
**                                              with UX_QUOTE_CHAR surrounding it.
**                                      UX_NQUOTES: any output system dependent path name is returned
**                                              with UX_QUOTE_CHAR surrounding it.
**                      Incorrectly specified is the same as UX_PRTERRS | UX_NCHK | UX_QUOTES.
**       OUTPUT :  
**          basename            File base name.
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_get_base_farea(pathname, basename, options)
	char *pathname;
	char *basename;
	int options;
{
	char *rindex();
	char *position;
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	UX_pathname farea, fname;
	UX_pathname noquote;

	uu_denter(UU_XTRC,(us,"ux_get_base_farea(%s,?,op:%d)",pathname,options));
	UX_CHK_PATHERRS(pathname, printit, options);
	status = UU_SUCCESS; /* assume success */

	if (ux_decompose_path(pathname,farea,fname,options|UX_NQUOTES|UX_NCHK)
		!= UU_SUCCESS)
	{
		strcpy(noquote, pathname);
		ux_strip_quotes(noquote);
		strcpy(basename, noquote);
	}
	else
		strcpy(basename, fname);

	/* take off any filearea suffix: symlib_S.dir or symlib_S becomes symlib */
	position = rindex(basename, '_');
	if (position != UU_NULL)        /* strip everything following */
		*position = '\0';
		
	/* check length, if the base name is too long, some machines truncate */
	if (strlen(basename) > (UX_MAX_FILE_LEN - UX_SUFFIX_LEN))
		{
		goto failed;
		}
goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}       
/***********************************************************************
**    E_FUNCTION : ux_is_type_legal(pathname, fenv, options) 
**                      Attempts to determine if "pathname", is a legal pathname
**                      as far the UNICAD file type data in the environmental
**                      variable, "fenv", is concerned; i.e. does the file path 
**                      contain the file area(s) and file types in "fenv".
**              PARAMTERS:
**                      "pathname": Full quoted system dependent path name to check.
**                      "fenv": Environmental variable giving file type data to use
**                                      in checking "pathname".
**                      "options": A bitwise "OR" of one choice in each of the 
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2.      UX_CHK: check the syntax of the path name.
**                                      UX_NCHK: don't check the syntax of the path
**                                              name.
**                                      Incorrectly specified is the same as UX_PRTERRS | UX_NCHK.
**              RETURNS:
**                      UU_SUCCESS if files of the type specified by "fenv" can be included 
**                      in the file area; otherwise, one of the following values is
**                      returned:
**                                      (the following are considered errors)
**                              UX_BAD_TYPE: some file type extension on "pathname" is not
**                                              legal for the file area designated;
**                              UX_BAD_ENV: environmental variable was not found or not correct
**                                      file area extension for this pathname;
**                              UX_BAD_SUBJECT: "farea" is syntactically incorrect;
**                              UX_FAILURE: something else went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_is_type_legal(pathname, fenv, options)
	char *pathname;
	char *fenv;
	int options;
{
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;
	UX_pathname filearea;
	UX_pathname filename;
	UX_pathname alts, tempalts;
	UX_ftype ftype;
	UX_ftype farea;
	char *rindex();
	char *index();
	char *fext;
	char *nl, *nr;
	char *ft;

	uu_denter(UU_XTRC,(us,"ux_is_type_legal(pathname:%x,fenv:%s,options:%d)",
			pathname, fenv, options));
#ifdef UU_DEBUGON
	if (UU_debmask & UU_XTRC)
	{
		if (pathname!= UU_NULL)
			uu_dprint(UU_XTRC,(us,"pathname:%s", pathname));
	}
#endif
	UX_CHK_PATHERRS(pathname, printit, options);
	status = UU_SUCCESS; /* assume success */

	/* decompose the path at last identifier */
	if (ux_decompose_path(pathname,filearea,filename,(options|UX_NCHK|UX_NQUOTES))
		== UX_FAILURE)
		goto failed;

	/* search for _ and filea area extension */
	if ( (fext =  rindex(filearea,'_')) == UU_NULL)
		goto failed;

	/* search for . and filetype extension */
	if ( (ft = rindex(filename,'.')) == UU_NULL)
		goto failed;

	/* get fenv info from environmental variable value */
	if (uxi_get_suffixes(fenv,farea,ftype,alts,options) == UX_FAILURE )
	{
		status = UX_BAD_ENV;
		goto done;
	}

	/* compare for a match */
	if (strncmp(fext+1,farea,strlen(farea)) != 0)
	{
		status = UX_BAD_ENV;                    /* not the variable with this area extension */
		goto done;
	}

	if (strncmp(ft+1,ftype,strlen(ftype)) != 0) 
	{
		/* loop through any alternate types in "alts" string */
		while ( (nl = index(alts,UX_QUOTE_CHAR)) != UU_NULL)
		{
			nr = index(nl+1,UX_QUOTE_CHAR);
			strcpy(tempalts,nr+1);
			*nr = '\0';
			strcpy(ftype,nl+1);
			strcpy(alts,tempalts);          /*      lop off the first alternate type found */
			if (strncmp(ft+1,ftype,strlen(ftype)) == 0)             /* types match ok */
			goto done;
		}
		status = UX_BAD_TYPE;
	}

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}       
/***************************************************************
**    I_FUNCTION : uxi_get_suffixes(fenv, farea, ftype, alts, options) 
**              PARAMTERS:
**              RETURNS:
**                      UU_SUCCESS 
**                              UX_FAILURE: something else went wrong.
**    SIDE EFFECTS: none
**    WARNINGS: none
***************************************************************/
int uxi_get_suffixes(fenv,farea,ftype,alts,options)
	char *fenv;
	UX_ftype farea;
	UX_ftype ftype;
	UX_pathname alts;
	int options;
{
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	int nin;
	UX_pathname string;
	UX_pathname buff, outpath, inpath;
	char *index();
	char *pathlist;
	char *chr, *nextl;
	char *first, *second, *third, *fourth;
	char *ux_getenv();
	UU_LOGICAL found;

	uu_denter(UU_XTRC,(us,"uxi_get_suffixes(fenv:%s,?,?,options:%d)",
		fenv,options));
	status = UU_SUCCESS; /* assume success */

	chr = ux_getenv(fenv,options);
	if (chr == UU_NULL)
		goto failed;
	strcpy(string,chr);
	strcpy(buff,"");

	/* read string, to be "inpath", up to next space */
	while (UX_SSCANF0((string, "%s", inpath), nin) != EOF)
	{
		/* remove any intermediate whitespace */
		if ((*string == ' ' ) || (*string == '\t'))
			strcpy(string,string+1);
		strcpy(string,(string+strlen(inpath)));
		/* value is expanded and put, with its quotes, into buffer */
		pathlist = UU_NULL;
		if (ux_get_syspath(inpath,&pathlist,outpath,&found,(UX_PRTERRS|UX_QUOTES))
			!= UU_SUCCESS)
			goto failed;
		uu_lsdel(pathlist);             /* remove the list used by syspath call */
		strcat(buff,outpath);
	}
	/* this takes something like FEV = F1 F2 F3, F1 = "S", F2 = "sy", F3 = "dw":
	and returns "S""sy""dw" */

	first = index(buff,UX_QUOTE_CHAR);
	second = index(first+1,UX_QUOTE_CHAR);
	third = index(second+1,UX_QUOTE_CHAR);
	fourth = index(third+1,UX_QUOTE_CHAR);
	*fourth = '\0';
	strcpy(ftype,++third);
	*second = '\0';
	strcpy(farea,++first);

	if ( (nextl=index(fourth+1,UX_QUOTE_CHAR)) != UU_NULL)
		/* then there are more file types listed */
		strcpy(alts,nextl);
	else
		strcpy(alts,"");

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}       
/*********************************************************************
**       E_FUNCTION : int ux_test_prefix(prefix, fullpath, foundptr, options)
**               This function tests whether "prefix" represents an initial part of 
**               the path name "fullpath".
**       PARAMETERS     
**               INPUT: 
**                       prefix:                Prefix string, may be symbols.
**                       fullpath:       Path name to be checked for "prefix", may be symbols.
**                       options: A bitwise "OR" of one choice in each of the 
**                              following categories:
**                              1.      UX_PRTERRS: print any errors as specified by
**                                              uu_uerror functions.
**                                      UX_NPRTERRS: don't print any errors as 
**                                              specified by uu_uerror functions.
**                              2.      UX_CHK: check the syntax of the path name.
**                                      UX_NCHK: don't check the syntax of the path
**                                              name.
**                                      Incorrectly specified is the same as UX_PRTERRS | UX_NCHK.
**               OUTPUT :  
**                       foundptr        Pointer to UU_SUCCESS iff "prefix" is a representation 
**                                                      for an initial segment of "fullpath".
**       RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**                               UX_FAILURE is returned if either of the path names are found
**                               illegal.
**       SIDE EFFECTS: none
**       WARNINGS: none
*********************************************************************/
int ux_test_prefix(prefix, fullpath, foundptr, options)
	char *prefix;
	char *fullpath;
	int *foundptr;
	int options;
{
	UX_pathname newprefix, newfullpath;
	int len;
	int n;
	UU_LOGICAL foundexp;    
	char *list = UU_NULL;
	char *index();
	char *rindex();
	char *lq, *fq;
	int status;              /* return status; either UU_SUCCESS or UX_FAILURE */
	UU_LOGICAL printit;

	uu_denter(UU_XTRC,(us,
		"ux_test_prefix(prefix:%s,fullpath:%s,?,options:%d)",
		prefix,fullpath,options));

	UX_CHK_PATHERRS(fullpath, printit, options);
	status = UU_SUCCESS; /* assume success */
	*foundptr = UU_FAILURE; /* prefix not found yet */

	/* expand completely the strings */
	if (ux_get_syspath(prefix,&list,newprefix,&foundexp,UX_PRTERRS|UX_NCHK)
		!= UU_SUCCESS) goto failed;
	else
		uu_lsdel(list);
	list = UU_NULL;
	if (ux_get_syspath(fullpath,&list,newfullpath,&foundexp,UX_PRTERRS|UX_NCHK)
		!= UU_SUCCESS) goto failed;
	else
		uu_lsdel(list);
/*
.....Remove quotes and make comparison using upper case.  Using
.....upper case ignores any discrepencies in paths that do not matter
.....for current supported systems - ASF 8/1/13.
*/
	fq = index(newprefix,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(newprefix,(fq+1));
			lq = rindex(newprefix,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	fq = index(newfullpath,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(newfullpath,(fq+1));
			lq = rindex(newfullpath,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
	}
	len = strlen(newprefix) - 1;
	if (len >= 0)
	{
		newfullpath[len] = newprefix[len] = '\0';
		n = ul_compare_upper(newfullpath,newprefix);
		//strncmp(newfullpath,newprefix,len);
	}
	else n = 0;
	if (n == 0)
		/* we have located complete prefix string in the full path name */
		*foundptr = UU_SUCCESS;

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  
/*********************************************************************
**    E_FUNCTION : ux_strip_quotes
**                      If quote characters are found around the name, they are removed.
**                      
**    PARAMETERS   
**       INPUT  : 
**                              name: possible quoted system dependent pathname 
**       OUTPUT :  
**                              name: non-quoted system dependent pathname 
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_strip_quotes(name)
	char *name;
{
	int status;                     /* return status; either UU_SUCCESS or UX_FAILURE */
	UX_pathname noquote;
	char *lq, *fq;
	char *index();
	char *rindex();

	uu_denter(UU_XTRC, (us, "ux_strip_quotes(name:%s)",name));
	status = UU_SUCCESS; /* assume success */

	/* remove quotes to get working pathname: */
	fq = index(name,UX_QUOTE_CHAR);
	if (fq != UU_NULL)
	{
			strcpy(noquote,(fq+1));
			lq = rindex(noquote,UX_QUOTE_CHAR);
			if (lq != UU_NULL)
				*lq = '\0';
			else
				goto failed;
			strcpy(name,noquote);
	}
	/* else a quote character wasn't found; leave name as is */

	goto done;
failed: status = UX_FAILURE;
#if (TRACE) 
	UX_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
}       
/*********************************************************************
**    E_FUNCTION :  int uxi_get_legal_name(name, modifiedptr)
**       This function determines whether the  "name" is 
**       valid or not; we also take out blanks; currently, valid characters
**       are 'a' thru 'z', 'A' thru 'Z', '0' thru '9', '.', '_' '()'.
**    PARAMETERS   
**       INPUT  : 
**          name                    
**       OUTPUT :  
**          name            Modified name; i.e. without blanks.
**          modifiedptr     Returns with UU_TRUE iff "name" was modified.
**    RETURNS: UU_SUCCESS if no problems encountered; UX_FAILURE otherwise;
**             i.e. UX_FAILURE when name is not legal and is not modified to 
**             a legal name.
**    SIDE EFFECTS: none
**    WARNINGS: Note, no errors are printed here.
*********************************************************************/
int uxi_get_legal_name(name, modifiedptr)
	char name[];
	UU_LOGICAL *modifiedptr;
{
	int i, j;
	UU_LOGICAL foundblank;
	UX_pathname basic;
	int nbrblanks1, nbrblanks2;
	int len;
	int status;                     /* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_XTRC, (us, "uxi_get_legal_name(%s,?)", name));
	status = UU_SUCCESS; /* assume success */
	*modifiedptr = UU_FALSE;

	/* delete any trailing or leading blanks */
	len = strlen(name);
	for (i=0, foundblank=UU_TRUE, nbrblanks1=0; (i<len && foundblank); i++)
		if (name[i] == ' ')
			nbrblanks1++;
		else
			foundblank = UU_FALSE;
	if (nbrblanks1 == len) /* then had all blanks */
		goto failed;

	/* "i" is the index of the character after the first nonblank character */
	for (j=len-1, foundblank=UU_TRUE, nbrblanks2=0; (j>=i && foundblank); j--)
		if (name[j] == ' ')
			nbrblanks2++;
		else
			foundblank = UU_FALSE;

	if ( (nbrblanks1+nbrblanks2) != 0)
		*modifiedptr = UU_TRUE;

	uu_move_byte(&(name[nbrblanks1]), name, len-(nbrblanks1+nbrblanks2));
	name[len-(nbrblanks1+nbrblanks2)] = '\0';

	/* now check for illegal characters inside of name */
	len = strlen(name);
	for (i=0; i<len; i++)
		if ( (('a'<=name[i]) && (name[i]<='z')) || 
			  (('A'<=name[i]) && (name[i]<='Z')) ||
			  (('0'<=name[i]) && (name[i]<='9')) ||
	    		name[i]=='_' || name[i]=='.' ||
	    		name[i]=='(' || name[i]==')')
			  ;
		else if (name[i] == ' ') /* then replace with an '_' */
		{
#if UU_COMP!=UU_WIN2K
				name[i] = '_';
				*modifiedptr = UU_TRUE;
#else
				;
#endif
		}
		else goto failed;

	/* check the base file name for length limit: */
	if (ux_get_base_fname(name, basic, UX_NQUOTES|UX_PRTERRS) != UU_SUCCESS)
		goto failed;

#if UU_COMP == UU_VAXVMS
	UX_TO_UPPER(name)
#endif
	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}       
/*********************************************************************
**    E_FUNCTION :  int ux_accept_name(name)
**                      This function determines whether the  "name" is 
**                      acceptable input, general to all subsystems that handle
**                      file naming. Note that symbol names must pass the syntax
**                      checking of routine uxi_get_legal_name.
**    PARAMETERS   
**       INPUT  : 
**                              name                    
**       OUTPUT :  
**    RETURNS: UU_SUCCESS if no problems encountered; UX_FAILURE otherwise;
**                                      i.e. UX_FAILURE when name is not legal (base filename is
**                                      too long or doesn't pass the UX_CHK_PATHERRS syntax rules.
**    SIDE EFFECTS: none
**    WARNINGS:
*********************************************************************/
int ux_accept_name(name)
	char name[];
{
	UX_pathname base;
	UU_LOGICAL printit;
	int status;                     /* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_XTRC, (us, "ux_accept_name(%s)", name));
	status = UX_CHK_PATHERRS(name, printit, UX_CHK | UX_PRTERRS);
	if (status == UX_BAD_SUBJECT)   /* put out an error about syntax */
		UX_ERROR0(25,printit);

	status = UU_SUCCESS; /* assume success */
	if (ux_get_base_fname(name,base,UX_NCHK|UX_NQUOTES) != UU_SUCCESS)
	{
		UX_ERROR0(24,printit);          /* put out an error about length */
		goto failed;
	}

	goto done;
failed: status = UX_FAILURE;
	UX_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}       

/*********************************************************************
**    E_FUNCTION :  int ux_getsys_ext(envlst,txtlst)
**                      checking of routine uxi_get_legal_name.
**    PARAMETERS   
**       INPUT  : 
**                              name                    
**       OUTPUT :  
**    RETURNS: UU_SUCCESS if no problems encountered; UX_FAILURE otherwise;
**                                      i.e. UX_FAILURE when name is not legal (base filename is
**                                      too long or doesn't pass the UX_CHK_PATHERRS syntax rules.
**    SIDE EFFECTS: none
**    WARNINGS:
*********************************************************************/
int ux_getsys_ext(llen,envlst,txtlst)
int llen;
UX_pathname envlst[];
UX_pathname txtlst[];
{
 char *tlistptr;
 int status,found,i;

 status = 0;
 tlistptr = 0;
 for (i=0; i<llen; i++)
  { 
		if (NCLX_external_unibase)
		{
			if (strcmp(envlst[i],"UR_UNB_FILE") == 0)
				strcpy(txtlst[i],"u");
			else if (strcmp(envlst[i],"UR_ASCII_PART") == 0)
				strcpy(txtlst[i],"ud");
			else
				strcpy(txtlst[i],envlst[i]);
		}
		else
		{

  			if  (ux_get_syspath(envlst[i],&tlistptr,txtlst[i],&found,
			     (UX_NPRTERRS|UX_NQUOTES)) != UU_SUCCESS)
			{
				status = UX_BAD_ENV;
				goto l_p99;
			}
		}
#if UU_COMP == UU_VAXVMS
     ul_to_upper(txtlst[i]);
#endif
   if (tlistptr != 0) uu_lsdel(tlistptr);
   tlistptr = 0;
  }
l_p99:
 return(status);
}
