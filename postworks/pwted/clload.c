/*********************************************************************
**  NAME:  clload.c  
**  Description:
**				Function for load/save cl file
**    CONTAINS:
**			Ptd_clload(char *filename, char **outdata)
**			Ptd_clsave(char*clafile, char*clfile, int *kerr)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       clload.c , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:59:19
*********************************************************************/
#include <stdio.h>
#include <string.h>
#include "PtdFunc.h"

#ifndef WNT
#ifndef VAXVMS
#ifndef HP
#ifndef IBM
#define ptd_clopen			ptd_clopen_
#define ptd_cltocla			ptd_cltocla_
#define ptd_clclose			ptd_clclose_
#define ptd_clatocl			ptd_clatocl_	 
#define init_load				init_load_



#endif
#endif
#endif
#endif

/*********************************************************************
**    E_FUNCTION     : Ptd_clload(char *filename, char **outdata)
**       Load a binary cl file and saved in outdata string as text
**
**    PARAMETERS
**       INPUT  : filename: cl file to load
**       OUTPUT : outdata
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
Ptd_clload(char *filename, char **outdata)
{
	int krec,kpt, nc, kerr, flag, out_size;
	char ctape[100000], out_line[80];

	nc = strlen(filename);

	ptd_clopen(filename, &nc);

	Ptd_cllist_init();
	kerr = 0;
	krec = 1;
	kpt = 0;
/*
.....before convert, reset default value
*/
	init_load();
	while (kerr==0)
	{
		ptd_cltocla (&krec,&kpt, ctape, &nc, &flag, &kerr);
 		if (kerr==1) break;
		if (nc > 0)
		{
			if (nc < 73 || flag == 3 || flag == 4)
			{
				Ptd_cllist_push_multiple (nc, ctape);
				if (flag!=3)
					Ptd_cllist_push_eol ();
				if (flag==0) break;
			}
			else
			{
				out_size = break_lines(nc, ctape, out_line);
				if (out_size > 0)
				{
					Ptd_cllist_push_multiple (out_size, out_line);
					Ptd_cllist_push_eol ();
				}
				else
				{
					Ptd_cllist_push_multiple (nc, ctape);
					if (flag!=3)
						Ptd_cllist_push_eol ();
				}
				if (flag==0) break;
			}
		}
	}
	Ptd_cllist_finish (outdata);
	ptd_clclose();
	return 0;
}


/*********************************************************************
**    E_FUNCTION     : Ptd_clsave(char*clafile, char*clfile, int *kerr)
**      save a text cla file as binary cl file 
**
**    PARAMETERS
**       INPUT  : clafile: text cla file
**						clfile: binary cl file
**       OUTPUT : kerr
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
void Ptd_clsave(char*clafile, char*clfile, int *kerr)
{
	int nc1, nc2;
	nc1 = strlen(clafile);
	nc2 = strlen(clfile);
	*kerr = 0;
	ptd_clatocl(clafile, &nc1, clfile, &nc2, kerr);
}


/*********************************************************************
**    E_FUNCTION     : break_lines(int nc, char ctape[], char out_line[])
**      Break up mulitple line statement when going from cl to cla. 
**
**    PARAMETERS
**       INPUT		nc         	number of characters to output
**					ctape      	statement to output
**
**       OUTPUT :	out_line	final string to output
**
**    RETURNS      : number of characters in out_line
**                   -1 if error
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int break_lines(int nc, char*ctape, char*out_line)
{
	int i, j, bad_line, out_size, first, num_remaining, textcom;
	char temp_str[80], *line_pos, tmptape[100000];
	const char slash = '/', dollar = '$', space = ' ';
	size_t starting_line_pos, line_size, total_written;

	bad_line = 0;
	first = 0;
/*
.....Start all following lines in statement after the slash in the first line.
*/
/*
.....Won't work on IRIX 5.3 (saturn).  Do it the hard way.
	starting_line_pos = strcspn(ctape, &slash);
*/
	for (i = 0; i < 72; i++)
	{
		if (ctape[i] == slash)
		{
			starting_line_pos = i + 1;
			break;
		}
	}
	if ((starting_line_pos > 72) || (starting_line_pos < 1)) bad_line = 1;
	textcom = 0;
	if (starting_line_pos-1-6>0)
	{
		if ((strncmp(&(ctape[starting_line_pos-7]), "INSERT", 6)==0)
			|| (strncmp(&(ctape[starting_line_pos-7]), "PPRINT", 6)==0)
			|| (strncmp(&(ctape[starting_line_pos-7]), "TPRINT", 6)==0)
			|| (strncmp(&(ctape[starting_line_pos-7]), "REMARK", 6)==0)
			|| (strncmp(&(ctape[starting_line_pos-7]), "LETTER", 6)==0)
			|| (strncmp(&(ctape[starting_line_pos-7]), "PARTNO", 6)==0))
		{
			textcom = 1;
		}
	}
	if (textcom==1)
	{
/*
.....here try to remove "~" with newline character immidiately follow in which
.....the "~' added at end of line because of the long too long
*/
		for (i=0, j=0; i<nc; i++, j++)
		{
			if (ctape[i]=='~')
			{
				if (ctape[i+1]=='\n')
					i += 2;
				else if ((ctape[i+1]=='\r')&&(ctape[i+2]=='\n'))
					i += 3;
			}
			tmptape[j] = ctape[i];
		}
		tmptape[j] = '\0';
		nc = j;
		strcpy(ctape, tmptape);
	}
	strncpy(temp_str, "", 80);
	strncpy(temp_str, ctape, 71);
	if (temp_str == NULL) bad_line = 1;
/*
.....Dont' split lines that
.....have already been split
*/
	if (strchr(temp_str,'\n') != 0) bad_line = 1;
/*
.....Go thru statement breaking it up into pieces less than 72 char long.
*/			
	line_size = starting_line_pos;
	total_written = 0;
	while (((first*starting_line_pos + nc) > 72) && (bad_line == 0))
	{
/*
.....Scan backwards down the substring until found delimiter.
.....Want to break lines after delimiter (ie. don't break up numbers or
.....post words).
.....If first time thru, first*starting_line_pos = 0.  Otherwise, it
.....represents the space used in formatting the output so that additional
.....lines in a statement start even with the "/" character in the first
.....line.
.....Make sure that there are no funny breaks before the "/".  For example,
.....N100  ,  REMARK/  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
.....There were no delimiters after the "/", so the comma before it was 
.....returned.  I don't want to break on that.
*/
		line_size = scan_backwards_for_delimiters(temp_str, 0, \
                                       first*starting_line_pos);
		if ((line_size > 0) && (line_size > starting_line_pos))
		{
			if (first == 0)
			{
				strncpy(out_line, "", 72);
				strncpy(out_line, temp_str, line_size);
				first = 1;
			}
			else
			{
				line_pos = out_line;
				for (i = 0; i < starting_line_pos; i++)
				{
					strcpy(line_pos, " ");
					line_pos++;
				}
				strncpy(line_pos, "", (80 - starting_line_pos));
				strncpy(line_pos, temp_str, line_size);
				strcat(out_line, "\0");
			}
		}
		else
		{
/*
.....Can't find where to end the line.
			bad_line = 2;
*/
			for (i = 71; i > 0; i--)
			{
//
				if (ctape[i] != space)
				{
					line_size = i + 1;
					break;
				}
			}
			strncpy(out_line, "", 72);
			if (line_size>71) 
			{
				line_size = 71;
				starting_line_pos = 0;
			}
			strncpy(out_line, temp_str, line_size);
			if (first == 0)
				first = 1;
		}
/*
.....Check if screwed up the creatation of the output.
*/
		if (out_line == NULL) bad_line = 3;
		out_size = strlen(out_line);
		if ((out_line[out_size-1] != '$') && ( out_line[out_size-1] != '~'))
		{
			if (textcom==1)
				strcat(out_line, "~");
			else
				strcat(out_line, "$");
		}
/*
.....IMPORTANT PART!!!        PUSH INFO INTO CL FILE LIST.
*/
		if (bad_line == 0)
		{
			out_size = strlen(out_line);
			Ptd_cllist_push_multiple (out_size, out_line);
			Ptd_cllist_push_eol ();
/*
.....Set up for next iteration.
*/
			total_written += line_size;
/*
.....check the new line chararcters just after the new written line
*/
			while ((ctape[total_written]=='\r') || (ctape[total_written]=='\n'))
			{
				total_written++;
				line_size++;
			}
			strncpy(temp_str, (ctape+total_written), 72);
			nc -= line_size;
			line_size = 0;
		}
	}
/*
.....Get last section of multiple line statement.
*/
	num_remaining = 0;
	if ((first == 0) && (bad_line == 0))
	{
		strncpy(out_line, temp_str, nc);
		num_remaining = nc;
	}
	else if (bad_line == 0)
	{
		line_pos = out_line;
		for (i = 0; i < starting_line_pos; i++)
		{
			strcpy(line_pos, " ");
			line_pos++;
		}
		strncpy(line_pos, temp_str, nc);

		num_remaining = nc + starting_line_pos;
	}
	else
	{
		out_line = NULL;
		num_remaining = -1;
	}

	return (num_remaining);
}

/*********************************************************************
**    E_FUNCTION     : scan_backwards_for_delimiters(char*str_in, \
**                                      int num_char, int starting_pos)
**
**      Take an input string and an index to start at (position in the
**      input string to start the scan), find the position in the string
**      of the delimiter closest to 72 characters.  The delimiters used
**      are "$" and ",".  The starting position is counted with the 72
**      characters.
**
**      The idea is that there are statements that are more than 72 
**      characters long and must be broken up.  This braking up must not
**      break up numbers or vocabulary words, so you can't just break
**      when reaching 72.  You must scan backwards down the string until
**      a delimiter is found.  That is the appropriate place to break.
**
**      For example:
**                                   72
**           xxxxxxxxxxxxxxxxxxx , OFFSET
**      Don't break up the word 'OFFSET'; instead find the comma that
**      preceded it.
**                                   72
**           xxxxxxxxxxxxxxxxxx,$
**      There is a line break expected at the line-continuation char '$',
**      so that is the natural place to break.
**
**      The starting line position is for formatting the output of these
**      multiline statements.  It is the position of the "/" at the
**      beginning of a command.  All following lines in the statement
**      should start even with the "/".  This means that there are 
**      extra blank characters taking up space at the beginning of each
**      following line in the statement.
**      The first line of the statement has starting_pos = 0.
**
**      For example:
**           N11   , CYCLE/ SHIFT, xxxxxxxxxxxxx   ->  starting_pos = 0
**                        0.  ,  0.  , 1.          ->  starting_pos = 13
**
**    PARAMETERS
**       INPUT  : str_in     	The input string
**                str_index 	Place in string from which to start the search.
**                starting_pos	formatting offset adding extra char to line
**
**       OUTPUT : None
**
**    RETURNS      : Position in input string to break the line or
**                     negative number for error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
************************************************************************/
int scan_backwards_for_delimiters(char*str_in, int str_index, int starting_pos)
{
	int answer, error_flag;
	char temp_str[80], *end_line, *end_line_c, *end_line_d, *end_line_t;
//	const char comma = ',', dollar = '$', txtd = '~';
	const char delim[] = ",$~";
	size_t curr_pos, last_pos, line_size;
/*
.....Get local copies of string and things.  Check for errors in input.
*/
	strcpy(temp_str, str_in);
	line_size = strlen(temp_str);
	if (line_size < 1) return (-1);
	if ((starting_pos > 71) || (starting_pos < 0)) return (-2);

	error_flag = 0;
	curr_pos = 0;
	while (((starting_pos + curr_pos) <= 72) && (error_flag == 0))
	{
		last_pos = curr_pos;
		end_line = strpbrk((temp_str+curr_pos), delim);
		if (end_line == NULL) break;
/*
.....Make it (ending_pos - starting_pos) + 1.  The "+ 1" part is to skip
.....the delimiter that was just found.
*/
		curr_pos = end_line - temp_str + 1;
		if (curr_pos < 0) error_flag = 1;
	}

	answer = 0;
	if ((curr_pos > 0) && ((curr_pos + starting_pos) <= 72))
		answer = curr_pos;
	else if ((last_pos > 0) && ((last_pos + starting_pos) <= 72))
		answer = last_pos;
	else if (line_size <= 72)
		answer = line_size;
	else
		answer = -3;

	return (answer);
}

