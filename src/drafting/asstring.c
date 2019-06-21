
/*********************************************************************
**    NAME         :  asstring.c
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       asstring.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:40
*********************************************************************/

/*********************************************************************
**    E_FUNCTION :  int ua_get_next_line (in_string)
**       Gets the next line from a character string and returns the 
**			input string minus the next line in the string.  A newline
**			character is used as the line terminators so, this function
**			searches for the first newline and returns the characters
**			up to the first newline character.
**    PARAMETERS   
**       INPUT  : 
**          in_string	- Input character string to get the next
**								  line from.
**
**       OUTPUT :  
**          in_string	-	Input character string less the returned line.
**				nxt_line		-	The next line in the input string.
**
**    RETURNS      : Status flag - 1 = ok, 0 = no more lines in buffer
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_get_next_line(in_string, char_pos, nxt_line)
char *in_string;
int *char_pos;
char *nxt_line;

{

	in_string = in_string + *char_pos;
	*char_pos = *char_pos + 1;
	while (*in_string != '\n')
	{
		*char_pos = *char_pos + 1;
		if (*in_string == '\0') return(0);
		*nxt_line = *in_string;
		*nxt_line;
		nxt_line++;
		in_string++;
	}

	*nxt_line = '\0';
	return(1);

}
