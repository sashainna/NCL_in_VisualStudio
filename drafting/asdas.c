
/*********************************************************************
**    NAME         :  asdas.c
**       CONTAINS:
**       ud_world_coord
**			ud_nd_coord
**			ud_vector
**			ud_distance
**			ud_unitless
**			ud_integer
**			ud_angle
**			ud_string
**			ud_string_def
**			ud_selection
**			ud_pick_loc
**			ud_choice
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       asdas.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:39
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "dasnog.h"
#include "asalc.h"


/*********************************************************************
**	R_FUNCTION:	integer ud_world_coord (subsystem, prompt, coords, numcoords,
**													outcoords, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get world
**			coordinates from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  COORD:		coords		-	Coordinate buffer
**			  INTEGER:	numcoords	-	Size of 'coords' 
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'coords'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outcoords	-	Number of coordinates returned in 'coords'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_world_coord(subsystem, prompt, coords, numcoords, outcoords, dfault_flag)

int	subsystem, prompt, numcoords, dfault_flag;
int		*outcoords;

US_COORD coords[];

{
    UD_NDCLOCREC tmp[100];
	int		retcode,i;

	uu_denter(UU_STRC,(us,"ud_world_coord(%d,%d,,%d,,%d)",
							subsystem,prompt,numcoords,dfault_flag));
	if (dfault_flag)
		retcode = ud_ldas(UD_DASCART, subsystem, prompt, tmp, numcoords, outcoords,
				  UD_DEFAULT);
	else
		retcode = ud_ldas(UD_DASCART, subsystem, prompt, tmp, numcoords, outcoords,
				  UD_NODEFAULT);
    for(i=0; i <*outcoords; i++)
    {
        coords[i].x = tmp[i].cord[0];
        coords[i].y = tmp[i].cord[1];
        coords[i].z = tmp[i].cord[2];
    }
    

	uu_denter2(UU_STRC,(us,"ud_world_coord(,,<%g,%g,%g>,,%d)"
		,coords[0].x,coords[0].y,coords[0].z,*outcoords));
	uu_dexit;
	uu_denter2(UU_STRC,(us,"ud_world_coord return(%d)",retcode));
	uu_dexit;
	uu_dexit;
	return(retcode);
}

/*********************************************************************
**	R_FUNCTION:	integer ud_nd_coord (subsystem, prompt, coords, numcoords,
**												outcoords, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get normalized
**			device coordinates from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  COORD:		coords		-	Coordinate buffer
**			  INTEGER:	numcoords	-	Size of 'coords' 
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'coords'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outcoords	-	Number of coordinates returned in 'coords'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_nd_coord(subsystem, prompt, coords, numcoords, outcoords, dfault_flag)

int		subsystem, prompt, numcoords, *outcoords, dfault_flag;

US_COORD coords[];

{

	if (dfault_flag)
		return(ud_ldas(UD_DASNDC, subsystem, prompt, coords, numcoords, outcoords,
				  UD_DEFAULT));
	else
		return(ud_ldas(UD_DASNDC, subsystem, prompt, coords, numcoords, outcoords,
				  UD_NODEFAULT));
}

/*********************************************************************
**	R_FUNCTION:	integer ud_vector (subsystem, prompt, vector, numvects,
**											 outvects, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get vector
**			from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  COORD:		vector		-	Vector buffer
**			  INTEGER:	numvects		-	Size of 'vector' 
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'vector'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outvects	-	Number of vectors returned in 'vector'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_vector(subsystem, prompt, vector, numvects, outvects, dfault_flag)

int		subsystem, prompt, numvects, *outvects, dfault_flag;

US_COORD vector[];

{

	if (dfault_flag)
		return(ud_ldas(UD_DASVEC, subsystem, prompt, vector, numvects, outvects,
				  UD_DEFAULT));
	else
		return(ud_ldas(UD_DASVEC, subsystem, prompt, vector, numvects, outvects,
				  UD_NODEFAULT));
}

/*********************************************************************
**	R_FUNCTION:	integer ud_distance (subsystem, prompt, dist, numdists,
**												outdists, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get a distance
**			from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  REAL:		dist			-	distance values buffer
**			  INTEGER:	numdists		-	Size of 'dist' 
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'dist'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outdists	-	Number of distance values returned in 'dist'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_distance(subsystem, prompt, dist, numdists, outdists, dfault_flag)

int		subsystem, prompt, numdists, *outdists, dfault_flag;
UU_REAL	dist[];

{

	if (dfault_flag)
		return(ud_ldas(UD_DASDISTANCE, subsystem, prompt, dist, numdists, outdists,
				  UD_DEFAULT));
	else
		return(ud_ldas(UD_DASDISTANCE, subsystem, prompt, dist, numdists, outdists,
				  UD_NODEFAULT));
}

/*********************************************************************
**	R_FUNCTION:	integer ud_unitless (subsystem, prompt, value, numvalues,
**												outvalues, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get a unitless
**			value from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  REAL:		value			-	values buffer
**			  INTEGER:	numvalues	-	Size of 'value' 
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'value'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outvalues -	Number of distance values returned in 'value'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_unitless(subsystem, prompt, value, numvalues, outvalues, dfault_flag)

int		subsystem, prompt, numvalues, *outvalues, dfault_flag;
UU_REAL	value[];

{

	if (dfault_flag)
		return(ud_ldas(UD_DASUNITLESS, subsystem, prompt, value, numvalues, outvalues,
				  UD_DEFAULT));
	else
		return(ud_ldas(UD_DASUNITLESS, subsystem, prompt, value, numvalues, outvalues,
				  UD_NODEFAULT));
}

/*********************************************************************
**	R_FUNCTION:	integer ud_integer (subsystem, prompt, integers, numints,
**											  outints, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get integer values
**			from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  INTEGER:	integers		-	integer values buffer
**			  INTEGER:	numints		-	Size of 'integers' 
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'integers'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outints	-	Number of integer values returned in 'integers'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_integer(subsystem, prompt, integers, numints, outints, dfault_flag)

int		subsystem, prompt, numints, *outints, dfault_flag;
int		integers[];

{

	if (dfault_flag)
		return(ud_ldas(UD_DASINT, subsystem, prompt, integers, numints, outints,
				  UD_DEFAULT));
	else
		return(ud_ldas(UD_DASINT, subsystem, prompt, integers, numints, outints,
				  UD_NODEFAULT));
}

/*********************************************************************
**	R_FUNCTION:	integer ud_angle (subsystem, prompt, angle, numangs,
**												outangs, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get an angle
**			in degrees from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  REAL:		angle			-	Angle values buffer
**			  INTEGER:	numangs		-	Size of 'angle' 
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'angle'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outangs	-	Number of coordinates returned in 'angle'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_angle(subsystem, prompt, angle, numangs, outangs, dfault_flag)

int		subsystem, prompt, numangs, *outangs, dfault_flag;
UU_REAL	angle[];

{

	if (dfault_flag)
		return(ud_ldas(UD_DASANGLE, subsystem, prompt, angle, numangs, outangs,
				  UD_DEFAULT));
	else
		return(ud_ldas(UD_DASANGLE, subsystem, prompt, angle, numangs, outangs,
				  UD_NODEFAULT));
}

/*********************************************************************
**	R_FUNCTION:	integer ud_string (subsystem, prompt, string, numchars,
**											 outchars, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get a character
**			string from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  CHAR:		string		-	Character string buffer
**			  INTEGER:	numchars		-	Size of 'string' 
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'string'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outchars		-	Number of characters returned in 'string'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_string(subsystem, prompt, string, numchars, outchars, dfault_flag)

int		subsystem, prompt, numchars, *outchars, dfault_flag;
char		string[];

{

	if (dfault_flag)
		return(ud_ldas(UD_DASSTRING, subsystem, prompt, string, numchars, outchars,
				  UD_DEFAULT));
	else
		return(ud_ldas(UD_DASSTRING, subsystem, prompt, string, numchars, outchars,
				  UD_NODEFAULT));
}

/*********************************************************************
**	R_FUNCTION:	integer ud_string_def (subsystem, prompt, string, numchars,
**											 outchars, termstat)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get a character
**			string from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  CHAR:		string		-	Character string buffer
**			  INTEGER:	numchars		-	Size of 'string' 
**
**       OUTPUT :  
**			  INTEGER:	outchars		-	Number of characters returned in 'string'
**			  INTEGER:	termstat		-	Termination status
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_string_def(subsystem, prompt, string, numchars, outchars, termstat)

int		subsystem, prompt, numchars, *outchars;
UD_DASTAT *termstat;
char		string[];

{
		int status;				/* local status cell */
		UD_STRREC strrec;		/* string control block */

		strrec.instring = string;

		status = ud_ldas(UD_DASSTRINGDEF, subsystem, prompt, &strrec, numchars,
				outchars, UD_DEFAULT);
		*termstat = strrec.termcon;
		return(status);
}

/*********************************************************************
**	R_FUNCTION:	integer ud_selection (subsystem, prompt, selrec, numrecs,
**											 outrecs, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get normalized
**			device coordinates from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  UD_SELREC:selrec		-	Selection record buffer
**			  INTEGER:	numrecs		-	Size of 'selrec' 
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'coords'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outrecs	-	Number of selecton records returned in 'selrec'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_selection(subsystem, prompt, selrec, numrecs, outrecs, dfault_flag)

int		subsystem, prompt, numrecs, *outrecs, dfault_flag;
UD_PPICKREC	selrec[];

{

	if (dfault_flag)
		return(ud_ldas(UD_DASSELECT, subsystem, prompt, selrec, numrecs, outrecs,
				  UD_DEFAULT));
	else
		return(ud_ldas(UD_DASSELECT, subsystem, prompt, selrec, numrecs, outrecs,
				  UD_NODEFAULT));
}

/*********************************************************************
**	R_FUNCTION:	integer ud_pick_loc (subsystem, prompt, ploc_rec, numrecs,
**												outrecs, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to pick an entity
**			and get a pick location from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  UD_PLOC:	ploc_rec		-	Pick location record buffer
**			  INTEGER:	numrecs		-	Size of 'ploc_rec' 
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'ploc_rec'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outrecs	-	Number of ploc records returned in 'ploc_rec'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_pick_loc(subsystem, prompt, ploc_rec, numrecs, outrecs, dfault_flag)

int		subsystem, prompt, numrecs, *outrecs, dfault_flag;
UD_PLOCREC	ploc_rec[];

{
	int		status;
	uu_denter(UU_STRC,(us,"ud_pick_loc(subs=%d,prompt=%d,)",
				subsystem,prompt));

	if (dfault_flag)
		status = ud_ldas(UD_DASPCKLOC, subsystem,
							prompt, ploc_rec, numrecs, outrecs, UD_DEFAULT);
	else
		status = ud_ldas(UD_DASPCKLOC, subsystem,
							prompt, ploc_rec, numrecs, outrecs, UD_NODEFAULT);
	uu_denter2(UU_STRC,(us,"ud_pick_loc returns=%d",status));
	uu_dexit;
	uu_dexit;
	return(status);
}

/*********************************************************************
**	R_FUNCTION:	integer ud_choice (subsystem, prompt, choice, numchoices,
**												outchoices, dfault_flag)
**	PURPOSE:
**			SAL run time front end to the DAS subsystem to get a
**			choice from the user.
**
**    PARAMETERS   
**       INPUT  : 
**         INTEGER:	subsystem	-	Subsystem number
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  INTEGER:	choice		-	Choice buffer
**			  INTEGER:	numchoices	-	Number of choices in choice menu
**			  LOGICAL:	dfault_flag	-	Prompt default flag
**												TRUE  = default exists in 'choice'
**												FALSE = no default exists
**
**       OUTPUT :  
**			  INTEGER:	outchoices	-	Number of choices returned in 'choice'
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ud_choice(subsystem, prompt, choice, numchoices, outchoices, dfault_flag)

int	subsystem, prompt, numchoices, *outchoices, dfault_flag;
int	*choice;

{

	if (dfault_flag)
		return(ud_chcldas(subsystem, prompt, choice, 1, outchoices, UD_DEFAULT));
	else
		return(ud_chcldas(subsystem, prompt, choice, 1, outchoices, UD_NODEFAULT));
}
