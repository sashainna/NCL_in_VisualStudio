/*********************************************************************
**    NAME         :  asdas.c
**       CONTAINS:
**			ua_initpp
**			ua_popup
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       aspop.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:40
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "dasnog.h"
#include "dasg.h"
#include "uhep.h"
#include "ustdio.h"

	static char *menu1[4] = {
			"ORIGIN MODE",
			"Delta offset",
			"Stack grid",
			"Align arrows"	};

	static char *menu2[4] = {
			"LEADER MODE",
			"Without",
			"With",
			"Extension"	};

	static char *menu3[5] = {
			"START LOCATION",
			"Top",
			"Bottom",
			"Right",
			"Left"	};

	static char *menu4[3] = {
			"TERMINATOR LOCATION",
			"On an entity",
			"Free"	};

	static char *menu5[5] = {
			"TEXT ALIGNMENT",
			"Horizontal",
			"Aligned",
			"Over Leader",
			"Over Stub"	};

	static char *menu6[5] = {
			"CREATE LINE",
			"Char Symbol",
			"Tolerance",
			"Datum",
			"Finished"	};

	static char *menu7[10] = {
			"CHAR SYMBOL",
			"Straight",
			"Flat",
			"Round",
			"Cylindrical",
			"Prof of Line",
			"Prof of Surf",
			"Angular",
			"Perpendicular",
			"More"	};

	static char *menu8[8] = {
			"CHAR SYMB01",
			"Parallel",
			"True Pos",
			"Concentric",
			"Symmetry",
			"Circ Runout",
			"Total Runout",
			"Rectangular"	};

	static char *menu9[4] = {
			"REFERENCE",
			"Single Datum",
			"Mult Datum",
			"Identify"	};

	static char *menu10[5] = {
			"SYMBOL",
			"No Symbol",
			"MMC",
			"RFS",
			"User Defined"	};

	static char *menu11[4] = {
			"TOL TEXT",
			"Symbol",
			"Tolerance",
			"Finished"	};

	static char *menu12[7] = {
			"SYMBOL1",
			"MMC",
			"RFS",
			"Tol Zone Proj",
			"Dia Symbol",
			"DIA",
			"User Defined"	};

	static char *menu13[10] = {
			"TOLERANCE",
			"0.0002",
			"0.0005",
			"0.001",
			"0.002",
			"0.005",
			"0.010",
			"0.025",
			"0.050",
			"User Defined"	};

	static char *menu14[5] = {
			"START CORNER",
			"Upper-left",
			"Lower-left",
			"Upper-right",
			"Lower-right"	};

	static char *menu15[4] = {
			"POINT",
			"End Point",
			"Tangent Point",
			"Center Point"	};

	static char *menu16[6] = {
			"ANGULAR TYPE",
			"Interior",
			"Supplement",
			"Exterior",
			"Align arrows",
			"Repetitive" }; 

	static char *menu17[3] = {
			"PARALL LINES",
			"Redo lines",
			"Redo type"	};

	static char *menu18[7] = {
			"HATCH PATTERN",
			"Iron",
			"Steel",
			"Brass/Copper",
			"Rubber/Plastic",
			"Lead",
			"Alum/Mag"	};

	static char *menu19[5] = {
			"RADIUS TYPE",
			"Arc Center",
			"Outside",
			"Large",
			"Repetitive"};

	static char *menu20[7] = {
			"DIAMETER TYPE",
			"Inside",
			"Outside",
			"Large",
			"Shaft",
			"Cylindrical",
			"Repetitive"};

	static char *menu21[4] = {
			"EDIT TYPE",
			"Main Text",
			"Appended Text",
			"Replace App"	};

	static char *menu22[6] = {
			"LOAD STDS",
			"Ansi Standard",
			"Iso Standard",
			"Bsi Standard",
			"Din Standard",
			"User defined" };

	static char *menu23[6] = {
			"EDIT OPTIONS",
			"Edit Line",
			"Delete Line",
			"Add Line Before",
			"Add Line After",
			"Exit" };

	static char *menu24[6] = {
			"CENTERLINE OPTIONS",
			"Single Line",
			"Colinear Points",
			"Colinear Arcs",
			"Full Bolt Circle",
			"Partial Bolt Circle",
			};

	static char *menu25[4] = {
			"PARTS LIST EDIT OPTIONS",
			"Add Entry",
			"Modify Entry",
			"Delete Entry" };

	static char *menu26[3] = {
			"BALLOON ARROW LOCATION",
			"On an entity",
			"Free"	};

	static char *menu27[3] = {
			"UPDATE OPTIONS",
			"All Entries",
			"Selected Subset"	};

	static char *menu28[10] = {
			"EDIT OPTION",
			"Change ID No.",
			"Change Simple to Ref",
			"Change Ref to Simple",
			"Change Multiplier Value",
			"Add X Multiplier",
			"Add PLCS Multiplier",
			"Remove Multiplier",
			"Add Leader",
			"Delete Leader"   };

	static char *menu29[5] = {
			"BALLOON TYPE ",
			"Simple",
			"Reference",
			"With X Multiplier",
			"With PLCS Multiplier"   };

	static char *menu30[3] = {
			"SYMBOL POSITION",
			"After",
			"Below"   };

	static char *menu31[3] = {
			"TEXT TYPE ",
			"Note",
			"Dimension"   };

	static UD_POPUPREC popup[31] = {
												 { 4, menu1, "", {.18, .11 }, 0, 0 },
												 { 4, menu2, "", {.18, .11 }, 0, 0 },
												 { 5, menu3, "", {.18, .127 }, 0, 0 },
												 { 3, menu4, "", {.18, .1 }, 0, 0 },
												 { 5, menu5, "", {.18, .127 }, 0, 0 },
												 { 5, menu6, "", {.18, .127 }, 0, 0 },
												 { 10, menu7, "", {.18, .22 }, 0, 0 },
												 { 8, menu8, "", {.18, .19 }, 0, 0 },
												 { 4, menu9, "", {.18, .11 }, 0, 0 },
												 { 5, menu10, "", {.18, .127 }, 0, 0 },
												 { 4, menu11, "", {.18, .11 }, 0, 0 },
												 { 7, menu12, "", {.18, .17 }, 0, 0 },
												 { 10, menu13, "", {.18, .22 }, 0, 0 },
												 { 5, menu14, "", {.18, .127 }, 0, 0 },
												 { 4, menu15, "", {.18, .11 }, 0, 0 },
												 { 6, menu16, "", {.18, .127 }, 0, 0 },
												 { 3, menu17, "", {.18, .1 }, 0, 0 },
												 { 7, menu18, "", {.18, .17 }, 0, 0 },
												 { 5, menu19, "", {.18, .11 }, 0, 0 },
												 { 7, menu20, "", {.18, .15 }, 0, 0 },
												 { 4, menu21, "", {.18, .127 }, 0, 0 },
												 { 6, menu22, "", {.18, .1 }, 0, 0 },
												 { 6, menu23, "", {.18, .15 }, 0, 0 },
												 { 6, menu24, "", {.18, .15 }, 0, 0 },
												 { 4, menu25, "", {.18, .15 }, 0, 0 },
												 { 3, menu26, "", {.18, .15 }, 0, 0 },
												 { 3, menu27, "", {.18, .15 }, 0, 0 },
												 { 10, menu28, "", {.18, .15 }, 0, 0 },
												 { 5, menu29, "", {.18, .15 }, 0, 0 },
												 { 3, menu30, "", {.18, .15 }, 0, 0 },
												 { 3, menu31, "", {.18, .15 }, 0, 0 }
										    };

/*********************************************************************
**	E_FUNCTION:	integer ua_popmenu (prompt, choice)
**
**	PURPOSE:
**			SAL run time front end to the DAS popup subsystem to get a
**			choice from the user.
**
**    PARAMETERS   
**       INPUT  : 
**			  INTEGER:	prompt		-	Number of the prompt in apphep file
**			  INTEGER:	choice		-	Choice buffer
**
**       OUTPUT :  
**
**    RETURNS      :	TRUE if the operator terminated with a "done"
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

int ua_popmenu(prompt, choice)
int	prompt;
int	*choice;
{
	int outchoices;

	return(ud_ddas(UD_POPUP, &popup[prompt-1], choice, 1,
					&outchoices, UD_NODEFAULT));
}

/*********************************************************************
**	R_FUNCTION:	integer ua_initpp ()
**
**	PURPOSE:
**			SAL run time front end to initialize the drafting pop up menus
**
**    PARAMETERS   
**       INPUT  :  none
**
**       OUTPUT :  none
**
**    RETURNS      :	none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

void ua_initpp()
{
/* NCL - do not load popup menus at initialization.
	int i;
	for(i=0; i<31; i++)
		ud_initpp(&popup[i]);
   end NCL */
}
