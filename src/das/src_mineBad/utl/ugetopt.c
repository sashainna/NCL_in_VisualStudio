#include <stdio.h>

/********************************************************************* 
**  NAME:  ugetopt.c
**
**	Contains:
**		uu_getopt()
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       ugetopt.c , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:53
**
*********************************************************************/

/*********************************************************************
**    E_FUNCTION     :  int uu_getopt( argc, argv, optstring )
**       
** Unicad's version of GETOPT(3C) for machines which don't supply it.
**	
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/* Externals for use with this procedure... */
char *optarg;
int optind=0;

int uu_getopt( argc, argv, optstring )
int argc;
char **argv;
char *optstring;
{
	int i;
	char *s;
	int c;

	/* Return at end of argv */
	if( ++optind >= argc ) return(EOF);

	/* Find next option letter in argv that matches a letter in optstring */
	optarg=argv[optind];

	/* Return EOF for the special string "-" */
	if( !strcmp(optarg,"-") ) {	/* Explicit end of options */
		++optind;
		return(EOF);
	}

	/* Return EOF if not an option string */
	if( *optarg != '-' ) return(EOF);

	++optarg;		/* Advance optarg past '-'  to option letter */

	for( c=optstring[i=0]; c!=NULL; c=optstring[++i] ) {

		/* If this is a colon, skip it */
		if( c == ':' ) continue;

		/* Does c match s */
		if( c == *optarg ) goto found;

	}

	/* Character not found in argv */
	fprintf(stderr,"getopt error. option -%c not known\n", *optarg );
	--optarg;		/* Point to '-' again */
	return('?');

	/* Character found */
found:

	if( optstring[i+1] == ':' ) {
		if( optarg[1] ) {			/* No white space */
			optarg++;
		}
		else {						/* White space */
			optarg = argv[++optind];
		}
	}
	
	return(c);
}
