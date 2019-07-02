
/*********************************************************************
**    NAME         :  reddlerr.h
**       CONTAINS:
**       defines for errors reported by the uniddl parser
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reddlerr.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:42
*********************************************************************/

#ifndef REDDLERRH


/* unrecognized statement - must start with create or control stmt */
#define UR_UNREC -1

/* unexpected end of input - EOF in mid definition */
#define UR_UEOI -2

/* expecting the word table */
#define UR_EXTAB -3

/* expecting a name */
#define UR_EXNAM -4

/* expecting open paren ( */
#define UR_EXOP -5

/* expecting a type */
#define UR_EXTYP -6

/* too many dimensions */
#define UR_TMD -7

/* expecting close square bracket ] */
#define UR_EXCB -8

/* expecting semicolon */
#define UR_EXSC -9

/* file not open for parse */
#define UR_FNO -10

/* pushed back too many characters (should be impossible) */
#define UR_TMC -11

/* too many parse files open */
#define UR_TMF -12

/* expecting a number */
#define UR_EXNUM -13

/* expecting a comma or a closeparenthesis */
#define UR_EXCM	-14

/* buffer overflow for ddltool */
#define UR_BUFLIMIT	-15

/* the JOIN declarations are not the last fied */
#define UR_JOINERR	-16

/* Unknown data type */
#define	UR_UNDEFTYPE	-17

#define REDDLERRH
#endif
