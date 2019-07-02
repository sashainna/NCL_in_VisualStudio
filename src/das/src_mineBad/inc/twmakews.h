/********************************************************************* 
**  NAME:  twmkwspart.h
**
**      Include file for all workstation generation tools
**
**  COPYRIGHT  1985  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       twmakews.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:58
**
*********************************************************************/

#define MDEF_BUF_LINELEN 128
#define MDEF_MAX_NUMLINE 2048

#define WJTD_BUF_LINELEN 32
#define WJTD_MAX_NUMLINE 64

#define WJT_BUFSIZE 32
#define WJT_MAXENTRIES 64

#define W_NO_GOOD -1

typedef enum {		/* type of current definition just read from the ".w" file */
	DUMMY,			/* so lex won't return a zero when it returns a "Wdef_type" */
	ALLPART_DEF,	/* belongs in all the ws ".c" files */
	DATA_DEF,		/* specifies the contents of the ws ".h" file */
	WDT_DEF,			/* specifies the contents of the ws "wdt.c" file */
	FUNC_DEF,		/* defines the code that implements a skel func */
	JT_REPL_DEF,	/* defines a j-t entry function that replaces a skel func */
	JT_SIM_DEF		/* specifies a simulation routine for a jump-table entry */
	} Wdef_type;

typedef enum {		/* defines how to replace skeleton macro references */
	REPL_ASIS,		/* simple direct replacement */
	REPL_F_ARGS,	/* function implementation code with arguments to replace */
	REPL_F_NOARGS,	/* function implementation code without args to replace */
	REPL_DATA,		/* direct replacement of arg list with simple data */
	REPL_ALL,		/* replace entire skeleton function with new code */
	REPL_DELETE		/* delete entire skeleton function */
	} Wrepl_flag;

typedef struct {			/* data concerning macro def from the ".w" file */
	int buf_pos;			/* position in "mdef_buf" of start of definition */
	int num_lines;			/* number of lines required for definition */
	Wdef_type def_type;	/* type of definition */
	int wjt_index;			/* index into ws-part jump-table */
	Wrepl_flag rep_flg;	/* how to do replacement */
	} Wmacdef;
