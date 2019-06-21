/*********************************************************************
**    NAME         : ssym.h 
**			Include file for symbol table types and variables
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ssym.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:52
*********************************************************************/

#ifndef SSYMH


#include "snode.h"				/* basic node typedef's */

#define	IDMAX			32			/* maximum length of an identifier */
#define	HASHSIZE		101		/* number of entries in hash table */	

/* symbol record */
typedef struct t_sym 
	{
	struct t_sym *	sym_next;				/* next symbol with same hash value */
	int				sym_context;			/* context in which defined */
	d_node *			sym_data;				/* data associated with this record */
	char				sym_string[1];			/* char string for symbol (1 byte for
														the terminating null) */
	} st_sym;

/* symbol table */
typedef struct t_table
	{
	st_sym			*tbl_hash[HASHSIZE];		/* hash table */
	/* the following values are used by us_sym_next() */
	int				tbl_hash_loc;				/* current hash location */
	st_sym			*tbl_sym_loc;
	} st_table;

/*
	Allowed values for current context and for 
	symbol token values to indicate where the symbol was found or
	that it was added to the current symbol table (CURRENT).
*/

#define	CX_CURRENT	0	
#define	CX_PUBLIC	1
#define	CX_PREDEF	2
#define	CX_UNIBASE	3
#define	CX_DDL		4
#define	CX_GLOBAL	5
#define	CX_PARAMS	6
#define	CX_LOCAL		7
#define	CX_RESERVED 8
#define	CX_RECORD 	9
#define	CX_VARIANT 	10

extern	void			us_sym_init();			/* initialize symbol tables */
extern	st_table *	us_st_create();		/* create a symbol table */
extern	st_sym *		us_sym_add();			/* add a symbol to a table */
extern	st_sym *		us_sym_register();	/* register a symbol */
extern	void			us_sym_start();		/* start a symbol-visiting loop */
extern	st_sym *		us_sym_next();			/* get next symbol in loop */
extern	st_sym *		us_sym_cx_find();		/* find symbol in current context */
extern	st_sym *		us_sym_find();			/* find symbol in particular table */
extern	void			us_push_context();	/* push current context */
extern	void			us_pop_context();		/* pop current context */
extern	int			us_is_local();			/* TRUE if context == local */
extern	st_sym *		us_rw_find();			/* find a reserved word */
extern	int			us_sym_cmp();			/* case-insensitive string compare */
extern	st_sym *		us_sym_copy();			/* copy a symbol */
extern	st_sym *		us_define_symbol();	/* check for legality of definition */

extern	st_table *	us_globals;				/* global symbol table */
extern	st_table *	us_predefs;				/* predefined symbol table */
extern	st_table *	us_unibase_st;			/* Unibase section symbol table */
extern	st_table *	us_ddl_st;				/* DDL section symbol table */

/* values of us_sal_ddl_flag */
#define US_SAL 0
#define US_DDL 1

#ifndef SSYMC
	extern int		us_sal_ddl_flag;
#endif


#define SSYMH
#endif
