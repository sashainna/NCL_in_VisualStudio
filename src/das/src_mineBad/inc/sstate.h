/*********************************************************************
**    NAME         :  sstate.h
**       Statement node typedef's and define's
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sstate.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:52
*********************************************************************/

#ifndef SSTATEH


#include "snode.h"	/* basic node def's */
#include "slist.h"	/* list def's */


/* if statement node */
#define IFNODE 500
typedef struct t_if
{
	short			tag;			/* tag = IFNODE */
	s_node *		st_next;		/* next statement */
	short			st_line;		/* line number */
	e_node *		if_cond;		/* condition expression */
	s_node *		if_then;		/* 'then' clause statements */
	s_node *		if_else;		/* 'else' clause statements */
} s_if;

/* goto statement node */
#define GOTONODE 501
typedef struct t_goto
{
	short			tag;			/* tag = GOTONODE */
	s_node *		st_next;		/* next statement */
	short			st_line;		/* line number */
	s_node *		goto_target;/* pointer to labelled statement */
	s_node *		goto_stlist;/* pointer to statement list */
} s_goto;

/* labelled statement node */
#define LABSTNODE 502
typedef struct t_labst
{
	short			tag;				/* tag = LABSTNODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	d_node *		labst_label;	/* pointer to label */
} s_labst;

/* assignment statement node */
#define ASSIGNNODE 503
typedef struct t_ass
{
	short			tag;				/* tag = ASSIGNNODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	e_node *		ass_vref;		/* pointer to variable reference */
	e_node *		ass_expr;		/* pointer to expression */
	d_node *		ass_type;		/* data type of assignment */
} s_ass;

/* return statement node */
#define RETURNNODE 504
typedef struct t_ret
{
	short			tag;				/* tag = RETURNNODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	e_node *		ret_expr;		/* pointer to expression */
} s_ret;

/* stop statement node */
#define STOPNODE 505
typedef struct t_stop
{
	short			tag;				/* tag = STOPNODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
} s_stop;

/* procedure call statement node */
#define CALLSTNODE 506
typedef struct
{
	short			tag;				/* tag = CALLSTNODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	e_node *		cst_expr;		/* pointer to procedure call expression */
} s_cst;

/* do statement node */
#define DONODE 507
typedef struct
{
	short			tag;				/* tag = DONODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	s_node *		do_slist;		/* statement list */
} s_do;

/* ENDDO statement node */
#define ENDDONODE 508
typedef struct
{
	short			tag;				/* tag = ENDDONODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	s_do *		enddo_do;		/* pointer to DO statement */
} s_enddo;

/* while statement node */
#define WHILENODE 509
typedef struct
{
	short			tag;				/* tag = WHILENODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	e_node *		while_cond;		/* condition expression */
	s_node *		while_loop;		/* loop statement containing while */
} s_while;

/* until statement node */
#define UNTILNODE 510
typedef struct
{
	short			tag;				/* tag = UNTILNODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	e_node *		until_cond;		/* condition expression */
	s_node *		until_loop;		/* loop statement containing until */
} s_until;

/* for statement node */
#define FORNODE 511
typedef struct
{
	short			tag;				/* tag = FORNODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	d_node		*for_vref;		/* FOR variable reference */
	e_node		*for_range;		/* range expression */
	e_node		*for_by;			/* BY expression */
	int			for_loc;			/* address of local FOR storage */
	int			for_type;		/* type of FOR statement (see below) */
	s_node		*for_slist;		/* statement list */
} s_for;

/* FOR statement types (values of for_type) */
#define IRFOR 1		/* INTEGER range */
#define RRFOR 2		/* REAL range */
#define ERFOR 3		/* ENUM range */
#define SVFOR 4		/* SET value range */

/* ENDFOR statement node */
#define ENDFORNODE 512
typedef struct
{
	short			tag;				/* tag = ENDFORNODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	s_for *		endfor_for;		/* pointer to FOR statement */
} s_endfor;

/* select statement node */
#define SELECTNODE 513
typedef struct
{
	short			tag;				/* tag = SELECTNODE */
	s_node *		st_next;			/* next statement */
	short			st_line;			/* line number */
	e_node *		sel_expr;		/* expression (NULL for type-2 SELECT)*/
	s_node *		first_case;		/* pointer to first CASE clause */
	s_node *		last_case;		/* pointer to last CASE clause */
	s_node *		sel_otherwise;	/* OTHERWISE statement list (NULL if none) */
} s_sel;

/* case type-1 statement node */
#define CASE1NODE 514
typedef struct
{
	short			tag;				/* tag = CASE1NODE */
	s_node *		st_next;			/* next case node */
	short			st_line;			/* line number */
	t_list *		case1_clist;	/* constant list */
	s_node *		case1_slist;	/* statement list */
} s_case1;

/* case type-2 statement node */
#define CASE2NODE 515
typedef struct
{
	short			tag;				/* tag = CASE2NODE */
	s_node *		st_next;			/* next case node */
	short			st_line;			/* line number */
	e_node *		case2_cond;		/* condition expression */
	s_node *		case2_slist;	/* statement list */
} s_case2;

/* read statement node */
#define READNODE 516
typedef struct
{
	short			tag;				/* tag = READNODE */
	s_node *		st_next;			/* next statement node */
	short			st_line;			/* line number */
	e_node *		read_unit;		/* I/O unit expression */
	t_list *		read_vlist;		/* variable reference expression list */
} s_read;

/* write statement node */
#define WRITENODE 517
typedef struct
{
	short			tag;				/* tag = WRITENODE */
	s_node *		st_next;			/* next statement node */
	short			st_line;			/* line number */
	e_node *		write_unit;		/* I/O unit expression */
	t_list *		write_elist;	/* expression list */
} s_write;

/* statement list node */
#define STLISTNODE 518
typedef struct t_stlist
{
	short			tag;				/* tag = STLISTNODE */
	s_node *		st_next;			/* pointer to first statement in list */
	short			st_line;			/* line number */
	s_node *		stlist_parent;	/* pointer to parent statement list */
} s_stlist;

/* statement list end node */
#define STENDNODE 519
typedef struct t_stend
{
	short			tag;				/* tag = STENDNODE */
	s_node *		st_next;			/* next statement node */
	short			st_line;			/* line number */
	s_node *		stend_target;	/* pointer to target statement */
} s_stend;

/* EXIT node */
#define EXITNODE 520
typedef struct t_exit
{
	short			tag;				/* tag = EXITNODE */
	s_node *		st_next;			/* next statement node */
	short			st_line;			/* line number */
	s_node *		exit_target;	/* pointer to target statement */
} s_exit;

/* CONTINUE node */
#define CONTNODE 521
typedef struct t_cont
{
	short			tag;				/* tag = CONTNODE */
	s_node *		st_next;			/* next statement node */
	short			st_line;			/* line number */
	s_node *		cont_target;	/* pointer to target statement */
} s_cont;

/* APPEND (list) node */
#define APPENDNODE 522
typedef struct
{
	short			tag;				/* tag = APPENDNODE */
	s_node *		st_next;			/* next statement node */
	short			st_line;			/* line number */
	e_node		*expr;			/* pointer to item or list expression to append */
	e_node		*vref;			/* pointer to variable reference expression */
} s_append;

/* INSERT (list) node */
#define INSERTNODE 523
typedef struct
{
	short			tag;				/* tag = INSERTNODE */
	s_node *		st_next;			/* next statement node */
	short			st_line;			/* line number */
	e_node		*expr;			/* pointer to item or list expression to insert */
	e_node		*pos_expr;		/* pointer to position expression */
	e_node		*vref;			/* pointer to variable reference expression */
} s_insert;

/* DELETE (list) node */
#define DELETENODE 524
typedef struct
{
	short			tag;				/* tag = DELETENODE */
	s_node *		st_next;			/* next statement node */
	short			st_line;			/* line number */
	e_node		*pos_expr;		/* pointer to position expression */
	e_node		*vref;			/* pointer to variable reference expression */
} s_delete;

/* FREE (list) node */
#define FREENODE 525
typedef struct
{
	short			tag;				/* tag = FREENODE */
	s_node *		st_next;			/* next statement node */
	short			st_line;			/* line number */
	e_node		*vref;			/* pointer to variable reference expression */
} s_free;

/* SORT (list) node */
#define SORTNODE 526
typedef struct
{
	short			tag;				/* tag = SORTNODE */
	s_node *		st_next;			/* next statement node */
	short			st_line;			/* line number */
	e_node		*vref;			/* pointer to variable reference expression */
	e_node		*expr;			/* pointer to procedure expression */
} s_sort;

extern void			us_statement_line();
extern s_node *	us_make_stlist();
extern s_node *	us_end_stlist();
extern void			us_term_stlist();
extern s_node *	us_make_if();
extern s_node *	us_make_goto();
extern s_node *	us_label_statement();
extern s_node *	us_make_assign();
extern s_node *	us_make_call_state();
extern s_node *	us_make_return();
extern s_node *	us_make_stop();
extern s_node *	us_make_read();
extern s_node *	us_make_write();
extern s_node *	us_make_stend();

extern void			us_make_do();
extern s_node *	us_end_do();
extern void			us_make_for();
extern s_node *	us_end_for();
extern void			us_for_range();
extern void			us_for_expression();
extern s_node *	us_make_while();
extern s_node *	us_make_until();
extern void			us_make_select();
extern s_node *	us_end_select();
extern void			us_make_case1();
extern void			us_make_case2();
extern void			us_make_otherwise();
extern s_node *	us_make_exit();
extern s_node *	us_make_continue();

extern e_node *	us_check_condition();

extern s_node *	us_make_append_st();
extern s_node *	us_make_insert_st();
extern s_node *	us_make_delete_st();
extern s_node *	us_make_free_st();
extern s_node *	us_make_sort_st();

#define SSTATEH
#endif
