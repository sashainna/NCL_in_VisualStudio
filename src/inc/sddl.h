/*********************************************************************
**    NAME         : sddl.h 
**       Header file for Uni-DDL processing
**		CONTAINS:
**			us_ddl_init( fname )
**			us_make_table( ident )
**			us_end_table()
**			us_make_field_group( ident )
**			us_end_field_group()
**			us_make_ddl_field( ident, type )
**			type = us_make_ddl_type( type_tag )
**			type = us_get_ddl_type( ident )
**			us_make_ddl_array( type, dimension_list )
**			us_make_domain( ident, domain )
**			domain = us_make_int_domain( lower, upper )
**			domain = us_make_real_domain( lower, upper )
**			domain = us_make_enum_domain( enum_list )
**
**			<true,false> = us_compile_ddl( fname )
**			us_start_unibase()
**			us_end_unibase()
**			us_change_default_size( size )
**			us_set_relation_type( ident, rel_type, mod_list )
**
**			us_make_class( ident, superclass )
**			us_set_class_table( table )
**			us_set_class_transf( tf_class )
**			us_set_class_attr( attr_class )
**			<true,false> = us_class_kind_of( class1, class2 )
**
**			proc_const = us_get_proc_const( ident )
**			us_init_c_header_output( fname )
**			us_set_c_header_output( fname )
**			fname = us_get_c_header_output()
**			us_output_list_start()
**			fname = us_output_list_next()
**			us_set_c_struct_name( sname )
**			name = us_get_c_struct_name( class_name )
**			class = us_get_class( ident )
**			table = us_get_table( ident )
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sddl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:50
*********************************************************************/

#ifndef SDDLH
#define SDDLH

#include "usysdef.h"
#include "ssym.h"		/* symbol table def's */
#include "slist.h"	/* list def's */
#include "snode.h"	/* basic node def's */

/* node tags */
#define DDL_TABLE			1000
#define DDL_FIELD			1001
#define DDL_DOMAIN		1002

/* data type codes */
#define ddl_is_type(p) (((p)->tag>=1100)&&((p)->tag<1200))
#define DDL_KEYID			1100
#define DDL_REAL			1101
#define DDL_INTEGER		1102
#define DDL_LOGICAL		1103
#define DDL_CHARACTER	1104
#define DDL_STRING		1105
#define DDL_COORD			1106
#define DDL_TRANSF		1107
#define DDL_JOIN			1108
#define DDL_FLDGRP		1109

/* domain type codes */
#define DDL_INT_RANGE	1200
#define DDL_REAL_RANGE	1201
#define DDL_ENUM_RANGE	1202

/* relation type codes */
#define DDL_DATA_REL			0
#define DDL_ATTR_REL			1
#define DDL_TRANSF_REL		2
#define DDL_JOIN_REL			3

typedef struct t_domain					/* domain node */
{
	short					tag;				/* tag = DDL_DOMAIN */
	st_sym *				sym;				/* pointer to symbol */
	short					range_code;		/* type of range */
	union
	{
		struct								/* range_code = DDL_INT_RANGE */
		{
			int			lower;
			int			upper;
		} i;
		struct								/* range_code = DDL_REAL_RANGE */
		{
			UU_REAL		lower;
			UU_REAL		upper;
		} r;
		t_list *			e;					/* range_code = DDL_ENUM_RANGE */
	} range;
} dd_domain;

typedef struct /* DDL data type */
{
	short					tag;				/* indicates base type */
	int					dim1;				/* first dimension (=0, not array) */
	int					dim2;				/* second dimension (=0, only 1 dim) */
	union
	{
		struct t_domain *		domain;	/* domain (=NULL, no domain) */
		struct t_ddl_tbl *	table;	/* tag = DDL_JOIN, pointer to table node */
		struct t_fldgrp *		fldgrp;	/* tag = DDL_FLDGRP, ptr to field group */
	} q;
} dd_type;

typedef struct t_field		/* field node */
{
	short					tag;				/* tag = DDL_FIELD */
	st_sym *				sym;				/* pointer to symbol */
	struct t_field *	next;				/* pointer to next field in the list */
	dd_type *			type;				/* pointer to the data type */
} dd_field;

typedef struct			/* field list */
{
	dd_field *			first;			/* pointer to first field in list */
	dd_field *			last;				/* pointer to last field in list */
} dd_list;

typedef struct t_ddl_tbl			/* table node */
{
	short					tag;				/* tag = DDL_TABLE */
	st_sym *				sym;				/* pointer to symbol */
	dd_list				fl;				/* field list */
	d_node *				class;			/* pointer to class (NULL if none) */
	struct t_ddl_tbl *	super_rel;	/* relation containing this relation */
	int					rel_num;			/* relation number */
	int					rel_type;		/* relation type (DDL_DATA_REL,...) */
	int					init_size;		/* initial table size */
	int					exp_size;		/* expansion table size */
} dd_table;

typedef struct t_fldgrp			/* field group node */
{
	short					tag;				/* tag = DDL_FLDGRP */
	st_sym *				sym;				/* pointer to symbol */
	dd_list				fl;				/* field list */
	d_node *				sal_type;		/* pointer to SAL data type (RECORD) */
	dd_type *			ddl_type;		/* pointer to DDL data type node */
} dd_fldgrp;

extern	void			us_ddl_init();
extern	void			us_make_table();
extern	void			us_end_table();
extern	void			us_make_field_group();
extern	void			us_end_field_group();
extern	void			us_make_ddl_field();
extern	dd_type *	us_make_ddl_type();
extern	dd_type *	us_get_ddl_type();
extern	void			us_make_ddl_array();
extern	void			us_make_domain();
extern	dd_domain *	us_make_int_domain();
extern	dd_domain *	us_make_real_domain();
extern	dd_domain *	us_make_enum_domain();

extern	int			us_compile_ddl();
extern	void			us_start_unibase();
extern	void			us_end_unibase();
extern	void			us_change_default_size();
extern	void			us_set_relation_type();

extern	void			us_make_class();
extern	d_node *		us_make_class_field();
extern	void			us_set_class_table();
extern	void			us_set_class_transf();
extern	void			us_set_class_attr();
extern	int			us_class_kind_of();

extern	e_node *		us_get_proc_const();
extern	void			us_init_c_header_output();
extern	void			us_set_c_header_output();
extern	char *		us_get_c_header_output();
extern	void			us_output_list_start();
extern	char *		us_output_list_next();
extern	void			us_set_c_struct_name();
extern	char *		us_get_c_struct_name();
extern	d_node *		us_get_class();
extern	d_node *		us_get_table();

#ifndef SDDLC
	extern	int	us_rel_num;
#endif

#endif

