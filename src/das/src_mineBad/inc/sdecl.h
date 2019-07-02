/*********************************************************************
**    NAME         : sdecl.h 
**       Define's and typedef's for declarations 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sdecl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:50
*********************************************************************/

#ifndef SDECLH


#include "snode.h"	/* basic node typedef */
#include "slist.h"	/* list typedef */
#include "ssym.h"		/* symbol table def's */

#define STRMAX		1024	/* maximum size of character strings */
#define SETMAX		1024	/* maximum elements in a set 
									(also size of enumeration type) */

/* storage and scope classes */

/* where defined */
#define CLOCAL		1 << 0
#define CGLOBAL   1 << 1
#define CPREDEF	1 << 2

/* availability */
#define CPRIVATE	1 << 3
#define CPUBLIC	1 << 4
#define CEXTERNAL 1 << 5

/* lifetime */
#define CSAVE		1 << 6
#define CAUTO		1 << 7
#define CARG		1 << 8

/* read-only */
#define CREF		1 << 9

/* passed by address */
#define CADDR		1 << 10

/* class types */
#define CT_DATA	0
#define CT_ATTR	1
#define CT_TRANSF	2

/***********************************

	Node tag value allocation:
		200 - 299	Object nodes
		300 - 399	Type nodes

***********************************/


#define is_type(tag) (((tag) >= 300) && ((tag) < 400))

/* variable node */
#define VARNODE 200
typedef struct t_var
	{
	short			tag;				/* tag = VARNODE */
	st_sym *		sym;				/* pointer to symbol */
	d_node *		var_type;		/* type of variable */
	int			var_class;		/* storage and scope classes */
	e_node *		var_init;		/* initializer, NULL if none */
	int			var_loc;			/* location (offset) of variable */
	} n_var;

/* field node */
#define FIELDNODE 201
typedef struct t_fld
	{
	short				tag;				/* tag = FIELDNODE */
	st_sym *			sym;				/* pointer to symbol */
	struct t_fld *	fld_next;		/* next field */
	d_node *			fld_type;		/* type of field */
	int				fld_loc;			/* offset of field within record */
	} n_fld;

/* field list for record and variant nodes */
typedef struct t_flist
	{
	n_fld *		first;	/* pointer to first field */
	n_fld *		last;		/* pointer to last field */
	} d_flist;

/* variant case node */
#define VCASENODE 202
typedef struct t_vcase
	{
	short					tag;			/* tag = VCASENODE */
	d_flist				fl;			/* field list */
	struct t_vcase *	vc_next;		/* pointer to next case */
	int					vc_const;	/* case constant value */
	} n_vcase;

/* procedure parameter node */
#define PARMNODE 204
typedef struct t_parm
	{
	short					tag;				/* tag = PARMNODE */
	st_sym *				sym;				/* pointer to symbol */
	struct t_parm *	parm_next;		/* next parameter */
	int					parm_class;		/* parameter storage and scope class */
	d_node *				parm_type;		/* type of parameter */
	int					parm_loc;		/* location offset for value */
	} n_parm;

/* method node */
#define MESSAGENODE 205
typedef struct t_message
{
	short					tag;			/* tag = MESSAGENODE */
	st_sym *				sym;			/* pointer to symbol */
	int					mess_num;	/* message number */
	d_node				*type;		/* pointer to procedure type */
} n_message;

/* method node */
#define METHODNODE 206
typedef struct t_method
{
	short					tag;			/* tag = METHODNODE */
	struct t_method	*next;		/* pointer to next method */
	n_message			*message;	/* pointer to message */
	e_node				*procc;		/* pointer to method procedure constant */
} n_method;

/* label node */
#define LABELNODE		207
typedef struct t_label
{
	short			tag;				/* tag = LABELNODE */
	st_sym *		sym;				/* pointer to symbol */
	s_node *		label_target;	/* pointer to labelled statement,
											also used for GOTO chain */
	s_node *		label_stlist;	/* pointer to statement list, NULL if undefined */
	short			label_flag;		/* TRUE if referenced by a GOTO */
} n_label;

/* constant node */
#define CONSTNODE 208
typedef struct
{
	short			tag;				/* tag = CONSTNODE */
	st_sym *		sym;				/* pointer to symbol */
	d_node *		const_type;		/* type of constant */
	e_node *		const_expr;		/* constant expression */
} n_const;

/* enumeration constant node */
#define ECONSTNODE 209
typedef struct t_ec
	{
	short			tag;				/* tag = ECONSTNODE */
	st_sym *		sym;				/* pointer to symbol */
	d_node *		ec_type;			/* pointer to enumeration type */
	int			ec_value;		/* position in enumeration type */
	d_node *		ec_next;			/* pointer to next enumeration constant in type */
	} n_ec;

/* CLASS constant node */
#define CLASSCNODE		210
typedef struct t_classc
{
	short					tag;				/* tag = CLASSCNODE */
	st_sym				*sym;				/* pointer to type identifier (or NULL) */
	int					size;				/* storage required in words */
	int					rel_num;			/* relation number */
	int					inst_flag;		/* TRUE if instances can be created */
	int					class_type;		/* CT_DATA, CT_ATTR, or CT_TRANSF */
	char					*struct_name;	/* pointer to C struct name */
	char					*output_fname;	/* C struct filename for output */
	d_node				*data_table;	/* pointer to table for permanent data */
	struct t_classc	*attr_class;	/* pointer to attr. class, NULL if none */
	struct t_classc	*tf_class;		/* pointer to transf. class, NULL if none */
	struct t_classc	*super_class;	/* pointer to superclass, NULL if none */
	struct t_classc	*sibling_class;/* pointer to next sibling class */
	struct t_classc	*sub_class;		/* pointer to first subclass */
	d_flist				perm_fl;			/* permanent data field list */
	d_flist				temp_fl;			/* temporary data field list */
	n_method				*methods;		/* pointer to first method */
} n_classc;

/* integer node */
#define INTNODE		300
extern	d_node		us_integer;

/* real node */
#define REALNODE		301
extern	d_node		us_real;

/* logical node */
#define LOGICALNODE	302
extern	d_node		us_logical;

/* object key node */
#define OBJKEYNODE	303
extern	d_node		us_obj_key;

/* character node */
#define CHARNODE		304
extern	d_node		us_char;

/* coord node */
#define COORDNODE		305
extern	d_node		us_coord;

/* transf node */
#define TRANSFNODE	306
extern	d_node		us_transf;

/* CLASS node */
#define CLASSNODE		307
extern	d_node		us_class;

/* STRING node */
#define STRINGNODE		308
extern	d_node		us_string;
typedef struct t_string
{
	short			tag;				/* tag = STRINGNODE */
	st_sym		*sym;				/* pointer to type identifier (or NULL) */
	int			size;				/* storage required in (int) units */
	int			string_dim;		/* dimension (max # of chars) */
} n_string;

/* bit node */
#define BITNODE		309
extern	d_node		us_bit;

/* LIST node */
#define LISTNODE		310
typedef struct
{
	short			tag;				/* tag = LISTNODE */
	st_sym		*sym;				/* pointer to type identifier (or NULL) */
	int			size;				/* storage required in (int) units */
	d_node		*elm_type;		/* pointer to element type */
} n_list;

/* record or packed record node */
#define RECNODE		311
#define PACKEDNODE	312
typedef struct t_rec
{
	short			tag;				/* tag = RECNODE */
	st_sym		*sym;				/* pointer to type identifier (or NULL) */
	int			size;				/* storage required in (int) units */
	short			rec_gflag;		/* type generated flag (used in C-gen) */
	short			rec_gnum;		/* type # (used in C-gen) */
	d_flist		fl;				/* field list */
} n_rec;

/* variant node */
#define VARIANTNODE	313
typedef struct t_vari
{
	short			tag;				/* tag = VARIANTNODE */
	st_sym		*sym;				/* pointer to type identifier (or NULL) */
	int			size;				/* storage required in (int) units */
	short			vari_gflag;		/* type generated flag (used in C-gen) */
	short			vari_gnum;		/* type # (used in C-gen) */
	n_fld *		vari_tag;		/* tag field */
	d_node *		vari_type;		/* tag type */
	n_vcase *	vari_case;		/* first case in variant */
} n_vari;

/* array node */
#define ARRAYNODE		314
typedef struct t_array
{
	short			tag;				/* tag = ARRAYNODE */
	st_sym		*sym;				/* pointer to type identifier (or NULL) */
	int			size;				/* storage required in (int) units */
	d_node *		arr_elm_type;	/* element type */
	e_node *		arr_index_expr;/* index bounds constant expression */
	int			arr_dim;			/* number of elements in the array */
} n_array;

/* procedure type node */
#define PROCNODE		315
typedef struct t_proc
{
	short			tag;				/* tag = PROCNODE */
	st_sym		*sym;				/* pointer to type identifier (or NULL) */
	int			size;				/* storage required in (int) units */
	int			proc_lang;		/* language (0 = SAL) */
	n_parm *		proc_first;		/* pointer to first argument */
	n_parm *		proc_last;		/* pointer to last argument */
	d_node *		proc_rtype;		/* return type (NULL if none) */
} n_proc;

/* enumeration node */
#define ENUMNODE		316
typedef struct
{
	short			tag;				/* tag = ENUMNODE */
	st_sym		*sym;				/* pointer to type identifier (or NULL) */
	int			size;				/* storage required in (int) units */
	d_node *		enum_base_set;	/* pointer to corresponding set type */
	int			enum_size;		/* number of enumeration constants */
	d_node *		enum_list;		/* pointer to first enumeration constant */
	short			enum_gflag;		/* type generated flag (used in C-gen) */
	short			enum_gnum;		/* type # (used in C-gen) */
} n_enum;

/* set node */
#define SETNODE		317
typedef struct
{
	short			tag;				/* tag = SETNODE */
	st_sym		*sym;				/* pointer to type identifier (or NULL) */
	int			size;				/* storage required in (int) units */
	d_node *		set_enum_type;	/* pointer to corresponding enumeration type */
	int			set_size;		/* number of enumeration constants */
	unsigned		set_mask;		/* bit-mask for significant bits in last (int) */
} n_set;
extern	d_node		us_set;

/* packed integer or bit node */
#define PINTNODE		318
#define PBITNODE		319
typedef struct
{
	short			tag;				/* tag = PINTNODE or PBITNODE */
	st_sym		*sym;				/* pointer to type identifier (or NULL) */
	int			size;				/* storage required in bits */
} n_packed;

/* OBJECT_DATA node */
#define OBJDATANODE		320
typedef struct
{
	short			tag;				/* tag = OBJDATANODE */
	st_sym		*sym;				/* pointer to type identifier (or NULL) */
	int			size;				/* storage required in bits */
	n_classc		*class;			/* pointer to class constant node */
} n_objdata;

/* type node */
#define TYPENODE		350
extern	d_node		us_type;

#ifndef USPROCC		/* if not compiling sproc.c */
	extern e_node	*us_current_proc;
	extern int		us_static_loc;
	extern int		us_auto_loc;
	extern int		us_auto_size;
#endif

#ifndef USRECVARC		/* if not compiling srecvar.c */
	/* the top node pointer */
	union 
	{
		n_rec			*r;
		n_vari		*v;
		n_classc		*c;
	} us_top_rec_node;
#endif

extern	n_var *		us_make_variable();
extern	void			us_type_variable();
extern	void			us_type_var_list();

extern	n_string *	us_make_string();

extern	d_node *		us_make_enum();
extern	void			us_end_enum();
extern	void			us_make_enum_const();
extern	d_node *		us_base_set();
extern	d_node *		us_enum_type();
extern	char *		us_find_enum();

extern	n_array *	us_make_array();
extern	void			us_make_array_elem();
extern	d_node *		us_make_list();

extern	d_node *		us_make_obj_data();

extern	void			us_init_initializers();
extern	t_list *		us_get_initializers();

extern	d_node *		us_make_type_id();
extern	d_node *		us_get_type();
extern	n_const *	us_make_constant();

extern	n_label *	us_make_label();

extern	n_rec *		us_make_record();
extern	void			us_end_record();
extern	n_fld *		us_make_field();
extern	n_fld *		us_find_field();
extern	void			us_type_field();
extern	void			us_type_field_list();
extern	st_sym *		us_validate_field();
extern	n_fld *		us_field_node();

extern	void			us_end_packed();
extern	d_node *		us_packed_type();

extern	void			us_push_record();
extern	void			us_pop_record();

extern	n_vari *		us_make_variant();
extern	void			us_end_variant();
extern	void			us_make_vcase();
extern	n_vcase *	us_find_vcase_expr();
extern	n_vcase *	us_find_vcase_value();

extern	n_proc *		us_make_procedure();
extern	void			us_end_procedure();
extern	void			us_type_procedure();
extern	void			us_check_params();
extern	void			us_make_param();
extern	void			us_type_param();
extern	void			us_type_param_list();
extern	n_parm *		us_get_param();
extern	n_parm *		us_find_param();
extern	void			us_proc_local();
extern	void			us_proc_body();
extern	e_node *		us_make_proc_const();
extern	e_node *		us_get_proc_const();

extern	d_node *		us_get_node();
extern	void			us_sym_node_link();
extern	void			us_init_dheader();

#define SDECLH
#endif
