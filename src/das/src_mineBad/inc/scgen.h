/*********************************************************************
**    NAME         : scgen.h 
**       Header file for SAL C-code generation 
**		CONTAINS:
**			scgexp1:
**				stream = us_cgen_integer( expr )
**				stream = us_cgen_real( expr )
**				stream = us_cgen_vref( expr )
**				stream = us_cgen_variable( var )
**				us_cgen_real_range( expr, *from, *count )
**				us_cgen_integer_range( expr, *from, *count )
**				name = us_get_enum_name( type )
**				stream = us_cgen_const_name( expr )
**				stream = us_cgen_sizeof( type )
**
**			scgexp2:
**				stream = us_cgen_coord( expr )
**				us_cgen_coord_assign( expr, result )
**				stream = us_cgen_transf( expr )
**				us_cgen_transf_assign( expr, result )
**				stream = us_cgen_cfref( expr, *temp_stream )
**				us_cgen_cfref_assign( expr, temp_stream )
**
**			scgexp3:
**				stream = us_cgen_character( expr )
**				stream = us_cgen_string( expr )
**				us_cgen_string_assign( expr, result )
**				stream = us_cgen_bit( expr )
**				stream = us_cgen_set( expr )
**				us_cgen_set_assign( expr, result )
**				us_cgen_sindex_assign( expr, expr_stream )
**				us_cgen_ssub_assign( expr, expr_stream )
**				us_cgen_bindex_assign( expr, expr_stream )
**				us_cgen_bsub_assign( expr, expr_stream )
**
**			scgexp4:
**				stream = us_cgen_array( expr )
**				us_cgen_array_assign( expr, result )
**				stream = us_cgen_list( expr )
**				us_cgen_list_init( vref_stream, list_type )
**				us_cgen_list_assign( expr, result )
**				stream = us_cgen_record( expr )
**				us_cgen_record_assign( expr, result )
**				us_cgen_packed_assign( expr, result )
**				stream = us_cgen_variant( expr )
**				us_cgen_variant_assign( expr, result )
**				us_cgen_pfref_assign( expr, temp_stream )
**
**			scgexp5:
**				stream = us_cgen_procedure( expr )
**				stream = us_cgen_call( call )
**				us_cgen_call_assign( call, result )
**
**			scgexp6:
**				stream = us_cgen_logical( expr )
**
**			scgexp7:
**				stream = us_cgen_obj_key( expr )
**				stream = us_cgen_obj_data( expr )
**				us_cgen_obj_data_assign( expr, result )
**				stream = us_cgen_class( expr )
**				stream = us_cgen_message( expr )
**
**			scgstat:
**				reachable = us_cgen_statements( slist, cont_label )
**				us_cgen_assign( expr, result )
**				reachable = us_cgen_compound_statement( slist, cont_label )
**				us_cgen_simple_assign( left, right )
**
**			scgloop:
**				us_cgen_do_statement( st )
**				us_cgen_for_statement( st )
**				reachable = us_cgen_select_statement( st )
**
**			scgio:
**				us_cgen_write( st )
**
**			scglist:
**				us_cgen_append_st( st )
**				us_cgen_insert_st( st )
**				us_cgen_delete_st( st )
**				us_cgen_free_st( st )
**				us_cgen_sort_st( st )
**
**			scgfile:
**				us_cgen_file( file_node )
**				us_cgen_prolog()
**
**			scgdecl1:
**				us_cgen_predefs( predefs )
**				us_cgen_globals( globals )
**				us_cgen_h_globals( globals )
**				us_cgen_locals( locals )
**				us_cgen_clear_types( table )
**
**			scgdecl2:
**				stream = us_cgen_declarator( type, decl_name )
**				stream = us_cgen_procc_decl( type, proc_name, parms )
**				us_cgen_type_decl( type )
**				status = us_cgen_type_copy( type )
**				us_cgen_variable_decl( type, class_name, decl_name, sal_name )
**				us_cgen_field_id( field, id )
**				us_cgen_case_id( type_num, vc, id )
**				us_cgen_struct( tag_name, decl_name, field )
**				us_cgen_field_list( field )
**				us_cgen_flist_type_decls( field )
**
**			scgproc:
**				us_cgen_proc_defs( globals )
**				id = us_cgen_proc_id()
**				us_cgen_return( expr )
**
**			scgout:
**				status = us_cgout_open( fname )
**				us_cgout_close()
**				us_cgout_set_current( stream )
**				us_cgout_current()
**				us_cgout_item( item )
**				us_cgout_stream( stream )
**				us_cgout_item_to_current( item )
**				us_cgout_stream_to_current( stream )
**				us_cgout_print_current()
**				stream = us_cgout_item_to_stream( stream, item )
**				stream = us_cgout_stream_to_stream( stream, s )
**				stream = us_cgout_integer( number )
**				stream = us_cgout_real( number )
**				stream = us_cgout_string( string )
**				stream = us_cgout_character( char )
**				stream = us_cgout_format(format,stream1, stream2, stream3, stream4 )
**				stream = us_cgout_dump( mess, stream )
**
**			us_cgen_file
**			us_cgen_path
**			us_temps_generated
**			us_list_temps
**
**			stream = us_cgen_expression( expr )
**			stream = us_cgen_vref_expr( expr )
**			stream = us_cgen_unary( expr, arg_func, op )
**			stream = us_cgen_binary( expr, arg_func, op )
**			stream = US_SIMPLE_TEMPORARY( type )
**			stream = us_cgen_temporary( stat_flag, type, init )
**			stream = us_cgen_pop_list( expr )
**			us_cgen_pop_list_assign( expr, result )
**			us_cgen_copy( type, from, to )
**			status = us_cgen_is_scalar( type )
**			status = us_cgen_is_array( type )
**			status = us_cgen_is_vref( expr )
**			us_cgen_func1( func, arg )
**			us_cgen_func2( func, arg1, arg2 )
**			us_cgen_func3( func, arg1, arg2, arg3 )
**			stream = us_cgen_proc1( proc, arg )
**			stream = us_cgen_proc2( proc, arg1, arg2 )
**			stream = us_cgen_proc3( proc, arg1, arg2, arg3 )
**			stream = us_cgen_address_of( vref_stream )
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       scgen.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:50
*********************************************************************/

#ifndef SCGENH

#include "ustdio.h"
#include "slist.h"		/* general list def's */

/* typedef for code output streams */
typedef	t_list	*cgout_stream;

extern cgout_stream	us_cgen_integer();
extern cgout_stream	us_cgen_real();
extern cgout_stream	us_cgen_vref();
extern cgout_stream	us_cgen_variable();
extern void				us_cgen_real_range();
extern void				us_cgen_integer_range();
extern char *			us_get_enum_name();
extern cgout_stream	us_cgen_const_name();
extern cgout_stream	us_cgen_sizeof();

extern cgout_stream	us_cgen_coord();
extern void				us_cgen_coord_assign();
extern cgout_stream	us_cgen_transf();
extern void				us_cgen_transf_assign();
extern cgout_stream	us_cgen_cfref();
extern void				us_cgen_cfref_assign();

extern cgout_stream	us_cgen_character();
extern cgout_stream	us_cgen_string();
extern void				us_cgen_string_assign();
extern cgout_stream	us_cgen_bit();
extern cgout_stream	us_cgen_set();
extern void				us_cgen_set_assign();
extern void				us_cgen_sindex_assign();
extern void				us_cgen_ssub_assign();
extern void				us_cgen_bindex_assign();
extern void				us_cgen_bsub_assign();

extern cgout_stream	us_cgen_array();
extern void				us_cgen_array_assign();
extern cgout_stream	us_cgen_list();
extern void				us_cgen_list_assign();
extern void				us_cgen_list_init();
extern cgout_stream	us_cgen_record();
extern void				us_cgen_record_assign();
extern void				us_cgen_packed_assign();
extern cgout_stream	us_cgen_variant();
extern void				us_cgen_variant_assign();
extern void				us_cgen_pfref_assign();

extern cgout_stream	us_cgen_procedure();
extern cgout_stream	us_cgen_call();
extern void				us_cgen_call_assign();

extern cgout_stream	us_cgen_logical();

extern cgout_stream	us_cgen_obj_key();
extern cgout_stream	us_cgen_obj_data();
extern void				us_cgen_obj_data_assign();
extern cgout_stream	us_cgen_class();
extern cgout_stream	us_cgen_message();

extern int				us_cgen_statements();
extern void				us_cgen_assign();
extern int				us_cgen_compound_statement();
extern void				us_cgen_simple_assign();

extern void				us_cgen_do_statement();
extern void				us_cgen_for_statement();
extern int				us_cgen_select_statement();

extern void				us_cgen_write();

extern void				us_cgen_append_st();
extern void				us_cgen_insert_st();
extern void				us_cgen_delete_st();
extern void				us_cgen_free_st();
extern void				us_cgen_sort_st();

extern void				us_cgen_file();
extern void				us_cgen_prolog();

extern void				us_cgen_predefs();
extern void				us_cgen_globals();
extern void				us_cgen_h_globals();
extern void				us_cgen_locals();
extern void				us_cgen_clear_types();

extern cgout_stream	us_cgen_declarator();
extern cgout_stream	us_cgen_procc_decl();
extern void				us_cgen_type_decl();
extern int				us_cgen_type_copy();
extern void				us_cgen_variable_decl();
extern void				us_cgen_field_id();
extern void				us_cgen_case_id();
extern void				us_cgen_struct();
extern void				us_cgen_field_list();
extern void				us_cgen_flist_type_decls();

extern void				us_cgen_proc_defs();
extern int				us_cgen_proc_id();
extern void				us_cgen_return();

extern int				us_cgout_open();
extern void				us_cgout_close();
extern void				us_cgout_set_current();
extern void				us_cgout_current();
extern void				us_cgout_item();
extern void				us_cgout_stream();
extern void				us_cgout_item_to_current();
extern void				us_cgout_stream_to_current();
extern void				us_cgout_print_current();
extern cgout_stream	us_cgout_item_to_stream();
extern cgout_stream	us_cgout_stream_to_stream();
extern cgout_stream	us_cgout_integer();
extern cgout_stream	us_cgout_real();
extern cgout_stream	us_cgout_character();
extern cgout_stream	us_cgout_string();
extern cgout_stream	us_cgout_format();
extern cgout_stream	us_cgout_dump();

#ifndef USCGFILEC
	extern int				us_cgen_id_count;		/* identifier generation counter */
	extern int				us_temps_generated;	/* TRUE if temporaries generated */
	extern t_list *		us_list_temps;			/* list of LIST type temps */
#endif

#ifndef USCGOUTC
	extern FILE		*us_cgout_file;		/* C output file descriptor */
	extern char		*us_cgout_path;		/* pointer to pathname */
#endif

extern cgout_stream		us_cgen_expression();
extern cgout_stream		us_cgen_vref_expr();
extern cgout_stream		us_cgen_unary();
extern cgout_stream		us_cgen_binary();
extern cgout_stream		us_cgen_temporary();
extern cgout_stream		us_cgen_pop_list();
extern void					us_cgen_pop_list_assign();
extern void					us_cgen_copy();
extern int					us_cgen_is_scalar();
extern int					us_cgen_is_array();
extern int					us_cgen_is_vref();
extern cgout_stream		us_cgen_func1();
extern cgout_stream		us_cgen_func2();
extern cgout_stream		us_cgen_func3();
extern void					us_cgen_proc1();
extern void					us_cgen_proc2();
extern void					us_cgen_proc3();
extern cgout_stream		us_cgen_address_of();

#define US_SIMPLE_TEMPORARY(type) us_cgen_temporary(UU_FALSE,type,UU_NULL)

#define US_CGEN_IS_COMPLEX_LIST(type) (type==UU_NULL)
/* IMPLEMENT */

#define SCGENH
#endif
