/*********************************************************************
**    NAME         : sdebug.h 
**       Header for general debugging aids 
**		CONTAINS:
**			us_print_exp( expr )
**			us_print_variable( var )
**			us_print_type( type )
**			us_print_field( field )
**			us_print_flist( flist )
**			us_print_param( parm )
**			us_print_proc( proc )
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sdebug.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:50
*********************************************************************/

#ifndef SDEBUGH


extern void us_print_exp();
extern void us_print_variable();
extern void us_print_type();
extern void us_print_field();
extern void us_print_flist();
extern void us_print_param();
extern void us_print_proc();


#define SDEBUGH
#endif
