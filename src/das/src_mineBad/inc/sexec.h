/*********************************************************************
**    NAME         : sexec.h 
**       SALEX executive definitions 
**		CONTAINS:
**			us_exec_init()
**			us_exec()
**			us_exec_add( file )
**			us_exec_delete( file )
**			us_exec_list()
**			us_exec_edit( file )
**			us_exec_run( proc )
**			us_exec_cgen( file, h_flag )
**			us_exec_error_mode()
**			us_comp_error_mode()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sexec.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:51
*********************************************************************/

#ifndef USEXECH
#define USEXECH

#include "sstore.h"		/* storage manager def's */
#include "slist.h"		/* list def's */
#include "ssym.h"			/* symbol table def's */
#include "sinput.h"		/* input def's */

/* structure for file name list nodes */
typedef struct t_fl
{
	char				*fl_name;		/* file name */
	int				fl_flag;			/* TRUE if compilation is up-to-date */
	st_block			*fl_store;		/* pointer to store */
	st_table			*fl_globals;	/* pointer to global symbol table */
	t_list			*fl_init_list;	/* pointer to initializer list */
	short				fl_init_flag;	/* initializer flag, 
												TRUE if variables need initializing */
	n_modinc			*modlist;		/* module/include list */
	struct t_fl		*fl_forw;		/* forward pointer to next file in list */
	struct t_fl		*fl_back;		/* backward pointer to next file in list */
} n_flist;

#ifndef USEXECC
	extern n_flist	*us_file_list;
#endif

extern void	us_exec_init();
extern void	us_exec();
extern void	us_exec_add();
extern void	us_exec_delete();
extern void	us_exec_list();
extern void	us_exec_edit();
extern void	us_exec_run();
extern void	us_exec_cgen();
extern void	us_exec_error_mode();
extern void	us_comp_error_mode();

#endif
