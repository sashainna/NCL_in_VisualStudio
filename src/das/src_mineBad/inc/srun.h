/*********************************************************************
**    NAME         : srun.h 
**       Run-time system header file 
**		CONTAINS:
**			us_run_init( global_size )
**			us_run_close()
**			us_run_stop()
**			us_run_break()
**			stat = us_run_stlist( stlist )
**			us_run_write( wst )
**			us_run_store( expr, loc )
**			us_run_print_init()
**			us_run_print_close()
**			us_run_print_exp( expr )
**			us_run_error_mode()
**			us_init_storage( store, size )
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       srun.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:52
*********************************************************************/

#ifndef SRUNH


extern void		us_run_init();
extern void		us_run_close();
extern void		us_run_stop();
extern int		us_run_stlist();
extern void		us_run_write();
extern void		us_run_store();
extern void		us_run_print_init();
extern void		us_run_print_close();
extern void		us_run_print_exp();
extern void		us_run_error_mode();
extern void		us_init_storage();

#ifndef USRUNC
	extern int *	us_run_global;
	extern int		us_run_static;
	extern int *	us_run_frame;
	extern char		*us_run_path;
	extern char		*us_run_pname;
	extern int		us_run_lineno;
	extern int		us_run_trace;
#endif


#define SRUNH
#endif
