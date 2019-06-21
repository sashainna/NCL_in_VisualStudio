/*********************************************************************
**    NAME         : serror.h 
**			CONTAINS:
**       	Error output
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       serror.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:51
*********************************************************************/

#ifndef SERRORH


extern void		us_error_init();
extern void		us_error_mode();
extern void		us_error_pop();
extern void		us_error();
extern void		us_error_line();
extern void		us_error_print();
extern void		us_error_abort();

#ifndef USERRORC
	extern char us_strbuf[100];
	extern int	us_error_count;
#endif

/* severity codes */
#define US_SEV_ERROR		1
#define US_SEV_WARNING	2
#define US_SEV_SYSTEM	3


#define SERRORH
#endif
