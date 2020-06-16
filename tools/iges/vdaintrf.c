#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**    NAME         : vdaintrf.c
**       CONTAINS:
**             vda_intro
**             vda_menu
**             vda_open_listing_file
**             vda_get_file
**             vda_get_label_type
**    COPYRIGHT 1989 (c) MILLS DATA SYSTEMS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       vdaintrf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:55
*********************************************************************/
#include "tiges.h"
#include "vda.h"
#include "xenv1.h"
#include <stdio.h>
#include <curses.h>

#if UU_COMP==UU_VAXVMS
#include iodef
extern int UIG_tty_channel;      /* used by SYS$QIOW */
#endif

#if UU_COMP==UU_IRIS4D
#include <fcntl.h>
#endif
#if UU_COMP==UU_RIDGE
#include <fcntl.h>
#endif
#if UU_COMP==UU_APOLLO
#include <fcntl.h>
#endif

#if UU_COMP!=UU_RIDGE
#if UU_COMP!=UU_APOLLO
#if UU_COMP!=UU_VAXVMS
#if UU_COMP!=UU_IRIS4D
#include <sys/file.h>
#endif
#endif
#endif
#endif

#if UU_COMP!=UU_VAXVMS
#include <sys/types.h>
#endif

/*NCL: version is now set in inc/nclver.h */
#include "nclver.h" 	/*change here to update version*/


/********************************************************************
**                                                                 **
**    MENU CHARACTER TABLE                                         **
**                                                                 **
********************************************************************/

static char *mutbl[] = {
 "VDA Translator",
 " 1 Process VDA file",
 " 2 Exit",	
 "       ",
 "INPUT SOURCE",
 " 1 From Tape",
 " 2 From File",
 " 3 Exit",
 "INPUT OPTIONS",
 " 1 File Summary",
 " 2 Create Unibase",
 " 3 Exit",
 "CONVERSION OPTIONS",
 " 1 All VDA Entities",
 " 2 By Layer Range",
 " 3 Exit",
 "ENTITY LABEL OPTIONS",
 " 1 Generate",
 " 2 From File",
 "            ",
 };

#define M1 1	
#define L1 3
#define M2 4
#define L2 4
#define M3 8
#define L3 4
#define M4 12
#define L4 4
#define M5 16
#define L5 4

extern int vda_fd, listfd;
extern char ext_filename[];

/*********************************************************************
**    I_FUNCTION     : vda_intro(status);
**       VDA function to clear the screen, get the VDA data file name
**       from the user, and initialize the record directory.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          status                      file status
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vda_intro(status)

   int *status;                              /* file status */

   {
   char count[10];
   char copy_it[80];
   int option;
   int vda_get_file();
   int vda_menu();

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   dbyte = pbyte = 0;

   /* display main menu */

main:
   option = vda_menu(M1,L1);
	if(option < -1) goto main;

   switch(option)
      {

      /* user selected VDA in */

      case 1:
         
         uig_clear();

         vda_fd=vda_get_file("Please enter the VDA data file name: ",0);
         if( vda_fd < 0 )
            {
            *status = -1;
            uig_str_out("\nError in opening file; press return to repeat",UU_TRUE);
            uig_str_in(p_buff);
            goto main;
            }
         *status = 0;
         break;
      
      /* exit */
      case 2:
         *status = -2;
         return;
      }
   }


/*********************************************************************
**    I_FUNCTION     :  vda_menu(indx,num)
**          Display a menu and return the user option.
**    PARAMETERS   
**       INPUT  : 
**          indx                    Indx into menu table
**          num                     Number of lines in the menu
**       OUTPUT :  
**          none
**    RETURNS      : choice number
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vda_menu(indx,num)
   int indx;
   int num;
   {
   int loc,i,option;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   uig_clear();

   /* display menu */

   if (indx==M1)		/* display version */
	  {
      sprintf(p_buff, "VDA Conversion Program %.3f\n", NCL_version);
	  uig_str_out(p_buff, UU_TRUE);
	  }
   loc = indx;
   for(i=0;i<num;i++)
      {
      uig_str_out(mutbl[loc],UU_FALSE);
      uig_str_out("\n",UU_FALSE);
      loc++;
      }

   /* get users selection */

   uig_prompt(1,&option);

   /* clear screen */

   uig_clear();

   return(option);
   }


/*********************************************************************
**    I_FUNCTION     :  vda_open_listing_file(fname)  
**          Initialize output listing file.
**    PARAMETERS   
**       INPUT  : 
**          fname  					iges input file name
**       OUTPUT :  
**          none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vda_open_listing_file(fname)         
	char fname[];
	{
	int i;
	char inputname[62];
	UX_pathname listname,filename,dir;

/*
.....Create listing file name
*/
	ul_break_fname(fname,dir,filename);
	i = strcspn(filename, ".");
	if (i != 0) filename[i] = '\0';
	ul_build_full_fname(dir,filename,"lst",listname);
/*
.....Create listing file
*/
	listfd = creat(listname, 0664);
	ul_short_filename(listname,inputname,60);
	if (listfd == -1)
		printf("cannot open listing file %s\n", inputname);
	sprintf(p_buff, "Listing file: %s\n", inputname);
	uig_str_out(p_buff, UU_TRUE);
	uig_list_out("V D A   C o n v e r s i o n   P r o g r a m\n", UU_FALSE);
	sprintf(p_buff, "              Version %.3f\n", NCL_version);	
	uig_list_out(p_buff, UU_FALSE);							
	uig_list_out(" \n", UU_FALSE);
	ul_short_filename(fname,inputname,60);
	sprintf(p_buff, "VDA Input File: %s\n", inputname);
	uig_list_out(p_buff, UU_FALSE);
	uig_list_out(" \n", UU_FALSE);
	}


/*********************************************************************
**    I_FUNCTION     :  vda_get_file(prompt, flag)
**          Get a file for the application
**    PARAMETERS   
**       INPUT  : 
**          prompt                  prompt string     
**          flag                    access permission flag
**       OUTPUT :  
**          output
**    RETURNS      : file pointer
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vda_get_file(prompt, flag)
char prompt[];
int flag;
	{
	UX_pathname filename;
	int fd;
	int status;

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	uig_str_out(prompt,UU_TRUE);
	filename[0] = '\0';
	uig_str_in(filename);
	if(strlen(filename) < 1)
		{
		fd = -1;
		return(fd);
		}
	strcpy(ext_filename, filename);		/* listing file */

	/*NCL: to handle writes to screen correctly on VAX */
	uig_tty_mode(SCROLL);

	/* create an unblocked version of the VDA input file called vda.tmp */
	uig_unblock(filename,"vda.tmp");

	if ((status=access(filename,0)) != 0)		/* file does'nt exist */
		fd = -1;
	else
		{
		fd = uig_open(filename, flag);
		}

	/*NCL: to handle writes to screen correctly on VAX */
	uig_tty_mode(RESET);

	return(fd);
	}

/*********************************************************************
**    I_FUNCTION     :  vda_get_label_type
**          Get label type from user.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

vda_get_label_type() 
	{
	int option;

	option = 0;
	while (option < 1 || option > 2)	
		option = vda_menu(M5,L5);
	label_type = option;
	switch (option)	
		{
		case 1:
			uig_list_out("Labels generated.\n", UU_FALSE);
			uig_list_out(" \n", UU_FALSE);
			break;
		case 2:
			uig_list_out("Labels from file.\n", UU_FALSE);
			uig_list_out(" \n", UU_FALSE);
			break;
		}
   }

#endif
