/*********************************************************************
**    NAME         :  tigintrf.c
**       CONTAINS:
**             uig_intro
**             uig_in_option
**             uig_prompt
**             uig_menu
**             uig_get_range
**             uig_get_curve_toler
**             uig_open_secondary(filename) 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tigintrf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:47
*********************************************************************/
#include "tiges.h"
#include "udebug.h"
#include "xenv1.h"
#include <stdio.h>
#if UU_COMP!=UU_WIN2K
#include <curses.h>
#endif

#if UU_COMP==UU_VAXVMS
#include iodef
extern int UIG_tty_channel;      /* used by SYS$QIOW */
#endif

#if UU_COMP==UU_IRIS || UU_COMP==UU_IRIS4D
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
#if UU_COMP!=UU_IRIS
#if UU_COMP!=UU_IRIS4D
#if UU_COMP!=UU_WIN2K
#include <sys/file.h>
#endif
#endif
#endif
#endif
#endif
#endif

#if UU_COMP!=UU_VAXVMS
#include <sys/types.h>
#endif
/*jkd43: display program version on screen and in listing file */
/*NCL: version is now set in inc/nclver.h */
#include "nclver.h" /*jkd43: change here to update version*/

/* NCL: global value used in igspline.c to set curve fit tolerance */
double ctol = 0.0002;

/*MILLS: double size of paramater record/now set by this var.*/
extern int MAX_PARA_REC;

extern int UG_def_line_wt;

/********************************************************************
**                                                                 **
**    MENU CHARACTER TABLE                                         **
**                                                                 **
********************************************************************/

static char *mutbl[] = {
 "IGES TRANSLATOR",
 " 1 IGES in",
 " 2 IGES out",
 " 3 Exit",	/*jkd51: suppress "Iges out"*/
/* "       ",*/ /*jkd51*/
 "INPUT SOURCE",
 " 1 From Tape",
 " 2 From File",
 " 3 Exit",
 "INPUT OPTIONS",
 " 1 File Summary",
 " 2 Create Unibase",	/*jkd18*/
 " 3 Exit",
 "CONVERSION OPTIONS",
 " 1 All IGES Entities",
 " 2 By Layer Range",
 " 3 Exit",
 "ENTITY LABEL OPTIONS",
 " 1 Generate by IGES",						/* cpp: 92 changed LABEL Menu text */
 " 2 Generate by IGES using Subscripts",
 " 3 From IGES File",
 " 4 From IGES File using Subscripts",
 " 5 From File Using CV Style Labels",
 " 6 From File Using Property Entity",
 " 7 From IGES File Using Max 6 Chars",
 };

#define M1 1	/*jkd43*/
#define L1 3	/*jkd43*/
#define M2 4
#define L2 4
#define M3 8
#define L3 4
#define M4 12
#define L4 4
#define M5 16
#define L5 8

extern char ext_filename[];	/*jkd52 */

/*********************************************************************
**    I_FUNCTION     : uig_intro(status);
**       IGES function to clear the screen, get the IGES data file name
**       fron the user,and initilize the terminal record pointers.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          status                      file status
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if UU_COMP!=UU_WIN2K
uig_intro(status)

   int *status;                              /* file status */

   {
   char count[10];
   char copy_it[80];
   int uig_open();
   int option;
   int uig_get_file();
   int uig_menu();
   char *p, *ux_getenv();

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_intro"));

   dbyte = pbyte = 0;

   /*MILLS: get size of parameter record */
   if ((p=ux_getenv("MAX_PARA_REC")) != UU_NULL)
      MAX_PARA_REC = atoi(p);

   UG_def_line_wt = 0;
   if ( (p=ux_getenv("UG_DEF_LINE_WT")) && !strcmp(p,"TRUE"))
      UG_def_line_wt = 1;
/*
...   Initialize generation of subscripted labels.
*/
   uig_init_sublab();

   /* display main menu */

main:
   option = uig_menu(M1,L1);
	if(option < -1) goto main;

   switch(option)
      {

      /* user selected IGES in */

      case 1:
         
         /* get users input option */

         /*
         option = uig_menu(M2,L2);
         */
         option = 2;
         uig_clear();

         switch(option)
            {

            /* input from tape to a file */

            case 1:
               uig_str_out("Enter disk file name: ",UU_TRUE);
               uig_str_in(ext_filename);
               uig_str_out("Enter tape blocking size in bytes: ",UU_TRUE);
               uig_str_in(p_buff);
               sscanf(p_buff,"%d",count);
               strcpy(copy_it,"dd if=/dev/rmt12 of=");
               strcat(copy_it,ext_filename);
               strcat(copy_it," ibs=");
               strcat(copy_it,count);
               strcat(copy_it," conv=unblock");
               sprintf(p_buff,"Tape read command to system:\n  %s\n",copy_it);
               uig_str_out(p_buff,UU_FALSE);
               uig_prompt(2,count);
               /*
               ioerr = system(copy_it);
               if(ioerr == 0)
                  {
                  iges_fd = iges_open(ext_filename);
                  uig_terminal_rec();
                  uig_clear();
                  *status = 0;
                  }
               else
                  {
                  *status = -1;
                  goto fexit;
                  }
                                                            */
               *status = -1;
               goto fexit;

            /* disk file exits - get file name and retrieve terminal record */

            case 2:
               iges_fd=uig_get_file("Please enter the IGES data file name: ",0);
               if( iges_fd < 0 )
                  {
                  *status = -1;
                  uig_str_out("\nError in opening file",UU_TRUE);
                  uig_prompt(2, &option);
                  goto main;
                  }
               sect_ptr[0] = sect_ptr[1] = sect_ptr[2] = sect_ptr[3] = 0;
               uig_terminal_rec();                       /*  terminal record */
               if(sect_ptr[2] <= 0) 
                  {
                  *status = -1;
                  uig_str_out("\nError in reading file",UU_TRUE);
                  uig_prompt(2, &option);
                  goto main;
                  }
               if(sect_ptr[3] < ((sect_ptr[2]/2)-1))
                  {
                  *status = -1;
                  uig_str_out("\nError in Terminal block",UU_TRUE);
                  uig_prompt(2, &option);
                  goto main;
                  }
               uig_clear();                           /* clear the screen  */
               *status = 0;
               break;

            case 3:
               *status = -1;
               break;
            }
         break;
      
      /* IGES out */
        case 2:
  			*status = uio_main();
  			goto main;
           break; 
      
      /* exit */
      case 3:
         *status = -2;
         goto fexit;
      }

fexit:;
	uu_dexit;
   }
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_in_option(option)
**        Get the next choice from the user. 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          option                   option number
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if UU_COMP!=UU_WIN2K
uig_in_option(option)
   int *option;                           /* option number selected */
   {
   int loc;
   int uig_menu();

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_in_option"));

   /* display menu */

   loc = uig_menu(M3,L3);
   *option = loc;

	uu_dexit;
   }
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_prompt(num,ans)
**       Pause for response from the user.
**    PARAMETERS   
**       INPUT  : 
**          num                  prompt number
**          ans                  answer if required   
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if UU_COMP!=UU_WIN2K
uig_prompt(num,ans)
   int num;                            /* prompt number */
   int *ans;                           /* answer if required */
   {
   char c[80];

#if UU_COMP==UU_VAXVMS
   short stat;

   /* termination mask for SYS$QIOW - all chars are terminators */
   unsigned long term_mask[2] = {0,0};

   /* i/o status buffer */
   short iosb[4];
#endif

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_prompt"));

   switch(num)
      {
      case 1:                          /* get menu selection */
         uig_str_out("choice number:",UU_TRUE);
         /*
         uig_str_in(c);
         */
#if UU_COMP == UU_VAXVMS

         /* c[0] must be initialized becase SYS$QIOW reads just 8 bits
         ** into this 32 bit "buffer"
         */
         c[0] = 0;

         /* don't use getch() - it requires a <CR> */
         SYS$QIOW(0, UIG_tty_channel, (IO$_READVBLK | IO$M_NOECHO | 
            IO$M_NOFILTR), iosb, 0, 0, c, 1, 0, term_mask, 0, 0);
         stat = iosb[0];
         if (stat != SS$_NORMAL)
            printf("uig_prompt: SYS$QIOW bad status %d", stat);
#else
         c[0] = getch();
#endif
			if(c[0] == '\15')
				{
				*ans = -1;
				}
			else
				{
         	c[1] = '\0';
				sscanf(c,"%d",ans);
				}
         uig_clear();
         break;
      case 2:                          /* pause for user response */
         uig_str_out("\nEnter c to continue:",UU_TRUE);
         /*
         uig_str_in(c);
         */
#if UU_COMP == UU_VAXVMS

         /* c[0] must be initialized becase SYS$QIOW reads just 8 bits
         ** into this 32 bit "buffer"
         */
         c[0] = 0;

         /* don't use getch() - it requires a <CR> */
         SYS$QIOW(0, UIG_tty_channel, (IO$_READVBLK | IO$M_NOECHO | 
            IO$M_NOFILTR), iosb, 0, 0, c, 1, 0, term_mask, 0, 0);
         stat = iosb[0];
         if (stat != SS$_NORMAL)
            printf("uig_prompt: SYS$QIOW bad status %d", stat);
#else
         c[0] = getch();
#endif
         uig_clear();
         break;
      }

	uu_dexit;
   }
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_menu(indx,num)
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
#if UU_COMP!=UU_WIN2K
uig_menu(indx,num)
   int indx;
   int num;
   {
   int loc,i,option;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   uig_clear();

   /* display menu */

   if (indx==M1)		/*jkd43: display version */
	  {
      sprintf(p_buff, "IGES Conversion Program %.3f\n", NCL_version);
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
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_get_range(num,range)
**          Get layer number ranges from user.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          num                     Number of ranges defined
**          range                   ranges 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#if UU_COMP!=UU_WIN2K
uig_get_range(num,range)
   int *num;
   int range[10][2];
   {
   int i,j,num1,num2,option;
   UU_LOGICAL finish;
   char buf[80];

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

	uu_denter(UU_MTRC,(us,"uig_get_data"));

   /* display menu */

   option = uig_menu(M4,L4);
	if(option < 0)
		{
		*num = -1;
		uu_dexit;
		return;
		}
	else
		{
	   switch(option)
  	    {
  	    /* all entities */

  	    case 1:
  	       *num = 0;
		   uig_list_out("All layers selected.\n", UU_FALSE);	/*jkd32 */
		   uig_list_out(" \n", UU_FALSE);
			 uu_dexit;
  	       return;
  	       break;
  	    case 2:              /* by layer number */
  	       j = 0;
  	       finish = UU_FALSE;
  	       echo();
  	       while(finish != UU_TRUE)
  	          {
  	          num1 = num2 = -1;		/*jkd46: let 0 0 be valid range*/
  	          uig_str_out("\nEnter layer range r1 r2 : ",UU_TRUE);
  	          uig_str_in(buf);
  	          if (strlen(buf) > 1)
  	          sscanf(buf,"%d %d",&num1,&num2);
  	          if((num1 == -1) && (num2 == -1))	/*jkd46*/
  	             {
  	             finish = UU_TRUE;
  	             }
  	          else
  	             {
  	             if (num2 == -1) num2 = num1;
  	             range[j][0] = num1;
  	             range[j][1] = num2;
  	             sprintf(p_buff, "\nSelected layers %d to %d\n", num1, num2);	/*jkd32 */
  	             uig_list_out(p_buff, UU_TRUE);
  	             j++;
  	             if (j > 9)
  	                {
  	                finish = UU_TRUE;
  	                uig_str_out("\nLayer selection complete",UU_TRUE);
  	                uig_prompt(2,&num1);
  	                }
  	             }
  	          }
  	       *num = j;
          if (j == 0)
		   		uig_list_out("All layers selected.\n", UU_FALSE);	/*jkd32 */
          uig_list_out(" \n", UU_FALSE);
  	       noecho();
  	       uig_clear();
			 uu_dexit;
  	       return;
  	       break;
  	    case 3:
  	       *num = -1;
			 uu_dexit;
  	       return;
  	       break;
  	    }
	  }
   }
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_get_label_type
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
#if UU_COMP!=UU_WIN2K
uig_get_label_type()    /*jkd15: new labels */
	{
	int option;

	option = 0;
	while (option < 1 || option > L5-1)	
		option = uig_menu(M5,L5);
	label_type = option;
	switch (option)		/*jkd32*/
		{
		case 1:
			uig_list_out("Labels generated.\n\n", UU_FALSE);
			break;
		case 2:
			uig_list_out("Labels generated using subscripts.\n\n", UU_FALSE);
			break;
		case 3:
			uig_list_out("Labels from file.\n\n", UU_FALSE);
			break;
		case 4:
			uig_list_out("Labels from file using subscripts.\n\n", UU_FALSE);
			break;
		case 5:
			uig_list_out("Labels from file using CV subscript.\n\n", UU_FALSE);
			break;
		case 6:
			uig_list_out("Labels from file using property.\n\n", UU_FALSE);
			break;
		}
   }
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_get_curve_toler
**          Get curve fit tolerance from user.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#if UU_COMP!=UU_WIN2K
int
uig_get_curve_toler(units)
int units;		/* current units */
	{
	char toler[20];
	double uig_atof();

	uig_clear();					

	if (units == 1)
		{
		ctol = 0.0002;
		uig_str_out("Enter NCL Curve fit tolerance (<return> for .0002 IN): ",
			UU_TRUE);			
		}
	else
		{
		ctol = 0.005;
		uig_str_out("Enter NCL Curve fit tolerance (<return> for .005 MM): ",
			UU_TRUE);			
		}

	/* get value from user */
	uig_str_in(toler);

	if(strlen(toler)> 1)	/* modify default */				
		ctol = uig_atof(toler);

	sprintf(p_buff,"Curve Fit Tolerance = %.3f\n", ctol);
	uig_list_out(p_buff, UU_TRUE);					
	uig_list_out(" \n", UU_FALSE);

	/* scale tolerance in case units are other than inches */
	ctol = ctol * unit_scale; 	

	return 0;
	}
#endif
/*********************************************************************
**    I_FUNCTION     :  uig_open_secondary()
**
**             Open secondary unibase to be used for labeling
**
**    PARAMETERS   
**       INPUT  : 
**          filename          Name of unibase to be opened
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS if unibase is opened
**                   UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uig_open_secondary(filename,ver_index) 
char filename[];
int *ver_index;
{
	UX_pathname tmp_file;
	UX_pathname fnameu;
	int  status,iostat,mode,index;
	int i;

	index = 0;
	strcpy(UIG_label_unibase, filename);
/*
.....Make sure file exists
*/
	mode = 0;
	status = ux_file_inquire(UU_NULL,UU_NULL,UIG_label_unibase,
					UU_NULL,UU_NULL, &mode,&iostat,fnameu,UX_NPRTERRS);
	if (status != 0 || iostat!=-28)
	{
#if UU_COMP!=UU_WIN2K
		uig_mfmsg_box(NULL, "ERROR", "Unable to open unibase file");
#else
		iges_wnt_msg(NULL, "ERROR", "Unable to open unibase file");
#endif
		status = UU_FAILURE;
		goto done;
	}
	index = ux_get_ver_index();
	status = ur_reset_unibase();
	if(status == UU_SUCCESS)
		status = ur_lp02(fnameu,0);

done:;
	*ver_index = index;
	return(status);
}
