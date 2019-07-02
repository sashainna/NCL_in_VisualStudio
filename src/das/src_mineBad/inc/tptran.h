/*********************************************************************
**    NAME         :  patran.h
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tptran.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:57
*********************************************************************/

#ifndef PATRAN


  /***************************************************************************/
  /*                                                                         */
  /*                                                                         */
  /*   include file for PATRAN translator project                            */
  /*                                                                         */
  /*                                                                         */
  /***************************************************************************/
  /*                                                                         */
  /*  include the standard UNICAD system defines                             */
  /*                                                                         */
#include "usysdef.h"
  /*                                                                     */
  /* PATRAN file designator                                                */ 
  /*                                                                     */
 int pat_fd;                  /* PATRAN disc file designator */
  /*                                                                     */
  /* PATRAN part file name 
  /*                                                                     */
char pat_save_file_name[80];

#define STANDARD     0
#define SCROLL       1
#define RESET        2
#define INIT         3

#define PATRAN
#endif
