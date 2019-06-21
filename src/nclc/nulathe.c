/*********************************************************************
**    NAME         :  nulathe.c
**       CONTAINS: Regional Mill interface routine
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nulathe.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:08
*********************************************************************/
#include "usysdef.h"
#include "ddef.h"
#include "dselmask.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"

#define PPLEN 80

static char para_table[10][PPLEN] = 
   {
   "0.0",            /* clearance distance */
   "0.0",            /* stock distance x-dir*/
   "0.0",            /* stock distance y-dir*/
   "",              /* post processor */
   "0.1",           /* maximum depth of each cutting pass */
					/* changed from 0. to 0.1 kathy */
   "",              /* post processor */
   "0.0",           /* retract angle */
   "0.0",          /* retract distance */
   "",              /* post processor */
   "",             /* return locator */
   };

static char para_table2[10][PPLEN] = 
   {
   "0.0",            /* stock distance x-dir*/
   "0.0",            /* stock distance y-dir*/
   "",              /* post processor */
   "0.0",            /* ENGAGE angle */
   "0.0",            /* ENGAGE length  */
   "",              /* post processor */
   "0.0",            /* RETRCT angle */
   "0.0",            /* RETRCT length */
   "",              /* post processor */
   "",              /* return locator */
   };
static UU_LOGICAL invers = UU_FALSE;
#define NO_DEFAULT 0
#define DEFAULT 1
/*********************************************************************
**    E_FUNCTION     : nclu_lathe_rough()
**       Set lathe/rough attributes using forms package.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_lathe_rough()
   
   {
   NCL_cmdbuf cmdbuf;
   int *def[10], *ans[10];
   char clearance_dist[PPLEN], stock_xdir[PPLEN], stock_ydir[PPLEN];
	char post_word1[PPLEN], depth[PPLEN], retrct_angle[PPLEN];
	char retrct_dist[PPLEN], return_loc[PPLEN], post_word2[PPLEN];
	char post_word3[PPLEN]; 
	UU_REAL clearance, xdir, ydir, angle;

   int status;

   uu_denter(UU_MTRC,(us,"ncl_lathe_rough()"));

   ncl_init_cmdbuf(&cmdbuf);
   ncl_add_token(&cmdbuf, NCL_lathe, NCL_nocomma);

   ncl_add_token(&cmdbuf, NCL_rough, NCL_comma);

   def[0] = (int *)para_table[0]; ans[0] = (int *)clearance_dist;
   def[1] = (int *)para_table[1]; ans[1] = (int *)stock_xdir;
   def[2] = (int *)para_table[2]; ans[2] = (int *)stock_ydir;
   def[3] = (int *)para_table[3]; ans[3] = (int *)post_word1;
   def[4] = (int *)para_table[4]; ans[4] = (int *)depth;
   def[5] = (int *)para_table[5]; ans[5] = (int *)post_word2;
   def[6] = (int *)para_table[6]; ans[6] = (int *)retrct_angle;
   def[7] = (int *)para_table[7]; ans[7] = (int *)retrct_dist;
   def[8] = (int *)para_table[8]; ans[8] = (int *)post_word3;
   def[9] = (int *)para_table[9]; ans[9] = (int *)return_loc;
 
   status = ud_form("lrough.frm", def, ans);
   if (status==-1)
	   return -1;
   status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 490, UD_ncl_sh);

   if (status == NCL_DONE)
       status = NCL_OKINPUT;

	if (status == NCL_OKINPUT)
	{
		ncl_get_scalar_value(clearance_dist, &clearance);
		if (clearance != 0.0)
		{
			ncl_add_token(&cmdbuf, NCL_cldist, NCL_comma);
			ncl_add_token(&cmdbuf, clearance_dist, NCL_comma);
			strcpy(para_table[0], clearance_dist);
		}
  
      if (status == NCL_OKINPUT)
      	status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 491, UD_ncl_sh);
      
      
		ncl_get_scalar_value(stock_xdir, &xdir);
		if (xdir != 0.0)
		{
          ncl_add_token(&cmdbuf, NCL_stock, NCL_comma);
          ncl_add_token(&cmdbuf, stock_xdir, NCL_comma);
          strcpy(para_table[1], stock_xdir);

			ncl_get_scalar_value(stock_ydir, &ydir);
          if (ydir != 0.0)
              {
              ncl_add_token(&cmdbuf, stock_ydir, NCL_comma);
              strcpy(para_table[2], stock_ydir);
              }
          }
 
      if (strlen(post_word1) != 0)
          {
          ncl_add_token(&cmdbuf, post_word1, NCL_comma);
          strcpy(para_table[3], post_word1);
          }

      ncl_add_token(&cmdbuf, NCL_depth, NCL_comma);
      ncl_add_token(&cmdbuf, depth, NCL_comma);
      strcpy(para_table[4], depth);

      if (strlen(post_word2) != 0)
          {
          ncl_add_token(&cmdbuf, post_word2, NCL_comma);
          strcpy(para_table[5], post_word2);
          }

      ncl_add_token(&cmdbuf, NCL_cutang, NCL_comma);
      ncl_add_token(&cmdbuf, "180", NCL_comma);
      
		ncl_get_scalar_value(retrct_angle, &angle);
      if (angle != 0.0)
          {
          ncl_add_token(&cmdbuf, NCL_retrct, NCL_comma);
          ncl_add_token(&cmdbuf, retrct_angle, NCL_comma);
          strcpy(para_table[6], retrct_angle);
          ncl_add_token(&cmdbuf, retrct_dist, NCL_comma);
          strcpy(para_table[7], retrct_dist);
          }

      if (strlen(post_word3) != 0)
          {
          ncl_add_token(&cmdbuf, post_word3, NCL_comma);
          strcpy(para_table[8], post_word3);
          }

      if (strlen(return_loc) != 0)
          {
          ncl_add_token(&cmdbuf, NCL_return, NCL_comma);
          ncl_add_token(&cmdbuf, return_loc, NCL_nocomma);
          strcpy(para_table[9], return_loc);
          }

   }
   if (status == NCL_OKINPUT)
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }
	return 0;
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_lathe_finish()
**       Set lathe/finish attributes using forms package.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_lathe_finish()
   
   {
   NCL_cmdbuf cmdbuf;
   int *def[11], *ans[11];
   char  stock_x[PPLEN], engage_angle[PPLEN], engage_length[PPLEN];
	char retrct_angle[PPLEN], retrct_length[PPLEN], return_loc[PPLEN];
	char post_word1[PPLEN], post_word2[PPLEN], post_word3[PPLEN];
	char stock_y[PPLEN]; 
	UU_REAL x, y, angle, rangle;
   int status;

   uu_denter(UU_MTRC,(us,"ncl_lathe_finish()"));

   ncl_init_cmdbuf(&cmdbuf);
   ncl_add_token(&cmdbuf, NCL_lathe, NCL_nocomma);

   ncl_add_token(&cmdbuf, NCL_finish, NCL_comma);

   status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 491, UD_ncl_sh);
/*
.....The geometry was being limited to shapes, so when user
.....selected to pick the RETURN-LOCATOR, only a shape was
.....able to be selected instead of a point, so limit geometry
.....to points.  JLS 9/27/99
*/

	ud_lgeo(UU_TRUE, UD_ncl_pt);

   if (status == NCL_DONE)
       status = NCL_OKINPUT;

   def[0] = (int *)para_table2[0]; ans[0] = (int *)stock_x;
   def[1] = (int *)para_table2[1]; ans[1] = (int *)stock_y;
   def[2] = (int *)&invers;        ans[2] = (int *)&invers;
   def[3] = (int *)para_table2[2]; ans[3] = (int *)post_word1;
   def[4] = (int *)para_table2[3]; ans[4] = (int *)engage_angle;
   def[5] = (int *)para_table2[4]; ans[5] = (int *)engage_length;
   def[6] = (int *)para_table2[5]; ans[6] = (int *)post_word2;
   def[7] = (int *)para_table2[6]; ans[7] = (int *)retrct_angle;
   def[8] = (int *)para_table2[7]; ans[8] = (int *)retrct_length;
   def[9] = (int *)para_table2[8]; ans[9] = (int *)post_word3;
   def[10] = (int *)para_table2[9]; ans[10] = (int *)return_loc;

   status = ud_form("lfinish.frm", def, ans);
   if (status==-1)
	   return -1;
	ncl_get_scalar_value(stock_y, &y);
	ncl_get_scalar_value(stock_x, &x);
   if (x != 0.0 || y != 0.0)
       {
       ncl_add_token(&cmdbuf, NCL_stock, NCL_comma);
       ncl_add_token(&cmdbuf, stock_x, NCL_comma);
       strcpy(para_table2[0], stock_x);
       ncl_add_token(&cmdbuf, stock_y, NCL_comma);
       strcpy(para_table2[1], stock_y);
       }
 
   if (invers)
       {
       ncl_add_token(&cmdbuf, NCL_invers, NCL_comma);
       }

   if (strlen(post_word1) != 0)
       {
       ncl_add_token(&cmdbuf, post_word1, NCL_comma);
       strcpy(para_table2[2], post_word1);
       }

	ncl_get_scalar_value(engage_angle, &angle);
   if (angle != 0.0)
       {
       ncl_add_token(&cmdbuf, NCL_engage, NCL_comma);
       ncl_add_token(&cmdbuf, engage_angle, NCL_comma);
       strcpy(para_table2[3], engage_angle);
       ncl_add_token(&cmdbuf, engage_length, NCL_comma);
       strcpy(para_table2[4], engage_length);
       }

   if (strlen(post_word2) != 0)
       {
       ncl_add_token(&cmdbuf, post_word2, NCL_comma);
       strcpy(para_table2[5], post_word2);
       }

	ncl_get_scalar_value(retrct_angle, &rangle);
   if (rangle != 0.0)
       {
       ncl_add_token(&cmdbuf, NCL_retrct, NCL_comma);
       ncl_add_token(&cmdbuf, retrct_angle, NCL_comma);
       strcpy(para_table2[6], retrct_angle);
       ncl_add_token(&cmdbuf, retrct_length, NCL_comma);
       strcpy(para_table2[7], retrct_length);
       }

   if (strlen(post_word3) != 0)
       {
       ncl_add_token(&cmdbuf, post_word3, NCL_comma);
       strcpy(para_table2[8], post_word3);
       }

   if (strlen(return_loc) != 0)
       {
       ncl_add_token(&cmdbuf, NCL_return, NCL_comma);
       ncl_add_token(&cmdbuf, return_loc, NCL_nocomma);
       strcpy(para_table2[9], return_loc);
       }

   if (status == NCL_OKINPUT)
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }
	return 0;
   uu_dexit;
   }
