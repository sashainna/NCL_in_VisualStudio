/*********************************************************************
**    NAME         :  nuprog.c
**       CONTAINS: User interface routines for program creation.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuprog.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:13
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "modef.h"

#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : nclu_call()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_call()

   {
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_call"));

   ncl_init_cmdbuf(&cmdbuf);
   status = NCL_OKINPUT;

   if (status == NCL_OKINPUT)
      status = ncl_add_token(&cmdbuf, NCL_calls, NCL_nocomma);

   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 190, NCL_comma);

   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 191, NCL_comma);

   if ((status == NCL_OKINPUT) || (status == NCL_DONE))
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_fini()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_fini()

   {
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_fini"));
   
   ncl_init_cmdbuf(&cmdbuf);

   status = ncl_add_token(&cmdbuf, NCL_fini, NCL_nocomma);

   ncl_add_cmdbuf(&cmdbuf);
   ncl_call(&cmdbuf);

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_if()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_if()

   {
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_if"));

   ncl_init_cmdbuf(&cmdbuf);

   ncl_add_token(&cmdbuf, NCL_if, NCL_nocomma);
   ncl_add_token(&cmdbuf, "( ", NCL_nocomma);

   status = ncl_add_str(&cmdbuf, 192, NCL_nocomma);

   if (status == NCL_OKINPUT)
      status = ncl_add_token(&cmdbuf, " ) ", 2);

   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 193, NCL_comma);

   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 194, NCL_comma);

   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 195, NCL_nocomma);

   if ((status == NCL_OKINPUT) || (status == NCL_DONE))
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_jumpto()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_jumpto()

   {
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_jumpto"));

   ncl_init_cmdbuf(&cmdbuf);
   status = NCL_OKINPUT;

   if (!NCL_auto_label)
      status = ncl_add_name(&cmdbuf, 1);

   if (status == NCL_OKINPUT)
      status = ncl_add_token(&cmdbuf, NCL_jumpto, NCL_nocomma);

   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 196, NCL_nocomma);

   if ((status == NCL_OKINPUT) || (status == NCL_DONE))
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }

   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : nclu_undo()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_undo()

   {
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_undo"));

   ncl_init_cmdbuf(&cmdbuf);
   status = NCL_OKINPUT;

   if (!NCL_auto_label)
      status = ncl_add_name(&cmdbuf, 1);

   if (status == NCL_OKINPUT)
      status = ncl_add_token(&cmdbuf, NCL_undo, NCL_nocomma);

   if ((status == NCL_OKINPUT) || (status == NCL_DONE))
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_loopnd()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_loopnd()

   {
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_loopnd"));

   ncl_init_cmdbuf(&cmdbuf);
   status = NCL_OKINPUT;

   if (!NCL_auto_label)
      status = ncl_add_name(&cmdbuf, 1);

   if (status == NCL_OKINPUT)
      status = ncl_add_token(&cmdbuf, NCL_loopnd, NCL_nocomma);

   if ((status == NCL_OKINPUT) || (status == NCL_DONE))
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }

   clswin();

   uu_dexit;
   }
/*********************************************************************
**    E_FUNCTION     : nclu_doloop_end()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_doloop_end()

   {
   NCL_cmdbuf cmdbuf;
   int status;
   int i;
   char *ptr;

   uu_denter(UU_MTRC,(us,"nclu_doloop_end"));

   ncl_init_cmdbuf(&cmdbuf);
   status = NCL_OKINPUT;

   if (!NCL_auto_label)
      status = ncl_add_name(&cmdbuf, 1);

   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 229, NCL_nocomma);
	
   if ((status == NCL_OKINPUT) || (status == NCL_DONE))
      {
	  if (cmdbuf.cur_str[strlen(cmdbuf.cur_str) - 1] != ':')
		status = ncl_add_token(&cmdbuf, ":", NCL_nocomma);

	  if (status == NCL_OKINPUT)
		{
		  ncl_add_cmdbuf(&cmdbuf);
		  ncl_call(&cmdbuf);
		}
      }
	clswin();

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_loopst()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_loopst()

   {
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_loopst"));

   ncl_init_cmdbuf(&cmdbuf);
   status = NCL_OKINPUT;

   if (!NCL_auto_label)
      status = ncl_add_name(&cmdbuf,1);

   if (status == NCL_OKINPUT)
      status = ncl_add_token(&cmdbuf, NCL_loopst, NCL_nocomma);

   if ((status == NCL_OKINPUT) || (status == NCL_DONE))
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_doloop()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_doloop()

   {
   NCL_cmdbuf cmdbuf;
   int status;
   char *ptr;

   uu_denter(UU_MTRC,(us,"nclu_doloop"));

   ncl_init_cmdbuf(&cmdbuf);
   status = NCL_OKINPUT;

   if (!NCL_auto_label)
      status = ncl_add_name(&cmdbuf,1);

   if (status == NCL_OKINPUT)
      status = ncl_add_token(&cmdbuf, NCL_doloop, NCL_nocomma);

   /** prompt for label **/
   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 229, NCL_comma);

   /** prompt for loop counter **/
   if (status == NCL_OKINPUT)
      status = ncl_add_name(&cmdbuf, 230);

   /** prompt for initial value **/
   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 231, NCL_comma);

   /** prompt for limit value **/
   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 232, NCL_comma);

   if (status == NCL_OKINPUT)
      status = ncl_add_token(&cmdbuf, NCL_incr, NCL_comma);

   /** prompt for increment value **/
   if (status == NCL_OKINPUT)
	  {
      status = ncl_add_str(&cmdbuf, 233, NCL_nocomma);

	  if (cmdbuf.cur_str[strlen(cmdbuf.cur_str) - 5] == 'I')
		  status = NCL_NOINPUT;
	  }

   /** send cmdbuf to ncl **/
   if (status == NCL_OKINPUT)
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }
   else if ((status == NCL_NOINPUT) || (status == NCL_DONE))
	  {
	  status = ncl_del_token(&cmdbuf, NCL_incr, NCL_comma);

	  if (status == NCL_OKINPUT)
		{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}
	  }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_termac()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_termac()

   {
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_termac"));

   ncl_init_cmdbuf(&cmdbuf);
   status = NCL_OKINPUT;

   if (!NCL_auto_label)
      status = ncl_add_name(&cmdbuf,1);

   if (status == NCL_OKINPUT)
      status = ncl_add_token(&cmdbuf, NCL_termac, NCL_nocomma);

   if ((status == NCL_OKINPUT) || (status == NCL_DONE))
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }

   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : nclu_one_cmd()
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_one_cmd()

   {
   NCL_cmdbuf cmdbuf;
   int status;

   uu_denter(UU_MTRC,(us,"nclu_one_cmd"));

   ncl_init_cmdbuf(&cmdbuf);
   status = NCL_OKINPUT;

   if (!NCL_auto_label)
      status = ncl_add_name(&cmdbuf,1);

   if (status == NCL_OKINPUT)
      status = ncl_add_str(&cmdbuf, 197, NCL_nocomma);

   if ((status == NCL_OKINPUT) || (status == NCL_DONE))
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }

   uu_dexit;
   }

