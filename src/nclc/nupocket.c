/*********************************************************************
**    NAME         :  nupocket.c
**       CONTAINS:
**        nclu_pocket()            Build APT type pocket statement.
**        nclu_unsubscript()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nupocket.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:11
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "nclcmd.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "mdrel.h"
#include "dselmask.h"

static char para_table[6][65] = {
   "0.25",
   "0.5",
   "0.05",
   "5.0",
   "25.0",
   "10.0",
   };

static UU_LOGICAL  island;
static UU_LOGICAL  pocket_type;

/*********************************************************************
**    E_FUNCTION     : nclu_pocket()
**       Build APT type pocket statement.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_pocket()
   
   {
   NCL_cmdbuf cmdbuf;
   int *def[8], *ans[8];
/*
....use allow input scalar (which is define as char[65])
*/
   char eff_radius[65], rough[65], finish[65], plunge[65], r_fedrate[65],
        f_fedrate[65];
   int status;
   int i;

   uu_denter(UU_MTRC,(us,"nclu_pocket()"));

   def[0] = (int *)para_table[0]; ans[0] = (int *)eff_radius;
   def[1] = (int *)para_table[1]; ans[1] = (int *)rough;
   def[2] = (int *)para_table[2]; ans[2] = (int *)finish;
   def[3] = (int *)para_table[3]; ans[3] = (int *)plunge;
   def[4] = (int *)para_table[4]; ans[4] = (int *)r_fedrate;
   def[5] = (int *)para_table[5]; ans[5] = (int *)f_fedrate;
   def[6] = (int *)&island;       ans[6] = (int *)&island;
   def[7] = (int *)&pocket_type;  ans[7] = (int *)&pocket_type;

	status = ud_form("npocket.frm", def, ans);
	if (status==-1)
		return -1;

   ncl_init_cmdbuf(&cmdbuf);
   ncl_add_token(&cmdbuf, NCL_pocket, NCL_nocomma);
   ncl_add_token(&cmdbuf, eff_radius, NCL_comma);
   strcpy(para_table[0], eff_radius);
   ncl_add_token(&cmdbuf, rough, NCL_comma);
   strcpy(para_table[1], rough);
   ncl_add_token(&cmdbuf, finish, NCL_comma);
   strcpy(para_table[2], finish);
   ncl_add_token(&cmdbuf, plunge, NCL_comma);
   strcpy(para_table[3], plunge);
   ncl_add_token(&cmdbuf, r_fedrate, NCL_comma);
   strcpy(para_table[4], r_fedrate);
   ncl_add_token(&cmdbuf, f_fedrate, NCL_comma);
   strcpy(para_table[5], f_fedrate);
   if(island)
      {
      ncl_add_token(&cmdbuf, "1", NCL_comma);
      }
   else
      {
      ncl_add_token(&cmdbuf, "0", NCL_comma);
      }
   if(pocket_type)
      {
      ncl_add_token(&cmdbuf, "3", NCL_comma);
      }
   else
      {
      ncl_add_token(&cmdbuf, "2", NCL_comma);
      }

   status = NCL_OKINPUT;
   i=0;
   while(i<20 && status == NCL_OKINPUT)
      {
      status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 435, UD_ncl_pt);
      i++;
      }

   if(i > 0)
      {
      ncl_add_cmdbuf(&cmdbuf);
      ncl_call(&cmdbuf);
      }

   uu_dexit;
   return (0);
   }

/*********************************************************************
**    I_FUNCTION     : nclu_unsubscript (label,tstr)
**			Get rid of a subscript, if any. For example, 'ptt(8)' --> 'ptt'
**    PARAMETERS   
**       INPUT  : label
**       OUTPUT : tstr - unsubscripted label
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_unsubscript (label,tstr)
char *label,*tstr;
{
	int k;

	for (k = 0; k < strlen(label) && k < 8; k++)
	{
		if (label[k] != '(') tstr[k] = label[k];
		else break;
	}
	if (k < 8) tstr[k] = '\0';
}
