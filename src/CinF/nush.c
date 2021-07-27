/*********************************************************************
**    NAME         :  nush.c
**       CONTAINS: User interface routines for shape creation.
**			nclu_create_shape()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nush.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:15
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

extern UU_LOGICAL  ud_gnxt();
/*********************************************************************
**    E_FUNCTION     : nclu_create_shape()
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

nclu_create_shape()

{
	NCL_cmdbuf cmdbuf;
	int status;
	int rel_num;

	char str[256];
	UM_PLOCREC pick;
	UU_KEY_ID key;
	UU_LOGICAL gnxt_init, cont;
	int numint;
	int inpmode;
	int reltyp;
	struct UC_entitydatabag e;

	uu_denter(UU_MTRC,(us,"nclu_create_shape"));

	while (UU_TRUE)
	{
		gnxt_init = UU_TRUE;
		cont = UU_TRUE;
		ncl_init_cmdbuf(&cmdbuf);
		status = NCL_OKINPUT;

		if (!NCL_auto_label)
			status = ncl_add_name(&cmdbuf, 1);

		if (status == NCL_OKINPUT)
			status = ncl_add_token(&cmdbuf, NCL_nsh, NCL_nocomma);
/*
.....Now the SELECT menu will appear so that the user may pick a chain
.....Limit the geometry to circles and lines.  JLS 5/11/99
..... Added curves and splines. Eduard 4/28/00
*/

		ud_lgeo(UU_TRUE, UD_ncl_lncicv);

		ud_ldas(UD_DASSELECT, UA_NCL,479,UU_NULL,/*&pick,*/1,&numint,UD_NODEFAULT);
		if (numint<1) goto done2;
		if (status == NCL_OKINPUT || status == NCL_NOINPUT) do
		{
/*
			ud_lgeo(UU_TRUE, UD_ncl_lncish);
*/
			ud_lgeo(UU_TRUE, UD_ncl_lncicv);

			cont = ud_gnxt(gnxt_init, UU_NULL, &key, 1);
/*
.....If all entities have been gotten, exit the loop.
*/
			if (!cont)
				 goto done;
			if (status == UD_DASALTACTION)
			{
				inpmode = UD_DASSTRING;
				status = NCL_ALTACTION;
				status = ncl_add_str(&cmdbuf, 481,NCL_comma);
			}

			else if (numint > 0)
			{
				e.key=key;
				ur_retrieve_data_fixed(&e);
				ncl_get_label(&e,str);
				um_retrieve_data_relnum(key, &reltyp);
				gnxt_init = UU_FALSE;
/*
.....Build up the cmdbuf string 
*/
				ncl_add_token(&cmdbuf, str, NCL_comma);
				status = NCL_OKINPUT;
			}
			else
				status = NCL_NOINPUT;
			if (status == NCL_NOINPUT) break;

		} while (cont);

done:;
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);

	}
done2:
	uu_dexit;
}
