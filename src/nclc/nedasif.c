/*********************************************************************
**    NAME         : nedasif.c
**       CONTAINS: routines to interface to DAS 
**			int ncl_add_str(cmdbuf, prompt, comma)
**			int ncl_add_str1(cmdbuf, prompt, comma)
**			int ncl_add_label(cmdbuf, prompt, selmask)
**			int ncl_add_label1(cmdbuf, prompt, selmask)
**			int ncl_add_label_rel(cmdbuf, prompt, selmask, reltyp)
**			int ncl_add_label_rel1(cmdbuf, prompt, selmask, reltyp)
**			int ncl_add_name(cmdbuf, prompt)
**			int ncl_add_coord(cmdbuf, prompt, dim)
**			int ncl_add_vector(cmdbuf, prompt)
**			int ncl_add_vector1(cmdbuf, prompt)
**			int ncl_add_length(cmdbuf, prompt)
**			int ncl_add_angle(cmdbuf, prompt)
**			int ncl_add_modifier(cmdbuf, menu)
**       int ncl_add_label_nrpt()
**       int ncl_get_nearpt_dis()
**       int ncl_get_neardis()
**			ncl_nearpt2()
**    COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nedasif.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:30
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "gsegop.h"
#include "gsegac.h"
#include "ginq.h"
#include "gvlib.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mcrv.h"
#include "modef.h"
#include "mdebug.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdeval.h"
#include "dinput.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclfc.h"

extern int ug_viwseg;     /* segment number for output clip/exp routines */

UU_LOGICAL ncl_where = UU_FALSE;
extern UD_POPUPREC nmodfy[];

/*
.....MAXPICK defines limit of entities to be picked via the SELECT subsystem.
.....Currently limited by max. num of curves from which to build a composite 
.....curve.
... aak 09-dec-1997: transferred NCL_MAXPICK to "nclcmd.h"
#define NCL_MAXPICK 100
*/

/*********************************************************************
**    E_FUNCTION     : int ncl_add_str(cmdbuf, prompt, comma)
**       Prompt the user to enter a text string and add to command buffer.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_str(cmdbuf, prompt, comma)
	NCL_cmdbuf *cmdbuf;
	int prompt;
	UU_LOGICAL comma;

	{
	char str[256];
	int ret_status;

	ret_status = ncl_get_str(str,prompt);
	if (ret_status == NCL_DONE)
	{
		ncl_add_token(cmdbuf, str, comma);
	}
	else if (ret_status == NCL_OKINPUT)
	{
		ncl_add_token(cmdbuf, str, comma);
		if (str[0] == '\0') ret_status = NCL_DONE;
	}

	return (ret_status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_str1(cmdbuf, prompt)
**       Prompt the user to enter a text string and add to command buffer.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_str1(cmdbuf, prompt)
	NCL_cmdbuf *cmdbuf;
	int prompt;

{
	char str[80];
	int status;
	int numint;
	UD_STRREC strrec;

	uu_denter(UU_MTRC,(us,"ncl_add_str(cmdbuf=%x, prompt=%d)",
		cmdbuf, prompt));

      str[0] = '\0';
      strrec.instring = str;

      ud_ldas(UD_DASSTRINGDEF, UA_NCL, prompt, &strrec, 80,
            &numint, UD_NODEFAULT);

      if ((numint <= 0) || (strrec.termcon == DE_DONE)) 
           status = NCL_DONE;
      else if (str[0] == '\0') 
           status = NCL_NOINPUT;
      else 
        {
           status = NCL_OKINPUT;
		     ncl_add_token(cmdbuf, str, NCL_comma);
        }

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_label(defmode, cmdbuf, prompt, selmask)
**			Limit DAS to pick the specified entities (SELMASK), prompt
**			the user to enter a label field.
**    PARAMETERS   
**       INPUT  : 
**				defmode					UD_DASPCKLOC or UD_DASSTRING
**          cmdbuf					command buffer to place label
**				prompt					prompt message number
**				selmask					DAS  pick select mask
**       OUTPUT :  
**          cmdbuf					label is appended to current line
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_label(defmode, cmdbuf, prompt, selmask)
	int defmode;
	NCL_cmdbuf *cmdbuf;
	int prompt;
	int selmask[];

	{
	char str[256];
	int inpmode,i;
	int ret_status;
	int numint;

	uu_denter(UU_MTRC,(us,"ncl_add_label(defmode=%d, cmdbug=%x, prompt=%d, selmask=%x)",
		defmode, cmdbuf, prompt, selmask));

/*
.....For some reason at times there is junk in str, make sure it is
.....no longer there.  JLS 2/11/99
*/
	for(i=0;i<256;i++) str[i]='\0';
	inpmode = defmode;
	do
	{
		if (inpmode == UD_DASPCKLOC || inpmode == UD_DASSTRING)
		{
			ret_status = ncl_get_dlabel(defmode, str, prompt, selmask);
/*
.....If ret_status is not okay, don't add token JLS 2/24/99
*/
			if (ret_status == NCL_OKINPUT)
				ncl_add_token(cmdbuf, str, NCL_comma);
		}
		else if (inpmode == UD_DASSELECT)
		{
			ud_lgeo(UU_TRUE, selmask);
/*
............Currently limit DASSELECT to return no more that 100 labels...
*/
			ret_status = ud_ldas(UD_DASSELECT, UA_NCL, prompt, UU_NULL, NCL_MAXPICK,
								&numint, UD_NODEFAULT);

			if (ret_status == UD_DASALTACTION)
			{
				inpmode = UD_DASSTRING;
				ret_status = NCL_ALTACTION;
			}
			else if (numint > 0)
			{
				ret_status = ncl_get_select_labels(cmdbuf,numint);
			}
			else
				ret_status = NCL_NOINPUT;
		}
	}
	while ((ret_status == NCL_ALTACTION) || (ret_status == NCL_NOINPUT)); 

	uu_dexit;
	return(ret_status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_label1(cmdbuf, prompt, selmask)
**			Limit DAS to pick the specified entities (SELMASK), prompt
**			the user to enter a label field.
**    PARAMETERS   
**       INPUT  : 
**          cmdbuf					command buffer to place label
**				prompt					prompt message number
**				selmask					DAS  pick select mask
**       OUTPUT :  
**          cmdbuf					label is appended to current line
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_DONE iff user hit "Done" 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_label1(cmdbuf, prompt, selmask)
	NCL_cmdbuf *cmdbuf;
	int prompt;
	int selmask[];

	{
	UM_PLOCREC pick;
	int ret_status;
	int numint;

	uu_denter(UU_MTRC,(us,"ncl_add_label1(cmdbug=%x, prompt=%d, selmask=%x)",
		cmdbuf, prompt, selmask));

	ud_lgeo(UU_TRUE, selmask);
	ret_status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, prompt, &pick, 1,
								&numint, 1); 
			if (numint > 0)
			{
			   ncl_add_token(cmdbuf, pick.ploc.label, NCL_comma);
				ret_status = NCL_OKINPUT;
			}
			else
				ret_status = NCL_DONE;

	while (ret_status == NCL_ALTACTION); 
	ud_lgeo(UU_FALSE, selmask);
	return(ret_status);
}


/*********************************************************************
**    E_FUNCTION     : int ncl_add_label_rel(defmode, cmdbuf, prompt,
**															selmask, reltyp)
**			Limit DAS to pick the specified entities (SELMASK), prompt
**			the user to enter a label field.  returns the relation type
**			of the entity picked.
**    PARAMETERS   
**       INPUT  : 
**				defmode					UD_DASPCKLOC or UD_DASSTRING
**          cmdbuf					command buffer to place label
**				prompt					prompt message number
**				selmask					DAS  pick select mask
**       OUTPUT :  
**          cmdbuf					label is appended to current line
**				reltyp					relation type of entity picked
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_label_rel(defmode, cmdbuf, prompt, selmask, reltyp)
	int defmode;
	NCL_cmdbuf *cmdbuf;
	int prompt;
	int selmask[];
	int *reltyp;

	{
	UM_PLOCREC pick;
	int inpmode;
	int ret_status;
	int numint;
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"ncl_add_label_rel(defmode=%d, cmdbug=%x, prompt=%d, selmask=%x)", defmode, cmdbuf, prompt, selmask));

	inpmode = defmode;
	do
		{
		if (inpmode == UD_DASPCKLOC)
			{
			ud_lgeo(UU_TRUE, selmask);
			ret_status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, prompt, &pick, 1,
								&numint, 1);
			if (ret_status == UD_DASALTACTION)
				{
/*
.....
..... We do not need it any more. Paul 08/06/92
.....
*/
/****			inpmode = UD_DASSTRING;			****/
/****			ret_status = NCL_ALTACTION;  	****/
				}
			else if (numint > 0)
				{
                ncl_add_token(cmdbuf, pick.ploc.label, NCL_comma);

/*				get key of picked entity then find out its relation type */
/*
.....
..... The "if" statement added to handle the "nested point". Paul. 
..... 06/15/93
.....
*/
                if(pick.pent.key[0] != 0 || pick.pent.key[1] != 0)
                {
				   key = um_get_pickkey(&pick.pent, 1);
				   um_retrieve_data_relnum(key, reltyp);
                }
                else
                   *reltyp = 1;

				ret_status = NCL_OKINPUT;
				}
			else
				ret_status = NCL_NOINPUT;
			}
		if (inpmode == UD_DASSTRING)
			{
			ret_status = ncl_add_str(cmdbuf, prompt, NCL_comma);
			if (ret_status == NCL_ALTACTION)
				inpmode = UD_DASPCKLOC;
			}
		}
	while (ret_status == NCL_ALTACTION); 

	uu_dexit;
	return(ret_status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_label_rel1(cmdbuf, prompt,
**															selmask, reltyp)
**			Limit DAS to pick the specified entities (SELMASK), prompt
**			the user to enter a label field.  returns the relation type
**			of the entity picked.
**    PARAMETERS   
**       INPUT  : 
**          cmdbuf					command buffer to place label
**				prompt					prompt message number
**				selmask					DAS  pick select mask
**       OUTPUT :  
**          cmdbuf					label is appended to current line
**				reltyp					relation type of entity picked
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_DONE iff user hit "Done"
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_label_rel1(cmdbuf, prompt, selmask, reltyp)
	NCL_cmdbuf *cmdbuf;
	int prompt;
	int selmask[];
	int *reltyp;

	{
	UM_PLOCREC pick;
	int ret_status;
	int numint;
	UU_KEY_ID key;

	uu_denter(UU_MTRC,(us,"ncl_add_label_rel1(cmdbug=%x, prompt=%d, selmask=%x)",  cmdbuf, prompt, selmask));

	ud_lgeo(UU_TRUE, selmask);
	ret_status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, prompt, &pick, 1,
								&numint, 1);
	if (numint > 0)
	{
      ncl_add_token(cmdbuf, pick.ploc.label, NCL_comma);

/*				get key of picked entity then find out its relation type */
/*
.....
..... The "if" statement added to handle the "nested point". Paul. 
..... 06/15/93
.....
*/
      if(pick.pent.key[0] != 0 || pick.pent.key[1] != 0)
      {
		   key = um_get_pickkey(&pick.pent, 1);
		   um_retrieve_data_relnum(key, reltyp);
      }
      else
         *reltyp = 1;

		ret_status = NCL_OKINPUT;
	}
	else
		ret_status = NCL_DONE;

	while (ret_status == NCL_ALTACTION); 

	uu_dexit;
	return(ret_status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_name(cmdbuf, prompt)
**			Prompt the user to enter a name.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_name(cmdbuf, prompt)
	NCL_cmdbuf *cmdbuf;
	int prompt;

	{
	char str[256];
	int ret_status;
	int numint;
	UM_PLOCREC pick;
	int inpmode;

	uu_denter(UU_MTRC,(us,"ncl_add_name(cmdbuf=%x, prompt=%d)",
		cmdbuf, prompt));

	/*RAH: why was this here? caused GROSS problems with copy in CADD! */
	/* ncl_where = UU_TRUE; */
	ret_status = ncl_add_str(cmdbuf, prompt, NCL_nocomma);
	/* ncl_where = UU_FALSE; */
	/* Added for toggle to pick capability when asked for data by text. Kathy */
	if (ret_status == NCL_ALTACTION)
		{
		inpmode = UD_DASPCKLOC;
		ud_lgeo(UU_TRUE, UD_ncl_geometry);
		ret_status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, prompt, &pick, 1,
						&numint, 1);
		if (numint >0)
			{
			ncl_picktostr(&pick, str);
			ncl_add_token(cmdbuf, str, NCL_nocomma);
			ret_status = NCL_OKINPUT;
			}
		else
			ret_status = NCL_NOINPUT;
		}
	if (ret_status == NCL_OKINPUT)
		{
		ncl_add_token(cmdbuf, "=", NCL_nocomma);
		}

	uu_dexit;
	return(ret_status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_coord(cmdbuf, prompt, dim)
**			Prompt the user to enter a coordinate.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_coord(cmdbuf, prompt, dim)
	NCL_cmdbuf *cmdbuf;
	int prompt;
	int dim;

	{
/******	UM_coord cc; ******/
    UD_NDCLOCREC cc;
	char str[256];
	int ret_status;
	int inpmode;
	int numint;

	uu_denter(UU_MTRC,(us,"ncl_add_coord(cmdbuf=%x, prompt=%d)",
		cmdbuf, prompt));

	inpmode = UD_DASCART;
	do
		{
		if (inpmode == UD_DASCART)
			{
   /*      UD_locint = UD_LOCATOR; */
			ret_status = ud_ldas(UD_DASCART, UA_NCL, prompt, &cc, 1,
						&numint, UD_NODEFAULT);
			if (ret_status == UD_DASALTACTION)
				{
				inpmode = UD_DASSTRING;
				ret_status = NCL_ALTACTION;
				}
			else if (numint > 0)
				{
                  if (strcmp(cc.label,"      ") == 0)
				  {
					ncl_cctostr(dim, cc.cord, str);
					ncl_add_token(cmdbuf, str, NCL_comma);
					ret_status = NCL_OKINPUT;
				  }
                  else
				  {
						ncl_add_token(cmdbuf, cc.label, NCL_comma);
						ret_status = NCL_OKINPUT;
				  }
				}
			else
				ret_status = NCL_NOINPUT;
			}
		if (inpmode == UD_DASSTRING)
			{
			ret_status = ncl_add_str(cmdbuf, prompt, NCL_comma);
			/* Added for toggle to pick capability when asked for data by text. Kathy */
			if (ret_status == NCL_ALTACTION)
				inpmode = UD_DASCART;
			}
		}
	while (ret_status == NCL_ALTACTION); 

	uu_dexit;
	return(ret_status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_vector(cmdbuf, prompt)
**       Prompt the user to enter a vector.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_vector(cmdbuf, prompt)
	NCL_cmdbuf *cmdbuf;
	int prompt;

	{
	char str[256];
	UM_vector vc;
	int ret_status;
	int inpmode;
	int numint;

	uu_denter(UU_MTRC,(us,"ncl_add_vector(cmdbuf=%x, prompt=%d)",
		cmdbuf, prompt));

	inpmode = UD_DASVEC;
	do
		{
		if (inpmode == UD_DASVEC)
			{
			ret_status = ud_ldas(UD_DASVEC, UA_NCL, prompt, vc, 1,
						&numint, UD_NODEFAULT);
			if (ret_status == UD_DASALTACTION)
				{
				inpmode = UD_DASSTRING;
				ret_status = NCL_ALTACTION;
				}
			else if (numint > 0)
				{
				ncl_vctostr(vc, str);
				ncl_add_token(cmdbuf, str, NCL_comma);
				ret_status = NCL_OKINPUT;
				}
			else
				ret_status = NCL_NOINPUT;
			}
		if (inpmode == UD_DASSTRING)
			{
			ret_status = ncl_add_str(cmdbuf, prompt, NCL_comma);
			if (ret_status == NCL_ALTACTION)
				inpmode = UD_DASCART;
			}
		}
	while (ret_status == NCL_ALTACTION); 

	uu_dexit;
	return(ret_status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_vector1(cmdbuf, prompt)
**       Prompt the user to enter a vector.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_vector1(cmdbuf, prompt)
	NCL_cmdbuf *cmdbuf;
	int prompt;

	{
	char str[256];
	char *uu_uprompt0(), msg[100];
	UM_vector vc;
	int ret_status;
	int inpmode;
	UD_DASTAT dastat; 
	UD_DASTAT ud_vec2();                /* semantic interpreter */
	UD_DEVENT event;                    /* event buffer */

	uu_denter(UU_MTRC,(us,"ncl_add_vector1(cmdbuf=%x, prompt=%d)",
		cmdbuf, prompt));

	ud_lgeo(UU_FALSE, UD_ncl_vepv);
	inpmode = UD_DASVEC;
   strcpy(msg, uu_uprompt0(UA_NCL, prompt));
   ud_rpwrcom(msg);

	do
		{
		if (inpmode == UD_DASVEC)
			{
         ud_gevt(&event, UD_vecint, msg, 1, UD_vecdev, 
                                              UD_vecech, NULL);
         dastat = ud_vec2(&event, vc);

			if (dastat == UD_DASALTACTION)
				{
				inpmode = UD_DASSTRING;
				ret_status = NCL_ALTACTION;
				}
			else if (dastat == DE_TRUE)
				{
				ncl_vctostr(vc, str);
				ncl_add_token(cmdbuf, str, NCL_comma);
				ret_status = NCL_OKINPUT;
				}
			else if (dastat == DE_AGAIN)
            ret_status = NCL_NOINPUT;
			else
				ret_status = NCL_DONE;
			}
		if (inpmode == UD_DASSTRING)
			{
			ret_status = ncl_add_str1(cmdbuf, prompt);
			if (ret_status == NCL_ALTACTION)
				inpmode = UD_DASCART;
			}
		}
	while (ret_status == NCL_ALTACTION); 

	uu_dexit;
	return(ret_status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_length(cmdbuf, prompt)
**       Prompt the user to enter a length.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_length(cmdbuf, prompt)
	NCL_cmdbuf *cmdbuf;
	int prompt;

	{
	int ret_status;

	uu_denter(UU_MTRC,(us,"ncl_add_label(cmdbuf=%x, prompt=%d)",
		cmdbuf, prompt));

	ret_status = ncl_add_str(cmdbuf, prompt, NCL_comma);

	uu_dexit;
	return(ret_status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_angle(cmdbuf, prompt)
**       Prompt the user to enter an angle.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_angle(cmdbuf, prompt)
	NCL_cmdbuf *cmdbuf;
	int prompt;

	{
	int ret_status;

	ret_status = ncl_add_str(cmdbuf, prompt, NCL_comma);

	return(ret_status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_reserv(defmode, cmdbuf, prompt, selmask)
**			Added the RESERV VARIABLE of a subscripted variable to cmdbuf.
**			Limit DAS to pick the specified entities (SELMASK), prompt
**			the user to enter a label field.
**    PARAMETERS   
**       INPUT  : 
**				defmode					UD_DASPCKLOC or UD_DASSTRING
**          cmdbuf					command buffer to place label
**				prompt					prompt message number
**				selmask					DAS  pick select mask
**       OUTPUT :  
**          cmdbuf					label is appended to current line
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_reserv(defmode, cmdbuf, prompt, selmask)
int defmode;
NCL_cmdbuf *cmdbuf;
int prompt;
int selmask[];
	{
	char str[256];
	UM_PLOCREC pick;
	int inpmode;
	int ret_status, i;
	int numint;
	struct UC_entitydatabag e;

	uu_denter(UU_MTRC,(us,"ncl_add_reserv(defmode=%d, cmdbug=%x, prompt=%d, selmask=%x)",
		defmode, cmdbuf, prompt, selmask));

	inpmode = defmode;
	do
		{
		if (inpmode == UD_DASPCKLOC)
			{
			ud_lgeo(UU_TRUE, selmask);
			ret_status = ua_dl_pldas(UD_DASPCKLOC, UA_NCL, prompt, &pick, 1,
								&numint, 1);
			if (ret_status == UD_DASALTACTION)
				{
				inpmode = UD_DASSTRING;
				ret_status = NCL_ALTACTION;
				}
			else if (numint > 0)
				{
				/* ckeck if reserve-var - report error */
				e.key = um_get_pickkey(&(pick.pent), 1);
				ur_retrieve_data_fixed(&e);
				ncl_get_label_and_subscr(&e, str, &i);

				if (i == 0)
					{
					/* report error */
					uu_uerror0(UA_NCL, 6);
					ret_status = NCL_NOINPUT;
					}

				else
					{
					ncl_add_token(cmdbuf, str, NCL_comma);
					ret_status = NCL_OKINPUT;
					}
				}
			else
				ret_status = NCL_NOINPUT;
			}
		if (inpmode == UD_DASSTRING)
			{
			ret_status = ncl_add_str(cmdbuf, prompt, NCL_comma);
			if (ret_status == NCL_ALTACTION)
				inpmode = UD_DASPCKLOC;
			}
		}
	while (ret_status == NCL_ALTACTION); 

	uu_dexit;
	return(ret_status);
	}


/*********************************************************************
**    E_FUNCTION     : int ncl_add_modifier(cmdbuf, menu)
**       Prompt the user to pick a modifier.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			NCL_OKINPUT iff user entered response
**			NCL_NOINPUT iff user did not enter response
**			NCL_DONE iff user hit "done" key while in text input
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_add_modifier(cmdbuf, menu)
	NCL_cmdbuf *cmdbuf;
	int menu;

	{
	char str[256];
	int status;
	int ret_status;
	int choice;
	int outchoice;

	uu_denter(UU_MTRC,(us,"ncl_add_modifier(cmdbuf=%x, menu=%d)",
		cmdbuf, menu));

	do
		{
		status = ud_ddas(UD_POPUP, &nmodfy[menu], &choice,
					1, &outchoice, UD_NODEFAULT);
		}
	while  (status != 1);

	ret_status = NCL_OKINPUT;
	switch (menu)
		{
		case NCL_XY_MODIFIER:
		case NCL_REDEF_KEEP:
		case NCL_REDEF_INTERSECTION:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_xlarge);
					break;
				case 2:
					strcpy(str, NCL_xsmall);
					break;
				case 3:
					strcpy(str, NCL_ylarge);
					break;
				case 4:
					strcpy(str, NCL_ysmall);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
		case NCL_XYZ_MODIFIER:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_xlarge);
					break;
				case 2:
					strcpy(str, NCL_xsmall);
					break;
				case 3:
					strcpy(str, NCL_ylarge);
					break;
				case 4:
					strcpy(str, NCL_ysmall);
					break;
				case 5:
					strcpy(str, NCL_zlarge);
					break;
				case 6:
					strcpy(str, NCL_zsmall);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
		case NCL_XYZ_DIRECTION:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_posx);
					break;
				case 2:
					strcpy(str, NCL_negx);
					break;
				case 3:
					strcpy(str, NCL_posy);
					break;
				case 4:
					strcpy(str, NCL_negy);
					break;
				case 5:
					strcpy(str, NCL_posz);
					break;
				case 6:
					strcpy(str, NCL_negz);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					/* added exit for default. kathy */
					uu_dexit;
					return (ret_status);
					break;
				}
			break;
		case NCL_LINE_RELATION:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_right);
					break;
				case 2:
					strcpy(str, NCL_left);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
		case NCL_WHICH_AXIS:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_xaxis);
					break;
				case 2:
					strcpy(str, NCL_yaxis);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
		case NCL_WHICH_LINE:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_xaxis);
					break;
				case 2:
					strcpy(str, NCL_yaxis);
					break;
				case 3:         /* vp3.3.93 pv added alternative to line  */
					ncl_add_label(UD_DASPCKLOC, cmdbuf, 62, UD_ncl_pvln);
					uu_dexit;
					return(ret_status);
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
		case NCL_CIRCLE_SIZE:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_large);
					break;
				case 2:
					strcpy(str, NCL_small);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
		case NCL_CIRCLE_RELATION:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_in);
					break;
				case 2:
					strcpy(str, NCL_out);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
		case NCL_VECTOR_OP:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_plus);
					break;
				case 2:
					strcpy(str, NCL_minus);
					break;
				default:
					break;
				}
			break;
		case NCL_ROTATION_AXIS:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_xyrot);
					break;
				case 2:
					strcpy(str, NCL_yzrot);
					break;
				case 3:
					strcpy(str, NCL_zxrot);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
		case NCL_CS_CONDITION:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_to);
					break;
				case 2:
					strcpy(str, NCL_on);
					break;
				case 3:
					strcpy(str, NCL_past);
					break;
				case 4:
					strcpy(str, NCL_tanto);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
		case NCL_TOOL_RELATION:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_tllft);
					break;
				case 2:
					strcpy(str, NCL_tlon);
					break;
				case 3:
					strcpy(str, NCL_tlrgt);
					break;
				case 4:
					strcpy(str, NCL_tlofps);
					break;
				case 5:
					strcpy(str, NCL_tlonps);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
		case NCL_DS_CONDITION:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_to);
					break;
				case 2:
					strcpy(str, NCL_on);
					break;
				case 3:
					strcpy(str, NCL_past);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					uu_dexit;
					return (ret_status);
					break;
				}
			break;
		case NCL_GOTO_PATERN:
			{
			static int invers = 0;

			switch (choice)
				{
				case 0:
					invers = 0;
					status = NCL_DONE;
					break;
				case 1:
					invers = 1;
					status = ncl_add_token(cmdbuf, NCL_invers, NCL_comma);
					break;
				case 2:
					if (invers)
						status = ncl_add_token(cmdbuf, NCL_const, NCL_comma);
					else
						{
						strcpy(str,"");
						uu_uerror0(UA_NCL, 4);
						}
					break;
				case 3:
					status = ncl_add_token(cmdbuf, NCL_avoid, NCL_comma);

					do
						{
						status = ncl_add_length(cmdbuf, 454); 
						}
					while (status != NCL_OKINPUT);

					do
						{
						status = ncl_add_str(cmdbuf, 455, NCL_comma); 
						}
					while (status != NCL_OKINPUT);

					break;
				case 4:
					status = ncl_add_token(cmdbuf, NCL_retain, NCL_comma);

					do 
						{
						status = ncl_add_str(cmdbuf, 452, NCL_comma); 
						}
					while (status != NCL_OKINPUT);

					break;
				case 5:
					status = ncl_add_token(cmdbuf, NCL_omit, NCL_comma);

					do 
						{
						status = ncl_add_str(cmdbuf, 453, NCL_comma); 
						}
					while (status != NCL_OKINPUT);

					break;
				default:
					break;
				}
			uu_dexit;
			return (status);
			}

			break;
		case NCL_PATERN_INCR:
			switch (choice)
				{
				case 0:
					ret_status = NCL_DONE;
					break;
				case 1:
					ncl_add_token(cmdbuf, NCL_incr, NCL_comma);

					if (ret_status == NCL_OKINPUT)
						ret_status = ncl_add_str(cmdbuf, 461, NCL_comma);

					break;
				case 2:
					ncl_add_token(cmdbuf, NCL_incr, NCL_comma);

					if (ret_status == NCL_OKINPUT)
						ret_status = ncl_add_length(cmdbuf, 466);

					ncl_add_token(cmdbuf, NCL_at, NCL_comma);

					if (ret_status == NCL_OKINPUT)
						ret_status = ncl_add_length(cmdbuf, 463);

					break;
				default:
					break;
				}
			uu_dexit;
			return (ret_status);

			break;

		case NCL_FEDRATE_AT:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_once);
					break;
				case 2:
					/* continuous is not a valid key word. kathy */
					/* strcpy(str, NCL_continuous); */
					strcpy(str, "");
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;

		case NCL_CLWCCW_OP:
		case NCL_REDEF_DIRECTION:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_clw);
					break;
				case 2:
					strcpy(str, NCL_ccw);
					break;
				default:
/*
.....If done was selected, ret_status should be set to NCL_NOINPUT
.....JLS 2/23/99
*/
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					break;
				}
			break;
			break;
		default:
			ret_status = NCL_NOINPUT;
			strcpy(str,"");
			break;
 
                /* added for shape. by kathy */

		case NCL_SHAPE_MODIFIER:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_clw);
					break;
				case 2:
					strcpy(str, NCL_ccw);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					uu_dexit;
					return (ret_status);
					break;
				}
			break;

			/* added for rmill. kathy */
		case NCL_CS_MODIFIER:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_to);
					break;
				case 2:
					strcpy(str, NCL_on);
					break;
				case 3:
					strcpy(str, NCL_past);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					uu_dexit;
					return (ret_status);
					break;
				}
			break;
		/* added for pocket. kathy */
		case NCL_PERIMETER_POS:
			switch (choice)
				{
				case 1:
					strcpy(str, NCL_in);
					break;
				case 2:
					strcpy(str, NCL_out);
					break;
				case 3:
					strcpy(str, NCL_on);
					break;
				default:
					ret_status = NCL_NOINPUT;
					strcpy(str,"");
					/* Do not put comma if nothing is picked. */
					uu_dexit;
					return (ret_status);
					break;
				}
			break;
		}

	ncl_add_token(cmdbuf, str, NCL_comma);
	uu_dexit;
	return (ret_status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_add_label_nrpt(prompt, selmask, elabel,
**                                            nlabel, relnum)
**       Prompt user to pick an entity and return entity label, a
**       point on entity near the pick location and the relation 
**       number of the picked entity.
**    PARAMETERS   
**       INPUT  : 
**          prompt     - Prompt message number.
**          selmask    - DAS pick select mask.
**       OUTPUT :  
**          elabel     - Label of picked entity.
**          nlabel     - Nested near point.
**          relnum     - Relation number of picked entity.
**    RETURNS      : 
**       NCL_OKINPUT iff user entered response
**       NCL_DONE iff user hit "Done" 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_add_label_nrpt(prompt, selmask, elabel, nlabel, relnum)
int prompt;
int selmask[];
char *elabel, *nlabel;
int *relnum;
{
	UU_KEY_ID cvkey;
	return (ncl_add_label_nrpt1(UA_NCL,prompt,selmask,elabel,nlabel,relnum,&cvkey));
}

/*********************************************************************
*********************************************************************/
int
ncl_add_label_nrpt1(subnum, prompt, selmask, elabel, nlabel, relnum,cvkey)
int prompt,subnum;
int selmask[];
char *elabel, *nlabel;
int *relnum;
UU_KEY_ID *cvkey;
{
	UD_PLOCREC ploc;
	UM_PICKENT pent;
	int status, numint, segno;
	UU_KEY_ID key;
	UU_LIST ptlst;
	UM_coord nearpt;
	UU_REAL svdis;

	ud_lgeo(UU_TRUE, selmask);
/*
.....Prompt user to pick entity
*/
	status = ud_ldas(UD_DASPCKLOC, subnum, prompt, &ploc, 1,
	            &numint, 1, UD_NODEFAULT); 
	if (numint > 0)
	{
		strcpy(elabel,ploc.pndc.label);
		segno = ploc.ppath.pickpath[0];
		svdis = 1.0e9;
		ncl_get_nearpt_dis((UD_NDCLOCREC *)&(ploc.pndc), segno, nearpt, &svdis);
		if (1.0e9+1>svdis>1.0e9-1)
		{
/*
......there is no nearest point
*/
			nlabel[0] = '\0';
		}
		else
		{
			strcpy(nlabel,"(PT/");
			ncl_cctostr (3, nearpt, &nlabel[4]);
			strcat(nlabel,")");
		}
/*
.....Get relation number of picked entity.
*/
		um_d_pickresolve(&(ploc.ppath), 1, &pent);
		key = um_get_pickkey(&pent, 1);
		*cvkey = key;
		*relnum = 0;
		if (key > 0) um_retrieve_data_relnum(key, relnum);
		status = NCL_OKINPUT;
	}
	else
		status = NCL_DONE;

	ud_lgeo(UU_FALSE, selmask);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_neardis(pickpt,pp1,pp2,p0,p1,p2,nearpt,svdis)
**       Find a point on a segment nearest to the pick point.
**    PARAMETERS
**       INPUT  :
**          pickpt   - pick point
**          pp1      - first point of segment
**          pp2      - second point of segment
**          p0       - pick point projected to view plane
**          p1       - first point projected to view plane
**          p2       - second point projected to view plane
**          svdis    - current minimum distance.
**       OUTPUT :
**          nearpt   - near point
**          svdis    - updated minimum distance.
**    RETURNS      :
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_neardis(pickpt,pp1,pp2,p0,p1,p2,nearpt,svdis)
UM_coord pickpt, pp1, pp2, p0, p1, p2, nearpt;
UU_REAL *svdis;
{
	UM_coord p3;
	UM_vector v1, uv1, v2;
	UU_REAL dp, d1, d2, d3, u;

	um_vcmnvc(p2,p1,v1);
	d3 = 0.0;
	if (um_mag(v1) > 1.e-6)
	{
		um_unitvc(v1,uv1);
		dp = um_dot(pickpt,uv1);
		d1 = um_dot(p1,uv1);
		d2 = um_dot(p2,uv1);
		d3 = d2 - d1;
	}
/*
.....If near point is behind first point or segment is zero length,
.....use distance to first point.
*/
	if (dp <= d1 || d3 < 1.e-6)
	{
		d1 = um_dcccc(p0,p1);
		if (*svdis > d1)
		{
			*svdis = d1;
			um_vctovc(pp1,nearpt);
		}
	}
/*
.....If near point is past second point, use distance to second point.
*/
	else if (dp >= d2)
	{
		d1 = um_dcccc(p0,p2);
		if (*svdis > d1)
		{
			*svdis = d1;
			um_vctovc(pp2,nearpt);
		}
	}
/*
.....If near point is between first and second points, calculate point
.....a percentage distance along the segment.
*/
	else
	{
		u = (dp-d1)/d3;
		um_vctmsc(v1,u,v2);
		um_vcplvc(p1,v2,p3);
		d1 = um_dcccc(p0,p3);
		if (*svdis > d1)
		{
			*svdis = d1;
			um_vcmnvc(pp2,pp1,v1);
			um_vctmsc(v1,u,v2);
			um_vcplvc(pp1,v2,nearpt);
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_ploc_to_coord(picker, ploc, nearpt)
**       Find a point on an entitiy nearest to the pick point.
**    PARAMETERS
**       INPUT  :
**          picker   - pick record.
**          ploc     - NDC pick record.
**       OUTPUT :
**          nearpt   - near point
**    RETURNS      :
**       UU_SUCCESS if no error, otherwise UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_ploc_to_coord(picker, ploc, nearpt)
UD_PPICKREC *picker;
UD_NDCLOCREC *ploc;
UM_coord nearpt;
{
	int status = UU_FAILURE, segno;
	UU_LIST ptlst;
	UU_REAL svdis;

	segno = picker->pickpath[0];
	svdis = 1.0e9;
/*
.....Get display points into list and calculate near point.
*/
	if (segno)
	{
		ncl_get_nearpt_dis((UD_NDCLOCREC *)&(ploc->cord), segno, nearpt, &svdis);
		status = UU_SUCCESS;
	}

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_nearpt_dis(pndc, segno, nearpt, dis)
**       Get the point in a display list nearest to the pick point.
**    PARAMETERS   
**       INPUT  : 
**          pndc     - NDC pick location.
**          segno   - picking segment no
**			dis: closest distant
**       OUTPUT :  
**          nearpt   - Near point
**			dis: closest distant
**    RETURNS      : 
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_nearpt_dis(pndc,segno,nearpt, dis)
UD_NDCLOCREC *pndc;
int segno;
UM_coord nearpt;
UU_REAL *dis;
{
	UG_plylna3op *cmd;         /* pointer to each cmd */
	int ncmd;                  /* number of commands in a segment */
	int i,m,len,opcode;
	char *ug_lsielt();         /* function to return list element */
	UG_segstli *segptr;        /* pointer to segment n's header */
	Gos systate;               /* save GKS state here */
	int oldviwseg;             /* save ug_viwseg here */
	UG_LSI *listp;             /* graphics command list for this seg */
	UM_coord hdr;
	UU_REAL svdis;
	int status = UU_FAILURE;

	svdis = *dis;
	hdr[0] = hdr[1] = hdr[2] = 0.0;
	segptr=ug_segac(segno);
	if (segptr == 0) return(UU_FAILURE);
	systate=ug_gksos.sysstate;      /* save GKS system state */

	if (systate==UG_SGOP) ug_gksos.sysstate=UG_WSAC;
	oldviwseg=ug_viwseg;            /* save ug_viwseg */
	ug_viwseg=segno;                /* for clip/exp routines */

	listp=(UG_LSI *)segptr->seglist;
	ncmd = ug_lsinelt((*listp));

	for( i=0; i < ncmd; i++ )
	{
		cmd = (UG_plylna3op *)ug_lsielt(listp,i);
		opcode=(*cmd).elttype;

		if (opcode<=UG_OPPTR) opcode=opcode&(UG_OPPTR-1);
/*
... Switch on segment element type
*/
		switch (opcode) 
		{
		case UG_PLYLNA3OP: 
			len=(*(UG_plylna3op *)cmd).len;
			hdr[0] = len;
			status = ncl_nearpt2(pndc, len, (*(UG_plylna3op *)cmd).pts, nearpt, &svdis);
			break;
		case UG_PLYMKA3OP: 
			len=(*(UG_plymka3op *)cmd).len;
			hdr[0] = len;
			status = ncl_nearpt2(pndc, len, (*(UG_plymka3op *)cmd).pts, nearpt, &svdis);
			break;
		case UG_CALLOP: 
			m=(*(UG_callop *)cmd).segno;  /* call seg m */
			status = ncl_get_nearpt_dis(pndc, m, nearpt, &svdis);
			break;
		case UG_SHADEAREAOP: 
			len=(*(UG_shadearea *)cmd).len;
			hdr[0] = -len;
			status = ncl_nearpt2(pndc,len, (*(UG_shadearea *)cmd).pts, nearpt, &svdis);
			break;
		case UG_SNTRANOP: 
			ug_sntran((*(UG_sntranop *)cmd).xform); 
			break;
		case UG_MTRANOP:
			ug_smodxf((*(UG_mtranop *)cmd).xf,(*(UG_mtranop *)cmd).type); 
			break;
		case UG_LMTRANOP:
			ug_slmodxf((*(UG_lmtranop *)cmd).xf,(*(UG_lmtranop *)cmd).type); 
			break;
		default:
			break;
		}             /* case */
	}                /* loop */
	ug_gksos.sysstate=systate;    /* restore GKS state */
	ug_viwseg=oldviwseg;          /* restore viwseg */
	*dis = svdis;
	return status;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_nearpt2(pndc, pts, len, nearpt, dis)
**       Get the nearest point in a display list nearest to the pick point.
**    PARAMETERS   
**       INPUT  : 
**          pndc     - NDC pick location.
**          pts   - display points
**			len:  - number of display points
**			dis: closest distant
**       OUTPUT :  
**          nearpt   - Near point
**			dis: closest distant
**    RETURNS      : 
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_nearpt2(pndc,len, pts, nearpt, dis)
UD_NDCLOCREC *pndc;
int len;
Gwpoint3 *pts;
UM_coord nearpt;
UU_REAL *dis;
{
	int i, j;
	UM_coord pickpt, vrefpt, p0, p1, p2, p3;
	UM_vector vpnorm;
	UU_REAL d1, svdis;
	UU_LOGICAL tess;
	UM_coord ptp, ptp1, ptp2, ptp3;
	int status = UU_FAILURE;

	svdis = *dis;
	um_ploctocc(pndc,pickpt);
/*
..... If there are no display points, return the pick point.
*/
	if (len == 0)
		um_vctovc(pickpt,nearpt);
	else
	{
		um_vpnorm(pndc->transform,vpnorm);
		um_vrefpt(pndc->transform,vrefpt);
		um_nptpln(pickpt,vrefpt,vpnorm,p0);
		tess = len < 0.0;
		len = fabs(len);
		j = 0;
		while (j<len)
		{
			ptp[0] = pts[j].x;
			ptp[1] = pts[j].y;
			ptp[2] = pts[j].z;
			um_nptpln(ptp,vrefpt,vpnorm,p1);
/*
..... If there is one display point, check it against the current min distance.
*/
			if (len == 1)
			{
				d1 = um_dcccc(p0,p1);
				if (svdis > d1)
				{
					svdis = d1;
					ptp[0] = pts[j].x;
					ptp[1] = pts[j].y;
					ptp[2] = pts[j].z;
					um_vctovc(ptp,nearpt);
				}
				j++;
			}
			else if (tess)
			{
/*
..... Find near point on tesselation triangles.
*/
				for (i=0;i<len;i+=3,j+=3)
				{
					ptp1[0] = pts[j].x;
					ptp1[1] = pts[j].y;
					ptp1[2] = pts[j].z;
					um_nptpln(ptp1,vrefpt,vpnorm,p1);
					ptp2[0] = pts[j+1].x;
					ptp2[1] = pts[j+1].y;
					ptp2[2] = pts[j+1].z;
					um_nptpln(ptp2,vrefpt,vpnorm,p2);
					ptp3[0] = pts[j+2].x;
					ptp3[1] = pts[j+2].y;
					ptp3[2] = pts[j+2].z;
					um_nptpln(ptp3,vrefpt,vpnorm,p3);
					ncl_get_neardis(pickpt,ptp1,ptp2,p0,p1,p2,nearpt,&svdis);
					ncl_get_neardis(pickpt,ptp2,ptp3,p0,p2,p3,nearpt,&svdis);
					ncl_get_neardis(pickpt,ptp3,ptp1,p0,p3,p1,nearpt,&svdis);
				}
			}
			else
			{
/*
..... Find near point on wireframe display points.
*/
				for (i=1;i<len;i++,j++)
				{
					ptp1[0] = pts[j].x;
					ptp1[1] = pts[j].y;
					ptp1[2] = pts[j].z;
					ptp2[0] = pts[j+1].x;
					ptp2[1] = pts[j+1].y;
					ptp2[2] = pts[j+1].z;
					um_nptpln(ptp2,vrefpt,vpnorm,p2);
					ncl_get_neardis(pickpt,ptp1,ptp2,p0,p1,p2,nearpt,&svdis);
					um_vctovc(p2,p1);
				}
				j++;
			}
			j++;
		}
	}
	*dis = svdis;
	return (UU_SUCCESS);
}
