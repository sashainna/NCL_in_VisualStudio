/*********************************************************************
**    NAME         :  nufilet.c
**       CONTAINS: User interface routines for CV fillet creation.
**
**       nclu_fill_cmd (e, nent, radius)
**       prefil (keys,radius,ierr)
**
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nufilet.c , 25.2
**     DATE AND TIME OF LAST  MODIFICATION
**       10/13/15 , 11:08:13
*********************************************************************/
#include "usysdef.h"
#include "mfort.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "modef.h"
#include "mdcoord.h"
#include "mdeval.h"
#include "mcrv.h"
#include "misect.h"
#include "mlab.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclfc.h"
#include "nclvx.h"

/**********************************************************************
**    E_FUNCTION     : nclu_fill_cmd(e, nent, radius)
**       generate fillet statements for pp file after it is processed
**    PARAMETERS   
**       INPUT  : 
**          e      - pointer to array of curve's ids (LN, CI)
**          nent   - number of entities in e list
**          radius - radius of fillet
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int nclu_fill_cmd (lab, sub, e, nent, radius)
struct NCL_id_rec *e;
int nent,sub;
UU_REAL radius;
char *lab;
{
	UM_f77_str fcmd;
	NCL_cmdbuf cmdbuf;
	struct NCL_id_rec *crv;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char str[80];
	int j,n;
	UM_int2 idx = 169;
	UU_REAL rnum;
	UM_real8 ver;
	UU_LOGICAL NCL_lv94;


	getsc(&idx, &ver);
	NCL_lv94 = (ver < 9.449);

	ncl_init_cmdbuf(&cmdbuf);
/*
.....Add Variable name to command buffer
.....Bobby  -  5/3/96
*/
  if (NCL_lv94)
  {
  ncl_add_token(&cmdbuf,lab,NCL_nocomma);
  if (sub != 0)
  {
     sprintf(str,"(%d)",sub);
     ncl_add_token(&cmdbuf,str,NCL_nocomma);
  }
  ncl_add_token(&cmdbuf,"=",NCL_nocomma);
  }
/*
.....Add FILLET keyword to command buffer
*/
   ncl_add_token(&cmdbuf, NCL_fillet, NCL_nocomma);
   ncl_add_token(&cmdbuf, "/", NCL_nocomma);
/*
.....vp 24-feb-97 convert radius according to units set in NCL
.....since radius is allways in inches when working with bare
.....unibase data (which is in inches dispite of NCL setup)
*/
	idx = 264;
	getifl (&idx,&idx);
	rnum = (idx == 1)? 25.4 * radius: radius;
	ncl_sprintf (str,&rnum,1);
	ncl_add_token(&cmdbuf, str, NCL_comma);
/*
...Loop thru all entities 
*/
	for (j = 0; j < nent; j++)
	{
		crv = (struct NCL_id_rec *) &e[j];
		strncpy(label,crv->label, NCL_MAX_LABEL); 
		if (crv->subscr > 0)
		{
			ncl_add_token(&cmdbuf, label, NCL_nocomma);
			sprintf(label,"(%d)", crv->subscr);
			ncl_add_token(&cmdbuf, label, NCL_comma);
		}
		else
			ncl_add_token(&cmdbuf, label, NCL_comma);
	}
/*
...Finish command line
*/
   if (NCL_lv94)
   {
  ncl_del_token(&cmdbuf,"", UU_TRUE);
  ncl_add_cmdbuf(&cmdbuf);
/*
...put in w2 (Fortran) array to output in pp file
*/
  for (j=0; j<cmdbuf.num_cmd; j++)
    {
     n = strlen (cmdbuf.cmd[j]);
     UM_init_f77_str(fcmd,cmdbuf.cmd[j],80);
     putinw (UM_addr_of_f77_str(fcmd),&n,&j);
    }
  putinf (&cmdbuf.num_cmd);
   }
   else
   {
	ncl_set_cmdmode(UU_TRUE);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
   }

	uu_dexit;
	return(UU_SUCCESS);
}

/**********************************************************************
**    E_FUNCTION     : prefil(keys, nent, radius, fillnm, subsc)
**       Creates fillets for the listed set of connected curves (LN, CI
**       curently supported).
**    PARAMETERS   
**       INPUT  : 
**          keys   - list of keys for specified curves.
**          nent   - number of curves in list.
**          radius - fillet radius 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int
prefil (keys, nent, radius, fillnm, subsc)
  int *keys, *nent;
  UU_REAL *radius;
  UM_f77_str_ptr fillnm;
  UM_int4 *subsc;
 {
  struct UM_crvdatabag e[2];
  int i, k, status, nclkey, nclrel, subs,nfil; 
  UU_KEY_ID first_key,delkey;
  UU_LOGICAL cont, last;
  UM_length rfil;
  UM_int2 cnon, ifnd, ier, idx;
  char *filid, label[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
  UM_int4 isub2;

	UM_int2 j2,ilab;
	char *lb0,buf[80];
	UM_f77_str_ptr str77;

/*
...Get first key and save to check with last
*/
  last  = UU_FALSE;
  cont  = UU_TRUE;
  e[0].key = first_key = keys[0];
  subs  = isub2 = *subsc;
	nfil = 0;
/*
...set label to use supplied name for fillets
*/
  if (*subsc > 0) 
    {
     filid = UM_cstr_of_f77_str(fillnm);
     strcpy (label,filid);
     for (i=strlen(label);i<NCL_MAX_LABEL-1;i++) label[i] = ' ';
     label[NCL_MAX_LABEL-1] = '\0';
    }

	UM_init_f77_str (str77, buf, 80);
	j2 = 296; getifl(&j2,&ilab);
	if (ilab > 0) lb0 = UM_cstr_of_f77_str(fillnm);

	idx = 41;
	getifl (&idx,&cnon);

  rfil  = *radius;
  k     = 1;
  while (cont)
    {
     cont = k < *nent;
     delkey = 0;
/*
...Get second key to make fillet, if it is the last, try 
...make fillet with the first
*/
     if (!cont)
        {
         last = UU_TRUE;
         e[1].key = e[0].key;
         e[0].key = first_key;
        }
     else
        e[1].key = keys[k++];
/*
...Check if label exists in unibase and canon is ON
*/
     if (isub2 > 0)
        {
/*         chklab (UM_addr_of_f77_str(label), &nclkey, &isub2, &ifnd, &cnon);*/
         chklab (fillnm, &nclkey, &isub2, &ifnd, &cnon);
         if (ifnd == 1)
            if (cnon == 0)
               {
                ier = 384;
                goto failed;
               }
            else if (cnon == 1)
              {
               ur_retrieve_data_relnum(nclkey,&nclrel);
               if (nclrel != UM_CIRCLE_REL)
                  {
                   ier  = 89;
                   goto failed;
                  }
/*              uc_delete (nclkey);
                ncl_randel(nclkey, nclrel);   */
               delkey = nclkey;
              }
         subs = isub2;
         isub2++;
        }
	else if (ilab > 0 && cnon == 0)
	{
		sprintf(buf,"%s%d",lb0,ilab);
		chklab (str77, &nclkey, &isub2, &ifnd, &cnon);
		if (ifnd == 1)
		{
			ier = 384;
			goto failed;
		}
	}
/*
...make fillet for this pair of curves, if failure
...replace first by second to continue with the next curve in list
*/
     status = um_make_fillets (e,rfil,last,delkey,subs,label,UU_FALSE);
		if (status != UU_SUCCESS)
			e[0].key = e[1].key;
		else
			nfil++;
		if (ilab > 0)
		{
			ilab++;
			sprintf(buf,"%s%d",lb0,ilab);
			stsavi(str77);
		}
	}
	if (nfil == 0)
	{
		ier = 474;
		goto failed;
	}
	status = 0;
   goto done;
/*
...Errors: geometry already defined
*/
failed:;
  status = ier;;

done:;
  uu_dexit;
  return (status);
 }
