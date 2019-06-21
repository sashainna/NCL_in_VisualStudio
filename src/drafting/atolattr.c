#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"

/*********************************************************************
**    NAME         :  atolattr.c
**       CONTAINS:
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       atolattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:40
*********************************************************************/
#define NO_DEFAULT 0
#define DEFAULT 1
/*********************************************************************
**    E_FUNCTION     : ua_set_tol_attr(formfile)
**       Set dimension attributes using forms package.
**    PARAMETERS   
**       INPUT  : 
**          formfile				form file name
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_tol_attr(formfile)
	char	*formfile;
	
	{
	int status;
	static int places, d_places, method, tol_site, zero_supp, d_zero_supp,
					round_off, d_round_off;
	static UU_REAL farr[8];
	static int *ans[] = {&places, &d_places, &method, &tol_site, (int *)&farr[0],
								(int *)&farr[1], (int *)&farr[2], (int *)&farr[3],
								(int *)&farr[4], (int *)&farr[5],
								&zero_supp, &d_zero_supp, &round_off, &d_round_off,
								(int *)&farr[6], (int *)&farr[7]};

	uu_denter(UU_MTRC,(us,"ua_set_tol_attr()"));


	places = UA_tol_dec_places;
	d_places = UA_d_tol_dec_plac;
	method = UA_tol_method; 
	tol_site = UA_tol_site; 
	farr[0] = UA_lin_up_tol_val;
	farr[1] = UA_lin_lo_tol_val;
	farr[2] = UA_d_lin_up_tol;
	farr[3] = UA_d_lin_lo_tol;
	farr[4] = UA_ang_up_tol_val;
	farr[5] = UA_ang_lo_tol_val;
	zero_supp = UA_tol_zero_sup; 
	d_zero_supp = UA_d_tol_z_sup; 
	round_off = UA_tol_roff_meth; 
	d_round_off = UA_d_tol_roff_meth; 
	farr[6] = UA_tol_rnd_fact;
	farr[7] = UA_d_tol_rnd_fact;

	status = ud_form(formfile, ans, ans);
   if (status==-1)
	   return -1;

	UA_tol_dec_places = places;
	UA_d_tol_dec_plac = d_places;
	UA_tol_method = method; 
	UA_tol_site = tol_site; 
	UA_lin_up_tol_val = farr[0];
	UA_lin_lo_tol_val = farr[1];
	UA_d_lin_up_tol = farr[2] ;
	UA_d_lin_lo_tol = farr[3];
	UA_ang_up_tol_val = farr[4];
	UA_ang_lo_tol_val = farr[5];
	UA_tol_zero_sup = zero_supp; 
	UA_d_tol_z_sup = d_zero_supp; 
	UA_tol_roff_meth = round_off; 
	UA_d_tol_roff_meth = d_round_off; 
	UA_tol_rnd_fact = farr[6];
	UA_d_tol_rnd_fact = farr[7];

	uu_dexit;
	}
