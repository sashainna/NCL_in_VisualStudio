#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"

/*********************************************************************
**    NAME         :  adimattr.c
**       CONTAINS:
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       adimattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:33
*********************************************************************/
static int choice[26][25];
#define NO_DEFAULT 0
#define DEFAULT 1
/*********************************************************************
**    E_FUNCTION     : ua_set_dim_attr(formfile)
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
ua_set_dim_attr(formfile)
	char	*formfile;
	
	{
	int status;
	static int  dim_type, d_dim_type, lin_units, ang_units, zero_sup,
					round_off, accuracy, format, diam_sym, diam_place, d_places,
					d_lin_units, d_ang_units, d_zero_sup, d_round_off, d_accuracy,
					d_format, rad_sym, rad_places, places;
	static UU_REAL r_fact, d_r_fact;
	static char sym[2], dsym[13], d_dsym[2], r_sym[13];

	static int *ans[] = {&dim_type, &d_dim_type, &places, (int *)&sym[0],
								&lin_units, &ang_units, &zero_sup, &round_off, 
								(int *)&r_fact, &accuracy, &format, &diam_sym, 
								(int *)&dsym[0], &diam_place, &d_places, 
								(int *)&d_dsym[0], &d_lin_units,
								&d_ang_units, &d_zero_sup, &d_round_off,
								(int *)&d_r_fact, &d_accuracy, &d_format,
						  		&rad_sym, (int *)&r_sym[0], &rad_places};

	uu_denter(UU_MTRC,(us,"ua_set_dim_attr()"));

	dim_type = UA_dim_type; 
	d_dim_type= UA_dual_format; 
	places = UA_dec_places;
	strcpy(sym, UA_dec_symbol);
	ua_set_lin_units(UA_linear_units, &lin_units);
	ang_units = UA_ang_units; 
	zero_sup = UA_dim_zero_sup; 
	round_off = UA_dim_roundoff; 
	r_fact = UA_dim_rnd_fact;
	format = UA_fraction_size/10;
	accuracy = UA_fraction_size - (format*10);
	diam_sym = UA_diam_symbol; 
	strcpy(dsym, UA_usr_dia_sym);
	diam_place = UA_dia_place; 
	d_places = UA_d_dec_places;
	strcpy(d_dsym, UA_d_dec_symbol);
	ua_set_lin_units(UA_d_lin_units, &d_lin_units);
	d_ang_units = UA_d_ang_units; 
	d_zero_sup = UA_d_dim_z_sup; 
	d_round_off = UA_d_dim_roundoff; 
	d_r_fact = UA_d_dim_rnd_fact;
	d_format = UA_d_frac_size/10;
	d_accuracy = UA_d_frac_size - (format*10);
	rad_sym = UA_radius_sym; 
	strcpy(r_sym, UA_usr_rad_sym);
	rad_places = UA_radius_place; 

	status = ud_form(formfile, ans, ans);
	if (status==-1)
		return;
	ua_set_lin_units(lin_units, &UA_linear_units);
	UA_ang_units 		=		ang_units;
	UA_dim_zero_sup 	=		zero_sup;
	UA_dim_roundoff 	=		round_off;
	UA_fraction_size  =     (format*10) + accuracy;
	UA_diam_symbol 	=		diam_sym;
	UA_dia_place 		=		diam_place;
	ua_set_lin_units(d_lin_units, &UA_d_lin_units);
	UA_d_ang_units 	=		d_ang_units;
	UA_d_dim_z_sup 	=		d_zero_sup;
	UA_d_dim_roundoff	=		d_round_off;
	UA_d_frac_size  	=     (d_format*10) + d_accuracy;
	UA_radius_sym 		=		rad_sym;
	UA_radius_place 	=		rad_places;
	UA_dual_format 	=		d_dim_type;
	UA_dim_type			= 		dim_type;
	UA_dec_places 		= 		places;
	strcpy(UA_dec_symbol, sym);
	UA_dim_rnd_fact 	= 		r_fact;
	strcpy(UA_usr_dia_sym, dsym);
	UA_d_dec_places 	= 		d_places;
	strcpy(UA_d_dec_symbol,d_dsym);
	UA_d_dim_rnd_fact = 		d_r_fact;
	strcpy(UA_usr_rad_sym, r_sym);

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_set_lin_units(in,out)
**       Set linear dimension  units
**    PARAMETERS   
**       INPUT  : 
**          in				      input value
**          out				   output value
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_lin_units(in, out)
	int in, *out;
	{
	uu_denter(UU_MTRC,(us,"ua_set_lin_units()"));

	switch(in)
		{
		case 0:
			*out = 3; 
			break;
		case 1:
			*out = 4; 
			break;
		case 2:
			*out = 5; 
			break;
		case 3:
			*out = 0; 
			break;
		case 4:
			*out = 1; 
			break;
		case 5:
			*out = 2; 
			break;
		}
	uu_dexit;
	}
