
/*********************************************************************
**    NAME         : autility.c
**       CONTAINS:
**				ua_degree_text
**				ua_english_text
**				ua_arch_text
**				ua_frac_blk
**				ua_dim_value
**    		ua_dual_degree_limit_text
**    		ua_degree_limit_text
**				ua_trim_line
**				ua_set_dim_text
**      	  ua_set_mdim_text
**      	  ua_set_ddim_text
**				ua_value_to_string
**				ua_dir_angle
**      	  ua_setcpln
**      	  ua_getcpln
**				ua_get_upvector
**				ua_set_rep_dim_text
**      	  ua_set_rep_mdim_text
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       autility.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:42
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) autility.c 3.4 6/24/88 10:51:58 single"};
#else
static char uu_sccsident[]={"@(#) autility.c 3.4 6/24/88 10:51:58 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "modef.h"

static UU_LOGICAL us_i110 = UU_TRUE;
static UU_LOGICAL us_i120 = UU_TRUE;
static UU_LOGICAL us_i130 = UU_TRUE;
static UU_LOGICAL us_i93  = UU_TRUE;

/*********************************************************************
**    E_FUNCTION     : us_init_autility()
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
us_init_autility()
	{
	}

/*********************************************************************
**    E_FUNCTION     : ua_set_dim_text(e)
**       Convert a real value to the dimension text string(s) given
**			current attribute settings for a dimension.
**    PARAMETERS   
**       INPUT  : 
**				e - dimension object_key to get the attribute setting from.
**       OUTPUT :  
**				e - dimension object_key with dim text strings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_dim_text(e)
struct UA_generic_draft	(*e);
	{

	uu_denter(UU_STRC,(us,"SAL ua_set_dim_text(e=%s)", "..."));

	ua_set_mdim_text(&((*e)));
	ua_set_ddim_text(&((*e)));
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_degree_tol_text(e, dimension, us_r67)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_degree_tol_text(e, dimension, us_r67)
struct UA_generic_draft	(*e);
UU_REAL	dimension;
char		us_r67[2];
	{
	UU_REAL	degrees;
	char		deg_text[21];
	UU_REAL	seconds;
	char		sec_text[21];
	char		min_text[21];
	UU_REAL	minutes;
	char		dtext[51];

	uu_denter(UU_STRC,(us,"ua_degree_tol_text(dimension=%g)",dimension));

	switch( (*e).ang_units )
		{
		case 0:
			{
			ua_value_to_string(dimension,(*e).tol_places,(*e).
			    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
			UA_dec_symbol,dtext);
			}
			break;
		case 1:
			{
			ua_rtos(dimension,0,deg_text);
			strcpy(dtext,deg_text);
			strcat(dtext,"\\m");
			}
			break;
		case 2:
			{
			ua_rtos(dimension,0,deg_text);
			strcpy(dtext,deg_text);
			strcat(dtext,"\\s");
			}
			break;
		case 3:
			{
			ua_value_to_string(dimension,(*e).tol_places,(*e).
			    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
			UA_dec_symbol,dtext);
			}
			break;
		}
	strcpy(us_r67,dtext);
	uu_dexit;
	return;
	}
/*********************************************************************
**    E_FUNCTION     : UU_REAL	ua_dir_angle(normal, vec1, vec2)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL	ua_dir_angle(normal, vec1, vec2)
UU_REAL	normal[3];
UU_REAL	vec1[3];
UU_REAL	vec2[3];
	{
	UU_REAL	ang;
	UU_REAL	uvec1[3];
	UU_REAL	uvec2[3];
	UU_REAL	nvec[3];

	uu_denter(UU_STRC,(us,"ua_dir_angle(normal=)"));

	um_unitvc(vec1,uvec1);
	um_unitvc(vec2,uvec2);
	um_unitvc(normal,nvec);
	ang = um_dot(uvec1,uvec2);
	if( ( fabs(ang)>9.999900e-001 ) )
		{
		if( ( ang>0.000000e+000 ) )
			{
			ang = 0.000000e+000;
			}
		else
			{
			ang = 3.141590e+000;
			}
		}
	else
		{
		ang = um_angle2p(uvec1,uvec2,nvec);
		}
	uu_dexit;
	return(ang);
	}
/*********************************************************************
**    E_FUNCTION     : UU_REAL	ua_dim_value(dval_type, dspace, dimvect,
**												pt1, pt2)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL	ua_dim_value(dval_type, dspace, dimvect, pt1, pt2)
enum UA_dvalue_types	dval_type;
enum UA_dim_spaces	dspace;
UU_REAL	dimvect[3];
UU_REAL	pt1[3];
UU_REAL	pt2[3];
	{
	UU_REAL	delta[3];
	UU_REAL	distance;

	uu_denter(UU_STRC,(us,"SAL ua_dim_value(dval_type=%d)",dval_type));

	um_vcmnvc(pt1,pt2,delta);
	distance = fabs(um_dot(dimvect,delta));
	uu_dexit;
	return(distance);
	}
/*********************************************************************
**    E_FUNCTION     : ua_frac_blk(e, numtext, dentext, type)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_frac_blk(e, numtext, dentext, type)
struct UA_generic_draft	(*e);
char		numtext[51];
char		dentext[51];
UU_LOGICAL	type;
	{
	char		fraction_text[51];
	int		format;

	uu_denter(UU_STRC,(us,"ua_frac_blk(numtext=%s)",numtext));

	(*e).txt_blk_use = ( (*e).txt_blk_use+3 );
	if( ( type==UU_TRUE ) )
		{
		format = ( (*e).fract_units/10 );
		}
	else
		{
		format = ( (*e).dual_f_units/10 );
		}
	if( ( format==0 ) )
		{
		strcpy(fraction_text,numtext);
		strcat(fraction_text,"\n-");
		strcat(fraction_text,"\n");
		strcat(fraction_text,dentext);
		}
	else
		{
		strcpy(fraction_text,numtext);
		strcat(fraction_text,"/");
		strcat(fraction_text,dentext);
		}
	(*e).txt_blk[( (*e).txt_blk_use-1 )-1].char_cnt = strlen(
	fraction_text);
	strcpy((*e).txt_blk[( (*e).txt_blk_use-1 )-1].tstring,
	fraction_text);
	if( ( type==UU_TRUE ) )
		{
		(*e).txt_blk[( (*e).txt_blk_use-1 )-1].subtype = main_txt1;
		}
	else
		{
		(*e).txt_blk[( (*e).txt_blk_use-1 )-1].subtype = main_txt2;
		}
	(*e).txt_blk[( (*e).txt_blk_use-1 )-1].txt_size = ( (*e).
	    sub_sup_ratio*(*e).char_size );
	(*e).txt_blk[( (*e).txt_blk_use-1 )-1].sub_super = (*e).
	    sub_sup_ratio;
	(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = 1;
	strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,"\\s");
	if( ( type==UU_TRUE ) )
		{
		(*e).txt_blk[(*e).txt_blk_use-1].subtype = main_txt1;
		}
	else
		{
		(*e).txt_blk[(*e).txt_blk_use-1].subtype = main_txt2;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_value_to_string(value, dec_places, zero_sup, 
**									round_off, round_factor, dec_symbol, us_r88)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_value_to_string(value, dec_places, zero_sup, 
round_off, round_factor, dec_symbol, us_r88)
UU_REAL	value;
int		dec_places;
int		zero_sup;
int		round_off;
UU_REAL	round_factor;
char		dec_symbol[2];
char		us_r88[2];
	{
	UU_REAL	temp;
	UU_REAL	tmp_value;
	UU_REAL	shftd_value;
	int		length;
	int		rnd_dec_places;
	char		dimension[51];
	int		i;

	uu_denter(UU_STRC,(us,"ua_value_to_string(value=%g)",value));

	rnd_dec_places = 0;
	temp = round_factor;
	if( ( round_factor>9.999000e-001 ) )
		{
		rnd_dec_places = 1;
		for(;;)
			{
			temp = ( temp/1.000000e+001 );
			if( ( temp<9.999000e-001 ) )
				{
				rnd_dec_places = ( -rnd_dec_places );
				goto us_l134;
				}
			else
				{
				rnd_dec_places = ( rnd_dec_places+1 );
				}
			}
us_l134: 
		;
		}
	else
		{
		for(;;)
			{
			temp = ( temp*1.000000e+001 );
			if( ( temp>9.999000e-001 ) )
				{
				goto us_l135;
				}
			else
				{
				rnd_dec_places = ( rnd_dec_places+1 );
				}
			}
us_l135: 
		;
		}
	switch( round_off )
		{
		case 0:
			{
			tmp_value = ( (UU_REAL)ua_trunc(( value*((UU_REAL) pow( 1.000000e+001,
							( (UU_REAL)dec_places ) ) ) )) );
			tmp_value = ( tmp_value*((UU_REAL) pow( 1.000000e+001, 
							( (UU_REAL)( -dec_places ) ) ) ) );
			}
			break;
		case 1:
			{
			tmp_value = ( (UU_REAL)ua_trunc(( ( value*((UU_REAL) pow( 1.000000e+001,
							( (UU_REAL)rnd_dec_places ) ) ) )+ 5.000000e-001 )) );
			tmp_value = ( tmp_value*((UU_REAL) pow( 1.000000e+001,
							( (UU_REAL)( -rnd_dec_places ) ) ) ) );
			}
			break;
		case 2:
			{
			shftd_value = ( ( value*((UU_REAL) pow( 1.000000e+001, 
								( (UU_REAL)rnd_dec_places ) ) ) )+5.000000e-001 );
			if( ( shftd_value==( (UU_REAL)ua_trunc(shftd_value) ) ) )
				{
				tmp_value = ( ( ( (UU_REAL)ua_trunc(shftd_value) )*
				    		((UU_REAL) pow( 1.000000e+001, ( (UU_REAL)( -rnd_dec_places
				   		 ) ) ) ) )-1.000000e+000 );
				}
			else
				{
				tmp_value = ( ( (UU_REAL)ua_trunc(shftd_value) )*
				   	 ((UU_REAL) pow( 1.000000e+001, ( (UU_REAL)( -rnd_dec_places
				   	 ) ) ) ) );
				}
			}
			break;
		case 3:
			{
			if( ( round_factor!=0.000000e+000 ) )
				{
				tmp_value = ( value+( round_factor/2.000000e+000 ) );
				tmp_value = ( tmp_value/round_factor );
				tmp_value = ( ( (UU_REAL)ua_trunc(tmp_value) )*round_factor );
				}
			else
				{
				tmp_value = value;
				}
			}
			break;
		}
	ua_rtos(tmp_value,dec_places,dimension);
	switch( zero_sup )
		{
		case 0:
			{
			goto next;
			}
		case 1:
			{
			length = strlen(dimension);
			if( ( dimension[0]=='0' ) )
				{
				strncpy( dimension, dimension + 2 - 1, (length - 2 + 1) );
				dimension[ (length - 2 + 1) ] = '\0';
				}
			}
			break;
		case 2:
			{
			length = strlen(dimension);
			for(;;)
				{
				if( ! (( dimension[length-1]=='0' )) ) goto us_l136;
				length = ( length-1 );
				strncpy( dimension, dimension + 1 - 1, (length - 1 + 1) );
				dimension[ (length - 1 + 1) ] = '\0';
				}
us_l136: 
			;
			}
			break;
		case 3:
			{
			length = strlen(dimension);
			for(;;)
				{
				if( ! (( dimension[length-1]=='0' )) ) goto us_l137;
				length = ( length-1 );
				strncpy( dimension, dimension + 1 - 1, (length - 1 + 1) );
				dimension[ (length - 1 + 1) ] = '\0';
				}
us_l137: 
			;
			if( ( dimension[0]=='0' ) )
				{
				strncpy( dimension, dimension + 2 - 1, (length - 2 + 1) );
				dimension[ (length - 2 + 1) ] = '\0';
				}
			}
			break;
		}
next:
	length = strlen(dimension);
		{
		int		us_t140;
		us_t140 = length;
		i = 1;
		for(;;)
			{
			if( i > us_t140 ) 	break;
			if( ( dimension[i-1]=='.' ) )
				{
				dimension[i-1] = dec_symbol[0];
				}
us_l138:
			i++ ;
			}
us_l139:;
		}
	strcpy(us_r88,dimension);
	uu_dexit;
	return;
	}
/*********************************************************************
**    E_FUNCTION     : ua_dual_degree_limit_text(e, dimension, up_value, 
**											low_value, uptext, lotext)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_dual_degree_limit_text(e, dimension, up_value, 
low_value, uptext, lotext)
struct UA_generic_draft	(*e);
UU_REAL	dimension;
UU_REAL	up_value;
UU_REAL	low_value;
char		uptext[51];
char		lotext[51];
	{
	UU_REAL	degrees;
	char		deg_text[21];
	UU_REAL	seconds;
	char		sec_text[21];
	char		min_text[21];
	UU_REAL	minutes;

	uu_denter(UU_STRC,(us,"ua_dual_degree_limit_text(dimension=%g)",dimension));

	switch( (*e).dual_a_units )
		{
		case 0:
			{
			ua_value_to_string(( dimension+up_value ),(*e).dual_place,(*
			    e).d_dim_z_sup,(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,
			UA_d_dec_symbol,uptext);
			strcpy(uptext,uptext);
			strcat(uptext,"\\d");
			ua_value_to_string(( dimension-low_value ),(*e).dual_place,
			(*e).d_dim_z_sup,(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,
			UA_d_dec_symbol,lotext);
			strcpy(lotext,lotext);
			strcat(lotext,"\\d");
			}
			break;
		case 1:
			{
			degrees = ( (UU_REAL)ua_trunc(dimension) );
			minutes = ( ( ( dimension-degrees )*6.000000e+001 )+up_value
			    );
			if( ( minutes>6.000000e+001 ) )
				{
				degrees = ( degrees+1.000000e+000 );
				minutes = ( minutes-6.000000e+001 );
				}
			ua_rtos(degrees,0,deg_text);
			ua_rtos(minutes,0,min_text);
			if( ( strcmp( min_text, "60" ) == 0 ) )
				{
				strcpy(min_text,"0");
				degrees = ( degrees+1.000000e+000 );
				ua_rtos(degrees,0,deg_text);
				}
			strcpy(uptext,deg_text);
			strcat(uptext,"\\d");
			strcat(uptext,min_text);
			strcat(uptext,"\\m");
			degrees = ( (UU_REAL)ua_trunc(dimension) );
			minutes = ( ( ( dimension-degrees )*6.000000e+001 )-
						    low_value );
			if( ( minutes<0.000000e+000 ) )
				{
				degrees = ( degrees-1.000000e+000 );
				minutes = ( minutes+6.000000e+001 );
				}
			ua_rtos(degrees,0,deg_text);
			ua_rtos(minutes,0,min_text);
			if( ( strcmp( min_text, "60" ) == 0 ) )
				{
				strcpy(min_text,"0");
				degrees = ( degrees+1.000000e+000 );
				ua_rtos(degrees,0,deg_text);
				}
			strcpy(lotext,deg_text);
			strcat(lotext,"\\d");
			strcat(lotext,min_text);
			strcat(lotext,"\\m");
			}
			break;
		case 2:
			{
			degrees = ( (UU_REAL)ua_trunc(dimension) );
			minutes = ( (UU_REAL)ua_trunc(( ( dimension-degrees )*
			   			 6.000000e+001 )) );
			seconds = ( ( ( ( dimension-degrees )-( minutes/
							6.000000e+001 ) )*3.600000e+003 )+up_value );
			if( ( seconds>6.000000e+001 ) )
				{
				seconds = ( seconds-6.000000e+001 );
				minutes = ( minutes+1.000000e+000 );
				if( ( minutes>6.000000e+001 ) )
					{
					minutes = ( minutes-6.000000e+001 );
					degrees = ( degrees+1.000000e+000 );
					}
				}
			ua_rtos(degrees,0,deg_text);
			ua_rtos(minutes,0,min_text);
			ua_rtos(seconds,0,sec_text);
			if( ( strcmp( sec_text, "60" ) == 0 ) )
				{
				strcpy(sec_text,"0");
				minutes = ( minutes+1.000000e+000 );
				ua_rtos(minutes,0,min_text);
				}
			if( ( strcmp( min_text, "60" ) == 0 ) )
				{
				strcpy(min_text,"0");
				degrees = ( degrees+1.000000e+000 );
				ua_rtos(degrees,0,deg_text);
				}
			strcpy(uptext,deg_text);
			strcat(uptext,"\\d");
			strcat(uptext,min_text);
			strcat(uptext,"\\m");
			strcat(uptext,sec_text);
			strcat(uptext,"\\s");
			degrees = ( (UU_REAL)ua_trunc(dimension) );
			minutes = ( (UU_REAL)ua_trunc(( ( dimension-degrees )*
			   		 6.000000e+001 )) );
			seconds = ( ( ( ( dimension-degrees )-( minutes/
							6.000000e+001 ) )*3.600000e+003 )-low_value );
			if( ( seconds<0.000000e+000 ) )
				{
				minutes = ( minutes-1.000000e+000 );
				seconds = ( seconds+6.000000e+001 );
				if( ( minutes<0.000000e+000 ) )
					{
					degrees = ( degrees-1.000000e+000 );
					minutes = ( minutes+6.000000e+001 );
					}
				}
			ua_rtos(degrees,0,deg_text);
			ua_rtos(minutes,0,min_text);
			ua_rtos(seconds,0,sec_text);
			if( ( strcmp( sec_text, "60" ) == 0 ) )
				{
				strcpy(sec_text,"0");
				minutes = ( minutes+1.000000e+000 );
				ua_rtos(minutes,0,min_text);
				}
			if( ( strcmp( min_text, "60" ) == 0 ) )
				{
				strcpy(min_text,"0");
				degrees = ( degrees+1.000000e+000 );
				ua_rtos(degrees,0,deg_text);
				}
			strcpy(uptext,deg_text);
			strcat(uptext,"\\d");
			strcat(uptext,min_text);
			strcat(uptext,"\\m");
			strcat(uptext,sec_text);
			strcat(uptext,"\\s");
			}
			break;
		case 3:
			{
			ua_value_to_string(( dimension+up_value ),(*e).dual_place,
						(* e).d_dim_z_sup,(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,
						UA_d_dec_symbol,uptext);
			strcpy(uptext,uptext);
			strcat(uptext,"rad");
			ua_value_to_string(( dimension-low_value ),(*e).dual_place,
							(*e).d_dim_z_sup,(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,
							UA_d_dec_symbol,lotext);
			strcpy(lotext,lotext);
			strcat(lotext,"rad");
			}
			break;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_arch_text(e, dimension, factor, md_flag, dtext
**										, numtext, dentext, inch_flg, fraction)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_arch_text(e, dimension, factor, md_flag, dtext
								, numtext, dentext, inch_flg, fraction)
struct UA_generic_draft	(*e);
UU_REAL	dimension;
UU_REAL	factor;
UU_LOGICAL	md_flag;
char		dtext[51];
char		numtext[51];
char		dentext[51];
UU_LOGICAL	(*inch_flg);
UU_LOGICAL	(*fraction);
	{
	char		inch_text[21];
	char		feet_text[21];
	UU_REAL	temp;
	UU_REAL	den;
	char		temp_text[21];
	UU_REAL	rounder;
	UU_REAL	num;
	UU_REAL	inches;
	UU_REAL	remainder;
	static UU_REAL	whole[6];
	static UU_REAL	frac[6];
	UU_REAL	feet;
	int		acc;
	UU_LOGICAL	feet_flg;

	uu_denter(UU_STRC,(us,"ua_arch_text(dimension=%g)",dimension));

	if( us_i93 )
		{
		us_i93 = UU_FALSE;
		whole[0] = 2.000000e+000;
		whole[1] = 4.000000e+000;
		whole[2] = 8.000000e+000;
		whole[3] = 1.600000e+001;
		whole[4] = 3.200000e+001;
		whole[5] = 6.400000e+001;
		frac[0] = 5.000000e-001;
		frac[1] = 2.500000e-001;
		frac[2] = 1.250000e-001;
		frac[3] = 6.250000e-002;
		frac[4] = 3.125000e-002;
		frac[5] = 1.562500e-002;
		}

	strcpy(dtext,"");
	temp = ( dimension/1.200000e+001 );
	feet = ( (UU_REAL)ua_trunc(temp) );
	inches = ( ( ( temp-feet )*1.200000e+001 )+5.000000e-004 );
	temp = ( inches/1.200000e+001 );
	(*fraction) = UU_FALSE;
	if( ( temp>1.000000e+000 ) )
		{
		feet = ( feet+1.000000e+000 );
		inches = ( ( temp-1.000000e+000 )*1.200000e+001 );
		}
	if( ( feet>factor ) )
		{
		ua_rtos(feet,0,feet_text);
		strcpy(dtext,feet_text);
		strcat(dtext,"\\m");
		}
	if( ( inches<factor ) )
		{
		(*inch_flg) = UU_FALSE;
		strcpy(numtext,"");
		strcpy(dentext,"");
		}
	else
		{
		temp = ( (UU_REAL)ua_trunc(inches) );
		remainder = ( inches-temp );
		remainder = ( remainder+5.000000e-004 );
		if( ( remainder>1.000000e+000 ) )
			{
			temp = ( temp+1.000000e+000 );
			remainder = ( remainder-1.000000e+000 );
			}
		inches = 0.000000e+000;
		if( ( temp>factor ) )
			{
			ua_rtos(temp,0,inch_text);
			strcpy(temp_text,dtext);
			strcat(temp_text,inch_text);
			strcpy(dtext,temp_text);
			(*inch_flg) = UU_TRUE;
			inches = temp;
			}
		if( ( md_flag==UU_TRUE ) )
			{
			acc = ( (*e).fract_units-( ( (*e).fract_units/10 )*10 ) );
			}
		else
			{
			acc = ( (*e).dual_f_units-( ( (*e).dual_f_units/10 )*10 ) );
			}
		temp = frac[( acc+1 )-1];
		den = whole[( acc+1 )-1];
		if( ( remainder<temp ) )
			{
			if( ( (*inch_flg)==UU_TRUE ) )
				{
				strcpy(temp_text,dtext);
				strcat(temp_text,"\\s");
				strcpy(dtext,temp_text);
				}
			uu_dexit;
			return;
			}
		else
			{
			rounder = ( remainder/temp );
			num = ( (UU_REAL)ua_trunc(rounder) );
			temp = ( rounder-num );
			if( ( temp>5.000000e-001 ) )
				{
				num = ( num+1.000000e+000 );
				}
			for(;;)
				{
				temp = ( (UU_REAL)ua_trunc(( ( num/2.000000e+000 )+ 5.000000e-004 )) );
				remainder = fabs(( num-( 2.000000e+000*temp ) ));
				if( ( remainder>1.000000e-003 ) )
					{
					goto us_l141;
					}
				num = ( (UU_REAL)ua_trunc(( ( num/2.000000e+000 )+5.000000e-004)) );
				den = ( (UU_REAL)ua_trunc(( ( den/2.000000e+000 )+5.000000e-004)) );
				}
us_l141: 
			;
			if( ( fabs(( den-1.000000e+000 ))<1.000000e-004 ) )
				{
				inches = ( inches+1.000000e+000 );
				strcpy(dtext,"");
				if( ( feet>factor ) )
					{
					ua_rtos(feet,0,feet_text);
					strcpy(dtext,feet_text);
					strcat(dtext,"\\m");
					}
				if( ( inches>factor ) )
					{
					ua_rtos(inches,0,inch_text);
					strcpy(temp_text,dtext);
					strcat(temp_text,inch_text);
					strcat(temp_text,"\\s");
					strcpy(dtext,temp_text);
					(*fraction) = UU_FALSE;
					}
				}
			else
				{
				ua_rtos(num,0,numtext);
				ua_rtos(den,0,dentext);
				(*fraction) = UU_TRUE;
				}
			}
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_trim_line(text_box, ept1, ept2, us_r95)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_trim_line(text_box, ept1, ept2, us_r95)
UU_REAL	text_box[4][3];
UU_REAL	ept1[3];
UU_REAL	ept2[3];
UU_REAL	us_r95[3];
	{
	UU_REAL	vec1[3];
	UU_REAL	close;
	UU_REAL	dis1;
	UU_REAL	dis2;
	UU_REAL	int_pts[2][3];
	int		i;
	int		j;
	UU_LOGICAL	status;

	static int		points[5] = 	{ 0,1,2,3,0		};


	uu_denter(UU_STRC,(us,"ua_trim_line(ept1=<%g>)",ept2[0]));

		{
		UU_REAL	us_t142[3];
		um_vcmnvc(ept2,ept1,us_t142);
		um_unitvc(us_t142,vec1);
		}
	ua_box_int(text_box,ept1,vec1,int_pts);
	um_vctovc(int_pts[0],us_r95);
	uu_dexit;
	return;
	}
/*********************************************************************
**    E_FUNCTION     : ua_degree_limit_text(e, dimension, up_value, 
**										low_value, uptext, lotext)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_degree_limit_text(e, dimension, up_value, 
											low_value, uptext, lotext)
struct UA_generic_draft	(*e);
UU_REAL	dimension;
UU_REAL	up_value;
UU_REAL	low_value;
char		uptext[51];
char		lotext[51];
	{
	UU_REAL	degrees;
	char		deg_text[21];
	UU_REAL	seconds;
	char		sec_text[21];
	char		min_text[21];
	UU_REAL	minutes;

	uu_denter(UU_STRC,(us,"ua_degree_limit_text(dimension=%g)",dimension));

	switch( (*e).ang_units )
		{
		case 0:
			{
			ua_value_to_string(( dimension+up_value ),(*e).dim_places,
						(*e).dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,uptext);
			strcpy(uptext,uptext);
			strcat(uptext,"\\d");
			ua_value_to_string(( dimension-low_value ),(*e).dim_places,
						(*e).dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,lotext);
			strcpy(lotext,lotext);
			strcat(lotext,"\\d");
			}
			break;
		case 1:
			{
			degrees = ( (UU_REAL)ua_trunc(dimension) );
			minutes = ( ( ( dimension-degrees )*6.000000e+001 )+up_value);
			if( ( minutes>6.000000e+001 ) )
				{
				minutes = ( minutes-6.000000e+001 );
				degrees = ( degrees+1.000000e+000 );
				}
			ua_rtos(degrees,0,deg_text);
			ua_rtos(minutes,0,min_text);
			if( ( strcmp( min_text, "60" ) == 0 ) )
				{
				strcpy(min_text,"0");
				degrees = ( degrees+1.000000e+000 );
				ua_rtos(degrees,0,deg_text);
				}
			strcpy(uptext,deg_text);
			strcat(uptext,"\\d");
			strcat(uptext,min_text);
			strcat(uptext,"\\m");
			degrees = ( (UU_REAL)ua_trunc(dimension) );
			minutes = ( ( ( dimension-degrees )*6.000000e+001 )- low_value );
			if( ( minutes<0.000000e+000 ) )
				{
				degrees = ( degrees-1.000000e+000 );
				minutes = ( minutes+6.000000e+001 );
				}
			ua_rtos(degrees,0,deg_text);
			ua_rtos(minutes,0,min_text);
			if( ( strcmp( min_text, "60" ) == 0 ) )
				{
				strcpy(min_text,"0");
				degrees = ( degrees+1.000000e+000 );
				ua_rtos(degrees,0,deg_text);
				}
			strcpy(lotext,deg_text);
			strcat(lotext,"\\d");
			strcat(lotext,min_text);
			strcat(lotext,"\\m");
			}
			break;
		case 2:
			{
			degrees = ( (UU_REAL)ua_trunc(dimension) );
			minutes = ( (UU_REAL)ua_trunc(( ( dimension-degrees )* 6.000000e+001 )) );
			seconds = ( ( ( ( dimension-degrees )-( minutes/
									6.000000e+001 ) )*3.600000e+003 )+up_value );
			if( ( seconds>6.000000e+001 ) )
				{
				seconds = ( seconds-6.000000e+001 );
				minutes = ( minutes+1.000000e+000 );
				if( ( minutes>6.000000e+001 ) )
					{
					minutes = ( minutes-6.000000e+001 );
					degrees = ( degrees+1.000000e+000 );
					}
				}
			ua_rtos(degrees,0,deg_text);
			ua_rtos(minutes,0,min_text);
			ua_rtos(seconds,0,sec_text);
			if( ( strcmp( sec_text, "60" ) == 0 ) )
				{
				strcpy(sec_text,"0");
				minutes = ( minutes+1.000000e+000 );
				ua_rtos(minutes,0,min_text);
				}
			if( ( strcmp( min_text, "60" ) == 0 ) )
				{
				strcpy(min_text,"0");
				degrees = ( degrees+1.000000e+000 );
				ua_rtos(degrees,0,deg_text);
				}
			strcpy(uptext,deg_text);
			strcat(uptext,"\\d");
			strcat(uptext,min_text);
			strcat(uptext,"\\m");
			strcat(uptext,sec_text);
			strcat(uptext,"\\s");
			degrees = ( (UU_REAL)ua_trunc(dimension) );
			minutes = ( (UU_REAL)ua_trunc(( ( dimension-degrees )* 6.000000e+001 )) );
			seconds = ( ( ( ( dimension-degrees )-( minutes/
							6.000000e+001 ) )*3.600000e+003 )-low_value );
			if( ( seconds<0.000000e+000 ) )
				{
				minutes = ( minutes-1.000000e+000 );
				seconds = ( seconds+6.000000e+001 );
				if( ( minutes<0.000000e+000 ) )
					{
					minutes = ( minutes+6.000000e+001 );
					degrees = ( degrees-1.000000e+000 );
					}
				}
			ua_rtos(degrees,0,deg_text);
			ua_rtos(minutes,0,min_text);
			ua_rtos(seconds,0,sec_text);
			if( ( strcmp( sec_text, "60" ) == 0 ) )
				{
				strcpy(sec_text,"0");
				minutes = ( minutes+1.000000e+000 );
				ua_rtos(minutes,0,min_text);
				}
			if( ( strcmp( min_text, "60" ) == 0 ) )
				{
				strcpy(min_text,"0");
				degrees = ( degrees+1.000000e+000 );
				ua_rtos(degrees,0,deg_text);
				}
			strcpy(uptext,deg_text);
			strcat(uptext,"\\d");
			strcat(uptext,min_text);
			strcat(uptext,"\\m");
			strcat(uptext,sec_text);
			strcat(uptext,"\\s");
			}
			break;
		case 3:
			{
			ua_value_to_string(( dimension+up_value ),(*e).dim_places,
						(*e).dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,uptext);
			strcpy(uptext,uptext);
			strcat(uptext,"rad");
			ua_value_to_string(( dimension-low_value ),(*e).dim_places,
						(*e).dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,lotext);
			strcpy(lotext,lotext);
			strcat(lotext,"rad");
			}
			break;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_getcpln(entity, cpln_origin, xaxis, yaxis, zaxis)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_getcpln(entity, cpln_origin, xaxis, yaxis, zaxis)
struct UA_generic_draft	(*entity);
UU_REAL	cpln_origin[3];
UU_REAL	xaxis[3];
UU_REAL	yaxis[3];
UU_REAL	zaxis[3];
	{

	uu_denter(UU_STRC,(us,"ua_getcpln(zaxis=%s)","..."));

	um_vctovc((*entity).cpln.cpln_origin,cpln_origin);
	um_vctovc((*entity).cpln.xaxis,xaxis);
	um_vctovc((*entity).cpln.yaxis,yaxis);
	um_vctovc((*entity).cpln.zaxis,zaxis);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_set_ddim_text(e)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_ddim_text(e)
struct UA_generic_draft	(*e);
	{
	char		fractext[51];
	UU_REAL	lolimit;
	char		dentext[51];
	UU_LOGICAL	inches;
	UU_REAL	factor;
	UU_REAL	uplimit;
	static UU_REAL	angular_convert[4];
	int		format;
	char		toltext[51];
	char		numtext[51];
	UU_REAL	dimension;
	int		i;
	static UU_REAL	linear_convert[6];
	char		lotext[51];
	UU_LOGICAL	arch;
	UU_REAL	conv_fact;
	char		uptext[51];
	char		dtext[51];
	UU_LOGICAL	fraction;
	int		start;
	UU_REAL um_length_conversion();

	uu_denter(UU_STRC,(us,"SAL ua_set_ddim_text(e=%s)", "..."));

	if( us_i110 )
		{
		us_i110 = UU_FALSE;
		angular_convert[0] = 5.729578e+001;
		angular_convert[1] = 5.729578e+001;
		angular_convert[2] = 5.729578e+001;
		angular_convert[3] = 1.000000e+000;
		linear_convert[0] = 3.937008e-001;
		linear_convert[1] = 3.937008e-001;
		linear_convert[2] = 3.937008e-001;
		linear_convert[3] = 1.000000e+001;
		linear_convert[4] = 1.000000e+000;
		linear_convert[5] = 1.000000e-002;
		}

	if( ( (*e).dual_format!=0 ) )
		{
		if( ( (*e).etype==50 ) )
			{
			dimension = ((*e).dim_value*angular_convert[( (*e).dual_a_units+1)-1]);
			}
		else
			{
			conv_fact = um_length_conversion(4);
			dimension = (((*e).dim_value*conv_fact )*linear_convert
								[((*e).dual_l_units+1 )-1] );
			}
		start = ( (*e).txt_blk_use+1 );
		arch = UU_FALSE;
		inches = UU_FALSE;
		fraction = UU_FALSE;
		switch( (*e).tol_method )
			{
			case 0:
				{
				switch( (*e).etype )
					{
					case 50:
						{
						ua_degree_text(&((*e)),dimension,dtext);
						}
						break;
					default:
						{
						if( ( (*e).dual_l_units==1 ) )
							{
							ua_english_text(&((*e)),dimension,factor,dtext);
							}
						else if( ( (*e).dual_l_units==2 ) )
							{
							arch = UU_TRUE;
							ua_arch_text(&((*e)),dimension,factor,UU_FALSE,dtext,
										numtext,dentext,&(inches),&(fraction));
							}
						else
							{
							ua_value_to_string(dimension,(*e).dual_place,
							(*e).d_dim_z_sup,(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,
							UA_d_dec_symbol,dtext);
							}
						}
					}
				if( ( arch==UU_TRUE ) )
					{
					if( ( fraction==UU_TRUE ) )
						{
						ua_frac_blk(&((*e)),numtext,dentext,UU_FALSE);
						}
					else
						{
						(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
						}
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
				break;
			case 4:
			case 3:
			case 2:
			case 1:
				{
				uplimit = ( dimension+(*e).d_lin_up_tol );
				lolimit = ( dimension-(*e).d_lin_lo_tol );
				if( ( (*e).etype!=50 ) )
					{
					ua_value_to_string(uplimit,(*e).dual_place,(*e).d_dim_z_sup,
					(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,UA_d_dec_symbol,
					uptext);
					ua_value_to_string(lolimit,(*e).dual_place,(*e).d_dim_z_sup,
					(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,UA_d_dec_symbol,
					lotext);
					}
				else
					{
					ua_dual_degree_limit_text(&((*e)),dimension,
					(*e).upper_tol,(*e).lower_tol,uptext,lotext);
					}
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				switch( (*e).tol_method )
					{
					case 1:
						{
						strcpy(dtext,lotext);
						strcat(dtext," - ");
						strcat(dtext,uptext);
						}
						break;
					case 2:
						{
						strcpy(dtext,uptext);
						strcat(dtext,"\n");
						strcat(dtext,lotext);
						}
						break;
					case 3:
						{
						strcpy(dtext,uptext);
						strcat(dtext," - ");
						strcat(dtext,lotext);
						}
						break;
					case 4:
						{
						strcpy(dtext,lotext);
						strcat(dtext,"\n");
						strcat(dtext,uptext);
						}
						break;
					}
				}
				break;
			case 5:
				{
				if( ( (*e).etype!=50 ) )
					{
					if( ( (*e).dual_l_units==1 ) )
						{
						ua_english_text(&((*e)),dimension,factor,dtext);
						}
					else if( ( (*e).dual_l_units==2 ) )
						{
						arch = UU_TRUE;
						ua_arch_text(&((*e)),dimension,factor,UU_FALSE,dtext,numtext
						    ,dentext,&(inches),&(fraction));
						}
					else
						{
						ua_value_to_string(dimension,(*e).dual_place,
							(*e).d_dim_z_sup,(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,
							UA_d_dec_symbol,dtext);
						}
					}
				else
					{
					ua_degree_text(&((*e)),dimension,dtext);
					}
				if( ( arch==UU_TRUE ) )
					{
					if( ( fraction==UU_TRUE ) )
						{
						ua_frac_blk(&((*e)),numtext,dentext,UU_FALSE);
						}
					else
						{
						(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
						}
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				if( ( (*e).etype!=50 ) )
					{
						{
						char		us_t143[1025];
						strcpy(toltext,"\\+");
						ua_value_to_string((*e).d_lin_up_tol,(*e).du_tol_pl,
							(*e).d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
							UA_d_dec_symbol,us_t143);
						strcat(toltext,us_t143);
						}
					}
				else
					{
						{
						char		us_t144[1025];
						strcpy(toltext,"\\+");
						ua_dual_degree_tol_text(&((*e)),(*e).upper_tol,us_t144);
						strcat(toltext,us_t144);
						}
					}
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
				strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
				(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt2;
				}
				break;
			case 6:
				{
				if( ( (*e).etype!=50 ) )
					{
					if( ( (*e).dual_l_units==1 ) )
						{
						ua_english_text(&((*e)),dimension,factor,dtext);
						}
					else if( ( (*e).dual_l_units==2 ) )
						{
						arch = UU_TRUE;
						ua_arch_text(&((*e)),dimension,factor,UU_FALSE,dtext,numtext
						    ,dentext,&(inches),&(fraction));
						}
					else
						{
						ua_value_to_string(dimension,(*e).dual_place,
							(*e).d_dim_z_sup,(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,
							UA_d_dec_symbol,dtext);
						}
					}
				else
					{
					ua_degree_text(&((*e)),dimension,dtext);
					}
				if( ( arch==UU_TRUE ) )
					{
					if( ( fraction==UU_TRUE ) )
						{
						ua_frac_blk(&((*e)),numtext,dentext,UU_FALSE);
						}
					else
						{
						(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
						}
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				if( ( (*e).etype!=50 ) )
					{
						{
						char		us_t145[1025];
						char		us_t146[1025];
						strcpy(toltext,"+");
						ua_value_to_string((*e).d_lin_up_tol,(*e).du_tol_pl,
							(*e).d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
							UA_d_dec_symbol,us_t145);
						strcat(toltext,us_t145);
						strcat(toltext,"\n-");
						ua_value_to_string((*e).d_lin_lo_tol,(*e).du_tol_pl,
							(*e).d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
							UA_d_dec_symbol,us_t146);
						strcat(toltext,us_t146);
						}
					}
				else
					{
						{
						char		us_t147[1025];
						char		us_t148[1025];
						strcpy(toltext,"+");
						ua_dual_degree_tol_text(&((*e)),(*e).upper_tol,us_t147);
						strcat(toltext,us_t147);
						strcat(toltext,"\n-");
						ua_dual_degree_tol_text(&((*e)),(*e).lower_tol,us_t148);
						strcat(toltext,us_t148);
						}
					}
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
				strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
				(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt2;
				(*e).txt_blk[(*e).txt_blk_use-1].txt_size =
								( (*e).sub_sup_ratio*(*e).char_size );
				(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
				}
				break;
			case 7:
				{
				if( ( (*e).etype!=50 ) )
					{
					if( ( (*e).dual_l_units==1 ) )
						{
						ua_english_text(&((*e)),dimension,factor,dtext);
						}
					else if( ( (*e).dual_l_units==2 ) )
						{
						arch = UU_TRUE;
						ua_arch_text(&((*e)),dimension,factor,UU_FALSE,dtext,numtext
						    ,dentext,&(inches),&(fraction));
						}
					else
						{
						ua_value_to_string(dimension,(*e).dual_place,
							(*e).d_dim_z_sup,(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,
							UA_d_dec_symbol,dtext);
						}
					}
				else
					{
					ua_degree_text(&((*e)),dimension,dtext);
					}
				if( ( arch==UU_TRUE ) )
					{
					if( ( fraction==UU_TRUE ) )
						{
						ua_frac_blk(&((*e)),numtext,dentext,UU_FALSE);
						}
					else
						{
						(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
						}
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				if( ( (*e).etype!=50 ) )
					{
						{
						char		us_t149[1025];
						char		us_t150[1025];
						strcpy(toltext,"+");
						ua_value_to_string((*e).d_lin_up_tol,(*e).du_tol_pl,(*e).
						    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
							UA_d_dec_symbol,us_t149);
						strcat(toltext,us_t149);
						strcat(toltext,"\n-");
						ua_value_to_string(0.000000e+000,(*e).du_tol_pl,(*e).
						    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
							UA_d_dec_symbol,us_t150);
						strcat(toltext,us_t150);
						}
					}
				else
					{
						{
						char		us_t151[1025];
						char		us_t152[1025];
						strcpy(toltext,"+");
						ua_dual_degree_tol_text(&((*e)),(*e).upper_tol,us_t151);
						strcat(toltext,us_t151);
						strcat(toltext,"\n-");
						ua_dual_degree_tol_text(&((*e)),0.000000e+000,us_t152);
						strcat(toltext,us_t152);
						}
					}
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
				strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
				(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt2;
				(*e).txt_blk[(*e).txt_blk_use-1].txt_size = 
							( (*e).sub_sup_ratio*(*e).char_size );
				(*e).txt_blk[(*e).txt_blk_use-1].sub_super = 
							(*e).sub_sup_ratio;
				}
				break;
			case 8:
				{
				if( ( (*e).etype!=50 ) )
					{
					if( ( (*e).dual_l_units==1 ) )
						{
						ua_english_text(&((*e)),dimension,factor,dtext);
						}
					else if( ( (*e).dual_l_units==2 ) )
						{
						arch = UU_TRUE;
						ua_arch_text(&((*e)),dimension,factor,UU_FALSE,dtext,numtext
						    ,dentext,&(inches),&(fraction));
						}
					else
						{
						ua_value_to_string(dimension,(*e).dual_place,(*e).
						    d_dim_z_sup,(*e).d_dim_roundoff,(*e).d_dim_rnd_fact,
							UA_d_dec_symbol,dtext);
						}
					}
				else
					{
					ua_degree_text(&((*e)),dimension,dtext);
					}
				if( ( arch==UU_TRUE ) )
					{
					if( ( fraction==UU_TRUE ) )
						{
						ua_frac_blk(&((*e)),numtext,dentext,UU_FALSE);
						}
					else
						{
						(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
						}
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				if( ( (*e).etype!=50 ) )
					{
						{
						char		us_t153[1025];
						char		us_t154[1025];
						strcpy(toltext,"+");
						ua_value_to_string(0.000000e+000,(*e).du_tol_pl,(*e).
						    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
							UA_d_dec_symbol,us_t153);
						strcat(toltext,us_t153);
						strcat(toltext,"\n-");
						ua_value_to_string((*e).d_lin_lo_tol,(*e).du_tol_pl,(*e).
						    d_tol_z_sup,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,
							UA_d_dec_symbol,us_t154);
						strcat(toltext,us_t154);
						}
					}
				else
					{
						{
						char		us_t155[1025];
						char		us_t156[1025];
						strcpy(toltext,"+");
						ua_dual_degree_tol_text(&((*e)),0.000000e+000,us_t155);
						strcat(toltext,us_t155);
						strcat(toltext,"\n-");
						ua_dual_degree_tol_text(&((*e)),(*e).lower_tol,us_t156);
						strcat(toltext,us_t156);
						}
					}
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
				strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
				(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt2;
				(*e).txt_blk[(*e).txt_blk_use-1].txt_size = 
								( (*e).sub_sup_ratio*(*e).char_size );
				(*e).txt_blk[(*e).txt_blk_use-1].sub_super =
								(*e).sub_sup_ratio;
				}
				break;
			}
		(*e).txt_blk[start-1].char_cnt = strlen(dtext);
		strcpy((*e).txt_blk[start-1].tstring,dtext);
		(*e).txt_blk[start-1].subtype = main_txt2;
			{
			int		us_t159;
			us_t159 = (*e).txt_blk_use;
			i = start;
			for(;;)
				{
				if( i > us_t159 ) 	break;
				(*e).txt_blk[i-1].txt_just = 2;
us_l157:
				i++ ;
				}
us_l158: 
			;
			}
		}
	format = (*e).dual_format;
	if( ( ( ( ( format==5 )||( format==6 ) )||( format==7 ) )||
	    ( format==8 ) ) )
		{
		if( ( ( ( ( ( (*e).tol_method==2 )||( (*e).tol_method==4 ) )
		    ||( (*e).tol_method==7 ) )||( (*e).tol_method==8 ) )||
			 ( (*e).tol_method==6 ) ) )
			{
			uu_dexit;
			return;
			}
		else
			{
			strcpy(toltext,(*e).txt_blk[start-1].tstring);
			strcpy(dtext,"[");
			strcat(dtext,toltext);
			(*e).txt_blk[start-1].char_cnt = strlen(dtext);
			strcpy((*e).txt_blk[start-1].tstring,dtext);
			i = (*e).txt_blk_use;
			strcpy(toltext,(*e).txt_blk[i-1].tstring);
			strcpy(dtext,toltext);
			strcat(dtext,"]");
			(*e).txt_blk[i-1].char_cnt = strlen(dtext);
			strcpy((*e).txt_blk[i-1].tstring,dtext);
			}
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_setcpln(entity)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_setcpln(entity)
struct UA_generic_draft	(*entity);
	{

	uu_denter(UU_STRC,(us,"SAL ua_setcpln(entity=%s)", "..."));

	um_getcpln((*entity).cpln.cpln_origin,(*entity).cpln.xaxis,
						(*entity).cpln.yaxis,(*entity).cpln.zaxis);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_english_text(e, dimension, factor, dtext)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_english_text(e, dimension, factor, dtext)
struct UA_generic_draft	(*e);
UU_REAL	dimension;
UU_REAL	factor;
char		dtext[51];
	{
	char		inch_text[21];
	char		feet_text[21];
	UU_REAL	temp;
	char		temp_text[21];
	UU_REAL	inches;
	UU_REAL	feet;

	uu_denter(UU_STRC,(us,"ua_english_text(dimension=%g)",dimension));

	temp = ( dimension/1.200000e+001 );
	feet = ( (UU_REAL)ua_trunc(temp) );
	inches = ( ( ( temp-feet )*1.200000e+001 )+5.000000e-004 );
	temp = ( inches/1.200000e+001 );
	if( ( temp>1.000000e+000 ) )
		{
		feet = ( feet+1.000000e+000 );
		inches = ( ( temp-1.000000e+000 )*1.200000e+001 );
		}
	if( ( feet>factor ) )
		{
		ua_rtos(feet,0,feet_text);
		strcpy(dtext,feet_text);
		strcat(dtext,"\\m");
		}
	if( ( inches>factor ) )
		{
		ua_value_to_string(inches,(*e).dim_places,(*e).dim_zero_sup,
					(*e).dim_roundoff,(*e).dim_rnd_fact,UA_dec_symbol,inch_text);
		strcpy(temp_text,dtext);
		strcat(temp_text,inch_text);
		strcat(temp_text,"\\s");
		strcpy(dtext,temp_text);
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_set_mdim_text(e)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_mdim_text(e)
struct UA_generic_draft	(*e);
	{
	char		fractext[51];
	UU_REAL	lolimit;
	char		dentext[51];
	UU_LOGICAL	inches;
	UU_REAL	factor;
	UU_REAL	uplimit;
	static UU_REAL	angular_convert[4];
	char		toltext[51];
	char		numtext[51];
	UU_REAL	dimension;
	int		i;
	static UU_REAL	linear_convert[6];
	char		lotext[51];
	UU_LOGICAL	arch;
	UU_REAL	conv_fact;
	char		uptext[51];
	char		dtext[51];
	UU_LOGICAL	fraction;
	int		start;
	UU_REAL um_length_conversion();

	uu_denter(UU_STRC,(us,"SAL ua_set_mdim_text(e=%s)", "..."));

	if( us_i120 )
		{
		us_i120 = UU_FALSE;
		angular_convert[0] = 5.729578e+001;
		angular_convert[1] = 5.729578e+001;
		angular_convert[2] = 5.729578e+001;
		angular_convert[3] = 1.000000e+000;
		linear_convert[0] = 3.937008e-001;
		linear_convert[1] = 3.937008e-001;
		linear_convert[2] = 3.937008e-001;
		linear_convert[3] = 1.000000e+001;
		linear_convert[4] = 1.000000e+000;
		linear_convert[5] = 1.000000e-002;
		}

	if( ( (*e).etype==50 ) )
		{
		dimension = ( (*e).dim_value*angular_convert[( (*e).ang_units+1 )-1] );
		}
	else
		{
		conv_fact = um_length_conversion(4);
		dimension = (((*e).dim_value*conv_fact )*linear_convert
								[((*e).linear_units+1 )-1] );
		}
	start = ( (*e).txt_blk_use+1 );
	factor = 1.000000e+000;
	if( ( (*e).dim_places>0 ) )
		{
			{
			int		us_t162;
			us_t162 = (*e).dim_places;
			i = 1;
			for(;;)
				{
				if( i > us_t162 ) 	break;
				factor = ( factor/1.000000e+001 );
us_l160:
				i++ ;
				}
us_l161: 
			;
			}
		}
	arch = UU_FALSE;
	fraction = UU_FALSE;
	inches = UU_FALSE;
	switch( (*e).tol_method )
		{
		case 0:
			{
			switch( (*e).etype )
				{
				case 50:
					{
					ua_degree_text(&((*e)),dimension,dtext);
					}
					break;
				default:
					{
					if( ( (*e).linear_units==1 ) )
						{
						ua_english_text(&((*e)),dimension,factor,dtext);
						}
					else if( ( (*e).linear_units==2 ) )
						{
						arch = UU_TRUE;
						ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
							dentext,&(inches),&(fraction));
						}
					else
						{
						ua_value_to_string(dimension,(*e).dim_places,(*e).
						    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
							UA_dec_symbol,dtext);
						}
					}
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			}
			break;
		case 4:
		case 3:
		case 1:
		case 2:
			{
			uplimit = ( dimension+(*e).upper_tol );
			lolimit = ( dimension-(*e).lower_tol );
			if( ( (*e).etype!=50 ) )
				{
				ua_value_to_string(uplimit,(*e).dim_places,(*e).dim_zero_sup
				    ,(*e).dim_roundoff,(*e).dim_rnd_fact,UA_dec_symbol,uptext);
				ua_value_to_string(lolimit,(*e).dim_places,(*e).dim_zero_sup
				    ,(*e).dim_roundoff,(*e).dim_rnd_fact,UA_dec_symbol,lotext);
				}
			else
				{
				ua_degree_limit_text(&((*e)),dimension,(*e).upper_tol,(*e).
				    lower_tol,uptext,lotext);
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			switch( (*e).tol_method )
				{
				case 1:
					{
					strcpy(dtext,lotext);
					strcat(dtext," - ");
					strcat(dtext,uptext);
					}
					break;
				case 2:
					{
					strcpy(dtext,uptext);
					strcat(dtext,"\n");
					strcat(dtext,lotext);
					}
					break;
				case 3:
					{
					strcpy(dtext,uptext);
					strcat(dtext," - ");
					strcat(dtext,lotext);
					}
					break;
				case 4:
					{
					strcpy(dtext,lotext);
					strcat(dtext,"\n");
					strcat(dtext,uptext);
					}
					break;
				}
			}
			break;
		case 5:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
						dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,(*e).
					    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t163[1025];
					strcpy(toltext,"\\+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t163);
					strcat(toltext,us_t163);
					}
				}
			else
				{
					{
					char		us_t164[1025];
					strcpy(toltext,"\\+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t164);
					strcat(toltext,us_t164);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			}
			break;
		case 6:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
						dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,(*e).
					    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t165[1025];
					char		us_t166[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t165);
					strcat(toltext,us_t165);
					strcat(toltext,"\n-");
					ua_value_to_string((*e).lower_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t166);
					strcat(toltext,us_t166);
					}
				}
			else
				{
					{
					char		us_t167[1025];
					char		us_t168[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t167);
					strcat(toltext,us_t167);
					strcat(toltext,"\n-");
					ua_degree_tol_text(&((*e)),(*e).lower_tol,us_t168);
					strcat(toltext,us_t168);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size =
							( (*e).sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
			}
			break;
		case 7:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
						dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,(*e).
					    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t169[1025];
					char		us_t170[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t169);
					strcat(toltext,us_t169);
					strcat(toltext,"\n-");
					ua_value_to_string(0.000000e+000,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t170);
					strcat(toltext,us_t170);
					}
				}
			else
				{
					{
					char		us_t171[1025];
					char		us_t172[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t171);
					strcat(toltext,us_t171);
					strcat(toltext,"\n-");
					ua_degree_tol_text(&((*e)),0.000000e+000,us_t172);
					strcat(toltext,us_t172);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size =
							( (*e).sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
			}
			break;
		case 8:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
								dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,
							(*e).dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
							UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t173[1025];
					char		us_t174[1025];
					strcpy(toltext,"+");
					ua_value_to_string(0.000000e+000,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t173);
					strcat(toltext,us_t173);
					strcat(toltext,"\n-");
					ua_value_to_string((*e).lower_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t174);
					strcat(toltext,us_t174);
					}
				}
			else
				{
					{
					char		us_t175[1025];
					char		us_t176[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),0.000000e+000,us_t175);
					strcat(toltext,us_t175);
					strcat(toltext,"\n-");
					ua_degree_tol_text(&((*e)),(*e).lower_tol,us_t176);
					strcat(toltext,us_t176);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size =
					( (*e).sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
			}
			break;
		case 9:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
							dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,(*e).
					    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t177[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t177);
					strcat(toltext,us_t177);
					}
				}
			else
				{
					{
					char		us_t178[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t178);
					strcat(toltext,us_t178);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = (*e).char_size;
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
			}
			break;
		case 10:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
							dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,
						(*e).dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t179[1025];
					strcpy(toltext,"-");
					ua_value_to_string((*e).lower_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t179);
					strcat(toltext,us_t179);
					}
				}
			else
				{
					{
					char		us_t180[1025];
					strcpy(toltext,"-");
					ua_degree_tol_text(&((*e)),(*e).lower_tol,us_t180);
					strcat(toltext,us_t180);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = (*e).char_size;
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
			}
			break;
		}
	(*e).txt_blk[start-1].char_cnt = strlen(dtext);
	strcpy((*e).txt_blk[start-1].tstring,dtext);
	(*e).txt_blk[start-1].subtype = main_txt1;
		{
		int		us_t183;
		us_t183 = (*e).txt_blk_use;
		i = start;
		for(;;)
			{
			if( i > us_t183 ) 	break;
			(*e).txt_blk[i-1].txt_just = 2;
us_l181:
			i++ ;
			}
us_l182: 
		;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_degree_text(e, dimension, dtext)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_degree_text(e, dimension, dtext)
struct UA_generic_draft	(*e);
UU_REAL	dimension;
char		dtext[51];
	{
	UU_REAL	degrees;
	char		deg_text[21];
	UU_REAL	seconds;
	char		sec_text[21];
	char		min_text[21];
	UU_REAL	minutes;

	uu_denter(UU_STRC,(us,"ua_degree_text(dtext=%s)",dtext));

	if(dimension < 0.0174532 && e->draft_stand != 0)
		{
		e->ang_units = 2;
		}
	switch( (*e).ang_units )
		{
		case 0:
			{
			ua_value_to_string(dimension,(*e).dim_places,(*e).
			    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
				UA_dec_symbol,dtext);
			strcpy(dtext,dtext);
			strcat(dtext,"\\d");
			}
			break;
		case 1:
			{
			degrees = ( (UU_REAL)ua_trunc(dimension) );
			minutes = ( ( dimension-degrees )*6.000000e+001 );
			ua_rtos(degrees,0,deg_text);
			ua_rtos(minutes,0,min_text);
			if( ( strcmp( min_text, "60" ) == 0 ) )
				{
				strcpy(min_text,"0");
				degrees = ( degrees+1.000000e+000 );
				ua_rtos(degrees,0,deg_text);
				}
			strcpy(dtext,deg_text);
			strcat(dtext,"\\d");
			strcat(dtext,min_text);
			strcat(dtext,"\\m");
			}
			break;
		case 2:
			{
			degrees = ( (UU_REAL)ua_trunc(dimension) );
			minutes = ( (UU_REAL)ua_trunc(( ( dimension-degrees )*
			    6.000000e+001 )) );
			seconds = ( ( ( dimension-degrees )-( minutes/6.000000e+001
			    ) )*3.600000e+003 );
			ua_rtos(degrees,0,deg_text);
			ua_rtos(minutes,0,min_text);
			ua_rtos(seconds,0,sec_text);
			if( ( strcmp( sec_text, "60" ) == 0 ) )
				{
				strcpy(sec_text,"0");
				minutes = ( minutes+1.000000e+000 );
				ua_rtos(minutes,0,min_text);
				}
			if( ( strcmp( min_text, "60" ) == 0 ) )
				{
				strcpy(min_text,"0");
				degrees = ( degrees+1.000000e+000 );
				ua_rtos(degrees,0,deg_text);
				}
			strcpy(dtext,deg_text);
			strcat(dtext,"\\d");
			strcat(dtext,min_text);
			strcat(dtext,"\\m");
			strcat(dtext,sec_text);
			strcat(dtext,"\\s");
			}
			break;
		case 3:
			{
			ua_value_to_string(dimension,(*e).dim_places,(*e).
			    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
				UA_dec_symbol,dtext);
			strcpy(dtext,dtext);
			strcat(dtext,"rad");
			}
			break;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_get_upvector(entity, rot_angle, us_r123)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_get_upvector(entity, rot_angle, us_r123)
struct UA_generic_draft	(*entity);
UU_REAL	rot_angle;
UU_REAL	us_r123[3];
	{
	UU_REAL	cpxaxis[3];
	UU_REAL	cpyaxis[3];
	UU_REAL	cpzaxis[3];
	UU_REAL	cporig[3];
	UU_REAL	rotmat[4][3];

	uu_denter(UU_STRC,(us,"SAL ua_get_upvector(rot_angle=%g)",rot_angle));

	ua_getcpln(&((*entity)),cporig,cpxaxis,cpyaxis,cpzaxis);
	um_rottf(cpzaxis,rot_angle,rotmat);
	um_cctmtf(cpyaxis,rotmat,cpyaxis);
	um_vctovc(cpyaxis,us_r123);
	uu_dexit;
	return;
	}
/*********************************************************************
**    E_FUNCTION     : ua_dual_degree_tol_text(e, dimension, us_r132)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_dual_degree_tol_text(e, dimension, us_r132)
struct UA_generic_draft	(*e);
UU_REAL	dimension;
char		us_r132[2];
	{
	UU_REAL	degrees;
	char		deg_text[21];
	UU_REAL	seconds;
	char		sec_text[21];
	char		min_text[21];
	UU_REAL	minutes;
	char		dtext[51];

	uu_denter(UU_STRC,(us,"ua_dual_degree_tol_text(dimension=%g)",dimension));

	switch( (*e).dual_a_units )
		{
		case 0:
			{
			ua_value_to_string(dimension,(*e).du_tol_pl,(*e).d_tol_z_sup
			    ,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,UA_d_dec_symbol,
				dtext);
			}
			break;
		case 1:
			{
			ua_rtos(dimension,0,deg_text);
			strcpy(dtext,deg_text);
			strcat(dtext,"\\m");
			}
			break;
		case 2:
			{
			ua_rtos(dimension,0,deg_text);
			strcpy(dtext,deg_text);
			strcat(dtext,"\\s");
			}
			break;
		case 3:
			{
			ua_value_to_string(dimension,(*e).du_tol_pl,(*e).d_tol_z_sup
			    ,(*e).d_tol_roff_meth,(*e).d_tol_rnd_fact,UA_d_dec_symbol,
				dtext);
			}
			break;
		}
	strcpy(us_r132,dtext);
	uu_dexit;
	return;
	}
/*********************************************************************
**    E_FUNCTION     : ua_set_rep_dim_text(e,n)
**       Convert a real value to the dimension text string(s) given
**			current attribute settings for a dimension.
**    PARAMETERS   
**       INPUT  : 
**				e - dimension object_key to get the attribute setting from.
**       OUTPUT :  
**				e - dimension object_key with dim text strings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_rep_dim_text(e,n)
struct UA_generic_draft	(*e);
int *n;
	{

	uu_denter(UU_STRC,(us,"SAL ua_set_rep_dim_text(e=%s)", "..."));

	ua_set_rep_mdim_text(&((*e)),&(*n));
	ua_set_ddim_text(&((*e)));
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_set_rep_mdim_text(e,n)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_set_rep_mdim_text(e,n)
struct UA_generic_draft	(*e);
int *n;
	{
	char		fractext[51];
	UU_REAL	lolimit;
	UU_REAL	lolimitn;
	char		dentext[51];
	UU_LOGICAL	inches;
	UU_REAL	factor;
	UU_REAL	uplimit;
	UU_REAL	uplimitn;
	static UU_REAL	angular_convert[4];
	char		toltext[51];
	char		numtext[51];
	UU_REAL	dimension, dim_eq;
	int		i;
	static UU_REAL	linear_convert[6];
	char		lotext[51];
	UU_LOGICAL	arch;
	UU_REAL	conv_fact;
	char		uptext[51];
	char		uptextn[51];
	char		lotextn[51];
	char		dtextn[20];
	char		dimtext[20];
	char		dtextln[20];
	char		ldimtext[20];
	char		dtext[51];
	UU_LOGICAL	fraction;
	int		start;
	UU_REAL um_length_conversion();

	uu_denter(UU_STRC,(us,"SAL ua_set_rep_mdim_text(e=%s)", "..."));

	if( us_i130 )
		{
		us_i130 = UU_FALSE;
		angular_convert[0] = 5.729578e+001;
		angular_convert[1] = 5.729578e+001;
		angular_convert[2] = 5.729578e+001;
		angular_convert[3] = 1.000000e+000;
		linear_convert[0] = 3.937008e-001;
		linear_convert[1] = 3.937008e-001;
		linear_convert[2] = 3.937008e-001;
		linear_convert[3] = 1.000000e+001;
		linear_convert[4] = 1.000000e+000;
		linear_convert[5] = 1.000000e-002;
		}

	if( ( (*e).etype==50 ) )
		{
		dimension = ( (*e).dim_value*angular_convert[( (*e).ang_units+1 )-1] );
		dim_eq = ( (((*e).dim_value)/(*n))*angular_convert[( (*e).ang_units+1 )-1] );
		}
	else
		{
		conv_fact = um_length_conversion(4);
		dimension = ((((*e).dim_value*conv_fact )*linear_convert
								[((*e).linear_units+1 )-1])/(*n) );
		}
	start = ( (*e).txt_blk_use+1 );
	factor = 1.000000e+000;
	if( ( (*e).dim_places>0 ) )
		{
			{
			int		us_t162;
			us_t162 = (*e).dim_places;
			i = 1;
			for(;;)
				{
				if( i > us_t162 ) 	break;
				factor = ( factor/1.000000e+001 );
us_l160:
				i++ ;
				}
us_l161: 
			;
			}
		}
	arch = UU_FALSE;
	fraction = UU_FALSE;
	inches = UU_FALSE;
	switch( (*e).tol_method )
		{
		case 0:
			{
			switch( (*e).etype )
				{
				case 50:
					{
					ua_degree_text(&((*e)),dimension,dimtext);
					ua_degree_text(&((*e)),dim_eq,dtextn);
					sprintf(dtext,"%dX %s(=%s)",*n,dtextn,dimtext);
					}
					break;
				default:
					{
					if( ( (*e).linear_units==1 ) )
						{
						ua_english_text(&((*e)),dimension,factor,dtext);
						}
					else if( ( (*e).linear_units==2 ) )
						{
						arch = UU_TRUE;
						ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
							dentext,&(inches),&(fraction));
						}
					else
						{
						ua_value_to_string(dimension,((*e).dim_places+1),(*e).
						    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
							UA_dec_symbol,dtextn);
						ua_value_to_string((dimension*(*n)),(*e).dim_places,(*e).
						    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
							UA_dec_symbol,dimtext);
						sprintf(dtext,"%dX %s(=%s)",(*n),dtextn,dimtext);
						}
					}
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			}
			break;
		case 4:
		case 3:
		case 1:
		case 2:
			{
			uplimit = ( dimension+(*e).upper_tol );
			lolimit = ( dimension-(*e).lower_tol );
			uplimitn = (( dimension*(*n))+(*e).upper_tol );
			lolimitn = (( dimension*(*n))-(*e).lower_tol );
			if( ( (*e).etype!=50 ) )
				{
				ua_value_to_string(uplimit,((*e).dim_places+1),(*e).
				        dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
					UA_dec_symbol,dtextn);
				ua_value_to_string(lolimit,(*e).dim_places+1,(*e).dim_zero_sup
				    ,(*e).dim_roundoff,(*e).dim_rnd_fact,UA_dec_symbol,dtextln);
				ua_value_to_string(uplimitn,(*e).dim_places,(*e).
							    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
								UA_dec_symbol,dimtext);
				sprintf(uptext,"%dX %s(=%s)",(*n),dtextn,dimtext);
				ua_value_to_string(lolimitn,(*e).dim_places,(*e).
							    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
								UA_dec_symbol,ldimtext);
				sprintf(lotext,"%dX %s(=%s)",(*n),dtextln,ldimtext);
				}
			else
				{
				ua_degree_limit_text(&((*e)),dimension,(*e).upper_tol,(*e).
				    lower_tol,uptext,lotext);
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			switch( (*e).tol_method )
				{
				case 1:
					{
					strcpy(dtext,lotext);
					strcat(dtext," - ");
					strcat(dtext,uptext);
					}
					break;
				case 2:
					{
					strcpy(dtext,uptext);
					strcat(dtext,"\n");
					strcat(dtext,lotext);
					}
					break;
				case 3:
					{
					strcpy(dtext,uptext);
					strcat(dtext," - ");
					strcat(dtext,lotext);
					}
					break;
				case 4:
					{
					strcpy(dtext,lotext);
					strcat(dtext,"\n");
					strcat(dtext,uptext);
					}
					break;
				}
			}
			break;
		case 5:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
						dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,(*e).
					    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t163[1025];
					strcpy(toltext,"\\+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t163);
					strcat(toltext,us_t163);
					}
				}
			else
				{
					{
					char		us_t164[1025];
					strcpy(toltext,"\\+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t164);
					strcat(toltext,us_t164);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			}
			break;
		case 6:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
						dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,(*e).
					    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t165[1025];
					char		us_t166[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t165);
					strcat(toltext,us_t165);
					strcat(toltext,"\n-");
					ua_value_to_string((*e).lower_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t166);
					strcat(toltext,us_t166);
					}
				}
			else
				{
					{
					char		us_t167[1025];
					char		us_t168[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t167);
					strcat(toltext,us_t167);
					strcat(toltext,"\n-");
					ua_degree_tol_text(&((*e)),(*e).lower_tol,us_t168);
					strcat(toltext,us_t168);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size =
							( (*e).sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
			}
			break;
		case 7:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
						dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,(*e).
					    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t169[1025];
					char		us_t170[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t169);
					strcat(toltext,us_t169);
					strcat(toltext,"\n-");
					ua_value_to_string(0.000000e+000,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t170);
					strcat(toltext,us_t170);
					}
				}
			else
				{
					{
					char		us_t171[1025];
					char		us_t172[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t171);
					strcat(toltext,us_t171);
					strcat(toltext,"\n-");
					ua_degree_tol_text(&((*e)),0.000000e+000,us_t172);
					strcat(toltext,us_t172);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size =
							( (*e).sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
			}
			break;
		case 8:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
								dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,
							(*e).dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
							UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t173[1025];
					char		us_t174[1025];
					strcpy(toltext,"+");
					ua_value_to_string(0.000000e+000,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t173);
					strcat(toltext,us_t173);
					strcat(toltext,"\n-");
					ua_value_to_string((*e).lower_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t174);
					strcat(toltext,us_t174);
					}
				}
			else
				{
					{
					char		us_t175[1025];
					char		us_t176[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),0.000000e+000,us_t175);
					strcat(toltext,us_t175);
					strcat(toltext,"\n-");
					ua_degree_tol_text(&((*e)),(*e).lower_tol,us_t176);
					strcat(toltext,us_t176);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size =
					( (*e).sub_sup_ratio*(*e).char_size );
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
			}
			break;
		case 9:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
							dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,(*e).
					    dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t177[1025];
					strcpy(toltext,"+");
					ua_value_to_string((*e).upper_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t177);
					strcat(toltext,us_t177);
					}
				}
			else
				{
					{
					char		us_t178[1025];
					strcpy(toltext,"+");
					ua_degree_tol_text(&((*e)),(*e).upper_tol,us_t178);
					strcat(toltext,us_t178);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = (*e).char_size;
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
			}
			break;
		case 10:
			{
			if( ( (*e).etype!=50 ) )
				{
				if( ( (*e).linear_units==1 ) )
					{
					ua_english_text(&((*e)),dimension,factor,dtext);
					}
				else if( ( (*e).linear_units==2 ) )
					{
					arch = UU_TRUE;
					ua_arch_text(&((*e)),dimension,factor,UU_TRUE,dtext,numtext,
							dentext,&(inches),&(fraction));
					}
				else
					{
					ua_value_to_string(dimension,(*e).dim_places,
						(*e).dim_zero_sup,(*e).dim_roundoff,(*e).dim_rnd_fact,
						UA_dec_symbol,dtext);
					}
				}
			else
				{
				ua_degree_text(&((*e)),dimension,dtext);
				}
			if( ( arch==UU_TRUE ) )
				{
				if( ( fraction==UU_TRUE ) )
					{
					ua_frac_blk(&((*e)),numtext,dentext,UU_TRUE);
					}
				else
					{
					(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
					}
				}
			else
				{
				(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
				}
			if( ( (*e).etype!=50 ) )
				{
					{
					char		us_t179[1025];
					strcpy(toltext,"-");
					ua_value_to_string((*e).lower_tol,(*e).tol_places,(*e).
					    tol_zero_sup,(*e).tol_roundoff,(*e).tol_rnd_fact,
						UA_dec_symbol,us_t179);
					strcat(toltext,us_t179);
					}
				}
			else
				{
					{
					char		us_t180[1025];
					strcpy(toltext,"-");
					ua_degree_tol_text(&((*e)),(*e).lower_tol,us_t180);
					strcat(toltext,us_t180);
					}
				}
			(*e).txt_blk_use = ( (*e).txt_blk_use+1 );
			(*e).txt_blk[(*e).txt_blk_use-1].char_cnt = strlen(toltext);
			strcpy((*e).txt_blk[(*e).txt_blk_use-1].tstring,toltext);
			(*e).txt_blk[(*e).txt_blk_use-1].subtype = tol_txt1;
			(*e).txt_blk[(*e).txt_blk_use-1].txt_size = (*e).char_size;
			(*e).txt_blk[(*e).txt_blk_use-1].sub_super = (*e).sub_sup_ratio;
			}
			break;
		}
	(*e).txt_blk[start-1].char_cnt = strlen(dtext);
	strcpy((*e).txt_blk[start-1].tstring,dtext);
	(*e).txt_blk[start-1].subtype = main_txt1;
		{
		int		us_t183;
		us_t183 = (*e).txt_blk_use;
		i = start;
		for(;;)
			{
			if( i > us_t183 ) 	break;
			(*e).txt_blk[i-1].txt_just = 2;
us_l181:
			i++ ;
			}
us_l182: 
		;
		}
	uu_dexit;
	}
