/*********************************************************************
**    NAME         : axhatch2.c
**       CONTAINS:
**			support routines for axhatch.sal
**
**				ua_xh_set_def()		-- sets up initial attribute defaults
**    		ua_save_xh_def()
**    		ua_restore_xh_def()
**    		ua_stuff_xh_def()
**    		ua_check_xh_def()
**    		ua_is_ch_regen_required()
**    		ua_xhatch_compare()	--	used to sort list
**				ua_xhatch_mergeilist()-	merge two sorted lists
**				ua_xhatch_init()		--	initializes xhatch definition lists
**				ua_print_list()		--	debugging dump of LIST of UA_ilnode
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       axhatch2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:42
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) axhatch2.c 3.4 4/4/88 15:18:16 single"};
#else
static char uu_sccsident[]={"@(#) axhatch2.c 3.4 4/4/88 15:18:16 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "axhatch.h"
#include "mattr.h"
#include "mdattr.h"
#include "ulist.h"

extern UU_LIST    xh_table;   /* table of xhatch pattern data */
extern UA_xh_attr_rec   UA_xh_defaults;   /* xhatch default attributes */

/*********************************************************************
**    E_FUNCTION     : ua_xh_set_def()
**				initializes record UA_xh_defaults - called once
**				per DD1 session
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_xh_set_def()
	{
	uu_denter(UU_STRC,(us,"ua_xh_set_def()"));

	UA_xh_defaults.material[1-1] = 1;
	UA_xh_defaults.xh_angle = 7.853982e-001;
	UA_xh_defaults.xh_scale = 2.500000e-001;
	UA_xh_defaults.color[1-1] = 1;
	UA_xh_defaults.linestyle[1-1] = 0;
	UA_xh_defaults.pen = 1;

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_save_xh_def()
**				Save xhatch defaults
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_save_xh_def(save_xh_int, save_xh_real)
	int save_xh_int[4];
	UU_REAL save_xh_real[2];
	{
	uu_denter(UU_STRC,(us,"ua_save_xh_def()"));

	save_xh_real[0] = UA_xh_defaults.xh_angle;
	save_xh_real[1] = UA_xh_defaults.xh_scale;
	save_xh_int[0] = UA_xh_defaults.material[0];
	save_xh_int[1] = UA_xh_defaults.color[0];
	save_xh_int[2] = UA_xh_defaults.linestyle[0] ;
	save_xh_int[3] = UA_xh_defaults.pen;

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_restore_xh_def()
**				Restore xhatch defaults
**				per DD1 session
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_restore_xh_def(save_xh_int, save_xh_real)
	int save_xh_int[4];
	UU_REAL save_xh_real[2];
	{
	uu_denter(UU_STRC,(us,"ua_save_xh_def()"));

	UA_xh_defaults.xh_angle = save_xh_real[0];
	UA_xh_defaults.xh_scale = save_xh_real[1];
	UA_xh_defaults.material[0] = save_xh_int[0];
	UA_xh_defaults.color[0] = save_xh_int[1];
	UA_xh_defaults.linestyle[0]  = save_xh_int[2];
	UA_xh_defaults.pen = save_xh_int[3];

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_stuff_xh_def()
**				Restore xhatch defaults
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_stuff_xh_def(edrf, attr)
	struct UA_generic_draft *edrf;
	struct UM_attrdata_rec *attr;
	{
	UU_REAL scale;
	uu_denter(UU_STRC,(us,"ua_stuff_xh_def()"));

	um_get_drwscale(&scale);
	UA_xh_defaults.xh_angle = edrf->xh_angle;
	UA_xh_defaults.xh_scale = edrf->xh_spacing*scale;
	UA_xh_defaults.material[0] = edrf->xh_pattern;
	UA_xh_defaults.color[0] = attr->color;
	UA_xh_defaults.pen = attr->pen;
	switch(attr->line_style)
		{
		case 1:
			UA_xh_defaults.linestyle[0]  = 0;
			break;
		case 6:
			UA_xh_defaults.linestyle[0]  = 1;
			break;
		case 7:
			UA_xh_defaults.linestyle[0]  = 2;
			break;
		case 5:
			UA_xh_defaults.linestyle[0]  = 3;
			break;
		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_check_xh_def()
**				Check xhatch defaults
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ua_check_xh_def(edrf, attr)
	struct UA_generic_draft *edrf;
	struct UM_attrdata_rec *attr;
	{
	UU_REAL scale;
	UU_LOGICAL status;

	uu_denter(UU_STRC,(us,"ua_check_xh_def()"));

	status = UU_FALSE;
	um_get_drwscale(&scale);
	if(UA_xh_defaults.xh_angle != edrf->xh_angle)
		{
		status = UU_TRUE;
		}
	else
		{
		if(UA_xh_defaults.xh_scale != edrf->xh_spacing*scale) 
			{
			status = UU_TRUE;
			}
		else
			{
			if(UA_xh_defaults.material[0] != edrf->xh_pattern)
				{
				status = UU_TRUE; 
				}
			else
				{
				if(UA_xh_defaults.color[0] != attr->color)
					{
					status = UU_TRUE;
					}
				else
					{
					if(UA_xh_defaults.pen != attr->pen)
						{
						status = UU_TRUE;
						}
					else
						{
						switch(attr->line_style)
							{
							case 1:
								if(UA_xh_defaults.linestyle[0]  != 0) status = UU_TRUE;
								break;
							case 6:
								if(UA_xh_defaults.linestyle[0]  != 1) status = UU_TRUE;
								break;
							case 7:
								if(UA_xh_defaults.linestyle[0]  != 2) status = UU_TRUE;
								break;
							case 5:
								if(UA_xh_defaults.linestyle[0]  != 3) status = UU_TRUE;
							break;
							}
						}
					}
				}
			}
		}

fexit:
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : ua_is_xh_regen_required()
**				Is regeneration of xhatch required?
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ua_is_xh_regen_required(edrf)
	struct UA_generic_draft *edrf;
	{
	UU_REAL scale;
	UU_LOGICAL status;

	uu_denter(UU_STRC,(us,"ua_is_xh_regen_required()"));

	status = UU_FALSE;
	um_get_drwscale(&scale);
	if(UA_xh_defaults.xh_angle != edrf->xh_angle)
		{
		status = UU_TRUE;
		uu_dprint(UU_STRC,(us,"xhatch angle def=%g, edrf=%g", UA_xh_defaults.xh_angle, edrf->xh_angle));
		}
	else
		{
		if(UA_xh_defaults.xh_scale != edrf->xh_spacing*scale) 
			{
			status = UU_TRUE;
		uu_dprint(UU_STRC,(us,"xhatch scale def=%g, edrf=%g", UA_xh_defaults.xh_scale, edrf->xh_spacing*scale));
			}
		else
			{
			if(UA_xh_defaults.material[0] != edrf->xh_pattern)
		uu_dprint(UU_STRC,(us,"xhatch pattern def=%d, edrf=%d", UA_xh_defaults.material[0], edrf->xh_pattern));
				{
				status = UU_TRUE; 
				}
			}
		}

fexit:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ua_xhatch_mergeilist(il1, il2, outl)
**       merges sorted lists of UA_ilnode, subject to node.t0 value
**    PARAMETERS   
**       INPUT  : 
**          il1, il2		--		Lists of UA_ilnode, sorted by increasing .t0
**       OUTPUT :  
**          outl			--		will be sorted by t.  (ties in favor of il1)
**				(NOTE: may be same as il1 or il2)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_xhatch_mergeilist(il1, il2, outl)
UU_LIST	(*il1);
UU_LIST	(*il2);
UU_LIST	(*outl);
	{
	int		index1;
	int		index2;
	int		current_list;
	int		first_index;
	int		ix;
	UU_LIST	templ;
	UU_REAL	next_t;

	uu_denter(UU_STRC,(us,
		"ua_xhatch_mergeilist(il1=%d items, il2=%d items, outl=%d items)",
		UU_LIST_LENGTH(il1), UU_LIST_LENGTH(il2), UU_LIST_LENGTH(outl)));

	templ.data = UU_NULL;
	uu_list_init( &(templ), sizeof(struct UA_ilnode	)
	    , 0, 10 );
	if( ( UU_LIST_LENGTH(il1)==0 ) )
		{
		uu_list_free( &templ );
		uu_dexit;
		return;
		}
	if( ( UU_LIST_LENGTH(il2)==0 ) )
		{
		uu_list_free( &templ );
		uu_dexit;
		return;
		}
	if( ( ((struct UA_ilnode	(*)) ((il1)->data))[1-1].t0<=((
	struct UA_ilnode	(*)) ((il2)->data))[1-1].t0 ) )
		{
		next_t = ((struct UA_ilnode	(*)) ((il2)->data))[1-1].t0;
		current_list = 1;
		}
	else
		{
		next_t = ((struct UA_ilnode	(*)) ((il1)->data))[1-1].t0;
		current_list = 2;
		}
	index1 = 1;
	index2 = 1;
	first_index = 1;
	for(;;)
		{
		if( ( current_list==1 ) )
			{
			if( ( ((struct UA_ilnode	(*)) ((il1)->data))[index1-1].t0
			    <=next_t ) )
				{
				index1 = ( index1+1 );
				if( ( index1>UU_LIST_LENGTH(il1) ) )
					{
						{
						UU_LIST	us_t89;
						uu_list_init( &(us_t89), sizeof(struct UA_ilnode	)
						    , 0, 10 );
						uu_list_push_multiple(&(us_t89),(( index1-1 ) - first_index
						    + 1),&UU_LIST_ARRAY(il1)[(first_index-1)*((il1
						    )->item_size)]);
						uu_list_push_list(&(templ),&(us_t89));
						uu_list_free( &us_t89 );
						}
						{
						UU_LIST	us_t90;
						uu_list_init( &(us_t90), sizeof(struct UA_ilnode	)
						    , 0, 10 );
						uu_list_push_multiple(&(us_t90),(UU_LIST_LENGTH(il2) - 
						    index2 + 1),&UU_LIST_ARRAY(il2)[(index2-1)*((il2
						    )->item_size)]);
						uu_list_push_list(&(templ),&(us_t90));
						uu_list_free( &us_t90 );
						}
					goto Done;
					}
				}
			else
				{
					{
					UU_LIST	us_t91;
					uu_list_init( &(us_t91), sizeof(struct UA_ilnode	)
					    , 0, 10 );
					uu_list_push_multiple(&(us_t91),(( index1-1 ) - first_index
					    + 1),&UU_LIST_ARRAY(il1)[(first_index-1)*((il1
					    )->item_size)]);
					uu_list_push_list(&(templ),&(us_t91));
					uu_list_free( &us_t91 );
					}
				next_t = ((struct UA_ilnode	(*)) ((il1)->data))[index1-1].
				    t0;
				current_list = 2;
				first_index = index2;
				}
			}
		else
			{
			if( ( ((struct UA_ilnode	(*)) ((il2)->data))[index2-1].t0
			    <=next_t ) )
				{
				index2 = ( index2+1 );
				if( ( index2>UU_LIST_LENGTH(il2) ) )
					{
						{
						UU_LIST	us_t92;
						uu_list_init( &(us_t92), sizeof(struct UA_ilnode	)
						    , 0, 10 );
						uu_list_push_multiple(&(us_t92),(( index2-1 ) - first_index
						    + 1),&UU_LIST_ARRAY(il2)[(first_index-1)*((il2
						    )->item_size)]);
						uu_list_push_list(&(templ),&(us_t92));
						uu_list_free( &us_t92 );
						}
						{
						UU_LIST	us_t93;
						uu_list_init( &(us_t93), sizeof(struct UA_ilnode	)
						    , 0, 10 );
						uu_list_push_multiple(&(us_t93),(UU_LIST_LENGTH(il1) - 
						    index1 + 1),&UU_LIST_ARRAY(il1)[(index1-1)*((il1
						    )->item_size)]);
						uu_list_push_list(&(templ),&(us_t93));
						uu_list_free( &us_t93 );
						}
					goto Done;
					}
				}
			else
				{
					{
					UU_LIST	us_t94;
					uu_list_init( &(us_t94), sizeof(struct UA_ilnode	)
					    , 0, 10 );
					uu_list_push_multiple(&(us_t94),(( index2-1 ) - first_index
					    + 1),&UU_LIST_ARRAY(il2)[(first_index-1)*((il2
					    )->item_size)]);
					uu_list_push_list(&(templ),&(us_t94));
					uu_list_free( &us_t94 );
					}
				next_t = ((struct UA_ilnode	(*)) ((il2)->data))[index2-1].
				    t0;
				current_list = 1;
				first_index = index1;
				}
			}
		}
us_l88: 
	;
Done:
	uu_list_init( outl, sizeof(struct UA_ilnode	), 0, 10 );
		{
		int		us_t97;
		us_t97 = UU_LIST_LENGTH(&(templ));
		ix = 1;
		for(;;)
			{
			if( ix > us_t97 ) 	break;
			uu_list_push(outl,&(((struct UA_ilnode	(*)) ((&(templ)
			    )->data))[ix-1]));
us_l95:
			ix++ ;
			}
us_l96: 
		;
		}
	uu_list_free( &templ );
	uu_dexit;
	return;
	}
/*********************************************************************
**    E_FUNCTION     : ua_print_list(ilist)
**       debug dump of intersection list
**    PARAMETERS   
**       INPUT  : 
**          LIST of UA_ilnode: ilist
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_print_list(ilist)
UU_LIST	(*ilist);
	{
	int		i;
	struct UA_ilnode	l;

	uu_denter(UU_STRC,(us,"ua_print_list(ilist=%d items)", 
		UU_LIST_LENGTH(ilist)));

#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"\nList-- length %d",UU_LIST_LENGTH(ilist));
		us_write(4,us);
		}
#endif
		{
		int		us_t100;
		us_t100 = UU_LIST_LENGTH(ilist);
		i = 1;
		for(;;)
			{
			if( i > us_t100 ) 	break;
#if UU_DEBUG
				{
				char us[1024];
				sprintf(us,"\tline id; %d, \tline parameter: %g, \tevent %d"
				    ,((struct UA_ilnode	(*)) ((ilist)->data))[i-1].line_id,((
				struct UA_ilnode	(*)) ((ilist)->data))[i-1].t0,((struct 
				    UA_ilnode	(*)) ((ilist)->data))[i-1].event);
				us_write(4,us);
				}
#endif
us_l98:
			i++ ;
			}
us_l99: 
		;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_xhatch_init()
**       initializes some lists for pattern definitions
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_xhatch_init()
	{
	struct UA_lfdata	lf;
	struct UA_xhpattern	xhpat;

	uu_denter(UU_STRC,(us,"ua_xhatch_init()"));

	uu_list_init( &(xh_table), sizeof(struct UA_xhpattern	)
	    , 0, 10 );
	xhpat.xh_id = 0;
	uu_list_init( &(xhpat.xh_lf), sizeof(struct UA_lfdata	)
	    , 0, 10 );
	lf.lf_angle = 0.000000e+000;
	lf.lf_xorg = 0.000000e+000;
	lf.lf_yorg = 0.000000e+000;
	lf.lf_offset = 0.000000e+000;
	lf.lf_space = 1.000000e+000;
	lf.lf_shift = 0.000000e+000;
	uu_list_init( &(lf.lf_dashes), sizeof(UU_REAL), 0, 10 );

	uu_list_push(&(xhpat.xh_lf),&(lf));
	uu_list_push(&(xh_table),&(xhpat));
	xhpat.xh_id = 1;
	uu_list_init( &(xhpat.xh_lf), sizeof(struct UA_lfdata	)
	    , 0, 10 );
	lf.lf_angle = 0.000000e+000;
	lf.lf_xorg = 0.000000e+000;
	lf.lf_yorg = 0.000000e+000;
	lf.lf_offset = 0.000000e+000;
	lf.lf_space = 1.000000e+000;
	lf.lf_shift = 0.000000e+000;
	uu_list_init( &(lf.lf_dashes), sizeof(UU_REAL), 0, 10 );

	uu_list_push(&(xhpat.xh_lf),&(lf));
	uu_list_push(&(xh_table),&(xhpat));
	xhpat.xh_id = 2;
	uu_list_init( &(xhpat.xh_lf), sizeof(struct UA_lfdata	), 0, 10 );
	lf.lf_angle = 0.000000e+000;
	lf.lf_xorg = 0.000000e+000;
	lf.lf_yorg = 0.000000e+000;
	lf.lf_offset = 0.000000e+000;
	lf.lf_shift = 0.000000e+000;
	lf.lf_space = 1.000000e+000;
	uu_list_init( &(lf.lf_dashes), sizeof(UU_REAL), 0, 10 );
	
	uu_list_push(&(xhpat.xh_lf),&(lf));
	lf.lf_offset = 2.500000e-001;
	uu_list_push(&(xhpat.xh_lf),&(lf));
	uu_list_push(&(xh_table),&(xhpat));
	xhpat.xh_id = 3;
	uu_list_init( &(xhpat.xh_lf), sizeof(struct UA_lfdata	), 0, 10 );
	lf.lf_angle = 0.000000e+000;
	lf.lf_xorg = 0.000000e+000;
	lf.lf_yorg = 0.000000e+000;
	lf.lf_offset = 0.000000e+000;
	lf.lf_space = 1.000000e+000;
	lf.lf_shift = 0.000000e+000;
	uu_list_init( &(lf.lf_dashes), sizeof(UU_REAL), 0, 10 );
		{
		UU_REAL	us_t101;
		us_t101 = 5.000000e-001;
		uu_list_push(&(lf.lf_dashes),&(us_t101));
		}
		{
		UU_REAL	us_t102;
		us_t102 = -5.000000e-001;
		uu_list_push(&(lf.lf_dashes),&(us_t102));
		}
	uu_list_push(&(xhpat.xh_lf),&(lf));
	lf.lf_offset = 5.000000e-001;
	uu_list_init( &(lf.lf_dashes), sizeof(UU_REAL), 0, 10 );
	uu_list_push(&(xhpat.xh_lf),&(lf));
	uu_list_push(&(xh_table),&(xhpat));
	xhpat.xh_id = 4;
	uu_list_init( &(xhpat.xh_lf), sizeof(struct UA_lfdata	)
	    , 0, 10 );
	lf.lf_angle = 0.000000e+000;
	lf.lf_xorg = 0.000000e+000;
	lf.lf_yorg = 0.000000e+000;
	lf.lf_offset = 0.000000e+000;
	lf.lf_space = 1.000000e+000;
	lf.lf_shift = 0.000000e+000;
	uu_list_init( &(lf.lf_dashes), sizeof(UU_REAL), 0, 10 );

	uu_list_push(&(xhpat.xh_lf),&(lf));
	lf.lf_offset = 1.500000e-001;
	uu_list_push(&(xhpat.xh_lf),&(lf));
	lf.lf_offset = 3.000000e-001;
	uu_list_push(&(xhpat.xh_lf),&(lf));
	lf.lf_offset = 4.500000e-001;
	uu_list_push(&(xhpat.xh_lf),&(lf));
	uu_list_push(&(xh_table),&(xhpat));
	xhpat.xh_id = 5;
	uu_list_init( &(xhpat.xh_lf), sizeof(struct UA_lfdata	), 0, 10 );

	lf.lf_angle = 0.000000e+000;
	lf.lf_xorg = 0.000000e+000;
	lf.lf_yorg = 0.000000e+000;
	lf.lf_offset = 0.000000e+000;
	lf.lf_space = 1.000000e+000;
	lf.lf_shift = 0.000000e+000;
	uu_list_init( &(lf.lf_dashes), sizeof(UU_REAL), 0, 10 );

	uu_list_push(&(xhpat.xh_lf),&(lf));
	lf.lf_angle = 1.570796e+000;
	uu_list_push(&(xhpat.xh_lf),&(lf));
	uu_list_push(&(xh_table),&(xhpat));
	xhpat.xh_id = 6;
	uu_list_init( &(xhpat.xh_lf), sizeof(struct UA_lfdata	) , 0, 10 );

	lf.lf_angle = 0.000000e+000;
	lf.lf_xorg = 0.000000e+000;
	lf.lf_yorg = 0.000000e+000;
	lf.lf_offset = 0.000000e+000;
	lf.lf_space = 1.000000e+000;
	lf.lf_shift = 0.000000e+000;
	uu_list_init( &(lf.lf_dashes), sizeof(UU_REAL), 0, 10 );
	
	uu_list_push(&(xhpat.xh_lf),&(lf));
	lf.lf_angle = 1.570796e+000;
	lf.lf_offset = 7.500000e-001;
		{
		UU_REAL	us_t103;
		us_t103 = 5.000000e-001;
		uu_list_push(&(lf.lf_dashes),&(us_t103));
		}
		{
		UU_REAL	us_t104;
		us_t104 = -5.000000e-001;
		uu_list_push(&(lf.lf_dashes),&(us_t104));
		}
	uu_list_push(&(xhpat.xh_lf),&(lf));
	uu_list_push(&(xh_table),&(xhpat));
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : int		ua_xhatch_compare(first, second)
**       comparator for two UA_ilnode's
**    PARAMETERS   
**       INPUT  : 
**          first, second	--	to UA_ilnodes (VAR)
**       OUTPUT :  
**          none
**    RETURNS      : >0 if first is greater, <0 if second is greater, 0 if equal
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_xhatch_compare(first, second)
struct UA_ilnode	(*first);
struct UA_ilnode	(*second);
	{
	uu_denter(UU_STRC,(us,"ua_xhatch_compare(first=%s, second=%s)",
		"...", "..."));

	if( ( (*first).line_id>(*second).line_id ) )
		{
		uu_dexit;
		return(1);
		}
	else if( ( (*first).line_id<(*second).line_id ) )
		{
		uu_dexit;
		return(-1);
		}
	else if( ( (*first).line_id==(*second).line_id ) )
		{
		if( ( (*first).t0>(*second).t0 ) )
			{
			uu_dexit;
			return(1);
			}
		else if( ( (*first).t0<(*second).t0 ) )
			{
			uu_dexit;
			return(-1);
			}
		else if( ( (*first).t0==(*second).t0 ) )
			{
			uu_dexit;
			return(0);
			}
		}
	}
