/*********************************************************************
**    NAME         : adebug.c
**       CONTAINS:
**     		  ua_dump_ent
**     		  ua_dump_set
**     		 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       adebug.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:32
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) adebug.c 3.2 3/23/88 16:54:00 single"};
#else
static char uu_sccsident[]={"@(#) adebug.c 3.2 3/23/88 16:54:00 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"
#include "dasnog.h"

extern int /* bit */ UA_dump_choice_set;

/*********************************************************************
**    E_FUNCTION     : ua_dump_ent(unit_num, entity)
**       Writes out out all the data in an entity record.
**    PARAMETERS   
**       INPUT  : 
**          unit_num	-	unit number to write to
**				entity	-	entity record
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_dump_ent(unit_num, entity)
int		unit_num;
struct UA_generic_draft	(*entity);
	{
	int		i;
	int		j;

	uu_denter(UU_STRC,(us,"ua_dump_ent(unit_num=%d, entity=%s)",unit_num,"..."));
	if( ( ((UA_dump_choice_set >> (1 - 1)) & 1)==0x1 ) )
		{
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"key = %d, rel_num = %d\n",(*entity).key,(*entity).rel_num);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"entity types = %d, subtype = %d\n",
				(*entity).etype,(*entity).subtype);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"text placement = %d, entity site = %d\n",
				(*entity).txt_place,(*entity).entity_site);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"text entry method = %d\n",(*entity).txt_entry);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"appended text = %d, text orientation = %d\n",
				(*entity).appn_text,(*entity).txt_orent);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"text justificatin = %d, stacking grid = %d\n",
				(*entity).txt_just,(*entity).stack_grid);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us, "units symbol = %d, dual units symbol = %d\n",
				(*entity) .units_sym,(*entity).d_units_sym);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
				"dim decimal places = %d, tolerance decimal places = %d\n",
				(*entity).dim_places,(*entity).tol_places);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
				"dual dim decimal places = %d, dual tol decimal places = %d\n",
				(*entity).dual_place,(*entity).du_tol_pl);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
				"diameter sym placement = %d,  radius sym placement = %d\n",
				(*entity).dia_place,(*entity).rad_place);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"diameter symbol = %d,    radius symbol = %d\n",
				(*entity).diam_symbol,(*entity).rad_symb);
				us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us, "linear units = %d, dual linear units = %d\n",
				(*entity).linear_units,(*entity).dual_l_units);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us, "angular dim units = %d, dual dim angular units = %d\n",
				(*entity).ang_units,(*entity).dual_a_units);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us, "fractional units = %d, dual dim fractional units = %d\n",
				(*entity).fract_units,(*entity).dual_f_units);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us, "dual dimension format = %d, dimension types = %d\n",
				(*entity).dual_format,(*entity).dim_type);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"dim zero supp = %d, dual dim zero supp = %d\n",
				(*entity).dim_zero_sup,(*entity).d_dim_z_sup);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"tol zero supp = %d,      dual tol zero supp = %d\n",(*
			    entity).tol_zero_sup,(*entity).d_tol_z_sup);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"dimension roundoff = %d, tolerance roundoff = %d\n",(*
			    entity).dim_roundoff,(*entity).tol_roundoff);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"dual dim roundoff = %d, dual tol roundoff = %d\n",(*entity)
			    .d_dim_roundoff,(*entity).d_tol_roff_meth);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"tolerancing method = %d, tolerance site = %d\n",
			(*entity).tol_method,(*entity).tol_site);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"ext line suppression = %d, leader line suppression = %d\n",
			(*entity).ext_line_sup,(*entity).lead_orient);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"leader location = %d,    arrow placement = %d\n"
			    ,(*entity).leader_loc,(*entity).arrow_place);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"drafting standard = %d,  dimension display = %d\n",(*entity
			    ).draft_stand,(*entity).dims_display);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"grid distance = %f,      character slant = %f\n"
			    ,(*entity).grid_dist,(*entity).char_slant);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"character size = %f,     sub/superscript ratio = %f\n",(*
			    entity).char_size,(*entity).sub_sup_ratio);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"dim rounding factor = %f, tol rounding factor = %f\n",(*
			    entity).dim_rnd_fact,(*entity).tol_rnd_fact);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"dual dim rounding fact = %f, dual tol rounding fact = %f\n"
			    ,(*entity).d_dim_rnd_fact,(*entity).d_tol_rnd_fact);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"upper tolerance value = %f, lower tolerance value = %f\n",
			(*entity).upper_tol,(*entity).lower_tol);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"dual upper tol value = %f, dual lower tol value = %f\n",(*
			    entity).d_lin_up_tol,(*entity).d_lin_lo_tol);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"gap to geometry = %f,    extension past dim line = %f\n",(*
			    entity).gap_to_geo,(*entity).ext_past_line);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"oblique ext line angle = %f, gap from text to line = %f\n",
			(*entity).oblique_angle,(*entity).txt_gap);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"leader stub length = %f, arrowhead size = %f\n",
			(*entity).stub_length,(*entity).arrow_size);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"main dim value = %f,     secondary dim value = %f\n",(*
			    entity).dim_value,(*entity).dim2_value);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"dim origin (x,y,z) = (%f,%f,%f)\n",(*entity).
			    dim_origin[0],(*entity).dim_origin[1],(*entity).dim_origin
			    [2]);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,
			"cross hatch: pattern: %d, angle %f, spacing %f\n",(*entity)
			    .xh_pattern,(*entity).xh_angle,(*entity).xh_spacing);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"cpln origin (x,y,z) = (%f,%f,%f)\n",(*entity).
			    cpln.cpln_origin[0],(*entity).cpln.cpln_origin[1],(*entity).
			    cpln.cpln_origin[2]);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"cpln xaxis vector (x,y,z) = (%f,%f,%f)\n",(*
			    entity).cpln.xaxis[0],(*entity).cpln.xaxis[1],(*entity).cpln
			    .xaxis[2]);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"cpln yaxis vector (x,y,z) = (%f,%f,%f)\n",(*
			    entity).cpln.yaxis[0],(*entity).cpln.yaxis[1],(*entity).cpln
			    .yaxis[2]);
			us_write(unit_num,us);
			}
#endif
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"cpln zaxis vector (x,y,z) = (%f,%f,%f)\n",(*
			    entity).cpln.zaxis[0],(*entity).cpln.zaxis[1],(*entity).cpln
			    .zaxis[2]);
			us_write(unit_num,us);
			}
#endif
		}
	if( ( ((UA_dump_choice_set >> (6 - 1)) & 1)==0x1 ) )
		{
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"text blocks used = %d\n",(*entity).txt_blk_use);
			us_write(unit_num,us);
			}
#endif
			{
			int		us_t57;
			us_t57 = (*entity).txt_blk_use;
			i = 1;
			for(;;)
				{
				if( i > us_t57 ) 	break;
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\t***** TEXT BLOCK NUMBER %d\n",i);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tText block types = %d\n",(*entity).txt_blk[i
					    -1].subtype);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"  Line font=%d, density=%f, color=%d\n",(*entity
					    ).txt_blk[i-1].text.line_font,(*entity).txt_blk[i-1].text.
					    line_density,(*entity).txt_blk[i-1].text.color);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tCharacter count = %d\n",(*entity).txt_blk[i-1]
					    .char_cnt);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tText justify = %d\n",(*entity).txt_blk[i-1].
					    txt_just);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tFont name = %s\n",(*entity).txt_blk[i-1].
					    fontname);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tcharacter string = %s\n",(*entity).txt_blk[i
					    -1].tstring);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\torigin = <%f, %f, %f>\n",(*entity).txt_blk[i
					    -1].origin[0],(*entity).txt_blk[i-1].origin[1],(*entity).
					    txt_blk[i-1].origin[2]);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tdx = %f and dy = %f\n",(*entity).txt_blk[i-1].
					    dx,(*entity).txt_blk[i-1].dy);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tChar slant = %f Text angle = %f\n",(*entity).
					    txt_blk[i-1].slant,(*entity).txt_blk[i-1].tangle);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tText size = %f Sub/super ratio = %f\n",(*
					    entity).txt_blk[i-1].txt_size,(*entity).txt_blk[i-1].
					    sub_super);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"  Char expansion=%g, spacing=%g\n",(*entity).
					    txt_blk[i-1].char_expansion,(*entity).txt_blk[i-1].
					    char_space);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"  Line spacing=%g\n",(*entity).txt_blk[i-1].
					    line_spacing);
					us_write(unit_num,us);
					}
#endif
us_l55:
				i++ ;
				}
us_l56: 
			;
			}
		}
	if( ( ((UA_dump_choice_set >> (2 - 1)) & 1)==0x1 ) )
		{
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"arc blocks used = %d\n",(*entity).arc_blk_use);
			us_write(unit_num,us);
			}
#endif
			{
			int		us_t60;
			us_t60 = (*entity).arc_blk_use;
			i = 1;
			for(;;)
				{
				if( i > us_t60 ) 	break;
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\t***** ARC BLOCK NUMBER %d\n",i);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tArc block types = %d\n",
						(*entity).arc_blk[i-1].subtype);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"  Line font=%d, density=%f, color=%d\n",(*entity
					    ).arc_blk[i-1].arc.line_font,(*entity).arc_blk[i-1].arc.
					    line_density,(*entity).arc_blk[i-1].arc.color);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tNumber of points = %d\n",
						(*entity).arc_blk[i-1].num_pts);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tCenter point = <%f, %f, %f>\n",(*entity).
					    arc_blk[i-1].center_pt[0],(*entity).arc_blk[i-1].center_pt
					    [1],(*entity).arc_blk[i-1].center_pt[2]);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tRadius = %f\n",(*entity).arc_blk[i-1].radius);
					us_write(unit_num,us);
					}
#endif
					{
					int		us_t63;
					us_t63 = (*entity).arc_blk[i-1].num_pts;
					j = 1;
					for(;;)
						{
						if( j > us_t63 ) 	break;
#if UU_DEBUG
							{
							char us[1024];
							sprintf(us,"\t\tangle # %d = %f\n",
								j,(*entity).arc_blk[i-1].angles[j-1]);
							us_write(unit_num,us);
							}
#endif
us_l61:
						j++ ;
						}
us_l62: 
					;
					}
us_l58:
				i++ ;
				}
us_l59: 
			;
			}
		}
	if( ( ((UA_dump_choice_set >> (5 - 1)) & 1)==0x1 ) )
		{
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"line blocks used = %d\n",(*entity).line_blk_use);
			us_write(unit_num,us);
			}
#endif
			{
			int		us_t66;
			us_t66 = (*entity).line_blk_use;
			i = 1;
			for(;;)
				{
				if( i > us_t66 ) 	break;
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\t***** LINE BLOCK NUMBER %d\n",i);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tLine block types = %d\n",
						(*entity).line_blk[i-1].subtype);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"  Line font=%d, density=%f, color=%d\n",
						(*entity).line_blk[i-1].line.line_font,
						(*entity).line_blk[i-1].line.line_density,
						(*entity).line_blk[i-1].line.color);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tNumber of points = %d\n",
						(*entity).line_blk[i-1].num_pts);
					us_write(unit_num,us);
					}
#endif
					{
					int		us_t69;
					us_t69 = (*entity).line_blk[i-1].num_pts;
					j = 1;
					for(;;)
						{
						if( j > us_t69 ) 	break;
#if UU_DEBUG
							{
							char us[1024];
							sprintf(us,"\t\tpoint # %d = <%f, %f, %f>\n",j,(*entity).
							    line_blk[i-1].line_seg[j-1][0],(*entity).line_blk[i-1].
							    line_seg[j-1][1],(*entity).line_blk[i-1].line_seg[j-1][2]);
							us_write(unit_num,us);
							}
#endif
us_l67:
						j++ ;
						}
us_l68: 
					;
					}
us_l64:
				i++ ;
				}
us_l65: 
			;
			}
		}
	if( ( ((UA_dump_choice_set >> (4 - 1)) & 1)==0x1 ) )
		{
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"arrow blocks used = %d\n",(*entity).arrow_blk_use);
			us_write(unit_num,us);
			}
#endif
			{
			int		us_t72;
			us_t72 = (*entity).arrow_blk_use;
			i = 1;
			for(;;)
				{
				if( i > us_t72 ) 	break;
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\t***** ARROW BLOCK NUMBER %d\n",i);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tarrow types = %d\n",
						(*entity).arrow_blk[i-1].arrow_type);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"  Line font=%d, density=%f, color=%d\n",(*entity
					    ).arrow_blk[i-1].arrow.line_font,(*entity).arrow_blk[i-1].
					    arrow.line_density,(*entity).arrow_blk[i-1].arrow.color);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tarrow head location = <%f, %f, %f>\n",
						(*entity).arrow_blk[i-1].location[0],
						(*entity).arrow_blk[i-1].location[1],
						(*entity).arrow_blk[i-1].location[2]);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,
					"\tarrowhead angle = %f and arrowhead size = %f\n",
					(*entity).arrow_blk[i-1].aangle,(*entity).arrow_blk[i-1].size);
					us_write(unit_num,us);
					}
#endif
us_l70:
				i++ ;
				}
us_l71: 
			;
			}
		}
	if( ( ((UA_dump_choice_set >> (3 - 1)) & 1)==0x1 ) )
		{
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"associtivity blocks used = %d\n",(*entity).asso_blk_use);
			us_write(unit_num,us);
			}
#endif
			{
			int		us_t75;
			us_t75 = (*entity).asso_blk_use;
			i = 1;
			for(;;)
				{
				if( i > us_t75 ) 	break;
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\t***** ASSOCITIVITY BLOCK NUMBER %d\n",i);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"associtivity types= %d, associtivity modifier= %d\n",
						(*entity).asso_blk[i-1].asso_type,
						(*entity).asso_blk[i-1].modifier);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"associated entity key = %d\n",
						(*entity).asso_blk[i-1].key);
					us_write(unit_num,us);
					}
#endif
#if UU_DEBUG
					{
					char us[1024];
					sprintf(us,"\tassocitivity location = <%f, %f, %f>\n",
						(*entity).asso_blk[i-1].location[0],
						(*entity).asso_blk[i-1].location[1],
						(*entity).asso_blk[i-1].location[2]);
					us_write(unit_num,us);
					}
#endif
us_l73:
				i++ ;
				}
us_l74: 
			;
			}
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_dump_set(dump_choice)
**       Selects if and which data is dumped by ua_dump_ent.
**    PARAMETERS   
**       INPUT  : 
**          BIT: dump_choice  - Choice of if and which data is dumped
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_dump_set(dump_choice)
unsigned	dump_choice;
	{

	uu_denter(UU_STRC,(us,"ua_dump_set(dump_choice=%x)", dump_choice));
	if( ( dump_choice==0x0 ) )
		{
		UA_dump_choice_set = 0x0;
		}
	UA_dump_choice_set = ( UA_dump_choice_set|dump_choice );
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : uau_dump_draft()
**       Prompt the user for a drafting entity to dump. 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uau_dump_draft()
	{
	struct UA_PLOCREC	pick_rec;
	struct UA_PICKENT	pent;
	int		size;
	int		key;
	int		number_entered;
	struct UA_generic_draft	entity;
	int		status;

	uu_denter(UU_STRC,(us,"uau_dump_draft()"));
	status = ud_pick_loc(13,28,&(pick_rec),1,&(number_entered),
	UU_FALSE);
	if( ( status!=0 ) )
		{
		status = um_d_pickresolve(&(pick_rec.ppick),1,&(pent));
		key = um_get_pickkey(&(pent),1);
#if UU_DEBUG
			{
			char us[1024];
			sprintf(us,"dump key = %d",key);
			us_write(1,us);
			}
#endif
		size = 22500;
		entity.key = key;
		status = uc_retrieve_data(&(entity),size);
		ua_dump_ent(1,&(entity));
		}
	uu_dexit;
	}
