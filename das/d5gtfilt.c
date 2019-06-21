/*********************************************************************
**
**    NAME         :  d5gtfilt.c 
**
**       CONTAINS:
**				ud_getfilt() 
** 				ud_f_cont_layers
**				ud_f_spec_layer
**				ud_f_layer
**				ud_f_color
**				ud_f_linestyle
**				ud_f_entity
**				ud_f_pen
**    		mstate(filedno, val, stat)
**    		layerstate(filedno, val, stat)
**				ud_initflt()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d5gtfilt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:11
**
*********************************************************************/

/*#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) d5gtfilt.c 12.2 10/25/95 15:37:05 single"};
#else
static char uu_sccsident[]={"@(#) d5gtfilt.c 12.2 10/25/95 15:37:05 double"};
#endif
*/

#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "dselect.h"
#include "zsysdep.h"

#define DOT    1
#define PLUS   2
#define STAR   3
#define CIRCLE 4
#define CROSS  5
#define TRIAN  6
#define DIMOND 7
#define SQUARE 8
#define DBLCIR 9
#define LRGDOT 10
#define CUBE   11

static UD_FSTAT mstate();
static UD_FSTAT layerstate();

/* raw filter data defines "setfilt.frm" defaults */
static UD_RAWFILTREC UD_rawfilter = 
	/*NCL: {2, 2, 2, 2, 2, 0, 1, 4096, 0, 0, 0, 1, 256}; */
/*	{2, 2, 2, 2, 2, 0, 1, 4096, 1, 0, 0, 1, 256}; */
	{2, 2, 2, 2, 2, 0, 2, 1, 9999, 1, 0, 0, 1, 256, 0};
											
/* array for the set filter form fields */
static UD_RAWFILTREC temp_filter;
static int *ans[] = {(int *)&temp_filter.color_state, (int *)&temp_filter.colornum, 
	(int *)&temp_filter.linestyle_state, (int *)&temp_filter.linestyle, 
	(int *)&temp_filter.entity_state, (int *)&temp_filter.entnum, 
	(int *)&temp_filter.pen_state, (int *)&temp_filter.pen_min, 
	(int *)&temp_filter.pen_max, (int *)&temp_filter.layer_state, 
	(int *)&temp_filter.layer_min, (int *)&temp_filter.layer_max, 
	(int *)&temp_filter.cont_state,
	(int *)&temp_filter.marker_state, (int *)&temp_filter.marker_type};

/* Set layer defaults - for "layerfilt.frm */
static UD_layer_filter head_layer = {{2, 2, 2, 2, 2, 2, 2, 2},
	{1, 1, 1, 1, 1, 1, 1, 1},
/*	{4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096}, */
	{9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999},
	UU_FALSE,
	0};

/* answers from the layer filter form fields */
static UD_layer_filter scratch;
static int *answers[] = {(int *)&scratch.state[0], 
	(int *)&scratch.min[0], (int *)&scratch.max[0],
	(int *)&scratch.state[1], (int *)&scratch.min[1],
	(int *)&scratch.max[1], (int *)&scratch.state[2], 
	(int *)&scratch.min[2], (int *)&scratch.max[2],
	(int *)&scratch.state[3], (int *)&scratch.min[3],
	(int *)&scratch.max[3], (int *)&scratch.state[4],
	(int *)&scratch.min[4], (int *)&scratch.max[4],
	(int *)&scratch.state[5], (int *)&scratch.min[5],
	(int *)&scratch.max[5], (int *)&scratch.state[6],
	(int *)&scratch.min[6], (int *)&scratch.max[6],
	(int *)&scratch.state[7], (int *)&scratch.min[7],
	(int *)&scratch.max[7], (int *)&scratch.cont};

/*********************************************************************
**
**    E_FUNCTION:  ud_getfilt()
**  						puts up filter form - ud_form1, gets filter data and 
**              creates	the bit array filters - ud_f_*
**
**    PARAMETERS   
**       INPUT: none
**       OUTPUT: none
**
**    RETURNS: none
**    SIDE EFFECTS: changes entries in UD_filter & UD_rawfilter 
**    WARNINGS: none
**
*********************************************************************/
ud_getfilt()
{
	int status;
	/* User defined methods for all the Change/No Change fields */
	static UD_METHOD methods[] = {mstate, 0,
	                              mstate, 0,
	                              mstate, 0,
	                              mstate, 0, 0,
	                              mstate, 0, 0,
	                              0,
											mstate, 0};

	/* The 'called' array indicates when user-supplied methods are called.
	 * One entry in the array for each field of the form.
	 * Bitwise or of:
	 *		1 - Invoked on field entry.
	 *		2 - Invoked on field exit.
	 *		4 - Invoked on field toggle.
	 */
	static char called[] = {6, 6,
                          6, 6, 
                          6, 6, 
                          6, 6, 6,
                          6, 6, 6,
                          6,
								  6, 6}; 

	/* Form traverse mask - if 1 traverse, if 0 do not traverse.
	 * This will change in mstate as we traverse the form.
	 */
	static char traverse[] = {1, 0,
                            1, 0,
                            1, 0,
                            1, 0, 0,
                            1, 0, 0,
                            0,
									 1, 0};

uu_denter(UU_DTRC,(us,"entering ud_getfilt"));

/* copy permanent UD_rawfilter into temp_filt */
zbytecp(temp_filter, UD_rawfilter);

/*
.....Set traversal flags based on current states
.....Bobby  -  2/21/97
*/
	if (UD_rawfilter.color_state == 2) traverse[1] = 0;
	else traverse[1] = 1;
	if (UD_rawfilter.linestyle_state == 2) traverse[3] = 0;
	else traverse[3] = 1;
	if (UD_rawfilter.entity_state == 2) traverse[5] = 0;
	else traverse[5] = 1;
	if (UD_rawfilter.pen_state == 2)
	{
		traverse[7] = 0;
		traverse[8] = 0;
	}
	else
	{
		traverse[7] = 1;
		traverse[8] = 1;
	}
	if (UD_rawfilter.layer_state == 2)
	{
		traverse[10] = 0;
		traverse[11] = 0;
		traverse[12] = 0;
	}
	else
	{
		traverse[10] = 1;
		traverse[11] = 1;
		traverse[12] = 1;
	}
	if (UD_rawfilter.marker_state == 2) traverse[14] = 0;
	else traverse[14] = 1;

/* process form */
	status = ud_form1("setfilt.frm",ans,ans,methods,called,UU_NULL,traverse);
/*Yurong*/
	if (status==-1)
		return -1;

/* now copy temp_filter back to the permanent UD_rawfilter */
	zbytecp(UD_rawfilter, temp_filter);

/* process data in UD_rawfilter & put it in UD_filter */
	UD_filter.layer_state = UD_rawfilter.layer_state;
	UD_filter.color_state= UD_rawfilter.color_state;
	UD_filter.linestyle_state = UD_rawfilter.linestyle_state;
	UD_filter.entity_state = UD_rawfilter.entity_state;
	UD_filter.pen_state = UD_rawfilter.pen_state;
	UD_filter.cont_state = UD_rawfilter.cont_state;
	UD_filter.marker_state = UD_rawfilter.marker_state;

/* clear & reset bit arrays */
	ud_f_layer (UD_rawfilter.layer_min, UD_rawfilter.layer_max);
	ud_f_color (UD_rawfilter.colornum);
	ud_f_linestyle (UD_rawfilter.linestyle);
	ud_f_entity (UD_rawfilter.entnum);
	ud_f_pen (UD_rawfilter.pen_min, UD_rawfilter.pen_max);
	ud_f_marker (UD_rawfilter.marker_type);

/* Continue to enter layer numbers? */
	if (UD_filter.cont_state && UD_filter.layer_state != UD_INACTIVE)
		ud_f_cont_layers();

	uu_denter2(UU_DTRC,(us,"ud_getfilt: layer %x color %x line %x ent %x pen %x", UD_filter.f_layer,UD_filter.f_color,UD_filter.f_linestyle, UD_filter.f_entity,UD_filter.f_entity,UD_filter.f_pen));
	uu_dexit;
	uu_dexit;
	return (0);
}
/*********************************************************************
**    E_FUNCTION :   ud_f_cont_layers ()
**         To get here the user has set the layer field of the 
**         "SELECTION FILTER" to include or exclude and the continue
**         field to yes.
**         Continue to enter layer numbers to filter on.
**         Define a new set of form variables to bring up a new
**         form for the user to interact with.
**
**    PARAMETERS   
**       INPUT: none 
**       OUTPUT: none
**
**    RETURNS: none
**    SIDE EFFECTS: Modifies the global UD_filter - specifically the
**                   f_layer field.
**    WARNINGS: none
**
*********************************************************************/
ud_f_cont_layers()
{
int i, status;
UD_layer_filter *layers;
/* define the method to be used with this form */
static UD_METHOD layer_routine[] = {layerstate, layerstate, layerstate,
		layerstate, layerstate, layerstate,
		layerstate, layerstate, layerstate,
		layerstate, layerstate, layerstate, 
		layerstate, layerstate, layerstate, 
		layerstate, layerstate, layerstate,
		layerstate, layerstate, layerstate, 
		layerstate, layerstate, layerstate, 
		layerstate};

/* Set the form calling mask - 1 - Invoked on field entry.
 *                             2 - Invoked on field exit.
 *                             4 - Invoked on field toggle.
 */
static char called[] = {6, 6, 6, 6, 6, 6,
		6, 6, 6, 6, 6, 6,
		6, 6, 6, 6, 6, 6,
		6, 6, 6, 6, 6, 6, 6};

/* Set the from traverse mask - if 1 traverse */
/* Don't allow the user to run rampent thru the form */
static char layer_trav[] = {1, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0};

static UD_layer_filter default_rec = {{2, 2, 2, 2, 2, 2, 2, 2},
	{1, 1, 1, 1, 1, 1, 1, 1},
	{9999, 9999, 9999, 9999, 9999, 9999, 9999, 9999},
	UU_FALSE,
	0};

uu_denter(UU_DTRC,(us,"entering ud_f_cont_layers, state=%d",
          UD_filter.layer_state));

layers = &head_layer;
while (UD_filter.cont_state)
	{
	zbytecp(scratch, (*layers));
	status = ud_form1 ("layerfilt.frm",answers,answers,layer_routine,
			called,UU_NULL,layer_trav);
	if (status==-1)
		return -1;
	zbytecp((*layers), scratch);
	UD_filter.cont_state = layers->cont;

	/* call ud_f_spec_layer to set the layer filter mask */
	for (i = 0; i < 8; i++)
		ud_f_spec_layer(layers->state[i], layers->min[i], layers->max[i]);

	/* get the next form record */
	if (layers->cont) 
		{
		if (layers->link == 0) 
			{
			layers->link = (UD_layer_filter*) uu_toolmalloc(sizeof(UD_layer_filter));
			layers = layers->link;
			zbytecp((*layers), default_rec);
			}
		else layers = layers->link;
		}
	}

/* if (UD_filter.clear_state) then deallocate all memory move the default
 * into the head_layer
 */
uu_dexit;
return (0);
}

/*********************************************************************
**    E_FUNCTION :   ud_f_spec_layer (min, max)
**        Set the layer filter mask - set only those bits specified.
**            UD_INCLUDE - Include layers min thru max in the filter.
**            UD_EXCLUDE - Exclude layers min thru max from the filter.
**            UD_INACTIVE - Do not filter on the layer number.
**
**    PARAMETERS   
**       INPUT  :  min - minimun layer number
**                 max - maximum layer number
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_f_spec_layer(state, min, max)
int state,
		min,
		max;
{
int i;

uu_denter(UU_DTRC,(us,"entering ud_f_layer, state=%d",
          UD_filter.layer_state));
switch (state)
  {
  case UD_INCLUDE:
      for (i = min; i <= max; i++)
          uu_set_bit(UD_filter.f_layer, i);
      break;
  case UD_EXCLUDE:
	  	for (i = min; i <= max; i++) 
					uu_clr_bit(UD_filter.f_layer, i); 
	  	break;
  case UD_INACTIVE:
  default:
      break;
  }
uu_dexit;
return (0);
}
/*********************************************************************
**    E_FUNCTION :   ud_f_layer (min, max)
**        Set the layer filter mask.  If the user wishes to include
**        min thru max he/she is also excluding 1 thru min and max 
**        thru 9999 - and vice versa.
**            UD_INCLUDE - Include layers min thru max in the filter.
**            UD_EXCLUDE - Exclude layers min thru max from the filter.
**            UD_INACTIVE - Do not filter on the layer number.
**
**    PARAMETERS   
**       INPUT  :  min - minimun layer number
**                 max - maximum layer number
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_f_layer(min, max)
int min,
		max;
{
int i;

uu_denter(UU_DTRC,(us,"entering ud_f_layer, state=%d",
          UD_filter.layer_state));
switch (UD_filter.layer_state)
  {
  case UD_INCLUDE:
      for (i = 0; i < UD_NLAYER_WORDS; i++)
          UD_filter.f_layer[i] = 0;
      for (i = min; i <= max; i++)
          uu_set_bit(UD_filter.f_layer, i);
      break;
  case UD_EXCLUDE:
      for (i = 0; i < UD_NLAYER_WORDS; i++) 
					UD_filter.f_layer[i] = 0xFFFFFFFF; 
	  	for (i = min; i <= max; i++) 
					uu_clr_bit(UD_filter.f_layer, i); 
	  	break;
  case UD_INACTIVE:
  default:
      break;
  }
uu_dexit;
return(0);
}
/*********************************************************************
**    E_FUNCTION :   ud_f_color (color)
**         Set the color filter mask.  If a user include a color
**         he/she is also excluding all others - and vice versa.
**             UD_INCLUDE - Include 'color' in the filter.
**             UD_EXCLUDE - Exclude 'color' from the filter.
**             UD_INACTIVE - Do not filter on color.
**
**    PARAMETERS   
**       INPUT  : color - entity color 
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_f_color(color)
int color;
{
int i;

uu_denter(UU_DTRC,(us,"entering ud_f_color, state=%d",
					UD_filter.color_state));
switch (UD_filter.color_state)
{
case UD_INCLUDE:
      for (i=0; i < UD_NCOLOR_WORDS; i++)
          UD_filter.f_color[i] = 0;
      uu_set_bit(UD_filter.f_color,color);
      break;
  case UD_EXCLUDE:
      for (i=0; i < UD_NCOLOR_WORDS; i++)
          UD_filter.f_color[i] = 0xFFFFFFFF;
      uu_clr_bit(UD_filter.f_color,color);
  case UD_INACTIVE:
  default:
      break;
  }
	uu_dexit;
	return(0);
}
/*********************************************************************
**    E_FUNCTION :   ud_f_linestyle (linestyle)
**         Set the line style filter mask.  if the user includes a
**         linestyle he/she is also excluding all others - and vice
**         versa.
**              UD_INCLUDE -Include line style in the filter.
**              UD_EXCLUDE - Exclude line style from the filter.
**              UD_INACTIVE - Do not filter on line style.
**
**    PARAMETERS   
**       INPUT  :  linestyle - solid, dashed, phantom, etc. 
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_f_linestyle(linestyle)
int linestyle;
{
int i;

uu_denter(UU_DTRC,(us,"entering ud_f_linestyle, state=%d",
					UD_filter.linestyle_state));
switch (UD_filter.linestyle_state)
	{
  case UD_INCLUDE:
      for (i=0; i < UD_NLINESTYLE_WORDS; i++)
          UD_filter.f_linestyle[i] = 0;
      switch (linestyle)
        {
        case 0:
						uu_set_bit(UD_filter.f_linestyle,UM_SOLID_LINE - 1);
	    	    break;
        case 1:
						uu_set_bit(UD_filter.f_linestyle,UM_SMLDASH_LINE - 1);
            break;
        case 2:
						uu_set_bit(UD_filter.f_linestyle,UM_DOT_LINE - 1);
            break;
        case 3:
						uu_set_bit(UD_filter.f_linestyle,UM_CENTER_LINE - 1);
            break;
        case 4:
						uu_set_bit(UD_filter.f_linestyle,UM_PHANTOM_LINE - 1);
            break;
        case 5:
						uu_set_bit(UD_filter.f_linestyle,UM_DASHED_LINE - 1);
            break;
        case 6:
						uu_set_bit(UD_filter.f_linestyle,UM_DASHDOT_LINE - 1);
            break;
        case 7:
						uu_set_bit(UD_filter.f_linestyle,UM_DASHSPC_LINE - 1);
            break;
        }
				break;
  case UD_EXCLUDE:
      for (i=0; i < UD_NLINESTYLE_WORDS; i++)
          UD_filter.f_linestyle[i] = 0xFFFFFFFF;
      switch (UD_rawfilter.linestyle)
        {
        case 0:
            uu_clr_bit(UD_filter.f_linestyle,UM_SOLID_LINE - 1);
            break;
        case 1:
				uu_clr_bit(UD_filter.f_linestyle,UM_SMLDASH_LINE - 1);
            break;
        case 2:
				uu_clr_bit(UD_filter.f_linestyle,UM_DOT_LINE - 1);
            break;
        case 3:
            uu_clr_bit(UD_filter.f_linestyle,UM_CENTER_LINE - 1);
            break;
		  case 4:
            uu_clr_bit(UD_filter.f_linestyle,UM_PHANTOM_LINE - 1);
				break;
		  case 5:
            uu_clr_bit(UD_filter.f_linestyle,UM_DASHED_LINE - 1);
				break;
		  case 6:
				uu_clr_bit(UD_filter.f_linestyle,UM_DASHDOT_LINE - 1);
				break;
		  case 7:
				uu_clr_bit(UD_filter.f_linestyle,UM_DASHSPC_LINE - 1);
				break;
        }
				break;
  case UD_INACTIVE:
  default:
	break;
  }
uu_dexit;
return(0);
}
/*********************************************************************
**    E_FUNCTION :   ud_f_marker (marker)
**         Set the marker type filter mask.  if the user includes a
**         marker type he/she is also excluding all others - and vice
**         versa.
**              UD_INCLUDE -Include marker type in the filter.
**              UD_EXCLUDE - Exclude marker type from the filter.
**              UD_INACTIVE - Do not filter on marker type.
**
**    PARAMETERS   
**       INPUT  :  marker type - dot,plus,star, etc. 
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_f_marker(marker)
int marker;
{
int i;

uu_denter(UU_DTRC,(us,"entering ud_f_linestyle, state=%d",
					UD_filter.marker_state));
switch (UD_filter.marker_state)
	{
  case UD_INCLUDE:
      for (i=0; i < UD_NLINESTYLE_WORDS; i++)
          UD_filter.f_marker[i] = 0;
      switch (marker)
        {
        case 0:
				uu_set_bit(UD_filter.f_marker,DOT - 1);
	    	   break;
        case 1:
				uu_set_bit(UD_filter.f_marker,PLUS - 1);
            break;
        case 2:
				uu_set_bit(UD_filter.f_marker,STAR - 1);
            break;
        case 3:
				uu_set_bit(UD_filter.f_marker,CIRCLE - 1);
            break;
        case 4:
				uu_set_bit(UD_filter.f_marker,CROSS - 1);
            break;
        case 5:
				uu_set_bit(UD_filter.f_marker,TRIAN - 1);
            break;
        case 6:
				uu_set_bit(UD_filter.f_marker,DIMOND - 1);
            break;
        case 7:
				uu_set_bit(UD_filter.f_marker,SQUARE - 1);
            break;
        case 8:
				uu_set_bit(UD_filter.f_marker,DBLCIR - 1);
            break;
        case 9:
				uu_set_bit(UD_filter.f_marker,LRGDOT - 1);
            break;
        case 10:
				uu_set_bit(UD_filter.f_marker,CUBE - 1);
            break;
        }
		break;
  case UD_EXCLUDE:
      for (i=0; i < UD_NLINESTYLE_WORDS; i++)
          UD_filter.f_marker[i] = 0xFFFFFFFF;
      switch (UD_rawfilter.marker_type)
        {
        case 0:
				uu_clr_bit(UD_filter.f_marker,DOT - 1);
	    	   break;
        case 1:
				uu_clr_bit(UD_filter.f_marker,PLUS - 1);
            break;
        case 2:
				uu_clr_bit(UD_filter.f_marker,STAR - 1);
            break;
        case 3:
				uu_clr_bit(UD_filter.f_marker,CIRCLE - 1);
            break;
        case 4:
				uu_clr_bit(UD_filter.f_marker,CROSS - 1);
            break;
        case 5:
				uu_clr_bit(UD_filter.f_marker,TRIAN - 1);
            break;
        case 6:
				uu_clr_bit(UD_filter.f_marker,DIMOND - 1);
            break;
        case 7:
				uu_clr_bit(UD_filter.f_marker,SQUARE - 1);
            break;
        case 8:
				uu_clr_bit(UD_filter.f_marker,DBLCIR - 1);
            break;
        case 9:
				uu_clr_bit(UD_filter.f_marker,LRGDOT - 1);
            break;
        case 10:
				uu_clr_bit(UD_filter.f_marker,CUBE - 1);
            break;
        }
		break;
  case UD_INACTIVE:
  default:
	break;
  }
uu_dexit;
return(0);
}
/*********************************************************************
**    E_FUNCTION :   ud_f_entity (entity_num)
**         Set the entity filter mask.  If the user includes an 
**         entity then he/she is also excluding all other entities -
**         and vice versa.
**             UD_INCLUDE - Include the entity identified by 
**                entity_num in the filter.
**             UD_EXCLUDE - Exclude the entity identified by
**                entity_num from the filter.
**             UD_INACTIVE - Do not filter by entity type.
**
**    PARAMETERS   
**       INPUT  :  entity_num - an integer identifier 
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_f_entity(entity_num)
int entity_num;
{
int i;

uu_denter(UU_DTRC,(us,"entering ud_f_entity, state=%d",
					UD_filter.entity_state));
switch (UD_filter.entity_state )
	{
  case UD_INCLUDE:
      for (i=0;i < UD_NENTITY_WORDS; i++)
          UD_filter.f_entity[i] = 0;
      switch (entity_num)
        {
				case 0:  
					uu_set_bit(UD_filter.f_entity, NCL_POINT_REL - 1);
					uu_set_bit(UD_filter.f_entity, UM_POINT_REL - 1);
					break;
				case 1:  
					uu_set_bit(UD_filter.f_entity, NCL_LINE_REL - 1);
					uu_set_bit(UD_filter.f_entity, UM_LINE_REL - 1);
					break;
				case 2:  
					uu_set_bit(UD_filter.f_entity, NCL_CIRCLE_REL - 1);
					uu_set_bit(UD_filter.f_entity, UM_CIRCLE_REL - 1);
					break;
				case 3:  
					uu_set_bit(UD_filter.f_entity, NCL_PLN_REL - 1);
					break;
				case 4:  
					uu_set_bit(UD_filter.f_entity, NCL_VECTOR_REL - 1);
					break;
				case 5:  
					uu_set_bit(UD_filter.f_entity, NCL_POINTVEC_REL - 1);
					break;
				case 6:  
					uu_set_bit(UD_filter.f_entity, NCL_MATRIX_REL - 1);
					break;
				case 7:
					uu_set_bit(UD_filter.f_entity, NCL_CURVE_REL - 1);
					uu_set_bit(UD_filter.f_entity, NCL_EVALCV_REL - 1);
					uu_set_bit(UD_filter.f_entity, UM_CONIC_REL - 1);
					uu_set_bit(UD_filter.f_entity, UM_COMPCRV_REL - 1);
					uu_set_bit(UD_filter.f_entity, UM_AGCRV_REL - 1);
					uu_set_bit(UD_filter.f_entity, UM_RBSPLCRV_REL - 1);
					break;
				case 8:
   				uu_set_bit(UD_filter.f_entity, NCL_CURVE_REL - 1);
   				break;
				case 9:  
					uu_set_bit(UD_filter.f_entity, UM_COMPCRV_REL - 1);
					break;
				case 10:  
					uu_set_bit(UD_filter.f_entity, UM_AGCRV_REL - 1);
					uu_set_bit(UD_filter.f_entity, UM_RBSPLCRV_REL - 1);
					break;
				case 11:  
					uu_set_bit(UD_filter.f_entity, UM_UVCVONSF_REL - 1);
					break;
				case 12:  
					uu_set_bit(UD_filter.f_entity, UM_CONIC_REL - 1);
					break;
				case 13:	
					uu_set_bit(UD_filter.f_entity, NCL_PATERN_REL - 1);
					break;
			   case 14:
   				uu_set_bit(UD_filter.f_entity, NCL_SURF_REL - 1);
   				uu_set_bit(UD_filter.f_entity, NCL_EVALSF_REL - 1);
   				uu_set_bit(UD_filter.f_entity, UM_AGSRF_REL - 1);
   				uu_set_bit(UD_filter.f_entity, UM_RBSPLSRF_REL - 1);
					uu_set_bit(UD_filter.f_entity, NCL_MESHSURF_REL - 1);
					uu_set_bit(UD_filter.f_entity, NCL_QUILTSURF_REL - 1);
					uu_set_bit(UD_filter.f_entity, NCL_NETSF_REL - 1);
					uu_set_bit(UD_filter.f_entity, NCL_TRIMSF_REL - 1);
					uu_set_bit(UD_filter.f_entity, NCL_REVSURF_REL - 1);
   				break;	
				case 15:
					uu_set_bit(UD_filter.f_entity, NCL_SURF_REL - 1);
					break;
				case 16:
					uu_set_bit(UD_filter.f_entity, UM_RBSPLSRF_REL - 1);
					break;
				case 17:	
			 		uu_set_bit(UD_filter.f_entity, NCL_TRIMSF_REL - 1);
 					break;
				case 18:
					uu_set_bit(UD_filter.f_entity, NCL_REVSURF_REL - 1);
 					break;
				case 19:
					uu_set_bit(UD_filter.f_entity, NCL_NETSF_REL - 1);
 					break;
				case 20:	
					uu_set_bit(UD_filter.f_entity, NCL_MESHSURF_REL - 1);
					break;
				case 21:	
					uu_set_bit(UD_filter.f_entity, NCL_QUILTSURF_REL - 1);
					break;
				case 22:	
					uu_set_bit(UD_filter.f_entity, NCL_SHAPE_REL - 1);
					break;
				case 23:
					uu_set_bit(UD_filter.f_entity, UM_SOLID_REL - 1);
					break;
				case 24:
					uu_set_bit(UD_filter.f_entity, UA_TEXT_REL - 1);
					break;
				case 25:	
					for (i = UA_LINEAR_DIMS_REL; i <= UA_DRWBORDR_REL; i++)
					uu_set_bit(UD_filter.f_entity,i-1);
					break;
      }
			break;
  case UD_EXCLUDE:
      for (i=0;i < UD_NENTITY_WORDS; i++)
      UD_filter.f_entity[i] = 0xFFFFFFFF;
      switch (entity_num)
        {
				case 0:  
					uu_clr_bit(UD_filter.f_entity, NCL_POINT_REL - 1);
					uu_clr_bit(UD_filter.f_entity, UM_POINT_REL - 1);
					break;
				case 1:  
					uu_clr_bit(UD_filter.f_entity, NCL_LINE_REL - 1);
					uu_clr_bit(UD_filter.f_entity, UM_LINE_REL - 1);
					break;
				case 2:  
					uu_clr_bit(UD_filter.f_entity, NCL_CIRCLE_REL - 1);
					uu_clr_bit(UD_filter.f_entity, UM_CIRCLE_REL - 1);
					break;
				case 3:  
					uu_clr_bit(UD_filter.f_entity, NCL_PLN_REL - 1);
					break;
				case 4:  
					uu_clr_bit(UD_filter.f_entity, NCL_VECTOR_REL - 1);
					break;
				case 5:  
					uu_clr_bit(UD_filter.f_entity, NCL_POINTVEC_REL - 1);
					break;
				case 6:  
					uu_clr_bit(UD_filter.f_entity, NCL_MATRIX_REL - 1);
					break;
				case 7:
					uu_clr_bit(UD_filter.f_entity, NCL_CURVE_REL - 1);
					uu_clr_bit(UD_filter.f_entity, NCL_EVALCV_REL - 1);
					uu_clr_bit(UD_filter.f_entity, UM_CONIC_REL - 1);
					uu_clr_bit(UD_filter.f_entity, UM_COMPCRV_REL - 1);
					uu_clr_bit(UD_filter.f_entity, UM_AGCRV_REL - 1);
					uu_clr_bit(UD_filter.f_entity, UM_RBSPLCRV_REL - 1);
					break;
				case 8:
   				uu_clr_bit(UD_filter.f_entity, NCL_CURVE_REL - 1);
   				break;
				case 9:  
					uu_clr_bit(UD_filter.f_entity, UM_COMPCRV_REL - 1);
					break;
				case 10:  
					uu_clr_bit(UD_filter.f_entity, UM_AGCRV_REL - 1);
					uu_clr_bit(UD_filter.f_entity, UM_RBSPLCRV_REL - 1);
					break;
				case 11:
   				uu_clr_bit(UD_filter.f_entity, UM_UVCVONSF_REL - 1);
   				break;
				case 12:  
					uu_clr_bit(UD_filter.f_entity, UM_CONIC_REL - 1);
					break;
				case 13:	
					uu_clr_bit(UD_filter.f_entity, NCL_PATERN_REL - 1);
					break;
				case 14:
    				uu_clr_bit(UD_filter.f_entity, NCL_SURF_REL - 1);
    				uu_clr_bit(UD_filter.f_entity, NCL_EVALSF_REL - 1);
   	 			uu_clr_bit(UD_filter.f_entity, UM_AGSRF_REL - 1);
    				uu_clr_bit(UD_filter.f_entity, UM_RBSPLSRF_REL - 1);
    				uu_clr_bit(UD_filter.f_entity, NCL_MESHSURF_REL - 1);
    				uu_clr_bit(UD_filter.f_entity, NCL_QUILTSURF_REL - 1);
    				uu_clr_bit(UD_filter.f_entity, NCL_NETSF_REL - 1);
    				uu_clr_bit(UD_filter.f_entity, NCL_TRIMSF_REL - 1);
    				uu_clr_bit(UD_filter.f_entity, NCL_REVSURF_REL - 1);
    				break;
				case 15:
					uu_clr_bit(UD_filter.f_entity, NCL_SURF_REL - 1);
					break;
				case 16:
					uu_clr_bit(UD_filter.f_entity, UM_RBSPLSRF_REL - 1);
 					break;
				case 17:
					uu_clr_bit(UD_filter.f_entity, NCL_TRIMSF_REL - 1);
 					break;
				case 18:
					uu_clr_bit(UD_filter.f_entity, NCL_REVSURF_REL - 1);
 					break;
				case 19:
					uu_clr_bit(UD_filter.f_entity, NCL_NETSF_REL - 1);
 					break;
				case 20:	
					uu_clr_bit(UD_filter.f_entity, NCL_MESHSURF_REL - 1);
					break;
				case 21:	
					uu_clr_bit(UD_filter.f_entity, NCL_QUILTSURF_REL - 1);
					break;
				case 22:	
					uu_clr_bit(UD_filter.f_entity, NCL_SHAPE_REL - 1);
					break;
				case 23:
					uu_clr_bit(UD_filter.f_entity, UM_SOLID_REL - 1);
					break;
				case 24:
					uu_clr_bit(UD_filter.f_entity, UA_TEXT_REL - 1);
					break;
				case 25:	
					for (i = UA_LINEAR_DIMS_REL; i <= UA_DRWBORDR_REL; i++)
					uu_clr_bit(UD_filter.f_entity,i-1);
					break;
        }
        break;
  case UD_INACTIVE:
  default:
  break;
	}
uu_dexit;
return(0);
}

/*********************************************************************
**    E_FUNCTION :   ud_f_pen (min, max)
**       Set the pen filter mask.  If the user includes pens min thru 
**       max then he/she is also excluding pens 1 thru min and max 
**       thru 4096 - and vice versa.
**           UD_INCLUDE - Include pens min thru max in the filter.
**           UD_EXCLUDE - Exclude pens min thru max from the filter.
**           UD_INACTIVE - Do not filter by pen neumbers.
**
**    PARAMETERS   
**       INPUT  :  min - minimum pen number
**                 max - maximum pen number
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
ud_f_pen(min, max)
int min,
		max;
{
int i;

uu_denter(UU_DTRC,(us,"entering ud_f_pen, state=%d",
					UD_filter.pen_state));
switch (UD_filter.pen_state)
	{
  case UD_INCLUDE:
      for (i=0;i < UD_NPEN_WORDS; i++) UD_filter.f_pen[i] = 0;
      for (i = min; i <= max; i++)
          uu_set_bit(UD_filter.f_pen,i-1);
      break;
  case UD_EXCLUDE:
      for (i=0;i < UD_NPEN_WORDS; i++) UD_filter.f_pen[i] = 0xFFFFFFFF;
      for (i = min; i <= max; i++)
          uu_clr_bit(UD_filter.f_pen,i-1);
      break;
  case UD_INACTIVE:
  default:
      break;
	}
uu_dexit;
return(0);
}
/*********************************************************************
**    S_FUNCTION     :  mstate(filedno, val, stat)
**       Method called at each change/nochange toggle field
**			in the set filter form. (setfilt.frm)
**    PARAMETERS   
**       INPUT  : fieldno	Field number being changed.
**                val		Current field value.
**                stat		Field status.
**       OUTPUT :  none
**
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form traverse mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT mstate(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{

uu_denter(UU_MTRC,(us,"mstate(%d)", *fieldno));

/*
....Modified so that switch statement is navigated before the call to
....ud_default_method() (ud_default_method() increments *fieldno which
....would cause the case statements to be skipped) 
....AND, only when the field is not being TOGGLED.
....This corrects fields set to "include" or "exclude" not being navigated.
....
....Original Unicad comment:
....Switch traverse mask based on the field that changed.  This code
....sets the traverse mask so that input fields associated with
....change/nochange toggles are not traversed when the toggles value
....is nochange.
*/

/* Call the default method.  This changes the toggle display, and
 * causes the answer field to be updated.
 */
ud_default_method(fieldno, val, stat);
/*if ((stat != UD_TFWD) && (stat != UD_TBAK))*/
	{
	switch(*fieldno) {
		case 0:	/* Color */
			if( *ans[0] == 2 )
				ud_set_traverse_mask(1, UU_FALSE);
			else
				ud_set_traverse_mask(1, UU_TRUE);
				break;

		case 2:	/* Line style */
			if( *ans[2] == 2 )
				ud_set_traverse_mask(3, UU_FALSE);
			else
				ud_set_traverse_mask(3, UU_TRUE);
			break;

		case 4:	/* Entity */
			if( *ans[4] ==2 )
				ud_set_traverse_mask(5, UU_FALSE);
			else
				ud_set_traverse_mask(5, UU_TRUE);
			break;

		case 6:	/* Logical pen */
			if( *ans[6] ==2 )
				{
				ud_set_traverse_mask(7, UU_FALSE);
				ud_set_traverse_mask(8, UU_FALSE);
				}
			else
				{
				ud_set_traverse_mask(7, UU_TRUE);
				ud_set_traverse_mask(8, UU_TRUE);
				}
			break;

		case 9:	/* Layer number */
			if( *ans[9] == 2 ) 
				{
				ud_set_traverse_mask(10, UU_FALSE);
				ud_set_traverse_mask(11, UU_FALSE);
				}
			else
				{
				ud_set_traverse_mask(10, UU_TRUE);
				ud_set_traverse_mask(11, UU_TRUE);
				ud_set_traverse_mask(12, UU_TRUE);
				}
			break;

		case 13:	/* Marker */
			if( *ans[13] ==2 )
				ud_set_traverse_mask(14, UU_FALSE);
			else
				ud_set_traverse_mask(14, UU_TRUE);
			break;

		default:
			uu_dprint(UU_MTRC,(us,"mstate: Bad field number: %d",*fieldno));
		}
	}

uu_dexit;
return (stat);
}

/*********************************************************************
**    S_FUNCTION     :  layerstate(filedno, val, stat)
**       Method called at each change/nochange toggle field
**			 in the layer filter form. (layerfilter.frm)
**    PARAMETERS   
**       INPUT  : fieldno	Field number being changed.
**                val		Current field value.
**                stat		Field status.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form traverse mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT layerstate(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{

uu_denter(UU_MTRC,(us,"layerstate(%d)", *fieldno));


/*
....Modified to navigate switch statement first and only if not toggleing
....the inactive/include/exclude field.
*/
	ud_default_method(fieldno, val, stat);
/*	if ((stat != UD_TFWD) && (stat != UD_TBAK))*/
		{
		switch(*fieldno) {
			case 0:	/* state */
			case 3:
			case 6:
			case 9:
			case 12:
			case 15:
			case 18:
			case 21:
					if(*answers[*fieldno] == 2) 
						{
						ud_set_traverse_mask((*fieldno + 1), UU_FALSE);
						ud_set_traverse_mask((*fieldno + 2), UU_FALSE);
						}
					else
						{
						ud_set_traverse_mask((*fieldno + 1), UU_TRUE);
						ud_set_traverse_mask((*fieldno + 2), UU_TRUE);
						ud_set_traverse_mask((*fieldno + 3), UU_TRUE);
						}
					break;
			default:
					uu_dprint(UU_MTRC,(us,"layerstate: Bad field number: %d",*fieldno));
					break;
			}
		}

	uu_dexit;
	return (stat);
}
/*********************************************************************
**    E_FUNCTION :   ud_initflt()
**       Initialize raw filter.
**
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

ud_initflt()
{
   int i;
   for (i=0;i<6;i++)
   {
      UD_rawfilter.layer_state=0;
      UD_rawfilter.color_state=0;
      UD_rawfilter.linestyle_state=0;
      UD_rawfilter.entity_state=0;
      UD_rawfilter.pen_state=0;
	   UD_rawfilter.cont_state = 0;
		UD_rawfilter.marker_state = 0;
      UD_rawfilter.colornum=0;
      UD_rawfilter.linestyle=0;
      UD_rawfilter.entnum=0;
   }
   UD_rawfilter.layer_state= 2;
   UD_rawfilter.color_state=2;
   UD_rawfilter.linestyle_state=2;
   UD_rawfilter.entity_state=2;
   UD_rawfilter.pen_state=2;
   UD_rawfilter.marker_state=2;
   UD_rawfilter.layer_min=1;
   UD_rawfilter.layer_max=9999;
   UD_rawfilter.colornum=1;
   UD_rawfilter.pen_min=1;
   UD_rawfilter.pen_max=256;
	UD_rawfilter.marker_type=0;
return 0;
}
