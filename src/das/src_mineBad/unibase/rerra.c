/*********************************************************************
**    NAME         :  rerra.c
**       CONTAINS:
**       ur_retrieve_color()
**       ur_retrieve_layer()
**       ur_retrieve_pen()
**       ur_retrieve_line_style()
**       ur_retrieve_line_width()
**       ur_retrieve_line_weight()
**       ur_retrieve_displayable()
**       ur_retrieve_selectable()
**       ur_retrieve_blanked()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rerra.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:35
*********************************************************************/

#include	"usysdef.h"
#include	"rbase.h"
#include	"ribase.h"
#include	"rmtuple.h"
#include	"udebug.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_retrieve_color(key,&color)
**      retrieve color
**    PARAMETERS   
**       INPUT  : 
**        key,		key to to use in retrieving the color
**			color,	a pointer to where the color is to be put
**       OUTPUT :  
**			color,	the appropriate color
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_color(key,color)
UU_KEY_ID	 	key;		/* the key to update to */
int				*color;	/* pointer to the color */
{
	int				status;		/* status, -1 if error, 0 otherwise */
	UU_KEY_ID		attr_key;
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle */
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;

	uu_denter(UU_RTRC,(us,"ur_retrieve_color for key = %d",key));

	/* first determine that the master tuple is active and that there */
	/* is an attribute bundle */
	status = ur_retrieve_master_attr(key,UR_ATTR_INDX,&attr_key);
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_tuple_ptr(rel, tuple, &attr_ptr);
		if(status == 0)
			*color = attr_ptr->color;
	}
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION     :  status = ur_retrieve_layer(key,&layer)
**      retrieve layer
**    PARAMETERS   
**       INPUT  : 
**        key,		key to to use in retrieving the layer
**			layer,	a pointer to where the layer is to be put
**       OUTPUT :  
**			layer,	the appropriate layer
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_layer(key,layer)
UU_KEY_ID	key;		/* the key to update to */
int			*layer;	/* pointer to the layer */
{
	int				status;		/* status, -1 if error, 0 otherwise		*/
	UU_KEY_ID		attr_key;
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;

	uu_denter(UU_RTRC,(us,"ur_retrieve_layer for key = %d",key));

	/* first determine that the master tuple is active and that there */
	/* is an attribute bundle */
	status = ur_retrieve_master_attr(key,UR_ATTR_INDX,&attr_key);
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_tuple_ptr(rel, tuple, &attr_ptr);
		if(status == 0)
			*layer = attr_ptr->layer;
	}
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION     :  status = ur_retrieve_pen(key,&pen)
**      retrieve pen
**    PARAMETERS   
**       INPUT  : 
**        key,		key to to use in retrieving the pen
**			pen,	a pointer to where the pen is to be put
**       OUTPUT :  
**			pen,	the appropriate pen
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_pen(key,pen)
UU_KEY_ID 	key;	/* the key to update to */
int			*pen;	/* pointer to the pen */
{
	int				status;		/* status, -1 if error, 0 otherwise */
	UU_KEY_ID		attr_key;
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;

	uu_denter(UU_RTRC,(us,"ur_retrieve_pen for key = %d",key));

	/* first determine that the master tuple is active and that there */
	/* is an attribute bundle */
	status = ur_retrieve_master_attr(key,UR_ATTR_INDX,&attr_key);
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_tuple_ptr(rel, tuple, &attr_ptr);
		if(status == 0)
			*pen = attr_ptr->pen;
	}
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION     :  status = ur_retrieve_layer(key,&line_style)
**      retrieve line_style
**    PARAMETERS   
**       INPUT  : 
**        key,		key to to use in retrieving the line_style
**			line_style,	a pointer to where the line_style is to be put
**       OUTPUT :  
**			line_style,	the appropriate line_style
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_line_style(key,line_style)
UU_KEY_ID 	key;				/* the key to update to */
int			*line_style;	/* pointer to the line_style */
{
	int				status;		/* status, -1 if error, 0 otherwise */
	UU_KEY_ID		attr_key;
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;

	uu_denter(UU_RTRC,(us,"ur_retrieve_line_style for key = %d",key));

	/* first determine that the master tuple is active and that there */
	/* is an attribute bundle */
	status = ur_retrieve_master_attr(key,UR_ATTR_INDX,&attr_key);
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_tuple_ptr(rel, tuple, &attr_ptr);
		if(status == 0)
			*line_style = attr_ptr->line_style;
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_retrieve_line_weight(key,&line_weight)
**      retrieve line_weight
**    PARAMETERS   
**       INPUT  : 
**        key,		key to to use in retrieving the line_weight
**			line_weight,	a pointer to where the line_weight is to be put
**       OUTPUT :  
**			line_weight,	the appropriate line_weight
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_line_weight(key,line_weight)
UU_KEY_ID 	key;				/* the key to update to					*/
UU_REAL		*line_weight;	/* pointer to the line_weight				*/
{
	int				status;		/* status, -1 if error, 0 otherwise */
	UU_KEY_ID		attr_key;
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle */
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;

	uu_denter(UU_RTRC,(us,"ur_retrieve_line_weight for key = %d",key));

	/* first determine that the master tuple is active and that there */
	/* is an attribute bundle */
	status = ur_retrieve_master_attr(key,UR_ATTR_INDX,&attr_key);
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_tuple_ptr(rel, tuple, &attr_ptr);
		if(status == 0)
			*line_weight = attr_ptr->line_weight;
	}
	uu_dprint(UU_RITRC,(us,"line weight = %g",*line_weight));
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION     :  status = ur_retrieve_line_width(key,&line_width)
**      retrieve line_width
**    PARAMETERS   
**       INPUT  : 
**        key,		key to to use in retrieving the line_width
**			line_width,	a pointer to where the line_width is to be put
**       OUTPUT :  
**			line_width,	the appropriate line_width
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_line_width(key,line_width)
UU_KEY_ID 	key;				/* the key to update to */
UU_REAL		*line_width;	/* pointer to the line_width */
{
	int				status;		/* status, -1 if error, 0 otherwise */
	UU_KEY_ID		attr_key;
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle */
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;

	uu_denter(UU_RTRC,(us,"ur_retrieve_line_width for key = %d",key));

	/* first determine that the master tuple is active and that there */
	/* is an attribute bundle */
	status = ur_retrieve_master_attr(key,UR_ATTR_INDX,&attr_key);
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_tuple_ptr(rel, tuple, &attr_ptr);
		if(status == 0)
			*line_width = attr_ptr->line_width;
	}
	uu_dprint(UU_RITRC,(us,"line width = %g",*line_width));
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION     :  status = ur_retrieve_displayable(key,&displayable)
**      retrieve displayable
**    PARAMETERS   
**       INPUT  : 
**        key,		key to to use in retrieving the displayable
**			displayable,	a pointer to where the displayable is to be put
**       OUTPUT :  
**			displayable,	the appropriate displayable
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_displayable(key,displayable)
UU_KEY_ID	key;				/* the key to update to */
int			*displayable;	/* pointer to the displayable */
{
	int				status;		/* status, -1 if error, 0 otherwise		*/
	UU_KEY_ID		attr_key;
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;

	uu_denter(UU_RTRC,(us,"ur_retrieve_displayable(key=%d)",key));

	/* first determine that the master tuple is active and that there */
	/* is an attribute bundle */
	status = ur_retrieve_master_attr(key,UR_ATTR_INDX,&attr_key);
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_tuple_ptr(rel, tuple, &attr_ptr);
		if(status == 0)
			*displayable = attr_ptr->displayable;
	}
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION     :  status = ur_retrieve_selectable(key,&selectable)
**      retrieve selectable
**    PARAMETERS   
**       INPUT  : 
**        key,		key to to use in retrieving the selectable
**			selectable,	a pointer to where the selectable is to be put
**       OUTPUT :  
**			selectable,	the appropriate selectable
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_selectable(key,selectable)
UU_KEY_ID 	key;				/* the key to update to */
UU_LOGICAL	*selectable;	/* pointer to the selectable */
{
	int				status;		/* status, -1 if error, 0 otherwise */
	UU_KEY_ID		attr_key;
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle */
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;

	uu_denter(UU_RTRC,(us,"ur_retrieve_selectable for key = %d",key));

	/* first determine that the master tuple is active and that there */
	/* is an attribute bundle */
	status = ur_retrieve_master_attr(key,UR_ATTR_INDX,&attr_key);
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_tuple_ptr(rel, tuple, &attr_ptr);
		if(status == 0)
			*selectable = attr_ptr->selectable;
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_retrieve_blanked(key,&blanked)
**      retrieve blanked
**    PARAMETERS   
**       INPUT  : 
**        key,		key to to use in retrieving the blanked
**			blanked,	a pointer to where the blanked is to be put
**       OUTPUT :  
**			blanked,	the appropriate blanked
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_blanked(key,blanked)
UU_KEY_ID	key;			/* the key to update to */
UU_LOGICAL	*blanked;	/* pointer to the blanked */
{
	int						status;		/* status, -1 if error, 0 otherwise */
	struct UR_MTID_rec	*m_ptr;		/* ptr to relation control block*/
	UR_REL_NUM				rel;
	UR_TUPLE_INDX			tuple;

	uu_denter(UU_RTRC,(us,"ur_retrieve_blanked(%d)", key));

	/* first determine that the master tuple is active and */
	/* get master tuple as relation,entry, and a pointer to the fixed data */
	ur_k2rt(key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (status == 0)
	{
		if (ur_blankedp(m_ptr))
			*blanked = UU_TRUE;
		else
			*blanked = UU_FALSE;
	}
	uu_dexit;
	return(status);
}

