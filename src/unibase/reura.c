/*********************************************************************
**    NAME         :  reura
**       CONTAINS:
**       ur_update_color
**       ur_update_color1()
**       ur_update_layer
**       ur_update_layer1()
**       ur_update_pen
**       ur_update_pen1()
**       ur_update_line_style
**       ur_update_line_style1()
**       ur_update_line_width
**       ur_update_line_width1()
**       ur_update_line_weight
**       ur_update_line_weight1()
**       ur_update_displayable
**       ur_update_displayable1()
**       ur_update_selectable
**       ur_update_selectable1()
**       ur_update_blanked
**       ur_update_blanked1()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reura.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:40
*********************************************************************/

#include	"usysdef.h"
#include "umessages.h"
#include	"rmtuple.h"
#include	"rbase.h"
#include	"ribase.h"
#include "riddle.h"
#include	"udebug.h"
#include "r1emsgpk.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_update_color(key, color)
**      update color
**    PARAMETERS   
**       INPUT  : 
** 	      key,		key to to use in updating the color
**			 	color, 		new color
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_color(key, color)
UU_KEY_ID	key;	/* the key to update to */
int			color;	/* pointer to the color */
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_color(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_COLOR, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_color1(key, color, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION     :  status = ur_update_color1(key, color, theMessage, messageCnt)
**      update color
**    PARAMETERS   
**       INPUT  : 
** 	      key,		key to to use in updating the color
**			 	color, 		new color
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_color1(key, color, theMessage, messageCnt)
UU_KEY_ID	key;	/* the key to update to */
int			color;	/* pointer to the color */
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int				status;		/* status, -1 if error, 0 otherwise */
	UU_REL_ID		attr_key;
	UR_REL_NUM		attr_rel;	/* attribute relation */
	UR_TUPLE_INDX	attr_indx;	/* attribute tuple index */
	struct UR_attr	attr;			/* an attibute bundle */
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	int				attr_len;
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC, (us,
			"ur_update_color1(key= 0x%x, color= %d messageCnt=%d)",
			key, color, messageCnt));

	/* first determine if master tuple is active and that there is an */
	/* attribute bundle */
	ur_k2rt(key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (ur_editablep(m_ptr))
	{
		attr_key = m_ptr->assocs[UR_ATTR_INDX];
	}
	else
	{
		status = -1;	/* entity is not editable */
	}
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_varlist_ptr(rel, tuple, 0, &attr_ptr, &attr_len);
		if(status == 0)
		{
			if(attr_ptr->use_count == 1)
			{
				attr_ptr->color = color;
			}
			else
			{
				attr.key_id = key;
				ur_retrieve_attr(&attr);
				attr.color = color;
				status = ur_update_attr(&attr);
			}
			/* get master rel & tuple, and a pointer to the master fixed data */
			ur_k2rt(key, &rel, &tuple);
			if(rel == UR_MTUPLE_REL)
			{
				status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

				/* now notify associates */
				if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
				{
					uri_notify(m_ptr, key, theMessage, messageCnt);
				}
			}
		}
	}
	uu_dexit;
	return(status);
}


/*********************************************************************
**    E_FUNCTION     :  status = ur_update_layer(key, layer)
**      update layer
**    PARAMETERS   
**       INPUT  : 
** 	      key,		key to to use in updating the layer
**			 	layer, 		new layer
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_layer(key, layer)
UU_KEY_ID	 	key;	/* the key to update to					*/
int				layer;	/* pointer to the layer						*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_layer(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_LAYER, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_layer1(key, layer, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION     :  status = ur_update_layer1(key, layer, theMessage, messageCnt)
**      update layer
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the layer
**			 layer, 		new layer
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_layer1(key, layer, theMessage, messageCnt)
UU_KEY_ID	key;	/* the key to update to					*/
int			layer;	/* pointer to the layer						*/
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int				status;		/* status, -1 if error, 0 otherwise		*/
	UU_REL_ID		attr_key;
	UR_REL_NUM		attr_rel;	/* attribute relation							*/
	UR_TUPLE_INDX	attr_indx;	/* attribute tuple index						*/
	struct UR_attr	attr;			/* an attibute bundle						*/
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	int				attr_len;
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC, (us,
			"ur_update_layer1(key= 0x%x, layer= %d messageCnt=%d)",
			key, layer, messageCnt));

	/* first determine if master tuple is active and that there is an */
	/* attribute bundle */
	ur_k2rt(key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (ur_editablep(m_ptr))
	{
		attr_key = m_ptr->assocs[UR_ATTR_INDX];
	}
	else
	{
		status = -1;	/* entity is not editable */
	}
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_varlist_ptr(rel, tuple, 0, &attr_ptr, &attr_len);
		if(status == 0)
		{
			if(attr_ptr->use_count == 1)
			{
				attr_ptr->layer = layer;
			}
			else
			{
				attr.key_id = key;
				ur_retrieve_attr(&attr);
				attr.layer = layer;
				status = ur_update_attr(&attr);
			}
			/* get master rel & tuple, and a pointer to the master fixed data */
			ur_k2rt(key, &rel, &tuple);
			if(rel == UR_MTUPLE_REL)
			{
				status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

				/* now notify associates */
				if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
				{
					uri_notify(m_ptr, key, theMessage, messageCnt);
				}
			}
		}
	}
	uu_dexit;
	return(status);
}


/*********************************************************************
**    E_FUNCTION     :  status = ur_update_pen(key, pen)
**      update pen
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the pen
**			 pen, 		new pen
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_pen(key, pen)
UU_KEY_ID	key;	/* the key to update to					*/
int			pen;		/* pointer to the pen						*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_pen(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_PEN, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_pen1(key, pen, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION  :  status = ur_update_pen1(key, pen, theMessage, messageCnt)
**      update pen
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the pen
**			 pen, 		new pen
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_pen1(key, pen, theMessage, messageCnt)
UU_KEY_ID	key;	/* the key to update to					*/
int			pen;		/* pointer to the pen						*/
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int				status;		/* status, -1 if error, 0 otherwise		*/
	UU_REL_ID		attr_key;
	UR_REL_NUM		attr_rel;	/* attribute relation							*/
	UR_TUPLE_INDX	attr_indx;	/* attribute tuple index						*/
	struct UR_attr	attr;			/* an attibute bundle						*/
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	int				attr_len;
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC, (us, "ur_update_pen1(key= 0x%x, pen= %d messageCnt=%d)",
			key, pen, messageCnt));

	/* first determine if master tuple is active and that there is an */
	/* attribute bundle */
	ur_k2rt(key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (ur_editablep(m_ptr))
	{
		attr_key = m_ptr->assocs[UR_ATTR_INDX];
	}
	else
	{
		status = -1;	/* entity is not editable */
	}
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_varlist_ptr(rel, tuple, 0, &attr_ptr, &attr_len);
		if(status == 0)
		{
			if(attr_ptr->use_count == 1)
			{
				attr_ptr->pen = pen;
			}
			else
			{
				attr.key_id = key;
				ur_retrieve_attr(&attr);
				attr.pen = pen;
				status = ur_update_attr(&attr);
			}	
			/* get master rel & tuple, and a pointer to the master fixed data */
			ur_k2rt(key, &rel, &tuple);
			if(rel == UR_MTUPLE_REL)
			{
				status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

				/* now notify associates */
				if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
				{
					uri_notify(m_ptr, key, theMessage, messageCnt);
				}
			}
		}
	}
	uu_dexit;
	return(status);
}



/*********************************************************************
**    E_FUNCTION     :  status = ur_update_line_style(key, line_style)
**      update line_style
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the line_style
**			 line_style, new line_style
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_line_style(key, line_style)
UU_KEY_ID	key;		/* the key to update to					*/
int			line_style;	/* pointer to the line_style				*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_line_style(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_LNSTY, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_line_style1(key, line_style, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION     :  status = ur_update_line_style1(key, line_style,
**									theMessage, messageCnt)
**      update line_style
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the line_style
**			 line_style, new line_style
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_line_style1(key, line_style, theMessage, messageCnt)
UU_KEY_ID	key;		/* the key to update to					*/
int			line_style;	/* pointer to the line_style				*/
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int				status;		/* status, -1 if error, 0 otherwise		*/
	UU_REL_ID		attr_key;
	UR_REL_NUM		attr_rel;	/* attribute relation							*/
	UR_TUPLE_INDX	attr_indx;	/* attribute tuple index						*/
	struct UR_attr	attr;			/* an attibute bundle						*/
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	int				attr_len;
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC, (us,
			"ur_update_line_style1(key= 0x%x, line_style= %d messageCnt=%d)",
			key, line_style, messageCnt));

	/* first determine if master tuple is active and that there is an */
	/* attribute bundle */
	ur_k2rt(key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (ur_editablep(m_ptr))
	{
		attr_key = m_ptr->assocs[UR_ATTR_INDX];
	}
	else
	{
		status = -1;	/* entity is not editable */
	}
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_varlist_ptr(rel, tuple, 0, &attr_ptr, &attr_len);
		if(status == 0)
		{
			if(attr_ptr->use_count == 1)
			{
				attr_ptr->line_style = line_style;
			}
			else
			{
				attr.key_id = key;
				ur_retrieve_attr(&attr);
				attr.line_style = line_style;
				status = ur_update_attr(&attr);
			}
			/* get master rel & tuple, and a pointer to the master fixed data */
			ur_k2rt(key, &rel, &tuple);
			if(rel == UR_MTUPLE_REL)
			{
				status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

				/* now notify associates */
				if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
				{
					uri_notify(m_ptr, key, theMessage, messageCnt);
				}
			}
		}
	}
	uu_dexit;
	return(status);
}



/*********************************************************************
**    E_FUNCTION     :  status = ur_update_line_weight(key, line_weight)
**      update line_weight
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the line_weight
**			 line_weight,new line_weight
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_line_weight(key, line_weight)
UU_KEY_ID 	key;			/* the key to update to					*/
UU_REAL		line_weight;	/* the line_weight			*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_line_weight(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_LNWGT, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_line_weight1(key, line_weight, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION     :  status = ur_update_line_weight1(key, line_weight,
**											theMessage, messageCnt)
**      update line_weight
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the line_weight
**			 line_weight,new line_weight
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_line_weight1(key, line_weight, theMessage, messageCnt)
UU_KEY_ID 	key;			/* the key to update to					*/
UU_REAL		line_weight;	/* the line_weight			*/
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int				status;		/* status, -1 if error, 0 otherwise		*/
	UU_REL_ID		attr_key;
	UR_REL_NUM		attr_rel;	/* attribute relation							*/
	UR_TUPLE_INDX	attr_indx;	/* attribute tuple index						*/
	struct UR_attr	attr;			/* an attibute bundle						*/
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	int				attr_len;
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC, (us,
			"ur_update_line_weight1(key= 0x%x, line_weight= %g messageCnt=%d)",
			key, line_weight, messageCnt));

	/* first determine if master tuple is active and that there is an */
	/* attribute bundle */
	ur_k2rt(key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (ur_editablep(m_ptr))
	{
		attr_key = m_ptr->assocs[UR_ATTR_INDX];
	}
	else
	{
		status = -1;	/* entity is not editable */
	}
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_varlist_ptr(rel, tuple, 0, &attr_ptr, &attr_len);
		if(status == 0)
		{
			if(attr_ptr->use_count == 1)
			{
				attr_ptr->line_weight = line_weight;
			}
			else
			{
				attr.key_id = key;
				ur_retrieve_attr(&attr);
				attr.line_weight = line_weight;
				status = ur_update_attr(&attr);
			}
			/* get master rel & tuple, and a pointer to the master fixed data */
			ur_k2rt(key, &rel, &tuple);
			if(rel == UR_MTUPLE_REL)
			{
				status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

				/* now notify associates */
				if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
				{
					uri_notify(m_ptr, key, theMessage, messageCnt);
				}
			}
		}
	}
	uu_dexit;
	return(status);
}



/*********************************************************************
**    E_FUNCTION     :  status = ur_update_line_width(key, line_width)
**      update line_width
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the line_width
**			 line_width,new line_width
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_line_width(key, line_width)
UU_KEY_ID 	key;		/* the key to update to					*/
UU_REAL		line_width;	/* the line_width			*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_line_width(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_LNWID, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_line_width1(key, line_width, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION     :  status = ur_update_line_width1(key, line_width,
**												theMessage, messageCnt)
**      update line_width
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the line_width
**			 line_width,new line_width
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_line_width1(key, line_width, theMessage, messageCnt)
UU_KEY_ID 	key;		/* the key to update to					*/
UU_REAL		line_width;	/* the line_width			*/
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int				status;		/* status, -1 if error, 0 otherwise		*/
	UU_REL_ID		attr_key;
	UR_REL_NUM		attr_rel;	/* attribute relation							*/
	UR_TUPLE_INDX	attr_indx;	/* attribute tuple index						*/
	struct UR_attr	attr;			/* an attibute bundle						*/
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	int				attr_len;
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC, (us,
			"ur_update_line_width1(key= 0x%x, line_width= %g messageCnt=%d)",
			key, line_width, messageCnt));

	/* first determine if master tuple is active and that there is an */
	/* attribute bundle */
	ur_k2rt(key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (ur_editablep(m_ptr))
	{
		attr_key = m_ptr->assocs[UR_ATTR_INDX];
	}
	else
	{
		status = -1;	/* entity is not editable */
	}
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_varlist_ptr(rel, tuple, 0, &attr_ptr, &attr_len);
		if(status == 0)
		{
			if(attr_ptr->use_count == 1)
			{
				attr_ptr->line_width = line_width;
			}
			else
			{
				attr.key_id = key;
				ur_retrieve_attr(&attr);
				attr.line_width = line_width;
				status = ur_update_attr(&attr);
			}
			/* get master rel & tuple, and a pointer to the master fixed data */
			ur_k2rt(key, &rel, &tuple);
			if(rel == UR_MTUPLE_REL)
			{
				status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

				/* now notify associates */
				if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
				{
					uri_notify(m_ptr, key, theMessage, messageCnt);
				}
			}
		}
	}
	uu_dexit;
	return(status);
}



/*********************************************************************
**    E_FUNCTION   :  status = ur_update_displayable(key, displayable)
**      update displayable
**    PARAMETERS   
**       INPUT  : 
** 	       key,			key to to use in updating the displayable
**			 displayable, 	new displayable
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_displayable(key, displayable)
UU_KEY_ID 	key;			/* the key to update to					*/
int			displayable;	/* pointer to the displayable				*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_displayable(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_DISP, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_displayable1(key, displayable, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION   :  status = ur_update_displayable1(key, displayable,
**										theMessage, messageCnt)
**      update displayable
**    PARAMETERS   
**       INPUT  : 
** 	       key,			key to to use in updating the displayable
**			 displayable, 	new displayable
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_displayable1(key, displayable, theMessage, messageCnt)
UU_KEY_ID 	key;			/* the key to update to					*/
int			displayable;	/* pointer to the displayable				*/
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int				status;		/* status, -1 if error, 0 otherwise		*/
	UU_REL_ID		attr_key;
	UR_REL_NUM		attr_rel;	/* attribute relation							*/
	UR_TUPLE_INDX	attr_indx;	/* attribute tuple index						*/
	struct UR_attr	attr;			/* an attibute bundle						*/
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	int				attr_len;
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC, (us,
			"ur_update_displayable1(key= 0x%x, displayable= %d messageCnt=%d)",
			key, displayable, messageCnt));

	/* first determine if master tuple is active and that there is an */
	/* attribute bundle */
	status = ur_retrieve_master_attr(key, UR_ATTR_INDX, &attr_key);
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_varlist_ptr(rel, tuple, 0, &attr_ptr, &attr_len);
		if(status == 0)
		{
			if(attr_ptr->use_count == 1)
			{
				attr_ptr->displayable = displayable;
			}
			else
			{
				/* Create new attribute tuple - set use count to 1 */
				attr.key_id = key;
				ur_retrieve_attr(&attr);
				attr.displayable = displayable;
				attr.use_count = 1;
				status = ur_create_tuple(rel, &tuple, &attr);
				if(status == 0)
				{
					ur_rt2k(rel, tuple, &attr_key);
					ur_update_mtuple_attr(key, UR_ATTR_INDX, attr_key);
				}
			}
			if(status == 0)
			{
				/* get master rel & tuple, and a pointer to the master fixed data */
				ur_k2rt(key, &rel, &tuple);
				if(rel == UR_MTUPLE_REL)
				{
					status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

					/* now notify associates */
					if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
					{
						uri_notify(m_ptr, key, theMessage, messageCnt);
					}
				}
			}
		}
	}
	uu_dexit;
	return(status);
}



/*********************************************************************
**    E_FUNCTION     :  status = ur_update_selectable(key, selectable)
**      update selectable
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the selectable
**			 selectable,new selectable
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_selectable(key, selectable)
UU_KEY_ID	key;			/* the key to update to					*/
UU_LOGICAL	selectable;	/* pointer to the selectable				*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_selectable(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_SEL, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_selectable1(key, selectable, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION     :  status = ur_update_selectable1(key, selectable,
**									theMessage, messageCnt)
**      update selectable
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the selectable
**			 selectable,new selectable
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_selectable1(key, selectable, theMessage, messageCnt)
UU_KEY_ID			key;			/* the key to update to					*/
UU_LOGICAL			selectable;	/* pointer to the selectable				*/
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int				status;		/* status, -1 if error, 0 otherwise		*/
	UU_REL_ID		attr_key;
	UR_REL_NUM		attr_rel;	/* attribute relation							*/
	UR_TUPLE_INDX	attr_indx;	/* attribute tuple index						*/
	struct UR_attr	attr;			/* an attibute bundle						*/
	struct UR_attr	*attr_ptr;	/* pointer to a generic attr bundle	*/
	int				attr_len;
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC, (us,
			"ur_update_selectable1(key= 0x%x, selectable= %d messageCnt=%d)",
			key, selectable, messageCnt));

	/* first determine if master tuple is active and that there is an */
	/* attribute bundle */
	status = ur_retrieve_master_attr(key, UR_ATTR_INDX, &attr_key);
	if(status == 0 && attr_key != 0)
	{
		ur_k2rt(attr_key, &rel, &tuple);
		status = ur_get_varlist_ptr(rel, tuple, 0, &attr_ptr, &attr_len);
		if(status == 0)
		{
			if(attr_ptr->use_count == 1)
			{
				attr_ptr->selectable = selectable;
			}
			else
			{
				attr.key_id = key;
				ur_retrieve_attr(&attr);
				attr.selectable = selectable;
				attr.use_count = 1;
				status = ur_create_tuple(rel, &tuple, &attr);
				if(status == 0)
				{
					ur_rt2k(rel, tuple, &attr_key);
					ur_update_mtuple_attr(key, UR_ATTR_INDX, attr_key);
				}
			}
			if(status == 0)
			{
				/* get master rel & tuple, and a pointer to the master fixed data */
				ur_k2rt(key, &rel, &tuple);
				if(rel == UR_MTUPLE_REL)
				{
					status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

					/* now notify associates */
					if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
					{
						uri_notify(m_ptr, key, theMessage, messageCnt);
					}
				}
			}
		}
	}
	uu_dexit;
	return(status);
}



/*********************************************************************
**    E_FUNCTION     :  status = ur_update_blanked(key, blanked)
**      update blanked
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the blanked
**			 blanked, new blanked
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_blanked(key, blanked)
UU_KEY_ID	key;		/* the key to update to					*/
UU_LOGICAL	blanked;	/* pointer to the blanked				*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_blanked(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_BLNKD, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_blanked1(key, blanked, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION     :  status = ur_update_blanked1(key, blanked,
**										theMessage, messageCnt)
**      update blanked
**    PARAMETERS   
**       INPUT  : 
** 	       key,		key to to use in updating the blanked
**			 blanked, new blanked
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_blanked1(key, blanked, theMessage, messageCnt)
UU_KEY_ID	key;		/* the key to update to */
UU_LOGICAL	blanked;	/* pointer to the blanked */
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int						status;		/* status, -1 if error, 0 otherwise */
	UR_REL_NUM				rel;
	UR_TUPLE_INDX			tuple;
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC, (us,
			"ur_update_blanked1(key= 0x%x, blanked= %d messageCnt=%d)",
			key, blanked, messageCnt));

	/* first determine if master tuple is active and */
	/* get master tuple as relation,entry, and a pointer to the fixed data */
	ur_k2rt(key, &rel, &tuple);
	if(rel == UR_MTUPLE_REL)
	{
		status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
		if(status == 0)
		{
			if (blanked)
				ur_blank(m_ptr);
			else
				ur_unblank(m_ptr);
			/* get master rel & tuple, and a pointer to the master fixed data */
			ur_k2rt(key, &rel, &tuple);
			if(rel == UR_MTUPLE_REL)
			{
				status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

				/* now notify associates */
				if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
				{
					uri_notify(m_ptr, key, theMessage, messageCnt);
				}
			}
		}
	}
	else
	{
		status = -1;
	}
	uu_dexit;
	return(status);
}

