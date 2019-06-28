 /*********************************************************************
**    NAME         :  nucattr.c   ( Change ATTRibutes )
**       CONTAINS: User interface routines for changing entity attributes.
**			nclu_color_change()
**			nclu_line_style_change()
**			nclu_layer_change()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nucattr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:04
*********************************************************************/
#include "usysdef.h"
#include "ddef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "adrfcom.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "mattrddl.h"
/*#include "dgetfilt.h"*/
#include "mdattr.h"
#include "view.h"

static int choice[25][25];
#define NO_DEFAULT 0
#define DEFAULT 1


/*********************************************************************
**    E_FUNCTION     : nclu_color_change()
**       change color attribute via pop-up menu
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_color_change()
{
	int status;
	int choice;

	uu_denter( UU_MTRC,(us,"ncl_color_change()"));

	status = ncl_popup(NCL_COLOR_CHOICE, &choice);
      
	if(status == NCL_OKINPUT)
		ncl_set_color_attr(choice);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : nclu_line_style_change()
**       change line style attribute via pop-up menu
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_line_style_change()

      {
      int status;
      int choice;

      uu_denter( UU_MTRC,(us,"ncl_line_style_change()"));

      status = ncl_popup(NCL_LINE_CHOICE, &choice);
      
      if(status == NCL_OKINPUT) ur_put_attrmdl_line_style(choice);

      uu_dexit;
      }

/*********************************************************************
**    E_FUNCTION     : nclu_layer_change()
**       change layer number attribute via pop-up menu
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_layer_change()

      {
      int status;
      int choice;
      int layer_no;

      uu_denter( UU_MTRC,(us,"ncl_layer_change()"));

      status = ncl_popup(NCL_LAYER_CHOICE, &choice);
      
      if(status == NCL_OKINPUT)
      	{
      	switch (choice)
      		{
/*	layers 1 to 5 */
      		case 1:
      		case 2:
      		case 3:
      		case 4:
      		case 5:
      			ur_put_attrmdl_layer(choice);
      			break;

/*	make it ((layer number - 5) * 10) */
      		case 6:		/* layer 10 */
      		case 7:		/* layer 20 */
      		case 8:		/* layer 30 */
      		case 9:		/* layer 40 */
      		case 10:	/* layer 50 */
      			ur_put_attrmdl_layer((choice-5)*10);
      			break;
/*	ask what layer number by text */
      		case 11:
/*			status = ncl_get_num(&layer_no, 437);  */
/*      			if (status == NCL_OKINPUT)     */
/*      			ur_put_attrmdl_color(layer_no);*/
                        umu_sda1_layer_num();

      		}
      	}
/*
......Update the status area with the new layer number
......at all times
......Bobby  -  3/16/92
*/
		um_set_layer_num(ur_get_attrmdl_layer());
		um_set_active_layer(ur_get_attrmdl_layer());
      uz_actlayer(ur_get_attrmdl_layer());
      uu_dexit;
      }
