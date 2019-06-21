
/*********************************************************************
**    NAME         :  m2ucolor.c
**       CONTAINS: user interface routines for setting color
**       um_sea_entclr
**       um_sea_fillclr
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2ucolor.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:49
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "msol.h"
#include "mdpick.h"
#include "gobas.h"
#include "go3.h"
#include "ginqst.h"
#include "dmark.h"
#include "class.h"

UU_LOGICAL	ud_gnxt();

/*********************************************************************
**    E_FUNCTION: umu_sea_entclr()
**       Change the color of an entity.
**    PARAMETERS   
**       INPUT  : 
**				color					new color for the entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_sea_entclr()
	{
	int color;
	struct UC_entitydatabag e;						/* picked entity */
	int numint;
	int markval,n;
	char cmd[120];										/* ROMULUS command buffer */
	UU_LOGICAL initialize;							/* initialize get next entity */
	UU_LOGICAL firstsym = UU_TRUE;

	uu_denter(UU_MTRC,(us,"umu_sea_entclr()"));

	UD_MARK(markval, UU_FALSE);
	if(markval == 0)
		{
		/* ask user for a color */
		ud_setpallet2(8);
		color = 0;			/* backgroung color */
		while (color == 0) color = ud_getcolr("choose entity color");					
		/* take down color pallette */
		n=gqsegerasecolor();	/* Set erase color to bkgnd since color bar	*/
		gsegerasecolor(0);	/*		is not in graphics area	*/
		ud_colrvis(UG_INVISIBLE);			/* make color pallet invisible */
		gsegerasecolor(n);				/* Set erase color back to prev value */

		ud_lgeo(UU_TRUE, UD_geomandsym);
		ud_ldas(UD_DASSELECT,/*pick entity*/UM_MODEL,26, 
							UU_NULL,1,&numint,UD_NODEFAULT);

		initialize = UU_TRUE;
		while(ud_gnxt(initialize, UU_NULL, &e.key, 1))
			{
			if (uc_retrieve_data(&e, sizeof(e)) != UU_SUCCESS) goto failed;
			switch(uc_super_class(e.rel_num))
				{
				case UC_SYMBOL_CLASS:
					/* change color and display instance */
					if (ubu_change_instance_color(&e, color, &firstsym) 
							!= UU_SUCCESS) goto failed;
					break;
				case UC_GEOMETRY_CLASS:
					{
					ur_update_color(e.key,color);
					if (e.rel_num == UM_BODY_REL)
						{
						struct UM_body_rec *ptr;
	
						ptr = (struct UM_body_rec*)&e;
						um_init_rombuf();
						switch (color)
							{
							case 1:
	 							sprintf(cmd,"APPEARANCE %s SET COLOUR 0 0 .1",ptr->name);
								break;
							case 3:
	 							sprintf(cmd,"APPEARANCE %s SET COLOUR 0 0 .3",ptr->name);
								break;
							case 4:
	 							sprintf(cmd,"APPEARANCE %s SET COLOUR 0 0 .4",ptr->name);
								break;
							case 5:
	 							sprintf(cmd,"APPEARANCE %s SET COLOUR 0 0 .5",ptr->name);
								break;
							case 6:
	 							sprintf(cmd,"APPEARANCE %s SET COLOUR 0 0 .6",ptr->name);
								break;
							case 7:
	 							sprintf(cmd,"APPEARANCE %s SET COLOUR 0 0 .7 ",ptr->name);
								break;
							case 8:
	 							sprintf(cmd,"APPEARANCE %s SET COLOUR 0 0 .8",ptr->name);
								break;
							}
	 					um_add_rombuf(cmd);
						um_callromulus();
						}/* end if solid */

					if (uc_display(&e) != UU_SUCCESS) goto failed;
					break;
					}/* end geometry case */
				default:
					break;
				}/* end switch on class */
			initialize = UU_FALSE;
			}/* end while */
		}
	else	
		{
		/* take down color pallette */
		n=gqsegerasecolor();	/* Set erase color to bkgnd since color bar	*/
		gsegerasecolor(0);	/*		is not in graphics area	*/
		ud_colrvis(UG_INVISIBLE);			/* make color pallet invisible */
		gsegerasecolor(n);				/* Set erase color back to prev value */
		}
failed:;
	UD_UNMARK(markval);
	ud_lgeo(UU_FALSE, UD_geomandsym);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_sea_fillclr(color)
**       Change the fill color of an appropriate entity
**    PARAMETERS   
**       INPUT  : 
**				color					new fill color for the entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

umu_sea_fillclr()
	{
	UM_PLOCREC	pick;						/* the pick id and location */
	int			numint;					/* the number user interactions	*/
	struct UM_crvdatabag	e;				/* entity to be filled	*/
	struct UM_poly_rec	*p;
	struct UM_compcrv_rec	*c;
	int color,n;
	int markval;

	uu_denter(UU_MTRC,(us,"umu_sea_fillclr()"));

	UD_MARK(markval, UU_FALSE);
	if (markval == 0)
		{
		/* ask user for a color */
		ud_setpallet2(8);
		/* background color allowed	*/
		color = ud_getcolr("choose fill color");
		/* take down color pallette */
		n=gqsegerasecolor();	/* Set erase color to bkgnd since color bar	*/
		gsegerasecolor(0);	/*		is not in graphics area	*/
		ud_colrvis(UG_INVISIBLE);			/* make color pallet invisible */
		gsegerasecolor(n);				/* Set erase color back to prev value */
	
		while (UU_TRUE)
			{
			um_dl_pldas( UD_DASPCKLOC, /* Pick an entity*/UM_MODEL,
					153, &pick, 1, &numint, 1);
			if (numint < 1)
				goto Done;
	
			e.key = um_get_pickkey(&pick.pent, 1);
			um_get_all_geom(&e, sizeof(struct UM_crvdatabag) );
			
			switch (e.rel_num)
				{
		 	case	UM_POLY_REL:
				p = ((struct UM_poly_rec *) &e);
				p->fcolor = color;
				break;
		 	case	UM_COMPCRV_REL:
				c = ((struct UM_compcrv_rec *) &e);
				c->fcolor = color;
				break;
		 	default:
				uu_uerror0(UM_MODEL, 43 /* Entity cannot be filled	*/);
				/** NOTE: I cheated and used a prexisting message	jdm **/
				goto Done;
	
				}

			um_update_geom(&e, UM_DEFAULT_TF);
			uc_display(&e);
			}
		}
	else
		{
		/* take down color pallette */
		n=gqsegerasecolor();	/* Set erase color to bkgnd since color bar	*/
		gsegerasecolor(0);	/*		is not in graphics area	*/
		ud_colrvis(UG_INVISIBLE);			/* make color pallet invisible */
		gsegerasecolor(n);				/* Set erase color back to prev value */
		goto Done;
		}

Done:
	UD_UNMARK(markval);
	uu_dexit;
	}
