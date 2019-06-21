/*********************************************************************
**    NAME         :  feareal
**       CONTAINS:
**			int um_fearealstring(value,string)
**			int um_fealength(strprefix,length,pos,loc,dflag)
**			int um_feaangle(strprefix,angle,pos,loc,dflag)
**			int um_feareal(string,value,pos,loc,dflag)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       feareal.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:45
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "usysg.h"
#include	"gtbl.h"
#include "g.h"
#include "mdcoord.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "mfeatcom.h"
#include "modef.h"
#include "mattr.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     :  int um_fearealstring(value,string)
**      Convert a value for a real feature to a string.
**    PARAMETERS   
**       INPUT  : 
**          value					value of the real feature to put in table 
**       OUTPUT :  
**          string				string representing value of real feature
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void um_fearealstring (value,string)
	UU_REAL value;
   char *string;

	{
	char vstr[20];
	int vstr_len;
	char vwh[11];
	char vdec[9];
	int char_count;
	UU_REAL disp_val;
	int i;

	uu_denter(UU_MTRC,(us,"um_fearealstring(value:%g,%x)",value,string));

	/* convert real value to string value */
	disp_val = value + (0.5 * pow((UU_REAL) 10.0,(UU_REAL) -UM_dispattr.fea_decpl));
	sprintf (vstr, "%g", disp_val);

	/* initialize the character count */
	char_count = 0;

	/* get the length of vstr */
	vstr_len = strlen(vstr);

	/* check string length for bounds */
	if (vstr_len > 10) vstr_len = 10;

	/* strip out the whole part */
	do
		{
		vwh[char_count] = vstr[char_count];
		if (vwh[char_count] == '.') break;
		char_count++;
		} while (char_count < vstr_len);

	/* check for whole number input */
	if (char_count < vstr_len) /* increment char_count */
			char_count++;

	/* add a null teminator */
	vwh[char_count] = 0;

	/* now get the decimal part with desired number of decimal places */
	for (i=0 ; i<UM_dispattr.fea_decpl ; i++)
		{
		if (char_count + i >= vstr_len) break;
		vdec[i] = vstr[char_count + i];
		}

	/* add a null teminator */
	vdec[i++] = 0;

	/* clear out the old string */
	for (i=0 ; i<vstr_len ; i++)
		vstr[i] = ' ';

	/* put the value back together */
	sprintf (string, "%s%s", vwh, vdec);

	uu_dexit;

	}

/*********************************************************************
**    E_FUNCTION     :  int um_fealength(strprefix,length,pos,loc,dflag)
**      Convert a "length" feature to the current output units for
**			length, display the feature, and put the value into the
**			feature table.
**    PARAMETERS   
**       INPUT  : 
**          strprefix			text for the feature
**          length				value of the length feature to put in table 
**          pos					position of the text of the feature
**          loc					location of the leader line for the feature
**		    	dflag					UU_TRUE => draw leader line
**										UU_FALSE => don't draw leader line
**       OUTPUT :  
**          output: none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
um_fealength(strprefix,length,pos,loc,dflag)
   Gchar *strprefix;
	UU_REAL length;
   UU_REAL pos[];
   UU_REAL loc[];
   UU_LOGICAL dflag;

	{
	UU_REAL dspval;
	char str[256];
	char strvalue[20];
	char strsuffix[4];

	uu_denter(UU_MTRC,(us,"um_fealength(%s, %g, %x, %x, %d)", 
		strprefix, length, dspval, pos, loc, dflag));

	/* determine the text for the real feature */
	UM_len_inttoext(length, dspval);
	um_fearealstring(dspval,strvalue);

	/* determine text suffix */
	switch (UM_cpln.length_unit)
		{
		case UM_INCH:
			strcpy(strsuffix,"in");
			break;
		case UM_FEET:
			strcpy(strsuffix,"ft");
			break;
		case UM_MILE:
			strcpy(strsuffix,"mi");
			break;
		case UM_MM:
			strcpy(strsuffix,"mm");
			break;
		case UM_CM:
			strcpy(strsuffix,"cm");
			break;
		case UM_M:
			strcpy(strsuffix,"m");
			break;
		case UM_KM:
			strcpy(strsuffix,"km");
			break;
		case UM_MIL:
			strcpy(strsuffix,"mil");
			break;
		default:
			strcpy(strsuffix,"");
			break;
		}

	/* compose the text for the feature and add to the real feature table */
	sprintf(str,"%s%s %s",strprefix, strvalue, strsuffix);
	return um_feareal(0, str,dspval,pos,loc,dflag);

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     :  int um_feaangle(strprefix,angle,pos,loc,dflag)
**      Convert a "angle" feature to the current output units for
**			angle, display the feature, and put the value into the
**			feature table.
**    PARAMETERS   
**       INPUT  : 
**          strprefix			text for the feature
**          angle					value of the angle feature to put in table 
**          pos					position of the text of the feature
**          loc					location of the leader line for the feature
**		    	dflag					UU_TRUE => draw leader line
**										UU_FALSE => don't draw leader line
**       OUTPUT :  
**          output: none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
um_feaangle(strprefix,angle,pos,loc,dflag)
   Gchar *strprefix;
	UU_REAL angle;
   UU_REAL pos[];
   UU_REAL loc[];
   UU_LOGICAL dflag;

	{
	UU_REAL dspval;
	char str[256];
	char strvalue[20];
	char strsuffix[4];

	uu_denter(UU_MTRC,(us,"um_feaangle(%s, %g, %x, %x, %d)", 
		strprefix, angle, pos, loc, dflag));

	/* deterimine the string representing the value of the feature */
	dspval = angle;
	if (UM_cpln.angle_unit == UM_DEGR)
		dspval = (180.0/UM_PI) * dspval;
	um_fearealstring(dspval, strvalue);

	/* determine the text line suffix */
	switch (UM_cpln.angle_unit)
		{
		case UM_DEGR:
			strcpy(strsuffix,"deg");
			break;
		case UM_RADI:
			strcpy(strsuffix,"rad");
			break;
		default:
			strcpy(strsuffix,"");
			break;
		}

	/* compose the text for the feature and add to the real feature table */
	sprintf(str,"%s%s %s",strprefix, strvalue, strsuffix);
	return um_feareal(1,str,dspval,pos,loc,dflag);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     :  int um_feareal(realtype,string,value,pos,loc,dflag)
**      add a real feature to the real features table
**    PARAMETERS   
**       INPUT  : 
**				realtype				0 => length; 1 => angle
**          string				text for the feature
**          value					value of the real feature to put in table 
**          pos					position of the text of the feature
**          loc					location of the leader line for the feature
**		    	dflag					UU_TRUE => draw leader line
**										UU_FALSE => don't draw leader line
**       OUTPUT :  
**          output: none
**    RETURNS      : none
**    SIDE EFFECTS :
**				adds the real feature to the features
**          tables which are included in featcom.h
**          and draw the feature on the display device
**    WARNINGS     : none
*********************************************************************/
um_feareal (realtype,string,value,pos,loc,dflag)
	int realtype;
   Gchar *string; /* the string identifier for the feature */
	UU_REAL value; /* real feature value */
   UU_REAL pos[]; /* the position of the feature */
   UU_REAL loc[]; /* the location of the leader line for the feature */
   UU_LOGICAL dflag; /* draw or don't draw the leader line */

	{
   int cur_type; /* current GKS marker type */
   UU_REAL lead[6]; /* the coordinates of the leader line */
   int pick_id; /* the local offset GKS pick id number */

	uu_denter(UU_MTRC,(us,"um_feareal(%s, %g, %x, %x, %d)", 
											string, value, pos, loc, dflag));

	if (UM_F_real_count < UM_MAXREALF) /* enough room in real table */
		{
      /* offset the GKS pick id for the real range */
		pick_id = UM_F_real_count + UM_REALMIN;
		gspickid(pick_id);

		/* check flag for leader line display */
		if (dflag == UU_TRUE)
			{
			/* save the current GKS marker type */
			cur_type = gqmarktype();

			/* set the marker type to O (4) */
			gsmarktype(4);

			/* display a GKS marker at the leader location position */
			gpolymarker3(1,loc);

			/* restore the current GKS marker type */
			gsmarktype(cur_type);

			/* put the text position and the leader line location in lead */
			um_vctovc(loc,lead);
			um_vctovc(pos,&lead[3]);

			/* draw the leader line */
			gpolyline3 (2,lead);
			}

		/* write the label text */
		gtext (pos, string);

		/* update number of real features in feature table */
		sprintf(UM_sbuf,"feareal: pick_id=%d count=%d value=%f",
			pick_id,UM_F_real_count,value);
		um_pscroll(UM_sbuf);
		UM_F_real[UM_F_real_count] = value;
		UM_F_real_count ++;
		}
	else
	{
		uu_uerror0(UU_FEATERROR, 12);
		return UU_FAILURE;
	}
	return UU_SUCCESS;
	uu_dexit;

}
