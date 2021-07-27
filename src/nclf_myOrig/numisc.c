/*********************************************************************
**    NAME         :  numisc.c
**       CONTAINS: User interface routines for MISC subsystem
**			nclu_refsys() : in nclc/nurefsys.c
**			nclu_zsurf() : in nclc/nuzsurf.c
**          nclu_genpts()
**          nclu_can()
**          nclu_dist()
**          nclu_dot()
**          nclu_func()
**			nclu_num()
**
**    MODULE NAME AND RELEASE LEVEL
**       numisc.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:09
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mdcoord.h"
#include "modef.h"

#include "nccs.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "nclcmd.h"
#include "nclmodals.h"
#include "nclfc.h"

#include "ncldef.h"
#include "modef.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"

static int geotyp = 0;
static int vectyp = 0;
static int ptyp = 0;
static int cladj = 0;

/*********************************************************************
**
**    I_FUNCTION         :  gdstog()             
**
*********************************************************************/
static UD_FSTAT  gdstog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case 0: 
/*
.....Enable correct fields
.....based on ,Geometry Type field value
*/
/*
.....Vector Type checkbox enabled for Point Vectors but not Point
*/
		ud_set_traverse_mask(1,geotyp==1);
		if (geotyp == 0)
		{
			vectyp = 0;
			ud_update_answer(1,&vectyp);
		}
/*
.....Forward axis vector options enabled for points and pointvectors when vector type is of both axis.
*/
		ud_set_traverse_mask(5, geotyp == 0 ||
			(geotyp == 1 && vectyp != 0 ));
		ud_set_traverse_mask(6, geotyp == 0 ||
			(geotyp == 1 && vectyp != 1));
		break;
/*
..... Enable correct field 
..... based on ,Vector Type field value
*/
	case 1:
      ud_set_traverse_mask(5, geotyp == 0 ||
         (geotyp == 1 && vectyp != 0 ));
      ud_set_traverse_mask(6, geotyp == 0 ||
         (geotyp == 1 && vectyp != 1));
		break;
	default:
		break;
    }
	return(stat);
}


/*********************************************************************
**    S_FUNCTION     :  static OnText(filedno, val, stat)
**       Method called at when input in form's text field
**		it must be a text, can't be a value
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnText(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
	char tempstr[256];
	UU_REAL value;
	strcpy(tempstr, val->frmstr);
	status = ncl_get_scalar_value(tempstr, &value);
/*
.....it is number, not a scalar or string, stat=1, scalar label, =-1, not defined
.....scalar string
*/
	if (status==0)
	{
		if  (*fieldno==2)
			ud_wrerr("Invalid input. Can't be a value");
		else
			ud_wrerr("Invalid input, enter the Geometry Label");
		return UD_BADREQ;
	}
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : OnVideo(fieldno,val,stat)
**       Method called when an Video button is pressed.
**		Video button will play default video file, but we can change the file
**		doing nothing now exepect play default file, but put here in case we need it later
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnVideo(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : nclu_genpts(option)
**       description
**    PARAMETERS   
**       INPUT  : 
**          option					0 => prompt the user
**									1 => set genpts to nomore
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_genpts(option)
int option;
	{
	NCL_cmdbuf cmdbuf;
	int status, scal, stat;
	UU_REAL value;
/*
.....Set up form fields
*/
   static char traverse[]     = {1,0,1,1,1,0,0,1,1};
   static UD_METHOD methods[] = {gdstog,gdstog,UU_NULL,OnText,OnText,
                        OnText,OnText,UU_NULL,OnVideo};
   static char called[]       = {6,6,6,6,6,6,6,6,6};
   static char numscalar[22] = {0}, geolab[22] = {0}, fwdvec[22] = {0}, tlaxsvec[22] = {0};
   static int *ans[] = {&geotyp, &vectyp, &ptyp, (int *)&numscalar,
		(int *)&geolab,(int *)&fwdvec, (int*)&tlaxsvec,&cladj, UU_NULL};

   uu_denter(UU_MTRC,(us,"nclu_genpts(option=%d)", option));
/*
.....Initialize NCL_comand_buffers
*/
   ncl_init_cmdbuf(&cmdbuf);

   ncl_add_token(&cmdbuf, NCL_genpts, NCL_nocomma);
	if(option ==0) goto nomore;
	if(option == 1) 
		geotyp = 0;
	else 
		if(option == 2)
			geotyp = 1;
		
/*
.....Set traverse mask.
*/
/*
..... Vector type combo box is enabled only if geotype is pointvec.
*/
	traverse[1] = (geotyp != 0);
/*
.....Forward vector is enabled for points and for point vectors when
.....vector type is forward or both.
*/
	traverse[5] = (geotyp ==0) || ((geotyp ==1) && (vectyp !=0));

/*
.....Tool axis vector is enabled for points and for point vectors when
.....vector type is Tlaxis or both.
*/
   traverse[6] = (geotyp ==0) || ((geotyp ==1) && (vectyp !=1));
again:;
/*
.....Get the Form input
*/
   status =
      ud_form1("genpts.frm", ans, ans, methods, called, UU_NULL, traverse);
	if(status ==-1)
	{
		numscalar[0] = '\0';
		geolab[0] = '\0';
		fwdvec[0] = '\0';
		tlaxsvec[0] = '\0';
		goto done;
	}
/*
.....Format the tool axis command.
*/
/*
.....Initialize NCL_comand_buffers
*/
   ncl_init_cmdbuf(&cmdbuf);

   ncl_add_token(&cmdbuf, NCL_genpts, NCL_nocomma);
/*
.....Enter the vector type
*/
	if(geotyp)
	{
		ncl_add_token(&cmdbuf, NCL_pntvec, NCL_comma);
		if (vectyp == 0) 
			ncl_add_token(&cmdbuf,"1",NCL_comma);
		else 
			ncl_add_token(&cmdbuf,"2",NCL_comma);
	}
/*
.....Enter point generation type
*/
	if (ptyp == 1)
		ncl_add_token(&cmdbuf,NCL_arc,NCL_comma);
	else if (ptyp == 2)
		ncl_add_token(&cmdbuf,NCL_ps,NCL_comma);
	else if (ptyp == 3)
		ncl_add_token(&cmdbuf,NCL_ds,NCL_comma);
/*
..... enter a scalar variable
*/
	scal =strlen(numscalar);
	if (scal) ul_strip_blanks(numscalar,&scal);

	if (scal)
	{
		stat = ncl_get_scalar_value(numscalar, &value);
		if (stat==0)
		{
			ud_wrerr("Invalid input. Can't be a value");
		}
	}
	if (scal)
   		ncl_add_token(&cmdbuf, numscalar, NCL_comma);
	else
	{
		ud_wrerr("Invalid input, enter the Number of Entities Scalar.");
		goto again;
	}

/*
.....enter a reserved point array 	
*/
	scal =strlen(geolab);
	if (scal) ul_strip_blanks(geolab,&scal);

	if (scal)
		ncl_add_token(&cmdbuf, geolab, NCL_comma);
	else
	{
		ud_wrerr("Invalid input, enter the Geometry Label");
		goto again;
	}
/* 
.....enter a reserved direction array - OPTIONAL 
*/
	if((geotyp != 1 ) || (vectyp != 0))
	{
      scal =strlen(fwdvec);
      if (scal) ul_strip_blanks(fwdvec,&scal);

      if (scal)
         ncl_add_token(&cmdbuf, fwdvec, NCL_comma);
	}
/* 
.....enter a reserved tool axis array - OPTIONAL
 */
	if((geotyp != 1 ) || (vectyp != 1))
	{
     	scal =strlen(tlaxsvec);
      if (scal) ul_strip_blanks(tlaxsvec,&scal);

      if (scal)
         ncl_add_token(&cmdbuf, tlaxsvec, NCL_comma);
	}
/*
.....Enter NEXT
*/
	if (cladj == 1)
		ncl_add_token(&cmdbuf, "NEXT", NCL_comma);

nomore:;
	if(option == 0)
   	status = ncl_add_token(&cmdbuf, NCL_nomore, NCL_nocomma);
		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
done:;	
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_can()
**       description
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_can()
	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_can()"));

	ncl_init_cmdbuf(&cmdbuf);

	/* enter a scalar variable */
	status = ncl_add_str(&cmdbuf, 234, NCL_nocomma);
	if (strlen(cmdbuf.cur_str) == 0)
		status = NCL_NOINPUT;

	/* add =CAN( */
	if (status == NCL_OKINPUT)
		ncl_add_token(&cmdbuf, NCL_can, NCL_nocomma);

	/* pick geometry */
	if (status == NCL_OKINPUT)
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 238, UD_ncl_canonical);

	/* Enter a scalar value */
	if (status == NCL_OKINPUT)
		status = ncl_add_str(&cmdbuf, 223, NCL_nocomma);
	
	if (status == NCL_OKINPUT)
		status = ncl_add_token(&cmdbuf, ")", NCL_nocomma);

	if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_dist()
**       description
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_dist()
	{
	NCL_cmdbuf cmdbuf;
	int status;
	int rel;

	uu_denter(UU_MTRC,(us,"nclu_dist()"));

	ncl_init_cmdbuf(&cmdbuf);

	/* enter a scalar variable */
	status = ncl_add_str(&cmdbuf, 234, NCL_nocomma);
	if (strlen(cmdbuf.cur_str) == 0)
		status = NCL_NOINPUT;

	/* add DIST */
	if (status == NCL_OKINPUT)
		ncl_add_token(&cmdbuf, NCL_dist, NCL_nocomma);

	/* pick geometry */
	if (status == NCL_OKINPUT)
		status = ncl_add_label_rel(UD_DASPCKLOC, 
			&cmdbuf, 238, UD_ncl_dist, &rel);

	if (status == NCL_OKINPUT)
		switch (rel)
			{
			case NCL_POINTVEC_REL:
			case NCL_POINT_REL:
			case UM_POINT_REL:
				status = ncl_add_label(UD_DASPCKLOC, 
					&cmdbuf, 238, UD_ncl_ptplsf);
				break;

			case NCL_LINE_REL:
			case UM_LINE_REL:
				status = ncl_add_label(UD_DASPCKLOC, 
					&cmdbuf, 238, UD_ncl_pvln);
				break;

			case NCL_PLN_REL:
				status = ncl_add_label(UD_DASPCKLOC,
					&cmdbuf, 238, UD_ncl_ptpl);
				break;

			case NCL_CURVE_REL:
			case UM_AGCRV_REL:
				break;
			}

	if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_dot()
**       description
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_dot()
	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_dot()"));

	ncl_init_cmdbuf(&cmdbuf);

	/* enter a scalar variable */
	status = ncl_add_str(&cmdbuf, 234, NCL_nocomma);
	if (strlen(cmdbuf.cur_str) == 0)
		status = NCL_NOINPUT;

	/* add =DOT( */
	if (status == NCL_OKINPUT)
		ncl_add_token(&cmdbuf, NCL_dot, NCL_nocomma);

	/* pick geometry */
	if (status == NCL_OKINPUT)
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 218, UD_ncl_vepv);

	if (status == NCL_OKINPUT)
		status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 218, UD_ncl_vepv);

	if (status == NCL_OKINPUT)
		{
		cmdbuf.cur_str[strlen(cmdbuf.cur_str)-1] = '\0';
		status = ncl_add_token(&cmdbuf, ")", NCL_nocomma);

		}

	if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : nclu_funct(option)
**       description
**    PARAMETERS   
**       INPUT  : option where  		0: function = sqrt
**										1: function = log
**										2: function = abs
**										3: function = sin
**										4: function = asin
**										5: function = cos
**										6: function = acos
**										7: function = tan
**										8: function = atan
**										9: function = atan2
**									   10: function = int
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_funct(option)
int option;
	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_function()"));

	ncl_init_cmdbuf(&cmdbuf);

	/* enter a scalar variable */
	status = ncl_add_str(&cmdbuf, 234, NCL_nocomma);
	if (strlen(cmdbuf.cur_str) == 0)
		status = NCL_NOINPUT;

	switch (option)
		{
		case 0:	/* SQRT */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_sqrt, NCL_nocomma);
			break;
		case 1:	/* LOG */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_log, NCL_nocomma);
			break;
		case 2:	/* ABS */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_abs, NCL_nocomma);
			break;
		case 3:	/* SIN */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_sin, NCL_nocomma);
			break;
		case 4:	/* ASIN */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_asin, NCL_nocomma);
			break;
		case 5:	/* COS */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_cos, NCL_nocomma);
			break;
		case 6:	/* ACOS */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_acos, NCL_nocomma);
			break;
		case 7:	/* TAN */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_tan, NCL_nocomma);
			break;
		case 8:	/* ATAN */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_atan, NCL_nocomma);
			break;
		case 9:	/* ATAN2 */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_atan2, NCL_nocomma);
			break;
		case 10:	/* INT */
			if (status == NCL_OKINPUT)
				ncl_add_token(&cmdbuf, NCL_int, NCL_nocomma);
			break;
		}

	/* added to fix atan2. kathy */
	if (status == NCL_OKINPUT && option == 9)
		status = ncl_add_str(&cmdbuf, 223, NCL_comma);

	/* enter scalar expression */
	if (status == NCL_OKINPUT)
		status = ncl_add_str(&cmdbuf, 223, NCL_nocomma);

	if (status == NCL_OKINPUT)
		status = ncl_add_token(&cmdbuf, ")", NCL_nocomma);

	if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : nclu_num()
**       description
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_num(arg)
int arg;		/* 0 for RESERVE; 1 for PATERN */
	{
	NCL_cmdbuf cmdbuf;
	int status;

	uu_denter(UU_MTRC,(us,"nclu_num()"));

	ncl_init_cmdbuf(&cmdbuf);

	/* enter a scalar variable */
	status = ncl_add_str(&cmdbuf, 234, NCL_nocomma);
	if (strlen(cmdbuf.cur_str) == 0)
		status = NCL_NOINPUT;

	/* add =NUM( */
	if (status == NCL_OKINPUT)
		ncl_add_token(&cmdbuf, NCL_num, NCL_nocomma);

	/* get reserv_variable or PATERN label */
	if (status == NCL_OKINPUT)
		switch (arg)
			{
			case 0: 	/* RESERVE */
				status = ncl_add_reserv(UD_DASPCKLOC, &cmdbuf,
					239, UD_ncl_geometry);
				break;
			case 1: 	/* PATERN */
				status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 
					450, UD_ncl_patern);
				break;
			}

	if (status == NCL_OKINPUT)
		{
		/* get rid of comma added by ncl_add_label() */
		cmdbuf.cur_str[strlen(cmdbuf.cur_str)-1] = '\0';

		status = ncl_add_token(&cmdbuf, ")", NCL_nocomma);
		}

	if ((status == NCL_OKINPUT) || (status == NCL_DONE))
		{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}

	uu_dexit;
	}

