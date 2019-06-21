/*********************************************************************
**
**    NAME         :  d4ckdat.c
**
**       CONTAINS:
**				UD_FSTAT ud_ckdata(de,fno)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d4ckdat.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:08
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "zsysdep.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfrmdec.h"
#include "udfmracs.h"
#include "ddef.h"

#define UD_FORMFUZZ (UU_REAL) 1.0e-6

/*********************************************************************
**    I_FUNCTION :   UD_FSTAT ud_ckdata(fstruct, de,fno)
**          ud_ckdata does data-type appropriate bounded range checks
**          on the data element pointed to. returns value-ok,
**          value-bad, and type-bad status depending on result.
**          Eventually will do mask-format checking also.
**          If field is a choice type, check against the set of legal 
**          values.
**
**    PARAMETERS   
**       INPUT  : 
**				fstruct: form structure used to check data
**          de  : pointer to UD_DASIN data element
**          fno : field number
**       OUTPUT :  
**          none
**    RETURNS      :
**          The value of the function is of type UD_FSTAT:
**             UD_VALOK - data passed all checks
**             UD_BADRNG - data out of range
**             UD_BADTYP - data was not an allowed type
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
......this function changed because we no long only
......use one iset of global form structure
......Yurong
*/
UD_FSTAT ud_ckdata(fstruct, de,fno)                /* data checker */
UD_FSTRUCT fstruct;
UD_DASIN *de;                             /* data element ptr */
int fno;                                  /* field number */
{
	UD_DASIN *lptr, *hptr;                 /* lo/hi range element ptrs */
	UD_DASIN low, high;                    /* the actual range lim.s */
	int dtyp;                              /* data type */
	UD_FSTAT stat;                         /* return status */
	int ix;                                /* local cntr, index */
	char erms[132];
	UD_DASIN *defpt;

	uu_denter(UU_DTRC,(us,"entering ud_ckdata: das data ptr=%x, field no=%d",
					de, fno));

/*
......get range bounds
*/
/*
......if there, get low range
*/
	lptr = (fstruct.input_flds[fno].range_flag) ? 
					&(fstruct.input_flds[fno].range[0]):((UD_DASIN *)0);
	if(lptr != 0)
		zbytecp(low,*lptr);
/*
......similarly, high range
*/
	hptr = (fstruct.input_flds[fno].range_flag) ? 
					&(fstruct.input_flds[fno].range[1]):((UD_DASIN *)0);
	if (hptr != 0)
		zbytecp(high,*hptr);
/*
......not a choice type
*/
	if(fstruct.input_flds[fno].toggle != 2)
	{

/*
......do range checks according to data type
......if range specified, according to type
*/
		if((lptr!=0) && (hptr!=0))
		{
			dtyp = fstruct.input_flds[fno].ud_datatyp;
			switch (dtyp)
			{
			case UD_DASCHOICE:
			case UD_DASINT:
			case UD_SCAINT:
				if((de->dint < low.dint) || (de->dint>high.dint))
				{
					stat=UD_BADRNG;
					sprintf(erms, "%d is not within range %d to %d",
							de->dint, low.dint, high.dint);
					ud_wrerr(erms);
					break;
				}
				else
				{
					stat=UD_VALOK;
					break;
				}
/*
.....this is check data range, so we used scalar as value
*/
			case UD_DASSCALAR:
			case UD_SCADISTANCE:
			case UD_SCAUNITLESS:
			case UD_SCAANGLE:
			case UD_SCAVAL:
			case UD_DASDISTANCE:
			case UD_DASUNITLESS:
			case UD_DASANGLE:
			case UD_DASVAL:
				if((de->dreal<low.dreal - UD_FORMFUZZ) ||
						(de->dreal>high.dreal + UD_FORMFUZZ))
				{
					stat=UD_BADRNG;
					sprintf(erms,"%g is not within range %g to %g",
							de->dreal, low.dreal, high.dreal);
					ud_wrerr(erms);
				}
				else
				{
					stat=UD_VALOK;
				}
				break;
			case UD_SCACART:
			case UD_SCANDC:
			case UD_SCAVEC:
			case UD_DASCART:
			case UD_DASNDC:
			case UD_DASVEC:
				for(ix=0; ix<3; ix++)
				{
					if((de->cord.cord[ix] < low.cord.cord[ix] - UD_FORMFUZZ)
						|| (de->cord.cord[ix] > high.cord.cord[ix] + UD_FORMFUZZ))
					{
						stat=UD_BADRNG;
						sprintf(erms,
							"<%g,%g,%g> not in range <%g,%g,%g> to <%g,%g,%g>",
							de->cord.cord[0], de->cord.cord[1], de->cord.cord[2], 
							low.cord.cord[0], low.cord.cord[1], low.cord.cord[2], 
							high.cord.cord[0], high.cord.cord[1],high.cord.cord[2]);
						ud_wrerr(erms);
						break;
					}
					else
						stat=UD_VALOK;
				}
				break;

/*
......ignore select, pick - locator cases for now
*/
			case UD_DASSELECT:
			case UD_DASPCKLOC:   
				stat=UD_VALOK;
				break;
   
			case UD_DASSTRING:
				if((strcmp(de->dstr, low.dstr)<0) ||
							(strcmp(de->dstr, high.dstr)>0))
					{
						stat=UD_BADRNG;
						sprintf(erms,"\"%s\" not in range \"%s\" to \"%s\"",
								de->dstr, low.dstr, high.dstr);
						ud_wrerr(erms);
					}
					else
					{
						stat=UD_VALOK;
					}
					break;
			default:
				stat=UD_BADTYP;                     /* data type error */
				sprintf(erms,"Bad data type '%d' in field #%d",dtyp,fno);
				ud_wrerr(erms);
				break;
			}
		}
		else
			stat=UD_VALOK;
	}                          /* end of not a choice type */
/*
......is a choice type, check legal values which are contained 
......in this field's defaults
*/
	else
	{
		stat=UD_BADRNG;
		for (ix=0; ix<fstruct.input_flds[fno].n_defaults; ix++) 
		{
			defpt = (fstruct.input_flds[fno].defaults == (UD_DASIN *)0) ? 
								((UD_DASIN *)0):
								&(fstruct.input_flds[fno].defaults[ix]);
			if(strcmp(defpt->dstr, de->dstr) == 0)
			{
				stat = UD_VALOK;
				break;
			}
		}
	}
	ud_pstat(stat, "ud_ckdata");
	uu_dexit;
	return(stat);
}
/*********************************************************************
**    I_FUNCTION :  UD_FSTAT ud_scatodas(data,das,dtyp)
**			transfer data at *data (which is a union of pointers)
**			to *das. dtyp tells what kind of data it is. returns XFROK
**			or BADXFR if dtype was in error.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ud_scatodas(dsda, de, dtyp)
UD_DASDATA *dsda;
UD_DASIN *de;
int dtyp;
{
	int irtn = UU_TRUE;

	switch (dtyp)
	{
	case UD_SCAVEC:
		if (dsda->dtype != 2) goto failed;
		um_ccstomcs(1, dsda->stval.stcord.coord, de->cord.cord);
		break;
	case UD_SCACART:
		if (dsda->dtype != 2) goto failed;
		um_ccstomcs(0,dsda->stval.stcord.coord, de->cord.cord);
		break;
	case UD_SCANDC:
		if (dsda->dtype != 2) goto failed;
		de->cord.cord[0] = dsda->stval.stcord.coord[0];
		de->cord.cord[1] = dsda->stval.stcord.coord[1];
		de->cord.cord[2] = dsda->stval.stcord.coord[2];
		break;
	case UD_DASSCALAR:
	case UD_SCAVAL:
	case UD_SCAUNITLESS:
	case UD_SCADISTANCE:
	case UD_SCAANGLE:
		if (dsda->dtype != 1) goto failed;
		de->dreal = dsda->stval.dval;
		break;
	case UD_SCAINT:
		if (dsda->dtype != 1) goto failed;
		de->dint = (int)dsda->stval.dval;
		break;
	default:
		goto failed;
	}
	goto done;
failed:;
	irtn = UU_FALSE;
done:;
	return(irtn);
}
