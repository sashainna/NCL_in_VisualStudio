/*********************************************************************
**
**    NAME         :  d4fill.c
**
**       CONTAINS:
**       	ud_filall
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d4fill.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:08
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfrmdec.h"
#include "udfmracs.h"

#define STATNO 29
#define UD_MAXTOGGLE 99

extern int ud_curfldno;				/* curr field no. (see udfiopak) */
extern char ud_partial[80];		/* partial string saved by ud_frmgets */
extern char sfstat[STATNO][12];	/* UD_FSTAT strings: must match typedef enum */
extern int *dfltno;					/* default number of each field */

/*******************************************************************
**
**		I_FUNCTION	:		UD_FSTAT ud_filall(fd)
**
**			For each data element in the form-data structure,
**			ud_filall displays the default value of that element at the
**			coresponding field location, and ifno error occurred,
**			activates the field forinput. Appropriate status is
**			returned. This routine also builds a table of mandatory
**			fields, which is checked before it exits. It will request
**			the user to fill mandatory fields before exiting
**
**			PARAMETERS	:
**				INPUT		:
**					fd		: a pointer to a UD_FDATA variable
**				OUTPUT	:
**			RETURNS		:
**				The value of this function is of type UD_FSTAT:
**					UD_FDSYNC - no of form fields not equal no of data fields
**					UD_DFLTER - the default display errored out
**					UD_BADREQ - last field requested had undefined or invalid
**									data type specifiied
**					UD_FILLOK - the form has been filled, all data present, chk'd
**					UD_DONE - the "done" key was hit with no oustanding mandatory
**									fields
**					UD_ALT - the "user alternate action" key was hit
**			SIDE EFFECTS:
**					display of default may result in default values changing
**			WARNINGS		:
********************************************************************/

UD_FSTAT ud_filall(fd)						/* fill all fields routine */
UD_FDATA *fd;									/* pointer to data field struct */
{
	int ffnum, dfnum;							/* field numbers */
	int ix;										/* general use index */
	int iy;										/* likewise */
	short *mndf, *echf;						/* mandatory, echo tables */
	short *filled;								/* data filled table */
	UD_FSTAT stat;								/* function status return */
	int mndcnt;									/* count of mandatory fields */
	int ftyp;									/* field type */
	int i;
	int *p;										/* really points to answer string*/
	char tmpstr[200];
	UD_FSTAT ud_fildfl(), ud_actfld();

	uu_denter(UU_DTRC,(us,"entering ud_filall: ptr to data struct=%x", fd));

	/* -- first build local tables (ie-mandatory flags) -- */

	ffnum=ud_getffno();							/* get number of fields */
	dfnum=fd->ud_ndf;								/* and no of data elements */

	uu_dprint(UU_DTRC,(us,"ud_filall: no fields=%d  no elements=%d",
			ffnum, dfnum));

/* -- check formismatch -- */

	if(ffnum != dfnum)
	{
		ud_pstat(stat=UD_FDSYNC, "ud_filall");
		goto rtn;
	}

	mndcnt = 0;										/* init mand cnt */
	mndf = (short *) uu_malloc(dfnum * sizeof(short));
	echf = (short *) uu_malloc(dfnum * sizeof(short));
	filled = (short *) uu_malloc(dfnum * sizeof(short));

/* -- get mandatory and echo flags */

	for(ix=0; ix<dfnum; ix++)
	{
		mndf[ix]=ud_getffmf(ix);
		if(mndf[ix] != 0)
		{
			filled[ix] = 0;
			mndcnt++;
		}
		echf[ix] = ud_getffef(ix);
	}

/* -- initialize default numbers -- */

	for(ix=0; ix<dfnum; ix++) 
	{
		dfltno[ix] = 0;	

/* 	-- for each toggle field, set dfltno[ix] to the default specified
			in the fdata structure 

			get field type and it is a toggle field -- */

		ftyp = ud_getfft(ix);
		if(ftyp == 2) 
		{

/* 		-- fdata contains a default get pointer to fdata (integer) -- */

			if((*fd).ud_data[ix].dflg==1) 
			{
				p=(int *)(*fd).ud_data[ix].ud_delem.frmstr;

				uu_dprint(UU_DTRC,(us,"ud_filall. toggle default=%d", *p));
				if(((*p)<0) || ((*p)>UD_MAXTOGGLE)) 
				{
					uu_dprint(-1,(us,
						"ud_filall. error. toggle field %d default is %d. Using 0.",
						ix, *p));
					*p = 0;
				}
				dfltno[ix]= *p; 
			}										/* end iffdata contains a default */
		}											/* end if(ftyp==2) */
	}												/* end initialize default numbers */
	
/* -- next display default values -- */

	for(ix=0; ix<dfnum; ix++)					/* for each data element */
	{														/* ifthere is one */
		uu_dprint(UU_DTRC,(us,"ud_filall: dsply dflt fno=%d", ix));
		if((fd->ud_data[ix].dflg==1) ||
			(ud_getffd(ix, dfltno[ix])!=(UD_DASIN *)UU_NULL))
		{
			if(UD_frm->ud_display_mask[UD_frm->n_display_fields+ix] == 1)
				stat=
					ud_fildfl(ix, dfltno[ix], &(fd->ud_data[ix]));/* disp default */
		}
		else
			stat=UD_DFLTOK;

		ud_pstat(stat, "ud_filall");

		if(stat != UD_DFLTOK)
		{
			goto rtn;
		}
	}

	uu_dprint(UU_DTRC,(us,"ud_filall: now filling fields #%d to %d", 0, dfnum));

/* -- next go and fill data fields -- */

	stat=UD_FLDOK;
	do
	{

/* 	-- for each data element -- */

		for(ix=0; ix<dfnum; )
		{

/* 		-- get field type -- */

			ftyp = ud_getfft(ix);
			iy = ud_getndef(ix) - 1;
			uu_dprint(UU_DTRC,(us, "ud_filall:dflt lim=%d, ftyp=%d dfltno[%d]=%d",
				iy, ftyp, ix, dfltno[ix]));

/* 		-- if display field -- */
			if(ftyp == 0)
			{
				uu_dprint(UU_DTRC,(us,"ud_filall: fno %d was dsply only", ix));
				if(stat==UD_BAK) 
				{
					if(ix == 0)
						ix = dfnum - 1;
					else
						ix--;	/* decrement field no */
				}
				else 
				{
					if(ix == dfnum - 1)
						ix = 0;
		 			 else
						ix++;	/* or increment it */
				}
			}

/* 		-- else if an input field either data or choice -- */

			else if((ftyp==1) || (ftyp==2))
			{
				while((UD_frm->ud_display_mask[UD_frm->n_display_fields+ix] == 0) || 
						(UD_frm->traverse_mask[ix] == 0))/* If not vis or 
																		Not traversable	*/
				{
					if(stat == UD_BAK)
					{
						if(ix == 0)
							ix = UD_fdata->ud_ndf-1;
						else
							ix--;
					}
					else
					{
						if(ix == UD_fdata->ud_ndf - 1)
							ix = 0;
					 	else
							ix++; /* goto next field */
					}
				}

				if(UD_frm->input_flds[ix].method_returns & UD_ENTER)
					(*UD_frm->input_flds[ix].method)(&ix, &(fd->ud_data[ix].ud_delem),
						UD_ENTERF);

/* 			-- send current field number to iopak:
					get data, put in field -- */

				ud_curfldno = ix;
				stat = ud_actfld(ix, &(fd->ud_data[ix]));
				ud_pstat(stat, "ud_filall");

/* 			-- echo field contents -- */

				if((stat!=UD_TFWD) && (stat!=UD_TBAK)) 
				{

/* 				-- don't echo field contents iftoggle, because the call to
						ud_fildfl at end of switch below will do it after
						cycling to next choice -- */

					uu_dprint(UU_DTRC,(us,
						"ud_filall. not toggle. echo field contents" ));

					if((stat == UD_FLDOK) && (ud_getfft(ix) == 2))
					{
						uu_dprint(UU_DITRC,(us,"FLDOK: str = %s",
							fd->ud_data[ix].ud_delem.frmstr));
						dfltno[ix]= *fd->ud_data[ix].ud_delem.frmint;
					}

					ud_fildfl(ix, dfltno[ix], &(fd->ud_data[ix]));
					uu_dprint(UU_DTRC,(us,"ud_filall. leave loop ix=%d", ix));
				}
				
/* 			-- per return val, and key entered -- */

				switch(stat)
				{

/* 				-- field back key entered -- */

					case UD_BAK:
						if(UD_frm->input_flds[ix].method_returns & UD_EXIT)
							(*UD_frm->input_flds[ix].method)(&ix,
										&(fd->ud_data[ix].ud_delem), UD_BAK);
						continue;

/* 				-- data and cr entered -- */

					case UD_FLDOK:
						uu_dprint(UU_DTRC,(us,"ud_filall: switch UD_FLDOK, fldno=%d",
													ix));
						filled[ix] = 1;
						if(UD_frm->input_flds[ix].method_returns & UD_EXIT)
							(*UD_frm->input_flds[ix].method)(&ix,
									&(fd->ud_data[ix].ud_delem), UD_FLDOK);
						uu_dprint(UU_DITRC,(us,"	ret: UD_FLDOK, fldno = %d", ix));

						mndcnt--;
						continue;

/* 				-- field forward key entered -- */

					case UD_FWD:
						if(UD_frm->input_flds[ix].method_returns & UD_EXIT)
							(*UD_frm->input_flds[ix].method)(&ix, &(fd->ud_data[ix].ud_delem)
							,UD_FWD);
						continue;

/* 				-- 'done' key entered -- */

					case UD_DONE:
						ix=dfnum;									/* or escape ifdone */
						continue;									/* not marked "filled" */

/* 				-- only cr entered -- */

					case UD_PASSF:
						if(filled[ix]==0)
						{
							mndcnt--;
							filled[ix]=1;                    /* remember filled */
						}
						if(UD_frm->input_flds[ix].method_returns & UD_EXIT)
							(*UD_frm->input_flds[ix].method)(&ix, &(fd->ud_data[ix].ud_delem)
							,UD_PASSF);
						continue;

/* 				-- choice field toggle forward */

					case UD_TFWD:
						*(fd->ud_data[ix].ud_delem.frmint) = dfltno[ix];

						if(UD_frm->input_flds[ix].method_returns & UD_TOGGLE)
						{
							(*UD_frm->input_flds[ix].method)(&ix, &(fd->ud_data[ix].ud_delem)
							,UD_TFWD);
							dfltno[ix] = *(fd->ud_data[ix].ud_delem.frmint);
						}

						break;

/* 				-- choice field toggle backward */

					case UD_TBAK:
						*(fd->ud_data[ix].ud_delem.frmint) = dfltno[ix];
						if(UD_frm->input_flds[ix].method_returns & UD_TOGGLE)
						{
							(*UD_frm->input_flds[ix].method)(&ix, &(fd->ud_data[ix].ud_delem)
								,UD_TBAK);
							dfltno[ix] = *(fd->ud_data[ix].ud_delem.frmint);
						}
						break;

/* 				-- or blow out if err -- */

					default:
						ud_pstat(stat, "ud_filall");
						goto rtn;
				}									/* end of switch(stat) */

/* 			-- gets here ifstat is UD_TFWD or UD_TBAK. Call fildfl
					because frmgets no longer blanks the field -- */
			}						/* end ifan input field */
		}							/* end forix = each field */
	} while((mndcnt>0) || (stat != UD_DONE));

	ud_pstat(stat=UD_FILLOK, "ud_filall");

rtn: 
	if(((int)stat<=0)||((int)stat>=STATNO)) 
	{
		uu_dprint(UU_DTRC,(us,"ud_filall returns unknown stat %x",(int)stat));
	}
	else 
	{
		uu_dprint(UU_DTRC,(us,"ud_filall returns %s", &sfstat[(int)stat][0]));
	}

	uu_free(mndf);
	uu_free(echf);
	uu_free(filled);
	uu_dexit;
	return(stat);
}

/*********************************************************************
**    I_FUNCTION :  ud_dfindstr(ix, str)
**			find str in field ix's defautls.
**    PARAMETERS   
**       INPUT  :  ind ix -- field number.
**						 char *str -- string to look for.
**       OUTPUT :  
**    RETURNS      : numerical posn ifstr, or -1 if not there.
**    SIDE EFFECTS : prints error msg on trace file ifdoesn't find str.
**    WARNINGS     : none
*********************************************************************/

int ud_dfindstr(ix, str)
int ix;
char *str;
{
	int i, ixok;
	char us[120];

	ixok=0;
	for(i=0; i<ud_getndef(ix); i++) 
	{
		if(strcmp(ud_getffd(ix, i)->dstr, str) == 0) 
		{
			ixok=1;
			break;
		}
	}

/* -- panic error -- */

	if(ixok == 0) 
	{
		uu_dprint(-1,(us,"ud_dfindstr panic error. ix=%d, i=%d", ix, i));
		i= -1;
	}
	uu_dprint(UU_DTRC,(us,"%d=ud_dfindstr(%d,%s)", i, ix, str));
	return(i);
}
