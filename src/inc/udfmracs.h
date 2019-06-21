
/*************************************************************************
**		NAME	:		udfmracs.h
**			Contains:	This module contains all read access macros for the
**					form structure defined in udforms.h. In the following list,
**					those definitions prefixed with a star are not yet
**					implemented:
**
**						name:		implied type:	description/purpose:
**					-----------------------------------------------------------
**					ud_setfs		---				imports ptr to form-struct for
**														access routines
**					ud_getfwllx	UU_REAL				returns ndc lower left x-coord
**					ud_getfwlly	UU_REAL				returns ndc lower left y-coord
**					ud_getfwurx	UU_REAL				returns ndc upper right x-coord
**					ud_getfwury	UU_REAL				returns ndc upper right y-coord
**					ud_getfpgc	int				returns window bkgrnd color index
**					ud_getftxc	int				returns window text color index
**				*	ud_getftxf	int				returns window text font index
**				*	ud_getftxs	UU_REAL				returns window text size
**					ud_getfmno	int				returns number of dsply msgs in form
**					ud_getfmsg	char *			returns ptr to selected msg string
**					ud_getffno	int				returns no. of fields in form struct
**				*	ud_getffld	int				returns field ordinal no of field
**					ud_getflcr	int				returns start row of selected field
**					ud_getflcc	int				returns start col of selected field
**					ud_getflpcr	int				returns start row of prompt
**					ud_getflpcc	int				returns start col of prompt
**					ud_getfft	int				returns field type of selected field
**					ud_getffdt	int				returns data type of selected field
**					ud_getffmf	short				returns mandatory flag value of field
**					ud_getffef	short				returns echo flag value of field
**					ud_getfprec	int				returns field precision of field
**					ud_getflen	int				returns field length (max) of field
**					ud_getffp	char *			returns ptr to prompt of field
**				*	ud_getfm		char *			returns ptr to mask string of field
**					ud_getffrl	UD_DASIN *		returns ptr to field low range limit
**					ud_getffrh	UD_DASIN *		returns ptr to field high range limit
**					ud_getfdix	int				returns index to default array or -1
**					ud_getffd	UD_dpiece *		returns ptr to field default value
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       udfmracs.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:03
**************************************************************************/

#ifndef UDFMRACSH

#include <stdio.h>
#include "udfrmdec.h"

/*******************************************************************
**
**		E_FUNCTION	:		ud_setfs(fs)
**								UD_FSTRUCT *fs;
**
**			Synopsis:	sets local static form-struct pointer equal to the
**					the form-struct pointer passed in. This is so that that same
**					pointer doesn't have to be passed all over the place in order
**					to use these access routines.
**
**			Parameters	:
**				Input		:
**					fs		:	pointer to the current active form-struct
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_setfs(fs) UD_frm=fs				/* note that 'frm' MUST be defined */
				
									/* in src of forms package as static*/
/*******************************************************************
**
**		E_FUNCTION	:		UU_REAL ud_getfwllx()
**
**			Synopsis:	returns ndc x-coord of lower left corner of form
**					window in current fstruct
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				The value of the implied function is of type UU_REAL
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getfwllx() (UD_frm->ud_frmdf.ud_fwin.ll.x)


/*******************************************************************
**
**		E_FUNCTION	:		UU_REAL ud_getfwlly()
**
**			Synopsis:	returns ndc y-coord of lower left corner of form
**					window in cuurent fstruct
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				The value of the implied function is of type UU_REAL
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getfwlly() (UD_frm->ud_frmdf.ud_fwin.ll.y)


/*******************************************************************
**
**		E_FUNCTION	:		UU_REAL ud_getfwurx()
**
**			Synopsis:	returns ndc x-coord of upper right corner of form
**					window in current fstruct
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				The value of the implied function is of type UU_REAL
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getfwurx() (UD_frm->ud_frmdf.ud_fwin.ur.x)


/*******************************************************************
**
**		E_FUNCTION	:		UU_REAL ud_getfwury()
**
**			Synopsis:	returns ndc y-coord of upper right corner of form
**					window in current fstruct
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				The value of the implied function is of type UU_REAL
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getfwury() (UD_frm->ud_frmdf.ud_fwin.ur.y)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getfpgc()
**
**			Synopsis:	returns page color index of current form's window
**					definition.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				The value of the implied function is of type int
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getfpgc() (UD_frm->ud_frmdf.ud_pgcolr)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getftxc()
**
**			Synopsis:	returns text color index of current form's
**					attributes specification
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				The value of the implied function is of type int
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getftxc() (UD_frm->ud_frmdf.ud_txcolr)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getftxf()
**
**			Synopsis:	returns text font index of cuurent form's
**					attributes specification
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				The value of the implied function is of type int
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getftxf() (UD_frm->ud_frmdf.ud_txfont)


/*******************************************************************
**
**		E_FUNCTION	:		UU_REAL ud_getftxs()
**
**			Synopsis:	returns text character height of current form's
**					attributes specification
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				The value of the implied function is of type UU_REAL
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getftxs() (UD_frm->ud_frmdf.ud_txsiz)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getfmno()
**
**			Synopsis:	returns the number of fieldless display messages
**					in the current form struct, of -1 if none
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				The value of the implied function is of type int
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getfmno() ((UD_frm->n_display_fields== 0)?\
							(-1):\
							UD_frm->n_display_fields)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getffno()
**
**			Synopsis:	returns the number of fields defined in the 
**					current form structure, or -1 if none
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**				The value of the implied function is of type int.
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getffno() (UD_frm->n_input_fields)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getflpcr(fno)
**								int fno;
**
**			Synopsis:	returns the start row of indicated field's
**				prompt string
**
**			Parameters	:
**				Input		:
**					fno	:	int field number
**				Output	:
**			Returns		:
**					The value of the implied function is of type int.
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getflpcr(fno) (UD_frm->input_flds[fno].ud_prmloc.ud_r)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getflpcc(fno)
**								int fno;
**
**			Synopsis:	returns the start col of indicated field's
**				prompt string
**
**			Parameters	:
**				Input		:
**					fno	:	int field number
**				Output	:
**			Returns		:
**					The value of the implied function is of type int.
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getflpcc(fno) (UD_frm->input_flds[fno].ud_prmloc.ud_c)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getfft(fno)
**								int fno;
**
**			Synopsis:	returns the field type of the selected field
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**					The value of the implied function is of type short
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getfft(fno)	(UD_frm->input_flds[fno].toggle)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getflcr(fno)
**
**			Synopsis:	returns the start row of the specified field
**					in the current form struct (entry area)
**
**			Parameters	:
**				Input		:
**					fno	:	field number
**				Output	:
**			Returns		:
**				The value of the implied function is of type int
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getflcr(fno) (UD_frm->input_flds[fno].ud_fldloc.ud_r)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getflcc(fno)
**
**			Synopsis:	returns the start column of the specified field
**					in the current form struct (entry area)
**
**			Parameters	:
**				Input		:
**					fno	:	field number
**				Output	:
**			Returns		:
**				The value of the implied function is of type int
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getflcc(fno) (UD_frm->input_flds[fno].ud_fldloc.ud_c)


/*******************************************************************
**
**		E_FUNCTION	:		short ud_getffdt(fno)
**
**			Synopsis:	returns the data type of the selected field in
**					the current form struct
**
**			Parameters	:
**				Input		:
**					fno	:	index of desired field
**				Output	:
**			Returns		:
**				The value of the implied function is of type short
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getffdt(fno) (UD_frm->input_flds[fno].ud_datatyp)


/*******************************************************************
**
**		E_FUNCTION	:		short ud_getffmf(fno)
**
**			Synopsis:	returns the mandatory flag of the selected field
**					in the current form struct
**
**			Parameters	:
**				Input		:
**					fno	:	index of desired field
**				Output	:
**			Returns		:
**				The value of the implied function is of type short
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getffmf(fno) (UD_frm->input_flds[fno].ud_mndflg)


/*******************************************************************
**
**		E_FUNCTION	:		short ud_getffef(fno)
**
**			Synopsis:	returns the echo flag of the selected field in
**					the current form struct
**
**			Parameters	:
**				Input		:
**					fno	:	index of desired field
**				Output	:
**			Returns		:
**				The value of the implied function is of type short
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getffef(fno) (UD_frm->input_flds[fno].ud_echo)


/*******************************************************************
**
**		E_FUNCTION	:		int ud_getfprec(fno)
**								int fno;
**
**			Synopsis:	returns the field precision of field fno. this
**						is the number of digits right of the decimal, or
**						the maximum string length.
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getfprec(fno) (UD_frm->input_flds[fno].ud_fprec)


/*******************************************************************
**
**		E_FUNCTION	:		int ud_getflen(fno)
**								int fno;
**
**			Synopsis:	returns the min field length of field fno
**
**			Parameters	:
**				Input		:
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getflen(fno) (UD_frm->input_flds[fno].ud_flen)


/********************************************************************
**
**    E_FUNCTION  :     char *ud_getfmsg(ix)
**                      int ix;
**
**       Synopsis:   returns a pointer to the ix-th message
**             in prompt storage of curent form struct.
**
**       Parameters  :
**          Input    :
**          Output   :
**       Returns     :
**          The value of the implied function is of type char *.
**       Side Effects:
**       Warnings    :
********************************************************************/
#define ud_getfmsg(ix) \
	(UD_frm->display_flds[ix].message)


/*******************************************************************
**
**    E_FUNCTION  :     UD_DASIN *ud_getffd(fno,i))
**                      int fno;
**                      int i;
**
**       Synopsis:   returns a pointer to the default value of the
**             indexed field of the current form struct
**
**       Parameters  :
**          Input    :
**             fno   :  index of desired field
**          Output   :
**       Returns     :
**          The value of the implied function is of type UD_DASIN *
**             is = to loc of default, or null
**       Side Effects:
**       Warnings    :
********************************************************************/

#define ud_getffd(fno,i)  ((UD_frm->input_flds[fno].defaults == (UD_DASIN *)0) ? \
								((UD_DASIN *)0):&(UD_frm->input_flds[fno].defaults[i]))


/*******************************************************************
**
**		E_FUNCTION	:		UD_DASIN *ud_getffrl(fno)
**
**			Synopsis:	returns a pointer to the low boundary of the range
**					specification of the indexed field in the current form
**					struct
**
**			Parameters	:
**				Input		:
**					fno	:	index of desired field
**				Output	:
**			Returns		:
**				The value of the implied function is of type UD_DASIN *
**					is = to loc of low range boundary, or null
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getffrl(fno) ((UD_frm->input_flds[fno].range_flag) ? \
							&(UD_frm->input_flds[fno].range[0]):((UD_DASIN *)0))



/*******************************************************************
**
**		E_FUNCTION	:		UD_DASIN *ud_getffrh(fno)
**
**			Synopsis:	returns a pointer to the high boundary of the range
**					specification of the indexed field in the current form
**					struct
**
**			Parameters	:
**				Input		:
**					fno	:	index of desired field
**				Output	:
**			Returns		:
**				The value of the implied function is of type UD_DASIN *
**					is = to loc of high range boundary, or null
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getffrh(fno) ((UD_frm->input_flds[fno].range_flag) ? \
							&(UD_frm->input_flds[fno].range[1]):((UD_DASIN *)0))


/*******************************************************************
**
**		E_FUNCTION	:		char *ud_getffp(fno)
**
**			Synopsis:	returns a pointer to the prompt string associated
**					with the given field number in the current form struct.
**					If there is no prompt, returns 0 (NULL)
**
**			Parameters	:
**				Input		:
**					fno	:	index of desired field
**				Output	:
**			Returns		:
**				The value of the implied function is of type char *:
**					valid char *, or , if no prompt, null
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getffp(fno) UD_frm->input_flds[fno].prompt


/*******************************************************************
**
**		E_FUNCTION	:		ud_getfmr(ix)
**
**			Synopsis:	returns the start row of the indexed message
**					number in current form struct
**
**			Parameters	:
**				Input		:
**					ix		:	index of message
**				Output	:
**			Returns		:
**					the value of the implied function is of type int
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getfmr(ix) (UD_frm->display_flds[ix].pos.ud_r)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getfmc(ix)
**
**			Synopsis:	returns the start column of the indexed message
**					number in current form struct
**
**			Parameters	:
**				Input		:
**					ix		:	index of message
**				Output	:
**			Returns		:
**					the value of the implied function is of type int
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_getfmc(ix) (UD_frm->display_flds[ix].pos.ud_c)


/*******************************************************************
**
**		E_FUNCTION	:		ud_getndef(fno)
**
**			Synopsis:	returns the number of defaults for field 'fno'
**
**			Parameters	:
**				Input		:
**					ix		:	index of field
**				Output	:
**			Returns		:
**					the value of the implied function is of type int
**			Side Effects:
**			Warnings		:
********************************************************************/
#define ud_getndef(fno) (UD_frm->input_flds[fno].n_defaults)

/*******************************************************************
**
**		E_FUNCTION	:		ud_setfdata(fdata)
**								UD_FDATA *fdata;
**
**			Synopsis:	sets local static form-struct pointer equal to the
**					the form-struct pointer passed in. This is so that that same
**					pointer doesn't have to be passed all over the place in order
**					to use these access routines.
**
**			Parameters	:
**				Input		:
**					fdata		:	pointer to the current active form data struct
**				Output	:
**			Returns		:
**			Side Effects:
**			Warnings		:
********************************************************************/

#define ud_setfdata(fdata) UD_fdata=fdata				/* note that 'UD_fdata' MUST be defined */
				
/*
mod_method

mod disp mask

mod trav mask

set single method

set single disp mask

set single trav mask

set range

set val

update form
*/

#define UDFMRACSH
#endif
