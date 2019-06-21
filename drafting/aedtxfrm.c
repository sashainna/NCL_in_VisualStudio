/*********************************************************************
**    NAME         :  aedtxfrm.c
**       CONTAINS:
**			ua_edit_text_trvfrm					bring up edit text form
**			ua_edtx_addelb							add edit line after current
**			ua_edtx_rmelb							remove current edit line 
**			ua_edtx_rmallelb						remove entire edit line list
**			ua_edtx_modelb						modify elb line string 
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       aedtxfrm.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:34
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "adrfcom.h"
/* #include "adraft.h" */

#define NO_DEFAULT 0
#define DEFAULT 1
#define MAXLINELEN 60
#define MAXTYPELEN 16
#define UA_OPCOMPLETE 1
#define CLSEP '\n'
#define SLSEP "\n"

typedef struct uaelb {				/* edit line block */
	struct uaelb	*next;					/* next line */
	struct uaelb	*prev;					/* previous line */
	char				*line;					/* pointer to line */
	int				lineno;
}	UA_elb;

static UA_elb	*p_elb;					/* current elb */
static UA_elb	*ph_elb;					/* head elb in list */
static UA_elb	*pt_elb;					/* tail elb in list */

/*********************************************************************
**    E_FUNCTION     : ua_edit_text_trvfrm(stg,&stglen,line_no,option)
**			Edit each line of stg using a form. Reconstruct for user.
**    PARAMETERS   
**       INPUT  : 
**			stg						multi-line string to edit.
**			stglen					length of stg.
**       OUTPUT :  
**			stg						edited string.
**			stglen					new string length
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_edit_text_trvfrm(stg,stglen,line_no,option)
	char		*stg;								/* string to edit */
	int		*stglen;							/* length of stg */
	int		line_no,option;
	
{
	int			action[4];
	int			frmlineno;
	int			frmlinemax;
	int			i;
	int			ibeg;
	int			ilinebeg;
	int			iline;
	int			ilineno;
	int			ilinemax;
	int			ipos;
	int 			status;
	int 			length;
	int 			retstat;
	int 			loc_line_no;
	char			c;
	char			endlsep;
	char			chlineno;						/* line number changed flag */
	char			modline[1000];				/* current line being modified */
	char			more;
	char			*uu_malloc();
	UA_elb		*pelb;


	/*------- make sure string ends in a line sep char ------------*/
	if ( stg[*stglen-1]!=CLSEP ) {
		stg[*stglen] = CLSEP;				/* make stgend null linesep */
		(*stglen)++;
		endlsep = 0;							/* no end line sep */
	}
	else
		endlsep = 1;							/* string has end line sep */
	/*-------- go through string and break into line- make list */
	p_elb = ph_elb = pt_elb = 0;
	ilineno = ilinemax = 0;
	ibeg = 0;
	for (i=0; i<*stglen; i++) {			/* see how many lines */
		if (stg[i]==CLSEP) {					/* end of line */
			ilinemax++;
			stg[i] = '\0';						/* terminate string at sep */
			ua_edtx_addelb(&stg[ibeg]);
			stg[i] = CLSEP;
			ibeg = i + 1;
		}
	}

	/*------------  start at head of list  --------------------------*/

	p_elb = ph_elb;							/* to first line */
	loc_line_no = line_no;
	if(loc_line_no > ilinemax )
		{
		uu_uerror0(UA_DRAFTING,38);
		return(1);
		}

		/*------- position to new line number for action --------------*/
	if (loc_line_no!=p_elb->lineno) 	/* user changed line number  */
		{
		for (pelb=ph_elb;				/* find the  lineno wanted */
				pelb!=0 && pelb->lineno!=loc_line_no;
				pelb=pelb->next ) ;
		if (pelb!=0)					/* found the line number */
			p_elb = pelb;				/* set current block */
		}

	strcpy(modline,p_elb->line);		/* move line to mod area */
	if (strlen(modline)>MAXLINELEN)	/* max line size we can edit */
		modline[MAXLINELEN] = '\0';	/* truncate  */
	length = 0;

	switch (option) {
	 case 1:									/***** MODIFY ******/
		status = ud_string_def(UA_DRAFTING, 125, modline, MAXLINELEN, 
					&length, &retstat);
		if( status != UA_OPCOMPLETE || length <= 0 ) return(1);
		ua_edtx_modelb(p_elb,modline);
		p_elb = p_elb->next;				/* to next line */
		break;
	 case 2:									/****DELETE ******/
		if( ilinemax != 1 )
			{
			ilinemax--;
			ua_edtx_rmelb();					/* delete the current elb */
			}
		else
			{
			return(1);
			}
		break;
	 case 3:									/****INSERT (before current)*/
		status = ud_string(UA_DRAFTING, 126, modline, MAXLINELEN, 
					&length, UU_FALSE);
		if( status != UA_OPCOMPLETE || length <= 0 ) return(1);
		if (p_elb!=0)
			p_elb = p_elb->prev;			/* add after previous */
		ua_edtx_addelb(modline);
		ilinemax++;
		p_elb = p_elb->next;
		break;
	 case 4:									/*****ADD (after current)*/
		status = ud_string(UA_DRAFTING, 126, modline, MAXLINELEN, 
					&length, UU_FALSE);
		if( status != UA_OPCOMPLETE || length <= 0 ) return(1);
		ua_edtx_addelb(modline);
		ilinemax++;
		break;
	}

	/*---------- put lines into users string -----------------------*/
	stg[0] = '\0';								/* empty string */
	for ( p_elb=ph_elb; p_elb!=0; p_elb=p_elb->next ) {
		strcat( stg, p_elb->line);
		if (p_elb->next!=0) 					/* have next string - add sep*/
			strcat( stg,SLSEP);
	}
	*stglen = strlen(stg);					/* and string length */
	ua_edtx_rmallelb();
	return(0);
}
/*********************************************************************
**    I_FUNCTION :  ua_edtx_addelb(stg)
**		Add new edit line block to list after current and 
**		make new block current.
**		Allocate memory for both elb and string.
**		NOTE: if p_elb is NULL, then add block to head of list.
**    PARAMETERS   
**       INPUT  : 
**				char			*stg				string
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static ua_edtx_addelb(stg)
	char		*stg;							/* edit string */
{
	int		lineno;
	char		*pstg;
	char		*uu_malloc();
	UA_elb	*pelb;

	uu_denter(UU_STRC,(us,"ua_edtx_addelb(%s)",stg));
	pelb = (UA_elb *)uu_malloc( sizeof(UA_elb) );	/* get new block mem */
	if (p_elb==0) {						/* no current block */
		pelb->next = ph_elb;				/* make new block head of list */
		pelb->prev = 0;
		ph_elb = pelb;						/* this new head block */
		pt_elb = ph_elb;						/* this new tail block */
		lineno = 1;
	}
	else {									/* add after p_elb */
		pelb->next = p_elb->next;
		pelb->prev = p_elb;
		p_elb->next = pelb;
		if (pelb->next==0)				/* this at end of list */
			pt_elb = pelb;					/* this new tail block */
		else
			(pelb->next)->prev = pelb;
		lineno = p_elb->lineno + 1;
	}
	p_elb = pelb;
	p_elb->line = uu_malloc( strlen(stg)+1);	/* get string area */
	strcpy(p_elb->line,stg);			/* copy in string */
	p_elb->lineno = lineno;				/* and line number */
	uu_dprint(UU_STRC,(us,"ua_edtx_addelb: lineno=%d",lineno));
	/*------- bump subsequent line numbers by 1 ----------------------*/
	for (pelb=p_elb->next; pelb!=0; pelb=pelb->next)	/* bump linenos */
		pelb->lineno = ++lineno;
	uu_dexit;
}
/*********************************************************************
**    I_FUNCTION :  ua_edtx_rmallelb()
**		Remove edit line block list - control blocks and line strings.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static ua_edtx_rmallelb()
{
	UA_elb		*p;
	/*------- free control block and lines from tail to head ----------*/
	uu_denter(UU_STRC,(us,"ua_edtx_rmallelb()"));
	p_elb=pt_elb;
	while ( p_elb!=0 ) {
		uu_free(p_elb->line);			/* first free the line string */
		p = p_elb;
		p_elb = p_elb->prev;				/* get previous block */
		uu_free(p);							/* free the control block */
	}
	uu_dexit;
}
/*********************************************************************
**    I_FUNCTION :  ua_edtx_rmelb()
**		Remove current line block - set current to its next.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static ua_edtx_rmelb()
{
	int			lineno;
	UA_elb		*pelb;

	uu_denter(UU_STRC,(us,"ua_edtx_rmelb() line=%d",p_elb->lineno));
	pelb = p_elb;							/* remember block to delete */
	if (p_elb->prev==0) {				/* no previous- this head elb */
		ph_elb = p_elb->next;
		if (ph_elb!=0)						/* have next elb */
			ph_elb->prev = 0;				/* will not have a previous */
		else									/* list empty */
			pt_elb = 0;						/* have no tail either */
		p_elb = ph_elb;
	}
	else if (p_elb->next==0) {			/* no next - this tail elb */
		pt_elb = p_elb->prev;
		pt_elb->next = 0;
		p_elb = pt_elb;
	}
	else {									/* normal - in center of list */
		(p_elb->prev)->next = p_elb->next;
		(p_elb->next)->prev = p_elb->prev;
		p_elb = p_elb->next;
	}
	uu_free(pelb->line);					/* first free the line string */
	uu_free(pelb);							/* free the control block */
	/*------- renumber remaining lines -----------------------------*/
	lineno = 0;
	for (pelb=ph_elb; pelb!=0; pelb=pelb->next)	/* number the lines */
			pelb->lineno = ++lineno;
	uu_dexit;
}
/*********************************************************************
**    I_FUNCTION :  ua_edtx_modelb(pelb,modline)
**		Replace line in elb with modline. Use existing string if
**		modline length <= current, else get new larger area and free old.
**    PARAMETERS   
**       INPUT  : 
**			UA_elb			*pelb			edit line block to modify line
**			char				*modline		new line for this line.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static ua_edtx_modelb(pelb,modline)
	UA_elb			*pelb;				/* line block to modify */
	char				*modline;			/* new string for line */
{
	char				*uu_malloc();

	uu_denter(UU_STRC,(us,"ua_edtx_modelb(line %d,%s)",
			pelb->lineno,modline));
	if ( strcmp(modline,pelb->line)!=0 ) {	/* mod diff from current */
		if ( strlen(modline)>strlen(pelb->line) ) {	/* new too big */
			uu_free(pelb->line);			/* free current strine area */
			pelb->line = uu_malloc(strlen(modline)+1);
		}
		strcpy(pelb->line,modline);		/* move in new strin */
	}
	uu_dexit;
}
