/*********************************************************************
**    NAME         :  jsignonf.c
**       CONTAINS:
**    			uj_signon
**    			uj_signoff
**    			uji_packinfo
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       jsignonf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:48
*********************************************************************/

#include	"ustdio.h"
#include "usysdef.h"
#include "uhep.h"
#include "dascom.h"
#include "ddef.h"
#include "udebug.h"
#include "xenv1.h"

#define	DEFAULT	1
#define	NODEFAULT	0
static char	yname[60];							/* user's name  */
static char	pjnu[17], acntnu[21];			/* project number, account number */
static char	atsav[2];							/* autosave? */

/*********************************************************************
**    E_FUNCTION :  uj_signon()
**       sign on module for DDC
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_signon()
{
	
	int done, i,j,status;
	static char	tyname[60];					/* Field 0...user's name  */
	static char	tpjnu[17];					/* Field 1...project number */
	static char	tacntnu[21];				/* Field 2...account number */
	static int	autosave;					/* Field 3...autosave flag */
	static int	togstr;						/* Field 4...exit flag	*/

	static int *ans[] = { (int *)tyname, (int *)tpjnu, (int *)tacntnu, 
								 &autosave, &togstr }; 

	static int *def[] = { (int *)tyname, (int *)tpjnu, (int *)tacntnu, 
								 NULL, NULL }; 


	uu_denter(UU_DTRC,(us, "uj_signon"));

	/* Set initial field values */
	strcpy(tyname,yname);
	strcpy(tpjnu,pjnu);
	strcpy(tacntnu,acntnu);

	status = ud_form( "jsignon.frm", def, ans );
	if (status==-1)
		return  -1;
	/* Save new values in global space */
	strcpy(yname,tyname);
	strcpy(pjnu,tpjnu);
	strcpy(acntnu,tacntnu);

	uu_dprint(UU_DTRC,(us,"ud_form returned-ynm=%s,pjnu=%s,actnu=%s,tog=%d",
							yname, pjnu, acntnu, togstr));

	uji_packinfo(yname,pjnu,acntnu,autosave,togstr,&done);
	
	if (autosave)
		ur_enable_auto_save();
	else
		ur_disable_auto_save();

	uu_dexit;
	return(!togstr);
}	/* uj_signon*/


/*********************************************************************
**    E_FUNCTION :  uj_signoff()
**       When user signoff, copy the sign on information to the signoff
**			record
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_signoff()
{
	/* FILE	*fd1; */
	int	fd1;
	int	status;
	int	mode;
	int	nb;
	char	temp[132], buf[132];
	char	pathnm[100];
	char	us[132];

	/* ux_mk_def("U_SIGNON","signon.rec",pathnm); */
	mode = 0;
	ux_mk_chk_syspath(UU_NULL, "^UJ_SIGNON", "signon.rec", UU_NULL, UU_NULL,
		&mode, pathnm, UX_PRTERRS);
	uu_dprint(UU_DTRC,(us,"in sign_off, pathnm=%s", pathnm));
	if ((status=ux_open(pathnm,"a","STREAM", "TEXT", &fd1, UX_PRTERRS))
		==0)
	{
		sprintf(buf,
			"SIGN OFF Name: %s, Project #: %s, Acct. #: %s, Autosave? : %s; ",
			yname, pjnu, acntnu, atsav);
		nb = strlen(buf);
		ux_write(fd1,buf,sizeof(char),&nb,UX_PRTERRS);
		uu_udatime(temp);				
		sprintf(buf, "%s\n", temp);
		nb = strlen(buf);
		ux_write(fd1,buf,sizeof(char),&nb,UX_PRTERRS);
		ux_close(fd1, UX_PRTERRS);
	}
	else
	{
		uu_uerror1 (UJ_SUPPORT,7,pathnm);
		uu_dprint(UU_DTRC,(us,"can't open signon.rec,status=%d",status));
	}
}	/* uj_signoff  */

/*********************************************************************
**    E_FUNCTION :  uji_packinfo(yname,pjnu,acntnu,issave,gout,done)
**       Pack the sign on information to the signon record
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uji_packinfo(yname,pjnu,acntnu,issave,gout,done)
char	yname[];
char	*pjnu, *acntnu;
int	gout, *done;
int	issave;			/* is autosave on? */
{
	/* FILE	*fd1; */
	int	fd1;
	int	mode;
	char	temp[132], buf[132];
	int	status;
	char	pathnm[100];
	char	us[132];
	int	len;

	*done = UU_FALSE;
	if ((yname[0]!='\0')&&(!gout))
	  {
		/* 	ux_mk_def("U_SIGNON","signon.rec",pathnm); */
		mode = 0;
		ux_mk_chk_syspath(UU_NULL, "^UJ_SIGNON", "signon.rec", UU_NULL, UU_NULL,
			&mode, pathnm, UX_PRTERRS);
		uu_dprint(UU_DTRC,(us,"in packinfo, pathnm=%s", pathnm));
		/* if ((status=ux_open_file(&fd1,pathnm,"append","text",0,0,0))!=0) */
		if ((status=ux_open(pathnm,"a","STREAM","TEXT",&fd1,UX_PRTERRS))
			!= UU_SUCCESS)
			/* what should the mode (octal ) be ? */
			status = ux_create_file(pathnm,0660, UU_NULL, "STREAM",
											"TEXT", "UX_NOEXTRA", &fd1, UX_PRTERRS);
		uu_dprint(UU_DTRC,(us,"in packinfo, status=%d", status));
		if (status == 0)
		  {
			atsav[0] = (issave==0)? 'Y': 'N';
			atsav[1] = '\0';
			sprintf(temp, "SIGN ON Name: %s, Project #: %s, Acct. #: %s, Autosave? : %s; ",
					yname, pjnu, acntnu, atsav);
			len = strlen(temp);
			status = ux_write(fd1,temp,sizeof(char),&len,UX_PRTERRS);
			/*  formerly a call to ux_wrt_text(fd1,temp); */
			uu_dprint(UU_DTRC,(us,"after wrtext, status=%d", status));
			uu_udatime(buf);				
			sprintf(temp,"%s\n", buf);
			len = strlen(temp);
			ux_write(fd1,temp, sizeof(char), &len, UX_PRTERRS);
			status = ux_close(fd1, UX_PRTERRS);
			uu_dprint(UU_DTRC,(us,"after closefile, status=%d", status));
		  }
	   else
			/* printf("can't open file signon.rec to write\n"); */
			uu_uerror1 (UJ_SUPPORT,7,pathnm);
		*done = UU_TRUE;
	  }
	else
		if (gout)
			*done = UU_TRUE;
}	/* uji_packinfo  */
