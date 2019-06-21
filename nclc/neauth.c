/*********************************************************************
**    NAME         :  neauth.c
**       CONTAINS: 
**       gtenv(tok,var)
**       setflg(flg) ** commented out **
**       cadkey()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       neauth.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:23
*********************************************************************/

#include "usysdef.h"
#include "mfort.h"
#include "nclfc.h"
#include "xenv1.h"

static int cadflg=0;
#if UU_COMP == UU_WINNT
extern char NCL_lic[];
#endif

/*********************************************************************
**    E_FUNCTION     : gtenv
**      Fortran callable routine to call ux_getenv to get environment variables
**    PARAMETERS   
**       INPUT  : 
**          tok    - variable to translate
**       OUTPUT :  
**          var    - translation of tok
**          len    - length of translated token
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gtenv(tok,var,len)
UM_f77_str_ptr tok,var;
UM_int2 *len;
{
	char *ux_getenv(),*tokstr,*varstr,retstr[256],*p;
	char buf[256];
	int i;
	char ltok[255],spac[2];
	tokstr = UM_cstr_of_f77_str(tok);
	varstr = UM_cstr_of_f77_str(var);
	strncpy (ltok,tokstr,80);
	for (i=0; ltok[i] != ' ' && ltok[i] != '\0' && i<80; i++);
	ltok[i] = '\0';
#if UU_COMP == UU_WINNT
	if (strcmp(ltok,"NCCS_LICENSE") == 0) p = NCL_lic;
	else p = ux_getenv(ltok,UX_NPRTERRS);
#else
	p = ux_getenv(ltok,UX_NPRTERRS);
#endif
	if (p == 0)
		strcpy(retstr," ");
	else
		strcpy(retstr,p);
#if UU_COMP == UU_WINNT
	if (retstr[0] == '/' && retstr[2] == '=')
	{
		strcpy(buf,retstr);
		retstr[0] = buf[1];
		retstr[1] = ':';
		strcpy(&retstr[2],&buf[3]);
	}
#endif
	for (i=0; retstr[i] != '\0' && i < 64; i++) varstr[i] = retstr[i];
	*len = i;
}

/*********************************************************************
**    E_FUNCTION     : setflg(flg)
**      Fortran callable routine to set the cadd authorization flag
**      to contol use of drafting, drawing and symbols.
**    PARAMETERS   
**       INPUT  : 
**          flg    - value to set cadflg to
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#ifdef OLDNCL
setflg(flg)
UM_int2 *flg;
   {
   cadflg = (*flg);
   }
#endif OLDNCL
/*********************************************************************
**    E_FUNCTION     : cadkey()
**      Return the cadd authorization flag.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
cadkey()
   {
   return(cadflg);
   }
