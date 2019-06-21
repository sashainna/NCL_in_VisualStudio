/*********************************************************************
**    NAME         :  gsavsc.c -- device dependent icon files.
**       CONTAINS:
**      ug_savescrn(fd,mempt,len) -- UG_DSAVESCRN user callable routine.
**      ug_restscrn(fd,mempt,len) -- UG_DRESTSCRN user callable routine.
**      ug_dsavescrn(prms,reply) -- UG_DSAVESCRN simulation routine.
**      ug_drestscrn(prms,reply) -- UG_DRESTSCRN simulation routine.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gsavsc.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:24
*********************************************************************/
#include "udebug.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"

/*********************************************************************
**    E_FUNCTION :  ug_savescrn(ws,fd,mempt,len) -- UG_DSAVESCRN routine.
**    PARAMETERS   
**       INPUT  : 	int ws -- workstation id
**							int fd -- file descriptor of file.
**							int *mempt -- pointer to data to save
**							int len -- length (bytes) of data to save.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_savescrn(ws,fd,mempt,len)
int fd;
Gws ws;
int *mempt,len;
{
int prms[5],reply[1];

	uu_denter(UU_GITRC,(us,"ug_savescrn(ws=%d,fd=%d,mempt=%x,len=%d",
			ws,fd,mempt,len));
	prms[0] = UG_DSAVSCROP;
	prms[2] = fd;
	prms[3] = (int)mempt;
	prms[4] = len;
	ug_wkcal(ws,prms,reply);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ug_restscrn(ws,fd,rasll,rasur) -- UG_DRESTSCRN routine.
**    PARAMETERS   
**       INPUT  : 	Gws ws -- workstation id
**							int fd; -- file descriptor of open file.
**							Gipoint *rasll,*rasur; -- corners of rectangle to restore.
**       OUTPUT :    
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_restscrn(ws,fd,rasll,rasur)
int fd,ws;								/* file descriptor of open file */
int rasll[2],rasur[2];
{
int prms[7],reply[1];

	uu_denter(UU_GITRC,(us,"ug_restscrn(fd=%d)",fd));
	prms[0] = UG_DRESSCROP;
	prms[2] = fd;
	prms[3]=rasll[0];
	prms[4]=rasll[1];
	prms[5]=rasur[0];
	prms[6]=rasur[1];
	ug_wkcal(ws,prms,reply);
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION :  ug_dsavescrn(prms,reply) -- UG_DSAVESCRN sim routine.
**    PARAMETERS   
**       INPUT  : 	prms[2] -- file descriptor of file.
**							prms[3] -- pointer to data to save
**							prms[4] -- length (bytes) of data to save.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_dsavescrn(prms,reply)
int prms[],reply[];
{
int tmp;

	uu_denter(UU_GITRC,(us,"ug_dsavscrn"));
	write(prms[2],&prms[4],sizeof(int));
	tmp=write(prms[2],(int *)prms[3],prms[4]);
	uu_dprint(UU_GITRC,(us,"ug_dsavescrn:len=%d, bytes written=%d",prms[4],tmp));
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION :  ug_drestscrn(prms,reply) -- UG_DRESTSCRN sim routine.
**    PARAMETERS   
**       INPUT  : 	prms[2] -- file descriptor of open file.(int)
**              :    prms[3] -- where file data was placed.(int**)
**						   prms[4] -- length of data.(int*)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_drestscrn(prms,reply)
int prms[],reply[];
{

	int len,*menupt;
	int rasll[2],rasur[2];
	int offll[2],offur[2];
	int ws,tmp;


	uu_denter(UU_GITRC,(us,"ug_drestscrn()"));
	read(prms[2],&len,sizeof(int));
	menupt = (int *) uu_toolmalloc(len);
	tmp=read(prms[2],menupt,len);
	uu_dprint(UU_GITRC,(us,"ug_drestscrn: bytes read=%d",tmp));
			/* Display menu   */
	ws = prms[1];
	rasll[0]=prms[3];
	rasll[1]=prms[4];
	rasur[0]=prms[5];
	rasur[1]=prms[6];
	offll[0] = 0; offll[1] = 0; offur[0] = 0; offur[1] = 0;
 (*(ug_gksstli.wsopen[ws].connid)[UG_DRASPUT])
	      ( rasll, rasur, menupt, UG_COPY,offll,offur);

	/* Free memory */
 uu_toolfree(menupt);

	uu_dexit;
}

