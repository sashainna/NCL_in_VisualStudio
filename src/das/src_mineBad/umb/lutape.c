/*********************************************************************
**	FILENAME: lutape.c
**	CONTAINS:		ul_dnc_it
**				ul_punch_it
**				ul_set_dnc_mod
**				ul_set_punch_mod
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lutape.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:21
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "dtypes.h"
#include "lcom.h"
#include "lumb.h"
#include "udebug.h"
#include "udfdata.h"
#include "udforms.h"
#include "uhep.h"
#include "ustdio.h"
#include "xenv1.h"
#include "xfsys0.h"
#include "xfsys1.h"
#include "ctype.h"
#include "nclfc.h"


#if UU_COMP == UU_VAXVMS
#define UTIL_DIR "NCL$UTIL:"
#define DATA_DIR "NC14:"
#define ldlm "/"
#define ldlms '/'
#else
#define UTIL_DIR "NCL_UTIL"
#define DATA_DIR "nc14"
#define ldlm "-"
#define ldlms '-'
#endif

#define NODEFAULT 0
#define DEFAULT 1

/*********************************************************************
**	E_FUNCTION:	ul_dnc_it (batch)
**			This function runs the DNC utility. The DNC
**			utility must reside in NCL$UTIL.
**
**       INPUT  :	batch = 0 = Interactive run.
**				1 = Batch run.
**       OUTPUT :  none.
**
**    RETURNS      :	UU_SUCCESS if successful, UU_FAILURE otherwise.
**
**    SIDE EFFECTS : Spawns a sub-process and opens & writes to a
**			scrolling window.
**
**    WARNINGS     : none.
*********************************************************************/
ul_dnc_it(batch)
	int batch;
{
	char *pointer,*position,*rindex();
	char buf[200],buf1[200],com[200];
	UX_pathname fullname;
	int wargs[2],numint,stat,status,file_status,mode;
	int i,markval,ierr;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_dnc_it(batch)"));
	status = UU_SUCCESS;
/*
.....Get full path name of Utility Directory
*/
	if (ul_get_full_dir(UTIL_DIR,fullname) != UU_SUCCESS)
	{
		sprintf (com,"Could not access utility directory %s",
			fullname);
		ud_wrerr (com);
		goto failed;
	}
/*
.....Verify that DNC exists
*/
	strcpy (buf,"dnc");
	strcpy (buf1,buf);
#if UU_COMP == UU_VAXVMS
	strcat (buf1,".EXE");
#endif
	mode = UX_EXISTS|UX_READ;
	stat = ux_file_inquire(UU_NULL,fullname,buf1,UU_NULL,UU_NULL,&mode,
		&file_status,com,UX_NPRTERRS);
	if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		ud_wrerr ("DNC routine does not exist.");
		goto failed;
	}
/*
.....Get name of punch file to run DNC against
*/
	strcpy (buf1,UL_program);
	strcat (buf1,".pu1");
	do
	{
		ul_string_def ("Enter file name",buf1,sizeof(buf1),
				&numint,&file_status);
		ul_strip_blanks (buf1,&numint);
		if (strlen(buf1) == 0) goto done;
/*
.....Verify that input file exists
*/
		ierr = 0;
		if (UL_dnc_mode == 0 || UL_dnc_mode == 4)
		{
			strcpy (com,buf1);
			position = rindex(com,'/');
			if (position != 0) *position = '\0';
			mode = UX_EXISTS|UX_READ;
			stat = ux_file_inquire(UU_NULL,UU_NULL,com,UU_NULL,UU_NULL,&mode,
				&file_status,com,UX_NPRTERRS);
			if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
			{
				sprintf (com,"Punch file %s does not exist",buf1);
				ud_wrerr (com);
				ierr = 1;
			}
		}
	} while (ierr == 1);
/*
.....Append default switches
*/
	position = rindex(buf1,'/');
	if (position == 0)
	{
/*
.....Append mode switch
*/
		switch (UL_dnc_mode)
		{
		case 0:
			strcat (buf1,"/SE");
			if (strlen(UL_dnc_send_pr) != 0)
			{
				strcat (buf1,":{");
				strcat (buf1,UL_dnc_send_pr);
				strcat (buf1,"}");
			}
			break;
		case 1:
			strcat (buf1,"/RE");
			if (strlen(UL_dnc_rcv_pr) != 0)
			{
				strcat (buf1,":{");
				strcat (buf1,UL_dnc_rcv_pr);
				strcat (buf1,"}");
			}
			break;
		case 2:
			if (strlen(UL_dnc_rcv_pr) != 0)
			{
				strcat (buf1,"/RE:{");
				strcat (buf1,UL_dnc_rcv_pr);
				strcat (buf1,"}");
			}
			if (batch == 0)
				strcat (buf1,"/VE");
			else
				strcat (buf1,"/VB");
			break;
		case 3:
			strcat (buf1,"/TRM");
			break;
		}

/*
.....Append TT switch
*/
		if (strlen(UL_dnc_port) != 0)
		{
			strcat (buf1,"/TT:{");
			strcat (buf1,UL_dnc_port);
			strcat (buf1,"}");
		}
/*
.....Append WT switch
*/
		if (strlen(UL_dnc_echo_pr) != 0)
		{
			strcat (buf1,"/WT:{");
			strcat (buf1,UL_dnc_echo_pr);
			strcat (buf1,"}");
		}
/*
.....Append ST switch
*/
		if (strlen(UL_dnc_strt) != 0)
		{
			strcat (buf1,"/ST:{");
			strcat (buf1,UL_dnc_strt);
			strcat (buf1,"}");
		}
/*
.....Append /XON switch
*/
		if (UL_dnc_xon == 0) strcat (buf1,"/-XON");
		else strcat (buf1,"/XON");
/*
.....Append LI switch
*/
		if (UL_dnc_list == 0) strcat (buf1,"/-LI");
		else strcat (buf1,"/LI");
/*
.....Append MXC switch
*/
		sprintf (com,"/MXC:%d",UL_dnc_reclen);
		strcat (buf1,com);
/*
.....Append PAR switch
*/
		if (UL_dnc_par == 0) strcat (buf1,"/-PAR");
		else strcat (buf1,"/PAR");
/*
.....Append  CHW switch
*/
		if (UL_dnc_chw != 0)
		{
			sprintf (com,"/CHW:%d",UL_dnc_chw);
			strcat (buf1,com);
		}
/*
.....Append  CRW switch
*/
		if (UL_dnc_crw != 0)
		{
			sprintf (com,"/CRW:%d",UL_dnc_crw);
			strcat (buf1,com);
		}
/*
.....Append EOF switch
*/
		if (strlen(UL_dnc_eof) != 0)
		{
			strcat (buf1,"/EOF:{");
			strcat (buf1,UL_dnc_eof);
			strcat (buf1,"}");
		}
/*
.....Append EOT switch
*/
		if (strlen(UL_dnc_eot) != 0)
		{
			strcat (buf1,"/EOT:{");
			strcat (buf1,UL_dnc_eot);
			strcat (buf1,"}");
		}
/*
.....Append RS switch
*/
		strcat (buf1,"/RS:");
		strcat (buf1,UL_dnc_tfile);
	}
/*
.....Run in batch
*/
	if (batch == 1)
	{
#if UU_COMP == UU_VAXVMS
		sprintf (com,"%s %s",buf,buf1);
#else
		sprintf (com,"%s/%s \'\"%s\"\'",fullname,buf,buf1);
#endif
		ul_que_util (com,1);
		ul_submit_batch(2);
		goto done;
	}
/*
.....Let operating system know where DNC is
.....so that we may run it
*/
#if UU_COMP == UU_VAXVMS
	strcpy (com,"$ ");
	strcat (com,UTIL_DIR);
	strcat (com,buf);
	ul_set_symbol (buf,com);
#endif
/*
.....Run DNC
*/
#if UU_COMP == UU_VAXVMS
	strcat (buf," ");
	strcat (buf,buf1);
	ul_spawn (buf,1);
#else
	sprintf (com,"%s/%s \"%s\"",fullname,buf,buf1);
	ul_spawn (com,1);
#endif
/*
.....Failure
*/
failed: status = UU_FAILURE;
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**	E_FUNCTION:	ul_punch_it (flag,batch)
**			This function runs the tape punch utilities. The
**			punch utilities must reside in NCL$UTIL.
**
**       INPUT  :	flag =	1 = Run NCP.
**				2 = Run NCI.
**				3 = Run NCV.
**			batch = 0 = Interactive run.
**				1 = Batch run.
**       OUTPUT :  none.
**
**    RETURNS      :	UU_SUCCESS if successful, UU_FAILURE otherwise.
**
**    SIDE EFFECTS : Spawns a sub-process and opens & writes to a
**			scrolling window.
**
**    WARNINGS     : none.
*********************************************************************/
ul_punch_it(flag,batch)
	int flag,batch;
{
	char *pointer,*position,*rindex();
	char buf[200],buf1[200],com[200];
	UX_pathname fullname;
	int wargs[2],numint,stat,status,file_status,mode;
	int i,markval,ierr;
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_punch_it(flag,batch)"));
	status = UU_SUCCESS;
/*
.....Command Reject
*/
	UD_MARK (markval,UU_FALSE);
	if (markval != 0)
	{
		ul_close_window();
		UD_UNMARK (markval);
		return;
	}
/*
.....Get full path name of Utility Directory
*/
/*	if (ub_check_fullpath(UTIL_DIR, UU_NULL, UU_NULL, UU_NULL,
		UU_NULL,fullname,&file_status) != UU_SUCCESS)*/
	if (ul_get_full_dir(UTIL_DIR,fullname) != UU_SUCCESS)
	{
		sprintf (com,"Could not access utility directory %s",
			fullname);
		ud_wrerr (com);
		if (flag == 2) ul_close_window();
		goto failed;
	}
/*
.....Get name of punch utility to run
*/
	switch (flag)
	{
	case 1:
		strcpy (buf,"ncp");
		break;
	case 2:
		strcpy (buf,"nci");
		break;
	case 3:
		strcpy (buf,"ncv");
		break;
	}
/*
.....Verify that punch routine exists
*/
/*	strcpy (buf1,UTIL_DIR);*/
	strcpy (buf1,buf);
#if UU_COMP == UU_VAXVMS
	strcat (buf1,".EXE");
#endif
	mode = UX_EXISTS|UX_READ;
	stat = ux_file_inquire(UU_NULL,fullname,buf1,UU_NULL,UU_NULL,&mode,
		&file_status,com,UX_NPRTERRS);
	if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		sprintf (com,"Punch routine %s does not exist",buf1);
		ud_wrerr (com);
		goto failed;
	}
/*
.....Get name of punch file to run punch routine against
*/
	strcpy (buf1,UL_program);
	strcat (buf1,".pu1");
	do
	{
		ul_string_def ("Enter file name",buf1,sizeof(buf1),
				&numint,&file_status);
		ul_strip_blanks (buf1,&numint);
		if (strlen(buf1) == 0) goto done;
/*
.....Verify that input file exists
*/
		ierr = 0;
		if (flag != 2)
		{
			strcpy (com,buf1);
			position = rindex(com,ldlms);
			if (position != 0) *position = '\0';
			mode = UX_EXISTS|UX_READ;
			stat = ux_file_inquire(UU_NULL,UU_NULL,com,UU_NULL,UU_NULL,&mode,
				&file_status,com,UX_NPRTERRS);
			if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
			{
				sprintf (com,"Punch file %s does not exist",buf1);
				ud_wrerr (com);
				ierr = 1;
			}
		}
	} while (ierr == 1);
/*
.....Append default switches
*/
	position = rindex(buf1,ldlms);
	if (position == 0)
	{
/*
.....Append GRC switch
*/
		strcat(buf1,ldlm);
		if (UL_pch_greco == 0) strcat (buf1,"NO");
		strcat (buf1,"GRC");
/*
.....Append LI switch
*/
		strcat(buf1,ldlm);
		if (UL_pch_list == 0) strcat (buf1,"NO");
		strcat (buf1,"LI");
/*
.....Append MXC switch
*/
		sprintf (com,"%sMXC:%d",ldlm,UL_pch_reclen);
		strcat (buf1,com);
/*
.....Append PCD switch
*/
		if (flag == 1)
		{
			sprintf (com,"%sPCD:%d",ldlm,UL_pch_on);
			strcat (buf1,com);
		}
/*
.....Append RCD switch
*/
		if (flag != 1)
		{
			sprintf (com,"%sRCD:%d",ldlm,UL_pch_off);
			strcat (buf1,com);
		}
/*
.....Append RS switch
*/
		strcat (buf1,ldlm);
		strcat (buf1,"RS:");
		strcat (buf1,UL_pch_tfile);
/*
.....Append TM switch
*/
		strcat(buf1,ldlm);
		if (UL_pch_trans == 0) strcat (buf1,"NO");
		strcat (buf1,"TM");
/*
.....Append ST switch
*/
		if (strlen(UL_pch_strt) != 0)
		{
			strcat(buf1,ldlm);
			strcat (buf1,"ST:{");
			strcat (buf1,UL_pch_strt);
			strcat (buf1,"}");
		}
/*
.....Append COM switch
*/
		if (strlen(UL_pch_com) != 0)
		{
			strcat (buf1,ldlm);
			strcat (buf1,"COM:");
			strcat (buf1,UL_pch_com);
		}
/*
.....Append LDR switch
*/
		if (flag == 1)
		{
			sprintf (com,"%sLDR:%d",ldlm,UL_pch_ldr);
			strcat (buf1,com);
/*
.....Append MR switch
*/
			strcat(buf1,ldlm);
			if (UL_pch_partno == 0) strcat (buf1,"NO");
			strcat (buf1,"MR");
/*
.....Append SPD switch
*/
			sprintf (com,"%sSPD:%d",ldlm,UL_pch_speed);
			strcat (buf1,com);
		}
	}
/*
.....Run in batch
*/
	if (batch == 1)
	{
#if UU_COMP == UU_VAXVMS
		sprintf (com,"%s %s/BAT",buf,buf1);
#else
		sprintf (com,"%s/%s \'\"%s-BAT\"\'",fullname,buf,buf1);
#endif
		ul_que_util (com,1);
		ul_submit_batch(2);
		goto done;
	}
/*
.....Let operating system know where punch routine is
.....so that we may run it
*/
#if UU_COMP == UU_VAXVMS
	strcpy (com,"$ ");
	strcat (com,UTIL_DIR);
	strcat (com,buf);
	ul_set_symbol (buf,com);
#endif
/*
.....Run the punch routine
*/
#if UU_COMP == UU_VAXVMS
	strcat (buf," ");
	strcat (buf,buf1);
	ul_spawn (buf,1);
#else
	sprintf (com,"%s/%s \"%s\"",fullname,buf,buf1);
	ul_spawn (com,1);
#endif
/*
.....Failure
*/
failed: status = UU_FAILURE;
done:;
	UD_UNMARK(markval);
	uu_dexit;
	return(status);
}	

/*********************************************************************
**	 E_FUNCTION : ul_set_dnc_mod()
**			This function sets the default options to use
**			when scheduling DNC.
**	 PARAMETERS	
**		 INPUT  :  none
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Defines DNC routine modals.
**	 WARNINGS: none.
*********************************************************************/

ul_set_dnc_mod()
{
	UX_pathname fullname;
	char port[21],spr[21],rpr[21],echo[21],lstrt[21],eof[21],eot[21];
	char tfile[4];
	int mode[8],xon[4],list[4],reclen[4],par[4],chw[5],crw[5];
	char buf[80],buf1[UX_MAX_PATH_LEN+40];
	int stat,fmode,file_status,n;
	int *ans[15];
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_set_dnc_mod()"));
/*
.....Set up the defaults
.....Load the input values into
.....local storage area
*/
	mode[0] = UL_dnc_mode;
	strcpy (port,UL_dnc_port);
	strcpy (spr,UL_dnc_send_pr);
	strcpy (rpr,UL_dnc_rcv_pr);
	strcpy (echo,UL_dnc_echo_pr);
	strcpy (lstrt,UL_dnc_strt);
	xon[0] = UL_dnc_xon;
	list[0] = UL_dnc_list;
	reclen[0] = UL_dnc_reclen;
	par[0] = UL_dnc_par;
	chw[0] = UL_dnc_chw;
	crw[0] = UL_dnc_crw;
	strcpy (eof,UL_dnc_eof);
	strcpy (eot,UL_dnc_eot);
	strcpy (tfile,UL_dnc_tfile);
	do
	{
/*
.....Field 0 is DNC mode
*/
		ans[0] = (int *)mode;
/*
.....Field 1 is Communications port
*/
		ans[1] = (int *)port;
/*
.....Field 2 is Send prompt
*/
		ans[2] = (int *)spr;
/*
.....Field 3 is Receive prompt
*/
		ans[3] = (int *)rpr;
/*
.....Field 4 is Echo start prompt
*/
		ans[4] = (int *)echo;
/*
.....Field 5 is Start string
*/
		ans[5] = (int *)lstrt;
/*
.....Field 6 is XON/XOFF
*/
		ans[6] = (int *)xon;
/*
.....Field 7 is Display file
*/
		ans[7] = (int *)list;
/*
.....Field 8 is Record length
*/
		ans[8] = (int *)reclen;
/*
.....Field 9 is Even parity
*/
		ans[9] = (int *)par;
/*
.....Field 10 is Character wait
*/
		ans[10] = (int *)chw;
/*
.....Field 11 is Carriage return wait
*/
		ans[11] = (int *)crw;
/*
.....Field 12 is EOF string
*/
		ans[12] = (int *)eof;
/*
.....Field 13 is EOT string
*/
		ans[13] = (int *)eot;
/*
.....Field 14 is Translation file
*/
		ans[14] = (int *)tfile;
/*
.....Get the Form input
*/
		stat = ud_form("ldnc.frm",ans,ans);
		if (stat==-1)
			return;
/*
.....Verify that Translation file exists
*/
		ul_get_full_dir(DATA_DIR,fullname);
		sprintf (buf,"nctape.%s",tfile);
		fmode = UX_EXISTS|UX_READ;
		stat = ux_file_inquire(UU_NULL,fullname,buf,UU_NULL,UU_NULL,&fmode,
			&file_status,buf1,UX_NPRTERRS);
		if (stat != UU_SUCCESS || fmode == (fmode|UX_NEXISTS))
		{
			sprintf (buf1,"Translation file %s does not exist",buf);
			ud_wrerr (buf1);
		}
	}
	while (stat != UU_SUCCESS || fmode == (fmode|UX_NEXISTS));
/*
.....Strip blanks from char strings
*/
	n = strlen(port);
	ul_strip_blanks(port,&n);

	strcpy (buf,spr);
	n = strlen(buf);
	ul_strip_blanks(buf,&n);
	if (n == 0) spr[0] = '\0';

	strcpy (buf,rpr);
	n = strlen(buf);
	ul_strip_blanks(buf,&n);
	if (n == 0) rpr[0] = '\0';

	strcpy (buf,echo);
	n = strlen(buf);
	ul_strip_blanks(buf,&n);
	if (n == 0) echo[0] = '\0';

	strcpy (buf,lstrt);
	n = strlen(buf);
	ul_strip_blanks(buf,&n);
	if (n == 0) lstrt[0] = '\0';

	strcpy (buf,eof);
	n = strlen(buf);
	ul_strip_blanks(buf,&n);
	if (n == 0) eof[0] = '\0';

	strcpy (buf,eot);
	n = strlen(buf);
	ul_strip_blanks(buf,&n);
	if (n == 0) eot[0] = '\0';
/*
.....Store DNC Modals
*/
	UL_dnc_mode = mode[0];
	strcpy (UL_dnc_port,port);
	strcpy (UL_dnc_send_pr,spr);
	strcpy (UL_dnc_rcv_pr,rpr);
	strcpy (UL_dnc_echo_pr,echo);
	strcpy (UL_dnc_strt,lstrt);
	UL_dnc_xon = xon[0];
	UL_dnc_list = list[0];
	UL_dnc_reclen = reclen[0];
	UL_dnc_par = par[0];
	UL_dnc_chw = chw[0];
	UL_dnc_crw = crw[0];
	strcpy (UL_dnc_eof,eof);
	strcpy (UL_dnc_eot,eot);
	strcpy (UL_dnc_tfile,tfile);
failed:;
done:;
	uu_dexit;
	return;
}

/*********************************************************************
**	 E_FUNCTION : ul_set_punch_mod()
**			This function sets the default options to use
**			when scheduling NCI, NCP, or NCV.
**	 PARAMETERS	
**		 INPUT  :  none
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Defines Punch routine modals.
**	 WARNINGS: none.
*********************************************************************/

ul_set_punch_mod()
{
	UX_pathname fullname;
	char tfile[4],lstrt[22],lcom[2];
	int greco[4],list[4],reclen[3],on[3],off[3],trans[4],ldr[3],partno[4];
	int speed[3];
	char buf[80],buf1[UX_MAX_PATH_LEN+40];
	int stat,mode,file_status,n;
	int *ans[12];
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_set_punch_mod()"));
/*
.....Set up the defaults
.....Load the input values into
.....local storage area
*/
	greco[0] = UL_pch_greco;
	list[0] = UL_pch_list;
	reclen[0] = UL_pch_reclen;
	on[0] = UL_pch_on;
	off[0] = UL_pch_off;
	strcpy (tfile,UL_pch_tfile);
	trans[0] = UL_pch_trans;
	ldr[0] = UL_pch_ldr;
	partno[0] = UL_pch_partno;
	speed[0] = UL_pch_speed;
	strcpy (lstrt,UL_pch_strt);
	strcpy (lcom,UL_pch_com);
	do
	{
/*
.....Field 0 is Greco file
*/
		ans[0] = (int *)greco;
/*
.....Field 1 is Display Tape
*/
		ans[1] = (int *)list;
/*
.....Field 2 is File record length
*/
		ans[2] = (int *)reclen;
/*
.....Field 3 is Punch on code
*/
		ans[3] = (int *)on;
/*
.....Field 4 is Punch off code
*/
		ans[4] = (int *)off;
/*
.....Field 5 is Translation file
*/
		ans[5] = (int *)tfile;
/*
.....Field 6 is User translated codes
*/
		ans[6] = (int *)trans;
/*
.....Field 7 is EOT leader
*/
		ans[7] = (int *)ldr;
/*
.....Field 8 is Punch speed
*/
		ans[8] = (int *)speed;
/*
.....Field 9 is Manreadable Partno
*/
		ans[9] = (int *)partno;
/*
.....Field 10 is Start string
*/
		ans[10] = (int *)lstrt;
/*
.....Field 11 is Comment character
*/
		ans[11] = (int *)lcom;
/*
.....Get the Form input
*/
		stat = ud_form("lpunch.frm",ans,ans);
		if (stat==-1)
			return;
/*
.....Verify that Translation file exists
*/
		ul_get_full_dir(DATA_DIR,fullname);
		sprintf (buf,"nctape.%s",tfile);
		mode = UX_EXISTS|UX_READ;
		stat = ux_file_inquire(UU_NULL,fullname,buf,UU_NULL,UU_NULL,&mode,
			&file_status,buf1,UX_NPRTERRS);
		if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
		{
			sprintf (buf1,"Translation file %s does not exist",buf);
			ud_wrerr (buf1);
		}
	}
	while (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS));
/*
.....Strip blanks from char strings
*/
	strcpy (buf,lstrt);
	n = strlen(buf);
	ul_strip_blanks(buf,&n);
	if (n == 0) lstrt[0] = '\0';

	strcpy (buf,lcom);
	n = strlen(buf);
	ul_strip_blanks(buf,&n);
	if (n == 0) lcom[0] = '\0';
/*
.....Store Punch Modals
*/
	UL_pch_greco = greco[0];
	UL_pch_list = list[0];
	UL_pch_reclen = reclen[0];
	UL_pch_on = on[0];
	UL_pch_off = off[0];
	strcpy (UL_pch_tfile,tfile);
	UL_pch_trans = trans[0];
	UL_pch_ldr = ldr[0];
	UL_pch_partno = partno[0];
	UL_pch_speed = speed[0];
	strcpy (UL_pch_strt,lstrt);
	strcpy (UL_pch_com,lcom);
failed:;
done:;
	uu_dexit;
	return;
}
