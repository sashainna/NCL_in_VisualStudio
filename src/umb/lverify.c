/*********************************************************************
**  FILENAME: lverify.c
**  CONTAINS:   ul_verify_reset()
**              ul_verify(ifl)
**              ul_verify_end()
**              ul_verify_modals()
**              ul_verify_open(fd)
**				nclu_ipv_form(iclf,kbeg,kend,cpart,cwip)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lverify.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       07/28/15 , 11:17:37
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "xfsys0.h"
#include "lcom.h"
#include "lipv.h"
#include "mdrel.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nsimulate.h"
#include "nkeywd.h"
#include <ctype.h>
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"

static int pcnt=0;

static int fsrc=0, fstrt=1, fend=1;
static UX_pathname clfil;
static char fsseq[22],fscmd[42],feseq[22],fecmd[42];
static char fpartf[22],fwipf[22];


static int tsrc,tstrt,tend,ttra;
static char tpartf[22],twipf[22],teseq[22],tsseq[22];

/*********************************************************************
**
**    E_FUNCTION         :  filtog()             
**
*********************************************************************/
static UD_FSTAT  filtog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_default_method(fieldno, val, stat);
/*
.....Enable correct fields
.....based on toggle field value
*/
	switch(*fieldno)
	{
	case 0:
		if (tsrc == 1) 
		{
			ud_set_traverse_mask(1,UU_TRUE);
			ud_set_traverse_mask(2,UU_TRUE);
		}
		else 
		{
			ud_set_traverse_mask(1,UU_FALSE);
			ud_set_traverse_mask(2,UU_FALSE);
		}
		break;
	case 3:
		if (tstrt == 1) 
			ud_set_traverse_mask(4,UU_TRUE);
		else 
			ud_set_traverse_mask(4,UU_FALSE);
		if (tstrt == 2) 
			ud_set_traverse_mask(5,UU_TRUE);
		else 
			ud_set_traverse_mask(5,UU_FALSE);
		break;
	case 6:
		if (tend == 1) 
			ud_set_traverse_mask(7,UU_TRUE);
		else 
			ud_set_traverse_mask(7,UU_FALSE);
		if (tend == 2) 
			ud_set_traverse_mask(8,UU_TRUE);
		else 
			ud_set_traverse_mask(8,UU_FALSE);
		break;
	case 11:
		if (ttra == 1) 
			ud_set_traverse_mask(12,UU_TRUE);
		else 
			ud_set_traverse_mask(12,UU_FALSE);
		break;
	default:
		break;
    }
	return(UD_FLDOK);
}

/*********************************************************************
**	 E_FUNCTION:int ul_verify_reset()
**		This function resets the counter for NCLIPV tool path files
**		and sets up the stock name to 'partprogram'.stk.  It should be
**		called prior to starting any new part.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_verify_reset()
{
	strcpy(UL_stk_file,UL_program);
	strcat(UL_stk_file,".stk");
	pcnt = 0;
	fsrc = 0; fstrt = 1; fend = 1;
	clfil[0] = '\0';
	fsseq[0] = '\0'; fscmd[0] = '\0';
	feseq[0] = '\0'; fecmd[0] = '\0';
	fpartf[0] = '\0'; fwipf[0] = '\0';
	ul_ipv_initialize();
	return(UU_SUCCESS);
}

/*********************************************************************
**	 E_FUNCTION:int ul_verify(ifl)
**		This function controls the interaction between NCL and NCV
**		(NCL IPV).
**	 PARAMETERS	
**		 INPUT  : ifl = 1 - IN PROCESS VERIFY
**						(Starts NCV with the current tool path)
**						2 - STANDALONE VERIFY
**						(Starts NCV without any specific tool path)
**						3 - TOOLPATH ON
**						(Closes the current clfile and starts a new one)
**						4 - TOOLPATH OFF
**						(Writes the current clfile out and incs counter)
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_verify(ifl)
	int ifl;
{
	int inum,status,kerr,i,nc1,nc2;
	UN_clstruc *kbeg,*kend;
	int nc;
	UM_int2 iclf;
	UX_pathname lcom,partf,wipf,fnam,fpartf;
	char lbuf[UX_MAX_PATH_LEN+40],cpart[22],cwip[22];
	UM_f77_str asn;
	status = UU_SUCCESS;
/*
.....IN PROCESS VERIFY
.....Spawn NCV using Work in Process file
*/
	if (ifl == 1)
	{
/*
.....Use new NCLIPV
*/
		if (LW_version == LW_MWORKS)
		{
			ul_ipv_start(UU_TRUE);
		}
/*
.....Get the toolpath to send to NCLIPV
*/
		else
		{
#if UU_COMP == UU_WIN2K
			goto failed;
#endif
			status = nclu_ipv_form(&iclf,&kbeg,&kend,cpart,cwip);
			if (status != 0) goto endit;
			nc1 = strlen(cpart);
			nc2 = strlen(cwip);
			ul_strip_blanks(cpart,&nc1);
			ul_strip_blanks(cwip,&nc2);
/*
.....Create Part and WIP file names
*/
			strcpy(partf,UL_program);
			if (nc1 != 0)
			{
				strcat (partf,"_");
				strcat (partf,cpart);
				strcat (partf,".ipv");
			}
			if (nc2 != 0)
			{
				strcpy (wipf,UL_program);
				strcat (wipf,"_");
				strcat (wipf,cwip);
				strcat (wipf,".wip");
			}
			else
			{
				wipf[0] = '\0';
			}
/*
.....Create Apt Source file
*/
			UM_init_f77_str (asn,fpartf,UX_MAX_PATH_LEN);
			strcpy(fpartf,partf);
			nc = strlen(fpartf);
			for (i=nc;i<UX_MAX_PATH_LEN;i++) fpartf[i] = ' ';
			ncvclf(&iclf,UM_addr_of_f77_str(asn),&kbeg,&kend,&kerr);
			if (kerr != 0)
			{
				status = UU_FAILURE;
				ud_prmerr ("Could not create NCLIPV part file.");
				goto endit;
			}
/*
.....Build command line
....."ncv -f -i stk=PART.stk tp=PART_#.ipv wip=PART_(#-1).wip"
*/
			strcpy (lcom,"nclipv -f -i ");
			inum = strlen(UL_stk_file);
			ul_strip_blanks (UL_stk_file,&inum);
			if (strlen(wipf) == 0 && inum != 0)
			{
				strcat (lcom,"stk=");
				strcat (lcom,UL_stk_file);
				strcat (lcom," ");
			}
			strcat (lcom,"tp=");
			strcat (lcom,partf);
			if (strlen(wipf) != 0)
			{
				strcat (lcom," wip=");
				strcat (lcom,wipf);
			}
/*
.....Spawn NCV
*/
			pcnt++;
/*		printf("%s\n",lcom);*/
			ul_spawn (lcom,0);
		}
	}
/*
.....STANDALONE VERIFY
.....Spawn NCV
*/
	else if (ifl == 2)
	{
/*
.....Terminate new NCLIPV
*/
		if (LW_version == LW_MWORKS)
		{
			ul_ipv_end();
		}
/*
.....Old NCLIPV
*/
		else
		{
#if UU_COMP == UU_WIN2K
			goto failed;
#endif
			strcpy (lcom,"nclipv -f");
			ul_spawn (lcom,0);
		}
	}
/*
.....TOOLPATH ON
.....Start new SEQUNC
*/
	else if (ifl == 3)
	{
		if (UU_application != UU_NCLCAM)
		{
			status = UU_FAILURE;
			ud_wrerr ("An NCLIPV Toolpath can only be initiated from CAM.");
			goto endit;
		}
		nclu_sequnc(1);
	}
/*
.....TOOLPATH OFF
.....Terminate current SEQUNC
*/
	else if (ifl == 4)
	{
		nclu_sequnc(2);
	}
/*
.....REPLAY
*/
	else if (ifl == 5)
	{
/*
........List Replay files
*/
/*
.....we will use file browser in ul_list_program_names
.....so we don't need status window 
.....Yurong 9/24/98
*/
		fnam[0] = '\0';
		ud_get_filename("NCLIPV Playback Files","NCLIPV Playback Files","*.rep",
			fnam,&nc1, "NCLIPV Playback Files (*.rep)", 1,UU_FALSE);
/*
........Spawn REPLAY
*/
		if (nc1 > 0)
		{
			sprintf(lbuf,"replay %s",fnam);
			ul_spawn(lbuf,0);
		}
/*		UD_UNMARK(markval);            */
	}
/*
.....UNRECOGNIZED OPTION
*/
	else
	{
		status = UU_FAILURE;
		ud_wrerr ("Invalid call to ul_verify");
	}
	goto endit;
/*
.....Standalone IPV not valid on Windows NT
*/
failed:;
	ud_wrerr("You are not authorized to run NCLIPV on this terminal.");
/*
.....End of routine
*/
endit:;
	return(status);
}

/*********************************************************************
**	 E_FUNCTION:int ul_verify_end()
**		This function deletes the NCLIPV Tool Path and Work in
**		Progress files, if requested in the IPV Modals form.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_verify_end()
{
	char lcom[UX_MAX_FILE_LEN+40];
/*
.....Build command line
....."rm -f PART_*.ipv PART_*.wip"
*/
	if ((UL_del_ipv == 1 || UL_del_wip == 1) && pcnt > 0)
	{
#if UU_COMP == UU_VAXVMS
		strcpy (lcom,"delete ");
		if (UL_del_ipv == 1)
		{
			strcat (lcom,UL_program);
			strcat (lcom,"_*.ipv;*");
		}
		if (UL_del_wip == 1)
		{
			if (UL_del_ipv == 1) strcat (lcom,",");
			strcat (lcom,UL_program);
			strcat (lcom,"_*.wip;*");
		}
#else
		strcpy (lcom,"rm -f ");
		if (UL_del_ipv == 1)
		{
			strcat (lcom,UL_program);
			strcat (lcom,"_*.ipv ");
		}
		if (UL_del_wip == 1)
		{
			strcat (lcom,UL_program);
			strcat (lcom,"_*.wip");
		}
#endif
/*
.....Delete files
*/
		ul_spawn (lcom,0);
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}
/*********************************************************************
**	 E_FUNCTION : ul_verify_modals()
**			This function handles the NCLIPV modals form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Initializes NCL502 variables.
**	 WARNINGS:
*********************************************************************/
void ul_verify_modals()
{
	int delipv[4],delwip[4],npts[4];
	UX_pathname sfile;
	int option, status;
	int *ans[5];	/* default answers/answers for form */
	UU_LOGICAL cmdreject;
	UD_FSTAT uj_browse_file();
	static UD_METHOD methods[] = {UU_NULL,uj_browse_file,
											UU_NULL,UU_NULL,UU_NULL };
	static char called[]       = {6,6,6,6,6};
/*
.....Use new NCLIPV
*/
		if (LW_version == LW_MWORKS)
		{
			ul_ipv_modal_form();
			return;
		}
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_verify_modals()"));
/*
.....Load the input values into
.....local storage area
*/
	strcpy (sfile,UL_stk_file);
	npts[0] = UL_ipv_npts;
	delipv[0] = UL_del_ipv;
	delwip[0] = UL_del_wip;
/*
.....Field 0 is Stock File Name
*/
	ans[0] = (int *)&sfile[0];
/*
..... added "Browse" button
.....Yurong 9/24/98
*/
	ans[1] = (int *)&option;
/*
.....Field 2 is Number of points per curve
*/
	ans[2] = (int *)&npts[0];
/*
.....Field 3 is Delete all Tool Paths
*/
	ans[3] = (int *)&delipv[0];
/*
.....Field 4 is Delete all WIP files
*/
	ans[4] = (int *)&delwip[0];
/*
.....set file filter for browse button
*/
	uj_browse_set_filter(1, "*.stk","NCLIPV Stock file name (*.stk)");
/*
.....Get the Form input
*/
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
		status = ud_form1("lipv.frm", ans, ans, methods, called, UU_NULL, UU_NULL);
		if (status==-1)
			goto done;
	}	
	else
		goto done;
/*
.....Store IPV Modals
*/
	strcpy(UL_stk_file,sfile);
	UL_ipv_npts = npts[0];
	UL_del_ipv = delipv[0];
	UL_del_wip = delwip[0];
done:
	uj_browse_reset_filter(1);
	UD_UNMARK(cmdreject);
	uu_dexit;
	return;
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_open(fd)
**      This function opens the IPV stock file for writing.  It will
**		create a new file if it does not already exist or it will ask
**		the user if they want to append to an existing stock file.
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : fd = File descriptor of stock file.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_open(fd)
	FILE **fd;
{
	int iapp,fstat,mode,stat;
	UX_pathname lbuf;
	FILE *fdes;
/*
.....Check the existence of the stock file
*/
	iapp = 0;
	mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
	stat = ux_file_inquire(UU_NULL,UU_NULL,UL_stk_file,UU_NULL,
				UU_NULL,&mode,&fstat,lbuf,UX_NPRTERRS);
	if (stat != UU_SUCCESS || mode != (mode|UX_NEXISTS|UX_CREATE))
	{
/*
.....File already exists
.....Ask if the user wants to append to it
*/
		if (mode == (mode|UX_EXISTS))
		{
			sprintf (lbuf,
				"%s already exists.\nDo you wish to append to existing file?",
				UL_stk_file);
			stat = ud_yesno (0, lbuf, "File exists");
			if (stat==1) iapp = 1;
		}
/*
.....File cannot be created
*/
		else
		{
			goto failure;
		}
	}
/*
.....Create the stock file
*/
	if (iapp == 0)
	{
		stat = ux_fopen0(UL_stk_file,"w",&fdes);
		if (stat != UU_SUCCESS) goto failure;
		*fd = fdes;
	}
/*
.....Append to existing stock file
*/
	else
	{
		stat = ux_fopen0(UL_stk_file,"a",&fdes);
		if (stat != UU_SUCCESS) goto failure;
		*fd = fdes;
	}
	goto done;
/*
.....Error trying to create stock file
*/
failure:;
	sprintf (lbuf,"Cannot create %s.",UL_stk_file);
	ud_wrerr (lbuf);
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	return(stat);
}
/*********************************************************************
**
**    E_FUNCTION         :  ud_chain_modals()  
**       changes the list of conditions for ud_chain().
**
**    PARAMETERS
**       INPUT  :  
**       OUTPUT :
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
nclu_ipv_form(iclf,kbeg,kend,cpart,cwip)
UN_clstruc **kbeg,**kend;
UM_int2 *iclf;
char cpart[],cwip[];
{
/*
.....added "Browse" button to fields correspond to file
.....Yurong 9/24/98
*/
	int stat,isav,nc,fstat,mode,irtn;
	UN_clstruc *clst,*clen;
	char *p,*ux_getenv();
	UX_pathname buf,tmpf,filter,ext1,descrip;
	char *seqerr = "Could not find sequence: %s";
	UM_int2 ival1,jfl,istat,ifl,ival;
	UM_f77_str label;
	UU_LOGICAL cmdreject;
	int status,relnum;
	NCL_cmdbuf cmdbuf;
	UN_sim_machine mach;
	UU_KEY_ID key;
/*
.....Set up form fields
*/
	UD_FSTAT uj_browse_file();
	static int option;
	static UX_pathname tclfil;
	static char tscmd[42],tecmd[42],tmx[66];
	static char traverse[]     = {1,0,0, 1,0,0, 1,0,0, 1, 1, 1,0};
	static UD_METHOD methods[] = {filtog,UU_NULL,uj_browse_file,
											filtog,UU_NULL,UU_NULL,
	                              filtog,UU_NULL,UU_NULL,
											UU_NULL, UU_NULL,filtog,UU_NULL};
	static char called[]       = {6,6,6, 6,6,6, 6,6,6, 6, 6, 6,6};
	static int *ans[] = {&tsrc, (int *)&tclfil[0], (int *)&option, &tstrt,
	                     (int *)&tsseq[0], (int *)&tscmd[0], &tend,
	                     (int *)&teseq[0], (int *)&tecmd[0],
	                     (int *)&tpartf[0], (int *)&twipf[0],
								(int *)&ttra,(int *)&tmx};
	status = 0;
/*
.....Set up the field entries
*/
	tsrc = fsrc; strcpy(tclfil,clfil);
	tstrt = fstrt; strcpy(tsseq,fsseq); strcpy(tscmd,fscmd);
	tend = fend; strcpy(teseq,feseq); strcpy(tecmd,fecmd);
	ifl = 353;
	getifl(&ifl,&ival);
	ttra = ival;
	tmx[0] = '\0';
/*
........Using current clfile and
........starting at a SEQUNC
*/
	if (tsrc == 0 && tstrt == 1)
	{
		ival1 = 0;
		ncl_clseq_label(&ival1,tsseq,1);
		strcpy(teseq,tsseq);
/*
...........Part file is set to auto
*/
		if (tend == 1) strcpy(tpartf,teseq);
		else strcpy(tpartf,tsseq);
		strcpy(twipf,tsseq);
		ncl_clseq_label(&ival1,twipf,2);
	}
/*
.....Make sure WIP file exists
*/
	if (twipf[0] != '\0')
	{
		strcpy (tmpf,UL_program);
		strcat (tmpf,"_");
		strcat (tmpf,twipf);
		strcat (tmpf,".wip");
	    mode = UX_EXISTS|UX_READ;
	    stat = ux_file_inquire(UU_NULL,UU_NULL,tmpf,UU_NULL,
				UU_NULL,&mode,&fstat,buf,UX_NPRTERRS);
		if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
			twipf[0] = '\0';
	}
/*
.....Enable correct fields
.....based on DISPLAY field value
*/
form:;
	traverse[1] = 0;
	traverse[2] = 0;
	if (tsrc == 1) 
	{
		traverse[1] = 1;
		traverse[2] = 1;
	}

	traverse[4] = 0; traverse[5] = 0;
	if (tstrt == 1) traverse[4] = 1;
	if (tstrt == 2) traverse[5] = 1;

	traverse[7] = 0; traverse[8] = 0;
	if (tend == 1) traverse[7] = 1;
	if (tend == 2) traverse[8] = 1;

	traverse[12] = 0;
	if (ttra == 1) traverse[12] = 1;
/*
.....set file filter for browse button
*/
	strcpy(filter,"*.");
	strcpy(descrip, "Clfiles (");
	p = ux_getenv("UL_CLFILE1_SUFFIX",UX_NPRTERRS);
	if (p!=NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(filter,ext1);
	}	
	else 
		strcat(filter,"cl");

	strcat(descrip, filter);
	strcat(descrip, ")|Secondary Clfiles (*.");
			
	p = ux_getenv("UL_CLFILE2_SUFFIX");
	strcat(filter,"|*.");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(filter,ext1);
		strcat(descrip, ext1);
	}
	else
	{
		strcat(filter,"cln");
		strcat(descrip, "cln");
	}
	strcat(descrip, ")|Simulation Files (*.sim)");
	strcat(filter, "|*.sim");
	uj_browse_set_filter(2, filter, descrip);
/*
.....Get the Form input
*/
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
		status = ud_form1("nclipv.frm", ans, ans, methods, called, UU_NULL, traverse);
		if (status==-1)
			goto failed;
	}
	else
	{
		goto failed;
	}
	ul_to_upper(tsseq); ul_to_upper(tscmd);
	ul_to_upper(teseq); ul_to_upper(tecmd);
/*
.....Load an external clfile
*/
	isav = *iclf;
	if (tsrc != 1)
	{
		*iclf = 0;
	}
	else
	{
		*iclf = 1;
		jfl = 0;
		nc = strlen(tclfil);
/*
........Try and load as a Simulation file first
*/
		irtn = ncl_simulate_load(tclfil,&mach);
/*
.....Load as a clfile
*/
		if (irtn != UU_SUCCESS)
		{
			UM_init_f77_str(label,tclfil,nc);
			clload(iclf,UM_addr_of_f77_str(label),&nc,&jfl,&istat);
			if (istat != 0)
			{
				*iclf = isav;
				sprintf(buf,"Could not load external clfile: %s",tclfil);
				ud_wrerr(buf);
/*				goto form;             */
				goto failed;
			}
		}
	}
/*
.....Set up the starting and ending
.....clfile locations for playback
*/
	stat = ncl_get_clseq(tsseq,tscmd,tstrt,teseq,tecmd,tend,&clst,&clen);
	if (stat != 0) *iclf = isav;
	if (stat == 1)
	{
		if (tstrt == 1) sprintf(buf,seqerr,tsseq);
		else if (tstrt == 2) sprintf(buf,seqerr,tscmd);
		else sprintf(buf,seqerr," ");
		ud_prmerr(buf);
/*		goto form;              */
		goto failed;
	}
	else if (stat == 2)
	{
		if (tend == 1) sprintf(buf,seqerr,teseq);
		else if (tend == 2) sprintf(buf,seqerr,tecmd);
		else sprintf(buf,seqerr," ");
		ud_prmerr(buf);
/*		goto form;              */
		goto failed;
	}
	*kbeg = clst; *kend = clen;
	strcpy(cpart,tpartf); strcpy(cwip,twipf);
/*
.....Load the current clfile parameters
.....when starting from a "*Command"
*/
	if (tstrt != 1) ncl_fill_sequnc(clst);
/*
.....Create simulation file for current clfile
*/
	if (tsrc == 2)
	{
		irtn = ncl_create_simulate(&clst,&clen,0,-1);
		if (irtn != UU_SUCCESS)
		{
			*iclf = isav;
			goto form;
		}
		*iclf = 1;
		*kbeg = UU_NULL; *kend = UU_NULL;
	}
/*
.....TRACUT APT source file
*/
	if (ttra == 1)
	{
		ul_strip_blanks(tmx,&nc);
		ul_to_upper(tmx);
		if (nc != 0)
		{
			UM_init_f77_str(label,tmx,nc);
			getkey(UM_addr_of_f77_str(label),&key);
			if (key == 0)
			{
				sprintf(buf,"Matrix '%s' does not exist.",tmx);
				ud_prmerr(buf);
				goto failed;
			}
			else
			{
				ur_retrieve_data_relnum(key,&relnum);
				if (relnum != NCL_MATRIX_REL)
				{
					sprintf(buf,"'%s' is not a matrix.",tmx);
					ud_prmerr(buf);
					goto failed;
				}
			}
		}
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf,NCL_set,NCL_nocomma);
		ncl_add_token(&cmdbuf,NCL_cmd_aptsrc,NCL_comma);
		if (nc != 0)
		{
			ncl_add_token(&cmdbuf,NCL_mtracut,NCL_comma);
			ncl_add_token(&cmdbuf,tmx,NCL_nocomma);
		}
		else
		{
			ncl_add_token(&cmdbuf,NCL_mtracut,NCL_nocomma);
		}
	}
	else
	{
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf,NCL_reset,NCL_nocomma);
		ncl_add_token(&cmdbuf,NCL_cmd_aptsrc,NCL_comma);
		ncl_add_token(&cmdbuf,NCL_mtracut,NCL_nocomma);
	}
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....Save the form entries
*/
	fsrc = tsrc; strcpy(clfil,tclfil);
	fstrt = tstrt; strcpy(fsseq,tsseq); strcpy(fscmd,tscmd);
	fend = tend; strcpy(feseq,teseq); strcpy(fecmd,tecmd);
	strcpy(fpartf,tpartf);
	strcpy(fwipf,twipf);
	goto done;
failed:
	status = -1;
done:
	UD_UNMARK(cmdreject);
	uj_browse_reset_filter(2);
	return status;
}

