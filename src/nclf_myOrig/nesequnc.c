/*********************************************************************
**    NAME         :  nesequnc.c
**       CONTAINS:
**			seqsto(iclf,lab,nc)
**			seqend(iclf)
**			ncl_clseq_label(iclf,label,ifl)
**			ncl_delete_sequnc(iclf,label)
**			ncl_parse_postcmd(scmd,ktyp,token,ntok)
**			ncl_get_clseq(sseq,scmd,sfl,eseq,ecmd,efl,strt,send)
**			ncl_find_clcmd(token,ntok,idir,strt)
**			ncl_fill_sequnc(strt)
**			nclu_sequnc(kfl)
**			ncl_list_clseq(iclf)
**			ncl_endlist_clseq()
**			gclinf(spt,cutr,fed,npt)
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nesequnc.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:47
*********************************************************************/

#include "usysdef.h"
#include "modef.h"
#include "mfort.h"
#include "lcom.h"
#include "nclfc.h"
#include "nclfile.h"
#include "nclmplay.h"
#include "nclcmd.h"
#include "uhep.h"
#include "udforms.h"
#include "lipv.h"

void seqend();
void ncl_find_clcmd();
void ncl_list_clseq();
void ncl_endlist_clseq();

typedef struct
{
	int type; int voc; UU_REAL val; char str[22];
} S_token_struc;

char *uu_malloc();

/*********************************************************************
**    E_FUNCTION     : seqsto(iclf,lab,nc)
**			FORTRAN callable routine to save SEQUNC information for
**			quick reference.  The SEQUNC label, starting clfile
**			location and ending clfile location are saved.
**    PARAMETERS   
**       INPUT  : 
**			iclf    = 0 = Primary clfile.  1 = Secondary clfile.
**			lab     = SEQUNC label.
**			nc      = Number of chars in 'lab'.
**       OUTPUT :  
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void seqsto(iclf,lab,nc)
UM_f77_str_ptr lab;
UM_int2 *nc,*iclf;
{
	int inc,ipt,spt,i;
	char *p,buf[22];
/*
.....Store sequence label and
.....internal clfile pointer
*/
	ipt = *iclf;
	p = UM_cstr_of_f77_str(lab);
	inc = *nc;
	strncpy(buf,p,inc);
	ul_strip_blanks(buf,&inc);
	buf[inc] = '\0';
	spt = UN_clseq[ipt] + 1;
    for (i=0;i<=UN_clseq[ipt];i++)
	{
		if (strcmp(buf,UN_clseq_label[ipt][i]) == 0)
		{
			spt = i;
			break;
		}
	}
	if (spt < MAXCLSEQ)
	{
		if (UN_clseq[ipt] < spt) UN_clseq[ipt] = spt;
		seqend(iclf);
		strcpy(UN_clseq_label[ipt][spt],buf);
		UN_clseq_rec[ipt][spt] = UN_clpt[ipt];
		UN_clseq_end[ipt][spt] = UU_NULL;
		UN_clseq_cur[ipt] = spt;
	}
/*
.....added to status bar
*/
	uz_actsequnc("");
}

/*********************************************************************
**    E_FUNCTION     : seqend(iclf)
**			FORTRAN callable routine to save the ending clfile location
**			for the active toolpath (SEQUNC).
**    PARAMETERS   
**       INPUT  : 
**			iclf    = 0 = Primary clfile.  1 = Secondary clfile.
**       OUTPUT :  
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void seqend(iclf)
UM_int2 *iclf;
{
	int ipt,spt;
	ipt = *iclf;
	if (UN_clseq_cur[ipt] >= 0)
	{
		spt = UN_clseq_cur[ipt];
		if (UN_clseq_end[ipt][spt] == UU_NULL)
			UN_clseq_end[ipt][spt] = UN_clpt[ipt];
		UN_clseq_cur[ipt] = -1;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_clseq_label(iclf,label,ifl)
**			Returns the current, previous, or specified SEQUNC label,
**			depending on 'ifl'.
**    PARAMETERS   
**       INPUT  : 
**			iclf    = 0 = Primary clfile.  1 = Secondary clfile.
**			ifl     = 0 - Return current OPEN SEQUNC label.
**			          1 - Return current or previous (if none OPEN)
**			              SEQUNC label.
**			          2 - Return SEQUNC lable prior to SEQUNC specified
**			              by 'label'
**			label   = SEQUNC label when 'ifl = 2'.
**       OUTPUT :  
**			label   = SEQUNC label requested, or blank if one was not
**			          found.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_clseq_label(UM_int2 *iclf,char* label, int ifl)
//UM_int2 *iclf;
//int ifl;
//char *label;
{
	int ipt,spt,i;
/*
.....Return active SEQUNC if any
*/
	ipt = *iclf;
	if (UN_clseq[ipt] >= 0)
	{
		if (ifl != 2)
		{
			spt = UN_clseq[ipt];
			if (UN_clseq_end[ipt][spt] != UU_NULL && ifl == 0) label[0] = '\0';
			else strcpy(label,UN_clseq_label[ipt][spt]);
		}
		else
		{
			spt = -1;
			for (i=0;i<=UN_clseq[ipt];i++)
			{
				if (strcmp(label,UN_clseq_label[ipt][i]) == 0)
				{
					spt = i - 1;
					break;
				}
			}
			if (spt >= 0) strcpy(label,UN_clseq_label[ipt][spt]);
			else label[0] = '\0';
		}
	}
	else
	{
		label[0] = '\0';
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_delete_sequnc(iclf,label)
**			Deletes an entire toolpath (SEQUNC) out of the clfile.
**    PARAMETERS   
**       INPUT  : 
**			iclf    = 0 = Primary clfile.  1 = Secondary clfile.
**			label   = SEQUNC label to delete.
**       OUTPUT :  
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_delete_sequnc(iclf,label)
UM_int2 *iclf;
char label[];
{
	int ipt,spt,i;
/*
.....Delete Sequence index and associated data
*/
	ipt = *iclf;
	if (UN_clseq[ipt] >= 0)
	{
		spt = -1;
		for (i=0;i<=UN_clseq[ipt];i++)
		{
			if (strcmp(label,UN_clseq_label[ipt][i]) == 0)
			{
				spt = i;
				break;
			}
		}
		if (spt >= 0)
		{
			for (i=spt;i<UN_clseq[ipt];i++)
			{
				UN_clseq_rec[ipt][i] = UN_clseq_rec[ipt][i+1];
				UN_clseq_end[ipt][i] = UN_clseq_end[ipt][i+1];
				strncpy(UN_clseq_label[ipt][i],UN_clseq_label[ipt][i+1],22);
			}
			if (UN_clseq_cur[ipt] == spt) UN_clseq_cur[ipt] = -1;
			else if (UN_clseq_cur[ipt] > spt) UN_clseq_cur[ipt]--;
			UN_clseq[ipt]--;
		}
	}
/*
.....added to status bar
*/
	uz_actsequnc("");
}

/*********************************************************************
**    E_FUNCTION     : ncl_parse_postcmd(scmd,ktyp,token,ntok)
**			Parses a text string which should contain a valid post-
**			processor command.
**    PARAMETERS   
**       INPUT  : 
**			scmd    = Text string to parse.
**       OUTPUT :  
**			ktyp    = -1 - Invalid post-processor command.
**			          0  - 'scmd' contains numeric value (ISN).
**			          1  - 'scmd' contains valid post command.
**			token   = Structure array which contains parsed post
**			          command.
**			             .typ = 0 - Blank token.
**			                    1 - Vocabulary word.
**			                    2 - Numeric value.
**			                    3 - Text string.
**			             .voc = Vocabulary word value when 'typ' = 1.
**			             .val = Numeric value when 'typ' = 2.
**			             .str = Text string when 'typ' = 3.
**			ntok    = Number of tokens returned.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_parse_postcmd(scmd,ktyp,token,ntok)
char *scmd;
int *ktyp,*ntok;
S_token_struc token[20];
{
	int jnum,inc,nc,idel,stat,knc;
	char ltok[22],cbuf[80];
	UU_REAL rnum;
	UM_int2 inum;
	UM_f77_str label;
/*
.....Check for text post-processor command
.....LETTER, PPRINT, PARTNO, & INSERT
*/
	jnum = 0;
	if (strncmp(scmd,"LETTER",6) == 0) jnum = 1043;
	else if (strncmp(scmd,"PPRINT",6) == 0) jnum = 1044;
	else if (strncmp(scmd,"PARTNO",6) == 0) jnum = 1045;
	else if (strncmp(scmd,"INSERT",6) == 0) jnum = 1046;
	if (jnum != 0)
	{
		token[0].type = 1;
		token[0].voc = jnum;
		token[1].type = 3;
		strncpy(token[1].str,&scmd[6],20);
		*ntok = 2;
	}
/*
.....Initialize routine
*/
	*ktyp = -1;
	*ntok = 0;
	strcpy(cbuf,scmd);
	strcat(cbuf,",");
	knc = strlen(cbuf);
	inc = -1;
	nc = 0;
/*
.....Parse input buffer
*/
	while (inc < knc)
	{
		inc++;
/*
........Check for valid delimiter (/,)
*/
		idel = 0;
		if (cbuf[inc] == '/')
		{
			if (*ntok != 0) goto failed;
			idel = 1;
		}
		else if (cbuf[inc] == ',')
		{
			idel = 1;
		}
/*
........Part of token
*/
		else if (cbuf[inc] != ' ' && cbuf[inc] != '	')
		{
			if (nc >= 20) goto failed;
			ltok[nc] = cbuf[inc];
			nc++;
		}
/*
........Delimiter found
........Parse token
*/
		if (idel == 1)
		{
			if (*ntok >= 20) goto failed;
			ltok[nc] = '\0';
/*
...........Blank token
*/
			if (nc == 0)
			{
				token[*ntok].type = 0;
			}
/*
...........Number
*/
			else
			{
				stat = ul_to_reals(&rnum,&jnum,1,ltok);
				if (stat == UU_SUCCESS)
				{
					token[*ntok].type = 2;
					token[*ntok].val = rnum;
				}
/*
...........Vocabulary word
*/
				else
				{
					UM_init_f77_str(label,ltok,nc);
					voccad(UM_addr_of_f77_str(label),&inum);
					if (inum != 0)
					{
						token[*ntok].type = 1;
						token[*ntok].voc = inum;
						strcpy(token[*ntok].str,ltok);
					}
/*
...........Text string
*/
					else
					{
						token[*ntok].type = 3;
						strcpy(token[*ntok].str,ltok);
					}
				}
			}
			*ntok = *ntok + 1;
			nc = 0;
		}
	}
/*
.....Determine type of command
*/
	if (*ntok == 0) goto failed;
	if (token[0].type == 2)
	{
		if (*ntok != 1) goto failed;
		*ktyp = 0;
		goto done;
	}
	else if (token[0].type == 1)
	{
		*ktyp = 1;
		goto done;
	}
failed:;
	*ktyp = -1;
done:;
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_clseq(sseq,scmd,sfl,eseq,ecmd,efl,strt,send)
**			Searches the clfile for a beginning and ending Toolpath
**			(SEQUNC) and/or post-processor commmand.  Returns the
**			beginning and ending clfile record numbers obtained from
**			the search.
**    PARAMETERS   
**       INPUT  : 
**			sseq    = Starting SEQUNC label when 'sfl' = 1.
**			scmd    = Starting post command when 'sfl' = 2.
**			sfl     = 0 = Toolpath starts at beginning of file.
**			          1 = Toolpath starts at specified SEQUNC.
**			          2 = Tollpath starts at post-processor command.
**			eseq    = Ending SEQUNC label when 'efl' = 1.
**			ecmd    = Ending post command when 'efl' = 2.
**			efl     = 0 = Toolpath ends at end of file.
**			          1 = Toolpath ends at specified SEQUNC.
**			          2 = Tollpath ends at post-processor command.
**       OUTPUT :  
**			strt    = Starting clfile record for this Toolpath.
**			send    = Ending clfile record for this Toolpath.
**    RETURNS      : 0 when Toolpath was found.  Otherwise 1.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_get_clseq(sseq,scmd,sfl,eseq,ecmd,efl,strt,send)
char *sseq,*eseq,*scmd,*ecmd;
int sfl,efl;
UN_clstruc **strt,**send;
{
	int ilabst,i,status,ktyp,ntok;
	S_token_struc token[20];
/*
.....Find the beginning sequence
*/

	ilabst = 0;
	status = 0;
	switch (sfl)
	{
	case 0:
		*strt = UU_NULL;
		break;
	case 1:
		status = 1;
		i = 0;
		while (i<=UN_clseq[UN_clfile] && status == 1)
		{
			if (strcmp(sseq,UN_clseq_label[UN_clfile][i]) == 0)
			{
				status = 0;
				*strt = UN_clseq_rec[UN_clfile][i];
				ilabst = i;
			}
			i++;
		}
		break;
	case 2:
		ncl_parse_postcmd(scmd,&ktyp,token,&ntok);
		if (ktyp == -1)
		{
			status = 1;
		}
		else
		{
			*strt = UU_NULL;
			ncl_find_clcmd(token,ntok,-1,strt);
			if (*strt == UU_NULL)
			{
				status = 1;
			}
		}
		break;
	default:
		status = 1;
	}
	if (status != 0) goto done;
/*
.....Find the ending sequence
*/
	switch (efl)
	{
	case 0:
		*send = 0;
		break;
	case 1:
		status = 2;
		i = ilabst;
		while (i<=UN_clseq[UN_clfile] && status == 2)
		{
			if (strcmp(eseq,UN_clseq_label[UN_clfile][i]) == 0)
			{
				status = UU_SUCCESS;
				*send = UN_clseq_end[UN_clfile][i];
			}
			i++;
		}
		break;
	case 2:
		ncl_parse_postcmd(ecmd,&ktyp,token,&ntok);
		if (ktyp == -1)
		{
			status = 2;
		}
		else
		{
			*send = *strt;
			ncl_find_clcmd(token,ntok,1,send);
			if (*send == UU_NULL)
			{
				status = 2;
			}
		}
		break;
	default:
		status = 2;
		break;
	}
/*
.....Return status
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_find_clcmd(token,ntok,idir,strt)
**			Searches the clfile for a specific post-processor command.
**			Returns the record number which contains this command.
**    PARAMETERS   
**       INPUT  : 
**			token   = Token array which contains post-processor
**			          command to search for.
**			ntok    = Number of tokens in 'token'.
**			idir    = -1 = Search backwards for this post command.
**			          1  = Search forwards.
**			strt    = Clfile record to start search from.
**       OUTPUT :  
**			strt    = Clfile record which contains this post commmand.
**			          Returns -1 when the post command was not found.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_find_clcmd(token,ntok,idir,strt)
S_token_struc token[20];
int ntok,idir;
UN_clstruc **strt;
{
	int jnum,voc,i;
	char *cpt;
	UM_int2 *ipt,jerr;
	UM_int4 iclw[6];
	UN_clstruc *irec;
	UM_real8 rclw[245],fabs();
/*
.....Read a clfile record
*/
	irec = *strt;
	do
	{
		if (idir == -1) clprev(&UN_clfile,&irec,iclw,rclw);
		else clread(&UN_clfile,&irec,iclw,rclw,&jerr);
		if (jerr == 1) goto failed;
/*
........Check for ISN match
*/
		if (token[0].type == 2)
		{
			jnum = token[0].val;
			if (iclw[0] == jnum) goto done;
		}
/*
........Check for post command match
*/
		else
		{
			voc = token[0].voc;
/*
...........FROM,GOTO
*/
			if (voc == 701 || voc == 703)
			{
				if (ntok > 6) goto failed;
				if (ntok > (iclw[4]+1)) goto endloop;
				if (iclw[2] == 5000)
				{
					if (voc == 701 && iclw[3] != 3) goto endloop;
					if (voc == 703 && iclw[3] == 3) goto endloop;
					for (i=1;i<ntok;i++)
					{
						if (token[i].type != 2 && token[i].type !=0) goto failed;
						if (token[i].type == 2 &&
							fabs(token[i].val-rclw[i-1]) > UM_FUZZ)
								goto endloop;
					}
					goto done;
				}
			}
/*
...........CUTTER
*/
			else if (voc == 716)
			{
/*
..............CUTTER/DISPLY
*/
				if (ntok > 1 && token[1].type == 1 && token[1].voc == 1021)
				{
					if (iclw[2] == 7100)
					{
						if (ntok == 2) goto done;
/*
.................CUTTER/DISPLY,parms
*/
						if (iclw[3] == 1 && token[2].type == 2)
						{
							if (ntok > 8) goto failed;
							if (ntok > (iclw[4]+1)) goto endloop;
							for (i=2;i<ntok;i++)
							{
								if (token[i].type != 2 && token[i].type !=0)
									goto failed;
								if (token[i].type == 2 &&
									fabs(token[i].val-rclw[i-2]) > UM_FUZZ)
										goto endloop;
							}
							goto done;
						}
/*
.................CUTTER/DISPLY,symbol
*/
						else if (iclw[3] == 2 && token[2].type != 2)
						{
							if (ntok < 3 || ntok > 4) goto endloop;
							if (token[1].type == 2) goto endloop;
							if (ntok > (iclw[4]+1)) goto endloop;
							cpt = (char *)&rclw[0];
							jnum = strlen(cpt);
							ul_strip_blanks(cpt,&jnum);
							if (strcmp(token[2].str,cpt) != 0)
								goto endloop;
							if (ntok == 4)
							{
								if (token[3].type != 2) goto failed;
								if (fabs(token[3].val - rclw[2]) > UM_FUZZ)
									goto endloop;
							}
							goto done;
						}
/*
.................CUTTER/DISPLY,(PART,ALL)
*/
						else if (iclw[3] == 3)
						{
							if (ntok != 3) goto endloop;
							if (token[2].type != 1 || (token[2].voc != 166 &&
								token[2].voc != 816)) goto endloop;
							ipt = (UM_int2 *)&rclw[1];
							if (ipt[3] == 0 && token[2].type == 166)
								goto done;
							if (ipt[3] == 1 && token[2].type == 816)
								goto done;
						}
					}
				}
/*
..............CUTTER/parms
*/
				else
				{
					if (iclw[2] == 6000)
					{
						if (ntok > 7) goto failed;
						if (ntok > (iclw[4]+1)) goto endloop;
						for (i=1;i<ntok;i++)
						{
							if (token[i].type != 2 && token[i].type !=0)
								goto failed;
							if (token[i].type == 2 &&
								fabs(token[i].val-rclw[i-1]) > UM_FUZZ)
									goto endloop;
						}
						goto done;
					}
				}
			}
/*
...........TRACUT
*/
			else if (voc == 812)
			{
				if (iclw[2] == 7000)
				{
					if (ntok > 13) goto failed;
					for (i=1;i<ntok;i++)
					{
						if (token[i].type != 2 && token[i].type !=0)
							goto failed;
						if (token[i].type == 2 &&
							fabs(token[i].val-rclw[i-1]) > UM_FUZZ)
								goto endloop;
					}
					goto done;
				}
			}
/*
...........SEQUNC
*/
			else if (voc == 818)
			{
				if (iclw[2] == 7200)
				{
					if (ntok > 2) goto failed;
					if (ntok == 1) goto done;
					cpt = (char *)&rclw[0];
					jnum = strlen(cpt);
					ul_strip_blanks(cpt,&jnum);
					if (iclw[3] == 1 && strcmp(token[1].str,cpt) == 0)
						goto done;
					if (iclw[3] == 2 && strcmp(token[1].str,"END") == 0)
						goto done;
				}
			}
/*
...........MULTAX
*/
			else if (voc == 815)
			{
				if (iclw[2] == 9000)
				{
					if (ntok > 2) goto failed;
					if (ntok == 1) goto done;
					if (token[1].type == 1 && token[1].voc == 71)
					{
						if (iclw[3] == 0) goto done;
					}
					else if (token[1].type == 1 && token[1].voc == 72)
					{
						if (iclw[3] == 1) goto done;
					}
					else
					{
						goto failed;
					}
				}
			}
/*
...........Post-processor command
*/
			{
				if (iclw[2] == 2000 && voc == iclw[3])
				{
					if (ntok > (iclw[4]+1)) goto endloop;
					for (i=1;i<ntok;i++)
					{
						ipt = (UM_int2 *)&rclw[i-1];
						if (token[i].type == 2)
						{
							if (ipt[0] == 0 ||
								fabs(token[i].val-rclw[i-1]) > UM_FUZZ)
									goto endloop;
						}
						else if (token[i].type == 1)
						{
							if (ipt[0] != 0 || token[i].voc != ipt[3])
								goto endloop;
						}
						else if (token[i].type != 0)
						{
							goto failed;
						}
					}
					goto done;
				}
			}
		}
endloop:;
	} while (irec != UU_NULL);
/*
.....Could not find sequence
*/
failed:;
	irec = UU_NULL;
/*
.....End of routine
*/
done:;
	*strt = irec;
}

/*********************************************************************
**    E_FUNCTION     : ncl_fill_sequnc(strt)
**			Scans the clfile for all information needed at the
**			beginning of a tool path (FEDRAT, CUTTER, FROM, MULTAX,
**			etc.).  These values are stored in the playback structure
**			'UN_playparm'.
**    PARAMETERS   
**       INPUT  : 
**			strt    = Clfile record which defines the start of the
**			          Toolpath.  The scan will be from the beginning
**			          of the clfile to this record.
**       OUTPUT :  
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_fill_sequnc(strt)
UN_clstruc *strt;
{
	UN_clstruc *istrt,*icur,*iend;
	int ipt;
	int modals[20];
/*
.....Initialize clfile parameters
.....by scanning through the clfile
.....until we reach the current start point
*/
	ncl_reset_playparm();
	if (strt != 0)
	{
		modals[0] = 0; modals[1] = 100; modals[2] = 0; modals[3] = 1;
		modals[4] = 0; modals[5] = 0; modals[6] = 0; modals[7] = 0;
		modals[8] = 0; modals[9] = 0; modals[10] = 0; modals[11] = 0;
		modals[12] = 0; modals[13] = 0; modals[14] = 0; modals[15] = 0;
		modals[16] = 0;
		istrt = UN_clfile_start;
		icur  = UN_clfile_current;
		ipt   = UN_clfile_curpt;
		iend  = UN_clfile_end;
		UN_clfile_start = 0;
		UN_clfile_current = 0;
		UN_clfile_curpt = 0;
		UN_clfile_end = strt;
		ncl_motion_playback(modals,1,UU_NULL,&LW_tool_list,&LW_ntool);
		UN_clfile_start = istrt;
		UN_clfile_current = icur;
		UN_clfile_curpt = ipt;
		UN_clfile_end = iend;
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_sequnc(kfl)
**			This is the interface routine for manipulating SEQUNCs.
**    PARAMETERS   
**       INPUT  : 
**			kfl     = 0 - List all defined SEQUNCs.
**			          1 - Start a new SEQUNC.
**			          2 - End the current SEQUNC.
**			          3 - Delete an entire Toolpath from the internal
**			              clfile.
**       OUTPUT :  
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_sequnc(int kfl)
//int kfl;
{
	int status;
	int numint,stat,ifl;
	UN_clstruc *strt,*send,*inxt;
	char lcom[80],lbuf[80];
	UM_int2 iclf;
	NCL_cmdbuf cmdbuf;
	switch (kfl)
	{
/*
.....List all sequences
*/
	case 0:
		iclf = 0;
		ncl_list_clseq(&iclf);
		ud_hakt(UX_UDOS,1);
		ncl_endlist_clseq();
		break;
/*
.....Start new sequence
*/
	case 1:
/*
.....use form with sequence list
.....Yurong 9/28/98
*/
/*
........Get Toolpath label to start
*/
		lcom[0] = '\0';
		status = nclu_seqform("nstartseq.frm", lcom, &numint);
		if (status==-1)
			return -1;
		ul_strip_blanks (lcom,&numint);
		if (numint == 0 || numint > 20)
		{
			stat = 1;
		}
/*
........Valid Toolpath label
........start SEQUNC
*/
		else
		{
			stat = 0;
			strcpy(lbuf,"*SEQUNC/");
			strcat(lbuf,lcom);
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf,lbuf,UU_FALSE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
		}
		if (stat == 1)
		{
			ud_wrerr ("Invalid response.");
		}
		sprintf (lbuf,"Toolpath '%s' started.",lcom);
		ud_prmerr (lbuf);
		break;
/*
.....Terminate current SEQUNC
*/
	case 2:
/*
.....Make sure a Toolpath is being created
*/
		iclf = 0;
		ncl_clseq_label(&iclf,lcom,0);
		if (lcom[0] == '\0')
		{
			ud_wrerr ("A Toolpath has not been started.");
			goto endit;
		}
/*
.....Output Toolpath end message
*/
		else
		{
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf,"*SEQUNC/END",UU_FALSE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			sprintf(lbuf,"Toolpath '%s' ended.",lcom);
			ud_prmerr(lbuf);
		}
		break;
/*
.....Delete specified SEQUNC
*/
	case 3:
/*
.....use form with sequence list
.....Yurong 9/28/98
*/
/*
........Get Toolpath label to delete
*/
		if (UN_clseq[0] < 0)
		{
			ud_wrerr("No Toolpaths defined yet.");
			return(UU_FAILURE);
		}
		lcom[0] = '\0';
		numint = 0;
		status = nclu_seqform("nstartseq.frm", lcom, &numint);
		if (status==-1)
			return -1;
		ul_strip_blanks (lcom,&numint);
		ul_to_upper(lcom);
		stat = 0;
		if (numint == 0 || numint > 20)
		{
			stat = 1;
		}
/*
........Valid Toolpath label
........delete SEQUNC
*/
		else
		{
			ifl = 1;
			stat = ncl_get_clseq(lcom,lcom,ifl,lcom,lcom,ifl,&strt,&send);
			if (stat != 0)
			{
				sprintf(lbuf,"Could not find Toolpath: %s",lcom);
			}
			else
			{
				iclf = 0;
				cldel(&iclf,&strt,&send,&inxt);
				ncl_delete_sequnc(&iclf,lcom);
			}
		}
		sprintf(lbuf,"Toolpath '%s' deleted.",lcom);
		ud_prmerr(lbuf);
		break;
	}
endit:;
/*
.....added to status bar
*/
	uz_actsequnc("");
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_list_clseq(iclf)
**			Lists all defined Toolpaths (SEQUNC).
**    PARAMETERS   
**       INPUT  : 
**			iclf   = 0 = List Toolpaths in primary clfile.
**			         1 = Secondary clfile.
**       OUTPUT :  
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#define tabn 10
void ncl_list_clseq(iclf)
UM_int2 *iclf;
{
	char *spac="                    ";
	char buff[250];
	int i,m,n,ipt,wargs[3];
/*
.....Open a scrolling window
*/
	ipt = *iclf;
    wargs[1] = 1;
	if (ul_open_window(UL_winrow,UL_wincol,wargs) != UU_SUCCESS) goto done;
/*
.....Write out the window header
*/
	ul_win_out(" List of programmed Toolpaths:",0);
	ul_win_out(" ",0);
/*
.....There are no such files in this directory
.....Show the empty list
*/
	if (UN_clseq[ipt] < 0)
	{
		ul_win_out ("     No Toolpaths defined yet.",1);
		goto done;
	}
/*
.....List defined Toolpaths
*/
	buff[0] = '\0';
    for (i=0;i<=UN_clseq[ipt];i++)
	{
/*
........Write out the filename to the window
*/
		if (strlen(buff)+strlen(UN_clseq_label[ipt][i])+1 > UL_wincol)
		{
			ul_win_out(buff,1);
			buff[0] = '\0';
		}
		strcat(buff,UN_clseq_label[ipt][i]);
		n = strlen(buff);
		m = (n/tabn+1) * tabn - n;
		if (m+n >= UL_wincol) m = UL_wincol - n - 1;
		strncat (buff,spac,m);
	}
	if (strlen(buff) != 0) ul_win_out (buff,1);
/*
.....End of list reached
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_endlist_clseq()
**			Terminates the Toolpath listing initiated in 'ncl_list_clseq'.
**    PARAMETERS   
**       INPUT  : 
**			none.
**       OUTPUT :  
**			none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_endlist_clseq()
{
	ul_close_window();
}

/*********************************************************************
**    E_FUNCTION     : gclinf(spt,cutr,fed,npt)
**			Returns the current tool location, cutter definition,
**			feed rate, and multax status as stored in the motion
**			playback structure 'UN_playparm'.
**    PARAMETERS   
**       INPUT  : 
**			none.
**       OUTPUT :  
**			spt    = Current tool location and tool axis.
**			cutr   = Current cutter definition.
**			fed    = Current feed rate.
**			npt    = 3 = MULTAX/OFF.  6 = MULTAX/ON.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gclinf(spt,cutr,fed,npt)
UM_real8 spt[],cutr[],*fed;
UM_int2 *npt;
{
	int i;
	for (i=0;i<6;i++)
	{
		spt[i] = UN_playparm.spt[i];
		cutr[i] = UN_playparm.cutr[i];
	}
	*fed = UN_playparm.fedrat[0];
	*npt = UN_playparm.npt;
}

/*********************************************************************
**    E_FUNCTION     : ncl_seqform(formname, seqc, num)
**			Display form with seqence list and get user choice.
**    PARAMETERS   
**       INPUT  : 
**				formname:   form name
**       OUTPUT :  
**			   seqc:       seqence string
**				num:			number of character of seqence string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added by Yurong 9/28/98
*/
nclu_seqform(formname, seqc, num)
char *formname, *seqc;
int *num;
{
	int *ans[1];
	int i, len, status;
	UD_LIST seq_name_list;
	UU_LOGICAL cmdreject;
	static char seqname[22] = " ";

	status = 0;
	seq_name_list.answer = (char *) uu_malloc(80 * sizeof(char));
	strcpy(seq_name_list.answer, seqname);
	if (UN_clseq[0] < 0)
	{
		seq_name_list.num_item = 1;
		seq_name_list.item = (char **) uu_malloc(1*sizeof(char *));
		seq_name_list.item[0] = (char *) uu_malloc(2 * sizeof(char));
		strcpy(seq_name_list.item[0]," ");
	}
	else
	{
		seq_name_list.item = (char **) uu_malloc((UN_clseq[0]+1)*sizeof(char *));
		for (i=0;i<=UN_clseq[0];i++)
		{
			len = strlen(UN_clseq_label[0][i]);
			seq_name_list.item[i] = (char *) uu_malloc(len * sizeof(char));
			strcpy(seq_name_list.item[i], UN_clseq_label[0][i]);
		}
		seq_name_list.num_item = UN_clseq[0]+1;
	}
	ans[0] = (int *)&seq_name_list;
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
		status = ud_form(formname, ans, ans);
		if (status==-1)
			goto done;
	}
	else
		goto done;
	*num = strlen(seq_name_list.answer);
	if (*num>0)
		strcpy(seqc, seq_name_list.answer);
done:
	ud_free_flist(&seq_name_list);
	UD_UNMARK(cmdreject);
	return status;
}
