/*********************************************************************
**    NAME         :  tspstep.c
**       CONTAINS:
**             utp_free_lists
**             utp_read_step_file
**             utp_count_entities
**             utp_get_record
**             utp_summary
**             utp_get_unistat
**             utp_reset_unistat
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**      tspstep.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**      10/27/16 , 15:22:18
*********************************************************************/
#include "usysdef.h"
#include "xenv1.h"
#include <stdio.h>
#include <ctype.h>
#include	"udebug.h"
#include "nclver.h"
#include <io.h>
#include <fcntl.h>
#include	<time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "tstep.h"
#include "nconst.h"
#include "riddle.h"

#define write _write
#define read _read
#define close _close
#define open _open
#define creat _creat
#define lseek _lseek
#define STEP_MAXCHARS 5000
/* static char Sbuf[512]; */
static char Sbuf[STEP_MAXCHARS];
static int Sntrim;
static struct UR_unistat_rec Sunistat;

void utp_reset_unistat();

static void S_get_token();
static void S_parse_error();

/*********************************************************************
**    E_FUNCTION     :  utp_free_lists(flag)
**				Empties the static lists and frees the memory between runs
**          or when STEP is exited.
**    PARAMETERS   
**       INPUT  : 
**            flag     Reset UTP_step_recod if UU_TRUE. (Use on exit)
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_free_lists(flag)
UU_LOGICAL flag;
{
	utp_free_xform_list();
	utp_free_label_list();
	utp_free_attr_lists();
 /*
.....The step record list will be reset when a new file is read so
.....the list only needs to be freed on exit.
*/
	if (flag && UTP_step_record.data != UU_NULL)
		uu_list_free(&UTP_step_record);
}

/*********************************************************************
**    E_FUNCTION     :  utp_read_step_file(filename)
**       UNIVERSAL unblock filea and open file
**    PARAMETERS   
**       INPUT  : 
**          filename  - Name of input STEP file to read.
**       OUTPUT :  
**    RETURNS      : UU_SUCCESS if no errors, UU_FAILURE otherwise.
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
int utp_read_step_file(filename)
char *filename;
{
	int inc,stat,nc,method,subfl,concat;
	char fnam[42],*p,*strchr();
	FILE *fd;
	static char Sfilename[UX_MAX_PATH_LEN]={""};
/*
.....File was already read in
*/
	stat = UU_SUCCESS;
	ul_default_ftype("stp",filename);
	if (strcmp(filename,Sfilename) == 0) goto done;
/*
.....Initialize routine
*/
	ul_short_filename(filename,fnam,40);
/*
.....Output conversion message
*/
	uig_str_out("\nConverting STEP input file to internal format\n",UU_TRUE);
/*
.....Open STEP file
*/
	if (access(filename,0) != 0)
		fd = UU_NULL;
	else
		fd = fopen(filename,"r");
	if (fd == 0) goto file_err;
/*
.....Reset Unibase Statistics
*/
	utp_reset_unistat();
/*
.....Count records
*/
	inc = 0;
	do
	{
		stat = ul_fread(fd,Sbuf,sizeof(Sbuf),&nc);
		if (stat == UX_EOF) break;
		inc++;
	} while (stat == UU_SUCCESS);
	rewind(fd);
/*
.....Read in records
*/
	UTP_step_numrec = inc;
	stat = S_parse_records(fd,&UTP_step_record,&UTP_step_numrec);
	if (stat != UU_SUCCESS) goto done;
/*
.....Open listing file
*/
	open_listing_file(filename,"STEP");
/*
.....Output debug records
*/
	utp_get_label_method(&method,&subfl,&concat);
	if (method == 3) utp_debug_step_file(UU_TRUE);
	strcpy(Sfilename,filename);
	goto done;
/*
.....Error opening file
*/
file_err:;
	sprintf (Sbuf, "\nError in opening file %s.\n", fnam);
	uig_error(Sbuf);
	stat = UU_FAILURE;
	goto done;
/*
.....Parsing error
*/
parse_err:;
	sprintf (Sbuf, "\nError in parsing record %s.\n", Sbuf);
	uig_error(Sbuf);
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	if (fd != 0) fclose(fd);
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     :  utp_count_entities()
**       Counts the number of base entities in the STEP file.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : Number of entities in STEP file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int utp_count_entities()
{
	int i,j,nent;
	UU_LOGICAL ifl=UU_TRUE,found;
	char label[NCL_MAX_LABEL+1];
	UTPs_step_record *ptr;
/*
.....Loop through the STEP DATA records
*/
	ptr = (UTPs_step_record *)UU_LIST_ARRAY(&UTP_step_record);
	nent = 0;
	for (i=0;i<UTP_step_numrec;i++)
	{
		if (ptr[i].used == 0)
		{
			nent++;
			if (ptr[i].command == SHAPE_DEFINITION_REPRESENTATION ||
				ptr[i].command == SHAPE_REPRESENTATION_RELATIONSHIP)
			{
				for (j=0;j<ptr[i].nparm;j++)
				{
					if (ptr[i].parm[j].type == UTP_RECNO)
					{
						found = utp_define_shape_label(ptr[i].parm[j].ptype.recno,
							label,UU_TRUE);
						if (found)
						{
							utp_shape_label_push(label);
							break;
						}
					}
				}
			}
		}
		else if (ifl && ptr[i].command == GEOMETRIC_REPRESENTATION_CONTEXT)
		{
			utp_map_units(&ptr[i]);
			ifl = UU_FALSE;
		}
	}
	return(nent);
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_record(recno)
**          Returns the STEP record pointed to by 'recno'.
**    PARAMETERS   
**       INPUT  : 
**          recno    Record number to obtain.
**       OUTPUT :  none
**    RETURNS      :
**          Pointer to STEP record pointed to by 'recno'.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UTPs_step_record *utp_get_record(recno)
int recno;
{
	UTPs_step_record *ptr;
/*
.....Verify the record number is valid
*/
	if (recno < 1 || recno > UTP_step_numrec)
	{
		utp_invalid_ptr(recno);
		return(UU_NULL);
	}
/*
.....Return pointer to STEP record
*/
	ptr = (UTPs_step_record *)UU_LIST_ARRAY(&UTP_step_record);
	return(&ptr[recno-1]);
}

/*********************************************************************
**    E_FUNCTION     :  utp_summary()
**          Outputs the STEP file summary to the listing file and
**          status window.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_summary()
{
	int ncomp,i;
	char fnam[62],sbuf[80];
	char *sptr,*utp_get_shape_label();
/*
.....Print out filenames
*/
/*
	ul_short_filename(iges_fname,fnam,60);
	sprintf(sbuf,"Input file: %s\n",fnam);
*/
/*
.....Store component names
*/
	utp_count_entities();
/*
.....Print out components
*/
	ncomp = utp_shape_label_nstack();
	if (ncomp != 0)
	{
		sprintf(sbuf,"\nNumber of Components = %d\n",ncomp);
		uig_list_out(sbuf,UU_TRUE);
		for (i=0;i<ncomp;i++)
		{
			sptr = utp_get_shape_label(i);
			sprintf(sbuf,"Component = %s\n",sptr);
			uig_list_out(sbuf,UU_TRUE);
		}
	}
/*
.....Print out number of surfaces
*/
	sprintf(sbuf,"\nNumber of Trimmed Faces = %d\n",Sntrim);
	uig_list_out(sbuf,UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION     :  utp_get_unistat(unistat)
**          Returns the Unibase Statistics record.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          unistat    Unibase Statistics record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_get_unistat(unistat)
struct UR_unistat_rec *unistat;
{
	*unistat = Sunistat;
}

/*********************************************************************
**    E_FUNCTION     :  utp_reset_unistat()
**          Resets the Unibase Statistics record.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_reset_unistat(unistat)
{
	Sunistat.key = 0;
}


/*********************************************************************
**    I_FUNCTION     :  S_parse_records(fd,step_rec,numrec)
**       Parses a record from a STEP file and stores it in the
**       internal structure.
**    PARAMETERS   
**       INPUT  : 
**          fd        - File to read from.
**          numrec    - Estimated number of records in file.
**       OUTPUT :  
**          step_rec  - Step record read.
**          numrec    - Actual number of records parsed.
**    RETURNS      : UU_SUCCESS if no errors, UU_FAILURE otherwise.
**    SIDE EFFECTS :
**        Defines the Unibase Statistics record using the STEP HEADER
**        section.
**    WARNINGS     : none
*********************************************************************/
static int S_parse_records(fd,step_rec,numrec)
FILE *fd;
UU_LIST *step_rec;
int *numrec;
{
	int i,nrec,totrec,nc,ncc,stat,ipt,irecpt,irec,nparm,tbufsz,strsz;
	UU_LOGICAL first;
	char *str,obuf[80],*tbuf,*pbuf;
	UTP_token_type type;
	UTPs_step_record *sptr;
	UTPs_step_token token;
	UTP_command_type sect,command;
/*
.....Initialize storage
*/
	str = tbuf = UU_NULL;
	uu_list_init(step_rec,sizeof(UTPs_step_record),*numrec,2000);
	step_rec->cur_cnt += *numrec;
	sptr = (UTPs_step_record *)UU_LIST_ARRAY(step_rec);
	strsz = STEP_MAXCHARS;
	str = (char *)uu_malloc(sizeof(char)*strsz);
	if (str == UU_NULL) goto no_mem;
	tbufsz = STEP_MAXCHARS;
	tbuf = (char *)uu_malloc(sizeof(char)*tbufsz);
	if (tbuf == UU_NULL) goto no_mem;
	tbuf[0] = '\0';
/*
.....Initialize routine
*/
	nrec = 0;
	totrec = *numrec;
	sect = UTP_UNKNOWN;
	Sntrim = 0;
/*
.....Initialize the Unibase Statistics record
*/
	stat = ur_retrieve_unibase_stat(&Sunistat);
	sprintf(Sunistat.system,"NCL/STEP V%.3f",NCL_version);
	strcpy(Sunistat.translator,Sunistat.system);
/*
.....Read logical record
*/
	do
	{
		ncc = 0;
		tbuf[0] = '\0';
		do
		{
			stat = ul_fread1(fd,&str,&strsz,&nc);
			if (stat == UX_EOF)
				break;
			if (stat != UU_SUCCESS) 
				goto read_err;
			if (ncc+nc+2 > tbufsz)
			{
				tbuf[ncc+1] = '\0';
				if (ncc+nc > tbufsz*2)
					tbufsz = tbufsz + nc;
				else
					tbufsz *= 2;
				pbuf = (char *)uu_malloc(sizeof(char)*tbufsz);
				if (pbuf == UU_NULL) goto no_mem;
				strcpy(pbuf,tbuf);
				uu_free(tbuf);
				tbuf = pbuf;
			}
			strcat(tbuf,str);
			ncc += nc;
		} while (tbuf[ncc-1] != ';');
/*
.....Break out tokens
*/
		ipt = 0;
		first = UU_TRUE;
		do
		{
			token.str = str;
			S_get_token(tbuf,&ipt,&type,&token);
			if (type == UTP_NONE) break;
			if (type == UTP_END)
			{
				first = UU_TRUE;
				break;
			}
/*
........First token
*/
			if (first)
			{
				command = UTP_UNKNOWN;
				if (type == UTP_COMMAND)
				{
					if (sect == UTP_UNKNOWN)
						sect = token.cmd;
					else if (token.cmd == ENDSEC)
						sect = UTP_UNKNOWN;
					else if (sect == HEADER)
					{
						command = token.cmd;
						nparm = 0;
					}
				}
				else if (sect == DATA)
				{
					if (type != UTP_RECNO)
					{
						S_parse_error("RECNO_EXP",tbuf);
					}
					else
					{
						irec = token.recno;
						irecpt = irec - 1;
						
						if (irec > totrec)
						{
							uu_list_expand(step_rec,irec-totrec);
							sptr = (UTPs_step_record *)UU_LIST_ARRAY(step_rec);
							totrec = irec;
						}
						if (irec > nrec)
						{
							for (i=nrec;i<irec;i++)
							{
								sptr[i].used = 0;
								sptr[i].recno = -1;
								sptr[i].command = UTP_UNKNOWN;
								sptr[i].nparm = 0;
							}
							nrec = irec;
						}
						sptr[irecpt].recno = irec;
						sptr[irecpt].nparm = S_count_tokens(tbuf) - 1;
						sptr[irecpt].command = UTP_UNKNOWN;
						if (sptr[irecpt].nparm > 0)
						{
							sptr[irecpt].parm = (UTPs_step_parm *)
								uu_malloc(sizeof(UTPs_step_parm)*sptr[irecpt].nparm);
							if (sptr[irecpt].parm == UU_NULL) goto no_mem;
							nparm = 0;
						}
						else
							sptr[irecpt].parm = UU_NULL;
					}
				}
				first = UU_FALSE;
			}
/*
.....HEADER section
.....Store Unibase Statistics
*/
			else if (sect == HEADER)
			{
				if (command == FILE_DESCRIPTION)
				{
					if (type == UTP_STRING)
					{
						if (nparm == 0)
						{
							nc = strlen(token.str);
							Sunistat.notes = (char *)uu_malloc(2048*sizeof(char));
							if (Sunistat.notes != UU_NULL)
								strcpy(Sunistat.notes,token.str);
						}
						else if (Sunistat.no_notes+strlen(token.str) < 2047)
						{
							strcat(Sunistat.notes,"\r\n");
							strcat(Sunistat.notes,token.str);
						}
						Sunistat.no_notes = strlen(Sunistat.notes);
						nparm++;
					}
				}
				else if (command == FILE_NAME)
				{
					if (type == UTP_STRING)
					{
						if (nparm == 0)
							strcpy(Sunistat.fname,token.str);
						else if (nparm == 1)
							strcpy(Sunistat.date,token.str);
						else if (nparm == 2)
							strcpy(Sunistat.author,token.str);
						else if (nparm == 3)
							strcpy(Sunistat.company,token.str);
						else if (nparm == 4)
							strcpy(Sunistat.system,token.str);
						else if (nparm == 5)
							strcpy(Sunistat.processor,token.str);
						nparm++;
					}
				}
			}
/*
.....DATA section
.....Parse parameters
*/
			else if (sect == DATA)
			{
				sptr[irecpt].parm[nparm].type = type;
				if (type == UTP_RECNO)
				{
					irec = token.recno;
					if (irec > totrec)
					{
						uu_list_expand(step_rec,irec-totrec);
						sptr = (UTPs_step_record *)UU_LIST_ARRAY(step_rec);
						totrec = irec;
					}
					if (irec > nrec)
					{
						for (i=nrec;i<irec;i++)
						{
							sptr[i].used = 0;
							sptr[i].recno = -1;
							sptr[i].command = UTP_UNKNOWN;
							sptr[i].nparm = 0;
						}
						nrec = irec;
					}
					sptr[irec-1].used++;
					sptr[irecpt].parm[nparm].ptype.recno = irec;
				}
				else if (type == UTP_COMMAND)
				{
					if (nparm == 0 && sptr[irecpt].command == UTP_UNKNOWN)
					{
						sptr[irecpt].command = token.cmd;
						sptr[irecpt].nparm--; nparm--;
						if (token.cmd == ADVANCED_FACE) Sntrim++;
					}
					else
						sptr[irecpt].parm[nparm].ptype.cmd = token.cmd;
				}
				else if (type == UTP_REAL)
					sptr[irecpt].parm[nparm].ptype.value = token.value;
				else if (type == UTP_STRING)
				{
					nc = strlen(token.str);
					sptr[irecpt].parm[nparm].ptype.str =
						(char *)uu_malloc((nc+1)*sizeof(char));
					if (sptr[irecpt].parm[nparm].ptype.str == UU_NULL) goto no_mem;
					strcpy(sptr[irecpt].parm[nparm].ptype.str,token.str);
				}
				nparm++;
			}
			else
				break;
		} while (ipt < ncc);
	} while (stat == UU_SUCCESS);
	stat = UU_SUCCESS;
	goto done;
/*
.....Error reading from file
*/
read_err:;
	uig_error("\nError reading from input STEP file.\n");
	Sbuf[40] = '\0';
	sprintf(obuf,"Record = %s\n",Sbuf);
	uig_error(obuf);
	stat = UU_FAILURE;
	goto done;
/*
.....Could not allocate memory
*/
no_mem:;
	uig_error("Error could not allocate memory.\n");
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	if (tbuf != UU_NULL) uu_free(tbuf);
	if (str != UU_NULL) uu_free(str);
	*numrec = nrec;
	return(stat);
}

/*********************************************************************
**    I_FUNCTION     :  S_get_token(sbuf,ipt,type,token)
**       Parses the next token from the STEP file record.
**    PARAMETERS   
**       INPUT  : 
**          sbuf      - Active STEP line from file.
**          ipt       - Position within 'sbuf' to start parsing from.
**       OUTPUT :  
**          ipt       - Updated position within 'sbuf'.
**          type      - Type of token returned.
**                      UTP_COMMAND, UTP_REAL, UTP_RECNO, UTP_STRING
**          token     - Parsed token.
**                     
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_get_token(sbuf,ipt,type,token)
char *sbuf;
int *ipt;
UTP_token_type *type;
UTPs_step_token *token;
{
	int i,iqot,stat,inc,np,nc,ncc;
	UU_LOGICAL sgn,icmt;
	char tmpbuf[512];
	UU_TRUEDOUBLE uig_atof();
	UTP_token_type itp;
/*
.....Initialize routine
*/
	inc = *ipt;
	iqot = 0;
	*type = UTP_NONE;
	itp = UTP_NONE;
	nc = 0;
	ncc = strlen(sbuf);
	sgn = UU_TRUE;
	icmt = UU_FALSE;
/*
.....Get next token
*/
	for (i=inc;i<ncc;i++)
	{
/*
.....Check for comment
*/
		if (i < ncc-1 && sbuf[i] == '/' && sbuf[i+1] == '*') icmt = UU_TRUE;
/*
.....Check for quotation marks
*/
		if (!icmt)
		{
			if (sbuf[i] == '\'' && iqot != 2)
			{
				if (iqot == 1)
				{
					iqot = 0;
					itp = UTP_STRING;
				}
				else iqot = 1;
			}
			else if (sbuf[i] == '\"' && iqot != 1)
			{
				if (iqot == 2)
				{
					iqot = 0;
					itp = UTP_STRING;
				}
				else iqot = 2;
			}
/*
.....Quoted string
*/
			else if (iqot != 0)
			{
				itp = UTP_STRING;
				if (nc <= 512) tmpbuf[nc++] = sbuf[i];
			}
/*
.....Start of Record number
*/
			else if (sbuf[i] == '#')
				itp = UTP_RECNO;
/*
.....White space
*/
			else if (sbuf[i] == ' ' || sbuf[i] == '\t')
				continue;

/*
.....Delimiter
*/
 			else if (sbuf[i] == '=' || sbuf[i] == '(' || sbuf[i] == ')' ||
				sbuf[i] == ',')
			{
				if (itp == UTP_NONE) continue;
				break;
			}
/*
.....End-of-Command
*/
			else if (sbuf[i] == ';')
			{
				if (itp == UTP_NONE) itp = UTP_END;
				break;
			}
/*
.....All other characters
*/
			else
			{
/*
........Number
*/
				if (isdigit(sbuf[i]) || sbuf[i] == '.' ||
					(sgn && (sbuf[i] == '+' || sbuf[i] == '-')) ||
					(nc != 0 && itp == UTP_REAL && (sbuf[i] == 'E' ||
					sbuf[i] == 'e')))
				{
					if (itp != UTP_RECNO && nc == 0) itp = UTP_REAL;
					tmpbuf[nc++] = sbuf[i];
				}
/*
........Alpha character
*/
				else
				{
					itp = UTP_COMMAND;
					tmpbuf[nc++] = sbuf[i];
				}
			}
			sgn = UU_FALSE;
			if (itp == UTP_REAL && (tmpbuf[nc-1] == 'E' || tmpbuf[nc-1] == 'e'))
				sgn = UU_TRUE;
		}
/*
.....Comment in effect
*/
		else if (sbuf[i] == '/' && sbuf[i-1] == '*')
			icmt = UU_FALSE;
	}
/*
.....Broke out token
.....Convert it to proper type
*/
	if (inc == strlen(sbuf) && itp == UTP_NONE) itp = UTP_END;
	*ipt = i + 1;
	tmpbuf[nc] = '\0';
	strcpy(token->str,tmpbuf);
	*type = itp;
	if (itp == UTP_RECNO)
		token->recno = atoi(tmpbuf);
	else if (itp == UTP_REAL)
		token->value = uig_atof(tmpbuf);
	else if (itp == UTP_COMMAND)
	{
		*type = UTP_STRING;
		for (i=0;i<UTP_MAXCMD;i++)
		{
			if (strcmp(UTP_command[i].str,"END-OF-ARRAY") == 0) break;
			if (strcmp(tmpbuf,UTP_command[i].str) == 0)
			{
				*type = UTP_COMMAND;
				token->cmd = UTP_command[i].type;
				break;
			}
		}
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    I_FUNCTION     :  S_count_tokens(sbuf)
**       Counts the number of tokens in the current STEP record.
**    PARAMETERS   
**       INPUT  : 
**          sbuf      - Active STEP line from file.
**       OUTPUT :  
**          none
**    RETURNS      : Number of tokens in STEP record.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_count_tokens(sbuf)
char *sbuf;
{
	int i,iqot,stat,inc,ntok;
	UU_LOGICAL lsttok;
	long ofs;
/*
.....Initialize routine
*/
	ntok = 0;
	lsttok = UU_FALSE;
/*
.....Search for End-of-Record (;)
.....in current record
*/
	iqot = 0;
	for (i=0;i<strlen(sbuf);i++)
	{
		if (sbuf[i] == '\'' && iqot != 2)
		{
			if (iqot == 1) iqot = 0;
			else iqot = 1;
		}
		else if (sbuf[i] == '\"' && iqot != 1)
		{
			if (iqot == 2) iqot = 0;
			else iqot = 2;
		}
		if (iqot == 0)
		{
			if (sbuf[i] == ';' || sbuf[i] == '=' || sbuf[i] == ',' ||
				sbuf[i] == '(' || sbuf[i] == ')')
			{
				if (!lsttok) ntok++;
				if (sbuf[i] == ';') goto done;
				lsttok = UU_TRUE;
			}
			else if (sbuf[i] != ' ' && sbuf[i] != '\t')
				lsttok = UU_FALSE;
		}
	}
/*
.....End of routine
*/
done:;
	return(ntok);
}

/*********************************************************************
**    I_FUNCTION     :  S_count_tokens(sbuf)
**       Counts the number of tokens in the current STEP record.
**    PARAMETERS   
**       INPUT  : 
**          sbuf      - Active STEP line from file.
**       OUTPUT :  
**          none
**    RETURNS      : Number of tokens in STEP record.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_parse_error(msg,sbuf)
char *msg;
char *sbuf;
{
	int i;
	char tbuf[80];
#define NERR 4
	static char *err[NERR]={
		"INVALID_FILE", "ERROR - Not a recognized STEP file."
		"RECNO_EXP",    "ERROR - Record number (#1) expected."};
/*
.....Find error message label
*/
	for (i=0;i<NERR;i=i+2)
	{
		if (strcmp(msg,err[i]) == 0)
		{
			uig_list_out(err[i+1],UU_TRUE);
			if (sbuf != UU_NULL)
			{
				strncpy(tbuf,sbuf,80);
				if (strlen(sbuf) > 80) tbuf[79] = '\0';
				uig_list_out(tbuf,UU_TRUE);
			}
		}
	}
	return;
}
