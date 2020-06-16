/*********************************************************************
**    NAME         :  tspdebug.c
**       CONTAINS:
**             utp_debug_step_file
**             utp_debug_record
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tspdebug.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:22
*********************************************************************/
#include "usysdef.h"
#include "tstep.h"

static UU_LIST Signore_list;
static UU_LOGICAL Signore_init=UU_FALSE;
static UTP_command_type Stop_command;

void utp_debug_tree();
void utp_debug_record();

static void S_get_command();
static void S_format_parm();

/*********************************************************************
**    E_FUNCTION     :  utp_debug_step_file(sort)
**       Prints out the internal STEP file to the listing file.
**    PARAMETERS   
**       INPUT  :
**          sort     = UU_TRUE - Sort output by Command Trees.
**                     UU_FALSE- Output the file in order.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_debug_step_file(sort)
UU_LOGICAL sort;
{
	int i,nlev;
	UTPs_step_record *ptr;
/*
.....Loop through the STEP DATA records
*/
	if (uig_is_listing_on())
	{
		ptr = (UTPs_step_record *)UU_LIST_ARRAY(&UTP_step_record);
		nlev = 0;
		for (i=0;i<UTP_step_numrec;i++)
		{
			Stop_command = ptr[i].command;
			if (sort && ptr[i].used == 0)
				utp_debug_tree(&ptr[i],nlev);
			else if (!sort)
				utp_debug_record(&ptr[i],UU_FALSE,-1);
		}
		if (Signore_init)
		{
			uu_list_free(&Signore_list);
			Signore_init = UU_FALSE;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_debug_tree(ptr,nlev)
**       Re-entrant code to print out STEP records in a logical order
**       based on their tree structure.
**    PARAMETERS   
**       INPUT  :
**          ptr      Pointer to the STEP record to format and output.
**          nlev     Level of tree that we are currently processing.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_debug_tree(ptr,nlev)
UTPs_step_record *ptr;
int nlev;
{
	int i,inc,*p;
	UTPs_step_record *sptr;
/*
.....Record is not used
*/
	if (ptr->recno == -1) return;
/*
.....STYLED_ITEM & OVER_RIDING_STYLED_ITEM
.....can be duplicated many times in the tree
.....only output the first occurrence
*/
	if (ptr->command == STYLED_ITEM || 
		ptr->command == OVER_RIDING_STYLED_ITEM)
	{
		if (!Signore_init)
		{
			uu_list_init(&Signore_list,sizeof(int),200,200);
			Signore_init = UU_TRUE;
		}
		else
		{
			p = (int *)UU_LIST_ARRAY(&Signore_list);
			for (i=0;i<UU_LIST_LENGTH(&Signore_list);i++)
				if (ptr->recno == p[i]) return;
		}
		uu_list_push(&Signore_list,&ptr->recno);
	}
/*
.....Print out active record
*/
	utp_debug_record(ptr,UU_FALSE,nlev);
/*
.....Loop through the STEP DATA records
*/
	inc = nlev + 1;
	for (i=0;i<ptr->nparm;i++)
	{
		if (ptr->parm[i].type == UTP_RECNO)
		{
			if (Stop_command == PRESENTATION_LAYER_ASSIGNMENT &&
				(ptr->command == CLOSED_SHELL || ptr->command == OPEN_SHELL))
					continue;
			if (ptr->command == STYLED_ITEM && i == ptr->nparm-1) continue;
			if (ptr->command == OVER_RIDING_STYLED_ITEM && i == ptr->nparm-2)
				continue;
			if (ptr->command == SHAPE_REPRESENTATION && i == 1) continue;
			if (ptr->command == REPRESENTATION_RELATIONSHIP && i == 2)
				continue;
			sptr = utp_get_record(ptr->parm[i].ptype.recno);
			if (sptr != UU_NULL) utp_debug_tree(sptr,inc);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     :  utp_debug_record(ptr,flag,indent)
**       Formats
**    PARAMETERS   
**       INPUT  :
**          ptr      Pointer to the STEP record to format and output.
**          flag     UU_TRUE = Format record for error output.
**                   UU_FALSE = Format record for debug output.
**          indent   Number of spaces to indent output.  If set to
**                   -1 then the indentation will be determined on
**                   error/debug output and the number of times the
**                   record is used.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_debug_record(ptr,flag,indent)
UTPs_step_record *ptr;
UU_LOGICAL flag;
int indent;
{
	int i,j,inc;
	char sbuf[512],tbuf[512],t1buf[512];
/*
.....Initialize routine
*/
	if (ptr->recno == -1) return;
	if (flag)
		inc = 0;
	else
	{
		inc = indent;
		if (inc == -1)
			inc = ptr->used;
		else if (inc == 0)
			uig_list_out(" \n",flag);
	}
/*
.....Format output string
*/
	strcpy(t1buf,"                    ");
	t1buf[inc] = '\0';
	S_get_command(tbuf,ptr->command);
	sprintf(sbuf,"%s#%d = %s",t1buf,ptr->recno,tbuf);
/*
........Loop through variables
*/
	for (j=0;j<ptr->nparm;j++)
	{
		S_format_parm(tbuf,&ptr->parm[j]);
		strcat(sbuf,",");
		if (strlen(tbuf)+strlen(sbuf) > 80)
		{
			strcat(sbuf,"\n");
			uig_list_out(sbuf,flag);
			strcpy(sbuf,"                    ");
			sbuf[inc+3] = '\0';
		}
		strcat(sbuf,tbuf);
	}
	strcat(sbuf,";\n");
	uig_list_out(sbuf,flag);
}

/*********************************************************************
**    I_FUNCTION     :  S_get_command(sbuf,command)
**       Formats the input command as a text string.
**    PARAMETERS   
**       INPUT  : 
**          command   - STEP command to format.
**       OUTPUT :  
**          sbuf      - Formatted text string.
**                     
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_get_command(sbuf,command)
char *sbuf;
UTP_command_type command;
{
	int ist,ien,inc;
	UU_LOGICAL found;
/*
.....Commands are in proper order
*/
	if (command == UTP_command[(int)command].type)
	{
		strcpy(sbuf,UTP_command[(int)command].str);
	}
/*
.....Loop through array to find 
.....proper command
.....Start 10 back from 'command' pointer
*/
	else
	{
		ist = command - 10; if (ist < 0) ist = 0;
		ien = ist - 1; if (ien < 0) ien = 0;
		inc = ist;
		found = UU_FALSE;
		do
		{
			if (command == UTP_command[inc].type)
			{
				strcpy(sbuf,UTP_command[inc].str);
				found = UU_TRUE;
				break;
			}
			inc++;
			if (inc >= UTP_MAXCMD) inc = 0;
		} while (inc != ien);
/*
.....Could not find command
*/
		if (!found)
			strcpy(sbuf,"(unknown)");
	}
}
		

/*********************************************************************
**    I_FUNCTION     :  S_format_parm(sbuf,parm)
**       Formats the input STEP parameter as a text string.
**    PARAMETERS   
**       INPUT  : 
**          parm      - Step parameter to format.
**       OUTPUT :  
**          sbuf      - Formatted text string.
**                     
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_format_parm(sbuf,parm)
UTPs_step_parm *parm;
char *sbuf;
{
	int i,iqot,stat,inc,np,nc;
	char tmpbuf[512];
	UU_TRUEDOUBLE uig_atof();
	UTP_token_type itp;
/*
.....Record number
*/
	switch (parm->type)
	{
	case UTP_RECNO:
		sprintf(sbuf,"#%d",parm->ptype.recno);
		break;
/*
.....Real value
*/
	case UTP_REAL:
		sprintf(sbuf,"%lf",parm->ptype.value);
		break;
/*
.....Text string
*/
	case UTP_STRING:
		strcpy(sbuf,parm->ptype.str);
		break;
/*
.....Command
*/
	case UTP_COMMAND:
		S_get_command(sbuf,parm->ptype.cmd);
		break;
/*
.....Unknown parameter type
*/
	default:
		strcpy(sbuf,"(unknown)");
		break;
	}
}
