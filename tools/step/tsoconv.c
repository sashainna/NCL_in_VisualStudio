/*********************************************************************
**    NAME         :  tsoconv.c
**       CONTAINS:
**			  utp_out_header
**			  utp_out_data
**			  utp_out_write
**			  utp_out_record
**			  utp_out_multiple_record
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tsoconv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:13:21
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "mdrel.h"
#include "nccs.h"
#include "tiges.h"
#include "tioconv.h"
#include "tstep.h"
#include "nclver.h"
#include "xenv1.h"
#include "riddle.h"
#include <time.h>

static int Sfd,Srecno;
extern UM_int2 NCL_ubas_unit;

static void S_date_and_time();

/*********************************************************************
**    E_FUNCTION :  utp_out_header(fd1)
**			Write out the STEP file header information.
**    PARAMETERS   
**       INPUT  : 
**          fd1    = Step file descriptor.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_out_header(fd1)
int fd1;
{
	char	inbuf[2048],dbuf[80],*notes;
	int	count, linenum,stat;
	int	len, i, j, k;
	static struct UR_unistat_rec unistat;
/*
.....Output Header lines
*/
	strcpy(inbuf,"ISO-10303-21;");
	len = strlen(inbuf);
	utp_out_write(fd1,inbuf,len);

	strcpy(inbuf,"HEADER;");
	len = strlen(inbuf);
	utp_out_write(fd1,inbuf,len);
/*
.....Get the Unibase statistics
*/
	stat = ur_retrieve_unibase_stat(&unistat);
/*
.....Output file description
*/
	if (unistat.no_notes == 0)
	{
		if (UTP_step_214)
			strcpy(inbuf,"FILE_DESCRIPTION('STEP AP214');");
		else
			strcpy(inbuf,"FILE_DESCRIPTION('STEP AP203');");
	}
	else
	{
		notes = (char *)uu_malloc(sizeof(char)*(unistat.no_notes+1));
		ur_retrieve_data_varlist(unistat.key,1,notes,1,unistat.no_notes+1);
		strcpy(inbuf,"FILE_DESCRIPTION(");
		len = unistat.no_notes;
		notes[len] = '\0';
		if (notes[len-1] != '\n')
		{
			strcat(notes,"\n");
			len++;
		}
/*
........Store individual lines as separate text strings
*/
		dbuf[0] = '\'';
		k = 1;
		for (i=0 ; i<len; i++)
		{
			if (notes[i] == '\n')
			{
				dbuf[k++] = '\''; dbuf[k++] = '\0';
				strcat(inbuf,dbuf);
				if (i < len-1) strcat(inbuf,",");
				dbuf[0] = '\'';
				k = 1;
			}
			else if (notes[i] != '\r')
				dbuf[k++] = notes[i];
		}
		strcat(inbuf,");");
	}
	len = strlen(inbuf);
	utp_out_write(fd1,inbuf,len);
/*
.....Output file name
*/
	
	sprintf(inbuf,"FILE_NAME('%s'",iges_outfile);
	S_date_and_time(dbuf);
	strcat(inbuf,",'"); strcat(inbuf,dbuf); strcat(inbuf,"'");

	if (unistat.mod_author[0] != '\0') strcpy(dbuf,unistat.mod_author);
	else strcpy(dbuf,unistat.author);
	strcat(inbuf,",'"),strcat(inbuf,dbuf); strcat(inbuf,"'");

	if (unistat.mod_company[0] != '\0') strcpy(dbuf,unistat.mod_company);
	else strcpy(dbuf,unistat.company);
	strcat(inbuf,",'"),strcat(inbuf,dbuf); strcat(inbuf,"'");

	if (unistat.mod_system[0] != '\0' && unistat.mod_system[0] != '	')
		strcpy(dbuf,unistat.mod_system);
	else strcpy(dbuf,unistat.system);
	strcat(inbuf,",'"),strcat(inbuf,dbuf); strcat(inbuf,"'");

	sprintf(dbuf,",'NCL/STEP V%.3f'",NCL_version); strcat(inbuf,dbuf);
	strcat(inbuf,",'authorization'");
	strcat(inbuf,");");
	len = strlen(inbuf);
	utp_out_write(fd1,inbuf,len);
/*
.....Output File schema
*/
	if (UTP_step_214)
		strcpy(inbuf,"FILE_SCHEMA(('AUTOMOTIVE_DESIGN'));");
	else
		strcpy(inbuf,"FILE_SCHEMA(('CONFIG_CONTROL_DESIGN'));");
	len = strlen(inbuf);
	utp_out_write(fd1,inbuf,len);
/*
.....End of section
*/
	utp_out_write(fd1,"ENDSEC;",7);
	return;
}

/*********************************************************************
**    E_FUNCTION :  utp_out_data(fd1)
**			Write out the STEP file DATA section information.
**    PARAMETERS   
**       INPUT  : 
**          fd1    = Step file descriptor.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_out_data(fd1)
int fd1;
{
	int	entnum,len,status;
	char	inbuf[2048];
	struct NCL_fixed_databag e;
/*
.....Initialize routine
*/
	Sfd = fd1;
	Srecno = 0;
/*
.....Output Section DATA lines
*/
	strcpy(inbuf,"DATA;");
	len = strlen(inbuf);
	utp_out_write(fd1,inbuf,len);
/*
.....Output standard attribute records
*/
	utp_out_identity_attr();
/*
.....Initialize attribute stack
*/
	utp_init_attr_stack();
/*
.....Output all composite solids
*/
	if (entity_out[0] == 1)
	{
		entnum = 0;
		do
		{
			entnum++;
			e.rel_num = UM_SOLID_REL;
			status = ur_get_next_data_key(e.rel_num,&entnum,&e.key);
			if (status != UU_SUCCESS) break;
			utp_out_solid(&e);
		} while (status == UU_SUCCESS);
	}
/*
.....Output trimmed surfaces
*/
	if (entity_out[1] == 1)
		utp_out_surfaces(NCL_TRIMSF_REL);
/*
.....Output net surfaces
*/
	if (entity_out[5] == 1)
		utp_out_surfaces(NCL_NETSF_REL);
/*
.....Output untrimmed surfaces
*/
	if (entity_out[6] == 1)
	{
		utp_out_surfaces(UM_RBSPLSRF_REL);
		utp_out_surfaces(NCL_REVSURF_REL);
	}
/*
.....Output planes
*/
	if (entity_out[3] == 1)
		utp_out_planes();
/*
.....Output wireframe and points
*/
	if (entity_out[2] == 1 || entity_out[4] == 1)
		utp_out_wireframe(entity_out[2],entity_out[4]);
/*
.....Output End-of-Section
*/
	strcpy(inbuf,"ENDSEC;");
	len = strlen(inbuf);
	utp_out_write(fd1,inbuf,len);

	strcpy(inbuf,"END-ISO-10303-21;");
	len = strlen(inbuf);
	utp_out_write(fd1,inbuf,len);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_record(sbuf)
**			Write out a STEP file DATA section record.
**    PARAMETERS   
**       INPUT  : 
**          sbuf   = Text string to output.
**       OUTPUT :  
**          output
**    RETURNS      :
**       Record number assigned to the output record.
**    SIDE EFFECTS :
**       This routine maintains the record numbers output to the STEP
**       file and adds the current record number to the input line
**       prior to outputting it.
**    WARNINGS     : none
*********************************************************************/
int utp_out_record(sbuf)
char *sbuf;
{
	int len;
	char tbuf[81920];
	sprintf(tbuf,"#%d = %s",++Srecno,sbuf);
	len = strlen(tbuf);
	utp_out_write(Sfd,tbuf,len);
	return(Srecno);
}

/*********************************************************************
**    E_FUNCTION :  utp_out_multiple_record(sbuf,sflag,recno)
**			Write out a STEP file DATA section record that may have to be
**       output in multiple physical records.
**    PARAMETERS   
**       INPUT  : 
**          sbuf   = Text string to output.
**          flag   = 0 = 1st part of record, add record number assignment.
**                   1 = Continuation record.
**                   2 = Last record, force buffer output.
**       OUTPUT :
**          flag   = Updated to 1 after outputting first record.
**          recno  = Record number of output record.  Remains unchanged
**                   after outputting the first record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_out_multiple_record(sbuf,flag,recno)
char *sbuf;
int *flag,*recno;
{
	int nc1,nc2;
	char tbuf[80],*p,*strrchr();
	static char Sobuf[512];
/*
.....Format first part of record
*/
	if (*flag == 0)
	{
		sprintf(Sobuf,"#%d = %s",++Srecno,sbuf);
		*flag = 1;
		*recno = Srecno;
	}
/*
.....Continuation records
*/
	else
	{
		nc1 = strlen(Sobuf);
		nc2 = strlen(sbuf);
		if (nc1+nc2 >= sizeof(Sobuf))
		{
			tbuf[0] = '\0';
			p = strrchr(Sobuf,',');
			if (p != UU_NULL)
			{
				strcpy(tbuf,++p);
				*p = '\0';
				nc1 = strlen(Sobuf);
			}
			utp_out_write(Sfd,Sobuf,nc1);
			strcpy(Sobuf,tbuf);
			nc1 = strlen(Sobuf);;
		}
		strcat(Sobuf,sbuf);
		if (*flag == 2)
		{
			utp_out_write(Sfd,Sobuf,nc1+nc2);
			*flag = 0;
		}
	}
}

/*********************************************************************
**	I_FUNCTION:	S_date_and_time(str)
**	This function formats the current date and time into a string.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : str  = Contains current date and time in the format
**                       'yyyy-mm-ddThh:mm:ss'.
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
static void S_date_and_time(str)
char *str;
{
	int year;
	struct tm *time_st;
	time_t timval;
/*
.....Get the current date and time
*/
	time(&timval);
	time_st = localtime(&timval);
	year = time_st->tm_year;
	if (year < 70) year = year + 2000;
	else if (year > 99) year = year + 1900;
	sprintf(str,"%04d-%02d-%02dT%02d:%02d:%02d",year,time_st->tm_mday,
		time_st->tm_mon+1,time_st->tm_hour,time_st->tm_min,time_st->tm_sec);
	return;
}
