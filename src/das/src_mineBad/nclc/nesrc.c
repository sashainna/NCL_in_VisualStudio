/*********************************************************************
**    NAME         :  nesrc.c
**       CONTAINS:
**             nclf_set_loadpp()
**             nclf_reset_loadpp()
**					nclf_getsrc()
**             ncl_getsrc_rec()
**					nclf_putsrc()
**					nclf_delsrc()
**					nclf_srceof()
**					nclf_close_src()
**					nclf_save_src()
**					nclf_load_src()
**					nclf_getw2()
**					nclf_putw2()
**					nclf_storew2()
**					nclf_putmerge()
**					nclf_merge_source()
**					nclf_src_line_to_rec()
**					nclf_src_rec_to_line()
**					nclf_src_next_rec()
**             ncl_close_tmpsrc()
**             ncl_open_tmpsrc()
**             ncl_set_auto_save_params()
**             ncl_get_auto_save_params()
**             ncl_autosave_pop()
**             ncl_cond_auto_save()
**             ncl_auto_save_ppg()
**             ncl_snap_save()
**             ncl_recover_ppg()
**             ncl_close_autosave()
**    COPYRIGHT 2009 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nesrc.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       02/28/17 , 15:45:41
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include	"lcom.h"
#include	"mfort.h"
#include	"nclfc.h"
#include "nesrc.h"
#include "nclcmd.h"
#include "ulist.h"
#include "uhep.h"

#define MAXW2 50
#define DEBUG 0
#define MAXAUTO 100

static UU_LIST Ssource,Smerge;
static NCL_srcptr_struc Srec={-1,-1,-1},Sptr={-1,-1,-1};

static UU_LOGICAL Sinit = UU_FALSE, Sinitm = UU_FALSE;
static int Scurrent_rec = -1;

static int Snw2 = 0;
static int Sw2_nc[MAXW2];
static int Sw2_size[MAXW2];
static int Sw2_type[MAXW2];
static char *Sw2_buf[MAXW2];

static FILE *Sfd=UU_NULL;
static UX_pathname Stemp_file;

static void S_find_match();
static void S_match_range();
/*
.....Added part program auto save - ASF 1/10/14
*/
UU_LOGICAL NCL_ason; /* autosave on flag */
long NCL_last_autosave = 0; /* last autosave time */
int NCL_switch = 0;         /* default is time */
int NCL_time = 300;         /* default is 5 min. (300 sec) */
int NCL_num_chg = 50;       /* default is 50 changes? */
int NCL_chg_cnt = 0;        /* PP file change counter - initially 0 */
int NCL_max_saves = 0, NCL_saves = 0;
static UU_LOGICAL Sauto_init = UU_FALSE, NCL_loadpp = UU_FALSE;
static int Ssaves[MAXAUTO], Sauto_ind=0;
extern UX_pathname NCL_load_part;
void ncl_cond_auto_save();
/*********************************************************************
**   E_FUNCTION :  nclf_reset_loadpp()
**      Set NCL_loadpp flag to UU_TRUE so the auto save feature is
**      not used when loading a part pragram file.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_set_loadpp()
{
	NCL_loadpp = UU_TRUE;
}
/*********************************************************************
**   E_FUNCTION :  nclf_reset_loadpp()
**       Reset NCL_loadpp flag
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_reset_loadpp()
{
	NCL_loadpp = UU_FALSE;
}
/*********************************************************************
**    E_FUNCTION :  nclf_getsrc(nline,sbuf,knc,ktype,kcmt)
**       Gets the requested source line.
**    PARAMETERS   
**       INPUT  : 
**          nline    Source line number to retrieve.  Based on 0.
**          kcmt     0 = Don't return comment chars, 1 = Return comment
**                   chars
**       OUTPUT :  
**          sbuf     Source line.
**          knc      Number of characters in 'sbuf'.
**          ktype    Type of record retrieved.
**                         0 = Normal
**                         1 = INCLUDE
**                         2 = INCERR*
**                         3 = INCFILE
**                         4 = READIT*
**                         5 = READINC
**                        10 = Current Line    (N)
**                        20 = Executing loop  (X)
**                        30 = Executing Macro (C)
**    RETURNS      : -1 if at end of file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UM_int4 nclf_getsrc(nline,sbuf,knc,ktype,kcmt)
UM_int4 *nline;
char *sbuf;
UM_int4 *knc;
UM_int2 *ktype,*kcmt;
{
	int status,ilin,inc,i;
	NCL_source_struc *psrc;
/*
.....Initialize routine
*/
	status = 0;
	ilin = *nline;
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
/*
.....Find position within source array
.....of line
*/
	inc = S_find_line(ilin);
	if (inc == -1) goto failed;
/*
.....Return line
*/
	strncpy(sbuf,psrc[inc].buf,psrc[inc].nc);
	if (*kcmt == 0 && psrc[inc].nc >= UL_comment_column &&
		UL_comment_column != 0)
	{
		for (i=UL_comment_column-2;i>=0;i--)
		{
			if (sbuf[i] != ' ') break;
		}
		*knc = i + 1;
	}
	else
		*knc = psrc[inc].nc;
	*ktype = psrc[inc].type;
/*
......remove trailing spaces and new line characters
*/
	for (i=*knc; i>0; i--)
	{
		if ((sbuf[i-1]==' ')||(sbuf[i-1]=='\t')||(sbuf[i-1]=='\r')||(sbuf[i-1]=='\n'))
			sbuf[i-1] = '\0';
		else
			break;
	}		
	*knc = i;
/*
.....Set the current pointers
*/
	Sptr.current = inc;
	Srec.current = ilin;
	goto done;
/*
.....No such record
*/
failed:
	status = -1;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ncl_getsrc_rec(nrec,nline,sbuf,knc)
**       Gets the requested source line using the storage record number
**       as reference.
**    PARAMETERS   
**       INPUT  : 
**          nrec     Source record number to retrieve.
**       OUTPUT :  
**          nline    Source line number, based on 1.
**          sbuf     Source line.
**          knc      Number of characters in 'sbuf'.
**    RETURNS      : UU_FAILURE if record does not exist.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_getsrc_rec(nrec,nline,sbuf,knc)
int nrec,*nline;
char *sbuf;
int *knc;
{
	int status,i;
	NCL_source_struc *psrc;
/*
.....Make sure requested record exists
*/
	if (nrec >= UU_LIST_LENGTH(&Ssource)) goto failed;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
/*
.....Return line
*/
	*nline = psrc[nrec].lineno;
	strncpy(sbuf,psrc[nrec].buf,psrc[nrec].nc);
	if (psrc[nrec].nc >= UL_comment_column && UL_comment_column != 0)
	{
		for (i=UL_comment_column-2;i>=0;i--)
		{
			if (sbuf[i] != ' ') break;
		}
		*knc = i + 1;
	}
	else
		*knc = psrc[nrec].nc;
	sbuf[*knc] = '\0';
	goto done;
/*
.....No such record
*/
failed:
	*knc = 0;
	sbuf[*knc] = '\0';
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	return(status);
}

void nclc_putsrc(nline,sbuf,knc,ktype,kins)
int nline, knc;
short ktype, kins;
char *sbuf;
{
	nclf_putsrc(&nline, sbuf, &knc, &ktype, &kins);
}

/*********************************************************************
**    E_FUNCTION :  nclf_putsrc(nline,sbuf,knc,ktype,kins)
**       Stores the provided source line.
**    PARAMETERS   
**       INPUT  : 
**          nline    Source line number to store buffer at.  Based on 0.
**          sbuf     Source line to store.
**          knc      Number of characters in 'sbuf'.
**          ktype    Type of record to store.
**                         0 = Normal
**                         1 = INCLUDE
**                         2 = INCERR*
**                         3 = INCFILE
**                         4 = READIT*
**                         5 = READINC
**                        10 = Current Line    (N)
**                        20 = Executing loop  (X)
**                        30 = Executing Macro (C)
**          kins     0 = Overwrite current line, 1 = Insert line at this
**                   location.
**       OUTPUT :  none
**    RETURNS      : -1 if could not store line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclf_putsrc(nline,sbuf,knc,ktype,kins)
UM_int4 *nline;
char *sbuf;
UM_int4 *knc;
UM_int2 *ktype,*kins;
{
	int status,ilin,nsrc,n,i;
	NCL_source_struc *psrc,src;
	UU_LOGICAL changed = UU_FALSE;
/*
.....Initialize routine
*/
	status = 0;
	ilin = *nline;
/*
.....Initialize source storage
*/
	if (!Sinit)
	{
		status = S_init_source(UU_TRUE);
		if (status == UU_FAILURE) goto failed;
	}
/*
.....Get number of source lines in storage
*/
	nsrc = UU_LIST_LENGTH(&Ssource);
/*
.....Place source commands at end of file
*/
	if (ilin > Srec.last)
	{
		n = ilin - Srec.last;
		if (n >= 0) uu_list_expand(&Ssource,n);
		psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
		src.nc = 0;
		src.size = 0;
		src.buf = UU_NULL;
		src.type = 0;
		for (i=Srec.last+1;i<=ilin;i++)
		{
			src.prev = Sptr.last;
			src.next = i + 1;
			src.lineno = i + 1;
			if (i == ilin)
			{
				src.next = -1;
				src.nc = *knc;
				src.size = *knc;
				src.type = *ktype;
				if (src.nc != 0)
				{
					src.buf = (char *)uu_malloc(sizeof(char)*(src.nc+1));
					if (src.buf == UU_NULL) goto failed;
					strncpy(src.buf,sbuf,src.nc);
					src.buf[src.nc] = '\0';
				}
			}
			psrc[nsrc] = src;
			if (Sptr.last != -1) psrc[Sptr.last].next = nsrc;
/*
.....Store current pointers
*/
			Sptr.last = nsrc;
			Srec.last = ilin;
			Sptr.current = nsrc;
			Srec.current = ilin;
/*
.....Write record to temp file
*/
			S_write_source(psrc,nsrc);
			nsrc++; changed = UU_TRUE;
		}
	}
/*
.....Overwrite mode
*/
	else if (*kins == 0)
	{
		status = S_store_line(ilin,sbuf,*knc,*ktype,&changed);
		if (status != UU_SUCCESS) goto failed;
	}
/*
.....Insert mode
*/
	else
	{
		status = S_insert_line(ilin,sbuf,*knc,*ktype);
		if (status != UU_SUCCESS) goto failed;
		changed = UU_TRUE;
	}
/*
.....Added part program auto save - ASF 1/10/14
*/
	if (changed && !NCL_loadpp)
	{
		NCL_chg_cnt++;
		ncl_cond_auto_save();
	}
	goto done;
/*
.....Failure storing source line
*/
failed:
	status = -1;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  nclf_delsrc(first,last)
**       Deletes the requested source lines.
**    PARAMETERS   
**       INPUT  : 
**          first    Start of source lines to delete.
**          last     End of source lines to delete.
**       OUTPUT :  none
**    RETURNS      : -1 if could not delete lines.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclf_delsrc(first,last)
UM_int4 *first,*last;
{
	int inc1,inc2,ilin,elin,nlin,status;
	NCL_source_struc *psrc;
/*
.....Initialize routine
*/
	status = 0;
	ilin = *first;
	elin = *last;
	if (ilin > elin) goto done;
	nlin = *last - *first + 1;
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
/*
.....Find position within source array
.....of line range to delete
*/
	inc1 = S_find_line(ilin);
	inc2 = S_find_line(elin);
	if (inc1 == -1 || inc2 == -1) goto failed;
/*
.....Adjust pointers to remove line range
*/
	if (psrc[inc1].prev == -1 && psrc[inc2].next == -1)
	{
		Sptr.first = Sptr.current = Sptr.last = -1;
		Srec.first = Srec.current = Srec.last = -1;
		UU_LIST_EMPTY(&Ssource);
		goto done;
	}
	else if (psrc[inc1].prev == -1)
	{
		Sptr.first = psrc[inc2].next;
		psrc[psrc[inc2].next].prev = -1;
		S_write_source(psrc,psrc[inc2].next);
	}
	else if (psrc[inc2].next == -1)
	{
		Sptr.last = psrc[inc1].prev;
		psrc[psrc[inc1].prev].next = -1;
		S_write_source(psrc,psrc[inc1].prev);
	}
	else
	{
		psrc[psrc[inc1].prev].next = psrc[inc2].next;
		psrc[psrc[inc2].next].prev = psrc[inc1].prev;
		S_write_source(psrc,psrc[inc1].prev);
		S_write_source(psrc,psrc[inc2].next);
	}
/*
.....Adjust line numbers
*/
	inc1 = psrc[inc2].next;
	while (inc1 != -1)
	{
		psrc[inc1].lineno -= nlin;
		inc1 = psrc[inc1].next;
	}
/*
.....Set the current pointers
*/
	Sptr.current = psrc[inc2].next;
	Srec.current = ilin;
	Srec.last -= nlin;
	goto done;
/*
.....Could not delete lines
*/
failed:
	status = -1;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  nclf_srceof(nline)
**       Marks the specified line as the last line in the file.
**    PARAMETERS   
**       INPUT  : 
**          nline    Last source line number in file.
**       OUTPUT :  none
**    RETURNS      : -1 if could not store line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_srceof(nline)
UM_int4 *nline;
{
	int inc,ilin;
	NCL_source_struc *psrc;
/*
.....Initialize routine
*/
	ilin = *nline;
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
/*
.....Find position within source array
*/
	inc = S_find_line(ilin);
/*
.....Mark line as last line
*/
	if (inc != -1)
	{
		psrc[inc].next = -1;
		Sptr.last = inc;
		Srec.last = ilin;
	}
}

/*********************************************************************
**    E_FUNCTION :  nclf_close_src()
**       Frees the source file memory and closes the temorary external
**       file.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : -1 if could not store line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_close_src()
{
	int i,nsrc;
	FILE *tmp;
	UX_pathname *fname;
	NCL_source_struc *psrc;
/*
.....Free the internal source memory
*/
	if (Sinit)
	{
		psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
		for (i=0;i<UU_LIST_LENGTH(&Ssource);i++)
		{
			if (psrc[i].buf != UU_NULL) uu_free(psrc[i].buf);
		}
		uu_list_free(&Ssource);
		Sinit = UU_FALSE;
	}
/*
.....Close and delete the external file
*/
	if (Sfd != UU_NULL)
	{
		ux_fclose0(Sfd);
		ux_delete(Stemp_file,UX_PRTERRS);
		Sfd = UU_NULL;
	}
/*
.....Reset the source pointers
*/
	Sptr.first = 0;
	Sptr.current = Sptr.last = -1;
	Srec.first = 0;
	Srec.current = Srec.last = -1;
}

/*********************************************************************
**    E_FUNCTION :  nclf_save_src(fnam,knc,ioerr)
**       Saves a temporary work file to a permanent disk file.
**    PARAMETERS   
**       INPUT  : 
**          fnam     Name of file to save.
**          knc      Number of chars in 'fnam'.
**       OUTPUT :  
**          ioerr    Returns -1 if file could not be written.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_save_src(fnam,knc,ioerr)
char *fnam;
UM_int4 *knc,*ioerr;
{
	int i,ip,np,stat,lstrec,nc,inc;
	char sbuf[NCL_MAX_COMLINE+20];
	UX_pathname sfil;
	NCL_source_struc *psrc,src;
	FILE *fd;
/*
.....Initialize routine
*/
	*ioerr = 0;
	strncpy(sfil,fnam,*knc);
	sfil[*knc] = '\0';
	fd = Sfd;
/*
.....Open the work file
*/
	stat = ux_fopen0(sfil,"w",&Sfd);
	if (stat != UU_SUCCESS) goto failed;
/*
.....Store the work file
*/
	inc = Sptr.first;
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
	while (inc != -1)
	{
		stat = S_write_source(psrc,inc);
		if (stat != UU_SUCCESS) goto failed;
		inc = psrc[inc].next;
	}
/*
.....End of routine
*/
done:;
	ux_fclose0(Sfd);
	Sfd = fd;
	return;
/*
.....Error writing file
*/
failed:;
	*ioerr = -1;
	goto done;
}

/*********************************************************************
**    E_FUNCTION :  nclf_load_src(fnam,knc,ifl,nlines)
**       Loads a temporary work file into the part program source
**       list.
**    PARAMETERS   
**       INPUT  : 
**          fnam     Name of file to load.
**          knc      Number of chars in 'fnam'.
**          ifl      0 = Don't open new temporary work file,
**                   1 = Open new temp file (called from load Session).
**       OUTPUT :  
**          nlines   Returns then number of lines read in.  Returns
**                   -1 if file could not be read.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_load_src(fnam,knc,ifl,nlines)
char *fnam;
UM_int4 *knc,*nlines,*ifl;
{
	int i,ip,np,stat,lstrec,nc,inc;
	UU_LOGICAL wrfl;
	char sbuf[NCL_MAX_COMLINE+20];
	UX_pathname sfil;
	NCL_source_struc *psrc,src;
	FILE *fd;
/*
.....Initialize routine
*/
	*nlines = -1;
	strncpy(sfil,fnam,*knc);
	sfil[*knc] = '\0';
	wrfl = *ifl == 1 ? UU_TRUE : UU_FALSE;
/*
.....Initialize the source file storage
*/
	nclf_close_src();
	S_init_source(wrfl);
	lstrec = -1;
/*
.....Open the work file
*/
	stat = ux_fopen0(sfil,"r",&fd);
	if (stat != UU_SUCCESS) goto done;
/*
.....Read a record
*/
	do
	{
		stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) break;
/*
.....Store the record if requested
*/
		if (wrfl)
		{
			stat = ux_fwrite0(sbuf,nc,1,Sfd,&nc);
			stat = ux_fwrite0("\n",1,1,Sfd,&nc);
		}
		if (stat != UU_SUCCESS) break;
/*
.....Parse the record
*/
		sscanf(sbuf,"%d %d %d %d %d",&inc,&src.nc,&src.type,&src.prev,
			&src.next);
		src.size = src.nc;
		src.type = um_mod(src.type,10);
		src.lineno = 0;
		if (src.nc == 0) src.buf = UU_NULL;
		else
		{
			np = 0;
			for (i=0;i<strlen(sbuf);i++)
			{
				if (sbuf[i] == ' ')
				{
					ip = i+1;
					np++;
					if (np == 5) break;
				}
			}
			src.buf = (char *)uu_malloc(sizeof(char)*(src.nc+1));
			strncpy(src.buf,&sbuf[ip],src.nc);
			src.buf[src.nc] = '\0';
		}
/*
.....Store the record
*/
		np = UU_LIST_LENGTH(&Ssource);
		if (inc >= np) uu_list_expand(&Ssource,inc-np+1);
		psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
		psrc[inc] = src;
		if (src.prev != -1 && src.prev < np) psrc[src.prev].next = inc;
		if (src.next != -1 && src.next < np) psrc[src.next].prev = inc;
/*
.....Store the pointers
*/
		if (src.prev == -1)
		{
			Sptr.first = inc;
			Sptr.current = inc;
		}
		if (src.next == -1)
		{
			if (lstrec != -1) psrc[lstrec].next = inc;
			lstrec = inc;
			Sptr.last = inc;
		}
	} while (stat == UU_SUCCESS);
/*
.....Assign the NCL line numbers
*/
	np = UU_LIST_LENGTH(&Ssource);
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
	inc = Sptr.first;
	for (i=0;i<np;i++)
	{
		psrc[inc].lineno = i + 1;
		inc = psrc[inc].next;
		if (inc == -1) break;
	}
	Srec.last = i;
	*nlines = Srec.last + 1;
	if (!wrfl) ul_build_full_fname("",sfil,"",Stemp_file);
/*
.....End of routine
*/
done:;
	if (!wrfl) Sfd = fd;
	return;
}

/*********************************************************************
**    E_FUNCTION :  nclf_getw2(nline,sbuf,knc,ktype)
**       Gets the requested line from the 'w2' (active command) array.
**    PARAMETERS   
**       INPUT  : 
**          nline    Source line number to retrieve.  Based on 1.
**       OUTPUT :  
**          sbuf     Source line.
**          knc      Number of characters in 'sbuf'.
**          ktype    Type of record retrieved.
**                         0 = Normal
**                         1 = INCLUDE
**                         2 = INCERR*
**                         3 = INCFILE
**                         4 = READIT*
**                         5 = READINC
**                        10 = Current Line    (N)
**                        20 = Executing loop  (X)
**                        30 = Executing Macro (C)
**    RETURNS      : -1 if at end of file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_getw2(nline,sbuf,knc,ktype)
UM_int2 *nline;
char *sbuf;
UM_int4 *knc;
UM_int2 *ktype;
{
	int ilin;
/*
.....Initialize routine
*/
	ilin = *nline - 1;
	if (ilin < 0 || ilin >= Snw2)
	{
		sbuf[0] = ' ';
		*knc = 0;
		*ktype = 0;
	}
/*
.....Return requested text buffer
*/
	else
	{
		strncpy(sbuf,Sw2_buf[ilin],Sw2_nc[ilin]);
		*knc = Sw2_nc[ilin];
		*ktype = Sw2_type[ilin];
	}
/*
.....End of routine
*/
done:
	return;
}

/*********************************************************************
**    E_FUNCTION :  nclf_putw2(nline,sbuf,knc,ktype)
**       Stores the requested line in the 'w2' (active command) array.
**    PARAMETERS   
**       INPUT  : 
**          nline    Source line number to retrieve.  Based on 1.
**          sbuf     Source line.
**          knc      Number of characters in 'sbuf'.
**          ktype    Type of record retrieved.
**                         0 = Normal
**                         1 = INCLUDE
**                         2 = INCERR*
**                         3 = INCFILE
**                         4 = READIT*
**                         5 = READINC
**                        10 = Current Line    (N)
**                        20 = Executing loop  (X)
**                        30 = Executing Macro (C)
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_putw2(nline,sbuf,knc,ktype)
UM_int2 *nline;
char *sbuf;
UM_int4 *knc;
UM_int2 *ktype;
{
	int ilin,nc;
/*
.....Initialize routine
*/
	ilin = *nline - 1;
	nc = *knc;
	if (ilin < 0 || ilin >= MAXW2) goto done;
/*
.....See if we need to allocate memory
*/
	if (ilin >= Snw2 || *knc > Sw2_size[ilin])
	{
		if (ilin < Snw2) uu_free(Sw2_buf[ilin]);
		Sw2_buf[ilin] = (char *)uu_malloc(sizeof(char)*(nc+1));
		Sw2_size[ilin] = nc;
		if (Sw2_buf[ilin] == UU_NULL) goto done;
	}
/*
.....Store the text buffer
*/
	strncpy(Sw2_buf[ilin],sbuf,nc);
	Sw2_buf[ilin][nc] = '\0';
	Sw2_nc[ilin] = nc;
	if (nc > Sw2_size[ilin]) Sw2_size[ilin] = nc;
	Sw2_type[ilin] = *ktype;
	if (*nline > Snw2) Snw2 = *nline;
/*
.....End of routine
*/
done:
	return;
}

/*********************************************************************
**    E_FUNCTION :  nclf_storew2(nline,knw2,kins)
**       Stores the 'w2' buffer in the source file.
**    PARAMETERS   
**       INPUT  : 
**          nline    Source line number to store.  Based on 0.
**          knw2     Number of lines in 'w2' buffer to store.
**          kins     Record within 'w2' buffer to start insert mode.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_storew2(nline,knw2,kins)
UM_int4 *nline;
UM_int2 *knw2;
UM_int2 *kins;
{
	int ilin,i;
	UM_int2 insm,ityp;
/*
.....Initialize routine
*/
	ilin = *nline;
/*
.....Store 'w2' array in source file
*/
	insm = 0;
	for (i=0;i<*knw2 && i<Snw2;i++)
	{
		if (i >= *kins) insm = 1;
		ityp = Sw2_type[i];
		nclf_putsrc(&ilin,Sw2_buf[i],&Sw2_nc[i],&ityp,&insm);
		ilin++;
	}
/*
.....End of routine
*/
done:
	return;
}

/*********************************************************************
**    E_FUNCTION :  nclf_putmerge(sbuf,knc,ktype,ifl)
**       Stores the provided source line in the source merge list.
**       This list will be merged with the original source list in the
**       'nclf_merge_source' routine.
**    PARAMETERS   
**       INPUT  : 
**          sbuf     Source line to store.
**          knc      Number of characters in 'sbuf'.
**          ktype    Type of record to store.
**                         0 = Normal
**                         1 = INCLUDE
**                         2 = INCERR*
**                         3 = INCFILE
**                         4 = READIT*
**                         5 = READINC
**                        10 = Current Line    (N)
**                        20 = Executing loop  (X)
**                        30 = Executing Macro (C)
**          ifl      0 = Append to end of list, 1 = Overwrite last record.
**       OUTPUT :  none
**    RETURNS      : -1 if could not store line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclf_putmerge(sbuf,knc,ktype,ifl)
char *sbuf;
UM_int4 *knc;
UM_int2 *ktype,*ifl;
{
	int status,nsrc,inc;
	NCL_source_struc src,*psrc;
/*
.....Initialize routine
*/
	status = 0;
/*
.....Initialize source storage
*/
	if (!Sinitm)
	{
		nsrc = UU_LIST_LENGTH(&Ssource);
		if (nsrc == 0) nsrc = 500;
		uu_list_init(&Smerge,sizeof(NCL_source_struc),nsrc,50);
		Sinitm = UU_TRUE;
	}
	nsrc = UU_LIST_LENGTH(&Smerge);
/*
.....Overwrite last line with new data
*/
	if (*ifl == 1 && nsrc > 0)
	{
		psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Smerge);
		inc = nsrc - 1;
		if (*knc > psrc[inc].nc)
		{
			uu_free(psrc[inc].buf);
			psrc[inc].buf = (char *)uu_malloc(sizeof(char)*(*knc+1));
			if (psrc[inc].buf == UU_NULL) goto failed;
		}
		psrc[inc].nc = *knc;
		strncpy(psrc[inc].buf,sbuf,psrc[inc].nc);
		psrc[inc].buf[psrc[inc].nc] = '\0';
		psrc[inc].type = *ktype;
	}
/*
.....Place source commands at end of file
*/
	else
	{
		src.nc = *knc;
		src.size = *knc;
		if (src.nc != 0)
		{
			src.buf = (char *)uu_malloc(sizeof(char)*(src.nc+1));
			if (src.buf == UU_NULL) goto failed;
			strncpy(src.buf,sbuf,src.nc);
			src.buf[src.nc] = '\0';
		}
		src.type = *ktype;
		src.prev = nsrc - 1;
		src.next = nsrc + 1;
		src.lineno = nsrc + 1;
		uu_list_push(&Smerge,&src);
	}
	goto done;
/*
.....Could not allocate memory
*/
failed:;
	status = -1;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  nclf_merge_source(ichg)
**       Merges the loaded source file (*EDT) with the active source
**       file while trying to maintain the original source file's
**       array positions.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          ichg     Returns 1 if changes were made to the pp file.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_merge_source(ichg)
UM_int2 *ichg;
{
	int nmrg,nsrc,i,j,inc,pst,mst,mary[5],ien,fary[3],*mptr,srce,mrge;
	int nary,ntmp,*tptr,icnt;
	UM_int2 kins=0,ityp;
	NCL_source_struc *psrc,*msrc;
	UU_LIST mlist,mtemp;
#if DEBUG == 1
	int tims,timm,tems,temm;
	char sbuf[80];
#endif
/*
.....Initialize routine
*/
	*ichg = 0;
	nsrc = Srec.last + 1;
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
	msrc = (NCL_source_struc *)UU_LIST_ARRAY(&Smerge);
	uu_list_init0(&mlist);
	uu_list_init0(&mtemp);
/*
.....If merge file has no records
.....Then leave the source file as is
*/
	if (!Sinitm) nmrg = 0;
	else nmrg = UU_LIST_LENGTH(&Smerge);
	if (nmrg == 0) goto done;
/*
.....Source file is blank
.....Copy the merge file into the source file
*/
	if (nsrc == 0)
	{
		for (i=0;i<nmrg;i++)
		{
			ityp = msrc[i].type;
			nclf_putsrc(&i,msrc[i].buf,&msrc[i].nc,&ityp,&kins);
		}
		goto done;
	}
/*
.....Build an array of matching lines
*/
/*	inc = nmrg < 100 ? 10 : nmrg/10;*/
	inc = 100;
	uu_list_init(&mlist,sizeof(int)*5,inc,inc);
	pst = mst = 0;
	ien = 0;
#if DEBUG == 1
	gtimx(&tims,&timm);
#endif
	icnt = 4;
	do
	{
		srce = nsrc > 100 ? pst + (nsrc-pst) / icnt : nsrc;
		mrge = nmrg > 100 ? mst + (nmrg-mst) / icnt : nmrg;
		icnt--;
		if (srce >= nsrc || mrge >= nmrg)
		{
			srce = nsrc; mrge = nmrg;
			ien = 1;
		}
		nary = UU_LIST_LENGTH(&mlist);
		S_match_range(psrc,pst,srce,msrc,mst,mrge,10,&mlist);
		mptr = (int *)UU_LIST_ARRAY(&mlist);
		inc = UU_LIST_LENGTH(&mlist) - 1;
		if (inc-nary > 0 && mptr[inc*5+4] == 0 && ien == 0)
		{
			pst = mptr[inc*5]; mst = mptr[inc*5+2];
			uu_list_delete(&mlist,inc,1);
		}
		else
		{
			pst = srce; mst = mrge;
		}
	} while (pst < nsrc && mst < nmrg);
#if DEBUG == 1
	gtimx(&tems,&temm);
	sprintf(sbuf,"Matching time = %d secs %d ms",tems-tims,temm-timm);
	NclxDbgPstr(sbuf);
	tims = tems; timm = temm;
#endif
/*
.....Perform a finer match on the unmatched sections
*/
	uu_list_init(&mtemp,sizeof(int)*5,inc+1,inc+1);
	mptr = (int *)UU_LIST_ARRAY(&mlist);
/*
........First merge disjointed sections
*/
	nary = UU_LIST_LENGTH(&mlist);
	for (i=nary-2;i>=0;i--)
	{
		if (mptr[i*5+4] == mptr[(i+1)*5+4])
		{
			mptr[i*5+1] = mptr[(i+1)*5+1];
			mptr[i*5+3] = mptr[(i+1)*5+3];
			uu_list_delete(&mlist,i+1,1);
		}
	}
/*
........Match unmatched sections
*/
	mptr = (int *)UU_LIST_ARRAY(&mlist);
	nary = UU_LIST_LENGTH(&mlist);
	for (i=0;i<nary;i++)
	{
		if (mptr[i*5+4] == 0)
		{
			if (mptr[i*5] <= mptr[i*5+1] && mptr[i*5+2] <= mptr[i*5+3])
			{
				S_match_range(psrc,mptr[i*5],mptr[i*5+1]+1,msrc,mptr[i*5+2],
					mptr[i*5+3]+1,1,&mtemp);
#if DEBUG == 1
				tptr = (int *)UU_LIST_ARRAY(&mtemp);
#endif
				ntmp = UU_LIST_LENGTH(&mtemp);
				if (ntmp > 1)
				{
					uu_list_delete(&mlist,i,1);
					uu_list_insert_list(&mlist,i,&mtemp);
					i = i + ntmp - 1;
					mptr = (int *)UU_LIST_ARRAY(&mlist);
				}
				UU_LIST_EMPTY(&mtemp);
			}
		}
	}
#if DEBUG == 1
	gtimx(&tems,&temm);
	sprintf(sbuf,"Section time = %d secs %d ms",tems-tims,temm-timm);
	NclxDbgPstr(sbuf);
	tims = tems; timm = temm;
#endif
/*
.....Matching is finished
.....Store the matched lines first
.....in case the record types changed
*/
	mptr = (int *)UU_LIST_ARRAY(&mlist);
#if DEBUG == 1
	for (i=0;i<UU_LIST_LENGTH(&mlist);i++)
	{
		sprintf(sbuf,"Mary[%d] = %3d %3d %3d %3d %d",i,mptr[i*5],mptr[i*5+1],
			mptr[i*5+2],mptr[i*5+3],mptr[i*5+4]);
		NclxDbgPstr(sbuf);
	}
#endif
	for (i=0;i<UU_LIST_LENGTH(&mlist);i++)
	{
		if (mptr[i*5+4] == 1)
		{
			for (j=mptr[i*5+2];j<=mptr[i*5+3];j++)
			{
				inc = S_find_line(mptr[i*5]++);
				psrc[inc].type = msrc[j].type;
			}
		}
	}
#if DEBUG == 1
	gtimx(&tems,&temm);
	sprintf(sbuf,"Matched Cases = %d secs %d ms",tems-tims,temm-timm);
	NclxDbgPstr(sbuf);
	tims = tems; timm = temm;
#endif
/*
.....Store unmatched entities
*/
	for (i=UU_LIST_LENGTH(&mlist)-1;i>=0;i--)
	{
		if (mptr[i*5+4] == 0)
		{
			*ichg = 1;
/*
........First delete existing records
*/
			if (mptr[i*5] <= mptr[i*5+1]) nclf_delsrc(&mptr[i*5],&mptr[i*5+1]);
/*
........Now insert the new records
*/
			kins = 1;
			for (j=mptr[i*5+3];j>=mptr[i*5+2];j--)
			{
				ityp = msrc[j].type;
				nclf_putsrc(&mptr[i*5],msrc[j].buf,&msrc[j].nc,&ityp,&kins);
			}
		}
	}
#if DEBUG == 1
	gtimx(&tems,&temm);
	sprintf(sbuf,"Unmatched Cases = %d secs %d ms",tems-tims,temm-timm);
	NclxDbgPstr(sbuf);
	tims = tems; timm = temm;
#endif
/*
.....End of routine
*/
done:;
	if (Sinitm)
	{
		uu_list_free(&Smerge);
		uu_list_free(&mlist);
		uu_list_free(&mtemp);
	}
	Sinitm = UU_FALSE;
	return;
}

/*********************************************************************
**    E_FUNCTION :  nclf_src_line_to_rec(nline,nrec)
**       Converts a line number to the index within the source file
**       storage that points to this line.
**    PARAMETERS   
**       INPUT  : 
**          nline    Source line number. Based on 1.
**       OUTPUT :  
**          nrec     Index into source storage.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_src_line_to_rec(nline,nrec)
UM_int4 *nline,*nrec;
{
	int ilin;
/*
.....Find position within source array
*/
	ilin = *nline - 1;
	if (ilin > Srec.last) *nrec = UU_LIST_LENGTH(&Ssource) + ilin-Srec.last - 1;
	else
	{
		*nrec = S_find_line(ilin);
		if (*nrec == -1) *nrec = Sptr.last + 1;
	}
}

/*********************************************************************
**    E_FUNCTION :  nclf_src_rec_to_line(nrec,nline)
**       Converts an index into the source file storage to a line number.
**    PARAMETERS   
**       INPUT  : 
**          nrec     Index into source storage.
**       OUTPUT :  
**          nline    Source line number.  Returns eof-line +1 if the source
**                   record does not exist.  Based on 1.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_src_rec_to_line(nrec,nline)
UM_int4 *nline,*nrec;
{
	int np;
	NCL_source_struc *psrc;
/*
.....Return line number of array position
*/
	np = UU_LIST_LENGTH(&Ssource) - 1;
/*	if (*nrec > np) *nline = np + 2;*/
	if (*nrec > np) *nline = Srec.last + 2;
	else
	{
		psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
		*nline = psrc[*nrec].lineno;
/*
.....Set current pointers
*/
		Srec.current = *nline - 1;
		Sptr.current = *nrec;
	}
}

/*********************************************************************
**    E_FUNCTION :  nclf_src_next_rec(nrec)
**       Returns the next available index within the source file.
**       Typically called prior to inserting a line into the source file.
**    PARAMETERS   
**       INPUT  : 
**          nline    Source line number. Based on 1.
**       OUTPUT :  
**          nrec     Index into source storage.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_src_next_rec(nrec)
UM_int4 *nrec;
{
/*
.....Return next position within source array
*/
	*nrec = UU_LIST_LENGTH(&Ssource);
}

/*********************************************************************
**    E_FUNCTION :  ncl_close_tmpsrc()
**       Closes the temporary work file.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_close_tmpsrc()
{
	if (Sfd != UU_NULL)
	{
		ux_fclose0(Sfd);
		Sfd = UU_NULL;
	}
}

/*********************************************************************
**    E_FUNCTION :  ncl_open_tmpsrc()
**       Opens an existing temporary work file.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_open_tmpsrc()
{
	if (Sfd == UU_NULL)
		ux_fopen0(Stemp_file,"a",&Sfd);
}

/*********************************************************************
**    I_FUNCTION :  S_init_source(flag)
**       Initializes the source line storage.
**    PARAMETERS   
**       INPUT  :
**          flag    = UU_TRUE = Open temporary work file, UU_FALSE = don't.
**       OUTPUT :  none
**    RETURNS      : UU_FAILURE if could not initialize storage.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_init_source(flag)
UU_LOGICAL flag;
{
	UM_int2 idx,ival;
	int status,mode,file_status,inc;
	UX_pathname lpgm,fname,tmp,dir;
/*
.....Initialize source file storage
*/
	if (!Sinit)
	{
		uu_list_init(&Ssource,sizeof(NCL_source_struc),2000,500);
		Sptr.first = 0;
		Sptr.current = Sptr.last = -1;
		Srec.first = 0;
		Srec.current = Srec.last = -1;
/*
.....Open temporary file for storing source
.....in case NCL aborts
*/
		idx = 35; getifl(&idx,&ival);
		if (ival != 1 && flag)
		{
			if (UL_program[0] == '\0') strcpy(lpgm,"partpgm.ncl");
			else strcpy(lpgm,UL_program);
/*
........Create temporary name
*/
			inc = 0;
			mode = UX_EXISTS;
			do
			{
				ul_break_fname(lpgm,dir,tmp);
				sprintf(fname,"~%d_%s",inc,tmp);
				ul_build_full_fname(dir,fname,UL_program_suffix,tmp);
				status = ux_file_inquire(UU_NULL,UU_NULL,tmp,UU_NULL,UU_NULL,
					&mode,&file_status,Stemp_file,UX_NPRTERRS);
				if (status == UU_SUCCESS && mode != (mode|UX_NEXISTS))
					status = UU_FAILURE;
				inc++;
			} while (status != UU_SUCCESS && inc < 100);
/*
........Could not open temporary file
*/
			if (status != UU_SUCCESS)
			{
				ud_wrerr("Could not open part program work file.\n\
							Any changes to the part program cannot be recovered.\n\
							A maximum of 100 work files with the same name are allowed.");
			}
/*
........Open filename
*/
			else
			{
				ul_remove_quotes(Stemp_file);
				status = ux_fopen0(Stemp_file,"w",&Sfd);
			}
			if (status != UU_SUCCESS) Sfd = UU_NULL;
		}
		Sinit = UU_TRUE;
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION :  S_store_line(ilin,sbuf,knc,ktype,changed)
**       Stores the provided source line in the location pointed to by
**       'ilin'.
**    PARAMETERS   
**       INPUT  : 
**          ilin     Fortran record number to store source line at.
**                   The actual array number will be calculated.
**          sbuf     Source line to store.
**          knc      Number of characters in 'sbuf'.
**          ktype    Type of record to store.
**       OUTPUT :  none
**    RETURNS      : UU_FAILURE if could not store line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_store_line(ilin,sbuf,knc,ktype,changed)
int ilin;
char *sbuf;
int knc;
UM_int2 ktype;
UU_LOGICAL *changed;
{
	int nsiz,inc,status,nc,i;
	UU_LOGICAL istor;
	NCL_source_struc *psrc;
/*
.....Initialize routine
*/
	nsiz = UU_LIST_LENGTH(&Ssource);
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
/*
.....Find position within source array
.....to store line
*/
	inc = S_find_line(ilin);
	if (inc == -1) goto failed;
/*
.....Set the current pointers
*/
	Sptr.current = inc;
	Srec.current = ilin;
/*
.....New data is larger than previous data
.....allocate more space
*/
	if (knc > psrc[inc].size)
	{
		if (psrc[inc].buf != UU_NULL) uu_free(psrc[inc].buf);
		psrc[inc].buf = (char *)uu_malloc(sizeof(char)*(knc+1));
		if (psrc[inc].buf == UU_NULL) goto failed;
		psrc[inc].size = knc;
		psrc[inc].nc = 0;
	}
/*
.....Store new data
*/
	if (knc != 0)
	{
/*
........Don't overwrite comment columnts
*/
		istor = UU_TRUE;
		if (psrc[inc].nc >= UL_comment_column && UL_comment_column != 0)
		{
			for (i=UL_comment_column-2;i>=0;i--) if (psrc[inc].nc != ' ') break;
			nc = i + 1;
		}
		else
			nc = knc;
		if (knc == nc && strncmp(sbuf,psrc[inc].buf,nc) == 0) istor = UU_FALSE;
/*
........Store buffer
*/
		if (istor)
		{
			strncpy(psrc[inc].buf,sbuf,knc);
			psrc[inc].buf[knc] = '\0';
		}
	}
	psrc[inc].nc = knc;
	psrc[inc].type = ktype;
	psrc[inc].lineno = ilin + 1;
	*changed = istor;
/*
........Write record to temp file
*/
	if (istor) S_write_source(psrc,inc);
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to store record
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_insert_line(ilin,sbuf,knc,ktype)
**       Inserts the provided source line prior to the location pointed
**       to by 'ilin'.
**    PARAMETERS   
**       INPUT  : 
**          ilin     Fortran record number to store source line at.
**                   The actual array number will be calculated.
**          sbuf     Source line to store.
**          knc      Number of characters in 'sbuf'.
**          ktype    Type of record to store.
**       OUTPUT :  none
**    RETURNS      : UU_FAILURE if could not store line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_insert_line(ilin,sbuf,knc,ktype)
int ilin;
char *sbuf;
int knc;
UM_int2 ktype;
{
	int nsiz,inc,status,i;
	NCL_source_struc *psrc,src;
/*
.....Initialize routine
*/
	nsiz = UU_LIST_LENGTH(&Ssource);
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
/*
.....Find position within source array
.....to store line
*/
	inc = S_find_line(ilin);
	if (inc == -1) goto failed;
/*
.....Set the current pointers
*/
	Sptr.current = nsiz;
	Srec.current = ilin;
	Srec.last++;
	if (psrc[inc].prev == -1)
	{
		Sptr.first = nsiz;
		Srec.first = ilin;
	}
/*
.....Insert the line
*/
	src.prev = psrc[inc].prev;
	src.next = inc;
	src.nc = knc;
	src.size = 0;
	src.type = ktype;
	src.lineno = ilin + 1;
	src.buf = UU_NULL;
	if (knc != 0) 
	{
		src.buf = (char *)uu_malloc(sizeof(char)*(knc+1));
		if (src.buf == UU_NULL) goto failed;
		strncpy(src.buf,sbuf,knc);
		src.buf[knc] = '\0';
		src.size = knc;
	}
	uu_list_push(&Ssource,&src);
/*
.....Modify the previous/next record
*/
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
	if (inc != -1) psrc[inc].prev = nsiz;
	if (src.prev != -1) psrc[src.prev].next = nsiz;
/*
........Write record to temp file
*/
	i = UU_LIST_LENGTH(&Ssource) - 1;
	S_write_source(psrc,i);
/*
.....Update the line numbers
*/
	do
	{
		psrc[inc].lineno++;
		inc = psrc[inc].next;
	} while (inc != -1);
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to store record
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_find_line(ilin,kinc)
**       Finds the source line in the location pointed to by 'ilin'.
**    PARAMETERS   
**       INPUT  : 
**          ilin     Fortran record number to find.
**                   The actual array number will be calculated.
**       OUTPUT : none
**    RETURNS      : Array index of Fortran record number 'ilin'.
**                   -1 = Requested record does not exist.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_find_line(ilin)
int ilin;
{
	UU_LOGICAL ifwd;
	int nsiz,n,n1,n2,n3,ist,i,inc;
	NCL_source_struc *psrc,*ptr;
/*
.....Initialize routine
*/
	inc = -1;
	psrc = (NCL_source_struc *)UU_LIST_ARRAY(&Ssource);
	if (ilin < 0 || ilin > Srec.last) goto done;
/*
.....Find position within source array
.....to store line
*/
	n1 = ilin;
	n2 = ilin - Srec.current;
	n3 = Srec.last - ilin;
/*
.....Start from beginning of file
*/
	if (n1 < abs(n2) && n1 < n3)
	{
		ist = Sptr.first;
		ifwd = UU_TRUE;
		n = n1;
	}
/*
.....Start from current record
*/
	else if (abs(n2) <= n3)
	{
		ist = Sptr.current;
		ifwd = n2 > 0 ? UU_TRUE : UU_FALSE;
		n = n2;
	}
/*
.....Start from end of file
*/
	else
	{
		ist = Sptr.last;
		ifwd = UU_FALSE;
		n = n3;
	}
/*
.....Position at record
*/
	ptr = &psrc[ist];
	inc = ist;
	for (i=0;i<abs(n);i++)
	{
		if (ifwd) inc = ptr->next;
		else inc = ptr->prev;
		ptr = &psrc[inc];
	}
/*
.....End of routine
*/
done:
	return(inc);
}

/*********************************************************************
**    I_FUNCTION :  S_write_source(psrc)
**       Initializes the source line storage.
**    PARAMETERS   
**       INPUT  :
**          psrc    = Pointer to Source record list.
**          inc     = Index of source record to write out.
**       OUTPUT :  none
**    RETURNS      : UU_FAILURE if could not write record.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_write_source(psrc,inc)
NCL_source_struc *psrc;
int inc;
{
	int status,nc;
	char sbuf[NCL_MAX_COMLINE+20];
/*
.....Only write out record if file is open
*/
	if (Sfd != UU_NULL)
	{
/*
.....Format and write out record
*/
		if (psrc[inc].nc == 0)
			sprintf(sbuf,"%d %d %d %d %d ~\n",inc,psrc[inc].nc,psrc[inc].type,
				psrc[inc].prev,psrc[inc].next);
		else
			sprintf(sbuf,"%d %d %d %d %d %s\n",inc,psrc[inc].nc,psrc[inc].type,
				psrc[inc].prev,psrc[inc].next,psrc[inc].buf);
		status = ux_fwrite0(sbuf,strlen(sbuf),1,Sfd,&nc);
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  S_match_range(psrc,pbeg,nsrc,msrc,mbeg,nmrg,minmat,mlist)
**       Builds a list of matched/unmatched records in the original
**       part program file and the *EDT file.
**    PARAMETERS   
**       INPUT  :
**          psrc    = Pointer to Source record list.
**          pbeg    = Starting line number in 'psrc' to match.
**          nsrc    = Number of records in 'psrc'
**          msrc    = Pointer to Merge record list.
**          mbeg    = Starting record in 'msrc' to match.
**          nmrg    = Number of records in 'msrc'
**          minmat  = Minimum number of records that must match.
**       OUTPUT :
**          mlist    = Array containing pointer to matching records.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_match_range(psrc,pbeg,nsrc,msrc,mbeg,nmrg,minmat,mlist)
NCL_source_struc *psrc;
int pbeg,nsrc;
NCL_source_struc *msrc;
int mbeg,nmrg,minmat;
UU_LIST *mlist;
{
	int pst,mst,fary[3],mary[5];
/*
.....Initialize routine
*/
	pst = pbeg;
	mst = mbeg;
/*
.....Find next matching sequence
*/
	do
	{
		S_find_match(psrc,pst,nsrc,msrc,mst,nmrg,minmat,fary);
		if (fary[2] != 0)
		{
/*
........Unmatched sequence between matching sequences
*/
			if (fary[0] != pst || fary[1] != mst)
			{
				mary[0] = pst;
				mary[1] = fary[0] - 1;
				mary[2] = mst;
				mary[3] = fary[1] - 1;
				mary[4] = 0;
				uu_list_push(mlist,mary);
			}
/*
........Store matching sequences
*/
			mary[0] = fary[0];
			mary[1] = fary[0] + fary[2] - 1;
			mary[2] = fary[1];
			mary[3] = fary[1] + fary[2] - 1;
			mary[4] = 1;
			uu_list_push(mlist,mary);
		}
/*
........No more matching sequences
*/
		else
		{
			mary[0] = pst;
			mary[1] = nsrc - 1;
			mary[2] = mst;
			mary[3] = nmrg - 1;
			mary[4] = 0;
			uu_list_push(mlist,mary);
		}
/*
........Point to next records
*/
		pst = mary[1] + 1;
		mst = mary[3] + 1;
	} while (pst < nsrc && mst < nmrg);
/*
.....Store any remaining lines
*/
	if (pst != nsrc)
	{
		mary[0] = pst;
		mary[1] = nsrc - 1;
		mary[2] = nmrg;
		mary[3] = nmrg - 1;
		mary[4] = 0;
		uu_list_push(mlist,mary);
	}
	else if (mst != nmrg)
	{
		mary[0] = nsrc;
		mary[1] = nsrc - 1;
		mary[2] = mst;
		mary[3] = nmrg - 1;
		mary[4] = 0;
		uu_list_push(mlist,mary);
	}
}

/*********************************************************************
**    I_FUNCTION :  S_find_match(psrc,pst,pnc,msrc,mst,mnc,minmat,mary)
**       Finds the next set of matching records in the source and
**       merge files.
**    PARAMETERS   
**       INPUT  :
**          psrc    = Pointer to Source record list.
**          pst     = Starting line number in 'psrc' to match.
**          pnc     = Number of records in 'psrc'
**          msrc    = Pointer to Merge record list.
**          mst     = Starting record in 'msrc' to match.
**          mnc     = Number of records in 'msrc'
**          minmat  = Minimum number of records that must match.
**       OUTPUT :
**          mary    = Array containing pointer to matching records.
**                       0 = 1st record in 'psrc'
**                       1 = 1st record in 'msrc'
**                       2 = Number of matched records.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_find_match(psrc,pst,pnc,msrc,mst,mnc,minmat,mary)
NCL_source_struc *psrc;
int pst,pnc;
NCL_source_struc *msrc;
int mst,mnc;
int *mary;
{
	int minc,pinc,i,j,inc,nmatch,mincst,pincst,imat,rcur,pcur;
/*
.....Initialize routine
*/
	minc = mst;
	pinc = pst;
	mary[0] = mary[1] = mary[2] = 0;
	rcur = Srec.current;
	pcur = Sptr.current;
/*
.....Find a matching sequence of records
.....By stepping through the files
*/
	i = pinc;
	nmatch = 0;
	do
	{
		j = minc;
		do
		{
			inc = S_find_line(i);
			Srec.current = i;
			Sptr.current = inc;
			imat = 1;
			if (psrc[inc].nc == 0 && msrc[j].nc == 0) imat = 0;
			else if (msrc[j].nc == psrc[inc].nc)
				imat = strcmp(psrc[inc].buf,msrc[j].buf);
/*
.....Found a match
*/
			if (imat == 0)
			{
				if (nmatch == 0)
				{
					pincst = i;
					mincst = j;
				}
				minc = j + 1;
				nmatch++;
				break;
			}
/*
.....No match
........If enough previous records matched
........then exit loop
*/
			else
			{
				if (nmatch >= minmat)
				{
					mary[0] = pincst;
					mary[1] = mincst;
					mary[2] = nmatch;
					goto done;
				}
/*
........Not enough matches
........Keep looking for a longer sequence
*/
				else
				{
					if (nmatch > mary[2])
					{
						mary[0] = pincst;
						mary[1] = mincst;
						mary[2] = nmatch;
					}
				}
				i -= nmatch;
				j -= nmatch;
				nmatch = 0;
			}
			j++;
		} while (j < mnc);
		if (j == mnc)
		{
			if (nmatch > mary[2])
			{
				mary[0] = pincst;
				mary[1] = mincst;
				mary[2] = nmatch;
			}
			minc = mst;
			if (i+1 < pnc) nmatch = 0;
		}
		if (minc >= mnc) break;
		i++;
	} while (i < pnc);
/*
.....Store final matching sequence
*/
/*	if (nmatch != 0)*/
	if (nmatch > mary[2])
	{
		mary[0] = pincst;
		mary[1] = mincst;
		mary[2] = nmatch;
	}
/*
.....End of routine
*/
done:;
   Srec.current = rcur;
   Sptr.current = pcur;
}
/*********************************************************************
**    E_FUNCTION :  nclf_delsrc(first,last)
**       Deletes the requested source lines.
**    PARAMETERS   
**       INPUT  : 
**          first    Start of source lines to delete.
**          last     End of source lines to delete.
**       OUTPUT :  none
**    RETURNS      : -1 if could not delete lines.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void  nclc_delsrc(first,last)
int first, last;
{
	nclf_delsrc(&first, &last);
}

/*********************************************************************
**    E_FUNCTION :  ncl_autosave_pop()
**       Deletes the first auto save file using the index in the 
**       index list and current part program name.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : -1 if could not delete file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_autosave_pop()
{
	int i,status,mode,file_status;
	UX_pathname lpgm,asfile,fname,dir,tmp;
/*
.....Get correct file name to search for
*/
	if (UL_program[0] == '\0') strcpy(lpgm,"partpgm.ncl");
	else strcpy(lpgm,UL_program);
	mode = UX_EXISTS;
/*
.....Build auto save file name to search for
*/
	ul_break_fname(lpgm,dir,tmp);
	sprintf(fname,"%%%d_%s",Ssaves[0],tmp);
	ul_build_full_fname(dir,fname,UL_program_suffix,tmp);
	status = ux_file_inquire(UU_NULL,UU_NULL,tmp,UU_NULL,UU_NULL,
		&mode,&file_status,asfile,UX_NPRTERRS);
/*
.....file found, why return? only not found, return
.....Yurong
//	if (status == UU_SUCCESS && mode != (mode|UX_NEXISTS))
//		return UU_SUCCESS;
*/
/*
.....if file not found, return UU_FAILURE since no file to delete
*/
	if (status != UU_SUCCESS || mode == (mode|UX_NEXISTS))
		return UU_FAILURE;
/*
.....Delete if found, and update index list and count
*/
	status = ux_delete(asfile,UX_PRTERRS);
	if (status == UU_SUCCESS)
	{
		for (i=0;i<NCL_saves-1;i++)
			Ssaves[i] = Ssaves[i+1];
		NCL_saves--;
	}
	return status;
}

/*********************************************************************
**    E_FUNCTION     :  ncl_cond_auto_save()
**       check to see if we should perform an auto save
**			and if conditions are appropriate then do so.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_cond_auto_save()
{
	UM_int2 idx,ival;
	int status;
	char buf[120];

	uu_denter(UU_RTRC,(us,"ncl_cond_auto_save"));
	idx = 35; getifl(&idx,&ival); if (ival == 1) return;
/*
.....check if autosave is enabled and if auto save condition is hit
*/
	if(NCL_ason)
	{
		if((NCL_switch == 0 && (time((long *)0)-NCL_last_autosave > NCL_time)) ||
			(NCL_switch == 1 && (NCL_chg_cnt >= NCL_num_chg)))
		{
			sprintf (buf,"Part Program Autosave Condition Hit...");
			ud_prmerr(buf);
			status = ncl_auto_save_ppg();
			if (status == UU_FAILURE) goto failed;
/*
.....set up values for next time
*/
			NCL_last_autosave = time((long *) 0);
			NCL_chg_cnt = 0;
		}
	}
	else if (NCL_ason)
	{
		ud_prmerr("Maximum number of Auto Save files created. Turning off Auto Save");
		NCL_ason = UU_FALSE;
	}
	goto done;
failed:
	ud_prmerr("Unable to autosave source. Error reading current source.");
done:
	uu_dexit ;
	return;
}

/*********************************************************************
**    E_FUNCTION     :  ncl_auto_save_ppg()
**       check to see if we should perform an auto save
**			and if conditions are appropriate then do so.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_auto_save_ppg()
{
	int i,status,nsrc,mode,file_status,inc = 0;
	UM_int2 wflg;
	UX_pathname lpgm,asfile,fname,dir,tmp;
	UM_f77_str name;
	char buf[256];

	nsrc = UU_LIST_LENGTH(&Ssource);
	if (nsrc <= 0) return 1;
	UM_init_f77_str(name, asfile, UX_MAX_PATHLEN);
/*
.....Make sure auto save file list is saved
*/
	if (!Sauto_init) Sauto_init = UU_TRUE;
/*
.....Create new autosave file.
*/
	if (UL_program[0] == '\0') strcpy(lpgm,"partpgm.ncl");
	else strcpy(lpgm,UL_program);
	mode = UX_EXISTS;
	do
	{
		ul_break_fname(lpgm,dir,tmp);
		sprintf(fname,"%%%d_%s",Sauto_ind,tmp);
		ul_build_full_fname(dir,fname,UL_program_suffix,tmp);
		status = ux_file_inquire(UU_NULL,UU_NULL,tmp,UU_NULL,UU_NULL,
			&mode,&file_status,asfile,UX_NPRTERRS);
		if (status == UU_SUCCESS && mode != (mode|UX_NEXISTS))
			status = UU_FAILURE;
		Sauto_ind++; inc++;
	} while (status != UU_SUCCESS && inc < 1000);
	wflg = 0; i = strlen(asfile);
	status = savepp(UM_addr_of_f77_str(name), &i, &wflg);
/*
........Could not open auto save file
*/
	if (status != UU_SUCCESS)
	{
		sprintf (buf,"Could not open auto save file.\n\
			Any changes to the part program cannot be recovered.\n\
			A maximum of %d auto save files with the same name are allowed.",
			MAXAUTO);
		ud_wrerr(buf);
		goto failed;
	}
/*
........Open filename
*/
	else
	{
		if (status != UU_SUCCESS) goto failed;
/*
.....Make sure there is room in list of indices.
*/
		if ((NCL_max_saves == 0 && NCL_saves == MAXAUTO-1) ||
			(NCL_saves + 1 > NCL_max_saves)) ncl_autosave_pop();
		Ssaves[NCL_saves++] = Sauto_ind - 1;
	}
	goto done;
failed:
	status = UU_FAILURE;
done:
/*
.....Close auto save since it will only be written to this one time.
*/
	return status;
}

/*********************************************************************
**    E_FUNCTION     :  ncl_snap_save()
**       force an autosave whether enabled or not and regardless
**			of whether the normal conditions are met.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_snap_save()
{
	int status;

	uu_denter(UU_RTRC,(us,"ncl_snap_save()"));
	status = ncl_auto_save_ppg();
	if (status != UU_SUCCESS)
	{
		uu_dprint(UU_RITRC,(us,"bad status on autosave open %d",status));
		NCL_ason = UU_FALSE;
		uu_uerror0(UU_UBASE,24);	/* tell user no can do. */
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  ncl_recover_ppg()
**       Recover an auto save part program file
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_recover_ppg()
{
	int inum;
	UU_LOGICAL answer,init,ud_yesno();
	UX_pathname lpgm,dir,tmp,load;
	char ext[UX_SUFFIX_LEN],suffix[UX_SUFFIX_LEN];
	char sbuf[80],descrip[80];
/*
.....Set up file browser settings.
*/
	strcpy(sbuf,"Load Autosave File");
	if (UL_program[0] == '\0')
	{
		strcpy(tmp,"partpgm\0");
		strcpy(suffix,"ncl\0");
	}
	else
	{
		strcpy(lpgm,UL_program);
		ul_break_fname(lpgm,dir,tmp);
		strcpy(suffix,UL_program_suffix);
	}
	sprintf(ext,"%%*_%s.%s",tmp,suffix);
	sprintf(descrip,"Autosave Files (%%*_%s.%s)",tmp,suffix);
	inum = 0;
	load[0] = '\0';
/*
.....Load file browser and get file from user.
*/
	ud_get_filename(sbuf,sbuf,ext,load,&inum,descrip, 1);
	if (inum <= 0) return;
/*
.....Prompt user before overwriting current source.
*/
	answer = ud_yesno(0, "Overwrite program file?", "Overwrite program file?");
	if (answer)
	{
		init = Sauto_init;
		Sauto_init = UU_FALSE;
		ncl_open_file(load);
		Sauto_init = init;
	}
}

/*********************************************************************
**    E_FUNCTION     :  nclf_close_autosave()
**       Close all auto save files generated during the current
**       ncl session.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_close_autosave()
{
	int i,nsrc,status;
	UX_pathname *fname;
	int tmp = NCL_saves;

	if (Sauto_init)
	{
		while (NCL_saves > 0) 
		{
/*
.....check return to avoid recursion loop
*/
			status = ncl_autosave_pop();
			if (status==UU_FAILURE)
				break;
		}
		Sauto_init = UU_FALSE;
		Sauto_ind = 0; NCL_saves = 0;
	}
}

