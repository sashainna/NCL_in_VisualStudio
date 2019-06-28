/*********************************************************************
**    NAME         :  GKS archive routines .
**
**    CONTAINS:
**    ginitarch() 						-- initialize archive.
**    gopenarcf(fname)		 			-- open archive file.
**    gclosarcf() 						-- close archive file.
**    glenarcf(fd)						-- length of archive file.
**    gsetconres(value) 				-- set archive conflict resolution.
**    ug_check_wstat(i,len,string)	-- check write status.
**    ug_check_rstat(i,len,string)	-- check read status.
**    ug_warctofc()						-- write table of contents.
**    ug_rarctofc()						-- read table of contents.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
** 
**    MODULE NAME AND RELEASE LEVEL
**       garch.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:17
*********************************************************************/

#include <stdio.h>
#include "udebug.h"
#include "usysdef.h"
#include "gtbl.h"
#include "xfsys1.h"
#include "xenv1.h"

#define GARCHMAIN
#include "garch.h"

#define TRUE 1
#define FALSE 0


#define ANMLEN	UX_MAX_PATH_LEN
static char archive_name[ANMLEN];

static void ug_rarctofc();
static void ug_warctofc();

void ug_check_wstat();
void ug_check_rstat();
void ug_archrewrite();
char *ug_lsielt();
char *ug_lsiins();

/*********************************************************************
**    E_FUNCTION     :  ginitarch() -- initialize archive.
**								Must be called once before other archive fctns.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ginitarch()
{
	uu_denter(UU_GTRC,(us,"ginitarch()"));

 	/* initialize the conflict resolution flag to replace */
	ug_conresflag=UG_REPLACE;

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     :  Gint gopenarcf(fname) -- open archive file.
**    PARAMETERS   
**       INPUT  : 	char *fname -- archive file name.
**       OUTPUT :  
**    RETURNS      : 0 if file existed, 1 if created, -1 if can't create.
**    SIDE EFFECTS : none
**    WARNINGS     : This function may get called with a quoted pathname,
**							Unicad's internal representation of system dependent
**							paths.
*********************************************************************/

Gint gopenarcf(fname)
/*$ INPUT */
char *fname;
{
	FILE *xfd;							/* Unicad archive file pointer */
	Gint irtn=0;

	uu_denter(UU_GTRC,(us,"gopenarcf(%s)",fname));

	if (UG_AROP==TRUE) {
		uu_dprint(-1,(us,"gopenarcf(%s). Another archive file is open", fname));
		irtn = -1;
		goto rtn;
	}

	UG_NEWFILE = FALSE;							/* Opening new file */
	if( strlen(fname) < ANMLEN ) {
		strcpy( archive_name, fname );		/* Copy fname to nqfname */
		ux_strip_quotes( archive_name );		/* Strip quotes from string */
	}
	else {
		uu_dprint(-1,(us,"ERROR gopenarcf. pathname overflow %s", fname));
		irtn = -1;
		goto rtn;
	}
	
	/* Open for read/write */
/*	if( (UG_Archfid = open(archive_name, 2)) < 0)  */
#if UU_COMP==UU_WIN2K
/*
......must be open as binary mode
*/
	if( (UG_Archfid = open(archive_name, 0x8000 | 0x0002)) < 0) 
#else
	if( (UG_Archfid = open(archive_name, 2)) < 0) 
#endif
	{ 

		/* Open for read */
#if UU_COMP==UU_WIN2K
		if( (UG_Archfid = open(archive_name, 0x8000)) < 0) {
#else
		if( (UG_Archfid = open(archive_name, 0)) < 0) {
#endif
			/* Create the file (create with xio to put in unicad header) */
			if( ux_create_file(fname, 0644, NULL, "STREAM", "BINARY", 
						"UX_NOEXTRA", &xfd,	UX_PRTERRS) != 0 ) {
      		uu_dprint(-1,(us,"gopenarcf can't open %s\n",fname)); 
				irtn = -1; 
				goto rtn;
			}
    		else {					/* created */
				ux_close(xfd, UX_PRTERRS);				/* Close xio version */
#if UU_COMP==UU_WIN2K
				if( (UG_Archfid = open(archive_name, 0x8000 | 0x0002)) < 0) { /* Reopen file */
#else
				if( (UG_Archfid = open(archive_name, 2) ) < 0) { /* Reopen file */
#endif				
					uu_dprint(-1,(us,
						"ERROR gopenarcf. can't reopen created xio file %s (%s)", 
						fname, archive_name));
					irtn = -1; 
					goto rtn;
				}
				UG_NEWFILE = TRUE;
				irtn=1;
			}
   	}
	}

	uu_dprint(UU_GTRC,(us,"UG_Archfid = %d", UG_Archfid));

	ug_lsiinit( ug_tofc );			/* Initialize table of contents list */

	if( !UG_NEWFILE ) {				/* Read the archive table of contents */
		ug_rarctofc();				
	}

	UG_AROP = TRUE;					/* Archive file is currently open */
	ug_tofcmod = FALSE;				/* Archive's table of contents not modified */

rtn:
	uu_dprint(UU_GTRC,(us,"gopenarcf returns %d", irtn));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     :  gclosarcf() -- close archive file.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gclosarcf()
{
	Gint len;

	uu_denter(UU_GTRC,(us,"gclosarcf()"));
		

	if (UG_AROP==FALSE) {	/* Error */
		uu_dprint(-1,(us,"gclosarcf(). No archive file is open"));
		uu_dexit;
		return;
	}

	UG_AROP = FALSE;			/* No archive file open */

	if( !ug_tofcmod ) {		/* No structures added, just close */
		uu_dprint(UU_GTRC,(us,"old file/no structures added"));
		close(UG_Archfid);
		uu_dexit;
		return;
	}

	len = ug_lsinelt( ug_tofc );		/* Number of entries in table */
	uu_dprint(UU_GTRC,(us,"%d table entries", len));

	/* If table of contents won't fit... (this should almost never happen) */
	if( len > ug_tofcsize ) {
		ug_archrewrite(len);
	}

	else {		/* Normal archive close... just write table of contents */
		ug_warctofc(UG_Archfid, len);
		close(UG_Archfid);
	}

	ug_tofcmod = FALSE; 		/* Remember archive table of contents up to date */

	uu_dexit;

}

/*********************************************************************
**    E_FUNCTION :  Gint glenarcf(file) -- return length of digs data
**		in an archive file.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :   char *file -- name of archive file.
**    RETURNS      : length of archive file, or < 0 if doesn't exist.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gint glenarcf(file)						/* length of archive file */
/*$ INPUT */
char *file;
{
	char lname[ANMLEN];
	Gint len;
	int fd;

	uu_denter(UU_GTRC,(us,"glenarcf(%s)", file));

	if( strlen(file) < ANMLEN ) {
		strcpy( lname, file );				/* Copy file to lname */
		ux_strip_quotes( lname );			/* Strip quotes from string */
	}
	else {
		uu_dprint(-1,(us,"ERROR gopenarcf. pathname overflow %s", file));
		uu_dexit;
		return(EOF);
	}

	/* If this is the current file, just return current archive length */
	if (UG_AROP==TRUE && (strcmp(archive_name, lname) == 0) ) {

			uu_dprint(UU_GTRC,(us,"length of open file is %d",ug_filelen));
			len = ug_filelen - UX_HDR_LEN;
	}
	
	/* Open the file, and return its data length */
	else {

		/* Open for read */
#if UU_COMP==UU_WIN2K
		if( (fd = open(lname, 0x8000)) < 0) 
#else
		if( (fd = open(lname, 0)) < 0) 
#endif
		{
			uu_dprint(UU_GTRC,(us,"unable to open file %s (lname)", file, lname));
			len = EOF;
   	}
		else {
			lseek(fd, UX_HDR_LEN, 0);			/* Seek past uniad header */
			read(fd, &len, sizeof(int));		/* Read length */
			len -= UX_HDR_LEN;
			close(fd);
		}
	}

	uu_dprint(UU_GTRC,(us,"glenarcf returns %d",len));
	uu_dexit;
	return(len);
}

/*********************************************************************
**    E_FUNCTION     :  gsetconres(value) -- set archive conflict resolution.
**				If value=UG_REPLACE, segments read from the archive file will
**				replace like numbered segments in memory. 
**				If value=UG_MAINTAIN, segments in memory will not be overwritten.
**
**				**** Not implemented yet ****
**
**    PARAMETERS   
**       INPUT  : 	Gconres value -- either UG_MAINTAIN or UG_REPLACE.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gsetconres(value)
/*$ INPUT */
  Gconres value;
{
	if (value==UG_MAINTAIN) 
		ug_conresflag=UG_MAINTAIN;
 	else 
		ug_conresflag=UG_REPLACE;
}

/*********************************************************************
**    I_FUNCTION     :  ug_check_wstat(i,len,string)
**			Check the return status on a write to an archive file
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_check_wstat(i,len,string)
Gint i;
Gint len;
char *string;
{
	char us[120];

	if (i != len) {
		fprintf(stderr,"archive file write error -- %s",string); 
		uu_denter2(-1,(us,"archive file write error -- %s",string));
		uu_dexit;
	}
}


/*********************************************************************
**    I_FUNCTION     :  ug_check_rstat(i,len,string)
**			Check the return status on a read from an archive file
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_check_rstat(i,len,string)
Gint i;
Gint len;
char *string;
{
	char us[120];

	if (i != len) {
		fprintf(stderr,"archive file read error -- %s",string);
		uu_denter2(-1,(us,"archive file read error -- %s, stat=%d",string,i));
		uu_dexit;
	}
}


/*********************************************************************
**    I_FUNCTION     :  ug_warctofc()
**			Writes the table of contents of an archive file.
**    PARAMETERS   
**       INPUT  :  int fd;		File descriptor of archive.
**       OUTPUT :  int len;	Length of the table of contents.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : Need to fix so that more that 100 entries are
**							written correctly.
*********************************************************************/
static void ug_warctofc(fd, len)
int fd;
int len;
{
	Gint i;
	Gint stat;
	UG_Tofc *item;

	uu_denter(UU_GTRC,(us,"ug_warctof(%d, %d)", fd, len));

	/* Write the file length */
	lseek(fd, UX_HDR_LEN, 0);
	uu_dprint(UU_GTRC,(us,"file length=%d",ug_filelen));
	stat = write( fd, &ug_filelen, sizeof(Gint));			/* Write length */
	ug_check_wstat(stat, sizeof(Gint), "ug_warctofc()");	/* Check status */

	/* Write the table of contents length */
	stat = write( fd, &len, sizeof(len) );
	ug_check_wstat(stat,sizeof(len),"ug_warctofc()");

	/* Could do this loop in one fell swoop */
	for( i=0; i<len; i++ ) {

		/* Get pointer to this table etry */
		item = (UG_Tofc *) ug_lsielt(ug_tofc,i);

		uu_dprint(UU_GTRC,(us,"item %d: id=%d len=%d offset=%d",
			i, item->id, item->len, item->offset));

		/* Write this item */
		stat = write( fd, item, sizeof(UG_Tofc) );
		ug_check_wstat(stat,sizeof(UG_Tofc),"ug_warctofc()");
	}

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_rarctofc()
**			Reads the table of contents of an archive file.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void ug_rarctofc()
{
	Gint i;
	Gint len;
	Gint stat;
	UG_Tofc *item;

	uu_denter(UU_GTRC,(us,"ug_rarctofc()"));

	/* Read file length */
	lseek(UG_Archfid, UX_HDR_LEN, 0);
	i = read (UG_Archfid, &ug_filelen, sizeof(Gint));
	ug_check_rstat (i, sizeof(Gint), "ug_filelen");
	uu_dprint(UU_GTRC,(us,"file length %d",ug_filelen));

	/* Read length of table of contents */
	stat = read( UG_Archfid, &len, sizeof(Gint) );
	ug_check_rstat(stat,sizeof(Gint),"len");
	uu_dprint(UU_GTRC,(us,"files tofc len = %d",len));

	for( i=0; i<len; i++ ) {

		/* Get pointer to this table etry */
		item = (UG_Tofc *) ug_lsiins(ug_tofc, i, sizeof(UG_Tofc));

		/* Read this item */
		stat = read( UG_Archfid, item, sizeof(UG_Tofc) );
		ug_check_rstat(stat,sizeof(UG_Tofc),"ug_rarctofc()");

		uu_dprint(UU_GTRC,(us,"table item %d, id=%d len=%d offset=%d",
			i, item->id, item->len, item->offset));
	}

	/* Set files table of contents length...its next multiple of TOFCINC */
	ug_tofcsize = (len+TOFCINC) / TOFCINC * TOFCINC;
	uu_dprint(UU_GTRC,(us,"file's table of contents space %d",ug_tofcsize));

	uu_dexit;

}

/*********************************************************************
**    I_FUNCTION     :  ug_archrewrite()
**			Replace (rewrite) an archive file.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_archrewrite(len)
Gint len;
{
	Gint i;
	int xfdtmp;					/* Unicad file pointer */
	int fdtmp;					/* Unix file descriptor */
	Gint stat;
	Gint offset;
	UG_Tofc *item;
	int buflen=0;
	char *buffer=NULL;
	static char tmpfile[] = "garchive.tmp";		/* Temp file for copy */
	char *uu_toolmalloc();

	uu_denter(UU_GTRC,(us,"ug_archrewrite(len=%d)",len));

	/* New table of contents won't fit in existing archive file...
	 * Open temporary data file.
	 * Copy sturctures into temp archive one painfull sturcture at a time.
	 * Write new table of contents with updated structure offsets.
	 * Delete the archive file.
	 * Rename the temp file to archive file.
	 */

	/* Close the archive */
	close(UG_Archfid);

	/* Re-open for read only */
#if UU_COMP==UU_WIN2K
/*
......must be open as binary mode
*/
	UG_Archfid = open(archive_name, 0x8000);
#else
	UG_Archfid = open(archive_name, 0);
#endif
	
	/* Create file tmp file */
	if( ux_create_file(tmpfile, 0644, NULL, "STREAM", "BINARY", 
		"UX_NOHEADER", &xfdtmp, UX_PRTERRS) != 0 ) {
     	uu_dprint(UU_GTRC,(us,"ERROR ug_archrewrite. can't create tmp file %s", 
			tmpfile)); 
		uu_dexit;
		return; 		/* couldn't create */
	}
	else {			/* Xio file created, close and reopen */
		ux_close(xfdtmp, UX_PRTERRS);
#if UU_COMP==UU_WIN2K
/*
......must be open as binary mode
*/
		fdtmp = open(tmpfile, 0x8000 | 0x0002);
#else
		fdtmp = open(tmpfile, 2);
#endif
		if( fdtmp < 0 ) {
			uu_dprint(-1,(us,
				"ERROR ug_archrewrite. can't reopen created xio file %s", tmpfile));
			uu_dexit;
			return;
		}
	}

	offset = (len+TOFCINC) / TOFCINC * TOFCINC * sizeof(UG_Tofc);
	uu_dprint(UU_GTRC,(us,"end of tofc at %d",offset));
	lseek(fdtmp, offset, 0); 			/* Seek to end of table of contents */

	/* Copy the data */
	for(i=0; i<len; i++) {

		/* Get pointer to this table item */
		item = (UG_Tofc *) ug_lsielt(ug_tofc,i);

		uu_dprint(UU_GTRC,(us,"item %d: id=%d len=%d offset=%d",
			i, item->id, item->len, item->offset));

		lseek(UG_Archfid, item->offset, 0);		/* Seek to data */

		uu_dprint(UU_GTRC,(us,"new offset %d",offset));
		item->offset = offset;

		/* Read this item */
		if( buflen < item->len ) {
			buflen = item->len;
			if( buffer != NULL ) uu_toolfree(buffer);
			buffer = uu_toolmalloc(buflen);
		}
		stat = read( UG_Archfid, buffer, item->len );
		ug_check_rstat(stat, item->len, "ug_archrewrite()");

		/* Write this item on temp file */
		stat = write( fdtmp, buffer, item->len );
		ug_check_wstat(stat, item->len,"ug_archrewrite()");

		offset += item->len;
		
	}

	ug_warctofc(fdtmp, len);				/* Write table of contents */
	close(fdtmp);								/* Finished with copy */

	/* Delete the archive file */
	close(UG_Archfid);								/* Close the archive */
	ux_delete(archive_name, UX_PRTERRS);		/* Remove the archive */

	/* Rename our temp file to archive name */
	ux_rename0( tmpfile, archive_name );

	uu_dexit;

}
