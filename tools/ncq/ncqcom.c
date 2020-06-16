/************************************************************************
**
**   FILE NAME: ncqcom.c
**
**	 CONTAINS: 
**              ncqbatch
**              ncq_init
**              ncq_add
**              ncq_del
**              ncq_opnque
**              ncq_lodque
**              ncq_clsque
**              ncq_writeln
**              ncq_readln
**              ncq_dispmsg
**              ncq_selectlst
**              ncq_selectfile
**              ncq_reset_list
**              ncq_additem
**              ncq_delpos
**              ncq_getsel
**              ncq_setlnbuf
**              ncq_sel_lastpos
**              ncq_short_filename
**              ncq_to_upper
**				ncq_getpost_msg
**				ncl_que_is_empty
**				ncq_saveopt_ini
**				ncq_loadque_file
**				ncq_getopt
**				ncq_getlnbuf
**				ncq_break_fname(fullname,dir,fname,ext)
**				ncq_build_fname
**				ncq_get_dir
**				ncq_is_dir
**				ux_vaxdir0
**				ncq_getwd
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqcom.c , 26.3
**    DATE AND TIME OF LAST  MODIFICATION
**       09/25/18 , 10:34:12
**
************************************************************************
*/
#include "usysdef.h"
#if UU_COMP == UU_VAXVMS
#include descrip
#endif
#include <stdio.h>
#include <sys/stat.h>
#define NCQ_MAIN
#include "ncqcom.h"
#undef NCQ_MAIN
#include "mfort.h"
#define NCLVERSION
#include "nclver.h"

void ncq_clsque();
void ncq_writeln ();
void ncq_getsel();
void ncq_dispmsg();
void ncq_selectlst();
void ncq_selectfile();
void ncq_reset_list();
void ncq_additem();
void ncq_setlnbuf();
void ncq_short_filename();
void ncq_parse_filename();
void ncq_to_upper();
void ncq_sel_lastpos();

static FILE *ncq_fptr = NULL;
char ncq_localdir[MAX_PATH];
char ncq_initfile[MAX_PATH];
char ncq_file[MAX_PATH];
char NCQ_file_ext[20][256];
char NCQ_file_desp[20][256];
int NCQ_monitor = 0;
int NCQ_runver;
int NCQ_time_limit = 2;
#if UU_COMP == UU_WIN2K
	static char *delim={"\\"};
#else
	static char *delim={"/"};
#endif
/***********************************************************************
c
c   FUNCTION: ncqbatch (comstr, knc, cmsg)
c
c         Processes the NCQ command line.
c
c   INPUT:
c      comstr  = Input command line.
c      knc     = Number of chars in 'comstr'.
c
c   OUTPUT :
c      cmsg    = Text of error message when an error occurs.
c   RETURN:    None
c
**********************************************************************/
int ncqbatch (comstr,knc,cmsg)
char *comstr;
int knc;
char *cmsg;
{
	int iret,idel,ncf;
	char optstr[20],mfil[MAX_PATH];
/*
.....Initialize routine
*/
	iret = 0;
	idel = 0;
	cmsg[0] = '\0';
/*
.....get filename, options
*/
	iret = ncq_getopt(comstr, knc, optstr, mfil, &ncf, &idel, cmsg);
/*
.....Delete the QUE entry
*/
	if ((iret==0)&&(idel == 1) && (ncf>0))
	{
		ncq_del(mfil,ncf);
	}
	ncq_setlnbuf(optstr);
/*
.....Add the QUE entry
*/
	if ((iret==0)&&(idel == 0) && (ncf>0))
	{
		ncq_add(mfil,ncf);
	}
	if (ncf>0)
	{
		if (iret==0)
			cmsg[0] = '\0';
		iret = 1;
	}
	return(iret);
}

/***********************************************************************
c
c   FUNCTION: ncq_init()
c
c         initialize ncq value
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_init()
{
	FILE *fp;
	int i, j,nc, del, knc, stat, ver_int;
	char *tok, linestr[256], optstr[20], mfil[MAX_PATH], cmsg[256], 
		init_fstr[MAX_PATH], ext[20];
	void ncq_build_fname();

//	NCQ_runver = NCL_version*10 + 0.5;

	NCQ_runver = NCL_version + 0.5;
	ncq_ipglen = 60;
	ncq_lpri   = 'H';
	for (i=0;i<20;i++)
		ncq_linbuf[i] = ' ';
	strcpy(ncq_linbuf, "101010H060");
/*
.....NCQ_time_limit = 0  time value = 0, disabled
.....NCQ_time_limit = 1  time value = 15 mins
.....NCQ_time_limit = 2  time value = 30 mins  default
.....NCQ_time_limit = 3  time value = 60 mins
.....NCQ_time_limit = 4  time value = 120 mins
*/
	NCQ_time_limit = 2;
/*
.....load ncq.ini
*/
	strcpy(ncq_initfile,ncq_localdir);
#if (UU_COMP == UU_WIN2K)
	strcat(ncq_initfile,"\\ncq.ini");
#else
	strcat(ncq_initfile,"/ncq.ini");
#endif
	if ((  (fp = fopen(ncq_initfile, "r") ) == NULL))
	{
//		ver_int = NCL_version*10 + 0.5;
		ver_int = NCL_version + 0.5;
		sprintf(init_fstr,"NCL%d_INIT_FILES", ver_int);
/*
.....Open init file
*/
		ext[0] = '\0';
		ncq_build_fname(ncq_initfile, init_fstr, "ncq.ini", ext);
/*
		fdir = (char*)getenv(init_fstr);
		if (fdir==NULL)
			return;
		strcpy(ncq_initfile, fdir);
#if (UU_COMP == UU_WIN2K)
		strcat(ncq_initfile,"\\ncq.ini");
#else
		strcat(ncq_initfile,"/ncq.ini");
#endif
*/
		if (((fp = fopen(ncq_initfile, "r")) == NULL))
			return;
	}
	i = 0;
	NCQ_file_ext[i][0] = '\0';
	NCQ_file_desp[i][0] = '\0';
	while (fgets(linestr, 256, fp))
	{
		knc = strlen (linestr);
/*
.....remove line ned marker such as '\n' or '\r'
*/
		while ((linestr[knc-1]=='\n') ||
					(linestr[knc-1]=='\r'))  knc--;
/*
.....check if the line start with "-", if yes, then options,
.....else it's default file extension for browser
*/
		if (linestr[0]=='-')
		{
			stat = ncq_getopt(linestr, knc, optstr, mfil, &nc, &del, cmsg);
			if (stat==0)
				ncq_setlnbuf(optstr);
		}
		else if ((linestr[0]!='\0')&&(i<20))
		{
			strcpy(NCQ_file_ext[i], linestr);
			NCQ_file_desp[i][0] = '\0';
			tok = (char*)strtok(linestr, ";");
			if (tok==NULL) 
				continue;
			else
			{
				strcpy(NCQ_file_ext[i], tok);
				tok = (char*)strtok(NULL, "\r\n");
				if (tok!=NULL)
					strcpy(NCQ_file_desp[i], tok);
			}
			j = 0;
			while ((NCQ_file_ext[i][j]==' ') || (NCQ_file_ext[i][j]=='\t'))
				j++;
			strcpy(NCQ_file_ext[i], &(NCQ_file_ext[i][j]));
			j = 0;
			while ((NCQ_file_desp[i][j]==' ') || (NCQ_file_ext[i][j]=='\t'))
				j++;
			strcpy(NCQ_file_desp[i], &(NCQ_file_desp[i][j]));
			i++;
		}
	}
}

/***********************************************************************
c
c   FUNCTION: ncq_add (cfil,knc)
c
c         Add a filename and options to the NCQ list
c
c   INPUT:
c      cfil  = Filename to be added
c      knc   = Number of chars in 'cfil'.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
ncq_add (cfil,knc)
char *cfil;
int knc;
{
	FILE *fptr ;
	char msg[256],buf[MAX_PATH+20];
	int i,ierr;

	fptr = fopen(cfil,"r");
	if (fptr == 0) 
	{
		ncq_short_filename(cfil,buf,59);
		sprintf(msg, "Could not open %s", buf);
		ncq_dispmsg(msg);
		return -1;
	}
	fclose(fptr);
/*
.....Store in queue
.....Support older versions of NCL
.....when filename is less than 120 chars
*/
	ncq_opnque (&ierr);
	if (ierr != 0) return -1;
	ncq_lodque();
	strcpy(buf,cfil);
	if (knc < 120)
	{
		for (i=knc; i<120; i++) buf[i] = ' '; 
		strcpy(&buf[120],ncq_linbuf);
	}
	else
	{
		strcat(buf," ");
		strcat(buf,ncq_linbuf);
	}
	ncq_writeln (buf);
/*
......Redisplay the queue
*/
	ncq_clsque();
	if (UU_BATCH == 0)
	{
		ncq_additem (buf);
		ncq_sel_lastpos();
	}
	return 0;
}

/***********************************************************************
c
c   FUNCTION: ncq_del (cfil,ncf)
c
c         Delete a filename from the NCQ list
c
c   INPUT:
c      cfil   = Filename to be deleted.
c      ncf    = Number of chars in 'cfil'.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
ncq_del(cfil,ncf)
char *cfil;
int ncf;
{
	char buf[MAX_PATH+20], lbuf[200][MAX_PATH+20];
	char lfil[MAX_PATH],lopts[20];
	int i, ierr, cur, status, inc, first, del[100], dinx;
	first = 1;
/*
.....Get the current selection
*/
	cur = -1;
	ncq_getsel(&cur);
	if (cfil[0]=='\0')
	{
		if (cur<0)
		{		
			ncq_dispmsg ("You need to select an entry first.");
			return -1;
		}
	}
/*
.....Open the NCL Que
*/
	ncq_opnque (&ierr);
	if (ierr != 0) return -1;
	rewind(ncq_fptr);
/*
.....Get the next line
.....from the Que
*/
	inc = 0;
	for (i=0; i<100; i++) del[i] = -1;
	dinx = 0;
read:;
	status = ncq_readln(buf);
	if (status!=0) goto write;
	strcpy(lbuf[inc], buf);
/*
.....Check for a match
*/
	ncq_parse_filename(buf,lfil,lopts);
	if (strcmp(cfil,lfil) == 0)
	{
		cfil[0] = '\0';
		first = 0;
		del[dinx] = inc;
		dinx++;
	}
	else if ((UU_BATCH == 0) && (inc == cur) && (first==1))
	{
		cur = -1;
		first = 0;
		del[dinx] = inc;
		dinx++;
	}
	else
		inc = inc + 1; 
	goto read;
write:;
/*
....we need erase the content of original file
....and recreate a new one
*/
	ncq_clsque();
	ncq_fptr = fopen(ncq_file,"w+");
	if (ncq_fptr == 0) 
	{
		ncq_dispmsg("Could not open ncl.que");
		return -1;
	}
	for (i=0; i<inc; i++)
		ncq_writeln(lbuf[i]);
/*
......don't load que again
......just delete the delete item
......from the list
*/
/*
	if (cfil[0]=='\0') 
		ncq_lodque();
*/
	ncq_clsque();
	if (UU_BATCH == 0)
	{
		if (dinx==0)
		{		
			ncq_dispmsg ("No file deleted.");
			i = 0;
		}
		for (i=0; i<dinx; i++)
			ncq_delpos(del[i]);
	}
	return 0;
}


/***********************************************************************
c
c   FUNCTION: ncq_opnque(int *err)
c         open a ncq file
c
c   INPUT:  None
c
c   OUTPUT :   err: not 0: hit a err
c   RETURN:    None
c
**********************************************************************/
ncq_opnque(err)
int *err;
{
	*err = -1;
	ncq_fptr = fopen(ncq_file,"a+");
	if (ncq_fptr == 0) 
	{
		ncq_dispmsg("Could not open ncl.que");
		return -1;
	}
	*err = 0;
	return 0;
}

/***********************************************************************
c
c   FUNCTION: ncq_lodque()
c
c         load a ncq file lists
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
ncq_lodque()
{
	char *line;
	char lnstr[MAX_PATH+20];
	int status = 0;

	strcpy(ncq_linsav, ncq_linbuf);
	if (UU_BATCH == 0)
		ncq_reset_list();
	rewind(ncq_fptr);
read:;
	if ((line=fgets(lnstr,MAX_PATH+20,ncq_fptr))==0)
	{
		if (feof(ncq_fptr)==0)
			goto failed;
		else
		{
			status = 1;
		}
	}
	if ((UU_BATCH == 0) && (status!=1))
		ncq_additem (lnstr);
	if (status==1)
		goto done;
	else
		goto read;
failed:;	
	status = -1;
done:;
	strcpy(ncq_linbuf, ncq_linsav);
	return status;
}

/***********************************************************************
c
c   FUNCTION: ncq_clsque()
c
c         Close a ncq file
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_clsque()
{
	if (ncq_fptr!=NULL)
		fclose(ncq_fptr);
	ncq_fptr = NULL;
}

/***********************************************************************
c
c   FUNCTION: ncq_writeln (buf)
c
c         write a line into ncq file
c
c   INPUT:  buf: line to be wroten
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_writeln (buf)
char *buf;
{
	if (ncq_fptr==NULL)
		return;
	fprintf(ncq_fptr, "%s\n", buf);
}

/***********************************************************************
c
c   FUNCTION: ncq_readln()
c
c         read a line from ncq file
c
c   INPUT:  buf: line to be read
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int ncq_readln (buf)
char *buf;
{
	char *line;
	char lnstr[MAX_PATH+20];
	int nc, status = 0;

	if (ncq_fptr==NULL)
		return -1;

	if ((line=fgets(lnstr,MAX_PATH+20,ncq_fptr))==0)
	{
		if (feof(ncq_fptr)==0)
			goto failed;
		else
		{
			status = 1;
		}
	}
	goto done;
failed:;	
	status = -1;
	return status;
done:;
	if (line!=NULL)
	{
		strcpy(buf, lnstr);
		nc = strlen(buf);
		while ((buf[nc-1]=='\n')||(buf[nc-1]=='\r')) nc--;
		buf[nc] = '\0';
	}
	return status;
}

/***********************************************************************
c
c   FUNCTION: ncq_dispmsg(msg)
c
c         Display a message
c
c   INPUT:  msg: message to be displayed
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_dispmsg(msg)
char *msg;
{
#ifdef WIN32
	ncq_ntdispmsg(msg);
#else
	ncq_unxdispmsg(NULL, msg);
#endif
}

/***********************************************************************
c
c   FUNCTION: ncq_selectlst(indx)
c
c         Selected a list item by index number
c   INPUT:  indx: index number to be selected
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_selectlst(indx)
int indx;
{
#ifdef WIN32
	ncq_ntselectlst(indx);
#else
	ncq_unxselectlst(indx);
#endif
}

/***********************************************************************
c
c   FUNCTION: ncq_selectfile(file)
c
c         Selected a list item by string
c
c   INPUT:  indx: string to be selected
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_selectfile(file)
char *file;
{
#ifdef WIN32
	ncq_ntselectfile(file);
#else
	ncq_unxselectfile(file);
#endif
}

/***********************************************************************
c
c   FUNCTION: ncq_reset_list()
c
c         Reste lists
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_reset_list()
{
#ifdef WIN32
	ncq_ntreset_list();
#else
	ncq_unxreset_list();
#endif
}


/***********************************************************************
c
c   FUNCTION: ncq_additem(item)
c
c         Added a item into a filelist box
c
c   INPUT:  item: item to be added
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_additem(item)
char *item;
{
	char filename[256], tempstr[256], lopts[20];
	ncq_parse_filename(item,tempstr,lopts);
	ncq_short_filename(tempstr,filename,100);
	strcat(filename,"  ");
	strcat(filename,lopts);
#ifdef WIN32
	ncq_ntadditem(filename);
#else
	ncq_unxadditem(filename);
#endif
}

/***********************************************************************
c
c   FUNCTION: ncq_delpos(pos)
c
c         Delete a position from a filelist box
c
c   INPUT:  pos: position to be deleted
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int ncq_delpos(pos)
int pos;
{
#ifdef WIN32
	ncq_ntdel_pos(pos);
#else
	ncq_unxdel_pos(pos);
#endif
	return 0;
}

/***********************************************************************
c
c   FUNCTION: ncq_getsel(sel)
c
c         Get select item index number
c
c   INPUT:  None
c
c   OUTPUT :   sel: selected index
c   RETURN:    None
c
**********************************************************************/
void ncq_getsel(sel)
int *sel;
{
#ifdef WIN32
	ncq_ntgetsel(sel);
#else
	ncq_unxgetsel(sel);
#endif
}

/***********************************************************************
c
c   FUNCTION: ncq_sel_lastpos()
c
c         Select last item in the filelist
c
c   INPUT:  None
c
c   OUTPUT :  None 
c   RETURN:    None
c
**********************************************************************/
void ncq_sel_lastpos()
{
#ifdef WIN32
	ncq_ntsel_lastpos();
#else
	ncq_unxsel_lastpos();
#endif
}

/***********************************************************************
c
c   FUNCTION: ncq_setlnbuf(linbuf)
c
c         Set line buffer global value
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_setlnbuf(linbuf)
char *linbuf;
{
	int i;
	char tmp[4];
	for (i=0; i<20; i++)
		ncq_linbuf[i] = linbuf[i];
	strncpy(tmp, &(ncq_linbuf[7]), 3);
	tmp[3] = '\0';
	ncq_ipglen = atoi(tmp);
	ncq_lpri   = ncq_linbuf[6];
}
/***********************************************************************
c
c   FUNCTION: ncq_getabout_str(msg1, msg2)
c
c         Get about dialog text string
c   INPUT:  None
c
c   OUTPUT :   msg1: first line of about text
c				msg2: second line of about text
c   RETURN:    None
c
**********************************************************************/
void ncq_getabout_str(msg1, msg2)
char *msg1, *msg2;
{
	sprintf(msg1, "Ncq Version %7.2f", NCL_version);
	strcpy(msg2, "Copyright (C) Numerical Control Computer Sciences 1991-2018");
}

/***********************************************************************
c
c   FUNCTION: ncq_short_filename(fin,fout,maxc)
c
c         Truncate a filename for output in a message.
c   INPUT:
c      fin    = Input filename.
c      maxc   = Maximum number of chars in output filename.
c
c   OUTPUT :
c      fout   = Output filename.
c   RETURN:    None
c
**********************************************************************/
void ncq_short_filename(fin,fout,maxc)
char *fin,*fout;
int maxc;
{
	char *p,*strpbrk();
	char b1[MAX_PATH],b2[MAX_PATH];
	int nc1,nc2,i;
/*
.....Filename is short enough
*/
	if (strlen(fin) < maxc)
		strcpy(fout,fin);
/*
.....Shorten filename
*/
	else
	{
		nc1 = strlen(fin) - 1;
		for (i=nc1;i>=0;i--)
		{
#if (UU_COMP == UU_WIN2K)
			if (fin[i] == '\\')
#else
			if (fin[i] == '/') 
#endif
				break;
		}
		if (i > 0)
		{
			strcpy(b1,fin);
			b1[i] = '\0';
			strcpy(b2,&fin[i+1]);
		}
		else
		{
			b1[0] = '\0';
			strcpy(b2,fin);
		}
		nc1 = strlen(b1);
		nc2 = strlen(b2);
/*
........Filename by itself is too long
........Use first part of directory with filename
*/
		if (nc2+5 >= maxc)
		{
/*
....why use &b1[4] instead of b1? to consist with 'shfile' function in opnfil.f,
....use b1
*/
			p = strpbrk(&b1[4],"/\\");
			if (p == 0) nc1 = 0;
			else
			{
				nc1 = p - b1 + 1;
				if (nc1 > maxc/4) nc1 = maxc / 4;
				strncpy(fout,b1,nc1);
			}
			fout[nc1] = '\0';
			strcat(fout,"{...}"); nc1 = nc1 + 5;
			p = b2 + nc2 - maxc + nc1;
			strcat(fout,p);
		}
/*
........Use partial directory and filename
*/
		else
		{
			nc1 = maxc - nc2 - 5;
			strncpy(fout,b1,nc1);
			fout[nc1] = '\0';
			strcat(fout,"{...}");
			strcat(fout,b2);
		}
	}
}

/***********************************************************************
c
c   FUNCTION: ncq_parse_filename(fin,fout,lopts)
c
c         Extracts the filename and options string from an NCL Que line.
c   INPUT:
c      fin    = NCL Que entry.
c
c   OUTPUT :
c      fout   = Output filename.
c      lopts  = Options string.
c   RETURN:    None
c
**********************************************************************/
void ncq_parse_filename(fin,fout,lopts)
char *fin,*fout,*lopts;
{
	int i;
/*
.....Get rid of Que options
*/
	strcpy(fout,fin);
	for (i=strlen(fin)-1;i>=0;i--) if (fout[i] == ' ') break;
	strcpy(lopts,&fout[i+1]);
	for (i=i;i>0;i--) if (fout[i] != ' ') break;
	fout[i+1] = '\0';
}

/***********************************************************************
c
c   FUNCTION: ncq_to_upper(str)
c
c         Convert a string to upper case.
c   INPUT:
c      str    = Input string.
c
c   OUTPUT :
c      str    = Converted string.
c   RETURN:    None
c
**********************************************************************/
void ncq_to_upper(str)
char *str;
{
	int i;
	char *p;
/*
.....Convert string to upper case
*/
	p = str;
	for (i=0;i<strlen(str);i++)
	{
		if (isalpha(*p) && islower(*p)) *p = toupper(*p);
		p++;
	}
}

/***********************************************************************
c
c   FUNCTION: ncq_getlnbuf(linbuf)
c
c         Get line buffer global value
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_getlnbuf(linbuf)
char *linbuf;
{
	int i;
	for (i=0; i<20; i++)
		linbuf[i] = ncq_linbuf[i];
}

/***********************************************************************
c
c   FUNCTION: ncq_getopt(comstr, knc, optstr, mfil, nc, del, msg)
c
c         parse the command string to get command option and filename
c   INPUT:  comstr: command string to be parse
c			knc: command string length
c
c   OUTPUT :   mfil: filename
c				ncf: filename length
c				del: if delete option is parsed
c				msg: error message if err
c   RETURN:    0: success
c			1: error
c
**********************************************************************/
ncq_getopt(comstr, knc, optstr, mfil, ncf, del, msg)
char *comstr, *mfil, *optstr, *msg;
int knc, *ncf, *del;
{
	int i, nopt, iopt, nco[MAXOPT], nc, iret, inc, ifil;
	char *p,*pinc,*opt[MAXOPT], *strchr();

	iret = 0;
	comstr[knc] = '\0';
/*
.....get default option
*/
	ncq_getlnbuf(optstr);
/*
.....Break out filename & options
*/
	*ncf = 0;
	*del = 0;
	nopt = 0;
	iopt = 1;
	ifil = 0;
	p = comstr;
	i = 0;
	do
	{
		pinc = strchr(p,' ');
		if (pinc == 0)
		{
			nco[nopt] = strlen(p);
			opt[nopt] = (char *)malloc((nco[nopt]+1)*sizeof(char));
			strcpy(opt[nopt],p);
			nopt++;
			i = knc + 1;
		}
		else
		{
			nco[nopt] = pinc - p;
			opt[nopt] = (char *)malloc((nco[nopt]+1)*sizeof(char));
			strncpy(opt[nopt],p,nco[nopt]);
			opt[nopt][nco[nopt]] = '\0';
			p = p + nco[nopt];
			nopt++;
			do {p++;} while (*p == ' ');
		}
/*
.....Filename has spaces
.....concatenate strings
*/
		if (opt[nopt-1][0] == '-') iopt = 1;
		else if (iopt == 0)
		{
			nc = nco[nopt-1] + nco[nopt-2] + 2;
			pinc = (char *)malloc(nc*sizeof(char));
			strcpy(pinc,opt[nopt-2]);
			strcat(pinc," ");
			strcat(pinc,opt[nopt-1]);
			free(opt[nopt-2]); free(opt[nopt-1]);
			opt[nopt-2] = pinc;
			nco[nopt-2] = nc - 1;
			iopt = 0;
			nopt--;
		}
		else
			iopt = 0;
		if (nopt== MAXOPT) break;
	} while (i < knc);
/*
.....Loop through parameters
*/
	for (i=0;i<nopt;i++)
	{
/*
........Option
*/
		if (opt[i][0] == '-')
		{
			ncq_to_upper(opt[i]);
/*
...........Delete
*/
			if (strcmp(opt[i],"-DE") == 0)
			{
				*del = 1;
			}
/*
...........Clfile
*/
			if (strcmp(opt[i],"-NOCL") == 0)
			{
				optstr[0] = '0';
			}
			else if (strcmp(opt[i],"-CL") == 0)
			{
				optstr[0] = '1';
			}
/*
...........APT Source File
*/
			else if (strcmp(opt[i],"-NOAS") == 0)
			{
				optstr[1] = '0';
			}
			else if (strcmp(opt[i],"-AS") == 0)
			{
				optstr[1] = '1';
			}
/*
...........Short Print File
*/
			else if (strcmp(opt[i],"-NOLI") == 0)
			{
				optstr[2] = '0';
			}
			else if (strcmp(opt[i],"-LI") == 0)
			{
				optstr[2] = '1';
			}
/*
...........Update Part Program
*/
			else if (strcmp(opt[i],"-NOUP") == 0)
			{
				optstr[3] = '0';
			}
			else if (strcmp(opt[i],"-UP") == 0)
			{
				optstr[3] = '1';
			}
/*
...........Create Print File
*/
			else if (strcmp(opt[i],"-NOOP") == 0)
			{
				optstr[4] = '0';
			}
			else if (strcmp(opt[i],"-OP") == 0)
			{
				optstr[4] = '1';
			}
/*
...........Post-Process
*/
			else if (strcmp(opt[i],"-NOPP") == 0)
			{
				optstr[5] = '0';
			}
			else if (strcmp(opt[i],"-PP") == 0)
			{
				optstr[5] = '1';
			}
/*
...........Priority
*/
			else if (strncmp(opt[i],"-PR", 3) == 0)
			{
				if (opt[i][3] != ':' || !isalpha(opt[i][4])) goto bad_param;
				optstr[6] = opt[i][4];
			}
/*
...........Number of lines per page
*/
			else if (strncmp(opt[i],"-NL",3) == 0)
			{
				if (opt[i][3] != ':') goto bad_param;
				inc = atoi(&opt[i][4]);
				if (inc < 0 || inc > 999) goto bad_param;
				sprintf(&optstr[7],"%03d",inc);
			}
			else if (strcmp(opt[i],"-MO") == 0)
				NCQ_monitor = 1;
			else if (strcmp(opt[i],"-NOMO") == 0)
				NCQ_monitor = 0;
			else if (strncmp(opt[i], "-TIME", 5) == 0)
			{
				if (opt[i][5] != ':') goto bad_param;
				inc = atoi(&opt[i][6]);
				if (inc==0)
					NCQ_time_limit = 0;
				else if (inc==15)
					NCQ_time_limit = 1;
				else if (inc==30)
					NCQ_time_limit = 2;
				else if (inc==60)
					NCQ_time_limit = 3;
				else if (inc==120)
					NCQ_time_limit = 4;
			}
/*
...........Unrecognized option
*/
			else
				goto bad_param;
/*
...........Filename
*/
		}
		else
		{
			if (ifil == 1) goto bad_param;
			strcpy(mfil,opt[i]);
			*ncf = nco[i];
			ifil = 1;
		}
	}
	goto done;
/*
.....Invalid NCQ option
*/
bad_param:;
	sprintf(msg,"Invalid option for ncq: %s",opt[i]);
	iret = 1;
/*
.....End of routine
*/
done:;
	for (i=0;i<nopt;i++) free(opt[i]);
	return(iret);
}

/***********************************************************************
c
c   FUNCTION: ncq_loadque_file (filen)
c
c         open the queue file and load into current queue
c   INPUT:  filen: command string to be parse
c
c   OUTPUT :   none
c   RETURN:    none
c
**********************************************************************/
void ncq_loadque_file (filen)
char *filen;
{
	FILE *fptr ;
	char msg[256],questr[MAX_PATH+20];

	fptr = fopen(filen,"r");
	if (fptr == 0) 
	{
		sprintf(msg, "Could not open %s", filen);
		ncq_dispmsg(msg);
		return;
	}
/*
.....delete the current list first
*/
	ncq_reset_list();

	ncq_fptr = fopen(ncq_file,"w");
	if (ncq_fptr == 0) 
	{
		ncq_dispmsg("Could not open ncl.que to rewrite");
		return;
	}
/*
.....read queue in filen and write into ncq_file
*/
	while (fgets(questr, 256, fptr))
	{
		fprintf(ncq_fptr, "%s", questr);
/*
......Redisplay the queue
*/
		if (UU_BATCH == 0)
		{
			ncq_additem (questr);
		}
	}
	if (fptr!=NULL)
		fclose(fptr);
	ncq_sel_lastpos();
	ncq_clsque();
}

/***********************************************************************
c
c   FUNCTION: ncq_saveopt_ini ()
c
c         save the option into ncq.ini
c   INPUT:  filen: command string to be parse
c
c   OUTPUT :   none
c   RETURN:    none
c
**********************************************************************/
void ncq_saveopt_ini (optstr)
char *optstr;
{
	char buf[256], tempstr[20];
	int i =0;
	FILE *fptr = fopen(ncq_initfile,"w");
	if (fptr == 0) 
	{
		ncq_dispmsg("Could not open ncq.ini");
		return;
	}
	buf[0] = '\0';
/*
...........Clfile
*/
	if (optstr[0] == '0')
	{
		strcat (buf, "-NOCL ");
	}
	else if (optstr[0] == '1')
	{
		strcat (buf, "-CL ");
	}
/*
...........APT Source File
*/
	if (optstr[1] == '0')
	{
		strcat (buf, "-NOAS ");
	}
	else if (optstr[1] == '1')
	{
		strcat (buf, "-AS ");
	}
/*
...........Short Print File
*/
	if (optstr[2] == '0')
	{
		strcat (buf, "-NOLI ");
	}
	else if (optstr[2] == '1')
	{
		strcat (buf, "-LI ");
	}
/*
...........Update Part Program
*/
	if (optstr[3] == '0')
	{
		strcat (buf, "-NOUP ");
	}
	else if (optstr[3] == '1')
	{
		strcat (buf, "-UP ");
	}
/*
...........Create Print File
*/
	if (optstr[4] == '0')
	{
		strcat (buf, "-NOOP ");
	}
	else if (optstr[4] == '1')
	{
		strcat (buf, "-OP ");
	}
/*
...........Post-Process
*/
	if (optstr[5] == '0')
	{
		strcat (buf, "-NOPP ");
	}
	else if (optstr[5] == '1')
	{
		strcat (buf, "-PP ");
	}
	if (NCQ_monitor)
		strcat (buf, "-MO ");
	else
		strcat (buf, "-NOMO ");
/*
...........Priority
*/
	strcat(buf, "-PR:");
	tempstr[0] = optstr[6];
	tempstr[1] = ' ';
	tempstr[2] = '\0';
	strcat(buf, tempstr);
	strcat(buf, "-NL:");
	strncpy(tempstr, (&optstr[7]), 3);
	tempstr[3] = ' ';
	tempstr[4] = '\0';
	strcat(buf, tempstr);
	fprintf(fptr, "%s\n", buf);
/*
...........Filter Specifications
*/
	buf[0] = '\0';
	while(i<20)
	{
		if (NCQ_file_ext[i][0]=='\0')
			break;
		strcpy(buf, NCQ_file_ext[i]);
		strcat(buf, "; ");
		strcat(buf, NCQ_file_desp[i]);
		tempstr[0] = '\0';
		strcat(buf, tempstr);
		fprintf(fptr, "%s\n", buf);
		i++;
	}
	fclose (fptr);
}
/**********************************************************************
**    I_FUNCTION :  ncq_getpost_msg(ppfile, msg1, msg2)
**       Get Pworks post running message from pworks.log file
**       
**    PARAMETERS
**       INPUT  :
**          ppfile: running pp file
**       OUTPUT :
**          msg1, msg2: pp file post running message
**						it could be one line msg1 (completed line statement)
**						or two lines include a error, warning lines (msg2)
**    RETURNS      : none
**		
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_getpost_msg(ppfile, msg1, msg2)
char *ppfile, *msg1, *msg2;
{
	FILE *fp;
	char *indx, linestr[256], tempstr[MAX_PATH], fullname[MAX_PATH];
	char fname[MAX_PATH], fname2[MAX_PATH], dir[MAX_PATH], logfile[MAX_PATH];
	char ext[80],*strstr();
	int flen, knc, flen1, flen2, flen22;
	char part11[MAX_PATH], part12[MAX_PATH], part21[MAX_PATH], part22[MAX_PATH];
	void ncq_break_fname();
/*
.....don't change ppfile, it should be fixed
*/
	strcpy(fullname, ppfile);
	indx = (char*) strrchr(fullname, '.');
	if (indx!=NULL)
		*indx = '\0';
	strcpy(fname2, fullname);
/*
.....break directory from filename because UNIX only save the filename
.....without directory and also we need directory to open pworks.log file
*/
	ncq_break_fname(fullname,dir,fname,ext);
/*
.....but for WINNT, the pworks.log save the whole path of pp file
*/
#if (UU_COMP == UU_WIN2K)
	strcpy(fname, fname2);
#endif
	flen = strlen(fname);
	msg1[0] = '\0';
	msg2[0] = '\0';
/*
.....open pworks.log in the directory the PP file is in, so it should be
.....directory from ppfile 
*/
	strcpy(logfile, dir);
   if (dir[0] != '\0') strcat(logfile, delim);
	strcat(logfile, "pworks.log");
	if (((fp = fopen(logfile, "r") ) == NULL))
	{
		return;
	}

	while (fgets(linestr, 256, fp))
	{
start:;
		if (strncmp(linestr, "Pworks - ", 9)!=0)
			continue;
		strcpy(tempstr, &(linestr[9]));
/*
.....we need consider shorten filename because the saved filename (from pworks.log)
.....could be a shorten filename (40chars)
*/
		if ((indx = strstr (tempstr, "{...}"))!=NULL)
		{
			*indx = '\0';
			strcpy (part12, indx+5);
			indx = (char*) strrchr(part12, '.');
			if (indx!=NULL)
				*indx = '\0';
			strcpy (part11, tempstr);
		}
		else
			ncq_break_fname(tempstr,part11,part12,ext);

		flen1 = strlen (part11);
		flen2 = strlen (part12);

		ncq_break_fname(fname,part21,part22,ext);
/*
.....compare the first part (directory)
*/
		if (strncmp(part11, part21, flen1)!=0)
			continue;
/*
.....compare the second part, shorten filename from the end
*/
		flen22 = strlen (part22);
		if (flen<flen2) continue;
/*
.....it could be negtive and cause trouble
*/
//		if (strncmp(part12, &(part22[flen22-flen2]), flen2)!=0)
		if (flen22-flen2>=0)
		{
			if (strncmp(part12, &(part22[flen22-flen2]), flen2)!=0)
				continue;
		}
		else if (flen22-flen2<0)
		{
			if (strncmp(part22, &(part12[flen2-flen22]), flen2)!=0)
				continue;
		}
/*
......if match the filename, then copy this line as msg1
......and check if there are following warn/error lines
*/
        msg1[0] = '\0';
		msg2[0] = '\0';
		strcpy(msg1, linestr); 		
        while (fgets(linestr, 256, fp))
        {   
            if (strncmp(linestr, "Pworks - ", 9)==0)
                goto start;
            strcat(msg2,linestr);
            strcat(msg2,"\r\n");
        }
/*
......we have to continue because we need get the last matched line
......the pworks.log will contains all files ran which including
......the older one (could be same name)
*/
		continue;
	}
		
	knc = strlen (msg1);
	if (knc!=0)
	{
		while ((msg1[knc-1]=='\n') ||
					(msg1[knc-1]=='\r'))  knc--;
		msg1[knc] = '\0';
	} 
		    
	knc = strlen (msg2);
	if (knc!=0)
	{
		while ((msg2[knc-1]=='\n') ||
					(msg2[knc-1]=='\r'))  knc--;
		msg2[knc] = '\0';
	}

	fclose(fp);
}


/**********************************************************************
**    I_FUNCTION :  ncl_que_is_empty()
**       Check if ncl.que is empty now
**       
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : 1: empty
**							0: not empty
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_que_is_empty()
{
	struct stat fileInfo;
	int ret;
	ret = stat(ncq_file, &fileInfo);
	if (ret!=0)
	{
		ncq_dispmsg("Could not get ncl.que file info");
		return 1;
	}
	if (fileInfo.st_size==0)
		return 1;
	return 0;
}
	
/*******************************************************************
**   E_FUNCTION : ncq_get_fname(fullname,fname)
**              This function get filename without path
**   PARAMETERS  
**       INPUT  :  fullname  = full filename specification.
**       OUTPUT :  
**		   fname = filename specification of 'fullname'.
**		     Blank if no filename is specified.
**   RETURNS:    none.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
void ncq_get_fname(fullname,fname)
char *fullname,*fname;
{
   char *pointer;
	char buf[200];

	strcpy (buf,fullname);

#if UU_COMP == UU_VAXVMS

   pointer = (char*) strrchr (buf,']');
	if (pointer != 0)
	{
		pointer++;
		strcpy (fname,pointer);
		*pointer = '\0';
	}
	else
	{
		pointer = (char*) strrchr (buf,':');
		if (pointer != 0)
		{
			pointer++;
			strcpy (fname,pointer);
			*pointer = '\0';
		}
		else
		{
			strcpy (fname,buf);
		}
	}
#else
	if (buf[0] == '\0')
	{
		strcpy(fname,buf);
	}
	else
	{
		pointer = (char*) strrchr (buf,delim[0]);
		if (pointer != 0)
		{
			pointer++;
			strcpy (fname,pointer);
			pointer--;
			*pointer = '\0';
		}
		else
		{
			strcpy (fname,buf);
		}
	}
#endif
}
/*******************************************************************
**   E_FUNCTION : ncq_break_fname(fullname,dir,fname,ext)
**              Breaks a full filename specfication into its
**              subcomponents (directory, filename, file extension).
**   PARAMETERS  
**       INPUT  :  fullname  = full filename specification.
**       OUTPUT :  dir       = Directory.
**                 fname     = Base filename.
**                 ext       = File extension.
**   RETURNS:    none.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
void ncq_break_fname(fullname,dir,fname,ext)
char *fullname,*dir,*fname,*ext;
{
	char *p, *strchr();
/*
.....Get the directory
*/
	strcpy(dir,fullname);
	p = (char*) strrchr (dir,delim[0]);
	if (p != 0) *p = '\0';
	else dir[0] = '\0';
/*
.....Get the filename
*/
	ncq_get_fname(fullname,fname);
/*
.....Break out the file base name and extension
*/
	p = strchr(fname,'.');
	if (p == 0)
		ext[0] = '\0';
	else
	{
		strcpy(ext,p+1);
		*p = '\0';
	}
}

/*******************************************************************
**   E_FUNCTION : ncq_build_fname(fullname,dir,fname,ext)
**              Breaks a full filename specfication into its
**              subcomponents (directory, filename, file extension).
**   PARAMETERS  
**       INPUT  :  fullname  = full filename specification.
**       OUTPUT :  dir       = Directory.
**                 fname     = Base filename.
**                 ext       = File extension.
**   RETURNS:    none.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
void ncq_build_fname(fullname,dir,fname,ext)
char *fullname,*dir,*fname,*ext;
{
/*
.....Append directory
*/
	ncq_get_dir(dir,fullname);
	if (dir[0] != '\0') strcat(fullname,delim);
/*
.....Append filename
*/
	if (fname[0] != '\0') strcat(fullname,fname);
/*
.....Append file extension
*/
	if (ext[0] != '\0')
	{
		strcat(fullname,".");
		strcat(fullname,ext);
	}
}

/*******************************************************************
**   E_FUNCTION : ncq_get_dir(dir,fullname)
**          This function takes the symbol 'dir' and returns the
**          full directory specification in 'fullname'.  'dir' can
**          be  a enviroment variable.
**   PARAMETERS
**       INPUT  :  dir  = directory symbol.
**       OUTPUT :  fullname = full directory specification of 'dir'.
**   RETURNS:    0: can't get valid directory
**				1: get valid durectory
**       otherwise.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
int ncq_get_dir(dir,fullname)
char *dir,*fullname;
{
	char *p, *q,*getenv(), *strchr();
	char tmp[MAX_PATH], dir1[MAX_PATH];
	int ret, done = 0, first = 1;
/*
.....to extend all path
*/
	if (dir[0] == '\0')
	{
		fullname[0] = '\0';
		return 0;
	}
	strcpy(dir1, dir);
get_dir:;
/*
.....Get directory
*/
	strcpy(tmp,dir1);
	p = (char*)strchr(tmp,delim[0]);
	if (p != 0 && p != tmp) 
	{
		*p = '\0';
		strcpy(dir1, p+1);
		if (*(p+1)=='\0')
			done = 1;
	}
	else
		done = 1;
	if (strcmp(tmp,".") == 0)
	{
		ncq_getwd(tmp);
		q = tmp;
	}
	else
	{
		q = getenv(tmp);
		if (q==0)
			q = tmp;
	}
	if (q != 0)
	{
		if (first)
		{
			strcpy(fullname, q);
			first = 0;
		}
		else
		{
			strcat (fullname, delim);
			strcat (fullname, q);
		}
	}
	if (ncq_is_dir(fullname))
	{
		if (done)
			return 1;
		else
			goto get_dir;
	}
	else if (done)
		return 0;
	else
	{
		ret = ncq_get_dir(fullname, fullname);
		if (ret==0)
			return 0;
		goto get_dir;
	}
}
/*********************************************************************
**    E_FUNCTION : ncq_is_dir(pathname)
**		Check if "pathname" is a valid directory
**    PARAMETERS   
**       INPUT  : 
**    RETURNS: 1: yes.
**				0: No
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ncq_is_dir(pathname)
char *pathname;
{
#if UU_COMP==UU_WIN2K
	int len;
#endif
	char noquote[MAX_PATH];
	struct stat stbuf;	

	strcpy(noquote,pathname);

#if UU_COMP == UU_VAXVMS
	char *right,dirname[MAX_PATH];
	if ((right = strrchr(noquote,']')) != UU_NULL)
		if ( *(right+1) == '\0')
		{
			if (ux_vaxdir0(noquote,dirname) != UU_SUCCESS)
				goto failed;
			strcpy(noquote,dirname);
		}
#endif

#if UU_COMP==UU_WIN2K
	len = strlen(noquote);
	if (noquote[len-1]=='\\')
		noquote[len-1] = '\0';
	len = strlen(noquote);
	if (noquote[len-1]==':')
		strcat(noquote, "\\");
#endif
	if  (stat(noquote, &stbuf) != 0)
		return 0;						
	if ((stbuf.st_mode & S_IFMT) == S_IFDIR)
		return 1;								
	else
		return 0;					
}	

/*********************************************************************
**    E_FUNCTION : ux_vaxdir0(filename,dirspec)
**		Routine to convert a vax file spec format to .DIR type spec,
**		since many lib functions work only on this form 
**			Copied from NCL function
**    PARAMETERS   
**       INPUT  : 
**    RETURNS: UU_SUCCESS if no problems encountered, UX_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ux_vaxdir0(filename,dirspec)
	char *filename;
	char *dirspec;
{
	int status;
	char  *rindex();

	status = 0;
	strcpy(dirspec,filename);
#if UU_COMP == UU_VAXVMS
	matchb = rindex(dirspec,'[');
	if (matchb == UU_NULL)
		goto failed;
	endb = rindex(matchb,']');
	if (endb == UU_NULL)
		goto failed;

	if ( (*(endb+1)!='\0') && ( *(endb+1)!='"') )
		goto done;

	lastd = rindex(dirspec,'.');
	if (lastd == UU_NULL)
	{	
		*endb = '\0';
		if ( *(endb+1) == '"')
		{
			strcpy(tempname,"\"[-]");
			strcat(tempname,matchb+1);
			strcat(tempname,".DIR\"");
		}
		else
		{
			strcpy(tempname,"[-]");
			strcat(tempname,matchb+1);
			strcat(tempname,".DIR");
		}
		strcpy(dirspec,tempname);
	}
	else
	{
		*lastd = ']';
		*endb = '\0';
		strcat(dirspec, ".DIR");
		if (*(endb+1) == '"')
			strcat(dirspec,"\"");
	}
#endif
	goto done;
failed: status = -1;
done:;
	return(status);
}	
/**********************************************************************
**    I_FUNCTION :  ncq_getwd (dir)
**			Returns the current default directory.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          dir         - current default directory.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncq_getwd (cdir)
char *cdir;
{
#if UU_COMP == UU_VAXVMS
	static UX_pathname dirp;
	static $DESCRIPTOR(dirp_desc,dirp);
	int len;

	sys$setddir(0,&len,&dirp_desc);
	strcpy(cdir,dirp);
#else
#if UU_COMP == UU_WIN2K
	getcwd(cdir, MAX_PATH);
#else
	getwd(cdir);
#endif
#endif
	return(0);
}

