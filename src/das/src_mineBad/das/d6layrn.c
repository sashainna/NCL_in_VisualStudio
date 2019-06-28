/*********************************************************************
**    NAME         :  d6layrn.c -- layout run-time routines.
**       CONTAINS:
**			ud_getuims() -- read layout file from M_UD_LAYOUT shell variable,
**									generate undetectable segments.
**			ud_uims() -- old entry into ud_getuims
**			ud_actuims() -- ask user for layout filename, then read it.
**       ud_readuims(duims,curlayout,fname) -- read a specified layout file, 
**									generate undetectable segments.
**       ud_rduims(duims,fname) -- read a specified layout file.
**       ud_rduimss(duims,fname,s) -- read and scale a specified layout file.
**    	ud_initchcarea(duims,j) -- init choice dev for jth UD_CHC area.
**    	ud_initchcarea2(type,duims,j) -- init choice dev for jth 'type' area.
**    	ud_initcuruims(curlayout) -- initialize current uims values.
**			ud_setarea(duims,curlayout,typ,n,vis,multi) -- turn a layout area 
**									ON or OFF.
**			ud_setuims(duims,curlayout,n) -- turn the nth areas ON, the rest OFF.
**			ud_setuims2(duims,curlayout,n) -- turn the nth areas ON, the rest OFF.
**			ud_setuims3(duims,curlayout,n) -- turn the nth areas ON, the rest OFF.
**    	ud_genareaseg(areapt) -- gen an undetectable segment for area.
**    	ud_genareasegdet(areapt,det) -- gen a seg for an area.
**    	ud_genareaseg2(segno,areapt) -- put fillarea, border in segno.
**			ud_deluims(duims) -- delete a uims.
**    	ud_gensegs(uimsdef,det) -- generate segs for a layout.
**       ud_geticdata(n)				--- read icon data from archive file
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d6layrn.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:13
*********************************************************************/
#include "udebug.h"
#include "usysdef.h"
#include "usysg.h"
#define UIMSPGM
#include "uims.h"
#undef UIMSPGM
#include "zsysdep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "gobas.h"
#include "dasnog.h"
#include "ginqxf.h"
#include "ginqatt3.h"
#include "ginqst.h"
#include "unserve.h"
/*#include "umath.h"*/
#include "usysg.h"
#include "gichoice.h"
#include "gsegdet.h"
#include "gtblvar4.h"
#include "driver.h"

#ifdef UU_DEBUGON
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) d6layrn.c 2.55 10/17/86 08:02:04 single"};
#else
static char uu_sccsident[]={"@(#) d6layrn.c 2.55 10/17/86 08:02:04 double"};
#endif
#endif

#define ON 1
#define OFF 0
#define MASK 0644			/* File permission mask	*/
#define UD_LAYPRI (UU_REAL) 1.0	/* Layout segment proiority	*/

/* THIS MUST BE REMOVED TO COMPILE DAS W/O LAY ED> NEEDS TO BE PAST
	AS AN ARGUMENT TO RDUIMSS														*/
UD_CURLAYOUT UD_newcur;			/* current areas of UD_newuims */
UD_UIMS UD_newuims;				/* user interface we are editing */
										/* This is part of the layout editor and should
											NOT be here. The real fix is to pass it as
											a parm to any routine needing it				*/
char *ux_getenv();
void ud_readuims(UD_UIMS *duims, UD_CURLAYOUT *curlayout, char fname[]);
void ud_rduims(UD_UIMS *duims, char fname[]);
void ud_rduimss(UD_UIMS *duims, char fnam[], UU_REAL s);
void ud_initchcarea2(int type, UD_DUIMSDEF *duims, int j, Gchoicerec *choicerec);
void ud_initcuruims(UD_CURLAYOUT *curlayout);
void ud_setuims(UD_UIMS *duims, UD_CURLAYOUT *curlayout, int n);
void ud_setuims2(UD_UIMS *duims, UD_CURLAYOUT *curlayout, int n);
void ud_genareasegdet();
void ud_genareaseg2(int segno, UD_AREA *areapt);
void ud_gensegs();
void ud_deluims(UD_UIMS *duims);

int *raster[UD_MAXAREAS][UD_MAXICONS];             /* pointers to icon rasters */
Gipoint rassiz[UD_MAXAREAS][UD_MAXICONS];       /* size of icon rasters */

/*
static char sdet[2][16]={"UG_UNDETECTABLE","DETECTABLE"};	for debugging
static char typstr[UD_NTYPES][10]={"UD_GRAF","UD_SPRMT","UD_LPRMT",
		"UD_ERR","UD_HLP","UD_CHC","UD_ICON","UD_SCROLL","UD_FORMS",
		"UD_MENU","UD_ICONM"};			 for debug printout 
*/
static int gotauims=0;				/* 1 = got a uims in UD_duimsdeflt */

/*MILLS: for all platforms, store highest segment number for
		 screen layout segments for Unibase resets.
*/
extern int NCL_MAXWINSEGNO;
extern int UU_comargc;
extern char *UU_comargv[COMMAX];
/*********************************************************************
**    E_FUNCTION :  ud_getuims() -- read uims from layout file.
**       Read information from layout file into UD_duimsdeflt, initialize
**			choice devices for UD_CHOICE icons. Gen segments.
**    PARAMETERS   
**       INPUT  :  non
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_getuims()
{
	char filename[UX_MAX_PATH_LEN], fullname[UX_MAX_PATH_LEN];
	int status;
	char *pathptr;
	UU_LOGICAL fptr;
	int i;
	char msg[UX_MAX_PATH_LEN+40];
/*
.....Start of code
*/
	uu_denter(UU_DTRC,(us,"ud_getuims(). gotauims=%d"));
	if (gotauims==1) ud_deluims(&UD_duimsdeflt);
	pathptr = UU_NULL;
	filename[0] = '\0';
/*
...get layout file from command line
...Yurong
*/
	if(UU_comargc>1)
	{
		for(i=0; i<UU_comargc-1; i++)
		{
/*
...get file name 
...Yurong
*/
			if((strstr(UU_comargv[i], "-l=")!=0) 
						||(strstr(UU_comargv[i], "-L=")!=0))
		   	strcpy(filename, &(UU_comargv[i][3]));
		}
	}
/*
...if get layout file from command line, use it
...otherwise, use default
*/
	if(filename[0]=='\0')
	{
		status = ux_get_syspath("M_UD_LAYOUT",&pathptr,filename,&fptr,UX_PRTERRS);
   		ux_strip_quotes(filename);
	}
	if(filename[0]=='\0')
	{
  		ud_printmsg("Mandatory symbol M_UD_LAYOUT not defined\n");
  		sleep(1);
  		uu_abort();			/* Exit	*/
  	}
/*
.....Determine if layout file contains a
.....directory specification.
.....It it does not, then add the default spec
*/
	strcpy(fullname,filename);
	status = ul_open_mod_file("UU_USER_SETTINGS","layout", "M_UD_LAYOUT_DIR", 
		UU_NULL,fullname, 0, UU_NULL);
	if (status!=0)
   {
  		sprintf(msg, "Layout file '%s' could not be opened.\n", fullname);
  		ud_printmsg(msg);
		exit(1);
  	}
	ud_readuims(&UD_duimsdeflt,&UD_curlayout,fullname);
	gotauims=1;		/* remember we got a uims in UD_duimsdeflt*/
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ud_uims(newuims)
**			set up user interface for unicad system.
**			This is for historical compatibility. Should call ud_getuims
**			directly.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_uims()
{
	uu_denter(UU_DTRC,(us,"ud_uims()"));
	ud_getuims();
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ud_actuims() -- read uims from layout file.
**			Ask user for filename. Then
**       read information from layout file into UD_duimsdeflt, initialize
**			choice devices for UD_CHOICE icons. Gen segments.
**    PARAMETERS   
**       INPUT  :  non
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_actuims()
{
	char fname[60];
	int numint;
	uu_denter(UU_DTRC,(us,"ud_actuims()"));
		ud_ddas(UD_DASSTRING, "Enter layout filename ",fname, 60,
			&numint, UD_NODEFAULT);
	if (gotauims==1) ud_deluims(&UD_duimsdeflt);		/* delete old uims */
	ud_readuims(&UD_duimsdeflt,&UD_curlayout,fname);
	gotauims=1;							/* remember we got a uims in UD_duimsdeflt*/
	ud_setuims(&UD_duimsdeflt,&UD_curlayout,0);	/* set ON 1st screen*/
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ud_readuims(duims,curlayout,fname) -- read uims from 
**			layout file.  Read information from layout file fname into duims, 
**			initialize choice devices for UD_CHOICE icons. Gen segments.
**    PARAMETERS   
**       INPUT  :  char fname[] -- layout filename.
**       OUTPUT :  UD_UIMS duims -- uims structure to read into.
**						 UD_CURLAYOUT *curlayout -- current areas in use.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_readuims(duims,curlayout,fname)
char fname[];					/* name of layout file */
UD_UIMS *duims;				/* uims to read into */
UD_CURLAYOUT *curlayout;
{
	int scrn;
	uu_denter(UU_DTRC,(us,"ud_readuims(%x,%x,%s)",duims,curlayout,fname));
/*
.....Read layout file
*/
	udm_read_layout(duims,fname);
	/* generate undetectable segments for most types of areas on all screens */
	for (scrn=0; scrn<(*duims).nscreens; scrn++) {
		ud_gensegs(&(*duims).screen[scrn],(Gsegdet)UG_UNDETECTABLE);		
	}
	ud_initcuruims(curlayout);			/* set current layout values to 
													initial values */
	/* set erase color to background color of 1st UD_GRAF area in 1st screen*/
#if UU_COMP!=UU_WIN2K
	gsegerasecolor(((*duims).screen[0].areas[UD_GRAF])[0].color);
#endif
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ud_rduims(duims,fname) -- read a uims file.
**    PARAMETERS   
**       INPUT  : 	char *fname -- uims filename.
**       OUTPUT :  	UD_UIMS duims -- uims struct to read into
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_rduims(duims,fname)				/* read a uims file */
char fname[];							/* uims filename to read */
UD_UIMS *duims;						/* uims struct to read into */
{
	ud_rduimss(duims,fname,(UU_REAL) 1.);	/* read in with unity scale factor */
}

/*********************************************************************
**    E_FUNCTION :  ud_rduimss(duims,fname,s) -- read, scale a uims file.
**    PARAMETERS   
**       INPUT  : 	char *fname -- uims filename.
**							UU_REAL s -- scale factor.
**       OUTPUT :  	UD_UIMS duims -- uims struct to read into
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		The format of the iconfile info at the end of the archive file is:
**		number of icons -- 1 integer word (4 bytes).
**		int rows,cols					(one integer each	)
**		for each icon {
**			lower left and upper right corners of this icon -- 2 floats.
**		}
**		for each icon {
**			length of icon filename (not counting the null terminator)-- 1 word.
**		null terminated icon filename. -- n+1 bytes.
**		}
*********************************************************************/

void ud_rduimss(duims,fname,s)			/* read and scale a uims file */
char fname[];							/* uims filename to read */
UD_UIMS *duims;						/* uims struct to read into */
UU_REAL s;									/* scale factor */
{
	int type;					/* type of area */
	int noareas;				/* number of areas of this type */
	int scrn;
	int i,j;
	int ainit;
	int segnos[20];			/* segment numbers on archive file */
	int segno;
	char filename[100];		/* complete icon archive filename */
  static UU_REAL modident[4][4]={1.,0.,0.,0.,
                                 0.,1.,0.,0.,
                                 0.,0.,1.,0.,
                                 0.,0.,0.,1.};
	Gchoicerec choicerec;
	FILE *fd;
	UD_AREA *areapt;			/* pointer to an array of areas */
	Gdrect *windpt;			/* pointer to wswind for a screen */
	int maxchoice;
   int arclen;
	int rows,cols,k,len;
	char devfname[100];
	char tmpnm[100],tmpnm1[10];
	int dev_spec;					/* device dependant file flag,1 if file exists	*/
	int ddfd;					/* dev dependant file descriptor	*/
	char **iconfile;
	Gipoint ras,ras1;
	Girect *iposn;
	Gdrect position;
	int graphics_flag,daslen,total_len,choices;
#if UU_COMP==UU_IRIS 
#ifdef UU_DOUBLE
	long float tmp1,tmp2,tmp3,tmp4;
#else
	float tmp1,tmp2,tmp3,tmp4;
#endif /* UU_DOUBLE */
#endif /* UU_IRIS */
#if UU_COMP != UU_IRIS
	float tmp1,tmp2,tmp3,tmp4;
#endif
	char sympath[UX_MAX_PATH_LEN], msg[UX_MAX_PATH_LEN+40];
	
	uu_denter(UU_DTRC,(us,"ud_rduimss(%x,%s)",duims,fname));
	fd=fopen(fname,"r");
	if (fd==NULL) {
		sprintf(msg, "Can't read layout file %s: Exiting\n", fname);
		ud_printmsg(msg);
		uu_dprint(UU_DTRC,(us,"ud_rduimss cant open file %s",fname));
		uu_abort();
	}
	else {												/* file opened */
		fscanf(fd,"%d",&(*duims).nscreens);
		uu_dprint(UU_DTRC,(us,"ud_rduimss nscreens=%d",(*duims).nscreens));

		/* Get extension for fast icon file names	*/
		strcpy(tmpnm1,ug_gksstli.wsopen[*UD_ksws].wdtptr->type);
		tmpnm1[3] = '\0';

		for (scrn=0; scrn<(*duims).nscreens; scrn++) {	/* for each screen*/
			ainit=0;
			windpt= &(*duims).screen[scrn].wswind;
			fscanf(fd,"%e%e%e%e",&tmp1,&tmp2,&tmp3,&tmp4);
			(*windpt).ll.x = tmp1;
			(*windpt).ll.y = tmp2;
			(*windpt).ur.x = tmp3;
			(*windpt).ur.y = tmp4;
				(*windpt).ll.x=(*windpt).ll.x*s;
				(*windpt).ll.y=(*windpt).ll.y*s;
				(*windpt).ur.x=(*windpt).ur.x*s;
				(*windpt).ur.y=(*windpt).ur.y*s;
  				gswswindow(UD_ksws,&(*duims).screen[scrn].wswind);
			uu_dprint(UU_DTRC,(us,"ud_rduimss. scaled wswind[%d]=%g %g %g %g",
				scrn,(*windpt).ll.x,(*windpt).ll.y,(*windpt).ur.x,
				(*windpt).ur.y));
			for (type=0; type<UD_NTYPES; type++) {	/* for each type of area */
				fscanf(fd,"%d",&noareas);
				uu_dprint(UU_DTRC,(us,"ud_rduimss. noareas[%d]=%d",type,noareas));
				(*duims).screen[scrn].noareas[type]=noareas;
				if(noareas > 0)
					{
					areapt=(UD_AREA *)uu_toolmalloc(sizeof(UD_AREA)*noareas);
					(*duims).screen[scrn].areas[type]=areapt;
					}
				else
					(*duims).screen[scrn].areas[type]=NULL;
				uu_dprint(UU_DTRC,(us,"ud_rduimss. screen[%d].areas[%d]=%x",
						scrn,type,areapt));
				for (j=0; j<(*duims).screen[scrn].noareas[type] ; j++) 
					{			/* for each area of this type*/
					fscanf(fd,"%d%d%d",
						&areapt[j].color,&areapt[j].bordercolor,&areapt[j].contcolor);
			fscanf(fd,"%e%e%e%e",&tmp1,&tmp2,&tmp3,&tmp4);
			areapt[j].posn.ll.x = tmp1;
			areapt[j].posn.ll.y = tmp2;
			areapt[j].posn.ur.x = tmp3;
			areapt[j].posn.ur.y = tmp4;
					areapt[j].posn.ll.x=areapt[j].posn.ll.x*s;
					areapt[j].posn.ll.y=areapt[j].posn.ll.y*s;
					areapt[j].posn.ur.x=areapt[j].posn.ur.x*s;
					areapt[j].posn.ur.y=areapt[j].posn.ur.y*s;
					uu_dprint(UU_DTRC,(us,"ud_rduimss. colors=%d %d %d",
						areapt[j].color,areapt[j].bordercolor,areapt[j].contcolor));
					uu_dprint(UU_DTRC,(us,"ud_rduimss. scaled posn=%g %g %g %g",
						areapt[j].posn.ll.x, areapt[j].posn.ll.y,
						areapt[j].posn.ur.x, areapt[j].posn.ur.y));
					switch (type) {			/* read contents, dependent upon type */
					case UD_CHC:
					case UD_ICONM:
					case UD_ICON:
						/* read choice icon  archive filename,  no.
							of choices */
						dev_spec=0;
						fscanf(fd,"%s",areapt[j].cont.fname);	
						uu_dprint(UU_DTRC,(us,"ud_rduimss. CHC fname=%s",
							areapt[j].cont.fname));
								
						sprintf(sympath,"U_ICONDIR/%s", areapt[j].cont.fname);
						if(ux_search_for_path(sympath,devfname,
								UX_PRTERRS|UX_NCHK|UX_NQUOTES) != UU_SUCCESS)
							strcpy(tmpnm,areapt[j].cont.fname);
						else
							strcpy(tmpnm,devfname);
						if (type != UD_ICON)
							tmpnm[strlen(tmpnm) - 1] = '\0';
						else
							tmpnm[strlen(tmpnm) - 3] = '\0';
						sprintf(devfname,"%s%s",tmpnm,tmpnm1);
						uu_dprint(UU_DTRC,(us,"after udos: devfname = %s",devfname));
						if (ainit==0) ginitarch();
						ainit=1;
						areapt[j].devno=uu_nserv_req(UU_CHOICE_NM);
						zbytecp(choicerec,
						  ug_gksstli.wsopen[*UD_ksws].inptr->
														choicedata[areapt[j].devno-1].record);
						 choicerec.devfname=(char *)uu_toolmalloc(strlen(devfname)+1);
						 strcpy(choicerec.devfname,devfname);
						 dev_spec = 0;
						 ddfd = open(devfname,0);
						 if(ddfd != -1)/* file does exist	*/
							 {
		 				 	 uu_dprint(UU_DTRC,(us,"ud_rduimss:ddfd = %d\tfile = %s",
									 ddfd,choicerec.devfname));
							 uu_dprint(UU_DTRC,(us,"dev dep file %s opened",devfname));
							 /* read file	*/
							 read(ddfd,&ras,sizeof(Gipoint));
							 uu_dprint(UU_DTRC,(us,"ud_rduimss:read:ras = %d %d",
																ras.x,ras.y));
							 ras1=ug_gksstli.wsopen[*UD_ksws].wdtptr->dspsize.raster;
							 if((ras.x == ras1.x) && (ras.y == ras1.y)){
							 read(ddfd,&graphics_flag,sizeof(int));
							 if(graphics_flag){
								read(ddfd,&daslen,sizeof(int));
							 	read(ddfd,&position,sizeof(Gdrect));
								 /*	read archive data	*/
                        read(ddfd,&(areapt[j].cont.nchoice),sizeof(int));
                        read(ddfd,&rows,sizeof(int));
                        read(ddfd,&cols,sizeof(int));
                        maxchoice = areapt[j].cont.nchoice;
								if(maxchoice > 0)
									 {
                        	 areapt[j].cont.posn= (Gnrect *)
                               uu_toolmalloc(sizeof(Gnrect)*maxchoice);
                        	 iposn=(Girect *)uu_toolmalloc
											(sizeof(Girect)*maxchoice);
									 }
                         read(ddfd,iposn,
                               areapt[j].cont.nchoice*sizeof(Girect));
							 	 ras=ug_gksstli.wsopen[*UD_ksws].wdtptr->dspsize.raster;
								 for(k=0;k<areapt[j].cont.nchoice;k++)
                           {
               				 areapt[j].cont.posn[k].ll.x=
											 ((UU_REAL)iposn[k].ll.x/(UU_REAL)ras.x);
               				 areapt[j].cont.posn[k].ll.y
											 =((UU_REAL)iposn[k].ll.y/(UU_REAL)ras.x);
               				 areapt[j].cont.posn[k].ur.x
											 =((UU_REAL)iposn[k].ur.x/(UU_REAL)ras.x);
               				 areapt[j].cont.posn[k].ur.y
											 =((UU_REAL)iposn[k].ur.y/(UU_REAL)ras.x);
                           }
								 uu_toolfree(iposn);
								 close(ddfd);
								 ddfd = -1;
								 dev_spec++;
							 ud_initchcarea2(type,&(*duims).screen[scrn],j,&choicerec);
								  }/* End if graphics data	*/
								 } 	/* End if ras = ras1	*/
								 if(ddfd != -1) close(ddfd);
								 }/* End if file opened	*/
						if(dev_spec==0) 		/* device dependant file wasn't read	*/
							{
							int filedesc;
							int nread;
							int status;
							FILE *fileptr;
							char sympath[200];


							 sprintf(sympath,"U_ICONDIR/%s",
													areapt[j].cont.fname);
							 if(ux_search_for_path(sympath,filename,
												UX_PRTERRS|UX_NCHK|UX_QUOTES) != UU_SUCCESS)
								 strcpy(filename,areapt[j].cont.fname);

							 uu_dprint(UU_DTRC,(us,"after udos: filename = %s", 
																filename));
							 strcpy(devfname,&filename[1]);
							 if (type != UD_ICON)
								devfname[strlen(filename) - 3] = '\0';
							 else
								devfname[strlen(filename) - 5] = '\0';

							 strcat(devfname, tmpnm1);
						    uu_dprint(UU_DTRC,(us,"dev dep file %s not used",
														devfname));
							 uu_toolfree(choicerec.devfname);
						 choicerec.devfname=(char *)uu_toolmalloc(strlen(devfname)+1);

						 	 strcpy(choicerec.devfname,devfname);
							 if ((status=ux_open_to_data(filename, "r", "STREAM", 
									"BINARY", &filedesc, UX_PRTERRS)) ==UU_SUCCESS) 
								{
								arclen = glenarcf(filename);

								/* Position to end of file	*/
								ux_get_os_filedesc(filedesc, &fileptr, UX_PRTERRS);
								ux_fseek0(fileptr, arclen, 1);

							/*read data from tail of archfile, Must allocate memory	*/
								nread=1;
								ux_read(filedesc, &areapt[j].cont.nchoice, 
										sizeof(int), &nread, UX_PRTERRS);
								nread=1;
								ux_read(filedesc,&rows,sizeof(int),&nread,UX_PRTERRS);
								nread=1;
								ux_read(filedesc,&cols,sizeof(int),&nread,UX_PRTERRS);
								uu_dprint(UU_DTRC,(us,
									"after read: nchc=%d, rows=%d, cols= %d",
									areapt[j].cont.nchoice,rows,cols))
									maxchoice = areapt[j].cont.nchoice;
      						areapt[j].cont.posn= (Gnrect *)
            						uu_toolmalloc(sizeof(Gnrect)*maxchoice);
      						iconfile= (char **) 
										uu_toolmalloc(sizeof(char *)*maxchoice);
								nread=maxchoice;
								ux_read(filedesc, areapt[j].cont.posn, 
									sizeof(Gnrect), &nread, UX_PRTERRS);
								total_len = 0;
								for(k=0;k<areapt[j].cont.nchoice;k++)
									{
									nread=1;
									ux_read(filedesc, &len, sizeof(int), &nread, UX_PRTERRS);
									total_len += (len+1);
									iconfile[k]=(char*)
										uu_toolmalloc(UX_MAX_FILE_LEN*sizeof(char));
									nread=len+1;
									ux_read(filedesc, iconfile[k], sizeof(char), &nread, UX_PRTERRS);
									}
								ux_close(filedesc, UX_PRTERRS);
								/* write device dependant file	*/
								 if((ddfd=creat(devfname,MASK))== -1){
uu_dprint(-1,(us, "ud_rduimss:Can't create device specific file %s", devfname));
								 }
								 else
									{
		 				uu_dprint(UU_DTRC,(us,"ud_rduimss:ddfd = %d\tfile = %s",
														ddfd,choicerec.devfname));
							      	uu_dprint(UU_DTRC,(us, 
									   	"ud_rduimss: creating device specific file %s",
											devfname));
								ras=ug_gksstli.wsopen[*UD_ksws].wdtptr->dspsize.raster;
								uu_dprint(UU_DTRC,(us,"write xy = %d %d",ras.x,ras.y));
								write(ddfd,&ras,sizeof(Gipoint));
							/* Write graphics flag	*/
								graphics_flag = 0;
								write(ddfd,&graphics_flag,sizeof(int));
	
								/* Solve for total len of das data	*/
								choices = areapt[j].cont.nchoice;
								daslen=3*sizeof(int)+sizeof(Gdrect)+choices*sizeof(Girect)+
									choices*sizeof(int)+total_len * sizeof(char);
	
								write(ddfd,&daslen,sizeof(int));
								write(ddfd,&areapt[j].posn,sizeof(Gdrect));
											/* write archive data*/
                     	write(ddfd,&(areapt[j].cont.nchoice),sizeof(int));
                     	write(ddfd,&(rows),sizeof(int));
                     	write(ddfd,&(cols),sizeof(int));
								ras=ug_gksstli.wsopen[*UD_ksws].wdtptr->dspsize.raster;
								iposn = (Girect *) uu_toolmalloc(sizeof(Girect) * 
										areapt[j].cont.nchoice);
								for(k=0;k<areapt[j].cont.nchoice;k++)
									{
								 	iposn[k].ll.x = (int)(areapt[j].cont.posn[k].ll.x*ras.x);
								 	iposn[k].ll.y = (int)(areapt[j].cont.posn[k].ll.y*ras.x);
								 	iposn[k].ur.x = (int)(areapt[j].cont.posn[k].ur.x*ras.x);
								 	iposn[k].ur.y = (int)(areapt[j].cont.posn[k].ur.y*ras.x);
								 	uu_dprint(UU_DTRC,(us,"rduimss:areapt = %g %g %g %g",
								 		areapt[j].cont.posn[k].ll.x,
								 		areapt[j].cont.posn[k].ll.y,
								 		areapt[j].cont.posn[k].ur.x,
								 		areapt[j].cont.posn[k].ur.y));
									}
                     	write(ddfd,iposn,
											areapt[j].cont.nchoice*sizeof(Girect));
								uu_toolfree(iposn);
                     	for(k=0;k<areapt[j].cont.nchoice;k++)
                        	{
                        	len = strlen(iconfile[k]);
                        	write(ddfd,&len,sizeof(int));
                        	write(ddfd,iconfile[k],len+1);
                        	}
								close(ddfd);
						  		}		/* dev spec file created	*/
								for(k=0;k<areapt[j].cont.nchoice;k++)
									uu_toolfree(iconfile[k]);
								uu_toolfree(iconfile);
							/* Only init UD_CHC areas if not running layout editor*/
							/* deactivate so won't draw the seg*/
								gdeactivatews(UD_ksws);	
							/* get seg nos from archive file */
								ux_strip_quotes(filename);    
								ud_gopenarcf(filename);
								gqarchids(&i,segnos);
							/* get an unused segment no. */
								segno=gnseg();
							/* retrieve 1st seg from archive */
								gretrstr2(1,segnos,&segno);
								gclosarcf();
								areapt[j].segno=segno;
								gactivatews(UD_ksws);
								/* init choice dev for this area*/
						ud_initchcarea2(type,&(*duims).screen[scrn],j,&choicerec);
								gdeactivatews(UD_ksws);
								gdeleteseg(segno);
								gactivatews(UD_ksws);
						  	}/* arcf opened	*/
						else
							{
							char tmp[80],sfn[62];

							ul_short_filename(filename,sfn,60);
							sprintf(tmp,"Can't open file: %s",sfn);
							ud_wrerr(tmp);
							}

                 		gsmodxf(modident,UG_MODREPLACE);
							areapt[j].visible=UG_INVISIBLE;
							}		/* dev spec file not used */
						break;
					}								/* end switch */
				}									/* end for each area of this type*/
			}										/* end for each type of area */
		}											/* end for each screen */
		fclose(fd);
	}														/* end of file opened */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ud_initchcarea2(type,duims,j) -- init choice dev for 
**										jth area of type 'type'.
**							(type is either UD_CHC or UD_ICONM)
**    PARAMETERS   
**       INPUT  : 	int type -- type of area to init.(UD_CHC or UD_ICONM)
**                   int j -- which area to initialize.
**							UD_DUIMSDEF duims -- uims structure.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_initchcarea2(type,duims,j,choicerec)/*init dev for jth area of type 'type'*/
int type;		/* either UD_CHC or UD_ICONM  */
int j;
UD_DUIMSDEF *duims;
Gchoicerec *choicerec;
{
	UD_AREA *areapt;
	static char menutitle[]={"UD_CHC"};
	static char *menutitlep={menutitle};
	static char menutitle2[]={"UD_ICONM"};
	static char *menutitlep2={menutitle2};
	int chcno,k;
	Gdrect pos;
	areapt= &((*duims).areas[type])[j];
	uu_denter2(UU_DTRC,(us,"ud_initchcarea2(%s,%x,%d) segno=%d",
			&typstr[type][0], duims,j,(*areapt).segno));
	choicerec->seg=(*areapt).segno;
	choicerec->number=(*areapt).cont.nchoice;
	if(choicerec->number == 0) choicerec->number =1;
	if(type == UD_CHC)
		choicerec->strings= &menutitlep;
	else
		choicerec->strings= &menutitlep2;
	choicerec->enable=NULL;
	choicerec->chposn = areapt->cont.posn;
	chcno=areapt->devno;
	/* we want the position of the choice device to be specified in NDC.
		However, ginitchoice takes DC. Therefore we must apply the ws xform to
		the echoarea rectangle */
	ud_devrect(&(*areapt).posn,&pos);
	gsedgeflag(OFF);						/* turn off fill area edge flag */
	for(k=0;k<choicerec->number;k++)
		{
			uu_dprint(UU_DTRC,(us,"initchc:choicerec.posn[%d] = %g %g %g %g",
				k,choicerec->chposn[k].ll.x, choicerec->chposn[k].ll.y,
				choicerec->chposn[k].ur.x, choicerec->chposn[k].ur.y));
		}
	ginitchoice(UD_ksws,chcno,0,5,&pos,choicerec);
	(*areapt).visible=UG_INVISIBLE;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ud_initcuruims(curlayout) -- initialize current uims values.
**    PARAMETERS   
**       INPUT :  
**       OUTPUT  : UD_CURLAYOUT *curlayout -- current uims to init.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_initcuruims(curlayout)					/* initialize current values */
UD_CURLAYOUT *curlayout;					/* current layout to initialize */
{
	(*curlayout).curr_menu_stklen=0;
	(*curlayout).curr_menu_area[0]=0;
	(*curlayout).curr_lex_prompt_area=0;
	(*curlayout).curr_sem_prompt_area=0;
	(*curlayout).curr_error_area=0;
	(*curlayout).curr_help_area=0;
	(*curlayout).curr_screen= -1;
}

/*********************************************************************
**    E_FUNCTION :  ud_setarea(duims,curlayout,typ,n,vis,multi) 
**			Set nth layout area in duims ON or OFF. Update curlayout.
**    PARAMETERS   
**       INPUT  : 		int typ -- type of area.
**								int n -- number of area of type typ.
**								int vis -- 1=ON, 0=OFF.
**								int multi-- 1=only 1 LPRMT, SPRMT, ERR, HLP area
**												on at a time.
**												0=any number on at a time.
**								UD_DUIMSDEF duims -- uims struct.
**       OUTPUT :  		UD_CURLAYOUT *curlayout.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_setarea(duims,curlayout,typ,n,vis,multi)
UD_DUIMSDEF *duims;			/* uims structure */
UD_CURLAYOUT *curlayout;	/* update current values in curlayout */
int typ;							/* type of area */
int n;							/* number of area of type typ. */
int vis;							/* 0=OFF, 1=ON. */
int multi;						/* 1=only 1 LPRMT, SPRMT, ERR, HLP area ON */
{
	UD_AREA *areapt,*areapt2;
	int i;

	if ((typ<0)||(typ>=UD_NTYPES)) {
		uu_denter2(UU_DTRC,(us,"ud_setarea(error typ=%d,n=%d,vis=%d)",typ,n,vis));
	}
	else {							/* typ is OK */
	uu_denter2(UU_DTRC,(us,"ud_setarea(%x,%x,%s,%d,vis=%d,multi=%d)",
		duims,curlayout,&typstr[typ][0],n,vis,multi));
	if ((*duims).noareas[typ]>n){
	areapt= &((*duims).areas[typ])[n];
	uu_dprint(UU_DTRC,(us,"ud_setarea. segno=%d",(*areapt).segno));
	switch (typ) {
	case UD_GRAF:
	case UD_MENU:
	case UD_SCROLL:
	case UD_FORMS:
		if (vis==ON) {
			if ((*areapt).visible==UG_INVISIBLE) 
				gssegvis((*areapt).segno,UG_VISIBLE);
			(*areapt).visible=UG_VISIBLE;
		}
		if (vis==OFF) {
			if ((*areapt).visible==UG_VISIBLE) 
				gssegvis((*areapt).segno,UG_INVISIBLE);
			(*areapt).visible=UG_INVISIBLE;
		}
		break;
	case UD_LPRMT:
	case UD_SPRMT:
	case UD_ERR:
	/*case UD_HLP:		Do not display help area	*/
		if (multi==1) {
		/* take down any other prompt, error, or help msg area */
		for (i=0; i<(*duims).noareas[typ]; i++) {
			if (i!=n) {
				areapt2= &((*duims).areas[typ])[i];
				if ((*areapt2).visible==UG_VISIBLE) {
					gssegvis((*areapt2).segno,UG_INVISIBLE);
					(*areapt2).visible=UG_INVISIBLE;
				}
			}
		}
		}					/* end of multi==1 */
		if (vis==ON) {
			if ((*areapt).visible==UG_INVISIBLE)		/* prompt not already up */
				gssegvis((*areapt).segno,UG_VISIBLE);	/* put up prompt area */
			(*areapt).visible=UG_VISIBLE;					/* remember it is up */
			switch (typ) {
			case UD_LPRMT:
				(*curlayout).curr_lex_prompt_area=n;
				break;
			case UD_SPRMT:
				(*curlayout).curr_sem_prompt_area=n;
				break;
			case UD_ERR:
				(*curlayout).curr_error_area=n;
				break;
			case UD_HLP:
				(*curlayout).curr_help_area=n;
				break;
			}
		}
		if (vis==OFF) {
			if ((*areapt).visible==UG_VISIBLE)				/* prompt up */
				gssegvis((*areapt).segno,UG_INVISIBLE);	/* take down prompt area */
			(*areapt).visible=UG_INVISIBLE;					/* remember it is down */
			switch (typ) {
			case UD_LPRMT:
				/* if we just made the current prompt area invisible,
					don't know which one is now current, so set it to 0 */
				if ((*curlayout).curr_lex_prompt_area==n) 
					(*curlayout).curr_lex_prompt_area=0;
				break;
			case UD_SPRMT:
				if ((*curlayout).curr_sem_prompt_area==n)
					(*curlayout).curr_sem_prompt_area=0;
				break;
			case UD_ERR:
				if ((*curlayout).curr_error_area==n)
					(*curlayout).curr_error_area=0;
				break;
			case UD_HLP:
				if ((*curlayout).curr_help_area==n)
					(*curlayout).curr_help_area=0;
				break;
			}
		}
		break;					/* end of outer case UD_LPRMT, UD_SPRMT, UD_ERR */
	case UD_CHC:
	case UD_ICONM:
	case UD_ICON:
		if (vis==ON) {
			if ((*areapt).visible==UG_INVISIBLE)		/* icons not already up */
				{
				/* put up icons */
/*
.....Leave Icons down by default
.....at start of program
.....Bobby  -  10/9/92
*/
/*						gschoicemode(UD_ksws,(*areapt).devno,UG_EVENT,UG_ECHO);	*/
				}
/*			(*areapt).visible=UG_VISIBLE;*/
		}
		if (vis==OFF) {
			/* take icons down */
			if ((*areapt).visible==UG_VISIBLE)
				{
					{
					/*if(typ != UD_ICON)*/
						gschoicemode(UD_ksws,(*areapt).devno,UG_REQUEST,UG_NOECHO);	
					/*else
						gssegvis((*areapt).segno,UG_INVISIBLE);*/
					}
				}
			(*areapt).visible=UG_INVISIBLE;
		}
		break;
	}								/* end of outer switch(typ) */
	}								/* end of noareas>n */
	}								/* end of typ OK */
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ud_actscrn() --
**    PARAMETERS   
**       INPUT  :  int n - screen number to activate
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_actscrn(screen)
int screen;
{
	uu_denter(UU_DTRC,(us,"ud_actscrn(%d)",screen));
	ud_setuims(&UD_duimsdeflt,&UD_curlayout,screen);
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION         :  ud_setuims(duims,curlayout,n) 
**		Turn ON nth user interface (screen) in duims. Update curlayout.
**		Turn ON:
**			nth graphics area, nth choice area, nth lex prompt area,
**			nth semantic prompt area, nth error msg area, nth help msg
**			area, nth workstation window.
**    PARAMETERS   
**       INPUT  : 	int n -- user interface number.
**							UD_UIMS *duims.
**       OUTPUT :  	UD_CURLAYOUT *curlayout;
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : Do not use this routine if you are using the viewing
**							system - Use uv_setscreen(n)
*********************************************************************/

void ud_setuims(duims,curlayout,n)
UD_UIMS *duims;							/* uims structure to set */
UD_CURLAYOUT *curlayout;				/* update this current layout */
int n;
{
	int tmpcolor;

	uu_denter(UU_DTRC,(us,"ud_setuims(%x,%x,%d)",duims,curlayout,n));
	if(n > (*duims).nscreens) 
		{
		uu_dprint(UU_DTRC,(us,"ud_setuims ERROR - Less than %d screens",n));
		uu_dexit;
		return;
		}
	if (n>=0) 
	{
		if ((*curlayout).curr_screen!=n) 
		{					/* screen changed */
				
			tmpcolor = gqsegerasecolor();
			gsegerasecolor(0);

			/* set the workstation window */
			gswswindow(UD_ksws,&(*duims).screen[n].wswind);

			ud_setuims2(duims,curlayout,n);		/* set areas */
					
			ud_igks(UU_TRUE);		/* causes DAS to re-initialize input
										devices so echoes will be in correct areas */
			gsegerasecolor(tmpcolor);
		}										/* end of screen changed */
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION         :  ud_setuims2(duims,curlayout,n) 
**		Turn ON nth user interface (screen) in duims, except for wswind
**		and UD_MENU.  Update curlayout.
**		Dont turn on UD_MENU because menu traverser pops up and down the
**		menus. 
**    PARAMETERS   
**       INPUT  : 	int n -- user interface number.
**							UD_UIMS *duims.
**       OUTPUT :  	UD_CURLAYOUT *curlayout;
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_setuims2(duims,curlayout,n)
UD_UIMS *duims;							/* uims structure to set */
UD_CURLAYOUT *curlayout;				/* update this current layout */
int n;										/* which screen to turn ON */
{
	int i;
	int typ;
	int scrn;								/* number of currently ON screen */

	uu_denter(UU_DTRC,(us,"ud_setuims2(%x,%x,%d)",duims,curlayout,n));
	if (n>=0) {
		/* take down all areas for other screen that may be up */
		scrn=(*curlayout).curr_screen;
		if ((scrn!=n)&&(scrn != -1)) {	/* another screen is up */
			for (typ=0; typ<UD_NTYPES; typ++) {
				if (typ!=UD_MENU) {
					for (i=0; i<(*duims).screen[scrn].noareas[typ]; i++) {	
						ud_setarea(&(*duims).screen[scrn],curlayout,typ,i,OFF,1);
					}
				}
			}
		}												/* end of another screen is up */

		/* put up all areas for this screen */
		for (typ=0; typ<UD_NTYPES; typ++) {
			if (typ!=UD_MENU) {
				for (i=0; i<(*duims).screen[n].noareas[typ]; i++) {	
					ud_setarea(&(*duims).screen[n],curlayout,typ,i,ON,1);
				}
			}
		}
		(*curlayout).curr_screen=n;			/* update current screen */
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION         :  ud_setuims3(duims,curlayout,n) 
**		Turn ON nth user interface (screen) in duims, except for wswind.
**		Update curlayout.
**    PARAMETERS   
**       INPUT  : 	int n -- user interface number.
**							UD_UIMS *duims.
**       OUTPUT :  	UD_CURLAYOUT *curlayout;
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_setuims3(duims,curlayout,n)
UD_UIMS *duims;							/* uims structure to set */
UD_CURLAYOUT *curlayout;				/* update this current layout */
int n;										/* which screen to turn ON */
{
	int i;
	int typ;
	int scrn;								/* number of currently ON screen */

	uu_denter(UU_DTRC,(us,"ud_setuims3(%x,%x,%d)",duims,curlayout,n));
	if (n>=0) {
		/* take down all areas for other screen that may be up */
		scrn=(*curlayout).curr_screen;
		if (scrn!=n) {	/* another screen is up */
			for (typ=0; typ<UD_NTYPES; typ++) {
				for (i=0; i<(*duims).screen[scrn].noareas[typ]; i++) {	
					ud_setarea(&(*duims).screen[scrn],curlayout,typ,i,OFF,1);
				}
			}
		}												/* end of another screen is up */

		/* put up all areas for this screen */
		for (typ=0; typ<UD_NTYPES; typ++) {
			for (i=0; i<(*duims).screen[n].noareas[typ]; i++) {	
				ud_setarea(&(*duims).screen[n],curlayout,typ,i,ON,1);
			}
		}
		(*curlayout).curr_screen=n;			/* update current screen */
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ud_genareaseg(areapt) -- gen undetectable segment for area.
**    PARAMETERS   
**       INPUT  : UD_AREA *areapt -- pointer to area.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_genareaseg(areapt)		/* generate an undetectable segment for an area*/
UD_AREA *areapt;					/* pointer to area to gen border for */
{
	uu_denter(UU_DTRC,(us,"ud_genareaseg(%x)",areapt));
	ud_genareasegdet(areapt,(Gsegdet)UG_UNDETECTABLE);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ud_genareasegdet(areapt,det) -- gen a seg for an area.
**    PARAMETERS   
**       INPUT  : 	UD_AREA *areapt -- pointer to area.
**							Gsegdet det -- detectability of new seg.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_genareasegdet(areapt,det)			/* gen a seg for an area */
												/* return segment number */
UD_AREA *areapt;							/* pointer to area */
Gsegdet det;								/* detectability of new seg */
{
	int segno;
  	Glntype type;
	segno=gnseg();
	(*areapt).segno=segno;
	uu_denter2(UU_DTRC,(us,"ud_genareasegdet(%x,%s) segno=%d",
		areapt,&sdet[(int)det][0],(*areapt).segno));
	gcreateseg(segno);			/* create seg */
  	type.typeno=1;
  	gslinetype(&type);
	gslinecolor(1);
	gslinewidth(1.0);
	ud_genareaseg2(segno,areapt);
	gcloseseg();
	gssegpri(segno,UD_LAYPRI);
	if (det==UG_UNDETECTABLE) gssegdet(segno,UG_UNDETECTABLE);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ud_genareaseg2(segno,areapt) -- put fillarea into seg.
**    PARAMETERS   
**       INPUT  : 	int segno -- segment number. Must be open.
**							UD_AREA *areapt -- pointer to area.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_genareaseg2(segno,areapt)				/* put fillarea into area seg */
int segno;										/* segment no. */
UD_AREA *areapt;								/* pointer to area */
{
	int colr;
	Gwpoint points[5];
	uu_denter(UU_DTRC,(us,"ud_genareaseg2(segno=%d,%x)",segno,areapt));
	gssegvis(segno,UG_INVISIBLE);
	(*areapt).visible=UG_INVISIBLE;
	gsnormtran(0);
	colr=(*areapt).color;
	ud_get5pts(&(*areapt).posn,points);
	if(colr != 0)
		{
		gsfillcolor(colr);
		gfillarea(4,points);
		}
	colr=(*areapt).bordercolor;
	if(colr != 0)
		{
		gslinecolor(colr);
		gpolyline(5,points);
		}
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION :  ud_initchcarea(duims,j) -- init choice dev for 
**										jth UD_CHC area.
**    PARAMETERS   
**       INPUT  : 	int j -- which UD_CHC area to initialize.
**							UD_DUIMSDEF duims -- uims structure.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_initchcarea(duims,j)				/* init choice dev for jth UD_CHC area*/
int j;
UD_DUIMSDEF *duims;
{
Gchoicerec choicerec;
	uu_denter(UU_DTRC,(us,"ud_initchcarea(%x,%d)",
			duims,j));
	ud_initchcarea2(UD_CHC,duims,j,&choicerec);
	uu_dexit;
}

/************** functions below here are not user callable ***********/
/*********************************************************************
**    I_FUNCTION :  ud_gensegs(uimsdef,det) -- generate segs for a layout.
**    PARAMETERS   
**       INPUT  :  UD_DUIMSDEF *uimsdef -- pointer to a layout structure.
**						Gsegdet det -- generated segment detectability, 
**											DETECTABLE or UG_UNDETECTABLE
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ud_gensegs(uimsdef,det)	/* generate border/background segments for
							all types of areas except UD_CHC and UD_ICON.  */
UD_DUIMSDEF *uimsdef;		/* pointer to the uims to generate segs for */
Gsegdet det;					/* detectability for generated segments */
{
	int i,typ;
	UD_AREA *areapt;
	uu_denter(UU_DTRC,(us,"ud_gensegs(%x,%s)",uimsdef,&Sdet[(int)det][0]));
	for (typ=0; typ<UD_NTYPES; typ++) {
		if ((typ!=UD_CHC)&&(typ!=UD_ICON)&&(typ!=UD_ICONM)) {
			uu_dprint(UU_DTRC,(us,"ud_gensegs. generate %s segments",
				&typstr[typ][0]));
			for (i=0; i<(*uimsdef).noareas[typ]; i++) {
				areapt= &(((*uimsdef).areas[typ])[i]);
				ud_genareasegdet(areapt,det);
/*MILLS: set number of highest window boarder segment */
				NCL_MAXWINSEGNO = areapt->segno;
			}
		}	
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  Gnrect  *ud_geticdata(n)
**       get icon position array from the open archive file. Return
**			pointer to it.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  	int *n; -- number of choices in menu.
**    RETURNS      : pointer to array of positions of each icon.
**    SIDE EFFECTS : allocated memory for the choice positions.
**    WARNINGS     : An archive file must be open.
*********************************************************************/

Gnrect *ud_geticdata(n)				/* get icon info, return pointer to it*/
int *n;									/* number of choices in menu */
{
	int arclen;
	int archf;							/* file descriptor for archive file */
	Gnrect *p;
	int itrash[2];
	arclen = glenarcf(&archf);
	/* NOTE read data from tail of archfile */
	read(archf,n,sizeof(int));				/* read number of choices */
	uu_denter2(UU_DTRC,(us,"ud_geticdata(nchoices=%d). arclen=%d",(*n),arclen));
	read(archf,itrash,2*sizeof(int));		/* throw away rows, cols */
	p=(Gnrect *)uu_toolmalloc((*n)*sizeof(Gnrect));
	read(archf,p,(*n)*sizeof(Gnrect));
	uu_dexit;
	return(p);
}

/*********************************************************************
**    E_FUNCTION :  ud_deluims(duims) -- delete a uims.
**    PARAMETERS   
**       INPUT  :  UD_UIMS *duims -- uims to delete.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ud_deluims(duims)							/* delete a uims */
UD_UIMS *duims;						/* uims to delete */
{
	int typ,j,n;
	UD_AREA *areapt;
	int scrn;

	uu_denter(UU_DTRC,(us,"ud_deluims(%x)",duims));
	for (scrn=0; scrn<(*duims).nscreens; scrn++) {
		uu_dprint(UU_DTRC,(us,"ud_deluims deleting scrn %d",scrn));
		for (typ=0; typ<UD_NTYPES; typ++) {
			n=(*duims).screen[scrn].noareas[typ];
			for (j=0; j<n; j++) {
				areapt= &((*duims).screen[scrn].areas[typ])[j];
				uu_dprint(UU_DTRC,(us,"ud_deluims delete area[%s][%d]. segno=%d",
					&typstr[typ][0],j,(*areapt).segno));
				if((typ == UD_CHC)|| (typ == UD_ICON)|| (typ == UD_ICONM))
					{
					/*gdeactivatews(UD_ksws);
					for(l=0;l<30;l++)
					gactivatews(UD_ksws); */
					}
				else
					{
					if(typ == UD_ICON)
						{
						gdeactivatews(UD_ksws);
						gdeleteseg(areapt->segno);
						gactivatews(UD_ksws);
						}
					else
						gdeleteseg(areapt->segno);
					}
			}
			/* free the memory for the n areas of this type */
			uu_toolfree((*duims).screen[scrn].areas[typ]);
			/* remember we have no areas of this type*/
			(*duims).screen[scrn].noareas[typ]=0;	
		}
	}
	uu_dexit;
}
