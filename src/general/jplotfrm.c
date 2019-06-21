
/*********************************************************************
**    NAME         :  jplotfrm.c
**       CONTAINS:
**            uj_plotfrm              uji_getsetup            uji_packit
**            uj_copysetup    
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       jplotfrm.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:47
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "uhep.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "plotfrm.h"
#include "xenv1.h"
#include "xfsys1.h"

#define  DEFAULT  1
#define  NODEFAULT   0
/* char  us[132]; */
void uji_getsetup();
void uji_packit();
/*********************************************************************
**    I_FUNCTION :  uj_plotfrm()
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uj_plotfrm()

{
	/*UD_FSTRUCT *fs;                     /* form structure */
	/*UD_FDATA *fdata;                    /* form answers */
	int     *ans[260];                                                      /* contains form's answer address */
	int     *def[260];                                                      /* contains default information */
   PLOTFRM  plotdata;                  /* plotter form data */
   int i,j;
   UX_pathname tempnm;
   int   fcount, status;
   int   len;
   char *p, *ux_getenv(), ext[UX_MAX_PATH_LEN], ext1[UX_MAX_FILE_LEN];
	char descrip[UX_MAX_PATH_LEN];

   uu_denter(UU_DTRC,(us, "uj_plotfrm"));
/*
.....changed to add file browse
*/
	tempnm[0] = '\0';
	strcpy(descrip, "Plot Setup Files (");
	strcpy(ext,"*.");
/*      p = ux_getenv("UJ_PLOT_SUFFIX"); */
	p = ux_getenv("UJ_PLOT_SETUP"); 
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
	}       
	else
	{
		strcat(ext,"ps");
	}
	strcat(descrip, ext);
	strcat(descrip, ")");
	ud_get_filename("Enter Setup File", "Enter Setup File", ext, tempnm, &len,
		descrip, 1, UU_FALSE);
	if (tempnm[0] == '\0')
/*
.....Canceled
*/
	{
		return;
	}
   uu_dprint(UU_DTRC,(us,"after ud_ldas,tempnm=%s",tempnm));
   plotdata.sfnm[0] = '\0';
   plotdata.mdtype[0] = '\0';
   plotdata.formno = 0;
   plotdata.pentbnm[0] = '\0';
   for (i=0;i<256;i++)
      plotdata.pen[i][0] = '\0';
   uji_getsetup(tempnm,&plotdata,&fcount);
   uu_dprint(UU_DTRC,(us,"after uji_getsetup.-fnm=%s,psize=%d,mdtype=%s,fno=%d,ptbnm=%s",
   plotdata.sfnm,plotdata.dsize,plotdata.mdtype,plotdata.formno,plotdata.pentbnm)); 
   uu_dprint(UU_DTRC,(us,"fcount=%d,pencolor=%s,%s,%s,%s,%s,%s,%s,%s,offset=%d",
   fcount,plotdata.pen[0],plotdata.pen[1],plotdata.pen[2],plotdata.pen[3],
   plotdata.pen[4],plotdata.pen[5],plotdata.pen[6], plotdata.pen[7],offset));

	/* initialize "ans"&"def" to point to necessary data field */ 
    /* field 0-- set up file name interaction -- */
    ans[0] = (int *)plotdata.sfnm;
    def[0] = (int *)plotdata.sfnm;                              /* with default value */
    
    /* filed 1 -- media type   */
    ans[1] = (int *)plotdata.mdtype;
    def[1] = (int *)plotdata.mdtype;                    /* with default value */
 
    /* filed 2 -- form number  */
    ans[2] = (int *)&plotdata.formno;
    def[2] = (int *)&plotdata.formno;                                   /* with default value */
 
    /* filed 3 -- pen table name  */
    ans[3] = (int *)plotdata.pentbnm;
    def[3] = (fcount<=0)? UU_NULL : (int *)plotdata.pentbnm;    
 
    /* filed 4 to 11 -- pen holder setup   */
    j = 4;
    for (i=0; i<20; i++)
      {
       ans[j] = (int *)plotdata.pen[i];
       def[j] = (fcount<=0)? UU_NULL : (int *)plotdata.pen[i]; 
		 j++;
      }
    /* -- go get all that input -- */
	status = ud_form("plotform.frm", def, ans);
	if (status==-1)
		return;
/*   plotdata.dsize = (DCSIZE)ppsize[0];        */
   uji_packit(&plotdata,fcount);
   uu_dexit;
}  /* uj_plotfrm  */




/*********************************************************************
**    I_FUNCTION :  uji_getsetup(tempnm,plotdata,fcount)
**       Get a setup data from a file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uji_getsetup(tempnm,plotdata,fcount)
char  *tempnm;
PLOTFRM  *plotdata;
int   *fcount;

{
   int   fd1;
	FILE    *fdpt;
   UX_pathname filenm;
	char temp[132], buf[132];
	char templ[1320];
	int     ind;
   int   done;
   int   i, j, k;
	int     mode;

	uu_denter(UU_DTRC,(us,"uji_getsetup,tempnm=%s",tempnm));
	*fcount = -1;
	done = UU_FALSE;
/*
.....we allow using pass in empty setup table name, in that case, we will using
.....default data
*/
	if (tempnm[0] == '\0')
	{
	   plotdata->sfnm[0] = '\0';
	   plotdata->mdtype[0] = '\0';
	   plotdata->formno = 0;
	   plotdata->pentbnm[0] = '\0';
	   for (i=0;i<256;i++)
		  plotdata->pen[i][0] = '\0';
	   *fcount = 0;
	   return;
	}
	sprintf (plotdata->sfnm, "%s", tempnm);
	mode = 0;
/*
.....added for check directory not default
.....Yurong 8/29/97
*/
	ul_break_fname(tempnm,filenm,templ);
	if (filenm[0] != '\0')
		ux_mk_chk_syspath(UU_NULL,UU_NULL,tempnm,UU_NULL,"UJ_PLOT_SUFFIX",
			&mode,filenm,UX_PRTERRS); 
	else
		ux_mk_chk_syspath(UU_NULL,"^UJ_PLOTSETUP",tempnm,UU_NULL,"UJ_PLOT_SUFFIX",
			&mode,filenm,UX_PRTERRS); 
		
	
   uu_dprint(UU_DTRC,(us,"after ux_mk_chk_syspath, filenm=%s",filenm));
   if (mode != (mode|UX_NEXISTS))
   { 
		if (ux_open_to_data(filenm,"r+","STREAM","TEXT",&fd1,UX_PRTERRS) ==
			UU_SUCCESS)
		{
		   ux_get_os_filedesc(fd1,&fdpt,UX_PRTERRS);
		   if (ux_fgets(temp,132,fdpt)==UX_EOF)         
			  {
				*fcount = 0;
				uu_dexit;       return;
			  }
	 sscanf(temp, "%s", plotdata->sfnm);

		   if (ux_fgets(temp,132,fdpt)==UX_EOF)         
			  {
				*fcount = 0;
				uu_dexit;       return;
			  }
		   ind = -1;
/*----------    no paper size any more
		  for (i=0; (temp[++ind]!=';'); i++)    buf[i] = temp[ind];
		  buf[i] = '\0';
		  sscanf(buf,"%d", &plotdata->dsize);
------------ */
			if(((char*)strchr(temp, ';'))==NULL)
			{
				*fcount = 0;
	    uu_dexit;   return;
			}               
		   for (i=0; (temp[++ind]!=';'); i++)   plotdata->mdtype[i] = temp[ind];
		   plotdata->mdtype[i] = '\0';
		   for (i=0; (temp[++ind]!=';'); i++)   buf[i] = temp[ind];
		   buf[i] = '\0';
		   sscanf(buf,"%d", &plotdata->formno);
		   for (i=0; (temp[++ind]!=';'); i++)   plotdata->pentbnm[i] = temp[ind];
		   plotdata->pentbnm[i] = '\0';
	   if ((strlen(plotdata->mdtype)==1)&&(plotdata->mdtype[0]=='@'))
	     plotdata->mdtype[0] = '\0';
	   if ((strlen(plotdata->pentbnm)==1)&&(plotdata->pentbnm[0]=='@'))
	     plotdata->pentbnm[0] = '\0';
		   if (ux_fgets(temp,132,fdpt)==UX_EOF)         
			  {
				*fcount = 0;
				uu_dexit;       return;
			  }
		   ind = -1;
/*
...Change pen to 256
...Yurong
*/          
			k = 0;
		templ[0] = '\0';
		while(1)
		{  
				if(k!=0)
					if (ux_fgets(temp,132,fdpt)==UX_EOF)
						break;
		k++;
			strncat(templ, temp,80);
		}

		for (j=0; j<256; j++)
			{       
/*
......check for new line
.....Yurong 8/5/97
*/
				if (templ[ind+1] == '\n')
					ind++;
				if(templ[ind+1] != '\0')
				{       
					for (i=0; (templ[++ind]!=';'); i++)     
						plotdata->pen[j][i] = templ[ind];
					plotdata->pen[j][i] = '\0';
				if (plotdata->pen[j][0]=='@')
					plotdata->pen[j][0] = '\0';
				}
				else
					plotdata->pen[j][0] = '\0';
			}
			uu_dprint(UU_DTRC,(us,"uji_getsetup.fnm=%s,psize=%d,mdtype=%s,fno=%d,ptbnm=%s",
	 plotdata->sfnm,plotdata->dsize,plotdata->mdtype,plotdata->formno,plotdata->pentbnm)); 
	 uu_dprint(UU_DTRC,(us,"pencolor=%s,%s,%s,%s,%s,%s,%s,%s",
	 plotdata->pen[0],plotdata->pen[1],plotdata->pen[2],plotdata->pen[3],
	 plotdata->pen[4],plotdata->pen[5],plotdata->pen[6], plotdata->pen[7]));
	
	 ux_close(fd1, UX_PRTERRS);
	 *fcount = 1;
		}
		else 
			*fcount = 0;
		
    }
   uu_dexit;
}  /* uji_getsetup */

/*********************************************************************
**    I_FUNCTION :  uji_packit80(plotdata,fcount)
**       Put a string into a file (80 chars a line)
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_pack80(fid,buf,len)
int fid;
char  buf[];
int   len;
{
   int icnt,nc,ipt;
   char sbuf[82];
   uu_denter(UU_GITRC,(us,"uw_7475func,len=%d", len));
/*
.....Added logic to write 80 col records
.....Bobby  -  8/8/91
*/
   nc = len;
   ipt = 0;
   do
   {
      icnt = nc;
      if (icnt > 80) icnt = 80;
      strncpy (sbuf,&buf[ipt],icnt);
      sbuf[icnt] = '\n';
      nc = nc - icnt;
      ipt = ipt + icnt;
      icnt++;
		ux_write(fid,sbuf,sizeof(char),&icnt,UX_PRTERRS);
   } while (nc > 0);

   uu_dexit;
}


/*********************************************************************
**    I_FUNCTION :  uji_packit(plotdata,fcount)
**        Put a set of setup data into a file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uji_packit(plotdata,fcount)
PLOTFRM  *plotdata;
int   fcount;

{
   int   fd1;
   int   i;
   int   len, mode;
   char  buf[600];
	char  buf1[1000];
   UX_pathname pathnm;
	char msg[200];
	int answer;
	int uj_pack80();
   uu_denter(UU_DTRC,(us,"uji_packit,sfnm=%s",plotdata->sfnm));

/*
.....added for check directory not default
.....Yurong 9/9/97
*/
	mode = 0;
	ul_break_fname(plotdata->sfnm,pathnm,buf);
	if (pathnm[0] != '\0')
		ux_mk_chk_syspath(UU_NULL, UU_NULL, plotdata->sfnm,UU_NULL,
			"UJ_PLOT_SUFFIX", &mode,pathnm,UX_PRTERRS);
	else
		ux_mk_chk_syspath(UU_NULL,"^UJ_PLOTSETUP",plotdata->sfnm,UU_NULL,
			"UJ_PLOT_SUFFIX", &mode,pathnm,UX_PRTERRS);
/*
.....added by Yurong
.....9/9/97
*/
	if (!(mode & UX_NEXISTS))
	{
		sprintf(msg, "File %s Exists, Overwrite?",pathnm);
		answer = ud_yesno(UU_NULL, msg, "File Exists");
		if (answer == 1) ux_delete(pathnm, UX_PRTERRS);
		else return;
	}
   for (i=0; i<256; i++)
     if (plotdata->pen[i][0]=='\0')
      {
       plotdata->pen[i][0] = '@';
       plotdata->pen[i][1] = '\0';
      }
	if (ux_create_file(pathnm,0660,UU_NULL,"STREAM","TEXT","UX_NOEXTRA",
		&fd1,UX_PRTERRS) == UU_SUCCESS)
     {
		uu_dprint(UU_DTRC,(us,"success in creating a new file"));
      if (plotdata->mdtype[0]=='\0')  
	sprintf(plotdata->mdtype, "@");
      if (plotdata->pentbnm[0]=='\0')
	sprintf(plotdata->pentbnm, "@");                
      sprintf(buf,"%s\n%s;%d;%s;\n", plotdata->sfnm,plotdata->mdtype,
	   plotdata->formno,plotdata->pentbnm);
		len = strlen(buf);
		ux_write(fd1,buf,sizeof(char),&len,UX_PRTERRS);
		buf1[0] = '\0';
		for( i=0; i<256; i++)
		{
			sprintf(buf, "%s;", plotdata->pen[i]);
			strcat(buf1,buf);
		}
		len = strlen(buf1);
	uj_pack80(fd1,buf1,len);
		sprintf(buf, "\n");
      len = strlen(buf);
      ux_write(fd1,buf,sizeof(char),&len,UX_PRTERRS);
      uu_denter2(UU_DTRC,(us,"uji_packit,buf=%s",buf));
      uu_dexit;
      ux_close(fd1,UX_PRTERRS);
     }
	else
	{
		uu_uerror1 (UJ_SUPPORT,30, pathnm);
		return;
	}

   uu_dexit;
}  /* uji_packit */

/*********************************************************************
**    I_FUNCTION :  uj_copysetup()
**       Copy a setup file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....change this function to use generic function "file copy"
.....Yurong 9/16/98
*/
void uj_copysetup(setup_name, newname)
char *setup_name, *newname;
{
   PLOTFRM  plotdata;                  /* plotter form data */
   int   fcount;

   uu_denter(UU_DTRC,(us, "uj_copysetup"));

	if (strlen(setup_name)== 0) 
	{
		uu_uerror1 (UJ_SUPPORT,5);
		return;
	}
	ux_add_ftype("UJ_PLOT_SUFFIX", setup_name, UX_PRTERRS);
	ul_remove_quotes(setup_name);
	uji_getsetup(setup_name,&plotdata,&fcount);
	if ( fcount <= 0 )
		uu_uerror1 (UJ_SUPPORT,1,setup_name);
	else
	{
		if (strlen(newname)== 0) 
		{
			uu_uerror1 (UJ_SUPPORT,5);
			return;
		}
		ux_add_ftype("UJ_PLOT_SUFFIX", newname, UX_PRTERRS);
		ul_remove_quotes(newname);
		strcpy(plotdata.sfnm,newname);
		fcount = -1;
		uji_packit(&plotdata,fcount);
	}
   uu_dexit;
}  /* uj_copysetup   */

