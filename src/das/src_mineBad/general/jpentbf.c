/*********************************************************************
**    NAME         :  jpentbf.c
**       CONTAINS:
**          uj_pentbfrm     uji_getpentb    uji_pentbst     uji_pentbpack
**          uj_copypentb
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       jpentbf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:47
*********************************************************************/

#include        "stdio.h"
#include "usysdef.h"
#include "uhep.h"
#include "udebug.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include        "plotfrm.h"
#include "xenv1.h"              /* used by "ux" calls: UX_PRTERRS value */
#include "xfsys1.h"

#define DEFAULT 1
#define NODEFAULT       0
#if UU_DEBUG
char    us[132];
#endif

void uji_getpentb();
void uji_pentbpack();

/*********************************************************************
**    I_FUNCTION :  uj_pentbfrm()
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

void uj_pentbfrm()

{
/*
.....change ans[27], def[27]
.....to hold pen number allow
.....user set in form to 16
.....Yurong 8/4/97
*/
	int     *ans[51];       /* contains form's answer address */
	PENTBFRM        pentbdata;                                                      /* plotter form data */
	int     i,j,k, status;
	int     dcont[5];
	UX_pathname tempnm;
	int     fcount,pencount;
	int     len;
	UX_pathname ext,descrip;
	char *p, ext1[UX_SUFFIX_LEN],*ux_getenv();
	int     conti, kcount;
	int     pentb[256][3];
	int   page, m, n;
	uu_denter(UU_DTRC,(us, "uj_pentbfrm"));
	tempnm[0] = '\0';
	
	strcpy(descrip, "Pen Table Files (");
	strcpy(ext,"*.");
	p = ux_getenv("UJ_PEN_SUFFIX");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
	}       
	else 
	{
		strcat(ext,"pt");
	}
	strcat(descrip, ext);
	strcat(descrip, ")");
	ud_get_filename("Enter Pen Table File", "Enter Pen Table File", ext, tempnm,
		&len, descrip, UU_FALSE);

	if (tempnm[0]=='\0')
	{
/*
.....canceled
*/
		return;
	}
	for (j=0;j<256;j++)
	  {
		pentbdata.penno[j][0] = 0;
		pentbdata.penno[j][1] = 0;
		pentbdata.penno[j][2] = 0;
	  }
	dcont[0] = 0;
	kcount = 0;
	uji_getpentb(tempnm,pentb,&fcount,&pencount);
	uu_dprint(UU_DTRC,(us,"after uji_getpentb,fcount=%d,pen=%d",fcount,pencount));
	
	sprintf(pentbdata.ptbnm,"%s",tempnm); 
	conti = UU_TRUE;
	page = 0;
	for (i=0; i<256; i++,kcount++)
		for (j=0; j<3; j++)
			pentbdata.penno[i][j] = pentb[kcount][j];
	while (conti)
	{
/*
......field 0-- pen table name 
*/
		ans[0] = (int *)(pentbdata.ptbnm);

/*
......filed 1 -- continue editing page number 
*/
		ans[1] = (int *)&page;
  
/*
......filed 2 -- continue displaying? 
*/
		ans[2] = dcont;
  
		k = 0;
		for (j=page*16;j<(page+1)*16;j++)
		{
/*
......filed 1 -- logical pen # - from  
*/
			ans[k+3] = &(pentbdata.penno[j][0]);
  
/*
......filed 2 -- logical pen # - to  
*/
			ans[k+4] = &(pentbdata.penno[j][1]);
  
/*
......filed 3 -- physical pen # - to  
*/
			ans[k+5] = &(pentbdata.penno[j][2]);
			k = k + 3;
		}
	  /*    -- go get all that input -- */
		status = ud_form("jpentb.frm", ans, ans);
		if (status==-1)
			return;
		conti = (dcont[0]);
		for (m = 16*(page); m<(page+1)*16; m++)
			for (n=0; n<3; n++)
				pentb[m][n] = pentbdata.penno[m][n];
	 }
	uji_pentbpack(pentbdata.ptbnm,pentb,fcount);
   uu_dexit;
}       /* uj_pentbfrm  */



/*********************************************************************
**    I_FUNCTION :  uji_getpentb(temnm,pentb,fcount)
**       Get the pentable information from the pentable file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uji_getpentb(pentbnm,pentb,fcount,pencount)
char    *pentbnm;
int     pentb[256][3], *fcount;
int     *pencount;

{
	int     fd1;
	FILE    *fdpt;
	int     i, k;
	UX_pathname nmtemp;
/*
.....this is not enough for 256 pen
.....chaged Yurong 2/11/98
*/
/* char temp[1320]; */
	char temp[5000];
	UX_pathname pathnm,buf;
	int     j, kcount;
	int     mode;

	uu_denter(UU_DTRC,(us,"uji_getpentb"));
	for (i=0; i<16; i++)
		for (j=0; j<3; j++)
			pentb[i][j] = (i+1);
	for (i=16; i<256; i++)
	{
		for (j=0; j<2; j++)
			pentb[i][j] = i+1;
		pentb[i][2] = 1;
	}
/*
.....we allow using pass in empty pen table name, in that case, we will using
.....default data
*/
	if (pentbnm[0] == '\0')
	{
		*fcount = 0;
		*pencount = 256;
	}
	*fcount = -1;
	*pencount = 0;

	strcpy(nmtemp,pentbnm);
/*
.....added for check directory not default
.....Yurong 9/9/97
*/
	mode = 0;
	ul_break_fname(nmtemp,pathnm,buf);
	if (pathnm[0] != '\0')
		ux_mk_chk_syspath(UU_NULL,UU_NULL,nmtemp,UU_NULL,"UJ_PEN_SUFFIX",
			&mode, pathnm, UX_PRTERRS);
	else
		ux_mk_chk_syspath(UU_NULL,"^UJ_PENTABLE",nmtemp,UU_NULL, "UJ_PEN_SUFFIX",
			&mode, pathnm, UX_PRTERRS);
	if (mode != (mode | UX_NEXISTS)) 
	  {
		if (ux_open_to_data(pathnm,"r+","STREAM","BINARY",&fd1,UX_PRTERRS)
			== UU_SUCCESS)
	     {
		   ux_get_os_filedesc(fd1,&fdpt,UX_PRTERRS);
		   if (ux_fgets(temp,1320,fdpt)==UX_EOF)        
			  {
				*fcount = 0;
				uu_dexit;       return;
			  }
			sscanf(temp, "%s %d", nmtemp, pencount);
/*       if (ux_fgets(temp,1320,fdpt)==UX_EOF)   */
		   if (ux_fgets(temp,5000,fdpt)==UX_EOF)        
			  {
				*fcount = 0;
				uu_dexit;       return;
			  }
			if(strchr(temp, '#')==NULL)
			{
	    *fcount = 0;
	    uu_dexit;   return;
			}
			k = 0;
			i = -1;
			kcount = *pencount;
/*
.....changed back
.....Yurong 9/9/97
*/
/* WHY put following lines?  */
/*
......256 pen need more space
*/
/*       while ((++i<256)&&(--kcount>=0)&&(k<1320)) */
			while ((++i<256)&&(--kcount>=0)&&(k<5000))
			  {
			   sscanf (&temp[k],"%d %d %d",&pentb[i][0],&pentb[i][1],&pentb[i][2]);
				while (temp[k++] != '#');
				uu_dprint(UU_DTRC,(us,"pentb=%d, %d, %d",
				pentb[i][0],pentb[i][1],pentb[i][2]));
			  }
			ux_close(fd1, UX_PRTERRS);
			*fcount = 1;
		  }
		else
			*fcount = 0;
	  }
	uu_dexit;
}       /* uji_getpentb */




/*********************************************************************
**    I_FUNCTION :  uji_pentbst(pentbdata,tempdata,index)
**       Put the pentable information in each form into the temporary
**              array.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uji_pentbst(pendata,tempdata,index)
int     pendata[256][3], tempdata[256][3];
int     index;

{
	int     i, j;

	for (i=0; i<256; i++,index++)
	  for (j=0; j<3; j++)
		 tempdata[index][j] = pendata[i][j];
}       /* uji_pentbst */




/*********************************************************************
**    I_FUNCTION :  uji_pentbpack(pentbdata.ptbnm,pentb,fcount)
**       Put the pentable information into the pentable file
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uji_pentbpack(tbnm,pentb,fcount)
char    *tbnm;
int     pentb[256][3];
int     fcount;

{
	int     fd1;
	UX_pathname temp, pathnm;
	int     i, pairno, jlen;
	int     len;
	char    *buf, *infor;
	int     mode, nb;
	int answer;
	char msg[200];

	buf = (char*) uu_malloc(5000*sizeof(char));
	infor = (char*) uu_malloc(5000*sizeof(char));
	infor[0] = '\0';
	for (i=0,pairno=0,jlen=0; i<256; i++)
		if (pentb[i][0] != -1)
		{
		  if (pentb[i][1]==-1)
			 pentb[i][1] = pentb[i][0];
		  pairno++;
		  sprintf(&infor[jlen],"%d %d %d#",pentb[i][0],pentb[i][1],
					  pentb[i][2]);
		  jlen = strlen(infor);
	    }

	strcpy(temp,tbnm);
/*
.....added for check directory not default
.....Yurong 9/9/97
*/
	mode = 0;
	ul_break_fname(temp,pathnm,buf);
	if (pathnm[0] != '\0')
		ux_mk_chk_syspath(UU_NULL,UU_NULL,temp,UU_NULL,"UJ_PEN_SUFFIX",
			&mode, pathnm, UX_PRTERRS);
	else
		ux_mk_chk_syspath(UU_NULL,"^UJ_PENTABLE",temp,UU_NULL, "UJ_PEN_SUFFIX",
			&mode, pathnm, UX_PRTERRS);
		
/*
.....added by Yurong
.....9/9/97
*/
	if (!(mode & UX_NEXISTS))
	{
		sprintf(msg, "File %s Exists, Overwrite?",pathnm);
		answer = ud_yesno(UU_NULL, msg, "File Exists");
		if (answer == 1) ux_delete(pathnm, UX_PRTERRS);
		else 
		{
			uu_free(buf);
			uu_free(infor); 
			return;
		}
	}
	if (ux_create_file(pathnm, 0660, UU_NULL, "STREAM", "BINARY",
			"UX_NOEXTRA", &fd1, UX_PRTERRS) == UU_SUCCESS)
	  {
		sprintf(buf,"%s %d\n%s\n",tbnm,pairno,infor);
		len = strlen(buf);
			nb = len;
			ux_write(fd1,buf,sizeof(char),&nb,UX_PRTERRS);
			ux_close(fd1,UX_PRTERRS);
	  }
	uu_free(buf);
	uu_free(infor); 
}       /* uji_pentbpack */



UD_FSTAT uj_noop()
{return(UD_OUTOK);}

/*********************************************************************
**    I_FUNCTION :  uj_copypentb()
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
/*
.....changed this function for use general function "file copy"
.....Yurong 9/16/98
*/
void uj_copypentb(pt_name, newname)
char* pt_name, *newname;
{
	int   pentb[256][3];
	int     fcount;
	int     pencount;

	uu_denter(UU_DTRC,(us, "uj_copypentb"));
	if (strlen(pt_name)== 0) 
	{
		uu_uerror1 (UJ_SUPPORT,29);
		return;
	}
	ux_add_ftype("UJ_PEN_SUFFIX", pt_name, UX_PRTERRS);
	ul_remove_quotes(pt_name);
	uji_getpentb(pt_name,pentb,&fcount,&pencount);
	if ( fcount <= 0 )
		uu_uerror1 (UJ_SUPPORT,2,pt_name);
	else
	  {
		if (strlen(newname)==0)
		{
			uu_uerror1 (UJ_SUPPORT,29);
			return;
		}
		fcount = -1;
		ux_add_ftype("UJ_PEN_SUFFIX", newname, UX_PRTERRS);
		ul_remove_quotes(newname);
		uji_pentbpack(newname,pentb,fcount);
	  }
	uu_dexit;
}       /* uj_copypentb */

