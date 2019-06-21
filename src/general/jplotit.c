/*********************************************************************
**    NAME         :  jplotit.c
**       CONTAINS:
**                              uji_savinfo(dsize,pfnm,dsize)
**                              uji_qksavinfo(dsize,pfnm,dp)
**                              uji_matchpen(pentb,pencount)
**                              uj_setpen(c1)
**                              uj_miplotting()
**                              uj_writefile()
**                              uj_change_plotter()
**                              uj_change_port
**                              uj_browse_plotfile
**                              uj_browse_setupfile
**                              uj_option
**                              uj_change_plottype
**                              uj_change_plotoutput
**                              uj_create_plot()
**                              uj_local_plot
**                              uj_quick_plot
**                              
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       jplotit.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 09:30:23
*********************************************************************/

#include "usysdef.h"
#include "uhep.h"
#include "uims.h"
#include        "dtypes.h"
#include "usysg.h"      
#include "udebug.h"
#include        "ginq.h"
#include "dinput.h"
#include        "plotfrm.h"
#include        "mxxx.h"        
#include "xenv1.h"
#include "driver.h"
/*
.....added by Yurong
..... 9/8/97
*/
#include "mdunits.h"
#include "mdraw.h"
#include "jplot.h"
#include <string.h>
#if UU_COMP == UU_WIN2K
#include <io.h>
#endif
#include <stdlib.h>
#include "view.h"
#include "udforms.h"
#include "udfdata.h"
#include "xfsys1.h"
#include "mdrwsize.h"
UU_LOGICAL      ud_yesno();

static  int     plotpen[257];
UU_LOGICAL      UJ_plot = UU_FALSE;
#if UU_DEBUG
char    us[120];
#endif
static int dum_create = 0;
static int pos = 1;
/*
.....variable for plot
.....added Yurong 8/29/97
*/
static int tmp_plot_output, tmp_plot_type;
static int plot_output=0;
static int plot_type=0;
static UU_REAL line_width = 1.0;
static int plotter_type = 0;
static int plot_size = 0;
static int bypass_mode = 1;
static int tmp_plotter;
static char port_text[80] = "HPLOT";
static char print_text[80] = "";
static UX_pathname uj_setupnm = " ";
static UX_pathname uj_plotfnm = " ";
static UX_pathname uj_drawnm = " ";
static UD_LIST drawing_name_list;
UX_pathname tmp_setupnm, tmp_plotfnm;
static int enter_draw = 0;
static int view_draw = 0;
static  UU_REAL tmp_width ;
static int tmp_size, tmp_mode;
static char tmp_port[80],tmp_print[80];

void uj_create_plot();
/* 
......0 is reserved for main form window
......use -1 when no option form displayed
*/
static int Ocancel,Ofrm=-1;
/*********************************************************************
**    I_FUNCTION : uj_writefile(fd,buf,len) 
**       Write a string in the file
**    PARAMETERS   
**       INPUT  : 
**          fd: file
**                              buf: string need write into file
**                              len: length of the string
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_writefile(fd,buf,len)
FILE  *fd;
char  buf[];
int   len;
{
   int icnt,nc,ipt, len1, len2;
   char sbuf[82];

   nc = len;
   ipt = 0;
	icnt = nc + pos;
	if (icnt == 81)
	{
		fprintf(fd, buf, len);
		fprintf (fd,"\n");
		pos = 1;
	} 
/*
.....changed by Yurong 8/4/97
*/
/*      else if (icnt < 80)  */
	else if (icnt <= 80) 
	{       
		fprintf(fd, buf, len);
		pos = icnt;
	}
	else  
	{                       
		len1 = 81 - pos;  
		strncpy (sbuf,buf,len1);
		sbuf[len1] = '\n';
/*
.....added end marker
.....Yurong 8/4/97
*/
		sbuf[len1+1] = '\0';
		fprintf (fd,"%s",sbuf);
		len2 = icnt - 81;
		strncpy (sbuf,&buf[len1],len2);
/*
.....added end marker
.....Yurong 8/4/97
*/
		sbuf[len2] = '\0';
		fprintf (fd,"%s",sbuf); 
		pos = len2; 
	}
	return(UU_SUCCESS);
}
/*********************************************************************
**    I_FUNCTION :  uji_savinfo(dsize,pfnm,dsize,pentb)
**       Save the current window, vport, vport normal, view up and view
**                      reference point information
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uji_savinfo(plotdata,pfnm,dsize,pentb)
PLOTFRM *plotdata;
UX_pathname     pfnm;
int     dsize;                  /* drawing size */
int pentb[256][3];
{
	int uj_writefile();
	Gwpoint3 *vrefpt;
	int     i;
	UU_REAL  x,y,xm,ym;
	FILE    *fd /* ,*fopen() */;
	char temp1[20];
	uu_denter(UU_GITRC,(us,"entering uji_savinfo"));
	fd = fopen(pfnm, "w");
	fprintf(fd, "%s;%s;%d;%s;\n",
			  plotdata->sfnm, plotdata->mdtype,
			  plotdata->formno, plotdata->pentbnm);
   for (i=0; i<256; i++)
      {
	 if (plotdata->pen[i][0] != '\0')
	 {
	    sprintf (temp1,"%s;", plotdata->pen[i]);
				uj_writefile(fd,temp1,strlen(temp1));
	 }
	 else
	 {  sprintf (temp1,"@;");
				uj_writefile(fd,temp1,strlen(temp1));
			}
      }
   fprintf(fd,"\n");
	pos = 1;
	fprintf(fd, "0 ");              /* not a quick plot */
/*
.....Save the actual size of the drawing
.....instead of the drawing size number
.....Convert units from Inch to CM
.....Bobby  -  7/17/91
*/
	um_get_drawing_extents(dsize,&x,&y,&xm,&ym);
	x = x * 2.54; y = y * 2.54;
/*
.....We need more prec
.....changed by Yurong
*/
	fprintf (fd, "%d %.9g %.9g\n", dsize,x,y);
/*      fprintf (fd, "%d %g %g\n", dsize,x,y); */
/*      fprintf(fd, "%d\n", dsize);*/     /* save the size of drawing */

	vrefpt = gqrefpt(0);
	fprintf(fd,"%g %g %g\n",vrefpt->x,vrefpt->y,vrefpt->z);

	/* Added the pen table description, to be read and displayed by
	   plottool. kathy */
	for (i=0; i<256; i++)
		{
			if (pentb[i][0] != -1)
			{
				sprintf (temp1,"%d %d %d#",pentb[i][0],pentb[i][1],pentb[i][2]);
				uj_writefile(fd,temp1,strlen(temp1));
			}
			else  
				continue;
		}
	fprintf(fd,"\n");
	pos = 1;
	fclose(fd);
	uu_dexit;
	return(UU_SUCCESS);
}       /* uji_savinfo  */

/*********************************************************************
**    I_FUNCTION :  uji_qksavinfo(dsize,pfnm,dp,pentb)
**       Save the current window, vport, vport normal, view up and view
**                      reference point information for the quick plot
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uji_qksavinfo(plotdata,pfnm,dp,pentb)
PLOTFRM *plotdata;
UX_pathname     pfnm;
Gdspsize         *dp;
int    pentb[256][3];
{
	int uj_writefile();
	Gwrect3 *window;
	Gnrect3 *vport;
	Gwpoint3 *vpn,*vup,*vrefpt;
	int     i;
	Gnrect *gr_area;
	int       curr_scrn;
	char temp1[20];
	FILE    *fd /*, *fopen()*/ ;
/*
.....Added 'vp'
.....Bobby  -  7/23/91
*/
	int j,inc;
	UV_vport vp;

	uu_denter(UU_GITRC,(us,"entering uji_qksavinfo"));
	fd = fopen(pfnm, "w");
	fprintf(fd, "%s;%s;%d;%s;\n",
			  plotdata->sfnm, plotdata->mdtype,
			  plotdata->formno, plotdata->pentbnm);
   for (i=0; i<256; i++)
      {
	      if (plotdata->pen[i][0] != '\0')
	 {
	    sprintf (temp1,"%s;", plotdata->pen[i]);
				uj_writefile(fd,temp1,strlen(temp1));
	 }
	else
			{
				sprintf (temp1,"@;");
				uj_writefile(fd,temp1,strlen(temp1));
			}
      }
   fprintf(fd,"\n");
	pos = 1;

/*
.....Output quick plot flag and
.....Number of view ports & Border flag
.....Bobby  -  7/23/91
*/
/*      fprintf(fd, "1\n"); */            /* a quick plot */
	inc = 0;
	for (i=0; i<UV_no_act_screens; i++)
		inc = inc + UV_act_screen[i].nvports;
	fprintf(fd,"1 %d",inc);
	for (i=0; i<UV_no_act_screens; i++)
	{
		for (j=0;j<UV_act_screen[i].nvports;j++)
		{
			uv_getvpid(UV_act_screen[i].vports[j],&vp);
			fprintf(fd," %d",vp.bord_on);
		}
	}
	fprintf(fd,"\n");

	/* fprintf(fd, "%d\n",dsize);  */
/*
.....Changed to only output transformations
.....only for each active viewport
.....Bobby  -  7/23/91
*/
/*      for (i=1; i<UG_MAXNTRAN; i++) */
	for (i=1; i<=inc; i++)
	  {
			/* Gwrect3 gqwindow3(xform) -- Inquire window 3D.               */
		window = gqwindow3(i);
		fprintf(fd,"%g %g %g %g %g %g\n",window->llf.x,window->llf.y,
		window->llf.z,window->urb.x,window->urb.y,window->urb.z);
			/* Gnrect3 gqvport3(xform) -- Inquire viewport 3D. */
		vport = gqvport3(i);
		fprintf(fd,"%g %g %g %g %g %g\n",vport->llf.x,vport->llf.y,vport->llf.z,
			vport->urb.x,vport->urb.y,vport->urb.z);
			/* Gwpoint3 gqvup3(xform) -- Inquire view up vector 3D.         */
		vup = gqvup3(i);
		fprintf(fd,"%g %g %g\n",vup->x,vup->y,vup->z);
			/* Gwpoint3 gqvpn3(xform) -- Inquire view plane normal 3D.      */
		vpn = gqvpn3(i);
		fprintf(fd,"%g %g %g\n",vpn->x,vpn->y,vpn->z);
			/* Gwpoint3 gqrefpt(xform) -- Inquire view reference point.     */
		vrefpt = gqrefpt(i);
		fprintf(fd,"%g %g %g\n",vrefpt->x,vrefpt->y,vrefpt->z);
	  }
	/*fprintf(fd, "0.0 0.0 1.0 1.0 %g 0.0\n",dp->device.y/dp->device.x); */
			/* get graphic area in NDC coordinate */
	curr_scrn = UD_curlayout.curr_screen;
	gr_area = &(UD_duimsdeflt.screen[curr_scrn].areas[UD_GRAF][0].posn);
	fprintf(fd, "%g %g 1.0 %g %g 0.0\n",gr_area->ll.x,gr_area->ll.y,
													gr_area->ur.x,gr_area->ur.y);

	/* Added the pen table description, to be read and displayed by
	   plottool. kathy */
   for (i=0; i<256; i++)
      {
	 if (pentb[i][0] != -1)
	 {
	    sprintf (temp1,"%d %d %d#",pentb[i][0],pentb[i][1],pentb[i][2]);
				uj_writefile(fd,temp1,strlen(temp1));
	 }
	 else
	    continue;
      }
   fprintf(fd,"\n");
	pos = 1;

/* -----                no need to save the paper size, this is asked in plottool --
	fprintf(fd, "0.0 0.0 1.0 %g %g 0.0\n", ppsize[dsize].x, ppsize[dsize].y);
-------  */
	fclose(fd);
	uu_dexit;
	return(UU_SUCCESS);
}       /* uji_qksavinfo        */

/*********************************************************************
**    I_FUNCTION :  uji_matchpen(pentb,pencount)
**       Mapping the logical pens to the physical pen table.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uji_matchpen(pentb,pencount)
int     pentb[256][3];
int     pencount;
{
	register int i, j;

	for (i=0; i<256; i++)           /* set default logical & physical pen matching */
		 plotpen[i] = i;
	for (i=0; i<pencount; i++)              /* matchs according to users definition */
		for (j=pentb[i][0]; j<=pentb[i][1]; j++)
			plotpen[j] = pentb[i][2];;
}       /* uji_matchpen */

/*********************************************************************
**    I_FUNCTION :  uj_setpen(c1)
**       Given a logical pen number, return the physical pen number.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_setpen(c1)
register int c1;
{
	if ((c1>0)&&(c1<257))           /* in the legal range */
		return (plotpen[c1]);
	else
		return(1);
}       /* uj_setpen */

/*********************************************************************
**    I_FUNCTION :  uj_miplotting()
**       return UU_TRUE if either quick plot or local plot is in progress.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_miplotting()
{
	return(UJ_plot);
}       /* uj_miplotting */


/*********************************************************************
**    S_FUNCTION     :  static uj_change_plotter(fieldno, val, stat)
**       Method called at 'Plotter' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT uj_change_plotter(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (tmp_plotter )
	{
		case 0:
		case 1: ud_setfrm_traverse_mask(Ofrm, 5, UU_TRUE);
				break;
		case 2:
		case 3: ud_setfrm_traverse_mask(Ofrm, 5, UU_FALSE);
				break;
	}
	return UD_FLDOK;                
}

/*********************************************************************
**   I_FUNCTION: OnCancel(fieldno,val,stat)
**      Callback function for the Cancel button.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCancel(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....User rejected the form
*/
	Ocancel = UU_FAILURE;
	ud_close_dispfrm(Ofrm);
	*fieldno = -1;
	Ofrm = -1;
	return(UD_FRMCLOSE);
}

/*********************************************************************
**    S_FUNCTION     :  uj_browse_plotfile(fieldno, val, stat)
**       Method called at 'Plot file browser' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT uj_browse_plotfile(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UX_pathname filename,ext,descrip;
	char *p, *ux_getenv(), ext1[UX_SUFFIX_LEN];
	int len;
	filename[0] = '\0';
	if (tmp_plot_output==0)
	{
		strcpy(descrip, "Plot Output Files (");
		strcpy(ext,"*.");
		p = ux_getenv("UJ_PLOT1_SUFFIX");
		if (p != UU_NULL)
		{
			strcpy(ext1,p);
			ul_remove_quotes(ext1);
			strcat(ext,ext1);
		}       
		else
		{
			strcat(ext,"pl1");
		}
		strcat(descrip, ext);
		strcat(descrip, ")");
		ud_get_filename("PL Files", "PL Files", ext, filename, &len,descrip, 0,
			UU_FALSE);
	}
	else if(tmp_plot_output==1)
		ud_get_filename("Plot File", "Plot File", "*.plt", filename, &len,
			"Plot Output Files(*.plt)", 0, UU_FALSE);
	if (filename[0]!='\0')
	{
		ud_update_answer(*fieldno-1, (int*)filename);
	}
	else
/*
.....canceled
*/
		*fieldno = -1;
	return UD_FLDOK;                
}
/*********************************************************************
**    S_FUNCTION     :  uj_browse_setupfile(fieldno, val, stat)
**       Method called at 'Setup file browser' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
UD_FSTAT uj_browse_setupfile(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname filename,ext,descrip;
	char *p, *ux_getenv(), ext1[UX_SUFFIX_LEN];
	int len;
	filename[0] = '\0';
	strcpy(descrip, "Plot Setup Files (");
	strcpy(ext,"*.");
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
	ud_get_filename("Setup File", "Setup File", ext, filename, &len,descrip, 1,
		UU_FALSE);
	if (filename[0]!='\0')
	{
		ud_update_answer(*fieldno-1, (int*)filename);
	}
	else
/*
.....canceled
*/
		*fieldno = -1;
	return UD_FLDOK;                
}

/*********************************************************************
**    S_FUNCTION     :  OnClose()
**       Method called at when option form is closed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose()
{
	if (Ocancel != UU_FAILURE)
	{
		plotter_type = tmp_plotter;
		plot_size = tmp_size;
		strcpy(port_text, tmp_port);
		strcpy(print_text, tmp_print);
		line_width = tmp_width;
		bypass_mode = tmp_mode;
	}
/*
.....Mark the form as closed
*/
	Ofrm = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static uj_option(fieldno, val, stat)
**       Method called at 'Option' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT uj_option(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int *ans[7];
	UU_KEY_ID   key;
	struct UM_drawing_rec drawing;
	UD_FSTAT uj_noop();
	static UD_METHOD methods[] = {
						uj_change_plotter, uj_noop,
						uj_noop, uj_noop, uj_noop, uj_noop, OnCancel, OnClose };
	static char called[] = { 
						  6, 6, 6, 6, 6, 6, 6, 6};
	static char traverse[] = { 1,1,0,0,1,1, 1};
	static char display[] = {1,1,1,1,1,1,1, 1};
#if UU_COMP == UU_WIN2K
	int i;
#else
	int len;
#endif
/*
......if option form already opened, do nothing
*/
	if (Ofrm != -1) return UD_FLDOK;
/*
.....init form value
*/
	tmp_plotter = plotter_type;
	tmp_size = plot_size;
	strcpy(tmp_port, port_text);
	strcpy(tmp_print, print_text);
	tmp_width = line_width;
	tmp_mode = bypass_mode;
	ans[0] = (int *)&tmp_plotter;
	ans[1] = (int *)&tmp_size;
	ans[2] = (int *)tmp_port;
	ans[3] = (int *)tmp_print;
	ans[4] = (int *)&tmp_width;
	ans[5] = (int *)&tmp_mode;
	ans[6] = UU_NULL;
/*
......remove black space from filename
......Yurong 2/15/98
*/
/*
.....for WinNT, we allow filename with spaces, 
.....only remove trailling spaces for WinNT
.....Yurong 1/17/02
*/
#if UU_COMP!=UU_WIN2K
	len = strlen(tmp_plotfnm);
	ul_strip_blanks(tmp_plotfnm, &len);
	len = strlen(tmp_setupnm);
	ul_strip_blanks(tmp_setupnm, &len);
#else
/*
.....we also need to remove preceding spaces
.....Yurong 3/1/02
*/
	for (i=0; i<strlen(tmp_plotfnm); i++)
	{
		if (tmp_plotfnm[i]!=' ') break;
	}
	strcpy(tmp_plotfnm, &(tmp_plotfnm[i]));
	for (i=strlen(tmp_plotfnm); i>0; i--)
	{
		if (tmp_plotfnm[i-1]==' ')
			tmp_plotfnm[i-1] = '\0';
		else
			break;
	}
	for (i=0; i<strlen(tmp_plotfnm); i++)
	{
		if (tmp_plotfnm[i]!=' ') break;
	}
	strcpy(tmp_plotfnm, &(tmp_plotfnm[i]));
	for (i=strlen(tmp_setupnm); i>0; i--)
	{
		if (tmp_setupnm[i-1]==' ')
			tmp_setupnm[i-1] = '\0';
		else
			break;
	}
#endif
/*
.....Then we need open option form
*/
	if (tmp_plotfnm==0)
	{
/*
.....Get the drawing information in order to
.....init option form data
*/
		if (drawing_name_list.answer!=NULL)
		{
			strcpy(uj_drawnm, drawing_name_list.answer);
			if (um_key_from_drawing_name(uj_drawnm,&key) == 0)
			{
				drawing.key = key;
				um_get_all_geom(&drawing, sizeof(drawing));
				if ((drawing.drwsize > 15)&&(drawing.drwsize <32))
				{
					tmp_plotter = 1;
					tmp_size = drawing.drwsize - 16;
				}
				else if ((drawing.drwsize > 31)&&(drawing.drwsize <48))
				{
					tmp_plotter = 2;
					tmp_size = drawing.drwsize - 32;
				}
				else if ((drawing.drwsize > 47)&&(drawing.drwsize <64))
				{
					tmp_plotter = 3;
					tmp_size = drawing.drwsize - 48;
				}
				else if ((drawing.drwsize > 63)&&(drawing.drwsize <80))
				{
					tmp_plotter = 0;
					tmp_size = drawing.drwsize - 60;
				}
				else
				{
					tmp_plotter = 0;
					tmp_size = drawing.drwsize;
				}
			}
		}       
	}
/*
.....according to plot form value user
.....chose from plot form and default 
.....value, set traverse value
.....Yurong 9/2/97
*/
#if UU_COMP!=UU_WIN2K
	if (tmp_plot_output==2)
	{
		traverse[2] = 1;
	}
	else
	{
		traverse[2] = 0;
	}
/*
......Print Que
*/
	if (tmp_plot_output==3)
	{
		traverse[3] = 1;
	}
	else
	{
		traverse[3] = 0;
	}
#else
	traverse[2] = 0;
	traverse[3] = 0;
#endif
/*
.....Bypass mode
*/
	if ((tmp_plotter==1)||(tmp_plotter==0))
		traverse[5] = 1;
	else
		traverse[5] = 0;
	Ofrm = ud_form_display1("jplotopt.frm", ans, ans, methods, called, display, traverse);
	if (Ofrm == -1) goto nofrm;
/*
.....Wait for the user to select a layer
.....or cancel the form
*/
	Ocancel = UU_SUCCESS;
	uw_dispfrm_wait(Ofrm);
	goto done;
nofrm:
	ud_wrerr("Could not load 'jplotopt.frm'.");
	Ocancel = UU_FAILURE;
/*
.....End of routine
*/
done:
	if (Ocancel != UU_FAILURE)
	{
		plotter_type = tmp_plotter;
		plot_size = tmp_size;
		strcpy(port_text, tmp_port);
		strcpy(print_text,tmp_print);
		line_width = tmp_width;
		bypass_mode = tmp_mode;
	}
	*fieldno = -1;
	if (Ocancel == UU_SUCCESS) return(UD_FLDOK);
	else return(UD_BADREQ);
}       


/*********************************************************************
**    S_FUNCTION     :  static uj_change_plottype(filedno, val, stat)
**       Method called at 'Plot Type' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/

static UD_FSTAT uj_change_plottype(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch(tmp_plot_type)
	{
		case 0: ud_set_traverse_mask(2, UU_TRUE);
				break;
		case 1: ud_set_traverse_mask(2, UU_FALSE);
				break;
	}                                       
	return UD_FLDOK;                
}

/*********************************************************************
**    S_FUNCTION     :  static uj_change_plotoutput(filedno, val, stat)
**       Method called at 'Plot Output' toggle field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT uj_change_plotoutput(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch(tmp_plot_output)
	{
		case 0: ud_set_traverse_mask(7, UU_FALSE);
/*
.....close the option dialog
*/
				if (Ofrm!=-1)
				{
					Ocancel = UU_FAILURE;
					ud_close_dispfrm(Ofrm);
					Ofrm = -1;
				}
				ud_set_traverse_mask(3,UU_TRUE); 
				ud_set_traverse_mask(4,UU_TRUE); 
				break;          
		case 1: 
/*
.....update the option dialog
*/
				if (Ofrm>0)
				{
					ud_setfrm_traverse_mask(Ofrm, 3, UU_FALSE);
					ud_setfrm_traverse_mask(Ofrm, 4, UU_FALSE);
				}
				ud_set_traverse_mask(3, UU_TRUE);
				ud_set_traverse_mask(4,UU_TRUE); 
				ud_set_traverse_mask(7,UU_TRUE); 
				break;
		case 2:
				if (Ofrm>0)
				{
					ud_setfrm_traverse_mask(Ofrm, 3, UU_TRUE);
					ud_setfrm_traverse_mask(Ofrm, 4, UU_FALSE);
				}
				ud_set_traverse_mask(3, UU_FALSE);
				ud_set_traverse_mask(4, UU_FALSE);
				ud_set_traverse_mask(7,UU_TRUE); 
				break;
		case 3:
				if (Ofrm>0)
				{
					ud_setfrm_traverse_mask(Ofrm, 3, UU_FALSE);
					ud_setfrm_traverse_mask(Ofrm, 4, UU_TRUE);
				}
				ud_set_traverse_mask(3, UU_FALSE);
				ud_set_traverse_mask(4, UU_FALSE);
				ud_set_traverse_mask(7,UU_TRUE); 
				break;
	}                                       
	return UD_FLDOK;                
}

/*********************************************************************
**    I_FUNCTION :  uj_create_plot()  
**                      create a plot .
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uj_create_plot()
{
	char  *p, *ux_getenv();
	UD_FSTAT uj_noop();
	char *indx;
	int *ans[8];
#if UU_COMP == UU_WIN2K
	UX_pathname localdir,envstr;
#endif
	UX_pathname fullname, dum1, dum2, plotfile;
	char bypass_str[5], optstr[80], *tname;
	char *tempnam();
	int len, status, mode;
	UU_KEY_ID   key;
	char com[UX_MAX_PATH_LEN+40];
	char size[10], type[10];
	char command[100];
	static UD_METHOD methods[8] = {
						uj_change_plottype, uj_change_plotoutput, uj_noop, 
						uj_noop, uj_browse_plotfile, uj_noop, 
						uj_browse_setupfile, uj_option};
	static char called[] = { 
						6, 6, 6, 6, 6, 6, 6, 6};
	static char traverse[] = { 1,1,1,1,1,1,1,0};
	static char display[] = {1,1,1,1,1,1,1,1,1};
	static int option = 0;
#if UU_COMP == UU_WIN2K
	int i;
#endif
	p = ux_getenv("UJ_PLOT_SETUP_FILE"); 
	if (p != UU_NULL)
	{
		strcpy(uj_setupnm, p);
	}       
	else
	{
		uj_setupnm[0] = '\0';
	}
/*
......remove it for list drawing in any case
......Yurong 2/13/98
*/
	drawing_name_list.item =
						(char **) um_get_drawing_name(&(drawing_name_list.num_item));
/*
.....if no drawing, still display empty drawing list
.....because user can use quick plot
*/
	if (drawing_name_list.num_item == 0)
	{
		drawing_name_list.num_item = 1;
/*
......we need malloc spaces. If we use '=' to assign a char, can't free it later
......yurong
*/
		drawing_name_list.item = (char **) uu_malloc(1*sizeof(char *));
		drawing_name_list.item[0] = (char *) uu_malloc(8 * sizeof(char));
		strcpy(drawing_name_list.item[0], " ");
		drawing_name_list.answer = NULL;
	}
	if (uj_drawnm!=NULL)
	{
		len = strlen(uj_drawnm)+1;
		drawing_name_list.answer = (char *) uu_malloc(len * sizeof(char));
		strcpy (drawing_name_list.answer, uj_drawnm); 
	}
	strcpy(tmp_plotfnm, uj_plotfnm);
	strcpy(tmp_setupnm, uj_setupnm);
	tmp_plot_type = plot_type;
	tmp_plot_output = plot_output;
	ans[0] = (int *)&tmp_plot_type;
	ans[1] = (int *)&tmp_plot_output;
	ans[2] = (int *)&drawing_name_list;
	ans[3] = (int *)tmp_plotfnm;
	ans[4] = (int *)&option;
	ans[5] = (int *)tmp_setupnm;
	ans[6] = (int *)&option;
	ans[7] = (int *)&option;
form_again:;
	if ((tmp_plot_output==2)||(tmp_plot_output==3))
	{
		traverse[3] = 0;
		traverse[4] = 0;
	}
	else
	{
		traverse[4] = 1;
		traverse[3] = 1;
	}
	if (tmp_plot_type == 0)
		traverse[2] = 1;
	else
		traverse[2] = 0;
	if (tmp_plot_output == 0)
		traverse[7] = 0;
	else 
		traverse[7] = 1;

	status = ud_form1("jplot.frm", ans, ans, methods, called, display, traverse);
/*
......if form cancel, return
*/
	if (status == -1)
	{
		ud_free_flist(&drawing_name_list);
		return;
	}
/*
.....For plot file or plotiter output
.....We need a dum PL file first
.....Yurong 8/29/97
*/
/*
......remove black space from filename
......Yurong 2/15/98
*/
/*
.....for WinNT, we allow filename with spaces, 
.....only remove trailling spaces for WinNT
.....Yurong 1/17/02
*/
#if UU_COMP!=UU_WIN2K
	len = strlen(tmp_plotfnm);
	ul_strip_blanks(tmp_plotfnm, &len);
	len = strlen(tmp_setupnm);
	ul_strip_blanks(tmp_setupnm, &len);
#else
/*
.....we also need to remove preceding spaces
.....Yurong 3/1/02
*/
	for (i=0; i<strlen(tmp_plotfnm); i++)
	{
		if (tmp_plotfnm[i]!=' ') break;
	}
	strcpy(tmp_plotfnm, &(tmp_plotfnm[i]));
	for (i=strlen(tmp_plotfnm); i>0; i--)
	{
		if (tmp_plotfnm[i-1]==' ')
			tmp_plotfnm[i-1] = '\0';
		else
			break;
	}
	if ((strlen (tmp_plotfnm)==0) && 
			(tmp_plot_output!=2)&&(tmp_plot_output!=3))
	{
		ud_wrerr("You must enter a plot file name!");
		goto form_again;
	}
/*
.....we also need to remove preceding spaces
.....Yurong 3/1/02
*/
	for (i=0; i<strlen(tmp_setupnm); i++)
	{
		if (tmp_setupnm[i]!=' ') break;
	}
	strcpy(tmp_setupnm, &(tmp_setupnm[i]));
	for (i=strlen(tmp_setupnm); i>0; i--)
	{
		if (tmp_setupnm[i-1]==' ')
			tmp_setupnm[i-1] = '\0';
		else
			break;
	}
#endif
	strcpy(uj_plotfnm, tmp_plotfnm);
	strcpy(uj_setupnm, tmp_setupnm);
	plot_type = tmp_plot_type;
	plot_output = tmp_plot_output;
	if (plot_output != 0)
	{
/*
.....don't use tmpnam because it may use name such as '/sf.1'
.....which will confusing when in some function
.....use tempnam instead because it will default default name
.....and prefix
.....Yurong
*/
/*
		tmpnam(plotfile);
*/
/*
.....in WINDOWS, tempnam function's dir could replaced by "TMP" directory defined
.....by window, The "TMP" directory could have "shortcut" such as 
....."C:\Ducume~1\Yurong\Local~1\tmp" but the we can't open file in
.....those shortcut directory. SO I reset temp directory to the working
.....directory  
*/
#if UU_COMP == UU_WIN2K
		getcwd(localdir,UX_MAX_PATH_LEN);
		sprintf (envstr, "TMP=%s", localdir);
		_putenv(envstr);
		tname = tempnam(localdir, "plot");
#else
		tname = tempnam("c:\\tmp", "plot");
#endif
		strcpy(plotfile, tname);
		dum_create = 1;
	}
	else
		strcpy(plotfile, uj_plotfnm);
	if (strlen(plotfile) == 0)              
	{
		uu_uerror1 (UJ_SUPPORT, 13);
		ud_free_flist(&drawing_name_list);
		return;
	}
	if (plot_output==0)
	{
		indx = strstr(plotfile, ".pl1");
		if (indx!=NULL)
			*indx = '\0';
	}
	if (plot_type == 0)
	{
		if (drawing_name_list.answer==NULL)
		{
			uu_uerror1 (UJ_SUPPORT, 4);
			ud_free_flist(&drawing_name_list);
			return;
		}
		strcpy(uj_drawnm, drawing_name_list.answer);
		if (um_key_from_drawing_name(uj_drawnm,&key) != 0)
		{
			uu_uerror1 (UJ_SUPPORT,11,uj_drawnm);
			ud_free_flist(&drawing_name_list);
			return;
		}
		else
		{
			ud_prmerr("Starting to create plot file.");
			if (uj_local_plot(&key, uj_setupnm, plotfile)!=1)
			{
				ud_free_flist(&drawing_name_list);
				return ;        
			}
		}
	}
	else 
	{
		ud_prmerr("Starting to create plot file.");
		if (uj_quick_plot(uj_setupnm, plotfile)!=1)
		{
			ud_free_flist(&drawing_name_list);
			return ;
		}
	}
/*
.....if output to plot file or Plotter
.....we need call PLOT process
*/
	if (plot_output != 0)
	{
		strcpy(command, "nclplot");
		if (plotter_type == 0) strcpy (type, "7475");
		else if (plotter_type == 1) strcpy (type, "7580");
		else if (plotter_type == 2) strcpy (type, "ps");
		else strcpy (type, "1043");
		strcpy(size, UM_drawing_size_name[plot_size]);
		if (bypass_mode == 1)
			strcpy(bypass_str, "ON");
		else
			strcpy(bypass_str, "OFF");
/*
.....Output to port
*/
/*
.....Output to disk file
*/
		optstr[0] = '\0';
		if (plot_output==1)
			sprintf(optstr,"-d=%s",uj_plotfnm);
#if UU_COMP!=UU_WIN2K
		else if (plot_output==2)
		{
			p = ux_getenv(port_text, UX_PRTERRS);
			if (p!=NULL)
				sprintf(optstr,"-p=%s",p);
			else
				sprintf(optstr,"-p=%s",port_text);
		}
/*
.....Output to print que
*/
		else
		{
			if (print_text[0] == '\0')
				strcpy(optstr,"-q");
			else
				sprintf (optstr,"-q=%s",print_text);
		}
#else
		else
			strcpy(optstr,"-q");
#endif
/*
......should run Batch
*/
		sprintf (com, "%s %s -t=%s %s -s=%s -w=%f -y=%s", command,plotfile,
						 type, optstr, size, line_width, bypass_str);
		ul_spawn(com, 0);
/*
.....disable plotting
*/
		UJ_plotting = UU_FALSE;
/*
.....delete the dumm file
*/
		if (dum_create==1)
		{
			sprintf(dum1, "%s.pl1", plotfile);
			sprintf(dum2, "%s.pl2", plotfile);
			mode = 0;
			status = ux_mk_chk_syspath(UU_NULL, UU_NULL, dum1, UU_NULL,
			  UU_NULL, &mode, fullname, UX_PRTERRS);
			if (!(mode & UX_NEXISTS))
				ux_delete(fullname, UX_PRTERRS);
			mode = 0;
			status = ux_mk_chk_syspath(UU_NULL, UU_NULL, dum2, UU_NULL,
			  UU_NULL, &mode, fullname, UX_PRTERRS);
			if (!(mode & UX_NEXISTS))
				 ux_delete(fullname, UX_PRTERRS);
			dum_create = 0;
		}
	}
	if ((enter_draw ==1)||(view_draw==1))
	{
		udm_exit_drawing();
		enter_draw = 0;
	}
	if (view_draw==1)
	{
		udm_enter_drawing();
		view_draw = 0;
	}
	ud_prmerr("Finished creating plot file.");
	ud_free_flist(&drawing_name_list);
}
				
		
/*********************************************************************
**    I_FUNCTION :  uj_local_plot(key, setup_file, plotfile)  
**       Local plot - put all the parts in a user specified drawing into
**                      a plot file.
**    PARAMETERS   
**       INPUT  : 
**          key: drawing ID
**                              setup_file: setup filename
**                              plotfile: plotfile name                                         
**       OUTPUT :  
**          output
**    RETURNS      : 1: if success
**                                                      0: faid
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uj_local_plot(key, setup_file, plotfile)
char *setup_file, *plotfile;
UU_KEY_ID *key;
{
	char buf[120], msg[80];
	UU_KEY_ID active_drawing;
	PLOTFRM plotdata;
	int     fcount;
	int     pentb[256][3], pencount, strids[2];
	struct UM_drawing_rec   drawing;
	UX_pathname fullname, homedir, penfile, plfile, buf1;
	int status, mode, answer;
/*
.....we allow the empty setup file now
*/
	uji_getsetup(setup_file,&plotdata,&fcount);
	uji_getpentb(plotdata.pentbnm,pentb,&fcount,&pencount);
	sprintf(buf,"%s.pl1", plotfile);
	if (ux_access0(buf,0) == UU_SUCCESS)    /* file already exist */
	{
		strcpy(homedir, "^UX_HOMEDIR");
		mode = 0;
		ul_break_fname(buf,fullname,buf1);
		if (fullname[0] != '\0')
			status = ux_mk_chk_syspath(UU_NULL, UU_NULL, buf, UU_NULL,
				UU_NULL, &mode, fullname, UX_PRTERRS);
		else
			status = ux_mk_chk_syspath(UU_NULL, homedir, buf, UU_NULL,
				UU_NULL, &mode, fullname, UX_PRTERRS);
		sprintf(msg, "File %s Exists, Overwrite?", fullname);
		if (dum_create==0)
		{
			answer = ud_yesno(UU_NULL, msg, "File Exists");
		}
		else
			answer = 1;
		if (answer == 1) ux_delete(fullname, UX_PRTERRS);
		else return 0;
	}
	uji_matchpen(pentb,pencount);
	um_get_drawing_to_plot(*key, &drawing, sizeof(drawing));
	if (drawing.no_member == 0)
	if (ud_yesno(0, uu_uprompt0(UJ_SUPPORT,19), "Question?") == UU_FALSE)
		return 0;
	UJ_plot = UU_TRUE;
	sprintf(penfile, "%s.pl2", plotfile);

	/* Add pentb to .pl2 file; to be read by plottool. kathy */
	uji_savinfo(&plotdata,penfile,drawing.drwsize,pentb);
/*
.....if not in drawing mode
.....enter drawing and view drawing
*/
	if (UM_2d3d_mode != UM_2D)
	{
		enter_draw = 1;
		udm_enter_drawing();
		um_view_drawing(&drawing); 
	}
	else
/*
.....if already in drawing mode
*/
	{
		active_drawing = ur_get_drwmdl_curdrw();
/*
.....if no drawing display
.....we need view drawing because
.....the active screen does not store
.....drawings, set flag to exit the view later
*/
		if (active_drawing == 0)
		{
			um_view_drawing(&drawing);
			view_draw = 1;
		}
	}

	strids[0] = gnseg();
	gdeactivatews(UD_ksws);
	gcreateseg(strids[0]);
	um_plot_drawing(*key);
	gcloseseg();
	sprintf(plfile, "%s.pl1", plotfile);
	gopenarcf(plfile);
	garchstr(1,strids);
	gclosarcf();
	gdeleteseg(strids[0]);
	gactivatews(UD_ksws);
	UJ_plot = UU_FALSE;
	return 1;
}       
/*********************************************************************
**    I_FUNCTION :  uj_quick_plot()  
**       all the parts on the current window are saved in the plot file.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS  1: success
**                                      0: failed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uj_quick_plot(setupnm, plotfnm)
UX_pathname setupnm, plotfnm;
{
	char buf[120], msg[80];
	UX_pathname pfnm;
	PLOTFRM plotdata;
	int     fcount;
	int     pentb[256][3], pencount, strids[2];
	Gdspsize         *dp;
	UX_pathname fullname, homedir,buf1;
	int status, mode, answer;

/*
.....we allow the empty setup file now
*/
	uji_getsetup(setupnm,&plotdata,&fcount);
	uji_getpentb(plotdata.pentbnm,pentb,&fcount,&pencount);
	sprintf(buf,"%s.pl1", plotfnm);
	if (ux_access0(buf,0) == UU_SUCCESS)    /* file already exist */
	{
		strcpy(homedir, "^UX_HOMEDIR");
		mode = 0;
		ul_break_fname(buf,fullname,buf1);
		if (fullname[0] != '\0')
		status = ux_mk_chk_syspath(UU_NULL, UU_NULL, buf, UU_NULL,
				UU_NULL, &mode, fullname, UX_PRTERRS);
		else
			status = ux_mk_chk_syspath(UU_NULL, homedir, buf, UU_NULL,
				UU_NULL, &mode, fullname, UX_PRTERRS);
		sprintf(msg, "File %s Exists, Overwrite?", fullname);
		if (dum_create==0)
		{
			answer = ud_yesno(UU_NULL, msg, "File Exists");
		}
		else
			answer = 1;
		if (answer == 1) ux_delete(fullname, UX_PRTERRS);
		else return 0;
	}
	UJ_plot = UU_TRUE;
	uji_matchpen(pentb,pencount);
	sprintf(pfnm, "%s.pl2", plotfnm);
	dp = gqdisplaysize(UD_ksws);  

	/* Add pentb to .pl2 file. kathy */
	uji_qksavinfo(&plotdata,pfnm,dp,pentb);
	strids[0] = gnseg();
	gdeactivatews(UD_ksws);
	gcreateseg(strids[0]);
	uv_plot_screen();
	gcloseseg();
	sprintf(pfnm, "%s.pl1", plotfnm);
	gopenarcf(pfnm);
	garchstr(1,strids);
	gclosarcf();
	gdeleteseg(strids[0]);
	gactivatews(UD_ksws);
	UJ_plot = UU_FALSE;
	return 1;
}

