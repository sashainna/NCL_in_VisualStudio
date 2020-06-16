/**********************************************************************
**    NAME         :  tplot.c
**       CONTAINS:
**				utp_plot
**				utp_setupok
**              uti_setup1
**				utp_setws
**				utp_adjust_plotsize
**				utp_qksetws
**				utp_adjustbound
**				utp_setlinwt
**				utp_set_dashlen
**          utp_ready
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tplot.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:13:19      
*********************************************************************/
#include<string.h>
#define MPGM 1
#include "ustdio.h"
#include "udebug.h"
#include "gtbl.h"
#include "g.h"
#include "usysdef.h"
#include "dinput.h"
#include "gtbldef.h"
#include "gdidd.h"
#include "ualloc.h"
#include "modef.h"
#include "xenv1.h"
/*
.....we need it for W2K version when plot alone
.....that mean we create it plot util, not for use 
.....this file for NCL
.....Yurong
*/
/*
.....seems like the plot util need define UM_MPGM
.....anyway because jplotsz.o we used not define it
*/
#define	UM_MPGM 
#if (UU_COMP != UU_WIN2K)
#include "mdrwsize.h"
#include "tplot.h" 
#include "mfort.h" 
#else
/*
#ifdef PLOT_ALONE
#define	UM_MPGM 
*/
#include "mdrwsize.h"
#include "tplot.h" 
#include "mfort.h"
/*
#undef	UM_MPGM
#endif
*/
#endif
#undef	UM_MPGM
#include "nclver.h"
/* 
.....Added to scale linelen in ug_lnstyle if neccessary.
.....9/1/99 JLS
*/
extern UU_LOGICAL  T_PLOT;					 
extern UU_REAL line_scale;					 

char	*ux_getenv();
extern Glntype	ug_lntypes[];
extern int pts;
extern int wsplotdx, wsplotdy;
extern int uw_758_longplot ;
extern int uw_758fd_page ;
/*********************************************************************
**    I_FUNCTION :  utp_plot(fd1,drwsize,qkplot,P1P2_control,plotfnm)
**       Plot the specified plotfile which is associated with the
**			required condition.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

utp_plot(fd1,drwsize,qkplot,P1P2_control,plotfnm)
FILE	*fd1;
int	drwsize;
int	qkplot;
UU_LOGICAL	P1P2_control;
/* change from float */
char	*plotfnm;

{
	char	pfnm[200];
	Gws *gopenws();
	int i;
	Gerror gcreateseg();
	Gws *id;
	int	num_strs, str_ids[6];
	extern  int	 ws7580, ws104, ws7475, wsps;
	Gnpoint p1,p2;
	void utp_setws(), utp_qksetws(), utp_setlinwt(), utp_set_dashlen();

	pts = UU_FALSE;
	T_PLOT = UU_TRUE;
	sprintf(pfnm, "%s.pl1", plotfnm);
	gdelallsegs();
	gopenarcf(pfnm);
	gqarchids(&num_strs, str_ids);
	gretrstr(num_strs, str_ids);
	uj_setps(&plotopts.size,PLOT_size);

	PAPER_size[0] = PLOT_size[0];
	PAPER_size[1] = PLOT_size[1];
	if (strcmp(plotopts.type,"7580")==0)
		id = gopenws(plotopts.port,&ws7580); 
	else if (strcmp(plotopts.type,"1043")==0)
			  id = gopenws(plotopts.port,&ws104);
	else if (strcmp(plotopts.type,"7475")==0)
		id = gopenws(plotopts.port,&ws7475); 
	else if (strcmp(plotopts.type,"ps")==0 || strcmp(plotopts.type,"PS")==0)
		id = gopenws(plotopts.port,&wsps); 
	gactivatews(id);
	if (qkplot)
	{
		utp_qksetws(fd1,id,P1P2_control);
	}
	else
	{
		utp_setws(fd1,id,drwsize,P1P2_control);
	}

	if (utp_ready())
	  {
		utp_setlinwt(id,plotopts.linewt);
		utp_set_dashlen(id);
/*
.....Draw borders
.....Bobby  -  7/23/91
*/
start:
		for (i=0; i<TP_nviews; i++)
		{
			if (TP_border[i] == UU_TRUE)
			{
				p1.x = BORDER_pt[i].llf.x;
				p1.y = BORDER_pt[i].llf.y;
				p2.x = BORDER_pt[i].urb.x;
				p2.y = BORDER_pt[i].llf.y;
				ug_linndc(&p1,&p2);
				p1.x = BORDER_pt[i].urb.x;
				p1.y = BORDER_pt[i].llf.y;
				p2.x = BORDER_pt[i].urb.x;
				p2.y = BORDER_pt[i].urb.y;
				ug_linndc(&p1,&p2);
				p1.x = BORDER_pt[i].urb.x;
				p1.y = BORDER_pt[i].urb.y;
				p2.x = BORDER_pt[i].llf.x;
				p2.y = BORDER_pt[i].urb.y;
				ug_linndc(&p1,&p2);
				p1.x = BORDER_pt[i].llf.x;
				p1.y = BORDER_pt[i].urb.y;
				p2.x = BORDER_pt[i].llf.x;
				p2.y = BORDER_pt[i].llf.y;
				ug_linndc(&p1,&p2);
			}
		}

		gredrawsegws(id);
/*
.....Changed because old UG_DRASLINE did not
.....accept standard arguments
.....This is sloppy fix it in next release
.....Bobby  -  7/24/91
*/
/*		(*(ug_gksstli.wsopen[*id].connid)[UG_DRASLINE])();*/
		if (strcmp(plotopts.type,"7580")==0) uw_7580dmpline();
		else if (strcmp(plotopts.type,"7475")==0) uw_7475dmpline();
		else if (strcmp(plotopts.type,"1043")==0) uw_104dmpline();
		else if (strcmp(plotopts.type,"ps")==0 || strcmp(plotopts.type,"PS")==0)
			uw_psdmpline();
		/* added kathy */
 		if (pts)
			(*(ug_gksstli.wsopen[*id].connid)[UG_DRASPNTS])();
/*
.....For long axis plot
.....We need send the data
.....again if we have not
.....finish all the frames.
.....added by yurong
*/
		uw_758fd_page--;
		if ((uw_758_longplot==1)&&(uw_758fd_page>=0))
		{
			uw_758advan_frame();
/*
.....reset buffer pointer
.....Yurong 10/12/98
*/
			uw_758resetbuf();
			goto start;
		}
		gdeactivatews(id);
		gclosews(id);					/* close workstation */
		gclosarcf(pfnm);
/*
.....use utp_strout call which is for both Motif and Textual version
.....Yurong 8/3/98
*/
		utp_strout("\n*** PLOTTING FINISHED ***\n");
	  }
	else
 	  {
		gdeactivatews(id);
		gclosews(id);					/* close workstation */
		gclosarcf(pfnm);
/*
.....use utp_strout call which is for both Motif and Textual version
.....Yurong 8/3/98
*/
		utp_strout("\n*** PLOTTING ABORTED ***\n");
	  }
	fflush(stdout);
}	/* main */




/*********************************************************************
**    I_FUNCTION :  setupok(fd1,drwsize,qkplot,control)
**			bring up the setup file and ask for the continuation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

utp_setupok(fd1,drwsize,qkplot,control)
FILE	*fd1;
int	*drwsize;
UU_LOGICAL	*qkplot;
UU_LOGICAL	*control;
{
	char ch;
	void uti_setup1();
/*
.....Get the plot information from the '.pl2' file
*/
	uti_setup1(fd1,drwsize,qkplot);
	*control = UU_TRUE;
/*
.....Ask if controlled by 'p1/p2' if an HP plotter and
.....in interactive mode.
*/
	if (strcmp(plotopts.type,"1043") == 0 || !(plotopts.flag & INTER) ||
		plotopts.diskfnm[0] != '\0')
	{
		ch = 'n';
	}
	else
	{
#if (UU_COMP != UU_WIN2K)
		if (Motif_Plot==1)
		{
			if (utp_mfcontrol())
				ch = 'y';
			else
				ch = 'n';
		}
#else
		if (WNT_Plot==1)
		{
			ch = 'n';
		}
#endif
		else
		{
			printf("\nIs plot size controlled by p1 and p2? (y/n) ");
			fflush(stdout);
			scanf("%c", &ch);
		}
	}
/*
.....Let everybody know the physical paper size selection
*/
	strcpy (TP_size,"P1P2");
/*
.....Get the paper size if in Interactive mode
*/
	if ((ch!='y')&&(ch!='Y'))
	{
		*control = UU_FALSE;
		if ((plotopts.flag & INTER)&&(Motif_Plot!=1))
		{
#define	dnm	UM_drawing_size_name
			printf("   1. %-8s 2. %-8s 3. %-6s   4. %-6s   5. %-6s\n",dnm[0],dnm[1],
					 dnm[2],dnm[3],dnm[4]);
			printf("   6. %-8s 7. %-8s 8. %-6s   9. %-6s  10. %-6s\n",dnm[5],dnm[6],
					  dnm[7],dnm[8],dnm[9]);
			printf("  11. %-8s12. %-6s\n",dnm[10],dnm[11]);
			printf("\nEnter the choice of the plot size: ");
  			fflush(stdout);
			scanf("\n%d",&plotopts.size);
			plotopts.size = plotopts.size - 1;
/*
.....increase plotopts.size up to 16
.....Yurong 8/5/97
*/
			if ((plotopts.size < 0) || (plotopts.size > 15))
	  		{
				printf ("Illegal plot size\n");
				return(UU_FALSE);
	  		}
/*
.....Let everybody know the physical paper size selection
*/
		}
		strcpy (TP_size,dnm[plotopts.size]);
	}
	return(UU_TRUE);

}	/* utp_setupok */

/*********************************************************************
**    I_FUNCTION :  uti_setup1(fd1,drwsize,qkplot)
**			Bring up the setup file, find out the drawing size and whether
**			it is a quick plot or not.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uti_setup1(fd1,drwsize,qkplot)
FILE	*fd1;
int	*drwsize;
UU_LOGICAL	*qkplot;

{
	char	temp[512];
	char  templ[1000];
	char	buf[80];
	char tmp_str[300];
	int	num, i, j, ind;
	int	qk;
/*
...Change pen table to hold 256 pen
...Yurong
*/	
	char	penstk[256][20];  
	float x,y;

	/* Added for version number. kathy */

/*
.....Get and list the plot file setup variables (.pl2)
*/
/*
.....if Motif version, always display list info in status window
.....Yurong
*/
	if ((plotopts.flag & LIST)||(Motif_Plot==1))
	{
/*
.....use utp_strout call which is for both Motif and Textual version
.....Yurong 8/3/98
*/
		sprintf (tmp_str, "\nNCL CADPLOT Program Version  %6.3f\n\n",
						NCL_version);
		utp_strout (tmp_str);
		utp_strout
			("**************************************************************\n");
		utp_strout ("                      PLOTTER SETUP\n\n");
	}
	i = 0;
/*
.....Read a line from the '.pl2' file
*/
	while ((temp[i++]=getc(fd1))!='\n');
/*
.....Display Setup file
*/
	if ((plotopts.flag & LIST)||(Motif_Plot==1))
	{
		ind = -1;
		for (i=0; temp[++ind]!=';'; i++) buf[i] = temp[ind];
		buf[i] = '\0';
/*
.....use utp_strout call which is for both Motif and Textual version
.....Yurong 8/3/98
*/
/*		printf("     Setup File Name:  %s\n", buf); */
		sprintf(tmp_str, "     Setup File Name:  %s\n", buf);
		utp_strout (tmp_str);
/*
.....Display Media type
*/
		for (i=0; temp[++ind]!=';'; i++) buf[i] = temp[ind];
		buf[i] = '\0';
		sprintf(tmp_str, "     Media Type:       %s\n", buf);
		utp_strout (tmp_str);
/*
.....Display Form number
*/
		for (i=0; temp[++ind]!=';'; i++) buf[i] = temp[ind];
		buf[i] = '\0';
		sscanf(buf, "%d", &num);
		sprintf(tmp_str, "     Form Number:      %d\n", num); 
		utp_strout (tmp_str);
/*
.....Display Pen table
*/
		for (i=0; temp[++ind]!=';'; i++) buf[i] = temp[ind];
		buf[i] = '\0';
		sprintf(tmp_str, "     Pen Table Name:   %s\n\n", buf);
		utp_strout (tmp_str);
		utp_strout ("                     Pen Holder Setup\n");
		utp_strout ("   Pen Number Description:   Pen Number Description:\n");
	}
/*
.....Read the next line from the '.pl2' file
*/
	templ[0] = '\0';
	while(1)
	{	i = 0;
		while ((temp[i++]=getc(fd1))!='\n');
		temp[i] = '\0';
		if(strchr(temp,';')==NULL)
			break;
		else 
		{	temp[80] = '\0';
			strcat(templ, temp);
		}
	}
	for (i=0;i<256;i++) penstk[i][0] = '\0';
	ind = 0;
	for (i=0; i<256; i++, ind++)
	{
		if(templ[ind]=='\0' || templ[ind] == '\n') break;
		for (j=0; templ[ind]!=';' && templ[ind]!='\n'; j++, ind++) 
			penstk[i][j] = templ[ind];
		penstk[i][j] = '\0';
		if (penstk[i][0] == '@') 
			penstk[i][0] = '\0';
	}
	if ((plotopts.flag & LIST)||(Motif_Plot==1))
	{
		for (i=0; i<8; i++)
		{
			sprintf(tmp_str, "       %1d: %-22s %1d: %-22s\n",
					 i+1,penstk[i],i+9,penstk[i+8]);
			utp_strout (tmp_str);
		}
		utp_strout ("**************************************************************\n\n");
	}
	num = sscanf (temp,"%d %d %f %f",&qk,drwsize,&x,&y);
/*
.....This is a drawing
*/
	*qkplot = qk;
	if (!(*qkplot))
	{

/*
.....Change max size type to 16
.....for each plotter
.....Yurong 7/31/97
*/
/*
.....	Plotter model  drwsize range
.....	 HP 7580        0-15
.....	 HP 7475       16-31
.....	 PostScript    32-47
.....	 Calcomp 1045  48-63
.....  Generic       64-79 
*/
		if ((*drwsize>15) && (*drwsize<32)) *drwsize = *drwsize - 16;
		if ((*drwsize>31) && (*drwsize<48)) *drwsize = *drwsize - 32;
		if ((*drwsize>47) && (*drwsize<64)) *drwsize = *drwsize - 48;
      if ((*drwsize>63) && (*drwsize<80)) *drwsize = *drwsize - 64;
/*
.....Get the drawing size
*/
		if (num == 4)
		{
			DRAW_size[0] = x;
			DRAW_size[1] = y;
		}
		else
		{
			uj_setps(drwsize,DRAW_size);
		}
/*
.....Display the drawing size
*/
		if ((plotopts.flag & LIST)||(Motif_Plot==1))
		{
			utp_strout ("The drawing size is ");
			if ((*drwsize<0)||(*drwsize>80))
				sprintf(tmp_str, "?=%d\n\n",*drwsize);
			else
				sprintf(tmp_str, "%s\n\n",UM_drawing_size_name[*drwsize]);
			utp_strout (tmp_str);
		}
	}
/*
.....This is a quick plot
*/
	else
	{
/*
.....Obtain number of active views &
.....Border status for each view
.....Currently supports up to 10 views
*/
		num = sscanf (temp,"%d %d %d %d %d %d %d %d %d %d %d %d",
			&qk,&TP_nviews,&TP_border[0],&TP_border[1],&TP_border[2],
			&TP_border[3],&TP_border[4],&TP_border[5],&TP_border[6],
			&TP_border[7],&TP_border[8],&TP_border[9]);
		if (num < 3)
		{
			TP_nviews = UG_MAXNTRAN - 1;
			for (i=0;i<10;i++) TP_border[i] = UU_FALSE;
		}

		if ((plotopts.flag & LIST)||(Motif_Plot==1))
			utp_strout ("A quick plot, no drawing size is specified.\n\n");
	}
}	/* uti_setup1 */



/*********************************************************************
**    I_FUNCTION :  utp_setws(fd1,id,drwsize,control)
**       set workstation window, vport, view up vector, view plane 
**		normal, view reference information 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void utp_setws(fd1,id,drwsize,control)
FILE	*fd1;
Gws	*id;
int	drwsize;
UU_LOGICAL	control;
{
	int	i, len;
	Gwrect3 window;
	Gnrect3 vport, vwport;
	Gwpoint3 vrefpt;
	UU_REAL	vx, vy, vz;
	char tmp_str[300];
/*
...change pen to 256
...Yurong
*/
/*
....changed because every pen# have three part of description
....Yurong 2/13/98
*/
	char    penstk[256][3][20];
	char    temp[512];
	int     ind;
	char templ[5000], *str, str1[10];
	void utp_adjust_plotsize();
	void utp_adjustbound();

#if UU_COMP==UU_IRIS && UU_DOUBLE
	long float tmpvx,tmpvy,tmpvz;
#else
	float tmpvx,tmpvy,tmpvz;
#endif
	UU_REAL	dratio, pratio;
/*
.....consider reselution when write size
.....in *.pl2 file, There may have defferent
.....value when read same number from enviorment and 
.....*.pl2 file . add 0.002 cm defference, that is
.....less then 1 dev unit
.....changed by Yurong 8/8/97
*/
	str = str1;
	if ((DRAW_size[0] >= PLOT_size[0]-0.002)
				&&(DRAW_size[0]<=PLOT_size[0]+0.002)
				&&(DRAW_size[1]>=PLOT_size[1]-0.002)
			   &&(DRAW_size[1]<=PLOT_size[1]+0.002))     
	{
		DRAW_size[0] = PLOT_size[0];
		DRAW_size[1] = PLOT_size[1];
	}
	dratio = DRAW_size[1]/DRAW_size[0];
	pratio = PLOT_size[1]/PLOT_size[0];
/*
.....Set up the plot's window & view port
*/
	for (i=1; i<2 /*UG_MAXNTRAN*/; i++) 	
	{
		window.llf.x = -DRAW_size[0]/2;
		window.llf.y = -DRAW_size[1]/2;
		window.llf.z = 10000.0;
		window.urb.x = DRAW_size[0]/2;
		window.urb.y = DRAW_size[1]/2;
		window.urb.z = -10000.0;
		gswindow3(i,&window);
		vport.llf.x = 0;
		vport.llf.y = 0;
		vport.llf.z = 1;
		vport.urb.x = 1;
		if (DRAW_size[1] <= DRAW_size[0])
		{
			vport.urb.x = 1;
			vport.urb.y =  DRAW_size[1]/DRAW_size[0];
		}
		else
		{
/*
.....The ratio should exactly draw ratio
.....Yurong 8/6/97
*/
			vport.urb.x = DRAW_size[0]/DRAW_size[1];
			vport.urb.y = 1;
		}
		vport.urb.z = 0;
		gsview3(i,&vport);
		gswswindow3(id, &vport);
		if (control)	/* controlled by user's p1 and p2 */
			 utp_adjustbound(id,vport.urb.y,drwsize,&vwport,0,UU_FALSE); 
/*
.....Use ratios instead of drwsize and plot size
.....as the same size drawings on different plotters
.....have different boundaries
*/
		else if (DRAW_size[0] == PLOT_size[0] &&
				 DRAW_size[1] == PLOT_size[1])     
		{
			vwport.llf.x = 0.0;
			vwport.llf.y = 0.0;
			vwport.llf.z = 1.0;
			vwport.urb.x = (DRAW_size[0])/100.0;
			vwport.urb.y = (DRAW_size[1])/100.0; 
			vwport.urb.z = 0.0;
		}
/*
.....Drawing and plot size are different
.....Adjust boundaries
*/
		else
			utp_adjust_plotsize(dratio,pratio,PLOT_size[0],PLOT_size[1],&vwport);
/*
.....Set the current viewport
*/
		gswsview3(id,&vwport);

		fscanf(fd1,"%e%e%e",&tmpvx,&tmpvy,&tmpvz);
		vx = tmpvx; vy = tmpvy; vz = tmpvz;
		vrefpt.y = DRAW_size[1]/2;
		vrefpt.x = DRAW_size[0]/2; 
		vrefpt.z = vz;
		gsvref3(i,&vrefpt);
	}
/*
.....Display the physical pen table
*/
	if ((plotopts.flag & LIST)||(Motif_Plot==1))
	{
		templ[0] = '\0';
		temp[0] = '\0';
		i = 0;
		while (1)
		{
			if (fgets(temp, 80, fd1)==NULL)
				break;
			else
			{
				len = strlen(temp);
				if (temp[len]=='\n')
					temp[len] = '\0';
				strcat(templ, temp);
			}
		}
		ind = 0;
#define WHITE " #"
/*
.....changed to let str not null when
.....use strcpy or str[0] = '\0'
.....Yurong 7/31/98
*/
		str[0] = '\0';
		str = strtok(templ, WHITE);
      if (str != NULL)
		{
			strcpy(penstk[0][0], str);
			str = strtok(NULL, WHITE);
		}
		else
			penstk[0][0][0] = '\0';
      if (str != NULL)
		{
			strcpy(penstk[0][1], str);
			str = strtok(NULL, WHITE);
		}
		else
			penstk[0][1][0] = '\0';
      if (str != NULL)
			strcpy(penstk[0][2], str);
		else
			penstk[0][2][0] = '\0';
  		for (i=1; i<256; i++)
		{
      	if (str != NULL)
			{
				str = strtok(NULL, WHITE);
				strcpy(penstk[i][0], str);
			}
			else
				penstk[i][0][0] = '\0';
      	if (str != NULL)
			{
				str = strtok(NULL, WHITE);
				strcpy(penstk[i][1], str);
			}
			else
				penstk[i][1][0] = '\0';
      	if (str != NULL)
			{
				str = strtok(NULL, WHITE);
				strcpy(penstk[i][2], str);
			}
			else
				penstk[i][2][0] = '\0';
		}
		utp_strout("\n                 Pen Table Description   \n\n");
		utp_strout("***************************************************************\n\n");
/*
.....only display 16 pen number
.....Yurong 2/13/98
*/
   	for (i=0; i<16; i++)
		{
			sprintf(tmp_str, "Logical pen: %s    Thru: %s    Physical Pen: %s\n",
						penstk[i][0],penstk[i][1],penstk[i][2]);
			utp_strout(tmp_str);
		}
	}
}	/* utp_setws */



/*********************************************************************
**    I_FUNCTION :  utp_adjust_plotsize(dratio,pratio,px,py,vwport)
**       Scale the drawing down to fit the new plot size.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void utp_adjust_plotsize(dratio,pratio,px,py,vwport)
UU_REAL 	dratio, pratio;
UU_REAL 	px, py;
Gnrect3  *vwport;

{
	UU_REAL	newx, newy;
	UU_REAL	dif;
	UU_REAL 	pfx, pfy;

	pfx = px / 100.0;
	pfy = py / 100.0;
/*
.....X-axis needs to be reduced
*/
	if (dratio >= pratio)
	{
		newx = pfy / dratio;
		dif = (pfx - newx) / 2.0;
		vwport->llf.x = 0.0;
		vwport->llf.y = 0.0;
		vwport->llf.z = 1.0;
		vwport->urb.x = newx;
		vwport->urb.y = pfy;
		vwport->urb.z = 0.0;
	}
/*
.....Y-axis needs to be reduced
*/
	else							/* y axis need to be reduced */
	{
		newy = pfx * dratio;			/* new y axis length */
		dif = (pfy - newy) / 2.0;
		vwport->llf.x = 0.0;
		vwport->llf.y = 0.0;
		vwport->llf.z = 1.0;
		vwport->urb.x = pfx;
		vwport->urb.y = newy;
		vwport->urb.z = 0.0;
	}
}	/* utp_plotsize */

/*********************************************************************
**    I_FUNCTION :  utp_qksetws(fd1,id,control)
**       set workstation window, vport, view up vector, view plane 
**		   normal, view reference information for quick plot
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void utp_qksetws(fd1,id,control)
FILE	*fd1;
Gws	*id;
UU_LOGICAL 	control;

{
	int	i, len;
	Gwrect3 window, vwindow;
	Gnrect3 vport, vwport;
	Gwpoint3 vpn,vup,vrefpt;
/*
....changed because every pen# have three part of description
....Yurong 2/13/98
*/
	char    penstk[256][3][20];
	char    temp[512], tmp_str[300];
	int     ind;
	char templ[5000], *str, str1[10];
	void utp_adjustbound();

#if UU_COMP==UU_IRIS && UU_DOUBLE
	long float	llfx, llfy, llfz, urbx, urby, urbz;
#else
	float	llfx, llfy, llfz, urbx, urby, urbz;
#endif
	str = str1;

/*
.....Set up each window & viewport in a quickplot
*/
	for (i=1; i<=TP_nviews; i++) 
  	{
		fscanf(fd1,"%e%e%e%e%e%e",&llfx,&llfy,&llfz,&urbx,&urby,&urbz);
		window.llf.x = llfx;
		window.llf.y = llfy;
		window.llf.z = llfz;
		window.urb.x = urbx;
		window.urb.y = urby;
		window.urb.z = urbz;
		gswindow3(i,&window);
		fscanf(fd1,"%e%e%e%e%e%e",&llfx,&llfy,&llfz,&urbx,&urby,&urbz);
		vport.llf.x = llfx;
		vport.llf.y = llfy;
		vport.llf.z = llfz;
		vport.urb.x = urbx;
		vport.urb.y = urby;
		vport.urb.z = urbz;
		gsview3(i,&vport);
/*
.....Save border coordinates
*/
		BORDER_pt[i-1].llf.x = llfx;
		BORDER_pt[i-1].llf.y = llfy;
		BORDER_pt[i-1].urb.x = urbx;
		BORDER_pt[i-1].urb.y = urby;
/*
.....Set coordinate system
*/
		fscanf(fd1,"%e%e%e",&llfx,&llfy,&llfz);
		vup.x = llfx;
		vup.y = llfy;
		vup.z = llfz;
		gsvup3(i,&vup);
		fscanf(fd1,"%e%e%e",&llfx,&llfy,&llfz);
		vpn.x = llfx;
		vpn.y = llfy;
		vpn.z = llfz;
		gsvpn3(i,&vpn);
		fscanf(fd1,"%e%e%e",&llfx,&llfy,&llfz);
		vrefpt.x = llfx;
		vrefpt.y = llfy;
		vrefpt.z = llfz;
		gsvref3(i,&vrefpt);
	}
/*
.....Get the graphics area of the quick plot
*/
	fscanf(fd1,"%e%e%e%e%e%e",&llfx,&llfy,&llfz,&urbx,&urby,&urbz);
	vwindow.llf.x = llfx;
	vwindow.llf.y = llfy;
	vwindow.llf.z = llfz;
	vwindow.urb.x = urbx;
	vwindow.urb.y = urby;
	vwindow.urb.z = urbz;
	gswswindow3(id, &vwindow);

	if (!control)
	{
		vwport.llf.x = 0.0;
		vwport.llf.y = 0.0;
		vwport.llf.z = 1.0;
		vwport.urb.x = PLOT_size[0]/100.0;
		vwport.urb.y = PLOT_size[1]/100.0;
		vwport.urb.z = 0.0;
	}
	else
		utp_adjustbound(id,vwindow.urb.y,-1,&vwport,0.0,UU_TRUE);
	gswsview3(id,&vwport);
/*
.....Display the Logical Pen Table
*/
	if ((plotopts.flag & LIST)||(Motif_Plot==1))
	{
		templ[0] = '\0';
		temp[0] = '\0';
		i = 0;
		while (1)
		{
			if (fgets(temp, 80, fd1)==NULL)
				break;
			else
			{
				len = strlen(temp);
				if (temp[len]=='\n')
					temp[len] = '\0';
				strcat(templ, temp);
			}
		}
		ind = 0;
#define SPWHITE "\n #"
		str[0] = '\0';
		str = strtok(templ, SPWHITE);
		if (str != NULL)
			strcpy(penstk[0][0], str);
		else
			strcpy(penstk[0][0], " ");
		if (str != NULL)
			str = strtok(NULL, SPWHITE);
		if (str != NULL)
			strcpy(penstk[0][1], str);
		else
			strcpy(penstk[0][1], " ");
		if (str != NULL)
			str = strtok(NULL, SPWHITE);
		if (str != NULL)
			strcpy(penstk[0][2], str);
		else
			strcpy(penstk[0][2], " ");
  		for (i=1; i<256; i++)
		{
			if (str != NULL)
				str = strtok(NULL, SPWHITE);
			if (str != NULL)
				strcpy(penstk[i][0], str);
			else
				strcpy(penstk[i][0], " ");
			if (str != NULL)
				str = strtok(NULL, SPWHITE);
			if (str != NULL)
				strcpy(penstk[i][1], str);
			else
				strcpy(penstk[i][1], " ");
			if (str != NULL)
				str = strtok(NULL, SPWHITE);
			if (str != NULL)
				strcpy(penstk[i][2], str);
			else
				strcpy(penstk[i][2], " ");
		}
		utp_strout("\n                 Pen Table Description   \n\n");
		utp_strout("****************************************************************\n\n");
/*
.....we need only display 16 pen number
.....Yurong 2/13/98
*/
   	for (i=0; i<16; i++)
		{
			sprintf(tmp_str, "Logical pen: %s    Thru: %s    Physical Pen: %s\n", 
			penstk[i][0],penstk[i][1],penstk[i][2]);
			utp_strout(tmp_str);
		}
	}
}	/* utp_setws */


/*********************************************************************
**    I_FUNCTION :  utp_adjustbound(id,ratio,drwsize,vwport,Margin,qkplot)
**       Find the rectangle in between the user's p1 and p2 point to suit 
**			the drawing size.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void utp_adjustbound(id,ratio,drwsize,vwport,Margin,qkplot)
Gws	*id;
UU_REAL	ratio;
int	drwsize;
Gnrect3  *vwport;
UU_REAL  Margin;
UU_LOGICAL qkplot;

{
	int	x1, y1, x2, y2;
	UU_REAL	r, dist, convert, dum;
	UU_REAL	dx, dy, dx1, dy1;
	UU_REAL  fuzz;
	UU_LOGICAL	scale;

		scale = UU_TRUE;
		convert = (*ug_gksstli.wsopen[*id].wdtptr).dspsize.device.x/
					 (*ug_gksstli.wsopen[*id].wdtptr).dspsize.raster.x;
		x1 = plot_bound.ur.x - plot_bound.ll.x;
		y1 = plot_bound.ur.y - plot_bound.ll.y;
/*
.....Store physical plot size
.....Bobby  -  7/17/91
*/
		PLOT_size[0] = x1 * convert;
		PLOT_size[1] = y1 * convert;

		if (drwsize >= 0)		/* a drawing */
		  {
			dx = (DRAW_size[0]+2.0*Margin)/100.0;
			dy = (DRAW_size[1]+2.0*Margin*0.8)/100.0;
			dx1 = x1 * convert;
			dy1 = y1 * convert;
			fuzz = 2.0*Margin/100.0;
				/* if p1,p2's range is greater than drawing size, don't scale up*/
			if (((dx1+fuzz)>=dx)&&((dy1+fuzz)>=dy))
			  {
				scale = UU_FALSE;
			(*vwport).llf.x = plot_bound.ll.x*convert;
			(*vwport).llf.y = plot_bound.ll.y*convert;
			(*vwport).llf.z = 1.0;
			(*vwport).urb.x = plot_bound.ll.x*convert + dx;
			(*vwport).urb.y = plot_bound.ll.y*convert + dy;
			(*vwport).urb.z = 0.0;
		  }
	  }
	if (scale)		/* need to be scaled down */
	  {
/*
.....Replaced .5's with dum's
.....as cheating on the borders is no longer
.....necessary due to new plotting size specification
.....Bobby  -  7/16/91
*/
		dum = 0.;
		r = (UU_REAL)y1 / (UU_REAL)x1;
		if (r < ratio)
	  	{
			x2 = (int)(y1 / ratio + dum);
			dist = (UU_REAL)(x1 - x2)/2.0 + dum;
			plot_bound.ll.x = (int)(plot_bound.ll.x + dist);
			plot_bound.ur.x = (int)(plot_bound.ur.x - dist);
	  	}
		else if (r > ratio)
			 	{
			  	y2 = (int)(x1 * ratio + dum);
			  	dist = (UU_REAL)(y1 - y2)/2.0 + dum;
			  	plot_bound.ll.y = (int)(plot_bound.ll.y + dist);
			  	plot_bound.ur.y = (int)(plot_bound.ur.y - dist);
			 	}
		(*vwport).llf.x = plot_bound.ll.x*convert;
		(*vwport).llf.y = plot_bound.ll.y*convert;
		(*vwport).llf.z = 1.0;
		(*vwport).urb.x = plot_bound.ur.x*convert;
		(*vwport).urb.y = plot_bound.ur.y*convert;
		(*vwport).urb.z = 0.0;

		/***-- put out warning for scaling down a drawing only--***/
		if (!qkplot)
			utp_strout("WARNING -- THIS PLOT WILL BE SCALED DOWN!!\n");
	  }
}	/* utp_findbound */



/*********************************************************************
**    I_FUNCTION :  utp_setlinwt(id,linewt)
**      Change lineweight from inches to ndc coordinate.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void utp_setlinwt(id,linewt)
Gws	*id;
/* change from float */
UU_REAL 	linewt;

{
	int	lineras;
	struct { Gint op; Gws id; Gscale wid; } prms;
	int reply;
	UU_REAL	convert;

	convert = (*ug_gksstli.wsopen[*id].wdtptr).dspsize.raster.x/
				(*ug_gksstli.wsopen[*id].wdtptr).dspsize.device.x;
	lineras = (int)(linewt*0.0254*convert);	/* convert inches to device coordinate */
	prms.id = *id;
	prms.wid = lineras;		/* linewidth in device coordinate */
	(*(ug_gksstli.wsopen[*id].connid)[UG_DLINEWIDTH])(&prms,&reply);
}	/* utp_setlinwt */


/*********************************************************************
**    I_FUNCTION :  utp_set_dashlen(id)
**      Set the dashline length to 4/16-2/16".
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void utp_set_dashlen(id)
Gws	*id;

{
	int	lineras[2];
	Gfloat	ndc[2];
	UU_REAL	convert;
	UU_REAL	scale;
	
	/* Reset line styles to adjust for plot size	*/
	/* ug_lntypes is defined in gout2.c	*/
	convert = (*ug_gksstli.wsopen[*id].wdtptr).dspsize.raster.x/
				(*ug_gksstli.wsopen[*id].wdtptr).dspsize.device.x;
			/* convert dash length(inches) to device coordinate */
	lineras[0] = (int)(6.0/16.0*0.0254*convert);	
	lineras[1] = 1;
			/* convert device coordinate to ndc  */
	(*(ug_gksstli.wsopen[*id].connid)[UG_DDEVNDC])(lineras,ndc,*id);
			/* get the scale factor for each ndc length  */
/*
.....T_PLOT and line_scale are used to indicate to ug_lnstyle
.....that the line length needs to be scaled, otherwise as the
.....paper size increase so does the length of the line pattern.
.....the number 1.8519369 is the scale when using 7475AH. JLS 9/1/99
*/
	T_PLOT = UU_TRUE;
	scale = ndc[0]/ug_lntypes[4].patnlen; 
	line_scale = 1.8519369/scale;
}	/* utp_set_dashlen */




/*********************************************************************
**    I_FUNCTION :  utp_ready()
**      Give the operator the last chance to decide to continuing 
**		  plotting or not.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

utp_ready()

{
	char	ch;
	int	status;
	UU_LOGICAL	first;


/*
.....Non-interactive mode
.....Don't prompt for ready
*/
	if ((!(plotopts.flag & INTER))||(Motif_Plot==1))
	{
		status = UU_TRUE;
	}
/*
.....Interactive mode
.....Ask the user if they are ready
*/
	else
	{
		first = UU_TRUE;
		while (UU_TRUE)
		{
  		 printf("\nReady to plot? (y/n): ");
  		 fflush(stdout);
		 if (first)
		   {
			 first = UU_FALSE;
  			 scanf("\n");	
			}
  		 scanf("%c", &ch);
  		 if ((ch=='y')||(ch=='Y'))		/* only the first char count */
			{
 			 status = UU_TRUE;
			 break;
			}
		 else if ((ch=='n')||(ch=='N'))
				  {
					status = UU_FALSE;
					break;
				  }
		 while (ch!='\n')			/* skip through the rest of line */
  		 	scanf("%c", &ch);
   		}
	}
	return(status);
}	/* utp_ready */
