/*********************************************************************
**
**    NAME         :  d1initmu.c
**
**       CONTAINS:
**  			ud_initmu
**  			ud_initmu2
**  			ud_initmu3
**			
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d1initmu.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:03
**
*********************************************************************/

#include "usysdef.h"
#include "usysg.h"
#include "ustdio.h"
#include "xenv1.h"
#include "dasnog.h"
#include "dasg.h"
#include "gviw.h"
#include "gtbl.h"
#include "gsegop.h"
#include "mdattr.h"
#include "uerror.h"
#include "udebug.h"
#include "uims.h"
#include "zsysdep.h"

#define MASK 0644
/**************************************************************************
**  I_FUNCTION         :  ud_initmu
**      initialize text popup menu devices
**  PARAMETERS   
**      INPUT  : 
**
**          device = device number for menu
**				count  = number of choices
**				text   = text of choices as array of strings (count+1 in length)
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

ud_initmu(device, count, text, location)
int device;									/* device number of menu */
int count;									/* number of choices */
char *text[];								/* text of menus plus header */
Gdrect *location;							/* location of pull down menus */
{

	Gchoicerec inchoice;					/* GKS choice initialize struct */
	UD_AREA *areapt;
	Gdrect pos;
	int i;
	int scrn;
	Gnrect *ud_geticdata();
	int nch;
/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
**		-- set physical unit table to UD_CONS -- 
*/
	uu_denter(UU_DTRC,(us,"ud_initmu(%d, count=%d, %s)",device,count,text[0]));
	zbytecp(inchoice,
		 ug_gksstli.wsopen[*UD_ksws].inptr->choicedata[device-1].record);
	inchoice.strings = text;
	inchoice.number = count;

/* use current menu area from the uims layout */

	if(location == NULL)
	{
	 i=UD_curlayout.curr_menu_area[UD_curlayout.curr_menu_stklen];
  	 areapt= &(UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_MENU])[i];
	 inchoice.bkcolor= areapt->color;
	 inchoice.bordcolor=areapt->bordercolor;
	 inchoice.txcolor=areapt->contcolor;

/* 	we want the position of the choice device to be specified in NDC.
		However, ginitchoice takes DC. Therefore we must apply the ws xform
		to the echo area rectangle, to change it to DC */

	 ud_devrect(&(*areapt).posn, &pos);
	}
	else
	{

/* 	we want the position of the choice device to be specified in NDC.
		However, ginitchoice takes DC. Therefore we must apply the ws xform
		to the echo area rectangle, to change it to DC */

		ud_devrect(location, &pos);

		/* Set menu colors	*/
		/* Should use 'color' field if none NULL	*/
   	inchoice.bordcolor = 1; 
   	inchoice.txcolor = 1;
   	inchoice.bkcolor = 0; 
	}
	ginitchoice(UD_ksws, device, 0, UD_chcech, &pos, &inchoice);
	uu_dexit;
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_initmu2
**      initialize graphical popup menu devices.
**
**  PARAMETERS   
**      INPUT  : 
**          device = device number for menu
**				count  = number of choices
**      OUTPUT :  
**				position   = icon menu placement position
**
**  RETURNS      :  status of operation (0=OK, otherwise not OK)
**
**  SIDE EFFECTS :  none
**
**  WARNINGS     :  none
**
**************************************************************************/

int ud_initmu2(device, fname, position)
int device;								/* device number of menu */
char *fname;							/* archive filename containing the menu */
Gdrect *position;						/* return icon menu placement position */
{

	Gchoicerec inchoice;				/* GKS choice initialize struct */
	UD_AREA *areapt;
	int i;
	int segno;							/* segment no. for menu */
	int scrn;
	int strs[10];						/* seg nos on archive file */
	int num;								/* length of strs */
	char ermsg[150];
	int elttyp,eltsiz;				/* element type, size */
	int status=1;							/* status return cell */
	int trash;
	int choices,ddfd,k,j,rows,cols;
	int len, graphics_flag,total_len;
	char **iconfile;
	char devfname[120];
	char devtmp[120];
	char devname[10];
	int archf,arclen;
	Gnrect *ud_geticdata();
   Girect *iposn;
	Gipoint ras,ras1;
	char sympath[200];
	char filename[100];

/* the following is from gsegop.h. Should #include gsegop.h, but won't 
	compile on SUN. */
#define ELTTYPE unsigned char elttype
/* UG_PLYLNA2OP */ 
/*typedef struct {
/*	ELTTYPE;
/*	short     len;
/*	Gwpoint pts[10000];
/*	} UG_plylna2op;
*/
UG_segplylna2op *eltpt;		/* UG_SEGPLYLNA2OP element pointer */

static char *icmsg = "ICON MENU,  PET 5";
/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
**		-- set physical unit table to UD_CONS -- 
*/
	uu_denter(UU_DTRC,(us,"ud_initmu2(%d, fname=%s)",device,fname));

/* -- use current menu area from the uims layout */

	i=UD_curlayout.curr_menu_area[UD_curlayout.curr_menu_stklen];
	areapt= &(UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_MENU])[i];
	
	/* take the following out. Now used position from archive file, not
		from layout file */
	zbytecp(inchoice,
		 ug_gksstli.wsopen[*UD_ksws].inptr->choicedata[device-1].record);
	inchoice.devfname=NULL;
	sprintf(sympath,"U_ICONDIR/%s", fname);
	if(ux_search_for_path(sympath,devtmp,
			UX_PRTERRS|UX_NCHK|UX_NQUOTES) != UU_SUCCESS)
		  strcpy(devtmp,fname);
	devtmp[strlen(devtmp)-2] = '\0';
	strcpy(devname,ug_gksstli.wsopen[*UD_ksws].wdtptr->type);
	devname[3] = '\0';
	sprintf(devfname,"%s.%s",devtmp,devname);
	inchoice.devfname = (char *) uu_toolmalloc(strlen(devfname)+1);
	strcpy(inchoice.devfname,devfname);
	uu_dprint(UU_DTRC,(us,"devfname = %s",inchoice.devfname));
	ddfd = -1;
	ddfd = open(devfname,0);
	if(ddfd != -1){
		uu_dprint(UU_DTRC,(us,"ud_initmu2:ddfd = %d\tfile = %s",
			 ddfd,inchoice.devfname));
		uu_dprint(UU_DTRC,(us,"dev dep file %s opened",devfname));
		/* read file	*/
		read(ddfd,&ras,sizeof(Gipoint));
		ras1=ug_gksstli.wsopen[*UD_ksws].wdtptr->dspsize.raster;
		if((ras1.x == ras.x) && (ras1.y == ras.y)){
		read(ddfd,&graphics_flag,sizeof(int));
		if(graphics_flag){
			read(ddfd,&len,sizeof(int));
			read(ddfd,position,sizeof(Gdrect));
			uu_dprint(UU_DTRC,(us,"position = %g %g %g %g",position->ll.x,
					position->ll.y,position->ur.x,position->ur.y));
			/*	read archive data	*/
          read(ddfd,&choices,sizeof(int));
          read(ddfd,&trash,sizeof(int));	/* Number of rows	*/
          read(ddfd,&trash,sizeof(int));	/* Number of cols	*/
          iposn=(Girect *)uu_toolmalloc(sizeof(Girect)*choices);
			 inchoice.chposn=(Gnrect *)uu_toolmalloc((choices)*sizeof(Gnrect));
          read(ddfd,iposn,choices*sizeof(Girect));
			 for(k=0;k<choices;k++)
            {
             inchoice.chposn[k].ll.x=((Gfloat)iposn[k].ll.x/(Gfloat)ras.x);
             inchoice.chposn[k].ll.y=((Gfloat)iposn[k].ll.y/(Gfloat)ras.x);
             inchoice.chposn[k].ur.x=((Gfloat)iposn[k].ur.x/(Gfloat)ras.x);
             inchoice.chposn[k].ur.y=((Gfloat)iposn[k].ur.y/(Gfloat)ras.x);
            }
			 uu_toolfree(iposn);
			 status=0;
			/*	-- init choice request record --*/
			
			 inchoice.strings = &icmsg;
			 inchoice.number = choices;
			
			/* 	-- init the choice device for pet==5 (graphical menu) -- */
				
			 close(ddfd);
			 ginitchoice(UD_ksws, device, 0, 5, position, &inchoice);
		
			 }/* End if graphics data	*/
			}	/* End if ras = ras1	*/
		close(ddfd);
		}	/* End if dev spec file opened	*/
	if(status != 0)
		{
		int filedesc;

		sprintf(sympath,"U_ICONDIR/%s", fname);
		if(ux_search_for_path(sympath,filename,
					UX_PRTERRS|UX_NCHK|UX_QUOTES) != UU_SUCCESS)
			strcpy(filename,fname);
		strcpy(devfname,&filename[1]);
		devfname[strlen(filename) - 3] = '\0';
		strcat(devfname,devname);
		uu_toolfree(inchoice.devfname);
		inchoice.devfname=(char *)uu_toolmalloc(strlen(devfname)+1);
		strcpy(inchoice.devfname,devfname);
		if ((status=ux_open_to_data(filename, "r", "STREAM",
			  "BINARY", &filedesc, UX_PRTERRS)) ==UU_SUCCESS)
			{
			FILE *fileptr;
			int nread;

			arclen = glenarcf(filename);

			/* Position to end of file	*/
			ux_get_os_filedesc(filedesc, &fileptr, UX_PRTERRS);
			ux_fseek0(fileptr, arclen, 1);

         /* read data from tail of archfile,
         Must allocate memory */
			nread=1;
			ux_read(filedesc, &choices,sizeof(int),&nread,UX_PRTERRS);
			nread=1;
			ux_read(filedesc,&rows,sizeof(int),&nread,UX_PRTERRS);
			nread=1;
			ux_read(filedesc,&cols,sizeof(int),&nread,UX_PRTERRS);

			inchoice.chposn = (Gnrect *) uu_toolmalloc(sizeof(Gnrect)*choices);
         iconfile= (char **) uu_toolmalloc(sizeof(char *)*choices);
			nread=choices;
			ux_read(filedesc, inchoice.chposn, sizeof(Gnrect), &nread, UX_PRTERRS);
			total_len = 0;
         for(k=0;k<choices;k++)
            {
				nread=1;
				ux_read(filedesc, &len, sizeof(int), &nread, UX_PRTERRS);
				total_len += (len+1);
				iconfile[k]=(char*) uu_toolmalloc(80*sizeof(char));
				nread=len+1;
				ux_read(filedesc, iconfile[k], sizeof(char), &nread, UX_PRTERRS);
            }
			ux_close(filedesc, UX_PRTERRS);

	
	/* 	-- get the position of this menu from the 2nd element (polyline)
				in the archive file segment -- */
	
			ginitarch();
			ux_strip_quotes(filename);    
			ud_gopenarcf(filename);
			segno=gnseg();					/* get an unused segment to put menu in */
	/* 	read menu segment from archive file */
			gdeactivatews(UD_ksws);		/* deactivate so won't draw the seg*/
			gqarchids(&num,strs);		/* inquire structures on the file */
			gretrstr2(1,strs,&segno);	/* retrieve the 1st structure on the 
																			file into segno */
			gclosarcf();
			gopenseg(segno);
			gseteltptr(5);					/* polyline is 4th elt in an icon file. */
			gqelttype(&elttyp,&eltsiz);

			if (elttyp!=UG_PLYLNA2OP) 
			{
	
				uu_dprint(UU_DTRC,(us,
					"ud_initmu2 error. icon file %d is bad. (elttype=%d)", filename,
					elttyp));
	
				uu_uerror1(DAS, 83, filename);
				gcloseseg();
				gdeleteseg(segno);
				gactivatews(UD_ksws);		/* re-activate the workstation */
				status = 5;
				goto done;
			}
			else 
			{							/* elttype is OK */
				gqeltcont(&eltpt);
				ud_devrect((*eltpt).pts, position);
				uu_denter2(UU_DTRC,(us,"ud_initmu2 xform in = %g %g %g %g",
					(*eltpt).pts[0].x, (*eltpt).pts[0].y,
					(*eltpt).pts[1].x, (*eltpt).pts[1].y));
				uu_dexit;
				uu_denter2(UU_DTRC,(us,"ud_initmu2 xform out = %g %g %g %g",
					(*position).ll.x, (*position).ll.y,
					(*position).ur.x, (*position).ur.y));
				uu_dexit;
			}

			gcloseseg();
			gactivatews(UD_ksws);		/* re-activate the workstation */
		
		/*	-- init choice request record --*/
		
			inchoice.seg=segno;			/* put menu segno into choice record */
			inchoice.strings = &icmsg;		/* put menu segno into choice record */
			inchoice.number = choices;
			/*inchoice.devfname = (char *) uu_toolmalloc(strlen(devfname)+1);
			strcpy(inchoice.devfname,devfname);
			uu_dprint(UU_DTRC,(us,"devfname = %s",inchoice.devfname));
			/*inchoice.number = 40;*/
		
		/* Write device specific file			*/
         if((ddfd=creat(devfname,MASK))== -1){
				uu_dprint(UU_DTRC,(us, 
					"ud_initmu2:Can't create device specific file %s", devfname));
				}
			else     /* File created   */
				{
				uu_dprint(UU_DTRC,(us,"ud_initmu2:ddfd = %d\tfile = %s",
									ddfd,inchoice.devfname));
				uu_dprint(UU_DTRC,(us,"ud_initmu2:creating dev spec file %s",
						devfname));
				/* Write graphics flag	*/
				ras=ug_gksstli.wsopen[*UD_ksws].wdtptr->dspsize.raster;
				uu_dprint(UU_DTRC,(us,"initmu2:write: ras = %d %d",ras.x,ras.y));
				write(ddfd,&ras,sizeof(Gipoint));
				graphics_flag = 0;
				write(ddfd,&graphics_flag,sizeof(int));

				/* Solve for total len of das data	*/
				len = 3*sizeof(int)+sizeof(Gdrect)+choices*sizeof(Girect)+
						choices*sizeof(int)+total_len * sizeof(char);

				/* write archive data*/
				write(ddfd,&len,sizeof(int));
				write(ddfd,position,sizeof(Gdrect));
            write(ddfd,&choices,sizeof(int));
            write(ddfd,&rows,sizeof(int));
            write(ddfd,&cols,sizeof(int));
				iposn = (Girect *)uu_toolmalloc(sizeof(Girect)*choices);
				for(k=0;k<choices;k++)
					{
						iposn[k].ll.x=inchoice.chposn[k].ll.x*ras.x;
						iposn[k].ll.y=inchoice.chposn[k].ll.y*ras.x;
						iposn[k].ur.x=inchoice.chposn[k].ur.x*ras.x;
						iposn[k].ur.y=inchoice.chposn[k].ur.y*ras.x;
					}
            write(ddfd,iposn, choices*sizeof(Girect));
				uu_toolfree(iposn);
            for(k=0;k<choices;k++)
              {
              len = strlen(iconfile[k]);
              write(ddfd,&len,sizeof(int));
              write(ddfd,iconfile[k],len+1);
              }
			close(ddfd);
	
				}		/* dev spec file created	*/

	/* 	-- init the choice device for pet==5 (graphical menu) -- */
			ginitchoice(UD_ksws, device, 0, 5, position, &inchoice);
			

	/*		-- delete the graphic segment holding the icon -- */

			gdeactivatews(UD_ksws);
			gdeleteseg(segno);
			gactivatews(UD_ksws);
	
			uu_dprint(UU_DTRC,(us,"ud_initmu2 returns. segno=%d, num=%d",
							segno, num));
			}
		}/* End if status != 0	*/

done:;
	uu_denter2(UU_DTRC,(us,"ud_initmu2 return=%d", status));
	uu_dexit;
	uu_dexit;
	return(status);
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_initmu3
**      initialize text popup menu devices
**
**  PARAMETERS   
**      INPUT  : 
**          device = device number for menu
**				count  = number of choices
**				text   = text of choices as array of strings (count+1 in length)
**				rect	 = enclosing rectangle
**
**      OUTPUT :  none
**
**  RETURNS      :  none
**
**  SIDE EFFECTS :  none
**
**  WARNINGS     :  none
**
**************************************************************************/

ud_initmu3(poprec)
UD_POPUPREC *poprec;						/* popup control block */
{

	Gdrect rect;
	Gchoicerec inchoice;					/* GKS choice initialize struct */
	UU_REAL height;						/* menu text height return */
	UU_REAL width;							/* menu text width return */
	int i, len, bestlen;

/*
**  Start of Executable Code
*/
	uu_denter(UU_DTRC,(us,"ud_initmu3"));

/*	-- set up choice init structure -- */

	inchoice.strings = (*poprec).text;
	inchoice.number = (*poprec).numchoice - 1;
   inchoice.bordcolor = 1; 
   inchoice.txcolor = 1;
   inchoice.bkcolor = 0; 


/*	-- compute menu size -- */

/*	-- first find the length of the longest line -- */

	bestlen = 13;
	for(i=0; i<(*poprec).numchoice; i++)
	{
		len = strlen((*poprec).text[i]);
		if(bestlen < len)
			bestlen = len;
	}

	gqmenutextsize(UD_ksws, &height, &width);

	rect.ll.x = 0.;
	rect.ll.y = 0.;
	rect.ur.x = width * (bestlen + 5);
	rect.ur.y = height * ((*poprec).numchoice+3) + height/2; 

/*	-- save newly computed box in popup control block -- */

	(*poprec).menusz.x = rect.ur.x;
	(*poprec).menusz.y = rect.ur.y;

	ginitchoice(UD_ksws, (*poprec).devnum, 0, (*poprec).popet-30, &rect,
					&inchoice);

	uu_dexit;
}
