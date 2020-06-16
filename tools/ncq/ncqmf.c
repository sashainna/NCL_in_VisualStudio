#include "usysdef.h"
#if UU_COMP != UU_WIN2K
/*********************************************************************
**    NAME         :  ncqmf.c
**       CONTAINS:
**						ncq_kill_ncl()
**						ncl_que_is_empty()
**						timeoutCB
**						FileOkCB						
**						FileCancelCB
**						ncq_savoptCB
**						ncq_opt_cancelCB
**						ncq_mf_filename
**						file_text_returnCB
**						selectchgCB
**						ncq_filedelete()
**						ncq_fileopen()
**						ncq_update()
**						ncq_ncl_update()
**						ncq_run_ncl()
**						ncq_if_ncl_runnung()
**						appexit()
**						ncq_mfcreate_action
**						ncq_options()
**						appexitCB
**						filemenuCB
**						viewmenuCB
**						priomenuCB
**						timemenuCB
**						optionmenuCB
**						helpmenuCB
**						CreatePushbutton
**						CreatePulldown
**						main()
**						ncq_unxselectlst
**						ncq_unxselectfile
**						ncq_unxreset_list
**						ncq_unxdispmsg
**						ncq_unxadditem
**						ncq_unxgetsel
**						ncq_setimage_size
**						toolbarCB
**						ncq_getimage(pix_file, image)
**						ncq_create_ximage(widget, image_data, output_image, indx)
**						ncq_mfupd_nclinfo(info)
**						ncq_mfstatus_message
**						ncq_getnclmsg()
**						ncq_get_nclid()
**						ncq_getnclinfo()
**						ncq_open_common(com_key)
**						ncq_close_common()
**						ncq_write_comblk(NCLInfo *info)
**						ncq_read_comblk(NCLInfo *info)
**						ncq_read_commsg(int *jobno, int *err, int *warn)
**						ncq_read_comint(int *data, int pos)
**						ncq_write_comint(int *data, int pos, int size)	
**						
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**     MODULE NAME AND RELEASE LEVEL
**			ncqmf.c , 25.1
**		DATE AND TIME OF LAST MODIFICATION
**      	04/29/15 , 15:13:02
*********************************************************************/
#include "usysdef.h"
#include <stdio.h>
#ifndef UU_RS6000
#include <stropts.h>
#endif
#include <errno.h>
#include <poll.h>
#include <sys/resource.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/CascadeBG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/DrawnB.h>
#include <Xm/BulletinB.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <Xm/MwmUtil.h>
#include <Xm/Protocols.h>
#include <Xm/SashP.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <X11/cursorfont.h>
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>
#include <Xm/CascadeB.h>
#include <stdio.h>
#include <math.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <Xm/Separator.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include "ncqcom.h"
/*
.....The variables being passed into the fortran
.....routines below had all been declared as normal
*/

#define BI_RGB	0
#define BI_RLE8	1
#define BI_RLE4	2

extern int NCQ_runver;
extern char ncq_file[MAX_PATH];
extern char ncq_localdir[MAX_PATH];
extern int NCQ_monitor;
Widget NCQ_Parent;
XtAppContext NCQ_App;
char NCL_common_name[256];
static int ncq_mfanswer;
static int yes_or_no;
static char NCQ_current_file[41];

static Widget file_fld=0, runbut=0, listbox=0, stat_area=0, ncq_stat_win=0;
static   Widget label, label2, ncq_pane, ncq_window, ncq_form,
			option_dialog, opt_chk[7], opt_edt[2];
static Widget monitor_dialog, mon_edit[7];
static   Widget Main_Window, menuBar, menu1, menu2, menu3, menu4, menu5;
static   Widget menu31, menu32;
static int formEof;
static unsigned long yl_color_px, green_color_px, red_color_px, grey_color_px;

static void ncq_savoptCB(), ncq_opt_cancelCB(), ncq_cls_monitorCB();
static void ncq_file_openCB(), ncq_file_runCB(), toolbarCB();
static void ncq_file_deleteCB(), ncq_updateCB(), ncq_optionCB();
static void ncq_file_loadqueueCB(), ncq_file_savequeueCB(),
			ncq_view_monitorCB(), ncq_view_clearCB();

typedef struct
{
   char *label;
   void (*callback)();
   XtPointer data;
} ActionAreaItem;

typedef struct
{
   char *bmpfile;
	char *label;
   void (*callback)();
   XtPointer data;
	XImage *image;
} ToolbarItem;

static ActionAreaItem actionList[] =
{
   {"OK", ncq_savoptCB, (XtPointer)1},
   {"CANCEL", ncq_opt_cancelCB, (XtPointer)2}
};

static ToolbarItem toolbarlst[] = 
{
	{"open_pp_24.bmp", "Open", ncq_file_openCB, (XtPointer)0,0 },
	{"delete_24.bmp", "Delete", ncq_file_deleteCB, (XtPointer)0,0 },
	{"load_24.bmp", "LoadQueue", ncq_file_loadqueueCB, (XtPointer)0,0 },
	{"save_24.bmp", "SaveQueue", ncq_file_savequeueCB, (XtPointer)0,0 },
	{"monitor_24.bmp", "Monitor", ncq_view_monitorCB, (XtPointer)0,0 }, 
	{"blank_24.bmp", "ClearStatus", ncq_view_clearCB, (XtPointer)0,0 },
	{"repaint_24.bmp", "Reflush", ncq_updateCB, (XtPointer)0,0 },
	{"option_24.bmp", "Optionis", ncq_optionCB, (XtPointer)0,0 },
	{"Prog_Run_24.bmp", "Run NCL", ncq_file_runCB, (XtPointer)1,0}
};

static void filemenuCB(), viewmenuCB(), optionmenuCB(), helpmenuCB(),
		priomenuCB(), timemenuCB(), ratemenuCB(),
		selectchgCB(), file_text_returnCB();
static int cur_sel = -1;

static Widget mine;
typedef struct
{
   int sel;
   char *file;
} FSELECT;

typedef struct
{
	int btype;
	char    *name;
	char *accelerator;
	char *accelText;
	int *ciStruct;
	int subMenuCount;
	XtCallbackProc callback;
	XtPointer client_data;
} CmdInterfaceStruct;

typedef struct 
{
    unsigned short    bfType;
    unsigned int   bfSize;
    unsigned short    bfReserved1;
    unsigned short    bfReserved2;
    unsigned int   bfOffBits;
} BITMAPFILEHEADER;


typedef struct
{
    unsigned int   bcSize;
    unsigned short    bcWidth;
    unsigned short    bcHeight;
    unsigned short    bcPlanes;
    unsigned short    bcBitCount;
} BITMAPCOREHEADER;


typedef struct
{
    unsigned int  biSize;
    long   biWidth;
    long   biHeight;
    unsigned short   biPlanes;
    unsigned short   biBitCount;
    unsigned int  biCompression;
    unsigned int  biSizeImage;
    long   biXPelsPerMeter;
    long   biYPelsPerMeter;
    unsigned int  biClrUsed;
    unsigned int  biClrImportant;
} BITMAPINFOHEADER;

typedef struct
{
	BYTE rgbtBlue;
	BYTE rgbtGreen;
	BYTE rgbtRed;
} RGBTRIPLE;

typedef struct
{
	BYTE    rgbBlue;
	BYTE    rgbGreen;
	BYTE    rgbRed;
	BYTE    rgbReserved;
} RGBQUAD;
CmdInterfaceStruct FileMenu[] =
{
	{0, "Open",   "Ctrl <Key> o", "Ctl+o", NULL,0, filemenuCB, (XtPointer)0},
   {0, "Delete", "Ctrl <Key> d", "Ctl+d", NULL,0, filemenuCB, (XtPointer)1},
	{-1, NULL,     NULL,          NULL,    NULL,0, NULL,       (XtPointer)-1},
   {0, "Load Queue", "Ctrl <Key> l", "Ctl+l", NULL,0, filemenuCB, (XtPointer)2},
   {0, "Save Queue", "Ctrl <Key> s", "Ctl+s", NULL,0, filemenuCB, (XtPointer)3},
	{-1, NULL,     NULL,          NULL,    NULL,0, NULL,       (XtPointer)-1},
   {0, "Run NCL Batch", "Ctrl <Key> r", "Ctl+r", NULL,0, filemenuCB, (XtPointer)4},
	{-1, NULL,     NULL,          NULL,    NULL,0, NULL,       (XtPointer)-1},
	{0, "Exit",   "Ctrl <Key> x", "Ctl+x", NULL,0, filemenuCB, (XtPointer)5}
};

CmdInterfaceStruct RateMenu[] =
{
	{1, "1 second",   NULL, NULL, NULL,0, ratemenuCB, (XtPointer)0},
   {1, "2 seconds", NULL, NULL, NULL,0, ratemenuCB, (XtPointer)1},
   {1, "3 seconds", NULL, NULL, NULL,0, ratemenuCB, (XtPointer)2},
   {1, "5 seconds", NULL, NULL, NULL,0, ratemenuCB, (XtPointer)3},
   {1, "10 seconds", NULL, NULL, NULL,0, ratemenuCB, (XtPointer)4}
};
CmdInterfaceStruct ViewMenu[] =
{
	{0, "Update",   "Ctrl <Key> u", "Ctl+u", NULL,0, viewmenuCB, (XtPointer)0},
	{-1, NULL,     NULL,          NULL,    NULL,0, NULL,       (XtPointer)-1},
	{0, "NCL Monitor",   "Ctrl <Key> m", "Ctl+m", NULL,0, viewmenuCB, (XtPointer)1},
	{0, "Update Rate",   NULL, NULL, (int *)RateMenu, 5, viewmenuCB, (XtPointer)2},
	{-1, NULL,     NULL,          NULL,    NULL,0, NULL,       (XtPointer)-1},
	{0, "Clear Status",   "Ctrl <Key> c", "Ctl+c", NULL,0, viewmenuCB, (XtPointer)3}
};

CmdInterfaceStruct PrioMenu[] =
{
	{1, "Low",   NULL, NULL, NULL,0, priomenuCB, (XtPointer)0},
   {1, "Normal", NULL, NULL, NULL,0, priomenuCB, (XtPointer)1}
/*
   {1, "Normal", NULL, NULL, NULL,0, priomenuCB, (XtPointer)1},
   {1, "High", NULL, NULL, NULL,0, priomenuCB, (XtPointer)2}
*/
};

CmdInterfaceStruct TimeMenu[] =
{
	{1, "15 Minutes",   NULL, NULL, NULL,0, timemenuCB, (XtPointer)0},
   {1, "30 Minutes", NULL, NULL, NULL,0, timemenuCB, (XtPointer)1},
   {1, "1 hour", NULL, NULL, NULL,0, timemenuCB, (XtPointer)3},
   {1, "2 hour", NULL, NULL, NULL,0, timemenuCB, (XtPointer)4},
   {1, "Disabled", NULL, NULL, NULL,0, timemenuCB, (XtPointer)5}
};

CmdInterfaceStruct OptionMenu[] =
{
	{0, "Options",   "Ctrl <Key> p", "Ctl+p", NULL,0, optionmenuCB, (XtPointer)0},
	{0, "Priority",   NULL, NULL, (int *)PrioMenu, 2, optionmenuCB, (XtPointer)1},
	{0, "Time Limit",   NULL, NULL, (int *)TimeMenu, 5, optionmenuCB, (XtPointer)2}
}; 

CmdInterfaceStruct HelpMenu[] =
{
	{0, "About",   "Ctrl <Key> h", "Ctl+h", NULL,0, helpmenuCB, (XtPointer)0}
};
	
void ncq_update();
void ncq_run_ncl();

pid_t Ncq_ncl_pid = -1;
int m_nclrun = 0;
time_t m_runtime = 30*60*1000;
int m_priority = 0;
char Ncq_ncl_exe[MAX_PATH];
void ncq_unxdispmsg();
XtIntervalId timer1 = -1, timer2 = -1, timer3 = -1;
int Ncq_poll_ncl = 1;
int ncq_poll_time = 1000;
/*
.....we need delocate memory space when exit, use global value because
.....seems that XmNdestroyCallback for toolbar is not working
*/
char *ncq_image_data[9] = {NULL, NULL, NULL, NULL, NULL,
										NULL, NULL, NULL, NULL};
XImage *toolbar_image[9] = {NULL, NULL, NULL, NULL, NULL,
										NULL, NULL, NULL, NULL};

int NCQ_common_open = 0;
char *NCQ_comptr;

int NCL_common_key, NCQ_shmid;
int NCQ_filenum;
int NCQ_comm_pos = 0;
char **NCQ_ppfile = NULL;
int *NCQ_ppf_post = NULL;

extern int NCQ_time_limit;

/***********************************************************************
**
**   SUBROUTINE: ncq_write_comint(int *data, int pos, int size)
**
**   FUNCTION:  This routine write a integer
**						into common memory area at position 'pos'
**
**   INPUT:  pos: position of data to be read
**				size: size of data to be write
**				data: data to be written
**				 
**   OUTPUT:  none
**
***********************************************************************/
ncq_write_comint(data,pos,size)	
int *data,pos,size;
{
	int i;
	char *temp = (char *)data;
	if (NCQ_common_open==0)
		return;
	for (i=pos; i<size; i++)
		NCQ_comptr[i] = temp[i];
}

/***********************************************************************
**
**   SUBROUTINE: ncq_write_comblk(NCLInfo *info)
**
**   FUNCTION:  This routine write NCL information from common memory area
**
**   INPUT:  info:         NCLInfo structure contain NCL information
**
**   OUTPUT: none
**
***********************************************************************/
void ncq_write_comblk(info)
NCLInfo *info;
{
	int i;
	int pos, isize;
	char *temp;
/*
	char *temp = (char *)info;
*/
	if (NCQ_common_open)
	{
		isize = sizeof (int);
/*
......the first word is for NCL process ID, read from after first word
*/
		pos = isize;
		temp = (char *)&(info->flag);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + isize;

		temp = (char *)&(info->ppfile);
		for (i=0; i<256; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + 256;

		temp = (char *)&(info->current);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + isize;

		temp = (char *)&(info->highest);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + isize;

		temp = (char *)&(info->lines);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + isize;

		temp = (char *)&(info->macro);
		for (i=0; i<64; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + 64;

		temp = (char *)&(info->warn);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
		pos = pos + isize;

		temp = (char *)&(info->error);
		for (i=0; i<isize; i++)
			NCQ_comptr[i+pos] = temp[i];
	}
}				

/***********************************************************************
**
**   SUBROUTINE: ncq_read_comint(int *data, int pos)
**
**   FUNCTION:  This routine read a integer
**						from common memory area at position 'pos'
**
**   INPUT:  pos: position of data to be read
**				 
**   OUTPUT:  data: data read
**
***********************************************************************/
ncq_read_comint(data,pos)
int *data,pos;
{
	int i;
	char *temp = (char *)data;
	if (NCQ_common_open==0)
		return;
	for (i=0; i<sizeof (int); i++)
		temp[i] = NCQ_comptr[pos+i];
}

/***********************************************************************
**
**   SUBROUTINE: ncq_read_commsg(int *jobno, int *err, int *warn, char *ldate, char *ltim)
**
**   FUNCTION:  This routine read NCL pp file execution
**						information from common memory area
**
**   INPUT:  none
**				 
**   OUTPUT:  jobno: job number executable now
**				 err: error executable the job
**					warn: warnings executable the job
**			ldate, ltim: date and time	
**
***********************************************************************/
ncq_read_commsg(jobno, err, warn, ldate, ltim)
int *jobno, *err, *warn;
char *ldate, *ltim;
{
	char *temp;
	int i;
	if (NCQ_common_open==0)
		return;
/*
......the first word is for NCL process ID, 
......the second part is NCLInfo for NCL running data,
......read from after NCLInfo
*/
	if (NCQ_comm_pos==0)
		NCQ_comm_pos = sizeof (int) + sizeof (NCLInfo);
	temp = (char *)jobno;
	for (i=0; i<sizeof (int); i++)
		temp[i] = NCQ_comptr[NCQ_comm_pos+i];
	if (*jobno<=0)	
	{
		*jobno = 0;
		return 0;
	}
	if (*jobno>NCQ_filenum)
	{
		*jobno = 0;
		return 0;
	}
	NCQ_comm_pos = NCQ_comm_pos + sizeof(int);
	temp = (char *)err;
	for (i=0; i<sizeof (int); i++)
		temp[i] = NCQ_comptr[NCQ_comm_pos+i];
	NCQ_comm_pos = NCQ_comm_pos + sizeof(int);
	
	temp = (char *)warn;
	for (i=0; i<sizeof (int); i++)
		temp[i] = NCQ_comptr[NCQ_comm_pos+i];
	NCQ_comm_pos = NCQ_comm_pos + sizeof(int);

	for (i=0; i<12; i++)
		ldate[i] = NCQ_comptr[NCQ_comm_pos+i];
	ldate[11] = '\0';
	NCQ_comm_pos = NCQ_comm_pos + 12;

	for (i=0; i<9; i++)
		ltim[i] = NCQ_comptr[NCQ_comm_pos+i];
	ltim[8] = '\0';
	NCQ_comm_pos = NCQ_comm_pos + 9;
}
/***********************************************************************
**
**   SUBROUTINE: ncq_read_comblk(NCLInfo *info)
**
**   FUNCTION:  This routine read NCL information from common memory area
**
**   INPUT:  none
**
**   OUTPUT: info:         NCLInfo structure contain NCL information
**
***********************************************************************/
ncq_read_comblk(info)
NCLInfo *info;
{
	int i;
	int pos,isize;
	char *temp;
	if (NCQ_common_open)
	{
		isize = sizeof (int);
/*
......the first word is for NCL process ID, read from after first word
*/
		pos = isize;
		temp = (char *)&(info->flag);
		for (i=0; i<isize; i++)
			temp[i] = NCQ_comptr[i+pos];
		pos = pos + isize;

		temp = (char *)&(info->ppfile);
		for (i=0; i<256; i++)
			temp[i] = NCQ_comptr[i+pos];
		pos = pos + 256;

		temp = (char *)&(info->current);
		for (i=0; i<isize; i++)
			temp[i] = NCQ_comptr[i+pos];
		pos = pos + isize;

		temp = (char *)&(info->highest);
		for (i=0; i<isize; i++)
			temp[i] = NCQ_comptr[i+pos];
		pos = pos + isize;

		temp = (char *)&(info->lines);
		for (i=0; i<isize; i++)
			temp[i] = NCQ_comptr[i+pos];
		pos = pos + isize;

		temp = (char *)&(info->macro);
		for (i=0; i<64; i++)
			temp[i] = NCQ_comptr[i+pos];
		pos = pos + 64;

		temp = (char *)&(info->warn);
		for (i=0; i<isize; i++)
			temp[i] = NCQ_comptr[i+pos];
		pos = pos + isize;

		temp = (char *)&(info->error);
		for (i=0; i<isize; i++)
			temp[i] = NCQ_comptr[i+pos];
	}
}
/***********************************************************************
**
**   SUBROUTINE: ncq_close_common(int com_key)
**
**   FUNCTION:  This routine close the common memory area
**
**   INPUT:  com_key:			common area key
**
**   OUTPUT: none
**
***********************************************************************/
ncq_close_common()
{
	if (NCQ_common_open==0)
		return;
	shmdt(NCQ_comptr);
	shmctl (NCQ_shmid, IPC_RMID,  NULL);
	NCQ_common_open = 0;
}
/***********************************************************************
**
**   SUBROUTINE: ncq_open_common(int com_key)
**
**   FUNCTION:  This routine creates the common memory area
**
**   INPUT:  com_key:			common area key
**
**   OUTPUT: none
**
***********************************************************************/
ncq_open_common(com_key)
int com_key;
{
	int *addr,shmflg,size;
	NCLInfo info;
	int num, i, pos;
/*
.....Try to map to global section
*/
	shmflg = 438;	/* Allow everybody Read/Write access */
/*
......one word for process ID of NCL (must write from NCL)
......one NCLInfo struct for ncl running data
......6 word for 1 pp file (start and end both have 3 word)
*/
	size = sizeof (int) + sizeof (NCLInfo) + 
					2 * NCQ_filenum * (3*sizeof (int) + (12+9)*sizeof (char));
	NCQ_shmid = shmget(com_key,size,shmflg);
/*
.....No such global section
.....Let's create it
*/
	if (NCQ_shmid>0)
/*
.....already open, it shouldn't be, anyway, close it and open again
*/
	{
		ncq_close_common();
		NCQ_shmid = shmget(com_key,size,shmflg);
		if (NCQ_shmid>0)
			goto failed;
	}
	if (NCQ_shmid < 0)
	{
		shmflg = IPC_CREAT | shmflg;
		NCQ_shmid = shmget(com_key,size,shmflg);
		if (NCQ_shmid < 0) goto failed;
/*
........Attach to global section
*/
		addr = 0;
		shmflg = 0;
		NCQ_comptr = (char *)shmat(NCQ_shmid,addr,shmflg);
		if ((int)NCQ_comptr == -1) goto failed;
		num = 0;
		ncq_write_comint(&num, 0, sizeof (int));	
		info.flag = 0;
		ncq_write_comblk(&info);
		pos = sizeof (int) + sizeof (NCLInfo);
		for (i=0; i<NCQ_filenum*2;i++)
		{
			ncq_write_comint(&num, pos, sizeof (int));	
			pos = pos + sizeof (int);
			ncq_write_comint(&num, pos, sizeof (int));	
			pos = pos + sizeof (int);
			ncq_write_comint(&num, pos, sizeof (int));	
			pos = pos + sizeof (int);
/*
.....19 byte for date and time
*/
			pos = pos + 19;
		}
	}
	NCQ_common_open = 1;
	NCQ_comm_pos = 0;
	goto done;
/*
.....Error trying to allocate memory
*/
failed:;
	printf("*FATAL* Could not malloc common memory for NCQ/NCL\n");
/*
.....End of routine
*/
done:;
	return;
}
/***********************************************************************
c
c   FUNCTION: ncq_getnclinfo()
c         get date from common memory and update the monitor
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
ncq_getnclinfo()
{
	NCLInfo info;
	char msg[256], msg1[256], msg2[256];
	int i, counter = 0;
	int stat = 0;
	if (NCQ_common_open==0)
		return;
	ncq_read_comblk(&info);
	if (info.flag==0)
		return stat;
	if (info.flag==1)
	{
		if (strcmp(info.ppfile, "NCL EXIT")==0)
		{
			ncq_update();
			Ncq_ncl_pid = -1;
			m_nclrun = 0;
			XmTextSetString(stat_area, "Idle");
			XtVaSetValues(stat_area, XmNforeground, red_color_px, NULL);
			if (timer1!=-1)
			{
				XtRemoveTimeOut(timer1);
				timer1 = -1;
			}
			if (timer2!=-1)
			{
				XtRemoveTimeOut(timer2);
				timer2 = -1;
			}
			if (timer3!=-1)
			{
				XtRemoveTimeOut(timer3);
				timer3 = -1;
			}
/*
......check last time before exit NCL
*/
			ncq_getnclmsg();
			ncq_close_common();
/*
.....free the job list too
*/
			if (NCQ_ppfile!=NULL)
			{
				for (i=0; i<NCQ_filenum; i++)
					free(NCQ_ppfile[i]);
				free (NCQ_ppfile);
				NCQ_ppfile = NULL;
			}
			if (NCQ_ppf_post!=NULL)
			{
				free (NCQ_ppf_post);
				NCQ_ppf_post = NULL;
			}
			ncq_cls_monitorCB(monitor_dialog, 0, 0);
			Ncq_ncl_pid = -1;
		}
		else if (strcmp(info.ppfile, "NCL UPDATE")==0)
			ncq_update();
	}
	else if (info.flag==3)
	{
		ncq_mfupd_nclinfo(info);
	}
}
/***********************************************************************
c
c   FUNCTION: ncq_get_nclid()
c         get NCL running ID
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
ncq_get_nclid()
{
	int id = -1;
	ncq_read_comint(&id, 0);
	return id;
}

/***********************************************************************
c
c   FUNCTION: ncq_getnclmsg()
c         get message date from common memory and display those message
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
ncq_getnclmsg()
{
	char msg[256], filename[41], ldate[12], ltim[9];
	char msg1[256], msg2[256];
	int err, warn, jobno;
	int count = 0;
	if (NCQ_common_open==0)
		return;
read:;
	ncq_read_commsg(&jobno, &err, &warn, ldate, ltim);
	count++;
	if (jobno==0)
		return 0;
	ncq_short_filename(NCQ_ppfile[jobno-1], filename, 40);
	strcpy(NCQ_current_file, filename);
	if (err==-1)
		sprintf(msg, "%s started at %s %s.", filename, ldate, ltim);
	else if (err>0)
	{
		if (warn>0)
		{
			sprintf(msg, "%s ended with %d errors and %d warnings at %s %s.", 
						filename,err, warn, ldate, ltim);
		}
		else
		{
			sprintf(msg, "%s ended with %d errors at %s %s.", 
						filename, err, ldate, ltim);				
		}
/*
.....update the que window
*/
		ncq_update();
	}
	else if (err==0)
	{
		if (warn>0)
			sprintf(msg, "%s ended with %d warnings at %s %s.", 
							filename, warn, ldate, ltim);
		else
			sprintf(msg, "%s ended at %s %s.", filename, ldate, ltim);
/*
.....update the que window
*/
		ncq_update();
	}
	ncq_mfstatus_message(msg);
	if ((err!=-1) && (NCQ_ppf_post[jobno-1]=='1'))
	{
/*
.....after pp file finished, it it run post process, check pworks.log last 2
line
.....and displayed in status area
*/
		int stat = ncq_getpost_msg(NCQ_ppfile[jobno-1], msg1, msg2);
		if (stat!=-1)
		{
			if (msg1[0]!='\0')
				ncq_mfstatus_message(msg1);
			if (msg2[0]!='\0')
				ncq_mfstatus_message(msg2);
		}
	}
/*
.....continue read until there is no NCL message data
*/
	if (count<NCQ_filenum*2)
		goto read;
	return 1;
}
/**********************************************************************
**    I_FUNCTION :  ncq_flush()
**       Flush the NCQ event.
**       
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_flush()
{
	int i;
	XEvent x_event;
	i = XPending(XtDisplay(NCQ_Parent));
	if (i > 0)
	{
		while (XCheckWindowEvent(XtDisplay(NCQ_Parent), XtWindow(Main_Window),
					NoEventMask, &x_event))
		{
			XtDispatchEvent(&x_event);
		}
	}
	return;
}
					
void ncq_modncl_priority(prio)
int prio;
{
	if (Ncq_ncl_pid==-1)
		return;
	setpriority(PRIO_PROCESS, Ncq_ncl_pid, prio);
} 
/**********************************************************************
**    I_FUNCTION :  ncq_kill_ncl()
**       Kill the NCL process runs from NCQ
**       
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncq_kill_ncl()
{
	int i;
	if (Ncq_ncl_pid!=-1)
	{
		kill (Ncq_ncl_pid, 9);
		Ncq_ncl_pid = -1;
		m_nclrun = 0;
		if (timer1!=-1)
		{
			XtRemoveTimeOut(timer1);
			timer1 = -1;
		}
		if (timer2!=-1)
		{
			XtRemoveTimeOut(timer2);
			timer2 = -1;
		}
		if (timer3!=-1)
		{
			XtRemoveTimeOut(timer3);
			timer3 = -1;
		}
		XmTextSetString(stat_area, "Unknown");
		XtVaSetValues(stat_area, XmNforeground, red_color_px, NULL);
		ncq_update();
		ncq_close_common();
/*
.....free the job list too
*/
		if (NCQ_ppfile!=NULL)
		{
			for (i=0; i<NCQ_filenum; i++)
				free(NCQ_ppfile[i]);
			free (NCQ_ppfile);
			NCQ_ppfile = NULL;
		}
		if (NCQ_ppf_post!=NULL)
		{
			free (NCQ_ppf_post);
			NCQ_ppf_post = NULL;
		}
		ncq_cls_monitorCB(monitor_dialog, 0, 0);
	}
}

/**********************************************************************
**    I_FUNCTION :  timeoutCB(XtPointer closure, XtIntervalId* id)
**       Callback for timeout function
**       
**    PARAMETERS
**       INPUT  :
**          closure: user date, not used here
**				id: timer ID
**       OUTPUT :
**          none
**    RETURNS      
**				none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
XtTimerCallbackProc timeoutCB(closure, id)
XtPointer closure;
XtIntervalId* id;
{
	char msg[256];
	int i, stat, filenum;
	static int poll_time = 0;
	if ((*id!=timer1) && (*id!=timer2) && (*id!=timer3))
	{
/*
.....it must be the old time out left, ignore them
.....if not, it could abord the new process
*/
		return;
	}
	if (*id==timer1)
	{
		if ((Ncq_ncl_pid<=0)&&(m_nclrun!=0))
/*
......get the NC process id
*/
		{
			Ncq_ncl_pid = ncq_get_nclid();
/*
......if we check 10 time (10*ncq_poll_time) and still can't get
......NCL message, then it must be call the wrong executable
......or older version NCL.exe without message communication
......kill this time and set automaic update NCQ every 10 second
*/
			if (Ncq_ncl_pid<=0) 
			{
/*
.....haven't get the NCL id yet, so just return
.....could be system slow or runs a run executable
.....so we will wait 
*/
				poll_time++;
				if (poll_time==10)
				{
					if (timer1!=-1)
					{
						XtRemoveTimeOut(timer1);
						timer1 = -1;
					}
					if (timer2!=-1)
					{
						XtRemoveTimeOut(timer2);
						timer2 = -1;
					}
					timer3 = XtAppAddTimeOut(NCQ_App, 10000,
									(XtTimerCallbackProc)timeoutCB, NULL);
					Ncq_poll_ncl = 0;
					XmTextSetString(stat_area, "Unknown");
					XtVaSetValues(stat_area, XmNforeground, yl_color_px, NULL);
				}
				else
				{
					if (timer1!=-1)
						XtRemoveTimeOut(timer1);
					timer1 = XtAppAddTimeOut(NCQ_App, ncq_poll_time,
								(XtTimerCallbackProc)timeoutCB, NULL);
				}
				return;
			}
			else
				poll_time = 0;
		}
/*
.....check the common memory to get NCL status info
*/
		ncq_getnclmsg();
/*
.....check monitor info and message info
*/
		ncq_getnclinfo();
		if (timer1!=-1)
			XtRemoveTimeOut(timer1);
		timer1 = XtAppAddTimeOut(NCQ_App, ncq_poll_time,
				(XtTimerCallbackProc)timeoutCB, NULL);
	}
	else if (*id==timer2)
	{
/*
.....reach NCL run time limit
*/
		ncq_getnclmsg();
		ncq_getnclinfo();
/*
.....Force kill NCL
*/
		if (m_nclrun==0) return 0;
		if (ncq_if_ncl_running())
		{
			ncq_kill_ncl();
			if (NCQ_current_file[0] != '\0')
			{
				sprintf(msg, "%s ended due to time limit exceeded",
									NCQ_current_file);
			}
			else
				strcpy(msg, "NCL aborted due to time limit exceeded");
			ncq_mfstatus_message(msg);
			ncq_cls_monitorCB(monitor_dialog, 0, 0);
			Ncq_ncl_pid = -1;
			ncq_run_ncl();
		}
		return 0;
	}
	else if (*id==timer3)
	{
/*
.....Update NCL
*/
      if (m_nclrun)
      {
			ncq_update();
/*
......if after 10 second, the ncl.que still have seem number of PP files,
......it means it don't run NCL (or NCL abort before even executed ncl.que)
......or it run a wrong executable
......in this case, show NCL abort with unknow status
......and consider done.
*/
			XtVaGetValues (listbox, XmNitemCount, &filenum, NULL);
/*
.....check if ncl.que is empty now, if yes, update NCQ
.....and m_nclrun = 0 (so that we can consider NCL is done
.....until next time user run NCL) and update NCQ. If ncl.que is not empty,
.....update NCQ
*/
			if ((ncl_que_is_empty()) || (filenum==NCQ_filenum))
			{
				m_nclrun = 0;
				if (filenum==NCQ_filenum)
				{
/*
......if after 10 second, the ncl.que still have same number of PP files,
......it means it don't run NCL (or NCL abort before even executed ncl.que)
......or it run a run executable
......in this case, show NCL abort with unknow status
*/
					ncq_mfstatus_message("Error trying to run NCLEXE");		
					ncq_mfstatus_message(
						"Please make sure that the NCLEXE variable points to the NCL executable (ncl.exe)");		
					XmTextSetString(stat_area, "Unknown");
					XtVaSetValues(stat_area, XmNforeground, yl_color_px, NULL);
				}
				else
				{
					XmTextSetString(stat_area, "Idle");
					XtVaSetValues(stat_area, XmNforeground, red_color_px, NULL);
				}
				if (timer1!=-1)
				{
					XtRemoveTimeOut(timer1);
					timer1 = -1;
				}
				if (timer2!=-1)
				{
					XtRemoveTimeOut(timer2);
					timer2 = -1;
				}
				if (timer3!=-1)
				{
					XtRemoveTimeOut(timer3);
					timer3 = -1;
				}
				ncq_close_common();
/*
.....free the job list too
*/
				if (NCQ_ppfile!=NULL)
				{
					for (i=0; i<NCQ_filenum; i++)
						free(NCQ_ppfile[i]);
					free (NCQ_ppfile);
					NCQ_ppfile = NULL;
				}
				if (NCQ_ppf_post!=NULL)
				{
					free (NCQ_ppf_post);
					NCQ_ppf_post = NULL;
				}
				ncq_cls_monitorCB(monitor_dialog, 0, 0);
				ncq_flush();
			}
			else
			{
				XtRemoveTimeOut(timer3);
				timer3 = XtAppAddTimeOut(NCQ_App, 10000,
								(XtTimerCallbackProc)timeoutCB, NULL);
			}
		}
		return;
	}
/*
......check if NCL abort, if so, it is not normal exit
.....normal exit handled by ncq_getnclinfo
*/
	if ((ncq_if_ncl_running()==0) && (m_nclrun!=0) && (Ncq_poll_ncl==1))
	{
		ncq_getnclmsg();
		ncq_getnclinfo();
		ncq_update();
		if (m_nclrun!=0)
		{
			m_nclrun = 0;
			ncq_close_common();
/*
.....free the job list too
*/
			if (NCQ_ppfile!=NULL)
			{
				for (i=0; i<NCQ_filenum; i++)
					free(NCQ_ppfile[i]);
				free (NCQ_ppfile);
				NCQ_ppfile = NULL;
			}
			if (NCQ_ppf_post!=NULL)
			{
				free (NCQ_ppf_post);
				NCQ_ppf_post = NULL;
			}
			if (NCQ_current_file[0] != '\0')
			{
				sprintf(msg, "%s ended with status unknown.", 
							NCQ_current_file);
			}
			else
				strcpy(msg, "NCL aborted with status unknown.");
			ncq_mfstatus_message(msg);
			XmTextSetString(stat_area, "Unknown");
			if (timer1!=-1)
			{
				XtRemoveTimeOut(timer1);
				timer1 = -1;
			}
			if (timer2!=-1)
			{
				XtRemoveTimeOut(timer2);
				timer2 = -1;
			}
			if (timer3!=-1)
			{
				XtRemoveTimeOut(timer3);
				timer3 = -1;
			}
			ncq_cls_monitorCB(monitor_dialog, 0, 0);
			Ncq_ncl_pid = -1;
			ncq_run_ncl();
		}
	}
}				

/**********************************************************************
**    I_FUNCTION :  FileOkCB(widget,client_data,call_data)
**       Processes the OK button in the File Selection dialog.  Returns
**       the filename selected by the user.
**    PARAMETERS
**       INPUT  :
**          widget      = File Selection dialog shell.
**          client_data = Pointer to structure which will contain the
**                        selected filename.
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
static void FileOkCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
   char *filen, *dir;
   FSELECT *p=(FSELECT *)client_data;

/*
.....Get the directory path and filename entered
*/
   XmFileSelectionBoxCallbackStruct *cbs =
   (XmFileSelectionBoxCallbackStruct *)call_data;
   if (!XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&filen))
   {
      p->file[0] = '\0';
   }
   else
   {
      strcpy(p->file,filen);
      XtFree(filen);
   }
   p->sel = 1;
/*
.....Destroy the dialog
*/
   XtUnmanageChild(widget);
/*   XtDestroyWidget(widget); */
}
/**********************************************************************
**    I_FUNCTION :  FileCancelCB(widget,client_data,call_data)
**       Processes the CANCEL button in the File Selection dialog.
**    PARAMETERS
**       INPUT  :
**          widget      = File Selection dialog shell.
**          client_data = Pointer to structure which will contain the
**                        blanked out filename.
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the File Selection dialog shell.
**    WARNINGS     : none
*********************************************************************/
static void FileCancelCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
/*
.....Set the filename to NULL
*/
   FSELECT *p = (FSELECT *)client_data;
   p->sel = 1;
   p->file[0] = '\0';
/*
.....Destroy the dialog
*/
   XtUnmanageChild(widget);
/*   XtDestroyWidget(widget); */
}
/**********************************************************************
**    I_FUNCTION :  ncq_savoptCB(widget,client_data,call_data)
**       Processes the OK button in the Option dialog.
**    PARAMETERS
**       INPUT  :
**          widget      = dialog shell.
**          client_data = 
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the Option dialog shell.
**    WARNINGS     : none
*********************************************************************/
static void ncq_savoptCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	char fname[MAX_PATH], *text;
	int tmp, len;
	Widget dlg = (Widget) client_data;

	text = XmTextGetString(opt_edt[0]);
	if (text!=NULL)
	{
		ncq_ipglen = atoi(text);
		len = strlen(text);
		ncq_linbuf[9] = text[len-1];
		if (len-2>=0)
			ncq_linbuf[8] = text[len-2];
		else
			ncq_linbuf[8] = '0';
		if (len-3>=0)
			ncq_linbuf[7] = text[len-3];
		else
			ncq_linbuf[7] = '0';
		XtFree(text);
	}
	text = XmTextGetString(opt_edt[1]);
	if (text!=NULL)
	{
		ncq_lpri = text[0];
		ncq_lpri = toupper (ncq_lpri);
		ncq_linbuf[6] = ncq_lpri;
		XtFree(text);
	}
	tmp = XmToggleButtonGetState(opt_chk[0]); 
	if (tmp)
		ncq_linbuf[0] = '1';
	else
		ncq_linbuf[0] = '0';

	tmp = XmToggleButtonGetState(opt_chk[1]); 
	if (tmp)
		ncq_linbuf[1] = '1';
	else
		ncq_linbuf[1] = '0';

	tmp = XmToggleButtonGetState(opt_chk[2]); 
	if (tmp)
		ncq_linbuf[4] = '1';
	else
		ncq_linbuf[4] = '0';

	tmp = XmToggleButtonGetState(opt_chk[4]); 
	if (tmp)
		ncq_linbuf[5] = '1';
	else
		ncq_linbuf[5] = '0';

	tmp = XmToggleButtonGetState(opt_chk[3]); 
	if (tmp)
		ncq_linbuf[2] = '0';
	else
		ncq_linbuf[2] = '1';

	tmp = XmToggleButtonGetState(opt_chk[5]); 
	if (tmp)
		ncq_linbuf[3] = '1';
	else
		ncq_linbuf[3] = '0';

	NCQ_monitor = XmToggleButtonGetState(opt_chk[6]); 

	formEof = True;
/*
.....Destroy the dialog
*/
   XtUnmanageChild(dlg);
   XtDestroyWidget(dlg); 
	option_dialog = NULL;
}
/**********************************************************************
**    I_FUNCTION :  ncq_opt_cancelCB(widget,client_data,call_data)
**       Processes the CANCEL button in the Option dialog.
**    PARAMETERS
**       INPUT  :
**          widget      = dialog shell.
**          client_data = 
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Destroys the Option dialog shell.
**    WARNINGS     : none
*********************************************************************/
static void ncq_opt_cancelCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	Widget dlg = (Widget) client_data;
	formEof = True;
/*
.....Destroy the dialog
*/
   XtUnmanageChild(dlg);
   XtDestroyWidget(dlg); 
	option_dialog = NULL;
}

/**********************************************************************
**    I_FUNCTION : yes_or_noCB()
**       action for yes or no choice
**    PARAMETERS
**       INPUT  :
**          widget      = Ignored.
**          client_data = Ignored.
**          call_data   = Motif callback structure.  Contains
**                        answer selected by the user.
**       OUTPUT :
**             none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void yes_or_noCB(w, client_data, call_data)
Widget w;
XtPointer client_data,call_data;
{
	XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)call_data;
	switch(cbs->reason)
	{
		case XmCR_OK:
			yes_or_no = 1;
			XtPopdown(XtParent(w));
			break;
		case XmCR_CANCEL:
			yes_or_no = 0;
			XtPopdown(XtParent(w));
			break;
	}
	ncq_mfanswer = 1;
}

/**********************************************************************
**    I_FUNCTION : ncq_yes_or_no(parent, msg, title)
**			popup a dialog box for yes or no choice
**    PARAMETERS   
**       INPUT  : msg: question label
**						title: dialog title
**						parent: parent window
**       OUTPUT :  
**         			none 
**    RETURNS      : none
**    SIDE EFFECTS : bring out a dialog box
**    WARNINGS     : none
*********************************************************************/
ncq_yes_or_no(parent, msg, title)
Widget parent;
char *msg, *title;
{
	Widget dialog;
	XEvent x_event;
	Arg args[5];
	XmString m;
	int n  = 0;
	XmString yes = XmStringCreateSimple("Yes");
	XmString no = XmStringCreateSimple("No");
/*
.....default answer to "No"
*/
	yes_or_no = 0;
/*
...create a yesno dialog
*/
	m = XmStringCreateSimple(msg);
	XtSetArg(args[n], XmNautoUnmanage, False); n++;
	XtSetArg(args[n], XmNmessageString, m); n++;
	XtSetArg(args[n], XmNokLabelString, yes); n++;
	XtSetArg(args[n], XmNcancelLabelString, no); n++;
	if(parent==UU_NULL)
	{
		dialog = (Widget) XmCreateQuestionDialog(NCQ_Parent, 
											title, args, n);
	}
	else
	{
		dialog = (Widget) XmCreateQuestionDialog(parent, 
											title, args, n);
	}
	XtVaSetValues(XtParent(dialog),XmNtitle,title,NULL);
/*
...callback
*/
	XtAddCallback(dialog, XmNokCallback, yes_or_noCB, (XtPointer)NULL);
	XtAddCallback(dialog, XmNcancelCallback, yes_or_noCB, (XtPointer)NULL);
	XmStringFree(m);
	XmStringFree(no);
	XmStringFree(yes);

	XtUnmanageChild((Widget)XmMessageBoxGetChild((Widget)dialog,
									XmDIALOG_HELP_BUTTON)); 
	XtManageChild(dialog);
	XtPopup(XtParent(dialog), XtGrabNone);
	ncq_mfanswer = 0;
	while (!ncq_mfanswer)
	{
		XtAppNextEvent(NCQ_App ,&x_event);
		XtDispatchEvent(&x_event);
	}
	return yes_or_no;
}
/**********************************************************************
**    I_FUNCTION :  ncq_mf_filename(title,filter,filename,nc)
**       Opens a File Selection dialog and returns the user selected
**       filename.
**    PARAMETERS
**       INPUT  :
**          title     = Title of File Selection dialog.
**          filter    = Filename filter to use for list of available
**                      files.
**       OUTPUT :
**          filename  = Name of selected file.
**          nc        = Number of chars in 'filename'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncq_mf_filename(parent, title, ext, filter,filename,nc)
Widget parent;
char *title,*filter,*filename, *ext;
int *nc;
{
   int i, n,jmpflag;
   char buf[80];
	char *pp;
   Arg args[20];
   Widget list;
   XmString labstr;
   XEvent event;
   FSELECT Fselect;
/*
.....Create file selection dialog
*/
   n = 0;
   labstr = XmStringCreateSimple(filter);
   XtSetArg(args[n],XmNdirMask,labstr); n++; 
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
   if(parent==NULL)
   {
      mine = (Widget) XmCreateFileSelectionDialog
         (NCQ_Parent,"file_select",args,n);
   }
   else
	{
      mine = (Widget) XmCreateFileSelectionDialog
         (parent,"file_select",args,n);
   }
   XtVaSetValues(XtParent(mine),XmNtitle,title,NULL);
   XmStringFree(labstr);
#if UU_COMP == UU_SUN
   list = (Widget)XmFileSelectionBoxGetChild(mine, XmDIALOG_LIST);
   XtVaSetValues(list, XmNscrollBarDisplayPolicy , XmSTATIC,
            NULL);
#endif
/*
.....Get rid of the HELP button
*/
	XtUnmanageChild((Widget)XmFileSelectionBoxGetChild(mine,XmDIALOG_HELP_BUTTON));
/*
.....Add Callbacks
*/
   Fselect.file = filename;
   XtAddCallback(mine,XmNcancelCallback,FileCancelCB,&Fselect);
   XtAddCallback(mine,XmNokCallback,FileOkCB,&Fselect);
/*
.....Manage the File Selection Dialog
*/
   XtManageChild(mine);

/*
.....Loop until user selects a file
*/
   Fselect.sel = 0;
   do
   {
      XtAppNextEvent(NCQ_App,&event);
      XtDispatchEvent(&event);
   } while (Fselect.sel==0);
/*
.....Return filename
*/
   *nc = strlen(Fselect.file);
done:;
	return;
}
/*******************************************************************
**   E_FUNCTION : file_text_returnCB(widget, client_data, call_data)
**              Callback function "File" menu
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = not used
**          call_data   = not used
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void file_text_returnCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	char fname[MAX_PATH], *text;
	text = XmTextGetString(file_fld);
	if (text!=NULL)
	{
		strcpy(fname, text);
		ncq_add(fname,strlen(fname));
		XtFree(text);
	}
}
/*******************************************************************
**   E_FUNCTION : selectchgCB(widget, client_data, call_data)
**              Callback function for selecting a file from listbox
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = not used
**          call_data   = Motif callback structure.
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void selectchgCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	XmListCallbackStruct* ptr =
				(XmListCallbackStruct*)call_data;
/*
.....if deselected,set  pos=-1
*/
	if (cur_sel==ptr->item_position - 1)
	{
		cur_sel = -1;
		return;
	}
	cur_sel = ptr->item_position - 1;
}
/*******************************************************************
**   E_FUNCTION : appexit()
**              Handle "exit" funtion for NCQ
**   PARAMETERS
**       INPUT  : None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void ncq_filedelete()
{
	char fname[MAX_PATH], *text;
	text = XmTextGetString(file_fld);
	if (text!=NULL)
	{
		strcpy(fname, text);
		ncq_del(fname,strlen(fname));
		XtFree(text);
	}
}
/*******************************************************************
**   E_FUNCTION : ncq_fileopen()
**              Handle "file Open" function
**   PARAMETERS
**       INPUT  :
**          none
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
ncq_fileopen()
{
	char name[MAX_PATH];
	static int fopn_flag = 0;
	int nc;
	if (fopn_flag) return;
	fopn_flag = 1;
	name[0] = '\0';
	ncq_mf_filename(NULL, "NCQ open file", "*.pp",
		"*.pp", name, &nc);
	if (nc!=0)
	{
		XmTextSetString(file_fld, name);
		XmTextSetInsertionPosition(file_fld, nc);
	}
	fopn_flag = 0;
	if (nc!=0)
		ncq_add (name,strlen(name));
}

/*******************************************************************
**   E_FUNCTION : ncq_mfloadque()
**              Handle "Load Queue" function
**   PARAMETERS
**       INPUT  :
**          none
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
ncq_mfloadque()
{
	char name[MAX_PATH];
	static int fopn_flag = 0;
	int nc;
	if (fopn_flag) return;
	fopn_flag = 1;
	name[0] = '\0';
	ncq_mf_filename(NULL, "NCQ Load Queue", "*.que",
		"*.que", name, &nc);
	if (nc!=0)
	{
		ncq_loadque_file (name);
	}
	fopn_flag = 0;
}
/*******************************************************************
**   E_FUNCTION : ncq_mfsaveque()
**              Handle "Save Queue" function
**   PARAMETERS
**       INPUT  :
**          none
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
ncq_mfsaveque()
{
	FILE *fptr;
	char name[MAX_PATH];
	int nc, n, stat;
	int fd1, fd2;
	char buf[1], msg[256];
	static int fopn_flag = 0;
	if (fopn_flag) return;
	fopn_flag = 1;
	name[0] = '\0';
	ncq_mf_filename(NULL, "NCQ Save Queue", "*.que",
		"*.que", name, &nc);
	stat = 0;
	if (nc!=0)
	{
		fptr = fopen(name,"r");
		if (fptr != 0)
		{
			sprintf(msg, "FIle %s exist, overwrite?", name);
			stat = ncq_yes_or_no(NULL, msg, "File overwrite?");
		}
		else
			stat = 1;
	}
	if (stat)
	{
		fd1 = open(ncq_file, 0);
		if (fd1 == -1)
			return -1;
		fd2 = creat(name, 0755);
		if (fd2 == -1)
			return -1;
		while ( (n=read(fd1,buf,1)) > 0)
			if (write(fd2,buf,n) != n)
				return -1;
	}
	fopn_flag = 0;
}
/*******************************************************************
**   E_FUNCTION : ncq_update()
**              Update NCQ View
**   PARAMETERS
**       INPUT  : None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void ncq_update()
{
	int ierr = 0;
	ncq_opnque (&ierr);
	if (ierr != 0) return;
	ncq_lodque();
	ncq_clsque();
}

/**************************************************************************
**
**  E_FUNCTION: ncq_cls_monitorCB((widget, clientData, callData) 
**      Called when the user closes monitor window.
**
**  PARAMETERS
**      INPUT  :  
**          widget     = Ignored.
**          clientData = Ignored.
**          callData   = Ignored.
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void ncq_cls_monitorCB(widget, clientData, callData)
Widget widget;
XtPointer clientData,callData;
{
	if (monitor_dialog!=NULL)
	{
   	XtUnmanageChild(monitor_dialog);
   	XtDestroyWidget(monitor_dialog); 
	}
   monitor_dialog = NULL;
}

/*******************************************************************
**   E_FUNCTION : ncq_clear_status()
**             Clear the status text area
**   PARAMETERS
**       INPUT  : None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
ncq_clear_status()
{
	XmTextSetString(ncq_stat_win, "");
	XFlush(XtDisplay(ncq_stat_win));
}
/*******************************************************************
**   E_FUNCTION : ncq_view_monitor()
**             Display a process monitor 
**   PARAMETERS
**       INPUT  : None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void ncq_view_monitor()
{
	XEvent x_event;
	Arg args[20];
	Widget label1, label2, pane, winid, action_area, ncq_mfcreate_action();
	int n, i, rows,cols, ifl, nc, x, y;
	char tmp[40];
	Atom watom;

	if (monitor_dialog!=NULL)
	{
		ncq_cls_monitorCB(monitor_dialog, 0, 0);
		return;
	}
/*
......create form
*/
	n = 0;
	XtSetArg(args[n],XmNallowResize,True); n++;
	XtSetArg(args[n],XmNx, 100); n++;
	XtSetArg(args[n],XmNy, 100); n++;
	monitor_dialog = XtCreatePopupShell("NCL Monitor", xmDialogShellWidgetClass,
							NCQ_Parent, args,n);
	XtVaSetValues(monitor_dialog, XmNtitle, "NCL Monitor", NULL);
	watom =  XmInternAtom(XtDisplay(NCQ_Parent),"WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(monitor_dialog, watom, ncq_cls_monitorCB, NULL);
/*
.....Create a paned window to hold the form &
.....Action Area
*/
	pane = XtVaCreateWidget("pane",xmPanedWindowWidgetClass, monitor_dialog,
				XmNsashWidth,1,
				XmNsashHeight,1,
				NULL);
/*
.....Create a Form widget
*/
	n = 0;
	rows = 3; cols = 75;
	XtSetArg(args[n],XmNfractionBase,rows*cols); n++;
	winid = XtCreateWidget("form", xmFormWidgetClass,pane,args,n);
	if (winid == NULL) return;
/*
.....first row
*/
/*
......Edit field for PP filename
*/
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	label = XtCreateManagedWidget("Program:", xmLabelWidgetClass, winid, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,1*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 10*rows); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n], XmNeditable, False);  n++;
	XtSetArg(args[n],XmNcolumns, 60); n++;
	mon_edit[0] = XtCreateManagedWidget("Monitor_Text1", xmTextWidgetClass, 
								winid, args,n);
/*
......second row
...... 3 eidt fields
*/
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	label = XtCreateManagedWidget("Current:", xmLabelWidgetClass, winid, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,2*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 10*rows); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n], XmNeditable, False);  n++;
	XtSetArg(args[n],XmNcolumns, 8); n++;
	mon_edit[1] = XtCreateManagedWidget("Monitor_Text2", xmTextWidgetClass, 
								winid, args,n);
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 26*rows); n++;
	label = XtCreateManagedWidget("Highest:", xmLabelWidgetClass, winid, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,2*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 36*rows); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n], XmNeditable, False);  n++;
	XtSetArg(args[n],XmNcolumns, 8); n++;
	mon_edit[2] = XtCreateManagedWidget("Monitor_Text3", xmTextWidgetClass, 
								winid, args,n);
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 51*rows); n++;
	label = XtCreateManagedWidget("Lines:", xmLabelWidgetClass, winid, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,2*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 60*rows); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n], XmNeditable, False);  n++;
	XtSetArg(args[n],XmNcolumns, 8); n++;
	mon_edit[3] = XtCreateManagedWidget("Monitor_Text4", xmTextWidgetClass, 
								winid, args,n);
/*
......third row
...... 3 eidt fields
*/
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	label = XtCreateManagedWidget("Macro:", xmLabelWidgetClass, winid, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,3*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 10*rows); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n], XmNeditable, False);  n++;
	XtSetArg(args[n],XmNcolumns, 8); n++;
	mon_edit[4] = XtCreateManagedWidget("Monitor_Text5", xmTextWidgetClass, 
								winid, args,n);
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 26*rows); n++;
	label = XtCreateManagedWidget("Warnings:", xmLabelWidgetClass, winid, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,3*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 36*rows); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n], XmNeditable, False);  n++;
	XtSetArg(args[n],XmNcolumns, 8); n++;
	mon_edit[5] = XtCreateManagedWidget("Monitor_Text6", xmTextWidgetClass, 
								winid, args,n);
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 51*rows); n++;
	label = XtCreateManagedWidget("Errors:", xmLabelWidgetClass, winid, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,3*cols); n++;
/*
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label); n++;
*/
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 60*rows); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n], XmNeditable, False);  n++;
	XtSetArg(args[n],XmNcolumns, 8); n++;
	mon_edit[6] = XtCreateManagedWidget("Monitor_Text7", xmTextWidgetClass, 
														winid, args,n);
	XmTextSetString(mon_edit[0], " ");
	XmTextSetString(mon_edit[4], " ");
	XmTextSetString(mon_edit[1], " ");
	XmTextSetString(mon_edit[2], " ");
	XmTextSetString(mon_edit[3], " ");
	XmTextSetString(mon_edit[5], " ");
	XmTextSetString(mon_edit[6], " ");
/*
.....Manage the Form
*/
	XtManageChild(winid);
	XtManageChild(pane);
	XtManageChild(monitor_dialog);
}
/*******************************************************************
**   E_FUNCTION : ncq_run_ncl()
**              Handle "run ncl batch" funtion for NCQ
**   PARAMETERS
**       INPUT  : None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void ncq_run_ncl()
{
	char parm[256], *com, nclexe[MAX_PATH];
	int stat, msg;
	int ncqid,i, len;
	XmStringTable str_list;
	char *str;
	char buf[MAX_PATH+20], lopts[20];
/*
.....if can run once at the time
*/
	if (m_nclrun)
		return;
/*
......check and open a common memory space for NCL info data
......to save. It will read from NCQ
......the name of the memory object will be named with
......process id to be unique
*/
	ncqid = getpid();
	NCL_common_key = ncqid;
	XtVaGetValues (listbox, XmNitemCount, &NCQ_filenum, NULL);
/*
......save the job list before run NCL
*/
	if (NCQ_filenum<=0)
		return;
	XtVaGetValues (listbox, XmNitems, &str_list, NULL);
	if (str_list==NULL)
		return;
	
	NCQ_ppfile = (char **) malloc(NCQ_filenum * sizeof (char*));
	NCQ_ppf_post = (int *) malloc(NCQ_filenum * sizeof (int));
	for (i=0; i<NCQ_filenum; i++)
	{
		buf[0] = '\0';
		XmStringGetLtoR(str_list[i], XmSTRING_DEFAULT_CHARSET, &str);
		ncq_parse_filename(str,buf,lopts);
		NCQ_ppf_post[i] = lopts[5];
		len = strlen(buf) - 1;
		while ((buf[len]==' ')||(buf[len]=='\t') || 
					(buf[len]=='\r') || (buf[len]=='\n'))
			len--;
		buf[len+1] = '\0';
		NCQ_ppfile[i] = (char *)malloc((len+1) * sizeof (char));
		strcpy(NCQ_ppfile[i], buf);
		XtFree(str);
	}

	ncq_open_common(NCL_common_key, NCQ_runver);

	com = (char*)getenv("NCLEXE");
	NCQ_current_file[0] = '\0';
	Ncq_poll_ncl = 1;
	Ncq_ncl_pid = -1;

	ncq_modncl_priority(m_priority);
	if (com!=NULL)
	{
		if ((NCQ_monitor)&&(monitor_dialog==NULL))
			ncq_view_monitor();
		strcpy(Ncq_ncl_exe, com);
		sprintf(nclexe, "%s -q=%d&", com, ncqid);
		stat = system(nclexe);
	}
	if ((com==NULL) || (stat==-1))
	{
		if (stat==-1)
			ncq_unxdispmsg(NULL, "Error trying to run NCL\n");
		m_nclrun = 0;
		ncq_close_common();
/*
.....free the job list too
*/
		if (NCQ_ppfile!=NULL)
		{
			for (i=0; i<NCQ_filenum; i++)
				free(NCQ_ppfile[i]);
			free (NCQ_ppfile);
			NCQ_ppfile = NULL;
		}
		if (NCQ_ppf_post!=NULL)
		{
			free (NCQ_ppf_post);
			NCQ_ppf_post = NULL;
		}
		ncq_cls_monitorCB(monitor_dialog, 0, 0);
		Ncq_ncl_pid = -1;
/*
.....display message on status window too
*/
		ncq_mfstatus_message("Error trying to run NCLEXE");		
		ncq_mfstatus_message(
			"Please make sure that the NCLEXE variable points to the NCL executable (ncl.exe)");		
		return;
	}
	m_nclrun = 1;
	timer1 = XtAppAddTimeOut(NCQ_App, ncq_poll_time, (XtTimerCallbackProc)timeoutCB, NULL);
	if (m_runtime>0)
		timer2 = XtAppAddTimeOut(NCQ_App, m_runtime, (XtTimerCallbackProc)timeoutCB, NULL);
	XmTextSetString(stat_area, "Running");
	XtVaSetValues(stat_area, XmNforeground, green_color_px, NULL);
}

/*******************************************************************
**   E_FUNCTION : ncq_if_ncl_running()
**              Check to see if NCL batch is running
**   PARAMETERS
**       INPUT  : None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
int ncq_if_ncl_running()
{
	char cmd[256], *tok;
	char buf[BUFSIZ], str[BUFSIZ];
   FILE *ptr;
	if (Ncq_ncl_pid==-1)
		return 0;
/*
	sprintf(cmd, "/usr/bin/ps -af");
   if ((ptr = popen(cmd, "r")) != NULL)
   {
		while (fgets(buf, BUFSIZ, ptr) != NULL)
		{
			strcpy(str, buf);
			tok = strtok(buf, " \t\n");
			if (tok==NULL) continue;
			tok = strtok(NULL, " \t\n");
			if (tok==NULL) continue;
			if (Ncq_ncl_pid==atoi(tok))
			{
				if ((strstr(str, Ncq_ncl_exe)!=NULL))
					return 1;
			}
		}                                  
	}
*/
	sprintf(cmd, "/usr/bin/ps -p %d", Ncq_ncl_pid);
   if ((ptr = popen(cmd, "r")) != NULL)
   {
		while (fgets(buf, BUFSIZ, ptr) != NULL)
		{
			strcpy(str, buf);
			tok = strtok(buf, " \t\n");
			if (tok==NULL) continue;
			if (Ncq_ncl_pid==atoi(tok))
			{
				pclose(ptr);
				return 1;
			}
		}                                  
		pclose(ptr);
	}
	return 0;
}

/*******************************************************************
**   E_FUNCTION : appexit()
**              Handle "exit" funtion for NCQ
**   PARAMETERS
**       INPUT  : None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void appexit()
{
	ncq_clsque();
	if (ncq_image_data[0])
		free (ncq_image_data[0]);
	if (ncq_image_data[1])
		free (ncq_image_data[1]);
	if (ncq_image_data[2])
		free (ncq_image_data[2]);
	if (ncq_image_data[3])
		free (ncq_image_data[3]);
	if (ncq_image_data[4])
		free (ncq_image_data[4]);
	if (ncq_image_data[5])
		free (ncq_image_data[5]);
	if (ncq_image_data[6])
		free (ncq_image_data[6]);
	if (ncq_image_data[7])
		free (ncq_image_data[7]);
	if (ncq_image_data[8])
		free (ncq_image_data[8]);
	if (ncq_image_data)
		free (ncq_image_data);

	if (toolbar_image[0]!=NULL)
		XDestroyImage(toolbar_image[0]);
	if (toolbar_image[1]!=NULL)
		XDestroyImage(toolbar_image[1]);
	if (toolbar_image[2]!=NULL)
		XDestroyImage(toolbar_image[2]);
	if (toolbar_image[3]!=NULL)
		XDestroyImage(toolbar_image[3]);
	if (toolbar_image[4]!=NULL)
		XDestroyImage(toolbar_image[4]);
	if (toolbar_image[5]!=NULL)
		XDestroyImage(toolbar_image[5]);
	if (toolbar_image[6]!=NULL)
		XDestroyImage(toolbar_image[6]);
	if (toolbar_image[7]!=NULL)
		XDestroyImage(toolbar_image[7]);
	if (toolbar_image[8]!=NULL)
		XDestroyImage(toolbar_image[8]);
	exit(0);
}

/**********************************************************************
**    I_FUNCTION :  ncq_toolbar_destroyCB(widget, client_data, call_data)
**      Callback for toolbar button Destroy. 
**      
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**                       
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_toolbar_destroyCB(widget,client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	XImage* image = (XImage* )client_data;
	if (image==NULL) return;
	XDestroyImage(image);
}

/**********************************************************************
**    I_FUNCTION :  ncq_toolbar_disarmCBCB(widget, client_data, call_data)
**      Callback for toolbar button disarm event. 
**      
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**                       
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_toolbar_disarmCB(widget,client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	void ncq_toolbar_enterCB();
	ncq_toolbar_enterCB(widget, NULL, NULL);
}
/**********************************************************************
**    I_FUNCTION :  ncq_toolbar_armCBCB(widget, client_data, call_data)
**      Callback for toolbar button arm event. 
**      
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**                       
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_toolbar_armCB(widget,client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	Colormap cmap;
	XColor current_color;
	int x, y, cx, cy, x1, x2, y1, y2;

	XSetLineAttributes(XtDisplay(widget), 
								(GC)(XDefaultGCOfScreen(XtScreen(widget))), 
								2, LineSolid,
								CapProjecting, JoinBevel);

	XtVaGetValues(widget, XmNcolormap, &cmap, NULL);
/*
	current_color.red = 255*255;
	current_color.green = 255*255;
	current_color.blue = 255*255;
*/
	current_color.red = 255*100;
	current_color.green = 255*100;
	current_color.blue = 255*100;
	XAllocColor(XtDisplay(widget), cmap, &current_color);
	XSetForeground(XtDisplay(widget),
					(GC)(XDefaultGCOfScreen(XtScreen(widget))),
					current_color.pixel);

	XtVaGetValues(widget, XmNx, &x, XmNy, &y, XmNwidth, &cx,
						 XmNheight, &cy, NULL);
/*
	x = x + 3;
	y = y - 4;
*/
	x = 3;
	y = 3;
	cx = 24 + 6;
	cy = 24 + 6;

	x1 = x; y1= y;
	x2 = x+cx; y2 = y;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);
	x1 = x; y1= y+cy;
	x2 = x; y2 = y;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);

/*
	current_color.red = 255*100;
	current_color.green = 255*100;
	current_color.blue = 255*100;
*/
	current_color.red = 255*200;
	current_color.green = 255*200;
	current_color.blue = 255*200;
	XAllocColor(XtDisplay(widget), cmap, &current_color);
	XSetForeground(XtDisplay(widget),
					(GC)(XDefaultGCOfScreen(XtScreen(widget))),
					current_color.pixel);
	x1 = x+cx; y1 = y;
	y2= y + cy; x2 = x+cx;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);
	x1 = x2; y1 = y2;
	y2= y+ cy; x2 = x;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);
	XFlush(XtDisplay(widget));
}
			
void ncq_toolbar_enterCB(widget,client_data, event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	Colormap cmap;
	XColor current_color;
	int x, y, cx, cy, x1, x2, y1, y2;
   XmProcessTraversal(widget,XmTRAVERSE_CURRENT);

	XSetLineAttributes(XtDisplay(widget), 
								(GC)(XDefaultGCOfScreen(XtScreen(widget))), 
								2, LineSolid,
								CapProjecting, JoinBevel);

	XtVaGetValues(widget, XmNcolormap, &cmap, NULL);
	current_color.red = 255*200;
	current_color.green = 255*200;
	current_color.blue = 255*200;
	XAllocColor(XtDisplay(widget), cmap, &current_color);
	XSetForeground(XtDisplay(widget),
					(GC)(XDefaultGCOfScreen(XtScreen(widget))),
					current_color.pixel);

	XtVaGetValues(widget, XmNx, &x, XmNy, &y, XmNwidth, &cx,
						 XmNheight, &cy, NULL);
/*
	x = x + 3;
	y = y - 4;
*/
	x = 3;
	y = 3;
	cx = 24 + 6;
	cy = 24 + 6;

	x1 = x; y1= y;
	x2 = x+cx; y2 = y;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);
	x1 = x; y1= y+cy;
	x2 = x; y2 = y;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);

	current_color.red = 255*100;
	current_color.green = 255*100;
	current_color.blue = 255*100;
	XAllocColor(XtDisplay(widget), cmap, &current_color);
	XSetForeground(XtDisplay(widget),
					(GC)(XDefaultGCOfScreen(XtScreen(widget))),
					current_color.pixel);
	x1 = x+cx; y1 = y;
	y2= y + cy; x2 = x+cx;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);
	x1 = x2; y1 = y2;
	y2= y+ cy; x2 = x;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);

	XFlush(XtDisplay(widget));
}
			
void ncq_toolbar_leaveCB(widget,client_data, event)
Widget widget;
XtPointer client_data;
XEvent *event;
{
	Pixel bg;
	Colormap cmap;
	XColor current_color;
	int x, y, cx, cy, x1, x2, y1, y2;
   XmProcessTraversal(file_fld, XmTRAVERSE_CURRENT);

	XtVaGetValues(widget, XmNbackground, &bg, NULL);

	XSetLineAttributes(XtDisplay(widget), 
								(GC)(XDefaultGCOfScreen(XtScreen(widget))), 
								2, LineSolid,
								CapProjecting, JoinBevel);

	XtVaGetValues(widget, XmNcolormap, &cmap, NULL);
	XSetForeground(XtDisplay(widget),
					(GC)(XDefaultGCOfScreen(XtScreen(widget))),
					bg);

	XtVaGetValues(widget, XmNx, &x, XmNy, &y, XmNwidth, &cx,
						 XmNheight, &cy, NULL);
/*
	x = x + 3;
	y = y - 4;
*/
	x = 3;
	y = 3;
	cx = 24 + 6;
	cy = 24 + 6;

	x1 = x; y1= y;
	x2 = x+cx; y2 = y;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);
	x1 = x; y1= y+cy;
	x2 = x; y2 = y;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);

	x1 = x+cx; y1 = y;
	y2= y + cy; x2 = x+cx;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);
	x1 = x2; y1 = y2;
	y2= y+ cy; x2 = x;
	XDrawLine(XtDisplay(widget), XtWindow(widget), 
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), x1,y1,x2,y2);
	XFlush(XtDisplay(widget));
}

/**********************************************************************
**    I_FUNCTION :  ncq_mfcreate_toolbar(parent,toolbars,num_toolbars)
**       Creates the toolbar area for forms
**    PARAMETERS   
**       INPUT  : 
**          parent      = Form widget to create Toolbar Area for.
**				actions     = List of Toolbar Area buttons and procedures.
**				num_actions = Number of 'toolbar' for this area.
**       OUTPUT :  
**          output
**    RETURNS      : Toolbar Area Widget.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Widget ncq_mfcreate_toolbar(parent,toolbars,num_toolbars)
Widget parent;
ToolbarItem *toolbars;
int num_toolbars;
{
	Widget toolbarBut[10];
	Widget toolbar_area,widget;
	int i, n;
	Arg args[20];
	Dimension ht, st, h, height, x, y;
	Pixmap pixmap;
	Pixel fg, bg;
	char tlabel[20];
	char *image_data;
#define TIGHTNESS 40

/*
.....Create Action Area Form
*/
	toolbar_area = XtVaCreateManagedWidget("toolbar_area",
		xmFormWidgetClass,parent,
		NULL);
/*
.....Create each of the buttons
*/
	XtVaGetValues(toolbar_area, XmNforeground, &fg,
						XmNbackground, &bg, NULL);

	for (i=0; i<num_toolbars; i++)
	{
		sprintf(tlabel, "toolbar_%d", i+1);
		n = 0;
		if (i!=0)
		{
			XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
			XtSetArg(args[n], XmNleftWidget, toolbarBut[i-1]); n++;
		}
		else
		{
			XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
		}

		image_data = 0;
		ncq_getimage(toolbars[i].bmpfile, 
									&image_data);

		XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
		XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
		if (image_data!=0)
		{
			toolbarBut[i] = XtVaCreateManagedWidget(toolbars[i].label, 
								xmDrawnButtonWidgetClass,
								toolbar_area, args,n); 
		}
		else
		{
			toolbarBut[i] = XtVaCreateManagedWidget(toolbars[i].label, 
								xmPushButtonWidgetClass,
								toolbar_area, args,n); 
		}
		XtVaGetValues(toolbarBut[i], XmNhighlightThickness, &ht,
												XmNshadowThickness, &st,
												XmNx, &x,
												XmNy, &y,
												NULL);
		if (i==0)
		{
			XtVaSetValues(toolbarBut[i], XmNwidth, 2*ht +  2*st + 24 + 4,
								XmNheight, 2*ht +  2*st + 24 + 4,
								NULL);
		}
		else
		{
			XtVaGetValues(toolbarBut[i-1], 
												XmNx, &x,
												XmNy, &y,
												NULL);
			XtVaSetValues(toolbarBut[i], XmNwidth, 2*ht +  2*st + 24 + 4,
								XmNheight, 2*ht +  2*st + 24 + 4,
								XmNx,  x+2*st + 24 + 6 + 4,
								XmNy, y,
								NULL);
		}

/*
.....Add the button callback
*/
		if (toolbars[i].callback)
			XtAddCallback(toolbarBut[i],XmNactivateCallback,
				toolbars[i].callback,toolbars[i].data);
/*
.....load toolbar image
*/

		if (image_data!=0)
		{
			ncq_create_ximage(toolbarBut[i], image_data,
							&(toolbars[i].image), i);
			toolbar_image[i] = toolbars[i].image;
			XtAddCallback(toolbarBut[i],XmNexposeCallback, toolbarCB, 
				(XtPointer)toolbars[i].image);
			XtAddCallback(toolbarBut[i],XmNarmCallback, ncq_toolbar_armCB, 
				(XtPointer)toolbars[i].image);
			XtAddCallback(toolbarBut[i],XmNdisarmCallback, ncq_toolbar_disarmCB, 
				(XtPointer)toolbars[i].image);
/*
			ncq_create_ximage(toolbarBut[i], image_data, 
							&(toolbars[i].image), i);
			XtAddCallback(toolbarBut[i],XmNexposeCallback, toolbarCB, 
				(XtPointer)&(toolbars[i].image));
			XtAddCallback(toolbarBut[i],XmNarmCallback, ncq_toolbar_armCB, 
				(XtPointer)&(toolbars[i].image));
			XtAddCallback(toolbarBut[i],XmNdisarmCallback, ncq_toolbar_disarmCB, 
				(XtPointer)&(toolbars[i].image));
*/
			XtAddEventHandler(toolbarBut[i], EnterWindowMask, False,
									(XtEventHandler)ncq_toolbar_enterCB,
									NULL);
			XtAddEventHandler(toolbarBut[i], LeaveWindowMask, False,
									(XtEventHandler)ncq_toolbar_leaveCB,
									NULL);
			XtAddCallback(toolbarBut[i], XmNdestroyCallback, ncq_toolbar_destroyCB,
									(XtPointer)&(toolbars[i].image));
		}
		if (i == 0)
		{
			XtVaGetValues(toolbar_area, XmNmarginHeight, &h, NULL);
			XtVaGetValues(toolbarBut[i], XmNheight, &height, NULL);
			height += 2 * h;
			XtVaSetValues(toolbar_area,
				XmNpaneMaximum, height,
				XmNpaneMinimum, height,
				NULL);
		}
	}
	XtVaSetValues(toolbar_area, XmNshowAsDefault, 0, NULL);
/*
.....Manage the Toolbar Area
*/
	XtManageChild(toolbar_area);
	return(toolbar_area);
}

/**********************************************************************
**    I_FUNCTION :  ncq_mfcreate_action(parent,actions,num_actions)
**       Creates the action area for forms
**    PARAMETERS   
**       INPUT  : 
**          parent      = Form widget to create Action Area for.
**				actions     = List of Action Area buttons and procedures.
**				num_actions = Number of 'actions' for this area.
**       OUTPUT :  
**          output
**    RETURNS      : Action Area Widget.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Widget ncq_mfcreate_action(parent,actions,num_actions)
Widget parent;
ActionAreaItem *actions;
int num_actions;
{
	Widget actionBut[10];
#define TIGHTNESS 40
	Widget action_area,widget;
	int i;
	Dimension h,height;
/*
.....Create Action Area Form
*/
	action_area = XtVaCreateWidget("action_area",
		xmFormWidgetClass,parent,
		XmNfractionBase,num_actions*TIGHTNESS-1,
		XmNleftOffset,10,
		XmNrightOffset,10,
		NULL);
/*
.....Create each of the buttons
*/
	for (i=0; i<num_actions; i++)
	{
		actionBut[i] = XtVaCreateManagedWidget(actions[i].label,
			xmPushButtonWidgetClass, action_area,
			XmNleftAttachment, i? XmATTACH_POSITION : XmATTACH_FORM,
			XmNleftPosition, TIGHTNESS*i,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNrightAttachment, i!=num_actions ? XmATTACH_POSITION : XmATTACH_FORM,
			XmNrightPosition, TIGHTNESS * i + (TIGHTNESS - 1),
			XmNshowAsDefault, i==0,
			XmNdefaultButtonShadowThickness, 1,
			NULL);
/*
.....Add the button callback
*/
		if (actions[i].callback)
			XtAddCallback(actionBut[i],XmNactivateCallback,
				actions[i].callback,actions[i].data);
/*
.....Set the default button and disable
.....resize of this pane
*/
		if (i == 0)
		{
			XtVaGetValues(action_area, XmNmarginHeight, &h, NULL);
			XtVaGetValues(actionBut[i], XmNheight, &height, NULL);
			height += 2 * h;
			XtVaSetValues(action_area,
				XmNdefaultButton, actionBut[i],
				XmNpaneMaximum, height,
				XmNpaneMinimum, height,
				NULL);
		}
	}
/*
.....Manage the Action Area
*/
	XtManageChild(action_area);
	return(action_area);
}

/*******************************************************************
**   E_FUNCTION : ncq_options()
**              Handle "options" funtion for NCQ
**   PARAMETERS
**       INPUT  : None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void ncq_options()
{
	XEvent x_event;
	Arg args[20];
	Widget label1, label2, pane, winid, action_area, ncq_mfcreate_action();
	int n, i, rows,cols, ifl, nc, x, y;
	char tmp[40];

	if (option_dialog!=NULL)
		return;
/*
......create form
*/
	n = 0;
	ifl = MWM_DECOR_ALL | MWM_DECOR_MAXIMIZE | MWM_DECOR_MENU |
					MWM_DECOR_MINIMIZE;
	XtSetArg(args[n],XmNmwmDecorations,ifl); n++;
	ifl = MWM_FUNC_ALL | MWM_FUNC_MINIMIZE | MWM_FUNC_MAXIMIZE |
					MWM_FUNC_CLOSE;
	XtSetArg(args[n],XmNmwmFunctions,ifl); n++;

	option_dialog = XtCreatePopupShell("NCQ option", xmDialogShellWidgetClass,
							NCQ_Parent, args,n);
	XtVaSetValues(option_dialog, XmNtitle, "NCQ options", NULL);
/*
.....Create a paned window to hold the form &
.....Action Area
*/
	pane = XtVaCreateWidget("pane",xmPanedWindowWidgetClass, option_dialog,
				XmNsashWidth,1,
				XmNsashHeight,1,
				NULL);
/*
.....Create a Form widget
*/
	n = 0;
	rows = 5; cols = 50;
	XtSetArg(args[n],XmNfractionBase,rows*cols); n++;
	winid = XtCreateWidget("form", xmFormWidgetClass,pane,args,n);
	if (winid == NULL) return;
/*
.....first row
*/
/*
......Toggle field for CL files
*/
   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
   opt_chk[0] = XtCreateManagedWidget("Create Clfile", 
								xmToggleButtonWidgetClass, winid,
                        args,n);

   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 0*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 1*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 26*rows); n++;

   opt_chk[1] = XtCreateManagedWidget("Create APT Source", 
								xmToggleButtonWidgetClass, winid,
                        args,n);

   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
   opt_chk[2] = XtCreateManagedWidget("Create Print File",
								xmToggleButtonWidgetClass, winid,
                        args,n);

   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 1*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 2*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 26*rows); n++;

   opt_chk[3] = XtCreateManagedWidget("Short Print file", 
								xmToggleButtonWidgetClass, winid,
                        args,n);

   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
   opt_chk[4] = XtCreateManagedWidget("Post Process",
								xmToggleButtonWidgetClass, winid,
                        args,n);

   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 2*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 3*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 26*rows); n++;

   opt_chk[5] = XtCreateManagedWidget("Update file", 
								xmToggleButtonWidgetClass, winid,
                        args,n);

   n = 0;
   XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNtopPosition, 3*cols); n++;
   XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNbottomPosition, 4*cols); n++;
   XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
   XtSetArg(args[n],XmNleftPosition, 1*rows); n++;

   opt_chk[6] = XtCreateManagedWidget("Monitor Process", 
								xmToggleButtonWidgetClass, winid,
                        args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 4*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 5*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 1*rows); n++;
	label1 = XtCreateManagedWidget("Page Length:", xmLabelWidgetClass, winid, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 4*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,5*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label1); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 4); n++;
	opt_edt[0] = XtCreateManagedWidget("Opt_Text1", xmTextWidgetClass, winid,
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 4*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 5*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 26*rows); n++;
	label2 = XtCreateManagedWidget("Priority:", xmLabelWidgetClass, winid, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 4*cols); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,5*cols); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label2); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNcolumns, 4); n++;
	opt_edt[1] = XtCreateManagedWidget("Opt_Text2", xmTextWidgetClass, winid,
														args,n);
	if (ncq_linbuf[0] == '1')
		XmToggleButtonSetState(opt_chk[0], True, False);
	else
		XmToggleButtonSetState(opt_chk[0], False, False);

	if (ncq_linbuf[1] == '1')
		XmToggleButtonSetState(opt_chk[1], True, False);
	else
		XmToggleButtonSetState(opt_chk[1], False, False);

	if (ncq_linbuf[4] == '1')
		XmToggleButtonSetState(opt_chk[2], True, False);
	else
		XmToggleButtonSetState(opt_chk[2], False, False);

	if (ncq_linbuf[5] == '1')
		XmToggleButtonSetState(opt_chk[4], True, False);
	else
		XmToggleButtonSetState(opt_chk[4], False, False);

	if (ncq_linbuf[2] == '1')
		XmToggleButtonSetState(opt_chk[3], False, False);
	else
		XmToggleButtonSetState(opt_chk[3], True, False);

	if (ncq_linbuf[3] == '1')
		XmToggleButtonSetState(opt_chk[5], True, False);
	else
		XmToggleButtonSetState(opt_chk[5], False, False);

	if (NCQ_monitor)
		XmToggleButtonSetState(opt_chk[6], True, False);
	else
		XmToggleButtonSetState(opt_chk[6], False, False);

	sprintf(tmp, "%d", ncq_ipglen);
	XmTextSetString(opt_edt[0], tmp);

	tmp[0] = ncq_lpri;
	tmp[1] = '\0';
	XmTextSetString(opt_edt[1], tmp);
/*
......create the Action Area
*/
	n = XtNumber(actionList);
	for (i=0;i<n;i++) actionList[i].data = (XtPointer)option_dialog;
	action_area = ncq_mfcreate_action(pane,actionList,n);
/*
.....Manage the Form
*/
	XtManageChild(winid);
	XtManageChild(pane);
	XtManageChild(option_dialog);
	formEof = False;
	while (!formEof)
	{
		XtAppNextEvent(NCQ_App, &x_event);
		XtDispatchEvent(&x_event);
	}
}

/**************************************************************
**   E_FUNCTION : appexitCCB(widget, client_data, call_data)
**              Callback function for "close" NCQ
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = not used
**          call_data   = Motif callback structure.
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void appexitCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	appexit();
}
/**********************************************************************
**    I_FUNCTION :  ncq_file_openCB(widget, client_data, call_data
**       Processes the "open" toolbar button
**       
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**                        selected filename.
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_file_openCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	ncq_fileopen();
}

/**********************************************************************
**    I_FUNCTION :  ncq_file_deleteCB(widget, client_data, call_data
**       Processes the "delete" toolbar button
**       
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**                        
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_file_deleteCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	ncq_filedelete();
}

/**********************************************************************
**    I_FUNCTION :  ncq_file_loadqueueCB(widget, client_data, call_data
**       Processes the "Load Queue" toolbar button
**       
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_file_loadqueueCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	ncq_mfloadque();
}

/**********************************************************************
**    I_FUNCTION :  ncq_file_savequeueCB(widget, client_data, call_data
**       Processes the "Save Queue" toolbar button
**       
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_file_savequeueCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	ncq_mfsaveque();
}

/**********************************************************************
**    I_FUNCTION :  ncq_view_monitorCB(widget, client_data, call_data
**       Processes the "Monitor" toolbar button
**       
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_view_monitorCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	ncq_view_monitor();
}

/**********************************************************************
**    I_FUNCTION :  ncq_view_clearCB(widget, client_data, call_data
**       Processes the "Clear Status" toolbar button
**       
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_view_clearCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	ncq_clear_status();
}

/**********************************************************************
**    I_FUNCTION :  ncq_updateCB(widget, client_data, call_data
**       Processes the "update" toolbar button
**       
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**                        
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_updateCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	ncq_update();
}

/**********************************************************************
**    I_FUNCTION :  ncq_optionCB(widget, client_data, call_data
**       Processes the "option" toolbar button
**       
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**                        
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_optionCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	ncq_options();
}
/**********************************************************************
**    I_FUNCTION :  ncq_file_runCB(widget, client_data, call_data)
**      Processes the "run" toolbar button
**      
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = 
**                       
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncq_file_runCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	ncq_run_ncl();
}

static unsigned int RoundRow (width)
unsigned int width;
{
  unsigned int result = (width + 3)
                      & ~3 ;
  return result ;
}

/**********************************************************************
**    I_FUNCTION :  ncq_setimage_size(cc, bits, ww, hh, image_data)
**      Set a iamge size
**      
**    PARAMETERS
**       INPUT  :
**         cc, 
**				bits: 
**          ww             
**          hh
**       OUTPUT :
**          image_data
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncq_setimage_size(cc, bits, ww, hh, image_data)
unsigned int cc, bits, ww, hh;
char **image_data;
{
	unsigned int bitsize, bytecount, row_width;
	switch (bits)
	{
		case 1:
		case 2:
		case 4:
		case 8:
		{
			bitsize = bits * ww ;
			row_width = RoundRow ((bitsize + 7)/8) ;
			bytecount = row_width * hh ;
			*image_data = (char *)malloc(bytecount*sizeof(char));
			break ;
		}
		case 24:
		{
			row_width = RoundRow (3 * ww) ;
			*image_data = (char *)malloc(row_width * hh*sizeof(char));
			break ;
		}
		default:
			printf("Invalid bit count\n");
			return -1;
	}
	return 0;
}

unsigned short SystemAdjustValue1 (value)
unsigned short value;
{
   return (unsigned short) ((value&0xFF) << 8)|((value&0xFF00)>>8) ;
}

unsigned int SystemAdjustValue2 (value)
unsigned int value;
{
   return ((value&0xFF000000L)>>24)|((value&0xFF0000L)>>8)
				| ((value&0xFF00L)<<8) | ((value&0xFFL)<<24) ;
}

/**********************************************************************
**    I_FUNCTION :  ncq_getimage(pix_file, image)
**      load a bitmap file (Windows) into a image data string
**      
**    PARAMETERS
**       INPUT  :
**          pix_file: bitmap file
**          
**       OUTPUT :
**          image: a image data string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncq_getimage(pix_file, image)
char *pix_file;
char **image;
{
	Dimension ht, st;
	char *image_data;

	unsigned long headersize ;
	FILE *fptr;
	BITMAPFILEHEADER fileheader ;
	char data, hi, lo ;
	unsigned long width, ii, jj, xx, yy, col, row ;
	long height ;
	unsigned int bitcount, index ;
	unsigned int compression ;
	BITMAPCOREHEADER bheader ;
   BITMAPINFOHEADER infoheader ;
	unsigned int colorcount ;
   RGBTRIPLE rgbcolor ;
	RGBQUAD qcolor ;
	RGBTRIPLE *colormap;
	char dx, dy;
	unsigned int bitwidth;
	unsigned int rowwidth;
	unsigned int physicalrowsize;
	unsigned int padsize;
	static char pad [4] ;
	int done, os2format;
	char *dir, filename[MAX_PATH];
	struct
	{
		char count ;
		char command ;
	} opcode ;
	unsigned int bytesread;
/*	const unsigned short signature = 'B' | ('M' << 8) ;*/
	static unsigned short signature = 'B' | ('M' << 8) ;

	bytesread = 0;
	colormap = 0;

/*
......looking for bitmap file in 'NCL_BITMAP' directory
*/
	dir = (char *)getenv ("NCL_BITMAP");
	if (dir!=NULL)
	{
		strcpy(filename, dir);
		strcat(filename, "/");
		strcat(filename, pix_file);
	}	
	else
		strcpy(filename, pix_file);
	fptr = fopen(filename,"rb");
	if (fptr == 0)
	{
		printf("Can't find bitmap file %s\n", pix_file);
		return -1;
	}

	fread((char *) &(fileheader.bfType), 1, 2, fptr) ;
	fileheader.bfType = SystemAdjustValue1(fileheader.bfType);

	fread((char *) &(fileheader.bfSize), 1, 4, fptr) ;
	fileheader.bfSize = SystemAdjustValue2(fileheader.bfSize);
	
	fread((char *) &(fileheader.bfReserved1), 1, 2, fptr) ;
	fileheader.bfReserved1 = SystemAdjustValue1(fileheader.bfReserved1);

	fread((char *) &(fileheader.bfReserved2), 1, 2, fptr) ;
	fileheader.bfReserved2 = SystemAdjustValue1(fileheader.bfReserved2);

	fread((char *) &(fileheader.bfOffBits), 1, 4, fptr) ;
	fileheader.bfOffBits = SystemAdjustValue2(fileheader.bfOffBits);

	bytesread += 14;
	if (fileheader.bfType != signature)
	{
		printf("Not a bitmap file\n");
		return -1;
	}

	fread ((char *) &headersize, 1, 4, fptr) ;
	headersize = SystemAdjustValue2(headersize);

	if (headersize == 12)
	{
		bheader.bcSize = headersize ;
		fread ((char *) &bheader.bcWidth, 1, 2, fptr) ;
		bheader.bcWidth = SystemAdjustValue1(bheader.bcWidth);

		fread ((char *) &bheader.bcHeight, 1, 2, fptr) ;
		bheader.bcHeight = SystemAdjustValue1(bheader.bcHeight);

		fread ((char *) &bheader.bcPlanes, 1, 2, fptr) ;
		bheader.bcPlanes = SystemAdjustValue1(bheader.bcPlanes);

		fread ((char *) &bheader.bcBitCount, 1, 2, fptr) ;
		bheader.bcBitCount = SystemAdjustValue1(bheader.bcBitCount);

		bytesread += 12;
		width = bheader.bcWidth ;
		height = bheader.bcHeight ;
		bitcount = bheader.bcBitCount ;
		compression = BI_RGB ;
		os2format = 1 ;
	}
	else if (headersize >= sizeof (BITMAPINFOHEADER))
	{
		infoheader.biSize = headersize ;

		fread ((char *) &infoheader.biWidth, 1, 4, fptr) ;
		infoheader.biWidth = SystemAdjustValue2((unsigned int)infoheader.biWidth);

		fread ((char *) &infoheader.biHeight, 1, 4, fptr) ;
		infoheader.biHeight = SystemAdjustValue2((unsigned int)infoheader.biHeight);

		fread ((char *) &infoheader.biPlanes, 1, 2, fptr) ;
		infoheader.biPlanes = SystemAdjustValue1(infoheader.biPlanes);

		fread ((char *) &infoheader.biBitCount, 1, 2, fptr) ;
		infoheader.biBitCount = SystemAdjustValue1(infoheader.biBitCount);

		fread ((char *) &infoheader.biCompression, 1, 4, fptr) ;
		infoheader.biCompression = SystemAdjustValue2(infoheader.biCompression);

		fread ((char *) &infoheader.biSizeImage, 1, 4, fptr) ;
		infoheader.biSizeImage = SystemAdjustValue2(infoheader.biSizeImage);

		fread ((char *) &infoheader.biXPelsPerMeter, 1, 4, fptr) ;
		infoheader.biXPelsPerMeter = 
				SystemAdjustValue2((unsigned int)infoheader.biXPelsPerMeter);

		fread ((char *) &infoheader.biYPelsPerMeter, 1, 4, fptr) ;
		infoheader.biYPelsPerMeter = 
				SystemAdjustValue2((unsigned int)infoheader.biYPelsPerMeter);

		fread ((char *) &infoheader.biClrUsed, 1, 4, fptr) ;
		infoheader.biClrUsed = SystemAdjustValue2(infoheader.biClrUsed);

		fread ((char *) &infoheader.biClrImportant, 1, 4, fptr) ;
		infoheader.biClrImportant = SystemAdjustValue2(infoheader.biClrImportant);

		bytesread += 40;
		compression = infoheader.biCompression ;

		width = infoheader.biWidth ;
		height = infoheader.biHeight ;
		bitcount = infoheader.biBitCount ;

		for (ii = 0 ;
					ii < headersize - sizeof (BITMAPINFOHEADER) ;
					++ ii)
		{
			++ bytesread ;
			fread ((char *) &data, 1, 1, fptr) ;
		}
		os2format = 0 ;
	}
	else
	{
		printf("Bad Bitmap file\n");
	}

	switch (bitcount)
	{
		case 1:
		if (compression != BI_RGB)
      {
			printf("This bitmap type not supported\n");
			return -1;
		}
		colorcount = 1 << bitcount ;
		break ;
		case 4:
		if (compression != BI_RGB && compression != BI_RLE4)
      {
			printf("This bitmap type not supported\n");
			return -1;
		}
		colorcount = 1 << bitcount ;
		break ;
		case 8:
		if (compression != BI_RGB &&  compression != BI_RLE8)
      {
			printf("This bitmap type not supported\n");
			return -1;
		}
		colorcount = 1 << bitcount ;
		break ;
		case 24:
		if (compression != BI_RGB)
      {
			printf("This bitmap type not supported\n");
			return -1;
		}
		colorcount = 0 ;
		break ;
		default:
			printf("This bitmap type not supported\n");
			return -1;
	}

	if (colorcount)
		colormap = (RGBTRIPLE *)malloc(colorcount*sizeof(RGBTRIPLE));
	if (os2format)
	{
		for (ii = 0 ; ii < colorcount ; ++ ii)
		{
			fread ((char *) &rgbcolor, 1, sizeof (rgbcolor), fptr) ;
			colormap[ii].rgbtRed = rgbcolor.rgbtRed ;
			colormap[ii].rgbtBlue = rgbcolor.rgbtBlue ;
			colormap[ii].rgbtGreen = rgbcolor.rgbtGreen ;
			bytesread += sizeof (rgbcolor) ;
		}
	}
	else
	{
		for (ii = 0 ; ii < colorcount ; ++ ii)
		{
			fread ((char *) &qcolor, 1, sizeof (qcolor), fptr) ;
			colormap[ii].rgbtRed = qcolor.rgbRed ;
			colormap[ii].rgbtBlue = qcolor.rgbBlue ;
			colormap[ii].rgbtGreen = qcolor.rgbGreen ;
			bytesread += sizeof (qcolor) ;
		}
	}
	if (bytesread > fileheader.bfOffBits)
		printf("Bad Bitmap file\n");
	for (ii = bytesread ; ii < fileheader.bfOffBits ; ++ ii)
	{
		fread ((char *) &data, 1, 1, fptr) ;
	}
	ncq_setimage_size(colorcount, bitcount, width, height, &image_data);
	if (bitcount != 24)
	{
		if (compression == BI_RGB)
		{
			bitwidth = bitcount * width ;
			rowwidth = (bitwidth + 7)/8 ;
			physicalrowsize = (rowwidth + 0x3) & ~0x3 ;
			padsize = physicalrowsize -  rowwidth ;

			if (height > 0)
			{
				for (ii = 0 ; ii < height ; ++ ii)
				{
					fread ((char *)&(image_data [ii]), rowwidth, 1, fptr) ;
					fread (pad, padsize, 1, fptr) ;
				}
			}
			else
			{
				for (ii = 0 ; ii < - height ; ++ ii)
				{
					fread ((char *)&image_data [-height-ii-1], rowwidth, 1, fptr) ;
					fread (pad, 1, padsize, fptr) ;
				}
			}
		}
		else if (compression == BI_RLE8)
		{
			row = height - 1 ;
			col = 0 ;       
			done = 0;
			while (! feof(fptr) && ! done)
			{
				fread ((char *) &opcode, 1, sizeof (opcode), fptr) ;
				if (opcode.count == 0)
				{
					switch (opcode.command)
					{
						case 0: /* 0 => Move to next row */
							-- row ;
							col = 0 ;
							break ;
						case 1: /* 1 => Image is finished */
							done = 1;
							break ;
						case 2: /* 2 => Move to a new relative position. */
						{
							fread ((char *) &dx, 1, 1, fptr) ;
							fread ((char *) &dy, 1, 1, fptr) ;
							col += dx ;
							row -= dy ;
						}
						break ;
						default:
						{
							if (row >= height || col + opcode.command > width)
								printf("Bad Bitmap file\n");
							for (ii = 0 ; ii < opcode.command ; ++ ii)
							{
								fread ((char *) &data, 1,1, fptr) ;
								image_data [row*rowwidth+col] = data ;
								++ col ;
							}
							if ((opcode.command & 1) != 0)
								fread ((char *) &data, 1,1, fptr) ;
						}
						break ;
					}
				}
				else
				{
					if (row >= height || col + opcode.count > width)
					for (ii = 0 ; ii < opcode.count ; ++ ii)
					{
						image_data [row*rowwidth+col] = opcode.command ;
						++ col ;
					}
				}
			}
			if (! done)
				printf("Bad Bitmap file\n");
		}
		else if (compression == BI_RLE4)
		{
			row = height - 1 ;
			col = 0 ;
			done = 0;
			while (! feof(fptr) && ! done)
			{
				fread ((char *) &opcode, 1, sizeof (opcode), fptr) ;
				if (opcode.count == 0)
				{
					switch (opcode.command)
					{
						case 0: /* 0 => Move to next row */
							-- row ;
							col = 0 ;
							break ;
						case 1: /* 1 => Image is finished */
							done = 1;
							break ;
						case 2: /* 2 => Move to a new relative position. */
						{
							fread ((char *) &dx, 1, 1, fptr) ;
							fread ((char *) &dy, 1, 1, fptr) ;
							col += dx ;
							row -= dy ;
						}
						break ;
						default:
						{
							if (row >= height || col + opcode.command > width)
								printf("Bad Bitmap file\n");
							for (ii = 0 ; ii < opcode.command ; ++ ii)
							{
								if ((ii & 1) == 0)
								{
									fread ((char *) &data, 1,1, fptr) ;
									lo = data & 0xF ;
									hi = (data & 0xF0) >> 4 ;
								}
								if ((col & 1) == 0)
								{
									if ((ii & 1) == 0)
									{
										image_data[row*rowwidth+col/2] = hi << 4 ;
									}
									else
									{
										image_data [row*rowwidth+col/2] = lo << 4 ;
									}
								}
								else
								{
									if ((ii & 1) == 0)
										image_data [row*rowwidth+col/2] |= hi  ;
									else
										image_data [row*rowwidth+col/2] |= lo ;
								}
								col++;
							}
							switch (opcode.command & 0x3)
							{
								case 1: case 2:
								fread ((char *) &data, 1,1, fptr) ;
								break ;
							}
						}
						break ;
					}
				}
				else
				{
					hi = opcode.command >> 4 ;
					lo = opcode.command & 0xF ;
					if (row >= height || col + opcode.count > width)
							printf("Bad Bitmap file\n");

					for (ii = 0 ; ii < opcode.count ; ++ ii)
					{
						if ((col & 1) == 0)
						{
							if ((ii & 1) == 0)
							{
								image_data [row*rowwidth+col/2] = hi << 4 ;
							}
							else
							{
								image_data [row*rowwidth+col/2] = lo << 4 ;
							}
						}
						else
						{
							if ((ii & 1) == 0)
							{
								image_data [row*rowwidth+col/2] |= hi  ;
							}
							else
							{
								image_data [row*rowwidth+col/2] |= lo ;
							}
						}
						++ col ;
					}
				}
			}
			if (! done)
				printf("Bad Bitmap file\n");
		}
		else
		{
			printf("Bad Bitmap file\n");
		}
	}
	else
	{
		physicalrowsize = (3 * width + 0x3) & ~0x3 ;
		padsize = physicalrowsize -  3 * width ;

		if (height > 0)
		{
			for (yy = 0 ; yy < height ; ++ yy)
			{
/*
				index = height - yy - 1 ;
*/
				index = yy;
				for (xx = 0 ; xx < 3 * width ; xx += 3)
				{
					fread ((char *)&image_data[index*3*width+xx+2], 1, 1, fptr) ;
					fread ((char *)&image_data[index*3*width+xx + 1], 1, 1, fptr) ;
					fread ((char *)&image_data[index*3*width+xx], 1, 1, fptr) ;
				}
				if (padsize!=0)
					fread (pad, padsize, 1, fptr) ;
			}
		}
		else
		{
			for (yy = 0 ; yy < -height ; ++ yy)
			{
				for (xx = 0 ; xx < 3 * width ; xx += 3)
				{
					fread ((char *)&image_data[ii*3*width+xx+2], 1, 1, fptr) ;
					fread ((char *)&image_data[ii*3*width+xx+1], 1, 1, fptr) ;
					fread ((char *)&image_data[ii*3*width+xx+0], 1, 1, fptr) ;
				}
				if (padsize!=0)
					fread (pad, padsize, 1, fptr) ;
			}
		}
	}
	if (colormap!=0)
		free(colormap);
	fclose(fptr);
	*image = image_data;
}


/**********************************************************************
**    I_FUNCTION :  ncq_create_ximage(widget, image_data, output_image, indx)
**      Create a XImage from a image data string
**      
**    PARAMETERS
**       INPUT  :
**          widget: Ximage base widget
**				image_data: a image data string
**          indx: indx of image data
**       OUTPUT :
**          output_image: XImage to be created 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
***********************************************************************/
ncq_create_ximage(widget, image_data, output_image, indx)
char *image_data;
Widget widget;
XImage **output_image;
int indx;
{
	Colormap cmap;
	XColor current_color;
	Pixel bg;
	int i, j;
	XVisualInfo vinfo;
	Display *disp;
	XImage *image;
/*
	image->width = 24;
	image->height = 24;
	image->xoffset = 0;
	image->data = ncq_image_data[indx];
	image->format = ZPixmap;
	image->byte_order = LSBFirst;
	image->bitmap_unit = 8;
	image->bitmap_bit_order = LSBFirst;
	image->bitmap_pad = 8;
	image->depth = 8;
	image->bytes_per_line = 0;
	image->bits_per_pixel = 8;
	XInitImage(image);
*/

	XtVaGetValues(widget, XmNcolormap, &cmap, NULL);
	disp = XtDisplay(widget);
	image = XCreateImage(disp, DefaultVisual(disp, DefaultScreen(disp)), 
						8, ZPixmap, 0, 
						ncq_image_data[indx], 24, 24, 8, 0);
	for (i=0; i<24; i++)
	{
		for (j=0; j<24*3; j += 3)
		{
			current_color.red = 255*image_data[i*3*24+j];
			current_color.green = 255*image_data[i*3*24+j+1];
			current_color.blue = 255*image_data[i*3*24+j+2];
/*
			if ((image_data[i*3*24+j]==192)&&(image_data[i*3*24+j+1]==192)
					&&(image_data[i*3*24+j]==192))
*/
			if (((image_data[i*3*24+j]>190)&&(image_data[i*3*24+j]<200))
					&&((image_data[i*3*24+j+1]>190)&&(image_data[i*3*24+j+1]<200))
					&&((image_data[i*3*24+j+2]>190)&&(image_data[i*3*24+j+2]<200)))
			{
				XtVaGetValues(widget, XmNbackground, &bg, NULL);
				XPutPixel(image, j/3, 24-i-1, bg);
			}
			else
			{
				XAllocColor(XtDisplay(widget), cmap, &current_color);
				XPutPixel(image, j/3, 24-i-1, current_color.pixel);
			}
		}
	}
	*output_image = image;
	if (image_data!=NULL)
		free (image_data);
}

/**********************************************************************
**    I_FUNCTION :  toolbarCB(widget, client_data, call_data)
**      Callback for toolbar expose event. It actually draw bitmap button
**      
**    PARAMETERS
**       INPUT  :
**          widget      = 
**          client_data = bitmap pixel of button
**                       
**          call_data   = Motif callback structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void toolbarCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	Dimension ht, st;
	char *pix_file;
	XImage *image;
	XmDrawnButtonCallbackStruct *cbs = (XmDrawnButtonCallbackStruct*) call_data;
	image = (XImage* )client_data;
	if (image==NULL) return;
	if (cbs->reason==XmCR_EXPOSE)
	{
		XtVaGetValues(widget, XmNhighlightThickness, &ht,
						XmNshadowThickness, &st, NULL);
		XtVaSetValues(widget, XmNwidth, 2*ht +  2*st + 24 + 4,
								XmNheight, 2*ht +  2*st + 24 + 4,
								NULL);
		XPutImage(XtDisplay(widget), XtWindow(widget),
					(GC)(XDefaultGCOfScreen(XtScreen(widget))), 
					image, 0, 0,
					ht + st + 2, ht+st + 2, 24, 24);
		ncq_toolbar_leaveCB(widget, NULL, NULL);
	}
}
/*******************************************************************
**   E_FUNCTION : filemenuCB(widget, client_data, call_data)
**              Callback function "File" menu function
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = menu item number
**          call_data   = not used
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void filemenuCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	int indx = (int )client_data;
	switch (indx)
	{
		case 0:
			ncq_fileopen();
			break;
		case 1:
			ncq_filedelete();
			break;
		case 2:
			ncq_mfloadque();
			break;
		case 3:
			ncq_mfsaveque();
			break;
		case 4:
			ncq_run_ncl();
			break;
		case 5:
			appexit();
			break;
	}		
}
/*******************************************************************
**   E_FUNCTION : ratemenuCB(widget, client_data, call_data)
**              Callback function "Update Rate" menu function
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = menu item number
**          call_data   = not used
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void ratemenuCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	int indx;
	static Widget picked_menu = NULL;
	indx = (int )client_data;
	switch (indx)
	{
		case 0:
			ncq_poll_time = 1000;
			break;
		case 1:
			ncq_poll_time = 2000;
			break;
		case 2:
			ncq_poll_time = 3000;
			break;
		case 3:
			ncq_poll_time = 5000;
			break;
		case 4:
			ncq_poll_time = 10000;
			break;
	}
	XmToggleButtonSetState(widget, 1, False);
	if (picked_menu!=NULL)
		XmToggleButtonSetState(picked_menu, 0, False);
	picked_menu = widget;
	if (timer1!=-1)
	{
		XtRemoveTimeOut(timer1);
		timer1 = XtAppAddTimeOut(NCQ_App, ncq_poll_time,
				(XtTimerCallbackProc)timeoutCB, NULL);
	}
}

/*******************************************************************
**   E_FUNCTION : viewmenuCB(widget, client_data, call_data)
**              Callback function "View" menu function
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = menu item number
**          call_data   = not used
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void viewmenuCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	int indx = (int )client_data;
	switch (indx)
	{
		case 0:
			ncq_update();
			break;
		case 1:
			ncq_view_monitor();
			break;
		case 3:
			ncq_clear_status();
			break;
	}
}

/*******************************************************************
**   E_FUNCTION : priomenuCB(widget, client_data, call_data)
**              Callback function "Option->proirity" menu function
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = menu item number
**          call_data   = not used
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void priomenuCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	int indx;
	static Widget picked_menu = NULL;
	indx = (int )client_data;
	switch (indx)
	{
		case 0:
			m_priority = -20;
			ncq_modncl_priority(-20);
			break;
		case 1:
			m_priority = 0;
			ncq_modncl_priority(0);
			break;
	}
	XmToggleButtonSetState(widget, 1, False);
	if (picked_menu!=NULL)
		XmToggleButtonSetState(picked_menu, 0, False);
	picked_menu = widget;
}
/*******************************************************************
**   E_FUNCTION : timemenuCB(widget, client_data, call_data)
**              Callback function "Option->Limit Time" menu function
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = menu item number
**          call_data   = not used
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void timemenuCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	int indx;
	static Widget picked_menu = NULL;
	indx = (int )client_data;
	switch (indx)
	{
		case 0:
			m_runtime = 15*60*1000;
			NCQ_time_limit = 1;
			break;
		case 1:
			m_runtime = 30*60*1000;
			NCQ_time_limit = 2;
			break;
		case 2:
			m_runtime = 60*60*1000;
			NCQ_time_limit = 3;
			break;
		case 3:
			m_runtime = 120*60*1000;
			NCQ_time_limit = 4;
			break;
		case 4:
			m_runtime = -1;
			NCQ_time_limit = 0;
			break;
	}
	XmToggleButtonSetState(widget, 1, False);
	if (picked_menu!=NULL)
		XmToggleButtonSetState(picked_menu, 0, False);
	picked_menu = widget;
}
/*******************************************************************
**   E_FUNCTION : optionmenuCB(widget, client_data, call_data)
**              Callback function "Option" menu function
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = menu item number
**          call_data   = not used
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void optionmenuCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	int indx = (int )client_data;
	switch (indx)
	{
		case 0:
			ncq_options();
			break;
	}
}
/*******************************************************************
**   E_FUNCTION : helpmenuCB(widget, client_data, call_data)
**              Callback function "Help" menu
**   PARAMETERS
**       INPUT  :
**          widget      = not used
**          client_data = menu item number
**          call_data   = not used
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void helpmenuCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data;
XtPointer call_data;
{
	char msg[500], msg1[256], msg2[256];
	ncq_getabout_str(msg1, msg2);
	sprintf(msg, "\n%s\n%s\n", msg1, msg2);
	ncq_unxdispmsg("About Ncq", msg);	
}

/**********************************************************************
**    I_FUNCTION : CreatePushbutton(parent, name, callback, client_data)
**			Create a push button	
**    PARAMETERS   
**       INPUT  : 
**          parent :      parent widget
**          name:         name of push button
**          callback:     callback function for push button
**				client_data = client_data needed for push botton call back.
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Widget CreatePushbutton(parent, name, callback, client_data)
Widget parent;
char *name;
XtCallbackProc callback;
XtPointer client_data;
{
	Widget push;
	Arg args[20];
	Cardinal n;
	n = 0;
	push = XmCreatePushButton(parent, name, args, n);
	XtAddCallback(push, XmNactivateCallback, callback, client_data);
	XtManageChild(push);
	return push;
}

/**********************************************************************
**    I_FUNCTION : CreatePulldown(parent, label, commandStruct, count)
**			Create a Pulldown menu
**    PARAMETERS   
**       INPUT  : 
**          parent :      parent widget
**          label:         name of Pulldown menu
**          commandStruct: command struct
**				count: menu item of Pulldown menu
**       OUTPUT :  
**         		none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void CreatePulldown(parent, label, commandStruct, count)
Widget parent;
char *label;
CmdInterfaceStruct *commandStruct;
int count;
{
	Widget pulldown, cascade, but;
	int i, n,item;
	XmString    xmstr;
	Arg         args[8];
	Widget temp_widget;
	
	pulldown = (Widget)XmCreatePulldownMenu(parent, label, NULL,0);
	cascade = XtVaCreateManagedWidget(label, 
					xmCascadeButtonWidgetClass,
					parent,
					XmNsubMenuId,pulldown,
					NULL);
	for (i = 0; i < count; i++)
	{
		if (commandStruct[i].ciStruct!=NULL)
		{
			CreatePulldown(pulldown, commandStruct[i].name, 
						commandStruct[i].ciStruct, commandStruct[i].subMenuCount);
		}
		else
		{
			n = 0;
			xmstr = NULL;
			if (commandStruct[i].accelerator!=NULL)
			{
				XtSetArg(args[n], XmNaccelerator, commandStruct[i].accelerator); 
				n++;
				if (commandStruct[i].accelText!= NULL)
				{
					xmstr = XmStringCreate(commandStruct[i].accelText,
												XmSTRING_DEFAULT_CHARSET);
					XtSetArg(args[n], XmNacceleratorText, xmstr); n++;
				}
			}
			if (commandStruct[i].btype == -1)
			{
				but = XtCreateManagedWidget("sep", xmSeparatorWidgetClass,
						pulldown, args, n);
			}
			else if (commandStruct[i].btype == 0)
			{
				but = XtCreateManagedWidget(commandStruct[i].name,
						xmPushButtonGadgetClass,		
						pulldown, args, n);
				XtAddCallback(but, XmNactivateCallback, commandStruct[i].callback, 
										commandStruct[i].client_data);
			}
			else if (commandStruct[i].btype == 1)
			{
				XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY); n++;
				but = XtCreateManagedWidget(commandStruct[i].name,
						xmToggleButtonGadgetClass,
						pulldown, args, n);
				XtAddCallback(but, XmNvalueChangedCallback, 
										commandStruct[i].callback, 
										commandStruct[i].client_data);
				temp_widget = XtNameToWidget(pulldown, PrioMenu[1].name);
				if (temp_widget)
					XmToggleButtonSetState(temp_widget, 1, True);

				if (NCQ_time_limit==0)
					item = 4;
				else if (NCQ_time_limit==1)
					item = 0;
				else if (NCQ_time_limit==2)
					item = 1;
				else if (NCQ_time_limit==3)
					item = 2;
				else if (NCQ_time_limit==4)
					item = 3;
				temp_widget = XtNameToWidget(pulldown, TimeMenu[item].name);
				if (temp_widget)
					XmToggleButtonSetState(temp_widget, 1, True);
			}
		}
	}
}


/*******************************************************************
**   E_FUNCTION : main()
**
**   PARAMETERS
**       INPUT  :
**          None
**       OUTPUT : None
**   RETURNS:    none.
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void main(argc, argv)
int argc;
char *argv[];
{
	XmString file, view, option, helpm;
	int num, istop;
	char comstr[MAX_PATH+40],cmsg[MAX_PATH+40];
	int i, j, n, rows, cols, rows1, cols1;
	int nc, ierr ;
	Arg args[20];
	XmString str1, str11, str2, str3, str4, str5, key1, key2, key3, key4, key5;
	Atom watom;
	Widget widget, but1, but2, toolbar_area;
	CmdInterfaceStruct commandStruct, commandStruct1;
	XColor current_color;
	Colormap cmap;
	XEvent x_event;
	int stat,flag;
	XFontStruct *fontstrct;
	XmFontList fontlist;
/*
.....we only use one ncq.que file, so we need save this file when first
.....time open with all path because if not we may open a different ncq.que
.....in another
.....directory because of file browser change
*/
	ncq_getwd (ncq_file);
	strcpy(ncq_localdir, ncq_file);
	strcat (ncq_file, "/ncl.que");
	ncq_init();
/*
.....malloc space for toolbar image
*/
	ncq_image_data[0] = (char *) malloc(24*72*sizeof (char*));
	ncq_image_data[1] = (char *) malloc(24*72*sizeof (char*));
	ncq_image_data[2] = (char *) malloc(24*72*sizeof (char*));
	ncq_image_data[3] = (char *) malloc(24*72*sizeof (char*));
	ncq_image_data[4] = (char *) malloc(24*72*sizeof (char*));
	ncq_image_data[5] = (char *) malloc(24*72*sizeof (char*));
	ncq_image_data[6] = (char *) malloc(24*72*sizeof (char*));
	ncq_image_data[7] = (char *) malloc(24*72*sizeof (char*));
	ncq_image_data[8] = (char *) malloc(24*72*sizeof (char*));

	comstr[0] = '\0';
	for (i=1; i<argc; i++)
	{
		strcat(comstr, argv[i]);
		if (i!=argc-1)
			strcat(comstr, " ");
	}

	if (argc>1)
	{
		UU_BATCH = 1;
		num = strlen(comstr);
		istop = ncqbatch (comstr, num, cmsg);
		if (istop == 1) 
		{
			if (cmsg[0]!='\0')
			{
				strcat(cmsg,"\n");
				ncq_unxdispmsg(NULL, cmsg);
			}
			return;
		}
	}		

	i = 0;
	option_dialog = NULL;
	n = 0;
	XtSetArg(args[n],XmNdeleteResponse, XmDO_NOTHING); n++;
	NCQ_Parent = XtAppInitialize (&NCQ_App, "NCQ",
				NULL,0,&i,NULL,NULL,args,n);

	fontstrct = XLoadQueryFont(XtDisplay(NCQ_Parent), 
						"-*-courier-bold-r-normal--20-*-*-*-*-*-iso8859-1");
	fontlist =  XmFontListCreate (fontstrct, XmSTRING_DEFAULT_CHARSET);
	XtVaSetValues(NCQ_Parent, XmNdefaultFontList, fontlist,
							XmNtextFontList, fontlist,
							XmNlabelFontList, fontlist,
							XmNbuttonFontList, fontlist,
							NULL);
	n=0;
	XtSetArg(args[n],XmNdeleteResponse, XmDO_NOTHING); n++;
	Main_Window = XtCreateManagedWidget("ncq_main", xmMainWindowWidgetClass,
						NCQ_Parent, args,n);
	XtVaSetValues(Main_Window, XmNdefaultFontList, fontlist,
							XmNtextFontList, fontlist,
							XmNlabelFontList, fontlist,
							XmNbuttonFontList, fontlist,
							NULL);
	XtVaSetValues(NCQ_Parent, XmNtitle, "NCL Batch Scheduler (Ncq)", NULL);
	watom = XmInternAtom(XtDisplay(NCQ_Parent), "WM_DELETE_WINDOW",False);
	XmAddWMProtocolCallback(NCQ_Parent, watom,appexitCB,
								NULL);

	menuBar = (Widget)XmCreateMenuBar(Main_Window, "menubar", NULL,0);
	CreatePulldown(menuBar, "File", FileMenu, 9);
	CreatePulldown(menuBar, "View", ViewMenu, 6);
	CreatePulldown(menuBar, "Options", OptionMenu, 3);
	CreatePulldown(menuBar, "Help", HelpMenu, 1);

	XtManageChild(menuBar);

	ncq_pane = XtVaCreateWidget("ncq_pane",xmPanedWindowWidgetClass, 
					Main_Window,
					XmNsashWidth,1,
					XmNsashHeight,1,
					NULL);
	n = XtNumber(toolbarlst);
	for (i=0;i<n;i++) toolbarlst[i].data = (XtPointer)ncq_pane;
	toolbar_area = ncq_mfcreate_toolbar(ncq_pane, toolbarlst, n);
	n = 0;
	ncq_form = XtCreateWidget("entry_form", xmFormWidgetClass,
						ncq_pane, args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 5); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 13); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2); n++;
	label = XtCreateManagedWidget("File:", xmLabelWidgetClass, ncq_form, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 5); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition,13); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 65); n++;
	file_fld = XtCreateManagedWidget("File_Text", xmTextWidgetClass, ncq_form,
														args,n);
	XtAddCallback(file_fld,XmNactivateCallback,
					file_text_returnCB, NULL);
/*
......status
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 5); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 13); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 68); n++;
	label2 = XtCreateManagedWidget("Status:", xmLabelWidgetClass, ncq_form, 
														args,n);
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 5); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 13); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(args[n],XmNleftWidget, label2); n++;
	XtSetArg(args[n],XmNeditMode, XmSINGLE_LINE_EDIT); n++;
	XtSetArg(args[n],XmNeditable, False); n++;
	XtSetArg(args[n],XmNcolumns, 8); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNrightPosition, 98); n++;
	current_color.red = 255*255;
	current_color.green = 255*255;
	current_color.blue = 0;
	XtVaGetValues(ncq_form, XmNcolormap, &cmap, NULL);
	XAllocColor(XtDisplay(ncq_form), cmap, &current_color);
	yl_color_px = current_color.pixel;
	current_color.red = 0;
	current_color.green = 255*255;
	current_color.blue = 0;
	XtVaGetValues(ncq_form, XmNcolormap, &cmap, NULL);
	XAllocColor(XtDisplay(ncq_form), cmap, &current_color);
	green_color_px = current_color.pixel;
	current_color.red = 255*255;
	current_color.green = 0;
	current_color.blue = 0;
	XtVaGetValues(ncq_form, XmNcolormap, &cmap, NULL);
	XAllocColor(XtDisplay(ncq_form), cmap, &current_color);
	red_color_px = current_color.pixel;
	current_color.red = 100*255;
	current_color.green = 100*255;
	current_color.blue = 100*255;
	XtVaGetValues(ncq_form, XmNcolormap, &cmap, NULL);
	XAllocColor(XtDisplay(ncq_form), cmap, &current_color);
	grey_color_px = current_color.pixel;
	stat_area = XtCreateManagedWidget("Status_Text", xmTextWidgetClass, 
													ncq_form, args,n);

	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 15); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 60); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;

	XtSetArg(args[n], XmNselectionPolicy, XmSINGLE_SELECT); n++;
	XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
	XtSetArg(args[n], XmNscrollHorizontal, TRUE);  n++;
	XtSetArg(args[n], XmNscrollVertical, TRUE);  n++;
	XtSetArg(args[n], XmNlistSizePolicy,  XmCONSTANT); n++;

	listbox = (Widget)XmCreateScrolledList(ncq_form, "list", args, n);
	XtVaSetValues(listbox, XmNdefaultFontList, fontlist,
							XmNtextFontList, fontlist,
							XmNlabelFontList, fontlist,
							XmNbuttonFontList, fontlist,
							NULL);
	XtAddCallback(listbox, XmNsingleSelectionCallback,
					(XtCallbackProc)selectchgCB, NULL);
	XtManageChild(listbox);
/*
......add status text window
*/
/*
....."status" label
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 62); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2); n++;
	label = XtCreateManagedWidget("Status:",
						xmLabelWidgetClass,ncq_form,args,n);
/*
.....text display window
*/
	n = 0;
	XtSetArg(args[n],XmNtopAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNtopPosition, 67); n++;
	XtSetArg(args[n],XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNbottomPosition, 95); n++;
	XtSetArg(args[n],XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n],XmNleftPosition, 2); n++;
	XtSetArg(args[n],XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNscrollVertical, True); n++;
	XtSetArg(args[n], XmNeditable, False); n++;
	XtSetArg(args[n],XmNeditMode, XmMULTI_LINE_EDIT); n++;
	ncq_stat_win = XmCreateScrolledText(ncq_form, "status_text" ,args,n);
	XmTextSetString(ncq_stat_win, "");
	XtManageChild(ncq_stat_win);
/*
.....Manage the Form
*/
	XtManageChild(ncq_form);
	XtManageChild(ncq_pane);
	XtVaSetValues(Main_Window, XmNworkWindow,ncq_pane, NULL);
	XtRealizeWidget(NCQ_Parent);

	XmTextSetString(stat_area, "Unknown");
	XtVaSetValues(stat_area, XmNforeground, yl_color_px, NULL);
	XtVaSetValues(stat_area, XmNbackground, grey_color_px, NULL);
	ncq_opnque (&ierr);
	if (ierr != 0) return;
	ncq_lodque();
	ncq_clsque();

	if (UU_BATCH == 0)
		ncq_selectlst(0);
/*
.....Set the new list field as active focus
*/
   XmProcessTraversal(file_fld, XmTRAVERSE_CURRENT);

	while(1)
	{
		XtAppNextEvent(NCQ_App, &x_event);
		XtDispatchEvent(&x_event);
	}
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_unxselectlst(int indx)
c
c   FUNCTION:  Select an item by index of the file listbox
c
c   INPUT:  indx: index number to be selected.
c
c   OUTPUT: none
c
c***********************************************************************
*/
void ncq_unxselectlst(indx)
int indx;
{
   if (listbox==0)
      return;
	indx++;
/*
	XmListSelectPos(listbox, indx, True);
*/
	XmListSelectPos(listbox, indx);
   indx -= 4;
   if (indx<1) indx = 1;
	XmListSetPos(listbox, indx);
	cur_sel = indx - 1;
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_unxselectfile(char *file)
c
c   FUNCTION:  Select an item by string from the file listbox
c
c   INPUT:  file: string to be selected
c
c   OUTPUT: none
c
c***********************************************************************
*/
void ncq_unxselectfile(file)
char *file;
{
	int sel;
	XmString str;
   if (listbox==0)
      return;
	str = XmStringCreateSimple(file);
	XmListSelectItem(listbox, str, True);
	XmStringFree(str);
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_unxreset_list()
c
c   FUNCTION:  Reset(clear) the file listbox
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void ncq_unxreset_list()
{
   if (listbox==0)
      return;
	XmListDeleteAllItems(listbox);
}

/***********************************************************************
c
c   SUBROUTINE:  ncq_unxdispmsg(char *msg)
c
c   FUNCTION:  Display a message
c
c   INPUT:  msg: message to be displayed
c
c   OUTPUT: none
c
c***********************************************************************
*/
void ncq_unxdispmsg(title, msg)
char *title, *msg;
{
	int n;
	Arg args[20];
	Widget dlg,but;
	XmString lstr;
	Position x,y,h,w;

	if (UU_BATCH != 0)
		printf(msg);
/*
.....Bring up Message Dialog
*/
	n = 0;
	lstr = XmStringCreateLtoR(msg, XmSTRING_DEFAULT_CHARSET);
	XtSetArg(args[n],XmNmessageString,lstr); n++;
/*
........Disable user interaction until
........Error message is acknowleged
*/
	XtSetArg(args[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL); n++;
	if (title==NULL)
	{
		XtSetArg(args[n],XmNtitle, "NCQ message box"); n++;
	}
	else
	{
		XtSetArg(args[n],XmNtitle, title); n++;
	}
	dlg = (Widget) XmCreateInformationDialog(NCQ_Parent, "MSG",args,n);
	XtUnmanageChild((Widget)XmMessageBoxGetChild(dlg,XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild((Widget)XmMessageBoxGetChild(dlg,XmDIALOG_HELP_BUTTON));
	but = (Widget) XmMessageBoxGetChild(dlg,XmDIALOG_OK_BUTTON);
	XmStringFree(lstr);
	XtAddCallback(dlg,XmNokCallback,(XtPointer)XtDestroyWidget,(XtPointer)
NULL);
	XtManageChild(dlg);
/*
....Warp cursor to OK Button
*/
	n = 0;
	XtSetArg(args[n],XmNx,&x); n++;
	XtSetArg(args[n],XmNy,&y); n++;
	XtSetArg(args[n],XmNwidth,&w); n++;
	XtSetArg(args[n],XmNheight,&h); n++;
	XtGetValues(but,args,n);
	x = x + w/2 ;
	y = y + h/2 ;
	XWarpPointer(XtDisplay(NCQ_Parent),None,XtWindow(dlg),0,0,0,0,x,y);
}

/***********************************************************************
c
c   FUNCTION: ncq_unxdel_pos(pos)
c
c         Delete a position from a filelist box
c
c   INPUT:  pos: position to be deleted
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void ncq_unxdel_pos(pos)
int pos;
{
	pos++;
	XmListDeletePos(listbox, pos);

	if (pos>1)
		pos = pos - 1;
	else
		pos = 1;
	XmListSelectPos(listbox, pos);
	cur_sel = pos - 1;
}

/**********************************************************************
c   SUBROUTINE:  ncq_unxadditem(char *item)
c
c   FUNCTION:  Add a item into a file listbox
c
c   INPUT:  item: item to be added
c
c   OUTPUT: none
c
c***********************************************************************
*/
void ncq_unxadditem(item)
char *item;
{
	char tmp[MAX_PATH];
	int nc;
	XmString str;
	strcpy(tmp, item);
	nc = strlen(tmp);
	while ((tmp[nc-1]=='\n')||(tmp[nc-1]=='\r')) nc--;
	tmp[nc] = '\0';

	if (listbox==0)
		return;
	str = XmStringCreateSimple(item);
	XmListAddItem(listbox, str, 0);
	XmListSelectItem(listbox, str, True);
	XmStringFree(str);
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
ncq_unxsel_lastpos()
{
	int count, indx;
	XtVaGetValues(listbox, XmNitemCount, &count, NULL);
   XmListSelectPos(listbox, count);
	cur_sel = count - 1;
   indx = count - 4;
   if (indx<1) indx = 1;
	XmListSetPos(listbox, indx);
}
/***********************************************************************
c
c   SUBROUTINE:  ncq_unxgetsel(int *sel)
c
c   FUNCTION:  Get current select of the file listbox
c

c   INPUT:  none
c
c   OUTPUT: sel: current select
c
c***********************************************************************
*/
int ncq_unxgetsel(sel)
int *sel;
{
   if (listbox==0)
      return -1;
	*sel = cur_sel;
	return cur_sel;
}

/*********************************************************************
**    I_FUNCTION     :  ncq_mfstatus_message(char *msg)
**          Output a string to main window's status text window
**    PARAMETERS
**       INPUT  :
**          msg                     message to output
**                        
**       OUTPUT :
**         			None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncq_mfstatus_message(msg)
char *msg;
{
	static int win_pos = 0;
	char tempmsg[256];
	sprintf(tempmsg, "%s\n", msg);
	XmTextInsert(ncq_stat_win, win_pos, tempmsg);
	win_pos = win_pos + strlen(tempmsg);
	XmTextShowPosition(ncq_stat_win, win_pos);
	XFlush(XtDisplay(ncq_stat_win));
}

/*********************************************************************
**    I_FUNCTION     : ncq_mfupd_nclinfo(info) 
**          Update the NCL information on Monitor window
**    PARAMETERS
**       INPUT  :
**          info: NCL information to update on Monitor window
**                        
**       OUTPUT :
**         			None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncq_mfupd_nclinfo(info)
NCLInfo info;
{
	char num[8];
	if (monitor_dialog==NULL)
		return 0;
	if (strlen(info.ppfile)>0)
		XmTextSetString(mon_edit[0], info.ppfile);
	else
		XmTextSetString(mon_edit[0], " ");
	if (strlen(info.macro)>0)
		XmTextSetString(mon_edit[4], info.macro);
	else
		XmTextSetString(mon_edit[4], " ");
	sprintf(num, "%d", info.current);
	XmTextSetString(mon_edit[1], num);
	info.highest = info.highest;
	sprintf(num, "%d", info.highest);
	XmTextSetString(mon_edit[2], num);
	sprintf(num, "%d", info.lines);
	XmTextSetString(mon_edit[3], num);
	sprintf(num, "%d", info.warn);
	XmTextSetString(mon_edit[5], num);
	sprintf(num, "%d", info.error);
	XmTextSetString(mon_edit[6], num);
	XFlush(XtDisplay(monitor_dialog));
}

#endif
