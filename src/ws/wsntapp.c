#include "usysdef.h"
#if UU_COMP==UU_WIN2K
/*********************************************************************
**    NAME         :wsntapp.c
**				 
**    CONTAINS     : 
**		uw_ntdnprompt(ws,num)
**		uw_ntstr_prompt(ws,prompt,loc,num)
**		uw_ntmenu_choice(wid,devno,xy,k,choice,menu)	
**		uw_ntwrprm(msg)
**		uw_ntset_cmdstr(msg)
**		uw_ntwrplabel(msg)
**		uw_ntprmerr(msg)
**		uw_ntgetprmerr(msg)
**		uw_ntgetprm(msg)
**		uw_ntgetprlabel(msg)
**		uw_ntwrstat(field,msg)
**		uw_ntget_cmdstr(cmdstr)	
**		uw_ntreset_prompt()
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntapp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:13
*********************************************************************/
#include<string.h>
#include "dasnog.h"
#include "dinput.h"
#include "dmark.h"
#include "dmotif.h"
#include "gi1.h"
#include "gtbl.h"
#include "zkeysym.h"
#include "driver.h"
#include "xenv1.h"

static char prmtxt[200] = "", errtxt[200] = "", labtxt[200]="";
extern struct
{
	Giclass curdevclas;     /* currently being used device class */
	Gint curdevno;       /* currently being used device number */
	Giclass curreqclas;     /* currently requested device class, or -1 */
	Gint curreqno;       /* currently requested device number */
} ug_wsdev;

/*********************************************************************
**    I_FUNCTION     :  uw_ntdnprompt(ws,num)
**       Take down prompt string.
**    PARAMETERS
**       INPUT  :
**          ws     = Workstation id.
**          num    = Prompt number to take down.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntdnprompt(ws,num)
Gws  ws;
Gint num;
{
	uw_ntccwrplabel(" ");
	return;
}

/*********************************************************************
**    I_FUNCTION     :  uw_ntstr_prompt(ws,prompt,loc,num)
**       Pop up prompt string at loc.
**    PARAMETERS
**       INPUT  :
**          ws     = Workstation id.
**          prompt = Prompt text
**          loc    = XY location of prompt in pixels
**          num    = Prompt id number.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntstr_prompt(ws,prompt,loc,num)
Gws     ws;
Gipoint *loc;
char    *prompt;
Gint     num;
{
//	uw_ntwrplabel(prompt);
	strcpy(labtxt,prompt);
	uw_ntccwrplabel(prompt);
	return;
}
/*********************************************************************
**    I_FUNCTION     :  uw_ntmenu_choice(ws,devno,xy,k,choice,menu)
**       
**		Defines and activates a DAS internal POPUP menu and 
**		returns the device number and choice
**      number of the menu selected, plus the input event.
**
**    PARAMETERS   
**      INPUT:
**         ws     = Workstation id 
**         devno  = choice device number
**      OUTPUT:
**         k      = ending key or choice value
**         xy     = loc position in dev coords
**         choice = menu choice number, or zero 
**         menu   = the menu number, or zero
**
**    RETURNS      :
**      0 = A keypad-2 key was hit, k contains the key number. 
**      1 = An ASCII kbd key was hit, k contains ascii value.
**      2 = A keypad-1 key was hit, k contains key number. 
**      3 = A tablet puck button was used, k contains number. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_ntmenu_choice(wid,devno,xy,k,choice,menu)	
Gws wid;
int devno,*k,xy[2],*choice,*menu;
{
	int i,n,pos[2],size[2],kinc,stat, select;
	char **p;
	char msg[200];
	Gchoicest *choicept;
	int ix;
/*
.....Get the parameters for this menu
*/
	choicept = &((*ug_gksstli.wsopen[wid].inptr).choicedata[devno-1]);
	n = choicept->record.number;
	p = choicept->record.strings;
	
/*
.....Initialize menu structure
*/
	pos[0] = -1; pos[1] = -1;
	size[0] = -1; size[1] = -1;
	stat = udm_init_menu_desc(*p,pos, size,&kinc);
	if (kinc >= UDM_menu_count)
	{
		UDM_menu[kinc].rows = n + 1;
		UDM_menu[kinc].cols = 1;
		UDM_menu[kinc].type = UDM_MTYPE_INTERNAL;
		UDM_menu[kinc].num = UDM_menu[kinc].rows;
/*
.....Initialize menu entries
*/
		stat = udm_init_menu();
		for (i=0; i<=n; i++,p++)
		{
			strcpy(UDM_menu[kinc].menus[i].name,*p);
			sprintf(UDM_menu[kinc].menus[i].file,"%d",i);
			UDM_menu[kinc].menus[i].kinc = kinc;

			UDM_menu[kinc].menus[i].bgcolor[0] = '\0';
			UDM_menu[kinc].menus[i].color[0] = 200;
			UDM_menu[kinc].menus[i].color[1] = 200;
			UDM_menu[kinc].menus[i].color[2] = 200;
		}
		UDM_menu_count++;
	}
	sprintf(msg,"Popup Menu %s", UDM_menu[kinc].menus[0].name);
	ud_rpwrcom(msg);
/*
.....Display menu
*/
	select = uw_ntchoice_menu(kinc,xy,k,choice,menu);
	xy[0] = 0; xy[1] = 0;
	*menu = devno;
/*
.....Make sure current menu is set to the active menu
*/
	ug_wsdev.curdevno = devno;
	ug_wsdev.curreqno = devno;
	return select;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntwrprm(msg)
**       Displays a message on the prompt line of the Prompt area.
**    PARAMETERS   
**       INPUT  : 
**          msg     = Text to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntwrprm(msg)
char *msg; 
{
	if (strcmp(prmtxt,msg) != 0)
	{
		uw_ntccwrlabel(msg);
		strcpy(prmtxt,msg);
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_ntset_cmdstr(msg)
**       Set command line text of single line
**    PARAMETERS   
**       INPUT  : 
**          msg     = Text to Set
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntset_cmdstr(msg)
char *msg; 
{
	uw_ntccset_cmdstr(msg);
}

/**********************************************************************
**    I_FUNCTION :  uw_mfwrprlabel(msg)
**       Displays a message on the user input line of the Prompt area.
**    PARAMETERS   
**       INPUT  : 
**          msg     = Text to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntwrplabel(msg)
char *msg; 
{
	if (strcmp(labtxt,msg) != 0)
	{
		strcpy(labtxt,msg);
		uw_ntccwrplabel(msg);
	}
}


/**********************************************************************
**    I_FUNCTION :  uw_ntprmerr(msg)
**       Displays a message on the error line of the Prompt area.
**    PARAMETERS   
**       INPUT  : 
**          msg     = Text to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntprmerr(msg)
char *msg;
{
/*
.....Display the message
*/
	if (strcmp(errtxt,msg) != 0)
	{
		strcpy(errtxt,msg);
		uw_ntccwrerr(errtxt);
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_ntgetprmerr(msg)
**       Returns the text from the prompt label field.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          msg     = Error line text.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntgetprmerr(msg)
char *msg;
{
	strcpy(msg,errtxt);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntgetprm(msg)
**       Returns the text from the prompt label field.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          msg     = Prompt text.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntgetprm(msg)
char *msg;
{
	strcpy(msg,prmtxt);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntgetprlabel(msg)
**       Returns the text from the prompt label field.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          msg     = Prompt label text.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntgetprlabel(msg)
char *msg;
{
	strcpy(msg,labtxt);
}



/**********************************************************************
**    I_FUNCTION :  uw_ntwrstat(field,msg)
**       Outputs a message to a field in the Status Area.
**    PARAMETERS   
**       INPUT  : 
**          field   = Field number to write to.
**			msg     = Message to output.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntwrstat(field,msg)
int field;
char *msg;
{
	uw_ntccwrstat(field, msg);
}


/**********************************************************************
**    I_FUNCTION :  uw_ntget_cmdstr(cmdstr)
**       Get the command line input of single line window
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          cmdstr: commmand input string
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntget_cmdstr(cmdstr)
char *cmdstr;
{
	uw_ntccget_cmdstr(cmdstr);
}


/**********************************************************************
**    I_FUNCTION : uw_ntreset_prompt()
**       Disables the command line text field and erases all prompt
**			lines.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntreset_prompt()
{
	char *nstr="";
	uw_ntset_cmdstr(nstr);
	uw_ntenable_cmd(0);
	uw_ntwrplabel(nstr);
	uw_ntwrprm(nstr);
	uw_ntprmerr(nstr);
}




#endif
