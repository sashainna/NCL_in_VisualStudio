/*********************************************************************
**  NAME:  PtdFrameWindow.h
**  Description:
**				all member function and variable for class PtdFrameWindow
**				
**    CONTAINS:
**
**    COPYRIGHT 1994 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       PtdFrameWindow.h , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**       09/11/13 , 12:58:30
*********************************************************************/
#ifndef PTDFRAMEWINDOWH
#define PTDFRAMEWINDOWH

#include "PtdMainWindow.h"
#include "PtdCommandList.h"
#include "PtdTemplate.h"


class PtdMenuBar;
class PtdView;

#define PTED_TYPING     1
#define PTED_CUT        2
#define PTED_PASTE      3
#define PTED_REPLACE    4
#define PTED_DELETE     5
#define PTED_INSERT     6
#define PTED_CONVERT    7
#define PTED_RESEQUENCE 8
#define PTED_FORMAT     9
#define PTED_UNFORMAT   10
#define PTED_REVERSE    11
#define PTED_INCLUDE    12
#define PTED_GET        13
#define PTED_ADD        14
#define PTED_MIR        15
#define PTED_MULT       16
#define PTED_ROTATE     17
#define PTED_SCALE      18
#define PTED_TRANS      19
#define PTED_CANT       20

#define REDO_COM  2
#define UNDO_COM  1


class PtdFrameWindow : public PtdMainWindow
{
	
   private:
	char *m_sLogMessage;
	PtdCommandList *m_pFileCommands;
	PtdCommandList *m_pEditCommands;
	PtdCommandList *m_pViewCommands;
	PtdCommandList *m_pFindCommands;
	PtdCommandList *m_pConvertCommands;
	PtdCommandList *m_pWindowCommands;
	PtdCommandList *m_pOptionCommands;
	PtdCommandList *m_pHelpCommands;
    
	PtdView* m_parent;
	static int m_num;
	int m_id;
	int m_actcom;
	int m_buffer_type;   /* 1: user buffer, use "New" open this buffer      */
								/* 2: Backup buffer, use "Backup" open this buffer */
								/* 3: Text read only */

	protected:
    
	PtdMenuBar		*m_pMenuBar;
	PtdView			*m_pView;

	int m_undo_menu;
	int m_flist;
	char m_filelist[5][UX_MAX_PATH];
	char m_flist_name[UX_MAX_PATH];

	virtual void CreateMenuPanes();  
	virtual void CreateToolButtons(){}

  public:
	int exitOk;
	char m_filen[UX_MAX_PATH];
	int m_ftype;
	int m_openflag;
	char m_input_mdf[UX_MAX_PATH], m_output_mdf[UX_MAX_PATH];
/*
.....0: main window
*/
	int m_type;
    
	enum FrameMenu
	{
		PTD_FILE_NEW=1, 
		PTD_FILE_OPEN,
		PTD_FILE_INCLUDE,
		PTD_FILE_LOADCUTTERFILE,
		PTD_FILE_SAVEAS,
		PTD_FILE_SAVE,
		PTD_FILE_RUN,
		PTD_FILE_LOADMA,
		PTD_FILE_LOADMAI,
		PTD_FILE_LOADMAO,
		PTD_FILE_FILE1,
		PTD_FILE_FILE2,
		PTD_FILE_FILE3,
		PTD_FILE_FILE4,
		PTD_FILE_FILE5,
		PTD_FILE_CLOSE,
		PTD_FILE_QUIT,
		PTD_EDIT_UNDO,
		PTD_EDIT_REDO,
		PTD_EDIT_REPEAT,
		PTD_EDIT_COPY,  
		PTD_EDIT_PASTE,  
		PTD_EDIT_CUT,  
		PTD_EDIT_DELETE,  
		PTD_EDIT_INSERTLN,
		PTD_EDIT_DELETELN,
		PTD_EDIT_REVER,
		PTD_EDIT_INP,
		PTD_EDIT_OUT,
		PTD_VIEW_TOP,
		PTD_VIEW_BOTTOM,
		PTD_VIEW_STATUS,
		PTD_VIEW_REFLUSH,
		PTD_FIND_IT,
		PTD_FIND_NEXT,
		PTD_FIND_PREV,
		PTD_FIND_ALL,
		PTD_FIND_REPLACE,
		PTD_WINDOW_CLA,  
		PTD_WINDOW_PU,  
		PTD_WINDOW_CL,  
		PTD_WINDOW_APT,  
		PTD_WINDOW_SIM,  
		PTD_WINDOW_CUT,  
		PTD_WINDOW_TOSIM,  
		PTD_WINDOW_LOADCUTTER,  
		PTD_WINDOW_COMMAND,  
		PTD_CONVERT_CONVERT,
		PTD_CONVERT_CONVERT2,
		PTD_CONVERT_FORMAT,
		PTD_CONVERT_FORMAT2,
		PTD_CONVERT_UNFORM,
		PTD_CONVERT_UNFORM2,
		PTD_CONVERT_BAD,
		PTD_CONVERT_BAD2,
		PTD_CONVERT_RESEQ,
		PTD_CONVERT_LENGTH,
		PTD_CONVERT_LENGTH2,
		PTD_CONVERT_SET,
		PTD_CONVERT_NCTOAPT,
		PTD_CONVERT_APTTONC,
		PTD_CONVERT_RUN,
		PTD_OPTION_PWORKS,
		PTD_HELP_ABOUT
	};

	
    virtual void Initialize();
    virtual void ManageInitialize();

/*
.....0, main window
.....1, normal child window
.....2, Backup child window
.....3, Text read only window
*/
    PtdFrameWindow(char *name, int type=0);
	virtual ~PtdFrameWindow();
	static void UnManageCallback(Widget, XtPointer, XtPointer);
	virtual void SetTitle(char *);
	PtdView *View() { return m_pView;}
	void SetView(PtdView *pView) { m_pView = pView;}

	void FrameCallback(FrameMenu, XtPointer);
	void SetFileTypeFromName(char *fileName);
	int Programload(char *fileName, int verify=0);
	void SetPreFile();
	void SetFile(char*);
	void GetFile(char*);
	int SafeClose();
	void SetParent(PtdView *view) { m_parent = view; }
	int GetId () { return m_id; }
	void SetId(int id) { m_id = id; }
	
	void ProgramNew();
	void ProgramSaveAs(char *);
	void Programopen(void);
	void FileLoadCutter(void);
	void Display_about();
	int HandleSafeClose();
	void Include();
	void OnEditEditinput();
	void OnEditEditoutput();
	void OnFileLoadinputmdf();
	void OnFileLoadoutputmdf();
	void OnFileLoadmdf();
	void OnSetRegister();
	void OnFileFileList(int indx);
	void Reset_menu();
	void Reset_undo_menu(char *label1=NULL, char* label2=NULL, int enable1=0,
			int enable2 = 0, int flag=0, int setflag=1);
	void Pworks_options();
	void Runpworks();
	void Set_ftype(int ftype);
	void OnWindowLoadCutterData();
	void OnConvertToSim();
	void OnConvertNCToSim();
	void OnCommandOn();
	void OnViewStatus();
	void SetName(char *fname) { 
				strcpy(m_filen, fname); 
				m_openflag = 1;
			}
	void GetText(char**, int*);
};
#endif
