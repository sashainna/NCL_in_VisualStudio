/************************************************************************
**
**   FILE NAME: wsntform.h
**
**       Description - Functions and struct declarations for
**              CNCLForm class (NCL forms)
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntform.h , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/10/18 , 14:34:43
**********************************************************************
*/

#ifndef NCLFORM_H
#define NCLFORM_H

#include "udfdata.h"
#include "udforms.h"
#include "wsntdlgitem.h"
#include "wsnttxtwin.h"
#include "wsntfsview.h"

// HitTest Constants
#define DHT_CLOSE		0x01
#define DHT_CAPTION		0x04
#define DHT_MIN		0x03

class CNCLForm : public CDialog
{
	DECLARE_DYNAMIC(CNCLForm)
public:
	CNCLForm(CWnd* pParent = NULL, int dispflag = 0);
	~CNCLForm();
	CNCLFormScrollView *m_pScrollView;
	int Create(); 
	void repaint_pic();
	void InitFormDialog();
	void InitAdjust();
	void InitSectionButtons();
	void init_button(int i, UINT fid, COLORREF bcolor, COLORREF fcolor);
	void init_button2(int i, UINT pfid, COLORREF pbcolor, COLORREF pfcolor, UINT fid, COLORREF bcolor, COLORREF fcolor);
	void init_button3(int i, UINT fid, COLORREF bcolor, COLORREF fcolor);
	void init_button4(int i, UINT fid, COLORREF bcolor, char *imgfile);
	void init_listctl(int i, UINT fid);
	void init_listctl2(int i, UINT fid);
	void init_picarea(int i, UINT fid);
	void init_slider(int i, UINT fid);
	void set_picarea(int picno, int n_picarea, UD_PICAREA *picarea, UINT pID);
	void sort_tlist(UINT fldid, int isub, UD_SMETHOD sortfunc);
	void set_sort_func(int fldno, UD_SMETHOD sortfunc);
	void set_focus(int fieldno);
	int Getindx(int page);
	HBRUSH Onctlcolor2(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	CNCLTextWin *m_helpbox;
	CEdit *m_actedit;
	void initform(UD_FSTRUCT *fstruct, UD_FDATA *fdata);
	UD_METHOD m_method[MAX_FORMITEM];
	char m_method_ret[MAX_FORMITEM];
	char m_disp_mask[MAX_FORMITEM]; 
	char m_trav_mask[MAX_FORMITEM];
	UD_SMETHOD m_sortfunc[MAX_FORMITEM];
	UX_pathname m_filename;
	int m_disptype;
	int m_pocket;
	int m_fldno;
	int m_dspno;
	int m_framno;
	int m_picno;
	int m_type[MAX_FORMITEM];
	int m_datatype[MAX_FORMITEM];
	int m_prec[MAX_FORMITEM];
	int m_len[MAX_FORMITEM];
	int m_frmid;
	int m_return_key;
	CFont m_fieldFont[MAX_FORMITEM];
	CFont m_dfieldFont[MAX_FORMITEM];
	CFont m_ffieldFont[MAX_FORMITEM];
	char *m_helptext;
	UD_FSTRUCT m_fStruct;
	UD_FDATA m_fData;
	int *m_savfrm;
	int m_childnum, m_parentID;
	int m_childfrm[60];
	int uw_ntform_redisplay();
	int form_accept_close();
	void set_list(int fieldno, UD_LIST *form_list);
	void set_tlist(int fieldno, int *form_list);
	void set_text(int fieldno, char *val);
	void set_choice(int fieldno, int choice);
	void set_label(int fieldno, char* label);
	void set_butlabel(int fieldno, char* label);
	void reset_picture(int fieldno, int *val);
	void reset_fldlabel(int fieldno, char* label);
	void set_traverse_mask(int fieldno, int val);
	void set_display_mask(int fieldno, int val);
	void Get_field_data(int fieldno, UD_DDATA data, UU_LOGICAL str_flag);
	void FormInvis();
	void FormVis();
	void FormClose();
	void removechild(int formID);
	void addchild(int formID);
	void set_pocket(int flag) { m_pocket = flag; };
	int update_current_input(char *text=NULL);
	int get_idtype(UINT id);
	afx_msg void OnAccelFunctions(UINT nID);
	void CtrlListCallback(UINT id, UD_TABLEINFO *info);
	void OntlistFocus(UINT id);
	void Adjust_pos(CRect &rect);
	void Adjust_pos2(CRect &rect, int x, int y, int fcx, int fcy, int att_win, int ref_flag);
	void SetButColor(int fieldno, int color);
	void PicAreaClick(UINT fid, char *params);
	int Get_fldno_from_ID(UINT id);
	void setdlgtyp(int typ) 
	{
		m_dlgtyp = typ;
	};
	void setsecbut(int num);
	void OnSecButton(int indx);
	void EnableSection(char *sec_name, int flag);
	void SetSecColor(char *sec_name, int color[3], int bold);
	void GetSecHelpText(char *dsptext);
	void SelectSection(char *sec_name);
	void GetDataType(int n, int *data_type);
	void SetDataInitType(int n, int *data_type);
	void SetFieldColr(int fieldno, int fg, int bg);
	void HandleVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	void HandleHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	void EnableOKBut(UU_LOGICAL flag);
	void EnableCloseBut(UU_LOGICAL flag);
	void OnFilterTlist(int frmfld);
	void reset_picarea(int picno, int n_picarea, UD_PICAREA *picarea, int fldno);
//	void ReShowFrame();
	// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLForm)
	protected:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

protected:

	CWnd* m_pParent;
	DLGTEMPLATE m_dlgTempl;
	CDialogItem     m_rgDlgItem[MAX_FORMITEM];
	char m_title[256];
	int m_itemnum, m_secno;
	int m_current_fld;
	UINT m_idtable[MAX_FORMITEM];
	UINT m_idsection[MAX_SECTION_BUT+1];
	int m_formchcnum[MAX_FORMITEM];
	char ** m_formchoice[MAX_FORMITEM];
	int m_input[MAX_FORMITEM];
	int m_pick_mask[MAX_FORMITEM][UD_NMENTWD];
	short m_modal[MAX_FORMITEM];
	char m_fldlabel[MAX_FORMITEM][80];
	char m_dsplabel[MAX_FORMITEM][80];
	char m_frame_title[20][80];
	int m_init;
	UINT m_def_id;
	HACCEL m_accel;
	ACCEL *m_form_accel;
	int m_form_accelnum;
	int m_hotkey_num[4];
	int m_forward;
	int m_dlgtyp;
	int m_init_data_type_flag, m_data_type[200];

	UD_DASIN m_range[MAX_FORMITEM][2];
	CBrush* m_pEditBkBrush;
	CBrush* m_BkBrush[MAX_FORMITEM];
	CBrush* m_pBkBrush[MAX_FORMITEM];
	CBrush* m_dBkBrush[MAX_FORMITEM];
	CBrush* m_fBkBrush[MAX_FORMITEM];
	CRect m_oldRect, m_Rect;
	CRect *m_fldrec, *m_fldlrec, *m_dsprec, *m_frmrec, *m_picrec;
	CRect m_oldbox1, m_oldbox2;
	int m_show;

	int NCL_form_SaveData();
	int FormPick(int fldno);
	int FormLoc(int fldno);
	void FormColor(int fldno);
	void PickColor(int fldno);
	int FormSelect(int fldno);
	int form_ckdata(int fldno, char *string, int *flag);
	int chk_field(int fldno, int flag, int *redisp, UD_FSTAT fstat);
	LRESULT Execute_command(UINT id);
	void Form_unhilite_selected();
	void adjust_height(int fieldno);

	CRect m_rcIcon;
	CRect m_rcClose, m_rcMin, m_oldsize;
	DWORD m_LastHit;
	DWORD m_ButtonDown;
	DWORD HitTest(CPoint pt);
	void HandleOnSize(int dx, int dy, int flag);
//	int RunModalLoop(DWORD dwFlags);
	// Generated message map functions
	//{{AFX_MSG(CNCLForm)
//	virtual INT_PTR DoModal();

	virtual void OnSize( UINT nType, int cx, int cy );
	virtual afx_msg void OnMove(int x, int y);
	virtual BOOL OnInitDialog();
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	afx_msg void OnPaint();
	afx_msg void FormCancel();
	afx_msg void FormAccept();
	afx_msg void FormHelp();
//for normal button callbacks
	LRESULT  FormUserCallbacks1(WPARAM wparm, LPARAM lparm);
//for list box selection callbacks
	LRESULT  FormUserCallbacks2(WPARAM wparm, LPARAM lparm);
//for combo box selection callbacks
	LRESULT CNCLForm::FormUserCallbacks3(WPARAM wparm, LPARAM lparm);
//for edit box edit change callbacks
	LRESULT CNCLForm::FormUserCallbacks4(WPARAM wparm, LPARAM lparm);
//for edit box focus callbacks
	LRESULT CNCLForm::FormUserCallbacks5(WPARAM wparm, LPARAM lparm);
//for Comb/list box focus callbacks
	LRESULT CNCLForm::FormUserCallbacks7(WPARAM wparm, LPARAM lparm);
//"hit return on edit fields" callbacks
	afx_msg void FormUserCallbacks6();
	afx_msg void FormUserCallbacks0(UINT id);
	virtual void OnClose();
	afx_msg void OnFormClose();
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);

	afx_msg void OnNcPaint();
    afx_msg void OnNcLButtonDown(UINT nHitTest, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);	
	afx_msg LRESULT OnNcHitTest(CPoint point);
	afx_msg BOOL OnNcActivate(BOOL bActive);
	afx_msg void OnFormTabbed();
	afx_msg void OnFormSTabbed();
	afx_msg void OnFormTextHighlight();
	LRESULT OnKickIdle(WPARAM wparm, LPARAM lparm);
	LRESULT OnItemClick(WPARAM wParam, LPARAM lParam);
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	LRESULT  FormUserCBLDblclk(WPARAM wparm, LPARAM lparm);
	
	LRESULT OnCtrlColorStatic(WPARAM wParam, LPARAM lParam);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif

