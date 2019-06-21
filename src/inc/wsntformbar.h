/************************************************************************
**
**   FILE NAME: wsntformbar.h 
**
**       Description - Functions and struct declarations for
**              CNCLFormBar class (NCL forms)
**    COPYRIGHT 2005 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntformbar.h , 25.8
**    DATE AND TIME OF LAST  MODIFICATION
**			01/20/17 , 09:57:52
**********************************************************************
*/

#ifndef NCLFORMBAR_H
#define NCLFORMBAR_H

#include "udfdata.h"
#include "udforms.h"
#include "wsntdlgbar.h"
#include "wsnttxtwin.h"
#include "wsntclrbtn.h"
#include "wsntlstctl.h"
#include "wsntlstctl2.h"
#include "wsntpicsel.h"
#include "wsntsliderctrl.h"
#include "wsntnclbutton.h"
// HitTest Constants
#define DHT_CLOSE		0x01
#define DHT_NCBAR		0x02
#define DHT_HELP		0x03
#define DHT_CAPTION		0x04

class CNCLFormBar : public CNCLDialogBar
{
	DECLARE_DYNAMIC(CNCLFormBar)
public:
	CNCLFormBar(CWnd* pParent = NULL, int dispflag = 0);
	~CNCLFormBar();
	BOOL CreateFormBar(); 
	CNCLTextWin *m_helpbox;
	CEdit *m_actedit;
	void initform(UD_FSTRUCT *fstruct, UD_FDATA *fdata);
	void InitAdjust();
	void init_button(int i, UINT fid, COLORREF bcolor, COLORREF fcolor);
	void init_button2(int i, UINT pfid, COLORREF pbcolor, COLORREF pfcolor, UINT fid, COLORREF bcolor, COLORREF fcolor);
	void init_button3(int i, UINT fid, COLORREF bcolor, COLORREF fcolor);
	void init_listctl(int i, UINT fid);
	void init_listctl2(int i, UINT fid);
	void init_slider(int i, UINT fid);
	void sort_tlist(UINT fldid, int isub, UD_SMETHOD sortfunc);
	void set_sort_func(int fldno, UD_SMETHOD sortfunc);
	void set_focus(int fieldno);
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
	int m_forward;
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
	void Get_field_data(int fieldno, UD_DDATA data,UU_LOGICAL str_flag);
	void FormInvis();
	void FormVis();
	void FormClose();
	void FormHelp();
	void removechild(int formID);
	void addchild(int formID);
	void Recreate_button(int i, LPCTSTR lpszCaption, DWORD dwStyle, RECT& rect, UINT nID,
		COLORREF bcolor, COLORREF fcolor);
	void Recreate_button2(int i, LPCTSTR lpszCaption, DWORD dwStyle, RECT& rect, UINT nID,
		COLORREF bcolor, COLORREF fcolor);
	void Recreate_button3(int i, LPCTSTR lpszCaption, DWORD dwStyle, RECT& rect, UINT nID,
		COLORREF bcolor, COLORREF fcolor, int type);
	void Recreate_listctl(int i, char *listptr, DWORD dwStyle,
										 RECT& rect, UINT nID);
	void Recreate_listctl2(int i, char *listptr, DWORD dwStyle,
										 RECT& rect, UINT nID);
	void Create_Slider(int i, UINT fid, DWORD dwStyle, RECT& rbtn, 
				int range1, int range2, int init_value, UINT budid, int vert);
	void SetButColor(int fieldno, int color);
	int Get_fldno_from_ID(UINT id);
	void Create_PicArea(int indx, char *name, char* filename, RECT& rect, UINT nID);
	void Reset_picture_file(int indx, char* filename);
	void set_picarea(int picno, int n_picarea, UD_PICAREA *picarea, UINT pID);
	void init_picarea(int i, UINT fid);
	void PicAreaClick(UINT fid, char *params);
	void GetDataType(int n, int *data_type);
	void SetDataInitType(int n, int *data_type);
	void SetFieldColr(int fieldno, int fg, int bg);
	// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLFormBar)
	protected:
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

protected:

	CWnd* m_pParent;
	DLGTEMPLATE m_dlgTempl;
	CDialogItem     m_rgDlgItem[MAX_FORMITEM];
	char m_title[256];
	int m_itemnum;
	int m_current_fld;
	UINT m_idtable[300];
	int m_formchcnum[MAX_FORMITEM];
	CNCLColorButton m_button[MAX_FORMITEM], m_button2[MAX_FORMITEM];
	CNCLButton m_button3[MAX_FORMITEM];
	CNCLListCtrl m_listctl[MAX_FORMITEM];
	CNCLListCtrl2 m_listctl2[MAX_FORMITEM];
	CNCLPicSelWin *m_picture[MAX_FORMITEM];
	CNCLSliderCtrl m_slider[MAX_FORMITEM];
	char ** m_formchoice[MAX_FORMITEM];
	int m_input[MAX_FORMITEM];
	int m_pick_mask[MAX_FORMITEM][UD_NMENTWD];
	short m_pmodal[MAX_FORMITEM];
	char m_fldlabel[MAX_FORMITEM][80];
	char m_dsplabel[MAX_FORMITEM][80];
	char m_frame_title[20][80];
	int m_init;
	HACCEL m_accel;
	ACCEL *m_form_accel;
	int m_form_accelnum;
	int m_hotkey_num[4];
	int m_init_data_type_flag, m_data_type[200];

	UD_DASIN m_range[MAX_FORMITEM][2];
	CBrush* m_pEditBkBrush;
	CBrush* m_BkBrush[MAX_FORMITEM];
	CBrush* m_pBkBrush[MAX_FORMITEM];
	CBrush* m_dBkBrush[MAX_FORMITEM];
	CBrush* m_fBkBrush[MAX_FORMITEM];

	int NCL_form_SaveData();
	int FormPick(int fldno);
	int FormLoc(int fldno);
	void FormColor(int fldno);
	void PickColor(int fldno);
	int FormSelect(int fldno);
	int form_ckdata(int fldno, char *string, int *flag);
	int chk_field(int fldno, int flag, int *redisp);

	CRect m_rcIcon;
	CRect m_rcClose, m_rcHelp;
	DWORD m_LastHit;
	DWORD m_ButtonDown;

	// Generated message map functions
	//{{AFX_MSG(CNCLFormBar)
	afx_msg LRESULT HandleInitDialog(WPARAM, LPARAM);
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	afx_msg void OnPaint();
	afx_msg void FormCancel();
	afx_msg void FormAccept();
	afx_msg void OnHelp();
//for normal button callbacks
	afx_msg void FormUserCallbacks1(UINT id);
//for list box selection callbacks
	afx_msg void FormUserCallbacks2(UINT id);
//for combo box selection callbacks
	afx_msg void FormUserCallbacks3(UINT id);
//for edit box edit change callbacks
	afx_msg void FormUserCallbacks4(UINT id);
//for edit box focus callbacks
	afx_msg void FormUserCallbacks5(UINT id);
//"hit return on edit fields" callbacks
	afx_msg void FormUserCallbacks6();
//for Comb/list box focus callbacks
	afx_msg void FormUserCallbacks7(UINT id);
	virtual void OnClose();
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
    afx_msg void OnNcLButtonDown(UINT nHitTest, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);	
	afx_msg BOOL OnNcActivate(BOOL bActive);
	afx_msg void OnFormTabbed();
	afx_msg void OnFormSTabbed();
	afx_msg void OnFormTextHighlight();
	afx_msg void OnAccelFunctions(UINT id);
	void CtrlListCallback(UINT id, UD_TABLEINFO *info);
	afx_msg void FormUserCallbacks10(UINT id, NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void FormUserCallbacks11(UINT id, NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void FormUserCallbacks12(UINT id, NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void FormUserCallbacks13(UINT id, NMHDR *pNMHDR, LRESULT *pResult);
	LRESULT OnItemClick(WPARAM wParam, LPARAM lParam);
	afx_msg void OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif

