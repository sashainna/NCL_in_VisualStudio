/************************************************************************
**
**   FILE NAME: wsntDDform.h
**
**       Description - Functions and struct declarations for
**              CNCLDDform class (Class for Drag&Drop form)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDform.h , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:20:51
**********************************************************************
*/
#ifndef WSNTDDFORM_H
#define WSNTDDFORM_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "wsntmDropTarget2.h"
#include "udforms.h"
#include "wsntFormProp.h"
#include "wsntwininfo.h"

// *****************************************************************
// CNCLDDform window
// *****************************************************************
class CNCLDDform : public CWnd
{
	DECLARE_DYNAMIC(CNCLDDform)

public:
	CWnd *m_parent;
	CNCLDDform(CWnd *parent);
	virtual ~CNCLDDform();
	void	InitDrag();
	void OnDragDropCallback(CPoint pt, char *input_text, int flag = 0);
	void OnDragDropHotSpotCallback(CPoint pt, char *input_text);
	int DeleteItemOnPt(CPoint pt, CWnd *wnd, int flag=0, int *delnum = NULL, int *deltyp = NULL, CNCLWinInfo *del_win1=NULL, CNCLWinInfo *del_win2=NULL);
	int SaveUndoInfo(int action, CNCLWinInfo *info1, CNCLWinInfo *info2);
	void Delete_dispwin(int item_no, int sel=0);
	void Delete_frmwin(int item_no, int sel=0);
	void Delete_picwin(int item_no, int sel=0);
	void Delete_inpwin(int item_no, int sel=0);
	void SetActionItem(int action,int itemno, int type, int itype=-1);
	int SaveForm(char* filename, char *title, int cx, int cy, int type, CString helpstr, int flag=1);
	void ReplaceItem(int olditem_no, int newitem_no, int type = 4);
	void ReplaceDispItem(int olditem_no, int newitem_no);
	void ReplaceFrameItem(int olditem_no, int newitem_no);
	void ReplacePicItem(int olditem_no, int newitem_no);
	void OnSelectItem(int itemno, int type, int itype, int flag);
	void OnSelectAutoItem(int itemno, int type, int itype, int flag);
	int  HitTest(CPoint point);
	void DrawSizeRect(CDC *dc, CRect &rect, int vis);
	void DrawSelect(int focus);
	void HandleButtonDown(UINT nFlags, CPoint point, int dropdown = 0);
	void HandleButtonup(UINT nFlags, CPoint point, int dropdown = 0);
	void HandleMouseMove(UINT nFlags, CPoint point);
	void Reset_selrec(CRect rect);
	CImageList * GetDragAllImage();
	HBITMAP CreateAllToBitmap(int *cx, int *cy, int macro_num=-1);
	void DrawSelectWindowFrame(CDC *dc);
	void DrawHotspotFrame(CDC *dc, int item_no);
	void DrawAutoWindowFrame(CDC *dc, int macro_num);
	void DrawSelectArray(CDC *dc);
	void SelectItem(int item_no, int type);
	void DeleteSelItem(int item);
	void AdjuctMoveWindows(int flag);
	void LoadFormItem(UD_FSTRUCT *fstruct);
	void UpdateDDFormView();
	void OpenPropertyPage(CNCLFormProp *prop_dlg, int flag = 0); 
	void SaveProperty();
	int SaveUndoItem();
	void SaveUndoItemInfo(CNCLWinInfo* pItem);
	void SaveUndoItem2(int action);
	void UpdateWinProp(CNCLWinInfo* pItem, CNCLWinInfo* out_pItem, int dtype, int action);
	void SaveUndoNumber(int undo_num, int frameno, int picno, int dispno, int inputno, 
		int pageno, int selitem, int seldtyp, int selityp);
	void UndoItem();
	void RedoItem();
	void CreateWinFromInfo(CNCLWinInfo* pItem, int dispno, int frameno, int picno, int inputno);
	int Handle_frame_event(int evt, UINT nFlags, CPoint point);
	void MoveItemPlace(int new_indx, int old_indx, int dtype=4);
	void DrawGuides(CRect rect);
	void updateWindow_draw(int flag=0, CRect *rect = NULL);
	int is_frame_clicked(CPoint point);
	int IsWindowSelected(CWnd *wnd);
	void ChangePromptType(int type);
	void SetMacroFlag(int flag)
	{
		m_macro_flag = flag;
	};
	void OnCheckMacro(int num, int checked);
	int CheckMacroForm(char *err);
	void GetInputItemLabel(int item, CString &label);
	int GetItemWinInfo(int item, CNCLWinInfo *del_info1, CNCLWinInfo *del_info2);
	void UpdatePropertyPos(int x, int y);
	void UpdatePropertySize(int cx, int cy);
	void UpdatePropertyPicture(CNCLFormProp *prop); 
	int IsSelItem(int itemno, int type, int itype);
	int CheckPos(int &x, int &y);
	void SetSizeDir (int dir) 
	{ 
		m_sizedir = dir; 
		m_hittest = dir;
	};
	void OnSecButton(int indx);
	void SetsecNum(int sec)
	{
		m_secno = sec;
	};
	int isallsec();
	void DeleteSec(int indx);
	void ReplaceSecNo(int old_no, int new_no);
	void reset_selvalue();
	void DisplayAll();
	void OnDragDropSection(int secnum, char *drop_text);
	void SaveUndoItems(int action /*add,delete, edit..*/, int dtype, int itype, int itemno, int extra=0);
	void SaveRedoItem(CNCLWinInfo* pItem);
	void SaveRedoNumber(int redo_num, int frameno, int picno, int dispno, int inputno, 
		int pageno, int selitem, int seldtyp, int selityp);
	void SetWinPropFromInfo(CNCLWinInfo* pItem, int dispno, int frameno, int picno, int inputno);
	int SaveUndoSecItem(int dtype, int item_no);
	void Remove_redoList();
	void AddUndo_addSection(int secno);
	void Reset_Undo_Redo();
	int HandleHotSpot(CPoint pt, char *class_str, int itemno); 
	void CreateHotSpotRect(CRect rect);
	void ReseteHotSpotRect();
	void DrawHotspotRect(CDC *dc, CRect &rect);
	void UpdatePropertyHSPTSize(CRect sizerec, int itemno);
//	void Convert_pic_rect(CRect in_rect, CRect *out_rect, int flag);
	void Convert_pic_rect(float in_rect[4], float out_rect[4], int flag);
	void SetPicAreaFromProp(CNCLFormProp *prop);
	void LoadActiveHotSpot(int dtype, int itype, int inputno, int hpnum);
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLDDform)
	//}}AFX_VIRTUAL

protected:
	int m_no_undo_save, m_no_redo, m_reset_redo;
	int m_frmtyp;
	CRect m_selrec, m_selsize[10], m_sizerec, m_hotspot_rect;
	int m_hittest, m_sizedir, m_selitem, m_seldtyp, m_resize;
	CWnd *m_selwin;
	int m_selarray[200],  m_seltype[200], m_selnum;
	int m_update_area;
	CRect m_update_rect0[200], m_update_rect1[200], m_update_rect2[200], m_update_rect3[200], 
		m_update_rect4[200], m_update_rect5[6];
	int m_select_draw;
	int m_macro_flag, m_mwin_no;
	UINT	m_TimerID;
	CPoint	m_StartPoint;
	int m_buttondown;
	HCURSOR m_cursor[11];
	int m_type[200], m_disp_secpage[200], m_inp_secpage[200], m_pic_secpage[200], m_frm_secpage[200];
	CWnd *m_inpwin[200];
	CWnd *m_dinpwin[200];
	CWnd *m_disp_win[200];
	CWnd *m_frame[200];
	CWnd *m_picture[200], *m_current_pic;
	CWnd *m_macro_win[6];
	int m_dispno, m_inputno, m_frameno, m_picno, m_current_sec, m_secno;
	CFont m_Font;
	int m_prec[100];
	int m_len[100];
	CFont m_fieldFont[100];
	CFont m_dfieldFont[100];
	CFont m_ffieldFont[20];
	UD_DASIN m_range[100][2];
	int m_range_flag;
	int m_input[100];
	int m_dropdown, m_multi_drag;
	int DeleteItem(int item_no, int type, int sel=0);
	int is_valid_area(CRect rect, CPoint point);
	void Re_arrange_item();
	//{{AFX_MSG(CNCLDDform)
	afx_msg void OnDestroy();
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnDeleteItems();
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnPaint();
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
	
	LRESULT OnCtrlColorEdit(WPARAM wParam, LPARAM lParam);
	LRESULT OnCtrlColorBtn(WPARAM wParam, LPARAM lParam);
	LRESULT OnCtrlColorListBox(WPARAM wParam, LPARAM lParam);
	LRESULT OnCtrlColorScroll(WPARAM wParam, LPARAM lParam);
	LRESULT OnCtrlColorStatic(WPARAM wParam, LPARAM lParam);
private:
//	int m_undo_num, m_undo_delno, m_undo_deltyp;
	int m_action, m_undo_num, m_undo_itemno, m_undo_typ,
		m_action_item, m_action_typ, m_action_ityp;
	int m_reset_color;
	CTypedPtrList<CPtrList,CNCLWinInfo *> m_pUndoList; /* undo list */
	CTypedPtrList<CPtrList,CNCLWinInfo *> m_pRedoList; /* redo list */
	CNCLMnDropTarget2		*m_TargetDrop;
	friend class CNCLMnDropTarget2;
	friend class CNCLFdsnFrame;
	friend class CNCLFormView;
	friend class CNCLFormMView;
};
#endif
