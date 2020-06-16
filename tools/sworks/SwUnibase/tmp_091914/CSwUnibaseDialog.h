#pragma once


// CSwUnibaseDialog dialog

class CSwUnibaseDialog : public CDialog
{
	DECLARE_DYNAMIC(CSwUnibaseDialog)

public:
	CSwUnibaseDialog(CWnd* pParent = NULL);   // standard constructor
	virtual ~CSwUnibaseDialog();

// Dialog Data
	enum { IDD = IDD_CSwUnibaseDialog };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()
};
