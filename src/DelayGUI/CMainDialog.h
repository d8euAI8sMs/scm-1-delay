#pragma once

#include <util\common\gui\SimulationDialog.h>

#include <interop\types.h>

// CMainDialog dialog

class CMainDialog : public CSimulationDialog
{
	DECLARE_DYNAMIC(CMainDialog)

public:
	CMainDialog(CWnd* pParent = nullptr);   // standard constructor
	virtual ~CMainDialog();

public:
	hsd_callback m_callbackCb;

// Dialog Data
#ifdef AFX_DESIGN_TIME
	enum { IDD = IDD_CMainDialog };
#endif

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()
public:
	virtual BOOL OnInitDialog();
	virtual BOOL DestroyWindow();
	void Refresh();
};
