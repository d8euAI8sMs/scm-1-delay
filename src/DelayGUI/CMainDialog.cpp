// CMainDialog.cpp : implementation file
//

#include "pch.h"
#include "DelayGUI.h"
#include "CMainDialog.h"
#include "afxdialogex.h"


// CMainDialog dialog

IMPLEMENT_DYNAMIC(CMainDialog, CSimulationDialog)

CMainDialog::CMainDialog(CWnd* pParent /*=nullptr*/)
    : CSimulationDialog(IDD_CMainDialog, pParent)
	, m_callbackCb(0)
{

}

CMainDialog::~CMainDialog()
{
}

void CMainDialog::DoDataExchange(CDataExchange* pDX)
{
	CSimulationDialog::DoDataExchange(pDX);
}


BEGIN_MESSAGE_MAP(CMainDialog, CSimulationDialog)
END_MESSAGE_MAP()


// CMainDialog message handlers


BOOL CMainDialog::OnInitDialog()
{
	CSimulationDialog::OnInitDialog();

	if (m_callbackCb != 0) m_callbackCb(HSDEV_PRE_SHOW, 0, 0);

	return TRUE;  // return TRUE unless you set the focus to a control
				  // EXCEPTION: OCX Property Pages should return FALSE
}


BOOL CMainDialog::DestroyWindow()
{
	if (m_callbackCb != 0) m_callbackCb(HSDEV_PRE_HIDE, 0, 0);

	return CSimulationDialog::DestroyWindow();
}


void CMainDialog::Refresh()
{
	if (m_callbackCb != 0) m_callbackCb(HSDEV_POST_REFRESH, 0, 0);
}
