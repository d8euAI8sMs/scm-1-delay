// DelayGUI.cpp : Defines the initialization routines for the DLL.
//

#include "pch.h"
#include "framework.h"
#include "DelayGUI.h"
#include "CMainDialog.h"
#include <interop\types.h>

// Dummy exported function (otherwise no .lib is generated and build fails)
#include <cstdio>
extern "C" {
	__declspec(dllexport) void dummy() {
		printf("Hello from dll!");
	}
}

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

//
//TODO: If this DLL is dynamically linked against the MFC DLLs,
//		any functions exported from this DLL which call into
//		MFC must have the AFX_MANAGE_STATE macro added at the
//		very beginning of the function.
//
//		For example:
//
//		extern "C" BOOL PASCAL EXPORT ExportedFunction()
//		{
//			AFX_MANAGE_STATE(AfxGetStaticModuleState());
//			// normal function body here
//		}
//
//		It is very important that this macro appear in each
//		function, prior to any calls into MFC.  This means that
//		it must appear as the first statement within the
//		function, even before any object variable declarations
//		as their constructors may generate calls into the MFC
//		DLL.
//
//		Please see MFC Technical Notes 33 and 58 for additional
//		details.
//

// CDelayGUIApp

BEGIN_MESSAGE_MAP(CDelayGUIApp, CWinApp)
END_MESSAGE_MAP()


// CDelayGUIApp construction

CDelayGUIApp::CDelayGUIApp()
	: m_bInitialized(FALSE)
	, m_callbackCb(0)
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}


// The one and only CDelayGUIApp object

CDelayGUIApp theApp;


// CDelayGUIApp initialization

BOOL CDelayGUIApp::InitInstance()
{
	if (!m_bInitialized) return TRUE;

	// InitCommonControlsEx() is required on Windows XP if an application
	// manifest specifies use of ComCtl32.dll version 6 or later to enable
	// visual styles.  Otherwise, any window creation will fail.
	INITCOMMONCONTROLSEX InitCtrls;
	InitCtrls.dwSize = sizeof(InitCtrls);
	// Set this to include all the common control classes you want to use
	// in your application.
	InitCtrls.dwICC = ICC_WIN95_CLASSES;
	InitCommonControlsEx(&InitCtrls);

	CWinApp::InitInstance();

	CMainDialog dlg;
	dlg.m_callbackCb = m_callbackCb;
	m_pMainWnd = &dlg;

	if (m_callbackCb != 0) m_callbackCb(HSDEV_POST_INIT, 0, 0);

	INT_PTR nResponse = dlg.DoModal();

	if (m_callbackCb != 0) m_callbackCb(HSDEV_PRE_EXIT, 0, 0);

	return TRUE;
}

void hsd_init() {
}

void hsd_show_and_wait(hsd_callback cb)
{
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	theApp.m_bInitialized = true;
	theApp.m_callbackCb = cb;
	if (theApp.InitInstance())
	{
		theApp.Run();
	}
}

void hsd_refresh() {
	static_cast<CMainDialog*>(theApp.GetMainWnd())->Refresh();
}

void hsd_exit() {
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	theApp.ExitInstance();
}
