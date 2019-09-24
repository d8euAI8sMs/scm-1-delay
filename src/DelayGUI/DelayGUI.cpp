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
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
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
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	static_cast<CMainDialog*>(theApp.GetMainWnd())->Refresh();
}

void hsd_exit() {
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	theApp.ExitInstance();
}

void _set_data_range(model::points_t& pts, int count, point_t* s) {
	pts.resize(static_cast <size_t> (count));
	for (size_t i = 0; i < static_cast <size_t> (count); ++i) {
		pts[i] = { s[i].t, s[i].x };
	}
}

void hsd_get_params(params_t* data) {
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	auto wnd = static_cast<CMainDialog*>(theApp.GetMainWnd());
	auto& sig = wnd->m_data;
	data->mode				= (wnd->m_mode == CMainDialog::MODE_DEMO) ? HSDMD_DEMO : HSDMD_SIM;
	data->span				= sig.params->span;
	data->shift				= sig.params->shift;
	data->carrier			= sig.params->carrier;
	data->bitrate			= sig.params->bitrate;
	data->sampling_rate		= sig.params->sampling_rate;
	data->snr				= sig.params->snr;
	data->snr_from			= sig.params->snr_from;
	data->snr_to			= sig.params->snr_to;
	data->snr_count			= sig.params->snr_count;
	data->num_of_tests		= sig.params->num_of_tests;
}

void hsd_set_signals(gen_data_t * data) {
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	auto & sig = static_cast<CMainDialog*>(theApp.GetMainWnd())->m_data;
	_set_data_range(*sig.signals.am.data, data->count_base, data->data_am_base);
	_set_data_range(*sig.signals.pm.data, data->count_base, data->data_pm_base);
	_set_data_range(*sig.signals.fm.data, data->count_base, data->data_fm_base);
	_set_data_range(*sig.signals_shifted.am.data, data->count_shifted, data->data_am_shifted);
	_set_data_range(*sig.signals_shifted.pm.data, data->count_shifted, data->data_pm_shifted);
	_set_data_range(*sig.signals_shifted.fm.data, data->count_shifted, data->data_fm_shifted);
}

void hsd_set_cur_demo(demo_data_t* data) {
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	auto& sig = static_cast<CMainDialog*>(theApp.GetMainWnd())->m_data;
	_set_data_range(*sig.correlation.am.data, data->correlation_count, data->correlation_am);
	_set_data_range(*sig.correlation.pm.data, data->correlation_count, data->correlation_pm);
	_set_data_range(*sig.correlation.fm.data, data->correlation_count, data->correlation_fm);
	auto& res = static_cast<CMainDialog*>(theApp.GetMainWnd())->m_demoResults;
	res.shift_am = data->shift_am;
	res.shift_pm = data->shift_pm;
	res.shift_fm = data->shift_fm;
}

void hsd_set_cur_sim(sim_data_t* data) {
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	auto& sig = static_cast<CMainDialog*>(theApp.GetMainWnd())->m_data;
	sig.results.am.data->emplace_back(data->snr, data->prob_am);
	sig.results.pm.data->emplace_back(data->snr, data->prob_pm);
	sig.results.fm.data->emplace_back(data->snr, data->prob_fm);
}

void hsd_stop_sim() {
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	static_cast<CMainDialog*>(theApp.GetMainWnd())->StopSimulationThread();
}
