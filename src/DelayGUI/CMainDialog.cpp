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
	, m_data(model::make_model_data())
	, m_mode(MODE_DEMO)
{

}

CMainDialog::~CMainDialog()
{
}

void CMainDialog::DoDataExchange(CDataExchange* pDX)
{
	CSimulationDialog::DoDataExchange(pDX);
	DDX_Control(pDX, IDC_PLOT1, m_cSignalsPlot);
	DDX_Control(pDX, IDC_PLOT2, m_cSignalsShiftedPlot);
	DDX_Control(pDX, IDC_PLOT3, m_cResultsPlot);
	DDX_Control(pDX, IDC_PLOT4, m_cCorrelationPlot);
	DDX_Text(pDX, IDC_EDIT1, m_data.params->span);
	DDX_Text(pDX, IDC_EDIT2, m_data.params->shift);
	DDX_Text(pDX, IDC_EDIT3, m_data.params->carrier);
	DDX_Text(pDX, IDC_EDIT4, m_data.params->sampling_rate);
	DDX_Text(pDX, IDC_EDIT5, m_data.params->bitrate);
	DDX_Text(pDX, IDC_EDIT6, m_data.params->snr);
	DDX_Text(pDX, IDC_EDIT7, m_data.params->snr_from);
	DDX_Text(pDX, IDC_EDIT8, m_data.params->snr_to);
	DDX_Text(pDX, IDC_EDIT13, m_data.params->snr_count);
	DDX_Text(pDX, IDC_EDIT11, m_data.params->num_of_tests);
	DDX_Text(pDX, IDC_EDIT9, m_demoResults.shift_am);
	DDX_Text(pDX, IDC_EDIT10, m_demoResults.shift_pm);
	DDX_Text(pDX, IDC_EDIT12, m_demoResults.shift_fm);
}


BEGIN_MESSAGE_MAP(CMainDialog, CSimulationDialog)
	ON_BN_CLICKED(IDC_BUTTON3, &CMainDialog::OnBnClickedButton3)
	ON_BN_CLICKED(IDC_BUTTON1, &CMainDialog::OnBnClickedButton1)
	ON_BN_CLICKED(IDC_BUTTON2, &CMainDialog::OnBnClickedButton2)
END_MESSAGE_MAP()


// CMainDialog message handlers


BOOL CMainDialog::OnInitDialog()
{
	CSimulationDialog::OnInitDialog();

	m_cSignalsPlot.plot_layer.with(
		model::make_root_drawable(m_data.signals, { {
				m_data.signals.am.plot,
				m_data.signals.pm.plot,
				m_data.signals.fm.plot
		} })
	);
	m_cSignalsShiftedPlot.plot_layer.with(
		model::make_root_drawable(m_data.signals_shifted, { {
				m_data.signals_shifted.am.plot,
				m_data.signals_shifted.pm.plot,
				m_data.signals_shifted.fm.plot
		} })
	);
	m_cCorrelationPlot.plot_layer.with(
		model::make_root_drawable(m_data.correlation, { {
				m_data.correlation.am.plot,
				m_data.correlation.pm.plot,
				m_data.correlation.fm.plot
		} })
	);
	m_cResultsPlot.plot_layer.with(
		model::make_root_drawable(m_data.results, { {
				m_data.results.am.plot,
				m_data.results.pm.plot,
				m_data.results.fm.plot
		} })
	);

	m_cSignalsPlot.triple_buffered = true;
	m_cSignalsShiftedPlot.triple_buffered = true;
	m_cCorrelationPlot.triple_buffered = true;
	m_cResultsPlot.triple_buffered = true;

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
	if (m_mode == MODE_DEMO)
	{
		m_data.signals.autoworld->clear();
		m_data.signals.autoworld->adjust(*m_data.signals.am.data);
		m_data.signals.autoworld->adjust(*m_data.signals.pm.data);
		m_data.signals.autoworld->adjust(*m_data.signals.fm.data);
		m_data.signals.autoworld->flush();

		m_data.signals_shifted.autoworld->clear();
		m_data.signals_shifted.autoworld->adjust(*m_data.signals_shifted.am.data);
		m_data.signals_shifted.autoworld->adjust(*m_data.signals_shifted.pm.data);
		m_data.signals_shifted.autoworld->adjust(*m_data.signals_shifted.fm.data);
		m_data.signals_shifted.autoworld->flush();

		m_data.correlation.autoworld->clear();
		m_data.correlation.autoworld->adjust(*m_data.correlation.am.data);
		m_data.correlation.autoworld->adjust(*m_data.correlation.pm.data);
		m_data.correlation.autoworld->adjust(*m_data.correlation.fm.data);
		m_data.correlation.autoworld->flush();

		m_cSignalsPlot.RedrawBuffer(); m_cSignalsPlot.SwapBuffers();
		m_cSignalsPlot.RedrawWindow();
		m_cCorrelationPlot.RedrawBuffer(); m_cCorrelationPlot.SwapBuffers();
		m_cCorrelationPlot.RedrawWindow();
		m_cSignalsShiftedPlot.RedrawBuffer(); m_cSignalsShiftedPlot.SwapBuffers();
		m_cSignalsShiftedPlot.RedrawWindow();

		UpdateData(FALSE);
	}
	else
	{
		m_data.results.autoworld->clear();
		m_data.results.autoworld->adjust(*m_data.results.am.data);
		m_data.results.autoworld->adjust(*m_data.results.pm.data);
		m_data.results.autoworld->adjust(*m_data.results.fm.data);
		m_data.results.autoworld->flush();

		m_cResultsPlot.RedrawBuffer();
		m_cResultsPlot.SwapBuffers();
		Invoke([this]() {
			m_cResultsPlot.RedrawWindow();
		});
	}

	if (m_callbackCb != 0) m_callbackCb(HSDEV_POST_REFRESH, 0, 0);
}


void CMainDialog::OnBnClickedButton3()
{
	StopSimulationThread();
	UpdateData(TRUE);

	m_mode = MODE_DEMO;

	if (m_callbackCb != 0) m_callbackCb(HSDEV_ON_ACTION, HSDAC_DEMO_START, 0);
	if (m_callbackCb != 0) m_callbackCb(HSDEV_ON_ACTION, HSDAC_DEMO_STEP, 0);
	if (m_callbackCb != 0) m_callbackCb(HSDEV_ON_ACTION, HSDAC_DEMO_END, 0);
}


void CMainDialog::OnBnClickedButton1()
{
	StartSimulationThread();
}


void CMainDialog::OnBnClickedButton2()
{
	StopSimulationThread();
}


void CMainDialog::OnSimulation()
{
	Invoke([this]() {
		UpdateData(TRUE);
		m_mode = MODE_SIM;
		if (m_callbackCb != 0) m_callbackCb(HSDEV_ON_ACTION, HSDAC_SIM_START, 0);
	});

	while (m_bWorking) {
		Invoke([this]() {
			if (m_callbackCb != 0) m_callbackCb(HSDEV_ON_ACTION, HSDAC_SIM_STEP, 0);
		});
	}

	Invoke([this]() {
		if (m_callbackCb != 0) m_callbackCb(HSDEV_ON_ACTION, HSDAC_SIM_END, 0);
		m_mode = MODE_DEMO;
	});

	CSimulationDialog::OnSimulation();
}
