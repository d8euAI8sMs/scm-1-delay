#pragma once

#include <util\common\gui\SimulationDialog.h>
#include <util\common\gui\PlotControl.h>

#include <interop\types.h>

#include "model.h"

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

	enum {
		MODE_DEMO,
		MODE_SIM
	} m_mode;

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()
public:
	virtual BOOL OnInitDialog();
	virtual BOOL DestroyWindow();
	virtual void OnSimulation();
	void Refresh();
public:
	CPlotControl m_cSignalsPlot;
	CPlotControl m_cSignalsShiftedPlot;
	CPlotControl m_cCorrelationPlot;
	CPlotControl m_cResultsPlot;
	model::model_data m_data;
	model::demo_results m_demoResults;
	afx_msg void OnBnClickedButton3();
	afx_msg void OnBnClickedButton1();
	afx_msg void OnBnClickedButton2();
};
