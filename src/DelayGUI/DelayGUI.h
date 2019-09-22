// DelayGUI.h : main header file for the DelayGUI DLL
//

#pragma once

#include <interop\types.h>

#ifndef __AFXWIN_H__
	#error "include 'pch.h' before including this file for PCH"
#endif

#include "resource.h"		// main symbols


// CDelayGUIApp
// See DelayGUI.cpp for the implementation of this class
//

class CDelayGUIApp : public CWinApp
{
public:
	CDelayGUIApp();

// Overrides
public:
	virtual BOOL InitInstance();
	BOOL m_bInitialized;
	hsd_callback m_callbackCb;

	DECLARE_MESSAGE_MAP()
};
