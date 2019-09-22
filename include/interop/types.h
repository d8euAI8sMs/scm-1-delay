#pragma once

#define HSD_ABI

#ifdef HSD_INTEROP_EXPORTS
#define HSD_API __declspec(dllexport) HSD_ABI
#else
#define HSD_API HSD_ABI
#endif

#ifdef __cplusplus
extern "C" {
#endif

	enum hsd_event {
		HSDEV_POST_INIT = 0,
		HSDEV_PRE_EXIT,
		HSDEV_PRE_SHOW,
		HSDEV_PRE_HIDE,
		HSDEV_POST_REFRESH,
	};

	typedef void HSD_ABI (*hsd_callback) (int evt, int param1, void *param2);



	void HSD_API hsd_init(void);

	void HSD_API hsd_show_and_wait(hsd_callback cb);

	void HSD_API hsd_refresh(void);

	void HSD_API hsd_exit(void);



#ifdef __cplusplus
}
#endif
