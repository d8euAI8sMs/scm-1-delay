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

	// generic interface

	typedef enum {
		HSDEV_POST_INIT = 0,
		HSDEV_PRE_EXIT,
		HSDEV_PRE_SHOW,
		HSDEV_PRE_HIDE,
		HSDEV_POST_REFRESH,
		HSDEV_ON_ACTION,
	} hsd_event;

	typedef void HSD_ABI (*hsd_callback) (int evt, int param1, void *param2);



	void HSD_API hsd_init(void);

	void HSD_API hsd_show_and_wait(hsd_callback cb);

	void HSD_API hsd_refresh();

	void HSD_API hsd_exit(void);



	// task-specific interface

	typedef enum {
		HSDAC_DEMO_START = 0,
		HSDAC_DEMO_STEP,
		HSDAC_DEMO_END,
		HSDAC_SIM_START,
		HSDAC_SIM_STEP,
		HSDAC_SIM_END,
	} hsd_action;

	typedef enum {
		HSDMD_DEMO = 0,
		HSDMD_SIM,
	} hsd_mode;

	typedef struct {
		hsd_mode mode;
		double span;
		double shift;
		double carrier;
		double bitrate;
		double sampling_rate;
		double snr;
		double snr_from, snr_to;
		int snr_count;
		int num_of_tests;
	} params_t;

	typedef struct {
		double t;
		double x;
	} point_t;

	// in Haskell, there is no simple way to marshall nested structures
	// so we are limited in decomposition of `gen_data_t` on e.g. `signal_t`
	// fields -- they all must be pointers
	typedef struct {
		int count_base;
		int count_shifted;
		point_t* data_am_base;
		point_t* data_am_shifted;
		point_t* data_pm_base;
		point_t* data_pm_shifted;
		point_t* data_fm_base;
		point_t* data_fm_shifted;
	} gen_data_t;

	typedef struct {
		double snr;
		double prob_am;
		double prob_pm;
		double prob_fm;
	} sim_data_t;

	typedef struct {
		int correlation_count;
		point_t* correlation_am;
		point_t* correlation_pm;
		point_t* correlation_fm;
		double shift_am;
		double shift_pm;
		double shift_fm;
	} demo_data_t;

	void HSD_API hsd_get_params(params_t * data);

	void HSD_API hsd_set_signals(gen_data_t * data);

	void HSD_API hsd_set_cur_demo(demo_data_t * data);

	void HSD_API hsd_set_cur_sim(sim_data_t * data);

	void HSD_API hsd_stop_sim();

#ifdef __cplusplus
}
#endif
