#pragma once

#include <util/common/geom/point.h>
#include <util/common/math/vec.h>
#include <util/common/plot/plot.h>
#include <util/common/math/fuzzy.h>

#include <cstdint>
#include <vector>
#include <map>
#include <set>
#include <array>

#include <omp.h>

namespace model
{

    /*****************************************************/
    /*                     params                        */
    /*****************************************************/

	struct parameters
	{
		double span;
		double shift;
		double carrier;
		double bitrate;
		double sampling_rate;
		double snr;
		double snr_from, snr_to; int snr_count;
		int num_of_tests;
	};

	inline parameters make_default_parameters()
	{
		parameters p =
		{
			1,
			0.1,
			25000,
			9600,
			250000,
			-10,
			10, -10, 100,
			10
		};
		return p;
	}

    /*****************************************************/
    /*                     data                          */
    /*****************************************************/

    /*****************************************************/
    /*                     drawing                       */
    /*****************************************************/
	
    using points_t = std::vector < geom::point2d_t > ;

    struct plot_data
    {
        util::ptr_t < points_t > data;
        plot::list_drawable < points_t > :: ptr_t plot;
    };

	struct plot_group_data
	{
		plot::world_t::ptr_t world;
		plot::auto_viewport < points_t > ::ptr_t autoworld;
		plot_data am, pm, fm;
	};

    struct model_data
    {
        util::ptr_t < parameters > params;

        plot_group_data signals;
		plot_group_data signals_shifted;
		plot_group_data correlation;
		plot_group_data results;
    };

	struct demo_results
	{
		double shift_am, shift_pm, shift_fm;
	};

    inline static plot_data make_plot_data
    (
        plot::palette::pen_ptr pen = plot::palette::pen(0xffffff),
        plot::list_data_format data_format = plot::list_data_format::chain
    )
    {
        plot_data pd;
        pd.data = util::create < points_t > ();
        pd.plot = plot::list_drawable < points_t > :: create
        (
            plot::make_data_source(pd.data),
            nullptr, // no point painter
            pen
        );
        pd.plot->data_format = data_format;
        return pd;
    }

	inline static plot_group_data make_plot_group_data
	(
		plot::palette::pen_ptr am = plot::palette::pen(0xffffff),
		plot::palette::pen_ptr pm = plot::palette::pen(0x999999),
		plot::palette::pen_ptr fm = plot::palette::pen(0x555555),
		plot::list_data_format data_format = plot::list_data_format::chain
	)
	{
		plot_group_data pd;
		pd.world = plot::world_t::create();
		pd.autoworld = plot::min_max_auto_viewport < points_t > ::create();
		pd.am = make_plot_data(am, data_format);
		pd.pm = make_plot_data(pm, data_format);
		pd.fm = make_plot_data(fm, data_format);
		return pd;
	}

    inline static plot::drawable::ptr_t make_root_drawable
    (
        const plot_group_data & p,
        std::vector < plot::drawable::ptr_t > layers
    )
    {
        using namespace plot;

        return viewporter::create(
            tick_drawable::create(
                layer_drawable::create(layers),
                const_n_tick_factory<axe::x>::create(
                    make_simple_tick_formatter(6, 8),
                    0,
                    5
                ),
                const_n_tick_factory<axe::y>::create(
                    make_simple_tick_formatter(6, 8),
                    0,
                    5
                ),
                palette::pen(RGB(80, 80, 80)),
                RGB(200, 200, 200)
            ),
            make_viewport_mapper(make_world_mapper < points_t > (p.autoworld))
        );
    }

    inline model_data make_model_data(const parameters & p = make_default_parameters())
    {
        model_data md;
        md.params = util::create < parameters > (p);
        md.signals = make_plot_group_data();
		md.signals_shifted = make_plot_group_data();
		md.correlation = make_plot_group_data();
		md.results = make_plot_group_data();
        return md;
    }
}
