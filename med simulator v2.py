import fire
import numpy as np
import datetime
import matplotlib.pyplot as plt

"""
simple pharmacokinetic simulator.
"""


def pharmacokinetic_simulator(
        mg_per_pill=10,
        hour_taken=[9, 11, 12, 14],
        bioavailability=0.4,
        abs_half_life=2.5,
        elim_half_life=0.4,
        plot_hour_bounds=[7, 24],
        show_plot=True,
        ):
    """
    Compute pharmakinetic values and plot them.

    example usage:
    python ./this_script.py --hour_taken "[9, 13, 15]"

    Parameters
    ==========
    mg_per_pill: int, default 10
        amount of mg inside a pill
    hour_taken: list of numbers
        the hour when you took the pill. For examen [8.25, 14.5] if you
        took them at 8:15am and 2:30pm
    bioavailability: float, default 0.4
        expected bioavailability. Literature indicates 0.4 to be a plausible
        guess. Increase if you tend take the methylphenidate during the meal,
        especially if rich in fat.
    abs_half_life: float, default 2.5
        absorption half life in hours, approximated to be 2.5 from the
        literature
    elim_half_life: float, default 0.4
        elimination half life in hour, empirically determined to be 0.4.
        I think it is a coincidence that 1/2.5 = 0.4.
        Initially computed to comeup within 1h13 (about the average between
        1h (during meal) and 1h30 (on empty stomach)).
    plot_hour_bounds: list of numers, default [7, 24]
        a list with the first and last hour of the plot to display
    show_plot: bool or str, default True
        if True do plot and show it
        if False, don't do plot
        if string: do plot and save it to value
    """

    print("Checking arguments.")
    # argument check
    assert len([x
                for x in hour_taken if not isinstance(x, (float, int))
                ]) == 0, (
                        "You have to put numbers inside hour_taken!")
    assert isinstance(mg_per_pill, (int, float)), "mg_per_pill is not a number"
    assert isinstance(bioavailability, (int, float)), "bioabailability is not a number"
    assert isinstance(plot_hour_bounds, list), "plot_hour_bounds is not a list"
    assert len(plot_hour_bounds) == 2, "invalid length of plot_hour_bounds"
    assert len([x
                for x in plot_hour_bounds if not isinstance(x, int)
                ]) == 0, (
                        "You have to put ints inside plot_hour_bounds!")
    assert isinstance(show_plot, (bool, str)), "wrong type for show_plot"

    time_step = 0.1  # time subdivision
    hour_taken = sorted(hour_taken)  # make sure they are in order
    dose_per_pill = mg_per_pill * 0.4  # bioabailability=0.4
    total_amount = len(hour_taken) * mg_per_pill

    # create the time axis
    x = np.fromiter(
            (round(i * time_step, 1
                )
                for i in range(int(plot_hour_bounds[1] / time_step) + 1)
                ), dtype=float)

    # create the typical absorption curve values
    alpha = np.log(2) / abs_half_life
    beta = np.log(2) / elim_half_life
    absorption_exp = dose_per_pill * np.exp(-alpha * x)
    elimination_exp = dose_per_pill * np.exp(-beta * x)
    curve_template = absorption_exp - elimination_exp

    # make a long string of zero that will be summed with the offset curve
    # values
    y = np.fromiter((0 for i in x), dtype=float)
    for i, h in enumerate(hour_taken):
        limit = len(y) - int(h * 1 / time_step)
        y[int(h * 1/time_step):] += curve_template[:limit]

    # compute AUC elapsed
    now = datetime.datetime.now()
    hour = int(now.hour)
    minute = int(now.minute)
    time = hour+minute/60
    elapsed = round(sum(y[:int(time * 1 / time_step)]) / sum(y) * 100, 2)
    print(f"Elapsed portion: {elapsed}%")

    if show_plot is False:
        print("Not creating plot, finished.")

    # create plot
    y_max = 1.1 * max(y)
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(x, y, color='lightblue', linewidth=3)
    ax.set(xlim=[plot_hour_bounds[0], plot_hour_bounds[1]],
           ylim=[0, y_max])

    # axis
    ax.set(xlabel='Time',
           ylabel='Amount of medication (semi arbitrary unit)')
    xticks = []
    for i in range(0, 10):
        xticks.append(i)
    xticks.append(max(y))
    xticks.append(y[-1])

    for i in range(0, len(xticks)):
        xticks[i] = round(xticks[i], 3)
    ax.yaxis.set(ticks=xticks)

    # title
    plotTitle = (f"Pharmacokinetic Simulator \n({total_amount}mg "
                 f"in {len(hour_taken)} pills)\nElapsed: {elapsed}%")
    plotTitle = str(''.join(plotTitle))
    fig.suptitle(plotTitle)

    # vertical lines:
    # at midnight:
    plt.axhline(y=y[int(24 / time_step)], color="red", linestyle='--', linewidth=1)
    # at maximum:
    plt.axhline(y=max(y), color="red", linestyle='--', linewidth=1)
    # when taken:
    for i in range(0, len(hour_taken)):
        plt.axvline(x=hour_taken[i], linestyle=':', linewidth=1)
    # at current time:
    plt.axvline(
            x=time,
            color="purple",
            linewidth=2,
            linestyle=":",
            label="Now")

    ax.legend(loc='best')  # no overlapping elements
    if show_plot is True:
        print("Showing plot.")
        plt.show()
    else:
        print(f"Saving plot to {show_plot}")
        plt.savefig(show_plot + ".png")


if __name__ == "__main__":
    fire.Fire(pharmacokinetic_simulator)
    print("Finished.")
