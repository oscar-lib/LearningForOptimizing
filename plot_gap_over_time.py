# creates a subplot of gap over time for each problem and each agent
import bisect
import os

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import re
from collections import defaultdict

from matplotlib.ticker import MaxNLocator

TIMEOUT = 900.0
SHOW_STD = False

plt.rcParams.update({
    'text.usetex': True,
    'font.family': 'serif',
    #'font.serif': ['Latin Modern Roman'],  # Replace with the specific font used in your beamer poster
    #'font.sans-serif': ['Latin Modern Sans'],
    'font.size': 10,  # in beamer
})


def cm_to_inch(cm):
    """
    for a more civilized age
    """
    return cm / 2.54


ORANGE = '#e69f00'
BLUE = '#0072b2'
BLUISH_GREEN = '#009e73'
VERMILION = '#d55e00'
REDDISH_PURPLE = '#cc79a7'
SKY_BLUE = '#56b4e9'
YELLOW = '#f0e442'
BLACK = '#000000'
WHITE = '#ffffff'
MY_COLORS = [
    ORANGE,
    BLUE,
    BLUISH_GREEN,
    VERMILION,
    REDDISH_PURPLE,
    SKY_BLUE,
    YELLOW,
    BLACK,
    WHITE
]

COLORS_METHOD = {
    "ucb-r1": ORANGE,
    "ucb-r2": ORANGE,
    "epsilongreedy-r1": SKY_BLUE,
    "epsilongreedy-r2": SKY_BLUE,
    "bestslopefirst": BLACK,
    "random": BLUISH_GREEN,
    "roundrobin": REDDISH_PURPLE
}

METHOD_PRETTY_NAMES = {
    "ucb-r1": "UCB R1",
    "ucb-r2": "UCB R2",
    "epsilongreedy-r1": r"$\epsilon$-Greedy R1",
    "epsilongreedy-r2": r"$\epsilon$-Greedy R2",
    "random": "Random",
    "roundrobin": "RoundRobin",
    "bestslopefirst": "BSF",
}


trivial_upper_bounds = {}


class InstanceData:

    def __init__(self, instance: str, method: str, solution_list: list[(float, int, float)]):
        self.instance = instance
        self.method = method
        self.solution_list = solution_list
        if len(solution_list) > 0:
            first_sol_val = solution_list[0][2]
            if instance not in trivial_upper_bounds:
                trivial_upper_bounds[instance] = first_sol_val
            else:
                trivial_upper_bounds[instance] = max(first_sol_val, trivial_upper_bounds[instance])
        self.times, self.iterations, self.percentages = self._percentage_objective_over_time()

    def _percentage_objective_over_time(self) -> tuple[list[float], list[int], list[float]]:
        percentages = []
        iterations = []
        times = []
        best = obj_best[self.instance]
        upper_bound = trivial_upper_bounds[self.instance]
        use_upper_bound = False #"csp" in self.instance or "carseq" in self.instance
        for sol in self.solution_list:
            if sol[2] == best:
                percentage = 0.0
            else:
                if use_upper_bound:
                    percentage = 100.0 - 100.0 * (upper_bound - sol[2]) / (upper_bound - best)
                else:
                    percentage = 100.0 * abs(sol[2] - best) / max(abs(sol[2]), abs(best))
            percentage = max(0.0, min(percentage, 100.0))
            if len(percentages) == 0 or percentage < percentages[-1]:
                times.append(sol[0])
                iterations.append(sol[1])
                percentages.append(percentage)
        return times, iterations, percentages

    def percentage_at_time(self, t: float) -> float:
        if t >= self.times[-1]:
            return self.percentages[-1]
        """
        idx_right = bisect.bisect_right(self.solution_time_list, time_limit)
        for sol in self.solution_list:
            if sol.time <= time_limit:
                best_sol = sol
            else:
                break
        return best_sol
        """
        idx_left = bisect.bisect_left(self.times, t)
        if idx_left < len(self.percentages):
            return self.percentages[idx_left]
        else:
            raise Exception("invalid")

    def __str__(self):
        return self.solution_list

    def __repr__(self):
        return f"{self.method}-{str(self.solution_list)}"


def average_instance_data(instance_data_list: list[InstanceData]) -> InstanceData:
    """
    Combines several instance data to a single one, by computing the average
    :param instance_data_list:
    :return:
    """
    if len(instance_data_list) == 1:
        return instance_data_list[0]
    instance = instance_data_list[0].instance
    method = instance_data_list[0].method
    solution_list = []
    # retrieve all times for all solutions and average them
    times = set()
    for results in instance_data_list:
        for time in results.times:
            times.add(time)
    times = sorted(list(times))
    solution_list = []
    avg_times = []
    avg_iterations = []
    avg_percentages = []
    n = len(instance_data_list)
    for t in times:
        percentage = sum([result.percentage_at_time(t) for result in instance_data_list]) / n
        avg_times.append(t)
        avg_percentages.append(percentage)
    aggregated = InstanceData(instance, method, solution_list)
    aggregated.times = avg_times
    aggregated.iterations = avg_iterations
    aggregated.percentages = avg_percentages
    return aggregated

problem_list = ["tsp", "csp", "pdptw"]

# ICORES template: one column = 7.5cm , 2 columns + margin = 15.8cm

figsize = (cm_to_inch(15.8), cm_to_inch(4))
fig, axs = plt.subplots(1, 3, sharex=True, figsize=figsize)

for axi, problem in enumerate(problem_list):
    if problem == "tsp":
        filename_list = ["results/tsp_baseline.csv", "results/tsp_bandits.csv"]
        colnames = ["instance", "bandit", "reward", "timeout", "unroutedNodes", "travelLength", "solutions", "integralPrimalGap"]
    elif problem == "csp":
        filename_list = ["results/csp_baseline.csv", "results/csp_bandits.csv"]
        colnames = ["instance", "bandit", "reward", "timeout", "best", "solutions"]
    elif problem == "pdptw":
        filename_list = ["results/pdptw_baseline.csv", "results/pdptw_bandits.csv"]
        colnames = ["instance", "bandit", "reward", "timeout", "unroutedNodes", "nVehicles", "travelLength", "objective", "integralPrimalGap", "solutions"]
    else:
        raise Exception(f"problem {problem} not recognized")
    df_list = []
    for filename in filename_list:
        new_df = pd.read_csv(filename, names=colnames, header=None)
        df_list.append(new_df)
    df = pd.concat(df_list)
    df["instance"] = df["instance"].apply(lambda name: "/".join(name.split("/")[-2:]).removesuffix(".txt"))
    instance_set = set(df["instance"])

    # for each instance, its worst and best objective value
    obj_best = {instance: float("inf") for instance in instance_set}
    obj_worst = {instance: -1.0 for instance in instance_set}
    data_per_method_full = {}
    methods = set()
    pattern_sol = r"t:(\d+\.\d+)-t:(\d+)-v:(\d+\.\d+)"
    for i, row in df.iterrows():
        instance = str(row["instance"])
        if row["bandit"] not in ["bestslopefirst", "random", "roundrobin"]:
            method_name = f"{row['bandit']}-{row['reward']}"
        else:
            method_name = f"{row['bandit']}"
        methods.add(method_name)
        solutions_split = str(row["solutions"]).removesuffix(")]").removeprefix("[(").split(")-(")
        solutions_split = [
            (float(re.search(pattern_sol, s).group(1)),
            int(re.search(pattern_sol, s).group(2)),
            float(re.search(pattern_sol, s).group(3)))
            for s in solutions_split
        ]
        worst = solutions_split[0][-1]
        best = solutions_split[-1][-1]
        obj_worst[instance] = max(obj_worst[instance], best)
        obj_best[instance] = min(obj_best[instance], best)
        if method_name not in data_per_method_full:
            data_per_method_full[method_name] = {}
        instance_data = InstanceData(instance, method_name, solutions_split)
        if instance not in data_per_method_full[method_name]:
            data_per_method_full[method_name][instance] = []
        data_per_method_full[method_name][instance].append(instance_data)

    # average the X runs to get only one
    data_per_method = {}
    for method in data_per_method_full:
        data_per_method[method] = {}
        for instance in data_per_method_full[method]:
            value = average_instance_data(data_per_method_full[method][instance])
            data_per_method[method][instance] = value

    # for each method, tracks the time at which the objective changed on each instance
    methods_values = {}
    for method in methods:
        current_values = {instance: 100.0 for instance in instance_set}
        data_update_at_time = {TIMEOUT: []}
        for instance in instance_set:
            data = data_per_method[method][instance]
            time_point = data.times
            for t in time_point:
                if t not in data_update_at_time:
                    data_update_at_time[t] = []
                data_update_at_time[t].append(data)
        all_time_points = [0.0] + list(sorted(data_update_at_time.keys())) + [TIMEOUT]

        n = len(instance_set)
        average_list = [100.0 * n]
        std_list = [0.0]
        for t in all_time_points[1:]:  # ignoring time t=0: it's always 100%
            to_update = data_update_at_time[t]
            average = average_list[-1]  # get previous average
            delta = 0.0
            for update_data in to_update:
                old_value = current_values[update_data.instance]
                new_value = update_data.percentage_at_time(t)
                assert new_value >= 0.0
                if new_value > old_value:
                    #print(f"WARNING: old value = {old_value}")
                    #print(f"WARNING: new value = {new_value}")
                    pass
                #assert new_value <= old_value
                delta = delta - old_value + new_value
                current_values[update_data.instance] = new_value
            # average over the number of update points performed
            #assert delta <= 0.0
            average = average + delta
            v = np.asarray(list(current_values.values()))
            new_std = np.std(v)
            std_list.append(new_std)
            average_list.append(average)


        average_list = [avg / n for avg in average_list]
        #std_list = [4.0 for _ in average_list]
        methods_values[method] = [all_time_points, average_list, std_list]

    # sort methods by final reading
    final_readings = {method: methods_values[method][1][-1] for method in methods}
    final_readings_sorted = list(sorted(final_readings.values()))
    sorted_methods = sorted(list(methods), key=lambda m: methods_values[m][1][-1], reverse=True)

    #fig = plt.figure(figsize=(cm_to_inch(8), cm_to_inch(8)))
    ax = axs[axi]
    ax.set_title(problem.upper())
    for method in sorted_methods:
        x, y, std = methods_values[method]
        if "r1" in method:
            linestyle = "dotted"
        else:
            linestyle = "solid"
        label = METHOD_PRETTY_NAMES.get(method, method)

        ax.plot(x, y, label=label, linestyle=linestyle, color=COLORS_METHOD.get(method, None))
        if SHOW_STD:
            print(f"{problem}: {method}: std = [{min(std):.2f} ... {max(std):.2f}]")
            y_array = np.array(y)
            y_std = np.array(std)
            ax.fill_between(x, y - y_std, y + y_std, alpha=0.3, color=COLORS_METHOD.get(method, None))

    ax.set_xlabel("Time (s)")
    if axi == 0:
        ax.set_ylabel(r"Average primal gap (\%)")
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.set_xlim([0, TIMEOUT])
    ax.xaxis.set_major_locator(MaxNLocator(nbins=4))  # n well-spaced ticks

    zoom = True
    if zoom:
        delta_y_scale = 4
        n_best = 10
        y_lim_low = max(final_readings_sorted[0] - delta_y_scale, 0.0)
        y_lim_high = final_readings_sorted[min(n_best, len(final_readings_sorted)-1)] + delta_y_scale
        ax.set_ylim(y_lim_low, y_lim_high)
        # Shrink current axis by 75% to put legend box there
        #box = ax.get_position()
        #ax.set_position([box.x0, box.y0 + box.height * 0.08, box.width, box.height * 0.67])
    #if axi == 1:
    #    ax.legend(loc='center', bbox_to_anchor=(0.3, 1.3))
    ax.grid(axis="y")
ax = axs[1]
handles, labels = ax.get_legend_handles_labels()
leg = fig.legend(handles, labels,
                 bbox_to_anchor=(0.5, 1.35), loc='upper center',
                 ncol=4, handlelength=1.5, columnspacing=1.3, handletextpad=0.6)

plot_filename = f"subplot-gap-over-time.pdf"
fig.savefig(plot_filename,
            bbox_inches='tight',
            bbox_extra_artists=(leg,),   # helps some backends include the legend
            pad_inches=0.05)
print(f"figure saved to {plot_filename}")
