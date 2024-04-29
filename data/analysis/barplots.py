import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker

def default_axis_config(ax, limit_y=True):
    if limit_y:
        ax.set_ylim(-.05, 1.1)
        yticks = np.arange(0, 1.2, 0.2)
        ax.set_yticks(yticks)
        ax.set_yticklabels(['{:.1f}'.format(y) for y in yticks])
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    ax.tick_params(axis='x', which='both', bottom=False, top=False)

def bar_plot(categories, identification):
    """
        categories : [(name, hit_rate, color), ...]
        identification : [(name, data), ...]
    """
    names, hit_rates, colors = zip(*categories)
    id_names, id_data = zip(*identification)

    fig, ax = plt.subplots(figsize=(10, 5))
    default_axis_config(ax)
    ax.bar(names, hit_rates, color=colors)

    # add identification into the plot
    ax.text(0.5, 1.1, f'{id_names[0]} {id_data[0]}', horizontalalignment='center', verticalalignment='center', transform=ax.transAxes)
    ax.text(0.5, 1.05, f'{id_names[1]} {id_data[1]}', horizontalalignment='center', verticalalignment='center', transform=ax.transAxes)
    ax.text(0.5, 1.0, f'{id_names[2]} {id_data[2]}', horizontalalignment='center', verticalalignment='center', transform=ax.transAxes)

    formatter = ticker.FormatStrFormatter('%.1f')
    plt.gca().yaxis.set_major_formatter(formatter)

    plt.xlabel('Category')
    plt.ylabel('Hit Rate')
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    plt.show()

def bar_subplots(container, save=False):
    """
    container = [{categories : [(name, hit_rate, color), ...], identification : [(name, data), ...]} ]
    """
    # decide how many columns and rows we need using the contaner length
    number_of_plots = len(container)
    columns = 2
    rows = number_of_plots // columns + 1

    fig, axs = plt.subplots(rows, columns, figsize=(5, 10), sharex=True, sharey=True)
    axs = axs.flatten()

    n = number_of_plots
    bottom_plots = [n - 2, n - 1] if n % 2 == 0 else [n - 1]


    for ax in axs[number_of_plots:]:
        fig.delaxes(ax)

    for i, (data, ax) in enumerate(zip(container, axs[:number_of_plots])):
        categories = data['categories']
        identification = data['identification']
        names, hit_rates, colors = zip(*categories)
        id_names, id_data = zip(*identification)

        # ax = axs[i // columns, i % columns]
        default_axis_config(ax)
        ax.bar(names, hit_rates, color=colors)
        ax.set_title(f'{id_names[0]} {id_data[0]}\n{id_names[1]} {id_data[1]}\n{id_names[2]} {id_data[2]}', fontsize=8)

        formatter = ticker.FormatStrFormatter('%.1f')
        ax.yaxis.set_major_formatter(formatter)

        ax.set_xticklabels(names, rotation=45, ha='right')
        if i in bottom_plots:
            ax.tick_params(axis='x', labelbottom=True)


    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()

    if save:
        filename = id_data[0]+'.pdf'
        plt.savefig(filename)
    else:
        plt.show()

def box_plot(container, save=False, include_names=[]):
    """
    container = [{categories : [(name, hit_rate, color), ...], identification : [(name, data), ...]} ]
    """
    fig, ax = plt.subplots(figsize=(5, 5))
    default_axis_config(ax)

    # group all hit_rates by name
    grouped_hit_rates = {}
    for data in container:
        # we don't need identification for grouped data
        # identification = data['identification']
        categories = data['categories']
        names, hit_rates, colors = zip(*categories)
        for name, hit_rate in zip(names, hit_rates):
            if name not in include_names:
                continue
            if hit_rate is None:
                continue

            if name in grouped_hit_rates:
                grouped_hit_rates[name].append(hit_rate)
            else:
                grouped_hit_rates[name] = [hit_rate]

    ax.text(0.2, 1.2, "Teaching words\nnibo, fale ...",
            horizontalalignment='center',
            verticalalignment='center',
            transform=ax.transAxes,
            weight='bold')

    ax.text(0.75, 1.2, "Recombinative words\nfalo, bena",
            horizontalalignment='center',
            verticalalignment='center',
            transform=ax.transAxes,
            weight='bold')

    # plot hit_rates grouped by name
    ax.boxplot(grouped_hit_rates.values(), showmeans=True)
    ax.set_xticklabels(grouped_hit_rates.keys())

    # give a name to y axis
    ax.set_ylabel('Hit proportion')

    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()

    if save:
        filename = 'Fig2_BC_CB.pdf'
        plt.savefig(filename)
    else:
        plt.show()

def dispersion_plot_per_cycle(container, save=False, style='boxplot', include_names=[], append_to_filename=''):
    """
    container = [{
        categories : [(name, hit_rate, color), ...],
        identification : [(name, date, duration, cycle), ...]} ]
    """
    # group all hit_rates by name
    grouped_hit_rates = {}
    max_cycle = 0
    for data in container:
        identification = data['identification']
        categories = data['categories']
        # we need to group hit_rates per name then per cycle
        # cycle is the last element in identification tuple
        id_names, id_data = zip(*identification)
        max_cycle = max(max_cycle, int(id_data[3]))
        cycle = id_data[3]
        if cycle == '0':
            continue
        cycle = 'Cycle ' + cycle
        names, hit_rates, colors = zip(*categories)

        for name, hit_rate in zip(names, hit_rates):
            if name not in include_names:
                continue

            if hit_rate is None:
                continue

            if cycle in grouped_hit_rates:
                if name in grouped_hit_rates[cycle]:
                    if isinstance(hit_rate, (int, float)):
                        hit_rate = [hit_rate]
                    grouped_hit_rates[cycle][name].extend(hit_rate)
                else:
                    grouped_hit_rates[cycle][name] = [hit_rate]
            else:
                grouped_hit_rates[cycle] = {name: [hit_rate]}

    # decide how many columns and rows we need using the contaner length
    number_of_plots = max_cycle
    columns = 2
    rows = number_of_plots // columns + 1

    # set height based on the number of rows
    height = 2 * rows
    fig, axs = plt.subplots(rows, columns, figsize=(4.5, height), sharex=True, sharey=True)
    axs = axs.flatten()

    n = number_of_plots
    bottom_plots = [n - 2, n - 1] if n % 2 == 0 else [n - 1]

    for ax in axs[number_of_plots:]:
        fig.delaxes(ax)

    for i, (cycle, names) in enumerate(grouped_hit_rates.items()):
    # for i, (data, ax) in enumerate(zip(container, axs[:number_of_plots])):
        ax = axs[i % number_of_plots]
        default_axis_config(ax)
        ax.text(1.0, 0.5, cycle,
                horizontalalignment='center',
                verticalalignment='center',
                transform=ax.transAxes,
                weight='bold')

        if style == 'boxplot':
            ax.boxplot(grouped_hit_rates[cycle].values(), showmeans=True)
            xticks = list(grouped_hit_rates[cycle].keys()) * (i+1)
            ax.set_xticks(range(len(xticks)))
            ax.set_xticklabels(xticks, rotation=45, ha='right')
        elif style == 'scatter':
            for name, hit_rates in grouped_hit_rates[cycle].items():
                ax.scatter([name]*len(hit_rates), hit_rates, alpha=0.5)
            ax.set_xticks(range(len(grouped_hit_rates[cycle])))
            ax.set_xticklabels(grouped_hit_rates[cycle].keys(), rotation=45, ha='right')
            plt.xticks(rotation=45, ha='right')

        if i in bottom_plots:
            ax.tick_params(axis='x', labelbottom=True)

    # for i, (cycle, names) in enumerate(grouped_hit_rates.items()):
    #     ax = axs[i // 2, i % 2]
    #     default_axis_config(ax)
    #     ax.text(1.0, 0.5, cycle,
    #             horizontalalignment='center',
    #             verticalalignment='center',
    #             transform=ax.transAxes,
    #             weight='bold')

    #     if style == 'boxplot':
    #         ax.boxplot(names.values(), showmeans=True)
    #         # create a list of strings with names.keys() repeated i times
    #         # this will be used as xticks
    #         xticks = list(names.keys()) * (i+1)
    #         ax.set_xticks(range(len(xticks)))
    #         ax.set_xticklabels(xticks, rotation=45, ha='right')
    #     elif style == 'scatter':
    #         for name, hit_rates in names.items():
    #             ax.scatter([name]*len(hit_rates), hit_rates, alpha=0.5)
    #         ax.set_xticks(range(len(names)))
    #         ax.set_xticklabels(names.keys(), rotation=45, ha='right')
    #         plt.xticks(rotation=45, ha='right')

    # give a name to y axis
    axs[0].set_ylabel('Hit proportion')
    plt.tight_layout()

    if save:
        filename = 'Fig_per_cycle'+append_to_filename+'.pdf'
        plt.savefig(filename)
    else:
        plt.show()

def barplot_per_cycle(container, save=False, include_names=[], append_to_filename=''):
    """
    container = [{
        categories : [(name, hit_rate, color), ...],
        identification : [(name, date, duration, cycle), ...]} ]
    """
    # one bar per cycle inside one single plot
    grouped_hit_rates = {}
    max_cycle = 0

    for data in container:
        identification = data['identification']
        categories = data['categories']
        id_names, id_data = zip(*identification)
        max_cycle = max(max_cycle, int(id_data[3]))
        cycle = id_data[3]
        if cycle == '0':
            continue
        cycle = 'Cycle ' + cycle
        names, hit_rates, colors = zip(*categories)

        for name, hit_rate in zip(names, hit_rates):
            if name not in include_names:
                continue

            if hit_rate is None:
                continue

            if cycle in grouped_hit_rates:
                if name in grouped_hit_rates[cycle]:
                    if isinstance(hit_rate, (int, float)):
                        hit_rate = [hit_rate]
                    grouped_hit_rates[cycle][name].extend(hit_rate)
                else:
                    grouped_hit_rates[cycle][name] = [hit_rate]
            else:
                grouped_hit_rates[cycle] = {name: [hit_rate]}

    unique_names = list(include_names)
    unique_cycles = sorted(grouped_hit_rates.keys(), key=lambda x: int(x.split(' ')[1]))
    colors = ['b', 'g', 'r', 'c', 'm', 'y', 'k']

    bar_width = 0.2
    r = np.arange(len(unique_cycles))
    fig, ax = plt.subplots(figsize=(4, 4))
    default_axis_config(ax)
    # Create a bar for each name. Group by cycle.
    for i, name in enumerate(unique_names):
        hit_rates = [np.mean(grouped_hit_rates[cycle][name]) if name in grouped_hit_rates[cycle] else 0 for cycle in unique_cycles]
        ax.bar(r + i * bar_width, hit_rates, color=colors[i % len(colors)], width=bar_width, edgecolor='grey', label=name)
    ax.legend()

    # give a name to y axis
    ax.set_ylabel('Hit proportion')
    ax.set_xlabel('Cycles')

    # set xticks from 1 to len(unique_cycles), as integers starting from 1
    plt.xticks([r + bar_width for r in range(len(unique_cycles))], [i+1 for i in range(len(unique_cycles))])

    plt.tight_layout()

    if save:
        filename = 'Fig_per_cycle'+append_to_filename+'.pdf'
        plt.savefig(filename)
    else:
        plt.show()