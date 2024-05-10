import numpy as np
import matplotlib.pyplot as plt

# Assuming you have two lists of data:
def plot_correlation(x, y, xlabel, ylabel, title, save=False):
    # prepare data
    correlation_matrix = np.corrcoef(x, y)
    correlation_xy = correlation_matrix[0,1]
    r_squared = correlation_xy**2

    # plot
    fig, ax = plt.subplots()
    ax.scatter(x, y)
    #draw line of best fit
    ax.plot(np.unique(x), np.poly1d(np.polyfit(x, y, 1))(np.unique(x)), color='red')
    ax.text(0.3, 0.9, f'R^2 = {r_squared}', horizontalalignment='center', verticalalignment='center', transform=ax.transAxes)

    if ylabel == 'Latency':
        plt.ylim(0, 40)

    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title)

    fig.tight_layout()
    if save:
        plt.savefig(f'{title}.png')
    else:
        plt.show()
