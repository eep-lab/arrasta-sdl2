import scipy.stats as stats
from sklearn.utils import resample
import statsmodels.api as sm
import matplotlib.pyplot as plt
import numpy as np
from statsmodels.sandbox.stats.runs import mcnemar
from scipy.stats import norm

def Kruskal_Wallis(container):
    """
    grouped_hit_rates[cycle][name]
    """
    values = []
    for name in container:
        values.append(container[name]*100)

    H, p = stats.kruskal(*values)

    if p < 0.001:
        print("H =", H, ", p < 0.001")
    elif p < 0.01:
        print("H =", H, ", p < 0.01")
    elif p < 0.05:
        print("H =", H, ", p < 0.05")
    else:
        print("H =", H, ", p =", p)

def Mann_Whitney_U(container):
    """
    grouped_hit_rates[cycle][name]
    """
    AB, AC, CD = [container[name] * 100 for name in container]

    p_criteria = 0.001

    U, p = stats.mannwhitneyu(AB, AC)
    if p < p_criteria:
        print("AB vs AC: U =", U, ", p < 0.001")
    elif p < p_criteria*10:
        print("AB vs AC: U =", U, ", p < 0.01")
    elif p < p_criteria*50:
        print("AB vs AC: U =", U, ", p < 0.05")
    else:
        print("AB vs AC: U =", U, ", p =", p)

    U, p = stats.mannwhitneyu(AB, CD)
    if p < p_criteria:
        print("AB vs CD: U =", U, ", p < 0.001")
    elif p < p_criteria*10:
        print("AB vs CD: U =", U, ", p < 0.01")
    elif p < p_criteria*50:
        print("AB vs CD: U =", U, ", p < 0.05")
    else:
        print("AB vs CD: U =", U, ", p =", p)

    U, p = stats.mannwhitneyu(AC, CD)
    if p < p_criteria:
        print("AC vs CD: U =", U, ", p < 0.001")
    elif p < p_criteria*10:
        print("AC vs CD: U =", U, ", p < 0.01")
    elif p < p_criteria*50:
        print("AC vs CD: U =", U, ", p < 0.05")
    else:
        print("AC vs CD: U =", U, ", p =", p)

def mcnemar_exact_test_confidence_interval(groupA, groupB):
    """
    McNemar exact test with confidence interval for binary data.
    Recommended by a decision map of statistical tests for binary data (Sauro & Lewis, 2012).

    McNemar mid-p and McNemar asymptotic tests are more powerful than the exact test when
    discordant pairs are less than 25.

    Source: https://measuringu.com/mcnemar-exact-test/
    """
    a = np.sum((groupA == 1) & (groupB == 1))
    b = np.sum((groupA == 1) & (groupB == 0))
    c = np.sum((groupA == 0) & (groupB == 1))
    d = np.sum((groupA == 0) & (groupB == 0))
    # Calculate z_alpha/2 for the specified confidence level

    z_alpha = norm.ppf(0.975)
    z_alpha_squared = z_alpha ** 2

    # Adjust the cells of the 2x2 table
    adjustment = z_alpha_squared / 8
    a_adj = a + adjustment
    b_adj = b + adjustment
    c_adj = c + adjustment
    d_adj = d + adjustment

    # Calculate the adjusted proportions
    N_adj = a_adj + b_adj + c_adj + d_adj
    p_12_adj = b_adj / N_adj
    p_21_adj = c_adj / N_adj

    # Calculate the difference in adjusted proportions
    diff_proportions = p_12_adj - p_21_adj

    # Calculate the standard error
    standard_error = np.sqrt((p_12_adj + p_21_adj - (p_12_adj - p_21_adj) ** 2) / N_adj)

    # Calculate the confidence interval
    margin_of_error = z_alpha * standard_error
    lower_bound = diff_proportions - margin_of_error
    upper_bound = diff_proportions + margin_of_error

    return diff_proportions, lower_bound, upper_bound

def paired_chi_squared(hits_trials_per_cycle_per_phase):
    """
    Perform a paired Chi-Square test for each cycle
    34 participants, 1 measure (hits, trials) for each of the 3 teaching phases

    # hits_trials_per_cycle_per_phase[participant][cycle][name]
    # we need data formated per cycle = data
    # each row is a participant, each column is a name

    data = np.array([
       # AB    AC    CD
       # X1 N1 X2 N2 X3 N3
        [60 60 49 51  8  8]
        [70 72 81 92  8  8]
        [60 60 46 46  8  8]
        [60 60 58 58 14 16]
        [82 84 46 46  8  8]
        [60 60 46 46  8  8]
        # ...
        [60 60 46 46  8  8])
    """
    # format the data
    cycles_data = []
    cycles = list(hits_trials_per_cycle_per_phase.values())[0].keys()

    for cycle in cycles:
        cycle_data = []
        for participant in hits_trials_per_cycle_per_phase:
            phases = []
            for name in hits_trials_per_cycle_per_phase[participant][cycle]:
                values = hits_trials_per_cycle_per_phase[participant][cycle][name]
                # we assume daily sessions, and we may have more than one day for each cycle
                # Participant "AND" is an expection with 2 days in cycle 1
                # so we take the first day here and ignore the rest since all other participants have only day one
                if len(values) > 1:
                    values = [values[0]]

                # remove extra encapulation
                values = values[0]
                phases.append(values)
            cycle_data.append(phases)
        # get the number of participants in cycle_data
        cycles_data.append((cycle, np.array(cycle_data).reshape(len(cycle_data), 6)))

    # global Chi-Square test
    for (cycle, observed) in cycles_data:
        # print(cycle, observed)

        row_totals = observed.sum(axis=1)
        col_totals = observed.sum(axis=0)
        total = observed.sum()
        expected = np.outer(row_totals, col_totals) / total

        chi2_stat = ((observed - expected)**2 / expected).sum()

        num_rows = observed.shape[0]  # number of participants
        num_columns = observed.shape[1]/2  # number of pairs
        df = (num_rows - 1) * (num_columns - 1)

        p_value = stats.chi2.sf(chi2_stat, df)

        # print pretty Chi-Square statistics
        print('Raw hits and trials: AB, AC, CD')
        print(f"{cycle}")
        print(f"Chi-Square statistic: {chi2_stat}")
        print(f"Degrees of freedom: {df}")
        print(f"p-value: {p_value}")

        # # Calculate the proportions for each pair
        # prop_AB = observed[:, 0] / observed[:, 1]
        # prop_AC = observed[:, 2] / observed[:, 3]
        # prop_CD = observed[:, 4] / observed[:, 5]

        # # Convert proportions to binary outcomes (success or failure)
        # success_AB = np.where(prop_AB >= 0.98, 1, 0)
        # success_AC = np.where(prop_AC >= 0.98, 1, 0)
        # success_CD = np.where(prop_CD >= 0.98, 1, 0)

        # print('Proportions: AB, AC, CD')
        # print(f"AB: {success_AB}")
        # print(f"AC: {success_AC}")
        # print(f"CD: {success_CD}")

        # # Count discordant pairs
        # discordant_pairs_AB_AC = np.sum(success_AB != success_AC)
        # discordant_pairs_AB_CD = np.sum(success_AB != success_CD)
        # discordant_pairs_AC_CD = np.sum(success_AC != success_CD)

        # warning = "Warning: The number of discordant pairs is less than 25."
        # print('Discordant pairs: AB vs AC, AB vs CD, AC vs CD')
        # print(f"AB_AC:{discordant_pairs_AB_AC}")
        # if discordant_pairs_AB_AC < 25:
        #     print("AB_AC:"+ warning)

        # print(f"AB_CD:{discordant_pairs_AB_CD}")
        # if discordant_pairs_AB_CD < 25:
        #     print("AB_CD:"+ warning)

        # print(f"AC_CD:{discordant_pairs_AC_CD}")
        # if discordant_pairs_AC_CD < 25:
        #     print("AC_CD:"+ warning)

        # # Perform McNemar's test between each pair of proportions
        # result_AB_AC = mcnemar(success_AB, success_AC, exact=True)
        # result_AB_CD = mcnemar(success_AB, success_CD, exact=True)
        # result_AC_CD = mcnemar(success_AC, success_CD, exact=True)

        # print('AB vs AC:', result_AB_AC, mcnemar_exact_test_confidence_interval(success_AB, success_AC))
        # print('AB vs CD:', result_AB_CD, mcnemar_exact_test_confidence_interval(success_AB, success_CD))
        # print('AC vs CD:', result_AC_CD, mcnemar_exact_test_confidence_interval(success_AC, success_CD))
