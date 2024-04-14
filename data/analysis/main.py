from fileutils import as_data, file_exists
from fileutils import cd, list_files, list_data_folders, load_file
from fileutils import walk_and_execute
from classes import Information
from metadata import Metadata
from datetime import timedelta
from barplots import bar_plot, bar_subplots, box_plot, dispersion_plot_per_cycle, barplot_per_cycle
from anonimizator import anonimize

import pandas as pd

def calculate_measures_in_day_and_save_to_metadata(folder, date):
    def collect_data():
        time1 = timedelta()
        time2 = timedelta()

        container = []
        levenstein = {
            'CD_Training': [],
            'CD_Probes_1': [],
            'CD_Probes_2': []}

        cycle_of_day = '0'
        for entry in list_files('.info.processed'):

            data_file = as_data(entry, processed=True)
            probe_file = data_file.replace('.data', '.probes')
            if not file_exists(data_file):
                continue

            info = Information(entry)

            if info.has_valid_result():
                container.append(load_file(data_file))
                if 'Pre-treino' in info.session_name:
                    time1 += info.duration.value
                else:
                    time2 += info.duration.value

                if 'Treino-AB' in info.session_name: # Ciclo1-1-Treino-AB
                    # get 'Ciclo{n}' part of the session name
                    cycle_of_day = info.session_name.split('-')[0].replace('Ciclo', '')

                if file_exists(probe_file):
                    if 'Treino-AC-CD' in info.session_name:
                        levenstein['CD_Training'].append(load_file(probe_file)['Levenshtein'])

                    elif 'Sondas-CD-Palavras-12-ensino-8-generalizacao' in info.session_name:
                        levenstein['CD_Probes_1'].append(load_file(probe_file)['Levenshtein'])

                    elif 'Sondas-CD-Palavras-generalizacao-reservadas' in info.session_name:
                        levenstein['CD_Probes_2'].append(load_file(probe_file)['Levenshtein'])

        # join all data files
        return pd.concat(container), levenstein, (time1, time2), cycle_of_day

    participant = anonimize(folder, as_path=False).split('-')[1]
    data, levenstein, time, cycle_of_day = collect_data()

    total_pre_training_durantion_in_day, total_training_probes_duration_in_day = time
    total_pre_training_trials_in_day = 0
    total_pre_training_bb_trials_in_day = 0
    total_pre_training_cd_trials_in_day = 0

    total_training_trials_in_day = 0
    total_training_hits_in_day = 0
    total_training_misses_in_day = 0

    total_teaching_trials_in_day = 0
    total_teaching_hits_in_day = 0
    total_teaching_misses_in_day = 0

    total_AB_training_trials_in_day = 0
    total_AB_training_hits_in_day = 0
    total_AB_training_misses_in_day = 0

    total_AC_training_trials_in_day = 0
    total_AC_training_hits_in_day = 0
    total_AC_training_misses_in_day = 0

    total_CD_training_trials_in_day = 0
    total_CD_training_hits_in_day = 0
    total_CD_training_misses_in_day = 0

    total_probe_trials_in_day = 0
    total_probe_hits_in_day = 0
    total_probe_misses_in_day = 0

    total_BC_w1_probe_trials_in_day = 0
    total_BC_w1_probe_hits_in_day = 0
    total_BC_w1_probe_misses_in_day = 0

    total_CB_w1_probe_trials_in_day = 0
    total_CB_w1_probe_hits_in_day = 0
    total_CB_w1_probe_misses_in_day = 0

    total_BC_w2_probe_trials_in_day = 0
    total_BC_w2_probe_hits_in_day = 0
    total_BC_w2_probe_misses_in_day = 0

    total_CB_w2_probe_trials_in_day = 0
    total_CB_w2_probe_hits_in_day = 0
    total_CB_w2_probe_misses_in_day = 0

    total_CD_w1_probe_trials_in_day = 0
    total_CD_w1_probe_hits_in_day = 0
    total_CD_w1_probe_misses_in_day = 0

    total_CD_w2_probe_trials_in_day = 0
    total_CD_w2_probe_hits_in_day = 0
    total_CD_w2_probe_misses_in_day = 0

    total_AC_probe_trials_in_day = 0
    total_AC_probe_hits_in_day = 0
    total_AC_probe_misses_in_day = 0

    try:
        levenstein['CD_Training'] = pd.concat(levenstein['CD_Training'])
    except ValueError:
        levenstein['CD_Training'] = pd.DataFrame()
        levenstein['CD_Training']['Levenshtein'] = [0]

    try:
        levenstein['CD_Probes_1'] = pd.concat(levenstein['CD_Probes_1'])
    except ValueError:
        levenstein['CD_Probes_1'] = pd.DataFrame()
        levenstein['CD_Probes_1']['Levenshtein'] = [0]

    try:
        levenstein['CD_Probes_2'] = pd.concat(levenstein['CD_Probes_2'])
    except ValueError:
        levenstein['CD_Probes_2'] = pd.DataFrame()
        levenstein['CD_Probes_2']['Levenshtein'] = [0]

    pre_training_trials = data[data['Condition'] == 0]
    total_pre_training_trials_in_day = pre_training_trials.shape[0]

    pre_training_bb_trials_in_day = pre_training_trials[pre_training_trials['Relation'] == 'B-B']
    total_pre_training_bb_trials_in_day = pre_training_bb_trials_in_day.shape[0]

    pre_training_cd_trials = pre_training_trials[pre_training_trials['Relation'] == 'C-D']
    total_pre_training_cd_trials_in_day = pre_training_cd_trials.shape[0]

    data = data[data['Condition'] != 0]

    total_training_trials_in_day = data.shape[0]

    # hits
    hits = data[data['Result'] == 'Hit']
    total_training_hits_in_day = hits.shape[0]

    misses = data[data['Result'] == 'Miss']
    total_training_misses_in_day = misses.shape[0]

    # teaching trials
    training_trials = data[data['HasDifferentialReinforcement'] == True]
    total_teaching_trials_in_day = training_trials.shape[0]

    teaching_hits = training_trials[training_trials['Result'] == 'Hit']
    total_teaching_hits_in_day = teaching_hits.shape[0]

    teaching_misses = training_trials[training_trials['Result'] == 'Miss']
    total_teaching_misses_in_day = teaching_misses.shape[0]

    AB_training = training_trials[training_trials['Relation'] == 'A-B']
    total_AB_training_trials_in_day = AB_training.shape[0]

    AB_training_hits = AB_training[AB_training['Result'] == 'Hit']
    total_AB_training_hits_in_day = AB_training_hits.shape[0]

    AB_training_misses = AB_training[AB_training['Result'] == 'Miss']
    total_AB_training_misses_in_day = AB_training_misses.shape[0]

    AC_training = training_trials[training_trials['Relation'] == 'A-C']
    total_AC_training_trials_in_day = AC_training.shape[0]

    AC_training_hits = AC_training[AC_training['Result'] == 'Hit']
    total_AC_training_hits_in_day = AC_training_hits.shape[0]

    AC_training_misses = AC_training[AC_training['Result'] == 'Miss']
    total_AC_training_misses_in_day = AC_training_misses.shape[0]

    CD_training = training_trials[training_trials['Relation'] == 'C-D']
    total_CD_training_trials_in_day = CD_training.shape[0]

    CD_training_hits = CD_training[CD_training['Result'] == 'Hit']
    total_CD_training_hits_in_day = CD_training_hits.shape[0]

    CD_training_misses = CD_training[CD_training['Result'] == 'Miss']
    total_CD_training_misses_in_day = CD_training_misses.shape[0]

    # probe trials
    probe_trials = data[data['HasDifferentialReinforcement'] == False]
    total_probe_trials_in_day = probe_trials.shape[0]

    probe_hits = probe_trials[probe_trials['Result'] == 'Hit']
    total_probe_hits_in_day = probe_hits.shape[0]

    probe_misses = probe_trials[probe_trials['Result'] == 'Miss']
    total_probe_misses_in_day = probe_misses.shape[0]

    BC_w1_probes = data[(data['Relation'] == 'B-C') & (data['Condition'] == 3)]
    total_BC_w1_probe_trials_in_day = BC_w1_probes.shape[0]

    BC_w1_hits = BC_w1_probes[BC_w1_probes['Result'] == 'Hit']
    total_BC_w1_probe_hits_in_day = BC_w1_hits.shape[0]

    BC_w1_misses = BC_w1_probes[BC_w1_probes['Result'] == 'Miss']
    total_BC_w1_probe_misses_in_day = BC_w1_misses.shape[0]

    CB_w1_probes = data[(data['Relation'] == 'C-B') & (data['Condition'] == 3)]
    total_CB_w1_probe_trials_in_day = CB_w1_probes.shape[0]

    CB_w1_hits = CB_w1_probes[CB_w1_probes['Result'] == 'Hit']
    total_CB_w1_probe_hits_in_day = CB_w1_hits.shape[0]

    CB_w1_misses = CB_w1_probes[CB_w1_probes['Result'] == 'Miss']
    total_CB_w1_probe_misses_in_day = CB_w1_misses.shape[0]

    BC_w2_probes = data[(data['Relation'] == 'B-C') & (data['Condition'] == 4)]
    total_BC_w2_probe_trials_in_day = BC_w2_probes.shape[0]

    BC_w2_hits = BC_w2_probes[BC_w2_probes['Result'] == 'Hit']
    total_BC_w2_probe_hits_in_day = BC_w2_hits.shape[0]

    BC_w2_misses = BC_w2_probes[BC_w2_probes['Result'] == 'Miss']
    total_BC_w2_probe_misses_in_day = BC_w2_misses.shape[0]

    CB_w2_probes = data[(data['Relation'] == 'C-B') & (data['Condition'] == 4)]
    total_CB_w2_probe_trials_in_day = CB_w2_probes.shape[0]

    CB_w2_hits = CB_w2_probes[CB_w2_probes['Result'] == 'Hit']
    total_CB_w2_probe_hits_in_day = CB_w2_hits.shape[0]

    CB_w2_misses = CB_w2_probes[CB_w2_probes['Result'] == 'Miss']
    total_CB_w2_probe_misses_in_day = CB_w2_misses.shape[0]

    CD_w2_probes = data[(data['Relation'] == 'C-D') & (data['Condition'] == 5)]
    total_CD_w2_probe_trials_in_day = CD_w2_probes.shape[0]

    CD_w2_hits = CD_w2_probes[CD_w2_probes['Result'] == 'Hit']
    total_CD_w2_probe_hits_in_day = CD_w2_hits.shape[0]

    CD_w2_misses = CD_w2_probes[CD_w2_probes['Result'] == 'Miss']
    total_CD_w2_probe_misses_in_day = CD_w2_misses.shape[0]

    mean_CD_w2_leveshtein = levenstein['CD_Probes_2'].values.mean()

    CD_w1_probes = data[(data['Relation'] == 'C-D') & (data['Condition'] == 7)]
    total_CD_w1_probe_trials_in_day = CD_w1_probes.shape[0]

    CD_w1_hits = CD_w1_probes[CD_w1_probes['Result'] == 'Hit']
    total_CD_w1_probe_hits_in_day = CD_w1_hits.shape[0]

    CD_w1_misses = CD_w1_probes[CD_w1_probes['Result'] == 'Miss']
    total_CD_w1_probe_misses_in_day = CD_w1_misses.shape[0]

    mean_CD_w1_leveshtein = levenstein['CD_Probes_1'].values.mean()

    AC_probes = data[(data['Relation'] == 'A-C') & (data['Condition'] == 6)]
    total_AC_probe_trials_in_day = AC_probes.shape[0]

    AC_hits = AC_probes[AC_probes['Result'] == 'Hit']
    total_AC_probe_hits_in_day = AC_hits.shape[0]

    AC_misses = AC_probes[AC_probes['Result'] == 'Miss']
    total_AC_probe_misses_in_day = AC_misses.shape[0]

    metadata = Metadata()
    metadata.items.clear()
    metadata.items['participant'] = participant
    metadata.items['date'] = date
    metadata.items['cycle_of_day'] = cycle_of_day

    metadata.items['total_pre_training_durantion_in_day'] = str(total_pre_training_durantion_in_day)
    metadata.items['total_pre_training_trials_in_day'] = str(total_pre_training_trials_in_day)
    metadata.items['total_pre_training_bb_trials_in_day'] = str(total_pre_training_bb_trials_in_day)
    metadata.items['total_pre_training_cd_trials_in_day'] = str(total_pre_training_cd_trials_in_day)

    metadata.items['total_training_probes_duration_in_day'] = str(total_training_probes_duration_in_day)
    metadata.items['total_training_trials_in_day'] = str(total_training_trials_in_day)
    metadata.items['total_training_hits_in_day'] = str(total_training_hits_in_day)
    metadata.items['total_training_misses_in_day'] = str(total_training_misses_in_day)

    metadata.items['total_teaching_trials_in_day'] = str(total_teaching_trials_in_day)
    metadata.items['total_teaching_hits_in_day'] = str(total_teaching_hits_in_day)
    metadata.items['total_teaching_misses_in_day'] = str(total_teaching_misses_in_day)

    metadata.items['total_probe_trials_in_day'] = str(total_probe_trials_in_day)
    metadata.items['total_probe_hits_in_day'] = str(total_probe_hits_in_day)
    metadata.items['total_probe_misses_in_day'] = str(total_probe_misses_in_day)

    metadata.items['total_AB_training_trials_in_day'] = str(total_AB_training_trials_in_day)
    metadata.items['total_AB_training_hits_in_day'] = str(total_AB_training_hits_in_day)
    metadata.items['total_AB_training_misses_in_day'] = str(total_AB_training_misses_in_day)

    metadata.items['total_AC_training_trials_in_day'] = str(total_AC_training_trials_in_day)
    metadata.items['total_AC_training_hits_in_day'] = str(total_AC_training_hits_in_day)
    metadata.items['total_AC_training_misses_in_day'] = str(total_AC_training_misses_in_day)

    metadata.items['total_CD_training_trials_in_day'] = str(total_CD_training_trials_in_day)
    metadata.items['total_CD_training_hits_in_day'] = str(total_CD_training_hits_in_day)
    metadata.items['total_CD_training_misses_in_day'] = str(total_CD_training_misses_in_day)

    metadata.items['total_BC_w1_probe_trials_in_day'] = str(total_BC_w1_probe_trials_in_day)
    metadata.items['total_BC_w1_probe_hits_in_day'] = str(total_BC_w1_probe_hits_in_day)
    metadata.items['total_BC_w1_probe_misses_in_day'] = str(total_BC_w1_probe_misses_in_day)

    metadata.items['total_CB_w1_probe_trials_in_day'] = str(total_CB_w1_probe_trials_in_day)
    metadata.items['total_CB_w1_probe_hits_in_day'] = str(total_CB_w1_probe_hits_in_day)
    metadata.items['total_CB_w1_probe_misses_in_day'] = str(total_CB_w1_probe_misses_in_day)

    metadata.items['total_BC_w2_probe_trials_in_day'] = str(total_BC_w2_probe_trials_in_day)
    metadata.items['total_BC_w2_probe_hits_in_day'] = str(total_BC_w2_probe_hits_in_day)
    metadata.items['total_BC_w2_probe_misses_in_day'] = str(total_BC_w2_probe_misses_in_day)

    metadata.items['total_CB_w2_probe_trials_in_day'] = str(total_CB_w2_probe_trials_in_day)
    metadata.items['total_CB_w2_probe_hits_in_day'] = str(total_CB_w2_probe_hits_in_day)
    metadata.items['total_CB_w2_probe_misses_in_day'] = str(total_CB_w2_probe_misses_in_day)

    metadata.items['total_CD_w1_probe_trials_in_day'] = str(total_CD_w1_probe_trials_in_day)
    metadata.items['total_CD_w1_probe_hits_in_day'] = str(total_CD_w1_probe_hits_in_day)
    metadata.items['total_CD_w1_probe_misses_in_day'] = str(total_CD_w1_probe_misses_in_day)
    metadata.items['mean_CD_w1_leveshtein'] = str(mean_CD_w1_leveshtein)


    metadata.items['total_CD_w2_probe_trials_in_day'] = str(total_CD_w2_probe_trials_in_day)
    metadata.items['total_CD_w2_probe_hits_in_day'] = str(total_CD_w2_probe_hits_in_day)
    metadata.items['total_CD_w2_probe_misses_in_day'] = str(total_CD_w2_probe_misses_in_day)
    metadata.items['mean_CD_w2_leveshtein'] = str(mean_CD_w2_leveshtein)

    metadata.items['total_AC_probe_trials_in_day'] = str(total_AC_probe_trials_in_day)
    metadata.items['total_AC_probe_hits_in_day'] = str(total_AC_probe_hits_in_day)
    metadata.items['total_AC_probe_misses_in_day'] = str(total_AC_probe_misses_in_day)

    metadata.save()

def create_metadata():
    cd('..')
    participant_folders = list_data_folders()
    for participant_folder in participant_folders:
        cd(participant_folder)
        cd('analysis')

        safety_copy_folders_by_date = list_data_folders()
        for date_folder in safety_copy_folders_by_date:
            cd(date_folder)
            calculate_measures_in_day_and_save_to_metadata(participant_folder, date_folder)
            cd('..')

        cd('..')
        cd('..')

def collect_metadata(container, plot_individual_days=False, use_levenstein=False):
    data = Metadata().items
    try:
        participant = data['participant']
    except KeyError:
        return

    date = data['date']
    cycle_of_day = data['cycle_of_day']

    total_training_probes_duration_in_day = data['total_training_probes_duration_in_day']
    if total_training_probes_duration_in_day == '0':
        return

    try:
        if use_levenstein:
            cd_probes_1_hit_rate = float(data['mean_CD_w1_leveshtein'])
        else:
            cd_probes_1_hit_rate = int(data['total_CD_w1_probe_hits_in_day']) / int(data['total_CD_w1_probe_trials_in_day'])
    except ZeroDivisionError:
        cd_probes_1_hit_rate = None

    try:
        if use_levenstein:
            cd_probes_2_hit_rate = float(data['mean_CD_w2_leveshtein'])
        else:
            cd_probes_2_hit_rate = int(data['total_CD_w2_probe_hits_in_day']) / int(data['total_CD_w2_probe_trials_in_day'])
    except ZeroDivisionError:
        cd_probes_2_hit_rate = None

    try:
        bc_probes_1_hit_rate = int(data['total_BC_w1_probe_hits_in_day']) / int(data['total_BC_w1_probe_trials_in_day'])
    except ZeroDivisionError:
        bc_probes_1_hit_rate = None

    try:
        cb_probes_1_hit_rate = int(data['total_CB_w1_probe_hits_in_day']) / int(data['total_CB_w1_probe_trials_in_day'])
    except ZeroDivisionError:
        cb_probes_1_hit_rate = None

    try:
        bc_probes_2_hit_rate = int(data['total_BC_w2_probe_hits_in_day']) / int(data['total_BC_w2_probe_trials_in_day'])
    except ZeroDivisionError:
        bc_probes_2_hit_rate = None

    try:
        cb_probes_2_hit_rate = int(data['total_CB_w2_probe_hits_in_day']) / int(data['total_CB_w2_probe_trials_in_day'])
    except ZeroDivisionError:
        cb_probes_2_hit_rate = None

    try:
        ac_probes_hit_rate = int(data['total_AC_probe_hits_in_day']) / int(data['total_AC_probe_trials_in_day'])
    except ZeroDivisionError:
        ac_probes_hit_rate = None

    try:
        ab_training_hit_rate = int(data['total_AB_training_hits_in_day']) / int(data['total_AB_training_trials_in_day'])
    except ZeroDivisionError:
        ab_training_hit_rate = None

    try:
        ac_training_hit_rate = int(data['total_AC_training_hits_in_day']) / int(data['total_AC_training_trials_in_day'])
    except ZeroDivisionError:
        ac_training_hit_rate = None

    try:
        cd_training_hit_rate = int(data['total_CD_training_hits_in_day']) / int(data['total_CD_training_trials_in_day'])
    except ZeroDivisionError:
        cd_training_hit_rate = None

    identification = [
        ("Participant", participant),
        ("Date", date),
        ("Total Duration in Day", total_training_probes_duration_in_day),
        ("Cycle of Day", cycle_of_day)
    ]

    # set names and colors for each category
    categories = [
        ("CD Probes 1", cd_probes_1_hit_rate, 'red'),
        ("AB Training", ab_training_hit_rate, 'blue'),
        ("AC Training", ac_training_hit_rate, 'blue'),
        ("CD Training", cd_training_hit_rate, 'blue'),
        ("BC Probes 1", bc_probes_1_hit_rate, 'green'),
        ("CB Probes 1", cb_probes_1_hit_rate, 'green'),
        ("BC Probes 2", bc_probes_2_hit_rate, 'yellow'),
        ("CB Probes 2", cb_probes_2_hit_rate, 'yellow'),
        ("CD Probes 2", cd_probes_2_hit_rate, 'red'),
        ("AC Probes", ac_probes_hit_rate, 'purple')
    ]

    if plot_individual_days:
        bar_plot(categories, identification)

    container.append({'categories':categories, 'identification':identification})

def single_participant_plots():
    # names = ['BC Probes 1', 'CB Probes 1', 'BC Probes 2', 'CB Probes 2']
    # names = ['AB Training', 'AC Training', 'CD Training']
    names = ['CD Probes 1', 'CD Probes 2']
    # get the first two characters of each name and remove duplicates
    codes = list(set([name[:2] for name in names]))
    # sort the codes
    codes.sort()
    # join codes with _ and append to filename
    codes = '_'.join(codes)

    cd('..')
    participant_folders = list_data_folders()
    for folder in participant_folders:
        container = []
        walk_and_execute(folder, collect_metadata, container, False, True)
        participant = anonimize(folder, as_path=False).split('-')[1]
        # bar_subplots(container, True)
        barplot_per_cycle(container=container, save=True, include_names=names, append_to_filename='_'+codes+'_'+participant+'_barplot')
        # dispersion_plot_per_cycle(container, True, style='scatter', include_names=names, append_to_filename='_'+codes+'_'+participant)

def all_participants_plots():
    cd('..')
    container = []
    participant_folders = list_data_folders()
    for folder in participant_folders:
        walk_and_execute(folder, collect_metadata, container, False, True)
    names = ['BC Probes 1', 'CB Probes 1', 'BC Probes 2', 'CB Probes 2']
    box_plot(container, True, include_names=names)
    # dispersion_plot_per_cycle(container, False, include_names=names)

if __name__ == "__main__":
    # create_metadata()
    single_participant_plots()
    # all_participants_plots()