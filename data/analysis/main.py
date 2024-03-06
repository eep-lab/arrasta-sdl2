from fileutils import as_data, file_exists
from fileutils import cd, list_files, list_data_folders, load_file
from fileutils import walk_and_execute
from classes import Information
from metadata import Metadata
from datetime import timedelta

import pandas as pd

def calculate_measures_in_day_and_save_to_metadata():
    total_duration_in_day = timedelta()

    total_trials_in_day = 0
    total_hits_in_day = 0
    total_misses_in_day = 0

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

    container = []
    for entry in list_files('.info.processed'):
        data_file = as_data(entry, processed=True)
        if not file_exists(data_file):
            continue

        info = Information(entry)
        if info.has_valid_result():
            container.append(load_file(data_file))
            total_duration_in_day += info.duration.value

    # join all data files
    data = pd.concat(container)
    container = None

    total_trials_in_day = data.shape[0]

    # hits
    hits = data[data['Result'] == 'Hit']
    total_hits_in_day = hits.shape[0]

    misses = data[data['Result'] == 'Miss']
    total_misses_in_day = misses.shape[0]

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



    CD_w1_probes = data[(data['Relation'] == 'C-D') & (data['Condition'] == 7)]
    total_CD_w1_probe_trials_in_day = CD_w1_probes.shape[0]

    CD_w1_hits = CD_w1_probes[CD_w1_probes['Result'] == 'Hit']
    total_CD_w1_probe_hits_in_day = CD_w1_hits.shape[0]

    CD_w1_misses = CD_w1_probes[CD_w1_probes['Result'] == 'Miss']
    total_CD_w1_probe_misses_in_day = CD_w1_misses.shape[0]

    

    AC_probes = data[(data['Relation'] == 'A-C') & (data['Condition'] == 6)]
    total_AC_probe_trials_in_day = AC_probes.shape[0]

    AC_hits = AC_probes[AC_probes['Result'] == 'Hit']
    total_AC_probe_hits_in_day = AC_hits.shape[0]

    AC_misses = AC_probes[AC_probes['Result'] == 'Miss']
    total_AC_probe_misses_in_day = AC_misses.shape[0]

    metadata = Metadata()
    metadata.items['total_duration_in_day'] = str(total_duration_in_day)
    metadata.items['total_trials_in_day'] = str(total_trials_in_day)
    metadata.items['total_hits_in_day'] = str(total_hits_in_day)
    metadata.items['total_misses_in_day'] = str(total_misses_in_day)

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

    metadata.items['total_CD_w2_probe_trials_in_day'] = str(total_CD_w2_probe_trials_in_day)
    metadata.items['total_CD_w2_probe_hits_in_day'] = str(total_CD_w2_probe_hits_in_day)
    metadata.items['total_CD_w2_probe_misses_in_day'] = str(total_CD_w2_probe_misses_in_day)

    metadata.items['total_AC_probe_trials_in_day'] = str(total_AC_probe_trials_in_day)
    metadata.items['total_AC_probe_hits_in_day'] = str(total_AC_probe_hits_in_day)
    metadata.items['total_AC_probe_misses_in_day'] = str(total_AC_probe_misses_in_day)

    metadata.save()

def create_metadata():
    cd('..')
    participant_folders = list_data_folders()
    for folder in participant_folders:
        walk_and_execute(folder, calculate_measures_in_day_and_save_to_metadata)

if __name__ == "__main__":
    create_metadata()