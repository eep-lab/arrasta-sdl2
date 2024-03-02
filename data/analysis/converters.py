import csv

import pandas as pd

from headers import data_header, timestamps_header, session_name_dict
from fileutils import as_timestamps, load_file

def get_data_events(entry, row):
    hashed_row = dict(zip(data_header, row))
    uid = int(hashed_row[data_header[1]])
    events = load_file(as_timestamps(entry))
    return events[events[data_header[1]] == uid]

def has_consequence(events):
    target_events_names = ['Hit.Start', 'Miss.Start']
    for event_name in target_events_names:
        if True in events[timestamps_header[3]].str.contains(event_name).values:
            return 'True'
    return 'False'

def get_comparison_latency(events):
    start_pattern = r'(Comparisons.Start|MTSStimuli.ModalityCD.Show)'
    start_events = events[timestamps_header[3]].str.match(start_pattern)

    end_pattern = r'(Stimulus.Response.*.Comparison|Stimulus.Response.Speech|Stimulus.Robot.Response.Speech)'
    end_events = events[timestamps_header[3]].str.match(end_pattern)

    filtered_start_events = events[start_events]
    filtered_end_events = events[end_events]

    if filtered_start_events.empty or filtered_end_events.empty:
        return 'NA'

    start_timestamp = filtered_start_events[timestamps_header[0]].str.replace(',', '.').astype(float).min()
    end_timestamp = filtered_end_events[timestamps_header[0]].str.replace(',', '.').astype(float).min()

    return str(end_timestamp - start_timestamp).replace('.', ',')

def save_processed_file(entry, processed_lines):
    with open(entry+'.processed', 'w', encoding='utf-8', newline='') as outfile:
        writer = csv.writer(outfile, delimiter='\t')
        writer.writerows(processed_lines)
    print(f'Processed file {entry} saved to {entry}.processed')

def convert_data_file(entry):
    # Open the TSV file
    with open(entry, 'r', encoding='utf-8') as file:
        reader = csv.reader(file, delimiter='\t')

        # Processed lines will be stored in this list
        processed_lines = []
        first_row = True
        for row in reader:
            if first_row:
                first_row = False
                processed_lines.append(data_header)
                continue

            # Skip lines starting with "Report.Timestamp"
            if row[0].startswith(data_header[0]):
                continue

            # Insert columns based on the number of columns in the row
            if len(row) == 18:
                row.insert(17, 'NA')  # Insert first new column at index 17
                row.insert(18, 'NA')  # Insert second new column at index 18
            elif len(row) == 19:
                row.insert(18, 'NA')  # Insert new column at index 17

            # add missing values
            row.append('NA')
            row.append('NA')

            target_events = get_data_events(entry, row)

            row[20] = has_consequence(target_events)
            row[21] = get_comparison_latency(target_events)

            trial_ID, _, cycle_ID, name, relation, comparisons = row[7].replace('C)', '').split(' ')
            row[7] = trial_ID
            row.insert(8, cycle_ID)
            row.insert(9, name)
            row.insert(10, relation)
            row.insert(11, comparisons)

            # if len(row) > 26:
            #     print(f'Row has more than 22 columns: {row}')
            #     # delete all items after the 22th
            #     del row[26:]

            row = row[:26]

            # Add the processed row to the list
            processed_lines.append(row)

    save_processed_file(entry, processed_lines)

def convert_info_file(entry):

    # check if .interrupted.bin file exists
    bin_file = entry.replace('.info', '.interrupted.bin')
    if file_exists(bin_file):
        print(f'Bin file {bin_file} exists, session was interrupted.')
        session_result = 'Interrompida'

    # Open .data.processed TSV with pandas
    data_file = entry.replace('.info', '.data.processed')

    # check if data_file is empty
    try:
        data = pd.read_csv(data_file, sep='\t', encoding='utf-8')
    except pd.errors.EmptyDataError:
        print(f'Empty data file {data_file}')
        return

    session_key = '-'.join([data[data_header[6]][0], str(data[data_header[8]][0])])
    print(f'Session key: {session_key}')
    # Open .info TSV file
    with open(entry, 'r', encoding='utf-8') as file:
        reader = csv.reader(file, delimiter='\t')

        # Processed lines will be stored in this list
        processed_lines = []
        for i, row in enumerate(reader):
            if (i == 0) or (i == 1) or (i == 2) or (i == 3) or (i == 6) or (i == 7):
                if len(row) == 2:
                    pass

                elif len(row) == 3:
                    row[0] = row[0]+row[1]
                    row[1] = row[2]
                    row = row[:2]

                if row[1] == 'Sessão':
                    row[1] = session_name_dict[session_key]
                    row = row[:2]

            elif i == 4:
                if len(row) == 3:
                    if row[0] == 'Inicio:':
                        new_row = ['Data_Inicio:', row[1]]
                        processed_lines.append(new_row)

                        row[0] = 'Hora_Inicio:'
                        row[1] = row[2]
                        row = row[:2]

                elif len(row) == 4:
                    if row[0] == 'Inicio':
                        new_row = ['Data_Inicio:', row[2]]
                        processed_lines.append(new_row)

                        row[0] = 'Hora_Inicio:'
                        row[1] = row[3]
                        row = row[:2]

            elif i == 5:
                if len(row) == 3:
                    if row[0] == 'Termino:':
                        new_row = ['Data_Termino:', row[1]]
                        processed_lines.append(new_row)

                        row[0] = 'Hora_Termino:'
                        row[1] = row[2]
                        row = row[:2]

                elif len(row) == 4:
                    if row[0] == 'Termino':
                        new_row = ['Data_Termino:', row[2]]
                        processed_lines.append(new_row)

                        row[0] = 'Hora_Termino:'
                        row[1] = row[3]
                        row = row[:2]

            # Add the processed row to the list
            processed_lines.append(row)

    save_processed_file(entry, processed_lines)