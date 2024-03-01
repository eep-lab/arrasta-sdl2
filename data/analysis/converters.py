import csv

import pandas as pd

from headers import data_header, timestamps_header
from fileutils import as_timestamps, load_file

def get_data_events(entry, row):
    hashed_row = dict(zip(data_header, row))
    uid = int(hashed_row[data_header[1]])
    events = load_file(as_timestamps(entry))
    return events[events[data_header[1]] == uid]

def has_consequence(events):
    target_events_names = ['Hit.Start', 'Miss.Start']
    for event_name in target_events_names:
        if True in events['Event'].str.contains(event_name).values:
            return 'True'
    return 'False'

def get_comparison_latency(events):
    start_pattern = r'(Comparisons.Start|MTSStimuli.ModalityCD.Show)'
    start_events = events['Event'].str.match(start_pattern)

    end_pattern = r'(Stimulus.Response.*.Comparison|Stimulus.Response.Speech)'
    end_events = events['Event'].str.match(end_pattern)

    filtered_start_events = events[start_events]
    filtered_end_events = events[end_events]

    if filtered_start_events.empty or filtered_end_events.empty:
        return 'NA'

    start_timestamp = filtered_start_events['Timestamp'].str.replace(',', '.').astype(float).min()
    end_timestamp = filtered_end_events['Timestamp'].str.replace(',', '.').astype(float).min()

    return str(end_timestamp - start_timestamp).replace('.', ',')


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

            if len(row) > 26:
                print(f'Row has more than 22 columns: {row}')
                # delete all items after the 22th
                del row[26:]

            row = row[:26]

            # Add the processed row to the list
            processed_lines.append(row)

    # Write the processed lines to a new TSV file
    with open(entry+'.processed', 'w', encoding='utf-8', newline='') as outfile:
        writer = csv.writer(outfile, delimiter='\t')
        writer.writerows(processed_lines)