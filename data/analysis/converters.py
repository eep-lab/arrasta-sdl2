import csv

import pandas as pd

from headers import info_header, data_header, timestamps_header, session_name_dict
from fileutils import as_timestamps, as_data, as_info, load_file, file_exists, cd

from anonimizator import anonimize
from timeutils import sum

def get_data_events(entry, row, offset=0):
    hashed_row = dict(zip(data_header, row))
    uid = int(hashed_row[data_header[1]]) + offset
    events = load_file(as_timestamps(entry))
    return events[events[data_header[1]] == uid]

def has_differential_reinforcement(events):
    target_events_names = ['Hit.Start', 'Miss.Start']
    for event_name in target_events_names:
        if True in events[timestamps_header[3]].str.contains(event_name).values:
            return 'True'
    return 'False'

def get_comparison_latency(events, events_next_row=None):

    def match_events(events, pattern):
        pattern_events = events[timestamps_header[3]].str.match(pattern)
        return events[pattern_events]

    def get_timestamp(events):
        return events[timestamps_header[0]].str.replace(',', '.').astype(float).min()

    start_pattern = r'(Comparisons.Start|Stimulus.Text.Prompt.End)'
    filtered_start_events = match_events(events, start_pattern)
    if filtered_start_events.empty:
        filtered_start_events = match_events(events, r'MTSStimuli.ModalityCD.Show')
        if filtered_start_events.empty:
            return 'NA'
        else:
            start_timestamp = get_timestamp(filtered_start_events)
            # add one second of prompt length
            start_timestamp += 1.0
    else:
        start_timestamp = get_timestamp(filtered_start_events)

    end_pattern = r'(Stimulus.Response.*.Comparison|Stimulus.Response.Speech|Stimulus.Robot.Response.Speech)'
    filtered_end_events = match_events(events, end_pattern)
    if filtered_end_events.empty:
        # try to use the next trial start event as current trial end event
        if events_next_row is not None:
            start_pattern = r'MTSStimuli.ModalityCD.Show'
            filtered_end_events = match_events(events_next_row, start_pattern)
            if filtered_end_events.empty:
                return 'NA'
            else:
                end_timestamp = get_timestamp(filtered_start_events)
                # subtract 1 second of inter trial interval
                end_timestamp -= 1.0
        else:
            return 'NA'
    else:
        end_timestamp = get_timestamp(filtered_end_events)

    latency = end_timestamp - start_timestamp
    return str(latency).replace('.', ',')

def save_processed_file(entry, processed_lines):
    if len(processed_lines) > 0:
        with open(entry+'.processed', 'w', encoding='utf-8', newline='') as outfile:
            writer = csv.writer(outfile, delimiter='\t')
            writer.writerows(processed_lines)
        print(f'Processed file {entry} saved to {entry}.processed')
    else:
        print(f'Processed lines contanainer of file {entry} is empty after processing')

def convert_data_file(entry, override=False):

    def get_valid_next(reader):
        next_row = next(reader, None)
        while next_row is not None:
            if next_row[0].startswith(data_header[0]):
                next_row = next(reader, None)
            else:
                break
        return next_row

    if not override:
        if file_exists(as_data(entry, processed=True)):
            print(f'Processed data file {entry} already exists. Skipping...')
            return

    with open(entry, 'r', encoding='utf-8') as file:
        reader = csv.reader(file, delimiter='\t')
        processed_lines = []

        first_row = True
        row = get_valid_next(reader)
        while row is not None:
            next_row = get_valid_next(reader)

            if first_row:
                first_row = False
                processed_lines.append(data_header)

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

            if next_row is None:
                target_events_next_row = None
            else:
                target_events_next_row = get_data_events(entry, row, 1)

            row[20] = has_differential_reinforcement(target_events)
            row[21] = get_comparison_latency(target_events, target_events_next_row)

            trial_ID, _, cycle_ID, name, relation, comparisons = row[7].replace(')', '').split(' ')

            row[7] = trial_ID
            row.insert(8, cycle_ID)
            row.insert(9, name)
            row.insert(10, relation)
            row.insert(11, comparisons.replace('C', ''))

            row = row[:26]

            # todo: copy rows based on the number of comparisons
            if relation == 'C-D':
                row[23] = row[21]
                row[21] = 'NA'

            # Add the processed row to the list
            processed_lines.append(row)
            row = next_row

    save_processed_file(entry, processed_lines)

def get_data_file(entry):
    try:
        data = pd.read_csv(entry, sep='\t', encoding='utf-8')
    except (pd.errors.EmptyDataError, FileNotFoundError):
        print(f'Invalid data file {entry}')
        return None
    return data

def get_session_duration(entry):
    """
    Get the duration of a session in seconds.
    Use when duration is missing in the .info file.
    """
    timestamps_file = as_timestamps(entry)
    timestamps_df = pd.DataFrame()
    timestamps_df[timestamps_header[0]] = pd.Series()
    if file_exists(timestamps_file):
        timestamps_df = load_file(timestamps_file)

    data_file = as_data(entry, processed=True)
    data_df = pd.DataFrame()
    data_df[data_header[0]] = pd.Series()
    if file_exists(data_file):
        data_df = load_file(data_file)

    # get Timestamps column from timestamps_df
    timestamps = timestamps_df[timestamps_header[0]]
    timestamps = timestamps.str.replace(',', '.').astype(float)

    # get Timestamps column from data_df
    data = data_df[data_header[0]]
    data = data.str.replace(',', '.').astype(float)

    # mix both columns
    mixed = pd.concat([timestamps, data])
    if mixed.empty:
        return 0

    # get the first and last timestamps
    start = mixed.min()
    end = mixed.max()

    # return the duration rounded in seconds as integer
    return end - start



def convert_info_file(entry, override=False):
    if not override:
        if file_exists(as_info(entry, processed=True)):
            print(f'Processed info file {entry} already exists. Skipping...')
            return

    version = 0
    with open(entry, 'r', encoding='utf-8') as file:
        first_line = file.readline().strip()
        if first_line == 'Version:1':
            version = 1

            # replace header in each line for header+tab
            lines = [first_line] + file.readlines()
            processed_lines = [line.strip().replace(header, header + '\t').split('\t') \
                               for header, line in zip(info_header, lines)]

    if version == 0:
        # check if .interrupted.bin file exists
        bin_file = entry.replace('.info', '.interrupted.bin')
        if file_exists(bin_file):
            print(f'Bin file {bin_file} exists, session was interrupted.')
            session_result = 'Interrompida'
        else:
            session_result = None

        # Open .data.processed TSV with pandas
        data_file = entry.replace('.info', '.data.processed')
        data = get_data_file(data_file)
        if data is None:
            print(f'Because {data_file} file was empty, session was calceled.')
            session_result = 'Cancelada'
            session_key = None
        else:
            session_key = '-'.join([data[data_header[6]][0], str(data[data_header[8]][0])])

        print(f'Session: {entry}, key: {session_key}')

        # Open .info TSV file
        with open(entry, 'r', encoding='utf-8') as file:
            reader = csv.reader(file, delimiter='\t')

            # Processed lines will be stored in this list
            processed_lines = []
            for i, row in enumerate(reader):
                # split row in case of '=' in the first column
                if '=' in row[0]:
                    row = row[0].split('=')

                if row[0] == info_header[10]: # Resultado:
                    session_result = None

                if (i == 0) or (i == 1) or (i == 2) or (i == 3) or \
                   (i == 6) or (i == 7) or (i == 8) or (i == 9):
                    if len(row) == 2:
                        pass

                    elif len(row) == 3:
                        row[0] = row[0]+row[1]
                        row[1] = row[2]
                        row = row[:2]

                    if row[1] == 'Sessão':
                        if session_key is not None:
                            row[1] = session_name_dict[session_key]
                            row = row[:2]

                elif i == 4:
                    if len(row) == 3:
                        if row[0] == 'Inicio:':
                            new_row = [info_header[5], row[1]] # Data_Inicio:
                            processed_lines.append(new_row)

                            row[0] = info_header[6] # Hora_Inicio:
                            row[1] = row[2]
                            row = row[:2]

                    elif len(row) == 4:
                        if row[0] == 'Inicio':
                            new_row = [info_header[5], row[2]] # Data_Inicio:
                            processed_lines.append(new_row)

                            row[0] = info_header[6] # Hora_Inicio:
                            row[1] = row[3]
                            row = row[:2]

                elif i == 5:
                    if len(row) == 3:
                        if row[0] == 'Termino:':
                            new_row = [info_header[7], row[1]] # Data_Termino:
                            processed_lines.append(new_row)

                            row[0] = info_header[8] # Hora_Termino:
                            row[1] = row[2]
                            row = row[:2]

                    elif len(row) == 4:
                        if row[0] == 'Termino':
                            new_row = [info_header[7], row[2]]  # Data_Termino:
                            processed_lines.append(new_row)

                            row[0] = info_header[8] # Hora_Termino:
                            row[1] = row[3]
                            row = row[:2]

                # Add the processed row to the list
                processed_lines.append(row)

            if session_result != None:
                processed_lines.append([info_header[10], session_result])

    if len(processed_lines) == 0:
        processed_lines.append([info_header[10], 'Cancelada'])
    elif len(processed_lines) > 0:
        get_value = lambda processed_lines, header: [line[1] for line in processed_lines if line[0] == header][0]
        for header in info_header:
            if header not in [line[0] for line in processed_lines]:
                if header == info_header[7]: # Data_Termino:
                    processed_lines.append([header, get_value(processed_lines, info_header[5])])

                elif header == info_header[8]: # Hora_Termino:
                    # get start time
                    start_time = get_value(processed_lines, info_header[6])

                    # sum start time with session duration
                    end_time = str(sum(int(get_session_duration(entry)), start_time))
                    processed_lines.append([header, end_time])

                elif header == info_header[9]: # Duration:
                    duration = str(sum(int(get_session_duration(entry)), '00:00:00'))
                    processed_lines.append([header, duration])

                elif header == info_header[10]: # Resultado:
                    processed_lines.append([header, 'Concluida'])

    save_processed_file(entry, processed_lines)

def add_info_to_data_files(entry, override=False):
    """
    Read .info.processed and add it to .data.processed files.
    """
    def add_columns_to_data_file(entry, column_data):
        name, condition, date, time = column_data

        data = get_data_file(entry)
        if data is None:
            return

        # include new columns into the dataframe
        data['Participant'] = anonimize(name)
        data['Condition'] = condition
        data['Date'] = date
        data['Time'] = time
        data['File'] = entry

        # save the new dataframe into a new file
        data.to_csv(entry, sep='\t' ,index=False)
        print(f'Participant, Condition, Date, Time, File, columns added to file {entry}.')

    if not override:
        if file_exists(as_data(entry, processed=True)):
            print(f'Processed data file {entry} already exists. Skipping...')
            return

    # read csv info file
    data_info = entry.replace('.data.processed', '.info.processed')
    if file_exists(data_info):
        info = load_file(data_info)
        name = info.loc['Nome_do_sujeito:'][1]
        date = info.loc['Data_Inicio:'][1]
        time = info.loc['Hora_Inicio:'][1]

        if '0-Pre-treino' in info.loc['Nome_da_sessao:'][1]:
            condition = '0'

        elif '0-Sondas-CD-Palavras-12-ensino-8-generalizacao' in info.loc['Nome_da_sessao:'][1]:
            condition = '7'

        elif '1-Treino-AB' in info.loc['Nome_da_sessao:'][1]:
            condition = '1'

        elif '2a-Treino-AC-CD' in info.loc['Nome_da_sessao:'][1]:
            condition = '2a'

        elif '2b-Treino-AC-Ref-Intermitente' in info.loc['Nome_da_sessao:'][1]:
            condition = '2b'

        elif '3-Sondas-BC-CB-Palavras-de-ensino' in info.loc['Nome_da_sessao:'][1]:
            condition = '3'

        elif '4-Sondas-BC-CB-Palavras-reservadas' in info.loc['Nome_da_sessao:'][1]:
            condition = '4'

        elif '5-Sondas-CD-Palavras-generalizacao-reservadas' in info.loc['Nome_da_sessao:'][1]:
            condition = '5'

        elif '6-Sondas-AC-Palavras-generalizacao-reservadas' in info.loc['Nome_da_sessao:'][1]:
            condition = '6'

        elif '7-Sondas-CD-Palavras-12-ensino-8-generalizacao' in info.loc['Nome_da_sessao:'][1]:
            condition = '7'

        else:
            condition = 'NA'

        add_columns_to_data_file(entry, [name, condition, date, time])

if __name__ == "__main__":
    cd('..')
    cd('0-Teste')
    cd('analysis')
    cd('2024-02-07')
    print(sum(int(get_session_duration('012.info')), '08:52:21'))