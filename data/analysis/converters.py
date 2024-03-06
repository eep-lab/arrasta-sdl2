import csv

import pandas as pd

from headers import data_header, timestamps_header, session_name_dict
from fileutils import as_timestamps, load_file, file_exists

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

def convert_data_file(entry):

    def get_valid_next(reader):
        next_row = next(reader, None)
        while next_row is not None:
            if next_row[0].startswith(data_header[0]):
                next_row = next(reader, None)
            else:
                break
        return next_row

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

            if '033' in entry:
                pass

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

            # Add the processed row to the list
            processed_lines.append(row)
            row = next_row

    save_processed_file(entry, processed_lines)

def get_data_file(entry):
    try:
        data = pd.read_csv(entry, sep='\t', encoding='utf-8')
    except pd.errors.EmptyDataError:
        print(f'Empty data file {entry}')
        return None
    return data

def convert_info_file(entry):
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

    print(f'Session key: {session_key}')
    # Open .info TSV file
    with open(entry, 'r', encoding='utf-8') as file:
        reader = csv.reader(file, delimiter='\t')

        # Processed lines will be stored in this list
        processed_lines = []
        for i, row in enumerate(reader):
            # split row in case of '=' in the first column
            if '=' in row[0]:
                row = row[0].split('=')

            if (i == 0) or (i == 1) or (i == 2) or (i == 3) or \
               (i == 6) or (i == 7) or (i == 8) or (i == 9):
                if len(row) == 2:
                    if row[0] == 'Resultado:':
                        session_result = None

                elif len(row) == 3:
                    if row[0] == 'Resultado':
                        session_result = None

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

        if session_result is not None:
            processed_lines.append(['Resultado:', session_result])

    if len(processed_lines) == 0:
        processed_lines.append(['Resultado:', 'Cancelada'])
    elif len(processed_lines) > 0:
        if 'Resultado' in processed_lines[-1][0]:
            pass
        else:
            processed_lines.append(['Resultado:', 'Concluida'])

    save_processed_file(entry, processed_lines)

def add_info_to_data_files(entry):
    """
    Read .info.processed and add it to .data.processed files.
    """
    def add_columns_to_data_file(entry, column_data):
        name, condition, date, time = column_data

        data = get_data_file(entry)
        if data is None:
            return

        # include new columns into the dataframe
        data['Participant'] = name
        data['Condition'] = condition
        data['Date'] = date
        data['Time'] = time
        data['File'] = entry

        # save the new dataframe into a new file
        data.to_csv(entry, sep='\t' ,index=False)
        print(f'Participant, Condition, Date, Time, File, columns added to file {entry}.')

    # read csv info file
    data_info = entry.replace('.data.processed', '.info.processed')
    if file_exists(data_info):
        info = load_file(data_info)
        name = info.loc['Nome_do_sujeito:'][1]
        date = info.loc['Data_Inicio:'][1]
        time = info.loc['Hora_Inicio:'][1]

        if '0-Pre-treino' in info.loc['Nome_da_sessao:'][1]:
            condition = '0'

        elif '7-Sondas-CD-Palavras-12-ensino-8-generalizacao' in info.loc['Nome_da_sessao:'][1]:
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

        else:
            condition = 'NA'

        add_columns_to_data_file(entry, [name, condition, date, time])