from fileutils import as_data, file_exists, data_dir
from fileutils import cd, list_files, list_data_folders, load_file
from fileutils import walk_and_execute
from correlation import plot_correlation
from classes import Information

import pandas as pd
import Levenshtein
from unidecode import unidecode_expect_ascii
from anonimizator import anonimize
from words import pre_test_hardcoded_order, cycle_per_word, category_per_word
from bs4 import BeautifulSoup
import copy
import sass

def data_by_relation(pattern, container):
    for entry in list_files('.info.processed'):
        data_file = as_data(entry, processed=True)
        if not file_exists(data_file):
            print(f'File {data_file} does not exist.')
            continue

        info = Information(entry)
        if info.has_valid_result():
            data = load_file(data_file)
            data = data[data['Relation'].str.match(pattern)]

            if not data.empty:
                print(f'File {entry} has valid result.')
                container.append(data)
            else:
                print(f'File {entry} is empty.')
        else:
            print(f'File {entry} has no valid result.')

def get_data_folders(anonimized=False):
    def folder_sorter(x):
        return int(x.split('-')[0])
    data_dir()
    participant_folders = list_data_folders()
    if anonimized:
        participant_folders = [anonimize(folder) for folder in participant_folders]
    participant_folders.sort(key=folder_sorter)
    return participant_folders

def get_probes():
    participant_folders = get_data_folders()
    container = []
    for folder in participant_folders:
        walk_and_execute(folder, data_by_relation, 'C-D', container)
    return pd.concat(container)

def save_probes_by_participant(should_fix_cycles):
    data = get_probes()
    # sort first by Date/Time, from oldest to newest
    # but convert to DateTime first
    # data['DateTime'] = pd.to_datetime(f"{data['Date']} {data['Time']}", format='%d/%m/%Y %H:%M:%S')
    # data = data.sort_values(by=['DateTime', ''])
    data = data[data['HasDifferentialReinforcement'] == False]
    column = data['Name']
    data.drop(columns='Name', inplace=True)
    data['Name'] = column

    cd('analysis')
    cd('output')

    # get unique names from  participant column
    participants = data['Participant'].unique()
    # remove nan from participants
    participants = [x for x in participants if str(x) != 'nan']
    for participant in participants:
        # filter data per participant
        filtered_data = data[data['Participant'] == participant]
        # add a new column with incremental ID
        filtered_data = filtered_data.copy()
        filtered_data['ID'] = range(1, len(filtered_data) + 1)

        # save data to participant file
        participant = participant.replace('\\', '').replace('-', '_')
        # check if participant is float
        filename = f'probes_CD_{participant}.data'
        # check if file exists
        if file_exists(filename):
            # load file
            existing_data = pd.read_csv(filename, sep='\t')
            # if same lenght, do nothing, there is no data to append
            if existing_data.shape[0] == filtered_data.shape[0]:
                print(f'File {filename} already exists and has the same length. Skipping...')
                if should_fix_cycles:
                    print(f'Fixing cycles for {filename}...')
                    existing_data['Cycle.ID'] =  filtered_data['Cycle.ID'].reset_index(drop=True)
                    existing_data.to_csv(filename, sep='\t', index=False)
                continue

            print(f'File {filename} already exists. Appending...')
            # concatenate data and keep only unique rows
            filtered_data = pd.concat([existing_data, filtered_data])
            # drop duplicates in-place
            filtered_data.drop_duplicates(subset=['ID'], keep='first', inplace=True)
        else:
            print(f'File {filename} does not exist. Creating...')
        # add 'Speech', 'Speech-2' if not exists
        if 'Speech' not in filtered_data.columns:
            filtered_data['Speech'] = ''
        if 'Speech-2' not in filtered_data.columns:
            filtered_data['Speech-2'] = ''
        if 'Speech-3' not in filtered_data.columns:
            filtered_data['Speech-3'] = ''
        filtered_data.to_csv(filename, sep='\t' , index=False)

    cd('..')

def load_all_probes():
    participant_folders = get_data_folders(anonimized=True)
    cd('analysis')
    cd('output')
    container = []
    for participant in participant_folders:
        participant = participant.replace('\\', '').replace('-', '_')
        try:
            filename = f'probes_CD_{participant}.data'
            data = pd.read_csv(filename, sep='\t')
            print(f'File {filename} loaded.')
        except FileNotFoundError:
            print(f'File {filename} not found.')
            continue
        container.append(data)
    return pd.concat(container)

def concatenate_probes(filename='probes_CD.data'):
    data = load_all_probes()
    data.to_csv(filename, sep='\t', index=False)
    print(f'All probes concatenated to file: {filename}')

def validated_speech(word):
    if not isinstance(word, str):
        word = ''
    return unidecode_expect_ascii(word.strip().lower().replace(' ', ''))

def similarity(row):
    return Levenshtein.ratio(row['Name'], validated_speech(row['Speech-2']))

def result(row):
    if row['Name'] == validated_speech(row['Speech-2']):
        return 'Hit'
    return 'Miss'

def calculate_similarity():
    cd('..')
    cd('analysis')
    cd('output')
    data = pd.read_csv('probes_CD.data', sep='\t')
    data['Levenshtein'] = data.apply(lambda row: similarity(row), axis=1)
    data['Result'] = data.apply(lambda row: result(row), axis=1)
    data['Latency'] = data.apply(lambda row: row['Latency'].replace(',', '.'), axis=1)
    data['Participant'] = data.apply(lambda row: row['Participant'].replace('\\', ''), axis=1)
    data.to_csv('probes_CD.data.processed', sep='\t' , index=False)
    # print a message indicating that the process has finished
    print('Levenshtein similarity calculated.')

def correlate_latency_levenshtein(do_global_analysis=False):
    cd('..')
    participants = list_data_folders(exclude_list=['22-GLB'])
    print(participants)
    cd('analysis')
    cd('output')
    all_data = pd.read_csv('probes_CD.data.processed', sep='\t')

    if do_global_analysis:
        # filter data by word name
        data = all_data[all_data['Name'].str.match(r'(bena|falo)')]
        data = data.sort_values(by=['Date', 'Time'])
        # plot_correlation(data['Levenshtein'], data['Latency'], 'Levenshtein', 'Latency', 'Bena e Falo')
        plot_correlation(data.index, data['Levenshtein'], 'Trial', 'Levenshtein', 'Bena e Falo')
        # plot_correlation(data.index, data['Latency'], 'Trial', 'Latency', 'Bena e Falo')
    else:
        for participant in participants:
            # print participant name and message
            print(f'Analysing participant {participant}.....')
            name = anonimize(participant, as_path=False)
            # filter data by participant
            data = all_data[all_data['Participant'] == name]

            # sort data by Cycle and Time
            # data = data.sort_values(by=['Date'])

            # 4 per cycle
            # regular_expression = r'(bena|falo)'
            # words = 'Bena e Falo'

            # 4 per cycle
            regular_expression = r'(nibe|lofi|bofi|nale|leba|nofa|bona|lefi|fabe|nilo|febi|lano)'
            words = 'Nibe e Lofi (etc)'

            data = data[data['Condition'] == 5]
            data = data[data['Name'].str.match(regular_expression)]
            # filter by word
            # plot_correlation(data['Levenshtein'], data['Latency'], 'Levenshtein index', 'Latency', name+'- Bena e Falo')
            data.reset_index(inplace=True)
            data.index = data.index + 1
            # plot_correlation(data.index, data['Levenshtein'], 'Trials', 'Levenshtein index', name+'- Bena e Falo')
            plot_correlation(data.index, data['Latency'], 'Trials', 'Latency', name+' - '+ words, save=True)

def override_CD_probes_in_data_file(must_not_override=True):
    cd('output')
    data = pd.read_csv('probes_CD.data.processed', sep='\t')
    print('----------------------------- override data files and creating probes files')
    cd('..')
    cd('..')
    participant_folders = list_data_folders()
    for participant_folder in participant_folders:
        cd(participant_folder)
        cd('analysis')
        safety_copy_data_folders = list_data_folders()
        for data_folder_by_day in safety_copy_data_folders:
            cd(data_folder_by_day)

            filtered_data = None
            for entry in list_files('.info.processed'):
                data_file = as_data(entry, processed=True)
                if not file_exists(data_file):
                    continue

                info = Information(entry)
                if info.has_valid_result():
                    if not '-CD-' in info.session_name:
                        continue

                    if file_exists(data_file.replace('.data', '.probes')):
                        if must_not_override:
                            continue
                    # print a message idenifying participant, folder, and file
                    print(f'Participant: {participant_folder}, Folder: {data_folder_by_day}, File: {data_file}')
                    data_to_override = load_file(data_file)

                    filtered_data = data[data['File'] == data_file]

                    participant = filtered_data['Participant'].str.replace(r'\\', '')
                    filtered_data = filtered_data[participant == anonimize(participant_folder, as_path=False)]

                    # check if count is the same
                    if filtered_data.shape[0] != data_to_override.shape[0]:
                        print(f'Count is different for {data_file}')
                        raise Exception('Count is different')
                    else:
                        data_to_override['Result'] = filtered_data['Result'].values
                        data_to_override['Response'] = filtered_data['Speech-2'].values
                        data_to_override.to_csv(data_file, sep='\t', index=False)
                        filtered_data['Levenshtein'].to_csv(data_file.replace('.data', '.probes'), sep='\t', index=False)
            cd('..')
        cd('..')
        cd('..')
# Function to compare two strings and make equal characters bold
def make_equal_chars_bold(str1, str2):
    if isinstance(str1, str) and isinstance(str2, str):
        if str2 == 'ns':
            return '-'
        else:
            # check if str1 and str2 are the same length
            if len(str1) != len(str2):
                # if not, return a string with the same length as str2 appending - to str1
                str1 = str1 + '-' * (len(str2) - len(str1))

            # html bold tag
            # return ''.join(f'<b>{c1}</b>' \
            #     if c1 == c2 else c2 for c1, c2 in zip(str1, str2))

            # html span tag with background color
            return ''.join(f'<span style="background-color: #D3D3D3;">{c1}</span>' \
                if c1 == c2 else c2 for c1, c2 in zip(str1, str2))
    else:
        return '-'

def create_html_tables():
    cd('output')
    # Load the data
    df = pd.read_csv('probes_CD.data.processed', sep='\t')
    # Create a dictionary to store the DataFrames for each participant
    dfs = {}
    # Iterate over the unique participants
    for participant in df['Participant'].unique():
        # Create a new DataFrame for this participant
        df_participant = df[df['Participant'] == participant].copy()

        df_participant.drop(columns='Participant', inplace=True)
        # Create the 'HTML' column
        df_participant['HTML'] = df_participant.apply(lambda row: make_equal_chars_bold(row['Name'], row['Speech-2']), axis=1)

        condition7 = df_participant[df_participant['Condition'] == 7].copy()
        condition5 = df_participant[df_participant['Condition'] == 5].copy()
        # get bena and falo data
        bena_falo_tbody = get_bena_falo_tbody(condition5)
        df_participant = condition7
        # join date time columns
        df_participant['Date'] = df_participant['Date'] + ' ' + df_participant['Time']

        # find unique date/time
        dates = df_participant['Date'].unique()

        # create a dictionary to store Cycle.ID of each Date
        cycle_per_date = {}
        for date in dates:
            cycle_per_date[date] = df_participant[df_participant['Date'] == date]['Cycle.ID'].values[0]

        container = {}
        for date in dates:
            df2 = pd.DataFrame()
            df2['Word'] = df_participant[df_participant['Date'] == date]['Name']
            # use category_per_word to get word categories
            df2['Category'] = df2['Word'].map(category_per_word)
            # category must be the first column
            df2 = df2[['Category', 'Word']]
            # use cycle_per_word to get the cycle of each word
            df2['Cycles'] = df2['Word'].map(cycle_per_word)
            df2['Cycles'] = pd.to_numeric(df2['Cycles'], errors='coerce').fillna(0).astype(int)
            df2[date] = df_participant[df_participant['Date'] == date]['HTML']
            # make sure that the order is the same as the pre_test_hardcoded_order
            df2['Word'] = pd.Categorical(df2['Word'], pre_test_hardcoded_order)
            df2 = df2.sort_values('Word')
            # drop all word columns, except the first one
            if len(container) > 0:
                df2.drop(columns='Word', inplace=True)
                df2.drop(columns='Cycles', inplace=True)
                df2.drop(columns='Category', inplace=True)

            # reset index
            df2.reset_index(drop=True, inplace=True)
            container[date] = df2

        # concatenate all dataframes
        df_participant = pd.concat(container.values(), axis=1)

        # Store the DataFrame in the dictionary
        dfs[participant] = (df_participant, cycle_per_date, bena_falo_tbody)

    # Convert DataFrames to an HTML table using tabulate and save it to a file
    for participant, (df_participant, cycle_per_date, body) in dfs.items():
        # # Add the new column
        # df_participant.insert(0, 'Category', ['Teaching']*12 + ['Assessment']*4 + ['Assessment2']*4)

        # Convert the DataFrame to HTML
        html = df_participant.to_html(escape=False, index=False)

        html_table = BeautifulSoup(html, 'html.parser')
        # use soup to duplicate the first row
        first_row = html_table.select('tr:first-child')[0]
        # insert two rows after the first row
        first_row.insert_after(copy.deepcopy(first_row))
        # iterate over the cells of the second row
        for cell in html_table.select('tr:nth-child(2) th'):
            # use cycle_per_date to get the cycle of each date
            if cell.string in cycle_per_date:
                cycle = cycle_per_date[cell.string]
                # insert the cycle number in the cell
                cell.string = f'Cycle {cycle}'

        # Set the rowspan attribute and remove the spanned cells for header
        for category in ['Category','Word', 'Cycles']:
            cells = html_table.select(f'th:-soup-contains("{category}")')
            cells[0]['rowspan'] = len(cells)
            for cell in cells[1:]:
                cell.decompose()

        # Select the cells in the first row from the third column onwards
        cells = html_table.select('tr:first-child th:nth-of-type(n+3)')

        # Add classes to the table to identify rows with data from the same cycle
        for thead in html_table.find_all('thead'):
            tr_tags = thead.find_all('tr')
            # get the first one
            th_tags = tr_tags[0].find_all('th')

        # create a new dictionary with counts of cycles
        for tbody in html_table.find_all('tbody'):
            # # bena and falo
            tr_tags = body.find_all('tr')
            for tr in reversed(tr_tags):
                tbody.insert(0, tr)
            # teaching and assessment
            tr_tags = tbody.find_all('tr')
            for tr in tr_tags:
                cycle_count_per_date = {}
                # if tr is odd and less then 12 add class "training-row"
                if tr_tags.index(tr) % 2 != 0:
                    tr['class'] = 'even-row'
                else:
                    tr['class'] = 'odd-row'

                if tr_tags.index(tr) < 4:
                    tr['class'] += f' assessment3-row word-{tr_tags.index(tr)+1}'
                elif tr_tags.index(tr) >= 4 and tr_tags.index(tr) < 16:
                    tr['class'] += f' training-row word-{tr_tags.index(tr)+1}'
                elif tr_tags.index(tr) >= 16 and tr_tags.index(tr) < 20:
                    tr['class'] += f' assessment1-row word-{tr_tags.index(tr)+1}'
                else:
                    tr['class'] += f' assessment2-row word-{tr_tags.index(tr)+1}'

                td_tags = tr.find_all('td')

                # add class "label-col-end" to td_tags[2]
                td_tags[0]['class'] = 'category-col'
                td_tags[1]['class'] = 'word-col'
                td_tags[2]['class'] = 'labels-end-col'

                # content columns start at index 3
                td_start = 3
                for td, th in zip(td_tags[td_start:], th_tags[3:]):
                    if th.string in cycle_per_date:
                        class1 = 'training-col'
                        cycle = cycle_per_date[th.string]

                        # count the number of occurrences of each cycle
                        if cycle in cycle_count_per_date:
                            cycle_count_per_date[cycle] += 1
                        else:
                            cycle_count_per_date[cycle] = 1

                        # if first occurrence of cycle, add "first-occurence" class
                        if cycle_count_per_date[cycle] == 1:
                            class2 = f'cycle{cycle} first-occurrence'
                        else:
                            class2 = f'cycle{cycle}'

                        td['class'] = ' '.join([class1, class2])

        # Replace the white space with a <br> tag
        for cell in cells:
            parts = cell.string.split(' ', 1)
            if len(parts) == 1:
                continue
            cell.clear()
            cell.append(parts[0])
            cell.append(html_table.new_tag('br'))
            cell.append(parts[1])

        # Set the rowspan attribute and remove the spanned cells
        for category in ['Teaching', 'Assessment', 'Generalization']:
            cells = html_table.select(f'td:-soup-contains("{category}")')
            if cells:
                cells[0]['rowspan'] = len(cells)
                for cell in cells[1:]:
                    cell.decompose()
            else:
                print(f'Category {category} not found in participant {participant}')

        # replace Assessment with "Assessment<br>Syllabic|Intrasyllabic"
        cells = html_table.select('td:-soup-contains("Assessment")')
        br_tag = html_table.new_tag('br')
        cells[0].clear()
        cells[0].append('Assessment')
        cells[0].append(br_tag)
        cells[0].append('Syllabic|Intrasyllabic')

        # replace Generalization with "Assessment<br>Intrasyllabic|Syllabic"
        cells = html_table.select('td:-soup-contains("Generalization")')
        if cells:
            br_tag = html_table.new_tag('br')
            cells[0].clear()
            cells[0].append('Assessment')
            cells[0].append(br_tag)
            cells[0].append('Intrasyllabic|Syllabic')
        else:
            print(f'Category Generalization not found in participant {participant}')

        for category in ['0', '1', '2', '3', '6', '7', 'Constant', 'falo', 'bena']:
            if category == 'falo' or category == 'bena':
                cells = html_table.select(f'td.word-col:-soup-contains("{category}")')
            else:
                cells = html_table.select(f'td:-soup-contains("{category}")')
            if cells:
                cells[0]['rowspan'] = len(cells)
                for cell in cells[1:]:
                    cell.decompose()
            else:
                print(f'Category {category} not found in participant {participant}')

        for category in ['4', '5']:
            cells = html_table.select(f'td:-soup-contains("{category}")')
            if cells:
                cells[0]['rowspan'] = 2
                cells[2]['rowspan'] = 2
                for cell in cells[1:2]:
                    cell.decompose()
                for cell in cells[3:]:
                    cell.decompose()
            else:
                print(f'Category {category} not found in participant {participant}')

        # replace 0 in Cycles with -
        cells = html_table.select('td:-soup-contains("0")')
        for cell in cells:
            cell.string = 'None'

        # replace 7 in Cycles with -
        cells = html_table.select('td:-soup-contains("7")')
        for cell in cells:
            cell.string = 'All'

        # Create a new BeautifulSoup object
        soup = BeautifulSoup('', 'html.parser')
        html_tag = soup.new_tag('html')
        head_tag = soup.new_tag('head')
        link_tag = soup.new_tag('link', rel='stylesheet', href='style.css')
        body_tag = soup.new_tag('body')
        body_tag.append(html_table)
        head_tag.append(link_tag)
        # Append the table to the body tag
        html_tag.append(head_tag)
        html_tag.append(body_tag)
        soup.clear()
        soup.append(html_tag)

        # Convert the modified HTML back to a string
        html = str(soup)

        # Write the HTML to a file
        participant = participant.replace('\\', '').replace('-', '_')
        with open(f'probes_CD_{participant}.html', 'w') as f:
            f.write(html)
        print(f'File probes_CD_{participant}.html created.')
    cd('..')

def get_bena_falo_tbody(df_participant):
    # find unique date/time
    dates = df_participant['Date'].unique()

    # create a dictionary to store Cycle.ID of each Date
    cycle_per_date = {}
    for date in dates:
        cycle_per_date[date] = df_participant[df_participant['Date'] == date]['Cycle.ID'].values[0]

    # create a new dataframe with only bena and falo
    pattern = r'(bena|falo)'
    df_constant = df_participant[df_participant['Name'].str.match(pattern)].copy()
    # df_other = df_participant[~df_participant['Name'].str.match(pattern)].copy()
    container = {}
    for date in dates:
        df2 = pd.DataFrame()
        df2['Word'] = df_constant[df_constant['Date'] == date]['Name']
        # use category_per_word to get word categories
        df2['Category'] = df2['Word'].map(category_per_word)
        # category must be the first column
        df2 = df2[['Category', 'Word']]
        # use cycle_per_word to get the cycle of each word
        df2['Cycles'] = df2['Word'].map(cycle_per_word)
        df2['Cycles'] = pd.to_numeric(df2['Cycles'], errors='coerce').fillna(0).astype(int)
        df2[date] = df_constant[df_constant['Date'] == date]['HTML']
        # make sure that the order is the same as the pre_test_hardcoded_order
        df2['Word'] = pd.Categorical(df2['Word'], pre_test_hardcoded_order)
        df2 = df2.sort_values('Word')
        # drop all word columns, except the first one
        if len(container) > 0:
            df2.drop(columns='Word', inplace=True)
            df2.drop(columns='Cycles', inplace=True)
            df2.drop(columns='Category', inplace=True)

        # reset index
        df2.reset_index(drop=True, inplace=True)
        container[date] = df2

    # concatenate all dataframes
    df_participant = pd.concat(container.values(), axis=1)

    # Convert the DataFrame to HTML
    html = df_participant.to_html(escape=False, index=False)
    soup = BeautifulSoup(html, 'html.parser')
    # get table tbody
    return soup.find('tbody')

def compile_sass():
    # compile sass to css
    sass.compile(dirname=('.', '.'))

if __name__ == "__main__":
    compile_sass()
    # correlate_latency_levenshtein()
    create_html_tables()