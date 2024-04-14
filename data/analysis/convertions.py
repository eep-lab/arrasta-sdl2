from fileutils import cd, list_data_folders, list_files, walk_and_execute
from converters import convert_data_file, convert_info_file, add_info_to_data_files

def convert(override=True):
    print('*****************************   Converting data files...')
    for entry in list_files('.data'):
        convert_data_file(entry, override=override)

    print('*****************************   Converting info files...')
    for entry in list_files('.info'):
        convert_info_file(entry, override=override)

    print('*****************************   Adding information to data files...')
    for entry in list_files('.data.processed'):
        add_info_to_data_files(entry, override=override)

def convert_all(exclude_list=[]):
    cd('..')

    print('Folders that will be converted:')
    participant_folders = list_data_folders(exclude_list=exclude_list)
    for folder in participant_folders:
        print(folder)
    print('*****************************   Conversion started...')

    for folder in participant_folders:
        walk_and_execute(folder, convert)

    cd('analysis')

if __name__ == "__main__":
    convert_all()