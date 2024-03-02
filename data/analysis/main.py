from fileutils import cd, list_data_folders, list_files
from converters import convert_data_file, convert_info_file

cd('..')

excluded_folders = ['0-Rafael', '3-Teste']
data_folders = list_data_folders(excluded_folders)

def convert_data():
    for folder_name in data_folders:
        cd(folder_name)
        cd('analysis')
        safety_copy_folders = list_data_folders()
        for data_folder in safety_copy_folders:
            cd(data_folder)
            for entry in list_files('.data'):
                convert_data_file(entry)

            for entry in list_files('.info'):
                convert_info_file(entry)
            cd('..')
        cd('..')
        cd('..')

if __name__ == "__main__":
    convert_data()

