import os
import time
import datetime
import shutil

import pandas as pd

from headers import data_header

def cd(directory):
    os.chdir(directory)
    print("Current Working Directory: ", os.getcwd())

def get_real_filepath(entry):
    return os.path.join(os.getcwd(), entry)

def load_file(entry):
    return pd.read_csv(entry, sep='\t', header=0, engine='python')

    # # get directory path where this script is located
    # dir_path = os.path.dirname(os.path.realpath(__file__))
    # # concatenate directory path with file name
    # filepath = os.path.join(dir_path, filename)
    # # return the file
    # return pd.read_excel(filepath, engine='odf')

def list_data_folders(exclude_list=[]):
    exclude_list += ['__pycache__', 'analysis']
    # Get all entries in the current directory
    all_entries = os.listdir('.')
    # Filter out files and excluded folders
    return [entry for entry in all_entries \
               if os.path.isdir(entry) \
               and entry not in exclude_list]

def list_files(extension=''):
    # Get all entries in the current directory
    all_entries = os.listdir('.')
    # Filter out folders and files with different extensions
    return [entry for entry in all_entries \
               if os.path.isfile(entry) \
               and entry != 'ID' \
               and entry.endswith(extension)]

def get_readable_creation_date(real_filepath):
    """
    Note: Windows only.
    """
    # Get the creation time of the file
    creation_time = os.path.getctime(real_filepath)
    # Convert the creation time to a human-readable format
    return time.ctime(creation_time)


def get_creation_date(real_filepath, format='%Y-%m-%d'):
    """
    Note: Windows only.
    """
    # Get the creation time of the file
    creation_time = os.path.getctime(real_filepath)
    # Convert the creation time to a datetime object
    date_time_obj = datetime.datetime.fromtimestamp(creation_time)
    # Format the date as YYYY-MM-DD
    return date_time_obj.strftime(format)

def safety_copy(entry):
    source = get_real_filepath(entry)
    creation_date = get_creation_date(source)
    destination = os.path.join(os.getcwd(), 'analysis', creation_date, entry)

    # Extract the directory part of the destination path
    destination_dir = os.path.dirname(destination)

    # Check if the destination file already exists
    if not os.path.exists(destination):
        # Create intermediate directories if they don't exist
        if not os.path.exists(destination_dir):
            os.makedirs(destination_dir, exist_ok=True)
        shutil.copy2(source, destination)
        print(f"File '{source}' copied to '{destination}'.")
    else:
        print(f"The file '{destination}' already exists.")

    return destination

def replace_extension(filename, new_extension):
    # Split the filename into root and extension
    root, _ = os.path.splitext(filename)
    # Add the new extension
    return root + new_extension

def as_timestamps(entry):
    return replace_extension(entry, '.timestamps')