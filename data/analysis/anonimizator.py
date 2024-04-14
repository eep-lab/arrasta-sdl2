import os
from metadata import Metadata

__names_dir__ = os.path.dirname(os.path.realpath(__file__))
names = Metadata(entry=os.path.join(__names_dir__, 'names.yaml'))

def anonimize(input_name, as_path=True):
    name = input_name.replace('\\', '')

    try:
        anonimized_name = names.items[name]
    except KeyError:
        anonimized_name = name
    except TypeError:
        print(f'Error: {name} is not a valid name.')

    if as_path:
        anonimized_name = anonimized_name+'\\'

    return anonimized_name

if __name__ == "__main__":
    print(anonimize('1-Sara'))