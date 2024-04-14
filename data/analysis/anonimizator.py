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

def deanonimize(input_name):
    def get_key_of_value(my_dict, input_value):
        for key, value in my_dict.items():
            if input_value == value:
                return key

        print(f'Error: {input_value} is not a valid name.')

    name = input_name.replace('\\', '')

    try:
        deanonimized_name = get_key_of_value(names.items, name)
    except KeyError:
        deanonimized_name = name
    except TypeError:
        print(f'Error: {name} is not a valid name.')

    return deanonimized_name

if __name__ == "__main__":
    print(anonimize('1-XXXXX'))
    print(deanonimize('1-SAR'))