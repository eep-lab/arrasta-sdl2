from constants import participant_folders as folders
from constants import participants_natural as folders_natural
from constants import participants_social as folders_social

from metadata import Metadata
from fileutils import cd

cd('..')
container = []

for folder in folders:
    cd(folder)
    cd('analysis')
    metadata = Metadata()
    container.append({'metadata': metadata, 'folder': folder})
    cd('..')
    cd('..')

def count_participants(container, attribute_name):
    attribute_list = []
    for item in container:
        metadata = item['metadata']
        # print('Folder:', item['folder'])
        # print('Code:', metadata.items['code'])
        attribute_list.append(metadata.items[attribute_name])

    counts = {}
    for attribute_value in attribute_list:
        if attribute_value in counts:
            counts[attribute_value] += 1
        else:
            counts[attribute_value] = 1

    print(attribute_name, counts)

count_participants(container, 'sex')
count_participants(container, 'age')