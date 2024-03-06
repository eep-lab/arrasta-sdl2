import yaml
from fileutils import file_exists

__default_entry__ = 'metadata.yaml'

class Metadata:
    def __init__(self, entry=None):
        if entry is None:
            self.entry = __default_entry__
        else:
            self.entry = entry
        self.items = {}
        self.load()

    def __str__(self) -> str:
        return str(self.items)

    def load(self):
        if file_exists(self.entry):
            with open(self.entry, 'r') as file:
                self.items = yaml.safe_load(file)
        else:
            self.items = {}

        if self.items is None:
            self.items = {}

    def save(self):
        with open(self.entry, 'w') as file:
            yaml.safe_dump(self.items, file)