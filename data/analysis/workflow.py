from safety_copy import make_safety_copy
from convertions import convert_all
from speech import save_probes_by_participant, concatenate_probes
from speech import calculate_similarity, override_CD_probes_in_data_file
from main import create_metadata, fix_cycles
from anonimizator import deanonimize

def prepare_for_transcription():
    """
    One must manually add transcripts to probes_CD_{participant}.data files
    """
    make_safety_copy()
    exclude_list = [
        deanonimize('1-SAR'),
        deanonimize('2-FIO'),
        deanonimize('4-LIV'),
        deanonimize('5-JUL'),
        deanonimize('6-MAR'),
        '9-CES',
        '12-MED',
        '13-AND',
        '14-MSC',
        '15-VER',
        '16-GUA',
        '18-FEL',
        '19-SAN',
        '20-CAM',
        '21-GIO',
        '23-KTL',
        '24-ADO',
        '27-DAL',
        '28-TIG',
        '32-KIK',
        '34-GST',
        '29-SIN',

        '17-XIL',
        '25-NIC',
        '30-CRL',
        '35-SMI',

        # desistências
        '8-ROS',
        '10-VAU',
        '11-DNA',
        '26-IGR',
        '31-JUA',
        '22-GLB',
        '33-VCT',
    ]

    convert_all(exclude_list=exclude_list)
    fix_cycles()
    save_probes_by_participant(False)

if __name__ == "__main__":
    # prepare_for_transcription()
    # do manual transcription
    # concatenate_probes()
    # calculate_similarity()
    # override_CD_probes_in_data_file(False)
    create_metadata()