from safety_copy import make_safety_copy
from convertions import convert_all
from speech import save_probes_by_participant, concatenate_probes
from speech import calculate_similarity, override_CD_probes_in_data_file
from main import create_metadata

def prepare_for_transcription():
    """
    One must manually add transcripts to probes_CD_{participant}.data files
    """
    make_safety_copy()
    exclude_list = [
        '8-ROS',
        '10-VAU',

        '1-Sara',
        '2-Fione',
        '4-Livia',
        '5-Juliana',
        '6-Marcus',
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
        '27-DAL',
        '28-TIG',
        '24-ADO',
        # '29-SIN',
        # '32-KIK',
        # '34-GST',

        '17-XIL',
        '25-NIC',
        '30-CRL',

        # desistências
        # '11-DNA',
        # '26-IGR',
        # '31-JUA',
        # '22-GLB',
        # '33-VCT',
        # '35-SMI',
    ]

    convert_all(exclude_list)
    save_probes_by_participant()

if __name__ == "__main__":
    prepare_for_transcription()
    # do manual transcription
    # concatenate_probes()
    # calculate_similarity()
    # override_CD_probes_in_data_file()
    # create_metadata()