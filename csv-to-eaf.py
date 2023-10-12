##Command-line wrapper for CSV to EAF conversion using elan_data
import argparse
from elan_data import ELAN_Data
import os
from pathlib import Path
import pandas as pd
import re

##Uncomment for testing
# in_path = 'csv/LV10interview3.csv'
# out_path = 'final/LV10interview3.eaf'

def csv_to_eaf(in_path: str, out_path: str):
  ##Expand user paths
  in_path = os.path.expanduser(in_path)
  out_path = Path(os.path.expanduser(out_path))
  
  ##Read CSV
  csv = pd.read_csv(in_path)
  
  ##Create ELAN_Data object
  wav_path = Path(re.sub(r'csv/(.+)\.csv', r'input/\1.wav', in_path))
  eaf = ELAN_Data.from_dataframe(csv, out_path, wav_path, True)
  
  ##Manipulate tiers
  eaf.remove_tiers(['default'])
  eaf.add_tiers(['Noise', 'Comment', 'Redaction'])
  for tier in eaf.tier_names:
    eaf.add_participant(tier, tier)
  
  ##Save eaf
  if not os.path.isfile(out_path):
    out_path.touch()
  eaf.save_ELAN()

  return(wav_path)

def parseArguments():
  parser = argparse.ArgumentParser()
  parser.add_argument("inPath", type=str)
  parser.add_argument("outPath", type=str)
  args = parser.parse_args()
  return args

##As command-line
if __name__ == "__main__":
  args = parseArguments()
  csv_to_eaf(args.inPath, args.outPath)
