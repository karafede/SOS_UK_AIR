# Set-up
# Import modules
import os
import xmltodict
import json
import argparse

# Change directory
# os.chdir(os.path.expanduser(working_directory))

# Define function
def str2bool(v):
  return v.lower() in ("yes", "true", "t", "1")

# Define arguments
parser = argparse.ArgumentParser()
parser.add_argument("-i", "--input_file", default = "", 
                    help = "File name of XML file to be converted.")
parser.add_argument("-o", "--output_file", default = "", 
                    help = "File name of converted file.")
parser.add_argument("-p", "--pretty", default = True, 
                    help = "Should the output be formatted for readability (a pretty output)?")

# Parse arguments
args = parser.parse_args()

# To logical, to-do: do this a better way
args.pretty = str2bool(args.pretty)

# Ensure path is expanded
args.input_file = os.path.expanduser(args.input_file)

# Load document
document_file = open(args.input_file, "r")
text = document_file.read()

# Parse xml document
xml = xmltodict.parse(text)

# Create a json character string
if args.pretty:
  json = json.dumps(xml, sort_keys = False, indent = 2, separators = (",", ": "))
else:
  json = json.dumps(xml, sort_keys = False, separators = (",", ":"))

# Save json as text
output_file = open(args.output_file, "w")
output_file.write(json)
output_file.close()
