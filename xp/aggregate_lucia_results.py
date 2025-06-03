from os import listdir, remove
from os.path import isfile, join, exists
import re

output_files_seen = set()
for filename in listdir("logs"):
    filename_path = join("logs", filename)
    if not isfile(filename_path):
        continue
    pattern_job_id = r"^(.*)_(\d+)_.*(out)"
    search_obj = re.search(pattern_job_id, filename)
    if search_obj is not None:
        xp_name = search_obj.group(1)
        job_id = int(search_obj.group(2))
        output_file = f"results/{xp_name}_{job_id}.csv"
        if output_file not in output_files_seen and exists(output_file):
            remove(output_file)
        output_files_seen.add(output_file)
        with open(filename_path) as read_file:
            content = read_file.readline()
        with open(output_file, "a+") as file:
            file.write(content)
print("results written to:")
for file in output_files_seen:
    print(f"- {file}")

