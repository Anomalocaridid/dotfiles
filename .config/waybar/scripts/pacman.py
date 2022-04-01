#!/usr/bin/env python
# ~/.config/waybar/scripts/pacman.py
# Check for updates and indicate when reboot would be required.

import fnmatch as fn
import re
import subprocess as sub
import json

# File to examine for packages that would require a reboot
FILE = "/usr/share/libalpm/hooks/eos-reboot-required.hook"

# Regex for searching for packages in $FILE
REGEX = r"(?<=Target\s=\s).*"

# Icon for segment
ICON = ""

# Command to run to check updates
COMMAND = "checkupdates && paru -Qua --color never"

# Get list of available updates
update_list = sub.run(COMMAND, stdout=sub.PIPE, shell=True).stdout.decode("utf-8").split()[::4]

# Get number of updates
update_count = len(update_list)

# Add update count to output object
output = {
    "text": f"{ICON} {update_count}",
}

# Indicate whether system is up to date or how many updates are available
if update_count == 0:
    output["tooltip"] = "Up to date"
else:
    # Otherwise, have it say how many updates are available
    output["tooltip"] = f"{update_count} updates available"

# Read file with packages
with open(FILE) as package_list:
    text = package_list.read()

# Extract list of package glob patterns from FILE
reboot_packages = re.findall(REGEX, text)

# Translate globs to regex, combine into one regex, and remove occurances of \Z
reboot_regex = "|".join(map(fn.translate, reboot_packages)).replace(r"\Z", "")

# If any of the available updates would need a reboot, indicate it
if re.search(reboot_regex, " ".join(update_list)) is not None:
    output["text"] += "!"
    output["tooltip"] += ", reboot after update recommended"

# Print output as json with unicode
print(json.dumps(output, ensure_ascii=False))
