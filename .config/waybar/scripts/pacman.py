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

with open(FILE) as package_list:
    text = package_list.read()

reboot_packages = re.findall(REGEX, text)

reboot_regex = "|".join(map(fn.translate, reboot_packages))

update_list = sub.run("checkupdates", stdout=sub.PIPE).stdout.decode("utf-8")

update_count = len(update_list.split("\n")) - 1

output = {
    "text": f"{ICON} {update_count}",
}

if update_count == 0:
    output["tooltip"] = "Up to date"
else:
    output["tooltip"] = f"{update_count} updates available"

if re.search(reboot_regex, update_list) is not None:
    output["text"] += "!"
    output["tooltip"] += ", reboot after update recommended"

print(json.dumps(output, ensure_ascii=False))
