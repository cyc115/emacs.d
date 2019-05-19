#! /usr/bin/env bash
# take screenshot and save image path to clipboard as pastable org file path
#
# dependency:
# clipit
# gnome-screenshot

img_name=$(date -u +'%Y-%m-%d-%H-%M-%S').png
gnome-screenshot -a -f ~/org/res/img/$img_name
echo $img_name | clipit \[\[~/org/res/img/$img_name\]\]
