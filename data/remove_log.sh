#!/bin/sh

if [[ "$OSTYPE" == "darwin"* ]]; then
	sed -i "" 's/\ \-a\ .*\.log$//g' Kepler_KOI_DV_wget.bat
else
	sed -i 's/\ \-a\ .*\.log$//g' Kepler_KOI_DV_wget.bat
fi