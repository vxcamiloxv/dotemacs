#!/bin/bash

# timestamp of previous mail sync
NBACK=$(date +%s --date="$1 sec ago")
# number of messages after timestamp
ACTION="mu find flag:unread --after=$NBACK"
NMAIL=$($ACTION 2>/dev/null | wc -l)

# Notify config
appname=mu4e
icon=mail-unread
category=email
urgency=normal
timeout=-1

if [ $NMAIL -eq 1 ]
then
    summary="1 New message in $($ACTION --fields="m" 2>/dev/null)"
    body="From: $($ACTION --fields="f" 2>/dev/null) Subject: $($ACTION --fields="s" 2>/dev/null)"
    notify-send -a ${appname} -i ${icon} -c ${category} -u  ${urgency} -t ${timeout} "${summary}" "${body}"
elif [ $NMAIL -gt 1 ]
then
    summary="$NMAIL New messages"
    body=""
    notify-send -a ${appname} -i ${icon} -c ${category} -u  ${urgency} -t ${timeout} "${summary}" "${body}"
fi
