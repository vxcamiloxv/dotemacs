#!/bin/bash
MUHOME="$HOME/.mu"

usage="usage: $(basename "$0") [-h] NUMBER_SECONDS

Runs mu commaind to find any new messages for any account

where:
    -h       show this help text
    NUMBER_SECONDS number minute efore to find new/unred messages"

: ${1?"$usage"}

while getopts ':hs:' option; do
    case "$option" in
        h) echo "$usage"
           exit
           ;;
        \?) printf "illegal option: -%s\n" "$OPTARG" >&2
            echo "$usage" >&2
            exit 1
            ;;
    esac
done

# timestamp of previous mail sync
NBACK=$(($(date +%s) - $1*60))
# number of messages after timestamp
ACTION="mu find --muhome=$MUHOME flag:new AND flag:unread AND NOT flag:trashed AND maildir:/inbox/ --sortfield=date --reverse --after=$NBACK"
NMAIL=$($ACTION 2>/dev/null | wc -l)
# Notify config
appname="mu4e"
icon=$HOME/.icons/emacs/mail-unread.svg
category="email"
urgency=normal
timeout=12000

if [ $NMAIL -eq 1 ]
then
    summary="1 New message in $($ACTION --fields="m" 2>/dev/null)"
    body="$($ACTION --fields='📩 f: s\n' 2>/dev/null)"
    notify-send -a ${appname} -i ${icon} -c ${category} -u  ${urgency} -t ${timeout} "${summary}" "${body}" &
elif [ $NMAIL -gt 1 ]
then
    summary="$NMAIL New messages"
    body="$($ACTION --fields='📩 f: s\n' 2>/dev/null)"
    notify-send -a ${appname} -i ${icon} -c ${category} -u  ${urgency} -t ${timeout} "${summary}" "${body}" &
fi
exit 0
