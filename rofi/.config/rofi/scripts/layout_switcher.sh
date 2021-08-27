#!/bin/sh

# layouts
tile="[]="
spiral="[@]"
deck="H[]"
bstack="TTT"
centeredmaster="|M|"
monocle="[M]"
dwindle="[\]"
grid="HHH"
centeredfloatingmaster=">M>"
tatami="|+|"
floating="><>"

layouts="$tile\n$spiral\n$deck\n$bstack\n$centeredmaster\n$monocle\n$dwindle\n$grid\n$centeredfloatingmaster\n$tatami\n$floating"

rofi_command="rofi -theme $HOME/.config/rofi/layout_switcher.rasi"

choice="$(printf "$layouts" | $rofi_command -p "-dmenu")"

setlayout() {
  dwmc run_command setlayoutex "$1"
}

case $choice in
  "$tile")
    setlayout 0
    exit ;;
  "$spiral")
    setlayout 2
    exit ;;
  "$deck")
    setlayout 4
    exit ;;
  "$bstack")
    setlayout 5
    exit ;;
  "$centeredmaster")
    setlayout 7
    exit ;;
  "$monocle")
    setlayout 1
    exit ;;
  "$dwindle")
    setlayout 3
    exit ;;
  "$grid")
    setlayout 6
    exit ;;
  "$centeredfloatingmaster")
    setlayout 8
    exit ;;
  "$tatami")
    setlayout 9
    exit ;;
  "$floating")
    setlayout 10
    exit ;;


esac
