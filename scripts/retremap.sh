xmodmap -e "remove Control = Control_R"

xmodmap -e "keycode 0x69 = Return"
xmodmap -e "keycode 0x24 = Control_R"
xmodmap -e "add Control = Control_R"

xcape -t 10000 -e "Control_R=Return"

#xmodmap -e 'keycode 36 = 0x1234'
#xmodmap -e 'add control = 0x1234'
#xmodmap -e 'keycode any = Return'
#xcape -t 10000 -e '0x1234=Return'
