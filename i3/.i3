set $mod Mod4

for_window [window_role="About"] floating enable
for_window [window_role="Organizer"] floating enable
for_window [window_role="Preferences"] floating enable
for_window [window_role="bubble"] floating enable
for_window [window_role="page-info"] floating enable
for_window [window_role="pop-up"] floating enable
for_window [window_role="task-dialog"] floating enable
for_window [window_role="task_dialog"] floating enable
for_window [window_role="toolbox"] floating enable
for_window [window_role="webconsole"] floating enable

for_window [window_type="dialog"] floating enable
for_window [window_type="menu"] floating enable

for_window [class="systemsettings"] floating enable
for_window [class="plasmashell"] floating enable
for_window [class="Plasma"] floating enable; border none
for_window [class="plasma-desktop"] floating enable; border none
for_window [class="win7"] floating enable; border none
for_window [class="krunner"] floating enable; border none
for_window [class="Kmix"] floating enable; border none
for_window [class="Klipper"] floating enable; border none

# plasmoid check
for_window [class="Plasmoidviewer"] floating enable; border none

for_window [class="(?!)*nextcloud*"] floating disable;

exec --no-startup-id wmctrl -c Plasma
for_window [title="Desktop @ QRect.*"] kill; floating enable; border none
for_window [title="Desktop - Plasma"] kill; floating enable; border none


bindsym $mod+f fullscreen toggle
bindsym $mod+Shift+r restart
bindsym $mod+Shift+f floating toggle
bindsym $mod+Shift+q kill

for_window [all] floating enable

