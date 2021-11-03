# Manually Sync VSCode Settings

## Export

There are 3 things to export:
1. `settings.json` file.
2. `keybindings.json` file.
3. Extensions.

### Files

Just copy and paste the files contents.  

To find the settings file, go to settings (`Ctrl+,`) and on the top
right click on "Go to file" and there it is, the user settings in json
format.

To find the keybindings file, go to Keyboard Shortcuts (`Ctrl+k Ctrl+s`)
and repeat the same as above.

### Extensions

To get the list of extensions do:
`code --list-extensions`.  

To get the list ready for install somewhere else:
`code --list-extensions | xargs -L 1 echo code --install-extension`

## Import

1. Copy the settings and keybindings settings into the files.
2. Run the installation commands for the extensions.
