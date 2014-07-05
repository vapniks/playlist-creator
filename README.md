playlist-creator
================

A .m3u playlist creator written in Haskell with the option of using relative paths.

Usage: makeplaylist [-rn] [-o FILE] FILE/DIR...

*  -r       --recursive       Recursively read the subdirectories.
*  -R       --relative-paths  Use relative paths in the playlist.
*  -n       --noprompt        Don't prompt to overwrite existing playlist file.
*  -a       --append          Append to existing playlist instead of overwriting it.
*  -o FILE  --output=FILE     Choose the name of the playlist file to generate (default behavior is to display on standard output).
