# Lunar Lander for Analogue Pocket

+ FPGA implementation of Arcade _Lunar Lander_ (Atari, 1979) for Analogue Pocket.
+ Based on Rev.2 schematics.
+ Ported from the [original MiSTer implementation.](https://github.com/MiSTer-devel/Arcade-LunarLander_MiSTer)
+ Multiplayer support via dock.

## Known Issues

+ DIP switches are not implemented.

## ROM Instructions

ROM files are not included, you must use [mra-tools-c](https://github.com/sebdel/mra-tools-c/) to convert to a singular `llander.rom` file, then place the ROM file in `/Assets/lunarlander/ericlewis.LunarLander`.