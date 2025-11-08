# mintz80_monitor
Monitor program for MintZ80.
This code was copied from https://github.com/electrickery/Z80SerialMonitor.git
It had to be altered to allow it to assemble using "Zeus" emulator's built in assembler zcl.exe

During initial development, a card from another Z80 project was used. This card contains 
AY-3-8910 IO/sound chip and 8250 UART. Since I'm already familiar with this UART, 
I chose to use it for initial development instead of learning Z80 SIO programming while 
trying to bring up unknown software on new board. This seems to be what original author
did since code to support 8250 was present in original source.

Initially code handling receiving bytes via SIO, SIOA_RX, was having trouble keeping up 
with incoming data. After optimizing it, Intel Hex records may now be sent with 0 delay 
between characters. This allows using SIOA as console from now on and not needing the 
8250 UART card mentioned above.

Memory map

After reset, memory mapper at IO d8-df points to EEPROM for first bank 0000-1fff and to 
RAM for rest of memory 2000-ffff. This means that 8K of memory is initially used for 
boot code/monitor. If more memory is needed, additional 8K of EEPROM may be unveiled 
by switching bank 2000-3fff to EEPROM by writing 0 to IO D9. This will reduce available RAM.

Notes on programming new version of EEPROM code.

I recommend programming new code into RAM first, thoroughly testing it and then writing to EEPROM
This can be done by skipping jumper moving and using page 01 as target

It is possible to use current monitor to program new one in. I would only do this to 
program page 02 or RAM with new version of monitor for testing. After programming, new code 
will be launched and if it contains any bugs, unbricking may be necessary.

Preferred method is to use special version of monitor which loads at A000 and using it 
to alter EEPROM. 

To prepare new Intel HEX file for writing to EEPROM at 0000-1fff, edit it and replace addresses 
to load into 4000-5fff range instead. ```sed -i -e 's/^:100/:104/;s/^:101/:105/' myhexfile.hex```
* move EEPROM WR jumper to active position
* zero all RAM just to keep things neat. F 2000 ffff 00
* load special version of monitor into RAM at $A000
* switch to that version of monitor
* load new code to be programmed into RAM. I load it at $4000
* call library routine to prep programming code: K bd54
* call another routine to customize addresses: K bd57
* enter values as prompted
* currently CPLD is having trouble latching new values of memory map
  possibly due to CPLD clock being only 2x SYSCLK, so latch page of
  EEPROM to be programmed manually: O d8 00 or O d8 02
  and use p command to make sure it latched
* write new code to EEPROM by executing K bf00
* buzzer will buzz and adresses will be printed as progress is made
* checksum will be printed once programming finishes and you'll be back in monitor
* additional checks may be run now like calculating checksum
* if checksum does not match, use command A to compare EEPROM content with
  code at 4000 which was programmed into it
* because moving WR jumper is glitchy and can cause stray writes to EEPROM corrupting it, 
  safest is to switch in RAM for EEPROM: O d8 01
* once that is done move WR jumper to disabled position and switch 
  EEPROM bank in as needed
