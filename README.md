# mintz80_monitor
Monitor program for MintZ80.
This code was copied from https://github.com/electrickery/Z80SerialMonitor.git
It had to be altered to allow it to assemble using "Zeus" emulator's built in assembler zcl.exe

During initial development, a card from another Z80 project was used. This card contains 
AY-3-8910 IO/sound chip and 8250 UART. Since I'm already familiar with this UART, 
I cose to use it for initial development instead of learning Z80 SIO programming while 
trying to bring up unknown software on new board. This seems to be what original author
did since code to support 8250 was present in original source.

Eventually this monitor will be switched to use Z80 SIO.
