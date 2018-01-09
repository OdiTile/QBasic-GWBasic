'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Program  : DIR_IO.BAS                                             บ
'บ  Revision : 1.0                                                    บ
'บ  Date     : 11/10/90                        Axiom Tech. Co., Ltd.  บ
'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
'บ  Purpose  : This demo program perform one channel A/D conversion   บ
'บ             on software trigger by controling I/O port directly.   บ
'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ

'ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
'บ AX5210 IO PORT บ
'ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ
   CONST IOPORT%  =&H300

'ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
'บ INITIAL AX5210     บ
'ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ
	OUT IOPORT%+2, 1	' Write 0x01 pattern to control register */
	I%=INP(IOPORT%+2)
	IF ((I% AND 1) = 0) THEN PRINT "ax5210 not exist "
	OUT IOPORT%+2, 2	' Write 0x02 pattern to control register */
	I%=INP(IOPORT%+2)
	IF ((I% AND 2) = 0) THEN PRINT "ax5210 not exist "

'ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
'บ SET START & STOP CHANNEL บ
'ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ
	INPUT "ENTER START CHANNEL NUMBER <S/E--0...15> ";STR%
	INPUT "ENTER STOP  CHANNEL NUMBER <S/E--0...15> ";STP%
	J%=STR%
	OUT IOPORT%+2,0 	' Enable software trigger mode

'ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
'บ SOFTWARE TRIGGER บ
'ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ
	WHILE (1)
	   OUT IOPORT%+1,J%	' Set channel
	   OUT IOPORT%,0	' Software trigger
	   WHILE (1)		' EOC = 0 ADC ready
	     I%=INP(IOPORT%+2)
	     I%=(I% AND &H08)
	     IF I%=0 THEN GOTO CONVERSION.OK
	   WEND

CONVERSION.OK:
	   'ึฤฤฤฤฤฤฤฤฤฤฤท
	   'บ READ DATA บ
	   'ำฤฤฤฤฤฤฤฤฤฤฤฝ
	   DAT.L%=INP(IOPORT%)
	   DAT.H%=INP(IOPORT%+1)
	   CH%=(DAT.L% AND &H0F)
	   DAT%=(DAT.H% * 16) + (DAT.L% \ 16)
	   DAT%=DAT%-2048
	   PRINT USING " CHANNEL ##,   DATA ##### ";CH%;DAT%
	   INPUT " PRESS ENTER TO CONVERT NEXT CHANNEL, <e> TO EXIT ";A$
	   J%=J%+1
	   IF J% > STP% THEN J%=STR%
	   IF ((A$ = "E") OR (A$ = "e")) THEN END
	WEND
