'浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
'�  Program  : DIR_IO.BAS                                             �
'�  Revision : 1.0                                                    �
'�  Date     : 11/10/90                        Axiom Tech. Co., Ltd.  �
'把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
'�  Purpose  : This demo program perform one channel A/D conversion   �
'�             on software trigger by controling I/O port directly.   �
'藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕

'崢陳陳陳陳陳陳陳跳
'� AX5210 IO PORT �
'喞陳陳陳陳陳陳陳捗
   CONST IOPORT%  =&H300

'崢陳陳陳陳陳陳陳陳陳跳
'� INITIAL AX5210     �
'喞陳陳陳陳陳陳陳陳陳捗
	OUT IOPORT%+2, 1	' Write 0x01 pattern to control register */
	I%=INP(IOPORT%+2)
	IF ((I% AND 1) = 0) THEN PRINT "ax5210 not exist "
	OUT IOPORT%+2, 2	' Write 0x02 pattern to control register */
	I%=INP(IOPORT%+2)
	IF ((I% AND 2) = 0) THEN PRINT "ax5210 not exist "

'崢陳陳陳陳陳陳陳陳陳陳陳陳跳
'� SET START & STOP CHANNEL �
'喞陳陳陳陳陳陳陳陳陳陳陳陳捗
	INPUT "ENTER START CHANNEL NUMBER <S/E--0...15> ";STR%
	INPUT "ENTER STOP  CHANNEL NUMBER <S/E--0...15> ";STP%
	J%=STR%
	OUT IOPORT%+2,0 	' Enable software trigger mode

'崢陳陳陳陳陳陳陳陳跳
'� SOFTWARE TRIGGER �
'喞陳陳陳陳陳陳陳陳捗
	WHILE (1)
	   OUT IOPORT%+1,J%	' Set channel
	   OUT IOPORT%,0	' Software trigger
	   WHILE (1)		' EOC = 0 ADC ready
	     I%=INP(IOPORT%+2)
	     I%=(I% AND &H08)
	     IF I%=0 THEN GOTO CONVERSION.OK
	   WEND

CONVERSION.OK:
	   '崢陳陳陳陳陳�
	   '� READ DATA �
	   '喞陳陳陳陳陳�
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