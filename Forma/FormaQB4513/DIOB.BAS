'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
'บ  Program  : DIOB.BAS                                               บ
'บ  Revision : 1.0                                                    บ
'บ  Date     : 11/10/90                        Axiom Tech. Co., Ltd.  บ
'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
'บ  Purpose  : This demo program perform 8 bits Digit out and         บ
'บ             Digit in by function INIT%,DO.BYTE%,DI.BYTE%.          บ
'บ  Remark   : Please connect DO channels to DI'S respectively        บ
'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ

'ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
'บ FUNCTION NO. DEFINITION บ
'ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ
   CONST INIT%	   = 0,SET.CH%	 = 1,SET.GAIN% = 2,SFT.TRG% = 3,NADC.ARY% = 4
   CONST INTR%	   = 5,DIS.INTR% = 6,READ.ST%  = 7,TRAN.DAT%= 8
   CONST SET.TIMER%= 9,TRG.SLOPE%=10,DO.BYTE%  =11,DI.BYTE% =12,NADCG.INT%=13
'ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
'บ SUB FUNCTION NO. DEFINITION บ
'ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ
   CONST RISE.EDGE%= 0,FALL.EDGE%= 1,EXT.TRG%  = 0,TIMER.TRG%= 1
   CONST ONE.CYC%  = 0,RE.CYC%	 = 1
'ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
'บ AX5210 IO PORT บ
'ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ
   CONST IOPORT%  =&H300
'ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
'บ FUNCTION PARAMETER บ
'ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ
   DIM DIO%(7)
   COMMON SHARED DIO%()
   COMMON SHARED FLAG%,FUN%

'ึฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤท
'บ INITIAL AX5210     บ
'ำฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฝ
	FUN%=INIT%		     ' Initial function
	DIO%(0)=IOPORT% 	     ' Ioport set
	DIO%(1)=3		     ' IRQ NO.
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " DRIVER INITIALIZATION FAILED ,flag=####";flag%

'ึฤฤฤฤฤฤฤฤฤฤฤท
'บ Digit out บ
'ำฤฤฤฤฤฤฤฤฤฤฤฝ
	PRINT "Please connect digit out channels to digit in ones repectively"
	INPUT "Key in digit out pattern(&H00 - &HFF)?",PAT%
	FUN%= DO.BYTE%		     ' Digit out function
	DIO%(0)=PAT%		     ' Digit out pattern
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " Digit out error ,flag=##";FLAG%
	PRINT USING "Digit OUT = \  \ ";HEX$(DIO%(0))

'ึฤฤฤฤฤฤฤฤฤฤท
'บ DIGIT IN บ
'ำฤฤฤฤฤฤฤฤฤฤฝ
	FUN%= DI.BYTE%		     ' Digit in function
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " Digit in error ,flag=##";FLAG%
	PRINT USING "Digit in  = \  \ ";HEX$(DIO%(0))
END
