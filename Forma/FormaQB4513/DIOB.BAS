'浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
'�  Program  : DIOB.BAS                                               �
'�  Revision : 1.0                                                    �
'�  Date     : 11/10/90                        Axiom Tech. Co., Ltd.  �
'把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
'�  Purpose  : This demo program perform 8 bits Digit out and         �
'�             Digit in by function INIT%,DO.BYTE%,DI.BYTE%.          �
'�  Remark   : Please connect DO channels to DI'S respectively        �
'藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕

'崢陳陳陳陳陳陳陳陳陳陳陳陳�
'� FUNCTION NO. DEFINITION �
'喞陳陳陳陳陳陳陳陳陳陳陳陳�
   CONST INIT%	   = 0,SET.CH%	 = 1,SET.GAIN% = 2,SFT.TRG% = 3,NADC.ARY% = 4
   CONST INTR%	   = 5,DIS.INTR% = 6,READ.ST%  = 7,TRAN.DAT%= 8
   CONST SET.TIMER%= 9,TRG.SLOPE%=10,DO.BYTE%  =11,DI.BYTE% =12,NADCG.INT%=13
'崢陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
'� SUB FUNCTION NO. DEFINITION �
'喞陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
   CONST RISE.EDGE%= 0,FALL.EDGE%= 1,EXT.TRG%  = 0,TIMER.TRG%= 1
   CONST ONE.CYC%  = 0,RE.CYC%	 = 1
'崢陳陳陳陳陳陳陳跳
'� AX5210 IO PORT �
'喞陳陳陳陳陳陳陳捗
   CONST IOPORT%  =&H300
'崢陳陳陳陳陳陳陳陳陳跳
'� FUNCTION PARAMETER �
'喞陳陳陳陳陳陳陳陳陳捗
   DIM DIO%(7)
   COMMON SHARED DIO%()
   COMMON SHARED FLAG%,FUN%

'崢陳陳陳陳陳陳陳陳陳跳
'� INITIAL AX5210     �
'喞陳陳陳陳陳陳陳陳陳捗
	FUN%=INIT%		     ' Initial function
	DIO%(0)=IOPORT% 	     ' Ioport set
	DIO%(1)=3		     ' IRQ NO.
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " DRIVER INITIALIZATION FAILED ,flag=####";flag%

'崢陳陳陳陳陳�
'� Digit out �
'喞陳陳陳陳陳�
	PRINT "Please connect digit out channels to digit in ones repectively"
	INPUT "Key in digit out pattern(&H00 - &HFF)?",PAT%
	FUN%= DO.BYTE%		     ' Digit out function
	DIO%(0)=PAT%		     ' Digit out pattern
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " Digit out error ,flag=##";FLAG%
	PRINT USING "Digit OUT = \  \ ";HEX$(DIO%(0))

'崢陳陳陳陳跳
'� DIGIT IN �
'喞陳陳陳陳捗
	FUN%= DI.BYTE%		     ' Digit in function
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " Digit in error ,flag=##";FLAG%
	PRINT USING "Digit in  = \  \ ";HEX$(DIO%(0))
END
