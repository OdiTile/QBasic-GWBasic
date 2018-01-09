'��������������������������������������������������������������������ͻ
'�  Program  : DIOB.BAS                                               �
'�  Revision : 1.0                                                    �
'�  Date     : 11/10/90                        Axiom Tech. Co., Ltd.  �
'��������������������������������������������������������������������Ķ
'�  Purpose  : This demo program perform 8 bits Digit out and         �
'�             Digit in by function INIT%,DO.BYTE%,DI.BYTE%.          �
'�  Remark   : Please connect DO channels to DI'S respectively        �
'��������������������������������������������������������������������ͼ

'�������������������������ķ
'� FUNCTION NO. DEFINITION �
'�������������������������Ľ
   CONST INIT%	   = 0,SET.CH%	 = 1,SET.GAIN% = 2,SFT.TRG% = 3,NADC.ARY% = 4
   CONST INTR%	   = 5,DIS.INTR% = 6,READ.ST%  = 7,TRAN.DAT%= 8
   CONST SET.TIMER%= 9,TRG.SLOPE%=10,DO.BYTE%  =11,DI.BYTE% =12,NADCG.INT%=13
'�����������������������������ķ
'� SUB FUNCTION NO. DEFINITION �
'�����������������������������Ľ
   CONST RISE.EDGE%= 0,FALL.EDGE%= 1,EXT.TRG%  = 0,TIMER.TRG%= 1
   CONST ONE.CYC%  = 0,RE.CYC%	 = 1
'����������������ķ
'� AX5210 IO PORT �
'����������������Ľ
   CONST IOPORT%  =&H300
'��������������������ķ
'� FUNCTION PARAMETER �
'��������������������Ľ
   DIM DIO%(7)
   COMMON SHARED DIO%()
   COMMON SHARED FLAG%,FUN%

'��������������������ķ
'� INITIAL AX5210     �
'��������������������Ľ
	FUN%=INIT%		     ' Initial function
	DIO%(0)=IOPORT% 	     ' Ioport set
	DIO%(1)=3		     ' IRQ NO.
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " DRIVER INITIALIZATION FAILED ,flag=####";flag%

'�����������ķ
'� Digit out �
'�����������Ľ
	PRINT "Please connect digit out channels to digit in ones repectively"
	INPUT "Key in digit out pattern(&H00 - &HFF)?",PAT%
	FUN%= DO.BYTE%		     ' Digit out function
	DIO%(0)=PAT%		     ' Digit out pattern
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " Digit out error ,flag=##";FLAG%
	PRINT USING "Digit OUT = \  \ ";HEX$(DIO%(0))

'����������ķ
'� DIGIT IN �
'����������Ľ
	FUN%= DI.BYTE%		     ' Digit in function
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " Digit in error ,flag=##";FLAG%
	PRINT USING "Digit in  = \  \ ";HEX$(DIO%(0))
END
