'浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
'�  Program  : INTR.BAS                                               �
'�  Revision : 1.0                                                    �
'�  Date     : 11/10/90                        Axiom Tech. Co., Ltd.  �
'把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
'�  Purpose  : This demo program perform N channel A/D conversion with�
'�             Interrupt by function INIT%,SET.CH%,SET.GAIN%,         �
'�             SET.TIMER%,INTR%,READ.ST%,DIS.INTR%,TRAN.DAT%.         �
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
   DIM CH%(2000),DAT%(2000)

'崢陳陳陳陳陳陳陳陳陳跳
'� INITIAL AX5210     �
'喞陳陳陳陳陳陳陳陳陳捗
	FUN%=INIT%		' Initial function
	DIO%(0)=IOPORT% 	' Ioport set
	DIO%(1)=3		' IRQ NO.
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
		 PRINT USING " DRIVER INITIALIZATION FAILED ,flag=####";FLAG%

'崢陳陳陳陳陳陳陳陳陳陳陳陳跳
'� SET START & STOP CHANNEL �
'喞陳陳陳陳陳陳陳陳陳陳陳陳捗
	INPUT	"ENTER START CHANNEL NUMBER <S/E--0...15>",STR%
	INPUT	"ENTER STOP  CHANNEL NUMBER <S/E--0...15>",STP%

	FUN%=SET.CH%		' Set channel function
	DIO%(0)=STR%		' Start channel set
	DIO%(1)=STP%		' Stop  channel set
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN PRINT "SET SCAN CHANNEL FAILED ! RE-ENTER"
'崢陳陳陳陳陳陳陳陳陳陳陳陳跳
'� SET GAIN FOR ALL CHANNEL �
'喞陳陳陳陳陳陳陳陳陳陳陳陳捗
	FOR I%=0 to 15
	    FUN%=Set.gain%	' Set gain function
	    DIO%(0)=I%		' Set channel
	    DIO%(1)=1		' Set gain
	    CALL AX5210(fun%,dio%(0),FLAG%)
	    IF FLAG% <> 0 THEN PRINT "SET GAIN FAILED ! RE-ENTER"
	NEXT I%

'崢陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳跳
'�  SET TIMER BEFORE INTERRUPT (MAX RATE=10k) �
'喞陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳捗
	FUN%=SET.TIMER% 	' Set timer rate = 1M/(divisor1 * divisor2)
	DIO%(0)=50		' Divisor 1
	DIO%(1)=10		' Divisor 2
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN PRINT "Set Timer error  flag=####",FLAG%

'崢陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳跳
'� ENABLE INTERRUPT ACCORDING TO TIMER RATE �
'喞陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳捗
	CNT%=1024
	FUN%=INTR%		' Interrupt function
	DIO%(0)=CNT%		' Count
	DIO%(1)=&H8000		' Memory segment
	DIO%(2)=TIMER.TRG%	' Pacer trigger
	DIO%(3)=ONE.CYC%	' One cycle
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN PRINT "INTERRUPT FAILED FLAG= ####";FLAG%

'崢陳陳陳陳陳陳陳陳陳陳跳
'� READ INTERRUPT COUNT �
'喞陳陳陳陳陳陳陳陳陳陳捗
	WHILE (1)
	    fun%=READ.ST%	' Read Interrupt count function
	    CALL AX5210(fun%,dio%(0),FLAG%)
	    LOCATE 1,50: PRINT "under testing"
	    IF DIO%(1) = 0 THEN GOTO COUNT.EXIT
	WEND

COUNT.EXIT:
'崢陳陳陳陳陳陳陳陳陳�
'� DISABLE INTERRUPT �
'喞陳陳陳陳陳陳陳陳陳�
	FUN%=DIS.INTR%		 ' Disable interrupt
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN PRINT "INTERRUPT DISABLE FAIL"

'崢陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳跳
'� TRANSFER DATA FROM MEMORY TO ARRAY �
'喞陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳捗
	FUN%=TRAN.DAT%			' Data transfer function
	DIO%(0)=CNT%			' Count
	DIO%(1)=&H8000			' Data memory segment
	DIO%(2)=0			' Starting data number
	DIO%(3) = VARPTR(DAT%(0))	' Get data array
	DIO%(4) = VARPTR(CH%(0))	' Get channel array
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN PRINT "data transfer error ! "

'崢陳陳陳陳陳陳陳陳陳陳陳跳
'� DISPLAY CHANNEL & DATA �
'喞陳陳陳陳陳陳陳陳陳陳陳捗
	LOCATE 3,1
	FOR I=CNT%-40 TO (CNT%-1)
	    PRINT USING " ch=##, data=#####";CH%(I);DAT%(I)
	NEXT
END
