'��������������������������������������������������������������������ͻ
'�  Program  : SFT_TRG.BAS                                            �
'�  Revision : 1.0                                                    �
'�  Date     : 11/10/90                        Axiom Tech. Co., Ltd.  �
'��������������������������������������������������������������������Ķ
'�  Purpose  : This demo program perform one channel A/D conversion   �
'�             with software trigger by function INIT%,SET.CH%,       �
'�             SET.GAIN%,SFT.TRG%.                                    �
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
	FUN%=INIT%		' Initial function
	DIO%(0)=IOPORT% 	' Ioport set
	DIO%(1)=3		' IRQ NO.
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " DRIVER INITIALIZATION FAILED ,flag=####";FLAG%

'��������������������������ķ
'� SET START & STOP CHANNEL �
'��������������������������Ľ
	INPUT	"ENTER START CHANNEL NUMBER <S/E--0...15>",STR%
	INPUT	"ENTER STOP  CHANNEL NUMBER <S/E--0...15>",STP%
	FUN%=SET.CH%		' Set channel function
	DIO%(0)=STR%		' Start channel set
	DIO%(1)=STP%		' Stop  channel set
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT "SET SCAN CHANNEL FAILED ! RE-ENTER"

'��������������������������ķ
'� SET GAIN FOR ALL CHANNEL �
'��������������������������Ľ
	FOR I%=0 to 15
	    FUN%=Set.gain%	' Set gain function
	    DIO%(0)=I%		' Set channel
	    DIO%(1)=1		' Set gain
	    CALL AX5210(fun%,dio%(0),FLAG%)
	    IF FLAG% <> 0 THEN _
	       PRINT "SET GAIN FAILED ! RE-ENTER"
	NEXT I%

'������������������ķ
'� SOFTWARE TRIGGER �
'������������������Ľ
	WHILE (1)
	   FUN%=SFT.TRG%	' Software trigger function
	   CALL AX5210(FUN%,DIO%(0),FLAG%)
	   IF FLAG% <> 0 THEN _
	      PRINT "A/D CONVERSION FAILED !"
	   PRINT USING " CHANNEL ##  READING ##### " ;DIO%(1);DIO%(0)
	   INPUT " PRESS ENTER TO CONVERT NEXT CHANNEL, <e> TO EXIT ",A$
	   IF ((A$ = "E") OR (A$ = "e")) THEN END
	WEND
