'имммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╩
'╨  Program  : THERMO.BAS                                             ╨
'╨  Revision : 1.0                                                    ╨
'╨  Date     : 11/10/90                        Axiom Tech. Co., Ltd.  ╨
'гдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╤
'╨  Purpose  : This demo program perform measuring thermocouple       ╨
'╨             with AX5210 and AX752.                                 ╨
'хмммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммммм╪

'жддддддддддддддддддддддддд╥
'╨ FUNCTION NO. DEFINITION ╨
'сддддддддддддддддддддддддд╫
   CONST INIT%	   = 0,SET.CH%	 = 1,SET.GAIN% = 2,SFT.TRG% = 3,NADC.ARY% = 4
   CONST INTR%	   = 5,DIS.INTR% = 6,READ.ST%  = 7,TRAN.DAT%= 8
   CONST SET.TIMER%= 9,TRG.SLOPE%=10,DO.BYTE%  =11,DI.BYTE% =12,NADCG.INT%=13
'жддддддддддддддддддддддддддддд╥
'╨ SUB FUNCTION NO. DEFINITION ╨
'сддддддддддддддддддддддддддддд╫
   CONST RISE.EDGE%= 0,FALL.EDGE%= 1,EXT.TRG%  = 0,TIMER.TRG%= 1
   CONST ONE.CYC%  = 0,RE.CYC%	 = 1
'ждддддддддддддддд╥
'╨ AX5210 IO PORT ╨
'сдддддддддддддддд╫
   CONST IOPORT%  =&H300
'ждддддддддддддддддддд╥
'╨ FUNCTION PARAMETER ╨
'сдддддддддддддддддддд╫
   DIM DIO%(7)
   COMMON SHARED DIO%()
   COMMON SHARED FLAG%,FUN%
   DIM F.TC.R(6,2),F.TC.P(6,9),TC(2)

'жддддддддддддддддддд╥
'╨ read tc temp data ╨
'сддддддддддддддддддд╫
	GOSUB READ.TC.TEMP.DATA

'ждддддддддддддддддддд╥
'╨ INITIAL AX5210     ╨
'сдддддддддддддддддддд╫
	FUN%=INIT%		' Initial function
	DIO%(0)=IOPORT% 	' Ioport set
	DIO%(1)=3		' IRQ NO.
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN _
	   PRINT USING " DRIVER INITIALIZATION FAILED ,flag=####";FLAG%
'жддддддддддддддддддддд╥
'╨ DESCRIPTION         ╨
'сддддддддддддддддддддд╫
     print
     print  "1. The demo program performs measuring thermocouple with AX5210 and AX752."
     print  "2. Please connect AX5210 CN1 TO AX752 CNA .      "
     print  "3. Please set CJC to channel 0 on JP1 on AX752.      "
     print  "4. Please set OUT to channel 1 on JP2 on AX752.      "
     print  "5. Please set GAIN on JP4 on AX752, say, GAIN=200 for k type TC. ."
     print  "=========================================================================="

'ждддддддддддддддддддддддддд╥
'╨ SET START & STOP CHANNEL ╨
'сдддддддддддддддддддддддддд╫
	FUN%=SET.CH%		' Set channel function
	DIO%(0)=0		' Start channel set
	DIO%(1)=1		' Stop  channel set
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN PRINT "SET SCAN CHANNEL FAILED ! RE-ENTER"

'ждддддддддддддддддддддддддддддддддддд╥
'╨ SET GAIN FOR CHANNEL 0 & CHANNEL 1 ╨
'сдддддддддддддддддддддддддддддддддддд╫
	FOR I%=0 to 1
	    FUN%=Set.gain%	' Set gain function
	    DIO%(0)=I%		' Set channel
	    DIO%(1)=1		' Set gain
	    CALL AX5210(fun%,dio%(0),FLAG%)
	    IF FLAG% <> 0 THEN PRINT "SET GAIN FAILED ! RE-ENTER"
	NEXT I%


'ждддддддддддддддддддддддддддддддддддддд╥
'╨ SELECT THERMOCOUPLE CHANNEL IN AX752 ╨
'сдддддддддддддддддддддддддддддддддддддд╫
	INPUT	"ENTER THERMOCOUPLE CHANNEL ON AX752 <0.....15>",TC.CH%
	INPUT	"ENTER GAIN ON AX752 <10, 50, 100, 200, 1000> ",PRE.GAIN%
	INPUT	"ENTER THERMOCOUPLE TYPE <J,K,T,E,R,S>",F.A$
	FUN%=DO.BYTE%		    ' Digit out
	DIO%(0)=TC.CH%
	CALL AX5210(FUN%,DIO%(0),FLAG%)
	IF FLAG% <> 0 THEN PRINT "DIGIT OUT FAILED ! RE-ENTER"

'ждддддддддддддддддд╥
'╨ SOFTWARE TRIGGER ╨
'сдддддддддддддддддд╫
	WHILE (1)
	   FOR I%=0 TO 1
	       FUN%=SFT.TRG%  ' software trigger function
	       CALL AX5210(FUN%,DIO%(0),FLAG%)
	       IF FLAG% <> 0 THEN PRINT "A/D CONVERSION FAILED !"
	       IF DIO%(1)=0 THEN
		  LOCATE 12,1
		  PRINT USING " COLD JUNCTION : CHANNEL ##  READING ##### " _
			      ;DIO%(1);DIO%(0)
	       ELSE
		  LOCATE 13,1
		  PRINT USING " THERMOCOUPLE  : CHANNEL ##  READING ##### " _
			      ;DIO%(1);DIO%(0)
	       END IF
	       TC(DIO%(1))=DIO%(0)
	   NEXT I%
	   GOSUB CALTMP1
	   LOCATE 14,1
	   PRINT USING " TEMPERATURE   : #####.#### ";F.DATA
	   PRINT " PRESS ANY KEY TO EXIT "
	   A$=INKEY$
	   IF LEN(A$) <> 0 THEN END
	WEND

'========
CALTMP1:
'========
'ждддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╥
'╨ TC temp (JKTERS)                                         ╨
'╨ COLD JUNCTION TEMPERATURE  DEGREE :F.T0 = Vc/(24.44mV/C) ╨
'сдддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╫
	F.T0=(TC(0)/2048*5)/0.0244
	IF INSTR(F.A$,"J")<>0 OR INSTR(F.A$,"j")<>0 THEN F.R%=0
	IF INSTR(F.A$,"K")<>0 OR INSTR(F.A$,"k")<>0 THEN F.R%=1
	IF INSTR(F.A$,"T")<>0 OR INSTR(F.A$,"t")<>0 THEN F.R%=2
	IF INSTR(F.A$,"E")<>0 OR INSTR(F.A$,"e")<>0 THEN F.R%=3
	IF INSTR(F.A$,"R")<>0 OR INSTR(F.A$,"r")<>0 THEN F.R%=4
	IF INSTR(F.A$,"S")<>0 OR INSTR(F.A$,"s")<>0 THEN F.R%=5
	' -- Thermpcouple temperature degree : F.ANS = Vm / (GAIN OF AX752)
	F.ANS=(TC(1)/2048*5)/PRE.GAIN%
	IF F.ANS>100 OR F.ANS<-100 THEN F.DATA=9999:RETURN
	' -- Calculate thermocouple degree
	YS.V1=F.TC.R(F.R%,0)+F.T0*(F.TC.R(F.R%,1)+F.T0*F.TC.R(F.R%,2))+F.ANS
	YS.T1=YS.V1*(F.TC.P(F.R%,5)+YS.V1*(F.TC.P(F.R%,6)+_
	   YS.V1*(F.TC.P(F.R%,7)+YS.V1*(F.TC.P(F.R%,8)+YS.V1*F.TC.P(F.R%,9)))))
	YS.T2=F.TC.P(F.R%,0)+YS.V1*(F.TC.P(F.R%,1)+YS.V1*_
	   (F.TC.P(F.R%,2)+YS.V1*(F.TC.P(F.R%,3)+YS.V1*(F.TC.P(F.R%,4)+YS.T1))))
	F.DATA=INT(YS.T2*100+.5)/100
	RETURN

'==================
READ.TC.TEMP.DATA:
'==================
'жддддддддддддддддддд╥
'╨ read tc temp data ╨
'сддддддддддддддддддд╫
    ' --J type
      F.TC.R(0,0)=-.00000075004344#   : F.TC.R(0,1)=.0000505321995#
      F.TC.R(0,2)=2.348050017D-08     : F.TC.P(0,0)=-.3595568424#
      F.TC.P(0,1)=19750.87948#	      : F.TC.P(0,2)=-175116.54251252#
      F.TC.P(0,3)=18212965.58#	      : F.TC.P(0,4)=-2831128435#
      F.TC.P(0,5)=271508383300#       : F.TC.P(0,6)=-13801412100000#
      F.TC.P(0,7)=379243843260000#    : F.TC.P(0,8)=-5371925517000000#
      F.TC.P(0,9)=3.0840255439D+16
    ' --K type
      F.TC.R(1,0)=-.0000008167743593# : F.TC.R(1,1)=.00003963927586#
      F.TC.R(1,2)=1.640341085D-08     : F.TC.P(1,0)=-.05115307103#
      F.TC.P(1,1)=24850.28007#	      : F.TC.P(1,2)=-382662.2822#
      F.TC.P(1,3)=99661056.73#	      : F.TC.P(1,4)=-10820623570#
      F.TC.P(1,5)=603928552400#       : F.TC.P(1,6)=-19108999620000#
      F.TC.P(1,7)=347823473000000#    : F.TC.P(1,8)=-3399102821000000#
      F.TC.P(1,9)=1.382851398D+16
    ' --T type
      F.TC.R(2,0)=.000000525792984#   : F.TC.R(2,1)=.00003860071243#
      F.TC.R(2,2)=4.186486602D-08     : F.TC.P(2,0)=.1238117795#
      F.TC.P(2,1)=26861.17637#	      : F.TC.P(2,2)=-896494.288#
      F.TC.P(2,3)=-46489260.88#       : F.TC.P(2,4)=12441142450#
      F.TC.P(2,5)=2275304922000#      : F.TC.P(2,6)=-639949686700000#
      F.TC.P(2,7)=5.435757807D+16     : F.TC.P(2,8)=-2.02361537D+18
      F.TC.P(2,9)=2.830121167D+19
    ' --E type
      F.TC.R(3,0)=.0000001237730126#  : F.TC.R(3,1)=.00005866024018#
      F.TC.R(3,2)=4.545892772D-08     : F.TC.P(3,0)=.2146439563#
      F.TC.P(3,1)=16129.94414#	      : F.TC.P(3,2)=-205410.8711#
      F.TC.P(3,3)=30494320.13#	      : F.TC.P(3,4)=-2969024265#
      F.TC.P(3,5)=142849580700#       : F.TC.P(3,6)=-3722194380000#
      F.TC.P(3,7)=53981272330000#     : F.TC.P(3,8)=-410552947700000#
      F.TC.P(3,9)=1277746629000000#
    ' --R type
      F.TC.R(4,0)=-.0000002112842336# : F.TC.R(4,1)=.000005334025378#
      F.TC.R(4,2)=1.195845433D-08     : F.TC.P(4,0)=-1.468841678#
      F.TC.P(4,1)=172645.1124#	      : F.TC.P(4,2)=-32766927.31#
      F.TC.P(4,3)=7787787267#	      : F.TC.P(4,4)=-1139825933000#
      F.TC.P(4,5)=101396298600000#    : F.TC.P(4,6)=-5484260171000000#
      F.TC.P(4,7)=1.74144946D+17      : F.TC.P(4,8)=-2.92355047D+18
      F.TC.P(4,9)=1.928074475D+19
    ' --S type
      F.TC.R(5,0)=-.0000002019201233# : F.TC.R(5,1)=.000005442370913#
      F.TC.R(5,2)=1.066719241D-08     : F.TC.P(5,0)=-1.374980264#
      F.TC.P(5,1)=173544.5798#	      : F.TC.P(5,2)=-32625706.06#
      F.TC.P(5,3)=8477335148#	      : F.TC.P(5,4)=-1327039894000#
      F.TC.P(5,5)=123611232200000#    : F.TC.P(5,6)=-6792674872000000#
      F.TC.P(5,7)=2.068929393D+17     : F.TC.P(5,8)=-2.887484948D+18
      F.TC.P(5,9)=8.266372801D+18
      RETURN
