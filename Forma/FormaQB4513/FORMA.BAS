'--------------------------------- F o r m a -----------------------------
'Programma controllo stampatrice   ver 4.04
'copyright 1994         hardware - software       - STAVIT Snc -
'revisione 11-02-1994: modifica algoritmo fermo macchina
'revisione 06-04-1994: inserimento grafici
'revisione 09-05-1994: menu esterno, stampatrici diverse
'revisione 05-09-1994: rientro in funzione notte, correzione tempo lavoro
'                      lim intermedio variabile, scala grafica continua
'revisione 03-03-1995: autoscala grafico trend; in corso d'opera...
'
'Data division: dichiarazione delle variabili
DECLARE SUB grafo (dat%())
DECLARE SUB settavariabili (dummy1%)
CONST IOPORT% = &H300   'indirizzo della scheda interfaccia
DIM DIO%(7)             'vettore parametri passati alla scheda
COMMON SHARED DIO%()
COMMON SHARED FLAG%, FUN% ' flag%=ritorno tipo errore, fun%=parametro scheda
DIM CH%(2000), dat%(350) 'ch% = canale,   dat%=letture consecutive
COMMON SHARED CH%(), dat%()
COMMON SHARED numlett%, mass1%, mass2%, limite11%, limite21%
COMMON SHARED maxprep%, maxfin%, tmenu%
DIM analisi%(650)       'array analisi temporale sul finitore
COMMON SHARED analisi%()
d% = 0                  'porre a 1 per attivare debug
'contatori              i contatori limite a 0 sono esclusi automaticamente
conta01! = 0            'contatore principale  (fino a 3,4x10^38)
conta02! = 0            'limite di arresto da contatore principale
conta11& = 0            'contatore punzone     (fino a 2147483647)
conta12& = 0            'limite punzone
conta21& = 0            'contatore matrice
conta22& = 0            'limite matrice
conta31& = 0            'contatore lotto
conta32& = 0            'limite lotto
conta41& = 0            'contatore turno ausiliario
conta42& = 0            'limite turno ausiliario

'variabili di processo
mass1% = 0              'livello picco segnale 1    ( fino a 32767)
mass2% = 0              'livello picco segnale 2
media1% = 0             'valore medio 1
media2% = 0             'valore medio 2
rif1% = 0               'livello riferimento 1
rif2% = 0               'livello riferimento 2
stab1% = 10             'stabilita': si ha intervento se il segnale esce da
			' +-10% (rif) per  (conta51) volte successive
stab2% = 10
limite1% = 100          'interv. piccoli scostamenti= 100 conta51 conta61
limite2% = 200          'interv. grandi scostamenti=200 1 colpo
limite3% = 400          'scostamento assoluto
conta51% = 0            'contatore piccoli scostamenti consecutivi 1
conta52% = 5            'limite scostamenti consecutivi 1
conta61% = 0            'contatore piccoli scostamenti consecutivi 2
conta62% = 5            'limite scostamenti consecutivi 2
conta71% = 0            'contatore autoapprendimento
conta72% = 10           'limite autoapprendimento
conta81% = 0            'numero scostamenti preparatore
conta811% = 10          'piccoli scostamenti
conta812% = 5           'grandi scostamenti
conta813% = 3           'scostamenti assoluti
conta82% = 0            'numero scostamenti finitore
conta821% = 10          'piccoli scostamenti
conta822% = 5           'grandi scostamenti
conta823% = 3           'scostamenti assoluti
tempo11& = 0            'temporizzatore eventi
tempo12& = 5            'limite temporizzatore eventi (watchdog)
tempo21& = 0            'tempo turno ausiliario in secondi
tempo22& = 0            'limite intervento turno ausiliario (0 escluso) sec.
tempo23$ = ""           'tempo21& tradotto in ore e minuti
stato% = 0              '0 escluso 1 min.      1 funzione giorno
			'2 funzione notte      3 manuale 15 minuti
ritlett1% = 0           'ritardo inizio lettura preparatore
ritlett2% = 0           'ritardo inizio lettura finitore
limite3% = 400          'scostamento assoluto
tipomacc% = 1           ' 1=TP2  2=873  3=635/1 ....
numlett% = 300          'se si varia bisogna ridimensionare  DIM dat% (...)
			'max 350 letture
numsalvadati% = 100     'salva dati su disco ogni 100 colpi
integra% = 2            'colpi contabilizzati durante il salvataggio
ampli1% = 1             'amplificazione preparatore
ampli2% = 1             'amplificazione finitore
flett% = 8              'frequenza letture
delayfilo% = 1000       'ritardo tra stacco filo e ferma macchina
dummy1% = 0: limite12% = 100: limite22% = 100
			'dummy4% = 0: dummy5% = 0: dummy6% = 0  -variate-
spot% = 1               'spot=0 legge max spot=1 legge media di 3
'spotpre% = 150          'punto lettura spot preparatore su 300
'spotfin% = 150          'punto lettura spot finitore su 300
dummy7% = 0: dummy8% = 0: dummy9% = 0
			'spazio per variabili future
'variabili non inserite nel file forma.dat
in1% = 0                'ingresso digitale 1
in2% = 0                'ingresso digitale 2
in3% = 0                'ingresso digitale 3
in4% = 0                'ingresso digitale 4
			'numero di letture per ogni segnale -attenzione-
numlett% = 300          'se si varia bisogna ridimensionare  DIM dat% (...)
ferma% = 0              'arresta la macchina se diverso da 0 (codice fermo)
			'1=scost. consec. 1        2=scost. consec. 2
			'3=grande scostam.1        4=grande scostam.2
			'5=scost. assoluto 1       6=scost. assoluto 2
			'7=timeout        1        8=timeout        2
			'9=contatore princ.        10=contatore punzone
			'11=contatore matrice      12=contatore lotto
			'13=contatore turno aux    14=tempo turno aux
			'15=supero tempo in menu
			'ecc.
limitepre1% = 0         'piccolo scostamento precedente 1 superato se = 1
limitepre2% = 0         'piccolo scostamento precedente 2 superato se = 1
limite01% = 0           'valore raggiunto dal preparatore
limite11% = 0           'valore di controllo preparatore
limite02% = 0           'valore raggiunto dal finitore
'limite12% = 0           'valore di controllo finitore  ANNULLATO
rif01% = 0              'riferimento assoluto senza inseguimento
rif02% = 0              'riferimento assoluto senza inseguimento
dummy% = 0              'variabile per usi vari
conta8% = 50            'ogni 50 colpi esegue autoinseguimento
conta9% = 100           'numero colpi salva dati su disco
'tempo24% = INT(TIMER / 60)      'var. locale tempo aux
tempo24& = TIMER                'var locale tempo ausiliario
'tempo241& = 0                   'var locale sec tempo aux    ANNULLATA
tempo3% = INT(TIMER / 60)       'tempo iniziale in stato%=0,3
sel$ = ""               'variabile locale menu scelta
inval$ = ""             'var locale input valori
disco% = 0              'disattiva interruzione quando scrive su disco
salvadati% = 0          '-da eliminare nel definitivo-
colpimin0& = 150        'colpi al minuto (su conta41&)
colpimin1& = 0          'dummy colpi al minuto
tempo4! = TIMER         'dummy tempo per colpi al minuto
tmenu% = 0              'tempo menu superato - valori come stato%
'legge i dati dal file forma.dat; se non esiste lo crea e setta i dati
ON ERROR GOTO gesterr   'codice file non trovato = 53
  OPEN "forma.dat" FOR INPUT AS #1        'se si effettua una modifica dei
  INPUT #1, conta01!, conta02!          'parametri cambiare anche in gesterr
  INPUT #1, conta11&, conta12&
  INPUT #1, conta21&, conta22&
  INPUT #1, conta31&, conta32&
  INPUT #1, conta41&, conta42&
  INPUT #1, conta51%, conta52%
  INPUT #1, conta61%, conta62%
  INPUT #1, conta71%, conta72%
  INPUT #1, media1%, media2%
  INPUT #1, rif1%, rif2%, stab1%, stab2%
  INPUT #1, limite1%, limite2%
  INPUT #1, tempo12&, tempo21&, tempo22&
  INPUT #1, conta811%, conta812%, conta813%
  INPUT #1, conta821%, conta822%, conta823%
  INPUT #1, ritlett1%, ritlett2%, limite3%
  INPUT #1, tipomacc%, numlett%, numsalvadati%
  INPUT #1, integra%, ampli1%, ampli2%
  INPUT #1, flett%, delayfilo%
  INPUT #1, dummy1%, limite12%, limite22%
  INPUT #1, spot%, spotpre%, spotfin%
  INPUT #1, dummy7%, dummy8%, dummy9%
  CLOSE #1
ON ERROR GOTO 0                 'chiude gestione errori
'
'Inizializzazione  ax5210
	DIO%(0) = IOPORT%       ' Ioport set  &H300
	DIO%(1) = 5             ' IRQ NO. ***** controllare ******
	CALL AX5210(0, DIO%(0), FLAG%)
SELECT CASE tipomacc%              'uscita digitale (in 4 punti del prog)
 CASE IS = 1                       'macchina TP2/CL
   DIO%(0) = 0                     '0 0 0 tutti i rele' sono spenti
   CALL AX5210(11, DIO%(0), FLAG%)
 CASE IS = 2                       'macchina 873/SV
   DIO%(0) = 7                     '1 1 1 tutti i rele attivati
   CALL AX5210(11, DIO%(0), FLAG%)
 'Aggiungere qui ulteriori casi
 CASE ELSE
   DIO%(0) = 0
   CALL AX5210(11, DIO%(0), FLAG%)
END SELECT
'
modificaparametri:
CALL settavariabili(dummy1%)             'modifica dati su forma.dat
    IF tmenu% = 2 THEN
      stato% = 2                         'funzione notte -> ferma tutto
      ferma% = 15                        'fermo per supero tempo in menu
      GOSUB fermamacchina
    END IF
    stato% = 0                           'passa in manuale 1 minuto
    tmenu% = 0
    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
ON ERROR GOTO gesterr                    'codice file non trovato = 53
					 'legge i dati dal file forma.dat
  OPEN "forma.dat" FOR INPUT AS #1       'se si effettua una modifica dei
  INPUT #1, conta01!, conta02!           'parametri cambiare anche in gesterr
  INPUT #1, conta11&, conta12&
  INPUT #1, conta21&, conta22&
  INPUT #1, conta31&, conta32&
  INPUT #1, conta41&, conta42&
  INPUT #1, conta51%, conta52%
  INPUT #1, conta61%, conta62%
  INPUT #1, conta71%, conta72%
  INPUT #1, media1%, media2%
  INPUT #1, rif1%, rif2%, stab1%, stab2%
  INPUT #1, limite1%, limite2%
  INPUT #1, tempo12&, tempo21&, tempo22&
  INPUT #1, conta811%, conta812%, conta813%
  INPUT #1, conta821%, conta822%, conta823%
  INPUT #1, ritlett1%, ritlett2%, limite3%
  INPUT #1, tipomacc%, numlett%, numsalvadati%
  INPUT #1, integra%, ampli1%, ampli2%
  INPUT #1, flett%, delayfilo%
  INPUT #1, dummy1%, limite12%, limite22%
  INPUT #1, spot%, spotpre%, spotfin%
  INPUT #1, dummy7%, dummy8%, dummy9%
  CLOSE #1
  OPEN "analisi.dat" FOR INPUT AS #2
  FOR i = 0 TO 639
    INPUT #2, analisi%(i)
  NEXT
  CLOSE #2
  IF conta01! = 0 THEN                   'azzera dati del trend
      OPEN "analisi.dat" FOR OUTPUT AS #2  'in questo punto e ..
      FOR i = 0 TO 639                   '
	  PRINT #2, 100
      NEXT
      CLOSE #2
  END IF
ON ERROR GOTO 0             'chiude gestione errori
colpimin1& = conta41&       'prepara dummy colpi al minuto
'tabella codici scheda  0=inizializza         1=seleziona range canali
'                       2=gain per canale     3=singola conversione a/d
'                       4=n conversioni a/d   5=n conv. a/d + interrupt
'                       6=reset interrupt     7=stato dopo interrupt
'                       8=dati mem -> array   9=frequenza letture
'                      10=livello trigger    11=uscita digitale
'                      12=ingresso digitale  13=n a/d in background
'
'Inizializzazione  ax5210
	DIO%(0) = IOPORT%       ' Ioport set  &H300
	DIO%(1) = 5             ' IRQ NO. ***** controllare ******
	CALL AX5210(0, DIO%(0), FLAG%)
	LOCATE 25, 10
	'PRINT "Inizializzazione.....", FLAG%;
'        SLEEP (1)
'Seleziona solo il canale 0
	DIO%(0) = 0             ' Start channel set
	DIO%(1) = 0             ' Stop  channel set
	CALL AX5210(1, DIO%(0), FLAG%)
	LOCATE 25, 10
	'PRINT "Selezione canale 0...", FLAG%;
'        SLEEP (1)
'Imposta il guadagno per tutti i canali
IF d% THEN GOTO salta5          'attivo durante il debug
	FOR i% = 0 TO 15
	    DIO%(0) = i%        ' Set channel
	    DIO%(1) = ampli1%   ' Set gain
	    CALL AX5210(2, DIO%(0), FLAG%)
	LOCATE 25, 10
	'PRINT "Imposta il guadagno..", i%; FLAG%;
'       SLEEP (1)
	NEXT i%
SELECT CASE tipomacc%              'uscita digitale (in 4 punti del prog)
 CASE IS = 1                       'macchina TP2/CL
   DIO%(0) = 0                     '0 0 0 tutti i rele' sono spenti
   CALL AX5210(11, DIO%(0), FLAG%)
 CASE IS = 2                       'macchina 873/SV
   DIO%(0) = 7                     '1 1 1 tutti i rele attivati
   CALL AX5210(11, DIO%(0), FLAG%)
 'Aggiungere qui ulteriori casi
 CASE ELSE
   DIO%(0) = 0
   CALL AX5210(11, DIO%(0), FLAG%)
END SELECT
salta5:
'Imposta la frequenza di lettura al massimo
' SET TIMER BEFORE INTERRUPT (MAX RATE=10k)
				' Set timer rate = 1M/(divisor1 * divisor2)
	DIO%(0) = 10            ' Divisor 1   era 10
	DIO%(1) = flett%        ' Divisor 2 normalmente 8 range 2-65535
	CALL AX5210(9, DIO%(0), FLAG%)
	LOCATE 25, 10
	'PRINT "Selezione frequenza..", FLAG%;
	SLEEP (1)
	LOCATE 25, 1
	PRINT SPC(75);
'       SLEEP (1)
CLS
'GOSUB prefazione
'CALL settavariabili(dummy1%)  'modifica dati su forma.dat
autoapprendimento:            'preparazione delle variabili
conta71% = 0
IF conta71% < conta72% THEN   'conta71% conta72%
  in1% = 0                  'autoapprendimento preparatore (1)
  DO WHILE in1% = 0         'resta in attesa finche' arriva il prossimity 1
    CALL AX5210(12, DIO%(0), FLAG%)
    IF (DIO%(0) AND 1) <> 0 THEN in1% = 1
    IF d% THEN in1% = 1      'continua se si e' in debug
    ' in questo punto si devono inserire routine di gestione varie
  LOOP
  IF ritlett1% <> 0 THEN           'ritardo inziale prefissato
    FOR i = 1 TO ritlett1%: NEXT
  END IF
  DIO%(0) = numlett%        'numero letture o conversioni
  DIO%(1) = VARPTR(dat%(0)) 'puntatore array dati
  DIO%(2) = 1               'trigger interno temporizzato max velocita'
  DIO%(3) = VARPTR(CH%(0))  'puntatore array canali
  CALL AX5210(4, DIO%(0), FLAG%)
  IF d% THEN GOTO salto1    'continua se si e' in debug
  FOR i = 1 TO (numlett% - 1)
	IF ABS(dat%(i)) > mass1% THEN mass1% = ABS(dat%(i))
  NEXT i
salto1:
  IF conta71% = 0 THEN      'se primo dato
    rif1% = mass1%          'viene preso tale e quale
  ELSE
    rif1% = (rif1% + mass1%) / 2   'altrimenti e' la media
  END IF
'***********************************************************************
  in2% = 0                   'autoapprendimento finitore
  DO WHILE in2% = 0          'resta in attesa finche' arriva il prossimity 2
    CALL AX5210(12, DIO%(0), FLAG%)
    IF (DIO%(0) AND 2) <> 0 THEN in2% = 1
    IF d% THEN in2% = 1      'continua se si e' in debug
    ' in questo punto si devono inserire routine di gestione varie
  LOOP
  IF ritlett2% <> 0 THEN           'ritardo inziale prefissato
    FOR i = 1 TO ritlett2%: NEXT
  END IF
  DIO%(0) = numlett%        'numero letture o conversioni
  DIO%(1) = VARPTR(dat%(0)) 'puntatore array dati
  DIO%(2) = 1               'trigger interno temporizzato max velocita'
  DIO%(3) = VARPTR(CH%(0))  'puntatore array canali
  CALL AX5210(4, DIO%(0), FLAG%)
  IF d% THEN GOTO salto2    'continua se si e' in debug
  FOR i = 1 TO (numlett% - 1)
	IF ABS(dat%(i)) > mass2% THEN mass2% = ABS(dat%(i))
  NEXT i
salto2:
  IF conta71% = 0 THEN      'se primo dato
    rif2% = mass2%          'viene preso tale e quale
  ELSE
    rif2% = (rif2% + mass2%) / 2   'altrimenti e' la media
  END IF
  conta71% = conta71% + 1
END IF              ' if alla riga 129 pronti rif1% , mass1% , rif2%, mass2%
rif01% = rif1%
rif02% = rif2%
LOCATE 25, 1
PRINT "                       Premere un tasto per uscire     ";
stato% = 0                  'la prima volta che parte stato% = 0
tempo3% = INT(TIMER / 60)   'tempo iniziale in stato%=0
ciclocontrollo:             'ciclo di controllo principale
'***************************************************************************
in1% = 0       '*********************************ciclo preparatore**********
tempo11& = TIMER
'IF ampli1% <> 1 THEN
'            DIO%(0) = 0         ' Imposta l'amplificazione per
'            DIO%(1) = ampli1%   ' il canale 0 range 1,2,4,8,16
'            CALL AX5210(2, DIO%(0), FLAG%)
'END IF
DO WHILE in1% = 0     'resta in attesa finche' arriva il prossimity 1
  CALL AX5210(12, DIO%(0), FLAG%)
  IF (DIO%(0) AND 1) <> 0 THEN in1% = 1
  IF TIMER - tempo11& > tempo12& THEN ferma% = 7 ' timeout 1
  IF d% THEN in1% = 1      'continua se si e' in debug
  IF (INT(TIMER / 60) - tempo3%) > 1 THEN          'tempo 1 minuto
   IF stato% = 0 THEN stato% = 2                   'rientra in f. notte
  END IF
  IF (INT(TIMER / 60) - tempo3%) > 15 THEN          'tempo 15 minuti
   IF stato% = 3 THEN stato% = 2                    'rientra in f. notte
  END IF
 
  IF (TIMER - tempo4!) > 60 THEN   ' conta41& colpimin0& colpimin1& tempo4!
    colpimin0& = conta41& - colpimin1&
    colpimin1& = conta41&
    tempo4! = TIMER
  END IF
  in3$ = INKEY$                           'variabile locale che trattiene
  IF in3$ <> "" THEN GOSUB menu           'la iniziale inserita
'  IF INKEY$ <> "" THEN GOSUB menu
LOOP
  IF ritlett1% <> 0 THEN           'ritardo inziale prefissato
    FOR i = 1 TO ritlett1%: NEXT
  END IF
LOCATE 1, 14
PRINT "Preparatore..", CHR$(1)
DIO%(0) = numlett%          'numero letture o conversioni
DIO%(1) = VARPTR(dat%(0))   'puntatore array dati
DIO%(2) = 1                 'trigger interno temporizzato max velocita'
DIO%(3) = VARPTR(CH%(0))    'puntatore array canali
CALL AX5210(4, DIO%(0), FLAG%)
mass1% = 0
IF d% THEN GOTO salto3      'continua se si e' in debug
FOR i = 1 TO (numlett% - 1)
  IF ABS(dat%(i)) > mass1% THEN
    mass1% = ABS(dat%(i))
    maxprep% = i            'punto massimo preparatore
  END IF
NEXT i
salto3:
IF conta31& MOD conta8% = 0 THEN GOSUB inseguimento1    'aggiorna rif1%
' Uscita digitale. Confronta il mass% con rif% +- stab1% e segnala in uscita
' ferma% = 0
limite01% = ABS(mass1% - rif1%)
conta81% = conta81% + 1
limite11% = stab1%
'limite12% = stab1% + 50       ANNULLATO viene gestito a menu
SELECT CASE limite01%
  CASE IS >= limite3%
    IF conta81% > conta813% THEN       'verifica che lo scostamento non sia
       conta81% = 0                    'un disturbo accidentale
       ferma% = 5                      'supero valore assoluto per 3 volte
    END IF                             '
  CASE IS >= limite12%                 '
    IF conta81% > conta812% THEN       '  3  5  10
       conta81% = 0
       ferma% = 3
    END IF
  CASE IS >= limite11%
    IF conta81% > conta811% THEN
      conta81% = 0
      ferma% = 1
    END IF
  CASE ELSE
    conta81% = 0
END SELECT
'''''''''''''''''algoritmo sostituito'''''''''''''''''''
'limite11% = (rif1% * stab1%) / 100
'dummy% = ABS(limite01% - limite11%) + 100
'IF (ABS(mass1% - rif01%) - (rif01% * stab1%) / 100) >= limite3% THEN
'    IF limitepre1% = 1 THEN conta51% = conta51% + 1
'    limitepre1% = 1
'    IF conta51% >= conta52% THEN ferma% = 1   'ferma codice 1
'  IF conta81% = 0 THEN conta81% = 1  'verifica che lo scostamento non sia
'  IF conta81% <> 0 THEN               'un disturbo accidentale
'    conta81% = 0                     '
'    ferma% = 5                       'supero valore assoluto per 2 volte
'  END IF                             '
'ELSEIF limite01% <= limite11% THEN       'in tolleranza
'  limitepre1% = 0
'  conta51% = 0
'ELSEIF (dummy% >= limite1%) AND (dummy% < limite2%) THEN
' IF limitepre1% = 1 THEN conta51% = conta51% + 1
' limitepre1% = 1
' IF conta51% >= conta52% THEN
'   ferma% = 1                           'ferma codice 1
'   IF ((stato% = 1) OR (stato% = 2)) THEN GOSUB fermamacchina
' END IF
'ELSEIF dummy% >= (limite2% + 100) THEN  'correzione da valutare
'    ferma% = 3
'END IF


IF ((stato% = 1) OR (stato% = 2)) AND (ferma% <> 0) THEN GOSUB fermamacchina
LOCATE 1, 29
PRINT SPC(10);
LOCATE 1, 70                    'pulisce lampeggio errore
PRINT SPC(7);
LOCATE 2, 70
PRINT SPC(5);
LOCATE 5, 60                     'pulisce lampeggio cambio punzone
PRINT SPC(10);
LOCATE 6, 60                     'pulisce lampeggio cambio matrice
PRINT SPC(10);
LOCATE 7, 60                     'pulisce lampeggio fine lotto
PRINT SPC(10);
in2% = 0       '*********************************ciclo finitore**********
tempo11& = TIMER
'IF ampli2% <> 1 THEN
'            DIO%(0) = 0         ' Imposta l'amplificazione per
'            DIO%(1) = ampli2%   ' il canale 0 range 1,2,4,8,16
'            CALL AX5210(2, DIO%(0), FLAG%)
'END IF
DO WHILE in2% = 0     'resta in attesa finche' arriva il prossimity 2
  CALL AX5210(12, DIO%(0), FLAG%)
  IF (DIO%(0) AND 2) <> 0 THEN in2% = 1
  IF TIMER - tempo11& > tempo12& THEN ferma% = 8 ' timeout 2
  IF d% THEN in2% = 1      'continua se si e' in debug
  in3$ = INKEY$                           'variabile locale che trattiene
  IF in3$ <> "" THEN GOSUB menu           'la iniziale inserita
'  IF INKEY$ <> "" THEN GOSUB menu
  ' in questo punto si devono inserire routine di gestione varie
  ' ecc.
LOOP
  IF ritlett2% <> 0 THEN           'ritardo inziale prefissato
    FOR i = 1 TO ritlett2%: NEXT
  END IF
LOCATE 2, 14
PRINT "Finitore.....", CHR$(1);
DIO%(0) = numlett%          'numero letture o conversioni
DIO%(1) = VARPTR(dat%(0))   'puntatore array dati
DIO%(2) = 1                 'trigger interno temporizzato max velocita'
DIO%(3) = VARPTR(CH%(0))    'puntatore array canali
CALL AX5210(4, DIO%(0), FLAG%)
mass2% = 0
IF d% THEN GOTO salto4      'continua se si e' in debug
FOR i = 1 TO (numlett% - 1)
  IF ABS(dat%(i)) > mass2% THEN
    mass2% = ABS(dat%(i))
    maxfin% = i             'punto massimo finitore
  END IF
NEXT i
salto4:
IF conta31& MOD conta8% = 0 THEN GOSUB inseguimento2      'aggiorna rif2%
' Uscita digitale. Confronta il mass% con rif% +- stab2% e segnala in uscita
'stato% = 1         'ferma se esce dalla tolleranza
'ferma% = 0
limite02% = ABS(mass2% - rif2%)
conta82% = conta82% + 1
limite21% = stab2%
'limite22% = stab2% + 50               ANNULLATO viene gestito a menu'
SELECT CASE limite02%
  CASE IS >= limite3%
    IF conta82% > conta823% THEN       'verifica che lo scostamento non sia
       conta82% = 0                    'un disturbo accidentale
       ferma% = 6                      'supero valore assoluto per 3 volte
    END IF                             '
  CASE IS >= limite22%
    IF conta82% > conta822% THEN       '  3  5  10
       conta82% = 0
       ferma% = 4
    END IF
  CASE IS >= limite21%
    IF conta82% > conta821% THEN
      conta82% = 0
      ferma% = 2
    END IF
  CASE ELSE
    conta82% = 0
END SELECT
'''''''''''''''''algoritmo sostituito'''''''''''''''''''
'limite12% = (rif2% * stab2%) / 100
'dummy% = ABS(limite02% - limite12%) + 100
'IF (ABS(mass2% - rif02%) - (rif02% * stab2%) / 100) >= limite3% THEN
'  IF conta82% = 0 THEN conta82% = 1  'verifica che lo scostamento non sia
'  IF conta82% <> 0 THEN               'un disturbo accidentale
'    conta82% = 0                     '
'    ferma% = 6                       'supero valore assoluto per 2 volte
'  END IF                             '
'ELSEIF limite02% <= limite12% THEN       'in tolleranza
'  limitepre2% = 0
'  conta61% = 0
'ELSEIF (dummy% >= limite1%) AND (dummy% < limite2%) THEN
'  IF limitepre2% = 1 THEN conta61% = conta61% + 1
'  limitepre2% = 1
'  IF conta61% >= conta62% THEN
'    ferma% = 2                           'ferma codice 1
'    IF ((stato% = 1) OR (stato% = 2)) THEN GOSUB fermamacchina
'  END IF
'ELSEIF dummy% >= limite2% THEN
''  IF limitepre2% = 1 THEN conta61% = conta61% + 1
''  limitepre2% = 1
''  IF conta61% >= conta62% THEN ferma% = 2   'ferma codice 2
' IF conta51% >= conta52% THEN
'   ferma% = 1                           'ferma codice 1
'   IF ((stato% = 1) OR (stato% = 2)) THEN GOSUB fermamacchina
' END IF
'  ferma% = 4
'END IF
IF ((stato% = 1) OR (stato% = 2)) AND (ferma% <> 0) THEN GOSUB fermamacchina
LOCATE 2, 29
PRINT SPC(10);
GOSUB aggcontatori
'  IF RIGHT$(STR$(conta31&), 2) = "11" THEN
'             ' conta31& MOD 100 = 0 THEN   'porre a 1000 o 2000
'      salvadati% = salvadati% + 1      ' da eliminare -solo per prova-
'
  IF conta41& MOD numsalvadati% = 0 THEN  'era scritto MOD conta9%=0 =errore
'     LOCATE 20, 45         'lampeggia quando salva i dati su disco
'     PRINT CHR$(178);
     GOSUB salva            ' salva dati su disco
     disco% = 0
'     LOCATE 20, 44         'pulisce il lampeggio
'     PRINT "   ";
     conta01! = conta01! + integra% 'recupero del tempo perso nel salvataggio
     conta11& = conta11& + integra%
     conta21& = conta21& + integra%
     conta31& = conta31& + integra%
     conta41& = conta41& + integra%
  END IF
IF ferma% <> 0 THEN             'lampeggia
LOCATE 1, 70
PRINT STRING$(5, 178); ferma%
LOCATE 2, 70
PRINT STRING$(5, 178);
END IF
IF ((stato% = 1) OR (stato% = 2)) AND (ferma% <> 0) THEN GOSUB fermamacchina
disco% = disco% + 1
ferma% = 0                      'resetta causa di errore solo in questo punto
GOTO ciclocontrollo             'ripete continuamente il ciclo
'****************************************************************************
'****************************************************************************





'modifica variabili dall'utente

'subroutine varie
CLS
END
gesterr:                'subroutine gestione errori; manca file = 53
IF ERR = 53 THEN
  CLOSE #1
  OPEN "forma.dat" FOR OUTPUT AS #1
  PRINT #1, conta01!, conta02!          'prepara un file dati iniziale
  PRINT #1, conta11&, conta12&
  PRINT #1, conta21&, conta22&
  PRINT #1, conta31&, conta32&
  PRINT #1, conta41&, conta42&
  PRINT #1, conta51%, conta52%
  PRINT #1, conta61%, conta62%
  PRINT #1, conta71%, conta72%
  PRINT #1, media1%, media2%
  PRINT #1, rif1%, rif2%, stab1%, stab2%
  PRINT #1, limite1%, limite2%
  PRINT #1, tempo12&, tempo21&, tempo22&
  PRINT #1, conta811%, conta812%, conta813%
  PRINT #1, conta821%, conta822%, conta823%
  PRINT #1, ritlett1%, ritlett2%, limite3%
  PRINT #1, tipomacc%, numlett%, numsalvadati%
  PRINT #1, integra%, ampli1%, ampli2%
  PRINT #1, flett%, delayfilo%
  PRINT #1, dummy1%, limite12%, limite22%
  PRINT #1, spot%, spotpre%, spotfin%
  PRINT #1, dummy7%, dummy8%, dummy9%
  CLOSE #1
  CLOSE #2
  OPEN "analisi.dat" FOR OUTPUT AS #2
  FOR i = 0 TO 639
    PRINT #2, 0
  NEXT
  CLOSE #2
  RESUME 0
END IF
fermamacchina:
IF (disco% < 5) AND (tmenu% = 0) THEN RETURN  'disabilita quando scrive
					      'su disco se non errore menu
'SELECT CASE stato%
'  CASE IS = 1                   'ferma in funzione giorno
'   DIO%(0) = 1                  'attiva la uscita digitale 1
'   CALL AX5210(11, DIO%(0), FLAG%)
'  CASE IS = 2                   'ferma in funzione notte
'   DIO%(0) = 2                  'attiva la uscita digitale 2
'   CALL AX5210(11, DIO%(0), FLAG%)
'  CASE IS = 3
'   'siamo in manuale e non si fa niente
'END SELECT
SELECT CASE tipomacc%                'uscita digitale (in 4 punti)
 CASE IS = 1                         'macchina TP2/CL
   IF stato% = 1 THEN
     DIO%(0) = 1                     'ferma giorno
     CALL AX5210(11, DIO%(0), FLAG%)
   END IF
   IF stato% = 2 THEN
     DIO%(0) = 2                     'ferma notte
     CALL AX5210(11, DIO%(0), FLAG%)
   END IF
 CASE IS = 2                         'macchina 873/SV
   IF stato% = 1 THEN
     DIO%(0) = 3                     'stacca filo
     CALL AX5210(11, DIO%(0), FLAG%)
     FOR i = 1 TO delayfilo%: NEXT   'ritardo tra stacco filo e stop
     DIO%(0) = 1                     'ferma giorno
     CALL AX5210(11, DIO%(0), FLAG%)
   END IF
   IF stato% = 2 THEN
     DIO%(0) = 3                     'stacca filo
     CALL AX5210(11, DIO%(0), FLAG%)
     FOR i = 1 TO delayfilo%: NEXT   'ritardo tra stacco filo e stop
     DIO%(0) = 0                     'ferma notte
     CALL AX5210(11, DIO%(0), FLAG%)
   END IF
'
'                         Aggiungere qui ulteriori casi
'
 CASE ELSE
'   DIO%(0) = 0
'   CALL AX5210(11, DIO%(0), FLAG%)
END SELECT



LOCATE 24, 1
PRINT "         *********************   FERMO  MACCHINA  ********************";
LOCATE 25, 1
PRINT SPC(80);
LOCATE 25, 1
SELECT CASE ferma%
  CASE IS = 1
   espferma$ = "Fermo supero piccoli scostamenti sul preparatore"
  CASE IS = 2
   espferma$ = "Fermo supero piccoli scostamenti sul finitore"
  CASE IS = 3
   espferma$ = "Fermo per grande scostamento sul preparatore"
  CASE IS = 4
   espferma$ = "Fermo per grande scostamento sul finitore"
  CASE IS = 5
   espferma$ = "Fermo scostamento assoluto sul preparatore"
  CASE IS = 6
   espferma$ = "Fermo scostamento assoluto sul finitore"
  CASE IS = 7
   espferma$ = "Fermo per fouri tempo massimo preparatore"
  CASE IS = 8
   espferma$ = "Fermo per fuori tempo massimo finitore"
  CASE IS = 9
   espferma$ = "Fermo per raggiunto contatore principale"
  CASE IS = 10
   espferma$ = "Fermo per raggiunto contatore punzone"
  CASE IS = 11
   espferma$ = "Fermo per raggiunto contatore matrice"
  CASE IS = 12
   espferma$ = "Fermo per raggiunto contatore lotto"
  CASE IS = 13
   espferma$ = "Fermo per raggiunto contatore turno ausiliario"
  CASE IS = 14
   espferma$ = "Fermo per raggiunto contatore tempo ausiliario"
  CASE IS = 15
   espferma$ = "Fermo per supero tempo in modo menu"
  CASE ELSE
   espferma$ = ".............................................................."
END SELECT
PRINT SPC((80 - LEN(espferma$)) / 2); espferma$; SPC((80 - LEN(espferma$)) / 2);
LOCATE 25, 75: PRINT SPC(5);
ferma% = 0
IF (stato% = 1) OR (stato% = 2) THEN stato% = 0
tempo3% = INT(TIMER / 60)       'tempo iniziale in stato%=0
LOCATE 23, 22
DO
 INPUT ; "Premere il tasto Invio per ripartire...", inval$
LOOP UNTIL inval$ <> CHR$(13)
'DIO%(0) = 0                  'resetta tutte le uscite digitali
'CALL AX5210(11, DIO%(0), FLAG%)
SELECT CASE tipomacc%              'uscita digitale (in 4 punti del prog)
 CASE IS = 1                       'macchina TP2/CL
   DIO%(0) = 0                     '0 0 0 tutti i rele' sono spenti
   CALL AX5210(11, DIO%(0), FLAG%)
 CASE IS = 2                       'macchina 873/SV
   DIO%(0) = 7                     '1 1 1 tutti i rele attivati
   CALL AX5210(11, DIO%(0), FLAG%)
 'Aggiungere qui ulteriori casi
 CASE ELSE
   DIO%(0) = 0
   CALL AX5210(11, DIO%(0), FLAG%)
END SELECT
CLS
LOCATE 25, 1
PRINT "                       Premere un tasto per uscire                  ";
GOSUB salva             ' salva dat correnti
RETURN
aggcontatori:
conta01! = conta01! + 1
conta11& = conta11& + 1
conta21& = conta21& + 1
conta31& = conta31& + 1
conta41& = conta41& + 1
IF (conta01! >= conta02!) AND (conta02! <> 0) THEN ferma% = 9
'IF (conta11& >= conta12&) AND (conta12& <> 0) THEN ferma% = 10
'IF (conta21& >= conta22&) AND (conta22& <> 0) THEN ferma% = 11
'IF (conta31& >= conta32&) AND (conta32& <> 0) THEN ferma% = 12
IF (conta41& >= conta42&) AND (conta42& <> 0) THEN ferma% = 13
LOCATE 3, 1
PRINT STRING$(80, 205);
LOCATE 4, 1
PRINT "| 1  2| Contatore principale..."        '; conta01!, conta02!
LOCATE 4, 33: PRINT USING "########,"; conta01!
LOCATE 4, 46: PRINT USING "########,"; conta02!
LOCATE 5, 1
PRINT "| 3  4| Punzone - no stop -...."        '; conta11&, conta12&
LOCATE 5, 33: PRINT USING "########,"; conta11&
LOCATE 5, 46: PRINT USING "########,"; conta12&
IF conta11& > conta12& THEN
  LOCATE 5, 60                         'lampeggia cambio punzone
  IF conta12& <> 0 THEN PRINT STRING$(10, 178);
END IF
LOCATE 6, 1
PRINT "| 5  6| Matrice - no stop -...."         '; conta21&, conta22&
LOCATE 6, 33: PRINT USING "########,"; conta21&
LOCATE 6, 46: PRINT USING "########,"; conta22&
IF conta21& > conta22& THEN
  LOCATE 6, 60                         'lampeggia cambio matrice
  IF conta22& <> 0 THEN PRINT STRING$(10, 178);
END IF
LOCATE 7, 1
PRINT "| 7  8| Lotto   - no stop -...."         '; conta31&, conta32&
LOCATE 7, 33: PRINT USING "########,"; conta31&
LOCATE 7, 46: PRINT USING "########,"; conta32&
IF conta31& > conta32& THEN
  LOCATE 7, 60                         'lampeggia fine lotto
  IF conta32& <> 0 THEN PRINT STRING$(10, 178);
END IF
LOCATE 8, 1
PRINT "| 9 10| Contatore turno aux...."          '; conta41&, conta42&; ""
LOCATE 8, 33: PRINT USING "########,"; conta41&
LOCATE 8, 46: PRINT USING "########,"; conta42&
tempo23$ = STR$(INT(tempo21& / 3600)) '+ " :" + STR$(tempo21&/60)
LOCATE 9, 1
PRINT "|11 12| Tempo in minuti........" '; tempo21&/60, tempo22&/60, tempo23$; " ore"
LOCATE 9, 33: PRINT USING "########,"; tempo21& / 60
LOCATE 9, 46: PRINT USING "########,"; tempo22& / 60
LOCATE 9, 60: PRINT tempo23$; " ore        ";
LOCATE 10, 1
PRINT STRING$(80, 196);
LOCATE 11, 21: PRINT "Preparatore    Finitore      Ammessi"
LOCATE 12, 1: PRINT "Segnale        "; SPC(23);
LOCATE 12, 24: PRINT USING "####"; mass1%;
LOCATE 12, 38: PRINT USING "####"; mass2%
LOCATE 13, 1: PRINT "Scostamento    "; SPC(23);
LOCATE 13, 24: PRINT USING "####"; limite01%
LOCATE 13, 38: PRINT USING "####"; limite02%
LOCATE 14, 1: PRINT "Limite min     "; SPC(23);
LOCATE 14, 24: PRINT USING "####"; limite11%
LOCATE 14, 38: PRINT USING "####"; limite21%
LOCATE 14, 51: PRINT USING "####"; conta811%
LOCATE 15, 1: PRINT "Limite med     "; SPC(23); ""
LOCATE 15, 24: PRINT USING "####"; limite12%
LOCATE 15, 38: PRINT USING "####"; limite22%
LOCATE 15, 51: PRINT USING "####"; conta812%
LOCATE 16, 1: PRINT "Limite max     "; SPC(23);
LOCATE 16, 24: PRINT USING "####"; limite3%
LOCATE 16, 38: PRINT USING "####"; limite3%
LOCATE 16, 51: PRINT USING "####"; conta813%
'LOCATE 11, 50
'PRINT SPC(28);
'LOCATE 11, 1
'PRINT "Preparatore...................."; mass1%, rif1%, limite01%
'LOCATE 12, 50
'PRINT SPC(28);
'LOCATE 12, 1
'PRINT "Finitore......................."; mass2%, rif2%, limite02%
'LOCATE 13, 1
'PRINT "Stabilita'....................."; stab1%, stab2%, conta81%; conta82%
'LOCATE 14, 1
'PRINT "Limiti preparatore............."; limite11%, limite12%, limite3%
''LOCATE 14, 55
''PRINT conta51%
'LOCATE 15, 1
'PRINT "Limiti finitore................"; limite21%, limite22%, limite3%
''LOCATE 15, 55
''PRINT conta61%
'LOCATE 16, 1
'PRINT "Pezzi ammessi.................."; conta811%, conta812%, conta813%
 LOCATE 17, 1
 PRINT STRING$(80, 196);
 LOCATE 18, 1
 PRINT "Funzionamento attivo:..........";
 SELECT CASE stato%
   CASE IS = 0
   PRINT " disattivo per 1 minuto  "; STR$(INT(TIMER / 60) - tempo3%);
   CASE IS = 1
   PRINT " funzione giorno              ";
   CASE IS = 2
   PRINT " funzione notte               ";
   CASE IS = 3
   PRINT " disattivo 15 minuti     "; STR$(INT(TIMER / 60) - tempo3%);
 END SELECT
 LOCATE 19, 1
 PRINT STRING$(80, 205);
'LOCATE 20, 1
'PRINT "Salva dati su disco ogni ......"; STR$(conta9%) + " colpi"
'LOCATE 21, 1
'PRINT "Produzione pezzi al minuto....."; " " + RIGHT$(STR$(colpimin0&), 3)
 LOCATE 25, 1
 PRINT "Esci                Men—             Grafico            Funzione           Reset";
RETURN
'
inseguimento1:           ' autoinseguimento  ogni conta8% colpi  ( 50 )
IF mass1% > rif1% THEN rif1% = rif1% + 1
IF mass1% < rif1% THEN rif1% = rif1% - 1
' con l'occasione si aggiorna anche l'orologio
LOCATE 1, 1
PRINT TIME$
LOCATE 2, 1
PRINT "        "
LOCATE 2, 1
PRINT "Vel."; " " + RIGHT$(STR$(colpimin0&), 3)
GOSUB tempoaux
RETURN
inseguimento2:
IF mass2% > rif2% THEN rif2% = rif2% + 1
IF mass2% < rif2% THEN rif2% = rif2% - 1
RETURN
'
salva:
OPEN "forma.dat" FOR OUTPUT AS #1   'salva i dati ogni 50 colpi, oppure ogni
  PRINT #1, conta01!, conta02!    'volta che esce fuori tolleranza
  PRINT #1, conta11&, conta12&
  PRINT #1, conta21&, conta22&
  PRINT #1, conta31&, conta32&
  PRINT #1, conta41&, conta42&
  PRINT #1, conta51%, conta52%
  PRINT #1, conta61%, conta62%
  PRINT #1, conta71%, conta72%
  PRINT #1, media1%, media2%
  PRINT #1, rif1%, rif2%, stab1%, stab2%
  PRINT #1, limite1%, limite2%
  PRINT #1, tempo12&, tempo21&, tempo22&
  PRINT #1, conta811%, conta812%, conta813%
  PRINT #1, conta821%, conta822%, conta823%
  PRINT #1, ritlett1%, ritlett2%, limite3%
  PRINT #1, tipomacc%, numlett%, numsalvadati%
  PRINT #1, integra%, ampli1%, ampli2%
  PRINT #1, flett%, delayfilo%
  PRINT #1, dummy1%, limite12%, limite22%
  PRINT #1, spot%, spotpre%, spotfin%
  PRINT #1, dummy7%, dummy8%, dummy9%
CLOSE #1
var% = 0                            'variabile locale dummy
OPEN "analisi.dat" FOR INPUT AS #2  'apre e carica array precedente
  INPUT #2, var%                    'butta via il dato piu vecchio
  FOR i = 0 TO 638                  'con uno shift di -1
  INPUT #2, analisi%(i)
  NEXT
CLOSE #2
OPEN "analisi.dat" FOR OUTPUT AS #2    'scrive il nuovo array di analisi
  FOR i = 0 TO 638
   PRINT #2, analisi%(i)
  NEXT
  PRINT #2, mass2%
CLOSE #2
RETURN
tempoaux:
ferma% = 0
'tempo21& = 0            'tempo turno ausiliario in secondi
'tempo22& = 0            'limite intervento turno ausiliario (0 escluso)
'tempo24%  viene impostata a TIMER ad ogni avvio
IF TIMER < tempo24& THEN tempo24& = TIMER     'passate le 24 bisogna
					      'aggiornare tempo24&
tempo21& = tempo21& + (TIMER - tempo24&)
tempo24& = TIMER
IF tempo21& / 60 >= tempo22& / 60 AND tempo22& <> 0 THEN ferma% = 14
RETURN

menu:
'DO                      'cicla fino a che non si preme invio
' LOCATE 25, 1
' PRINT "Esci                Men—             Grafico            Funzione           Reset";
' LOCATE 24, 19
' INPUT ; " Inserire la iniziale del comando ", sel$  'variabile scelta
sel$ = in3$
 IF UCASE$(sel$) = "E" THEN
  GOSUB salva            'prima di uscire salva i dati
 SELECT CASE tipomacc%                'uscita digitale (in 4 punti)
 CASE IS = 1                         'macchina TP2/CL
     DIO%(0) = 1                     'ferma giorno
     CALL AX5210(11, DIO%(0), FLAG%)
 CASE IS = 2                         'macchina 873/SV
     DIO%(0) = 3                     'stacca filo
     CALL AX5210(11, DIO%(0), FLAG%)
     FOR i = 1 TO delayfilo%: NEXT   'ritardo tra stacco filo e stop
     DIO%(0) = 1                     'ferma giorno
     CALL AX5210(11, DIO%(0), FLAG%)
   '                         Aggiungere qui ulteriori casi
  END SELECT
  CLS
  END
 ELSEIF UCASE$(sel$) = "M" THEN
  GOTO modificaparametri                 'modifica i dati e riparte da capo
 ELSEIF UCASE$(sel$) = "R" THEN
  rif1% = mass1%
  rif2% = mass2%
  stato% = 0                             'passa in manuale 1 minuto
  tempo3% = INT(TIMER / 60)              'azzera conteggio 1 minuto
' ELSEIF UCASE$(sel$) = "S" THEN
'  LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'  INPUT ; "Inserire stabilita' 1................", inval$
'  IF inval$ <> "" THEN
'    stab1% = VAL(inval$)
'  END IF
'  LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'  INPUT ; "Inserire stabilita' 2................", inval$
'  IF inval$ <> "" THEN
'    stab2% = VAL(inval$)
'  END IF
'  LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'  INPUT ; "Piccoli scostamenti ammessi..........", inval$
'  IF inval$ <> "" THEN
'    conta811% = VAL(inval$)
'    conta821% = VAL(inval$)
'  END IF
'  LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'  INPUT ; "Grandi scostamenti ammessi...........", inval$
'  IF inval$ <> "" THEN
'    conta812% = VAL(inval$)
'    conta822% = VAL(inval$)
'  END IF
'  LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'  INPUT ; "Scostamenti assoluti ammessi.........", inval$
'  IF inval$ <> "" THEN
'    conta813% = VAL(inval$)
'    conta823% = VAL(inval$)
'  END IF
'  stato% = 0                             'passa in manuale 1 minuto
'  tempo3% = INT(TIMER / 60)              'azzera conteggio 1 minuto
 ELSEIF UCASE$(sel$) = "F" THEN
  LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 22
  INPUT ; "0=temp. 1=giorno 2=notte 3=manuale ", inval$
  IF inval$ <> "" THEN
    IF VAL(inval$) < 4 THEN stato% = VAL(inval$)
'    IF VAL(inval$) = 4 THEN
'       CALL grafo(dat%())
'       stato% = 0                             'passa in manuale 1 minuto
'       tempo3% = INT(TIMER / 60)              'azzera conteggio 1 minuto
    END IF
    IF stato% = 3 OR 0 THEN tempo3% = INT(TIMER / 60) 'inizializza stato%=3,0
 ELSEIF UCASE$(sel$) = "G" THEN
    CALL grafo(dat%())
    stato% = 0                             'passa in manuale 1 minuto
    tempo3% = INT(TIMER / 60)              'azzera conteggio 1 minuto
 END IF
' ELSEIF UCASE$(sel$) = "C" THEN
'   LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'   INPUT ; "Impostazione di tutti i contatori...", inval$
'   IF inval$ <> "" THEN
'     conta01! = VAL(inval$)      'contatore principale  (fino a 3,4x10^38)
'     conta11& = VAL(inval$)      'contatore punzone     (fino a 2147483647)
'     conta21& = VAL(inval$)      'contatore matrice
'     conta31& = VAL(inval$)      'contatore lotto
'     conta41& = VAL(inval$)      'contatore turno ausiliario
'   END IF
'   IF conta01! = 0 THEN                    'azzera dati del trend
'      OPEN "analisi.dat" FOR OUTPUT AS #2  'in questo punto e alla riga 735
'        FOR i = 0 TO 639
'          PRINT #2, 100
'        NEXT
'      CLOSE #2
'   END IF
'  tempo3% = INT(TIMER / 60)             'azzera conteggio 1 minuto
'   stato% = 0                            'passa in manuale 1 minuto
' END IF
' SELECT CASE VAL(sel$)
'  CASE IS = 1
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Contatore principale.................", inval$
'    IF inval$ <> "" THEN
'      conta01! = VAL(inval$)
'    END IF
'    IF conta01! = 0 THEN                   'azzera dati del trend
'      OPEN "analisi.dat" FOR OUTPUT AS #2  'in questo punto e alla
'       FOR i = 0 TO 639                   'riga 710
'          PRINT #2, 100
'        NEXT
'      CLOSE #2
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 2
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Limite contatore principale..........", inval$
'    IF inval$ <> "" THEN
'      conta02! = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 3
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Contatore punzone....................", inval$
'    IF inval$ <> "" THEN
'      conta11& = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 4
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Limite contatore punzone.............", inval$
'    IF inval$ <> "" THEN
'      conta12& = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 5
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Contatore matrice....................", inval$
'    IF inval$ <> "" THEN
'      conta21& = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 6
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Limite contatore matrice.............", inval$
'    IF inval$ <> "" THEN
'      conta22& = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 7
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Contatore lotto......................", inval$
'    IF inval$ <> "" THEN
'      conta31& = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 8
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Limite contatore lotto...............", inval$
'    IF inval$ <> "" THEN
'      conta32& = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 9
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Contatore turno ausiliario...........", inval$
'    IF inval$ <> "" THEN
'      conta41& = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 10
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Limite turno ausiliario..............", inval$
'    IF inval$ <> "" THEN
'      conta42& = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 11
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Tempo turno ausiliario in minuti.....", inval$
'    IF inval$ <> "" THEN
'      tempo21& = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
'  CASE IS = 12
'    LOCATE 24, 15: PRINT SPC(60); : LOCATE 24, 15
'    INPUT ; "Limite turno ausiliario in minuti....", inval$
'    IF inval$ <> "" THEN
'      tempo22& = VAL(inval$)
'    END IF
'    stato% = 0                           'passa in manuale 1 minuto
'    tempo3% = INT(TIMER / 60)            'azzera conteggio 1 minuto
' END SELECT
'LOOP UNTIL sel$ <> CHR$(13)
GOSUB salva
CLS
'LOCATE 25, 1
'PRINT "                       Premere un tasto per uscire                  ";
RETURN
'
prefazione:                 'visualizza e modifica dati alla partenza
CLS
'tempo3% = INT(TIMER / 60)   'rimane a tempo determinato
'in$ = INKEY$                'pulisce buffer tastiera
'DO
 LOCATE 3, 1
 PRINT STRING$(80, 205);
 LOCATE 4, 1
 PRINT "(1-2)   Contatore principale..."; conta01!, conta02!
 LOCATE 5, 1
 PRINT "(3-4)   Punzone - no stop -...."; conta11&, conta12&
 IF conta11& > conta12& THEN
   LOCATE 5, 60                         'lampeggia cambio punzone
   IF conta12& <> 0 THEN PRINT STRING$(10, 178);
 END IF
 LOCATE 6, 1
 PRINT "(5-6)   Matrice - no stop -...."; conta21&, conta22&
 IF conta21& > conta22& THEN
   LOCATE 6, 60                         'lampeggia cambio matrice
   IF conta22& <> 0 THEN PRINT STRING$(10, 178);
 END IF
 LOCATE 7, 1
 PRINT "(7-8)   Lotto   - no stop -...."; conta31&, conta32&
 IF conta31& > conta32& THEN
   LOCATE 7, 60                         'lampeggia fine lotto
   IF conta32& <> 0 THEN PRINT STRING$(10, 178);
 END IF
 LOCATE 8, 1
 PRINT "(9-10)  Contatore turno aux...."; conta41&, conta42&
 tempo23$ = STR$(INT(tempo21& / 3600)) + " :" + STR$(tempo21& MOD 60)
 LOCATE 9, 1
 PRINT "(11-12) Tempo in minuti........"; tempo21& / 60, tempo22& / 60, tempo23$
 LOCATE 10, 1
 PRINT STRING$(80, 196);
 LOCATE 11, 50
 PRINT SPC(28);
 LOCATE 11, 1
 PRINT "Preparatore...................."; mass1%, rif1%, limite01%
 LOCATE 12, 50
 PRINT SPC(28);
 LOCATE 12, 1
 PRINT "Finitore......................."; mass2%, rif2%, limite02%
 LOCATE 13, 1
 PRINT "Stabilita'....................."; stab1%, stab2%, conta81%; conta82%
 LOCATE 14, 1
 PRINT "Limiti preparatore............."; limite11%, limite12%, limite3%
 'LOCATE 14, 55
 'PRINT conta51%
 LOCATE 15, 1
 PRINT "Limiti finitore................"; limite21%, limite22%, limite3%
 'LOCATE 15, 55
 'PRINT conta61%
 LOCATE 16, 1
 PRINT "Pezzi ammessi.................."; conta811%, conta812%, conta813%
 LOCATE 17, 1
 PRINT STRING$(80, 196);
 LOCATE 18, 1
 PRINT "Funzionamento attivo:..........";
 SELECT CASE stato%
   CASE IS = 0
   PRINT " disattivo per 1 minuto  "; STR$(INT(TIMER / 60) - tempo3%);
   CASE IS = 1
   PRINT " funzione giorno              ";
   CASE IS = 2
   PRINT " funzione notte               ";
   CASE IS = 3
   PRINT " disattivo 15 minuti     "; STR$(INT(TIMER / 60) - tempo3%);
 END SELECT
 LOCATE 19, 1
 PRINT STRING$(80, 205);
 LOCATE 20, 1
 PRINT "Salva dati su disco ogni ......"; STR$(conta9%) + " colpi"
 LOCATE 21, 1
 PRINT "Produzione pezzi al minuto....."; " " + RIGHT$(STR$(colpimin0&), 3)
 LOCATE 23, 16
 PRINT ; "            F1 per modificare i dati              "
ON KEY(1) GOSUB menu
KEY(1) ON
SLEEP (30)
KEY(1) OFF
' in$ = INKEY$
' LOCATE 25, 40
' INPUT in$
' IF in$ = "M" OR in$ = "m" THEN GOSUB menu
' IF (INT(TIMER / 60) - tempo3%) > 1 THEN EXIT DO        'tempo 1 minuto
' CLS
'LOOP UNTIL ((in$ = "A") OR (in$ = "a"))
'tempo3% = INT(TIMER / 60)   'tempo iniziale in stato%=0
CLS
RETURN
'*************************************FINE**********************************

SUB grafo (dat%())
prefin% = 1              '0 sposta preparatore - 1 sposta finitore
'legge il file di dati e permette la modifica di tutti i parametri
'la correttezza del file e' gia' stata controllata nel modulo principale
ON ERROR GOTO gesterr   'codice file non trovato = 53
  OPEN "forma.dat" FOR INPUT AS #1
  INPUT #1, conta01!, conta02!
  INPUT #1, conta11&, conta12&
  INPUT #1, conta21&, conta22&
  INPUT #1, conta31&, conta32&
  INPUT #1, conta41&, conta42&
  INPUT #1, conta51%, conta52%
  INPUT #1, conta61%, conta62%
  INPUT #1, conta71%, conta72%
  INPUT #1, media1%, media2%
  INPUT #1, rif1%, rif2%, stab1%, stab2%
  INPUT #1, limite1%, limite2%
  INPUT #1, tempo12&, tempo21&, tempo22&
  INPUT #1, conta811%, conta812%, conta813%
  INPUT #1, conta821%, conta822%, conta823%
  INPUT #1, ritlett1%, ritlett2%, limite3%
  INPUT #1, tipomacc%, numlett%, numsalvadati%
  INPUT #1, integra%, ampli1%, ampli2%
  INPUT #1, flett%, delayfilo%
  INPUT #1, dummy1%, limite12%, limite22%
  INPUT #1, spot%, spotpre%, spotfin%
  INPUT #1, dummy7%, dummy8%, dummy9%
  CLOSE #1
ON ERROR GOTO 0
CLS
SCREEN 12
'modo grafico 640 x 400
successivo% = 0
tempo = INT(TIMER)                                   'tempo iniziale
ripeti2:
in2% = 0
DO WHILE in2% = 0     'resta in attesa finche' arriva il prossimity 2
  CALL AX5210(12, DIO%(0), FLAG%)
  IF (DIO%(0) AND 2) <> 0 THEN in2% = 1
LOOP
in2% = 0
DO WHILE in2% = 0     'resta in attesa finche' arriva il prossimity 1
  CALL AX5210(12, DIO%(0), FLAG%)
  IF (DIO%(0) AND 1) <> 0 THEN in2% = 1
LOOP
IF ritlett1% <> 0 THEN           'ritardo inziale prefissato
    FOR i = 1 TO ritlett1%: NEXT
END IF
DIO%(0) = numlett%          'numero letture o conversioni
DIO%(1) = VARPTR(dat%(0))   'puntatore array dati
DIO%(2) = 1                 'trigger interno temporizzato max velocita'
DIO%(3) = VARPTR(CH%(0))    'puntatore array canali
CALL AX5210(4, DIO%(0), FLAG%)
'****************************************************************************
SELECT CASE successivo%
 CASE IS <= 1
   LINE (320, 1)-(320, 280)
   LINE (1, 280)-(639, 280)
   LINE (5, 160)-(315, 160)
   LINE (325, 160)-(635, 160)
   LOCATE 1, 1
   PRINT "Preparatore"
   LOCATE 1, 35
   PRINT " STAVIT snc";
   LOCATE 1, 73
   PRINT "Finitore"
 CASE IS > 10
  successivo% = 0
   CLS
END SELECT
successivo% = successivo% + 1
SELECT CASE successivo%
 CASE IS <= 1
'---------------------------------  t r e n d -----------------------------
'   LOCATE 20, 26
'   PRINT " -  Scostamento  finitore  -"
LOCATE 20, 1
PRINT "64000 - - - - - - -48000 - - - - - trend finitore - - - - - 16000 - - - - - - -0"
LINE (0, 479)-(639, 479)
'CLEAR
x0% = 0
y0% = 479
maxtrend% = 1
FOR i = 0 TO 638
  IF analisi%(i) > maxtrend% THEN maxtrend% = analisi%(i)
NEXT i
sc% = maxtrend% / 150                 'il massimo possibile e' 160
'SELECT CASE mass2%                   'imposta scala grafico trend
'  CASE IS < 150
'    sc% = 2
'  CASE 150 TO 500
'    sc% = 4
'  CASE 501 TO 1000
'    sc% = 9
'  CASE 1001 TO 1600
'    sc% = 15
'  CASE 1601 TO 2100
'    sc% = 20
'END SELECT
FOR i = 0 TO 638
 LINE (x0%, y0% - analisi%(i) / sc%)-(x0% + 1, y0% - analisi%(i + 1) / sc%)
 x0% = x0% + 1
NEXT i
'----------------------------- fine trend -----------------------------------
END SELECT
x0% = 5
y0% = 160
'sc% = rapporto di scala; fondoscala=120; range 0 - 2400
IF mass1% = 0 THEN mass1% = 1         '\
sc% = mass1% / 150                    '\+1 per evitare /0
IF sc% = 0 THEN sc% = 1               '\
'SELECT CASE mass1%                   'imposta scala grafico finitore
'  CASE IS < 150
'    sc% = 1
'  CASE 150 TO 500
'    sc% = 3
'  CASE 501 TO 1000
'    sc% = 6
'  CASE 1001 TO 1600
'    sc% = 11
'  CASE 1601 TO 2100
'    sc% = 14
'END SELECT
presup% = mass1% + limite11%             'locale linea limite superiore
preinf% = mass1% - limite11%
IF dat%(maxprep%) > 0 THEN
  LINE (x0%, y0% - preinf% / sc%)-(x0% + 298, y0% - preinf% / sc%)
  LINE (x0%, y0% - presup% / sc%)-(x0% + 298, y0% - presup% / sc%)
 ELSE
  LINE (x0%, y0% + preinf% / sc%)-(x0% + 298, y0% + preinf% / sc%)
  LINE (x0%, y0% + presup% / sc%)-(x0% + 298, y0% + presup% / sc%)
END IF
'smooth  - tolto perche' rallenta troppo
'FOR i = 2 TO 296
'  dat%(i) = (dat%(i - 2) + dat%(i - 1) + dat%(i) + dat%(i + 1) + dat%(i + 2)) / 5
'NEXT i
'
FOR i = 0 TO 298
	LINE (x0%, y0% - dat%(i) / sc%)-(x0% + 1, y0% - dat%(i + 1) / sc%)
	x0% = x0% + 1
NEXT i
'-----------------------  finitore    -------------------------------------
in2% = 0
DO WHILE in2% = 0     'resta in attesa finche' arriva il prossimity 1
  CALL AX5210(12, DIO%(0), FLAG%)
  IF (DIO%(0) AND 1) <> 0 THEN in2% = 1
LOOP
in2% = 0
DO WHILE in2% = 0     'resta in attesa finche' arriva il prossimity 2
  CALL AX5210(12, DIO%(0), FLAG%)
  IF (DIO%(0) AND 2) <> 0 THEN in2% = 1
LOOP
IF ritlett2% <> 0 THEN           'ritardo inziale prefissato
    FOR i = 1 TO ritlett2%: NEXT
END IF
DIO%(0) = numlett%          'numero letture o conversioni
DIO%(1) = VARPTR(dat%(0))   'puntatore array dati
DIO%(2) = 1                 'trigger interno temporizzato max velocita'
DIO%(3) = VARPTR(CH%(0))    'puntatore array canali
CALL AX5210(4, DIO%(0), FLAG%)
'****************************************************************************
x0% = 325
y0% = 160
IF mass2% = 0 THEN mass2% = 1         '\
sc% = mass2% / 150                    '\+1 per evitare /0
IF sc% = 0 THEN sc% = 1               '\
'SELECT CASE mass2%                   'imposta scala grafico finitore
'  CASE IS < 150
'    sc% = 1
'  CASE 150 TO 500
'    sc% = 3
'  CASE 501 TO 1000
'    sc% = 6
'  CASE 1001 TO 1600
'    sc% = 11
'  CASE 1601 TO 2100
'    sc% = 14
'END SELECT
'IF sc% = 0 THEN sc% = 1          'escludo il caso mass2% < 120
finsup% = mass2% + limite21%             'locale linea limite superiore
fininf% = mass2% - limite21%
IF dat%(maxfin%) > 0 THEN
    LINE (x0%, y0% - fininf% / sc%)-(x0% + 298, y0% - fininf% / sc%)
    LINE (x0%, y0% - finsup% / sc%)-(x0% + 298, y0% - finsup% / sc%)
  ELSE
    LINE (x0%, y0% + fininf% / sc%)-(x0% + 298, y0% + fininf% / sc%)
    LINE (x0%, y0% + finsup% / sc%)-(x0% + 298, y0% + finsup% / sc%)
END IF
'smooth - tolto perche' rallenta troppoe
'FOR i = 2 TO 296
'  dat%(i) = (dat%(i - 2) + dat%(i - 1) + dat%(i) + dat%(i + 1) + dat%(i + 2)) / 5
'NEXT i
'
FOR i = 0 TO 298
	LINE (x0%, y0% - dat%(i) / sc%)-(x0% + 1, y0% - dat%(i + 1) / sc%)
	x0% = x0% + 1
NEXT i
GOTO finetrend                      'usare solo per debug
'-------------------   aggiorna   trend   ---------------------------------
mass21% = 0                         'variabile locale massimo
var% = 0                            'variabile locale dummy
FOR i = 0 TO 300
  IF dat%(i) > mass21% THEN mass21% = dat%(i)
NEXT
OPEN "analisi.dat" FOR INPUT AS #2  'apre e carica array precedente
  INPUT #2, var%
  FOR i = 0 TO 638                  'con uno shift di -1
  INPUT #2, analisi%(i)
  NEXT
CLOSE #2
OPEN "analisi.dat" FOR OUTPUT AS #2    'scrive il nuovo array di analisi
  FOR i = 0 TO 638
   PRINT #2, analisi%(i)
  NEXT
  PRINT #2, mass21%
CLOSE #2
'------------------------  fine aggiorna trend  ---------------------------
finetrend:
LOCATE 30, 2: PRINT spotpre%; : LOCATE 30, 19: PRINT spotfin%;
LINE (spotpre%, 10)-(spotpre%, 80)
LINE (spotfin% + 325, 10)-(spotfin% + 325, 80)
'***************************** varia spot ********************************
setspot:
s$ = INKEY$
SELECT CASE s$
  CASE CHR$(9)
   IF prefin% = 0 THEN prefin% = 1         ' prefin%=1 finitore
   IF prefin% = 1 THEN prefin% = 0         ' prefin%=0 preparatore
  CASE "p"
   LINE (spotpre%, 10)-(spotpre%, 80), 0        'cancella la linea precedente
   LINE (spotfin% + 325, 10)-(spotfin% + 325, 80), 0
   IF prefin% = 0 THEN spotpre% = spotpre% + 1
   IF prefin% = 1 THEN spotfin% = spotfin% + 1
   IF prefin% = 0 THEN
     LOCATE 30, 1: PRINT ">"; : LOCATE 30, 18: PRINT " ";
     ELSE
     LOCATE 30, 1: PRINT " "; : LOCATE 30, 18: PRINT ">";
   END IF
   LOCATE 30, 2: PRINT spotpre%; : LOCATE 30, 19: PRINT spotfin%;
   LINE (spotpre%, 10)-(spotpre%, 80)
   LINE (spotfin% + 325, 10)-(spotfin% + 325, 80)
  CASE "m"
   LINE (spotpre%, 10)-(spotpre%, 80), 0        'cancella la linea precedente
   LINE (spotfin% + 325, 10)-(spotfin% + 325, 80), 0
   IF prefin% = 0 THEN
     LOCATE 30, 1: PRINT ">"; : LOCATE 30, 18: PRINT " ";
     ELSE
     LOCATE 30, 1: PRINT " "; : LOCATE 30, 18: PRINT ">";
   END IF
   IF prefin% = 0 THEN spotpre% = spotpre% - 1
   IF prefin% = 1 THEN spotfin% = spotfin% - 1
   LOCATE 30, 2: PRINT spotpre%; : LOCATE 30, 19: PRINT spotfin%;
   LINE (spotpre%, 10)-(spotpre%, 80)
   LINE (spotfin% + 325, 10)-(spotfin% + 325, 80)
  CASE CHR$(13)
   GOTO escispot
  CASE ELSE
   GOTO setspot
END SELECT
escispot:
IF INKEY$ <> "" THEN
  IF INKEY$ <> CHR$(9) OR INKEY$ <> "p" OR INKEY$ <> "m" THEN
    SCREEN 0
    CLS
    EXIT SUB   'esce se si preme un tasto
  END IF
END IF
IF (INT(TIMER) - tempo) > 60 THEN
   SCREEN 0
   CLS
   EXIT SUB     'esce per supero tempo
END IF
GOTO ripeti2
SCREEN 0
finespot:
OPEN "forma.dat" FOR OUTPUT AS #1   'salva i dati su disco alla fine di
  PRINT #1, conta01!, conta02!      'tutte le variazioni
  PRINT #1, conta11&, conta12&
  PRINT #1, conta21&, conta22&
  PRINT #1, conta31&, conta32&
  PRINT #1, conta41&, conta42&
  PRINT #1, conta51%, conta52%
  PRINT #1, conta61%, conta62%
  PRINT #1, conta71%, conta72%
  PRINT #1, media1%, media2%
  PRINT #1, rif1%, rif2%, stab1%, stab2%
  PRINT #1, limite1%, limite2%
  PRINT #1, tempo12&, tempo21&, tempo22&
  PRINT #1, conta811%, conta812%, conta813%
  PRINT #1, conta821%, conta822%, conta823%
  PRINT #1, ritlett1%, ritlett2%, limite3%
  PRINT #1, tipomacc%, numlett%, numsalvadati%
  PRINT #1, integra%, ampli1%, ampli2%
  PRINT #1, flett%, delayfilo%
  PRINT #1, dummy1%, limite12%, limite22%
  PRINT #1, spot%, spotpre%, spotfin%
  PRINT #1, dummy7%, dummy8%, dummy9%
CLOSE #1
'EXIT SUB
CLS
END SUB

SUB settavariabili (dummy1%)
'legge il file di dati e permette la modifica di tutti i parametri
'la correttezza del file e' gia' stata controllata nel modulo principale
ON ERROR GOTO gesterr   'codice file non trovato = 53
  OPEN "forma.dat" FOR INPUT AS #1
  INPUT #1, conta01!, conta02!
  INPUT #1, conta11&, conta12&
  INPUT #1, conta21&, conta22&
  INPUT #1, conta31&, conta32&
  INPUT #1, conta41&, conta42&
  INPUT #1, conta51%, conta52%
  INPUT #1, conta61%, conta62%
  INPUT #1, conta71%, conta72%
  INPUT #1, media1%, media2%
  INPUT #1, rif1%, rif2%, stab1%, stab2%
  INPUT #1, limite1%, limite2%
  INPUT #1, tempo12&, tempo21&, tempo22&
  INPUT #1, conta811%, conta812%, conta813%
  INPUT #1, conta821%, conta822%, conta823%
  INPUT #1, ritlett1%, ritlett2%, limite3%
  INPUT #1, tipomacc%, numlett%, numsalvadati%
  INPUT #1, integra%, ampli1%, ampli2%
  INPUT #1, flett%, delayfilo%
  INPUT #1, dummy1%, limite12%, limite22%
  INPUT #1, spot%, spotpre%, spotfin%
  INPUT #1, dummy7%, dummy8%, dummy9%
  CLOSE #1
ON ERROR GOTO 0
tempo = INT(TIMER)              'tempo iniziale
ripeti3:                        'ottenuti i dati si procede alle modifiche
'-----------------------------visualizza parametri--------------------------
CLS
LOCATE 1, 35: PRINT "Stavit snc"
LOCATE 2, 30: PRINT " Modifica parametri "
LOCATE 3, 1
PRINT STRING$(80, 205);
LOCATE 4, 1
PRINT "01  02  Contatore principale..."        '; conta01!, conta02!
LOCATE 4, 33: PRINT USING "########,"; conta01!
LOCATE 4, 46: PRINT USING "########,"; conta02!
LOCATE 5, 1
PRINT "03  04  Punzone - no stop -...."        '; conta11&, conta12&
LOCATE 5, 33: PRINT USING "########,"; conta11&
LOCATE 5, 46: PRINT USING "########,"; conta12&
LOCATE 6, 1
PRINT "05  06  Matrice - no stop -...."         '; conta21&, conta22&
LOCATE 6, 33: PRINT USING "########,"; conta21&
LOCATE 6, 46: PRINT USING "########,"; conta22&
LOCATE 7, 1
PRINT "07  08  Lotto   - no stop -...."         '; conta31&, conta32&
LOCATE 7, 33: PRINT USING "########,"; conta31&
LOCATE 7, 46: PRINT USING "########,"; conta32&
LOCATE 8, 1
PRINT "09  10  Contatore turno aux...."          '; conta41&, conta42&; ""
LOCATE 8, 33: PRINT USING "########,"; conta41&
LOCATE 8, 46: PRINT USING "########,"; conta42&
LOCATE 9, 1
PRINT "11  12  Tempo in minuti........"
LOCATE 9, 33: PRINT USING "########,"; tempo21& / 60
LOCATE 9, 46: PRINT USING "########,"; tempo22& / 60
LOCATE 10, 1
PRINT STRING$(80, 196);
LOCATE 11, 1: PRINT USING "13 Errori minimi ammessi....    ##,"; conta811%
LOCATE 11, 46: PRINT USING "14 Errori medi ammessi......    ##,"; conta812%
LOCATE 12, 1: PRINT USING "15 Errori massimi ammessi...    ##,"; conta813%
LOCATE 12, 46: PRINT USING "16 Limite massimo...........  ####,"; limite3%
LOCATE 13, 1: PRINT USING "17 Limite minimo preparatore  ####,"; stab1%
LOCATE 13, 46: PRINT USING "18 Limite minimo finitore...  ####,"; stab2%
LOCATE 14, 1: PRINT USING "19 Limite medio preparatore. #####,"; limite12%
LOCATE 14, 46: PRINT USING "20 Limite medio finitore.... #####,"; limite22%
LOCATE 15, 1: PRINT USING "21 Ritardo lettura.......... #####,"; ritlett1%
LOCATE 15, 46: PRINT USING "22 Amplificazione ..........    ##,"; ampli1%
LOCATE 16, 1: PRINT USING "23 Tipo macchina............   ###,"; tipomacc%
LOCATE 16, 46: PRINT USING "24 Reintegro contatori......    ##,"; integra%
LOCATE 17, 1: PRINT USING "25 Frequenza letture dati...   ###,"; flett%
LOCATE 17, 46: PRINT USING "26 Anticipo stacco filo..... #####,"; delayfilo%
LOCATE 18, 1: PRINT USING "27 Numero letture...........   ###,"; numlett%
LOCATE 18, 46: PRINT USING "28 Frequenza salva dati.....   ###,"; numsalvadati%
LOCATE 19, 1: PRINT "29 Imposta tutti i contatori       ";
LOCATE 19, 46: PRINT "30 Attesa in manuale........       ";
LOCATE 20, 1
PRINT STRING$(80, 205);
in$ = ""                        'variabile locale scelta
in1$ = ""                       'variabile locale scelta costruita
inval$ = ""                     'variabile locale dati immessi
LOCATE 21, 22
PRINT "Selezionare il dato da variare ";
'LINE INPUT "Selezionare il dato da variare "; in$   'variabile scelta
DO
 in$ = INKEY$
 IF in$ <> "" THEN
  IF LEN(in1$) < 2 THEN
    in1$ = in1$ + in$
    LOCATE 21, 55
    PRINT in1$;
  END IF
  IF LEN(in1$) = 2 THEN EXIT DO
  IF in$ = CHR$(13) THEN GOTO fine
 END IF
 in$ = ""
 IF (INT(TIMER) - tempo) > 600 THEN
   tmenu% = 2
   GOTO fine       'esce per supero tempo
 END IF
 LOCATE 1, 76: PRINT USING "####"; (599 - (INT(TIMER) - tempo) + 1);
LOOP WHILE 1

SELECT CASE VAL(in1$)
 CASE IS = 1
  LOCATE 24, 17: PRINT "Contatore principale.................",
  GOSUB verinp
  IF inval$ <> "" THEN conta01! = VAL(inval$)
 CASE IS = 2
  LOCATE 24, 17: PRINT "Limite contatore principale..........",
  GOSUB verinp
  IF inval$ <> "" THEN conta02! = VAL(inval$)
 CASE IS = 3
  LOCATE 24, 17: PRINT "Contatore punzone....................",
  GOSUB verinp
  IF inval$ <> "" THEN conta11& = VAL(inval$)
 CASE IS = 4
  LOCATE 24, 17: PRINT "Limite contatore punzone.............",
  GOSUB verinp
  IF inval$ <> "" THEN conta12& = VAL(inval$)
 CASE IS = 5
  LOCATE 24, 17: PRINT "Contatore matrice....................",
  GOSUB verinp
  IF inval$ <> "" THEN conta21& = VAL(inval$)
 CASE IS = 6
  LOCATE 24, 17: PRINT "Limite contatore matrice.............",
  GOSUB verinp
  IF inval$ <> "" THEN conta22& = VAL(inval$)
 CASE IS = 7
  LOCATE 24, 17: PRINT "Contatore lotto......................",
  GOSUB verinp
  IF inval$ <> "" THEN conta31& = VAL(inval$)
 CASE IS = 8
  LOCATE 24, 17: PRINT "Limite contatore lotto...............",
  GOSUB verinp
  IF inval$ <> "" THEN conta32& = VAL(inval$)
 CASE IS = 9
  LOCATE 24, 17: PRINT "Contatore turno ausiliario...........",
  GOSUB verinp
  IF inval$ <> "" THEN conta41& = VAL(inval$)
 CASE IS = 10
  LOCATE 24, 17: PRINT "Limite turno ausiliario..............",
  GOSUB verinp
  IF inval$ <> "" THEN conta42& = VAL(inval$)
 CASE IS = 11
  LOCATE 24, 17: PRINT "Tempo turno ausiliario in minuti.....",
  GOSUB verinp
'  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN tempo21& = VAL(inval$) * 60
 CASE IS = 12
  LOCATE 24, 17: PRINT "Limite turno ausiliario in minuti....",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN tempo22& = VAL(inval$) * 60
 CASE IS = 13
  LOCATE 24, 17: PRINT "Errori minimi ammessi................",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN conta811% = VAL(inval$)
 CASE IS = 14
  LOCATE 24, 17: PRINT "Errori medi ammessi..................",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN conta812% = VAL(inval$)
 CASE IS = 15
  LOCATE 24, 17: PRINT "Errori massimi ammessi...............",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN conta813% = VAL(inval$)
 CASE IS = 16
  LOCATE 24, 17: PRINT "Limite assoluto......................",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN limite3% = VAL(inval$)
 CASE IS = 17
  LOCATE 24, 17: PRINT "Stabilit… preparatore................",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN stab1% = VAL(inval$)
 CASE IS = 18
  LOCATE 24, 17: PRINT "Stabilit… finitore...................",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN stab2% = VAL(inval$)
 CASE IS = 19
  LOCATE 24, 17: PRINT "Scostamento preparatore..............",
  GOSUB verinp
  IF VAL(inval$) > 2000 THEN inval$ = ""
  IF inval$ <> "" THEN limite12% = VAL(inval$)
 CASE IS = 20
  LOCATE 24, 17: PRINT "Scostamento finitore.................",
  GOSUB verinp
  IF VAL(inval$) > 2000 THEN inval$ = ""
  IF inval$ <> "" THEN limite22% = VAL(inval$)
 CASE IS = 21
  LOCATE 24, 17: PRINT "Ritardo lettura ( < 32000 )..........",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = "1"
  IF inval$ <> "" THEN ritlett1% = VAL(inval$)
  ritlett2% = ritlett1%
 CASE IS = 22
  LOCATE 24, 17: PRINT "Amplificazione (1 2 4 8 16)..........",
  GOSUB verinp
  IF VAL(inval$) > 16 THEN inval$ = "1"
  IF inval$ <> "" THEN ampli1% = VAL(inval$)
  ampli2% = ampli1%
 CASE IS = 23
  LOCATE 24, 17: PRINT "Tipo macchina........................",
  LOCATE 25, 17: PRINT "1 TP2   2 873/SV   3 635/I    .......";
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  LOCATE 25, 17: PRINT "                                     ";
  IF inval$ <> "" THEN tipomacc% = VAL(inval$)
 CASE IS = 24
  LOCATE 24, 17: PRINT "Reintegro contatori..................",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN integra% = VAL(inval$)
 CASE IS = 25
  LOCATE 24, 17: PRINT "Frequenze letture dati...............",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = "8"        'range 2-65535
  IF VAL(inval$) < 2 THEN inval$ = "8"
  IF inval$ <> "" THEN flett% = VAL(inval$)
 CASE IS = 26
  LOCATE 24, 17: PRINT "Anticipo stacco filo.................",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN delayfilo% = VAL(inval$)
 CASE IS = 27
  LOCATE 24, 17: PRINT "Numero letture.......................",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN numlett% = VAL(inval$)
 CASE IS = 28
  LOCATE 24, 17: PRINT "Frequenza salvataggio dati...........",
  GOSUB verinp
  IF VAL(inval$) > 32000 THEN inval$ = ""
  IF inval$ <> "" THEN numsalvadati% = VAL(inval$)
 CASE IS = 29
  LOCATE 24, 17: PRINT "Imposta tutti i contatori............",
  GOSUB verinp
  IF inval$ <> "" THEN
    conta01! = VAL(inval$)      'contatore principale  (fino a 3,4x10^38)
    conta11& = VAL(inval$)      'contatore punzone     (fino a 2147483647)
    conta21& = VAL(inval$)      'contatore matrice
    conta31& = VAL(inval$)      'contatore lotto
    conta41& = VAL(inval$)      'contatore turno ausiliario
  END IF
 CASE IS = 30
  LOCATE 24, 20: PRINT "           Attesa in manuale         ";
  attesa% = 1                    'variabile locale
  GOSUB verinp                   'resta in verinp finche non si preme invio
  attesa% = 0                    'variabile locale
END SELECT
GOTO ripeti3
fine:
CLS
LOCATE 12, 34: PRINT "Attendere.."
OPEN "forma.dat" FOR OUTPUT AS #1   'salva i dati su disco alla fine di
  PRINT #1, conta01!, conta02!      'tutte le variazioni
  PRINT #1, conta11&, conta12&
  PRINT #1, conta21&, conta22&
  PRINT #1, conta31&, conta32&
  PRINT #1, conta41&, conta42&
  PRINT #1, conta51%, conta52%
  PRINT #1, conta61%, conta62%
  PRINT #1, conta71%, conta72%
  PRINT #1, media1%, media2%
  PRINT #1, rif1%, rif2%, stab1%, stab2%
  PRINT #1, limite1%, limite2%
  PRINT #1, tempo12&, tempo21&, tempo22&
  PRINT #1, conta811%, conta812%, conta813%
  PRINT #1, conta821%, conta822%, conta823%
  PRINT #1, ritlett1%, ritlett2%, limite3%
  PRINT #1, tipomacc%, numlett%, numsalvadati%
  PRINT #1, integra%, ampli1%, ampli2%
  PRINT #1, flett%, delayfilo%
  PRINT #1, dummy1%, limite12%, limite22%
  PRINT #1, spot%, spotpre%, spotfin%
  PRINT #1, dummy7%, dummy8%, dummy9%
CLOSE #1
GOTO fine2
verinp:
in2$ = ""                          'var locale
inval$ = ""                          'var locale costruita
DO
 in2$ = INKEY$
 IF in2$ = CHR$(13) THEN EXIT DO
 inval$ = inval$ + in2$
 LOCATE 24, 55
 PRINT inval$;
 in2$ = ""
 IF attesa% <> 1 THEN                             'variabile locale
  IF (INT(TIMER) - tempo) > 600 THEN               'tempo attesa in menu
    tmenu% = 2                                    'esce per supero tempo
    EXIT DO
  END IF
  LOCATE 1, 76: PRINT USING "####"; (599 - (INT(TIMER) - tempo) + 1);
 END IF
 IF attesa% = 1 THEN tempo = INT(TIMER)           'ripristina tempo iniziale
LOOP WHILE 1
RETURN
fine2:
END SUB

