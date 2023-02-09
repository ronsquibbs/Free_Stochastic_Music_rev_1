      PROGRAM FREE_STOCHASTIC_MUSIC
C	  
C     SOURCE CODE AS GIVEN IN XENAKIS, FORMALIZED MUSIC, REV. ED. 
C     PP. 145-52, SLIGHTLY MODIFIED FOR COMPATIBILITY WITH
C     APPROXIMATRIX SIMPLY FORTRAN 3, V 3.27. INPUT DATA FROM PP.
C     151-2 IS READ IN FROM FILE DATA.TXT. NOTE THAT DATA ON P. 152,
C     LINE 20 IS MISSING. THIS DATA, FOR KTEST3,KTEST1,KTEST2 SHOULD
C     BE 000000000 IF THE TESTS ARE OFF, 001001001 IF THEY ARE ON.
C     NOTE ALSO THAT STATEMENT AT LABEL 520 (P. 149) SHOULD OMIT
C     "RANF(-1)", WHICH OCCURS IN THE REV. ED. OF FORMALIZED MUSIC
C     BUT NOT IN THE 1971 ED. LINE NUMBERS "XEN xxx" ARE RETAINED
C     FOR REFERENCE TO FORMALIZED MUSIC (INCLUDING ANY GAPS IN THE
C     ORIGINAL NUMBERING SYSTEM). COMMENTS THAT I HAVE INTERPOLATED
C     ARE LEFT UNNUMBERED.
C
C     PROGRAM FREE STOCHASTIC MUSIC (FORTRAN IV)                        XEN    6
C                                                                       XEN    7
C     GLOSSARY OF THE PRINCIPAL ABBREVIATIONS                           XEN    8
C
C     A - DURATION OF EACH SEQUENCE IN SECONDS                          XEN    9
C     A10,A20,A17,A35,A30 - NUMBERS FOR GLISSANDO CALCULATION           XEN   10
C     ALEA - PARAMETER USED TO ALTER THE RESULT OF A SECOND RUN WITH    XEN   11
C     THE SAME INPUT DATA                                               XEN   12
C     ALFA(3) - THREE EXPRESSIONS ENTERING INTO THE THREE SPEED VALUES  XEN   13
C     OF THE SLIDING TONES (GLISSANDI)                                  XEN   14
C     ALIM - MAXIMUM LIMIT OF SEQUENCE DURATION A                       XEN   15
C     (AMAX(I),I=1,KTR) TABLE OF AN EXPRESSION ENTERING INTO THE        XEN   16
C     CALCULATION OF THE NOTE LENGTH IN PART 8                          XEN   17
C     BF - DYNAMIC FORM NUMBER, THE LIST IS ESTABLISHED INDEPENDENTLY   XEN   18
C     OF THIS PROGRAM AND IS SUBJECT TO MODIFICATION                    XEN   19
C     DELTA - THE RECIPROCAL OF THE MEAN DENSITY OF SOUND EVENTS DURING XEN   20
C     A SEQUENCE OF DURATION A                                          XEN   21
C     (E(I,J),I=1,KTR,J=1,KTE) - PROBABILITIES OF THE KTR TIMBRE        XEN   22
C     CLASSES INTRODUCED AS INPUT DATA, DEPENDING ON THE CLASS NUMBER   XEN   23
C     I=KR AND ON THE POWER J=U OBTAINED FROM V3*EXP(U)=DA              XEN   24
C     EPSI - EPSILON FOR ACCURACY IN CALCULATING PN AND E(I,J), WHICH   XEN   25
C     IT IS ADVISABLE TO RETAIN.                                        XEN   26
C     (GN(I,J),I=1,KTR,J=1,KTS) - TABLE OF THE GIVEN LENGTH OF BREATH   XEN   27
C     FOR EACH INSTRUMENT, DEPENDING ON CLASS I AND INSTRUMENT J        XEN   28
C     GTNA - GREATEST NUMBER OF NOTES IN THE SEQUENCE OF DURATION A     XEN   29
C     GTNS - GREATEST NUMBER OF NOTES IN KW LOOPS                       XEN   30
C     (HAMIN(I,J),HAMAX(I,J),HBMIN(I,J),HBMAX(I,J),I=1,KTR,J=1,KTS)     XEN   31
C     TABLE OF INSTRUMENT COMPASS LIMITS, DEPENDING ON TIMBRE CLASS I   XEN   32
C     AND INSTRUMENT J. TEST INSTRUCTION 480 IN PART 6 DETERMINES       XEN   33
C     WHETHER THE HA OR THE HB TABLE IS FOLLOWED. THE NUMBER 7 IS       XEN   34
C     ARBITRARY.                                                        XEN   35
C     JW - ORDINAL NUMBER OF THE SEQUENCE COMPUTED.                     XEN   36
C     KNL - NUMBER OF LINES PER PAGE OF THE PRINTED RESULT. KNL=50      XEN   37
C     KR1 - NUMBER IN THE CLASS KR=I USED FOR PERCUSSION OR             XEN   38
C     INSTRUMENTS WITHOUT A DEFINITE PITCH.                             XEN   39
C     KTE - POWER OF THE EXPONENTIAL COEFFICIENT E SUCH THAT            XEN   40
C     DA(MAX)=V3*(E**(KTE-1))                                           XEN   41
C     KTR - NUMBER OF TIMBRE CLASSES                                    XEN   42
C     KW - MAXIMUM NUMBER OF JW                                         XEN   43
C     KTEST1,TAV1,ETC. - EXPRESSIONS USEFUL IN CALCULATING HOW LONG     XEN   44
C     THE VARIOUS PARTS OF THE PROGRAM WILL RUN.                        XEN   45
C     KT1 - ZERO IF THE PROGRAM IS BEING RUN, NONZERO DURING DEBUGGING  XEN   46
C     KT2 - NUMBER OF LOOPS, EQUAL TO 15 BY ARBITRARY DEFINITION.       XEN   47
C     (MODI(IX8),IX8=7,1) AUXILIARY FUNCTION TO INTERPOLATE VALUES      XEN   48
C     IN THE TETA(256) TABLE (SEE PART 7)                               XEN   49
C     NA - NUMBER OF SOUNDS CALCULATED FOR THE SEQUENCE A (NA=DA*A)     XEN   50
C     (NT(I),I=1,KTR) NUMBER OF INSTRUMENTS ALLOCATED TO EACH OF THE    XEN   51
C     KTR TIMBRE CLASSES.                                               XEN   52
C     (PN(I,J),I=1,KTR,J=1,KTS),(KTS=NT(I),I=1,KTR) TABLE OF            XEN   53
C     PROBABILITY OF EACH INSTRUMENT OF THE CLASS I.                    XEN   54
C     (Q(I),I=1,KTR) PROBABILITIES OF THE KTR TIMBRE CLASSES,           XEN   55
C     CONSIDERED AS LINEAR FUNCTIONS OF THE DENSITY DA.                 XEN   56
C     (S(I),I=1,KTR) SUN OF THE SUCCESSIVE Q(I) PROBABILITIES, USED     XEN   57
C     TO CHOOSE THE CLASS KR BY COMPARING IT TO A RANDOM NUMBER X1      XEN   58
C     (SEE PART 3, LOOP 380 AND PART 5, LOOP 430).                      XEN   59
C     SINA - SUM OF THE COMPUTED NOTES IN THE NEW CLOUDS NA, ALWAYS     XEN   60
C     LESS THAN GTNS (SEE TEST IN PART 10).                             XEN   61
C     SQPI - SQUARE ROOT OF PI (3.14159...)                             XEN   62
C     TA - SOUND ATTACK TIME ABCISSA.                                   XEN   63
C     TETA(256) - TABLE OF THE 256 VALUES OF TEH INTEGRAL OF THE        XEN   64
C     NORMAL DISTRIBUTION CURVE WHICH IS USEFUL IN CALCULATING          XEN   65
C     GLISSANDO SPEED AND SOUND EVENT DURATION.                         XEN   66
C     VIGL - GLISSANDO SPEED (VITESSE GLISSANDO), WHICH CAN VARY AS,    XEN   67
C     BE INDEPENDENT OF, OR VARY INVERSELY AS THE DENSITY OF THE        XEN   68
C     SEQUENCE, THE ACTUAL MODE OF VARIATION EMPLOYED REMAINING THE     XEN   69
C     SAME FOR THE ENTIRE SEQUENCE (SEE PART 7).                        XEN   70
C     VITLIM - MAXIMUM LIMITING GLISSANDO SPEED (IN SEMITONES/SEC),     XEN   71
C     SUBJECT TO MODIFICATION.                                          XEN   72
C     V3 - MINIMUM CLOUD DENSITY DA                                     XEN   73
C     (Z1(I),Z2(I),I=1,8) TABLE COMPLEMENTARY TO THE TETA TABLE.        XEN   74
C                                                                       XEN   75
C                                                                       XEN   76
C     READ CONSTANTS AND TABLES                                         XEN   77
C                                                                       XEN   78
      DIMENSION Q(12),S(12),E(12,12),PN(12,50),SPN(12,50),NT(12),       XEN   79
     *HAMIN(12,50),HAMAX(12,50),HBMIN(12,50),HBMAX(12,50),GN(12,50),    XEN   80
     *H(12,50),TETA(256),VIGL(3),MODI(7),Z1(8),Z2(8),ALFA(3),AMAX(12)   XEN   81
C                                                                       XEN   82
C                                                                       XEN   83
C                                                                       XEN   84
      I=1                                                               XEN   85
      DO 10 IX=1,7                                                      XEN   86
      IX8=8-IX                                                          XEN   87
      MODI(IX8)=I                                                       XEN   88
10	  I=I+1                                                             XEN   89
C                                             	                        XEN   90
C OPENS DATA FILE       
      OPEN(UNIT=1,FILE="DATA.TXT")
      READ(1,20) (TETA(I),I=1,256)                                      XEN   91
20	  FORMAT(12F6.6)                                                    XEN   92
C Z1(I) AND Z2(I) ARE READ FROM DATA.TXT IN ALTERNATION
      READ(1,30) (Z1(I),Z2(I),I=1,8)                                    XEN   93
30	  FORMAT(6(F3.2,F9.8)/F3.2,F9.8,E6.2,F9.8)                          XEN   94
C OPENS FILE FOR OUTPUT
      OPEN(UNIT=2,FILE="OUTPUT.TXT",FORM="FORMATTED")
C WRITES TO FILE: ALL OF THE TETA ARRAY, ALL OF Z1, ALL OF Z2
      WRITE(2,40) TETA,Z1,Z2                                            XEN   95
40	  FORMAT(' THE TETA TABLE = ',/,21(12F10.6,/),4F10.6,//,            XEN   96
     *' THE Z1 TABLE = ',/,7F6.2,E12.3,/,' THE Z2 TABLE = ',/,          XEN   97
     *8F14.8,/)                                                         XEN   98
      READ(1,50) DELTA,V3,A10,A20,A17,A30,A35,BF,SQPI,EPSI,VITLIM,      XEN   99
     *ALEA,ALIM                                                         XEN  100
50    FORMAT(F3.0,F3.3,5F3.1,F2.0,F8.7,F8.8,F4.2,F8.8,F5.2)             XEN  114
      READ(1,60) KT1,KT2,KW,KNL,KTR,KTE,KR1,GTNA,GTNS,(NT(I),I=1,KTR)   XEN  115
60    FORMAT(5I3,2I2,2F6.0,12I2)                                        XEN  126
      WRITE(2,70) DELTA,V3,A10,A20,A17,A30,A35,BF,SQPI,EPSI,VITLIM,ALEA,XEN  127
     *ALIM,KT1,KT2,KW,KNL,KTR,KTE,KR1,GTNA,GTNS,((I),NT(I),I=1,KTR)     XEN  128
70    FORMAT(' DELTA = ',F4.0,/,' V3 = ',F6.3,/,' A10 = ',F4.1,/,       XEN  129
     *' A20 = ',F4.1,/,' A17 = ',F4.1,/,' A30 = ',F4.1,/,' A35 = ',F4.1,XEN  130
     */,' BF = ',F3.0,/,' SQPI =',F11.8,/,' EPSI =',F12.8,/,' VITLIM = 'XEN  131
     *,F5.2,/,' ALEA =',F12.8,/,' ALIM = ',F6.2,/,' KT1 = ',I3,/,       XEN  132
     *' KT2 = ',I3,/,' KW = ',I3,/,' KNL = ',I3,/,' KTR = ',I3,/,       XEN  133
     *' KTE = ',I2,/,' KR1 = ',I2,/,' GTNA = ',F7.0,/,' GTNS = ',F7.0,  XEN  134
     */,12(' IN CLASS ',I2,', THERE ARE ',I2,' INSTRUMENTS.',/))        XEN  135
      READ(1,80) KTEST3,KTEST1,KTEST2                                   XEN  136
80	  FORMAT(3I3)                                                       XEN  141
      WRITE(2,90) KTEST3,KTEST1,KTEST2                                  XEN  142
90	  FORMAT(' KTEST3 = ',I3,/,' KTEST1 = ',I3,/,' KTEST2 = ',I3)       XEN  143
C                                                                       XEN  144
      IF(KTEST3.NE.0) WRITE(2,830)                                      XEN  145
      R=KTE-1                                                           XEN  146
      A10=A10*SQPI                                                      XEN  147
      A20=A20*SQPI/R                                                    XEN  148
      A30=A30*SQPI                                                      XEN  149
C     IF ALEA IS NON-ZERO, THE RANDOM NUMBER IS GENERATED FROM THE TIME XEN  150
C     WHEN THE FOLLOWING INSTRUCTION IS EXECUTED. IF ALEA IS NON-ZERO   XEN  151
C     EACH RUN OF THIS PROGRAM WILL PRODUCE DIFFERENT OUTPUT DATA.      XEN  152
      IF(ALEA.NE.0.0) CALL SRAND(TIME())                                XEN  153
      WRITE(2,830)                                                      XEN  154
      DO 130 I=1,KTR                                                    XEN  155
      Y=0.0                                                             XEN  156
      KTS=NT(I)                                                         XEN  157
C     IN THIS SECTION, I = TIMBRE CLASS AND J = INSTRUMENT
      READ(1,100) (HAMIN(I,J),HAMAX(I,J),HBMIN(I,J),HBMAX(I,J),GN(I,J), XEN  158
     *PN(I,J),J=1,KTS)                                                  XEN  159
100	  FORMAT(5(5F2.0,F3.3))                                             XEN  160
      WRITE(2,110) I,(J,HAMIN(I,J),HAMAX(I,J),HBMIN(I,J),HBMAX(I,J),    XEN  161
     *GN(I,J),PN(I,J),J=1,KTS)                                          XEN  162
110	  FORMAT(/,' IN CLASS NUMBER ',I2,/,(' FOR INSTRUMENT NO. ',I2,     XEN  163
     *' HAMIN = ',F3.0,', HAMAX = ',F3.0,', HBMIN = ',F3.0,', HBMAX = ',XEN  164
     *F3.0,', GN = ',F3.0,', AND PN = ',F6.3))                          XEN  165
C     LINES 166-9 SUM PROBABILITIES FOR THE INSTRUMENTS IN EACH TIMBRAL
C     CLASS AND VERIFY THAT THEY ARE WITHIN 0.01 (EPSI) OF 1.0
      DO 120 J=1,KTS                                                    XEN  166
      Y=Y+PN(I,J)                                                       XEN  167
120	  SPN(I,J)=Y                                                        XEN  168
130	  IF(ABS(Y-1.0).GE.EPSI) CALL EXIT()                                XEN  169
C                                                                       XEN  170
C     IN THIS SECTION, I = TIMBRE CLASS AND J = DENSITY LEVEL
      DO 150 I=1,KTR                                                    XEN  171
      READ(1,140) (E(I,J),J=1,KTE)                                      XEN  172
140	  FORMAT(12F2.2)                                                    XEN  173
150	  WRITE(2,160) I,(J,E(I,J),J=1,KTE)                                 XEN  174
160	  FORMAT(/,' CLASS NUMBER ',I2,/,(' IN DENSITY LEVEL ',I2,          XEN  175
     *' HAS A PROBABILITY OF ',F6.2))                                   XEN  176
C     LINES 177-80 SUM PROBABILITIES OF THE TIMBRAL CLASSES FOR EACH
C     DENSITY LEVEL AND VERIFY THAT THEY ARE WITHIN 0.01 (EPSI) OF 1.0
      DO 180 J=1,KTE                                                    XEN  177
      Y=0.0                                                             XEN  178
      DO 170 I=1,KTR                                                    XEN  179
170	  Y=Y+E(I,J)                                                        XEN  180
180	  IF(ABS(Y-1.0).GE.EPSI) CALL EXIT()                                XEN  181
C     LINES 182-91 DETERMINE THE MAX DURATION OF NOTES FOR EACH TIMBRE
C     CLASS (AMAX). OUTPUT DISPLAYS CANDIDATE VALUES FOR EACH CLASS
C     FOLLOWED BY THE HIGHEST VALUE FOR EACH CLASS. THE HIGHEST AMAX
C     VALUE FOR EACH CLASS IS USED IN PART 8. AMAX DEPENDS ON THE 
C     PROBABILITY OF NOTES IN EACH CLASS FOR EACH LEVEL OF DENSITY.
C     THE LOWER THE PROBABILITY AND THE LOWER THE DENSITY, THE GREATER
C     THE DURATIONS OF INDIVIDUAL NOTES MAY BE.
      DO 200 I=1,KTR                                                    XEN  182
      AMAX(I)=1.0/E(I,1)                                                XEN  183
C     INTERPOLATED WRITE STATEMENT HERE
      IF(KT1.NE.0) WRITE(2,181) I
181	  FORMAT(/,' AMAX CANDIDATES FOR CLASS ',I2,':')      
      DO 200 J=2,KTE                                                    XEN  184
      AJ=J-1                                                            XEN  185
      AX=1.0/(E(I,J)*EXP(AJ))                                           XEN  186
      IF(KT1.NE.0) WRITE(2,190) AX                                      XEN  187
190	  FORMAT(E15.8)                                                     XEN  188
200	  IF(AX.GT.AMAX(I)) AMAX(I)=AX                                      XEN  189
C     INTERPOLATED WRITE STATEMENT BELOW
      IF(KT1.NE.0) WRITE(2,201)
201	  FORMAT(/,' FINAL AMAX VALUES FOR CLASSES 1-12:')
      IF(KT1.NE.0) WRITE(2,210) AMAX                                    XEN  190
210	  FORMAT(6E15.8)                                                    XEN  191
C     INTERPOLATED DATE AND TIME STAMP HERE
      WRITE(2,211) CTIME(TIME())
211   FORMAT(/, A25)
C                                                                       XEN  192
      JW=1                                                              XEN  193
      SINA=0.0                                                          XEN  194
      IF(KTEST1.NE.0) TAV1=TIME()                                       XEN  195
220	  NLINE=50                                                          XEN  196
C                                                                       XEN  197
C     PARTS 1 AND 2, DEFINE SEQUENCE A SECONDS AND CLOUD NA DURING A    XEN  198
C                                                                       XEN  199
      KNA=0                                                             XEN  200
      K1=0                                                              XEN  201
C     ALGORITHM FOR DETERMINING THE LENGTH (A) OF THE SECTION OF MUSIC
230	  X1=RAND(0)                                                        XEN  202
      A=-DELTA*LOG(X1)                                                  XEN  203
      IF(A.LE.ALIM) GO TO 250                                           XEN  204
      IF(K1.GE.KT2) GO TO 240                                           XEN  205
      K1=K1+1                                                           XEN  206
      GO TO 230                                                         XEN  207
240	  A=ALIM/2.0                                                        XEN  208
      X1=0.0                                                            XEN  209
C     ALGORITHM TO CALCULATE DENSITY (DA) AND NUMBER OF SOUNDS (NA)
250	  K2=0                                                              XEN  210
260	  X2=RAND(0)                                                        XEN  211
      IF(JW.GT.1) GO TO 280                                             XEN  212
270	  UX=R*X2                                                           XEN  213
      GO TO 310                                                         XEN  214
280	  IF(RAND(0).GE.0.5) GO TO 290                                      XEN  215
      UX=UPR+R*(1.0-SQRT(X2))                                           XEN  216
      GO TO 300                                                         XEN  217
290	  UX=UPR-R*(1.0-SQRT(X2))                                           XEN  218
300	  IF((UX.GE.0.0).AND.(UX.LE.R)) GO TO 310                           XEN  219
      IF(K2.GE.KT2) GO TO 270                                           XEN  220
      K2=K2+1                                                           XEN  221
      GO TO 260                                                         XEN  222
310	  U=UX                                                              XEN  223
      DA=V3*EXP(U)                                                      XEN  224
      NA=INT(A*DA+0.5)+1                                                XEN  225
      IF(GTNA.GT.FLOAT(NA)) GO TO 330                                   XEN  226
      IF(KNA.GE.KT2) GO TO 320                                          XEN  227
      KNA=KNA+1                                                         XEN  228
      GO TO 230                                                         XEN  229
320	  A=DELTA                                                           XEN  230
      GO TO 260                                                         XEN  231
330	  UPR=U                                                             XEN  232
      IF(KT1.EQ.0) GO TO 360                                            XEN  233
      WRITE(2,340) JW,KNA,K1,K2,X1,X2,A,DA,NA                           XEN  234
340	  FORMAT(4I8,4E18.8,I8)                                             XEN  235
      NA=KT1                                                            XEN  236
      IF(KTEST3.NE.0) WRITE(2,350) JW,NA,A                              XEN  237
350	  FORMAT(2I9,F10.2)                                                 XEN  238
C                                                                       XEN  239
C     PART 3. DEFINE CONSTITUTION OF ORCHESTRA DURING SEQUENCE A        XEN  240
C                                                                       XEN  241
360	  SINA=SINA+FLOAT(NA)                                               XEN  242
      XLOGDA=U                                                          XEN  243
      XALOG=A20*XLOGDA                                                  XEN  244
      M=INT(XLOGDA)                                                     XEN  245
      IF((M+2).GT.KTE) M=KTE-2                                          XEN  246
      SR=0.0                                                            XEN  247
      M1=M+1                                                            XEN  248
      M2=M+2                                                            XEN  249
      DO 380 I=1,KTR                                                    XEN  250
      ALFX=E(I,M1)                                                      XEN  251
      BETA=E(I,M2)                                                      XEN  252
      XM=M                                                              XEN  253
      QR=(XLOGDA-XM)*(BETA-ALFX)+ALFX                                   XEN  254
      IF(KT1.NE.0) WRITE(2,370) XM,ALFX,BETA                            XEN  255
370	  FORMAT(3F20.8)                                                    XEN  256
      Q(I)=QR                                                           XEN  257
      SR=SR+QR                                                          XEN  258
380	  S(I)=SR                                                           XEN  259
      IF(KT1.NE.0) WRITE(2,390) (Q(I),I=1,KTR),(S(I),I=1,KTR)           XEN  260
390	  FORMAT(12F9.4)                                                    XEN  261
C                                                                       XEN  262
C     PART 4, DEFINE INSTANT TA OF EACH POINT IN SEQUENCE A             XEN  263
C                                                                       XEN  264
      IF(KTEST2.NE.0) TAV2=TIME()                                       XEN  265
      N=1                                                               XEN  266
C     INTERPOLATED WRITE STATEMENT HERE
C      WRITE(2,391) N
C391	  FORMAT(' BEFORE LINE 267, N = ',I8)
      T=0.0                                                             XEN  267
C     INTERPOLATED WRITE STATEMENT HERE
C      WRITE(2,392) T
C392	  FORMAT(' BEFORE LINE 268, T = ',E20.8)
      TA=0.0                                                            XEN  268
C     INTERPOLATED WRITE STATEMENT HERE
C      WRITE(2,393) TA
C393	  FORMAT(' BEFORE LINE 269, TA = ',E20.8)      
      GO TO 410                                                         XEN  269
400	  N=N+1                                                             XEN  270
C     INTERPOLATED WRITE STATEMENT HERE
C      WRITE(2,401) N
C401	  FORMAT(' BEFORE LINE 271, N = ',I8)
      X=RAND(0)                                                         XEN  271
C     INTERPOLATED WRITE STATEMENT HERE
C      WRITE(2,402) X
C402	  FORMAT(' BEFORE LINE 272, X = ',E20.8)      
      T=-LOG(X)/DA                                                      XEN  272
C     INTERPOLATED WRITE STATEMENT HERE
C      WRITE(2,403) T
C403	  FORMAT(' BEFORE LINE 273, T = ',E20.8)      
      TA=TA+T                                                           XEN  273
C     INTERPOLATED WRITE STATEMENT HERE
C      WRITE(2,404) TA
C404	  FORMAT(' BEFORE LINE 274, TA = ',E20.8)      
410	  IF(KT1.NE.0) WRITE(2,420) N,X,T,TA                                XEN  274
420	  FORMAT(I8,3E20.8)                                                 XEN  275
C                                                                       XEN  276
C     PART 5, DEFINE CLASS AND INSTRUMENT NUMBER TO EACH POINT OF A     XEN  277
C                                                                       XEN  278
      X1=RAND(0)                                                        XEN  279
      DO 430 I=1,KTR                                                    XEN  280
430	  IF(X1.LE.S(I)) GO TO 440                                          XEN  281
      I=KTR                                                             XEN  282
440	  KTS=NT(I)                                                         XEN  283
      KR=I                                                              XEN  284
      X2=RAND(0)                                                        XEN  285
      DO 450 J=1,KTS                                                    XEN  286
      SPIEN=SPN(KR,J)                                                   XEN  287
      INSTRM=J                                                          XEN  288
450	  IF(X2.LE.SPIEN) GO TO 460                                         XEN  289
      INSTRM=KTS                                                        XEN  290
460	  PIEN=PN(KR,INSTRM)                                                XEN  291
      IF(KT1.NE.0) WRITE(2,470) X1,S(KR),KR,X2,SPIEN,INSTRM             XEN  292
470	  FORMAT(2E20.8,I6,2E20.8,I6)                                       XEN  293
C                                                                       XEN  294
C     PART 6, DEFINE PITCH HN FOR EACH POINT OF SEQUENCE A              XEN  295
C                                                                       XEN  296
      IF(KR.GT.1) GO TO 480                                             XEN  297
      IF(INSTRM.GE.KR1) GO TO 490                                       XEN  298
      HX=0.0                                                            XEN  299
      GO TO 560                                                         XEN  300
480	  IF(KR.LT.7) GO TO 490                                             XEN  301
      HSUP=HBMAX(KR,INSTRM)                                             XEN  302
      HINF=HBMIN(KR,INSTRM)                                             XEN  303
      GO TO 500                                                         XEN  304
490	  HSUP=HAMAX(KR,INSTRM)                                             XEN  305
      HINF=HAMIN(KR,INSTRM)                                             XEN  306
500	  HM=HSUP-HINF                                                      XEN  307
      HPR=H(KR,INSTRM)                                                  XEN  308
      K=0                                                               XEN  309
      IF(HPR.LE.0.0) GO TO 520                                          XEN  310
510	  X=RAND(0)                                                         XEN  311
      IF(N.GT.1) GO TO 530                                              XEN  312
520	  HX=HINF+HM*X                                                      XEN  313
      GO TO 560                                                         XEN  314
530	  IF(RAND(0).GE.0.5) GO TO 540                                      XEN  315
      HX=HPR+HM*(1.0-SQRT(X))                                           XEN  316
      GO TO 550                                                         XEN  317
540	  HX=HPR-HM*(1.0-SQRT(X))                                           XEN  318
550	  IF((HX.GE.HINF).AND.(HX.LE.HSUP)) GO TO 560                       XEN  319
      IF(K.GE.KT2) GO TO 520                                            XEN  320
      K=K+1                                                             XEN  321
      GO TO 510                                                         XEN  322
560	  H(KR,INSTRM)=HX                                                   XEN  323
      IF(KT1.NE.0) WRITE(2,570) K,X,HX                                  XEN  324
570	  FORMAT(I6,2E20.8)                                                 XEN  325
C                                                                       XEN  326
C     PART 7, DEFINE SPEED VIGL TO EACH POINT OF A                      XEN  327
C                                                                       XEN  328
      IF(KR.EQ.5) GO TO 580                                             XEN  329
      VIGL(1)=0.0                                                       XEN  330
      VIGL(2)=0.0                                                       XEN  331
      VIGL(3)=0.0                                                       XEN  332
      X1=0.0                                                            XEN  333
      X2=0.0                                                            XEN  334
      XLAMBDA=0.0                                                       XEN  335
      GO TO 740                                                         XEN  336
580	  KX=1                                                              XEN  337
590	  X1=RAND(0)                                                        XEN  338
      IF(X1-0.9997) 600,650,680                                         XEN  339
600	  I=128                                                             XEN  340
      DO 630 IX=1,7                                                     XEN  341
      IF(TETA(I)-X1) 610,640,620                                        XEN  342
610	  I=I+MODI(IX)                                                      XEN  343
      GO TO 630                                                         XEN  344
620	  I=I-MODI(IX)                                                      XEN  345
630	  CONTINUE                                                          XEN  346
      IF(TETA(I)-X1) 670,640,660                                        XEN  347
640	  XLAMBDA=FLOAT(I-1)/100.0                                          XEN  348
      GO TO (720,760),KX                                                XEN  349
650	  XLAMBDA=2.55                                                      XEN  350
      GO TO (720,760),KX                                                XEN  351
660	  I=I-1                                                             XEN  352
670	  TX1=TETA(I)                                                       XEN  353
      XLAMBDA=(FLOAT(I-1)+(X1-TX1)/(TETA(I+1)-TX1))/100.0               XEN  354
      GO TO (720,760),KX                                                XEN  355
680	  DO 690 I=2,7                                                      XEN  356
      TX1=Z2(I)                                                         XEN  357
      IF(X1-TX1) 700,710,690                                            XEN  358
690	  CONTINUE                                                          XEN  359
      I=8                                                               XEN  360
      TX1=1.0                                                           XEN  361
700	  TX2=Z1(I)                                                         XEN  362
      XLAMBDA=TX2-((TX1-X1)/(TX1-Z2(I-1)))*(TX2-Z1(I-1))                XEN  363
      GO TO (720,760),KX                                                XEN  364
710	  XLAMBDA=Z1(I)                                                     XEN  365
      GO TO (720,760),KX                                                XEN  366
720	  ALFA(1)=A10+XALOG                                                 XEN  367
      ALFA(3)=A30-XALOG                                                 XEN  368
      X2=RAND(0)                                                        XEN  369
      ALFA(2)=A17+A35*X2                                                XEN  370
      DO 730 I=1,3                                                      XEN  371
      VIGL(I)=INT(ALFA(I)*XLAMBDA+0.5)                                  XEN  372
      IF(VIGL(I).LT.0.0) VIGL(I)=-VIGL(I)                               XEN  373
      IF(VIGL(I).GT.VITLIM) VIGL(I)=VITLIM                              XEN  374
730	  IF(RAND(0).LT.0.5) VIGL(I)=-VIGL(I)                               XEN  375
740	  IF(KT1.NE.0) WRITE(2,750) X1,X2,XLAMBDA,VIGL                      XEN  376
750	  FORMAT(6E19.8)                                                    XEN  377
C                                                                       XEN  378
C     PART 8, DEFINE DURATION FOR EACH POINT OF A                       XEN  379
C                                                                       XEN  380
      IF((KR.EQ.7).OR.(KR.EQ.8)) GO TO 780                              XEN  381
      ZMAX=AMAX(KR)/(V3*PIEN)                                           XEN  382
      G=GN(KR,INSTRM)                                                   XEN  383
      RO=G/LOG(ZMAX)                                                    XEN  384
      QPNDA=1.0/(Q(KR)*PIEN*DA)                                         XEN  385
      GE=ABS(RO*LOG(QPNDA))                                             XEN  386
      XMU=GE/2.0                                                        XEN  387
      SIGMA=GE/4.0                                                      XEN  388
      KX=2                                                              XEN  389
      GO TO 590                                                         XEN  390
760	  TAU=SIGMA*XLAMBDA*1.4142                                          XEN  391
      X2=RAND(0)                                                        XEN  392
      IF(X2.GE.0.5) GO TO 770                                           XEN  393
      XDUR=XMU+TAU                                                      XEN  394
      GO TO 790                                                         XEN  395
770	  XDUR=XMU-TAU                                                      XEN  396
      IF(XDUR.GE.0.0) GO TO 790                                         XEN  397
780	  XDUR=0.0                                                          XEN  398
790	  IF(KT1.NE.0) WRITE(2,800) ZMAX,XMU,SIGMA,X1,XLAMBDA,X2,XDUR       XEN  399
800	  FORMAT(5E15.8,E11.4,E15.8)                                        XEN  400
C                                                                       XEN  401
C     PART 9, DEFINE INTENSITY FORM TO EACH POINT OF A                  XEN  402
C                                                                       XEN  403
      IFORM=INT(RAND(0)*BF+0.5)                                         XEN  404
      IF(KT1.EQ.0) GO TO 840                                            XEN  405
      IF(NLINE.LT.KNL) GO TO 810                                        XEN  406
      IF(NLINE.EQ.KNL) GO TO 820                                        XEN  407
      NLINE=1                                                           XEN  408
      GO TO 900                                                         XEN  409
810	  NLINE=NLINE+1                                                     XEN  410
      GO TO 900                                                         XEN  411
820	  WRITE(2,830)                                                      XEN  412
830	  FORMAT(/)                                                         XEN  413
      NLINE=0                                                           XEN  414
      GO TO 900                                                         XEN  415
840	  IF(NLINE.GE.KNL) GO TO 850                                        XEN  416
      NLINE=NLINE+1                                                     XEN  417
      GO TO 880                                                         XEN  418
850	  WRITE(2,860) JW,A,NA,(Q(I),I=1,KTR)                               XEN  419
860	  FORMAT(/,'JW=',I3,4X,'A=',F8.2,4X,'NA=',I6,4X,'Q(I)=',            XEN  420
     *12(F4.2,'/'),/)                                                   XEN  421
      WRITE(2,870)                                                      XEN  422
870	  FORMAT(6X,'N',8X,'START',5X,'CLASS',4X,'INSTRM',4X,'PITCH',6X,    XEN  423
     *'GLISS1',4X,'GLISS2',4X,'GLISS3',8X,'DURATION',5X,'DYNAM')        XEN  424
      NLINE=1                                                           XEN  425
880	  WRITE(2,890) N,TA,KR,INSTRM,HX,(VIGL(I),I=1,3),XDUR,IFORM         XEN  426
890	  FORMAT(I7,F12.2,I9,I8,F11.1,F13.1,2F10.1,F14.2,I11)               XEN  427
C                                                                       XEN  428
C     PART 10, REPEAT SAME DEFINITIONS FOR ALL POINTS OF A              XEN  429
C                                                                       XEN  430
900	  IF(N.LT.NA) GO TO 400                                             XEN  431
C                                                                       XEN  432
C     PART 11, REPEAT SEQUENCES A                                       XEN  433
C                                                                       XEN  434
      IF(KTEST2.EQ.0) GO TO 910                                         XEN  435
      TAP2=TIME()-TAV2                                                  XEN  436
      TAP2=TAP2/FLOAT(NA)                                               XEN  437
      WRITE(2,750) TAP2                                                 XEN  438
C                                                                       XEN  439
910	  IF(JW.GE.KW) GO TO 930                                            XEN  440
920	  JW=JW+1                                                           XEN  441
      IF(GTNS.GT.SINA) GO TO 220                                        XEN  442
930	  IF(KTEST1.EQ.0) CALL EXIT                                         XEN  443
940	  TAP1=TIME()-TAV1                                                  XEN  444
      TAP1=TAP1/FLOAT(KW)                                               XEN  445
      WRITE(2,750) TAP1                                                 XEN  446
C                                                                       XEN  447
C CLOSES FILES
      CLOSE(UNIT=1)
      CLOSE(UNIT=2)
      END                                                               XEN  448
      