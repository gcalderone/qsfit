;;Default comparison function
;;In both BASIC and SORTED mode t1 is a single structure. t2 is an
;;array of structures in BASIC mode and a single structure in SORTED
;;mode.
;;On output this function returns:
;;- BASIC mode: an array of BYTE (same number of elements as t2) with
;;  zeroes on matching rows, 1 on non-matching rows;
;;- SORTED mode: 0 for matching row, 1 if values from t2 are "bigger"
;;  than those from t1, -1 otherwise.
FUNCTION gjoin_score_default, t1, t2
 COMPILE_OPT IDL2
 ON_ERROR, !glib.on_error

  IF (gn(t2) GT 1) THEN BEGIN ;BASIC mode
     score = BYTE(t2.(0) EQ t1.(0))
     FOR i=1, N_TAGS(t1)-1 DO $
        score += BYTE(t2.(i) EQ t1.(i))
     score /= N_TAGS(t1)
     score = score XOR 1b
  ENDIF $
  ELSE BEGIN
     FOR i=0, N_TAGS(t1)-1 DO BEGIN
        score = LONG(t2.(i) GE t1.(i)) - LONG(t2.(i) LE t1.(i))
        IF (score NE 0) THEN BREAK
     ENDFOR
  ENDELSE

  RETURN, score
END








FUNCTION gjoin_score2ord, score, maxscore
 COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error

  IF (ABS(score) LE maxscore) THEN $
     ord = 0 $
  ELSE $
     ord = LONG(score / ABS(score))

  score = ABS(score)
  RETURN, ord
END





;+
;NAME:
;    gjoin
;
;PURPOSE:
;    Perform either a single, multiple or customized key(s) join of
;    two arrays (table A and table B).
;
;CALLING SEQUENCE:
;    ret = gjoin(<input from table A>, <input from table B>
;                [N1=n1] [N2=n2]
;                [FSCORE='score_function_name']
;                [MAXSCORE=score_threshold]
;                [/LEFT] [/RIGHT] 
;                [SUBSET=subset]
;                [/SORTED])
;
;INPUTS:
;    <input from table A> is either:
;         - an array of structures;
;         - any number (<=4) of arrays of data from table 1.
;
;    In the default configuration, base type should be of any type for
;    which IDL's builtin comparison operators (LE and GE) works,
;    i.e. numbers and strings.
;
;    <input from table B>: analogously for table B
;
;    Maximum number of parameters is 8.
;
;    table1_key_A: first key of table 1, array of either number or
;                  string (i.e. data which can be compared with IDL's
;                  bultin operator GE and LE)
;    table2_key_A: first key of table 2
;
;OPTIONAL INPUTS:
;    FSCORE=fscore: name of a user-provided function to compare keys
;      (see gjoin_coord.pro for an example);
;    MAXSCORE=maxscore: score threshold below which a match occur
;
;KEYWORDS:
;    LEFT : perform a LEFT join, i.e. include records from table 1 not
;           matched in table 2. Corresponding table 2 indices will be
;           set to -1
;    RIGHT: perform a RIGHT join,i.e. include records from table 2 not
;           matched in table 1. Corresponding table 1 indices will be
;           set to -1
;    SORTED: work in SORTED operational mode (see OPERATIONAL MODES
;    below).
;    VERBOSE: provides information about what is being done.
;
;OUTPUTS:
;    Return value: array of structure with two (or three) fields:
;             id1: indices in table 1 of matched records 
;             (-1 for non-matched records in a RIGHT join)
;             id2: indices in table 2 of matched records 
;             (-1 for non-matched records in a LEFT join)
;             [score]: values of score values which lead to a match
;             (NaN for non-matched rows in either a LEFT or RIGHT
;             join.
;    N1=n1: array of LONG with same number of elements as table
;           1. Each cell report how many times that element has been
;           matched.
;    N2=n2: array of LONG with same number of elements as table
;           2. Each cell report how many times that element has been
;           matched.
;
;OPERATIONAL MODES:
;   gjoin works in either BASIC or SORTED operational modes. By
;   default it uses the BASIC mode, unless the /SORTED Kw is set.
;
;   - BASIC: In the simplest case it compares each row from table A to
;     each row of table B. This is the less efficient operational
;     mode, but provides the correct solution in any case.
;     The BASIC mode can be optimized by using the SUBSET optional
;     input. SUBSET should be set to a structure containing two
;     fields: i1 and i2. Each field is a LIST of arrays of LONG,
;     containing indexes of cells in table A (subset.i1) and table B
;     (subset.i2) to be compared. Using the SUBSET kw allow to limit
;     the number of rows in table B against which entries from table A
;     are compared (see gjoin_coord.pro for an example).
;
;   - SORTED: Sort the input tables according to all keys, then scan
;     both tables synchronously to find matching rows. This mode is
;     particularly efficient, but has a limitation: there must be at
;     least a key break for each matched record, i.e. repeated keys
;     are allowed only on one side. If a key break on at least one
;     table is not detected after each match an error is issued.
;
;  Whatever the operational mode, gjoin scan both tables, calls a
;  comparison function and if the row matches it stores corresponding
;  row IDs. If either LEFT or RIGHT keys are given, IDs of non-matched
;  records in table 2 and 1 respectively, are added to output.
;
;  See gjoin_test for some examples.
;
;HISTORY:
;    Written (Mar 2012): Giorgio Calderone
;-
FUNCTION gjoin                           $
   , t1, t2, t3, t4, t5, t6, t7, t8      $
   , N1=nmatch1, N2=nmatch2              $
   , LEFT=left, RIGHT=right              $
   , FSCORE=fscore_in, MAXSCORE=maxscore $
   , SUBSET=subset                       $
   , SORTED=sorted                       $
   , VERBOSE=verbose
  COMPILE_OPT IDL2
  ON_ERROR, !glib.on_error


  ;;Input & KWs
  left    = KEYWORD_SET(left)
  right   = KEYWORD_SET(right)
  sorted  = KEYWORD_SET(sorted)
  verbose = KEYWORD_SET(verbose)




  ;;If input parameters are not array of structures...
  IF ((N_PARAMS() GT 2)   OR   (gtype(t1[0]) NE 'STRUCT')) THEN BEGIN
     ;;backup input parameters
     bkp_t1 = t1
     bkp_t2 = t2

     ;;Build up appropriate array of structures
     np = N_PARAMS() / 2

     ii = gn2s(INDGEN(np)+1)
     ss = 'f' + ii + ': t' + ii
     ss = 't1 = {' + STRJOIN(ss, ', ') + '}'
     dummy = EXECUTE(ss)
     t1 = greformstruct(t1)
     
     ii = gn2s(INDGEN(np)+1+np)
     ss = 'f' + ii + ': t' + ii
     ss = 't2 = {' + STRJOIN(ss, ', ') + '}'
     dummy = EXECUTE(ss)
     t2 = greformstruct(t2)
  ENDIF



  IF (~KEYWORD_SET(fscore_in)) THEN BEGIN
     fscore = 'gjoin_score_default'

     ;;When using default comparison functions both tables must have
     ;;same number of fields
     IF (N_TAGS(t1[0]) NE (N_TAGS(t2[0]))) THEN $
        MESSAGE, 'Input structures must have same number of elements'

     maxscore = 0               ;forced to be 0
  ENDIF $
  ELSE $
     fscore = fscore_in
  IF (gn(maxscore) EQ 0) THEN maxscore = 0




  ;;Number of elements in tables
  n1 = gn(t1)
  n2 = gn(t2)

  ;;Provide information about join task
  IF (verbose) THEN BEGIN
     time = JULDAY()
     PRINT, 'Input table A:', n1, ' rows, ', N_TAGS(t1[0]), ' fields'
     PRINT, 'Input table B:', n2, ' rows, ', N_TAGS(t2[0]), ' fields'

     type = ''
     IF (left ) THEN type += ' LEFT'
     IF (right) THEN type += ' RIGHT'
     IF (type EQ '') THEN type = 'INNER'
     PRINT, 'Join type: ', type

     score = '(built-in)'
     IF (KEYWORD_SET(fscore_in)) THEN $
        score = fscore + ', maxscore: ' + gn2s(maxscore)

     PRINT, 'Score function: ', score
  ENDIF



  ;;Prepare data according to operational mode

  IF (sorted) THEN BEGIN
     ;;In SORTED mode both input tables must be sorted. Store IDs of
     ;;sorted values in is1 and is2 arrays.

     IF (verbose) THEN PRINT, 'Sorting input table A...'
     CASE (N_TAGS(t1[0])) OF 
        1: is1 = SORT(t1.(0))
        2: is1 = CALL_FUNCTION('MULTISORT', t1.(0), t1.(1))
        3: is1 = CALL_FUNCTION('MULTISORT', t1.(0), t1.(1), t1.(2))
        4: is1 = CALL_FUNCTION('MULTISORT', t1.(0), t1.(1), t1.(2), t1.(3))
     ENDCASE

     IF (verbose) THEN PRINT, 'Sorting input table B...'
     CASE (N_TAGS(t2[0])) OF 
        1: is2 = SORT(t2.(0))
        2: is2 = CALL_FUNCTION('MULTISORT', t2.(0), t2.(1))
        3: is2 = CALL_FUNCTION('MULTISORT', t2.(0), t2.(1), t2.(2))
        4: is2 = CALL_FUNCTION('MULTISORT', t2.(0), t2.(1), t2.(2), t2.(3))
     ENDCASE
  ENDIF $    
  ELSE BEGIN
     ;;The BASIC mode needs a subset structure to work. If it is not
     ;;provided by user build the most simple one: i.e. a single, big
     ;;subset.
     IF (~KEYWORD_SET(subset)) THEN $
        subset = {i1: LIST(LINDGEN(n1)), i2: LIST(LINDGEN(n2)) }
  ENDELSE



  ;;Output variables
  id1 = LIST()
  id2 = LIST()
  ascore = LIST()
  ;id1.add, 0l
  ;id2.add, 0l
  ;ascore.add, 0.
  nmatch1 = LONARR(n1)
  nmatch2 = LONARR(n2)




  ;Begin matching rows
  IF (verbose) THEN PRINT, 'Matching tables...'

  
  IF (sorted) THEN BEGIN
     ;--- SORTED OPERATIONAL MODE ---
     i1 = 0
     i2 = 0
     ;;minscore1 = gnan()
     ;;minscore2 = gnan()
     REPEAT BEGIN
        last1 = (i1 EQ n1-1)
        last2 = (i2 EQ n2-1)

     
        ;;Compare values in tables
        score = CALL_FUNCTION(fscore, t1[is1[i1]], t2[is2[i2]])
        ord = gjoin_score2ord(score, maxscore)


        CASE (ord) OF 
           -1: i2 += 1          ;Increment appropriate pointer
           1 : i1 += 1
        
        
           0: BEGIN             ;MATCH!!!
              id1.add, /extract, is1[i1]
              id2.add, /extract, is2[i2]
              ascore.add, /extract, score
              nmatch1[i1] += 1
              nmatch2[i2] += 1


              
              ;;Search for key break in table 1
              IF (~last1) THEN BEGIN
                 score1 = CALL_FUNCTION(fscore, t1[is1[i1]], t1[is1[i1+1]])
                 ;;minscore1 = MIN([minscore1, score1], /nan)
                 ord1 = gjoin_score2ord(score1, maxscore)
              ENDIF $
              ELSE ord1 = 1


              ;;Search for key break in table 2
              IF (~last2) THEN BEGIN
                 score2 = CALL_FUNCTION(fscore, t2[is2[i2]], t2[is2[i2+1]])
                 ;;minscore2 = MIN([minscore2, score2], /nan)
                 ord2 = gjoin_score2ord(score2, maxscore)
              ENDIF $
              ELSE  ord2 = 1


              ;;Consistency check
              IF (ord1 EQ -1) THEN MESSAGE, 'Data in table 1 should be sorted'
              IF (ord2 EQ -1) THEN MESSAGE, 'Data in table 2 should be sorted'
              IF ((ord1 EQ 0)   AND   (ord2 EQ 0)) THEN $
                 MESSAGE, 'ERROR: There must be a key break for each match.'


              ;;Increment pointer(s)
              i1 += (~last1   AND   (last2   OR   ord2))
              i2 += (~last2   AND   (last1   OR   ord1))
           END
        ENDCASE

        IF (i1 EQ gn(t1)) THEN BREAK
        IF (i2 EQ gn(t2)) THEN BREAK
     ENDREP UNTIL (last1 AND last2)
  ENDIF


  IF (~sorted) THEN BEGIN
     ;--- BASIC OPERATIONAL MODE ---
     FOR iset=0, gn(subset.i1)-1 DO BEGIN
        IF (verbose) THEN $
           PRINT, FORMAT=gcfmt('Analyzing subset %d/%d\r') $
                  , iset+1, gn(subset.i1)

        FOR iiset1=0, gn((subset.i1)[iset])-1 DO BEGIN
           i1 = ((subset.i1)[iset])[iiset1]
           i2 = ((subset.i2)[iset])
           
           ;;Compare values in table 1 with those in table 2
           score = CALL_FUNCTION(fscore, t1[i1], t2[i2])
           
           imatch = WHERE(score LE maxscore)
           IF (imatch[0] NE -1) THEN BEGIN
              ;;MATCH!
              id1.add, /extract, REPLICATE(i1, gn(imatch))
              id2.add, /extract, i2[imatch]
              ascore.add, /extract, score[imatch]
              nmatch1[i1]         += gn(imatch)
              nmatch2[i2[imatch]] += 1
           ENDIF
        ENDFOR
     ENDFOR
  ENDIF




  ;OUTER LEFT join ?
  IF (left) THEN BEGIN
     i = WHERE(nmatch1 EQ 0)
     IF (i[0] NE -1) THEN BEGIN
        IF (sorted) THEN id1.add, /extract, is1[i] $
        ELSE             id1.add, /extract, i
        id2.add, /extract, REPLICATE(-1, gn(i))
        ascore.add, /extract, REPLICATE(gnan(), gn(i))
     ENDIF
  END

  ;OUTER RIGHT join ? 
  IF (right) THEN BEGIN
     i = WHERE(nmatch2 EQ 0)
     IF (i[0] NE -1) THEN BEGIN
        id1.add, /extract, REPLICATE(-1, gn(i))
        IF (sorted) THEN id2.add, /extract, is2[i] $
        ELSE             id2.add, /extract, i
        ascore.add, /extract, REPLICATE(gnan(), gn(i))
     ENDIF
  END


  id1   = id1.toArray()
  id2   = id2.toArray()
  score = ascore.toArray()



  IF (gn(id1) GT 0) THEN BEGIN
     ;;Identify best matches
     bestm = BYTARR(gn(id1)) + 1b
     
     IF (gsearch(nmatch1 GE 2, imm)) THEN BEGIN
        FOR i=0, gn(imm)-1 DO BEGIN
           j = imm[i]
           j = WHERE(id1 eq j)
           dummy = MIN(score[j], jj)
           bestj = j[jj]
           bestm[j] = 0
           bestm[bestj] = 1
        ENDFOR
     ENDIF


     IF (gsearch(nmatch2 GE 2, imm)) THEN BEGIN
        FOR i=0, gn(imm)-1 DO BEGIN
           j = imm[i]
           j = WHERE(id2 eq j)
           dummy = MIN(score[j], jj)
           bestj = j[jj]
           bestm[j] = 0
           bestm[bestj] = 1
        ENDFOR
     ENDIF
  ENDIF




  ;;Final log
  IF (verbose) THEN BEGIN
     time = (JULDAY() - time) * 3600. * 24
     PRINT
     PRINT, 'Matched rows in table A:', gn(WHERE(nmatch1 GE 1, /null))
     PRINT, '          multi-matched:', gn(WHERE(nmatch1 GE 2, /null))
     PRINT, 'Matched rows in table B:', gn(WHERE(nmatch2 GE 1, /null))
     PRINT, '          multi-matched:', gn(WHERE(nmatch2 GE 2, /null))
     PRINT, 'Rows in ouput: ', gn(id1)
     PRINT
     IF (KEYWORD_SET(fscore_in)   AND   gn(score) GT 10) THEN BEGIN
        PRINT, 'Score range: ', gminmax(score)
        PRINT, 'mean/stddev: ', MEAN(score), STDDEV(score)
        ;;gplot, /hist, score, xtitle='Score', {c_line:1}, xst=1
     ENDIF
     PRINT, 'Time: ', gn2s(time), ' sec'
  ENDIF



  ;Prepare output structure
  IF (gn(id1) EQ 0) THEN RETURN, []

  
  join = {id1: 0l, id2: 0l, score: 0., best: 0b}
  join = REPLICATE(join, gn(id1))
  join.id1 = id1
  join.id2 = id2
  join.score = score
  join.best = bestm




  ;;If input parameters has been modified restore original values
  IF (gn(bkp_t1) NE 0) THEN BEGIN
     t1 = bkp_t1
     t2 = bkp_t2
  ENDIF

  RETURN, join
END










PRO gjoin_test
  t1 = [1, 2, 3]
  t2 = [2, 3, 3, 4]

  join = gjoin(t1, t2)
  gps, join

  ;p, gcell(t1, join.id1), gcell(t2, join.id2)
  ;
  ;a = gshen()
  ;b = greadfits(gdatapath() + '/catalog/2008-Jul-16-The_FIRST_Survey_Catalog/first.fits.gz')
  ;join = gjoin_coord(a.ra, a.dec, b.ra, b.dec, 2, /verb)
  ;gplh, join.score
END
