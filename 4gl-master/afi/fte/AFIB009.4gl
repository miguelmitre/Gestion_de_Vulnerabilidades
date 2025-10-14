###########################################################################
#Proyecto          => Sistema de Afores.( MEXICO )                        #
#Propietario       => E.F.P.                                              #
#Programa AFIB009  => CONSULTA POR FOLIO DE LOS NSS DUPLICADOS            #
#Fecha             => 08 DE MARZO DEL2001.                                #
#Actualizado       => LAURA EUGENIA CORTES GUZMAN                         #
#Sistema           => AFI                                                 #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                          #
#Fecha mod         => 23 DE ENERO DEL 2003.                               #
#                  => SE ANEXO COMO CRITERIO DE BUSQUEDA DE INFORMACION   #
#                     EL NSS.                                             #
#Modificado por    => FERNANDO HERRERA HERNANDEZ                          #
#Fecha mod         => 28 DE MARZO DEL 2006.                               #
#                     CIRCULAR 7-12                                       #
###########################################################################
DATABASE safre_af
GLOBALS

    DEFINE #glo #date
           HOY                   DATE

    DEFINE #glo #char
           enter                 CHAR(001),
           x_busca               INTEGER
  
    DEFINE #glo #int
           vfolio                ,
           next_fld              ,
           total_pa              ,
           array_sz              ,
           x                     ,
           v_rowid               INTEGER
            
    DEFINE #glo #smallint
           last_key              ,
           arr_c                 ,
           arr_c_1               ,
           arr_c_3               ,
           linea                 ,
           over_size             ,
           cambio                ,
           tot_pag               SMALLINT

    DEFINE arr_0 ARRAY[10000] OF RECORD
           folio                 LIKE afi_det_dup_sol.folio      ,
           n_seguro              LIKE afi_det_dup_sol.nss_sol    ,
           curp                  LIKE afi_det_dup_sol.curp_sol   ,
           nombre                CHAR(030)                       ,
           diag_aclara           LIKE afi_det_dup_sol.diag_aclara,
           diag_desc             CHAR(010)                       ,
           tipo_deposito         CHAR(020)                       ,
           desc_tipo_depos       CHAR(030)                       ,
           fecha_envio           DATE
    END RECORD

    DEFINE arr_0_1 ARRAY[10000] OF RECORD
           folio                 LIKE afi_det_dup_sol.folio      ,
           n_seguro              LIKE afi_det_dup_sol.nss_sol    ,
           curp                  LIKE afi_det_dup_sol.curp_sol   ,
           paterno               LIKE afi_det_dup_sol.paterno    ,
           materno               LIKE afi_det_dup_sol.materno    ,
           nombres               LIKE afi_det_dup_sol.nombres    ,
           diag_aclara           LIKE afi_det_dup_sol.diag_aclara,
           fecha_envio           LIKE afi_det_dup_sol.fecha_envio
    END RECORD

    DEFINE
    cla_where			 CHAR(400),
    cla_sel			 CHAR(500)
   
    DEFINE
    rfolio                       INTEGER

END GLOBALS

MAIN
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ERROR  LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG("AFIB009.log")

    LET array_sz = 10000
    LET HOY      = TODAY
    LET int_flag = FALSE

    OPEN WINDOW afib009 AT 4,4 WITH FORM "AFIB0091" ATTRIBUTE(BORDER)

    DISPLAY " <Ctrl-B> Selec.Duplicados <Ctrl-F> Rech.Sol.      <Ctrl-C> Para Salir         " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " AFIB009                     R E G   SOLICITUD                                 " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY "                             R E G   DUPLICADOS                                 " AT 8,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

        CALL primer_paso()#pp

        PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR"
        FOR enter

    CLOSE WINDOW afib009

END MAIN

FUNCTION primer_paso() 
#pp-------------------

    DEFINE 
        text_1      CHAR(1000) ,
        diag_aclara CHAR(02)   ,
        rdiag_aclara CHAR(02)  ,
        diag_desc   CHAR(10)

    DEFINE 
        arr_c ,
        scr_l ,
        sw_1  ,
        i     SMALLINT,
        j     SMALLINT

    LET arr_c = 0
    LET scr_l = 0
    LET sw_1  = 0
    LET i  = 0
    LET j  = 0

    CONSTRUCT cla_where ON folio, nss_sol, curp_sol
                      FROM vfolio, vnss_sol, vcurp_sol

      ON KEY (ESC)
	 LET int_flag = FALSE
	 EXIT CONSTRUCT
    
      ON KEY (INTERRUPT)
	 LET int_flag = FALSE
	 RETURN

      ON KEY (CONTROL - C)
	 LET int_flag = FALSE
	 RETURN

    END CONSTRUCT

    IF int_flag THEN
       LET int_flag = FALSE
       ERROR "Proceso cancelado. . ."
       SLEEP 2
       ERROR ""
       CLEAR SCREEN
       CLOSE WINDOW afib009
       RETURN
    END IF

    INITIALIZE cla_sel TO NULL

    LET over_size = FALSE
    LET i         = 1
    LET cla_sel   = "SELECT folio, nss_sol, curp_sol, paterno, materno, ",
                    " nombres, diag_aclara, fecha_envio",
		    " FROM afi_det_dup_sol",
		    " WHERE  ", cla_where CLIPPED

    PREPARE claexe FROM cla_sel
    DECLARE cur_1 CURSOR FOR claexe
    FOREACH cur_1 INTO arr_0_1[i].folio       ,
                       arr_0_1[i].n_seguro    ,
                       arr_0_1[i].curp        , 
                       arr_0_1[i].paterno     ,
                       arr_0_1[i].materno     ,
                       arr_0_1[i].nombres     ,
                       arr_0_1[i].diag_aclara ,
                       arr_0_1[i].fecha_envio 

      LET arr_0[i].folio            = arr_0_1[i].folio
      LET arr_0[i].n_seguro         = arr_0_1[i].n_seguro
      LET arr_0[i].curp             = arr_0_1[i].curp
      LET arr_0[i].nombre           = arr_0_1[i].paterno     CLIPPED," ",
                                      arr_0_1[i].materno     CLIPPED," ",
                                      arr_0_1[i].nombres     CLIPPED 
      LET arr_0[i].diag_aclara      = arr_0_1[i].diag_aclara
      LET arr_0[i].fecha_envio      = arr_0_1[i].fecha_envio
       
      IF arr_0[i].diag_aclara = '01' THEN
         LET arr_0[i].diag_desc = 'ACEPTADO'
      END IF
      IF arr_0[i].diag_aclara = '02' THEN
         LET arr_0[i].diag_desc = 'RECHAZADO'
      END IF

      SELECT ind_deposito
        INTO arr_0[i].tipo_deposito
        FROM cta_ctr_reg_ind
       WHERE curp = arr_0[i].curp
      IF STATUS <> NOTFOUND THEN
         SELECT desc_tipo_depos
           INTO arr_0[i].desc_tipo_depos
           FROM tab_tipo_deposito
          WHERE tipo_deposito = arr_0[i].tipo_deposito
      END IF

      LET rfolio = arr_0[i].folio

      LET i = i + 1
      IF  array_sz <= i THEN
	   LET over_size = TRUE
	   EXIT FOREACH
      END IF

    END FOREACH
    IF i = 1 THEN 
       PROMPT " NO SE ENCONTRARON REGISTROS... < ENTER > PARA SALIR"
       FOR CHAR enter
       RETURN
    END IF 
 
    IF over_size THEN
       PROMPT "ARREGLO LLENO, PARAMETRIZAR SU BUSQUEDA [Enter] PARA CONTINUAR"
       FOR enter
       RETURN
    END IF

    CALL SET_COUNT(i-1)

    DISPLAY "" AT 19,1
    DISPLAY ARRAY arr_0 TO scr_0.*

    ON KEY(CONTROL-B)
    DISPLAY " <Ctrl-B> Selec.Duplicados                         <Ctrl-C> Para Salir         " AT 1,1 ATTRIBUTE(REVERSE)
       LET arr_c = ARR_CURR()
       LET linea = SCR_LINE()
       CALL pantalla_2() RETURNING j
       CASE j
          WHEN 1
            LET diag_aclara = "01"
            LET diag_desc   = "ACEPTADO"
            DISPLAY diag_aclara TO vdiag_aclara
            DISPLAY diag_desc   TO vdiag_desc
            LET arr_0[arr_c].diag_aclara = diag_aclara
            LET arr_0[arr_c].diag_desc   = diag_desc
          WHEN 2
            LET diag_aclara = "02"
            LET diag_desc   = "RECHAZADO"
            DISPLAY diag_aclara TO vdiag_aclara
            DISPLAY diag_desc   TO vdiag_desc
            LET arr_0[arr_c].diag_aclara = diag_aclara
            LET arr_0[arr_c].diag_desc   = diag_desc
          WHEN 3
            LET diag_aclara = ""
            DISPLAY diag_aclara TO vdiag_aclara
            LET arr_0[arr_c].diag_aclara = diag_aclara
            LET arr_0[arr_c].diag_desc   = diag_desc
          WHEN 4
            EXIT DISPLAY    
       END CASE

    ON KEY(CONTROL-F)
       DISPLAY "                                                   <Ctrl-C> Para Salir         " AT 1,1 ATTRIBUTE(REVERSE)
       LET diag_aclara = "02"
       LET diag_desc   = "RECHAZADO"

       UPDATE afi_det_dup_aso
       SET    diag_aclara  = "04",
              marca        = 1
       WHERE  @folio       = rfolio 
       AND    @diag_aclara <> '01'
       AND    @marca       IS NULL

       UPDATE afi_det_dup_sol
       SET    diag_aclara  = diag_aclara,
              marca        = 1
       WHERE  @folio       = rfolio
       AND    @diag_aclara <> '01'
       AND    @marca       IS NULL

       DISPLAY diag_aclara TO vdiag_aclara
       DISPLAY diag_desc   TO vdiag_desc
           
       CALL despliega()

    ON KEY(CONTROL-C)
       EXIT DISPLAY    
           
    ON KEY(INTERRUPT)
       EXIT DISPLAY    
           
    END DISPLAY

END FUNCTION

FUNCTION pantalla_2()
#p2------------------

     DEFINE x         ,
            x_1       ,
            scr_l     INTEGER,

            regresa   ,
            marmarca  ,
            indice    ,
            curr_pa   ,
            curr_sa   ,
            mar_03    ,
            mar_04    ,
            mar_05    SMALLINT,

            arr_2  ARRAY[50] OF RECORD 
            n_seguro   LIKE afi_det_dup_aso.nss_dup    ,
            curp       LIKE afi_det_dup_aso.curp_dup   ,
	    cve_ent    LIKE afi_det_dup_aso.cve_ent_dup,
            desc_ent   CHAR(30)                        ,
            marca      CHAR(02)                        ,
            desc_marca CHAR(10)
            END RECORD ,

            pr_workmov RECORD 
            n_seguro   LIKE afi_det_dup_aso.nss_dup    ,
            curp       LIKE afi_det_dup_aso.curp_dup   ,
	    cve_ent    LIKE afi_det_dup_aso.cve_ent_dup,
            desc_ent   CHAR(30)                        ,
            marca      CHAR(02)                        ,
            desc_marca CHAR(10)
            END RECORD ,

            vcurp_dup  CHAR(16)
 
     LET x      = 1
     LET x_1    = 0
     LET mar_03 = 0
     LET mar_04 = 0
     LET mar_05 = 0
     LET arr_c = ARR_CURR()

     LET vcurp_dup = arr_0[arr_c].curp

     DECLARE cur_2 CURSOR FOR 

           SELECT nss_dup, curp_dup, cve_ent_dup
           FROM   afi_det_dup_aso
           WHERE  folio    = arr_0[arr_c].folio
           AND    nss_sol  = arr_0[arr_c].n_seguro
           AND    curp_dup[1,16] = vcurp_dup

     FOREACH cur_2 INTO arr_2[x].*

         SELECT afore_desc
           INTO arr_2[x].desc_ent
           FROM tab_afore
          WHERE afore_cod = arr_2[x].cve_ent

         LET x = x + 1
     END FOREACH

     IF x > 1 THEN
        LET x_1 = x - 1
     ELSE
        IF x = 1 THEN
           LET x_1 = 1
        END IF
     END IF

     CALL SET_COUNT(x-1)
     --LET total_pa = ARR_COUNT()

     INPUT ARRAY arr_2 WITHOUT DEFAULTS FROM scr_1.*

         BEFORE FIELD marca
              LET curr_pa = ARR_CURR()
              LET curr_sa = SCR_LINE()
              LET total_pa = ARR_COUNT()
              LET pr_workmov.* = arr_2[curr_pa].*

            AFTER FIELD marca
                 IF arr_2[curr_pa].marca <> "03"    AND 
                    arr_2[curr_pa].marca <> "04"  THEN
                      ERROR "    MARCA DESCONOCIDA " ATTRIBUTE(NORMAL)
                      LET arr_2[curr_pa].marca = pr_workmov.marca
                      DISPLAY    arr_2[curr_pa].marca
                      TO         scr_1[curr_sa].marca
                      NEXT FIELD marca
                 END IF 
              
                 IF arr_2[curr_pa].marca = "03"    THEN
                    LET mar_03                    = mar_03 + 1
                    LET arr_2[curr_pa].desc_marca = "ACEPTADO"
                    DISPLAY    arr_2[curr_pa].desc_marca
                    TO         scr_1[curr_sa].desc_marca
                 ELSE
                    IF arr_2[curr_pa].marca = "04"    THEN
                       LET mar_04 = mar_04 + 1
                       LET arr_2[curr_pa].desc_marca = "RECHAZADO"
                       DISPLAY    arr_2[curr_pa].desc_marca
                       TO         scr_1[curr_sa].desc_marca
                    ELSE
                       IF arr_2[curr_pa].marca = "  "    OR
                          arr_2[curr_pa].marca IS NULL   THEN
                          LET mar_05 = mar_05 + 1
                       END IF 
                    END IF 
                 END IF 

                 LET last_key = FGL_LASTKEY()                       
                 LET next_fld = (last_key = FGL_KEYVAL("right"))
                                OR (last_key = FGL_KEYVAL("return"))
                                OR (last_key = FGL_KEYVAL("tab"))
                                OR (last_key = FGL_KEYVAL("down"))
                                                             
                 IF (curr_pa >= total_pa) THEN 

                    IF next_fld THEN
                       ERROR "    ULTIMO REGISTRO" ATTRIBUTE(NORMAL)
                       NEXT FIELD marca
                    END IF 
                 END IF

            ON KEY (ESC)
                IF mar_03 = 0  AND
                   mar_04 = 0  AND 
                   mar_05 > 0  THEN
                   FOR indice = 1 TO total_pa
                       LET vcurp_dup = arr_2[indice].curp
                       UPDATE afi_det_dup_aso
                       SET    diag_aclara = NULL,
                              marca       = NULL
                       WHERE  folio       = arr_0[arr_c].folio
                       AND    nss_sol     = arr_0[arr_c].n_seguro
                       AND    nss_dup     = arr_2[indice].n_seguro
                       AND    curp_dup[1,16]    = vcurp_dup 
                   END FOR
                   UPDATE afi_det_dup_sol
                   SET    diag_aclara = NULL,
                          marca       = NULL
                   WHERE  folio       = arr_0[arr_c].folio
                   AND    nss_sol     = arr_0[arr_c].n_seguro
                   AND    curp_sol    = arr_0[arr_c].curp
                   LET regresa = 3
                   EXIT INPUT
                END IF

                IF mar_03 = 0  AND
                   mar_04 = 0  AND 
                   mar_05 = 0  THEN
                   FOR indice = 1 TO total_pa
                       LET vcurp_dup = arr_2[indice].curp
                       UPDATE afi_det_dup_aso
                       SET    diag_aclara = NULL,
                              marca       = NULL
                       WHERE  folio       = arr_0[arr_c].folio
                       AND    nss_sol     = arr_0[arr_c].n_seguro
                       AND    nss_dup     = arr_2[indice].n_seguro
                       AND    curp_dup[1,16] = vcurp_dup
                   END FOR
                   UPDATE afi_det_dup_sol
                   SET    diag_aclara = NULL,
                          marca       = NULL
                   WHERE  folio       = arr_0[arr_c].folio
                   AND    nss_sol     = arr_0[arr_c].n_seguro
                   AND    curp_sol    = arr_0[arr_c].curp
                   LET regresa = 3
                   EXIT INPUT
                END IF

                IF mar_03 > 0  AND
                   mar_04 > 0  AND 
                   mar_05 > 0  THEN
                   FOR indice = 1 TO total_pa
                       LET vcurp_dup = arr_0[arr_c].curp
                       UPDATE afi_det_dup_aso
                       SET    diag_aclara = arr_2[indice].marca,
                              marca       = 1
                       WHERE  folio       = arr_0[arr_c].folio
                       AND    nss_sol     = arr_0[arr_c].n_seguro
                       AND    curp_dup[1,16] = vcurp_dup
                   END FOR
                   LET regresa = 3
                   EXIT INPUT
                END IF

                IF mar_03 > 0    AND
                   mar_03 = x_1  AND 
                   mar_04 = 0    AND 
                   mar_05 = 0    THEN
                   FOR indice = 1 TO total_pa
                       LET vcurp_dup = arr_2[indice].curp
                       UPDATE afi_det_dup_aso
                       SET    diag_aclara = arr_2[indice].marca,
                              marca       = 1
                       WHERE  folio       = arr_0[arr_c].folio
                       AND    nss_sol     = arr_0[arr_c].n_seguro
                       AND    nss_dup     = arr_2[indice].n_seguro
                       AND    curp_dup[1,16] =  vcurp_dup
                   END FOR
                   UPDATE afi_det_dup_sol
                   SET    diag_aclara = "01",
                          marca       = 1
                   WHERE  folio       = arr_0[arr_c].folio
                   AND    nss_sol     = arr_0[arr_c].n_seguro
                   AND    curp_sol    = arr_0[arr_c].curp
                   LET regresa = 1
                   EXIT INPUT
                END IF

                IF mar_03 = 0    AND
                   mar_04 > 0    AND 
                   mar_04 = x_1  AND 
                   mar_05 = 0    THEN
                   FOR indice = 1 TO total_pa
                       LET vcurp_dup = arr_2[indice].curp
                       UPDATE afi_det_dup_aso
                       SET    diag_aclara = arr_2[indice].marca,
                              marca       = 1
                       WHERE  folio       = arr_0[arr_c].folio
                       AND    nss_sol     = arr_0[arr_c].n_seguro
                       AND    nss_dup     = arr_2[indice].n_seguro
                       AND    curp_dup[1,16] =  vcurp_dup
                   END FOR
                   UPDATE afi_det_dup_sol
                   SET    diag_aclara = "02",
                          marca       = 1
                   WHERE  folio       = arr_0[arr_c].folio
                   AND    nss_sol     = arr_0[arr_c].n_seguro
                   AND    curp_sol    = arr_0[arr_c].curp
                   LET regresa = 2
                   EXIT INPUT
                END IF

                IF mar_03 > 0    AND
                   mar_04 > 0    AND 
                   (mar_03 + mar_04 ) = x_1 AND
                   mar_05 = 0    THEN
                   FOR indice = 1 TO total_pa
                       LET vcurp_dup = arr_2[indice].curp
                       UPDATE afi_det_dup_aso
                       SET    diag_aclara = arr_2[indice].marca,
                              marca       = 1
                       WHERE  folio       = arr_0[arr_c].folio
                       AND    nss_sol     = arr_0[arr_c].n_seguro
                       AND    nss_dup     = arr_2[indice].n_seguro
                       AND    curp_dup[1,16] = vcurp_dup
                   END FOR
                   UPDATE afi_det_dup_sol
                   SET    diag_aclara = "02",
                          marca       = 1
                   WHERE  folio       = arr_0[arr_c].folio
                   AND    nss_sol     = arr_0[arr_c].n_seguro
                   AND    curp_sol    = arr_0[arr_c].curp
                   LET regresa = 2
                   EXIT INPUT
                END IF

                IF mar_03 > 0    AND
                   mar_04 = 0    AND 
                   mar_05 > 0    THEN
                   FOR indice = 1 TO total_pa
                       LET vcurp_dup = arr_2[indice].curp
                       UPDATE afi_det_dup_aso
                       SET    diag_aclara = arr_2[indice].marca,
                              marca       = 1
                       WHERE  folio       = arr_0[arr_c].folio
                       AND    nss_sol     = arr_0[arr_c].n_seguro
                       AND    nss_dup     = arr_2[indice].n_seguro
                       AND    curp_dup[1,16] = vcurp_dup
                   END FOR
                   UPDATE afi_det_dup_sol
                   SET    diag_aclara = NULL,
                          marca       = 1
                   WHERE  folio       = arr_0[arr_c].folio
                   AND    nss_sol     = arr_0[arr_c].n_seguro
                   AND    curp_sol    = arr_0[arr_c].curp
                   LET regresa = 3
                   EXIT INPUT
                END IF

                IF mar_03 = 0    AND
                   mar_04 > 0    AND 
                   mar_05 > 0    THEN
                   FOR indice = 1 TO total_pa
                       LET vcurp_dup = arr_2[indice].curp
                       UPDATE afi_det_dup_aso
                       SET    diag_aclara = arr_2[indice].marca,
                              marca       = 1
                       WHERE  folio       = arr_0[arr_c].folio
                       AND    nss_sol     = arr_0[arr_c].n_seguro
                       AND    nss_dup     = arr_2[indice].n_seguro
                       AND    curp_dup[1,16] = vcurp_dup
                   END FOR
                   UPDATE afi_det_dup_sol
                   SET    diag_aclara = NULL,
                          marca       = 1
                   WHERE  folio       = arr_0[arr_c].folio
                   AND    nss_sol     = arr_0[arr_c].n_seguro
                   AND    curp_sol    = arr_0[arr_c].curp
                   LET regresa = 3
                   EXIT INPUT
                END IF

            ON KEY (CONTROL-C)
                LET regresa = 4
                EXIT INPUT

            ON KEY (INTERRUPT)
                LET regresa = 4
                EXIT INPUT

        END INPUT
        FOR x = 1 TO total_pa
           CLEAR scr_1[x].*
        END FOR
        DISPLAY " <Ctrl-B> Selec.Duplicados <Ctrl-F> Rech.Sol.      <Ctrl-C> Para Salir         " AT 1,1 ATTRIBUTE(REVERSE)
        RETURN regresa
END FUNCTION

FUNCTION despliega()

 DEFINE 
   sfolio     INTEGER,
   snss       CHAR(11),
   scurp      CHAR(18),
   arr_c      ,
   scr_l      ,
   sw_1       ,
   i          SMALLINT,
   j          SMALLINT

 DEFINE 
   x          ,
   x_1        ,
   arr_s      INTEGER

  LET arr_s = 0
  LET scr_l = 0
  LET sw_1  = 0
  LET i     = 0
  LET j     = 0

  LET x      = 1

  FOR x = 1 TO 1
      CLEAR scr_0[x].*
      INITIALIZE arr_0_1[x].* TO NULL
  END FOR

  LET i         = 1

  DISPLAY " <Ctrl-B> Despl.Duplicados                         <Ctrl-C> Para Salir         " AT 1,1 ATTRIBUTE(REVERSE)

  DECLARE cur_3 CURSOR FOR
  SELECT folio, nss_sol, curp_sol, paterno, materno, 
         nombres, diag_aclara, fecha_envio
  FROM afi_det_dup_sol
  WHERE folio = rfolio 
  FOREACH cur_3 INTO arr_0_1[i].folio       ,
                     arr_0_1[i].n_seguro    ,
                     arr_0_1[i].curp        , 
                     arr_0_1[i].paterno     ,
                     arr_0_1[i].materno     ,
                     arr_0_1[i].nombres     ,
                     arr_0_1[i].diag_aclara ,
                     arr_0_1[i].fecha_envio 

    LET arr_0[i].folio            = arr_0_1[i].folio
    LET arr_0[i].n_seguro         = arr_0_1[i].n_seguro
    LET arr_0[i].curp             = arr_0_1[i].curp
    LET arr_0[i].nombre           = arr_0_1[i].paterno     CLIPPED," ",
                                    arr_0_1[i].materno     CLIPPED," ",
                                    arr_0_1[i].nombres     CLIPPED 
    LET arr_0[i].diag_aclara      = arr_0_1[i].diag_aclara
    LET arr_0[i].fecha_envio      = arr_0_1[i].fecha_envio
       
    IF arr_0[i].diag_aclara = '01' THEN
       LET arr_0[i].diag_desc = 'ACEPTADO'
    END IF
    IF arr_0[i].diag_aclara = '02' THEN
       LET arr_0[i].diag_desc = 'RECHAZADO'
    END IF

    SELECT ind_deposito
      INTO arr_0[i].tipo_deposito
      FROM cta_ctr_reg_ind
     WHERE curp = arr_0[i].curp
    IF STATUS <> NOTFOUND THEN
       SELECT desc_tipo_depos
         INTO arr_0[i].desc_tipo_depos
         FROM tab_tipo_deposito
        WHERE tipo_deposito = arr_0[i].tipo_deposito
    END IF

    LET rfolio = arr_0[i].folio

    LET i = i + 1
    IF  array_sz <= i THEN
	LET over_size = TRUE
	EXIT FOREACH
    END IF

  END FOREACH

  IF i = 1 THEN 
     PROMPT " NO SE ENCONTRARON REGISTROS... < ENTER > PARA SALIR"
     FOR CHAR enter
     RETURN
  END IF 
 
  IF over_size THEN
     PROMPT "ARREGLO LLENO, PARAMETRIZAR SU BUSQUEDA [Enter] PARA CONTINUAR"
     FOR enter
     RETURN
  END IF

  CALL SET_COUNT(i-1)

  DISPLAY "" AT 19,1
  DISPLAY ARRAY arr_0 TO scr_0.*

    ON KEY(CONTROL-B)
       LET arr_s = ARR_CURR()
       LET sfolio = arr_0[arr_s].folio
       LET snss   = arr_0[arr_s].n_seguro
       LET scurp  = arr_0[arr_s].curp

       CALL despliega_asociados(sfolio, snss, scurp)
       DISPLAY " <Ctrl-B> Despl.Duplicados                         <Ctrl-C> Para Salir         " AT 1,1 ATTRIBUTE(REVERSE)

    ON KEY(CONTROL-C)
       EXIT DISPLAY    
           
    ON KEY(INTERRUPT)
       EXIT DISPLAY    
           
  END DISPLAY


END FUNCTION

FUNCTION despliega_asociados(dfolio, dnss, dcurp)

  DEFINE 
    dfolio     INTEGER,
    dnss       CHAR(11),
    dcurp      CHAR(18),
    dcurp1     CHAR(16),
    z          ,
    x_1        INTEGER,

    arr_2  ARRAY[50] OF RECORD 
    nss_dup    LIKE afi_det_dup_aso.nss_dup    ,
    curp_dup   LIKE afi_det_dup_aso.curp_dup   ,
    cve_ent    LIKE afi_det_dup_aso.cve_ent_dup,
    desc_ent   CHAR(30)                        ,
    marca      CHAR(02)                        ,
    desc_marca CHAR(10)
    END RECORD

    DISPLAY "                           <Ctrl-T> Regresar       <Ctrl-C> Para Salir         " AT 1,1 ATTRIBUTE(REVERSE)

    FOR x_1 = 1 TO 1
        CLEAR scr_1[x_1].*
        INITIALIZE arr_2[x_1].* TO NULL
    END FOR

    LET z = 1

    LET dcurp1 = dcurp

    DECLARE cur_4 CURSOR FOR 
    SELECT nss_dup, curp_dup, cve_ent_dup, 0, diag_aclara, 0 
    FROM   afi_det_dup_aso
    WHERE  folio    = dfolio
    AND    nss_sol  = dnss
    AND    curp_dup = dcurp1

    FOREACH cur_4 INTO arr_2[z].*

      SELECT afore_desc
        INTO arr_2[z].desc_ent
        FROM tab_afore
       WHERE afore_cod = arr_2[z].cve_ent

      IF arr_2[z].marca = '03' THEN 
         LET arr_2[z].desc_marca = 'ACEPTADO'
      ELSE
         LET arr_2[z].desc_marca = 'RECHAZADO'
      END IF

      LET z = z + 1
    END FOREACH

    DISPLAY ARRAY arr_2 TO scr_1.*

      ON KEY(CONTROL-C)
         EXIT DISPLAY    
           
      ON KEY(INTERRUPT)
         EXIT DISPLAY    

      ON KEY(CONTROL-T)
         FOR x_1 = 1 TO 1
             CLEAR scr_1[x_1].*
         END FOR
         RETURN
           
    END DISPLAY

END FUNCTION
