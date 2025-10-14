################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETM820  => CARGA Y CONSULTA DEL ARCHIVO DE RESOLUCIONES TRANSITORIAS#
#                     Y COMPLEMENTARIAS                                        #
#Fecha creacion    => 1 DE JUNIO DE 2009                                       #
#By                => JAVIER GONZALEZ JERONIMO                                 #
#Fecha actualiz.   => 12 DE JUNIO DE 2009                                      #
#Actualizacion     => SE AGREGO LA OPCION PARA CARGAR EL ARCHIVO DE            #
#                  => RESOLUCIONES COMPLEMENTARIAS                             #
#Fecha actualiz.   => 17 DE JULIO DE 2009                                      #
#Actualizacion     => SE AGREGO LA OPCION PARA CARGAR EL ARCHIVO DE            #
#                  => RESOLUCIONES COMPLEMENTARIAS CON NUEVO FORMATO ENVIADO   #
#                  => POR PROCESAR PARA LAS AFORES                             #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
        nom_archivo          CHAR(20)
    END RECORD

    DEFINE #glo #record
        gs_modulo             RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        carga_reg             CHAR(360) ,
        enter                 CHAR(001) ,
        usuario               CHAR(008)

    DEFINE #glo INTEGER
       ultimo_folio          ,
       cuantos               INTEGER

    DEFINE #glo #smallint
        gs_recibido          SMALLINT

END GLOBALS

MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL init()

    OPEN WINDOW retm820 AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY "                          < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETM820  RESOL TRANSITORIAS Y COMPLEMENTARIAS RET PARCIALES                   " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    -- Menu Principal
    MENU "CARGA RESOLUCIONES"
        COMMAND "TRANSITORIAS" "Carga Archivo de Resoluciones Transitorias"
            
            -- Submenu Transitorias
            MENU "TRANSITORIAS"
                COMMAND "Carga archivo" "Carga Archivo de Resoluciones Transitorias"
                    IF carga_archivo(1) THEN
                        LET ultimo_folio = f_ultimo_folio()

                        DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)
                        CALL f_carga_transitorias ()  #pp

                        DISPLAY " DETERMINANDO TIPO DE PAGO ... " AT 18,1 ATTRIBUTE(REVERSE)
                        CALL f_iden_tipo_pago(ultimo_folio)
                    END IF

                    CLOSE WINDOW retm8201
                    CLEAR SCREEN

                COMMAND "Consulta" "Consulta de Resoluciones Transitorias"
                    CALL f_consulta_trans()
                    CLEAR SCREEN

                COMMAND "Regresar" "Regresa al Menu Principal"
                    EXIT MENU
            END MENU
        #-- -----------------------------------------------------------------------------

        COMMAND "COMPLEMENTARIAS" "Carga Archivo de Resoluciones Complementarias"
            
            -- Submenu Complementarias
            MENU "COMPLEMENTARIAS"
                COMMAND "Carga Lote Comp" "Carga Lote de Resoluciones Complementarias"
                    IF carga_archivo(2) THEN
                        LET ultimo_folio = f_ultimo_folio()

                        DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)
                        CALL f_carga_complementarias()

                    END IF

                    CLOSE WINDOW retm8201
                    CLEAR SCREEN

                COMMAND "Carga Archivo Proc" "Carga Archivo de Resoluciones enviado por Procesar"
                    IF carga_archivo(3) THEN
                        LET ultimo_folio = f_ultimo_folio()

                        DISPLAY " CARGANDO ARCHIVO ... " AT 18,1 ATTRIBUTE(REVERSE)
                        CALL f_carga_comp_procesar()

                    END IF

                    CLOSE WINDOW retm8201
                    CLEAR SCREEN


                COMMAND "Consulta" "Consulta de Resoluciones Complementarias"
                    CALL f_consulta_comp()
                    CLEAR SCREEN

                COMMAND "Regresar" "Regresa al Menu Principal"
                    EXIT MENU

            END MENU
        #-- -----------------------------------------------------------------------------
        
        COMMAND "Salir" "Salir del Programa "
            EXIT MENU

    END MENU

    CLOSE WINDOW retm820

END MAIN


FUNCTION init()
#i------------
    LET HOY = TODAY

    SELECT USER,
           *
    INTO   usuario,
           gs_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT A.estado_solicitud
    INTO   gs_recibido
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

END FUNCTION


#-----------------------------------------------------------------------#
# Realiza la carga del archivo en la tabla temporal tmp_resol_esp       #
#-----------------------------------------------------------------------#
FUNCTION carga_archivo(ps_id_carga)

    DEFINE
        ps_id_carga         ,
        ls_procesa          SMALLINT

    DEFINE
        ruta_arch_trans     CHAR(200)

    OPEN WINDOW retm8201 AT 4,4 WITH FORM "RETM8201"  -- ATTRIBUTE(BORDER)
    DISPLAY "                          < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)

    CASE ps_id_carga 
        
        WHEN 1    
            DISPLAY " RETM820   CARGA ARCHIVO RESOL. TRANSITORIAS DE RET PARCIALES                  " AT 3,1 ATTRIBUTE(REVERSE)
        WHEN 2
            DISPLAY " RETM820  CARGA ARCHIVO RESOL. COMPLEMENTARIAS DE RET PARCIALES                " AT 3,1 ATTRIBUTE(REVERSE)            
        WHEN 3
            DISPLAY " RETM820        CARGA ARCHIVO COMPLEMENTARIAS DE RET PARCIALES                " AT 3,1 ATTRIBUTE(REVERSE)        
    END CASE
    
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    CALL f_tabla_tmp()
    LET cuantos     = 0
    LET ls_procesa  = 0

    INPUT BY NAME reg_1.nom_archivo WITHOUT DEFAULTS
        BEFORE FIELD nom_archivo
            LET reg_1.nom_archivo = NULL
            CLEAR FORM

        AFTER FIELD nom_archivo
            IF reg_1.nom_archivo IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO  "
                NEXT FIELD nom_archivo
            END IF

            -- Valida que no se haya cargado previamente el archivo
            SELECT "OK"
            FROM   ret_cza_resol
            WHERE  nom_archivo = reg_1.nom_archivo
            GROUP BY 1

            IF STATUS <> NOTFOUND THEN
                ERROR " ARCHIVO YA PROCESADO CON ANTERIORIDAD  "
                NEXT FIELD nom_archivo
            END IF

            -- Valida que el archivo exista en la ruta de rescate o que contenga informacion
            LET ruta_arch_trans = gs_modulo.ruta_rescate CLIPPED,"/",
                                  reg_1.nom_archivo CLIPPED

            
            WHENEVER ERROR CONTINUE

            LOAD FROM ruta_arch_trans DELIMITER "+"
            INSERT INTO tmp_resol_esp

            WHENEVER ERROR STOP

            SELECT COUNT(*)
            INTO   cuantos
            FROM   tmp_resol_esp

            IF cuantos = 0 THEN
                ERROR "  NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO  "
                NEXT FIELD nom_archivo
            END IF

            LET ls_procesa = f_valida_arch_comp(ps_id_carga) 
            
            IF ls_procesa = 0 THEN
                PROMPT " ARCHIVO CON FORMATO INVALIDO... <ENTER> PARA SALIR " FOR CHAR enter
                EXIT INPUT
            END IF

            -- Validacion de carga del usuario
            WHILE TRUE
                PROMPT " ESTA SEGURO S/N ? " FOR CHAR enter
                IF enter MATCHES "[sSnN]" THEN
                    IF enter MATCHES "[sS]" THEN
                        LET ls_procesa  = 1
                        EXIT INPUT
                    ELSE
                        PROMPT " CARGA CANCELADA...<ENTER> PARA SALIR " FOR CHAR enter
                        EXIT WHILE
                    END IF
                END IF
            END WHILE

        ON KEY (INTERRUPT)
            ERROR " PROCESO CANCELADO  "
            SLEEP 1
            ERROR ""
            EXIT INPUT
    END INPUT

    RETURN ls_procesa

END FUNCTION

#-----------------------------------------------------------------------#
# Realiza la carga del archivo de resoluciones transitorias en la       #
# tabla ret_resol_alterno                                               #
#-----------------------------------------------------------------------#
FUNCTION f_carga_transitorias()

    DEFINE lr_cza_resol RECORD LIKE ret_cza_resol.*

    DEFINE lr_resol_alt RECORD LIKE ret_resol_alterno.*

    DEFINE #loc #char
        lc_fecha_recep          CHAR(010),
        lc_salario              CHAR(010)

    LET lr_cza_resol.total_registros = 0
    LET lr_resol_alt.folio           = ultimo_folio

    DECLARE cur_2 CURSOR FOR
    SELECT *
    FROM   tmp_resol_esp

    FOREACH cur_2 INTO carga_reg

        LET lr_resol_alt.nss                = carga_reg[001,011]
        LET lr_resol_alt.num_resolucion     = carga_reg[012,017]

        LET lc_salario                      = carga_reg[018,024],".",
                                              carga_reg[025,026]
        LET lr_resol_alt.salario_base_cot   = lc_salario

        LET lc_fecha_recep                  = carga_reg[031,032],"/",
                                              carga_reg[033,034],"/",
                                              carga_reg[027,030]

        LET lr_resol_alt.fecha_recepcion    = lc_fecha_recep

        INSERT INTO ret_resol_alterno VALUES (lr_resol_alt.*)

        LET lr_cza_resol.total_registros = lr_cza_resol.total_registros + 1
        DISPLAY "TOTAL DE REGISTROS CARGADOS                : ",lr_cza_resol.total_registros AT 11,9

    END FOREACH

    INSERT INTO ret_cza_resol
    VALUES(ultimo_folio                 , #folio
           HOY                          , #fecha_operacion
           lr_cza_resol.total_registros , #total_registros
           0                            , #total_importe
           reg_1.nom_archivo            , #nom_archivo
           HOY                          , #fecha_carga
           usuario                      , #usuario
           gs_recibido                    #estado_lote
          )

    DISPLAY "TOTAL DE REGISTROS PROCESADOS              : ",cuantos AT 10,9
    DISPLAY "TOTAL DE REGISTROS CARGADOS                : ",lr_cza_resol.total_registros AT 11,9

END FUNCTION

#-----------------------------------------------------------------------#
# Identifica el tipo de pago a generarse de acuerdo a la posicion       #
# 3 y 4 del nss y actualiza el valor en la tabla de resoluciones        #
# transitorias                                                          #
#-----------------------------------------------------------------------#
FUNCTION f_iden_tipo_pago(p_folio)

    DEFINE
        p_folio         INTEGER

    DEFINE lr_alterno RECORD LIKE ret_resol_alterno.*

    DEFINE
        ls_anio         SMALLINT

    DEFINE
        li_cont         INTEGER

    LET li_cont = 0

    DECLARE cur_idtipo CURSOR FOR
    SELECT *
    FROM   ret_resol_alterno
    WHERE  folio = p_folio

    FOREACH cur_idtipo INTO lr_alterno.*

        LET ls_anio = f_obten_anio(lr_alterno.nss[3,4])

        IF ls_anio <= 2004 THEN
            LET lr_alterno.ind_tipo_pago = 2
        ELSE
            LET lr_alterno.ind_tipo_pago = 3
        END IF

        UPDATE ret_resol_alterno
        SET    ind_tipo_pago    = lr_alterno.ind_tipo_pago
        WHERE  nss              = lr_alterno.nss
        AND    num_resolucion   = lr_alterno.num_resolucion

        LET li_cont = li_cont + 1
        DISPLAY "REGISTROS PROCESADOS TIPO DE PAGO          : ", li_cont AT 12,9
    END FOREACH

    DISPLAY "                                               " AT 18,1
    DISPLAY "                      FOLIO DE CARGA       : ", ultimo_folio AT 14,9
    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
    FOR CHAR enter

END FUNCTION


#-----------------------------------------------------------------------#
# Realiza la consulta sobre la tabla de resoluciones transitorias       #
#-----------------------------------------------------------------------#
FUNCTION f_consulta_trans()

    DEFINE #loc #char
        cla_where            ,
        sel_where            CHAR(800)

    DEFINE #loc #smallint
        pos                  SMALLINT

    DEFINE l_record ARRAY[4000] OF RECORD
         nss                  CHAR(11) ,
         folio                INTEGER  ,
         num_resolucion       INTEGER  ,
         salario_base_cot     LIKE ret_resol_alterno.salario_base_cot,
         ind_tipo_pago        LIKE ret_resol_alterno.ind_tipo_pago,
         desc_tipo_pago       CHAR(30) ,
         fecha_recepcion      DATE     ,
         nom_archivo          CHAR(20) ,
         fecha_carga          DATE
    END RECORD

    LET pos = 2

    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)

        OPEN WINDOW retm8202 AT 4,4 WITH FORM "RETM8202"
        DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
        DISPLAY " RETM820      CONSULTA DE RES TRANSITORIAS DE RET PARCIALES                    " AT 3,1 ATTRIBUTE(REVERSE)
        DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

        LET int_flag = FALSE

        CONSTRUCT cla_where ON A.nss             ,
                               A.folio           ,
                               A.num_resolucion  ,
                               A.fecha_recepcion ,
                               B.nom_archivo     ,
                               B.fecha_carga
                          FROM nss               ,
                               folio             ,
                               num_resolucion    ,
                               fecha_recepcion   ,
                               nom_archivo       ,
                               fecha_carga

            ON KEY ( ESC )
                LET int_flag = FALSE
                EXIT CONSTRUCT

            ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        END CONSTRUCT

        IF INT_FLAG = TRUE THEN
            LET INT_FLAG = FALSE
            ERROR "  BUSQUEDA CANCELADA...  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW retm8202
            RETURN
        END IF

        LET sel_where = "SELECT  A.nss                ,",
                               " A.folio              ,",
                               " A.num_resolucion     ,",
                               " A.salario_base_cot   ,",
                               " A.ind_tipo_pago      ,",
                               " ' '                  ,",
                               " A.fecha_recepcion    ,",
                               " B.nom_archivo        ,",
                               " B.fecha_carga         ",
                               " FROM ret_resol_alterno A, ret_cza_resol B ",
                               " WHERE ",cla_where CLIPPED ,
                               " AND   A.folio = B.folio "
        PREPARE query_1 FROM sel_where
        DECLARE cur_3 CURSOR FOR query_1

        LET pos = 1

        FOREACH cur_3 INTO l_record[pos].*

            IF l_record[pos].ind_tipo_pago = "02" THEN
                LET l_record[pos].desc_tipo_pago = "PAGO RETIRO A o B"
            ELSE
                LET l_record[pos].desc_tipo_pago = "75 DIAS SBC o 10% RCV"
            END IF

            LET pos = pos + 1

            IF pos >= 4000 THEN
                ERROR "SE SOBREPASO LA CAPACIDAD DEL ARREGLO ..."
                SLEEP 2
                EXIT FOREACH
            END IF

        END FOREACH

        INITIALIZE l_record[pos].* TO NULL

        IF (pos-1) >= 1 THEN
            CALL SET_COUNT(pos-1)

            DISPLAY ARRAY l_record TO scr_1.*

                ON KEY (INTERRUPT)
                    EXIT DISPLAY

            END DISPLAY

            CLOSE WINDOW retm8202

        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLOSE WINDOW retm8202
        END IF
    END IF
END FUNCTION

-- ---------------------------------------------------------------------------

#-----------------------------------------------------------------------#
# Realiza la carga del archivo de resoluciones complementarias en la    #
# tabla ret_resol_comp                                                  #
#-----------------------------------------------------------------------#
FUNCTION f_carga_complementarias()
#pp------------------

    DEFINE lr_cza_resol RECORD LIKE ret_cza_resol.*

    DEFINE lr_resol_comp RECORD LIKE ret_resol_comp.*

    DEFINE #loc #char
        lc_saldo_rcv            CHAR(011),
        lc_monto_pagado         CHAR(011)

    LET lr_cza_resol.total_registros = 0
    LET lr_resol_comp.folio           = ultimo_folio

    DECLARE cur_comp CURSOR FOR
    SELECT *
    FROM   tmp_resol_esp

    FOREACH cur_comp INTO carga_reg

        INITIALIZE lr_resol_comp.monto_pagado TO NULL
        INITIALIZE lr_resol_comp.saldo_rcv TO NULL

        LET lr_resol_comp.nss               = carga_reg[001,011]
--        LET lr_resol_comp.afore             = carga_reg[012,014]

        LET lc_monto_pagado                 = carga_reg[015,022],".",
                                              carga_reg[023,024]
        LET lr_resol_comp.monto_pagado      = lc_monto_pagado

        LET lc_saldo_rcv                    = carga_reg[025,032],".",
                                              carga_reg[033,034]
        LET lr_resol_comp.saldo_rcv         = lc_saldo_rcv

        SELECT "OK"
        FROM   ret_resol_comp
        WHERE  nss = lr_resol_comp.nss
        GROUP BY 1

        IF STATUS = NOTFOUND THEN
            INSERT INTO ret_resol_comp
            VALUES (lr_resol_comp.*)

            LET lr_cza_resol.total_registros = lr_cza_resol.total_registros + 1
            DISPLAY "TOTAL DE REGISTROS CARGADOS                : ",lr_cza_resol.total_registros AT 11,9
        END IF
    END FOREACH

    INSERT INTO ret_cza_resol
    VALUES(ultimo_folio                 , #folio
           HOY                          , #fecha_operacion
           lr_cza_resol.total_registros , #total_registros
           0                            , #total_importe
           reg_1.nom_archivo            , #nom_archivo
           HOY                          , #fecha_carga
           usuario                      , #usuario
           gs_recibido                    #estado_lote
          )

    DISPLAY "TOTAL DE REGISTROS PROCESADOS              : ",cuantos AT 10,9
    DISPLAY "TOTAL DE REGISTROS CARGADOS                : ",lr_cza_resol.total_registros AT 11,9

    DISPLAY "                                               " AT 18,1
    DISPLAY "                      FOLIO DE CARGA       : ", ultimo_folio AT 14,9
    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
    FOR CHAR enter

END FUNCTION


FUNCTION f_carga_comp_procesar()

    DEFINE lr_resol RECORD 
        nss         LIKE ret_parcial_resol.nss              ,
        num_resol   LIKE ret_parcial_resol.num_resolucion   ,
        sbc_a       LIKE ret_parcial_resol.salario_base_a   ,
        sbc_b       LIKE ret_parcial_resol.salario_base_b   ,
        id_comp     LIKE ret_parcial_resol.id_complemento   
    END RECORD 

    DEFINE lr_cza_resol RECORD LIKE ret_cza_resol.*

    DEFINE lr_comp RECORD
        folio       LIKE ret_parcial_resol.folio        ,
        fol_comp    LIKE ret_parcial_resol.folio        ,
        mto_pagado  LIKE ret_parcial_resol.monto_pagado ,
        saldo_ant   LIKE ret_parcial_resol.saldo_rcv_ant
    END RECORD
    
    
    DEFINE
        lc_salario          CHAR(008)
    
    
    LET lr_cza_resol.total_registros = 0
    
    DECLARE cur_proc CURSOR FOR
    SELECT *
    FROM   tmp_resol_esp

    FOREACH cur_proc INTO carga_reg

        LET lr_comp.fol_comp        = 0
        LET lr_comp.mto_pagado      = 0
        LET lr_comp.saldo_ant       = 0

        LET lr_resol.nss            = carga_reg[007,017]
        LET lr_resol.num_resol      = carga_reg[223,228]
        
        LET lc_salario              = carga_reg[287,291],".",
                                      carga_reg[292,293]
        LET lr_resol.sbc_a          = lc_salario
        
        LET lc_salario              = carga_reg[294,298],".",
                                      carga_reg[299,300]
        LET lr_resol.sbc_b          = lc_salario
        
        LET lr_resol.id_comp        = carga_reg[301,301]

        SELECT MAX(folio)
        INTO   lr_comp.folio
        FROM   ret_parcial_resol
        WHERE  nss = lr_resol.nss
        AND    num_resolucion = lr_resol.num_resol
        
        IF STATUS <> NOTFOUND THEN
            LET lr_cza_resol.total_registros = lr_cza_resol.total_registros + 1
            
            SELECT folio       ,
                   monto_pagado,
                   saldo_rcv
            INTO   lr_comp.fol_comp   ,
                   lr_comp.mto_pagado ,
                   lr_comp.saldo_ant 
            FROM   ret_resol_comp
            WHERE  nss = lr_resol.nss  
            GROUP BY 1,2,3
            HAVING folio = MAX(folio)
            
            UPDATE ret_parcial_resol
            SET    monto_pagado     = lr_comp.mto_pagado,
                   saldo_rcv_ant    = lr_comp.saldo_ant ,
                   salario_base_a   = lr_resol.sbc_a    ,
                   salario_base_b   = lr_resol.sbc_b    ,
                   id_complemento   = lr_resol.id_comp  
            WHERE  nss            = lr_resol.nss
            AND    num_resolucion = lr_resol.num_resol
            AND    folio          = lr_comp.folio
                   
            DISPLAY "TOTAL DE REGISTROS ACTUALIZADOS            : ",lr_cza_resol.total_registros AT 11,9
        END IF
    END FOREACH
    
    INSERT INTO ret_cza_resol
    VALUES(ultimo_folio                 , #folio
           HOY                          , #fecha_operacion
           lr_cza_resol.total_registros , #total_registros
           0                            , #total_importe
           reg_1.nom_archivo            , #nom_archivo
           HOY                          , #fecha_carga
           usuario                      , #usuario
           gs_recibido                    #estado_lote
          )

    DISPLAY "TOTAL DE REGISTROS PROCESADOS              : ",cuantos AT 10,9
    DISPLAY "TOTAL DE REGISTROS ACTUALIZADOS            : ",lr_cza_resol.total_registros AT 11,9

    DISPLAY "                                               " AT 18,1
    DISPLAY "                      FOLIO DE CARGA       : ", ultimo_folio AT 14,9
    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
    FOR CHAR enter    


END FUNCTION

#-----------------------------------------------------------------------#
# Realiza la consulta sobre la tabla de resoluciones complementarias    #
#-----------------------------------------------------------------------#
FUNCTION f_consulta_comp()

    DEFINE #loc #char
        cla_where            ,
        sel_where            CHAR(800)

    DEFINE #loc #smallint
        pos                  SMALLINT

    DEFINE l_record ARRAY[4000] OF RECORD
         nss                  CHAR(11) ,
         folio                INTEGER  ,
         monto_pagado         LIKE ret_resol_comp.monto_pagado,
         saldo_rcv            LIKE ret_resol_comp.saldo_rcv,
         ult_sbc              LIKE ret_resol_comp.ult_sbc,
         nom_archivo          CHAR(20) ,
         fecha_carga          DATE
    END RECORD

    LET pos = 2

    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)

        OPEN WINDOW retm8203 AT 4,4 WITH FORM "RETM8203"
        DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
        DISPLAY " RETM820   CONSULTA DE RES COMPLEMENTARIAS DE RET PARCIALES                    " AT 3,1 ATTRIBUTE(REVERSE)
        DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

        LET INT_FLAG = FALSE

        CONSTRUCT cla_where ON A.nss             ,
                               A.folio           ,
                               B.nom_archivo     ,
                               B.fecha_carga
                          FROM nss               ,
                               folio             ,
                               nom_archivo       ,
                               fecha_carga

            ON KEY ( ESC )
                LET int_flag = FALSE
                EXIT CONSTRUCT

            ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT CONSTRUCT

        END CONSTRUCT

        IF INT_FLAG = TRUE THEN
            LET INT_FLAG = FALSE
            ERROR "  BUSQUEDA CANCELADA...  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW retm8203
            RETURN
        END IF

        LET sel_where = "SELECT  A.nss              ,",
                               " A.folio            ,",
                               " A.monto_pagado     ,",
                               " A.saldo_rcv        ,",
                               " A.ult_sbc          ,",
                               " B.nom_archivo      ,",
                               " B.fecha_carga       ",
                               " FROM ret_resol_comp A, ret_cza_resol B ",
                               " WHERE ",cla_where CLIPPED ,
                               " AND   A.folio = B.folio "
        PREPARE query_cmp FROM sel_where
        DECLARE cur_cmp CURSOR FOR query_cmp

        LET pos = 1

        FOREACH cur_cmp INTO l_record[pos].*

            LET pos = pos + 1

            IF pos >= 4000 THEN
                ERROR "SE SOBREPASO LA CAPACIDAD DEL ARREGLO ..."
                SLEEP 2
                EXIT FOREACH
            END IF

        END FOREACH

        INITIALIZE l_record[pos].* TO NULL

        IF (pos-1) >= 1 THEN
            CALL SET_COUNT(pos-1)

            DISPLAY ARRAY l_record TO scr_com.*

                ON KEY (INTERRUPT)
                    EXIT DISPLAY

            END DISPLAY

            CLOSE WINDOW retm8203
        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLOSE WINDOW retm8203
        END IF
    END IF
END FUNCTION

-- ---------------------------------------------------------------------------



FUNCTION f_valida_arch_comp(ps_id_carga)

    DEFINE
        ps_id_carga         SMALLINT

    DEFINE
        lc_carga_val        CHAR(360),
        lc_tipo_serv        CHAR(006)

    DEFINE
        ls_flag             SMALLINT


    LET ls_flag = 1
    
    DECLARE cur_val CURSOR FOR
    SELECT *
    FROM   tmp_resol_esp

    FOREACH cur_val INTO lc_carga_val

        LET lc_tipo_serv    = lc_carga_val[001,006]

        IF lc_tipo_serv = "030407" AND ps_id_carga <> 3 THEN
            LET ls_flag = 0
            EXIT FOREACH
        END IF
        
        IF lc_tipo_serv <> "030407" AND ps_id_carga = 3 THEN
            LET ls_flag = 0
            EXIT FOREACH
        END IF
        
    END FOREACH
    
    RETURN ls_flag

END FUNCTION 

#-----------------------------------------------------------------------#
# Determina y aparta el ultimo folio para el proceso de carga realizado #
#-----------------------------------------------------------------------#
FUNCTION f_ultimo_folio()

    DEFINE
        li_folio       INTEGER

    SELECT MAX(folio) + 1
    INTO   li_folio
    FROM   glo_folio

    IF li_folio IS NULL THEN
        LET li_folio = 1
    END IF

    INSERT INTO glo_folio VALUES (li_folio)

    RETURN li_folio

END FUNCTION

#-----------------------------------------------------------------------#
# Dado el año en formato [YY] determina el año completo en formato      #
# [YYYY]                                                                #
#-----------------------------------------------------------------------#
FUNCTION f_obten_anio(pc_anio)

    DEFINE
        pc_anio     CHAR(2)


    DEFINE
        ls_anio     SMALLINT


    LET ls_anio = pc_anio

    IF pc_anio[1] = "0" THEN
        LET ls_anio = ls_anio + 2000
    ELSE
        LET ls_anio = ls_anio + 1900
    END IF

    RETURN ls_anio

END FUNCTION

#-----------------------------------------------------------------------#
# Genera la tabla temporal donde se realiza la carga del archivo        #
#-----------------------------------------------------------------------#
FUNCTION f_tabla_tmp()

    WHENEVER ERROR CONTINUE

        DROP TABLE tmp_resol_esp

        CREATE TEMP TABLE tmp_resol_esp
        (
         n_registros  CHAR(360)
        )

    WHENEVER ERROR STOP

END FUNCTION
