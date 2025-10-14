################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETC839  => RECEPCION Y CONSULTA DE NOTIFICACION DE RESOLUCIONES DE  #
#                     RETIROS PARCIALES OPERACION 16                           #
#Fecha creacion    => 22 DE NOVIEMBRE DE 2004                                  #
#By                => ISAI JIMENEZ ROJAS                                       #
#Fecha actualiz.   => 15/12/2004 08:00p.m.                                     #
#Actualizacion     =>                                                          #
#Sistema           => RET                                                      #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE reg_1 RECORD #glo #reg_1
        nom_archivo16         CHAR(20)
    END RECORD

    DEFINE #glo #record
        s_modulo              RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        ruta_arch16           CHAR(200) ,
        carga_reg             CHAR(360) ,
        enter                 CHAR(001) ,
        usuario               CHAR(008)

    DEFINE #glo INTEGER
       ultimo_folio          ,
       cuantos               INTEGER

    DEFINE #glo #smallint
        s_recepcionado        SMALLINT
END GLOBALS


MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I 

    LET HOY = TODAY
    OPEN WINDOW RETC8391 AT 4,4 WITH 19 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY "                          < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC839            RESOLUCIONES DE RETIROS PARCIALES                          " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "OPERACION 16"
        COMMAND "Carga archivo" "Carga Archivo de Resoluciones Parciales(OP 16)"
            CALL carga_archivo()
            CLEAR SCREEN
        COMMAND "Consulta" "Consulta Registro de Resoluciones Parciales(OP 16)"
            CALL consulta_op16()
            CLEAR SCREEN
        COMMAND "Salir" "Salir del Programa "
            EXIT MENU
   CLOSE WINDOW RETC8391
   END MENU
END MAIN     


FUNCTION init()
#i------------
    SELECT USER
    INTO   usuario
    FROM   tab_afore_local

    SELECT A.estado_solicitud
    INTO   s_recepcionado
    FROM   ret_estado A
    WHERE  A.descripcion = "RECEPCIONADO"

    SELECT MAX(folio) + 1
    INTO   ultimo_folio
    FROM   glo_folio

    IF ultimo_folio IS NULL THEN
        LET ultimo_folio = 1
    END IF

    INSERT INTO glo_folio VALUES (ultimo_folio)

    DATABASE safre_tmp
    WHENEVER ERROR CONTINUE

        DROP TABLE tmp_archivo16
        CREATE TABLE tmp_archivo16
        (
         n_registros          CHAR(400)
        )

    WHENEVER ERROR STOP
    DATABASE safre_af
END FUNCTION


FUNCTION carga_archivo()
#ca--------------------
    CALL init()
    OPEN WINDOW RETC8391A AT 4,4 WITH FORM "RETC8391"  -- ATTRIBUTE(BORDER)
    DISPLAY "                          < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETC839   CARGA ARCHIVO DE RESOLUCIONES DE RETIROS PARCIALES                  " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    LET cuantos = 0

    INPUT BY NAME reg_1.nom_archivo16 WITHOUT DEFAULTS
        BEFORE FIELD nom_archivo16
            LET reg_1.nom_archivo16 = NULL
            CLEAR FORM

        AFTER FIELD nom_archivo16
            IF reg_1.nom_archivo16 IS NULL THEN
                ERROR "   CAMPO NO PUEDE SER NULO  "
                NEXT FIELD nom_archivo16
            END IF

            SELECT "OK"
            FROM   ret_cza_resp16    
            WHERE  nom_archivo = reg_1.nom_archivo16

            IF STATUS <> NOTFOUND THEN
                ERROR " ARCHIVO YA PROCESADO CON ANTERIORIDAD  "
              NEXT FIELD nom_archivo16
            END IF
          
            WHENEVER ERROR CONTINUE
                SELECT *
                INTO   s_modulo.*
                FROM   seg_modulo
                WHERE  modulo_cod = "ret"

                LET ruta_arch16 = s_modulo.ruta_rescate CLIPPED,"/",
                                  reg_1.nom_archivo16 CLIPPED

                LOAD FROM ruta_arch16 DELIMITER "+"
                INSERT INTO safre_tmp:tmp_archivo16

                SELECT count(*)
                INTO   cuantos
                FROM   safre_tmp:tmp_archivo16

                IF cuantos = 0 THEN
                    ERROR "  NOMBRE DE ARCHIVO INCORRECTO O ARCHIVO VACIO  "
                    NEXT FIELD nom_archivo16
                ELSE
                    CALL primer_paso()  #pp 
                    EXIT INPUT
                END IF
            WHENEVER ERROR STOP

        ON KEY (INTERRUPT)
            ERROR " PROCESO CANCELADO  "
            SLEEP 1
            ERROR ""
            EXIT INPUT            
     END INPUT
     CLOSE WINDOW RETC8391A
END FUNCTION


FUNCTION primer_paso()
#pp------------------
    DEFINE reg_2             RECORD LIKE ret_parcial_resp16.*
    DEFINE reg_3             RECORD LIKE ret_cza_resp16.*      

    DEFINE #loc #char
        v_fecha_fall_mat_des ,
        v_fecha_ini_vigencia ,
        v_fecha_fin_vigencia ,
        v_fecha_operacion    CHAR(010)

    DECLARE cur_2 CURSOR FOR
    SELECT *
    FROM   safre_tmp:tmp_archivo16

    FOREACH cur_2 INTO carga_reg
        IF carga_reg[001,002] = "" THEN
            ERROR "  ERROR, ARCHIVO VACIO "
            EXIT FOREACH
        ELSE
            IF carga_reg[001,002] = "03" THEN  --DETALLE
                LET reg_2.nss                = carga_reg[007,017]
                LET reg_2.consecutivo        = carga_reg[167,172]
                LET reg_2.diag_procesar      = carga_reg[285,287]

                --VERIFICA QUE NO EXISTA
                SELECT "OK"
                FROM   ret_parcial_resp16
                WHERE  nss   = reg_2.nss
                AND    folio = ultimo_folio

                IF STATUS = NOTFOUND THEN
                    INSERT INTO ret_parcial_resp16
                    VALUES (ultimo_folio             ,
                            reg_2.nss                ,
                            reg_2.consecutivo        ,
                            reg_2.diag_procesar
                           )
                END IF
            END IF

            IF carga_reg[001,002] = "01" THEN
                LET v_fecha_operacion      = carga_reg[019,020],"/",
                                             carga_reg[021,022],"/",
                                             carga_reg[015,018]
                LET reg_3.fecha_operacion = v_fecha_operacion
            END IF

            IF carga_reg[001,002] = "09" THEN
                LET reg_3.total_registros = carga_reg[028,033]
                LET reg_3.total_importe   = carga_reg[034,048]
            END IF
        END IF 
    END FOREACH

    IF carga_reg[001,002] = "" THEN 
        ERROR "  CARGA DEL ARCHIVO CANCELADA  "
    ELSE
        SELECT "OK"
        FROM   ret_cza_resp16
        WHERE  nom_archivo = reg_1.nom_archivo16

        IF STATUS = NOTFOUND THEN
            INSERT INTO ret_cza_resp16
            VALUES(ultimo_folio          , #folio
                   reg_3.fecha_operacion , #fecha_operacion
                   reg_3.total_registros , #total_registros
                   reg_3.total_importe   , #total_importe
                   reg_1.nom_archivo16   , #nom_archivo
                   HOY                   , #fecha_carga   
                   usuario               , #usuario
                   s_recepcionado          #estado_lote
                  )

            LET cuantos = cuantos -2

            DISPLAY "TOTAL DE REGISTROS CARGADOS(DETALLE): ",cuantos AT 11,15
            DISPLAY "FOLIO : ", ultimo_folio AT 17,2
            DISPLAY "EL ARCHIVO ", reg_1.nom_archivo16 CLIPPED, 
                    " HA SIDO CARGADO " AT 18,2
        ELSE
            DELETE
            FROM  glo_folio
            WHERE folio = ultimo_folio
        END IF
    END IF  

    PROMPT " PROCESO FINALIZADO...<ENTER> PARA SALIR " ATTRIBUTE(REVERSE) 
    FOR CHAR enter -- ATTRIBUTE(REVERSE)
END FUNCTION

  
FUNCTION consulta_op16()
#cop16-----------------
    DEFINE #loc #char
        cla_where            , 
        sel_where            CHAR(800)

    DEFINE #loc #smallint
        pos                  SMALLINT

    DEFINE l_record ARRAY[2000] OF RECORD
         nss                  CHAR(11) , 
         folio                INTEGER  ,
         consecutivo          DECIMAL(11,0),
         diag_procesar        CHAR(03) , 
         nom_archivo          CHAR(20)
    END RECORD

    LET pos = 2

    IF (pos-1) >= 1 THEN
        CALL  SET_COUNT(pos-1)

        OPEN WINDOW RETC8392 AT 4,4 WITH FORM "RETC8392"  -- ATTRIBUTE(BORDER)
        DISPLAY "  < Ctrl-C > Salir                                       ",
                "< ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
        DISPLAY " RETC839      CONSULTA DE RESOLUCIONES DE RETIROS PARCIALES",
                "                    " AT 3,1 ATTRIBUTE(REVERSE)
        DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

        LET int_flag = FALSE

        CONSTRUCT cla_where ON A.nss             ,
                               B.folio           ,
                               B.consecutivo     ,
                               A.diag_procesar   ,   
                               B.nom_archivo     
                          FROM nss              ,
                               folio            ,
                               consecutivo      ,
                               diag_procesar    , 
                               nom_archivo      
            ON KEY ( ESC )      
                LET int_flag = FALSE
                EXIT CONSTRUCT
            ON KEY (control-c)
                LET int_flag = TRUE
                EXIT CONSTRUCT
        END CONSTRUCT

        IF int_flag = TRUE THEN
            LET int_flag = FALSE
            ERROR "  BUSQUEDA CANCELADA...  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW RETC8392 
            RETURN
        END IF

        LET sel_where = "SELECT  A.nss                   , ",
                               " A.folio                 , ",
                               " A.consecutivo           , ",
                               " A.diag_procesar         , ",
                               " B.nom_archivo             ",
                               " FROM ret_parcial_resp16 A,",
                               "      ret_cza_resp16     B ",
                               " WHERE ",cla_where CLIPPED , 
                               " AND   A.folio = B.folio "
        PREPARE query_1 FROM sel_where
        DECLARE cur_3 CURSOR FOR query_1

        LET pos = 1

        FOREACH cur_3 INTO l_record[pos].*
            LET pos = pos + 1
        END FOREACH

        INITIALIZE l_record[pos].* TO NULL

      IF (pos-1) >= 1 THEN
         CALL SET_COUNT(pos-1)
         DISPLAY ARRAY l_record TO scr_1.*
            ON KEY (INTERRUPT)
               EXIT DISPLAY
         END DISPLAY
         CLOSE WINDOW RETC8392 
      ELSE
         ERROR "  NO EXISTEN REGISTROS  "     
         SLEEP 1
         ERROR ""
         CLOSE WINDOW RETC8392 
      END IF
   END IF
END FUNCTION 

