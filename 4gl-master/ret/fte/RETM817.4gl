################################################################################
#Owner             => E.F.P.                                                   #
#Programa RETM817  => RECEPCION Y CONSULTA DE NOTIFICACION DE RESOLUCIONES DE  #
#                     RETIROS PARCIALES OPERACION 07                           #
#Fecha creacion    => 22 DE NOVIEMBRE DE 2004                                  #
#By                => JUAN CARLOS MENDOZA MORENO                               #
#Fecha actualiz.   => 15 DE MAYO DE 2009                                       #
#Actualizacion     => JAVIER GONZALEZ JERONIMO                                 #
#                  => Modificaciones para soportar los cambios indicados por   #
#                  => la circular 31-11 de Retiros Parciales. Se agregan       #
#                  => nuevos campos al layout de la operacion 7                #
#Sistema           => RET                                                      #
################################################################################
#Requerimiento     => EFPS-266  17-Mzo-2014   Alejandro Chagoya Salazar        #
#Descripcion       => Se agregan los nuevos campos del contrato de la Nueva    #
#                  => Plataforma de Retiros (asi como el orden)                #
################################################################################

DATABASE safre_af

    DEFINE #glo #record
        gs_modulo             RECORD LIKE seg_modulo.*

    DEFINE #glo #date
        HOY                   DATE

    DEFINE #glo #char
        enter                 CHAR(001) ,
        usuario               CHAR(008)

    DEFINE 
       m_folio ,
       m_cuantos               INTEGER,
       m_fecha                 DATE

    DEFINE #glo #smallint
        gs_recibido          SMALLINT

################################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        ACCEPT KEY CONTROL-I

    CALL STARTLOG(FGL_GETENV("USER")||".RETM817.log") 
    CALL init()

    OPEN WINDOW RETM8171 AT 4,4 WITH 20 ROWS, 75 COLUMNS ATTRIBUTE(BORDER)
    DISPLAY "                          < Ctrl-C > Salir                                     " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETM817            RESOLUCIONES DE RETIROS PARCIALES                          " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

    MENU "OPERACION 07"
        COMMAND "Folio" "Asigna folio a Resoluciones Parciales (OP 07)"
            CALL f_procesa()
            CLEAR SCREEN
            CLOSE WINDOW RETM8172

        COMMAND "Consulta" "Consulta Registro de Resoluciones Parciales (OP 07)"
            CALL consulta_op07()
            CLEAR SCREEN

        COMMAND "Salir" "Salir del Programa "
            EXIT MENU
    END MENU

    CLOSE WINDOW RETM8171

END MAIN

#################################################################################
FUNCTION init()

    LET HOY = TODAY   LET m_cuantos = 0
    LET m_fecha = TODAY 
    SELECT USER, * INTO usuario, gs_modulo.*
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    SELECT A.status INTO gs_recibido
    FROM   ret_status A
    WHERE  A.descripcion = "RECEPCIONADO"

END FUNCTION

#################################################################################
FUNCTION f_procesa()

    OPEN WINDOW RETM8172 AT 4,4 WITH FORM "RETM8171"  -- ATTRIBUTE(BORDER)
    DISPLAY "    < ESC > Inicia Proceso                        < Ctrl-C > Salir             " AT 1,1 ATTRIBUTE(REVERSE)
    DISPLAY " RETM817    ASIGNA FOLIO DE RESOLUCIONES DE RETIROS PARCIALES         " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING"DD-MM-YYYY" AT 3,66 ATTRIBUTE(REVERSE)

    LET INT_FLAG = FALSE

          INPUT m_fecha WITHOUT DEFAULTS FROM FORMONLY.fecha
             AFTER FIELD fecha
                IF m_fecha IS NULL THEN
                      ERROR "ERROR, INGRESE UNA FECHA VALIDA"
                      NEXT FIELD fecha
                END IF

                IF m_fecha > TODAY THEN
                   ERROR "ERROR, LA FECHA NO PUEDE SER MAYOR AL DIA ACTUAL"
                   NEXT FIELD fecha
                END IF

                IF f_cuenta() = 0 THEN
                   ERROR "NO EXISTEN REGISTROS, VERIFIQUE"
                   NEXT FIELD fecha
                END IF

             ON KEY (ESC)
                IF m_fecha IS NULL THEN
                      ERROR "ERROR, INGRESE UNA FECHA VALIDA"
                      NEXT FIELD fecha
                END IF

                IF m_fecha > TODAY THEN
                   ERROR "ERROR, LA FECHA NO PUEDE SER MAYOR AL DIA ACTUAL"
                   NEXT FIELD fecha
                END IF

                IF f_cuenta() = 0 THEN
                   ERROR "NO EXISTEN REGISTROS, VERIFIQUE"
                   NEXT FIELD fecha
                END IF
                EXIT INPUT
                LET INT_FLAG = FALSE

             ON KEY (CONTROL-C, INTERRUPT)
                LET INT_FLAG = TRUE
                ERROR "PROCESO CANCELADO" SLEEP 2
                ERROR ""
                EXIT INPUT
          END INPUT

    WHILE NOT INT_FLAG
      DISPLAY "REGISTROS ENCONTRADOS: ", m_cuantos USING "<<<<<<<" AT 5,2 ATTRIBUTE(REVERSE)
        PROMPT "¿DESEA ASIGNAR FOLIO A ESTOS REGISTROS? (S/N)" FOR CHAR enter
        IF enter MATCHES "[SsNn]" THEN
           IF enter MATCHES "[Ss]" THEN
              CALL primer_paso()
              EXIT WHILE
           ELSE
              ERROR "PROCESO CANCELADO" SLEEP 2
              ERROR ""
              EXIT WHILE
           END IF
        ELSE
          ERROR "SOLO PRESIONE S o N"
        END IF  
    END WHILE

END FUNCTION

#################################################################################
FUNCTION primer_paso()
DEFINE l_nss  CHAR(11)

LET m_folio = f_ultimo_folio()
LET m_cuantos = 0

    DECLARE cur_2 CURSOR FOR
      SELECT nss FROM ret_parcial_resol
      WHERE DATE(fecha_carga_afore) = m_fecha
      AND folio = -1
      AND estado_registro = 0

     INITIALIZE l_nss TO NULL
    FOREACH cur_2 INTO l_nss

      WHENEVER ERROR CONTINUE
          UPDATE ret_parcial_resol SET folio = m_folio,
                 estado_registro = gs_recibido
           WHERE nss = l_nss
           AND DATE(fecha_carga_afore) = m_fecha
           AND folio = -1
           AND estado_registro = 0

       IF SQLCA.SQLERRD[3] = 1 THEN
          LET m_cuantos = m_cuantos + SQLCA.SQLERRD[3]
       ELSE
          CALL ERRORLOG ("ERROR AL ACTUALIZAR ret_parcial_resol NSS: "||l_nss||" "||SQLCA.SQLERRD[3])
       END IF

      WHENEVER ERROR STOP

       INITIALIZE l_nss TO NULL
    END FOREACH

     DISPLAY "TOTAL DE REGISTROS ACTUALIZADOS : ",m_cuantos AT 7,9 ATTRIBUTE (REVERSE)
     DISPLAY "FOLIO ", m_folio USING "<<<<<<<" AT 9,2 ATTRIBUTE(REVERSE)

   WHENEVER ERROR CONTINUE
        INSERT INTO ret_cza_resol
        VALUES(m_folio ,    -- folio
               m_fecha ,    -- fecha_operacion
               m_cuantos ,  -- total_registros
               0,           -- total_importe
               "" ,         -- nom_archivo
               HOY,         -- fecha_carga
               usuario,     -- usuario
               gs_recibido  -- estado_lote
              )

       IF SQLCA.SQLERRD[3] != 1 THEN
          CALL ERRORLOG ("ERROR AL INSERTAR ret_cza_resol")
       END IF

      WHENEVER ERROR STOP

    PROMPT " PROCESO FINALIZADO. PRESIONE <ENTER> PARA REGRESAR AL MENU "
    FOR CHAR enter

END FUNCTION

#################################################################################
FUNCTION consulta_op07()
#cop07-----------------

    DEFINE #loc #char
        cla_where            ,
        sel_where            CHAR(800)

    DEFINE #loc #smallint
        pos                  SMALLINT

    DEFINE l_record ARRAY[2000] OF RECORD
         folio_procesar       CHAR(50) ,
         nss                  CHAR(11) ,
         folio                INTEGER  ,
         diag_procesar        CHAR(03) ,
         nombre_imss          CHAR(50) ,
         nombre_procanase     CHAR(50) ,
         nombre_dbnsar        CHAR(40) ,
         paterno_bdnsar       CHAR(40) ,
         materno_bdnsar       CHAR(40) ,
         tipo_prestacion      SMALLINT ,
         desc_prestacion      CHAR(20) ,
         fecha_fall_mat_des   DATE ,
         salario_base_a       LIKE ret_parcial_resol.salario_base_a,
         fecha_ini_vigencia   DATE ,
         salario_base_b       LIKE ret_parcial_resol.salario_base_b,
         fecha_fin_vigencia   DATE ,
         id_complemento       LIKE ret_parcial_resol.id_complemento,
         num_resolucion       INTEGER ,
         monto_pagado         LIKE ret_parcial_resol.monto_pagado ,
         saldo_rcv_ant        LIKE ret_parcial_resol.saldo_rcv_ant ,
         id_aport_cs          LIKE ret_parcial_resol.salario_base_a ,
         f_carga              DATE,
         h_carga              CHAR(8)
    END RECORD

    DEFINE
        r_consulta RECORD
           nss           CHAR(11),
           fecha         DATE,
           t_prest       SMALLINT
        END RECORD,
        l_prest_desc          CHAR(20)

        OPEN WINDOW RETM8175 AT 4,4 WITH FORM "RETM8175"
        DISPLAY "  < Ctrl-C > Salir                                       < ESC > Consulta      " AT 1,1 ATTRIBUTE(REVERSE)
        DISPLAY " RETM817      CONSULTA DE RESOLUCIONES DE RETIROS PARCIALES                    " AT 3,1 ATTRIBUTE(REVERSE)
        DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

        LET int_flag = FALSE

        INITIALIZE r_consulta.*, cla_where, sel_where TO NULL

        INPUT r_consulta.* WITHOUT DEFAULTS FROM FORMONLY.nss,
                                                 FORMONLY.fecha,
                                                 FORMONLY.tipo_prest

           BEFORE INPUT
            INITIALIZE cla_where TO NULL

            AFTER FIELD nss

               IF r_consulta.nss IS NOT NULL THEN
                  LET cla_where = " A.nss ='",r_consulta.nss,"'"
               END IF

            AFTER FIELD fecha

               IF r_consulta.fecha IS NOT NULL THEN
                  IF LENGTH(cla_where) > 0 THEN
                     LET cla_where = cla_where CLIPPED, " AND DATE(A.fecha_carga_afore) = '", r_consulta.fecha, "'"
                  ELSE
                     LET cla_where = " DATE(A.fecha_carga_afore) = '", r_consulta.fecha, "'"
                  END IF
               END IF

            AFTER FIELD tipo_prest

                 IF r_consulta.t_prest IS NOT NULL THEN
                    IF r_consulta.t_prest = 6 THEN
                       LET l_prest_desc = "DESEMPLEO"
                     ELSE
                       LET l_prest_desc = "MATRIMONIO"
                    END IF

                    IF LENGTH(cla_where) > 0 THEN
                       LET cla_where = cla_where CLIPPED, " AND A.tipo_prestacion = ", r_consulta.t_prest
                    ELSE
                       LET cla_where = " A.tipo_prestacion = ",r_consulta.t_prest
                    END IF

                 ELSE
                    LET l_prest_desc = ""
                 END IF

                  DISPLAY l_prest_desc TO FORMONLY.prest_desc

            ON KEY ( ESC )
                IF LENGTH(cla_where CLIPPED) = 0 THEN
                   ERROR "DEBE INGRESAR AL MENOS UN CRITERIO DE BUSQUEDA"
                   NEXT FIELD nss
                END IF

                LET int_flag = FALSE
                EXIT INPUT

            ON KEY (CONTROL-C)
                LET int_flag = TRUE
                EXIT INPUT

        END INPUT

        IF INT_FLAG = TRUE THEN
            LET INT_FLAG = FALSE
            ERROR "  BUSQUEDA CANCELADA...  "
            SLEEP 1
            ERROR ""
            CLEAR SCREEN
            CLOSE WINDOW RETM8175
            RETURN
        END IF
        LET sel_where = " SELECT                ",
                        " A.folio_t_procesar ,  ",
                        " A.nss              ,  ",
                        " A.folio            ,  ",
                        " A.diag_procesar    ,  ",
                        " A.nombre_imss      ,  ",
                        " A.nombre_procanase ,  ",
                        " A.nombre_bdnsar    ,  ",
                        " A.paterno_bdnsar   ,  ",
                        " A.materno_bdnsar   ,  ",
                        " A.tipo_prestacion  ,  ",
                        " CASE WHEN a.tipo_prestacion = '06' THEN 'DESEMPLEO'  ",
                        "      WHEN a.tipo_prestacion = '07' THEN 'MATRIMONIO' ",
                        " END  desc_prestacion, ",
                        " A.fecha_fall_mat_des, ",
                        " A.salario_base_a    , ",
                        " A.fecha_ini_vigencia, ",
                        " A.salario_base_b    , ",
                        " A.fecha_fin_vigencia, ",
                        " A.id_complemento    , ",
                        " A.num_resolucion    , ",
                        " A.monto_pagado      , ",
                        " A.saldo_rcv_ant     , ",
                        " A.id_aportacion_cs  , ",
                        " DATE(A.fecha_carga_afore), ",
                        " SUBSTR(fecha_carga_afore,12,19)  ",
                        " FROM ret_parcial_resol A ",
                        " WHERE ",cla_where CLIPPED
--DISPLAY  "qry " sel_where SLEEP 1
        PREPARE query_1 FROM sel_where
        DECLARE cur_3 CURSOR FOR query_1

        LET pos = 1

        FOREACH cur_3 INTO l_record[pos].*

            LET pos = pos + 1
            IF pos >= 2000 THEN
               ERROR "SE HA EXCEDIDO LA CAPACIDAD DEL ARREGLO, SOLO SE MUESTRAN 2000 REGISTROS"
             END IF
        END FOREACH

        INITIALIZE l_record[pos].* TO NULL

        IF (pos-1) >= 1 THEN

            OPEN WINDOW RETM8172 AT 4,4 WITH FORM "RETM8172"
            DISPLAY "  < Ctrl-C > Salir                                                             " AT 1,1 ATTRIBUTE(REVERSE)
            DISPLAY " RETM817      CONSULTA DE RESOLUCIONES DE RETIROS PARCIALES                    " AT 3,1 ATTRIBUTE(REVERSE)
            DISPLAY HOY USING"DD-MM-YYYY" AT 3,65 ATTRIBUTE(REVERSE)

            CALL SET_COUNT(pos-1)
            DISPLAY ARRAY l_record TO scr_1.*

                ON KEY (INTERRUPT)
                    EXIT DISPLAY

            END DISPLAY

            CLOSE WINDOW RETM8172
            CLOSE WINDOW RETM8175
        ELSE
            ERROR "  NO EXISTEN REGISTROS  "
            SLEEP 1
            ERROR ""
            CLOSE WINDOW RETM8172
            CLOSE WINDOW RETM8175
        END IF

END FUNCTION

#################################################################################
FUNCTION f_cuenta()
LET m_cuantos = 0

   SELECT COUNT(*) INTO m_cuantos 
   FROM safre_af:ret_parcial_resol
   WHERE DATE(fecha_carga_afore) = m_fecha
   AND folio = -1
   AND estado_registro = 0

RETURN m_cuantos

END FUNCTION

#################################################################################
FUNCTION f_ultimo_folio()
DEFINE  li_folio       INTEGER

LET li_folio = NULL
    SELECT MAX(folio) + 1 INTO li_folio
    FROM   glo_folio

    IF li_folio IS NULL THEN
        LET li_folio = 1
    END IF

    INSERT INTO glo_folio VALUES (li_folio)

 RETURN li_folio

END FUNCTION
