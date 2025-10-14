##############################################################################
#Project           => SAFRE (Mexico)
#Owner             => E.F.P.
#Programa AFIM008  => CONSULTA DE CONTROL DOCUMENTAL
#Fecha actualiz.   => 16 MARZO 2001
#Actualizacion     => LAURA EUGENIA CURTES GUZMAN
#Fecha actualiz.   => 
#Sistema           => AFI
##############################################################################

DATABASE safre_af
GLOBALS
    DEFINE cap_1 RECORD
               condicion          CHAR(1),
               fecha_auto         DATE,
               tip_solicitud      SMALLINT,
               operador           CHAR(08)
           END RECORD,

           cap_1_1 RECORD
               lote               INTEGER,
               condicion          CHAR(1),
               fecha_auto         DATE,
               tip_solicitud      SMALLINT,
               operador           CHAR(08)
           END RECORD,

           cap_con RECORD
              lote                INTEGER
           END RECORD,

           desc_condicion         CHAR(11),
           desc_solicitud         CHAR(24),
           desc_operador          CHAR(40),
           descripcion            CHAR(35),
           enter                  CHAR(01),
           completar              CHAR(01),
           usuario                CHAR(08),

           HOY                    DATE,

           fueron                 INTEGER,
           registros              INTEGER,
           son_lotes              INTEGER,
           son_folios             INTEGER,
           son_cond               INTEGER,
           son_exp                INTEGER,

           rechaza_rechazado      SMALLINT,
           primera_vez            SMALLINT,
           salir                  SMALLINT,
           salida                 SMALLINT,
           ss                     SMALLINT,

           cap_con_2  ARRAY[1000] OF RECORD
                  solicitud       SMALLINT,
                  folio           INTEGER,
                  nss             CHAR(11),
                  estado_sol      SMALLINT
           END RECORD,

           cap_2  ARRAY[1000] OF RECORD
                  solicitud       SMALLINT,
                  folio           INTEGER,
                  nss             CHAR(11),
                  estado_sol      CHAR(01)
           END RECORD,

           cap_2_2 RECORD
                  solicitud       SMALLINT,
                  folio           INTEGER,
                  nss             CHAR(11),
                  estado_sol      CHAR(01)
           END RECORD,

           cap_con_3 ARRAY[100] OF RECORD
                  consecutivo     SMALLINT,
                  descripcion     CHAR(35),
                  obliga          SMALLINT,
                  estado          SMALLINT,
                  ubicacion       SMALLINT
           END RECORD,

           cap_3 ARRAY[100] OF RECORD
                  consecutivo     SMALLINT,
                  descripcion     CHAR(35),
                  obliga          CHAR(01),
                  estado          CHAR(01),
                  ubicacion       SMALLINT
           END RECORD,

           cap_3_3 RECORD
                  consecutivo     SMALLINT,
                  descripcion     CHAR(35),
                  obliga          CHAR(01),
                  estado          CHAR(01),
                  ubicacion       SMALLINT
           END RECORD,

           cap_4 ARRAY[100] OF RECORD
                  consecutivo     SMALLINT,
                  codigo          SMALLINT,
                  desc_codigo     CHAR(30),
                  rechazo         SMALLINT
           END RECORD,

           cap_4_4 RECORD
                  consecutivo     SMALLINT,
                  codigo          SMALLINT,
                  desc_codigo     CHAR(30),
                  rechazo         SMALLINT
           END RECORD,

           cap_5 ARRAY[100] OF RECORD
                  consecutivo     SMALLINT,
                  codigo          SMALLINT,
                  desc_codigo     CHAR(30),
                  rechazo         SMALLINT
           END RECORD,

           cap_6_6 RECORD
                  cod_promotor     LIKE afi_recepcion.cod_promotor,
                  fecha            LIKE afi_recepcion.fecha_recepcion,
                  sucursal         LIKE pro_mae_promotor.agenc_cod,
                  paterno          LIKE pro_mae_promotor.paterno,
                  materno          LIKE pro_mae_promotor.materno,
                  nombres          LIKE pro_mae_promotor.nombres
           END RECORD,

           cap_6 RECORD
                  cod_promotor     CHAR(10),
                  nombre           CHAR(32),
                  sucursal         CHAR(10),
                  fecha            DATE
           END RECORD,

           la_fol RECORD
                  folio            INTEGER
           END RECORD,

           g_paramgrales RECORD LIKE glo_parametro.*

        ### Mod ###
	DEFINE
           pos			   INTEGER,
	   sel_where		   CHAR(1000),
           cla_where		   CHAR(1000),
	   z_reg2		   ARRAY[9] OF SMALLINT,
           z_reg		   RECORD  
				   orden_1     SMALLINT,           
				   orden_2     SMALLINT,           
				   orden_3     SMALLINT,           
				   orden_4     SMALLINT,           
				   orden_5     SMALLINT,
				   orden_6     SMALLINT,
				   orden_7     SMALLINT
    				   END RECORD
          
	DEFINE
           reg			   ARRAY[5000] OF RECORD
                                   n_seguro		CHAR(11),
				   n_folio          DECIMAL(10,0),
				   lote      		INTEGER,
				   tipo_solicitud	SMALLINT,
				   fecha		DATE,
				   usuario		CHAR(8),
 				   docto_obliga		CHAR(1)
       				   END RECORD,
	   sw_4			   SMALLINT

        
	DEFINE
          vn_seguro		   CHAR(11),
	  vn_folio          DECIMAL(10,0),
          vregistros		   INTEGER,
	  vdocto_obliga		   CHAR(1),
	  vestado_exp		   SMALLINT
   
END GLOBALS

############################################################################
MAIN
    DEFER INTERRUPT
    OPTIONS
    PROMPT LINE LAST,
          #ACCEPT KEY CONTROL-I,
           INPUT WRAP

    CALL STARTLOG('AFIM008.log')

       WHENEVER ERROR CONTINUE

    LET HOY = TODAY
    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "AFIM0081" ATTRIBUTE(BORDER)
    DISPLAY " AFIM008                     CONTROL   DOCUMENTAL                              " AT 3,1 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "dd-mm-yyyy" AT 3,68 ATTRIBUTE(REVERSE)
    DISPLAY "                   C O N D I C I O N    D O C U M E N T A L                    " AT 6,1 ATTRIBUTE(REVERSE)

    MENU "CTR DOCUMENTAL"
         COMMAND "Agregar" "Alta "                   #a
                  CALL alta()
         COMMAND "Consultar" "Consulta "             #c
                  CALL consulta()
         COMMAND "Modificar" "Modifica "             #m
                  CALL modifica()
         COMMAND "Eliminar" "Elimina "               #e
                  CALL elimina()
         COMMAND "Salir" "Salir del Programa"
              EXIT PROGRAM
    END MENU
END MAIN

############################################################################
FUNCTION init()
  INITIALIZE  desc_condicion, desc_solicitud,
              desc_operador,  descripcion      TO NULL
    INITIALIZE cap_1.*,   enter,     cap_2_2.*, 
               cap_3_3.*, cap_4_4.*, cap_6_6.*,
               cap_6.*                         TO NULL
    LET salir  = 0
    LET salida = 0
    LET ss     = 0
    LET fueron = 0
    LET registros = 0
    LET vregistros= 0
    LET son_lotes = 0
    LET son_folios= 0
    LET son_cond  = 0
    LET son_exp   = 0
    LET primera_vez = 0
    LET rechaza_rechazado = 0
	 LET cap_con.lote = 0

    FOR ss = 1 TO 1000
         INITIALIZE cap_con_2[ss].* TO NULL
         INITIALIZE cap_2[ss].* TO NULL
    END FOR

    LET ss = 0
    FOR ss = 1 TO 100
         INITIALIZE cap_3[ss].* TO NULL
         INITIALIZE cap_4[ss].* TO NULL
         INITIALIZE cap_5[ss].* TO NULL
    END FOR

   SELECT * , USER
          INTO   g_paramgrales.* ,
                 usuario        
          FROM   glo_parametro

END FUNCTION

############################################################################
FUNCTION alta()
#a-----------------
    DEFINE cont_lotes      INTEGER,
           fecha_captura   DATE,
           cerrada         CHAR(1)
    OPTIONS
          ACCEPT KEY CONTROL-I,
          INPUT WRAP

    INITIALIZE fecha_captura, cerrada TO NULL
    CALL init()
    LET cont_lotes = 0
    LET son_lotes = 0

    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " A L T A " AT 1,70 ATTRIBUTE ( REVERSE )
    DISPLAY " < Ctrl-C >Salir  < ESC >Acepta Folio                      " 
    AT 2,1
    INITIALIZE cap_1 TO NULL

    WHILE TRUE
       PROMPT " Agregar Solicitudes Completas o Rechazadas (C/R) ? "
       FOR CHAR cap_1.condicion 
       IF cap_1.condicion NOT MATCHES "[CcRr]" THEN
           ERROR "  Inexistente el Tipo de Condicion" ATTRIBUTE(NORMAL)
           CONTINUE WHILE
       ELSE
          IF cap_1.condicion MATCHES "[Cc]" THEN
                LET cap_1.condicion = "C"
              EXIT WHILE
            END IF

          IF cap_1.condicion MATCHES "[Rr]" THEN
                LET cap_1.condicion = "R"
              EXIT WHILE
            END IF
       END IF
    END WHILE

    LET cap_1.tip_solicitud = NULL
    LET cap_1.fecha_auto = TODAY
    INPUT BY NAME cap_1.* WITHOUT DEFAULTS

          BEFORE FIELD condicion
                 CASE cap_1.condicion
                   WHEN "C"
                          LET desc_condicion = "COMPLETAS"
                   WHEN "R"
                          LET desc_condicion = "RECHAZADAS"
                   WHEN " "
                      ERROR "  Tipo de Condicion Inexistente" ATTRIBUTE(NORMAL)
                       NEXT FIELD condicion
                   OTHERWISE
                      ERROR "  Tipo de Condicion Inexistente" ATTRIBUTE(NORMAL)
                       NEXT FIELD condicion
                 END CASE
                 DISPLAY cap_1.condicion TO condicion
                 DISPLAY desc_condicion TO desc_condicion
                 DISPLAY usuario TO operador
                 NEXT FIELD fecha_auto

          AFTER FIELD fecha_auto
               IF cap_1.fecha_auto IS NULL THEN
                   ERROR "  La Fecha no puede ser Nula" ATTRIBUTE(NORMAL)
                   NEXT FIELD fecha_auto
               END IF


          AFTER FIELD tip_solicitud
               IF cap_1.tip_solicitud IS NULL THEN
                     CALL ven_solicitud() RETURNING cap_1.tip_solicitud, 
                                                    desc_solicitud
                    IF cap_1.tip_solicitud IS NULL  OR
                       cap_1.tip_solicitud = 0      THEN
                         ERROR "  El Tipo de Solicitud No puede ser nulo o cero"
                         ATTRIBUTE(NORMAL)
                        NEXT FIELD tip_solicitud
                    END IF
                    DISPLAY cap_1.tip_solicitud TO tip_solicitud
                    DISPLAY desc_solicitud TO desc_solicitud
               ELSE
                    SELECT a.desc_solicitud 
                           INTO desc_solicitud 
                                FROM tab_tipo_solic a
                                    WHERE a.tipo_solicitud = cap_1.tip_solicitud
                    IF STATUS = NOTFOUND THEN
                         ERROR " El Tipo de Solicitud No Existe"
                         ATTRIBUTE(NORMAL)
                         NEXT FIELD tip_solicitud
                    ELSE
                           DISPLAY desc_solicitud TO desc_solicitud
                    END IF
               END IF


            ON KEY (ESC)
                     IF cap_1.tip_solicitud MATCHES " " OR
                        cap_1.tip_solicitud IS NULL     THEN
                           ERROR "  Tipo de Condicion No puede ser Nula"
                           ATTRIBUTE(NORMAL)
                           NEXT FIELD tip_solicitud
                     END IF
                     IF cap_1.fecha_auto IS NULL THEN
                        ERROR "  La Fecha no puede ser Nula" ATTRIBUTE(NORMAL)
                        NEXT FIELD fecha_auto
                     END IF

                     SELECT MAX(w.lote), COUNT(*)
                            INTO cap_con.lote, son_lotes
                            FROM afi_ctr_lote w
                            WHERE w.fecha = cap_1.fecha_auto
                              AND w.usuario = usuario
                              AND w.estado  = cap_1.condicion
                              AND w.tipo_solicitud = cap_1.tip_solicitud
                              AND w.cerrada        = "A"
                     IF son_lotes = 0 THEN
                        SET LOCK MODE TO WAIT

                        SELECT COUNT(*)
                               INTO cont_lotes
                               FROM afi_ctr_lote
                        IF cont_lotes = 0 THEN
                           LET cap_con.lote = 1
                        ELSE
                           SELECT MAX(lote+1)
                                  INTO cap_con.lote
                                  FROM afi_ctr_lote
                        END IF

                        INSERT INTO afi_ctr_lote VALUES(cap_con.lote,
                                                        cap_1.fecha_auto,
                                                        usuario,
                                                        cap_1.condicion,
                                                        0,
                                                        cap_1.tip_solicitud,
                                                        "A",
                                                        TODAY
                                                       )

                     ELSE
                          SELECT h.no_solic, h.fecha_captura, h.cerrada
                                 INTO registros, fecha_captura, cerrada
                                 FROM afi_ctr_lote h
                                 WHERE h.lote    = cap_con.lote
                                   AND h.fecha   = cap_1.fecha_auto
                                   AND h.usuario = usuario
                                   AND h.estado  = cap_1.condicion
                                   AND h.tipo_solicitud = cap_1.tip_solicitud
                                   AND h.cerrada        = "A"
                          IF ((fecha_captura != TODAY)  AND
                             (cerrada        = "A"))    THEN

                             UPDATE afi_ctr_lote SET cerrada = "C"
                                    WHERE lote = cap_con.lote

                             SET LOCK MODE TO WAIT

                             SELECT MAX(lote+1)
                                    INTO cap_con.lote
                                    FROM afi_ctr_lote


                             INSERT INTO afi_ctr_lote 
                                         VALUES(cap_con.lote,
                                                cap_1.fecha_auto,
                                                usuario,
                                                cap_1.condicion,
                                                0,
                                                cap_1.tip_solicitud,
                                                "A",
                                                TODAY
                                               )

                          END IF
                     END IF

                     DISPLAY cap_con.lote TO lote
                     LET salir = 1
                     EXIT INPUT

            ON KEY (CONTROL-C)
                LET salir = 3
                EXIT INPUT

            ON KEY (INTERRUPT)
                LET salir = 3
                EXIT INPUT
    END INPUT
    CASE salir 
        WHEN 1
                CALL llena_uno() RETURNING salir
                CASE salir
                   WHEN 1
                           PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR" 
                              FOR CHAR enter
#                            CLEAR FORM
                   WHEN 3
                           PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                              FOR CHAR enter
#                            CLEAR FORM
                END CASE
        WHEN 3
                PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                FOR CHAR enter
#               CLEAR FORM
    END CASE
    CLEAR FORM
END FUNCTION

############################################################################
FUNCTION llena_uno()
   DEFINE con_1,    con_2        INTEGER,
          con_1_a,  con_2_a      INTEGER,
          estado_11              SMALLINT

             LET con_1   = 0   LET con_2   = 0
             LET con_1_a = 0   LET con_2_a = 0

##  DISPLAY " < Ctrl-C >Salir  < RETURN >Acepta Docto.                  " AT 2,1
    DISPLAY " < RETURN >Acepta Docto.                                   " AT 2,1
    INPUT ARRAY cap_2  WITHOUT DEFAULTS FROM scr_1.*

          BEFORE FIELD folio
                 LET con_1 = ARR_CURR()
                 LET con_2 = SCR_LINE()
                 LET cap_2_2.* = cap_2[con_1].*
                 LET cap_2[con_1].solicitud = cap_1.tip_solicitud
                 DISPLAY cap_2[con_1].solicitud TO scr_1[con_2].solicitud

          AFTER FIELD folio
               IF cap_2[con_1].folio IS NULL  OR
                  cap_2[con_1].folio = 0 THEN
                  ERROR "  EL FOLIO NO PUEDE SER NULO o CERO" ATTRIBUTE(NORMAL)
                  NEXT FIELD folio
               ELSE
                      SELECT n_seguro 
                                INTO cap_2[con_1].nss
                                FROM  afi_recepcion
                               WHERE @n_folio   = cap_2[con_1].folio
                                  AND @tipo_solicitud = cap_2[con_1].solicitud
                      IF STATUS = NOTFOUND THEN
                          ERROR "  Registro Inexistente" ATTRIBUTE(NORMAL)
                          NEXT FIELD folio
                      END IF
                      
                      SELECT COUNT(*)
                             INTO son_folios
                             FROM afi_expediente
                      WHERE @n_folio = cap_2[con_1].folio
                        AND @lote    in (SELECT @lote
                                           FROM afi_ctr_lote
                                          WHERE @lote    = cap_con.lote 
                                            AND @cerrada = 'A') 
                      IF son_folios > 0 THEN
                         ERROR "Folio ya ingresado en el lote."
                         LET registros  = registros - 1
 
                         ### Modificacion ###
                         SELECT COUNT(unique n_folio)
                           INTO vregistros
                           FROM afi_expediente
                          WHERE @lote = cap_con.lote
                         IF vregistros <> registros THEN  
                            LET registros = vregistros

                            ### Modificacion ###
                            UPDATE afi_ctr_lote SET no_solic = registros
                             WHERE @lote = cap_con.lote
                            ### Modificacion ###

                          END IF 

                         LET son_folios = 0
                         DISPLAY registros TO registros
                         NEXT FIELD folio
                      END IF
                      
                      NEXT FIELD nss
               END IF

          BEFORE FIELD nss
                     DISPLAY cap_2[con_1].nss TO scr_1[con_2].nss
                     LET cap_2_2.* = cap_2[con_1].*
                     SELECT COUNT(unique n_folio)
                       INTO vregistros
                       FROM afi_expediente
                      WHERE @lote = cap_con.lote
                     IF vregistros <> registros THEN  
                        LET registros = vregistros

                        ### Modificacion ###
                        UPDATE afi_ctr_lote SET no_solic = registros
                         WHERE @lote = cap_con.lote
                        ### Modificacion ###

                     END IF 

                     LET registros = registros + 1
                     DISPLAY registros TO registros
                     CALL llena_dos_promotor()
                     CALL llena_dos() RETURNING salida, cap_2[con_1].estado_sol
                     CASE salida
                           WHEN 1
                                    LET salir = 1
                                    IF cap_2[con_1].estado_sol = 1 THEN
                                       LET cap_2[con_1].estado_sol = "N"
                                    ELSE
                                       LET cap_2[con_1].estado_sol = "S"
                                    END IF
                                    DISPLAY cap_2[con_1].estado_sol
                                                     TO scr_1[con_2].estado_sol
                           WHEN 3
                                    LET salir = 3
                                    EXIT INPUT
                     END CASE

                     LET rechaza_rechazado = 0
                     LET primera_vez = 0          LET ss  = 0
                     FOR ss = 1 TO 100
                         INITIALIZE cap_5[ss].* TO NULL
                     END FOR
                     DISPLAY " < RETURN >Acepta Docto.  < ESC >Salir  < Ctrl-N >Cerrar Lote " AT 2,1


          ON KEY (ESC)
                SELECT COUNT(unique @n_folio)
                  INTO vregistros
                  FROM afi_expediente
                 WHERE @lote = cap_con.lote
                IF vregistros <> registros THEN  
                   LET registros = vregistros
                END IF 

                UPDATE afi_ctr_lote SET no_solic = registros
                 WHERE @lote = cap_con.lote
                   LET salir = 1
                  EXIT INPUT

          ON KEY (CONTROL-N)
                SELECT COUNT(unique @n_folio)
                  INTO vregistros
                  FROM afi_expediente
                 WHERE @lote = cap_con.lote
                IF vregistros <> registros THEN  
                   LET registros = vregistros
                END IF 

                UPDATE afi_ctr_lote SET no_solic = registros,
                                        cerrada  = "C"
                 WHERE @lote = cap_con.lote
                   LET salir = 1
                   EXIT INPUT

          ON KEY (CONTROL-C)
                SELECT COUNT(unique @n_folio)
                  INTO vregistros
                  FROM afi_expediente
                 WHERE @lote = cap_con.lote
                IF vregistros <> registros THEN  
                   LET registros = vregistros
                END IF 
                UPDATE afi_ctr_lote SET no_solic = registros,
                                        cerrada  = "C"
                 WHERE @lote = cap_con.lote
                   LET salir = 1
                   EXIT INPUT

    END INPUT
 
    RETURN salir

END FUNCTION

############################################################################
FUNCTION llena_dos_promotor()

    SELECT a.cod_promotor, a.fecha_recepcion, b.agenc_cod,
           b.paterno, b.materno, b.nombres
           INTO cap_6_6.*
           FROM afi_recepcion a, pro_mae_promotor b
           WHERE a.n_seguro = cap_2_2.nss
				 AND a.n_folio = cap_2_2.folio
				 AND a.tipo_solicitud = cap_1.tip_solicitud
             AND b.cod_promotor = a.cod_promotor

    IF STATUS != NOTFOUND THEN
       LET cap_6.cod_promotor = cap_6_6.cod_promotor
       LET cap_6.nombre       = cap_6_6.paterno CLIPPED," ",
                                         cap_6_6.materno CLIPPED," ",
                                         cap_6_6.nombres CLIPPED
       LET cap_6.sucursal     = cap_6_6.sucursal
       LET cap_6.fecha        = cap_6_6.fecha

    ELSE
      ERROR "  No existen datos del Promotor" ATTRIBUTE(NORMAL)
    END IF
    DISPLAY cap_6.cod_promotor TO cod_promotor
    DISPLAY cap_6.nombre       TO nombre
    DISPLAY cap_6.sucursal     TO sucursal
    DISPLAY cap_6.fecha        TO fecha
END FUNCTION

############################################################################
FUNCTION llena_dos()
   DEFINE llena_dos_1, llena_dos_2,
          cuantos,     cuantos_cap2,
          com_llena_dos,
          i, aa, n                    INTEGER,
          obligados                   CHAR(01),
          rechazados                  CHAR(01),
	  estado_edo                  SMALLINT

   DEFINE salto			      SMALLINT

   LET salto = 0 

   LET aa = 0
   FOR aa = 1 TO 100
         INITIALIZE cap_3[aa].* TO NULL
   END FOR
   LET cuantos_cap2 = 0
   LET aa = 0

  DISPLAY " < RETURN >Otro Docto.  < ESC >Confirma Informacion Doctos.                    " AT 2,1
  WHILE TRUE
    INPUT ARRAY cap_3  WITHOUT DEFAULTS FROM scr_2.*
          BEFORE FIELD consecutivo
              LET llena_dos_1 = ARR_CURR()
              LET llena_dos_2 = SCR_LINE()
              LET cap_3_3.* = cap_3[llena_dos_1].*
              IF aa = 0 THEN
                 LET com_llena_dos = llena_dos_1
                 LET aa = 1
              END IF

          AFTER FIELD consecutivo
                  LET salto = 0
                  IF cap_3[llena_dos_1].consecutivo IS NULL OR
                     cap_3[llena_dos_1].consecutivo =  " " OR
                     cap_3[llena_dos_1].consecutivo = 0     THEN
                     CALL ven_consecutivo() RETURNING 
                                            cap_3[llena_dos_1].consecutivo, 
                                            cap_3[llena_dos_1].descripcion

                     DISPLAY cap_3[llena_dos_1].consecutivo 
                             TO scr_2[llena_dos_2].consecutivo

                     DISPLAY cap_3[llena_dos_1].descripcion 
                             TO scr_2[llena_dos_2].descripcion

                     SELECT b.docto_obliga 
                            INTO cap_3[llena_dos_1].obliga
                            FROM  tab_ctr_doc b
                            WHERE b.docto_cod = cap_3[llena_dos_1].consecutivo
                 ELSE
                        SELECT b.docto_desc, b.docto_obliga 
                               INTO cap_3[llena_dos_1].descripcion,
                                    cap_3[llena_dos_1].obliga
                                 FROM  tab_ctr_doc b
                                 WHERE b.docto_cod = cap_3[llena_dos_1].consecutivo
                        IF STATUS = NOTFOUND THEN
                            ERROR "  Documento Inexistente" ATTRIBUTE(NORMAL)
                            NEXT FIELD consecutivo
                        ELSE
                              DISPLAY cap_3[llena_dos_1].descripcion 
                                     TO scr_2[llena_dos_2].descripcion

                        END IF
                 END IF

                  LET cap_3_3.* = cap_3[llena_dos_1].*
                  CALL llena_tres() 
                            RETURNING salir, cuantos, cap_3[llena_dos_1].estado

                  CASE salir
                      WHEN 1
                             LET salir = 1
                      WHEN 3
                               LET salir = 3
                               EXIT WHILE
                  END CASE

                  IF cap_3[llena_dos_1].obliga = "0" THEN
                       LET cap_3[llena_dos_1].obliga = "N"
                  ELSE
                       LET cap_3[llena_dos_1].obliga = "S"
                  END IF
                  DISPLAY cap_3[llena_dos_1].obliga TO scr_2[llena_dos_2].obliga

                  IF cap_3[llena_dos_1].estado = "0" THEN
                       LET cap_3[llena_dos_1].estado = "S"
                  ELSE
                       LET cap_3[llena_dos_1].estado = "N"
                         LET rechaza_rechazado = 1
                  END IF
                  DISPLAY cap_3[llena_dos_1].estado TO scr_2[llena_dos_2].estado
                  LET cuantos_cap2 = llena_dos_1 + 1

          BEFORE FIELD ubicacion
                 ERROR " <ENTER> para confirmar documento" 
                 ATTRIBUTE(REVERSE)

          AFTER FIELD ubicacion

                 ERROR " <ESC> Ingresar informacion siguiente folio o  <ENTER> Siguiente documento." 
                 ATTRIBUTE(REVERSE)
                  IF cap_3[llena_dos_1].ubicacion IS NULL OR
                     cap_3[llena_dos_1].ubicacion =  " " THEN
                     LET cap_3[llena_dos_1].ubicacion = 0 
                 END IF

                 LET son_exp = 0
                 SELECT COUNT(*)
                        INTO son_exp
                        FROM afi_expediente
                        WHERE lote           = cap_con.lote
                          AND n_folio        = cap_2_2.folio
                          AND tipo_solicitud = cap_1.tip_solicitud
                          AND docto_cod      = cap_3[llena_dos_1].consecutivo
                 IF son_exp = 0 THEN
                       IF cap_3[llena_dos_1].estado = "N" THEN
                           LET estado_edo = 1
                       ELSE
                           LET estado_edo = 0
                       END IF
                       INSERT INTO afi_expediente
                              VALUES(cap_2_2.folio,
                                       cap_1.tip_solicitud,
                                       cap_2_2.nss,
                                       cap_3[llena_dos_1].consecutivo,
                                       cap_3[llena_dos_1].ubicacion,
                                       usuario,
                                       cap_1.fecha_auto,
                                       estado_edo,
                                       cap_con.lote,
                                       TODAY
                                    )
                END IF
                IF fgl_lastkey() = fgl_keyval("ACCEPT")      OR 
                   fgl_lastkey() = fgl_keyval("CONTROL - M") OR
                   fgl_lastkey() = fgl_keyval("RETURN")      OR
                   fgl_lastkey() = fgl_keyval("DOWN")        THEN
                   ERROR " <ESC> Ingresar informacion siguiente folio o  <ENTER> Siguiente documento." 
                   ATTRIBUTE(REVERSE)
                   LET salto = 1
                ELSE
                   ERROR " <ENTER> para confirmar documento" 
                   ATTRIBUTE(REVERSE)
                   LET salto = 0
                END IF

                ON KEY ( ESC )
                   IF salto = 1 THEN
                     IF cap_3[llena_dos_1].consecutivo IS NULL OR
                        cap_3[llena_dos_1].consecutivo =  0    THEN
                        IF llena_dos_1 = 1 THEN
                           ERROR "  El Documento no puede ser en nulo "
                           NEXT FIELD consecutivo
                        END IF
                     END IF

                     IF cap_3[llena_dos_1].ubicacion IS NULL OR
                        cap_3[llena_dos_1].ubicacion =  " " THEN
                        LET cap_3[llena_dos_1].ubicacion = 0 
                     END IF

                     IF rechaza_rechazado = 1 THEN
                        UPDATE afi_recepcion
                           SET estado_exp     = 1
                         WHERE n_folio        = cap_2_2.folio
                           AND tipo_solicitud = cap_2_2.solicitud
                     ELSE
                        UPDATE afi_recepcion
                           SET estado_exp     = 0
                         WHERE n_folio        = cap_2_2.folio
                           AND tipo_solicitud = cap_2_2.solicitud
                     END IF
                     LET salir = 1
                     EXIT WHILE
                   ELSE
                     LET salto = 0
                     ERROR " <ENTER> para confirmar documento" 
                     ATTRIBUTE(REVERSE)
                   END IF 

    END INPUT
  END WHILE
  RETURN salir, rechaza_rechazado
END FUNCTION

############################################################################
FUNCTION llena_tres()
   DEFINE ven_1    ,
             ven_2    ,
             aa, aaa    INTEGER,
             rechazo_1, 
             sw         SMALLINT

    LET aa = 0
    FOR aa = 1 TO 100
         INITIALIZE cap_4[aa].* TO NULL
    END FOR

    OPEN WINDOW ventana_tres AT  8,33 WITH FORM "AFIM0083" ATTRIBUTE(BORDER)
    DISPLAY " < ESC > Acepta Datos " AT 1,14

    LET rechazo_1 = 0
    INPUT ARRAY cap_4  WITHOUT DEFAULTS FROM scr_docto.*
          BEFORE FIELD consecutivo
              LET ven_1 = ARR_CURR()
              LET ven_2 = SCR_LINE()
              LET cap_4[ven_1].consecutivo = cap_3_3.consecutivo
              DISPLAY cap_4[ven_1].consecutivo TO scr_docto[ven_2].consecutivo
              NEXT FIELD codigo

          AFTER FIELD codigo
                  IF cap_4[ven_1].codigo IS NULL OR
                     cap_4[ven_1].codigo = 0     OR
                     cap_4[ven_1].codigo = " "   THEN
                     CALL ven_codigo() RETURNING cap_4[ven_1].codigo, 
                                                 cap_4[ven_1].desc_codigo,
                                                 cap_4[ven_1].rechazo
                     DISPLAY cap_4[ven_1].codigo 
                                 TO scr_docto[ven_2].codigo
                     DISPLAY cap_4[ven_1].desc_codigo 
                                 TO scr_docto[ven_2].descripcion
                     DISPLAY cap_4[ven_1].rechazo 
                                 TO scr_docto[ven_2].rechazo
                     IF cap_4[ven_1].rechazo = 1 THEN
                         LET rechazo_1 = 1
                     END IF

                 ELSE
                      SELECT f.cond_desc, f.cond_rechazo 
                                INTO cap_4[ven_1].desc_codigo,
                                     cap_4[ven_1].rechazo
                                FROM   afi_condicion_doc f
                                WHERE  f.docto_cod = cap_4[ven_1].consecutivo
                                  AND  f.cond_cod   = cap_4[ven_1].codigo
                      IF STATUS = NOTFOUND THEN
                          ERROR "  Registro Inexistente" ATTRIBUTE(NORMAL)
                          NEXT FIELD codigo
                      ELSE
                        DISPLAY cap_4[ven_1].desc_codigo 
                                     TO scr_docto[ven_2].descripcion
                        DISPLAY cap_4[ven_1].rechazo 
                                     TO scr_docto[ven_2].rechazo
                        IF cap_4[ven_1].rechazo = 1 THEN
                            LET rechazo_1 = 1
                        END IF
                      END IF
                 END IF

                 LET son_cond = 0
                 SELECT COUNT(*)
                     INTO son_cond
                     FROM afi_condicion_exp
                    WHERE lote           = cap_con.lote
                      AND n_folio        = cap_2_2.folio
                      AND tipo_solicitud = cap_1.tip_solicitud
                      AND docto_cod      = cap_4[ven_1].consecutivo
                      AND cond_cod       = cap_4[ven_1].codigo
                 IF son_cond = 0 THEN
                        INSERT INTO afi_condicion_exp
                                  VALUES(cap_2_2.folio,
                                         cap_1.tip_solicitud,
                                         cap_4[ven_1].consecutivo,
                                         cap_4[ven_1].codigo,
                                         "",
                                         cap_con.lote
                                          )
                 END IF

         ON KEY (ESC) 
                IF cap_4[ven_1].codigo IS NULL OR
                   cap_4[ven_1].codigo =  0    THEN
                    IF ven_1 = 1 THEN
                       ERROR "  El Codigo no puede ser en nulo "
                       NEXT FIELD codigo
                    END IF

                END IF
                LET salir = 1
                EXIT INPUT
                

    END INPUT
    CLOSE WINDOW ventana_tres
    RETURN salir, fueron, rechazo_1
      
END FUNCTION

############################################################################
FUNCTION llena_reg()
     DECLARE apt_reg_mod CURSOR FOR                
          SELECT UNIQUE n_folio FROM afi_expediente
               WHERE lote = cap_con.lote         
     FOREACH apt_reg_mod INTO la_fol.*             
          LET registros = registros + 1          
     END FOREACH                                   
     UPDATE afi_ctr_lote SET no_solic = registros  
          WHERE lote = cap_con.lote              
END FUNCTION

############################################################################
FUNCTION ven_solicitud()

   DEFINE arr_solicitud ARRAY[50] OF RECORD
              numero      SMALLINT,
              descri      CHAR(30)
              END RECORD,

              arr_ret,
              cont      SMALLINT

  DECLARE apt_cur CURSOR FOR
        SELECT a.tipo_solicitud, a.desc_solicitud 
                    FROM tab_tipo_solic a
        ORDER BY 1
  LET cont=1
  FOREACH apt_cur INTO arr_solicitud[cont].*
          LET cont = cont + 1
  END FOREACH

        
    OPEN WINDOW ventana_solicitud AT 8,20 WITH FORM "AFIM0082" ATTRIBUTE(BORDER)
       DISPLAY "TIPOS DE SOLICITUD" AT 1,14

       CALL SET_COUNT(cont-1)
       DISPLAY ARRAY arr_solicitud TO scr_solicitud.*
             ON KEY ( RETURN )
                   LET cont = ARR_CURR()
                   EXIT DISPLAY

             ON KEY ( INTERRUPT )
                   EXIT DISPLAY
       END DISPLAY
    CLOSE WINDOW ventana_solicitud
    RETURN arr_solicitud[cont].numero, arr_solicitud[cont].descri 

END FUNCTION

############################################################################
FUNCTION ven_consecutivo()

   DEFINE arr_consecutivo ARRAY[100] OF RECORD
              numero      SMALLINT,
              descri      CHAR(25)
              END RECORD,

              arr_ret,
              zz          SMALLINT

  DECLARE apt_cur2 CURSOR FOR
        SELECT a.docto_cod, a.docto_desc 
                    FROM tab_ctr_doc a
        ORDER BY 1
  LET zz=1
  FOREACH apt_cur2 INTO arr_consecutivo[zz].*
          LET zz = zz + 1
  END FOREACH

        
    OPEN WINDOW ventana_consecutivo AT 8,20 WITH FORM "AFIM0084" ATTRIBUTE(BORDER)
       DISPLAY "TIPOS DE DOCUMENTOS" AT 1,13

       CALL SET_COUNT(zz-1)
       DISPLAY ARRAY arr_consecutivo TO scr_consecutivo.*
             ON KEY ( RETURN )
                   LET zz = ARR_CURR()
                   EXIT DISPLAY

             ON KEY ( INTERRUPT )
                   EXIT DISPLAY
       END DISPLAY
    CLOSE WINDOW ventana_consecutivo
    RETURN arr_consecutivo[zz].numero, arr_consecutivo[zz].descri 

END FUNCTION

############################################################################
FUNCTION ven_codigo()

   DEFINE arr_codigo ARRAY[100] OF RECORD
              codigo      SMALLINT,
              descri      CHAR(25),
                  rechazo     SMALLINT
              END RECORD,

              arr_ret,
              mm          SMALLINT

  DECLARE apt_cur3 CURSOR FOR
          SELECT s.cond_cod, s.cond_desc, s.cond_rechazo                  
                 FROM   afi_condicion_doc s                   
                        WHERE  s.docto_cod = cap_3_3.consecutivo
          ORDER BY 1, 2
  LET mm=1
  FOREACH apt_cur3 INTO arr_codigo[mm].*
          LET mm = mm + 1
  END FOREACH

        
    OPEN WINDOW ventana_codigo AT 8,20 WITH FORM "AFIM0085" ATTRIBUTE(BORDER)
       DISPLAY "TIPOS DE CONDICIONES" AT 1,13

       CALL SET_COUNT(mm-1)
       DISPLAY ARRAY arr_codigo TO scr_codigo.*
             ON KEY ( RETURN )
                   LET mm = ARR_CURR()
                   EXIT DISPLAY

             ON KEY ( INTERRUPT )
                   EXIT DISPLAY
       END DISPLAY
    CLOSE WINDOW ventana_codigo
    RETURN arr_codigo[mm].codigo, arr_codigo[mm].descri, arr_codigo[mm].rechazo

END FUNCTION


############################################################################

FUNCTION consulta()
#c-----------------
    DEFINE u_nnnnno    SMALLINT

    OPTIONS
          ACCEPT KEY ESC,
          INPUT WRAP

    CALL init()
    OPEN WINDOW ventana_2 AT 3,5 WITH FORM "AFIM0086" ATTRIBUTE( BORDER)
    DISPLAY "                         OPCIONES DE CONSULTA                                  " AT 2,1 ATTRIBUTE(REVERSE) 
    DISPLAY "[ ESC ] Grabar   [ Ctrl-C ] Salir" AT 1,1 ATTRIBUTES(BOLD)
    LET pos = 2
    IF  (pos-1) >= 1 THEN     
        CALL SET_COUNT(pos-1) 
        LET int_flag = FALSE
        CONSTRUCT BY NAME cla_where ON b.n_seguro, b.n_folio, a.lote, 
                                       a.tipo_solicitud, a.fecha,
			               a.usuario, c.estado_exp
            AFTER FIELD estado_exp
              LET vestado_exp = get_fldbuf(estado_exp)
              IF vestado_exp < 0 OR
		 vestado_exp > 1 THEN
                 ERROR "Valor de datos solo < 0 = S >  o   < 1 = N >"
                 NEXT FIELD estado_exp
              ELSE
                 CONTINUE CONSTRUCT 
              END IF 
        END CONSTRUCT

        IF  int_flag = TRUE THEN          
            LET int_flag = FALSE          
            ERROR "BUSQUEDA CANCELADA..." 
            SLEEP 2                       
            ERROR ""                      
            CLOSE WINDOW ventana_2        
	    RETURN 
        END IF
        CALL ordena() #o
	CLEAR FORM

        LET z_reg2[1] = z_reg.orden_1
        LET z_reg2[2] = z_reg.orden_2
        LET z_reg2[3] = z_reg.orden_3
        LET z_reg2[4] = z_reg.orden_4
        LET z_reg2[5] = z_reg.orden_5
        LET z_reg2[6] = z_reg.orden_6
	LET z_reg2[7] = z_reg.orden_7
        
        LET sel_where = " SELECT unique b.n_seguro, b.n_folio, a.lote,", 
			" a.tipo_solicitud, a.fecha, ", 
                        " a.usuario, c.estado_exp",
                        " FROM afi_ctr_lote a, afi_expediente b,",
                        " afi_recepcion c",
			" WHERE ", cla_where CLIPPED ,
                        " AND a.lote = b.lote",
                        #" AND a.usuario = '", usuario, "'",
                        " AND c.n_folio = b.n_folio",
                        " ORDER BY ", z_reg.orden_1,",",z_reg.orden_2,",",
                                      z_reg.orden_3,",",z_reg.orden_4,",",
                                      z_reg.orden_5,",",z_reg.orden_6,",",
				      z_reg.orden_7

        LET sel_where = sel_where CLIPPED
        PREPARE qry_modif FROM sel_where 
        DECLARE cursor_m CURSOR FOR qry_modif
        LET pos = 1
        FOREACH cursor_m INTO reg[pos].n_seguro,
			      reg[pos].n_folio, 
			      reg[pos].lote,
			      reg[pos].tipo_solicitud,
			      reg[pos].fecha,
			      reg[pos].usuario,
			      reg[pos].docto_obliga
            IF reg[pos].docto_obliga = 0 THEN
               LET reg[pos].docto_obliga = 'S'
            ELSE
               LET reg[pos].docto_obliga = 'N'
            END IF
            LET pos = pos + 1                 
        END FOREACH                          
        CLOSE WINDOW ventana_2              
        INITIALIZE reg[pos].* TO NULL   

        IF  (pos-1) >= 1 THEN                 
            CALL  SET_COUNT(pos-1)            

            ERROR ""                     
            IF registros = 0 THEN
               CALL llena_reg()
            END IF
            OPEN WINDOW ventana_10 AT 3,2 WITH FORM "AFIM0088"
               ATTRIBUTE( BORDER)
               DISPLAY "" AT 1,1
               DISPLAY "" AT 2,1
               DISPLAY "[ Ctrl-C ] Salir      [ ENTER ] Selecc. " AT 2,1 
               ATTRIBUTE(BOLD)
               DISPLAY " CONSULTA " AT 1,65 ATTRIBUTE(REVERSE,BOLD)
               DISPLAY ARRAY reg TO scr_1.*
                  ON KEY ( INTERRUPT )
                     EXIT DISPLAY

                  ON KEY ( CONTROL - M )
                     LET pos = ARR_CURR()
                     LET cap_con.lote        = reg[pos].lote
                     LET cap_1.fecha_auto    = reg[pos].fecha
                     LET cap_1.tip_solicitud = reg[pos].tipo_solicitud
                     LET cap_1.operador      = reg[pos].usuario
                     LET vdocto_obliga       = reg[pos].docto_obliga
                     LET vn_seguro	     = reg[pos].n_seguro
                     LET vn_folio	     = reg[pos].n_folio
                     LET vdocto_obliga       = reg[pos].docto_obliga
                     EXIT DISPLAY
               END DISPLAY 
          CLOSE WINDOW ventana_10

          DISPLAY cap_con.lote        TO lote
          DISPLAY cap_1.condicion     TO condicion
          DISPLAY cap_1.fecha_auto    TO fecha_auto
          DISPLAY cap_1.tip_solicitud TO tip_solicitud
          DISPLAY cap_1.operador      TO operador
          DISPLAY vdocto_obliga       TO docto_obliga

          IF cap_con.lote IS NULL  OR
             cap_con.lote = 0      THEN
             ERROR "  El Lote No puede ser nulo o cero" ATTRIBUTE(NORMAL)
             LET salir = 3 
          ELSE
             LET salir = 1
          END IF

          CASE salir 
             WHEN 1
                CALL llena_con_uno() RETURNING salir
                CASE salir
                   WHEN 1
                      PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR" 
                      FOR CHAR enter
                      CLEAR FORM
                   WHEN 3
                      PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                      FOR CHAR enter
                      CLEAR FORM
                END CASE
             WHEN 3
                PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                FOR CHAR enter
          END CASE
        ELSE                                   
            ERROR "ARCHIVO DE RECHAZOS... VACIO"
            SLEEP 2                        
            ERROR ""                            
	    RETURN
        END IF                                 
    END IF

END FUNCTION

############################################################################
{
FUNCTION consulta()
#c-----------------

    CALL init()
    DISPLAY "" AT 1,1
    DISPLAY "" AT 2,1
    DISPLAY " CONSULTAR " AT 1,68 ATTRIBUTE ( REVERSE )
    DISPLAY " < Ctrl-C >Salir  < ESC >Aceptar                           " AT 2,1

    INPUT cap_con.lote FROM FORMONLY.lote

          AFTER FIELD lote
                IF cap_con.lote IS NULL  OR
                   cap_con.lote = 0      THEN
                   ERROR "  El Lote No puede ser nulo o cero" ATTRIBUTE(NORMAL)
                   NEXT FIELD lote
                END IF

                SELECT d.fecha, d.usuario, d.estado, d.no_solic, d.tipo_solicitud
                       INTO cap_1.fecha_auto, cap_1.operador, 
                            cap_1.condicion,  registros,  
                            cap_1.tip_solicitud
                       FROM afi_ctr_lote d
                       WHERE d.lote = cap_con.lote
                IF STATUS = NOTFOUND THEN
                    ERROR "  Lote Inexistente" ATTRIBUTE(NORMAL)
                    NEXT FIELD lote
                END IF
                IF cap_1.operador != usuario THEN
                   ERROR "  El operador ",usuario," no tiene acceso a este Lote"
                   ATTRIBUTE(NORMAL)
                   NEXT FIELD lote
                END IF
                IF registros = 0 THEN
                   CALL llena_reg()
                END IF
                DISPLAY cap_1.condicion TO condicion
                DISPLAY cap_1.fecha_auto TO fecha_auto
                DISPLAY cap_1.tip_solicitud TO tip_solicitud
                DISPLAY cap_1.operador TO operador
                DISPLAY registros TO registros



            ON KEY (ESC)
                IF cap_con.lote IS NULL  OR
                   cap_con.lote = 0      THEN
                   ERROR "  El Lote No puede ser nulo o cero" ATTRIBUTE(NORMAL)
                   NEXT FIELD lote
                END IF
                LET salir = 1
                EXIT INPUT

            ON KEY (CONTROL-C)
                LET salir = 3
                EXIT INPUT

            ON KEY (INTERRUPT)
                LET salir = 3
                EXIT INPUT
    END INPUT
    CASE salir 
        WHEN 1
           CALL llena_con_uno() RETURNING salir
           CASE salir
              WHEN 1
                      PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR" 
                      FOR CHAR enter
                      CLEAR FORM
              WHEN 3
                      PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                      FOR CHAR enter
                      CLEAR FORM
           END CASE
        WHEN 3
                PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                FOR CHAR enter
    END CASE
END FUNCTION
}

############################################################################
FUNCTION ordena()
#o--------------
    OPEN WINDOW ventana_3 AT 3,5 WITH FORM "AFIM0087" ATTRIBUTE(BORDER)
    DISPLAY " [ Esc ] Grabar      [ Ctrl-C ] Salir"
             AT 1,1 ATTRIBUTE(BOLD) 
    DISPLAY "                      OPCIONES DE ORDENAMIENTO                                        " AT 2,1 ATTRIBUTE(REVERSE,BOLD)
    LET z_reg.orden_1 = 1
    LET z_reg.orden_2 = 2
    LET z_reg.orden_3 = 3
    LET z_reg.orden_4 = 4
    LET z_reg.orden_5 = 5
    LET z_reg.orden_6 = 6
    LET z_reg.orden_7 = 7
    DISPLAY BY NAME z_reg.*
    INPUT BY NAME z_reg.* WITHOUT DEFAULTS
        AFTER FIELD orden_1
            IF  z_reg.orden_1 IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD orden_1
            ELSE
                IF  z_reg.orden_1 < 1 OR z_reg.orden_1 > 7 THEN
                    ERROR "La opcion de orden digitada no existe...Reingrese"
                    NEXT FIELD orden_1
                END IF
            END IF
        AFTER FIELD orden_2
            IF  z_reg.orden_2 IS NULL THEN
                ERROR "Campo NO puede ser NULO"
                NEXT FIELD orden_2
            ELSE
               IF  z_reg.orden_2 < 1 OR z_reg.orden_2 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_2
               END IF
               IF  z_reg.orden_2 = z_reg.orden_1 THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_2
               END IF
           END IF
       AFTER FIELD orden_3
           IF  z_reg.orden_3 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_3
           ELSE
               IF  z_reg.orden_3 < 1 OR z_reg.orden_3 > 7 THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_3
               END IF
               IF  (z_reg.orden_3 = z_reg.orden_1)
                   OR (z_reg.orden_3 = z_reg.orden_2) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_3
               END IF
           END IF
       AFTER FIELD orden_4
           IF  z_reg.orden_4 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_4
           ELSE
               IF  z_reg.orden_4 < 1 OR z_reg.orden_4 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_4
               END IF
               IF  (z_reg.orden_4 = z_reg.orden_1)
                   OR (z_reg.orden_4 = z_reg.orden_2)
                   OR (z_reg.orden_4 = z_reg.orden_3) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_4
               END IF
           END IF
       AFTER FIELD orden_5
           IF  z_reg.orden_5 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_5
           ELSE
               IF  z_reg.orden_5 < 1 OR z_reg.orden_5 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_5
               END IF
               IF  (z_reg.orden_5 = z_reg.orden_1)
                   OR (z_reg.orden_5 = z_reg.orden_2)
                   OR (z_reg.orden_5 = z_reg.orden_3) 
                   OR (z_reg.orden_5 = z_reg.orden_4) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_5
               END IF
           END IF

       AFTER FIELD orden_6
           IF  z_reg.orden_6 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_6
           ELSE
               IF  z_reg.orden_6 < 1 OR z_reg.orden_6 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_6
               END IF
               IF  (z_reg.orden_6 = z_reg.orden_1)
                   OR (z_reg.orden_6 = z_reg.orden_2)
                   OR (z_reg.orden_6 = z_reg.orden_3) 
                   OR (z_reg.orden_6 = z_reg.orden_4) 
                   OR (z_reg.orden_6 = z_reg.orden_5) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_6
               END IF
           END IF

       AFTER FIELD orden_7
           IF  z_reg.orden_7 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_7
           ELSE
               IF  z_reg.orden_7 < 1 OR z_reg.orden_7 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_7
               END IF
               IF  (z_reg.orden_7 = z_reg.orden_1)
                   OR (z_reg.orden_7 = z_reg.orden_2)
                   OR (z_reg.orden_7 = z_reg.orden_3) 
                   OR (z_reg.orden_7 = z_reg.orden_4) 
                   OR (z_reg.orden_7 = z_reg.orden_5) 
                   OR (z_reg.orden_7 = z_reg.orden_6) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_7
               END IF
           END IF

       ON KEY ( ESC )
           IF  z_reg.orden_1 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_1
           ELSE
               IF  z_reg.orden_1 < 1 OR z_reg.orden_1 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_1
               END IF
           END IF
           IF  z_reg.orden_2 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_2
           ELSE
               IF  z_reg.orden_2 < 1 OR z_reg.orden_2 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_2
               END IF
               IF  z_reg.orden_2 = z_reg.orden_1 THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_2
               END IF
           END IF
           IF  z_reg.orden_3 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_3
           ELSE
               IF  z_reg.orden_3 < 1 OR z_reg.orden_3 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_3
               END IF
               IF  (z_reg.orden_3 = z_reg.orden_1)
                   OR (z_reg.orden_3 = z_reg.orden_2) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_3
               END IF
           END IF
           IF  z_reg.orden_4 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_4
           ELSE
               IF  z_reg.orden_4 < 1 OR z_reg.orden_4 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_4
               END IF
               IF  (z_reg.orden_4 = z_reg.orden_1)
                   OR (z_reg.orden_4 = z_reg.orden_2)
                   OR (z_reg.orden_4 = z_reg.orden_3) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_4
               END IF
           END IF
           IF  z_reg.orden_5 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_5
           ELSE
               IF  z_reg.orden_5 < 1 OR z_reg.orden_5 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_5
               END IF
               IF  (z_reg.orden_5 = z_reg.orden_1)
                   OR (z_reg.orden_5 = z_reg.orden_2)
                   OR (z_reg.orden_5 = z_reg.orden_3) 
                   OR (z_reg.orden_5 = z_reg.orden_4) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_5
               END IF
           END IF

           IF  z_reg.orden_6 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_6
           ELSE
               IF  z_reg.orden_6 < 1 OR z_reg.orden_6 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_6
               END IF
               IF  (z_reg.orden_6 = z_reg.orden_1)
                   OR (z_reg.orden_6 = z_reg.orden_2)
                   OR (z_reg.orden_6 = z_reg.orden_3) 
                   OR (z_reg.orden_6 = z_reg.orden_4) 
                   OR (z_reg.orden_6 = z_reg.orden_5) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_6
               END IF
           END IF

           IF  z_reg.orden_7 IS NULL THEN
               ERROR "Campo NO puede ser NULO"
               NEXT FIELD orden_7
           ELSE
               IF  z_reg.orden_7 < 1 OR z_reg.orden_7 > 7  THEN
                   ERROR "La opcion de orden digitada no existe...Reingrese"
                   NEXT FIELD orden_7
               END IF
               IF  (z_reg.orden_7 = z_reg.orden_1)
                   OR (z_reg.orden_7 = z_reg.orden_2)
                   OR (z_reg.orden_7 = z_reg.orden_3) 
                   OR (z_reg.orden_7 = z_reg.orden_4) 
                   OR (z_reg.orden_7 = z_reg.orden_5) 
                   OR (z_reg.orden_7 = z_reg.orden_6) THEN
                   ERROR "Opcion ya digitada ...Reintente "
                   NEXT FIELD orden_7
               END IF
           END IF

       ON KEY ( INTERRUPT )
           LET sw_4 = 0
           EXIT INPUT
    END INPUT
    CLOSE WINDOW ventana_3
    RETURN
END FUNCTION
############################################################################

FUNCTION llena_con_uno()

     DEFINE  xx           ,
             ven_con_1_1  ,
             ven_con_1_2    INTEGER

     DISPLAY " < Ctrl-C >Salir  < ESC >Acepta Doctos.                    " 
      AT 2,1

     LET ven_con_1_1 = 1
     DECLARE apt_con_1 CURSOR FOR
            SELECT UNIQUE tipo_solicitud, n_folio, n_seguro
                    FROM  afi_expediente
                    WHERE @lote     = cap_con.lote
                      AND @n_seguro = vn_seguro
 		      AND @n_folio  = vn_folio

      FOREACH apt_con_1 INTO cap_con_2[ven_con_1_1].solicitud,
                             cap_con_2[ven_con_1_1].folio,
                             cap_con_2[ven_con_1_1].nss
             LET cap_2[ven_con_1_1].solicitud = cap_con_2[ven_con_1_1].solicitud
             LET cap_2[ven_con_1_1].folio = cap_con_2[ven_con_1_1].folio
             LET cap_2[ven_con_1_1].nss = cap_con_2[ven_con_1_1].nss

             SELECT m.estado_exp INTO cap_con_2[ven_con_1_1].estado_sol
                    FROM afi_recepcion m
                    WHERE m.n_folio = cap_con_2[ven_con_1_1].folio

             IF cap_con_2[ven_con_1_1].estado_sol = 0 THEN
                 LET cap_2[ven_con_1_1].estado_sol = "S"
             ELSE
                 LET cap_2[ven_con_1_1].estado_sol = "N"
             END IF

             LET ven_con_1_1 = ven_con_1_1 + 1
      END FOREACH
      IF  (ven_con_1_1-1) <= 0 THEN
           ERROR "  No existen Registros para este Lote" ATTRIBUTE(NORMAL)
           RETURN 1
      END IF

     CALL SET_COUNT(ven_con_1_1-1)
     DISPLAY ARRAY cap_2 TO scr_1.*

         ON KEY (ESC)
             LET ven_con_1_1 = ARR_CURR()
             LET ven_con_1_2 = SCR_LINE()
             LET cap_2_2.* = cap_2[ven_con_1_1].*
             CALL llena_dos_promotor()
             CALL llena_con_dos() RETURNING salir
             CASE salir
                WHEN 3
                        LET salir = 3
                        EXIT DISPLAY
             END CASE
             DISPLAY " < Ctrl-C >Salir  < ESC >Acepta Doctos.                    " 
              AT 2,1
            
         ON KEY (INTERRUPT)
                LET salir = 3
                EXIT DISPLAY

         ON KEY (CONTROL-C)
                LET salir = 3
                EXIT DISPLAY

      END DISPLAY
      RETURN salir

END FUNCTION

############################################################################
FUNCTION llena_con_dos()
     DEFINE jj            ,
            ven_con_2_1   ,
            ven_con_2_2     INTEGER  

     DISPLAY " < Ctrl-C >Salir  < ESC >T.Condicion  < Ctrl-V >Confirmar Informacion Doctos."
      AT 2,1

     LET ven_con_2_1 = 1
     DECLARE apt_con_2 CURSOR FOR
            SELECT a.docto_cod,  b.docto_desc,  b.docto_obliga, 
                   a.estado_exp, a.ubicacion 
                FROM  afi_expediente a, tab_ctr_doc b 
                WHERE a.lote    =  cap_con.lote
                  AND a.n_folio =  cap_2_2.folio
                  AND a.tipo_solicitud = cap_2_2.solicitud
                  AND b.docto_cod = a.docto_cod 

      FOREACH apt_con_2 INTO cap_con_3[ven_con_2_1].*
         LET cap_3[ven_con_2_1].consecutivo = cap_con_3[ven_con_2_1].consecutivo
         LET cap_3[ven_con_2_1].descripcion = cap_con_3[ven_con_2_1].descripcion

         IF cap_con_3[ven_con_2_1].obliga = 0 THEN
               LET cap_3[ven_con_2_1].obliga = "N"
         ELSE
               LET cap_3[ven_con_2_1].obliga = "S"
         END IF

         IF cap_con_3[ven_con_2_1].estado = 0 THEN
             LET cap_3[ven_con_2_1].estado = "S"
         ELSE
             LET cap_3[ven_con_2_1].estado = "N"
         END IF
         LET cap_3[ven_con_2_1].ubicacion = cap_con_3[ven_con_2_1].ubicacion

         LET ven_con_2_1 = ven_con_2_1 + 1
      END FOREACH

     CALL SET_COUNT(ven_con_2_1-1)
     DISPLAY ARRAY cap_3 TO scr_2.*

         ON KEY (ESC)
             LET ven_con_2_1 = ARR_CURR()
             LET ven_con_2_2 = SCR_LINE()
             LET cap_3_3.* = cap_3[ven_con_2_1].*
             CALL llena_con_tres() RETURNING salir
             CASE salir
                  WHEN 3  EXIT DISPLAY
             END CASE
            
         ON KEY (CONTROL-V)
             LET salir = 1
             EXIT DISPLAY

         ON KEY (INTERRUPT)
             LET salir = 3
             EXIT DISPLAY

         ON KEY (CONTROL-C)
             LET salir = 3
             EXIT DISPLAY
      END DISPLAY
      RETURN salir

END FUNCTION

############################################################################
FUNCTION llena_con_tres()
 
DEFINE mm            ,
       ven_con_3_1   ,
       ven_con_3_2     INTEGER

     LET mm = 0
     FOR mm = 1 TO 100
       INITIALIZE cap_4[mm].* TO NULL
     END FOR

     DECLARE apt_con_3 CURSOR FOR
         SELECT a.docto_cod, a.cond_cod, b.cond_desc,  b.cond_rechazo  
             FROM afi_condicion_exp a, afi_condicion_doc b
             WHERE a.lote           =  cap_con.lote
               AND a.n_folio        =  cap_2_2.folio
               AND a.tipo_solicitud =  cap_2_2.solicitud
               AND a.docto_cod      =  cap_3_3.consecutivo
               AND a.docto_cod      =  b.docto_cod
               AND a.cond_cod       =  b.cond_cod
         LET ven_con_3_1 = 1
     FOREACH apt_con_3 INTO cap_4[ven_con_3_1].*              
         LET ven_con_3_1 = ven_con_3_1 + 1
     END FOREACH

     IF ven_con_3_1 = 1 THEN
        ERROR "  No existen Condiciones para este Docto."
        ATTRIBUTE(NORMAL)
        LET salir = 1
        RETURN salir
     END IF

     OPEN WINDOW ven_tana_tres AT 8,33 WITH FORM "AFIM0083" ATTRIBUTE(BORDER)
     DISPLAY " < ESC > Regresa Docto" AT 1,13

          CALL SET_COUNT(ven_con_3_1-1)           
          DISPLAY ARRAY cap_4 TO scr_docto.*

                    ON KEY (ESC)
                       LET ven_con_3_1 = ARR_CURR()
                       LET ven_con_3_2 = SCR_LINE()
                       LET salir = 1
                       EXIT DISPLAY

                    ON KEY (INTERRUPT)
                       LET salir = 3
                       EXIT DISPLAY

                    ON KEY (CONTROL-C)
                       LET salir = 3
                       EXIT DISPLAY
          END DISPLAY

     CLOSE WINDOW ven_tana_tres
      RETURN salir

END FUNCTION

############################################################################
FUNCTION modifica()
#m-----------------
    DEFINE u_nnnnno    SMALLINT

    OPTIONS
          ACCEPT KEY CONTROL-I,
          INPUT WRAP

   CALL init()
   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " MODIFICAR " AT 1,68 ATTRIBUTE ( REVERSE )
   DISPLAY " < Ctrl-C >Salir  < ESC >Aceptar                           " AT 2,1

    INPUT cap_con.lote FROM FORMONLY.lote

          AFTER FIELD lote
                IF cap_con.lote IS NULL  OR
                   cap_con.lote = 0      THEN
                   ERROR "  El Lote No puede ser nulo o cero" ATTRIBUTE(NORMAL)
                   NEXT FIELD lote
                END IF

                SELECT d.fecha, d.usuario, d.estado, d.no_solic, d.tipo_solicitud
                       INTO cap_1.fecha_auto, cap_1.operador, 
                            cap_1.condicion,  registros,  
                            cap_1.tip_solicitud
                       FROM afi_ctr_lote d
                       WHERE d.lote = cap_con.lote
                IF STATUS = NOTFOUND THEN
                    ERROR "  Lote Inexistente" ATTRIBUTE(NORMAL)
                    NEXT FIELD lote
                END IF
                IF cap_1.operador != usuario THEN
                   ERROR "  El operador ",usuario," no tiene acceso a este Lote"
                   ATTRIBUTE(NORMAL)
                   NEXT FIELD lote
                END IF

                DISPLAY cap_1.condicion TO condicion
                DISPLAY cap_1.fecha_auto TO fecha_auto
                DISPLAY cap_1.tip_solicitud TO tip_solicitud
                DISPLAY cap_1.operador TO operador
                DISPLAY registros TO registros

            ON KEY (ESC)
                IF cap_con.lote IS NULL  OR
                   cap_con.lote = 0      THEN
                   ERROR "  El Lote No puede ser nulo o cero" ATTRIBUTE(NORMAL)
                   NEXT FIELD lote
                END IF
                LET salir = 1
                EXIT INPUT

            ON KEY (CONTROL-C)
                LET salir = 3
                EXIT INPUT

            ON KEY (INTERRUPT)
                LET salir = 3
                EXIT INPUT
    END INPUT
    CASE salir 
        WHEN 1
             CALL llena_mod_uno() RETURNING salir
             CASE salir
                WHEN 1
                       PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR" 
                       FOR CHAR enter
                       CLEAR FORM
                WHEN 3
                        PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                        FOR CHAR enter
                        CLEAR FORM
             END CASE
        WHEN 3
                PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                FOR CHAR enter
    END CASE
END FUNCTION

############################################################################
FUNCTION llena_mod_uno()

     DEFINE  xx           ,
             cambia1      ,
             ven_mod_1_1  ,
             ven_mod_1_2    INTEGER

     DISPLAY " < Ctrl-C >Salir  < ESC >Acepta Doctos.                    " 
     AT 2,1

     LET cambia1 = 0
     LET ven_mod_1_1 = 1
     DECLARE apt_mod_1 CURSOR FOR
            SELECT UNIQUE tipo_solicitud, n_folio, n_seguro
                    FROM  afi_expediente
                    WHERE lote = cap_con.lote

      FOREACH apt_mod_1 INTO cap_con_2[ven_mod_1_1].solicitud,
                             cap_con_2[ven_mod_1_1].folio,
                             cap_con_2[ven_mod_1_1].nss
             LET cap_2[ven_mod_1_1].solicitud = cap_con_2[ven_mod_1_1].solicitud
             LET cap_2[ven_mod_1_1].folio = cap_con_2[ven_mod_1_1].folio
             LET cap_2[ven_mod_1_1].nss = cap_con_2[ven_mod_1_1].nss

             SELECT m.estado_exp INTO cap_con_2[ven_mod_1_1].estado_sol
                    FROM afi_recepcion m
                    WHERE m.n_folio = cap_con_2[ven_mod_1_1].folio

             IF cap_con_2[ven_mod_1_1].estado_sol = 0 THEN
                 LET cap_2[ven_mod_1_1].estado_sol = "S"
             ELSE
                 LET cap_2[ven_mod_1_1].estado_sol = "N"
             END IF

             LET ven_mod_1_1 = ven_mod_1_1 + 1
      END FOREACH
      IF  (ven_mod_1_1-1) <= 0 THEN
           ERROR "  No existen Registros para este Lote" ATTRIBUTE(NORMAL)
           RETURN 1
      END IF

     CALL SET_COUNT(ven_mod_1_1-1)
     DISPLAY ARRAY cap_2 TO scr_1.*

         ON KEY (ESC)
             LET ven_mod_1_1 = ARR_CURR()
             LET ven_mod_1_2 = SCR_LINE()
             LET cap_2_2.* = cap_2[ven_mod_1_1].*
             CALL llena_dos_promotor()
             CALL llena_mod_dos() RETURNING salir, cambia1
             IF cambia1 = 1   AND 
                cap_2[ven_mod_1_1].estado_sol = "S" THEN
                     LET cap_2[ven_mod_1_1].estado_sol = "N"
                     DISPLAY "N"
                             TO scr_1[ven_mod_1_2].estado_sol
             END IF
             IF cambia1 = 0   AND 
                cap_2[ven_mod_1_1].estado_sol = "N" THEN
                     LET cap_2[ven_mod_1_1].estado_sol = "S"
                     DISPLAY "S"
                             TO scr_1[ven_mod_1_2].estado_sol
             END IF
             CASE salir
                     WHEN 3
                            LET salir = 3
                            EXIT DISPLAY
             END CASE
             DISPLAY " < Ctrl-C >Salir  < ESC >Acepta Doctos.                    " 
             AT 2,1
            
         ON KEY (INTERRUPT)
                LET salir = 3
                EXIT DISPLAY

         ON KEY (CONTROL-C)
                LET salir = 3
                EXIT DISPLAY

      END DISPLAY
      RETURN salir

END FUNCTION

############################################################################
FUNCTION llena_mod_dos()
     DEFINE jj            ,
            ven_mod_2_1   ,
            ven_mod_2_2     INTEGER, 
            estado_n      ,
				cambio          SMALLINT

     DISPLAY " < Ctrl-C >Salir  < ESC >T.Condicion  < Ctrl-V >Otro Folio " 
     AT 2,1

     LET cambio      = 0
     LET ven_mod_2_1 = 1
     DECLARE apt_mod_2 CURSOR FOR
            SELECT a.docto_cod,  b.docto_desc,  b.docto_obliga, 
                   a.estado_exp, a.ubicacion 
                FROM  afi_expediente a, tab_ctr_doc b 
                WHERE a.lote    =  cap_con.lote
                  AND a.n_folio =  cap_2_2.folio
                  AND a.tipo_solicitud = cap_2_2.solicitud
                  AND b.docto_cod = a.docto_cod 

      FOREACH apt_mod_2 INTO cap_con_3[ven_mod_2_1].*
        LET cap_3[ven_mod_2_1].consecutivo = cap_con_3[ven_mod_2_1].consecutivo
        LET cap_3[ven_mod_2_1].descripcion = cap_con_3[ven_mod_2_1].descripcion

        IF cap_con_3[ven_mod_2_1].obliga = 0 THEN
               LET cap_3[ven_mod_2_1].obliga = "N"
        ELSE
               LET cap_3[ven_mod_2_1].obliga = "S"
        END IF

        IF cap_con_3[ven_mod_2_1].estado = 0 THEN
             LET cap_3[ven_mod_2_1].estado = "S"
        ELSE
             LET cap_3[ven_mod_2_1].estado = "N"
             LET estado_n = estado_n + 1
        END IF
        LET cap_3[ven_mod_2_1].ubicacion = cap_con_3[ven_mod_2_1].ubicacion

        LET ven_mod_2_1 = ven_mod_2_1 + 1
      END FOREACH

      CALL SET_COUNT(ven_mod_2_1-1)
      DISPLAY ARRAY cap_3 TO scr_2.*

         ON KEY (ESC)
             LET ven_mod_2_1 = ARR_CURR()
             LET ven_mod_2_2 = SCR_LINE()
             LET cap_3_3.* = cap_3[ven_mod_2_1].*
             CALL llena_mod_tres() RETURNING salir, cambio
             IF cambio = 0 THEN
                LET cap_con_3[ven_mod_2_1].estado = "S" 
                DISPLAY "S" TO scr_2[ven_mod_2_2].estado
                LET estado_n = estado_n - 1
                UPDATE afi_expediente
						 SET estado_exp = 0
                   WHERE lote           = cap_con.lote
                     AND n_folio        = cap_2_2.folio
                     AND tipo_solicitud = cap_2_2.solicitud
                     AND docto_cod      = cap_3[ven_mod_2_1].consecutivo
             END IF
             IF cambio = 1 THEN
                LET cap_con_3[ven_mod_2_1].estado = "N"
                DISPLAY "N" TO scr_2[ven_mod_2_2].estado
                LET estado_n = estado_n + 1

                UPDATE afi_expediente
                   SET estado_exp = 1
                 WHERE lote           = cap_con.lote
                   AND n_folio        = cap_2_2.folio
                   AND tipo_solicitud = cap_2_2.solicitud
                   AND docto_cod      = cap_3[ven_mod_2_1].consecutivo
             END IF
             CASE salir
                  WHEN 3  EXIT DISPLAY
             END CASE

         ON KEY (CONTROL-V)
                IF estado_n > 0 THEN
                   UPDATE afi_recepcion
                      SET estado_exp = 1
                    WHERE n_folio = cap_2_2.folio
                      AND tipo_solicitud = cap_2_2.solicitud
                   LET estado_n = 1
                ELSE
                   UPDATE afi_recepcion
                      SET estado_exp = 0
                    WHERE n_folio = cap_2_2.folio
                      AND tipo_solicitud = cap_2_2.solicitud
                   LET estado_n = 0
                END IF
                LET salir = 1
                EXIT DISPLAY

         ON KEY (INTERRUPT)
                LET salir = 3
                EXIT DISPLAY

         ON KEY (CONTROL-C)
                LET salir = 3
                EXIT DISPLAY
      END DISPLAY
      RETURN salir, estado_n

END FUNCTION

############################################################################
FUNCTION llena_mod_tres()
 
DEFINE mm            ,
       ven_mod_3_1   ,
       ven_mod_3_2     INTEGER,
       conta_mov     ,
       rechazo_uno   ,
		 rechazo_1       SMALLINT

     LET mm = 0
     FOR mm = 1 TO 100
         INITIALIZE cap_4[mm].* TO NULL
     END FOR

     DECLARE apt_mod_3 CURSOR FOR
         SELECT a.docto_cod, a.cond_cod, b.cond_desc,  b.cond_rechazo  
             FROM afi_condicion_exp a, afi_condicion_doc b
             WHERE a.lote           =  cap_con.lote
               AND a.n_folio        =  cap_2_2.folio
               AND a.tipo_solicitud =  cap_2_2.solicitud
               AND a.docto_cod      =  cap_3_3.consecutivo
               AND a.docto_cod      =  b.docto_cod
               AND a.cond_cod       =  b.cond_cod
     LET ven_mod_3_1 = 1

     FOREACH apt_mod_3 INTO cap_4[ven_mod_3_1].*              
         IF cap_4[ven_mod_3_1].rechazo = 1 THEN
             LET rechazo_uno = rechazo_uno + 1
         END IF

         LET ven_mod_3_1 = ven_mod_3_1 + 1
     END FOREACH
 
     IF ven_mod_3_1 = 1 THEN
        ERROR "  No existen Condiciones para este Docto."
        ATTRIBUTE(NORMAL)
        LET rechazo_uno = 1
        LET salir = 1
        RETURN salir, rechazo_uno
     END IF
 
     LET conta_mov = ven_mod_3_1 - 1

     OPEN WINDOW ven_modi_tres AT 8,33 WITH FORM "AFIM0083" ATTRIBUTE(BORDER)
     DISPLAY " < ESC > Regresa Docto" AT 1,13

          CALL SET_COUNT(ven_mod_3_1-1)
          INPUT ARRAY cap_4  WITHOUT DEFAULTS FROM scr_docto.*
               BEFORE FIELD consecutivo
                   LET ven_mod_3_1 = ARR_CURR()
                   LET ven_mod_3_2 = SCR_LINE()
                   LET cap_4_4.* = cap_4[ven_mod_3_1].*
                   NEXT FIELD codigo

                AFTER FIELD codigo
                    IF cap_4[ven_mod_3_1].codigo IS NULL OR
                       cap_4[ven_mod_3_1].codigo = 0     THEN
                       CALL ven_codigo() RETURNING cap_4[ven_mod_3_1].codigo,
                                                 cap_4[ven_mod_3_1].desc_codigo,
                                                 cap_4[ven_mod_3_1].rechazo
                       DISPLAY cap_4[ven_mod_3_1].codigo
                               TO scr_docto[ven_mod_3_2].codigo
                       DISPLAY cap_4[ven_mod_3_1].desc_codigo
                               TO scr_docto[ven_mod_3_2].descripcion
                       DISPLAY cap_4[ven_mod_3_1].rechazo
                               TO scr_docto[ven_mod_3_2].rechazo

                       IF cap_4_4.rechazo      = 1 AND
								  cap_4[ven_mod_3_1].rechazo = 0 THEN
                          LET rechazo_uno = rechazo_uno - 1 
                       END IF

                       IF cap_4[ven_mod_3_1].rechazo = 1 THEN
                          LET rechazo_1 = 1
                       END IF
                    ELSE

                        SELECT f.cond_desc, f.cond_rechazo
                          INTO cap_4[ven_mod_3_1].desc_codigo,
                               cap_4[ven_mod_3_1].rechazo
                          FROM   afi_condicion_doc f
                          WHERE  f.docto_cod = cap_4[ven_mod_3_1].consecutivo
                            AND  f.cond_cod   = cap_4[ven_mod_3_1].codigo
                        IF STATUS = NOTFOUND THEN
                            ERROR "  Registro Inexistente" ATTRIBUTE(NORMAL)
                            NEXT FIELD codigo
                        ELSE
                            DISPLAY cap_4[ven_mod_3_1].desc_codigo
                                    TO scr_docto[ven_mod_3_2].descripcion
                            DISPLAY cap_4[ven_mod_3_1].rechazo
                                    TO scr_docto[ven_mod_3_2].rechazo

                            IF cap_4_4.rechazo      = 1 AND
								       cap_4[ven_mod_3_1].rechazo = 0 THEN
                               LET rechazo_uno = rechazo_uno - 1 
                            END IF

                            IF cap_4[ven_mod_3_1].rechazo = 1 THEN
                               LET rechazo_1 = 1
                            END IF
                        END IF
                    END IF
                    UPDATE afi_condicion_exp
                       SET   cond_cod       = cap_4[ven_mod_3_1].codigo
                       WHERE n_folio        = cap_2_2.folio
                         AND tipo_solicitud = cap_2_2.solicitud
                         AND docto_cod      = cap_4_4.consecutivo
                         AND cond_cod       = cap_4_4.codigo

                    IF ven_mod_3_1 > conta_mov THEN
                       ERROR "  No Existen mas Registros a Modificar" ATTRIBUTE(NORMAL)
							  NEXT FIELD codigo
                    END IF

                    ON KEY (ESC)                        
                       IF cap_4[ven_mod_3_1].codigo IS NULL OR
                          cap_4[ven_mod_3_1].codigo = 0     THEN
                          LET ven_mod_3_1 = ven_mod_3_1 - 1
                       END IF
							  IF rechazo_uno = 0   AND
								  rechazo_1   = 1   THEN
                          LET rechazo_uno = 1
                       END IF
							  IF rechazo_uno = 1   AND
								  rechazo_1   = 0   THEN
                          LET rechazo_uno = 1
                       END IF
							  IF rechazo_uno = 0   AND
								  rechazo_1   = 0   THEN
                          LET rechazo_uno = 0
                       END IF

                       LET salir = 1
                       EXIT INPUT

                    ON KEY (INTERRUPT)
                       LET salir = 3
                       EXIT INPUT

                    ON KEY (CONTROL-C)
                       LET salir = 3
                       EXIT INPUT
          END INPUT

     CLOSE WINDOW ven_modi_tres
     RETURN salir, rechazo_uno

END FUNCTION
############################################################################
FUNCTION elimina()
#e-----------------
   DEFINE   valida_fecha    DATE

   OPTIONS
         ACCEPT KEY CONTROL-I,
         INPUT WRAP

   INITIALIZE valida_fecha TO NULL
   CALL init()

   DISPLAY "" AT 1,1
   DISPLAY "" AT 2,1
   DISPLAY " ELIMINA " AT 1,70 ATTRIBUTE ( REVERSE )
   DISPLAY " < Ctrl-C >Salir  < ESC >Aceptar                           " AT 2,1

    INPUT cap_con.lote FROM FORMONLY.lote

          AFTER FIELD lote
                IF cap_con.lote IS NULL  OR
                   cap_con.lote = 0      THEN
                   ERROR "  El Lote No puede ser nulo o cero" ATTRIBUTE(NORMAL)
                   NEXT FIELD lote
                END IF

                SELECT d.fecha, d.usuario, d.estado, d.no_solic, d.tipo_solicitud
                       INTO cap_1.fecha_auto, cap_1.operador, 
                            cap_1.condicion,  registros,  
                            cap_1.tip_solicitud
                       FROM afi_ctr_lote d
                       WHERE d.lote = cap_con.lote
                IF STATUS = NOTFOUND THEN
                    ERROR "  Lote Inexistente" ATTRIBUTE(NORMAL)
                    NEXT FIELD lote
                END IF
                IF cap_1.operador != usuario THEN
                   ERROR "  El operador ",usuario," no tiene acceso a este Lote"
                   ATTRIBUTE(NORMAL)
                   NEXT FIELD lote
                END IF

                SELECT "OK"
                       FROM afi_ctr_lote
                       WHERE lote = cap_con.lote
                         AND fecha_captura = TODAY
                IF STATUS = NOTFOUND THEN
                     ERROR "  Ya No puede eliminar ningun registro de este Lote"
                     ATTRIBUTE(NORMAL)
                     NEXT FIELD lote
                END IF

                DISPLAY cap_1.condicion TO condicion
                DISPLAY cap_1.fecha_auto TO fecha_auto
                DISPLAY cap_1.tip_solicitud TO tip_solicitud
                DISPLAY cap_1.operador TO operador
                DISPLAY registros TO registros

            ON KEY (ESC)
                IF cap_con.lote IS NULL  OR
                   cap_con.lote = 0      THEN
                   ERROR "  El Lote No puede ser nulo o cero" ATTRIBUTE(NORMAL)
                   NEXT FIELD lote
                END IF
                LET salir = 1
                EXIT INPUT

            ON KEY (CONTROL-C)
                LET salir = 3
                EXIT INPUT

            ON KEY (INTERRUPT)
                LET salir = 3
                EXIT INPUT
    END INPUT
    CASE salir 
        WHEN 1
             CALL llena_eli_uno() RETURNING salir
             CASE salir
                WHEN 1
                       PROMPT " PROCESO FINALIZADO... < ENTER > PARA SALIR" 
                       FOR CHAR enter
                       CLEAR FORM
                WHEN 3
                        PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                        FOR CHAR enter
                        CLEAR FORM
             END CASE
        WHEN 3
                PROMPT " PROCESO CANCELADO... < ENTER > PARA SALIR" 
                FOR CHAR enter
    END CASE
END FUNCTION

############################################################################
FUNCTION llena_eli_uno()

     DEFINE  xx           ,
             cambia1      ,
             ven_eli_1_1  ,
             ven_eli_1_2    INTEGER

     DISPLAY " < Ctrl-C >Salir  < ESC >Acepta Doctos.                    " 
     AT 2,1

     LET cambia1 = 0
     LET ven_eli_1_1 = 1
     DECLARE apt_eli_1 CURSOR FOR
            SELECT UNIQUE tipo_solicitud, n_folio, n_seguro
                    FROM  afi_expediente
                    WHERE lote = cap_con.lote

      FOREACH apt_eli_1 INTO cap_con_2[ven_eli_1_1].solicitud,
                             cap_con_2[ven_eli_1_1].folio,
                             cap_con_2[ven_eli_1_1].nss
             LET cap_2[ven_eli_1_1].solicitud = cap_con_2[ven_eli_1_1].solicitud
             LET cap_2[ven_eli_1_1].folio = cap_con_2[ven_eli_1_1].folio
             LET cap_2[ven_eli_1_1].nss = cap_con_2[ven_eli_1_1].nss

             SELECT m.estado_exp INTO cap_con_2[ven_eli_1_1].estado_sol
                    FROM afi_recepcion m
                    WHERE m.n_folio = cap_con_2[ven_eli_1_1].folio

             IF cap_con_2[ven_eli_1_1].estado_sol = 0 THEN
                 LET cap_2[ven_eli_1_1].estado_sol = "S"
             ELSE
                 LET cap_2[ven_eli_1_1].estado_sol = "N"
             END IF

             LET ven_eli_1_1 = ven_eli_1_1 + 1
      END FOREACH

     CALL SET_COUNT(ven_eli_1_1-1)
     DISPLAY ARRAY cap_2 TO scr_1.*

         ON KEY (ESC)
             LET ven_eli_1_1 = ARR_CURR()
             LET ven_eli_1_2 = SCR_LINE()
             LET cap_2_2.* = cap_2[ven_eli_1_1].*
             CALL llena_dos_promotor()
             CALL llena_eli_dos() RETURNING salir, cambia1
             IF cambia1 = 1  AND  cap_2[ven_eli_1_1].estado_sol = "S" THEN
                LET cap_2[ven_eli_1_1].estado_sol = "N"
                DISPLAY cap_2[ven_eli_1_1].estado_sol
                        TO scr_1[ven_eli_1_2].estado_sol
             END IF
             IF cambia1 = 0 AND  cap_2[ven_eli_1_1].estado_sol = "N" THEN
                LET cap_2[ven_eli_1_1].estado_sol = "S"
                DISPLAY cap_2[ven_eli_1_1].estado_sol
                        TO scr_1[ven_eli_1_2].estado_sol
             END IF
             CASE salir
                     WHEN 3
                            LET salir = 3
                            EXIT DISPLAY
             END CASE
             DISPLAY " < Ctrl-C >Salir  < ESC >Acepta Doctos.                    " 
             AT 2,1
            
         ON KEY (INTERRUPT)
                LET salir = 3
                EXIT DISPLAY

         ON KEY (CONTROL-C)
                LET salir = 3
                EXIT DISPLAY

      END DISPLAY
      RETURN salir

END FUNCTION

FUNCTION llena_eli_dos()
     DEFINE jj            ,
            ven_eli_2_1   ,
            ven_eli_2_2     INTEGER, 
            estado_n,
            cam_eli         SMALLINT

     DISPLAY " < Ctrl-C >Salir  < ESC >T.Condicion  < Ctrl-V >Otro Folio " 
     AT 2,1

     LET ven_eli_2_1 = 1
     DECLARE apt_eli_2 CURSOR FOR
            SELECT a.docto_cod,  b.docto_desc,  b.docto_obliga, 
                   a.estado_exp, a.ubicacion 
                FROM  afi_expediente a, tab_ctr_doc b 
                WHERE a.lote    =  cap_con.lote
                  AND a.n_folio =  cap_2_2.folio
                  AND a.tipo_solicitud = cap_2_2.solicitud
                  AND b.docto_cod = a.docto_cod 

      FOREACH apt_eli_2 INTO cap_con_3[ven_eli_2_1].*
        LET cap_3[ven_eli_2_1].consecutivo = cap_con_3[ven_eli_2_1].consecutivo
        LET cap_3[ven_eli_2_1].descripcion = cap_con_3[ven_eli_2_1].descripcion

        IF cap_con_3[ven_eli_2_1].obliga = 0 THEN
               LET cap_3[ven_eli_2_1].obliga = "N"
        ELSE
               LET cap_3[ven_eli_2_1].obliga = "S"
        END IF

        IF cap_con_3[ven_eli_2_1].estado = 0 THEN
             LET cap_3[ven_eli_2_1].estado = "S"
        ELSE
             LET cap_3[ven_eli_2_1].estado = "N"
             LET estado_n = estado_n + 1
        END IF
        LET cap_3[ven_eli_2_1].ubicacion = cap_con_3[ven_eli_2_1].ubicacion

        LET ven_eli_2_1 = ven_eli_2_1 + 1
      END FOREACH

      CALL SET_COUNT(ven_eli_2_1-1)
      DISPLAY ARRAY cap_3 TO scr_2.*

         ON KEY (ESC)
             LET ven_eli_2_1 = ARR_CURR()
             LET ven_eli_2_2 = SCR_LINE()
             LET cap_3_3.* = cap_3[ven_eli_2_1].*
             CALL llena_eli_tres() RETURNING salir, cam_eli
             CASE salir
                  WHEN 3  EXIT DISPLAY
             END CASE
             IF cam_eli = 1 THEN
                 DISPLAY "N" TO scr_2[ven_eli_2_1].estado
             END IF

         ON KEY (CONTROL-V)
                LET salir = 1
                EXIT DISPLAY

         ON KEY (INTERRUPT)
                LET salir = 3
                EXIT DISPLAY

         ON KEY (CONTROL-C)
                LET salir = 3
                EXIT DISPLAY
      END DISPLAY
      RETURN salir, estado_n

END FUNCTION

FUNCTION llena_eli_tres()
 
DEFINE mm            ,
       ven_eli_3_1   ,
       ven_eli_3_2     INTEGER,
       eli_mina        CHAR(01),
		 rechazo_uno,
       regreso         SMALLINT

		 OPTIONS MESSAGE LINE LAST,
					PROMPT  LINE LAST -1

     LET mm = 0
     FOR mm = 1 TO 100
         INITIALIZE cap_4[mm].* TO NULL
     END FOR

     DECLARE apt_eli_3 CURSOR FOR
         SELECT a.docto_cod, a.cond_cod, b.cond_desc,  b.cond_rechazo  
             FROM afi_condicion_exp a, afi_condicion_doc b
             WHERE a.lote           =  cap_con.lote
               AND a.n_folio        =  cap_2_2.folio
               AND a.tipo_solicitud =  cap_2_2.solicitud
               AND a.docto_cod      =  cap_3_3.consecutivo
               AND a.docto_cod      =  b.docto_cod
               AND a.cond_cod       =  b.cond_cod
     LET ven_eli_3_1 = 1
     FOREACH apt_eli_3 INTO cap_4[ven_eli_3_1].*              
         IF cap_4[ven_eli_3_1].rechazo = 1 THEN
             LET rechazo_uno = rechazo_uno + 1
         END IF
         LET ven_eli_3_1 = ven_eli_3_1 + 1
     END FOREACH

     IF ven_eli_3_1 = 1 THEN
        ERROR "  No existen Condiciones para este Docto."
        ATTRIBUTE(NORMAL)
        LET salir = 1
        LET regreso = 0
        RETURN salir, regreso
     END IF

     OPEN WINDOW ven_elii_tres AT 8,33 WITH FORM "AFIM0083" ATTRIBUTE(BORDER)
     DISPLAY " < ESC > Regresa Docto" AT 1,13
     MESSAGE " < Ctrl-B > Elimina Registro" 

     CALL SET_COUNT(ven_eli_3_1-1)
     DISPLAY ARRAY cap_4 TO scr_docto.*
             ON KEY (CONTROL-B)
                LET ven_eli_3_1 = ARR_CURR()
                LET ven_eli_3_2 = SCR_LINE()
                DELETE FROM afi_condicion_exp
                   WHERE n_folio = cap_2_2.folio
                     AND tipo_solicitud = cap_2_2.solicitud
                     AND docto_cod = cap_4[ven_eli_3_1].consecutivo
                     AND cond_cod  = cap_4[ven_eli_3_1].codigo
                INITIALIZE cap_4[ven_eli_3_1].* TO NULL 
                DISPLAY cap_4[ven_eli_3_1].* 
                        TO scr_docto[ven_eli_3_2].*

              ON KEY (ESC)                        
                 IF ven_eli_3_1 = 1 THEN
                    UPDATE afi_recepcion SET estado_exp = 1
                           WHERE n_folio = cap_2_2.folio
                             AND tipo_solicitud = cap_2_2.solicitud
                    LET regreso = 1
                    UPDATE afi_expediente SET estado_exp = 1
                           WHERE n_folio = cap_2_2.folio
                             AND tipo_solicitud = cap_2_2.solicitud
                             AND docto_cod = cap_4[ven_eli_3_1].consecutivo
                 ELSE
                    LET regreso = 0
                 END IF
                 LET salir = 1
                 EXIT DISPLAY

              ON KEY (INTERRUPT)
                 LET salir = 3
                 EXIT DISPLAY

              ON KEY (CONTROL-C)
                 LET salir = 3
                 EXIT DISPLAY
     END DISPLAY

     CLOSE WINDOW ven_elii_tres
     RETURN salir, regreso

END FUNCTION

{
FUNCTION val_docto(vdocto_obliga)

  DEFINE vdocto_obliga CHAR(1)

  IF vdocto_obliga = 'S' THEN
     LET vdocto_obliga = 1 
  ELSE
     LET vdocto_obliga = 0 
  END IF
  RETURN vdocto_obliga

END FUNCTION
}
